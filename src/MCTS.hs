{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module MCTS
    ( MCTSNode(..)
    , MCTSParams(..)
    , selectBestAction
    , mcts
    , mctsParallel
    ) where

import Types
import GameExecution (executeAction, isValidPosition, applyAction)
import System.Random
import Control.Monad.State hiding (state)
import qualified Control.Monad.State as ST
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Parallel.Strategies
import Control.Monad (replicateM)
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Vector as V

-- MCTS Node representing a state in the game tree
data MCTSNode = MCTSNode
    { nodeState :: GameState
    , nodeAction :: Maybe Action
    , nodeVisits :: !Int
    , nodeWins :: !Double
    , nodeChildren :: Map Action MCTSNode
    , nodeUntriedActions :: [Action]
    } deriving (Show, Generic)

instance NFData MCTSNode

-- MCTS Parameters
data MCTSParams = MCTSParams
    { mctsIterations :: Int
    , mctsExploration :: Double
    , mctsMaxDepth :: Int
    } deriving (Show)

defaultMCTSParams :: MCTSParams
defaultMCTSParams = MCTSParams
    { mctsIterations = 1000
    , mctsExploration = 1.41  -- sqrt(2) - standard UCB1 value
    , mctsMaxDepth = 200
    }

-- Get all VALID actions for a state (not walls)
legalActions :: GameState -> [Action]
legalActions gs = 
    let g = grid gs
        pos = pacmanPos gs
    in filter (\action -> isValidPosition g (applyAction pos action)) allActions

-- UCB1 formula for action selection
ucb1 :: Double -> Int -> Double -> Int -> Double
ucb1 explorationConstant parentVisits nodeWins nodeVisits
    | nodeVisits == 0 = 1.0e308  -- Infinite value for unvisited
    | parentVisits == 0 = 0.0
    | otherwise = 
        let exploitation = nodeWins / fromIntegral nodeVisits
            exploration = explorationConstant * sqrt (log (fromIntegral parentVisits) / fromIntegral nodeVisits)
        in exploitation + exploration

-- Select best child using UCB1
selectChild :: Double -> MCTSNode -> MCTSNode
selectChild explorationConstant node =
    let children = Map.elems (nodeChildren node)
        parent_visits = nodeVisits node
        scores = map (\child -> 
            (ucb1 explorationConstant parent_visits (nodeWins child) (nodeVisits child), child)) children
    in snd $ maximumBy (comparing fst) scores

-- Expand node by adding one untried action
expand :: RandomGen g => MCTSNode -> State g MCTSNode
expand node = do
    case nodeUntriedActions node of
        [] -> return node
        actions -> do
            -- Randomly pick an untried action
            idx <- ST.state $ randomR (0, length actions - 1)
            let action = actions !! idx
                rest = take idx actions ++ drop (idx + 1) actions
                state = nodeState node
                (newState, _) = executeAction state action
                newNode = MCTSNode
                    { nodeState = newState
                    , nodeAction = Just action
                    , nodeVisits = 0
                    , nodeWins = 0
                    , nodeChildren = Map.empty
                    , nodeUntriedActions = legalActions newState
                    }
                updatedNode = node 
                    { nodeChildren = Map.insert action newNode (nodeChildren node)
                    , nodeUntriedActions = rest
                    }
            return updatedNode

-- Check if a position would be immediately dangerous (ghost adjacent after their move)
isDangerousMove :: GameState -> Action -> Bool
isDangerousMove gs action =
    let newPos = applyAction (pacmanPos gs) action
        g = grid gs
        ghosts = ghostPositions gs
        -- Simulate where each ghost COULD move
        possibleGhostMoves = [applyAction ghost a | ghost <- ghosts, a <- allActions]
        validGhostMoves = filter (isValidPosition g) possibleGhostMoves
        -- Check if any ghost could reach our new position
    in newPos `elem` validGhostMoves

-- HEAVILY improved simulation with extreme ghost avoidance
simulate :: RandomGen g => GameState -> Int -> State g Double
simulate initialState maxDepth = go initialState 0 0.0 []
    where
        go gameState depth totalReward visitedPositions
            | depth >= maxDepth = return totalReward
            | isTerminal gameState = 
                if pelletsRemaining gameState == 0
                    then return (totalReward + 1000.0)  -- Huge bonus for winning (increased from 500)
                    else return (totalReward - 500.0)  -- Huge penalty for dying
            | otherwise = do
                let validActions = legalActions gameState
                
                if null validActions
                    then return (totalReward - 200.0)  -- Trapped!
                    else do
                        -- Filter out immediately dangerous moves
                        let safeActions = filter (not . isDangerousMove gameState) validActions
                        let actionsToUse = if null safeActions then validActions else safeActions
                        
                        -- 80% safe greedy, 20% random (prioritize survival)
                        r <- ST.state random
                        action <- if r < (0.8 :: Double)
                            then return $ chooseSafeGreedyAction gameState actionsToUse visitedPositions
                            else do
                                idx <- ST.state $ randomR (0, length actionsToUse - 1)
                                return $ actionsToUse !! idx
                        
                        let (newState, reward) = executeAction gameState action
                            currentPos = pacmanPos newState
                            currentGrid = grid newState
                            
                        -- MUCH heavier penalties for dangerous situations
                        let ghostPenalty = ghostProximityPenalty newState
                            revisitPenalty = if currentPos `elem` take 5 visitedPositions
                                            then -25.0  -- Increased from -8.0
                                            else 0.0
                            -- Extra penalty for immediate backtrack
                            backtrackPenalty = if not (null visitedPositions) && currentPos == head visitedPositions
                                              then -100.0
                                              else 0.0
                            -- HUGE penalty for going to empty spaces we've been to
                            emptyRevisitPenalty = if (currentGrid V.! snd currentPos V.! fst currentPos) == Empty
                                                    && currentPos `elem` take 10 visitedPositions
                                                    then -150.0
                                                    else 0.0
                            -- Penalty for being in a corner or having few escape routes
                            escapeRoutes = length $ filter (isValidPosition currentGrid) 
                                                  [applyAction currentPos a | a <- allActions]
                            trappedPenalty = case escapeRoutes of
                                1 -> -30.0  -- Only one way out - very dangerous!
                                2 -> -10.0  -- Two ways out - concerning
                                _ -> 0.0
                        
                        let adjustedReward = reward + ghostPenalty + revisitPenalty + backtrackPenalty + emptyRevisitPenalty + trappedPenalty
                        let discountedReward = totalReward + adjustedReward * (0.95 ^ depth)
                        
                        go newState (depth + 1) discountedReward (currentPos : visitedPositions)
        
        -- MUCH harsher ghost proximity penalties
        ghostProximityPenalty gs =
            let pos = pacmanPos gs
                ghosts = ghostPositions gs
                manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
                distances = [manhattan pos g | g <- ghosts]
                minDist = if null distances then 100 else minimum distances
                -- Count how many ghosts are nearby (being surrounded is worse)
                nearbyGhosts = length $ filter (<= 3) distances
                surroundPenalty = fromIntegral nearbyGhosts * (-20.0)  -- Increased from -15
                
                -- Penalty based on average distance to all ghosts
                avgDist = if null distances 
                         then 100.0 
                         else fromIntegral (sum distances) / fromIntegral (length distances)
                avgDistPenalty = case avgDist of
                    d | d <= 2.0 -> -80.0   -- Way too close on average
                    d | d <= 3.0 -> -40.0   -- Too close
                    d | d <= 4.0 -> -15.0   -- Concerning
                    _ -> 0.0
                    
            in case minDist of
                0 -> -500.0  -- On ghost - should never happen but catastrophic
                1 -> -250.0  -- Adjacent to ghost - extremely dangerous! (increased from -200)
                2 -> -80.0   -- 2 steps away - very dangerous (increased from -60)
                3 -> -30.0   -- 3 steps away - dangerous (increased from -20)
                4 -> -10.0   -- 4 steps away - concerning (increased from -5)
                5 -> -3.0    -- 5 steps away - still want more distance
                _ -> 0.0
            + surroundPenalty + avgDistPenalty
        
        -- Choose action with EXTREME ghost avoidance priority
        chooseSafeGreedyAction gs actions recentPositions =
            let pos = pacmanPos gs
                g = grid gs
                ghosts = ghostPositions gs
                
                scoreAction action =
                    let newPos = applyAction pos action
                        manhattan (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)
                        
                        -- Distance to nearest ghost (TOP PRIORITY)
                        ghostDistances = [manhattan newPos gh | gh <- ghosts]
                        minGhostDist = if null ghostDistances then 100.0 else minimum ghostDistances
                        -- HEAVILY weight ghost avoidance with exponential penalty for proximity
                        ghostScore = case minGhostDist of
                            d | d <= 1.0 -> -10000.0  -- NEVER move adjacent to ghost
                            d | d <= 2.0 -> -1000.0   -- Very bad
                            d | d <= 3.0 -> -200.0    -- Bad
                            d | d <= 4.0 -> -50.0     -- Concerning
                            d | d <= 5.0 -> d * 20.0  -- Still want to maintain distance
                            d -> d * 50.0             -- Good - maximize distance
                        
                        -- How many ghosts can reach this position next turn?
                        threateningGhosts = length $ filter (\gh -> manhattan newPos gh <= 2.0) ghosts
                        threatScore = fromIntegral threateningGhosts * (-500.0)
                        
                        -- Additional penalty based on average distance to ALL ghosts
                        avgGhostDist = if null ghostDistances 
                                      then 100.0 
                                      else sum ghostDistances / fromIntegral (length ghostDistances)
                        avgDistScore = case avgGhostDist of
                            d | d <= 3.0 -> -100.0  -- Surrounded!
                            d | d <= 5.0 -> -30.0   -- Too close on average
                            d -> d * 5.0            -- Reward being far from all ghosts
                        
                        -- Count escape routes from new position
                        escapeRoutes = length $ filter (isValidPosition g) 
                                              [applyAction newPos a | a <- allActions]
                        escapeScore = case escapeRoutes of
                            1 -> -300.0  -- Dead end - avoid!
                            2 -> -50.0   -- Limited options
                            3 -> 20.0    -- Decent
                            _ -> 50.0    -- Good - many options
                        
                        -- Distance to nearest pellet (MUCH HIGHER PRIORITY NOW)
                        pelletDist = nearestPelletDist g newPos
                        pelletScore = negate $ pelletDist * 2.0  -- Increased from 0.3 to 2.0!
                        
                        -- Bonus for being on a pellet (MUCH BIGGER)
                        -- But ONLY if there's actually a pellet there!
                        onPelletBonus = if (g V.! snd newPos V.! fst newPos) == Pellet 
                                       then 150.0 else 0.0  -- Increased from 100.0
                        
                        -- CRITICAL: Heavy penalty for moving to empty spaces we've visited
                        -- This prevents going back to where pellets were already eaten
                        emptyRevisitPenalty = if (g V.! snd newPos V.! fst newPos) == Empty 
                                             && newPos `elem` take 10 recentPositions
                                             then -250.0  -- Massive penalty for backtracking to empty spaces
                                             else 0.0
                        
                        -- Penalize revisiting (stronger now to prevent backtracking)
                        revisitPenalty = if newPos `elem` take 3 recentPositions
                                        then -150.0  -- Much stronger penalty (was -30.0)
                                        else 0.0
                        
                        -- Extra penalty for going back to exact previous position
                        backtrackPenalty = if not (null recentPositions) && newPos == head recentPositions
                                          then -300.0  -- Massive penalty for immediate backtrack
                                          else 0.0
                        
                        -- NEW: Progress toward uncollected pellets
                        -- Reward moving closer to the nearest pellet
                        currentPelletDist = nearestPelletDist g pos
                        progressBonus = if pelletDist < currentPelletDist
                                       then 40.0  -- Moving closer to pellet
                                       else if pelletDist > currentPelletDist
                                       then -40.0  -- Moving away from pellet
                                       else -20.0  -- Standing still (no progress)
                        
                        -- NEW: Exploration bonus for moving to less-visited areas
                        visitCount = length $ filter (== newPos) (take 20 recentPositions)
                        explorationBonus = case visitCount of
                            0 -> 30.0   -- Never been here
                            1 -> 10.0   -- Been once
                            2 -> -10.0  -- Been twice
                            _ -> -50.0  -- Been many times - avoid!
                        
                    in ghostScore + threatScore + avgDistScore + escapeScore + pelletScore + onPelletBonus + revisitPenalty + backtrackPenalty + emptyRevisitPenalty
                
                scoredActions = [(action, scoreAction action) | action <- actions]
            in fst $ maximumBy (comparing snd) scoredActions
        
        nearestPelletDist g pos =
            let rows = V.length g
                pelletPositions = [(x, y) | y <- [0..rows-1], 
                                            x <- [0..V.length (g V.! y)-1],
                                            (g V.! y V.! x) == Pellet]
                manhattan (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)
            in if null pelletPositions 
               then 100.0 
               else minimum [manhattan pos p | p <- pelletPositions]

-- Backpropagate result through the tree
backpropagate :: Double -> [MCTSNode] -> [MCTSNode]
backpropagate result path =
    map (\node -> node 
        { nodeVisits = nodeVisits node + 1
        , nodeWins = nodeWins node + result
        }) path

-- FIXED iteration to properly update the tree structure
mctsIteration :: RandomGen g => MCTSParams -> MCTSNode -> State g MCTSNode
mctsIteration params rootNode = do
    (selectedNode, path) <- selectAndExpand params rootNode []
    result <- simulate (nodeState selectedNode) (mctsMaxDepth params)
    
    -- Update nodes in path with new statistics
    let updatedPath = backpropagate result (selectedNode : path)
    
    -- Reconstruct tree from updated path
    return $ reconstructTree updatedPath
    where
        reconstructTree [] = rootNode
        reconstructTree [node] = node
        reconstructTree (child:parent:rest) =
            let action = case nodeAction child of
                    Just a -> a
                    Nothing -> error "Child node must have an action"
                updatedParent = parent {
                    nodeChildren = Map.insert action child (nodeChildren parent)
                }
            in reconstructTree (updatedParent:rest)
        
        selectAndExpand params node path
            | isTerminal (nodeState node) = return (node, path)
            | not (null (nodeUntriedActions node)) = do
                expandedParent <- expand node
                let newChildren = Map.difference (nodeChildren expandedParent) (nodeChildren node)
                if Map.null newChildren
                    then return (expandedParent, path)
                    else
                        let newChild = head $ Map.elems newChildren
                        in return (newChild, expandedParent : path)
            | Map.null (nodeChildren node) = return (node, path)
            | otherwise = do
                let bestChild = selectChild (mctsExploration params) node
                    action = case nodeAction bestChild of
                        Just a -> a
                        Nothing -> MoveUp
                    actualChild = (nodeChildren node) Map.! action
                selectAndExpand params actualChild (node : path)

-- Run MCTS for a given number of iterations
mcts :: RandomGen g => MCTSParams -> GameState -> State g MCTSNode
mcts params initialState = do
    let rootNode = MCTSNode
            { nodeState = initialState
            , nodeAction = Nothing
            , nodeVisits = 0
            , nodeWins = 0
            , nodeChildren = Map.empty
            , nodeUntriedActions = legalActions initialState
            }
    go params rootNode (mctsIterations params)
    where
        go params node 0 = return node
        go params node n = do
            updatedNode <- mctsIteration params node
            go params updatedNode (n - 1)

-- Parallel MCTS using root parallelization
mctsParallel :: Int -> MCTSParams -> GameState -> IO MCTSNode
mctsParallel numThreads params initialState = do
    let iterationsPerThread = mctsIterations params `div` numThreads
        params' = params { mctsIterations = iterationsPerThread }
    
    seeds <- replicateM numThreads randomIO
    
    let trees = parMap rdeepseq (\seed ->
            evalState (mcts params' initialState) (mkStdGen seed)
            ) seeds
    
    return $ mergeTrees trees

-- Merge multiple MCTS trees by combining statistics
mergeTrees :: [MCTSNode] -> MCTSNode
mergeTrees [] = error "Cannot merge empty list of trees"
mergeTrees (first:rest) = 
    let combinedChildren = foldl mergeChildMaps (nodeChildren first) (map nodeChildren rest)
        totalVisits = sum $ map nodeVisits (first:rest)
        totalWins = sum $ map nodeWins (first:rest)
    in first
        { nodeVisits = totalVisits
        , nodeWins = totalWins
        , nodeChildren = combinedChildren
        }
    where
        mergeChildMaps m1 m2 = Map.unionWith combineNodes m1 m2
        combineNodes n1 n2 = n1
            { nodeVisits = nodeVisits n1 + nodeVisits n2
            , nodeWins = nodeWins n1 + nodeWins n2
            , nodeChildren = Map.unionWith combineNodes (nodeChildren n1) (nodeChildren n2)
            }

-- IMPROVED action selection - heavily consider safety AND immediate danger
selectBestAction :: MCTSNode -> Action
selectBestAction node =
    let children = Map.toList (nodeChildren node)
    in if null children
        then MoveUp  -- Fallback
        else
            -- First, filter out immediately dangerous moves
            let currentState = nodeState node
                
                -- Check if an action leads to immediate death
                isImmediatelyDangerous action = 
                    case Map.lookup action (nodeChildren node) of
                        Nothing -> False
                        Just childNode -> 
                            let childState = nodeState childNode
                            in isTerminal childState && pelletsRemaining childState > 0
                
                -- Also check if moving would put us adjacent to a ghost
                isAdjacentToGhost action =
                    let newPos = applyAction (pacmanPos currentState) action
                        ghosts = ghostPositions currentState
                        manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
                    in any (\g -> manhattan newPos g <= 1) ghosts
                
                -- Filter out dangerous moves
                safeChildren = filter (\(action, _) -> 
                    not (isImmediatelyDangerous action) && 
                    not (isAdjacentToGhost action)) children
                
                -- Use safe children if available, otherwise use all (last resort)
                childrenToConsider = if null safeChildren then children else safeChildren
            
            in if null childrenToConsider
                then MoveUp  -- Fallback
                else
                    -- Prioritize actions that are well-explored AND have good win rates
                    let scoreChild (action, child) =
                            let visits = nodeVisits child
                                winRate = if visits > 0 
                                          then nodeWins child / fromIntegral visits 
                                          else -10000.0
                                -- Need reasonable exploration (at least 5% of total iterations)
                                explorationThreshold = fromIntegral (nodeVisits node) * 0.05
                                explorationPenalty = if fromIntegral visits < explorationThreshold
                                                    then -1000.0
                                                    else 0.0
                                -- Heavily weight win rate for safety
                                score = winRate * 100.0 + fromIntegral visits + explorationPenalty
                            in (score, action)
                        
                        scored = map scoreChild childrenToConsider
                        bestChild = maximumBy (comparing fst) scored
                    in snd bestChild
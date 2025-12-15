{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module MCTS
    ( MCTSNode(..)
    , MCTSParams(..)
    , selectBestAction
    , mcts
    , mctsParallel
    , mctsParallelChunked  -- New: better parallel strategy
    ) where

import Types
import GameExecution (executeAction, isValidPosition, applyAction)
import System.Random
import Control.Monad.State hiding (state)
import qualified Control.Monad.State as ST
import Data.List (maximumBy, foldl')
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Parallel.Strategies
import Control.Monad (replicateM)
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Data.Vector as V
import Control.Concurrent.Async (mapConcurrently)

-- MCTS tree node
data MCTSNode = MCTSNode
    { nodeState :: !GameState
    , nodeAction :: !(Maybe Action)
    , nodeVisits :: !Int
    , nodeWins :: !Double
    , nodeChildren :: !(Map Action MCTSNode)
    , nodeUntriedActions :: ![Action]
    } deriving (Show, Generic)

instance NFData MCTSNode where
    rnf (MCTSNode s a v w c u) = 
        rnf s `seq` rnf a `seq` rnf v `seq` rnf w `seq` rnf c `seq` rnf u

-- MCTS hyperparameters
data MCTSParams = MCTSParams
    { mctsIterations :: Int      -- Number of iterations per tree
    , mctsExploration :: Double  -- UCB1 exploration constant (√2 ≈ 1.41)
    , mctsMaxDepth :: Int        -- Maximum simulation depth
    } deriving (Show)

-- Get valid actions for current state
legalActions :: GameState -> [Action]
legalActions gs = 
    let g = grid gs
        pos = pacmanPos gs
    in filter (\action -> isValidPosition g (applyAction pos action)) allActions

-- UCB1 selection formula
ucb1 :: Double -> Int -> Double -> Int -> Double
ucb1 explorationConstant parentVisits nodeWins nodeVisits
    | nodeVisits == 0 = 1.0e308
    | parentVisits == 0 = 0.0
    | otherwise = 
        let exploitation = nodeWins / fromIntegral nodeVisits
            exploration = explorationConstant * sqrt (log (fromIntegral parentVisits) / fromIntegral nodeVisits)
        in exploitation + exploration

-- Select child with highest UCB1 score
selectChild :: Double -> MCTSNode -> MCTSNode
selectChild explorationConstant node =
    let children = Map.elems (nodeChildren node)
        parentVisits = nodeVisits node
        scores = map (\child -> 
            (ucb1 explorationConstant parentVisits (nodeWins child) (nodeVisits child), child)) children
    in snd $ maximumBy (comparing fst) scores

-- Expand node by adding one untried action
expand :: RandomGen g => MCTSNode -> State g MCTSNode
expand node = do
    case nodeUntriedActions node of
        [] -> return node
        actions -> do
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
            return $ node 
                { nodeChildren = Map.insert action newNode (nodeChildren node)
                , nodeUntriedActions = rest
                }

-- Simulation phase with intelligent rollout policy
simulate :: RandomGen g => GameState -> Int -> State g Double
simulate initialState maxDepth = go initialState 0 0.0 []
    where
        go state depth totalReward visited
            | depth >= maxDepth = return totalReward
            | isTerminal state = 
                if pelletsRemaining state == 0
                    then return (totalReward + 1000.0)
                    else return (totalReward - 1000.0)
            | otherwise = do
                let validActions = legalActions state
                
                if null validActions
                    then return (totalReward - 500.0)
                    else do
                        let safeActions = filter (not . isImmediatelyDangerous state) validActions
                            actionsToUse = if null safeActions then validActions else safeActions
                        
                        r <- ST.state random
                        action <- if r < (0.8 :: Double)
                            then return $ greedyAction state actionsToUse visited
                            else do
                                idx <- ST.state $ randomR (0, length actionsToUse - 1)
                                return $ actionsToUse !! idx
                        
                        let (newState, reward) = executeAction state action
                            currentPos = pacmanPos newState
                            
                            ghostPenalty = ghostProximityPenalty newState
                            revisitPenalty = if currentPos `elem` take 5 visited then -25.0 else 0.0
                            
                            adjustedReward = reward + ghostPenalty + revisitPenalty
                            discountedReward = totalReward + adjustedReward * (0.95 ^ depth)
                        
                        go newState (depth + 1) discountedReward (currentPos : visited)
        
        isImmediatelyDangerous state action =
            let newPos = applyAction (pacmanPos state) action
                ghosts = ghostPositions state
                manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
            in any (\g -> manhattan newPos g <= 1) ghosts
        
        ghostProximityPenalty gs =
            let pos = pacmanPos gs
                ghosts = ghostPositions gs
                manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
                distances = [manhattan pos g | g <- ghosts]
                minDist = if null distances then 100 else minimum distances
            in case minDist of
                0 -> -1000.0
                1 -> -500.0
                2 -> -150.0
                3 -> -50.0
                4 -> -15.0
                5 -> -5.0
                _ -> 0.0
        
        greedyAction state actions visited =
            let pos = pacmanPos state
                g = grid state
                ghosts = ghostPositions state
                
                scoreAction action =
                    let newPos = applyAction pos action
                        manhattan (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)
                        
                        ghostDistances = [manhattan newPos gh | gh <- ghosts]
                        minGhostDist = if null ghostDistances then 100.0 else minimum ghostDistances
                        ghostScore = case minGhostDist of
                            d | d <= 1.0 -> -100000.0
                            d | d <= 2.0 -> -10000.0
                            d | d <= 3.0 -> -1000.0
                            d | d <= 4.0 -> d * 50.0
                            d -> d * 100.0
                        
                        nearbyGhosts = length $ filter (<= 3.0) ghostDistances
                        surroundScore = fromIntegral nearbyGhosts * (-500.0)
                        
                        pelletDist = nearestPelletDist g newPos
                        pelletScore = negate pelletDist * 20.0
                        
                        onPelletBonus = if (g V.! snd newPos V.! fst newPos) == Pellet 
                                       then 500.0 else 0.0
                        
                        revisitPenalty = case length $ filter (== newPos) visited of
                            0 -> 0.0
                            1 -> -200.0
                            2 -> -500.0
                            _ -> -1000.0
                        
                        oscillationPenalty = if not (null visited) && newPos == head visited
                                            then -2000.0
                                            else 0.0
                        
                        currentPelletDist = nearestPelletDist g pos
                        progressBonus = if pelletDist < currentPelletDist
                                       then 100.0
                                       else if pelletDist > currentPelletDist
                                       then -100.0
                                       else -50.0
                        
                    in ghostScore + surroundScore + pelletScore + onPelletBonus + revisitPenalty + oscillationPenalty + progressBonus
                
                scoredActions = [(action, scoreAction action) | action <- actions]
            in fst $ maximumBy (comparing snd) scoredActions
        
        nearestPelletDist g pos =
            let rows = V.length g
                cols = if rows > 0 then V.length (g V.! 0) else 0
                pelletPositions = [(x, y) | y <- [0..rows-1], 
                                            x <- [0..cols-1],
                                            x < V.length (g V.! y),
                                            (g V.! y V.! x) == Pellet]
                manhattan (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)
            in if null pelletPositions 
               then 100.0 
               else minimum [manhattan pos p | p <- pelletPositions]

-- Backpropagation: update nodes along path
backpropagate :: Double -> [MCTSNode] -> [MCTSNode]
backpropagate result = map (\node -> node 
    { nodeVisits = nodeVisits node + 1
    , nodeWins = nodeWins node + result
    })

-- Single MCTS iteration
mctsIteration :: RandomGen g => MCTSParams -> MCTSNode -> State g MCTSNode
mctsIteration params rootNode = do
    (selectedNode, path) <- selectAndExpand params rootNode []
    result <- simulate (nodeState selectedNode) (mctsMaxDepth params)
    
    let updatedPath = backpropagate result (selectedNode : path)
    return $ reconstructTree updatedPath
    where
        reconstructTree [] = rootNode
        reconstructTree [node] = node
        reconstructTree (child:parent:rest) =
            let action = case nodeAction child of
                    Just a -> a
                    Nothing -> error "Child must have action"
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

-- Sequential MCTS: build tree with N iterations
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

-- IMPROVED: Parallel MCTS with better chunking strategy
-- Instead of building separate trees, run iterations in parallel and merge incrementally
mctsParallelChunked :: Int -> MCTSParams -> GameState -> IO MCTSNode
mctsParallelChunked numThreads params initialState = do
    let totalIterations = mctsIterations params
        iterationsPerThread = max 10 (totalIterations `div` numThreads)
    
    -- Create initial root node
    let rootNode = MCTSNode
            { nodeState = initialState
            , nodeAction = Nothing
            , nodeVisits = 0
            , nodeWins = 0
            , nodeChildren = Map.empty
            , nodeUntriedActions = legalActions initialState
            }
    
    seeds <- replicateM numThreads randomIO
    
    -- Each thread runs iterations starting from rootNode
    -- This parallelizes the ITERATION work, not separate trees
    let params' = params { mctsIterations = iterationsPerThread }
    
    trees <- mapConcurrently (\seed -> do
        -- Each thread builds its own tree from the same starting state
        let tree = evalState (mcts params' initialState) (mkStdGen seed)
        return $! force tree
        ) seeds
    
    -- Merge all trees together
    return $! mergeTrees trees

-- Original parallel version (kept for compatibility)
mctsParallel :: Int -> MCTSParams -> GameState -> IO MCTSNode
mctsParallel = mctsParallelChunked

-- OPTIMIZED: Merge multiple MCTS trees with strict evaluation
mergeTrees :: [MCTSNode] -> MCTSNode
mergeTrees [] = error "Cannot merge empty list"
mergeTrees [single] = single
mergeTrees (first:rest) = 
    let !combinedChildren = foldl' mergeChildMaps (nodeChildren first) (map nodeChildren rest)
        !totalVisits = sum $ map nodeVisits (first:rest)
        !totalWins = sum $ map nodeWins (first:rest)
    in MCTSNode
        { nodeState = nodeState first
        , nodeAction = nodeAction first
        , nodeVisits = totalVisits
        , nodeWins = totalWins
        , nodeChildren = combinedChildren
        , nodeUntriedActions = nodeUntriedActions first
        }
    where
        foldl' = Data.List.foldl'
        
        mergeChildMaps m1 m2 = Map.foldlWithKey' (\acc k v ->
            case Map.lookup k acc of
                Nothing -> Map.insert k v acc
                Just existing -> Map.insert k (combineNodes existing v) acc
            ) m1 m2
        
        combineNodes n1 n2 = 
            let !v = nodeVisits n1 + nodeVisits n2
                !w = nodeWins n1 + nodeWins n2
                !c = Map.unionWith combineNodes (nodeChildren n1) (nodeChildren n2)
            in MCTSNode
                { nodeState = nodeState n1
                , nodeAction = nodeAction n1
                , nodeVisits = v
                , nodeWins = w
                , nodeChildren = c
                , nodeUntriedActions = nodeUntriedActions n1
                }

-- Select best action from root node
selectBestAction :: MCTSNode -> Action
selectBestAction node =
    let children = Map.toList (nodeChildren node)
    in if null children
        then MoveUp
        else
            let currentState = nodeState node
                ghosts = ghostPositions currentState
                recentPos = recentPositions currentState
                g = grid currentState
                
                manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
                
                isDeadlyAction (action, child) =
                    let childState = nodeState child
                        newPos = pacmanPos childState
                    in isTerminal childState && pelletsRemaining childState > 0
                       || any (\gh -> manhattan newPos gh <= 1) ghosts
                
                isTrappingAction (action, child) =
                    let childState = nodeState child
                        newPos = pacmanPos childState
                        escapeActions = filter (\a -> isValidPosition g (applyAction newPos a)) allActions
                        safeEscapes = filter (\a -> 
                            let escapePos = applyAction newPos a
                                escapeGhostDists = [fromIntegral $ manhattan escapePos gh | gh <- ghosts]
                                minEscapeDist = if null escapeGhostDists then 100.0 else minimum escapeGhostDists
                            in minEscapeDist > 2.0
                            ) escapeActions
                    in length safeEscapes == 0
                
                isOscillation (action, child) =
                    let newPos = pacmanPos (nodeState child)
                    in newPos `elem` take 3 recentPos
                
                safeChildren = filter (not . isDeadlyAction) children
                nonTrapping = filter (not . isTrappingAction) safeChildren
                nonOscillating = filter (not . isOscillation) nonTrapping
                
                childrenToConsider = if not (null nonOscillating)
                                    then nonOscillating
                                    else if not (null nonTrapping)
                                    then nonTrapping
                                    else if not (null safeChildren)
                                    then safeChildren
                                    else children
                
                scored = map (\(action, child) ->
                    let visits = nodeVisits child
                        winRate = if visits > 0 
                                  then nodeWins child / fromIntegral visits 
                                  else -10000.0
                        explorationThreshold = fromIntegral (nodeVisits node) * 0.03
                        explorationPenalty = if fromIntegral visits < explorationThreshold
                                            then -5000.0
                                            else 0.0
                        
                        childPos = pacmanPos (nodeState child)
                        recentPosPenalty = if childPos `elem` take 5 recentPos
                                          then -2000.0
                                          else 0.0
                        
                        score = winRate * 1000.0 + fromIntegral visits + explorationPenalty + recentPosPenalty
                    in (score, action)
                    ) childrenToConsider
            in if null scored
                then MoveUp
                else snd $ maximumBy (comparing fst) scored
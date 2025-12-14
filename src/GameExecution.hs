module GameExecution
    ( executeAction
    , applyAction
    , isValidPosition
    ) where

import Types
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import Data.List (sortBy, minimumBy, nub, maximumBy)
import Data.Ord (comparing)

-- Execute an action and return new state and reward
executeAction :: GameState -> Action -> (GameState, Double)
executeAction state action =
    let newPos = applyAction (pacmanPos state) action
        validPos = isValidPosition (grid state) newPos
    in if not validPos
        then (state, -1.0)  -- Hit wall, negative reward
        else
            let -- Track recent positions (keep last 10)
                updatedRecent = take 10 (newPos : recentPositions state)
                newState = state { pacmanPos = newPos, recentPositions = updatedRecent }
                -- Check collision BEFORE moving ghosts
                collisionBeforeGhosts = newPos `elem` ghostPositions newState
            in if collisionBeforeGhosts
                then (newState { isTerminal = True, deathPos = Just newPos }, -100.0)
                else
                    let stateAfterPellet = checkPelletCollection newState
                        finalState = moveGhosts stateAfterPellet
                        -- Check collision AFTER moving ghosts
                        collisionAfterGhosts = pacmanPos finalState `elem` ghostPositions finalState
                    in if collisionAfterGhosts
                        then (finalState { isTerminal = True, deathPos = Just (pacmanPos finalState) }, -100.0)
                        else (finalState, snd $ getPelletReward stateAfterPellet)

-- Apply action to position
applyAction :: Position -> Action -> Position
applyAction (x, y) MoveUp    = (x, y - 1)
applyAction (x, y) MoveDown  = (x, y + 1)
applyAction (x, y) MoveLeft  = (x - 1, y)
applyAction (x, y) MoveRight = (x + 1, y)

-- Check if position is valid (not a wall)
isValidPosition :: Vector (Vector Cell) -> Position -> Bool
isValidPosition g (x, y) =
    let rows = V.length g
        cols = if rows > 0 then V.length (g V.! 0) else 0
    in x >= 0 && x < cols && y >= 0 && y < rows && 
        (g V.! y V.! x) /= Wall

-- Check for pellet collection and update state
checkPelletCollection :: GameState -> GameState
checkPelletCollection state =
    let pos = pacmanPos state
        cell = grid state V.! snd pos V.! fst pos
    in case cell of
        Pellet -> 
            let newGrid = updateGrid (grid state) pos Empty
                newPellets = pelletsRemaining state - 1
                newScore = score state + 10
                terminal = newPellets == 0
            in state { grid = newGrid
                     , pelletsRemaining = newPellets
                     , score = newScore
                     , isTerminal = terminal
                     }
        _ -> state

-- Get reward for pellet collection
getPelletReward :: GameState -> (GameState, Double)
getPelletReward state =
    let pos = pacmanPos state
        cell = grid state V.! snd pos V.! fst pos
    in case cell of
        Pellet -> (state, 50.0)  -- Increased from 10.0 - much bigger reward for pellets!
        _ -> (state, -0.1)

-- BFS to find shortest path distance considering walls
bfsDistance :: Vector (Vector Cell) -> Position -> Position -> Maybe Int
bfsDistance g start target
    | start == target = Just 0
    | otherwise = bfs (Set.singleton start) (Map.singleton start 0) [(start, 0)]
    where
        bfs visited distances [] = Map.lookup target distances
        bfs visited distances ((pos, dist):queue)
            | pos == target = Just dist
            | otherwise =
                let neighbors = [applyAction pos a | a <- allActions]
                    validNeighbors = filter (\p -> isValidPosition g p && not (Set.member p visited)) neighbors
                    newVisited = foldr Set.insert visited validNeighbors
                    newDistances = foldr (\p -> Map.insert p (dist + 1)) distances validNeighbors
                    newQueue = queue ++ [(p, dist + 1) | p <- validNeighbors]
                in bfs newVisited newDistances newQueue

-- Get all positions reachable by Pac-Man within N moves (his escape zone)
getPacmanEscapeZone :: Vector (Vector Cell) -> Position -> Int -> [Position]
getPacmanEscapeZone g pacPos maxDist = go (Set.singleton pacPos) [pacPos] 0
    where
        go visited current dist
            | dist >= maxDist = Set.toList visited
            | otherwise =
                let neighbors = [applyAction p a | p <- current, a <- allActions]
                    validNeighbors = filter (\p -> isValidPosition g p && not (Set.member p visited)) neighbors
                    newVisited = foldr Set.insert visited validNeighbors
                in if null validNeighbors
                   then Set.toList visited
                   else go newVisited validNeighbors (dist + 1)

-- Find the cardinal directions relative to Pac-Man (N, S, E, W)
getCardinalDirection :: Position -> Position -> Maybe (Int, Int)
getCardinalDirection pacPos ghostPos =
    let (px, py) = pacPos
        (gx, gy) = ghostPos
        dx = gx - px
        dy = gy - py
        -- Return normalized direction vector
    in if abs dx > abs dy
       then Just (signum dx, 0)  -- East or West
       else Just (0, signum dy)  -- North or South

-- Assign ghost roles: some chase, some cut off
-- Strategy: Nearest ghost chases, others try to flank/cut off
assignGhostRoles :: Position -> [Position] -> [(Position, GhostRole)]
assignGhostRoles pacPos ghosts =
    let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
        ghostsWithDist = [(g, manhattan pacPos g) | g <- ghosts]
        sorted = sortBy (comparing snd) ghostsWithDist
    in case sorted of
        [] -> []
        (closest:rest) -> 
            -- Closest ghost chases
            (fst closest, Chaser) : 
            -- Other ghosts get flanking roles based on their position
            assignFlankingRoles pacPos (map fst rest)

data GhostRole = Chaser | Flanker | Interceptor
    deriving (Eq, Show)

-- Assign flanking roles to ghosts based on their position relative to Pac-Man
assignFlankingRoles :: Position -> [Position] -> [(Position, GhostRole)]
assignFlankingRoles _ [] = []
assignFlankingRoles pacPos ghosts =
    let roles = cycle [Flanker, Interceptor]  -- Alternate between roles
    in zip ghosts roles

-- Calculate a score for a ghost move based on its role and strategy
scoreMoveForEncirclement :: Vector (Vector Cell) -> Position -> Position -> Position -> [Position] -> [Position] -> GhostRole -> Double
scoreMoveForEncirclement g ghostMove pacPos currentGhostPos otherGhosts allNewGhosts role =
    let manhattan (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)
        distToPacman = fromMaybe 999.0 (fmap fromIntegral $ bfsDistance g ghostMove pacPos)
        
        -- Penalty if another ghost is already going to this position
        collisionPenalty = if ghostMove `elem` allNewGhosts then 1000.0 else 0.0
        
        -- Bonus for blocking Pac-Man's escape routes
        pacmanNeighbors = [applyAction pacPos a | a <- allActions]
        validPacmanMoves = filter (isValidPosition g) pacmanNeighbors
        isBlockingEscape = if ghostMove `elem` validPacmanMoves then -100.0 else 0.0
        
        -- Get Pac-Man's escape zone (positions he can reach in 3 moves)
        escapeZone = getPacmanEscapeZone g pacPos 3
        isInEscapeZone = if ghostMove `elem` escapeZone then -50.0 else 0.0
        
        -- Penalty for being too close to other ghosts (encourage spreading)
        minDistToOtherGhost = if null otherGhosts
            then 100.0
            else minimum [manhattan ghostMove og | og <- otherGhosts]
        clusteringPenalty = if minDistToOtherGhost < 2 then 30.0 else 0.0
        
        -- Role-specific scoring
        roleScore = case role of
            Chaser -> 
                -- Chaser: prioritize getting closer to Pac-Man
                distToPacman * 1.0
                
            Flanker ->
                -- Flanker: try to get to the side/ahead of Pac-Man
                let (px, py) = pacPos
                    (gx, gy) = ghostMove
                    -- Try to position perpendicular to Pac-Man's direction
                    pacmanRecentMove = if null (recentPositions (GameState g pacPos otherGhosts 0 0 False Nothing []))
                                      then (0, 0)
                                      else let (rx, ry) = head (recentPositions (GameState g pacPos otherGhosts 0 0 False Nothing []))
                                           in (px - rx, py - ry)
                    -- Position ahead in perpendicular direction
                    perpendicularBonus = if abs (gx - px) > abs (gy - py) 
                                        then -30.0  -- Positioned on horizontal axis
                                        else -30.0  -- Positioned on vertical axis
                in distToPacman * 1.2 + perpendicularBonus
                
            Interceptor ->
                -- Interceptor: try to cut off Pac-Man's path by positioning ahead
                let (px, py) = pacPos
                    (gx, gy) = ghostMove
                    -- Prefer positions that are roughly equal distance but "ahead" of Pac-Man
                    -- If Pac-Man is moving, get ahead of him
                    aheadBonus = if gx > px && gy > py then -40.0  -- NE quadrant
                                 else if gx < px && gy > py then -40.0  -- NW quadrant  
                                 else if gx > px && gy < py then -40.0  -- SE quadrant
                                 else -40.0  -- SW quadrant
                in distToPacman * 0.8 + aheadBonus  -- Less emphasis on direct distance
        
    in roleScore + collisionPenalty + isBlockingEscape + isInEscapeZone + clusteringPenalty

-- Find the best next move for a ghost with encirclement strategy
findBestGhostMoveEncirclement :: Vector (Vector Cell) -> Position -> Position -> Position -> [Position] -> [Position] -> GhostRole -> Position
findBestGhostMoveEncirclement g ghostPos pacPos currentGhostPos otherGhosts alreadyMovedGhosts role =
    let possibleMoves = [applyAction ghostPos a | a <- allActions]
        validMoves = filter (isValidPosition g) possibleMoves
        -- Remove moves that would put us on another ghost's position
        safeMoves = filter (\m -> not (m `elem` otherGhosts) && not (m `elem` alreadyMovedGhosts)) validMoves
        
        movesToConsider = if null safeMoves then [ghostPos] else safeMoves
        
        -- Score each move based on encirclement strategy
        movesWithScores = [(move, scoreMoveForEncirclement g move pacPos ghostPos otherGhosts alreadyMovedGhosts role) 
                          | move <- movesToConsider]
        
    in if null movesWithScores
        then ghostPos
        else fst $ minimumBy (comparing snd) movesWithScores

-- Ghost movement with smart encirclement
moveGhosts :: GameState -> GameState
moveGhosts state = 
    let pacPos = pacmanPos state
        g = grid state
        ghosts = ghostPositions state
        
        -- Assign roles to ghosts
        ghostRoles = assignGhostRoles pacPos ghosts
        
        -- Move ghosts sequentially with role-based strategy
        newGhosts = moveGhostsWithRoles g pacPos ghostRoles []
        
    in state { ghostPositions = newGhosts }
    where
        moveGhostsWithRoles g pacPos [] movedGhosts = reverse movedGhosts
        moveGhostsWithRoles g pacPos ((ghost, role):rest) movedGhosts =
            let remainingGhosts = map fst rest
                otherGhosts = remainingGhosts ++ movedGhosts
                newPos = findBestGhostMoveEncirclement g ghost pacPos ghost otherGhosts movedGhosts role
            in moveGhostsWithRoles g pacPos rest (newPos : movedGhosts)

-- Update grid cell
updateGrid :: Vector (Vector Cell) -> Position -> Cell -> Vector (Vector Cell)
updateGrid g (x, y) cell = g V.// [(y, (g V.! y) V.// [(x, cell)])]
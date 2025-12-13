module GameExecution
    ( executeAction
    , applyAction
    , isValidPosition
    ) where

import Types
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)

executeAction :: GameState -> Action -> (GameState, Double)
executeAction state action =
    let newPos = applyAction (pacmanPos state) action
        validPos = isValidPosition (grid state) newPos
        actualPacPos = if validPos then newPos else pacmanPos state
        stateAfterMove = state { pacmanPos = actualPacPos }
        collisionBeforeGhosts = actualPacPos `elem` ghostPositions stateAfterMove
    in if collisionBeforeGhosts
        then (stateAfterMove { isTerminal = True, deathPos = Just actualPacPos }, -100.0)
        else
            let
                stateAfterPellet = checkPelletCollection stateAfterMove
                finalState = moveGhosts stateAfterPellet
                collisionAfterGhosts = pacmanPos finalState `elem` ghostPositions finalState
                reward = if not validPos
                         then -1.0
                         else if collisionAfterGhosts
                         then -100.0
                         else snd $ getPelletReward stateAfterPellet
            in if collisionAfterGhosts
                then (finalState { isTerminal = True, deathPos = Just (pacmanPos finalState) }, reward)
                else (finalState, reward)

applyAction :: Position -> Action -> Position
applyAction (x, y) MoveUp    = (x, y - 1)
applyAction (x, y) MoveDown  = (x, y + 1)
applyAction (x, y) MoveLeft  = (x - 1, y)
applyAction (x, y) MoveRight = (x + 1, y)

isValidPosition :: Vector (Vector Cell) -> Position -> Bool
isValidPosition g (x, y) =
    let rows = V.length g
        cols = if rows > 0 then V.length (g V.! 0) else 0
    in x >= 0 && x < cols && y >= 0 && y < rows && 
        (g V.! y V.! x) /= Wall

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

getPelletReward :: GameState -> (GameState, Double)
getPelletReward state =
    let pos = pacmanPos state
        cell = grid state V.! snd pos V.! fst pos
    in case cell of
        Pellet -> (state, 10.0)
        _ -> (state, -0.1)

findPathBFS :: Vector (Vector Cell) -> Position -> Position -> [Position] -> Maybe [Position]
findPathBFS grid start goal occupiedPositions
    | start == goal = Just [goal]
    | otherwise = bfs [start] (Set.singleton start) (Map.singleton start Nothing)
    where
        bfs [] _ _ = Nothing
        bfs (current:queue) visited parents
            | current == goal = Just (reconstructPath current parents)
            | otherwise =
                let neighbors = getValidNeighbors grid current occupiedPositions
                    unvisited = filter (`Set.notMember` visited) neighbors
                    newVisited = foldr Set.insert visited unvisited
                    newParents = foldr (\n -> Map.insert n (Just current)) parents unvisited
                    newQueue = queue ++ unvisited
                in bfs newQueue newVisited newParents
        
        reconstructPath pos parents =
            case Map.lookup pos parents of
                Nothing -> [pos]
                Just Nothing -> [pos]
                Just (Just parent) -> reconstructPath parent parents ++ [pos]

getValidNeighbors :: Vector (Vector Cell) -> Position -> [Position] -> [Position]
getValidNeighbors grid pos occupiedPositions =
    let neighbors = [applyAction pos a | a <- allActions]
    in filter (\p -> isValidPosition grid p && p `notElem` occupiedPositions) neighbors

getBlockedDirections :: Position -> [Position] -> Vector (Vector Cell) -> [Action]
getBlockedDirections pacPos ghostPositions grid =
    let checkBlocked action =
            let targetPos = applyAction pacPos action
            in not (isValidPosition grid targetPos) || targetPos `elem` ghostPositions
    in filter checkBlocked allActions

getTrapTargets :: Position -> Vector (Vector Cell) -> [Position]
getTrapTargets pacPos grid =
    let targets = [applyAction pacPos a | a <- allActions]
    in filter (isValidPosition grid) targets

moveGhosts :: GameState -> GameState
moveGhosts state = state { ghostPositions = newPositions }
    where
        pacPos = pacmanPos state
        g = grid state
        oldPositions = ghostPositions state
        
        trapTargets = getTrapTargets pacPos g
        
        newPositions = moveGhostsCoordinated oldPositions trapTargets [] 0
        
        moveGhostsCoordinated [] _ acc _ = reverse acc
        moveGhostsCoordinated (gPos:rest) targets acc ghostIndex =
            let occupiedPositions = acc ++ rest
                newPos = if ghostIndex < 2
                         then moveGhostChase gPos occupiedPositions
                         else moveGhostTrap gPos targets occupiedPositions
            in moveGhostsCoordinated rest targets (newPos : acc) (ghostIndex + 1)
        
        moveGhostChase gPos occupiedPositions =
            case findPathBFS g gPos pacPos occupiedPositions of
                Just path | length path > 1 -> path !! 1
                _ -> moveGhostSimple gPos occupiedPositions
        
        moveGhostTrap gPos targets occupiedPositions =
            let targetDistances = [(t, manhattan gPos t) | t <- targets]
                sortedTargets = map fst $ sortBy (comparing snd) targetDistances
            in case sortedTargets of
                [] -> moveGhostSimple gPos occupiedPositions
                (target:_) -> case findPathBFS g gPos target occupiedPositions of
                    Just path | length path > 1 -> path !! 1
                    _ -> moveGhostSimple gPos occupiedPositions
        
        moveGhostSimple gPos occupiedPositions =
            let possibleMoves = [applyAction gPos a | a <- allActions]
                validMoves = filter (\p -> isValidPosition g p && p `notElem` occupiedPositions) possibleMoves
                closerMoves = filter (\p -> manhattan p pacPos < manhattan gPos pacPos) validMoves
            in if null closerMoves 
                then if null validMoves then gPos else head validMoves
                else head closerMoves

updateGrid :: Vector (Vector Cell) -> Position -> Cell -> Vector (Vector Cell)
updateGrid g (x, y) cell = g V.// [(y, (g V.! y) V.// [(x, cell)])]
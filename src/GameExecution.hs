module GameExecution
    ( executeAction
    , applyAction
    , isValidPosition
    ) where

import Types
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe (isJust)

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

moveGhosts :: GameState -> GameState
moveGhosts state = state { ghostPositions = newPositions }
    where
        pacPos = pacmanPos state
        g = grid state
        oldPositions = ghostPositions state
        
        newPositions = moveGhostsSequentially oldPositions []
        
        moveGhostsSequentially [] acc = reverse acc
        moveGhostsSequentially (gPos:rest) acc = 
            let occupiedPositions = acc ++ rest
                newPos = moveGhost gPos occupiedPositions
            in moveGhostsSequentially rest (newPos : acc)
        
        moveGhost gPos occupiedPositions = 
            let possibleMoves = [applyAction gPos a | a <- allActions]
                validMoves = filter (\p -> isValidPosition g p && p `notElem` occupiedPositions) possibleMoves
                closerMoves = filter (\p -> dist p pacPos < dist gPos pacPos) validMoves
            in if null closerMoves 
                then if null validMoves then gPos else head validMoves
                else head closerMoves
        
        dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

updateGrid :: Vector (Vector Cell) -> Position -> Cell -> Vector (Vector Cell)
updateGrid g (x, y) cell = g V.// [(y, (g V.! y) V.// [(x, cell)])]
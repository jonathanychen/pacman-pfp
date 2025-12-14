{-# LANGUAGE BangPatterns #-}

module QLearning
    ( stateKey
    , getQValue
    , chooseAction
    , bestAction
    , updateQValue
    , runEpisode
    , trainSequential
    ) where

import Types
import GameExecution (executeAction)
import qualified Data.Map.Strict as Map
import System.Random
import Control.Monad.State
import Data.Vector ((!))

ghostInDirectionWithinNTiles :: GameState -> Action -> Int -> Bool
ghostInDirectionWithinNTiles gs action n =
    let (x, y) = pacmanPos gs
        ghostPosSet = ghostPositions gs
        targetPos = case action of
            MoveUp    -> [(x, y - i) | i <- [1..n]]
            MoveDown  -> [(x, y + i) | i <- [1..n]]
            MoveLeft  -> [(x - i, y) | i <- [1..n]]
            MoveRight -> [(x + i, y) | i <- [1..n]]
    in any (`elem` ghostPosSet) targetPos

cellInDirection :: GameState -> Action -> Cell -> Bool
cellInDirection gs action cellType =
    let (x, y) = pacmanPos gs
        gridCells = grid gs
        targetPos = case action of
            MoveUp    -> (x, y - 1)
            MoveDown  -> (x, y + 1)
            MoveLeft  -> (x - 1, y)
            MoveRight -> (x + 1, y)
        (tx, ty) = targetPos
    in (ty >= 0 && ty < length gridCells && tx >= 0 && tx < length (gridCells ! ty)) &&
       ((gridCells ! ty) ! tx == cellType)

pelletInDirection :: GameState -> Action -> Bool
pelletInDirection gs action = cellInDirection gs action Pellet

wallInDirection :: GameState -> Action -> Bool
wallInDirection gs action = cellInDirection gs action Wall

-- Extract state key from full game state
stateKey :: GameState -> StateKey
stateKey gs = StateKey {
    skPacmanPos = pacmanPos gs,
    pelletLeft = pelletInDirection gs MoveLeft,
    pelletRight = pelletInDirection gs MoveRight,
    pelletUp = pelletInDirection gs MoveUp,
    pelletDown = pelletInDirection gs MoveDown,
    ghostLeft = ghostInDirectionWithinNTiles gs MoveLeft 3,
    ghostRight = ghostInDirectionWithinNTiles gs MoveRight 3,
    ghostUp = ghostInDirectionWithinNTiles gs MoveUp 3,
    ghostDown = ghostInDirectionWithinNTiles gs MoveDown 3,
    wallLeft = wallInDirection gs MoveLeft,
    wallRight = wallInDirection gs MoveRight,
    wallUp = wallInDirection gs MoveUp,
    wallDown = wallInDirection gs MoveDown
}

-- Get Q-value for state-action pair
getQValue :: QTable -> StateKey -> Action -> Double
getQValue qtable s a = Map.findWithDefault 0.0 (s, a) qtable

legalActions :: GameState -> [Action]
legalActions gs = filter (not . wallInDirection gs) allActions

-- Choose action using epsilon-greedy policy
chooseAction :: RandomGen g => GameState -> QTable -> StateKey -> Double -> State g Action
chooseAction gs qtable s eps = do
    r <- state random
    if r < eps
        then do  -- Explore: random action
            idx <- state $ randomR (0, length (legalActions gs) - 1)
            return $ legalActions gs !! idx
        else do  -- Exploit: best action
            return $ bestAction gs qtable s

-- Find best action for a state
bestAction :: GameState -> QTable -> StateKey -> Action
bestAction gs qtable s =
    let qValues = [(a, getQValue qtable s a) | a <- legalActions gs]
    in fst $ foldr1 (\x y -> if snd x > snd y then x else y) qValues

-- Update Q-value
updateQValue :: QTable -> StateKey -> Action -> Double -> StateKey -> QLearningParams -> QTable
updateQValue qtable s a reward s' params =
    let currentQ = getQValue qtable s a
        maxNextQ = maximum [getQValue qtable s' a' | a' <- allActions]
        newQ = currentQ + alpha params * (reward + gamma params * maxNextQ - currentQ)
    in Map.insert (s, a) newQ qtable

-- Run one episode of Q-learning
runEpisode :: RandomGen g => GameState -> QTable -> QLearningParams -> State g QTable
runEpisode initialState initialQTable params = go initialState initialQTable
    where
        go gs !qtable
            | isTerminal gs = return qtable
            | otherwise = do
                let s = stateKey gs
                action <- chooseAction gs qtable s (epsilon params)

                -- Execute action and get next state and reward
                let (nextState, reward) = executeAction gs action
                let s' = stateKey nextState

                -- Update Q-table
                let newQTable = updateQValue qtable s action reward s' params

                go nextState newQTable

-- Sequential training
trainSequential :: RandomGen g => GameState -> QLearningParams -> State g QTable
trainSequential initialState params = go Map.empty 0
    where
        go !qtable episodeNum
            | episodeNum >= episodes params = return qtable
            | otherwise = do
                newQTable <- runEpisode initialState qtable params
                go newQTable (episodeNum + 1)
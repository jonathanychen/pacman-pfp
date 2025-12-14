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
import Control.DeepSeq
import Data.Vector ((!))
import qualified Data.Vector as V

-- Helper: Check if ghost is in a direction (adjacent)
ghostInDirection :: GameState -> Action -> Bool
ghostInDirection gs action =
    let (x, y) = pacmanPos gs
        targetPos = case action of
            MoveUp    -> (x, y - 1)
            MoveDown  -> (x, y + 1)
            MoveLeft  -> (x - 1, y)
            MoveRight -> (x + 1, y)
    in targetPos `elem` ghostPositions gs

-- Helper: Check if cell type is in a direction (adjacent)
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
        rows = V.length gridCells
        cols = if rows > 0 then V.length (gridCells ! 0) else 0
    in tx >= 0 && tx < cols && ty >= 0 && ty < rows &&
       ((gridCells ! ty) ! tx == cellType)

pelletInDirection :: GameState -> Action -> Bool
pelletInDirection gs action = cellInDirection gs action Pellet

wallInDirection :: GameState -> Action -> Bool
wallInDirection gs action = cellInDirection gs action Wall

-- Check if any ghost is within Manhattan distance N
ghostNearby :: GameState -> Int -> Bool
ghostNearby gs maxDist =
    let (px, py) = pacmanPos gs
        ghosts = ghostPositions gs
        manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    in any (\gPos -> manhattan (px, py) gPos <= maxDist) ghosts

-- Check if any pellet is within Manhattan distance N
pelletNearby :: GameState -> Int -> Bool
pelletNearby gs maxDist =
    let (px, py) = pacmanPos gs
        gridCells = grid gs
        rows = V.length gridCells
        manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
        checkCell y x cell = 
            cell == Pellet && manhattan (px, py) (x, y) <= maxDist
    in V.any (\(y, row) -> V.any (\(x, cell) -> checkCell y x cell) (V.indexed row)) 
             (V.indexed gridCells)

-- Extract state key from full game state (improved representation)
stateKey :: GameState -> StateKey
stateKey gs = StateKey
    { pelletLeft = pelletInDirection gs MoveLeft
    , pelletRight = pelletInDirection gs MoveRight
    , pelletUp = pelletInDirection gs MoveUp
    , pelletDown = pelletInDirection gs MoveDown
    , ghostLeft = ghostInDirection gs MoveLeft
    , ghostRight = ghostInDirection gs MoveRight
    , ghostUp = ghostInDirection gs MoveUp
    , ghostDown = ghostInDirection gs MoveDown
    , wallLeft = wallInDirection gs MoveLeft
    , wallRight = wallInDirection gs MoveRight
    , wallUp = wallInDirection gs MoveUp
    , wallDown = wallInDirection gs MoveDown
    , ghostNear = ghostNearby gs 3
    , pelletNear = pelletNearby gs 3
    }

-- Get Q-value for state-action pair
getQValue :: QTable -> StateKey -> Action -> Double
getQValue qtable s a = Map.findWithDefault 0.0 (s, a) qtable

-- Get legal actions (not walls)
legalActions :: GameState -> [Action]
legalActions gs = filter (not . wallInDirection gs) allActions

-- Choose action using epsilon-greedy policy
chooseAction :: RandomGen g => GameState -> QTable -> StateKey -> Double -> State g Action
chooseAction gs qtable s eps = do
    r <- state random
    if r < eps
        then do  -- Explore: random legal action
            let legal = legalActions gs
            idx <- state $ randomR (0, length legal - 1)
            return $ legal !! idx
        else  -- Exploit: best action
            return $ bestAction qtable s

-- Find best action for a state
bestAction :: QTable -> StateKey -> Action
bestAction qtable s =
    let qValues = [(a, getQValue qtable s a) | a <- allActions]
    in fst $ foldr1 (\x y -> if snd x > snd y then x else y) qValues

-- Update Q-value using temporal difference learning
updateQValue :: QTable -> StateKey -> Action -> Double -> StateKey -> QLearningParams -> QTable
updateQValue qtable s a reward s' params =
    let currentQ = getQValue qtable s a
        maxNextQ = maximum [getQValue qtable s' a' | a' <- allActions]
        newQ = currentQ + alpha params * (reward + gamma params * maxNextQ - currentQ)
    in Map.insert (s, a) newQ qtable

-- Run one episode of Q-learning
runEpisode :: RandomGen g => GameState -> QTable -> QLearningParams -> State g QTable
runEpisode initialState initialQTable params = go initialState initialQTable 0
    where
        maxSteps = 500  -- Prevent infinite episodes
        
        go state !qtable steps
            | steps >= maxSteps = return qtable
            | isTerminal state = return qtable
            | otherwise = do
                let s = stateKey state
                action <- chooseAction state qtable s (epsilon params)
                
                -- Execute action and get next state and reward
                let (nextState, reward) = executeAction state action
                let s' = stateKey nextState
                
                -- Update Q-table
                let newQTable = updateQValue qtable s action reward s' params
                
                go nextState newQTable (steps + 1)

-- Sequential training
trainSequential :: RandomGen g => GameState -> QLearningParams -> State g QTable
trainSequential initialState params = go Map.empty 0
    where
        go !qtable episodeNum
            | episodeNum >= episodes params = return qtable
            | otherwise = do
                newQTable <- runEpisode initialState qtable params
                go newQTable (episodeNum + 1)
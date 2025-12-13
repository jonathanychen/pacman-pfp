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
import GameExecution (executeAction)  -- Import executeAction from GameExecution module
import qualified Data.Map.Strict as Map
import System.Random
import Control.Monad.State
import Control.DeepSeq

-- Extract state key from full game state
stateKey :: GameState -> StateKey
stateKey gs = (pacmanPos gs, ghostPositions gs, pelletsRemaining gs)

-- Get Q-value for state-action pair
getQValue :: QTable -> StateKey -> Action -> Double
getQValue qtable s a = Map.findWithDefault 0.0 (s, a) qtable

-- Choose action using epsilon-greedy policy
chooseAction :: RandomGen g => QTable -> StateKey -> Double -> State g Action
chooseAction qtable s eps = do
    r <- state random
    if r < eps
        then do  -- Explore: random action
            idx <- state $ randomR (0, length allActions - 1)
            return $ allActions !! idx
        else do  -- Exploit: best action
            return $ bestAction qtable s

-- Find best action for a state
bestAction :: QTable -> StateKey -> Action
bestAction qtable s = 
    let qValues = [(a, getQValue qtable s a) | a <- allActions]
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
        go state !qtable
            | isTerminal state = return qtable
            | otherwise = do
                let s = stateKey state
                action <- chooseAction qtable s (epsilon params)
                
                -- Execute action and get next state and reward
                let (nextState, reward) = executeAction state action
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
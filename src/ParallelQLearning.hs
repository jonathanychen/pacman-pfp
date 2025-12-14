{-# LANGUAGE BangPatterns #-}

module ParallelQLearning
    ( trainParallel
    , mergeQTables
    ) where

import Types
import QLearning
import qualified Data.Map.Strict as Map
import Control.Parallel.Strategies
import System.Random
import Control.Monad.State
import Data.List (foldl')
import Control.Monad (replicateM)
import Control.DeepSeq (force)

-- Parallel Q-learning: Run multiple episodes in parallel and merge Q-tables
trainParallel :: Int -> GameState -> QLearningParams -> IO QTable
trainParallel numCores initialState params = do
    let episodesPerCore = episodes params `div` numCores
    
    -- Generate random seeds for each core
    seeds <- replicateM numCores randomIO
    
    -- Run episodes in parallel on different cores
    let params' = params { episodes = episodesPerCore }
        qTables = parMap rdeepseq (\seed -> 
            force $ evalState (trainSequential initialState params') (mkStdGen seed)
            ) seeds
    
    -- Force evaluation of all Q-tables before merging
    let !forcedTables = force qTables
    
    -- Merge Q-tables (average Q-values)
    return $! mergeQTables forcedTables

-- Merge multiple Q-tables by averaging
mergeQTables :: [QTable] -> QTable
mergeQTables qtables = 
    let allKeys = concatMap Map.keys qtables
        uniqueKeys = foldl' (\acc k -> if k `elem` acc then acc else k:acc) [] allKeys
        
        avgValue key = 
            let values = [Map.findWithDefault 0.0 key qt | qt <- qtables]
                nonZeroValues = filter (/= 0.0) values
                count = length nonZeroValues
            in if count > 0 then sum nonZeroValues / fromIntegral count else 0.0
    
    in force $ Map.fromList [(k, avgValue k) | k <- uniqueKeys]
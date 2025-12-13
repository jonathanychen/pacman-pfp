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

-- Parallel Q-learning: Run multiple episodes in parallel and merge Q-tables
trainParallel :: Int -> GameState -> QLearningParams -> IO QTable
trainParallel numCores initialState params = do
    let episodesPerCore = episodes params `div` numCores
    
    -- Generate random seeds for each core
    seeds <- replicateM numCores randomIO
    
    -- Run episodes in parallel on different cores
    let qTables = parMap rdeepseq (\seed -> 
            let params' = params { episodes = episodesPerCore }
            in evalState (trainSequential initialState params') (mkStdGen seed)
            ) seeds
    
    -- Merge Q-tables (average Q-values)
    return $ mergeQTables qTables

-- Merge multiple Q-tables by averaging
mergeQTables :: [QTable] -> QTable
mergeQTables qtables = 
    let allKeys = concatMap Map.keys qtables
        uniqueKeys = foldr (\k acc -> if k `elem` acc then acc else k:acc) [] allKeys
        
        avgValue key = 
            let values = [Map.findWithDefault 0.0 key qt | qt <- qtables]
                count = length $ filter (/= 0.0) values
            in if count > 0 then sum values / fromIntegral count else 0.0
    
    in Map.fromList [(k, avgValue k) | k <- uniqueKeys]
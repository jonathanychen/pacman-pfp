{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Types
import Game
import MCTS
import GameExecution (executeAction, applyAction, isValidPosition)
import System.Environment (getArgs)
import System.Random
import Control.Monad.State
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import System.CPUTime (getCPUTime)
import Text.Printf
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM, replicateM)

data RunMode = Sequential | Parallel | BatchParallel
    deriving (Eq, Show)

data Config = Config
    { numCores :: Int
    , visualize :: Bool
    , runMode :: RunMode
    , gridFile :: FilePath
    , useGloss :: Bool
    , configIterations :: Int
    , maxSteps :: Int
    , batchSize :: Int  -- New: for batch parallelism
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { numCores = 1
    , visualize = False
    , runMode = Sequential
    , gridFile = "grids/large.txt"
    , useGloss = False
    , configIterations = 1000
    , maxSteps = 200
    , batchSize = 4
    }

main :: IO ()
main = do
    args <- getArgs
    config <- parseArgs args defaultConfig
    
    printf "Pac-Man MCTS Parallel Implementation\n"
    printf "====================================\n"
    printf "Configuration:\n"
    printf "  Mode: %s\n" (show $ runMode config)
    printf "  Cores: %d\n" (numCores config)
    printf "  MCTS Iterations: %d\n" (configIterations config)
    printf "  Visualization: %s\n" (show $ visualize config)
    printf "  Grid file: %s\n" (gridFile config)
    printf "  Max steps: %d\n" (maxSteps config)
    if runMode config == BatchParallel
        then printf "  Batch size: %d\n\n" (batchSize config)
        else printf "\n"
    
    initialState <- loadGrid (gridFile config)
    
    case runMode config of
        Sequential -> runSequential initialState config
        Parallel -> runParallel initialState config
        BatchParallel -> runBatchParallel initialState config

parseArgs :: [String] -> Config -> IO Config
parseArgs [] cfg = return cfg
parseArgs ("--cores":n:rest) cfg = parseArgs rest cfg { numCores = read n }
parseArgs ("-c":n:rest) cfg = parseArgs rest cfg { numCores = read n }
parseArgs ("--visualize":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("-v":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("--gloss":rest) cfg = parseArgs rest cfg { useGloss = True, visualize = True }
parseArgs ("--sequential":rest) cfg = parseArgs rest cfg { runMode = Sequential }
parseArgs ("--parallel":rest) cfg = parseArgs rest cfg { runMode = Parallel }
parseArgs ("--batch-parallel":rest) cfg = parseArgs rest cfg { runMode = BatchParallel }
parseArgs ("--grid":file:rest) cfg = parseArgs rest cfg { gridFile = file }
parseArgs ("--iterations":n:rest) cfg = parseArgs rest cfg { configIterations = read n }
parseArgs ("-i":n:rest) cfg = parseArgs rest cfg { configIterations = read n }
parseArgs ("--steps":n:rest) cfg = parseArgs rest cfg { maxSteps = read n }
parseArgs ("--batch-size":n:rest) cfg = parseArgs rest cfg { batchSize = read n }
parseArgs (unknown:rest) cfg = do
    printf "Warning: Unknown argument '%s'\n" unknown
    parseArgs rest cfg

runSequential :: GameState -> Config -> IO ()
runSequential initialState config = do
    let params = MCTSParams
            { mctsIterations = configIterations config
            , mctsExploration = 1.41
            , mctsMaxDepth = 200
            }
    
    printf "Running Sequential MCTS...\n"
    
    wallStart <- getTime Monotonic
    cpuStart <- getCPUTime
    (finalState, totalNodes) <- runMCTSGame initialState params (maxSteps config) (visualize config) Nothing
    cpuEnd <- getCPUTime
    wallEnd <- getTime Monotonic
    
    let wallTime = fromIntegral (toNanoSecs (wallEnd - wallStart)) / 1e9 :: Double
        cpuTime = fromIntegral (cpuEnd - cpuStart) / 1e12 :: Double
    
    printf "\n=== Sequential Results ===\n"
    printf "Wall-clock time: %.3f seconds\n" wallTime
    printf "CPU time: %.3f seconds\n" cpuTime
    printf "Time per step: %.3f ms\n" ((wallTime * 1000) / fromIntegral (maxSteps config))
    printf "Total tree nodes explored: %d\n" totalNodes
    printf "Final Score: %d\n" (score finalState)
    printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
    printf "Won: %s\n" (if pelletsRemaining finalState == 0 then "Yes" else "No")

runParallel :: GameState -> Config -> IO ()
runParallel initialState config = do
    let params = MCTSParams
            { mctsIterations = configIterations config
            , mctsExploration = 1.41
            , mctsMaxDepth = 200
            }
    
    printf "Running Parallel MCTS with %d cores (NO tree merging!)...\n" (numCores config)
    
    start <- getTime Monotonic
    (finalState, totalNodes) <- runMCTSGame initialState params (maxSteps config) (visualize config) (Just $ numCores config)
    end <- getTime Monotonic
    
    let diff = fromIntegral (toNanoSecs (end - start)) / 1e9 :: Double
    
    printf "\n=== Parallel Results (No Merging) ===\n"
    printf "Total time: %.3f seconds\n" diff
    printf "Time per step: %.3f ms\n" ((diff * 1000) / fromIntegral (maxSteps config))
    printf "Total tree nodes explored: %d\n" totalNodes
    printf "Final Score: %d\n" (score finalState)
    printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
    printf "Won: %s\n" (if pelletsRemaining finalState == 0 then "Yes" else "No")

-- NEW: Batch parallel - process multiple steps in parallel
runBatchParallel :: GameState -> Config -> IO ()
runBatchParallel initialState config = do
    let params = MCTSParams
            { mctsIterations = configIterations config
            , mctsExploration = 1.41
            , mctsMaxDepth = 200
            }
    
    printf "Running Batch Parallel MCTS with %d cores...\n" (numCores config)
    printf "Processing %d steps in batches of %d\n\n" (maxSteps config) (batchSize config)
    
    start <- getTime Monotonic
    (finalState, totalNodes) <- runMCTSGameBatched initialState params (maxSteps config) (batchSize config) (visualize config)
    end <- getTime Monotonic
    
    let diff = fromIntegral (toNanoSecs (end - start)) / 1e9 :: Double
    
    printf "\n=== Batch Parallel Results ===\n"
    printf "Total time: %.3f seconds\n" diff
    printf "Time per step: %.3f ms\n" ((diff * 1000) / fromIntegral (maxSteps config))
    printf "Total tree nodes explored: %d\n" totalNodes
    printf "Final Score: %d\n" (score finalState)
    printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
    printf "Won: %s\n" (if pelletsRemaining finalState == 0 then "Yes" else "No")
    printf "\nSpeedup vs sequential should be visible with large batches!\n"

-- Original sequential/parallel per-step approach
runMCTSGame :: GameState -> MCTSParams -> Int -> Bool -> Maybe Int -> IO (GameState, Int)
runMCTSGame initialState params maxSteps visualize maybeNumCores = 
    go initialState 0 0
    where
        go state step totalNodes
            | step >= maxSteps = return (state, totalNodes)
            | isTerminal state = return (state, totalNodes)
            | otherwise = do
                tree <- case maybeNumCores of
                    Nothing -> do
                        seed <- randomIO
                        return $! evalState (mcts params state) (mkStdGen seed)
                    Just cores -> mctsParallel cores params state  -- Uses simple aggregation now!
                
                _ <- evaluate (force tree)
                
                let !action = selectBestAction tree
                    !nodes = nodeVisits tree
                    (!newState, _) = executeAction state action
                
                when visualize $ do
                    renderGame state
                    printf "Step %d: MCTS selected %s (visits: %d)\n" 
                        step (show action) nodes
                    threadDelay 300000
                
                go newState (step + 1) (totalNodes + nodes)

-- NEW: Batch processing - compute multiple MCTS trees in parallel
runMCTSGameBatched :: GameState -> MCTSParams -> Int -> Int -> Bool -> IO (GameState, Int)
runMCTSGameBatched initialState params maxSteps batchSize visualize =
    go initialState 0 0
    where
        go state step totalNodes
            | step >= maxSteps = return (state, totalNodes)
            | isTerminal state = return (state, totalNodes)
            | otherwise = do
                -- Process next batch of steps in parallel
                let remainingSteps = maxSteps - step
                    currentBatchSize = min batchSize remainingSteps
                
                -- Simulate multiple futures in parallel
                results <- processBatch state currentBatchSize params
                
                -- Take the best action from the first step
                let (action, nodes) = head results
                    (newState, _) = executeAction state action
                
                when visualize $ do
                    renderGame state
                    printf "Step %d: MCTS selected %s (visits: %d) [batch of %d]\n" 
                        step (show action) nodes currentBatchSize
                    threadDelay 300000
                
                go newState (step + 1) (totalNodes + nodes)

-- Process a batch of game steps in parallel
-- Returns list of (action, node_count) for each step
processBatch :: GameState -> Int -> MCTSParams -> IO [(Action, Int)]
processBatch initialState batchSize params = do
    -- Generate future states by following a greedy rollout
    futureStates <- return $ generateFutureStates initialState batchSize
    
    -- Build MCTS trees for all future states in parallel
    seeds <- replicateM batchSize randomIO
    
    trees <- mapConcurrently (\(state, seed) -> do
        let tree = evalState (mcts params state) (mkStdGen seed)
        _ <- evaluate (force tree)
        return tree
        ) (zip futureStates seeds)
    
    -- Extract best actions
    return [(selectBestAction tree, nodeVisits tree) | tree <- trees]

-- Generate future game states using greedy policy (fast lookahead)
generateFutureStates :: GameState -> Int -> [GameState]
generateFutureStates initialState count = take count $ iterate nextState initialState
    where
        nextState state
            | isTerminal state = state
            | otherwise =
                let validActions = filter (\action -> 
                        let pos = pacmanPos state
                            g = grid state
                            newPos = applyAction pos action
                        in isValidPosition g newPos
                        ) allActions
                in if null validActions
                   then state
                   else
                       let action = head validActions  -- Simple: just take first valid action
                           (newState, _) = executeAction state action
                       in newState
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Types
import Game
import MCTS
import System.Environment (getArgs)
import System.Random
import Control.Monad.State
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import Text.Printf
import qualified Data.Map.Strict as Map
import Control.Concurrent (threadDelay)

data RunMode = Sequential | Parallel
    deriving (Eq, Show)

data Config = Config
    { numCores :: Int
    , visualize :: Bool
    , runMode :: RunMode
    , gridFile :: FilePath
    , useGloss :: Bool
    , configIterations :: Int
    , maxSteps :: Int
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
    printf "  Max steps: %d\n\n" (maxSteps config)
    
    initialState <- loadGrid (gridFile config)
    
    case runMode config of
        Sequential -> runSequential initialState config
        Parallel -> runParallel initialState config

parseArgs :: [String] -> Config -> IO Config
parseArgs [] cfg = return cfg
parseArgs ("--cores":n:rest) cfg = parseArgs rest cfg { numCores = read n }
parseArgs ("-c":n:rest) cfg = parseArgs rest cfg { numCores = read n }
parseArgs ("--visualize":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("-v":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("--gloss":rest) cfg = parseArgs rest cfg { useGloss = True, visualize = True }
parseArgs ("--sequential":rest) cfg = parseArgs rest cfg { runMode = Sequential }
parseArgs ("--parallel":rest) cfg = parseArgs rest cfg { runMode = Parallel }
parseArgs ("--grid":file:rest) cfg = parseArgs rest cfg { gridFile = file }
parseArgs ("--iterations":n:rest) cfg = parseArgs rest cfg { configIterations = read n }
parseArgs ("-i":n:rest) cfg = parseArgs rest cfg { configIterations = read n }
parseArgs ("--steps":n:rest) cfg = parseArgs rest cfg { maxSteps = read n }
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
    
    start <- getTime Monotonic
    (finalState, totalNodes) <- runMCTSGame initialState params (maxSteps config) (visualize config) Nothing
    end <- getTime Monotonic
    
    let diff = fromIntegral (toNanoSecs (end - start)) / 1e9 :: Double
    
    printf "\n=== Sequential Results ===\n"
    printf "Total time: %.3f seconds\n" diff
    printf "Time per step: %.3f ms\n" ((diff * 1000) / fromIntegral (maxSteps config))
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
    
    printf "Running Parallel MCTS with %d cores...\n" (numCores config)
    
    start <- getTime Monotonic
    (finalState, totalNodes) <- runMCTSGame initialState params (maxSteps config) (visualize config) (Just $ numCores config)
    end <- getTime Monotonic
    
    let diff = fromIntegral (toNanoSecs (end - start)) / 1e9 :: Double
    
    printf "\n=== Parallel Results ===\n"
    printf "Total time: %.3f seconds\n" diff
    printf "Time per step: %.3f ms\n" ((diff * 1000) / fromIntegral (maxSteps config))
    printf "Total tree nodes explored: %d\n" totalNodes
    printf "Final Score: %d\n" (score finalState)
    printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
    printf "Won: %s\n" (if pelletsRemaining finalState == 0 then "Yes" else "No")

-- Run MCTS game step by step (no tree reuse between steps)
runMCTSGame :: GameState -> MCTSParams -> Int -> Bool -> Maybe Int -> IO (GameState, Int)
runMCTSGame initialState params maxSteps visualize maybeNumCores = 
    go initialState 0 0
    where
        go state step totalNodes
            | step >= maxSteps = return (state, totalNodes)
            | isTerminal state = return (state, totalNodes)
            | otherwise = do
                -- Build fresh MCTS tree for this step (no reuse)
                tree <- case maybeNumCores of
                    Nothing -> do
                        seed <- randomIO
                        return $ evalState (mcts params state) (mkStdGen seed)
                    Just cores -> mctsParallel cores params state
                
                let action = selectBestAction tree
                    nodes = nodeVisits tree
                    (newState, _) = executeAction state action
                
                when visualize $ do
                    renderGame state
                    printf "Step %d: MCTS selected %s (visits: %d)\n" 
                        step (show action) nodes
                    threadDelay 300000
                
                go newState (step + 1) (totalNodes + nodes)
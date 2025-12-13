module Main where

import Types
import Game
import GlossVisual
import QLearning
import ParallelQLearning
import System.Environment (getArgs)
import System.Random
import Control.Monad.State
import System.CPUTime
import Text.Printf
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Control.Concurrent

data RunMode = Train | Test | Demo
    deriving (Eq, Show)

data Config = Config
    { numCores :: Int
    , visualize :: Bool
    , runMode :: RunMode
    , gridFile :: FilePath
    , episodeCount :: Int
    , useGloss :: Bool
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { numCores = 1
    , visualize = False
    , runMode = Train
    , gridFile = "grids/simple.txt"
    , episodeCount = 1000
    , useGloss = False
    }

main :: IO ()
main = do
    args <- getArgs
    config <- parseArgs args defaultConfig
    
    printf "Pac-Man Q-Learning\n"
    printf "==================\n"
    printf "Configuration:\n"
    printf "  Mode: %s\n" (show $ runMode config)
    printf "  Cores: %d\n" (numCores config)
    printf "  Visualization: %s\n" (show $ visualize config)
    printf "  Use Gloss: %s\n" (show $ useGloss config)
    printf "  Grid file: %s\n" (gridFile config)
    printf "  Episodes: %d\n\n" (episodeCount config)
    
    -- Load game state
    initialState <- loadGrid (gridFile config)
    
    case runMode config of
        Demo -> runDemo initialState config
        Train -> runTraining initialState config
        Test -> runTesting initialState config

-- Parse command line arguments
parseArgs :: [String] -> Config -> IO Config
parseArgs [] cfg = return cfg
parseArgs ("--cores":n:rest) cfg = parseArgs rest cfg { numCores = read n }
parseArgs ("-c":n:rest) cfg = parseArgs rest cfg { numCores = read n }
parseArgs ("--visualize":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("-v":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("--gloss":rest) cfg = parseArgs rest cfg { useGloss = True, visualize = True }
parseArgs ("-g":rest) cfg = parseArgs rest cfg { useGloss = True, visualize = True }
parseArgs ("--demo":rest) cfg = parseArgs rest cfg { runMode = Demo }
parseArgs ("--train":rest) cfg = parseArgs rest cfg { runMode = Train }
parseArgs ("--test":rest) cfg = parseArgs rest cfg { runMode = Test }
parseArgs ("--grid":file:rest) cfg = parseArgs rest cfg { gridFile = file }
parseArgs ("--episodes":n:rest) cfg = parseArgs rest cfg { episodeCount = read n }
parseArgs ("-e":n:rest) cfg = parseArgs rest cfg { episodeCount = read n }
parseArgs (unknown:rest) cfg = do
    printf "Warning: Unknown argument '%s'\n" unknown
    parseArgs rest cfg

-- Demo mode: random actions with visualization
runDemo :: GameState -> Config -> IO ()
runDemo initialState config = do
    printf "Running demo with random actions...\n\n"
    
    -- Generate random actions
    gen <- newStdGen
    let actions = take 100 $ randoms gen  -- 100 random actions
    
    if useGloss config
        then do
            printf "Starting Gloss visualization...\n"
            runGlossGame initialState actions
        else do
            finalState <- runGameWithVisualization initialState actions (visualize config)
            printf "\n=== Demo Complete ===\n"
            printf "Final Score: %d\n" (score finalState)
            printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
            printf "Terminal: %s\n" (if isTerminal finalState then "Yes" else "No")

runTraining :: GameState -> Config -> IO ()
runTraining initialState config = do
    let params = QLearningParams
            { alpha = 0.1
            , gamma = 0.9
            , epsilon = 0.1
            , episodes = episodeCount config
            }
    
    printf "Training Q-Learning with %d cores for %d episodes...\n" 
        (numCores config) (episodes params)
    
    start <- getCPUTime
    qtable <- if numCores config == 1
                then do
                    seed <- randomIO
                    return $ evalState (trainSequential initialState params) (mkStdGen seed)
                else trainParallel (numCores config) initialState params
    end <- getCPUTime
    
    let diff = fromIntegral (end - start) / (10^12)
    printf "Training completed in %.2f seconds\n" (diff :: Double)
    printf "Q-table size: %d entries\n" (Map.size qtable)
    
    when (visualize config) $ do
        printf "\nVisualizing learned policy...\n"
        visualizePolicy initialState qtable config

runTesting :: GameState -> Config -> IO ()
runTesting initialState config = do
    printf "Testing mode - you need to provide a trained Q-table\n"
    printf "For now, training a quick model...\n\n"
    
    let params = QLearningParams
            { alpha = 0.1
            , gamma = 0.9
            , epsilon = 0.05
            , episodes = min 500 (episodeCount config)
            }
    
    seed <- randomIO
    let qtable = evalState (trainSequential initialState params) (mkStdGen seed)
    
    printf "Testing learned policy...\n"
    visualizePolicy initialState qtable config

visualizePolicy :: GameState -> QTable -> Config -> IO ()
visualizePolicy initialState qtable config = do
    let maxSteps = 200
    
    if useGloss config
        then do
            let actions = generatePolicyActions initialState qtable maxSteps
            printf "Starting Gloss visualization with learned policy...\n"
            runGlossGame initialState actions
        else do
            finalState <- runPolicyWithVisualization initialState qtable maxSteps (visualize config)
            printf "\n=== Policy Test Complete ===\n"
            printf "Final Score: %d\n" (score finalState)
            printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
            printf "Terminal: %s\n" (if isTerminal finalState then "Yes" else "No")

generatePolicyActions :: GameState -> QTable -> Int -> [Action]
generatePolicyActions initialState qtable maxSteps = go initialState 0
    where
        go state step
            | step >= maxSteps = []
            | isTerminal state = []
            | otherwise =
                let s = stateKey state
                    action = bestAction qtable s
                    (newState, _) = executeAction state action
                in action : go newState (step + 1)

runPolicyWithVisualization :: GameState -> QTable -> Int -> Bool -> IO GameState
runPolicyWithVisualization initialState qtable maxSteps visualize = 
    go initialState (0 :: Int)
    where
        go state step
            | step >= maxSteps = do
                when visualize $ printf "Reached max steps\n"
                return state
            | isTerminal state = do
                when visualize $ do
                    renderDeathScreen state
                    threadDelay 2000000
                return state
            | otherwise = do
                let s = stateKey state
                    action = bestAction qtable s
                
                when visualize $ do
                    renderGame state
                    printf "Step %d: Best Action = %s\n" step (show action)
                    threadDelay 500000
                
                let (newState, reward) = executeAction state action
                
                when visualize $ 
                    printf "Reward: %.2f\n" reward
                
                go newState (step + 1)

-- Testing without a file
createTestGame :: GameState
createTestGame = 
    let gridData = 
            [ [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
            , [Wall, Pellet, Pellet, Empty, Pellet, Pellet, Pellet, Pellet, Pellet, Wall]
            , [Wall, Wall, Wall, Pellet, Pellet, Pellet, Pellet, Pellet, Pellet, Wall]
            , [Wall, Pellet, Pellet, Pellet, Pellet, Pellet, Pellet, Empty, Pellet, Wall]
            , [Wall, Pellet, Pellet, Wall, Wall, Wall, Pellet, Pellet, Pellet, Wall]
            , [Wall, Pellet, Empty, Pellet, Pellet, Pellet, Wall, Wall, Empty, Wall]
            , [Wall, Pellet, Pellet, Empty, Pellet, Pellet, Pellet, Pellet, Pellet, Wall]
            , [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
            ]
        gridVec = V.fromList $ map V.fromList gridData
        pacPos = (3, 6)
        ghosts = [(3, 1), (7, 3), (2, 5), (8, 5)]
        pelletCount = sum [length [() | cell <- row, cell == Pellet] | row <- gridData]
    in GameState
        { grid = gridVec
        , pacmanPos = pacPos
        , ghostPositions = ghosts
        , score = 0
        , pelletsRemaining = pelletCount
        , isTerminal = False
        , deathPos = Nothing
        }

threadDelay :: Int -> IO ()
threadDelay = Control.Concurrent.threadDelay
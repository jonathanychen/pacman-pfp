{-# LANGUAGE BangPatterns #-}

    module Main where

    import Control.DeepSeq (force)
    import Control.Exception (evaluate)
    import Types
    import Game
    import MCTS
    import QLearning
    import ParallelQLearning
    import GlossVisual
    import System.Environment (getArgs)
    import System.Random
    import Control.Monad.State
    import Control.Monad (replicateM)
    import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
    import Text.Printf
    import qualified Data.Map.Strict as Map
    import qualified Data.Vector as V
    import qualified Control.Concurrent
    import Control.Parallel.Strategies (parMap, rdeepseq)

    data RunMode = Train | Test | Demo | MCTS | MCTSParallel
        deriving (Eq, Show)

    data Config = Config
        { numCores :: Int
        , visualize :: Bool
        , runMode :: RunMode
        , gridFile :: FilePath
        , episodeCount :: Int
        , useGloss :: Bool
        , configMCTSIterations :: Int
        } deriving (Show)

    defaultConfig :: Config
    defaultConfig = Config
        { numCores = 1
        , visualize = False
        , runMode = Train
        , gridFile = "grids/large.txt"
        , episodeCount = 1000
        , useGloss = False
        , configMCTSIterations = 1000
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
            MCTS -> runMCTS initialState config
            MCTSParallel -> runMCTSParallel initialState config

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
    parseArgs ("--mcts":rest) cfg = parseArgs rest cfg { runMode = MCTS }
    parseArgs ("--mcts-parallel":rest) cfg = parseArgs rest cfg { runMode = MCTSParallel }
    parseArgs ("--grid":file:rest) cfg = parseArgs rest cfg { gridFile = file }
    parseArgs ("--episodes":n:rest) cfg = parseArgs rest cfg { episodeCount = read n }
    parseArgs ("-e":n:rest) cfg = parseArgs rest cfg { episodeCount = read n }
    parseArgs ("--iterations":n:rest) cfg = parseArgs rest cfg { configMCTSIterations = read n }
    parseArgs ("-i":n:rest) cfg = parseArgs rest cfg { configMCTSIterations = read n }
    parseArgs (unknown:rest) cfg = do
        printf "Warning: Unknown argument '%s'\n" unknown
        parseArgs rest cfg

    -- Run demo mode (random actions with visualization)
    runDemo :: GameState -> Config -> IO ()
    runDemo initialState config = do
        printf "Running demo with random actions...\n\n"
        
        -- Generate random actions
        gen <- newStdGen
        let actions = take 100 $ randoms gen
        
        -- Run game with visualization
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

    -- Run training mode
    runTraining :: GameState -> Config -> IO ()
    runTraining initialState config = do
        let params = QLearningParams
                { alpha = 0.2
                , gamma = 0.95
                , epsilon = 0.15
                , episodes = episodeCount config
                }
        
        printf "Training Q-Learning for %d episodes...\n" (episodes params)
        
        start <- getTime Monotonic
        seed <- randomIO
        let qtable = evalState (trainSequential initialState params) (mkStdGen seed)
        result <- evaluate (force qtable)
        end <- getTime Monotonic
        
        let diff = fromIntegral (toNanoSecs (end - start)) / 1e9 :: Double
        printf "Training completed in %.3f seconds\n" diff
        printf "Q-table size: %d entries\n" (Map.size result)
        printf "Average time per episode: %.3f ms\n" ((diff * 1000) / fromIntegral (episodes params))
        
        -- Optionally visualize learned policy
        when (visualize config) $ do
            printf "\nVisualizing learned policy...\n"
            visualizePolicy initialState result config

    -- Run testing mode (test learned policy)
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

    -- Visualize the learned policy
    visualizePolicy :: GameState -> QTable -> Config -> IO ()
    visualizePolicy initialState qtable config = do
        let maxSteps = 200
        
        if useGloss config
            then do
                -- Generate actions from policy
                let actions = generatePolicyActions initialState qtable maxSteps
                printf "Starting Gloss visualization with learned policy...\n"
                runGlossGame initialState actions
            else do
                finalState <- runPolicyWithVisualization initialState qtable maxSteps (visualize config)
                printf "\n=== Policy Test Complete ===\n"
                printf "Final Score: %d\n" (score finalState)
                printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
                printf "Terminal: %s\n" (if isTerminal finalState then "Yes" else "No")

    -- Generate actions from policy
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

    -- Run the learned policy
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

    -- Create a simple test game (for testing without a file)
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
            , recentPositions = []
            }

    -- Helper for thread delay
    threadDelay :: Int -> IO ()
    threadDelay = Control.Concurrent.threadDelay

    -- Generate a complete action sequence using MCTS (no reuse)
    generateMCTSActions :: GameState -> MCTSParams -> Int -> IO [Action]
    generateMCTSActions initialState params maxSteps = do
        printf "Generating MCTS action sequence (this may take a moment)...\n"
        go initialState 0 []
        where
            go state step actions
                | step >= maxSteps = return (reverse actions)
                | isTerminal state = return (reverse actions)
                | otherwise = do
                    -- Run MCTS for this state (fresh tree each time)
                    seed <- randomIO
                    let tree = evalState (mcts params state) (mkStdGen seed)
                        action = selectBestAction tree
                        (newState, _) = executeAction state action
                    
                    -- Progress indicator
                    when (step `mod` 10 == 0) $ 
                        printf "  Generated %d/%d actions...\r" step maxSteps
                    
                    go newState (step + 1) (action : actions)

    -- Run MCTS testing modes
    runMCTS :: GameState -> Config -> IO ()
    runMCTS initialState config = do
        let params = MCTSParams
                { mctsIterations = configMCTSIterations config
                , mctsExploration = 1.41  -- sqrt(2) for balanced exploration
                , mctsMaxDepth = 200
                }
        
        printf "Running MCTS Test with %d iterations per step...\n" 
            (configMCTSIterations config)
        
        if useGloss config && visualize config
            then do
                -- Generate full action sequence, then visualize with Gloss
                printf "Generating action sequence for visualization...\n"
                actions <- generateMCTSActions initialState params 200
                printf "\nGenerated %d actions. Starting Gloss visualization...\n" (length actions)
                runGlossGame initialState actions
            else do
                -- Run step-by-step with terminal visualization or stats
                start <- getTime Monotonic
                finalState <- runMCTSStepByStep initialState params 200 (visualize config)
                end <- getTime Monotonic
                
                let diff = fromIntegral (toNanoSecs (end - start)) / 1e9 :: Double
                
                printf "\n=== MCTS Test Complete ===\n"
                printf "Total time: %.2f seconds\n" diff
                printf "Final Score: %d\n" (score finalState)
                printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
                printf "Terminal: %s\n" (if isTerminal finalState then "Yes" else "No")

    -- Run MCTS step by step (for terminal visualization or stats only)
    runMCTSStepByStep :: GameState -> MCTSParams -> Int -> Bool -> IO GameState
    runMCTSStepByStep initialState params maxSteps visualize = 
        go initialState (0 :: Int)
        where
            go state step
                | step >= maxSteps = do
                    when visualize $ printf "Reached max steps\n"
                    return state
                | isTerminal state = do
                    when visualize $ do
                        renderDeathScreen state
                        threadDelay 1000000
                    return state
                | otherwise = do
                    -- Run MCTS for this state (fresh tree)
                    seed <- randomIO
                    let tree = evalState (mcts params state) (mkStdGen seed)
                        action = selectBestAction tree
                        visits = nodeVisits tree
                        children = Map.size $ nodeChildren tree
                    
                    when visualize $ do
                        renderGame state
                        printf "Step %d: MCTS selected %s (tree visits: %d, children: %d)\n" 
                            step (show action) visits children
                        threadDelay 500000
                    
                    let (newState, reward) = executeAction state action
                    
                    when visualize $ 
                        printf "Reward: %.2f\n" reward
                    
                    go newState (step + 1)

    -- Run parallel MCTS (faster for testing)
    runMCTSParallel :: GameState -> Config -> IO ()
    runMCTSParallel initialState config = do
        let params = MCTSParams
                { mctsIterations = configMCTSIterations config
                , mctsExploration = 1.41  -- sqrt(2) for balanced exploration
                , mctsMaxDepth = 200
                }
        
        printf "Running Parallel MCTS Test with %d cores and %d iterations...\n" 
            (numCores config) (configMCTSIterations config)
        
        if useGloss config && visualize config
            then do
                -- Generate actions in parallel, then visualize
                printf "Generating action sequence in parallel...\n"
                actions <- generateMCTSActionsParallel initialState params 200 (numCores config)
                printf "\nGenerated %d actions. Starting Gloss visualization...\n" (length actions)
                runGlossGame initialState actions
            else do
                -- Run step-by-step with parallel MCTS at each step
                start <- getTime Monotonic
                finalState <- runMCTSStepByStepParallel initialState params 200 (visualize config) (numCores config)
                end <- getTime Monotonic
                
                let diff = fromIntegral (toNanoSecs (end - start)) / 1e9 :: Double
                
                printf "\n=== Parallel MCTS Test Complete ===\n"
                printf "Total time: %.2f seconds\n" diff
                printf "Final Score: %d\n" (score finalState)
                printf "Pellets Remaining: %d\n" (pelletsRemaining finalState)
                printf "Terminal: %s\n" (if isTerminal finalState then "Yes" else "No")

    -- Generate actions using parallel MCTS (no reuse)
    generateMCTSActionsParallel :: GameState -> MCTSParams -> Int -> Int -> IO [Action]
    generateMCTSActionsParallel initialState params maxSteps numCores = do
        printf "Generating MCTS action sequence in parallel (this may take a moment)...\n"
        go initialState 0 []
        where
            go state step actions
                | step >= maxSteps = return (reverse actions)
                | isTerminal state = return (reverse actions)
                | otherwise = do
                    -- Run parallel MCTS for this state (fresh tree)
                    tree <- mctsParallel numCores params state
                    let action = selectBestAction tree
                        (newState, _) = executeAction state action
                    
                    -- Progress indicator
                    when (step `mod` 10 == 0) $ 
                        printf "  Generated %d/%d actions...\r" step maxSteps
                    
                    go newState (step + 1) (action : actions)

    -- Run parallel MCTS step by step
    runMCTSStepByStepParallel :: GameState -> MCTSParams -> Int -> Bool -> Int -> IO GameState
    runMCTSStepByStepParallel initialState params maxSteps visualize numCores = 
        go initialState (0 :: Int)
        where
            go state step
                | step >= maxSteps = do
                    when visualize $ printf "Reached max steps\n"
                    return state
                | isTerminal state = do
                    when visualize $ do
                        renderDeathScreen state
                        threadDelay 1000000
                    return state
                | otherwise = do
                    -- Run parallel MCTS for this state (fresh tree)
                    tree <- mctsParallel numCores params state
                    let action = selectBestAction tree
                        visits = nodeVisits tree
                        children = Map.size $ nodeChildren tree
                    
                    when visualize $ do
                        renderGame state
                        printf "Step %d: Parallel MCTS selected %s (tree visits: %d, children: %d)\n" 
                            step (show action) visits children
                        threadDelay 500000
                    
                    let (newState, reward) = executeAction state action
                    
                    when visualize $ 
                        printf "Reward: %.2f\n" reward
                    
                    go newState (step + 1)
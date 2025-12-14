module Game 
    ( module GameExecution
    , loadGrid
    , parseGrid
    , clearScreen
    , renderGame
    , renderDeathScreen
    , visualizeGameStates
    , runGameWithVisualization
    , when
    ) where

import Types
import GameExecution
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import Text.Printf

-- Load grid from file
loadGrid :: FilePath -> IO GameState
loadGrid filepath = do
    content <- readFile filepath
    let lns = lines content
        -- Parse grid and find special positions
        (gridData, pacPos, ghosts, pelletCount) = parseGrid lns
        
        gridVec = V.fromList $ map V.fromList gridData
    
    return $ GameState
        { grid = gridVec
        , pacmanPos = pacPos
        , ghostPositions = ghosts
        , score = 0
        , pelletsRemaining = pelletCount
        , isTerminal = False
        , deathPos = Nothing
        }

-- Parse grid from lines of text
parseGrid :: [String] -> ([[Cell]], Position, [Position], Int)
parseGrid lns = go lns 0 (0, 0) [] 0 []
    where
        go [] _ pacPos ghosts pellets gridRows = 
            (reverse gridRows, pacPos, reverse ghosts, pellets)
        go (line:rest) y pacPos ghosts pellets gridRows =
            let (row, pacPos', ghosts', pellets') = parseLine line y 0 pacPos ghosts pellets []
            in go rest (y + 1) pacPos' ghosts' pellets' (row : gridRows)
        
        parseLine [] _ _ pacPos ghosts pellets rowAcc = 
            (reverse rowAcc, pacPos, ghosts, pellets)
        parseLine (c:cs) y x pacPos ghosts pellets rowAcc =
            case c of
                '1' -> parseLine cs y (x + 1) pacPos ghosts pellets (Wall : rowAcc)
                '0' -> parseLine cs y (x + 1) pacPos ghosts (pellets + 1) (Pellet : rowAcc)
                'P' -> parseLine cs y (x + 1) (x, y) ghosts pellets (Empty : rowAcc)
                'G' -> parseLine cs y (x + 1) pacPos ((x, y) : ghosts) (pellets + 1) (Pellet : rowAcc)  -- Changed: Pellet under ghost
                ' ' -> parseLine cs y (x + 1) pacPos ghosts pellets (Empty : rowAcc)
                _   -> parseLine cs y (x + 1) pacPos ghosts pellets (Empty : rowAcc)

-- Visualization functions
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

renderGame :: GameState -> IO ()
renderGame state = do
    clearScreen
    let g = grid state
    
    -- Print header
    printf "Score: %d | Pellets: %d | Status: %s\n" 
        (score state) 
        (pelletsRemaining state)
        (if isTerminal state then "GAME OVER" else "Playing")
    putStrLn $ replicate 40 '-'
    
    -- Print grid
    V.imapM_ (\y row -> do
        V.imapM_ (\x cell -> do
            let pos = (x, y)
            if pos == pacmanPos state then
                putStr "P "  -- Pac-Man
            else if pos `elem` ghostPositions state then
                putStr "G "  -- Ghost
            else case cell of
                Wall   -> putStr "█ "
                Pellet -> putStr "· "
                Empty  -> putStr "  "
            ) row
        putStrLn ""
        ) g
    
    putStrLn $ replicate 40 '-'
    hFlush stdout

-- Render death screen with X at collision point
renderDeathScreen :: GameState -> IO ()
renderDeathScreen state = do
    clearScreen
    let g = grid state
    
    -- Print header
    printf "\n"
    printf "╔════════════════════════════════════════╗\n"
    printf "║              GAME OVER                 ║\n"
    printf "╚════════════════════════════════════════╝\n"
    printf "Score: %d | Pellets: %d\n" 
        (score state) 
        (pelletsRemaining state)
    putStrLn $ replicate 40 '-'
    
    -- Print grid with X at death position
    V.imapM_ (\y row -> do
        V.imapM_ (\x cell -> do
            let pos = (x, y)
            case deathPos state of
                Just deathPosition | pos == deathPosition -> 
                    putStr "X "  -- Death marker
                _ | pos == pacmanPos state && pos `elem` ghostPositions state ->
                    putStr "X "  -- Collision point
                _ | pos == pacmanPos state ->
                    putStr "P "  -- Pac-Man
                _ | pos `elem` ghostPositions state ->
                    putStr "G "  -- Ghost
                _ -> case cell of
                    Wall   -> putStr "█ "
                    Pellet -> putStr "· "
                    Empty  -> putStr "  "
            ) row
        putStrLn ""
        ) g
    
    putStrLn $ replicate 40 '-'
    printf "Pac-Man was caught by a ghost!\n\n"
    hFlush stdout

-- Visualize a sequence of game states
visualizeGameStates :: [GameState] -> Int -> IO ()
visualizeGameStates states delayMs = do
    mapM_ (\state -> do
        renderGame state
        threadDelay (delayMs * 1000)  -- Convert ms to microseconds
        ) states

-- Run a game episode with visualization
runGameWithVisualization :: GameState -> [Action] -> Bool -> IO GameState
runGameWithVisualization initialState actions visualize = 
    go initialState actions (0 :: Int)
    where
        go state [] _ = return state
        go state _ _ | isTerminal state = do
            when visualize $ do
                renderDeathScreen state
                threadDelay 2000000  -- 2 second pause on death screen
            return state
        go state (action:rest) step = do
            when visualize $ do
                renderGame state
                printf "Step %d: Action = %s\n" step (show action)
                threadDelay 500000  -- 500ms delay
            
            let (newState, reward) = executeAction state action
            
            when visualize $ 
                printf "Reward: %.2f\n" reward
            
            go newState rest (step + 1)

-- Helper function for conditional execution
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()
module GlossVisual
    ( runGlossGame
    , renderGameGloss
    ) where

import Types
import GameExecution
import qualified Data.Vector as V
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Constants
cellSize :: Float
cellSize = 40

windowWidth :: Int
windowWidth = 600

windowHeight :: Int
windowHeight = 600

-- Main function to run game with Gloss
runGlossGame :: GameState -> [Action] -> IO ()
runGlossGame initialState actions = do
    let world = GlossWorld
            { gwState = initialState
            , gwActions = actions
            , gwStepDelay = 0.5  -- 500ms between steps
            , gwTimeSinceStep = 0
            }
    play
        (InWindow "Pac-Man Q-Learning" (windowWidth, windowHeight) (100, 100))
        black           -- Background color
        2               -- Steps per second (will be controlled by gwStepDelay)
        world
        drawWorld
        handleInput
        updateWorld

-- World state for Gloss
data GlossWorld = GlossWorld
    { gwState :: GameState
    , gwActions :: [Action]
    , gwStepDelay :: Float
    , gwTimeSinceStep :: Float
    }

-- Draw the world
drawWorld :: GlossWorld -> Picture
drawWorld world = 
    let state = gwState world
        g = grid state
        rows = V.length g
        cols = if rows > 0 then V.length (g V.! 0) else 0
        
        -- Calculate offsets to center the grid
        offsetX = -fromIntegral cols * cellSize / 2
        offsetY = fromIntegral rows * cellSize / 2
        
        -- Draw all cells
        cellPictures = concat $ V.toList $ V.imap (\y row ->
            V.toList $ V.imap (\x cell ->
                let pos = (x, y)
                    xPos = offsetX + fromIntegral x * cellSize
                    yPos = offsetY - fromIntegral y * cellSize
                in drawCell cell pos xPos yPos state
            ) row
            ) g
        
        -- Draw UI
        uiText = Pictures
            [ Translate (-280) 280 $ Scale 0.15 0.15 $ Color white $ Text ("Score: " ++ show (score state))
            , Translate (-280) 260 $ Scale 0.15 0.15 $ Color white $ Text ("Pellets: " ++ show (pelletsRemaining state))
            , if isTerminal state
                then Translate (-100) 0 $ Scale 0.3 0.3 $ Color red $ Text "GAME OVER"
                else Blank
            ]
    in Pictures (cellPictures ++ [uiText])

-- Draw a single cell
drawCell :: Cell -> Position -> Float -> Float -> GameState -> Picture
drawCell cell pos xPos yPos state =
    let cellCenter = Translate (xPos + cellSize/2) (yPos - cellSize/2)
        isPacman = pos == pacmanPos state
        isGhost = pos `elem` ghostPositions state
    in if isPacman
        then cellCenter $ Pictures
            [ Color blue $ ThickCircle 0 (cellSize * 0.4)  -- Pac-Man body
            , Color yellow $ ThickCircle 0 (cellSize * 0.35)
            ]
        else if isGhost
            then cellCenter $ Pictures
                [ Color red $ ThickCircle 0 (cellSize * 0.4)  -- Ghost body
                , Color white $ Translate (-8) 5 $ ThickCircle 0 3  -- Eye 1
                , Color white $ Translate 8 5 $ ThickCircle 0 3     -- Eye 2
                , Color black $ Translate (-8) 5 $ ThickCircle 0 1.5  -- Pupil 1
                , Color black $ Translate 8 5 $ ThickCircle 0 1.5     -- Pupil 2
                ]
            else case cell of
                Wall -> cellCenter $ Color (greyN 0.3) $ rectangleSolid cellSize cellSize
                Pellet -> cellCenter $ Color white $ ThickCircle 0 (cellSize * 0.1)
                Empty -> Blank

-- Handle keyboard input
handleInput :: Event -> GlossWorld -> GlossWorld
handleInput _ world = world  -- No interaction needed for demo

-- Update world state
updateWorld :: Float -> GlossWorld -> GlossWorld
updateWorld dt world
    | isTerminal (gwState world) = world
    | null (gwActions world) = world
    | gwTimeSinceStep world + dt >= gwStepDelay world =
        let action = head (gwActions world)
            (newState, _) = executeAction (gwState world) action
        in world
            { gwState = newState
            , gwActions = tail (gwActions world)
            , gwTimeSinceStep = 0
            }
    | otherwise = world { gwTimeSinceStep = gwTimeSinceStep world + dt }

-- Render game state to Gloss Picture (for static rendering)
renderGameGloss :: GameState -> Picture
renderGameGloss state = drawWorld (GlossWorld state [] 0 0)
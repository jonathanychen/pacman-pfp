{-# LANGUAGE DeriveGeneric #-}

module Types
    ( Cell(..)
    , Position
    , Action(..)
    , allActions
    , GameState(..)
    , QLearningParams(..)
    , QTable
    , StateKey(..)
    ) where

import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import Control.DeepSeq
import GHC.Generics (Generic)
import System.Random

-- Grid cell types
data Cell = Wall | Empty | Pellet
    deriving (Eq, Show, Generic)

instance NFData Cell

-- Position on the grid
type Position = (Int, Int)

-- Direction/Action
data Action = MoveUp | MoveDown | MoveLeft | MoveRight
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData Action

instance Random Action where
    randomR (lo, hi) g =
        let (i, g') = randomR (fromEnum lo, fromEnum hi) g
        in (toEnum i, g')
    random = randomR (minBound, maxBound)

allActions :: [Action]
allActions = [minBound .. maxBound]

-- Game state
data GameState = GameState
    { grid :: Vector (Vector Cell)
    , pacmanPos :: Position
    , ghostPositions :: [Position]
    , score :: Int
    , pelletsRemaining :: Int
    , isTerminal :: Bool
    , deathPos :: Maybe Position  -- NEW: Position where Pac-Man died
    } deriving (Eq, Show, Generic)

instance NFData GameState

-- Q-Learning parameters
data QLearningParams = QLearningParams
    { alpha :: Double      -- Learning rate
    , gamma :: Double      -- Discount factor
    , epsilon :: Double    -- Exploration rate
    , episodes :: Int      -- Number of training episodes
    } deriving (Show)

-- Q-table: Map from (State, Action) to Q-value
type QTable = Map.Map (StateKey, Action) Double

<<<<<<< HEAD
-- Simplified state representation for Q-table
-- (You'll need to simplify the full state for the key)
type StateKey = (Position, [Position], Int)  -- Pacman pos, ghost positions, pellets left
=======
data StateKey = StateKey
    {
    skPacmanPos :: Position -- Pacman position
    , pelletLeft :: Bool, pelletRight :: Bool, pelletUp :: Bool, pelletDown :: Bool -- Whether a pellet is left, right, up, down
    , ghostLeft :: Bool, ghostRight :: Bool, ghostUp :: Bool, ghostDown :: Bool -- Whether a ghost is left, right, up, down
    , wallLeft :: Bool, wallRight :: Bool, wallUp :: Bool, wallDown :: Bool -- Whether a wall is left, right, up, down
    } deriving  (Eq, Ord, Show, Generic)

instance NFData StateKey

-- type StateKey = (Position, [Position], Int)  -- Pacman pos, ghost positions, pellets left
>>>>>>> 2b81383 (fix part of rewards system, add new large grid)

{-# LANGUAGE DeriveGeneric #-}

module Types 
    ( Cell(..)
    , Position
    , Action(..)
    , allActions
    , GameState(..)
    , StateKey(..)
    , QLearningParams(..)
    , QTable
    , StateKey
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
    random g = randomR (minBound, maxBound) g

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
    , deathPos :: Maybe Position
    , recentPositions :: [Position]
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

-- Improved state representation for Q-Learning (removed exact position for better generalization)
data StateKey = StateKey
    { pelletLeft :: Bool
    , pelletRight :: Bool
    , pelletUp :: Bool
    , pelletDown :: Bool
    , ghostLeft :: Bool
    , ghostRight :: Bool
    , ghostUp :: Bool
    , ghostDown :: Bool
    , wallLeft :: Bool
    , wallRight :: Bool
    , wallUp :: Bool
    , wallDown :: Bool
    , ghostNear :: Bool  -- Whether any ghost is within 3 steps
    , pelletNear :: Bool  -- Whether any pellet is within 3 steps
    } deriving (Eq, Ord, Show, Generic)

instance NFData StateKey
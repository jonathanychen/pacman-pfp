{-# LANGUAGE DeriveGeneric #-}

module Types 
    ( Cell(..)
    , Position
    , Action(..)
    , allActions
    , GameState(..)
    , QLearningParams(..)
    , QTable
    , StateKey
    ) where

import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import Control.DeepSeq
import GHC.Generics (Generic)
import System.Random

data Cell = Wall | Empty | Pellet
    deriving (Eq, Show, Generic)

instance NFData Cell

type Position = (Int, Int)

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

data GameState = GameState
    { grid :: Vector (Vector Cell)
    , pacmanPos :: Position
    , ghostPositions :: [Position]
    , score :: Int
    , pelletsRemaining :: Int
    , isTerminal :: Bool
    , deathPos :: Maybe Position
    } deriving (Eq, Show, Generic)

instance NFData GameState

data QLearningParams = QLearningParams
    { alpha :: Double      -- Learning rate
    , gamma :: Double      -- Discount factor
    , epsilon :: Double    -- Exploration rate
    , episodes :: Int      -- Number of training episodes
    } deriving (Show)

type QTable = Map.Map (StateKey, Action) Double


type StateKey = (Position, [Position], Int)  -- Pacman pos, ghost positions, pellets left
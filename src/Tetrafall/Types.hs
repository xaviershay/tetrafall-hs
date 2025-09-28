{-# LANGUAGE TemplateHaskell #-}

module Tetrafall.Types 
  ( Coordinate
  , TetrominoType(..)
  , Orientation(..)
  , Cell(..)
  , Grid
  , Game(..)
  , Tetromino(..)
  , Action(..)
  , SlideState(..)
  , Randomizer
  , RandomizerEnv(..)
  , defaultTetrominoMap
  , TetrominoMap
  , dimensions
  , setAt
  , grid
  , score
  , currentPiece
  , slideState
  , rng
  , particles
  , windowSize
  , tetrominoType
  , position
  , orientation
  , particleLocation
  , mkParticle
  , rotateCW
  , rotateCCW
  , randomizerEnvRng
  ) where

import Data.HashMap.Strict (HashMap, fromList)
import Data.Hashable (Hashable(..))
import Lens.Micro.Platform
import System.Random (StdGen, mkStdGen)

import Tetrafall.Types.Coordinate
import Tetrafall.Types.Grid

data TetrominoType = S | Z | J | L | O | I | T
  deriving (Show, Enum, Eq, Ord)

instance Hashable TetrominoType where
  hashWithSalt s = hashWithSalt s . fromEnum

data Cell = Empty | Garbage | TetrominoCell TetrominoType
  deriving (Eq, Ord, Show)

data RandomizerEnv = RandomizerEnv
  { _randomizerEnvRng :: StdGen
  }
makeLenses ''RandomizerEnv

data Orientation = North | East | South | West
  deriving (Show, Enum, Eq)

rotateCW :: Orientation -> Orientation
rotateCW North = East
rotateCW East = South
rotateCW South = West
rotateCW West = North

rotateCCW :: Orientation -> Orientation  
rotateCCW North = West
rotateCCW West = South
rotateCCW South = East
rotateCCW East = North

data Tetromino = Tetromino
  { _tetrominoType :: TetrominoType
  , _position :: Coordinate
  , _orientation :: Orientation
  } deriving (Eq, Show)
makeLenses ''Tetromino

data Particle = Particle
  { _particleLocation :: (Float, Float)
  , _particleAge :: Int
  }
makeLenses ''Particle

mkParticle :: Particle
mkParticle = Particle { _particleLocation = (0.0, 0.0), _particleAge = 0 }

type Randomizer = RandomizerEnv -> (TetrominoType, RandomizerEnv)

data Action =
    ActionLeft
  | ActionRight
  | ActionRotateCW
  | ActionRotateCCW
  | ActionSoftDrop
  | ActionHardDrop
  deriving (Eq, Show)

data Game = Game
  { _grid :: Grid Cell
  , _currentPiece :: Maybe Tetromino
  , _score :: Int
  , _slideState :: SlideState
  , _randomizer :: Randomizer
  , _rng :: StdGen
  , _particles :: [Particle]
  , _windowSize :: (Int, Int)
  }

data SlideState = 
    CanFall  -- Piece can still fall normally
  | Sliding Coordinate  -- Piece cannot fall, tracking position for slide detection
  | ShouldLock  -- Piece should be locked immediately (e.g., after hard drop)
  deriving (Eq, Show)

makeLenses ''Game

type TetrominoMap = HashMap TetrominoType (Grid Cell)

defaultTetrominoMap :: TetrominoMap
defaultTetrominoMap = fromList
  [ (I, makeSparseWithExtent Empty ((-1, -1), (2, 2)) [((- 1, 0), TetrominoCell I), ((0, 0), TetrominoCell I), ((1, 0), TetrominoCell I), ((2, 0), TetrominoCell I)])
  , (T, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), TetrominoCell T), ((0, 0), TetrominoCell T), ((1, 0), TetrominoCell T), ((0, -1), TetrominoCell T)])
  , (S, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((0, -1), TetrominoCell S), ((1, -1), TetrominoCell S), ((-1, 0), TetrominoCell S), ((0, 0), TetrominoCell S)])
  , (Z, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, -1), TetrominoCell Z), ((0, -1), TetrominoCell Z), ((0, 0), TetrominoCell Z), ((1, 0), TetrominoCell Z)])
  , (J, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), TetrominoCell J), ((0, 0), TetrominoCell J), ((1, 0), TetrominoCell J), ((-1, -1), TetrominoCell J)])
  , (L, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), TetrominoCell L), ((0, 0), TetrominoCell L), ((1, 0), TetrominoCell L), ((1, -1), TetrominoCell L)])
  , (O, makeSparseWithExtent Empty ((0, -1), (1, 0)) [((0, 0), TetrominoCell O), ((1, 0), TetrominoCell O), ((0, -1), TetrominoCell O), ((1, -1), TetrominoCell O)])
  ]
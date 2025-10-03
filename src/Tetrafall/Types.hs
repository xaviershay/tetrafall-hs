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
  , ScoreEvent(..)
  , ScoringAlgorithm
  , Randomizer
  , RandomizerEnv(..)
  , defaultTetrominoMap
  , TetrominoMap
  , dimensions
  , setAt
  , grid
  , score
  , currentPiece
  , gameNextPieces
  , slideState
  , rng
  , particles
  , windowSize
  , gameScoreAlgorithm
  , gameTime
  , gameStepSize
  , gameStepAccum
  , tetrominoType
  , position
  , orientation
  , particleLocation
  , mkParticle
  , rotateCW
  , rotateCCW
  , randomizerEnvRng
  , randomizerEnv
  , scoreLines
  , scoreLevel
  ) where

import Data.HashMap.Strict (HashMap, fromList)
import Data.Hashable (Hashable(..))
import Data.Time.Clock (NominalDiffTime)
import Lens.Micro.Platform
import System.Random (StdGen)


import Tetrafall.Types.Coordinate
import Tetrafall.Types.Grid

data TetrominoType = S | Z | J | L | O | I | T
  deriving (Show, Enum, Eq, Ord)

instance Hashable TetrominoType where
  hashWithSalt s = hashWithSalt s . fromEnum

data Cell = Empty | Garbage | TetrominoCell TetrominoType
  deriving (Eq, Ord, Show)

type Randomizer = RandomizerEnv -> (TetrominoType, RandomizerEnv)

data RandomizerEnv = RandomizerEnv
  { _randomizerEnvRng :: StdGen
  , _randomizerEnvHistory :: [TetrominoType] -- Most recent piece at head of list
  , _randomizerBagCapacity :: Int
  , _randomizerBag :: [TetrominoType]
  , _randomizerSelection :: Randomizer
  , _randomizerCount :: Int
  , _randomizerSinceLast :: HashMap TetrominoType Int
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

data ParticleType = ParticleStar

data Particle = Particle
  { _particleLocation :: (Float, Float)
  , _particleAge :: Int
  , _particleType :: ParticleType
  }
makeLenses ''Particle

mkParticle :: Particle
mkParticle = Particle
  { _particleLocation = (0.0, 0.0)
  , _particleAge = 0
  , _particleType = ParticleStar
  }

data Action =
    ActionLeft
  | ActionRight
  | ActionRotateCW
  | ActionRotateCCW
  | ActionSoftDrop
  | ActionHardDrop
  | ActionStep
  | ActionTick NominalDiffTime
  deriving (Eq, Show)

data ScoreEvent = ScoreEvent
  { _scoreLines :: Int
  , _scoreLevel :: Int
  } deriving (Eq, Show)
makeLenses ''ScoreEvent

type ScoringAlgorithm = ScoreEvent -> Int

data Game = Game
  { _grid :: Grid Cell
  , _currentPiece :: Maybe Tetromino
  , _gameNextPieces :: [TetrominoType]
  , _score :: Int
  , _slideState :: SlideState
  , _randomizerEnv :: RandomizerEnv
  , _rng :: StdGen
  , _particles :: [Particle]
  , _windowSize :: (Int, Int)
  , _gameScoreAlgorithm :: ScoringAlgorithm
  , _gameTime :: NominalDiffTime
  , _gameStepSize :: NominalDiffTime
  , _gameStepAccum :: NominalDiffTime
  }

data SlideState = 
    CanFall  -- Piece can still fall normally
  | Sliding Coordinate  -- Piece cannot fall, tracking position for slide detection
  | ShouldLock  -- Piece should be locked immediately (e.g., after hard drop)
  deriving (Eq, Show)

makeLenses ''Game

type TetrominoMap = HashMap TetrominoType (Grid Cell)

-- Helper functions for tetromino cell construction
i, t, s, z, j, l, o :: Cell
i = TetrominoCell I
t = TetrominoCell T
s = TetrominoCell S
z = TetrominoCell Z
j = TetrominoCell J
l = TetrominoCell L
o = TetrominoCell O

defaultTetrominoMap :: TetrominoMap
defaultTetrominoMap = fromList
  [ (I, makeSparseWithExtent Empty ((-1, -1), (2, 2)) [((- 1, 0), i), ((0, 0), i), ((1, 0), i), ((2, 0), i)])
  , (T, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), t), ((0, 0), t), ((1, 0), t), ((0, -1), t)])
  , (S, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((0, -1), s), ((1, -1), s), ((-1, 0), s), ((0, 0), s)])
  , (Z, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, -1), z), ((0, -1), z), ((0, 0), z), ((1, 0), z)])
  , (J, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), j), ((0, 0), j), ((1, 0), j), ((-1, -1), j)])
  , (L, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), l), ((0, 0), l), ((1, 0), l), ((1, -1), l)])
  , (O, makeSparseWithExtent Empty ((0, -1), (1, 0)) [((0, 0), o), ((1, 0), o), ((0, -1), o), ((1, -1), o)])
  ]
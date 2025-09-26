{-# LANGUAGE TemplateHaskell #-}

module Tetrafall.Types 
  ( Coordinate
  , TetrominoType(..)
  , Orientation(..)
  , Cell(..)
  , Grid(..)
  , Game(..)
  , Tetromino(..)
  , Tick(..)
  , tetrominoI
  , dimensions
  , setAt
  , grid
  , score
  ) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Lens.Micro.Platform

import Tetrafall.Types.Coordinate
import Tetrafall.Types.Grid

data Tick = Tick

data TetrominoType = S | Z | J | L | O | I | T
  deriving (Show, Enum)

data Cell = Empty | Garbage | TetrominoCell TetrominoType
data Orientation = North | East | South | West
  deriving (Show, Enum)

data Tetromino = Tetromino
  { _tetrominoType :: TetrominoType
  , _position :: Coordinate
  , _orientation :: Orientation
  }
makeLenses ''Tetromino

data Game = Game
  { _grid :: Grid Cell
  , _currentPiece :: Maybe Tetromino
  , _score :: Int
  }

makeLenses ''Game


tetrominoI = Tetromino
  { _tetrominoType = I
  , _position = (5, 3)
  , _orientation = North
  }

--type TetrominoMap = HashMap TetrominoType (Grid Cell)
--
--defaultTertominoMap = fromList
--  [(I, makeSparse (TetrominoCell I) [(-1, 0), (0, 0), (1, 0), (2, 0)])
--  ]
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

data Game = Game
  { _grid :: Grid Cell
  , _score :: Int
  }

makeLenses ''Game

data Orientation = North | East | South | West
  deriving (Show, Enum)

data Tetromino = Tetromino
  { _type :: TetrominoType
  , _position :: Coordinate
  , _orientation :: Orientation
  }


module Tetrafall.Types 
  ( Coordinate
  , TetrominoType(..)
  , Orientation(..)
  , Cell(..)
  , Grid(..)
  , Tetromino(..)
  , dimensions
  , contents
  , mkGrid
  , setAt
  ) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Lens.Micro.Platform

type Coordinate = (Int, Int)

data TetrominoType = S | Z | J | L | O | I | T
  deriving (Show, Enum)

data Orientation = North | East | South | West
  deriving (Show, Enum)

data Cell = Empty | Garbage | TetrominoCell TetrominoType

data Grid = Grid
  { _dimensions :: Coordinate
  , _contents :: Vector (Vector Cell)
  }

-- Lenses for Grid
dimensions :: Lens' Grid Coordinate
dimensions f grid = fmap (\d -> grid { _dimensions = d }) (f (_dimensions grid))

contents :: Lens' Grid (Vector (Vector Cell))
contents f grid = fmap (\d -> grid { _contents = d }) (f (_contents grid))

mkGrid :: Int -> Int -> Grid
mkGrid w h = Grid
  { _dimensions =  (w, h)
  , _contents = V.fromList $ replicate h (V.fromList (replicate w Empty))
  }

setAt :: Coordinate -> Cell -> Grid -> Grid
setAt (x, y) cell grid = 
  grid & contents . ix y . ix x .~ cell

data Tetromino = Tetromino
  { _type :: TetrominoType
  , _position :: Coordinate
  , _orientation :: Orientation
  }


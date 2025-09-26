module Tetrafall.Types.Grid (makeDense, makeSparse, dimensions, overlay, Grid) where

import Tetrafall.Types (Coordinate)

import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List (foldl')

data CellData a = 
    Sparse [(Coordinate, a)]
  | Dense (Vector (Vector a))

data Grid a = Grid
  { _dimensions :: Coordinate
  , _cells :: (CellData a)
  }

-- toList :: Sparse a -> [(Coordinate, a)]
-- toList = _contents

dimensions :: Grid a -> Coordinate
dimensions = _dimensions

makeDense :: Coordinate -> a -> Grid a
makeDense ds zero = Grid
  { _dimensions = ds
  , _cells = Dense $ V.fromList (replicate (snd ds) (V.fromList (replicate (fst ds) zero)))
  }

makeSparse :: [(Coordinate, a)] -> Grid a
makeSparse xs = Grid
    { _dimensions = if null xs then (0, 0) else (maxX - minX + 1, maxY - minY + 1)
    , _cells = Sparse xs
    }
  where
    (minX, maxX, minY, maxY) = foldl' updateBounds (maxBound, minBound, maxBound, minBound) (map fst xs)
    updateBounds (minX', maxX', minY', maxY') (x, y) = (min x minX', max x maxX', min y minY', max y maxY')

-- Limitations:
overlay :: Grid a -> Grid a -> Grid a
overlay bg fg = case (_cells bg, _cells fg) of
  (Dense bgCells, Sparse fgCells) -> 
    let updatedCells = foldl' updateCell bgCells fgCells
    in Grid (_dimensions bg) (Dense updatedCells)
    where
      updateCell cells (coord, value) = 
        let (x, y) = coord
        in cells V.// [(y, (cells V.! y) V.// [(x, value)])]
  
  -- Other combinations - leave undefined for now
  (Dense _, Dense _) -> undefined
  (Sparse _, Dense _) -> undefined  
  (Sparse _, Sparse _) -> undefined
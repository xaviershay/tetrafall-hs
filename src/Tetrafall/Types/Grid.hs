module Tetrafall.Types.Grid (makeDense, makeSparse, dimensions, overlay, toList, toVector, setAt, double, Grid) where

import Tetrafall.Types.Coordinate

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

toList :: Grid a -> [(Coordinate, a)]
toList g = case (_cells g) of
  Sparse xs -> xs
  Dense xs -> concatMap (\(y, row) -> map (\(x, val) -> ((x, y), val)) (V.toList (V.indexed row))) (V.toList (V.indexed xs))

toVector :: Grid a -> Vector (Vector a)
toVector g = case (_cells g) of
  Sparse _ -> undefined
  Dense xs -> xs
  

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

-- Overlay foreground grid on top of background grid
overlay :: Grid a -> Grid a -> Grid a
overlay bg fg = case (_cells bg, _cells fg) of
  (Dense bgCells, Sparse fgCells) -> 
    let updatedCells = foldl' updateCell bgCells fgCells
    in Grid (_dimensions bg) (Dense updatedCells)
    where
      updateCell cells (coord, value) = 
        let (x, y) = coord
            (maxX, maxY) = _dimensions bg
        in if x >= 0 && x < maxX && y >= 0 && y < maxY
           then cells V.// [(y, (cells V.! y) V.// [(x, value)])]
           else cells  -- Ignore out-of-bounds coordinates
  
  (Dense bgCells, Dense fgCells) ->
    let (bgW, bgH) = _dimensions bg
        (fgW, fgH) = _dimensions fg
        minW = min bgW fgW
        minH = min bgH fgH
        updatedCells = V.imap (\y row -> 
          if y < minH 
          then V.imap (\x cell -> 
            if x < minW 
            then (fgCells V.! y) V.! x 
            else cell) row
          else row) bgCells
    in Grid (_dimensions bg) (Dense updatedCells)
  
  (Sparse _, Dense _) ->
    -- For sparse background and dense foreground, just return the foreground
    fg
  
  (Sparse bgCells, Sparse fgCells) ->
    let combinedCells = fgCells ++ filter (\(coord, _) -> coord `notElem` map fst fgCells) bgCells
        newDims = if null combinedCells 
                 then (0, 0) 
                 else let coords = map fst combinedCells
                          xs = map fst coords
                          ys = map snd coords
                      in (maximum xs - minimum xs + 1, maximum ys - minimum ys + 1)
    in Grid newDims (Sparse combinedCells)

double :: Grid a ->  Grid a
double grid = case (_cells grid) of
  Sparse _ -> undefined
  Dense originalContents -> Grid
    { _dimensions = (w * 2, h * 2)
    , _cells = Dense $ V.fromList $ concatMap doubleRow (V.toList originalContents)
    }
    where
      (w, h) = _dimensions grid
      doubleRow row = [doubledRow, doubledRow]
        where doubledRow = V.fromList $ concatMap (\cell -> [cell, cell]) (V.toList row)


setAt :: Coordinate -> a -> Grid a -> Grid a
setAt (x, y) cell grid = case (_cells grid) of
  Dense cells -> 
    let (maxX, maxY) = _dimensions grid
    in if x >= 0 && x < maxX && y >= 0 && y < maxY
       then Grid (_dimensions grid) (Dense (cells V.// [(y, (cells V.! y) V.// [(x, cell)])]))
       else grid  -- Ignore out-of-bounds coordinates
  
  Sparse cells -> 
    let updatedCells = ((x, y), cell) : filter ((/= (x, y)) . fst) cells
    in Grid (_dimensions grid) (Sparse updatedCells)

emptyGrid :: Grid a
emptyGrid = Grid (0, 0) (Dense V.empty)

instance Semigroup (Grid a) where
  (<>) = overlay

instance Monoid (Grid a) where
  mempty = emptyGrid
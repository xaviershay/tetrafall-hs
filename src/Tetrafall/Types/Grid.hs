module Tetrafall.Types.Grid (makeDense, makeSparse, dimensions, extent, overlay, toList, toVector, setAt, double, toSparse, overlap, isWithinBounds, Grid) where

import Tetrafall.Types.Coordinate

import qualified Data.Vector as V
import Data.Vector (Vector)

data CellData a = 
    Sparse [(Coordinate, a)]
  | Dense (Vector (Vector a))

data Grid a = Grid
  { _extent :: (Coordinate, Coordinate)  -- (topLeft, bottomRight)
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
dimensions grid = 
  let ((minX, minY), (maxX, maxY)) = _extent grid
  in (maxX - minX + 1, maxY - minY + 1)

extent :: Grid a -> (Coordinate, Coordinate)
extent = _extent

makeDense :: Coordinate -> a -> Grid a
makeDense (w, h) zero = Grid
  { _extent = ((0, 0), (w - 1, h - 1))
  , _cells = Dense $ V.fromList (replicate h (V.fromList (replicate w zero)))
  }

makeSparse :: [(Coordinate, a)] -> Grid a
makeSparse xs = Grid
    { _extent = if null xs then ((0, 0), (-1, -1)) else ((minX, minY), (maxX, maxY))
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
    in Grid (_extent bg) (Dense updatedCells)
    where
      updateCell cells (coord, value) = 
        let (x, y) = coord
            ((minX, minY), (maxX, maxY)) = _extent bg
        in if x >= minX && x <= maxX && y >= minY && y <= maxY
           then cells V.// [(y - minY, (cells V.! (y - minY)) V.// [(x - minX, value)])]
           else cells  -- Ignore out-of-bounds coordinates
  
  (Dense bgCells, Dense fgCells) ->
    let ((bgMinX, bgMinY), (bgMaxX, bgMaxY)) = _extent bg
        ((fgMinX, fgMinY), (fgMaxX, fgMaxY)) = _extent fg
        bgW = bgMaxX - bgMinX + 1
        bgH = bgMaxY - bgMinY + 1
        fgW = fgMaxX - fgMinX + 1 
        fgH = fgMaxY - fgMinY + 1
        minW = min bgW fgW
        minH = min bgH fgH
        updatedCells = V.imap (\y row -> 
          if y < minH 
          then V.imap (\x cell -> 
            if x < minW 
            then (fgCells V.! y) V.! x 
            else cell) row
          else row) bgCells
    in Grid (_extent bg) (Dense updatedCells)
  
  (Sparse _, Dense _) ->
    -- For sparse background and dense foreground, just return the foreground
    fg
  
  (Sparse bgCells, Sparse fgCells) ->
    let combinedCells = fgCells ++ filter (\(coord, _) -> coord `notElem` map fst fgCells) bgCells
        newExtent = if null combinedCells 
                   then ((0, 0), (-1, -1))
                   else let coords = map fst combinedCells
                            xs = map fst coords
                            ys = map snd coords
                        in ((minimum xs, minimum ys), (maximum xs, maximum ys))
    in Grid newExtent (Sparse combinedCells)

double :: Grid a ->  Grid a
double grid = case (_cells grid) of
  Sparse _ -> undefined
  Dense originalContents -> Grid
    { _extent = ((minX * 2, minY * 2), (maxX * 2 + 1, maxY * 2 + 1))
    , _cells = Dense $ V.fromList $ concatMap doubleRow (V.toList originalContents)
    }
    where
      ((minX, minY), (maxX, maxY)) = _extent grid
      doubleRow row = [doubledRow, doubledRow]
        where doubledRow = V.fromList $ concatMap (\cell -> [cell, cell]) (V.toList row)


setAt :: Coordinate -> a -> Grid a -> Grid a
setAt (x, y) cell grid = case (_cells grid) of
  Dense cells -> 
    let ((minX, minY), (maxX, maxY)) = _extent grid
    in if x >= minX && x <= maxX && y >= minY && y <= maxY
       then Grid (_extent grid) (Dense (cells V.// [(y - minY, (cells V.! (y - minY)) V.// [(x - minX, cell)])]))
       else grid  -- Ignore out-of-bounds coordinates
  
  Sparse cells -> 
    let updatedCells = ((x, y), cell) : filter ((/= (x, y)) . fst) cells
        oldExtent = _extent grid
        newExtent = if null cells
                   then ((x, y), (x, y))
                   else let ((oldMinX, oldMinY), (oldMaxX, oldMaxY)) = oldExtent
                        in ((min x oldMinX, min y oldMinY), (max x oldMaxX, max y oldMaxY))
    in Grid newExtent (Sparse updatedCells)

emptyGrid :: Grid a
emptyGrid = Grid ((0, 0), (-1, -1)) (Dense V.empty)

toSparse :: (Eq a, Monoid a) => Grid a -> [(Coordinate, a)]
toSparse grid = filter ((/= mempty) . snd) (toList grid)

overlap :: (Eq a, Monoid a) => Grid a -> Grid a -> Bool
overlap grid1 grid2 = 
  let sparse1 = toSparse grid1
      sparse2 = toSparse grid2
      coords1 = map fst sparse1
      coords2 = map fst sparse2
  in any (`elem` coords2) coords1

isWithinBounds :: Grid a -> Grid a -> Bool
isWithinBounds baseGrid pieceGrid =
    let ((baseMinX, baseMinY), (baseMaxX, baseMaxY)) = _extent baseGrid
        pieceCoords = map fst (toList pieceGrid)
    in all (\(x, y) -> x >= baseMinX && x <= baseMaxX && y >= baseMinY && y <= baseMaxY) pieceCoords

instance Semigroup (Grid a) where
  (<>) = overlay

instance Monoid (Grid a) where
  mempty = emptyGrid
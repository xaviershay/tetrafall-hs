module Tetrafall.Types.Grid (makeDense, makeSparse, makeSparseWithExtent, dimensions, extent, overlay, toList, toVector, setAt, double, toSparse, overlap, isWithinBounds, Grid, emptyGrid, rotateClockwise, rotateCounterClockwise, clearLines) where

import Tetrafall.Types.Coordinate

import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List (sortBy)
import Data.Function (on)

data CellData a = 
    Sparse [(Coordinate, a)]
  | Dense (Vector (Vector a))

data Grid a = Grid
  { _extent :: (Coordinate, Coordinate)  -- (topLeft, bottomRight)
  , _cells :: (CellData a)
  , _emptyValue :: a
  }

instance Show a => Show (CellData a) where
  show (Sparse xs) = "Sparse " ++ show xs
  show (Dense xs) = "Dense " ++ show xs

instance Show a => Show (Grid a) where
  show (Grid gridExtent cells emptyValue) = 
    "Grid { _extent = " ++ show gridExtent ++ 
    ", _cells = " ++ show cells ++ 
    ", _emptyValue = " ++ show emptyValue ++ " }"

instance Eq a => Eq (CellData a) where
  (Sparse xs) == (Sparse ys) = 
    let sortByCoord = sortBy (compare `on` fst)
    in sortByCoord xs == sortByCoord ys
  (Dense xs) == (Dense ys) = xs == ys
  _ == _ = undefined -- TODO: Implement

instance Eq a => Eq (Grid a) where
  (Grid extent1 cells1 emptyValue1) == (Grid extent2 cells2 emptyValue2) =
    extent1 == extent2 && cells1 == cells2 && emptyValue1 == emptyValue2

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
makeDense (w, h) empty = Grid
  { _extent = ((0, 0), (w - 1, h - 1))
  , _cells = Dense $ V.fromList (replicate h (V.fromList (replicate w empty)))
  , _emptyValue = empty
  }

makeSparse :: Eq a => a -> [(Coordinate, a)] -> Grid a
makeSparse empty xs = Grid
    { _extent = if null filteredXs then ((0, 0), (-1, -1)) else ((minX, minY), (maxX, maxY))
    , _cells = Sparse filteredXs
    , _emptyValue = empty
    }
  where
    filteredXs = filter ((/= empty) . snd) xs
    (minX, maxX, minY, maxY) = foldl' updateBounds (maxBound, minBound, maxBound, minBound) (map fst filteredXs)
    updateBounds (minX', maxX', minY', maxY') (x, y) = (min x minX', max x maxX', min y minY', max y maxY')

makeSparseWithExtent :: Eq a => a -> (Coordinate, Coordinate) -> [(Coordinate, a)] -> Grid a
makeSparseWithExtent empty extentVal xs = Grid
    { _extent = extentVal
    , _cells = Sparse (filter ((/= empty) . snd) xs)
    , _emptyValue = empty
    }

-- Overlay foreground grid on top of background grid
overlay :: Grid a -> Grid a -> Grid a
overlay bg fg = case (_cells bg, _cells fg) of
  (Dense bgCells, Sparse fgCells) -> 
    let updatedCells = foldl' updateCell bgCells fgCells
    in bg { _cells = (Dense updatedCells) }
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
    in bg { _cells = Dense updatedCells }
  
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
    in Grid newExtent (Sparse combinedCells) (_emptyValue bg)

double :: Grid a ->  Grid a
double grid = case (_cells grid) of
  Sparse _ -> undefined
  Dense originalContents -> Grid
    { _extent = ((minX * 2, minY * 2), (maxX * 2 + 1, maxY * 2 + 1))
    , _cells = Dense $ V.fromList $ concatMap doubleRow (V.toList originalContents)
    , _emptyValue = _emptyValue grid
    }
    where
      ((minX, minY), (maxX, maxY)) = _extent grid
      doubleRow row = [doubledRow, doubledRow]
        where doubledRow = V.fromList $ concatMap (\cell -> [cell, cell]) (V.toList row)


setAt :: Eq a => Coordinate -> a -> Grid a -> Grid a
setAt (x, y) cell grid = case (_cells grid) of
  Dense cells -> 
    let ((minX, minY), (maxX, maxY)) = _extent grid
    in if x >= minX && x <= maxX && y >= minY && y <= maxY
       then grid { _cells = (Dense (cells V.// [(y - minY, (cells V.! (y - minY)) V.// [(x - minX, cell)])])) }
       else grid  -- Ignore out-of-bounds coordinates
  
  Sparse cells -> 
    let filteredCells = filter ((/= (x, y)) . fst) cells
        updatedCells = if cell == _emptyValue grid
                      then filteredCells  -- Remove the cell if setting to empty
                      else ((x, y), cell) : filteredCells  -- Add/update the cell if non-empty
        oldExtent = _extent grid
        newExtent = if null updatedCells
                   then ((0, 0), (-1, -1))  -- Empty grid
                   else if cell /= _emptyValue grid
                   then let ((oldMinX, oldMinY), (oldMaxX, oldMaxY)) = oldExtent
                        in ((min x oldMinX, min y oldMinY), (max x oldMaxX, max y oldMaxY))
                   else oldExtent  -- Don't expand extent when removing cells
    in Grid newExtent (Sparse updatedCells) (_emptyValue grid)

emptyGrid :: a -> Grid a
emptyGrid empty = Grid ((0, 0), (-1, -1)) (Dense V.empty) empty

toSparse :: Eq a => Grid a -> [(Coordinate, a)]
toSparse grid = case (_cells grid) of
  Sparse xs -> xs
  Dense _ -> filter ((/= _emptyValue grid) . snd) (toList grid)

overlap :: Eq a => Grid a -> Grid a -> Bool
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
  
-- Rotate a square grid clockwise around its center
rotateClockwise :: Eq a => Grid a -> Grid a
rotateClockwise grid = 
    let gridCells = toList grid
        ((minX, minY), (maxX, maxY)) = _extent grid
        
        -- Calculate the center of the square (floating point for precision)
        centerX = fromIntegral (minX + maxX) / (2.0 :: Double)
        centerY = fromIntegral (minY + maxY) / (2.0 :: Double)
        
        -- Rotate each cell around the center
        rotatedCells = map (\((x, y), cell) -> 
            let -- Translate to center
                relX = fromIntegral x - centerX
                relY = fromIntegral y - centerY
                -- Clockwise rotation: (x, y) -> (y, -x)
                newRelX = relY
                newRelY = -relX
                -- Translate back
                newX = round (newRelX + centerX)
                newY = round (newRelY + centerY)
            in ((newX, newY), cell)) gridCells
        
    in if null rotatedCells 
       then emptyGrid (_emptyValue grid)
       else makeSparse (_emptyValue grid) rotatedCells

-- Rotate a square grid counter-clockwise around its center
rotateCounterClockwise :: Eq a => Grid a -> Grid a  
rotateCounterClockwise grid =
    let gridCells = toList grid
        ((minX, minY), (maxX, maxY)) = _extent grid
        
        -- Calculate the center of the square (floating point for precision)
        centerX = fromIntegral (minX + maxX) / (2.0 :: Double)
        centerY = fromIntegral (minY + maxY) / (2.0 :: Double)
        
        -- Rotate each cell around the center
        rotatedCells = map (\((x, y), cell) -> 
            let -- Translate to center
                relX = fromIntegral x - centerX
                relY = fromIntegral y - centerY
                -- Counter-clockwise rotation: (x, y) -> (-y, x)
                newRelX = -relY
                newRelY = relX
                -- Translate back
                newX = round (newRelX + centerX)
                newY = round (newRelY + centerY)
            in ((newX, newY), cell)) gridCells
        
    in if null rotatedCells
       then emptyGrid (_emptyValue grid)
       else makeSparse (_emptyValue grid) rotatedCells

-- Check if a row is completely filled (no empty cells)
isCompleteLine :: Eq a => a -> Vector a -> Bool
isCompleteLine emptyValue row = V.all (/= emptyValue) row

-- Remove complete lines and shift cells down
clearLines :: Eq a => Grid a -> Grid a
clearLines grid = case (_cells grid) of
  Dense gridVector -> 
    let emptyValue = _emptyValue grid
        -- Find lines that are completely filled
        completeLineIndices = V.filter (\(_, row) -> isCompleteLine emptyValue row) $ V.indexed $ gridVector
        completeIndices = V.map fst completeLineIndices
        -- Keep only non-complete lines
        remainingLines = V.ifilter (\i _ -> i `V.notElem` completeIndices) gridVector
        -- Calculate how many lines were cleared
        numClearedLines = V.length completeLineIndices
        (gridWidth, _) = dimensions grid
        -- Create empty lines to add at the top
        emptyLines = V.replicate numClearedLines (V.replicate gridWidth emptyValue)
        -- Combine empty lines at top with remaining lines
        newGridVector = emptyLines V.++ remainingLines
    in grid { _cells = Dense newGridVector }
  
  Sparse _ -> grid  -- For sparse grids, we don't support line clearing yet
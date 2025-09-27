module Tetrafall.Game (step, apply, getTetrominoGrid) where

import Tetrafall.Types
import Tetrafall.Types.Grid
import Lens.Micro.Platform

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap, fromList)

import System.Random (StdGen, randomR, mkStdGen)

step :: Game -> Game
step game = 
    let newGame = over score ((+) 1) game
    in case newGame ^. currentPiece of
        Nothing -> newGame
        Just piece -> 
            let movedPiece = over position (\(x, y) -> (x, y + 1)) piece
                movedPieceGrid = getTetrominoGrid movedPiece
                baseGrid = newGame ^. grid
                canFall = not (overlap baseGrid movedPieceGrid) && isWithinBounds baseGrid movedPieceGrid
            in if canFall
               then -- Piece can fall normally, reset slide state
                   newGame & currentPiece .~ Just movedPiece & slideState .~ CanFall
               else -- Piece cannot fall, check slide state
                   case newGame ^. slideState of
                       CanFall -> 
                           -- First time piece can't fall, enter sliding state
                           newGame & slideState .~ Sliding (piece ^. position)
                       Sliding originalPos -> 
                           -- Check if piece has moved since sliding started
                           if piece ^. position == originalPos
                           then -- Piece hasn't moved, lock it down
                               let currentPieceGrid = getTetrominoGrid piece
                                   gridWithPiece = baseGrid `overlay` currentPieceGrid
                                   newGrid = clearLines gridWithPiece
                                   (newPiece, newRng) = randomTetromino (newGame ^. rng)
                               in newGame & grid .~ newGrid 
                                         & currentPiece .~ Just newPiece
                                         & slideState .~ CanFall
                                         & rng .~ newRng
                           else -- Piece has moved, continue sliding with new position
                               newGame & slideState .~ Sliding (piece ^. position)
                       ShouldLock -> 
                           let currentPieceGrid = getTetrominoGrid piece
                               gridWithPiece = baseGrid `overlay` currentPieceGrid
                               newGrid = clearLines gridWithPiece
                               (newPiece, newRng) = randomTetromino (newGame ^. rng)
                           in newGame & grid .~ newGrid 
                                     & currentPiece .~ Just newPiece
                                     & slideState .~ CanFall
                                     & rng .~ newRng

apply :: Action -> Game -> Game
apply ActionLeft game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position) - 1, snd (piece ^. position))
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionRight game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position) + 1, snd (piece ^. position))
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionRotateCW game =
  case game ^. currentPiece of
    Nothing -> game
    Just piece ->
      let newOrientation = rotateCW (piece ^. orientation)
          newPiece = piece & orientation .~ newOrientation
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionRotateCCW game =
  case game ^. currentPiece of
    Nothing -> game
    Just piece ->
      let newOrientation = rotateCCW (piece ^. orientation)
          newPiece = piece & orientation .~ newOrientation
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionSoftDrop game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position), snd (piece ^. position) + 1)
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionHardDrop game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let finalPiece = hardDropPiece game piece
      in game & currentPiece .~ Just finalPiece & slideState .~ ShouldLock

hardDropPiece :: Game -> Tetromino -> Tetromino
hardDropPiece game piece =
  let (x, y) = piece ^. position
      tryPosition newY = 
        let candidatePiece = piece & position .~ (x, newY)
        in if isValidMove game candidatePiece
           then tryPosition (newY + 1)
           else piece & position .~ (x, newY - 1)
  in tryPosition (y + 1)

isValidMove :: Game -> Tetromino -> Bool
isValidMove game piece =
  let pieceGrid = getTetrominoGrid piece
      gameGrid = game ^. grid
  in isWithinBounds gameGrid pieceGrid && not (overlap gameGrid pieceGrid)

getTetrominoGrid :: Tetromino -> Grid Cell
getTetrominoGrid piece =
  let shapeGrid = case HashMap.lookup (piece ^. tetrominoType) defaultTetrominoMap of
        Just shape -> shape
        Nothing -> emptyGrid Empty
      -- Apply rotations based on orientation
      rotatedGrid = case piece ^. orientation of
        North -> shapeGrid
        East -> rotateClockwise shapeGrid
        South -> rotateClockwise (rotateClockwise shapeGrid)
        West -> rotateCounterClockwise shapeGrid
      (dx, dy) = piece ^. position
      translatedCells = map (\((x, y), cell) -> ((x + dx, y + dy), cell)) (toSparse rotatedGrid)
  in makeSparse Empty translatedCells

-- Create a random tetromino at the spawn position
randomTetromino :: StdGen -> (Tetromino, StdGen)
randomTetromino gen = 
  let allTypes = [S, Z, J, L, O, I, T]
      (index, newGen) = randomR (0, length allTypes - 1) gen
      selectedType = allTypes !! index
  in (Tetromino selectedType (4, 1) North, newGen)

-- Helper function to reset slide state when piece moves
resetSlideState :: Game -> Game
resetSlideState = slideState .~ CanFall
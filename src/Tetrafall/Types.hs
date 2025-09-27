{-# LANGUAGE TemplateHaskell #-}

module Tetrafall.Types 
  ( Coordinate
  , TetrominoType(..)
  , Orientation(..)
  , Cell(..)
  , Grid
  , Game(..)
  , Tetromino(..)
  , Tick(..)
  , Action(..)
  , SlideState(..)
  , apply
  , tetrominoI
  , randomTetromino
  , defaultTetrominoMap
  , TetrominoMap
  , dimensions
  , setAt
  , grid
  , score
  , currentPiece
  , slideState
  , rng
  , tetrominoType
  , position
  , orientation
  , getTetrominoGrid
  ) where


import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap, fromList)
import Data.Hashable (Hashable(..))
import Lens.Micro.Platform
import System.Random (StdGen, randomR)

import Tetrafall.Types.Coordinate
import Tetrafall.Types.Grid

data Tick = Tick

data TetrominoType = S | Z | J | L | O | I | T
  deriving (Show, Enum, Eq, Ord)

instance Hashable TetrominoType where
  hashWithSalt s = hashWithSalt s . fromEnum

data Cell = Empty | Garbage | TetrominoCell TetrominoType
  deriving (Eq, Ord, Show)



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
  , _rng :: StdGen
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
  , (T, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), TetrominoCell T), ((0, 0), TetrominoCell T), ((1, 0), TetrominoCell T), ((0, 1), TetrominoCell T)])
  , (S, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((0, 0), TetrominoCell S), ((1, 0), TetrominoCell S), ((0, -1), TetrominoCell S), ((1, -1), TetrominoCell S)])
  , (Z, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, -1), TetrominoCell Z), ((0, -1), TetrominoCell Z), ((0, 0), TetrominoCell Z), ((1, 0), TetrominoCell Z)])
  , (J, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), TetrominoCell J), ((0, 0), TetrominoCell J), ((1, 0), TetrominoCell J), ((-1, -1), TetrominoCell J)])
  , (L, makeSparseWithExtent Empty ((-1, -1), (1, 1)) [((-1, 0), TetrominoCell L), ((0, 0), TetrominoCell L), ((1, 0), TetrominoCell L), ((1, -1), TetrominoCell L)])
  , (O, makeSparseWithExtent Empty ((0, -1), (1, 0)) [((0, 0), TetrominoCell O), ((1, 0), TetrominoCell O), ((0, -1), TetrominoCell O), ((1, -1), TetrominoCell O)])
  ]

-- Helper function to reset slide state when piece moves
resetSlideState :: Game -> Game
resetSlideState = slideState .~ CanFall

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

tetrominoI :: Tetromino
tetrominoI = Tetromino
  { _tetrominoType = I
  , _position = (4, 1)
  , _orientation = North
  }

-- Create a random tetromino at the spawn position
randomTetromino :: StdGen -> (Tetromino, StdGen)
randomTetromino gen = 
  let allTypes = [S, Z, J, L, O, I, T]
      (index, newGen) = randomR (0, length allTypes - 1) gen
      selectedType = allTypes !! index
  in (Tetromino selectedType (4, 1) North, newGen)
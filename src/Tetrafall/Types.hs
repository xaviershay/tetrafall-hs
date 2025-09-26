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
  , apply
  , tetrominoI
  , defaultTetrominoMap
  , TetrominoMap
  , dimensions
  , setAt
  , grid
  , score
  , currentPiece
  , tetrominoType
  , position
  , orientation
  ) where


import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap, fromList)
import Data.Hashable (Hashable(..))
import Lens.Micro.Platform

import Tetrafall.Types.Coordinate
import Tetrafall.Types.Grid

data Tick = Tick

data TetrominoType = S | Z | J | L | O | I | T
  deriving (Show, Enum, Eq, Ord)

instance Hashable TetrominoType where
  hashWithSalt s = hashWithSalt s . fromEnum

data Cell = Empty | Garbage | TetrominoCell TetrominoType
  deriving (Eq)



data Orientation = North | East | South | West
  deriving (Show, Enum, Eq)

data Tetromino = Tetromino
  { _tetrominoType :: TetrominoType
  , _position :: Coordinate
  , _orientation :: Orientation
  } deriving (Eq, Show)
makeLenses ''Tetromino

data Action =
    ActionLeft
  | ActionRight
  deriving (Eq, Show)

data Game = Game
  { _grid :: Grid Cell
  , _currentPiece :: Maybe Tetromino
  , _score :: Int
  }

makeLenses ''Game

type TetrominoMap = HashMap TetrominoType (Grid Cell)

defaultTetrominoMap :: TetrominoMap
defaultTetrominoMap = fromList
  [ (I, makeSparse Empty [((- 1, 0), TetrominoCell I), ((0, 0), TetrominoCell I), ((1, 0), TetrominoCell I), ((2, 0), TetrominoCell I)])
  , (T, makeSparse Empty [((-1, 0), TetrominoCell T), ((0, 0), TetrominoCell T), ((1, 0), TetrominoCell T), ((0, 1), TetrominoCell T)])
  , (S, makeSparse Empty [((0, 0), TetrominoCell S), ((1, 0), TetrominoCell S), ((0, -1), TetrominoCell S), ((1, -1), TetrominoCell S)])
  , (Z, makeSparse Empty [((-1, -1), TetrominoCell Z), ((0, -1), TetrominoCell Z), ((0, 0), TetrominoCell Z), ((1, 0), TetrominoCell Z)])
  , (J, makeSparse Empty [((-1, 0), TetrominoCell J), ((0, 0), TetrominoCell J), ((1, 0), TetrominoCell J), ((-1, -1), TetrominoCell J)])
  , (L, makeSparse Empty [((-1, 0), TetrominoCell L), ((0, 0), TetrominoCell L), ((1, 0), TetrominoCell L), ((1, -1), TetrominoCell L)])
  , (O, makeSparse Empty [((0, 0), TetrominoCell O), ((1, 0), TetrominoCell O), ((0, -1), TetrominoCell O), ((1, -1), TetrominoCell O)])
  ]

apply :: Action -> Game -> Game
apply ActionLeft game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position) - 1, snd (piece ^. position))
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then game & currentPiece .~ Just newPiece
         else game

apply ActionRight game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position) + 1, snd (piece ^. position))
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then game & currentPiece .~ Just newPiece
         else game

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
      (dx, dy) = piece ^. position
      translatedCells = map (\((x, y), cell) -> ((x + dx, y + dy), cell)) (toSparse shapeGrid)
  in makeSparse Empty translatedCells

tetrominoI :: Tetromino
tetrominoI = Tetromino
  { _tetrominoType = I
  , _position = (4, 1)
  , _orientation = North
  }
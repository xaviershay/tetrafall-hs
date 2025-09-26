module Types.Types (typesTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tetrafall.Types
import Tetrafall.Types.Grid

typesTests :: TestTree  
typesTests = testGroup "Types Tests"
  [ testGroup "Action apply function"
    [ testCase "ActionLeft moves piece left when valid" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just tetrominoI 
              , _score = 0
              }
        let originalPos = case _currentPiece testGame of 
              Just p -> _position p
              Nothing -> error "Expected piece"
        let gameAfterLeft = apply ActionLeft testGame
        let newPos = case _currentPiece gameAfterLeft of 
              Just p -> _position p
              Nothing -> error "Expected piece after move"
        fst newPos @?= fst originalPos - 1
        snd newPos @?= snd originalPos

    , testCase "ActionRight moves piece right when valid" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just tetrominoI 
              , _score = 0
              }
        let originalPos = case _currentPiece testGame of 
              Just p -> _position p
              Nothing -> error "Expected piece"
        let gameAfterRight = apply ActionRight testGame
        let newPos = case _currentPiece gameAfterRight of 
              Just p -> _position p
              Nothing -> error "Expected piece after move"
        fst newPos @?= fst originalPos + 1
        snd newPos @?= snd originalPos

    , testCase "ActionLeft does nothing when no current piece" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Nothing
              , _score = 0
              }
        let gameAfterLeft = apply ActionLeft testGame
        _currentPiece gameAfterLeft @?= Nothing

    , testCase "ActionRight does nothing when no current piece" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Nothing
              , _score = 0
              }
        let gameAfterRight = apply ActionRight testGame
        _currentPiece gameAfterRight @?= Nothing

    , testCase "ActionLeft blocked by left boundary" $ do
        let piece = Tetromino 
              { _tetrominoType = I
              , _position = (0, 10)  -- At left edge
              , _orientation = North
              }
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just piece
              , _score = 0
              }
        let gameAfterLeft = apply ActionLeft testGame
        let finalPos = case _currentPiece gameAfterLeft of 
              Just p -> _position p
              Nothing -> error "Expected piece after blocked move"
        finalPos @?= _position piece  -- Should not move

    , testCase "ActionRight blocked by right boundary" $ do
        let piece = Tetromino 
              { _tetrominoType = I
              , _position = (7, 10)  -- Near right edge (I piece is 4 wide)
              , _orientation = North
              }
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just piece
              , _score = 0
              }
        let gameAfterRight = apply ActionRight testGame
        let finalPos = case _currentPiece gameAfterRight of 
              Just p -> _position p
              Nothing -> error "Expected piece after blocked move"
        finalPos @?= _position piece  -- Should not move

    , testCase "ActionLeft blocked by existing cells" $ do
        let piece = Tetromino 
              { _tetrominoType = I
              , _position = (5, 10)
              , _orientation = North
              }
        let gridWithBlockage = setAt (4, 10) Garbage (makeDense (10, 20) Empty)
        let testGame = Game
              { _grid = gridWithBlockage
              , _currentPiece = Just piece
              , _score = 0
              }
        let gameAfterLeft = apply ActionLeft testGame
        let finalPos = case _currentPiece gameAfterLeft of 
              Just p -> _position p
              Nothing -> error "Expected piece after blocked move"
        finalPos @?= _position piece  -- Should not move
    ]
  ]
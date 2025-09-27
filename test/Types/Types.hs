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
              , _slideState = CanFall
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
              , _slideState = CanFall
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
              , _slideState = CanFall
              }
        let gameAfterLeft = apply ActionLeft testGame
        _currentPiece gameAfterLeft @?= Nothing

    , testCase "ActionRight does nothing when no current piece" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Nothing
              , _score = 0
              , _slideState = CanFall
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
              , _slideState = CanFall
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
              , _slideState = CanFall
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
              , _slideState = CanFall
              }
        let gameAfterLeft = apply ActionLeft testGame
        let finalPos = case _currentPiece gameAfterLeft of 
              Just p -> _position p
              Nothing -> error "Expected piece after blocked move"
        finalPos @?= _position piece  -- Should not move

    , testCase "ActionRotateCW rotates piece clockwise when valid" $ do
        let piece = Tetromino 
              { _tetrominoType = T
              , _position = (5, 10)
              , _orientation = North
              }
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just piece
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfterRotate = apply ActionRotateCW testGame
        let newOrientation = case _currentPiece gameAfterRotate of 
              Just p -> _orientation p
              Nothing -> error "Expected piece after rotation"
        newOrientation @?= East

    , testCase "ActionRotateCCW rotates piece counter-clockwise when valid" $ do
        let piece = Tetromino 
              { _tetrominoType = T
              , _position = (5, 10)
              , _orientation = North
              }
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just piece
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfterRotate = apply ActionRotateCCW testGame
        let newOrientation = case _currentPiece gameAfterRotate of 
              Just p -> _orientation p
              Nothing -> error "Expected piece after rotation"
        newOrientation @?= West

    , testCase "ActionRotateCW does nothing when no current piece" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Nothing
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfterRotate = apply ActionRotateCW testGame
        _currentPiece gameAfterRotate @?= Nothing

    , testCase "ActionRotateCCW does nothing when no current piece" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Nothing
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfterRotate = apply ActionRotateCCW testGame
        _currentPiece gameAfterRotate @?= Nothing

    , testCase "Multiple rotations work correctly" $ do
        let piece = Tetromino 
              { _tetrominoType = T
              , _position = (5, 10)
              , _orientation = North
              }
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just piece
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfter1CW = apply ActionRotateCW testGame
        let gameAfter2CW = apply ActionRotateCW gameAfter1CW
        let gameAfter3CW = apply ActionRotateCW gameAfter2CW
        let gameAfter4CW = apply ActionRotateCW gameAfter3CW
        
        let finalOrientation = case _currentPiece gameAfter4CW of 
              Just p -> _orientation p
              Nothing -> error "Expected piece after rotations"
        finalOrientation @?= North  -- Full circle should return to North

    , testCase "ActionSoftDrop moves piece down when valid" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just tetrominoI 
              , _score = 0
              , _slideState = CanFall
              }
        let originalPos = case _currentPiece testGame of 
              Just p -> _position p
              Nothing -> error "Expected piece"
        let gameAfterDrop = apply ActionSoftDrop testGame
        let newPos = case _currentPiece gameAfterDrop of 
              Just p -> _position p
              Nothing -> error "Expected piece after move"
        fst newPos @?= fst originalPos
        snd newPos @?= snd originalPos + 1

    , testCase "ActionSoftDrop does nothing when no current piece" $ do
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Nothing
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfterDrop = apply ActionSoftDrop testGame
        _currentPiece gameAfterDrop @?= Nothing

    , testCase "ActionSoftDrop blocked by bottom boundary" $ do
        let piece = Tetromino 
              { _tetrominoType = I
              , _position = (4, 17)
              , _orientation = East
              }
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just piece
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfterDrop = apply ActionSoftDrop testGame
        let finalPos = case _currentPiece gameAfterDrop of 
              Just p -> _position p
              Nothing -> error "Expected piece after blocked move"
        finalPos @?= _position piece  -- Should not move

    , testCase "ActionSoftDrop blocked by existing cells" $ do
        let piece = Tetromino 
              { _tetrominoType = I
              , _position = (4, 10)
              , _orientation = North
              }
        let gridWithBlockage = setAt (4, 11) Garbage (makeDense (10, 20) Empty)
        let testGame = Game
              { _grid = gridWithBlockage
              , _currentPiece = Just piece
              , _score = 0
              , _slideState = CanFall
              }
        let gameAfterDrop = apply ActionSoftDrop testGame
        let finalPos = case _currentPiece gameAfterDrop of 
              Just p -> _position p
              Nothing -> error "Expected piece after blocked move"
        finalPos @?= _position piece  -- Should not move

    , testCase "ActionSoftDrop resets slide state" $ do
        let piece = Tetromino 
              { _tetrominoType = I
              , _position = (4, 10)
              , _orientation = North
              }
        let testGame = Game
              { _grid = makeDense (10, 20) Empty
              , _currentPiece = Just piece
              , _score = 0
              , _slideState = Sliding (4, 10)  -- In sliding state
              }
        let gameAfterDrop = apply ActionSoftDrop testGame
        _slideState gameAfterDrop @?= CanFall  -- Should reset to CanFall
    ]
  ]
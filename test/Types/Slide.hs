module Types.Slide (slideTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tetrafall.Types
import Tetrafall.Types.Grid
import Lens.Micro.Platform

-- Helper to create a game with a piece that can't fall
createSlidingGame :: Game
createSlidingGame = 
  let piece = Tetromino I (4, 19) North  -- I piece near bottom
      grid = setAt (4, 20) Garbage (makeDense (10, 22) Empty)  -- Block directly below
  in Game
     { _grid = grid
     , _currentPiece = Just piece
     , _score = 0
     , _slideState = CanFall
     }

-- Helper to simulate step function (simplified version for testing)
-- This is a simplified version that focuses on the slide logic
simulateStep :: Game -> Game
simulateStep game =
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
        let movedPiece = over position (\(x, y) -> (x, y + 1)) piece
            movedPieceGrid = getTetrominoGrid movedPiece
            baseGrid = game ^. grid
            canFall = not (overlap baseGrid movedPieceGrid) && isWithinBounds baseGrid movedPieceGrid
        in if canFall
           then -- Piece can fall normally, reset slide state
               game & currentPiece .~ Just movedPiece & slideState .~ CanFall
           else -- Piece cannot fall, check slide state
               case game ^. slideState of
                   CanFall -> 
                       -- First time piece can't fall, enter sliding state
                       game & slideState .~ Sliding (piece ^. position)
                   Sliding originalPos -> 
                       -- Check if piece has moved since sliding started
                       if piece ^. position == originalPos
                       then -- Piece hasn't moved, lock it down
                           let currentPieceGrid = getTetrominoGrid piece
                               newGrid = baseGrid `overlay` currentPieceGrid
                           in game & grid .~ newGrid 
                                   & currentPiece .~ Just tetrominoI 
                                   & slideState .~ CanFall
                       else -- Piece has moved, continue sliding with new position
                           game & slideState .~ Sliding (piece ^. position)

slideTests :: TestTree
slideTests = testGroup "Slide Tests"
  [ testCase "Piece enters sliding state when it can't fall" $ do
      let game = createSlidingGame
      let gameAfterStep = simulateStep game
      case gameAfterStep ^. slideState of
        Sliding _ -> return ()
        _ -> assertFailure "Expected piece to enter sliding state"

  , testCase "Piece locks after one step if no movement" $ do
      let game = createSlidingGame
      let gameAfterFirstStep = simulateStep game
      let gameAfterSecondStep = simulateStep gameAfterFirstStep
      -- Should have locked the piece and spawned a new one
      case gameAfterSecondStep ^. currentPiece of
        Just newPiece -> do
          -- Should be a new I piece at spawn position
          (newPiece ^. tetrominoType) @?= I
          (newPiece ^. position) @?= (4, 1)  -- tetrominoI spawn position
        Nothing -> assertFailure "Expected new piece after locking"

  , testCase "Piece continues sliding if it moves" $ do
      let game = createSlidingGame
      let gameAfterFirstStep = simulateStep game  -- Enter sliding state
      let gameAfterMove = apply ActionLeft gameAfterFirstStep  -- Move left
      let gameAfterSecondStep = simulateStep gameAfterMove  -- Should continue sliding
      
      case gameAfterSecondStep ^. slideState of
        Sliding pos -> do
          -- Should be sliding at the new position (moved left)
          let originalPos = case createSlidingGame ^. currentPiece of
                Just p -> p ^. position
                Nothing -> error "Expected piece"
          pos @?= (fst originalPos - 1, snd originalPos)
        _ -> assertFailure "Expected piece to continue sliding after movement"

  , testCase "Piece resets slide state when it can fall again" $ do
      let piece = Tetromino I (4, 10) North  -- I piece in middle of field
      let game = Game
           { _grid = makeDense (10, 22) Empty  -- No obstacles
           , _currentPiece = Just piece
           , _score = 0
           , _slideState = Sliding (4, 9)  -- Previously sliding
           }
      let gameAfterStep = simulateStep game
      
      -- Should have fallen and reset slide state
      case gameAfterStep ^. currentPiece of
        Just newPiece -> do
          (newPiece ^. position) @?= (4, 11)  -- Should have moved down
          (gameAfterStep ^. slideState) @?= CanFall  -- Should reset slide state
        Nothing -> assertFailure "Expected piece to move down"

  , testCase "Actions reset slide state when piece moves" $ do
      let game = Game
           { _grid = makeDense (10, 22) Empty
           , _currentPiece = Just (Tetromino I (4, 10) North)
           , _score = 0
           , _slideState = Sliding (4, 9)  -- Currently sliding
           }
      
      -- Test left movement
      let gameAfterLeft = apply ActionLeft game
      (gameAfterLeft ^. slideState) @?= CanFall
      
      -- Test right movement  
      let gameAfterRight = apply ActionRight game
      (gameAfterRight ^. slideState) @?= CanFall
      
      -- Test rotation
      let gameAfterRotate = apply ActionRotateCW game
      (gameAfterRotate ^. slideState) @?= CanFall

  , testCase "Edge case: piece moves left then right should not lock" $ do
      let game = createSlidingGame
      let gameAfterFirstStep = simulateStep game  -- Enter sliding state
      let gameAfterLeftMove = apply ActionLeft gameAfterFirstStep  -- Move left
      let gameAfterRightMove = apply ActionRight gameAfterLeftMove  -- Move back right
      let gameAfterSecondStep = simulateStep gameAfterRightMove  -- Should continue sliding
      
      case gameAfterSecondStep ^. slideState of
        Sliding _ -> return ()  -- Should still be sliding
        CanFall -> assertFailure "Piece should still be sliding, not able to fall"
        -- Note: The piece shouldn't lock because it did move (even though it's back to original position)
  ]
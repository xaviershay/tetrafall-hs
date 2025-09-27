module Types.Slide (slideTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tetrafall.Types
import Tetrafall.Types.Grid
import Tetrafall.Game
import Lens.Micro.Platform

slideTests :: TestTree
slideTests = testGroup "Slide Tests"
  [ testCase "Actions reset slide state when piece moves" $ do
      let game = defaultGame
           { _grid = makeDense (10, 22) Empty
           , _currentPiece = Just (Tetromino I (4, 10) North)
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

  ]
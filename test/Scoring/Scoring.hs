module Scoring.Scoring (scoringTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tetrafall.Types
import Tetrafall.Scoring

scoringTests :: TestTree  
scoringTests = testGroup "Scoring Tests"
  [ testCase "Single line clear (level 1)" $ do
      let event = ScoreEvent 1 1
      calculateScore event @?= 100

  , testCase "Double line clear (level 1)" $ do
      let event = ScoreEvent 2 1
      calculateScore event @?= 300

  , testCase "Triple line clear (level 1)" $ do
      let event = ScoreEvent 3 1  
      calculateScore event @?= 500

  , testCase "Tetris line clear (level 1)" $ do
      let event = ScoreEvent 4 1
      calculateScore event @?= 800

  , testCase "No lines cleared" $ do
      let event = ScoreEvent 0 1
      calculateScore event @?= 0

  , testCase "Invalid line count (5 lines)" $ do
      let event = ScoreEvent 5 1
      calculateScore event @?= 0

  , testCase "Single line clear with higher level (level 3)" $ do
      let event = ScoreEvent 1 3
      calculateScore event @?= 300  -- 100 × 3

  , testCase "Double line clear with higher level (level 2)" $ do
      let event = ScoreEvent 2 2
      calculateScore event @?= 600  -- 300 × 2

  , testCase "Triple line clear with higher level (level 4)" $ do
      let event = ScoreEvent 3 4
      calculateScore event @?= 2000  -- 500 × 4

  , testCase "Tetris with higher level (level 5)" $ do
      let event = ScoreEvent 4 5
      calculateScore event @?= 4000  -- 800 × 5
  ]
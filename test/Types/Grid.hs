module Types.Grid (gridTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tetrafall.Types.Grid

gridTests :: TestTree  
gridTests = testGroup "Grid Tests"
  [ testGroup "Grid creation"
    [ testCase "Dense grid dimensions" $ do
        let grid = makeDense (3, 4) 0
        dimensions grid @?= (3, 4)
    
    , testCase "Sparse grid dimensions" $ do
        let coords = [((0, 0), 'a'), ((2, 1), 'b')]
        let grid = makeSparse coords
        dimensions grid @?= (3, 2)  -- width=3 (0 to 2), height=2 (0 to 1)
    
    , testCase "Empty sparse grid" $ do
        let grid = makeSparse []
        dimensions grid @?= (0, 0)
    ]
  
  , testGroup "Grid overlay function" 
    [ testCase "Overlay dense over sparse - TODO" $ do
        -- TODO: Implement overlay function tests
        -- When implemented, this should test overlaying a dense grid over a sparse one
        let sparse = makeSparse [((1, 1), 'X')]
        let dense = makeDense (3, 3) 'O'
        -- overlay dense sparse should place 'X' at position (1,1) over the dense grid
        -- For now, we'll skip this test
        return ()
        
    , testCase "Overlay sparse over dense - basic functionality" $ do
        -- Test the one implemented case: sparse over dense
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse [((1, 1), 'X'), ((0, 2), 'Y')]
        let result = overlay dense sparse
        
        -- This should work since it's the implemented case
        dimensions result @?= (3, 3)
        
    , testCase "Overlay other combinations - TODO" $ do
        -- TODO: Implement remaining overlay combinations
        -- - Dense over Dense
        -- - Sparse over Sparse  
        -- - Sparse over Dense (other direction)
        return ()
    ]
  ]
module Types.Grid (gridTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tetrafall.Types (Coordinate)
import Tetrafall.Types.Grid

gridTests :: TestTree  
gridTests = testGroup "Grid Tests"
  [ testGroup "Grid creation"
    [ testCase "Dense grid dimensions" $ do
        let grid = makeDense (3, 4) 0
        dimensions grid @?= (3, 4)
    
    , testCase "Sparse grid dimensions and contents" $ do
        let coords = [((0, 0), 'a'), ((2, 1), 'b')]
        let grid = makeSparse coords
        dimensions grid @?= (3, 2)  -- width=3 (0 to 2), height=2 (0 to 1)
        toList grid @?= coords
    
    , testCase "Empty sparse grid" $ do
        let grid = makeSparse ([] :: [(Coordinate, Char)])
        dimensions grid @?= (0, 0)
        toList grid @?= []
    
    , testCase "Dense grid contents" $ do
        let grid = makeDense (2, 2) 'X'
        dimensions grid @?= (2, 2)
        toList grid @?= [((0, 0), 'X'), ((1, 0), 'X'), ((0, 1), 'X'), ((1, 1), 'X')]
    
    , testCase "Dense grid 1x1" $ do
        let grid = makeDense (1, 1) 42
        toList grid @?= [((0, 0), 42)]
    
    , testCase "Dense grid 3x1" $ do
        let grid = makeDense (3, 1) 'A'
        toList grid @?= [((0, 0), 'A'), ((1, 0), 'A'), ((2, 0), 'A')]
    
    , testCase "Dense grid 1x3" $ do
        let grid = makeDense (1, 3) 'B'
        toList grid @?= [((0, 0), 'B'), ((0, 1), 'B'), ((0, 2), 'B')]
    ]
  
  , testGroup "toList function"
    [ testCase "Sparse grid toList preserves input order" $ do
        let coords = [((2, 1), 'z'), ((0, 0), 'a'), ((1, 3), 'x')]
        let grid = makeSparse coords
        toList grid @?= coords
    
    , testCase "Dense grid toList order" $ do
        -- Dense grids should list in row-major order: (0,0), (1,0), (0,1), (1,1)
        let grid = makeDense (2, 2) 'T'
        let expected = [((0, 0), 'T'), ((1, 0), 'T'), ((0, 1), 'T'), ((1, 1), 'T')]
        toList grid @?= expected
    
    , testCase "toList roundtrip for sparse" $ do
        -- Converting sparse to list and back should preserve the data
        let coords = [((5, 2), 'h'), ((1, 8), 'e'), ((3, 0), 'l')]
        let grid = makeSparse coords
        let reconstructed = makeSparse (toList grid)
        toList reconstructed @?= coords
        dimensions reconstructed @?= dimensions grid
    ]
  
  , testGroup "Grid overlay function" 
    [ testCase "Overlay sparse over dense - basic functionality" $ do
        -- Test overlaying sparse grid on dense background
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse [((1, 1), 'X'), ((0, 2), 'Y')]
        let result = overlay dense sparse
        
        -- Result should have same dimensions as background (dense)
        dimensions result @?= (3, 3)
        -- Check that overlay positions were updated
        let resultList = toList result
        lookup (1, 1) resultList @?= Just 'X'
        lookup (0, 2) resultList @?= Just 'Y'
        -- Check that non-overlay positions retain original value
        lookup (0, 0) resultList @?= Just 'O'
        lookup (2, 2) resultList @?= Just 'O'
        
    , testCase "Overlay sparse over dense - empty sparse" $ do
        -- Overlaying empty sparse grid should leave dense unchanged
        let dense = makeDense (2, 2) 'A'
        let sparse = makeSparse []
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        -- All cells should remain 'A'
        toList result @?= [((0, 0), 'A'), ((1, 0), 'A'), ((0, 1), 'A'), ((1, 1), 'A')]
        
    , testCase "Overlay sparse over dense - single cell" $ do
        -- Test with single sparse cell
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse [((1, 1), 'X')]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (1, 1) resultList @?= Just 'X'  -- Changed cell
        lookup (0, 0) resultList @?= Just 'O'  -- Unchanged cell
        
    , testCase "Overlay sparse over dense - corner positions" $ do
        -- Test overlaying at corner positions
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse [((0, 0), 'A'), ((2, 0), 'B'), ((0, 2), 'C'), ((2, 2), 'D')]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just 'A'  -- Top-left
        lookup (2, 0) resultList @?= Just 'B'  -- Top-right
        lookup (0, 2) resultList @?= Just 'C'  -- Bottom-left
        lookup (2, 2) resultList @?= Just 'D'  -- Bottom-right
        lookup (1, 1) resultList @?= Just 'O'  -- Center unchanged
        
    , testCase "Overlay sparse over dense - overwrite same position" $ do
        -- Test multiple values at same position (last should win)
        let dense = makeDense (2, 2) 'O'
        let sparse = makeSparse [((0, 0), 'X'), ((0, 0), 'Y')]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just 'Y'  -- Last value should win
        
    , testCase "Overlay sparse over dense - full coverage" $ do
        -- Test sparse grid that covers all positions of dense grid
        let dense = makeDense (2, 2) 'O'
        let sparse = makeSparse [((0, 0), 'A'), ((1, 0), 'B'), ((0, 1), 'C'), ((1, 1), 'D')]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        toList result @?= [((0, 0), 'A'), ((1, 0), 'B'), ((0, 1), 'C'), ((1, 1), 'D')]
        
    , testCase "Overlay sparse over dense - large grid with few sparse cells" $ do
        -- Test performance/correctness with large background and few overlay cells
        let dense = makeDense (10, 10) 'Z'
        let sparse = makeSparse [((5, 5), 'C'), ((0, 9), 'K')]
        let result = overlay dense sparse
        
        dimensions result @?= (10, 10)
        let resultList = toList result
        lookup (5, 5) resultList @?= Just 'C'
        lookup (0, 9) resultList @?= Just 'K'
        lookup (0, 0) resultList @?= Just 'Z'  -- Check an unchanged position
        -- Verify we have 100 total cells
        length resultList @?= 100
        
    , testCase "Overlay sparse over dense - boundary positions" $ do
        -- Test all edge and corner positions on a 3x3 grid
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse 
              [ -- Top edge
                ((0, 0), '1'), ((1, 0), '2'), ((2, 0), '3')
                -- Side edges  
              , ((0, 1), '4'), ((2, 1), '5')
                -- Bottom edge
              , ((0, 2), '6'), ((1, 2), '7'), ((2, 2), '8')
              ]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        -- Check all border positions
        lookup (0, 0) resultList @?= Just '1'
        lookup (1, 0) resultList @?= Just '2'
        lookup (2, 0) resultList @?= Just '3'
        lookup (0, 1) resultList @?= Just '4'
        lookup (2, 1) resultList @?= Just '5'
        lookup (0, 2) resultList @?= Just '6'
        lookup (1, 2) resultList @?= Just '7'
        lookup (2, 2) resultList @?= Just '8'
        -- Check center remains unchanged
        lookup (1, 1) resultList @?= Just 'O'
        
    , testCase "Overlay sparse over dense - out of bounds cells ignored" $ do
        -- Test that sparse cells outside dense grid bounds are ignored
        let dense = makeDense (2, 2) 'D'  -- 2x2 grid: valid coords are (0,0), (1,0), (0,1), (1,1)
        let sparse = makeSparse 
              [ -- Valid positions
                ((0, 0), 'A'), ((1, 1), 'B')
                -- Out of bounds positions (should be ignored)
              , ((2, 0), 'X'), ((0, 2), 'Y'), ((3, 3), 'Z'), ((-1, 0), 'W'), ((1, -1), 'V')
              ]
        let result = overlay dense sparse
        
        -- Dimensions should remain the same as dense grid
        dimensions result @?= (2, 2)
        let resultList = toList result
        
        -- Valid overlay positions should be updated
        lookup (0, 0) resultList @?= Just 'A'
        lookup (1, 1) resultList @?= Just 'B'
        
        -- Positions not in sparse should retain dense values
        lookup (1, 0) resultList @?= Just 'D'
        lookup (0, 1) resultList @?= Just 'D'
        
        -- Should still have exactly 4 cells (2x2)
        length resultList @?= 4
        
        -- Out of bounds coordinates should not appear in result
        lookup (2, 0) resultList @?= Nothing
        lookup (0, 2) resultList @?= Nothing
        lookup (3, 3) resultList @?= Nothing
        lookup (-1, 0) resultList @?= Nothing
        lookup (1, -1) resultList @?= Nothing
    ]
  ]
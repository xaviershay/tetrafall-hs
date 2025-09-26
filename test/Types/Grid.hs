module Types.Grid (gridTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Monoid (Sum(..))

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
        let dense = makeDense (2, 2) 'A'
        let sparse = makeSparse []
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        toList result @?= [((0, 0), 'A'), ((1, 0), 'A'), ((0, 1), 'A'), ((1, 1), 'A')]
        
    , testCase "Overlay sparse over dense - single cell" $ do
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse [((1, 1), 'X')]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (1, 1) resultList @?= Just 'X'
        lookup (0, 0) resultList @?= Just 'O'
        
    , testCase "Overlay sparse over dense - corner positions" $ do
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse [((0, 0), 'A'), ((2, 0), 'B'), ((0, 2), 'C'), ((2, 2), 'D')]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just 'A'
        lookup (2, 0) resultList @?= Just 'B'
        lookup (0, 2) resultList @?= Just 'C'
        lookup (2, 2) resultList @?= Just 'D'
        lookup (1, 1) resultList @?= Just 'O'
        
    , testCase "Overlay sparse over dense - overwrite same position" $ do
        let dense = makeDense (2, 2) 'O'
        let sparse = makeSparse [((0, 0), 'X'), ((0, 0), 'Y')]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just 'Y'
        
    , testCase "Overlay sparse over dense - full coverage" $ do
        let dense = makeDense (2, 2) 'O'
        let sparse = makeSparse [((0, 0), 'A'), ((1, 0), 'B'), ((0, 1), 'C'), ((1, 1), 'D')]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        toList result @?= [((0, 0), 'A'), ((1, 0), 'B'), ((0, 1), 'C'), ((1, 1), 'D')]
        
    , testCase "Overlay sparse over dense - large grid with few sparse cells" $ do
        let dense = makeDense (10, 10) 'Z'
        let sparse = makeSparse [((5, 5), 'C'), ((0, 9), 'K')]
        let result = overlay dense sparse
        
        dimensions result @?= (10, 10)
        let resultList = toList result
        lookup (5, 5) resultList @?= Just 'C'
        lookup (0, 9) resultList @?= Just 'K'
        lookup (0, 0) resultList @?= Just 'Z'
        length resultList @?= 100
        
    , testCase "Overlay sparse over dense - boundary positions" $ do
        let dense = makeDense (3, 3) 'O'
        let sparse = makeSparse 
              [ ((0, 0), '1'), ((1, 0), '2'), ((2, 0), '3')
              , ((0, 1), '4'), ((2, 1), '5')
              , ((0, 2), '6'), ((1, 2), '7'), ((2, 2), '8')
              ]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just '1'
        lookup (1, 0) resultList @?= Just '2'
        lookup (2, 0) resultList @?= Just '3'
        lookup (0, 1) resultList @?= Just '4'
        lookup (2, 1) resultList @?= Just '5'
        lookup (0, 2) resultList @?= Just '6'
        lookup (1, 2) resultList @?= Just '7'
        lookup (2, 2) resultList @?= Just '8'
        lookup (1, 1) resultList @?= Just 'O'
        
    , testCase "Overlay sparse over dense - out of bounds cells ignored" $ do
        let dense = makeDense (2, 2) 'D'
        let sparse = makeSparse 
              [ ((0, 0), 'A'), ((1, 1), 'B')
              , ((2, 0), 'X'), ((0, 2), 'Y'), ((3, 3), 'Z'), ((-1, 0), 'W'), ((1, -1), 'V')
              ]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        let resultList = toList result
        
        lookup (0, 0) resultList @?= Just 'A'
        lookup (1, 1) resultList @?= Just 'B'
        
        lookup (1, 0) resultList @?= Just 'D'
        lookup (0, 1) resultList @?= Just 'D'
        
        length resultList @?= 4
        
        lookup (2, 0) resultList @?= Nothing
        lookup (0, 2) resultList @?= Nothing
        lookup (3, 3) resultList @?= Nothing
        lookup (-1, 0) resultList @?= Nothing
        lookup (1, -1) resultList @?= Nothing
    ]

  , testGroup "Grid overlap function"
    [ testCase "No overlap - completely separate grids" $ do
        let grid1 = makeSparse [((0, 0), "A"), ((1, 0), "B")]
        let grid2 = makeSparse [((2, 0), "X"), ((3, 0), "Y")]
        overlap grid1 grid2 @?= False
        overlap grid2 grid1 @?= False

    , testCase "No overlap - same coordinates but one is mempty" $ do
        let grid1 = makeSparse [((0, 0), mempty), ((1, 0), mempty)]
        let grid2 = makeSparse [((0, 0), "X"), ((1, 0), "Y")]
        overlap grid1 grid2 @?= False
        overlap grid2 grid1 @?= False

    , testCase "No overlap - both have mempty at same coordinates" $ do
        let grid1 = makeSparse [((0, 0), mempty), ((1, 0), "A")]
        let grid2 = makeSparse [((0, 0), mempty), ((2, 0), "B")]
        overlap grid1 grid2 @?= False

    , testCase "Overlap - same coordinates with non-mempty values" $ do
        let grid1 = makeSparse [((0, 0), "A"), ((1, 0), "B")]
        let grid2 = makeSparse [((0, 0), "X"), ((2, 0), "Y")]
        overlap grid1 grid2 @?= True
        overlap grid2 grid1 @?= True  -- Should be symmetric

    , testCase "Overlap - multiple overlapping coordinates" $ do
        let grid1 = makeSparse [((0, 0), "A"), ((1, 0), "B"), ((2, 1), "C")]
        let grid2 = makeSparse [((0, 0), "X"), ((1, 0), "Y"), ((3, 0), "Z")]
        overlap grid1 grid2 @?= True  -- Overlaps at (0,0) and (1,0)

    , testCase "Overlap - single overlapping coordinate among many" $ do
        let grid1 = makeSparse [((0, 0), "A"), ((1, 0), "B"), ((2, 0), "C")]
        let grid2 = makeSparse [((2, 0), "X"), ((3, 0), "Y"), ((4, 0), "Z")]
        overlap grid1 grid2 @?= True  -- Overlaps only at (2,0)

    , testCase "No overlap - empty grids" $ do
        let grid1 = makeSparse ([] :: [(Coordinate, String)])
        let grid2 = makeSparse ([] :: [(Coordinate, String)])
        overlap grid1 grid2 @?= False

    , testCase "No overlap - one empty grid" $ do
        let grid1 = makeSparse [((0, 0), "A")]
        let grid2 = makeSparse ([] :: [(Coordinate, String)])
        overlap grid1 grid2 @?= False
        overlap grid2 grid1 @?= False

    , testCase "Overlap with dense grids - no overlap" $ do
        let grid1 = makeDense (2, 2) mempty
        let grid2 = makeDense (2, 2) mempty
        let grid1Modified = setAt (0, 0) "A" grid1
        let grid2Modified = setAt (1, 1) "B" grid2
        overlap grid1Modified grid2Modified @?= False

    , testCase "Overlap with dense grids - with overlap" $ do
        let grid1 = makeDense (2, 2) mempty
        let grid2 = makeDense (2, 2) mempty
        let grid1Modified = setAt (0, 0) "A" $ setAt (1, 0) "B" grid1
        let grid2Modified = setAt (0, 0) "X" $ setAt (0, 1) "Y" grid2
        overlap grid1Modified grid2Modified @?= True

    , testCase "Overlap with mixed dense/sparse - no overlap" $ do
        let denseGrid = makeDense (3, 3) mempty
        let denseModified = setAt (0, 0) "A" $ setAt (1, 1) "B" denseGrid
        let sparseGrid = makeSparse [((2, 0), "X"), ((0, 2), "Y")]
        overlap denseModified sparseGrid @?= False

    , testCase "Overlap with mixed dense/sparse - with overlap" $ do
        let denseGrid = makeDense (3, 3) mempty
        let denseModified = setAt (0, 0) "A" $ setAt (1, 1) "B" denseGrid
        let sparseGrid = makeSparse [((0, 0), "X"), ((2, 2), "Y")]
        overlap denseModified sparseGrid @?= True

    , testCase "Overlap edge case - mempty vs non-mempty boundary" $ do
        let grid1 = makeSparse [((0, 0), mempty), ((1, 0), "A"), ((2, 0), mempty)]
        let grid2 = makeSparse [((0, 0), "X"), ((1, 0), mempty), ((2, 0), "Z")]
        overlap grid1 grid2 @?= False

    , testCase "Overlap with different value types - numeric mempty" $ do
        let grid1 = makeSparse [((0, 0), 5), ((1, 0), 0), ((2, 0), 3)] :: Grid (Sum Int)
        let grid2 = makeSparse [((0, 0), 0), ((1, 0), 7), ((2, 0), 2)] :: Grid (Sum Int)
        overlap grid1 grid2 @?= True

    , testCase "Overlap symmetry property" $ do
        let grid1 = makeSparse [((0, 0), "A"), ((1, 1), "B")]
        let grid2 = makeSparse [((0, 0), "X"), ((2, 2), "Y")]
        let grid3 = makeSparse [((3, 3), "Z")]
        
        overlap grid1 grid2 @?= overlap grid2 grid1
        overlap grid1 grid3 @?= overlap grid3 grid1
        overlap grid2 grid3 @?= overlap grid3 grid2

    , testCase "Overlap with large coordinates" $ do
        let grid1 = makeSparse [((1000, 2000), "A"), ((5000, 7000), "B")]
        let grid2 = makeSparse [((1000, 2000), "X"), ((9000, 1000), "Y")]
        overlap grid1 grid2 @?= True
        
        let grid3 = makeSparse [((1001, 2000), "Z")]
        overlap grid1 grid3 @?= False

    , testCase "Overlap with negative coordinates" $ do
        -- Test with negative coordinate values
        let grid1 = makeSparse [((-1, -1), "A"), ((-5, -10), "B")]
        let grid2 = makeSparse [((-1, -1), "X"), ((0, 0), "Y")]
        overlap grid1 grid2 @?= True
        
        let grid3 = makeSparse [((-2, -1), "Z")]
        overlap grid1 grid3 @?= False
    ]
  ]
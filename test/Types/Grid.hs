module Types.Grid (gridTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Monoid (Sum(..))

import Tetrafall.Types (Coordinate, Cell(..))
import Tetrafall.Types.Grid

gridTests :: TestTree  
gridTests = testGroup "Grid Tests"
  [ testGroup "Grid creation"
    [ testCase "Dense grid dimensions" $ do
        let grid = makeDense (3, 4) 0
        dimensions grid @?= (3, 4)
    
    , testCase "Sparse grid dimensions and contents" $ do
        let coords = [((0, 0), "a"), ((2, 1), "b")]
        let grid = makeSparse mempty coords
        dimensions grid @?= (3, 2)  -- width=3 (0 to 2), height=2 (0 to 1)
        toList grid @?= coords
    
    , testCase "Empty sparse grid" $ do
        let grid = makeSparse mempty ([] :: [(Coordinate, String)])
        dimensions grid @?= (0, 0)
        toList grid @?= []
    
    , testCase "Dense grid contents" $ do
        let grid = makeDense (2, 2) "X"
        dimensions grid @?= (2, 2)
        toList grid @?= [((0, 0), "X"), ((1, 0), "X"), ((0, 1), "X"), ((1, 1), "X")]
    
    , testCase "Dense grid 1x1" $ do
        let grid = makeDense (1, 1) 42
        toList grid @?= [((0, 0), 42)]
    
    , testCase "Dense grid 3x1" $ do
        let grid = makeDense (3, 1) "A"
        toList grid @?= [((0, 0), "A"), ((1, 0), "A"), ((2, 0), "A")]
    
    , testCase "Dense grid 1x3" $ do
        let grid = makeDense (1, 3) "B"
        toList grid @?= [((0, 0), "B"), ((0, 1), "B"), ((0, 2), "B")]
    ]
  
  , testGroup "makeSparseWithExtent function"
    [ testCase "Extent larger than actual coordinates" $ do
        let coords = [((0, 0), "X")]
        let grid = makeSparseWithExtent mempty ((-2, -2), (3, 3)) coords
        extent grid @?= ((-2, -2), (3, 3))
        dimensions grid @?= (6, 6)
        toList grid @?= coords

    , testCase "Empty coords with custom extent" $ do
        let grid = makeSparseWithExtent mempty ((-1, -1), (1, 1)) ([] :: [(Coordinate, String)])
        extent grid @?= ((-1, -1), (1, 1))
        dimensions grid @?= (3, 3)
        toList grid @?= []
    ]
  
  , testGroup "toList function"
    [ testCase "Sparse grid toList preserves input order" $ do
        let coords = [((2, 1), "z"), ((0, 0), "a"), ((1, 3), "x")]
        let grid = makeSparse mempty coords
        toList grid @?= coords
    
    , testCase "Dense grid toList order" $ do
        -- Dense grids should list in row-major order: (0,0), (1,0), (0,1), (1,1)
        let grid = makeDense (2, 2) "T"
        let expected = [((0, 0), "T"), ((1, 0), "T"), ((0, 1), "T"), ((1, 1), "T")]
        toList grid @?= expected
    
    , testCase "toList roundtrip for sparse" $ do
        -- Converting sparse to list and back should preserve the data
        let coords = [((5, 2), "h"), ((1, 8), "e"), ((3, 0), "l")]
        let grid = makeSparse mempty coords
        let reconstructed = makeSparse mempty (toList grid)
        toList reconstructed @?= coords
        dimensions reconstructed @?= dimensions grid
    ]
  
  , testGroup "Grid overlay function" 
    [ testCase "Overlay sparse over dense - basic functionality" $ do
        let dense = makeDense (3, 3) "O"
        let sparse = makeSparse mempty [((1, 1), "X"), ((0, 2), "Y")]
        let result = overlay dense sparse
        
        -- Result should have same dimensions as background (dense)
        dimensions result @?= (3, 3)
        -- Check that overlay positions were updated
        let resultList = toList result
        lookup (1, 1) resultList @?= Just "X"
        lookup (0, 2) resultList @?= Just "Y"
        -- Check that non-overlay positions retain original value
        lookup (0, 0) resultList @?= Just "O"
        lookup (2, 2) resultList @?= Just "O"
        
    , testCase "Overlay sparse over dense - empty sparse" $ do
        let dense = makeDense (2, 2) "A"
        let sparse = makeSparse mempty []
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        toList result @?= [((0, 0), "A"), ((1, 0), "A"), ((0, 1), "A"), ((1, 1), "A")]
        
    , testCase "Overlay sparse over dense - single cell" $ do
        let dense = makeDense (3, 3) "O"
        let sparse = makeSparse mempty [((1, 1), "X")]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (1, 1) resultList @?= Just "X"
        lookup (0, 0) resultList @?= Just "O"
        
    , testCase "Overlay sparse over dense - corner positions" $ do
        let dense = makeDense (3, 3) "O"
        let sparse = makeSparse mempty [((0, 0), "A"), ((2, 0), "B"), ((0, 2), "C"), ((2, 2), "D")]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just "A"
        lookup (2, 0) resultList @?= Just "B"
        lookup (0, 2) resultList @?= Just "C"
        lookup (2, 2) resultList @?= Just "D"
        lookup (1, 1) resultList @?= Just "O"
        
    , testCase "Overlay sparse over dense - overwrite same position" $ do
        let dense = makeDense (2, 2) "O"
        let sparse = makeSparse mempty [((0, 0), "X"), ((0, 0), "Y")]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just "Y"
        
    , testCase "Overlay sparse over dense - full coverage" $ do
        let dense = makeDense (2, 2) "O"
        let sparse = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        toList result @?= [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
        
    , testCase "Overlay sparse over dense - large grid with few sparse cells" $ do
        let dense = makeDense (10, 10) "Z"
        let sparse = makeSparse mempty [((5, 5), "C"), ((0, 9), "K")]
        let result = overlay dense sparse
        
        dimensions result @?= (10, 10)
        let resultList = toList result
        lookup (5, 5) resultList @?= Just "C"
        lookup (0, 9) resultList @?= Just "K"
        lookup (0, 0) resultList @?= Just "Z"
        length resultList @?= 100
        
    , testCase "Overlay sparse over dense - boundary positions" $ do
        let dense = makeDense (3, 3) "O"
        let sparse = makeSparse mempty 
              [ ((0, 0), "1"), ((1, 0), "2"), ((2, 0), "3")
              , ((0, 1), "4"), ((2, 1), "5")
              , ((0, 2), "6"), ((1, 2), "7"), ((2, 2), "8")
              ]
        let result = overlay dense sparse
        
        dimensions result @?= (3, 3)
        let resultList = toList result
        lookup (0, 0) resultList @?= Just "1"
        lookup (1, 0) resultList @?= Just "2"
        lookup (2, 0) resultList @?= Just "3"
        lookup (0, 1) resultList @?= Just "4"
        lookup (2, 1) resultList @?= Just "5"
        lookup (0, 2) resultList @?= Just "6"
        lookup (1, 2) resultList @?= Just "7"
        lookup (2, 2) resultList @?= Just "8"
        lookup (1, 1) resultList @?= Just "O"
        
    , testCase "Overlay sparse over dense - out of bounds cells ignored" $ do
        let dense = makeDense (2, 2) "D"
        let sparse = makeSparse mempty 
              [ ((0, 0), "A"), ((1, 1), "B")
              , ((2, 0), "X"), ((0, 2), "Y"), ((3, 3), "Z"), ((-1, 0), "W"), ((1, -1), "V")
              ]
        let result = overlay dense sparse
        
        dimensions result @?= (2, 2)
        let resultList = toList result
        
        lookup (0, 0) resultList @?= Just "A"
        lookup (1, 1) resultList @?= Just "B"
        
        lookup (1, 0) resultList @?= Just "D"
        lookup (0, 1) resultList @?= Just "D"
        
        length resultList @?= 4
        
        lookup (2, 0) resultList @?= Nothing
        lookup (0, 2) resultList @?= Nothing
        lookup (3, 3) resultList @?= Nothing
        lookup (-1, 0) resultList @?= Nothing
        lookup (1, -1) resultList @?= Nothing
    ]

  , testGroup "Grid overlap function"
    [ testCase "No overlap - completely separate grids" $ do
        let grid1 = makeSparse mempty [((0, 0), "A"), ((1, 0), "B")]
        let grid2 = makeSparse mempty [((2, 0), "X"), ((3, 0), "Y")]
        overlap grid1 grid2 @?= False
        overlap grid2 grid1 @?= False

    , testCase "No overlap - same coordinates but one is mempty" $ do
        let grid1 = makeSparse mempty [((0, 0), mempty), ((1, 0), mempty)]
        let grid2 = makeSparse mempty [((0, 0), "X"), ((1, 0), "Y")]
        overlap grid1 grid2 @?= False
        overlap grid2 grid1 @?= False

    , testCase "No overlap - both have mempty at same coordinates" $ do
        let grid1 = makeSparse mempty [((0, 0), mempty), ((1, 0), "A")]
        let grid2 = makeSparse mempty [((0, 0), mempty), ((2, 0), "B")]
        overlap grid1 grid2 @?= False

    , testCase "Overlap - same coordinates with non-mempty values" $ do
        let grid1 = makeSparse mempty [((0, 0), "A"), ((1, 0), "B")]
        let grid2 = makeSparse mempty [((0, 0), "X"), ((2, 0), "Y")]
        overlap grid1 grid2 @?= True
        overlap grid2 grid1 @?= True  -- Should be symmetric

    , testCase "Overlap - multiple overlapping coordinates" $ do
        let grid1 = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((2, 1), "C")]
        let grid2 = makeSparse mempty [((0, 0), "X"), ((1, 0), "Y"), ((3, 0), "Z")]
        overlap grid1 grid2 @?= True  -- Overlaps at (0,0) and (1,0)

    , testCase "Overlap - single overlapping coordinate among many" $ do
        let grid1 = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((2, 0), "C")]
        let grid2 = makeSparse mempty [((2, 0), "X"), ((3, 0), "Y"), ((4, 0), "Z")]
        overlap grid1 grid2 @?= True  -- Overlaps only at (2,0)

    , testCase "No overlap - empty grids" $ do
        let grid1 = makeSparse mempty ([] :: [(Coordinate, String)])
        let grid2 = makeSparse mempty ([] :: [(Coordinate, String)])
        overlap grid1 grid2 @?= False

    , testCase "No overlap - one empty grid" $ do
        let grid1 = makeSparse mempty [((0, 0), "A")]
        let grid2 = makeSparse mempty ([] :: [(Coordinate, String)])
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
        let sparseGrid = makeSparse mempty [((2, 0), "X"), ((0, 2), "Y")]
        overlap denseModified sparseGrid @?= False

    , testCase "Overlap with mixed dense/sparse - with overlap" $ do
        let denseGrid = makeDense (3, 3) mempty
        let denseModified = setAt (0, 0) "A" $ setAt (1, 1) "B" denseGrid
        let sparseGrid = makeSparse mempty [((0, 0), "X"), ((2, 2), "Y")]
        overlap denseModified sparseGrid @?= True

    , testCase "Overlap edge case - mempty vs non-mempty boundary" $ do
        let grid1 = makeSparse mempty [((0, 0), mempty), ((1, 0), "A"), ((2, 0), mempty)]
        let grid2 = makeSparse mempty [((0, 0), "X"), ((1, 0), mempty), ((2, 0), "Z")]
        overlap grid1 grid2 @?= False

    , testCase "Overlap with different value types - numeric mempty" $ do
        let grid1 = makeSparse mempty [((0, 0), 5), ((1, 0), 0), ((2, 0), 3)] :: Grid (Sum Int)
        let grid2 = makeSparse mempty [((0, 0), 0), ((1, 0), 7), ((2, 0), 2)] :: Grid (Sum Int)
        overlap grid1 grid2 @?= True

    , testCase "Overlap symmetry property" $ do
        let grid1 = makeSparse mempty [((0, 0), "A"), ((1, 1), "B")]
        let grid2 = makeSparse mempty [((0, 0), "X"), ((2, 2), "Y")]
        let grid3 = makeSparse mempty [((3, 3), "Z")]
        
        overlap grid1 grid2 @?= overlap grid2 grid1
        overlap grid1 grid3 @?= overlap grid3 grid1
        overlap grid2 grid3 @?= overlap grid3 grid2

    , testCase "Overlap with large coordinates" $ do
        let grid1 = makeSparse mempty [((1000, 2000), "A"), ((5000, 7000), "B")]
        let grid2 = makeSparse mempty [((1000, 2000), "X"), ((9000, 1000), "Y")]
        overlap grid1 grid2 @?= True
        
        let grid3 = makeSparse mempty [((1001, 2000), "Z")]
        overlap grid1 grid3 @?= False

    , testCase "Overlap with negative coordinates" $ do
        -- Test with negative coordinate values
        let grid1 = makeSparse mempty [((-1, -1), "A"), ((-5, -10), "B")]
        let grid2 = makeSparse mempty [((-1, -1), "X"), ((0, 0), "Y")]
        overlap grid1 grid2 @?= True
        
        let grid3 = makeSparse mempty [((-2, -1), "Z")]
        overlap grid1 grid3 @?= False
    ]

  , testGroup "isWithinBounds function"
    [ testCase "Piece completely within positive bounds" $ do
        let baseGrid = makeDense (5, 5) "O"
        let pieceGrid = makeSparse mempty [((1, 1), "X"), ((2, 2), "Y")]
        isWithinBounds baseGrid pieceGrid @?= True

    , testCase "Piece partially out of bounds - exceeds width" $ do
        let baseGrid = makeDense (3, 3) "O"
        let pieceGrid = makeSparse mempty [((1, 1), "X"), ((3, 1), "Y")]
        isWithinBounds baseGrid pieceGrid @?= False

    , testCase "Piece partially out of bounds - exceeds height" $ do
        let baseGrid = makeDense (3, 3) "O"
        let pieceGrid = makeSparse mempty [((1, 1), "X"), ((1, 3), "Y")]
        isWithinBounds baseGrid pieceGrid @?= False

    , testCase "Piece completely out of bounds - negative coordinates" $ do
        let baseGrid = makeDense (3, 3) "O"
        let pieceGrid = makeSparse mempty [((-1, 0), "X"), ((0, -1), "Y")]
        isWithinBounds baseGrid pieceGrid @?= False

    , testCase "Empty piece grid is always within bounds" $ do
        let baseGrid = makeDense (3, 3) "O"
        let pieceGrid = makeSparse mempty ([] :: [(Coordinate, String)])
        isWithinBounds baseGrid pieceGrid @?= True

    , testCase "Base grid with negative coordinates - piece within bounds" $ do
        let baseGrid = makeSparse mempty [((-2, -2), "O"), ((0, 0), "P"), ((1, 1), "Q")]
        let pieceGrid = makeSparse mempty [((-1, -1), "X"), ((0, 0), "Y")]
        isWithinBounds baseGrid pieceGrid @?= True

    , testCase "Base grid with negative coordinates - piece out of bounds" $ do
        let baseGrid = makeSparse mempty [((-2, -2), "O"), ((0, 0), "P"), ((1, 1), "Q")]
        let pieceGrid = makeSparse mempty [((-3, -1), "X"), ((0, 0), "Y")]
        isWithinBounds baseGrid pieceGrid @?= False

    , testCase "Mixed positive/negative coordinates" $ do
        let baseGrid = makeSparse mempty [((-1, -1), "A"), ((2, 2), "B")]
        let pieceGrid = makeSparse mempty [((0, 0), "X"), ((1, 1), "Y")]
        isWithinBounds baseGrid pieceGrid @?= True

    , testCase "Piece at exact boundaries - should be within bounds" $ do
        let baseGrid = makeSparse mempty [((-2, -2), "O"), ((2, 2), "P")]
        let pieceGrid = makeSparse mempty [((-2, -2), "X"), ((2, 2), "Y")]
        isWithinBounds baseGrid pieceGrid @?= True

    , testCase "Dense base grid vs sparse piece with negative coords" $ do
        -- Dense grids start at (0,0) and only have positive coordinates
        let baseGrid = makeDense (3, 3) "O"
        let pieceGrid = makeSparse mempty [((-1, 0), "X"), ((1, 1), "Y")]
        isWithinBounds baseGrid pieceGrid @?= False
    ]

  , testGroup "Grid rotation functions (square grids only)"
    [ testGroup "rotateClockwise"
      [ testCase "Empty grid rotation" $ do
          let grid = makeSparse mempty ([] :: [(Coordinate, String)])
          let rotated = rotateClockwise grid
          toList rotated @?= []

      , testCase "Single cell grid rotates around itself" $ do
          let grid = makeSparse mempty [((0, 0), "A")]
          let rotated = rotateClockwise grid
          toList rotated @?= [((0, 0), "A")]

      , testCase "2x2 square clockwise rotation" $ do
          -- Pattern: A B    ->   C A
          --          C D         D B
          let grid = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
          let rotated = rotateClockwise grid
          toList rotated @?= [((1, 0), "A"), ((1, 1), "B"), ((0, 0), "C"), ((0, 1), "D")]

      , testCase "3x3 square clockwise rotation (corners)" $ do
          -- Only test corners to keep it simple
          let grid = makeSparse mempty [((0, 0), "A"), ((2, 0), "C"), ((0, 2), "G"), ((2, 2), "I")]
          let rotated = rotateClockwise grid
          toList rotated @?= [((2, 0), "A"), ((2, 2), "C"), ((0, 0), "G"), ((0, 2), "I")]

      , testCase "3x3 square with center element" $ do
          let grid = makeSparse mempty [((0, 0), "A"), ((1, 1), "E"), ((2, 2), "I")]
          let rotated = rotateClockwise grid
          toList rotated @?= [((2, 0), "A"), ((1, 1), "E"), ((0, 2), "I")]
      ]

    , testGroup "rotateCounterClockwise"
      [ testCase "Empty grid rotation" $ do
          let grid = makeSparse mempty ([] :: [(Coordinate, String)])
          let rotated = rotateCounterClockwise grid
          toList rotated @?= []

      , testCase "Single cell grid rotates around itself" $ do
          let grid = makeSparse mempty [((0, 0), "A")]
          let rotated = rotateCounterClockwise grid
          toList rotated @?= [((0, 0), "A")]

      , testCase "2x2 square counter-clockwise rotation" $ do
          -- Pattern: A B    ->   B D
          --          C D         A C
          let grid = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
          let rotated = rotateCounterClockwise grid
          toList rotated @?= [((0, 1), "A"), ((0, 0), "B"), ((1, 1), "C"), ((1, 0), "D")]
      ]

    , testGroup "Rotation properties"
      [ testCase "Four clockwise rotations return to original (2x2)" $ do
          let original = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
          let rotated = rotateClockwise . rotateClockwise . rotateClockwise . rotateClockwise $ original
          toList rotated @?= toList original

      , testCase "Four counter-clockwise rotations return to original (2x2)" $ do
          let original = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
          let rotated = rotateCounterClockwise . rotateCounterClockwise . rotateCounterClockwise . rotateCounterClockwise $ original
          toList rotated @?= toList original

      , testCase "Clockwise then counter-clockwise returns to original (3x3)" $ do
          let original = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((2, 0), "C"),
                                           ((0, 1), "D"), ((1, 1), "E"), ((2, 1), "F"),
                                           ((0, 2), "G"), ((1, 2), "H"), ((2, 2), "I")]
          let rotated = rotateCounterClockwise . rotateClockwise $ original
          toList rotated @?= toList original

      , testCase "Counter-clockwise then clockwise returns to original (3x3)" $ do
          let original = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((2, 0), "C"),
                                           ((0, 1), "D"), ((1, 1), "E"), ((2, 1), "F"),
                                           ((0, 2), "G"), ((1, 2), "H"), ((2, 2), "I")]
          let rotated = rotateClockwise . rotateCounterClockwise $ original
          toList rotated @?= toList original

      , testCase "Three clockwise equals one counter-clockwise (2x2)" $ do
          let original = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
          let clockwise3 = rotateClockwise . rotateClockwise . rotateClockwise $ original
          let counterClockwise1 = rotateCounterClockwise original
          toList clockwise3 @?= toList counterClockwise1

      , testCase "Three counter-clockwise equals one clockwise (2x2)" $ do
          let original = makeSparse mempty [((0, 0), "A"), ((1, 0), "B"), ((0, 1), "C"), ((1, 1), "D")]
          let counterClockwise3 = rotateCounterClockwise . rotateCounterClockwise . rotateCounterClockwise $ original
          let clockwise1 = rotateClockwise original
          toList counterClockwise3 @?= toList clockwise1
      ]
    ]

  , testGroup "Line Clearing Tests"
    [ testCase "No complete lines - grid unchanged" $ do
        let grid = makeDense (3, 3) Empty
            result = clearLines grid
        result @?= grid

    , testCase "One complete line - middle row cleared" $ do
        let grid = makeDense (3, 3) Empty
            -- Fill middle row completely
            gridWithLine = foldr (\x acc -> setAt (x, 1) Garbage acc) grid [0, 1, 2]
            result = clearLines gridWithLine
            expectedGrid = makeDense (3, 3) Empty
        result @?= expectedGrid

    , testCase "One complete line - bottom row cleared" $ do
        let grid = makeDense (3, 3) Empty
            -- Fill bottom row completely
            gridWithLine = foldr (\x acc -> setAt (x, 2) Garbage acc) grid [0, 1, 2]
            result = clearLines gridWithLine
            expectedGrid = makeDense (3, 3) Empty
        result @?= expectedGrid

    , testCase "Multiple complete lines cleared" $ do
        let grid = makeDense (3, 4) Empty
            -- Fill rows 1 and 3 completely
            gridWithLines = foldr (\x acc -> 
                                    setAt (x, 1) Garbage $ 
                                    setAt (x, 3) Garbage acc) grid [0, 1, 2]
            result = clearLines gridWithLines
            expectedGrid = makeDense (3, 4) Empty
        result @?= expectedGrid

    , testCase "Complex line clearing scenario" $ do
        let grid = makeDense (4, 5) Empty
            -- Place cells in various positions
            gridWithCells = setAt (0, 0) Garbage $ 
                           setAt (1, 1) Garbage $ 
                           setAt (2, 2) Garbage grid
            -- Fill row 3 completely  
            gridWithLine = foldr (\x acc -> setAt (x, 3) Garbage acc) gridWithCells [0, 1, 2, 3]
            result = clearLines gridWithLine
            -- After clearing row 3, cells should shift down:
            -- (0,0) -> (0,1), (1,1) -> (1,2), (2,2) -> (2,3)
            expectedGrid = setAt (0, 1) Garbage $ 
                          setAt (1, 2) Garbage $ 
                          setAt (2, 3) Garbage $ 
                          makeDense (4, 5) Empty
        result @?= expectedGrid

    , testCase "Tetris scenario - four lines cleared simultaneously" $ do
        let grid = makeDense (10, 6) Empty
            -- Fill bottom four rows completely
            gridWithLines = foldr (\(x, y) acc -> setAt (x, y) Garbage acc) 
                                  grid 
                                  [(x, y) | x <- [0..9], y <- [2, 3, 4, 5]]
            -- Add some cells in top rows
            gridWithTopCells = setAt (3, 0) Garbage $ 
                              setAt (4, 0) Garbage $ 
                              setAt (5, 1) Garbage gridWithLines
            result = clearLines gridWithTopCells
            -- After clearing 4 lines, top cells should move down
            expectedGrid = setAt (3, 4) Garbage $ 
                          setAt (4, 4) Garbage $ 
                          setAt (5, 5) Garbage $ 
                          makeDense (10, 6) Empty
        result @?= expectedGrid
    ]

  ]
import qualified Data.HashMap.Strict as HashMap
import Tetrafall.Types
import Tetrafall.Types.Grid
import qualified Data.Vector as V
-- Use project definitions

-- Debug visualization functions
formatCellForDebug :: Cell -> Char
formatCellForDebug Empty = '.'
formatCellForDebug (TetrominoCell I) = 'I'
formatCellForDebug (TetrominoCell T) = 'T'
formatCellForDebug (TetrominoCell S) = 'S'
formatCellForDebug (TetrominoCell Z) = 'Z'
formatCellForDebug (TetrominoCell J) = 'J'
formatCellForDebug (TetrominoCell L) = 'L'
formatCellForDebug (TetrominoCell O) = 'O'
formatCellForDebug Garbage = '#'

visualizeGrid :: Grid Cell -> String
visualizeGrid grid =
    let coords = toList grid
        ((minX, minY), (maxX, maxY)) = extent grid
        
        coordMap = HashMap.fromList coords
        
        rows = [minY..maxY]
        cols = [minX..maxX]
        
        gridLines = map (\y -> map (\x -> 
            formatCellForDebug $ HashMap.lookupDefault Empty (x, y) coordMap) cols) rows
        
    in unlines gridLines

getRotatedGrid :: Grid Cell -> Orientation -> Grid Cell
getRotatedGrid baseGrid North = baseGrid
getRotatedGrid baseGrid East = rotateClockwise baseGrid
getRotatedGrid baseGrid South = rotateClockwise (rotateClockwise baseGrid)
getRotatedGrid baseGrid West = rotateCounterClockwise baseGrid

debugTetromino :: TetrominoType -> IO ()
debugTetromino pieceType = do
    putStrLn $ "=== " ++ show pieceType ++ " PIECE ==="
    case HashMap.lookup pieceType defaultTetrominoMap of
        Nothing -> putStrLn "ERROR: Piece not found!"
        Just baseGrid -> do
            let orientations = [North, East, South, West]
            mapM_ (\orientation -> do
                putStrLn $ show orientation ++ ":"
                let rotatedGrid = getRotatedGrid baseGrid orientation
                    coords = filter (\(_, cell) -> cell /= Empty) (toList rotatedGrid)
                putStr $ visualizeGrid rotatedGrid
                putStrLn ""
                ) orientations
    putStrLn ""

main :: IO ()
main = do
    putStrLn "TETROMINO ROTATION DEBUG"
    putStrLn "========================"
    putStrLn ""
    
    let allPieces = [S, Z, J, L, O, I, T]
    mapM_ debugTetromino allPieces
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, makeDense, overlap, isWithinBounds, emptyGrid, overlay, clearLines, toSparse, double)
import qualified Tetrafall.KeyboardConfig as KeyConfig

import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import System.Random (mkStdGen)

import Lens.Micro.Platform

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter, vCenter)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

getFinalGrid :: Game -> Grid Cell
getFinalGrid game =
    let baseGrid = game ^. grid
        pieceGrid = fromMaybe (emptyGrid Empty) $ do
          piece <- game ^. currentPiece
          return (getTetrominoGrid piece)
    in baseGrid `overlay` pieceGrid

playfield :: Game -> Widget ()
playfield game =
    let
      g = getFinalGrid game
      s = game ^. score
    in

    hCenter $
    vCenter $
    (border $
    foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (toVector (double g)))) <+> ( border $ hLimit 6 $ padLeft Max $ str (show s))

formatCell :: Cell -> Widget ()
formatCell Empty = str " "
formatCell _ = str "█"

-- Debug functions for logging
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

debugGridToString :: Grid Cell -> String
debugGridToString g =
    let gridVector = toVector g
        rows = V.toList $ V.map (V.toList . V.map formatCellForDebug) gridVector
        rowStrings = map (++ "\n") rows
    in concat rowStrings

debugPieceInfo :: Maybe Tetromino -> String
debugPieceInfo Nothing = "No current piece"
debugPieceInfo (Just piece) = 
    let pieceGrid = getTetrominoGrid piece
        gridCoords = map fst (toSparse pieceGrid)
    in "Piece: " ++ show (piece ^. tetrominoType) ++ 
       " at " ++ show (piece ^. position) ++ 
       " facing " ++ show (piece ^. orientation) ++
       " grid coords: " ++ show gridCoords

step :: Game -> Game
step game = 
    let newGame = over score ((+) 1) game
    in case newGame ^. currentPiece of
        Nothing -> newGame
        Just piece -> 
            let movedPiece = over position (\(x, y) -> (x, y + 1)) piece
                movedPieceGrid = getTetrominoGrid movedPiece
                baseGrid = newGame ^. grid
                canFall = not (overlap baseGrid movedPieceGrid) && isWithinBounds baseGrid movedPieceGrid
            in if canFall
               then -- Piece can fall normally, reset slide state
                   newGame & currentPiece .~ Just movedPiece & slideState .~ CanFall
               else -- Piece cannot fall, check slide state
                   case newGame ^. slideState of
                       CanFall -> 
                           -- First time piece can't fall, enter sliding state
                           newGame & slideState .~ Sliding (piece ^. position)
                       Sliding originalPos -> 
                           -- Check if piece has moved since sliding started
                           if piece ^. position == originalPos
                           then -- Piece hasn't moved, lock it down
                               let currentPieceGrid = getTetrominoGrid piece
                                   gridWithPiece = baseGrid `overlay` currentPieceGrid
                                   newGrid = clearLines gridWithPiece
                                   (newPiece, newRng) = randomTetromino (newGame ^. rng)
                               in newGame & grid .~ newGrid 
                                         & currentPiece .~ Just newPiece
                                         & slideState .~ CanFall
                                         & rng .~ newRng
                           else -- Piece has moved, continue sliding with new position
                               newGame & slideState .~ Sliding (piece ^. position)
                       ShouldLock -> 
                           let currentPieceGrid = getTetrominoGrid piece
                               gridWithPiece = baseGrid `overlay` currentPieceGrid
                               newGrid = clearLines gridWithPiece
                               (newPiece, newRng) = randomTetromino (newGame ^. rng)
                           in newGame & grid .~ newGrid 
                                     & currentPiece .~ Just newPiece
                                     & slideState .~ CanFall
                                     & rng .~ newRng

appEvent :: BrickEvent () Tick -> EventM () Game ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent (VtyEvent (V.EvKey key [])) = do
    case KeyConfig.getActionForKey KeyConfig.defaultConfig key of
        Just action -> modify (apply action)
        Nothing -> return ()
appEvent (AppEvent Tick) = do
    game <- get
    let pieceInfo = debugPieceInfo (game ^. currentPiece)
        finalGrid = getFinalGrid game
        gridString = debugGridToString finalGrid
        debugInfo = "=== TICK ===\n" ++ 
                   pieceInfo ++ "\n" ++
                   "Score: " ++ show (game ^. score) ++ "\n" ++
                   "Slide State: " ++ show (game ^. slideState) ++ "\n" ++
                   "Grid:\n" ++ gridString ++ "\n"
    liftIO $ appendFile "tetrafall.log" debugInfo
    modify step
    return ()

appEvent _ = return ()

redAttr :: Int -> AttrName
redAttr i = attrName ("red-" <> show i)

attributes :: AttrMap
attributes =  attrMap (V.green `on` V.black) $
    map (\i -> (redAttr i, bg (V.rgbColor i 0 0))) [0..255]

app :: App Game Tick ()
app =
    App { appDraw = \s -> [playfield s]
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const attributes
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    chan <- newBChan 10
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 1000000 -- decides how fast your game moves
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app defaultGame

defaultGame :: Game
defaultGame = Game
  { _grid =  makeDense (10, 22) Empty
  , _score = 0
  , _currentPiece = Just tetrominoI
  , _slideState = CanFall
  , _rng = mkStdGen 42  -- Fixed seed for reproducible testing, could be randomized
  }
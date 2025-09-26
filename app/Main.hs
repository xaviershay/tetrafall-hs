{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, makeDense, toList, makeSparse, overlap, isWithinBounds)

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

import Lens.Micro.Platform

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter, vCenter)
import Control.Monad (forever, void)

import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

translateTetromino :: Tetromino -> Grid Cell -> Grid Cell
translateTetromino tetromino shape =
    let (px, py) = tetromino ^. position
        cells = toList shape
        translatedCells = map (\((x, y), cell) -> ((x + px, y + py), cell)) cells
    in makeSparse translatedCells

getFinalGrid :: Game -> Grid Cell
getFinalGrid game =
    let baseGrid = game ^. grid
        pieceGrid = fromMaybe mempty $ do
          piece <- game ^. currentPiece
          shape <- HM.lookup (piece ^. tetrominoType) defaultTetrominoMap
          return (translateTetromino piece shape)
    in baseGrid <> pieceGrid

playfield :: Game -> Widget ()
playfield game =
    let
      g = getFinalGrid game
      s = game ^. score
    in

    hCenter $
    vCenter $
    (border $
    foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (toVector (g)))) <+> ( border $ hLimit 6 $ padLeft Max $ str (show s))

formatCell :: Cell -> Widget ()
formatCell Empty = str " "
formatCell _ = str "█"

step :: Game -> Game
step game = 
    let newGame = over score ((+) 1) game
    in case newGame ^. currentPiece of
        Nothing -> newGame
        Just piece -> 
            let movedPiece = over position (\(x, y) -> (x, y + 1)) piece
                movedPieceGrid = fromMaybe mempty $ do
                    shape <- HM.lookup (movedPiece ^. tetrominoType) defaultTetrominoMap
                    return (translateTetromino movedPiece shape)
                baseGrid = newGame ^. grid
            in if overlap baseGrid movedPieceGrid || not (isWithinBounds baseGrid movedPieceGrid)
               then -- Place piece and reset
                   let currentPieceGrid = fromMaybe mempty $ do
                           shape <- HM.lookup (piece ^. tetrominoType) defaultTetrominoMap
                           return (translateTetromino piece shape)
                       newGrid = baseGrid <> currentPieceGrid
                   in newGame & grid .~ newGrid & currentPiece .~ Just tetrominoI
               else -- Move piece
                   newGame & currentPiece .~ Just movedPiece

appEvent :: BrickEvent () Tick -> EventM () Game ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent (AppEvent Tick) = do
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
        threadDelay 100000 -- decides how fast your game moves
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app defaultGame

defaultGame :: Game
defaultGame = Game
  { _grid =  makeDense (10, 22) Empty
  , _score = 0
  , _currentPiece = Just tetrominoI
  }
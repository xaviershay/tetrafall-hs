{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, makeDense, overlap, isWithinBounds, emptyGrid, overlay)
import qualified Tetrafall.KeyboardConfig as KeyConfig

import qualified Data.Vector as V
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
                                   newGrid = baseGrid `overlay` currentPieceGrid
                               in newGame & grid .~ newGrid 
                                         & currentPiece .~ Just tetrominoI 
                                         & slideState .~ CanFall
                           else -- Piece has moved, continue sliding with new position
                               newGame & slideState .~ Sliding (piece ^. position)

appEvent :: BrickEvent () Tick -> EventM () Game ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent (VtyEvent (V.EvKey key [])) = do
    case KeyConfig.getActionForKey KeyConfig.defaultConfig key of
        Just action -> modify (apply action)
        Nothing -> return ()
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
  , _slideState = CanFall
  }
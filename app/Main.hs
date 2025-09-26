{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, double, makeDense)

import qualified Data.Vector as V

import Lens.Micro.Platform

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Core (padLeft, Padding(Max))
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter, vCenter)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (MonadState, modify, get)
import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

playfield :: Game -> Widget ()
playfield game =
    let
      g = view grid game
      s = view score game
    in

    hCenter $
    vCenter $
    (border $
    foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (toVector (double g)))) <+> ( border $ hLimit 6 $ padLeft Max $ str (show s))

formatCell Empty = str " "
formatCell _ = str "█"

step :: Game -> Game
step = over score ((+) 1)

appEvent :: BrickEvent () Tick -> EventM () Game ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent (AppEvent Tick) = do
    x <- get
    put x
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
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100000 -- decides how fast your game moves
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app defaultGame

defaultGame = Game
  { _grid = setAt (5, 5) Garbage $ setAt (5, 10) (TetrominoCell T) $ setAt (6, 10) (TetrominoCell T) $ makeDense (10, 22) Empty
  , _score = 0
  , _currentPiece = Just tetrominoI
  }
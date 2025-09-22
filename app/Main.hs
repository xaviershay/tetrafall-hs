{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Tetrafall.Types

import qualified Data.Vector as V

import Lens.Micro.Platform

import Control.Monad (void)

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter, vCenter)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

data AppState = AppState
  { _grid :: Grid
  }
makeLenses ''AppState

--ui :: Widget ()
--ui =
--    border $
--    hLimit 10 $
--    vLimit 22 $
--    (foldl (<+>) (str "") (map (\i -> withDefAttr (redAttr (i * 2)) $ str (show i)) [0..10]))

playfield :: Grid -> Widget ()
playfield grid =
    hCenter $
    vCenter $
    (border $
    --hLimit 10 $
    --vLimit 22 $
    foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (view contents (doubleGrid grid)))) <+> (border $ str "12345")

formatCell Empty = str " "
formatCell _ = str "█"

doubleGrid :: Grid ->  Grid
doubleGrid grid = Grid
  { _dimensions = (w * 2, h * 2)
  , _contents = V.fromList $ concatMap doubleRow (V.toList originalContents)
  }
  where
    (w, h) = view dimensions grid
    originalContents = view contents grid
    doubleRow row = [doubledRow, doubledRow]
      where doubledRow = V.fromList $ concatMap (\cell -> [cell, cell]) (V.toList row)

appEvent :: BrickEvent () e -> EventM () AppState ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent _ = return ()

redAttr :: Int -> AttrName
redAttr i = attrName ("red-" <> show i)

attributes :: AttrMap
attributes =  attrMap (V.green `on` V.black) $
    map (\i -> (redAttr i, bg (V.rgbColor i 0 0))) [0..255]

app :: App AppState e ()
app =
    App { appDraw = \s -> [playfield (_grid s)]
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const attributes
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    void $ customMain initialVty buildVty Nothing app defaultAppState

defaultAppState = AppState
  { _grid = setAt (5, 5) Garbage $ setAt (5, 10) (TetrominoCell T) $ setAt (6, 10) (TetrominoCell T) $ mkGrid 10 22
  }
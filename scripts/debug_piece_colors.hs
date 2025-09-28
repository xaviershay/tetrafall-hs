#!/usr/bin/env -S stack runghc --package brick --package vty --package microlens-platform --package vector --package unordered-containers --package tetrafall-hs

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, overlay, emptyGrid, double, makeDense)
import Tetrafall.Game (getTetrominoGrid)

import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Control.Monad (void)

import Lens.Micro.Platform

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)

import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

-- Color attribute names (same as in Main.hs)
sBlockAttr :: AttrName
sBlockAttr = attrName "s-block"

zBlockAttr :: AttrName
zBlockAttr = attrName "z-block"

iBlockAttr :: AttrName
iBlockAttr = attrName "i-block"

oBlockAttr :: AttrName
oBlockAttr = attrName "o-block"

lBlockAttr :: AttrName
lBlockAttr = attrName "l-block"

jBlockAttr :: AttrName
jBlockAttr = attrName "j-block"

tBlockAttr :: AttrName
tBlockAttr = attrName "t-block"

borderAttr :: AttrName
borderAttr = attrName "border"

-- Color attributes matching Main.hs
attributes :: AttrMap
attributes = attrMap (V.white `on` V.black) $
    [ (borderAttr, fg (V.rgbColor 180 180 180))  -- dull white
    -- Tetromino colors
    , (sBlockAttr, fg (V.rgbColor 0 255 0))      -- green
    , (zBlockAttr, fg (V.rgbColor 255 0 0))      -- red
    , (iBlockAttr, fg (V.rgbColor 173 216 230))  -- light blue
    , (oBlockAttr, fg (V.rgbColor 255 255 0))    -- yellow
    , (lBlockAttr, fg (V.rgbColor 255 165 0))    -- orange
    , (jBlockAttr, fg (V.rgbColor 0 0 255))      -- blue
    , (tBlockAttr, fg (V.rgbColor 128 0 128))    -- purple
    ]

formatCell :: Cell -> Widget ()
formatCell Empty = str " "
formatCell (TetrominoCell S) = withDefAttr sBlockAttr $ str "█"
formatCell (TetrominoCell Z) = withDefAttr zBlockAttr $ str "█"
formatCell (TetrominoCell I) = withDefAttr iBlockAttr $ str "█"
formatCell (TetrominoCell O) = withDefAttr oBlockAttr $ str "█"
formatCell (TetrominoCell L) = withDefAttr lBlockAttr $ str "█"
formatCell (TetrominoCell J) = withDefAttr jBlockAttr $ str "█"
formatCell (TetrominoCell T) = withDefAttr tBlockAttr $ str "█"
formatCell Garbage = str "█"

-- Create a tetromino at a specific position
createTetromino :: TetrominoType -> Coordinate -> Tetromino
createTetromino pieceType pos = Tetromino
    { _tetrominoType = pieceType
    , _position = pos
    , _orientation = North
    }

-- Render a single piece in a small grid
renderPiece :: TetrominoType -> Widget ()
renderPiece pieceType =
    let piece = createTetromino pieceType (2, 2)  -- Center the piece in a small grid
        pieceGrid = getTetrominoGrid piece
        -- Create a base grid large enough to contain the translated piece
        baseGrid = makeDense (6, 6) Empty  -- 6x6 grid should be large enough
        finalGrid = baseGrid `overlay` pieceGrid
        colorName = case pieceType of
            S -> "Green"
            Z -> "Red" 
            I -> "Light Blue"
            O -> "Yellow"
            L -> "Orange"
            J -> "Blue"
            T -> "Purple"
    in withDefAttr borderAttr $ 
       borderWithLabel (str $ show pieceType ++ " - " ++ colorName) $
       hLimit 14 $ vLimit 8 $
       foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (toVector finalGrid))

-- Main UI
ui :: Widget ()
ui = 
    vCenter $ hCenter $
    vBox [ str "Tetrafall - All Tetromino Pieces with RGB Colors"
         , str " "
         , hBox [ renderPiece S, str "  ", renderPiece Z, str "  ", renderPiece I ]
         , str " "
         , hBox [ renderPiece O, str "  ", renderPiece L, str "  ", renderPiece J ]
         , str " "
         , hBox [ renderPiece T ]
         , str " "
         , str "Press 'q' or ESC to quit"
         ]

appEvent :: BrickEvent () e -> EventM () () ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent _ = return ()

app :: App () e ()
app = App 
    { appDraw = const [ui]
    , appHandleEvent = appEvent
    , appStartEvent = return ()
    , appAttrMap = const attributes
    , appChooseCursor = neverShowCursor
    }

main :: IO ()
main = do
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    void $ customMain initialVty buildVty Nothing app ()
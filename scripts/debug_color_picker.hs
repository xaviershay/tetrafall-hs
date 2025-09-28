#!/usr/bin/env stack runghc

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.List (intercalate)
import Control.Concurrent (forkIO, threadDelay)

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

-- Custom event type
data CustomEvent = AutoQuit deriving Show

-- Application state
data ColorPicker = ColorPicker
    { selectedGrid :: Int  -- Which of the 6 grids is selected (0-5)
    , selectedPos :: (Int, Int)  -- Position within the selected grid (0-5, 0-5)
    } deriving Show

initialState :: ColorPicker
initialState = ColorPicker 0 (0, 0)

-- Grid size
gridSize :: Int
gridSize = 6

-- Unicode block character
blockChar :: String
blockChar = "█"

-- Generate RGB color for a specific grid and position
getRGBColor :: Int -> Int -> Int -> V.Color
getRGBColor gridIndex row col =
    let redValue = row * 51    -- Red varies 0-255 (6 steps * 51 ≈ 255)
        blueValue = col * 51   -- Blue varies 0-255 (6 steps * 51 ≈ 255)
        greenValue = gridIndex * 51  -- Green varies per grid: 0, 51, 102, 153, 204, 255
    in V.rgbColor redValue greenValue blueValue

-- Get grid title
getGridTitle :: Int -> String
getGridTitle gridIndex =
    let greenValue = gridIndex * 51
    in "Red-Blue (G=" ++ show greenValue ++ ")"

-- Create a single color cell
colorCell :: Int -> Int -> Int -> Bool -> Widget ()
colorCell gridIndex row col isSelected =
    let color = getRGBColor gridIndex row col
        cellAttr = attrName $ "grid-" ++ show gridIndex ++ "-" ++ show row ++ "-" ++ show col
        widget = withAttr cellAttr $ str blockChar
    in if isSelected
        then withAttr selectionAttr $ str "[" <+> widget <+> str "]"
        else str " " <+> widget <+> str " "

-- Create a single 6x6 grid
createGrid :: ColorPicker -> Int -> Widget ()
createGrid picker gridIndex =
    let isSelectedGrid = selectedGrid picker == gridIndex
        (selRow, selCol) = selectedPos picker
        title = str $ getGridTitle gridIndex
        grid = vBox $ map (\row ->
                    hBox $ map (\col ->
                        let isSelected = isSelectedGrid && row == selRow && col == selCol
                        in colorCell gridIndex row col isSelected
                    ) [0..gridSize-1]
                ) [0..gridSize-1]
    in vBox [title, grid]

-- Main UI
ui :: ColorPicker -> [Widget ()]
ui picker =
    let grids = map (createGrid picker) [0..5]
        topRow = hBox [grids !! 0, str "  ", grids !! 1, str "  ", grids !! 2]
        bottomRow = hBox [grids !! 3, str "  ", grids !! 4, str "  ", grids !! 5]
        instructions = vBox
            [ str ""
            , str "Color Picker - RGB Colorspace Explorer"
            , str "======================================"
            , str ""
            , str "Controls:"
            , str "  Arrow Keys: Move within grid"
            , str "  Tab/Shift+Tab: Switch between grids"
            , str "  Space: Show current color info"
            , str "  q/Esc: Quit"
            , str ""
            , str $ "Current Grid: " ++ show (selectedGrid picker) ++ " - " ++ getGridTitle (selectedGrid picker)
            , str $ "Position: " ++ show (selectedPos picker)
            , str $ "RGB Value: " ++ show (getCurrentRGB picker)
            ]
    in [vBox [topRow, str "", bottomRow, str "", instructions]]

-- Get current RGB values
getCurrentRGB :: ColorPicker -> (Int, Int, Int)
getCurrentRGB picker =
    let gridIndex = selectedGrid picker
        (row, col) = selectedPos picker
        redValue = row * 51
        blueValue = col * 51
        greenValue = gridIndex * 51
    in (redValue, greenValue, blueValue)

-- Event handling
handleEvent :: BrickEvent () CustomEvent -> EventM () ColorPicker ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (AppEvent AutoQuit) = halt
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = modify $ \s -> s { selectedGrid = (selectedGrid s - 1) `mod` 6 }
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = modify $ \s -> s { selectedGrid = (selectedGrid s + 1) `mod` 6 }
handleEvent (VtyEvent (V.EvKey V.KUp [])) = modify $ \s ->
    let (row, col) = selectedPos s
    in s { selectedPos = (max 0 (row - 1), col) }
handleEvent (VtyEvent (V.EvKey V.KDown [])) = modify $ \s ->
    let (row, col) = selectedPos s
    in s { selectedPos = (min (gridSize - 1) (row + 1), col) }
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = modify $ \s ->
    let (row, col) = selectedPos s
    in s { selectedPos = (row, max 0 (col - 1)) }
handleEvent (VtyEvent (V.EvKey V.KRight [])) = modify $ \s ->
    let (row, col) = selectedPos s
    in s { selectedPos = (row, min (gridSize - 1) (col + 1)) }
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
    picker <- get
    let (r, g, b) = getCurrentRGB picker
    -- Could show a popup with detailed color info here
    return ()
handleEvent _ = return ()

-- Attribute names
selectionAttr :: AttrName
selectionAttr = attrName "selection"

-- Generate all color attributes
generateColorAttrs :: [(AttrName, V.Attr)]
generateColorAttrs =
    [ (attrName $ "grid-" ++ show gridIndex ++ "-" ++ show row ++ "-" ++ show col,
       fg (getRGBColor gridIndex row col))
    | gridIndex <- [0..5]
    , row <- [0..gridSize-1]
    , col <- [0..gridSize-1]
    ]

-- Attribute map
attributes :: AttrMap
attributes = attrMap (bg (V.rgbColor 0 0 0)) $
    (selectionAttr, fg V.yellow `V.withStyle` V.bold) : generateColorAttrs

-- Brick app definition
app :: App ColorPicker CustomEvent ()
app = App
    { appDraw = ui
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const attributes
    }

main :: IO ()
main = do
    eventChan <- newBChan 10
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    
    -- Start a thread that will send an AutoQuit event after a short delay
    _ <- forkIO $ do
        threadDelay 500000000
        writeBChan eventChan AutoQuit
    
    void $ customMain initialVty buildVty (Just eventChan) app initialState
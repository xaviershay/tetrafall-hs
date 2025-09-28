#!/usr/bin/env -S stack runghc --package brick --package vty --package microlens-platform --package vector --package unordered-containers --package tetrafall-hs

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, overlay, emptyGrid, double, makeDense)
import Tetrafall.Game (getTetrominoGrid)

import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Control.Monad (void)
import Data.List (intercalate)
import Control.Monad.IO.Class (liftIO)

import Lens.Micro.Platform

import Brick
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)

import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

-- Application state
data AppState = AppState
    { selectedTetromino :: TetrominoType
    , selectedGrid :: Int  -- Which of the 6 color grids is selected (0-5)
    , selectedPos :: (Int, Int)  -- Position within the selected grid (0-5, 0-5)
    , tetrominoColors :: HM.HashMap TetrominoType (Int, Int, Int)  -- RGB colors for each tetromino
    } deriving Show

-- Initial tetromino colors (matching current colors)
initialColors :: HM.HashMap TetrominoType (Int, Int, Int)
initialColors = HM.fromList
    [ (S, (0, 255, 0))      -- green
    , (Z, (255, 0, 0))      -- red
    , (I, (173, 216, 230))  -- light blue
    , (O, (255, 255, 0))    -- yellow
    , (L, (255, 165, 0))    -- orange
    , (J, (0, 0, 255))      -- blue
    , (T, (128, 0, 128))    -- purple
    ]

initialState :: AppState
initialState = 
    let baseState = AppState
            { selectedTetromino = S
            , selectedGrid = 0
            , selectedPos = (0, 0)
            , tetrominoColors = initialColors
            }
    in updateSelectionToMatchColor baseState

-- Grid size for color picker
gridSize :: Int
gridSize = 6

-- Unicode block character
blockChar :: String
blockChar = "█"

-- Color attribute names
sBlockAttr, zBlockAttr, iBlockAttr, oBlockAttr, lBlockAttr, jBlockAttr, tBlockAttr, borderAttr, selectionAttr :: AttrName
sBlockAttr = attrName "s-block"
zBlockAttr = attrName "z-block"
iBlockAttr = attrName "i-block"
oBlockAttr = attrName "o-block"
lBlockAttr = attrName "l-block"
jBlockAttr = attrName "j-block"
tBlockAttr = attrName "t-block"
borderAttr = attrName "border"
selectionAttr = attrName "selection"

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

-- Create a single color cell for the color picker
colorCell :: Int -> Int -> Int -> Bool -> Widget ()
colorCell gridIndex row col isSelected =
    let color = getRGBColor gridIndex row col
        cellAttr = attrName $ "grid-" ++ show gridIndex ++ "-" ++ show row ++ "-" ++ show col
        widget = withAttr cellAttr $ str blockChar
    in if isSelected
        then withAttr selectionAttr $ str "[" <+> widget <+> str "]"
        else str " " <+> widget <+> str " "

-- Create a single 6x6 color grid
createColorGrid :: AppState -> Int -> Widget ()
createColorGrid state gridIndex =
    let isSelectedGrid = selectedGrid state == gridIndex
        (selRow, selCol) = selectedPos state
        title = str $ getGridTitle gridIndex
        grid = vBox $ map (\row ->
                    hBox $ map (\col ->
                        let isSelected = isSelectedGrid && row == selRow && col == selCol
                        in colorCell gridIndex row col isSelected
                    ) [0..gridSize-1]
                ) [0..gridSize-1]
    in vBox [title, grid]

-- Get current RGB values based on selection
getCurrentRGB :: AppState -> (Int, Int, Int)
getCurrentRGB state =
    let gridIndex = selectedGrid state
        (row, col) = selectedPos state
        redValue = row * 51
        blueValue = col * 51
        greenValue = gridIndex * 51
    in (redValue, greenValue, blueValue)

-- Convert RGB color back to grid position and selection
rgbToGridPosition :: (Int, Int, Int) -> (Int, (Int, Int))
rgbToGridPosition (r, g, b) =
    let gridIndex = min 5 (g `div` 51)  -- Green determines grid (0-5)
        row = min 5 (r `div` 51)        -- Red determines row (0-5)
        col = min 5 (b `div` 51)        -- Blue determines col (0-5)
    in (gridIndex, (row, col))

-- Update state to match current tetromino's color
updateSelectionToMatchColor :: AppState -> AppState
updateSelectionToMatchColor state =
    let currentColor = HM.lookupDefault (255, 255, 255) (selectedTetromino state) (tetrominoColors state)
        (newGrid, newPos) = rgbToGridPosition currentColor
    in state { selectedGrid = newGrid, selectedPos = newPos }

-- Get attribute for a tetromino type
getTetrominoAttr :: TetrominoType -> AttrName
getTetrominoAttr S = sBlockAttr
getTetrominoAttr Z = zBlockAttr
getTetrominoAttr I = iBlockAttr
getTetrominoAttr O = oBlockAttr
getTetrominoAttr L = lBlockAttr
getTetrominoAttr J = jBlockAttr
getTetrominoAttr T = tBlockAttr

formatCell :: Cell -> Widget ()
formatCell Empty = str " "
formatCell (TetrominoCell pieceType) = withDefAttr (getTetrominoAttr pieceType) $ str "█"
formatCell Garbage = str "█"

-- Create a tetromino at a specific position
createTetromino :: TetrominoType -> Coordinate -> Tetromino
createTetromino pieceType pos = Tetromino
    { _tetrominoType = pieceType
    , _position = pos
    , _orientation = North
    }

-- Render a single piece in a small grid
renderPiece :: AppState -> TetrominoType -> Widget ()
renderPiece state pieceType =
    let piece = createTetromino pieceType (2, 2)  -- Center the piece in a small grid
        pieceGrid = getTetrominoGrid piece
        -- Create a base grid large enough to contain the translated piece
        baseGrid = makeDense (6, 6) Empty  -- 6x6 grid should be large enough
        finalGrid = baseGrid `overlay` pieceGrid
        (r, g, b) = HM.lookupDefault (255, 255, 255) pieceType (tetrominoColors state)
        colorName = "RGB(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
        isSelected = selectedTetromino state == pieceType
        borderLabel = if isSelected 
                     then str $ ">>> " ++ show pieceType ++ " - " ++ colorName ++ " <<<"
                     else str $ show pieceType ++ " - " ++ colorName
    in withDefAttr borderAttr $ 
       borderWithLabel borderLabel $
       hLimit 20 $ vLimit 8 $
       foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (toVector finalGrid))

-- Main UI
ui :: AppState -> [Widget ()]
ui state =
    let colorGrids = map (createColorGrid state) [0..5]
        topColorRow = hBox [colorGrids !! 0, str "  ", colorGrids !! 1, str "  ", colorGrids !! 2]
        bottomColorRow = hBox [colorGrids !! 3, str "  ", colorGrids !! 4, str "  ", colorGrids !! 5]
        
        pieceGrids = [ renderPiece state S, str "  ", renderPiece state Z, str "  ", renderPiece state I
                     , str " "
                     , renderPiece state O, str "  ", renderPiece state L, str "  ", renderPiece state J
                     , str " "
                     , renderPiece state T
                     ]
        
        currentRGB = getCurrentRGB state
        instructions = vBox
            [ str ""
            , str "Tetrafall - Piece Color Editor"
            , str "============================="
            , str ""
            , str "Controls:"
            , str "  s,z,i,o,l,j,t: Select tetromino type"
            , str "  Arrow Keys: Move within color picker"
            , str "  Tab/Shift+Tab: Switch between color grids"
            , str "  q/Esc: Quit"
            , str ""
            , str $ "Selected Tetromino: " ++ show (selectedTetromino state)
            , str $ "Current Grid: " ++ show (selectedGrid state) ++ " - " ++ getGridTitle (selectedGrid state)
            , str $ "Position: " ++ show (selectedPos state)
            , str $ "RGB Value: " ++ show currentRGB
            ]
    in [vBox [ str "Color Picker:"
             , topColorRow
             , str ""
             , bottomColorRow
             , str ""
             , str "Tetromino Pieces:"
             , vBox pieceGrids
             , instructions
             ]]

-- Write colors to log file
writeColorsToLog :: AppState -> IO ()
writeColorsToLog state = do
    let colorLines = map (\pieceType ->
            let (r, g, b) = HM.lookupDefault (255, 255, 255) pieceType (tetrominoColors state)
            in show pieceType ++ ": RGB(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
          ) [S, Z, I, O, L, J, T]
        content = unlines colorLines
    writeFile "colors.log" content

-- Event handling
handleEvent :: BrickEvent () e -> EventM () AppState ()
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt

-- Tetromino selection keys
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = do
    state <- get
    let newState = updateSelectionToMatchColor $ state { selectedTetromino = S }
    put newState
handleEvent (VtyEvent (V.EvKey (V.KChar 'z') [])) = do
    state <- get
    let newState = updateSelectionToMatchColor $ state { selectedTetromino = Z }
    put newState
handleEvent (VtyEvent (V.EvKey (V.KChar 'i') [])) = do
    state <- get
    let newState = updateSelectionToMatchColor $ state { selectedTetromino = I }
    put newState
handleEvent (VtyEvent (V.EvKey (V.KChar 'o') [])) = do
    state <- get
    let newState = updateSelectionToMatchColor $ state { selectedTetromino = O }
    put newState
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = do
    state <- get
    let newState = updateSelectionToMatchColor $ state { selectedTetromino = L }
    put newState
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = do
    state <- get
    let newState = updateSelectionToMatchColor $ state { selectedTetromino = J }
    put newState
handleEvent (VtyEvent (V.EvKey (V.KChar 't') [])) = do
    state <- get
    let newState = updateSelectionToMatchColor $ state { selectedTetromino = T }
    put newState

-- Color picker navigation
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = do
    state <- get
    let newState = state { selectedGrid = (selectedGrid state - 1) `mod` 6 }
        newColors = HM.insert (selectedTetromino newState) (getCurrentRGB newState) (tetrominoColors newState)
        finalState = newState { tetrominoColors = newColors }
    liftIO $ writeColorsToLog finalState
    put finalState
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
    state <- get
    let newState = state { selectedGrid = (selectedGrid state + 1) `mod` 6 }
        newColors = HM.insert (selectedTetromino newState) (getCurrentRGB newState) (tetrominoColors newState)
        finalState = newState { tetrominoColors = newColors }
    liftIO $ writeColorsToLog finalState
    put finalState
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    state <- get
    let (row, col) = selectedPos state
        newState = state { selectedPos = (max 0 (row - 1), col) }
        newColors = HM.insert (selectedTetromino newState) (getCurrentRGB newState) (tetrominoColors newState)
        finalState = newState { tetrominoColors = newColors }
    liftIO $ writeColorsToLog finalState
    put finalState
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    state <- get
    let (row, col) = selectedPos state
        newState = state { selectedPos = (min (gridSize - 1) (row + 1), col) }
        newColors = HM.insert (selectedTetromino newState) (getCurrentRGB newState) (tetrominoColors newState)
        finalState = newState { tetrominoColors = newColors }
    liftIO $ writeColorsToLog finalState
    put finalState
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
    state <- get
    let (row, col) = selectedPos state
        newState = state { selectedPos = (row, max 0 (col - 1)) }
        newColors = HM.insert (selectedTetromino newState) (getCurrentRGB newState) (tetrominoColors newState)
        finalState = newState { tetrominoColors = newColors }
    liftIO $ writeColorsToLog finalState
    put finalState
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
    state <- get
    let (row, col) = selectedPos state
        newState = state { selectedPos = (row, min (gridSize - 1) (col + 1)) }
        newColors = HM.insert (selectedTetromino newState) (getCurrentRGB newState) (tetrominoColors newState)
        finalState = newState { tetrominoColors = newColors }
    liftIO $ writeColorsToLog finalState
    put finalState

handleEvent _ = return ()

-- Generate all color attributes for the color picker
generateColorAttrs :: [(AttrName, V.Attr)]
generateColorAttrs =
    [ (attrName $ "grid-" ++ show gridIndex ++ "-" ++ show row ++ "-" ++ show col,
       fg (getRGBColor gridIndex row col))
    | gridIndex <- [0..5]
    , row <- [0..gridSize-1]
    , col <- [0..gridSize-1]
    ]

-- Dynamic attribute map that updates based on current colors
dynamicAttributes :: AppState -> AttrMap
dynamicAttributes state = 
    let tetrominoAttrs = map (\pieceType ->
            let (r, g, b) = HM.lookupDefault (255, 255, 255) pieceType (tetrominoColors state)
            in (getTetrominoAttr pieceType, fg (V.rgbColor r g b))
          ) [S, Z, I, O, L, J, T]
        baseAttrs = [ (borderAttr, fg (V.rgbColor 180 180 180))  -- dull white
                    , (selectionAttr, fg V.yellow `V.withStyle` V.bold)
                    ]
    in attrMap (bg (V.rgbColor 0 0 0)) $ baseAttrs ++ tetrominoAttrs ++ generateColorAttrs

app :: App AppState e ()
app = App 
    { appDraw = ui
    , appHandleEvent = handleEvent
    , appStartEvent = do
        state <- get
        liftIO $ writeColorsToLog state
        return ()
    , appAttrMap = dynamicAttributes
    , appChooseCursor = neverShowCursor
    }

main :: IO ()
main = do
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    void $ customMain initialVty buildVty Nothing app initialState
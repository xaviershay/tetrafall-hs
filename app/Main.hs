{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, overlap, isWithinBounds, emptyGrid, overlay, clearLines, toSparse, double)
import Tetrafall.Game (step, apply, getTetrominoGrid)
import qualified Tetrafall.KeyboardConfig as KeyConfig

import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Lens.Micro.Platform

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter, hCenterLayer, vCenter, vCenterLayer)

import qualified Brick.Animation as A
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

data CustomEvent =
      Tick
    | AnimationUpdate (EventM () St ())

data St = St
  { _stAnimationManager :: A.AnimationManager St CustomEvent ()
  , _stGame :: Game
  , _particleAnimations :: M.Map Location (A.Animation St ())
  }
makeLenses ''St

startParticleAnimation :: Location -> EventM () St ()
startParticleAnimation coord = do
    mgr <- use stAnimationManager
    a <- use (particleAnimations . at coord)
    case a of
        Just {} -> return () -- Animation already running at this location
        Nothing -> A.startAnimation mgr clip1 100 A.Once (particleAnimations . at coord)


getFinalGrid :: Game -> Grid Cell
getFinalGrid game =
    let baseGrid = game ^. grid
        pieceGrid = fromMaybe (emptyGrid Empty) $ do
          piece <- game ^. currentPiece
          return (getTetrominoGrid piece)
    in baseGrid `overlay` pieceGrid

-- Create individual layers for each particle (one layer per particle)
drawParticleLayerList :: St -> [Widget ()]
drawParticleLayerList st = map (drawParticleAnimation st) (M.toList (st ^. particleAnimations))

backgroundLayer :: St -> Widget ()
backgroundLayer st = hCenter $ vCenter $ str " "

-- Foreground layer with the playfield and score
playfieldLayer :: St -> Widget ()
playfieldLayer st =
    let
      game = st ^. stGame
      g = getFinalGrid game
      s = game ^. score
      (w, h) = game ^. windowSize
    in
    hCenterLayer $
    vCenterLayer $
    (border $
    foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (toVector (double g)))) <+> ( border $ hLimit 15 $ vBox [padLeft Max $ str (show s)])



drawParticleAnimation :: St -> (Location, A.Animation St ()) -> Widget ()
drawParticleAnimation st (location, animation) =
    translateBy location $ A.renderAnimation (const $ str "*") st (Just animation)

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

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent (AppEvent (AnimationUpdate act)) = act
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent (VtyEvent (V.EvResize width height)) = do
    modify (stGame . windowSize .~ (width, height))
appEvent (VtyEvent (V.EvKey key [])) = do
    case KeyConfig.getActionForKey KeyConfig.defaultConfig key of
        Just action -> modify (stGame %~ apply action)
        Nothing -> return ()
appEvent (AppEvent Tick) = do
    st <- get
    let game = st ^. stGame
        pieceInfo = debugPieceInfo (game ^. currentPiece)
        finalGrid = getFinalGrid game
        gridString = debugGridToString finalGrid
        debugInfo = "=== TICK ===\n" ++ 
                   pieceInfo ++ "\n" ++
                   "Score: " ++ show (game ^. score) ++ "\n" ++
                   "Slide State: " ++ show (game ^. slideState) ++ "\n" ++
                   "Grid:\n" ++ gridString ++ "\n"
    liftIO $ appendFile "tetrafall.log" debugInfo
    modify (stGame %~ step)
    
    -- Start animations for all particles
    newGame <- gets (^. stGame)
    let locations = map (\p -> let (x, y) = p ^. particleLocation in Location (round x, round y)) (newGame ^. particles)
    mapM_ startParticleAnimation locations
    return ()

appEvent _ = return ()

redAttr :: Int -> AttrName
redAttr i = attrName ("red-" <> show i)

attributes :: AttrMap
attributes =  attrMap (V.green `on` V.black) $
    map (\i -> (redAttr i, bg (V.rgbColor i 0 0))) [0..255]

app :: App St CustomEvent ()
app =
    App { appDraw = \s -> 
            [ playfieldLayer s  -- Foreground layer (playfield on top)
            ] ++ drawParticleLayerList s  -- Background layers (one per particle)
            ++ [backgroundLayer s]
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
    
    -- Get initial window size
    let output = V.outputIface initialVty
    (width, height) <- V.displayBounds output
    
    mgr <- A.startAnimationManager 50 chan AnimationUpdate

    let gameWithWindowSize = defaultGame & windowSize .~ (width, height)
        defaultState = St
          { _stAnimationManager = mgr
          , _stGame = gameWithWindowSize
          , _particleAnimations = M.empty
          }

    void $ customMain initialVty buildVty (Just chan) app defaultState 

clip1 :: A.Clip a ()
clip1 =
    A.newClip_
    [ str "0"
    , str "O"
    , str "o"
    , str "*"
    , str "~"
    , str "."
    ]


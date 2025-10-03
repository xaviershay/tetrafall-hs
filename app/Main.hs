{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Tetrafall.Types
import Tetrafall.Types.Grid (toVector, emptyGrid, overlay, toList, double, makeDense, crop)
import Tetrafall.Game (apply, getTetrominoGrid, defaultGame)
import Tetrafall.Animation
import qualified Tetrafall.KeyboardConfig as KeyConfig

import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.IORef (newIORef, readIORef, writeIORef)
import Text.Printf (printf)

import Lens.Micro.Platform

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (hCenter, hCenterLayer, vCenter, vCenterLayer)

import qualified Brick.Animation as A
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

data CustomEvent =
      Tick NominalDiffTime
    | AnimationUpdate (EventM () St ())

data St = St
  { _stAnimationManager :: A.AnimationManager St CustomEvent ()
  , _stGame :: Game
  , _particleAnimations :: M.Map Location (A.Animation St ())
  , _stIntermediaryScores :: [Int]  -- List of scores to animate through
  }
makeLenses ''St

-- Get the score currently being displayed (head of intermediary list or game score)
getDisplayedScore :: St -> Int
getDisplayedScore st = 
    case st ^. stIntermediaryScores of
        (x:_) -> x
        [] -> st ^. stGame . score

-- Check if score changed and generate intermediary scores if needed
checkAndGenerateScoreAnimation :: Int -> EventM () St ()
checkAndGenerateScoreAnimation oldScore = do
    st <- get
    let newScore = st ^. stGame . score
    when (oldScore /= newScore) $ do
        let newScores = generateIntermediaryScores oldScore newScore
        modify (stIntermediaryScores .~ newScores)

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

renderNextPiece :: Game -> Widget ()
renderNextPiece game =
    case game ^. gameNextPieces of
        [] -> withDefAttr borderAttr $ border $ hLimit 10 $ vLimit 2 $ fill ' '
        (nextType:_) -> 
            let nextPiece = Tetromino nextType (2, 1) North
                nextGrid = getTetrominoGrid nextPiece
                previewGrid = double $ makeDense (5, 2) Empty `overlay` nextGrid
                finalPreviewGrid = if ((fst . dimensions) nextGrid `mod` 2 == 0) then crop ((1, 0), (9, 1)) previewGrid else previewGrid
                previewWidget = vBox $ V.toList $ V.map (\row -> 
                    hBox $ V.toList $ V.map formatCell row
                    ) (toVector finalPreviewGrid)
            in withDefAttr borderAttr $ borderWithLabel (str "Next") $ hLimit 10 $ vLimit 2 $ 
               vCenter $ hCenter $ previewWidget

drawParticleLayerList :: St -> [Widget ()]
drawParticleLayerList st = map (drawParticleAnimation st) (M.toList (st ^. particleAnimations))

backgroundLayer :: St -> Widget ()
backgroundLayer _ = hCenter $ vCenter $ str " "

playfieldLayer :: St -> Widget ()
playfieldLayer st =
    let
      game = st ^. stGame
      g = getFinalGrid game
      s = getDisplayedScore st
      
      -- Main playfield
      playfield = withDefAttr borderAttr $ border $
                  foldl (<=>) (str "") (V.map (\row -> foldl (<+>) (str "") (V.map formatCell row)) (toVector (double g)))
      
      -- Right sidebar with next piece, score, and time
      timeSeconds = realToFrac (game ^. gameTime) :: Double
      timeStr = printf "%.2f" timeSeconds
      sidebar = vBox 
                [ renderNextPiece game
                , withDefAttr borderAttr $ borderWithLabel (str "Score") $ 
                  hLimit 10 $ padLeft Max $ withDefAttr scoringTextAttr $ str (show s)
                , withDefAttr borderAttr $ borderWithLabel (str "Time") $
                  hLimit 10 $ padLeft Max $ withDefAttr scoringTextAttr $ str timeStr
                ]
    in
    hCenterLayer $
    vCenterLayer $
    playfield <+> str " " <+> sidebar



drawParticleAnimation :: St -> (Location, A.Animation St ()) -> Widget ()
drawParticleAnimation st (location, animation) =
    translateBy location $ A.renderAnimation (const $ withDefAttr particleAttr $ str "*") st (Just animation)

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
        gridCoords = map fst (toList pieceGrid)
    in "Piece: " ++ show (piece ^. tetrominoType) ++ 
       " at " ++ show (piece ^. position) ++ 
       " facing " ++ show (piece ^. orientation) ++
       " grid coords: " ++ show gridCoords

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent (AppEvent (AnimationUpdate act)) = do
    act
    -- Handle score animation - just pop the next score from the list
    st <- get
    let intermediaryScores = st ^. stIntermediaryScores
        
    case intermediaryScores of
        (_:xs) -> modify (stIntermediaryScores .~ xs)
        [] -> return ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent (VtyEvent (V.EvResize width height)) = do
    modify (stGame . windowSize .~ (width, height))
appEvent (VtyEvent (V.EvKey key [])) = do
    case KeyConfig.getActionForKey KeyConfig.defaultConfig key of
        Just action -> do
            st <-  get
            let oldScore = st ^. stGame ^. score
            modify (stGame %~ apply action)
            checkAndGenerateScoreAnimation oldScore
        Nothing -> return ()
appEvent (AppEvent (Tick dt)) = do
    st <- get
    let oldScore = st ^. stGame ^. score
    modify (stGame %~ apply (ActionTick dt))
    checkAndGenerateScoreAnimation oldScore
    newGame <- gets (^. stGame)
    let locations = map (\p -> let (x, y) = p ^. particleLocation in Location (round x, round y)) (newGame ^. particles)
    mapM_ startParticleAnimation locations

appEvent _ = return ()

-- Color attribute names
backgroundAttr :: AttrName
backgroundAttr = attrName "background"

particleAttr :: AttrName
particleAttr = attrName "particle"

borderAttr :: AttrName
borderAttr = attrName "border"

scoringTextAttr :: AttrName
scoringTextAttr = attrName "scoring-text"

-- Tetromino color attributes
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

redAttr :: Int -> AttrName
redAttr i = attrName ("red-" <> show i)

attributes :: AttrMap
attributes = attrMap (V.white `on` rgb 0 0 0) $
    [ (backgroundAttr, bg (rgb 0 0 0))
    , (particleAttr, fg V.white)
    , (borderAttr, fg (rgb 180 180 180))  -- dull white
    , (scoringTextAttr, fg V.white)
    -- Tetromino colors
    , (sBlockAttr, fg (rgb 0 153 0))
    , (zBlockAttr, fg (rgb 204 0 0))
    , (iBlockAttr, fg (rgb 0 255 204))
    , (oBlockAttr, fg (rgb 255 204 0))
    , (lBlockAttr, fg (rgb 255 102 51))
    , (jBlockAttr, fg (rgb 51 0 255))
    , (tBlockAttr, fg (rgb 204 0 204))
    ] ++ map (\i -> (redAttr i, bg (rgb i 0 0))) [0..255]
    where
        rgb :: Int -> Int -> Int -> V.Color
        rgb = V.rgbColor

app :: App St CustomEvent ()
app =
    App { appDraw = \s -> 
            [ playfieldLayer s
            ] ++ drawParticleLayerList s
            ++ [backgroundLayer s]
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const attributes
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    chan <- newBChan 10
    lastTimeRef <- newIORef =<< getCurrentTime
    _ <- forkIO $ forever $ do
        let tickIntervalMs = 1 * 1000
        threadDelay tickIntervalMs
        currentTime <- getCurrentTime
        lastTime <- readIORef lastTimeRef
        let elapsed = diffUTCTime currentTime lastTime
        writeIORef lastTimeRef currentTime
        writeBChan chan (Tick elapsed)
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
          , _stIntermediaryScores = []
          }

    void $ customMain initialVty buildVty (Just chan) app defaultState 

clip1 :: A.Clip a ()
clip1 =
    A.newClip_
    [ withDefAttr particleAttr $ str "0"
    , withDefAttr particleAttr $ str "O"
    , withDefAttr particleAttr $ str "o"
    , withDefAttr particleAttr $ str "*"
    , withDefAttr particleAttr $ str "~"
    , withDefAttr particleAttr $ str "."
    ]


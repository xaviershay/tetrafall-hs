module Main (main) where

import Control.Monad (void)

import Brick -- (Widget, simpleMain, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import qualified Graphics.Vty as V
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Config.ColorMode (ColorMode(..))

ui :: Widget ()
ui =
    joinBorders $
    withBorderStyle unicode $
    foldl (<+>) (str "") (map (\i -> withDefAttr (redAttr (i * 2)) $ str " ") [0..100])
    --str "" <+> (withDefAttr (redAttr 100) $ str " ") <+> str " "

appEvent :: BrickEvent () e -> EventM () Int ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent _ = return ()

testColorAttr :: AttrName
testColorAttr = attrName "testColor"

redAttr :: Int -> AttrName
redAttr i = attrName ("red-" <> show i)

attributes :: AttrMap
attributes =  attrMap (V.green `on` V.black) $
    map (\i -> (redAttr i, bg (V.rgbColor i 0 0))) [0..255]

app :: App Int e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const attributes
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = do
    let buildVty = do
            config <- defaultConfig
            V.mkVty $ config { configPreferredColorMode = FullColor }
    void $ customMain buildVty Nothing app 1
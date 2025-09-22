module ColorTest (main) where

import Control.Monad (void)

import Brick
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (VtyUserConfig(..), defaultConfig)
import Graphics.Vty.Attributes.Color (ColorMode(..))

ui :: Widget ()
ui =
    foldl (<+>) (str "") (map (\i -> withDefAttr (redAttr (i * 2)) $ str " ") [0..100])

appEvent :: BrickEvent () e -> EventM () Int ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent _ = return ()

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
    let buildVty = mkVty $ defaultConfig { configPreferredColorMode = Just FullColor }
    initialVty <- buildVty
    void $ customMain initialVty buildVty Nothing app 1
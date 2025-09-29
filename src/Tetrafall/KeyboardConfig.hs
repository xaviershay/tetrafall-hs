module Tetrafall.KeyboardConfig
  ( KeyboardConfig(..)
  , qwertyConfig
  , colemakConfig
  , defaultConfig
  , getActionForKey
  ) where

import Tetrafall.Types (Action(..))
import qualified Graphics.Vty as V
import Data.Maybe (listToMaybe, mapMaybe)

data KeyboardConfig = KeyboardConfig
  { leftKeys :: [V.Key]
  , rightKeys :: [V.Key]
  , rotateCWKeys :: [V.Key]
  , rotateCCWKeys :: [V.Key]
  , softDropKeys :: [V.Key]
  , hardDropKeys :: [V.Key]
  } deriving (Show, Eq)

qwertyConfig :: KeyboardConfig
qwertyConfig = KeyboardConfig
  { leftKeys = [V.KLeft, V.KChar 'a']
  , rightKeys = [V.KRight, V.KChar 'd']
  , rotateCWKeys = [V.KUp]
  , rotateCCWKeys = [V.KChar 'w']
  , softDropKeys = [V.KDown, V.KChar 's']
  , hardDropKeys = [V.KChar ' ']
  }

colemakConfig :: KeyboardConfig
colemakConfig = KeyboardConfig
  { leftKeys = [V.KLeft, V.KChar 'a']
  , rightKeys = [V.KRight, V.KChar 's']
  , rotateCWKeys = [V.KUp]
  , rotateCCWKeys = [V.KChar 'w']
  , softDropKeys = [V.KDown, V.KChar 'r']
  , hardDropKeys = [V.KChar ' ']
  }

defaultConfig :: KeyboardConfig
defaultConfig = colemakConfig

getActionForKey :: KeyboardConfig -> V.Key -> Maybe Action
getActionForKey config key = 
  listToMaybe $ mapMaybe (\(keys, action) -> 
    if key `elem` keys config then Just action else Nothing) $
  [ (leftKeys, ActionLeft)
  , (rightKeys, ActionRight) 
  , (rotateCWKeys, ActionRotateCW)
  , (rotateCCWKeys, ActionRotateCCW)
  , (softDropKeys, ActionSoftDrop)
  , (hardDropKeys, ActionHardDrop)
  ]
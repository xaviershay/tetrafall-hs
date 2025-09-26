module Tetrafall.KeyboardConfig
  ( KeyboardConfig(..)
  , qwertyConfig
  , colemakConfig
  , defaultConfig
  , getActionForKey
  ) where

import Tetrafall.Types (Action(..))
import qualified Graphics.Vty as V

data KeyboardConfig = KeyboardConfig
  { leftKeys :: [V.Key]
  , rightKeys :: [V.Key]
  , rotateCWKeys :: [V.Key]
  , rotateCCWKeys :: [V.Key]
  } deriving (Show, Eq)

-- | QWERTY keyboard layout configuration
qwertyConfig :: KeyboardConfig
qwertyConfig = KeyboardConfig
  { leftKeys = [V.KLeft, V.KChar 'a']
  , rightKeys = [V.KRight, V.KChar 'd']
  , rotateCWKeys = [V.KChar 'w']
  , rotateCCWKeys = [V.KUp]
  }

colemakConfig :: KeyboardConfig
colemakConfig = KeyboardConfig
  { leftKeys = [V.KLeft, V.KChar 'a']
  , rightKeys = [V.KRight, V.KChar 's']
  , rotateCWKeys = [V.KChar 'w']
  , rotateCCWKeys = [V.KUp]
  }

defaultConfig :: KeyboardConfig
defaultConfig = colemakConfig

getActionForKey :: KeyboardConfig -> V.Key -> Maybe Action
getActionForKey config key
  | key `elem` leftKeys config = Just ActionLeft
  | key `elem` rightKeys config = Just ActionRight
  | key `elem` rotateCWKeys config = Just ActionRotateCW
  | key `elem` rotateCCWKeys config = Just ActionRotateCCW
  | otherwise = Nothing
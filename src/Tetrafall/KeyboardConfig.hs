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
  } deriving (Show, Eq)

-- | QWERTY keyboard layout configuration
qwertyConfig :: KeyboardConfig
qwertyConfig = KeyboardConfig
  { leftKeys = [V.KLeft, V.KChar 'a']
  , rightKeys = [V.KRight, V.KChar 'd']
  }

colemakConfig :: KeyboardConfig
colemakConfig = KeyboardConfig
  { leftKeys = [V.KLeft, V.KChar 'a']
  , rightKeys = [V.KRight, V.KChar 's']
  }

defaultConfig :: KeyboardConfig
defaultConfig = colemakConfig

getActionForKey :: KeyboardConfig -> V.Key -> Maybe Action
getActionForKey config key
  | key `elem` leftKeys config = Just ActionLeft
  | key `elem` rightKeys config = Just ActionRight
  | otherwise = Nothing
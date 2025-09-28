module Tetrafall.Scoring (calculateScore) where

import Tetrafall.Types

calculateScore :: ScoreEvent -> Int
calculateScore scoreEvent = 
  let linesCleared = _scoreLines scoreEvent
      level = _scoreLevel scoreEvent
  in case linesCleared of
       1 -> 100 * level
       2 -> 300 * level
       3 -> 500 * level
       4 -> 800 * level
       _ -> error "invalid number of lines cleared"
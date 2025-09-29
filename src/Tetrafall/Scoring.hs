module Tetrafall.Scoring (simple) where

import Tetrafall.Types

simple :: ScoreEvent -> Int
simple (ScoreEvent linesCleared level) = 
  maybe (error "invalid number of lines cleared") (* level) $
  lookup linesCleared [(1, 100), (2, 300), (3, 500), (4, 800)]
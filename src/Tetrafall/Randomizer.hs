module Tetrafall.Randomizer (Randomizer, og1985) where

import Tetrafall.Types
import System.Random (randomR)

og1985 :: Randomizer
og1985 = uniform

uniform :: Randomizer
uniform env = 
  let allTypes = [S, Z, J, L, O, I, T]
      (index, newGen) = randomR (0, length allTypes - 1) (_randomizerEnvRng env)
      selectedType = allTypes !! index
  in (selectedType, env { _randomizerEnvRng = newGen })
module Tetrafall.Randomizer (Randomizer, og1985) where

import Tetrafall.Types
import System.Random (randomR)

og1985 :: Randomizer
og1985 = uniform

nes :: Randomizer
nes = withHistory 1 (retryN 1 (forbidRecent 1) uniform)

uniform :: Randomizer
uniform env = 
  let allTypes = [S, Z, J, L, O, I, T]
      (index, newGen) = randomR (0, length allTypes - 1) (_randomizerEnvRng env)
      selectedType = allTypes !! index
  in (selectedType, env { _randomizerEnvRng = newGen })

-- Select a piece from the randomizer, then return it as well as adding it to
-- the environment's history. Only keeps N recent pieces.
withHistory :: Int -> Randomizer -> Randomizer
withHistory = undefined

-- take n from history and return true if none of them match the given type
forbidRecent :: Int -> (RandomizerEnv -> TetrominoType -> Bool)
forbidRecent n env tetrominoType = 
  let recentPieces = take n (_randomizerEnvHistory env)
  in tetrominoType `notElem` recentPieces

-- Select from the underlying randomizer up to N times, retrying if the selected
-- type does not cause the provided function to return True. After N
-- unsuccessful tries, default to the underlying randomizer with no checking.
retryN :: Int -> (RandomizerEnv -> TetrominoType -> Bool) -> Randomizer -> Randomizer
retryN maxTries validator baseRandomizer env = 
  let tryGenerate tries currentEnv
        | tries >= maxTries = baseRandomizer currentEnv
        | otherwise = 
            let (tetrominoType, newEnv) = baseRandomizer currentEnv
            in if validator currentEnv tetrominoType
               then (tetrominoType, newEnv)
               else tryGenerate (tries + 1) newEnv
  in tryGenerate 0 env
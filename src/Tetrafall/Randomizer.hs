module Tetrafall.Randomizer (Randomizer, og1985, nes, tetrisWorlds, tgm, tgm2, tgma, tgm3) where

import Tetrafall.Types
import System.Random (StdGen, randomR)
import Data.HashMap.Strict (empty, insert, lookup)
import Prelude hiding (lookup)

type Predicate = RandomizerEnv -> TetrominoType -> Bool

build :: StdGen -> Randomizer -> RandomizerEnv
build rng f = RandomizerEnv rng [] 0 [] f 0 empty

og1985 :: StdGen -> RandomizerEnv
og1985 rng = build rng uniform

nes :: StdGen -> RandomizerEnv
nes rng = build rng $ withHistory 1 (retryN 1 (forbidRecent 1) uniform)

tetrisWorlds :: StdGen -> RandomizerEnv
tetrisWorlds rng = build rng $ refillOnEmpty 7 bag

-- TODO: Should use intiial history of z,z,z,z
tgm :: StdGen -> RandomizerEnv
tgm rng = build rng $ withHistory 4 (retryForever forbidInitialOverhang (retryN 4 (forbidRecent 4) uniform))

-- TODO: Should use intiial history of s,z,s,z
tgm2 :: StdGen -> RandomizerEnv
tgm2 rng = build rng $ withHistory 4 (retryForever forbidInitialOverhang (retryN 6 (forbidRecent 4) uniform))

-- TODO: Should use intiial history of s,z,s,z
tgma :: StdGen -> RandomizerEnv
tgma rng = build rng $ (retryForever forbidInitialOverhang (refillOnEmpty 7 bag))

-- TODO: Should use intiial history of s,z,s,z
tgm3 :: StdGen -> RandomizerEnv
tgm3 rng = build rng $ withHistory 4 (retryForever forbidInitialOverhang (retryN 6 (forbidRecent 4) (refillOnEmpty 35 (refillLongestUnseen bag))))

uniform :: Randomizer
uniform env = 
  let allTypes = [S, Z, J, L, O, I, T]
      (index, newGen) = randomR (0, length allTypes - 1) (_randomizerEnvRng env)
      selectedType = allTypes !! index
  in (selectedType, env { _randomizerEnvRng = newGen })

bag :: Randomizer
bag env = 
  case _randomizerBag env of
    [] -> error "Bag is empty - should be refilled before calling bag"
    bagPieces -> 
      let (index, newGen) = randomR (0, length bagPieces - 1) (_randomizerEnvRng env)
          selectedPiece = bagPieces !! index
          remainingBag = take index bagPieces ++ drop (index + 1) bagPieces
      in (selectedPiece, env { _randomizerEnvRng = newGen, _randomizerBag = remainingBag })

-- Place the selected piece back in the bag
refillOnSelect :: Randomizer -> Randomizer
refillOnSelect baseRandomizer env =
  let (selectedType, newEnv) = baseRandomizer env
      updatedBag = selectedType : _randomizerBag newEnv
  in (selectedType, newEnv { _randomizerBag = updatedBag })

refillOnEmpty :: Int -> Randomizer -> Randomizer
refillOnEmpty n baseRandomizer env =
  let refillBag = take n $ cycle [S, Z, J, L, O, I, T]
      
      newEnv = if null (_randomizerBag env)
               then env { _randomizerBag = refillBag }
               else env
  in baseRandomizer newEnv

refillLongestUnseen :: Randomizer -> Randomizer
refillLongestUnseen baseRandomizer env =
  let newEnv = if _randomizerCount env == 0
               then env  -- Don't refill on first piece
               else
                 let allTypes = [S, Z, J, L, O, I, T]
                     sinceLast = _randomizerSinceLast env
                     -- Get count since last seen for each type, defaulting to very high number for unseen types
                     typesWithCounts = map (\t -> (t, maybe maxBound id (lookup t sinceLast))) allTypes
                     -- Find the maximum count (longest since last seen)
                     maxCount = maximum (map snd typesWithCounts)
                     -- Get all types that have this maximum count
                     longestUnseenTypes = map fst (filter ((== maxCount) . snd) typesWithCounts)
                     -- If there's a tie, we need to pick randomly
                     (index, newRng) = randomR (0, length longestUnseenTypes - 1) (_randomizerEnvRng env)
                     droughtPiece = longestUnseenTypes !! index
                     updatedBag = droughtPiece : _randomizerBag env
                 in env { _randomizerBag = updatedBag, _randomizerEnvRng = newRng }
  in baseRandomizer newEnv

-- Select a piece from the randomizer, then return it as well as adding it to
-- the environment's history. Only keeps N recent pieces.
withHistory :: Int -> Randomizer -> Randomizer
withHistory n baseRandomizer env =
  let (selectedType, newEnv) = baseRandomizer env
      updatedHistory = take n (selectedType : _randomizerEnvHistory newEnv)
      incrementedCount = _randomizerCount newEnv + 1
      -- Update the sinceLast map: reset selected type to 0, increment all others
      allTypes = [S, Z, J, L, O, I, T]
      oldSinceLast = _randomizerSinceLast newEnv
      updatedSinceLast = foldr (\t acc -> 
        if t == selectedType 
        then insert t 0 acc
        else insert t (maybe maxBound (+1) (lookup t acc)) acc
        ) oldSinceLast allTypes
  in (selectedType, newEnv { _randomizerEnvHistory = updatedHistory, _randomizerCount = incrementedCount, _randomizerSinceLast = updatedSinceLast })

-- AND conjuction of predicates
andPredicate :: Predicate -> Predicate -> Predicate
andPredicate pred1 pred2 env tetrominoType = pred1 env tetrominoType && pred2 env tetrominoType

-- If this is the first piece (count == 0), forbid if the given type.
forbidInitial :: TetrominoType -> Predicate
forbidInitial forbiddenType env tetrominoType = 
  _randomizerCount env /= 0 || tetrominoType /= forbiddenType

forbidInitialOverhang :: Predicate
forbidInitialOverhang = forbidInitial I `andPredicate` forbidInitial S `andPredicate` forbidInitial Z

-- take n from history and return true if none of them match the given type
forbidRecent :: Int -> Predicate
forbidRecent n env tetrominoType = 
  let recentPieces = take n (_randomizerEnvHistory env)
  in tetrominoType `notElem` recentPieces

-- Select from the underlying randomizer up to N times, retrying if the selected
-- type does not cause the provided function to return True. After N
-- unsuccessful tries, default to the underlying randomizer with no checking.
retryN :: Int -> Predicate -> Randomizer -> Randomizer
retryN maxTries validator baseRandomizer env = 
  let tryGenerate tries currentEnv
        | tries >= maxTries = baseRandomizer currentEnv
        | otherwise = 
            let (tetrominoType, newEnv) = baseRandomizer currentEnv
            in if validator currentEnv tetrominoType
               then (tetrominoType, newEnv)
               else tryGenerate (tries + 1) newEnv
  in tryGenerate 0 env

-- Select from randomizer repeatedly until predicate passes
retryForever :: Predicate -> Randomizer -> Randomizer
retryForever validator baseRandomizer env = 
  let (tetrominoType, newEnv) = baseRandomizer env
  in if validator env tetrominoType
     then (tetrominoType, newEnv)
     else retryForever validator baseRandomizer newEnv
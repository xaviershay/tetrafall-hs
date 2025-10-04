module Tetrafall.Game (defaultGame, apply, getTetrominoGrid, spawnMultipleFireworks) where

import Tetrafall.Types
import Tetrafall.Types.Grid
import Tetrafall.Particle.Physics
import Tetrafall.Particle.Firework
import qualified Tetrafall.Randomizer
import qualified Tetrafall.Scoring
import Lens.Micro.Platform

import Data.Time.Clock (NominalDiffTime)
import qualified Data.HashMap.Strict as HashMap

import System.Random (randomR, mkStdGen, StdGen)

step :: Game -> Game
step = handleCurrentPiece . spawnNewParticle

spawnNewParticle :: Game -> Game
spawnNewParticle game = 
    let (windowWidth, windowHeight) = game ^. windowSize
        (randX, rng1) = randomR (0, windowWidth - 1) (game ^. rng)
        (randY, rng2) = randomR (0, windowHeight - 1) rng1
        newParticle = mkParticle & particleLocation .~ (fromIntegral randX, fromIntegral randY)
    in game & particles .~ [newParticle] & rng .~ rng2

handleCurrentPiece :: Game -> Game
handleCurrentPiece game = case game ^. currentPiece of
    Nothing -> handleNoPiece game
    Just piece -> handleExistingPiece piece game

handleNoPiece :: Game -> Game
handleNoPiece game = 
    case game ^. gameNextPieces of
        [] -> error "No next pieces available - should never happen"
        (nextPieceType:remainingPieces) ->
            let currentRandomizerEnv = game ^. randomizerEnv
                randomizerFunc = _randomizerSelection currentRandomizerEnv
                (newNextPiece, newRandomizerEnv) = randomizerFunc currentRandomizerEnv
                newPiece = Tetromino nextPieceType (4, 1) North
                updatedNextPieces = remainingPieces ++ [newNextPiece]
            in game & currentPiece .~ Just newPiece 
                    & gameNextPieces .~ updatedNextPieces
                    & randomizerEnv .~ newRandomizerEnv

handleExistingPiece :: Tetromino -> Game -> Game  
handleExistingPiece piece game =
    let movedPiece = over position (\(x, y) -> (x, y + 1)) piece
        movedPieceGrid = getTetrominoGrid movedPiece
        baseGrid = game ^. grid
        canFall = not (overlap baseGrid movedPieceGrid) && isWithinBounds baseGrid movedPieceGrid
    in if canFall
       then game & currentPiece .~ Just movedPiece & slideState .~ CanFall
       else handlePieceCannotFall piece game
           
handlePieceCannotFall :: Tetromino -> Game -> Game
handlePieceCannotFall piece game = 
    case game ^. slideState of
        CanFall -> 
            game & slideState .~ Sliding (piece ^. position)
        Sliding originalPos -> 
            if piece ^. position == originalPos
            then lockPiece piece game
            else game & slideState .~ Sliding (piece ^. position)
        ShouldLock -> 
            lockPiece piece game

lockPiece :: Tetromino -> Game -> Game
lockPiece piece game =
    let currentPieceGrid = getTetrominoGrid piece
        baseGrid = game ^. grid
        gridWithPiece = baseGrid `overlay` currentPieceGrid
        (newGrid, linesCleared) = clearLinesWithCount gridWithPiece
        scorePoints = calculateScore game linesCleared
        (newParticles, newRng) = if linesCleared > 0
                                  then spawnFireworksForLines linesCleared game
                                  else (game ^. particles, game ^. rng)
    in game & grid .~ newGrid 
            & currentPiece .~ Nothing
            & slideState .~ CanFall
            & score %~ (+ scorePoints)
            & particles .~ newParticles
            & rng .~ newRng

spawnFireworksForLines :: Int -> Game -> ([Particle], StdGen)
spawnFireworksForLines linesCleared game =
    let (windowWidth, windowHeight) = game ^. windowSize
        currentRng = game ^. rng
        existingParticles = game ^. particles
        numFireworks = min linesCleared 4  -- Spawn 1 firework per line cleared, max 4
    in spawnMultipleFireworks numFireworks windowWidth windowHeight currentRng existingParticles

spawnMultipleFireworks :: Int -> Int -> Int -> StdGen -> [Particle] -> ([Particle], StdGen)
spawnMultipleFireworks 0 _ _ rng particles = (particles, rng)
spawnMultipleFireworks n width height rng particles =
    let (x, rng1) = randomR (0, width - 1) rng
        (y, rng2) = randomR (0, height - 1) rng1
        center = Vec2 (fromIntegral x) (fromIntegral y)
        (fireworkParticles, rng3) = createLineClearFirework center rng2
        newParticles = map fireworkParticleToParticle fireworkParticles
    in spawnMultipleFireworks (n - 1) width height rng3 (particles ++ newParticles)

fireworkParticleToParticle :: FireworkParticle -> Particle
fireworkParticleToParticle fp =
    let Vec2 x y = _fpPos fp
    in Particle
        { _particleLocation = (x, y)
        , _particleAge = 0
        , _particleType = ParticleFirework fp
        }

moveLeft, moveRight, moveDown :: Coordinate -> Coordinate
moveLeft (x, y) = (x - 1, y)
moveRight (x, y) = (x + 1, y)
moveDown (x, y) = (x, y + 1)

applyMovement :: (Coordinate -> Coordinate) -> Game -> Game
applyMovement moveFunc game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPiece = piece & position %~ moveFunc
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

applyRotation :: (Orientation -> Orientation) -> Game -> Game
applyRotation rotateFunc game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPiece = piece & orientation %~ rotateFunc
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply :: Action -> Game -> Game
apply ActionStep = step
apply (ActionTick dt) = applyTick dt
apply ActionLeft = applyMovement moveLeft
apply ActionRight = applyMovement moveRight
apply ActionSoftDrop = applyMovement moveDown
apply ActionRotateCW = applyRotation rotateCW
apply ActionRotateCCW = applyRotation rotateCCW
apply ActionHardDrop = applyHardDrop

applyTick :: NominalDiffTime -> Game -> Game
applyTick dt game =
  let newAccum = game ^. gameStepAccum + dt
      stepSize = game ^. gameStepSize
      gameWithTime = game & gameTime %~ (+ dt)
      updatedParticles = updateParticles dt (game ^. particles)
      gameWithParticles = gameWithTime & particles .~ updatedParticles
  in if newAccum >= stepSize
     then step (gameWithParticles & gameStepAccum .~ (newAccum - stepSize))
     else gameWithParticles & gameStepAccum .~ newAccum

updateParticles :: NominalDiffTime -> [Particle] -> [Particle]
updateParticles dt = filter (not . isParticleDead) . map (updateParticle dt)

updateParticle :: NominalDiffTime -> Particle -> Particle
updateParticle dt particle = case _particleType particle of
  ParticleStar -> particle & particleAge %~ (+1)
  ParticleFirework fp ->
    let updatedFp = updateFireworkParticle dt defaultFireworkConfig fp
        Vec2 x y = _fpPos updatedFp
    in particle 
        & particleLocation .~ (x, y)
        & particleType .~ ParticleFirework updatedFp

isParticleDead :: Particle -> Bool
isParticleDead particle = case _particleType particle of
  ParticleStar -> _particleAge particle > 100  -- Keep star particles for 100 ticks
  ParticleFirework fp -> _fpLifeState fp == ParticleDead

applyHardDrop :: Game -> Game
applyHardDrop game =
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let finalPiece = hardDropPiece game piece
      in game & currentPiece .~ Just finalPiece & slideState .~ ShouldLock

hardDropPiece :: Game -> Tetromino -> Tetromino
hardDropPiece game piece =
  let (x, y) = piece ^. position
      tryPosition newY = 
        let candidatePiece = piece & position .~ (x, newY)
        in if isValidMove game candidatePiece
           then tryPosition (newY + 1)
           else piece & position .~ (x, newY - 1)
  in tryPosition (y + 1)

isValidMove :: Game -> Tetromino -> Bool
isValidMove game piece =
  let pieceGrid = getTetrominoGrid piece
      gameGrid = game ^. grid
  in isWithinBounds gameGrid pieceGrid && not (overlap gameGrid pieceGrid)

getTetrominoGrid :: Tetromino -> Grid Cell
getTetrominoGrid piece =
  let shapeGrid = case HashMap.lookup (piece ^. tetrominoType) defaultTetrominoMap of
        Just shape -> shape
        Nothing -> emptyGrid Empty
      -- Apply rotations based on orientation
      rotatedGrid = case piece ^. orientation of
        North -> shapeGrid
        East -> rotateClockwise shapeGrid
        South -> rotateClockwise (rotateClockwise shapeGrid)
        West -> rotateCounterClockwise shapeGrid
      (dx, dy) = piece ^. position
      translatedCells = map (\((x, y), cell) -> ((x + dx, y + dy), cell)) (toList rotatedGrid)
  in makeSparse Empty translatedCells



calculateScore :: Game -> Int -> Int
calculateScore game linesCleared
  | linesCleared <= 0 = 0
  | otherwise = 
      let algorithm = game ^. gameScoreAlgorithm
          level = 1  -- TODO: Add proper level calculation
          scoreEvent = ScoreEvent linesCleared level
      in algorithm scoreEvent

resetSlideState :: Game -> Game
resetSlideState = slideState .~ CanFall

defaultGame :: Game
defaultGame = 
  let initialRandomizerEnv = Tetrafall.Randomizer.tgm3 (mkStdGen 24)
      randomizerFunc = _randomizerSelection initialRandomizerEnv
      (nextPiece, updatedRandomizerEnv) = randomizerFunc initialRandomizerEnv
  in Game
    { _grid =  makeDense (10, 22) Empty
    , _score = 0
    , _currentPiece = Nothing
    , _gameNextPieces = [nextPiece]
    , _slideState = CanFall
    , _rng = mkStdGen 42
    , _particles = mempty
    , _windowSize = (0, 0)
    , _randomizerEnv = updatedRandomizerEnv
    , _gameScoreAlgorithm = Tetrafall.Scoring.simple
    , _gameTime = 0
    , _gameStepSize = 0.5
    , _gameStepAccum = 0
    }
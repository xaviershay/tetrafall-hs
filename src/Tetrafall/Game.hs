module Tetrafall.Game (defaultGame, step, apply, getTetrominoGrid) where

import Tetrafall.Types
import Tetrafall.Types.Grid
import qualified Tetrafall.Randomizer
import Lens.Micro.Platform

import qualified Data.HashMap.Strict as HashMap

import System.Random (randomR, mkStdGen)

step :: Game -> Game
step game = 
    let -- Clear existing particles and spawn new one at random location
        (windowWidth, windowHeight) = game ^. windowSize
        (randX, rng1) = randomR (0, windowWidth - 1) (game ^. rng)
        (randY, rng2) = randomR (0, windowHeight - 1) rng1
        newParticle = mkParticle & particleLocation .~ (fromIntegral randX, fromIntegral randY)
        gameWithParticle = game & particles .~ [newParticle] & rng .~ rng2
        newGame = over score ((+) 1) gameWithParticle
    in case newGame ^. currentPiece of
        Nothing -> 
            case newGame ^. gameNextPieces of
                [] -> error "No next pieces available - should never happen"
                (nextPieceType:remainingPieces) ->
                    let currentRandomizerEnv = newGame ^. randomizerEnv
                        randomizerFunc = _randomizerSelection currentRandomizerEnv
                        (newNextPiece, newRandomizerEnv) = randomizerFunc currentRandomizerEnv
                        newPiece = Tetromino nextPieceType (4, 1) North
                        updatedNextPieces = remainingPieces ++ [newNextPiece]
                    in newGame & currentPiece .~ Just newPiece 
                               & gameNextPieces .~ updatedNextPieces
                               & randomizerEnv .~ newRandomizerEnv
        Just piece -> 
            let movedPiece = over position (\(x, y) -> (x, y + 1)) piece
                movedPieceGrid = getTetrominoGrid movedPiece
                baseGrid = newGame ^. grid
                canFall = not (overlap baseGrid movedPieceGrid) && isWithinBounds baseGrid movedPieceGrid
            in if canFall
               then -- Piece can fall normally, reset slide state
                   newGame & currentPiece .~ Just movedPiece & slideState .~ CanFall
               else -- Piece cannot fall, check slide state
                   case newGame ^. slideState of
                       CanFall -> 
                           -- First time piece can't fall, enter sliding state
                           newGame & slideState .~ Sliding (piece ^. position)
                       Sliding originalPos -> 
                           -- Check if piece has moved since sliding started
                           if piece ^. position == originalPos
                           then -- Piece hasn't moved, lock it down
                               let currentPieceGrid = getTetrominoGrid piece
                                   gridWithPiece = baseGrid `overlay` currentPieceGrid
                                   newGrid = clearLines gridWithPiece
                               in newGame & grid .~ newGrid 
                                         & currentPiece .~ Nothing
                                         & slideState .~ CanFall
                           else -- Piece has moved, continue sliding with new position
                               newGame & slideState .~ Sliding (piece ^. position)
                       ShouldLock -> 
                           let currentPieceGrid = getTetrominoGrid piece
                               gridWithPiece = baseGrid `overlay` currentPieceGrid
                               newGrid = clearLines gridWithPiece
                           in newGame & grid .~ newGrid 
                                     & currentPiece .~ Nothing
                                     & slideState .~ CanFall

apply :: Action -> Game -> Game
apply ActionLeft game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position) - 1, snd (piece ^. position))
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionRight game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position) + 1, snd (piece ^. position))
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionRotateCW game =
  case game ^. currentPiece of
    Nothing -> game
    Just piece ->
      let newOrientation = rotateCW (piece ^. orientation)
          newPiece = piece & orientation .~ newOrientation
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionRotateCCW game =
  case game ^. currentPiece of
    Nothing -> game
    Just piece ->
      let newOrientation = rotateCCW (piece ^. orientation)
          newPiece = piece & orientation .~ newOrientation
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionSoftDrop game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position), snd (piece ^. position) + 1)
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionHardDrop game = 
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
      translatedCells = map (\((x, y), cell) -> ((x + dx, y + dy), cell)) (toSparse rotatedGrid)
  in makeSparse Empty translatedCells



-- Helper function to reset slide state when piece moves
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
    }
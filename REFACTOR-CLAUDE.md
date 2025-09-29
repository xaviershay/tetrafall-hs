# Refactoring Opportunities for Tetrafall-HS

## Overview
This document identifies opportunities to enhance code clarity and conciseness using point-free style, DRY principles, and other clean code practices.

## High Priority Refactors

### 1. Game.hs - Apply Function Pattern Matching
**Current:** Verbose pattern matching with repetitive structure
```haskell
apply ActionLeft game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPosition = (fst (piece ^. position) - 1, snd (piece ^. position))
          newPiece = piece & position .~ newPosition
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game
```

**Refactor:** Extract common movement pattern
```haskell
applyMovement :: (Coordinate -> Coordinate) -> Game -> Game
applyMovement moveFunc game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPiece = piece & position %~ moveFunc
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionLeft = applyMovement (\(x, y) -> (x - 1, y))
apply ActionRight = applyMovement (\(x, y) -> (x + 1, y))
apply ActionSoftDrop = applyMovement (\(x, y) -> (x, y + 1))
```

### 2. Game.hs - Position Manipulation
**Current:** Tuple manipulation with `fst`/`snd`
```haskell
let newPosition = (fst (piece ^. position) - 1, snd (piece ^. position))
```

**Refactor:** Use lens combinators or helper functions

```
moveLeft, moveRight, moveDown :: Coordinate -> Coordinate
moveLeft (x, y) = (x - 1, y)
moveRight (x, y) = (x + 1, y)
moveDown (x, y) = (x, y + 1)
```

### 3. Game.hs - Rotation Pattern
**Current:** Duplicate rotation logic
```haskell
apply ActionRotateCW game =
  case game ^. currentPiece of
    Nothing -> game
    Just piece ->
      let newOrientation = rotateCW (piece ^. orientation)
          newPiece = piece & orientation .~ newOrientation
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game
```

**Refactor:** Extract rotation pattern
```haskell
applyRotation :: (Orientation -> Orientation) -> Game -> Game
applyRotation rotateFunc game = 
  case game ^. currentPiece of
    Nothing -> game
    Just piece -> 
      let newPiece = piece & orientation %~ rotateFunc
      in if isValidMove game newPiece
         then resetSlideState $ game & currentPiece .~ Just newPiece
         else game

apply ActionRotateCW = applyRotation rotateCW
apply ActionRotateCCW = applyRotation rotateCCW
```

### 5. Scoring.hs - Pattern Matching
**Current:** Explicit case analysis with error for invalid input
```haskell
simple :: ScoreEvent -> Int
simple scoreEvent = 
  let linesCleared = _scoreLines scoreEvent
      level = _scoreLevel scoreEvent
  in case linesCleared of
       1 -> 100 * level
       2 -> 300 * level
       3 -> 500 * level
       4 -> 800 * level
       _ -> error "invalid number of lines cleared"
```

**Refactor:** Use lookup table or point-free style
```haskell
-- Option 1: Lookup table
simple :: ScoreEvent -> Int
simple (ScoreEvent lines level) = 
  maybe (error "invalid number of lines cleared") (* level) $
  lookup lines [(1, 100), (2, 300), (3, 500), (4, 800)]
```

### 6. KeyboardConfig.hs - getActionForKey Function
**Current:** Repetitive guard conditions
```haskell
getActionForKey :: KeyboardConfig -> V.Key -> Maybe Action
getActionForKey config key
  | key `elem` leftKeys config = Just ActionLeft
  | key `elem` rightKeys config = Just ActionRight
  | key `elem` rotateCWKeys config = Just ActionRotateCW
  | key `elem` rotateCCWKeys config = Just ActionRotateCCW
  | key `elem` softDropKeys config = Just ActionSoftDrop
  | key `elem` hardDropKeys config = Just ActionHardDrop
  | otherwise = Nothing
```

**Refactor:** Use lookup table approach
```haskell
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
```

## Medium Priority Refactors

### 7. Grid.hs - Bounds Checking Pattern
**Current:** Repetitive bounds checking in multiple functions
```haskell
if x >= minX && x <= maxX && y >= minY && y <= maxY
```

**Refactor:** Extract bounds checking function
```haskell
inBounds :: (Coordinate, Coordinate) -> Coordinate -> Bool
inBounds ((minX, minY), (maxX, maxY)) (x, y) = 
  x >= minX && x <= maxX && y >= minY && y <= maxY

-- Usage:
if inBounds (_extent bg) coord then ...
```

### 9. Randomizer.hs - Predicate Combinations
**Current:** Manual predicate combination
```haskell
andPredicate :: Predicate -> Predicate -> Predicate
andPredicate pred1 pred2 env tetrominoType = pred1 env tetrominoType && pred2 env tetrominoType
```

**Refactor:** Use function composition
```haskell
andPredicate :: Predicate -> Predicate -> Predicate
andPredicate = liftA2 (&&)
```

## Low Priority Refactors

### 10. Types.hs - TetrominoMap Construction
**Current:** Large fromList with repetitive TetrominoCell constructors

**Refactor:** Helper functions to reduce noise
```haskell
i, t, s, z, j, l, o :: Cell
i = TetrominoCell I
t = TetrominoCell T
-- etc.

defaultTetrominoMap :: TetrominoMap
defaultTetrominoMap = fromList
  [ (I, makeSparseWithExtent Empty ((-1, -1), (2, 2)) [((-1, 0), i), ((0, 0), i), ((1, 0), i), ((2, 0), i)])
  -- etc.
  ]
```

### 11. Game.hs - Step Function Complexity
**Current:** Large nested function with multiple responsibilities

**Refactor:** Break into smaller functions
```haskell
step :: Game -> Game
step = spawnNewParticle >=> handleCurrentPiece

spawnNewParticle :: Game -> Game
handleCurrentPiece :: Game -> Game
handleNoPiece :: Game -> Game  
handleExistingPiece :: Tetromino -> Game -> Game
```

## Implementation Priority

1. **High Priority** - Address repetitive patterns in Game.hs first as they affect the core game loop
2. **Medium Priority** - Grid.hs improvements will benefit multiple modules  
3. **Low Priority** - Cosmetic improvements that don't significantly impact maintainability

## Benefits Expected

- **Reduced Duplication**: Eliminate repeated patterns in movement and rotation logic
- **Enhanced Readability**: Point-free style where appropriate, clearer intent
- **Better Maintainability**: Changes to common patterns only need to be made in one place
- **Type Safety**: Helper functions can provide better type safety than raw tuple manipulation
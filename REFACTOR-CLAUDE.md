# Refactoring Opportunities for Tetrafall-HS

## Overview
This document identifies opportunities to enhance code clarity and conciseness using point-free style, DRY principles, and other clean code practices.

## High Priority Refactors





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
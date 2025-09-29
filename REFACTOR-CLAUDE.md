# Refactoring Opportunities for Tetrafall-HS

## Overview
This document identifies opportunities to enhance code clarity and conciseness using point-free style, DRY principles, and other clean code practices.

## High Priority Refactors







## Medium Priority Refactors



## Low Priority Refactors

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
# Agent Instructions

## Using Project Scripts

**ALWAYS use `bin/test` instead of `stack` commands directly.**

- **DO**: Use `bin/test` for building, running, and testing the program
- **DON'T**: Use `stack test`, `stack build`, or other stack commands directly
- **DON'T**: Try to run subsets of tests. Always run the full suite, using `bin/test`.
- **WHY**: The `bin/test` script provides consistent error handling and may include additional project-specific configuration

If you need to do something that cannot be accomplished with the available `bin/` scripts, pause and ask for assistance rather than falling back to direct `stack` commands.

Available scripts:
- `bin/test` - Runs the test suite (equivalent to `stack test` with additional flags)

## Handling GHC Compiler Warnings

When fixing GHC compiler warnings, follow these principles:

### Trust the Compiler
- **DO**: When GHC says an import is redundant, remove it immediately without checking for usage
- **DON'T**: Spend time grep searching or manually verifying if imports are used when the compiler already told you they're redundant

### Redundant Import Warnings
GHC warnings like:
```
warning: [GHC-66111] [-Wunused-imports]
    The import of 'Module.Name' is redundant
```

Mean the import should be deleted entirely. The compiler has already done the analysis.

### Missing Type Signatures
For warnings like:
```
warning: [GHC-38417] [-Wmissing-signatures]
    Top-level binding with no type signature:
      functionName :: Type -> Type
```

Add the suggested type signature above the function definition.

### Unused Import Items
For warnings like:
```
warning: [GHC-38856] [-Wunused-imports]
    The import of 'item1, item2, item3'
    from module 'Module.Name' is redundant
```

Remove only the listed items from the import list, keeping the import if other items are still used.

### Workflow
1. Run the build to see warnings
2. Fix warnings by trusting GHC's analysis
3. Verify with another build
4. Do not second-guess the compiler's unused import detection

## Avoiding Redundant Comments

When writing code, avoid comments that simply repeat what the code already says clearly.

### Examples of Redundant Comments to Avoid

**Function Comments:**
```haskell
-- BAD: Comment just repeats the function name
-- Translate a tetromino shape to its world position
translateTetromino :: Tetromino -> Grid Cell -> Grid Cell

-- GOOD: No comment needed, function name is self-explanatory
translateTetromino :: Tetromino -> Grid Cell -> Grid Cell
```

**Test Comments:**
```haskell
-- BAD: Comment repeats test name
testCase "Overlay sparse over dense - basic functionality" $ do
  -- Test overlaying sparse grid on dense background
  let dense = makeDense (3, 3) 'O'

-- GOOD: No redundant comment
testCase "Overlay sparse over dense - basic functionality" $ do
  let dense = makeDense (3, 3) 'O'
```

**Assertion Comments:**
```haskell
-- BAD: Obvious comments on assertions
lookup (0, 0) resultList @?= Just 'A'  -- Top-left
lookup (2, 2) resultList @?= Just 'D'  -- Bottom-right

-- GOOD: Let the test structure speak for itself
lookup (0, 0) resultList @?= Just 'A'
lookup (2, 2) resultList @?= Just 'D'
```

### When Comments Are Useful

**Explaining non-obvious logic:**
```haskell
-- Use row-major order: (0,0), (1,0), (0,1), (1,1)
let expected = [((0, 0), 'T'), ((1, 0), 'T'), ((0, 1), 'T'), ((1, 1), 'T')]
```

**Documenting complex algorithms or business rules:**
```haskell
-- Check that last value wins when multiple values exist at same position
let sparse = makeSparse [((0, 0), 'X'), ((0, 0), 'Y')]
```

### Principle
Write code that is self-documenting through clear naming. Only add comments when they provide information that isn't already obvious from reading the code itself.
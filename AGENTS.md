# Agent Instructions

DO NOT TELL ME I AM ABSOLUTELY RIGHT

## Haskell Specific Things

- **DON'T**: Try to import `foldl'`. That isn't required anymore.

## Interactive Development - STRICTLY PROHIBITED

- **NEVER**: Use `stack ghci`, `ghci`, or any REPL/interactive session
- **NEVER**: Use `stack repl` or any interactive development commands
- **NEVER**: Try to "test quickly" or "experiment" in GHCi
- **NEVER**: Use interactive sessions for debugging, exploration, or verification

## Using Project Scripts

**Use `bin/test` for primary validation, debugging scripts for investigation.**

- **DO**: Use `bin/test` for building, running, and testing the program
- **DO**: Use `stack runghc scripts/script_name.hs` for debugging and investigation scripts
- **DON'T**: Use `stack test`, `stack build`, or any other stack commands directly
- **DON'T**: Use `bin/run` or any other bin scripts
- **DON'T**: Try to run subsets of tests. Always run the full suite, using `bin/test`
- **DON'T**: Try to build or run the application for verification - `bin/test` handles all necessary validation
- **WHY**: The `bin/test` script provides consistent error handling and is the single source of truth for project validation

Available commands:
- `bin/test` - Primary script. Runs the test suite and handles all building/validation (equivalent to `stack test` with additional flags)
- `stack runghc scripts/<script>.hs` - For running debugging and investigation scripts in the `scripts/` folder

## Debugging Scripts

All debugging and investigation scripts should be placed in the `scripts/` folder and run using `stack runghc scripts/script_name.hs`.

### Guidelines for Debugging Scripts
- **DO**: Place all debugging scripts in `scripts/` folder
- **DO**: Import from project modules (e.g., `import Tetrafall.Types`)
- **DO**: Use descriptive names like `debug_tetrominos.hs`, `debug_rotation.hs`
- **DON'T**: Place debugging scripts in the root directory
- **DON'T**: Create debugging scripts that modify the main codebase

### Running Debugging Scripts

```bash
# Run a debugging script
stack runghc scripts/debug_tetrominos.hs
```

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

**State and Value Comments:**
```haskell
-- BAD: Comments that just repeat what the code says
_slideState = Sliding (4, 10)  -- In sliding state
_slideState gameAfter @?= CanFall  -- Should reset to CanFall

-- GOOD: Let the test case name and code speak for itself
_slideState = Sliding (4, 10)
_slideState gameAfter @?= CanFall
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
Write code that is self-documenting through clear naming. **NEVER add comments that simply restate what the code already shows clearly**. This includes:
- Comments that repeat variable names or values
- Comments that describe obvious assertions 
- Comments that restate test case names
- Comments that describe simple state assignments

Only add comments when they provide information that isn't already obvious from reading the code itself.

## Testing Guidelines

### When to Write Tests

Not all features require tests.

**DO** write tests for:
- Core game logic and business rules (e.g., `apply` function for Actions)
- Complex algorithms and data transformations  
- Functions with multiple code paths or edge cases
- Grid operations and collision detection
- State transitions and game mechanics

**DON'T** write tests for:
- Simple configuration mappings (e.g., KeyboardConfig key mappings) - these are straightforward data mappings without complex logic
- Straightforward data structure accessors/setters
- Pure configuration without complex logic
- Simple helper functions with obvious behavior

### Always Use Proper Test Framework
When testing new functionality:

**DO**: Create unit tests using the existing Tasty test framework following the established patterns
- Add test modules in the `test/` directory following naming conventions (e.g., `test/Types/Types.hs` for testing `src/Tetrafall/Types.hs`)
- Import the test module in `test/Spec.hs` and add to the test tree
- Use `testCase` with descriptive names for individual tests
- Group related tests with `testGroup`

**DON'T**: Create standalone test files or try to run quick verification scripts
- Avoid creating temporary test files like `test_apply.hs`
- Don't use manual verification outside the test framework
- Don't skip adding tests to the main test suite

### Test Module Structure
Follow this pattern for new test modules:
```haskell
module ModuleName.TestName (testFunctionName) where

import Test.Tasty
import Test.Tasty.HUnit
import ModuleName

testFunctionName :: TestTree  
testFunctionName = testGroup "Feature Description"
  [ testCase "specific behavior description" $ do
      -- test implementation
  ]
```

**WHY**: Consistent testing ensures reliability and makes it easier to catch regressions. The established test framework provides better error reporting and integration with the build system.
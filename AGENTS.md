# Agent Instructions

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
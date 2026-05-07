# oj-hs: Haskell Online Judge Library

## Architecture

**Purpose**: A Haskell library for competitive programming algorithms and problem solutions, with experimental bundler tooling.

**Key Components**:
- **`src/`**: Core library (algorithms, data structures, solutions)
  - `Algorithm/`: String algorithms (KMP, Aho-Corasick, Suffix Automaton)
  - `Data/`: Custom data structures (FingerTree, SegTree, Trie, EulerTourTree)
  - `Solution/`: Problem solutions (Codeforces, Luogu)
- **`app/all-in-one/`**: Standalone executable with GHC API integration for bundling
- **`bench/`**: Benchmark suite (Criterion-based)
- **`openspec/`**: Design specs and change management (OpenSpec workflow)

**Bundler Project**: Experimental tool for creating self-contained Haskell executables via GHC API. See `openspec/changes/haskell-bundler/` for design docs.

## Build and Test

```bash
# Generate .cabal file from package.yaml
hpack

# Build library and all executables
cabal build

# Run tests
cabal test all

# Run benchmarks
cabal run benchmark-prac-haskell-bench

# Clean and rebuild
cabal clean
cabal build

# Generate .cabal file after modifying package.yaml
hpack && cabal build
```

**Dev Environment**: Use `nix develop` or `shell.nix` for reproducible GHC setup (no Stack dependency). The `.cabal` file is auto-generated from `package.yaml` using `hpack`.

## Conventions

### Code Style
- **Strict warnings**: `-Wall -Wcompat -Widentities` etc. in `app/all-in-one/`
- **NoImplicitPrelude**: `all-in-one` executable uses custom Prelude via `RIO`
- **Module organization**: `Module.Path.CamelCase` naming convention
- **Type signatures**: Explicit signatures preferred (see `Algorithm.Text.KMP` for examples)

### Testing
- **Framework**: Hspec + QuickCheck + HUnit hybrid
- **Property-based testing**: Heavy use of QuickCheck modifiers (`NonNegative`, `Positive`, `ASCIIString`)
- **Test file structure**: `ModuleNameSpec.hs` parallel to source
- **NFData instances**: Required for `deepseq`-based assertions

### Bundler Development
- **Bootstrap testing**: Bundler must produce deterministic output (same input → identical `.hs` output)
- **GHC API**: AST manipulation via `ghc-lib-parser`
- **Symbol transformation**: Internal symbols renamed to avoid conflicts; external deps qualified

## Key Files
- `package.yaml`: Dependency and executable configuration
- `openspec/config.yaml`: Change management configuration
- `openspec/changes/haskell-bundler/`: Active development area with design/docs

## Potential Pitfalls
- **GHC API complexity**: Bundler requires understanding of GHC internals (Core, AST, renamer)
- **Symbol collision**: Bundled code must qualify all external imports and rename module-internal symbols
- **Bootstrap determinism**: Symbol renaming must be stable across compilation runs

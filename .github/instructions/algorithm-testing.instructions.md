---
name: algorithm-testing-instructions
description: "Testing patterns for algorithm implementations using Hspec, QuickCheck, and HUnit"
applyTo:
  - test/Algorithm/**/*.hs
  - test/Data/**/*.hs
  - bench/**/*.hs
---

# Algorithm Testing Guidelines

## Testing Framework

This project uses a **hybrid testing approach**:
- **Hspec**: BDD-style specification tests
- **QuickCheck**: Property-based testing
- **HUnit**: Unit tests with assertions

## Test Structure

### Specification Files
```
src/Algorithm/Text/KMP.hs
test/Algorithm/KMPSpec.hs  # Parallel structure
```

### Test Organization
```haskell
spec :: Spec
spec = describe "Algorithm.KMP" $ do
  describe "prefix function" $ do
    it "simple test case" $ do
      (prefix . V.fromList) "" `shouldBe` [0]
    
    prop "generates valid prefix array" $ \pattern -> 
      property
  
  describe "automaton compilation" $ do
    it "compiles simple pattern" $ do
      compile "ab" `shouldReturn` expectedAutomaton
```

## Property-Based Testing

### Custom Modifiers
Use QuickCheck modifiers for constrained inputs:

```haskell
import Test.QuickCheck.Modifiers (NonNegative, Positive, ASCIIString)

prop_prefixValid :: NonNegative Int -> Property
prop_prefixValid (NonNegative n) = 
  -- test property
```

### Custom Arbitrary Instances
```haskell
instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
```

### Example Properties
```haskell
-- Property: prefix function always produces non-negative values
prop_prefixNonNegative :: V.Vector Int -> Property
prop_prefixNonNegative vec = 
  all (>= 0) (prefix vec) === True

-- Property: prefix length matches input length
prop_prefixLength :: V.Vector Int -> Property
prop_prefixLength vec = 
  length (prefix vec) === length vec

-- Property: compiled automaton accepts pattern
prop_automatonAccepts :: PrintableString -> Property
prop_automatonAccepts (PrintableString pat) = 
  isAccept (run (compile pat) (V.fromList pat)) === True
```

## Test Best Practices

### 1. Cover Edge Cases
```haskell
it "handles empty input" $ do
  prefix V.empty `shouldBe` [0]

it "handles single character" $ do
  prefix (V.fromList ['a']) `shouldBe` [0]
```

### 2. Use NFData for Deep Comparison
```haskell
import Control.DeepSeq (NFData, deepseq)

it "produces correct automaton" $ do
  let result = compile "test"
  result `shouldBe` expected `deepseq` True
```

### 3. Property Testing with Guards
```haskell
prop_validPrefix :: Positive Int -> V.Vector Char -> Property
prop_validPrefix (Positive n) vec = 
  VG.length vec > 0 ==> 
  all (<= n) (prefix vec)
```

### 4. Integration Tests
```haskell
-- Test algorithm end-to-end
it "finds pattern in text" $ do
  let text = "abcdefg"
      pattern = "cde"
  run (compile pattern) (V.fromList text) `shouldContain` expectedMatches
```

## Benchmarking

### Criterion Setup
```haskell
-- bench/Algorithm/KMP/Bench.hs
import Criterion.Main

main = defaultMain [
  bench "prefix small" $ whnf (prefix . V.fromList) smallText
  , bench "prefix large" $ whnf (prefix . V.fromList) largeText
  , bench "compile small" $ whnf compile smallPattern
  ]
```

### Benchmark Patterns
- Measure both time and memory usage
- Compare against baseline implementations
- Test with realistic input sizes

## Common Patterns

### QuickCheck Modifiers
```haskell
import Test.QuickCheck.Modifiers (
  NonNegative, Positive, NonEmpty,
  ASCIIString, PrintableString
)
```

### Hspec Assertions
```haskell
shouldBe      -- Exact equality
shouldReturn  -- IO equality
shouldContain -- List containment
===          -- QuickCheck property assertion
```

### Combining Tests
```haskell
spec = describe "KMP" $ do
  -- HUnit-style tests
  it "basic functionality" $ ...
  
  -- QuickCheck properties
  prop "invariant 1" $ \x -> ...
  prop "invariant 2" $ \x y -> ...
  
  -- Integration tests
  it "end-to-end scenario" $ ...
```

## Debugging Tests

### Failing Properties
When a property fails:
1. Check the counterexample provided by QuickCheck
2. Reduce the counterexample using `quickCheckWith stdArgs {maxSuccess = 100}`
3. Add constraints if the counterexample is unrealistic

### Slow Tests
If tests are slow:
1. Reduce `maxSize` in QuickCheck args
2. Use `forAll` with smaller generators
3. Split into faster/smaller property tests

## Related Documentation
- `test/Algorithm/KMPSpec.hs` - Example test structure
- `bench/Algorithm/KMP/Bench.hs` - Benchmark patterns
- Package `QuickCheck` - Property-based testing library

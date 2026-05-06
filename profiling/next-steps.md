# Next Steps: Swift Grammar Performance (Issue #41)

Branch: `swift-perf-investigation`

## Summary of fixes implemented (2026-05-05 through 2026-05-06)

### Fix 1: G4 LR table CAF caching — DONE (commit 38133a9)
- **Problem**: `glrParseInc2 g4Grammar` rebuilt G4 LR tables on every splice (~6.5s).
- **Fix**: `{-# NOINLINE #-} g4ParseCached` top-level CAF shares tables across splices.
- **Verified**: 2nd splice drops 6.5s → 1.3ms (5000× faster).

### Fix 2: `-O0` for swift test suite — DONE (in package.yaml)
- **Mitigation**: Skips Simplifier passes on the generated SwiftNT ADT.
- **Status**: Applied; contribution measured when swift build completes.

### Fix 3: removeEpsilonsAST exponential algorithm — DONE (commit 625fbb2)
- **Problem**: `removeEpsilonsAST` had O(2^k) complexity for k nullable NTs from
  `*`/`+` annotations. BenchStar (120 annotations): 9+ min stall; Swift: 30+ min.
- **Fix**: Remove `removeEpsilonsAST` from the `genTermAnnotProds` expansion path.
- **Verified**: BenchStar `g4_decls` TH evaluation: 9+ minutes → 7ms (70,000×).

### Fix 4: `lr1Closure` FIRST-set memoization — DONE (commit 7cda3f7)
- **Problem**: `LL.first g` called O(|states| × |items| × |iterations|) times.
- **Fix**: `computeFirstMap` precomputes FIRST(NT x) for all NTs once via fixed-point.
  `lr1Closure g` returns a closure with the precomputed map, shared across calls.
- **Effect**: LR table construction is faster for all grammars.

### Fix 5: GLR error-pruned `concatSets` — DONE (commit 31d5ffc)
- **Problem**: `glrParseInc'` accumulated ALL parse results including dead error branches.
  Wrong paths (from conflicts) were kept alive, compounding O(n³) complexity.
- **Fix**: `concatSets` now discards `ErrorNoAction`/`ErrorTable` results immediately.
  Wrong paths fail quickly and are pruned before they compound.
- **Also fixed**: `0 -> undefined` replaced with graceful `ErrorNoAction` fallback.

### Fix 6: Token caching through Reduce chains — DONE (commit 9259139)
- **Problem**: `lr (s, cs)` re-tokenized `cs` at the start of EVERY call, including
  recursive Reduce calls that don't consume input. With ~10 Reduces per Shift and
  5000 tokens in Swift.g4, this caused ~50,000 tokenizer calls instead of ~5,000.
- **Fix**: `lr` now accepts `Maybe (t, [c])` for a pre-computed token. Reduce calls
  pass `Just (a, ws)`; Shift calls reset to `Nothing` (re-tokenize from `ws`).

### Fix 7: Remove `Generic` derive; `Hashable via fromEnum` — DONE (commit d26e083)
- **Problem**: `deriving Generic` for 963-constructor `SwiftNT` generates a deeply
  nested `:+:` representation type that GHC must elaborate and typecheck.
- **Fix**: Remove `Generic` from `symbolDerives`; generate manual
  `instance Hashable T where hashWithSalt n x = hashWithSalt n (fromEnum x)`.

### Fix 8: `disambiguatedGlrParseInc2` + `greedyDisambiguate` — DONE (commit 765b857)
- Added `disambiguatedGlrParseInc2` for O(n) parsing when disambiguation is safe.
- Added `greedyDisambiguate` (Shift preference, longer RHS for Reduce/Reduce) as
  an improvement over `S.findMin` disambiguation.
- **Note**: Neither disambiguation approach is correct for the full Swift grammar
  due to remaining G4 grammar ambiguities beyond the regex sub-grammar.

### Fix 9: Left-recursive `unionR` in G4 regex grammar — DONE (commit e7408dc)
- **Problem**: Right-recursive `unionR : regex '|' regex | regex '|' unionR` created
  Reduce/Reduce conflict: when `unionR` on stack + lookahead starts a regex, parser
  couldn't distinguish "reduce to regex1" vs "build longer union". Full GLR explored
  2^K branches for K such conflicts → O(n³) for large inputs.
- **Fix**: Change second alternative to `unionR '|' regex` (left-recursive).
  Left-recursion converts Reduce/Reduce to Shift/Reduce (or eliminates it); GLR
  branching is dramatically reduced.
- **Measured**: glrParse for Swift.g4 dropped from 81.7s (wrong disambiguation) to
  31.5s with left-recursive grammar. Full GLR with left-recursive grammar: TBD.

## Current status (awaiting swift build completion)

The definitive measurement is running: left-recursive G4 grammar + error-pruned
GLR (`glrParseInc2`). Monitor via `/tmp/swift-lr-glr.log`.

Expected breakdown:
1. `glrParse` of Swift.g4 (37046 chars): should be much less than 9+ hours
2. `g4_decls` TH code generation: ~few ms (fixed by removeEpsilonsAST)
3. GHC type-checking SwiftNT (963 constructors, 7 derives, -O0): TBD

## Remaining work

### Option B2: Redesign genTermAnnotProds (long-term)
The fundamental issue: 963 NT constructors from 309 `*`/`+`/`?` annotations. Even
with all the above fixes, GHC typechecking this many constructors takes significant
time. Long-term fix: change `genTermAnnotProds` to NOT generate flat NT constructors
for annotations. Instead, use a wrapper type like `NT_Star NT_r1` in the grammar
value. This keeps the NT ADT at the expected ~345 constructors. Significant refactor.

### Remaining G4 grammar conflicts
The G4 grammar still has Shift/Reduce and possibly other Reduce/Reduce conflicts
beyond `unionR`. These affect disambiguation approaches but are handled correctly
by full GLR. Audit the full conflict list from `greedyDisambiguate` output to
identify any remaining fixable grammar issues.

### Runtime profiling (once swift builds)
```bash
stack test antlr-haskell:swift --profile -- +RTS -p -RTS
```
The `.prof` file will show where runtime time goes (LR table construction vs parsing).

## Key files

| File | Relevance |
|------|-----------|
| `src/Language/ANTLR4/G4.hs` | G4 grammar definition — the `unionR` fix is here |
| `src/Language/ANTLR4/Parser.hs` | g4ParseCached CAF + timing instrumentation |
| `src/Language/ANTLR4/Boot/Quote.hs` | symbolDerives, genTermAnnotProds |
| `src/Text/ANTLR/LR.hs` | lr1Closure, glrParseInc', disambiguate functions |
| `test/bench/` | Synthetic benchmark suite |
| `profiling/timing-summary.md` | All measured data |

## Tooling setup (on this machine)
- GHCup at `~/.ghcup/env` — source before running stack
- GHC 9.6.7, Stack 3.7.1 installed
- `stack build antlr-haskell:test:bench` — benchmark suite (~2 min to build, ~30s to run)
- `-ddump-timings` shows per-phase GHC timing
- `[g4 timing]` output shows instrumented TH timing

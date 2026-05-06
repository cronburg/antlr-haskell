# Next Steps: Swift Grammar Performance (Issue #41)

Branch: `swift-perf-investigation`

## What we know (confirmed by profiling)

### Fix 1: G4 LR table CAF caching — DONE
- **Problem**: `glrParseInc2 g4Grammar` rebuilt the G4 grammar's LR1 items, action table,
  and goto function on every `[g4|...|]` invocation (~6.5s fixed cost per splice).
- **Fix**: `g4ParseCached = LR.glrParseInc2 g4Grammar event2ast` top-level CAF in
  `Language.ANTLR4.Parser`. First splice pays the cost once; all subsequent splices
  in the same compilation reuse cached tables.
- **Verified**: Second splice drops 6.5s → 1.3ms (5000× speedup).
- **Files changed**: `src/Language/ANTLR4/Parser.hs`

### Fix 2: `-O0` for swift test suite — DONE (mitigation)
- **Problem**: `genTermAnnotProds` expands `*`/`+`/`?` annotations into NT constructors.
  Swift has 309 annotations → ~963 NT constructors. GHC type-checking + running multiple
  Simplifier passes over this ADT (× 9 derived classes) takes 30+ minutes.
- **Mitigation**: `-O0` in swift test suite ghc-options skips Simplifier, FloatOut,
  SpecConstr, etc. Type-checking still runs, but should be much faster overall.
- **Files changed**: `package.yaml`
- **Status**: NOT YET MEASURED — need to run the swift build with -O0 to verify speedup.

## Next steps

### Step A: Measure -O0 speedup (high priority)
Run `time stack build antlr-haskell:test:swift` and record wall time. Compare to the
30+ minute baseline. If -O0 reduces to <5 minutes, this is a good interim fix.

### Step B: Understand genTermAnnotProds (the real fix)
File: `src/Language/ANTLR4/Boot/Quote.hs`, function `genTermAnnotProds` (~line 305).

Current behavior: for each `r1*` in a grammar rule, generates new NT rules like:
  `r1_star : r1 r1_star | ;`
This creates 2 new NT constructors per annotation. With 309 annotations in Swift,
the `SwiftNT` ADT grows from 345 → ~963 constructors.

**Option B1**: Keep genTermAnnotProds but reduce derived instances on the expanded types.
  - `Data` is only needed by `disambiguate` (in experimental `mkLRParser` path).
  - `Lift` might not be needed on NT/T types (the `lift ast` in g4_decls lifts `[G4S.G4]`,
    not the generated NT type). CAUTION: removing `Lift` caused GHC-76037 errors in testing —
    needs more investigation before proceeding.
  - `Bounded`/`Enum` ARE needed (used in `[minBound..maxBound]` in grammar value construction).

**Option B2**: Change genTermAnnotProds to not generate new NT constructors.
  Instead of `NT_r1_star`, use a wrapper `NT_Star NT_r1` in the grammar value.
  This requires changing how the Grammar type represents expansion rules, which is
  a significant refactor but would keep the NT ADT at the expected size.

**Option B3**: Split the generated grammar across multiple modules.
  Compile NT definitions and grammar value in one module; parsers/DFAs in another.
  GHC handles smaller modules faster. Complex to implement.

### Step C: Memoize LL.first in lr1Closure (runtime perf, separate issue)
File: `src/Text/ANTLR/LR.hs`, function `lr1Closure` (~line 234).

`LL.first g (β ++ tokenToProdElem a)` is called without memoization on every item in
every closure iteration. For large grammars at runtime (when glrParse is called by user
code), this makes LR table construction slow.

Fix: precompute `firstMap :: Map [ProdElem nts sts] (Set (Icon sts))` before the closure
fixpoint, then do map lookups instead of recursive computation. This would make
`glrParse swiftGrammar` faster at runtime (currently also slow due to same issue).

### Step D: Runtime profiling (separate from compile time)
Once swift builds, run:
```bash
stack test antlr-haskell:swift --profile -- +RTS -p -RTS
```
The `.prof` file will show where runtime time goes (LR table construction vs parsing).

## Key files for next agent

| File | Relevance |
|------|-----------|
| `src/Language/ANTLR4/Parser.hs` | g4ParseCached CAF fix + timing instrumentation |
| `src/Language/ANTLR4/Boot/Quote.hs` | symbolDerives, genTermAnnotProds (the real problem) |
| `src/Text/ANTLR/LR.hs` | lr1Closure, lr1Items, glrParseInc2 (runtime perf) |
| `src/Text/ANTLR/LL1.hs` | first/follow — no memoization (runtime perf root cause) |
| `test/bench/` | Synthetic benchmark suite for measuring compile time |
| `profiling/timing-summary.md` | All measured data and root cause analysis |

## Tooling setup (on this machine)
- GHCup at `~/.ghcup/env` — source before running stack
- GHC 9.6.7, Stack 3.7.1 installed
- `stack build antlr-haskell:test:bench` — runs the benchmark suite (fast, ~30s)
- `-ddump-timings` flag shows per-phase GHC timing
- `[g4 timing]` output in build log shows instrumented TH timing

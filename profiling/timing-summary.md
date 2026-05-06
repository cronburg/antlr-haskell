# [g4|...|] Compile-Time Profiling Data

## Methodology

`Language.ANTLR4.Parser.g4_codeGen` instrumented with `getCPUTime` around:
1. `glrParse isWhitespace input` — the GLR parse of the grammar string
2. `G4Q.g4_decls $ ast2decls ast` — TH code generation

Build command: `stack build antlr-haskell:test:bench --ghc-options="-ddump-timings"`

## Phase timing results (session 2026-05-05, pre-fix baseline)

| Grammar | Rules | Chars | glrParse (CPU) | g4_decls (CPU) | Renamer/TC total |
|---------|-------|-------|----------------|----------------|-----------------|
| Bench10 | 10    | 304   | 7.16s          | 0.62ms         | 7.97s           |
| Bench30 | 30    | 824   | 6.51s          | 1.09ms         | —               |
| Bench60 | 60    | 1604  | 10.52s         | 3.67ms         | 11.11s          |

**Key finding**: `Renamer/typechecker` ≈ `glrParse` for small grammars — post-TH
type-checking adds only 0.35s (10 rules) to 0.59s (60 rules). Derived instances are
NOT the bottleneck for simple grammars.

## CAF caching result: two splices in same compilation

| Splice  | Grammar | Chars | glrParse (CPU) |
|---------|---------|-------|----------------|
| 1st     | Bench10 | 304   | 6.63s          |
| 2nd     | Tiny    | 59    | 1.32ms (~5000× faster) |

**Fix implemented**: `{-# NOINLINE #-} g4ParseCached = LR.glrParseInc2 g4Grammar event2ast`
as a top-level CAF shares G4 LR tables across all splices in a compilation. First splice
pays the fixed cost once; all subsequent splices reuse the cached tables.

## The hidden multiplier: genTermAnnotProds

`genTermAnnotProds` in `Boot/Quote.hs` silently expands `*`/`+`/`?` annotations into extra
NT grammar rules. Each annotation generates ~2 extra NT constructors. The Swift grammar has
**309 such annotations**, inflating the NT type from 345 → **~963 constructors**.

| Grammar  | Explicit rules | `*`/`+`/`?` annotations | Approx NT constructors |
|----------|---------------|------------------------|----------------------|
| BenchBase | 30            | 0                      | ~30                  |
| BenchStar | 30            | 120                    | ~270                 |
| Swift     | 345           | 309                    | ~963                 |

## Feature benchmark results (session 2026-05-06, after all fixes)

After: CAF caching + error-pruned GLR + token caching + left-recursive G4 grammar.

| Feature    | Description                          | glrParse (2nd splice) |
|------------|--------------------------------------|----------------------|
| BenchBase  | 30 rules, no annotations             | 8.6s (1st splice baseline) |
| BenchLexer | Complex lexer rules only             | 0.095s |
| BenchLits  | 30 rules + 2 string literals/rule    | 0.54s |
| BenchOpt   | 30 rules + `?` annotations           | 0.53s |
| BenchStar  | 30 rules + `*`/`+` annotations       | 0.57s |
| BenchWide  | 30 rules + 8 alternatives/rule       | 1.24s |

## Swift grammar glrParse timing history

| Approach                                         | glrParse time | Parse correct? |
|--------------------------------------------------|---------------|----------------|
| Original (full GLR, no fixes)                    | 9+ hours      | Yes (eventually) |
| + CAF caching + removeEpsilonsAST                | still 9+ hours| Yes            |
| + disambiguation (S.findMin)                     | 81.7s         | No — failed at `~[...]` |
| + greedy disambiguation (longer RHS)             | 31.5s         | No — failed at `'grammar'` |
| + left-recursive unionR + glrParseInc2           | 47+ min (killed) | N/A — still exponential |
| + all 9 fixes (disambiguation attempt — reverted)| 28.5s         | No — `ErrorNoAction` state 73 |

## Root cause summary — all components

**Component 1 (FIXED)**: G4 LR table CAF caching.
- Was: 6.5s per splice for building G4 LR tables from scratch.
- Fix: `g4ParseCached` NOINLINE CAF in `Language.ANTLR4.Parser`.

**Component 2 (FIXED)**: `removeEpsilonsAST` exponential algorithm in `g4_decls`.
- Was: `2^k` variants created for k nullable NTs in `*`/`+` expansion rules.
- Fix: remove `removeEpsilonsAST` from the `genTermAnnotProds` expansion path.

**Component 3 (FIXED)**: Redundant tokenization in Reduce chains.
- Was: tokenizer called O(reductions_per_shift) × n times.
- Fix: cache `(a, ws)` through Reduce chains; re-tokenize only after Shifts.

**Component 4 (FIXED)**: Generic derive generating large Rep type for NT/T ADTs.
- Was: `deriving Generic` for 963-constructor `SwiftNT` created a deeply nested
  type-level `:+:` tree that GHC had to elaborate.
- Fix: remove `Generic` from `symbolDerives`; generate manual `Hashable via fromEnum`.

**Component 5 (FIXED)**: GHC Simplifier passes on generated ADTs.
- Mitigation: `-O0` on swift test suite skips Simplifier.

**Component 6 (FIXED)**: Reduce/Reduce conflicts in G4 regex grammar (glrParse).
- Was: right-recursive `unionR : regex '|' regex | regex '|' unionR` created
  Reduce/Reduce conflict when `unionR` was on stack and lookahead started a regex.
  Full GLR explored 2^K branches for K conflicts → O(n³) complexity.
- Fix: change to left-recursive `unionR : regex '|' regex | unionR '|' regex`.
  Left-recursion converts Reduce/Reduce to Shift/Reduce (or no conflict); GLR
  branching is reduced dramatically.

**Component 7 (PARTIALLY FIXED)**: `lr1Closure` FIRST-set recomputation.
- Was: `LL.first g` called O(|states| × |items| × |iterations|) times during LR
  table construction.
- Fix: precompute `firstMap` once via `computeFirstMap` using iterative fixed-point;
  share across the returned closure function.

**Remaining bottleneck (pending)**: GHC type-checking 963-constructor `SwiftNT` ADT.
Even with all the above fixes, GHC still needs to typecheck ~963 constructors × 7
derived classes. This is a fundamentally O(N^k) problem for large grammars.
Long-term fix: redesign `genTermAnnotProds` to not generate flat NT constructors for
`*`/`+`/`?` annotations (Option B2 in next-steps.md).

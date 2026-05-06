# [g4|...|] Compile-Time Profiling Data

## Methodology

`Language.ANTLR4.Parser.g4_codeGen` instrumented with `getCPUTime` around:
1. `glrParse isWhitespace input` — the GLR parse of the grammar string
2. `G4Q.g4_decls $ ast2decls ast` — TH code generation

Build command: `stack build antlr-haskell:test:bench --ghc-options="-ddump-timings"`

## Phase timing results

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

BenchStar (120 annotations, ~270 NT constructors): glrParse 0.61s, then GHC stalled
12+ minutes on type-checking the generated ADT.

Swift (309 annotations, ~963 NT constructors) → the 30-minute build.

## Three-component model (corrected)

```
total [g4|...|] compile time ≈
    glrParse_cost             (fixed 6.5s for 1st splice, ~1ms for subsequent)
  + g4_decls_cost             (always ~1-5ms — negligible)
  + GHC_typecheck_cost        (dominates for grammars with many annotations)

NT constructors = explicit_rules + 2 × star_plus_question_annotations

GHC_typecheck_cost scales super-linearly with NT constructor count:
  BenchBase (~30 NT): fast (<0.5s)
  BenchStar (~270 NT): 12+ minutes (observed)
  Swift     (~963 NT): 30+ minutes (observed)
```

## Feature benchmark results (glrParse after first-splice caching)

| Feature    | Description                          | glrParse |
|------------|--------------------------------------|----------|
| BenchBase  | 30 rules, no annotations             | baseline (first splice, 8s) |
| BenchLexer | Complex lexer rules only             | 0.07-0.14s |
| BenchLits  | 30 rules + 2 string literals/rule    | 0.45-0.81s |
| BenchOpt   | 30 rules + `?` annotations           | 0.60-0.73s |
| BenchStar  | 30 rules + `*`/`+` annotations       | 0.54-0.87s |
| BenchWide  | 30 rules + 8 alternatives/rule       | (stalled — same issue as BenchStar) |

None of these features dramatically slow `glrParse` — the slowness is in GHC type-checking
the NT ADT after TH evaluation, not in the parsing phase.

## Root cause summary

The Swift [g4|...|] compile bottleneck has two confirmed components:

**Component 1 (fixed)**: G4 LR table construction (6.5s fixed cost per module).
Fix: `g4ParseCached` CAF in `Language.ANTLR4.Parser`.

**Component 2 (pending)**: GHC type-checking `SwiftNT` (~963 constructors from
genTermAnnotProds expansion) × 9 derived classes.
Immediate mitigation: `-O0` on swift test suite (skips multiple Simplifier passes).
Proper fix: redesign genTermAnnotProds to avoid NT constructor explosion.

# [g4|...|] Compile-Time Profiling Data

## Methodology

`Language.ANTLR4.Parser.g4_codeGen` instrumented with `getCPUTime` around:
1. `glrParse isWhitespace input` — the GLR parse of the grammar string
2. `G4Q.g4_decls $ ast2decls ast` — TH code generation

Build command: `stack build antlr-haskell:test:bench --ghc-options="-ddump-timings"`

## Results

| Grammar | Rules | Chars | glrParse (CPU) | g4_decls (CPU) |
|---------|-------|-------|----------------|----------------|
| Bench10 | 10    | 304   | 7.16s          | 0.62ms         |
| Bench30 | 30    | 824   | 6.51s          | 1.09ms         |

## Key finding: two splices in same compilation

| Splice  | Grammar | Chars | glrParse (CPU) |
|---------|---------|-------|----------------|
| 1st     | Bench10 | 304   | 6.63s          |
| 2nd     | Tiny    | 59    | 1.32ms (~5000× faster) |

## Interpretation

**`glrParse` cost is dominated by G4 grammar LR table construction, not input parsing.**

Evidence: Bench10 (304 chars) ≈ Bench30 (824 chars) in time despite 3× input size.
The G4 grammar (17 parser rules + 8 lexer rules) LR table construction is fixed at ~6.5s
per module load inside GHC's bytecode interpreter.

**Caching works**: adding `{-# NOINLINE #-} g4ParseCached = LR.glrParseInc2 g4Grammar event2ast`
as a top-level CAF causes the LR tables to be shared. The second `[g4|...|]` splice is
5000× faster because the tables are already computed.

## Two-component model of compile time

For a grammar with N rules:

```
total compile time ≈ glrParse_cost + g4_decls_cost + GHC_typecheck_cost

glrParse_cost  ≈ 6.5s (fixed, first splice) or 1ms (subsequent splices)
g4_decls_cost  ≈ 1ms (negligible)
GHC_typecheck_cost ≈ O(N^k) — dominates for N ≥ ~50 rules
```

For small grammars (N < 50): `glrParse_cost` dominates.
For large grammars (N ≥ 50, e.g. Swift N=326): GHC type-checking dominates.

The Swift grammar spent 30+ minutes in `Renamer/typechecker [Grammar]`, of which
only ~6.5s was `glrParse`. The remainder (~29+ minutes) is GHC type-checking:
- `Bench10NT` data type: 10 constructors × 9 derived classes
- `Bench10T` data type: 3 constructors × 9 derived classes
- `SwiftNT` data type: 326 constructors × 9 derived classes (Generic/Data/Lift = slow)

# Swift Grammar Performance Investigation

This directory holds timing and profiling artifacts from the investigation of the Swift
grammar test suite's performance (Issue #41).

## What's slow

`test/swift/` is excluded from CI because it is extremely slow. The grammar is derived
from the full Swift 4 grammar (~1083 rules in `Swift.g4`), compiled via:

1. `Grammar.hs` — hand-ported Haskell file using `[g4|...|]` quasiquoter (1138 lines)
2. `Parser.hs` — `$(g4_parsers swiftAST swiftGrammar)` TH splice
3. `swift.hs` — runtime test: parses `"var i = 0;"` via `glrParse`

The bottleneck could be:
- Compile-time: TH splice expansion (ATN/LR table construction at compile time)
- Runtime: ATN construction, LR table generation, or actual parsing

## Artifacts

| File | How generated | What it tells us |
|------|---------------|-----------------|
| `compile-timings.txt` | `stack build :swift --ghc-options="-ddump-timings"` | Per-phase GHC compile time; isolates TH splice cost |
| `runtime.prof` | `stack test :swift --profile -- +RTS -p -RTS` | Runtime heap/CPU profile |
| `runtime-heap.hp` | same run | Heap profile over time (render with `hp2ps`) |

## Key questions

1. Is the slowness at compile time (TH) or runtime?
2. If compile: which phase — parsing the G4 QQ, or the `g4_parsers` splice?
3. If runtime: is it ATN construction, LR table gen, or parsing `"var i = 0;"`?

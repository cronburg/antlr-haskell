## NT Constructor Explosion: Design Plan

The Swift grammar generates ~963 NT constructors in a single flat enum (`SwiftNTSymbol`), each
carrying 9 derived instances. GHC spends 30+ minutes typechecking the resulting derived code.
This document describes three incremental fixes, ordered by impact-to-risk ratio.

---

### Phase 1: Move `Data` and `Lift` to standalone deriving in `mkLRParser`

Remove `"Data"` and `"Lift"` from `symbolDerives`. In `mkLRParser`, emit standalone deriving
declarations for the NT and terminal symbol types before generating the parser functions:

```haskell
StandaloneDerivD Nothing [] (AppT (ConT ''Data) (ConT ntSym))
StandaloneDerivD Nothing [] (AppT (ConT ''Lift) (ConT ntSym))
-- same for tSym
```

`g4_decls` — the path taken by `test/swift/` and all non-LR users — generates no code that
requires `Lift SwiftNTSymbol` or `Data SwiftNTSymbol`. `the_ast` lifts `[G4S.G4]`, not the
generated NT type. The quasiquote brackets in `grammar` refer to constructors by name, not by
lifting values. Only `test/coreg4/` calls `mkLRParser`, and its grammar has ~10 rules, so the
standalone instances are trivially cheap there.

`Data` generates `gfoldl`, `gunfold`, and the `gmapX` family — each a full 963-case match.
`Lift` generates a 963-case `lift` function. Removing both from the bulk derive is the highest-ROI
change in this plan.

Risk is low. The earlier GHC-76037 note about `liftData` needing `Data` does not apply: the
generated types use a proper derived instance, not the empty-instance trick from the
fix-warnings branch.

---

### Phase 2: Replace `Bounded`/`Enum` with TH-generated explicit lists

In `Quote.hs` around line 396, the `grammar` function initializes `ns` and `ts` via
`[minBound..maxBound]`. Replace those with explicit TH list expressions:

```haskell
{ ns = Set.fromList $(listE $ map (conE . mkName . ("NT_"++)) (nonterms ast ++ regexNonTermSymbols ast))
, ts = Set.fromList $(listE $ map (conE . mkName . ("T_"++)) (allLexicalSymbols ast))
```

The lists are already fully known at TH time. The substitution is semantically identical:
constructors appear in declaration order in both cases.

Remove `"Bounded"` and `"Enum"` from `symbolDerives`. Add standalone `Bounded`/`Enum` deriving
inside `mkLRParser` if `allProdElems'` (LR.hs line 317) is still needed there — that function
uses `[minBound..maxBound]` and is only called from `mkLRParser`. If it can be replaced with the
same explicit-list approach, remove the instances entirely.

`Bounded` and `Enum` for a 963-constructor type produce large `toEnum`/`fromEnum` case
expressions that the GHC simplifier works hard on even at `-O0`. Phase 2 eliminates both.

Risk is medium. Audit all call sites of `allProdElems'` before removing the constraints.

---

### Phase 3: Split the NT type at the annotation boundary

This phase is warranted only if Phases 1 and 2 together do not reach the <5 minute target.

Generate two types instead of one flat enum:

```haskell
-- ~345 constructors for Swift
data SwiftBaseNT = NT_swiftFile | NT_topLevelDecl | ...
  deriving (Eq, Ord, Show, Generic, Hashable)

-- 2 constructors regardless of grammar size
data SwiftNTSymbol = NTBase SwiftBaseNT | NTAnnot TermAnnot SwiftBaseNT
  deriving (Eq, Ord, Show, Generic, Hashable)
```

`genTermAnnotProds` currently adds names like `expr_star` to the flat NT enum. Under this
scheme it instead represents annotation-expanded references as `NTAnnot Star (NTBase NT_expr)`.
This propagates into `getProds` and `toElem`. The `ns` field in `grammar` becomes an explicit
TH list (from Phase 2) mixing `NTBase` and `NTAnnot` values.

The derived instances for `SwiftBaseNT` are cheap (5 classes, ~345 constructors). The derived
instances for `SwiftNTSymbol` are trivially cheap (2 constructors). The annotation-expansion
code in `ntDataDeclQ` is replaced by a new `ntBaseDataDeclQ` that emits only the base NT
declarations, and `ntDataDeclQ` is rewritten to emit the 2-constructor composite.

The `Prettify` instance for NT also needs updating to handle the `NTAnnot` case.

This is a breaking API change. Pattern matches on `NT_expr_star` in user code must become
`NTAnnot Star (NTBase NT_expr)` or a synonym. Audit `test/swift/` and any downstream users
before landing.

---

### Files to change

| File | Phase | Change |
|------|-------|--------|
| `src/Language/ANTLR4/Boot/Quote.hs` | 1 | Remove `Data`/`Lift` from `symbolDerives`; add standalone deriving in `mkLRParser` |
| `src/Language/ANTLR4/Boot/Quote.hs` | 2 | Replace `[minBound..maxBound]` with explicit TH lists; remove `Bounded`/`Enum` from `symbolDerives` |
| `src/Language/ANTLR4/Boot/Quote.hs` | 3 | New `ntBaseDataDeclQ`; rewrite `ntDataDeclQ` to 2-constructor type; update `getProds`/`toElem`/`genTermAnnotProds` |
| `src/Text/ANTLR/LR.hs` | 2 | Remove `Bounded`/`Enum` constraint from `allProdElems'`, or replace its `[minBound..maxBound]` with an explicit parameter |
| `test/coreg4/G4Fast.hs` | 1–2 | Verify still compiles after standalone deriving change |

---

### Measurement

After each phase:

```bash
source ~/.ghcup/env
time stack build antlr-haskell:test:swift --ghc-options="-ddump-timings" 2>&1 | tee profiling/swift-build-phaseN.log
stack build antlr-haskell:test:bench
```

The bench suite runs in ~30 seconds and catches regressions in the non-Swift paths. The target
is a swift build under 5 minutes, at which point Issue #41 can be re-enabled in CI with an
appropriate timeout guard.

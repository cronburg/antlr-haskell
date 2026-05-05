# Known Test Failures

This file documents test failures that are pre-existing (not regressions introduced by the
`ghc-modernize` branch) or are due to known incomplete features.

## `:lr` — `test_GLRInc`, `test_GLRPartial`

**Status**: Pre-existing failures on master (GHC 8.8.4) and ghc-modernize (GHC 9.6.6).

GLR incremental tokenization (`GLRInc`) and partial tokenization (`GLRPartial`) are
experimental features explicitly documented as incomplete in the README. These tests
exercise that experimental path and fail with parse errors on valid input.

## `:unit` — compile failure in `PlusBug0.hs`

**Status**: Pre-existing failure. The `[g4|...|]` quasiquoter generates `NT_LowerID_plus`
in the data type definition but references `T_LowerID_plus` in pattern match code for the
rule `plus : LowerID+ -> Plus`. This is an inconsistency in the quasiquoter's name
generation for annotated terminal references (`+`, `*`, `?`).

The test was added as part of issue #34 (regex annotation syntactic sugar) but the
underlying naming bug was not fully resolved. This is a quasiquoter correctness issue
unrelated to the GHC version upgrade.

## `:chisel` — compile failure, missing test fixture

**Status**: Pre-existing. `test/chisel/Main.hs` references
`[open| test/chisel/Language/Chisel/Examples/GHC.chi |]` via the `FileOpener`
quasiquoter, which reads the file at compile time. The file and its parent
directory (`Language/Chisel/Examples/`) were never committed to the repository,
so the test suite does not compile in a clean checkout.

**Excluded from CI** until the fixture file is restored or the test is rewritten.

## Notes on `hashable` and `HashSet` ordering

The `hashable` library changed its hashing algorithm between 1.3.x (GHC 8.x) and 1.4.x
(GHC 9.x). Any code that relies on a specific iteration order of `HashSet`/`HashMap` may
behave differently across GHC versions. One such bug was fixed in `Text.ANTLR.LL1.leftFactor`
(the `maximumBy: empty structure` crash) as part of the `ghc-modernize` work.

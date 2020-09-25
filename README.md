# antlr-haskell
A Haskell implementation of ANTLR.

In implementing ANTLR we referenced the behavior of the original Java version
(ANTLR4):
[The definitive ANTLR4 Reference.](https://pragprog.com/book/tpantlr2/the-definitive-antlr-4-reference)
However we have taken much liberty in the design of this library compared to the
workflow of the original Java version. In particular in implementing ANTLR for
Haskell we have followed the following principles:

- Parsing backends should be interchangeable
  - GLR, LR, SLR, LL, ALL(\*)
- Code should be first class and declarative
  - The implementation of G4 is metacircular
  - Regular expressions are interpreted
- Implement algorithms from first principles
  - Set notation is used in implementing LL and LR algorithms.
  - Pure functional implementations of parsing algorithms can eventually support
    embedding of arbitrary (including IO) actions without breaking the predictive
    parsing abstraction.

More info can be found here:
[https://www.cronburg.com/2018/antlr-haskell-project/](https://www.cronburg.com/2018/antlr-haskell-project/)

## Build instructions

The library can be built with:

```bash
stack build # stack version 2.3.3
stack test :simple
```

Or with cabal-3.0.1.0 like:

```bash
cabal configure
cabal install --only-dependencies --enable-tests
cabal build
cabal test sexpression
```

Here's a good one to run when making changes to the library, and you're unsure
of what may become affected by those changes:

```bash
stack test :simple :atn :ll :lr :sexpression :allstar :c
```

And then compare the results with that of this upstream branch. Some of the
GLR features (incremental and partial tokenization, notably) are still experimental,
and so there are known test cases which currently fail.

## Version History

- September 25th, 2020. Released version 0.1.0.1: bug fixes, documentation, and
  library versioning updates.


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

## Build instructions

The library can be built with:

```
stack init # Currently selects lts-6.26 resolver
stack repl
```


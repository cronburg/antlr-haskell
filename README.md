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
stack build # stack version 1.9.3
stack test :simple
```

Or with cabal-2.4.0.1 like:

```bash
cabal configure
cabal install --only-dependencies --enable-tests
cabal build
cabal test sexpression
```

## Sample grammar for ALL(\*)

```
S -> Ac | Ad

A -> aA | b
```

### ALL(\*) Input/output examples

```haskell
*Test.AllStarTests> parse ['a', 'b', 'c'] (NT 'S') atnEnv
(Just True, Node 'S' [Node 'A' [Leaf 'a', Node 'A' [Leaf 'b']], Leaf 'c'])
```

```haskell
*Test.AllStarTests> parse ['b', 'd'] (NT 'S') atnEnv
(Just True, Node 'S' [Node 'A' [Leaf 'b'], Leaf 'd'])
```

```haskell
*Test.AllStarTests> parse ['a', 'a', 'a', 'a', 'b', 'c'] (NT 'S') atnEnv
(Just True, Node 'S' [Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'a', Node 'A' [Leaf 'b']]]]], Leaf 'c'])
```


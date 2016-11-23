# ANTLR4-Haskell
A Haskell implementation of ANTLR4

A reimplementation of the ANTLR4 specification as described in:
[The definitive ANTLR4 Reference](https://pragprog.com/book/tpantlr2/the-definitive-antlr-4-reference)

This implementation has three main parts:
- Text.ALLSTAR
  - The LL algorithm module and associated helpers to implement "allstar" or ALL(\*), the top down parsing algorithm for ANTLR4.
  - Runtime utilities such as the gui and interactive tools
- Language.ANTLR4
  - the quasiquotation and monadic eDSL for using ANTLR4 in terms of ANTLR g4 style grammar files.
- Debug.ANTLR4 and Debug.ALLSTAR
  - An implementation of sidBison for ANTLR

## Build instructions

Presently the library can be built with:

```
stack init # Currently selects lts-6.26 resolver
stack repl
```


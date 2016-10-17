# Overview
A quick overview of what translations need to happen:  
## Translation Strategy
TODO
## ANTLR Runtime Classes
The following are terms of the Java and we can annotate with a translation strategy.  

ANTLRInputStream
- parameterized by the input stream

CommonTokensStream
- parameterized by a grammar specific lexer

ParseTree (datastructure)
- invocation of a grammar specific parser at a specific start token

ParseTreeWalker
- parameterized by callback implementations for entering and exiting specific productions
- parameterized by a specific parse tree data structure

## Generated Grammar specific Classes

\*Lexer
- Parameterized by an ANTLRInputStream

\*Parser
- Parameterized by a Token Stream

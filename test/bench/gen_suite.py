#!/usr/bin/env python3
"""Generate targeted benchmark grammars isolating specific Swift grammar features."""
import os, sys

OUT = os.path.dirname(os.path.abspath(__file__))

def write(name, body):
    path = f"{OUT}/Bench{name}.hs"
    content = f"""\
{{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell #-}}
module Bench{name} where
import Language.ANTLR4

[g4|

{body}
|]
"""
    open(path, 'w').write(content)
    size = len(body)
    print(f"  wrote Bench{name}.hs ({size} chars of grammar)")

N = 30

# --- Baseline: simple rules, no literals
rules_base = "\n".join(f"r{i} : r{(i+1)%N} | r{(i+2)%N} | A | B ;" for i in range(N))
write("Base", f"grammar BenchBase;\n\nstart : r0 ;\n\n{rules_base}\n\nA : 'a' ;\nB : 'b' ;\nWS : [ \\t\\n\\r]+ ;")

# --- Feature 1: String literals as alternatives (each rule has 3-4 literals)
LITS = ["';'", "'.'", "':'", "'->'", "'?'", "'!'", "'+'", "'-'", "'*'", "'/'",
        "'('", "')'", "'['", "']'", "'{'", "'}'", "'='", "'=='", "'!='", "'<'",
        "'>'", "'<='", "'>='", "'&&'", "'||'", "'let'", "'var'", "'if'", "'else'", "'return'"]
rules_lits = "\n".join(
    f"r{i} : r{(i+1)%N} | r{(i+2)%N} | A | {LITS[i % len(LITS)]} | {LITS[(i+1) % len(LITS)]} ;"
    for i in range(N))
write("Lits", f"grammar BenchLits;\n\nstart : r0 ;\n\n{rules_lits}\n\nA : 'a' ;\nWS : [ \\t\\n\\r]+ ;")

# --- Feature 2: Optional elements with '?' annotations (like Swift's 'stmt ';'?')
rules_opt = "\n".join(
    f"r{i} : r{(i+1)%N} ';'? | r{(i+2)%N} '.'? | A ';'? ;"
    for i in range(N))
write("Opt", f"grammar BenchOpt;\n\nstart : r0 ;\n\n{rules_opt}\n\nA : 'a' ;\nWS : [ \\t\\n\\r]+ ;")

# --- Feature 3: Wide rules — many alternatives per rule (like Swift's statement rule)
rules_wide = "\n".join(
    " | ".join([f"r{(i+k)%N}" for k in range(8)] + ["A", "B"]) + " ;"
    if i == 0 else
    f"r{i} : r{(i+1)%N} | r{(i+2)%N} | A | B ;"
    for i in range(N))
# Actually make ALL rules wide
rules_wide = "\n".join(
    "r{} : {} ;".format(i, " | ".join([f"r{(i+k)%N}" for k in range(1,5)] + ["A", "B", f"'{chr(97+i%26)}'", f"'{chr(65+i%26)}'"]  ))
    for i in range(N))
write("Wide", f"grammar BenchWide;\n\nstart : r0 ;\n\n{rules_wide}\n\nA : 'a' ;\nB : 'b' ;\nWS : [ \\t\\n\\r]+ ;")

# --- Feature 4: Complex lexer rules with character classes (like Swift's IdentifierHead)
complex_lexer = """
grammar BenchLexer;

start : ID ID ID ;

ID : IdentifierHead IdentifierChar* ;
IdentifierHead : [a-zA-Z_] ;
IdentifierChar : [a-zA-Z0-9_] ;
Dec : [0-9]+ ;
Hex : '0x' [0-9a-fA-F]+ ;
Float : [0-9]+ '.' [0-9]+ ;
Str : '\"' (~'\"')* '\"' ;
LCmt : '//' (~'\\n')* '\\n' ;
BCmt : '/*' (~'*')* '*/' ;
WS : [ \\t\\n\\r]+ ;
"""
write("Lexer", complex_lexer)

# --- Feature 5: Plus/star annotations (like Swift's 'statements*', 'parameter+')
rules_star = "\n".join(
    f"r{i} : r{(i+1)%N}* | r{(i+2)%N}+ | A* | B+ ;"
    for i in range(N))
write("Star", f"grammar BenchStar;\n\nstart : r0 ;\n\n{rules_star}\n\nA : 'a' ;\nB : 'b' ;\nWS : [ \\t\\n\\r]+ ;")

print("Done.")

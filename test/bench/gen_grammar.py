#!/usr/bin/env python3
"""Generate a synthetic G4 grammar with N parser rules for compile-time scaling tests.

Usage: gen_grammar.py N [--literals]
  --literals: each rule uses string literals as alternatives (like Swift), not just NT refs
"""
import sys

args = sys.argv[1:]
use_literals = '--literals' in args
args = [a for a in args if not a.startswith('--')]
N = int(args[0]) if args else 30

KEYWORDS = ['if','else','while','for','return','var','let','func','class',
            'struct','enum','case','switch','break','continue','import',
            'true','false','nil','self','super','init','deinit','get','set']

print(f"grammar Bench{N}{'L' if use_literals else ''};")
print()
print(f"start : r0 ;")
print()
for i in range(N):
    a = (i + 1) % N
    b = (i + 2) % N
    if use_literals and i < len(KEYWORDS):
        kw = KEYWORDS[i % len(KEYWORDS)]
        print(f"r{i} : r{a} | r{b} | A | '{kw}' | ';' | ':' | '.' ;")
    else:
        print(f"r{i} : r{a} | r{b} | A | B ;")
print()
print("A : 'a' ;")
print("B : 'b' ;")
print("WS : [ \\t\\n\\r]+ ;")

#!/usr/bin/env python3
"""Generate a synthetic G4 grammar with N parser rules for compile-time scaling tests."""
import sys

N = int(sys.argv[1]) if len(sys.argv) > 1 else 30

print(f"grammar Bench{N};")
print()
print(f"start : r0 ;")
print()
for i in range(N):
    a = (i + 1) % N
    b = (i + 2) % N
    print(f"r{i} : r{a} | r{b} | A | B ;")
print()
print("A : 'a' ;")
print("B : 'b' ;")
print("WS : [ \\t\\n\\r]+ ;")

# Reduced-C

Because why not.

... time to learn some cool compiler tricks.

## TODO

1. Lex and parse into S-exprs

2. desugar, normalise, typecheck, SSA?
   - Optimizations:
      - typecheck: Steensgard's algorithm
      - dead code elim
      - common subexpr     
      - constant prop
      - strength reduction
      - tailrec elim
      - flat closures, pointers to refs in heap
      - bounds checking, pointer analysis
      - bound check elimination

3. code gen to MIPS or ARM

## See also

Reduced-C in Haskell: https://github.com/lcycon/reducedc



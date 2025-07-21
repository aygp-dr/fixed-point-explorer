# CONTINU vs CONTINUE Note

In the Lean specifications, we use "continu" instead of "continue" because:

1. `continue` is a reserved keyword in Lean 4
2. It's a playful misspelling similar to `docs/images/tshit.svg` 
3. Throughout the project, CONTINUE and CONTINU may be used interchangeably

The pattern refers to the continuation-style function builders used in the Y combinator implementation, where functions like:
- CONTINUE-fib = λf.λn. if n ≤ 1 then n else f(n-1) + f(n-2)

...are passed to Y to create recursive functions without explicit recursion.

This naming convention adds a bit of humor while solving a technical constraint!
-- Performance analysis of recursive functions
-- Made standalone to avoid import issues during testing

namespace Performance

-- Copy of fib from Fibonacci.lean for standalone compilation
def fib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib (n + 1) + fib n

-- Memoized Fibonacci using arrays
def fibMemo (n : Nat) : Nat := Id.run do
  if n ≤ 1 then return n
  let mut cache : Array Nat := Array.mkArray (n + 1) 0
  cache := cache.set! 0 0
  cache := cache.set! 1 1
  for i in [2:n+1] do
    cache := cache.set! i (cache[i-1]! + cache[i-2]!)
  return cache[n]!

-- Tail recursive Fibonacci with accumulator
def fibTailRec (n : Nat) : Nat :=
  let rec loop : Nat → Nat → Nat → Nat
    | 0, a, _ => a
    | n + 1, a, b => loop n b (a + b)
  loop n 0 1

-- Performance comparison (informally)
#eval fibMemo 100     -- Much faster for large n
#eval fibTailRec 100  -- Also efficient

-- Theorem: All three implementations compute the same values
theorem fib_implementations_equal (n : Nat) :
  fib n = fibMemo n ∧ fib n = fibTailRec n := by
  sorry -- Would need induction on n

end Performance

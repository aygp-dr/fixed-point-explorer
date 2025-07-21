-- Comprehensive test suite
import «Fibonacci»
import «Properties»
import «Performance»

namespace TestSuite

def testFibonacci : IO Unit := do
  IO.println "=== Fibonacci Tests ==="
  let testCases := [(0, 0), (1, 1), (5, 5), (10, 55), (20, 6765)]
  for (n, expected) in testCases do
    let result := fib n
    if result = expected then
      IO.println s!"✓ fib({n}) = {result}"
    else
      IO.println s!"✗ fib({n}) = {result}, expected {expected}"

def testListOps : IO Unit := do
  IO.println "\n=== List Operation Tests ==="
  
  -- Test append
  let l1 := [1, 2, 3]
  let l2 := [4, 5, 6]
  IO.println s!"append {l1} {l2} = {myAppend l1 l2}"
  
  -- Test map
  let doubled := myMap (· * 2) l1
  IO.println s!"map (*2) {l1} = {doubled}"
  
  -- Test filter  
  let evens := myFilter (· % 2 = 0) [1, 2, 3, 4, 5, 6]
  IO.println s!"filter even [1..6] = {evens}"
  
  -- Test composition
  let composed := myMap (· + 1) (myFilter (· % 2 = 0) [1, 2, 3, 4, 5, 6])
  IO.println s!"map (+1) ∘ filter even = {composed}"

def testPerformance : IO Unit := do
  IO.println "\n=== Performance Tests ==="
  
  -- Compare implementations for n = 35
  let n := 35
  IO.println s!"Computing fib({n})..."
  
  -- Note: The naive version will be slow
  let t1 := (← IO.monoNanosNow)
  let r1 := fib n
  let t2 := (← IO.monoNanosNow)
  IO.println s!"  Naive: {r1} in {(t2 - t1) / 1000000}ms"
  
  let t3 := (← IO.monoNanosNow)
  let r2 := Performance.fibMemo n
  let t4 := (← IO.monoNanosNow)
  IO.println s!"  Memoized: {r2} in {(t4 - t3) / 1000000}ms"
  
  let t5 := (← IO.monoNanosNow)
  let r3 := Performance.fibTailRec n
  let t6 := (← IO.monoNanosNow)
  IO.println s!"  Tail Recursive: {r3} in {(t6 - t5) / 1000000}ms"

def main : IO Unit := do
  testFibonacci
  testListOps
  testPerformance
  IO.println "\n✓ All tests completed!"

end TestSuite

-- Fibonacci.lean
-- Formal specification of Fibonacci function

-- Standard recursive definition
def fib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib (n + 1) + fib n

-- Test cases matching our Scheme tests
#eval fib 0   -- 0
#eval fib 1   -- 1
#eval fib 5   -- 5
#eval fib 10  -- 55
#eval fib 20  -- 6765

-- Theorems about fibonacci
theorem fib_zero : fib 0 = 0 := rfl
theorem fib_one : fib 1 = 1 := rfl
theorem fib_five : fib 5 = 5 := rfl
theorem fib_ten : fib 10 = 55 := rfl

-- List operations
def myAppend {α : Type} : List α → List α → List α
  | [], ys => ys
  | x :: xs, ys => x :: myAppend xs ys

def myLength {α : Type} : List α → Nat
  | [] => 0
  | _ :: xs => 1 + myLength xs

def myMap {α β : Type} (f : α → β) : List α → List β
  | [] => []
  | x :: xs => f x :: myMap f xs

def myFilter {α : Type} (p : α → Bool) : List α → List α
  | [] => []
  | x :: xs => if p x then x :: myFilter p xs else myFilter p xs

-- Test cases matching our Scheme tests
#eval myAppend [1, 2, 3] [4, 5, 6]  -- [1, 2, 3, 4, 5, 6]
#eval myLength ['a', 'b', 'c', 'd', 'e']  -- 5
#eval myMap (· * 2) [1, 2, 3, 4]  -- [2, 4, 6, 8]
#eval myFilter (· % 2 = 0) [1, 2, 3, 4, 5, 6]  -- [2, 4, 6]

-- Verify properties
theorem append_empty {α : Type} (xs : List α) : myAppend xs [] = xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => simp [myAppend, ih]
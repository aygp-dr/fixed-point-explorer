-- Simple.lean
-- Basic test to verify Lean 4 is working

-- Simple check
#check (1 : Nat)

-- Basic evaluation
#eval 1 + 1

-- Simple theorem
theorem one_eq_one : 1 = 1 := rfl

-- Function definition
def double (n : Nat) : Nat := n + n

#eval double 21  -- Should output 42

-- List example
#eval [1, 2, 3] ++ [4, 5, 6]

-- Simple proof
theorem double_two : double 2 = 4 := rfl
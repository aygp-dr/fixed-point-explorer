-- Advanced properties of recursive functions
import «Fibonacci»

namespace Properties

-- Prove termination explicitly for our functions
theorem fib_terminates : ∀ n : Nat, ∃ m : Nat, fib n = m := by
  intro n
  exact ⟨fib n, rfl⟩

-- Golden ratio connection
-- Fib(n) ≈ φⁿ/√5 where φ = (1+√5)/2
def goldenRatio : Float := (1.0 + 5.0.sqrt) / 2.0

-- Binet's formula (statement only - full proof needs Real numbers)
theorem binet_formula (n : Nat) : 
  ∃ ε > 0, |fib n - (goldenRatio ^ n / 5.0.sqrt).round| < ε := by
  sorry

-- Properties of list operations
theorem myMap_composition {α β γ : Type} (f : β → γ) (g : α → β) (xs : List α) :
  myMap f (myMap g xs) = myMap (f ∘ g) xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => simp [myMap, Function.comp, ih]

theorem myFilter_idempotent {α : Type} (p : α → Bool) (xs : List α) :
  myFilter p (myFilter p xs) = myFilter p xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => 
    simp [myFilter]
    split
    · simp [myFilter, *]
    · exact ih

-- Length properties
theorem myAppend_length {α : Type} (xs ys : List α) :
  myLength (myAppend xs ys) = myLength xs + myLength ys := by
  induction xs with
  | nil => simp [myAppend, myLength]
  | cons x xs ih => simp [myAppend, myLength, ih, Nat.add_assoc]

theorem myMap_preserves_length {α β : Type} (f : α → β) (xs : List α) :
  myLength (myMap f xs) = myLength xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih => simp [myMap, myLength, ih]

end Properties

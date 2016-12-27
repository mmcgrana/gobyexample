-- An Agda example file

module test where

open import Coinduction
open import Data.Bool
open import {- pointless comment between import and module name -} Data.Char
open import Data.Nat
open import Data.Nat.Properties
open import Data.String
open import Data.List hiding ([_])
open import Data.Vec hiding ([_])
open import Relation.Nullary.Core
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; trans; inspect; [_])
  renaming (setoid to setiod)

open SemiringSolver

{- this is a {- nested -} comment -}

postulate pierce : {A B : Set} → ((A → B) → A) → A

instance
  someBool : Bool
  someBool = true

-- Factorial
_! : ℕ → ℕ
0 ! = 1
(suc n) ! = (suc n) * n !

-- The binomial coefficient
_choose_ : ℕ → ℕ → ℕ
_ choose 0 = 1
0 choose _ = 0
(suc n) choose (suc m) = (n choose m) + (n choose (suc m)) -- Pascal's rule

choose-too-many : ∀ n m → n ≤ m → n choose (suc m) ≡ 0
choose-too-many .0 m z≤n = refl
choose-too-many (suc n) (suc m) (s≤s le) with n choose (suc m) | choose-too-many n m le | n choose (suc (suc m)) | choose-too-many n (suc m) (≤-step le)
... | .0 | refl | .0 | refl = refl

_++'_ : ∀ {a n m} {A : Set a} → Vec A n → Vec A m → Vec A (m + n)
_++'_ {_} {n} {m} v₁ v₂ rewrite solve 2 (λ a b → b :+ a := a :+ b) refl n m = v₁ Data.Vec.++ v₂

++'-test : (1 ∷ 2 ∷ 3 ∷ []) ++' (4 ∷ 5 ∷ []) ≡ (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ [])
++'-test = refl

data Coℕ : Set where
  co0   : Coℕ
  cosuc : ∞ Coℕ → Coℕ

nanana : Coℕ
nanana = let two = ♯ cosuc (♯ (cosuc (♯ co0))) in cosuc two

abstract
  data VacuumCleaner : Set where
    Roomba : VacuumCleaner

pointlessLemmaAboutBoolFunctions : (f : Bool → Bool) → f (f (f true)) ≡ f true
pointlessLemmaAboutBoolFunctions f with f true | inspect f true
... | true  | [ eq₁ ] = trans (cong f eq₁) eq₁
... | false | [ eq₁ ] with f false | inspect f false
... | true  | _       = eq₁
... | false | [ eq₂ ] = eq₂

mutual
  isEven : ℕ → Bool
  isEven 0       = true
  isEven (suc n) = not (isOdd n)

  isOdd : ℕ → Bool
  isOdd 0       = false
  isOdd (suc n) = not (isEven n)

foo : String
foo = "Hello World!"

nl : Char
nl = '\n'

private
  intersperseString : Char → List String → String
  intersperseString c []       = ""
  intersperseString c (x ∷ xs) = Data.List.foldl (λ a b → a Data.String.++ Data.String.fromList (c ∷ []) Data.String.++ b) x xs

baz : String
baz = intersperseString nl (Data.List.replicate 5 foo)

postulate
  Float : Set

{-# BUILTIN FLOAT Float  #-}

pi : Float
pi = 3.141593

-- Astronomical unit
au : Float
au = 1.496e11 -- m

plusFloat : Float → Float → Float
plusFloat a b = {! !}

record Subset (A : Set) (P : A → Set) : Set where
  constructor _#_
  field
    elem   : A
    .proof : P elem

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Fixpoint.Fix (module Data.Fixpoint.Fix) where

import Prelude.Staged
import Data.Functor.Classes

type Fix ∷ (Type → Type) → Type
newtype Fix f = Fix (f (Fix f))

wrap ∷ f (Fix f) → Fix f
wrap = Fix

unwrap ∷ Fix f → f (Fix f)
unwrap (Fix x) = x

fold ∷ Functor f ⇒ (f a → a) → Fix f → a
fold φ = φ ∘ fmap (fold φ) ∘ unwrap

unfold ∷ Functor f ⇒ (a → f a) → a → Fix f
unfold ψ = wrap ∘ fmap (unfold ψ) ∘ ψ

hoist ∷ Functor f ⇒ (∀ a. f a → g a) → Fix f → Fix g
hoist α = Fix ∘ α ∘ fmap (hoist α) ∘ unwrap

instance Eq1 f ⇒ Eq (Fix f) where
    Fix a == Fix b = eq1 a b

instance Ord1 f ⇒ Ord (Fix f) where
    compare (Fix a) (Fix b) = compare1 a b

instance Show1 f ⇒ Show (Fix f) where
    showsPrec d (Fix a) = showParen (d > 10) $ showString "Fix " ∘ showsPrec1 11 a

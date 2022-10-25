{-# LANGUAGE NoImplicitPrelude #-}

module Data.Fixpoint.Nu (Nu (..), wrap, unwrap, fold, unfold, hoist) where

import Prelude.Staged

type Nu ∷ (Type → Type) → Type
data Nu f = ∀ a. Nu (a → f a) a

-- "Cheap"; piggy backs of a different expensive operation

hoist ∷ (∀ a. f a → g a) → Nu f → Nu g
hoist α (Nu ψ s) = Nu (α ∘ ψ) s

unfold ∷ (a → f a) → a → Nu f
unfold = Nu

wrap ∷ Functor f ⇒ f (Nu f) → Nu f
wrap = unfold (fmap unwrap)

unwrap ∷ Functor f ⇒ Nu f → f (Nu f)
unwrap (Nu ψ s) = fmap (Nu ψ) (ψ s)

-- "Expensive"; introduces recursion

fold ∷ Functor f ⇒ (f a → a) → Nu f → a
fold φ (Nu ψ s) = let go = φ ∘ fmap go ∘ ψ in go s

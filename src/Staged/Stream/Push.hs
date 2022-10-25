{-# LANGUAGE NoImplicitPrelude #-}

module Staged.Stream.Push (Push (..), fold, unfold, wrap, hoist) where

import Prelude.Staged

type Push ∷ (Type → Type) → Type
newtype Push f = Push (∀ a. (f (Code a) → Code a) → Code a)

fold ∷ (f (Code a) → Code a) → Push f → Code a
fold φ (Push k) = k φ

hoist ∷ (∀ a. f (Code a) → g (Code a)) → Push f → Push g
hoist α (Push k) = Push \φ → k (φ ∘ α)

unfold ∷ Functor f ⇒ (Code a → f (Code a)) → Code a → Push f
unfold ψ x = Push \φ → let go = φ ∘ fmap go ∘ ψ in go x

wrap ∷ Functor f ⇒ f (Push f) → Push f
wrap x = Push \φ → φ (fmap (fold φ) x)


{-# LANGUAGE NoImplicitPrelude #-}

module Data.Fixpoint.Mu (Mu, wrap, unwrap, fold, unfold, hoist) where

import Prelude.Staged

type Mu ∷ (Type → Type) → Type
newtype Mu f = Mu (∀ a. (f a → a) → a)

-- "Cheap"; piggy backs of a different expensive operation

hoist ∷ (∀ a. f a → g a) → Mu f → Mu g
hoist α (Mu k) = Mu \φ → k (φ ∘ α)

fold ∷ (f a → a) → Mu f → a
fold φ (Mu k) = k φ

wrap ∷ Functor f ⇒ f (Mu f) → Mu f
wrap x = Mu \φ → φ (fmap (fold φ) x)

unwrap ∷ Functor f ⇒ Mu f → f (Mu f)
unwrap = fold (fmap wrap)

-- "Expensive"; introduces recursion

unfold ∷ Functor f ⇒ (a → f a) → a → Mu f
unfold ψ x = Mu \φ → let go = φ ∘ fmap go ∘ ψ in go x


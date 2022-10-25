{-# LANGUAGE NoImplicitPrelude #-}

module Data.ListF (ListF (..)) where

import Prelude.Staged

data ListF a b
    = NilF
    | ConsF a b
    deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

instance Bifunctor ListF where
    bimap _ _ NilF = NilF
    bimap f g (ConsF a b) = ConsF (f a) (g b)




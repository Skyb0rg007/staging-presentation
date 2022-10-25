{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Stream.Push (module Data.Stream.Push) where

import Prelude.Staged
import Data.ListF

newtype Push a = Push (∀ b. (ListF (Code a) (Code b) → Code b) → Code b)

map ∷ (Code a → Code b) → Push a → Push b
map f (Push k) = Push \φ → k (φ ∘ first f)

fold ∷ (ListF (Code a) (Code b) -> Code b) -> Push a -> Code b
fold φ (Push k) = k φ

wrap ∷ ListF (Code a) (Push a) -> Push a
wrap x = Push \φ -> φ (fmap (fold φ) x)

cons ∷ Code a → Push a → Push a
cons x xs = wrap (ConsF x xs)

empty ∷ Push a
empty = wrap NilF

foldr ∷ (Code a → Code b → Code b) → Code b → Push a → Code b
foldr f z (Push k) = k φ
    where
        φ NilF = z
        φ (ConsF a b) = f a b

foldl ∷ ∀ a b. (Code b → Code a → Code b) → Code b → Push a → Code b
foldl f z (Push k) = [|| $$(k φ) $$z ||]
    where
        φ ∷ ListF (Code a) (Code (b → b)) → Code (b → b)
        φ NilF = [|| \acc → acc ||]
        φ (ConsF a b) = [|| \acc → $$b $$(f [|| acc ||] a) ||]

sum ∷ Push Int → Code Int
sum = foldl (\a b → [|| $$a + $$b ||]) [|| 0 ||]

range ∷ Code Int → Code Int → Push Int
range lo' hi' = Push \φ → [||
        let go lo hi =
                if lo >= hi
                    then $$(φ NilF)
                    else $$(φ (ConsF [||lo||] [||go (lo + 1) hi||]))
         in go $$lo' $$hi' ||]


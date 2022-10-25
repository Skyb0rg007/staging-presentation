
module Staged.Stream.PushList (module Staged.Stream.PushList) where

import Prelude.Staged
import Staged.Stream.Push qualified as Push
import Staged.Stream.Push (Push (..))

type ListF ∷ Type → Type → Type
data ListF a b
    = NilF
    | ConsF a b
    deriving Functor

instance Bifunctor ListF where
    first _ NilF = NilF
    first f (ConsF a b) = ConsF (f a) b

    second _ NilF = NilF
    second f (ConsF a b) = ConsF a (f b)

type PushList ∷ Type → Type
type PushList a = Push (ListF (Code a))

empty ∷ PushList a
empty = Push \φ → φ NilF

-- cons ∷ Code a → PushList a → PushList a
-- cons x xs = Push φ → φ (ConsF )

map ∷ (Code a → Code b) → PushList a → PushList b
map f = Push.hoist (first f)

-- foldl ∷ (Code a → Code b → Code b) → Code b → PushList a → Code b
-- foldl f z (Push k) = [|| $$(k φ) $$z]
--     where
--         φ NilF = 

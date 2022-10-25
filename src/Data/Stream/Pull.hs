{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Stream.Pull (module Data.Stream.Pull) where

import Prelude.Staged
import Data.Stream.Push qualified as Push
import Data.Stream.Push (Push (Push))
import Data.ListF

data Pull a = ∀ b. Pull (Code b) (∀ r. Code b → (ListF (Code a) (Code b) → Code r) → Code r)

map ∷ (Code a → Code b) → Pull a → Pull b
map f (Pull s ψ) = Pull s \seed k →
    ψ seed \case
        NilF      → k NilF
        ConsF a b → k (ConsF (f a) b)

zip ∷ Pull a → Pull b → Pull (a, b)
zip (Pull s ψ) (Pull t ω) = Pull [||($$s, $$t)||]
    \seed k → [||
        let (seed0, seed1) = $$seed
         in $$(ψ [||seed0||] \case
            NilF → k NilF
            ConsF a seed0' →
                ω [||seed1||] \case
                    NilF → k NilF
                    ConsF b seed1' →
                        k (ConsF [||($$a, $$b)||] [||($$seed0', $$seed1')||])) ||]

zipPush ∷ Pull a → Push b → Push (a, b)
zipPush (Pull s ψ) (Push k) = Push \φ →
    let φ' NilF        = [|| \_ → $$(φ NilF) ||]
        φ' (ConsF a b) =
            [|| \seed → $$(ψ [||seed||] \case
                NilF          → φ NilF
                ConsF x seed' → φ (ConsF [||($$x, $$a)||] [||$$b $$seed'||])) ||]
     in [|| $$(k φ') $$s ||]

foldl ∷ (Code b → Code a → Code b) → Code b → Pull a → Code b
foldl f z (Pull s ψ) =
    [|| let go seed acc = $$(ψ [||seed||] \case
                                        NilF          → [||acc||]
                                        ConsF x seed' → [||go $$seed' $$(f [||acc||] x)||])
         in go $$s $$z ||]

sum ∷ Pull Int → Code Int
sum = foldl (\a b → [|| $$a + $$b ||]) [|| 0 ||]

range ∷ Code Int → Code Int → Pull Int
range lo' hi' = Pull [||($$lo', $$hi')||] ψ
    where
        ψ ∷ ∀ r. Code (Int, Int) → (ListF (Code Int) (Code (Int, Int)) → Code r) → Code r
        ψ seed k =
            [|| let (cur, hi) = $$seed
                 in if cur >= hi
                        then $$(k NilF)
                        else $$(k (ConsF [||cur||] [||(cur + 1, hi)||])) ||]

countFrom ∷ Code Int → Pull Int
countFrom i = Pull [||$$i||] ψ
    where
        ψ ∷ Code Int → (ListF (Code Int) (Code Int) → Code r) → Code r
        ψ seed k = k (ConsF seed [||$$seed + 1||])

append ∷ Pull a → Pull a → Pull a
append (Pull s0 ψ0) (Pull s1 ψ1) = Pull [||Left $$s0||] \seed k →
    [|| case $$seed of
            Left s → $$(ψ0 [||s||] \case
                NilF → ψ1 s1 \case
                    NilF → k NilF
                    ConsF a b → k (ConsF a [||Right $$b||])
                ConsF a b → k (ConsF a [||Left $$b||]))
            Right s → $$(ψ1 [||s||] \case
                NilF → k NilF
                ConsF a b → k (ConsF a [||Right $$b||])) ||]



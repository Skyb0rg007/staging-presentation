{-# LANGUAGE TemplateHaskell #-}

module Prelude.Staged
    ( Code
    , showCode
    , (∘)
    , Bifunctor (bimap, first, second)
    , Type
    , module Prelude
    ) where

import Data.Bifunctor
import Data.Kind (Type)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (liftTyped))
import Prelude hiding (foldl, foldr, map, sum)
import System.IO.Unsafe (unsafePerformIO)

type Code = TH.Code TH.Q

(∘) ∷ (b → c) → (a → b) → a → c
(∘) = (.)

showCode ∷ Code a → String
showCode = unsafePerformIO ∘ TH.runQ ∘ fmap (show ∘ TH.ppr) ∘ TH.unTypeCode

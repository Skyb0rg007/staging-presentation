{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude.Staged
import Data.Stream.Push qualified as Push
import Data.Stream.Push (Push)
import Data.Stream.Pull qualified as Pull
import Data.Stream.Pull (Pull)

sumOfSquares ∷ Push Int → Code Int
sumOfSquares = Push.sum ∘ Push.map (\x → [|| $$x + $$x ||])

foo ∷ Push Int → Code Int
foo = sumOfSquares ∘ Push.cons [|| 40 ||]

test ∷ Code Int 
test = foo (Push.range [|| 0 ||] [|| 5 ||])

-- (\a ->
--   (let go lo hi =
--          if lo >= hi
--              then \acc -> acc
--              else \acc -> go (lo + 1) hi (acc + (lo + lo))
--     in go 0 5) a + 40 + 40)
--    0
--
-- ====>
--
-- let go lo hi acc =
--       if lo >= hi
--         then acc
--         else go (lo + 1) hi (acc + lo + lo)
--  in go 0 5 (0 + 40 + 40)

test2 ∷ Code Int
test2 = Pull.sum ∘ Pull.map (\x → [|| $$x + $$x||]) $ Pull.range [||0||] [||5||]

-- let go s acc =
--       case s of
--         (cur, hi) ->
--           if cur >= hi
--             then acc
--             else go (cur + 1, hi) (acc + (cur + cur))
--  in go (0, 5) 0
--
-- ====>
--
-- let go cur hi acc =
--       if cur >= hi
--          then acc
--          else go (cur + 1) hi (acc + cur + cur)
--  in go 0 5 0

withIndex ∷ Push a → Push (Int, a)
withIndex = Pull.zipPush (Pull.countFrom [||0||])

test3 ∷ Code Int
test3 = Push.sum ∘ Push.map (\p → [||fst $$p + snd $$p||]) ∘ withIndex $ Push.range [||0||] [||5||]

-- let go lo hi =
--       if lo >= hi
--         then \_ acc -> acc
--         else \s acc -> go (lo + 1) hi (s + 1) (acc + fst (s, lo) + snd (s, lo))
--  in go 0 5 0 0
--
-- ====>
--
-- let go lo hi s acc =
--       if lo >= hi
--          then acc
--          else go (lo + 1) hi (s + 1) (acc + s + lo)
--  in go 0 5 0 0


main :: IO ()
main = do
    putStrLn "Hello, world!"
    putStrLn $ "Code: " ++ showCode test
    putStrLn $ "Code2: " ++ showCode test2
    putStrLn $ "Code3: " ++ showCode test3


{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches #-}
{-# LANGUAGE RankNTypes #-}
module TestRerename (tests) where

import Rerename
import Language.Haskell.TH
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Rerename" $
  [testCase "newName \"x\"" $ assertEqTHSelf $ newName "x",
   testCase "two (newName \"x\")s" $
     assertEqTHSelf $
       [|($(VarE <$> newName "x"), $(VarE <$> newName "x"))|],
   testCase "11" $ assertEqTHSelf [|11|],
   testCase "Nothing" $ assertEqTHSelf [|Nothing|],
   testCase "id" $ assertEqTHSelf [|id|],
   testCase "\\x -> x" $ assertEqTHSelf [|\x -> x|],
   testCase "\\x -> x ≠ \\y -> y" $
     assertNeqTH [|\x -> x|] [|\y -> y|],
   testCase "\\x -> \\x -> x" $
     assertEqTHSelf [|\x -> \x -> x|],

   testCase "x [pattern]" $ assertEqTHSelf [p|x|],
   testCase "Just (x, y) [pattern]" $ assertEqTHSelf [p|Just (x, y)|],

   testCase "Either" $ assertEqTHSelf [t|Either|],
   testCase "forall a b. Either a b" $
     assertEqTHSelf [t|forall a b. Either a b|],

   testCase "f x y = (y, x, x)" $
     assertEqTHSelf [d|f x y = (y, x, x)|],
   testCase "id2 :: a -> a; id2 x = x" $
     -- can't have just a type signature by itself :(
     assertEqTHSelf [d|id2 :: a -> a; id2 x = x|],

   testCase "data Pair a b = Pair a b deriving Eq" $ do
     assertEqTHSelf [d|data Pair a b = Pair a b deriving Eq|]
  ]

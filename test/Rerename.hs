{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Rerename
  (rerename, rerename', eqTH, neqTH, assertEqTH, assertNeqTH, assertEqTHSelf)
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Generics.SYB
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Tasty.HUnit


type Rerename = MonadState RerenameState

data RerenameState =
  RS { nameMap :: Map Name Name, lastIndex :: Map OccName Int }

-- | Replaces the name suffixes from 'newName' or TH quotes in a deterministic
-- way.
--
-- The name bases are still kept so this doesn't make e.g.
-- @\\x -> x@ and @\\y -> y@ equal! But the result of two different instances
-- of @'newName' \"a\"@ or @[|\\x -> x|]@ will be.
rerename :: Data a => a -> a
rerename x = evalState (rerename' x) (RS Map.empty Map.empty)

rerename' :: (Data a, Rerename m) => a -> m a
rerename' = everywhereM $ mkM rerename1

rerename1 :: Rerename m => Name -> m Name
rerename1 n@(Name b (NameU _)) = do -- from newName or [|...|]
  RS {..} <- get
  case Map.lookup n nameMap of
    Just n' -> pure n'
    Nothing -> do
      case Map.lookup b lastIndex of
        Just i -> do
          let n' = mkName $ occString b ++ show i
          modify $ \r -> r {nameMap = Map.insert n n' nameMap}
          pure n'
        Nothing -> do
          let n' = mkName $ occString b
          put $ RS {nameMap   = Map.insert n n' nameMap,
                    lastIndex = Map.insert b 0 lastIndex}
          pure n'
rerename1 n = pure n

infix 4 `eqTH`, `neqTH`  -- same as ==, /=
eqTH, neqTH :: (Eq a, Data a) => a -> a -> Bool
x  `eqTH` y = rerename x == rerename y
x `neqTH` y = not $ x `eqTH` y

assertEqTHSelf :: (Data a, Eq a, Ppr a) => Q a -> Assertion
assertEqTHSelf x = assertEqTH x x

assertEqTH, assertNeqTH :: (Eq a, Data a, Ppr a)
                        => Q a -> Q a -> Assertion
assertEqTH  = assertEqTH' "expected" "but got" eqTH
assertNeqTH = assertEqTH' "first"    "second"  neqTH

assertEqTH' :: Ppr a
            => String -> String -> (a -> a -> Bool)
            -> Q a -> Q a -> Assertion
assertEqTH' mx my p qx qy = do
  x <- runQ qx; y <- runQ qy
  let msg = mx ++ ":\n" ++ indent (pprint x) ++ "\n" ++
            my ++ ":\n" ++ indent (pprint y)
  assertBool msg (x `p` y)

indent :: String -> String
indent = concatMap $ \case '\n' -> "\n    "; c -> [c]

{-# LANGUAGE
    TemplateHaskell, TypeFamilies, PatternSynonyms,
    FlexibleInstances, TypeSynonymInstances, StandaloneDeriving
  #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
import Example1Base

data NoExt

extendA "A" [t|NoExt|] defaultExtA

deriving instance Show a => Show (A a)

main :: IO ()
main = pure () -- print $ (B (A 5) (A 5) :: A Int)

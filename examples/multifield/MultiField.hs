{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
import MultiFieldBase

extendA "A" [] [t|Int|] $
  \a -> defaultExtA {
    typeA = Just [[t|Int|], [t|$a|]],
    typeB = Nothing,
    typeAX = [("C", [[t|Bool|], [t|Char|]])]
  }

foo :: A () -> ()
foo (A u₁ i u₂) = () where _ = (u₁ :: (), i :: Int, u₂ :: ())
foo (C b c)     = () where _ = (b :: Bool, c :: Char)

main :: IO ()
main = pure ()

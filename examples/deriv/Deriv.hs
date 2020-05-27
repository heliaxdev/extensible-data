import Extensible
import DerivBase

data T

extendA "A" [] [t|T|] $
  \a -> defaultExtA {
    typeA = Ann [[t| [$a] |]]
  }

extendB "B" [] [t|T|] $ \_ -> defaultExtB

main :: IO ()
main = print $
  (A (B "") 5 ["a", "b"]) ==
  (A (B "") 5 ["a", "b", "c"] :: A String)
    -- annotation needed until pattern synonyms have
    -- type signatures :/

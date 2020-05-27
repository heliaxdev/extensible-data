import Extensible
import DerivBase

data T

extendA "A" [] [t|T|] $ defaultExtA {
    typeA = Ann [\a -> [t| [$a] |]]
  }

extendB "B" [] [t|T|] $ defaultExtB

main :: IO ()
main = print $
  (A (B "") 5 ["a", "b"]) ==
  (A (B "") 5 ["a", "b", "c"] :: A String)
    -- annotation needed until pattern synonyms have
    -- type signatures :/

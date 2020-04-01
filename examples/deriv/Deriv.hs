import Extensible
import DerivBase

data T

extendA "A" [] [t|T|] $ defaultExtA {
    typeA = Ann $ \a -> [t| [$a] |]
  }

main = print $
  A "" 5 ["a", "b"] ==
  (A "" 5 ["a", "b", "c"] :: A String)
    -- annotation needed until pattern synonyms have
    -- type signatures :/

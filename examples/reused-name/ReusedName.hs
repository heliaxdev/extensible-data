import ReusedNameBase

extendA "A" [] [t|Int|] $
  defaultExtA {
    typeAX = [("B", [("y", [t|String|])])]
  }

main :: IO ()
main = pure ()

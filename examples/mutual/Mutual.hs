import Extensible
import MutualBase

data Ext

extendA "A" [] [t|Ext|] defaultExtA {
  typeAX = [("AI", [t|Int|])]
}

extendB "B" [] [t|Ext|] defaultExtB {
  typeBA = Ann [t|String|]
}

main :: IO ()
main = pure ()

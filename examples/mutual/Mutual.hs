import Extensible
import MutualBase

data Ext

extendA "A" [] [t|Ext|] defaultExtA {
  typeAX = [("AI", [[t|Int|]])]
}

extendB "B" [] [t|Ext|] defaultExtB {
  typeBA = Just [[t|String|]]
}

main :: IO ()
main = pure ()

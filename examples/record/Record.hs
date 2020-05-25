import Extensible
import qualified RecordBase as R
import qualified RecordBase


data WithString

R.extendRec "Rec" [] [t|WithString|] $
  R.defaultExtRec {
    R.typeR1 = Ann ("label1", [t|String|]),
    R.typeR2 = Ann ("label2", [t|String|]),
    R.typeRecX = [("R3", "contents", [t|Int|])]
  }

foo :: Rec -> Rec
foo r = r { R.beep = False, R.extR1 = "aaaaaa" }

main :: IO ()
main = print (R1 {beep = True, boop = 4, label1 = "hello"} :: Rec)
  -- type annotation needed until the pat syns get signatures :/

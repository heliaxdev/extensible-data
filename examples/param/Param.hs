import ParamBase
import Extensible

data With a

do an <- newName "a"
   let a = varT an
   extendT "T" [an] [t|With $a|] $ defaultExtT {
     typeTX = [("Extra", a)]
   }

main :: IO ()
main = pure ()

import qualified QualifiedBase as Q

Q.extendT "T" [] [t|()|] $ Q.defaultExtT {
  Q.typeTX = [("C", [[t|Char|]])]
}

main :: IO ()
main = pure ()

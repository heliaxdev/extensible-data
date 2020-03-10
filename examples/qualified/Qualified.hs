import qualified QualifiedBase

QualifiedBase.extendT "T" [t|()|] $ QualifiedBase.defaultExtT {
  QualifiedBase.typeTX = [("C", [t|Char|])]
}

main = pure ()

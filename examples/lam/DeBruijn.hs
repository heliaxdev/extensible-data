module DeBruijn where
import LamBase

data DeBruijn

extendLam "DBTerm" [] [t|DeBruijn|] $
  \a p -> defaultExtLam {
    typeVar = Nothing, -- replaced with Free and Bound
    typeAbs = Nothing, -- replaced with a version without absVar
    typeLamX =
      [("Free",  [("freeVar", a)]),
       ("Bound", [("boundVar", [t|Int|])]),
       ("Abs",   [("absBody",  [t|Lam' DeBruijn $a $p|])])]
  }

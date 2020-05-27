import LamBase
import Extensible

data NoExt

data Type t =
    Base t
  | Arr (Type t) (Type t)

data Typed t

do t' <- newName "t"; let t = varT t'
   extendLam "TypedLam" [t'] [t|Typed $t|] $
     \a p -> defaultExtLam {
       typeVar = Just [("varType", [t|Type $t|])],
       typeAbs = Just [("absArg",  [t|Type $t|])],
       typeLamX = [("TypeAnn",
          [("annTerm", [t|Lam' (Typed $t) $a $p|]),
           ("annType", [t|Type $t|])])]
     }

main :: IO ()
main = pure ()

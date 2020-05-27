module LamBase where
import Extensible

extensible [d|
  data Lam a p =
      Var {varVar :: a}
    | Prim {primVal :: p}
    | App {appFun, appArg :: Lam a p}
    | Abs {absVar :: a, absBody :: Lam a p}
    deriving (Eq, Show)
  |]

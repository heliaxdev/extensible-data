module TypeSynonymBase where
import Extensible

extensible [d|
    data Exp = App Exp Args | Var String
    data Arg = Arg String Exp
    type Args = [Arg]
  |]

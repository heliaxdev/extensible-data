module QualifiedBase where
import qualified Extensible as Ex

Ex.extensible [d|data T = I Int | B Bool|]

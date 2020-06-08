import Extensible
import Data.Kind

-- example from #13
-- @Foo@ should have @Eq (Threither' ext String Int Bool)@ constraint
extensible [d|
    data Threither a b c = Left a | Mid b | Right c deriving Eq
    data Foo = F (Threither String Int Bool) deriving Eq
  |]

-- should *not* have @Eq a@ constraint
extensible [d|
    data Phantom a = P Int deriving Eq
  |]

-- should have an @Eq (f (Rose' ext f a))@ constraint
extensible [d|
    data Rose (f :: Type -> Type) a = Rose a (f (Rose f a)) deriving Eq
  |]


main :: IO ()
main = pure ()

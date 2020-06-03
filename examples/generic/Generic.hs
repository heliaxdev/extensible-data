{-# LANGUAGE DeriveGeneric #-}
import Extensible
import GHC.Generics

extensible [d| data A a = A a deriving (Eq, Generic) |]

main :: IO ()
main = pure ()

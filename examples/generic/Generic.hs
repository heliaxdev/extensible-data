{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
import Extensible
import GHC.Generics
import Data.Data

extensible [d| data A a = A a deriving (Eq, Data, Generic) |]

main :: IO ()
main = pure ()

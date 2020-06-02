module Main (main) where

import Test.Tasty
import qualified TestRerename

main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [TestRerename.tests]

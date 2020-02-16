{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
import Example1Base

data NoExt

extendA "A" [t|NoExt|] defaultExtA

main = pure () -- print B (A 5) (A 5)

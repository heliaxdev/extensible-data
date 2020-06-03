import TypeSynonymBase

data NoExt

extendExp "Exp" [] [t|NoExt|] defaultExtExp
extendArg "Arg" [] [t|NoExt|] defaultExtArg
type Args = Args' NoExt

main :: IO ()
main = pure ()

# `extensible-data`

Generates (some of) the boilerplate needed for the [Trees That Grow][ttg] pattern. See the [haddock][] documentation for more information.

[ttg]: //www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf
[haddock]: //cryptiumlabs.github.io/extensible-data/Extensible.html

## Caveats

- When importing an extensible datatype qualified, you must (also) import it
  qualified using the original module name, since `extendFoo` generates names
  like `FullModuleName.Constructor`. (Fixing this is probably possible, but
  trickier than I expected.)

  ```haskell
  import qualified Types       -- won't work without this
  import qualified Types as T

  T.extendThing "ThingWithInt" [t|Int|] $ T.defaultThingExt {
    T.typeThingX = [("Int", [t|Int|])]
  }
  ```

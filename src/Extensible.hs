{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveLift, PatternSynonyms, StandaloneDeriving, TemplateHaskell #-}

-- | Generates an extensible datatype from a datatype declaration, roughly
-- following the pattern given by the /Trees that Grow/ paper by Najd and
-- Peyton Jones.
--
--
-- * A type family is generated for each constructor, taking an argument named
--   @ext@ for the extension type, followed by the arguments of the datatype.
--   The names of the type families correspond to the constructors themselves
--   modified with 'annotationName'.
-- * An extra type family is generated with the same arguments, named after the
--   datatype modified with 'extensionName'.
-- * The datatype itself is renamed according to 'datatypeName' and given an
--   extra argument called @ext@ (before the others).
-- * Each existing constructor is renamed according to 'constructorName', and
--   given an extra strict field of the corresponding type family generated
--   above.
-- * An extra constructor is generated for the extension type family (with the
--   same name), containing it as its sole field.
--
-- Note that due to GHC's staging restriction, it is not possible to write
-- @extensible [d| data Foo = ... |]@ and use the generated @extendFoo@ function
-- within the same module.
--
-- Example:
--
-- @
-- module Foo.Base where
-- import E -- FIXME real module name
--
-- extensible [d| data Foo a = Bar a | Baz (Foo a) (Foo Int) |]
--
-- ====>
--
-- type family XBar ext a
-- type family XBaz ext a
-- type family FooX ext a
--
-- data Foo' ext a =
--     Bar' a                           !(XBar ext a)
--   | Baz' (Foo' ext a) (Foo' ext Int) !(XBaz ext a)
--   | FooX !(FooX ext a)
--
-- data ExtFoo = ExtFoo {
--     nameBar  :: String,
--     typeBar  :: Maybe (TypeQ -> TypeQ),
--     nameBaz  :: String,
--     typeBaz  :: Maybe (TypeQ -> TypeQ),
--     typeFooX :: [(String, TypeQ -> TypeQ)]
--   }
--
-- defaultExtFoo :: ExtFoo
-- defaultExtFoo = ExtFoo {
--     nameBar  = "Bar",
--     typeBar  = Just $ \_ -> [t| () |],
--     nameBaz  = "Baz",
--     typeBaz  = Just $ \_ -> [t| () |],
--     typeFooX = []
--   }
--
-- extendFoo :: String -- ^ Type alias name
--           -> TypeQ  -- ^ Tag for this annotation
--           -> ExtFoo
--           -> DecsQ
-- extendFoo = ...
-- @
--
-- @
-- module Foo (module Foo.Base, module Foo) where
-- import Foo.Base
--
-- extendFoo "Foo" [t| () |] $ defaultExtFoo {
--   typeBar = Nothing,
--   extFooX = [("Quux", \_ -> [t|Int|]),
--              ("Zoop", \a -> [t|Foo' () $a|])]
-- }
--
-- ====>
--
-- type instance XBar () a = Void
-- type instance XBaz () a = ()
-- type instance FooX () a = Either Int Bool
--
-- type Foo = Foo' ()
--
-- -- no pattern for Bar
--
-- pattern Baz :: Foo a -> Foo Int -> Foo a
-- pattern Baz x y = Baz' x y ()
--
-- pattern Quux :: Int -> Foo a
-- pattern Quux x = FooX (Left x)
--
-- pattern Zoop :: Foo a -> Foo a
-- pattern Zoop x = FooX (Right x)
--
-- {-# COMPLETE Baz, Quux, Zoop #-}
-- @
module Extensible
  (-- * Name manipulation
   NameAffix (.., NamePrefix, NameSuffix), applyAffix,
   -- * Generating extensible datatypes
   extensible, extensibleWith, Config (..), defaultConfig, ConAnn(..))
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Generics.SYB (everywhere, mkT)
import Control.Monad
import Data.Functor.Identity
import Data.Void

-- ☹
deriving instance Lift Name
deriving instance Lift OccName
deriving instance Lift NameFlavour
deriving instance Lift ModName
deriving instance Lift NameSpace
deriving instance Lift PkgName

-- | Extra strings to add to the beginning and/or end of (the base part of)
-- 'Name's
data NameAffix =
  NameAffix {naPrefix, naSuffix :: String}
  deriving (Eq, Show, Lift)
pattern NamePrefix, NameSuffix :: String -> NameAffix
pattern NamePrefix pre = NameAffix {naPrefix = pre, naSuffix = ""}
pattern NameSuffix suf = NameAffix {naPrefix = "",  naSuffix = suf}

instance Semigroup NameAffix where
  NameAffix pre1 suf1 <> NameAffix pre2 suf2 =
    NameAffix (pre1 <> pre2) (suf2 <> suf1)
instance Monoid NameAffix where mempty = NameAffix "" ""

onNameBase :: Functor f => (String -> f String) -> Name -> f Name
onNameBase f name = addModName <$> f (nameBase name) where
  addModName b = mkName $ case nameModule name of
    Nothing -> b
    Just m  -> m ++ "." ++ b

applyAffix :: NameAffix -> Name -> Name
applyAffix (NameAffix pre suf) =
  runIdentity . onNameBase (\b -> Identity $ pre ++ b ++ suf)


-- | Configuration options for how to name the generated constructors, type
-- families, etc.
data Config = Config {
    -- | Applied to input datatype's name to get extensible type's name
    datatypeName :: NameAffix,
    -- | Appled to constructor names
    constructorName :: NameAffix,
    -- | Appled to constructor names to get the annotation name
    annotationName :: NameAffix,
    -- | Applied to datatype name to get extension constructor
    extensionName :: NameAffix,
    -- | Applied to datatype name to get extension record name
    extRecordName :: NameAffix,
    -- | Applied to constructor names to get the names of the type fields in the
    -- extension record
    extRecTypeName :: NameAffix,
    -- | Applied to constructor names to get the names of the name fields in the
    -- extension record (which are used to name the pattern synonyms)
    extRecNameName :: NameAffix,
    -- | Applied to the 'extRecordName' to get the name of the default extension
    defExtRecName :: NameAffix,
    -- | Applied to datatype name to get the name of the extension
    -- generator function
    extFunName :: NameAffix
  } deriving (Eq, Show, Lift)

-- | Default config:
--
-- @
-- Config {
--   datatypeName    = NameSuffix "'",
--   constructorName = NameSuffix "'",
--   annotationName  = NamePrefix "X",
--   extensionName   = NameSuffix "X",
--   extRecordName   = NamePrefix "Ext",
--   extRecTypeName  = NamePrefix "type",
--   extRecNameName  = NamePrefix "name",
--   defExtRecName   = NamePrefix "default",
--   extFunName      = NamePrefix "extend"
-- }
-- @
defaultConfig :: Config
defaultConfig = Config {
    datatypeName    = NameSuffix "'",
    constructorName = NameSuffix "'",
    annotationName  = NamePrefix "X",
    extensionName   = NameSuffix "X",
    extRecordName   = NamePrefix "Ext",
    extRecTypeName  = NamePrefix "type",
    extRecNameName  = NamePrefix "name",
    defExtRecName   = NamePrefix "default",
    extFunName      = NamePrefix "extend"
  }


-- | An annotation for a constructor. @t@ is @'TypeQ' -> ... -> 'TypeQ'@ with
-- one argument for each type variable in the original datatype declaration.
--
-- * 'Ann': the annotation is the given type
-- * 'NoAnn': no annotation (filled in with '()' autmatically by the pattern
--   synonym)
-- * 'Disabled': constructor disabled (annotation type is 'Void' and no pattern
--   synonym generated)
data ConAnn t = Ann t | NoAnn | Disabled

-- | Transforms a 'ConAnn' to a @['TypeQ' -> ... -> 'TypeQ']@; the input 'Int'
-- is the number of type arguments (so that a lambda of the right shape can be
-- generated in the 'NoAnn' case).
conAnnToList :: Int -> ExpQ
conAnnToList n = [| \ann -> case ann of
    Ann t    -> [t]
    NoAnn    -> [$(lamE (replicate n wildP) [|tupleT 0|])]
    Disabled -> []
  |]


-- | A \"simple\" constructor (non-record, non-GADT)
data SimpleCon = SimpleCon {
    scName   :: Name,
    scFields :: [BangType]
  } deriving (Eq, Show)

-- | A \"simple\" datatype (no context, no kind signature, no deriving)
data SimpleData = SimpleData {
    sdName :: Name,
    sdVars :: [TyVarBndr],
    sdCons :: [SimpleCon]
  } deriving (Eq, Show)

-- | Extract a 'SimpleData' from a 'Dec', if it is a datatype with the given
-- restrictions.
simpleData :: Dec -> Q SimpleData
simpleData (DataD ctx name tvs kind cons derivs)
  | _:_    <- ctx    = fail "data contexts unsupported"
  | Just _ <- kind   = fail "kind signatures unsupported"
  | _:_    <- derivs = fail "deriving unsupported"
  | otherwise        = SimpleData name tvs <$> traverse simpleCon cons
simpleData _ = fail "not a datatype"

-- | Extract a 'SimpleCon' from a 'Con', if it is the 'NormalC' case.
simpleCon :: Con -> Q SimpleCon
simpleCon (NormalC name fields) = pure $ SimpleCon name fields
simpleCon _ = fail "only simple constructors supported for now"


-- | As 'extensibleWith', using 'defaultConfig'.
extensible :: DecsQ -> DecsQ
extensible = extensibleWith defaultConfig

extensibleWith :: Config -> DecsQ -> DecsQ
extensibleWith conf ds =
    fmap concat . traverse (makeExtensible conf <=< simpleData) =<< ds

tyvarName :: TyVarBndr -> Name
tyvarName (PlainTV  x)   = x
tyvarName (KindedTV x _) = x

makeExtensible :: Config -> SimpleData -> DecsQ
makeExtensible conf (SimpleData name tvs cs) = do
  let name' = applyAffix (datatypeName conf) name
  ext <- newName "ext"
  let tvs' = PlainTV ext : tvs
  cs' <- traverse (extendCon conf name name' ext tvs) cs
  let cx = extensionCon conf name ext tvs
  efs <- traverse (extendFam conf tvs) cs
  efx <- extensionFam conf name tvs
  (rname, fcnames, fname, rec) <- extRecord conf name tvs cs
  (_dname, defRec) <- extRecDefault conf rname fcnames fname tvs
  (_ename, extFun) <- makeExtender conf name rname tvs cs
  return $
    DataD [] name' tvs' Nothing (cs' ++ [cx]) [] :
    efs ++ [efx, rec] ++ defRec ++ extFun ++
    []

nonstrict :: Bang
nonstrict = Bang NoSourceUnpackedness NoSourceStrictness

strict :: Bang
strict = Bang NoSourceUnpackedness SourceStrict

appExtTvs :: Type -> Name -> [TyVarBndr] -> Type
appExtTvs t ext tvs = foldl AppT t $ fmap VarT $ ext : fmap tyvarName tvs

extendCon :: Config -> Name -> Name -> Name -> [TyVarBndr]
          -> SimpleCon -> ConQ
extendCon conf cname cname' ext tvs (SimpleCon name fields) = do
  let name' = applyAffix (constructorName conf) name
      xname = applyAffix (annotationName conf) name
      fields' = map (extendRec cname cname' ext) fields
  pure $ NormalC name' $
    fields' ++ [(strict, appExtTvs (ConT xname) ext tvs)]

extendRec :: Name -> Name -> Name -> (Bang, Type) -> (Bang, Type)
extendRec cname cname' ext = everywhere $ mkT go where
  go (ConT k) | k == cname = ConT cname' `AppT` VarT ext
  go t = t

extensionCon :: Config -> Name -> Name -> [TyVarBndr] -> Con
extensionCon conf name ext tvs =
  let namex = applyAffix (extensionName conf) name in
  NormalC namex [(strict, appExtTvs (ConT namex) ext tvs)]

extendFam :: Config -> [TyVarBndr] -> SimpleCon -> DecQ
extendFam conf tvs (SimpleCon name _) =
  extendFam' (applyAffix (annotationName conf) name) tvs

extensionFam :: Config -> Name -> [TyVarBndr] -> DecQ
extensionFam conf name tvs =
  extendFam' (applyAffix (extensionName conf) name) tvs

extendFam' :: Name -> [TyVarBndr] -> DecQ
extendFam' name tvs = do
  ext <- newName "ext"
  pure $ OpenTypeFamilyD $ TypeFamilyHead name (PlainTV ext : tvs) NoSig Nothing

-- | returns, in order:
--
-- * record name
-- * constructor annotation field names
-- * extension constructor field name
-- * record declaration to splice
extRecord :: Config -> Name -> [TyVarBndr] -> [SimpleCon]
          -> Q (Name, [(Name, Name, String)], Name, Dec)
extRecord conf cname tvs cs = do
  let rname = applyAffix (extRecordName conf) cname
      conann_  t = [t| ConAnn $t |]
      lblList_ t = [t| [(String, $t)] |]
  tfields  <- traverse (extRecTypeField conann_ conf tvs . scName) cs
  nfields  <- traverse (extRecNameField conf . scName) cs
  extField <- extRecTypeField lblList_ conf tvs
                (applyAffix (extensionName conf) cname)
  pure (rname,
        zip3 (map fieldName tfields)
             (map fieldName nfields)
             (map (nameBase . scName) cs),
        fieldName extField,
        DataD [] rname [] Nothing
          [RecC rname (tfields ++ nfields ++ [extField])] [])
 where
  fieldName (n, _, _) = n

extRecTypeField :: (TypeQ -> TypeQ)
                -> Config -> [TyVarBndr] -> Name -> VarBangTypeQ
extRecTypeField f conf tvs name = do
  let fname = applyAffix (extRecTypeName conf) name
  ty <- f (mkTy tvs)
  pure (fname, nonstrict, ty)
 where
  mkTy []     = [t|TypeQ|]
  mkTy (_:xs) = [t|TypeQ -> $(mkTy xs)|]

extRecNameField :: Config -> Name -> VarBangTypeQ
extRecNameField conf name = do
  let fname = applyAffix (extRecNameName conf) name
  ty <- [t|String|]
  pure (fname, nonstrict, ty)

extRecDefault :: Config -> Name -> [(Name, Name, String)] -> Name
              -> [TyVarBndr] -> Q (Name, [Dec])
extRecDefault conf rname fcnames fname tvs = do
  let mkField (t, n, c) = [fieldExp t [|NoAnn|], fieldExp n (stringE c)]
      fields = concatMap mkField fcnames
      xfield = fieldExp fname [| [] |]
      dname = applyAffix (defExtRecName conf) rname
  defn <- valD (varP dname) (normalB (recConE rname (fields ++ [xfield]))) []
  pure (dname, [SigD dname (ConT rname), defn])

-- | Generate the @extendX@ function, which 
makeExtender :: Config -> Name -> Name -> [TyVarBndr] -> [SimpleCon]
             -> Q (Name, [Dec])
makeExtender conf name rname tvs cs = do
  let ename = applyAffix (extFunName conf) name
  sig  <- sigD ename [t|String -> TypeQ -> $(conT rname) -> DecsQ|]
  syn  <- newName "syn"
  tag  <- newName "tag"
  exts <- newName "exts"
  defn <- [|sequence $ concat $(listE $
              map (decsForCon conf tag exts tvs) cs ++
              [decsForExt conf tag exts tvs name,
               makeTySyn conf name syn tag])|]
  let val = FunD ename [Clause [VarP syn, VarP tag, VarP exts] (NormalB defn) []]
  pure (ename, [sig, val])

-- | See 'decsFor''; this is the case for a real constructor
decsForCon :: Config -> Name -> Name -> [TyVarBndr] -> SimpleCon -> ExpQ
decsForCon conf tag exts tvs (SimpleCon name _) =
  decsFor' (conAnnToList (length tvs)) tag exts tvs
    (applyAffix (annotationName conf) name)
    (applyAffix (extRecTypeName conf) name)
    (applyAffix (extRecNameName conf) name)

-- | See 'decsFor''; this is the case for an extension constructor
decsForExt :: Config -> Name -> Name -> [TyVarBndr] -> Name -> ExpQ
decsForExt conf tag exts tvs name =
  let namex = applyAffix (extensionName conf) name in
  decsFor' [|map snd|] tag exts tvs
    namex
    (applyAffix (extRecTypeName conf) namex)
    (applyAffix (extRecNameName conf) namex)

-- | Generates a TH expression which when spliced gives the declarations for
-- extending a constructor (a type family instance and possibly a pattern
-- synonym)
decsFor' :: ExpQ -- ^ convert the @typeX@ field to @[TypeQ -> ... -> TypeQ]@
         -> Name -- ^ the name of @extendX@'s @tag@ argument
         -> Name -- ^ the name of @extendX@'s @exts@ argument
         -> [TyVarBndr] -- ^ the datatype's type variables
         -> Name -- ^ the name of the type family
         -> Name -- ^ the name of the @typeX@ field
         -> Name -- ^ the name of the @nameX@ field
         -> ExpQ
decsFor' toList tag exts tvs namex tfield nfield =
  listE $ [tyInst] ++ patSyn
 where
  tyInst = do
    let tvs'    = map ((\x -> [|varT x|]) . tyvarName) tvs
        famArgs = listE (varE tag : tvs')
        void_   = ''Void
        either_ = ''Either
    [|tySynInstD $ tySynEqn Nothing
        (foldl appT (conT namex) $famArgs)
        (case ($toList ($(varE tfield) $(varE exts))) of
           [] -> conT void_
           fs -> foldr1 mkEither $ map appTvs fs
             where mkEither t u = conT either_ `appT` t `appT` u
                   appTvs f     = $(appsE $ [|f|] : tvs'))
     |]
  patSyn = do
    []

makeTySyn :: Config -> Name -> Name -> Name -> ExpQ
makeTySyn conf name syn tag =
  let tyname = applyAffix (datatypeName conf) name in
  [|[tySynD (mkName $(varE syn)) [] (appT (conT tyname) $(varE tag))]|]

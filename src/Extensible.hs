{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    CPP, DeriveDataTypeable, DeriveLift, PatternSynonyms, StandaloneDeriving,
    TemplateHaskell
  #-}

-- | #maindoc#
-- Generates an extensible datatype from a datatype declaration, roughly
-- following the pattern given by the /Trees that Grow/ paper by Najd and
-- Peyton Jones.
--
--
-- * A type family is generated for each constructor, taking an argument named
--   @ext@ for the extension type, followed by the arguments of the datatype.
--   The names of the type families correspond to the constructors themselves
--   modified with 'annotationName' (see @<#XVar XVar>@ etc below).
-- * An extra type family is generated with the same arguments, named after the
--   datatype modified with 'extensionName' (see @<#LamX LamX>@).
-- * The datatype itself is renamed according to 'datatypeName' and given an
--   extra argument called @ext@ (before the others).
-- * Each existing constructor is renamed according to 'constructorName', and
--   given an extra strict field of the corresponding type family generated
--   above.
-- * An extra constructor is generated for the extension type family (with the
--   same name), containing it as its sole field (see @<#Lam' Lam'>@ for the
--   transformation).
-- * A constraint synonym is generated, named according to 'bundleName', which
--   contains a constraint for each extension (see @<#LamAll LamAll>@).
-- * A record and TH function are generated for creating new extensions of the
--   base datatype (see @<#ExtLam ExtLam>@ and @<#extendLam extendLam>@).
-- * A standalone @deriving@ declaration is generated for each derived instance
--   listed.
--
-- = Known bugs and shortcomings
--
-- * Due to GHC's staging restriction, a Template Haskell function cannot be
--   spliced in the same module as it is defined. That means it is not possible
--   to write @'extensible' [d| data Foo = ... |]; extendFoo ...@ within the
--   same module.
-- * When using qualified imports, the module containing @extendFoo@ must be
--   imported using its real name. It can also be imported using an alias if
--   desired, e.g.
--   @import qualified LongName; import qualified LongName as L@.
-- * The same record label cannot be used for multiple different constructors.
--   (The @DuplicateRecordFields@ extension doesn't seem to lift this
--   restriction with pattern synonyms.)
-- * Pattern synonyms do not yet get type annotations, which means that GHC
--   cannot always work out which variant of the type you want. You will
--   probably also want to disable the warning in modules calling @extendFoo@
--   until this is fixed (e.g. with
--   @{-\# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures \#-}@).
--
-- * The @deriving@ supported is quite limited compared to full GHC:
--
--     * Only @stock@ and @anyclass@ strategies are supported.
--     * __The context is not calculated properly like a real deriving clause__.
--       Instead, a constraint of the given class is required for each type
--       variable and each extension. If this doesn't work (e.g. you want to
--       derive 'Eq' but have a type variable of kind @'K.Type' -> 'K.Type'@),
--       you must instead write your own declaration outside of the call to
--       'extensible'.
--     * Deriving for non-regular datatypes (datatypes with recursive
--       occurrences applied to different types) doesn't work.
--
-- = Language extensions
--
-- The module where @extensible@ is called needs the extensions
-- @TemplateHaskell@, @TypeFamilies@, @FlexibleContexts@,
-- @UndecidableInstances@, @ConstraintKinds@, @KindSignatures@,
-- @StandaloneDeriving@ to be enabled.
--
-- Modules calling @extendFoo@ need @TemplateHaskell@, @TypeFamilies@,
-- @PatternSynonyms@.
--
--
-- = Example
--
-- @
-- module Base where    #Base#
-- import Extensible
--
-- extensible [d| #LamOrig#
--   data Lam a p =
--       Var {varVar :: a}
--     | Prim {primVal :: p}
--     | App {appFun, appArg :: Lam a p}
--     | Abs {absVar :: a, absBody :: Lam a p}
--     deriving (Eq, Show)
--   |]
--
-- ====>
--
-- -- type families for each constructor, and one for adding additional ones
-- type family XVar  ext a p    #XVar#
-- type family XPrim ext a p    #XPrim#
-- type family XApp  ext a p    #XApp#
-- type family XAbs  ext a p    #XAbs#
-- type family LamX  ext a p    #LamX#
--
-- data Lam' ext a p = #Lam'#
--     Var' {                                 #Var'#
--       varVar :: a,                         #varVar#
--       extVar :: !(<#XVar XVar> ext a p)    #extVar#
--         -- each constructor gets a slot for extra fields
--     }
--   | Prim' {                          #Prim'#
--       primVal :: p,                  #primVal#
--       extPrim :: !(XPrim ext a p)    #extPrim#
--     }
--   | App' {                                 #App'#
--       appFun, appArg :: Lam' ext a p,      #appFun# #appArg#
--         -- recursive occurrences are dealt with
--       extApp :: !(<#XApp XApp> ext a p)    #extApp#
--     }
--   | Abs' {                                 #Abs'#
--       absVar :: a p,                       #absVar#
--       absBody :: Lam' ext a p,             #absBody#
--       extAbs :: !(<#XLam XLam> ext a p)    #extAbs#
--     }
--   | LamX { -- a constructor for extensions      #LamX#
--       extLam :: !(<#LamX LamX> ext a p)         #extLam#
--     }
--
-- type LamAll (c :: 'K.Type' -> 'K.Constraint') ext a =    #LamAll#
--   (c (<#XVar XVar> ext a), c (<#XPrim XPrim> ext a),
--    c (<#XApp XApp> ext a), c (<#XAbs XAbs> ext a),
--    c (<#LamX LamX> ext a))
--
-- -- deriving clauses transformed to standalone deriving
-- deriving instance ('Eq'   a, <#LamAll LamAll> 'Eq'   ext a) => 'Eq'   (<#Lam' Lam'> ext a)
-- deriving instance ('Show' a, <#LamAll LamAll> 'Show' ext a) => 'Show' (<#Lam' Lam'> ext a)
--
-- -- a description of an extension
-- -- (don't rely on the field order; use record syntax instead)
-- data ExtLam =                                                         #ExtLam#
--   ExtLam {
--     -- rename the Var constructor
--     nameVar :: 'String',                                              #nameVar#
--
--     -- a list of extra field names and types for Var
--     -- * for a non-record, this is a 'Maybe' ['TypeQ'] instead
--     -- * 'Nothing' disables the constructor
--     typeVar :: 'Maybe' [('String', 'TypeQ')],                         #typeVar#
--
--     -- same for the others
--     namePrim :: 'String', typePrim :: 'Maybe' [('String', 'TypeQ')],  #namePrim# #typePrim#
--     nameApp  :: 'String', typeApp  :: 'Maybe' [('String', 'TypeQ')],  #nameApp#  #typeApp#
--     nameAbs  :: 'String', typeAbs  :: 'Maybe' [('String', 'TypeQ')],  #nameAbs#  #typeAbs#
--
--     -- extra constructors, their names & fields
--     -- * multiple are possible, represented with nested 'Either'
--     -- * extensions are records because all of the proper constructors are
--     -- * otherwise, has type [('String', ['TypeQ'])]
--     typeLamX :: [('String', [('String', 'TypeQ')])]                   #typeLamX#
--   }
--
-- -- no extensions (reproduces the input datatype)
-- defaultExtLam :: <#ExtLam ExtLam>    #defaultExtLam#
-- defaultExtLam =
--   <#ExtLam ExtLam> {
--     <#nameVar  nameVar>  = \"Var\",  <#typeVar  typeVar>  = 'Just' [],
--     <#namePrim namePrim> = \"Prim\", <#typePrim typePrim> = 'Just' [],
--     <#nameApp  nameApp>  = \"App\",  <#typeApp  typeApp>  = 'Just' [],
--     <#nameAbs  nameAbs>  = \"Abs\",  <#typeAbs  typeAbs>  = 'Just' [],
--     <#typeLamX typeLamX> = []
--   }
--
-- -- produces an extended datatype; see below for details
-- extendLam :: 'String' -- ^ extended type's name    #extendLam#
--           -> ['Name'] -- ^ extra type variables, if needed
--           -> 'TypeQ'  -- ^ tag for this variant of the type
--                     --   (the \"ext\" parameter; should contain the above vars)
--           -> ('TypeQ' -> 'TypeQ' -> <#ExtLam ExtLam>)
--                     -- ^ description of extension
--                     --   (input is <#Lam Lam>'s type variables a and p)
--           -> 'DecsQ'
-- extendLam = ...
-- @
--
-- == De Bruijn terms
--
-- @
-- import <#Base Base>
--
-- data DeBruijn    #DeBruijn#
--
-- <#extendLam extendLam> \"DBTerm\" [] [t|<#DeBruijn DeBruijn>|] $
--   -- \"a\" and \"p\" are <#Lam Lam>'s type parameters
--   \\a p -> <#defaultExtLam defaultExtLam> {
--     <#typeVar typeVar> = 'Nothing', -- replaced with Free and Bound
--     <#typeAbs typeAbs> = 'Nothing', -- replaced with a version without absVar
--     <#typeLamX typeLamX> =
--       [(\"Free\",  [(\"freeVar\",  a)]),
--        (\"Bound\", [(\"boundVar\", [t|'Int'|])]),
--        (\"Abs\",   [(\"absBody\",  [t|<#Lam' Lam'> <#DeBruijn DeBruijn> $a $p|])])]
--          -- (we have to say Lam' DeBruijn here because
--          --  the DBTerm alias doesn't exist yet)
--   }
--
-- ====>
--
-- type instance <#XVar XVar> <#DeBruijn DeBruijn> a p = 'Void'
--
-- type instance <#XPrim XPrim> <#DeBruijn DeBruijn> a p = ()
-- pattern Prim {primVal} = <#Prim' Prim'> primVal ()
--
-- type instance <#XApp XApp> <#DeBruijn DeBruijn> a p = ()
-- pattern App {appFun, appArg} = <#App' App'> appFun appArg ()
--
-- type instance <#XAbs XAbs> <#DeBruijn DeBruijn> a p = 'Void'
--
-- type instance LamX DeBruijn a p =
--   'Either' a                  -- Free
--     ('Either' 'Int'             -- Bound
--        (<#Lam' Lam'> <#DeBruijn DeBruijn> a p)) -- Abs
-- pattern Free  {freeVar}  = <#LamX LamX> ('Left'         freeVar)
-- pattern Bound {boundVar} = <#LamX LamX> ('Right' ('Left'  boundVar))
-- pattern Abs   {absBody}  = <#LamX LamX> ('Right' ('Right' absBody))
--
-- {-\# COMPLETE Prim, App, Free, Bound, Abs \#-}
-- @
--
-- == Type-annotated terms
--
-- @
-- import <#Base Base>
-- import Extensible
--
-- data Type t =                #Type#
--     Base t                   #Base#
--   | Arr (Type t) (Type t)    #Arr#
--
-- data Typed t    #Typed#
--
-- do -- create a new type variable for <#Typed Typed>
--    -- ('newName' and 'varT' are reexported from TH by Extensible)
--    t' <- 'newName' \"t\"; let t = 'varT' t'
--    <#extendLam extendLam> \"TypedLam\" [t'] [t|<#Typed Typed> $t|] $
--      \\a p -> <#defaultExtLam defaultExtLam> {
--        <#typeVar typeVar> = 'Just' [(\"varType\", [t|<#Type Type> $t|])],
--        <#typeAbs typeAbs> = 'Just' [(\"absArg\",  [t|<#Type Type> $t|])],
--        <#typeLamX typeLamX> = [(\"TypeAnn\",
--           [(\"annTerm\", [t|<#Lam' Lam'> (<#Typed Typed> $t) $a $p|]),
--            (\"annType\", [t|<#Type Type> $t|])])]
--      }
--
-- ====>
--
-- type TypedLam t = Lam' (Typed t)    #TypedLam#
--
-- type instance <#XVar XVar> (<#Typed Typed> t) a p = <#Type Type> t
-- pattern Var {varVar, varType} = <#Var' Var'> varVar varType
--
-- type instance <#XPrim XPrim> (<#Typed Typed> t) a p = ()
-- pattern Prim {primVal} = <#Prim' Prim'> primVal ()
--
-- type instance <#XApp XApp> (<#Typed Typed> t) a p = ()
-- pattern App {appFun, appArg} = <#App' App'> appFun appArg ()
--
-- type instance <#XAbs XAbs> (<#Typed Typed> t) a p = <#Type Type> t
-- pattern Abs {absVar, absBody, absArg} = <#Abs' Abs'> absVar absBody absArg
--
-- type instance <#LamX LamX> (<#Typed Typed> t) a p = (<#Lam' Lam'> (<#Typed Typed> t) a p, <#Type Type> t)
-- pattern TypeAnn {annTerm, annType} = <#LamX LamX> (annTerm, annType)
--
-- {-\# COMPLETE Var, Prim, App, Abs, TypeAnn \#-}
-- @

module Extensible
  (-- * Name manipulation
   NameAffix (.., NamePrefix, NameSuffix), applyAffix,
   -- ** Template Haskell re-exports
   newName, varT,
   -- * Generating extensible datatypes
   extensible, extensibleWith, Config (..), defaultConfig)
where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Generics.SYB (Data, everywhere, mkT)
import Control.Monad
import Data.Functor.Identity
import Data.Void
import Data.Kind as K

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
-- | Just a prefix, with an empty suffix
pattern NamePrefix pre = NameAffix {naPrefix = pre, naSuffix = ""}
-- | Just a suffix, with an empty prefix
pattern NameSuffix suf = NameAffix {naPrefix = "",  naSuffix = suf}

instance Semigroup NameAffix where
  NameAffix pre1 suf1 <> NameAffix pre2 suf2 =
    NameAffix (pre1 <> pre2) (suf2 <> suf1)
instance Monoid NameAffix where mempty = NameAffix "" ""

onNameBaseF :: Functor f => (String -> f String) -> Name -> f Name
onNameBaseF f name = addModName <$> f (nameBase name) where
  addModName b = mkName $ case nameModule name of
    Nothing -> b
    Just m  -> m ++ "." ++ b

onNameBase :: (String -> String) -> Name -> Name
onNameBase f = runIdentity . onNameBaseF (Identity . f)

-- |
-- >>> applyAffix (NameAffix "pre" "Suf") (mkName "Foo")
-- preFooSuf
-- >>> applyAffix (NameAffix "pre" "Suf") (mkName "Foo.Bar")
-- Foo.preBarSuf
applyAffix :: NameAffix -> Name -> Name
applyAffix (NameAffix pre suf) = onNameBase (\b -> pre ++ b ++ suf)


-- | Qualified a name with a module, /unless/ it is already qualified.
--
-- >>> qualifyWith "Mod" (mkName "foo")
-- Mod.foo
-- >>> qualifyWith "Mod" (mkName "OtherMod.foo")
-- OtherMod.foo
qualifyWith :: String -> Name -> Name
qualifyWith m n = case nameModule n of
  Nothing -> mkName (m ++ "." ++ nameBase n)
  Just _  -> n


-- | Configuration options for how to name the generated constructors, type
-- families, etc.
data Config = Config {
    -- | Applied to input datatype's name to get extensible type's name
    datatypeName :: NameAffix,
    -- | Applied to input constructor names to get extensible constructor names
    constructorName :: NameAffix,
    -- | Applied to type name to get constraint bundle name
    bundleName :: NameAffix,
    -- | Appled to constructor names to get the annotation type family's name
    annotationName :: NameAffix,
    -- | Applied to datatype name to get extension constructor & type family's
    -- name
    extensionName :: NameAffix,
    -- | If extending a record constructor, apply this to the constructor name
    -- to get the extension field's label.
    extensionLabel :: NameAffix,
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
--   datatypeName    = NameSuffix \"'\",
--   constructorName = NameSuffix \"'\",
--   bundleName      = NameSuffix "All",
--   annotationName  = NamePrefix \"X\",
--   extensionName   = NameSuffix \"X\",
--   extensionLabel  = NamePrefix \"ext\",
--   extRecordName   = NamePrefix \"Ext\",
--   extRecTypeName  = NamePrefix \"type\",
--   extRecNameName  = NamePrefix \"name\",
--   defExtRecName   = NamePrefix \"default\",
--   extFunName      = NamePrefix \"extend\"
-- }
-- @
defaultConfig :: Config
defaultConfig = Config {
    datatypeName    = NameSuffix "'",
    constructorName = NameSuffix "'",
    bundleName      = NameSuffix "All",
    annotationName  = NamePrefix "X",
    extensionName   = NameSuffix "X",
    extensionLabel  = NamePrefix "ext",
    extRecordName   = NamePrefix "Ext",
    extRecTypeName  = NamePrefix "type",
    extRecNameName  = NamePrefix "name",
    defExtRecName   = NamePrefix "default",
    extFunName      = NamePrefix "extend"
  }


-- | A \"simple\" constructor (non-record, non-GADT)
data SimpleCon = SimpleCon {
    scName   :: Name,
    scFields :: SimpleFields
  } deriving (Eq, Show, Data)

data SimpleFields = NormalFields [BangType] | RecFields [VarBangType]
  deriving (Eq, Show, Data)

-- | A \"simple\" datatype (no context, no kind signature, no deriving)
data SimpleData = SimpleData {
    sdName   :: Name,
    sdVars   :: [TyVarBndr],
    sdCons   :: [SimpleCon],
    sdDerivs :: [SimpleDeriv]
  } deriving (Eq, Show, Data)

-- 'SBlank' and 'SStock' have the same effect but the first will trigger
-- @-Wmissing-deriving-strategies@ if it is enabled and the second requires
-- the @DerivingStrategies@ extension
data SimpleStrategy = SBlank | SStock | SAnyclass deriving (Eq, Show, Data)

-- | A \"simple\" deriving clause—either @stock@ or @anyclass@ strategy
data SimpleDeriv =
  SimpleDeriv {
    sdStrat   :: SimpleStrategy,
    dsContext :: Cxt
  } deriving (Eq, Show, Data)

-- | Extract a 'SimpleData' from a 'Dec', if it is a datatype with the given
-- restrictions.
simpleData :: Dec -> Q SimpleData
simpleData (DataD ctx name tvs kind cons derivs)
  | not $ null ctx    = fail "data contexts unsupported"
  | Just _ <- kind    = fail "kind signatures unsupported"
  | otherwise =
      SimpleData name tvs
        <$> traverse simpleCon cons
        <*> traverse simpleDeriv derivs
simpleData _ = fail "not a datatype"

-- | Extract a 'SimpleCon' from a 'Con', if it is the 'NormalC' case.
simpleCon :: Con -> Q SimpleCon
simpleCon (NormalC name fields) = pure $ SimpleCon name $ NormalFields fields
simpleCon (RecC    name fields) = pure $ SimpleCon name $ RecFields    fields
simpleCon _ = fail "only simple constructors supported for now"

simpleDeriv :: DerivClause -> Q SimpleDeriv
simpleDeriv (DerivClause strat prds) =
  SimpleDeriv <$> simpleStrat strat <*> pure prds
 where
  simpleStrat Nothing                 = pure SBlank
  simpleStrat (Just StockStrategy)    = pure SStock
  simpleStrat (Just AnyclassStrategy) = pure SAnyclass
  simpleStrat (Just NewtypeStrategy)  = fail "newtype deriving unsupported"
  simpleStrat (Just (ViaStrategy _))  = fail "deriving via unsupported"

-- | As 'extensibleWith', using 'defaultConfig'.
extensible :: DecsQ -> DecsQ
extensible = extensibleWith defaultConfig

-- | Generate an extensible datatype using the given 'Config' for creating
-- names. See <#maindoc the module documentation> for more detail on what this
-- function spits out.
extensibleWith :: Config -> DecsQ -> DecsQ
extensibleWith conf ds = do
  ds'  <- traverse simpleData =<< ds
  home <- loc_module <$> location
  makeExtensible conf home ds'

tyvarName :: TyVarBndr -> Name
tyvarName (PlainTV  x)   = x
tyvarName (KindedTV x _) = x

tvbToTypeExp :: TyVarBndr -> ExpQ
tvbToTypeExp tv = [|varT $(lift $ tyvarName tv)|]

isRecordFields :: SimpleFields -> Bool
isRecordFields (NormalFields {}) = False
isRecordFields (RecFields    {}) = True

isRecordCon :: SimpleCon -> Bool
isRecordCon = isRecordFields . scFields

extIsRecord :: [SimpleCon] -> Bool
extIsRecord = all isRecordCon

makeExtensible :: Config
               -> String -- ^ module where @extensible{With}@ was called
               -> [SimpleData] -> DecsQ
makeExtensible conf home datas =
  let nameMap = [(name, applyAffix (datatypeName conf) name)
                  | SimpleData {sdName = name} <- datas]
  in concat <$> mapM (makeExtensible1 conf home nameMap) datas

makeExtensible1 :: Config
                -> String -- ^ module where @extensible{With}@ was called
                -> [(Name, Name)] -- ^ mapping @(old, new)@ for datatype names
                -> SimpleData -> DecsQ
makeExtensible1 conf home nameMap (SimpleData name tvs cs derivs) = do
  let name' = applyAffix (datatypeName conf) name
  ext <- newName "ext"
  let tvs' = PlainTV ext : tvs
  cs' <- traverse (extendCon conf nameMap ext tvs) cs
  let cx = extensionCon conf (extIsRecord cs) name ext tvs
  efs <- traverse (extendFam conf tvs) cs
  efx <- extensionFam conf name tvs
  bnd <- constraintBundle conf name ext tvs cs
  insts <- fmap concat $
    traverse (makeInstances conf name' (map fst nameMap) ext tvs) derivs
  (rname, fcnames, fname, rec) <- extRecord conf name cs
  (_dname, defRec) <- extRecDefault conf rname fcnames fname
  (_ename, extFun) <- makeExtender conf home name rname tvs cs
  return $
    DataD [] name' tvs' Nothing (cs' ++ [cx]) [] :
    efs ++ [efx, bnd] ++ insts ++ [rec] ++ defRec ++ extFun

nonstrict :: BangQ
nonstrict = bang noSourceUnpackedness noSourceStrictness

strict :: Bang
strict = Bang NoSourceUnpackedness SourceStrict

-- | @appExtTvs t ext tvs@ applies @t@ to @ext@ and then to all of @tvs@.
appExtTvs :: TH.Type -> Name -> [TyVarBndr] -> TH.Type
appExtTvs t ext tvs = foldl AppT t $ fmap VarT $ ext : fmap tyvarName tvs

-- | Generate an extended constructor by renaming it, replacing recursive
-- occurrences of the datatype, and adding an extension field at the end
extendCon :: Config
          -> [(Name, Name)] -- ^ original & new datatype names
          -> Name           -- ^ @ext@ type variable name
          -> [TyVarBndr]    -- ^ original type variables
          -> SimpleCon -> ConQ
extendCon conf nameMap ext tvs (SimpleCon name fields) = do
  let name'    = applyAffix (constructorName conf) name
      xname    = applyAffix (annotationName conf) name
      fields'  = extendRecursions nameMap ext fields
      extField = appExtTvs (ConT xname) ext tvs
  case fields' of
    NormalFields fs -> pure $ NormalC name' $ fs ++ [(strict, extField)]
    RecFields fs ->
      let extLabel = applyAffix (extensionLabel conf) name in
      pure $ RecC name' $ fs ++ [(extLabel, strict, extField)]

-- | Replaces recursive occurences of the datatype with the new one.
extendRecursions :: [(Name, Name)] -- ^ original & new datatype names
                 -> Name           -- ^ new type variable name
                 -> SimpleFields -> SimpleFields
extendRecursions nameMap ext = everywhere $ mkT go where
  go (ConT k) | Just new <- lookup k nameMap = ConT new `AppT` VarT ext
  go t = t

extensionCon :: Config
             -> Bool        -- ^ make a record constructor?
             -> Name        -- ^ datatype name
             -> Name        -- ^ @ext@ type variable
             -> [TyVarBndr] -- ^ original type variables
             -> Con
extensionCon conf record name ext tvs =
  let namex = applyAffix (extensionName conf) name
      label = applyAffix (extensionLabel conf) name
      typ   = appExtTvs (ConT namex) ext tvs
  in
  if record then
    RecC namex [(label, strict, typ)]
  else
    NormalC namex [(strict, typ)]

extendFam :: Config -> [TyVarBndr] -> SimpleCon -> DecQ
extendFam conf tvs (SimpleCon name _) =
  extendFam' (applyAffix (annotationName conf) name) tvs

extensionFam :: Config -> Name -> [TyVarBndr] -> DecQ
extensionFam conf name tvs =
  extendFam' (applyAffix (extensionName conf) name) tvs

constraintBundle :: Config
                 -> Name -- ^ datatype name
                 -> Name -- ^ extension type variable name
                 -> [TyVarBndr] -> [SimpleCon] -> DecQ
constraintBundle conf name ext tvs cs = do
  c <- newName "c"
  ckind <- [t|K.Type -> Constraint|]
  let cnames = map scName cs
      bname  = applyAffix (bundleName conf) name
      tvs'   = kindedTV c ckind : plainTV ext : tvs
      con1 n = varT c `appT`
               foldl appT (conT n) (varT ext : map (varT . tyvarName) tvs)
      tupled ts = foldl appT (tupleT (length ts)) ts
  tySynD bname tvs' $ tupled $ map con1 $
    map (applyAffix $ annotationName conf) cnames ++
    [applyAffix (extensionName conf) name]

makeInstances :: Config
              -> Name   -- ^ name of the __output__ datatype
              -> [Name] -- ^ names of all datatypes in this group
              -> Name   -- ^ extension type variable name
              -> [TyVarBndr]
              -> SimpleDeriv
              -> DecsQ
makeInstances conf name names ext tvs (SimpleDeriv strat prds) =
  pure $ map make1 prds
 where
  make1 prd = StandaloneDerivD strat'
    (map tvPred tvs ++ map allPred names)
    (prd `AppT` appExtTvs (ConT name) ext tvs)
   where
    tvPred = AppT prd . VarT . tyvarName
    allPred name' = appExtTvs (ConT bname `AppT` prd) ext tvs
      where bname = applyAffix (bundleName conf) name'
    strat' = case strat of
      SBlank    -> Nothing
      SStock    -> Just StockStrategy
      SAnyclass -> Just AnyclassStrategy

extendFam' :: Name -> [TyVarBndr] -> DecQ
extendFam' name tvs = do
  ext <- newName "ext"
  pure $ OpenTypeFamilyD $ TypeFamilyHead name (PlainTV ext : tvs) NoSig Nothing

-- | Generates the @XExts@ record, whose values contain descriptions of the
-- extensions applied to @X@.
--
-- Returns, in order:
--
-- * record name
-- * constructor annotation field names
--   (type field, name field, constructor name)
-- * extension constructor field name
-- * record declaration to splice
extRecord :: Config -> Name -> [SimpleCon]
          -> Q (Name, [(Name, Name, String)], Name, Dec)
extRecord conf cname cs = do
  let rname = applyAffix (extRecordName conf) cname
      conann c | isRecordCon c = [t| Maybe [(String, TypeQ)] |]
               | otherwise     = [t| Maybe [         TypeQ ] |]
      extList | extIsRecord cs = [t| [(String, [(String, TypeQ)])] |]
              | otherwise      = [t| [(String, [         TypeQ ])] |]
  tfields  <- traverse (\c -> extRecTypeField conf (conann c) (scName c)) cs
  nfields  <- traverse (extRecNameField conf . scName) cs
  extField <- extRecTypeField conf extList $
                applyAffix (extensionName conf) cname
  pure (rname,
        zip3 (map fieldName tfields)
             (map fieldName nfields)
             (map (nameBase . scName) cs),
        fieldName extField,
        DataD [] rname [] Nothing
          [RecC rname (tfields ++ nfields ++ [extField])] [])
 where
  fieldName (n, _, _) = n

extRecTypeField :: Config -> TypeQ -> Name -> VarBangTypeQ
extRecTypeField conf ty name =
  varBangType (applyAffix (extRecTypeName conf) name) $ bangType nonstrict ty

extRecNameField :: Config -> Name -> VarBangTypeQ
extRecNameField conf name = do
  varBangType (applyAffix (extRecNameName conf) name) $
    bangType nonstrict [t|String|]

extRecDefault :: Config
              -> Name -- ^ record name
              -> [(Name, Name, String)]
                  -- ^ type field, name field, and constructor name for each
                  -- constructor
              -> Name -- ^ field name for extension
              -> Q (Name, [Dec])
extRecDefault conf rname fcnames fname = do
  let mkField (t, n, c) = [fieldExp t [|Just []|], fieldExp n (stringE c)]
      fields = concatMap mkField fcnames
      xfield = fieldExp fname [| [] |]
      dname = applyAffix (defExtRecName conf) rname
  defn <- valD (varP dname) (normalB (recConE rname (fields ++ [xfield]))) []
  pure (dname, [SigD dname (ConT rname), defn])

-- | Generate the @extendX@ function, which is used to generate extended
-- versions of @X@
makeExtender :: Config
             -> String -- ^ module where @extensible@ was called
             -> Name   -- ^ datatype name
             -> Name   -- ^ extension record name
             -> [TyVarBndr] -> [SimpleCon] -> Q (Name, [Dec])
makeExtender conf home name' rname' tvs cs = do
  let name  = qualifyWith home name'
      rname = qualifyWith home rname'
      ename = applyAffix (extFunName conf) name'
      rtype = go tvs where
        go []     = conT rname
        go (_:xs) = [t|TypeQ -> $(go xs)|]
  sig  <- sigD ename [t|String -> [Name] -> TypeQ -> $rtype -> DecsQ|]
  syn  <- newName "syn"
  vars <- newName "vars"
  tag  <- newName "tag"
  exts <- newName "exts"
  exts' <- newName "exts'"
  let defn =
        [|sequence $ concat $(listE $
            map (decsForCon conf home exts' tag tvs) cs ++
            [decsForExt conf home exts' tag (extIsRecord cs) tvs name,
             makeTySyn conf home name syn vars tag,
             completePrag conf exts' cs name])|]
  let args = map (\tv -> [|varT $(lift $ tyvarName tv)|]) tvs
  val <- funD ename
        [clause (map varP [syn, vars, tag, exts]) (normalB defn)
         [valD (varP exts') (normalB (appsE (varE exts : args))) []]]
  pure (ename, [sig, val])

-- | Generates a type synonym for an extensible datatype applied to a specific
-- extension type, like @type Foo = Foo' Ext1@.
makeTySyn :: Config
          -> String -- ^ module where @extensible@ was called
          -> Name   -- ^ datatype name
          -> Name   -- ^ variable containing synonym's name
          -> Name   -- ^ variable containing extension's extra type arguments
          -> Name   -- ^ variable containing tag type
          -> ExpQ
makeTySyn conf home name syn vars tag =
  let tyname = qualifyWith home $ applyAffix (datatypeName conf) name in
  [|[tySynD (mkName $(varE syn))
            (map plainTV $(varE vars))
            (appT (conT tyname) $(varE tag))]|]

-- | Generates the type instance and pattern synonym (if any) for a constructor.
decsForCon :: Config
           -> String -- ^ module where @extensible@ was called
           -> Name -- ^ name of the bound @exts@ variable in @extendX@
           -> Name -- ^ name of the bound @tag@ variable in @extendX@
           -> [TyVarBndr] -> SimpleCon -> ExpQ
decsForCon conf home extsName tagName tvs (SimpleCon name fields) = do
  args <- case fields of
    NormalFields fs -> replicateM (length fs) (newName "x")
    RecFields    fs -> mapM (\(n, _, _) -> newName $ nameBase n) fs
  let tyfam = qualifyWith home $ applyAffix (annotationName conf) name
      name' = qualifyWith home $ applyAffix (constructorName conf) name
      typeC = varE $ qualifyWith home $ applyAffix (extRecTypeName conf) name
      nameC = varE $ qualifyWith home $ applyAffix (extRecNameName conf) name
      exts  = varE extsName
      tag   = varE tagName
      isRec = isRecordFields fields
      tvs'  = listE $ map tvbToTypeExp tvs
  [|let
#if MIN_VERSION_template_haskell(2,15,0)
        mkTf rhs = tySynInstD $
          tySynEqn Nothing
            (foldl appT (conT tyfam) $ $tag : $tvs')
            rhs
#else
        mkTf rhs = tySynInstD tyfam $ tySynEqn ($tag : $tvs') rhs
#endif
        annType = $typeC $exts; patName = mkName $ $nameC $exts
        mkPatSyn args' rhs = patSynD patName lhs implBidir rhs where
          lhs = $(if isRec then [|recordPatSyn|] else [|prefixPatSyn|]) args'
    in
    case annType of
      Just as ->
        let ty = tupT $(if isRec then [|map snd as|] else [|as|])
            anns =
              $(if isRec then
                [|map (mkName . fst) as|]
              else
                [|makeVars "ann" $ length as|])
        in
        [mkTf ty,
         mkPatSyn (args ++ anns)
                  (conP name' (map varP args ++ [tupP (map varP anns)]))]
      Nothing ->
        [mkTf (conT $(lift ''Void))]
   |]

-- | Generates the type instance and pattern synonym(s) for the extension.
decsForExt :: Config
           -> String -- ^ module where @extensible@ was called
           -> Name -- ^ name of the bound @exts@ variable in @extendX@
           -> Name -- ^ name of the bound @tag@ variable in @extendX@
           -> Bool -- ^ is the extension a record?
           -> [TyVarBndr] -> Name -> ExpQ
decsForExt conf home extsName tagName isRec tvs name = do
  let cname'   = applyAffix (extensionName conf) name
      cname    = qualifyWith home cname'
      typeC    = varE $ applyAffix (extRecTypeName conf) cname'
      tyfam    = applyAffix (extensionName conf) name
      exts     = varE extsName; tag = varE tagName
      getTy    = if isRec then [|map snd|] else [|id|]
      tvs'     = listE $ map tvbToTypeExp tvs
  [|let typs = $typeC $exts
        tySynRhs = case typs of
          [] -> conT $(lift ''Void)
          ts -> foldr1 mkEither $ map (tupT . $getTy . snd) ts
          where mkEither t u = conT $(lift ''Either) `appT` t `appT` u
#if MIN_VERSION_template_haskell(2,15,0)
        tySyn = tySynInstD $ tySynEqn Nothing
          (foldl appT (conT tyfam) ($tag : $tvs'))
          tySynRhs
#else
        tySyn = tySynInstD tyfam $
          tySynEqn ($tag : $tvs') tySynRhs
#endif
        mkPatSyn mkRhs (patName, flds) =
          let lbls =
                $(if isRec then
                  [|map (mkName . fst) flds|]
                else
                  [|makeVars "x" $ length flds|])
              lhs = $(if isRec then [|recordPatSyn|] else [|prefixPatSyn|])
          in
          patSynD (mkName patName) (lhs lbls) implBidir
            (conP cname [mkRhs (tupP $ map varP lbls)])
    in
    tySyn : zipWith mkPatSyn (makeEithers (length typs)) typs|]

makeVars :: String -> Int -> [Name]
makeVars pfx n = map (mkName . (pfx ++) . show) $ take n [1 :: Int ..]

-- | Generates an expression producing a @COMPLETE@ pragma.
completePrag :: Config
             -> Name -- ^ name of @exts@ argument
             -> [SimpleCon]
             -> Name -- ^ name of datatype
             -> ExpQ
completePrag conf extsName cs name =
  let exts = varE extsName
      mkCie cie (SimpleCon cname _) =
        let nameC = varE $ applyAffix (extRecNameName conf) cname
            typeC = varE $ applyAffix (extRecTypeName conf) cname
        in
        [|$cie (mkName ($nameC $exts)) ($typeC $exts)|]
      typeE = varE $ applyAffix (extRecTypeName <> extensionName $ conf) name
  in
  [|let conIfEnabled _ Nothing  = []
        conIfEnabled n (Just _) = [n]
        allExts = map $ mkName . fst
    in
    [pragCompleteD
      (concat $(listE $ map (mkCie [|conIfEnabled|]) cs) ++
       allExts ($typeE $exts))
      Nothing]
   |]

-- | Generates a list of functions which wrap patterns in successive branches of
-- right-nested 'Either's. For example, @makeEithers 4@ produces:
--
-- @
-- [\p -> [p|Left $p|],
--  \p -> [p|Right (Left $p)|],
--  \p -> [p|Right (Right (Left $p))|],
--  \p -> [p|Right (Right (Right $p))|]]
-- @
--
-- @makeEithers 1@ produces @[\p -> p]@.
makeEithers :: Int -> [PatQ -> PatQ]
makeEithers = addEithers' id where
  addEithers' _ 0 = []
  addEithers' f 1 = [f]
  addEithers' f n =
    (\p -> f [p|Left $p|]) :
    addEithers' (\p -> [p|Right $(f p)|]) (n - 1)

-- | Wraps a list of types in a tuple of the appropriate length, analogously
-- with 'tupE' and 'tupP'.
tupT :: [TypeQ] -> TypeQ
tupT [t] = t
tupT ts  = foldl appT (tupleT (length ts)) ts

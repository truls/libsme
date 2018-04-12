-- | Resolves all import statements by parsing the files they point to and
-- flatten the input structure through renaming.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module SME.ImportResolver
  ( resolveImports
  , RenameState
  ) where

import           Control.Arrow               (first)
import           Control.Exception           (throw)
import           Control.Monad               (unless)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.State.Strict
import qualified Data.Bimap                  as B
import           Data.Generics.Uniplate.Data (transformBiM, universeBi)
import           Data.List                   (intersect, nub, (\\))
import           Data.List.NonEmpty          (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty          as N
import           Data.Loc
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe)
import           Data.Semigroup              ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.Directory            (doesFileExist, makeAbsolute)
import           System.FilePath.Posix       (joinPath, takeDirectory, (<.>),
                                              (</>))

import           Language.SMEIL.Parser
import           Language.SMEIL.Syntax
import           SME.Error


data RenameState = RenameState
  { nameMap    :: B.Bimap Ident Ref
  , nameSource :: Integer
  } deriving (Show)

mkRenameState :: RenameState
mkRenameState = RenameState {nameMap = B.empty, nameSource = 0}

-- | Reader monad uesd for tracing import history
type PaM m = StateT RenameState m

newtype Module =
  Module (String, [String], DesignFile, [Module])
  deriving (Show)

data ModuleCtx = ModuleCtx
  { moduleName           :: Ref
  -- ^ The name that the module is imported as
  , importedNames        :: [Ident]
  -- ^ Specific names imported from module
  , parentNames          :: [Ref]
  -- ^ Names defined in parent modules
  , stmLocation          :: SrcLoc
  -- ^ Location of import statement
  , parentImportPrefixes :: [Ref]
  -- ^ Names imported from parent will be prefixed with
  , modulePath           :: FilePath
  -- ^ Path of module
  }

mkModuleCtx :: ModuleCtx
mkModuleCtx = ModuleCtx (Ident "" noLoc :| []) [] [] noLoc [] ""

toFileName :: FilePath -> Ref -> FilePath
toFileName fp ss =
  takeDirectory fp </> joinPath (map toString (N.toList ss)) <.> ".sme"

toModName :: (ToString a) => NonEmpty a -> T.Text
toModName ids = T.intercalate "_" (map toText (N.toList ids))

importError :: FilePath -> SrcLoc -> IO ()
importError f ss = throw $ ImportNotFoundError f ss

parseFile :: (MonadThrow m, MonadIO m) => FilePath -> SrcLoc -> m DesignFile
parseFile fp ss = do
  liftIO $ doesFileExist fp >>= flip unless (importError fp ss)
  modSrc <- liftIO $ TIO.readFile fp
  case parse fp modSrc of
    (Right r) -> return r
    (Left l)  -> throw $ ParseError l

parseImport :: FilePath -> [Ref] -> [Ref] -> Import -> ModuleCtx
parseImport dir ns prefixes =
  let fn = toFileName dir
  in (\case
        SimpleImport {..} ->
          (ModuleCtx
             (fromMaybe modName ((:| []) <$> qualified))
             []
             ns
             loc
             prefixes
             (fn modName))
        SpecificImport {..} ->
          (ModuleCtx
             (fromMaybe modName ((:| []) <$> qualified))
             entities
             ns
             loc
             prefixes
             (fn modName)))

-- FIXME: avoid this code duplication of the function above
importName :: Import -> [Ref]
importName SimpleImport {..}   = [fromMaybe modName ((:| []) <$> qualified)]
importName SpecificImport {..} =
  let ents = map (:| []) entities
  in fromMaybe ents ((\q -> map (q <|) ents) <$> qualified)

-- | Returns all defined idents and base hierarchical accesses defined in a
-- module.
dfNames :: DesignFile -> [Ref]
dfNames f =
  map refOf (universeBi f :: [Ident]) ++ map refOf (universeBi f :: [Name])

-- | Returns the names of all declarations in the program
declNames :: DesignFile -> [[Ident]]
declNames f =
  map (: []) $
  map nameOf (universeBi f :: [Variable]) ++
  map nameOf (universeBi f :: [Constant]) ++
  map nameOf (universeBi f :: [Bus]) ++
  map nameOf (universeBi f :: [Function]) ++
  map nameOf (universeBi f :: [Enumeration]) ++
  map nameOf (universeBi f :: [Instance]) ++
  map nameOf (universeBi f :: [Process]) ++
  map nameOf (universeBi f :: [Network])

-- | Prefix matching based union operation matching items on from list into into
-- list. Returns names without matching prefix
prefixUnion :: [Ref] -> [Ref] -> [[Ident]]
prefixUnion from into
  -- trace
  --   ("initial from " ++ ppShow from ++ " show into " ++ ppShow into)
 = concatMap (matchPrefix (map N.toList into) . N.toList) from
  where
    matchPrefix :: [[Ident]] -> [Ident] -> [[Ident]]
    matchPrefix withs match =
      nub
        (map (drop (length match)) $
         filter
           (\x
              --trace ("comparing " ++ show match ++ " with " ++ show x)
             -> match == take (length match) x)
           withs)

getFirstNames :: [[Ident]] -> [Ident]
getFirstNames = nub . concatMap go
  where go (i:_) = [i]
        go []    = []

-- | Return module containing only imported top level names and no imports
filterModule :: [Ident] -> DesignFile -> DesignFile
filterModule required df@DesignFile {..} = DesignFile (map go units) loc
  -- TODO: Extend dependency tracking with a list mapping functions to their
  -- dependencies. This allows us to remove unused functions that were included
  -- as dependencies of functions we removed.
  where
    procs = universeBi df :: [Process]
    nets = universeBi df :: [Network]
    -- Include module dependencies in required modules
    -- Assumption: Since all in-module dependencies are declared as IdentNames
    -- in the top-level, taking the first component of each hier access name
    -- will work.
    required' =
      required ++
      (map N.head (concatMap processDeps procs) `intersect` topLevelDefs df) ++
      (map N.head (concatMap networkDeps nets) `intersect` topLevelDefs df)

    go DesignUnit {unitElement = unitElement, loc = duloc} =
      if null required
        then DesignUnit [] unitElement loc
        else DesignUnit [] (filter go' unitElement) duloc
    go' UnitProc {process = Process {name = name}} = name `elem` required'
    go' UnitNet {network = Network {name = name}}  = name `elem` required'

-- | Return the names of top level definitions in a network
topLevelDefs :: DesignFile -> [Ident]
topLevelDefs df =
  map nameOf (universeBi df :: [Process]) ++
  map nameOf (universeBi df :: [Network])

-- | Scans the body of a module and looks up dependencies
processDeps :: Process -> [Ref]
processDeps Process {..} = map refOf (universeBi body :: [Name])

-- | Scans the body of a module and looks up dependencies
networkDeps :: Network -> [Ref]
networkDeps Network {..} = map refOf (universeBi netDecls :: [Name])

addNameMap :: (MonadIO m, MonadThrow m) => Ident -> Ref -> PaM m ()
addNameMap n1 n2 = do
  s <- get
  when (refOf n1 /= n2) $ put $ s {nameMap = B.insert n1 n2 (nameMap s)}

lookupName :: (MonadIO m, MonadThrow m) => Ref -> PaM m (Maybe Ident)
lookupName name --trace (show name)
 = do
  s <- gets nameMap
  let res = B.lookupR name s :: Maybe Ident
  return res

-- | Given a list of idents, try to look up the longest possible prefix of the
-- list in the map. Returns maybe a tuple containing the length of matched name
-- chain and the result of the lookup
lookupPrefix ::
     (MonadIO m, MonadThrow m) => Ref -> PaM m (Maybe (Int, Ident))
lookupPrefix = go . reverse . N.toList
  where
    go [] = return Nothing
    go (x:xs) =
      lookupName (N.reverse (x :| xs)) >>= \case
        Just r -> return $ Just (length xs + 1, r)
        Nothing -> go xs

-- | Transform top-level names and add maps, tracking the transformed names in a
-- map
renameModule ::
     (MonadThrow m, MonadIO m) => Ref -> Bool -> DesignFile -> PaM m DesignFile
renameModule n asSpecific f =
  transformBiM renameProc f >>= transformBiM renameNet
  -- Pre populate map with possible module names
  where
    renameProc p@Process {name = name}
      -- Map from top-level name to new name
     = do
      addMaps name
      return (p {name = newName name} :: Process)
    renameNet p@Network {name = name} = do
      addMaps name
      return (p {name = newName name} :: Network)
    newName Ident {..} =
      let n' = toModName n
         -- Return new name prefixed with _ to avoid clashing with existing
        -- namespace
        -- TODO: Avoid the _ prefix by explicitly checking for name clashes
      in Ident
           ((if T.null n'
              then ""
              else "_" <> n' <> "_") <> val)
           loc
    addMaps name =
      addNameMap
        (newName name)
        (if asSpecific
           then refOf name
           else n <> refOf name)

-- | Rename all references to renamed modules accordingly
renameRefs :: (MonadThrow m, MonadIO m) => DesignFile -> PaM m DesignFile
renameRefs = transformBiM go
  where
    go o@Name {..} =
      let b = refOf o
      in lookupPrefix b >>= \case
           Just (n, ha) ->
             return $
             Name (IdentName ha (SrcLoc $ locOf ha) :| N.drop n parts) loc
           Nothing -> return o

-- DONE: When we have unqualified specific imports of names, we need to make
-- sure that these names does not clash with any definitions in the program. For
-- eaxmple, a reference to a name foo from within a process defined in a module
-- that imports an entity foo from another module is ambiguous. Solve this by
-- gathering the names of all defined entities and making sure that name clashes
-- occur.
-- TODO: Use forward-passed list of defined names to ensure that we don't rename
-- modules such that name-clashes will occur
parseModule :: (MonadThrow m, MonadIO m) => ModuleCtx -> PaM m DesignFile
parseModule ModuleCtx {..} = do
  res <- liftIO $ parseFile modulePath stmLocation
  let imports = universeBi res :: [Import]
      importPrefixes = concatMap importName imports
      names = dfNames res
      decNames = declNames res
  -- liftIO $ putStrLn ("parentImportPrefixes " ++ ppShow parentImportPrefixes)
  -- liftIO $ putStrLn ("names " ++ ppShow names)
  -- If we have a module foo/bar containing a process baz and we import foo.bar,
  -- ents' should contain all names following the import prefix. So if baz is
  -- referenced as foo.bar.baz, we get all name compounds following foo.bar. In
  -- this example, ents' would get the value [baz]
  -- The union here is a bit redundant. The semantics are intended to be if null
  -- ents then ents'
  let ents' =
        if null importedNames
          then getFirstNames (prefixUnion parentImportPrefixes parentNames) -- not neded TODO
          else importedNames
  -- liftIO $ putStrLn ("ents " ++ ppShow ents')
  let topNames = topLevelDefs res
  areNamesDefined ents' topNames
  -- FIXME: This currently checks against ALL names in the program. It's maybe a
  -- bit too general since it is probably ok for process variables to shadow
  -- global names
  areNamesUnique decNames importPrefixes
  -- Forward pass: List of defined names. Should include accumulated entries
  let filtered = filterModule ents' res
  renamed <- renameModule moduleName (not $ null importedNames) filtered
  mods <-
    mapM (parseModule . parseImport modulePath names importPrefixes) imports
  -- Backwards pass: We know what names should be. Do prefix matching.
  -- Second rename pass to rename references to renamed modules accordingly
  renamed' <- renameRefs renamed
  return $ foldl (<>) renamed' mods
    -- Check that all imported names are defined
  where
    areNamesDefined lns names = do
      let diff = lns \\ names
      unless (null diff) $
        throw $ UndefinedIdentifierError (map nameOf diff) stmLocation
    areNamesUnique reqNames defs
      -- TODO: Location in error message
     =
      let a = reqNames `intersect` map N.toList defs
      in unless (null a) $
         throw $ IdentifierClashError (map nameOf (getFirstNames a)) stmLocation

-- | Resolves all imports and inlines them returning a "flat" SMEIL
-- program. Imported entities are inlined as appropriate
resolveImports ::
     (MonadThrow m, MonadIO m) => FilePath -> m (DesignFile, M.Map String Ref)
resolveImports fp = do
  fp' <- liftIO $ makeAbsolute fp
  (m, s) <-
    runStateT (parseModule (mkModuleCtx {modulePath = fp'})) mkRenameState
  -- TODO: Maybe the BiMap is redundant
  return (m, M.fromList (map (first toString) (B.toList (nameMap s))))

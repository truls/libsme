-- | Resolves all import statements by parsing the files they point to and
-- flatten the input structure through renaming.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}

module SME.ImportResolver
  ( resolveImports
  , RenameState
  ) where

import           Prelude                     hiding (span)

import           Control.Exception           (throw)
import           Control.Monad               (unless)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.State.Strict
import qualified Data.Bimap                  as B
import           Data.Generics.Uniplate.Data (transformBiM, universeBi)
import           Data.List                   (intercalate, intersect, nub, (\\))
import           Data.Loc
import           Data.Maybe                  (fromMaybe)
import           Data.Semigroup              ((<>))
import           System.Directory            (doesFileExist, makeAbsolute)
import           System.FilePath.Posix       (joinPath, takeDirectory, (<.>),
                                              (</>))

import           Language.SMEIL.Parser
import           Language.SMEIL.Syntax


-- import           Debug.Trace                 (trace)
-- import           Text.Show.Pretty            (ppShow)

import           SME.Error

data RenameState = RenameState
  { nameMap    :: B.Bimap Ident [Ident]
  , nameSource :: Integer
  } deriving (Show)

mkRenameState :: RenameState
mkRenameState = RenameState {nameMap = B.empty, nameSource = 0}

-- | Reader monad uesd for tracing import history
type PaM m = StateT RenameState m

newtype Module = Module (String, [String], DesignFile, [Module])
  deriving (Show)

data ModuleCtx = ModuleCtx
  { moduleName           :: [Ident]   -- | The name that the module is imported as
  , importedNames        :: [Ident]   -- | Specific names imported from module
  , parentNames          :: [[Ident]] -- | Names defined in parent modules
  , stmLocation          :: SrcLoc    -- | Location of import statement
  , parentImportPrefixes :: [[Ident]] -- | Names imported from parent will be prefixed with
  , modulePath           :: FilePath  -- | Path of module
  }

mkModuleCtx :: ModuleCtx
mkModuleCtx = ModuleCtx [] [] [] noLoc [] ""

toFileName :: FilePath -> [Ident] -> FilePath
toFileName _ []  = ""
toFileName fp ss = takeDirectory fp </> joinPath (map toString ss) <.> ".sme"

toModName :: (ToString a) => [a] -> String
toModName ids = intercalate "_" (map toString ids)

importError :: FilePath -> SrcLoc -> IO ()
importError f ss = throw $ ImportNotFoundError f ss

parseFile :: (MonadThrow m, MonadIO m) => FilePath -> SrcLoc -> m DesignFile
parseFile fp ss = do
  liftIO $ doesFileExist fp >>= flip unless (importError fp ss)
  modSrc <- liftIO $ readFile fp
  case parse fp modSrc of
    (Right r) -> return r
    (Left l)  -> throw $ ParseError l

parseImport :: FilePath -> [[Ident]] -> [[Ident]] -> Import -> ModuleCtx
parseImport dir ns prefixes =
  let fn = toFileName dir
  in (\case
        SimpleImport {..} ->
          (ModuleCtx
             (fromMaybe modName ((: []) <$> qualified))
             []
             ns
             loc
             prefixes
             (fn modName))
        SpecificImport {..} ->
          (ModuleCtx
             (fromMaybe modName ((: []) <$> qualified))
             entities
             ns
             loc
             prefixes
             (fn modName)))

-- FIXME: avoid this code duplication of the function above
importName :: Import -> [[Ident]]
importName SimpleImport {..}   = [fromMaybe modName ((: []) <$> qualified)]
importName SpecificImport {..} =
  let ents = map (: []) entities
  in fromMaybe ents ((\q -> map (q :) ents) <$> qualified)

-- | Returns all defined idents and base hierarchical accesses defined in a
-- module.
dfNames :: DesignFile -> [[Ident]]
dfNames f =
  map (: []) (universeBi f :: [Ident]) ++ map refOf (universeBi f :: [Name])

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
prefixUnion :: [[Ident]] -> [[Ident]] -> [[Ident]]
prefixUnion from into
  -- trace
  --   ("initial from " ++ ppShow from ++ " show into " ++ ppShow into)
 = concatMap (matchPrefix into) from
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
  where
    procs = universeBi df :: [Process]
    nets = universeBi df :: [Network]
    -- Include module dependencies in required modules
    -- FIXME: Partial function
    -- Assumption: Since all in-module dependencies are declared as IdentNames
    -- in the top-level, taking the first component of each hier access name
    -- will work.
    required' =
      required ++
      (map head (concatMap processDeps procs) `intersect` topLevelDefs df) ++
      (map head (concatMap networkDeps nets) `intersect` topLevelDefs df)
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
processDeps :: Process -> [[Ident]]
processDeps Process {..} = map refOf (universeBi body :: [Name])

-- | Scans the body of a module and looks up dependencies
networkDeps :: Network -> [[Ident]]
networkDeps Network {..} = map refOf (universeBi netDecls :: [Name])

addNameMap :: (MonadIO m, MonadThrow m) => Ident -> [Ident] -> PaM m ()
addNameMap n1 n2 = do
  s <- get
  when ([n1] /= n2) $ put $ s {nameMap = B.insert n1 n2 (nameMap s)}

lookupName :: (MonadIO m, MonadThrow m) => [Ident] -> PaM m (Maybe Ident)
lookupName name --trace (show name)
 = do
  s <- gets nameMap
  let res = B.lookupR name s :: Maybe Ident
  return res

-- | Given a list of idents, try to look up the longest possible prefix of the
-- list in the map. Returns maybe a tuple containing the length of matched name
-- chain and the result of the lookup
lookupPrefix ::
     (MonadIO m, MonadThrow m) => [Ident] -> PaM m (Maybe (Int, Ident))
lookupPrefix = go . reverse
  where
    go [] = return Nothing
    go (x:xs) =
      lookupName (reverse (x : xs)) >>= \case
        Just r -> return $ Just (length xs + 1, r)
        Nothing -> go xs

-- | Transform top-level names and add
renameModule ::
     (MonadThrow m, MonadIO m) => [Ident] -> Bool -> DesignFile -> PaM m DesignFile
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
      in Ident
           ((n' ++
             (if null n'
                then ""
                else "_")) ++
            val)
           loc
    addMaps name =
      addNameMap
        (newName name)
        (if asSpecific
           then [name]
           else n ++ [name])

renameRefs :: (MonadThrow m, MonadIO m) => DesignFile -> PaM m DesignFile
renameRefs = transformBiM go
  where
    go o@Name {..} =
      let b = refOf o
      in lookupPrefix b >>= \case
           Just (n, ha) ->
             return $
             Name
               (IdentName ha (SrcLoc $ locOf ha))
               (drop n (base : parts))
               loc
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
  areNamesUnique decNames importPrefixes
  -- Forward pass: List of defined names. Should include accumulated entries
  -- TODO: How to filter out unused entities when an entire module is defined
  -- Get a list of names following prefixes, match those against the defined top
  -- level names of the imported module.
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
    areNamesUnique reqNames defs =
      -- TODO: Location in error message
      let a = reqNames `intersect` defs
      in unless (null a) $
         throw $ IdentifierClashError (map nameOf (getFirstNames a)) stmLocation

resolveImports :: (MonadThrow m, MonadIO m) => FilePath -> m DesignFile
resolveImports fp = do
  fp' <- liftIO $ makeAbsolute fp
  (m, _) <-
    runStateT (parseModule (mkModuleCtx {modulePath = fp'})) mkRenameState
  -- liftIO $ print ma
  return m

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
module Main (main) where

import Cabal.Project (parseProject, prjPackages, resolveProject)
import Control.Exception (throwIO)
import Control.Monad (filterM)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Either (partitionEithers)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes, isJust)
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Client.Add
import Distribution.PackageDescription (
  ComponentName,
  GenericPackageDescription,
  packageDescription,
  specVersion,
 )
import Distribution.PackageDescription.Quirks (patchQuirks)
import Options.Applicative (
  Parser,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  optional,
  progDesc,
  strArgument,
  strOption,
 )
import Options.Applicative.NonEmpty (some1)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))

data RawConfig = RawConfig
  { rcnfMProjectFile :: !(Maybe FilePath)
  , rcnfArgs :: !(NonEmpty String)
  }
  deriving (Show)

parseRawConfig :: Parser RawConfig
parseRawConfig = do
  rcnfMProjectFile <-
    optional $
      strOption $
        long "project-file"
          <> metavar "FILE"
          <> help "Set the path of the cabal.project file. Detect cabal.project or *.cabal in the current folder, if omitted."
  rcnfArgs <-
    some1 $
      strArgument $
        metavar "ARGS"
          <> help "Optional package component (wildcards such as 'exe', 'test' or 'bench' are supported) to update, followed by a non-empty list of package(s) to add to 'build-depends' section. Version bounds can be provided as well, use quotes to escape comparisons from your shell. E. g., 'foo < 0.2'."
  pure RawConfig {..}

resolveCabalProjectInCurrentFolder :: IO (Maybe FilePath)
resolveCabalProjectInCurrentFolder = do
  let fn = "cabal.project"
  exists <- doesFileExist fn
  pure $ if exists then Just fn else Nothing

resolveCabalFileInCurrentFolder :: IO (Either String FilePath)
resolveCabalFileInCurrentFolder = do
  files <- listDirectory "."

  -- filter in two steps to reduce IO
  let cabalFiles' = filter (".cabal" `L.isSuffixOf`) files
  -- make sure we don't catch directories
  cabalFiles <- filterM doesFileExist cabalFiles'

  pure $ case cabalFiles of
    [] ->
      Left "Found no cabal files in current folder. Giving up."
    [fn] ->
      Right fn
    _ : _ : _ ->
      Left "Found multiple cabal files in current folder. Giving up."

extractCabalFilesFromProject :: FilePath -> IO [FilePath]
extractCabalFilesFromProject projectFn = do
  project <- B.readFile projectFn
  parsed <- case parseProject projectFn project of
    Left exc -> throwIO exc
    Right p -> pure p
  resolved <- resolveProject projectFn parsed
  case resolved of
    Left exc -> throwIO exc
    Right prj -> pure $ map (takeDirectory projectFn </>) $ prjPackages prj

resolveCabalFiles :: Maybe FilePath -> IO [FilePath]
resolveCabalFiles = \case
  Nothing -> do
    projectFn <- resolveCabalProjectInCurrentFolder
    case projectFn of
      Nothing -> do
        cabalFn <- resolveCabalFileInCurrentFolder
        case cabalFn of
          Left e -> die e
          Right fn -> pure [fn]
      Just fn -> extractCabalFilesFromProject fn
  Just fn -> extractCabalFilesFromProject fn

readCabalFile :: FilePath -> IO (Maybe ByteString)
readCabalFile fileName = do
  cabalFileExists <- doesFileExist fileName
  if cabalFileExists
    then Just . snd . patchQuirks <$> B.readFile fileName
    else pure Nothing

stripAdd :: [String] -> [String]
stripAdd ("add" : xs) = xs
stripAdd xs = xs

data Input = Input
  { inpFilePath :: FilePath
  , inpPackageDescription :: GenericPackageDescription
  , inpConfig :: Config
  }
  deriving (Show)

mkInputs
  :: Bool
  -- ^ Must the first argument be a component name?
  -- As opposed to interpreting it as one of dependencies to add.
  -> FilePath
  -- ^ Cabal file name, primarily for error reporting.
  -- Note that 'mkInputs' does not do any 'IO'.
  -> ByteString
  -- ^ Cabal file content.
  -> NonEmpty String
  -- ^ List of arguments. The first one is either a component name
  -- or a dependency (see 'Bool' flag above), the rest are dependencies.
  -> Either String Input
  -- ^ Either an error or a tuple of input data.
mkInputs isCmpRequired cabalFile origContents args = do
  (fields, packDescr) <- parseCabalFile cabalFile origContents
  let specVer :: CabalSpecVersion
      specVer = specVersion $ packageDescription packDescr
      mkCmp :: Maybe String -> Either String (Either CommonStanza ComponentName)
      mkCmp = resolveComponent cabalFile (fields, packDescr)
      mkDeps :: NonEmpty String -> Either String (NonEmpty ByteString)
      mkDeps = traverse (validateDependency specVer)
  (cmp, deps) <- case args of
    x :| (y : ys)
      | Right c <- mkCmp (Just x) ->
          (c,) <$> mkDeps (y :| ys)
      | isCmpRequired -> Left $ "Target component '" ++ x ++ "' is not found"
    _ ->
      if isCmpRequired
        then Left "Target component is required"
        else (,) <$> mkCmp Nothing <*> mkDeps args
  pure $ Input cabalFile packDescr (Config origContents fields cmp deps BuildDepends)

disambiguateInputs
  :: Maybe FilePath
  -> [(FilePath, Either String Input)]
  -> Either String Input
disambiguateInputs mProjectFile inputs = case inputs' of
  ([], []) -> Left $ case mProjectFile of
    Nothing -> "No Cabal files or projects are found in the current folder, please specify --project-file."
    Just projFn -> "No Cabal files are found in " ++ projFn
  (errs, []) ->
    Left $
      L.intercalate "\n" $
        map (\(fn, err) -> "Cannot add a dependency to " ++ fn ++ " because: " ++ err) errs
  (_, [inp]) -> pure $ snd inp
  (_, _inps) ->
    Left
      "Cannot add a dependency, because target component is ambiguous. Please specify it as package:type:component. See https://cabal.readthedocs.io/en/latest/cabal-commands.html#target-forms for reference"
  where
    inputs' = partitionEithers $ map (\(fn, e) -> bimap (fn,) (fn,) e) inputs

main :: IO ()
main = do
  isCabalEnvVarSet <- isJust <$> lookupEnv "CABAL"
  rawArgs <- getArgs
  RawConfig {..} <-
    withArgs ((if isCabalEnvVarSet then stripAdd else id) rawArgs) $
      execParser $
        info
          (helper <*> parseRawConfig)
          (fullDesc <> progDesc "Extend build-depends from the command line")

  cabalFiles <- resolveCabalFiles rcnfMProjectFile
  cabalFilesAndContent <-
    catMaybes
      <$> traverse (\fn -> fmap (fn,) <$> readCabalFile fn) cabalFiles
  let getInput isCmpRequired =
        disambiguateInputs rcnfMProjectFile $
          map
            (\(fn, cnt) -> (fn, mkInputs isCmpRequired fn cnt rcnfArgs))
            cabalFilesAndContent

  input <- case getInput False of
    Right i -> pure i
    Left errFalse -> case rcnfArgs of
      _ :| [] -> die errFalse
      _ -> case getInput True of
        Right i -> pure i
        Left errTrue ->
          die $
            "If the first argument is interpreted as a dependency:\n"
              ++ errFalse
              ++ "\n\nIf the first argument is interpreted as a target component:\n"
              ++ errTrue

  let Input cabalFile origPackDescr cnf = input

  case executeConfig (validateChanges origPackDescr) cnf of
    Nothing ->
      die $
        "Cannot extend build-depends in "
          ++ cabalFile
          ++ ", please report as a bug at https://github.com/Bodigrim/cabal-add/issues"
    Just r -> B.writeFile cabalFile r

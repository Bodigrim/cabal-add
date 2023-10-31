{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
module Main (main) where

import Control.Monad (filterM, unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Distribution.Client.Add
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
  short,
  strArgument,
  strOption,
 )
import Options.Applicative.NonEmpty (some1)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (die)

data RawConfig = RawConfig
  { rcnfMCabalFile :: !(Maybe FilePath)
  , rcnfComponent :: !(Maybe String)
  , rcnfDependencies :: !(NonEmpty String)
  }
  deriving (Show)

parseRawConfig :: Parser RawConfig
parseRawConfig = do
  rcnfMCabalFile <-
    optional $
      strOption $
        long "cabal-file"
          <> short 'f'
          <> metavar "FILE"
          <> help "Cabal file to edit in place (tries to detect cabal file in current folder if omitted)."
  rcnfComponent <-
    optional $
      strOption $
        long "component"
          <> short 'c'
          <> metavar "ARG"
          <> help "Package component to update (the main library, if omitted). Wildcards such as 'exe', 'test' or 'bench' are supported."
  rcnfDependencies <-
    some1 $
      strArgument $
        metavar "DEP"
          <> help "Package(s) to add to build-depends section. Version bounds can be provided as well, use quotes to escape comparisons from your shell. E. g., 'foo < 0.2'."
  pure RawConfig {..}

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

readCabalFile :: FilePath -> IO ByteString
readCabalFile fileName = do
  cabalFileExists <- doesFileExist fileName
  unless cabalFileExists $
    die $
      fileName ++ " does not exist or is not a file"
  snd . patchQuirks <$> B.readFile fileName

main :: IO ()
main = do
  RawConfig {..} <-
    execParser $
      info
        (helper <*> parseRawConfig)
        (fullDesc <> progDesc "Extend build-depends from the command line")

  (cnfOrigContents, cabalFile) <- case rcnfMCabalFile of
    Just rcnfCabalFile -> (,rcnfCabalFile) <$> readCabalFile rcnfCabalFile
    Nothing -> do
      resolveCabalFileInCurrentFolder >>= \case
        Left e -> die e
        Right defaultCabalFile -> (,defaultCabalFile) <$> readCabalFile defaultCabalFile

  let inputs = do
        (fields, packDescr) <- parseCabalFile cabalFile cnfOrigContents
        cmp <- resolveComponent cabalFile (fields, packDescr) rcnfComponent
        deps <- traverse validateDependency rcnfDependencies
        pure (fields, packDescr, cmp, deps)

  (cnfFields, origPackDescr, cnfComponent, cnfDependencies) <- case inputs of
    Left err -> die err
    Right pair -> pure pair

  case executeConfig (validateChanges origPackDescr) Config {..} of
    Nothing ->
      die $
        "Cannot extend build-depends in "
          ++ cabalFile
          ++ ", please report as a bug at https://github.com/Bodigrim/cabal-add/issues"
    Just r -> B.writeFile cabalFile r

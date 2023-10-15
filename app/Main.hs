{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Distribution.Client.Add
import Distribution.PackageDescription.Quirks
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import System.Directory
import System.Exit

data RawConfig = RawConfig
  { rcnfCabalFile :: !FilePath
  , rcnfComponent :: !(Maybe String)
  , rcnfDependencies :: !(NonEmpty String)
  }
  deriving (Show)

parseRawConfig :: Maybe FilePath -> Parser RawConfig
parseRawConfig defaultCabalFile = do
  rcnfCabalFile <-
    strOption $
      long "cabal-file"
        <> short 'f'
        <> metavar "FILE"
        <> showDefault
        <> maybe mempty value defaultCabalFile
        <> help "Cabal file to edit in place."
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

resolveCabalFileInCurrentFolder :: IO (Maybe FilePath)
resolveCabalFileInCurrentFolder = do
  files <- listDirectory "."
  let cabalFiles = filter (".cabal" `L.isSuffixOf`) files
  pure $ case cabalFiles of
    [fn] -> Just fn
    _ -> Nothing

readCabalFile :: FilePath -> IO ByteString
readCabalFile fileName = do
  cabalFileExists <- doesFileExist fileName
  unless cabalFileExists $
    die $
      fileName ++ " does not exist"
  snd . patchQuirks <$> B.readFile fileName

main :: IO ()
main = do
  defaultCabalFile <- resolveCabalFileInCurrentFolder
  RawConfig {..} <-
    execParser $
      info
        (parseRawConfig defaultCabalFile <**> helper)
        (fullDesc <> progDesc "Extend build-depends from the command line")

  cnfOrigContents <- readCabalFile rcnfCabalFile

  let inputs = do
        (fields, packDescr) <- parseCabalFile rcnfCabalFile cnfOrigContents
        cmp <- resolveComponent rcnfCabalFile (fields, packDescr) rcnfComponent
        deps <- traverse validateDependency rcnfDependencies
        pure (fields, packDescr, cmp, deps)

  (cnfFields, origPackDescr, cnfComponent, cnfDependencies) <- case inputs of
    Left err -> die err
    Right pair -> pure pair

  case executeConfig (validateChanges origPackDescr) Config {..} of
    Nothing ->
      die $
        "Cannot extend build-depends in "
          ++ rcnfCabalFile
          ++ ", please report as a bug at https://github.com/Bodigrim/cabal-add/issues"
    Just r -> B.writeFile rcnfCabalFile r

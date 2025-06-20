{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
--
-- Building blocks of @cabal-add@ executable.
module Distribution.Client.Add (
  parseCabalFile,
  resolveComponent,
  CommonStanza (..),
  validateDependency,
  AddConfig (..),
  executeAddConfig,
  validateChanges,
  TargetField (..),
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Internal (isSpaceChar8)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Distribution.CabalSpecVersion (CabalSpecVersion (CabalSpecV1_0, CabalSpecV3_0))
import Distribution.Client.Common (CommonStanza (..), TargetField (..), getTargetName, isComponent, splitAtPosition)
import Distribution.Fields (
  Field (..),
  FieldLine (..),
  Name (..),
  SectionArg (..),
  readFields,
 )
import Distribution.PackageDescription (
  ComponentName (..),
  Dependency,
  GenericPackageDescription (..),
  LibraryName (..),
  PackageDescription (..),
  componentNameString,
  pkgName,
  unPackageName,
  unUnqualComponentName,
 )
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (
  parseGenericPackageDescription,
  parseGenericPackageDescriptionMaybe,
  runParseResult,
 )
import Distribution.Parsec (
  Position (..),
  eitherParsec,
  showPError,
 )
import Distribution.Simple.BuildTarget (
  BuildTarget (BuildTargetComponent),
  readUserBuildTargets,
  resolveBuildTargets,
 )

-- | An input for 'executeAddConfig'.
data AddConfig = AddConfig
  { cnfOrigContents :: !ByteString
  -- ^ Original Cabal file (with quirks patched,
  -- see "Distribution.PackageDescription.Quirks"),
  -- must be in sync with 'cnfFields'.
  , cnfFields :: ![Field Position]
  -- ^ Parsed (by 'Distribution.Fields.readFields' or, more specifically, by 'parseCabalFile')
  -- representation of the Cabal file,
  -- must be in sync with 'cnfOrigContents'.
  , cnfComponent :: !(Either CommonStanza ComponentName)
  -- ^ Which component to update?
  -- Usually constructed by 'resolveComponent'.
  , cnfTargetField :: !TargetField
  -- ^ Which field to add the provided content to?
  , cnfAdditions :: !(NonEmpty ByteString)
  -- ^ Which content to add to the target field?
  -- Usually constructed by 'validateDependency'.
  }
  deriving (Eq, Show)

requiresCommas :: TargetField -> Bool
requiresCommas = \case
  BuildDepends -> True
  _ -> False

extractComponentNames :: GenericPackageDescription -> Set ComponentName
extractComponentNames GenericPackageDescription {..} =
  foldMap (const $ S.singleton $ CLibName LMainLibName) condLibrary
    <> foldMap (S.singleton . CLibName . LSubLibName . fst) condSubLibraries
    <> foldMap (S.singleton . CFLibName . fst) condForeignLibs
    <> foldMap (S.singleton . CExeName . fst) condExecutables
    <> foldMap (S.singleton . CTestName . fst) condTestSuites
    <> foldMap (S.singleton . CBenchName . fst) condBenchmarks

extractCommonStanzas :: [Field ann] -> Set CommonStanza
extractCommonStanzas = foldMap go
  where
    go = \case
      Section (Name _ "common") [SecArgName _pos sectionArg] _subFields ->
        S.singleton $ CommonStanza sectionArg
      Section (Name _ "common") [SecArgStr _pos sectionArg] _subFields ->
        S.singleton $ CommonStanza sectionArg
      _ -> mempty

data Resolution a = NotFound | Resolved a | Ambiguous
  deriving (Functor)

instance Semigroup (Resolution a) where
  a@Resolved {} <> _ = a
  _ <> a@Resolved {} = a
  Ambiguous <> _ = Ambiguous
  _ <> Ambiguous = Ambiguous
  NotFound <> NotFound = NotFound

resolveMainLib :: Set ComponentName -> Resolution ComponentName
resolveMainLib knownNames
  | CLibName LMainLibName `elem` knownNames = Resolved $ CLibName LMainLibName
  | otherwise = NotFound

resolveDefaultComponent :: Set ComponentName -> (ComponentName -> Bool) -> Resolution ComponentName
resolveDefaultComponent knownNames predicate =
  case filter predicate (S.toList knownNames) of
    [] -> NotFound
    [x] -> Resolved x
    _ -> Ambiguous

isCLibName :: ComponentName -> Bool
isCLibName = \case
  CLibName {} -> True
  _ -> False

isCFLibName :: ComponentName -> Bool
isCFLibName = \case
  CFLibName {} -> True
  _ -> False

isCExeName :: ComponentName -> Bool
isCExeName = \case
  CExeName {} -> True
  _ -> False

isCTestName :: ComponentName -> Bool
isCTestName = \case
  CTestName {} -> True
  _ -> False

isCBenchName :: ComponentName -> Bool
isCBenchName = \case
  CBenchName {} -> True
  _ -> False

resolveToComponentName :: Set ComponentName -> Maybe String -> Resolution ComponentName
resolveToComponentName knownNames = \case
  Nothing -> case S.minView knownNames of
    Just (knownName, rest)
      | S.null rest -> Resolved knownName
    _ -> resolveMainLib knownNames
  Just name
    -- Cf. Distribution.Simple.BuildTarget.matchComponentKind
    | name `elem` ["lib", "library"] ->
        resolveMainLib knownNames
    | name `elem` ["flib", "foreign-library"] ->
        resolveDefaultComponent knownNames isCFLibName
    | name `elem` ["exe", "executable"] ->
        resolveDefaultComponent knownNames isCExeName
    | name `elem` ["tst", "test", "test-suite"] ->
        resolveDefaultComponent knownNames isCTestName
    | name `elem` ["bench", "benchmark"] ->
        resolveDefaultComponent knownNames isCBenchName
    | otherwise ->
        resolveDefaultComponent knownNames $ \x -> case componentNameString x of
          Nothing -> False
          Just xs -> unUnqualComponentName xs == name

specialComponents :: Set ComponentName -> Set String
specialComponents knownNames =
  S.fromList $
    mapMaybe isResolvable ["lib", "flib", "exe", "test", "bench"]
  where
    isResolvable xs = case resolveToComponentName knownNames (Just xs) of
      Resolved {} -> Just xs
      _ -> Nothing

resolveToCommonStanza :: Set CommonStanza -> Maybe String -> Resolution CommonStanza
resolveToCommonStanza knownNames (Just (CommonStanza . B.pack -> name))
  | S.member name knownNames = Resolved name
resolveToCommonStanza _ _ = NotFound

isSection :: Field ann -> Bool
isSection = \case
  Field {} -> False
  Section {} -> True

-- | Parse Cabal file into two representations.
parseCabalFile
  :: MonadError String m
  => FilePath
  -- ^ File name, just for error reporting.
  -> ByteString
  -- ^ Contents of the Cabal file.
  -> m ([Field Position], GenericPackageDescription)
  -- ^ Parsed data, suitable for 'resolveComponent'.
parseCabalFile fileName contents = do
  let legacyErr = "Legacy, unsectioned Cabal files are unsupported"
      errorWithCtx msg =
        throwError $
          "Cannot parse input Cabal file "
            ++ fileName
            ++ " because:\n"
            ++ msg

  fields <- case readFields contents of
    Left err -> errorWithCtx $ show err
    Right fs
      | any isSection fs -> pure fs
      | otherwise -> errorWithCtx legacyErr

  packDescr <- case snd $ runParseResult $ parseGenericPackageDescription contents of
    Left (_, err) ->
      errorWithCtx $ L.unlines $ map (showPError fileName) $ NE.toList err
    Right GenericPackageDescription {packageDescription = PackageDescription {specVersion = CabalSpecV1_0}} ->
      errorWithCtx legacyErr
    Right pd -> pure pd

  pure (fields, packDescr)

readBuildTarget :: PackageDescription -> String -> Maybe ComponentName
readBuildTarget pkg targetStr =
  readBuildTarget' pkg targetStr <|> readBuildTarget'' pkg targetStr

readBuildTarget' :: PackageDescription -> String -> Maybe ComponentName
readBuildTarget' pkg targetStr = do
  let (_, utargets) = readUserBuildTargets [targetStr]
  [utarget] <- pure utargets
  let (_, btargets) = resolveBuildTargets pkg [(utarget, False)]
  [BuildTargetComponent btarget] <- pure btargets
  pure btarget

-- | Surprisingly, 'resolveBuildTargets' does not support package component.
-- Let's work around this limitation manually for now.
readBuildTarget'' :: PackageDescription -> String -> Maybe ComponentName
readBuildTarget'' pkg targetStr = do
  (pref, ':' : suff) <- pure $ span (/= ':') targetStr
  guard $ unPackageName (pkgName (package pkg)) == pref
  readBuildTarget' pkg suff

-- | Resolve a raw component name.
resolveComponent
  :: MonadError String m
  => FilePath
  -- ^ File name, just for error reporting.
  -> ([Field a], GenericPackageDescription)
  -- ^ Parsed Cabal file, as returned by 'parseCabalFile'.
  -> Maybe String
  -- ^ Component name (or default component if 'Nothing'),
  -- roughly adhering to the syntax
  -- of [component targets](https://cabal.readthedocs.io/en/3.12/cabal-commands.html#target-forms).
  -> m (Either CommonStanza ComponentName)
  -- ^ Resolved component.
resolveComponent _ (_, gpd) (Just component)
  | Just cmp <- readBuildTarget (flattenPackageDescription gpd) component =
      pure $ Right cmp
resolveComponent
  fileName
  (extractCommonStanzas -> commonStanzas, extractComponentNames -> componentNames)
  component = case resolution of
    NotFound -> throwError $ case component of
      Nothing ->
        "Default target component not found in "
          ++ fileName
          ++ ".\n"
          ++ knownTargetsHint
      Just cmp ->
        "Target component '"
          ++ cmp
          ++ "' not found in "
          ++ fileName
          ++ ".\n"
          ++ knownTargetsHint
    Resolved cmp -> pure cmp
    Ambiguous ->
      throwError $
        "Target component is ambiguous.\n"
          ++ knownTargetsHint
    where
      allTargets :: Set String
      allTargets =
        S.fromList (mapMaybe (fmap unUnqualComponentName . componentNameString) (S.toList componentNames))
          <> S.map (B.unpack . unCommonStanza) commonStanzas
          <> specialComponents componentNames

      knownTargetsHint :: String
      knownTargetsHint =
        "Specify one with -c: "
          ++ L.intercalate ", " (S.toList allTargets)
          ++ "."

      resolution :: Resolution (Either CommonStanza ComponentName)
      resolution =
        fmap Right (resolveToComponentName componentNames component)
          <> fmap Left (resolveToCommonStanza commonStanzas component)

-- | Validate [dependency syntax](https://cabal.readthedocs.io/en/3.12/cabal-package-description-file.html#pkg-field-build-depends),
-- checking whether Cabal would be able to parse it.
validateDependency
  :: MonadError String m
  => CabalSpecVersion
  -- ^ Cabal format version to adhere to.
  -> String
  -- ^ Raw dependency to add.
  -> m ByteString
  -- ^ Validated dependency as 'ByteString' (or an error).
validateDependency specVer d = case eitherParsec d of
  Right (_ :: Dependency)
    | specVer < CabalSpecV3_0 && elem ':' d ->
        throwError $
          "Cannot use the specified dependency '"
            ++ d
            ++ "' because cabal-version must be at least 3.0."
    | otherwise -> pure $ B.pack d
  Left err ->
    throwError $
      "Cannot parse the specified dependency '"
        ++ d
        ++ "' because:\n"
        ++ err

splitAtPositionLine :: Position -> ByteString -> (ByteString, ByteString)
splitAtPositionLine (Position line _row) = splitAtPosition (Position line 1)

findNonImportField :: [Field Position] -> Maybe Position
findNonImportField (Section _ _ subFields : rest) =
  case filter (not . isImportField) subFields of
    fld : _ -> Just $ getFieldNameAnn fld
    [] -> case rest of
      fld : _ -> case getFieldNameAnn fld of
        Position line _ -> Just (Position line defaultRow)
      [] -> Just (Position maxBound defaultRow)
  where
    defaultRow = case reverse subFields of
      [] -> 3
      fld : _ -> case getFieldNameAnn fld of
        Position _ row -> row
findNonImportField _ = Nothing

isImportField :: Field a -> Bool
isImportField = \case
  Field (Name _ fieldName) _ -> fieldName == "import"
  Section {} -> False

getFieldNameAnn :: Field ann -> ann
getFieldNameAnn = \case
  Field (Name ann _) _ -> ann
  Section (Name ann _) _ _ -> ann

isTargetField :: ByteString -> Field ann -> Bool
isTargetField name = \case
  Field (Name _ fieldName) _ -> name == fieldName
  _ -> False

detectLeadingSeparator :: ByteString -> Maybe ByteString
detectLeadingSeparator xs = case B.uncons xs of
  Just (',', ys) -> Just $ B.cons ',' $ B.takeWhile (== ' ') ys
  Just (' ', ys) -> Just $ B.takeWhile (== ' ') ys
  _ -> Nothing

isCommaSeparated :: ByteString -> Bool
isCommaSeparated xs = ',' `B.elem` xs

dropRepeatingSpaces :: ByteString -> ByteString
dropRepeatingSpaces xs = case B.uncons xs of
  Just (' ', ys) -> B.cons ' ' (B.dropWhile (== ' ') ys)
  _ -> xs

-- | Find a target section and insert new
-- fields at the beginning, trying our best
-- to preserve formatting. This often breaks however
-- if there are comments in between target fields.
fancyAddAlgorithm :: AddConfig -> Maybe ByteString
fancyAddAlgorithm AddConfig {cnfFields, cnfComponent, cnfOrigContents, cnfAdditions, cnfTargetField} = do
  subFields : _ <- pure $ mapMaybe (isComponent cnfComponent) cnfFields
  targetField <- L.find (isTargetField $ getTargetName cnfTargetField) subFields
  Field _ (FieldLine firstDepPos _dep : restDeps) <- pure targetField

  -- This is not really the second dependency:
  -- it's a dependency on the next line.
  let secondDepPos = case restDeps of
        FieldLine pos _dep : _ -> Just pos
        _ -> Nothing
      fillerPred c = isSpaceChar8 c || c == ','

  let (rawPref, rawSuff) = splitAtPosition firstDepPos cnfOrigContents
      pref = B.takeWhileEnd fillerPred rawPref
      suff = B.takeWhile fillerPred rawSuff
      prefSuff = pref <> suff
      commaSeparation = isCommaSeparated rawSuff || requiresCommas cnfTargetField
      (afterLast, inBetween, beforeFirst) = case secondDepPos of
        Nothing ->
          ( if B.any (== ',') prefSuff || not commaSeparation then pref' else "," <> pref'
          , if B.any (== ',') prefSuff || not commaSeparation then prefSuff' else "," <> prefSuff'
          , suff
          )
          where
            prefSuff' = dropRepeatingSpaces prefSuff
            pref' = dropRepeatingSpaces pref
        Just pos ->
          ( if B.any (== ',') suff then pref1 else prefSuff1
          , prefSuff1
          , suff
          )
          where
            prefSuff1 = pref1 <> suff1
            (B.takeWhileEnd fillerPred -> pref1, B.takeWhile fillerPred -> suff1) =
              splitAtPosition pos cnfOrigContents

  let (beforeFirstDep, afterFirstDep) = splitAtPosition firstDepPos cnfOrigContents
      newFieldContents = beforeFirst <> B.intercalate inBetween (NE.toList cnfAdditions) <> afterLast

  let ret = beforeFirstDep <> newFieldContents <> afterFirstDep
  pure ret

-- | Find a target section and insert new
-- fields at the beginning. Very limited effort
-- is put into preserving formatting.
niceAddAlgorithm :: AddConfig -> Maybe ByteString
niceAddAlgorithm AddConfig {cnfFields, cnfComponent, cnfOrigContents, cnfAdditions, cnfTargetField} = do
  subFields : _ <- pure $ mapMaybe (isComponent cnfComponent) cnfFields
  targetField <- L.find (isTargetField (getTargetName cnfTargetField)) subFields
  Field _ (FieldLine pos _dep : _) <- pure targetField

  let (before, after) = splitAtPosition pos cnfOrigContents
      (_, targetHeader) = splitAtPosition (getFieldNameAnn targetField) before
      leadingSeparatorStyle = detectLeadingSeparator after
      filler = dropRepeatingSpaces $ B.drop 1 $ B.dropWhile (/= ':') targetHeader
      defaultSep = if isCommaSeparated after || cnfTargetField == BuildDepends then "," else ""
      filler' = maybe (defaultSep <> filler) (filler <>) leadingSeparatorStyle
      newFieldContents =
        fromMaybe "" leadingSeparatorStyle
          <> B.intercalate filler' (NE.toList cnfAdditions)
          <> (if isJust leadingSeparatorStyle then filler else filler')
  pure $
    before <> newFieldContents <> after

-- | Introduce a new target section
-- after the last common stanza import.
-- This is not fancy, but very robust.
roughAddAlgorithm :: AddConfig -> Maybe ByteString
roughAddAlgorithm AddConfig {cnfFields, cnfComponent, cnfOrigContents, cnfAdditions, cnfTargetField} = do
  let componentAndRest = L.dropWhile (isNothing . isComponent cnfComponent) cnfFields
  pos@(Position _ row) <- findNonImportField componentAndRest
  let (before, after) = splitAtPositionLine pos cnfOrigContents
      lineEnding' = B.takeWhileEnd isSpaceChar8 before
      lineEnding = if B.null lineEnding' then "\n" else lineEnding'
      needsNewlineBefore = maybe False ((/= '\n') . snd) (B.unsnoc before)
      buildDeps =
        (if needsNewlineBefore then lineEnding else "")
          <> B.replicate (row - 1) ' '
          <> (getTargetName cnfTargetField <> ": ")
          <> B.intercalate ", " (NE.toList cnfAdditions)
          <> lineEnding
  pure $
    before <> buildDeps <> after

-- | The main workhorse, adding fields to a specified component
-- in the Cabal file.
executeAddConfig
  :: (Either CommonStanza ComponentName -> ByteString -> Bool)
  -- ^ How to validate results? See 'validateChanges'.
  -> AddConfig
  -- ^ Input arguments.
  -> Maybe ByteString
  -- ^ Updated contents, if validated successfully.
executeAddConfig validator cnf@AddConfig {cnfComponent} =
  L.find (validator cnfComponent) $
    mapMaybe ($ cnf) [fancyAddAlgorithm, niceAddAlgorithm, roughAddAlgorithm]

-- | Validate that updates did not cause unexpected effects on other sections
-- of the Cabal file.
validateChanges
  :: GenericPackageDescription
  -- ^ Original package description.
  -> Either CommonStanza ComponentName
  -- ^ Which component was supposed to be updated?
  -- Usually constructed by 'resolveComponent'.
  -> ByteString
  -- ^ Updated Cabal file.
  -> Bool
  -- ^ Was the update successful?
validateChanges origPackDesc (Left _commonStanza) newContents =
  case parseGenericPackageDescriptionMaybe newContents of
    Nothing -> False
    Just newPackDesc ->
      packageDescription origPackDesc == packageDescription newPackDesc
        && gpdScannedVersion origPackDesc == gpdScannedVersion newPackDesc
        && genPackageFlags origPackDesc == genPackageFlags newPackDesc
validateChanges origPackDesc (Right component) newContents =
  case parseGenericPackageDescriptionMaybe newContents of
    Nothing -> False
    Just newPackDesc ->
      packageDescription origPackDesc == packageDescription newPackDesc
        && gpdScannedVersion origPackDesc == gpdScannedVersion newPackDesc
        && genPackageFlags origPackDesc == genPackageFlags newPackDesc
        && mainLibMatch
        && subLibsMatch
        && foreignLibsMatch
        && executablesMatch
        && testsMatch
        && benchmarksMatch
      where
        mainLibMatch = case (condLibrary origPackDesc, condLibrary newPackDesc) of
          (Nothing, Nothing) -> True
          (Just x, Just y) -> component == CLibName LMainLibName || x == y
          _ -> False

        subLibsMatch = length xs == length ys && and (zipWith predicate xs ys)
          where
            xs = condSubLibraries origPackDesc
            ys = condSubLibraries newPackDesc
            predicate x y = x == y || isCLibName component && fst x == fst y && componentNameString component == Just (fst x)

        foreignLibsMatch = length xs == length ys && and (zipWith predicate xs ys)
          where
            xs = condForeignLibs origPackDesc
            ys = condForeignLibs newPackDesc
            predicate x y = x == y || isCFLibName component && fst x == fst y && componentNameString component == Just (fst x)

        executablesMatch = length xs == length ys && and (zipWith predicate xs ys)
          where
            xs = condExecutables origPackDesc
            ys = condExecutables newPackDesc
            predicate x y = x == y || isCExeName component && fst x == fst y && componentNameString component == Just (fst x)

        testsMatch = length xs == length ys && and (zipWith predicate xs ys)
          where
            xs = condTestSuites origPackDesc
            ys = condTestSuites newPackDesc
            predicate x y = x == y || isCTestName component && fst x == fst y && componentNameString component == Just (fst x)

        benchmarksMatch = length xs == length ys && and (zipWith predicate xs ys)
          where
            xs = condBenchmarks origPackDesc
            ys = condBenchmarks newPackDesc
            predicate x y = x == y || isCBenchName component && fst x == fst y && componentNameString component == Just (fst x)

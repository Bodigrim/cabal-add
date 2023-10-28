{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
--
-- Building blocks of @cabal-add@ executable,
-- probably not terribly useful on their own.
module Distribution.Client.Add (
  CommonStanza (..),
  Config (..),
  parseCabalFile,
  resolveComponent,
  validateDependency,
  executeConfig,
  validateChanges,
) where

import Control.Monad (forM, foldM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char
import qualified Data.Foldable1 as F1
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Distribution.CabalSpecVersion
import Distribution.Fields
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Parsec

-- | Just a newtype wrapper, since @Cabal-syntax@ does not provide any.
newtype CommonStanza = CommonStanza {unCommonStanza :: ByteString}
  deriving (Eq, Ord, Show)

-- | An input for 'executeConfig'.
data Config = Config
  { cnfOrigContents :: !ByteString
  -- ^ Original Cabal file (with quirks patched,
  -- see "Distribution.PackageDescription.Quirks"),
  -- must be in sync with 'cnfFields'.
  , cnfFields :: ![Field Position]
  -- ^ Parsed (by 'readFields') representation of the Cabal file,
  -- must be in sync with 'cnfOrigContents'.
  , cnfComponents :: !(NonEmpty (Either CommonStanza ComponentName))
  -- ^ Which component to update?
  , cnfDependencies :: !(NonEmpty ByteString)
  -- ^ Which dependencies to add?
  }
  deriving (Eq, Show)

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
  :: FilePath
  -- ^ File name, just for error reporting.
  -> ByteString
  -- ^ Contents of the Cabal file.
  -> Either String ([Field Position], GenericPackageDescription)
  -- ^ Either error or parsed data.
parseCabalFile fileName contents = do
  let legacyErr = "Legacy, unsectioned Cabal files are unsupported"
      errorWithCtx msg =
        Left $
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

-- | Resolve a raw component name.
resolveComponent
  :: FilePath
  -- ^ File name, just for error reporting.
  -> ([Field Position], GenericPackageDescription)
  -- ^ Parsed Cabal file, as returned by 'parseCabalFile'.
  -> Maybe String
  -- ^ Component name (default component if 'Nothing').
  -> Either String (Either CommonStanza ComponentName)
  -- ^ Either error or resolved component.
resolveComponent
  fileName
  (extractCommonStanzas -> commonStanzas, extractComponentNames -> componentNames)
  component = case resolution of
    NotFound -> Left $ case component of
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
      Left $
        "Target component is ambiguous.\n"
          ++ knownTargetsHint
    where
      allTargets =
        S.fromList (mapMaybe (fmap unUnqualComponentName . componentNameString) (S.toList componentNames))
          <> S.map (B.unpack . unCommonStanza) commonStanzas
          <> specialComponents componentNames
      knownTargetsHint =
        "Specify one with -c: "
          ++ L.intercalate ", " (S.toList allTargets)
          ++ "."
      resolution =
        fmap Right (resolveToComponentName componentNames component)
          <> fmap Left (resolveToCommonStanza commonStanzas component)

-- | Validate dependency syntax.
validateDependency
  :: String
  -- ^ Raw dependency to add
  -> Either String ByteString
  -- ^ Either error or dependency as 'ByteString'.
validateDependency d = case eitherParsec d of
  Right (_ :: Dependency) -> pure $ B.pack d
  Left err ->
    Left $
      "Cannot parse the specified dependency '"
        ++ d
        ++ "' because:\n"
        ++ err

-- Both lines and rows are 1-based.
splitAtPosition :: Position -> ByteString -> (ByteString, ByteString)
splitAtPosition (Position line row) bs
  | line <= 1 = B.splitAt (row - 1) bs
  | otherwise = case L.drop (line - 2) nls of
      [] -> (bs, mempty)
      nl : _ -> B.splitAt (nl + row) bs
  where
    nls = B.elemIndices '\n' bs

splitAtPositionLine :: Position -> ByteString -> (ByteString, ByteString)
splitAtPositionLine (Position line _row) = splitAtPosition (Position line 1)

isComponent :: Either CommonStanza ComponentName -> Field a -> Bool
isComponent (Right cmp) = \case
  Section (Name _ "library") [] _subFields
    | cmp == CLibName LMainLibName ->
        True
  Section (Name _ sectionName) [SecArgName _pos sectionArg] _subFields
    | sectionName <> " " <> sectionArg == B.pack (componentNameStanza cmp) ->
        True
  Section (Name _ sectionName) [SecArgStr _pos sectionArg] _subFields
    | sectionName <> " " <> sectionArg == B.pack (componentNameStanza cmp) ->
        True
  _ -> False
isComponent (Left (CommonStanza commonName)) = \case
  Section (Name _ "common") [SecArgName _pos sectionArg] _subFields ->
    sectionArg == commonName
  Section (Name _ "common") [SecArgStr _pos sectionArg] _subFields ->
    sectionArg == commonName
  _ -> False

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

isBuildDependsField :: Field ann -> Bool
isBuildDependsField = \case
  Field (Name _ "build-depends") _ -> True
  _ -> False

detectLeadingComma :: ByteString -> Maybe ByteString
detectLeadingComma xs = case B.uncons xs of
  Just (',', ys) -> Just $ B.cons ',' $ B.takeWhile (== ' ') ys
  _ -> Nothing

dropRepeatingSpaces :: ByteString -> ByteString
dropRepeatingSpaces xs = case B.uncons xs of
  Just (' ', ys) -> B.cons ' ' (B.dropWhile (== ' ') ys)
  _ -> xs

-- | Find build-depends section and insert new
-- dependencies at the beginning, trying our best
-- to preserve formatting. This often breaks however
-- if there are comments in between build-depends.
fancyAlgorithm :: Config -> Maybe ByteString
fancyAlgorithm Config {cnfFields, cnfComponents, cnfOrigContents, cnfDependencies} = Nothing
{--
  component <- L.find (isComponent cnfComponent) cnfFields
  Section _ _ subFields <- pure component
  buildDependsField <- L.find isBuildDependsField subFields
  Field _ (FieldLine firstDepPos _dep : restDeps) <- pure buildDependsField

  -- This is not really the second dependency:
  -- it's a dependency on the next line.
  let secondDepPos = case restDeps of
        FieldLine pos _dep : _ -> Just pos
        _ -> Nothing
      fillerPred c = isSpace c || c == ','

  let (B.takeWhileEnd fillerPred -> pref, B.takeWhile fillerPred -> suff) =
        splitAtPosition firstDepPos cnfOrigContents
      prefSuff = pref <> suff

      (afterLast, inBetween, beforeFirst) = case secondDepPos of
        Nothing ->
          ( if B.any (== ',') prefSuff then pref' else "," <> pref'
          , if B.any (== ',') prefSuff then prefSuff' else "," <> prefSuff'
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
      newBuildDeps = beforeFirst <> B.intercalate inBetween (NE.toList cnfDependencies) <> afterLast

  let ret = beforeFirstDep <> newBuildDeps <> afterFirstDep
  pure ret
--}

-- | Find build-depends section and insert new
-- dependencies at the beginning. Very limited effort
-- is put into preserving formatting.
niceAlgorithm :: Config -> Maybe ByteString
niceAlgorithm Config {cnfFields, cnfComponents, cnfOrigContents, cnfDependencies} = do
  positions <- L.sortOn (\(Field _ (FieldLine (Position line _) _ : _)) -> line) <$> forM (NE.toList cnfComponents) getField
  Just (insertBuildDeps positions)

 where
  insertBuildDeps :: [Field Position] -> ByteString
  insertBuildDeps = go 0 0 cnfOrigContents
   where
    go :: Int -> Int -> ByteString -> [Field Position] -> ByteString
    go _ _ content [] = content
    go offset rowOffset content (buildDependsField@(Field _ (FieldLine pos@(Position line row) _dep : _)):rest) =
      let (before, after) = splitAtPosition pos content
          (_, buildDepsHeader) = splitAtPosition (Position (line - offset) (row - rowOffset)) before
          filler = dropRepeatingSpaces $ B.drop 1 $ B.dropWhile (/= ':') buildDepsHeader
          leadingCommaStyle = detectLeadingComma after
          filler' = maybe ("," <> filler) (filler <>) leadingCommaStyle
          newBuildDeps =
            fromMaybe "" leadingCommaStyle
              <> B.intercalate filler' (NE.toList cnfDependencies)
              <> (if isJust leadingCommaStyle then filler else filler')
          insertLen = (length . B.lines) before + (length . B.lines) newBuildDeps - 1
      in before <> newBuildDeps <> go (offset + insertLen) row after rest
    go _ _ _ _ = error "Internal error"

  getField :: Either CommonStanza ComponentName -> Maybe (Field Position)
  getField cnfComponent = do
    component <- L.find (isComponent cnfComponent) cnfFields
    Section _ _ subFields <- pure component
    buildDependsField <- L.find isBuildDependsField subFields
    p@(Field _ (FieldLine _ _ : _)) <- pure buildDependsField
    pure p


-- | Introduce a new build-depends section
-- after the last common stanza import.
-- This is not fancy, but very robust.
roughAlgorithm :: Config -> Maybe ByteString
roughAlgorithm Config {cnfFields, cnfComponents, cnfOrigContents, cnfDependencies} = do
  positions <- L.sortOn (\(Position line _) -> line) <$> forM (NE.toList cnfComponents) getPos
  Just (insertBuildDeps positions)
 where
  insertBuildDeps :: [Position] -> ByteString
  insertBuildDeps = go 0 cnfOrigContents
   where
    go :: Int -> ByteString -> [Position] -> ByteString
    go _ content [] = content
    go offset content (Position line row:rest) =
      let (before, after) = splitAtPositionLine (Position (line - offset) row) content
          lineEnding' = B.takeWhileEnd isSpace before
          lineEnding = if B.null lineEnding' then "\n" else lineEnding'
          needsNewlineBefore = maybe False ((/= '\n') . snd) (B.unsnoc before)
          buildDeps =
            (if needsNewlineBefore then lineEnding else "")
              <> B.replicate (row - 1) ' '
              <> "build-depends: "
              <> B.intercalate ", " (NE.toList cnfDependencies)
              <> lineEnding
          insertLen = (length . B.lines) before + (length . B.lines) buildDeps -1
      in before <> buildDeps <> go (offset + insertLen) after rest

  getPos :: Either CommonStanza ComponentName -> Maybe Position
  getPos cnfComponent = do
    let componentAndRest = L.dropWhile (not . isComponent cnfComponent) cnfFields
    findNonImportField componentAndRest


-- | Main work horse of the module, adding dependencies to a specified component
-- in the Cabal file.
executeConfig
  :: (Either CommonStanza ComponentName -> ByteString -> Bool)
  -- ^ How to validate results? See 'validateChanges'.
  -> Config
  -- ^ Input arguments.
  -> Maybe ByteString
  -- ^ Updated contents, if validated successfully.
executeConfig validator cnf@Config {cnfComponents} =
  L.find (\bs -> and ((\cnfComponent -> validator cnfComponent bs) <$> cnfComponents)) $
    mapMaybe ($ cnf) [niceAlgorithm]

-- | Validate that updates did not cause unexpected effects on other sections
-- of the Cabal file.
validateChanges
  :: GenericPackageDescription
  -- ^ Original package description.
  -> Either CommonStanza ComponentName
  -- ^ Which component was supposed to be updated?
  -> ByteString
  -- ^ Update Cabal file.
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

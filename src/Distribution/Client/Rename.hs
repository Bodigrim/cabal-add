{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
module Distribution.Client.Rename (
  parseCabalFile,
  resolveComponent,
  CommonStanza (..),
  validateDependency,
  RenameConfig (..),
  executeRenameConfig,
  validateChanges,
  TargetField (..),
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isPunctuation, isSpace)
import Data.Foldable (fold)
import Data.List qualified as L
import Distribution.Client.Add (
  TargetField (..),
  parseCabalFile,
  resolveComponent,
  validateChanges,
  validateDependency,
 )
import Distribution.Client.Common (CommonStanza (..), getTargetName, isComponent, splitAtPosition)
import Distribution.Fields (
  Field (..),
  FieldLine (..),
  Name (..),
  SectionArg (..),
 )
import Distribution.PackageDescription (
  ComponentName (..),
 )
import Distribution.Parsec (
  Position (..),
 )

-- | An input for 'executeRenameConfig'.
data RenameConfig = RenameConfig
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
  -- ^ In which field to rename the provided content?
  , cnfRenameFrom :: !ByteString
  -- ^ Rename what?
  , cnfRenameTo :: !ByteString
  -- ^ Rename to what?
  }
  deriving (Eq, Show)

-- | The main workhorse, renaming fields in a specified component
-- in the Cabal file.
executeRenameConfig
  :: (Either CommonStanza ComponentName -> ByteString -> Bool)
  -- ^ How to validate results? See 'validateChanges'.
  -> RenameConfig
  -- ^ Input arguments.
  -> Maybe ByteString
  -- ^ Updated contents, if validated successfully.
executeRenameConfig !_ RenameConfig {cnfRenameFrom}
  | B.null cnfRenameFrom = Nothing
executeRenameConfig validator RenameConfig {cnfFields, cnfComponent, cnfOrigContents, cnfRenameFrom, cnfRenameTo, cnfTargetField} = do
  let fieldsWithSource = annotateFieldsWithSource cnfOrigContents cnfFields
      fieldsWithSource' = map replaceInSection fieldsWithSource
      newContents = foldMap fold fieldsWithSource'
  if validator cnfComponent newContents then Just newContents else Nothing
  where
    replaceInBS :: ByteString -> ByteString
    replaceInBS hay
      | B.null rest =
          hay
      | not startsWithBoundary || not endsWithBoundary =
          pref <> cnfRenameFrom <> replaceInBS suff
      | otherwise =
          pref <> cnfRenameTo <> replaceInBS suff
      where
        (pref, rest) = B.breakSubstring cnfRenameFrom hay
        suff = B.drop (B.length cnfRenameFrom) rest
        isTokenBoundary c = (isSpace c || isPunctuation c || c `elem` "^>=<") && c /= '-' && c /= '.'
        startsWithBoundary = maybe True (isTokenBoundary . snd) (B.unsnoc pref)
        endsWithBoundary = maybe True (isTokenBoundary . fst) (B.uncons suff)

    replaceInFieldLine :: FieldLine ByteString -> FieldLine ByteString
    replaceInFieldLine (FieldLine ann cnt) = FieldLine (replaceInBS ann) (replaceInBS cnt)

    replaceInField :: Field ByteString -> Field ByteString
    replaceInField = \case
      Field name@(Name _ann fieldName) fls
        | fieldName == getTargetName cnfTargetField ->
            Field name (map replaceInFieldLine fls)
      fld@Field {} -> fld
      Section name args subFields -> Section name args (map replaceInField subFields)

    replaceInSection :: Field ByteString -> Field ByteString
    replaceInSection = \case
      sct@(Section name args subFields) -> Section name args (map func subFields)
        where
          func = case isComponent cnfComponent sct of
            Nothing -> replaceInSection
            Just {} -> replaceInField
      fld@Field {} -> fld

annotateFieldsWithSource :: ByteString -> [Field Position] -> [Field ByteString]
annotateFieldsWithSource bs = snd . L.mapAccumR annotateField maxBoundPos
  where
    annotateField :: Position -> Field Position -> (Position, Field ByteString)
    annotateField finishPos = \case
      Field (Name pos name) fls -> (pos, Field (Name (getSrcBetween pos finishPos') name) fls')
        where
          (finishPos', fls') = L.mapAccumR annotateFieldLine finishPos fls
      Section (Name pos name) args fs -> (pos, Section (Name (getSrcBetween pos finishPos'') name) args' fs')
        where
          (finishPos', fs') = L.mapAccumR annotateField finishPos fs
          (finishPos'', args') = L.mapAccumR annotateSectionArg finishPos' args

    annotateFieldLine :: Position -> FieldLine Position -> (Position, FieldLine ByteString)
    annotateFieldLine finishPos (FieldLine pos xs) = (pos, FieldLine (getSrcBetween pos finishPos) xs)

    annotateSectionArg :: Position -> SectionArg Position -> (Position, SectionArg ByteString)
    annotateSectionArg finishPos = \case
      SecArgName pos xs -> (pos, SecArgName (getSrcBetween pos finishPos) xs)
      SecArgStr pos xs -> (pos, SecArgStr (getSrcBetween pos finishPos) xs)
      SecArgOther pos xs -> (pos, SecArgOther (getSrcBetween pos finishPos) xs)

    getSrcBetween :: Position -> Position -> ByteString
    getSrcBetween from to = snd $ splitAtPosition from $ fst $ splitAtPosition to bs

    maxBoundPos :: Position
    maxBoundPos = Position maxBound maxBound

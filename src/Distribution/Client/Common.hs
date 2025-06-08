{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
module Distribution.Client.Common (
  CommonStanza (..),
  isComponent,
  splitAtPosition,
  TargetField (..),
  getTargetName,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List qualified as L
import Distribution.Fields (
  Field (..),
  Name (..),
  SectionArg (..),
 )
import Distribution.PackageDescription (
  ComponentName (..),
  LibraryName (..),
  componentNameStanza,
 )
import Distribution.Parsec.Position (Position (..))

-- | Just a newtype wrapper, since @Cabal-syntax@ does not provide any.
newtype CommonStanza = CommonStanza {unCommonStanza :: ByteString}
  deriving (Eq, Ord, Show)

isComponent :: Either CommonStanza ComponentName -> Field a -> Maybe [Field a]
isComponent (Right cmp) = \case
  Section (Name _ "library") [] subFields
    | cmp == CLibName LMainLibName ->
        Just subFields
  Section (Name _ sectionName) [SecArgName _pos sectionArg] subFields
    | sectionName <> " " <> sectionArg == B.pack (componentNameStanza cmp) ->
        Just subFields
  Section (Name _ sectionName) [SecArgStr _pos sectionArg] subFields
    | sectionName <> " " <> sectionArg == B.pack (componentNameStanza cmp) ->
        Just subFields
  _ -> Nothing
isComponent (Left (CommonStanza commonName)) = \case
  Section (Name _ "common") [SecArgName _pos sectionArg] subFields
    | sectionArg == commonName ->
        Just subFields
  Section (Name _ "common") [SecArgStr _pos sectionArg] subFields
    | sectionArg == commonName ->
        Just subFields
  _ -> Nothing

-- Both lines and rows are 1-based.
splitAtPosition :: Position -> ByteString -> (ByteString, ByteString)
splitAtPosition (Position line row) bs
  | line <= 1 = B.splitAt (row - 1) bs
  | otherwise = case L.drop (line - 2) nls of
      [] -> (bs, mempty)
      nl : _ -> B.splitAt (nl + row) bs
  where
    nls = B.elemIndices '\n' bs

-- | A field in a cabal file, new content can be added to
data TargetField
  = -- | Corresponds to @build-depends@ in the cabal file
    BuildDepends
  | -- | Corresponds to @exposed-modules@ in the cabal file
    ExposedModules
  | -- | Corresponds to @other-modules@ in the cabal file
    OtherModules
  deriving (Eq, Show, Ord)

getTargetName :: TargetField -> ByteString
getTargetName = \case
  BuildDepends -> "build-depends"
  ExposedModules -> "exposed-modules"
  OtherModules -> "other-modules"

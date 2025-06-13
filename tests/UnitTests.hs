{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
module Main (main) where

import Data.Algorithm.Diff (Diff, PolyDiff (..), getDiff)
import Data.ByteString.Char8 qualified as B
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.String.QQ (s)
import Distribution.Client.Add (AddConfig (..), TargetField (..), executeAddConfig, parseCabalFile)
import Distribution.Client.Rename (RenameConfig (..), executeRenameConfig)
import Distribution.Fields.Field (Field)
import Distribution.PackageDescription (ComponentName (..), LibraryName (..))
import Distribution.Parsec.Position (Position)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)

data CabalAddTest = CabalAddTest
  { catName :: String
  , catConfig :: AddConfig
  , catOutput :: String
  }

instance IsTest CabalAddTest where
  testOptions = pure []

  run _opts CabalAddTest {..} _yieldProgress = do
    let outputM = executeAddConfig (const $ const True) catConfig
    case outputM of
      Nothing -> pure $ testFailed "config could not be applied"
      Just output ->
        pure $
          if B.unpack output == catOutput
            then testPassed ""
            else testFailed $ prettyDiff $ getDiff (lines catOutput) (lines (B.unpack output))

data CabalRenameTest = CabalRenameTest
  { crtName :: String
  , crtConfig :: RenameConfig
  , crtOutput :: String
  }

instance IsTest CabalRenameTest where
  testOptions = pure []

  run _opts CabalRenameTest {..} _yieldProgress = do
    let outputM = executeRenameConfig (const $ const True) crtConfig
    case outputM of
      Nothing -> pure $ testFailed "config could not be applied"
      Just output ->
        pure $
          if B.unpack output == crtOutput
            then testPassed ""
            else testFailed $ prettyDiff $ getDiff (lines crtOutput) (lines (B.unpack output))

parseCabalFileOrError :: B.ByteString -> [Field Position]
parseCabalFileOrError inContents = case parseCabalFile "" inContents of
  Right (fields, _) -> fields
  Left err -> error $ "Failed to parse cabal file with error: " <> err

caseMultipleBuildDependencies1 :: TestTree
caseMultipleBuildDependencies1 =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple dependencies 1"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CLibName LMainLibName
            , cnfAdditions = NE.fromList ["foo < 1 && >0.7", "baz ^>= 2.0"]
            , cnfTargetField = BuildDepends
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    foo < 1 && >0.7,
    baz ^>= 2.0,
    base >=4.15 && <5
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
|]

caseMultipleExposedModules1 :: TestTree
caseMultipleExposedModules1 =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple exposed modules 1"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CLibName LMainLibName
            , cnfAdditions = NE.fromList ["Test.Mod1", "Test.Mod2"]
            , cnfTargetField = ExposedModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1, Test.Mod2, Main, OtherModule.Mine
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Main, OtherModule.Mine
|]

caseMultipleExposedModulesUsingSpaces :: TestTree
caseMultipleExposedModulesUsingSpaces =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple exposed modules with spaces"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CLibName LMainLibName
            , cnfAdditions = NE.fromList ["Test.Mod1", "Test.Mod2"]
            , cnfTargetField = ExposedModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1 Test.Mod2 Main OtherModule.Mine
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Main OtherModule.Mine
|]

caseMultipleOtherModules :: TestTree
caseMultipleOtherModules =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple other modules"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CTestName "testss"
            , cnfAdditions = NE.fromList ["Test.Mod1", "Mod3"]
            , cnfTargetField = OtherModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1 Test.Mod2
test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:
    Test.Mod1,
    Mod3,
    Dir.Mod1,
    Dir.Mod2
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1 Test.Mod2
test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:
    Dir.Mod1,
    Dir.Mod2
|]

caseMultipleOtherModulesUsingSpaces :: TestTree
caseMultipleOtherModulesUsingSpaces =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple other modules with space separators"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CTestName "testss"
            , cnfAdditions = NE.fromList ["Test.Mod1", "Mod3"]
            , cnfTargetField = OtherModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:
    Test.Mod1
    Mod3
    Dir.Mod1
    Dir.Mod2

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1, Test.Mod2
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:
    Dir.Mod1
    Dir.Mod2

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1, Test.Mod2
|]

caseMultipleOtherModulesUsingLeadingCommas :: TestTree
caseMultipleOtherModulesUsingLeadingCommas =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple other modules preserving leading commas"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CTestName "testss"
            , cnfAdditions = NE.fromList ["Test.Mod1", "Mod3"]
            , cnfTargetField = OtherModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:  Test.Mod1
                , Mod3
                , Dir.Mod1
                , Dir.Mod2

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1, Test.Mod2
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:  Dir.Mod1
                , Dir.Mod2

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1, Test.Mod2
|]

caseMultipleOtherModulesUsingLeadingSpaces :: TestTree
caseMultipleOtherModulesUsingLeadingSpaces =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple other modules preserving leading spaces"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CTestName "testss"
            , cnfAdditions = NE.fromList ["Test.Mod1", "Mod3"]
            , cnfTargetField = OtherModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:  Test.Mod1
                  Mod3
                  Dir.Mod1
                  Dir.Mod2

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1, Test.Mod2
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

test-suite testss
  main-is: Main.hs
  type: exitcode-stdio-1.0
  other-modules:  Dir.Mod1
                  Dir.Mod2

library
  build-depends:
    base >=4.15 && <5
  exposed-modules: Test.Mod1, Test.Mod2
|]

caseMultipleOtherModulesWithImportFields :: TestTree
caseMultipleOtherModulesWithImportFields =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple other modules with import field"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CLibName LMainLibName
            , cnfAdditions = NE.fromList ["This.Dir.Mod1", "Mod3"]
            , cnfTargetField = OtherModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar
  other-modules: Other

library
  import: foo
  other-modules: This.Dir.Mod1, Mod3
  build-depends: foo < 1 && >0.7, quux < 1
  exposed-modules: Foo
|]
      }
  where
    inContents =
      [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar
  other-modules: Other

library
  import: foo
  build-depends: foo < 1 && >0.7, quux < 1
  exposed-modules: Foo
|]

caseMultipleOtherModulesWithImportFields2 :: TestTree
caseMultipleOtherModulesWithImportFields2 =
  mkAddTest $
    CabalAddTest
      { catName = "add multiple other modules with capitalised import field"
      , catConfig =
          AddConfig
            { cnfComponent = Right $ CLibName LMainLibName
            , cnfAdditions = NE.fromList ["This.Dir.Mod1", "Mod3"]
            , cnfTargetField = OtherModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            }
      , catOutput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar
  other-modules: Other

library
  Import: foo
  other-modules: This.Dir.Mod1, Mod3
  build-depends: foo < 1 && >0.7, quux < 1
  exposed-modules: Foo
|]
      }
  where
    inContents =
      [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar
  other-modules: Other

library
  Import: foo
  build-depends: foo < 1 && >0.7, quux < 1
  exposed-modules: Foo
|]

caseRenameDependency1 :: TestTree
caseRenameDependency1 =
  mkRenameTest $
    CabalRenameTest
      { crtName = "rename dependency"
      , crtConfig =
          RenameConfig
            { cnfComponent = Right $ CLibName LMainLibName
            , cnfTargetField = BuildDepends
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            , cnfRenameFrom = "base"
            , cnfRenameTo = "relude"
            }
      , crtOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    relude >=4.15 && <5,
    base-foo,
    foo-base

benchmark foo
  type: exitcode-stdio-1.0
  main-is: Main
  build-depends: base
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5,
    base-foo,
    foo-base

benchmark foo
  type: exitcode-stdio-1.0
  main-is: Main
  build-depends: base
|]

caseRenameExposedModule1 :: TestTree
caseRenameExposedModule1 =
  mkRenameTest $
    CabalRenameTest
      { crtName = "rename exposed module"
      , crtConfig =
          RenameConfig
            { cnfComponent = Right $ CLibName LMainLibName
            , cnfTargetField = ExposedModules
            , cnfFields = parseCabalFileOrError inContents
            , cnfOrigContents = inContents
            , cnfRenameFrom = "Data.Foo"
            , cnfRenameTo = "Data.Quux"
            }
      , crtOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  exposed-modules: Data.Quux, Data.Foo.Bar, Data.Foo.Baz

benchmark foo
  type: exitcode-stdio-1.0
  main-is: Main
  exposed-modules: Data.Foo
|]
      }
  where
    inContents =
      [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  exposed-modules: Data.Foo, Data.Foo.Bar, Data.Foo.Baz

benchmark foo
  type: exitcode-stdio-1.0
  main-is: Main
  exposed-modules: Data.Foo
|]

prettyDiff :: [Diff String] -> String
prettyDiff =
  unlines
    . mapMaybe
      ( \case
          First xs -> Just $ '-' : xs
          Second ys -> Just $ '+' : ys
          Both xs _ -> Just $ ' ' : xs
      )

mkAddTest :: CabalAddTest -> TestTree
mkAddTest cat = singleTest (catName cat) cat

mkRenameTest :: CabalRenameTest -> TestTree
mkRenameTest cat = singleTest (crtName cat) cat

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Unit Tests"
      [ caseMultipleBuildDependencies1
      , caseMultipleExposedModules1
      , caseMultipleExposedModulesUsingSpaces
      , caseMultipleOtherModules
      , caseMultipleOtherModulesUsingSpaces
      , caseMultipleOtherModulesUsingLeadingCommas
      , caseMultipleOtherModulesUsingLeadingSpaces
      , caseMultipleOtherModulesWithImportFields
      , caseMultipleOtherModulesWithImportFields2
      , caseRenameDependency1
      , caseRenameExposedModule1
      ]

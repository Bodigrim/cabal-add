{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright:   (c) 2023 Bodigrim
-- License:     BSD-3-Clause
module Main (main) where

import Data.Algorithm.Diff (Diff, PolyDiff (..), getDiff)
import Data.Char (isAlpha)
import Data.Maybe (mapMaybe)
import Data.String.QQ (s)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (cwd, env, proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Providers (IsTest (..), singleTest, testFailed, testPassed)

data CabalAddTest = CabalAddTest
  { catName :: String
  , catArgs :: [String]
  , catEnv :: [(String, String)]
  , catInput :: String
  , catOutput :: String
  }

instance IsTest CabalAddTest where
  testOptions = pure []
  run _opts CabalAddTest {..} _yieldProgress = do
    mCabalAddExe <- findExecutable "cabal-add"
    case mCabalAddExe of
      Nothing -> pure $ testFailed "cabal-add executable is not in PATH"
      Just cabalAddExe -> do
        let template = map (\c -> if isAlpha c then c else '_') catName
        withSystemTempDirectory template $ \tempDir -> do
          let cabalFileName = tempDir ++ "/" ++ template ++ ".cabal"
          writeFile cabalFileName catInput
          let crpr = (proc cabalAddExe catArgs) {cwd = Just tempDir, env = Just catEnv}
          (code, _out, err) <- readCreateProcessWithExitCode crpr ""
          case code of
            ExitFailure {} -> pure $ testFailed err
            ExitSuccess -> do
              output <- readFile cabalFileName
              pure $
                if output == catOutput
                  then testPassed ""
                  else testFailed $ prettyDiff $ getDiff (lines catOutput) (lines output)

prettyDiff :: [Diff String] -> String
prettyDiff =
  unlines
    . mapMaybe
      ( \case
          First xs -> Just $ '-' : xs
          Second ys -> Just $ '+' : ys
          Both xs _ -> Just $ ' ' : xs
      )

mkTest :: CabalAddTest -> TestTree
mkTest cat = singleTest (catName cat) cat

caseMultipleDependencies1 :: TestTree
caseMultipleDependencies1 =
  mkTest $
    CabalAddTest
      { catName = "add multiple dependencies 1"
      , catArgs = ["foo < 1 && >0.7", "baz ^>= 2.0"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
|]
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

caseMultipleDependencies2 :: TestTree
caseMultipleDependencies2 =
  mkTest $
    CabalAddTest
      { catName = "add multiple dependencies 2"
      , catArgs = ["foo < 1 && >0.7", "baz ^>= 2.0"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends: base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends: foo < 1 && >0.7, baz ^>= 2.0, base >=4.15 && <5
|]
      }

caseMultipleDependencies3 :: TestTree
caseMultipleDependencies3 =
  mkTest $
    CabalAddTest
      { catName = "add multiple dependencies 3"
      , catArgs = ["foo < 1 && >0.7", "baz ^>= 2.0"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:   base >=4.15 && <5,
                   containers
|]
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:   foo < 1 && >0.7,
                   baz ^>= 2.0,
                   base >=4.15 && <5,
                   containers
|]
      }

caseMultipleDependencies4 :: TestTree
caseMultipleDependencies4 =
  mkTest $
    CabalAddTest
      { catName = "add multiple dependencies 4"
      , catArgs = ["foo < 1 && >0.7", "baz ^>= 2.0"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:   base >=4.15 && <5
                 , containers
                 , deepseq
|]
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:   foo < 1 && >0.7
                 , baz ^>= 2.0
                 , base >=4.15 && <5
                 , containers
                 , deepseq
|]
      }

caseLibraryInDescription :: TestTree
caseLibraryInDescription =
  mkTest $
    CabalAddTest
      { catName = "word 'library' in description"
      , catArgs = ["foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple
description:
  A library of basic functionality.

library
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple
description:
  A library of basic functionality.

library
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseImportFields1 :: TestTree
caseImportFields1 =
  mkTest $
    CabalAddTest
      { catName = "import fields 1"
      , catArgs = ["foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar

library
  import: foo
  exposed-modules: Foo
|]
      , catOutput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar

library
  import: foo
  build-depends: foo < 1 && >0.7, quux < 1
  exposed-modules: Foo
|]
      }

caseImportFields2 :: TestTree
caseImportFields2 =
  mkTest $
    CabalAddTest
      { catName = "import fields 2"
      , catArgs = ["foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar

library
  Import : foo
  exposed-modules: Foo
|]
      , catOutput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar

library
  Import : foo
  build-depends: foo < 1 && >0.7, quux < 1
  exposed-modules: Foo
|]
      }

caseImportFields3 :: TestTree
caseImportFields3 =
  mkTest $
    CabalAddTest
      { catName = "import fields 3"
      , catArgs = ["foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar

library
  Import : foo
|]
      , catOutput =
          [s|
cabal-version: 2.2
name:          dummy
version:       0.13.0.0
build-type:    Simple

common foo
  build-depends: bar

library
  Import : foo
  build-depends: foo < 1 && >0.7, quux < 1
|]
      }

caseSublibraryTarget1 :: TestTree
caseSublibraryTarget1 =
  mkTest $
    CabalAddTest
      { catName = "sublibrary target 1"
      , catArgs = ["foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

library baz
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

library baz
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseSublibraryTarget2 :: TestTree
caseSublibraryTarget2 =
  mkTest $
    CabalAddTest
      { catName = "sublibrary target 2"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5

library baz
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5

library baz
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseExecutableTarget1 :: TestTree
caseExecutableTarget1 =
  mkTest $
    CabalAddTest
      { catName = "executable target 1"
      , catArgs = ["exe", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseExecutableTarget2 :: TestTree
caseExecutableTarget2 =
  mkTest $
    CabalAddTest
      { catName = "executable target 2"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseExecutableTarget3 :: TestTree
caseExecutableTarget3 =
  mkTest $
    CabalAddTest
      { catName = "executable target 3"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable "baz"
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable "baz"
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseExecutableTarget4 :: TestTree
caseExecutableTarget4 =
  mkTest $
    CabalAddTest
      { catName = "executable target 4"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common baz
  language: Haskell2010

executable baz
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common baz
  language: Haskell2010

executable baz
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseTestTarget1 :: TestTree
caseTestTarget1 =
  mkTest $
    CabalAddTest
      { catName = "test target 1"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common baz
  language: Haskell2010

test-suite baz
  type: exitcode-stdio-1.0
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common baz
  language: Haskell2010

test-suite baz
  type: exitcode-stdio-1.0
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseTestTarget2 :: TestTree
caseTestTarget2 =
  mkTest $
    CabalAddTest
      { catName = "test target 2"
      , catArgs = ["test:baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common baz
  language: Haskell2010

test-suite baz
  type: exitcode-stdio-1.0
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common baz
  language: Haskell2010

test-suite baz
  type: exitcode-stdio-1.0
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseCommonStanzaTarget1 :: TestTree
caseCommonStanzaTarget1 =
  mkTest $
    CabalAddTest
      { catName = "common stanza as a target 1"
      , catArgs = ["foo", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common foo
  language: Haskell2010
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

common foo
  build-depends: foo < 1 && >0.7, quux < 1
  language: Haskell2010
|]
      }

caseCommonStanzaTarget2 :: TestTree
caseCommonStanzaTarget2 =
  mkTest $
    CabalAddTest
      { catName = "common stanza as a target 2"
      , catArgs = ["foo", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

Common    foo
  language: Haskell2010
  build-depends:
    , base
    , containers
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

Common    foo
  language: Haskell2010
  build-depends:
    , foo < 1 && >0.7
    , quux < 1
    , base
    , containers
|]
      }

caseTwoSpacesInStanza :: TestTree
caseTwoSpacesInStanza =
  mkTest $
    CabalAddTest
      { catName = "two spaces in stanza"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable  baz
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable  baz
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseTitleCaseStanza1 :: TestTree
caseTitleCaseStanza1 =
  mkTest $
    CabalAddTest
      { catName = "title case in stanza 1"
      , catArgs = ["foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

Library
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

Library
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseTitleCaseStanza2 :: TestTree
caseTitleCaseStanza2 =
  mkTest $
    CabalAddTest
      { catName = "title case in stanza 2"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

Executable baz
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

Executable baz
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseTitleCaseBuildDepends :: TestTree
caseTitleCaseBuildDepends =
  mkTest $
    CabalAddTest
      { catName = "title case in build-depends"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  main-is: Main.hs
  Build-Depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  main-is: Main.hs
  Build-Depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

caseSharedComponentPrefixes :: TestTree
caseSharedComponentPrefixes =
  mkTest $
    CabalAddTest
      { catName = "shared component prefixes"
      , catArgs = ["baz", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable bazzzy
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5

executable baz
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable bazzzy
  main-is: Main.hs
  build-depends:
    base >=4.15 && <5

executable baz
  main-is: Main.hs
  build-depends:
    foo < 1 && >0.7,
    quux < 1,
    base >=4.15 && <5
|]
      }

windowsLineEndings :: TestTree
windowsLineEndings =
  mkTest $
    CabalAddTest
      { catName = "Windows line endings"
      , catArgs = ["exe", "foo < 1 && >0.7", "quux < 1"]
      , catEnv = mempty
      , catInput =
          convertToWindowsLineEndings
            [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  main-is: Main.hs
|]
      , catOutput =
          convertToWindowsLineEndings
            [s|
name:          dummy
version:       0.1.0.0
cabal-version: 2.0
build-type:    Simple

executable baz
  build-depends: foo < 1 && >0.7, quux < 1
  main-is: Main.hs
|]
      }
  where
    convertToWindowsLineEndings :: String -> String
    convertToWindowsLineEndings = concatMap (\c -> if c == '\n' then ['\r', '\n'] else [c])

caseLeadingComma1 :: TestTree
caseLeadingComma1 =
  mkTest $
    CabalAddTest
      { catName = "build-depends start from comma 1"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends:
    , base >=4.15 && <5
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends:
    , baz ^>= 2.0
    , quux < 1
    , base >=4.15 && <5
|]
      }

caseLeadingComma2 :: TestTree
caseLeadingComma2 =
  mkTest $
    CabalAddTest
      { catName = "build-depends start from comma 2"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: ,base >=4.15 && <5
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: ,baz ^>= 2.0 ,quux < 1 ,base >=4.15 && <5
|]
      }

caseLeadingComma3 :: TestTree
caseLeadingComma3 =
  mkTest $
    CabalAddTest
      { catName = "build-depends start from comma 3"
      , catArgs = ["baz ^>= 2.0", "quux > 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: ,base >=4.15 && <5
                 ,containers
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: ,baz ^>= 2.0
                 ,quux > 1
                 ,base >=4.15 && <5
                 ,containers
|]
      }

caseConditionalBuildDepends :: TestTree
caseConditionalBuildDepends =
  mkTest $
    CabalAddTest
      { catName = "build-depends are under condition"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  if impl(ghc >= 9.6)
    build-depends:
      base >=4.15 && <5
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: baz ^>= 2.0, quux < 1
  if impl(ghc >= 9.6)
    build-depends:
      base >=4.15 && <5
|]
      }

caseEmptyComponent1 :: TestTree
caseEmptyComponent1 =
  mkTest $
    CabalAddTest
      { catName = "empty component 1"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library

executable bar
  main-is: Bar.hs
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library

  build-depends: baz ^>= 2.0, quux < 1

executable bar
  main-is: Bar.hs
|]
      }

caseEmptyComponent2 :: TestTree
caseEmptyComponent2 =
  mkTest $
    CabalAddTest
      { catName = "empty component 2"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: baz ^>= 2.0, quux < 1
|]
      }

caseEmptyComponent3 :: TestTree
caseEmptyComponent3 =
  mkTest $
    CabalAddTest
      { catName = "empty component 3"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: baz ^>= 2.0, quux < 1
|]
      }

caseEmptyBuildDepends :: TestTree
caseEmptyBuildDepends =
  mkTest $
    CabalAddTest
      { catName = "empty build-depends"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends:
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

library
  build-depends: baz ^>= 2.0, quux < 1
  build-depends:
|]
      }

caseComponentInBraces :: TestTree
caseComponentInBraces =
  mkTest $
    CabalAddTest
      { catName = "component in figure braces"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

Library {
  Build-Depends: base >= 4 && < 5
}
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

Library {
  Build-Depends: baz ^>= 2.0, quux < 1, base >= 4 && < 5
}
|]
      }

caseCommentsWithCommas :: TestTree
caseCommentsWithCommas =
  mkTest $
    CabalAddTest
      { catName = "comments with commas"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  main-is:       Main.hs
  build-depends:    magda,
                    -- Something, which
                    -- contains commas.
                    base
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  main-is:       Main.hs
  build-depends:    baz ^>= 2.0, quux < 1, magda,
                    -- Something, which
                    -- contains commas.
                    base
|]
      }

caseCommentsWithoutCommas :: TestTree
caseCommentsWithoutCommas =
  mkTest $
    CabalAddTest
      { catName = "comments without commas"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  main-is:       Main.hs
  build-depends:  magda,
                  -- Something without commas
                  base
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  main-is:       Main.hs
  build-depends:  baz ^>= 2.0, quux < 1, magda,
                  -- Something without commas
                  base
|]
      }

caseDependenciesOnTheSameLine :: TestTree
caseDependenciesOnTheSameLine =
  mkTest $
    CabalAddTest
      { catName = "all build-deps on the same line"
      , catArgs = ["baz ^>= 2.0", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   magda, base
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   baz ^>= 2.0, quux < 1, magda, base
|]
      }

caseIgnoreAddArgument :: TestTree
caseIgnoreAddArgument =
  mkTest $
    CabalAddTest
      { catName = "ignore add as the first command-line argument"
      , catArgs = ["add", "baz ^>= 2.0", "quux < 1"]
      , catEnv = [("CABAL", "/fake/usr/bin/local/cabal")]
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   magda, base
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   baz ^>= 2.0, quux < 1, magda, base
|]
      }

caseDoNotIgnoreAddArgument :: TestTree
caseDoNotIgnoreAddArgument =
  mkTest $
    CabalAddTest
      { catName = "do not ignore add as the second command-line argument"
      , catArgs = ["baz ^>= 2.0", "add", "quux < 1"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   magda, base
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   baz ^>= 2.0, add, quux < 1, magda, base
|]
      }

caseSublibraryDependency :: TestTree
caseSublibraryDependency =
  mkTest $
    CabalAddTest
      { catName = "sublibrary as a dependency"
      , catArgs = ["foo:bar"]
      , catEnv = mempty
      , catInput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   magda, base
|]
      , catOutput =
          [s|
cabal-version: 3.6
name:          dummy
version:       0.1
build-type:    Simple

executable dagda
  build-depends:   foo:bar, magda, base
|]
      }

caseSpecialDependencyName1 :: TestTree
caseSpecialDependencyName1 =
  mkTest $
    CabalAddTest
      { catName = "add dependency named 'add'"
      , catArgs = ["add"]
      , catEnv = mempty
      , catInput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    base >=4.15 && <5
|]
      , catOutput =
          [s|
name:          dummy
version:       0.13.0.0
cabal-version: 2.0
build-type:    Simple

library
  build-depends:
    add,
    base >=4.15 && <5
|]
      }

main :: IO ()
main =
  defaultMain $
    testGroup
      "All"
      [ caseMultipleDependencies1
      , caseMultipleDependencies2
      , caseMultipleDependencies3
      , caseMultipleDependencies4
      , caseLibraryInDescription
      , caseImportFields1
      , caseImportFields2
      , caseImportFields3
      , caseSublibraryTarget1
      , caseSublibraryTarget2
      , caseExecutableTarget1
      , caseExecutableTarget2
      , caseExecutableTarget3
      , caseExecutableTarget4
      , caseTestTarget1
      , caseTestTarget2
      , caseCommonStanzaTarget1
      , caseCommonStanzaTarget2
      , caseTwoSpacesInStanza
      , caseTitleCaseStanza1
      , caseTitleCaseStanza2
      , caseTitleCaseBuildDepends
      , caseSharedComponentPrefixes
      , windowsLineEndings
      , caseLeadingComma1
      , caseLeadingComma2
      , caseLeadingComma3
      , caseConditionalBuildDepends
      , caseEmptyComponent1
      , caseEmptyComponent2
      , caseEmptyComponent3
      , caseEmptyBuildDepends
      , caseComponentInBraces
      , caseCommentsWithCommas
      , caseCommentsWithoutCommas
      , caseDependenciesOnTheSameLine
      , caseIgnoreAddArgument
      , caseDoNotIgnoreAddArgument
      , caseSublibraryDependency
      , caseSpecialDependencyName1
      ]

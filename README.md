# cabal-add [![Hackage](http://img.shields.io/hackage/v/cabal-add.svg)](https://hackage.haskell.org/package/cabal-add) [![Stackage LTS](http://stackage.org/package/cabal-add/badge/lts)](http://stackage.org/lts/package/cabal-add) [![Stackage Nightly](http://stackage.org/package/cabal-add/badge/nightly)](http://stackage.org/nightly/package/cabal-add)

Extend Cabal `build-depends` from the command line.

`cabal-add` does not have limitations of
[`cabal-edit`](https://hackage.haskell.org/package/cabal-edit):
it works on any sectioned Cabal file,
supports stanzas and conditional blocks,
and preserves original formatting.

Install the executable with

```
cabal install cabal-add
```

To add a dependency on `foo`, switch to a folder with your project and execute

```
cabal-add foo
```

If you are using Cabal 3.12+ which supports
[external commands](https://cabal.readthedocs.io/en/3.12/external-commands.html),
you can omit the dash:

```
cabal add foo
```

Command-line arguments:

* `--project-file FILE`

  Set the path of the cabal.project file. Detect `cabal.project` or `*.cabal`
  in the current folder, if omitted.

* `ARGS`

  Optional [target](https://cabal.readthedocs.io/en/latest/cabal-commands.html#target-forms)
  (wildcards such as `exe`, `test` or `bench` are supported) to update, followed
  by a non-empty list of package(s) to add to
  `build-depends` section. Version bounds can be
  provided as well, use quotes to escape comparisons
  from your shell. E. g., `'foo < 0.2'`.

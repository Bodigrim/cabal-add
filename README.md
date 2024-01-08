# cabal-add

Extend Cabal `build-depends` from the command line.

`cabal-add` does not have limitations of
[`cabal-edit`](https://hackage.haskell.org/package/cabal-edit):
it works on any sectioned Cabal file,
supports stanzas and conditional blocks,
and preserves original formatting.

Install with

```
git clone https://github.com/Bodigrim/cabal-add.git
cd cabal-add
cabal install exe:cabal-add --allow-newer='cabal-install-parsers:*'
```

To add a dependency on `foo`, switch to a folder with your project and execute

```
cabal-add foo
```

If you are using Cabal 3.11+ which supports
[external commands](https://cabal.readthedocs.io/en/latest/external-commands.html),
you can omit the dash:

```
cabal add foo
```

Command-line arguments:

* `--project-file FILE`

  Set the path of the cabal.project file. Detect `cabal.project` or `*.cabal`
  in the current folder, if omitted.

* `ARGS`

  Optional package component (wildcards such as `exe`,
  `test` or `bench` are supported) to update, followed
  by a non-empty list of package(s) to add to
  `build-depends` section. Version bounds can be
  provided as well, use quotes to escape comparisons
  from your shell. E. g., `'foo < 0.2'`.

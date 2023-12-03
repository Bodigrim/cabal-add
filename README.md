# cabal-add

```
$ cabal-add --help
Usage: cabal-add [-f|--cabal-file FILE] [-c|--component ARG] DEP

  Extend build-depends from the command line

Available options:
  -f,--cabal-file FILE     Cabal file to edit in place (tries to detect Cabal
                           file in current folder if omitted).
  -c,--component ARG       Package component to update (the main library, if
                           omitted). Wildcards such as 'exe', 'test' or 'bench'
                           are supported.
  DEP                      Package(s) to add to build-depends section. Version
                           bounds can be provided as well, use quotes to escape
                           comparisons from your shell. E. g., 'foo < 0.2'.
```

`cabal-add` does not have limitations of
[`cabal-edit`](https://hackage.haskell.org/package/cabal-edit):
it works on any sectioned Cabal file,
supports stanzas and conditional blocks,
and preserves original formatting.

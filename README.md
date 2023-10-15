# cabal-add

```
$ cabal-add --help
Usage: cabal-add (-f|--cabal-file FILE) [-c|--component ARG] DEP

  Extend build-depends from the command line

Available options:
  -f,--cabal-file FILE     Cabal file to edit in place.
  -c,--component ARG       Package component to update (the main library, if
                           omitted). Wildcards such as 'exe', 'test' or 'bench'
                           are supported.
  DEP                      Package(s) to add to build-depends section. Version
                           bounds can be provided as well, use quotes to escape
                           comparisons from your shell. E. g., 'foo < 0.2'.
```

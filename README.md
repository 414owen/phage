# The Phage Programming Language

## Usage

```bash
$ stack build        # to build
$ stack exec phage   # gives a repl
```

## Implemented

* Addition

## TODO

* Everything else

## Example Usage

```bash
$ stack exec phage <<< "(+ 1 2)"
Ast [AList [AAtom "+",ANum 1,ANum 2]]
PNum 3
```

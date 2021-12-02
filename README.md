# Advent Of Code 2021 [![Haskell CI](https://github.com/bthuilot/adventofcode2021/actions/workflows/haskell.yml/badge.svg)](https://github.com/bthuilot/adventofcode2021/actions/workflows/haskell.yml)
### *In Haskell*


## Running
To run first build the project, then use `stack exec` to run it

```shell
$ stack build
$ stack exec aoc2020
```

or you can use the `stack runghc` command directly on the `app/Main.hs` file


```shell
$ stack runghc app/Main.hs
```


## Tests

Tests for this repository are just the examples given in each prompt for the days. 

The suite can be run by executing

```shell
$ stack test
```

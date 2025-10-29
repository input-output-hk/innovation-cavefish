# Cavefish server
Cavefish tests is a sub-project of the [cavefish](../../cabal.project).

## Installing / Getting started
> Using `nix` is most convinent way to install all dependencies.  Non nix user, may refer to the [github action script][def] to build their developement environment

From cavefish-server folder:
```shell
nix develope
```

## Developing

```shell
cabal clean && cabal update 
cabal build cavefish-tests
```

To execute the tests:
```shell
cabal test cavefish-tests
```
For continues build, you may use [watchexec](https://github.com/watchexec/watchexec)

```shell
watchexec --clear -w ./cavefish 'cabal build all && cabal test cavefish-tests'
```

## Links
TBD

[def]: ../../../.github/workflows/cavefish-server-linux-ci.yml#L1-L5
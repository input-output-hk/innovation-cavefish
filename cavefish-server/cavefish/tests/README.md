# Cavefish server
Cavefish tests is the test suite for cavefish server application. It contains integration tests and end-to-end tests to ensure the correctness and reliability of the cavefish server.

## Installing / Getting started
> Using `nix` is the most convenient way to install all dependencies.  Non nix user, may refer to the [github action script][def] to build their development environment.

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

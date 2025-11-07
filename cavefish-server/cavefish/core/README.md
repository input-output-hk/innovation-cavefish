# Cavefish core
Cavefish-core is the core library for cavefish server and client applications. It contains common data types, utilities, and business logic shared across different components of the Cavefish ecosystem.

## Installing / Getting started
> Using `nix` is the most convenient way to install all dependencies.  Non nix user, may refer to the [github action script][def] to build their development environment.

From cavefish-server folder:
```shell
nix develope
```

## Developing

```shell
cabal clean && cabal update 
cabal build cavefish-core
```

For continues build, you may use [watchexec](https://github.com/watchexec/watchexec)

```shell
watchexec --clear -w ./cavefish/core 'cabal build cavefish-server && cabal test cavefish-tests'
```

## Links
TBD

[def]: ../../../.github/workflows/cavefish-server-linux-ci.yml#L1-L5

# Cavefish server
Cavefish server, http server, is a sub-project of the [cavefish](../../cabal.project).

## Installing / Getting started
> Using `nix` is the most convenient way to install all dependencies.  Non nix user, may refer to the [github action script][def] to build their development environment.

From the cavefish-server folder:
```shell
nix develope
```

## Developing

```shell
cabal clean && cabal update 
cabal build cavefish-server
```

To execute the server:
```shell
cabal run cavefish-server:exe:cavefish-server
```
Or you may invoke the executable:
```shel
$(cabal exec which cavefish-server)
```
For continues build, you may use [watchexec](https://github.com/watchexec/watchexec)

```shell
watchexec --clear -w ./cavefish/server 'cabal build cavefish-server && cabal test cavefish-tests'
```

## Links
TBD

[def]: ../../../.github/workflows/cavefish-server-linux-ci.yml#L1-L5

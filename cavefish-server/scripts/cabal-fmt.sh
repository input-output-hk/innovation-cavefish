#!/usr/bin/env sh
##
## Script to format all .cabal files in the current directory and its subdirectories
## using cabal-fmt.
##

find ../ -name "*.cabal" -exec cabal-fmt -i {} \;

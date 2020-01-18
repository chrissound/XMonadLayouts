#!/usr/bin/env bash

if which nix; then
    nix-shell --run "ghcid --command='cabal v2-repl my-xmonad'"
    exit 0
elif which stack; then
    ghcid '--command=stack ghci my-xmonad:exe:xmonad'
    exit 0
fi

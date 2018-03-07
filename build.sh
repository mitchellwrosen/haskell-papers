#!/bin/sh

set -e

mkdir -p .shake

if [ ! -f ".shake/shake" ]; then
  stack --resolver lts-10.7 ghc --package shake shake.hs -- -o .shake/shake -odir .shake -hidir .shake
fi

exec ".shake/shake" "$@"

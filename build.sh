#!/bin/sh

set -e

if [ ! -f ".shake/shake" ]; then
  mkdir -p .shake
  stack --resolver lts-10.7 ghc --package shake shake.hs -- -o .shake/shake -odir .shake -hidir .shake
fi

exec ".shake/shake" "$@"

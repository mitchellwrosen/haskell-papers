#!/bin/sh

set -ex

stack exec yaml2json papers.yaml > papers.json
elm make Main.elm --output main.js
./UglifyJS2/bin/uglifyjs main.js --compress --mangle toplevel=true --output main.min.js

#!/bin/sh

set -ex

stack exec yaml2json papers.yaml > papers.json
elm make Main.elm --output main.js

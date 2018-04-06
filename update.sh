#!/bin/bash

set -ex

./build.sh
git add static paper*.yaml
git commit -am "Update papers.yaml"
git push

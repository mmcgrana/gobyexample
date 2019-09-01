#!/bin/bash

set -eo pipefail

paths=$(ls examples/*/*.go)

for path in $paths; do
  gofmt -w=true $path
done

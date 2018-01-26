#!/bin/bash
set -ex

mkdir -p src/$PKG && cd src/$PKG
run -s "Cloning"  git clone $URL --branch $REF --single-branch .
git reset --hard $SHA

go get github.com/russross/blackfriday

PKGS=$(go list $PKG/... | grep -v examples)
run -s "Linting"  golint -set_exit_status $PKGS
run -s "Vetting"  go vet -x $PKGS
run -s "Building" tools/build

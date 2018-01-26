#!/bin/bash
set -ex

mkdir -p src/$PKG && cd src/$PKG
run -s "Cloning"  git clone $URL --branch $REF --single-branch .
git reset --hard $SHA

PKGS=$(go list $PKG/...)
run -s "Linting"  golint -set_exit_status $PKGS
run -s "Vetting"  go vet -x $PKGS
run -s "Building" go build -v $PKGS
run -s "Testing"  go test -v $PKGS

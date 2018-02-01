#!/bin/bash
set -ex

mkdir -p src/$PKG && cd src/$PKG
run -s "Cloning"  git clone $URL --branch $REF --single-branch .
git reset --hard $SHA

go get github.com/russross/blackfriday
go get github.com/golang/lint/golint

PKGS=$(go list $PKG/... | grep -v examples/variables)
run -s "Linting"  golint -set_exit_status $PKGS

PKGS=$(go list $PKG/... | grep -v examples/panic)
run -s "Vetting"  go vet -x $PKGS

run -s "Building" tools/build

run -s "Verifying" git diff --exit-code

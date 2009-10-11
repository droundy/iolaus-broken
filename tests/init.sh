#!/usr/bin/env bash
set -ev

mkdir temp1
cd temp1
iolaus initialize
test -d .git
iolaus init && exit 1
cd ..

# Some tests for the repodir flag
mkdir temp2
iolaus init --repodir temp2
test -d temp2/.git

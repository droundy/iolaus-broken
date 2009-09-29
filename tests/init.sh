#!/usr/bin/env bash
set -ev

# test-fails because I haven't yet created init command

mkdir temp1
cd temp1
grit init
test -d .git
grit init && exit 1
cd ..

# Some tests for the repodir flag
mkdir temp2
grit init --repodir temp2
test -d temp2/.git

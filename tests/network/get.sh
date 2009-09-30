#!/usr/bin/env bash
set -ev

iolaus get git://gitorious.org/ditz/mainline.git test
test -d test/.git
#test -f iolaus/iolaus.hs

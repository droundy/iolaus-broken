#!/usr/bin/env bash
set -ev

echo this test-fails because get is not yet implemented

iolaus get http://iolaus.abridgegame.org iolaus
test -d iolaus/.git
test -f iolaus/iolaus.hs

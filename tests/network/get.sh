#!/usr/bin/env bash
set -ev

echo this test-fails because get is not yet implemented

grit get http://grit.abridgegame.org grit
test -d grit/.git
test -f grit/grit.hs

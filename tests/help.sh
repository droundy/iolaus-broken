#!/usr/bin/env bash
set -ev

for i in `grit --commands | grep -v -- --`; do
    echo XXXXXXXXXXXXXXXXXXXXXXX
    echo Checkingn help on $i
    echo XXXXXXXXXXXXXXXXXXXXXXX
    grit $i --help
done

#!/usr/bin/env bash
set -ev

for i in `iolaus --commands | grep -v -- --`; do
    echo XXXXXXXXXXXXXXXXXXXXXXX
    echo Checkingn help on $i
    echo XXXXXXXXXXXXXXXXXXXXXXX
    iolaus $i --help
done

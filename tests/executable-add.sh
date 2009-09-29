#!/usr/bin/env bash
set -ev

mkdir temp1
cd temp1
git init
test -d .git

cat > foo <<EOF
#!/bin/sh
echo good
EOF
chmod +x foo
grit record -a -m 'add foo'
cd ..

grit get temp1 temp2
cd temp2
./foo

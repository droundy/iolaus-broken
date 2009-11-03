set -ev

mkdir temp
cd temp

mkdir .git-hooks
cat > .git-hooks/test <<EOF
#!/bin/sh
set -ev

echo hello world

echo goodbye world

echo this is another nice test
EOF
chmod +x .git-hooks/test

iolaus init
iolaus record -am 'create test' > out
cat out

grep hello out
grep goodbye out
grep 'another nice test' out

cat > .git-hooks/test <<EOF
#!/bin/sh
set -ev

echo hello world

false

echo goodbye world



true



echo this is another nice test

false
EOF

iolaus record -am 'modify test' && exit 1

iolaus whatsnew | grep false
iolaus whatsnew | grep true

echo `git config --get-color foobar reset`

echo the above should fail, since we added bugs to test

iolaus record --delta-debug -am 'safe parts of test'

iolaus wh

iolaus whatsnew | grep true && exit 1
iolaus whatsnew | grep false

echo `git config --get-color foobar reset`

iolaus revert -a
grep false .git-hooks/test && exit 1
grep true .git-hooks/test


# Now we'll check if --delta-debug handles "interference" situations,
# or logical dependencies.

cat > .git-hooks/test <<EOF
#!/bin/sh
set -ev

echo hello world

FOO=true

echo goodbye world


FOO=false


echo this is another nice test

\$FOO false
EOF

# XXXXXXXXXXXXXXXXXXXX
iolaus wh
# YYYYYYYYYYYYYYYYYYYY

iolaus record --delta-debug -am 'checking for interference'
iolaus wh

iolaus wh | grep 'FOO=true' && exit 1
if iolaus wh | grep 'FOO false'; then
    echo there should not be an equals
    iolaus wh | grep 'FOO=false' && exit 1
else
    echo there should be an equals
    iolaus wh | grep 'FOO=false'
fi

true

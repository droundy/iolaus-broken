set -ev

mkdir temp
cd temp

mkdir .git-hooks
cat > .git-hooks/test <<EOF
#!/bin/sh
set -ev

. ./.git-hooks/build

\$ECHO hello world

\$ECHO goodbye world

\$ECHO this is another nice test
EOF
chmod +x .git-hooks/test
cat > .git-hooks/build <<EOF
#!/bin/sh
set -ev

FOO=false
# FOO is currently buggy

# define ECHO environment variable
ECHO=echo

# be sure ECHO works
\$ECHO goodbye world
EOF
chmod +x .git-hooks/build

iolaus init
iolaus record -am 'create build/test' > out 2> err
cat out

grep hello out
grep goodbye out
grep 'another nice test' out
grep ECHO err

cat > .git-hooks/test <<EOF
#!/bin/sh
set -ev

. .git-hooks/build

\$ECHO hello world

\$ECHO goodbye world

true

\$ECHO this is another nice test

false
EOF

cat > .git-hooks/build <<EOF
#!/bin/sh
set -ev

FOO=echo
# FOO is no longer buggy

# We'll now define an intermediate variable, so that three changes are
# needed for the build to succeed.
BAZBAR=\$FOO

# define ECHO environment variable
ECHO=\$BAZBAR

# be sure ECHO works
\$ECHO goodbye world
EOF

iolaus record -am 'modify test' && exit 1

iolaus whatsnew | grep false
iolaus whatsnew | grep true
iolaus whatsnew | grep '\$BAZBAR'

echo `git config --get-color foobar reset`

echo the above should fail, since we added bugs to test

iolaus record --delta-debug -am 'safe parts of test'

iolaus wh

iolaus whatsnew | grep true && exit 1
iolaus whatsnew | grep false
# we should have recorded the BAZBAR bit...
iolaus whatsnew | grep '\$BAZBAR' && exit 1

iolaus record -am 'buggy stuff' && exit 1

iolaus record -am 'buggy stuff' --build

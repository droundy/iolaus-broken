set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus wh
iolaus wh --config-default --disable
cat .git/config
iolaus wh --debug && exit 1
rm .git/config
iolaus wh
iolaus wh --config-default --global --disable
iolaus wh && exit 1

true

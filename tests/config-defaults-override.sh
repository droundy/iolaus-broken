set -ev

iolaus record --config-default --global --no-test

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
echo true > .test
chmod +x .test

iolaus record --config-default --test

cat .git/config

iolaus wh | grep chmod

iolaus record -am addtest

iolaus changes > ch
cat ch
grep Tested- ch && exit 1
iolaus changes --show-tested | grep Tested-

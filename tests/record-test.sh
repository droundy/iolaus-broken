set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
echo true > .test
chmod +x .test

iolaus wh | grep chmod

iolaus record -am addtest --debug

iolaus changes > ch
cat ch
grep Tested- ch && exit 1
iolaus changes --show-tested | grep Tested-

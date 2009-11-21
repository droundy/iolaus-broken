set -ev

mkdir temp
cd temp
iolaus init
echo true > .test
chmod +x .test

iolaus record --config-default --no-test

cat .git/config

iolaus wh | grep chmod

iolaus record -am addtest

iolaus changes > ch
cat ch
grep Tested- ch && exit 1

cd ..

rm -rf temp

iolaus record --global --config-default --no-test
cat ~/.gitconfig

mkdir temp
cd temp
iolaus init
echo true > .test
chmod +x .test

iolaus wh | grep chmod

iolaus record -am addtest

iolaus changes > ch
cat ch
grep Tested- ch && exit 1

true

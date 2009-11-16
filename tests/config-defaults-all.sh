set -ev

mkdir temp
cd temp
iolaus init
mkdir .git-hooks
echo true > .git-hooks/test
chmod +x .git-hooks/test

iolaus all --config-default --no-test

cat .git/config

iolaus wh | grep chmod

iolaus record -am addtest

iolaus changes > ch
cat ch
grep Tested- ch && exit 1

cd ..

rm -rf temp

iolaus all --global --config-default --no-test
cat ~/.gitconfig

mkdir temp
cd temp
iolaus init
mkdir .git-hooks
echo true > .git-hooks/test
chmod +x .git-hooks/test

iolaus wh | grep chmod

iolaus record -am addtest

iolaus changes > ch
cat ch
grep Tested- ch && exit 1

true

set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
mkdir .git-hooks
echo true > .git-hooks/test
chmod +x .git-hooks/test

iolaus record --config-default --no-test

cat .git/config

iolaus wh | grep chmod

iolaus record -am addtest --debug

iolaus changes > ch
cat ch
grep Tested- ch && exit 1

true

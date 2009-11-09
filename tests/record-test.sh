set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
mkdir .git-hooks
echo true > .git-hooks/test
chmod +x .git-hooks/test

iolaus wh | grep chmod

iolaus record -am addtest --debug

iolaus changes > ch
cat ch
grep Tested- ch && exit 1
iolaus changes --show-tested | grep Tested-

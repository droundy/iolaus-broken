set -ev

mkdir temp
cd temp
iolaus init

echo '*~' > .gitignore
mkdir .git-hooks
echo true > .git-hooks/test
chmod +x .git-hooks/test
echo true > .git-hooks/build
chmod +x .git-hooks/build

iolaus wh | grep chmod

iolaus record -am addbuildandtest

iolaus changes > ch
cat ch
grep Tested- ch && exit 1
iolaus changes --show-tested | grep Tested-

echo false > .git-hooks/test
iolaus record -am failedtest && exit 1

rm .git-hooks/test
iolaus record -am onlybuild

iolaus changes
iolaus changes | grep Built- && exit 1
iolaus changes --show-tested | grep Built-

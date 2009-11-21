set -ev

mkdir temp
cd temp
iolaus init

echo '*~' > .gitignore
echo true > .test
chmod +x .test
echo true > .build
chmod +x .build

iolaus wh | grep chmod

iolaus record -am addbuildandtest

iolaus changes > ch
cat ch
grep Tested- ch && exit 1
iolaus changes --show-tested | grep Tested-

echo false > .test
iolaus record -am failedtest && exit 1

rm .test
iolaus record -am onlybuild

iolaus changes
iolaus changes | grep Built- && exit 1
iolaus changes --show-tested | grep Built-

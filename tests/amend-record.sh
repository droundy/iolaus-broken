set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
mkdir .git-hooks
echo true > .git-hooks/test
chmod +x .git-hooks/test

iolaus wh | grep chmod

iolaus record -am addtest

echo echo hi there > .git-hooks/test

iolaus record -am 'make test more welcoming'

echo echo hello there > .git-hooks/test

echo y | iolaus amend-record -a

iolaus changes > ch
cat ch
grep 'make test more' ch
grep Tested- ch

iolaus changes -v | grep 'hi there' && exit 1
iolaus changes -v | grep 'hello there'


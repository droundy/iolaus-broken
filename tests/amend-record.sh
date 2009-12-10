set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
echo true > .test
chmod +x .test

iolaus wh | grep chmod

iolaus record -am addtest

echo echo hi there > .test

iolaus record -am 'make test more welcoming'

echo echo hello there > .test

iolaus changes --graph --show-hash --show-merge

iolaus amend-record --dry-run
iolaus amend-record --dry-run | grep welcoming

echo y | iolaus amend-record -a

iolaus changes > ch
cat ch
grep 'make test more' ch
grep Tested- ch && exit 1
iolaus changes --show-tested | grep Tested-

iolaus changes --graph
iolaus changes -v | grep 'hi there' && exit 1
iolaus changes -v | grep 'hello there'


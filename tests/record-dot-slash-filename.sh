set -ev

mkdir temp
cd temp
iolaus init
echo hello world > foo
echo goodbye > bar

iolaus record -am addfile ./foo

iolaus changes -s > ch
cat ch
grep bar ch && exit 1
grep foo ch

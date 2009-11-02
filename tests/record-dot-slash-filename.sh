set -ev

mkdir temp
cd temp
iolaus init
echo hello world > foo

# test-fails

iolaus record -am addfile ./foo

iolaus changes -s > ch
cat ch
grep foo ch

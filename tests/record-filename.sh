set -ev

mkdir temp
cd temp
iolaus init
echo hello world > foo

# test-fails for now.

iolaus record -am addfoo foo

iolaus changes > ch
cat ch
grep foo ch

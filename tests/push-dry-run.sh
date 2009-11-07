set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record -am addfile
cd ..

mkdir temp1
cd temp1
iolaus init

date > bar
iolaus record -am addbar

iolaus push --dry-run ../temp > out
cat out

grep -i 'would push' out
grep addfile out && exit 1
grep addbar out

iolaus push --dry-run ../temp > out

grep addbar out

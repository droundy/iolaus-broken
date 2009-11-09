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

iolaus pull --dry-run ../temp > out

grep -i 'would pull' out
grep addbar out && exit 1
grep addfile out
grep foo out && exit 1

iolaus pull --dry-run --summary ../temp > out
cat out

grep addbar out && exit 1
grep addfile out
grep foo out

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

iolaus push --all ../temp

echo another patch > bar
iolaus record -am another

iolaus push --dry-run -s ../temp > out
cat out

grep -i 'would push' out
grep addfile out && exit 1
grep addbar out && exit 1
grep another out


iolaus push -a ../temp
iolaus push --dry-run ../temp
iolaus push --dry-run ../temp | grep 'Would push the following' && exit 1
iolaus push --dry-run ../temp | grep 'No commits'

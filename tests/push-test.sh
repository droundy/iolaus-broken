set -ev

mkdir temp
cd temp
iolaus init
true > .test
chmod +x .test
iolaus record -am addtest
cd ..

iolaus get temp temp1
iolaus get temp temp2
cd temp1

iolaus all --config-default --record-for ../temp

date > foo
iolaus record -am addfoo

iolaus changes --show-parents --graph
# addfoo should have addtest as a parent
iolaus changes --show-parents | grep Parent

date > bar
iolaus record -am addbar

iolaus push -a ../temp
cd ../temp

iolaus changes --show-merge | grep Merge
iolaus changes --show-merge --show-tested --max-count 1 | grep Tested

cd ../temp2
iolaus pull -a ../temp1

# pull should have gotten the merge commit as well
iolaus changes --show-merge --graph
iolaus changes --show-merge | grep Merge
iolaus changes --show-merge --show-tested --max-count 1 | grep Tested

iolaus pull -a ../temp2

iolaus changes --graph --show-merge --show-hash

iolaus wh

grep '|||' bar && exit 1

echo hello > bar
iolaus wh
iolaus record -am hibar
iolaus changes --graph --max-count 5 --show-hash -s

iolaus wh

grep '|||' bar && exit 1
cat bar

echo bye > bar
iolaus whatsnew
#iolaus all --config-default --record-for ../temp1


iolaus record -am byebar --debug

iolaus push -a ../temp2

cd ../temp2

iolaus changes --graph
iolaus changes | grep hibar
iolaus changes | grep byebar

iolaus changes --count
iolaus changes --count | grep 5

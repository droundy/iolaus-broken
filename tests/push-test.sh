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

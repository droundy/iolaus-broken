set -ev

mkdir temp
cd temp
iolaus init
echo true > .test
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

cd ../temp2
iolaus pull -a ../temp1

iolaus wh
iolaus wh | grep chunk && exit 1

echo hello > bar
iolaus wh
iolaus record -am hibar
iolaus changes --show-merge --graph --show-hash -s

rm .git/refs/merges/*

iolaus wh
iolaus wh | grep chunk && exit 1

true

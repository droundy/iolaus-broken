set -ev

mkdir temp
cd temp
iolaus init
cd ..
iolaus get temp temp1
iolaus get temp temp2

cd temp
echo foo > foo
iolaus record -am addfoo
echo true > .test
chmod +x .test
iolaus record -am addtest
iolaus push -a ../temp1
#echo bar > bar
#iolaus record --record-for ../temp1 -am addbar
#iolaus push -a ../temp1

cd ../temp1

iolaus changes --graph
# the merge displays because it is a head
iolaus changes --graph | grep Merge
iolaus changes --count
iolaus changes --count | grep 3

cd ../temp2

echo yy | iolaus pull ../temp1

iolaus changes --graph
# the merge displays because it is a head
iolaus changes --graph | grep Merge
iolaus changes --count
iolaus changes --count | grep 3

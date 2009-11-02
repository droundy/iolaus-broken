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

echo q | iolaus pull ../temp > out
cat out

grep addfile out
grep ancelled out

echo xq | iolaus pull ../temp > out

cat out
grep addfile out
grep foo out
grep ancelled out

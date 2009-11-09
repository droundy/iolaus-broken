set -ev

mkdir temp
cd temp
iolaus init

date > foo
iolaus record -am addfoo

git tag 1.0

cd ..
mkdir temp1
cd temp1
iolaus init
cd ../temp

iolaus push -a ../temp1

cd ../temp1

git show-ref --tags
git show-ref --tags | grep 1.0

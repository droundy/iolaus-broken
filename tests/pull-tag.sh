set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record -am addfoo

git tag 3.0
cd ..

mkdir temp1
cd temp1
iolaus init
echo hello world > bar
iolaus pull -a ../temp

git show-ref --tags
git show-ref --tags | grep 3.0

diff -u foo ../temp/foo

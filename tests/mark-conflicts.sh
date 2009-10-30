set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record -am datefoo
cd ..

iolaus get temp temp1

cd temp
echo hello world > foo
iolaus record -am greetearth
cd ..

cd temp1
echo goodbye dear abode > foo
iolaus record -am 'farewell sweet home'

iolaus pull -a ../temp

ls -lh foo
cat foo
grep 'goodbye dear abode' foo
grep 'hello world' foo

grep 'farewell sweet home' foo
grep greetearth foo

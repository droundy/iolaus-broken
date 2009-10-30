set -ev

mkdir temp
cd temp
iolaus init
echo hello world > foo
iolaus record -am greetearth
cd ..

mkdir temp1
cd temp1
iolaus init
echo goodbye dear abode > foo
iolaus record -am 'farewell sweet home'

iolaus pull -a ../temp

ls -lh foo
cat foo
grep 'goodbye dear abode' foo
grep 'hello world' foo

grep 'farewell sweet home' foo
grep greetearth foo

set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
echo true > .test
chmod +x .test

iolaus wh | grep chmod

iolaus record -am addtest

cd ..
iolaus get temp temp1
cd temp1

echo foo > bar
iolaus wh

echo y | iolaus amend-record --debug --record-for ../temp -a > out
cat out

grep 'no commit to amend' out

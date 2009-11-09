set -ev

mkdir temp
cd temp
iolaus init
echo -n hello > foo
echo -n goodbye > bar
iolaus whatsnew

iolaus record -am hellofoo

iolaus whatsnew

cd ..
iolaus get temp temp2

diff -u temp/foo temp2/foo


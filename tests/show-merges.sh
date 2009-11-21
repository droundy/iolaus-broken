set -ev

mkdir temp
cd temp
iolaus init

date > foo
echo true > .test
chmod +x .test

iolaus record -am addfoo
cd ..

mkdir temp2
cd temp2
iolaus init
echo hello > foo
iolaus record -am hellofoo
cd ..

mkdir temp3
cd temp3
iolaus init
iolaus pull -a ../temp ../temp2

HASH=`cat .git/refs/tested/*`

# make sure we see a conflict marker!
grep '|||' foo
echo goodbye > foo

iolaus record -am goodbyefoo

iolaus changes --config-default --show-hash

iolaus changes | grep $HASH && exit 1
iolaus changes --graph | grep $HASH && exit 1

echo $HASH
iolaus changes --show-merges
iolaus changes --show-merges --graph

iolaus changes --show-merges | grep $HASH

iolaus all --config-default --show-merges

iolaus changes --hide-merges | grep $HASH && exit 1

iolaus changes --no-show-hash | grep $HASH && exit 1

iolaus changes | grep $HASH

set -ev

mkdir temp
cd temp
iolaus init

echo foo > foo
iolaus record -am addfoo

cd ..

iolaus get temp temp1
test -d temp1
cd temp1
echo mod > foo
iolaus record -am modfoo
cd ..

iolaus get --repo-name=temp2 temp1
cd temp2
date > bar
# in the following we gratuitously use the --not-nice flag, because I
# don't see how to effectively test that it behaves as intended, but
# want to at least make sure it doesn't crash things...
iolaus record --not-nice -am datebar
iolaus changes | grep dateba
cd ../temp1
date > baz
# in the following we gratuitously use the --nice flag, because I
# don't see how to effectively test that it behaves as intended, but
# want to at least make sure it doesn't crash things...
iolaus record --nice -am datebaz
iolaus changes | grep dateba

cd ..
iolaus get temp temp3

cd temp
iolaus changes | grep dateba && exit 1
iolaus changes | grep modfoo && exit 1

iolaus pull -a --union ../temp1 ../temp2

iolaus changes | grep dateba
iolaus changes | grep modfoo
cd ../temp3
iolaus changes | grep dateba && exit 1
iolaus changes | grep modfoo && exit 1

iolaus pull -a --union ../temp1 ../temp2

iolaus changes | grep dateba
iolaus changes | grep modfoo

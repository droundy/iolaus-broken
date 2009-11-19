set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record --author myself -am datefoo

iolaus changes
iolaus changes | grep myself

date > bar
iolaus record -am datebar

iolaus changes --show-parents | grep Parent && exit 1

echo y | iolaus unrecord --patches datebar

iolaus record --config-default --cauterize-all

iolaus record -am datebar

iolaus changes --show-parents | grep Parent

echo y | iolaus unrecord --patches datebar

iolaus changes --show-parents | grep datebar && exit 1
iolaus changes --show-parents | grep Parent && exit 1
iolaus wh | grep bar

# check that --no-cauterize-all inverts the default we set...
iolaus record --no-cauterize-all -am datebar
iolaus changes --show-parents | grep Parent && exit 1
iolaus changes --show-parents | grep datebar

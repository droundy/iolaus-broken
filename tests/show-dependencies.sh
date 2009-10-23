set -ev

mkdir temp
cd temp
iolaus init

date > foo
iolaus record -am addfoo

echo hello > foo

iolaus show dependencies > deps
grep addfoo deps

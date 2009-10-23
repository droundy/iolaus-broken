set -ev

mkdir temp
cd temp
iolaus init

date > foo1
iolaus record -am addfoo1

date > foo2
iolaus record -am addfoo2

date > foo3
iolaus record -am addfoo3

date > foo4
iolaus record -am addfoo4

date > foo5
iolaus record -am addfoo5

date > foo6
iolaus record -am addfoo6 --commute-past=2

cd ..

mkdir temp2
cd temp2
iolaus init
iolaus pull ../temp -p addfoo6 -a
ls foo* | wc -l > numfoos
ls foo*
cat numfoos

grep 4 numfoos

set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record -am addfoo
echo hello > bar
iolaus record -am addbar
echo bye > bar
iolaus record -am modbar
cd ..

iolaus get temp temp1

cd temp
for i in *; do
    echo comparing $i
    diff -u ../temp1/$i $i
done

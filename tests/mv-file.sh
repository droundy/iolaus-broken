set -ev

mkdir temp
cd temp
iolaus init

date > foo

iolaus record -am foo

mv foo fah

iolaus wh

iolaus whatsnew | grep rmfile && exit 1
iolaus whatsnew | grep mv

for i in `seq 1 100`; do
    echo hello >> fah
done

iolaus record -am mv1

rm fah

for i in `seq 200`; do
    echo goodbye $i >> aack
done

iolaus whatsnew | grep mv && exit 1
iolaus whatsnew | grep rmfile

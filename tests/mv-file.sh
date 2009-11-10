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

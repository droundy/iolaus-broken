set -ev

mkdir temp
cd temp
iolaus init
echo world > foo

iolaus whatsnew > wh
grep foo wh

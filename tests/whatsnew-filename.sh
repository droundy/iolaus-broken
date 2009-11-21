set -ev

mkdir temp
cd temp
iolaus init
echo wh > .gitignore
iolaus record -am 'ignore files named wh'

echo world > foo

iolaus whatsnew > wh
grep foo wh

echo hello > bar
iolaus whatsnew > wh
cat wh
grep foo wh
grep bar wh

iolaus wh bar > wh
cat wh
grep foo wh && exit 1
grep bar wh

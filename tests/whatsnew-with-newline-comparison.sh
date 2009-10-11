set -ev

mkdir temp
cd temp
iolaus init
echo hello > foo
echo goodbye > bar
iolaus whatsnew

iolaus record -am hellofoo

iolaus whatsnew

cd ..
iolaus get temp temp2

diff temp/foo temp2/foo

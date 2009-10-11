set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus whatsnew

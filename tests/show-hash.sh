set -ev

mkdir temp
cd temp
iolaus init

date > foo
iolaus record -am addfoo

iolaus changes --show-hash > showhash

iolaus changes > nohash

iolaus changes --no-show-hash > hidehash

HASH=`cat .git/refs/heads/master`

grep $HASH nohash && exit 1
grep $HASH hidehash && exit 1
grep $HASH showhash

set -ev

mkdir temp
cd temp
iolaus init
echo hello world > foo
iolaus record -am 'add foo date'

HEAD=`git rev-parse --verify HEAD`

iolaus show commit HEAD > h
cat h
iolaus show commit master > m
cat m
iolaus show commit $HEAD > s
cat s

grep 'add foo date' h
diff h m
diff m s
grep 'hello' h

iolaus show commit -s master > sum
grep A sum

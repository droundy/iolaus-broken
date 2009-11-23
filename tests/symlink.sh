set -ev

mkdir temp
cd temp
iolaus init

ln -s foobar foo

# this used to cause a crash...
iolaus wh

set -ev

mkdir temp
cd temp
cp ../../../test-file-old foo
iolaus init
iolaus record -am oldfoo

cp ../../../test-file-new foo

iolaus wh


set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record -am addfoo
cd ..

mkdir temp1
cd temp1
iolaus init
echo hello world > bar
iolaus pull -a ../temp
diff -u foo ../temp/foo

iolaus record -am addbar

echo good morning > baz
iolaus record -am addbaz
cd ..

cd temp
date > bar
iolaus pull -a ../temp1 && exit 1

iolaus pull -ap addbaz ../temp1
grep morning baz
cd ..

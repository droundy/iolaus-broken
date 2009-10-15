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
cd ..

cd temp
date > bar
iolaus pull ../temp1 && exit 1
true
cd ..

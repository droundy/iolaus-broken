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
cd ../temp
iolaus push -a ../temp1
cd ../temp1
iolaus revert -a # FIXME this is really hokey
diff -u foo ../temp/foo

echo hello world > bar
iolaus record -am addbar
iolaus push ../temp # we now force things... should we?
cd ..

true

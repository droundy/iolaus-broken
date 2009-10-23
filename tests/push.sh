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
iolaus push -a ../temp # we now force things... should we?

echo feature > code
iolaus record -am addfeature
echo bug >> code
iolaus record -am addbug
iolaus push -ap addfeature ../temp
cd ../temp

iolaus revert -a # I'd prefer for push to do this!

grep bug code && exit 1
grep feature code

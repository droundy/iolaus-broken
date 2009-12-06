set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record -am addfoo
cd ..

iolaus get temp temp1
cd temp
echo hello > foo
iolaus amend-record -a

# test-fails because sic-hercules hasn't yet been debugged.
echo yy | iolaus sic-hercules --debug ../temp1

iolaus push -a ../temp1
cd ../temp1
iolaus revert -a
cd ..
diff temp/foo temp1/foo

set -ev

mkdir trunk
cd trunk
iolaus init
date > foo
iolaus record -am addfoo
cd ..

iolaus get trunk proposed
iolaus get trunk local

cd local
echo hello world > foo
iolaus record -am hellofoo
git remote add proposed ../proposed
iolaus push -a proposed

echo goodbye world > foo
# fix the hellofoo patch
iolaus amend-record -a

# now we can kill 
echo yy | iolaus sic-hercules --debug proposed

iolaus push -a proposed
cd ../proposed
iolaus changes --graph
iolaus revert -a
cd ..
diff proposed/foo local/foo

cd proposed
iolaus changes | grep addfoo
iolaus changes -v | grep 'hello world' && exit 1
iolaus changes -v | grep 'goodbye world'

set -ev

mkdir temp
cd temp
iolaus init

date > foo
iolaus record -am addfoo
echo hi there > foo
iolaus record -am hello
date > foo
iolaus record -am datefoo
echo bye there > foo
iolaus record -am bye

# test-fails because -p hasn't yet been implemented.

# Reality check that the -p flag successfully limits the list of changes. 
iolaus changes -p hello > changes_p_hello.txt
grep bye changes_p_hello.txt && exit 1
grep addfoo changes_p_hello.txt && exit 1
grep datefoo changes_p_hello.txt && exit 1

grep hello changes_p_hello.txt

iolaus changes -p foo --count > out
grep 2 out

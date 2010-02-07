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

# check that changes --count works properly
iolaus changes --count > num
cat num
echo 4 | diff -u num -
rm num

# check that changes --max-count --count works properly
iolaus changes --max-count 3 --count > num
cat num
echo 3 | diff -u num -
rm num

iolaus changes > ch
cat ch
grep bye ch

iolaus changes --reverse > rch
cat rch
grep bye rch

sort rch > srch
sort ch > sch

diff -u srch sch

# Reality check that the -p flag successfully limits the list of changes. 
iolaus changes -p hello > changes_p_hello.txt
grep bye changes_p_hello.txt && false


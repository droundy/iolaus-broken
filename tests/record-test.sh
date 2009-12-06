set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
echo true > .test
chmod +x .test

iolaus wh | grep chmod

iolaus record -am addtest --debug

iolaus changes > ch
cat ch
grep Tested- ch && exit 1
iolaus changes --show-tested | grep Tested-

cat > .test <<EOF
set -v
echo running test
ls
true
EOF
iolaus record -am 'start clean' --cauterize-all

ls .git/refs/heads/master1 && exit 1
iolaus changes --show-merge --graph

cd ..
iolaus get temp temp1
cd temp1

date > foo
iolaus record -am addfoo

date > bar
iolaus record -am addbar

date > baz
iolaus record -am addbaz

cat .git/config
echo hello > foo
iolaus record -am hellofoo --debug

iolaus changes --graph --show-merge --show-hash
iolaus changes --graph --show-merge | grep Merge && exit 1

cat > .test <<EOF
set -ev
ls
grep hello foo
EOF

./.test
iolaus record --debug -am hellotest

iolaus push --dry-run --show-merge --show-parents ../temp

iolaus push -a -p hellotest ../temp

iolaus push --dry-run ../temp
iolaus push --dry-run ../temp | grep addbar
iolaus push --dry-run ../temp | grep addbaz

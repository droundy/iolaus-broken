set -ev

mkdir trunk
cd trunk
iolaus init
cat > .test <<EOF
echo this is a slow test
sleep 1
echo test is done sleeping
EOF
chmod +x .test
iolaus record -am addtest
cd ..

iolaus get trunk branch

diff -u trunk/.git/refs/heads/master branch/.git/refs/remotes/origin/master
cat trunk/.git/refs/heads/master

cd trunk
date > foo
iolaus record -am addfoo --cauterize-all
iolaus changes --graph --show-hash
cd ../branch

cat .git/refs/remotes/origin/master
cat ../trunk/.git/refs/heads/master
diff -u ../trunk/.git/refs/heads/master .git/refs/remotes/origin/master && exit 1

date > bar
iolaus record -am addbar
# check that we update the "remotes" when we do a record
diff -u ../trunk/.git/refs/heads/master .git/refs/remotes/origin/master

echo hello > bar

# check that record works even if there is a --record-for that fails
# due to network being down.  To simulate this, we will make 'origin'
# point to an invalid network address.
git config --get remote.origin.url | grep remotes.sh/trunk
git config remote.origin.url foo@example.com:test.git
git config --get remote.origin.url
iolaus record -am modbar

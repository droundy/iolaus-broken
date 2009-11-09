set -ev

mkdir temp
cd temp
iolaus init
echo hello > foo
echo hello1 > foo1

iolaus record -am hellofoo

# the following is a very hokey trick to create a file with a null
# byte in it.
git config foo.bar hello
git config foo.baz hello
git config --null --list > bin

iolaus whatsnew

iolaus whatsnew | grep hello && exit 1

iolaus whatsnew | grep binary

# The following triggered a bug in Diff...
rm bin
date >> foo
echo hello >> foo1
iolaus whatsnew

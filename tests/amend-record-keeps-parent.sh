set -ev

mkdir temp
cd temp
iolaus init
echo foo > bar
iolaus record -am foobar

echo bug > bar
iolaus record -am fixme

iolaus changes -v | grep bug

echo baz > bar
echo y | iolaus amend-record -a

iolaus changes

# We have removed the bug from the history!
iolaus changes -v | grep bug && exit 1

# We should also still have two commits.
iolaus changes | grep foobar
iolaus changes | grep fixme

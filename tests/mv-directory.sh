set -ev

mkdir temp
cd temp
iolaus init

mkdir foo
mkdir foo/bar
date > foo/bar/baz
cat > foo/bar/again <<EOF
This is a fairly long file
that won't be changed when
we move the directory, so iolaus
ought to recognize that it's
a rename, not a removal, etc.
EOF

iolaus record -am foobarbaz

mv foo fah

iolaus wh

iolaus whatsnew | grep rmdir && exit 1
iolaus whatsnew | grep mv

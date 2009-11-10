set -ev

mkdir temp
cd temp
iolaus init

cat > foo <<EOF
This is a paragraph which I am very glad to write about.  It has many
interesting words in it, and really is quite unique.
EOF

iolaus record -am oldfoo

cat > foo <<EOF
We are now writing again, but without using any of the
old phrases, so that we can see great diffing!
EOF

iolaus wh

iolaus wh | grep 'We are now writing again, but without using'
iolaus wh | grep 'old phrases, so that we can see great diffing!'
git config --get-color "" "reset"
iolaus wh | grep 'This is a paragraph which I am very glad to write about.'
iolaus wh | grep 'interesting words in it, and really is quite unique.'
git config --get-color "" "reset"


iolaus record -am newfoo

cat > foo <<EOF
We are now writing again, but carefully reusing some of the
old phrases, so that we can see great diffing!
EOF

iolaus wh

iolaus wh | grep 'We are now writing again, but '
iolaus wh | grep 'carefully reusing some'
iolaus wh | grep ' of the'

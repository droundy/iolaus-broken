set -ev

mkdir temp
cd temp
iolaus init
cat > .gitignore <<EOF
*~
EOF

mkdir src
echo oops > src/oops~
echo good > src/good

iolaus wh -s
iolaus wh -s | grep good
iolaus wh -s | grep oops && exit 1

cd src

iolaus wh -s
iolaus wh -s | grep good
iolaus wh -s | grep oops && exit 1

true

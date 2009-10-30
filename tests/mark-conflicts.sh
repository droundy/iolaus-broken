set -ev

mkdir temp
cd temp
iolaus init
date > foo
iolaus record -am datefoo
cd ..

iolaus get temp temp1

cd temp
echo hello world > foo
iolaus record -am greetearth
cd ..

cd temp1
echo goodbye dear abode > foo
iolaus record -am 'farewell sweet home'

iolaus pull -a ../temp

ls -lh foo
cat foo
grep 'goodbye dear abode' foo
grep 'hello world' foo

grep 'farewell sweet home' foo
grep greetearth foo

#########################
echo now we will start over and look at modifications to adjacent lines.

cat > foo <<EOF
a
b
c
d
e
EOF
iolaus record -am 'a new beginning'

cd ../temp
iolaus pull -a ../temp1
diff -u foo ../temp1/foo

cat > foo <<EOF
a
B
C
d
e
EOF
iolaus record -am 'BC'

cd ../temp1
cat > foo <<EOF
a
b
CC
D
e
EOF
iolaus record -am 'CCD'

iolaus pull -a ../temp
cat foo
diff -u foo - <<EOF
a
B
||| BC >>>
C
<<< BC |||
||| CCD >>>
CC
<<< CCD |||
D
e
EOF


#########################
echo now we will start over and look at modifications to adjacent words.

sleep 1
echo a b c d e > foo
iolaus record -am 'a new beginning with words'

cd ../temp
iolaus pull -a ../temp1
diff -u foo ../temp1/foo

sleep 1
echo a B C d e > foo
iolaus record -am 'BC words'

cd ../temp1
sleep 1
echo a b CC D e > foo
iolaus record -am 'CCD words'

iolaus pull -a ../temp
cat foo
# done catting foo
diff -u - foo <<EOF
a ||| BC words >>>
B C d
<<< BC words |||
||| CCD words >>>
b CC D
<<< CCD words ||| e
EOF


set -ev

mkdir temp
cd temp

mkdir .git-hooks

cat > .git-hooks/test <<EOF
#!/usr/bin/env perl

foo();

bar("goodbye","world");

sub foo {
  print "hello world\n";
}

sub bar {
  my (\$a, \$b) = @_;
  print "\$a \$b\n";
}

bar("another nice","test");

EOF
chmod +x .git-hooks/test

iolaus init
iolaus record -am 'create test' > out
cat out

grep hello out
grep goodbye out
grep 'another nice test' out

cat > .git-hooks/test <<EOF
#!/usr/bin/env perl

foo();

baz("goodbye","world");

bar("is this a new bug?", "hello");

sub foo {
  print "hello silly world\n";
}

sub baz {
  my (\$a, \$b) = @_;
  print "\$a \$b\n";
}

baz("yet another nice","test");

baz("silly world", "uh huh");

EOF

iolaus record -am 'modify test' && exit 1

echo the above should fail, since we added bugs to test

iolaus record --delta-debug -am 'safe parts of test'

iolaus wh

grep baz .git-hooks/test
grep bar .git-hooks/test

iolaus revert -a

grep bar .git-hooks/test && exit 1
grep baz .git-hooks/test

# test-fails because we don't try very hard to find the largest subset
# of patches that passes the test.

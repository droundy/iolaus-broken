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
iolaus record -am 'create test'

cat > .git-hooks/test <<EOF
#!/usr/bin/env perl

foo();

baz("goodbye","world");

baz("is this a new bug?", "hello");

sub foo {
  print "hello silly world\n";
}

sub baz {
  my (\$a, \$b) = @_;
  prin "\$a \$b\n";
}

baz("yet another nice","test");

baz("silly world", "uh huh");

EOF

iolaus record -am 'modify test' && exit 1

echo the above should fail, since we added bugs to test

iolaus record --delta-debug -am 'safe parts of test'

iolaus wh

#grep baz .git-hooks/test
#grep bar .git-hooks/test

# test still fail
./.git-hooks/test && exit 1

iolaus revert -a

# make sure test now passes
./.git-hooks/test

#grep bar .git-hooks/test && exit 1
#grep baz .git-hooks/test

# This test passes, but not very well.  I'd like for iolaus to be able
# to figure out that if it just reverses the print -> prin
# misspelling, all the rest of the tests pass.  I think I'm using the
# delta debugging algorithm inappropriately.

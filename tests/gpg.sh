set -ev

export GIT_COMMITTER_NAME='Testing Person'
export GIT_COMMITTER_EMAIL='tester@example.com'

if test -d ../../dotgnupg; then
    cp -a ../../dotgnupg .gnupg
else
    gpg --gen-key --batch <<EOF
%echo generating a new gpg key...
Key-Type: DSA
Key-Length: 512
Key-Usage: sign
Name-Real: Testing Person
Name-Email: tester@example.com
%echo done generating a new gpg key...
EOF
    gpg --list-secret-keys
fi

mkdir temp
cd temp
iolaus init
date > foo
iolaus record --sign -am addfoo
cd ..

mkdir temp1
cd temp1
iolaus init

iolaus pull --debug --verify --dry-run ../temp

# pull should fail when given a nonexistent keyring
iolaus pull -a --verify-with xxx --dry-run ../temp && exit 1

iolaus pull --debug -a --verify-with ~/.gnupg/pubring.gpg --dry-run ../temp

iolaus pull --debug --verify -a ../temp
diff -u foo ../temp/foo

cd ../temp
echo hello world > foo
iolaus record -am hellofoo

cd ../temp1

# hellofoo wasn't signed to attempts to verify it should fail
iolaus pull -a --verify ../temp && exit 1
iolaus pull -a --verify-with ~/.gnupg/pubring.gpg ../temp && exit 1

# but it should be a valid record
iolaus pull -a ../temp
diff -u foo ../temp/foo

set -ev

# A few commands that don't really need testing...
iolaus --version
iolaus --exact-version

# for now, --overview, --help and help are redundant.  Is this good?
iolaus --overview > over
iolaus help > help
iolaus --help > help2
diff help over
diff help help2

for i in `iolaus --commands`; do
    set -ev
    echo -n Checking if $i is tested...
    grep -- "iolaus $i" ../../*.sh > out
    echo good.
done

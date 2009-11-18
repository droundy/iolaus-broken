set -ev

# test-fails

# A few flags that don't really need testing...

# --no-summary --debug-verbose --system

HAVEERR=0

for i in `iolaus all --list-options | sort --unique`; do
    set -ev
    if grep -- $i ../../*.sh > out; then
        #echo Checking if $i is tested... good.
        true
    else
        echo There is no test for $i
        HAVEERR=1
    fi
done

exit $HAVEERR

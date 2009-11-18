set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
chmod +x .gitignore

iolaus wh | grep chmod

echo yd | iolaus record -m addtest

iolaus wh | grep add && exit 1
iolaus wh | grep chmod

iolaus revert -a

iolaus wh | grep add && exit 1
iolaus wh | grep chmod && exit 1

iolaus unrevert

iolaus wh | grep add && exit 1
iolaus wh | grep chmod

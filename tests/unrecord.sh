set -ev

mkdir temp
cd temp
iolaus init
echo '*~' > .gitignore
date > foo
iolaus record -am addfoo
date > bar
iolaus wh > datebar~
iolaus record -am addbar

iolaus unrecord -p addbar -a

iolaus wh > datebar2~
diff datebar~ datebar2~

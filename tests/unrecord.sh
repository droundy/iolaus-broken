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

# FIXME the following behavior isn't ultimately what we want...
iolaus unrecord -a

iolaus wh > datebar2~
diff datebar~ datebar2~

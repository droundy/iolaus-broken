# Interacting with other repositories

This is an overview of the commands you can use to interact with
remote repositories.  To get more detail about any given command,
just click on its name.

[get][]
-----

This is basically just a synonym for `git clone`.  It gets a copy of
a specified repository.

[pull][]
-------

Iolaus [pull][] is quite similar to `git pull`.  You are prompted for
commits, and they are merged with your local changes.

[push][]
-------

[Push][push] is not *quite* the inverse of [pull][], since it doesn't
update the working directory of the remote repository.  Which is great
when you're pushing to a bare repository, but less than optimal if you
push to an ordinary repo.


[get]: get.html
[pull]: pull.html
[push]: push.html

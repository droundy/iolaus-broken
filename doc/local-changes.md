This is an overview of the commands you can use to modify your local
repository.  To get more detail about any given command, just click on
its name.

[initialize][]
-------------

You usually only [initialize][] once, so I won't say anything more
about it here.

[record][]
---------

One of the first things you'll want to do is [record][] some changes.
[record][] does essentially a `git add` and a `git commit`, and
records some of your locally-made changes.  To see what there is to
record, you'd want to use [whatsnew][].

[amend][]
---------

Another popular command is [amend-record][].  Like all darcs commands,
you can use any unique prefix to specify [amend-record][], and
[amend][] is a particularly friendly one.  [amend][] modifies an
already-recorded commit.  You don't want to do this if you've already
made that commit public, but it's handy when you just made a mistake.

[unrecord][]
-----------

[Unrecord][unrecord] does what it says: it undoes a [record][].  So
[amend][] is roughly equivalent to [unrecord][] followed by
[record][] (but more convenient).

[revert][]
---------

[Revert][revert] is in quite a different category, in that it prompts
you for locally-made changes that you want to remove from your working
directory.  I think this is sort of like `git reset`.

[unrevert][]
-----------

[Unrevert][unrevert] tries to undo the last [revert][].  It isn't
yet very clever, however, so it may fail.  Still, it's a handy trick
to be able to [revert][] something, see if it fixes a bug, and then
[unrevert][] it.

[initialize]: initialize.html
[record]: record.html
[whatsnew]: whatsnew.html
[amend]: amend-record.html
[unrecord]: unrecord.html
[revert]: revert.html
[unrevert]: unrevert.html

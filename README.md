Iolaus
======

<img alt="legendary head management" src="doc/hydra.svg" align="right">

"On Lerna's murderous hound, the many-headed hydra, he set his
branding-iron..."

*Herakles*, Euripides

"...and in his turn called for help on Iolaus who, by setting fire to
a piece of the neighbouring wood and burning the roots of the heads
with the brands, prevented them from sprouting. Having thus got the
better of the sprouting heads, he chopped off the immortal head, and
buried it, and put a heavy rock on it, beside the road that leads
through Lerna to Elaeus."

*Apollodorus*, translated by Sir James G. Frazer, 1921.

The origins of Iolaus
---------------------

I realized that the semantics of git are actually not nearly so far
from those of darcs as I had previously thought.  In particular, if we
view each commit as describing a patch in its "primitive context" (to
use darcs-speak), then there is basically a one-to-one mapping from
darcs' semantics to a git repository.   The catch is that it must be a 
git repository with *multiple heads*!

Fortunately, this is not such a foreign concept to git.  In fact, git
has a whole framework to help users manage repositories with multiple
heads (see, e.g. checkout and branch).  So it's not so very foreign.
There are just a couple of major differences how git works.  First, in
git your working directory will only reflect *one* of the heads, while
in darcs (or iolaus) the working directory reflects the union of all
changes in the repository.

The other major difference is that in git, new commits will normally
not introduce new heads, meaning that the history is normally linear.
This inhibits "cherry picking", since you cannot pull a patch without
pulling its parent.  Iolaus instead commutes patches back, so that the
parents of each commit are only the patches that that patch depends
upon.  In practice, we don't want to commute *that* far back, so we
only commute a far back as we might want to cherry pick.

Legendary head management
-------------------------

Iolaus takes charge of creating and decapitating heads.  This makes
its name something of a misnomer, since Heracles was in charge of
removing heads, which grew spontaneously, and Iolaus was merely tasked
with preventing their regrowth.  Nevertheless, iolaus the program
takes charge of both the growth and decapitation of heads, allowing
you (Heracles?) the programmer to focus your attention on
code---that's the theory, anyhow.

Why would you want to use Iolaus?
---------------------------------

- You're tired of the snappy response git provides, and want something
  that gives you a bit more time to check your email.

- You believe that anything written in Haskell must be better than
  anything written in C.

- You like darcs' user interface and want to use something familiar to
  deal with smallish git repositories.

- You want to experiment with a new git porcelain.

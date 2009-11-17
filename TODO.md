- Add delta debugging support for commits.  Essentially git bisect,
  but allowing for new merges, so we get a whole lot more potential
  combinations.

- Somehow allow record --record-for REPO to not always contact remote
  repo, so record will work offline (and be faster with a high-latency
  connection).  One problem is that of how to deal with situations
  where the --record-for is not a defined remote (e.g. --record-for
  git@github.com/droundy/iolaus).  The other is that of figuring out
  when to update the cached remote head information, since the
  --record-for may differ from the origin, pull repo, push repo, etc.
  In short, record may be the *only* scenario in which we interact
  with this particular remote, so *never* contacting the remote on
  record doesn't sound like such a hot idea.

- Add support (and documentation) for proper git-style hooks.

- Figure out what to call the .git-hooks/{test,build} scripts.

- Add proper support (and documentation) for proper git-style color
  configuration.  We're already part of the way there, but there's no
  support for the mechanism for turning color off.  The default will
  be color, however.


Iolaus is currently only available in source code format. Building iolaus
from source depends on the 'franchise' Haskell package installed, which in
turns depends on having darcs and GHC installed, as well as haddock.

Here's a process for Ubuntu Linux to get you started:

    sudo apt-get install ghc darcs haddock git-core

    darcs get --lazy http://physics.oregonstate.edu/~roundyd/code/franchise
    cd franchise
    sudo runhaskell Setup.hs install
    cd ../
    git clone git://github.com/droundy/iolaus.git
    cd iolaus

    runghc Setup.hs build

At this point you should have an "iolaus" binary in the current directory.  If
you are just trying it out, considering making a symlink to it from a directory
in your path, perhaps like this:

  ln -s $PWD/iolaus ~/bin/

You could also install it globally, although there is currently no easy "uninstall"
method:

    sudo runghc Setup.hs build

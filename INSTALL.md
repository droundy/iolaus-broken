If you're considering installing iolaus, you should first be aware
that iolaus is *completely unstable* and *completely unsupported*.
You are considering using experimental software that its author does
not consider to be fit for general consumption (and has therefore
never publicly announced) and has no time and little desire to make
fit for your consumption.  Not only that, but its build process
depends on software that is also unstable, which is also is not yet
intended for general use.

Iolaus is currently only available in source code format.  Building
iolaus from source depends on the 'franchise' Haskell package
installed, which in turns depends on having GHC installed, as well as
haddock.

Here's a process for Debian or Ubuntu to get you started:

    sudo apt-get install ghc haddock git-core

    git clone git://github.com/roundy/franchise.git
    cd franchise
    runhaskell Setup.hs configure --prefix=$HOME/local --user
    runhaskell Setup.hs install
    cd ../
    git clone git://github.com/droundy/iolaus.git
    cd iolaus

    runghc Setup.hs build

At this point you should have an "iolaus" binary in the bin/
directory.  If you are just trying it out, considering making a
symlink to it from a directory in your path, perhaps like this:

  ln -s $PWD/bin/iolaus ~/bin/

You could also install it globally with:

    sudo runghc Setup.hs install

and then uninstall later with

    sudo runghc Setup.hs uninstall

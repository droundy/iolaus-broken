
Iolaus is currently only available in source code format. Building iolaus
from source depends on the 'franchise' Haskell package installed, which in
turns depends on having darcs and GHC installed, as well as haddock.

Here's a process for Ubuntu Linux to get you started:

    sudo apt-get install ghc darcs haddock git-core
    
    darcs get --lazy http://franchise.abridgegame.org/franchise
    cd franchise
    sudo runhaskell Setup.hs install
    cd ../
    git clone git://github.com/droundy/iolaus.git
    cd iolaus
    
    runghc Setup.hs


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

runghc Setup.hs

# XXX FIXME: the last step currently results in errors for me, with GHC 6.10.4:

Setup.hs:13:10: Not in scope: `hcFlags'

Setup.hs:15:32: Not in scope: `etc'

Setup.hs:23:7: Not in scope: `privateExecutable'







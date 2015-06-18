# factorial-server
Small demos (factorial) for use of the snap framework, part of the UU AFP summerschool lecture on web programming.

# Installation & running
Build using cabal. Location of executables is mentioned by cabal build. The following factorial server variants are built:
* factorial-server-0hw: a hello world minimal server
* factorial-server-1html: rendering via blaze html
* factorial-server-2heist: rendering via snap+heist, digestive functor forms

Then run by (e.g.):
* .cabal-sandbox/bin/factorial-server-0hw -p 8000
* open a browser at http://localhost:8000

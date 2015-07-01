# factorial-server
Small demos (factorial) for use of the snap framework, part of the UU AFP summerschool lecture on web programming.

# Installation & running
Build using cabal. Location of executables is mentioned by cabal build. The following factorial server variants are built:
* factorial-server-0hw: a hello world minimal server
* factorial-server-1html: rendering via blaze html
* factorial-server-2heist: rendering via snap+heist, digestive functor forms
* factorial-server-3acid: same as 2heist but with persistency via acid

Then run by (e.g.):
* .cabal-sandbox/bin/factorial-server-0hw -p 8000
* open a browser at http://localhost:8000 (let url of this be called the `SERVER`)

In the browser, for
* factorial-server-0hw
  * `SERVER`, the static factorial result
* factorial-server-1html
  * `SERVER`/factorial/`nr`, the factorial of `nr`, in plain text
  * `SERVER`/factorial2/`nr`, the factorial of `nr`, in html text
  * `SERVER`/factorial3/`nr`, the factorial of `nr`, in html page (with body tag etc)
  * `SERVER`/factorial4/`nr`, the factorial of `nr`, in html page with table
* factorial-server-2heist
  * `SERVER`/factorial/`nr`, the factorial of `nr`, html created via heist template
  * `SERVER`/factorial2, `nr` entered & displayed via digestive form
* factorial-server-3acid
  * `SERVER`/factorial, `nr` entered & displayed via digestive form, remembered persistently

A short URL redirector, in Snap
===============================

Simple use case: immediately HTTP redirect in response to requests based on
looking up the target corresponding to the supplied hash code. For example,

<http://odyn.co/3YYIt>  →  <http://www.bbc.co.uk/news/>

Implemented as a small web application in Haskell, using the Snap Framework.
You'll want to have Redis installed locally to use it.

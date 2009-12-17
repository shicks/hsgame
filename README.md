hsgame
======

To compile hsgame, you'll need a recent version of
[franchise](http://franchise.abridgegame.org) and
[ghc](http://haskell.org/ghc).  You can compile with

    ./Setup.hs build

This builds a large number of test executables.  You can try running a
very simple chat server by running either `./named-chat` or `./chat`.
You can then connect to the server with a client by running
`./named-chat localhost` or `./chat localhost`.  Of course, you can
also connect from another computer, in which case you should specify
the name or IP address of your server in place of `localhost`.

We're working on an Javascript framework, built around a custom
HTTP server, which you can run with `./httpd` to start a server
at `http://127.0.0.1:8081`.
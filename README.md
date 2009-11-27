hsgame
======

To compile hsgame, you'll need a recent version of
[franchise](http://franchise.abridgegame.org) and ghc.  You can
compile with

    ./Setup.hs build

This builds a large number of test executables.  You can try running a
very simple chat server by running either `./named-chat` or `./chat`.
You can then connect to the server with a client by running
`./named-chat localhost` or `./chat localhost`.  Of course, you can
also connect from another computer, in which case you should specify
the name or IP address of your server in place of `localhost`.

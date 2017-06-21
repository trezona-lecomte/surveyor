*WIP!*

# Client

To set up for local development, clone the repo and then run the
following from within /client:

    elm make Main.elm --output=elm.js

    open index.html

You should now see the app running.

To recompile sass to css after making changes, run the following from
within /client:

    sass surveyor.sass surveyor.css

# Server

To compile the server, run the following from within /server:

    stack setup # this installs an isolated GHC to ~/.stack
    stack build # this compiles app/Main.hs, building server-exe
    stack exec server-exe # this runs server-exe

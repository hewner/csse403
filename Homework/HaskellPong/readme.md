# Haskell Pong

So I'd like you to go through the very nice tutorial here:

http://andrew.gibiansky.com/blog/haskell/haskell-gloss/

(I've also put [a pdf](tutorial.pdf) in the repo, but I think you'll
find the HTML version nicer)

To make this work, you'll need to have a working Haskell and OpenGL development environment.  For me, on a freshly installed Ubuntu 24 WSL image these packages were sufficient:

sudo apt install ghc cabal-install haskell-stack libgl1-mesa-dev mesa-utils libglew-dev freeglut3-dev

Also just use the latest version of gloss not what it suggests in the tutorial.

The [given code](Main.hs) is the first stage of the graphics part of
the tutorial, although you are welcome to start from scratch.

It is a good introduction both to Haskell's package system Cabal and
gloss, the graphics library we'll be using in Haskell.

For the homework, complete the tutorial itself and Exercises 1-3.


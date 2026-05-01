# Haskell Video Game

For your final Haskell project, I'm going to ask you to build a video
 game that's a little more sophisticated than Pong - a classic-style
 scrolling shooter game

My personal nostalgic example of this genre is Raptor: Call of the
Shadows.  You can see it here:

https://www.youtube.com/watch?v=ZUfeVcIg3Mg

# The Basics [40 Points]

Your game should have the distinctive elements of the
scrolling shooter genre:

   a.  A flying ship (that's you) that shoots

   b.  Tons of enemies that fly around (usually in prearranged patterns,
       coming and going at specific times)

   c.  You can blow up the enemies, your enemies can blow you up.
       Crashing into enemies blows them up and at least damages you. 

I encourage you to use aswd for motion and hjkl for the various weapon
kinds BTW.

It should be visbly obvious when you die, and there should be a key
(n) that starts a fresh game.

# A "Pipelined" Update [5 points]

One aspect of functional design that haskell shares is the idea of
decomposing a system into "layers" of updates that are implemented by
pipelines of functions (and these layers are then parts of even larger
layered systems like the handle/update/render system gloss provides).

Here's what my update function looks like:

    update :: Float -> PongGame -> PongGame
    update seconds = moveEverything . runUpdates . removeIntersections

You don't need to follow my design but I do want you to try and keep
the idea of a pipline consisting of several layers of functions.

# A Composible System for Making Enemies Move [30 points]

The reason I selected this project is because I think the various
kinds of enemies and weapons will encourage you to think about
abstractions that will let you build varied enemies and weapons out of
abstract and simple parts.

For what I understand of functional style, the goal tends to use
functions as raw building blocks to form "languages" of parts.

For example, here's a bullet I made in my system that breaks into four
parts:

    downThenRight :: Entity -> [Entity]
    bigSplitForever =
      fastup
      $ wait 15
      $ basicsplit
      $ wait 15
      $ basicsplit
      $ wait 15
      $ fastup
      $ continueForever

This same system is what I use for making enemies move and attack.

The only requirement is that you make enemies and weapons move in
significantly different ways.  The ability to "split" adds more
potential, but I won't require it.

To do this, you basically need to things:

1.  Different mathematical ways to move (e.g. up down diagonal in a
    circular arc etc) and different speeds
2.  A way to switch the way something moves over time (e.g. start by
    moving horizontally, then switch to diagonally)

From these basic ideas you can make almost an infinite number of cool
effects.  Little weapons that spray everywhere, big weapons that
charge up and then go, bombs that move slowly then explode, shields
that fly around you etc.  And of course tons of enemies that fly in
different patterns.

Use these ideas to create at least 5 creatively moving things.  Could
be different enemies, could be special weapons.


# A Few Notes


1. Please make sure it is clear how to play your game.  In the
   comments is fine, but I should have to go code spelunking to figure
   out what fires bullets.

2. Graphics are not important, except insofar as they communicate
   gameplay elements.  If you want this game to be about heroic green
   triangles and their attempt to defend their homeland from the
   nefarious red,yellow,and blue triangles I'm fine with that.

3. Lots of these games incorporate randomness, but that's not required
   here.  It's a little difficult to use randomness in haskell/gloss
   but you're welcome to try it out if you wish.

4. Make it clear what your 5 creative things are.

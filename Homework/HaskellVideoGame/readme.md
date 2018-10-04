# Haskell Video Game

For your final Haskell project, I'm going to ask you to build a video game
in elm that's a little more sophisticated than Pong - a
classic-style scrolling shooter game

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

# A Composible System for Enemies/Weapons [30 points]

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

It is not important to me that your make your system work like mine.
But I do want to see some evidence that you're composing your system
out of various parts.


# Weapons [15 Points]

3. [20 points] At least 3 qualitatively different weapons.  These
should not just be different color weapons that do different amounts
of damage.  At the very least, they should fire in highly different
patterns, spawn additional sub-bullets etc.  At best, they should act
differently (area of effect, damage over time, homing, etc.).  The
weapon effects should be clear visibly (obvious for homing, but damage
over time might need a halo effect or something).

Make it so I can use the different weapons immediately at the
beginning of the game (don't make them powerups).  You don't have to
be an expert game designer to get full credit, but I encourage you to
try and make weapons that are balanced and have strategy for when
you'd use one vs. the other.

# Enemies [15 points]

At least 3 qualitatively different enemies.  Again, more different
than just different amounts of life.  At the very least, different
patterns of movement and different numbers of bullet sources/times for
shooting.  At best, some simplistic AI or different attack effects.


# A Few Notes


1. Please make sure it is clear how to play your game.  In the
   comments is fine, but I should have to go code spelunking to figure
   out what fires bullets.

2. Graphics are not important, except insofar as they communicate
   gameplay elements.  If you want this game to be about heroic green
   triangles and their attempt to defend their homeland from the
   nefarious red,yellow,and blue triangles I'm fine with that.

3.  Lots of these games incorporate randomness, but that's not
    required here.  It's a little difficult to use randomness in
    haskell/gloss but you're welcome to try it out if you wish.

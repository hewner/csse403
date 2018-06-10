{-

For your final Elm project, I'm going to ask you to build a video game
in elm that's a little more sophisticated than Spacewar - a
classic-style scrolling shooter game

My personal nostalgic example of this genre is Raptor: Call of the
Shadows.  You can see it here:

https://www.youtube.com/watch?v=ZUfeVcIg3Mg

NOTE: Please make sure it is clear how to play your game.  In the
comments is fine, but I should have to go code spelunking to figure
out what fires bullets.

Here are the requirements:

1.  Graphics are not important, except insofar as they communicate
gameplay elements.  If you want this game to be about heroic green
triangles and their attempt to defend their homeland from the nefarious
red,yellow,and blue triangles I'm fine with that.

But if I can't tell what's a power-up, or all the weapons seem the
same because I can't see how they act differently - that's bad.

Note that if you want to have graphics, I reccommend you host the
graphics on a website you own so that your code runs in the online elm
"try" mode (I think that should work anyways).  In the past, I've had
problems installing students' games. In a pinch though, I'm happy to
demo it on your computer if that's what you need.

2. [33 points] Your game should have the distinctive elements of the
scrolling shooter genre:

   a.  A background that automatically slowly scrolls (could just be
       stars or clouds or something)

   b.  A flying ship (that's you) that shoots

   c.  Tons of enemies that fly around (usually in prearranged patterns,
       coming and going at specific times)

   d.  You can blow up the enemies, your enemies can blow you up.
       Crashing into enemies blows them up and at least damages you. 

3. [33 points] At least 4 qualitatively different weapons.  These
should not just be different color weapons that do different amounts
of damage.  At the very least, they should fire in highly different
patterns, spawn additional sub-bullets etc.  At best, they should act
differently (area of effect, damage over time, homing, etc.).  The
weapon effects should be clear visibly (obvious for homing, but damage
over time might need a halo effect or something).

Make a cheat or something so I can try all the weapons without getting
tons of powerups.

4. [33 points] At least 4 qualitatively different enemies.  Again,
more different than just different amounts of life.  At the very
least, different patterns of movement and different numbers of bullet
sources/times for shooting.  At best, some simplistic AI or different
attack effects.

The enemies should be visibly distinct.

-}


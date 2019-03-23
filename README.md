# Nature of code
[Nature of code] examples and exercises implemented in
Lisp using [trivial-gamekit].

![Example gif](https://raw.githubusercontent.com/mark-gerarts/nature-of-code/master/screenshots/Example%208.7%3A%20Dynamic%20recursive%20tree.gif)

## Installation and usage
1. You'll first have to add cl-bodge to quicklisp, as per the [install instructions]
of trivial gamekit:
`(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")`

1. Make sure quicklisp is able to detect this package (e.g. put it in
`~/quicklisp/local-projects`).

1. `(ql:quickload :nature-of-code)`

1. Run a sketch using for example `(nature-of-code.introduction.example-1:start-sketch)`.
You can find these snippets in the README of each example.

Some notes:
- `trivial-gamekit` only supports 2D animations, so examples and exercises involving 3D are omitted.
- The directory structure is set up to be easily browsable. This makes the ASDF definition kinda ugly.

[Nature of code]: http://natureofcode.com
[trivial-gamekit]: https://github.com/borodust/trivial-gamekit
[install instructions]: https://borodust.github.io/projects/trivial-gamekit/#installation-and-loading

## Table of contents
- [0. Introduction](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction)
  - [Example I.1: Traditional random walk](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Example%20I.1%3A%20Traditional%20random%20walk)
  - [Example I.2: Random number distribution](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Example%20I.2%3A%20Random%20number%20distribution)
  - [Example I.3: Walker that tends to move to the right](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Example%20I.3%3A%20Walker%20that%20tends%20to%20move%20to%20the%20right)
  - [Example I.4: Gaussian distribution](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Example%20I.4%3A%20Gaussian%20distribution)
  - [Example I.5: Perlin noise walker](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Example%20I.5%3A%20Perlin%20noise%20walker)
  - [Example I.6: 2D Perlin noise](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Example%20I.6%3A%202D%20Perlin%20noise)
  - [Exercise I.3: Dynamic walker](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Exercise%20I.3%3A%20Dynamic%20walker)
  - [Exercise I.4: Gaussian paint](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Exercise%20I.4%3A%20Gaussian%20paint)
  - [Exercise I.5: Gaussian random walk](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Exercise%20I.5%3A%20Gaussian%20random%20walk)
  - [Exercise I.6: Custom distribution walk](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Exercise%20I.6%3A%20Custom%20distribution%20walk)
  - [Exercise I.7: Perlin noise step size walker](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Exercise%20I.7%3A%20Perlin%20noise%20step%20size%20walker)
  - [Exercise I.8: 2D Perlin noise colour](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Exercise%20I.8%3A%202D%20Perlin%20noise%20colour)
  - [Exercise I.9: Moving 2D Perlin noise](https://github.com/mark-gerarts/nature-of-code/tree/master/0.%20Introduction/Exercise%20I.9%3A%20Moving%202D%20Perlin%20noise)
- [01. Vectors](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors)
  - [Example 1.1: Bouncing ball with no vectors](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.1%3A%20Bouncing%20ball%20with%20no%20vectors)
  - [Example 1.2: Bouncing ball with vectors](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.2%3A%20Bouncing%20ball%20with%20vectors)
  - [Example 1.3: Vector subtraction](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.3%3A%20Vector%20subtraction)
  - [Example 1.4: Multiplying a vector](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.4%3A%20Multiplying%20a%20vector)
  - [Example 1.5: Vector magnitude](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.5%3A%20Vector%20magnitude)
  - [Example 1.6: Normalizing a vector](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.6%3A%20Normalizing%20a%20vector)
  - [Example 1.7: Motion 101 (velocity)](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.7%3A%20Motion%20101%20%28velocity%29)
  - [Example 1.8: Motion 101 (velocity and constant acceleration)](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.8%3A%20Motion%20101%20%28velocity%20and%20constant%20acceleration%29)
  - [Example 1.9: Motion 101 (velocity and random acceleration)](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.9%3A%20Motion%20101%20%28velocity%20and%20random%20acceleration%29)
  - [Example 1.10: Acceleration towards the mouse](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.10%3A%20Acceleration%20towards%20the%20mouse)
  - [Example 1.11: Array of movers accelerating towards the mouse](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Example%201.11%3A%20Array%20of%20movers%20accelerating%20towards%20the%20mouse)
  - [Exercise 1.4: Vector limit](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Exercise%201.4%3A%20Vector%20limit)
  - [Exercise 1.5: Car acceleration simulation](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Exercise%201.5%3A%20Car%20acceleration%20simulation)
  - [Exercise 1.6: Perlin acceleration](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Exercise%201.6%3A%20Perlin%20acceleration)
  - [Exercise 1.8: Variable acceleration towards the mouse](https://github.com/mark-gerarts/nature-of-code/tree/master/01.%20Vectors/Exercise%201.8%3A%20Variable%20acceleration%20towards%20the%20mouse)
- [02. Forces](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces)
  - [Example 2.1: Forces](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Example%202.1%3A%20Forces)
  - [Example 2.2: Forces acting on many objects](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Example%202.2%3A%20Forces%20acting%20on%20many%20objects)
  - [Example 2.3: Gravity scaled by mass](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Example%202.3%3A%20Gravity%20scaled%20by%20mass)
  - [Example 2.4: Including friction](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Example%202.4%3A%20Including%20friction)
  - [Example 2.5: Fluid resistance](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Example%202.5%3A%20Fluid%20resistance)
  - [Example 2.6: Attraction](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Example%202.6%3A%20Attraction)
  - [Example 2.7: Attraction with many movers](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Example%202.7%3A%20Attraction%20with%20many%20movers)
  - [Exercise 2.1: Helium-filled balloon](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Exercise%202.1%3A%20Helium-filled%20balloon)
  - [Exercise 2.3: Invisible force](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Exercise%202.3%3A%20Invisible%20force)
  - [Exercise 2.4: Pockets of friction](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Exercise%202.4%3A%20Pockets%20of%20friction)
  - [Exercise 2.5: Fluid resistance with different heights](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Exercise%202.5%3A%20Fluid%20resistance%20with%20different%20heights)
  - [Exercise 2.6: Falling boxes](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Exercise%202.6%3A%20Falling%20boxes)
  - [Exercise 2.7: Multiple attractors](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Exercise%202.7%3A%20Multiple%20attractors)
  - [Exercise 2.9: Custom force](https://github.com/mark-gerarts/nature-of-code/tree/master/02.%20Forces/Exercise%202.9%3A%20Custom%20force)
- [03. Oscillation](https://github.com/mark-gerarts/nature-of-code/tree/master/03.%20Oscillation)
  - [Example 3.1: Angular motion](https://github.com/mark-gerarts/nature-of-code/tree/master/03.%20Oscillation/Example%203.1%3A%20Angular%20motion)
  - [Example 3.2: Forces with (arbitrary) angular motion](https://github.com/mark-gerarts/nature-of-code/tree/master/03.%20Oscillation/Example%203.2%3A%20Forces%20with%20%28arbitrary%29%20angular%20motion)
  - [Exercise 3.1: Rotation](https://github.com/mark-gerarts/nature-of-code/tree/master/03.%20Oscillation/Exercise%203.1%3A%20Rotation)
- [08. Fractals](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals)
  - [Example 8.1: Recursive circles I](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Example%208.1%3A%20Recursive%20circles%20I)
  - [Example 8.2: Recursion twice](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Example%208.2%3A%20Recursion%20twice)
  - [Example 8.3: Recursion four times](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Example%208.3%3A%20Recursion%20four%20times)
  - [Example 8.4: Cantor set](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Example%208.4%3A%20Cantor%20set)
  - [Example 8.5: Koch curve](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Example%208.5%3A%20Koch%20curve)
  - [Example 8.6: Recursive tree](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Example%208.6%3A%20Recursive%20tree)
  - [Example 8.7: Dynamic recursive tree](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Example%208.7%3A%20Dynamic%20recursive%20tree)
  - [Exercise 8.1: Custom recursion](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Exercise%208.1%3A%20Custom%20recursion)
  - [Exercise 8.2: Koch snowflake](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Exercise%208.2%3A%20Koch%20snowflake)
  - [Exercise 8.7: Recursive thinning tree](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Exercise%208.7%3A%20Recursive%20thinning%20tree)
  - [Exercise 8.8: List tree](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Exercise%208.8%3A%20List%20tree)
  - [Exercise 8.9: Growing tree](https://github.com/mark-gerarts/nature-of-code/tree/master/08.%20Fractals/Exercise%208.9%3A%20Growing%20tree)

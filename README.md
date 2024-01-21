solver for 30 cubes puzzle

NB this doesn't attempt to fit the "lines" into the boxes, just which cube should
be joined to which others. Putting the cubes into the boxes is relatively trivial
to do by hand

On my laptop (Intel® Core™ i5-10300H CPU @ 2.50GHz) is solves the moderate problems
(specific number of each background colour) in about 15ms.

I haven't made it user friendly at all. If you want to solve a different type of
problem you have to change the source code and recompile. There's also a lot of
unused stuff from previous methods of solving the problem.
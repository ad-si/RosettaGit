+++
title = "Talk:Mandelbrot set"
description = ""
date = 2011-11-26T10:59:47Z
aliases = []
[extra]
id = 6636
[taxonomies]
categories = []
tags = []
+++

==Gnuplot==
Like the picture! --[[User:Paddy3118|Paddy3118]] 10:42, 23 March 2010 (UTC)

== Better description ==

I think we could use a better description here.  I'm no mathemetician, but it looks like most of the Mandlebrot images we see (including most of the code attached to this task page) are stackings of the point sets each iteration. (Makes me wish I had a voxel view) Does that sound correct? --[[User:Short Circuit|Michael Mol]] 20:19, 14 August 2010 (UTC)

: I don't understand your description, but what usually determines the color is the number of iterations until the point leaves the circle of radius 2 (one can prove that if it ever leaves that circle, it ultimately goes to infinity; of course the reverse holds, too).
: The simplest way to draw the set (and the only one which would fulfill a strict definition of "drawing the set") would only use two colors: One color for the points in the set (usually, black is used for those points), and another for points outside the set (the natural choice would be white). Of course, what you can draw is only an approximation of the set (because you only do a finite number of iterations; to actually decide if a point is in the Mandelbrot set you'd need an infinite number of iterations, which isn't possible, of course).
: However, the algorithm gives you some more information: It not only tells you whether the point ever left the circle, but it also tells you <em>when</em> it left the circle. And since colored pictures look more interesting than black/white ones, usually that information is used to color the image. --[[User:Ce|Ce]] 21:02, 14 August 2010 (UTC)

== More complex version of JavaScript code ==

I've improved the JavaScript code to provide several choices of coloring (standard [the one I've already put on RosettaCode], smooth, relief and none [i.e.just the black set on white background]), a possibility to keep the aspect ratio (i.e. if you give a rectangle which doesn't have the same aspect ratio as the canvas, one dimension is extended to give the correct aspect ratio again), and tests for the main cardioid and the first bulb to improve calculation speed. However that complicates the code (e.g. due to the relief code, I have to go through the image in diagonals). Now I see several possibilities:
* Just leave the page as it is and don't put the advanced code here (maybe put it online elsewhere).
* Replace the current code with the new one.
* Add the new code after the current one.
* Make a new page for the additional implementation, put the extended code there, and link to that page after the existing code.
* Make a new page, and put both the existing code and the new version there.
Which option do you think I should choose? --[[User:Ce|Ce]] 08:15, 22 August 2010 (UTC)

== making C version work ==

What assumptions need to be filled to make the C version work?

I tried building it under cygwin, with the glut runtime and dev installed, using


```bash
CFLAGS='-L/usr/lib/w32api -lgl -lGLU32 -lopengl32 -lglut -lm' make mandelbrot
```


And that failed for me with lots of failures to link to various entry points which seem to be defined in /usr/lib/libglut.a

It's been years since I have mucked about with makefiles and linker options, and knowing what is supposed to be happening here might help.  --[[User:Rdm|Rdm]] 12:59, 28 July 2011 (UTC)
: Does the linker fail to find libglut or does it fail to find function that libglut calls?  On my system the program needs libGL.so, libGLU.so, libglut.so and libm.a, all of which should be portable.  Can you ldd those? --[[User:Ledrug|Ledrug]] 21:11, 28 July 2011 (UTC)

:: Try putting your flags in the correct order. The -l flags must be after the .c file, and libraries must go in order with -lglut before its dependencies. (ELF shared libraries might already know about their dependencies, but other kinds of libraries might not.) Try this:

:: 
```bash
make mandelbrot LDLIBS='-L/usr/lib/w32api -lglut -lGLU32 -lopengl32 -lgl -lm'
```


:: If you have missing entry points, please post their names; then we might know which library is missing. If anyone has no err.h, try deleting the <code>#include <err.h></code> line; I deleted it from this wiki page. --[[User:Kernigh|Kernigh]] 22:08, 28 July 2011 (UTC)

:::Using LDLIBS rather than CFLAGS worked.  Thanks.  --[[User:Rdm|Rdm]] 13:07, 29 July 2011 (UTC)

== Task description !!!! ==

Hi. Some programs make ASCII image on the screen, some make colur image to bmp, some to ppm file. I think that it makes comparisen very difficult ( ? impossible ). Regards --[[User:Adam majewski|Adam majewski]] 07:47, 26 November 2011 (UTC)

== Haskell ==

Hi. Haskell version works, but what means "\" in line 

 mandelbrot a = iterate (\z -> z^2 + a) 0 !! 50

????? Regards. --[[User:Adam majewski|Adam majewski]] 07:53, 26 November 2011 (UTC)

:<code>\</code> is how you write an anonymous function in Haskell (also known as lambda in some other functional languages). The symbol <code>\</code> is supposed to resemble the letter λ (lambda), which is used to write anonymous functions in lambda calculus. --[[User:Spoon!|Spoon!]] 10:59, 26 November 2011 (UTC)

+++
title = "Talk:Fractal tree"
description = ""
date = 2012-08-19T06:49:58Z
aliases = []
[extra]
id = 7179
[taxonomies]
categories = []
tags = []
+++

==Why a cairo version of the C code==

I wanted just to try it but without installing SGE, and the reason why I do not want to install SGE is that the project seems to me not well maintained. The Freshmeat.net page has a broken link for the main website. More over, I've downloaded the source from what it seems the main page now (found using google), [http://www.digitalfanatics.org/cal/sge/index.html this one], to check if the PI constant was defined into sge.h, and surprise, it is defined there, but as 3.1414(more correct digits).... The source tarball was <tt>sge030809.tar.gz</tt> just in case one wants to check it too.

So I decided to write the needed cairo code to see it... in the same time, I wanted to preserve the original code, maybe there are others using SGE happily... So defining a constant WITH_CAIRO, the compilation uses the other code. Personally I've compiled it with
<tt>gcc -DWITH_CAIRO -I /usr/include/cairo -lSDL -lcairo fractree.c</tt>. --[[User:ShinTakezou|ShinTakezou]] 16:07, 5 May 2010 (UTC)

== Random Perturbations? ==

Are we to add random variation to the tree or not? Two of the answers have it (including one I wrote) and the third does not. It would help if there was some consistency in what we are asked to do... –[[User:Dkf|Donal Fellows]] 09:40, 6 May 2010 (UTC)

:It seems not a task requirement, I've added something about perturbation in PostScript but ignored it in POV-Ray code (currently at least) &mdash;[[User:ShinTakezou|ShinTakezou]] 08:06, 14 May 2010 (UTC)

== J Explanation ==

The constants in the J program were loosely based on the C implementation.  However, using randomness in the line segment lengths would have added complication (maybe a line of code) so I left that out.

Most implementations of this algorithm will probably use a depth first or breadth first tree traversal algorithm.  However, I felt that in J this would have a significant complexity and performance penalty, so instead I went with a parallel approach: for each leaf node I would compute a polyline which reaches from the root to that leaf.  Most of the work for each polyline could then be computed in parallel with all of the others.

Note that this introduces a factor of 2 inefficiency at some parts of the calculations, but in return I gain considerable simplicity and an orthogonal architecture.  (And, in J, simplicity pays off big, in terms of performance, and the clean architecture means I am not constrained to depth first or breadth first algorithms.)

Note that with 14 branches we have 15 line segments in each polyline and 16 points in each path.  We also have 2^14 distinct polylines.  For the purpose of illustration, however, we should work with a smaller dataset:


```j
L0=: 50           NB. initial length
A0=: 1r8p1        NB. initial angle: pi divided by 8
dL=: 0.9          NB. shrink factor for length
dA=: 0.75         NB. shrink factor for angle
N=: 4
```
   

Without random variations, every polyline uses the same lengths for its line segments.  I attenuate the very first segment here because I will be taking on another segment (leading from the root) later:


```j
   L=: L0*dL^1+i.N
   L
45 40.5 36.45 32.805
```


We also need to describe the rotations to be used at each branch point.  I use positive and negative rotations at each branch, compute their running sums, attenuate them, and scale them:


```j
   _1 ^ #:i.2 ^ N
 1  1  1  1
 1  1  1 _1
 1  1 _1  1
 1  1 _1 _1
 1 _1  1  1
...
```


Note also that I use radians rather than degrees, to keep my code simple:


```j
   A=: A0*(dA^i.N) +/\@:*("1) _1 ^ #:i.2 ^ N
   A
 0.392699   0.687223  0.908117    1.07379
 0.392699   0.687223  0.908117   0.742447
 0.392699   0.687223   0.46633      0.632
 0.392699   0.687223   0.46633    0.30066
 0.392699  0.0981748  0.319068   0.484738
...
```


Finally, I glue the lengths and angles together, and on each polyline I paste (L0,0) on the front (which will be the line segment from the root to the first branch point, convert them as polar coordinates to complex numbers and then convert as rectangular coordinates back to real numbers, and then paste (0 0) on the front (which will be the coordinate of the root node) and compute each polyline's running sum:


```j
   P=: 0 0+/\@,"2 +.*.inv (L0,0),"2 L,"0"1 A
   P
      0        0
     50        0
91.5746  17.2208
122.882  42.9137
145.307  71.6489
160.948  100.485

      0        0
     50        0
91.5746  17.2208
122.882  42.9137
145.307  71.6489
169.478  93.8281

      0        0
     50        0
91.5746  17.2208
122.882  42.9137
...
```


So that takes care of the data, now we need to render it.

First off, I need some kind of graphics support:


```j
   require'gl2'
```


To render this, I create a form with a single graphical canvas.


```j
wd 0 :0
 pc P closeok;
 xywh 0 0 250 300;
 cc C isigraph rightmove bottommove;
 pas 0 0;
 pshow;
)
```


Briefly, the form parent is named P and includes built-in support for closing the form.  I set the dimensions for the control to be 250 units by 300 units (where each unit is 2 pixels for some arcane reason), and anchored in the upper left of the form.  The graphic control is named C and when the form is resized its right and bottom edges will follow the form's right and bottoms edges.  pas 0 0 sizes the form to contain the current set of controls, with no extra padding.  Finally, we show the form.

Of course, if I did only this, the display would be blank.  I needed to have set up an paint event handler for that canvas:


```j
P_C_paint=: gllines_jgl2_ bind (10 + ,/"2 P-"1<./,/P)
```


gllines_jgl2_ wants a single list of numbers to describe a polypath, with x and y values alternating.  And the numbers should be positive.  So I subtract the minimum value from all polypaths from P 


```j
   <./,/P
0 _100.485
```


This makes all values in P positive.  For good measure, I add another 10 pixels to keep the tree off the top and left edge of the form.

Now, every time the form needs to be refreshed (which includes when it is first rendered), the tree gets drawn.

Going further (randomness):  If I wanted to introduce randomness, L would need to have one row for every row of A.  I believe I would first compute L's random values in parallel with a different random value for each value in A.  Then, I would partition each column based on the number of repeats for that segment and I would pick an arbitrary value from within the partition.  Finally, I would compute running products along the rows to match the C implementation's approach.

Going further (fat root):  In some implementations, the root segment was "fat" and the segments gradually got thinner as we approached the leaves.  I could implement that here by adding a constant offset to each point's y coordinate -- as long as the offset was less than the thickness of a line (and it really should be, for aesthetic reasons) we would get this "fat root" appearance.

Going further (changing orientation):  To rotate the tree by angle a0, I would use a0+A where I currently use A, and I would use (L0,a0) where I currently use (L0,0).

Going further (combining the above):  To do all of these additional exercises, I might want to refactor the code slightly to make things easier.  For example, I could do the "fat line" thing and then rotate all polylines by angle a0.

==Python code==

Are changes to the Python versions not allowed or strongly discouraged?
:Edits to all examples are encouraged as long as they are correct. If they are drastically different it may be more beneficial for the community to add additional examples for the same language. --[[User:Mwn3d|Mwn3d]] 02:46, 5 March 2011 (UTC)

:Yep. Nothings sacred as such, but we encourage ''improvements'' :-)
:--[[User:Paddy3118|Paddy3118]] 07:08, 5 March 2011 (UTC)

== Task ==

Hi, 

There are some differences between solutions :
* different angles and levels ( depth )
* some programs draw on the screen, some to different raster ( ppm, bmp, png) or vector ( svg) graphic files 

Is it possible to add :
* statistic about each program ( number of lines and words of code, maybe time of computing on the same computer , ...)
* description about each program : what technique and  data types it uses,

Regards. --[[User:Adam majewski|Adam majewski]] 05:45, 19 August 2012 (UTC)

:On RC we tend not to focus on timings as they can be hard to reproduce as they may depend on so much to describe the running environment. That doesn't stop the reader from performing his own timings however. (Although even then we tend to limit the amount of timing information or relegate it to the talk page unless it is a general indicator between timings in the same language of two different implementations and be expressed in general terms such as "X orders of magnitude for this test data"). If one can analyse the Big-O notation of example algorithms then I think that probably would be welcomed. 

:Some tasks specify the exact algorithm to use - a few might also specify what datastructure also. Apart from that, you would normally have to read the code to work it out, although their are cases where novel algorithms get copied. --[[User:Paddy3118|Paddy3118]] 06:49, 19 August 2012 (UTC)

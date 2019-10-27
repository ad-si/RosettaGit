+++
title = "Talk:Total circles area"
description = ""
date = 2012-10-15T08:56:06Z
aliases = []
[extra]
id = 12311
[taxonomies]
categories = []
tags = []
+++

== Monte Carlo ==

A semi-important point and semi-nitpicking for doing Monte Carlo: you need to estimate the error in a credible way given the number of samples, and should ''not'' output more significant digits than the confidence allows.  The current C code sample size of 100M can't possibly allow 8 decimal point precision.  --[[User:Ledrug|Ledrug]] 04:29, 16 September 2012 (UTC)
: Right, in the C/Python outputs only the first few digits are correct. Feel free to add a bit of error estimation code.
:: This is basically what I used on reddit, but I don't feel like changing the example on the task page because MC really isn't the right tool for this task.
:: 
```c>#include <stdio.h

#include <stdlib.h>
#include <tgmath.h>
#include <time.h>

typedef float flt;
flt data[][3] = {
	{ 1.6417233788,  1.6121789534, 0.0848270516},
	...
};
 
#define N sizeof(data)/sizeof(data[0])

int main(void)
{
	flt x0 = 1/0., x1 = -1/0., y0 = 1/0., y1 = -1/0.;

	inline flt min(flt x, flt y) { return x < y ? x : y; }
	inline flt max(flt x, flt y) { return x > y ? x : y; }
	inline flt sq(flt x) { return x * x; }
	inline flt rndf(flt x, flt d) { return x + d * rand() / RAND_MAX; }

	inline int covered(flt x, flt y) {
		for (int i = 0; i < N; i++)
			if (sq(x - data[i][0]) + sq(y - data[i][1]) < data[i][2])
				return 1;
		return 0;
	}

	for (int i = 0; i < N; i++) {
		flt *p = data[i];
		x0 = min(x0, p[0] - p[2]);
		x1 = max(x1, p[0] + p[2]);
		y0 = min(y0, p[1] - p[2]);
		y1 = max(y1, p[1] + p[2]);

		p[2] *= p[2];
	}

	x1 -= x0, y1 -= y0;

	flt total = x1 * y1;
	unsigned long to_try = 0x10000, tries = 0, hit = 0;
	srand(time(0));

	while (1) {
		hit += covered(rndf(x0, x1), rndf(y0, y1));

		if (++tries == to_try) {
			flt area = total * hit / tries;
			flt r = (flt) hit / tries;
			flt s = area * sqrt(r * (1 - r) / tries);
			printf("%.4f +/- %.4f (%lu samples)\n", area, s, tries);
			// stop at 3 sigmas
			if (s * 3 <= 1e-3) break;
			to_try *= 2;
		}
	}
	return 0;
}
```
 --[[User:Ledrug|Ledrug]] 22:09, 16 September 2012 (UTC)

::: A bad solution algorithm is quite useful for didactic purposes (but I don't know how much Rosettacode cares for didactic).

== Haskell analytical solution ==
I like the analytical solution in Haskell, but I think a tuples soup harms readability. So do you mind if I replace it with this code that introduces the Vec, Circle and Arc types? http://ideone.com/JjD4x [[User:Bearophile|bearophile]] 12:11, 21 September 2012 (UTC)

: Go ahead.  I'm a complete newbie in Haskell, and have no problem with people improving it. --[[User:Ledrug|Ledrug]] 15:04, 21 September 2012 (UTC)

: Actually there is one thing that's not quite right: in an arc ((x,y,r), (a0,a1)) the (a0,a1) is not a vector, they are a pair of angles of the start and end points on the original circle.  It doesn't hurt the computation, but as readability goes, nameing them "Vec" is misleading. --[[User:Ledrug|Ledrug]] 18:26, 21 September 2012 (UTC)

:: Right, I am sorry. In a (Haskell) program types contain lot of information (like telling apart two tuples that contain two doubles). For a second programmer that tries to add more of such information it's easy to miss part of the semantics and introduce wrong type information. I will try to introduce a Angle2 type and fix the code. I am an Haskell newbie myself, but we are learning. --[[User:Bearophile|bearophile]] 19:36, 21 September 2012 (UTC)
:: Done. Please take a look at the code, to make sure I have not used an Angle where instead goes a normal Double, and I have not used an Angle2 where a Vet instead goes.

::: I think it's fine.  I changed the polyline area function, maybe it's clearer this way.  Also added some comments explaining some odd stuff I did in the code.  One thing sort of amuses me is, is there really a need for an <code>Angle</code> type?  I mean, it does no harm, but angles are really just plain numbers and doesn't do anything to distinguish itself from a <code>Double</code>. --[[User:Ledrug|Ledrug]] 06:11, 22 September 2012 (UTC)

:::: Your question is almost as interesting as the circles area Task itself.

:::: Unless you go to extremes, in such situations there is no wrong/correct solutions, just better/worse ones. It's often a judgement call. And judgement sometimes requires having experience in the (Haskell) language.

:::: I am learning Haskell so I have tried to see how well it manages subtypes of simple values. The answer is: mixed.

:::: It's easy to remove the Angle newtype from this program, if not desired.

:::: The program is a little more complex than before, as you see I have had to unpack the angle argument of arcPoint, I have defined a Pi angle, I have had to use a language extension (GeneralizedNewtypeDeriving) to do it simply, and so on.

:::: Functional languages like Haskell recognize and show the value of giving different types to different entities. In F# you even add units of measures (http://msdn.microsoft.com/en-us/library/dd233243.aspx ) to variables (many other languages have libraries for it, but I think they are rarely used in small programs like this). The idea of encoding units of measure or the meaning of values like Angle in the type system is to help avoid mistakes like mixing things that must not mix.

:::: In this program Angle has not spot bugs, but I think it helps self-document a bit better this sparsely comment code. One advantage of using types over comments is that the (Haskell) compiler enforces their correct usage. As example, the type of the aNorm function is "Angle -> Angle". Now it's easy to understand that "a" in its name refers to angle, and "Norm" of the function name means putting the angle value back in a simple range of values, no comments on this function are required now.

:::: In this program there are other unspecified Doubles (beside the input data itself), the return types of arcArea, pathArea, polylineArea and circlesArea. All of them are the same type, of areas of surface. This means that if we use units of measures system like in F# code, such Doubles need to be products of two distances. Currently this is not encoded in the types of this program.

:::: If you want and you have some time, I suggest you to add an image (like the ones in the Stack Overflow thread) to the Haskell code to help show how your program works, using only on four or five of the input circles for visual simplicity. --[[User:Bearophile|bearophile]] 11:57, 22 September 2012 (UTC)

==Link==
* [http://stackoverflow.com/a/1667789/10562 Stack Overflow. Nice diagram and comments].

==Python Van der Corput==
Is this solution converging faster or slower than the simpler grid or totally random sampling strategies?
: Lets see, 
:* The Python grid uses 500*500  = 250000 points to get a value of 21.561559772.
:* After 200000 points, the Van der Corput has a value of 21.565708203389384
:* The given analytical solution is 21.565036603856395
:It looks as if the VdC does more with its 200K points than the grid with 250K, but I like the fact that the VdC generates a finer and finer grid as you take more points unlike the fixed box grid. (Plus I had stowed away the fact that VdC was used for Monte Carlo sims and wanted to try it out). --[[User:Paddy3118|Paddy3118]] 15:07, 22 September 2012 (UTC)
::In contrast, the C coded Monte Carlo method takes 100 million points to compute 21.56262288. --[[User:Paddy3118|Paddy3118]] 15:15, 22 September 2012 (UTC)
::With 100 million points, the Python VdC area estimate is 21.565056432232886. --[[User:Paddy3118|Paddy3118]] 15:24, 22 September 2012 (UTC)
::: Maybe you want to write part of such information inside the page, instead of just here.

::: The comparisons are incorrect.  Here are two modified examples: 
```python
from __future__ import division
from math import sqrt, atan2
from itertools import count
from pprint import pprint as pp
try:
    from itertools import izip as zip
except:
    pass

# Remove duplicates and sort, largest first.
circles = sorted(set([(0.001,0.523,1)]))

def vdcgen(base=2):
    'Van der Corput sequence generator'
    for n in count():
        vdc, denom = 0,1
        while n:
            denom *= base
            n, remainder = divmod(n, base)
            vdc += remainder / denom
        yield vdc

def vdc_2d():
    'Two dimensional Van der Corput sequence generator'
    for x, y in zip(vdcgen(base=2), vdcgen(base=3)):
        yield x, y

def bounding_box(circles):
    'Return minx, maxx, miny, maxy'
    return (min(x - r for x,y,r in circles),
            max(x + r for x,y,r in circles),
            min(y - r for x,y,r in circles),
            max(y + r for x,y,r in circles)
           )
def circle_is_in_circle(c1, c2):
    x1, y1, r1 = c1
    x2, y2, r2 = c2
    return (sqrt((x2 - x1)**2 + (y2 - y1)**2) + r2) <= r1

def remove_covered_circles(circles):
    'Takes circles in decreasing radius order. Removes those covered by others'
    i, covered = 0, []
    while i < len(circles):
        c1 = circles[i]
        eliminate = []
        for c2 in circles[i+1:]:
            if circle_is_in_circle(c1, c2):
                eliminate.append(c2)
        if eliminate: covered += [c1, eliminate]
        for c in eliminate: circles.remove(c)
        i += 1
    #pp(covered)   

def main(circles):
    print('Originally %i circles' % len(circles))
    print('Bounding box: %r' % (bounding_box(circles),))
    remove_covered_circles(circles)
    print('  down to %i  due to some being wholly covered by others' % len(circles))
    minx, maxx, miny, maxy = bounding_box(circles)
    # Shift to 0,0 and compute r**2 once
    circles2 = [(x - minx, y - miny, r*r) for x, y, r in circles]
    scalex, scaley = abs(maxx - minx), abs(maxy - miny)
    pcount, inside, last = 0, 0, ''
    for px, py in vdc_2d():
        pcount += 1
        px *= scalex; py *= scaley
        for cx, cy, cr2 in circles2:
            if (px-cx)**2 + (py-cy)**2 <= cr2:
                inside += 1
                break
        if not pcount % 100000:
            area = (inside/pcount) * scalex * scaley
            print('Points: %8i, Area estimate: %r'
                  % (pcount, area))
            # Hack to check if precision OK
            this = '%.4f' % area
            if this == last:
                break
            else:
                last = this
    print('The value has settled to %s' % this)
    print('error = %e' % (area - 4 * atan2(1,1)))

if __name__ == '__main__':
    main(circles)
```


::: 
```python
from collections import namedtuple
from math import floor, atan2

Circle = namedtuple("Circle", "x y r")

circles = [ Circle(0.001, 0.523, 1) ]
def main():
    # compute the bounding box of the circles
    x_min = min(c.x - c.r for c in circles)
    x_max = max(c.x + c.r for c in circles)
    y_min = min(c.y - c.r for c in circles)
    y_max = max(c.y + c.r for c in circles)

    box_side = 512

    dx = (x_max - x_min) / float(box_side)
    dy = (y_max - y_min) / float(box_side)

    y_min = floor(y_min / dy) * dy
    x_min = floor(x_min / dx) * dx

    count = 0

    for r in xrange(box_side + 2):
        y = y_min + r * dy
        for c in xrange(box_side + 2):
            x = x_min + c * dx
            for circle in circles:
                if (x-circle.x)**2 + (y-circle.y)**2 <= (circle.r ** 2):
                    count += 1
                    break

    print "Approximated area:", count * dx * dy
    print "error = %e" % (count * dx * dy - 4 * atan2(1,1))

main()
```

::: Both examples have exactly one unit circle, and the regular method's error is about 8 times smaller and runs 10 times faster.  Both being more or less uniform sampling, there's no inherent reason one should be more accurate than the other, but regular sampling is just going to be faster due to simplicity.  Unless you can provide good reasoning or meaningful error estimate, it's very inapproriate to claim some method is better at it than others. --[[User:Ledrug|Ledrug]] 08:30, 15 October 2012 (UTC)

::Hi Ledrug, are you saying that the results quoted may be correct for the programs as given on the task page, but are either:
::* Not due to the use Van der Corput?
::* Or peculiar to the example circles chosen?
I must admit that I I only trawled the results stated for the other programs and did not do any extensive reruns with different circles as you have done. I just extracted accuracy vs points. --[[User:Paddy3118|Paddy3118]] 08:56, 15 October 2012 (UTC)

== C89 vs C99 ==

I have a suggestion: generally, don't convert C89 to C99 code.  Compatibility aside, GCC sometimes does weird things in C99 mode.  On my machine, for the second C entry, the current C99 code runtime is about twice as long as that of the old C89 (GCC 4.7, both with <code>-O -msse -lm</code>), even though they really shouldn't differ.  ICC (intel) doesn't have this problem, so it's definintely GCC, but still, GCC is the more available compiler out there. --[[User:Ledrug|Ledrug]] 20:36, 24 September 2012 (UTC)

: I prefer to use C99 where possible because it offers VLAs, better struct initializers, some nice standard libraries, and localized variable definitions that allow for more readable, safer and nicer looking C code. I try to show that just because it's C code it doesn't need to look ugly, unreadable and messy. And C11 is coming.

: Sometimes I too noticed small performance reductions with GCC using C99 instead of C89, but I have never seen a 2X runtime increase, and even in this case I can't reproduce the magnitude of your speed decrease. On my system using GCC to compile this nearly-GNU-C89 version is only about 2% faster than the GNU-C99 you see on the page (gcc 4.7.1, -Ofast -s -flto): http://ideone.com/9xihK  A further very small performance increase comes from using malloc+free instead of the variable length array (that are not present in C89), maybe because the heap memory has a bigger alignment. Maybe the performance decrease you are seeing doesn't come from C99 alone, but from other very small changes I have put in the code.

: Generally I think the quality of the code is more important than performance, especially if the performance is already enough and the performance difference is small. The small increase of language guarantees given by C99 (like not leaving to the implementation the definition of %) should not decrease performance. On the other hand C99 allows to reduce the scope of variables because it allows to define loop variables in place and to define variables where they are initialized and used; this should help increase code performance (and this is what happens if I am using the DMD D compiler).

: If the 2X speed decrease you are seeing is real (and I can't reproduce it), I suggest you to create a bug report for GCC devs, to help them fix the situation in the right place, inside GCC, instead of inside my code. --[[User:Bearophile|bearophile]] 12:13, 25 September 2012 (UTC)

==Excessive digits==
Maybe it's worth explaining after the Task description how the many digits of the reference answer were computed. Have you used this http://hackage.haskell.org/package/hmpfr with the Haskell analytical solution? --[[User:Bearophile|bearophile]] 11:22, 1 October 2012 (UTC)
: No, it was calculated using the same method as Haskell, but in C with MPFR, at 2000 bits precision.  I only did it as an exercise to know MPFR, and the code was too messy to post here -- I got too bored halfway and stopped keeping track of memory allocation, so there are hundreds of lost memory chunks and valgrind was extremely grumpy about it, but I do believe all the digits I posted are accurate. --[[User:Ledrug|Ledrug]] 20:06, 1 October 2012 (UTC)
:: If you want to create a D version of the analytical solution (using regular double or real floating point values), I will fix your code and help you with the D idioms. D has a garbage collector, so manual tracking of memory allocation/deallocation is usually not needed. Otherwise I'll eventually try to translate your Haskell code myself to D. --[[User:Bearophile|bearophile]] 23:07, 14 October 2012 (UTC)

I don't know D well enough to write code in it.  You are welcome to write it yourself, I'll just describe the algorithm in English in case there's any confusion about it:

# Given a bunch of circles, give all of them the same winding. Say, imagine every circle turns clockwise.
# Find all the intersection points among all circles.  Between each pair, there may be 0, 1 or 2 intersections.  Ignore 0 and 1, we need only the 2 case.
# For each circle, sort its intersection points in clockwise order (keep in mind it's a cyclic list), and split it into arc segments between neighboring point pairs.  Circles that don't cross other circles are treated as a single 360 degree arc.
# Imagine all circles are on a white piece of paper, and have their interiors inked black.  We are only interested in the arcs separating black and white areas, so we get rid of the arcs that aren't so.  These are the arcs that lie entirely within another circle.  Because we've taken care of all intersections eariler, we only need to check if any point on an arc segment is in any other circle; if so, remove this arc.  The haskell code uses the middle point of an arc for this, but anything other than the two end points would do.
# The remaining arcs form one or more closed paths.  Each path's area can be calculated, and the sum of them all is the area needed.  Each path is done like the way mentioned on that stackexchange page cited somewhere.  One thing that page didn't mention is that it works for holes, too.  Suppose the circles form an area with a hole in it; you'd end up with two paths, where the outer one winds clockwise, and the inner one ccw.  Use the same method to calculate the areas for both, and the outer one would have a positive area, the inner one negative.  Just add them up.

There are a few concerns about this algorithm: firstly, it's fast only if there are few circles.  Its complexity is maybe O(N^3) with N = number of circles, while normal scanline method is probably O(N * n) or less, with n = number of scanlines.  Secondly, step 4 needs to be accurate; a small precision error there may cause an arc to remain or be removed by mistake, with disastrous consequences.  Also, it's difficult to estimate the error in the final result.  The scanline or Monte Carlo methods have errors mostly due to statistics, while this method's error is due to floating point precision loss, which is a very different can of worms. --[[User:Ledrug|Ledrug]] 01:24, 15 October 2012 (UTC)

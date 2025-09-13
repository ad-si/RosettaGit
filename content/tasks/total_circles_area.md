+++
title = "Total circles area"
description = ""
date = 2019-05-17T03:47:14Z
aliases = []
[extra]
id = 12310
[taxonomies]
categories = ["task"]
tags = []
+++

[[File:total_circles_area_full.png|300px|thumb|right|Example circles]]
[[File:total_circles_area_filtered.png|300px|thumb|right|Example circles filtered]]

Given some partially overlapping circles on the plane, compute and show the total area covered by them, with four or six (or a little more) decimal digits of precision. The area covered by two or more disks needs to be counted only once.

One point of this Task is also to compare and discuss the relative merits of various solution strategies, their performance, precision and simplicity. This means keeping both slower and faster solutions for a language (like C) is welcome.

To allow a better comparison of the different implementations, solve the problem with this standard dataset, each line contains the '''x''' and '''y''' coordinates of the centers of the disks and their radii   (11 disks are fully contained inside other disks):

```txt

      xc             yc        radius
 1.6417233788  1.6121789534 0.0848270516
-1.4944608174  1.2077959613 1.1039549836
 0.6110294452 -0.6907087527 0.9089162485
 0.3844862411  0.2923344616 0.2375743054
-0.2495892950 -0.3832854473 1.0845181219
 1.7813504266  1.6178237031 0.8162655711
-0.1985249206 -0.8343333301 0.0538864941
-1.7011985145 -0.1263820964 0.4776976918
-0.4319462812  1.4104420482 0.7886291537
 0.2178372997 -0.9499557344 0.0357871187
-0.6294854565 -1.3078893852 0.7653357688
 1.7952608455  0.6281269104 0.2727652452
 1.4168575317  1.0683357171 1.1016025378
 1.4637371396  0.9463877418 1.1846214562
-0.5263668798  1.7315156631 1.4428514068
-1.2197352481  0.9144146579 1.0727263474
-0.1389358881  0.1092805780 0.7350208828
 1.5293954595  0.0030278255 1.2472867347
-0.5258728625  1.3782633069 1.3495508831
-0.1403562064  0.2437382535 1.3804956588
 0.8055826339 -0.0482092025 0.3327165165
-0.6311979224  0.7184578971 0.2491045282
 1.4685857879 -0.8347049536 1.3670667538
-0.6855727502  1.6465021616 1.0593087096
 0.0152957411  0.0638919221 0.9771215985

```


The result is 21.56503660... .


## Related tasks

*   [[Circles of given radius through two points]].


## See also

* http://www.reddit.com/r/dailyprogrammer/comments/zff9o/9062012_challenge_96_difficult_water_droplets/
* http://stackoverflow.com/a/1667789/10562





## C


### Montecarlo Sampling

This program uses a Montecarlo sampling. For this problem this is less efficient (converges more slowly) than a regular grid sampling, like in the Python entry.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <stdbool.h>

typedef double Fp;
typedef struct { Fp x, y, r; } Circle;

Circle circles[] = {
    { 1.6417233788,  1.6121789534, 0.0848270516},
    {-1.4944608174,  1.2077959613, 1.1039549836},
    { 0.6110294452, -0.6907087527, 0.9089162485},
    { 0.3844862411,  0.2923344616, 0.2375743054},
    {-0.2495892950, -0.3832854473, 1.0845181219},
    { 1.7813504266,  1.6178237031, 0.8162655711},
    {-0.1985249206, -0.8343333301, 0.0538864941},
    {-1.7011985145, -0.1263820964, 0.4776976918},
    {-0.4319462812,  1.4104420482, 0.7886291537},
    { 0.2178372997, -0.9499557344, 0.0357871187},
    {-0.6294854565, -1.3078893852, 0.7653357688},
    { 1.7952608455,  0.6281269104, 0.2727652452},
    { 1.4168575317,  1.0683357171, 1.1016025378},
    { 1.4637371396,  0.9463877418, 1.1846214562},
    {-0.5263668798,  1.7315156631, 1.4428514068},
    {-1.2197352481,  0.9144146579, 1.0727263474},
    {-0.1389358881,  0.1092805780, 0.7350208828},
    { 1.5293954595,  0.0030278255, 1.2472867347},
    {-0.5258728625,  1.3782633069, 1.3495508831},
    {-0.1403562064,  0.2437382535, 1.3804956588},
    { 0.8055826339, -0.0482092025, 0.3327165165},
    {-0.6311979224,  0.7184578971, 0.2491045282},
    { 1.4685857879, -0.8347049536, 1.3670667538},
    {-0.6855727502,  1.6465021616, 1.0593087096},
    { 0.0152957411,  0.0638919221, 0.9771215985}};

const size_t n_circles = sizeof(circles) / sizeof(Circle);

static inline Fp min(const Fp a, const Fp b) { return a <= b ? a : b; }

static inline Fp max(const Fp a, const Fp b) { return a >= b ? a : b; }

static inline Fp sq(const Fp a) { return a * a; }

// Return an uniform random value in [a, b).
static inline double uniform(const double a, const double b) {
    const double r01 = rand() / (double)RAND_MAX;
    return a + (b - a) * r01;
}

static inline bool is_inside_circles(const Fp x, const Fp y) {
    for (size_t i = 0; i < n_circles; i++)
        if (sq(x - circles[i].x) + sq(y - circles[i].y) < circles[i].r)
            return true;
    return false;
}

int main() {
    // Initialize the bounding box (bbox) of the circles.
    Fp x_min = INFINITY, x_max = -INFINITY;
    Fp y_min = x_min, y_max = x_max;

    // Compute the bounding box of the circles.
    for (size_t i = 0; i < n_circles; i++) {
        Circle *c = &circles[i];
        x_min = min(x_min, c->x - c->r);
        x_max = max(x_max, c->x + c->r);
        y_min = min(y_min, c->y - c->r);
        y_max = max(y_max, c->y + c->r);

        c->r *= c->r; // Square the radii to speed up testing.
    }

    const Fp bbox_area = (x_max - x_min) * (y_max - y_min);

    // Montecarlo sampling.
    srand(time(0));
    size_t to_try = 1U << 16;
    size_t n_tries = 0;
    size_t n_hits = 0;

    while (true) {
        n_hits += is_inside_circles(uniform(x_min, x_max),
                                    uniform(y_min, y_max));
        n_tries++;

        if (n_tries == to_try) {
            const Fp area = bbox_area * n_hits / n_tries;
            const Fp r = (Fp)n_hits / n_tries;
            const Fp s = area * sqrt(r * (1 - r) / n_tries);
            printf("%.4f +/- %.4f (%zd samples)\n", area, s, n_tries);
            if (s * 3 <= 1e-3) // Stop at 3 sigmas.
                break;
            to_try *= 2;
        }
    }

    return 0;
}
```

```txt
21.4498 +/- 0.0370 (65536 samples)
21.5031 +/- 0.0262 (131072 samples)
21.5170 +/- 0.0185 (262144 samples)
21.5442 +/- 0.0131 (524288 samples)
21.5477 +/- 0.0093 (1048576 samples)
21.5531 +/- 0.0065 (2097152 samples)
21.5624 +/- 0.0046 (4194304 samples)
21.5631 +/- 0.0033 (8388608 samples)
21.5602 +/- 0.0023 (16777216 samples)
21.5632 +/- 0.0016 (33554432 samples)
21.5617 +/- 0.0012 (67108864 samples)
21.5628 +/- 0.0008 (134217728 samples)
21.5639 +/- 0.0006 (268435456 samples)
21.5637 +/- 0.0004 (536870912 samples)
21.5637 +/- 0.0003 (1073741824 samples)
```



### Scanline Method

This version performs about 5 million scanlines in about a second, result should be accurate to maybe 10 decimal points.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

typedef double flt;
typedef struct {
	flt x, y, r, r2;
	flt y0, y1;	// extent of circle y+r and y-r
	flt x0, x1;	// where scanline intersects circle
} circle_t;
#define SZ sizeof(circle_t)

circle_t circles[] = {
	{ 1.6417233788,  1.6121789534, 0.0848270516},
#error data snipped for space; copy from previous C example
};

flt max(flt x, flt y) { return x < y ? y : x; }
flt min(flt x, flt y) { return x > y ? y : x; }
flt sq(flt x) { return x * x; }
flt cdist(circle_t *c1, circle_t *c2) {
	return sqrt(sq(c1->x - c2->x) + sq(c1->y - c2->y));
}

inline void swap_c(circle_t *c)
{
	circle_t tmp = c[0];
	c[0] = c[1], c[1] = tmp;
}

flt area(circle_t *circs, int n_circ, flt ymin, flt ymax, flt step)
{
	int i, n = n_circ;

	circle_t *c = malloc(SZ * n);
	memcpy(c, circs, SZ * n);

	while (n--)
		for (i = 0; i < n; i++)
			if (c[i].y1 < c[i+1].y1) swap_c(c + i);

	flt total = 0;

	int row = 1 + ceil((ymax - ymin) / step);
	while (row--) {
		flt y = ymin + step * row;
		for (n = 0; n < n_circ; n++)
			if (y >= c[n].y1) // rest of circles below scanline, ignore
				break;
			else if (y > c[n].y0) {
				flt dx = sqrt(c[n].r2 - sq(y - c[n].y));
				c[n].x0 = c[n].x - dx;
				c[n].x1 = c[n].x + dx;

				// keep circles sorted by left intersection
				for (i = n; i-- && c[i].x0 > c[i+1].x0; swap_c(c + i));

			} else {// remove a circle when scanline has passed it
				memmove(c + n, c + n + 1, SZ * (--n_circ - n));
				n--;
			}

		if (!n) continue;

		flt right = c->x1;
		total += c->x1 - c->x0;

		for (i = 1; i < n; i++) {
			if (c[i].x1 <= right) continue;
			total += c[i].x1 - max(c[i].x0, right);
			right = c[i].x1;
		}
	}

	free(c);
	return total * step;
}

int main(void)
{
	int n_circ = sizeof(circles) / SZ;
	flt ymin = INFINITY, ymax = -INFINITY;

	circle_t *c1, *c2;
	for (c1 = circles + n_circ; c1-- > circles; ) {
		for (c2 = circles + n_circ; c2-- > circles; )
			// throw out circles inside another circle
			if (c1 != c2 && cdist(c1, c2) + c1->r <= c2->r) {
				*c1 = circles[--n_circ];
				break;
			}
		ymin = min(ymin, c1->y0 = c1->y - c1->r);
		ymax = max(ymax, c1->y1 = c1->y + c1->r);
		c1->r2 = sq(c1->r);
	}

	flt s = 1. / (1 << 20);
	flt y0 = floor(ymin / s) * s;
	flt a = area(circles, n_circ, y0, ymax, s);
	int nlines = (ymax - y0) / s;

	// roughly, it cease to make sense if sqrt(nlines) * 1e-14 >> s * s
	printf("area = %.10f\tat %d scanlines\n", a, nlines);

	return 0;
}
```

 area = 21.5650366037    at 5637290 scanlines


## D

This version converges much faster than both the ordered grid and Montecarlo sampling solutions.

### Scanline Method

```d
import std.stdio, std.math, std.algorithm, std.typecons, std.range;

alias Fp = real;
struct Circle { Fp x, y, r; }

void removeInternalDisks(ref Circle[] circles) pure nothrow @safe {
    static bool isFullyInternal(in Circle c1, in Circle c2)
    pure nothrow @safe @nogc {
        if (c1.r > c2.r) // Quick exit.
            return false;
        return (c1.x - c2.x) ^^ 2 + (c1.y - c2.y) ^^ 2 <
               (c2.r - c1.r) ^^ 2;
    }

    // Heuristics for performance: large radii first.
    circles.sort!q{ a.r > b.r };

    // Remove circles inside another circle.
    for (auto i = circles.length; i-- > 0; )
        for (auto j = circles.length; j-- > 0; )
            if (i != j && isFullyInternal(circles[i], circles[j])) {
                circles[i] = circles[$ - 1];
                circles.length--;
                break;
            }
}

void main() {
    Circle[] circles = [
       { 1.6417233788,  1.6121789534, 0.0848270516},
       {-1.4944608174,  1.2077959613, 1.1039549836},
       { 0.6110294452, -0.6907087527, 0.9089162485},
       { 0.3844862411,  0.2923344616, 0.2375743054},
       {-0.2495892950, -0.3832854473, 1.0845181219},
       { 1.7813504266,  1.6178237031, 0.8162655711},
       {-0.1985249206, -0.8343333301, 0.0538864941},
       {-1.7011985145, -0.1263820964, 0.4776976918},
       {-0.4319462812,  1.4104420482, 0.7886291537},
       { 0.2178372997, -0.9499557344, 0.0357871187},
       {-0.6294854565, -1.3078893852, 0.7653357688},
       { 1.7952608455,  0.6281269104, 0.2727652452},
       { 1.4168575317,  1.0683357171, 1.1016025378},
       { 1.4637371396,  0.9463877418, 1.1846214562},
       {-0.5263668798,  1.7315156631, 1.4428514068},
       {-1.2197352481,  0.9144146579, 1.0727263474},
       {-0.1389358881,  0.1092805780, 0.7350208828},
       { 1.5293954595,  0.0030278255, 1.2472867347},
       {-0.5258728625,  1.3782633069, 1.3495508831},
       {-0.1403562064,  0.2437382535, 1.3804956588},
       { 0.8055826339, -0.0482092025, 0.3327165165},
       {-0.6311979224,  0.7184578971, 0.2491045282},
       { 1.4685857879, -0.8347049536, 1.3670667538},
       {-0.6855727502,  1.6465021616, 1.0593087096},
       { 0.0152957411,  0.0638919221, 0.9771215985}];

    writeln("Input Circles: ", circles.length);
    removeInternalDisks(circles);
    writeln("Circles left: ", circles.length);

    immutable Fp xMin = reduce!((acc, c) => min(acc, c.x - c.r))
                               (Fp.max, circles[]);
    immutable Fp xMax = reduce!((acc, c) => max(acc, c.x + c.r))
                               (Fp(0), circles[]);

    alias YRange = Tuple!(Fp,"y0", Fp,"y1");
    auto yRanges = new YRange[circles.length];

    Fp computeTotalArea(in Fp nSlicesX) nothrow @safe {
        Fp total = 0;

        // Adapted from an idea by Cosmologicon.
        foreach (immutable p; cast(int)(xMin * nSlicesX) ..
                              cast(int)(xMax * nSlicesX) + 1) {
            immutable Fp x = p / nSlicesX;
            size_t nPairs = 0;

            // Look for the circles intersecting the current
            // vertical secant:
            foreach (const ref c; circles) {
                immutable Fp d = c.r ^^ 2 - (c.x - x) ^^ 2;
                immutable Fp sd = d.sqrt;
                if (d > 0)
                    // And keep only the intersection chords.
                    yRanges[nPairs++] = YRange(c.y - sd, c.y + sd);
            }

            // Merge the ranges, counting the overlaps only once.
            yRanges[0 .. nPairs].sort();
            Fp y = -Fp.max;
            foreach (immutable r; yRanges[0 .. nPairs])
                if (y < r.y1) {
                    total += r.y1 - max(y, r.y0);
                    y = r.y1;
                }
        }

        return total / nSlicesX;
    }

    // Iterate to reach some precision.
    enum Fp epsilon = 1e-9;
    Fp nSlicesX = 1_000;
    Fp oldArea = -1;
    while (true) {
        immutable Fp newArea = computeTotalArea(nSlicesX);
        if (abs(oldArea - newArea) < epsilon) {
            writeln("N. vertical slices: ", nSlicesX);
            writefln("Approximate area: %.17f", newArea);
            return;
        }
        oldArea = newArea;
        nSlicesX *= 2;
    }
}
```

```txt
Input Circles: 25
Circles left: 14
N. vertical slices: 256000
Approximate area: 21.56503660593628004
```

Run-time is about 4.13 seconds with ldc2 compiler.


### Analytical Solution

This version is not fully idiomatic D, it retains some of the style of the Haskell version.

```d
import std.stdio, std.typecons, std.math, std.algorithm, std.range;

struct Vec { double x, y; }

alias VF = double function(in Vec, in Vec) pure nothrow @safe @nogc;
enum VF vCross = (v1, v2) => v1.x * v2.y - v1.y * v2.x;
enum VF vDot   = (v1, v2) => v1.x * v2.x + v1.y * v2.y;

alias VV = Vec function(in Vec, in Vec) pure nothrow @safe @nogc;
enum VV vAdd = (v1, v2) => Vec(v1.x + v2.x, v1.y + v2.y);
enum VV vSub = (v1, v2) => Vec(v1.x - v2.x, v1.y - v2.y);

enum vLen = (in Vec v) pure nothrow @safe @nogc => vDot(v, v).sqrt;
enum VF vDist = (a, b) => vSub(a, b).vLen;
enum vScale = (in double s, in Vec v) pure nothrow @safe @nogc =>
    Vec(v.x * s, v.y * s);
enum vNorm = (in Vec v) pure nothrow @safe @nogc =>
    Vec(v.x / v.vLen, v.y / v.vLen);

alias A = Typedef!double;

enum vAngle = (in Vec v) pure nothrow @safe @nogc => atan2(v.y, v.x).A;

A aNorm(in A a) pure nothrow @safe @nogc {
    if (a > PI)  return A(a - PI * 2.0);
    if (a < -PI) return A(a + PI * 2.0);
    return              a;
}

struct Circle { double x, y, r; }

A[] circleCross(in Circle c0, in Circle c1) pure nothrow {
    immutable d = vDist(Vec(c0.x, c0.y), Vec(c1.x, c1.y));
    if (d >= c0.r + c1.r || d <= abs(c0.r - c1.r))
        return [];

    immutable s = (c0.r + c1.r + d) / 2.0;
    immutable a = sqrt(s * (s - d) * (s - c0.r) * (s - c1.r));
    immutable h = 2.0 * a / d;
    immutable dr = Vec(c1.x - c0.x, c1.y - c0.y);
    immutable dx = vScale(sqrt(c0.r ^^ 2 - h ^^ 2), dr.vNorm);
    immutable ang = (c0.r ^^ 2 + d ^^ 2 > c1.r ^^ 2) ?
                    dr.vAngle :
                    A(PI + dr.vAngle);
    immutable da = asin(h / c0.r).A;
    return [A(ang - da), A(ang + da)].map!aNorm.array;
}

// Angles of the start and end points of the circle arc.
alias Angle2 = Tuple!(A,"a0", A,"a1");

alias Arc = Tuple!(Circle,"c", Angle2,"aa");

enum arcPoint = (in Circle c, in A a) pure nothrow @safe @nogc =>
    vAdd(Vec(c.x, c.y), Vec(c.r * cos(cast(double)a),
                            c.r * sin(cast(double)a)));

alias ArcF = Vec function(in Arc) pure nothrow @safe @nogc;
enum ArcF arcStart  = ar => arcPoint(ar.c, ar.aa.a0);
enum ArcF arcMid    = ar => arcPoint(ar.c, A((ar.aa.a0+ar.aa.a1) / 2));
enum ArcF arcEnd    = ar => arcPoint(ar.c, ar.aa.a1);
enum ArcF arcCenter = ar => Vec(ar.c.x, ar.c.y);

enum arcArea = (in Arc ar) pure nothrow @safe @nogc =>
    ar.c.r ^^ 2 * (ar.aa.a1 - ar.aa.a0) / 2.0;

Arc[] splitCircles(immutable Circle[] cs) pure /*nothrow*/ {
    static enum cSplit = (in Circle c, in A[] angs) pure nothrow =>
        c.repeat.zip(angs.zip(angs.dropOne).map!Angle2).map!Arc;

    // If an arc that was part of one circle is inside *another* circle,
    // it will not be part of the zero-winding path, so reject it.
    static bool inCircle(VC)(in VC vc, in Circle c) pure nothrow @nogc{
        return vc[1] != c && vDist(Vec(vc[0].x, vc[0].y),
                                   Vec(c.x, c.y)) < c.r;
    }

    enum inAnyCircle = (in Arc arc) nothrow @safe =>
        cs.map!(c => inCircle(tuple(arc.arcMid, arc.c), c)).any;

    auto f(in Circle c) pure nothrow {
        auto angs = cs.map!(c1 => circleCross(c, c1)).join;
        return tuple(c, ([A(-PI), A(PI)] ~ angs)
                        .sort().release);
    }

    return cs.map!f.map!(ca => cSplit(ca[])).join
           .filter!(ar => !inAnyCircle(ar)).array;
}


/** Given a list of arcs, build sets of closed paths from them. If
one arc's end point is no more than 1e-4 from another's start point,
they are considered connected.  Since these start/end points resulted
from intersecting circles earlier, they *should* be exactly the same,
but floating point precision may cause small differences, hence the
1e-4 error margin.  When there are genuinely different intersections
closer than this margin, the method will backfire, badly. */
const(Arc[])[] makePaths(in Arc[] arcs) pure nothrow @safe {
    static const(Arc[])[] joinArcs(in Arc[] a, in Arc[] xxs)
    pure nothrow {
        static enum eps = 1e-4;
        if (xxs.empty) return [a];
        immutable x = xxs[0];
        const xs = xxs.dropOne;
        if (a.empty) return joinArcs([x], xs);
        if (vDist(a[0].arcStart, a.back.arcEnd) < eps)
            return [a] ~ joinArcs([], xxs);
        if (vDist(a.back.arcEnd, x.arcStart) < eps)
            return joinArcs(a ~ [x], xs);
        return joinArcs(a, xs ~ [x]);
    }
    return joinArcs([], arcs);
}

// Slice N-polygon into N-2 triangles.
double polylineArea(in Vec[] vvs) pure nothrow {
    static enum triArea = (in Vec a, in Vec b, in Vec c)
        pure nothrow @nogc => vCross(vSub(b, a), vSub(c, b)) / 2.0;
    const vs = vvs.dropOne;
    immutable vvs0 = vvs[0];
    return zip(vs, vs.dropOne).map!(vv => triArea(vvs0, vv[])).sum;
}

double pathArea(in Arc[] arcs) pure nothrow {
    static f(in Tuple!(double, const(Vec)[]) ae, in Arc arc)
    pure nothrow {
        return tuple(ae[0] + arc.arcArea,
                     ae[1] ~ [arc.arcCenter, arc.arcEnd]);
    }
    const ae = reduce!f(tuple(0.0, (const(Vec)[]).init), arcs);
    return ae[0] + ae[1].polylineArea;
}

enum circlesArea = (immutable Circle[] cs) pure /*nothrow*/ =>
    cs.splitCircles.makePaths.map!pathArea.sum;


void main() {
    immutable circles = [
        Circle( 1.6417233788,  1.6121789534, 0.0848270516),
        Circle(-1.4944608174,  1.2077959613, 1.1039549836),
        Circle( 0.6110294452, -0.6907087527, 0.9089162485),
        Circle( 0.3844862411,  0.2923344616, 0.2375743054),
        Circle(-0.2495892950, -0.3832854473, 1.0845181219),
        Circle( 1.7813504266,  1.6178237031, 0.8162655711),
        Circle(-0.1985249206, -0.8343333301, 0.0538864941),
        Circle(-1.7011985145, -0.1263820964, 0.4776976918),
        Circle(-0.4319462812,  1.4104420482, 0.7886291537),
        Circle( 0.2178372997, -0.9499557344, 0.0357871187),
        Circle(-0.6294854565, -1.3078893852, 0.7653357688),
        Circle( 1.7952608455,  0.6281269104, 0.2727652452),
        Circle( 1.4168575317,  1.0683357171, 1.1016025378),
        Circle( 1.4637371396,  0.9463877418, 1.1846214562),
        Circle(-0.5263668798,  1.7315156631, 1.4428514068),
        Circle(-1.2197352481,  0.9144146579, 1.0727263474),
        Circle(-0.1389358881,  0.1092805780, 0.7350208828),
        Circle( 1.5293954595,  0.0030278255, 1.2472867347),
        Circle(-0.5258728625,  1.3782633069, 1.3495508831),
        Circle(-0.1403562064,  0.2437382535, 1.3804956588),
        Circle( 0.8055826339, -0.0482092025, 0.3327165165),
        Circle(-0.6311979224,  0.7184578971, 0.2491045282),
        Circle( 1.4685857879, -0.8347049536, 1.3670667538),
        Circle(-0.6855727502,  1.6465021616, 1.0593087096),
        Circle( 0.0152957411,  0.0638919221, 0.9771215985)];

    writefln("Area: %1.13f", circles.circlesArea);
}
```

```txt
Area: 21.5650366038564
```

The run-time is minimal (0.03 seconds or less).


## EchoLisp

Circles included in circles are discarded, and the circles are sorted : largest radius first. The circles bounding box is computed and divided into nxn rectangular tiles of size ds = dx * dy . Surfaces of tiles which are entirely included in a circle are added. Remaining tiles are divided into four smaller tiles, and the process is repeated : recursive call of procedure '''S''' , until ds < s-precision. To optimize things, a first pass  - procedure '''S0''' - is performed, which assigns a list of candidates intersecting circles to each tile. This is quite effective, since the mean number of candidates circles for a given tile is 1.008  for n = 800.


```scheme

(lib 'math)
(define (make-circle x0 y0 r)
    (vector x0 y0 r ))

(define-syntax-id _.radius (_ 2))
(define-syntax-id  _.x0  (_ 0))
(define-syntax-id  _.y0  (_ 1))

;; to sort circles
(define (cmp-circles a b) (> a.radius b.radius))

(define (included? circle: a circles)
    (for/or ((b circles))
        #:continue (equal? a b)
        (disk-in-disk? a b)))

;; eliminates, and sort
(define (sort-circles circles)
        (list-sort cmp-circles
          (filter (lambda(c) (not (included? c circles))) circles)))

(define circles (sort-circles
  (list (make-circle   1.6417233788  1.6121789534 0.0848270516)
        (make-circle  -1.4944608174  1.2077959613 1.1039549836)
        (make-circle   0.6110294452 -0.6907087527 0.9089162485)
        (make-circle   0.3844862411  0.2923344616 0.2375743054)
        (make-circle  -0.2495892950 -0.3832854473 1.0845181219)
        (make-circle   1.7813504266  1.6178237031 0.8162655711)
        (make-circle  -0.1985249206 -0.8343333301 0.0538864941)
        (make-circle  -1.7011985145 -0.1263820964 0.4776976918)
        (make-circle  -0.4319462812  1.4104420482 0.7886291537)
        (make-circle   0.2178372997 -0.9499557344 0.0357871187)
        (make-circle  -0.6294854565 -1.3078893852 0.7653357688)
        (make-circle   1.7952608455  0.6281269104 0.2727652452)
        (make-circle   1.4168575317  1.0683357171 1.1016025378)
        (make-circle   1.4637371396  0.9463877418 1.1846214562)
        (make-circle  -0.5263668798  1.7315156631 1.4428514068)
        (make-circle  -1.2197352481  0.9144146579 1.0727263474)
        (make-circle  -0.1389358881  0.1092805780 0.7350208828)
        (make-circle   1.5293954595  0.0030278255 1.2472867347)
        (make-circle  -0.5258728625  1.3782633069 1.3495508831)
        (make-circle  -0.1403562064  0.2437382535 1.3804956588)
        (make-circle   0.8055826339 -0.0482092025 0.3327165165)
        (make-circle  -0.6311979224  0.7184578971 0.2491045282)
        (make-circle   1.4685857879 -0.8347049536 1.3670667538)
        (make-circle  -0.6855727502  1.6465021616 1.0593087096)
        (make-circle   0.0152957411  0.0638919221 0.9771215985))))

;; bounding box
(define (enclosing-rect circles)
    (define xmin (for/min ((c circles)) (- c.x0 c.radius)))
    (define xmax (for/max ((c circles)) (+ c.x0 c.radius)))
    (define ymin (for/min ((c circles)) (- c.y0 c.radius)))
    (define ymax (for/max ((c circles)) (+ c.y0 c.radius)))
    (vector xmin ymin (- xmax xmin) (- ymax ymin)))

;; Compute surface of entirely overlapped tiles
;; and assign candidates circles to other tiles.
;; cands is a vector nsteps x nsteps of circles lists indexed by (i,j)

(define (S0 circles rect steps into: cands)
    (define dx (// (rect 2) steps)) ;; width / steps
    (define dy (// (rect 3) steps)) ;; height / steps
    (define ds (* dx dy)) ;; tile surface
    (define dr (vector (- rect.x0 dx) (- rect.y0 dy) dx dy))
    (define ijdx 0)

    (for/sum ((i steps))
        (vector+= dr 0 dx)
        (vector-set! dr 1 (- rect.y0 dy))

        (for/sum ((j steps))
        (vector+= dr 1 dy)
        (set! ijdx (+ i (* j steps)))

        (for/sum ((c circles))
            #:break (rect-in-disk? dr c) ;; enclosed ? add ds
                 => (begin   (vector-set! cands ijdx null) ds)
            #:continue (not (rect-disk-intersect? dr c))
                 ;; intersects ? add circle to candidates for this tile
            (vector-set! cands ijdx  (cons c (cands ijdx )))
            0)
    )))

(define ct 0)
;; return sum of surfaces of tiles which are not entirely overlapped
(define (S circles rect steps cands)
(++ ct)
    (define dx (// (rect 2) steps))
    (define dy (// (rect 3) steps))
    (define ds (* dx dy))
    (define dr (vector (- rect.x0 dx) (- rect.y0 dy) dx dy))
    (define ijdx 0)

    (for/sum ((i steps))
        (vector+= dr 0 dx)
        (vector-set! dr 1 (- (rect 1) dy))

        (for/sum ((j steps))
        (vector+= dr 1 dy)

        (when (!null? cands) (set! circles (cands (+ i (* j steps)))))
        #:continue (null? circles)

        ;; add surface
        (or
            (for/or ((c circles)) ;; enclosed ? add ds
            #:break (rect-in-disk? dr c) => ds
            #f )

            (if ;; not intersecting? add 0
            (for/or ((c circles))
             (rect-disk-intersect? dr c)) #f 0)

            ;; intersecting ? recurse until precision
            (when (> dx s-precision) (S circles dr 2 null))

            ;; no hope - add ds/2
            (// ds 2))
    )))

```

```txt

(length circles) → 14
(enclosing-rect circles) → #( -2.598415801 -2.2017717074 5.4340683427 5.3761387773)

(define nsteps 800)
(define cands (make-vector (* nsteps nsteps) null))
(define rect (enclosing-rect circles))

(define starter (S0 circles rect nsteps cands))
    → 21.479635555704785
(vector-length cands) → 640000
;; number of remaining tiles
(for/sum ((c cands)) (if (null? c) 0 1)) → 3670
;; number of candidates circles
(for/sum ((c cands)) (if (null? c) 0 (length c))) → 3701

;;    "The result is 21.565036603856399517.. . "
(define s-precision 0.0001)
(define delta (S null rect nsteps cands))
    → 0.08540009897433079 ;; 466_928 calls to S

(+ starter delta) ;; 5 decimals
   → 21.565035654679114

(define s-precision 0.00001)
(define delta (S null rect nsteps cands))
    → 0.08540100364929355 ;;  3_761_385 calls, time *= 10

(+ starter delta) ;; 6 decimals
    → 21.56503655935408

```


## Go

{{trans|Perl 6}} (more "based on" than a direct translation)
This is very memory inefficient and as written will not run on a 32 bit architecture (due mostly to the required size of the "unknown" Rectangle channel buffer to get even a few decimal places). It may be interesting anyway as an example of using channels with Go to split the work among several go routines (and processor cores).

```go
package main

import (
        "flag"
        "fmt"
        "math"
        "runtime"
        "sort"
)

// Note, the standard "image" package has Point and Rectangle but we
// can't use them here since they're defined using int rather than
// float64.

type Circle struct{ X, Y, R, rsq float64 }

func NewCircle(x, y, r float64) Circle {
        // We pre-calculate r² as an optimization
        return Circle{x, y, r, r * r}
}

func (c Circle) ContainsPt(x, y float64) bool {
        return distSq(x, y, c.X, c.Y) <= c.rsq
}

func (c Circle) ContainsC(c2 Circle) bool {
        return distSq(c.X, c.Y, c2.X, c2.Y) <= (c.R-c2.R)*(c.R-c2.R)
}

func (c Circle) ContainsR(r Rect) (full, corner bool) {
        nw := c.ContainsPt(r.NW())
        ne := c.ContainsPt(r.NE())
        sw := c.ContainsPt(r.SW())
        se := c.ContainsPt(r.SE())
        return nw && ne && sw && se, nw || ne || sw || se
}

func (c Circle) North() (float64, float64) { return c.X, c.Y + c.R }
func (c Circle) South() (float64, float64) { return c.X, c.Y - c.R }
func (c Circle) West() (float64, float64)  { return c.X - c.R, c.Y }
func (c Circle) East() (float64, float64)  { return c.X + c.R, c.Y }

type Rect struct{ X1, Y1, X2, Y2 float64 }

func (r Rect) Area() float64          { return (r.X2 - r.X1) * (r.Y2 - r.Y1) }
func (r Rect) NW() (float64, float64) { return r.X1, r.Y2 }
func (r Rect) NE() (float64, float64) { return r.X2, r.Y2 }
func (r Rect) SW() (float64, float64) { return r.X1, r.Y1 }
func (r Rect) SE() (float64, float64) { return r.X2, r.Y1 }

func (r Rect) Centre() (float64, float64) {
        return (r.X1 + r.X2) / 2.0, (r.Y1 + r.Y2) / 2.0
}

func (r Rect) ContainsPt(x, y float64) bool {
        return r.X1 <= x && x < r.X2 &&
                r.Y1 <= y && y < r.Y2
}

func (r Rect) ContainsPC(c Circle) bool { //  only N,W,E,S points of circle
        return r.ContainsPt(c.North()) ||
                r.ContainsPt(c.South()) ||
                r.ContainsPt(c.West()) ||
                r.ContainsPt(c.East())
}

func (r Rect) MinSide() float64 {
        return math.Min(r.X2-r.X1, r.Y2-r.Y1)
}

func distSq(x1, y1, x2, y2 float64) float64 {
        Δx, Δy := x2-x1, y2-y1
        return (Δx * Δx) + (Δy * Δy)
}

type CircleSet []Circle

// sort.Interface for sorting by radius big to small:
func (s CircleSet) Len() int           { return len(s) }
func (s CircleSet) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s CircleSet) Less(i, j int) bool { return s[i].R > s[j].R }

func (sp *CircleSet) RemoveContainedC() {
        s := *sp
        sort.Sort(s)
        for i := 0; i < len(s); i++ {
                for j := i + 1; j < len(s); {
                        if s[i].ContainsC(s[j]) {
                                s[j], s[len(s)-1] = s[len(s)-1], s[j]
                                s = s[:len(s)-1]
                        } else {
                                j++
                        }
                }
        }
        *sp = s
}

func (s CircleSet) Bounds() Rect {
        x1 := s[0].X - s[0].R
        x2 := s[0].X + s[0].R
        y1 := s[0].Y - s[0].R
        y2 := s[0].Y + s[0].R
        for _, c := range s[1:] {
                x1 = math.Min(x1, c.X-c.R)
                x2 = math.Max(x2, c.X+c.R)
                y1 = math.Min(y1, c.Y-c.R)
                y2 = math.Max(y2, c.Y+c.R)
        }
        return Rect{x1, y1, x2, y2}
}

var nWorkers = 4

func (s CircleSet) UnionArea(ε float64) (min, max float64) {
        sort.Sort(s)
        stop := make(chan bool)
        inside := make(chan Rect)
        outside := make(chan Rect)
        unknown := make(chan Rect, 5e7) // XXX

        for i := 0; i < nWorkers; i++ {
                go s.worker(stop, unknown, inside, outside)
        }
        r := s.Bounds()
        max = r.Area()
        unknown <- r
        for max-min > ε {
                select {
                case r = <-inside:
                        min += r.Area()
                case r = <-outside:
                        max -= r.Area()
                }
        }
        close(stop)
        return min, max
}

func (s CircleSet) worker(stop <-chan bool, unk chan Rect, in, out chan<- Rect) {
        for {
                select {
                case <-stop:
                        return
                case r := <-unk:
                        inside, outside := s.CategorizeR(r)
                        switch {
                        case inside:
                                in <- r
                        case outside:
                                out <- r
                        default:
                                // Split
                                midX, midY := r.Centre()
                                unk <- Rect{r.X1, r.Y1, midX, midY}
                                unk <- Rect{midX, r.Y1, r.X2, midY}
                                unk <- Rect{r.X1, midY, midX, r.Y2}
                                unk <- Rect{midX, midY, r.X2, r.Y2}
                        }
                }
        }
}

func (s CircleSet) CategorizeR(r Rect) (inside, outside bool) {
        anyCorner := false
        for _, c := range s {
                full, corner := c.ContainsR(r)
                if full {
                        return true, false // inside
                }
                anyCorner = anyCorner || corner
        }
        if anyCorner {
                return false, false // uncertain
        }
        for _, c := range s {
                if r.ContainsPC(c) {
                        return false, false // uncertain
                }
        }
        return false, true // outside
}

func main() {
        flag.IntVar(&nWorkers, "workers", nWorkers, "how many worker go routines to use")
        maxproc := flag.Int("cpu", runtime.NumCPU(), "GOMAXPROCS setting")
        flag.Parse()

        if *maxproc > 0 {
                runtime.GOMAXPROCS(*maxproc)
        } else {
                *maxproc = runtime.GOMAXPROCS(0)
        }

        circles := CircleSet{
                NewCircle(1.6417233788, 1.6121789534, 0.0848270516),
                NewCircle(-1.4944608174, 1.2077959613, 1.1039549836),
                NewCircle(0.6110294452, -0.6907087527, 0.9089162485),
                NewCircle(0.3844862411, 0.2923344616, 0.2375743054),
                NewCircle(-0.2495892950, -0.3832854473, 1.0845181219),
                NewCircle(1.7813504266, 1.6178237031, 0.8162655711),
                NewCircle(-0.1985249206, -0.8343333301, 0.0538864941),
                NewCircle(-1.7011985145, -0.1263820964, 0.4776976918),
                NewCircle(-0.4319462812, 1.4104420482, 0.7886291537),
                NewCircle(0.2178372997, -0.9499557344, 0.0357871187),
                NewCircle(-0.6294854565, -1.3078893852, 0.7653357688),
                NewCircle(1.7952608455, 0.6281269104, 0.2727652452),
                NewCircle(1.4168575317, 1.0683357171, 1.1016025378),
                NewCircle(1.4637371396, 0.9463877418, 1.1846214562),
                NewCircle(-0.5263668798, 1.7315156631, 1.4428514068),
                NewCircle(-1.2197352481, 0.9144146579, 1.0727263474),
                NewCircle(-0.1389358881, 0.1092805780, 0.7350208828),
                NewCircle(1.5293954595, 0.0030278255, 1.2472867347),
                NewCircle(-0.5258728625, 1.3782633069, 1.3495508831),
                NewCircle(-0.1403562064, 0.2437382535, 1.3804956588),
                NewCircle(0.8055826339, -0.0482092025, 0.3327165165),
                NewCircle(-0.6311979224, 0.7184578971, 0.2491045282),
                NewCircle(1.4685857879, -0.8347049536, 1.3670667538),
                NewCircle(-0.6855727502, 1.6465021616, 1.0593087096),
                NewCircle(0.0152957411, 0.0638919221, 0.9771215985),
        }
        fmt.Println("Starting with", len(circles), "circles.")
        circles.RemoveContainedC()
        fmt.Println("Removing redundant ones leaves", len(circles), "circles.")
        fmt.Println("Using", nWorkers, "workers with maxprocs =", *maxproc)
        const ε = 0.0001
        min, max := circles.UnionArea(ε)
        avg := (min + max) / 2.0
        rng := max - min
        fmt.Printf("Area = %v±%v\n", avg, rng)
        fmt.Printf("Area ≈ %.*f\n", 5, avg)
}
```

```txt
Starting with 25 circles.
Removing redundant ones leaves 14 circles.
Using 4 workers with maxprocs = 8
Area = 21.565036586751035±9.999999912935209e-05
Area ≈ 21.56504
       18.76 real        54.83 user        13.94 sys
```



## Haskell


### Grid Sampling Version

```haskell
data Circle = Circle { cx :: Double, cy :: Double, cr :: Double }

isInside :: Double -> Double -> Circle -> Bool
isInside x y c = (x - cx c) ^ 2 + (y - cy c) ^ 2 <= (cr c ^ 2)

isInsideAny :: Double -> Double -> [Circle] -> Bool
isInsideAny x y = any (isInside x y)

approximatedArea :: [Circle] -> Int -> Double
approximatedArea cs box_side = (fromIntegral count) * dx * dy
  where
    -- compute the bounding box of the circles
    x_min = minimum [cx c - cr c | c <- circles]
    x_max = maximum [cx c + cr c | c <- circles]
    y_min = minimum [cy c - cr c | c <- circles]
    y_max = maximum [cy c + cr c | c <- circles]
    dx = (x_max - x_min) / (fromIntegral box_side)
    dy = (y_max - y_min) / (fromIntegral box_side)
    count = length [0 | r <- [0 .. box_side - 1],
                        c <- [0 .. box_side - 1],
                        isInsideAny (posx c) (posy r) circles]
    posy r = y_min + (fromIntegral r) * dy
    posx c = x_min + (fromIntegral c) * dx

circles :: [Circle]
circles = [Circle ( 1.6417233788) ( 1.6121789534) 0.0848270516,
           Circle (-1.4944608174) ( 1.2077959613) 1.1039549836,
           Circle ( 0.6110294452) (-0.6907087527) 0.9089162485,
           Circle ( 0.3844862411) ( 0.2923344616) 0.2375743054,
           Circle (-0.2495892950) (-0.3832854473) 1.0845181219,
           Circle ( 1.7813504266) ( 1.6178237031) 0.8162655711,
           Circle (-0.1985249206) (-0.8343333301) 0.0538864941,
           Circle (-1.7011985145) (-0.1263820964) 0.4776976918,
           Circle (-0.4319462812) ( 1.4104420482) 0.7886291537,
           Circle ( 0.2178372997) (-0.9499557344) 0.0357871187,
           Circle (-0.6294854565) (-1.3078893852) 0.7653357688,
           Circle ( 1.7952608455) ( 0.6281269104) 0.2727652452,
           Circle ( 1.4168575317) ( 1.0683357171) 1.1016025378,
           Circle ( 1.4637371396) ( 0.9463877418) 1.1846214562,
           Circle (-0.5263668798) ( 1.7315156631) 1.4428514068,
           Circle (-1.2197352481) ( 0.9144146579) 1.0727263474,
           Circle (-0.1389358881) ( 0.1092805780) 0.7350208828,
           Circle ( 1.5293954595) ( 0.0030278255) 1.2472867347,
           Circle (-0.5258728625) ( 1.3782633069) 1.3495508831,
           Circle (-0.1403562064) ( 0.2437382535) 1.3804956588,
           Circle ( 0.8055826339) (-0.0482092025) 0.3327165165,
           Circle (-0.6311979224) ( 0.7184578971) 0.2491045282,
           Circle ( 1.4685857879) (-0.8347049536) 1.3670667538,
           Circle (-0.6855727502) ( 1.6465021616) 1.0593087096,
           Circle ( 0.0152957411) ( 0.0638919221) 0.9771215985]

main = putStrLn $ "Approximated area: " ++
                  (show $ approximatedArea circles 5000)
```

```txt
Approximated area: 21.564955642878786
```



### Analytical Solution

Breaking down circles to non-intersecting arcs and assemble zero winding paths, then calculate their areas. Pro: precision doesn't depend on a step size, so no need to wait longer for a more precise result; Con: probably not numerically stable in marginal situations, which can be catastrophic.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.List (sort)

data Vec = Vec Double Double

vCross, vDot :: Vec -> Vec -> Double
vCross (Vec a b) (Vec c d) = a*d - b*c
vDot (Vec a b) (Vec c d) = a*c + b*d

vAdd, vSub :: Vec -> Vec -> Vec
vAdd (Vec a b) (Vec c d) = Vec (a + c) (b + d)
vSub (Vec a b) (Vec c d) = Vec (a - c) (b - d)

vLen :: Vec -> Double
vLen x = sqrt $ vDot x x

vDist :: Vec -> Vec -> Double
vDist a b = vLen (a `vSub` b)

vScale :: Double -> Vec -> Vec
vScale s (Vec x y) = Vec (x * s) (y * s)

vNorm :: Vec -> Vec
vNorm v@(Vec x y) = Vec (x / l) (y / l) where l = vLen v


newtype Angle = A Double
    deriving (Eq, Ord, Num, Fractional)

aPi = A pi

vAngle :: Vec -> Angle
vAngle (Vec x y) = A (atan2 y x)

aNorm :: Angle -> Angle
aNorm a | a > aPi   = a - aPi * 2
        | a < -aPi  = a + aPi * 2
        | otherwise = a


data Circle = Circle Double Double Double
    deriving (Eq)

circleCross :: Circle -> Circle -> [Angle]
circleCross (Circle x0 y0 r0) (Circle x1 y1 r1)
    | d >= r0 + r1 || d <= abs(r0 - r1) = []
    | otherwise = map aNorm [ang - da, ang + da]
        where
            d = vDist (Vec x0 y0) (Vec x1 y1)
            s = (r0 + r1 + d) / 2
            a = sqrt $ s * (s - d) * (s - r0) * (s - r1)
            h = 2 * a / d
            dr = Vec (x1 - x0) (y1 - y0)
            dx = vScale (sqrt $ r0 ^ 2 - h ^ 2) $ vNorm dr
            ang = if r0 ^ 2 + d ^ 2 > r1 ^ 2
                    then vAngle dr
                    else aPi + vAngle dr
            da = A (asin (h / r0))


-- Angles of the start and end points of the circle arc.
data Angle2 = Angle2 Angle Angle

data Arc = Arc Circle Angle2

arcPoint :: Circle -> Angle -> Vec
arcPoint (Circle x y r) (A a) =
    vAdd (Vec x y) (Vec (r * cos a) (r * sin a))

arcStart, arcMid, arcEnd, arcCenter :: Arc -> Vec
arcStart  (Arc c (Angle2 a0 a1)) = arcPoint c a0
arcMid    (Arc c (Angle2 a0 a1)) = arcPoint c ((a0 + a1) / 2)
arcEnd    (Arc c (Angle2 a0 a1)) = arcPoint c a1
arcCenter (Arc (Circle x y r) _) = Vec x y

arcArea :: Arc -> Double
arcArea (Arc (Circle _ _ r) (Angle2 a0 a1)) = r ^ 2 * aDiff / 2
    where (A aDiff) = a1 - a0


splitCircles :: [Circle] -> [Arc]
splitCircles cs = filter (not . inAnyCircle) arcs where
    cSplit :: (Circle, [Angle]) -> [Arc]
    cSplit (c, angs) =
        zipWith Arc (repeat c) $ zipWith Angle2 angs $ tail angs

    -- If an arc that was part of one circle is inside *another* circle,
    -- it will not be part of the zero-winding path, so reject it.
    inCircle :: (Vec, Circle) -> Circle -> Bool
    inCircle (Vec x0 y0, c1) c2@(Circle x y r) =
        c1 /= c2 && vDist (Vec x0 y0) (Vec x y) < r

    f :: Circle -> (Circle, [Angle])
    f c = (c, sort $ [-aPi, aPi] ++ (concatMap (circleCross c) cs))
    cAngs = map f cs
    arcs = concatMap cSplit cAngs

    inAnyCircle :: Arc -> Bool
    inAnyCircle arc@(Arc c _) = any (inCircle (arcMid arc, c)) cs


{-
Given a list of arcs, build sets of closed paths from them.
If one arc's end point is no more than 1e-4 from another's
start point, they are considered connected.  Since these
start/end points resulted from intersecting circles earlier,
they *should* be exactly the same, but floating point
precision may cause small differences, hence the 1e-4 error
margin.  When there are genuinely different intersections
closer than this margin, the method will backfire, badly.
-}
makePaths :: [Arc] -> [[Arc]]
makePaths arcs = joinArcs [] arcs where
    joinArcs :: [Arc] -> [Arc] -> [[Arc]]
    joinArcs a [] = [a]
    joinArcs [] (x:xs) = joinArcs [x] xs
    joinArcs a (x:xs)
        | vDist (arcStart (head a)) (arcEnd (last a)) < 1e-4
            = a : joinArcs [] (x:xs)
        | vDist (arcEnd (last a)) (arcStart x) < 1e-4
            = joinArcs (a ++ [x]) xs
        | otherwise = joinArcs a (xs ++ [x])


pathArea :: [Arc] -> Double
pathArea arcs = a + polylineArea e where
    (a, e) = foldl f (0, []) arcs
    f (a, e) arc = (a + arcArea arc, e ++ [arcCenter arc, arcEnd arc])


-- Slice N-polygon into N-2 triangles.
polylineArea :: [Vec] -> Double
polylineArea (v:vs) = sum $ zipWith (triArea v) vs (tail vs)
    where triArea a b c = ((b `vSub` a) `vCross` (c `vSub` b)) / 2


circlesArea :: [Circle] -> Double
circlesArea = sum . map pathArea . makePaths . splitCircles


circles :: [Circle]
circles = [Circle ( 1.6417233788) ( 1.6121789534) 0.0848270516,
           Circle (-1.4944608174) ( 1.2077959613) 1.1039549836,
           Circle ( 0.6110294452) (-0.6907087527) 0.9089162485,
           Circle ( 0.3844862411) ( 0.2923344616) 0.2375743054,
           Circle (-0.2495892950) (-0.3832854473) 1.0845181219,
           Circle ( 1.7813504266) ( 1.6178237031) 0.8162655711,
           Circle (-0.1985249206) (-0.8343333301) 0.0538864941,
           Circle (-1.7011985145) (-0.1263820964) 0.4776976918,
           Circle (-0.4319462812) ( 1.4104420482) 0.7886291537,
           Circle ( 0.2178372997) (-0.9499557344) 0.0357871187,
           Circle (-0.6294854565) (-1.3078893852) 0.7653357688,
           Circle ( 1.7952608455) ( 0.6281269104) 0.2727652452,
           Circle ( 1.4168575317) ( 1.0683357171) 1.1016025378,
           Circle ( 1.4637371396) ( 0.9463877418) 1.1846214562,
           Circle (-0.5263668798) ( 1.7315156631) 1.4428514068,
           Circle (-1.2197352481) ( 0.9144146579) 1.0727263474,
           Circle (-0.1389358881) ( 0.1092805780) 0.7350208828,
           Circle ( 1.5293954595) ( 0.0030278255) 1.2472867347,
           Circle (-0.5258728625) ( 1.3782633069) 1.3495508831,
           Circle (-0.1403562064) ( 0.2437382535) 1.3804956588,
           Circle ( 0.8055826339) (-0.0482092025) 0.3327165165,
           Circle (-0.6311979224) ( 0.7184578971) 0.2491045282,
           Circle ( 1.4685857879) (-0.8347049536) 1.3670667538,
           Circle (-0.6855727502) ( 1.6465021616) 1.0593087096,
           Circle ( 0.0152957411) ( 0.0638919221) 0.9771215985]

main = print $ circlesArea circles
```

 21.5650366038564

This is how this solution works:

# Given a list of circles, give all of them the same winding. Say, imagine every circle turns clockwise.
# Find all the intersection points among all circles.  Between each pair, there may be 0, 1 or 2 intersections.  Ignore 0 and 1, we need only the 2 case.
# For each circle, sort its intersection points in clockwise order (keep in mind it's a cyclic list), and split it into arc segments between neighboring point pairs.  Circles that don't cross other circles are treated as a single 360 degree arc.
# Imagine all circles are on a white piece of paper, and have their interiors inked black.  We are only interested in the arcs separating black and white areas, so we get rid of the arcs that aren't so.  These are the arcs that lie entirely within another circle.  Because we've taken care of all intersections eariler, we only need to check if any point on an arc segment is in any other circle; if so, remove this arc.  (The haskell code uses the middle point of an arc for this, but anything other than the two end points would do).
# The remaining arcs form one or more closed paths.  Each path's area can be calculated, and the sum of them all is the area needed.  Each path is done like the way mentioned on that stackexchange page cited somewhere.  This works for holes, too.  Suppose the circles form an area with a hole in it; you'd end up with two paths, where the outer one winds clockwise, and the inner one ccw.  Use the same method to calculate the areas for both, and the outer one would have a positive area, the inner one negative.  Just add them up.

There are a few concerns about this algorithm: firstly, it's fast only if there are few circles.  Its complexity is maybe O(N^3) with N = number of circles, while normal scanline method is probably O(N * n) or less, with n = number of scanlines.  Secondly, step 4 needs to be accurate; a small precision error there may cause an arc to remain or be removed by mistake, with disastrous consequences.  Also, it's difficult to estimate the error in the final result.  The scanline or Monte Carlo methods have errors mostly due to statistics, while this method's error is due to floating point precision loss, which is a very different can of worms.


## J


### Uniform Grid

We're missing an error estimate.  Because it happened to be fairly accurate isn't proof that it's good.  Runtime is 16 seconds on a Lenovo T500 with plenty of memory and connected to the power grid.

```J
NB. check points on a regular grid within the bounding box


N=: 400  NB. grids in each dimension.  Controls accuracy.


'X Y R'=: |: XYR=: (_&".;._2~ LF&=)0 :0
 1.6417233788  1.6121789534 0.0848270516
-1.4944608174  1.2077959613 1.1039549836
 0.6110294452 -0.6907087527 0.9089162485
 0.3844862411  0.2923344616 0.2375743054
-0.2495892950 -0.3832854473 1.0845181219
 1.7813504266  1.6178237031 0.8162655711
-0.1985249206 -0.8343333301 0.0538864941
-1.7011985145 -0.1263820964 0.4776976918
-0.4319462812  1.4104420482 0.7886291537
 0.2178372997 -0.9499557344 0.0357871187
-0.6294854565 -1.3078893852 0.7653357688
 1.7952608455  0.6281269104 0.2727652452
 1.4168575317  1.0683357171 1.1016025378
 1.4637371396  0.9463877418 1.1846214562
-0.5263668798  1.7315156631 1.4428514068
-1.2197352481  0.9144146579 1.0727263474
-0.1389358881  0.1092805780 0.7350208828
 1.5293954595  0.0030278255 1.2472867347
-0.5258728625  1.3782633069 1.3495508831
-0.1403562064  0.2437382535 1.3804956588
 0.8055826339 -0.0482092025 0.3327165165
-0.6311979224  0.7184578971 0.2491045282
 1.4685857879 -0.8347049536 1.3670667538
-0.6855727502  1.6465021616 1.0593087096
 0.0152957411  0.0638919221 0.9771215985
)

bbox=: (<./@:- , >./@:+)&R
BBOXX=: bbox X
BBOXY=: bbox Y

grid=: 3 : 0
'MN MX N'=. y
D=. MX-MN
EDGE=. D%N
(MN(+ -:)EDGE)+(D-EDGE)*(i. % <:)N
)

assert 2.2 2.6 3 3.4 3.8 -: grid 2 4 5

GRIDDED_SAMPLES=: BBOXX {@:;&(grid@:(,&N)) BBOXY

Note '4 4{.GRIDDED_SAMPLES'  NB. example
┌─────────────────┬─────────────────┬─────────────────┬─────────────────┐
│_2.59706 _2.20043│_2.59706 _2.19774│_2.59706 _2.19505│_2.59706 _2.19236│
├─────────────────┼─────────────────┼─────────────────┼─────────────────┤
│_2.59434 _2.20043│_2.59434 _2.19774│_2.59434 _2.19505│_2.59434 _2.19236│
├─────────────────┼─────────────────┼─────────────────┼─────────────────┤
│_2.59162 _2.20043│_2.59162 _2.19774│_2.59162 _2.19505│_2.59162 _2.19236│
├─────────────────┼─────────────────┼─────────────────┼─────────────────┤
│_2.58891 _2.20043│_2.58891 _2.19774│_2.58891 _2.19505│_2.58891 _2.19236│
└─────────────────┴─────────────────┴─────────────────┴─────────────────┘
)
XY=: >,GRIDDED_SAMPLES  NB. convert to an usual array of floats.

mp=: $:~ :(+/ .*)  NB. matrix product
assert (*: 5 13) -: (mp"1) 3 4,:5 12

in=: *:@:{:@:] >: [: mp (- }:)    NB. logical function
assert 0 0 in 1 0 2         NB. X Y  in  X Y R
assert 0 0 (-.@:in) 44 2 3

CONTAINED=: XY in"1/XYR NB. logical table of circles containing each grid
FRACTION=: CONTAINED (+/@:(+./"1)@:[ % *:@:]) N
AREA=: BBOXX*&(-/)BBOXY  NB. area of the bounding box.
FRACTION*AREA

NB. result is 21.5645
```



## Java


Solution using recursive subdivision of space into rectangles. Base cases of recursion are
when the rectangle is fully inside some circle, is fully outside all circles, or the maximum
depth limit is reached.


```Java

public class CirclesTotalArea {

    /*
     * Rectangles are given as 4-element arrays [tx, ty, w, h].
     * Circles are given as 3-element arrays [cx, cy, r].
     */

    private static double distSq(double x1, double y1, double x2, double y2) {
        return (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1);
    }

    private static boolean rectangleFullyInsideCircle(double[] rect, double[] circ) {
        double r2 = circ[2] * circ[2];
        // Every corner point of rectangle must be inside the circle.
        return distSq(rect[0], rect[1], circ[0], circ[1]) <= r2 &&
          distSq(rect[0] + rect[2], rect[1], circ[0], circ[1]) <= r2 &&
          distSq(rect[0], rect[1] - rect[3], circ[0], circ[1]) <= r2 &&
          distSq(rect[0] + rect[2], rect[1] - rect[3], circ[0], circ[1]) <= r2;
    }

    private static boolean rectangleSurelyOutsideCircle(double[] rect, double[] circ) {
        // Circle center point inside rectangle?
        if(rect[0] <= circ[0] && circ[0] <= rect[0] + rect[2] &&
          rect[1] - rect[3] <= circ[1] && circ[1] <= rect[1]) { return false; }
        // Otherwise, check that each corner is at least (r + Max(w, h)) away from circle center.
        double r2 = circ[2] + Math.max(rect[2], rect[3]);
        r2 = r2 * r2;
        return distSq(rect[0], rect[1], circ[0], circ[1]) >= r2 &&
          distSq(rect[0] + rect[2], rect[1], circ[0], circ[1]) >= r2 &&
          distSq(rect[0], rect[1] - rect[3], circ[0], circ[1]) >= r2 &&
          distSq(rect[0] + rect[2], rect[1] - rect[3], circ[0], circ[1]) >= r2;
    }

    private static boolean[] surelyOutside;

    private static double totalArea(double[] rect, double[][] circs, int d) {
        // Check if we can get a quick certain answer.
        int surelyOutsideCount = 0;
        for(int i = 0; i < circs.length; i++) {
            if(rectangleFullyInsideCircle(rect, circs[i])) { return rect[2] * rect[3]; }
            if(rectangleSurelyOutsideCircle(rect, circs[i])) {
                surelyOutside[i] = true;
                surelyOutsideCount++;
            }
            else { surelyOutside[i] = false; }
        }
        // Is this rectangle surely outside all circles?
        if(surelyOutsideCount == circs.length) { return 0; }
        // Are we deep enough in the recursion?
        if(d < 1) {
            return rect[2] * rect[3] / 3;  // Best guess for overlapping portion
        }
        // Throw out all circles that are surely outside this rectangle.
        if(surelyOutsideCount > 0) {
            double[][] newCircs = new double[circs.length - surelyOutsideCount][3];
            int loc = 0;
            for(int i = 0; i < circs.length; i++) {
                if(!surelyOutside[i]) { newCircs[loc++] = circs[i]; }
            }
            circs = newCircs;
        }
        // Subdivide this rectangle recursively and add up the recursively computed areas.
        double w = rect[2] / 2; // New width
        double h = rect[3] / 2; // New height
        double[][] pieces = {
            { rect[0], rect[1], w, h }, // NW
            { rect[0] + w, rect[1], w, h }, // NE
            { rect[0], rect[1] - h, w, h }, // SW
            { rect[0] + w, rect[1] - h, w, h } // SE
        };
        double total = 0;
        for(double[] piece: pieces) { total += totalArea(piece, circs, d - 1); }
        return total;
    }

    public static double totalArea(double[][] circs, int d) {
        double maxx = Double.NEGATIVE_INFINITY;
        double minx = Double.POSITIVE_INFINITY;
        double maxy = Double.NEGATIVE_INFINITY;
        double miny = Double.POSITIVE_INFINITY;
        // Find the extremes of x and y for this set of circles.
        for(double[] circ: circs) {
            if(circ[0] + circ[2] > maxx) { maxx = circ[0] + circ[2]; }
            if(circ[0] - circ[2] < minx) { minx = circ[0] - circ[2]; }
            if(circ[1] + circ[2] > maxy) { maxy = circ[1] + circ[2]; }
            if(circ[1] - circ[2] < miny) { miny = circ[1] - circ[2]; }
        }
        double[] rect = { minx, maxy, maxx - minx, maxy - miny };
        surelyOutside = new boolean[circs.length];
        return totalArea(rect, circs, d);
    }

    public static void main(String[] args) {
        double[][] circs = {
            { 1.6417233788, 1.6121789534, 0.0848270516 },
            {-1.4944608174, 1.2077959613, 1.1039549836 },
            { 0.6110294452, -0.6907087527, 0.9089162485 },
            { 0.3844862411, 0.2923344616, 0.2375743054 },
            {-0.2495892950, -0.3832854473, 1.0845181219 },
            {1.7813504266, 1.6178237031, 0.8162655711 },
            {-0.1985249206, -0.8343333301, 0.0538864941 },
            {-1.7011985145, -0.1263820964, 0.4776976918 },
            {-0.4319462812, 1.4104420482, 0.7886291537 },
            {0.2178372997, -0.9499557344, 0.0357871187 },
            {-0.6294854565, -1.3078893852, 0.7653357688 },
            {1.7952608455, 0.6281269104, 0.2727652452 },
            {1.4168575317, 1.0683357171, 1.1016025378 },
            {1.4637371396, 0.9463877418, 1.1846214562 },
            {-0.5263668798, 1.7315156631, 1.4428514068 },
            {-1.2197352481, 0.9144146579, 1.0727263474 },
            {-0.1389358881, 0.1092805780, 0.7350208828 },
            {1.5293954595, 0.0030278255, 1.2472867347 },
            {-0.5258728625, 1.3782633069, 1.3495508831 },
            {-0.1403562064, 0.2437382535, 1.3804956588 },
            {0.8055826339, -0.0482092025, 0.3327165165 },
            {-0.6311979224, 0.7184578971, 0.2491045282 },
            {1.4685857879, -0.8347049536, 1.3670667538 },
            {-0.6855727502, 1.6465021616, 1.0593087096 },
            {0.0152957411, 0.0638919221, 0.9771215985 }
        };
        double ans = totalArea(circs, 24);
        System.out.println("Approx. area is " + ans);
        System.out.println("Error is " + Math.abs(21.56503660 - ans));
    }
}
```



## Julia

Simple grid algorithm. Borrows the xmin/xmax idea from the Python version. This algorithm is fairly slow.


```julia
# Total circles area: https://rosettacode.org/wiki/Total_circles_area
# v0.6

xc = [1.6417233788, -1.4944608174, 0.6110294452, 0.3844862411, -0.2495892950, 1.7813504266,
     -0.1985249206, -1.7011985145, -0.4319462812, 0.2178372997, -0.6294854565, 1.7952608455,
      1.4168575317, 1.4637371396, -0.5263668798, -1.2197352481, -0.1389358881, 1.5293954595,
     -0.5258728625, -0.1403562064, 0.8055826339, -0.6311979224, 1.4685857879, -0.6855727502,
      0.0152957411]
yc = [1.6121789534, 1.2077959613, -0.6907087527, 0.2923344616, -0.3832854473, 1.6178237031,
     -0.8343333301, -0.1263820964, 1.4104420482, -0.9499557344, -1.3078893852, 0.6281269104,
      1.0683357171, 0.9463877418, 1.7315156631, 0.9144146579, 0.1092805780, 0.0030278255,
      1.3782633069, 0.2437382535, -0.0482092025, 0.7184578971, -0.8347049536, 1.6465021616,
      0.0638919221]
r  = [0.0848270516, 1.1039549836, 0.9089162485, 0.2375743054, 1.0845181219, 0.8162655711,
      0.0538864941, 0.4776976918, 0.7886291537, 0.0357871187, 0.7653357688, 0.2727652452,
      1.1016025378, 1.1846214562, 1.4428514068, 1.0727263474, 0.7350208828, 1.2472867347,
      1.3495508831, 1.3804956588, 0.3327165165, 0.2491045282, 1.3670667538, 1.0593087096,
      0.9771215985]

# Size of my grid -- higher values => higher accuracy.
function main(xc::Vector{<:Real}, yc::Vector{<:Real}, r::Vector{<:Real}, ngrid::Integer=10000)
    r2 = r .* r
    ncircles = length(xc)

    # Compute the bounding box of the circles.
    xmin = minimum(xc .- r)
    xmax = maximum(xc .+ r)
    ymin = minimum(yc .- r)
    ymax = maximum(yc .+ r)
    # Keep a counter.
    inside = 0
    # For every point in my grid.
    for x in linspace(xmin, xmax, ngrid), y = linspace(ymin, ymax, ngrid)
        inside += any(r2 .> (x - xc) .^ 2 + (y - yc) .^ 2)
    end
    boxarea = (xmax - xmin) * (ymax - ymin)
    return boxarea * inside / ngrid ^ 2
end

println(@time main(xc, yc, r, 1000))
```



## Kotlin


### Grid Sampling Version

```scala
// version 1.1.2

class Circle(val x: Double, val y: Double, val r: Double)

val circles = arrayOf(
    Circle( 1.6417233788,  1.6121789534, 0.0848270516),
    Circle(-1.4944608174,  1.2077959613, 1.1039549836),
    Circle( 0.6110294452, -0.6907087527, 0.9089162485),
    Circle( 0.3844862411,  0.2923344616, 0.2375743054),
    Circle(-0.2495892950, -0.3832854473, 1.0845181219),
    Circle( 1.7813504266,  1.6178237031, 0.8162655711),
    Circle(-0.1985249206, -0.8343333301, 0.0538864941),
    Circle(-1.7011985145, -0.1263820964, 0.4776976918),
    Circle(-0.4319462812,  1.4104420482, 0.7886291537),
    Circle( 0.2178372997, -0.9499557344, 0.0357871187),
    Circle(-0.6294854565, -1.3078893852, 0.7653357688),
    Circle( 1.7952608455,  0.6281269104, 0.2727652452),
    Circle( 1.4168575317,  1.0683357171, 1.1016025378),
    Circle( 1.4637371396,  0.9463877418, 1.1846214562),
    Circle(-0.5263668798,  1.7315156631, 1.4428514068),
    Circle(-1.2197352481,  0.9144146579, 1.0727263474),
    Circle(-0.1389358881,  0.1092805780, 0.7350208828),
    Circle( 1.5293954595,  0.0030278255, 1.2472867347),
    Circle(-0.5258728625,  1.3782633069, 1.3495508831),
    Circle(-0.1403562064,  0.2437382535, 1.3804956588),
    Circle( 0.8055826339, -0.0482092025, 0.3327165165),
    Circle(-0.6311979224,  0.7184578971, 0.2491045282),
    Circle( 1.4685857879, -0.8347049536, 1.3670667538),
    Circle(-0.6855727502,  1.6465021616, 1.0593087096),
    Circle( 0.0152957411,  0.0638919221, 0.9771215985)
)

fun Double.sq() = this * this

fun main(args: Array<String>) {
    val xMin = circles.map { it.x - it.r }.min()!!
    val xMax = circles.map { it.x + it.r }.max()!!
    val yMin = circles.map { it.y - it.r }.min()!!
    val yMax = circles.map { it.y + it.r }.max()!!
    val boxSide = 5000
    val dx = (xMax - xMin) / boxSide
    val dy = (yMax - yMin) / boxSide
    var count = 0
    for (r in 0 until boxSide) {
        val y = yMin + r * dy
        for (c in 0 until boxSide) {
            val x = xMin + c * dx
            val b = circles.any { (x - it.x).sq() + (y - it.y).sq() <= it.r.sq() }
            if (b) count++
        }
    }
    println("Approximate area = ${count * dx * dy}")
}

```


```txt

Approximate area = 21.564955642878786

```



### Scanline Version

```scala
// version 1.1.2

class Point(val x: Double, val y: Double)

class Circle(val x: Double, val y: Double, val r: Double)

val circles = arrayOf(
    Circle( 1.6417233788,  1.6121789534, 0.0848270516),
    Circle(-1.4944608174,  1.2077959613, 1.1039549836),
    Circle( 0.6110294452, -0.6907087527, 0.9089162485),
    Circle( 0.3844862411,  0.2923344616, 0.2375743054),
    Circle(-0.2495892950, -0.3832854473, 1.0845181219),
    Circle( 1.7813504266,  1.6178237031, 0.8162655711),
    Circle(-0.1985249206, -0.8343333301, 0.0538864941),
    Circle(-1.7011985145, -0.1263820964, 0.4776976918),
    Circle(-0.4319462812,  1.4104420482, 0.7886291537),
    Circle( 0.2178372997, -0.9499557344, 0.0357871187),
    Circle(-0.6294854565, -1.3078893852, 0.7653357688),
    Circle( 1.7952608455,  0.6281269104, 0.2727652452),
    Circle( 1.4168575317,  1.0683357171, 1.1016025378),
    Circle( 1.4637371396,  0.9463877418, 1.1846214562),
    Circle(-0.5263668798,  1.7315156631, 1.4428514068),
    Circle(-1.2197352481,  0.9144146579, 1.0727263474),
    Circle(-0.1389358881,  0.1092805780, 0.7350208828),
    Circle( 1.5293954595,  0.0030278255, 1.2472867347),
    Circle(-0.5258728625,  1.3782633069, 1.3495508831),
    Circle(-0.1403562064,  0.2437382535, 1.3804956588),
    Circle( 0.8055826339, -0.0482092025, 0.3327165165),
    Circle(-0.6311979224,  0.7184578971, 0.2491045282),
    Circle( 1.4685857879, -0.8347049536, 1.3670667538),
    Circle(-0.6855727502,  1.6465021616, 1.0593087096),
    Circle( 0.0152957411,  0.0638919221, 0.9771215985)
)

fun Double.sq() = this * this

fun areaScan(precision: Double): Double {
    fun sect(c: Circle, y: Double): Point {
        val dr = Math.sqrt(c.r.sq() - (y - c.y).sq())
        return Point(c.x - dr, c.x + dr)
    }

    val ys = circles.map { it.y + it.r } + circles.map { it.y - it.r }
    val mins = Math.floor(ys.min()!! / precision).toInt()
    val maxs = Math.ceil(ys.max()!! / precision).toInt()
    var total = 0.0
    for (x in mins..maxs) {
        val y = x * precision
        var right = Double.NEGATIVE_INFINITY
        val points = circles.filter { Math.abs(y - it.y) < it.r }
                            .map { sect(it, y) }
                            .sortedBy { it.x }
        for (p in points) {
            if (p.y <= right) continue
            total += p.y - maxOf(p.x, right)
            right = p.y
        }
    }
    return total * precision
}

fun main(args: Array<String>) {
    val p = 1e-6
    println("Approximate area = ${areaScan(p)}")
}
```


```txt

Approximate area = 21.565036604050903

```



## Mathematica

Simple solution needs Mathematica 10:


```Mathematica
data = ImportString[" 1.6417233788  1.6121789534 0.0848270516
   -1.4944608174  1.2077959613 1.1039549836
    0.6110294452 -0.6907087527 0.9089162485
    0.3844862411  0.2923344616 0.2375743054
   -0.2495892950 -0.3832854473 1.0845181219
    1.7813504266  1.6178237031 0.8162655711
   -0.1985249206 -0.8343333301 0.0538864941
   -1.7011985145 -0.1263820964 0.4776976918
   -0.4319462812  1.4104420482 0.7886291537
    0.2178372997 -0.9499557344 0.0357871187
   -0.6294854565 -1.3078893852 0.7653357688
    1.7952608455  0.6281269104 0.2727652452
    1.4168575317  1.0683357171 1.1016025378
    1.4637371396  0.9463877418 1.1846214562
   -0.5263668798  1.7315156631 1.4428514068
   -1.2197352481  0.9144146579 1.0727263474
   -0.1389358881  0.1092805780 0.7350208828
    1.5293954595  0.0030278255 1.2472867347
   -0.5258728625  1.3782633069 1.3495508831
   -0.1403562064  0.2437382535 1.3804956588
    0.8055826339 -0.0482092025 0.3327165165
   -0.6311979224  0.7184578971 0.2491045282
    1.4685857879 -0.8347049536 1.3670667538
   -0.6855727502  1.6465021616 1.0593087096
    0.0152957411  0.0638919221 0.9771215985", "Table"];

toDisk[{x_, y_, r_}] := Disk[{x, y}, r];
RegionMeasure[RegionUnion[toDisk /@ data]]
```


Returns 21.5650370663759

If one assumes that the input data is exact (all omitted digits are zero) then the exact answer can be derived by changing  the definition of toDisk in the above code to

```Mathematica
toDisk[{x_, y_, r_}] := Disk[{Rationalize[x, 0], Rationalize[y, 0]}, Rationalize[r, 0]]
```

The first 100 digits of the decimal expansion of the result are 21.56503660385639951717662965041005741103435843377395022310218015982077828980273399575555145558778509 so the solution given in the problem statement is incorrect after the first 16 decimal places (if we assume no bugs in the parts of Mathematica used here).

=={{header|MATLAB}} / {{header|Octave}}==

Simple grid algorithm. Borrows the xmin/xmax idea from the Python version. This algorithm is fairly slow.


```Matlab
function res = circles()

tic
%
% Size of my grid -- higher values => higher accuracy.
%
ngrid = 5000;

xc = [1.6417233788 -1.4944608174  0.6110294452  0.3844862411 -0.2495892950  1.7813504266 -0.1985249206 -1.7011985145 -0.4319462812  0.2178372997 -0.6294854565  1.7952608455  1.4168575317  1.4637371396 -0.5263668798 -1.2197352481 -0.1389358881  1.5293954595 -0.5258728625 -0.1403562064  0.8055826339 -0.6311979224  1.4685857879 -0.6855727502  0.0152957411];
yc = [1.6121789534  1.2077959613 -0.6907087527  0.2923344616 -0.3832854473  1.6178237031 -0.8343333301 -0.1263820964  1.4104420482 -0.9499557344 -1.3078893852  0.6281269104  1.0683357171  0.9463877418  1.7315156631  0.9144146579  0.1092805780  0.0030278255  1.3782633069  0.2437382535 -0.0482092025  0.7184578971 -0.8347049536  1.6465021616  0.0638919221];
r  = [0.0848270516  1.1039549836  0.9089162485  0.2375743054  1.0845181219  0.8162655711  0.0538864941  0.4776976918  0.7886291537  0.0357871187  0.7653357688  0.2727652452  1.1016025378  1.1846214562  1.4428514068  1.0727263474  0.7350208828  1.2472867347  1.3495508831  1.3804956588  0.3327165165  0.2491045282  1.3670667538  1.0593087096  0.9771215985];
r2 = r .* r;

ncircles = length(xc);

%
% Compute the bounding box of the circles.
%
xmin = min(xc-r);
xmax = max(xc+r);
ymin = min(yc-r);
ymax = max(yc+r);

%
% Keep a counter.
%
inside = 0;

%
% For every point in my grid.
%
for x = linspace(xmin,xmax,ngrid)
    for y = linspace(ymin,ymax,ngrid)
        if any(r2 > (x - xc).^2 + (y - yc).^2)
            inside = inside + 1;
        end
    end
end

box_area = (xmax-xmin) * (ymax-ymin);

res = box_area * inside / ngrid^2;
toc

end
```



## Nim


### Grid Sampling Version

```nim
import future

type Circle = tuple[x, y, r: float]

const circles: seq[Circle] = @[
  ( 1.6417233788,  1.6121789534, 0.0848270516),
  (-1.4944608174,  1.2077959613, 1.1039549836),
  ( 0.6110294452, -0.6907087527, 0.9089162485),
  ( 0.3844862411,  0.2923344616, 0.2375743054),
  (-0.2495892950, -0.3832854473, 1.0845181219),
  ( 1.7813504266,  1.6178237031, 0.8162655711),
  (-0.1985249206, -0.8343333301, 0.0538864941),
  (-1.7011985145, -0.1263820964, 0.4776976918),
  (-0.4319462812,  1.4104420482, 0.7886291537),
  ( 0.2178372997, -0.9499557344, 0.0357871187),
  (-0.6294854565, -1.3078893852, 0.7653357688),
  ( 1.7952608455,  0.6281269104, 0.2727652452),
  ( 1.4168575317,  1.0683357171, 1.1016025378),
  ( 1.4637371396,  0.9463877418, 1.1846214562),
  (-0.5263668798,  1.7315156631, 1.4428514068),
  (-1.2197352481,  0.9144146579, 1.0727263474),
  (-0.1389358881,  0.1092805780, 0.7350208828),
  ( 1.5293954595,  0.0030278255, 1.2472867347),
  (-0.5258728625,  1.3782633069, 1.3495508831),
  (-0.1403562064,  0.2437382535, 1.3804956588),
  ( 0.8055826339, -0.0482092025, 0.3327165165),
  (-0.6311979224,  0.7184578971, 0.2491045282),
  ( 1.4685857879, -0.8347049536, 1.3670667538),
  (-0.6855727502,  1.6465021616, 1.0593087096),
  ( 0.0152957411,  0.0638919221, 0.9771215985)]

let xMin = min circles.map((c: Circle) => c.x - c.r)
let xMax = max circles.map((c: Circle) => c.x + c.r)
let yMin = min circles.map((c: Circle) => c.y - c.r)
let yMax = max circles.map((c: Circle) => c.y + c.r)

const boxSide = 500

let dx = (xMax - xMin) / boxSide
let dy = (yMax - yMin) / boxSide

var count = 0

for r in 0 .. <boxSide:
  let y = yMin + float(r) * dy
  for c in 0 .. <boxSide:
    let x = xMin + float(c) * dx
    for circle in circles:
      if (x-circle.x)*(x-circle.x) + (y-circle.y)*(y-circle.y) <= circle.r*circle.r:
        inc count
        break

echo "Approximated area: ", float(count) * dx * dy
```

Output:

```txt
Approximated area: 2.1561559772003317e+01
```




## Perl

```perl
use strict;
use warnings;
use feature 'say';

use List::AllUtils <min max>;

my @circles = (
    [ 1.6417233788,  1.6121789534, 0.0848270516],
    [-1.4944608174,  1.2077959613, 1.1039549836],
    [ 0.6110294452, -0.6907087527, 0.9089162485],
    [ 0.3844862411,  0.2923344616, 0.2375743054],
    [-0.2495892950, -0.3832854473, 1.0845181219],
    [ 1.7813504266,  1.6178237031, 0.8162655711],
    [-0.1985249206, -0.8343333301, 0.0538864941],
    [-1.7011985145, -0.1263820964, 0.4776976918],
    [-0.4319462812,  1.4104420482, 0.7886291537],
    [ 0.2178372997, -0.9499557344, 0.0357871187],
    [-0.6294854565, -1.3078893852, 0.7653357688],
    [ 1.7952608455,  0.6281269104, 0.2727652452],
    [ 1.4168575317,  1.0683357171, 1.1016025378],
    [ 1.4637371396,  0.9463877418, 1.1846214562],
    [-0.5263668798,  1.7315156631, 1.4428514068],
    [-1.2197352481,  0.9144146579, 1.0727263474],
    [-0.1389358881,  0.1092805780, 0.7350208828],
    [ 1.5293954595,  0.0030278255, 1.2472867347],
    [-0.5258728625,  1.3782633069, 1.3495508831],
    [-0.1403562064,  0.2437382535, 1.3804956588],
    [ 0.8055826339, -0.0482092025, 0.3327165165],
    [-0.6311979224,  0.7184578971, 0.2491045282],
    [ 1.4685857879, -0.8347049536, 1.3670667538],
    [-0.6855727502,  1.6465021616, 1.0593087096],
    [ 0.0152957411,  0.0638919221, 0.9771215985],
);

my $x_min = min map { $_->[0] - $_->[2] } @circles;
my $x_max = max map { $_->[0] + $_->[2] } @circles;
my $y_min = min map { $_->[1] - $_->[2] } @circles;
my $y_max = max map { $_->[1] + $_->[2] } @circles;

my $box_side = 500;
my $dx = ($x_max - $x_min) / $box_side;
my $dy = ($y_max - $y_min) / $box_side;
my $count = 0;

for my $r (0..$box_side) {
    my $y = $y_min + $r * $dy;
    for my $c (0..$box_side) {
        my $x = $x_min + $c * $dx;
        for my $c (@circles) {
            $count++ and last if ($x - $$c[0])**2 + ($y - $$c[1])**2 <= $$c[2]**2
        }
    }
}

printf "Approximated area: %.9f\n", $count * $dx * $dy;
```

```txt
Approximated area: 21.561559772
```



## Perl 6

This subdivides the outer rectangle repeatedly into subrectangles, and classifies them into wet, dry, or unknown.  The knowns are summed to provide an inner bound and an outer bound, while the unknowns are further subdivided.  The estimate is the average of the outer bound and the inner bound.  Not the simplest algorithm, but converges fairly rapidly because it can treat large areas sparsely, saving the fine subdivisions for the circle boundaries.  The number of unknown rectangles roughly doubles each pass, but the area of those unknowns is about half.

```perl6
class Point {
    has Real $.x;
    has Real $.y;
    has Int $!cbits;	# bitmap of circle membership

    method cbits { $!cbits //= set_cbits(self) }
    method gist { $!x ~ "\t" ~ $!y }
}

multi infix:<to>(Point $p1, Point $p2) {
    sqrt ($p1.x - $p2.x) ** 2 + ($p1.y - $p2.y) ** 2;
}

multi infix:<mid>(Point $p1, Point $p2) {
    Point.new(x => ($p1.x + $p2.x) / 2, y => ($p1.y + $p2.y) / 2);
}

class Circle {
    has Point $.center;
    has Real $.radius;

    has Point $.north = Point.new(x => $!center.x, y => $!center.y + $!radius);
    has Point $.west  = Point.new(x => $!center.x - $!radius, y => $!center.y);
    has Point $.south = Point.new(x => $!center.x, y => $!center.y - $!radius);
    has Point $.east  = Point.new(x => $!center.x + $!radius, y => $!center.y);

    multi method contains(Circle $c) { $!center to $c.center <= $!radius - $c.radius }
    multi method contains(Point $p) { $!center to $p <= $!radius }
    method gist { $!center.gist ~ "\t" ~ $.radius }
}

class Rect {
    has Point $.nw;
    has Point $.ne;
    has Point $.sw;
    has Point $.se;

    method diag { $!ne to $!se }
    method area { ($!ne.x - $!nw.x) * ($!nw.y - $!sw.y) }
    method contains(Point $p) {
	$!nw.x < $p.x < $!ne.x and
	$!sw.y < $p.y < $!nw.y;
    }
}

my @rawcircles = sort -*.radius,
    map -> $x, $y, $radius { Circle.new(:center(Point.new(:$x, :$y)), :$radius) },
    <
	 1.6417233788  1.6121789534 0.0848270516
	-1.4944608174  1.2077959613 1.1039549836
	 0.6110294452 -0.6907087527 0.9089162485
	 0.3844862411  0.2923344616 0.2375743054
	-0.2495892950 -0.3832854473 1.0845181219
	 1.7813504266  1.6178237031 0.8162655711
	-0.1985249206 -0.8343333301 0.0538864941
	-1.7011985145 -0.1263820964 0.4776976918
	-0.4319462812  1.4104420482 0.7886291537
	 0.2178372997 -0.9499557344 0.0357871187
	-0.6294854565 -1.3078893852 0.7653357688
	 1.7952608455  0.6281269104 0.2727652452
	 1.4168575317  1.0683357171 1.1016025378
	 1.4637371396  0.9463877418 1.1846214562
	-0.5263668798  1.7315156631 1.4428514068
	-1.2197352481  0.9144146579 1.0727263474
	-0.1389358881  0.1092805780 0.7350208828
	 1.5293954595  0.0030278255 1.2472867347
	-0.5258728625  1.3782633069 1.3495508831
	-0.1403562064  0.2437382535 1.3804956588
	 0.8055826339 -0.0482092025 0.3327165165
	-0.6311979224  0.7184578971 0.2491045282
	 1.4685857879 -0.8347049536 1.3670667538
	-0.6855727502  1.6465021616 1.0593087096
	 0.0152957411  0.0638919221 0.9771215985
    >».Num;

# remove redundant circles
my @circles;
while @rawcircles {
    my $c = @rawcircles.shift;
    next if @circles.any.contains($c);
    push @circles, $c;
}

sub set_cbits(Point $p) {
    my $cbits = 0;
    for @circles Z (1,2,4...*) -> ($c, $b) {
	$cbits += $b if $c.contains($p);
    }
    $cbits;
}

my $xmin = min @circles.map: { .center.x - .radius }
my $xmax = max @circles.map: { .center.x + .radius }
my $ymin = min @circles.map: { .center.y - .radius }
my $ymax = max @circles.map: { .center.y + .radius }

my $min-radius = @circles[*-1].radius;

my $outer-rect = Rect.new:
    nw => Point.new(x => $xmin, y => $ymax),
    ne => Point.new(x => $xmax, y => $ymax),
    sw => Point.new(x => $xmin, y => $ymin),
    se => Point.new(x => $xmax, y => $ymin);

my $outer-area = $outer-rect.area;

my @unknowns = $outer-rect;
my $known-dry = 0e0;
my $known-wet = 0e0;
my $div = 1;

# divide current rects each into four rects, analyze each
sub divide(@old) {

    $div *= 2;

    # rects too small to hold circle?
    my $smallish = @old[0].diag < $min-radius;

    my @unk;
    for @old {
	my $center = .nw mid .se;
	my $north = .nw mid .ne;
	my $south = .sw mid .se;
	my $west = .nw mid .sw;
	my $east = .ne mid .se;

	for Rect.new(nw => .nw, ne => $north, sw => $west, se => $center),
	    Rect.new(nw => $north, ne => .ne, sw => $center, se => $east),
	    Rect.new(nw => $west, ne => $center, sw => .sw, se => $south),
	    Rect.new(nw => $center, ne => $east, sw => $south, se => .se)
	{
	    my @bits = .nw.cbits, .ne.cbits, .sw.cbits, .se.cbits;

	    # if all 4 points wet by same circle, guaranteed wet
	    if [+&] @bits {
		$known-wet += .area;
		next;
	    }

	    # if all 4 corners are dry, must check further
	    if not [+|] @bits and $smallish {

		# check that no circle bulges into this rect
		my $ok = True;
		for @circles -> $c {
		    if .contains($c.east) or .contains($c.west) or
			.contains($c.north) or .contains($c.south)
		    {
			$ok = False;
			last;
		    }
		}
		if $ok {
		    $known-dry += .area;
		    next;
		}
	    }
	    push @unk, $_;	# dunno yet
	}
    }
    @unk;
}

my $delta = 0.001;
repeat until my $diff < $delta {
    @unknowns = divide(@unknowns);

    $diff = $outer-area - $known-dry - $known-wet;
    say 'div: ', $div.fmt('%-5d'),
	' unk: ', (+@unknowns).fmt('%-6d'),
	' est: ', ($known-wet + $diff/2).fmt('%9.6f'),
	' wet: ', $known-wet.fmt('%9.6f'),
	' dry: ', ($outer-area - $known-dry).fmt('%9.6f'),
	' diff: ', $diff.fmt('%9.6f'),
	' error: ', ($diff - @unknowns * @unknowns[0].area).fmt('%e');
}
```

```txt
div: 2     unk: 4      est: 14.607153 wet:  0.000000 dry: 29.214306 diff: 29.214306 error: 0.000000e+000
div: 4     unk: 15     est: 15.520100 wet:  1.825894 dry: 29.214306 diff: 27.388411 error: 0.000000e+000
div: 8     unk: 39     est: 20.313072 wet: 11.411838 dry: 29.214306 diff: 17.802467 error: 7.105427e-015
div: 16    unk: 107    est: 23.108972 wet: 17.003639 dry: 29.214306 diff: 12.210667 error: -1.065814e-014
div: 32    unk: 142    est: 21.368667 wet: 19.343066 dry: 23.394268 diff:  4.051203 error: 8.881784e-014
div: 64    unk: 290    est: 21.504182 wet: 20.469985 dry: 22.538380 diff:  2.068396 error: 1.771916e-013
div: 128   unk: 582    est: 21.534495 wet: 21.015613 dry: 22.053377 diff:  1.037764 error: -3.175238e-013
div: 256   unk: 1169   est: 21.557898 wet: 21.297343 dry: 21.818454 diff:  0.521111 error: -2.501332e-013
div: 512   unk: 2347   est: 21.563415 wet: 21.432636 dry: 21.694194 diff:  0.261558 error: -1.046996e-012
div: 1024  unk: 4700   est: 21.564111 wet: 21.498638 dry: 21.629584 diff:  0.130946 error: 1.481315e-013
div: 2048  unk: 9407   est: 21.564804 wet: 21.532043 dry: 21.597565 diff:  0.065522 error: 1.781700e-012
div: 4096  unk: 18818  est: 21.564876 wet: 21.548492 dry: 21.581260 diff:  0.032768 error: 1.098372e-011
div: 8192  unk: 37648  est: 21.564992 wet: 21.556797 dry: 21.573187 diff:  0.016389 error: -1.413968e-011
div: 16384 unk: 75301  est: 21.565017 wet: 21.560920 dry: 21.569115 diff:  0.008195 error: -7.683898e-011
div: 32768 unk: 150599 est: 21.565031 wet: 21.562982 dry: 21.567080 diff:  0.004097 error: -1.247991e-010
div: 65536 unk: 301203 est: 21.565035 wet: 21.564010 dry: 21.566059 diff:  0.002049 error: -2.830591e-010
div: 131072 unk: 602411 est: 21.565036 wet: 21.564524 dry: 21.565548 diff:  0.001024 error: -1.607121e-010
```

Here the "diff" is calculated by subtracting the known wet and dry areas from the total area, and the "error" is the difference between that and the sum of the areas of the unknown blocks, to give a rough idea of how much floating point roundoff error we've accumulated.


## Phix

```Phix
constant circles = {{ 1.6417233788,  1.6121789534, 0.0848270516},
                    {-1.4944608174,  1.2077959613, 1.1039549836},
                    { 0.6110294452, -0.6907087527, 0.9089162485},
                    { 0.3844862411,  0.2923344616, 0.2375743054},
                    {-0.2495892950, -0.3832854473, 1.0845181219},
                    { 1.7813504266,  1.6178237031, 0.8162655711},
                    {-0.1985249206, -0.8343333301, 0.0538864941},
                    {-1.7011985145, -0.1263820964, 0.4776976918},
                    {-0.4319462812,  1.4104420482, 0.7886291537},
                    { 0.2178372997, -0.9499557344, 0.0357871187},
                    {-0.6294854565, -1.3078893852, 0.7653357688},
                    { 1.7952608455,  0.6281269104, 0.2727652452},
                    { 1.4168575317,  1.0683357171, 1.1016025378},
                    { 1.4637371396,  0.9463877418, 1.1846214562},
                    {-0.5263668798,  1.7315156631, 1.4428514068},
                    {-1.2197352481,  0.9144146579, 1.0727263474},
                    {-0.1389358881,  0.1092805780, 0.7350208828},
                    { 1.5293954595,  0.0030278255, 1.2472867347},
                    {-0.5258728625,  1.3782633069, 1.3495508831},
                    {-0.1403562064,  0.2437382535, 1.3804956588},
                    { 0.8055826339, -0.0482092025, 0.3327165165},
                    {-0.6311979224,  0.7184578971, 0.2491045282},
                    { 1.4685857879, -0.8347049536, 1.3670667538},
                    {-0.6855727502,  1.6465021616, 1.0593087096},
                    { 0.0152957411,  0.0638919221, 0.9771215985}},
        {x,y,r} = columnize(circles),
        r2 = sq_power(r,2)

atom xMin = min(sq_sub(x,r)),
     xMax = max(sq_add(x,r)),
     yMin = min(sq_sub(y,r)),
     yMax = max(sq_add(y,r)),
     boxSide = 500,
     dx = (xMax - xMin) / boxSide,
     dy = (yMax - yMin) / boxSide,
     count = 0
sequence cxs = {}
for s=1 to boxSide do
    atom py = yMin + s * dy
    sequence cy = sq_power(sq_sub(py,y),2)
    for c=1 to boxSide do
        if s=1 then
            atom px = xMin + c * dx
            cxs = append(cxs,sq_power(sq_sub(px,x),2))
        end if
        sequence cx = cxs[c]
        for i=1 to length(circles) do
            if cx[i]+cy[i]<=r2[i] then count+=1 exit end if
        end for
    end for
end for
printf(1,"Approximate area = %.9f\n",{count * dx * dy})
```

```txt

Approximate area = 21.561559772

```



## Python


### Grid Sampling Version

This implements a regular grid sampling. For this problems this is more efficient than a Montecarlo sampling.

```python
from collections import namedtuple

Circle = namedtuple("Circle", "x y r")

circles = [
    Circle( 1.6417233788,  1.6121789534, 0.0848270516),
    Circle(-1.4944608174,  1.2077959613, 1.1039549836),
    Circle( 0.6110294452, -0.6907087527, 0.9089162485),
    Circle( 0.3844862411,  0.2923344616, 0.2375743054),
    Circle(-0.2495892950, -0.3832854473, 1.0845181219),
    Circle( 1.7813504266,  1.6178237031, 0.8162655711),
    Circle(-0.1985249206, -0.8343333301, 0.0538864941),
    Circle(-1.7011985145, -0.1263820964, 0.4776976918),
    Circle(-0.4319462812,  1.4104420482, 0.7886291537),
    Circle( 0.2178372997, -0.9499557344, 0.0357871187),
    Circle(-0.6294854565, -1.3078893852, 0.7653357688),
    Circle( 1.7952608455,  0.6281269104, 0.2727652452),
    Circle( 1.4168575317,  1.0683357171, 1.1016025378),
    Circle( 1.4637371396,  0.9463877418, 1.1846214562),
    Circle(-0.5263668798,  1.7315156631, 1.4428514068),
    Circle(-1.2197352481,  0.9144146579, 1.0727263474),
    Circle(-0.1389358881,  0.1092805780, 0.7350208828),
    Circle( 1.5293954595,  0.0030278255, 1.2472867347),
    Circle(-0.5258728625,  1.3782633069, 1.3495508831),
    Circle(-0.1403562064,  0.2437382535, 1.3804956588),
    Circle( 0.8055826339, -0.0482092025, 0.3327165165),
    Circle(-0.6311979224,  0.7184578971, 0.2491045282),
    Circle( 1.4685857879, -0.8347049536, 1.3670667538),
    Circle(-0.6855727502,  1.6465021616, 1.0593087096),
    Circle( 0.0152957411,  0.0638919221, 0.9771215985)]

def main():
    # compute the bounding box of the circles
    x_min = min(c.x - c.r for c in circles)
    x_max = max(c.x + c.r for c in circles)
    y_min = min(c.y - c.r for c in circles)
    y_max = max(c.y + c.r for c in circles)

    box_side = 500

    dx = (x_max - x_min) / box_side
    dy = (y_max - y_min) / box_side

    count = 0

    for r in xrange(box_side):
        y = y_min + r * dy
        for c in xrange(box_side):
            x = x_min + c * dx
            if any((x-circle.x)**2 + (y-circle.y)**2 <= (circle.r ** 2)
                   for circle in circles):
                count += 1

    print "Approximated area:", count * dx * dy

main()
```

```txt
Approximated area: 21.561559772
```



### Scanline Conversion


```python
from math import floor, ceil, sqrt

def area_scan(prec, circs):
    def sect((cx, cy, r), y):
        dr = sqrt(r ** 2 - (y - cy) ** 2)
        return (cx - dr, cx + dr)

    ys = [a[1] + a[2] for a in circs] + [a[1] - a[2] for a in circs]
    mins = int(floor(min(ys) / prec))
    maxs = int(ceil(max(ys) / prec))

    total = 0
    for y in (prec * x for x in xrange(mins, maxs + 1)):
        right = -float("inf")

        for (x0, x1) in sorted(sect((cx, cy, r), y)
                               for (cx, cy, r) in circs
                               if abs(y - cr) < r):
            if x1 <= right:
                continue
            total += x1 - max(x0, right)
            right = x1

    return total * prec

def main():
    circles = [
        ( 1.6417233788,  1.6121789534, 0.0848270516),
        (-1.4944608174,  1.2077959613, 1.1039549836),
        ( 0.6110294452, -0.6907087527, 0.9089162485),
        ( 0.3844862411,  0.2923344616, 0.2375743054),
        (-0.2495892950, -0.3832854473, 1.0845181219),
        ( 1.7813504266,  1.6178237031, 0.8162655711),
        (-0.1985249206, -0.8343333301, 0.0538864941),
        (-1.7011985145, -0.1263820964, 0.4776976918),
        (-0.4319462812,  1.4104420482, 0.7886291537),
        ( 0.2178372997, -0.9499557344, 0.0357871187),
        (-0.6294854565, -1.3078893852, 0.7653357688),
        ( 1.7952608455,  0.6281269104, 0.2727652452),
        ( 1.4168575317,  1.0683357171, 1.1016025378),
        ( 1.4637371396,  0.9463877418, 1.1846214562),
        (-0.5263668798,  1.7315156631, 1.4428514068),
        (-1.2197352481,  0.9144146579, 1.0727263474),
        (-0.1389358881,  0.1092805780, 0.7350208828),
        ( 1.5293954595,  0.0030278255, 1.2472867347),
        (-0.5258728625,  1.3782633069, 1.3495508831),
        (-0.1403562064,  0.2437382535, 1.3804956588),
        ( 0.8055826339, -0.0482092025, 0.3327165165),
        (-0.6311979224,  0.7184578971, 0.2491045282),
        ( 1.4685857879, -0.8347049536, 1.3670667538),
        (-0.6855727502,  1.6465021616, 1.0593087096),
        ( 0.0152957411,  0.0638919221, 0.9771215985)]

    p = 1e-3
    print "@stepsize", p, "area = %.4f" % area_scan(p, circles)

main()
```


### 2D Van der Corput sequence

[[File:Van_der_Corput_2D.png|200px|thumb|right]]
Remembering that the [[Van der Corput sequence]] is used for Monte Carlo-like simulations. This example uses a Van der Corput sequence generator of base 2 for the first dimension, and base 3 for the second dimension of the 2D space which seems to cover evenly.

To aid in efficiency:
* Circles are uniquified,
* Sorted in descending order of size,
* Wholly obscured circles removed,
* And the square of the radius computed outside the main loops.

```python
from __future__ import division
from math import sqrt
from itertools import count
from pprint import pprint as pp
try:
    from itertools import izip as zip
except:
    pass

# Remove duplicates and sort, largest first.
circles = sorted(set([
   #  xcenter       ycenter      radius
   (1.6417233788,  1.6121789534, 0.0848270516),
  (-1.4944608174,  1.2077959613, 1.1039549836),
   (0.6110294452, -0.6907087527, 0.9089162485),
   (0.3844862411,  0.2923344616, 0.2375743054),
  (-0.2495892950, -0.3832854473, 1.0845181219),
   (1.7813504266,  1.6178237031, 0.8162655711),
  (-0.1985249206, -0.8343333301, 0.0538864941),
  (-1.7011985145, -0.1263820964, 0.4776976918),
  (-0.4319462812,  1.4104420482, 0.7886291537),
   (0.2178372997, -0.9499557344, 0.0357871187),
  (-0.6294854565, -1.3078893852, 0.7653357688),
   (1.7952608455,  0.6281269104, 0.2727652452),
   (1.4168575317,  1.0683357171, 1.1016025378),
   (1.4637371396,  0.9463877418, 1.1846214562),
  (-0.5263668798,  1.7315156631, 1.4428514068),
  (-1.2197352481,  0.9144146579, 1.0727263474),
  (-0.1389358881,  0.1092805780, 0.7350208828),
   (1.5293954595,  0.0030278255, 1.2472867347),
  (-0.5258728625,  1.3782633069, 1.3495508831),
  (-0.1403562064,  0.2437382535, 1.3804956588),
   (0.8055826339, -0.0482092025, 0.3327165165),
  (-0.6311979224,  0.7184578971, 0.2491045282),
   (1.4685857879, -0.8347049536, 1.3670667538),
  (-0.6855727502,  1.6465021616, 1.0593087096),
   (0.0152957411,  0.0638919221, 0.9771215985),
   ]), key=lambda x: -x[-1])

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
    return sqrt((x2 - x1)**2 + (y2 - y1)**2) <= r1 - r2

def remove_covered_circles(circles):
    'Takes circles in decreasing radius order. Removes those covered by others'
    covered = []
    for i, c1 in enumerate(circles):
        eliminate = [c2 for c2 in circles[i+1:]
                        if circle_is_in_circle(c1, c2)]
        if eliminate: covered += [c1, eliminate]
        for c in eliminate: circles.remove(c)
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
        if any((px-cx)**2 + (py-cy)**2 <= cr2 for cx, cy, cr2 in circles2):
            inside += 1
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


if __name__ == '__main__':
    main(circles)
```

The above is tested to work with Python v.2.7, Python3 and PyPy.
```txt
python3 total_circle_area.py
Originally 25 circles
Bounding box: (-2.598415801, 2.8356525417, -2.2017717074, 3.1743670698999997)
  down to 14  due to some being wholly covered by others
Points:   100000, Area estimate: 21.57125892144117
Points:   200000, Area estimate: 21.565708203389384
Points:   300000, Area estimate: 21.56668201357391
Points:   400000, Area estimate: 21.566949811374652
Points:   500000, Area estimate: 21.567694776165812
Points:   600000, Area estimate: 21.566097727463195
Points:   700000, Area estimate: 21.565374325611838
Points:   800000, Area estimate: 21.565963828562822
Points:   900000, Area estimate: 21.56596788610526
The value has settled to 21.5660
```


;Comparison:
As mentioned on the [[Talk:Total_circles_area#Python_Van_der_Corput|talk page]], this version does more with its 200K points than the rectangular grid implementation does with its 250K points, (and the C coded Monte Carlo method does with 100 million points).


### Analytical Solution

This requires the dmath module. Because of the usage of Decimal and trigonometric functions implemented in Python, this program takes few minutes to run.
```python
from collections import namedtuple
from functools import partial
from itertools import repeat, imap, izip
from decimal import Decimal, getcontext

# Requires the egg: https://pypi.python.org/pypi/dmath/
from dmath import atan2, asin, sin, cos, pi as piCompute

getcontext().prec = 40 # Set FP precision.
sqrt = Decimal.sqrt
pi = piCompute()
D2 = Decimal(2)

Vec = namedtuple("Vec", "x y")
vcross = lambda (a, b), (c, d): a*d - b*c
vdot   = lambda (a, b), (c, d): a*c + b*d
vadd   = lambda (a, b), (c, d): Vec(a + c, b + d)
vsub   = lambda (a, b), (c, d): Vec(a - c, b - d)
vlen   = lambda x: sqrt(vdot(x, x))
vdist  = lambda a, b: vlen(vsub(a, b))
vscale = lambda s, (x, y): Vec(x * s, y * s)

def vnorm(v):
    l = vlen(v)
    return Vec(v.x / l, v.y / l)

vangle = lambda (x, y): atan2(y, x)

def anorm(a):
    if a > pi:  return a - pi * D2
    if a < -pi: return a + pi * D2
    return             a

Circle = namedtuple("Circle", "x y r")

def circle_cross((x0, y0, r0), (x1, y1, r1)):
    d = vdist(Vec(x0, y0), Vec(x1, y1))
    if d >= r0 + r1 or d <= abs(r0 - r1):
        return []

    s = (r0 + r1 + d) / D2
    a = sqrt(s * (s - d) * (s - r0) * (s - r1))
    h = D2 * a / d
    dr = Vec(x1 - x0, y1 - y0)
    dx = vscale(sqrt(r0 ** 2 - h ** 2), vnorm(dr))
    ang = vangle(dr) if \
          r0 ** 2 + d ** 2 > r1 ** 2 \
          else pi + vangle(dr)
    da = asin(h / r0)
    return map(anorm, [ang - da, ang + da])

# Angles of the start and end points of the circle arc.
Angle2 = namedtuple("Angle2", "a1 a2")

Arc = namedtuple("Arc", "c aa")

arcPoint = lambda (x, y, r), a: \
    vadd(Vec(x, y), Vec(r * cos(a), r * sin(a)))

arc_start  = lambda (c, (a0, a1)):  arcPoint(c, a0)
arc_mid    = lambda (c, (a0, a1)):  arcPoint(c, (a0 + a1) / D2)
arc_end    = lambda (c, (a0, a1)):  arcPoint(c, a1)
arc_center = lambda ((x, y, r), _): Vec(x, y)

arc_area = lambda ((_0, _1, r), (a0, a1)):  r ** 2 * (a1 - a0) / D2

def split_circles(cs):
    cSplit = lambda (c, angs): \
        imap(Arc, repeat(c), imap(Angle2, angs, angs[1:]))

    # If an arc that was part of one circle is inside *another* circle,
    # it will not be part of the zero-winding path, so reject it.
    in_circle = lambda ((x0, y0), c), (x, y, r): \
        c != Circle(x, y, r) and vdist(Vec(x0, y0), Vec(x, y)) < r

    def in_any_circle(arc):
        return any(in_circle((arc_mid(arc), arc.c), c) for c in cs)

    concat_map = lambda f, xs: [y for x in xs for y in f(x)]

    f = lambda c: \
        (c, sorted([-pi, pi] +
                   concat_map(partial(circle_cross, c), cs)))
    cAngs = map(f, cs)
    arcs = concat_map(cSplit, cAngs)
    return filter(lambda ar: not in_any_circle(ar), arcs)

# Given a list of arcs, build sets of closed paths from them.
# If one arc's end point is no more than 1e-4 from another's
# start point, they are considered connected.  Since these
# start/end points resulted from intersecting circles earlier,
# they *should* be exactly the same, but floating point
# precision may cause small differences, hence the 1e-4 error
# margin.  When there are genuinely different intersections
# closer than this margin, the method will backfire, badly.
def make_paths(arcs):
    eps = Decimal("0.0001")
    def join_arcs(a, xxs):
        if not xxs:
            return [a]
        x, xs = xxs[0], xxs[1:]
        if not a:
            return join_arcs([x], xs)
        if vdist(arc_start(a[0]), arc_end(a[-1])) < eps:
            return [a] + join_arcs([], xxs)
        if vdist(arc_end(a[-1]), arc_start(x)) < eps:
            return join_arcs(a + [x], xs)
        return join_arcs(a, xs + [x])
    return join_arcs([], arcs)

# Slice N-polygon into N-2 triangles.
def polyline_area(vvs):
    tri_area = lambda a, b, c: vcross(vsub(b, a), vsub(c, b)) / D2
    v, vs = vvs[0], vvs[1:]
    return sum(tri_area(v, v1, v2) for v1, v2 in izip(vs, vs[1:]))

def path_area(arcs):
    f = lambda (a, e), arc: \
        (a + arc_area(arc), e + [arc_center(arc), arc_end(arc)])
    (a, e) = reduce(f, arcs, (0, []))
    return a + polyline_area(e)

circles_area = lambda cs: \
    sum(imap(path_area, make_paths(split_circles(cs))))

def main():
    raw_circles = """\
         1.6417233788  1.6121789534 0.0848270516
        -1.4944608174  1.2077959613 1.1039549836
         0.6110294452 -0.6907087527 0.9089162485
         0.3844862411  0.2923344616 0.2375743054
        -0.2495892950 -0.3832854473 1.0845181219
         1.7813504266  1.6178237031 0.8162655711
        -0.1985249206 -0.8343333301 0.0538864941
        -1.7011985145 -0.1263820964 0.4776976918
        -0.4319462812  1.4104420482 0.7886291537
         0.2178372997 -0.9499557344 0.0357871187
        -0.6294854565 -1.3078893852 0.7653357688
         1.7952608455  0.6281269104 0.2727652452
         1.4168575317  1.0683357171 1.1016025378
         1.4637371396  0.9463877418 1.1846214562
        -0.5263668798  1.7315156631 1.4428514068
        -1.2197352481  0.9144146579 1.0727263474
        -0.1389358881  0.1092805780 0.7350208828
         1.5293954595  0.0030278255 1.2472867347
        -0.5258728625  1.3782633069 1.3495508831
        -0.1403562064  0.2437382535 1.3804956588
         0.8055826339 -0.0482092025 0.3327165165
        -0.6311979224  0.7184578971 0.2491045282
         1.4685857879 -0.8347049536 1.3670667538
        -0.6855727502  1.6465021616 1.0593087096
         0.0152957411  0.0638919221 0.9771215985""".splitlines()

    circles = [Circle(*imap(Decimal, row.split()))
               for row in raw_circles]
    print "Total Area:", circles_area(circles)

main()
```

```txt
Total Area: 21.56503660385639895908422492887814801839
```



## Racket


See: [[Example:Total_circles_area/Racket]]


## REXX

These REXX programs use the grid sampling method.

### using all circles


```rexx
/*REXX program calculates the total area of  (possibly overlapping)  circles.           */
parse arg box dig .                              /*obtain optional argument from the CL.*/
if box=='' | box==','  then box= 500             /*Not specified?  Then use the default.*/
if dig=='' | dig==','  then dig=  12             /* "      "         "   "   "     "    */
numeric digits dig                               /*have enough decimal digits for points*/
                       data = ' 1.6417233788   1.6121789534  0.0848270516',
                              '-1.4944608174   1.2077959613  1.1039549836',
                              ' 0.6110294452  -0.6907087527  0.9089162485',
                              ' 0.3844862411   0.2923344616  0.2375743054',
                              '-0.2495892950  -0.3832854473  1.0845181219',
                              ' 1.7813504266   1.6178237031  0.8162655711',
                              '-0.1985249206  -0.8343333301  0.0538864941',
                              '-1.7011985145  -0.1263820964  0.4776976918',
                              '-0.4319462812   1.4104420482  0.7886291537',
                              ' 0.2178372997  -0.9499557344  0.0357871187',
                              '-0.6294854565  -1.3078893852  0.7653357688',
                              ' 1.7952608455   0.6281269104  0.2727652452',
                              ' 1.4168575317   1.0683357171  1.1016025378',
                              ' 1.4637371396   0.9463877418  1.1846214562',
                              '-0.5263668798   1.7315156631  1.4428514068',
                              '-1.2197352481   0.9144146579  1.0727263474',
                              '-0.1389358881   0.1092805780  0.7350208828',
                              ' 1.5293954595   0.0030278255  1.2472867347',
                              '-0.5258728625   1.3782633069  1.3495508831',
                              '-0.1403562064   0.2437382535  1.3804956588',
                              ' 0.8055826339  -0.0482092025  0.3327165165',
                              '-0.6311979224   0.7184578971  0.2491045282',
                              ' 1.4685857879  -0.8347049536  1.3670667538',
                              '-0.6855727502   1.6465021616  1.0593087096',
                              ' 0.0152957411   0.0638919221  0.9771215985'
circles= words(data) % 3     /*     ══x══          ══y══       ══radius══     */
parse var data minX minY . 1 maxX maxY .              /*assign minimum & maximum values.*/
            do j=1  for circles;      _= j * 3  -  2  /*assign some circles with datum. */
            @x.j= word(data,_);    @y.j= word(data, _ + 1)
                                   @r.j= word(data,_+2);    @rr.j= @r.j**2
            minX= min(minX, @x.j - @r.j);   maxX= max(maxX, @x.j + @r.j)
            minY= min(minY, @y.j - @r.j);   maxY= max(maxY, @y.j + @r.j)
            end   /*j*/
dx= (maxX-minX) / box
dy= (maxY-minY) / box
#= 0                                                  /*count of sample points (so far).*/
      do   row=0  to box;          y= minY + row*dy   /*process each of the grid rows.  */
        do col=0  to box;          x= minX + col*dx   /*   "      "   "  "    "  column.*/
          do k=1  for circles                         /*now process each new circle.    */
          if (x - @x.k)**2 + (y - @y.k)**2 <= @rr.k  then  do;   #= # + 1;    leave;   end
          end   /*k*/
        end     /*col*/
      end       /*row*/
                                                      /*stick a fork in it, we're done. */
say 'Using ' box  " boxes (which have "  box**2  ' points)  and '  dig  " decimal digits,"
say 'the approximate area is: '       # * dx * dy
```

```txt

Using  500  boxes (which have  250000  points)  and  12  decimal digits,
the approximate area is:  21.5615597720

```



### optimized

This REXX version elides any circle that is completely contained in another circle.

Also, another optimization is the sorting of the circles by (descending) radii,

this reduces the computation time (for overlapping circles) by around 25%.

This, with other optimizations, makes this 2<sup>nd</sup> REXX version about twice as fast as the 1<sup>st</sup> REXX version.

This version also has additional information displayed.

```rexx
/*REXX program calculates the total area of  (possibly overlapping)  circles.           */
parse arg box dig .                              /*obtain optional argument from the CL.*/
if box=='' | box==','  then box= -500            /*Not specified?  Then use the default.*/
if dig=='' | dig==','  then dig=   12            /* "      "         "   "   "     "    */
verbose= box<0;    box= abs(box);  boxen= box+1  /*set a flag if we're in verbose mode. */
numeric digits dig                               /*have enough decimal digits for points*/

/* ══════x══════ ══════y══════ ═══radius═══     ══════x══════ ══════y══════ ═══radius═══*/
$=' 1.6417233788  1.6121789534 0.0848270516     -1.4944608174  1.2077959613 1.1039549836',
  ' 0.6110294452 -0.6907087527 0.9089162485      0.3844862411  0.2923344616 0.2375743054',
  '-0.2495892950 -0.3832854473 1.0845181219      1.7813504266  1.6178237031 0.8162655711',
  '-0.1985249206 -0.8343333301 0.0538864941     -1.7011985145 -0.1263820964 0.4776976918',
  '-0.4319462812  1.4104420482 0.7886291537      0.2178372997 -0.9499557344 0.0357871187',
  '-0.6294854565 -1.3078893852 0.7653357688      1.7952608455  0.6281269104 0.2727652452',
  ' 1.4168575317  1.0683357171 1.1016025378      1.4637371396  0.9463877418 1.1846214562',
  '-0.5263668798  1.7315156631 1.4428514068     -1.2197352481  0.9144146579 1.0727263474',
  '-0.1389358881  0.1092805780 0.7350208828      1.5293954595  0.0030278255 1.2472867347',
  '-0.5258728625  1.3782633069 1.3495508831     -0.1403562064  0.2437382535 1.3804956588',
  ' 0.8055826339 -0.0482092025 0.3327165165     -0.6311979224  0.7184578971 0.2491045282',
  ' 1.4685857879 -0.8347049536 1.3670667538     -0.6855727502  1.6465021616 1.0593087096',
  ' 0.0152957411  0.0638919221 0.9771215985 '         /*define circles with X, Y, and R.*/

circles= words($) % 3                                 /*figure out how many circles.    */
if verbose  then say 'There are'  circles  "circles." /*display the number of circles.  */
parse var  $   minX minY . 1 maxX maxY .              /*assign minimum & maximum values.*/

           do j=1  for circles;   _= j * 3  -  2      /*assign some circles with datum. */
           @x.j= word($, _);      @y.j=word($, _ + 1)
                                  @r.j=word($, _ + 2) / 1;         @rr.j= @r.j **2
           minX= min(minX, @x.j - @r.j);           maxX= max(maxX, @x.j + @r.j)
           minY= min(minY, @y.j - @r.j);           maxY= max(maxY, @y.j + @r.j)
           end   /*j*/

  do   m=1    for circles                             /*sort the circles by their radii.*/
    do n=m+1  to  circles                             /* [↓]  sort by  descending radii.*/
    if @r.n>@r.m then parse  value  @x.n @y.n @r.n   @x.m @y.m @r.m  with,
                                    @x.m @y.m @r.m   @x.n @y.n @r.n
    end   /*n*/                                       /* [↑]   Is it higher?  Then swap.*/
  end     /*m*/

dx= (maxX-minX) / box;        dy= (maxY-minY) / box   /*compute the  DX  and  DY  values*/
w= length(circles)                                    /*# in ►─ fully contained circles.*/
#in= 0
       do     j=1  for circles                        /*traipse through the  J  circles.*/
           do k=1  for circles;  if k==j | @r.j==0  then iterate  /*ignore self and/or 0*/
           if k==j | @r.j==0          then iterate    /*ignore self  and/or zero radius.*/
           if  @y.j+@r.j > @y.k+@r.k  |  @x.j-@r.j < @x.k-@r.k |,       /*is J inside K?*/
               @y.j-@r.j < @y.k-@r.k  |  @x.j+@r.j > @x.k+@r.k   then iterate
           if verbose  then say 'Circle ' right(j,w) ' is contained in circle ' right(k,w)
           @r.j= 0;             #in= #in + 1          /*elide this circle; and bump # in*/
           end   /*k*/
       end       /*j*/                                /* [↑]  elided overlapping circle.*/

if #in==0   then #in= 'no'                            /*use gooder English.  (humor).   */
if verbose  then do; say; say #in " circles are fully contained within other circles.";end
nC=0                                                  /*number of  "new"  circles.      */
           do n=1  for circles;   if @r.n==0  then iterate               /*skip if zero.*/
           nC=nC+1;  @x.nC=@x.n;  @y.nC=@y.n;  @r.nC=@r.n;  @rr.nC=@r.n**2
           end   /*n*/                                /* [↑]  elide overlapping circles.*/
#=0                                                   /*count of sample points (so far).*/
           do   row=0  for boxen;   y=minY + row*dy   /*process each of the grid row.   */
             do col=0  for boxen;   x=minX + col*dx   /*   "      "   "  "    "  column.*/
               do k=1  for nC                         /*now process each new circle.    */
               if (x - @x.k)**2 + (y - @y.k)**2 <= @rr.k  then  do;  #= #+1;  leave;   end
               end   /*k*/
             end     /*col*/
           end       /*row*/
say                                                   /*stick a fork in it, we're done. */
say 'Using ' box  " boxes (which have "  box**2  ' points)  and '  dig  " decimal digits,"
say 'the approximate area is: '  #*dx*dy
```

[Output shown is a combination of several runs.]

```txt

There are 25 circles.
Circle   1  is contained in circle   6
Circle   4  is contained in circle   5
Circle   7  is contained in circle   3
Circle   9  is contained in circle  15
Circle  10  is contained in circle   3
Circle  12  is contained in circle  13
Circle  17  is contained in circle  20
Circle  21  is contained in circle  18
Circle  22  is contained in circle  15
Circle  24  is contained in circle  15
Circle  25  is contained in circle  20
11  circles are fully contained within other circles.

Using  125  boxes (which have  15625  points)  and  12  decimal digits,
the approximate area is:  21.5353837543

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

Using  250  boxes (which have  62500  points)  and  12  decimal digits,
the approximate area is:  21.5536134809

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

Using  500  boxes (which have  250000  points)  and  12  decimal digits,
the approximate area is:  21.5615597720

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

Using  1000  boxes (which have  1000000  points)  and  12  decimal digits,
the approximate area is:  21.5638384878

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

Using  2000  boxes (which have  4000000  points)  and  12  decimal digits,
the approximate area is:  21.5646564884

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

Using  4000  boxes (which have  16000000  points)  and  12  decimal digits,
the approximate area is:  21.5649121136

░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

Using  8000  boxes (which have  64000000  points)  and  12  decimal digits,
the approximate area is:  21.5650301120

```



## Ruby

Common code

```ruby
circles = [
  [ 1.6417233788,  1.6121789534, 0.0848270516],
  [-1.4944608174,  1.2077959613, 1.1039549836],
  [ 0.6110294452, -0.6907087527, 0.9089162485],
  [ 0.3844862411,  0.2923344616, 0.2375743054],
  [-0.2495892950, -0.3832854473, 1.0845181219],
  [ 1.7813504266,  1.6178237031, 0.8162655711],
  [-0.1985249206, -0.8343333301, 0.0538864941],
  [-1.7011985145, -0.1263820964, 0.4776976918],
  [-0.4319462812,  1.4104420482, 0.7886291537],
  [ 0.2178372997, -0.9499557344, 0.0357871187],
  [-0.6294854565, -1.3078893852, 0.7653357688],
  [ 1.7952608455,  0.6281269104, 0.2727652452],
  [ 1.4168575317,  1.0683357171, 1.1016025378],
  [ 1.4637371396,  0.9463877418, 1.1846214562],
  [-0.5263668798,  1.7315156631, 1.4428514068],
  [-1.2197352481,  0.9144146579, 1.0727263474],
  [-0.1389358881,  0.1092805780, 0.7350208828],
  [ 1.5293954595,  0.0030278255, 1.2472867347],
  [-0.5258728625,  1.3782633069, 1.3495508831],
  [-0.1403562064,  0.2437382535, 1.3804956588],
  [ 0.8055826339, -0.0482092025, 0.3327165165],
  [-0.6311979224,  0.7184578971, 0.2491045282],
  [ 1.4685857879, -0.8347049536, 1.3670667538],
  [-0.6855727502,  1.6465021616, 1.0593087096],
  [ 0.0152957411,  0.0638919221, 0.9771215985],
]

def minmax_circle(circles)
  xmin = circles.map {|xc, yc, radius| xc - radius}.min
  xmax = circles.map {|xc, yc, radius| xc + radius}.max
  ymin = circles.map {|xc, yc, radius| yc - radius}.min
  ymax = circles.map {|xc, yc, radius| yc + radius}.max
  [xmin, xmax, ymin, ymax]
end

# remove internal circle
def select_circle(circles)
  circles = circles.sort_by{|cx,cy,r| -r}
  size = circles.size
  select = [*0...size]
  for i in 0...size-1
    xi,yi,ri = circles[i].to_a
    for j in i+1...size
      xj,yj,rj = circles[j].to_a
      select -= [j]  if (xi-xj)**2 + (yi-yj)**2 <= (ri-rj)**2
    end
  end
  circles.values_at(*select)
end
circles = select_circle(circles)
```



### Grid Sampling

```ruby
def grid_sample(circles, box_side=500)
  # compute the bounding box of the circles
  xmin, xmax, ymin, ymax = minmax_circle(circles)

  dx = (xmax - xmin) / box_side
  dy = (ymax - ymin) / box_side

  circle2 = circles.map{|cx,cy,r| [cx,cy,r*r]}
  include = ->(x,y){circle2.any?{|cx, cy, r2| (x-cx)**2 + (y-cy)**2 < r2}}
  count = 0

  box_side.times do |r|
    y = ymin + r * dy
    box_side.times do |c|
      x = xmin + c * dx
      count += 1  if include[x,y]
    end
  end
  #puts box_side => "Approximated area:#{count * dx * dy}"
  count * dx * dy
end

puts "Grid Sample"
n = 500
2.times do
  t0 = Time.now
  puts "Approximated area:#{grid_sample(circles, n)} (#{n} grid)"
  n *= 2
  puts "#{Time.now - t0} sec"
end
```


```txt

Grid Sample
Approximated area:21.561559772003317 (500 grid)
1.427082 sec
Approximated area:21.5638384878351 (1000 grid)
5.661324 sec

```



### Scanline Method

```ruby
def area_scan(prec, circles)
  sect = ->(y) do
    circles.select{|cx,cy,r| (y - cy).abs < r}.map do |cx,cy,r|
      dr = Math.sqrt(r ** 2 - (y - cy) ** 2)
      [cx - dr, cx + dr]
    end
  end
  xmin, xmax, ymin, ymax = minmax_circle(circles)
  ymin = (ymin / prec).floor
  ymax = (ymax / prec).ceil

  total = 0
  for y in ymin..ymax
    y *= prec
    right = xmin
    for x0, x1 in sect[y].sort
      next  if x1 <= right
      total += x1 - [x0, right].max
      right = x1
    end
  end
  total * prec
end

puts "Scanline Method"
prec = 1e-2
3.times do
  t0 = Time.now
  puts "%8.6f : %12.9f, %p sec" % [prec, area_scan(prec, circles), Time.now-t0]
  prec /= 10
end
```


```txt

Scanline Method
0.010000 : 21.566169251, 0.012001 sec
0.001000 : 21.565049562, 0.116006 sec
0.000100 : 21.565036221, 1.160066 sec

```



## Tcl

This presents three techniques, a regular grid sampling, a pure Monte Carlo sampling, and a technique where there is a regular grid but then a vote of uniformly-distributed samples within each grid cell is taken to see if the cell is “in” or “out”.

```tcl
set circles {
     1.6417233788  1.6121789534 0.0848270516
    -1.4944608174  1.2077959613 1.1039549836
     0.6110294452 -0.6907087527 0.9089162485
     0.3844862411  0.2923344616 0.2375743054
    -0.2495892950 -0.3832854473 1.0845181219
     1.7813504266  1.6178237031 0.8162655711
    -0.1985249206 -0.8343333301 0.0538864941
    -1.7011985145 -0.1263820964 0.4776976918
    -0.4319462812  1.4104420482 0.7886291537
     0.2178372997 -0.9499557344 0.0357871187
    -0.6294854565 -1.3078893852 0.7653357688
     1.7952608455  0.6281269104 0.2727652452
     1.4168575317  1.0683357171 1.1016025378
     1.4637371396  0.9463877418 1.1846214562
    -0.5263668798  1.7315156631 1.4428514068
    -1.2197352481  0.9144146579 1.0727263474
    -0.1389358881  0.1092805780 0.7350208828
     1.5293954595  0.0030278255 1.2472867347
    -0.5258728625  1.3782633069 1.3495508831
    -0.1403562064  0.2437382535 1.3804956588
     0.8055826339 -0.0482092025 0.3327165165
    -0.6311979224  0.7184578971 0.2491045282
     1.4685857879 -0.8347049536 1.3670667538
    -0.6855727502  1.6465021616 1.0593087096
     0.0152957411  0.0638919221 0.9771215985
}

proc init {} {
    upvar 1 circles circles
    set xMin [set yMin inf]
    set xMax [set yMax -inf]
    set i -1
    foreach {xc yc rad} $circles {
	set xMin [expr {min($xMin, $xc-$rad)}]
	set xMax [expr {max($xMax, $xc+$rad)}]
	set yMin [expr {min($yMin, $yc-$rad)}]
	set yMax [expr {max($yMax, $yc+$rad)}]
	lset circles [incr i 3] [expr {$rad**2}]
    }
    return [list $xMin $xMax $yMin $yMax]
}

proc sampleGrid {circles steps} {
    lassign [init] xMin xMax yMin yMax
    set dx [expr {($xMax-$xMin)/$steps}]
    set dy [expr {($yMax-$yMin)/$steps}]
    set n 0
    for {set i 0} {$i < $steps} {incr i} {
	set x [expr {$xMin + $i * $dx}]
	for {set j 0} {$j < $steps} {incr j} {
	    set y [expr {$yMin + $j * $dy}]
	    foreach {xc yc rad2} $circles {
		if {($x-$xc)**2 + ($y-$yc)**2 <= $rad2} {
		    incr n
		    break
		}
	    }
	}
    }
    return [expr {$dx * $dy * $n}]
}

proc sampleMC {circles samples} {
    lassign [init] xMin xMax yMin yMax
    set n 0
    for {set i 0} {$i < $samples} {incr i} {
	set x [expr {$xMin+rand()*($xMax-$xMin)}]
	set y [expr {$yMin+rand()*($yMax-$yMin)}]
	foreach {xc yc rad2} $circles {
	    if {($x-$xc)**2 + ($y-$yc)**2 <= $rad2} {
		incr n
		break
	    }
	}
    }
    return [expr {($xMax-$xMin) * ($yMax-$yMin) * $n / $samples}]
}

proc samplePerturb {circles steps votes} {
    lassign [init] xMin xMax yMin yMax
    set dx [expr {($xMax-$xMin)/$steps}]
    set dy [expr {($yMax-$yMin)/$steps}]
    set n 0
    for {set i 0} {$i < $steps} {incr i} {
	set x [expr {$xMin + $i * $dx}]
	for {set j 0} {$j < $steps} {incr j} {
	    set y [expr {$yMin + $j * $dy}]
	    foreach {xc yc rad2} $circles {
		set in 0
		for {set v 0} {$v < $votes} {incr v} {
		    set xr [expr {$x + (rand()-0.5)*$dx}]
		    set yr [expr {$y + (rand()-0.5)*$dy}]
		    if {($xr-$xc)**2 + ($yr-$yc)**2 <= $rad2} {
			incr in
		    }
		}
		if {$in*2 >= $votes} {
		    incr n
		    break
		}
	    }
	}
    }
    return [expr {$dx * $dy * $n}]
}

puts [format "estimated area (grid): %.4f" [sampleGrid $circles 500]]
puts [format "estimated area (monte carlo): %.2f" [sampleMC $circles 1000000]]
puts [format "estimated area (perturbed sample): %.4f" [samplePerturb $circles 500 5]]
```

```txt

estimated area (grid): 21.5616
estimated area (monte carlo): 21.56
estimated area (perturbed sample): 21.5645

```

Note that the error on the Monte Carlo sampling is actually very high; the above run happened to deliver a figure closer to the real value than usual.


## VBA

Analytical solution adapted from Haskell/Python.

```vb
Public c As Variant
Public pi As Double
Dim arclists() As Variant
Public Enum circles_
    xc = 0
    yc
    rc
End Enum
Public Enum arclists_
    rho
    x_
    y_
    i_
End Enum
Public Enum shoelace_axis
    u = 0
    v
End Enum
Private Sub give_a_list_of_circles()
    c = Array(Array(1.6417233788, 1.6121789534, 0.0848270516), _
    Array(-1.4944608174, 1.2077959613, 1.1039549836), _
    Array(0.6110294452, -0.6907087527, 0.9089162485), _
    Array(0.3844862411, 0.2923344616, 0.2375743054), _
    Array(-0.249589295, -0.3832854473, 1.0845181219), _
    Array(1.7813504266, 1.6178237031, 0.8162655711), _
    Array(-0.1985249206, -0.8343333301, 0.0538864941), _
    Array(-1.7011985145, -0.1263820964, 0.4776976918), _
    Array(-0.4319462812, 1.4104420482, 0.7886291537), _
    Array(0.2178372997, -0.9499557344, 0.0357871187), _
    Array(-0.6294854565, -1.3078893852, 0.7653357688), _
    Array(1.7952608455, 0.6281269104, 0.2727652452), _
    Array(1.4168575317, 1.0683357171, 1.1016025378), _
    Array(1.4637371396, 0.9463877418, 1.1846214562), _
    Array(-0.5263668798, 1.7315156631, 1.4428514068), _
    Array(-1.2197352481, 0.9144146579, 1.0727263474), _
    Array(-0.1389358881, 0.109280578, 0.7350208828), _
    Array(1.5293954595, 0.0030278255, 1.2472867347), _
    Array(-0.5258728625, 1.3782633069, 1.3495508831), _
    Array(-0.1403562064, 0.2437382535, 1.3804956588), _
    Array(0.8055826339, -0.0482092025, 0.3327165165), _
    Array(-0.6311979224, 0.7184578971, 0.2491045282), _
    Array(1.4685857879, -0.8347049536, 1.3670667538), _
    Array(-0.6855727502, 1.6465021616, 1.0593087096), _
    Array(0.0152957411, 0.0638919221, 0.9771215985))
    pi = WorksheetFunction.pi()
End Sub
Private Function shoelace(s As Collection) As Double
    's is a collection of coordinate pairs (x, y),
    'in clockwise order for positive result.
    'The last pair is identical to the first pair.
    'These pairs map a polygonal area.
    'The area is computed with the shoelace algoritm.
    'see the Rosetta Code task.
    Dim t As Double
    If s.Count > 2 Then
        s.Add s(1)
        For i = 1 To s.Count - 1
            t = t + s(i + 1)(u) * s(i)(v) - s(i)(u) * s(i + 1)(v)
        Next i
    End If
    shoelace = t / 2
End Function
Private Sub arc_sub(acol As Collection, f0 As Double, u0 As Double, v0 As Double, _
    f1 As Double, u1 As Double, v1 As Double, this As Integer, j As Integer)
    'subtract the arc from f0 to f1 from the arclist acol
    'complicated to deal with edge cases
    If acol.Count = 0 Then Exit Sub 'nothing to subtract from
    Debug.Assert acol.Count Mod 2 = 0
    Debug.Assert f0 <> f1
    If f1 = pi Or f1 + pi < 5E-16 Then f1 = -f1
    If f0 = pi Or f0 + pi < 5E-16 Then f0 = -f0
    If f0 < f1 Then
        'the arc does not pass the negative x-axis
        'find a such that acol(a)(0)<f0<acol(a+1)(0)
        ' and b such that acol(b)(0)<f1<acol(b+1)(0)
        If f1 < acol(1)(rho) Or f0 > acol(acol.Count)(rho) Then Exit Sub 'nothing to subtract
        i = acol.Count + 1
        start = 1
        Do
            i = i - 1
        Loop Until f1 > acol(i)(rho)
        If i Mod 2 = start Then
            acol.Add Array(f1, u1, v1, j), after:=i
        End If
        i = 0
        Do
            i = i + 1
        Loop Until f0 < acol(i)(rho)
        If i Mod 2 = 1 - start Then
            acol.Add Array(f0, u0, v0, j), before:=i
            i = i + 1
        End If
        Do While acol(i)(rho) < f1
            acol.Remove i
            If i > acol.Count Then Exit Do
        Loop
    Else
        start = 1
        If f0 > acol(1)(rho) Then
            i = acol.Count + 1
            Do
                i = i - 1
            Loop While f0 < acol(i)(0)
            If f0 = pi Then
                acol.Add Array(f0, u0, v0, j), before:=i
            Else
                If i Mod 2 = start Then
                    acol.Add Array(f0, u0, v0, j), after:=i
                End If
            End If
        End If
        If f1 <= acol(acol.Count)(rho) Then
            i = 0
            Do
                i = i + 1
            Loop While f1 > acol(i)(rho)
            If f1 + pi < 5E-16 Then
                acol.Add Array(f1, u1, v1, j), after:=i
            Else
                If i Mod 2 = 1 - start Then
                    acol.Add Array(f1, u1, v1, j), before:=i
                End If
            End If
        End If
        Do While acol(acol.Count)(rho) > f0 Or acol(acol.Count)(i_) = -1
            acol.Remove acol.Count
            If acol.Count = 0 Then Exit Do
        Loop
        If acol.Count > 0 Then
            Do While acol(1)(rho) < f1 Or (f1 = -pi And acol(1)(i_) = this)
                acol.Remove 1
                If acol.Count = 0 Then Exit Do
            Loop
        End If
    End If
End Sub
Private Sub circle_cross()
    ReDim arclists(LBound(c) To UBound(c))
    Dim alpha As Double, beta As Double
    Dim x3 As Double, x4 As Double, y3 As Double, y4 As Double
    Dim i As Integer, j As Integer
    For i = LBound(c) To UBound(c)
        Dim arccol As New Collection
        'arccol is a collection or surviving arcs of circle i.
        'It starts with the full circle. The collection
        'alternates between start and ending angles of the arcs.
        'This winds counter clockwise.
        'Noted are angle, x coordinate, y coordinate and
        'index number of circles with which circle i
        'intersects at that angle and -1 marks visited. This defines
        'ultimately a double linked list. So winding
        'clockwise in the end is easy.
        arccol.Add Array(-pi, c(i)(xc) - c(i)(r), c(i)(yc), i)
        arccol.Add Array(pi, c(i)(xc) - c(i)(r), c(i)(yc), -1)
        For j = LBound(c) To UBound(c)
            If i <> j Then
                x0 = c(i)(xc)
                y0 = c(i)(yc)
                r0 = c(i)(rc)
                x1 = c(j)(xc)
                y1 = c(j)(yc)
                r1 = c(j)(rc)
                d = Sqr((x0 - x1) ^ 2 + (y0 - y1) ^ 2)
                'Ignore 0 and 1, we need only the 2 case.
                If d >= r0 + r1 Or d <= Abs(r0 - r1) Then
                    'no intersections
                Else
                    a = (r0 ^ 2 - r1 ^ 2 + d ^ 2) / (2 * d)
                    h = Sqr(r0 ^ 2 - a ^ 2)
                    x2 = x0 + a * (x1 - x0) / d
                    y2 = y0 + a * (y1 - y0) / d
                    x3 = x2 + h * (y1 - y0) / d
                    y3 = y2 - h * (x1 - x0) / d
                    alpha = WorksheetFunction.Atan2(x3 - x0, y3 - y0)
                    x4 = x2 - h * (y1 - y0) / d
                    y4 = y2 + h * (x1 - x0) / d
                    beta = WorksheetFunction.Atan2(x4 - x0, y4 - y0)
                    'alpha is counterclockwise positioned w.r.t beta
                    'so the arc from beta to alpha (ccw) has to be
                    'subtracted from the list of surviving arcs as
                    'this arc lies fully in circle j
                    arc_sub arccol, alpha, x3, y3, beta, x4, y4, i, j
                End If
            End If
        Next j
        Set arclists(i) = arccol
        Set arccol = Nothing
    Next i
End Sub
Private Sub make_path()
    Dim pathcol As New Collection, arcsum As Double
    i0 = UBound(arclists)
    finished = False
    Do While True
        arcsum = 0
        Do While arclists(i0).Count = 0
            i0 = i0 - 1
        Loop
        j0 = arclists(i0).Count
        next_i = i0
        next_j = j0
        Do While True
            x = arclists(next_i)(next_j)(x_)
            y = arclists(next_i)(next_j)(y_)
            pathcol.Add Array(x, y)
            prev_i = next_i
            prev_j = next_j
            If arclists(next_i)(next_j - 1)(i_) = next_i Then
                'skip the join point at the negative x-axis
                next_j = arclists(next_i).Count - 1
                If next_j = 1 Then Exit Do 'loose full circle arc
            Else
                next_j = next_j - 1
            End If
            '------------------------------
            r = c(next_i)(rc)
            a1 = arclists(next_i)(prev_j)(rho)
            a2 = arclists(next_i)(next_j)(rho)
            If a1 > a2 Then
                alpha = a1 - a2
            Else
                alpha = 2 * pi - a2 + a1
            End If
            arcsum = arcsum + r * r * (alpha - Sin(alpha)) / 2
            '------------------------------
            next_i = arclists(next_i)(next_j)(i_)
            next_j = arclists(next_i).Count
            If next_j = 0 Then Exit Do 'skip loose arcs
            Do While arclists(next_i)(next_j)(i_) <> prev_i
                'find the matching item
                next_j = next_j - 1
            Loop
            If next_i = i0 And next_j = j0 Then
                finished = True
                Exit Do
            End If
        Loop
        If finished Then Exit Do
        i0 = i0 - 1
        Set pathcol = Nothing
    Loop
    Debug.Print shoelace(pathcol) + arcsum
End Sub
Public Sub total_circles()
    give_a_list_of_circles
    circle_cross
    make_path
End Sub
```
```txt
 21,5650366038564
```


## zkl

The circle data is stored in a file, just a copy/paste of the task data.
```zkl
circles:=File("circles.txt").pump(List,'wrap(line){
   line.split().apply("toFloat")  // L(x,y,r)
});
    # compute the bounding box of the circles
x_min:=(0.0).min(circles.apply(fcn([(x,y,r)]){ x - r })); // (0) not used, just the list of numbers
x_max:=(0.0).max(circles.apply(fcn([(x,y,r)]){ x + r }));
y_min:=(0.0).min(circles.apply(fcn([(x,y,r)]){ y - r }));
y_max:=(0.0).max(circles.apply(fcn([(x,y,r)]){ y + r }));

box_side:=500;
dx:=(x_max - x_min)/box_side;
dy:=(y_max - y_min)/box_side;

count:=0;
foreach r in (box_side){
   y:=y_min + dy*r;
   foreach c in (box_side){
      x:=x_min + dx*c;
      count+=circles.filter1('wrap([(cx,cy,cr)]){
         x-=cx; y-=cy;
      	 x*x + y*y <= cr*cr
      }).toBool(); // -->False|L(x,y,z), L(x,y,r).toBool()-->True,
   }
}

println("Approximated area: ", dx*dy*count);
```

```txt
Approximated area: 21.5616
```


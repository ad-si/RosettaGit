+++
title = "K-means++ clustering"
description = ""
date = 2019-08-31T05:55:39Z
aliases = []
[extra]
id = 9798
[taxonomies]
categories = ["task", "Probability and statistics"]
tags = []
languages = [
  "c",
  "d",
  "euler_math_toolbox",
  "fortran",
  "go",
  "haskell",
  "huginn",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rust",
  "scheme",
  "sequence_l",
  "tcl",
  "xpl0",
]
+++

## Task

[[WP:K-means%2B%2B|K-means++ clustering]] a classification of data, so that points assigned to the same cluster are similar (in some sense).  It is identical to the [[WP:K-means_clustering|K-means]] algorithm, except for the selection of initial conditions.  [[Image:CircleClusters.png|thumb|right|alt=A circular distribution of data partitioned into 7 colored clusters; similar to the top of a beach ball|This data was partitioned into 7 clusters using the [[WP:K-means_clustering|K-means algorithm]].]]

The task is to '''''implement the K-means++ algorithm'''''.  Produce a function which takes two arguments: the number of clusters K, and the dataset to classify.  K is a positive integer and the dataset is a list of points in the Cartesian plane.  The output is a list of clusters (related sets of points, according to the algorithm).

For '''''extra credit''''' (in order):
# Provide a function to exercise your code, which generates a list of random points.
# Provide a visualization of your results, including centroids (see example image).
# Generalize the function to polar coordinates (in radians).
# Generalize the function to points in an arbitrary N space (i.e. instead of x,y pairs, points are an N-tuples representing coordinates in ℝ<sup>N</sup>). <BR>''If this is different or more difficult than the [naive] solution for ℝ<sup>2</sup>, discuss what had to change to support N dimensions.''

Extra credit is only awarded if the examples given demonstrate the feature in question. To earn credit for 1. and 2., visualize 6 clusters of 30,000 points in ℝ<sup>2</sup>.  It is not necessary to provide visualization for spaces higher than ℝ<sup>2</sup> but the examples should demonstrate features 3. and 4. if the solution has them.


## C

[[file:k++-C.png|center]]
Output is in EPS. 100,000 point EPS file can take quite a while to display.

To extend the code to handle dimensions higher than 2, just make <code>point_t</code> have more coordinates, and change the <code>dist2</code> distance function (and the point generation, or course) accordingly.  Neither is difficult to do, but it would make code somewhat longer and less to the point of the demonstrating the algorithm, so I chose simplicity and clarity over extra credit.

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct { double x, y; int group; } point_t, *point;

double randf(double m)
{
	return m * rand() / (RAND_MAX - 1.);
}

point gen_xy(int count, double radius)
{
	double ang, r;
	point p, pt = malloc(sizeof(point_t) * count);

	/* note: this is not a uniform 2-d distribution */
	for (p = pt + count; p-- > pt;) {
		ang = randf(2 * M_PI);
		r = randf(radius);
		p->x = r * cos(ang);
		p->y = r * sin(ang);
	}

	return pt;
}

inline double dist2(point a, point b)
{
	double x = a->x - b->x, y = a->y - b->y;
	return x*x + y*y;
}

inline int
nearest(point pt, point cent, int n_cluster, double *d2)
{
	int i, min_i;
	point c;
	double d, min_d;

#	define for_n for (c = cent, i = 0; i < n_cluster; i++, c++)
	for_n {
		min_d = HUGE_VAL;
		min_i = pt->group;
		for_n {
			if (min_d > (d = dist2(c, pt))) {
				min_d = d; min_i = i;
			}
		}
	}
	if (d2) *d2 = min_d;
	return min_i;
}

void kpp(point pts, int len, point cent, int n_cent)
{
#	define for_len for (j = 0, p = pts; j < len; j++, p++)
	int i, j;
	int n_cluster;
	double sum, *d = malloc(sizeof(double) * len);

	point p, c;
	cent[0] = pts[ rand() % len ];
	for (n_cluster = 1; n_cluster < n_cent; n_cluster++) {
		sum = 0;
		for_len {
			nearest(p, cent, n_cluster, d + j);
			sum += d[j];
		}
		sum = randf(sum);
		for_len {
			if ((sum -= d[j]) > 0) continue;
			cent[n_cluster] = pts[j];
			break;
		}
	}
	for_len p->group = nearest(p, cent, n_cluster, 0);
	free(d);
}

point lloyd(point pts, int len, int n_cluster)
{
	int i, j, min_i;
	int changed;

	point cent = malloc(sizeof(point_t) * n_cluster), p, c;

	/* assign init grouping randomly */
	//for_len p->group = j % n_cluster;

	/* or call k++ init */
	kpp(pts, len, cent, n_cluster);

	do {
		/* group element for centroids are used as counters */
		for_n { c->group = 0; c->x = c->y = 0; }
		for_len {
			c = cent + p->group;
			c->group++;
			c->x += p->x; c->y += p->y;
		}
		for_n { c->x /= c->group; c->y /= c->group; }

		changed = 0;
		/* find closest centroid of each point */
		for_len {
			min_i = nearest(p, cent, n_cluster, 0);
			if (min_i != p->group) {
				changed++;
				p->group = min_i;
			}
		}
	} while (changed > (len >> 10)); /* stop when 99.9% of points are good */

	for_n { c->group = i; }

	return cent;
}

void print_eps(point pts, int len, point cent, int n_cluster)
{
#	define W 400
#	define H 400
	int i, j;
	point p, c;
	double min_x, max_x, min_y, max_y, scale, cx, cy;
	double *colors = malloc(sizeof(double) * n_cluster * 3);

	for_n {
		colors[3*i + 0] = (3 * (i + 1) % 11)/11.;
		colors[3*i + 1] = (7 * i % 11)/11.;
		colors[3*i + 2] = (9 * i % 11)/11.;
	}

	max_x = max_y = -(min_x = min_y = HUGE_VAL);
	for_len {
		if (max_x < p->x) max_x = p->x;
		if (min_x > p->x) min_x = p->x;
		if (max_y < p->y) max_y = p->y;
		if (min_y > p->y) min_y = p->y;
	}
	scale = W / (max_x - min_x);
	if (scale > H / (max_y - min_y)) scale = H / (max_y - min_y);
	cx = (max_x + min_x) / 2;
	cy = (max_y + min_y) / 2;

	printf("%%!PS-Adobe-3.0\n%%%%BoundingBox: -5 -5 %d %d\n", W + 10, H + 10);
	printf( "/l {rlineto} def /m {rmoveto} def\n"
		"/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n"
		"/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath "
		"	gsave 1 setgray fill grestore gsave 3 setlinewidth"
		" 1 setgray stroke grestore 0 setgray stroke }def\n"
	);
	for_n {
		printf("%g %g %g setrgbcolor\n",
			colors[3*i], colors[3*i + 1], colors[3*i + 2]);
		for_len {
			if (p->group != i) continue;
			printf("%.3f %.3f c\n",
				(p->x - cx) * scale + W / 2,
				(p->y - cy) * scale + H / 2);
		}
		printf("\n0 setgray %g %g s\n",
			(c->x - cx) * scale + W / 2,
			(c->y - cy) * scale + H / 2);
	}
	printf("\n%%%%EOF");
	free(colors);
#	undef for_n
#	undef for_len
}

#define PTS 100000
#define K 11
int main()
{
	int i;
	point v = gen_xy(PTS, 10);
	point c = lloyd(v, PTS, K);
	print_eps(v, PTS, c, K);
	// free(v); free(c);
	return 0;
}
```


## D

```d
import std.stdio, std.math, std.random, std.typecons, std.algorithm;

// On Windows this uses the printf from the Microsoft C runtime,
// that doesn't handle real type and some of the C99 format
// specifiers, but it's faster for bulk printing.
extern(C) nothrow int printf(const char*, ...);

struct Point {
    immutable double x, y; // Or float.
    size_t cluster;
}

Point[] generatePoints(in size_t nPoints,
                       in double radius,
                       ref Xorshift rnd)
in {
    assert(nPoints > 0);
    assert(radius > 0);
} out(result) {
    assert(result.length == nPoints);
    foreach (const ref p; result) {
        assert(p.cluster == 0);
        assert(!p.x.isNaN && !p.y.isNaN);
    }
} body {
    Point[] points;
    points.reserve(nPoints);

    // This is not a uniform 2D distribution.
    foreach (immutable i; 0 .. nPoints) {
        immutable r = uniform(0.0, radius, rnd);
        immutable ang = uniform(0.0, 2 * PI, rnd);
        points ~= Point(r * ang.cos, r * ang.sin); // Sincos?
    }

    return points;
}


struct ClusterCenter {
    double x, y;
    void opAssign(in ref Point p) pure nothrow @nogc {
        this.x = p.x;
        this.y = p.y;
    }
}


const(ClusterCenter)[] lloyd(Point[] points,
                             in size_t nclusters,
                             ref Xorshift rnd)
in {
    assert(points.length >= nclusters);
    assert(nclusters > 0);
    foreach (const ref p; points)
        assert(!p.x.isNaN && !p.y.isNaN);
} out(result) {
    assert(result.length == nclusters);
    foreach (const ref cc; result)
        assert(!cc.x.isNaN && !cc.y.isNaN);
} body {
    /// Distance and index of the closest cluster center.
    static Tuple!(size_t, double)
    nearestClusterCenter(in ref Point point,
                         in ClusterCenter[] centers) pure nothrow @nogc
    in {
        assert(centers.length > 0);
    } out(result) {
        assert(result[0] < centers.length);
        immutable ClusterCenter c = centers[result[0]];
        immutable d = (c.x - point.x) ^^ 2  +  (c.y - point.y) ^^ 2;
        assert(feqrel(result[1], d) > 45); // Arbitrary.
    } body {
        static double sqrDistance2D(in ref ClusterCenter a,
                                    in ref Point b) pure nothrow @nogc{
            return (a.x - b.x) ^^ 2 + (a.y - b.y) ^^ 2;
        }

        size_t minIndex = point.cluster;
        double minDist = double.max;

        foreach (immutable i, const ref cc; centers) {
            immutable d = sqrDistance2D(cc, point);
            if (minDist > d) {
                minDist = d;
                minIndex = i;
            }
        }

        return tuple(minIndex, minDist);
    }


    static void kMeansPP(Point[] points,
                         ClusterCenter[] centers,
                         ref Xorshift rnd)
    in {
        assert(points.length >= centers.length);
        assert(centers.length > 0);
    } body {
        centers[0] = points[uniform(0, $, rnd)];
        auto d = new double[points.length];

        foreach (immutable i; 1 .. centers.length) {
            double sum = 0;
            foreach (immutable j, const ref p; points) {
                d[j] = nearestClusterCenter(p, centers[0 .. i])[1];
                sum += d[j];
            }

            sum = uniform(0.0, sum, rnd);

            foreach (immutable j, immutable dj; d) {
                sum -= dj;
                if (sum > 0)
                    continue;
                centers[i] = points[j];
                break;
            }
        }

        foreach (ref p; points)
            // Implicit cast of Hconst!ClusterCenter
            // to ClusterCenter[].
            p.cluster = nearestClusterCenter(p, centers)[0];
    }


    auto centers = new ClusterCenter[nclusters];
    kMeansPP(points, centers, rnd);
    auto clusterSizes = new size_t[centers.length];

    size_t changed;
    do {
        // Find clusters centroids.
        centers[] = ClusterCenter(0, 0);
        clusterSizes[] = 0;

        foreach (immutable i, const ref p; points)
            with (centers[p.cluster]) {
                clusterSizes[p.cluster]++;
                x += p.x;
                y += p.y;
            }

        foreach (immutable i, ref cc; centers) {
            cc.x /= clusterSizes[i];
            cc.y /= clusterSizes[i];
        }

        // Find closest centroid of each point.
        changed = 0;
        foreach (ref p; points) {
            immutable minI = nearestClusterCenter(p, centers)[0];
            if (minI != p.cluster) {
                changed++;
                p.cluster = minI;
            }
        }
    // Stop when 99.9% of points are good.
    } while (changed > (points.length >> 10));

    return centers;
}


void printEps(in Point[] points, in ClusterCenter[] centers,
              in size_t W = 400, in size_t H = 400) nothrow
in {
    assert(points.length >= centers.length);
    assert(centers.length > 0);
    assert(W > 0 && H > 0);
    foreach (const ref p; points)
        assert(!p.x.isNaN && !p.y.isNaN);
    foreach (const ref cc; centers)
        assert(!cc.x.isNaN && !cc.y.isNaN);
} body {
    auto findBoundingBox() nothrow @nogc {
        double min_x, max_x, min_y, max_y;
        max_x = max_y = -double.max;
        min_x = min_y = double.max;

        foreach (const ref p; points) {
            if (max_x < p.x) max_x = p.x;
            if (min_x > p.x) min_x = p.x;
            if (max_y < p.y) max_y = p.y;
            if (min_y > p.y) min_y = p.y;
        }
        assert(max_x > min_x && max_y > min_y);

        return tuple(min(W / (max_x - min_x), H / (max_y - min_y)),
                     (max_x + min_x) / 2, (max_y + min_y) / 2);
    }
    //immutable (scale, cx, cy) = findBoundingBox();
    immutable sc_cx_cy = findBoundingBox();
    immutable double scale = sc_cx_cy[0];
    immutable double cx = sc_cx_cy[1];
    immutable double cy = sc_cx_cy[2];

    static immutable struct Color { immutable double r, g, b; }

    immutable size_t k = centers.length;
    Color[] colors;
    colors.reserve(centers.length);
    foreach (immutable i; 0 .. centers.length)
        colors ~= Color((3 * (i + 1) % k) / double(k),
                        (7 * i % k) / double(k),
                        (9 * i % k) / double(k));

    printf("%%!PS-Adobe-3.0\n%%%%BoundingBox: -5 -5 %d %d\n",
           W + 10, H + 10);

    printf("/l {rlineto} def /m {rmoveto} def\n" ~
           "/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n" ~
           "/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath " ~
           "   gsave 1 setgray fill grestore gsave 3 setlinewidth" ~
           " 1 setgray stroke grestore 0 setgray stroke }def\n");

    foreach (immutable i, const ref cc; centers) {
        printf("%g %g %g setrgbcolor\n", colors[i].tupleof);

        foreach (const ref p; points) {
            if (p.cluster != i)
                continue;
            printf("%.3f %.3f c\n",
                   (p.x - cx) * scale + W / 2,
                   (p.y - cy) * scale + H / 2);
        }

        printf("\n0 setgray %g %g s\n",
               (cc.x - cx) * scale + W / 2,
               (cc.y - cy) * scale + H / 2);
    }

    "\n%%%%EOF".printf;
}


void main() {
    enum size_t nPoints = 100_000;
    enum size_t nClusters = 11; // k.
    auto rnd = 1.Xorshift; // For speed and repeatability.

    auto points = generatePoints(nPoints, 10, rnd);
    const clusterCenters = lloyd(points, nClusters, rnd);
    printEps(points, clusterCenters);
}
```

Compiled with ldc2 it's about as fast as the C entry.


## Euler Math Toolbox



```Euler Math Toolbox

>type kmeanscluster
 function kmeanscluster (x: numerical, k: index)
     n=rows(x); m=cols(x);
     i=floor((0:k)/k*(n-1))+1;
     means=zeros(k,m);
     loop 1 to k;
         means[#]=sum(x[i[#]:(i[#+1]-1)]')'/(i[#+1]-i[#]);
     end;
     j=1:n;
     loop 1 to n;
         d=sum((x[#]-means)^2);
         j[#]=extrema(d')[2];
     end;
     repeat
         loop 1 to k;
             i=nonzeros(j==#);
             if cols(i)==0 then means[#]=1;
             else means[#]=(sum(x[i]')/cols(i))';
             endif;
         end;
         jold=j;
         loop 1 to n;
             d=sum((x[#]-means)^2);
             j[#]=extrema(d')[2];
         end;
         if all(jold==j) then break; endif;
     end
     return j
 endfunction

```


Let us apply to random data.


```Euler Math Toolbox

>load clustering.e
 Functions for clustering data.
>np=5; m=3*normal(np,2);
% Spread n points randomly around these points.
>n=5000; x=m[intrandom(1,n,np)]+normal(n,2);
% The function kmeanscluster contains the algorithm. It returns the
% indices of the clusters the points contain to.
>j=kmeanscluster(x,np);
% We plot each point with a color representing its cluster.
>P=x';  ...
>  plot2d(P[1],P[2],r=totalmax(abs(m))+2,color=10+j,points=1,style="."); ...
>  loop 1 to k; plot2d(m[#,1],m[#,2],points=1,style="o#",add=1); end; ...
>  insimg;

```


[[File:ClusteringEulerMathToolbox.png]]


## Fortran

```Fortran

***********************************************************************
* KMPP - K-Means++ - Traditional data clustering with a special initialization
* Public Domain - This program may be used by any person for any purpose.
*
* Origin:
*    Hugo Steinhaus, 1956
*
* Refer to:
*    "kmeans++: the advantages of careful seeding"
*    David Arthur and Sergei Vassilvitskii
*    Proceedings of the eighteenth annual ACM-SIAM symposium
*      on Discrete algorithms, 2007
*
*____Variable_______I/O_______Description___________________Type_______
*    X(P,N)         In        Data points                   Real
*    P              In        Dimension of the data         Integer
*    N              In        Number of points              Integer
*    K              In        # clusters                    Integer
*    C(P,K)         Out       Center points of clusters     Real
*    Z(N)           Out       What cluster a point is in    Integer
*    WORK(N)        Neither                                 Real
*    IFAULT         Out       Error code                    Integer
************************************************************************
      SUBROUTINE KMPP (X, P, N, K, C, Z, WORK, IFAULT)

       IMPLICIT NONE
       INTEGER P, N, K, Z, IFAULT
       REAL X, C, WORK
       DIMENSION X(P,N), C(P,K), Z(N), WORK(N)

*               constants
       INTEGER ITER                 ! maximum iterations
       REAL BIG                     ! arbitrary large number
       PARAMETER (ITER = 1000,
     $            BIG = 1E33)

*                local variables
       INTEGER
     $         H,          ! count iterations
     $         I,          ! count points
     $         I1,         ! point marked as initial center
     $         J,          ! count dimensions
     $         L,          ! count clusters
     $         L0,         ! present cluster ID
     $         L1          ! new cluster ID

       REAL
     $      BEST,                 ! shortest distance to a center
     $      D2,                   ! squared distance
     $      TOT,                  ! a total
     $      W                     ! temp scalar

       LOGICAL CHANGE             ! whether any points have been reassigned

************************************************************************
*           Begin.
************************************************************************
       IFAULT = 0
       IF (K < 1 .OR. K > N) THEN       ! K out of bounds
         IFAULT = 3
         RETURN
       END IF
       DO I = 1, N                       ! clear Z
         Z(I) = 0
       END DO

************************************************************************
*        initial centers
************************************************************************
       DO I = 1, N
         WORK(I) = BIG
       END DO

       CALL RANDOM_NUMBER (W)
       I1 = MIN(INT(W * FLOAT(N)) + 1, N)  ! choose first center at random
       DO J = 1, P
         C(J,1) = X(J,I1)
       END DO

       DO L = 2, K                    ! initialize other centers
         TOT = 0.
         DO I = 1, N                     ! measure from each point
           BEST = WORK(I)
           D2 = 0.                         ! to prior center
           DO J = 1, P
             D2 = D2 + (X(J,I) - C(J,L-1)) **2  ! Squared Euclidean distance
             IF (D2 .GE. BEST) GO TO 10               ! needless to add to D2
           END DO                          ! next J
           IF (D2 < BEST) BEST = D2          ! shortest squared distance
           WORK(I) = BEST
  10       TOT = TOT + BEST             ! cumulative squared distance
         END DO                      ! next data point

************************************************************************
* Choose center with probability proportional to its squared distance
*     from existing centers.
************************************************************************
         CALL RANDOM_NUMBER (W)
         W = W * TOT    ! uniform at random over cumulative distance
         TOT = 0.
         DO I = 1, N
           I1 = I
           TOT = TOT + WORK(I)
           IF (TOT > W) GO TO 20
         END DO                ! next I
  20     CONTINUE
         DO J = 1, P         ! assign center
           C(J,L) = X(J,I1)
         END DO
       END DO               ! next center to initialize

************************************************************************
*                      main loop
************************************************************************
       DO H = 1, ITER
         CHANGE = .FALSE.

*             find nearest center for each point
         DO I = 1, N
           L0 = Z(I)
           L1 = 0
           BEST = BIG
           DO L = 1, K
             D2 = 0.
             DO J = 1, P
               D2 = D2 + (X(J,I) - C(J,L)) **2
               IF (D2 .GE. BEST) GO TO 30
             END DO
  30         CONTINUE
             IF (D2 < BEST) THEN           ! new nearest center
               BEST = D2
               L1 = L
             END IF
           END DO        ! next L

           IF (L0 .NE. L1) THEN
             Z(I) = L1                   !  reassign point
             CHANGE = .TRUE.
           END IF
         END DO         ! next I
         IF (.NOT. CHANGE) RETURN      ! success

************************************************************************
*           find cluster centers
************************************************************************
         DO L = 1, K              ! zero population
           WORK(L) = 0.
         END DO
         DO L = 1, K               ! zero centers
           DO J = 1, P
             C(J,L) = 0.
           END DO
         END DO

         DO I = 1, N
           L = Z(I)
           WORK(L) = WORK(L) + 1.             ! count
           DO J = 1, P
             C(J,L) = C(J,L) + X(J,I)         ! add
           END DO
         END DO

         DO L = 1, K
           IF (WORK(L) < 0.5) THEN          ! empty cluster check
             IFAULT = 1                     ! fatal error
             RETURN
           END IF
           W = 1. / WORK(L)
           DO J = 1, P
             C(J,L) = C(J,L) * W     ! multiplication is faster than division
           END DO
         END DO

       END DO                   ! next H
       IFAULT = 2                ! too many iterations
       RETURN

      END  ! of KMPP


************************************************************************
*             test program (extra credit #1)
************************************************************************
      PROGRAM TPEC1
       IMPLICIT NONE
       INTEGER N, P, K
       REAL TWOPI
       PARAMETER (N = 30 000,
     $            P = 2,
     $            K = 6,
     $            TWOPI = 6.2831853)
       INTEGER I, L, Z(N), IFAULT
       REAL X(P,N), C(P,K), R, THETA, W, WORK(N)

*             Begin
       CALL RANDOM_SEED()
       DO I = 1, N                      ! random points over unit circle
         CALL RANDOM_NUMBER (W)
         R = SQRT(W)                      ! radius
         CALL RANDOM_NUMBER (W)
         THETA = W * TWOPI                ! angle
         X(1,I) = R * COS(THETA)          ! Cartesian coordinates
         X(2,I) = R * SIN(THETA)
       END DO

*             Call subroutine
       CALL KMPP (X, P, N, K, C, Z, WORK, IFAULT)
       PRINT *, 'kmpp returns with error code ', IFAULT

*            Print lists of points in each cluster
       DO L = 1, K
         PRINT *, 'Cluster ', L, ' contains points: '
  10     FORMAT (I6, $)
  20     FORMAT ()
         DO I = 1, N
           IF (Z(I) .EQ. L) PRINT 10, I
         END DO
         PRINT 20
       END DO

*         Write CSV file with Y-coordinates in different columns by cluster
       OPEN (UNIT=1, FILE='tpec1.csv', STATUS='NEW', IOSTAT=IFAULT)
       IF (IFAULT .NE. 0) PRINT *, 'tpec1: trouble opening file'
  30   FORMAT (F8.4, $)
  40   FORMAT (',', $)
  50   FORMAT (F8.4)
       DO I = 1, N
         WRITE (UNIT=1, FMT=30, IOSTAT=IFAULT) X(1,I)
         IF (IFAULT .NE. 0) PRINT *, 'tpec1: trouble writing X-coord'
         DO L = 1, Z(I)                     ! one comma per cluster ID
           WRITE (UNIT=1, FMT=40, IOSTAT=IFAULT)
           IF (IFAULT .NE. 0) PRINT *, 'tpec1: trouble writing comma'
         END DO
         WRITE (UNIT=1, FMT=50, IOSTAT=IFAULT) X(2,I)
         IF (IFAULT .NE. 0) PRINT *, 'tpec1: trouble writing Y-coord'
       END DO

*           Write the centroids in the far column
       DO L = 1, K
         WRITE (UNIT=1, FMT=30, IOSTAT=IFAULT) C(1,L)
         IF (IFAULT .NE. 0) PRINT *, 'tpec1: trouble writing X-coord'
         DO I = 1, K+1
           WRITE (UNIT=1, FMT=40, IOSTAT=IFAULT)
           IF (IFAULT .NE. 0) PRINT *, 'tpec1: trouble writing comma'
         END DO
         WRITE (UNIT=1, FMT=50, IOSTAT=IFAULT) C(2,L)
         IF (IFAULT .NE. 0) PRINT *, 'tpec1: trouble writing Y-coord'
       END DO
       CLOSE (UNIT=1)

      END  ! of test program

```



Uniform random points over the unit circle (compare to the solution in C)
[http://13olive.net/kmeans_FORTRAN_points.png (External image)]

The points with clusters marked by color:
[http://13olive.net/kmeans_FORTRAN_clusters.png (External image)]


## Go


```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "math"
    "math/rand"
    "os"
    "time"
)

type r2 struct {
    x, y float64
}

type r2c struct {
    r2
    c int // cluster number
}

// kmpp implements K-means++, satisfying the basic task requirement
func kmpp(k int, data []r2c) {
    kMeans(data, kmppSeeds(k, data))
}

// kmppSeeds is the ++ part.
// It generates the initial means for the k-means algorithm.
func kmppSeeds(k int, data []r2c) []r2 {
    s := make([]r2, k)
    s[0] = data[rand.Intn(len(data))].r2
    d2 := make([]float64, len(data))
    for i := 1; i < k; i++ {
        var sum float64
        for j, p := range data {
            _, dMin := nearest(p, s[:i])
            d2[j] = dMin * dMin
            sum += d2[j]
        }
        target := rand.Float64() * sum
        j := 0
        for sum = d2[0]; sum < target; sum += d2[j] {
            j++
        }
        s[i] = data[j].r2
    }
    return s
}

// nearest finds the nearest mean to a given point.
// return values are the index of the nearest mean, and the distance from
// the point to the mean.
func nearest(p r2c, mean []r2) (int, float64) {
    iMin := 0
    dMin := math.Hypot(p.x-mean[0].x, p.y-mean[0].y)
    for i := 1; i < len(mean); i++ {
        d := math.Hypot(p.x-mean[i].x, p.y-mean[i].y)
        if d < dMin {
            dMin = d
            iMin = i
        }
    }
    return iMin, dMin
}

// kMeans algorithm.  Lloyd's
func kMeans(data []r2c, mean []r2) {
    // initial assignment
    for i, p := range data {
        cMin, _ := nearest(p, mean)
        data[i].c = cMin
    }
    mLen := make([]int, len(mean))
    for {
        // update means
        for i := range mean {
            mean[i] = r2{}
            mLen[i] = 0
        }
        for _, p := range data {
            mean[p.c].x += p.x
            mean[p.c].y += p.y
            mLen[p.c]++
        }
        for i := range mean {
            inv := 1 / float64(mLen[i])
            mean[i].x *= inv
            mean[i].y *= inv
        }
        // make new assignments, count changes
        var changes int
        for i, p := range data {
            if cMin, _ := nearest(p, mean); cMin != p.c {
                changes++
                data[i].c = cMin
            }
        }
        if changes == 0 {
            return
        }
    }
}

// parameters for extra credit exercises
type ecParam struct {
    k          int
    nPoints    int
    xBox, yBox int
    stdv       int
}

// extra credit 1 and 2:
func main() {
    ec := &ecParam{6, 30000, 300, 200, 30}

    origin, data := genECData(ec)
    vis(ec, data, "origin")
    fmt.Println("Data set origins:")
    fmt.Println("    x      y")
    for _, o := range origin {
        fmt.Printf("%5.1f  %5.1f\n", o.x, o.y)
    }

    kmpp(ec.k, data)

    fmt.Println(
        "\nCluster centroids, mean distance from centroid, number of points:")
    fmt.Println("    x      y  distance  points")
    cent := make([]r2, ec.k)
    cLen := make([]int, ec.k)
    inv := make([]float64, ec.k)
    for _, p := range data {
        cent[p.c].x += p.x
        cent[p.c].y += p.y
        cLen[p.c]++
    }
    for i, iLen := range cLen {
        inv[i] = 1 / float64(iLen)
        cent[i].x *= inv[i]
        cent[i].y *= inv[i]
    }
    dist := make([]float64, ec.k)
    for _, p := range data {
        dist[p.c] += math.Hypot(p.x-cent[p.c].x, p.y-cent[p.c].y)
    }
    for i, iLen := range cLen {
        fmt.Printf("%5.1f  %5.1f  %8.1f  %6d\n",
            cent[i].x, cent[i].y, dist[i]*inv[i], iLen)
    }
    vis(ec, data, "clusters")
}

// genECData generates random data for extra credit tasks.
// k origin points are randomly selected in a bounding box.
// nPoints/k coordinates are then generated for each origin point.
// The x and y coordinates of the data are normally distributed
// with standard deviation stdv.  Thus data coordinates are not
// constrained to the origin box; they can range to +/- max float64.
func genECData(ec *ecParam) (orig []r2, data []r2c) {
    rand.Seed(time.Now().UnixNano())
    orig = make([]r2, ec.k)
    data = make([]r2c, ec.nPoints)
    for i, n := 0, 0; i < ec.k; i++ {
        x := rand.Float64() * float64(ec.xBox)
        y := rand.Float64() * float64(ec.yBox)
        orig[i] = r2{x, y}
        for j := ec.nPoints / ec.k; j > 0; j-- {
            data[n].x = rand.NormFloat64()*float64(ec.stdv) + x
            data[n].y = rand.NormFloat64()*float64(ec.stdv) + y
            data[n].c = i
            n++
        }
    }
    return
}

// vis writes a .png for extra credit 2.
func vis(ec *ecParam, data []r2c, fn string) {
    colors := make([]color.NRGBA, ec.k)
    for i := range colors {
        i3 := i * 3
        third := i3 / ec.k
        frac := uint8((i3 % ec.k) * 255 / ec.k)
        switch third {
        case 0:
            colors[i] = color.NRGBA{frac, 255 - frac, 0, 255}
        case 1:
            colors[i] = color.NRGBA{0, frac, 255 - frac, 255}
        case 2:
            colors[i] = color.NRGBA{255 - frac, 0, frac, 255}
        }
    }
    bounds := image.Rect(-ec.stdv, -ec.stdv, ec.xBox+ec.stdv, ec.yBox+ec.stdv)
    im := image.NewNRGBA(bounds)
    draw.Draw(im, bounds, image.NewUniform(color.White), image.ZP, draw.Src)
    fMinX := float64(bounds.Min.X)
    fMaxX := float64(bounds.Max.X)
    fMinY := float64(bounds.Min.Y)
    fMaxY := float64(bounds.Max.Y)
    for _, p := range data {
        imx := math.Floor(p.x)
        imy := math.Floor(float64(ec.yBox) - p.y)
        if imx >= fMinX && imx < fMaxX && imy >= fMinY && imy < fMaxY {
            im.SetNRGBA(int(imx), int(imy), colors[p.c])
        }
    }
    f, err := os.Create(fn + ".png")
    if err != nil {
        fmt.Println(err)
        return
    }
    err = png.Encode(f, im)
    if err != nil {
        fmt.Println(err)
    }
    err = f.Close()
    if err != nil {
        fmt.Println(err)
    }
}
```

Text output:

```txt

Data set origins:
    x      y
256.8  188.6
 91.7   51.2
201.8  100.2
161.6  102.8
 78.9  152.9
 97.8   17.4

Cluster centroids, mean distance from centroid, number of points:
    x      y  distance  points
152.4  102.1      30.9    5654
104.8    8.7      31.4    4947
211.3   99.4      32.0    4961
 78.3   57.7      29.4    4817
257.7  191.4      36.5    4915
 76.9  156.5      35.0    4706

```

Visualization.  Original clusters on left, discovered clusters on right.
[[file:GoOrigin.png|left]]
[[file:GoClusters.png]]


## Haskell

'''Solution'''
Uses Map for clusterization and MonadRandom library for random sampling.
Vectors are represented as lists, so the solution could be extended to any space dimension.


```haskell
{-# LANGUAGE Strict,FlexibleInstances #-}
module KMeans where

import Control.Applicative
import Control.Monad.Random
import Data.List (minimumBy, genericLength, transpose)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M


type Vec = [Float]
type Cluster = [Vec]

kMeansIteration :: [Vec] -> [Vec] -> [Cluster]
kMeansIteration pts = clusterize . fixPoint iteration
  where
    iteration = map centroid . clusterize

    clusterize centroids = M.elems $ foldr add m0 pts
      where add x = M.insertWith (++) (centroids `nearestTo` x) [x]
            m0 = M.unions $ map (`M.singleton` []) centroids

nearestTo :: [Vec] -> Vec -> Vec
nearestTo pts x =  minimumBy (comparing (distance x)) pts

distance :: Vec -> Vec -> Float
distance a b = sum $ map (^2) $ zipWith (-) a b

centroid :: [Vec] -> Vec
centroid = map mean . transpose
  where  mean pts = sum pts / genericLength pts

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x = if x == fx then x else fixPoint f fx where fx = f x

-- initial sampling

kMeans :: MonadRandom m => Int -> [Vec] -> m [Cluster]
kMeans n pts = kMeansIteration pts <$> take n <$> randomElements pts

kMeansPP :: MonadRandom m => Int -> [Vec] -> m [Cluster]
kMeansPP n pts = kMeansIteration pts <$> centroids
  where centroids = iterate (>>= nextCentroid) x0 !! (n-1)
        x0 = take 1 <$> randomElements pts
        nextCentroid cs = (: cs) <$> fromList (map (weight cs) pts)
        weight cs x = (x, toRational $ distance x (cs `nearestTo` x))

randomElements :: MonadRandom m => [a] -> m [a]
randomElements pts = map (pts !!) <$> getRandomRs (0, length pts)

-- sample cluster generation

instance (RandomGen g, Monoid m) => Monoid (Rand g m) where
   mempty = pure mempty
   mappend = liftA2 mappend

mkCluster n s m = take n . transpose <$> mapM randomsAround m
  where randomsAround x0 = map (\x -> x0+s*atanh x) <$> getRandomRs (-1,1)
```


'''Examples'''


```haskell
module Main where

import Graphics.EasyPlot
import Data.Monoid

import KMeans

test = do datum <- mkCluster 1000 0.5 [0,0,1]
                <> mkCluster 2000 0.5 [2,3,1]
                <> mkCluster 3000 0.5 [2,-3,0]
          cls <- kMeansPP 3 datum
          mapM_ (\x -> print (centroid x, length x)) cls

main = do datum  <- sequence [ mkCluster 30100 0.3 [0,0]
                             , mkCluster 30200 0.4 [2,3]
                             , mkCluster 30300 0.5 [2,-3]
                             , mkCluster 30400 0.6 [6,0]
                             , mkCluster 30500 0.7 [-3,-3]
                             , mkCluster 30600 0.8 [-5,5] ]
          cls <- kMeansPP 6 (mconcat datum)
          plot (PNG "plot1.png") $ map listPlot cls
            where
              listPlot = Data2D [Title "",Style Dots] [] . map (\(x:y:_) -> (x,y))

```

Result: all centroids and clusters are found.

```txt
λ> test
([3.161875e-3,-3.096125e-3,0.99095285],1002)
([2.004138,2.9655986,1.0139971],1999)
([2.006579,-2.9902787],2999)
```



## Huginn


```huginn
#! /bin/sh
exec huginn -E "${0}" "${@}"
#! huginn

import Algorithms as algo;
import Mathematics as math;
import OperatingSystem as os;

class Color { r = 0.; g = 0.; b = 0.; }
class Point { x = 0.; y = 0.; group = -1; }

k_means_initial_centroids( points_, clusterCount_ ) {
	centroids = [];
	discreteRng = math.Randomizer( math.Randomizer.DISTRIBUTION.DISCRETE, 0, size( points_ ) - 1 );
	uniformRng = math.Randomizer( math.Randomizer.DISTRIBUTION.UNIFORM, 0.0, 1.0 );
	centroids.push( copy( points_[discreteRng.next()] ) );
	for ( i : algo.range( clusterCount_ - 1 ) ) {
		distances = [];
		sum = 0.0;
		for ( p : points_ ) {
			shortestDist = math.INFINITY;
			for ( c : centroids ) {
				dx = c.x - p.x;
				dy = c.y - p.y;
				d = dx * dx + dy * dy;
				if ( d < shortestDist ) {
					shortestDist = d;
				}
			}
			distances.push( ( shortestDist, p ) );
			sum += shortestDist;
		}
		sum *= uniformRng.next();
		for ( d : distances ) {
			sum -= d[0];
			if ( sum <= 0.0 ) {
				centroids.push( copy( d[1] ) );
				break;
			}
		}
	}
	for ( i, c : algo.enumerate( centroids ) ) {
		c.group = i;
	}
	return ( centroids );
}

k_means( points_, clusterCount_, maxError_ = 0.001, maxIter_ = 100 ) {
	centroids = k_means_initial_centroids( points_, clusterCount_ );
	pointCount = real( size( points_ ) );
	for ( iter : algo.range( maxIter_ ) ) {
		updated = 0.0;
		for ( p : points_ ) {
			shortestDist = math.INFINITY;
			g = 0;
			for ( c : centroids ) {
				dx = c.x - p.x;
				dy = c.y - p.y;
				dist = dx * dx + dy * dy;
				if ( dist < shortestDist ) {
					shortestDist = dist;
					g = c.group;
				}
			}
			if ( p.group != g ) {
				p.group = g;
				updated += 1.0;
			}
		}
		for ( c : centroids ) {
			n = 0;
			c.x = 0.;
			c.y = 0.;
			for ( p : points_ ) {
				if ( p.group == c.group ) {
					c.x += p.x;
					c.y += p.y;
					n += 1;
				}
			}
			if ( n > 0 ) {
				c.x /= real( n );
				c.y /= real( n );
			}
		}
		err = updated / pointCount;
		os.stderr().write_line( "err = {}\n".format( err ) );
		if ( err < maxError_ ) {
			os.stderr().write_line( "done in {} iterations\n".format( iter ) );
			break;
		}
	}
	return ( centroids );
}

gen_points( numPoints_ ) {
	phiGen = math.Randomizer( math.Randomizer.DISTRIBUTION.UNIFORM, 0., 2. * math.pi( real ) );
	rGen = math.Randomizer( math.Randomizer.DISTRIBUTION.TRIANGLE, 0., 1., 1. );
	points = [];
	for ( i : algo.range( numPoints_ ) ) {
		phi = phiGen.next();
		r = rGen.next();
		points.push( Point( r * math.cosinus( phi ), r * math.sinus( phi ) ) );
	}
	return ( points );
}

import ProgramOptions as po;

main( argv_ ) {
	poh = po.Handler( "k-means++", "k-means++ clustering algorithm demo" );
	poh.add_option(
		name: "numPoints,N",
		requirement: po.VALUE_REQUIREMENT.REQUIRED,
		help: "number of points",
		conversion: integer,
		valueName: "num",
		defaultValue: 30000
	);
	poh.add_option(
		name: "numClusters,C",
		requirement: po.VALUE_REQUIREMENT.REQUIRED,
		help: "number of custers",
		conversion: integer,
		valueName: "num",
		defaultValue: 7
	);
	poh.add_option(
		name: "maxIterations,I",
		requirement: po.VALUE_REQUIREMENT.REQUIRED,
		help: "maximum number of iterations for the algorithm to run",
		conversion: integer,
		valueName: "num",
		defaultValue: 100
	);
	poh.add_option(
		name: "maxInvalidRatio,R",
		requirement: po.VALUE_REQUIREMENT.REQUIRED,
		help: "maximum ratio of points that are still assigned to invalid centroids",
		conversion: real,
		valueName: "num",
		defaultValue: 0.001
	);
	poh.add_option(
		name: "help,H",
		requirement: po.VALUE_REQUIREMENT.NONE,
		help: "show help information and stop"
	);
	poh.add_option(
		name: "verbose,v",
		requirement: po.VALUE_REQUIREMENT.NONE,
		help: "show more info about program execution"
	);
	parsed = poh.command_line( argv_ );
	if ( parsed == none ) {
		return ( 1 );
	}
	if ( parsed.options["help"] ) {
		print( poh.help_string() + "\n" );
		return ( 0 );
	}
	if ( parsed.options["verbose"] ) {
		os.stderr().write_line( string( parsed ) + "\n" );
	}
	points = gen_points( parsed.options["numPoints"] );
	print_eps(
		points,
		k_means(
			points,
			parsed.options["numClusters"],
			parsed.options["maxInvalidRatio"],
			parsed.options["maxIterations"]
		)
	);
}

print_eps( points, cluster_centers, W = 400, H = 400 ) {
	colors = [];
	for ( i : algo.range( size( cluster_centers ) ) ) {
		ii = real( i );
		colors.push(
			Color(
				( 3. * ( ii + 1. ) % 11. ) / 11.0,
				( 7. * ii % 11. ) / 11.0,
				( 9. * ii % 11. ) / 11.0
			)
		);
	}
	max_x = max_y = - math.INFINITY;
	min_x = min_y = math.INFINITY;
	for ( p : points ) {
		if ( max_x < p.x ) { max_x = p.x; }
		if ( min_x > p.x ) { min_x = p.x; }
		if ( max_y < p.y ) { max_y = p.y; }
		if ( min_y > p.y ) { min_y = p.y; }
	}
	scale = math.min( real( W ) / ( max_x - min_x ), real( H ) / ( max_y - min_y ) );
	cx = ( max_x + min_x ) / 2.;
	cy = ( max_y + min_y ) / 2.;
	print( "%!PS-Adobe-3.0\n%%BoundingBox: -5 -5 {} {}\n".format( W + 10, H + 10 ) );
	print(
		"/l {rlineto} def /m {rmoveto} def\n"
		"/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n"
		"/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath "
		"   gsave 1 setgray fill grestore gsave 3 setlinewidth"
		" 1 setgray stroke grestore 0 setgray stroke }def\n"
	);
	for ( i, cc : algo.enumerate( cluster_centers ) ) {
		print( "{} {} {} setrgbcolor\n".format( colors[i].r, colors[i].g, colors[i].b ) );
		for ( p : points ) {
			if ( p.group != i ) {
				continue;
			}
			print( "{:.3f} {:.3f} c\n".format( ( p.x - cx ) * scale + real( W ) / 2., ( p.y - cy ) * scale + real( H ) / 2. ) );
		}
		print("\n0 setgray {} {} s\n".format( ( cc.x - cx ) * scale + real( W ) / 2., ( cc.y - cy ) * scale + real( H ) / 2. ) );
	}
	print( "\n%%%%EOF\n" );
}
```



## J


'''Solution''':
```j
   NB.  Selection of initial centroids, per K-means++
   initialCentroids     =:  (] , randomCentroid)^:(<:@:]`(,:@:seedCentroid@:[))~
     seedCentroid       =:  {~ ?@#
     randomCentroid     =:  [ {~ [: wghtProb [: <./ distance/~
       distance         =:  +/&.:*:@:-"1  NB.  Extra credit #3 (N-dimensional is the same as 2-dimensional in J)
       wghtProb         =:  1&$: : ((%{:)@:(+/\)@:] I. [ ?@$ 0:)"0 1 NB.  Due to Roger Hui http://j.mp/lj5Pnt

   NB.  Having selected the initial centroids, the standard K-means algo follows
   centroids            =:  ([ mean/.~ closestCentroid)^:(]`_:`initialCentroids)
     closestCentroid    =:  [: (i.<./)"1 distance/
     mean               =:  +/ % #
```


'''Extra credit''':
```j
   randMatrix           =:  ?@$&0  NB.  Extra credit #1
   packPoints           =:  <"1@:|: NB.  Extra credit #2: Visualization code due to Max Harms http://j.mp/l8L45V
   plotClusters         =:  dyad define  NB.  as is the example image in this task
	require 'plot'
	pd 'reset;aspect 1;type dot;pensize 2'
	pd@:packPoints&> y
	pd 'type marker;markersize 1.5;color 0 0 0'
	pd@:packPoints x
	pd 'markersize 0.8;color 255 255 0'
	pd@:packPoints x
	pd 'show'
)

NB.  Extra credit #4:  Polar coordinates are not available in this version
NB.  but wouldn't be hard to provide with  &.cartToPole  .
```


'''Example''':
```j
   plotRandomClusters   =:  3&$: : (dyad define)
	dataset          =.  randMatrix 2 {. y,2

	centers          =.  x centroids dataset
	clusters         =.  centers (closestCentroid~ </. ])  dataset
	centers plotClusters clusters
)

     plotRandomClusters 300         NB.  300 points, 3 clusters
   6 plotRandomClusters 30000       NB.  3e5 points, 6 clusters
  10 plotRandomClusters 17000 5     NB.  17e3 points, 10 clusters, 5 dimensions
```



## JavaScript

'''Solution'''

Live Demo (Extra Credit #2) [https://ezward.github.io/kmeans-javascript/kmeansrandom.html KMeans++ in JavaScript]


```javascript

/**
 * kmeans module
 *
 *   cluster(model, k, converged = assignmentsConverged)
 *   distance(p, q),
 *   distanceSquared(p, q),
 *   centroidsConverged(delta)
 *   assignmentsConverged(model, newModel)
 *   assignmentsToClusters(model)
 */
define(function () {
    "use strict";

    /**
     * @public
     * Calculate the squared distance between two vectors.
     *
     * @param [number] p vector with same dimension as q
     * @param [number] q vector with same dimension as p
     * @return {number} the distance between p and q squared
     */
    function distanceSquared(p, q) {
        const d = p.length; // dimension of vectors

        if(d !== q.length) throw Error("p and q vectors must be the same length")

        let sum = 0;
        for(let i = 0; i < d; i += 1) {
            sum += (p[i] - q[i])**2
        }
        return sum;
    }

    /**
     * @public
     * Calculate the distance between two vectors of the same dimension.
     *
     * @param [number] p vector of same dimension as q
     * @param [number] q vector of same dimension as p
     * @return the distance between vectors p and q
     */
    function distance(p, q) {
        return Math.sqrt(distanceSquared(p, q));
    }

    /**
     * @private
     * find the closest centroid for the given observation and return it's index.
     *
     * @param [[number]] centroids - array of k vectors, each vector with same dimension as observations.
     *                               these are the center of the k clusters
     * @param [[number]] observation - vector with same dimension as centroids.
     *                                 this is the observation to be clustered.
     * @return {number} the index of the closest centroid in centroids
     */
    function findClosestCentroid(centroids, observation) {
        const k = centroids.length; // number of clusters/centroids

        let centroid = 0;
        let minDistance = distance(centroids[0], observation);
        for(let i = 1; i < k; i += 1) {
            const dist = distance(centroids[i], observation);
            if(dist < minDistance) {
                centroid = i;
                minDistance = dist;
            }
        }
        return centroid;
    }

    /**
     * @private
     * Calculate the centroid for the given observations.
     * This takes the average of all observations (at each dimension).
     * This average vector is the centroid for those observations.
     *
     * @param [[number]] observations - array of observations (each observatino is a vectors)
     * @return [number] centroid for given observations (vector of same dimension as observations)
     */
    function calculateCentroid(observations) {
        const n = observations.length;      // number of observations
        const d = observations[0].length;   // dimension of vectors

        // create zero vector of same dimension as observation
        let centroid = [];
        for(let i = 0; i < d; i += 1) {
            centroid.push(0.0);
        }

        //
        // sum all observations at each dimension
        //
        for(let i = 0; i < n; i += 1) {
            //
            // add the observation to the sum vector, element by element
            // to prepare to calculate the average at each dimension.
            //
            for(let j = 0; j < d; j += 1) {
                centroid[j] += observations[i][j];
            }
        }

        //
        // divide each dimension by the number of observations
        // to create the average vector.
        //
        for(let j = 0; j < d; j += 1) {
            centroid[j] /= n;
        }

        return centroid;
    }

    /**
     * @private
     * calculate the cluster assignments for the observations, given the centroids.
     *
     * @param [[number]] centroids - list of vectors with same dimension as observations
     * @param [[number]] observations - list of vectors with same dimension as centroids
     * @return [number] list of indices into centroids; one per observation.
     */
    function assignClusters(centroids, observations) {
        const n = observations.length;  // number of observations

        const assignments = [];
        for(let i = 0; i < n; i += 1) {
            assignments.push(findClosestCentroid(centroids, observations[i]));
        }

        return assignments; // centroid index for each observation
    }

    /**
     * @private
     * calculate one step of the k-means algorithm;
     * - assign each observation to the nearest centroid to create clusters
     * - calculate a new centroid for each cluster given the observations in the cluster.
     *
     * @param [[number]] centroids - list of vectors with same dimension as observations
     * @param [[number]] observations - list of vectors with same dimension as centroids
     * @return a new model with observations, centroids and assignments
     */
    function kmeansStep(centroids, observations) {
        const k = centroids.length; // number of clusters/centroids

        // assign each observation to the nearest centroid to create clusters
        const assignments = assignClusters(centroids, observations); // array of cluster indices that correspond observations

        // calculate a new centroid for each cluster given the observations in the cluster
        const newCentroids = [];
        for(let i = 0; i < k; i += 1) {
            // get the observations for this cluster/centroid
            const clusteredObservations = observations.filter((v, j) => assignments[j] === i);

            // calculate a new centroid for the observations
            newCentroids.push(calculateCentroid(clusteredObservations));
        }
        return {'observations': observations, 'centroids': newCentroids, 'assignments': assignments }
    }

    /**
     * @public
     * Run k-means on the given model until each centroid converges to with the given delta
     * The initial model is NOT modified by the algorithm, rather a new model is returned.
     *
     * @param {*} model - object with
     *                    observations: array, length n, of data points; each datapoint is
     *                                  itself an array of numbers (a vector).
     *                                  The length each datapoint (d) vector should be the same.
     *                    centroids: array of data points.
     *                               The length of the centroids array indicates the number of
     *                               of desired clusters (k).
     *                               each datapoint is array (vector) of numbers
     *                               with same dimension as the datapoints in observations.
     *                    assignments: array of integers, one per observation,
     *                                 with values 0..centroids.length - 1
     * @param number delta - the maximum difference between each centroid in consecutive runs for convergence
     * @return {*} - result with
     *               model: model, as described above, with updated centroids and assignments,
     *               iterations: number of iterations,
     *               durationMs: elapsed time in milliseconds
     */
    function kmeans(model, maximumIterations = 200, converged = assignmentsConverged) {
        const start = new Date();

        // calculate new centroids and cluster assignments
        let newModel = kmeansStep(model.centroids, model.observations);

        // continue until centroids do not change (within given delta)
        let i = 0;
        while((i < maximumIterations) && !converged(model, newModel)) {
            model = newModel;   // new model is our model now
            // console.log(model);

            // calculate new centroids and cluster assignments
            newModel = kmeansStep(model.centroids, model.observations);
            i += 1;
        }

        // console.log(newModel);
        const finish = new Date();
        return {'model': newModel, 'iterations': i, 'durationMs': (finish.getTime() - start.getTime())};
    }

    /**
     * @public
     * Return a function that determines convergence based on the centroids.
     * If two consecutive sets of centroids remain within a given delta,
     * then the algorithm is converged.
     *
     * @param number delta, the maximum difference between each centroid in consecutive runs for convergence
     * @return function to use as the converged function in kmeans call.
     */
    function centroidsConverged(delta) {
        /**
         * determine if two consecutive set of centroids are converged given a maximum delta.
         *
         * @param [[number]] centroids - list of vectors with same dimension as observations
         * @param [[number]] newCentroids - list of vectors with same dimension as observations
         * @param number delta - the maximum difference between each centroid in consecutive runs for convergence
         */
        return function(model, newModel) {
            const centroids = model.centroids;
            const newCentroids = newModel.centroids;

            const k = centroids.length; // number of clusters/centroids
            for(let i = 0; i < k; i += 1) {
                if(distance(centroids[i], newCentroids[i]) > delta) {
                    return false;
                }
            }

            return true;
        }
    }

    /**
     * @public
     * determine if two consecutive set of clusters are converged;
     * the clusters are converged if the cluster assignments are the same.
     *
     * @param {*} model - object with observations, centroids, assignments
     * @param {*} newModel - object with observations, centroids, assignments
     * @param number delta - the maximum difference between each centroid in consecutive runs for convergence
     */
    function assignmentsConverged(model, newModel) {
        function arraysEqual(a, b) {
            if (a === b) return true;
            if (a === undefined || b === undefined) return false;
            if (a === null || b === null) return false;
            if (a.length !== b.length) return false;

            // If you don't care about the order of the elements inside
            // the array, you should sort both arrays here.

            for (var i = 0; i < a.length; ++i) {
            if (a[i] !== b[i]) return false;
            }
            return true;
        }

        return arraysEqual(model.assignments, newModel.assignments);
    }

    /**
     * Use the model assignments to create
     * array of observation indices for each centroid
     *
     * @param {object} model with observations, centroids and assignments
     * @reutrn [[number]] array of observation indices for each cluster
     */
    function assignmentsToClusters(model) {
        //
        // put offset of each data points into clusters using the assignments
        //
        const n = model.observations.length;
        const k = model.centroids.length;
        const assignments = model.assignments;
        const clusters = [];
        for(let i = 0; i < k; i += 1) {
            clusters.push([])
        }
        for(let i = 0; i < n; i += 1) {
            clusters[assignments[i]].push(i);
        }

        return clusters;
    }


    //
    // return public methods
    //
    return {
        'cluster': kmeans,
        'distance': distance,
        'distanceSquared': distanceSquared,
        'centroidsConverged': centroidsConverged,
        'assignmentsConverged': assignmentsConverged,
        "assignmentsToClusters": assignmentsToClusters
    };

});

/**
 * kmeans++ initialization module
 */
define(function (require) {
    "use strict";

    const kmeans = require("./kmeans");

    /**
     * @public
     * create an initial model given the data and the number of clusters.
     *
     * This uses the kmeans++ algorithm:
     * 1. Choose one center uniformly at random from among the data points.
     * 2. For each data point x, compute D(x), the distance between x and
     *    the nearest center that has already been chosen.
     * 3. Choose one new data point at random as a new center,
     *    using a weighted probability distribution where a point x is chosen with probability proportional to D(x)^2.
     * 4. Repeat Steps 2 and 3 until k centers have been chosen.
     * 5. Now that the initial centers have been chosen, proceed using
     *    standard k-means clustering.
     *
     * @param {[float]} observations the data as an array of number
     * @param {integer} k the number of clusters
     */
    return function(observations, k) {

        /**
         * given a set of n  weights,
         * choose a value in the range 0..n-1
         * at random using weights as a distribution.
         *
         * @param {*} weights
         */
        function weightedRandomIndex(weights, normalizationWeight) {
            const n = weights.length;
            if(typeof normalizationWeight !== 'number') {
                normalizationWeight = 0.0;
                for(let i = 0; i < n; i += 1) {
                    normalizationWeight += weights[i];
                }
            }

            const r = Math.random();  // uniformly random number 0..1 (a probability)
            let index = 0;
            let cumulativeWeight = 0.0;
            for(let i = 0; i < n; i += 1) {
                //
                // use the uniform probability to search
                // within the normalized weighting (we divide by totalWeight to normalize).
                // once we hit the probability, we have found our index.
                //
                cumulativeWeight += weights[i] / normalizationWeight;
                if(cumulativeWeight > r) {
                    return i;
                }
            }

            throw Error("algorithmic failure choosing weighted random index");
        }

        const n = observations.length;
        const distanceToCloseCentroid = []; // distance D(x) to closest centroid for each observation
        const centroids = [];   // indices of observations that are chosen as centroids

        //
        // keep list of all observations' indices so
        // we can remove centroids as they are created
        // so they can't be chosen twice
        //
        const index = [];
        for(let i = 0; i < n; i += 1) {
            index[i] = i;
        }

        //
        //  1. Choose one center uniformly at random from among the data points.
        //
        let centroidIndex = Math.floor(Math.random() * n);
        centroids.push(centroidIndex);

        for(let c = 1; c < k; c += 1) {
            index.slice(centroids[c - 1], 1);    // remove previous centroid from further consideration
            distanceToCloseCentroid[centroids[c - 1]] = 0;  // this effectively removes it from the probability distribution

            //
            // 2. For each data point x, compute D(x), the distance between x and
            //    the nearest center that has already been chosen.
            //
            // NOTE: we used the distance squared (L2 norm)
            //
            let totalWeight = 0.0;
            for(let i = 0; i < index.length; i += 1) {
                //
                // if this is the first time through, the distance is undefined, so just set it.
                // Otherwise, choose the minimum of the prior closest and this new centroid
                //
                const distanceToCentroid = kmeans.distanceSquared(observations[index[i]], observations[centroids[c - 1]]);
                distanceToCloseCentroid[index[i]] =
                    (typeof distanceToCloseCentroid[index[i]] === 'number')
                    ? Math.min(distanceToCloseCentroid[index[i]], distanceToCentroid)
                    : distanceToCentroid;
                totalWeight += distanceToCloseCentroid[index[i]];
            }

            //
            //  3. Choose one new data point at random as a new center,
            //     using a weighted probability distribution where a point x is chosen with probability proportional to D(x)^2.
            //
            centroidIndex = index[weightedRandomIndex(distanceToCloseCentroid, totalWeight)];
            centroids.push(centroidIndex);

            //  4. Repeat Steps 2 and 3 until k centers have been chosen.
        }

        //
        //  5. Now that the initial centers have been chosen, proceed using
        //     standard k-means clustering. Return the model so that
        //     kmeans can continue.
        //
        return {
            'observations': observations,
            'centroids': centroids.map(x => observations[x]), // map centroid index to centroid value
            'assignments': observations.map((x, i) => i % centroids.length) // distribute among centroids
        }
    }

});

/**
 * Extra Credit #1
 * module for creating random models for kmeans clustering
 */
define(function (require) {
    "use strict";

    const kmeans = require("./kmeans");

    /**
     * @return a random, normally distributed number
     */
    function randomNormal() {
        // n = 6 gives a good enough approximation
        return ((Math.random() + Math.random() + Math.random() + Math.random() + Math.random() + Math.random()) - 3) / 3;
    }

    /**
     * Generate a uniform random unit vector
     *
     * @param {Integer} d dimension of data
     * @return n random datapoints of dimension d with length == 1
     */
    function randomUnitVector(d) {
        const range = max - min;
        let magnitude = 0.0;
        const observation = [];

        // uniform random for each dimension
        for(let j = 0; j < d; j += 1) {
            const x = Math.random();
            observation[j] = x;
            magnitude = x * x;
        }

        // normalize
        const magnitude = Math.sqrt(magnitude);
        for(let j = 0; j < d; j += 1) {
            observation[j] /= magnitude;
        }

        return observation;
    }

    /**
     * Generate a uniform random unit vectors for clustering
     *
     * @param {Integer} n number of data points
     * @param {Integer} d dimension of data
     * @return n random datapoints of dimension d with length == 1
     */
    function randomUnitVectors(n, d) {

        // create n random observations, each of dimension d
        const observations = [];
        for(let i = 0; i < n; i += 1) {
            // create random observation of dimension d
            const observation = randomUnitVector(d);
            observations.push(observation);
        }

        return observations;
    }



    /**
     * Generate a spherical random vector
     *
     * @param {Integer} n number of data points
     * @param {Integer} d dimension of data
     * @param {Number} r radium from center for data point
     * @return n random datapoints of dimension d
     */
    function randomSphericalVector(d, r) {
        const observation = [];

        let magnitude = 0.0;
        for(let j = 0; j < d; j += 1)
        {
            const x = randomNormal();
            observation[j] = x;
            magnitude += x * x;
        }

        // normalize
        magnitude = Math.sqrt(magnitude);
        for(let j = 0; j < d; j += 1) {
            observation[j] = observation[j] * r / magnitude;
        }

        return observation;
    }



    /**
     * Generate a spherical random vectors
     *
     * @param {Integer} n number of data points
     * @param {Integer} d dimension of data
     * @param {Number} max radius from center for data points
     * @return n random datapoints of dimension d
     */
    function randomSphericalVectors(n, d, r) {

        // create n random observations, each of dimension d
        const observations = [];
        for(let i = 0; i < n; i += 1) {
            // create random observation of dimension d with random radius
            const observation = randomSphericalVector(d, Math.random() * r);
            observations.push(observation);
        }

        return observations;
    }

    /**
     * Generate a uniform random model for clustering
     *
     * @param {Integer} n number of data points
     * @param {Integer} d dimension of data
     * @param {Number} radius of sphere
     * @return n random datapoints of dimension d
     */
    function randomVectors(n, d, min, max) {

        const range = max - min;

        // create n random observations, each of dimension d
        const observations = [];
        for(let i = 0; i < n; i += 1) {
            // create random observation of dimension d
            const observation = randomVector(d, min, max);
            observations.push(observation);
        }

        return observations;
    }

    /**
     * Generate a uniform random model for clustering
     *
     * @param {Integer} d dimension of data
     * @param {Number} radius of sphere
     * @return n random datapoints of dimension d
     */
    function randomVector(d, min, max) {

        // create random observation of dimension d
        const range = max - min;
        const observation = [];
        for(let j = 0; j < d; j += 1) {
            observation.push(min + Math.random() * range);
        }

        return observation;
    }

    return {
        'randomVector': randomVector,
        'randomUnitVector': randomUnitVector,
        'randomSphericalVector': randomSphericalVector,
        'randomVectors': randomVectors,
        'randomUnitVectors': randomUnitVectors,
        'randomSphericalVectors': randomSphericalVectors
    }

});

/**
 * Extra Credit #4
 * Application to cluster random data using kmeans++
 *
 * cluster(k, n, d) - cluster n data points of dimension d into k clusters
 * plot(canvas, result) - plot the results of cluster() to the given html5 canvas using clusterjs
 */
define(function (require) {
    "use strict";
    const kmeans = require("./kmeans/kmeans");
    const kmeanspp = require("./kmeans/kmeanspp");
    const randomCentroidInitializer = require("./kmeans/randomCentroidInitializer");
    const kmeansRandomModel = require("./kmeans/kmeansRandomModel");


    /**
     * @public
     * Load iris dataset and run kmeans on it given the number of clusters
     *
     * @param {integer} k number of clusters to create
     */
    function cluster(k, n, d) {

        //
        // map iris data rows from dictionary to vector (array), leaving out the label
        //
        const observations = kmeansRandomModel.randomSphericalVectors(n, d, 10.0);

        //
        // create the intial model and run it
        //
        // const initialModel = randomCentroidInitializer(observations, k);
        const initialModel = kmeanspp(observations, k);

        //
        // cluster into given number of clusters
        //
        const results = kmeans.cluster(initialModel);

        //
        // do this for the convenience of the plotting functions
        //
        results.clusters = kmeans.assignmentsToClusters(results.model);

        return results;
    }

    const clusterColor = ['red', 'green', 'blue', 'yellow', 'purple', 'cyan', 'magenta', 'pink', 'brown', 'black'];
    let chart = undefined;

    /**
     * plot the clustred iris data model.
     *
     * @param {object} results of cluster(), with model, clusters and clusterCompositions
     * @param {boolean} showClusterColor true to show learned cluster points
     * @param {boolean} showSpeciesColor true to show known dataset labelled points
     */
    function plot(canvas, results) {

        //
        // map iris data rows from dictionary to vector (array), leaving out the label
        //
        const model = results.model;
        const observations = model.observations;
        const assignments = model.assignments;
        const centroids = model.centroids;
        const d = observations[0].length;
        const n = observations.length;
        const k = centroids.length;

        //
        // put offset of each data points into clusters using the assignments
        //
        const clusters = results.clusters;

        //
        // plot the clusters
        //
        const chartData = {
            // for the purposes of plotting in 2 dimensions, we will use
            // x = dimension 0 and y = dimension 1
            datasets: clusters.map(function(c, i) {
                return {
                    label: "cluster" + i,
                    data: c.map(d => ({'x': observations[d][0], 'y': observations[d][1]})),
                    backgroundColor: clusterColor[i % clusterColor.length],
                    pointBackgroundColor: clusterColor[i % clusterColor.length],
                    pointBorderColor:  clusterColor[i % clusterColor.length]
                };
            })
        };
        const chartOptions = {
            responsive: true,
            maintainAspectRatio: false,
            title: {
                display: true,
                text: 'Random spherical data set (d=$d, n=$n) clustered using K-Means (k=$k)'
                        .replace("$d", d)
                        .replace('$n', n)
                        .replace('$k', k)
            },
            legend: {
                position: 'bottom',
                display: true
            },
            scales: {
                xAxes: [{
                    type: 'linear',
                    position: 'bottom',
                    scaleLabel: {
                        labelString: 'x axis',
                        display: false,
                    }
                }],
                yAxes: [{
                    type: 'linear',
                    position: 'left',
                    scaleLabel: {
                        labelString: 'y axis',
                        display: false
                    }
                }]
            }
        };

        //
        // we need to destroy the previous chart so it's interactivity
        // does not continue to run
        //
        if(undefined !== chart) {
            chart.destroy()
        }
        chart = new Chart(canvas, {
            type: 'scatter',
            data: chartData,
            options: chartOptions,
        });

    }

    return {'cluster': cluster, 'plot': plot};
});



```




## Julia


```julia
# run via Julia REPL
using Clustering, Makie, DataFrames, RDatasets

const iris = dataset("datasets", "iris")
const colors = [:red, :green, :blue]
const plt = Vector{Any}(undef,2)

scene1 = Scene()
scene2 = Scene()

for (i, sp) in enumerate(unique(iris[:Species]))
    idx = iris[:Species] .== sp
    sel = iris[idx, [:SepalWidth, :SepalLength]]
    plt[1] = scatter!(scene1, sel[1], sel[2], color = colors[i],
        limits = FRect(1.5, 4.0, 3.0, 4.0))
end

features = permutedims(convert(Array, iris[1:4]), [2, 1])

# K Means ++
result = kmeans(features, 3, init = :kmpp)  # set to 3 clusters with kmeans++ :kmpp

for center in unique(result.assignments)
    idx = result.assignments .== center
    sel = iris[idx, [:SepalWidth, :SepalLength]]
    plt[2] = scatter!(scene2, sel[1], sel[2], color = colors[center],
        limits = FRect(1.5, 4.0, 3.0, 4.0))
end

scene2[Axis][:names][:axisnames] = scene1[Axis][:names][:axisnames] =
    ("Sepal Width", "Sepal Length")
t1 = text(Theme(), "Species Classification", camera=campixel!)
t2 = text(Theme(), "Kmeans Classification", camera=campixel!)
vbox(hbox(plt[1], t1), hbox(plt[2], t2))

```



## Kotlin

The terminal output should, of course, be redirected to an .eps file so that it can be viewed with (for instance) Ghostscript.

As in the case of the C example, the data is partitioned into 11 clusters though, unlike C (which doesn't use srand), the output will be different each time the program is run.

```scala
// version 1.2.21

import java.util.Random
import kotlin.math.*

data class Point(var x: Double, var y: Double, var group: Int)

typealias LPoint = List<Point>
typealias MLPoint = MutableList<Point>

val origin get() = Point(0.0, 0.0, 0)
val r = Random()
val hugeVal = Double.POSITIVE_INFINITY

const val RAND_MAX = Int.MAX_VALUE
const val PTS = 100_000
const val K = 11
const val W = 400
const val H = 400

fun rand() = r.nextInt(RAND_MAX)

fun randf(m: Double) = m * rand() / (RAND_MAX - 1)

fun genXY(count: Int, radius: Double): LPoint {
    val pts = List(count) { origin }

    /* note: this is not a uniform 2-d distribution */
    for (i in 0 until count) {
        val ang = randf(2.0 * PI)
        val r = randf(radius)
        pts[i].x = r * cos(ang)
        pts[i].y = r * sin(ang)
    }
    return pts
}

fun dist2(a: Point, b: Point): Double {
    val x = a.x - b.x
    val y = a.y - b.y
    return x * x + y * y
}

fun nearest(pt: Point, cent: LPoint, nCluster: Int): Pair<Int, Double> {
    var minD = hugeVal
    var minI = pt.group
    for (i in 0 until nCluster) {
        val d = dist2(cent[i], pt)
        if (minD > d) {
            minD = d
            minI = i
        }
    }
    return minI to minD
}

fun kpp(pts: LPoint, len: Int, cent: MLPoint) {
    val nCent = cent.size
    val d = DoubleArray(len)
    cent[0] = pts[rand() % len].copy()
    for (nCluster in 1 until nCent) {
        var sum = 0.0
        for (j in 0 until len) {
            d[j] = nearest(pts[j], cent, nCluster).second
            sum += d[j]
        }
        sum = randf(sum)
        for (j in 0 until len) {
            sum -= d[j]
            if (sum > 0.0) continue
            cent[nCluster] = pts[j].copy()
            break
        }
    }
    for (j in 0 until len) pts[j].group = nearest(pts[j], cent, nCent).first
}

fun lloyd(pts: LPoint, len: Int, nCluster: Int): LPoint {
    val cent = MutableList(nCluster) { origin }
    kpp(pts, len, cent)
    do {
        /* group element for centroids are used as counters */
        for (i in 0 until nCluster) {
            with (cent[i]) { x = 0.0; y = 0.0; group = 0 }
        }
        for (j in 0 until len) {
            val p = pts[j]
            val c = cent[p.group]
            with (c) { group++; x += p.x; y += p.y }
        }
        for (i in 0 until nCluster) {
            val c = cent[i]
            c.x /= c.group
            c.y /= c.group
        }
        var changed = 0

        /* find closest centroid of each point */
        for (j in 0 until len) {
            val p = pts[j]
            val minI = nearest(p, cent, nCluster).first
            if (minI != p.group) {
                changed++
                p.group = minI
            }
        }
    }
    while (changed > (len shr 10))  /* stop when 99.9% of points are good */

    for (i in 0 until nCluster) cent[i].group = i
    return cent
}

fun printEps(pts: LPoint, len: Int, cent: LPoint, nCluster: Int) {
    val colors = DoubleArray(nCluster * 3)
    for (i in 0 until nCluster) {
        colors[3 * i + 0] = (3 * (i + 1) % 11) / 11.0
        colors[3 * i + 1] = (7 * i % 11) / 11.0
        colors[3 * i + 2] = (9 * i % 11) / 11.0
    }
    var minX = hugeVal
    var minY = hugeVal
    var maxX = -hugeVal
    var maxY = -hugeVal
    for (j in 0 until len) {
        val p = pts[j]
        if (maxX < p.x) maxX = p.x
        if (minX > p.x) minX = p.x
        if (maxY < p.y) maxY = p.y
        if (minY > p.y) minY = p.y
    }
    val scale = minOf(W / (maxX - minX), H / (maxY - minY))
    val cx = (maxX + minX) / 2.0
    val cy = (maxY + minY) / 2.0

    print("%%!PS-Adobe-3.0\n%%%%BoundingBox: -5 -5 %${W + 10} ${H + 10}\n")
    print("/l {rlineto} def /m {rmoveto} def\n")
    print("/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n")
    print("/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath ")
    print("	gsave 1 setgray fill grestore gsave 3 setlinewidth")
    print(" 1 setgray stroke grestore 0 setgray stroke }def\n")
    val f1 = "%g %g %g setrgbcolor"
    val f2 = "%.3f %.3f c"
    val f3 = "\n0 setgray %g %g s"
    for (i in 0 until nCluster) {
        val c = cent[i]
        println(f1.format(colors[3 * i], colors[3 * i + 1], colors[3 * i + 2]))
        for (j in 0 until len) {
            val p = pts[j]
            if (p.group != i) continue
            println(f2.format((p.x - cx) * scale + W / 2, (p.y - cy) * scale + H / 2))
        }
        println(f3.format((c.x - cx) * scale + W / 2, (c.y - cy) * scale + H / 2))
    }
    print("\n%%%%EOF")
}

fun main(args: Array<String>) {
    val v = genXY(PTS, 10.0)
    val c = lloyd(v, PTS, K)
    printEps(v, PTS, c, K)
}
```



## Lua

```lua


local function load_data(npoints, radius)
  -- Generate random data points
  --
  local data = {}
  for i = 1,npoints do
    local ang = math.random() * (2.0 * math.pi)
    local rad = math.random() * radius
    data[i] = {x = math.cos(ang) * rad, y = math.sin(ang) * rad}
  end
  return data
end

local function print_eps(data, nclusters, centers, cluster)
  local WIDTH  = 400
  local HEIGHT = 400

  -- Print an EPS file with clustered points
  --
  local colors = {}
  for k = 1,nclusters do
    colors[3*k + 0] = (3 * k % 11) / 11.0
    colors[3*k + 1] = (7 * k % 11) / 11.0
    colors[3*k + 2] = (9 * k % 11) / 11.0
  end

  local max_x, max_y, min_x, min_y = -math.maxinteger, -math.maxinteger,
    math.maxinteger, math.maxinteger

  for i = 1,#data do
    if max_x < data[i].x then max_x = data[i].x end
    if min_x > data[i].x then min_x = data[i].x end
    if max_y < data[i].y then max_y = data[i].y end
    if min_y > data[i].y then min_y = data[i].y end
  end

  local scale = WIDTH / (max_x - min_x)
  if scale > HEIGHT / (max_y - min_y) then scale = HEIGHT / (max_y - min_y) end

  local cx = (max_x + min_x) / 2.0
  local cy = (max_y + min_y) / 2.0

  print(string.format("%%!PS-Adobe-3.0\n%%%%BoundingBox: -5 -5 %d %d",
    WIDTH + 10, HEIGHT + 10))
  print(string.format("/l {rlineto} def /m {rmoveto} def\n/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath gsave 1 setgray fill grestore gsave 3 setlinewidth 1 setgray stroke grestore 0 setgray stroke }def"
))
  -- print(string.format("%g %g %g setrgbcolor\n", 1, 2, 3))
  for k = 1,nclusters do
    print(string.format("%g %g %g setrgbcolor",
      colors[3*k], colors[3*k + 1], colors[3*k + 2]))

    for i = 1,#data do
      if cluster[i] == k then
        print(string.format("%.3f %.3f c",
          (data[i].x - cx) * scale + WIDTH  / 2.0,
          (data[i].y - cy) * scale + HEIGHT / 2.0))
      end
    end

    print(string.format("0 setgray %g %g s",
      (centers[k].x - cx) * scale + WIDTH  / 2.0,
      (centers[k].y - cy) * scale + HEIGHT / 2.0))
  end
  print(string.format("\n%%%%EOF"))
end

local function kmeans(data, nclusters, init)
  -- K-means Clustering
  --
  assert(nclusters > 0)
  assert(#data > nclusters)
  assert(init == "kmeans++" or init == "random")

  local diss = function(p, q)
    -- Computes the dissimilarity between points 'p' and 'q'
    --
    return math.pow(p.x - q.x, 2) + math.pow(p.y - q.y, 2)
  end

  -- Initialization
  --
  local centers = {} -- clusters centroids
  if init == "kmeans++" then
    local K = 1

    -- take one center c1, chosen uniformly at random from 'data'
    local i = math.random(1, #data)
    centers[K] = {x = data[i].x, y = data[i].y}

    -- repeat until we have taken 'nclusters' centers
    while K < nclusters do
      -- take a new center ck, choosing a point 'i' of 'data' with probability
      -- D(i)^2 / sum_{i=1}^n D(i)^2

      local D = {}
      local sum_D = 0.0
      for i = 1,#data do
        local min_d = nil

        for c = 1,K do
          local d = diss(data[i], centers[c])

          if min_d == nil or d < min_d then
            min_d = d
          end
        end

        D[i] = min_d
        sum_D = sum_D + min_d
      end

      sum_D = math.random() * sum_D
      for i = 1,#data do
        sum_D = sum_D - D[i]

        if sum_D <= 0 then
          K = K + 1
          centers[K] = {x = data[i].x, y = data[i].y}
          break
        end
      end
    end
  elseif init == "random" then
    for k = 1,nclusters do
      local i = math.random(1, #data)
      centers[k] = {x = data[i].x, y = data[i].y}
    end
  end

  -- Lloyd K-means Clustering
  --
  local cluster = {} -- k-partition
  for i = 1,#data do cluster[i] = 0 end

  local J = function()
    -- Computes the loss value
    --
    local loss = 0.0
    for i = 1,#data do
      loss = loss + diss(data[i], centers[cluster[i]])
    end
    return loss
  end

  local updated = false
  repeat
    -- update k-partition
    --
    local card = {}
    for k = 1,nclusters do
      card[k] = 0.0
    end

    updated = false
    for i = 1,#data do
      local min_d, min_k = nil, nil

      for k = 1,nclusters do
        local d = diss(data[i], centers[k])

        if min_d == nil or d < min_d then
          min_d, min_k = d, k
        end
      end

      if min_k ~= cluster[i] then updated = true end

      cluster[i]  = min_k
      card[min_k] = card[min_k] + 1.0
    end
    -- print("update k-partition: ", J())

    -- update centers
    --
    for k = 1,nclusters do
      centers[k].x = 0.0
      centers[k].y = 0.0
    end

    for i = 1,#data do
      local k = cluster[i]

      centers[k].x = centers[k].x + (data[i].x / card[k])
      centers[k].y = centers[k].y + (data[i].y / card[k])
    end
    -- print("    update centers: ", J())
  until updated == false

  return centers, cluster, J()
end

 ------------------------------------------------------------------------------
 ---- MAIN --------------------------------------------------------------------

local N_POINTS   = 100000  -- number of points
local N_CLUSTERS = 11      -- number of clusters

local data = load_data(N_POINTS, N_CLUSTERS)
centers, cluster, loss = kmeans(data, N_CLUSTERS, "kmeans++")
-- print("Loss: ", loss)
-- for k = 1,N_CLUSTERS do
--   print("center.x: ", centers[k].x, " center.y: ", centers[k].y)
-- end
print_eps(data, N_CLUSTERS, centers, cluster)

```



## Mathematica

'''Solution - Initial kmeans code comes from http://mathematica.stackexchange.com/questions/7441/k-means-clustering, now extended to kmeans++ by introducing the function initM.
 Was not able to upload pictures of the result...''':
<lang>initM[list_List, k_Integer, distFunc_Symbol] :=
  Module[{m = {RandomChoice[list]}, n, d},
   While[Length[m] < k,
    n = RandomChoice@Nearest[m, #] & /@ list;
    d = Apply[distFunc, Transpose[{n, list}], {1}];
    m = Append[m, RandomChoice[d -> list]]
    ];
   m
   ];
kmeanspp[list_, k_,
   opts : OptionsPattern[{DistanceFunction ->
       SquaredEuclideanDistance, "RandomSeed" -> {}}]] :=
  BlockRandom[SeedRandom[OptionValue["RandomSeed"]];
   Module[{m = initM[list, k, OptionValue[DistanceFunction]], update,
     partition, clusters}, update[] := m = Mean /@ clusters;
    partition[_] := (clusters =
       GatherBy[list,
        RandomChoice@
          Nearest[m, #, (# -> OptionValue[#] &@DistanceFunction)] &];
      update[]);
    FixedPoint[partition, list];
    {clusters, m}
    ]
   ];
```


Extra credit:

1. no changes required for N dimensions, it juts works.

2. random data can be generated with

```txt
dim = 3;
points = 3000;
l = RandomReal[1, {points, dim}];
```

or

```txt
l = Select[ RandomReal[{-1, 1}, {points,2}],
   EuclideanDistance[#, {0, 0}] <= 1 &];
```

or

```txt
x1 = RandomVariate[MultinormalDistribution[{0, 0}, {{1, 0}, {0, 20}}],
   points];
x2 =
 RandomVariate[MultinormalDistribution[{10, 0}, {{1, 0}, {0, 20}}],
  points];
l = Join[x1, x2];

```

3. data can be visualized with
2D:

```txt

dim = 2;
points = 30000;
l = RandomReal[1, {points, dim}];
k = 6
r1 = kmeanspp[l, k];
p1 = ListPlot[r1[[1]]];
p2 = ListPlot[r1[[2]],PlotMarkers -> {"#"}];
Show[{p1, p2}]
```

3D:

```txt
dim = 3;
points = 3000;
l = RandomReal[1, {points, dim}];
k = 6
r1 = kmeanspp[l, k];
p1 = ListPointPlot3D[r1[[1]]];
p2 = ListPointPlot3D[r1[[2]]];
Show[{p1, p2}]
```




Another version

KMeans[k_, data_] :=
 Module[{Renew, Label, Iteration},
  clusters = RandomSample[data, k];
  Label[clusters_] :=
   Flatten[Table[
     Ordering[
      Table[EuclideanDistance[data[[i]], clusters[[j]]], {j,
        Length[clusters]}], 1], {i, Length[data]}]];
  Renew[labels_] :=
   Module[{position},
    position = PositionIndex[labels];
    Return[Table[Mean[data[[position[[i]]]]], {i, Length[position]}]]];
  Iteration[labels_, clusters_] :=

   Module[{newlabels, newclusters},
    newclusters = Renew[labels];
    newlabels = Label[newclusters];
    If[newlabels == labels, labels,
     Iteration[newlabels, newclusters]]];
  Return[Iteration[clusters, Label[clusters]]]]


## Perl 6

We use Complex numbers to represent points in the plane.  We feed the algorithm with three artificially made clouds of points so we can easily see if the output makes sense.

```perl6
sub postfix:«-means++»(Int $K) {
    return sub (@data) {
        my @means = @data.pick;
        until @means == $K {
            my @cumulD2 = [\+] @data.map: -> $x {
                min @means.map: { abs($x - $_)**2 }
            }
            my $rand = rand * @cumulD2[*-1];
            @means.push: @data[
                (^@data).first: { @cumulD2[$_] > $rand }
            ];
        }
        sub cluster { @data.classify: -> $x { @means.min: { abs($_ - $x) } } }
        loop (
            my %cluster;
            $*TOLERANCE < [+] (@means Z- keys (%cluster = cluster))».abs X** 2;
            @means = %cluster.values.map( { .elems R/ [+] @$_ } )
        ) { ; }
        return @means;
    }
}
 
my @centers = 0, 5, 3 + 2i;
my @data = flat @centers.map: { ($_ + .5 - rand + (.5 - rand) * i) xx 100 }
@data.=pick(*);
.say for 3-means++(@data);
```


```txt
5.04622376429502+0.0145269848483031i
0.0185674577571743+0.0298199687431731i
2.954898072093+2.14922298688815i
```



## Phix

I nicked the initial dataset creation from Go, as an alternative

```Phix
-- demo\rosetta\K_means_clustering.exw
--  Press F5 to restart
include pGUI.e

Ihandle dlg, canvas, timer
cdCanvas cddbuffer, cdcanvas

constant TITLE = "K-means++ clustering"

constant useGoInitialData = false       -- (not very well centered)

constant N = 30000,                     -- number of points
         K = 16                         -- number of clusters

sequence {Px, Py, Pc} @= repeat(0,N),   -- coordinates of points and their cluster
         {Cx, Cy} @= repeat(0,K)        -- coordinates of centroid of cluster

constant colours = {CD_RED, CD_DARK_RED, CD_BLUE, CD_DARK_BLUE, CD_CYAN, CD_DARK_CYAN,
                    CD_GREEN, CD_DARK_GREEN, CD_MAGENTA, CD_DARK_MAGENTA, CD_YELLOW,
                    CD_DARK_YELLOW, CD_DARK_ORANGE, CD_INDIGO, CD_PURPLE, CD_DARK_GREY}
if length(colours)<K then ?9/0 end if

function Centroid()
-- Find new centroids of points grouped with current centroids
bool change = false
    for c=1 to K do                         -- for each centroid...
        integer x=0, y=0, count:= 0;        -- find new centroid
        for i=1 to N do                     -- for all points
            if Pc[i] = c then               -- grouped with current centroid...
                x += Px[i]
                y += Py[i]
                count += 1
            end if
        end for
        if count!=0 then
            x = floor(x/count)
            y = floor(y/count)
            if Cx[c]!=x
            or Cy[c]!=y then
                Cx[c] = x
                Cy[c] = y
                change:= true
            end if
        end if
    end for
    return change
end function

function sq(atom x) return x*x end function

procedure Voronoi()             -- Group points with their nearest centroid
    integer d2,                 -- distance squared,
            min_d2              -- minimum distance squared
    for i=1 to N do             -- for each point...
        min_d2 := #3FFFFFFF     -- find closest centroid
        for c=1 to K do
            d2 := sq(Px[i]-Cx[c]) + sq(Py[i]-Cy[c])
            if d2<min_d2 then
                min_d2 := d2
                Pc[i] := c      -- update closest centroid
            end if
        end for
    end for
end procedure

function rand_xy()              -- Return random X,Y biased for polar coordinates
    atom d := rand(240)-1,              -- distance: 0..239
         a := rnd()*2*PI                -- angle:    0..2pi
    integer x:= floor(d*cos(a))+320,    -- rectangular coords centered on screen
            y:= floor(d*sin(a))+240     --     (that is, assuming 640x480)
    return {x,y}
end function

--This little bit is copied from/based on Go:
constant k = K,
         nPoints = N,
         xBox = 300,
         yBox = 200,
         stdv = 30

function genECData()
    sequence orig = repeat({0,0}, k),
             data = repeat({0,0,0}, nPoints)
    integer n = 0, nk = k
    for i=1 to k do
        integer x := rand(xBox)+320,
                y := rand(yBox)+240
        orig[i] = {x, y}
        for j=1 to floor((nPoints-n)/nk) do
            n += 1
            atom d := rand(stdv)-1,             -- distance: 0..239
                 a := rnd()*2*PI                -- angle:    0..2pi
            integer nx:= floor(d*cos(a))+x, -- rectangular coords centered on screen
                    ny:= floor(d*sin(a))+y  --     (that is, assuming 640x480)
            data[n] = {nx,ny,i}
        end for
        nk -= 1
    end for
    if n!=nPoints then ?9/0 end if
    return {orig, data}
end function
--</Go ends>

integer iteration = 0

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    if iteration=0 then
        if useGoInitialData then
            sequence {origins,data} = genECData()
            {Px, Py, Pc} = columnize(data)
            {Cx, Cy} = columnize(origins)
        else
            for i=1 to N do {Px[i],Py[i]} = rand_xy() end for   -- random set of points
            for i=1 to K do {Cx[i],Cy[i]} = rand_xy() end for   -- random set of cluster centroids
        end if
    end if
    sequence {r,g,b} @ = repeat(0,w*h)
    Voronoi()
    bool change := Centroid()
    for i=1 to N do
        integer idx = Px[i]+(Py[i]-1)*w
        {r[idx],g[idx],b[idx]} = cdDecodeColor(colours[Pc[i]])
    end for
    for i=1 to K do
        integer idx = Cx[i]+(Cy[i]-1)*w
        {r[idx],g[idx],b[idx]} = cdDecodeColor(CD_WHITE)
    end for
    cdCanvasPutImageRectRGB(cddbuffer, w, h, {r,g,b})
    cdCanvasFlush(cddbuffer)
    if change then
        iteration += 1
        IupSetStrAttribute(dlg, "TITLE", "%s (iteration %d)",{TITLE,iteration})
    else
        IupSetInt(timer,"RUN",0)                -- (stop timer)
        IupSetStrAttribute(dlg, "TITLE", TITLE)
    end if
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=K_F5 then
        iteration = 0
        IupSetInt(timer,"RUN",1)                -- (restart timer)
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x480")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    timer = IupTimer(Icallback("timer_cb"), 100)

    dlg = IupDialog(canvas,"DIALOGFRAME=YES")
    IupSetAttribute(dlg, "TITLE", TITLE)
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure

main()
```

Probably the hardest part of handling more than 2 dimensions would be deleteing all
the GUI code, or modifying it to produce an n-dimensional representation. Obviously
you would need Pz and Cz, or replace them with n-tuples, and to replace rand_xy().


## Python

```python
from math import pi, sin, cos
from collections import namedtuple
from random import random, choice
from copy import copy

try:
    import psyco
    psyco.full()
except ImportError:
    pass


FLOAT_MAX = 1e100


class Point:
    __slots__ = ["x", "y", "group"]
    def __init__(self, x=0.0, y=0.0, group=0):
        self.x, self.y, self.group = x, y, group


def generate_points(npoints, radius):
    points = [Point() for _ in xrange(npoints)]

    # note: this is not a uniform 2-d distribution
    for p in points:
        r = random() * radius
        ang = random() * 2 * pi
        p.x = r * cos(ang)
        p.y = r * sin(ang)

    return points


def nearest_cluster_center(point, cluster_centers):
    """Distance and index of the closest cluster center"""
    def sqr_distance_2D(a, b):
        return (a.x - b.x) ** 2  +  (a.y - b.y) ** 2

    min_index = point.group
    min_dist = FLOAT_MAX

    for i, cc in enumerate(cluster_centers):
        d = sqr_distance_2D(cc, point)
        if min_dist > d:
            min_dist = d
            min_index = i

    return (min_index, min_dist)


def kpp(points, cluster_centers):
    cluster_centers[0] = copy(choice(points))
    d = [0.0 for _ in xrange(len(points))]

    for i in xrange(1, len(cluster_centers)):
        sum = 0
        for j, p in enumerate(points):
            d[j] = nearest_cluster_center(p, cluster_centers[:i])[1]
            sum += d[j]

        sum *= random()

        for j, di in enumerate(d):
            sum -= di
            if sum > 0:
                continue
            cluster_centers[i] = copy(points[j])
            break

    for p in points:
        p.group = nearest_cluster_center(p, cluster_centers)[0]


def lloyd(points, nclusters):
    cluster_centers = [Point() for _ in xrange(nclusters)]

    # call k++ init
    kpp(points, cluster_centers)

    lenpts10 = len(points) >> 10

    changed = 0
    while True:
        # group element for centroids are used as counters
        for cc in cluster_centers:
            cc.x = 0
            cc.y = 0
            cc.group = 0

        for p in points:
            cluster_centers[p.group].group += 1
            cluster_centers[p.group].x += p.x
            cluster_centers[p.group].y += p.y

        for cc in cluster_centers:
            cc.x /= cc.group
            cc.y /= cc.group

        # find closest centroid of each PointPtr
        changed = 0
        for p in points:
            min_i = nearest_cluster_center(p, cluster_centers)[0]
            if min_i != p.group:
                changed += 1
                p.group = min_i

        # stop when 99.9% of points are good
        if changed <= lenpts10:
            break

    for i, cc in enumerate(cluster_centers):
        cc.group = i

    return cluster_centers


def print_eps(points, cluster_centers, W=400, H=400):
    Color = namedtuple("Color", "r g b");

    colors = []
    for i in xrange(len(cluster_centers)):
        colors.append(Color((3 * (i + 1) % 11) / 11.0,
                            (7 * i % 11) / 11.0,
                            (9 * i % 11) / 11.0))

    max_x = max_y = -FLOAT_MAX
    min_x = min_y = FLOAT_MAX

    for p in points:
        if max_x < p.x: max_x = p.x
        if min_x > p.x: min_x = p.x
        if max_y < p.y: max_y = p.y
        if min_y > p.y: min_y = p.y

    scale = min(W / (max_x - min_x),
                H / (max_y - min_y))
    cx = (max_x + min_x) / 2
    cy = (max_y + min_y) / 2

    print "%%!PS-Adobe-3.0\n%%%%BoundingBox: -5 -5 %d %d" % (W + 10, H + 10)

    print ("/l {rlineto} def /m {rmoveto} def\n" +
           "/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n" +
           "/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath " +
           "   gsave 1 setgray fill grestore gsave 3 setlinewidth" +
           " 1 setgray stroke grestore 0 setgray stroke }def")

    for i, cc in enumerate(cluster_centers):
        print ("%g %g %g setrgbcolor" %
               (colors[i].r, colors[i].g, colors[i].b))

        for p in points:
            if p.group != i:
                continue
            print ("%.3f %.3f c" % ((p.x - cx) * scale + W / 2,
                                    (p.y - cy) * scale + H / 2))

        print ("\n0 setgray %g %g s" % ((cc.x - cx) * scale + W / 2,
                                        (cc.y - cy) * scale + H / 2))

    print "\n%%%%EOF"


def main():
    npoints = 30000
    k = 7 # # clusters

    points = generate_points(npoints, 10)
    cluster_centers = lloyd(points, k)
    print_eps(points, cluster_centers)


main()
```



## Racket

The k-means clustering:

```racket

#lang racket
(require racket/dict
         math/distributions)

;; Divides the set of points into k clusters
;; using the standard k-means clustering algorithm
(define (k-means data k #:initialization (init k-means++))
  (define (iteration centroids)
    (map centroid (clusterize data centroids)))
  (fixed-point iteration (init data k) #:same-test small-shift?))

;; Finds the centroid for a set of points
(define (centroid pts)
  (vector-map (curryr / (length pts))
       (for/fold ([sum (car pts)]) ([x (in-list (cdr pts))])
         (vector-map + x sum))))

;; Divides the set of points into clusters
;; using given centroids
(define (clusterize data centroids)
  (for*/fold ([res (map list centroids)]) ([x (in-list data)])
    (define c (argmin (distanse-to x) centroids))
    (dict-set res c (cons x (dict-ref res c)))))

;; Stop criterion: all centroids change their positions
;; by less then 0.1% of the minimal distance between centroids.
(define (small-shift? c1 c2)
  (define min-distance
    (apply min
           (for*/list ([x (in-list c2)]
                       [y (in-list c2)] #:unless (equal? x y))
             ((metric) x y))))
  (for/and ([a (in-list c1)] [b (in-list c2)])
    (< ((metric) a b) (* 0.001 min-distance))))

```


Initialization methods


```racket

;; picks k points from a dataset randomly
(define (random-choice data k)
  (for/list ([i (in-range k)])
    (list-ref data (random (length data)))))

;; uses k-means++ algorithm
(define (k-means++ data k)
  (for/fold ([centroids (random-choice data 1)]) ([i (in-range (- k 1))])
    (define weights
      (for/list ([x (in-list data)])
        (apply min (map (distanse-to x) centroids))))
    (define new-centroid
      (sample (discrete-dist data weights)))
    (cons new-centroid centroids)))

```


Different metrics


```racket

(define (euclidean-distance a b)
  (for/sum ([x (in-vector a)] [y (in-vector b)])
    (sqr (- x y))))

(define (manhattan-distance a b)
  (for/sum ([x (in-vector a)] [y (in-vector b)])
    (abs (- x y))))

(define metric (make-parameter euclidean-distance))
(define (distanse-to x) (curry (metric) x))

```


The fixed point operator


```racket

(define (fixed-point f x0 #:same-test [same? equal?])
  (let loop ([x x0] [fx (f x0)])
    (if (same? x fx) fx (loop fx (f fx)))))

```


Creating sample clusters

[[File:Circle1.png|200px|thumb|right|Clustering using k-means++ method.]]
[[File:Circle2.png|200px|thumb|right|Clustering using random seeds.]]
[[File:Circle3.png|200px|thumb|right|Clustering using k-means++ method with Manhattan distanse.]]
[[File:Clouds1.png|200px|thumb|right|Clustering using k-means++ method almost always handles the difficult case.]]
[[File:Clouds2.png|200px|thumb|right|Clustering using random seeds may give poor results.]]



```racket

(define (gaussian-cluster N
                          #:stdev (σ 1)
                          #:center (r0 #(0 0))
                          #:dim (d 2))
  (for/list ([i (in-range N)])
    (define r (for/vector ([j (in-range d)]) (sample (normal-dist 0 σ))))
    (vector-map + r r0)))

(define (uniform-cluster N
                         #:radius (R 1)
                         #:center (r0 #(0 0)))
  (for/list ([i (in-range N)])
    (define r (* R (sqrt (sample (uniform-dist)))))
    (define φ (* 2 pi (sample (uniform-dist))))
    (vector-map + r0 (vector (* r (cos φ)) (* r (sin φ))))))

```


Visualization


```racket

(require plot)

(define (show-clustering data k #:method (method k-means++))
  (define c (k-means data k #:initialization method))
  (display
   (plot
    (append
     (for/list ([d (clusterize data c)]
                [i (in-naturals)])
       (points d #:color i #:sym 'fullcircle1))
     (list (points c
                   #:sym 'fullcircle7
                   #:fill-color 'yellow
                   #:line-width 3)))
    #:title (format "Initializing by ~a" (object-name method)))))

```


Testing


```racket

(module+ test
  (define circle (uniform-cluster 30000))
  ; using k-means++ method
  (show-clustering circle 6)
  ; using standard k-means method
  (show-clustering circle 6 #:method random-choice)
  ; using manhattan distance
  (parameterize ([metric manhattan-distance])
    (show-clustering circle 6)))

```


The difficult case.


```racket

(module+ test
  (define clouds
    (append
     (gaussian-cluster 1000 #:stdev 0.5 #:center #(0 0))
     (gaussian-cluster 1000 #:stdev 0.5 #:center #(2 3))
     (gaussian-cluster 1000 #:stdev 0.5 #:center #(2.5 -1))
     (gaussian-cluster 1000 #:stdev 0.5 #:center #(6 0))))

  ; using k-means++ method
  (show-clustering clouds 4)
  ; using standard k-means method
  (show-clustering clouds 4 #:method random-choice))

```


Multi-dimensional case.


```racket

(module+ test
  (define 5d-data
    (append
     (gaussian-cluster 1000 #:dim 5 #:center #(2 0 0 0 0))
     (gaussian-cluster 1000 #:dim 5 #:center #(0 2 0 0 0))
     (gaussian-cluster 1000 #:dim 5 #:center #(0 0 2 0 0))
     (gaussian-cluster 1000 #:dim 5 #:center #(0 0 0 2 0))
     (gaussian-cluster 1000 #:dim 5 #:center #(0 0 0 0 2))))

  (define centroids (k-means 5d-data 5))

  (map (curry vector-map round) centroids))

```

Output shows that centroids were found correctly.

```txt

(#(-0.0 2.0 -0.0 0.0 0.0)
 #(0.0 0.0 -0.0 2.0 -0.0)
 #(2.0 -0.0 -0.0 -0.0 -0.0)
 #(-0.0 -0.0 2.0 0.0 0.0)
 #(-0.0 -0.0 0.0 0.0 2.0))

```



## Rust

{{trans|Python}} (the initial point selection part)

```rust
extern crate csv;
extern crate getopts;
extern crate gnuplot;
extern crate nalgebra;
extern crate num;
extern crate rand;
extern crate rustc_serialize;
extern crate test;

use getopts::Options;
use gnuplot::{Axes2D, AxesCommon, Color, Figure, Fix, PointSize, PointSymbol};
use nalgebra::{DVector, Iterable};
use rand::{Rng, SeedableRng, StdRng};
use rand::distributions::{IndependentSample, Range};
use std::f64::consts::PI;
use std::env;

type Point = DVector<f64>;

struct Cluster<'a> {
    members: Vec<&'a Point>,
    center: Point,
}

struct Stats {
    centroids: Vec<Point>,
    mean_d_from_centroid: DVector<f64>,
}

/// DVector doesn't implement BaseFloat, so a custom distance function is required.
fn sqdist(p1: &Point, p2: &Point) -> f64 {
    (p1.clone() - p2.clone()).iter().map(|x| x * x).fold(0f64, |a, b| a + b)
}

/// Returns (distance^2, index) tuple of winning point.
fn nearest(p: &Point, candidates: &Vec<Point>) -> (f64, usize) {
    let (dsquared, the_index) = candidates.iter()
                                          .enumerate()
                                          .fold((sqdist(p, &candidates[0]), 0),
                                                |(d, index), next| {
                                                    let dprime = sqdist(p, &candidates[next.0]);
                                                    if dprime < d {
                                                        (dprime, next.0)
                                                    } else {
                                                        (d, index)
                                                    }
                                                });
    (dsquared, the_index)
}

/// Computes starting centroids and makes initial assignments.
fn kpp(points: &Vec<Point>, k: usize, rng: &mut StdRng) -> Stats {
    let mut centroids: Vec<Point> = Vec::new();
    // Random point for first centroid guess:
    centroids.push(points[rng.gen::<usize>() % points.len()].clone());
    let mut dists: Vec<f64> = vec![0f64; points.len()];

    for _ in 1..k {
        let mut sum = 0f64;
        for (j, p) in points.iter().enumerate() {
            let (dsquared, _) = nearest(&p, &centroids);
            dists[j] = dsquared;
            sum += dsquared;
        }

        // This part chooses the next cluster center with a probability proportional to d^2
        sum *= rng.next_f64();
        for (j, d) in dists.iter().enumerate() {
            sum -= *d;
            if sum <= 0f64 {
                centroids.push(points[j].clone());
                break;
            }
        }
    }

    let clusters = assign_clusters(points, &centroids);
    compute_stats(&clusters)
}

fn assign_clusters<'a>(points: &'a Vec<Point>, centroids: &Vec<Point>) -> Vec<Cluster<'a>> {
    let mut clusters: Vec<Cluster> = Vec::new();

    for _ in 0..centroids.len() {
        clusters.push(Cluster {
            members: Vec::new(),
            center: DVector::new_zeros(points[0].len()),
        });
    }

    for p in points.iter() {
        let (_, nearest_index) = nearest(p, centroids);
        clusters[nearest_index].center = clusters[nearest_index].center.clone() + p.clone();
        clusters[nearest_index].members.push(p);
    }

    for i in 0..clusters.len() {
        clusters[i].center = clusters[i].center.clone() / clusters[i].members.len() as f64;
    }

    clusters
}

/// Computes centroids and mean-distance-from-centroid for each cluster.
fn compute_stats(clusters: &Vec<Cluster>) -> Stats {
    let mut centroids = Vec::new();
    let mut means_vec = Vec::new();

    for c in clusters.iter() {
        let pts = &c.members;
        let seed: DVector<f64> = DVector::new_zeros(pts[0].len());
        let centroid = pts.iter().fold(seed, |a, &b| a + b.clone()) / pts.len() as f64;
        means_vec.push(pts.iter().fold(0f64, |acc, pt| acc + sqdist(pt, &centroid).sqrt()) /
                       pts.len() as f64);
        centroids.push(centroid);
    }

    Stats {
        centroids: centroids,
        mean_d_from_centroid: DVector::from_slice(means_vec.len(), means_vec.as_slice()),
    }
}

fn lloyd<'a>(points: &'a Vec<Point>,
             k: usize,
             stoppage_delta: f64,
             max_iter: u32,
             rng: &mut StdRng)
             -> (Vec<Cluster<'a>>, Stats) {

    let mut clusters = Vec::new();
    // Choose starting centroids and make initial assignments
    let mut stats = kpp(points, k, rng);

    for i in 1..max_iter {
        let last_means: DVector<f64> = stats.mean_d_from_centroid.clone();
        clusters = assign_clusters(points, &stats.centroids);
        stats = compute_stats(&clusters);
        let err = sqdist(&stats.mean_d_from_centroid, &last_means).sqrt();
        if err < stoppage_delta {
            println!("Stoppage condition reached on iteration {}", i);
            return (clusters, stats);
        }
        // Console output
        print!("Iter {}: ", i);
        for (cen, mu) in stats.centroids.iter().zip(stats.mean_d_from_centroid.iter()) {
            print_dvec(cen);
            print!(" {:1.2} | ", mu);
        }
        print!("{:1.5}\n", err);
    }

    println!("Stoppage condition not reached by iteration {}", max_iter);
    (clusters, stats)
}

/// Uniform sampling on the unit disk.
fn generate_points(n: u32, rng: &mut StdRng) -> Vec<Point> {
    let r_range = Range::new(0f64, 1f64);
    let theta_range = Range::new(0f64, 2f64 * PI);
    let mut points: Vec<Point> = Vec::new();

    for _ in 0..n {
        let root_r = r_range.ind_sample(rng).sqrt();
        let theta = theta_range.ind_sample(rng);
        points.push(DVector::<f64>::from_slice(2, &[root_r * theta.cos(), root_r * theta.sin()]));
    }

    points
}

// Plot clusters (2d only). Closure idiom allows us to borrow and mutate the Axes2D.
fn viz(clusters: Vec<Cluster>, stats: Stats, k: usize, n: u32, e: f64) {
    let mut fg = Figure::new();
    {
        let prep = |fg: &mut Figure| {
            let axes: &mut Axes2D = fg.axes2d();
            let title: String = format!("k = {}, n = {}, e = {:4}", k, n, e);
            let centroids_x = stats.centroids.iter().map(|c| c[0]);
            let centroids_y = stats.centroids.iter().map(|c| c[1]);
            for cluster in clusters.iter() {
                axes.points(cluster.members.iter().map(|p| p[0]),
                            cluster.members
                                   .iter()
                                   .map(|p| p[1]),
                            &[PointSymbol('O'), PointSize(0.25)]);
            }
            axes.set_aspect_ratio(Fix(1.0))
                .points(centroids_x,
                        centroids_y,
                        &[PointSymbol('o'), PointSize(1.5), Color("black")])
                .set_title(&title[..], &[]);
        };
        prep(&mut fg);
    }
    fg.show();
}

fn print_dvec(v: &DVector<f64>) {
    print!("(");
    for elem in v.at.iter().take(v.len() - 1) {
        print!("{:+1.2}, ", elem)
    }
    print!("{:+1.2})", v.at.iter().last().unwrap());
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [options]", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut k: usize = 7;
    let mut n: u32 = 30000;
    let mut e: f64 = 1e-3;
    let max_iterations = 100u32;

    let mut opts = Options::new();
    opts.optflag("?", "help", "Print this help menu");
    opts.optopt("k",
                "",
                "Number of clusters to assign (default: 7)",
                "<clusters>");
    opts.optopt("n",
                "",
                "Operate on this many points on the unit disk (default: 30000)",
                "<pts>");
    opts.optopt("e",
                "",
                "Min delta in norm of successive cluster centroids to continue (default: 1e-3)",
                "<eps>");
    opts.optopt("f", "", "Read points from file (overrides -n)", "<csv>");

    let program = args[0].clone();
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };
    if matches.opt_present("?") {
        print_usage(&program, opts);
        return;
    }
    match matches.opt_str("k") {
        None => {}
        Some(x) => k = x.parse::<usize>().unwrap(),
    };
    match matches.opt_str("n") {
        None => {}
        Some(x) => n = x.parse::<u32>().unwrap(),
    };
    match matches.opt_str("e") {
        None => {}
        Some(x) => e = x.parse::<f64>().unwrap(),
    };

    let seed: &[_] = &[1, 2, 3, 4];
    let mut rng: StdRng = SeedableRng::from_seed(seed);

    let mut points: Vec<Point>;

    match matches.opt_str("f") {
        None => {
            // Proceed with random 2d data
            points = generate_points(n, &mut rng)
        }
        Some(file) => {
            points = Vec::new();
            let mut rdr = csv::Reader::from_file(file.clone()).unwrap();
            for row in rdr.records().map(|r| r.unwrap()) {
                // row is Vec<String>
                let floats: Vec<f64> = row.iter().map(|s| s.parse::<f64>().unwrap()).collect();
                points.push(DVector::<f64>::from_slice(floats.len(), floats.as_slice()));
            }
            assert!(points.iter().all(|v| v.len() == points[0].len()));
            n = points.len() as u32;
            println!("Read {} points from {}", points.len(), file.clone());
        }
    };

    assert!(points.len() >= k);
    let (clusters, stats) = lloyd(&points, k, e, max_iterations, &mut rng);

    println!(" k       centroid{}mean dist    pop",
             std::iter::repeat(" ").take((points[0].len() - 2) * 7 + 7).collect::<String>());
    println!("===  {}
### ========  ==
",
             std::iter::repeat("=").take(points[0].len() * 7 + 2).collect::<String>());
    for i in 0..clusters.len() {
        print!(" {:>1}    ", i);
        print_dvec(&stats.centroids[i]);
        print!("      {:1.2}       {:>4}\n",
               stats.mean_d_from_centroid[i],
               clusters[i].members.len());
    }

    if points[0].len() == 2 {
        viz(clusters, stats, k, n, e)
    }
}

```

[Plots exist but file upload is broken at the moment.]

Output of run on 30k points on the unit disk:

```txt

Stoppage condition reached on iteration 10
 k       centroid       mean dist    pop

###   ================  ===========  ==

 0    (+0.34, -0.61)      0.27       4425
 1    (+0.70, -0.01)      0.26       4293
 2    (-0.37, -0.59)      0.27       4319
 3    (+0.35, +0.61)      0.26       4368
 4    (-0.00, +0.01)      0.25       4095
 5    (-0.34, +0.62)      0.26       4190
 6    (-0.71, +0.04)      0.26       4310

```

Extra credit 4:  Use of the DVector type in the nalgebra crate gives some arithmetic vector operations for free, and generalizes to n dimensions with no work. Here is the output of running this program on the 4-D Fisher Iris data (I don't think this data clusters well):

```txt

k       centroid                     mean dist    pop

###   ==============================  ===========  ==

0    (+5.00, +3.43, +1.46, +0.25)      0.49         49
1    (+5.88, +2.74, +4.39, +1.43)      0.73         61
2    (+6.85, +3.08, +5.72, +2.05)      0.73         39

```



## Scheme


The eps output is translated from the C version.
The 'tester' functions demonstrate the unit square and the unit circle, with eps graphical output, and a 5D unit square, with text-only output.
Nothing special is needed to handle multiple dimensions: all points are represented as lists, which the euclidean distance function works through in a loop.


```scheme

(import (scheme base) ; headers for R7RS Scheme
        (scheme file)
        (scheme inexact)
        (scheme write)
        (srfi 1 lists)
        (srfi 27 random-bits))

;; calculate euclidean distance between points, any dimension
(define (euclidean-distance pt1 pt2)
  (sqrt (apply + (map (lambda (x y) (square (- x y))) pt1 pt2))))

;; input
;; - K: the target number of clusters K
;; - data: a list of points in the Cartesian plane
;; output
;; - a list of K centres
(define (kmeans++ K data)
  (define (select-uniformly data)
    (let loop ((index (random-integer (length data))) ; uniform selection of index
               (rem data)
               (front '()))
      (if (zero? index)
        (values (car rem) (append (reverse front) (cdr rem)))
        (loop (- index 1) (cdr rem) (cons (car rem) front)))))
  ;
  (define (select-weighted centres data)
    (define (distance-to-nearest datum)
      (apply min (map (lambda (c) (euclidean-distance c datum)) centres)))
    ;
    (let* ((weights (map (lambda (d) (square (distance-to-nearest d))) data))
           (target-weight (* (apply + weights) (random-real))))
      (let loop ((rem data)
                 (front '())
                 (weight-sum 0.0)
                 (wgts weights))
        (if (or (>= weight-sum target-weight) (null? (cdr rem)))
          (values (car rem) (append (reverse front) (cdr rem)))
          (loop (cdr rem)
                (cons (car rem) front)
                (+ weight-sum (car wgts))
                (cdr weights))))))
  ;
  (let-values (((pt rem) (select-uniformly data)))
              (let loop ((centres (list pt))
                         (items rem))
                (if (= (length centres) K)
                  centres
                  (let-values (((pt rem) (select-weighted centres items)))
                              (loop (cons pt centres)
                                    rem))))))

;; assign a point into a cluster
;; input: a point and a list of cluster centres
;; output: index of cluster centre
(define (assign-cluster pt centres)
  (let* ((distances (map (lambda (centre) (euclidean-distance centre pt)) centres))
         (smallest (apply min distances)))
    (list-index (lambda (d) (= d smallest)) distances)))

;; input
;; - num: the number of clusters K
;; - data: a list of points in the Cartesian plane
;; output
;; - list of K centres
(define (cluster K data)
  (define (centroid-for-cluster i assignments)
    (let* ((cluster (map cadr (filter (lambda (a-d) (= (car a-d) i)) (zip assignments data))))
           (length-cluster (length cluster)))
      ; compute centroid for cluster
      (map (lambda (vals) (/ (apply + vals) length-cluster)) (apply zip cluster))))
  ;
  (define (update-centres assignments)
    (map (lambda (i) (centroid-for-cluster i assignments)) (iota K)))
  ;
  (let ((initial-centres (kmeans++ K data)))
    (let loop ((centres initial-centres)
               (assignments (map (lambda (datum) (assign-cluster datum initial-centres)) data)))
      (let* ((new-centres (update-centres assignments))
             (new-assignments (map (lambda (datum) (assign-cluster datum new-centres)) data)))
        (if (equal? assignments new-assignments)
          new-centres
          (loop new-centres new-assignments))))))

;; using eps output, based on that in C - only works for 2D points
(define (save-as-eps filename data clusters K)
  (when (file-exists? filename) (delete-file filename))
  (with-output-to-file
    filename
    (lambda ()
      (let* ((W 400)
             (H 400)
             (colours (make-vector (* 3 K) 0.0))
             (max-x (apply max (map car data)))
             (min-x (apply min (map car data)))
             (max-y (apply max (map cadr data)))
             (min-y (apply min (map cadr data)))
             (scale (min (/ W (- max-x min-x))
                         (/ H (- max-y min-y))))
             (cx (/ (+ max-x min-x) 2))
             (cy (/ (+ max-y min-y) 2)))

        ;; set up colours
        (for-each
          (lambda (i)
            (vector-set! colours (+ (* i 3) 0) (inexact (/ (modulo (* 3 (+ i 1)) 11) 11)))
            (vector-set! colours (+ (* i 3) 1) (inexact (/ (modulo (* 7 i) 11) 11)))
            (vector-set! colours (+ (* i 3) 2) (inexact (/ (modulo (* 9 i) 11) 11))))
          (iota K))

        (display ;; display header
          (string-append
            "%!PS-Adobe-3.0\n%%BoundingBox: -5 -5 "
            (number->string (+ 10 W)) " " (number->string (+ 10 H)) "\n"
            "/l {rlineto} def /m {rmoveto} def\n"
            "/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n"
            "/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath "
            "	gsave 1 setgray fill grestore gsave 3 setlinewidth"
            " 1 setgray stroke grestore 0 setgray stroke }def\n"))

        ;; display points
        (for-each ; top loop runs over the clusters
          (lambda (i)
            (display
              (string-append (number->string (vector-ref colours (* i 3)))
                             " "
                             (number->string (vector-ref colours (+ (* i 3) 1)))
                             " "
                             (number->string (vector-ref colours (+ (* i 3) 2)))
                             " setrgbcolor\n"))
            (for-each ;loop over points in cluster
              (lambda (pt)
                (when (= i (assign-cluster pt clusters))
                  (display
                    (string-append (number->string (+ (* (- (car pt) cx) scale) (/ W 2)))
                                   " "
                                   (number->string (+ (* (- (cadr pt) cy) scale) (/ H 2)))
                                   " c\n"))))
              data)
            (let ((center (list-ref clusters i))) ; display cluster centre
              (display
                (string-append "\n0 setgray "
                               (number->string (+ (* (- (car center) cx) scale) (/ W 2)))
                               " "
                               (number->string (+ (* (- (cadr center) cy) scale) (/ H 2)))
                               " s\n"))))
          (iota K))
        (display "\n%%EOF")))))

;; extra credit 1: creates a list of n random points in n-D unit square
(define (make-data num-points num-dimensions)
  (random-source-randomize! default-random-source)
  (map (lambda (i) (list-tabulate num-dimensions (lambda (i) (random-real)))) (iota num-points)))

;; extra credit 2, uses eps visualisation to display result
(define (tester-1 num-points K)
  (let ((data (make-data num-points 2)))
    (save-as-eps "clusters-1.eps" data (cluster K data) K)))

;; extra credit 3: uses radians instead to make data
(define (tester-2 num-points K radius)
  (random-source-randomize! default-random-source)
  (let ((data (map (lambda (i)
                     (let ((ang (* (random-real) 2 (* 4 (atan 1))))
                           (rad (* radius (random-real))))
                       (list (* rad (cos ang)) (* rad (sin ang)))))
                   (iota num-points))))
    ;; extra credit 2, uses eps visualisation to display result
    (save-as-eps "clusters-2.eps" data (cluster K data) K)))

;; extra credit 4: arbitrary dimensions - already handled, as all points are lists
(define (tester-3 num-points K num-dimensions)
  (display "Results:\n")
  (display (cluster K (make-data num-points num-dimensions)))
  (newline))

(tester-1 30000 6)
(tester-2 30000 6 10)
(tester-3 30000 6 5)

```


Images in eps files are output for the 2D unit square and unit circle.

Text output for the 5D centres:

```txt
Results:
((0.2616723761604841 0.6134082964889989 0.29284958577190745 0.5883330600440337 0.2701242883590077)
 (0.4495151954110258 0.7213650269267102 0.4785552477630192 0.2520793123281655 0.73785249828929)
 (0.6873676767669482 0.3228592693134481 0.4713526933057497 0.23850999205524145 0.3104607677290796)
 (0.6341937732424933 0.36435831485631176 0.2760548254423423 0.7120766805103155 0.7028127288541974)
 (0.2718747392615238 0.2743005712228975 0.7515030778279079 0.5424997615106112 0.5849261595501698)
 (0.6882031980026069 0.7048387370769692 0.7373477088448752 0.6859917992267395 0.4027193966445248))

```



## SequenceL

```sequencel

import <Utilities/Sequence.sl>;
import <Utilities/Random.sl>;
import <Utilities/Math.sl>;
import <Utilities/Conversion.sl>;

Point ::= (x : float, y : float);
Pair<T1, T2> ::= (first : T1, second : T2);

W := 400;
H := 400;

// ------------ Utilities --------------
distance(a, b) := (a.x-b.x)^2 + (a.y-b.y)^2;

nearestDistance(point, centers(1)) :=
    nearestCenterHelper(point, centers, 2, distance(point, centers[1]), 1).second;

nearestCenter(point, centers(1)) :=
    nearestCenterHelper(point, centers, 2, distance(point, centers[1]), 1).first;

nearestCenterHelper(point, centers(1), counter, minDistance, minIndex) :=
    let
        d := distance(point, centers[counter]);
    in
    (first : minIndex, second : minDistance) when counter > size(centers) else
    nearestCenterHelper(point, centers, counter + 1, d, counter) when minDistance > d else
    nearestCenterHelper(point, centers, counter + 1, minDistance, minIndex);

// ------------ KPP --------------
kpp(points(1), k, RG) :=
    let
        randomValues := getRandomSequence(RG, k).Value;
        centers := initialCenters(points, k, randomValues / (RG.RandomMax - 1.0),
                    [points[randomValues[1] mod size(points)]]);
    in
        nearestCenter(points, centers);

initialCenters(points(1), k, randoms(1), centers(1)) :=
    let
        distances := nearestDistance(points, centers);
        randomSum := randoms[size(centers) + 1] * sum(distances);
        newCenter := points[findNewCenter(randomSum, distances, 1)];
    in
        centers when size(centers) = k else
        initialCenters(points, k, randoms, centers++[newCenter]);

findNewCenter(s, distances(1), counter) :=
    let
        new_s := s - distances[counter];
    in
    counter when new_s <= 0 else
    findNewCenter(new_s, distances, counter + 1);

// ------------ K Means --------------
kMeans(points(1), groups(1), k) :=
    let
        newCenters := clusterAverage(points, groups, k);
        newGroups := nearestCenter(points, newCenters);
        threshold := size(points)/1024;
        // Calculate the number of changes between iterations
        changes[i] := 1 when groups[i] /= newGroups[i] else 0;
    in
        (first : newGroups, second : newCenters) when sum(changes) < threshold else
        kMeans(points, newGroups, k);

clusterAverage(points(1), groups(1), k) :=
        clusterAverageHelper(points, groups, 1, duplicate((x:0.0, y:0.0), k), duplicate(0, k));

clusterAverageHelper(points(1), groups(1), counter, averages(1), sizes(1)) :=
    let
        group := groups[counter];
        result[i] := (x : averages[i].x / sizes[i], y : averages[i].y / sizes[i]);
    in
    result when counter > size(points) else
    clusterAverageHelper(points, groups, counter + 1,
        setElementAt(averages, group,
                          (x : averages[group].x + points[counter].x,
                           y : averages[group].y + points[counter].y)),
        setElementAt(sizes, group, sizes[group] + 1));

// ------------ Generate Points --------------
gen2DPoints(count, radius, RG) :=
    let
        randA := getRandomSequence(RG, count);
        randR := getRandomSequence(randA.Generator, count);
        angles := 2*pi*(randA.Value / (RG.RandomMax - 1.0));
        radiuses := radius * (randR.Value / (RG.RandomMax - 1.0));
        points[i] := (x: radiuses[i] * cos(angles[i]), y : radiuses[i] * sin(angles[i]));
    in
        (first : points, second : randR.Generator);

// ------------ Visualize --------------
printEPS(points(1),groups(1),centers(1),k,maxVal) :=
    let
          scale := min(W / (maxVal * 2), H / (maxVal * 2));
          printedGroups := printGroup(points, groups, centers, k, 0.0, scale, 1 ... k);
    in
        "%!-PS-Adobe-3.0\n%%BoundingBox: -5 -5 " ++ toString(W + 10) ++ " " ++
        toString(H + 10) ++
        "\n/l {rlineto} def /m {rmoveto} def\n" ++
        "/c { .25 sub exch .25 sub exch .5 0 360 arc fill } def\n" ++
        "/s { moveto -2 0 m 2 2 l 2 -2 l -2 -2 l closepath " ++
        "   gsave 1 setgray fill grestore gsave 3 setlinewidth" ++
        " 1 setgray stroke grestore 0 setgray stroke }def\n" ++
        join(printedGroups) ++
        "\n%%EOF";

printGroup(points(1), groups(1), centers(1), k, maxVal, scale, group) :=
    let
        printedPoints[i] :=
            toString((points[i].x - maxVal) * scale + W/2) ++ " " ++
            toString((points[i].y - maxVal) * scale + H/2) ++ " c\n"
                when groups[i] = group;

        colors := toString((3 * group mod k) / (k * 1.0)) ++ " " ++
                  toString((7 * (group - 1) mod k) / (k * 1.0)) ++ " " ++
                  toString((9 * (group - 1) mod k) / (k * 1.0)) ++
                  " setrgbcolor\n";

        printedCenters := "\n0 setgray " ++
                   toString((centers[group].x - maxVal) * scale + W/2) ++ " " ++
                   toString((centers[group].y - maxVal) * scale + H/2) ++ " s\n";
    in
        colors ++ join(printedPoints) ++ printedCenters;

// Take number of points, K and seed for random data as command line inputs
main(args(2)) :=
    let
        n := stringToInt(args[1]) when size(args) >= 1 else 1000;
        k := stringToInt(args[2]) when size(args) >= 2 else 7;
        seed := stringToInt(args[3]) when size(args) >= 3 else 13;

        points := gen2DPoints(n, 10.0, seedRandom(seed));
        initialGroups := kpp(points.first, k, points.second);
        result := kMeans(points.first, initialGroups, k);
    in
        printEPS(points.first, result.first, result.second,k,10.0);

```



## Tcl

```tcl
package require Tcl 8.5
package require math::constants
math::constants::constants pi
proc tcl::mathfunc::randf m {expr {$m * rand()}}

proc genXY {count radius} {
    global pi
    for {set i 0} {$i < $count} {incr i} {
	set ang [expr {randf(2 * $pi)}]
	set r [expr {randf($radius)}]
	lappend pt [list [expr {$r*cos($ang)}] [expr {$r*sin($ang)}] -1]
    }
    return $pt
}
proc dist2 {a b} {
    lassign $a ax ay
    lassign $b bx by
    return [expr {($ax-$bx)**2 + ($ay-$by)**2}]
}

proc nearest {pt cent {d2var ""}} {
    set minD 1e30
    set minI [lindex $pt 2]
    set i -1
    foreach c $cent {
	incr i
	set d [dist2 $c $pt]
	if {$minD > $d} {
	    set minD $d
	    set minI $i
	}
    }
    if {$d2var ne ""} {
	upvar 1 $d2var d2
	set d2 $minD
    }
    return $minI
}

proc kpp {ptsVar centVar numClusters} {
    upvar 1 $ptsVar pts $centVar cent
    set idx [expr {int([llength $pts] * rand())}]
    set cent [list [lindex $pts $idx]]
    for {set nCent 1} {$nCent < $numClusters} {incr nCent} {
	set sum 0
	set d {}
	foreach p $pts {
	    nearest $p $cent dd
	    set sum [expr {$sum + $dd}]
	    lappend d $dd
	}
	set sum [expr {randf($sum)}]
	foreach p $pts dj $d {
	    set sum [expr {$sum - $dj}]
	    if {$sum <= 0} {
		lappend cent $p
		break
	    }
	}
    }
    set i -1
    foreach p $pts {
	lset pts [incr i] 2 [nearest $p $cent]
    }
}

proc lloyd {ptsVar numClusters} {
    upvar 1 $ptsVar pts
    kpp pts cent $numClusters
    while 1 {
	# Find centroids for round
	set groupCounts [lrepeat [llength $cent] 0]
	foreach p $pts {
	    lassign $p cx cy group
	    lset groupCounts $group [expr {[lindex $groupCounts $group] + 1}]
	    lset cent $group 0 [expr {[lindex $cent $group 0] + $cx}]
	    lset cent $group 1 [expr {[lindex $cent $group 1] + $cy}]
	}
	set i -1
	foreach groupn $groupCounts {
	    incr i
	    lset cent $i 0 [expr {[lindex $cent $i 0] / $groupn}]
	    lset cent $i 1 [expr {[lindex $cent $i 1] / $groupn}]
	}

	set changed 0
	set i -1
	foreach p $pts {
	    incr i
	    set minI [nearest $p $cent]
	    if {$minI != [lindex $p 2]} {
		incr changed
		lset pts $i 2 $minI
	    }
	}
	if {$changed < ([llength $pts] >> 10)} break
    }
    set i -1
    foreach c $cent {
	lset cent [incr i] 2 $i
    }
    return $cent
}
```

Demonstration/visualization code:
```tcl
package require Tk
image create photo disp -width 400 -height 400
pack [label .l -image disp]
update
proc plot {x y color} {
    disp put $color -to [expr {int(200+19.9*$x)}] [expr {int(200+19.9*$y)}]
}
apply {{} {
    set POINTS [genXY 100000 10]
    set CENTROIDS [lloyd POINTS 11]
    foreach c $CENTROIDS {
	lappend colors [list [list [format "#%02x%02x%02x" \
		[expr {64+int(128*rand())}] [expr {64+int(128*rand())}] \
		[expr {64+int(128*rand())}]]]]
    }
    foreach pt $POINTS {
	lassign $pt px py group
	plot $px $py [lindex $colors $group]
    }
    foreach c $CENTROIDS {
	lassign $c cx cy group
	plot $cx $cy black
    }
}}
```



## XPL0

Like C, simplicity and clarity was chosen over extra credit. Also, the
dataset is global, and the arrays are separate instead of being packed
into two arguments and passed into the KMeans procedure. Hopefully the
animated display, showing the convergence of the clusters, compensates
somewhat for these sins. Alas, image uploads appears to be broken.


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

def     N = 30000;              \number of points
def     K = 6;                  \number of clusters
int     Px(N), Py(N), Pc(N),    \coordinates of points and their cluster
        Cx(K), Cy(K);           \coordinates of centroid of cluster


func Centroid;  \Find new centroids of points grouped with current centroids
int  Change, Cx0(K), Cy0(K), C, Count, I;
[Change:= false;
for C:= 0 to K-1 do                       \for each centroid...
        [Cx0(C):= Cx(C);  Cy0(C):= Cy(C); \save current centroid
        Cx(C):= 0;  Cx(C):= 0;  Count:= 0;\find new centroid
        for I:= 0 to N-1 do               \for all points
            if Pc(I) = C then             \ grouped with current centroid...
                [Cx(C):= Cx(C) + Px(I);
                 Cy(C):= Cy(C) + Py(I);
                 Count:= Count+1;
                ];
        Cx(C):= Cx(C)/Count;  Cy(C):= Cy(C)/Count;
        if Cx(C)#Cx0(C) or Cy(C)#Cy0(C) then Change:= true;
        ];
return Change;
];


proc Voronoi;                   \Group points with their nearest centroid
int  D2, MinD2, I, C;           \distance squared, minimum distance squared
[for I:= 0 to N-1 do            \for each point...
        [MinD2:= -1>>1;         \find closest centroid
        for C:= 0 to K-1 do
                [D2:= sq(Px(I)-Cx(C)) + sq(Py(I)-Cy(C));
                if D2 < MinD2 then
                        [MinD2:= D2;  Pc(I):= C];  \update closest centroid
                ];
        ];
];


proc KMeans;                    \Group points into K clusters
int  Change, I;
repeat  Voronoi;
        Change:= Centroid;
        SetVid($101);           \show result on 640x480x8 screen
        for I:= 0 to N-1 do Point(Px(I), Py(I), Pc(I)+1);
        for I:= 0 to K-1 do Point(Cx(I), Cy(I), \bright white\ $F);
until   Change = false;


proc Random(X, Y);              \Return random X,Y biased for polar coordinates
int  X, Y;
real A, D;
[D:= float(Ran(240));                   \distance: 0..239
A:= float(Ran(314159*2)) / 10000.0;     \angle:    0..2pi
X(0):= fix(D*Cos(A)) + 320;             \rectangular coords centered on screen
Y(0):= fix(D*Sin(A)) + 240;
];


int  I;
[for I:= 0 to N-1 do Random(@Px(I), @Py(I));    \random set of points
 for I:= 0 to K-1 do Random(@Cx(I), @Cy(I));    \random set of cluster centroids
KMeans;
I:= ChIn(1);                    \wait for keystroke
SetVid($03);                    \restore normal text screen
]
```

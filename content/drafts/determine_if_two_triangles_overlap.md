+++
title = "Determine if two triangles overlap"
description = ""
date = 2019-08-30T00:26:03Z
aliases = []
[extra]
id = 21228
[taxonomies]
categories = []
tags = []
+++

[[Category:Geometry]]
[[Category:Collision detection]]
{{draft task}}

Determining if two triangles in the same plane overlap is an important topic in collision detection.


;Task:
Determine which of these pairs of triangles overlap in 2D:

:::*   (0,0),(5,0),(0,5)     and   (0,0),(5,0),(0,6)
:::*   (0,0),(0,5),(5,0)     and   (0,0),(0,5),(5,0)
:::*   (0,0),(5,0),(0,5)     and   (-10,0),(-5,0),(-1,6)
:::*   (0,0),(5,0),(2.5,5)        and   (0,4),(2.5,-1),(5,4)
:::*   (0,0),(1,1),(0,2)     and   (2,1),(3,0),(3,2)
:::*   (0,0),(1,1),(0,2)     and   (2,1),(3,-2),(3,4)


Optionally, see what the result is when only a single corner is in contact (there is no definitively correct answer):
:::*   (0,0),(1,0),(0,1)          and   (1,0),(2,0),(1,1)





## Ada


```ada

WITH Ada.Text_IO; USE Ada.Text_IO;

PROCEDURE Main IS
   TYPE Vertex IS MOD 3;
   TYPE Point IS ARRAY (0 .. 1) OF Float;
   TYPE Triangle IS ARRAY (Vertex) OF Point;
   TYPE Triangle_Vertices IS ARRAY (0 .. 5) OF Float;

   FUNCTION Same_Side (A, B, M, N : Point) RETURN Boolean IS
      FUNCTION Aff (U : Point) RETURN Float IS
        ((B (1) - A (1)) * (U (0) - A (0)) + (A (0) - B (0)) * (U (1) - A (1)));
   BEGIN
      RETURN Aff (M) * Aff (N) >= 0.0;
   END Same_Side;

   FUNCTION In_Side (T1 , T2 : Triangle) RETURN Boolean IS
     (FOR ALL V IN Vertex  =>
        (FOR Some P OF T2 => Same_Side (T1 (V + 1), T1 (V + 2), T1 (V), P)));

   FUNCTION Overlap (T1, T2 : Triangle) RETURN Boolean IS
     (In_Side (T1, T2) AND THEN  In_Side (T2, T1));

   FUNCTION "+" (T : Triangle_Vertices) RETURN Triangle IS
     ((T (0), T (1)), (T (2), T (3)), (T (4), T (5)));

   PROCEDURE Put (T1, T2 : Triangle_Vertices) IS
   BEGIN
      Put_Line (Overlap (+T1, +T2)'Img);
   END Put;

BEGIN
   Put ((0.0, 0.0, 5.0, 0.0, 0.0, 5.0), (0.0, 0.0, 5.0, 0.0, 0.0, 6.0));
   Put ((0.0, 0.0, 0.0, 5.0, 5.0, 0.0), (0.0, 0.0, 0.0, 5.0, 5.0, 0.0));
   Put ((0.0, 0.0, 5.0, 0.0, 0.0, 5.0), (-10.0, 0.0, -5.0, 0.0, -1.0, 6.0));
   Put ((0.0, 0.0, 5.0, 0.0, 2.5, 5.0), (0.0, 4.0, 2.5, -1.0, 5.0, 4.0));
   Put ((0.0, 0.0, 1.0, 1.0, 0.0, 2.0), (2.0, 1.0, 3.0, 0.0, 3.0, 2.0));
   Put ((0.0, 0.0, 1.0, 1.0, 0.0, 2.0), (2.0, 1.0, 3.0, -2.0, 3.0, 4.0));
   Put ((0.0, 0.0, 1.0, 0.0, 0.0, 1.0), (1.0, 0.0, 2.0, 0.0, 1.0, 1.0));
END Main;

```

{{out}}

```txt
true
true
false
true
false
false
true

```



## C

{{trans|C++}}

```c
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    double x, y;
} Point;

double det2D(const Point * const p1, const Point * const p2, const Point * const p3) {
    return p1->x * (p2->y - p3->y)
        + p2->x * (p3->y - p1->y)
        + p3->x * (p1->y - p2->y);
}

void checkTriWinding(Point * p1, Point * p2, Point * p3, bool allowReversed) {
    double detTri = det2D(p1, p2, p3);
    if (detTri < 0.0) {
        if (allowReversed) {
            double t = p3->x;
            p3->x = p2->x;
            p2->x = t;

            t = p3->y;
            p3->y = p2->y;
            p2->y = t;
        } else {
            errno = 1;
        }
    }
}

bool boundaryCollideChk(const Point *p1, const Point *p2, const Point *p3, double eps) {
    return det2D(p1, p2, p3) < eps;
}

bool boundaryDoesntCollideChk(const Point *p1, const Point *p2, const Point *p3, double eps) {
    return det2D(p1, p2, p3) <= eps;
}

bool triTri2D(Point t1[], Point t2[], double eps, bool allowReversed, bool onBoundary) {
    bool(*chkEdge)(Point*, Point*, Point*, double);
    int i;

    // Triangles must be expressed anti-clockwise
    checkTriWinding(&t1[0], &t1[1], &t1[2], allowReversed);
    if (errno != 0) {
        return false;
    }
    checkTriWinding(&t2[0], &t2[1], &t2[2], allowReversed);
    if (errno != 0) {
        return false;
    }

    if (onBoundary) {
        // Points on the boundary are considered as colliding
        chkEdge = boundaryCollideChk;
    } else {
        // Points on the boundary are not considered as colliding
        chkEdge = boundaryDoesntCollideChk;
    }

    //For edge E of trangle 1,
    for (i = 0; i < 3; ++i) {
        int j = (i + 1) % 3;

        //Check all points of trangle 2 lay on the external side of the edge E. If
        //they do, the triangles do not collide.
        if (chkEdge(&t1[i], &t1[j], &t2[0], eps) &&
            chkEdge(&t1[i], &t1[j], &t2[1], eps) &&
            chkEdge(&t1[i], &t1[j], &t2[2], eps)) {
            return false;
        }
    }

    //For edge E of trangle 2,
    for (i = 0; i < 3; i++) {
        int j = (i + 1) % 3;

        //Check all points of trangle 1 lay on the external side of the edge E. If
        //they do, the triangles do not collide.
        if (chkEdge(&t2[i], &t2[j], &t1[0], eps) &&
            chkEdge(&t2[i], &t2[j], &t1[1], eps) &&
            chkEdge(&t2[i], &t2[j], &t1[2], eps))
            return false;
    }

    //The triangles collide
    return true;
}

int main() {
    {
        Point t1[] = { {0, 0}, {5, 0}, {0, 5} };
        Point t2[] = { {0, 0}, {5, 0}, {0, 6} };
        printf("%d,true\n", triTri2D(t1, t2, 0.0, false, true));
    }

    {
        Point t1[] = { {0, 0}, {0, 5}, {5, 0} };
        Point t2[] = { {0, 0}, {0, 5}, {5, 0} };
        printf("%d,true\n", triTri2D(t1, t2, 0.0, true, true));
    }

    {
        Point t1[] = { {0, 0}, {5, 0}, {0, 5} };
        Point t2[] = { {-10, 0}, {-5, 0}, {-1, 6} };
        printf("%d,false\n", triTri2D(t1, t2, 0.0, false, true));
    }

    {
        Point t1[] = { {0, 0}, {5, 0}, {2.5, 5} };
        Point t2[] = { {0, 4}, {2.5, -1}, {5, 4} };
        printf("%d,true\n", triTri2D(t1, t2, 0.0, false, true));
    }

    {
        Point t1[] = { {0, 0}, {1, 1}, {0, 2} };
        Point t2[] = { {2, 1}, {3, 0}, {3, 2} };
        printf("%d,false\n", triTri2D(t1, t2, 0.0, false, true));
    }

    {
        Point t1[] = { {0, 0}, {1, 1}, {0, 2} };
        Point t2[] = { {2, 1}, {3, -2}, {3, 4} };
        printf("%d,false\n", triTri2D(t1, t2, 0.0, false, true));
    }

    //Barely touching
    {
        Point t1[] = { {0, 0}, {1, 0}, {0, 1} };
        Point t2[] = { {1, 0}, {2, 0}, {1, 1} };
        printf("%d,true\n", triTri2D(t1, t2, 0.0, false, true));
    }

    //Barely touching
    {
        Point t1[] = { {0, 0}, {1, 0}, {0, 1} };
        Point t2[] = { {1, 0}, {2, 0}, {1, 1} };
        printf("%d,false\n", triTri2D(t1, t2, 0.0, false, false));
    }

    return EXIT_SUCCESS;
}
```

{{out}}

```txt
1,true
1,true
0,false
1,true
0,false
0,false
1,true
0,false
```



## C++


```cpp
#include <vector>
#include <iostream>
#include <stdexcept>
using namespace std;

typedef std::pair<double, double> TriPoint;

inline double Det2D(TriPoint &p1, TriPoint &p2, TriPoint &p3)
{
	return +p1.first*(p2.second-p3.second)
		+p2.first*(p3.second-p1.second)
		+p3.first*(p1.second-p2.second);
}

void CheckTriWinding(TriPoint &p1, TriPoint &p2, TriPoint &p3, bool allowReversed)
{
	double detTri = Det2D(p1, p2, p3);
	if(detTri < 0.0)
	{
		if (allowReversed)
		{
			TriPoint a = p3;
			p3 = p2;
			p2 = a;
		}
		else throw std::runtime_error("triangle has wrong winding direction");
	}
}

bool BoundaryCollideChk(TriPoint &p1, TriPoint &p2, TriPoint &p3, double eps)
{
	return Det2D(p1, p2, p3) < eps;
}

bool BoundaryDoesntCollideChk(TriPoint &p1, TriPoint &p2, TriPoint &p3, double eps)
{
	return Det2D(p1, p2, p3) <= eps;
}

bool TriTri2D(TriPoint *t1,
	TriPoint *t2,
	double eps = 0.0, bool allowReversed = false, bool onBoundary = true)
{
	//Trangles must be expressed anti-clockwise
	CheckTriWinding(t1[0], t1[1], t1[2], allowReversed);
	CheckTriWinding(t2[0], t2[1], t2[2], allowReversed);

	bool (*chkEdge)(TriPoint &, TriPoint &, TriPoint &, double) = NULL;
	if(onBoundary) //Points on the boundary are considered as colliding
		chkEdge = BoundaryCollideChk;
	else //Points on the boundary are not considered as colliding
		chkEdge = BoundaryDoesntCollideChk;

	//For edge E of trangle 1,
	for(int i=0; i<3; i++)
	{
		int j=(i+1)%3;

		//Check all points of trangle 2 lay on the external side of the edge E. If
		//they do, the triangles do not collide.
		if (chkEdge(t1[i], t1[j], t2[0], eps) &&
			chkEdge(t1[i], t1[j], t2[1], eps) &&
			chkEdge(t1[i], t1[j], t2[2], eps))
			return false;
	}

	//For edge E of trangle 2,
	for(int i=0; i<3; i++)
	{
		int j=(i+1)%3;

		//Check all points of trangle 1 lay on the external side of the edge E. If
		//they do, the triangles do not collide.
		if (chkEdge(t2[i], t2[j], t1[0], eps) &&
			chkEdge(t2[i], t2[j], t1[1], eps) &&
			chkEdge(t2[i], t2[j], t1[2], eps))
			return false;
	}

	//The triangles collide
	return true;
}

int main()
{
	{TriPoint t1[] = {TriPoint(0,0),TriPoint(5,0),TriPoint(0,5)};
	TriPoint t2[] = {TriPoint(0,0),TriPoint(5,0),TriPoint(0,6)};
	cout << TriTri2D(t1, t2) << "," << true << endl;}

	{TriPoint t1[] = {TriPoint(0,0),TriPoint(0,5),TriPoint(5,0)};
	TriPoint t2[] = {TriPoint(0,0),TriPoint(0,5),TriPoint(5,0)};
	cout << TriTri2D(t1, t2, 0.0, true) << "," << true << endl;}

	{TriPoint t1[] = {TriPoint(0,0),TriPoint(5,0),TriPoint(0,5)};
	TriPoint t2[] = {TriPoint(-10,0),TriPoint(-5,0),TriPoint(-1,6)};
	cout << TriTri2D(t1, t2) << "," << false << endl;}

	{TriPoint t1[] = {TriPoint(0,0),TriPoint(5,0),TriPoint(2.5,5)};
	TriPoint t2[] = {TriPoint(0,4),TriPoint(2.5,-1),TriPoint(5,4)};
	cout << TriTri2D(t1, t2) << "," << true << endl;}

	{TriPoint t1[] = {TriPoint(0,0),TriPoint(1,1),TriPoint(0,2)};
	TriPoint t2[] = {TriPoint(2,1),TriPoint(3,0),TriPoint(3,2)};
	cout << TriTri2D(t1, t2) << "," << false << endl;}

	{TriPoint t1[] = {TriPoint(0,0),TriPoint(1,1),TriPoint(0,2)};
	TriPoint t2[] = {TriPoint(2,1),TriPoint(3,-2),TriPoint(3,4)};
	cout << TriTri2D(t1, t2) << "," << false << endl;}

	//Barely touching
	{TriPoint t1[] = {TriPoint(0,0),TriPoint(1,0),TriPoint(0,1)};
	TriPoint t2[] = {TriPoint(1,0),TriPoint(2,0),TriPoint(1,1)};
	cout << TriTri2D(t1, t2, 0.0, false, true) << "," << true << endl;}

	//Barely touching
	{TriPoint t1[] = {TriPoint(0,0),TriPoint(1,0),TriPoint(0,1)};
	TriPoint t2[] = {TriPoint(1,0),TriPoint(2,0),TriPoint(1,1)};
	cout << TriTri2D(t1, t2, 0.0, false, false) << "," << false << endl;}

}
```


{{out}}

```txt
1,1
1,1
0,0
1,1
0,0
0,0
1,1
0,0
```


=={{header|C#|C sharp}}==

```csharp
using System;
using System.Collections.Generic;

namespace TriangleOverlap {
    class Triangle {
        public Tuple<double, double> P1 { get; set; }
        public Tuple<double, double> P2 { get; set; }
        public Tuple<double, double> P3 { get; set; }

        public Triangle(Tuple<double, double> p1, Tuple<double, double> p2, Tuple<double, double> p3) {
            P1 = p1;
            P2 = p2;
            P3 = p3;
        }

        public double Det2D() {
            return P1.Item1 * (P2.Item2 - P3.Item2)
                 + P2.Item1 * (P3.Item2 - P1.Item2)
                 + P3.Item1 * (P3.Item1 - P2.Item2);
        }

        public void CheckTriWinding(bool allowReversed) {
            var detTri = Det2D();
            if (detTri < 0.0) {
                if (allowReversed) {
                    var a = P3;
                    P3 = P2;
                    P2 = a;
                } else {
                    throw new Exception("Triangle has wrong winding direction");
                }
            }
        }

        public bool BoundaryCollideChk(double eps) {
            return Det2D() < eps;
        }

        public bool BoundaryDoesntCollideChk(double eps) {
            return Det2D() <= eps;
        }

        public override string ToString() {
            return string.Format("Triangle: {0}, {1}, {2}", P1, P2, P3);
        }
    }

    class Program {
        static bool BoundaryCollideChk(Triangle t, double eps) {
            return t.BoundaryCollideChk(eps);
        }

        static bool BoundaryDoesntCollideChk(Triangle t, double eps) {
            return t.BoundaryDoesntCollideChk(eps);
        }

        static bool TriTri2D(Triangle t1, Triangle t2, double eps = 0.0, bool allowReversed = false, bool onBoundary = true) {
            // Triangles must be expressed anti-clockwise
            t1.CheckTriWinding(allowReversed);
            t2.CheckTriWinding(allowReversed);
            // 'onBoundary' determines whether points on boundary are considered as colliding or not
            var chkEdge = onBoundary
                ? (Func<Triangle, double, bool>)BoundaryCollideChk
                : BoundaryDoesntCollideChk;
            List<Tuple<double, double>> lp1 = new List<Tuple<double, double>>() { t1.P1, t1.P2, t1.P3 };
            List<Tuple<double, double>> lp2 = new List<Tuple<double, double>>() { t2.P1, t2.P2, t2.P3 };

            // for each edge E of t1
            for (int i = 0; i < 3; i++) {
                var j = (i + 1) % 3;
                // Check all points of t2 lay on the external side of edge E.
                // If they do, the triangles do not overlap.
                if (chkEdge(new Triangle(lp1[i], lp1[j], lp2[0]), eps) &&
                    chkEdge(new Triangle(lp1[i], lp1[j], lp2[1]), eps) &&
                    chkEdge(new Triangle(lp1[i], lp1[j], lp2[2]), eps)) {
                    return false;
                }
            }

            // for each edge E of t2
            for (int i = 0; i < 3; i++) {
                var j = (i + 1) % 3;
                // Check all points of t1 lay on the external side of edge E.
                // If they do, the triangles do not overlap.
                if (chkEdge(new Triangle(lp2[i], lp2[j], lp1[0]), eps) &&
                    chkEdge(new Triangle(lp2[i], lp2[j], lp1[1]), eps) &&
                    chkEdge(new Triangle(lp2[i], lp2[j], lp1[2]), eps)) {
                    return false;
                }
            }

            // The triangles overlap
            return true;
        }

        static void Overlap(Triangle t1, Triangle t2, double eps = 0.0, bool allowReversed = false, bool onBoundary = true) {
            if (TriTri2D(t1, t2, eps, allowReversed, onBoundary)) {
                Console.WriteLine("overlap");
            } else {
                Console.WriteLine("do not overlap");
            }
        }

        static void Main(string[] args) {
            var t1 = new Triangle(new Tuple<double, double>(0.0, 0.0), new Tuple<double, double>(5.0, 0.0), new Tuple<double, double>(0.0, 5.0));
            var t2 = new Triangle(new Tuple<double, double>(0.0, 0.0), new Tuple<double, double>(5.0, 0.0), new Tuple<double, double>(0.0, 6.0));
            Console.WriteLine("{0} and\n{1}", t1, t2);
            Overlap(t1, t2);
            Console.WriteLine();

            // need to allow reversed for this pair to avoid exception
            t1 = new Triangle(new Tuple<double, double>(0.0, 0.0), new Tuple<double, double>(0.0, 5.0), new Tuple<double, double>(5.0, 0.0));
            t2 = t1;
            Console.WriteLine("{0} and\n{1}", t1, t2);
            Overlap(t1, t2, 0.0, true);
            Console.WriteLine();

            t1 = new Triangle(new Tuple<double, double>(0.0, 0.0), new Tuple<double, double>(5.0, 0.0), new Tuple<double, double>(0.0, 5.0));
            t2 = new Triangle(new Tuple<double, double>(-10.0, 0.0), new Tuple<double, double>(-5.0, 0.0), new Tuple<double, double>(-1.0, 6.0));
            Console.WriteLine("{0} and\n{1}", t1, t2);
            Overlap(t1, t2);
            Console.WriteLine();

            t1.P3 = new Tuple<double, double>(2.5, 5.0);
            t2 = new Triangle(new Tuple<double, double>(0.0, 4.0), new Tuple<double, double>(2.5, -1.0), new Tuple<double, double>(5.0, 4.0));
            Console.WriteLine("{0} and\n{1}", t1, t2);
            Overlap(t1, t2);
            Console.WriteLine();

            t1 = new Triangle(new Tuple<double, double>(0.0, 0.0), new Tuple<double, double>(1.0, 1.0), new Tuple<double, double>(0.0, 2.0));
            t2 = new Triangle(new Tuple<double, double>(2.0, 1.0), new Tuple<double, double>(3.0, 0.0), new Tuple<double, double>(3.0, 2.0));
            Console.WriteLine("{0} and\n{1}", t1, t2);
            Overlap(t1, t2);
            Console.WriteLine();

            t2 = new Triangle(new Tuple<double, double>(2.0, 1.0), new Tuple<double, double>(3.0, -2.0), new Tuple<double, double>(3.0, 4.0));
            Console.WriteLine("{0} and\n{1}", t1, t2);
            Overlap(t1, t2);
            Console.WriteLine();

            t1 = new Triangle(new Tuple<double, double>(0.0, 0.0), new Tuple<double, double>(1.0, 0.0), new Tuple<double, double>(0.0, 1.0));
            t2 = new Triangle(new Tuple<double, double>(1.0, 0.0), new Tuple<double, double>(2.0, 0.0), new Tuple<double, double>(1.0, 1.1));
            Console.WriteLine("{0} and\n{1}", t1, t2);
            Console.WriteLine("which have only a single corner in contact, if boundary points collide");
            Overlap(t1, t2);
            Console.WriteLine();

            Console.WriteLine("{0} and\n{1}", t1, t2);
            Console.WriteLine("which have only a single corner in contact, if boundary points do not collide");
            Overlap(t1, t2, 0.0, false, false);
        }
    }
}
```

{{out}}

```txt
Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (0, 0), (5, 0), (0, 6)
overlap

Triangle: (0, 0), (0, 5), (5, 0) and
Triangle: (0, 0), (0, 5), (5, 0)
overlap

Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (-10, 0), (-5, 0), (-1, 6)
do not overlap

Triangle: (0, 0), (5, 0), (2.5, 5) and
Triangle: (0, 4), (2.5, -1), (5, 4)
overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, 0), (3, 2)
do not overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, -2), (3, 4)
do not overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1.1)
which have only a single corner in contact, if boundary points collide
do not overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1.1)
which have only a single corner in contact, if boundary points do not collide
do not overlap
```



## D

{{trans|Kotlin}}

```D
import std.stdio;
import std.typecons;

alias Pair = Tuple!(real, real);

struct Triangle {
    Pair p1;
    Pair p2;
    Pair p3;

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;
        sink("Triangle: ");
        formattedWrite!"%s"(sink, p1);
        sink(", ");
        formattedWrite!"%s"(sink, p2);
        sink(", ");
        formattedWrite!"%s"(sink, p3);
    }
}

auto det2D(Triangle t) {
    return t.p1[0] *(t.p2[1] - t.p3[1])
         + t.p2[0] *(t.p3[1] - t.p1[1])
         + t.p3[0] *(t.p1[1] - t.p2[1]);
}

void checkTriWinding(Triangle t, bool allowReversed) {
    auto detTri = t.det2D();
    if (detTri < 0.0) {
        if (allowReversed) {
            auto a = t.p3;
            t.p3 = t.p2;
            t.p2 = a;
        } else {
            throw new Exception("Triangle has wrong winding direction");
        }
    }
}

auto boundaryCollideChk(Triangle t, real eps) {
    return t.det2D() < eps;
}

auto boundaryDoesntCollideChk(Triangle t, real eps) {
    return t.det2D() <= eps;
}

bool triTri2D(Triangle t1, Triangle t2, real eps = 0.0, bool allowReversed = false, bool onBoundary = true) {
    // Triangles must be expressed anti-clockwise
    checkTriWinding(t1, allowReversed);
    checkTriWinding(t2, allowReversed);
    // 'onBoundary' determines whether points on boundary are considered as colliding or not
    auto chkEdge = onBoundary ? &boundaryCollideChk : &boundaryDoesntCollideChk;
    auto lp1 = [t1.p1, t1.p2, t1.p3];
    auto lp2 = [t2.p1, t2.p2, t2.p3];

    // for each edge E of t1
    foreach (i; 0..3) {
        auto j = (i + 1) % 3;
        // Check all points of t2 lay on the external side of edge E.
        // If they do, the triangles do not overlap.
        if (chkEdge(Triangle(lp1[i], lp1[j], lp2[0]), eps) &&
            chkEdge(Triangle(lp1[i], lp1[j], lp2[1]), eps) &&
            chkEdge(Triangle(lp1[i], lp1[j], lp2[2]), eps)) {
            return false;
        }
    }

    // for each edge E of t2
    foreach (i; 0..3) {
        auto j = (i + 1) % 3;
        // Check all points of t1 lay on the external side of edge E.
        // If they do, the triangles do not overlap.
        if (chkEdge(Triangle(lp2[i], lp2[j], lp1[0]), eps) &&
            chkEdge(Triangle(lp2[i], lp2[j], lp1[1]), eps) &&
            chkEdge(Triangle(lp2[i], lp2[j], lp1[2]), eps)) {
            return false;
        }
    }

    // The triangles overlap
    return true;
}

void overlap(Triangle t1, Triangle t2, real eps = 0.0, bool allowReversed = false, bool onBoundary = true) {
    if (triTri2D(t1, t2, eps, allowReversed, onBoundary)) {
        writeln("overlap");
    } else {
        writeln("do not overlap");
    }
}

void main() {
    auto t1 = Triangle(Pair(0.0, 0.0), Pair(5.0, 0.0), Pair(0.0, 5.0));
    auto t2 = Triangle(Pair(0.0, 0.0), Pair(5.0, 0.0), Pair(0.0, 6.0));
    writeln(t1, " and\n", t2);
    overlap(t1, t2);
    writeln;

    // need to allow reversed for this pair to avoid exception
    t1 = Triangle(Pair(0.0, 0.0), Pair(0.0, 5.0), Pair(5.0, 0.0));
    t2 = t1;
    writeln(t1, " and\n", t2);
    overlap(t1, t2, 0.0, true);
    writeln;

    t1 = Triangle(Pair(0.0, 0.0), Pair(5.0, 0.0), Pair(0.0, 5.0));
    t2 = Triangle(Pair(-10.0, 0.0), Pair(-5.0, 0.0), Pair(-1.0, 6.0));
    writeln(t1, " and\n", t2);
    overlap(t1, t2);
    writeln;

    t1.p3 = Pair(2.5, 5.0);
    t2 = Triangle(Pair(0.0, 4.0), Pair(2.5, -1.0), Pair(5.0, 4.0));
    writeln(t1, " and\n", t2);
    overlap(t1, t2);
    writeln;

    t1 = Triangle(Pair(0.0, 0.0), Pair(1.0, 1.0), Pair(0.0, 2.0));
    t2 = Triangle(Pair(2.0, 1.0), Pair(3.0, 0.0), Pair(3.0, 2.0));
    writeln(t1, " and\n", t2);
    overlap(t1, t2);
    writeln;

    t2 = Triangle(Pair(2.0, 1.0), Pair(3.0, -2.0), Pair(3.0, 4.0));
    writeln(t1, " and\n", t2);
    overlap(t1, t2);
    writeln;

    t1 = Triangle(Pair(0.0, 0.0), Pair(1.0, 0.0), Pair(0.0, 1.0));
    t2 = Triangle(Pair(1.0, 0.0), Pair(2.0, 0.0), Pair(1.0, 1.1));
    writeln(t1, " and\n", t2);
    writeln("which have only a single corner in contact, if boundary points collide");
    overlap(t1, t2);
    writeln;

    writeln(t1, " and\n", t2);
    writeln("which have only a single corner in contact, if boundary points do not collide");
    overlap(t1, t2, 0.0, false, false);
}
```


{{out}}

```txt
Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(5, 0), Tuple!(real, real)(0, 5) and
Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(5, 0), Tuple!(real, real)(0, 6)
overlap

Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(0, 5), Tuple!(real, real)(5, 0) and
Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(0, 5), Tuple!(real, real)(5, 0)
overlap

Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(5, 0), Tuple!(real, real)(0, 5) and
Triangle: Tuple!(real, real)(-10, 0), Tuple!(real, real)(-5, 0), Tuple!(real, real)(-1, 6)
do not overlap

Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(5, 0), Tuple!(real, real)(2.5, 5) and
Triangle: Tuple!(real, real)(0, 4), Tuple!(real, real)(2.5, -1), Tuple!(real, real)(5, 4)
overlap

Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(1, 1), Tuple!(real, real)(0, 2) and
Triangle: Tuple!(real, real)(2, 1), Tuple!(real, real)(3, 0), Tuple!(real, real)(3, 2)
do not overlap

Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(1, 1), Tuple!(real, real)(0, 2) and
Triangle: Tuple!(real, real)(2, 1), Tuple!(real, real)(3, -2), Tuple!(real, real)(3, 4)
do not overlap

Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(1, 0), Tuple!(real, real)(0, 1) and
Triangle: Tuple!(real, real)(1, 0), Tuple!(real, real)(2, 0), Tuple!(real, real)(1, 1.1)
which have only a single corner in contact, if boundary points collide
overlap

Triangle: Tuple!(real, real)(0, 0), Tuple!(real, real)(1, 0), Tuple!(real, real)(0, 1) and
Triangle: Tuple!(real, real)(1, 0), Tuple!(real, real)(2, 0), Tuple!(real, real)(1, 1.1)
which have only a single corner in contact, if boundary points do not collide
do not overlap
```


=={{header|F#|F sharp}}==
{{trans|Kotlin}}

```fsharp
open System

type Point = double * double
type Triangle = Point * Point * Point

let Det2D (t:Triangle) =
    let (p1, p2, p3) = t
    let (p1x, p1y) = p1
    let (p2x, p2y) = p2
    let (p3x, p3y) = p3

    p1x * (p2y - p3y) +
    p2x * (p3y - p1y) +
    p3x * (p1y - p2y)

let CheckTriWinding allowReversed t =
    let detTri = Det2D t
    if detTri < 0.0 then
        if allowReversed then
            let (p1, p2, p3) = t
            (p1, p3, p2)
        else
            raise (Exception "Triangle has wrong winding direction")
    else
        t

let boundaryCollideChk eps t =
    (Det2D t) < eps

let boundaryDoesntCollideChk eps t =
    (Det2D t) <= eps

let TriTri2D eps allowReversed onBoundary t1 t2 =
    // Triangles must be expressed anti-clockwise
    let t3 = CheckTriWinding allowReversed t1
    let t4 = CheckTriWinding allowReversed t2

    // 'onBoundary' determines whether points on boundary are considered as colliding or not
    let chkEdge = if onBoundary then boundaryCollideChk else boundaryDoesntCollideChk
    let (t1p1, t1p2, t1p3) = t3
    let (t2p1, t2p2, t2p3) = t4

    // Check all points of t2 lay on the external side of edge E.
    // If they do, the triangles do not overlap.
    if (chkEdge eps (t1p1, t1p2, t2p1)) && (chkEdge eps (t1p1, t1p2, t2p2)) && (chkEdge eps (t1p1, t1p2, t2p3)) then
        false
    else if (chkEdge eps (t1p2, t1p3, t2p1)) && (chkEdge eps (t1p2, t1p3, t2p2)) && (chkEdge eps (t1p2, t1p3, t2p3)) then
        false
    else if (chkEdge eps (t1p3, t1p1, t2p1)) && (chkEdge eps (t1p3, t1p1, t2p2)) && (chkEdge eps (t1p3, t1p1, t2p3)) then
        false

    // Check all points of t1 lay on the external side of edge E.
    // If they do, the triangles do not overlap.
    else if (chkEdge eps (t2p1, t2p2, t1p1)) && (chkEdge eps (t2p1, t2p2, t1p2)) && (chkEdge eps (t2p1, t2p2, t1p3)) then
        false
    else if (chkEdge eps (t2p2, t2p3, t1p1)) && (chkEdge eps (t2p2, t2p3, t1p2)) && (chkEdge eps (t2p2, t2p3, t1p3)) then
        false
    else if (chkEdge eps (t2p3, t2p1, t1p1)) && (chkEdge eps (t2p3, t2p1, t1p2)) && (chkEdge eps (t2p3, t2p1, t1p3)) then
        false

    else
        // The triangles overlap
        true

let Print t1 t2 =
    Console.WriteLine("{0} and\n{1}\n{2}\n", t1, t2, if TriTri2D 0.0 false true t1 t2 then "overlap" else "do not overlap")

[<EntryPoint>]
let main _ =
    let t1 = ((0.0, 0.0), (5.0, 0.0), (0.0, 5.0))
    let t2 = ((0.0, 0.0), (5.0, 0.0), (0.0, 6.0))
    Print t1 t2

    let t3 = ((0.0, 0.0), (0.0, 5.0), (5.0, 0.0))
    Console.WriteLine("{0} and\n{1}\n{2}\n", t3, t3, if TriTri2D 0.0 true true t3 t3 then "overlap (reversed)" else "do not overlap")

    let t4 = ((0.0, 0.0), (5.0, 0.0), (0.0, 5.0))
    let t5 = ((-10.0, 0.0), (-5.0, 0.0), (-1.0, 6.0))
    Print t4 t5

    let t6 = ((0.0, 0.0), (5.0, 0.0), (2.5, 5.0))
    let t7 = ((0.0, 4.0), (2.5, -1.0), (5.0, 4.0))
    Print t6 t7

    let t8 = ((0.0, 0.0), (1.0, 1.0), (0.0, 2.0))
    let t9 = ((2.0, 1.0), (3.0, 0.0), (3.0, 2.0))
    Print t8 t9

    let t10 = ((2.0, 1.0), (3.0, -2.0), (3.0, 4.0))
    Print t8 t10

    let t11 = ((0.0, 0.0), (1.0, 0.0), (0.0, 1.0))
    let t12 = ((1.0, 0.0), (2.0, 0.0), (1.0, 1.1))
    printfn "The following triangles which have only a single corner in contact, if boundary points collide"
    Print t11 t12

    Console.WriteLine("{0} and\n{1}\nwhich have only a single corner in contact, if boundary points do not collide\n{2}", t11, t12, if TriTri2D 0.0 false false t11 t12 then "overlap" else "do not overlap")

    0 // return an integer exit code
```

{{out}}

```txt
((0, 0), (5, 0), (0, 5)) and
((0, 0), (5, 0), (0, 6))
overlap

((0, 0), (0, 5), (5, 0)) and
((0, 0), (0, 5), (5, 0))
overlap (reversed)

((0, 0), (5, 0), (0, 5)) and
((-10, 0), (-5, 0), (-1, 6))
do not overlap

((0, 0), (5, 0), (2.5, 5)) and
((0, 4), (2.5, -1), (5, 4))
overlap

((0, 0), (1, 1), (0, 2)) and
((2, 1), (3, 0), (3, 2))
do not overlap

((0, 0), (1, 1), (0, 2)) and
((2, 1), (3, -2), (3, 4))
do not overlap

The following triangles which have only a single corner in contact, if boundary points collide
((0, 0), (1, 0), (0, 1)) and
((1, 0), (2, 0), (1, 1.1))
overlap

((0, 0), (1, 0), (0, 1)) and
((1, 0), (2, 0), (1, 1.1))
which have only a single corner in contact, if boundary points do not collide
do not overlap
```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

type point struct {
    x, y float64
}

func (p point) String() string {
    return fmt.Sprintf("(%.1f, %.1f)", p.x, p.y)
}

type triangle struct {
    p1, p2, p3 point
}

func (t *triangle) String() string {
    return fmt.Sprintf("Triangle %s, %s, %s", t.p1, t.p2, t.p3)
}

func (t *triangle) det2D() float64 {
    return t.p1.x * (t.p2.y - t.p3.y) +
           t.p2.x * (t.p3.y - t.p1.y) +
           t.p3.x * (t.p1.y - t.p2.y)
}

func (t *triangle) checkTriWinding(allowReversed bool) {
    detTri := t.det2D()
    if detTri < 0.0 {
        if allowReversed {
            a := t.p3
            t.p3 = t.p2
            t.p2 = a
        } else {
            panic("Triangle has wrong winding direction.")
        }
    }
}

func boundaryCollideChk(t *triangle, eps float64) bool {
    return t.det2D() < eps
}

func boundaryDoesntCollideChk(t *triangle, eps float64) bool {
    return t.det2D() <= eps
}

func triTri2D(t1, t2 *triangle, eps float64, allowReversed, onBoundary bool) bool {
    // Triangles must be expressed anti-clockwise.
    t1.checkTriWinding(allowReversed)
    t2.checkTriWinding(allowReversed)

    // 'onBoundary' determines whether points on boundary are considered as colliding or not.
    var chkEdge func (*triangle, float64) bool
    if onBoundary {
        chkEdge = boundaryCollideChk
    } else {
        chkEdge = boundaryDoesntCollideChk
    }
    lp1 := [3]point{t1.p1, t1.p2, t1.p3}
    lp2 := [3]point{t2.p1, t2.p2, t2.p3}

    // for each edge E of t1
    for i := 0; i < 3; i++ {
        j := (i + 1) % 3
        // Check all points of t2 lay on the external side of edge E.
        // If they do, the triangles do not overlap.
        tri1 := &triangle{lp1[i], lp1[j], lp2[0]}
        tri2 := &triangle{lp1[i], lp1[j], lp2[1]}
        tri3 := &triangle{lp1[i], lp1[j], lp2[2]}
        if chkEdge(tri1, eps) && chkEdge(tri2, eps) && chkEdge(tri3, eps) {
            return false
        }
    }

    // for each edge E of t2
    for i := 0; i < 3; i++ {
        j := (i + 1) % 3
        // Check all points of t1 lay on the external side of edge E.
        // If they do, the triangles do not overlap.
        tri1 := &triangle{lp2[i], lp2[j], lp1[0]}
        tri2 := &triangle{lp2[i], lp2[j], lp1[1]}
        tri3 := &triangle{lp2[i], lp2[j], lp1[2]}
        if chkEdge(tri1, eps) && chkEdge(tri2, eps) && chkEdge(tri3, eps) {
            return false
        }
    }

    // The triangles overlap.
    return true
}

func iff(cond bool, s1, s2 string) string {
    if cond {
        return s1
    }
    return s2
}

func main() {
    t1 := &triangle{point{0.0, 0.0}, point{5.0, 0.0}, point{0.0, 5.0}}
    t2 := &triangle{point{0.0, 0.0}, point{5.0, 0.0}, point{0.0, 6.0}}
    fmt.Printf("%s and\n%s\n", t1, t2)
    overlapping := triTri2D(t1, t2, 0.0, false, true)
    fmt.Println(iff(overlapping, "overlap", "do not overlap"))

    // Need to allow reversed for this pair to avoid panic.
    t1 = &triangle{point{0.0, 0.0}, point{0.0, 5.0}, point{5.0, 0.0}}
    t2 = t1
    fmt.Printf("\n%s and\n%s\n", t1, t2)
    overlapping = triTri2D(t1, t2, 0.0, true, true)
    fmt.Println(iff(overlapping, "overlap (reversed)", "do not overlap"))

    t1 = &triangle{point{0.0, 0.0}, point{5.0, 0.0}, point{0.0, 5.0}}
    t2 = &triangle{point{-10.0, 0.0}, point{-5.0, 0.0}, point{-1.0, 6.0}}
    fmt.Printf("\n%s and\n%s\n", t1, t2)
    overlapping = triTri2D(t1, t2, 0.0, false, true)
    fmt.Println(iff(overlapping, "overlap", "do not overlap"))

    t1.p3 = point{2.5, 5.0}
    t2 = &triangle{point{0.0, 4.0}, point{2.5, -1.0}, point{5.0, 4.0}}
    fmt.Printf("\n%s and\n%s\n", t1, t2)
    overlapping = triTri2D(t1, t2, 0.0, false, true)
    fmt.Println(iff(overlapping, "overlap", "do not overlap"))

    t1 = &triangle{point{0.0, 0.0}, point{1.0, 1.0}, point{0.0, 2.0}}
    t2 = &triangle{point{2.0, 1.0}, point{3.0, 0.0}, point{3.0, 2.0}}
    fmt.Printf("\n%s and\n%s\n", t1, t2)
    overlapping = triTri2D(t1, t2, 0.0, false, true)
    fmt.Println(iff(overlapping, "overlap", "do not overlap"))

    t2 = &triangle{point{2.0, 1.0}, point{3.0, -2.0}, point{3.0, 4.0}}
    fmt.Printf("\n%s and\n%s\n", t1, t2)
    overlapping = triTri2D(t1, t2, 0.0, false, true)
    fmt.Println(iff(overlapping, "overlap", "do not overlap"))

    t1 = &triangle{point{0.0, 0.0}, point{1.0, 0.0}, point{0.0, 1.0}}
    t2 = &triangle{point{1.0, 0.0}, point{2.0, 0.0}, point{1.0, 1.1}}
    fmt.Printf("\n%s and\n%s\n", t1, t2)
    println("which have only a single corner in contact, if boundary points collide")
    overlapping = triTri2D(t1, t2, 0.0, false, true)
    fmt.Println(iff(overlapping, "overlap", "do not overlap"))

    fmt.Printf("\n%s and\n%s\n", t1, t2)
    fmt.Println("which have only a single corner in contact, if boundary points do not collide")
    overlapping = triTri2D(t1, t2, 0.0, false, false)
    fmt.Println(iff(overlapping, "overlap", "do not overlap"))
}
```


{{out}}

```txt

Same as Kotlin entry.

```



## Java

{{trans|Kotlin}}
{{works with|Java|8}}

```Java
import java.util.function.BiFunction;

public class TriangleOverlap {
    private static class Pair {
        double first;
        double second;

        Pair(double first, double second) {
            this.first = first;
            this.second = second;
        }

        @Override
        public String toString() {
            return String.format("(%s, %s)", first, second);
        }
    }

    private static class Triangle {
        Pair p1, p2, p3;

        Triangle(Pair p1, Pair p2, Pair p3) {
            this.p1 = p1;
            this.p2 = p2;
            this.p3 = p3;
        }

        @Override
        public String toString() {
            return String.format("Triangle: %s, %s, %s", p1, p2, p3);
        }
    }

    private static double det2D(Triangle t) {
        Pair p1 = t.p1;
        Pair p2 = t.p2;
        Pair p3 = t.p3;
        return p1.first * (p2.second - p3.second)
            + p2.first * (p3.second - p1.second)
            + p3.first * (p1.second - p2.second);
    }

    private static void checkTriWinding(Triangle t, boolean allowReversed) {
        double detTri = det2D(t);
        if (detTri < 0.0) {
            if (allowReversed) {
                Pair a = t.p3;
                t.p3 = t.p2;
                t.p2 = a;
            } else throw new RuntimeException("Triangle has wrong winding direction");
        }
    }

    private static boolean boundaryCollideChk(Triangle t, double eps) {
        return det2D(t) < eps;
    }

    private static boolean boundaryDoesntCollideChk(Triangle t, double eps) {
        return det2D(t) <= eps;
    }

    private static boolean triTri2D(Triangle t1, Triangle t2) {
        return triTri2D(t1, t2, 0.0, false, true);
    }

    private static boolean triTri2D(Triangle t1, Triangle t2, double eps, boolean allowedReversed) {
        return triTri2D(t1, t2, eps, allowedReversed, true);
    }

    private static boolean triTri2D(Triangle t1, Triangle t2, double eps, boolean allowedReversed, boolean onBoundary) {
        // Triangles must be expressed anti-clockwise
        checkTriWinding(t1, allowedReversed);
        checkTriWinding(t2, allowedReversed);
        // 'onBoundary' determines whether points on boundary are considered as colliding or not
        BiFunction<Triangle, Double, Boolean> chkEdge = onBoundary ? TriangleOverlap::boundaryCollideChk : TriangleOverlap::boundaryDoesntCollideChk;
        Pair[] lp1 = new Pair[]{t1.p1, t1.p2, t1.p3};
        Pair[] lp2 = new Pair[]{t2.p1, t2.p2, t2.p3};

        // for each edge E of t1
        for (int i = 0; i < 3; ++i) {
            int j = (i + 1) % 3;
            // Check all points of t2 lay on the external side of edge E.
            // If they do, the triangles do not overlap.
            if (chkEdge.apply(new Triangle(lp1[i], lp1[j], lp2[0]), eps) &&
                chkEdge.apply(new Triangle(lp1[i], lp1[j], lp2[1]), eps) &&
                chkEdge.apply(new Triangle(lp1[i], lp1[j], lp2[2]), eps)) return false;
        }

        // for each edge E of t2
        for (int i = 0; i < 3; ++i) {
            int j = (i + 1) % 3;
            // Check all points of t1 lay on the external side of edge E.
            // If they do, the triangles do not overlap.
            if (chkEdge.apply(new Triangle(lp2[i], lp2[j], lp1[0]), eps) &&
                chkEdge.apply(new Triangle(lp2[i], lp2[j], lp1[1]), eps) &&
                chkEdge.apply(new Triangle(lp2[i], lp2[j], lp1[2]), eps)) return false;
        }

        // The triangles overlap
        return true;
    }

    public static void main(String[] args) {
        Triangle t1 = new Triangle(new Pair(0.0, 0.0), new Pair(5.0, 0.0), new Pair(0.0, 5.0));
        Triangle t2 = new Triangle(new Pair(0.0, 0.0), new Pair(5.0, 0.0), new Pair(0.0, 6.0));
        System.out.printf("%s and\n%s\n", t1, t2);
        if (triTri2D(t1, t2)) {
            System.out.println("overlap");
        } else {
            System.out.println("do not overlap");
        }

        // need to allow reversed for this pair to avoid exception
        t1 = new Triangle(new Pair(0.0, 0.0), new Pair(0.0, 5.0), new Pair(5.0, 0.0));
        t2 = t1;
        System.out.printf("\n%s and\n%s\n", t1, t2);
        if (triTri2D(t1, t2, 0.0, true)) {
            System.out.println("overlap (reversed)");
        } else {
            System.out.println("do not overlap");
        }

        t1 = new Triangle(new Pair(0.0, 0.0), new Pair(5.0, 0.0), new Pair(0.0, 5.0));
        t2 = new Triangle(new Pair(-10.0, 0.0), new Pair(-5.0, 0.0), new Pair(-1.0, 6.0));
        System.out.printf("\n%s and\n%s\n", t1, t2);
        if (triTri2D(t1, t2)) {
            System.out.println("overlap");
        } else {
            System.out.println("do not overlap");
        }

        t1.p3 = new Pair(2.5, 5.0);
        t2 = new Triangle(new Pair(0.0, 4.0), new Pair(2.5, -1.0), new Pair(5.0, 4.0));
        System.out.printf("\n%s and\n%s\n", t1, t2);
        if (triTri2D(t1, t2)) {
            System.out.println("overlap");
        } else {
            System.out.println("do not overlap");
        }

        t1 = new Triangle(new Pair(0.0, 0.0), new Pair(1.0, 1.0), new Pair(0.0, 2.0));
        t2 = new Triangle(new Pair(2.0, 1.0), new Pair(3.0, 0.0), new Pair(3.0, 2.0));
        System.out.printf("\n%s and\n%s\n", t1, t2);
        if (triTri2D(t1, t2)) {
            System.out.println("overlap");
        } else {
            System.out.println("do not overlap");
        }

        t2 = new Triangle(new Pair(2.0, 1.0), new Pair(3.0, -2.0), new Pair(3.0, 4.0));
        System.out.printf("\n%s and\n%s\n", t1, t2);
        if (triTri2D(t1, t2)) {
            System.out.println("overlap");
        } else {
            System.out.println("do not overlap");
        }

        t1 = new Triangle(new Pair(0.0, 0.0), new Pair(1.0, 0.0), new Pair(0.0, 1.0));
        t2 = new Triangle(new Pair(1.0, 0.0), new Pair(2.0, 0.0), new Pair(1.0, 1.1));
        System.out.printf("\n%s and\n%s\n", t1, t2);
        System.out.println("which have only a single corner in contact, if boundary points collide");
        if (triTri2D(t1, t2)) {
            System.out.println("overlap");
        } else {
            System.out.println("do not overlap");
        }

        System.out.printf("\n%s and\n%s\n", t1, t2);
        System.out.println("which have only a single corner in contact, if boundary points do not collide");
        if (triTri2D(t1, t2, 0.0, false, false)) {
            System.out.println("overlap");
        } else {
            System.out.println("do not overlap");
        }
    }
}
```

{{out}}

```txt
Triangle: (0.0, 0.0), (5.0, 0.0), (0.0, 5.0) and
Triangle: (0.0, 0.0), (5.0, 0.0), (0.0, 6.0)
overlap

Triangle: (0.0, 0.0), (0.0, 5.0), (5.0, 0.0) and
Triangle: (0.0, 0.0), (0.0, 5.0), (5.0, 0.0)
overlap (reversed)

Triangle: (0.0, 0.0), (5.0, 0.0), (0.0, 5.0) and
Triangle: (-10.0, 0.0), (-5.0, 0.0), (-1.0, 6.0)
do not overlap

Triangle: (0.0, 0.0), (5.0, 0.0), (2.5, 5.0) and
Triangle: (0.0, 4.0), (2.5, -1.0), (5.0, 4.0)
overlap

Triangle: (0.0, 0.0), (1.0, 1.0), (0.0, 2.0) and
Triangle: (2.0, 1.0), (3.0, 0.0), (3.0, 2.0)
do not overlap

Triangle: (0.0, 0.0), (1.0, 1.0), (0.0, 2.0) and
Triangle: (2.0, 1.0), (3.0, -2.0), (3.0, 4.0)
do not overlap

Triangle: (0.0, 0.0), (1.0, 0.0), (0.0, 1.0) and
Triangle: (1.0, 0.0), (2.0, 0.0), (1.0, 1.1)
which have only a single corner in contact, if boundary points collide
overlap

Triangle: (0.0, 0.0), (1.0, 0.0), (0.0, 1.0) and
Triangle: (1.0, 0.0), (2.0, 0.0), (1.0, 1.1)
which have only a single corner in contact, if boundary points do not collide
do not overlap
```



## Julia

{{trans|Python}}
'''Module''':

```julia
module Triangles

using LinearAlgebra

export AntiClockwise, Both, StrictCheck, MildCheck

abstract type Widing end
struct AntiClockwise <: Widing end
struct Both          <: Widing end

function _check_triangle_winding(t, widing::AntiClockwise)
    trisq = fill!(Matrix{eltype(t)}(undef, 3, 3), 1)
    trisq[:, 1:2] .= t
    det(trisq) < 0 && throw(ArgumentError("triangle has wrong winding direction"))
    return trisq
end

function _check_triangle_winding(t, widing::Both)
    trisq = fill!(Matrix{eltype(t)}(undef, 3, 3), 1)
    trisq[:, 1:2] .= t
    if det(trisq) < 0
        tmp = trisq[2, :]
        trisq[2, :] .= trisq[1, :]
        trisq[1, :] .= tmp
    end
    return trisq
end

abstract type OnBoundaryCheck end
struct StrictCheck <: OnBoundaryCheck end
struct MildCheck   <: OnBoundaryCheck end

_checkedge(::StrictCheck, x, ϵ) = det(x) < ϵ
_checkedge(::MildCheck, x, ϵ)   = det(x) ≤ ϵ

function overlap(T₁, T₂, onboundary::OnBoundaryCheck=MildCheck(),; ϵ=0.0, widing::Widing=AntiClockwise())
    # Trangles must be expressed anti-clockwise
    T₁ = _check_triangle_winding(T₁, widing)
    T₂ = _check_triangle_winding(T₂, widing)

    edge = similar(T₁)
    for (A, B) in ((T₁, T₂), (T₂, T₁)), i in 1:3
        circshift!(edge, A, (i, 0))
        @views if all(_checkedge(onboundary, vcat(edge[1:2, :], B[r, :]'), ϵ) for r in 1:3)
            return false
        end
    end

    return true
end

end  # module Triangles
```


'''Main''':

```julia
using .Triangles

t1 = [0 0; 5 0; 0 5]
t2 = [0 0; 5 0; 0 6]
@show Triangles.overlap(t1, t2)

t1 = [0 0; 0 5; 5 0]
t2 = [0 0; 0 6; 5 0]
@show Triangles.overlap(t1, t2, widing=Both())

t1 = [0 0; 5 0; 0 5]
t2 = [-10 0; -5 0; -1 6]
@show Triangles.overlap(t1, t2)

t1 = [0 0; 5 0; 2.5 5]
t2 = [0 4; 2.5 -1; 5 4]
@show Triangles.overlap(t1, t2)

t1 = [0 0; 1 1; 0 2]
t2 = [2 1; 3 0; 3 2]
@show Triangles.overlap(t1, t2)

t1 = [0 0; 1 1; 0 2]
t2 = [2 1; 3 -2; 3 4]
@show Triangles.overlap(t1, t2)

# Barely touching
t1 = [0 0; 1 0; 0 1]
t2 = [1 0; 2 0; 1 1]
@show Triangles.overlap(t1, t2, StrictCheck())

# Barely touching
t1 = [0 0; 1 0; 0 1]
t2 = [1 0; 2 0; 1 1]
@show Triangles.overlap(t1, t2, MildCheck())
```


{{out}}

```txt
Triangles.overlap(t1, t2) = true
Triangles.overlap(t1, t2, widing=Both()) = true
Triangles.overlap(t1, t2) = false
Triangles.overlap(t1, t2) = true
Triangles.overlap(t1, t2) = false
Triangles.overlap(t1, t2) = false
Triangles.overlap(t1, t2, StrictCheck()) = true
Triangles.overlap(t1, t2, MildCheck()) = false
```



## Kotlin

{{trans|C++}}

```scala
// version 1.1.0

typealias Point = Pair<Double, Double>

data class Triangle(var p1: Point, var p2: Point, var p3: Point) {
    override fun toString() = "Triangle: $p1, $p2, $p3"
}

fun det2D(t: Triangle): Double {
    val (p1, p2, p3) = t
    return  p1.first * (p2.second - p3.second) +
            p2.first * (p3.second - p1.second) +
            p3.first * (p1.second - p2.second)
}

fun checkTriWinding(t: Triangle, allowReversed: Boolean) {
    val detTri = det2D(t)
    if (detTri < 0.0) {
        if (allowReversed) {
           val a = t.p3
	   t.p3  = t.p2
	   t.p2 =  a
        }
        else throw RuntimeException("Triangle has wrong winding direction")
    }
}

fun boundaryCollideChk(t: Triangle, eps: Double) = det2D(t) < eps

fun boundaryDoesntCollideChk(t: Triangle, eps: Double) = det2D(t) <= eps

fun triTri2D(t1: Triangle, t2: Triangle, eps: Double = 0.0,
             allowReversed: Boolean = false, onBoundary: Boolean = true): Boolean {
    // Triangles must be expressed anti-clockwise
    checkTriWinding(t1, allowReversed)
    checkTriWinding(t2, allowReversed)
    // 'onBoundary' determines whether points on boundary are considered as colliding or not
    val chkEdge = if (onBoundary) ::boundaryCollideChk else ::boundaryDoesntCollideChk
    val lp1 = listOf(t1.p1, t1.p2, t1.p3)
    val lp2 = listOf(t2.p1, t2.p2, t2.p3)

    // for each edge E of t1
    for (i in 0 until 3) {
        val j = (i + 1) % 3
        // Check all points of t2 lay on the external side of edge E.
        // If they do, the triangles do not overlap.
	if (chkEdge(Triangle(lp1[i], lp1[j], lp2[0]), eps) &&
            chkEdge(Triangle(lp1[i], lp1[j], lp2[1]), eps) &&
            chkEdge(Triangle(lp1[i], lp1[j], lp2[2]), eps)) return false
    }

    // for each edge E of t2
    for (i in 0 until 3) {
        val j = (i + 1) % 3
        // Check all points of t1 lay on the external side of edge E.
        // If they do, the triangles do not overlap.
        if (chkEdge(Triangle(lp2[i], lp2[j], lp1[0]), eps) &&
            chkEdge(Triangle(lp2[i], lp2[j], lp1[1]), eps) &&
            chkEdge(Triangle(lp2[i], lp2[j], lp1[2]), eps)) return false
    }

    // The triangles overlap
    return true
}

fun main(args: Array<String>) {
    var t1 = Triangle(0.0 to 0.0, 5.0 to 0.0, 0.0 to 5.0)
    var t2 = Triangle(0.0 to 0.0, 5.0 to 0.0, 0.0 to 6.0)
    println("$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    // need to allow reversed for this pair to avoid exception
    t1 = Triangle(0.0 to 0.0, 0.0 to 5.0, 5.0 to 0.0)
    t2 = t1
    println("\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2, 0.0, true)) "overlap (reversed)" else "do not overlap")

    t1 = Triangle(0.0 to 0.0, 5.0 to 0.0, 0.0 to 5.0)
    t2 = Triangle(-10.0 to 0.0, -5.0 to 0.0, -1.0 to 6.0)
    println("\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t1.p3 = 2.5 to 5.0
    t2 = Triangle(0.0 to 4.0, 2.5 to -1.0, 5.0 to 4.0)
    println("\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t1 = Triangle(0.0 to 0.0, 1.0 to 1.0, 0.0 to 2.0)
    t2 = Triangle(2.0 to 1.0, 3.0 to 0.0, 3.0 to 2.0)
    println("\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t2 = Triangle(2.0 to 1.0, 3.0 to -2.0, 3.0 to 4.0)
    println("\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t1 = Triangle(0.0 to 0.0, 1.0 to 0.0, 0.0 to 1.0)
    t2 = Triangle(1.0 to 0.0, 2.0 to 0.0, 1.0 to 1.1)
    println("\n$t1 and\n$t2")
    println("which have only a single corner in contact, if boundary points collide")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    println("\n$t1 and\n$t2")
    println("which have only a single corner in contact, if boundary points do not collide")
    println(if (triTri2D(t1, t2, 0.0, false, false)) "overlap" else "do not overlap")
}
```


{{out}}

```txt

Triangle: (0.0, 0.0), (5.0, 0.0), (0.0, 5.0) and
Triangle: (0.0, 0.0), (5.0, 0.0), (0.0, 6.0)
overlap

Triangle: (0.0, 0.0), (0.0, 5.0), (5.0, 0.0) and
Triangle: (0.0, 0.0), (0.0, 5.0), (5.0, 0.0)
overlap (reversed)

Triangle: (0.0, 0.0), (5.0, 0.0), (0.0, 5.0) and
Triangle: (-10.0, 0.0), (-5.0, 0.0), (-1.0, 6.0)
do not overlap

Triangle: (0.0, 0.0), (5.0, 0.0), (2.5, 5.0) and
Triangle: (0.0, 4.0), (2.5, -1.0), (5.0, 4.0)
overlap

Triangle: (0.0, 0.0), (1.0, 1.0), (0.0, 2.0) and
Triangle: (2.0, 1.0), (3.0, 0.0), (3.0, 2.0)
do not overlap

Triangle: (0.0, 0.0), (1.0, 1.0), (0.0, 2.0) and
Triangle: (2.0, 1.0), (3.0, -2.0), (3.0, 4.0)
do not overlap

Triangle: (0.0, 0.0), (1.0, 0.0), (0.0, 1.0) and
Triangle: (1.0, 0.0), (2.0, 0.0), (1.0, 1.1)
which have only a single corner in contact, if boundary points collide
overlap

Triangle: (0.0, 0.0), (1.0, 0.0), (0.0, 1.0) and
Triangle: (1.0, 0.0), (2.0, 0.0), (1.0, 1.1)
which have only a single corner in contact, if boundary points do not collide
do not overlap

```



## Lua

{{trans|C++}}

```lua
function det2D(p1,p2,p3)
    return p1.x * (p2.y - p3.y)
         + p2.x * (p3.y - p1.y)
         + p3.x * (p1.y - p2.y)
end

function checkTriWinding(p1,p2,p3,allowReversed)
    local detTri = det2D(p1,p2,p3)
    if detTri < 0.0 then
        if allowReversed then
            local t = p3
            p3 = p2
            p2 = t
        else
            error("triangle has wrong winding direction")
        end
    end
    return nil
end

function boundaryCollideChk(p1,p2,p3,eps)
    return det2D(p1,p2,p3) < eps
end

function boundaryDoesntCollideChk(p1,p2,p3,eps)
    return det2D(p1,p2,p3) <= eps
end

function triTri2D(t1,t2,eps,allowReversed,onBoundary)
    eps = eps or 0.0
    allowReversed = allowReversed or false
    onBoundary = onBoundary or true

    -- triangles must be expressed anti-clockwise
    checkTriWinding(t1[1], t1[2], t1[3], allowReversed)
    checkTriWinding(t2[1], t2[2], t2[3], allowReversed)

    local chkEdge
    if onBoundary then
        -- points on the boundary are considered as colliding
        chkEdge = boundaryCollideChk
    else
        -- points on the boundary are not considered as colliding
        chkEdge = boundaryDoesntCollideChk
    end

    -- for edge E of triangle 1
    for i=0,2 do
        local j = (i+1)%3

        -- check all points of triangle 2 lay on the external side of the edge E.
        -- If they do, the triangles do not collide
        if chkEdge(t1[i+1], t1[j+1], t2[1], eps) and
           chkEdge(t1[i+1], t1[j+1], t2[2], eps) and
           chkEdge(t1[i+1], t1[j+1], t2[3], eps) then
            return false
        end
    end

    -- for edge E of triangle 2
    for i=0,2 do
        local j = (i+1)%3

        -- check all points of triangle 1 lay on the external side of the edge E.
        -- If they do, the triangles do not collide
        if chkEdge(t2[i+1], t2[j+1], t1[1], eps) and
           chkEdge(t2[i+1], t2[j+1], t1[2], eps) and
           chkEdge(t2[i+1], t2[j+1], t1[3], eps) then
            return false
        end
    end

    -- the triangles collide
    return true
end

function formatTri(t)
    return "Triangle: ("..t[1].x..", "..t[1].y
                .."), ("..t[2].x..", "..t[2].y
                .."), ("..t[3].x..", "..t[3].y..")"
end

function overlap(t1,t2,eps,allowReversed,onBoundary)
    if triTri2D(t1,t2,eps,allowReversed,onBoundary) then
        return "overlap\n"
    else
        return "do not overlap\n"
    end
end

-- Main
local t1 = {{x=0,y=0},{x=5,y=0},{x=0,y=5}}
local t2 = {{x=0,y=0},{x=5,y=0},{x=0,y=6}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2))

t1 = {{x=0,y=0},{x=0,y=5},{x=5,y=0}}
t2 = {{x=0,y=0},{x=0,y=5},{x=5,y=0}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2,0.0,true))

t1 = {{x=0,y=0},{x=5,y=0},{x=0,y=5}}
t2 = {{x=-10,y=0},{x=-5,y=0},{x=-1,y=6}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2))

t1 = {{x=0,y=0},{x=5,y=0},{x=2.5,y=5}}
t2 = {{x=0,y=4},{x=2.5,y=-1},{x=5,y=4}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2))

t1 = {{x=0,y=0},{x=1,y=1},{x=0,y=2}}
t2 = {{x=2,y=1},{x=3,y=0},{x=3,y=2}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2))

t1 = {{x=0,y=0},{x=1,y=1},{x=0,y=2}}
t2 = {{x=2,y=1},{x=3,y=-2},{x=3,y=4}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2))

-- Barely touching
t1 = {{x=0,y=0},{x=1,y=0},{x=0,y=1}}
t2 = {{x=1,y=0},{x=2,y=0},{x=1,y=1}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2,0.0,false,true))

-- Barely touching
local t1 = {{x=0,y=0},{x=1,y=0},{x=0,y=1}}
local t2 = {{x=1,y=0},{x=2,y=0},{x=1,y=1}}
print(formatTri(t1).." and")
print(formatTri(t2))
print(overlap(t1,t2,0.0,false,false))
```

{{out}}

```txt
Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (0, 0), (5, 0), (0, 6)
overlap

Triangle: (0, 0), (0, 5), (5, 0) and
Triangle: (0, 0), (0, 5), (5, 0)
overlap

Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (-10, 0), (-5, 0), (-1, 6)
do not overlap

Triangle: (0, 0), (5, 0), (2.5, 5) and
Triangle: (0, 4), (2.5, -1), (5, 4)
overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, 0), (3, 2)
do not overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, -2), (3, 4)
do not overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1)
overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1)
overlap
```


=={{header|Modula-2}}==

```modula2
MODULE Overlap;
FROM EXCEPTIONS IMPORT AllocateSource,ExceptionSource,GetMessage,RAISE;
FROM LongStr IMPORT RealToFixed;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE
    Point = RECORD
        x,y : LONGREAL;
    END;
    Triangle = RECORD
        p1,p2,p3 : Point;
    END;

VAR
    TextWinExSrc : ExceptionSource;

PROCEDURE WritePoint(p : Point);
VAR buf : ARRAY[0..31] OF CHAR;
BEGIN
    WriteString("(");
    RealToFixed(p.x, 2, buf);
    WriteString(buf);
    WriteString(", ");
    RealToFixed(p.y, 2, buf);
    WriteString(buf);
    WriteString(")")
END WritePoint;

PROCEDURE WriteTriangle(t : Triangle);
BEGIN
    WriteString("Triangle: ");
    WritePoint(t.p1);
    WriteString(", ");
    WritePoint(t.p2);
    WriteString(", ");
    WritePoint(t.p3)
END WriteTriangle;

PROCEDURE Det2D(p1,p2,p3 : Point) : LONGREAL;
BEGIN
    RETURN p1.x * (p2.y - p3.y)
         + p2.x * (p3.y - p1.y)
         + p3.x * (p1.y - p2.y)
END Det2D;

PROCEDURE CheckTriWinding(VAR p1,p2,p3 : Point; allowReversed : BOOLEAN);
VAR
    detTri : LONGREAL;
    t : Point;
BEGIN
    detTri := Det2D(p1, p2, p3);
    IF detTri < 0.0 THEN
        IF allowReversed THEN
            t := p3;
            p3 := p2;
            p2 := t
        ELSE
            RAISE(TextWinExSrc, 0, "triangle has wrong winding direction")
        END
    END
END CheckTriWinding;

PROCEDURE BoundaryCollideChk(p1,p2,p3 : Point; eps : LONGREAL) : BOOLEAN;
BEGIN
    RETURN Det2D(p1, p2, p3) < eps
END BoundaryCollideChk;

PROCEDURE BoundaryDoesntCollideChk(p1,p2,p3 : Point; eps : LONGREAL) : BOOLEAN;
BEGIN
    RETURN Det2D(p1, p2, p3) <= eps
END BoundaryDoesntCollideChk;

PROCEDURE TriTri2D(t1,t2 : Triangle; eps : LONGREAL; allowReversed,onBoundary : BOOLEAN) : BOOLEAN;
TYPE
    Points = ARRAY[0..2] OF Point;
VAR
    chkEdge : PROCEDURE(Point, Point, Point, LONGREAL) : BOOLEAN;
    lp1,lp2 : Points;
    i,j : CARDINAL;
BEGIN
    (* Triangles must be expressed anti-clockwise *)
    CheckTriWinding(t1.p1, t1.p2, t1.p3, allowReversed);
    CheckTriWinding(t2.p1, t2.p2, t2.p3, allowReversed);

    (* 'onBoundary' determines whether points on boundary are considered as colliding or not *)
    IF onBoundary THEN
        chkEdge := BoundaryCollideChk
    ELSE
        chkEdge := BoundaryDoesntCollideChk
    END;

    lp1 := Points{t1.p1, t1.p2, t1.p3};
    lp2 := Points{t2.p1, t2.p2, t2.p3};

    (* for each edge E of t1 *)
    FOR i:=0 TO 2 DO
        j := (i + 1) MOD 3;
        (* Check all points of t2 lay on the external side of edge E.
           If they do, the triangles do not overlap. *)
        IF     chkEdge(lp1[i], lp1[j], lp2[0], eps)
           AND chkEdge(lp1[i], lp1[j], lp2[1], eps)
           AND chkEdge(lp1[i], lp1[j], lp2[2], eps)
        THEN
            RETURN FALSE
        END
    END;

    (* for each edge E of t2 *)
    FOR i:=0 TO 2 DO
        j := (i + 1) MOD 3;
        (* Check all points of t1 lay on the external side of edge E.
           If they do, the triangles do not overlap. *)
        IF     chkEdge(lp2[i], lp2[j], lp1[0], eps)
           AND chkEdge(lp2[i], lp2[j], lp1[1], eps)
           AND chkEdge(lp2[i], lp2[j], lp1[2], eps)
        THEN
            RETURN FALSE
        END
    END;

    (* The triangles overlap *)
    RETURN TRUE
END TriTri2D;

PROCEDURE CheckOverlap(t1,t2 : Triangle; eps : LONGREAL; allowReversed,onBoundary : BOOLEAN);
BEGIN
    WriteTriangle(t1);
    WriteString(" and");
    WriteLn;
    WriteTriangle(t2);
    WriteLn;

    IF TriTri2D(t1, t2, eps, allowReversed, onBoundary) THEN
        WriteString("overlap")
    ELSE
        WriteString("do not overlap")
    END;
    WriteLn
END CheckOverlap;

(* main *)
VAR
    t1,t2 : Triangle;
BEGIN
    t1 := Triangle{{0.0,0.0},{5.0,0.0},{0.0,5.0}};
    t2 := Triangle{{0.0,0.0},{5.0,0.0},{0.0,6.0}};
    CheckOverlap(t1, t2, 0.0, FALSE, TRUE);
    WriteLn;

    t1 := Triangle{{0.0,0.0},{0.0,5.0},{5.0,0.0}};
    t2 := Triangle{{0.0,0.0},{0.0,5.0},{5.0,0.0}};
    CheckOverlap(t1, t2, 0.0, TRUE, TRUE);
    WriteLn;

    t1 := Triangle{{0.0,0.0},{5.0,0.0},{0.0,5.0}};
    t2 := Triangle{{-10.0,0.0},{-5.0,0.0},{-1.0,6.0}};
    CheckOverlap(t1, t2, 0.0, FALSE, TRUE);
    WriteLn;

    t1 := Triangle{{0.0,0.0},{5.0,0.0},{2.5,5.0}};
    t2 := Triangle{{0.0,4.0},{2.5,-1.0},{5.0,4.0}};
    CheckOverlap(t1, t2, 0.0, FALSE, TRUE);
    WriteLn;

    t1 := Triangle{{0.0,0.0},{1.0,1.0},{0.0,2.0}};
    t2 := Triangle{{2.0,1.0},{3.0,0.0},{3.0,2.0}};
    CheckOverlap(t1, t2, 0.0, FALSE, TRUE);
    WriteLn;

    t1 := Triangle{{0.0,0.0},{1.0,1.0},{0.0,2.0}};
    t2 := Triangle{{2.0,1.0},{3.0,-2.0},{3.0,4.0}};
    CheckOverlap(t1, t2, 0.0, FALSE, TRUE);
    WriteLn;

    t1 := Triangle{{0.0,0.0},{1.0,0.0},{0.0,1.0}};
    t2 := Triangle{{1.0,0.0},{2.0,0.0},{1.0,1.1}};
    CheckOverlap(t1, t2, 0.0, FALSE, TRUE);
    WriteLn;

    t1 := Triangle{{0.0,0.0},{1.0,0.0},{0.0,1.0}};
    t2 := Triangle{{1.0,0.0},{2.0,0.0},{1.0,1.1}};
    CheckOverlap(t1, t2, 0.0, FALSE, FALSE);
    WriteLn;

    ReadChar
END Overlap.
```



## ooRexx


```oorexx
/*--------------------------------------------------------------------
* Determine if two triangles overlap
* Fully (?) tested with integer coordinates of the 6 corners
* This was/is an exercise with ooRexx
* Removed the fraction arithmetic
*-------------------------------------------------------------------*/
Parse Version v

oid='trioo.txt'; 'erase' oid
Call o v
case=0
cc=0
Call trio_test '0 0   4 0   0 4   1 1   2  1   1 2'
Call trio_test '0 0 0 6 8 3 8 0 8 8 0 3'
Call trio_test '0 0 0 2 2 0 0 0 4 0 0 6'
/* The task's specified input */
Call trio_test '0 0 5 0 0 5 0 0 5 0 0 6'
Call trio_test '0 0 0 5 5 0 0 0 0 5 5 0'
Call trio_test '0 0 5 0 0 5 -10 0 -5 0 -1 6'
Call trio_test '0 0 5 0 2.5 5 0 4 2.5 -1 5 4'
Call trio_test '0 0 1 1 0 2 2 1 3 0 3 2'
Call trio_test '0 0 1 1 0 2 2 1 3 -2 3 4'
Call trio_test '0 0 1 0 0 1 1 0 2 0 1 1'
Exit
/* Other test cases */
Call trio_test '0 0   0 4   4 0   0 2   2  2   2 0'
Call trio_test '0 0   0 5   5 0   0 0   0  5   5 0'
Call trio_test '0 0   0 5   5 0   0 0   0  5   7 0'
Call trio_test '0 0   1 0   0 1   1 0   2  0   1 1'
Call trio_test '0 0   1 1   0 2   2 1   3  0   3 2'
Call trio_test '0 0   1 1   0 2   2 1   3 -2   3 4'
Call trio_test '0 0   2 0   2 2   3 3   5  3   5 5'
Call trio_test '0 0   2 0   2 3   0 0   2  0   2 3'
Call trio_test '0 0   4 0   0 4   0 2   2  0   2 2'
Call trio_test '0 0   4 0   0 4   1 1   2  1   1 2'
Call trio_test '0 0   5 0   0 2   5 0   8  0   4 8'
Call trio_test '0 0   5 0   0 5   0 0   5  0   0 6'
Call trio_test '0 0   5 0   0 5 -10 0  -5  0  -1 6'
Call trio_test '0 0   5 0   0 5  -5 0  -1  6  -3 0'
Call trio_test '0 0   5 0   3 5   0 4   3 -1   5 4'
Call trio_test '0 0   6 0   4 6   1 1   4  2   7 1'
Call trio_test '0 1   0 4   2 2   3 1   3  4   5 2'
Call trio_test '1 0   3 0   2 2   1 3   3  3   2 2'
Call trio_test '1 0   3 0   2 2   1 3   3  3   2 5'
Call trio_test '1 1   4 2   7 1   0 0   8  0   4 8'
Call trio_test '2 0   2 6   1 8   0 1   0  5   8 3'
Call trio_test '0 0   4 0   0 4   1 1   2  1   1 2'
Say case 'cases tested'
Say cc
Exit

trio_test:
Parse Arg tlist
cc+=1
tlist=space(tlist)
tl1=tlist                              ; Call trio_t tl1
tl2=reversex(tlist)                    ; Call trio_t tl2
tl3=''
tl=tlist
Do While tl<>''
  Parse Var tl x y tl
  tl3=tl3 y x
  End
                                         Call trio_t tl3
tl4=reversex(tl3)                      ; Call trio_t tl4
tl5=subword(tl4,7) subword(tl4,1,6)    ; Call trio_t tl5
tl6=subword(tl5,7) subword(tl5,1,6)    ; Call trio_t tl6
Return

trio_t:
Parse Arg tlist
tlist=space(tlist)
Say tlist
case+=1
Parse Arg ax ay bx by cx cy dx dy ex ey fx fy
/*---------------------------------------------------------------------
* First build the objects needed
*--------------------------------------------------------------------*/
a=.point~new(ax,ay); b=.point~new(bx,by); c=.point~new(cx,cy)
d=.point~new(dx,dy); e=.point~new(ex,ey); f=.point~new(fx,fy)
abc=.triangle~new(a,b,c)
def=.triangle~new(d,e,f)
Call o 'Triangle: ABC:' abc ,1
Call o 'Edges of ABC:'; Do i=1 To 3; Call o ' 'abc~edge(i); End
Call o 'Triangle: DEF:' def ,1
Call o 'Edges of DEF:'; Do i=1 To 3; Call o ' 'def~edge(i); End
pixl=' '
Do i=1 To 3
  pixl=pixl abc~draw(i,'O')
  pixl=pixl def~draw(i,'*')
  End
res=0
fc=0
touch=0
bordl=''
Do i=1 To 3
  p1=abc~point(i)
  p2=def~point(i)
  Do j=1 To 3
    e1=abc~edge(j)
    e2=def~edge(j)
    If e1~contains(p2) Then Do
      Call o e1 'contains' p2
      ps=p2~string
      If wordpos(ps,bordl)=0 Then Do
        bordl=bordl ps
        touch+=1
        End
      End
    Else
      Call o e1 'does not contain' p2 i j
    If e2~contains(p1) Then Do
      Call o e2 'contains' p1
      ps=p1~string
      If wordpos(ps,bordl)=0 Then Do
        bordl=bordl ps
        touch+=1
        End
      End
    Else
      Call o e2 'does not contain' p1
    End
  End

wb=words(bordl)                     /* how many of them?             */
If wb>0 Then
  Call o 'Corner(s) that touch the other triangle:' bordl,1

/*---------------------------------------------------------------------
* How many of them are corners of both triangles
*--------------------------------------------------------------------*/
m=0
cmatch=''
do i=1 To 3
  If wordpos(abc~point(i),bordl)>0 &,
     wordpos(abc~point(i),def)>0 Then Do
    cmatch=cmatch abc~point(i)
    m+=1
    End
  End

/*---------------------------------------------------------------------
* With two or three touching corners we show the result and return
*--------------------------------------------------------------------*/
Select
  When wb=3 Then Do                 /* all three touch               */
    Call draw(pixl)
    Select
      When m=3 Then
        Call o 'Triangles are identical',1
      When m=2 Then
        Call o 'Triangles have an edge in common:' cmatch,1
      Otherwise
        Call o 'Triangles overlap and touch on' bordl,1
      End
    Call o '',1
 --   Pull .
    Return
    End
  When wb=2 Then Do                 /* two of them match             */
    Call draw(pixl)
    If m=2 Then
      Call o 'Triangles have an edge in common:' cmatch,1
    Else
      Call o 'Triangles overlap and touch on' bordl,1
    Call o ''
 --   Pull .
    Return
    End
  When wb=1 Then Do                 /* one of them matches          */
    Call o 'Triangles touch on' bordl,1 /* other parts may overlap  */
    Call o '  we analyze further',1
    End
  Otherwise                         /* we know nothing yet           */
    Nop
  End

/*---------------------------------------------------------------------
* Now we look for corners of abc that are within the triangle def
*--------------------------------------------------------------------*/
in_def=0
Do i=1 To 3
  p=abc~point(i)
  Call o 'p  ='p
  Call o 'def='def
  If def~contains(p) &,
    wordpos(p,bordl)=0 Then Do
    Call o def 'contains' p
    in_def+=1
    End
  End

If in_def=3 Then Do
  Call o abc 'is fully contained in' def,1
  Call o '',1
  Call draw(pixl)
  fc=1
  End
res=(in_def>0)
/*---------------------------------------------------------------------
* Now we look for corners of def that are within the triangle abc
*--------------------------------------------------------------------*/
If res=0 Then Do
  in_abc=0
  If res=0 Then Do
    Do i=1 To 3
      p=def~point(i)
      Call o 'p  ='p
      Call o 'def='def
      If abc~contains(p) &,
         wordpos(p,bordl)=0 Then Do
        Call o abc 'contains' p
        in_abc+=1
        End
      End
    End
  If in_abc=3 Then Do
    Call o def 'is fully contained in' abc,1
    Call o '',1
    Call draw(pixl)
    fc=1
    End
  res=(in_abc>0)

  End

/*---------------------------------------------------------------------
* Now we check if some edge of abc crosses any edge of def
*--------------------------------------------------------------------*/
If res=0 Then Do
  Do i=1 To 3
    Do j=1 To 3
      e1=abc~edge(i); Call o 'e1='e1
      e2=def~edge(j); Call o 'e2='e2
      Call o 'crossing???'
      res=e1~crosses(e2)
If res Then Do
  End
      If res Then
        Call o 'edges cross'
      Else
        Call o 'edges don''t cross'
      End
    End
  End

If fc=0 Then Do                     /* no fully contained            */
  Call draw(pixl)
  If res=0 Then                     /* no overlap                    */
    If wb=1 Then                    /* but one touching corner       */
      call o abc 'and' def 'don''t overlap but touch on' bordl,1
    Else
      call o abc 'and' def 'don''t overlap',1
  Else                              /* overlap                       */
    If wb>0 Then                    /* one touching corner           */
      call o abc 'and' def 'overlap and touch on' bordl,1
    Else
      call o abc 'and' def 'overlap',1
  Call o '',1
--  Pull .
  End
Return

/*---------------------------------------------------------------------
* And here are all the classes and methods needed:
* point         init, x, y, string
* triangle      init, point, edge, contains, string
* edge          init, p1, p2, kdx, contains, crosses, string
*--------------------------------------------------------------------*/

::class point public
::attribute x
::attribute y
::method init
  expose x y
  use arg x,y
::method string
  expose x y
  return "("||x","y")"

::class triangle public
::method init
  expose point edge
  use arg p1,p2,p3
  point=.array~new
  point[1]=p1
  point[2]=p2
  point[3]=p3
  edge=.array~new
  Do i=1 To 3
    ia=i+1; If ia=4 Then ia=1
    edge[i]=.edge~new(point[i],point[ia])
    End
::method point
  expose point
  use arg n
  Return point[n]
::method edge
  expose edge
  use arg n
  Return edge[n]
::method contains
  expose point edge
  use arg pp
  Call o self
  Call o 'pp='pp
  xmin=1.e9
  ymin=1.e9
  xmax=-1.e9
  ymax=-1.e9
  Do i=1 To 3
    e=edge[i]
    Parse Value e~kdx With ka.i da.i xa.i
    Call o show_g(ka.i,da.i,xa.i)
    p1=e~p1
    p2=e~p2
    xmin=min(xmin,p1~x,p2~x)
    xmax=max(xmax,p1~x,p2~x)
    ymin=min(ymin,p1~y,p2~y)
    ymax=max(ymax,p1~y,p2~y)
    End
  If pp~x<xmin|pp~x>xmax|pp~y<ymin|pp~y>ymax Then
    res=0
  Else Do
    e=edge[1]
    e2=edge[2]
    p1=e2~p1
    p2=e2~p2
    Call o 'e:' e
    Select
      When ka.1='*' Then Do
        y2=ka.2*pp~x+da.2
        y3=ka.3*pp~x+da.3
        res=between(y2,pp~y,y3)
        End
      When ka.2='*' Then Do
        y2=ka.1*pp~x+da.1
        res=between(p1~y,y2,p2~y)
        End
      Otherwise Do
        dap=pp~y-ka.1*pp~x
        If ka.3='*' Then
          x3=xa.3
        Else
          x3=(da.3-dap)/(ka.1-ka.3)
        x2=(da.2-dap)/(ka.1-ka.2)
        res=between(x2,pp~x,x3)
        End
      End
    End
  Return res
::method string
  expose point
  ol=''
  Do p over point
    ol=ol p~string
    End
  return ol
::method draw
  expose point
  Use Arg i,c
  p=self~point(i)
  Return p~x p~y c
::class edge public
::method init
  expose edge p1 p2
  use arg p1,p2
  edge=.array~new
  edge[1]=p1
  edge[2]=p2
::method p1
  expose edge p1 p2
  return p1
::method p2
  expose edge p1 p2
  return p2
::method kdx
  expose edge p1 p2
  x1=p1~x
  y1=p1~y
  x2=p2~x
  y2=p2~y
  If x1=x2 Then Do
    Parse Value '*' '-' x1 With ka da xa
      Call o show_g(ka,da,xa)
    End
  Else Do
    ka=(y2-y1)/(x2-x1)
    da=y2-ka*x2
    xa='*'
    End
  Return ka da xa
::method contains
  Use Arg p
  p1=self~p1
  p2=self~p2
  parse Value self~kdx With k d x
  If k='*' Then Do
    res=(p~x=p1~x)&between(p1~y,p~y,p2~y,'I')
    End
  Else Do
    ey=k*p~x+d
    res=(ey=p~y)&between(p1~x,p~x,p2~x,'I')
    End
  If res Then Call o self 'contains' p
         Else Call o self 'does not contain' p
  Return res
::method crosses
  expose p1 p2
  Use Arg e
  q1=e~p1
  q2=e~p2
  Call o 'Test if' e 'crosses' self
  Call o self~kdx
  Call o e~kdx
  Parse Value self~kdx With ka da xa; Call o ka da xa
    Call o show_g(ka,da,xa)
  Parse Value    e~kdx With kb db xb; Call o kb db xb
    Call o show_g(kb,db,xb)
  Call o 'ka='ka
  Call o 'kb='kb
  Select
    When ka='*' Then Do
      If kb='*' Then Do
        res=(xa=xb)
        End
      Else Do
        Call o 'kb='kb 'xa='||xa  'db='db
        yy=kb*xa+db
        res=between(q1~y,yy,q2~y)
        End
      End
    When kb='*' Then Do
      yy=ka*xb+da
      res=between(p1~y,yy,p2~y)
      End
    When ka=kb Then Do
      If da=db Then Do
        If min(p1~x,p2~x)>max(q1~x,q2~x) |,
           min(q1~x,q2~x)>max(p1~x,p2~x) Then
          res=0
        Else Do
          res=1
          End
        End
      Else
        res=0
      End
    Otherwise Do
      x=(db-da)/(ka-kb)
      y=ka*x+da
      Call o 'cross:' x y
      res=between(p1~x,x,p2~x)
      End
    End
  Return res
::method string
  expose edge p1 p2
  ol=p1~string'-'p2~string
  return ol

::routine between         /* check if a number is between two others */
  Use Arg a,x,b,inc
  Call o 'between:' a x b
  Parse Var a anom '/' adenom
  Parse Var x xnom '/' xdenom
  Parse Var b bnom '/' bdenom
  If adenom='' Then adenom=1
  If xdenom='' Then xdenom=1
  If bdenom='' Then bdenom=1
  aa=anom*xdenom*bdenom
  xx=xnom*adenom*bdenom
  bb=bnom*xdenom*adenom
  If inc='I' Then
    res=sign(xx-aa)<>sign(xx-bb)
  Else
    res=sign(xx-aa)<>sign(xx-bb) & (xx-aa)*(xx-bb)<>0
  Call o a x b 'res='res
  Return res

::routine show_g          /* show a straight line's forula           */
/*---------------------------------------------------------------------
* given slope, y-distance, and (special) x-value
* compute y=k*x+d or, if a vertical line, k='*'; x=c
*--------------------------------------------------------------------*/
  Use Arg k,d,x
  Select
    When k='*' Then res='x='||x       /* vertical line               */
    When k=0   Then res='y='d         /* horizontal line             */
    Otherwise Do                      /* ordinary line               */
      Select
        When k=1  Then res='y=x'dd(d)
        When k=-1 Then res='y=-x'dd(d)
        Otherwise      res='y='k'*x'dd(d)
        End
      End
    End
  Return res

::routine dd  /* prepare a displacement for presenting it in show_g  */
/*---------------------------------------------------------------------
* prepare y-distance for display
*--------------------------------------------------------------------*/
  Use Arg dd
  Select
    When dd=0 Then dd=''            /* omit dd if it's zero          */
    When dd<0 Then dd=dd            /* use dd as is (-value)         */
    Otherwise      dd='+'dd         /* prepend '+' to positive dd    */
    End
  Return dd

::routine o               /* debug output                            */
  Use Arg txt,say
  If say=1 Then
    Say txt
  oid='trioo.txt'
  Return lineout(oid,txt)

::routine draw
  Use Arg pixl
    Return                       /* remove to see the triangle corners */
  Say 'pixl='pixl
  pix.=' '
  Do While pixl<>''
    Parse Var pixl x y c pixl
    x=2*x+16; y=2*y+4
    If pix.x.y=' ' Then
      pix.x.y=c
    Else
      pix.x.y='+'
    End
  Do j= 20 To 0 By -1
    ol=''
    Do i=0 To 40
      ol=ol||pix.i.j
      End
    Say ol
    End
    Return
::routine reversex
  Use Arg list
  n=words(list)
  res=word(list,n)
  Do i=n-1 to 1 By -1
    res=res word(list,i)
    End
  Return res
```

{{out}}

```txt
0 0 4 0 0 4 1 1 2 1 1 2
Triangle: ABC:  (0,0) (4,0) (0,4)
Triangle: DEF:  (1,1) (2,1) (1,2)
 (1,1) (2,1) (1,2) is fully contained in  (0,0) (4,0) (0,4)

2 1 1 2 1 1 4 0 0 4 0 0
Triangle: ABC:  (2,1) (1,2) (1,1)
Triangle: DEF:  (4,0) (0,4) (0,0)
 (2,1) (1,2) (1,1) is fully contained in  (4,0) (0,4) (0,0)

1 2 2 1 1 1 0 4 4 0 0 0
Triangle: ABC:  (1,2) (2,1) (1,1)
Triangle: DEF:  (0,4) (4,0) (0,0)
 (1,2) (2,1) (1,1) is fully contained in  (0,4) (4,0) (0,0)

0 0 0 4 4 0 1 1 1 2 2 1
Triangle: ABC:  (0,0) (0,4) (4,0)
Triangle: DEF:  (1,1) (1,2) (2,1)
 (1,1) (1,2) (2,1) is fully contained in  (0,0) (0,4) (4,0)

1 1 1 2 2 1 0 0 0 4 4 0
Triangle: ABC:  (1,1) (1,2) (2,1)
Triangle: DEF:  (0,0) (0,4) (4,0)
 (1,1) (1,2) (2,1) is fully contained in  (0,0) (0,4) (4,0)

0 0 0 4 4 0 1 1 1 2 2 1
Triangle: ABC:  (0,0) (0,4) (4,0)
Triangle: DEF:  (1,1) (1,2) (2,1)
 (1,1) (1,2) (2,1) is fully contained in  (0,0) (0,4) (4,0)

0 0 0 6 8 3 8 0 8 8 0 3
Triangle: ABC:  (0,0) (0,6) (8,3)
Triangle: DEF:  (8,0) (8,8) (0,3)
Corner(s) that touch the other triangle:  (0,3) (8,3)
Triangles overlap and touch on  (0,3) (8,3)
3 0 8 8 0 8 3 8 6 0 0 0
Triangle: ABC:  (3,0) (8,8) (0,8)
Triangle: DEF:  (3,8) (6,0) (0,0)
Corner(s) that touch the other triangle:  (3,8) (3,0)
Triangles overlap and touch on  (3,8) (3,0)
0 3 8 8 8 0 8 3 0 6 0 0
Triangle: ABC:  (0,3) (8,8) (8,0)
Triangle: DEF:  (8,3) (0,6) (0,0)
Corner(s) that touch the other triangle:  (8,3) (0,3)
Triangles overlap and touch on  (8,3) (0,3)
0 0 6 0 3 8 0 8 8 8 3 0
Triangle: ABC:  (0,0) (6,0) (3,8)
Triangle: DEF:  (0,8) (8,8) (3,0)
Corner(s) that touch the other triangle:  (3,0) (3,8)
Triangles overlap and touch on  (3,0) (3,8)
0 8 8 8 3 0 0 0 6 0 3 8
Triangle: ABC:  (0,8) (8,8) (3,0)
Triangle: DEF:  (0,0) (6,0) (3,8)
Corner(s) that touch the other triangle:  (3,8) (3,0)
Triangles overlap and touch on  (3,8) (3,0)
0 0 6 0 3 8 0 8 8 8 3 0
Triangle: ABC:  (0,0) (6,0) (3,8)
Triangle: DEF:  (0,8) (8,8) (3,0)
Corner(s) that touch the other triangle:  (3,0) (3,8)
Triangles overlap and touch on  (3,0) (3,8)
0 0 0 2 2 0 0 0 4 0 0 6
Triangle: ABC:  (0,0) (0,2) (2,0)
Triangle: DEF:  (0,0) (4,0) (0,6)
Corner(s) that touch the other triangle:  (0,0) (0,2) (2,0)
Triangles overlap and touch on  (0,0) (0,2) (2,0)

6 0 0 4 0 0 0 2 2 0 0 0
Triangle: ABC:  (6,0) (0,4) (0,0)
Triangle: DEF:  (0,2) (2,0) (0,0)
Corner(s) that touch the other triangle:  (0,2) (2,0) (0,0)
Triangles overlap and touch on  (0,2) (2,0) (0,0)

0 6 4 0 0 0 2 0 0 2 0 0
Triangle: ABC:  (0,6) (4,0) (0,0)
Triangle: DEF:  (2,0) (0,2) (0,0)
Corner(s) that touch the other triangle:  (2,0) (0,2) (0,0)
Triangles overlap and touch on  (2,0) (0,2) (0,0)

0 0 2 0 0 2 0 0 0 4 6 0
Triangle: ABC:  (0,0) (2,0) (0,2)
Triangle: DEF:  (0,0) (0,4) (6,0)
Corner(s) that touch the other triangle:  (0,0) (2,0) (0,2)
Triangles overlap and touch on  (0,0) (2,0) (0,2)

0 0 0 4 6 0 0 0 2 0 0 2
Triangle: ABC:  (0,0) (0,4) (6,0)
Triangle: DEF:  (0,0) (2,0) (0,2)
Corner(s) that touch the other triangle:  (0,0) (2,0) (0,2)
Triangles overlap and touch on  (0,0) (2,0) (0,2)

0 0 2 0 0 2 0 0 0 4 6 0
Triangle: ABC:  (0,0) (2,0) (0,2)
Triangle: DEF:  (0,0) (0,4) (6,0)
Corner(s) that touch the other triangle:  (0,0) (2,0) (0,2)
Triangles overlap and touch on  (0,0) (2,0) (0,2)

0 0 5 0 0 5 0 0 5 0 0 6
Triangle: ABC:  (0,0) (5,0) (0,5)
Triangle: DEF:  (0,0) (5,0) (0,6)
Corner(s) that touch the other triangle:  (0,0) (5,0) (0,5)
Triangles have an edge in common:  (0,0) (5,0)

6 0 0 5 0 0 5 0 0 5 0 0
Triangle: ABC:  (6,0) (0,5) (0,0)
Triangle: DEF:  (5,0) (0,5) (0,0)
Corner(s) that touch the other triangle:  (5,0) (0,5) (0,0)
Triangles have an edge in common:  (0,5) (0,0)

0 6 5 0 0 0 0 5 5 0 0 0
Triangle: ABC:  (0,6) (5,0) (0,0)
Triangle: DEF:  (0,5) (5,0) (0,0)
Corner(s) that touch the other triangle:  (0,5) (5,0) (0,0)
Triangles have an edge in common:  (5,0) (0,0)

0 0 0 5 5 0 0 0 0 5 6 0
Triangle: ABC:  (0,0) (0,5) (5,0)
Triangle: DEF:  (0,0) (0,5) (6,0)
Corner(s) that touch the other triangle:  (0,0) (0,5) (5,0)
Triangles have an edge in common:  (0,0) (0,5)

0 0 0 5 6 0 0 0 0 5 5 0
Triangle: ABC:  (0,0) (0,5) (6,0)
Triangle: DEF:  (0,0) (0,5) (5,0)
Corner(s) that touch the other triangle:  (0,0) (0,5) (5,0)
Triangles have an edge in common:  (0,0) (0,5)

0 0 0 5 5 0 0 0 0 5 6 0
Triangle: ABC:  (0,0) (0,5) (5,0)
Triangle: DEF:  (0,0) (0,5) (6,0)
Corner(s) that touch the other triangle:  (0,0) (0,5) (5,0)
Triangles have an edge in common:  (0,0) (0,5)

0 0 0 5 5 0 0 0 0 5 5 0
Triangle: ABC:  (0,0) (0,5) (5,0)
Triangle: DEF:  (0,0) (0,5) (5,0)
Corner(s) that touch the other triangle:  (0,0) (0,5) (5,0)
Triangles are identical

0 5 5 0 0 0 0 5 5 0 0 0
Triangle: ABC:  (0,5) (5,0) (0,0)
Triangle: DEF:  (0,5) (5,0) (0,0)
Corner(s) that touch the other triangle:  (0,5) (5,0) (0,0)
Triangles are identical

5 0 0 5 0 0 5 0 0 5 0 0
Triangle: ABC:  (5,0) (0,5) (0,0)
Triangle: DEF:  (5,0) (0,5) (0,0)
Corner(s) that touch the other triangle:  (5,0) (0,5) (0,0)
Triangles are identical

0 0 5 0 0 5 0 0 5 0 0 5
Triangle: ABC:  (0,0) (5,0) (0,5)
Triangle: DEF:  (0,0) (5,0) (0,5)
Corner(s) that touch the other triangle:  (0,0) (5,0) (0,5)
Triangles are identical

0 0 5 0 0 5 0 0 5 0 0 5
Triangle: ABC:  (0,0) (5,0) (0,5)
Triangle: DEF:  (0,0) (5,0) (0,5)
Corner(s) that touch the other triangle:  (0,0) (5,0) (0,5)
Triangles are identical

0 0 5 0 0 5 0 0 5 0 0 5
Triangle: ABC:  (0,0) (5,0) (0,5)
Triangle: DEF:  (0,0) (5,0) (0,5)
Corner(s) that touch the other triangle:  (0,0) (5,0) (0,5)
Triangles are identical

0 0 5 0 0 5 -10 0 -5 0 -1 6
Triangle: ABC:  (0,0) (5,0) (0,5)
Triangle: DEF:  (-10,0) (-5,0) (-1,6)
 (0,0) (5,0) (0,5) and  (-10,0) (-5,0) (-1,6) don't overlap

6 -1 0 -5 0 -10 5 0 0 5 0 0
Triangle: ABC:  (6,-1) (0,-5) (0,-10)
Triangle: DEF:  (5,0) (0,5) (0,0)
 (6,-1) (0,-5) (0,-10) and  (5,0) (0,5) (0,0) don't overlap

-1 6 -5 0 -10 0 0 5 5 0 0 0
Triangle: ABC:  (-1,6) (-5,0) (-10,0)
Triangle: DEF:  (0,5) (5,0) (0,0)
 (-1,6) (-5,0) (-10,0) and  (0,5) (5,0) (0,0) don't overlap

0 0 0 5 5 0 0 -10 0 -5 6 -1
Triangle: ABC:  (0,0) (0,5) (5,0)
Triangle: DEF:  (0,-10) (0,-5) (6,-1)
 (0,0) (0,5) (5,0) and  (0,-10) (0,-5) (6,-1) don't overlap

0 -10 0 -5 6 -1 0 0 0 5 5 0
Triangle: ABC:  (0,-10) (0,-5) (6,-1)
Triangle: DEF:  (0,0) (0,5) (5,0)
 (0,-10) (0,-5) (6,-1) and  (0,0) (0,5) (5,0) don't overlap

0 0 0 5 5 0 0 -10 0 -5 6 -1
Triangle: ABC:  (0,0) (0,5) (5,0)
Triangle: DEF:  (0,-10) (0,-5) (6,-1)
 (0,0) (0,5) (5,0) and  (0,-10) (0,-5) (6,-1) don't overlap

0 0 5 0 2.5 5 0 4 2.5 -1 5 4
Triangle: ABC:  (0,0) (5,0) (2.5,5)
Triangle: DEF:  (0,4) (2.5,-1) (5,4)
 (0,0) (5,0) (2.5,5) and  (0,4) (2.5,-1) (5,4) overlap

4 5 -1 2.5 4 0 5 2.5 0 5 0 0
Triangle: ABC:  (4,5) (-1,2.5) (4,0)
Triangle: DEF:  (5,2.5) (0,5) (0,0)
 (4,5) (-1,2.5) (4,0) and  (5,2.5) (0,5) (0,0) overlap

5 4 2.5 -1 0 4 2.5 5 5 0 0 0
Triangle: ABC:  (5,4) (2.5,-1) (0,4)
Triangle: DEF:  (2.5,5) (5,0) (0,0)
 (5,4) (2.5,-1) (0,4) and  (2.5,5) (5,0) (0,0) overlap

0 0 0 5 5 2.5 4 0 -1 2.5 4 5
Triangle: ABC:  (0,0) (0,5) (5,2.5)
Triangle: DEF:  (4,0) (-1,2.5) (4,5)
 (0,0) (0,5) (5,2.5) and  (4,0) (-1,2.5) (4,5) overlap

4 0 -1 2.5 4 5 0 0 0 5 5 2.5
Triangle: ABC:  (4,0) (-1,2.5) (4,5)
Triangle: DEF:  (0,0) (0,5) (5,2.5)
 (4,0) (-1,2.5) (4,5) and  (0,0) (0,5) (5,2.5) overlap

0 0 0 5 5 2.5 4 0 -1 2.5 4 5
Triangle: ABC:  (0,0) (0,5) (5,2.5)
Triangle: DEF:  (4,0) (-1,2.5) (4,5)
 (0,0) (0,5) (5,2.5) and  (4,0) (-1,2.5) (4,5) overlap

0 0 1 1 0 2 2 1 3 0 3 2
Triangle: ABC:  (0,0) (1,1) (0,2)
Triangle: DEF:  (2,1) (3,0) (3,2)
 (0,0) (1,1) (0,2) and  (2,1) (3,0) (3,2) don't overlap

2 3 0 3 1 2 2 0 1 1 0 0
Triangle: ABC:  (2,3) (0,3) (1,2)
Triangle: DEF:  (2,0) (1,1) (0,0)
 (2,3) (0,3) (1,2) and  (2,0) (1,1) (0,0) don't overlap

3 2 3 0 2 1 0 2 1 1 0 0
Triangle: ABC:  (3,2) (3,0) (2,1)
Triangle: DEF:  (0,2) (1,1) (0,0)
 (3,2) (3,0) (2,1) and  (0,2) (1,1) (0,0) don't overlap

0 0 1 1 2 0 1 2 0 3 2 3
Triangle: ABC:  (0,0) (1,1) (2,0)
Triangle: DEF:  (1,2) (0,3) (2,3)
 (0,0) (1,1) (2,0) and  (1,2) (0,3) (2,3) don't overlap

1 2 0 3 2 3 0 0 1 1 2 0
Triangle: ABC:  (1,2) (0,3) (2,3)
Triangle: DEF:  (0,0) (1,1) (2,0)
 (1,2) (0,3) (2,3) and  (0,0) (1,1) (2,0) don't overlap

0 0 1 1 2 0 1 2 0 3 2 3
Triangle: ABC:  (0,0) (1,1) (2,0)
Triangle: DEF:  (1,2) (0,3) (2,3)
 (0,0) (1,1) (2,0) and  (1,2) (0,3) (2,3) don't overlap

0 0 1 1 0 2 2 1 3 -2 3 4
Triangle: ABC:  (0,0) (1,1) (0,2)
Triangle: DEF:  (2,1) (3,-2) (3,4)
 (0,0) (1,1) (0,2) and  (2,1) (3,-2) (3,4) don't overlap

4 3 -2 3 1 2 2 0 1 1 0 0
Triangle: ABC:  (4,3) (-2,3) (1,2)
Triangle: DEF:  (2,0) (1,1) (0,0)
 (4,3) (-2,3) (1,2) and  (2,0) (1,1) (0,0) don't overlap

3 4 3 -2 2 1 0 2 1 1 0 0
Triangle: ABC:  (3,4) (3,-2) (2,1)
Triangle: DEF:  (0,2) (1,1) (0,0)
 (3,4) (3,-2) (2,1) and  (0,2) (1,1) (0,0) don't overlap

0 0 1 1 2 0 1 2 -2 3 4 3
Triangle: ABC:  (0,0) (1,1) (2,0)
Triangle: DEF:  (1,2) (-2,3) (4,3)
 (0,0) (1,1) (2,0) and  (1,2) (-2,3) (4,3) don't overlap

1 2 -2 3 4 3 0 0 1 1 2 0
Triangle: ABC:  (1,2) (-2,3) (4,3)
Triangle: DEF:  (0,0) (1,1) (2,0)
 (1,2) (-2,3) (4,3) and  (0,0) (1,1) (2,0) don't overlap

0 0 1 1 2 0 1 2 -2 3 4 3
Triangle: ABC:  (0,0) (1,1) (2,0)
Triangle: DEF:  (1,2) (-2,3) (4,3)
 (0,0) (1,1) (2,0) and  (1,2) (-2,3) (4,3) don't overlap

0 0 1 0 0 1 1 0 2 0 1 1
Triangle: ABC:  (0,0) (1,0) (0,1)
Triangle: DEF:  (1,0) (2,0) (1,1)
Corner(s) that touch the other triangle:  (1,0)
Triangles touch on  (1,0)
  we analyze further
 (0,0) (1,0) (0,1) and  (1,0) (2,0) (1,1) don't overlap but touch on  (1,0)

1 1 0 2 0 1 1 0 0 1 0 0
Triangle: ABC:  (1,1) (0,2) (0,1)
Triangle: DEF:  (1,0) (0,1) (0,0)
Corner(s) that touch the other triangle:  (0,1)
Triangles touch on  (0,1)
  we analyze further
 (1,1) (0,2) (0,1) and  (1,0) (0,1) (0,0) don't overlap but touch on  (0,1)

1 1 2 0 1 0 0 1 1 0 0 0
Triangle: ABC:  (1,1) (2,0) (1,0)
Triangle: DEF:  (0,1) (1,0) (0,0)
Corner(s) that touch the other triangle:  (1,0)
Triangles touch on  (1,0)
  we analyze further
 (1,1) (2,0) (1,0) and  (0,1) (1,0) (0,0) overlap and touch on  (1,0)

0 0 0 1 1 0 0 1 0 2 1 1
Triangle: ABC:  (0,0) (0,1) (1,0)
Triangle: DEF:  (0,1) (0,2) (1,1)
Corner(s) that touch the other triangle:  (0,1)
Triangles touch on  (0,1)
  we analyze further
 (0,0) (0,1) (1,0) and  (0,1) (0,2) (1,1) don't overlap but touch on  (0,1)

0 1 0 2 1 1 0 0 0 1 1 0
Triangle: ABC:  (0,1) (0,2) (1,1)
Triangle: DEF:  (0,0) (0,1) (1,0)
Corner(s) that touch the other triangle:  (0,1)
Triangles touch on  (0,1)
  we analyze further
 (0,1) (0,2) (1,1) and  (0,0) (0,1) (1,0) don't overlap but touch on  (0,1)

0 0 0 1 1 0 0 1 0 2 1 1
Triangle: ABC:  (0,0) (0,1) (1,0)
Triangle: DEF:  (0,1) (0,2) (1,1)
Corner(s) that touch the other triangle:  (0,1)
Triangles touch on  (0,1)
  we analyze further
 (0,0) (0,1) (1,0) and  (0,1) (0,2) (1,1) don't overlap but touch on  (0,1)
```



## Perl


### Port of Lua


```perl
use strict;
use warnings;

sub det2D {
    my $p1 = shift or die "4 Missing first point\n";
    my $p2 = shift or die "Missing second point\n";
    my $p3 = shift or die "Missing third point\n";

    return $p1->{x} * ($p2->{y} - $p3->{y})
         + $p2->{x} * ($p3->{y} - $p1->{y})
         + $p3->{x} * ($p1->{y} - $p2->{y});
}

sub checkTriWinding {
    my $p1 = shift or die "14 Missing first point\n";
    my $p2 = shift or die "Missing second point\n";
    my $p3 = shift or die "Missing third point\n";
    my $allowReversed = shift;

    my $detTri = det2D($p1, $$p2, $$p3);
    if ($detTri < 0.0) {
        if ($allowReversed) {
            my $t = $$p3;
            $$p3 = $$p2;
            $$p2 = $t;
        } else {
            die "triangle has wrong winding direction";
        }
    }
    return undef;
}

sub boundaryCollideChk {
    my $p1 = shift or die "33 Missing first point\n";
    my $p2 = shift or die "Missing second point\n";
    my $p3 = shift or die "Missing third point\n";
    my $eps = shift;

    return det2D($p1, $p2, $p3) < $eps;
}

sub boundaryDoesntCollideChk {
    my $p1 = shift or die "42 Missing first point\n";
    my $p2 = shift or die "Missing second point\n";
    my $p3 = shift or die "Missing third point\n";
    my $eps = shift;

    return det2D($p1, $p2, $p3) <= $eps;
}

sub triTri2D {
    my $t1 = shift or die "Missing first triangle to calculate with\n";
    my $t2 = shift or die "Missing second triangle to calculate with\n";
    my $eps = shift;
    my $allowReversed = shift;
    my $onBoundary = shift;

    # triangles must be expressed anti-clockwise
    checkTriWinding($t1->[0], \$t1->[1], \$t1->[2], $allowReversed);
    checkTriWinding($t2->[0], \$t2->[1], \$t2->[2], $allowReversed);

    my $chkEdge;
    if ($onBoundary) {
        # points on the boundary are considered as colliding
        $chkEdge = \&boundaryCollideChk;
    } else {
        # points on the boundary are NOT considered as colliding
        $chkEdge = \&boundaryDoesntCollideChk;
    }

    # for edge E of triangle 1
    foreach my $i (0, 1, 2) {
        my $j = ($i + 1) % 3;

        # check all points of triangle 2 lay on the external side of edge E
        # if they do, the triangles do not collide
        if ($chkEdge->($t1->[$i], $t1->[$j], $t2->[0], $eps)
        and $chkEdge->($t1->[$i], $t1->[$j], $t2->[1], $eps)
        and $chkEdge->($t1->[$i], $t1->[$j], $t2->[2], $eps)) {
            return 0; # false
        }
    }

    # for edge E of triangle 2
    foreach my $i (0, 1, 2) {
        my $j = ($i + 1) % 3;

        # check all points of triangle 1 lay on the external side of edge E
        # if they do, the triangles do not collide
        if ($chkEdge->($t2->[$i], $t2->[$j], $t1->[0], $eps)
        and $chkEdge->($t2->[$i], $t2->[$j], $t1->[1], $eps)
        and $chkEdge->($t2->[$i], $t2->[$j], $t1->[2], $eps)) {
            return 0; # false
        }
    }

    return 1; # true
}

sub formatTri {
    my $t = shift or die "Missing triangle to format\n";
    my $p1 = $t->[0];
    my $p2 = $t->[1];
    my $p3 = $t->[2];
    return "Triangle: ($p1->{x}, $p1->{y}), ($p2->{x}, $p2->{y}), ($p3->{x}, $p3->{y})";
}

sub overlap {
    my $t1 = shift or die "Missing first triangle to calculate with\n";
    my $t2 = shift or die "Missing second triangle to calculate with\n";
    my $eps = shift;
    my $allowReversed = shift or 0; # false
    my $onBoundary = shift or 1; # true

    unless ($eps) {
        $eps = 0.0;
    }

    if (triTri2D($t1, $t2, $eps, $allowReversed, $onBoundary)) {
        return "overlap\n";
    } else {
        return "do not overlap\n";
    }
}

###################################################
# Main
###################################################

my @t1 = ({x=>0, y=>0}, {x=>5, y=>0}, {x=>0, y=>5});
my @t2 = ({x=>0, y=>0}, {x=>5, y=>0}, {x=>0, y=>6});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2), "\n";

@t1 = ({x=>0, y=>0}, {x=>0, y=>5}, {x=>5, y=>0});
@t2 = ({x=>0, y=>0}, {x=>0, y=>5}, {x=>5, y=>0});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2, 0.0, 1), "\n";

@t1 = ({x=>0, y=>0}, {x=>5, y=>0}, {x=>0, y=>5});
@t2 = ({x=>-10, y=>0}, {x=>-5, y=>0}, {x=>-1, y=>6});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2), "\n";

@t1 = ({x=>0, y=>0}, {x=>5, y=>0}, {x=>2.5, y=>5});
@t2 = ({x=>0, y=>4}, {x=>2.5, y=>-1}, {x=>5, y=>4});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2), "\n";

@t1 = ({x=>0, y=>0}, {x=>1, y=>1}, {x=>0, y=>2});
@t2 = ({x=>2, y=>1}, {x=>3, y=>0}, {x=>3, y=>2});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2), "\n";

@t1 = ({x=>0, y=>0}, {x=>1, y=>1}, {x=>0, y=>2});
@t2 = ({x=>2, y=>1}, {x=>3, y=>-2}, {x=>3, y=>4});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2), "\n";

# Barely touching
@t1 = ({x=>0, y=>0}, {x=>1, y=>0}, {x=>0, y=>1});
@t2 = ({x=>1, y=>0}, {x=>2, y=>0}, {x=>1, y=>1});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2, 0.0, 0, 1), "\n";

# Barely touching
@t1 = ({x=>0, y=>0}, {x=>1, y=>0}, {x=>0, y=>1});
@t2 = ({x=>1, y=>0}, {x=>2, y=>0}, {x=>1, y=>1});
print formatTri(\@t1), " and\n", formatTri(\@t2), "\n", overlap(\@t1, \@t2, 0.0, 0, 0), "\n";
```

{{out}}

```txt
Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (0, 0), (5, 0), (0, 6)
overlap

Triangle: (0, 0), (0, 5), (5, 0) and
Triangle: (0, 0), (0, 5), (5, 0)
overlap

Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (-10, 0), (-5, 0), (-1, 6)
do not overlap

Triangle: (0, 0), (5, 0), (2.5, 5) and
Triangle: (0, 4), (2.5, -1), (5, 4)
overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, 0), (3, 2)
do not overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, -2), (3, 4)
do not overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1)
overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1)
do not overlap
```



### More Idiomatic


```perl
use strict;
use warnings;
use feature 'say';

sub det2D {
    my($p1,$p2,$p3) = @_;
    return $p1->[0] * ($p2->[1] - $p3->[1])
         + $p2->[0] * ($p3->[1] - $p1->[1])
         + $p3->[0] * ($p1->[1] - $p2->[1]);
}

# triangles must be expressed anti-clockwise
sub checkTriWinding {
    my($p1,$p2,$p3,$allowReversed) = @_;
    my $detTri = det2D($p1, $$p2, $$p3);
    if ($detTri < 0.0) {
        if ($allowReversed) { ($$p3,$$p2) = ($$p2,$$p3) }
        else                { die "triangle has wrong winding direction" }
    }
    return undef;
}

sub check_edge {
    our($t1,$t2,$eps,$onBoundary) = @_;

    # points on the boundary may be considered as colliding, or not
    my $chkEdge = $onBoundary ?  \&boundaryCollideChk : \&boundaryDoesntCollideChk;
    sub boundaryCollideChk       { return det2D($_[0], $_[1], $_[2]) <  $eps }
    sub boundaryDoesntCollideChk { return det2D($_[0], $_[1], $_[2]) <= $eps }

    # for edge E of triangle 1
    foreach my $i (0, 1, 2) {
        my $j = ($i + 1) % 3;

        # check all points of triangle 2 lay on the external side of edge E
        # if they do, the triangles do not collide
        if ($chkEdge->($$t1->[$i], $$t1->[$j], $$t2->[0], $eps)
        and $chkEdge->($$t1->[$i], $$t1->[$j], $$t2->[1], $eps)
        and $chkEdge->($$t1->[$i], $$t1->[$j], $$t2->[2], $eps)) {
            return 0; # false
        }
    }
    return 1;
}

sub triTri2D {
    my($t1,$t2,$eps,$allowReversed,$onBoundary) = @_;
    checkTriWinding($$t1->[0], \$$t1->[1], \$$t1->[2], $allowReversed);
    checkTriWinding($$t2->[0], \$$t2->[1], \$$t2->[2], $allowReversed);
    return check_edge($t1,$t2,$eps,$onBoundary) && check_edge($t2,$t1,$eps,$onBoundary);
}

sub formatTri {
    my $t = shift;
    my @pairs;
    push @pairs, sprintf "%8s", '(' . $$_[0] . ',' . $$_[1] . ')' for @$$t;
    join ', ', @pairs;
}

sub overlap {
    my $t1            = shift or die "Missing first triangle to calculate with\n";
    my $t2            = shift or die "Missing second triangle to calculate with\n";
    my $eps           = shift || 0;
    my $allowReversed = shift || 1;
    my $onBoundary    = shift || 1;

    my $triangles = formatTri($t1) . ' and ' . formatTri($t2);
    if (triTri2D($t1, $t2, $eps, $allowReversed, $onBoundary)) {
        return "       overlap:" . $triangles;
    } else {
        return "do not overlap:" . $triangles;
    }
}

my @tests = (
   [ [[0,0], [5,0],   [0,5]], [   [0,0],    [5,0],  [0,6]] ],
   [ [[0,0], [0,5],   [5,0]], [   [0,0],    [0,5],  [5,0]] ],
   [ [[0,0], [5,0],   [0,5]], [ [-10,0],   [-5,0], [-1,6]] ],
   [ [[0,0], [5,0], [2.5,5]], [   [0,4], [2.5,-1],  [5,4]] ],
   [ [[0,0], [1,1],   [0,2]], [   [2,1],    [3,0],  [3,2]] ],
   [ [[0,0], [1,1],   [0,2]], [   [2,1],   [3,-2],  [3,4]] ],             # barely touching
   [ [[0,0], [1,0],   [0,1]], [   [1,0],    [2,0],  [1,1]], 0.0, 0, 0 ]   # barely touching
);

say overlap(\$_->[0], \$_->[1], $_->[2], $_->[3], $_->[4]) for @tests;
```

{{out}}

```txt
       overlap:   (0,0),    (5,0),    (0,5) and    (0,0),    (5,0),    (0,6)
       overlap:   (0,0),    (0,5),    (5,0) and    (0,0),    (0,5),    (5,0)
do not overlap:   (0,0),    (5,0),    (0,5) and  (-10,0),   (-5,0),   (-1,6)
       overlap:   (0,0),    (5,0),  (2.5,5) and    (0,4), (2.5,-1),    (5,4)
do not overlap:   (0,0),    (1,1),    (0,2) and    (2,1),    (3,0),    (3,2)
do not overlap:   (0,0),    (1,1),    (0,2) and    (2,1),   (3,-2),    (3,4)
       overlap:   (0,0),    (1,0),    (0,1) and    (1,0),    (2,0),    (1,1)
```



## Perl 6

First, check if any vertex is inside each other triangle (that should cover most overlapping cases including enclosures).  Then see if an edge of triangle A intersects any of two edges of B (for shapes like Star of David [https://en.wikipedia.org/wiki/Star_of_David])

```perl6
#!/usr/bin/env perl6

# Reference:
# https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
# https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/

use v6;

sub if-overlap ($triangle-pair) {
   my (\A,\B) = $triangle-pair;
   my Bool $result = False;

   sub sign (\T) {
      return (T.[0][0] - T[2][0]) * (T[1][1] - T[2][1]) -
         (T[1][0] - T[2][0]) * (T[0][1] - T[2][1]);
   }

   sub point-in-triangle (\pt, \Y --> Bool) {
      my ($d1, $d2, $d3);
      my Bool ($has_neg, $has_pos);

      $d1 = sign (pt, Y.[0], Y.[1]);
      $d2 = sign (pt, Y.[1], Y.[2]);
      $d3 = sign (pt, Y.[2], Y.[0]);

      $has_neg = ($d1 < 0) || ($d2 < 0) || ($d3 < 0);
      $has_pos = ($d1 > 0) || ($d2 > 0) || ($d3 > 0);

      return !($has_neg && $has_pos);
   }

   sub orientation(\P, \Q, \R --> Int) {
      my \val = (Q.[1] - P.[1]) * (R.[0] - Q.[0]) -
         (Q.[0] - P.[0]) * (R.[1] - Q.[1]);

      return 0 if val == 0;  # colinear

      return (val > 0) ?? 1 !! 2; # clock or counterclock wise
   }

   sub onSegment(\P, \Q, \R --> Bool) {
      Q.[0] <= max(P.[0], R.[0]) && Q.[0] >= min(P.[0], R.[0]) &&
         Q.[1] <= max(P.[1], R.[1]) && Q.[1] >= min(P.[0], R.[1])
         ?? True !! False;
   }

   sub intersect(\A,\B,\C,\D --> Bool) {
      my \o1 = orientation A,C,D;
      my \o2 = orientation B,C,D;
      my \o3 = orientation A,B,C;
      my \o4 = orientation A,B,D;

      return True if o1 != o2 && o3 != o4;

      return True if (o1 == 0 && onSegment(A, C, D)) ;
      return True if (o2 == 0 && onSegment(B, C, D)) ;
      return True if (o3 == 0 && onSegment(A, B, C)) ;
      return True if (o4 == 0 && onSegment(A, B, D)) ;

      return False;
   }

   for ^3 {
      {$result = True; last } if point-in-triangle A.[$^i] , B or
          point-in-triangle B.[$^i] , A ;
   }

   unless $result {
      $result = True if intersect A.[0], A.[1], B.[0], B.[1] or
          intersect A.[0], A.[1], B.[0], B.[2]
   }

   say A, " and ", B, " do",  $result ?? "" !! " NOT" , " overlap.";
}

my \DATA = [
   [ [(0,0),(5,0),(0,5)]   ,  [(0,0),(5,0),(0,6)]     ],
   [ [(0,0),(0,5),(5,0)]   ,  [(0,0),(0,5),(5,0)]     ],
   [ [(0,0),(5,0),(0,5)]   ,  [(-10,0),(-5,0),(-1,6)] ],
   [ [(0,0),(5,0),(2.5,5)] ,  [ (0,4),(2.5,-1),(5,4)] ],
   [ [(0,0),(1,1),(0,2)]   ,  [(2,1),(3,0),(3,2)]     ],
   [ [(0,0),(1,1),(0,2)]   ,  [(2,1),(3,-2),(3,4)]    ],
   [ [(0,0),(1,0),(0,1)]   ,  [(1,0),(2,0),(1,1)]     ]
];

if-overlap $_ for DATA ;
```

{{out}}
```txt
[(0 0) (5 0) (0 5)] and [(0 0) (5 0) (0 6)] do overlap.
[(0 0) (0 5) (5 0)] and [(0 0) (0 5) (5 0)] do overlap.
[(0 0) (5 0) (0 5)] and [(-10 0) (-5 0) (-1 6)] do NOT overlap.
[(0 0) (5 0) (2.5 5)] and [(0 4) (2.5 -1) (5 4)] do overlap.
[(0 0) (1 1) (0 2)] and [(2 1) (3 0) (3 2)] do NOT overlap.
[(0 0) (1 1) (0 2)] and [(2 1) (3 -2) (3 4)] do NOT overlap.
[(0 0) (1 0) (0 1)] and [(1 0) (2 0) (1 1)] do overlap.

```



## Phix

{{libheader|pGUI}}
{{trans|zkl}}
Plus draw all eight pairs of triangles for visual confirmation.

```Phix
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

constant triangles = {{{{0,0},{5,0},{0,5}},{{0,0},{5,0},{0,6}}},
                      {{{0,0},{0,5},{5,0}},{{0,0},{0,5},{5,0}}},
                      {{{0,0},{5,0},{0,5}},{{-10,0},{-5,0},{-1,6}}},
                      {{{0,0},{5,0},{2.5,5}},{{0,4},{2.5,-1},{5,4}}},
                      {{{0,0},{1,1},{0,2}},{{2,1},{3,0},{3,2}}},
                      {{{0,0},{1,1},{0,2}},{{2,1},{3,-2},{3,4}}},
                      {{{0,0},{1,0},{0,1}},{{1,0},{2,0},{1,1}}},
                      {{{0,0},{1,0},{0,1}},{{1,0},{2,0},{1,1}}}}

procedure draw_triangle(sequence t, integer cx,cy, c)
    cdCanvasSetForeground(cddbuffer, c)
    cdCanvasBegin(cddbuffer,CD_CLOSED_LINES)
    for c=1 to 3 do
        atom {x,y} = t[c]
        cdCanvasVertex(cddbuffer, cx+x*10, cy+y*10)
    end for
    cdCanvasEnd(cddbuffer)
end procedure

function det2D(sequence triangle)
   atom {{p1x,p1y},{p2x,p2y},{p3x,p3y}} := triangle
   return p1x*(p2y-p3y) + p2x*(p3y-p1y) + p3x*(p1y-p2y)
end function

bool bReversed
function checkWinding(sequence triangle, bool allowReversed)
    atom detTri := det2D(triangle);
    if detTri<0.0 then
        if allowReversed then
            bReversed = true
            triangle = extract(triangle,{1,3,2})
        else
            throw("triangle has wrong winding direction")
        end if
    end if
    return triangle
end function

function overlap(sequence t1, t2, atom epsilon=0.0, bool allowReversed=false, onBoundary=true)
    -- Trangles must be expressed anti-clockwise
    bReversed = false
    t1 = checkWinding(t1, allowReversed)
    t2 = checkWinding(t2, allowReversed)

    for t=1 to 2 do                     -- check t1 then t2
        for edge=1 to 3 do              -- check each edge
            sequence p1 = t1[edge],
                     p2 = t1[mod(edge,3)+1]
            -- Check all points of trangle 2 lay on the external side
            -- of the edge E. If they do, the triangles do not collide.
            integer onside = 0
            for k=1 to 3 do
                integer c = compare(det2D({p1,p2,t2[k]}),epsilon)
                if onBoundary then
                    if not (c<0) then exit end if
                else
                    if not (c<=0) then exit end if
                end if
--              -- (the following incomprehensible one-liner is equivalent:)
--              if compare(det2D({p1,p2,t2[k]}),epsilon)>-onBoundary then exit end if
                onside += 1
            end for
            if onside=3 then
                return iff(onBoundary?"no overlap":"no overlap (no boundary)")
            end if
        end for
        {t2,t1} = {t1,t2}           -- flip and re-test
    end for
    return iff(bReversed?"overlap (reversed)":"overlap")
end function

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    integer cy = 200, cx = 100
    for i=1 to length(triangles) do
        sequence {t1,t2} = triangles[i]
        draw_triangle(t1,cx,cy,CD_RED)
        integer s = (i<=2)  -- (smudge tests[1..2] by one
                            --  pixel to show them better)
        draw_triangle(t2,cx+s,cy+s,CD_BLUE)
        cdCanvasSetForeground(cddbuffer, CD_BLACK)
        cdCanvasText(cddbuffer,cx+10,cy-40,overlap(t1,t2,0,i=2,i!=8))
        if i=4 then
            cy = 100
            cx = 100
        else
            cx += 300
        end if
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "1250x300")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas,"DIALOGFRAME=YES")
    IupSetAttribute(dlg, "TITLE", "Triangle overlap")
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    IupCloseOnEscape(dlg)

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## Python

Using numpy:


```python
from __future__ import print_function
import numpy as np

def CheckTriWinding(tri, allowReversed):
	trisq = np.ones((3,3))
	trisq[:,0:2] = np.array(tri)
	detTri = np.linalg.det(trisq)
	if detTri < 0.0:
		if allowReversed:
			a = trisq[2,:].copy()
			trisq[2,:] = trisq[1,:]
			trisq[1,:] = a
		else: raise ValueError("triangle has wrong winding direction")
	return trisq

def TriTri2D(t1, t2, eps = 0.0, allowReversed = False, onBoundary = True):
	#Trangles must be expressed anti-clockwise
	t1s = CheckTriWinding(t1, allowReversed)
	t2s = CheckTriWinding(t2, allowReversed)

	if onBoundary:
		#Points on the boundary are considered as colliding
		chkEdge = lambda x: np.linalg.det(x) < eps
	else:
		#Points on the boundary are not considered as colliding
		chkEdge = lambda x: np.linalg.det(x) <= eps

	#For edge E of trangle 1,
	for i in range(3):
		edge = np.roll(t1s, i, axis=0)[:2,:]

		#Check all points of trangle 2 lay on the external side of the edge E. If
		#they do, the triangles do not collide.
		if (chkEdge(np.vstack((edge, t2s[0]))) and
			chkEdge(np.vstack((edge, t2s[1]))) and
			chkEdge(np.vstack((edge, t2s[2])))):
			return False

	#For edge E of trangle 2,
	for i in range(3):
		edge = np.roll(t2s, i, axis=0)[:2,:]

		#Check all points of trangle 1 lay on the external side of the edge E. If
		#they do, the triangles do not collide.
		if (chkEdge(np.vstack((edge, t1s[0]))) and
			chkEdge(np.vstack((edge, t1s[1]))) and
			chkEdge(np.vstack((edge, t1s[2])))):
			return False

	#The triangles collide
	return True

if __name__=="__main__":
	t1 = [[0,0],[5,0],[0,5]]
	t2 = [[0,0],[5,0],[0,6]]
	print (TriTri2D(t1, t2), True)

	t1 = [[0,0],[0,5],[5,0]]
	t2 = [[0,0],[0,6],[5,0]]
	print (TriTri2D(t1, t2, allowReversed = True), True)

	t1 = [[0,0],[5,0],[0,5]]
	t2 = [[-10,0],[-5,0],[-1,6]]
	print (TriTri2D(t1, t2), False)

	t1 = [[0,0],[5,0],[2.5,5]]
	t2 = [[0,4],[2.5,-1],[5,4]]
	print (TriTri2D(t1, t2), True)

	t1 = [[0,0],[1,1],[0,2]]
	t2 = [[2,1],[3,0],[3,2]]
	print (TriTri2D(t1, t2), False)

	t1 = [[0,0],[1,1],[0,2]]
	t2 = [[2,1],[3,-2],[3,4]]
	print (TriTri2D(t1, t2), False)

	#Barely touching
	t1 = [[0,0],[1,0],[0,1]]
	t2 = [[1,0],[2,0],[1,1]]
	print (TriTri2D(t1, t2, onBoundary = True), True)

	#Barely touching
	t1 = [[0,0],[1,0],[0,1]]
	t2 = [[1,0],[2,0],[1,1]]
	print (TriTri2D(t1, t2, onBoundary = False), False)
```


{{out}}

```txt
True True
True True
False False
True True
False False
False False
True True
/False False
```


Using shapely:


```python
from __future__ import print_function
from shapely.geometry import Polygon

def PolyOverlaps(poly1, poly2):
	poly1s = Polygon(poly1)
	poly2s = Polygon(poly2)
	return poly1s.intersects(poly2s)

if __name__=="__main__":
	t1 = [[0,0],[5,0],[0,5]]
	t2 = [[0,0],[5,0],[0,6]]
	print (PolyOverlaps(t1, t2), True)

	t1 = [[0,0],[0,5],[5,0]]
	t2 = [[0,0],[0,6],[5,0]]
	print (PolyOverlaps(t1, t2), True)

	t1 = [[0,0],[5,0],[0,5]]
	t2 = [[-10,0],[-5,0],[-1,6]]
	print (PolyOverlaps(t1, t2), False)

	t1 = [[0,0],[5,0],[2.5,5]]
	t2 = [[0,4],[2.5,-1],[5,4]]
	print (PolyOverlaps(t1, t2), True)

	t1 = [[0,0],[1,1],[0,2]]
	t2 = [[2,1],[3,0],[3,2]]
	print (PolyOverlaps(t1, t2), False)

	t1 = [[0,0],[1,1],[0,2]]
	t2 = [[2,1],[3,-2],[3,4]]
	print (PolyOverlaps(t1, t2), False)

	#Barely touching
	t1 = [[0,0],[1,0],[0,1]]
	t2 = [[1,0],[2,0],[1,1]]
	print (PolyOverlaps(t1, t2), "?")
```


{{out}}

```txt
True True
True True
False False
True True
False False
False False
True ?
```



## QB64


```QB64

DATA 0,0,5,0,0,5,0,0,5,0,0,6
DATA 0,0,0,5,5,0,0,0,0,5,5,0
DATA 0,0,5,0,0,5,-10,0,-5,0,-1,6
DATA 0,0,5,0,2.5,5,0,4,2.5,-1,5,4
DATA 0,0,1,1,0,2,2,1,3,0,3,2
DATA 0,0,1,1,0,2,2,1,3,-2,3,4

TYPE point
    x AS INTEGER
    y AS INTEGER
END TYPE

DIM coord(12, 3) AS point

workscreen = _NEWIMAGE(800, 800, 32)
backscreen = _NEWIMAGE(800, 800, 32)

SCREEN workscreen
FOR i = 1 TO 12 '12 triangles
    FOR j = 1 TO 3 'with 3 coordinates for each
        READ coord(i, j).x 'X coord
        READ coord(i, j).y 'Y coord
        FixCoord coord(i, j)
    NEXT

NEXT
_DELAY .5
_SCREENMOVE _MIDDLE


FOR i = 1 TO 12
    _DEST workscreen
    CLS
    _DEST backscreen
    _DONTBLEND
    CLS , 0
    PSET (coord(i, 1).x, coord(i, 1).y), _RGBA32(255, 255, 255, 128)
    FOR j = 2 TO 3
        LINE -(coord(i, j).x, coord(i, j).y), _RGBA32(255, 255, 255, 128)
    NEXT
    LINE -(coord(i, 1).x, coord(i, 1).y), _RGBA32(255, 255, 255, 128)
    xinside = (coord(i, 1).x + coord(i, 2).x + coord(i, 3).x) / 3
    yinside = (coord(i, 1).y + coord(i, 2).y + coord(i, 3).y) / 3
    PAINT (xinside, yinside), _RGBA32(255, 255, 255, 128)
    _BLEND
    _PUTIMAGE , backscreen, 0
    CLS , 0
    _DONTBLEND
    i = i + 1
    PSET (coord(i, 1).x, coord(i, 1).y), _RGBA32(255, 0, 0, 128)
    FOR j = 2 TO 3
        LINE -(coord(i, j).x, coord(i, j).y), _RGBA32(255, 0, 0, 128)
    NEXT
    LINE -(coord(i, 1).x, coord(i, 1).y), _RGBA32(255, 0, 0, 128)
    xinside = (coord(i, 1).x + coord(i, 2).x + coord(i, 3).x) / 3
    yinside = (coord(i, 1).y + coord(i, 2).y + coord(i, 3).y) / 3
    PAINT (xinside, yinside), _RGBA32(255, 0, 0, 128)
    _BLEND
    _PUTIMAGE , backscreen, 0
    _DEST workscreen
    _SOURCE workscreen
    overlap = 0
    FOR x = 0 TO 999
        FOR y = 0 TO 999
            IF POINT(x, y) = _RGBA32(190, 63, 63, 255) THEN overlap = -1: GOTO overlap
        NEXT
    NEXT
    overlap:
    IF overlap THEN PRINT "OVERLAP" ELSE PRINT "NO OVERLAP"
    SLEEP
NEXT
SYSTEM

SUB FixCoord (p AS point)
    p.x = (10 + p.x) * 30 + 100
    p.y = (10 + p.y) * 30
END SUB

```



## Racket

{{trans|Zkl}}


```racket
#lang racket

;; A triangle is a list of three pairs of points: '((x . y) (x . y) (x . y))
(define (to-tri x1 y1 x2 y2 x3 y3) `((,x1 . ,y1) (,x2 . ,y2) (,x3 . ,y3)))

(define det-2D
  (match-lambda
    [`((,x1 . ,y1) (,x2 . ,y2) (,x3 . ,y3)) (+ (* x1 (- y2 y3)) (* x2 (- y3 y1)) (* x3 (- y1 y2)))]))

(define (assert-triangle-winding triangle allow-reversed?)
  (cond
    [(>= (det-2D triangle) 0) triangle]
    [allow-reversed? (match triangle [(list p1 p2 p3) (list p1 p3 p2)])]
    [else (error 'assert-triangle-winding "triangle is wound in wrong direction")]))

(define (tri-tri-2d? triangle1 triangle2
                     #:ϵ (ϵ 0)
                     #:allow-reversed? (allow-reversed? #f)
                     #:on-boundary? (on-boundary? #t))
  (define check-edge
    (if on-boundary? ; Points on the boundary are considered as colliding
        (λ (triangle) (< (det-2D triangle) ϵ))
        (λ (triangle) (<= (det-2D triangle) ϵ))))

  (define (inr t1 t2)
    (for*/and ((i (in-range 3)))
      ;; Check all points of trangle 2 lay on the external side
      ;; of the edge E. If they do, the triangles do not collide.
      (define t1.i (list-ref t1 i))
      (define t1.j (list-ref t1 (modulo (add1 i) 3)))
      (not (for/and ((k (in-range 3))) (check-edge (list (list-ref t2 k) t1.i t1.j))))))

  (let (;; Trangles must be expressed anti-clockwise
        (tri1 (assert-triangle-winding triangle1 allow-reversed?))
        (tri2 (assert-triangle-winding triangle2 allow-reversed?)))
    (and (inr tri1 tri2) (inr tri2 tri1))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)

  (define triangleses ; pairs of triangles
    (for/list ((a.b (in-list '(((0 0  5 0  0   5) (  0 0   5    0   0 6))
                               ((0 0  0 5  5   0) (  0 0   0    5   5 0))
                               ((0 0  5 0  0   5) (-10 0  -5    0  -1 6))
                               ((0 0  5 0  2.5 5) (  0 4   2.5 -1   5 4))
                               ((0 0  1 1  0   2) (  2 1   3    0   3 2))
                               ((0 0  1 1  0   2) (  2 1   3   -2   3 4))))))
      (map (curry apply to-tri) a.b)))

  (check-equal?
   (for/list ((t1.t2 (in-list triangleses)))
     (define t1 (first t1.t2))
     (define t2 (second t1.t2))
     (define-values (r reversed?)
       (with-handlers ([exn:fail? (λ (_) (values (tri-tri-2d? t1 t2 #:allow-reversed? #t) #t))])
         (values (tri-tri-2d? t1 t2) #f)))
     (cons r reversed?))
   '((#t . #f) (#t . #t) (#f . #f) (#t . #f) (#f . #f) (#f . #f)))

  (let ((c1 (to-tri 0 0  1 0  0 1)) (c2 (to-tri 1 0  2 0  1 1)))
    (check-true (tri-tri-2d? c1 c2 #:on-boundary? #t))
    (check-false (tri-tri-2d? c1 c2 #:on-boundary? #f))))

```


No output &rarr; all tests passed


## REXX

Note: The triangles must be real triangles (no edge of length 0)

```rexx
/* REXX */
Signal On Halt
Signal On Novalue
Signal On Syntax

fid='trio.in'
oid='trio.txt'; 'erase' oid


Call trio_test '0 0   5 0   0 5   0 0   5  0   0 6'
Call trio_test '0 0   0 5   5 0   0 0   0  5   5 0'
Call trio_test '0 0   5 0   0 5 -10 0  -5  0  -1 6'
Call trio_test '0 0   5 0 2.5 5   0 4 2.5 -1   5 4'
Call trio_test '0 0   1 1   0 2   2 1   3  0   3 2'
Call trio_test '0 0   1 1   0 2   2 1   3 -2   3 4'
Call trio_test '0 0   1 0   0 1   1 0   2  0   1 1'

Call trio_test '1 0   3 0   2 2   1 3   3  3   2 5'
Call trio_test '1 0   3 0   2 2   1 3   3  3   2 2'
Call trio_test '0 0   2 0   2 2   3 3   5  3   5 5'
Call trio_test '2 0   2 6   1 8   0 1   0  5   8 3'
Call trio_test '0 0   4 0   0 4   0 2   2  0   2 2'
Call trio_test '0 0   4 0   0 4   1 1   2  1   1 2'
Exit

trio_test:
parse Arg tlist
tlist=space(tlist)
Parse Arg ax ay bx by cx cy dx dy ex ey fx fy

Say 'ABC:' show_p(ax ay) show_p(bx by) show_p(cx cy)
Say 'DEF:' show_p(dx dy) show_p(ex ey) show_p(fx fy)

bordl=bord(tlist)  /* corners that are on the other triangle's edges */
If bordl<>'' Then
  Say 'Corners on the other triangle''s edges:' bordl
wb=words(bordl)                     /* how many of them?             */
Select
  When wb=3 Then Do                 /* all three match               */
    If ident(ax ay,bx by,cx cy,dx dy,ex ey,fx fy) Then
      Say 'Triangles are identical'
    Else
      Say 'Triangles overlap'
    Say ''
    Return
    End
  When wb=2 Then Do                 /* two of them match             */
    Say 'Triangles overlap'
    Say '  they have a common edge 'bordl
    Say ''
    Return
    End
  When wb=1 Then Do                 /* one of them match             */
    Say 'Triangles touch on' bordl  /* other parts may overlap       */
    Say '  we analyze further'
    End
  Otherwise                         /* we know nothing yet           */
    Nop
  End

trio_result=trio(tlist)             /* any other overlap?            */

Select
  When trio_result=0 Then Do        /* none whatsoever               */
    If wb=1 Then
      Say 'Triangles touch (border case) at' show_p(bordl)
    Else
      Say 'Triangles don''t overlap'
    End
  When trio_result>0 Then           /* plain overlapping case        */
    Say 'Triangles overlap'
  End
Say ''
Return

trio:
/*---------------------------------------------------------------------
* Determine if two triangles overlap
*--------------------------------------------------------------------*/
parse Arg tlist
Parse Arg pax pay pbx pby pcx pcy pdx pdy pex pey pfx pfy

abc=subword(tlist,1,6)
def=subword(tlist,7,6)

Do i=1 To 3
  s.i=subword(abc abc,i*2-1,4)
  t.i=subword(def def,i*2-1,4)
  End

abc_=''
def_=''

Do i=1 To 3
  abc.i=subword(abc,i*2-1,2)   /* corners of ABC */
  def.i=subword(def,i*2-1,2)   /* corners of DEF */
  Parse Var abc.i x y; abc_=abc_ '('||x','y')'
  Parse Var def.i x y; def_=def_ '('||x','y')'
  End
Call o 'abc_='abc_
Call o 'def_='def_

over=0
  Do i=1 To 3 Until over
    Do j=1 To 3 Until over
      If ssx(s.i t.j) Then Do       /* intersection of two edges     */
        over=1
        Leave
        End
      End
    End

If over=0 Then Do                   /* no edge intersection found    */
  Do ii=1 To 3 Until over           /* look for first possibility    */
    Call o '    '  'abc.'ii'='abc.ii 'def='def
    Call o 'ii='ii 'def.'ii'='def.ii 'abc='abc
    If in_tri(abc.ii,def) Then Do   /* a corner of ABC is in DEF     */
      Say abc.ii 'is within' def
      over=1
      End
    Else If in_tri(def.ii,abc) Then Do  /* a corner of DEF is in ABC */
      Say def.ii 'is within' abc
      over=1
      End
    End
  End

If over=0 Then rw='don''t '
Else rw=''

Call o 'Triangles' show_p(pax pay) show_p(pbx pby) show_p(pcx pcy),
             'and' show_p(pdx pdy) show_p(pex pey) show_p(pfx pfy),
                                                            rw'overlap'
Call o ''
Return over

ssx: Procedure Expose oid bordl
/*---------------------------------------------------------------------
* Intersection of 2 line segments A-B and C-D
*--------------------------------------------------------------------*/
Parse Arg xa ya xb yb xc yc xd yd

d=ggx(xa ya xb yb xc yc xd yd)

Call o 'ssx:' arg(1) d
res=0
Select
  When d='-' Then res=0
  When d='I' Then Do
    If xa<>xb Then Do
      xab_min=min(xa,xb)
      xcd_min=min(xc,xd)
      xab_max=max(xa,xb)
      xcd_max=max(xc,xd)
      If xab_min>xcd_max |,
         xcd_min>xab_max Then
        res=0
      Else Do
        res=1
        Select
          When xa=xc & isb(xc,xb,xd)=0 Then Do; x=xa; y=ya; End
          When xb=xc & isb(xc,xa,xd)=0 Then Do; x=xb; y=yb; End
          When xa=xd & isb(xc,xb,xd)=0 Then Do; x=xa; y=ya; End
          When xb=xd & isb(xc,xa,xd)=0 Then Do; x=xb; y=yb; End
          Otherwise Do
            x='*'
            y=ya
            End
          End
        Call o  'ssx:' x y
        End
      End
    Else Do
      yab_min=min(ya,yb)
      ycd_min=min(yc,yd)
      yab_max=max(ya,yb)
      ycd_max=max(yc,yd)
      If yab_min>ycd_max |,
         ycd_min>yab_max Then
        res=0
      Else Do
        res=1
        x=xa
        y='*'
        Parse Var bordl x_bord '/' y_bord
        If x=x_bord Then Do
          Call o  xa'/* IGNORED'
          res=0
          End
        End
      End
    End
  Otherwise Do
    Parse Var d x y
    If is_between(xa,x,xb) &,
       is_between(xc,x,xd) &,
       is_between(ya,y,yb) &,
       is_between(yc,y,yd) Then Do
      If x'/'y<>bordl Then
        res=1
      End
    End
  End
  If res=1 Then Do
    Say 'Intersection of line segments: ('||x'/'y')'
    Parse Var bordl x_bord '/' y_bord
    If x=x_bord Then Do
      res=0
      Call o x'/'y 'IGNORED'
      End
    End
  Else Call o  'ssx: -'
Return res

ggx: Procedure Expose oid bordl
/*---------------------------------------------------------------------
* Intersection of 2 (straight) lines
*--------------------------------------------------------------------*/
Parse Arg xa ya xb yb xc yc xd yd
res=''
If xa=xb Then Do
  k1='*'
  x1=xa
  If ya=yb Then Do
    res='Points A and B are identical'
    rs='*'
    End
  End
Else Do
  k1=(yb-ya)/(xb-xa)
  d1=ya-k1*xa
  End
If xc=xd Then Do
  k2='*'
  x2=xc
  If yc=yd Then Do
    res='Points C and D are identical'
    rs='*'
    End
  End
Else Do
  k2=(yd-yc)/(xd-xc)
  d2=yc-k2*xc
  End

If res='' Then Do
  If k1='*' Then Do
    If k2='*' Then Do
      If x1=x2 Then Do
        res='Lines AB and CD are identical'
        rs='I'
        End
      Else Do
        res='Lines AB and CD are parallel'
        rs='-'
        End
      End
    Else Do
      x=x1
      y=k2*x+d2
      End
    End
  Else Do
    If k2='*' Then Do
      x=x2
      y=k1*x+d1
      End
    Else Do
      If k1=k2 Then Do
        If d1=d2 Then Do
          res='Lines AB and CD are identical'
          rs='I'
          End
        Else Do
          res='Lines AB and CD are parallel'
          rs='-'
          End
        End
      Else Do
        x=(d2-d1)/(k1-k2)
        y=k1*x+d1
        End
      End
    End
  End
  If res='' Then Do
    res='Intersection is ('||x'/'y')'
    rs=x y
    Call o 'line intersection' x y
    End
  Call o 'A=('xa'/'ya') B=('||xb'/'yb') C=('||xc'/'yc') D=('||xd'/'yd')' '-->' res
  Return rs

isb: Procedure
  Parse Arg a,b,c
  Return sign(b-a)<>sign(b-c)

is_between: Procedure Expose oid
  Parse Arg a,b,c
  Return diff_sign(b-a,b-c)

diff_sign: Procedure
  Parse Arg diff1,diff2
  Return (sign(diff1)<>sign(diff2))|(sign(diff1)=0)

o:
/*y 'sigl='sigl */
Return lineout(oid,arg(1))

in_tri: Procedure Expose oid bordl
/*---------------------------------------------------------------------
* Determine if the point (px/py) is within the given triangle
*--------------------------------------------------------------------*/
Parse Arg px py,ax ay bx by cx cy
abc=ax ay bx by cx cy
res=0
maxx=max(ax,bx,cx)
minx=min(ax,bx,cx)
maxy=max(ay,by,cy)
miny=min(ay,by,cy)

If px>maxx|px<minx|py>maxy|py<miny Then
  Return 0

Parse Value mk_g(ax ay,bx by) With k.1 d.1 x.1
Parse Value mk_g(bx by,cx cy) With k.2 d.2 x.2
Parse Value mk_g(cx cy,ax ay) With k.3 d.3 x.3
/*
say 'g1:' show_g(k.1,d.1,x.1)
say 'g2:' show_g(k.2,d.2,x.2)
say 'g3:' show_g(k.3,d.3,x.3)
Say px py '-' ax ay bx by cx cy
*/
Do i=1 To 3
  Select
    When k.i='*' Then
      Call o 'g.'i':' 'x='||x.i
    When k.i=0 Then
      Call o 'g.'i':' 'y='d.i
    Otherwise
      Call o 'g.'i':' 'y=' k.i'*x'dd(d.i)
    End
  End

If k.1='*' Then Do
  y2=k.2*px+d.2
  y3=k.3*px+d.3
  If is_between(y2,py,y3) Then
    res=1
  End
Else Do
  kp1=k.1
  dp1=py-kp1*px
  If k.2='*' Then
    x12=x.2
  Else
    x12=(d.2-dp1)/(kp1-k.2)
  If k.3='*' Then
    x13=x.3
  Else
    x13=(d.3-dp1)/(kp1-k.3)
  If is_between(x12,px,x13) Then
    res=1
  End

If res=1 Then rr=' '
         Else rr=' not '
If pos(px'/'py,bordl)>0 Then Do
  ignored=' but is IGNORED'
  res=0
  End
Else
  ignored=''
Say 'P ('px','py') is'rr'in' abc  ignored
Return res

bord:
/*---------------------------------------------------------------------
* Look for corners of triangles that are situated
* on the edges of the other triangle
*--------------------------------------------------------------------*/
parse Arg tlist
Parse Arg pax pay pbx pby pcx pcy pdx pdy pex pey pfx pfy
bordl=''
abc=subword(tlist,1,6)
def=subword(tlist,7,6)

Do i=1 To 3
  s.i=subword(abc abc,i*2-1,4)
  t.i=subword(def def,i*2-1,4)
  End

abc_=''
def_=''
Do i=1 To 3
  abc.i=subword(abc,i*2-1,2)
  def.i=subword(def,i*2-1,2)
  Parse Var abc.i x y; abc_=abc_ '('||x','y')'
  Parse Var def.i x y; def_=def_ '('||x','y')'
  End

Do i=1 To 3
  i1=i+1
  If i1=4 Then i1=1
  Parse Value mk_g(abc.i,abc.i1) With k.1.i d.1.i x.1.i
  Parse Value mk_g(def.i,def.i1) With k.2.i d.2.i x.2.i
  End
Do i=1 To 3
  Call o  show_g(k.1.i,d.1.i,x.1.i)
  End
Do i=1 To 3
  Call o  show_g(k.2.i,d.2.i,x.2.i)
  End

pl=''
Do i=1 To 3
  p=def.i
  Do j=1 To 3
    j1=j+1
    If j1=4 Then j1=1
    g='1.'j
    If in_segment(p,abc.j,abc.j1) Then Do
      pp=Translate(p,'/',' ')
      If wordpos(pp,bordl)=0 Then
        bordl=bordl pp
      End
    Call o  show_p(p) show_g(k.g,d.g,x.g) '->' bordl
    End
  End
Call o  'Points on abc:' pl

pl=''
Do i=1 To 3
  p=abc.i
  Do j=1 To 3
    j1=j+1
    If j1=4 Then j1=1
    g='2.'j
    If in_segment(p,def.j,def.j1)Then Do
      pp=Translate(p,'/',' ')
      If wordpos(pp,bordl)=0 Then
        bordl=bordl pp
      End
    Call o  show_p(p) show_g(k.g,d.g,x.g) '->' bordl
    End
  End
Call o  'Points on def:' pl

Return bordl

in_segment: Procedure Expose g. sigl
/*---------------------------------------------------------------------
* Determine if point x/y is on the line segment ax/ay bx/by
*--------------------------------------------------------------------*/
Parse Arg x y,ax ay,bx by
Call show_p(x y) show_p(ax ay) show_p(bx by)
Parse Value mk_g(ax ay,bx by) With gk gd gx
Select
  When gx<>'' Then
    res=(x=gx & is_between(ay,y,by))
  When gk='*' Then
    res=(y=gd & is_between(ax,x,bx))
  Otherwise Do
    yy=gk*x+gd
    res=(y=yy & is_between(ax,x,bx))
    End
  End
Return res

mk_g: Procedure Expose g.
/*---------------------------------------------------------------------
* given two points (a and b)
* compute y=k*x+d or, if a vertical line, k='*'; x=c
*--------------------------------------------------------------------*/
Parse Arg a,b                       /* 2 points                      */
Parse Var a ax ay
Parse Var b bx by
If ax=bx Then Do                    /* vertical line                 */
  gk='*'                            /* special slope                 */
  gx=ax                             /* x=ax is  the equation         */
  gd='*'                            /* not required                  */
  End
Else Do
  gk=(by-ay)/(bx-ax)                /* compute slope                 */
  gd=ay-gk*ax                       /* compute y-distance            */
  gx=''                             /* not required                  */
  End
Return gk gd gx

is_between: Procedure
  Parse Arg a,b,c
  Return diff_sign(b-a,b-c)

diff_sign: Procedure
  Parse Arg diff1,diff2
  Return (sign(diff1)<>sign(diff2))|(sign(diff1)=0)

show_p: Procedure
  Call trace 'O'
  Parse Arg x y
  If pos('/',x)>0 Then
    Parse Var x x '/' y
  Return space('('||x'/'y')',0)

isb: Procedure Expose oid
  Parse Arg a,b,c
  Return sign(b-a)<>sign(b-c)

o: Call o  arg(1)
   Return

show_g: Procedure
/*---------------------------------------------------------------------
* given slope, y-distance, and (special) x-value
* compute y=k*x+d or, if a vertical line, k='*'; x=c
*--------------------------------------------------------------------*/
Parse Arg k,d,x
Select
  When k='*' Then res='x='||x       /* vertical line                 */
  When k=0   Then res='y='d         /* horizontal line               */
  Otherwise Do                      /* ordinary line                 */
    Select
      When k=1  Then res='y=x'dd(d)
      When k=-1 Then res='y=-x'dd(d)
      Otherwise      res='y='k'*x'dd(d)
      End
    End
  End
Return res

dd: Procedure
/*---------------------------------------------------------------------
* prepare y-distance for display
*--------------------------------------------------------------------*/
  Parse Arg dd
  Select
    When dd=0 Then dd=''            /* omit dd if it's zero          */
    When dd<0 Then dd=dd            /* use dd as is (-value)         */
    Otherwise      dd='+'dd         /* prepend '+' to positive dd    */
    End
  Return dd

ident: Procedure
/*---------------------------------------------------------------------
* Determine if the corners ABC match those of DEF (in any order)
*--------------------------------------------------------------------*/
  cnt.=0
  Do i=1 To 6
    Parse Value Arg(i) With x y
    cnt.x.y=cnt.x.y+1
    End
  Do i=1 To 3
    Parse Value Arg(i) With x y
    If cnt.x.y<>2 Then
      Return 0
    End
  Return 1

Novalue:
  Say  'Novalue raised in line' sigl
  Say  sourceline(sigl)
  Say  'Variable' condition('D')
  Signal lookaround

Syntax:
  Say  'Syntax raised in line' sigl
  Say  sourceline(sigl)
  Say  'rc='rc '('errortext(rc)')'

halt:
lookaround:
  If fore() Then Do
    Say  'You can look around now.'
    Trace ?R
    Nop
    End
  Exit 12
```

{{out}}

```txt
ABC: (0/0) (5/0) (0/5)
DEF: (0/0) (5/0) (0/6)
Corners on the other triangle's edges:  0/0 5/0 0/5
Triangles overlap

ABC: (0/0) (0/5) (5/0)
DEF: (0/0) (0/5) (5/0)
Corners on the other triangle's edges:  0/0 0/5 5/0
Triangles are identical

ABC: (0/0) (5/0) (0/5)
DEF: (-10/0) (-5/0) (-1/6)
Triangles don't overlap

ABC: (0/0) (5/0) (2.5/5)
DEF: (0/4) (2.5/-1) (5/4)
Intersection of line segments: (2/0)
Triangles overlap

ABC: (0/0) (1/1) (0/2)
DEF: (2/1) (3/0) (3/2)
Triangles don't overlap

ABC: (0/0) (1/1) (0/2)
DEF: (2/1) (3/-2) (3/4)
Triangles don't overlap

ABC: (0/0) (1/0) (0/1)
DEF: (1/0) (2/0) (1/1)
Corners on the other triangle's edges:  1/0
Triangles touch on  1/0
  we analyze further
Intersection of line segments: (1/0)
P (1,0) is in 0 0 1 0 0 1  but is IGNORED
P (1,0) is in 1 0 2 0 1 1  but is IGNORED
P (1,1) is not in 0 0 1 0 0 1
Triangles touch (border case) at (1/0)

ABC: (1/0) (3/0) (2/2)
DEF: (1/3) (3/3) (2/5)
Triangles don't overlap

ABC: (1/0) (3/0) (2/2)
DEF: (1/3) (3/3) (2/2)
Corners on the other triangle's edges:  2/2
Triangles touch on  2/2
  we analyze further
P (2,2) is in 1 3 3 3 2 2  but is IGNORED
P (2,2) is in 1 0 3 0 2 2  but is IGNORED
Triangles touch (border case) at (2/2)

ABC: (0/0) (2/0) (2/2)
DEF: (3/3) (5/3) (5/5)
Triangles don't overlap

ABC: (2/0) (2/6) (1/8)
DEF: (0/1) (0/5) (8/3)
Intersection of line segments: (2/4.50)
Triangles overlap

ABC: (0/0) (4/0) (0/4)
DEF: (0/2) (2/0) (2/2)
Corners on the other triangle's edges:  0/2 2/0 2/2
Triangles overlap

ABC: (0/0) (4/0) (0/4)
DEF: (1/1) (2/1) (1/2)
P (1,1) is in 0 0 4 0 0 4
1 1 is within 0 0 4 0 0 4
Triangles overlap

```



## Scala

{{trans|Kotlin}}

```scala
object Overlap {
  type Point = (Double, Double)

  class Triangle(var p1: Point, var p2: Point, var p3: Point) {
    override def toString: String = s"Triangle: $p1, $p2, $p3"
  }

  def det2D(t: Triangle): Double = {
    val (p1, p2, p3) = (t.p1, t.p2, t.p3)
    p1._1 * (p2._2 - p3._2) +
      p2._1 * (p3._2 - p1._2) +
      p3._1 * (p1._2 - p2._2)
  }

  def checkTriWinding(t: Triangle, allowReversed: Boolean): Unit = {
    val detTri = det2D(t)
    if (detTri < 0.0) {
      if (allowReversed) {
        val a = t.p3
        t.p3 = t.p2
        t.p2 = a
      } else throw new RuntimeException("Triangle has wrong winding direction")
    }
  }

  def boundaryCollideChk(t: Triangle, eps: Double): Boolean = det2D(t) < eps

  def boundaryDoesntCollideChk(t: Triangle, eps: Double): Boolean = det2D(t) <= eps

  def triTri2D(t1: Triangle, t2: Triangle, eps: Double = 0.0, allowReversed: Boolean = false, onBoundary: Boolean = true): Boolean = {
    //triangles must be expressed anti-clockwise
    checkTriWinding(t1, allowReversed)
    checkTriWinding(t2, allowReversed)
    // 'onBoundary' determines whether points on boundary are considered as colliding or not
    val chkEdge = if (onBoundary) Overlap.boundaryCollideChk _ else Overlap.boundaryDoesntCollideChk _
    val lp1 = Array(t1.p1, t1.p2, t1.p3)
    val lp2 = Array(t2.p1, t2.p2, t2.p3)

    // for each edge E of t1
    for (i <- 0 until 3) {
      val j = (i + 1) % 3
      // Check all points of t2 lay on the external side of edge E.
      // If they do, the triangles do not overlap.
      if (chkEdge(new Triangle(lp1(i), lp1(j), lp2(0)), eps)
        && chkEdge(new Triangle(lp1(i), lp1(j), lp2(1)), eps)
        && chkEdge(new Triangle(lp1(i), lp1(j), lp2(2)), eps)) return false
    }

    // for each edge E of t2
    for (i <- 0 until 3) {
      val j = (i + 1) % 3
      // Check all points of t1 lay on the external side of edge E.
      // If they do, the triangles do not overlap.
      if (chkEdge(new Triangle(lp2(i), lp2(j), lp1(0)), eps)
        && chkEdge(new Triangle(lp2(i), lp2(j), lp1(1)), eps)
        && chkEdge(new Triangle(lp2(i), lp2(j), lp1(2)), eps)) return false
    }

    // The triangles overlap
    true
  }

  def main(args: Array[String]): Unit = {
    var t1 = new Triangle((0.0, 0.0), (5.0, 0.0), (0.0, 5.0))
    var t2 = new Triangle((0.0, 0.0), (5.0, 0.0), (0.0, 6.0))
    println(s"$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    // need to allow reversed for this pair to avoid exception
    t1 = new Triangle((0.0, 0.0), (0.0, 5.0), (5.0, 0.0))
    t2 = t1
    println(s"\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2, 0.0, allowReversed = true)) "overlap (reversed)" else "do not overlap")

    t1 = new Triangle((0.0, 0.0), (5.0, 0.0), (0.0, 5.0))
    t2 = new Triangle((-10.0, 0.0), (-5.0, 0.0), (-1.0, 6.0))
    println(s"\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t1.p3 = (2.5, 5.0)
    t2 = new Triangle((0.0, 4.0), (2.5, -1.0), (5.0, 4.0))
    println(s"\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t1 = new Triangle((0.0, 0.0), (1.0, 1.0), (0.0, 2.0))
    t2 = new Triangle((2.0, 1.0), (3.0, 0.0), (3.0, 2.0))
    println(s"\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t2 = new Triangle((2.0, 1.0), (3.0, -2.0), (3.0, 4.0))
    println(s"\n$t1 and\n$t2")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    t1 = new Triangle((0.0, 0.0), (1.0, 0.0), (0.0, 1.0))
    t2 = new Triangle((1.0, 0.0), (2.0, 0.0), (1.0, 1.1))
    println(s"\n$t1 and\n$t2")
    println("which have only a single corner in contact, if boundary points collide")
    println(if (triTri2D(t1, t2)) "overlap" else "do not overlap")

    println(s"\n$t1 and\n$t2")
    println("which have only a single corner in contact, if boundary points do not collide")
    println(if (triTri2D(t1, t2, onBoundary = false)) "overlap" else "do not overlap")
  }
}
```

{{out}}

```txt
Triangle: (0.0,0.0), (5.0,0.0), (0.0,5.0) and
Triangle: (0.0,0.0), (5.0,0.0), (0.0,6.0)
overlap

Triangle: (0.0,0.0), (0.0,5.0), (5.0,0.0) and
Triangle: (0.0,0.0), (0.0,5.0), (5.0,0.0)
overlap (reversed)

Triangle: (0.0,0.0), (5.0,0.0), (0.0,5.0) and
Triangle: (-10.0,0.0), (-5.0,0.0), (-1.0,6.0)
do not overlap

Triangle: (0.0,0.0), (5.0,0.0), (2.5,5.0) and
Triangle: (0.0,4.0), (2.5,-1.0), (5.0,4.0)
overlap

Triangle: (0.0,0.0), (1.0,1.0), (0.0,2.0) and
Triangle: (2.0,1.0), (3.0,0.0), (3.0,2.0)
do not overlap

Triangle: (0.0,0.0), (1.0,1.0), (0.0,2.0) and
Triangle: (2.0,1.0), (3.0,-2.0), (3.0,4.0)
do not overlap

Triangle: (0.0,0.0), (1.0,0.0), (0.0,1.0) and
Triangle: (1.0,0.0), (2.0,0.0), (1.0,1.1)
which have only a single corner in contact, if boundary points collide
overlap

Triangle: (0.0,0.0), (1.0,0.0), (0.0,1.0) and
Triangle: (1.0,0.0), (2.0,0.0), (1.0,1.1)
which have only a single corner in contact, if boundary points do not collide
do not overlap
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Class Triangle
        Property P1 As Tuple(Of Double, Double)
        Property P2 As Tuple(Of Double, Double)
        Property P3 As Tuple(Of Double, Double)

        Sub New(p1 As Tuple(Of Double, Double), p2 As Tuple(Of Double, Double), p3 As Tuple(Of Double, Double))
            Me.P1 = p1
            Me.P2 = p2
            Me.P3 = p3
        End Sub

        Function Det2D() As Double
            Return P1.Item1 * (P2.Item2 - P3.Item2) +
                   P2.Item1 * (P3.Item2 - P1.Item2) +
                   P3.Item1 * (P1.Item2 - P2.Item2)
        End Function

        Sub CheckTriWinding(allowReversed As Boolean)
            Dim detTri = Det2D()
            If detTri < 0.0 Then
                If allowReversed Then
                    Dim a = P3
                    P3 = P2
                    P2 = a
                Else
                    Throw New Exception("Triangle has wrong winding direction")
                End If
            End If
        End Sub

        Function BoundaryCollideChk(eps As Double) As Boolean
            Return Det2D() < eps
        End Function

        Function BoundaryDoesntCollideChk(eps As Double) As Boolean
            Return Det2D() <= eps
        End Function

        Public Overrides Function ToString() As String
            Return String.Format("Triangle: {0}, {1}, {2}", P1, P2, P3)
        End Function
    End Class

    Function TriTri2D(t1 As Triangle, t2 As Triangle, Optional eps As Double = 0.0, Optional alloweReversed As Boolean = False, Optional onBoundary As Boolean = True) As Boolean
        'Triangles must be expressed anti-clockwise
        t1.CheckTriWinding(alloweReversed)
        t2.CheckTriWinding(alloweReversed)

        '"onboundary" determines whether points on boundary are considered as colliding or not
        Dim chkEdge = If(onBoundary, Function(t As Triangle) t.BoundaryCollideChk(eps), Function(t As Triangle) t.BoundaryDoesntCollideChk(eps))
        Dim lp1 As New List(Of Tuple(Of Double, Double)) From {t1.P1, t1.P2, t1.P3}
        Dim lp2 As New List(Of Tuple(Of Double, Double)) From {t2.P1, t2.P2, t2.P3}

        'for each edge E of t1
        For i = 0 To 2
            Dim j = (i + 1) Mod 3
            'Check all points of t2 lay on the external side of edge E.
            'If they do, the triangles do not overlap.
            If chkEdge(New Triangle(lp1(i), lp1(j), lp2(0))) AndAlso
               chkEdge(New Triangle(lp1(i), lp1(j), lp2(1))) AndAlso
               chkEdge(New Triangle(lp1(i), lp1(j), lp2(2))) Then
                Return False
            End If
        Next

        'for each edge E of t2
        For i = 0 To 2
            Dim j = (i + 1) Mod 3
            'Check all points of t1 lay on the external side of edge E.
            'If they do, the triangles do not overlap.
            If chkEdge(New Triangle(lp2(i), lp2(j), lp1(0))) AndAlso
               chkEdge(New Triangle(lp2(i), lp2(j), lp1(1))) AndAlso
               chkEdge(New Triangle(lp2(i), lp2(j), lp1(2))) Then
                Return False
            End If
        Next

        'The triangles overlap
        Return True
    End Function

    Sub Overlap(t1 As Triangle, t2 As Triangle, Optional eps As Double = 0.0, Optional allowReversed As Boolean = False, Optional onBoundary As Boolean = True)
        If TriTri2D(t1, t2, eps, allowReversed, onBoundary) Then
            Console.WriteLine("overlap")
        Else
            Console.WriteLine("do not overlap")
        End If
    End Sub

    Sub Main()
        Dim t1 = New Triangle(Tuple.Create(0.0, 0.0), Tuple.Create(5.0, 0.0), Tuple.Create(0.0, 5.0))
        Dim t2 = New Triangle(Tuple.Create(0.0, 0.0), Tuple.Create(5.0, 0.0), Tuple.Create(0.0, 6.0))
        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Overlap(t1, t2)
        Console.WriteLine()

        ' need to allow reversed for this pair to avoid exception
        t1 = New Triangle(Tuple.Create(0.0, 0.0), Tuple.Create(0.0, 5.0), Tuple.Create(5.0, 0.0))
        t2 = t1
        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Overlap(t1, t2, 0.0, True)
        Console.WriteLine()

        t1 = New Triangle(Tuple.Create(0.0, 0.0), Tuple.Create(5.0, 0.0), Tuple.Create(0.0, 5.0))
        t2 = New Triangle(Tuple.Create(-10.0, 0.0), Tuple.Create(-5.0, 0.0), Tuple.Create(-1.0, 6.0))
        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Overlap(t1, t2)
        Console.WriteLine()

        t1.P3 = Tuple.Create(2.5, 5.0)
        t2 = New Triangle(Tuple.Create(0.0, 4.0), Tuple.Create(2.5, -1.0), Tuple.Create(5.0, 4.0))
        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Overlap(t1, t2)
        Console.WriteLine()

        t1 = New Triangle(Tuple.Create(0.0, 0.0), Tuple.Create(1.0, 1.0), Tuple.Create(0.0, 2.0))
        t2 = New Triangle(Tuple.Create(2.0, 1.0), Tuple.Create(3.0, 0.0), Tuple.Create(3.0, 2.0))
        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Overlap(t1, t2)
        Console.WriteLine()

        t2 = New Triangle(Tuple.Create(2.0, 1.0), Tuple.Create(3.0, -2.0), Tuple.Create(3.0, 4.0))
        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Overlap(t1, t2)
        Console.WriteLine()

        t1 = New Triangle(Tuple.Create(0.0, 0.0), Tuple.Create(1.0, 0.0), Tuple.Create(0.0, 1.0))
        t2 = New Triangle(Tuple.Create(1.0, 0.0), Tuple.Create(2.0, 0.0), Tuple.Create(1.0, 1.1))
        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Console.WriteLine("which have only a single corner in contact, if boundary points collide")
        Overlap(t1, t2)
        Console.WriteLine()

        Console.WriteLine("{0} and", t1)
        Console.WriteLine("{0}", t2)
        Console.WriteLine("which have only a single corner in contact, if boundary points do not collide")
        Overlap(t1, t2, 0.0, False, False)
    End Sub

End Module
```

{{out}}

```txt
Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (0, 0), (5, 0), (0, 6)
overlap

Triangle: (0, 0), (0, 5), (5, 0) and
Triangle: (0, 0), (0, 5), (5, 0)
overlap

Triangle: (0, 0), (5, 0), (0, 5) and
Triangle: (-10, 0), (-5, 0), (-1, 6)
do not overlap

Triangle: (0, 0), (5, 0), (2.5, 5) and
Triangle: (0, 4), (2.5, -1), (5, 4)
overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, 0), (3, 2)
do not overlap

Triangle: (0, 0), (1, 1), (0, 2) and
Triangle: (2, 1), (3, -2), (3, 4)
do not overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1.1)
which have only a single corner in contact, if boundary points collide
overlap

Triangle: (0, 0), (1, 0), (0, 1) and
Triangle: (1, 0), (2, 0), (1, 1.1)
which have only a single corner in contact, if boundary points do not collide
do not overlap
```



## zkl

{{trans|C++}}

```zkl
// A triangle is three pairs of points: ( (x,y), (x,y), (x,y) )

fcn det2D(triangle){
   p1,p2,p3 := triangle;
   p1[0]*(p2[1] - p3[1]) + p2[0]*(p3[1] - p1[1]) + p3[0]*(p1[1] - p2[1]);
}
fcn checkTriWinding(triangle,allowReversed){ //-->triangle, maybe new
   detTri:=det2D(triangle);
   if(detTri<0.0){
      if(allowReversed){ p1,p2,p3 := triangle; return(p1,p3,p2); }  // reverse
      else throw(Exception.AssertionError(
		  "triangle has wrong winding direction"));
   }
   triangle	// no change
}
fcn triTri2D(triangle1,triangle2, eps=0.0, allowReversed=False, onBoundary=True){
   // Trangles must be expressed anti-clockwise
   triangle1=checkTriWinding(triangle1, allowReversed);
   triangle2=checkTriWinding(triangle2, allowReversed);

   chkEdge:=
      if(onBoundary) // Points on the boundary are considered as colliding
	 fcn(triangle,eps){ det2D(triangle)<eps }
      else           // Points on the boundary are not considered as colliding
	 fcn(triangle,eps){ det2D(triangle)<=eps };; // first ; terminates if

   t1,t2 := triangle1,triangle2;	// change names to protect the typist
   do(2){				// check triangle1 and then triangle2
      foreach i in (3){	//For edge E of trangle 1,
	 j:=(i+1)%3;	// 1,2,0
	 // Check all points of trangle 2 lay on the external side
	 // of the edge E. If they do, the triangles do not collide.
	 if(chkEdge(T(t1[i],t1[j],t2[0]), eps) and
	    chkEdge(T(t1[i],t1[j],t2[1]), eps) and
	    chkEdge(T(t1[i],t1[j],t2[2]), eps)) return(False);  // no collision
      }
      t2,t1 = triangle1,triangle2; // flip and re-test
   }
   True   // The triangles collide
}
```


```zkl
fcn toTri(ax,ay,bx,by,cx,cy){ //-->( (ax,ay),(bx,by),(cx,cy) )
   vm.arglist.apply("toFloat").pump(List,Void.Read)
}
triangles:=T(	// pairs of triangles
   T(toTri(0,0, 5,0, 0,  5), toTri(  0,0,  5,   0,  0,6)),
   T(toTri(0,0, 0,5, 5,  0), toTri(  0,0,  0,   5 , 5,0)),
   T(toTri(0,0, 5,0, 0,  5), toTri(-10,0, -5,   0, -1,6)),
   T(toTri(0,0, 5,0, 2.5,5), toTri(  0,4,  2.5,-1,  5,4)),
   T(toTri(0,0, 1,1, 0,  2), toTri(  2,1,  3,   0,  3,2)),
   T(toTri(0,0, 1,1, 0,  2), toTri(  2,1,  3,  -2,  3,4))
);

  // Expect: overlap, overlap (reversed), no overlap, overlap, no overlap, no overlap
foreach t1,t2 in (triangles){
   reg r, reversed=False;
   try{ r=triTri2D(t1,t2) }
   catch(AssertionError){ r=triTri2D(t1,t2,0.0,True); reversed=True; }
   print(t1,"\n",t2," ");
   println(r and "overlap" or "no overlap", reversed and " (reversed)" or "");
   println();
}

c1,c2 := toTri(0,0, 1,0, 0,1), toTri(1,0, 2,0, 1,1);
println("Corner case (barely touching): ",triTri2D(c1,c2,0.0,False,True));  // True
println("Corner case (barely touching): ",triTri2D(c1,c2,0.0,False,False)); // False
```

{{out}}

```txt

L(L(0,0),L(5,0),L(0,5))
L(L(0,0),L(5,0),L(0,6)) overlap

L(L(0,0),L(0,5),L(5,0))
L(L(0,0),L(0,5),L(5,0)) overlap (reversed)

L(L(0,0),L(5,0),L(0,5))
L(L(-10,0),L(-5,0),L(-1,6)) no overlap

L(L(0,0),L(5,0),L(2.5,5))
L(L(0,4),L(2.5,-1),L(5,4)) overlap

L(L(0,0),L(1,1),L(0,2))
L(L(2,1),L(3,0),L(3,2)) no overlap

L(L(0,0),L(1,1),L(0,2))
L(L(2,1),L(3,-2),L(3,4)) no overlap

Corner case (barely touching): True
Corner case (barely touching): False

```


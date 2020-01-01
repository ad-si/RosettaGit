+++
title = "Convex hull"
description = ""
date = 2019-08-19T10:35:09Z
aliases = []
[extra]
id = 19207
[taxonomies]
categories = []
tags = []
+++

{{task|geometry}}

Find the points which form a [[wp:Convex hull|convex hull]] from a set of arbitrary two dimensional points.

For example, given the points (16,3),  (12,17), (0,6),   (-4,-6), (16,6),  (16,-7), (16,-3), (17,-4), (5,19),  (19,-8), (3,16),  (12,13), (3,-4),  (17,5),  (-3,15), (-3,-9), (0,11),  (-9,-3), (-4,-2) and (12,10) the convex hull would be (-9,-3), (-3,-9), (19,-8), (17,5), (12,17), (5,19) and (-3,15).



;See also
* [https://www.youtube.com/watch?v=wRTGDig3jx8 Convex Hull (youtube)]
* http://www.geeksforgeeks.org/convex-hull-set-2-graham-scan/





## C

{{trans|C++}}

```C>#include <stdbool.h

#include <stdio.h>
#include <stdlib.h>

typedef struct tPoint {
    int x, y;
} Point;

typedef struct tNode {
    Point data;
    struct tNode *next;
} Node;

bool ccw(const Point *a, const Point *b, const Point *c) {
    return (b->x - a->x) * (c->y - a->y)
         > (b->y - a->y) * (c->x - a->x);
}

int comp(const void *lhs, const void *rhs) {
    Point lp = *((Point *)lhs);
    Point rp = *((Point *)rhs);
    if (lp.x < rp.x) return -1;
    if (rp.x < lp.x) return 1;
    return 0;
}

void freeNode(Node *ptr) {
    if (ptr == NULL) {
        return;
    }

    freeNode(ptr->next);
    ptr->next = NULL;
    free(ptr);
}

Node* pushBack(Node *ptr, Point data) {
    Node *tmp = ptr;

    if (ptr == NULL) {
        ptr = (Node*)malloc(sizeof(Node));
        ptr->data = data;
        ptr->next = NULL;
        return ptr;
    }

    while (tmp->next != NULL) {
        tmp = tmp->next;
    }

    tmp->next = (Node*)malloc(sizeof(Node));
    tmp->next->data = data;
    tmp->next->next = NULL;
    return ptr;
}

Node* popBack(Node *ptr) {
    Node *tmp = ptr;

    if (ptr == NULL) {
        return NULL;
    }
    if (ptr->next == NULL) {
        free(ptr);
        return NULL;
    }

    while (tmp->next->next != NULL) {
        tmp = tmp->next;
    }

    free(tmp->next);
    tmp->next = NULL;
    return ptr;
}

void print(Node *ptr) {
    printf("[");
    if (ptr != NULL) {
        printf("(%d, %d)", ptr->data.x, ptr->data.y);
        ptr = ptr->next;
    }
    while (ptr != NULL) {
        printf(", (%d, %d)", ptr->data.x, ptr->data.y);
        ptr = ptr->next;
    }
    printf("]");
}

Node* convexHull(int len, Point p[]) {
    Node *h = NULL;
    Node *hptr = NULL;
    size_t hLen = 0;
    int i;

    qsort(p, len, sizeof(Point), comp);

    /* lower hull */
    for (i = 0; i < len; ++i) {
        while (hLen >= 2) {
            hptr = h;
            while (hptr->next->next != NULL) {
                hptr = hptr->next;
            }
            if (ccw(&hptr->data, &hptr->next->data, &p[i])) {
                break;
            }

            h = popBack(h);
            hLen--;
        }

        h = pushBack(h, p[i]);
        hLen++;
    }

    /* upper hull */
    for (i = len - 1; i >= 0; i--) {
        while (hLen >= 2) {
            hptr = h;
            while (hptr->next->next != NULL) {
                hptr = hptr->next;
            }
            if (ccw(&hptr->data, &hptr->next->data, &p[i])) {
                break;
            }

            h = popBack(h);
            hLen--;
        }

        h = pushBack(h, p[i]);
        hLen++;
    }

    popBack(h);
    return h;
}

int main() {
    Point points[] = {
        {16,  3}, {12, 17}, { 0,  6}, {-4, -6}, {16,  6},
        {16, -7}, {16, -3}, {17, -4}, { 5, 19}, {19, -8},
        { 3, 16}, {12, 13}, { 3, -4}, {17,  5}, {-3, 15},
        {-3, -9}, { 0, 11}, {-9, -3}, {-4, -2}, {12, 10}
    };

    Node *hull = convexHull(sizeof(points) / sizeof(Point), points);
    printf("Convex Hull: ");
    print(hull);
    printf("\n");

    freeNode(hull);
    hull = NULL;

    return 0;
}
```

{{out}}

```txt
Convex Hull: [(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]
```



## C++

{{trans|D}}

```cpp
#include <algorithm>
#include <iostream>
#include <ostream>
#include <vector>
#include <tuple>

typedef std::tuple<int, int> point;

std::ostream& print(std::ostream& os, const point& p) {
    return os << "(" << std::get<0>(p) << ", " << std::get<1>(p) << ")";
}

std::ostream& print(std::ostream& os, const std::vector<point>& v) {
    auto it = v.cbegin();
    auto end = v.cend();

    os << "[";

    if (it != end) {
        print(os, *it);
        it = std::next(it);
    }
    while (it != end) {
        os << ", ";
        print(os, *it);
        it = std::next(it);
    }

    return os << "]";
}

// returns true if the three points make a counter-clockwise turn
bool ccw(const point& a, const point& b, const point& c) {
    return ((std::get<0>(b) - std::get<0>(a)) * (std::get<1>(c) - std::get<1>(a)))
         > ((std::get<1>(b) - std::get<1>(a)) * (std::get<0>(c) - std::get<0>(a)));
}

std::vector<point> convexHull(std::vector<point> p) {
    if (p.size() == 0) return std::vector<point>();
    std::sort(p.begin(), p.end(), [](point& a, point& b){
        if (std::get<0>(a) < std::get<0>(b)) return true;
        return false;
    });

    std::vector<point> h;

    // lower hull
    for (const auto& pt : p) {
        while (h.size() >= 2 && !ccw(h.at(h.size() - 2), h.at(h.size() - 1), pt)) {
            h.pop_back();
        }
        h.push_back(pt);
    }

    // upper hull
    auto t = h.size() + 1;
    for (auto it = p.crbegin(); it != p.crend(); it = std::next(it)) {
        auto pt = *it;
        while (h.size() >= t && !ccw(h.at(h.size() - 2), h.at(h.size() - 1), pt)) {
            h.pop_back();
        }
        h.push_back(pt);
    }

    h.pop_back();
    return h;
}

int main() {
    using namespace std;

    vector<point> points = {
        make_pair(16, 3),  make_pair(12, 17), make_pair(0,  6),  make_pair(-4, -6), make_pair(16,  6),
        make_pair(16, -7), make_pair(16, -3), make_pair(17, -4), make_pair(5, 19),  make_pair(19, -8),
        make_pair(3, 16),  make_pair(12, 13), make_pair(3, -4),  make_pair(17,  5), make_pair(-3, 15),
        make_pair(-3, -9), make_pair(0, 11),  make_pair(-9, -3), make_pair(-4, -2), make_pair(12, 10)
    };

    auto hull = convexHull(points);
    auto it = hull.cbegin();
    auto end = hull.cend();

    cout << "Convex Hull: ";
    print(cout, hull);
    cout << endl;

    return 0;
}
```

{{out}}

```txt
Convex Hull: [(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]
```


=={{header|C#|C sharp}}==

```csharp
using System;
using System.Collections.Generic;

namespace ConvexHull {
    class Point : IComparable<Point> {
        private int x, y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public int X { get => x; set => x = value; }
        public int Y { get => y; set => y = value; }

        public int CompareTo(Point other) {
            return x.CompareTo(other.x);
        }

        public override string ToString() {
            return string.Format("({0}, {1})", x, y);
        }
    }

    class Program {
        private static List<Point> ConvexHull(List<Point> p) {
            if (p.Count == 0) return new List<Point>();
            p.Sort();
            List<Point> h = new List<Point>();

            // lower hull
            foreach (var pt in p) {
                while (h.Count >= 2 && !Ccw(h[h.Count - 2], h[h.Count - 1], pt)) {
                    h.RemoveAt(h.Count - 1);
                }
                h.Add(pt);
            }

            // upper hull
            int t = h.Count + 1;
            for (int i = p.Count - 1; i >= 0; i--) {
                Point pt = p[i];
                while (h.Count >= t && !Ccw(h[h.Count - 2], h[h.Count - 1], pt)) {
                    h.RemoveAt(h.Count - 1);
                }
                h.Add(pt);
            }

            h.RemoveAt(h.Count - 1);
            return h;
        }

        private static bool Ccw(Point a, Point b, Point c) {
            return ((b.X - a.X) * (c.Y - a.Y)) > ((b.Y - a.Y) * (c.X - a.X));
        }

        static void Main(string[] args) {
            List<Point> points = new List<Point>() {
                new Point(16, 3),
                new Point(12, 17),
                new Point(0, 6),
                new Point(-4, -6),
                new Point(16, 6),

                new Point(16, -7),
                new Point(16, -3),
                new Point(17, -4),
                new Point(5, 19),
                new Point(19, -8),

                new Point(3, 16),
                new Point(12, 13),
                new Point(3, -4),
                new Point(17, 5),
                new Point(-3, 15),

                new Point(-3, -9),
                new Point(0, 11),
                new Point(-9, -3),
                new Point(-4, -2),
                new Point(12, 10)
            };

            List<Point> hull = ConvexHull(points);
            Console.Write("Convex Hull: [");
            for (int i = 0; i < hull.Count; i++) {
                if (i > 0) {
                    Console.Write(", ");
                }
                Point pt = hull[i];
                Console.Write(pt);
            }
            Console.WriteLine("]");
        }
    }
}
```

{{out}}

```txt
Convex Hull: [(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]
```



## D

{{trans|Kotlin}}

```D
import std.algorithm.sorting;
import std.stdio;

struct Point {
    int x;
    int y;

    int opCmp(Point rhs) {
        if (x < rhs.x) return -1;
        if (rhs.x < x) return 1;
        return 0;
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;
        sink("(");
        formattedWrite(sink, "%d", x);
        sink(",");
        formattedWrite(sink, "%d", y);
        sink(")");
    }
}

Point[] convexHull(Point[] p) {
    if (p.length == 0) return [];
    p.sort;
    Point[] h;

    // lower hull
    foreach (pt; p) {
        while (h.length >= 2 && !ccw(h[$-2], h[$-1], pt)) {
            h.length--;
        }
        h ~= pt;
    }

    // upper hull
    auto t = h.length + 1;
    foreach_reverse (i; 0..(p.length - 1)) {
        auto pt = p[i];
        while (h.length >= t && !ccw(h[$-2], h[$-1], pt)) {
            h.length--;
        }
        h ~= pt;
    }

    h.length--;
    return h;
}

/* ccw returns true if the three points make a counter-clockwise turn */
auto ccw(Point a, Point b, Point c) {
    return ((b.x - a.x) * (c.y - a.y)) > ((b.y - a.y) * (c.x - a.x));
}

void main() {
    auto points = [
        Point(16,  3), Point(12, 17), Point( 0,  6), Point(-4, -6), Point(16,  6),
        Point(16, -7), Point(16, -3), Point(17, -4), Point( 5, 19), Point(19, -8),
        Point( 3, 16), Point(12, 13), Point( 3, -4), Point(17,  5), Point(-3, 15),
        Point(-3, -9), Point( 0, 11), Point(-9, -3), Point(-4, -2), Point(12, 10)
    ];
    auto hull = convexHull(points);
    writeln("Convex Hull: ", hull);
}
```

{{out}}

```txt
Convex Hull: [(-9,-3), (-3,-9), (19,-8), (17,5), (12,17), (5,19), (-3,15)]
```



## Go


```go
package main

import (
	"fmt"
	"image"
	"sort"
)


// ConvexHull returns the set of points that define the
// convex hull of p in CCW order starting from the left most.
func (p points) ConvexHull() points {
	// From https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
	// with only minor deviations.
	sort.Sort(p)
	var h points

	// Lower hull
	for _, pt := range p {
		for len(h) >= 2 && !ccw(h[len(h)-2], h[len(h)-1], pt) {
			h = h[:len(h)-1]
		}
		h = append(h, pt)
	}

	// Upper hull
	for i, t := len(p)-2, len(h)+1; i >= 0; i-- {
		pt := p[i]
		for len(h) >= t && !ccw(h[len(h)-2], h[len(h)-1], pt) {
			h = h[:len(h)-1]
		}
		h = append(h, pt)
	}

	return h[:len(h)-1]
}

// ccw returns true if the three points make a counter-clockwise turn
func ccw(a, b, c image.Point) bool {
	return ((b.X - a.X) * (c.Y - a.Y)) > ((b.Y - a.Y) * (c.X - a.X))
}

type points []image.Point

func (p points) Len() int      { return len(p) }
func (p points) Swap(i, j int) { p[i], p[j] = p[j], p[i] }
func (p points) Less(i, j int) bool {
	if p[i].X == p[j].X {
		return p[i].Y < p[i].Y
	}
	return p[i].X < p[j].X
}

func main() {
	pts := points{
		{16, 3}, {12, 17}, {0, 6}, {-4, -6}, {16, 6},
		{16, -7}, {16, -3}, {17, -4}, {5, 19}, {19, -8},
		{3, 16}, {12, 13}, {3, -4}, {17, 5}, {-3, 15},
		{-3, -9}, {0, 11}, {-9, -3}, {-4, -2}, {12, 10},
	}
	hull := pts.ConvexHull()
	fmt.Println("Convex Hull:", hull)
}
```

{{out}}

```txt

Convex Hull: [(-9,-3) (-3,-9) (19,-8) (17,5) (12,17) (5,19) (-3,15)]

```



## Haskell


```Haskell
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing)

(x, y) = ((!! 0), (!! 1))

compareFrom
  :: (Num a, Ord a)
  => [a] -> [a] -> [a] -> Ordering
compareFrom o l r =
  compare ((x l - x o) * (y r - y o)) ((y l - y o) * (x r - x o))

distanceFrom
  :: Floating a
  => [a] -> [a] -> a
distanceFrom from to = ((x to - x from) ** 2 + (y to - y from) ** 2) ** (1 / 2)

convexHull
  :: (Floating a, Ord a)
  => [[a]] -> [[a]]
convexHull points =
  let o = minimum points
      presorted = sortBy (compareFrom o) (filter (/= o) points)
      collinears = groupBy (((EQ ==) .) . compareFrom o) presorted
      outmost = maximumBy (comparing (distanceFrom o)) <$> collinears
  in dropConcavities [o] outmost

dropConcavities
  :: (Num a, Ord a)
  => [[a]] -> [[a]] -> [[a]]
dropConcavities (left:lefter) (right:righter:rightest) =
  case compareFrom left right righter of
    LT -> dropConcavities (right : left : lefter) (righter : rightest)
    EQ -> dropConcavities (left : lefter) (righter : rightest)
    GT -> dropConcavities lefter (left : righter : rightest)
dropConcavities output lastInput = lastInput ++ output

main :: IO ()
main =
  mapM_ print $
  convexHull
    [ [16, 3]
    , [12, 17]
    , [0, 6]
    , [-4, -6]
    , [16, 6]
    , [16, -7]
    , [16, -3]
    , [17, -4]
    , [5, 19]
    , [19, -8]
    , [3, 16]
    , [12, 13]
    , [3, -4]
    , [17, 5]
    , [-3, 15]
    , [-3, -9]
    , [0, 11]
    , [-9, -3]
    , [-4, -2]
    , [12, 10]
    ]
```

{{Out}}

```txt
[-3.0,-9.0]
[19.0,-8.0]
[17.0,5.0]
[12.0,17.0]
[5.0,19.0]
[-3.0,15.0]
[-9.0,-3.0]
```



## J


Restated from the implementation at http://kukuruku.co/hub/funcprog/introduction-to-j-programming-language-2004 which in turn is a translation of http://dr-klm.livejournal.com/42312.html


```J
counterclockwise =: ({. , }. /: 12 o. }. - {.) @ /:~
crossproduct =: 11"_ o. [: (* +)/ }. - {.
removeinner =: #~ 1, 0 > 3 crossproduct\ ], 1:
hull =: [: removeinner^:_ counterclockwise
```


Example use:


```J
   hull 16j3 12j17 0j6 _4j_6 16j6 16j_7 16j_3 17j_4 5j19 19j_8 3j16 12j13 3j_4 17j5 _3j15 _3j_9 0j11 _9j_3 _4j_2 12j10
_9j_3 _3j_9 19j_8 17j5 12j17 5j19 _3j15
```



## Java

{{trans|Kotlin}}

```Java
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Collections.emptyList;

public class ConvexHull {
    private static class Point implements Comparable<Point> {
        private int x, y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public int compareTo(Point o) {
            return Integer.compare(x, o.x);
        }

        @Override
        public String toString() {
            return String.format("(%d, %d)", x, y);
        }
    }

    private static List<Point> convexHull(List<Point> p) {
        if (p.isEmpty()) return emptyList();
        p.sort(Point::compareTo);
        List<Point> h = new ArrayList<>();

        // lower hull
        for (Point pt : p) {
            while (h.size() >= 2 && !ccw(h.get(h.size() - 2), h.get(h.size() - 1), pt)) {
                h.remove(h.size() - 1);
            }
            h.add(pt);
        }

        // upper hull
        int t = h.size() + 1;
        for (int i = p.size() - 1; i >= 0; i--) {
            Point pt = p.get(i);
            while (h.size() >= t && !ccw(h.get(h.size() - 2), h.get(h.size() - 1), pt)) {
                h.remove(h.size() - 1);
            }
            h.add(pt);
        }

        h.remove(h.size() - 1);
        return h;
    }

    // ccw returns true if the three points make a counter-clockwise turn
    private static boolean ccw(Point a, Point b, Point c) {
        return ((b.x - a.x) * (c.y - a.y)) > ((b.y - a.y) * (c.x - a.x));
    }

    public static void main(String[] args) {
        List<Point> points = Arrays.asList(new Point(16, 3),
                                           new Point(12, 17),
                                           new Point(0, 6),
                                           new Point(-4, -6),
                                           new Point(16, 6),

                                           new Point(16, -7),
                                           new Point(16, -3),
                                           new Point(17, -4),
                                           new Point(5, 19),
                                           new Point(19, -8),

                                           new Point(3, 16),
                                           new Point(12, 13),
                                           new Point(3, -4),
                                           new Point(17, 5),
                                           new Point(-3, 15),

                                           new Point(-3, -9),
                                           new Point(0, 11),
                                           new Point(-9, -3),
                                           new Point(-4, -2),
                                           new Point(12, 10));

        List<Point> hull = convexHull(points);
        System.out.printf("Convex Hull: %s\n", hull);
    }
}
```

{{out}}

```txt
Convex Hull: [(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]
```



## Javascript


```Javascript

function convexHull(points) {
    points.sort(comparison);
    var L = [];
    for (var i = 0; i < points.length; i++) {
        while (L.length >= 2 && cross(L[L.length - 2], L[L.length - 1], points[i]) <= 0) {
            L.pop();
        }
        L.push(points[i]);
    }
    var U = [];
    for (var i = points.length - 1; i >= 0; i--) {
        while (U.length >= 2 && cross(U[U.length - 2], U[U.length - 1], points[i]) <= 0) {
            U.pop();
        }
        U.push(points[i]);
    }
    L.pop();
    U.pop();
    return L.concat(U);
}

function comparison(a, b) {
    return a.x == b.x ? a.y - b.y : a.x - b.x;
}

function cross(a, b, o) {
    return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
}

```


'''For usage''':
<nowiki>convexhull.js</nowiki>

```Javascript

var points = [];
var hull = [];

function setup() {
    createCanvas(1132, 700);
    frameRate(10);

    strokeWeight(4);
    stroke(220);
}

function draw() {
    background(40);
    // draw points
    for (i = 0; i < points.length; i++) {
        point(points[i].x, points[i].y);
    };
    console.log(hull);
    // draw hull
    noFill();
    beginShape();
    for (i = 0; i < hull.length; i++) {
        vertex(hull[i].x, hull[i].y);
    };
    endShape(CLOSE);
}

function mouseClicked() {
    points.push(createVector(mouseX, mouseY));
    hull = convexHull(points);
    noFill();
    //console.log(hull);
    beginShape();
    for (var i = 0; i < hull.length; i++) {
        vertex(hull[i].x, hull[i].y);
    }
    endShape(CLOSE);
    return false;
}

// https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain
function convexHull(points) {
    points.sort(comparison);
    var L = [];
    for (var i = 0; i < points.length; i++) {
        while (L.length >= 2 && cross(L[L.length - 2], L[L.length - 1], points[i]) <= 0) {
            L.pop();
        }
        L.push(points[i]);
    }
    var U = [];
    for (var i = points.length - 1; i >= 0; i--) {
        while (U.length >= 2 && cross(U[U.length - 2], U[U.length - 1], points[i]) <= 0) {
            U.pop();
        }
        U.push(points[i]);
    }
    L.pop();
    U.pop();
    return L.concat(U);
}

function comparison(a, b) {
    return a.x == b.x ? a.y - b.y : a.x - b.x;
}

function cross(a, b, o) {
    return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
}

```


<nowiki>convexhull.html</nowiki>

```html

<html>

<head>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.7.2/p5.js"></script>
    <script src="convexhull.js"></script>
</head>

<body>
    <table>
        <tr>
            <th><h1>Convex Hull</h4></th>
            <th><h4>Left mouse: Add points</h6></th>
        </tr>
    </table>

</body>

</html>

```



## Julia


```Julia
# v1.0.4
# https://github.com/JuliaPolyhedra/Polyhedra.jl/blob/master/examples/operations.ipynb
using Polyhedra, CDDLib

A = vrep([[16,3],  [12,17], [0,6], [-4,-6], [16,6], [16,-7], [16,-3], [17,-4], [5,19], [19,-8], [3,16], [12,13], [3,-4], [17,5], [-3,15], [-3,-9], [0,11], [-9,-3], [-4,-2], [12,10]])
P = polyhedron(A, CDDLib.Library())
Pch = convexhull(P, P)
removevredundancy!(Pch)
println("$Pch")
```

{{out}}

```txt
convexhull([5.0, 19.0], [19.0, -8.0], [17.0, 5.0], [-3.0, 15.0], [-9.0, -3.0], [12.0, 17.0], [-3.0, -9.0])
```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.3

class Point(val x: Int, val y: Int) : Comparable<Point> {

    override fun compareTo(other: Point) = this.x.compareTo(other.x)

    override fun toString() = "($x, $y)"
}

fun convexHull(p: Array<Point>): List<Point> {
    if (p.isEmpty()) return emptyList()
    p.sort()
    val h = mutableListOf<Point>()

    // lower hull
    for (pt in p) {
        while (h.size >= 2 && !ccw(h[h.size - 2], h.last(), pt)) {
            h.removeAt(h.lastIndex)
        }
        h.add(pt)
    }

    // upper hull
    val t = h.size + 1
    for (i in p.size - 2 downTo 0) {
        val pt = p[i]
        while (h.size >= t && !ccw(h[h.size - 2], h.last(), pt)) {
            h.removeAt(h.lastIndex)
        }
        h.add(pt)
    }

    h.removeAt(h.lastIndex)
    return h
}

/* ccw returns true if the three points make a counter-clockwise turn */
fun ccw(a: Point, b: Point, c: Point) =
    ((b.x - a.x) * (c.y - a.y)) > ((b.y - a.y) * (c.x - a.x))

fun main(args: Array<String>) {
    val points = arrayOf(
        Point(16,  3), Point(12, 17), Point( 0,  6), Point(-4, -6), Point(16,  6),
        Point(16, -7), Point(16, -3), Point(17, -4), Point( 5, 19), Point(19, -8),
        Point( 3, 16), Point(12, 13), Point( 3, -4), Point(17,  5), Point(-3, 15),
        Point(-3, -9), Point( 0, 11), Point(-9, -3), Point(-4, -2), Point(12, 10)
    )
    val hull = convexHull(points)
    println("Convex Hull: $hull")
}
```


{{out}}

```txt

Convex Hull: [(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]

```



## Lua

{{trans|C++}}

```lua
function print_point(p)
    io.write("("..p.x..", "..p.y..")")
    return nil
end

function print_points(pl)
    io.write("[")
    for i,p in pairs(pl) do
        if i>1 then
            io.write(", ")
        end
        print_point(p)
    end
    io.write("]")
    return nil
end

function ccw(a,b,c)
    return (b.x - a.x) * (c.y - a.y) > (b.y - a.y) * (c.x - a.x)
end

function pop_back(ta)
    table.remove(ta,#ta)
    return ta
end

function convexHull(pl)
    if #pl == 0 then
        return {}
    end
    table.sort(pl, function(left,right)
        return left.x < right.x
    end)

    local h = {}

    -- lower hull
    for i,pt in pairs(pl) do
        while #h >= 2 and not ccw(h[#h-1], h[#h], pt) do
            table.remove(h,#h)
        end
        table.insert(h,pt)
    end

    -- upper hull
    local t = #h + 1
    for i=#pl, 1, -1 do
        local pt = pl[i]
        while #h >= t and not ccw(h[#h-1], h[#h], pt) do
            table.remove(h,#h)
        end
        table.insert(h,pt)
    end

    table.remove(h,#h)
    return h
end

-- main
local points = {
    {x=16,y= 3},{x=12,y=17},{x= 0,y= 6},{x=-4,y=-6},{x=16,y= 6},
    {x=16,y=-7},{x=16,y=-3},{x=17,y=-4},{x= 5,y=19},{x=19,y=-8},
    {x= 3,y=16},{x=12,y=13},{x= 3,y=-4},{x=17,y= 5},{x=-3,y=15},
    {x=-3,y=-9},{x= 0,y=11},{x=-9,y=-3},{x=-4,y=-2},{x=12,y=10}
}
local hull = convexHull(points)

io.write("Convex Hull: ")
print_points(hull)
print()
```

{{out}}

```txt
Convex Hull: [(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]
```


=={{header|Modula-2}}==

```modula2
MODULE ConvexHull;
FROM FormatString IMPORT FormatString;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM SYSTEM IMPORT TSIZE;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf);
END WriteInt;

TYPE
    Point = RECORD
        x, y : INTEGER;
    END;

PROCEDURE WritePoint(pt : Point);
BEGIN
    WriteString("(");
    WriteInt(pt.x);
    WriteString(", ");
    WriteInt(pt.y);
    WriteString(")");
END WritePoint;

TYPE
    NextNode = POINTER TO PNode;
    PNode = RECORD
        value : Point;
        next : NextNode;
    END;

PROCEDURE WriteNode(it : NextNode);
BEGIN
    IF it = NIL THEN
        RETURN
    END;
    WriteString("[");

    WritePoint(it^.value);
    it := it^.next;

    WHILE it # NIL DO
        WriteString(", ");
        WritePoint(it^.value);
        it := it^.next
    END;
    WriteString("]")
END WriteNode;

PROCEDURE AppendNode(pn : NextNode; p : Point) : NextNode;
VAR it,nx : NextNode;
BEGIN
    IF pn = NIL THEN
        ALLOCATE(it,TSIZE(PNode));
        it^.value := p;
        it^.next := NIL;
        RETURN it
    END;

    it := pn;
    WHILE it^.next # NIL DO
        it := it^.next
    END;

    ALLOCATE(nx,TSIZE(PNode));
    nx^.value := p;
    nx^.next := NIL;

    it^.next := nx;
    RETURN pn
END AppendNode;

PROCEDURE DeleteNode(VAR pn : NextNode);
BEGIN
    IF pn = NIL THEN RETURN END;
    DeleteNode(pn^.next);

    DEALLOCATE(pn,TSIZE(PNode));
    pn := NIL
END DeleteNode;

PROCEDURE SortNode(VAR pn : NextNode);
VAR
    it : NextNode;
    tmp : Point;
    done : BOOLEAN;
BEGIN
    REPEAT
        done := TRUE;
        it := pn;
        WHILE (it # NIL) AND (it^.next # NIL) DO
            IF it^.next^.value.x < it^.value.x THEN
                tmp := it^.value;
                it^.value := it^.next^.value;
                it^.next^.value := tmp;
                done := FALSE
            END;
            it := it^.next;
        END
    UNTIL done;
END SortNode;

PROCEDURE NodeLength(it : NextNode) : INTEGER;
VAR length : INTEGER;
BEGIN
    length := 0;
    WHILE it # NIL DO
        INC(length);
        it := it^.next;
    END;
    RETURN length
END NodeLength;

PROCEDURE ReverseNode(fp : NextNode) : NextNode;
VAR rp,tmp : NextNode;
BEGIN
    IF fp = NIL THEN RETURN NIL END;

    ALLOCATE(tmp,TSIZE(PNode));
    tmp^.value := fp^.value;
    tmp^.next := NIL;
    rp := tmp;
    fp := fp^.next;

    WHILE fp # NIL DO
        ALLOCATE(tmp,TSIZE(PNode));
        tmp^.value := fp^.value;
        tmp^.next := rp;
        rp := tmp;
        fp := fp^.next;
    END;

    RETURN rp
END ReverseNode;

(* ccw returns true if the three points make a counter-clockwise turn *)
PROCEDURE CCW(a,b,c : Point) : BOOLEAN;
BEGIN
    RETURN ((b.x - a.x) * (c.y - a.y)) > ((b.y - a.y) * (c.x - a.x))
END CCW;

PROCEDURE ConvexHull(p : NextNode) : NextNode;
VAR
    hull,it,h1,h2 : NextNode;
    t : INTEGER;
BEGIN
    IF p = NIL THEN RETURN NIL END;
    SortNode(p);
    hull := NIL;

    (* lower hull *)
    it := p;
    WHILE it # NIL DO
        IF hull # NIL THEN
            WHILE hull^.next # NIL DO
                (* At least two points in the list *)
                h2 := hull;
                h1 := hull^.next;
                WHILE h1^.next # NIL DO
                    h2 := h1;
                    h1 := h2^.next;
                END;

                IF CCW(h2^.value, h1^.value, it^.value) THEN
                    BREAK
                ELSE
                    h2^.next := NIL;
                    DeleteNode(h1);
                    h1 := NIL
                END
            END
        END;

        hull := AppendNode(hull, it^.value);
        it := it^.next;
    END;

    (* upper hull *)
    t := NodeLength(hull) + 1;
    p := ReverseNode(p);
    it := p;
    WHILE it # NIL DO
        WHILE NodeLength(hull) >= t DO
            h2 := hull;
            h1 := hull^.next;
            WHILE h1^.next # NIL DO
                h2 := h1;
                h1 := h2^.next;
            END;

            IF CCW(h2^.value, h1^.value, it^.value) THEN
                BREAK
            ELSE
                h2^.next := NIL;
                DeleteNode(h1);
                h1 := NIL
            END
        END;

        hull := AppendNode(hull, it^.value);
        it := it^.next;
    END;
    DeleteNode(p);

    h2 := hull;
    h1 := h2^.next;
    WHILE h1^.next # NIL DO
        h2 := h1;
        h1 := h1^.next;
    END;
    h2^.next := NIL;
    DeleteNode(h1);
    RETURN hull
END ConvexHull;

(* Main *)
VAR nodes,hull : NextNode;
BEGIN
    nodes := AppendNode(NIL, Point{16, 3});
    AppendNode(nodes, Point{12,17});
    AppendNode(nodes, Point{ 0, 6});
    AppendNode(nodes, Point{-4,-6});
    AppendNode(nodes, Point{16, 6});
    AppendNode(nodes, Point{16,-7});
    AppendNode(nodes, Point{16,-3});
    AppendNode(nodes, Point{17,-4});
    AppendNode(nodes, Point{ 5,19});
    AppendNode(nodes, Point{19,-8});
    AppendNode(nodes, Point{ 3,16});
    AppendNode(nodes, Point{12,13});
    AppendNode(nodes, Point{ 3,-4});
    AppendNode(nodes, Point{17, 5});
    AppendNode(nodes, Point{-3,15});
    AppendNode(nodes, Point{-3,-9});
    AppendNode(nodes, Point{ 0,11});
    AppendNode(nodes, Point{-9,-3});
    AppendNode(nodes, Point{-4,-2});
    AppendNode(nodes, Point{12,10});

    hull := ConvexHull(nodes);
    WriteNode(hull);
    DeleteNode(hull);

    DeleteNode(nodes);
    ReadChar
END ConvexHull.
```

{{out}}

```txt
[(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]
```



## Perl 6

{{works with|Rakudo|2017.05}}
{{trans|zkl}}
Modified the angle sort method as the original could fail if there were multiple points on the same y coordinate as the starting point. Sorts on tangent rather than triangle area. Inexpensive since it still doesn't do any trigonometric math, just calculates the ratio of opposite over adjacent. The original returned the correct answer for the task example, but only by accident. If the points (14,-9), (1,-9) were added to the task example, it wouldn't give a correct answer. Now it does.


```perl6
class Point {
    has Real $.x is rw;
    has Real $.y is rw;
    method gist { [~] '(', self.x,', ', self.y, ')' };
}

sub ccw (Point $a, Point $b, Point $c) {
    ($b.x - $a.x)*($c.y - $a.y) - ($b.y - $a.y)*($c.x - $a.x);
}

sub tangent (Point $a, Point $b) {
    my $opp = $b.x - $a.x;
    my $adj = $b.y - $a.y;
    $adj != 0 ?? $opp / $adj !! Inf
}

sub graham-scan (**@coords) {
    # sort points by y, secondary sort on x
    my @sp = @coords.map( { Point.new( :x($_[0]), :y($_[1]) ) } )
                    .sort: {.y, .x};

    # need at least 3 points to make a hull
    return @sp if +@sp < 3;

    # first point on hull is minimum y point
    my @h  = @sp.shift;

    # re-sort the points by angle, secondary on x
    @sp    = @sp.map( { $++ => [tangent(@h[0], $_), $_.x] } )
                .sort( {-$_.value[0], $_.value[1] } )
                .map: { @sp[$_.key] };

    # first point of re-sorted list is guaranteed to be on hull
    @h.push:  @sp.shift;

    # check through the remaining list making sure that
    # there is always a positive angle
    for @sp -> $point {
        if ccw( |@h.tail(2), $point ) >= 0 {
            @h.push: $point;
        } else {
            @h.pop;
            redo;
        }
    }
    @h
}

my @hull = graham-scan(
    (16, 3), (12,17), ( 0, 6), (-4,-6), (16, 6), (16,-7), (16,-3),
    (17,-4), ( 5,19), (19,-8), ( 3,16), (12,13), ( 3,-4), (17, 5),
    (-3,15), (-3,-9), ( 0,11), (-9,-3), (-4,-2), (12,10)
  );

say "Convex Hull ({+@hull} points): ", @hull;

@hull = graham-scan(
    (16, 3), (12,17), ( 0, 6), (-4,-6), (16, 6), (16,-7), (16,-3),
    (17,-4), ( 5,19), (19,-8), ( 3,16), (12,13), ( 3,-4), (17, 5),
    (-3,15), (-3,-9), ( 0,11), (-9,-3), (-4,-2), (12,10), (14,-9), (1,-9)
  );

say "Convex Hull ({+@hull} points): ", @hull;
```

{{out}}

```txt
Convex Hull (7 points): [(-3, -9) (19, -8) (17, 5) (12, 17) (5, 19) (-3, 15) (-9, -3)]
Convex Hull (9 points): [(-3, -9) (1, -9) (14, -9) (19, -8) (17, 5) (12, 17) (5, 19) (-3, 15) (-9, -3)]

```



## Phix

{{trans|C}}

```Phix
enum x, y
function ccw(sequence a, b, c)
    return (b[x] - a[x]) * (c[y] - a[y])
         > (b[y] - a[y]) * (c[x] - a[x])
end function

function convex_hull(sequence points)
    sequence h = {}
    points = sort(points)

    /* lower hull */
    for i=1 to length(points) do
        while length(h)>=2
          and not ccw(h[$-1], h[$], points[i]) do
            h = h[1..$-1]
        end while
        h = append(h, points[i])
    end for

    /* upper hull */
    for i=length(points) to 1 by -1 do
        while length(h)>=2
          and not ccw(h[$-1],h[$],points[i]) do
            h = h[1..$-1]
        end while
        h = append(h, points[i])
    end for

    h = h[1..$-1]
    return h
end function

constant points = {{16,  3}, {12, 17}, { 0,  6}, {-4, -6}, {16,  6},
                   {16, -7}, {16, -3}, {17, -4}, { 5, 19}, {19, -8},
                   { 3, 16}, {12, 13}, { 3, -4}, {17,   5}, {-3, 15},
                   {-3, -9}, { 0, 11}, {-9, -3}, {-4, -2}, {12, 10}}
printf(1,"Convex Hull: %v\n",{convex_hull(points)})
```

{{out}}

```txt

Convex Hull: {{-9,-3},{-3,-9},{19,-8},{17,5},{12,17},{5,19},{-3,15}}

```



## Python


An approach that uses the shapely library:


```python
from __future__ import print_function
from shapely.geometry import MultiPoint

if __name__=="__main__":
	pts = MultiPoint([(16,3), (12,17), (0,6), (-4,-6), (16,6), (16,-7), (16,-3), (17,-4), (5,19), (19,-8), (3,16), (12,13), (3,-4), (17,5), (-3,15), (-3,-9), (0,11), (-9,-3), (-4,-2), (12,10)])
	print (pts.convex_hull)
```


{{out}}

```txt
POLYGON ((-3 -9, -9 -3, -3 15, 5 19, 12 17, 17 5, 19 -8, -3 -9))
```



## Racket

Also an implementation of https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain (therefore kinda {{trans|Go}}


```racket
#lang typed/racket
(define-type Point (Pair Real Real))
(define-type Points (Listof Point))

(: ⊗ (Point Point Point -> Real))
(define/match (⊗ o a b)
  [((cons o.x o.y) (cons a.x a.y) (cons b.x b.y))
   (- (* (- a.x o.x) (- b.y o.y)) (* (- a.y o.y) (- b.x o.x)))])

(: Point<? (Point Point -> Boolean))
(define (Point<? a b)
  (cond [(< (car a) (car b)) #t] [(> (car a) (car b)) #f] [else (< (cdr a) (cdr b))]))

;; Input: a list P of points in the plane.
(define (convex-hull [P:unsorted : Points])
  ;; Sort the points of P by x-coordinate (in case of a tie, sort by y-coordinate).
  (define P (sort P:unsorted Point<?))
  ;; for i = 1, 2, ..., n:
  ;;     while L contains at least two points and the sequence of last two points
  ;;             of L and the point P[i] does not make a counter-clockwise turn:
  ;;        remove the last point from L
  ;;     append P[i] to L
  ;; TB: U is identical with (reverse P)
  (define (upper/lower-hull [P : Points])
    (reverse
     (for/fold ((rev : Points null))
       ((P.i (in-list P)))
       (let u/l : Points ((rev rev))
         (match rev
           [(list p-2 p-1 ps ...) #:when (not (positive? (⊗ p-2 P.i p-1))) (u/l (list* p-1 ps))]
           [(list ps ...) (cons P.i ps)])))))

  ;; Initialize U and L as empty lists.
  ;; The lists will hold the vertices of upper and lower hulls respectively.
  (let ((U (upper/lower-hull (reverse P)))
        (L (upper/lower-hull P)))
    ;; Remove the last point of each list (it's the same as the first point of the other list).
    ;; Concatenate L and U to obtain the convex hull of P.
    (append (drop-right L 1) (drop-right U 1)))) ; Points in the result will be listed in CCW order.)

(module+ test
  (require typed/rackunit)
  (check-equal?
   (convex-hull
    (list '(16 . 3) '(12 . 17) '(0 . 6) '(-4 . -6) '(16 . 6) '(16 . -7) '(16 . -3) '(17 . -4)
          '(5 . 19) '(19 . -8) '(3 . 16) '(12 . 13) '(3 . -4) '(17 . 5) '(-3 . 15) '(-3 . -9)
          '(0 . 11) '(-9 . -3) '(-4 . -2) '(12 . 10)))
   (list '(-9 . -3) '(-3 . -9) '(19 . -8) '(17 . 5) '(12 . 17) '(5 . 19) '(-3 . 15))))
```


{{out}}

silence implies tests pass (and output is as expected)


## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* Compute the Convex Hull for a set of points
* Format of the input file:
* (16,3) (12,17) (0,6) (-4,-6) (16,6) (16,-7) (16,-3) (17,-4) (5,19)
* (19,-8) (3,16) (12,13) (3,-4) (17,5) (-3,15) (-3,-9) (0,11) (-9,-3)
* (-4,-2)
*--------------------------------------------------------------------*/
  Signal On Novalue
  Signal On Syntax
Parse Arg fid
If fid='' Then Do
  fid='chullmin.in'                 /* miscellaneous test data       */
  fid='chullx.in'
  fid='chullt.in'
  fid='chulla.in'
  fid='chullxx.in'
  fid='sq.in'
  fid='tri.in'
  fid='line.in'
  fid='point.in'
  fid='chull.in'                    /* data from task description    */
  End
g.0debug=''
g.0oid=fn(fid)'.txt'; 'erase' g.0oid
x.=0
yl.=''
Parse Value '1000 -1000' With g.0xmin g.0xmax
Parse Value '1000 -1000' With g.0ymin g.0ymax
/*---------------------------------------------------------------------
* First read the input and store the points' coordinates
* x.0 contains the number of points, x.i contains the x.coordinate
* yl.x contains the y.coordinate(s) of points (x/y)
*--------------------------------------------------------------------*/
Do while lines(fid)>0
  l=linein(fid)
  Do While l<>''
    Parse Var l '(' x ',' y ')' l
    Call store x,y
    End
  End
Call lineout fid
Do i=1 To x.0                       /* loop over points              */
  x=x.i
  yl.x=sortv(yl.x)                  /* sort y-coordinates            */
  End
Call sho

/*---------------------------------------------------------------------
* Now we look for special border points:
* lefthigh and leftlow: leftmost points with higheste and lowest y
* ritehigh and ritelow: rightmost points with higheste and lowest y
* yl.x contains the y.coordinate(s) of points (x/y)
*--------------------------------------------------------------------*/
leftlow=0
lefthigh=0
Do i=1 To x.0
  x=x.i
  If maxv(yl.x)=g.0ymax Then Do
    If lefthigh=0 Then lefthigh=x'/'g.0ymax
    ritehigh=x'/'g.0ymax
    End
  If minv(yl.x)=g.0ymin Then Do
    ritelow=x'/'g.0ymin
    If leftlow=0 Then leftlow=x'/'g.0ymin
    End
  End
Call o 'lefthigh='lefthigh
Call o 'ritehigh='ritehigh
Call o 'ritelow ='ritelow
Call o 'leftlow ='leftlow
/*---------------------------------------------------------------------
* Now we look for special border points:
* leftmost_n and leftmost_s: points with lowest x and highest/lowest y
* ritemost_n and ritemost_s: points with largest x and highest/lowest y
* n and s stand foNorth and South, respectively
*--------------------------------------------------------------------*/
x=g.0xmi; leftmost_n=x'/'maxv(yl.x)
x=g.0xmi; leftmost_s=x'/'minv(yl.x)
x=g.0xma; ritemost_n=x'/'maxv(yl.x)
x=g.0xma; ritemost_s=x'/'minv(yl.x)

/*---------------------------------------------------------------------
* Now we compute the paths from ritehigh to ritelow (n_end)
* and leftlow to lefthigh (s_end), respectively
*--------------------------------------------------------------------*/
x=g.0xma
n_end=''
Do i=words(yl.x) To 1 By -1
  n_end=n_end x'/'word(yl.x,i)
  End
Call o 'n_end='n_end
x=g.0xmi
s_end=''
Do i=1 To words(yl.x)
  s_end=s_end x'/'word(yl.x,i)
  End
Call o 's_end='s_end

n_high=''
s_low=''
/*---------------------------------------------------------------------
* Now we compute the upper part of the convex hull (nhull)
*--------------------------------------------------------------------*/
Call o 'leftmost_n='leftmost_n
Call o 'lefthigh  ='lefthigh
nhull=leftmost_n
res=mk_nhull(leftmost_n,lefthigh);
nhull=nhull res
Call o 'A nhull='nhull
Do While res<>lefthigh
  res=mk_nhull(res,lefthigh); nhull=nhull res
  Call o 'B nhull='nhull
  End
res=mk_nhull(lefthigh,ritemost_n); nhull=nhull res
Call o 'C nhull='nhull
Do While res<>ritemost_n
  res=mk_nhull(res,ritemost_n); nhull=nhull res
  Call o 'D nhull='nhull
  End

nhull=nhull n_end                /* attach the right vertical border */

/*---------------------------------------------------------------------
* Now we compute the lower part of the convex hull (shull)
*--------------------------------------------------------------------*/
res=mk_shull(ritemost_s,ritelow);
shull=ritemost_s res
Call o 'A shull='shull
Do While res<>ritelow
  res=mk_shull(res,ritelow)
  shull=shull res
  Call o 'B shull='shull
  End
res=mk_shull(ritelow,leftmost_s)
shull=shull res
Call o 'C shull='shull
Do While res<>leftmost_s
  res=mk_shull(res,leftmost_s);
  shull=shull res
  Call o 'D shull='shull
  End

shull=shull s_end

chull=nhull shull                 /* concatenate upper and lower part */
                                  /* eliminate duplicates             */
                                  /* too lazy to take care before :-) */
Parse Var chull chullx chull
have.=0
have.chullx=1
Do i=1 By 1 While chull>''
  Parse Var chull xy chull
  If have.xy=0 Then Do
    chullx=chullx xy
    have.xy=1
    End
  End
                                   /* show the result                */
Say 'Points of convex hull in clockwise order:'
Say    chullx
/**********************************************************************
* steps that were necessary in previous attempts
/*---------------------------------------------------------------------
* Final polish: Insert points that are not yet in chullx but should be
* First on the upper hull going from left to right
*--------------------------------------------------------------------*/
i=1
Do While i<words(chullx)
  xya=word(chullx,i)  ; Parse Var xya xa '/' ya
  If xa=g.0xmax Then Leave
  xyb=word(chullx,i+1); Parse Var xyb xb '/' yb
  Do j=1 To x.0
    If x.j>xa Then Do
      If x.j<xb Then Do
        xx=x.j
        parse Value kdx(xya,xyb) With k d x
        If (k*xx+d)=maxv(yl.xx) Then Do
          chullx=subword(chullx,1,i) xx'/'maxv(yl.xx),
                                                    subword(chullx,i+1)
          i=i+1
          End
        End
      End
    Else
      i=i+1
    End
  End

Say    chullx

/*---------------------------------------------------------------------
* Final polish: Insert points that are not yet in chullx but should be
* Then on the lower hull going from right to left
*--------------------------------------------------------------------*/
i=wordpos(ritemost_s,chullx)
Do While i<words(chullx)
  xya=word(chullx,i)  ; Parse Var xya xa '/' ya
  If xa=g.0xmin Then Leave
  xyb=word(chullx,i+1); Parse Var xyb xb '/' yb
  Do j=x.0 To 1 By -1
    If x.j<xa Then Do
      If x.j>xb Then Do
        xx=x.j
        parse Value kdx(xya,xyb) With k d x
        If (k*xx+d)=minv(yl.xx) Then Do
          chullx=subword(chullx,1,i) xx'/'minv(yl.xx),
                                                    subword(chullx,i+1)
          i=i+1
          End
        End
      End
    Else
      i=i+1
    End
  End
Say chullx
**********************************************************************/
Call lineout g.0oid

Exit

store: Procedure Expose x. yl. g.
/*---------------------------------------------------------------------
* arrange the points in ascending order of x (in x.) and,
* for each x in ascending order of y (in yl.x)
* g.0xmin is the smallest x-value, etc.
* g.0xmi  is the x-coordinate
* g.0ymin is the smallest y-value, etc.
* g.0ymi  is the x-coordinate of such a point
*--------------------------------------------------------------------*/
  Parse Arg x,y
  Call o 'store' x y
  If x<g.0xmin Then Do; g.0xmin=x; g.0xmi=x; End
  If x>g.0xmax Then Do; g.0xmax=x; g.0xma=x; End
  If y<g.0ymin Then Do; g.0ymin=y; g.0ymi=x; End
  If y>g.0ymax Then Do; g.0ymax=y; g.0yma=x; End
  Do i=1 To x.0
    Select
      When x.i>x Then
        Leave
      When x.i=x Then Do
        yl.x=yl.x y
        Return
        End
      Otherwise
        Nop
      End
    End
  Do j=x.0 To i By -1
    ja=j+1
    x.ja=x.j
    End
  x.i=x
  yl.x=y
  x.0=x.0+1
  Return

sho: Procedure Expose x. yl. g.
  Do i=1 To x.0
    x=x.i
    say  format(i,2) 'x='format(x,3) 'yl='yl.x
    End
  Say ''
  Return

maxv: Procedure Expose g.
  Call trace 'O'
  Parse Arg l
  res=-1000
  Do While l<>''
    Parse Var l v l
    If v>res Then res=v
    End
  Return res

minv: Procedure Expose g.
  Call trace 'O'
  Parse Arg l
  res=1000
  Do While l<>''
    Parse Var l v l
    If v<res Then res=v
    End
  Return res

sortv: Procedure Expose g.
  Call trace 'O'
  Parse Arg l
  res=''
  Do Until l=''
    v=minv(l)
    res=res v
    l=remove(v,l)
    End
  Return space(res)

lastword: return word(arg(1),words(arg(1)))

kdx: Procedure  Expose xy. g.
/*---------------------------------------------------------------------
* Compute slope and y-displacement of a straight line
* that is defined by two points:  y=k*x+d
* Specialty; k='*' x=xa if xb=xa
*--------------------------------------------------------------------*/
  Call trace 'O'
  Parse Arg xya,xyb
  Parse Var xya xa '/' ya
  Parse Var xyb xb '/' yb
  If xa=xb Then
    Parse Value '*' '-' xa With k d x
  Else Do
    k=(yb-ya)/(xb-xa)
    d=yb-k*xb
    x='*'
    End
  Return k d x

remove:
/*---------------------------------------------------------------------
* Remove a specified element (e) from a given string (s)
*--------------------------------------------------------------------*/
  Parse Arg e,s
  Parse Var s sa (e) sb
  Return space(sa sb)

o: Procedure Expose g.
/*---------------------------------------------------------------------
* Write a line to the debug file
*--------------------------------------------------------------------*/
  If arg(2)=1 Then say arg(1)
  Return lineout(g.0oid,arg(1))

is_ok: Procedure Expose x. yl. g. sigl
/*---------------------------------------------------------------------
* Test if a given point (b) is above/on/or below a straight line
* defined by two points (a and c)
*--------------------------------------------------------------------*/
  Parse Arg a,b,c,op
  Call o    'is_ok' a b c op
  Parse Value kdx(a,c) With k d x
  Parse Var b x'/'y
  If op='U' Then y=maxv(yl.x)
            Else y=minv(yl.x)
  Call o    y x (k*x+d)
  If (abs(y-(k*x+d))<1.e-8) Then Return 0
  If op='U' Then res=(y<=(k*x+d))
            Else res=(y>=(k*x+d))
  Return res

mk_nhull: Procedure Expose x. yl. g.
/*---------------------------------------------------------------------
* Compute the upper (north) hull between two points (xya and xyb)
* Move x from xyb back to xya until all points within the current
* range (x and xyb) are BELOW the straight line defined xya and x
* Then make x the new starting point
*--------------------------------------------------------------------*/
  Parse Arg xya,xyb
  Call o 'mk_nhull' xya xyb
  If xya=xyb Then Return xya
  Parse Var xya xa '/' ya
  Parse Var xyb xb '/' yb
  iu=0
  iv=0
  Do xi=1 To x.0
    if x.xi>=xa & iu=0 Then iu=xi
    if x.xi<=xb Then iv=xi
    If x.xi>xb Then Leave
    End
  Call o    iu iv
  xu=x.iu
  xyu=xu'/'maxv(yl.xu)
  Do h=iv To iu+1 By -1 Until good
    Call o 'iv='iv,g.0debug
    Call o ' h='h,g.0debug
    xh=x.h
    xyh=xh'/'maxv(yl.xh)
    Call o    'Testing' xyu xyh,g.0debug
    good=1
    Do hh=h-1 To iu+1 By -1 While good
      xhh=x.hh
      xyhh=xhh'/'maxv(yl.xhh)
      Call o 'iu hh iv=' iu hh h,g.0debug
      If is_ok(xyu,xyhh,xyh,'U') Then Do
        Call o    xyhh 'is under' xyu xyh,g.0debug
        Nop
        End
      Else Do
        good=0
        Call o    xyhh 'is above' xyu xyh '-' xyh 'ist nicht gut'
        End
      End
    End
  Call o xyh 'is the one'

  Return xyh

p: Return
Say arg(1)
Pull  .
Return

mk_shull: Procedure Expose x. yl. g.
/*---------------------------------------------------------------------
* Compute the lower (south) hull between two points (xya and xyb)
* Move x from xyb back to xya until all points within the current
* range (x and xyb) are ABOVE the straight line defined xya and x
* Then make x the new starting point
*-----
---------------------------------------------------------------*/
  Parse Arg xya,xyb
  Call o 'mk_shull' xya xyb
  If xya=xyb Then Return xya
  Parse Var xya xa '/' ya
  Parse Var xyb xb '/' yb
  iu=0
  iv=0
  Do xi=x.0 To 1 By -1
    if x.xi<=xa & iu=0 Then iu=xi
    if x.xi>=xb Then iv=xi
    If x.xi<xb Then Leave
    End
  Call o iu iv '_' x.iu x.iv
  Call o 'mk_shull iv iu' iv iu
  xu=x.iu
  xyu=xu'/'minv(yl.xu)
  good=0
  Do h=iv To iu-1 Until good
    xh=x.h
    xyh=xh'/'minv(yl.xh)
    Call o    'Testing' xyu xyh   h iu
    good=1
    Do hh=h+1 To iu-1 While good
      Call o 'iu hh h=' iu hh h
      xhh=x.hh
      xyhh=xhh'/'minv(yl.xhh)
      If is_ok(xyu,xyhh,xyh,'O') Then Do
        Call o xyhh 'is above' xyu xyh
        Nop
        End
      Else Do
        Call o xyhh 'is under' xyu xyh '-' xyh 'ist nicht gut'
        good=0
        End
      End
    End
  Call o xyh 'is the one'
  Return xyh

Novalue:
  Say 'Novalue raised in line' sigl
  Say sourceline(sigl)
  Say 'Variable' condition('D')
  Signal lookaround

Syntax:
  Say 'Syntax raised in line' sigl
  Say sourceline(sigl)
  Say 'rc='rc '('errortext(rc)')'

halt:
lookaround:
  Say 'You can look around now.'
  Trace ?R
  Nop
  Exit 12
```

{{out}}

```txt
 1 x= -9 yl=-3
 2 x= -4 yl=-6 -2
 3 x= -3 yl=-9 15
 4 x=  0 yl=6 11
 5 x=  3 yl=-4 16
 6 x=  5 yl=19
 7 x= 12 yl=13 17
 8 x= 16 yl=-7 -3 3 6
 9 x= 17 yl=-4 5
10 x= 19 yl=-8

Points of convex hull in clockwise order:
-9/-3 -3/15 5/19 12/17 17/5 19/-8 -3/-9
```



### version 2

After learning from https://www.youtube.com/watch?v=wRTGDig3jx8

```rexx
/* REXX ---------------------------------------------------------------
* Compute the Convex Hull for a set of points
* Format of the input file:
* (16,3) (12,17) (0,6) (-4,-6) (16,6) (16,-7) (16,-3) (17,-4) (5,19)
* (19,-8) (3,16) (12,13) (3,-4) (17,5) (-3,15) (-3,-9) (0,11) (-9,-3)
* (-4,-2)
* Alternate (better) method using slopes
* 1) Compute path from lowest/leftmost to leftmost/lowest
* 2) Compute leftmost vertical border
* 3) Compute path from rightmost/highest to highest/rightmost
* 4) Compute path from highest/rightmost to rightmost/highest
* 5) Compute rightmost vertical border
* 6) Compute path from rightmost/lowest to lowest_leftmost point
*--------------------------------------------------------------------*/
Parse Arg fid
If fid='' Then Do
  fid='line.in'
  fid='point.in'
  fid='chullmin.in'                 /* miscellaneous test data       */
  fid='chullxx.in'
  fid='chullx.in'
  fid='chullt.in'
  fid='chulla.in'
  fid='sq.in'
  fid='tri.in'
  fid='z.in'
  fid='chull.in'                    /* data from task description    */
  End
g.0debug=''
g.0oid=fn(fid)'.txt'; 'erase' g.0oid
x.=0
yl.=''
Parse Value '1000 -1000' With g.0xmin g.0xmax
Parse Value '1000 -1000' With g.0ymin g.0ymax
/*---------------------------------------------------------------------
* First read the input and store the points' coordinates
* x.0 contains the number of points, x.i contains the x.coordinate
* yl.x contains the y.coordinate(s) of points (x/y)
*--------------------------------------------------------------------*/
Do while lines(fid)>0
  l=linein(fid)
  Do While l<>''
    Parse Var l '(' x ',' y ')' l
    Call store x,y
    End
  End
Call lineout fid
g.0xlist=''
Do i=1 To x.0                       /* loop over points              */
  x=x.i
  g.0xlist=g.0xlist x
  yl.x=sortv(yl.x)                  /* sort y-coordinates            */
  End
Call sho
If x.0<3 Then Do
  Say 'We need at least three points!'
  Exit
  End
Call o 'g.0xmin='g.0xmin
Call o 'g.0xmi ='g.0xmi
Call o 'g.0ymin='g.0ymin
Call o 'g.0ymi ='g.0ymi

Do i=1 To x.0
  x=x.i
  If minv(yl.x)=g.0ymin Then Leave
  End
lowest_leftmost=i

highest_rightmost=0
Do i=1 To x.0
  x=x.i
  If maxv(yl.x)=g.0ymax Then
    highest_rightmost=i
  If maxv(yl.x)<g.0ymax Then
    If highest_rightmost>0 Then
      Leave
  End
Call o 'lowest_leftmost='lowest_leftmost
Call o 'highest_rightmost  ='highest_rightmost

x=x.lowest_leftmost
Call o 'We start at' from x'/'minv(yl.x)
path=x'/'minv(yl.x)
/*---------------------------------------------------------------------
* 1) Compute path from lowest/leftmost to leftmost/lowest
*--------------------------------------------------------------------*/
Call min_path lowest_leftmost,1
/*---------------------------------------------------------------------
* 2) Compute leftmost vertical border
*--------------------------------------------------------------------*/
Do i=2 To words(yl.x)
  path=path x'/'word(yl.x,i)
  End
cxy=x'/'maxv(yl.x)
/*---------------------------------------------------------------------
* 3) Compute path from rightmost/highest to highest/rightmost
*--------------------------------------------------------------------*/
Call max_path ci,highest_rightmost
/*---------------------------------------------------------------------
* 4) Compute path from highest/rightmost to rightmost/highest
*--------------------------------------------------------------------*/
Call max_path ci,x.0
/*---------------------------------------------------------------------
* 5) Compute rightmost vertical border
*--------------------------------------------------------------------*/
Do i=words(yl.x)-1 To 1 By -1
  cxy=x'/'word(yl.x,i)
  path=path cxy
  End
/*---------------------------------------------------------------------
* 6) Compute path from rightmost/lowest to lowest_leftmost
*--------------------------------------------------------------------*/
Call min_path ci,lowest_leftmost

Parse Var path pathx path
have.=0
Do i=1 By 1 While path>''
  Parse Var path xy path
  If have.xy=0 Then Do
    pathx=pathx xy
    have.xy=1
    End
  End
Say 'Points of convex hull in clockwise order:'
Say pathx
Call lineout g.0oid
Exit

min_path:
  Parse Arg from,tgt
  ci=from
  cxy=x.ci
  Do Until ci=tgt
    kmax=-1000
    Do i=ci-1 To 1 By sign(tgt-from)
      x=x.i
      k=k(cxy'/'minv(yl.cxy),x'/'minv(yl.x))
      If k>kmax Then Do
        kmax=k
        ii=i
        End
      End
    ci=ii
    cxy=x.ii
    path=path cxy'/'minv(yl.cxy)
    End
  Return

max_path:
  Parse Arg from,tgt
  Do Until ci=tgt
    kmax=-1000
    Do i=ci+1 To tgt
      x=x.i
      k=k(cxy,x'/'maxv(yl.x))
      If k>kmax Then Do
        kmax=k
        ii=i
        End
      End
    x=x.ii
    cxy=x'/'maxv(yl.x)
    path=path cxy
    ci=ii
    End
  Return

store: Procedure Expose x. yl. g.
/*---------------------------------------------------------------------
* arrange the points in ascending order of x (in x.) and,
* for each x in ascending order of y (in yl.x)
* g.0xmin is the smallest x-value, etc.
* g.0xmi  is the x-coordinate
* g.0ymin is the smallest y-value, etc.
* g.0ymi  is the x-coordinate of such a point
*--------------------------------------------------------------------*/
  Parse Arg x,y
  Call o 'store' x y
  If x<g.0xmin Then Do; g.0xmin=x; g.0xmi=x; End
  If x>g.0xmax Then Do; g.0xmax=x; g.0xma=x; End
  If y<g.0ymin Then Do; g.0ymin=y; g.0ymi=x; End
  If y>g.0ymax Then Do; g.0ymax=y; g.0yma=x; End
  Do i=1 To x.0
    Select
      When x.i>x Then
        Leave
      When x.i=x Then Do
        yl.x=yl.x y
        Return
        End
      Otherwise
        Nop
      End
    End
  Do j=x.0 To i By -1
    ja=j+1
    x.ja=x.j
    End
  x.i=x
  yl.x=y
  x.0=x.0+1
  Return

sho: Procedure Expose x. yl. g.
  Do i=1 To x.0
    x=x.i
    say  format(i,2) 'x='format(x,3) 'yl='yl.x
    End
  Say ''
  Return

maxv: Procedure Expose g.
  Call trace 'O'
  Parse Arg l
  res=-1000
  Do While l<>''
    Parse Var l v l
    If v>res Then res=v
    End
  Return res

minv: Procedure Expose g.
  Call trace 'O'
  Parse Arg l
  res=1000
  Do While l<>''
    Parse Var l v l
    If v<res Then res=v
    End
  Return res

sortv: Procedure Expose g.
  Call trace 'O'
  Parse Arg l
  res=''
  Do Until l=''
    v=minv(l)
    res=res v
    l=remove(v,l)
    End
  Return space(res)

lastword: return word(arg(1),words(arg(1)))

k: Procedure
/*---------------------------------------------------------------------
* Compute slope of a straight line
* that is defined by two points:  y=k*x+d
* Specialty; k='*' x=xa if xb=xa
*--------------------------------------------------------------------*/
  Call trace 'O'
  Parse Arg xya,xyb
  Parse Var xya xa '/' ya
  Parse Var xyb xb '/' yb
  If xa=xb Then
    k='*'
  Else
    k=(yb-ya)/(xb-xa)
  Return k

remove:
/*---------------------------------------------------------------------
* Remove a specified element (e) from a given string (s)
*--------------------------------------------------------------------*/
  Parse Arg e,s
  Parse Var s sa (e) sb
  Return space(sa sb)

o: Procedure Expose g.
/*---------------------------------------------------------------------
* Write a line to the debug file
*--------------------------------------------------------------------*/
  If arg(2)=1 Then say arg(1)
  Return lineout(g.0oid,arg(1))
```

{{out}}

```txt
 1 x= -9 yl=-3
 2 x= -4 yl=-6 -2
 3 x= -3 yl=-9 15
 4 x=  0 yl=6 11
 5 x=  3 yl=-4 16
 6 x=  5 yl=19
 7 x= 12 yl=13 17
 8 x= 16 yl=-7 -3 3 6
 9 x= 17 yl=-4 5
10 x= 19 yl=-8

Points of convex hull in clockwise order:
-3/-9 -9/-3 -3/15 5/19 12/17 17/5 19/-8 -3/-9
```


## Rust

Calculates convex hull from list of points (f32, f32). This can be executed entirely in the Rust Playground.

```Rust

#[derive(Debug, Clone)]
struct Point {
    x: f32,
    y: f32
}

fn calculate_convex_hull(points: &Vec<Point>) -> Vec<Point> {
    //There must be at least 3 points
    if points.len() < 3 { return points.clone(); }

    let mut hull = vec![];

    //Find the left most point in the polygon
    let (left_most_idx, _) = points.iter()
        .enumerate()
        .min_by(|lhs, rhs| lhs.1.x.partial_cmp(&rhs.1.x).unwrap())
        .expect("No left most point");


    let mut p = left_most_idx;
    let mut q = 0_usize;

    loop {
        //The left most point must be part of the hull
        hull.push(points[p].clone());

        q = (p + 1) % points.len();

        for i in 0..points.len() {
            if orientation(&points[p], &points[i], &points[q]) == 2 {
                q = i;
            }
        }

        p = q;

        //Break from loop once we reach the first point again
        if p == left_most_idx { break; }
    }

    return hull;
}

//Calculate orientation for 3 points
//0 -> Straight line
//1 -> Clockwise
//2 -> Counterclockwise
fn orientation(p: &Point, q: &Point, r: &Point) -> usize {
    let val = (q.y - p.y) * (r.x - q.x) -
        (q.x - p.x) * (r.y - q.y);

    if val == 0. { return 0 };
    if val > 0. { return 1; } else { return 2; }
}

fn main(){
    let points = vec![pt(16,3), pt(12,17), pt(0,6), pt(-4,-6), pt(16,6), pt(16,-7), pt(16,-3), pt(17,-4), pt(5,19), pt(19,-8), pt(3,16), pt(12,13), pt(3,-4), pt(17,5), pt(-3,15), pt(-3,-9), pt(0,11), pt(-9,-3), pt(-4,-2), pt(12,10)];
    let hull = calculate_convex_hull(&points);

    hull.iter()
        .for_each(|pt| println!("{:?}", pt));
}

fn pt(x: i32, y: i32) -> Point {
    return Point {x:x as f32, y:y as f32};
}

```

{{out}}

```txt

Point { x: -9.0, y: -3.0 }
Point { x: -3.0, y: -9.0 }
Point { x: 19.0, y: -8.0 }
Point { x: 17.0, y: 5.0 }
Point { x: 12.0, y: 17.0 }
Point { x: 5.0, y: 19.0 }
Point { x: -3.0, y: 15.0 }

```


## Scala

Scala Implementation to find Convex hull of given points collection. Functional Paradigm followed

```Scala

object convex_hull{
    def get_hull(points:List[(Double,Double)], hull:List[(Double,Double)]):List[(Double,Double)] = points match{
        case Nil  =>            join_tail(hull,hull.size -1)
        case head :: tail =>    get_hull(tail,reduce(head::hull))
    }
    def reduce(hull:List[(Double,Double)]):List[(Double,Double)] = hull match{
        case p1::p2::p3::rest => {
            if(check_point(p1,p2,p3))      hull
            else                           reduce(p1::p3::rest)
        }
        case _ =>                          hull
    }
    def check_point(pnt:(Double,Double), p2:(Double,Double),p1:(Double,Double)): Boolean = {
        val (x,y) = (pnt._1,pnt._2)
        val (x1,y1) = (p1._1,p1._2)
        val (x2,y2) = (p2._1,p2._2)
        ((x-x1)*(y2-y1) - (x2-x1)*(y-y1)) <= 0
    }
    def m(p1:(Double,Double), p2:(Double,Double)):Double = {
        if(p2._1 == p1._1 && p1._2>p2._2)       90
        else if(p2._1 == p1._1 && p1._2<p2._2)  -90
        else if(p1._1<p2._1)                    180 - Math.toDegrees(Math.atan(-(p1._2 - p2._2)/(p1._1 - p2._1)))
        else                                    Math.toDegrees(Math.atan((p1._2 - p2._2)/(p1._1 - p2._1)))
    }
    def join_tail(hull:List[(Double,Double)],len:Int):List[(Double,Double)] = {
        if(m(hull(len),hull(0)) > m(hull(len-1),hull(0)))   join_tail(hull.slice(0,len),len-1)
        else                                                hull
    }
    def main(args:Array[String]){
        val points = List[(Double,Double)]((16,3), (12,17), (0,6), (-4,-6), (16,6), (16,-7), (16,-3), (17,-4), (5,19), (19,-8), (3,16), (12,13), (3,-4), (17,5), (-3,15), (-3,-9), (0,11), (-9,-3), (-4,-2), (12,10))
        val sorted_points = points.sortWith(m(_,(0.0,0.0)) < m(_,(0.0,0.0)))
        println(f"Points:\n" + points + f"\n\nConvex Hull :\n" +get_hull(sorted_points,List[(Double,Double)]()))
    }
}

```

{{out}}

```txt

Points:
List((16.0,3.0), (12.0,17.0), (0.0,6.0), (-4.0,-6.0), (16.0,6.0), (16.0,-7.0), (16.0,-3.0), (17.0,-4.0), (5.0,19.0), (19.0,-8.0), (3.0,16.0), (12.0,13.0), (3.0,-4.0), (17.0,5.0), (-3.0,15.0), (-3.0,-9.0), (0.0,11.0), (-9.0,-3.0), (-4.0,-2.0), (12.0,10.0))

Convex Hull :
List((-3.0,-9.0), (-9.0,-3.0), (-3.0,15.0), (5.0,19.0), (12.0,17.0), (17.0,5.0), (19.0,-8.0))

```



## Sidef

{{trans|Perl 6}}

```ruby
class Point(Number x, Number y) {
    method to_s {
        "(#{x}, #{y})"
    }
}

func ccw (Point a, Point b, Point c) {
    (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)
}

func tangent (Point a, Point b) {
    (b.x - a.x) / (b.y - a.y)
}

func graham_scan (*coords) {

    ## sort points by y, secondary sort on x
    var sp = coords.map { |a|
        Point(a...)
    }.sort { |a,b|
        (a.y <=> b.y) ||
        (a.x <=> b.x)
    }

    # need at least 3 points to make a hull
    if (sp.len < 3) {
        return sp
    }

    # first point on hull is minimum y point
    var h = [sp.shift]

    # re-sort the points by angle, secondary on x
    sp = sp.map_kv { |k,v|
        Pair(k, [tangent(h[0], v), v.x])
    }.sort { |a,b|
        (b.value[0] <=> a.value[0]) ||
        (a.value[1] <=> b.value[1])
    }.map { |a|
        sp[a.key]
    }

    # first point of re-sorted list is guaranteed to be on hull
    h << sp.shift

    # check through the remaining list making sure that
    # there is always a positive angle
    sp.each { |point|
        loop {
            if (ccw(h.last(2)..., point) >= 0) {
                h << point
                break
            } else {
                h.pop
            }
        }
    }

    return h
}

var hull = graham_scan(
    [16, 3], [12,17], [ 0, 6], [-4,-6], [16, 6], [16,-7], [16,-3],
    [17,-4], [ 5,19], [19,-8], [ 3,16], [12,13], [ 3,-4], [17, 5],
    [-3,15], [-3,-9], [ 0,11], [-9,-3], [-4,-2], [12,10])

say("Convex Hull (#{hull.len} points): ", hull.join(" "))

hull = graham_scan(
    [16, 3], [12,17], [ 0, 6], [-4,-6], [16, 6], [16,-7], [16,-3],
    [17,-4], [ 5,19], [19,-8], [ 3,16], [12,13], [ 3,-4], [17, 5],
    [-3,15], [-3,-9], [ 0,11], [-9,-3], [-4,-2], [12,10], [14,-9], [1,-9])

say("Convex Hull (#{hull.len} points): ", hull.join(" "))
```

{{out}}

```txt

Convex Hull (7 points): (-3, -9) (19, -8) (17, 5) (12, 17) (5, 19) (-3, 15) (-9, -3)
Convex Hull (9 points): (-3, -9) (1, -9) (14, -9) (19, -8) (17, 5) (12, 17) (5, 19) (-3, 15) (-9, -3)

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports ConvexHull

Module Module1

    Class Point : Implements IComparable(Of Point)
        Public Property X As Integer
        Public Property Y As Integer

        Public Sub New(x As Integer, y As Integer)
            Me.X = x
            Me.Y = y
        End Sub

        Public Function CompareTo(other As Point) As Integer Implements IComparable(Of Point).CompareTo
            Return X.CompareTo(other.X)
        End Function

        Public Overrides Function ToString() As String
            Return String.Format("({0}, {1})", X, Y)
        End Function
    End Class

    Function ConvexHull(p As List(Of Point)) As List(Of Point)
        If p.Count = 0 Then
            Return New List(Of Point)
        End If
        p.Sort()
        Dim h As New List(Of Point)

        ' Lower hull
        For Each pt In p
            While h.Count >= 2 AndAlso Not Ccw(h(h.Count - 2), h(h.Count - 1), pt)
                h.RemoveAt(h.Count - 1)
            End While
            h.Add(pt)
        Next

        ' Upper hull
        Dim t = h.Count + 1
        For i = p.Count - 1 To 0 Step -1
            Dim pt = p(i)
            While h.Count >= t AndAlso Not Ccw(h(h.Count - 2), h(h.Count - 1), pt)
                h.RemoveAt(h.Count - 1)
            End While
            h.Add(pt)
        Next

        h.RemoveAt(h.Count - 1)
        Return h
    End Function

    Function Ccw(a As Point, b As Point, c As Point) As Boolean
        Return ((b.X - a.X) * (c.Y - a.Y)) > ((b.Y - a.Y) * (c.X - a.X))
    End Function

    Sub Main()
        Dim points As New List(Of Point) From {
            New Point(16, 3),
            New Point(12, 17),
            New Point(0, 6),
            New Point(-4, -6),
            New Point(16, 6),
            New Point(16, -7),
            New Point(16, -3),
            New Point(17, -4),
            New Point(5, 19),
            New Point(19, -8),
            New Point(3, 16),
            New Point(12, 13),
            New Point(3, -4),
            New Point(17, 5),
            New Point(-3, 15),
            New Point(-3, -9),
            New Point(0, 11),
            New Point(-9, -3),
            New Point(-4, -2),
            New Point(12, 10)
        }

        Dim hull = ConvexHull(points)
        Dim it = hull.GetEnumerator()
        Console.Write("Convex Hull: [")
        If it.MoveNext() Then
            Console.Write(it.Current)
        End If
        While it.MoveNext()
            Console.Write(", {0}", it.Current)
        End While
        Console.WriteLine("]")
    End Sub

End Module
```

{{out}}

```txt
Convex Hull: [(-9, -3), (-3, -9), (19, -8), (17, 5), (12, 17), (5, 19), (-3, 15)]
```



## zkl


```zkl
// Use Graham Scan to sort points into a convex hull
// https://en.wikipedia.org/wiki/Graham_scan, O(n log n)
// http://www.geeksforgeeks.org/convex-hull-set-2-graham-scan/
// http://geomalgorithms.com/a10-_hull-1.html
fcn grahamScan(points){
   N:=points.len();
   # find the point with the lowest y-coordinate, x is tie breaker
   p0:=points.reduce(fcn([(a,b)]ab,[(x,y)]xy){
	if(b<y)ab else if(b==y and a<x)ab else xy });
   #sort points by polar angle with p0, ie ccw from p0
   points.sort('wrap(p1,p2){ ccw(p0,p1,p2)>0 });

   # We want points[0] to be a sentinel point that will stop the loop.
   points.insert(0,points[-1]);
   M:=1; # M will denote the number of points on the convex hull.
   foreach i in ([2..N]){
      # Find next valid point on convex hull.
      while(ccw(points[M-1], points[M], points[i])<=0){
	 if(M>1) M-=1;
	 else if(i==N) break;  # All points are collinear
	 else i+=1;
      }
      points.swap(M+=1,i); # Update M and swap points[i] to the correct place.
   }
   points[0,M]
}
# Three points are a counter-clockwise turn if ccw > 0, clockwise if
# ccw < 0, and collinear if ccw = 0 because ccw is a determinant that
# gives twice the signed  area of the triangle formed by p1, p2 and p3.
fcn ccw(a,b,c){  // a,b,c are points: (x,y)
   ((b[0] - a[0])*(c[1] - a[1])) - ((b[1] - a[1])*(c[0] - a[0]))
}
```


```zkl
pts:=List( T(16,3), T(12,17), T(0,6), T(-4,-6), T(16,6),
	   T(16, -7), T(16,-3),T(17,-4), T(5,19), T(19,-8),
	   T(3,16), T(12,13), T(3,-4), T(17,5), T(-3,15),
	   T(-3,-9), T(0,11), T(-9,-3), T(-4,-2), T(12,10), )
	     .apply(fcn(xy){ xy.apply("toFloat") }).copy();
hull:=grahamScan(pts);
println("Convex Hull (%d points): %s".fmt(hull.len(),hull.toString(*)));
```

{{out}}

```txt

Convex Hull (7 points): L(L(-3,-9),L(19,-8),L(17,5),L(12,17),L(5,19),L(-3,15),L(-9,-3))

```


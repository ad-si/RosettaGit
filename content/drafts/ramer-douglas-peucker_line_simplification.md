+++
title = "Ramer-Douglas-Peucker line simplification"
description = ""
date = 2019-08-19T20:11:35Z
aliases = []
[extra]
id = 21223
[taxonomies]
categories = []
tags = []
+++

[[Category:Geometry]]
[[Category:Recursion]]

{{task}}
The   '''Ramer–Douglas–Peucker'''   algorithm is a line simplification algorithm for reducing the number of points used to define its shape.


;Task:
Using the   '''Ramer–Douglas–Peucker'''   algorithm, simplify the   2D   line defined by the points:
    (0,0)  (1,0.1)  (2,-0.1)  (3,5)  (4,6)  (5,7)  (6,8.1)  (7,9)  (8,9)  (9,9)

The error threshold to be used is:   '''1.0'''.

Display the remaining points here.


;Reference:
:*   the Wikipedia article:   [https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm Ramer-Douglas-Peucker algorithm].





## C++



```cpp
#include <iostream>
#include <cmath>
#include <utility>
#include <vector>
#include <stdexcept>
using namespace std;

typedef std::pair<double, double> Point;

double PerpendicularDistance(const Point &pt, const Point &lineStart, const Point &lineEnd)
{
	double dx = lineEnd.first - lineStart.first;
	double dy = lineEnd.second - lineStart.second;

	//Normalise
	double mag = pow(pow(dx,2.0)+pow(dy,2.0),0.5);
	if(mag > 0.0)
	{
		dx /= mag; dy /= mag;
	}

	double pvx = pt.first - lineStart.first;
	double pvy = pt.second - lineStart.second;

	//Get dot product (project pv onto normalized direction)
	double pvdot = dx * pvx + dy * pvy;

	//Scale line direction vector
	double dsx = pvdot * dx;
	double dsy = pvdot * dy;

	//Subtract this from pv
	double ax = pvx - dsx;
	double ay = pvy - dsy;

	return pow(pow(ax,2.0)+pow(ay,2.0),0.5);
}

void RamerDouglasPeucker(const vector<Point> &pointList, double epsilon, vector<Point> &out)
{
	if(pointList.size()<2)
		throw invalid_argument("Not enough points to simplify");

	// Find the point with the maximum distance from line between start and end
	double dmax = 0.0;
	size_t index = 0;
	size_t end = pointList.size()-1;
	for(size_t i = 1; i < end; i++)
	{
		double d = PerpendicularDistance(pointList[i], pointList[0], pointList[end]);
		if (d > dmax)
		{
			index = i;
			dmax = d;
		}
	}

	// If max distance is greater than epsilon, recursively simplify
	if(dmax > epsilon)
	{
		// Recursive call
		vector<Point> recResults1;
		vector<Point> recResults2;
		vector<Point> firstLine(pointList.begin(), pointList.begin()+index+1);
		vector<Point> lastLine(pointList.begin()+index, pointList.end());
		RamerDouglasPeucker(firstLine, epsilon, recResults1);
		RamerDouglasPeucker(lastLine, epsilon, recResults2);

		// Build the result list
		out.assign(recResults1.begin(), recResults1.end()-1);
		out.insert(out.end(), recResults2.begin(), recResults2.end());
		if(out.size()<2)
			throw runtime_error("Problem assembling output");
	}
	else
	{
		//Just return start and end points
		out.clear();
		out.push_back(pointList[0]);
		out.push_back(pointList[end]);
	}
}

int main()
{
	vector<Point> pointList;
	vector<Point> pointListOut;

	pointList.push_back(Point(0.0, 0.0));
	pointList.push_back(Point(1.0, 0.1));
	pointList.push_back(Point(2.0, -0.1));
	pointList.push_back(Point(3.0, 5.0));
	pointList.push_back(Point(4.0, 6.0));
	pointList.push_back(Point(5.0, 7.0));
	pointList.push_back(Point(6.0, 8.1));
	pointList.push_back(Point(7.0, 9.0));
	pointList.push_back(Point(8.0, 9.0));
	pointList.push_back(Point(9.0, 9.0));

	RamerDouglasPeucker(pointList, 1.0, pointListOut);

	cout << "result" << endl;
	for(size_t i=0;i< pointListOut.size();i++)
	{
		cout << pointListOut[i].first << "," << pointListOut[i].second << endl;
	}

	return 0;
}
```


{{out}}

```txt
result
0,0
2,-0.1
3,5
7,9
9,9
```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace LineSimplification {
    using Point = Tuple<double, double>;

    class Program {
        static double PerpendicularDistance(Point pt, Point lineStart, Point lineEnd) {
            double dx = lineEnd.Item1 - lineStart.Item1;
            double dy = lineEnd.Item2 - lineStart.Item2;

            // Normalize
            double mag = Math.Sqrt(dx * dx + dy * dy);
            if (mag > 0.0) {
                dx /= mag;
                dy /= mag;
            }
            double pvx = pt.Item1 - lineStart.Item1;
            double pvy = pt.Item2 - lineStart.Item2;

            // Get dot product (project pv onto normalized direction)
            double pvdot = dx * pvx + dy * pvy;

            // Scale line direction vector and subtract it from pv
            double ax = pvx - pvdot * dx;
            double ay = pvy - pvdot * dy;

            return Math.Sqrt(ax * ax + ay * ay);
        }

        static void RamerDouglasPeucker(List<Point> pointList, double epsilon, List<Point> output) {
            if (pointList.Count < 2) {
                throw new ArgumentOutOfRangeException("Not enough points to simplify");
            }

            // Find the point with the maximum distance from line between the start and end
            double dmax = 0.0;
            int index = 0;
            int end = pointList.Count - 1;
            for (int i = 1; i < end; ++i) {
                double d = PerpendicularDistance(pointList[i], pointList[0], pointList[end]);
                if (d > dmax) {
                    index = i;
                    dmax = d;
                }
            }

            // If max distance is greater than epsilon, recursively simplify
            if (dmax > epsilon) {
                List<Point> recResults1 = new List<Point>();
                List<Point> recResults2 = new List<Point>();
                List<Point> firstLine = pointList.Take(index + 1).ToList();
                List<Point> lastLine = pointList.Skip(index).ToList();
                RamerDouglasPeucker(firstLine, epsilon, recResults1);
                RamerDouglasPeucker(lastLine, epsilon, recResults2);

                // build the result list
                output.AddRange(recResults1.Take(recResults1.Count - 1));
                output.AddRange(recResults2);
                if (output.Count < 2) throw new Exception("Problem assembling output");
            }
            else {
                // Just return start and end points
                output.Clear();
                output.Add(pointList[0]);
                output.Add(pointList[pointList.Count - 1]);
            }
        }

        static void Main(string[] args) {
            List<Point> pointList = new List<Point>() {
                new Point(0.0,0.0),
                new Point(1.0,0.1),
                new Point(2.0,-0.1),
                new Point(3.0,5.0),
                new Point(4.0,6.0),
                new Point(5.0,7.0),
                new Point(6.0,8.1),
                new Point(7.0,9.0),
                new Point(8.0,9.0),
                new Point(9.0,9.0),
            };
            List<Point> pointListOut = new List<Point>();
            RamerDouglasPeucker(pointList, 1.0, pointListOut);
            Console.WriteLine("Points remaining after simplification:");
            pointListOut.ForEach(p => Console.WriteLine(p));
        }
    }
}
```

{{out}}

```txt
Points remaining after simplification:
(0, 0)
(2, -0.1)
(3, 5)
(7, 9)
(9, 9)
```



## D

{{trans|C++}}

```D
import std.algorithm;
import std.exception : enforce;
import std.math;
import std.stdio;

void main() {
    creal[] pointList = [
        0.0 +  0.0i,
        1.0 +  0.1i,
        2.0 + -0.1i,
        3.0 +  5.0i,
        4.0 +  6.0i,
        5.0 +  7.0i,
        6.0 +  8.1i,
        7.0 +  9.0i,
        8.0 +  9.0i,
        9.0 +  9.0i
    ];
    creal[] pointListOut;

    ramerDouglasPeucker(pointList, 1.0, pointListOut);

    writeln("result");
    for (size_t i=0; i< pointListOut.length; i++) {
        writeln(pointListOut[i].re, ",", pointListOut[i].im);
    }
}

real perpendicularDistance(const creal pt, const creal lineStart, const creal lineEnd) {
    creal d = lineEnd - lineStart;

    //Normalise
    real mag =  hypot(d.re, d.im);
    if (mag > 0.0) {
        d /= mag;
    }

    creal pv = pt - lineStart;

    //Get dot product (project pv onto normalized direction)
    real pvdot = d.re * pv.re + d.im * pv.im;

    //Scale line direction vector
    creal ds = pvdot * d;

    //Subtract this from pv
    creal a = pv - ds;

    return hypot(a.re, a.im);
}

void ramerDouglasPeucker(const creal[] pointList, real epsilon, ref creal[] output) {
    enforce(pointList.length >= 2, "Not enough points to simplify");

    // Find the point with the maximum distance from line between start and end
    real dmax = 0.0;
    size_t index = 0;
    size_t end = pointList.length-1;
    for (size_t i=1; i<end; i++) {
        real d = perpendicularDistance(pointList[i], pointList[0], pointList[end]);
        if (d > dmax) {
            index = i;
            dmax = d;
        }
    }

    // If max distance is greater than epsilon, recursively simplify
    if (dmax > epsilon) {
        // Recursive call
        creal[] firstLine = pointList[0..index+1].dup;
        creal[] lastLine = pointList[index+1..$].dup;

        creal[] recResults1;
        ramerDouglasPeucker(firstLine, epsilon, recResults1);

        creal[] recResults2;
        ramerDouglasPeucker(lastLine, epsilon, recResults2);

        // Build the result list
        output = recResults1 ~ recResults2;

        enforce(output.length>=2, "Problem assembling output");
    } else {
        //Just return start and end points
        output.length = 0;
        output ~= pointList[0];
        output ~= pointList[end];
    }
}
```


{{out}}

```txt
result
0,0
2,-0.1
3,5
7,9
9,9
```



## Go


```go
package main

import (
    "fmt"
    "math"
)

type point struct{ x, y float64 }

func RDP(l []point, ε float64) []point {
    x := 0
    dMax := -1.
    last := len(l) - 1
    p1 := l[0]
    p2 := l[last]
    x21 := p2.x - p1.x
    y21 := p2.y - p1.y
    for i, p := range l[1:last] {
        if d := math.Abs(y21*p.x - x21*p.y + p2.x*p1.y - p2.y*p1.x); d > dMax {
            x = i + 1
            dMax = d
        }
    }
    if dMax > ε {
        return append(RDP(l[:x+1], ε), RDP(l[x:], ε)[1:]...)
    }
    return []point{l[0], l[len(l)-1]}
}

func main() {
    fmt.Println(RDP([]point{{0, 0}, {1, 0.1}, {2, -0.1},
        {3, 5}, {4, 6}, {5, 7}, {6, 8.1}, {7, 9}, {8, 9}, {9, 9}}, 1))
}
```

{{out}}

```txt

[{0 0} {2 -0.1} {3 5} {7 9} {9 9}]

```



## J

'''Solution:'''

```j
mp=: +/ .*           NB. matrix product
norm=: +/&.:*:       NB. vector norm
normalize=: (% norm)^:(0 < norm)

dxy=. normalize@({: - {.)
pv=. -"1 {.
NB.*perpDist v Calculate perpendicular distance of points from a line
perpDist=: norm"1@(pv ([ -"1 mp"1~ */ ]) dxy) f.

rdp=: verb define
  1 rdp y
  :
  points=. ,:^:(2 > #@$) y
  epsilon=. x
  if. 2 > # points do. points return. end.

  NB. point with the maximum distance from line between start and end
  'imax dmax'=. ((i. , ]) >./) perpDist points
  if. dmax > epsilon do.
    epsilon ((}:@rdp (1+imax)&{.) , (rdp imax&}.)) points
  else.
    ({. ,: {:) points
  end.
)
```

'''Example Usage:'''

```j
   Points=: 0 0,1 0.1,2 _0.1,3 5,4 6,5 7,6 8.1,7 9,8 9,:9 9
   1.0 rdp Points
0    0
2 _0.1
3    5
7    9
9    9
```



## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.List;

public class LineSimplification {
    private static class Point extends Pair<Double, Double> {
        Point(Double key, Double value) {
            super(key, value);
        }

        @Override
        public String toString() {
            return String.format("(%f, %f)", getKey(), getValue());
        }
    }

    private static double perpendicularDistance(Point pt, Point lineStart, Point lineEnd) {
        double dx = lineEnd.getKey() - lineStart.getKey();
        double dy = lineEnd.getValue() - lineStart.getValue();

        // Normalize
        double mag = Math.hypot(dx, dy);
        if (mag > 0.0) {
            dx /= mag;
            dy /= mag;
        }
        double pvx = pt.getKey() - lineStart.getKey();
        double pvy = pt.getValue() - lineStart.getValue();

        // Get dot product (project pv onto normalized direction)
        double pvdot = dx * pvx + dy * pvy;

        // Scale line direction vector and subtract it from pv
        double ax = pvx - pvdot * dx;
        double ay = pvy - pvdot * dy;

        return Math.hypot(ax, ay);
    }

    private static void ramerDouglasPeucker(List<Point> pointList, double epsilon, List<Point> out) {
        if (pointList.size() < 2) throw new IllegalArgumentException("Not enough points to simplify");

        // Find the point with the maximum distance from line between the start and end
        double dmax = 0.0;
        int index = 0;
        int end = pointList.size() - 1;
        for (int i = 1; i < end; ++i) {
            double d = perpendicularDistance(pointList.get(i), pointList.get(0), pointList.get(end));
            if (d > dmax) {
                index = i;
                dmax = d;
            }
        }

        // If max distance is greater than epsilon, recursively simplify
        if (dmax > epsilon) {
            List<Point> recResults1 = new ArrayList<>();
            List<Point> recResults2 = new ArrayList<>();
            List<Point> firstLine = pointList.subList(0, index + 1);
            List<Point> lastLine = pointList.subList(index, pointList.size());
            ramerDouglasPeucker(firstLine, epsilon, recResults1);
            ramerDouglasPeucker(lastLine, epsilon, recResults2);

            // build the result list
            out.addAll(recResults1.subList(0, recResults1.size() - 1));
            out.addAll(recResults2);
            if (out.size() < 2) throw new RuntimeException("Problem assembling output");
        } else {
            // Just return start and end points
            out.clear();
            out.add(pointList.get(0));
            out.add(pointList.get(pointList.size() - 1));
        }
    }

    public static void main(String[] args) {
        List<Point> pointList = List.of(
                new Point(0.0, 0.0),
                new Point(1.0, 0.1),
                new Point(2.0, -0.1),
                new Point(3.0, 5.0),
                new Point(4.0, 6.0),
                new Point(5.0, 7.0),
                new Point(6.0, 8.1),
                new Point(7.0, 9.0),
                new Point(8.0, 9.0),
                new Point(9.0, 9.0)
        );
        List<Point> pointListOut = new ArrayList<>();
        ramerDouglasPeucker(pointList, 1.0, pointListOut);
        System.out.println("Points remaining after simplification:");
        pointListOut.forEach(System.out::println);
    }
}
```

{{out}}

```txt
Points remaining after simplification:
(0.000000, 0.000000)
(2.000000, -0.100000)
(3.000000, 5.000000)
(7.000000, 9.000000)
(9.000000, 9.000000)
```



## JavaScript

{{trans|Go}}

```JavaScript
/**
 * @typedef {{
 *    x: (!number),
 *    y: (!number)
 * }}
 */
let pointType;

/**
 * @param {!Array<pointType>} l
 * @param {number} eps
 */
const RDP = (l, eps) => {
  const last = l.length - 1;
  const p1 = l[0];
  const p2 = l[last];
  const x21 = p2.x - p1.x;
  const y21 = p2.y - p1.y;

  const [dMax, x] = l.slice(1, last)
      .map(p => Math.abs(y21 * p.x - x21 * p.y + p2.x * p1.y - p2.y * p1.x))
      .reduce((p, c, i) => {
        const v = Math.max(p[0], c);
        return [v, v === p[0] ? p[1] : i + 1];
      }, [-1, 0]);

  if (dMax > eps) {
    return [...RDP(l.slice(0, x + 1), eps), ...RDP(l.slice(x), eps).slice(1)];
  }
  return [l[0], l[last]]
};

const points = [
  {x: 0, y: 0},
  {x: 1, y: 0.1},
  {x: 2, y: -0.1},
  {x: 3, y: 5},
  {x: 4, y: 6},
  {x: 5, y: 7},
  {x: 6, y: 8.1},
  {x: 7, y: 9},
  {x: 8, y: 9},
  {x: 9, y: 9}];

console.log(RDP(points, 1));
```

{{out}}

```txt
[{x: 0, y: 0},
  {x: 2, y: -0.1},
  {x: 3, y: 5},
  {x: 7, y: 9},
  {x: 9, y: 9}]
```



## Julia

{{works with|Julia|0.6}}
{{trans|C++}}


```julia
const Point = Vector{Float64}

function perpdist(pt::Point, lnstart::Point, lnend::Point)
    d = normalize!(lnend .- lnstart)

    pv = pt .- lnstart
    # Get dot product (project pv onto normalized direction)
    pvdot = dot(d, pv)
    # Scale line direction vector
    ds = pvdot .* d
    # Subtract this from pv
    return norm(pv .- ds)
end

function rdp(plist::Vector{Point}, ϵ::Float64 = 1.0)
    if length(plist) < 2
        throw(ArgumentError("not enough points to simplify"))
    end

    # Find the point with the maximum distance from line between start and end
    distances  = collect(perpdist(pt, plist[1], plist[end]) for pt in plist)
    dmax, imax = findmax(distances)

    # If max distance is greater than epsilon, recursively simplify
    if dmax > ϵ
        fstline = plist[1:imax]
        lstline = plist[imax:end]

        recrst1 = rdp(fstline, ϵ)
        recrst2 = rdp(lstline, ϵ)

        out = vcat(recrst1, recrst2)
    else
        out = [plist[1], plist[end]]
    end

    return out
end

plist = Point[[0.0, 0.0], [1.0, 0.1], [2.0, -0.1], [3.0, 5.0], [4.0, 6.0], [5.0, 7.0], [6.0, 8.1], [7.0, 9.0], [8.0, 9.0], [9.0, 9.0]]
@show plist
@show rdp(plist)
```


{{out}}

```txt
plist = Array{Float64,1}[[0.0, 0.0], [1.0, 0.1], [2.0, -0.1], [3.0, 5.0], [4.0, 6.0], [5.0, 7.0], [6.0, 8.1], [7.0, 9.0], [8.0, 9.0], [9.0, 9.0]]
rdp(plist) = Array{Float64,1}[[0.0, 0.0], [2.0, -0.1], [2.0, -0.1], [3.0, 5.0], [3.0, 5.0], [7.0, 9.0], [7.0, 9.0], [9.0, 9.0]]
```



## Kotlin

{{trans|C++}}

```scala
// version 1.1.0

typealias Point = Pair<Double, Double>

fun perpendicularDistance(pt: Point, lineStart: Point, lineEnd: Point): Double {
    var dx = lineEnd.first - lineStart.first
    var dy = lineEnd.second - lineStart.second

    // Normalize
    val mag = Math.hypot(dx, dy)
    if (mag > 0.0) { dx /= mag; dy /= mag }
    val pvx = pt.first - lineStart.first
    val pvy = pt.second - lineStart.second

    // Get dot product (project pv onto normalized direction)
    val pvdot = dx * pvx + dy * pvy

    // Scale line direction vector and substract it from pv
    val ax = pvx - pvdot * dx
    val ay = pvy - pvdot * dy

    return Math.hypot(ax, ay)
}

fun RamerDouglasPeucker(pointList: List<Point>, epsilon: Double, out: MutableList<Point>) {
    if (pointList.size < 2) throw IllegalArgumentException("Not enough points to simplify")

    // Find the point with the maximum distance from line between start and end
    var dmax = 0.0
    var index = 0
    val end = pointList.size - 1
    for (i in 1 until end) {
        val d = perpendicularDistance(pointList[i], pointList[0], pointList[end])
        if (d > dmax) { index = i; dmax = d }
    }

    // If max distance is greater than epsilon, recursively simplify
    if (dmax > epsilon) {
        val recResults1 = mutableListOf<Point>()
        val recResults2 = mutableListOf<Point>()
        val firstLine = pointList.take(index + 1)
        val lastLine  = pointList.drop(index)
        RamerDouglasPeucker(firstLine, epsilon, recResults1)
        RamerDouglasPeucker(lastLine, epsilon, recResults2)

        // build the result list
        out.addAll(recResults1.take(recResults1.size - 1))
        out.addAll(recResults2)
        if (out.size < 2) throw RuntimeException("Problem assembling output")
    }
    else {
        // Just return start and end points
        out.clear()
        out.add(pointList.first())
        out.add(pointList.last())
    }
}

fun main(args: Array<String>) {
    val pointList = listOf(
        Point(0.0, 0.0),
        Point(1.0, 0.1),
        Point(2.0, -0.1),
        Point(3.0, 5.0),
        Point(4.0, 6.0),
        Point(5.0, 7.0),
        Point(6.0, 8.1),
	Point(7.0, 9.0),
	Point(8.0, 9.0),
        Point(9.0, 9.0)
    )
    val pointListOut = mutableListOf<Point>()
    RamerDouglasPeucker(pointList, 1.0, pointListOut)
    println("Points remaining after simplification:")
    for (p in pointListOut) println(p)
}
```


{{out}}

```txt

Points remaining after simplification:
(0.0, 0.0)
(2.0, -0.1)
(3.0, 5.0)
(7.0, 9.0)
(9.0, 9.0)

```



## Nim


```nim
import math

type
  Point = tuple[x, y: float64]

proc pointLineDistance(pt, lineStart, lineEnd: Point): float64 =
  var n, d, dx, dy: float64
  dx = lineEnd.x - lineStart.x
  dy = lineEnd.y - lineStart.y
  n = abs(dx * (lineStart.y - pt.y) - (lineStart.x - pt.x) * dy)
  d = sqrt(dx * dx + dy * dy)
  n / d

proc rdp(points: seq[Point], startIndex, lastIndex: int, ε: float64 = 1.0): seq[Point] =
  var dmax = 0.0
  var index = startIndex

  for i in index+1..<lastIndex:
    var d = pointLineDistance(points[i], points[startIndex], points[lastIndex])
    if d > dmax:
      index = i
      dmax = d

  if dmax > ε:
    var res1 = rdp(points, startIndex, index, ε)
    var res2 = rdp(points, index, lastIndex, ε)

    var finalRes: seq[Point] = @[]
    finalRes.add(res1[0..^2])
    finalRes.add(res2[0..^1])

    result = finalRes
  else:
    result = @[points[startIndex], points[lastIndex]]

var line: seq[Point] = @[(0.0, 0.0), (1.0, 0.1), (2.0, -0.1), (3.0, 5.0), (4.0, 6.0),
                         (5.0, 7.0), (6.0, 8.1), (7.0,  9.0), (8.0, 9.0), (9.0, 9.0)]
echo rdp(line, line.low, line.high)
```


{{out}}

```txt
@[(x: 0.0, y: 0.0), (x: 2.0, y: -0.1), (x: 3.0, y: 5.0), (x: 7.0, y: 9.0), (x: 9.0, y: 9.0)]
```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';
use List::MoreUtils qw(firstidx minmax);

my $epsilon = 1;

sub norm {
    my(@list) = @_;
    my $sum;
    $sum += $_**2 for @list;
    sqrt($sum)
}

sub perpendicular_distance {
    our(@start,@end,@point);
    local(*start,*end,*point) = (shift, shift, shift);
    return 0 if $start[0]==$point[0] && $start[1]==$point[1]
             or   $end[0]==$point[0] &&   $end[1]==$point[1];
    my ( $dx,  $dy)  = (  $end[0]-$start[0],  $end[1]-$start[1]);
    my ($dpx, $dpy)  = ($point[0]-$start[0],$point[1]-$start[1]);
    my $t = norm($dx, $dy);
    $dx /= $t;
    $dy /= $t;
    norm($dpx - $dx*($dx*$dpx + $dy*$dpy), $dpy - $dy*($dx*$dpx + $dy*$dpy));
}

sub Ramer_Douglas_Peucker {
    my(@points) = @_;
    return @points if @points == 2;
    my @d;
    push @d, perpendicular_distance(@points[0, -1, $_]) for 0..@points-1;
    my(undef,$dmax) = minmax @d;
    my $index = firstidx { $_ == $dmax } @d;
    if ($dmax > $epsilon) {
        my @lo = Ramer_Douglas_Peucker( @points[0..$index]);
        my @hi = Ramer_Douglas_Peucker( @points[$index..$#points]);
        return  @lo[0..@lo-2], @hi;
    }
    @points[0, -1];
}

say '(' . join(' ', @$_) . ') '
    for Ramer_Douglas_Peucker( [0,0],[1,0.1],[2,-0.1],[3,5],[4,6],[5,7],[6,8.1],[7,9],[8,9],[9,9] )
```

{{out}}

```txt
(0 0)
(2 -0.1)
(3 5)
(7 9)
(9 9)
```



## Perl 6

{{works with|Rakudo|2017.05}}
{{trans|C++}}


```perl6
sub norm (*@list) { @list»².sum.sqrt }

sub perpendicular-distance (@start, @end where @end !eqv @start, @point) {
    return 0 if @point eqv any(@start, @end);
    my ( $Δx, $Δy ) =   @end «-» @start;
    my ($Δpx, $Δpy) = @point «-» @start;
    ($Δx, $Δy) «/=» norm $Δx, $Δy;
    norm ($Δpx, $Δpy) «-» ($Δx, $Δy) «*» ($Δx*$Δpx + $Δy*$Δpy);
}

sub Ramer-Douglas-Peucker(@points where * > 1, \ε = 1) {
    return @points if @points == 2;
    my @d = (^@points).map: { perpendicular-distance |@points[0, *-1, $_] };
    my ($index, $dmax) = @d.first: @d.max, :kv;
    return flat
      Ramer-Douglas-Peucker( @points[0..$index], ε )[^(*-1)],
      Ramer-Douglas-Peucker( @points[$index..*], ε )
      if $dmax > ε;
    @points[0, *-1];
}

# TESTING
say Ramer-Douglas-Peucker(
   [(0,0),(1,0.1),(2,-0.1),(3,5),(4,6),(5,7),(6,8.1),(7,9),(8,9),(9,9)]
);
```

{{out}}

```txt
((0 0) (2 -0.1) (3 5) (7 9) (9 9))
```



## Phix

{{trans|Go}}

```Phix
function rdp(sequence l, atom e)
    if length(l)<2 then crash("not enough points to simplify") end if
    integer idx := 0
    atom dMax := -1,
    {p1x,p1y} := l[1],
    {p2x,p2y} := l[$],
    x21 := p2x - p1x,
    y21 := p2y - p1y
    for i=1 to length(l) do
        atom {px,py} = l[i],
             d = abs(y21*px-x21*py + p2x*p1y-p2y*p1x)
        if d>dMax then
            idx = i
            dMax = d
        end if
    end for
    if dMax>e then
        return rdp(l[1..idx], e) & rdp(l[idx..$], e)[2..$]
    end if
    return {l[1], l[$]}
end function

sequence points = {{0, 0}, {1, 0.1}, {2, -0.1}, {3, 5}, {4, 6},
                   {5, 7}, {6, 8.1}, {7,    9}, {8, 9}, {9, 9}}
?rdp(points, 1)
```

{{out}}

```txt

{{0,0},{2,-0.1},{3,5},{7,9},{9,9}}

```




## PHP

{{trans|C++}}

```php
function perpendicular_distance(array $pt, array $line) {
  // Calculate the normalized delta x and y of the line.
  $dx = $line[1][0] - $line[0][0];
  $dy = $line[1][1] - $line[0][1];
  $mag = sqrt($dx * $dx + $dy * $dy);
  if ($mag > 0) {
    $dx /= $mag;
    $dy /= $mag;
  }

  // Calculate dot product, projecting onto normalized direction.
  $pvx = $pt[0] - $line[0][0];
  $pvy = $pt[1] - $line[0][1];
  $pvdot = $dx * $pvx + $dy * $pvy;

  // Scale line direction vector and subtract from pv.
  $dsx = $pvdot * $dx;
  $dsy = $pvdot * $dy;
  $ax = $pvx - $dsx;
  $ay = $pvy - $dsy;

  return sqrt($ax * $ax + $ay * $ay);
}

function ramer_douglas_peucker(array $points, $epsilon) {
  if (count($points) < 2) {
    throw new InvalidArgumentException('Not enough points to simplify');
  }

  // Find the point with the maximum distance from the line between start/end.
  $dmax = 0;
  $index = 0;
  $end = count($points) - 1;
  $start_end_line = [$points[0], $points[$end]];
  for ($i = 1; $i < $end; $i++) {
    $dist = perpendicular_distance($points[$i], $start_end_line);
    if ($dist > $dmax) {
      $index = $i;
      $dmax = $dist;
    }
  }

  // If max distance is larger than epsilon, recursively simplify.
  if ($dmax > $epsilon) {
    $new_start = ramer_douglas_peucker(array_slice($points, 0, $index + 1), $epsilon);
    $new_end = ramer_douglas_peucker(array_slice($points, $index), $epsilon);
    array_pop($new_start);
    return array_merge($new_start, $new_end);
  }

  // Max distance is below epsilon, so return a line from with just the
  // start and end points.
  return [ $points[0], $points[$end]];
}

$polyline = [
  [0,0],
  [1,0.1],
  [2,-0.1],
  [3,5],
  [4,6],
  [5,7],
  [6,8.1],
  [7,9],
  [8,9],
  [9,9],
];

$result = ramer_douglas_peucker($polyline, 1.0);
print "Result:\n";
foreach ($result as $point) {
  print $point[0] . ',' . $point[1] . "\n";
}
```


{{out}}

```txt

Result:
0,0
2,-0.1
3,5
7,9
9,9

```



## Python

An approach using the shapely library:


```python
from __future__ import print_function
from shapely.geometry import LineString

if __name__=="__main__":
	line = LineString([(0,0),(1,0.1),(2,-0.1),(3,5),(4,6),(5,7),(6,8.1),(7,9),(8,9),(9,9)])
	print (line.simplify(1.0, preserve_topology=False))
```

{{out}}

```txt
LINESTRING (0 0, 2 -0.1, 3 5, 7 9, 9 9)
```



## Racket



```racket
#lang racket
(require math/flonum)
;; points are lists of x y (maybe extensible to z)
;; x+y gets both parts as values
(define (x+y p) (values (first p) (second p)))

;; https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
(define (⊥-distance P1 P2)
  (let*-values
      ([(x1 y1) (x+y P1)]
       [(x2 y2) (x+y P2)]
       [(dx dy) (values (- x2 x1) (- y2 y1))]
       [(h) (sqrt (+ (sqr dy) (sqr dx)))])
    (λ (P0)
      (let-values (((x0 y0) (x+y P0)))
        (/ (abs (+ (* dy x0) (* -1 dx y0) (* x2 y1) (* -1 y2 x1))) h)))))

(define (douglas-peucker points-in ϵ)
  (let recur ((ps points-in))
    ;; curried distance function which will be applicable to all points
    (let*-values
        ([(p0) (first ps)]
         [(pz) (last ps)]
         [(p-d) (⊥-distance p0 pz)]
         ;; Find the point with the maximum distance
         [(dmax index)
          (for/fold ((dmax 0) (index 0))
                    ((i (in-range 1 (sub1 (length ps))))) ; skips the first, stops before the last
            (define d (p-d (list-ref ps i)))
            (if (> d dmax) (values d i) (values dmax index)))])
      ;; If max distance is greater than epsilon, recursively simplify
      (if (> dmax ϵ)
          ;; recursive call
          (let-values ([(l r) (split-at ps index)])
            (append (drop-right (recur l) 1) (recur r)))
          (list p0 pz))))) ;; else we can return this simplification

(module+ main
  (douglas-peucker
   '((0 0) (1 0.1) (2 -0.1) (3 5) (4 6) (5 7) (6 8.1) (7 9) (8 9) (9 9))
   1.0))

(module+ test
  (require rackunit)
  (check-= ((⊥-distance '(0 0) '(0 1)) '(1 0)) 1 epsilon.0))
```


{{out}}

```txt
'((0 0) (2 -0.1) (3 5) (7 9) (9 9))
```



## REXX

The computation for the   ''perpendicular distance''   was taken from the   '''GO'''   example.

```rexx
/*REXX program uses the  Ramer─Douglas─Peucker (RDP)  line simplification algorithm  for*/
/*───────────────────────────── reducing the number of points used to define its shape. */
parse arg epsilon pts                            /*obtain optional arguments from the CL*/
if epsilon='' | epsilon=","   then epsilon= 1    /*Not specified?  Then use the default.*/
if pts=''  then pts= '(0,0) (1,0.1) (2,-0.1) (3,5) (4,6) (5,7) (6,8.1) (7,9) (8,9) (9,9)'
pts= space(pts)                                  /*elide all superfluous blanks.        */
say '  error threshold: '   epsilon              /*echo the error threshold to the term.*/
say ' points specified: '   pts                  /*  "   "    shape points   "  "    "  */
$= RDP(pts)                                      /*invoke Ramer─Douglas─Peucker function*/
say 'points simplified: '   rez($)               /*display points with () ───► terminal.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bld: parse arg _; #= words(_); dMax=-#; idx=1; do j=1  for #; @.j= word(_, j); end; return
px:  parse arg _;          return word( translate(_, , ','),  1)   /*obtain the X coörd.*/
py:  parse arg _;          return word( translate(_, , ','),  2)   /*   "    "  Y   "   */
reb: parse arg a,b,,_;                  do k=a  to b;   _=_ @.k;    end;   return strip(_)
rez: parse arg z,_;   do k=1  for words(z); _= _ '('word(z, k)") "; end;   return strip(_)
/*──────────────────────────────────────────────────────────────────────────────────────*/
RDP: procedure expose epsilon; parse arg PT;  call bld  space( translate(PT, , ')(][}{') )
     L= px(@.#)-px(@.1)
     H= py(@.#)-py(@.1)                          /* [↓] find point IDX with max distance*/
                        do i=2  to #-1
                        d= abs(H*px(@.i) - L*py(@.i) + px(@.#)*py(@.1) - py(@.#)*px(@.1) )
                        if d>dMax  then do;   idx= i;   dMax= d
                                        end
                        end   /*i*/              /* [↑]  D is the perpendicular distance*/

     if dMax>epsilon  then do;   r= RDP( reb(1, idx) )
                                 return subword(r, 1, words(r) - 1)     RDP( reb(idx, #) )
                           end
     return @.1  @.#
```

{{out|output|text=  when using the default inputs:}}

```txt

  error threshold:  1
 points specified:  (0,0) (1,0.1) (2,-0.1) (3,5) (4,6) (5,7) (6,8.1) (7,9) (8,9) (9,9)
points simplified:  (0,0)  (2,-0.1)  (3,5)  (7,9)  (9,9)

```



## Sidef

{{trans|Perl 6}}

```ruby
func perpendicular_distance(Arr start, Arr end, Arr point) {
    ((point == start) || (point == end)) && return 0
    var (Δx,  Δy ) = (  end »-« start)...
    var (Δpx, Δpy) = (point »-« start)...
    var h = hypot(Δx, Δy)
    [\Δx, \Δy].map { *_ /= h }
    (([Δpx, Δpy] »-« ([Δx, Δy] »*» (Δx*Δpx + Δy*Δpy))) »**» 2).sum.sqrt
}

func Ramer_Douglas_Peucker(Arr points { .all { .len > 1 } }, ε = 1) {
    points.len == 2 && return points

    var d = (^points -> map {
        perpendicular_distance(points[0], points[-1], points[_])
    })

    if (d.max > ε) {
        var i = d.index(d.max)
        return [Ramer_Douglas_Peucker(points.ft(0, i), ε).ft(0, -2)...,
                Ramer_Douglas_Peucker(points.ft(i),    ε)...]
    }

    return [points[0,-1]]
}

say Ramer_Douglas_Peucker(
    [[0,0],[1,0.1],[2,-0.1],[3,5],[4,6],[5,7],[6,8.1],[7,9],[8,9],[9,9]]
)
```

{{out}}

```txt

[[0, 0], [2, -1/10], [3, 5], [7, 9], [9, 9]]

```



## Yabasic


```Yabasic
sub perpendicularDistance(tabla(), i, ini, fin)
    local dx, cy, mag, pvx, pvy, pvdot, dsx, dsy, ax, ay

    dx = tabla(fin, 1) - tabla(ini, 1)
    dy = tabla(fin, 2) - tabla(ini, 2)

    //Normalise
    mag = (dx^2 + dy^2)^0.5
    if mag > 0 dx = dx / mag : dy = dy / mag

    pvx = tabla(i, 1) - tabla(ini, 1)
    pvy = tabla(i, 2) - tabla(ini, 2)

    //Get dot product (project pv onto normalized direction)
    pvdot = dx * pvx + dy * pvy

    //Scale line direction vector
    dsx = pvdot * dx
    dsy = pvdot * dy

    //Subtract this from pv
    ax = pvx - dsx
    ay = pvy - dsy

    return (ax^2 + ay^2)^0.5
end sub

sub DouglasPeucker(PointList(), ini, fin, epsilon)
    local dmax, index, i, d
    // Find the point with the maximum distance

    for i = ini + 1 to fin
        d = perpendicularDistance(PointList(), i, ini, fin)
        if d > dmax index = i : dmax = d
    next

    // If max distance is greater than epsilon, recursively simplify
    if dmax > epsilon then
        PointList(index, 3) = true
        // Recursive call
        DouglasPeucker(PointList(), ini, index, epsilon)
        DouglasPeucker(PointList(), index, fin, epsilon)
    end if
end sub


data 0,0, 1,0.1,  2,-0.1,  3,5,  4,6,  5,7,  6,8.1,  7,9,  8,9,  9,9

dim matriz(10, 3)

for i = 1 to 10
    read matriz(i, 1), matriz(i, 2)
next

DouglasPeucker(matriz(), 1, 10, 1)

matriz(1, 3) = true : matriz(10, 3) = true
for i = 1 to 10
    if matriz(i, 3) print matriz(i, 1), matriz(i, 2)
next
```



## zkl

{{trans|Perl 6}}

```zkl
fcn perpendicularDistance(start,end, point){  // all are tuples: (x,y) -->|d|
   dx,dy   := end  .zipWith('-,start);	// deltas
   dpx,dpy := point.zipWith('-,start);
   mag     := (dx*dx + dy*dy).sqrt();
   if(mag>0.0){ dx/=mag; dy/=mag; }
   p,dsx,dsy := dx*dpx + dy*dpy, p*dx, p*dy;
   ((dpx - dsx).pow(2) + (dpy - dsy).pow(2)).sqrt()
}

fcn RamerDouglasPeucker(points,epsilon=1.0){  // list of tuples --> same
   if(points.len()==2) return(points);  // but we'll do one point
   d:=points.pump(List,  // first result/element is always zero
      fcn(p, s,e){ perpendicularDistance(s,e,p) }.fp1(points[0],points[-1]));
   index,dmax := (0.0).minMaxNs(d)[1], d[index]; // minMaxNs-->index of min & max
   if(dmax>epsilon){
       return(RamerDouglasPeucker(points[0,index],epsilon)[0,-1].extend(
              RamerDouglasPeucker(points[index,*],epsilon)))
   } else return(points[0],points[-1]);
}
```


```zkl
RamerDouglasPeucker(
   T( T(0.0, 0.0), T(1.0, 0.1), T(2.0, -0.1), T(3.0, 5.0), T(4.0, 6.0),
      T(5.0, 7.0), T(6.0, 8.1), T(7.0,  9.0), T(8.0, 9.0), T(9.0, 9.0) ))
.println();
```

{{out}}

```txt

L(L(0,0),L(2,-0.1),L(3,5),L(7,9),L(9,9))

```


'''References'''
<references/>

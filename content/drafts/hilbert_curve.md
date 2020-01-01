+++
title = "Hilbert curve"
description = ""
date = 2019-10-14T01:11:52Z
aliases = []
[extra]
id = 21807
[taxonomies]
categories = []
tags = []
+++

{{task|Fractals}}
<br/>
;Task

Produce a graphical or ASCII-art representation of a [[wp:Hilbert curve|Hilbert curve]] of at least order 3.


## ALGOL 68

This generates the curve following the L-System rules described in the Wikipedia article.

{| class="wikitable"
| '''L-System rule''' || ''A'' || ''B'' || ''F''   || '''+''' || '''-'''
|-
| '''Procedure'''     || a     || b     || forward || right   || left
|}


```algol68
BEGIN
  INT level = 4;    # <-- change this #

  INT side = 2**level * 2 - 2;
  [-side:1, 0:side]STRING grid;
  INT x := 0, y := 0, dir := 0;
  INT old dir := -1;
  INT e=0, n=1, w=2, s=3;

  FOR i FROM 1 LWB grid TO 1 UPB grid DO
    FOR j FROM 2 LWB grid TO 2 UPB grid DO grid[i,j] := "  "
  OD OD;

  PROC left  = VOID: dir := (dir + 1) MOD 4;
  PROC right = VOID: dir := (dir - 1) MOD 4;
  PROC move  = VOID: (
    CASE dir + 1 IN
      # e: # x +:= 1, # n: # y -:= 1, # w: # x -:= 1, # s: # y +:= 1
    ESAC
  );
  PROC forward = VOID: (
    # draw corner #
    grid[y, x] := CASE old dir + 1 IN
                   # e # CASE dir + 1 IN "──", "─╯", " ?", "─╮" ESAC,
                   # n # CASE dir + 1 IN " ╭", " │", "─╮", " ?" ESAC,
                   # w # CASE dir + 1 IN " ?", " ╰", "──", " ╭" ESAC,
                   # s # CASE dir + 1 IN " ╰", " ?", "─╯", " │" ESAC
                  OUT "  "
                  ESAC;
    move;
    # draw segment #
    grid[y, x] := IF dir = n OR dir = s THEN " │" ELSE "──" FI;
    # advance to next corner #
    move;
    old dir := dir
  );

  PROC a = (INT level)VOID:
    IF level > 0 THEN
      left; b(level-1); forward; right; a(level-1); forward;
      a(level-1); right; forward; b(level-1); left
    FI,
      b = (INT level)VOID:
    IF level > 0 THEN
      right; a(level-1); forward; left; b(level-1); forward;
      b(level-1); left; forward; a(level-1); right
    FI;

  # draw #
  a(level);

  # print #
  FOR row FROM 1 LWB grid TO 1 UPB grid DO
    print((grid[row,], new line))
  OD
END

```

{{out}}

```txt
 ╭───╮   ╭───╮   ╭───╮   ╭───╮   ╭───╮   ╭───╮   ╭───╮   ╭───╮
 │   │   │   │   │   │   │   │   │   │   │   │   │   │   │   │
 │   ╰───╯   │   │   ╰───╯   │   │   ╰───╯   │   │   ╰───╯   │
 │           │   │           │   │           │   │           │
 ╰───╮   ╭───╯   ╰───╮   ╭───╯   ╰───╮   ╭───╯   ╰───╮   ╭───╯
     │   │           │   │           │   │           │   │
 ╭───╯   ╰───────────╯   ╰───╮   ╭───╯   ╰───────────╯   ╰───╮
 │                           │   │                           │
 │   ╭───────╮   ╭───────╮   │   │   ╭───────╮   ╭───────╮   │
 │   │       │   │       │   │   │   │       │   │       │   │
 ╰───╯   ╭───╯   ╰───╮   ╰───╯   ╰───╯   ╭───╯   ╰───╮   ╰───╯
         │           │                   │           │
 ╭───╮   ╰───╮   ╭───╯   ╭───╮   ╭───╮   ╰───╮   ╭───╯   ╭───╮
 │   │       │   │       │   │   │   │       │   │       │   │
 │   ╰───────╯   ╰───────╯   ╰───╯   ╰───────╯   ╰───────╯   │
 │                                                           │
 ╰───╮   ╭───────╮   ╭───────╮   ╭───────╮   ╭───────╮   ╭───╯
     │   │       │   │       │   │       │   │       │   │
 ╭───╯   ╰───╮   ╰───╯   ╭───╯   ╰───╮   ╰───╯   ╭───╯   ╰───╮
 │           │           │           │           │           │
 │   ╭───╮   │   ╭───╮   ╰───╮   ╭───╯   ╭───╮   │   ╭───╮   │
 │   │   │   │   │   │       │   │       │   │   │   │   │   │
 ╰───╯   ╰───╯   │   ╰───────╯   ╰───────╯   │   ╰───╯   ╰───╯
                 │                           │
 ╭───╮   ╭───╮   │   ╭───────╮   ╭───────╮   │   ╭───╮   ╭───╮
 │   │   │   │   │   │       │   │       │   │   │   │   │   │
 │   ╰───╯   │   ╰───╯   ╭───╯   ╰───╮   ╰───╯   │   ╰───╯   │
 │           │           │           │           │           │
 ╰───╮   ╭───╯   ╭───╮   ╰───╮   ╭───╯   ╭───╮   ╰───╮   ╭───╯
     │   │       │   │       │   │       │   │       │   │
  ───╯   ╰───────╯   ╰───────╯   ╰───────╯   ╰───────╯   ╰──

```



## C

{{trans|Kotlin}}

```c
#include <stdio.h>

#define N 32
#define K 3
#define MAX N * K

typedef struct { int x; int y; } point;

void rot(int n, point *p, int rx, int ry) {
    int t;
    if (!ry) {
        if (rx == 1) {
            p->x = n - 1 - p->x;
            p->y = n - 1 - p->y;
        }
        t = p->x;
        p->x = p->y;
        p->y = t;
    }
}

void d2pt(int n, int d, point *p) {
    int s = 1, t = d, rx, ry;
    p->x = 0;
    p->y = 0;
    while (s < n) {
        rx = 1 & (t / 2);
        ry = 1 & (t ^ rx);
        rot(s, p, rx, ry);
        p->x += s * rx;
        p->y += s * ry;
        t /= 4;
        s *= 2;
    }
}

int main() {
    int d, x, y, cx, cy, px, py;
    char pts[MAX][MAX];
    point curr, prev;
    for (x = 0; x < MAX; ++x)
        for (y = 0; y < MAX; ++y) pts[x][y] = ' ';
    prev.x = prev.y = 0;
    pts[0][0] = '.';
    for (d = 1; d < N * N; ++d) {
        d2pt(N, d, &curr);
        cx = curr.x * K;
        cy = curr.y * K;
        px = prev.x * K;
        py = prev.y * K;
        pts[cx][cy] = '.';
        if (cx == px ) {
            if (py < cy)
                for (y = py + 1; y < cy; ++y) pts[cx][y] = '|';
            else
                for (y = cy + 1; y < py; ++y) pts[cx][y] = '|';
        }
        else {
            if (px < cx)
                for (x = px + 1; x < cx; ++x) pts[x][cy] = '_';
            else
                for (x = cx + 1; x < px; ++x) pts[x][cy] = '_';
        }
        prev = curr;
    }
    for (x = 0; x < MAX; ++x) {
        for (y = 0; y < MAX; ++y) printf("%c", pts[y][x]);
        printf("\n");
    }
    return 0;
}
```

{{output}}

```txt
Same as Kotlin entry.
```



## C#

{{trans|Visual Basic .NET}}

```csharp
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace HilbertCurve {
    class Program {
        static void Swap<T>(ref T a, ref T b) {
            var c = a;
            a = b;
            b = c;
        }

        struct Point {
            public int x, y;

            public Point(int x, int y) {
                this.x = x;
                this.y = y;
            }

            //rotate/flip a quadrant appropriately
            public void Rot(int n, bool rx, bool ry) {
                if (!ry) {
                    if (rx) {
                        x = (n - 1) - x;
                        y = (n - 1) - y;
                    }
                    Swap(ref x, ref y);
                }
            }

            public override string ToString() {
                return string.Format("({0}, {1})", x, y);
            }
        }

        static Point FromD(int n, int d) {
            var p = new Point(0, 0);
            int t = d;

            for (int s = 1; s < n; s <<= 1) {
                var rx = (t & 2) != 0;
                var ry = ((t ^ (rx ? 1 : 0)) & 1) != 0;
                p.Rot(s, rx, ry);
                p.x += rx ? s : 0;
                p.y += ry ? s : 0;
                t >>= 2;
            }

            return p;
        }

        static List<Point> GetPointsForCurve(int n) {
            var points = new List<Point>();
            int d = 0;
            while (d < n * n) {
                points.Add(FromD(n, d));
                d += 1;
            }
            return points;
        }

        static List<string> DrawCurve(List<Point> points, int n) {
            var canvas = new char[n, n * 3 - 2];
            for (int i = 0; i < canvas.GetLength(0); i++) {
                for (int j = 0; j < canvas.GetLength(1); j++) {
                    canvas[i, j] = ' ';
                }
            }

            for (int i = 1; i < points.Count; i++) {
                var lastPoint = points[i - 1];
                var curPoint = points[i];
                var deltaX = curPoint.x - lastPoint.x;
                var deltaY = curPoint.y - lastPoint.y;
                if (deltaX == 0) {
                    Debug.Assert(deltaY != 0, "Duplicate point");
                    //vertical line
                    int row = Math.Max(curPoint.y, lastPoint.y);
                    int col = curPoint.x * 3;
                    canvas[row, col] = '|';
                } else {
                    Debug.Assert(deltaY == 0, "Duplicate point");
                    //horizontal line
                    var row = curPoint.y;
                    var col = Math.Min(curPoint.x, lastPoint.x) * 3 + 1;
                    canvas[row, col] = '_';
                    canvas[row, col + 1] = '_';
                }
            }

            var lines = new List<string>();
            for (int i = 0; i < canvas.GetLength(0); i++) {
                var sb = new StringBuilder();
                for (int j = 0; j < canvas.GetLength(1); j++) {
                    sb.Append(canvas[i, j]);
                }
                lines.Add(sb.ToString());
            }
            return lines;
        }

        static void Main() {
            for (int order = 1; order <= 5; order++) {
                var n = 1 << order;
                var points = GetPointsForCurve(n);
                Console.WriteLine("Hilbert curve, order={0}", order);
                var lines = DrawCurve(points, n);
                foreach (var line in lines) {
                    Console.WriteLine(line);
                }
                Console.WriteLine();
            }
        }
    }
}
```

{{out}}

```txt
Hilbert curve, order=1

|__|

Hilbert curve, order=2
 __    __
 __|  |__
|   __   |
|__|  |__|

Hilbert curve, order=3
    __ __    __ __
|__|   __|  |__   |__|
 __   |__    __|   __
|  |__ __|  |__ __|  |
|__    __ __ __    __|
 __|  |__    __|  |__
|   __   |  |   __   |
|__|  |__|  |__|  |__|

Hilbert curve, order=4
 __    __ __    __ __    __ __    __ __    __
 __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|

Hilbert curve, order=5
    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __
|__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|
 __   |__    __|   __   |   __   |   __   |__    __|   __   |   __   |   __   |__    __|   __
|  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |
|__    __ __ __    __|   __    __   |   __ __    __ __   |   __    __   |__    __ __ __    __|
 __|  |__    __|  |__   |  |__|  |  |__|   __|  |__   |__|  |  |__|  |   __|  |__    __|  |__
|   __   |  |   __   |  |__    __|   __   |__    __|   __   |__    __|  |   __   |  |   __   |
|__|  |__|  |__|  |__|   __|  |__ __|  |__ __|  |__ __|  |__ __|  |__   |__|  |__|  |__|  |__|
 __    __    __    __   |__    __ __    __ __    __ __    __ __    __|   __    __    __    __
|  |__|  |  |  |__|  |   __|  |__   |__|   __|  |__   |__|   __|  |__   |  |__|  |  |  |__|  |
|__    __|  |__    __|  |   __   |   __   |__    __|   __   |   __   |  |__    __|  |__    __|
 __|  |__ __ __|  |__   |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|   __|  |__ __ __|  |__
|   __ __    __ __   |   __    __   |   __ __    __ __   |   __    __   |   __ __    __ __   |
|__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|
 __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __
|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |
|__    __ __    __ __    __ __    __ __    __ __ __    __ __    __ __    __ __    __ __    __|
 __|  |__   |__|   __|  |__   |__|   __|  |__    __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |  |   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __    __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|  |__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__    __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |  |   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __    __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|
```



## D

{{trans|Java}}

```d
import std.stdio;

void main() {
    foreach (order; 1..6) {
        int n = 1 << order;
        auto points = getPointsForCurve(n);
        writeln("Hilbert curve, order=", order);
        auto lines = drawCurve(points, n);
        foreach (line; lines) {
            writeln(line);
        }
        writeln;
    }
}

struct Point {
    int x, y;

    //rotate/flip a quadrant appropriately
    void rot(int n, bool rx, bool ry) {
        if (!ry) {
            if (rx) {
                x = (n - 1) - x;
                y = (n - 1) - y;
            }

            import std.algorithm.mutation;
            swap(x, y);
        }
    }

    int calcD(int n) {
        bool rx, ry;
        int d;
        for (int s = n >>> 1; s > 0; s >>>= 1) {
            rx = ((x & s) != 0);
            ry = ((y & s) != 0);
            d += s * s * ((rx ? 3 : 0) ^ (ry ? 1 : 0));
            rot(s, rx, ry);
        }
        return d;
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format : formattedWrite;

        sink("(");
        sink.formattedWrite!"%d"(x);
        sink(", ");
        sink.formattedWrite!"%d"(y);
        sink(")");
    }
}

auto fromD(int n, int d) {
    Point p;
    bool rx, ry;
    int t = d;
    for (int s = 1; s < n; s <<= 1) {
        rx = ((t & 2) != 0);
        ry = (((t ^ (rx ? 1 : 0)) & 1) != 0);
        p.rot(s, rx, ry);
        p.x += (rx ? s : 0);
        p.y += (ry ? s : 0);
        t >>>= 2;
    }
    return p;
}

auto getPointsForCurve(int n) {
    Point[] points;
    for (int d; d < n * n; ++d) {
        points ~= fromD(n, d);
    }
    return points;
}

auto drawCurve(Point[] points, int n) {
    import std.algorithm.comparison : min, max;
    import std.array : uninitializedArray;
    import std.exception : enforce;

    auto canvas = uninitializedArray!(char[][])(n, n * 3 - 2);
    foreach (line; canvas) {
        line[] =  ' ';
    }

    for (int i = 1; i < points.length; ++i) {
        auto lastPoint = points[i - 1];
        auto curPoint = points[i];
        int deltaX = curPoint.x - lastPoint.x;
        int deltaY = curPoint.y - lastPoint.y;
        if (deltaX == 0) {
            enforce(deltaY != 0, "Duplicate point");
            // vertical line
            int row = max(curPoint.y, lastPoint.y);
            int col = curPoint.x * 3;
            canvas[row][col] = '|';
        } else {
            enforce(deltaY == 0, "Diagonal line");
            // horizontal line
            int row = curPoint.y;
            int col = min(curPoint.x, lastPoint.x) * 3 + 1;
            canvas[row][col] = '_';
            canvas[row][col + 1] = '_';
        }
    }

    string[] lines;
    foreach (row; canvas) {
        lines ~= row.idup;
    }

    return lines;
}
```

{{out}}

```txt
Hilbert curve, order=1

|__|

Hilbert curve, order=2
 __    __
 __|  |__
|   __   |
|__|  |__|

Hilbert curve, order=3
    __ __    __ __
|__|   __|  |__   |__|
 __   |__    __|   __
|  |__ __|  |__ __|  |
|__    __ __ __    __|
 __|  |__    __|  |__
|   __   |  |   __   |
|__|  |__|  |__|  |__|

Hilbert curve, order=4
 __    __ __    __ __    __ __    __ __    __
 __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|

Hilbert curve, order=5
    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __
|__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|
 __   |__    __|   __   |   __   |   __   |__    __|   __   |   __   |   __   |__    __|   __
|  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |
|__    __ __ __    __|   __    __   |   __ __    __ __   |   __    __   |__    __ __ __    __|
 __|  |__    __|  |__   |  |__|  |  |__|   __|  |__   |__|  |  |__|  |   __|  |__    __|  |__
|   __   |  |   __   |  |__    __|   __   |__    __|   __   |__    __|  |   __   |  |   __   |
|__|  |__|  |__|  |__|   __|  |__ __|  |__ __|  |__ __|  |__ __|  |__   |__|  |__|  |__|  |__|
 __    __    __    __   |__    __ __    __ __    __ __    __ __    __|   __    __    __    __
|  |__|  |  |  |__|  |   __|  |__   |__|   __|  |__   |__|   __|  |__   |  |__|  |  |  |__|  |
|__    __|  |__    __|  |   __   |   __   |__    __|   __   |   __   |  |__    __|  |__    __|
 __|  |__ __ __|  |__   |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|   __|  |__ __ __|  |__
|   __ __    __ __   |   __    __   |   __ __    __ __   |   __    __   |   __ __    __ __   |
|__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|
 __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __
|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |
|__    __ __    __ __    __ __    __ __    __ __ __    __ __    __ __    __ __    __ __    __|
 __|  |__   |__|   __|  |__   |__|   __|  |__    __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |  |   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __    __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|  |__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__    __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |  |   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __    __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Hilbert_curve this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC

{{trans|Yabasic}}

```freebasic

Dim Shared As Integer ancho = 64

Sub Hilbert(x As Integer, y As Integer, lg As Integer, i1 As Integer, i2 As Integer)
    If lg = 1 Then
        Line - ((ancho-x) * 10, (ancho-y) * 10)
        Return
    End If
    lg = lg / 2
    Hilbert(x+i1*lg, y+i1*lg, lg, i1, 1-i2)
    Hilbert(x+i2*lg, y+(1-i2)*lg, lg, i1, i2)
    Hilbert(x+(1-i1)*lg, y+(1-i1)*lg, lg, i1, i2)
    Hilbert(x+(1-i2)*lg, y+i2*lg, lg, 1-i1, i2)
End Sub

Screenres 655, 655

Hilbert(0, 0, ancho, 0, 0)
End

```



## Go

{{libheader|Go Graphics}}


The following is based on the recursive algorithm and C code in [https://www.researchgate.net/profile/Christoph_Schierz2/publication/228982573_A_recursive_algorithm_for_the_generation_of_space-filling_curves/links/0912f505c2f419782c000000/A-recursive-algorithm-for-the-generation-of-space-filling-curves.pdf this paper]. The image produced is similar to the one linked to in the zkl example.

```go
package main

import "github.com/fogleman/gg"

var points []gg.Point

const width = 64

func hilbert(x, y, lg, i1, i2 int) {
    if lg == 1 {
        px := float64(width-x) * 10
        py := float64(width-y) * 10
        points = append(points, gg.Point{px, py})
        return
    }
    lg >>= 1
    hilbert(x+i1*lg, y+i1*lg, lg, i1, 1-i2)
    hilbert(x+i2*lg, y+(1-i2)*lg, lg, i1, i2)
    hilbert(x+(1-i1)*lg, y+(1-i1)*lg, lg, i1, i2)
    hilbert(x+(1-i2)*lg, y+i2*lg, lg, 1-i1, i2)
}

func main() {
    hilbert(0, 0, width, 0, 0)
    dc := gg.NewContext(650, 650)
    dc.SetRGB(0, 0, 0) // Black background
    dc.Clear()
    for _, p := range points {
        dc.LineTo(p.X, p.Y)
    }
    dc.SetHexColor("#90EE90") // Light green curve
    dc.SetLineWidth(1)
    dc.Stroke()
    dc.SavePNG("hilbert.png")
}
```



## Haskell

{{Trans|Python}}
{{Trans|JavaScript}}

Defines an SVG string which can be rendered in a browser.
A Hilbert tree is defined in terms of a production rule,
and folded to a list of points in a square of given size.


```haskell
import Data.Bool (bool)
import Data.Tree

rule :: Char -> String
rule c =
  case c of
    'a' -> "daab"
    'b' -> "cbba"
    'c' -> "bccd"
    'd' -> "addc"
    _ -> []

vectors :: Char -> [(Int, Int)]
vectors c =
  case c of
    'a' -> [(-1, 1), (-1, -1), (1, -1), (1, 1)]
    'b' -> [(1, -1), (-1, -1), (-1, 1), (1, 1)]
    'c' -> [(1, -1), (1, 1), (-1, 1), (-1, -1)]
    'd' -> [(-1, 1), (1, 1), (1, -1), (-1, -1)]
    _ -> []

main :: IO ()
main = do
  let w = 1024
  putStrLn $ svgFromPoints w $ hilbertPoints w (hilbertTree 6)

hilbertTree :: Int -> Tree Char
hilbertTree n =
  let go tree =
        let c = rootLabel tree
            xs = subForest tree
        in Node c (bool (go <$> xs) (flip Node [] <$> rule c) (null xs))
      seed = Node 'a' []
  in bool seed (iterate go seed !! pred n) (0 < n)

hilbertPoints :: Int -> Tree Char -> [(Int, Int)]
hilbertPoints w tree =
  let go r xy tree =
        let d = quot r 2
            f g x = g xy + (d * g x)
            centres = ((,) . f fst) <*> f snd <$> vectors (rootLabel tree)
            xs = subForest tree
        in bool (concat $ zipWith (go d) centres xs) centres (null xs)
      r = quot w 2
  in go r (r, r) tree

svgFromPoints :: Int -> [(Int, Int)] -> String
svgFromPoints w xys =
  let sw = show w
      points =
        (unwords . fmap (((++) . show . fst) <*> ((' ' :) . show . snd))) xys
  in unlines
       [ "<svg xmlns=\"http://www.w3.org/2000/svg\""
       , unwords ["width=\"512\" height=\"512\" viewBox=\"5 5", sw, sw, "\"> "]
       , "<path d=\"M" ++ points ++ "\" "
       , "stroke-width=\"2\" stroke=\"red\" fill=\"transparent\"/>"
       , "</svg>"
       ]
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Hilbert.bas"
110 OPTION ANGLE DEGREES
120 GRAPHICS HIRES 2
130 LET N=5:LET P=1:LET S=11*2^(6-N)
140 PLOT 940,700,ANGLE 180;
150 CALL HILBERT(S,N,P)
160 DEF HILBERT(S,N,P)
170   IF N=0 THEN EXIT DEF
180   PLOT LEFT 90*P;
190   CALL HILBERT(S,N-1,-P)
200   PLOT FORWARD S;RIGHT 90*P;
210   CALL HILBERT(S,N-1,P)
220   PLOT FORWARD S;
230   CALL HILBERT(S,N-1,P)
240   PLOT RIGHT 90*P;FORWARD S;
250   CALL HILBERT(S,N-1,-P)
260   PLOT LEFT 90*P;
270 END DEF
```



## Java


```java
// Translation from https://en.wikipedia.org/wiki/Hilbert_curve

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class HilbertCurve {
    public static class Point {
        public int x;
        public int y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public String toString() {
            return "(" + x + ", " + y + ")";
        }

        //rotate/flip a quadrant appropriately
        public void rot(int n, boolean rx, boolean ry) {
            if (!ry) {
                if (rx) {
                    x = (n - 1) - x;
                    y = (n - 1) - y;
                }

                //Swap x and y
                int t  = x;
                x = y;
                y = t;
            }

            return;
        }

        public int calcD(int n) {
            boolean rx, ry;
            int d = 0;
            for (int s = n >>> 1; s > 0; s >>>= 1) {
                rx = ((x & s) != 0);
                ry = ((y & s) != 0);
                d += s * s * ((rx ? 3 : 0) ^ (ry ? 1 : 0));
                rot(s, rx, ry);
            }

            return d;
        }

    }

    public static Point fromD(int n, int d) {
        Point p = new Point(0, 0);
        boolean rx, ry;
        int t = d;
        for (int s = 1; s < n; s <<= 1) {
            rx = ((t & 2) != 0);
            ry = (((t ^ (rx ? 1 : 0)) & 1) != 0);
            p.rot(s, rx, ry);
            p.x += (rx ? s : 0);
            p.y += (ry ? s : 0);
            t >>>= 2;
        }
        return p;
    }

    public static List<Point> getPointsForCurve(int n) {
        List<Point> points = new ArrayList<Point>();
        for (int d = 0; d < (n * n); d++) {
            Point p = fromD(n, d);
            points.add(p);
        }

        return points;
    }

    public static List<String> drawCurve(List<Point> points, int n) {
        char[][] canvas = new char[n][n * 3 - 2];
        for (char[] line : canvas) {
            Arrays.fill(line, ' ');
        }
        for (int i = 1; i < points.size(); i++) {
             Point lastPoint = points.get(i - 1);
            Point curPoint = points.get(i);
            int deltaX = curPoint.x - lastPoint.x;
            int deltaY = curPoint.y - lastPoint.y;
            if (deltaX == 0) {
                if (deltaY == 0) {
                    // A mistake has been made
                    throw new IllegalStateException("Duplicate point, deltaX=" + deltaX + ", deltaY=" + deltaY);
                }
                // Vertical line
                int row = Math.max(curPoint.y, lastPoint.y);
                int col = curPoint.x * 3;
                canvas[row][col] = '|';
            }
            else {
                if (deltaY != 0) {
                    // A mistake has been made
                    throw new IllegalStateException("Diagonal line, deltaX=" + deltaX + ", deltaY=" + deltaY);
                }
                // Horizontal line
                int row = curPoint.y;
                int col = Math.min(curPoint.x, lastPoint.x) * 3 + 1;
                canvas[row][col] = '_';
                canvas[row][col + 1] = '_';
            }

        }
        List<String> lines = new ArrayList<String>();
        for (char[] row : canvas) {
            String line = new String(row);
            lines.add(line);
        }

        return lines;
    }

    public static void main(String... args) {
        for (int order = 1; order <= 5; order++) {
            int n = (1 << order);
            List<Point> points = getPointsForCurve(n);
            System.out.println("Hilbert curve, order=" + order);
            List<String> lines = drawCurve(points, n);
            for (String line : lines) {
                System.out.println(line);
            }
            System.out.println();
        }
        return;
    }
}
```

{{out}}

```txt
Hilbert curve, order=1

|__|

Hilbert curve, order=2
 __    __
 __|  |__
|   __   |
|__|  |__|

Hilbert curve, order=3
    __ __    __ __
|__|   __|  |__   |__|
 __   |__    __|   __
|  |__ __|  |__ __|  |
|__    __ __ __    __|
 __|  |__    __|  |__
|   __   |  |   __   |
|__|  |__|  |__|  |__|

Hilbert curve, order=4
 __    __ __    __ __    __ __    __ __    __
 __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|

Hilbert curve, order=5
    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __
|__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|
 __   |__    __|   __   |   __   |   __   |__    __|   __   |   __   |   __   |__    __|   __
|  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |
|__    __ __ __    __|   __    __   |   __ __    __ __   |   __    __   |__    __ __ __    __|
 __|  |__    __|  |__   |  |__|  |  |__|   __|  |__   |__|  |  |__|  |   __|  |__    __|  |__
|   __   |  |   __   |  |__    __|   __   |__    __|   __   |__    __|  |   __   |  |   __   |
|__|  |__|  |__|  |__|   __|  |__ __|  |__ __|  |__ __|  |__ __|  |__   |__|  |__|  |__|  |__|
 __    __    __    __   |__    __ __    __ __    __ __    __ __    __|   __    __    __    __
|  |__|  |  |  |__|  |   __|  |__   |__|   __|  |__   |__|   __|  |__   |  |__|  |  |  |__|  |
|__    __|  |__    __|  |   __   |   __   |__    __|   __   |   __   |  |__    __|  |__    __|
 __|  |__ __ __|  |__   |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|   __|  |__ __ __|  |__
|   __ __    __ __   |   __    __   |   __ __    __ __   |   __    __   |   __ __    __ __   |
|__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|
 __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __
|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |
|__    __ __    __ __    __ __    __ __    __ __ __    __ __    __ __    __ __    __ __    __|
 __|  |__   |__|   __|  |__   |__|   __|  |__    __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |  |   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __    __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|  |__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__    __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |  |   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __    __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|

```



## JavaScript


### Imperative


An implementation of GO. Prints an SVG string that can be read in a browser.

```javascript
const hilbert = (width, spacing, points) => (x, y, lg, i1, i2, f) => {
    if (lg === 1) {
        const px = (width - x) * spacing;
        const py = (width - y) * spacing;
        points.push(px, py);
        return;
    }
    lg >>= 1;
    f(x + i1 * lg, y + i1 * lg, lg, i1, 1 - i2, f);
    f(x + i2 * lg, y + (1 - i2) * lg, lg, i1, i2, f);
    f(x + (1 - i1) * lg, y + (1 - i1) * lg, lg, i1, i2, f);
    f(x + (1 - i2) * lg, y + i2 * lg, lg, 1 - i1, i2, f);
    return points;
};

/**
 * Draw a hilbert curve of the given order.
 * Outputs a svg string. Save the string as a .svg file and open in a browser.
 * @param {!Number} order
 */
const drawHilbert = order => {
    if (!order || order < 1) {
        throw 'You need to give a valid positive integer';
    } else {
        order = Math.floor(order);
    }


    // Curve Constants
    const width = 2 ** order;
    const space = 10;

    // SVG Setup
    const size = 500;
    const stroke = 2;
    const col = "red";
    const fill = "transparent";

    // Prep and run function
    const f = hilbert(width, space, []);
    const points = f(0, 0, width, 0, 0, f);
    const path = points.join(' ');

    console.log(
        `<svg xmlns="http://www.w3.org/2000/svg"
    width="${size}"
    height="${size}"
    viewBox="${space / 2} ${space / 2} ${width * space} ${width * space}">
  <path d="M${path}" stroke-width="${stroke}" stroke="${col}" fill="${fill}"/>
</svg>`);

};

drawHilbert(6);
```



### Functional

{{Trans|Python}}

A composition of pure functions which defines a Hilbert tree as the Nth application of a production rule to a seedling tree.

A list of points is derived by serialization of that tree.

Like the version above, generates an SVG string for display in a browser.


```JavaScript
(() => {
    'use strict';

    const main = () => {

        // rule :: Dict Char [Char]
        const rule = {
            a: ['d', 'a', 'a', 'b'],
            b: ['c', 'b', 'b', 'a'],
            c: ['b', 'c', 'c', 'd'],
            d: ['a', 'd', 'd', 'c']
        };

        // vectors :: Dict Char [(Int, Int)]
        const vectors = ({
            'a': [
                [-1, 1],
                [-1, -1],
                [1, -1],
                [1, 1]
            ],
            'b': [
                [1, -1],
                [-1, -1],
                [-1, 1],
                [1, 1]
            ],
            'c': [
                [1, -1],
                [1, 1],
                [-1, 1],
                [-1, -1]
            ],
            'd': [
                [-1, 1],
                [1, 1],
                [1, -1],
                [-1, -1]
            ]
        });

        // hilbertCurve :: Int -> SVG string
        const hilbertCurve = n => {
            const w = 1024
            return svgFromPoints(w)(
                hilbertPoints(w)(
                    hilbertTree(n)
                )
            );
        }

        // hilbertTree :: Int -> Tree Char
        const hilbertTree = n => {
            const go = tree =>
                Node(
                    tree.root,
                    0 < tree.nest.length ? (
                        map(go, tree.nest)
                    ) : map(x => Node(x, []), rule[tree.root])
                );
            const seed = Node('a', []);
            return 0 < n ? (
                take(n, iterate(go, seed)).slice(-1)[0]
            ) : seed;
        };

        // hilbertPoints :: Size -> Tree Char -> [(x, y)]
        // hilbertPoints :: Int -> Tree Char -> [(Int, Int)]
        const hilbertPoints = w => tree => {
            const go = d => (xy, tree) => {
                const
                    r = Math.floor(d / 2),
                    centres = map(
                        v => [
                            xy[0] + (r * v[0]),
                            xy[1] + (r * v[1])
                        ],
                        vectors[tree.root]
                    );
                return 0 < tree.nest.length ? concat(
                    zipWith(go(r), centres, tree.nest)
                ) : centres;
            };
            const d = Math.floor(w / 2);
            return go(d)([d, d], tree);
        };

        // svgFromPoints :: Int -> [(Int, Int)] -> String
        const svgFromPoints = w => xys =>
            ['<svg xmlns="http://www.w3.org/2000/svg"',
                `width="500" height="500" viewBox="5 5 ${w} ${w}">`,
                `<path d="M${concat(xys).join(' ')}" `,
                'stroke-width="2" stroke="red" fill="transparent"/>',
                '</svg>'
            ].join('\n');

        // TEST -------------------------------------------
        console.log(
            hilbertCurve(6)
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Node :: a -> [Tree a] -> Tree a
    const Node = (v, xs) => ({
        type: 'Node',
        root: v, // any type of value (consistent across tree)
        nest: xs || []
    });

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // iterate :: (a -> a) -> a -> Gen [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```



## Julia

Color graphics version using the Gtk package.

```julia
using Gtk, Graphics, Colors

Base.isless(p1::Vec2, p2::Vec2) = (p1.x == p2.x ? p1.y < p2.y : p1.x < p2.x)

struct Line
	p1::Point
	p2::Point
end

dist(p1, p2) = sqrt((p2.y - p1.y)^2 + (p2.x - p1.x)^2)
length(ln::Line) = dist(ln.p1, ln.p2)
isvertical(line) = (line.p1.x == line.p2.x)
ishorizontal(line) = (line.p1.y == line.p2.y)

const colorseq = [colorant"blue", colorant"red", colorant"green"]
const linewidth = 1
const toporder = 3

function drawline(ctx, p1, p2, color, width)
    move_to(ctx, p1.x, p1.y)
    set_source(ctx, color)
    line_to(ctx, p2.x, p2.y)
    set_line_width(ctx, width)
    stroke(ctx)
end
drawline(ctx, line, color, width=linewidth) = drawline(ctx, line.p1, line.p2, color, width)

function hilbertmutateboxes(ctx, line, order, maxorder=toporder)
    if line.p1 < line.p2
        p1, p2 = line.p1, line.p2
    else
        p2, p1 = line.p1, line.p2
    end
    color = colorseq[order % 3 + 1]
	d = dist(p1, p2) / 3
    if ishorizontal(line)
        pl = Point(p1.x + d, p1.y)
        plu = Point(p1.x + d, p1.y - d)
        pld = Point(p1.x + d, p1.y + d)
        pr = Point(p2.x - d, p2.y)
        pru = Point(p2.x - d, p2.y - d)
        prd = Point(p2.x - d, p2.y + d)
        lines = [Line(plu, pl), Line(plu, pru), Line(pru, pr),
                 Line(pr, prd), Line(pld, prd), Line(pld, pl)]
    else # vertical
        pu = Point(p1.x, p1.y + d)
        pul = Point(p1.x - d, p1.y + d)
        pur = Point(p1.x + d, p1.y + d)
        pd = Point(p2.x, p2.y - d)
        pdl = Point(p2.x - d, p2.y - d)
        pdr = Point(p2.x + d, p2.y - d)
        lines = [Line(pul, pu), Line(pul, pdl), Line(pdl, pd),
                 Line(pu, pur), Line(pur, pdr), Line(pd, pdr)]
    end
    for li in lines
        drawline(ctx, li, color)
    end
    if order <= maxorder
        for li in lines
            hilbertmutateboxes(ctx, li, order + 1, maxorder)
        end
    end
end


const can = @GtkCanvas()
const win = GtkWindow(can, "Hilbert 2D", 400, 400)

@guarded draw(can) do widget
    ctx = getgc(can)
    h = height(can)
    w = width(can)
    line = Line(Point(0, h/2), Point(w, h/2))
    drawline(ctx, line, colorant"black", 2)
    hilbertmutateboxes(ctx, line, 0)
end


show(can)
const cond = Condition()
endit(w) = notify(cond)
signal_connect(endit, win, :destroy)
wait(cond)

```



## Kotlin

Terminal drawing using ASCII characters within a 96 x 96 grid - starts at top left, ends at top right.

The coordinates of the points are generated using a translation of the C code in the Wikipedia article and then scaled by a factor of 3 (n = 32).

```scala
// Version 1.2.40

data class Point(var x: Int, var y: Int)

fun d2pt(n: Int, d: Int): Point {
    var x = 0
    var y = 0
    var t = d
    var s = 1
    while (s < n) {
        val rx = 1 and (t / 2)
        val ry = 1 and (t xor rx)
        val p = Point(x, y)
        rot(s, p, rx, ry)
        x = p.x + s * rx
        y = p.y + s * ry
        t /= 4
        s *= 2
    }
    return Point(x, y)
}

fun rot(n: Int, p: Point, rx: Int, ry: Int) {
    if (ry == 0) {
        if (rx == 1) {
            p.x = n - 1 - p.x
            p.y = n - 1 - p.y
        }
        val t  = p.x
        p.x = p.y
        p.y = t
    }
}

fun main(args:Array<String>) {
    val n = 32
    val k = 3
    val pts = List(n * k) { CharArray(n * k) { ' ' } }
    var prev = Point(0, 0)
    pts[0][0] = '.'
    for (d in 1 until n * n) {
        val curr = d2pt(n, d)
        val cx = curr.x * k
        val cy = curr.y * k
        val px = prev.x * k
        val py = prev.y * k
        pts[cx][cy] = '.'
        if (cx == px ) {
            if (py < cy)
                for (y in py + 1 until cy) pts[cx][y] = '|'
            else
                for (y in cy + 1 until py) pts[cx][y] = '|'
        }
        else {
            if (px < cx)
               for (x in px + 1 until cx) pts[x][cy] = '_'
            else
               for (x in cx + 1 until px) pts[x][cy] = '_'
        }
        prev = curr
    }
    for (i in 0 until n * k) {
        for (j in 0 until n * k) print(pts[j][i])
        println()
    }
}
```


{{output}}

```txt
.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .
|  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |
|  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
      |        |        |        |        |        |        |        |        |        |
      |        |        |        |        |        |        |        |        |        |
.__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.
|  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |
|  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |
.  .__.__.  .__.__.  .  .__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.  .  .__.__.  .__.__.  .
|                    |              |                    |              |                    |
|                    |              |                    |              |                    |
.__.  .__.__.__.  .__.  .__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.  .__.  .__.__.__.  .__.
   |  |        |  |     |  |  |  |  |  |     |  |     |  |  |  |  |  |     |  |        |  |
   |  |        |  |     |  |  |  |  |  |     |  |     |  |  |  |  |  |     |  |        |  |
.__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.
|        |  |        |  |        |        |        |        |        |  |        |  |        |
|        |  |        |  |        |        |        |        |        |  |        |  |        |
.  .__.  .  .  .__.  .  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .  .__.  .  .  .__.  .
|  |  |  |  |  |  |  |     |  |     |  |     |  |     |  |     |  |     |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |     |  |     |  |     |  |     |  |     |  |     |  |  |  |  |  |  |  |
.__.  .__.  .__.  .__.  .__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.  .__.  .__.  .__.  .__.
                        |                                            |
                        |                                            |
.__.  .__.  .__.  .__.  .__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.  .__.  .__.  .__.  .__.
|  |  |  |  |  |  |  |     |  |     |  |     |  |     |  |     |  |     |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |     |  |     |  |     |  |     |  |     |  |     |  |  |  |  |  |  |  |
.  .__.  .  .  .__.  .  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .  .__.  .  .  .__.  .
|        |  |        |  |        |        |        |        |        |  |        |  |        |
|        |  |        |  |        |        |        |        |        |  |        |  |        |
.__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.
   |  |        |  |     |  |  |  |  |  |     |  |     |  |  |  |  |  |     |  |        |  |
   |  |        |  |     |  |  |  |  |  |     |  |     |  |  |  |  |  |     |  |        |  |
.__.  .__.__.__.  .__.  .__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.  .__.  .__.__.__.  .__.
|                    |              |                    |              |                    |
|                    |              |                    |              |                    |
.  .__.__.  .__.__.  .  .__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.  .  .__.__.  .__.__.  .
|  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |
|  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |     |  |     |  |
.__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .  .__.  .__.  .__.  .__.
      |        |        |        |        |        |        |        |        |        |
      |        |        |        |        |        |        |        |        |        |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
|  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |
|  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |     |  |
.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .
|                                                                                            |
|                                                                                            |
.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.__.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.
   |  |     |  |     |  |     |  |     |  |        |  |     |  |     |  |     |  |     |  |
   |  |     |  |     |  |     |  |     |  |        |  |     |  |     |  |     |  |     |  |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
|        |        |        |        |        |  |        |        |        |        |        |
|        |        |        |        |        |  |        |        |        |        |        |
.  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .  .  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .
|  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |
|  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |
.__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.  .__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.
            |                    |                          |                    |
            |                    |                          |                    |
.__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.  .__.  .__.  .  .__.__.  .__.__.  .  .__.  .__.
|  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |
|  |  |  |  |  |     |  |     |  |  |  |  |  |  |  |  |  |  |  |     |  |     |  |  |  |  |  |
.  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .  .  .__.  .  .__.  .__.  .__.  .__.  .  .__.  .
|        |        |        |        |        |  |        |        |        |        |        |
|        |        |        |        |        |  |        |        |        |        |        |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
   |  |     |  |     |  |     |  |     |  |        |  |     |  |     |  |     |  |     |  |
   |  |     |  |     |  |     |  |     |  |        |  |     |  |     |  |     |  |     |  |
.__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.  .__.  .__.__.  .__.__.  .__.__.  .__.__.  .__.
|                                            |  |                                            |
|                                            |  |                                            |
.  .__.__.  .__.__.  .__.  .__.__.  .__.__.  .  .  .__.__.  .__.__.  .__.  .__.__.  .__.__.  .
|  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |
|  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
      |        |              |        |              |        |              |        |
      |        |              |        |              |        |              |        |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
|  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |
|  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |  |  |     |  |     |  |
.  .__.__.  .__.__.  .  .  .__.__.  .__.__.  .  .  .__.__.  .__.__.  .  .  .__.__.  .__.__.  .
|                    |  |                    |  |                    |  |                    |
|                    |  |                    |  |                    |  |                    |
.__.  .__.__.__.  .__.  .__.  .__.__.__.  .__.  .__.  .__.__.__.  .__.  .__.  .__.__.__.  .__.
   |  |        |  |        |  |        |  |        |  |        |  |        |  |        |  |
   |  |        |  |        |  |        |  |        |  |        |  |        |  |        |  |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
|        |  |        |  |        |  |        |  |        |  |        |  |        |  |        |
|        |  |        |  |        |  |        |  |        |  |        |  |        |  |        |
.  .__.  .  .  .__.  .  .  .__.  .  .  .__.  .  .  .__.  .  .  .__.  .  .  .__.  .  .  .__.  .
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
.__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.  .__.
```



## Lua

Solved by using the Lindenmayer path, printed with Unicode, which does not show perfectly on web,
but is quite nice on console. Should work with all Lua versions, used nothing special.
Should work up to Hilbert(12) if your console is big enough for that.

Implemented a full line-drawing Unicode/ASCII drawing and added for the example my signature to the default axiom "A" for fun and a second Hilbert "A" at the end, because it's looking better in the display like that. The implementation of repeated commands was just an additional line of code, so why not?

Lindenmayer:

* A,B are Lindenmayer AXIOMS

Line drawing:

* +,- turn right, left
* F   draw line forward
* <num> repeat the following draw command <num> times
* <any> move on canvas without drawing

```lua
-- any version from LuaJIT 2.0/5.1, Lua 5.2, Lua 5.3 to LuaJIT 2.1.0-beta3-readline
local bit=bit32 or bit -- Lua 5.2/5.3 compatibilty
-- Hilbert curve implemented by Lindenmayer system
function string.hilbert(s, n)
	for i=1,n do
		s=s:gsub("[AB]",function(c)
			if c=="A" then
				c="-BF+AFA+FB-"
			else
				c="+AF-BFB-FA+"
			end
			return c
		end)
	end
	s=s:gsub("[AB]",""):gsub("%+%-",""):gsub("%-%+","")
	return s
end
-- Or the characters for ASCII line drawing
function charor(c1, c2)
	local bits={
		[" "]=0x0, ["╷"]=0x1, ["╶"]=0x2, ["┌"]=0x3, ["╵"]=0x4, ["│"]=0x5, ["└"]=0x6, ["├"]=0x7,
		["╴"]=0x8, ["┐"]=0x9, ["─"]=0xa, ["┬"]=0xb, ["┘"]=0xc, ["┤"]=0xd, ["┴"]=0xe, ["┼"]=0xf,}
	local char={" ", "╷", "╶", "┌", "╵", "│", "└", "├", "╴", "┐", "─", "┬", "┘", "┤", "┴", "┼",}
	local b1,b2=bits[c1] or 0,bits[c2] or 0
	return char[bit.bor(b1,b2)+1]
end
-- ASCII line drawing routine
function draw(s)
	local char={
		{"─","┘","╴","┐",}, -- r
		{"│","┐","╷","┌",}, -- up
		{"─","┌","╶","└",}, -- l
		{"│","└","╵","┘",},	-- down
	}
	local scr={}
	local move={{x=1,y=0},{x=0,y=1},{x=-1,y=0},{x=0,y=-1}}
	local x,y=1,1
	local minx,maxx,miny,maxy=1,1,1,1
	local dir,turn=0,0
	s=s.."F"
	local rep=0
	for c in s:gmatch(".") do
		if c=="F" then
			repeat
				if scr[y]==nil then scr[y]={} end
				scr[y][x]=charor(char[dir+1][turn%#char[1]+1],scr[y][x] or " ")
				dir = (dir+turn) % #move
				x, y = x+move[dir+1].x,y+move[dir+1].y
				maxx,maxy=math.max(maxx,x),math.max(maxy,y)
				minx,miny=math.min(minx,x),math.min(miny,y)
				turn=0
				rep=rep>1 and rep-1 or 0
			until rep==0
		elseif c=="-" then
			repeat
				turn=turn+1
				rep=rep>1 and rep-1 or 0
			until rep==0
		elseif c=="+" then
			repeat
				turn=turn-1
				rep=rep>1 and rep-1 or 0
			until rep==0
		elseif c:match("%d") then -- allow repeated commands
			rep=rep*10+tonumber(c)
		else
			repeat
				x, y = x+move[dir+1].x,y+move[dir+1].y
				maxx,maxy=math.max(maxx,x),math.max(maxy,y)
				minx,miny=math.min(minx,x),math.min(miny,y)
				rep=rep>1 and rep-1 or 0
			until rep==0
		end
	end
	for i=maxy,miny,-1 do
		local oneline={}
		for x=minx,maxx do
			oneline[1+x-minx]=scr[i] and scr[i][x] or " "
		end
		local line=table.concat(oneline)
		io.write(line, "\n")
	end
end
-- MAIN --
local n=arg[1] and tonumber(arg[1]) or 3
local str=arg[2] or "A"
draw(str:hilbert(n))

```

{{output}} luajit hilbert.lua 4 1M9FAF-4F2+2F-2F-2F++4F-F-4F+2F+2F+2F++3F+2F+3F--4FA10F-16F-58F-16F-

```txt

┌─────────────────────────────────────────────────────────┐
│         ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐       ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐         │
│         │└┘││└┘││└┘││└┘│       │└┘││└┘││└┘││└┘│         │
│         └┐┌┘└┐┌┘└┐┌┘└┐┌┘       └┐┌┘└┐┌┘└┐┌┘└┐┌┘         │
│         ┌┘└──┘└┐┌┘└──┘└┐       ┌┘└──┘└┐┌┘└──┘└┐         │
│         │┌─┐┌─┐││┌─┐┌─┐│       │┌─┐┌─┐││┌─┐┌─┐│         │
│         └┘┌┘└┐└┘└┘┌┘└┐└┘       └┘┌┘└┐└┘└┘┌┘└┐└┘         │
│         ┌┐└┐┌┘┌┐┌┐└┐┌┘┌┐       ┌┐└┐┌┘┌┐┌┐└┐┌┘┌┐         │
│         │└─┘└─┘└┘└─┘└─┘│       │└─┘└─┘└┘└─┘└─┘│         │
│         └┐┌─┐┌─┐┌─┐┌─┐┌┘       └┐┌─┐┌─┐┌─┐┌─┐┌┘         │
│         ┌┘└┐└┘┌┘└┐└┘┌┘└┐       ┌┘└┐└┘┌┘└┐└┘┌┘└┐         │
│         │┌┐│┌┐└┐┌┘┌┐│┌┐│       │┌┐│┌┐└┐┌┘┌┐│┌┐│         │
│         └┘└┘│└─┘└─┘│└┘└┘╷ ╷┌─┐ └┘└┘│└─┘└─┘│└┘└┘         │
│         ┌┐┌┐│┌─┐┌─┐│┌┐┌┐│ ││ │ ┌┐┌┐│┌─┐┌─┐│┌┐┌┐         │
│         │└┘│└┘┌┘└┐└┘│└┘│├─┤├─┴┐│└┘│└┘┌┘└┐└┘│└┘│         │
│         └┐┌┘┌┐└┐┌┘┌┐└┐┌┘│ ││  │└┐┌┘┌┐└┐┌┘┌┐└┐┌┘         │
└──────────┘└─┘└─┘└─┘└─┘└─┘ └┴──┴─┘└─┘└─┘└─┘└─┘└──────────┘


```



## Mathematica

'''Works with:''' Mathematica 11

```Mathematica
Graphics@HilbertCurve[4]
```



## Perl


```perl
use SVG;
use List::Util qw(max min);

use constant pi => 2 * atan2(1, 0);

# Compute the curve with a Lindemayer-system
%rules = (
    A => '-BF+AFA+FB-',
    B => '+AF-BFB-FA+'
);
$hilbert = 'A';
$hilbert =~ s/([AB])/$rules{$1}/eg for 1..6;

# Draw the curve in SVG
($x, $y) = (0, 0);
$theta   = pi/2;
$r       = 5;

for (split //, $hilbert) {
    if (/F/) {
        push @X, sprintf "%.0f", $x;
        push @Y, sprintf "%.0f", $y;
        $x += $r * cos($theta);
        $y += $r * sin($theta);
    }
    elsif (/\+/) { $theta += pi/2; }
    elsif (/\-/) { $theta -= pi/2; }
}

$max =  max(@X,@Y);
$xt  = -min(@X)+10;
$yt  = -min(@Y)+10;
$svg = SVG->new(width=>$max+20, height=>$max+20);
$points = $svg->get_path(x=>\@X, y=>\@Y, -type=>'polyline');
$svg->rect(width=>"100%", height=>"100%", style=>{'fill'=>'black'});
$svg->polyline(%$points, style=>{'stroke'=>'orange', 'stroke-width'=>1}, transform=>"translate($xt,$yt)");

open  $fh, '>', 'hilbert_curve.svg';
print $fh  $svg->xmlify(-namespace=>'svg');
close $fh;
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/hilbert_curve.svg Hilbert curve] (offsite image)


## Perl 6

{{works with|Rakudo|2018.03}}


```perl6
use SVG;

role Lindenmayer {
    has %.rules;
    method succ {
        self.comb.map( { %!rules{$^c} // $c } ).join but Lindenmayer(%!rules)
    }
}

my $hilbert = 'A' but Lindenmayer( { A => '-BF+AFA+FB-', B => '+AF-BFB-FA+' } );

$hilbert++ xx 7;
my @points = (647, 13);

for $hilbert.comb {
    state ($x, $y) = @points[0,1];
    state $d = -5 - 0i;
    when 'F' { @points.append: ($x += $d.re).round(1), ($y += $d.im).round(1) }
    when /< + - >/ { $d *= "{$_}1i" }
    default { }
}

say SVG.serialize(
    svg => [
        :660width, :660height, :style<stroke:blue>,
        :rect[:width<100%>, :height<100%>, :fill<white>],
        :polyline[ :points(@points.join: ','), :fill<white> ],
    ],
);
```

See: [https://github.com/thundergnat/rc/blob/master/img/hilbert-perl6.svg Hilbert curve]

There is a variation of a Hilbert curve known as a [[wp:Moore curve|Moore curve]] which is essentially 4 Hilbert curves joined together in a loop.

```perl6
use SVG;

role Lindenmayer {
    has %.rules;
    method succ {
        self.comb.map( { %!rules{$^c} // $c } ).join but Lindenmayer(%!rules)
    }
}

my $moore = 'AFA+F+AFA' but Lindenmayer( { A => '-BF+AFA+FB-', B => '+AF-BFB-FA+' } );

$moore++ xx 6;
my @points = (327, 647);

for $moore.comb {
    state ($x, $y) = @points[0,1];
    state $d = 0 - 5i;
    when 'F' { @points.append: ($x += $d.re).round(1), ($y += $d.im).round(1) }
    when /< + - >/ { $d *= "{$_}1i" }
    default { }
}

say SVG.serialize(
    svg => [
        :660width, :660height, :style<stroke:darkviolet>,
        :rect[:width<100%>, :height<100%>, :fill<white>],
        :polyline[ :points(@points.join: ','), :fill<white> ],
    ],
);
```

See: [https://github.com/thundergnat/rc/blob/master/img/moore-perl6.svg Moore curve]


## Phix

{{libheader|pGUI}}
{{trans|Go}}

```Phix
-- demo\rosetta\hilbert_curve.exw
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

constant width = 64

sequence points = {}

procedure hilbert(integer x, y, lg, i1, i2)
    if lg=1 then
        integer px := (width-x) * 10,
                py := (width-y) * 10
        points = append(points, {px, py})
        return
    end if
    lg /= 2
    hilbert(x+i1*lg, y+i1*lg, lg, i1, 1-i2)
    hilbert(x+i2*lg, y+(1-i2)*lg, lg, i1, i2)
    hilbert(x+(1-i1)*lg, y+(1-i1)*lg, lg, i1, i2)
    hilbert(x+(1-i2)*lg, y+i2*lg, lg, 1-i1, i2)
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    cdCanvasBegin(cddbuffer, CD_OPEN_LINES)
    for i=1 to length(points) do
        integer {x,y} = points[i]
        cdCanvasVertex(cddbuffer, x, y)
    end for
    cdCanvasEnd(cddbuffer)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_MAGENTA)
    return IUP_DEFAULT
end function

procedure main()
    hilbert(0, 0, width, 0, 0)
    IupOpen()
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "655x655")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Hilbert Curve")
    IupSetAttribute(dlg, "DIALOGFRAME", "YES") -- no resize here
    IupCloseOnEscape(dlg)
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    IupMap(dlg)
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## Python


### Functional


Composition of pure functions, with type comments for the reader rather than the compiler.

An SVG path is serialised from the Nth application of re-write rules to a Hilbert tree structure.

(To view the Hilbert curve, save the output SVG text in a file with an appropriate extension (e.g. '''.svg'''), and open it with a browser).

{{Works with|Python|3.7}}

```Python
'''Hilbert curve'''

from itertools import (chain, islice, starmap)
from inspect import signature


# hilbertCurve :: Int -> SVG String
def hilbertCurve(n):
    '''An SVG string representing a
       Hilbert curve of degree n.
    '''
    w = 1024
    return svgFromPoints(w)(
        hilbertPoints(w)(
            hilbertTree(n)
        )
    )


# hilbertTree :: Int -> Tree Char
def hilbertTree(n):
    '''Nth application of a rule to a seedling tree.'''

    # rule :: Dict Char [Char]
    rule = {
        'a': ['d', 'a', 'a', 'b'],
        'b': ['c', 'b', 'b', 'a'],
        'c': ['b', 'c', 'c', 'd'],
        'd': ['a', 'd', 'd', 'c']
    }

    # go :: Tree Char -> Tree Char
    def go(tree):
        c = tree['root']
        xs = tree['nest']
        return Node(c)(
            map(go, xs) if xs else map(
                flip(Node)([]),
                rule[c]
            )
        )
    seed = Node('a')([])
    return list(islice(
        iterate(go)(seed), n
    ))[-1] if 0 < n else seed


# hilbertPoints :: Int -> Tree Char -> [(Int, Int)]
def hilbertPoints(w):
    '''Serialization of a tree to a list of points
       bounded by a square of side w.
    '''

    # vectors :: Dict Char [(Int, Int)]
    vectors = {
        'a': [(-1, 1), (-1, -1), (1, -1), (1, 1)],
        'b': [(1, -1), (-1, -1), (-1, 1), (1, 1)],
        'c': [(1, -1), (1, 1), (-1, 1), (-1, -1)],
        'd': [(-1, 1), (1, 1), (1, -1), (-1, -1)]
    }

    # points :: Int -> ((Int, Int), Tree Char) -> [(Int, Int)]
    def points(d):
        '''Size -> Centre of a Hilbert subtree -> All subtree points
        '''
        def go(xy, tree):
            r = d // 2
            centres = map(
                lambda v: (
                    xy[0] + (r * v[0]),
                    xy[1] + (r * v[1])
                ),
                vectors[tree['root']]
            )
            return chain.from_iterable(
                starmap(points(r), zip(centres, tree['nest']))
            ) if tree['nest'] else centres
        return lambda xy, tree: go(xy, tree)

    d = w // 2
    return lambda tree: list(points(d)((d, d), tree))


# svgFromPoints :: Int -> [(Int, Int)] -> SVG String
def svgFromPoints(w):
    '''Width of square canvas -> Point list -> SVG string'''

    def go(w, xys):
        xs = ' '.join(map(
            lambda xy: str(xy[0]) + ' ' + str(xy[1]),
            xys
        ))
        return '\n'.join(
            ['<svg xmlns="http://www.w3.org/2000/svg"',
             f'width="512" height="512" viewBox="5 5 {w} {w}">',
             f'<path d="M{xs}" ',
             'stroke-width="2" stroke="red" fill="transparent"/>',
             '</svg>'
             ]
        )
    return lambda xys: go(w, xys)


# TEST ----------------------------------------------------
def main():
    '''Testing generation of the SVG for a Hilbert curve'''
    print(
        hilbertCurve(6)
    )


# GENERIC FUNCTIONS ---------------------------------------

# Node :: a -> [Tree a] -> Tree a
def Node(v):
    '''Contructor for a Tree node which connects a
       value of some kind to a list of zero or
       more child trees.'''
    return lambda xs: {'type': 'Node', 'root': v, 'nest': xs}


# flip :: (a -> b -> c) -> b -> a -> c
def flip(f):
    '''The (curried or uncurried) function f with its
       arguments reversed.'''
    if 1 < len(signature(f).parameters):
        return lambda a, b: f(b, a)
    else:
        return lambda a: lambda b: f(b)(a)


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated
       applications of f to x.
    '''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


#  TEST ---------------------------------------------------
if __name__ == '__main__':
    main()
```



## Ring


```ring

# Project : Hilbert curve

load "guilib.ring"

paint = null
x1 = 0
y1 = 0

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("Hilbert curve")
                  setgeometry(100,100,400,500)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,400,400)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(150,400,100,30)
                          settext("draw")
                          setclickevent("draw()")
                  }
                  show()
        }
        exec()
        }

func draw
        p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
        }
        pen = new qpen() {
                 setcolor(color)
                 setwidth(1)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

        x1 = 0.5
        y1 = 0.5
        hilbert(0, 0, 200,  0,  0,  200,  4)

        endpaint()
        }
        label1 { setpicture(p1) show() }

func hilbert (x, y, xi, xj, yi, yj, n)
        cur = new QCursor() {
                 setpos(100, 100)
        }

        if (n <= 0)
           drawtoline(x + (xi + yi)/2, y + (xj + yj)/2)
       else
           hilbert(x, y, yi/2, yj/2, xi/2, xj/2, n-1)
           hilbert(x+xi/2, y+xj/2 , xi/2, xj/2, yi/2, yj/2, n-1)
           hilbert(x+xi/2+yi/2, y+xj/2+yj/2, xi/2, xj/2, yi/2, yj/2, n-1);
           hilbert(x+xi/2+yi, y+xj/2+yj, -yi/2,-yj/2, -xi/2, -xj/2, n-1)
       ok

func drawtoline x2, y2
        paint.drawline(x1, y1, x2, y2)
        x1 = x2
        y1 = y2

```

Output image:
[https://www.dropbox.com/s/anwtxtrnqhubh4a/HilbertCurve.jpg?dl=0 Hilbert curve]


## Racket


{{trans|Perl}}


```racket
#lang racket

(require racket/draw)

(define rules '([A . (- B F + A F A + F B -)]
                [B . (+ A F - B F B - F A +)]))

(define (get-cmds n cmd)
  (cond
    [(= 0 n) (list cmd)]
    [else (append-map (curry get-cmds (sub1 n))
                      (dict-ref rules cmd (list cmd)))]))

(define (make-curve DIM N R OFFSET COLOR BACKGROUND-COLOR)
  (define target (make-bitmap DIM DIM))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-background BACKGROUND-COLOR)
  (send dc set-pen COLOR 1 'solid)
  (send dc clear)
  (for/fold ([x 0] [y 0] [θ (/ pi 2)])
            ([cmd (in-list (get-cmds N 'A))])
    (define (draw/values x* y* θ*)
      (send/apply dc draw-line (map (curry + OFFSET) (list x y x* y*)))
      (values x* y* θ*))
    (match cmd
      ['F (draw/values (+ x (* R (cos θ))) (+ y (* R (sin θ))) θ)]
      ['+ (values x y (+ θ (/ pi 2)))]
      ['- (values x y (- θ (/ pi 2)))]
      [_  (values x y θ)]))
  target)

(make-curve 500 6 7 30 (make-color 255 255 0) (make-color 0 0 0))
```



## Scala

===[https://www.scala-js.org/ Scala.js]===

```Scala
@js.annotation.JSExportTopLevel("ScalaFiddle")
object ScalaFiddle {
  // $FiddleStart
  import scala.util.Random

  case class Point(x: Int, y: Int)

  def xy2d(order: Int, d: Int): Point = {
    def rot(order: Int, p: Point, rx: Int, ry: Int): Point = {
      val np = if (rx == 1) Point(order - 1 - p.x, order - 1 - p.y) else p
      if (ry == 0) Point(np.y, np.x) else p
    }

    @scala.annotation.tailrec
    def iter(rx: Int, ry: Int, s: Int, t: Int, p: Point): Point = {
      if (s < order) {
        val _rx = 1 & (t / 2)
        val _ry = 1 & (t ^ _rx)
        val temp = rot(s, p, _rx, _ry)
        iter(_rx, _ry, s * 2, t / 4, Point(temp.x + s * _rx, temp.y + s * _ry))
      } else p
    }

    iter(0, 0, 1, d, Point(0, 0))
  }

  def randomColor =
    s"rgb(${Random.nextInt(240)}, ${Random.nextInt(240)}, ${Random.nextInt(240)})"

  val order = 64
  val factor = math.min(Fiddle.canvas.height, Fiddle.canvas.width) / order.toDouble
  val maxD = order * order
  var d = 0
  Fiddle.draw.strokeStyle = randomColor
  Fiddle.draw.lineWidth = 2
  Fiddle.draw.lineCap = "square"

  Fiddle.schedule(10) {
    val h = xy2d(order, d)
    Fiddle.draw.lineTo(h.x * factor, h.y * factor)
    Fiddle.draw.stroke
    if ({d += 1; d >= maxD})
    {d = 1; Fiddle.draw.strokeStyle = randomColor}
    Fiddle.draw.beginPath
    Fiddle.draw.moveTo(h.x * factor, h.y * factor)
  }
  // $FiddleEnd
}
```

{{Out}}Best seen running in your browser by [https://scalafiddle.io/sf/x7t2zdK/0 ScalaFiddle (ES aka JavaScript, non JVM)].


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "keybd.s7i";

const integer: delta is 8;

const proc: drawDown (inout integer: x, inout integer: y, in integer: n) is forward;
const proc: drawUp (inout integer: x, inout integer: y, in integer: n) is forward;

const proc: drawRight (inout integer: x, inout integer: y, in integer: n) is func
  begin
    if n > 0 then
      drawDown(x, y, pred(n));
      line(x, y, 0, delta, white);
      y +:= delta;
      drawRight(x, y, pred(n));
      line(x, y, delta, 0, white);
      x +:= delta;
      drawRight(x, y, pred(n));
      line(x, y, 0, -delta, white);
      y -:= delta;
      drawUp(x, y, pred(n));
    end if;
  end func;

const proc: drawLeft (inout integer: x, inout integer: y, in integer: n) is func
  begin
    if n > 0 then
      drawUp(x, y, pred(n));
      line(x, y, 0, -delta, white);
      y -:= delta;
      drawLeft(x, y, pred(n));
      line(x, y, -delta, 0, white);
      x -:= delta;
      drawLeft(x, y, pred(n));
      line(x, y, 0, delta, white);
      y +:= delta;
      drawDown(x, y, pred(n));
    end if;
  end func;

const proc: drawDown (inout integer: x, inout integer: y, in integer: n) is func
  begin
    if n > 0 then
      drawRight(x, y, pred(n));
      line(x, y, delta, 0, white);
      x +:= delta;
      drawDown(x, y, pred(n));
      line(x, y, 0, delta, white);
      y +:= delta;
      drawDown(x, y, pred(n));
      line(x, y, -delta, 0, white);
      x -:= delta;
      drawLeft(x, y, pred(n));
    end if;
  end func;

const proc: drawUp (inout integer: x, inout integer: y, in integer: n) is func
  begin
    if n > 0 then
      drawLeft(x, y, pred(n));
      line(x, y, -delta, 0, white);
      x -:= delta;
      drawUp(x, y, pred(n));
      line(x, y, 0, -delta, white);
      y -:= delta;
      drawUp(x, y, pred(n));
      line(x, y, delta, 0, white);
      x +:= delta;
      drawRight(x, y, pred(n));
    end if;
  end func;

const proc: main is func
  local
    var integer: x is 11;
    var integer: y is 11;
  begin
    screen(526, 526);
    KEYBOARD := GRAPH_KEYBOARD;
    drawRight(x, y, 6);
    readln(KEYBOARD);
  end func;
```



## Sidef


```ruby
require('Image::Magick')

class Turtle(
    x      = 500,
    y      = 500,
    angle  = 0,
    scale  = 1,
    mirror = 1,
    xoff   = 0,
    yoff   = 0,
    color  = 'black',
) {

    has im = %O<Image::Magick>.new(size => "#{x}x#{y}")

    method init {
        angle.deg2rad!
        im.ReadImage('canvas:white')
    }

    method forward(r) {
        var (newx, newy) = (x + r*sin(angle), y + r*-cos(angle))

        im.Draw(
            primitive => 'line',
            points    => join(' ',
                           int(x    * scale + xoff),
                           int(y    * scale + yoff),
                           int(newx * scale + xoff),
                           int(newy * scale + yoff),
                        ),
            stroke      => color,
            strokewidth => 1,
        )

        (x, y) = (newx, newy)
    }

    method save_as(filename) {
        im.Write(filename)
    }

    method turn(theta) {
        angle += theta*mirror
    }

    method state {
        [x, y, angle, mirror]
    }

    method setstate(state) {
        (x, y, angle, mirror) = state...
    }

    method mirror {
        mirror.neg!
    }
}

class LSystem(
    angle  = 90,
    scale  = 1,
    xoff   = 0,
    yoff   = 0,
    len    = 5,
    color  = 'black',
    width  = 500,
    height = 500,
    turn   = 0,
) {

    has stack = []
    has table = Hash()

    has turtle = Turtle(
        x:     width,
        y:     height,
        angle: turn,
        scale: scale,
        color: color,
        xoff:  xoff,
        yoff:  yoff,
    )

    method init {

        angle.deg2rad!
        turn.deg2rad!

        table = Hash(
            '+' => { turtle.turn(angle) },
            '-' => { turtle.turn(-angle) },
            ':' => { turtle.mirror },
            '[' => { stack.push(turtle.state) },
            ']' => { turtle.setstate(stack.pop) },
        )
    }

    method execute(string, repetitions, filename, rules) {

        repetitions.times {
            string.gsub!(/(.)/, {|c| rules{c} \\ c })
        }

        string.each_char { |c|
            if (table.contains(c)) {
                table{c}.run
            }
            elsif (c.contains(/^[[:upper:]]\z/)) {
                turtle.forward(len)
            }
        }

        turtle.save_as(filename)
    }
}

var rules = Hash(
    a => '-bF+aFa+Fb-',
    b => '+aF-bFb-Fa+',
)

var lsys = LSystem(
    width:  600,
    height: 600,

    xoff: -50,
    yoff: -50,

    len:   8,
    angle: 90,
    color: 'dark green',
)

lsys.execute('a', 6, "hilbert_curve.png", rules)
```

{{out}}
[https://github.com/trizen/rc/blob/master/img/hilbert-curve-sidef.png Hilbert curve]


## Vala

{{libheader|Gtk+-3.0}}


```vala
struct Point{
    int x;
    int y;
    Point(int px,int py){
        x=px;
        y=py;
    }
}

public class Hilbert : Gtk.DrawingArea {

    private int it = 1;
    private Point[] points;
    private const int WINSIZE = 300;

    public Hilbert() {
        set_size_request(WINSIZE, WINSIZE);
    }

    public void button_toggled_cb(Gtk.ToggleButton button){
        if(button.get_active()){
            it = int.parse(button.get_label());
            redraw_canvas();
        }
    }

    public override bool draw(Cairo.Context cr){
        int border_size = 20;
        int unit = (WINSIZE - 2 * border_size)/((1<<it)-1);

        //adjust border_size to center the drawing
        border_size = border_size + (WINSIZE - 2 * border_size - unit * ((1<<it)-1)) / 2;

        //white background
        cr.rectangle(0, 0, WINSIZE, WINSIZE);
        cr.set_source_rgb(1, 1, 1);
        cr.fill_preserve();
        cr.stroke();

        points = {};
        hilbert(0, 0, 1<<it, 0, 0);

        //magenta lines
        cr.set_source_rgb(1, 0, 1);

        // move to first point
        Point point = translate(border_size, WINSIZE, unit*points[0].x, unit*points[0].y);
        cr.move_to(point.x, point.y);

        foreach(Point i in points[1:points.length]){
            point = translate(border_size, WINSIZE, unit*i.x, unit*i.y);
            cr.line_to(point.x, point.y);
        }
        cr.stroke();
        return false;
    }

    private Point translate(int border_size, int size, int x, int y){
        return Point(border_size + x,size - border_size - y);
    }

    private void hilbert(int x, int y, int lg, int i1, int i2) {
        if (lg == 1) {
            points += Point(x,y);
            return;
        }
        lg >>= 1;
        hilbert(x+i1*lg,     y+i1*lg,     lg, i1,   1-i2);
        hilbert(x+i2*lg,     y+(1-i2)*lg, lg, i1,   i2);
        hilbert(x+(1-i1)*lg, y+(1-i1)*lg, lg, i1,   i2);
        hilbert(x+(1-i2)*lg, y+i2*lg,     lg, 1-i1, i2);
    }

    private void redraw_canvas(){
        var window = get_window();
        if (window == null)return;
        window.invalidate_region(window.get_clip_region(), true);
    }
}


int main(string[] args){
    Gtk.init (ref args);

    var window = new Gtk.Window();
    window.title = "Rosetta Code / Hilbert";
    window.window_position = Gtk.WindowPosition.CENTER;
    window.destroy.connect(Gtk.main_quit);
    window.set_resizable(false);

    var label = new Gtk.Label("Iterations:");

    // create radio buttons to select the number of iterations
    var rb1 = new Gtk.RadioButton(null);
    rb1.set_label("1");
    var rb2 = new Gtk.RadioButton.with_label_from_widget(rb1, "2");
    var rb3 = new Gtk.RadioButton.with_label_from_widget(rb1, "3");
    var rb4 = new Gtk.RadioButton.with_label_from_widget(rb1, "4");
    var rb5 = new Gtk.RadioButton.with_label_from_widget(rb1, "5");

    var hilbert = new Hilbert();

    rb1.toggled.connect(hilbert.button_toggled_cb);
    rb2.toggled.connect(hilbert.button_toggled_cb);
    rb3.toggled.connect(hilbert.button_toggled_cb);
    rb4.toggled.connect(hilbert.button_toggled_cb);
    rb5.toggled.connect(hilbert.button_toggled_cb);

    var box = new Gtk.Box(Gtk.Orientation.HORIZONTAL, 0);
    box.pack_start(label, false, false, 5);
    box.pack_start(rb1, false, false, 0);
    box.pack_start(rb2, false, false, 0);
    box.pack_start(rb3, false, false, 0);
    box.pack_start(rb4, false, false, 0);
    box.pack_start(rb5, false, false, 0);

    var grid = new Gtk.Grid();
    grid.attach(box, 0, 0, 1, 1);
    grid.attach(hilbert, 0, 1, 1, 1);
    grid.set_border_width(5);
    grid.set_row_spacing(5);

    window.add(grid);
    window.show_all();

    //initialise the drawing with iteration = 4
    rb4.set_active(true);

    Gtk.main();
    return 0;
}
```



## Visual Basic .NET

{{trans|D}}

```vbnet
Imports System.Text

Module Module1

    Sub Swap(Of T)(ByRef a As T, ByRef b As T)
        Dim c = a
        a = b
        b = c
    End Sub

    Structure Point
        Dim x As Integer
        Dim y As Integer

        'rotate/flip a quadrant appropriately
        Sub Rot(n As Integer, rx As Boolean, ry As Boolean)
            If Not ry Then
                If rx Then
                    x = (n - 1) - x
                    y = (n - 1) - y
                End If
                Swap(x, y)
            End If
        End Sub

        Public Overrides Function ToString() As String
            Return String.Format("({0}, {1})", x, y)
        End Function
    End Structure

    Function FromD(n As Integer, d As Integer) As Point
        Dim p As Point
        Dim rx As Boolean
        Dim ry As Boolean
        Dim t = d
        Dim s = 1
        While s < n
            rx = ((t And 2) <> 0)
            ry = (((t Xor If(rx, 1, 0)) And 1) <> 0)
            p.Rot(s, rx, ry)
            p.x += If(rx, s, 0)
            p.y += If(ry, s, 0)
            t >>= 2

            s <<= 1
        End While
        Return p
    End Function

    Function GetPointsForCurve(n As Integer) As List(Of Point)
        Dim points As New List(Of Point)
        Dim d = 0
        While d < n * n
            points.Add(FromD(n, d))
            d += 1
        End While
        Return points
    End Function

    Function DrawCurve(points As List(Of Point), n As Integer) As List(Of String)
        Dim canvas(n, n * 3 - 2) As Char
        For i = 1 To canvas.GetLength(0)
            For j = 1 To canvas.GetLength(1)
                canvas(i - 1, j - 1) = " "
            Next
        Next

        For i = 1 To points.Count - 1
            Dim lastPoint = points(i - 1)
            Dim curPoint = points(i)
            Dim deltaX = curPoint.x - lastPoint.x
            Dim deltaY = curPoint.y - lastPoint.y
            If deltaX = 0 Then
                'vertical line
                Dim row = Math.Max(curPoint.y, lastPoint.y)
                Dim col = curPoint.x * 3
                canvas(row, col) = "|"
            Else
                'horizontal line
                Dim row = curPoint.y
                Dim col = Math.Min(curPoint.x, lastPoint.x) * 3 + 1
                canvas(row, col) = "_"
                canvas(row, col + 1) = "_"
            End If
        Next

        Dim lines As New List(Of String)
        For i = 1 To canvas.GetLength(0)
            Dim sb As New StringBuilder
            For j = 1 To canvas.GetLength(1)
                sb.Append(canvas(i - 1, j - 1))
            Next
            lines.Add(sb.ToString())
        Next
        Return lines
    End Function

    Sub Main()
        For order = 1 To 5
            Dim n = 1 << order
            Dim points = GetPointsForCurve(n)
            Console.WriteLine("Hilbert curve, order={0}", order)
            Dim lines = DrawCurve(points, n)
            For Each line In lines
                Console.WriteLine(line)
            Next
            Console.WriteLine()
        Next
    End Sub

End Module
```

{{out}}

```txt
Hilbert curve, order=1

|__|


Hilbert curve, order=2
 __    __
 __|  |__
|   __   |
|__|  |__|


Hilbert curve, order=3
    __ __    __ __
|__|   __|  |__   |__|
 __   |__    __|   __
|  |__ __|  |__ __|  |
|__    __ __ __    __|
 __|  |__    __|  |__
|   __   |  |   __   |
|__|  |__|  |__|  |__|


Hilbert curve, order=4
 __    __ __    __ __    __ __    __ __    __
 __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|


Hilbert curve, order=5
    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __    __ __
|__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|   __|  |__   |__|
 __   |__    __|   __   |   __   |   __   |__    __|   __   |   __   |   __   |__    __|   __
|  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |  |__ __|  |__ __|  |
|__    __ __ __    __|   __    __   |   __ __    __ __   |   __    __   |__    __ __ __    __|
 __|  |__    __|  |__   |  |__|  |  |__|   __|  |__   |__|  |  |__|  |   __|  |__    __|  |__
|   __   |  |   __   |  |__    __|   __   |__    __|   __   |__    __|  |   __   |  |   __   |
|__|  |__|  |__|  |__|   __|  |__ __|  |__ __|  |__ __|  |__ __|  |__   |__|  |__|  |__|  |__|
 __    __    __    __   |__    __ __    __ __    __ __    __ __    __|   __    __    __    __
|  |__|  |  |  |__|  |   __|  |__   |__|   __|  |__   |__|   __|  |__   |  |__|  |  |  |__|  |
|__    __|  |__    __|  |   __   |   __   |__    __|   __   |   __   |  |__    __|  |__    __|
 __|  |__ __ __|  |__   |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|   __|  |__ __ __|  |__
|   __ __    __ __   |   __    __   |   __ __    __ __   |   __    __   |   __ __    __ __   |
|__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |__|   __|  |__   |__|
 __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __   |__    __|   __
|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |__ __|  |
|__    __ __    __ __    __ __    __ __    __ __ __    __ __    __ __    __ __    __ __    __|
 __|  |__   |__|   __|  |__   |__|   __|  |__    __|  |__   |__|   __|  |__   |__|   __|  |__
|   __   |   __   |__    __|   __   |   __   |  |   __   |   __   |__    __|   __   |   __   |
|__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|  |__|  |__|  |  |__ __|  |__ __|  |  |__|  |__|
 __    __   |   __ __    __ __   |   __    __    __    __   |   __ __    __ __   |   __    __
|  |__|  |  |__|   __|  |__   |__|  |  |__|  |  |  |__|  |  |__|   __|  |__   |__|  |  |__|  |
|__    __|   __   |__    __|   __   |__    __|  |__    __|   __   |__    __|   __   |__    __|
 __|  |__ __|  |__ __|  |__ __|  |__ __|  |__    __|  |__ __|  |__ __|  |__ __|  |__ __|  |__
|   __ __    __ __    __    __ __    __ __   |  |   __ __    __ __    __    __ __    __ __   |
|__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|  |__|   __|  |__   |__|
 __   |__    __|   __    __   |__    __|   __    __   |__    __|   __    __   |__    __|   __
|  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |  |  |__ __|  |__ __|  |
|__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|  |__    __ __ __    __|
 __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__    __|  |__
|   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |  |   __   |
|__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|
```



## Yabasic

{{trans|Go}}

```Yabasic
width = 64

sub hilbert(x, y, lg, i1, i2)
    if lg = 1 then
        line to (width-x) * 10, (width-y) * 10
        return
    end if
    lg = lg / 2
    hilbert(x+i1*lg, y+i1*lg, lg, i1, 1-i2)
    hilbert(x+i2*lg, y+(1-i2)*lg, lg, i1, i2)
    hilbert(x+(1-i1)*lg, y+(1-i1)*lg, lg, i1, i2)
    hilbert(x+(1-i2)*lg, y+i2*lg, lg, 1-i1, i2)
end sub

open window 655, 655

hilbert(0, 0, width, 0, 0)
```



## zkl

Uses Image Magick and
the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
hilbert(6) : turtle(_);

fcn hilbert(n){	// Lindenmayer system --> Data of As & Bs
   var [const] A="-BF+AFA+FB-", B="+AF-BFB-FA+";
   buf1,buf2 := Data(Void,"A").howza(3), Data().howza(3);  // characters
   do(n){
      buf1.pump(buf2.clear(),fcn(c){ if(c=="A") A else if(c=="B") B else c });
      t:=buf1; buf1=buf2; buf2=t;	// swap buffers
   }
   buf1		// n=6 --> 13,651 letters
}

fcn turtle(hilbert){
   const D=10;
   ds,dir := T( T(D,0), T(0,-D), T(-D,0), T(0,D) ), 0;  // turtle offsets
   dx,dy := ds[dir];
   img:=PPM(650,650); x,y:=10,10; color:=0x00ff00;
   hilbert.replace("A","").replace("B","");  // A & B are no-op during drawing
   foreach c in (hilbert){
      switch(c){
	 case("F"){ img.line(x,y, (x+=dx),(y+=dy), color) }  // draw forward
	 case("+"){ dir=(dir+1)%4; dx,dy = ds[dir] } // turn right 90*
	 case("-"){ dir=(dir-1)%4; dx,dy = ds[dir] } // turn left 90*
      }
   }
   img.writeJPGFile("hilbert.zkl.jpg");
}
```

Image at [http://www.zenkinetic.com/Images/RosettaCode/hilbert.zkl.jpg hilbert curve]

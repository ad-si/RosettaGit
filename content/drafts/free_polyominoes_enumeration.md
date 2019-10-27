+++
title = "Free polyominoes enumeration"
description = ""
date = 2019-06-20T12:53:30Z
aliases = []
[extra]
id = 18165
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
A [[wp:Polyomino|Polyomino]] is a plane geometric figure formed by joining one or more equal squares edge to edge. Free polyominoes are distinct when none is a translation, rotation, reflection or glide reflection of another polyomino.

<b>Task</b>: generate all the free polyominoes with n cells.

You can visualize them just as a sequence of the coordinate pairs of their cells (rank 5):

```txt
[(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)]
[(0, 0), (0, 1), (0, 2), (0, 3), (1, 0)]
[(0, 0), (0, 1), (0, 2), (0, 3), (1, 1)]
[(0, 0), (0, 1), (0, 2), (1, 0), (1, 1)]
[(0, 0), (0, 1), (0, 2), (1, 0), (1, 2)]
[(0, 0), (0, 1), (0, 2), (1, 0), (2, 0)]
[(0, 0), (0, 1), (0, 2), (1, 1), (2, 1)]
[(0, 0), (0, 1), (0, 2), (1, 2), (1, 3)]
[(0, 0), (0, 1), (1, 1), (1, 2), (2, 1)]
[(0, 0), (0, 1), (1, 1), (1, 2), (2, 2)]
[(0, 0), (0, 1), (1, 1), (2, 1), (2, 2)]
[(0, 1), (1, 0), (1, 1), (1, 2), (2, 1)]
```


But a better basic visualization is using ASCII art (rank 5):

```txt
#    ##   #    ##  ##  ###  #     #    #    #    #      #
#    #    ##   ##  #   #    ###   #    ###  ##   ###   ###
#    #    #    #   ##  #    #     ##    #    ##    #    #
#    #    #                        #
#
```

Or perhaps with corner characters (rank 5):

```txt
 ┌───┐   ┌─────┐     ┌─┐   ┌───┐   ┌───┐     ┌───┐     ┌───┐     ┌───┐   ┌─┐     ┌─────┐   ┌─┐     ┌─┐
 │   │   │ ┌───┘   ┌─┘ │   │ ┌─┘   │ ┌─┘   ┌─┘ ┌─┘     │ ┌─┘   ┌─┘ ┌─┘   │ └─┐   └─┐ ┌─┘   │ │   ┌─┘ └─┐
 │ ┌─┘   │ │       │ ┌─┘   │ │     │ └─┐   └─┐ │     ┌─┘ │     │ ┌─┘     │ ┌─┘     │ │     │ │   └─┐ ┌─┘
 └─┘     └─┘       │ │     │ │     └───┘     └─┘     └───┘     └─┘       │ │       └─┘     │ │     └─┘
                   └─┘     └─┘                                           └─┘               │ │
                                                                                           └─┘
```


For a slow but clear solution see this Haskell Wiki page:
http://www.haskell.org/haskellwiki/The_Monad.Reader/Issue5/Generating_Polyominoes

<b>Bonus Task</b>: you can create an alternative program (or specialize your first program) to generate very quickly just the number of distinct free polyominoes, and to show a sequence like:

1, 1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655, 17073, 63600, 238591, 901971, 3426576, ...

Number of free polyominoes (or square animals) with n cells:
http://oeis.org/A000105


; Cf.
[[Pentomino_tiling|Pentomino tiling]]

=={{header|C sharp|C#}}==
{{trans|D}}
Turns out the source for the counting only version of the D code example could be tweaked to show solutions as well.  The max rank can be changed by supplying a command line parameter.  The free polyominos of any rank can be displayed by changing the variable named '''target''' to a reasonable number.  This program will also indicate the estimated times for larger ranks.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace cppfpe
{
    class Program
    {
        static int n, ns;               // rank, rank squared
        static long[] AnyR;             // Any Rotation count
        static long[] nFlip;            // Non-Flipped count
        static long[] Frees;            // Free Polyominoes count
        static int[] fChk, fCkR;        // field checks
        static int fSiz, fWid;          // field size, width
        static int[] dirs;              // directions
        static int[] rotO, rotX, rotY;  // rotations
        static List<string> polys;      // results
        static int target;              // rank to display
        static int clipAt;              // max columns for display

        static int Main(string[] args)
        {
            polys = new List<string>();
            n = 11; if (!(args.Length == 0)) int.TryParse(args[0], out n);
            if (n < 1 || n > 24) return 1;
            target = 5;
            Console.WriteLine("Counting polyominoes to rank {0}...", n);
            clipAt = 120;
            DateTime start = DateTime.Now;
            CountEm();
            TimeSpan ti = DateTime.Now - start;
            if (polys.Count > 0)
            {
                Console.WriteLine("Displaying rank {0}:", target);
                Console.WriteLine(Assemble(polys));
            }
            Console.WriteLine("Displaying results:");
            Console.WriteLine(" n      All Rotations     Non-Flipped      Free Polys");
            for (int i = 1; i <= n; i++)
                Console.WriteLine("{0,2} :{1,17}{2,16}{3,16}", i, AnyR[i], nFlip[i], Frees[i]);
            Console.WriteLine(string.Format("Elasped: {0,2}d {1,2}h {2,2}m {3:00}s {4:000}ms",
                              ti.Days, ti.Hours, ti.Minutes, ti.Seconds, ti.Milliseconds).Replace("  0d ", "")
                              .Replace(" 0h", "").Replace(" 0m", "").Replace(" 00s", ""));
            long ms = (long)ti.TotalMilliseconds, lim = int.MaxValue >> 2;
            if (ms > 250)
            {
                Console.WriteLine("Estimated completion times:");
                for (int i = n + 1; i <= n + 10; i++)
                {
                    if (ms >= lim) break; ms += 44; ms <<= 2; ti = TimeSpan.FromMilliseconds(ms);
                    Console.WriteLine("{0,2} : {1,2}d {2,2}h {3,2}m {4:00}.{5:000}s", i, 
                        ti.Days, ti.Hours, ti.Minutes, ti.Seconds, ti.Milliseconds);
                }
            }
            if (System.Diagnostics.Debugger.IsAttached) Console.ReadKey();
            return 0;
        }

        static void CountEm()
        {
            ns = n * n;
            AnyR = new long[n + 1];
            nFlip = new long[n + 1];
            Frees = new long[n + 1];
            fWid = n * 2 - 2;
            fSiz = (n - 1) * (n - 1) * 2 + 1;
            int[] pnField = new int[fSiz];
            int[] pnPutList = new int[fSiz];
            fChk = new int[ns];
            fCkR = new int[ns];
            dirs = new int[] { 1, fWid, -1, -fWid };
            rotO = new int[] { 0, n - 1, ns - 1, ns - n, n - 1, 0, ns - n, ns - 1 };
            rotX = new int[] { 1, n, -1, -n, -1, n, 1, -n };
            rotY = new int[] { n, -1, -n, 1, n, 1, -n, -1 };
            Recurse(0, pnField, pnPutList, 0, 1);
        }

        static void Recurse(int lv, int[] field, int[] putlist, int putno, int putlast)
        {
            CheckIt(field, lv);
            if (n == lv) return;
            int pos;
            for (int i = putno; i < putlast; i++)
            {
                field[pos = putlist[i]] |= 1;
                int k = 0;
                foreach (int dir in dirs)
                {
                    int pos2 = pos + dir;
                    if (0 <= pos2 && pos2 < fSiz && (field[pos2] == 0))
                    {
                        field[pos2] = 2;
                        putlist[putlast + k++] = pos2;
                    }
                }
                Recurse(lv + 1, field, putlist, i + 1, putlast + k);
                for (int j = 0; j < k; j++) field[putlist[putlast + j]] = 0;
                field[pos] = 2;
            }
            for (int i = putno; i < putlast; i++) field[putlist[i]] &= -2;
        }

        static void CheckIt(int[] field, int lv)
        {
            AnyR[lv]++;
            for (int i = 0; i < ns; i++) fChk[i] = 0;
            int x, y;
            for (x = n; x < fWid; x++)
                for (y = 0; y + x < fSiz; y += fWid)
                    if ((field[x + y] & 1) == 1) goto bail;
            bail:
            int x2 = n - x, t;
            for (int i = 0; i < fSiz; i++)
                if ((field[i] & 1) == 1) fChk[((t = (i + n - 2)) % fWid) + x2 + (t / fWid * n)] = 1;
            int of1; for (of1 = 0; of1 < fChk.Length && (fChk[of1] == 0); of1++) ;
            bool c = true; int r;
            for (r = 1; r < 8 && c; r++)
            {
                for (x = 0; x < n; x++) for (y = 0; y < n; y++)
                        fCkR[rotO[r] + rotX[r] * x + rotY[r] * y] = fChk[x + y * n];
                int of2; for (of2 = 0; of2 < fCkR.Length && (fCkR[of2] == 0); of2++) ;
                of2 -= of1;
                for (int i = of1; i < ns - ((of2 > 0) ? of2 : 0); i++)
                {
                    if (fChk[i] > fCkR[i + of2]) break;
                    if (fChk[i] < fCkR[i + of2]) { c = false; break; }
                }
            }
            if (r > 4) nFlip[lv]++;
            if (c)
            {
                if (lv == target) polys.Add(toStr(field.ToArray()));
                Frees[lv]++;
            }
        }

        static string toStr(int[] field) // converts field into a minimal string
        {
            char [] res = new string(' ', n * (fWid + 1) - 1).ToCharArray();
            for (int i = fWid; i < res.Length; i += fWid+1) res[i] = '\n';
            for (int i = 0, j = n - 2; i < field.Length; i++, j++)
            {
                if ((field[i] & 1) == 1) res[j] = '#';
                if (j % (fWid + 1) == fWid) i--;
            }
            List<string> t = new string(res).Split('\n').ToList();
            int nn = 100, m = 0, v, k = 0; // trim down
            foreach (string s in t)
            {
                if ((v = s.IndexOf('#')) < nn) if (v >= 0) nn = v;
                if ((v = s.LastIndexOf('#')) > m) if (v < fWid +1) m = v;
                if (v < 0) break; k++;
            }
            m = m - nn + 1; // convert difference to length
            for (int i = t.Count - 1; i >= 0; i--)
            {
                if (i >= k) t.RemoveAt(i);
                else t[i] = t[i].Substring(nn, m);
            }
            return String.Join("\n", t.ToArray());
        }

        // assembles string representation of polyominoes into larger horizontal band
        static string Assemble(List<string> p)
        {
            List<string> lines = new List<string>();
            for (int i = 0; i < target; i++) lines.Add(string.Empty);
            foreach (string poly in p)
            {
                List<string> t = poly.Split('\n').ToList();
                if (t.Count < t[0].Length) t = flipXY(t);
                for (int i = 0; i < lines.Count; i++)
                    lines[i] += (i < t.Count) ? ' ' + t[i] + ' ': new string(' ', t[0].Length + 2);
            }
            for (int i = lines.Count - 1; i > 0; i--)
                if (lines[i].IndexOf('#') < 0) lines.RemoveAt(i);
            if (lines[0].Length >= clipAt / 2-2) Wrap(lines, clipAt / 2-2);
            lines = Cornered(string.Join("\n", lines.ToArray())).Split('\n').ToList();
            return String.Join("\n", lines.ToArray());
        }

        static List<string> flipXY(List<string> p)  // flips a small string
        {
            List<string> res = new List<string>();
            for (int i = 0; i < p[0].Length; i++) res.Add(string.Empty);
            for (int i = 0; i < res.Count; i++)
                for(int j = 0; j < p.Count; j++) res[i] += p[j][i];
            return res;
        }

        static string DW(string s)  // double widths a string
        {
            string t = string.Empty;
            foreach (char c in s) t += string.Format("{0}{0}",c);
            return t;
        }

        static void Wrap(List<string> s, int w) // wraps a wide List<string>
        {
            int last = 0;
            while (s.Last().Length >= w)
            {
                int x = w, lim = s.Count; bool ok;
                do
                {
                    ok = true;
                    for (int i = last; i < lim; i++)
                        if (s[i][x] != ' ')
                        { ok = false; x--; break; }
                } while (!ok);
                for (int i = last; i < lim; i++)
                    if (s[i].Length > x) { s.Add(s[i].Substring(x)); s[i] = s[i].Substring(0, x + 1); }
                last = lim;
            }
            last = 0;
            for (int i = s.Count - 1; i > 0; i--)
                if ((last = (s[i].IndexOf('#') < 0) ? last + 1 : 0) > 1) s.RemoveAt(i + 1);
        }

        static string Cornered(string s)    // converts plain ascii art into cornered version
        {
            string[] lines = s.Split('\n');
            string res = string.Empty;
            string line = DW(new string(' ', lines[0].Length)), last;
            for (int i = 0; i < lines.Length; i++)
            {
                last = line; line = DW(lines[i]);
                res += Puzzle(last, line) + '\n';
            }
            res += Puzzle(line, DW(new string(' ', lines.Last().Length))) + '\n';
            return res;
        }

        static string Puzzle(string a, string b)    // tests each intersection to determine correct corner symbol
        {
            string res = string.Empty;
            if (a.Length > b.Length) b += new string(' ', a.Length - b.Length);
            if (a.Length < b.Length) a += new string(' ', b.Length - a.Length);
            for (int i = 0; i < a.Length - 1; i++)
                res += " 12└4┘─┴8│┌├┐┤┬┼"[(a[i] == a[i + 1] ? 0 : 1) + 
                                          (b[i + 1] == a[i + 1] ? 0 : 2) +
                                          (a[i] == b[i] ? 0 : 4) + 
                                          (b[i] == b[i + 1] ? 0 : 8)];
            return res;
        }
    }
}

```

{{out}}

```txt
Counting polyominoes to rank 11...
Displaying rank 5:
 ┌───┐   ┌─────┐     ┌─┐   ┌───┐   ┌───┐     ┌───┐     ┌───┐     ┌───┐   ┌─┐     ┌─────┐   ┌─┐     ┌─┐
 │   │   │ ┌───┘   ┌─┘ │   │ ┌─┘   │ ┌─┘   ┌─┘ ┌─┘     │ ┌─┘   ┌─┘ ┌─┘   │ └─┐   └─┐ ┌─┘   │ │   ┌─┘ └─┐
 │ ┌─┘   │ │       │ ┌─┘   │ │     │ └─┐   └─┐ │     ┌─┘ │     │ ┌─┘     │ ┌─┘     │ │     │ │   └─┐ ┌─┘
 └─┘     └─┘       │ │     │ │     └───┘     └─┘     └───┘     └─┘       │ │       └─┘     │ │     └─┘
                   └─┘     └─┘                                           └─┘               │ │
                                                                                           └─┘

Displaying results:
 n      All Rotations     Non-Flipped      Free Polys
 1 :                1               1               1
 2 :                2               1               1
 3 :                6               2               2
 4 :               19               7               5
 5 :               63              18              12
 6 :              216              60              35
 7 :              760             196             108
 8 :             2725             704             369
 9 :             9910            2500            1285
10 :            36446            9189            4655
11 :           135268           33896           17073
Elasped:  562ms
Estimated completion times:
12 :  0d  0h  0m 02.424s
13 :  0d  0h  0m 09.872s
14 :  0d  0h  0m 39.664s
15 :  0d  0h  2m 38.832s
16 :  0d  0h 10m 35.504s
17 :  0d  0h 42m 22.192s
18 :  0d  2h 49m 28.944s
19 :  0d 11h 17m 55.952s
20 :  1d 21h 11m 43.984s
21 :  7d 12h 46m 56.112s
```



## D

{{trans|Haskell}}

```d
import std.stdio, std.range, std.algorithm, std.typecons, std.conv;

alias Coord = byte;
alias Point = Tuple!(Coord,"x", Coord,"y");
alias Polyomino = Point[];

/// Finds the min x and y coordiate of a Polyomino.
enum minima = (in Polyomino poly) pure @safe =>
    Point(poly.map!q{ a.x }.reduce!min, poly.map!q{ a.y }.reduce!min);

Polyomino translateToOrigin(in Polyomino poly) {
    const minP = poly.minima;
    return poly.map!(p => Point(cast(Coord)(p.x - minP.x), cast(Coord)(p.y - minP.y))).array;
}

enum Point function(in Point p) pure nothrow @safe @nogc
    rotate90  = p => Point( p.y, -p.x),
    rotate180 = p => Point(-p.x, -p.y),
    rotate270 = p => Point(-p.y,  p.x),
    reflect   = p => Point(-p.x,  p.y);

/// All the plane symmetries of a rectangular region.
auto rotationsAndReflections(in Polyomino poly) pure nothrow {
    return only(poly,
                poly.map!rotate90.array,
                poly.map!rotate180.array,
                poly.map!rotate270.array,
                poly.map!reflect.array,
                poly.map!(pt => pt.rotate90.reflect).array,
                poly.map!(pt => pt.rotate180.reflect).array,
                poly.map!(pt => pt.rotate270.reflect).array);
}

enum canonical = (in Polyomino poly) =>
    poly.rotationsAndReflections.map!(pl => pl.translateToOrigin.sort().release).reduce!min;

auto unique(T)(T[] seq) pure nothrow {
    return seq.sort().uniq;
}

/// All four points in Von Neumann neighborhood.
enum contiguous = (in Point pt) pure nothrow @safe @nogc =>
    only(Point(cast(Coord)(pt.x - 1), pt.y), Point(cast(Coord)(pt.x + 1), pt.y),
         Point(pt.x, cast(Coord)(pt.y - 1)), Point(pt.x, cast(Coord)(pt.y + 1)));

/// Finds all distinct points that can be added to a Polyomino.
enum newPoints = (in Polyomino poly) nothrow =>
    poly.map!contiguous.joiner.filter!(pt => !poly.canFind(pt)).array.unique;

enum newPolys = (in Polyomino poly) =>
    poly.newPoints.map!(pt => canonical(poly ~ pt)).array.unique;

/// Generates polyominoes of rank n recursively.
Polyomino[] rank(in uint n) {
    static immutable Polyomino monomino = [Point(0, 0)];
    static Polyomino[] monominoes = [monomino]; // Mutable.
    if (n == 0) return [];
    if (n == 1) return monominoes;
    return rank(n - 1).map!newPolys.join.unique.array;
}

/// Generates a textual representation of a Polyomino.
char[][] textRepresentation(in Polyomino poly) pure @safe {
    immutable minPt = poly.minima;
    immutable maxPt = Point(poly.map!q{ a.x }.reduce!max, poly.map!q{ a.y }.reduce!max);
    auto table = new char[][](maxPt.y - minPt.y + 1, maxPt.x - minPt.x + 1);
    foreach (row; table)
        row[] = ' ';
    foreach (immutable pt; poly)
        table[pt.y - minPt.y][pt.x - minPt.x] = '#';
    return table;
}

void main(in string[] args) {
    iota(1, 11).map!(n => n.rank.length).writeln;

    immutable n = (args.length == 2) ? args[1].to!uint : 5;
    writefln("\nAll free polyominoes of rank %d:", n);

    foreach (const poly; n.rank)
        writefln("%-(%s\n%)\n", poly.textRepresentation);
}
```

{{out}}

```txt
[1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655]

All free polyominoes of rank 5:
#
#
#
#
#

##
#
#
#

#
##
#
#

##
##
#

##
#
##

###
#
#

#
###
#

#
#
##
 #

#
###
 #

#
##
 ##

#
###
  #

 #
###
 #
```



### D: Count Only

Translated and modified from C code: http://www.geocities.jp/tok12345/countomino.txt


```d
import core.stdc.stdio: printf;
import core.stdc.stdlib: atoi;

__gshared ulong[] g_pnCountNH;
__gshared uint[] g_pnFieldCheck, g_pnFieldCheckR;
__gshared uint g_nFieldSize, g_nFieldWidth;
__gshared uint[4] g_anLinkData;
__gshared uint[8] g_anRotationOffset, g_anRotationX, g_anRotationY;

void countMain(in uint n) nothrow {
    g_nFieldWidth = n * 2 - 2;
    g_nFieldSize = (n - 1) * (n - 1) * 2 + 1;
    g_pnCountNH = new ulong[n + 1];

    auto pnField = new uint[g_nFieldSize];
    auto pnPutList = new uint[g_nFieldSize];
    g_pnFieldCheck = new uint[n ^^ 2];
    g_pnFieldCheckR = new uint[n ^^ 2];
    g_anLinkData[0] = 1;
    g_anLinkData[1] = g_nFieldWidth;
    g_anLinkData[2] = -1;
    g_anLinkData[3] = -g_nFieldWidth;

    initOffset(n);

    countSub(n, 0, pnField, pnPutList, 0, 1);
}

void countSub(in uint n, in uint lv, uint[] field, uint[] putlist,
              in uint putno, in uint putlast) nothrow @nogc {
    check(field, n, lv);
    if (n == lv) {
        return;
    }

    foreach (immutable uint i; putno .. putlast) {
        immutable pos = putlist[i];
        field[pos] |= 1;
        uint k = 0;
        foreach (immutable uint j; 0 .. 4) {
            immutable pos2 = pos + g_anLinkData[j];
            if (0 <= pos2 && pos2 < g_nFieldSize && !field[pos2]) {
                field[pos2] = 2;
                putlist[putlast + k] = pos2;
                k++;
            }
        }
        countSub(n, lv + 1, field, putlist, i + 1, putlast + k);
        foreach (immutable uint j; 0 .. k)
            field[putlist[putlast + j]] = 0;
        field[pos] = 2;
    }

    foreach (immutable uint i; putno .. putlast) {
        immutable pos = putlist[i];
        field[pos] &= -2;
    }
}

void initOffset(in uint n) nothrow @nogc {
    g_anRotationOffset[0] = 0;
    g_anRotationX[0] = 1;
    g_anRotationY[0] = n;
    // 90
    g_anRotationOffset[1] = n - 1;
    g_anRotationX[1] = n;
    g_anRotationY[1] = -1;
    // 180
    g_anRotationOffset[2] = n ^^ 2 - 1;
    g_anRotationX[2] = -1;
    g_anRotationY[2] = -n;
    // 270
    g_anRotationOffset[3] = n ^^ 2 - n;
    g_anRotationX[3] = -n;
    g_anRotationY[3] = 1;

    g_anRotationOffset[4] = n - 1;
    g_anRotationX[4] = -1;
    g_anRotationY[4] = n;
    // 90
    g_anRotationOffset[5] = 0;
    g_anRotationX[5] = n;
    g_anRotationY[5] = 1;
    // 180
    g_anRotationOffset[6] = n ^^ 2 - n;
    g_anRotationX[6] = 1;
    g_anRotationY[6] = -n;
    // 270
    g_anRotationOffset[7] = n ^^ 2 - 1;
    g_anRotationX[7] = -n;
    g_anRotationY[7] = -1;
}

void check(in uint[] field, in uint n, in uint lv) nothrow @nogc {
    g_pnFieldCheck[0 .. n ^^ 2] = 0;

    uint x, y;
    outer:
    for (x = n; x < n * 2 - 2; x++)
        for (y = 0; y + x < g_nFieldSize; y += g_nFieldWidth)
            if (field[x + y] & 1)
                break outer;

    immutable uint x2 = n - x;
    foreach (immutable uint i; 0 .. g_nFieldSize) {
        x = (i + n - 2) % g_nFieldWidth;
        y = (i + n - 2) / g_nFieldWidth * n;
        if (field[i] & 1)
            g_pnFieldCheck[x + x2 + y] = 1;
    }

    uint of1;
    for (of1 = 0; of1 < g_pnFieldCheck.length && !g_pnFieldCheck[of1]; of1++) {}

    bool c = true;
    for (uint r = 1; r < 8 && c; r++) {
        for (x = 0; x < n; x++) {
            for (y = 0; y < n; y++) {
                immutable pos = g_anRotationOffset[r] +
                                g_anRotationX[r] * x + g_anRotationY[r] * y;
                g_pnFieldCheckR[pos] = g_pnFieldCheck[x + y * n];
            }
        }

        uint of2;
        for (of2 = 0; of2 < g_pnFieldCheckR.length && !g_pnFieldCheckR[of2]; of2++) {}
        of2 -= of1;
        immutable ed = (of2 > 0) ? (n ^^ 2 - of2) : (n ^^ 2);

        foreach (immutable uint i; of1 .. ed) {
            if (g_pnFieldCheck[i] > g_pnFieldCheckR[i + of2])
                break;
            if (g_pnFieldCheck[i] < g_pnFieldCheckR[i + of2]) {
                c = false;
                break;
            }
        }
    }

    if (c) {
        uint parity;
        if (!(lv & 1)) {
            parity = (lv & 2) >> 1;
            for (x = 0; x < n; x++)
                for (y = 0; y < n; y++)
                    parity ^= (x + y) & g_pnFieldCheck[x + y * n];
            parity &= 1;
        } else
            parity = 0;

        g_pnCountNH[lv]++;
    }
}

int main(in string[] args) {
    immutable n = (args.length == 2) ? (args[1] ~ '\0').ptr.atoi : 11;
    if (n < 1)
        return 1;

    if (n == 1)
        countMain(2);
    else
        countMain(n);

    foreach (immutable i; 1 .. n + 1)
        printf("%llu\n", g_pnCountNH[i]);

    return 0;
}
```

{{out}}

```txt
1
1
2
5
12
35
108
369
1285
4655
17073
```


Output with n=14 (run-time about 36 seconds):

```txt
1
1
2
5
12
35
108
369
1285
4655
17073
63600
238591
901971
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Polyominoes do
  defp translate2origin(poly) do
    # Finds the min x and y coordiate of a Polyomino.
    minx = Enum.map(poly, &elem(&1,0)) |> Enum.min
    miny = Enum.map(poly, &elem(&1,1)) |> Enum.min
    Enum.map(poly, fn {x,y} -> {x - minx, y - miny} end) |> Enum.sort
  end
  
  defp rotate90({x, y}), do: {y, -x}
  defp reflect({x, y}), do: {-x, y}
  
  # All the plane symmetries of a rectangular region.
  defp rotations_and_reflections(poly) do
    poly1 = Enum.map(poly,  &rotate90/1)
    poly2 = Enum.map(poly1, &rotate90/1)
    poly3 = Enum.map(poly2, &rotate90/1)
    poly4 = Enum.map(poly3, &reflect/1)
    poly5 = Enum.map(poly4, &rotate90/1)
    poly6 = Enum.map(poly5, &rotate90/1)
    poly7 = Enum.map(poly6, &rotate90/1)
    [poly, poly1, poly2, poly3, poly4, poly5, poly6, poly7]
  end
  
  defp canonical(poly) do
    rotations_and_reflections(poly) |> Enum.map(&translate2origin/1)
  end
  
  # All four points in Von Neumann neighborhood.
  defp contiguous({x,y}) do
    [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
  end
  
  # Finds all distinct points that can be added to a Polyomino.
  defp new_points(poly) do
    points = Enum.flat_map(poly, &contiguous/1)
    Enum.uniq(points) -- poly
  end
  
  defp new_polys(polys) do
    Enum.reduce(polys, {[], HashSet.new}, fn poly, {polyomino, pattern} ->
      Enum.reduce(new_points(poly), {polyomino, pattern}, fn point, {pol, pat} ->
        pl = translate2origin([point | poly])
        if pl in pat do
          {pol, pat}
        else
          canon = canonical(pl)
          {[Enum.min(canon) | pol], Enum.into(canon, pat)}
        end
      end)
    end)
    |> elem(0)
  end
  
  # Generates polyominoes of rank n recursively.
  def rank(0), do: [[]]
  def rank(1), do: [[{0,0}]]
  def rank(n), do: new_polys(rank(n-1))
  
  # Generates a textual representation of a Polyomino.
  def text_representation(poly) do
    table = Enum.map(poly, &{&1, "#"}) |> Enum.into(Map.new)
    maxx = Enum.map(poly, &elem(&1,0)) |> Enum.max
    maxy = Enum.map(poly, &elem(&1,1)) |> Enum.max
    Enum.map_join(0..maxx, "\n", fn x ->
      Enum.map_join(0..maxy, fn y -> Dict.get(table, {x,y}, " ") end)
    end)
  end
end

IO.inspect Enum.map(0..10, fn n -> length(Polyominoes.rank(n)) end)

n = if System.argv==[], do: 5, else: String.to_integer(hd(System.argv))
IO.puts "\nAll free polyominoes of rank #{n}:"
Enum.sort(Polyominoes.rank(n))
|> Enum.each(fn poly -> IO.puts "#{Polyominoes.text_representation(poly)}\n" end)
```


{{out}}

```txt

[1, 1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655]

All free polyominoes of rank 5:
#####

####
#

####
 #

###
##

###
# #

###
#
#

###
 #
 #

###
  ##

##
 ##
 #

##
 ##
  #

##
 #
 ##

 #
###
 #

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "sort"
)

type point struct{ x, y int }
type polyomino []point
type pointset map[point]bool

func (p point) rotate90() point  { return point{p.y, -p.x} }
func (p point) rotate180() point { return point{-p.x, -p.y} }
func (p point) rotate270() point { return point{-p.y, p.x} }
func (p point) reflect() point   { return point{-p.x, p.y} }

func (p point) String() string { return fmt.Sprintf("(%d, %d)", p.x, p.y) }

// All four points in Von Neumann neighborhood
func (p point) contiguous() polyomino {
    return polyomino{point{p.x - 1, p.y}, point{p.x + 1, p.y},
        point{p.x, p.y - 1}, point{p.x, p.y + 1}}
}

// Finds the min x and y coordinates of a Polyomino.
func (po polyomino) minima() (int, int) {
    minx := po[0].x
    miny := po[0].y
    for i := 1; i < len(po); i++ {
        if po[i].x < minx {
            minx = po[i].x
        }
        if po[i].y < miny {
            miny = po[i].y
        }
    }
    return minx, miny
}

func (po polyomino) translateToOrigin() polyomino {
    minx, miny := po.minima()
    res := make(polyomino, len(po))
    for i, p := range po {
        res[i] = point{p.x - minx, p.y - miny}
    }
    sort.Slice(res, func(i, j int) bool {
        return res[i].x < res[j].x || (res[i].x == res[j].x && res[i].y < res[j].y)
    })
    return res
}

// All the plane symmetries of a rectangular region.
func (po polyomino) rotationsAndReflections() []polyomino {
    rr := make([]polyomino, 8)
    for i := 0; i < 8; i++ {
        rr[i] = make(polyomino, len(po))
    }
    copy(rr[0], po)
    for j := 0; j < len(po); j++ {
        rr[1][j] = po[j].rotate90()
        rr[2][j] = po[j].rotate180()
        rr[3][j] = po[j].rotate270()
        rr[4][j] = po[j].reflect()
        rr[5][j] = po[j].rotate90().reflect()
        rr[6][j] = po[j].rotate180().reflect()
        rr[7][j] = po[j].rotate270().reflect()
    }
    return rr
}

func (po polyomino) canonical() polyomino {
    rr := po.rotationsAndReflections()
    minr := rr[0].translateToOrigin()
    mins := minr.String()
    for i := 1; i < 8; i++ {
        r := rr[i].translateToOrigin()
        s := r.String()
        if s < mins {
            minr = r
            mins = s
        }
    }
    return minr
}

func (po polyomino) String() string {
    return fmt.Sprintf("%v", []point(po))
}

func (po polyomino) toPointset() pointset {
    pset := make(pointset, len(po))
    for _, p := range po {
        pset[p] = true
    }
    return pset
}

// Finds all distinct points that can be added to a Polyomino.
func (po polyomino) newPoints() polyomino {
    pset := po.toPointset()
    m := make(pointset) 
    for _, p := range po {
        pts := p.contiguous()
        for _, pt := range pts {
            if !pset[pt] {
                m[pt] = true // using an intermediate set is about 15% faster!
            }
        }
    }
    poly := make(polyomino, 0, len(m))
    for k := range m {
        poly = append(poly, k)
    }
    return poly
}

func (po polyomino) newPolys() []polyomino {
    pts := po.newPoints()
    res := make([]polyomino, len(pts))
    for i, pt := range pts {
        poly := make(polyomino, len(po))
        copy(poly, po)
        poly = append(poly, pt)
        res[i] = poly.canonical()
    }
    return res
}

var monomino = polyomino{point{0, 0}}
var monominoes = []polyomino{monomino}

// Generates polyominoes of rank n recursively.
func rank(n int) []polyomino {
    switch {
    case n < 0:
        panic("n cannot be negative. Program terminated.")
    case n == 0:
        return []polyomino{}
    case n == 1:
        return monominoes
    default:
        r := rank(n - 1)
        m := make(map[string]bool)
        var polys []polyomino
        for _, po := range r {
            for _, po2 := range po.newPolys() {
                if s := po2.String(); !m[s] {
                    polys = append(polys, po2)
                    m[s] = true
                }
            }
        }
        sort.Slice(polys, func(i, j int) bool {
            return polys[i].String() < polys[j].String()
        })
        return polys
    }
}

func main() {
    const n = 5
    fmt.Printf("All free polyominoes of rank %d:\n\n", n)
    for _, poly := range rank(n) {
        for _, pt := range poly {
            fmt.Printf("%s ", pt)
        }
        fmt.Println()
    }
    const k = 10
    fmt.Printf("\nNumber of free polyominoes of ranks 1 to %d:\n", k)
    for i := 1; i <= k; i++ {
        fmt.Printf("%d ", len(rank(i)))
    }
    fmt.Println()
}
```


{{out}}

```txt

All free polyominoes of rank 5:

(0, 0) (0, 1) (0, 2) (0, 3) (0, 4) 
(0, 0) (0, 1) (0, 2) (0, 3) (1, 0) 
(0, 0) (0, 1) (0, 2) (0, 3) (1, 1) 
(0, 0) (0, 1) (0, 2) (1, 0) (1, 1) 
(0, 0) (0, 1) (0, 2) (1, 0) (1, 2) 
(0, 0) (0, 1) (0, 2) (1, 0) (2, 0) 
(0, 0) (0, 1) (0, 2) (1, 1) (2, 1) 
(0, 0) (0, 1) (0, 2) (1, 2) (1, 3) 
(0, 0) (0, 1) (1, 1) (1, 2) (2, 1) 
(0, 0) (0, 1) (1, 1) (1, 2) (2, 2) 
(0, 0) (0, 1) (1, 1) (2, 1) (2, 2) 
(0, 1) (1, 0) (1, 1) (1, 2) (2, 1) 

Number of free polyominoes of ranks 1 to 10:
1 1 2 5 12 35 108 369 1285 4655 

```



## Haskell

This Haskell solution is relatively slow, it's meant to be readable and as manifestly correct as possible.

Code updated and slightly improved from: http://www.haskell.org/haskellwiki/The_Monad.Reader/Issue5/Generating_Polyominoes

```haskell
import System.Environment (getArgs)
import Control.Arrow ((***), first)
import Data.Set (toList, fromList)
import Data.List (sort)
import Data.Bool (bool)

type Coord = Int

type Point = (Coord, Coord)

type Polyomino = [Point]

-- Finds the min x and y coordiate of a Polyomino.
minima :: Polyomino -> Point
minima (p:ps) = foldr (\(x, y) (mx, my) -> (min x mx, min y my)) p ps

translateToOrigin :: Polyomino -> Polyomino
translateToOrigin p =
  let (minx, miny) = minima p
  in (subtract minx *** subtract miny) <$> p

rotate90, rotate180, rotate270, reflect :: Point -> Point
rotate90 = uncurry (flip (,) . negate)

rotate180 = negate *** negate

rotate270 = uncurry (flip ((,) . negate))

reflect = first negate

-- All the plane symmetries of a rectangular region.
rotationsAndReflections :: Polyomino -> [Polyomino]
rotationsAndReflections =
  (<*>)
    (fmap <$>
     [ id
     , rotate90
     , rotate180
     , rotate270
     , reflect
     , rotate90 . reflect
     , rotate180 . reflect
     , rotate270 . reflect
     ]) .
  return

canonical :: Polyomino -> Polyomino
canonical = minimum . map (sort . translateToOrigin) . rotationsAndReflections

unique
  :: (Ord a)
  => [a] -> [a]
unique = toList . fromList

-- All four points in Von Neumann neighborhood.
contiguous :: Point -> [Point]
contiguous (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- Finds all distinct points that can be added to a Polyomino.
newPoints :: Polyomino -> [Point]
newPoints p =
  let notInP = filter (not . flip elem p)
  in unique . notInP . concatMap contiguous $ p

newPolys :: Polyomino -> [Polyomino]
newPolys p = unique . map (canonical . flip (:) p) $ newPoints p

monomino = [(0, 0)]

monominoes = [monomino]

-- Generates polyominoes of rank n recursively.
rank :: Int -> [Polyomino]
rank 0 = []
rank 1 = monominoes
rank n = unique . concatMap newPolys $ rank (n - 1)

-- Generates a textual representation of a Polyomino.
textRepresentation :: Polyomino -> String
textRepresentation p =
  unlines
    [ [ bool ' ' '#' ((x, y) `elem` p)
      | x <- [0 .. maxx - minx] ]
    | y <- [0 .. maxy - miny] ]
  where
    maxima :: Polyomino -> Point
    maxima (p:ps) = foldr (\(x, y) (mx, my) -> (max x mx, max y my)) p ps
    (minx, miny) = minima p
    (maxx, maxy) = maxima p

main :: IO ()
main = do
  print $ map (length . rank) [1 .. 10]
  args <- getArgs
  let n = bool (read $ head args :: Int) 5 (null args)
  putStrLn ("\nAll free polyominoes of rank " ++ show n ++ ":")
  mapM_ (putStrLn . textRepresentation) (rank n)
```

{{out}}

```txt
[1,1,2,5,12,35,108,369,1285,4655]

All free polyominoes of rank 5:
#
#
#
#
#

##
# 
# 
# 

# 
##
# 
# 

##
##
# 

##
# 
##

###
#  
#  

#  
###
#  

# 
# 
##
 #

#  
###
 # 

#  
## 
 ##

#  
###
  #

 # 
###
 # 
```



## J


Generating polyominoes as ascii art:


```J
polyominoes=:verb define
  if. 1>y do. i.0 0 0 return.end.
  if. 1=y do. 1 1 1$'#' return.end.
  }.~.' ',simplify ,/extend"2 polyominoes y-1
)

extend=:verb define
  reps=. ' ',"1~~.all y
  simplify ,/extend1"2 reps
)

extend1=:verb define
  b=. (i.#y),._1|."1 '# ' E."1 y
  simplify ,/b extend2"1 _ y
)

extend2=:verb define
:
  row=.{.x
  mask=.}.x
  row mask extend3 y&>1+i.+/mask
)

extend3=:conjunction define
:
  '#' (<x,I.m*y=+/\m)} n
)

simplify=:verb define
  t=. ~.trim"2 y
  t #~ +./"1 ((2{.$) $ (i.@# = i.~)@(,/)) all@trim"2 t
)

flip=: |."_1
all=: , flip@|:, |.@flip, |.@|:, |., |.@flip@|:, flip,: |:

trim=:verb define&|:^:2
  y#~+./"1 y~:' '
)
```


Example use (boxing each pentomino for display purposes):


```j
   <"2 polyominoes 5
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│#####│##   │#    │###  │##   │##   │###  │ ##  │ #   │ #   │ #   │ ##  │
│     │#    │##   │#    │##   │#    │  ## │ #   │ ##  │ #   │###  │##   │
│     │#    │#    │#    │#    │##   │     │##   │##   │###  │ #   │#    │
│     │#    │#    │     │     │     │     │     │     │     │     │     │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
```



## Java

Translation of [[Free_polyominoes_enumeration#Haskell|Haskell]] via [[Free_polyominoes_enumeration#D|D]]
{{works with|Java|8}}

```java
import java.awt.Point;
import java.util.*;
import static java.util.Arrays.asList;
import java.util.function.Function;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;

public class FreePolyominoesEnum {
    static final List<Function<Point, Point>> transforms = new ArrayList<>();

    static {
        transforms.add(p -> new Point(p.y, -p.x));
        transforms.add(p -> new Point(-p.x, -p.y));
        transforms.add(p -> new Point(-p.y, p.x));
        transforms.add(p -> new Point(-p.x, p.y));
        transforms.add(p -> new Point(-p.y, -p.x));
        transforms.add(p -> new Point(p.x, -p.y));
        transforms.add(p -> new Point(p.y, p.x));
    }

    static Point findMinima(List<Point> poly) {
        return new Point(
                poly.stream().mapToInt(a -> a.x).min().getAsInt(),
                poly.stream().mapToInt(a -> a.y).min().getAsInt());
    }

    static List<Point> translateToOrigin(List<Point> poly) {
        final Point min = findMinima(poly);
        poly.replaceAll(p -> new Point(p.x - min.x, p.y - min.y));
        return poly;
    }

    static List<List<Point>> rotationsAndReflections(List<Point> poly) {
        List<List<Point>> lst = new ArrayList<>();
        lst.add(poly);
        for (Function<Point, Point> t : transforms)
            lst.add(poly.stream().map(t).collect(toList()));
        return lst;
    }

    static Comparator<Point> byCoords = Comparator.<Point>comparingInt(p -> p.x)
            .thenComparingInt(p -> p.y);

    static List<Point> normalize(List<Point> poly) {
        return rotationsAndReflections(poly).stream()
                .map(lst -> translateToOrigin(lst))
                .map(lst -> lst.stream().sorted(byCoords).collect(toList()))
                .min(comparing(Object::toString)) // not efficient but simple
                .get();
    }

    static List<Point> neighborhoods(Point p) {
        return asList(new Point(p.x - 1, p.y), new Point(p.x + 1, p.y),
                new Point(p.x, p.y - 1), new Point(p.x, p.y + 1));
    }

    static List<Point> concat(List<Point> lst, Point pt) {
        List<Point> r = new ArrayList<>();
        r.addAll(lst);
        r.add(pt);
        return r;
    }

    static List<Point> newPoints(List<Point> poly) {
        return poly.stream()
                .flatMap(p -> neighborhoods(p).stream())
                .filter(p -> !poly.contains(p))
                .distinct()
                .collect(toList());
    }

    static List<List<Point>> constructNextRank(List<Point> poly) {
        return newPoints(poly).stream()
                .map(p -> normalize(concat(poly, p)))
                .distinct()
                .collect(toList());
    }

    static List<List<Point>> rank(int n) {
        if (n < 0)
            throw new IllegalArgumentException("n cannot be negative");

        if (n < 2) {
            List<List<Point>> r = new ArrayList<>();
            if (n == 1)
                r.add(asList(new Point(0, 0)));
            return r;
        }

        return rank(n - 1).stream()
                .parallel()
                .flatMap(lst -> constructNextRank(lst).stream())
                .distinct()
                .collect(toList());
    }

    public static void main(String[] args) {
        for (List<Point> poly : rank(5)) {
            for (Point p : poly)
                System.out.printf("(%d,%d) ", p.x, p.y);
            System.out.println();
        }
    }
}
```



```txt
(0,0) (0,1) (1,1) (1,2) (2,1) 
(0,0) (0,1) (0,2) (1,0) (1,1) 
(0,0) (0,1) (0,2) (0,3) (1,1) 
(0,1) (1,0) (1,1) (1,2) (2,1) 
(0,0) (0,1) (0,2) (1,1) (2,1) 
(0,0) (0,1) (1,1) (1,2) (2,2) 
(0,0) (0,1) (0,2) (1,2) (1,3) 
(0,0) (0,1) (1,1) (2,1) (2,2) 
(0,0) (0,1) (0,2) (1,0) (1,2) 
(0,0) (0,1) (0,2) (0,3) (1,0) 
(0,0) (0,1) (0,2) (1,0) (2,0) 
(0,0) (0,1) (0,2) (0,3) (0,4)
```




## Julia

{{trans|Haskell}}

```julia
import Base.show, Base.==, Base.hash

struct Point x::Float64; y::Float64 end
hash(p::Point) = hash(p.x, hash(p.y))
==(p1::Point, p2::Point) = p1.x == p2.x && p1.y == p2.y 

pointsort!(pv) = sort!(pv, lt = (a, b) -> a.x == b.x ? a.y < b.y : a.x < b.x)

mutable struct Poly
    vp::Vector{Point}
    Poly(v::Vector{Point}) = new(pointsort!(unique(v)))
end
Poly(poly::Poly) = Poly(poly.vp)
Poly(poly::Poly, v::Vector{Point}) = Poly(vcat(poly.vp, v))
Poly(poly, f::Function) = Poly(pointsort!(map(p -> f(p), deepcopy(poly.vp))))
==(p1::Poly, p2::Poly) = length(p1.vp) == length(p2.vp) && 
    all(i -> p1.vp[i] == p2.vp[i], 1:length(p1.vp))
hash(p1::Poly) = reduce((x, y) -> hash(hash(x), hash(y)), p1.vp)

polysort!(polyarr) = sort!(polyarr, lt = (a, b) -> string(a.vp) < string(b.vp))

translate_to_origin(poly) = Poly(poly, p -> Point(p.x - minimum(p -> p.x, poly.vp),
    p.y - minimum(p -> p.y, poly.vp)))

function asciimatrix(poly)
    if length(poly.vp) == 0
        return reshape(Char[], 0, 0)
    elseif length(poly.vp) == 1
        return reshape([' '], 1, 1)
    end
    vp = translate_to_origin(poly).vp
    sz = Int.((maximum(p -> p.x, vp), maximum(p -> p.y, vp))) .+ 1
    txtmat = fill(' ', sz)
    for i in 1:sz[1], j in 1:sz[2]
        if Point(i-1, j-1) in vp
            txtmat[i, j] = '#'
        end
    end
    txtmat
end

rotate90(poly) = Poly(poly, p -> Point(p.y, -p.x))
rotate180(poly) = Poly(poly, p -> Point(-p.x, -p.y))
rotate270(poly) = Poly(poly, p -> Point(-p.y, p.x))
reflect(poly) = Poly(poly, p -> Point(-p.x, p.y))

rotations_and_reflections(poly) = [poly, rotate90(poly), rotate180(poly),
    rotate270(poly), reflect(poly), reflect(rotate90(poly)),
    reflect(rotate180(poly)), reflect(rotate270(poly))]

canonical(poly) = polysort!(map(translate_to_origin, rotations_and_reflections(poly)))

contiguous(p) = [Point(p.x - 1, p.y), Point(p.x + 1, p.y),
    Point(p.x, p.y - 1), Point(p.x, p.y + 1)]

adjacentpoints(poly) = unique(filter(p -> !(p in poly.vp),
    reduce(vcat, [contiguous(p) for p in poly.vp])))

nextrank_adjacentpolys(poly) = map(pv -> pv[1], unique(canonical.(
    [Poly(poly, [p]) for p in adjacentpoints(poly)])))

const nullmino = Poly[]
const monomino = Poly([Point(0, 0)])

rank(n) = @assert n >= 0 && return n == 0 ? nullmino : n == 1 ? [monomino] :
    unique(reduce(vcat, map(nextrank_adjacentpolys, rank(n - 1))))

function Base.show(io::IO, poly::Poly)
    txtmat = asciimatrix(poly)
    w, h = size(txtmat)
    for i in 1:w
        for j in 1:h
            print(txtmat[i, j])
        end
        println()
    end
end

function testpolys(N = 5)
    println([length(rank(n)) for n in 1:10])

    println("\nAll free polyominoes of rank $N:")

    for poly in rank(5)
        println(poly)
    end
end

testpolys()

```
{{out}}

```txt

[1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655]

All free polyominoes of rank 5:
##
 ##
 #

###
##

####
 #

 #
###
 #

###
 #
 #

##
 ##
  #

###
  ##

##
 #
 ##

###
# #

####
#

###
#
#

#####

```



## Kotlin

{{trans|Python}}

```scala
// version 1.1.51

class Point(val x: Int, val y: Int) : Comparable<Point> {
    fun rotate90()  = Point( this.y, -this.x)
    fun rotate180() = Point(-this.x, -this.y)
    fun rotate270() = Point(-this.y,  this.x)
    fun reflect()   = Point(-this.x,  this.y)

    override fun equals(other: Any?): Boolean {
        if (other == null || other !is Point) return false
        return this.x == other.x && this.y == other.y
    }

    override fun compareTo(other: Point) =
        if (this == other ) 0
        else if (this.x < other.x || (this.x == other.x && this.y < other.y)) -1
        else 1

    override fun toString() = "($x, $y)"
}

typealias Polyomino = List<Point>

// Finds the min x and y coordinates of a Polyomino.
val Polyomino.minima get() = Pair(this.minBy { it.x }!!.x, this.minBy { it.y }!!.y)

fun Polyomino.translateToOrigin(): Polyomino {
    val (minX, minY) = this.minima
    return this.map { Point(it.x - minX, it.y - minY) }.sorted()
}

// All the plane symmetries of a rectangular region.
val Polyomino.rotationsAndReflections get() =
    listOf(
        this,
        this.map { it.rotate90() },
        this.map { it.rotate180() },
        this.map { it.rotate270() },
        this.map { it.reflect() },
        this.map { it.rotate90().reflect() },
        this.map { it.rotate180().reflect() },
        this.map { it.rotate270().reflect() }
    )

val Polyomino.canonical get() =
    this.rotationsAndReflections.map { it.translateToOrigin() }.minBy { it.toString() }!!

// All four points in Von Neumann neighborhood
val Point.contiguous get() =
    listOf(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1))

// Finds all distinct points that can be added to a Polyomino.
val Polyomino.newPoints get() = this.flatMap { it.contiguous }.filter { it !in this }.distinct()

val Polyomino.newPolys get() = this.newPoints.map { (this + it).canonical }

val monomino = listOf(Point(0, 0))
val monominoes = listOf(monomino)

// Generates polyominoes of rank n recursively.
fun rank(n: Int): List<Polyomino> = when {
    n < 0  -> throw IllegalArgumentException("n cannot be negative")
    n == 0 -> emptyList<Polyomino>()
    n == 1 -> monominoes
    else   -> rank(n - 1).flatMap { it.newPolys }
                         .distinctBy { it.toString() }
                         .sortedBy { it.toString() }
}

fun main(args: Array<String>) {
    val n = 5
    println("All free polyominoes of rank $n:\n")
    for (poly in rank(n)) {
        for (pt in poly) print("$pt ")
        println()
    }
    val k = 10
    println("\nNumber of free polyominoes of ranks 1 to $k:")
    for (i in 1..k) print("${rank(i).size} ")
    println()
}
```


{{out}}

```txt

All free polyominoes of rank 5:

(0, 0) (0, 1) (0, 2) (0, 3) (0, 4) 
(0, 0) (0, 1) (0, 2) (0, 3) (1, 0) 
(0, 0) (0, 1) (0, 2) (0, 3) (1, 1) 
(0, 0) (0, 1) (0, 2) (1, 0) (1, 1) 
(0, 0) (0, 1) (0, 2) (1, 0) (1, 2) 
(0, 0) (0, 1) (0, 2) (1, 0) (2, 0) 
(0, 0) (0, 1) (0, 2) (1, 1) (2, 1) 
(0, 0) (0, 1) (0, 2) (1, 2) (1, 3) 
(0, 0) (0, 1) (1, 1) (1, 2) (2, 1) 
(0, 0) (0, 1) (1, 1) (1, 2) (2, 2) 
(0, 0) (0, 1) (1, 1) (2, 1) (2, 2) 
(0, 1) (1, 0) (1, 1) (1, 2) (2, 1) 

Number of free polyominoes of ranks 1 to 10:
1 1 2 5 12 35 108 369 1285 4655 

```



## Phix

{{trans|C#}} ... but didn't bother with Wrap()

```Phix
-- demo\rosetta\Polyominoes.exw
integer n, ns,                  -- rank, rank squared
        target,                 -- rank to display
        clipAt,                 -- max columns for display
        fSiz, fWid              -- field size, width
sequence polys,                 -- results
         AnyR,                  -- Any Rotation count
         nFlip,                 -- Non-Flipped count
         Frees,                 -- Free Polyominoes count
         fChk, fCkR,            -- field checks
         dirs,                  -- directions
         rotO, rotX, rotY       -- rotations

-- (character indexes only work properly in utf32:)
constant glyphs = utf8_to_utf32(" 12└4┘─┴8│┌├┐┤┬┼")

function Puzzle(string a, string b) -- tests each intersection to determine correct corner symbol
    sequence res = ""
    if length(a)>length(b) then b &= repeat(' ', length(a)-length(b)) end if
    if length(a)<length(b) then a &= repeat(' ', length(b)-length(a)) end if
    for i=1 to length(a)-1 do
        integer n=i+1
        res &= glyphs[iff(a[i]==a[n]?0:1) + 
                      iff(b[n]==a[n]?0:2) +
                      iff(a[i]==b[i]?0:4) + 
                      iff(b[i]==b[n]?0:8) + 1];
    end for
    return utf32_to_utf8(res)
end function

function flipXY(sequence p) -- flips a small string
    sequence res = repeat("",length(p[1]))
    for i=1 to length(res) do
        for j=1 to length(p) do res[i] &= p[j][i] end for
    end for
    return res
end function

function double_width(string s)
    string t = ""
    for i=1 to length(s) do
        integer ch = s[i]
        t &= ch&ch
    end for
    return t
end function
 
function Cornered(string s) -- converts plain ascii art into cornered version
    sequence lines = split(s,'\n')
    string res = ""
    string line = repeat(' ', length(lines[1])*2), last
    for i=1 to length(lines) do
        last = line
        line = double_width(lines[i])
        res &= Puzzle(last, line) & '\n'
    end for
    res &= Puzzle(line, repeat(' ', length(lines[$])*2)) & '\n'
    return res
end function

function Assemble(sequence p)
-- assembles string representation of polyominoes into larger horizontal band
    sequence lines = repeat("",target)
    for i=1 to length(p) do
        sequence t = split(p[i],'\n')
        if length(t)<length(t[1]) then t = flipXY(t) end if
        for l=1 to length(lines) do
            lines[l] &= iff(l<=length(t)?' '&t[l]&' ':repeat(' ',length(t[1])+2))
        end for
    end for
    for i=length(lines) to 1 by -1 do
        if find('#',lines[i])=0 then lines[i..i] = {} end if
    end for
    return Cornered(join(lines,"\n"))&"\n"
end function
 
function toStr(sequence field)
-- converts field into a minimal string
    string res = repeat(' ',n*(fWid+1)-1)
    for i=fWid+1 to length(res) by fWid+1 do res[i] = '\n' end for
    integer i = 0, j = n-2
    while i<length(field) do
        if and_bits(field[i+1],1)=1 then res[j+1] = '#' end if
        if mod(j,fWid+1)==fWid then i -= 1 end if
        i += 1
        j += 1
    end while
    sequence t = split(res,'\n')
    integer nn = 100, m = 0, v, k = 0; -- trim down
    for i = 1 to length(t) do
        string s = t[i]
        v = find('#',s)
        if v=0 then exit end if
        if v<nn then nn=v end if
        v = rfind('#',s)
        if v>m then m=v end if
        k += 1
    end for
    t = t[1..k]
    for i=1 to length(t) do
        t[i] = t[i][nn..m]
    end for
    if platform()=WINDOWS then return t end if
    res = join(t,'\n')
    return res
end function

procedure CheckIt(sequence field, integer lv)
    AnyR[lv] += 1
    for i=1 to ns do fChk[i] = 0 end for
    integer x, y
    bool bail = false
    for x=n to fWid-1 do
        for y=0 to fSiz-x by fWid do
            bail = and_bits(field[x+y+1],1)=1
            if bail then exit end if
        end for
        if bail then exit end if
    end for
    integer x2 = n - x, t, of1, of2, r
    for i=1 to fSiz do
        if and_bits(field[i],1)==1 then
            t = (i + n - 3)
            fChk[mod(t,fWid)+x2+floor(t/fWid)*n+1] = 1
        end if
    end for
    for of1=1 to length(fChk) do if fChk[of1]!=0 then exit end if end for
    bool c = true
    for r=2 to 8 do
        for x=0 to n-1 do
            for y=0 to n-1 do
                fCkR[rotO[r]+rotX[r]*x+rotY[r]*y+1] = fChk[x+y*n+1]
            end for
        end for
        for of2=1 to length(fCkR) do if fCkR[of2]!=0 then exit end if end for
        of2 -= of1
        integer i = of1
        while true do
            if i>=ns-iff(of2>0?of2:0) then exit end if
            if fChk[i+1]>fCkR[i+of2+1] then exit end if
            if fChk[i+1]<fCkR[i+of2+1] then c = false; exit end if
            i += 1
        end while
        if not c then exit end if
    end for
    if r>4 then nFlip[lv] +=1 end if
    if c then
        if lv==target+1 then polys=append(polys,toStr(field)) end if
        Frees[lv] += 1
    end if
end procedure

function Recurse(integer lv, sequence field, putlist, integer putno, putlast)
-- this is probably about ten times slower than C#... 
--  (some you win, some you lose - it has certainly not helped converting
--   0-based indexing to 1-based simply by adding +1 almost everywhere.)
    CheckIt(field, lv)
    if n<lv then return {field,putlist} end if
    integer pos
    for i=putno to putlast do
        pos = putlist[i]
        field[pos+1] = or_bits(field[pos+1],1)
        integer k = 0
        for d=1 to length(dirs) do
            integer pos2 = pos + dirs[d]
            if 0<=pos2 and pos2<fSiz and field[pos2+1]==0 then
                field[pos2+1] = 2
                k += 1
                putlist[putlast+k] = pos2
            end if
        end for
        {field,putlist} = Recurse(lv+1, field, putlist, i+1, putlast+k)
        for j=1 to k do field[putlist[putlast+j]+1] = 0 end for
        field[pos+1] = 2
    end for
    for i=putno to putlast do field[putlist[i]+1] = and_bits(field[putlist[i]+1],-2) end for
    return {field,putlist}
end function

procedure CountEm()
    ns = n * n
    AnyR = repeat(0,n+1)
    nFlip = repeat(0,n+1)
    Frees = repeat(0,n+1)
    fWid = n*2 - 2
    fSiz = (n-1)*(n-1)*2 + 1
    sequence pnField = repeat(0,fSiz),
             pnPutList = repeat(0,fSiz)
    fChk = repeat(0,ns)
    fCkR = repeat(0,ns)
    dirs = {1, fWid, -1, -fWid}
    rotO = {0, n-1, ns-1, ns-n, n-1, 0, ns-n, ns-1}
    rotX = {1, n, -1, -n, -1, n, 1, -n}
    rotY = {n, -1, -n, 1, n, 1, -n, -1}
    {} = Recurse(1, pnField, pnPutList, 1, 1)
end procedure

procedure main()
    polys = {}
    n = 11
    target = 5
    printf(1,"Counting polyominoes to rank %d...\n", n)
    clipAt = 120
    atom start = time()
    CountEm()
    atom ti = time()-start
    if length(polys)>0 then
        printf(1,"Displaying rank %d:\n", target);
        if platform()=LINUX then
            puts(1,Assemble(polys))
        else
            -- Windows consoles not so clever with unicode...
            integer w = 0
            sequence lines = {}
            for i=1 to length(polys) do
                for j=1 to length(polys[i]) do
                    if j>length(lines) then
                        lines = append(lines,repeat(' ',w))
                    end if
                    lines[j] &= repeat(' ',w-length(lines[j]))
                    if i>1 then lines[j] &= "  " end if
                    lines[j] &= polys[i][j]
                end for
                w = length(lines[1])
            end for
            puts(1,join(lines,"\n")&"\n")
        end if
    end if
    printf(1,"Displaying results:\n")
    printf(1," n      All Rotations     Non-Flipped      Free Polys\n")
    for i=2 to n+1 do
        printf(1,"%2d : %16d %15d %15d\n", {i-1, AnyR[i], nFlip[i], Frees[i]})
    end for
    printf(1,"Elapsed: %s\n",{elapsed(ti)})
    atom ms = ti*1000
    if ms>250 then
        printf(1,"Estimated completion times:\n")
        for i=n+1 to n+10 do
            ms = (ms+44)*4
            printf(1,"%2d : %s\n",{i,elapsed(ms/1000)})
        end for
    end if
    {} = wait_key()
end procedure
main()
```

{{out}}
(windows)

```txt

Counting polyominoes to rank 11...
Displaying rank 5:
###  ###   ###  ####  ###   ##   ##   ##  ####  ###  #####   #
##   #    ##    #     # #  ##    #   ##    #     #          ###
     #                      #   ##   #           #           #
Displaying results:
 n      All Rotations     Non-Flipped      Free Polys
 1 :                1               1               1
 2 :                2               1               1
 3 :                6               2               2
 4 :               19               7               5
 5 :               63              18              12
 6 :              216              60              35
 7 :              760             196             108
 8 :             2725             704             369
 9 :             9910            2500            1285
10 :            36446            9189            4655
11 :           135268           33896           17073
Elapsed: 5.8s
Estimated completion times:
12 : 23.5s
13 : 1 minute and 34s
14 : 6 minutes and 17s
15 : 25 minutes and 07s
16 : 1 hour, 40 minutes and 28s
17 : 6 hours, 41 minutes and 52s
18 : 1 day, 2 hours, 47 minutes and 27s
19 : 4 days, 11 hours, 9 minutes and 49s
20 : 17 days, 20 hours, 39 minutes and 14s
21 : 71 days, 10 hours, 36 minutes and 57s

```

{{out}}
(linux)

```txt

Displaying rank 5:
 ┌───┐   ┌─────┐     ┌─┐   ┌───┐   ┌───┐     ┌───┐     ┌───┐     ┌───┐   ┌─┐     ┌─────┐   ┌─┐     ┌─┐   
 │   │   │ ┌───┘   ┌─┘ │   │ ┌─┘   │ ┌─┘   ┌─┘ ┌─┘     │ ┌─┘   ┌─┘ ┌─┘   │ └─┐   └─┐ ┌─┘   │ │   ┌─┘ └─┐ 
 │ ┌─┘   │ │       │ ┌─┘   │ │     │ └─┐   └─┐ │     ┌─┘ │     │ ┌─┘     │ ┌─┘     │ │     │ │   └─┐ ┌─┘ 
 └─┘     └─┘       │ │     │ │     └───┘     └─┘     └───┘     └─┘       │ │       └─┘     │ │     └─┘   
                   └─┘     └─┘                                           └─┘               │ │           
                                                                                           └─┘

```



## Python

{{trans|Haskell}}

```python
from itertools import imap, imap, groupby, chain, imap
from operator import itemgetter
from sys import argv
from array import array

def concat_map(func, it):
    return list(chain.from_iterable(imap(func, it)))

def minima(poly):
    """Finds the min x and y coordiate of a Polyomino."""
    return (min(pt[0] for pt in poly), min(pt[1] for pt in poly))

def translate_to_origin(poly):
    (minx, miny) = minima(poly)
    return [(x - minx, y - miny) for (x, y) in poly]

rotate90   = lambda (x, y): ( y, -x)
rotate180  = lambda (x, y): (-x, -y)
rotate270  = lambda (x, y): (-y,  x)
reflect    = lambda (x, y): (-x,  y)

def rotations_and_reflections(poly):
    """All the plane symmetries of a rectangular region."""
    return (poly,
            map(rotate90, poly),
            map(rotate180, poly),
            map(rotate270, poly),
            map(reflect, poly),
            [reflect(rotate90(pt)) for pt in poly],
            [reflect(rotate180(pt)) for pt in poly],
            [reflect(rotate270(pt)) for pt in poly])

def canonical(poly):
    return min(sorted(translate_to_origin(pl)) for pl in rotations_and_reflections(poly))

def unique(lst):
    lst.sort()
    return map(next, imap(itemgetter(1), groupby(lst)))

# All four points in Von Neumann neighborhood.
contiguous = lambda (x, y): [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

def new_points(poly):
    """Finds all distinct points that can be added to a Polyomino."""
    return unique([pt for pt in concat_map(contiguous, poly) if pt not in poly])

def new_polys(poly):
    return unique([canonical(poly + [pt]) for pt in new_points(poly)])

monomino = [(0, 0)]
monominoes = [monomino]

def rank(n):
    """Generates polyominoes of rank n recursively."""
    assert n >= 0
    if n == 0: return []
    if n == 1: return monominoes
    return unique(concat_map(new_polys, rank(n - 1)))

def text_representation(poly):
    """Generates a textual representation of a Polyomino."""
    min_pt = minima(poly)
    max_pt = (max(p[0] for p in poly), max(p[1] for p in poly))
    table = [array('c', ' ') * (max_pt[1] - min_pt[1] + 1)
             for _ in xrange(max_pt[0] - min_pt[0] + 1)]
    for pt in poly:
        table[pt[0] - min_pt[0]][pt[1] - min_pt[1]] = '#'
    return "\n".join(row.tostring() for row in table)

def main():
    print [len(rank(n)) for n in xrange(1, 11)]

    n = int(argv[1]) if (len(argv) == 2) else 5
    print "\nAll free polyominoes of rank %d:" % n

    for poly in rank(n):
        print text_representation(poly), "\n"

main()
```

{{out}}

```txt
[1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655]

All free polyominoes of rank 5:
##### 

####
#    

####
 #   

###
##  

###
# # 

###
#  
#   

###
 # 
 #  

### 
  ## 

## 
 ##
 #  

## 
 ##
  # 

## 
 # 
 ## 

 # 
###
 #  
```



## Racket


Uses Racket's arbitrary length integers as bit fields. It's not as
compact as it possible could be (all numbers are "square" in shape),
but it is correct.

Implemented in typed/racket. Don't balk at all the type annotations.
In the right environment (DrRacket), they allow the developer to keep
types in check.

Some functionality might be vestigial, or used in testing (test scripts
not included in code below). But I think it's interesting nonetheless.


```racket
#lang typed/racket
;; Inspired by C code in http://www.geocities.jp/tok12345/countomino.txt
;; but tries to take advantage of arbitrary width integers
(define-type Order Positive-Integer)
(define-type Shape Nonnegative-Integer)
;; "shape" functions are abbreviated s-...
(define-type Shapes (Listof Shape))
(define-type Shapes+ (Pairof Shape Shapes))
;; polynomino
;;  order: number of bits wide a row of the "shape" is
;;  shape: bit map (integer). bits set where the "animal" is
(struct polynominoes ([order : Order] [shapes : Shapes]))
(define-type shape-xform (Order Shape -> Shape))
(: s-reflect:y shape-xform)
(: s-reflect:x shape-xform)
(: s-reflect:xy shape-xform)
(: s-reflect:x=y shape-xform)
(: s-all-xforms (Order Shape #:bottom-mask Shape #:left-mask Shape -> Shapes))
(: s-grow+2 shape-xform)
(: s-shrink-1 shape-xform)
(: s-normalise (Order Shape #:bottom-mask Shape #:left-mask Shape -> Shape))
(: draw-shapes (Order Shapes -> Void))
(: draw-polynominoes (polynominoes -> Void))
(: polynominoes->string (polynominoes -> String))
(: order-1-polynominoes polynominoes)
(: shape-add-bit (Order Shape Nonnegative-Integer -> Shape))
(: s-add-all-edges
   (Order (Shape -> Shape) Shape #:bottom-mask Shape #:left-mask Shape (#:seen? (Shape -> Boolean))
          (#:seen! (Option (Shape -> Void))) -> Shapes))
(: s-least-xform (Order Shape #:bottom-mask Shape #:left-mask Shape
                        (#:seen? (Option (Shape -> Boolean))) -> (Option Shape)))
(: polynominoes-add-new-order (-> polynominoes polynominoes))
(: nth-order-polynominoes (-> Positive-Integer polynominoes))
(: s-identity shape-xform)
(: order->bottom-mask (Order -> Shape))
(: order->left-mask (Order -> Shape))

;; get in touch with your inner C programmer
(define << arithmetic-shift)
(define bits bitwise-bit-field)

(define (draw-shapes o sss)
  (let: loop ((need-newline? : Boolean #f) (sss sss))
    (define 10-or-sss-len (min (length sss) 10))
    (define ss (take sss 10-or-sss-len))
    (for ((y (in-range 0 o)))
      (for ((s (in-list ss)) (n (in-naturals)) #:when #t (x (in-range 0 o)))
        (match* (n y x)
          [(0 0 _) (void)] [(0 _ 0) (newline)] [(_ _ 0) (write-char #\space)] [(_ _ _) (void)])
        (write-char (cond [(bitwise-bit-set? s (+ x (* y o))) #\#] [else #\.]))))
    (newline)
    (define sss- (drop sss 10-or-sss-len))
    (unless (null? sss-) (when need-newline? (newline)) (loop #t sss-))))

(define (draw-polynominoes p)
  (draw-shapes (polynominoes-order p) (polynominoes-shapes p)))

(define (polynominoes->string p)
  (with-output-to-string (λ () (draw-polynominoes p))))

(define order-1-polynominoes (polynominoes 1 '(1)))

(define (shape-add-bit o s b)
  (bitwise-ior s (<< 1 b)))

(define (s-reflect:y o s)
  (let: loop ((s : Shape s) (s+ : Shape 0))
    (if (zero? s) s+ (loop (<< s (- o)) (bitwise-ior (bits s 0 o) (<< s+ o))))))

(define (s-reflect:x o s)
  (let y-loop ((s+ : Shape 0) (y : Nonnegative-Integer (- o 1)))
    (let x-loop ((s+ : Shape s+) (x : Nonnegative-Integer 0) (b (* o y)))
      (cond [(= o x) (if (= y 0) s+ (y-loop s+ (- y 1)))]
            [else (x-loop (bitwise-ior (<< s+ 1) (bits s b (+ b 1))) (+ x 1) (+ b 1))]))))

(define (s-reflect:xy o s) (s-reflect:x o (s-reflect:y o s)))

(define (s-reflect:x=y o s)
  (define o-1 (sub1 o))
  (let b-loop ((s+ : Shape 0) (w-y o-1) (w-x o-1))
    (cond [(< w-y 0) s+]
          [else (define r-bit (+ (* w-x o) w-y))
                (b-loop (bitwise-ior (<< s+ 1) (bits s r-bit (+ r-bit 1)))
                        (if (zero? w-x) (sub1 w-y) w-y)
                        (if (zero? w-x) o-1 (sub1 w-x)))])))

(define (s-identity o s) s)

(define (order->bottom-mask o) (- (expt 2 o) 1))

(define (order->left-mask o) (for/fold ((m : Shape 0)) ((i (in-range 0 o))) (bitwise-ior 1 (<< m o))))

(define (s-least-xform o s #:bottom-mask bm #:left-mask lm #:seen? (seen? #f))
  (: ss1 (Option Shapes))
  (define ss1
    (let loop : (Option Shapes)
      ((rv : (Option Shapes) null)
       (xs : (Listof shape-xform)
           (list s-identity s-reflect:y s-reflect:x s-reflect:xy)))
      (cond
        [(null? xs) rv]
        [(not rv) #f] ; option assures rv's type in else clause
        [else
         (define s_ (s-normalise o ((car xs) o s) #:bottom-mask bm #:left-mask lm))
         (if (and seen? (seen? s_)) #f (loop (cons s_ rv) (cdr xs)))])))
  
  (and ss1
       (let loop : (Option Shape)
         ((rv : (Option Shape) (sub1 (expt 2 (sqr o))))
          (ss : Shapes ss1))
         (cond
           [(null? ss) rv]
           [else
            (define s0 (car ss))
            (define s_ (s-normalise o (s-reflect:x=y o s0) #:bottom-mask bm #:left-mask lm))
            (define least-s (min s0 s_))
            (cond [(and seen? (seen? s_)) #f]
                  [else (and rv (loop (min rv least-s) (cdr ss)))])]))))

(define (s-all-xforms o s #:bottom-mask bm #:left-mask lm)
  (: s1 Shapes)
  (: s2 Shapes)
  (define s1
    (for/list : Shapes
      ((x : shape-xform (in-list (list s-reflect:y s-reflect:x s-reflect:xy))))
      (x o s)))
  (define s2
    (for/list : Shapes ((s+ : Shape (in-list (cons s s1))))
      (s-reflect:x=y o s+)))
  
  (for/list : Shapes ((s (in-list (append s1 s2))))
    (s-normalise o s #:bottom-mask bm #:left-mask lm)))

(define (s-grow+2 o s)
  (define o+2 (+ o 2))
  (define -o (- o))
  (define s+
    (let: loop : Shape ((s : Shape s) (shft : Nonnegative-Integer 0) (rv : Shape 0))
      (if (zero? s) rv
          (loop (<< s -o)
                (+ shft o+2)
                (bitwise-ior rv (<< (bits s 0 o) shft))))))
  (<< s+ (+ o+2 1))) ; centre it

(define (s-shrink-1 o s)
  (define o-1 (sub1 o))
  (define -o (- o))
  (let: loop : Shape ((s- : Shape s) (shft : Nonnegative-Integer 0) (rv : Shape 0))
    (if (zero? s-) rv (loop (<< s- -o) (+ shft o-1) (bitwise-ior rv (<< (bits s- 0 o) shft))))))

(define (s-normalise o s #:bottom-mask bm #:left-mask lm)
  (cond [(zero? s) s]; stop an infinte loop!
        [else
         (define -o (- o))  
         ;; if there are no bits in a mask, we need to pull some in from...
         (: s-down Shape)
         (define s-down (let: loop : Shape ((s : Shape s))
                          (if (zero? (bitwise-and s bm)) (loop (<< s -o)) s)))
         (let loop : Shape ((s : Shape s-down)) (if (zero? (bitwise-and s lm)) (loop (<< s -1)) s))]))

(define (s-add-all-edges o shrink s
                         #:bottom-mask bm #:left-mask lm
                         #:seen! (seen! #f) #:seen? (seen? #f))
  (define o+2 (+ o 2))
  (define s+ (s-grow+2 o s))
  ;; it will be of a new order with edges all round -- so expand it into that
  (define blur (bitwise-ior s+ (<< s+ 1) (<< s+ -1) (<< s+ o+2) (<< s+ (- o+2))))
  (let: loop : Shapes
    ((b : Nonnegative-Integer 0)
     (e : Shape (bitwise-xor blur s+)) ; the edge is the blur, less the original s+
     (rv : Shapes null))
    (match e
      [0 rv] ; run out of bits
      [(? even?) (loop (+ b 1) (<< e -1) rv)] ; bit 0 isn't
      [_ (define lsx (s-least-xform o+2 (shape-add-bit o+2 s+ b)
                                    #:bottom-mask bm #:left-mask lm #:seen? seen?))
         (loop (+ b 1) (<< e -1) (if lsx (begin0 (cons (shrink lsx) rv)
                                                 (when seen! (seen! lsx)))
                                     rv))])))

(define (polynominoes-add-new-order p)
  (match-define (polynominoes o ss) p)
  (: saae (Shape -> Shapes))
  (: seen? (Shape -> Boolean))
  (: seen! (Shape -> Void))
  
  (define bm (order->bottom-mask (+ 2 o)))
  (define lm (order->left-mask (+ 2 o)))
  (define shrink (curry s-shrink-1 (+ o 2)))
  (define (seen! s) (hash-set! all-seen-shapes s #t))
  (define (seen? s) (hash-ref all-seen-shapes s #f))
  (define (saae s) (s-add-all-edges o shrink s #:seen? seen? #:seen! seen!
                                    #:bottom-mask bm #:left-mask lm))
  (define all-seen-shapes #{(make-hash) :: (HashTable Shape Boolean)})
  (define all-new-shapes
    (for*/list : Shapes ((k : Shape (in-list ss)) (s : Shape (in-list (saae k)))) s))  
  (polynominoes (add1 o) all-new-shapes))

(define nth-order-polynominoes
  (let ((polynominoes-cache #{(make-hash) :: (HashTable Positive-Integer polynominoes)}))
    (hash-set! polynominoes-cache 1 order-1-polynominoes)
    (lambda (n)
      (hash-ref! polynominoes-cache n
                 (λ () (polynominoes-add-new-order
                        (nth-order-polynominoes (cast (sub1 n) Positive-Integer))))))))

(module+ main
  (time
   (for ((n : Positive-Integer (in-range 1 (add1 12))))
     (define p (time (nth-order-polynominoes n)))
     (printf "n: ~a~%" n)
     (when (< n 6) (draw-polynominoes p))
     (printf "count: ~a~%~%" (length (polynominoes-shapes p)))
     (flush-output))))
```


{{out}}
Output is done up to 13 (on my clockwork laptop... tomorrow, better results on a competent machine)

```txt
cpu time: 0 real time: 0 gc time: 0
n: 1
#
count: 1

cpu time: 0 real time: 0 gc time: 0
n: 2
##
..
count: 1

cpu time: 0 real time: 0 gc time: 0
n: 3
### ##.
... #..
... ...
count: 2

cpu time: 0 real time: 0 gc time: 0
n: 4
#### ###. ###. ##.. .##.
.... .#.. #... ##.. ##..
.... .... .... .... ....
.... .... .... .... ....
count: 5

cpu time: 0 real time: 0 gc time: 0
n: 5
##### ####. ####. #.... ###.. .#... .#... ###.. ###.. .###.
..... .#... #.... ###.. ##... ###.. ###.. #.... #.#.. ##...
..... ..... ..... #.... ..... .#... #.... #.... ..... .....
..... ..... ..... ..... ..... ..... ..... ..... ..... .....
..... ..... ..... ..... ..... ..... ..... ..... ..... .....
..#.. .##..
###.. ##...
#.... #....
..... .....
..... .....
count: 12

cpu time: 0 real time: 0 gc time: 0
n: 6
count: 35

cpu time: 0 real time: 0 gc time: 0
n: 7
count: 108

cpu time: 63 real time: 31 gc time: 0
n: 8
count: 369

cpu time: 187 real time: 94 gc time: 0
n: 9
count: 1285

cpu time: 735 real time: 360 gc time: 0
n: 10
count: 4655

cpu time: 3172 real time: 2189 gc time: 142
n: 11
count: 17073

cpu time: 9047 real time: 9048 gc time: 343
n: 12
count: 63600

cpu time: 75125 real time: 75508 gc time: 3310
n: 13
count: 238591

cpu time: 88985 real time: 87683 gc time: 3983
```



## Ruby

{{trans|Python}}

```ruby
require 'set'

def translate2origin(poly)
  # Finds the min x and y coordiate of a Polyomino.
  minx = poly.map(&:first).min
  miny = poly.map(&:last).min
  poly.map{|x,y| [x - minx, y - miny]}.sort
end

def rotate90(x,y) [y, -x] end
def reflect(x,y)  [-x, y] end

# All the plane symmetries of a rectangular region.
def rotations_and_reflections(poly)
  [poly,
   poly = poly.map{|x,y| rotate90(x,y)},
   poly = poly.map{|x,y| rotate90(x,y)},
   poly = poly.map{|x,y| rotate90(x,y)},
   poly = poly.map{|x,y| reflect(x,y)},
   poly = poly.map{|x,y| rotate90(x,y)},
   poly = poly.map{|x,y| rotate90(x,y)},
          poly.map{|x,y| rotate90(x,y)} ]
end

def canonical(poly)
  rotations_and_reflections(poly).map{|pl| translate2origin(pl)}
end

# All four points in Von Neumann neighborhood.
def contiguous(x,y)
  [[x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]]
end

# Finds all distinct points that can be added to a Polyomino.
def new_points(poly)
  points = []
  poly.each{|x,y| contiguous(x,y).each{|point| points << point}}
  (points - poly).uniq
end

def new_polys(polys)
  pattern = Set.new
  polys.each_with_object([]) do |poly, polyomino|
    new_points(poly).each do |point|
      next if pattern.include?(pl = translate2origin(poly + [point]))
      polyomino << canonical(pl).each{|p| pattern << p}.min
    end
  end
end

# Generates polyominoes of rank n recursively.
def rank(n)
  case n
  when 0 then [[]]
  when 1 then [[[0,0]]]
  else        new_polys(rank(n-1))
  end
end

# Generates a textual representation of a Polyomino.
def text_representation(poly)
  table = Hash.new(' ')
  poly.each{|x,y| table[[x,y]] = '#'}
  maxx = poly.map(&:first).max
  maxy = poly.map(&:last).max
  (0..maxx).map{|x| (0..maxy).map{|y| table[[x,y]]}.join}
end

p (0..10).map{|n| rank(n).size}
n = ARGV[0] ? ARGV[0].to_i : 5
puts "\nAll free polyominoes of rank %d:" % n
rank(n).sort.each{|poly| puts text_representation(poly),""}
```


{{out}}

```txt

[1, 1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655]

All free polyominoes of rank 5:
#####

####
#   

####
 #  

###
## 

###
# #

###
#  
#  

###
 # 
 # 

### 
  ##

## 
 ##
 # 

## 
 ##
  #

## 
 # 
 ##

 # 
###
 # 

```



## Scala

Translation of [[Free_polyominoes_enumeration#Haskell|Haskell]] via [[Free_polyominoes_enumeration#D|Java]]
{{works with|Scala|2.12}}

```Scala
object Free {
  type Point = (Int, Int)
  type Polyomino = List[Point]

  def rotate90(p: Point): Point = (p._2, -p._1)

  def rotate180(p: Point): Point = (-p._1, -p._2)

  def rotate270(p: Point): Point = (-p._2, p._1)

  def reflect(p: Point): Point = (-p._1, p._2)

  def minima(polyomino: Polyomino): Point = {
    polyomino.reduce((a,b) => (Math.min(a._1, b._1), Math.min(a._2, b._2)))
  }

  def translateToOrigin(polyomino: Polyomino): Polyomino = {
    val m = minima(polyomino)
    polyomino.map(p => (p._1 - m._1, p._2 - m._2))
  }

  def rotationsAndReflections(polyomino: Polyomino): List[Polyomino] = {
    val refPol = polyomino.map(reflect)
    List(
      polyomino,
      polyomino.map(rotate90),
      polyomino.map(rotate180),
      polyomino.map(rotate270),
      refPol,
      refPol.map(rotate90), // === pol
      refPol.map(rotate180),
      refPol.map(rotate270),
    )
  }

  def canonical(polyomino: Polyomino): Polyomino = {
    import Ordering.Implicits._
    rotationsAndReflections(polyomino)
      .map(translateToOrigin)
      .map(poly => poly.sorted).min
  }

  def contiguous(p: Point): List[Point] = List(
    (p._1 - 1, p._2),
    (p._1 + 1, p._2),
    (p._1, p._2 - 1),
    (p._1, p._2 + 1),
  )

  def newPoints(polyomino: Polyomino): List[Point] = {
    polyomino.flatMap(contiguous).filterNot(polyomino.contains(_)).distinct
  }

  def newPolyominos(polyomino: Polyomino): List[Polyomino] = {
    newPoints(polyomino).map(p => canonical(p :: polyomino)).distinct
  }

  val monomino: Polyomino = List((0, 0))
  val monominos: List[Polyomino] = List(monomino)

  def rank(n: Int): List[Polyomino] = {
    require(n >= 0)
    n match {
      case 0 => Nil
      case 1 => monominos
      case _ => rank(n - 1).flatMap(newPolyominos).distinct
    }
  }
}
```



```txt
(0,0) (0,1) (1,1) (1,2) (2,1) 
(0,0) (0,1) (0,2) (1,0) (1,1) 
(0,0) (0,1) (0,2) (0,3) (1,1) 
(0,1) (1,0) (1,1) (1,2) (2,1) 
(0,0) (0,1) (0,2) (1,1) (2,1) 
(0,0) (0,1) (1,1) (1,2) (2,2) 
(0,0) (0,1) (0,2) (1,2) (1,3) 
(0,0) (0,1) (1,1) (2,1) (2,2) 
(0,0) (0,1) (0,2) (1,0) (1,2) 
(0,0) (0,1) (0,2) (0,3) (1,0) 
(0,0) (0,1) (0,2) (1,0) (2,0) 
(0,0) (0,1) (0,2) (0,3) (0,4)
```



## Sidef

{{trans|Ruby}}

```ruby
func translate2origin(poly) {
  # Finds the min x and y coordiate of a Polyomino.
  var minx = poly.map(:head).min
  var miny = poly.map(:tail).min
  poly.map {|p| [p.head-minx, p.tail-miny] }.sort
}

func rotate90(x,y) { [y, -x] }
func reflect(x,y)  { [-x, y] }

# All the plane symmetries of a rectangular region.
func rotations_and_reflections(poly) {
    gather {
        take(poly)
        take(poly.map!{ rotate90(_...) })
        take(poly.map!{ rotate90(_...) })
        take(poly.map!{ rotate90(_...) })
        take(poly.map!{  reflect(_...) })
        take(poly.map!{ rotate90(_...) })
        take(poly.map!{ rotate90(_...) })
        take(poly.map!{ rotate90(_...) })
    }
}

func canonical(poly) {
  rotations_and_reflections(poly).map{|pl| translate2origin(pl) }
}

# All four points in Von Neumann neighborhood.
func contiguous(x, y) {
  [[x-1, y], [x+1, y], [x, y-1], [x, y+1]]
}

# Finds all distinct points that can be added to a Polyomino.
func new_points(poly) {
  var points = Set()
  poly.each { points << contiguous(_...)... }
  points - poly
}

func new_polys(polys) {
  var pattern = Set()
  polys.map { |poly|
    gather {
      new_points(poly).each { |point|
        var pl = translate2origin(poly + [point])
        next if pattern.has(pl)
        take canonical(pl).each{ pattern << _ }.min
      }
    }...
  }
}

# Generates polyominoes of rank n recursively.
func rank(n) {
  given (n) {
    when (0) { [[]] }
    when (1) { [[[0,0]]] }
    else     { new_polys(rank(n-1)) }
  }
}

# Generates a textual representation of a Polyomino.
func text_representation(poly) {
  var table = Hash()
  for x,y in (poly) { table{[x,y]} = '#' }
  var maxx = poly.map(:head).max
  var maxy = poly.map(:tail).max
  (0..maxx).map{|x| (0..maxy).map{|y| table{[x,y]} \\ ' ' }.join }
}

say 8.of { rank(_).len }

var n = (ARGV[0] ? ARGV[0].to_i : 5)
say ("\nAll free polyominoes of rank %d:" % n)
rank(n).sort.each{|poly| say text_representation(poly).join("\n")+"\n" }
```

{{out}}
<pre style="height:250px">
[1, 1, 1, 2, 5, 12, 35, 108]

All free polyominoes of rank 5:
#####

####
#   

####
 #  

###
## 

###
# #

###
#  
#  

###
 # 
 # 

### 
  ##

## 
 ##
 # 

## 
 ##
  #

## 
 # 
 ##

 # 
###
 # 

```


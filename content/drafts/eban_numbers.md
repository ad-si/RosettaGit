+++
title = "Eban numbers"
description = ""
date = 2019-10-04T00:02:12Z
aliases = []
[extra]
id = 22189
[taxonomies]
categories = []
tags = []
+++

{{task}}


;Definition:
An   '''eban'''   number is a number that has no letter   <big> '''e''' </big>   in it when the number is spelled in English.

Or more literally,   spelled numbers that contain the letter   '''e'''   are banned.


The American version of spelling numbers will be used here   (as opposed to the British).

'''2,000,000,000'''   is two billion,   ''not''   two milliard.


Only numbers less than   '''one sextillion'''   ('''10<sup>21</sup>''')   will be considered in/for this task.

This will allow optimizations to be used.



;Task:
:::*   show all eban numbers   ≤   '''1,000'''   (in a horizontal format),                   and a count
:::*   show all eban numbers between   '''1,000'''   and   '''4,000'''   (inclusive),   and a count
:::*   show a count of all eban numbers up and including            '''10,000'''
:::*   show a count of all eban numbers up and including                '''100,000'''
:::*   show a count of all eban numbers up and including                   '''1,000,000'''
:::*   show a count of all eban numbers up and including                      '''10,000,000'''
:::*   show all output here.


;See also:
:*   The MathWorld entry:   [http://mathworld.wolfram.com/EbanNumber.html  eban numbers].
:*   The OEIS      entry:   [http://oeis.org/A006933 A6933,                eban numbers].





## AWK


```AWK

# syntax: GAWK -f EBAN_NUMBERS.AWK
# converted from FreeBASIC
BEGIN {
    main(2,1000,1)
    main(1000,4000,1)
    main(2,10000,0)
    main(2,100000,0)
    main(2,1000000,0)
    main(2,10000000,0)
    main(2,100000000,0)
    exit(0)
}
function main(start,stop,printable,  b,count,i,m,r,t) {
    printf("%d-%d:",start,stop)
    for (i=start; i<=stop; i+=2) {
      b = int(i / 1000000000)
      r = i % 1000000000
      m = int(r / 1000000)
      r = i % 1000000
      t = int(r / 1000)
      r = r % 1000
      if (m >= 30 && m <= 66) { m %= 10 }
      if (t >= 30 && t <= 66) { t %= 10 }
      if (r >= 30 && r <= 66) { r %= 10 }
      if (x(b) && x(m) && x(t) && x(r)) {
        count++
        if (printable) {
          printf(" %d",i)
        }
      }
    }
    printf(" (count=%d)\n",count)
}
function x(n) {
    return(n == 0 || n == 2 || n == 4 || n == 6)
}

```

{{out}}

```txt

2-1000: 2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66 (count=19)
1000-4000: 2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000 (count=21)
2-10000: (count=79)
2-100000: (count=399)
2-1000000: (count=399)
2-10000000: (count=1599)
2-100000000: (count=7999)

```


## C

{{trans|D}}

```c
#include "stdio.h"
#include "stdbool.h"

#define ARRAY_LEN(a,T) (sizeof(a) / sizeof(T))

struct Interval {
    int start, end;
    bool print;
};

int main() {
    struct Interval intervals[] = {
        {2, 1000, true},
        {1000, 4000, true},
        {2, 10000, false},
        {2, 100000, false},
        {2, 1000000, false},
        {2, 10000000, false},
        {2, 100000000, false},
        {2, 1000000000, false},
    };
    int idx;

    for (idx = 0; idx < ARRAY_LEN(intervals, struct Interval); ++idx) {
        struct Interval intv = intervals[idx];
        int count = 0, i;

        if (intv.start == 2) {
            printf("eban numbers up to and including %d:\n", intv.end);
        } else {
            printf("eban numbers between %d and %d (inclusive:)", intv.start, intv.end);
        }

        for (i = intv.start; i <= intv.end; i += 2) {
            int b = i / 1000000000;
            int r = i % 1000000000;
            int m = r / 1000000;
            int t;

            r = i % 1000000;
            t = r / 1000;
            r %= 1000;
            if (m >= 30 && m <= 66) m %= 10;
            if (t >= 30 && t <= 66) t %= 10;
            if (r >= 30 && r <= 66) r %= 10;
            if (b == 0 || b == 2 || b == 4 || b == 6) {
                if (m == 0 || m == 2 || m == 4 || m == 6) {
                    if (t == 0 || t == 2 || t == 4 || t == 6) {
                        if (r == 0 || r == 2 || r == 4 || r == 6) {
                            if (intv.print) printf("%d ", i);
                            count++;
                        }
                    }
                }
            }
        }
        if (intv.print) {
            printf("\n");
        }
        printf("count = %d\n\n", count);
    }

    return 0;
}
```

{{out}}

```txt
eban numbers up to and including 1000:
2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66
count = 19

eban numbers between 1000 and 4000 (inclusive:)2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000
count = 21

eban numbers up to and including 10000:
count = 79

eban numbers up to and including 100000:
count = 399

eban numbers up to and including 1000000:
count = 399

eban numbers up to and including 10000000:
count = 1599

eban numbers up to and including 100000000:
count = 7999

eban numbers up to and including 1000000000:
count = 7999
```


## C#
{{trans|D}}

```c#
using System;

namespace EbanNumbers {
    struct Interval {
        public int start, end;
        public bool print;

        public Interval(int start, int end, bool print) {
            this.start = start;
            this.end = end;
            this.print = print;
        }
    }

    class Program {
        static void Main() {
            Interval[] intervals = {
                new Interval(2, 1_000, true),
                new Interval(1_000, 4_000, true),
                new Interval(2, 10_000, false),
                new Interval(2, 100_000, false),
                new Interval(2, 1_000_000, false),
                new Interval(2, 10_000_000, false),
                new Interval(2, 100_000_000, false),
                new Interval(2, 1_000_000_000, false),
            };
            foreach (var intv in intervals) {
                if (intv.start == 2) {
                    Console.WriteLine("eban numbers up to and including {0}:", intv.end);
                } else {
                    Console.WriteLine("eban numbers between {0} and {1} (inclusive):", intv.start, intv.end);
                }

                int count = 0;
                for (int i = intv.start; i <= intv.end; i += 2) {
                    int b = i / 1_000_000_000;
                    int r = i % 1_000_000_000;
                    int m = r / 1_000_000;
                    r = i % 1_000_000;
                    int t = r / 1_000;
                    r %= 1_000;
                    if (m >= 30 && m <= 66) m %= 10;
                    if (t >= 30 && t <= 66) t %= 10;
                    if (r >= 30 && r <= 66) r %= 10;
                    if (b == 0 || b == 2 || b == 4 || b == 6) {
                        if (m == 0 || m == 2 || m == 4 || m == 6) {
                            if (t == 0 || t == 2 || t == 4 || t == 6) {
                                if (r == 0 || r == 2 || r == 4 || r == 6) {
                                    if (intv.print) Console.Write("{0} ", i);
                                    count++;
                                }
                            }
                        }
                    }
                }
                if (intv.print) {
                    Console.WriteLine();
                }
                Console.WriteLine("count = {0}\n", count);
            }
        }
    }
}
```

{{out}}

```txt
eban numbers up to and including 1000:
2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66
count = 19

eban numbers between 1000 and 4000 (inclusive):
2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000
count = 21

eban numbers up to and including 10000:
count = 79

eban numbers up to and including 100000:
count = 399

eban numbers up to and including 1000000:
count = 399

eban numbers up to and including 10000000:
count = 1599

eban numbers up to and including 100000000:
count = 7999

eban numbers up to and including 1000000000:
count = 7999
```



## D

{{trans|Kotlin}}

```d
import std.stdio;

struct Interval {
    int start, end;
    bool print;
}

void main() {
    Interval[] intervals = [
        {2, 1_000, true},
        {1_000, 4_000, true},
        {2, 10_000, false},
        {2, 100_000, false},
        {2, 1_000_000, false},
        {2, 10_000_000, false},
        {2, 100_000_000, false},
        {2, 1_000_000_000, false},
    ];
    foreach (intv; intervals) {
        if (intv.start == 2) {
            writeln("eban numbers up to an including ", intv.end, ':');
        } else {
            writeln("eban numbers between ", intv.start ," and ", intv.end, " (inclusive):");
        }

        int count;
        for (int i = intv.start; i <= intv.end; i = i + 2) {
            int b = i / 1_000_000_000;
            int r = i % 1_000_000_000;
            int m = r / 1_000_000;
            r = i % 1_000_000;
            int t = r / 1_000;
            r %= 1_000;
            if (m >= 30 && m <= 66) m %= 10;
            if (t >= 30 && t <= 66) t %= 10;
            if (r >= 30 && r <= 66) r %= 10;
            if (b == 0 || b == 2 || b == 4 || b == 6) {
                if (m == 0 || m == 2 || m == 4 || m == 6) {
                    if (t == 0 || t == 2 || t == 4 || t == 6) {
                        if (r == 0 || r == 2 || r == 4 || r == 6) {
                            if (intv.print) write(i, ' ');
                            count++;
                        }
                    }
                }
            }
        }
        if (intv.print) {
            writeln();
        }
        writeln("count = ", count);
        writeln;
    }
}
```

{{out}}

```txt
eban numbers up to an including 1000:
2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66
count = 19

eban numbers between 1000 and 4000 (inclusive):
2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000
count = 21

eban numbers up to an including 10000:
count = 79

eban numbers up to an including 100000:
count = 399

eban numbers up to an including 1000000:
count = 399

eban numbers up to an including 10000000:
count = 1599

eban numbers up to an including 100000000:
count = 7999

eban numbers up to an including 1000000000:
count = 7999
```



## Factor

{{trans|Julia}}

```factor
USING: arrays formatting fry io kernel math math.functions
math.order math.ranges prettyprint sequences ;

: eban? ( n -- ? )
    1000000000 /mod 1000000 /mod 1000 /mod
    [ dup 30 66 between? [ 10 mod ] when ] tri@ 4array
    [ { 0 2 4 6 } member? ] all? ;

: .eban ( m n -- ) "eban numbers in [%d, %d]: " printf ;
: eban ( m n q -- o ) '[ 2dup .eban [a,b] [ eban? ] @ ] call ; inline
: .eban-range ( m n -- ) [ filter ] eban "%[%d, %]\n" printf ;
: .eban-count ( m n -- ) "count of " write [ count ] eban . ;

1 1000 1000 4000 [ .eban-range ] 2bi@
4 9 [a,b] [ [ 1 10 ] dip ^ .eban-count ] each
```

{{out}}

```txt

eban numbers in [1, 1000]: { 2, 4, 6, 30, 32, 34, 36, 40, 42, 44, 46, 50, 52, 54, 56, 60, 62, 64, 66 }
eban numbers in [1000, 4000]: { 2000, 2002, 2004, 2006, 2030, 2032, 2034, 2036, 2040, 2042, 2044, 2046, 2050, 2052, 2054, 2056, 2060, 2062, 2064, 2066, 4000 }
count of eban numbers in [1, 10000]: 79
count of eban numbers in [1, 100000]: 399
count of eban numbers in [1, 1000000]: 399
count of eban numbers in [1, 10000000]: 1599
count of eban numbers in [1, 100000000]: 7999
count of eban numbers in [1, 1000000000]: 7999

```



## FreeBASIC


```freebasic

' Eban_numbers
' Un número eban es un número que no tiene la letra e cuando el número está escrito en inglés.
' O más literalmente, los números escritos que contienen la letra e están prohibidos.
'
' Usaremos la versión americana de los números de ortografía (a diferencia de los británicos).
'  2000000000 son dos billones, no dos millardos (mil millones).
'

Data 2, 1000, 1
Data 1000, 4000, 1
Data 2, 10000, 0
Data 2, 100000, 0
Data 2, 1000000, 0
Data 2, 10000000, 0
Data 2, 100000000, 0
Data 0, 0, 0

Dim As Double tiempo = Timer
Dim As Integer start, ended, printable, count
Dim As Long i, b, r, m, t
Do
    Read start, ended, printable

    If start = 0 Then Exit Do
    If start = 2 Then
        Print "eban numbers up to and including"; ended; ":"
    Else
        Print "eban numbers between "; start; " and "; ended; " (inclusive):"
    End If

    count = 0
    For i = start To ended Step 2
        b = Int(i / 1000000000)
        r = (i Mod 1000000000)
        m = Int(r / 1000000)
        r = (i Mod 1000000)
        t = Int(r / 1000)
        r = (r Mod 1000)
        If m >= 30 And m <= 66 Then m = (m Mod 10)
        If t >= 30 And t <= 66 Then t = (t Mod 10)
        If r >= 30 And r <= 66 Then r = (r Mod 10)
        If b = 0 Or b = 2 Or b = 4 Or b = 6 Then
            If m = 0 Or m = 2 Or m = 4 Or m = 6 Then
                If t = 0 Or t = 2 Or t = 4 Or t = 6 Then
                    If r = 0 Or r = 2 Or r = 4 Or r = 6 Then
                        If printable Then Print i;
                        count += 1
                    End If
                End If
            End If
        End If
    Next i
    If printable Then Print
    Print "count = "; count & Chr(10)
Loop
tiempo = Timer - tiempo
Print "Run time: " & (tiempo) & " seconds."
End

```

{{out}}

```txt

eban numbers up to and including 100:
2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66
count = 19

eban numbers between 1000 and 4000 (inclusive):
2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000
count = 21

eban numbers up to and including 10000:
count = 79

eban numbers up to and including 100000:
count = 399

eban numbers up to and including 1000000:
count = 399

eban numbers up to and including 10000000:
count = 1599

eban numbers up to and including 100000000:
count = 7999

Run time: 1.848286400010693 seconds.

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Eban_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go


```go
package main

import "fmt"

type Range struct {
    start, end uint64
    print      bool
}

func main() {
    rgs := []Range{
        {2, 1000, true},
        {1000, 4000, true},
        {2, 1e4, false},
        {2, 1e5, false},
        {2, 1e6, false},
        {2, 1e7, false},
        {2, 1e8, false},
        {2, 1e9, false},
    }
    for _, rg := range rgs {
        if rg.start == 2 {
            fmt.Printf("eban numbers up to and including %d:\n", rg.end)
        } else {
            fmt.Printf("eban numbers between %d and %d (inclusive):\n", rg.start, rg.end)
        }
        count := 0
        for i := rg.start; i <= rg.end; i += 2 {
            b := i / 1000000000
            r := i % 1000000000
            m := r / 1000000
            r = i % 1000000
            t := r / 1000
            r %= 1000
            if m >= 30 && m <= 66 {
                m %= 10
            }
            if t >= 30 && t <= 66 {
                t %= 10
            }
            if r >= 30 && r <= 66 {
                r %= 10
            }
            if b == 0 || b == 2 || b == 4 || b == 6 {
                if m == 0 || m == 2 || m == 4 || m == 6 {
                    if t == 0 || t == 2 || t == 4 || t == 6 {
                        if r == 0 || r == 2 || r == 4 || r == 6 {
                            if rg.print {
                                fmt.Printf("%d ", i)
                            }
                            count++
                        }
                    }
                }
            }
        }
        if rg.print {
            fmt.Println()
        }
        fmt.Println("count =", count, "\n")
    }
}
```


{{out}}

```txt

eban numbers up to and including 1000:
2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66
count = 19

eban numbers between 1000 and 4000 (inclusive):
2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000
count = 21

eban numbers up to and including 10000:
count = 79

eban numbers up to and including 100000:
count = 399

eban numbers up to and including 1000000:
count = 399

eban numbers up to and including 10000000:
count = 1599

eban numbers up to and including 100000000:
count = 7999

eban numbers up to and including 1000000000:
count = 7999

```



## J


```J

Filter =: (#~`)(`:6)

itemAmend =: (29&< *. <&67)`(,: 10&|)}
iseban =: [: *./ 0 2 4 6 e.~ [: itemAmend [: |: (4#1000)&#:


   (;~ #) iseban Filter >: i. 1000
┌──┬─────────────────────────────────────────────────────┐
│19│2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66│
└──┴─────────────────────────────────────────────────────┘

   NB. INPUT are the correct integers, head and tail shown
   ({. , {:) INPUT =: 1000 + i. 3001
1000 4000

   (;~ #)  iseban Filter INPUT
┌──┬────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│21│2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000│
└──┴────────────────────────────────────────────────────────────────────────────────────────────────────────┘
   (, ([: +/ [: iseban [: >: i.))&> 10000 * 10 ^ i. +:2
 10000   79
100000  399
   1e6  399
   1e7 1599

```



## Julia


```Julia

function iseban(n::Integer)
    b, r = divrem(n, oftype(n, 10 ^ 9))
    m, r = divrem(r, oftype(n, 10 ^ 6))
    t, r = divrem(r, oftype(n, 10 ^ 3))
    m, t, r = (30 <= x <= 66 ? x % 10 : x for x in (m, t, r))
    return all(in((0, 2, 4, 6)), (b, m, t, r))
end

println("eban numbers up to and including 1000:")
println(join(filter(iseban, 1:100), ", "))

println("eban numbers between 1000 and 4000 (inclusive):")
println(join(filter(iseban, 1000:4000), ", "))

println("eban numbers up to and including 10000: ", count(iseban, 1:10000))
println("eban numbers up to and including 100000: ", count(iseban, 1:100000))
println("eban numbers up to and including 1000000: ", count(iseban, 1:1000000))
println("eban numbers up to and including 10000000: ", count(iseban, 1:10000000))
println("eban numbers up to and including 100000000: ", count(iseban, 1:100000000))
println("eban numbers up to and including 1000000000: ", count(iseban, 1:1000000000))

```


{{out}}

```txt
eban numbers up to and including 1000:
2, 4, 6, 30, 32, 34, 36, 40, 42, 44, 46, 50, 52, 54, 56, 60, 62, 64, 66
eban numbers between 1000 and 4000 (inclusive):
2000, 2002, 2004, 2006, 2030, 2032, 2034, 2036, 2040, 2042, 2044, 2046, 2050, 2052, 2054, 2056, 2060, 2062, 2064, 2066, 4000
eban numbers up to and including 10000: 79
eban numbers up to and including 100000: 399
eban numbers up to and including 1000000: 399
eban numbers up to and including 10000000: 1599
eban numbers up to and including 100000000: 7999
eban numbers up to and including 1000000000: 7999
```



## Kotlin

{{trans|Go}}

```scala
// Version 1.3.21

typealias Range = Triple<Int, Int, Boolean>

fun main() {
    val rgs = listOf<Range>(
        Range(2, 1000, true),
        Range(1000, 4000, true),
        Range(2, 10_000, false),
        Range(2, 100_000, false),
        Range(2, 1_000_000, false),
        Range(2, 10_000_000, false),
        Range(2, 100_000_000, false),
        Range(2, 1_000_000_000, false)
    )
    for (rg in rgs) {
        val (start, end, prnt) = rg
        if (start == 2) {
            println("eban numbers up to and including $end:")
        } else {
            println("eban numbers between $start and $end (inclusive):")
        }
        var count = 0
        for (i in start..end step 2) {
            val b = i / 1_000_000_000
            var r = i % 1_000_000_000
            var m = r / 1_000_000
            r = i % 1_000_000
            var t = r / 1_000
            r %= 1_000
            if (m >= 30 && m <= 66) m %= 10
            if (t >= 30 && t <= 66) t %= 10
            if (r >= 30 && r <= 66) r %= 10
            if (b == 0 || b == 2 || b == 4 || b == 6) {
                if (m == 0 || m == 2 || m == 4 || m == 6) {
                    if (t == 0 || t == 2 || t == 4 || t == 6) {
                        if (r == 0 || r == 2 || r == 4 || r == 6) {
                            if (prnt) print("$i ")
                            count++
                        }
                    }
                }
            }
        }
        if (prnt) println()
        println("count = $count\n")
    }
}
```


{{output}}

```txt

Same as Go example.

```



## Perl


### Exhaustive search

A couple of 'e'-specific optimizations keep the running time reasonable.

```perl
use strict;
use warnings;
use feature 'say';
use Lingua::EN::Numbers qw(num2en);

sub comma { reverse ((reverse shift) =~ s/(.{3})/$1,/gr) =~ s/^,//r }

sub e_ban {
    my($power) = @_;
    my @n;
    for (1..10**$power) {
        next unless 0 == $_%2;
        next if $_ =~ /[789]/ or /[12].$/ or /[135]..$/ or /[135]...$/ or /[135].....$/;
        push @n, $_ unless num2en($_) =~ /e/;
    }
    @n;
}

my @OK = e_ban(my $max = 7);

my @a = grep { $_ <= 1000 } @OK;
say "Number of eban numbers up to and including 1000: @{[1+$#a]}";
say join(', ',@a);
say '';

my @b = grep { $_ >= 1000 && $_ <= 4000 } @OK;
say "Number of eban numbers between 1000 and 4000 (inclusive): @{[1+$#b]}";
say join(', ',@b);
say '';

for my $exp (4..$max) {
    my $n = + grep { $_ <= 10**$exp } @OK;
    printf "Number of eban numbers and %10s: %d\n", comma(10**$exp), $n;
}
```

{{out}}

```txt
eban numbers up to and including 1000:
2, 4, 6, 30, 32, 34, 36, 40, 42, 44, 46, 50, 52, 54, 56, 60, 62, 64, 66

eban numbers between 1000 and 4000 (inclusive):
2000, 2002, 2004, 2006, 2030, 2032, 2034, 2036, 2040, 2042, 2044, 2046, 2050, 2052, 2054, 2056, 2060, 2062, 2064, 2066, 4000

Number of eban numbers up to     10,000: 79
Number of eban numbers up to    100,000: 399
Number of eban numbers up to  1,000,000: 399
Number of eban numbers up to 10,000,000: 1599
```



### Algorithmically generate / count

Alternately, a partial translation of Perl 6. Does not need to actually generate the e-ban numbers to count them. Display counts up to 10**21.


```perl
use strict;
use warnings;
use bigint;
use feature 'say';
use Lingua::EN::Nums2Words 'num2word';
use List::AllUtils 'sum';

sub comma { reverse ((reverse shift) =~ s/(.{3})/$1,/gr) =~ s/^,//r }

sub nban {
    my ($n, @numbers) = @_;
    grep { lc(num2word($_)) !~ /[$n]/i } @numbers;
}

sub enumerate {
    my ($n, $upto) = @_;
    my @ban = nban($n, 1 .. 99);
    my @orders;
    for my $o (2 .. $upto) {
        push @orders, [nban($n, map { $_ * 10**$o } 1 .. 9)];
    }
    for my $oom (@orders) {
        next unless +@$oom;
        my @these;
        for my $num (@$oom) {
            push @these, $num, map { $_ + $num } @ban;
        }
       push @ban, @these;
    }
    unshift @ban, 0 if nban($n, 0);
    @ban
}

sub count {
    my ($n, $upto) = @_;
    my @orders;
    for my $o (2 .. $upto) {
        push @orders, [nban($n, map { $_ * 10**$o } 1 .. 9)];
    }
    my @count = scalar nban($n, 1 .. 99);
    for my $o ( 0 .. $#orders - 1 ) {
        push @count, sum(@count) * (scalar @{$orders[$o]}) + (scalar @{$orders[$o]});
    }
    ++$count[0] if nban($n, 0);
    for my $m ( 0 .. $#count - 1 ) {
        next unless scalar $orders[$m];
        if (nban($n, 10**($m+2))) { $count[$m]++; $count[$m + 1]-- }
    }
    map { sum( @count[0..$_] ) } 0..$#count;
}

for my $t ('e') {
    my @bans  = enumerate($t, 4);
    my @count = count($t, my $max = 21);

    my @j = grep { $_ <= 10 } @bans;
    unshift @count, @{[1+$#j]};

    say "\n
### =======
 $t-ban:
### =======
";
    my @a = grep { $_ <= 1000 } @bans;
    say "$t-ban numbers up to 1000: @{[1+$#a]}";
    say '[', join(' ',@a), ']';
    say '';

    my @b = grep { $_ >= 1000 && $_ <= 4000 } @bans;
    say "$t-ban numbers between 1,000 & 4,000 (inclusive): @{[1+$#b]}";
    say '[', join(' ',@b), ']';
    say '';

    say "Counts of $t-ban numbers up to ", lc(num2word(10**$max));

    for my $exp (1..$max) {
        my $nu = $count[$exp-1];
        printf "Up to and including %23s: %s\n", lc(num2word(10**$exp)), comma($nu);
    }
}
```


```txt

### =======
 e-ban:
### =======

e-ban numbers up to 1000: 19
[2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66]

e-ban numbers between 1,000 & 4,000 (inclusive): 21
[2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000]

Counts of e-ban numbers up to one sextillion
Up to and including                     ten: 3
Up to and including             one hundred: 19
Up to and including            one thousand: 19
Up to and including            ten thousand: 79
Up to and including    one hundred thousand: 399
Up to and including             one million: 399
Up to and including             ten million: 1,599
Up to and including     one hundred million: 7,999
Up to and including             one billion: 7,999
Up to and including             ten billion: 31,999
Up to and including     one hundred billion: 159,999
Up to and including            one trillion: 159,999
Up to and including            ten trillion: 639,999
Up to and including    one hundred trillion: 3,199,999
Up to and including         one quadrillion: 3,199,999
Up to and including         ten quadrillion: 12,799,999
Up to and including one hundred quadrillion: 63,999,999
Up to and including         one quintillion: 63,999,999
Up to and including         ten quintillion: 255,999,999
Up to and including one hundred quintillion: 1,279,999,999
Up to and including          one sextillion: 1,279,999,999
```



## Perl 6

{{works with|Rakudo|2018.12}}
Modular approach, very little is hard coded. Change the $upto order-of-magnitude limit to adjust the search/display ranges. Change the letter(s) given to the enumerate / count subs to modify which letter(s) to disallow.

Will handle multi-character 'bans'. Demonstrate for e-ban, t-ban and subur-ban.

Directly find :
:* [[oeis:A006933|OEIS:A006933 Numbers without e]]: Eban
:* [[oeis:A008521|OEIS:A008521 Numbers without o]]: Oban
:* [[oeis:A008523|OEIS:A008523 Numbers without t]]: Tban
:* [[oeis:A072954|OEIS:A072954 Numbers without a, i, l, t]]: TALIban
:* [[oeis:A072955|OEIS:A072955 Numbers without b, r, s, u]]: SUBURban
:* [[oeis:A072956|OEIS:A072956 Numbers without r, t, u]]: TURban
:* [[oeis:A072957|OEIS:A072957 Numbers without r, u]]: URban
:* [[oeis:A072958|OEIS:A072958 Numbers without a, c, i, l]]: CALIban
:* [[oeis:A089589|OEIS:A089589 Numbers without i]]: Iban
:* [[oeis:A089590|OEIS:A089590 Numbers without u]]: Uban
:* ''and so on...''

Considering numbers up to <strong>10<sup>21</sup></strong>, as the task directions suggest.


```perl6
use Lingua::EN::Numbers;

sub nban ($seq, $n = 'e') { ($seq).map: { next if .&cardinal.contains(any($n.lc.comb)); $_ } }

sub enumerate ($n, $upto) {
    my @ban = [nban(1 .. 99, $n)],;
    my @orders;
    (2 .. $upto).map: -> $o {
        given $o % 3 { # Compensate for irregulars: 11 - 19
            when 1  { @orders.push: [flat (10**($o - 1) X* 10 .. 19).map(*.&nban($n)), |(10**$o X* 2 .. 9).map: *.&nban($n)] }
            default { @orders.push: [flat (10**$o X* 1 .. 9).map: *.&nban($n)] }
        }
    }
    ^@orders .map: -> $o {
        @ban.push: [] and next unless +@orders[$o];
        my @these;
        @orders[$o].map: -> $m {
            @these.push: $m;
            for ^@ban -> $b {
                next unless +@ban[$b];
                @these.push: $_ for (flat @ban[$b]) »+» $m ;
            }
        }
        @ban.push: @these;
    }
    @ban.unshift(0) if nban(0, $n);
    flat @ban.map: *.flat;
}

sub count ($n, $upto) {
    my @orders;
    (2 .. $upto).map: -> $o {
        given $o % 3 { # Compensate for irregulars: 11 - 19
            when 1  { @orders.push: [flat (10**($o - 1) X* 10 .. 19).map(*.&nban($n)), |(10**$o X* 2 .. 9).map: *.&nban($n)] }
            default { @orders.push: [flat (10**$o X* 1 .. 9).map: *.&nban($n)] }
        }
    }
    my @count  = +nban(1 .. 99, $n);
    ^@orders .map: -> $o {
        @count.push: 0 and next unless +@orders[$o];
        my $prev = so (@orders[$o].first( { $_ ~~ /^ '1' '0'+ $/ } ) // 0 );
        my $sum = @count.sum;
        my $these = +@orders[$o] * $sum + @orders[$o];
        $these-- if $prev;
        @count[1 + $o] += $these;
        ++@count[$o]  if $prev;
    }
    ++@count[0] if nban(0, $n);
    [\+] @count;
}

#for < e o t tali subur tur ur cali i u > -> $n { # All of them
for < e t subur > -> $n { # An assortment for demonstration
    my $upto   = 21; # 1e21
    my @bans   = enumerate($n, 4);
    my @counts = count($n, $upto);

    # DISPLAY
    my @k = @bans.grep: * < 1000;
    my @j = @bans.grep: 1000 <= * <= 4000;
    put "\n
### =======
 {$n}-ban:
### =======
\n" ~
        "{$n}-ban numbers up to 1000: {+@k}\n[{@k».&comma}]\n\n" ~
        "{$n}-ban numbers between 1,000 & 4,000: {+@j}\n[{@j».&comma}]\n" ~
        "\nCounts of {$n}-ban numbers up to {cardinal 10**$upto}"
        ;

    my $s = max (1..$upto).map: { (10**$_).&cardinal.chars };
    @counts.unshift: @bans.first: * > 10, :k;
    for ^$upto -> $c {
        printf "Up to and including %{$s}s: %s\n", cardinal(10**($c+1)), comma(@counts[$c]);
    }
}
```

{{out}}

```txt

### =======
 e-ban:
### =======

e-ban numbers up to 1000: 19
[2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66]

e-ban numbers between 1,000 & 4,000: 21
[2,000 2,002 2,004 2,006 2,030 2,032 2,034 2,036 2,040 2,042 2,044 2,046 2,050 2,052 2,054 2,056 2,060 2,062 2,064 2,066 4,000]

Counts of e-ban numbers up to one sextillion
Up to and including                     ten: 3
Up to and including             one hundred: 19
Up to and including            one thousand: 19
Up to and including            ten thousand: 79
Up to and including    one hundred thousand: 399
Up to and including             one million: 399
Up to and including             ten million: 1,599
Up to and including     one hundred million: 7,999
Up to and including             one billion: 7,999
Up to and including             ten billion: 31,999
Up to and including     one hundred billion: 159,999
Up to and including            one trillion: 159,999
Up to and including            ten trillion: 639,999
Up to and including    one hundred trillion: 3,199,999
Up to and including         one quadrillion: 3,199,999
Up to and including         ten quadrillion: 12,799,999
Up to and including one hundred quadrillion: 63,999,999
Up to and including         one quintillion: 63,999,999
Up to and including         ten quintillion: 255,999,999
Up to and including one hundred quintillion: 1,279,999,999
Up to and including          one sextillion: 1,279,999,999


### =======
 t-ban:
### =======

t-ban numbers up to 1000: 56
[0 1 4 5 6 7 9 11 100 101 104 105 106 107 109 111 400 401 404 405 406 407 409 411 500 501 504 505 506 507 509 511 600 601 604 605 606 607 609 611 700 701 704 705 706 707 709 711 900 901 904 905 906 907 909 911]

t-ban numbers between 1,000 & 4,000: 0
[]

Counts of t-ban numbers up to one sextillion
Up to and including                     ten: 7
Up to and including             one hundred: 9
Up to and including            one thousand: 56
Up to and including            ten thousand: 56
Up to and including    one hundred thousand: 56
Up to and including             one million: 57
Up to and including             ten million: 392
Up to and including     one hundred million: 785
Up to and including             one billion: 5,489
Up to and including             ten billion: 38,416
Up to and including     one hundred billion: 76,833
Up to and including            one trillion: 537,824
Up to and including            ten trillion: 537,824
Up to and including    one hundred trillion: 537,824
Up to and including         one quadrillion: 537,825
Up to and including         ten quadrillion: 3,764,768
Up to and including one hundred quadrillion: 7,529,537
Up to and including         one quintillion: 52,706,752
Up to and including         ten quintillion: 52,706,752
Up to and including one hundred quintillion: 52,706,752
Up to and including          one sextillion: 52,706,752


### =======
 subur-ban:
### =======

subur-ban numbers up to 1000: 35
[1 2 5 8 9 10 11 12 15 18 19 20 21 22 25 28 29 50 51 52 55 58 59 80 81 82 85 88 89 90 91 92 95 98 99]

subur-ban numbers between 1,000 & 4,000: 0
[]

Counts of subur-ban numbers up to one sextillion
Up to and including                     ten: 6
Up to and including             one hundred: 35
Up to and including            one thousand: 35
Up to and including            ten thousand: 35
Up to and including    one hundred thousand: 35
Up to and including             one million: 36
Up to and including             ten million: 216
Up to and including     one hundred million: 2,375
Up to and including             one billion: 2,375
Up to and including             ten billion: 2,375
Up to and including     one hundred billion: 2,375
Up to and including            one trillion: 2,375
Up to and including            ten trillion: 2,375
Up to and including    one hundred trillion: 2,375
Up to and including         one quadrillion: 2,375
Up to and including         ten quadrillion: 2,375
Up to and including one hundred quadrillion: 2,375
Up to and including         one quintillion: 2,375
Up to and including         ten quintillion: 2,375
Up to and including one hundred quintillion: 2,375
Up to and including          one sextillion: 2,375
```


Note that the limit to one sextillion is somewhat arbitrary and is just to match the task parameters.

This will quite happily count *-bans up to one hundred centillion. (10<sup>305</sup>) It takes longer, but still on the order of seconds, not minutes.

```txt
Counts of e-ban numbers up to one hundred centillion
 ...
Up to and including one hundred centillion: 35,184,372,088,831,999,999,999,999,999,999,999,999,999,999,999,999,999,999,999
```



## Phix

Why count when you can calculate?

```Phix
function count_eban(integer p10)
-- returns the count of eban numbers 1..power(10,p10)
    integer n = p10-floor(p10/3),
            p5 = floor(n/2),
            p4 = floor((n+1)/2)
    return power(5,p5)*power(4,p4)-1
end function

function eban(integer n)
-- returns true if n is an eban number (only fully tested to 10e9)
    if n=0 then return false end if
    while n do
        integer thou = remainder(n,1000)
        if floor(thou/100)!=0 then return false end if
        if not find(floor(thou/10),{0,3,4,5,6}) then return false end if
        if not find(remainder(thou,10),{0,2,4,6}) then return false end if
        n = floor(n/1000)
    end while
    return true
end function

sequence s = {}
for i=0 to 1000 do
    if eban(i) then s &= i end if
end for
printf(1,"eban to 1000 : %v (%d items)\n",{s,length(s)})
s = {}
for i=1000 to 4000 do
    if eban(i) then s &= i end if
end for
printf(1,"eban 1000..4000 : %v (%d items)\n\n",{s,length(s)})

atom t0 = time()
for i=0 to 21 do
    printf(1,"count_eban(10^%d) : %,d\n",{i,count_eban(i)})
end for
?elapsed(time()-t0)
```

{{out}}

```txt

eban to 1000 : {2,4,6,30,32,34,36,40,42,44,46,50,52,54,56,60,62,64,66} (19 items)
eban 1000..4000 : {2000,2002,2004,2006,2030,2032,2034,2036,2040,2042,2044,2046,2050,2052,2054,2056,2060,2062,2064,2066,4000} (21 items)

count_eban(10^0) : 0
count_eban(10^1) : 3
count_eban(10^2) : 19
count_eban(10^3) : 19
count_eban(10^4) : 79
count_eban(10^5) : 399
count_eban(10^6) : 399
count_eban(10^7) : 1,599
count_eban(10^8) : 7,999
count_eban(10^9) : 7,999
count_eban(10^10) : 31,999
count_eban(10^11) : 159,999
count_eban(10^12) : 159,999
count_eban(10^13) : 639,999
count_eban(10^14) : 3,199,999
count_eban(10^15) : 3,199,999
count_eban(10^16) : 12,799,999
count_eban(10^17) : 63,999,999
count_eban(10^18) : 63,999,999
count_eban(10^19) : 255,999,999
count_eban(10^20) : 1,279,999,999
count_eban(10^21) : 1,279,999,999
"0.0s"

```


## Python


```Python

# Use inflect

"""

  show all eban numbers <= 1,000 (in a horizontal format), and a count
  show all eban numbers between 1,000 and 4,000 (inclusive), and a count
  show a count of all eban numbers up and including 10,000
  show a count of all eban numbers up and including 100,000
  show a count of all eban numbers up and including 1,000,000
  show a count of all eban numbers up and including 10,000,000

"""

import inflect
import time

before = time.perf_counter()

p = inflect.engine()

# eban numbers <= 1000

print(' ')
print('eban numbers up to and including 1000:')
print(' ')

count = 0

for i in range(1,1001):
    if not 'e' in p.number_to_words(i):
        print(str(i)+' ',end='')
        count += 1

print(' ')
print(' ')
print('count = '+str(count))
print(' ')

# eban numbers 1000 to 4000

print(' ')
print('eban numbers between 1000 and 4000 (inclusive):')
print(' ')

count = 0

for i in range(1000,4001):
    if not 'e' in p.number_to_words(i):
        print(str(i)+' ',end='')
        count += 1

print(' ')
print(' ')
print('count = '+str(count))
print(' ')

# eban numbers up to 10000

print(' ')
print('eban numbers up to and including 10000:')
print(' ')

count = 0

for i in range(1,10001):
    if not 'e' in p.number_to_words(i):
        count += 1

print(' ')
print('count = '+str(count))
print(' ')

# eban numbers up to 100000

print(' ')
print('eban numbers up to and including 100000:')
print(' ')

count = 0

for i in range(1,100001):
    if not 'e' in p.number_to_words(i):
        count += 1

print(' ')
print('count = '+str(count))
print(' ')

# eban numbers up to 1000000

print(' ')
print('eban numbers up to and including 1000000:')
print(' ')

count = 0

for i in range(1,1000001):
    if not 'e' in p.number_to_words(i):
        count += 1

print(' ')
print('count = '+str(count))
print(' ')

# eban numbers up to 10000000

print(' ')
print('eban numbers up to and including 10000000:')
print(' ')

count = 0

for i in range(1,10000001):
    if not 'e' in p.number_to_words(i):
        count += 1

print(' ')
print('count = '+str(count))
print(' ')

after = time.perf_counter()

print(" ")
print("Run time in seconds: "+str(after - before))

```


Output:

```txt


eban numbers up to and including 1000:

2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66

count = 19


eban numbers between 1000 and 4000 (inclusive):

2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000

count = 21


eban numbers up to and including 10000:


count = 79


eban numbers up to and including 100000:


count = 399


eban numbers up to and including 1000000:


count = 399


eban numbers up to and including 10000000:


count = 1599


Run time in seconds: 1134.289519125

```



## REXX

Programming note:   REXX has no shortcuts for   '''if'''   statements, so the multiple   '''if'''   statements weren't combined into one.

```rexx
/*REXX program to display eban numbers (those that don't have an "e" their English name)*/
numeric digits 20                                /*support some gihugic numbers for pgm.*/
parse arg $                                      /*obtain optional arguments from the cL*/
if $=''  then $= '1 1000   1000 4000   1 -10000   1 -100000   1 -1000000   1 -10000000'

      do k=1  by 2  to words($)                  /*step through the list of numbers.    */
      call banE  word($, k),  word($, k+1)       /*process the numbers, from low──►high.*/
      end   /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
banE: procedure; parse arg x,y,_;  z= reverse(x) /*obtain the number to be examined.    */
      tell= y>=0                                 /*Is HI non-negative?  Display eban #s.*/
      #= 0                                       /*the count of  eban  numbers (so far).*/
           do j=x  to abs(y)                     /*probably process a range of numbers. */
           if hasE(j)  then iterate              /*determine if the number has an  "e". */
           #= # + 1                              /*bump the counter of  eban  numbers.  */
           if tell  then _= _  j                 /*maybe add to a list of eban numbers. */
           end   /*j*/
      if _\==''  then say strip(_)               /*display the list  (if there is one). */
      say;     say #   ' eban numbers found for: '   x   " "   y;     say copies('═', 105)
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
hasE: procedure; parse arg x;  z= reverse(x)     /*obtain the number to be examined.    */
        do k=1  by 3                             /*while there're dec. digit to examine.*/
        @= reverse( substr(z, k, 3) )            /*obtain 3 dec. digs (a period) from Z.*/
        if @=='   '           then return 0      /*we have reached the "end" of the num.*/
        uni= right(@, 1)                         /*get units dec. digit of this period. */
        if uni//2==1          then return 1      /*if an odd digit, then not an eban #. */
        if uni==8             then return 1      /*if an  eight,      "   "   "   "  "  */
        tens=substr(@, 2, 1)                     /*get tens  dec. digit of this period. */
        if tens==1            then return 1      /*if teens,        then not an eban #. */
        if tens==2            then return 1      /*if twenties,       "   "   "   "  "  */
        if tens>6             then return 1      /*if 70s, 80s, 90s,  "   "   "   "  "  */
        hun= left(@, 1)                          /*get hundreds dec. dig of this period.*/
        if hun==0             then iterate       /*if zero, then there is more of number*/
        if hun\==' '          then return 1      /*any hundrEd (not zero) has an  "e".  */
        end   /*k*/                              /*A "period" is a group of 3 dec. digs */
     return 0                                    /*in the number, grouped from the right*/
```

{{out|output|text=  when using the default inputs:}}

```txt

2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66

19  eban numbers found for:  1   1000
═════════════════════════════════════════════════════════════════════════════════════════════════════════
2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000

21  eban numbers found for:  1000   4000
═════════════════════════════════════════════════════════════════════════════════════════════════════════

79  eban numbers found for:  1   -10000
═════════════════════════════════════════════════════════════════════════════════════════════════════════

399  eban numbers found for:  1   -100000
═════════════════════════════════════════════════════════════════════════════════════════════════════════

399  eban numbers found for:  1   -1000000
═════════════════════════════════════════════════════════════════════════════════════════════════════════

1599  eban numbers found for:  1   -10000000
═════════════════════════════════════════════════════════════════════════════════════════════════════════

```



## Tailspin


```tailspin

templates isEban
  def number: $;
  '$;' -> (<'([246]|[3456][0246])(0[03456][0246])*'> $ !) -> $number !
end isEban

```


Alternatively, if regex is not your thing, we can do it numerically, which actually runs faster

```tailspin

templates isEban
  def number: $;
  $ -> (<1..> $!) -> #
  <0> $number !
  <?($ mod 1000 <0|2|4|6|30..66?($ mod 10 <0|2|4|6>)>)> $ / 1000 -> #
end isEban

```


Either version is called by the following code

```tailspin

def small: [1..1000 -> isEban];
$small -> !OUT::write
'
There are $small::length; eban numbers up to and including 1000

' -> !OUT::write

def next: [1000..4000 -> isEban];
$next -> !OUT::write
'
There are $next::length; eban numbers between 1000 and 4000 (inclusive)

' -> !OUT::write
'
There are $:[1..10000 -> isEban] -> $::length; eban numbers up to and including 10 000

' -> !OUT::write
'
There are $:[1..100000 -> isEban] -> $::length; eban numbers up to and including 100 000

' -> !OUT::write
'
There are $:[1..1000000 -> isEban] -> $::length; eban numbers up to and including 1 000 000

' -> !OUT::write
'
There are $:[1..10000000 -> isEban] -> $::length; eban numbers up to and including 10 000 000

' -> !OUT::write

```

{{out}}

```txt

[2, 4, 6, 30, 32, 34, 36, 40, 42, 44, 46, 50, 52, 54, 56, 60, 62, 64, 66]
There are 19 eban numbers up to and including 1000

[2000, 2002, 2004, 2006, 2030, 2032, 2034, 2036, 2040, 2042, 2044, 2046, 2050, 2052, 2054, 2056, 2060, 2062, 2064, 2066, 4000]
There are 21 eban numbers between 1000 and 4000 (inclusive)


There are 79 eban numbers up to and including 10 000


There are 399 eban numbers up to and including 100 000


There are 399 eban numbers up to and including 1 000 000


There are 1599 eban numbers up to and including 10 000 000

```



## Visual Basic .NET

{{trans|D}}

```vbnet
Module Module1

    Structure Interval
        Dim start As Integer
        Dim last As Integer
        Dim print As Boolean

        Sub New(s As Integer, l As Integer, p As Boolean)
            start = s
            last = l
            print = p
        End Sub
    End Structure

    Sub Main()
        Dim intervals As Interval() = {
            New Interval(2, 1_000, True),
            New Interval(1_000, 4_000, True),
            New Interval(2, 10_000, False),
            New Interval(2, 100_000, False),
            New Interval(2, 1_000_000, False),
            New Interval(2, 10_000_000, False),
            New Interval(2, 100_000_000, False),
            New Interval(2, 1_000_000_000, False)
        }
        For Each intv In intervals
            If intv.start = 2 Then
                Console.WriteLine("eban numbers up to and including {0}:", intv.last)
            Else
                Console.WriteLine("eban numbers between {0} and {1} (inclusive):", intv.start, intv.last)
            End If

            Dim count = 0
            For i = intv.start To intv.last Step 2
                Dim b = i \ 1_000_000_000
                Dim r = i Mod 1_000_000_000
                Dim m = r \ 1_000_000
                r = i Mod 1_000_000
                Dim t = r \ 1_000
                r = r Mod 1_000
                If m >= 30 AndAlso m <= 66 Then
                    m = m Mod 10
                End If
                If t >= 30 AndAlso t <= 66 Then
                    t = t Mod 10
                End If
                If r >= 30 AndAlso r <= 66 Then
                    r = r Mod 10
                End If
                If b = 0 OrElse b = 2 OrElse b = 4 OrElse b = 6 Then
                    If m = 0 OrElse m = 2 OrElse m = 4 OrElse m = 6 Then
                        If t = 0 OrElse t = 2 OrElse t = 4 OrElse t = 6 Then
                            If r = 0 OrElse r = 2 OrElse r = 4 OrElse r = 6 Then
                                If intv.print Then
                                    Console.Write("{0} ", i)
                                End If
                                count += 1
                            End If
                        End If
                    End If
                End If
            Next
            If intv.print Then
                Console.WriteLine()
            End If
            Console.WriteLine("count = {0}", count)
            Console.WriteLine()
        Next
    End Sub

End Module
```

{{out}}

```txt
eban numbers up to and including 1000:
2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66
count = 19

eban numbers between 1000 and 4000 (inclusive):
2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000
count = 21

eban numbers up to and including 10000:
count = 79

eban numbers up to and including 100000:
count = 399

eban numbers up to and including 1000000:
count = 399

eban numbers up to and including 10000000:
count = 1599

eban numbers up to and including 100000000:
count = 7999

eban numbers up to and including 1000000000:
count = 7999
```



## Yabasic

{{trans|Go}}

```Yabasic
data 2, 100, true
data 1000, 4000, true
data 2, 1e4, false
data 2, 1e5, false
data 2, 1e6, false
data 2, 1e7, false
data 2, 1e8, false
REM data 2, 1e9, false  // it takes a lot of time
data 0, 0, false

do
    read start, ended, printable
    if not start break

    if start = 2 then
        Print "eban numbers up to and including ", ended
    else
        Print "eban numbers between ", start, " and ", ended, " (inclusive):"
    endif
    count = 0
    for i = start to ended step 2
        b = int(i / 1000000000)
        r = mod(i, 1000000000)
        m = int(r / 1000000)
        r = mod(i, 1000000)
        t = int(r / 1000)
        r = mod(r, 1000)
        if m >= 30 and m <= 66 m = mod(m, 10)
        if t >= 30 and t <= 66 t = mod(t, 10)
        if r >= 30 and r <= 66 r = mod(r, 10)
        if b = 0 or b = 2 or b = 4 or b = 6 then
            if m = 0 or m = 2 or m = 4 or m = 6 then
                if t = 0 or t = 2 or t = 4 or t = 6 then
                    if r = 0 or r = 2 or r = 4 or r = 6 then
                        if printable Print i;
                        count = count + 1
                    endif
                endif
            endif
        endif
    next
    if printable Print
    Print "count = ", count, "\n"
loop
```



## zkl

{{trans|Go}}

```zkl
rgs:=T( T(2, 1_000, True),	// (start,end,print)
        T(1_000, 4_000, True),
        T(2, 1e4, False), T(2, 1e5, False), T(2, 1e6, False), T(2, 1e7, False),
        T(2, 1e8, False), T(2, 1e9, False), // slow and very slow
      );

foreach start,end,pr in (rgs){
   if(start==2) println("eban numbers up to and including %,d:".fmt(end));
   else println("eban numbers between %,d and %,d (inclusive):".fmt(start,end));

   count:=0;
   foreach i in ([start..end,2]){
      b,r := i/100_0000_000, i%1_000_000_000;
      m,r := r/1_000_000,    i%1_000_000;
      t,r := r/1_000,	     r%1_000;
      if(30<=m<=66) m=m%10;
      if(30<=t<=66) t=t%10;
      if(30<=r<=66) r=r%10;

      if(magic(b) and magic(m) and magic(t) and magic(r)){
         if(pr) print(i," ");
	 count+=1;
      }
   }
   if(pr) println();
   println("count = %,d\n".fmt(count));
}
fcn magic(z){ z.isEven and z<=6 }
```

{{out}}
<pre style="height:35ex">
eban numbers up to and including 1,000:
2 4 6 30 32 34 36 40 42 44 46 50 52 54 56 60 62 64 66
count = 19

eban numbers between 1,000 and 4,000 (inclusive):
2000 2002 2004 2006 2030 2032 2034 2036 2040 2042 2044 2046 2050 2052 2054 2056 2060 2062 2064 2066 4000
count = 21

eban numbers up to and including 10,000:
count = 79

eban numbers up to and including 100,000:
count = 399

eban numbers up to and including 1,000,000:
count = 399

eban numbers up to and including 10,000,000:
count = 1,599

eban numbers up to and including 100,000,000:
count = 7,999

eban numbers up to and including 1,000,000,000:
count = 7,999

```


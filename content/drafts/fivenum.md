+++
title = "Fivenum"
description = ""
date = 2019-09-12T17:41:04Z
aliases = []
[extra]
id = 21734
[taxonomies]
categories = []
tags = []
+++

'''Many big data or scientific programs use boxplots to show distributions of data.   In addition, sometimes saving large arrays for boxplots can be impractical and use extreme amounts of RAM.   It can be useful to save large arrays as arrays with five numbers to save memory.

For example, the   '''R'''   programming language implements Tukey's [[wp:Five-number summary|five-number summary]] as the '''[https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fivenum.html fivenum]''' function.


;Task:
Given an array of numbers, compute the five-number summary.


;Note:
While these five numbers can be used to draw a [[wp:Box plot|boxplot]], statistical packages will typically need extra data. Moreover, while there is a consensus about the "box" of the boxplot, there are variations among statistical packages for the whiskers.





## C

{{trans|Kotlin}}

```c
#include <stdio.h>
#include <stdlib.h>

double median(double *x, int start, int end_inclusive) {
    int size = end_inclusive - start + 1;
    if (size <= 0) {
        printf("Array slice cannot be empty\n");
        exit(1);
    }
    int m = start + size / 2;
    if (size % 2) return x[m];
    return (x[m - 1] + x[m]) / 2.0;
}

int compare (const void *a, const void *b) {
    double aa = *(double*)a;
    double bb = *(double*)b;
    if (aa > bb) return 1;
    if (aa < bb) return -1;
    return 0;
}

int fivenum(double *x, double *result, int x_len) {
    int i, m, lower_end;
    for (i = 0; i < x_len; i++) {
        if (x[i] != x[i]) {
           printf("Unable to deal with arrays containing NaN\n\n");
           return 1;
        }
    }
    qsort(x, x_len, sizeof(double), compare);
    result[0] = x[0];
    result[2] = median(x, 0, x_len - 1);
    result[4] = x[x_len - 1];
    m = x_len / 2;
    lower_end = (x_len % 2) ? m : m - 1;
    result[1] = median(x, 0, lower_end);
    result[3] = median(x, m, x_len - 1);
    return 0;
}

int show(double *result, int places) {
    int i;
    char f[7];
    sprintf(f, "%%.%dlf", places);
    printf("[");
    for (i = 0; i < 5; i++) {
        printf(f, result[i]);
        if (i < 4) printf(", ");
    }
    printf("]\n\n");
}

int main() {
    double result[5];

    double x1[11] = {15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0};
    if (!fivenum(x1, result, 11)) show(result, 1);

    double x2[6] = {36.0, 40.0, 7.0, 39.0, 41.0, 15.0};
    if (!fivenum(x2, result, 6)) show(result, 1);

    double x3[20] = {
         0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
        -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
        -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
         0.75775634,  0.32566578
    };
    if (!fivenum(x3, result, 20)) show(result, 9);

    return 0;
}
```


{{out}}

```txt

[6.0, 25.5, 40.0, 42.5, 49.0]

[7.0, 15.0, 37.5, 40.0, 41.0]

[-1.950595940, -0.676741205, 0.233247060, 0.746070945, 1.731315070]

```



## C++

{{trans|D}}

```cpp
#include <algorithm>
#include <iostream>
#include <ostream>
#include <vector>

/////////////////////////////////////////////////////////////////////////////
// The following is taken from https://cpplove.blogspot.com/2012/07/printing-tuples.html

// Define a type which holds an unsigned integer value
template<std::size_t> struct int_ {};

template <class Tuple, size_t Pos>
std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<Pos>) {
    out << std::get< std::tuple_size<Tuple>::value - Pos >(t) << ", ";
    return print_tuple(out, t, int_<Pos - 1>());
}

template <class Tuple>
std::ostream& print_tuple(std::ostream& out, const Tuple& t, int_<1>) {
    return out << std::get<std::tuple_size<Tuple>::value - 1>(t);
}

template <class... Args>
std::ostream& operator<<(std::ostream& out, const std::tuple<Args...>& t) {
    out << '(';
    print_tuple(out, t, int_<sizeof...(Args)>());
    return out << ')';
}

/////////////////////////////////////////////////////////////////////////////

template <class RI>
double median(RI beg, RI end) {
    if (beg == end) throw std::runtime_error("Range cannot be empty");
    auto len = end - beg;
    auto m = len / 2;
    if (len % 2 == 1) {
        return *(beg + m);
    }

    return (beg[m - 1] + beg[m]) / 2.0;
}

template <class C>
auto fivenum(C& c) {
    std::sort(c.begin(), c.end());

    auto cbeg = c.cbegin();
    auto cend = c.cend();

    auto len = cend - cbeg;
    auto m = len / 2;
    auto lower = (len % 2 == 1) ? m : m - 1;
    double r2 = median(cbeg, cbeg + lower + 1);
    double r3 = median(cbeg, cend);
    double r4 = median(cbeg + lower + 1, cend);

    return std::make_tuple(*cbeg, r2, r3, r4, *(cend - 1));
}

int main() {
    using namespace std;
    vector<vector<double>> cs = {
        { 15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0 },
        { 36.0, 40.0, 7.0, 39.0, 41.0, 15.0 },
        {
            0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
           -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
           -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
            0.75775634,  0.32566578
        }
    };

    for (auto & c : cs) {
        cout << fivenum(c) << endl;
    }

    return 0;
}
```

{{out}}

```txt
(6, 25.5, 40, 43, 49)
(7, 15, 37.5, 40, 41)
(-1.9506, -0.676741, 0.233247, 0.746071, 1.73132)
```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Fivenum {
    public static class Helper {
        public static string AsString<T>(this ICollection<T> c, string format = "{0}") {
            StringBuilder sb = new StringBuilder("[");
            int count = 0;
            foreach (var t in c) {
                if (count++ > 0) {
                    sb.Append(", ");
                }
                sb.AppendFormat(format, t);
            }
            return sb.Append("]").ToString();
        }
    }

    class Program {
        static double Median(double[] x, int start, int endInclusive) {
            int size = endInclusive - start + 1;
            if (size <= 0) throw new ArgumentException("Array slice cannot be empty");
            int m = start + size / 2;
            return (size % 2 == 1) ? x[m] : (x[m - 1] + x[m]) / 2.0;
        }

        static double[] Fivenum(double[] x) {
            foreach (var d in x) {
                if (Double.IsNaN(d)) {
                    throw new ArgumentException("Unable to deal with arrays containing NaN");
                }
            }
            double[] result = new double[5];
            Array.Sort(x);
            result[0] = x.First();
            result[2] = Median(x, 0, x.Length - 1);
            result[4] = x.Last();
            int m = x.Length / 2;
            int lowerEnd = (x.Length % 2 == 1) ? m : m - 1;
            result[1] = Median(x, 0, lowerEnd);
            result[3] = Median(x, m, x.Length - 1);
            return result;
        }

        static void Main(string[] args) {
            double[][] x1 = new double[][]{
                new double[]{ 15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0},
                new double[]{ 36.0, 40.0, 7.0, 39.0, 41.0, 15.0},
                new double[]{
                     0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
                    -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
                    -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
                     0.75775634,  0.32566578
                },
            };
            foreach(var x in x1) {
                var result = Fivenum(x);
                Console.WriteLine(result.AsString("{0:F8}"));
            }
        }
    }
}
```

{{out}}

```txt
[6.00000000, 25.50000000, 40.00000000, 42.50000000, 49.00000000]
[7.00000000, 15.00000000, 37.50000000, 40.00000000, 41.00000000]
[-1.95059594, -0.67674121, 0.23324706, 0.74607095, 1.73131507]
```



## D

{{trans|Java}}

```d
import std.algorithm;
import std.exception;
import std.math;
import std.stdio;

double median(double[] x) {
    enforce(x.length >= 0, "Array slice cannot be empty");
    int m = x.length / 2;
    if (x.length % 2 == 1) {
        return x[m];
    }
    return (x[m-1] + x[m]) / 2.0;
}

double[] fivenum(double[] x) {
    foreach (d; x) {
        enforce(!d.isNaN, "Unable to deal with arrays containing NaN");
    }

    double[] result;
    result.length = 5;

    x.sort;
    result[0] = x[0];
    result[2] = median(x);
    result[4] = x[$-1];

    int m = x.length / 2;
    int lower = (x.length % 2 == 1) ? m : m - 1;
    result[1] = median(x[0..lower+1]);
    result[3] = median(x[lower+1..$]);

    return result;
}

void main() {
    double[][] x1 = [
        [15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0],
        [36.0, 40.0, 7.0, 39.0, 41.0, 15.0],
        [
            0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
           -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
           -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
            0.75775634,  0.32566578
        ]
    ];
    foreach(x; x1) {
        writeln(fivenum(x));
    }
}
```

{{out}}

```txt
[6, 25.5, 40, 43, 49]
[7, 15, 37.5, 40, 41]
[-1.9506, -0.676741, 0.233247, 0.746071, 1.73132]
```


=={{header|F#|F sharp}}==
{{trans|C#}}

```fsharp
open System

// Take from https://stackoverflow.com/a/1175123
let rec last = function
    | hd :: [] -> hd
    | _ :: tl -> last tl
    | _ -> failwith "Empty list."

let median x =
    for e in x do
        if Double.IsNaN(e) then failwith "unable to deal with lists containing NaN"

    let size = List.length(x)
    if size <= 0 then failwith "Array slice cannot be empty"
    let m = size / 2
    if size % 2 = 1 then x.[m]
    else (x.[m - 1] + x.[m]) / 2.0

let fivenum x =
    let x2 = List.sort(x)
    let m = List.length(x2) / 2
    let lowerEnd = if List.length(x2) % 2 = 1 then m else m - 1
    [List.head x2, median x2.[..lowerEnd], median x2, median x2.[m..], last x2]

[<EntryPoint>]
let main _ =
    let x1 = [
        [15.0; 6.0; 42.0; 41.0; 7.0; 36.0; 49.0; 40.0; 39.0; 47.0; 43.0];
        [36.0; 40.0; 7.0; 39.0; 41.0; 15.0];
        [
             0.14082834;  0.09748790;  1.73131507;  0.87636009; -1.95059594;
             0.73438555; -0.03035726;  1.46675970; -0.74621349; -0.72588772;
             0.63905160;  0.61501527; -0.98983780; -1.00447874; -0.62759469;
             0.66206163;  1.04312009; -0.10305385;  0.75775634;  0.32566578
        ]
    ]

    for a in x1 do
        let y = fivenum a
        Console.WriteLine("{0}", y);

    0 // return an integer exit code
```

{{out}}

```txt
[(6, 25.5, 40, 42.5, 49)]
[(7, 15, 37.5, 40, 41)]
[(-1.95059594, -0.676741205, 0.23324706, 0.746070945, 1.73131507)]
```



## Factor


```factor
USING: combinators combinators.smart kernel math
math.statistics prettyprint sequences sorting ;
IN: rosetta-code.five-number

<PRIVATE

: bisect ( seq -- lower upper )
    dup length even? [ halves ]
    [ dup midpoint@ 1 + [ head ] [ tail* ] 2bi ] if ;

: (fivenum) ( seq -- summary )
    natural-sort {
        [ infimum ]
        [ bisect drop median ]
        [ median ]
        [ bisect nip median ]
        [ supremum ]
    } cleave>array ;

PRIVATE>

ERROR: fivenum-empty data ;
ERROR: fivenum-nan data ;

: fivenum ( seq -- summary )
    {
        { [ dup empty? ] [ fivenum-empty ] }
        { [ dup [ fp-nan? ] any? ] [ fivenum-nan ] }
        [ (fivenum) ]
    } cond ;

: fivenum-demo ( -- )
    { 15 6 42 41 7 36 49 40 39 47 43 }
    { 36 40 7 39 41 15 }
    {  0.14082834  0.09748790  1.73131507  0.87636009
      -1.95059594  0.73438555 -0.03035726  1.46675970
      -0.74621349 -0.72588772  0.63905160  0.61501527
      -0.98983780 -1.00447874 -0.62759469  0.66206163
       1.04312009 -0.10305385  0.75775634  0.32566578 }
    [ fivenum . ] tri@ ;

MAIN: fivenum-demo
```

{{out}}

```txt

{ 6 25+1/2 40 42+1/2 49 }
{ 7 15 37+1/2 40 41 }
{ -1.95059594 -0.676741205 0.23324706 0.746070945 1.73131507 }

```



## Go

{{trans|Perl}}

```go
package main

import (
    "fmt"
    "math"
    "sort"
)

func fivenum(a []float64) (n5 [5]float64) {
    sort.Float64s(a)
    n := float64(len(a))
    n4 := float64((len(a)+3)/2) / 2
    d := []float64{1, n4, (n + 1) / 2, n + 1 - n4, n}
    for e, de := range d {
        floor := int(de - 1)
        ceil := int(math.Ceil(de - 1))
        n5[e] = .5 * (a[floor] + a[ceil])
    }
    return
}

var (
    x1 = []float64{36, 40, 7, 39, 41, 15}
    x2 = []float64{15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43}
    x3 = []float64{
        0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594,
        0.73438555, -0.03035726, 1.46675970, -0.74621349, -0.72588772,
        0.63905160, 0.61501527, -0.98983780, -1.00447874, -0.62759469,
        0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578,
    }
)

func main() {
    fmt.Println(fivenum(x1))
    fmt.Println(fivenum(x2))
    fmt.Println(fivenum(x3))
}
```

{{out}}

```txt

[7 15 37.5 40 41]
[6 25.5 40 42.5 49]
[-1.95059594 -0.676741205 0.23324706 0.746070945 1.73131507]

```


'''Alternate:'''

This solution is aimed at handling larger data sets more efficiently.  It replaces the O(n log n) sort with O(n) quickselect.  It also does not attempt to reproduce the R result exactly, to average values to get a median of an even number of data values, or otherwise estimate quantiles.  The quickselect here leaves the input partitioned around the selected value, which allows another small optimization:  The first quickselect call partitions the full input around the median.  The second call, to get the first quartile, thus only has to process the partition up to the median.  The third call, to get the minimum, only has to process the partition up to the first quartile.  The 3rd quartile and maximum are obtained similarly.

```go
package main

import (
    "fmt"
    "math/rand"
)

func fivenum(a []float64) (n [5]float64) {
    last := len(a) - 1
    m := last / 2
    n[2] = qsel(a, m)
    q1 := len(a) / 4
    n[1] = qsel(a[:m], q1)
    n[0] = qsel(a[:q1], 0)
    a = a[m:]
    q3 := last - m - q1
    n[3] = qsel(a, q3)
    a = a[q3:]
    n[4] = qsel(a, len(a)-1)
    return
}

func qsel(a []float64, k int) float64 {
    for len(a) > 1 {
        px := rand.Intn(len(a))
        pv := a[px]
        last := len(a) - 1
        a[px], a[last] = a[last], pv
        px = 0
        for i, v := range a[:last] {
            if v < pv {
                a[px], a[i] = v, a[px]
                px++
            }
        }
        a[px], a[last] = pv, a[px]
        if px == k {
            return pv
        }
        if k < px {
            a = a[:px]
        } else {
            a = a[px+1:]
            k -= px + 1
        }
    }
    return a[0]
}

var (
    x1 = []float64{36, 40, 7, 39, 41, 15}
    x2 = []float64{15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43}
    x3 = []float64{
        0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594,
        0.73438555, -0.03035726, 1.46675970, -0.74621349, -0.72588772,
        0.63905160, 0.61501527, -0.98983780, -1.00447874, -0.62759469,
        0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578,
    }
)

func main() {
    fmt.Println(fivenum(x1))
    fmt.Println(fivenum(x2))
    fmt.Println(fivenum(x3))
}
```

{{out}}

```txt

[7 15 36 40 41]
[6 15 40 43 49]
[-1.95059594 -0.62759469 0.14082834 0.73438555 1.73131507]

```



## J

'''Solution'''

```j
midpts=: (1 + #) <:@(] , -:@[ , -) -:@<.@-:@(3 + #) NB. mid points of y
quartiles=: -:@(+/)@((<. ,: >.)@midpts { /:~@])  NB. quartiles of y
fivenum=: <./ , quartiles , >./                  NB. fivenum summary of y
```

'''Example Usage'''

```j
   test1=: 15 6 42 41 7 36 49 40 39 47 43
   test2=: 36 40 7 39 41 15
   test3=: , 0 ". ];._2 noun define
 0.14082834  0.09748790  1.73131507  0.87636009 -1.95059594
 0.73438555 -0.03035726  1.46675970 -0.74621349 -0.72588772
 0.63905160  0.61501527 -0.98983780 -1.00447874 -0.62759469
 0.66206163  1.04312009 -0.10305385  0.75775634  0.32566578
)
   fivenum &> test1;test2;test3
      6      25.5       40     42.5      49
      7        15     37.5       40      41
_1.9506 _0.676741 0.233247 0.746071 1.73132
```


## Java

{{trans|Kotlin}}

```java
import java.util.Arrays;

public class Fivenum {

    static double median(double[] x, int start, int endInclusive) {
        int size = endInclusive - start + 1;
        if (size <= 0) throw new IllegalArgumentException("Array slice cannot be empty");
        int m = start + size / 2;
        return (size % 2 == 1) ? x[m] : (x[m - 1] + x[m]) / 2.0;
    }

    static double[] fivenum(double[] x) {
        for (Double d : x) {
            if (d.isNaN())
                throw new IllegalArgumentException("Unable to deal with arrays containing NaN");
        }
        double[] result = new double[5];
        Arrays.sort(x);
        result[0] = x[0];
        result[2] = median(x, 0, x.length - 1);
        result[4] = x[x.length - 1];
        int m = x.length / 2;
        int lowerEnd = (x.length % 2 == 1) ? m : m - 1;
        result[1] = median(x, 0, lowerEnd);
        result[3] = median(x, m, x.length - 1);
        return result;
    }

    public static void main(String[] args) {
        double xl[][] = {
            {15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0},
            {36.0, 40.0, 7.0, 39.0, 41.0, 15.0},
            {
                 0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
                -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
                -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
                 0.75775634,  0.32566578
            }
        };
        for (double[] x : xl) System.out.printf("%s\n\n", Arrays.toString(fivenum(x)));
    }
}
```


{{out}}

```txt

[6.0, 25.5, 40.0, 42.5, 49.0]

[7.0, 15.0, 37.5, 40.0, 41.0]

[-1.95059594, -0.676741205, 0.23324706, 0.746070945, 1.73131507]

```



## Julia

{{works with|Julia|0.6}}


```julia
function mediansorted(x::AbstractVector{T}, i::Integer, l::Integer)::T where T
    len = l - i + 1
    len > zero(len) || throw(ArgumentError("Array slice cannot be empty."))
    mid = i + len ÷ 2
    return isodd(len) ? x[mid] : (x[mid-1] + x[mid]) / 2
end

function fivenum(x::AbstractVector{T}) where T<:AbstractFloat
    r = Vector{T}(5)
    xs = sort(x)
    mid::Int = length(xs) ÷ 2
    lowerend::Int = isodd(length(xs)) ? mid : mid - 1
    r[1] = xs[1]
    r[2] = mediansorted(xs, 1, lowerend)
    r[3] = mediansorted(xs, 1, endof(xs))
    r[4] = mediansorted(xs, mid, endof(xs))
    r[end] = xs[end]
    return r
end

for v in ([15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0],
          [36.0, 40.0, 7.0, 39.0, 41.0, 15.0],
          [0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
          -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
          -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
           0.75775634,  0.32566578])
    println("# ", v, "\n -> ", fivenum(v))
end
```


{{out}}

```txt
# [15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0]
 -> [6.0, 15.0, 40.0, 42.0, 49.0]
# [36.0, 40.0, 7.0, 39.0, 41.0, 15.0]
 -> [7.0, 11.0, 37.5, 39.5, 41.0]
# [0.140828, 0.0974879, 1.73132, 0.87636, -1.9506, 0.734386, -0.0303573, 1.46676, -0.746213, -0.725888, 0.639052, 0.615015, -0.989838, -1.00448, -0.627595,0.662062, 1.04312, -0.103054, 0.757756, 0.325666]
 -> [-1.9506, -0.725888, 0.233247, 0.734386, 1.73132]
```



## Kotlin

The following uses Tukey's method for calculating the lower and upper quartiles (or 'hinges') which is what the R function, fivenum, appears to use.

As arrays containing NaNs and nulls cannot really be dealt with in a sensible fashion in Kotlin, they've been excluded altogether.

```scala
// version 1.2.21

fun median(x: DoubleArray, start: Int, endInclusive: Int): Double {
    val size = endInclusive - start + 1
    require (size > 0) { "Array slice cannot be empty" }
    val m = start + size / 2
    return if (size % 2 == 1) x[m] else (x[m - 1] + x[m]) / 2.0
}

fun fivenum(x: DoubleArray): DoubleArray {
    require(x.none { it.isNaN() }) { "Unable to deal with arrays containing NaN" }
    val result = DoubleArray(5)
    x.sort()
    result[0] = x[0]
    result[2] = median(x, 0, x.size - 1)
    result[4] = x[x.lastIndex]
    val m = x.size / 2
    var lowerEnd = if (x.size % 2 == 1) m else m - 1
    result[1] = median(x, 0, lowerEnd)
    result[3] = median(x, m, x.size - 1)
    return result
}

fun main(args: Array<String>) {
    var xl = listOf(
        doubleArrayOf(15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0),
        doubleArrayOf(36.0, 40.0, 7.0, 39.0, 41.0, 15.0),
        doubleArrayOf(
             0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
            -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
            -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
             0.75775634,  0.32566578
        )
    )
    xl.forEach { println("${fivenum(it).asList()}\n") }
}
```


{{out}}

```txt

[6.0, 25.5, 40.0, 42.5, 49.0]

[7.0, 15.0, 37.5, 40.0, 41.0]

[-1.95059594, -0.676741205, 0.23324706, 0.746070945, 1.73131507]

```



## Lua


```lua
function slice(tbl, low, high)
    local copy = {}

    for i=low or 1, high or #tbl do
        copy[#copy+1] = tbl[i]
    end

    return copy
end

-- assumes that tbl is sorted
function median(tbl)
    m = math.floor(#tbl / 2) + 1
    if #tbl % 2 == 1 then
        return tbl[m]
    end
    return (tbl[m-1] + tbl[m]) / 2
end

function fivenum(tbl)
    table.sort(tbl)

    r0 = tbl[1]
    r2 = median(tbl)
    r4 = tbl[#tbl]

    m = math.floor(#tbl / 2)
    if #tbl % 2 == 1 then
        low = m
    else
        low = m - 1
    end
    r1 = median(slice(tbl, nil, low+1))
    r3 = median(slice(tbl, low+2, nil))

    return r0, r1, r2, r3, r4
end

x1 = {
    {15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0},
    {36.0, 40.0, 7.0, 39.0, 41.0, 15.0},
    {
        0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
       -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
       -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
        0.75775634,  0.32566578
    }
}

for i,x in ipairs(x1) do
    print(fivenum(x))
end
```

{{out}}

```txt
6       25.5    40      43      49
7       15      37.5    40      41
-1.95059594     -0.676741205    0.23324706      0.746070945     1.73131507
```


=={{header|Modula-2}}==

```modula2
MODULE Fivenum;
FROM FormatString IMPORT FormatString;
FROM LongStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteLongReal(v : LONGREAL);
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    RealToStr(v, buf);
    WriteString(buf)
END WriteLongReal;

PROCEDURE WriteArray(arr : ARRAY OF LONGREAL);
VAR i : CARDINAL;
BEGIN
    WriteString("[");
    FOR i:=0 TO HIGH(arr) DO
        WriteLongReal(arr[i]);
        WriteString(", ")
    END;
    WriteString("]")
END WriteArray;

(* Assumes that the input is sorted *)
PROCEDURE Median(x : ARRAY OF LONGREAL; beg,end : CARDINAL) : LONGREAL;
VAR m,cnt : CARDINAL;
BEGIN
    cnt := end - beg + 1;
    m := cnt / 2;
    IF cnt MOD 2 = 1 THEN
        RETURN x[beg + m]
    END;
    RETURN (x[beg + m - 1] + x[beg + m]) / 2.0
END Median;

TYPE Summary = ARRAY[0..4] OF LONGREAL;
PROCEDURE Fivenum(input : ARRAY OF LONGREAL) : Summary;
    PROCEDURE Sort();
    VAR
        i,j : CARDINAL;
        t : LONGREAL;
    BEGIN
        FOR i:=0 TO HIGH(input) DO
            FOR j:=0 TO HIGH(input) DO
                IF (i#j) AND (input[i] < input[j]) THEN
                    t := input[i];
                    input[i] := input[j];
                    input[j] := t
                END
            END
        END
    END Sort;
VAR
    result : Summary;
    size,m,low : CARDINAL;
BEGIN
    size := HIGH(input);
    Sort();

    result[0] := input[0];
    result[2] := Median(input,0,size);
    result[4] := input[size];

    m := size / 2;
    IF (size MOD 2 = 1) THEN
        low := m
    ELSE
        low := m - 1
    END;
    result[1] := Median(input, 0, m);
    result[3] := Median(input, m+1, size);

    RETURN result;
END Fivenum;

TYPE
    A6 = ARRAY[0..5] OF LONGREAL;
    A11 = ARRAY[0..10] OF LONGREAL;
    A20 = ARRAY[0..19] OF LONGREAL;
VAR
    a6 : A6;
    a11 : A11;
    a20 : A20;
BEGIN
    a11 := A11{15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0};
    WriteArray(Fivenum(a11));
    WriteLn;
    WriteLn;

    a6 := A6{36.0, 40.0, 7.0, 39.0, 41.0, 15.0};
    WriteArray(Fivenum(a6));
    WriteLn;
    WriteLn;

    a20 := A20{
        0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
        -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
        -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
        0.75775634,  0.32566578
    };
    WriteArray(Fivenum(a20));
    WriteLn;

    ReadChar
END Fivenum.
```

{{out}}

```txt
[6.000000000000000, 25.499999999999900, 40.000000000000000, 42.499999999999900, 49.000000000000000, ]

[7.000000000000000, 15.000000000000000, 35.500000000000000, 40.000000000000000, 40.499999999999900, ]

[-1.950594000000000, -0.676741205000000, 0.233247060000000, 0.746070945000000, 1.731315070000000, ]
```



## Perl


```Perl
use POSIX qw(ceil floor);

sub fivenum {
   my(@array) = @_;
   my $n = scalar @array;
   die "No values were entered into fivenum!" if $n == 0;
   my @x = sort {$a <=> $b} @array;
   my $n4 = floor(($n+3)/2)/2;
   my @d = (1, $n4, ($n +1)/2, $n+1-$n4, $n);
   my @sum_array;
   for my $e (0..4) {
      my $floor = floor($d[$e]-1);
      my $ceil  =  ceil($d[$e]-1);
      push @sum_array, (0.5 * ($x[$floor] + $x[$ceil]));
   }
   return @sum_array;
}

my @x = (15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43);
my @tukey = fivenum(\@x);
say join (',', @tukey);
#----------
@x = (36, 40, 7, 39, 41, 15),
@tukey = fivenum(\@x);
say join (',', @tukey);
#----------
@x = (0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,
     0.73438555, -0.03035726,  1.46675970, -0.74621349, -0.72588772,
     0.63905160,  0.61501527, -0.98983780, -1.00447874, -0.62759469,
     0.66206163,  1.04312009, -0.10305385,  0.75775634,  0.32566578);
@tukey = fivenum(\@x);
say join (',', @tukey);
```


{{out}}

```txt
6,25.5,40,42.5,49
7,15,37.5,40,41
-1.95059594,-0.676741205,0.23324706,0.746070945,1.73131507
```



## Perl 6

{{trans|Perl}}

```perl6
sub fourths ( Int $end ) {
    my $end_22 = $end div 2 / 2;

    return 0, $end_22, $end/2, $end - $end_22, $end;
}
sub fivenum ( @nums ) {
    my @x = @nums.sort(+*)
        or die 'Input must have at least one element';

    my @d = fourths(@x.end);

    return ( @x[@d».floor] Z+ @x[@d».ceiling] ) »/» 2;
}

say .&fivenum for [15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43],
                  [36, 40, 7, 39, 41, 15], [
    0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,
    0.73438555, -0.03035726,  1.46675970, -0.74621349, -0.72588772,
    0.63905160,  0.61501527, -0.98983780, -1.00447874, -0.62759469,
    0.66206163,  1.04312009, -0.10305385,  0.75775634,  0.32566578,
];

```

{{out}}

```txt
(6 25.5 40 42.5 49)
(7 15 37.5 40 41)
(-1.95059594 -0.676741205 0.23324706 0.746070945 1.73131507)
```



## Phix


```Phix
function median(sequence tbl, integer lo, hi)
    integer l = hi-lo+1
    integer m = lo+floor(l/2)
    if remainder(l,2)=1 then
        return tbl[m]
    end if
    return (tbl[m-1]+tbl[m])/2
end function

function fivenum(sequence tbl)
    tbl = sort(tbl)
    integer l = length(tbl),
            m = floor(l/2)+remainder(l,2)

    atom r1 = tbl[1],
         r2 = median(tbl,1,m),
         r3 = median(tbl,1,l),
         r4 = median(tbl,m+1,l),
         r5 = tbl[l]

    return {r1, r2, r3, r4, r5}
end function

constant x1 = {15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43},
         x2 = {36, 40, 7, 39, 41, 15},
         x3 = {0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594,
               0.73438555, -0.03035726, 1.46675970, -0.74621349, -0.72588772,
               0.63905160, 0.61501527, -0.98983780, -1.00447874, -0.62759469,
               0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578}
?fivenum(x1)
?fivenum(x2)
?fivenum(x3)
```

{{out}}

```txt

{6,25.5,40,43,49}
{7,15,37.5,40,41}
{-1.95059594,-0.676741205,0.23324706,0.746070945,1.73131507}

```



## Python


### Python: Standard commands

{{trans|Perl}}
'''Work with: Python 2'''

'''Work with: Python 3'''

```python
from __future__ import division
import math
import sys

def fivenum(array):
    n = len(array)
    if n == 0:
        print("you entered an empty array.")
        sys.exit()
    x = sorted(array)

    n4 = math.floor((n+3.0)/2.0)/2.0
    d = [1, n4, (n+1)/2, n+1-n4, n]
    sum_array = []

    for e in range(5):
        floor = int(math.floor(d[e] - 1))
        ceil = int(math.ceil(d[e] - 1))
        sum_array.append(0.5 * (x[floor] + x[ceil]))

    return sum_array

x = [0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.46675970,
-0.74621349, -0.72588772, 0.63905160, 0.61501527, -0.98983780, -1.00447874, -0.62759469, 0.66206163,
1.04312009, -0.10305385, 0.75775634, 0.32566578]

y = fivenum(x)
print(y)
```


{{out}}

```txt
[-1.95059594, -0.676741205, 0.23324706, 0.746070945, 1.73131507]
```



### Python: Pandas library

There are many ways to compute this kind of summary statistics (see [[wp:Percentile#Definitions]]). The Python Pandas library supports a range.

[https://pandas.pydata.org/ Pandas] is a well known Python library. Its [https://pandas.pydata.org/pandas-docs/version/0.17.0/generated/pandas.DataFrame.describe.html Dataframe.describe] method produces summary stats from data.

(Though these 25% and 75% values do '''not''' correspond to the Fivenum Tukey quartile values specified in this task)

```python
import pandas as pd
pd.DataFrame([1, 2, 3, 4, 5, 6]).describe()
```


{{out}}

```txt
              0
count  6.000000
mean   3.500000
std    1.870829
min    1.000000
25%    2.250000
50%    3.500000
75%    4.750000
max    6.000000
```


To get the fivenum values asked for, the [https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.quantile.html pandas.DataFrame.quantile] function can be used:

```python
import pandas as pd
pd.DataFrame([1, 2, 3, 4, 5, 6]).quantile([.0, .25, .50, .75, 1.00], interpolation='nearest')
```


{{out}}

```txt
      0
0.00  1
0.25  2
0.50  3
0.75  5
1.00  6
```


The interpolation value supports more of the differing ways of calculation in use.

===Python: Functional – without imports===
'''Works with: Python 3'''

```python
# fiveNums :: [Float] -> (Float, Float, Float, Float, Float)
def fiveNums(xs):
    def median(xs):
        lng = len(xs)
        m = lng // 2
        return xs[m] if (
            0 != lng % 2
        ) else (xs[m - 1] + xs[m]) / 2

    ys = sorted(xs)
    lng = len(ys)
    m = lng // 2
    return (
        ys[0],
        median(ys[0:(m + (lng % 2))]),
        median(ys),
        median(ys[m:]),
        ys[-1]
    ) if 0 < lng else None


# TEST --------------------------------------------------------------------
for xs in [[15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43],
           [36, 40, 7, 39, 41, 15],
           [
               0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594,
               0.73438555, -0.03035726, 1.46675970, -0.74621349, -0.72588772,
               0.63905160, 0.61501527, -0.98983780, -1.00447874, -0.62759469,
               0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578
           ]]:
    print(
        fiveNums(xs)
    )
```

{{Out}}

```txt
(6, 25.5, 40, 42.5, 49)
(7, 15, 37.5, 40, 41)
(-1.95059594, -0.676741205, 0.23324706, 0.746070945, 1.73131507)
```



## R

The '''fivenum''' function is built-in, see [https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fivenum.html R manual].


```R
x <- c(0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594,  0.73438555,-0.03035726, 1.46675970, -0.74621349, -0.72588772, 0.63905160, 0.61501527, -0.98983780, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634,  0.32566578)

fivenum(x)
```


'''Output'''


```txt
[1] -1.9505959 -0.6767412  0.2332471  0.7460709  1.7313151
```



## Racket


Racket's =quantile= functions use a different method to Tukey; so a new implementation was made.


```racket
#lang racket/base
(require math/private/statistics/quickselect)

;; racket's quantile uses "Method 1" of https://en.wikipedia.org/wiki/Quartile
;; Tukey (fivenum) uses "Method 2", so we will need a specialist median
(define (fivenum! data-v)
  (define (tukey-median start end)
    (define-values (n/2 parity) (quotient/remainder (- end start) 2))
    (define mid (+ start n/2))
    (if (zero? parity)
        (/ (+ (data-kth-value! (+ mid (sub1 parity))) (data-kth-value! mid)) 2)
        (data-kth-value! mid)))

  (define n-data (let ((l (vector-length data-v)))
                   (if (zero? l)
                       (raise-argument-error 'data-v "nonempty (Vectorof Real)" data-v)
                       l)))

  (define (data-kth-value! n) (kth-value! data-v n <))

  (define subset-size (let-values (((n/2 parity) (quotient/remainder n-data 2))) (+ n/2 parity)))

  (vector (data-kth-value! 0)
          (tukey-median 0 subset-size)
          (tukey-median 0 n-data)
          (tukey-median (- n-data subset-size) n-data)
          (data-kth-value! (sub1 n-data))))

(define (fivenum data-seq)
  (fivenum! (if (and (vector? data-seq) (not (immutable? data-seq)))
                data-seq
                (for/vector ((datum data-seq)) datum))))

(module+ test
  (require rackunit
           racket/vector)
  (check-equal? #(14 14 14 14 14) (fivenum #(14)) "Minimal case")
  (check-equal? #(8 11 14 17 20) (fivenum #(8 14 20)) "3-value case")
  (check-equal? #(8 11 15 18 20) (fivenum #(8 14 16 20)) "4-value case")

  (define x1-seq #(36 40 7 39 41 15))
  (define x1-v (vector-copy x1-seq))
  (check-equal? x1-seq x1-v "before fivenum! sequence and vector were not `equal?`")
  (check-equal? #(7 15 #e37.5 40 41) (fivenum! x1-v) "Test against Go results x1")
  (check-not-equal? x1-seq x1-v "fivenum! did not mutate mutable input vectors")

  (check-equal? #(6 #e25.5 40 #e42.5 49) (fivenum #(15 6 42 41 7 36 49 40 39 47 43)) "Test against Go results x2")

  (check-equal? #(-1.95059594 -0.676741205 0.23324706 0.746070945 1.73131507)
                (fivenum (vector 0.14082834  0.09748790  1.73131507  0.87636009 -1.95059594  0.73438555
                                 -0.03035726  1.46675970 -0.74621349 -0.72588772  0.63905160  0.61501527
                                 -0.98983780 -1.00447874 -0.62759469  0.66206163  1.04312009 -0.10305385
                                 0.75775634  0.32566578))
                "Test against Go results x3"))
```


This program passes its tests silently.


## REXX

Programming note:   this REXX program uses a unity─based array.

```rexx
/*REXX program computes the five─number summary  (LO─value, p25, medium, p75, HI─value).*/
parse arg x
if x=''  then x= 15 6 42 41 7 36 49 40 39 47 43  /*Not specified?  Then use the defaults*/
say 'input numbers: '     space(x)               /*display the original list of numbers.*/
call 5num                                        /*invoke the  five─number  function.   */
say ' five─numbers: '     result                 /*display "     "     "    results.    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bSort: procedure expose @.;  parse arg n;  m=n-1 /*N:  the number of  @  array elements.*/
         do m=m  for m  by -1  until ok;   ok= 1 /*keep sorting the  @  array 'til done.*/
           do j=1  for m;   k= j + 1;      if @.j<=@.k  then iterate  /*In order?  Good.*/
           parse value @.j @.k 0  with  @.k @.j ok    /*swap 2 elements;  flag as ¬done.*/
           end   /*j*/
         end     /*m*/;          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
med:   arg s,e;  $=e-s+1;  m=s+$%2;  if $//2  then return @.m;  _=m-1;  return (@._+@.m)/2
/*──────────────────────────────────────────────────────────────────────────────────────*/
5num:  #= words(x);                  if #==0  then return  '***error***  array is empty.'
       parse var x . 1 LO . 1 HI .               /*assume values for LO and HI (for now)*/
       q2= # % 2;                                er= '***error***  element'
                     do j=1  for #;     @.j= word(x, j)
                     if \datatype(@.j, 'N')  then return  er   j   "isn't numeric: "   @.j
                     LO= min(LO, @.j);  HI= max(HI, @.j)
                     end   /*j*/                /* [↑]  traipse thru array, find min,max*/
       call bSort #                             /*use a bubble sort  (easiest to code). */
       if #//2  then p25= q2                    /*calculate the second quartile number. */
                else p25= q2 - 1                /*    "      "     "       "       "    */
       return LO  med(1, p25)   med(1, #)   med(q2, #)   HI  /*return list of 5 numbers.*/
```

{{out|output|text=  when using the default input of:     <tt> 15 6 42 41 7 36 49 40 39 47 43 </tt>}}

```txt

input numbers:  15 6 42 41 7 36 49 40 39 47 43
 five─numbers:  6 15 40 42 49

```

{{out|output|text=  when using the (internal) default inputs of:     <tt> 36 40 7 39 41 15 </tt>}}

```txt

input numbers:  36 40 7 39 41 15
 five─numbers:  7 11 37.5 39.5 41

```


## Ruby

{{trans|Perl}}

```ruby
def fivenum(array)
  sorted_arr = array.sort
  n = array.size
  n4 = (((n + 3).to_f / 2.to_f) / 2.to_f).floor
  d = Array.[](1, n4, ((n.to_f + 1) / 2).to_i, n + 1 - n4, n)
  sum_array = []
  (0..4).each do |e| # each loops have local scope, for loops don't
    index_floor = (d[e] - 1).floor
    index_ceil  = (d[e] - 1).ceil
    sum_array.push(0.5 * (sorted_arr[index_floor] + sorted_arr[index_ceil]))
  end
  sum_array
end

test_array = [15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43]
tukey_array = fivenum(test_array)
p tukey_array
test_array = [36, 40, 7, 39, 41, 15]
tukey_array = fivenum(test_array)
p tukey_array
test_array = [0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594,
              0.73438555, -0.03035726, 1.46675970, -0.74621349, -0.72588772,
              0.63905160,  0.61501527, -0.98983780, -1.00447874, -0.62759469,
              0.66206163,  1.04312009, -0.10305385, 0.75775634,  0.32566578]
tukey_array = fivenum(test_array)
p tukey_array

```

{{out}}

```txt

[6.0, 15.0, 40.0, 43.0, 49.0]
[7.0, 15.0, 36.0, 40.0, 41.0]
[-1.95059594, -0.72588772, 0.14082834, 0.75775634, 1.73131507]
```



## SAS


```sas
/* build a dataset */
data test;
do i=1 to 10000;
	x=rannor(12345);
	output;
end;
keep x;
run;

/* compute the five numbers */
proc means data=test min p25 median p75 max;
var x;
run;
```


'''Output'''

<TABLE  cellspacing=1 cellpadding=7 rules=all frame=Box border=1>
<TR>
 <TD COLSPAN=5 ALIGN=CENTER VALIGN=BOTTOM>Analysis Variable : x </TD>
</TR>
<TR>
 <TD ALIGN=RIGHT VALIGN=BOTTOM>Minimum</TD>
 <TD ALIGN=RIGHT VALIGN=BOTTOM>25th Pctl</TD>
 <TD ALIGN=RIGHT VALIGN=BOTTOM>Median</TD>
 <TD ALIGN=RIGHT VALIGN=BOTTOM>75th Pctl</TD>
 <TD ALIGN=RIGHT VALIGN=BOTTOM>Maximum</TD>
</TR>
<TR>
 <TD ALIGN=RIGHT nowrap>-4.0692299</TD>
 <TD ALIGN=RIGHT nowrap>-0.6533022</TD>
 <TD ALIGN=RIGHT>0.0066299</TD>
 <TD ALIGN=RIGHT>0.6768043</TD>
 <TD ALIGN=RIGHT>4.1328026</TD>
</TR>
</TABLE>


## Scala


### Array based solution


```Scala
import java.util

object Fivenum extends App {

  val xl = Array(
    Array(15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0),
    Array(36.0, 40.0, 7.0, 39.0, 41.0, 15.0),
    Array(0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594, 0.73438555,
      -0.03035726, 1.46675970, -0.74621349, -0.72588772, 0.63905160, 0.61501527, -0.98983780,
      -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578)
  )

  for (x <- xl) println(f"${util.Arrays.toString(fivenum(x))}%s\n\n")

  def fivenum(x: Array[Double]): Array[Double] = {
    require(x.forall(!_.isNaN), "Unable to deal with arrays containing NaN")

    def median(x: Array[Double], start: Int, endInclusive: Int): Double = {
      val size = endInclusive - start + 1
      require(size > 0, "Array slice cannot be empty")
      val m = start + size / 2
      if (size % 2 == 1) x(m) else (x(m - 1) + x(m)) / 2.0
    }

    val result = new Array[Double](5)
    util.Arrays.sort(x)
    result(0) = x(0)
    result(2) = median(x, 0, x.length - 1)
    result(4) = x(x.length - 1)
    val m = x.length / 2
    val lowerEnd = if (x.length % 2 == 1) m else m - 1
    result(1) = median(x, 0, lowerEnd)
    result(3) = median(x, m, x.length - 1)
    result
  }

}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/8s0OdOO/2 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/Ady3dSnoRRKNhCaZYIVbig Scastie (JVM)].


## Sidef

{{trans|Perl 6}}

```ruby
func fourths(e) {
    var t = ((e>>1) / 2)
    [0, t, e/2, e - t, e]
}

func fivenum(nums) {
    var x = nums.sort
    var d = fourths(x.end)

    ([x[d.map{.floor}]] ~Z+ [x[d.map{.ceil}]]) »/» 2
}

var nums = [
    [15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43],
    [36, 40, 7, 39, 41, 15], [
    0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,
    0.73438555, -0.03035726,  1.46675970, -0.74621349, -0.72588772,
    0.63905160,  0.61501527, -0.98983780, -1.00447874, -0.62759469,
    0.66206163,  1.04312009, -0.10305385,  0.75775634,  0.32566578,
]]

nums.each { say fivenum(_).join(', ') }
```

{{out}}

```txt
6, 25.5, 40, 42.5, 49
7, 15, 37.5, 40, 41
-1.95059594, -0.676741205, 0.23324706, 0.746070945, 1.73131507
```



## Stata

First build a dataset:


```stata
clear
set seed 17760704
qui set obs 10000
gen x=rnormal()
```


The '''[https://www.stata.com/help.cgi?summarize summarize]''' command produces all the required statistics, and more:


```stata
qui sum x, detail
di r(min),r(p25),r(p50),r(p75),r(max)
```


'''Output'''


```txt
-3.6345866 -.66536 .0026834 .68398139 3.7997103
```


It's also possible to use the '''[https://www.stata.com/help.cgi?tabstat tabstat]''' command


```stata
tabstat x, s(mi q ma)
```


'''Output'''


```txt
    variable |       min       p25       p50       p75       max
-------------+--------------------------------------------------
           x | -3.634587   -.66536  .0026834  .6839814   3.79971
----------------------------------------------------------------
```


Another example:


```stata
clear
mat a=0.14082834\0.09748790\1.73131507\0.87636009\-1.95059594\ ///
	0.73438555\-0.03035726\1.46675970\-0.74621349\-0.72588772\ ///
	0.63905160\0.61501527\-0.98983780\-1.00447874\-0.62759469\ ///
	0.66206163\1.04312009\-0.10305385\0.75775634\0.32566578
svmat a
tabstat a1, s(mi q ma)
```


'''Output'''


```txt
    variable |       min       p25       p50       p75       max
-------------+--------------------------------------------------
          a1 | -1.950596 -.6767412  .2332471   .746071  1.731315
----------------------------------------------------------------
```



## VBA

Uses [[Sorting_algorithms/Quicksort#VBA|Quicksort]].
{{trans|Phix}}
```vb
Option Base 1
Private Function median(tbl As Variant, lo As Integer, hi As Integer)
    Dim l As Integer: l = hi - lo + 1
    Dim m As Integer: m = lo + WorksheetFunction.Floor_Precise(l / 2)
    If l Mod 2 = 1 Then
        median = tbl(m)
    Else
        median = (tbl(m - 1) + tbl(m)) / 2
    End if
End Function
Private Function fivenum(tbl As Variant) As Variant
    Sort tbl, UBound(tbl)
    Dim l As Integer: l = UBound(tbl)
    Dim m As Integer: m = WorksheetFunction.Floor_Precise(l / 2) + l Mod 2
    Dim r(5) As String
    r(1) = CStr(tbl(1))
    r(2) = CStr(median(tbl, 1, m))
    r(3) = CStr(median(tbl, 1, l))
    r(4) = CStr(median(tbl, m + 1, l))
    r(5) = CStr(tbl(l))
    fivenum = r
End Function
Public Sub main()
    Dim x1 As Variant, x2 As Variant, x3 As Variant
    x1 = [{15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43}]
    x2 = [{36, 40, 7, 39, 41, 15}]
    x3 = [{0.14082834, 0.09748790, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.46675970, -0.74621349, -0.72588772, 0.63905160, 0.61501527, -0.98983780, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578}]
    Debug.Print Join(fivenum(x1), " | ")
    Debug.Print Join(fivenum(x2), " | ")
    Debug.Print Join(fivenum(x3), " | ")
End Sub
```
{{out}}

```txt
6 | 25,5 | 40 | 43 | 49
7 | 15 | 37,5 | 40 | 41
-1,95059594 | -0,676741205 | 0,23324706 | 0,746070945 | 1,73131507
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Runtime.CompilerServices
Imports System.Text

Module Module1

    <Extension()>
    Function AsString(Of T)(c As ICollection(Of T), Optional format As String = "{0}") As String
        Dim sb As New StringBuilder("[")
        Dim it = c.GetEnumerator()
        If it.MoveNext() Then
            sb.AppendFormat(format, it.Current)
        End If
        While it.MoveNext()
            sb.Append(", ")
            sb.AppendFormat(format, it.Current)
        End While
        Return sb.Append("]").ToString()
    End Function

    Function Median(x As Double(), start As Integer, endInclusive As Integer) As Double
        Dim size = endInclusive - start + 1
        If size <= 0 Then
            Throw New ArgumentException("Array slice cannot be empty")
        End If
        Dim m = start + size \ 2
        Return If(size Mod 2 = 1, x(m), (x(m - 1) + x(m)) / 2.0)
    End Function

    Function Fivenum(x As Double()) As Double()
        For Each d In x
            If Double.IsNaN(d) Then
                Throw New ArgumentException("Unable to deal with arrays containing NaN")
            End If
        Next

        Array.Sort(x)
        Dim result(4) As Double

        result(0) = x.First()
        result(2) = Median(x, 0, x.Length - 1)
        result(4) = x.Last()

        Dim m = x.Length \ 2
        Dim lowerEnd = If(x.Length Mod 2 = 1, m, m - 1)

        result(1) = Median(x, 0, lowerEnd)
        result(3) = Median(x, m, x.Length - 1)

        Return result
    End Function

    Sub Main()
        Dim x1 = {
            New Double() {15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0},
            New Double() {36.0, 40.0, 7.0, 39.0, 41.0, 15.0},
            New Double() {
                     0.14082834, 0.0974879, 1.73131507, 0.87636009, -1.95059594, 0.73438555,
                    -0.03035726, 1.4667597, -0.74621349, -0.72588772, 0.6390516, 0.61501527,
                    -0.9898378, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385,
                     0.75775634, 0.32566578
            }
        }
        For Each x In x1
            Dim result = Fivenum(x)
            Console.WriteLine(result.AsString("{0:F8}"))
        Next
    End Sub

End Module
```

{{out}}

```txt
[6.00000000, 25.50000000, 40.00000000, 42.50000000, 49.00000000]
[7.00000000, 15.00000000, 37.50000000, 40.00000000, 41.00000000]
[-1.95059594, -0.67674121, 0.23324706, 0.74607095, 1.73131507]
```



## zkl

Uses GNU GSL library.

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
fcn fiveNum(v){ // V is a GSL Vector, --> min, 1st qu, median, 3rd qu, max
   v.sort();
   return(v.min(),v.quantile(0.25),v.median(),v.quantile(0.75),v.max())
}
```


```zkl
fiveNum(GSL.VectorFromData(
   15.0, 6.0, 42.0, 41.0, 7.0, 36.0, 49.0, 40.0, 39.0, 47.0, 43.0)).println();
println(fiveNum(GSL.VectorFromData(36.0, 40.0, 7.0, 39.0, 41.0, 15.0)));

v:=GSL.VectorFromData(
   0.14082834,  0.09748790,  1.73131507,  0.87636009, -1.95059594,  0.73438555,
  -0.03035726,  1.46675970, -0.74621349, -0.72588772,  0.63905160,  0.61501527,
  -0.98983780, -1.00447874, -0.62759469,  0.66206163,  1.04312009, -0.10305385,
   0.75775634,  0.32566578);
println(fiveNum(v));
```

{{out}}

```txt

L(6,25.5,40,42.5,49)
L(7,20.25,37.5,39.75,41)
L(-1.9506,-0.652168,0.233247,0.740228,1.73132)

```


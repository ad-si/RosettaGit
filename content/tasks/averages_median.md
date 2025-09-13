+++
title = "Averages/Median"
description = ""
date = 2019-10-06T04:58:23Z
aliases = []
[extra]
id = 4376
[taxonomies]
categories = ["Probability and statistics", "task"]
tags = []
+++

## Task

Write a program to find the   [[wp:Median|median]]   value of a vector of floating-point numbers.

The program need not handle the case where the vector is empty, but ''must'' handle the case where there are an even number of elements.   In that case, return the average of the two middle values.

There are several approaches to this.   One is to sort the elements, and then pick the element(s) in the middle.

Sorting would take at least   <big><span style="font-family: serif">O(''n'' log''n'')</span></big>.   Another approach would be to build a priority queue from the elements, and then extract half of the elements to get to the middle element(s).   This would also take   <big><span style="font-family: serif">O(''n'' log''n'')</span></big>.   The best solution is to use the   [[wp:Selection algorithm|selection algorithm]]   to find the median in   <big><span style="font-family: serif">O(''n'')</span></big>   time.

{{task heading|See also}}
[[Quickselect_algorithm]]
{{Related tasks/Statistical measures}}

<hr>


## 11l

{{trans|Python}}

```11l
F median(aray)
   V srtd = sorted(aray)
   V alen = srtd.len
   R 0.5 * (srtd[(alen - 1) I/ 2] + srtd[alen I/ 2])

print(median([4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2]))
print(median([4.1, 7.2, 1.7, 9.3, 4.4, 3.2]))
```

{{out}}

```txt

4.4
4.25

```



## Ada


```ada
with Ada.Text_IO, Ada.Float_Text_IO;

procedure FindMedian is

    f: array(1..10) of float := ( 4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5 );
    min_idx: integer;
    min_val, median_val, swap: float;

begin
    for i in f'range loop
        min_idx := i;
        min_val := f(i);
        for j in i+1 .. f'last loop
            if f(j) < min_val then
                min_idx := j;
                min_val := f(j);
            end if;
        end loop;
        swap := f(i); f(i) := f(min_idx); f(min_idx) := swap;
    end loop;

    if f'length mod 2 /= 0 then
        median_val := f( f'length/2+1 );
    else
        median_val := ( f(f'length/2) + f(f'length/2+1) ) / 2.0;
    end if;

    Ada.Text_IO.Put( "Median value: " );
    Ada.Float_Text_IO.Put( median_val );
    Ada.Text_IO.New_line;
end FindMedian;
```


## ALGOL 68

{{trans|C}}
```algol68
INT max_elements = 1000000;

# Return the k-th smallest item in array x of length len #
PROC quick_select = (INT k, REF[]REAL x) REAL:
   BEGIN

      PROC swap = (INT a, b) VOID:
         BEGIN
	    REAL t = x[a];
	    x[a] := x[b]; x[b] := t
         END;

      INT left := 1, right := UPB x;
      INT pos, i;
      REAL pivot;

      WHILE left < right DO
	 pivot := x[k];
	 swap (k, right);
	 pos := left;
	 FOR i FROM left TO right DO
	    IF x[i] < pivot THEN
	       swap (i, pos);
	       pos +:= 1
	    FI
	 OD;
	 swap (right, pos);
	 IF pos = k THEN break FI;
	 IF pos < k THEN left := pos + 1
	 ELSE right := pos - 1
         FI
      OD;
break:
      SKIP;
      x[k]
   END;

 # Initialize random length REAL array with random doubles #
 INT length = ENTIER (next random * max_elements);
 [length]REAL x;
 FOR i TO length DO
    x[i] := (next random * 1e6 - 0.5e6)
 OD;

 REAL median :=
    IF NOT ODD length THEN
       # Even number of elements, median is average of middle two #
       (quick_select (length % 2, x) + quick_select(length % 2 - 1, x)) / 2
    ELSE
       # select middle element #
       quick_select(length % 2, x)
    FI;

 # Sanity testing of median #
 INT less := 0, more := 0, eq := 0;
 FOR i TO length DO
    IF x[i] < median THEN less +:= 1
    ELIF x[i] > median THEN more +:= 1
    ELSE eq +:= 1
    FI
 OD;
 print (("length: ", whole (length,0), new line, "median: ", median, new line,
	 "<: ", whole (less,0), new line,
	 ">: ", whole (more, 0), new line,
	 "=: ", whole (eq, 0), new line))
```

Sample output:

```txt
length: 97738
median: -2.52550126608709e  +3
<: 48868
>: 48870
=: 0

```



## AntLang

AntLang has a built-in median function.

```AntLang
median[list]
```



## APL


```APL
median←{v←⍵[⍋⍵]⋄.5×v[⌈¯1+.5×⍴v]+v[⌊.5×⍴v]} ⍝ Assumes ⎕IO←0
```


First, the input vector ⍵ is sorted with ⍵[⍋⍵] and the result placed in v. If the dimension ⍴v of v is odd, then both ⌈¯1+.5×⍴v and ⌊.5×⍴v give the index of the middle element. If ⍴v is even, ⌈¯1+.5×⍴v and ⌊.5×⍴v give the indices of the two middle-most elements. In either case, the average of the elements at these indices gives the median.

Note that the index origin ⎕IO is assumed zero. To set it to zero use:
```APL
⎕IO←0
```


If you prefer an index origin of 1, use this code instead:

```APL

⎕IO←1
median←{v←⍵[⍋⍵] ⋄ 0.5×v[⌈0.5×⍴v]+v[⌊1+0.5×⍴v]}

```


This code was tested with ngn/apl and Dyalog 12.1. You can try this function online with [http://ngn.github.io/apl/web/index.html#code=median%u2190%7Bv%u2190%u2375%5B%u234B%u2375%5D%u22C4.5%D7v%5B%u2308%AF1+.5%D7%u2374v%5D+v%5B%u230A.5%D7%u2374v%5D%7D ngn/apl]. Note that ngn/apl currently only supports index origin 0. Examples:

```txt
median 1 5 3 6 4 2
3.5

median 1 5 3 2 4
3

median 4.4 2.3 ¯1.7 7.5 6.6 0.0 1.9 8.2 9.3 4.5
4.45

median 4.1 4 1.2 6.235 7868.33
4.1

median 4.1 5.6 7.2 1.7 9.3 4.4 3.2
4.4

median 4.1 7.2 1.7 9.3 4.4 3.2
4.25
```


Caveats: To keep it simple, no input validation is done. If you input a vector with zero elements (e.g., ⍳0), you get an INDEX ERROR. If you input a vector with 1 element, you get a RANK ERROR. Only (rank 1) numeric vectors of dimension 2 or more are supported. If you input a (rank 2 or more) matrix, you get a RANK ERROR. If you input a string (vector of chars), you get a DOMAIN ERROR:


```txt
median ⍳0
INDEX ERROR

median 66.6
RANK ERROR

median (2 2)⍴⍳4 ⍝ 2x2 matrix
RANK ERROR

median 'HELLO'
DOMAIN ERROR
```


## AppleScript



### By iteration


```AppleScript
set alist to {1, 2, 3, 4, 5, 6, 7, 8}
set med to medi(alist)

on medi(alist)

    set temp to {}
    set lcount to count every item of alist
    if lcount is equal to 2 then
        return (item (random number from 1 to 2) of alist)
    else if lcount is less than 2 then
        return item 1 of alist
    else --if lcount is greater than 2
        set min to findmin(alist)
        set max to findmax(alist)
        repeat with x from 1 to lcount
            if x is not equal to min and x is not equal to max then set end of temp to item x of alist
        end repeat
        set med to medi(temp)
    end if
    return med

end medi

on findmin(alist)

    set min to 1
    set alength to count every item of alist
    repeat with x from 1 to alength
        if item x of alist is less than item min of alist then set min to x
    end repeat
    return min

end findmin

on findmax(alist)

    set max to 1
    set alength to count every item of alist
    repeat with x from 1 to alength
        if item x of alist is greater than item max of alist then set max to x
    end repeat
    return max

end findmax
```



### Composing functionally

Using a quick select algorithm:
{{Trans|JavaScript}}
{{Trans|Haskell}}

```AppleScript
-- MEDIAN ---------------------------------------------------------------------

-- median :: [Num] -> Num
on median(xs)
    -- nth :: [Num] -> Int -> Maybe Num
    script nth
        on |λ|(xxs, n)
            if length of xxs > 0 then
                set {x, xs} to uncons(xxs)

                script belowX
                    on |λ|(y)
                        y < x
                    end |λ|
                end script

                set {ys, zs} to partition(belowX, xs)
                set k to length of ys
                if k = n then
                    x
                else
                    if k > n then
                        |λ|(ys, n)
                    else
                        |λ|(zs, n - k - 1)
                    end if
                end if
            else
                missing value
            end if
        end |λ|
    end script

    set n to length of xs
    if n > 0 then
        tell nth
            if n mod 2 = 0 then
                (|λ|(xs, n div 2) + |λ|(xs, (n div 2) - 1)) / 2
            else
                |λ|(xs, n div 2)
            end if
        end tell
    else
        missing value
    end if
end median

-- TEST -----------------------------------------------------------------------
on run

    map(median, [¬
        [], ¬
        [5, 3, 4], ¬
        [5, 4, 2, 3], ¬
        [3, 4, 1, -8.4, 7.2, 4, 1, 1.2]])

    --> {missing value, 4, 3.5, 2.1}
end run

-- GENERIC FUNCTIONS ----------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- partition :: predicate -> List -> (Matches, nonMatches)
-- partition :: (a -> Bool) -> [a] -> ([a], [a])
on partition(f, xs)
    tell mReturn(f)
        set lst to {{}, {}}
        repeat with x in xs
            set v to contents of x
            set end of item ((|λ|(v) as integer) + 1) of lst to v
        end repeat
    end tell
    {item 2 of lst, item 1 of lst}
end partition

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons
```

{{Out}}

```AppleScript
{missing value, 4, 3.5, 2.1}
```



## Applesoft BASIC


```Applesoft BASIC
 100 REMMEDIAN
 110 K = INT(L/2) : GOSUB 150
 120 R = X(K)
 130 IF L - 2 *  INT (L / 2) THEN R = (R + X(K + 1)) / 2
 140 RETURN

 150 REMQUICK SELECT
 160 LT = 0:RT = L - 1
 170 FOR J = LT TO RT STEP 0
 180     PT = X(K)
 190     P1 = K:P2 = RT: GOSUB 300
 200     P = LT
 210     FOR I = P TO RT - 1
 220         IF X(I) < PT THEN P1 = I:P2 = P: GOSUB 300:P = P + 1
 230     NEXT I
 240     P1 = RT:P2 = P: GOSUB 300
 250     IF P = K THEN  RETURN
 260     IF P < K THEN LT = P + 1
 270     IF P >  = K THEN RT = P - 1
 280 NEXT J
 290 RETURN

 300 REMSWAP
 310 H = X(P1):X(P1) = X(P2)
 320 X(P2) = H: RETURN
```
Example:
```ApplesoftBASIC
X(0)=4.4 : X(1)=2.3 : X(2)=-1.7 : X(3)=7.5 : X(4)=6.6 : X(5)=0.0 : X(6)=1.9 : X(7)=8.2 : X(8)=9.3 : X(9)=4.5 : X(10)=-11.7
L = 11 : GOSUB 100MEDIAN
? R
```
Output:
```txt
5.95
```


## AutoHotkey

Takes the lower of the middle two if length is even

```AutoHotkey
seq = 4.1, 7.2, 1.7, 9.3, 4.4, 3.2, 5
MsgBox % median(seq, "`,")  ; 4.1

median(seq, delimiter)
{
  Sort, seq, ND%delimiter%
  StringSplit, seq, seq, % delimiter
  median := Floor(seq0 / 2)
  Return seq%median%
}
```



## AWK


AWK arrays can be passed as parameters, but not returned, so they are usually global.


```awk
#!/usr/bin/awk -f

BEGIN {
    d[1] = 3.0
    d[2] = 4.0
    d[3] = 1.0
    d[4] = -8.4
    d[5] = 7.2
    d[6] = 4.0
    d[7] = 1.0
    d[8] = 1.2
    showD("Before: ")
    gnomeSortD()
    showD("Sorted: ")
    printf "Median: %f\n", medianD()
    exit
}

function medianD(     len, mid) {
    len = length(d)
    mid = int(len/2) + 1
    if (len % 2) return d[mid]
    else return (d[mid] + d[mid-1]) / 2.0
}

function gnomeSortD(    i) {
    for (i = 2; i <= length(d); i++) {
        if (d[i] < d[i-1]) gnomeSortBackD(i)
    }
}

function gnomeSortBackD(i,     t) {
    for (; i > 1 && d[i] < d[i-1]; i--) {
        t = d[i]
        d[i] = d[i-1]
        d[i-1] = t
    }
}

function showD(p,   i) {
    printf p
    for (i = 1; i <= length(d); i++) {
        printf d[i] " "
    }
    print ""
}

```


Example output:
 Before: 3 4 1 -8.4 7.2 4 1 1.2
 Sorted: -8.4 1 1 1.2 3 4 4 7.2
 Median: 2.100000


## BaCon


```freebasic
DECLARE a[] = { 4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2 } TYPE FLOATING
DECLARE b[] = { 4.1, 7.2, 1.7, 9.3, 4.4, 3.2 } TYPE FLOATING

DEF FN Dim(x) = SIZEOF(x) / SIZEOF(double)

DEF FN Median(x) = IIF(ODD(Dim(x)), x[(Dim(x)-1)/2], (x[Dim(x)/2-1]+x[Dim(x)/2])/2 )

SORT a
PRINT "Median of a: ", Median(a)

SORT b
PRINT "Median of b: ", Median(b)
```

{{out}}

```txt

Median of a: 4.4
Median of b: 4.25

```



## BASIC

{{works with|FreeBASIC}}
{{works with|PowerBASIC}}
{{works with|QB64}}
{{works with|QBasic}}
{{works with|Visual Basic}}

This uses the Quicksort function described at [[Quicksort#BASIC]], with <code>arr()</code>'s type changed to <code>SINGLE</code>.

Note that in order to truly work with the Windows versions of PowerBASIC, the module-level code must be contained inside <code>FUNCTION PBMAIN</code>. Similarly, in order to work under Visual Basic, the same module-level code must be contained with <code>Sub Main</code>.


```qbasic
DECLARE FUNCTION median! (vector() AS SINGLE)

DIM vec1(10) AS SINGLE, vec2(11) AS SINGLE, n AS INTEGER

RANDOMIZE TIMER

FOR n = 0 TO 10
    vec1(n) = RND * 100
    vec2(n) = RND * 100
NEXT
vec2(11) = RND * 100

PRINT median(vec1())
PRINT median(vec2())

FUNCTION median! (vector() AS SINGLE)
    DIM lb AS INTEGER, ub AS INTEGER, L0 AS INTEGER
    lb = LBOUND(vector)
    ub = UBOUND(vector)
    REDIM v(lb TO ub) AS SINGLE
    FOR L0 = lb TO ub
        v(L0) = vector(L0)
    NEXT
    quicksort v(), lb, ub
    IF ((ub - lb + 1) MOD 2) THEN
        median = v((ub + lb) / 2)
    ELSE
        median = (v(INT((ub + lb) / 2)) + v(INT((ub + lb) / 2) + 1)) / 2
    END IF
END FUNCTION
```


See also: [[#BBC BASIC|BBC BASIC]], [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]], [[#TI-83 BASIC|TI-83 BASIC]], [[#TI-89 BASIC|TI-89 BASIC]].


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0,0)

      DIM a(6), b(5)
      a() = 4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2
      b() = 4.1, 7.2, 1.7, 9.3, 4.4, 3.2

      PRINT "Median of a() is " ; FNmedian(a())
      PRINT "Median of b() is " ; FNmedian(b())
      END

      DEF FNmedian(a())
      LOCAL C%
      C% = DIM(a(),1) + 1
      CALL Sort%, a(0)
      = (a(C% DIV 2) + a((C%-1) DIV 2)) / 2

```

Output:

```txt
Median of a() is 4.4
Median of b() is 4.25
```



## Bracmat


Bracmat has no floating point numbers, so we have to parse floating point numbers as strings and convert them to rational numbers.
Each number is packaged in a little list and these lists are accumulated in a sum. Bracmat keeps sums sorted, so the median is the term in the middle of the list, or the average of the two terms in the middle of the list.


```bracmat
(median=
  begin decimals end int list med med1 med2 num number
.   0:?list
  &   whl
    ' ( @( !arg
         :   ?
             ((%@:~" ":~",") ?:?number)
             ((" "|",") ?arg|:?arg)
         )
      & @( !number
         : (   #?int "." [?begin #?decimals [?end
             & !int+!decimals*10^(!begin+-1*!end):?num
           | ?num
           )
         )
      & (!num.)+!list:?list
      )
  & !list:?+[?end
  & (   !end*1/2:~/
      & !list:?+[!(=1/2*!end+-1)+(?med1.)+(?med2.)+?
      & !med1*1/2+!med2*1/2:?med
    | !list:?+[(div$(1/2*!end,1))+(?med.)+?
    )
  & !med
);
```




```txt
 median$" 4.1 4 1.2 6.235 7868.33"
 41/10

 median$"4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5"
 89/20

 median$"1, 5, 3, 2, 4"
 3

 median$"1, 5, 3, 6, 4, 2"
 7/2
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct floatList {
    float *list;
    int   size;
} *FloatList;

int floatcmp( const void *a, const void *b) {
    if (*(const float *)a < *(const float *)b) return -1;
    else return *(const float *)a > *(const float *)b;
}

float median( FloatList fl )
{
    qsort( fl->list, fl->size, sizeof(float), floatcmp);
    return 0.5 * ( fl->list[fl->size/2] + fl->list[(fl->size-1)/2]);
}

int main()
{
    static float floats1[] = { 5.1, 2.6, 6.2, 8.8, 4.6, 4.1 };
    static struct floatList flist1 = { floats1, sizeof(floats1)/sizeof(float) };

    static float floats2[] = { 5.1, 2.6, 8.8, 4.6, 4.1 };
    static struct floatList flist2 = { floats2, sizeof(floats2)/sizeof(float) };

    printf("flist1 median is %7.2f\n", median(&flist1)); /* 4.85 */
    printf("flist2 median is %7.2f\n", median(&flist2)); /* 4.60 */
    return 0;
}
```


### Quickselect algorithm

Average O(n) time:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define MAX_ELEMENTS 1000000

/* Return the k-th smallest item in array x of length len */
double quick_select(int k, double *x, int len)
{
   inline void swap(int a, int b)
   {
      double t = x[a];
      x[a] = x[b], x[b] = t;
   }

   int left = 0, right = len - 1;
   int pos, i;
   double pivot;

   while (left < right)
   {
      pivot = x[k];
      swap(k, right);
      for (i = pos = left; i < right; i++)
      {
         if (x[i] < pivot)
         {
            swap(i, pos);
            pos++;
         }
      }
      swap(right, pos);
      if (pos == k) break;
      if (pos < k) left = pos + 1;
      else right = pos - 1;
   }
   return x[k];
}

int main(void)
{
   int i, length;
   double *x, median;

   /* Initialize random length double array with random doubles */
   srandom(time(0));
   length = random() % MAX_ELEMENTS;
   x = malloc(sizeof(double) * length);
   for (i = 0; i < length; i++)
   {
      // shifted by RAND_MAX for negative values
      // divide by a random number for floating point
      x[i] = (double)(random() - RAND_MAX / 2) / (random() + 1); // + 1 to not divide by 0
   }


   if (length % 2 == 0) // Even number of elements, median is average of middle two
   {
      median = (quick_select(length / 2, x, length) + quick_select(length / 2 - 1, x, length / 2)) / 2;
   }
   else // select middle element
   {
      median = quick_select(length / 2, x, length);
   }


   /* Sanity testing of median */
   int less = 0, more = 0, eq = 0;
   for (i = 0; i < length; i++)
   {
      if (x[i] < median) less ++;
      else if (x[i] > median) more ++;
      else eq ++;
   }
   printf("length: %d\nmedian: %lf\n<: %d\n>: %d\n=: %d\n", length, median, less, more, eq);

   free(x);
   return 0;
}

```


Output:

```c
length: 992021
median: 0.000473
<: 496010
>: 496010
=: 1
```



## C++

This function runs in linear time on average.

```cpp
#include <algorithm>

// inputs must be random-access iterators of doubles
// Note: this function modifies the input range
template <typename Iterator>
double median(Iterator begin, Iterator end) {
  // this is middle for odd-length, and "upper-middle" for even length
  Iterator middle = begin + (end - begin) / 2;

  // This function runs in O(n) on average, according to the standard
  std::nth_element(begin, middle, end);

  if ((end - begin) % 2 != 0) { // odd length
    return *middle;
  } else { // even length
    // the "lower middle" is the max of the lower half
    Iterator lower_middle = std::max_element(begin, middle);
    return (*middle + *lower_middle) / 2.0;
  }
}

#include <iostream>

int main() {
  double a[] = {4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2};
  double b[] = {4.1, 7.2, 1.7, 9.3, 4.4, 3.2};

  std::cout << median(a+0, a + sizeof(a)/sizeof(a[0])) << std::endl; // 4.4
  std::cout << median(b+0, b + sizeof(b)/sizeof(b[0])) << std::endl; // 4.25

  return 0;
}
```

## C#

```c#
using System;
using System.Linq;

namespace Test
{
    class Program
    {
        static void Main()
        {
            double[] myArr = new double[] { 1, 5, 3, 6, 4, 2 };

            myArr = myArr.OrderBy(i => i).ToArray();
            // or Array.Sort(myArr) for in-place sort

            int mid = myArr.Length / 2;
            double median;

            if (myArr.Length % 2 == 0)
            {
                //we know its even
                median = (myArr[mid] + myArr[mid - 1]) / 2.0;
            }
            else
            {
                //we know its odd
                median = myArr[mid];
            }

            Console.WriteLine(median);
            Console.ReadLine();
        }
    }
}

```



## Clojure

Simple:

```lisp
(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))
```



## COBOL

Intrinsic function:

```cobol
FUNCTION MEDIAN(some-table (ALL))
```



## Common Lisp


The recursive partitioning solution, without the median of medians optimization.


```lisp
((defun select-nth (n list predicate)
  "Select nth element in list, ordered by predicate, modifying list."
  (do ((pivot (pop list))
       (ln 0) (left '())
       (rn 0) (right '()))
      ((endp list)
       (cond
        ((< n ln) (select-nth n left predicate))
        ((eql n ln) pivot)
        ((< n (+ ln rn 1)) (select-nth (- n ln 1) right predicate))
        (t (error "n out of range."))))
    (if (funcall predicate (first list) pivot)
      (psetf list (cdr list)
             (cdr list) left
             left list
             ln (1+ ln))
      (psetf list (cdr list)
             (cdr list) right
             right list
             rn (1+ rn)))))

(defun median (list predicate)
  (select-nth (floor (length list) 2) list predicate))
```



## D


```d
import std.stdio, std.algorithm;

T median(T)(T[] nums) pure nothrow {
    nums.sort();
    if (nums.length & 1)
        return nums[$ / 2];
    else
        return (nums[$ / 2 - 1] + nums[$ / 2]) / 2.0;
}

void main() {
    auto a1 = [5.1, 2.6, 6.2, 8.8, 4.6, 4.1];
    writeln("Even median: ", a1.median);

    auto a2 = [5.1, 2.6, 8.8, 4.6, 4.1];
    writeln("Odd median:  ", a2.median);
}
```

{{out}}

```txt
Even median: 4.85
Odd median:  4.6
```



## Delphi


```Delphi
program AveragesMedian;

{$APPTYPE CONSOLE}

uses Generics.Collections, Types;

function Median(aArray: TDoubleDynArray): Double;
var
  lMiddleIndex: Integer;
begin
  TArray.Sort<Double>(aArray);

  lMiddleIndex := Length(aArray) div 2;
  if Odd(Length(aArray)) then
    Result := aArray[lMiddleIndex]
  else
    Result := (aArray[lMiddleIndex - 1] + aArray[lMiddleIndex]) / 2;
end;

begin
  Writeln(Median(TDoubleDynArray.Create(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)));
  Writeln(Median(TDoubleDynArray.Create(4.1, 7.2, 1.7, 9.3, 4.4, 3.2)));
end.
```



## E


TODO: Use the selection algorithm, whatever that is [[Category:E examples needing attention]]


```e
def median(list) {
    def sorted := list.sort()
    def count := sorted.size()
    def mid1 := count // 2
    def mid2 := (count - 1) // 2
    if (mid1 == mid2) {          # avoid inexact division
        return sorted[mid1]
    } else {
        return (sorted[mid1] + sorted[mid2]) / 2
    }
}
```



```e
? median([1,9,2])
# value: 2

? median([1,9,2,4])
# value: 3.0
```



## EasyLang


<lang>func quickselect k . list#[] res# .
  #
  subr partition
    swap list#[(left + right) / 2] list#[left]
    mid = left
    for i = left + 1 to right
      if list#[i] < list#[left]
        mid += 1
        swap list#[i] list#[mid]
      .
    .
    swap list#[left] list#[mid]
  .
  left = 0
  right = len list#[] - 1
  while left < right
    call partition
    if mid < k
      left = mid + 1
    elif mid > k
      right = mid - 1
    else
      left = right
    .
  .
  res# = list#[k]
.
func median . list#[] res# .
  h = len list#[] / 2
  call quickselect h list#[] res#
  if len list#[] mod 2 = 0
    call quickselect h - 1 list#[] h#
    res# = (res# + h#) / 2
  .
.
test#[] = [ 4.1 5.6 7.2 1.7 9.3 4.4 3.2 ]
call median test#[] med#
print med#
test#[] = [ 4.1 7.2 1.7 9.3 4.4 3.2 ]
call median test#[] med#
print med#
```



```txt

4.400
4.250

```



## EchoLisp


```scheme

(define (median L) ;; O(n log(n))
	(set! L (vector-sort! < (list->vector L)))
	(define dim (// (vector-length L) 2))
	(if (integer? dim)
		(// (+ [L dim] [L (1- dim)]) 2)
		[L (floor dim)]))

(median '( 3 4 5))
   → 4
(median '(6 5 4 3))
   → 4.5
(median (iota 10000))
   → 4999.5
(median (iota 10001))
   → 5000

```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import system'math;
import extensions;

extension op
{
    get Median()
    {
        var sorted := self.Ascendant;

        var len := sorted.Length;
        if (len == 0)
        {
            ^ nil
        }
        else
        {
            var middleIndex := len / 2;
            if (len.mod:2 == 0)
            {
                ^ (sorted[middleIndex - 1] + sorted[middleIndex]) / 2
            }
            else
            {
                ^ sorted[middleIndex]
            }
        }
    }
}

public program()
{
    var a1 := new real[]::(4.1r, 5.6r, 7.2r, 1.7r, 9.3r, 4.4r, 3.2r);
    var a2 := new real[]::(4.1r, 7.2r, 1.7r, 9.3r, 4.4r, 3.2r);

    console.printLine("median of (",a1,") is ",a1.Median);
    console.printLine("median of (",a2,") is ",a2.Median);

    console.readChar()
}
```

{{out}}

```txt

median of (4.1,5.6,7.2,1.7,9.3,4.4,3.2) is 4.4
median of (4.1,7.2,1.7,9.3,4.4,3.2) is 4.25

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Average do
  def median([]), do: nil
  def median(list) do
    len = length(list)
    sorted = Enum.sort(list)
    mid = div(len, 2)
    if rem(len,2) == 0, do: (Enum.at(sorted, mid-1) + Enum.at(sorted, mid)) / 2,
                      else: Enum.at(sorted, mid)
  end
end

median = fn list -> IO.puts "#{inspect list} => #{inspect Average.median(list)}" end
median.([])
Enum.each(1..6, fn i ->
  (for _ <- 1..i, do: :rand.uniform(6)) |> median.()
end)
```


{{out}}

```txt

[] => nil
[4] => 4
[1, 6] => 3.5
[5, 2, 4] => 4
[2, 3, 5, 1] => 2.5
[3, 2, 6, 3, 2] => 3
[6, 4, 2, 3, 1, 3] => 3.0

```



## Erlang


```erlang
-module(median).
-import(lists, [nth/2, sort/1]).
-compile(export_all).

median(Unsorted) ->
    Sorted = sort(Unsorted),
    Length = length(Sorted),
    Mid = Length div 2,
    Rem = Length rem 2,
    (nth(Mid+Rem, Sorted) + nth(Mid+1, Sorted)) / 2.
```



## ERRE

<lang>
PROGRAM MEDIAN

DIM X[10]

PROCEDURE QUICK_SELECT
    LT=0 RT=L-1
    J=LT
    REPEAT
        PT=X[K]
        SWAP(X[K],X[RT])
        P=LT
        FOR I=P TO RT-1 DO
            IF X[I]<PT THEN SWAP(X[I],X[P]) P=P+1 END IF
        END FOR
        SWAP(X[RT],X[P])
        IF P=K THEN EXIT PROCEDURE END IF
        IF P<K THEN LT=P+1 END IF
        IF P>=K THEN RT=P-1 END IF
    UNTIL J>RT
END PROCEDURE

PROCEDURE MEDIAN
    K=INT(L/2)
    QUICK_SELECT
    R=X[K]
    IF L-2*INT(L/2)<>0 THEN R=(R+X[K+1])/2 END IF
END PROCEDURE

BEGIN
   PRINT(CHR$(12);) !CLS
   X[0]=4.4 X[1]=2.3 X[2]=-1.7 X[3]=7.5 X[4]=6.6 X[5]=0
   X[6]=1.9 X[7]=8.2 X[8]=9.3 X[9]=4.5 X[10]=-11.7
   L=11
   MEDIAN
   PRINT(R)
END PROGRAM

```

Ouput is 5.95


## Euler Math Toolbox


The following function does much more than computing the median. It can handle
a matrix of x values row by row. Then it can handle multiplicities in the vector
v. Moreover it can search for the p median, not only the p=0.5 median.


```Euler Math Toolbox

>type median
 function median (x, v: none, p)

 ## Default for v : none
 ## Default for p : 0.5

     m=rows(x);
     if m>1 then
         y=zeros(m,1);
         loop 1 to m;
             y[#]=median(x[#],v,p);
         end;
         return y;
     else
         if v<>none then
             {xs,i}=sort(x); vsh=v[i];
             n=cols(xs);
             ns=sum(vsh);
             i=1+p*(ns-1); i0=floor(i);
             vs=cumsum(vsh);
             loop 1 to n
                 if vs[#]>i0 then
                     return xs[#];
                 elseif vs[#]+1>i0 then
                     k=#+1;
                     repeat;
                         if vsh[k]>0 or k>n then break; endif;
                         k=k+1;
                     end;
                     return (1-(i-i0))*xs[#]+(i-i0)*xs[k]+0;
                 endif;
             end;
             return xs[n];
         else
             xs=sort(x);
             n=cols(x);
             i=1+p*(n-1); i0=floor(i);
             if i0==n then return xs[n]; endif;
             return (i-i0)*xs[i+1]+(1-(i-i0))*xs[i];
         endif;
     endif;
 endfunction
>median(1:10)
 5.5
>median(1:9)
 5
>median(1:10,p=0.2)
 2.8
>0.2*10+0.8*1
 2.8

```



## Euphoria


```euphoria
function median(sequence s)
    atom min,k
    -- Selection sort of half+1
    for i = 1 to length(s)/2+1 do
        min = s[i]
        k = 0
        for j = i+1 to length(s) do
            if s[j] < min then
                min = s[j]
                k = j
            end if
        end for
        if k then
            s[k] = s[i]
            s[i] = min
        end if
    end for
    if remainder(length(s),2) = 0 then
        return (s[$/2]+s[$/2+1])/2
    else
        return s[$/2+1]
    end if
end function

? median({ 4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5 })
```


Output:

```txt
4.45

```



## Excel

Assuming the values are entered in the A column, type into any cell which will not be part of the list :


```excel

=MEDIAN(A1:A10)

```


Assuming 10 values will be entered, alternatively, you can just type


```excel

=MEDIAN(

```

and then select the start and end cells, not necessarily in the same row or column.

The output for the first expression, for any 10 numbers is

<lang>
23	11,5
21
12
3
19
7
23
11
9
0

```


=={{header|F Sharp|F#}}==
Median of Medians algorithm implementation

```fsharp

let rec splitToFives list =
    match list with
        | a::b::c::d::e::tail ->
            ([a;b;c;d;e])::(splitToFives tail)
        | [] -> []
        | _ ->
                let left = 5 - List.length (list)
                let last = List.append list (List.init left (fun _ -> System.Double.PositiveInfinity) )
                in [last]

let medianFromFives =
    List.map ( fun (i:float list) ->
        List.nth (List.sort i) 2 )

let start l =
    let rec magicFives list k =
        if List.length(list) <= 10 then
            List.nth (List.sort list) (k-1)
        else
            let s = splitToFives list
            let M = medianFromFives s
            let m = magicFives M (int(System.Math.Ceiling((float(List.length M))/2.)))
            let (ll,lg) = List.partition ( fun i -> i < m ) list
            let (le,lg) = List.partition ( fun i -> i = m ) lg
            in
               if (List.length ll >= k) then
                    magicFives ll k
               else if (List.length ll + List.length le >= k ) then m
               else
                    magicFives lg (k-(List.length ll)-(List.length le))
    in
        let len = List.length l in
        if (len % 2 = 1) then
            magicFives l ((len+1)/2)
        else
            let a = magicFives l (len/2)
            let b = magicFives l ((len/2)+1)
            in (a+b)/2.


let z = [1.;5.;2.;8.;7.;2.]
start z
let z' = [1.;5.;2.;8.;7.]
start z'

```



## Factor

The quicksort-style solution, with random pivoting. Takes the lesser of the two medians for even sequences.

```factor
USING: arrays kernel locals math math.functions random sequences ;
IN: median

: pivot ( seq -- pivot ) random ;

: split ( seq pivot -- {lt,eq,gt} )
  [ [ < ] curry partition ] keep
  [ = ] curry partition
  3array ;

DEFER: nth-in-order
:: nth-in-order-recur ( seq ind -- elt )
  seq dup pivot split
  dup [ length ] map  0 [ + ] accumulate nip
  dup [ ind <= [ 1 ] [ 0 ] if ] map sum 1 -
  [ swap nth ] curry bi@
  ind swap -
  nth-in-order ;

: nth-in-order ( seq ind -- elt )
  dup 0 =
  [ drop first ]
  [ nth-in-order-recur ]
  if ;

: median ( seq -- median )
  dup length 1 - 2 / floor nth-in-order ;
```


Usage:

```factor
( scratchpad ) 11 iota median .
5
( scratchpad ) 10 iota median .
4
```



## Forth

This uses the O(n) algorithm derived from [[quicksort]].

```forth
-1 cells constant -cell
: cell-   -cell + ;

defer lessthan ( a@ b@ -- ? )   ' < is lessthan

: mid ( l r -- mid ) over - 2/ -cell and + ;

: exch ( addr1 addr2 -- ) dup @ >r over @ swap ! r> swap ! ;

: part ( l r -- l r r2 l2 )
  2dup mid @ >r ( r: pivot )
  2dup begin
    swap begin dup @  r@ lessthan while cell+ repeat
    swap begin r@ over @ lessthan while cell- repeat
    2dup <= if 2dup exch >r cell+ r> cell- then
  2dup > until  r> drop ;

0 value midpoint

: select ( l r -- )
  begin 2dup < while
    part
    dup  midpoint >= if nip nip ( l l2 ) else
    over midpoint <= if drop rot drop swap ( r2 r ) else
    2drop 2drop exit then then
  repeat 2drop ;

: median ( array len -- m )
  1- cells over +  2dup mid to midpoint
  select           midpoint @ ;
```


```forth
create test 4 , 2 , 1 , 3 , 5 ,

test 4 median .   \ 2
test 5 median .   \ 3
```



## Fortran

{{works with|Fortran|90 and later}}


```fortran
program Median_Test

  real            :: a(7) = (/ 4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2 /), &
                     b(6) = (/ 4.1, 7.2, 1.7, 9.3, 4.4, 3.2 /)

  print *, median(a)
  print *, median(b)

contains

  function median(a, found)
    real, dimension(:), intent(in) :: a
      ! the optional found argument can be used to check
      ! if the function returned a valid value; we need this
      ! just if we suspect our "vector" can be "empty"
    logical, optional, intent(out) :: found
    real :: median

    integer :: l
    real, dimension(size(a,1)) :: ac

    if ( size(a,1) < 1 ) then
       if ( present(found) ) found = .false.
    else
       ac = a
       ! this is not an intrinsic: peek a sort algo from
       ! Category:Sorting, fixing it to work with real if
       ! it uses integer instead.
       call sort(ac)

       l = size(a,1)
       if ( mod(l, 2) == 0 ) then
          median = (ac(l/2+1) + ac(l/2))/2.0
       else
          median = ac(l/2+1)
       end if

       if ( present(found) ) found = .true.
    end if

  end function median

end program Median_Test
```


If one refers to [[Quickselect_algorithm#Fortran]] which offers function FINDELEMENT(K,A,N) that returns the value of A(K) when the array of N elements has been rearranged if necessary so that A(K) is the K'th in order, then, supposing that a version is devised using the appropriate type for array A,
```Fortran
      K = N/2
      MEDIAN = FINDELEMENT(K + 1,A,N)
      IF (MOD(N,2).EQ.0) MEDIAN = (FINDELEMENT(K,A,N) + MEDIAN)/2
```

As well as returning a result, the function possibly re-arranges the elements of the array, which is not "pure" behaviour. Not to the degree of fully sorting them, merely that all elements before K are not larger than A(K) as it now is, and all elements after K are not smaller than A(K).


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub quicksort(a() As Double, first As Integer, last As Integer)
  Dim As Integer length = last - first + 1
  If length < 2 Then Return
  Dim pivot As Double = a(first + length\ 2)
  Dim lft As Integer = first
  Dim rgt As Integer = last
  While lft <= rgt
    While a(lft) < pivot
      lft +=1
    Wend
    While a(rgt) > pivot
      rgt -= 1
    Wend
    If lft <= rgt Then
       Swap a(lft), a(rgt)
       lft += 1
       rgt -= 1
    End If
  Wend
  quicksort(a(), first, rgt)
  quicksort(a(), lft, last)
End Sub

Function median(a() As Double) As Double
  Dim lb As Integer = LBound(a)
  Dim ub As Integer = UBound(a)
  Dim length As Integer = ub - lb + 1
  If length = 0 Then Return 0.0/0.0 '' NaN
  If length = 1 Then Return a(ub)
  Dim mb As Integer = (lb + ub) \2
  If length Mod 2 = 1 Then Return a(mb)
  Return (a(mb) + a(mb + 1))/2.0
End Function

Dim a(0 To 9) As Double = {4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5}
quicksort(a(), 0, 9)
Print "Median for all 10 elements  : "; median(a())
' now get rid of final element
Dim b(0 To 8) As Double = {4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3}
quicksort(b(), 0, 8)
Print "Median for first 9 elements : "; median(b())
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Median for all 10 elements  :  4.45
Median for first 9 elements :  4.4

```



## GAP


```gap
Median := function(v)
  local n, w;
  w := SortedList(v);
  n := Length(v);
  return (w[QuoInt(n + 1, 2)] + w[QuoInt(n, 2) + 1]) / 2;
end;

a := [41, 56, 72, 17, 93, 44, 32];
b := [41, 72, 17, 93, 44, 32];

Median(a);
# 44
Median(b);
# 85/2
```


## Go


### Sort

Go built-in sort.  O(n log n).

```go
package main

import (
    "fmt"
    "sort"
)

func main() {
    fmt.Println(median([]float64{3, 1, 4, 1}))    // prints 2
    fmt.Println(median([]float64{3, 1, 4, 1, 5})) // prints 3
}

func median(a []float64) float64 {
    sort.Float64s(a)
    half := len(a) / 2
    m := a[half]
    if len(a)%2 == 0 {
        m = (m + a[half-1]) / 2
    }
    return m
}
```


### Partial selection sort


The task description references the WP entry for "selection algorithm" which (as of this writing) gives just one pseudocode example, which is implemented here.  As the WP article notes, it is O(kn).
Unfortunately in the case of median, k is n/2 so the algorithm is O(n^2).  Still, it gives the idea of median by selection.  Note that the partial selection sort does leave the k smallest values sorted, so in the case of an even number of elements, the two elements to average are available after a single call to sel().


```go
package main

import "fmt"

func main() {
    fmt.Println(median([]float64{3, 1, 4, 1}))    // prints 2
    fmt.Println(median([]float64{3, 1, 4, 1, 5})) // prints 3
}

func median(a []float64) float64 {
    half := len(a) / 2
    med := sel(a, half)
    if len(a)%2 == 0 {
        return (med + a[half-1]) / 2
    }
    return med
}

func sel(list []float64, k int) float64 {
    for i, minValue := range list[:k+1] {
        minIndex := i
        for j := i + 1; j < len(list); j++ {
            if list[j] < minValue {
                minIndex = j
                minValue = list[j]
                list[i], list[minIndex] = minValue, list[i]
            }
        }
    }
    return list[k]
}
```


### Quickselect

It doesn't take too much more code to implement a quickselect with random pivoting, which should run in expected time O(n).  The qsel function here permutes elements of its parameter "a" in place.  It leaves the slice somewhat more ordered, but unlike the sort and partial sort examples above, does not guarantee that element k-1 is in place.  For the case of an even number of elements then, median must make two separate qsel() calls.

```go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    fmt.Println(median([]float64{3, 1, 4, 1}))    // prints 2
    fmt.Println(median([]float64{3, 1, 4, 1, 5})) // prints 3
}

func median(list []float64) float64 {
    half := len(list) / 2
    med := qsel(list, half)
    if len(list)%2 == 0 {
        return (med + qsel(list, half-1)) / 2
    }
    return med
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
        if px == k {
            return pv
        }
        if k < px {
            a = a[:px]
        } else {
            // swap elements.  simply assigning a[last] would be enough to
            // allow qsel to return the correct result but it would leave slice
            // "a" unusable for subsequent use.  we want this full swap so that
            // we can make two successive qsel calls in the case of median
            // of an even number of elements.
            a[px], a[last] = pv, a[px]
            a = a[px+1:]
            k -= px + 1
        }
    }
    return a[0]
}
```



## Groovy

Solution (brute force sorting, with arithmetic averaging of dual midpoints (even sizes)):

```groovy
def median(Iterable col) {
    def s = col as SortedSet
    if (s == null) return null
    if (s.empty) return 0
    def n = s.size()
    def m = n.intdiv(2)
    def l = s.collect { it }
    n%2 == 1 ? l[m] : (l[m] + l[m-1])/2
}
```


Test:

```groovy
def a = [4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5]
def sz = a.size()

(0..sz).each {
    println """${median(a[0..<(sz-it)])} == median(${a[0..<(sz-it)]})
${median(a[it..<sz])} == median(${a[it..<sz]})"""
}
```


Output:

```txt
4.45 == median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5])
4.45 == median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5])
4.4 == median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3])
4.5 == median([2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5])
3.35 == median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2])
5.55 == median([-1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5])
2.3 == median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9])
6.6 == median([7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5])
3.35 == median([4.4, 2.3, -1.7, 7.5, 6.6, 0.0])
5.55 == median([6.6, 0.0, 1.9, 8.2, 9.3, 4.5])
4.4 == median([4.4, 2.3, -1.7, 7.5, 6.6])
4.5 == median([0.0, 1.9, 8.2, 9.3, 4.5])
3.35 == median([4.4, 2.3, -1.7, 7.5])
6.35 == median([1.9, 8.2, 9.3, 4.5])
2.3 == median([4.4, 2.3, -1.7])
8.2 == median([8.2, 9.3, 4.5])
3.35 == median([4.4, 2.3])
6.9 == median([9.3, 4.5])
4.4 == median([4.4])
4.5 == median([4.5])
0 == median([])
0 == median([])
```



## Haskell


This uses a quick select algorithm and runs in expected O(n) time.

```haskell
import Data.List (partition)

nth :: Ord t => [t] -> Int -> t
nth (x:xs) n
  | k == n = x
  | k > n = nth ys n
  | otherwise = nth zs $ n - k - 1
  where
    (ys, zs) = partition (< x) xs
    k = length ys

medianMay :: (Fractional a, Ord a) => [a] -> Maybe a
medianMay xs
  | n < 1 = Nothing
  | even n = Just ((nth xs (div n 2) + nth xs (div n 2 - 1)) / 2.0)
  | otherwise = Just (nth xs (div n 2))
  where
    n = length xs

main :: IO ()
main =
  mapM_
    (printMay . medianMay)
    [[], [7], [5, 3, 4], [5, 4, 2, 3], [3, 4, 1, -8.4, 7.2, 4, 1, 1.2]]
  where
    printMay = maybe (putStrLn "(not defined)") print
```

{{Out}}

```txt
(not defined)
7.0
4.0
3.5
2.1
```


Or {{libheader|hstats}}


```haskell>
 Math.Statistics.median [1,9,2,4]
3.0
```



## HicEst

If the input has an even number of elements, median is the mean of the middle two values:

```HicEst
REAL :: n=10, vec(n)

vec = RAN(1)
SORT(Vector=vec, Sorted=vec) ! in-place Merge-Sort

IF( MOD(n,2) ) THEN  ! odd n
    median = vec( CEILING(n/2) )
ELSE
    median = ( vec(n/2) + vec(n/2 + 1) ) / 2
ENDIF
```


=={{header|Icon}} and {{header|Unicon}}==
A quick and dirty solution:
<lang>procedure main(args)
    write(median(args))
end

procedure median(A)
    A := sort(A)
    n := *A
    return if n % 2 = 1 then A[n/2+1]
           else (A[n/2]+A[n/2+1])/2.0 | 0  # 0 if empty list
end
```


Sample outputs:

```txt
->am 3 1 4 1 5 9 7 6 3
4
->am 3 1 4 1 5 9 7 6
4.5
->
```



## J

The verb <code>median</code> is available from the <code>stats/base</code> addon and returns the mean of the two middle values for an even number of elements:

```j
  require 'stats/base'
  median 1 9 2 4
3
```

The definition given in the addon script is:

```j
midpt=: -:@<:@#
median=: -:@(+/)@((<. , >.)@midpt { /:~)
```


If, for an even number of elements, both values were desired when those two values are distinct, then the following implementation would suffice:

```j
   median=: ~.@(<. , >.)@midpt { /:~
   median 1 9 2 4
2 4
```



## Java

{{works with|Java|1.5+}}

Sorting:

```java5
// Note: this function modifies the input list
public static double median(List<Double> list) {
    Collections.sort(list);
    return (list.get(list.size() / 2) + list.get((list.size() - 1) / 2)) / 2;
}
```


{{works with|Java|1.5+}}

Using priority queue (which sorts under the hood):

```java5
public static double median2(List<Double> list) {
    PriorityQueue<Double> pq = new PriorityQueue<Double>(list);
    int n = list.size();
    for (int i = 0; i < (n - 1) / 2; i++)
        pq.poll(); // discard first half
    if (n % 2 != 0) // odd length
        return pq.poll();
    else
        return (pq.poll() + pq.poll()) / 2.0;
}
```



## JavaScript



### ES5


```javascript
function median(ary) {
    if (ary.length == 0)
        return null;
    ary.sort(function (a,b){return a - b})
    var mid = Math.floor(ary.length / 2);
    if ((ary.length % 2) == 1)  // length is odd
        return ary[mid];
    else
        return (ary[mid - 1] + ary[mid]) / 2;
}

median([]);   // null
median([5,3,4]);  // 4
median([5,4,2,3]);  // 3.5
median([3,4,1,-8.4,7.2,4,1,1.2]);  // 2.1
```



### ES6


Using a quick select algorithm
{{Trans|Haskell}}


```JavaScript
(() => {
    'use strict';

    // median :: [Num] -> Num
    function median(xs) {
        // nth :: [Num] -> Int -> Maybe Num
        let nth = (xxs, n) => {
                if (xxs.length > 0) {
                    let [x, xs] = uncons(xxs),
                        [ys, zs] = partition(y => y < x, xs),
                        k = ys.length;

                    return k === n ? x : (
                        k > n ? nth(ys, n) : nth(zs, n - k - 1)
                    );
                } else return undefined;
            },
            n = xs.length;

        return even(n) ? (
            (nth(xs, div(n, 2)) + nth(xs, div(n, 2) - 1)) / 2
        ) : nth(xs, div(n, 2));
    }



    // GENERIC

    // partition :: (a -> Bool) -> [a] -> ([a], [a])
    let partition = (p, xs) =>
        xs.reduce((a, x) =>
            p(x) ? [a[0].concat(x), a[1]] : [a[0], a[1].concat(x)], [
                [],
                []
            ]),

        // uncons :: [a] -> Maybe (a, [a])
        uncons = xs => xs.length ? [xs[0], xs.slice(1)] : undefined,

        // even :: Integral a => a -> Bool
        even = n => n % 2 === 0,

        // div :: Num -> Num -> Int
        div = (x, y) => Math.floor(x / y);

    return [
        [],
        [5, 3, 4],
        [5, 4, 2, 3],
        [3, 4, 1, -8.4, 7.2, 4, 1, 1.2]
    ].map(median);
})();
```


{{Out}}

```JavaScript
[
  null,
  4,
  3.5,
  2.1
]
```



## jq


```jq
def median:
  length as $length
  | sort as $s
  | if $length == 0 then null
    else ($length / 2 | floor) as $l2
      | if ($length % 2) == 0 then
          ($s[$l2 - 1] + $s[$l2]) / 2
        else $s[$l2]
        end
  end ;
```
This definition can be used in a jq program, but to  illustrate how it can be used as a command line filter, suppose the definition and the program '''median''' are in a file named median.jq, and that the file in.dat contains a sequence of arrays, such as
```sh
[4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2]
[4.1, 7.2, 1.7, 9.3, 4.4, 3.2]
```
Then invoking the jq program yields a stream of values:
```sh
$ jq -f median.jq in.dat
4.4
4.25
```



## Julia

Julia has a built-in median() function

```julia
using Statistics
function median2(n)
	s = sort(n)
	len = length(n)
	if len % 2 == 0
		return (s[floor(Int, len / 2) + 1] + s[floor(Int, len / 2)]) / 2
	else
		return  s[floor(Int, len / 2) + 1]
	end
end

a = [4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2]
b = [4.1, 7.2, 1.7, 9.3, 4.4, 3.2]

@show a b median2(a) median(a) median2(b) median(b)
```


{{out}}

```txt

a = [4.1,5.6,7.2,1.7,9.3,4.4,3.2]
b = [4.1,7.2,1.7,9.3,4.4,3.2]
median2(a) = 4.4
median(a) = 4.4
median2(b) = 4.25
median(b) = 4.25

```



## K


```k

  med:{a:x@<x; i:(#a)%2; :[(#a)!2; a@i; {(+/x)%#x} a@i,i-1]}
  v:10*6 _draw 0
  v
5.961475 2.025856 7.262835 1.814272 2.281911 4.854716
  med[v]
3.568313
  med[1_ v]
2.281911

```


An alternate solution which works in the oK implementation using the same dataset v from above and shows both numbers around the median point on even length datasets would be:

```k

med:{a:x@<x; i:_(#a)%2
$[2!#a; a@i; |a@i,i-1]}
med[v]
2.2819 4.8547

```



## Kotlin

{{works with|Kotlin|1.0+}}

```scala
fun median(l: List<Double>) = l.sorted().let { (it[it.size / 2] + it[(it.size - 1) / 2]) / 2 }

fun main(args: Array<String>) {
    median(listOf(5.0, 3.0, 4.0)).let { println(it) }  // 4
    median(listOf(5.0, 4.0, 2.0, 3.0)).let { println(it) }  // 3.5
    median(listOf(3.0, 4.0, 1.0, -8.4, 7.2, 4.0, 1.0, 1.2)).let { println(it) }  // 2.1
}
```



## Lasso

can't use Lasso's built in median method because
that takes 3 values, not an array of indeterminate length

Lasso's built in function is "median( value_1, value_2, value_3 )"

```Lasso
define median_ext(a::array) => {
	#a->sort

	if(#a->size % 2) => {
		// odd numbered element array, pick middle
		return #a->get(#a->size / 2 + 1)

	else
		// even number elements in array
		return (#a->get(#a->size / 2) + #a->get(#a->size / 2 + 1)) / 2.0
	}
}

median_ext(array(3,2,7,6)) // 4.5
median_ext(array(3,2,9,7,6)) // 6
```



## Liberty BASIC


```lb

    dim a( 100), b( 100)    '   assumes we will not have vectors of more terms...

    a$ ="4.1,5.6,7.2,1.7,9.3,4.4,3.2"
    print "Median is "; median( a$)        '   4.4   7 terms
    print
    a$ ="4.1,7.2,1.7,9.3,4.4,3.2"
    print "Median  is "; median( a$)        '   4.25  6 terms
    print
    a$ ="4.1,4,1.2,6.235,7868.33"   '   4.1
    print "Median of "; a$; " is "; median( a$)
    print
    a$ ="1,5,3,2,4"             '   3
    print "Median of "; a$; " is "; median( a$)
    print
    a$ ="1,5,3,6,4,2"          '   3.5
    print "Median of "; a$; " is "; median( a$)
    print
    a$ ="4.4,2.3,-1.7,7.5,6.6,0.0,1.9,8.2,9.3,4.5" '   4.45
    print "Median of "; a$; " is "; median( a$)

    end

    function median( a$)
        i =1
        do
            v$     =word$( a$, i, ",")
            if v$ ="" then exit do
            print v$,
            a( i)  =val( v$)
            i      =i +1
        loop until 0
        print

        sort a(), 1, i -1

        for j =1 to i -1
            print a( j),
        next j
        print

        middle    =( i -1) /2
        intmiddle =int( middle)
        if middle <>intmiddle then median= a( 1 +intmiddle) else median =( a( intmiddle) +a( intmiddle +1)) /2
    end function

```


```txt

4.1 5.6 7.2 1.7 9.3 4.4 3.2
Median is 4.4

4.1 7.2 1.7 9.3 4.4 3.2
Median is 4.25

4.1 4 1.2 6.235 7868.33
Median of 4.1,4,1.2,6.235,7868.33 is 4.1

1 5 3 2 4
Median of 1,5,3,2,4 is 3

1 5 3 6 4 2
Median of 1,5,3,6,4,2 is 3.5

4.4 2.3 -1.7 7.5 6.6 0.0 1.9 8.2 9.3 4.5
Median of 4.4,2.3,-1.7,7.5,6.6,0.0,1.9,8.2,9.3,4.5 is 4.45

```



## Lingo


```Lingo
on median (numlist)
    -- numlist = numlist.duplicate() -- if input list should not be altered
    numlist.sort()
    if numlist.count mod 2 then
        return numlist[numlist.count/2+1]
    else
        return (numlist[numlist.count/2]+numlist[numlist.count/2+1])/2.0
    end if
end
```



## LiveCode

LC has median as a built-in function

```LiveCode
put median("4.1,5.6,7.2,1.7,9.3,4.4,3.2") & "," & median("4.1,7.2,1.7,9.3,4.4,3.2")
returns 4.4, 4.25
```


To make our own, we need own own floor function first


```LiveCode
function floor n
    if n < 0 then
        return (trunc(n) - 1)
    else
        return trunc(n)
    end if
end floor

function median2 x
    local n, m
    set itemdelimiter to comma
    sort items of x ascending numeric
    put the number of items of x into n
    put floor(n / 2) into m
    if n mod 2 is 0 then
        return (item m of x + item (m + 1) of x) / 2
    else
        return item (m + 1) of x
    end if
end median2

returns the same as the built-in median, viz.
put median2("4.1,5.6,7.2,1.7,9.3,4.4,3.2") & "," & median2("4.1,7.2,1.7,9.3,4.4,3.2")
4.4,4.25
```



## LSL


```LSL
integer MAX_ELEMENTS = 10;
integer MAX_VALUE = 100;
default {
    state_entry() {
        list lst = [];
        integer x = 0;
        for(x=0 ; x<MAX_ELEMENTS ; x++) {
            lst += llFrand(MAX_VALUE);
        }
        llOwnerSay("lst=["+llList2CSV(lst)+"]");
        llOwnerSay("Geometric Mean: "+(string)llListStatistics(LIST_STAT_GEOMETRIC_MEAN, lst));
        llOwnerSay("           Max: "+(string)llListStatistics(LIST_STAT_MAX, lst));
        llOwnerSay("          Mean: "+(string)llListStatistics(LIST_STAT_MEAN, lst));
        llOwnerSay("        Median: "+(string)llListStatistics(LIST_STAT_MEDIAN, lst));
        llOwnerSay("           Min: "+(string)llListStatistics(LIST_STAT_MIN, lst));
        llOwnerSay("     Num Count: "+(string)llListStatistics(LIST_STAT_NUM_COUNT, lst));
        llOwnerSay("         Range: "+(string)llListStatistics(LIST_STAT_RANGE, lst));
        llOwnerSay("       Std Dev: "+(string)llListStatistics(LIST_STAT_STD_DEV, lst));
        llOwnerSay("           Sum: "+(string)llListStatistics(LIST_STAT_SUM, lst));
        llOwnerSay("   Sum Squares: "+(string)llListStatistics(LIST_STAT_SUM_SQUARES, lst));
    }
}
```

Output:

```txt

lst=[23.815209, 85.890704, 10.811144, 31.522696, 54.619416, 12.211729, 42.964463, 87.367889, 7.106129, 18.711078]
Geometric Mean:    27.325070
           Max:    87.367889
          Mean:    37.502046
        Median:    27.668953
           Min:     7.106129
     Num Count:    10.000000
         Range:    80.261761
       Std Dev:    29.819840
           Sum:   375.020458
   Sum Squares: 22067.040048

```



## Lua


```lua
function median (numlist)
    if type(numlist) ~= 'table' then return numlist end
    table.sort(numlist)
    if #numlist %2 == 0 then return (numlist[#numlist/2] + numlist[#numlist/2+1]) / 2 end
    return numlist[math.ceil(#numlist/2)]
end

print(median({4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2}))
print(median({4.1, 7.2, 1.7, 9.3, 4.4, 3.2}))
```



## Maple


###  Builtin

This works for numeric lists or arrays, and is designed for large data sets.

```Maple

> Statistics:-Median( [ 1, 5, 3, 2, 4 ] );
                                   3.

> Statistics:-Median( [ 1, 5, 3, 6, 2, 4 ] );
                            3.50000000000000

```


###  Using a sort

This solution can handle exact numeric inputs.  Instead of inputting a container of some kind, it simply finds the median of its arguments.

```Maple

median1 := proc()
        local L := sort( [ args ] );
        ( L[ iquo( 1 + nargs, 2 ) ] + L[ 1 + iquo( nargs, 2 ) ] ) / 2
end proc:

```

For example:

```Maple

> median1( 1, 5, 3, 2, 4 ); # 3
                                   3

> median1( 1, 5, 3, 6, 4, 2 ); # 7/2
                                  7/2

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Built-in function:

```Mathematica
Median[{1, 5, 3, 2, 4}]
Median[{1, 5, 3, 6, 4, 2}]
```

{{out}}

```txt
3
7/2
```

Custom function:

```Mathematica
mymedian[x_List]:=Module[{t=Sort[x],L=Length[x]},
 If[Mod[L,2]==0,
  (t[[L/2]]+t[[L/2+1]])/2
 ,
  t[[(L+1)/2]]
 ]
]
```

Example of custom function:

```Mathematica
mymedian[{1, 5, 3, 2, 4}]
mymedian[{1, 5, 3, 6, 4, 2}]
```

{{out}}

```txt
3
7/2
```



## MATLAB

If the input has an even number of elements, function returns the mean of the middle two values:

```Matlab
function medianValue = findmedian(setOfValues)
   medianValue = median(setOfValues);
end
```


## Maxima


```maxima
/* built-in */
median([41, 56, 72, 17, 93, 44, 32]);  /* 44 */
median([41, 72, 17, 93, 44, 32]);      /* 85/2 */
```



## MUMPS


```MUMPS
MEDIAN(X)
 ;X is assumed to be a list of numbers separated by "^"
 ;I is a loop index
 ;L is the length of X
 ;Y is a new array
 QUIT:'$DATA(X) "No data"
 QUIT:X="" "Empty Set"
 NEW I,ODD,L,Y
 SET L=$LENGTH(X,"^"),ODD=L#2,I=1
 ;The values in the vector are used as indices for a new array Y, which sorts them
 FOR  QUIT:I>L  SET Y($PIECE(X,"^",I))=1,I=I+1
 ;Go to the median index, or the lesser of the middle if there is an even number of elements
 SET J="" FOR I=1:1:$SELECT(ODD:L\2+1,'ODD:L/2) SET J=$ORDER(Y(J))
 QUIT $SELECT(ODD:J,'ODD:(J+$ORDER(Y(J)))/2)

```


```txt
USER>W $$MEDIAN^ROSETTA("-1.3^2.43^3.14^17^2E-3")
3.14
USER>W $$MEDIAN^ROSETTA("-1.3^2.43^3.14^17^2E-3^4")
3.57
USER>W $$MEDIAN^ROSETTA("")
Empty Set
USER>W $$MEDIAN^ROSETTA
No data
```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

class RAvgMedian00 public

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method median(lvector = java.util.List) public static returns Rexx
    cvector = ArrayList(lvector) -- make a copy of input to ensure it's contents are preserved
    Collections.sort(cvector, RAvgMedian00.RexxComparator())
    kVal = ((Rexx cvector.get(cvector.size() % 2)) + (Rexx cvector.get((cvector.size() - 1) % 2))) / 2
    return kVal

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method median(rvector = Rexx[]) public static returns Rexx
    return median(ArrayList(Arrays.asList(rvector)))

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method show_median(lvector = java.util.List) public static returns Rexx
    mVal = median(lvector)
    say 'Meadian:' mVal.format(10, 6, 3, 6, 's')', Vector:' (Rexx lvector).space(0)
    return mVal

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method show_median(rvector = Rexx[]) public static returns Rexx
    return show_median(ArrayList(Arrays.asList(rvector)))

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method run_samples() public static
    show_median([Rexx 10.0])                                                   -- 10.0
    show_median([Rexx 10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0])      -- 5.5
    show_median([Rexx 9, 8, 7, 6, 5, 4, 3, 2, 1])                              -- 5.0
    show_median([Rexx 1.0, 9, 2.0, 4.0])                                       -- 3.0
    show_median([Rexx 3.0, 1, 4, 1.0, 5.0, 9, 7.0, 6.0])                       -- 4.5
    show_median([Rexx 3, 4, 1, -8.4, 7.2, 4, 1, 1.2])                          -- 2.1
    show_median([Rexx -1.2345678e+99, 2.3e+700])                               -- 1.15e+700
    show_median([Rexx 4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2])                      -- 4.4
    show_median([Rexx 4.1, 7.2, 1.7, 9.3, 4.4, 3.2])                           -- 4.25
    show_median([Rexx 28.207, 74.916, 51.695, 72.486, 51.118, 3.241, 73.807])  -- 51.695
    show_median([Rexx 27.984, 89.172, 0.250, 66.316, 41.805, 60.043])          -- 50.924
    show_median([Rexx 5.1, 2.6, 6.2, 8.8, 4.6, 4.1])                           -- 4.85
    show_median([Rexx 5.1, 2.6, 8.8, 4.6, 4.1])                                -- 4.6
    show_median([Rexx 4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5])      -- 4.45
    show_median([Rexx 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0.11])        -- 3.0
    show_median([Rexx 10, 20, 30, 40, 50, -100, 4.7, -11e+2])                  -- 15.0
    show_median([Rexx 9.3, -2.0, 4.0, 7.3, 8.1, 4.1, -6.3, 4.2, -1.0, -8.4])   -- 4.05
    show_median([Rexx 8.3, -3.6, 5.7, 2.3, 9.3, 5.4, -2.3, 6.3, 9.9])          -- 5.7
    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    run_samples()
    return

--
### =======================================================================

class RAvgMedian00.RexxComparator implements Comparator

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method compare(i1=Object, i2=Object) public returns int
    i = Rexx i1
    j = Rexx i2

    if i < j then return -1
    if i > j then return +1
    else return 0

```

'''Output:'''

```txt

Meadian:         10.000000     , Vector: [10.0]
Meadian:          5.500000     , Vector: [10.0,9.0,8.0,7.0,6.0,5.0,4.0,3.0,2.0,1.0]
Meadian:          5.000000     , Vector: [9,8,7,6,5,4,3,2,1]
Meadian:          3.000000     , Vector: [1.0,9,2.0,4.0]
Meadian:          4.500000     , Vector: [3.0,1,4,1.0,5.0,9,7.0,6.0]
Meadian:          2.100000     , Vector: [3,4,1,-8.4,7.2,4,1,1.2]
Meadian:          1.150000E+700, Vector: [-1.2345678E+99,2.3e+700]
Meadian:          4.400000     , Vector: [4.1,5.6,7.2,1.7,9.3,4.4,3.2]
Meadian:          4.250000     , Vector: [4.1,7.2,1.7,9.3,4.4,3.2]
Meadian:         51.695000     , Vector: [28.207,74.916,51.695,72.486,51.118,3.241,73.807]
Meadian:         50.924000     , Vector: [27.984,89.172,0.250,66.316,41.805,60.043]
Meadian:          4.850000     , Vector: [5.1,2.6,6.2,8.8,4.6,4.1]
Meadian:          4.600000     , Vector: [5.1,2.6,8.8,4.6,4.1]
Meadian:          4.450000     , Vector: [4.4,2.3,-1.7,7.5,6.6,0.0,1.9,8.2,9.3,4.5]
Meadian:          3.000000     , Vector: [10,9,8,7,6,5,4,3,2,1,0,0,0,0,0.11]
Meadian:         15.000000     , Vector: [10,20,30,40,50,-100,4.7,-1100]
Meadian:          4.050000     , Vector: [9.3,-2.0,4.0,7.3,8.1,4.1,-6.3,4.2,-1.0,-8.4]
Meadian:          5.700000     , Vector: [8.3,-3.6,5.7,2.3,9.3,5.4,-2.3,6.3,9.9]

```



## NewLISP


```NewLISP
; median.lsp
; oofoe 2012-01-25

(define (median lst)
  (sort lst) ; Sorts in place.
  (if (empty? lst)
      nil
    (letn ((n (length lst))
           (h (/ (- n 1) 2)))
          (if (zero? (mod n 2))
              (div (add (lst h) (lst (+ h 1))) 2)
            (lst h))
          )))


(define (test lst) (println lst " -> " (median lst)))

(test '())
(test '(5 3 4))
(test '(5 4 2 3))
(test '(3 4 1 -8.4 7.2 4 1 1.2))

(exit)
```


Sample output:


```txt
() -> nil
(5 3 4) -> 4
(5 4 2 3) -> 3.5
(3 4 1 -8.4 7.2 4 1 1.2) -> 2.1

```



## Nim

{{trans|Python}}

```nim
import algorithm, strutils

proc median(xs: seq[float]): float =
  var ys = xs
  sort(ys, system.cmp[float])
  0.5 * (ys[ys.high div 2] + ys[ys.len div 2])

var a = @[4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2]
echo formatFloat(median(a), precision = 0)
a = @[4.1, 7.2, 1.7, 9.3, 4.4, 3.2]
echo formatFloat(median(a), precision = 0)
```


Example Output:
 4.4
 4.25

=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE Median;
IMPORT Out;
CONST
	MAXSIZE = 100;

PROCEDURE Partition(VAR a: ARRAY OF REAL; left, right: INTEGER): INTEGER;
VAR
	pValue,aux: REAL;
	store,i,pivot: INTEGER;
BEGIN
	pivot := right;
	pValue := a[pivot];
	aux := a[right];a[right] := a[pivot];a[pivot] := aux; (* a[pivot] <-> a[right] *)
	store := left;
	FOR i := left TO right -1 DO
		IF a[i] <= pValue THEN
			aux := a[store];a[store] := a[i];a[i]:=aux;
			INC(store)
		END
	END;
	aux := a[right];a[right] := a[store]; a[store] := aux;
	RETURN store
END Partition;

(* QuickSelect algorithm *)
PROCEDURE Select(a: ARRAY OF REAL; left,right,k: INTEGER;VAR r: REAL);
VAR
	pIndex, pDist : INTEGER;
BEGIN
	IF left = right THEN r := a[left]; RETURN END;
	pIndex := Partition(a,left,right);
	pDist := pIndex - left + 1;
	IF pDist = k THEN
		r := a[pIndex];RETURN
	ELSIF k < pDist THEN
		Select(a,left, pIndex - 1, k, r)
	ELSE
		Select(a,pIndex + 1, right, k - pDist, r)
	END
END Select;

PROCEDURE Median(a: ARRAY OF REAL;left,right: INTEGER): REAL;
VAR
	idx,len : INTEGER;
	r1,r2 : REAL;
BEGIN
	len := right - left + 1;
	idx := len DIV 2 + 1;
	r1 := 0.0;r2 := 0.0;
	Select(a,left,right,idx,r1);
	IF ODD(len) THEN RETURN r1 END;
	Select(a,left,right,idx - 1,r2);
	RETURN (r1 + r2) / 2;
END Median;


VAR
	ary: ARRAY MAXSIZE OF REAL;
	r: REAL;
BEGIN
	r := 0.0;
	Out.Fixed(Median(ary,0,0),4,2);Out.Ln;	(* empty *)
	ary[0] := 5;
	ary[1] := 3;
	ary[2] := 4;
	Out.Fixed(Median(ary,0,2),4,2);Out.Ln;
	ary[0] := 5;
	ary[1] := 4;
	ary[2] := 2;
	ary[3] := 3;
	Out.Fixed(Median(ary,0,3),4,2);Out.Ln;
	ary[0] := 3;
	ary[1] := 4;
	ary[2] := 1;
	ary[3] := -8.4;
	ary[4] := 7.2;
	ary[5] := 4;
	ary[6] := 1;
	ary[7] := 1.2;
	Out.Fixed(Median(ary,0,7),4,2);Out.Ln;
END Median.

```

Output:

```txt

0.00
4.00
3.50
2.10

```



## Objeck


```objeck

use Structure;

bundle Default {
  class Median {
    function : Main(args : String[]) ~ Nil {
      numbers := FloatVector->New([4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2]);
      DoMedian(numbers)->PrintLine();

      numbers := FloatVector->New([4.1, 7.2, 1.7, 9.3, 4.4, 3.2]);
      DoMedian(numbers)->PrintLine();
    }

    function : native : DoMedian(numbers : FloatVector) ~ Float {
      if(numbers->Size() = 0) {
        return 0.0;
      }
      else if(numbers->Size() = 1) {
        return numbers->Get(0);
      };

      numbers->Sort();

      i := numbers->Size() / 2;
      if(numbers->Size() % 2 = 0) {
        return (numbers->Get(i - 1) + numbers->Get(i)) / 2.0;
      };

      return numbers->Get(i);
    }
  }
}

```



## OCaml


```ocaml
(* note: this modifies the input array *)
let median array =
  let len = Array.length array in
    Array.sort compare array;
    (array.((len-1)/2) +. array.(len/2)) /. 2.0;;

let a = [|4.1; 5.6; 7.2; 1.7; 9.3; 4.4; 3.2|];;
median a;;
let a = [|4.1; 7.2; 1.7; 9.3; 4.4; 3.2|];;
median a;;
```



## Octave

Of course Octave has its own <tt>median</tt> function we can use to check our implementation. The Octave's median function, however, does not handle the case you pass in a void vector.

```octave
function y = median2(v)
  if (numel(v) < 1)
    y = NA;
  else
    sv = sort(v);
    l = numel(v);
    if ( mod(l, 2) == 0 )
      y = (sv(floor(l/2)+1) + sv(floor(l/2)))/2;
    else
      y = sv(floor(l/2)+1);
    endif
  endif
endfunction

a = [4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2];
b = [4.1, 7.2, 1.7, 9.3, 4.4, 3.2];

disp(median2(a))   % 4.4
disp(median(a))
disp(median2(b))   % 4.25
disp(median(b))
```



## ooRexx


```ooRexx

call testMedian .array~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
call testMedian .array~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11)
call testMedian .array~of(10, 20, 30, 40, 50, -100, 4.7, -11e2)
call testMedian .array~new

::routine testMedian
  use arg numbers
  say "numbers =" numbers~toString("l", ", ")
  say "median =" median(numbers)
  say

::routine median
  use arg numbers

  if numbers~isempty then return 0
  -- make a copy so the sort does not alter the
  -- original set.  This also means this will
  -- work with lists and queues as well
  numbers = numbers~makearray

  -- sort and return the middle element
  numbers~sortWith(.numbercomparator~new)
  size = numbers~items
  -- this handles the odd value too
  return numbers[size%2 + size//2]


-- a custom comparator that sorts strings as numeric values rather than
-- strings
::class numberComparator subclass comparator
::method compare
  use strict arg left, right
  -- perform the comparison on the names.  By subtracting
  -- the two and returning the sign, we give the expected
  -- results for the compares
  return (left - right)~sign

```



## Oz


```oz
declare
  fun {Median Xs}
     Len = {Length Xs}
     Mid = Len div 2 + 1 %% 1-based index
     Sorted = {Sort Xs Value.'<'}
  in
     if {IsOdd Len} then {Nth Sorted Mid}
     else ({Nth Sorted Mid} + {Nth Sorted Mid-1}) / 2.0
     end
  end
in
  {Show {Median [4.1 5.6 7.2 1.7 9.3 4.4 3.2]}}
  {Show {Median [4.1 7.2 1.7 9.3 4.4 3.2]}}
```



## PARI/GP

Sorting solution.

```parigp
median(v)={
  vecsort(v)[#v\2]
};
```


Linear-time solution, mostly proof-of-concept but perhaps suitable for large lists.

```parigp
BFPRT(v,k=#v\2)={
	if(#v<15, return(vecsort(v)[k]));
	my(u=List(),pivot,left=List(),right=List());
	forstep(i=1,#v-4,5,
		listput(u,BFPRT([v[i],v[i+1],v[i+2],v[i+3],v[i+4]]))
	);
	pivot=BFPRT(Vec(u));
	u=0;
	for(i=1,#v,
		if(v[i]<pivot,
			listput(left,v[i])
		,
			listput(right,v[i])
		)
	);
	if(k>#left,
		BFPRT(right, k-#left)
	,
		BFPRT(left, k)
	)
};
```



## Pascal

{{works with|Free_Pascal}}

```pascal
Program AveragesMedian(output);

type
  TDoubleArray = array of double;

procedure bubbleSort(var list: TDoubleArray);
var
  i, j, n: integer;
  t: double;
begin
  n := length(list);
  for i := n downto 2 do
    for j := 0 to i - 1 do
      if list[j] > list[j + 1] then
      begin
        t := list[j];
        list[j] := list[j + 1];
        list[j + 1] := t;
      end;
end;

function Median(aArray: TDoubleArray): double;
var
  lMiddleIndex: integer;
begin
  bubbleSort(aArray);
  lMiddleIndex := (high(aArray) - low(aArray)) div 2;
  if Odd(Length(aArray)) then
    Median := aArray[lMiddleIndex + 1]
  else
    Median := (aArray[lMiddleIndex + 1] + aArray[lMiddleIndex]) / 2;
end;

var
  A: TDoubleArray;
  i: integer;

begin
  randomize;
  setlength(A, 7);
  for i := low(A) to high(A) do
  begin
    A[i] := 100 * random;
    write (A[i]:7:3, ' ');
  end;
  writeln;
  writeln('Median: ', Median(A):7:3);

  setlength(A, 6);
  for i := low(A) to high(A) do
  begin
    A[i] := 100 * random;
    write (A[i]:7:3, ' ');
  end;
  writeln;
  writeln('Median: ', Median(A):7:3);
end.
```

Output:

```txt
% ./Median
 28.207  74.916  51.695  72.486  51.118   3.241  73.807
Median:  51.695
 27.984  89.172   0.250  66.316  41.805  60.043
Median:  50.924

```



## Perl

{{trans|Python}}

```perl
sub median {
  my @a = sort {$a <=> $b} @_;
  return ($a[$#a/2] + $a[@a/2]) / 2;
}
```



## Perl 6


{{works with|Rakudo|2016.08}}

```perl6
sub median {
  my @a = sort @_;
  return (@a[(*-1) div 2] + @a[* div 2]) / 2;
}
```


Notes:

* The <tt>div</tt> operator does integer division. The <tt>/</tt> operator (rational number division) would work too, since the array subscript automatically coerces to <tt>Int</tt>, but using <tt>div</tt> is more explicit (i.e. clearer to readers) as well as faster, and thus recommended in cases like this.
* The <tt>*</tt> inside the subscript stands for the array's length ([https://docs.perl6.org/language/subscripts.html#From_the_end see documentation]).



In a slightly more compact way:

```perl6
sub median { @_.sort[(*-1)/2, */2].sum / 2 }
```



## Phix

The obvious simple way:

```Phix
function median(sequence s)
atom res=0
integer l = length(s), k = floor((l+1)/2)
    if l then
        s = sort(s)
        res = s[k]
        if remainder(l,2)=0 then
            res = (res+s[k+1])/2
        end if
    end if
    return res
end function
```

It is also possible to use the [[Quickselect_algorithm#Phix|quick_select]] routine for a small (20%) performance improvement,
which as suggested below may with luck be magnified by retaining any partially sorted results.

```Phix
function medianq(sequence s)
atom res=0, tmp
integer l = length(s), k = floor((l+1)/2)
    if l then
        {s,res} = quick_select(s,k)
        if remainder(l,2)=0 then
            {s,tmp} = quick_select(s,k+1)
            res = (res+tmp)/2
        end if
    end if
    return res  -- (or perhaps return {s,res})
end function
```



## PHP

This solution uses the sorting method of finding the median.

```php

function median($arr)
{
    sort($arr);
    $count = count($arr); //count the number of values in array
    $middleval = floor(($count-1)/2); // find the middle value, or the lowest middle value
    if ($count % 2) { // odd number, middle is the median
        $median = $arr[$middleval];
    } else { // even number, calculate avg of 2 medians
        $low = $arr[$middleval];
        $high = $arr[$middleval+1];
        $median = (($low+$high)/2);
    }
    return $median;
}

echo median(array(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)) . "\n";  // 4.4
echo median(array(4.1, 7.2, 1.7, 9.3, 4.4, 3.2)) . "\n";       // 4.25

```



## PicoLisp


```PicoLisp
(de median (Lst)
   (let N (length Lst)
      (if (bit? 1 N)
         (get (sort Lst) (/ (inc N) 2))
         (setq Lst (nth (sort Lst) (/ N 2)))
         (/ (+ (car Lst) (cadr Lst)) 2) ) ) )

(scl 2)
(prinl (round (median (1.0 2.0 3.0))))
(prinl (round (median (1.0 2.0 3.0 4.0))))
(prinl (round (median (5.1 2.6 6.2 8.8 4.6 4.1))))
(prinl (round (median (5.1 2.6 8.8 4.6 4.1))))
```

Output:

```txt
2.00
2.50
4.85
4.60
```



## PL/I


```pli
call sort(A);
n = dimension(A,1);
if iand(n,1) = 1 then /* an odd number of elements */
   median = A(n/2);
else                  /* an even number of elements */
   median = (a(n/2) + a(trunc(n/2)+1) )/2;
```




## PowerShell

This function returns an object containing the minimal amount of statistical data, including Median, and could be modified to take input directly from the pipeline.

All statistical properties could easily be added to the output object.


```PowerShell

function Measure-Data
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   Position=0)]
        [double[]]
        $Data
    )

    Begin
    {
        function Get-Mode ([double[]]$Data)
        {
            if ($Data.Count -gt ($Data | Select-Object -Unique).Count)
            {
                $groups = $Data | Group-Object | Sort-Object -Property Count -Descending

                return ($groups | Where-Object {[double]$_.Count -eq [double]$groups[0].Count}).Name | ForEach-Object {[double]$_}
            }
            else
            {
                return $null
            }
        }

        function Get-StandardDeviation ([double[]]$Data)
        {
            $variance = 0
            $average  = $Data | Measure-Object -Average | Select-Object -Property Count, Average

            foreach ($number in $Data)
            {
                $variance +=  [Math]::Pow(($number - $average.Average),2)
            }

            return [Math]::Sqrt($variance / ($average.Count-1))
        }

        function Get-Median ([double[]]$Data)
        {
            if ($Data.Count % 2)
            {
                return $Data[[Math]::Floor($Data.Count/2)]
            }
            else
            {
                return ($Data[$Data.Count/2], $Data[$Data.Count/2-1] | Measure-Object -Average).Average
            }
        }
    }
    Process
    {
        $Data = $Data | Sort-Object

        $Data | Measure-Object -Maximum -Minimum -Sum -Average |
                Select-Object -Property Count,
                                        Sum,
                                        Minimum,
                                        Maximum,
                                        @{Name='Range'; Expression={$_.Maximum - $_.Minimum}},
                                        @{Name='Mean' ; Expression={$_.Average}} |
                Add-Member -MemberType NoteProperty -Name Median            -Value (Get-Median $Data)            -PassThru |
                Add-Member -MemberType NoteProperty -Name StandardDeviation -Value (Get-StandardDeviation $Data) -PassThru |
                Add-Member -MemberType NoteProperty -Name Mode              -Value (Get-Mode $Data)              -PassThru
    }
}

```


```PowerShell

$statistics = Measure-Data 4, 5, 6, 7, 7, 7, 8, 1, 1, 1, 2, 3
$statistics

```

{{Out}}

```txt

Count             : 12
Sum               : 52
Minimum           : 1
Maximum           : 8
Range             : 7
Mean              : 4.33333333333333
Median            : 4.5
StandardDeviation : 2.67423169368609
Mode              : {1, 7}

```

Median only:

```PowerShell

$statistics.Median

```

{{Out}}

```txt

4.5

```



## Prolog


```Prolog
median(L, Z) :-
    length(L, Length),
    I is Length div 2,
    Rem is Length rem 2,
    msort(L, S),
    maplist(sumlist, [[I, Rem], [I, 1]], Mid),
    maplist(nth1, Mid, [S, S], X),
    sumlist(X, Y),
    Z is Y/2.
```



## Pure

Inspired by the Haskell version.

```Pure
median x = (/(2-rem)) $ foldl1 (+) $ take (2-rem) $ drop (mid-(1-rem)) $ sort (<=) x
    when len = # x;
         mid = len div 2;
         rem = len mod 2;
         end;
```

Output:
```txt
> median [1, 3, 5];
3.0
> median [1, 2, 3, 4];
2.5

```



## PureBasic


```PureBasic
Procedure.d median(Array values.d(1), length.i)
  If length = 0 : ProcedureReturn 0.0 : EndIf
  SortArray(values(), #PB_Sort_Ascending)
  If length % 2
    ProcedureReturn values(length / 2)
  EndIf
  ProcedureReturn 0.5 * (values(length / 2 - 1) + values(length / 2))
EndProcedure

Procedure.i readArray(Array values.d(1))
  Protected length.i, i.i
  Read.i length
  ReDim values(length - 1)
  For i = 0 To length - 1
    Read.d values(i)
  Next
  ProcedureReturn i
EndProcedure

Dim floats.d(0)
Restore array1
length.i = readArray(floats())
Debug median(floats(), length)
Restore array2
length.i = readArray(floats())
Debug median(floats(), length)

DataSection
  array1:
    Data.i 7
    Data.d 4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2
  array2:
    Data.i 6
    Data.d 4.1, 7.2, 1.7, 9.3, 4.4, 3.2
EndDataSection
```



## Python


```python
def median(aray):
    srtd = sorted(aray)
    alen = len(srtd)
    return 0.5*( srtd[(alen-1)//2] + srtd[alen//2])

a = (4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)
print a, median(a)
a = (4.1, 7.2, 1.7, 9.3, 4.4, 3.2)
print a, median(a)
```



## R

R has its built-in median function.
{{trans|Octave}}


```rsplus
omedian <- function(v) {
  if ( length(v) < 1 )
    NA
  else {
    sv <- sort(v)
    l <- length(sv)
    if ( l %% 2 == 0 )
      (sv[floor(l/2)+1] + sv[floor(l/2)])/2
    else
      sv[floor(l/2)+1]
  }
}

a <- c(4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)
b <- c(4.1, 7.2, 1.7, 9.3, 4.4, 3.2)

print(median(a))   # 4.4
print(omedian(a))
print(median(b))   # 4.25
print(omedian(b))
```



## Racket


```Racket
#lang racket
(define (median numbers)
  (define sorted (list->vector (sort (vector->list numbers) <)))
  (define count (vector-length numbers))
  (if (zero? count)
      #f
      (/ (+ (vector-ref sorted (floor (/ (sub1 count) 2)))
            (vector-ref sorted (floor (/ count 2))))
         2)))

(median '#(5 3 4)) ;; 4
(median '#()) ;; #f
(median '#(5 4 2 3)) ;; 7/2
(median '#(3 4 1 -8.4 7.2 4 1 1.2)) ;; 2.1
```



## REBOL


```rebol

median: func [
    "Returns the midpoint value in a series of numbers; half the values are above, half are below."
    block [any-block!]
    /local len mid
][
    if empty? block [return none]
    block: sort copy block
    len: length? block
    mid: to integer! len / 2
    either odd? len [
        pick block add 1 mid
    ][
        (block/:mid) + (pick block add 1 mid) / 2
    ]
]

```




## REXX


```rexx
/*REXX program finds the  median  of a  vector  (and displays the  vector  and  median).*/
/*  ══════════vector════════════   ══show vector═══   ════════show result═══════════    */
    v=  1 9 2 4                ;   say "vector"  v;   say 'median──────►' median(v);   say
    v=  3 1 4 1 5 9 7 6        ;   say "vector"  v;   say 'median──────►' median(v);   say
    v= '3 4 1 -8.4 7.2 4 1 1.2';   say "vector"  v;   say 'median──────►' median(v);   say
    v=  -1.2345678e99  2.3e700 ;   say "vector"  v;   say 'median──────►' median(v);   say
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eSORT:  procedure expose @. #;     parse arg $;     #= words($)   /*$:  is the  vector. */
                do g=1  for #;   @.g= word($, g);   end  /*g*/    /*convert list──►array*/
        h=#                                                       /*#:  number elements.*/
                do  while  h>1;             h= h % 2              /*cut entries by half.*/
                   do i=1  for #-h;         j= i;        k= h + i /*sort lower section. */
                      do  while @.k<@.j;    parse value  @.j @.k  with  @.k @.j  /*swap.*/
                      if h>=j  then leave;  j= j - h;    k= k - h /*diminish  J  and  K.*/
                      end   /*while @.k<@.j*/
                   end      /*i*/
                end         /*while h>1*/                         /*end of exchange sort*/
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
median: procedure; call eSORT arg(1);   m= # % 2    /*   %   is REXX's integer division.*/
        n= m + 1                                    /*N:     the next element after  M. */
        if # // 2  then return @.n                  /*[odd?]   // ◄───REXX's ÷ remainder*/
                        return (@.m + @.n) / 2      /*process an  even─element  vector. */
```

{{out|output}}

```txt

vector: 1 9 2 4
median──────► 3

vector: 3 1 4 1 5 9 7 6
median──────► 4.5

vector: 3 4 1 -8.4 7.2 4 1 1.2
median──────► 2.1

vector: -1.2345678e99  2.3e700
median──────► 1.15000000E+700

```



## Ring


```ring

aList = [5,4,2,3]
see "medium : " + median(aList) + nl

func median aray
     srtd = sort(aray)
     alen = len(srtd)
     if alen % 2 = 0
        return (srtd[alen/2] + srtd[alen/2 + 1]) / 2.0
     else return srtd[ceil(alen/2)] ok

```



## Ruby


```ruby
def median(ary)
  return nil if ary.empty?
  mid, rem = ary.length.divmod(2)
  if rem == 0
    ary.sort[mid-1,2].inject(:+) / 2.0
  else
    ary.sort[mid]
  end
end

p median([])                        # => nil
p median([5,3,4])                   # => 4
p median([5,4,2,3])                 # => 3.5
p median([3,4,1,-8.4,7.2,4,1,1.2])  # => 2.1
```


Alternately:

```ruby
def median(aray)
    srtd = aray.sort
    alen = srtd.length
    (srtd[(alen-1)/2] + srtd[alen/2]) / 2.0
end
```



## Run BASIC


```Runbasic
sqliteconnect #mem, ":memory:"
mem$ = "CREATE TABLE med (x float)"
#mem execute(mem$)

a$ ="4.1,5.6,7.2,1.7,9.3,4.4,3.2"	:gosub [median]
a$ ="4.1,7.2,1.7,9.3,4.4,3.2"		:gosub [median]
a$ ="4.1,4,1.2,6.235,7868.33"  		:gosub [median]
a$ ="1,5,3,2,4"       			:gosub [median]
a$ ="1,5,3,6,4,2"       		:gosub [median]
a$ ="4.4,2.3,-1.7,7.5,6.6,0.0,1.9,8.2,9.3,4.5"   :gosub [median]'
end
[median]
#mem execute("DELETE FROM med")
for i = 1 to 100
	v$	= word$( a$, i, ",")
	if v$ = "" then exit for
	mem$	= "INSERT INTO med values(";v$;")"
	#mem execute(mem$)
next i
mem$ = "SELECT AVG(x) as median FROM (SELECT x FROM med
ORDER BY x LIMIT 2 - (SELECT COUNT(*) FROM med) % 2
OFFSET (SELECT (COUNT(*) - 1) / 2
FROM med))"

#mem	execute(mem$)
	#row 	= #mem #nextrow()
	median	= #row median()
print " Median :";median;chr$(9);" Values:";a$

RETURN
```
Output:

```txt
Median :4.4	 Values:4.1,5.6,7.2,1.7,9.3,4.4,3.2
 Median :4.25	 Values:4.1,7.2,1.7,9.3,4.4,3.2
 Median :4.1	 Values:4.1,4,1.2,6.235,7868.33
 Median :3.0	 Values:1,5,3,2,4
 Median :3.5	 Values:1,5,3,6,4,2
 Median :4.45	 Values:4.4,2.3,-1.7,7.5,6.6,0.0,1.9,8.2,9.3,4.5
```



## Rust


Sorting, then obtaining the median element:


```rust
fn median(mut xs: Vec<f64>) -> f64 {
    // sort in ascending order, panic on f64::NaN
    xs.sort_by(|x,y| x.partial_cmp(y).unwrap() );
    let n = xs.len();
    if n % 2 == 0 {
        (xs[n/2] + xs[n/2 - 1]) / 2.0
    } else {
        xs[n/2]
    }
}

fn main() {
    let nums = vec![2.,3.,5.,0.,9.,82.,353.,32.,12.];
    println!("{:?}", median(nums))
}
```


{{out}}

```txt
9
```



## Scala

{{works with|Scala|2.8}} (See the Scala discussion on [[Mean]] for more information.)


```scala
def median[T](s: Seq[T])(implicit n: Fractional[T]) = {
  import n._
  val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
  if (s.size % 2 == 0) (lower.last + upper.head) / fromInt(2) else upper.head
}
```


This isn't really optimal. The methods <tt>splitAt</tt> and <tt>last</tt> are O(n/2)
on many sequences, and then there's the lower bound imposed by the sort. Finally,
we call <tt>size</tt> two times, and it can be O(n).


## Scheme

{{trans|Python}}
Using Rosetta Code's [[Bubble_Sort#Scheme|bubble-sort function]]

```Scheme
(define (median l)
  (* (+ (list-ref (bubble-sort l >) (round (/ (- (length l) 1) 2)))
        (list-ref (bubble-sort l >) (round (/ (length l) 2)))) 0.5))
```


Using [http://srfi.schemers.org/srfi-95/srfi-95.html SRFI-95]:

```Scheme
(define (median l)
  (* (+ (list-ref (sort l less?) (round (/ (- (length l) 1) 2)))
        (list-ref (sort l less?) (round (/ (length l) 2)))) 0.5))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const type: floatList is array float;

const func float: median (in floatList: floats) is func
  result
    var float: median is 0.0;
  local
    var floatList: sortedFloats is 0 times 0.0;
  begin
    sortedFloats := sort(floats);
    if odd(length(sortedFloats)) then
      median := sortedFloats[succ(length(sortedFloats)) div 2];
    else
      median := 0.5 * (sortedFloats[length(sortedFloats) div 2] +
                       sortedFloats[succ(length(sortedFloats) div 2)]);
    end if;
  end func;

const proc: main is func
  local
    const floatList: flist1 is [] (5.1, 2.6, 6.2, 8.8, 4.6, 4.1);
    const floatList: flist2 is [] (5.1, 2.6, 8.8, 4.6, 4.1);
  begin
    writeln("flist1 median is " <& median(flist1) digits 2 lpad 7); # 4.85
    writeln("flist2 median is " <& median(flist2) digits 2 lpad 7); # 4.60
  end func;
```



## Sidef


```ruby
func median(arry) {
    var srtd = arry.sort;
    var alen = srtd.length;
    srtd[(alen-1)/2]+srtd[alen/2] / 2;
}
```



## Slate


```slate
s@(Sequence traits) median
[
  s isEmpty
    ifTrue: [Nil]
    ifFalse:
      [| sorted |
       sorted: s sort.
       sorted length `cache isEven
         ifTrue: [(sorted middle + (sorted at: sorted indexMiddle - 1)) / 2]
         ifFalse: [sorted middle]]
].
```



```slate
inform: { 4.1 . 5.6 . 7.2 . 1.7 . 9.3 . 4.4 . 3.2 } median.
inform: { 4.1 . 7.2 . 1.7 . 9.3 . 4.4 . 3.2 } median.
```



## Smalltalk

{{works with|GNU Smalltalk}}


```smalltalk
OrderedCollection extend [
    median [
      self size = 0
        ifFalse: [ |s l|
          l := self size.
          s := self asSortedCollection.
	  (l rem: 2) = 0
	    ifTrue: [ ^ ((s at: (l//2 + 1)) + (s at: (l//2))) / 2 ]
	    ifFalse: [ ^ s at: (l//2 + 1) ]
	]
	ifTrue: [ ^nil ]
    ]
].
```



```smalltalk
{ 4.1 . 5.6 . 7.2 . 1.7 . 9.3 . 4.4 . 3.2 } asOrderedCollection
   median displayNl.
{ 4.1 . 7.2 . 1.7 . 9.3 . 4.4 . 3.2 } asOrderedCollection
   median displayNl.
```



## Stata

Use '''[https://www.stata.com/help.cgi?summarize summarize]''' to compute the median of a variable (as well as other basic statistics).


```stata
set obs 100000
gen x=rbeta(0.2,1.3)
quietly summarize x, detail
display r(p50)
```


Here is a straightforward implementation using '''[https://www.stata.com/help.cgi? sort]'''.


```stata
program calcmedian, rclass sortpreserve
sort `1'
if mod(_N,2)==0 {
	return scalar p50=(`1'[_N/2]+`1'[_N/2+1])/2
}
else {
	return scalar p50=`1'[(_N-1)/2]
}
end

calcmedian x
display r(p50)
```



## Tcl


```tcl
proc median args {
    set list [lsort -real $args]
    set len [llength $list]
    # Odd number of elements
    if {$len & 1} {
        return [lindex $list [expr {($len-1)/2}]]
    }
    # Even number of elements
    set idx2 [expr {$len/2}]
    set idx1 [expr {$idx2-1}]
    return [expr {
        ([lindex $list $idx1] + [lindex $list $idx2])/2.0
    }]
}

puts [median 3.0 4.0 1.0 -8.4 7.2 4.0 1.0 1.2]; # --> 2.1
```


=={{header|TI-83 BASIC}}==

Using the built-in function:

```ti83b
median({1.1, 2.5, 0.3241})
```





=={{header|TI-89 BASIC}}==


```ti89b
median({3, 4, 1, -8.4, 7.2, 4, 1, 1})
```



## Ursala

the simple way (sort first and then look in the middle)

```Ursala
#import std
#import flo

median = fleq-<; @K30K31X eql?\~&rh div\2.+ plus@lzPrhPX
```

test program, once with an odd length and once with an even length vector

```Ursala
#cast %eW

examples =

median~~ (
   <9.3,-2.0,4.0,7.3,8.1,4.1,-6.3,4.2,-1.0,-8.4>,
   <8.3,-3.6,5.7,2.3,9.3,5.4,-2.3,6.3,9.9>)
```

output:

```txt

(4.050000e+00,5.700000e+00)
```


== {{header|Vala}} ==
Requires <code>--pkg posix -X -lm</code> compilation flags in order to use POSIX qsort, and to have access to math library.


```vala
int compare_numbers(void* a_ref, void* b_ref) {
    double a = *(double*) a_ref;
    double b = *(double*) b_ref;
    return a > b ? 1 : a < b ? -1 : 0;
}

double median(double[] elements) {
    double[] clone = elements;
    Posix.qsort(clone, clone.length, sizeof(double), compare_numbers);
    double middle = clone.length / 2.0;
    int first = (int) Math.floor(middle);
    int second = (int) Math.ceil(middle);
    return (clone[first] + clone[second]) / 2;
}
void main() {
    double[] array1 = {2, 4, 6, 1, 7, 3, 5};
    double[] array2 = {2, 4, 6, 1, 7, 3, 5, 8};
    print(@"$(median(array1)) $(median(array2))\n");
}
```



## VBA

{{trans|Phix}}
Uses [[Quickselect_algorithm#VBA|quick select]].

```vb
Private Function medianq(s As Variant) As Double
    Dim res As Double, tmp As Integer
    Dim l As Integer, k As Integer
    res = 0
    l = UBound(s): k = WorksheetFunction.Floor_Precise((l + 1) / 2, 1)
        If l Then
            res = quick_select(s, k)
            If l Mod 2 = 0 Then
                tmp = quick_select(s, k + 1)
                res = (res + tmp) / 2
            End If
        End If
    medianq = res
End Function
Public Sub main2()
    s = [{4, 2, 3, 5, 1, 6}]
    Debug.Print medianq(s)
End Sub
```
{{out}}

```txt
 3,5
```


## Vedit macro language

This is a simple implementation for positive integers using sorting.
The data is stored in current edit buffer in ascii representation. The values must be right justified.

The result is returned in text register @10. In case of even number of items, the lower middle value is returned.


```vedit
Sort(0, File_Size, NOCOLLATE+NORESTORE)
EOF Goto_Line(Cur_Line/2)
Reg_Copy(10, 1)
```



## Wortel


```wortel
@let {
  ; iterative
  med1 &l @let {a @sort l s #a i @/s 2 ?{%%s 2 ~/ 2 +`-i 1 a `i a `i a}}

  ; tacit
  med2 ^(\~/2 @sum @(^(\&![#~f #~c] \~/2 \~-1 #) @` @id) @sort)

  [[
    !med1 [4 2 5 2 1]
    !med1 [4 5 2 1]
    !med2 [4 2 5 2 1]
    !med2 [4 5 2 1]
  ]]
}
```

Returns:
```txt
[2 3 2 3]
```



## Yabasic

{{trans|Lua}}

```Yabasic
sub floor(x)
    return int(x + .05)
end sub

sub ceil(x)
    if x > int(x) x = x + 1
    return x
end sub

SUB ASort$(matriz$())
    local last, gap, first, tempi$, tempj$, i, j

    last = arraysize(matriz$(), 1)

    gap = floor(last / 10) + 1
    while(TRUE)
	first = gap + 1
	for i = first to last
	    tempi$ = matriz$(i)
	    j = i - gap
	    while(TRUE)
	        tempj$ = matriz$(j)
	 	if (tempi$ >= tempj$) then
	   	    j = j + gap
	   	    break
	 	end if
	 	matriz$(j+gap) = tempj$
	 	if j <= gap then
	   	    break
	 	end if
	 	j = j - gap
	    wend
	    matriz$(j) = tempi$
        next i
	if gap = 1 then
	    return
	else
	    gap = floor(gap / 3.5) + 1
	end if
    wend
END SUB


sub median(numlist$)
    local numlist$(1), n

    n = token(numlist$, numlist$(), ", ")

    ASort$(numlist$())

    if mod(n, 2) = 0 then return (val(numlist$(n / 2)) + val(numlist$(n / 2 + 1))) / 2 end if
    return val(numlist$(ceil(n / 2)))
end sub

print median("4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2")    // 4.4
print median("4.1, 7.2, 1.7, 9.3, 4.4, 3.2")         // 4.25

```



## zonnon


```zonnon

module Averages;

type
	Vector = array {math} * of real;

procedure Partition(var a: Vector; left, right: integer): integer;
var
	pValue,aux: real;
	store,i,pivot: integer;
begin
	pivot := right;
	pValue := a[pivot];
	aux := a[right];a[right] := a[pivot];a[pivot] := aux; (* a[pivot] <-> a[right] *)
	store := left;
	for i := left to right -1 do
		if a[i] <= pValue then
			aux := a[store];a[store] := a[i];a[i]:=aux;
			inc(store)
		end
	end;
	aux := a[right];a[right] := a[store]; a[store] := aux;
	return store
end Partition;

(* QuickSelect algorithm *)
procedure Select(a: Vector; left,right,k: integer;var r: real);
var
	pIndex, pDist : integer;
begin
	if left = right then r := a[left]; return end;
	pIndex := Partition(a,left,right);
	pDist := pIndex - left + 1;
	if pDist = k then
		r := a[pIndex];return
	elsif k < pDist then
		Select(a,left, pIndex - 1, k, r)
	else
		Select(a,pIndex + 1, right, k - pDist, r)
	end
end Select;

procedure Median(a: Vector): real;
var
	idx: integer;
	r1,r2 : real;
begin
	idx := len(a) div 2 + 1;
	r1 := 0.0;r2 := 0.0;
	Select(a,0,len(a) - 1,idx,r1);
	if odd(len(a)) then return r1 end;
	Select(a,0,len(a) - 1,idx - 1,r2);
	return (r1 + r2) / 2;
end Median;

var
	ary: Vector;
	r: real;

begin
	ary := new Vector(3);
	ary := [5.0,3.0,4.0];
	writeln(Median(ary):10:2);
	ary := new Vector(4);
	ary := [5.0,4.0,2.0,3.0];
	writeln(Median(ary):10:2);
	ary := new Vector(8);
	ary := [3.0,4.0,1.0,-8.4,7.2,4.0,1.0,1.2];
	writeln(Median(ary):10:2)
end Averages.

```


```txt

        4
       3,5
       2,1


```


## zkl

Using the [[Quickselect algorithm#zkl]] for O(n) time:

```zkl
var quickSelect=Import("quickSelect").qselect;

fcn median(xs){
   n:=xs.len();
   if (n.isOdd) return(quickSelect(xs,n/2));
   ( quickSelect(xs,n/2-1) + quickSelect(xs,n/2) )/2;
}
```


```zkl
median(T( 5.1, 2.6, 6.2, 8.8, 4.6, 4.1 )); //-->4.85
median(T( 5.1, 2.6, 8.8, 4.6, 4.1 ));      //-->4.6
```


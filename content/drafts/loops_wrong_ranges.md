+++
title = "Loops/Wrong ranges"
description = ""
date = 2019-09-05T12:13:21Z
aliases = []
[extra]
id = 21994
[taxonomies]
categories = []
tags = []
+++

[[Category:Loop modifiers]]

{{task|Iteration}}

Some languages have syntax or function(s) to generate a range of numeric values from a start value, a stop value, and an increment.

The purpose of this task is to select the range syntax/function that would generate at least two increasing numbers when given a stop value more than the start value and a positive increment of less than half the difference. You are than to use that ''same'' syntax/function but with different parameters; and show, here, what would happen.

Use these values if possible:
:{| class="wikitable"
!start ||stop ||increment ||Comment
|-
| -2||2||1||Normal
|-
| -2||2||0||Zero increment
|-
| -2||2||-1||Increments away from stop value
|-
| -2||2||10||First increment is beyond stop value
|-
|2||-2||1||Start more than stop: positive increment
|-
|2||2||1||Start equal stop: positive increment
|-
|2||2||-1||Start equal stop: negative increment
|-
|2||2||0||Start equal stop: zero increment
|-
|0||0||0||Start equal stop equal zero: zero increment
|}

;Related tasks:
*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## ALGOL W

Using the Algol W for loop and limiting the sequence to 10 values. The Algol W for loop considers the sign of the increment value when deciding whether to terminate when the loop counter exceeds the stop value (positive increment) or is smaller than the stop value (negative increment).

```algolw
begin
    % sets the first n elements of s to the sequences of values specified by start, stop and increment    %
    % s( 0 ) is set to the number of elements of s that have been set, in case the sequence ends before n %
    procedure sequence ( integer array s ( * )
                       ; integer value n, start, stop, increment
                       ) ;
    begin
        integer sPos;
        for j := 0 until n do s( j ) := 0;
        sPos  := 1;
        for j := start step increment until stop do begin
            if sPos > n then goto done;
               s( sPos ) := j;
               s( 0    ) := s( 0 ) + 1;
               sPos      := sPos + 1;
       end for_j ;
done:
    end sequence ;
    % tests the sequence procedure %
    procedure testSequence( integer    value start, stop, increment
                          ; string(48) value legend
                          ) ;
    begin
        integer array s ( 0 :: 10 );
        sequence( s, 10, start, stop, increment );
        s_w := 0; % set output formating %
        i_w := 4;
        write( legend, ": " );
        for i := 1 until s( 0 ) do writeon( s( i ) )
    end testSequence ;
    % task trest cases %
    testSequence( -2,  2,  1, "Normal"                                      );
    testSequence( -2,  2,  0, "Zero increment"                              );
    testSequence( -2,  2, -1, "Increments away from stop value"             );
    testSequence( -2,  2, 10, "First increment is beyond stop value"        );
    testSequence(  2, -2,  1, "Start more than stop: positive increment"    );
    testSequence(  2,  2,  1, "Start equal stop: positive increment"        );
    testSequence(  2,  2, -1, "Start equal stop: negative increment"        );
    testSequence(  2,  2,  0, "Start equal stop: zero increment"            );
    testSequence(  0,  0,  0, "Start equal stop equal zero: zero increment" )
end.
```

{{out}}

```txt

Normal                                          :   -2  -1   0   1   2
Zero increment                                  :   -2  -2  -2  -2  -2  -2  -2  -2  -2  -2
Increments away from stop value                 :
First increment is beyond stop value            :   -2
Start more than stop: positive increment        :
Start equal stop: positive increment            :    2
Start equal stop: negative increment            :    2
Start equal stop: zero increment                :    2   2   2   2   2   2   2   2   2   2
Start equal stop equal zero: zero increment     :    0   0   0   0   0   0   0   0   0   0

```



## C

C's 'for' statement appears to fit the bill here and so we use it directly to generate the required ranges of numbers though, as some of the ranges will be infinite, we limit the output to a maximum of 10 numbers. 

```c>#include <stdio.h


#define TRUE 1
#define FALSE 0

typedef int bool;

typedef struct {
    int start, stop, incr;
    const char *comment;
} S;

S examples[9] = {
    {-2, 2, 1, "Normal"},
    {-2, 2, 0, "Zero increment"},
    {-2, 2, -1, "Increments away from stop value"},
    {-2, 2, 10, "First increment is beyond stop value"},
    {2, -2, 1, "Start more than stop: positive increment"},
    {2, 2, 1, "Start equal stop: positive increment"},
    {2, 2, -1, "Start equal stop: negative increment"},
    {2, 2, 0, "Start equal stop: zero increment"},
    {0, 0, 0, "Start equal stop equal zero: zero increment"}
};

int main() {
    int i, j, c;
    bool empty;
    S s;
    const int limit = 10;
    for (i = 0; i < 9; ++i) {
        s = examples[i];
        printf("%s\n", s.comment);
        printf("Range(%d, %d, %d) -> [", s.start, s.stop, s.incr);
        empty = TRUE;
        for (j = s.start, c = 0; j <= s.stop && c < limit; j += s.incr, ++c) {
            printf("%d ", j);
            empty = FALSE;
        }
        if (!empty) printf("\b");
        printf("]\n\n");
    }
    return 0;
}
```


{{out}}

```txt

Normal
Range(-2, 2, 1) -> [-2 -1 0 1 2]

Zero increment
Range(-2, 2, 0) -> [-2 -2 -2 -2 -2 -2 -2 -2 -2 -2]

Increments away from stop value
Range(-2, 2, -1) -> [-2 -3 -4 -5 -6 -7 -8 -9 -10 -11]

First increment is beyond stop value
Range(-2, 2, 10) -> [-2]

Start more than stop: positive increment
Range(2, -2, 1) -> []

Start equal stop: positive increment
Range(2, 2, 1) -> [2]

Start equal stop: negative increment
Range(2, 2, -1) -> [2 1 0 -1 -2 -3 -4 -5 -6 -7]

Start equal stop: zero increment
Range(2, 2, 0) -> [2 2 2 2 2 2 2 2 2 2]

Start equal stop equal zero: zero increment
Range(0, 0, 0) -> [0 0 0 0 0 0 0 0 0 0]

```



## C sharp

{{trans|Visual Basic .NET}}
Behavior for naïve translation is different from VB.NET (and identical to C), as it does not handle an "overshot" iteration variable when the increment is negative.

```csharp
using System;
using System.Collections.Generic;

static class Program
{
    static void Main()
    {
        Example(-2, 2, 1, "Normal");
        Example(-2, 2, 0, "Zero increment");
        Example(-2, 2, -1, "Increments away from stop value");
        Example(-2, 2, 10, "First increment is beyond stop value");
        Example(2, -2, 1, "Start more than stop: positive increment");
        Example(2, 2, 1, "Start equal stop: positive increment");
        Example(2, 2, -1, "Start equal stop: negative increment");
        Example(2, 2, 0, "Start equal stop: zero increment");
        Example(0, 0, 0, "Start equal stop equal zero: zero increment");
    }

    static IEnumerable<int> Range(int start, int stop, int increment)
    {
        // To replicate the (arguably more correct) behavior of VB.NET:
        //for (int i = start; increment >= 0 ? i <= stop : stop <= i; i += increment)

        // Decompiling the IL emitted by the VB compiler (uses shifting right by 31 as the signum function and bitwise xor in place of the conditional expression):
        //for (int i = start; ((increment >> 31) ^ i) <= ((increment >> 31) ^ stop); i += increment)

        // "Naïve" translation.
        for (int i = start; i <= stop; i += increment)
            yield return i;
    }

    static void Example(int start, int stop, int increment, string comment)
    {
        // Add a space, pad to length 50 with hyphens, and add another space.
        Console.Write((comment + " ").PadRight(50, '-') + " ");

        const int MAX_ITER = 9;

        int iteration = 0;
        foreach (int i in Range(start, stop, increment))
        {
            Console.Write("{0,2} ", i);

            if (++iteration > MAX_ITER) break;
        }

        Console.WriteLine();
    }
}
```


{{out|note=identical to C}}

```txt
Normal ------------------------------------------- -2 -1  0  1  2
Zero increment ----------------------------------- -2 -2 -2 -2 -2 -2 -2 -2 -2 -2
Increments away from stop value ------------------ -2 -3 -4 -5 -6 -7 -8 -9 -10 -11
First increment is beyond stop value ------------- -2
Start more than stop: positive increment ---------
Start equal stop: positive increment -------------  2
Start equal stop: negative increment -------------  2  1  0 -1 -2 -3 -4 -5 -6 -7
Start equal stop: zero increment -----------------  2  2  2  2  2  2  2  2  2  2
Start equal stop equal zero: zero increment ------  0  0  0  0  0  0  0  0  0  0
```



### C translation

{{trans|C|version=287385}}
Just for fun; some strictly unnecessary changes made to be closer to idiomatic C#.

```csharp
using System;

static class Program {
    struct S {
        public int Start, Stop, Incr;
        public string Comment;
    }

    static readonly S[] examples = {
        new S{Start=-2, Stop=2, Incr=1, Comment="Normal"},
        new S{Start=-2, Stop=2, Incr=0, Comment="Zero increment"},
        new S{Start=-2, Stop=2, Incr=-1, Comment="Increments away from stop value"},
        new S{Start=-2, Stop=2, Incr=10, Comment="First increment is beyond stop value"},
        new S{Start=2, Stop=-2, Incr=1, Comment="Start more than stop: positive increment"},
        new S{Start=2, Stop=2, Incr=1, Comment="Start equal stop: positive increment"},
        new S{Start=2, Stop=2, Incr=-1, Comment="Start equal stop: negative increment"},
        new S{Start=2, Stop=2, Incr=0, Comment="Start equal stop: zero increment"},
        new S{Start=0, Stop=0, Incr=0, Comment="Start equal stop equal zero: zero increment"}
    };

    static int Main() {
        const int limit = 10;
        bool empty;
        for (int i = 0; i < 9; ++i) {
            S s = examples[i];
            Console.Write("{0}\n", s.Comment);
            Console.Write("Range({0:d}, {1:d}, {2:d}) -> [", s.Start, s.Stop, s.Incr);
            empty = true;
            for (int j = s.Start, c = 0; j <= s.Stop && c < limit; j += s.Incr, ++c) {
                Console.Write("{0:d} ", j);
                empty = false;
            }
            if (!empty) Console.Write("\b");
            Console.Write("]\n\n");
        }
        return 0;
    }
}
```


{{out}}
Same as original C.


## Factor

<code><range></code> divides by the step value, so a step of 0 causes a divide by zero exception. For the purpose of getting through all the examples, the exceptions are dropped and execution continues, which in general should be avoided.

```factor
USING: continuations formatting io kernel math.ranges
prettyprint sequences ;

: try-range ( from length step -- )
    [ <range> { } like . ]
    [ 4drop "Exception: divide by zero." print ] recover ;

{
    { -2 2 1 } { 2 2 0 } { -2 2 -1 } { -2 2 10 } { 2 -2 1 }
    { 2 2 1 } { 2 2 -1 } { 2 2 0 } { 0 0 0 }
}
[
    first3
    [ "%2d %2d %2d <range>  =>  " printf ]
    [ try-range ] 3bi
] each
```

{{out}}

```txt

-2  2  1 <range>  =>  { -2 -1 0 1 2 }
 2  2  0 <range>  =>  Exception: divide by zero.
-2  2 -1 <range>  =>  { }
-2  2 10 <range>  =>  { -2 }
 2 -2  1 <range>  =>  { }
 2  2  1 <range>  =>  { 2 }
 2  2 -1 <range>  =>  { 2 }
 2  2  0 <range>  =>  Exception: divide by zero.
 0  0  0 <range>  =>  Exception: divide by zero.

```



## Go

Go has only one loop, a 'for' statement, which supports four different syntactical forms commonly found in other C-family languages:

1. A C-like 'for' loop with initialization, condition and increment sections.

2. The 'while' loop functionality (condition only)

3. Infinite loop, equivalent to for(;;) (all sections omitted)

4. Looping over a range of values, similar to foreach etc. (using 'range' keyword).

It appears that either #1 or #4 fits the requirements of this task so I've written a function which generates the appropriate sequence using #1 (limited to a maximum of 10 elements as some sequences will be infinite). I've then applied #4 to the resulting sequence. All sequences include the stop value if it's actually reached.

```go
package main

import "fmt"

type S struct {
    start, stop, incr int
    comment          string
}

var examples = []S{
    {-2, 2, 1, "Normal"},
    {-2, 2, 0, "Zero increment"},
    {-2, 2, -1, "Increments away from stop value"},
    {-2, 2, 10, "First increment is beyond stop value"},
    {2, -2, 1, "Start more than stop: positive increment"},
    {2, 2, 1, "Start equal stop: positive increment"},
    {2, 2, -1, "Start equal stop: negative increment"},
    {2, 2, 0, "Start equal stop: zero increment"},
    {0, 0, 0, "Start equal stop equal zero: zero increment"},
}

func sequence(s S, limit int) []int {
    var seq []int
    for i, c := s.start, 0; i <= s.stop && c < limit; i, c = i+s.incr, c+1 {
        seq = append(seq, i)
    }
    return seq
}

func main() {
    const limit = 10
    for _, ex := range examples {
        fmt.Println(ex.comment)
        fmt.Printf("Range(%d, %d, %d) -> ", ex.start, ex.stop, ex.incr)
        fmt.Println(sequence(ex, limit))
        fmt.Println()
    }
}
```


{{out}}

```txt

Normal
Range(-2, 2, 1) -> [-2 -1 0 1 2]

Zero increment
Range(-2, 2, 0) -> [-2 -2 -2 -2 -2 -2 -2 -2 -2 -2]

Increments away from stop value
Range(-2, 2, -1) -> [-2 -3 -4 -5 -6 -7 -8 -9 -10 -11]

First increment is beyond stop value
Range(-2, 2, 10) -> [-2]

Start more than stop: positive increment
Range(2, -2, 1) -> []

Start equal stop: positive increment
Range(2, 2, 1) -> [2]

Start equal stop: negative increment
Range(2, 2, -1) -> [2 1 0 -1 -2 -3 -4 -5 -6 -7]

Start equal stop: zero increment
Range(2, 2, 0) -> [2 2 2 2 2 2 2 2 2 2]

Start equal stop equal zero: zero increment
Range(0, 0, 0) -> [0 0 0 0 0 0 0 0 0 0]

```


## Huginn

[https://huginn.org/ Huginn] has the Range generator in Algorithms package.
Instantiation of an a priori invalid range is a fatal error.


```huginn
import Algorithms as algo;

class Example {
  _start = none;
  _stop = none;
  _step = none;
  _comment = none;
}

main() {
  examples = [
    Example( -2,  2,  1, "Normal" ),
    Example(  2,  2,  0, "Start equal stop: zero increment" ),
    Example(  0,  0,  0, "Start equal stop equal zero: zero increment" ),
    Example(  2,  2,  1, "Start equal stop: positive increment" ),
    Example(  2,  2, -1, "Start equal stop: negative increment" ),
    Example( -2,  2, 10, "First increment is beyond stop value" ),
    Example( -2,  2,  0, "Zero increment, stop greater than start" ),
    Example( -2,  2, -1, "Increments away from stop value" ),
    Example(  2, -2,  1, "Start more than stop: positive increment" )
  ];
  for ( ex : examples ) {
    print(
      "{}\nRange( {}, {}, {} ) -> ".format(
        ex._comment, ex._start, ex._stop, ex._step
      )
    );
    r = algo.range( ex._start, ex._stop, ex._step );
    print(
      "{}\n\n".format(
        algo.materialize( algo.slice( r, 22 ), list )
      )
    );
  }
}
```


{{out}}

```txt
Normal
Range( -2, 2, 1 ) -> [-2, -1, 0, 1]

Start equal stop: zero increment
Range( 2, 2, 0 ) -> []

Start equal stop equal zero: zero increment
Range( 0, 0, 0 ) -> []

Start equal stop: positive increment
Range( 2, 2, 1 ) -> []

Start equal stop: negative increment
Range( 2, 2, -1 ) -> []

First increment is beyond stop value
Range( -2, 2, 10 ) -> [-2]

Zero increment, stop greater than start
Range( -2, 2, 0 ) -> ./range.hgn:32:17: Invalid range.
Exit 3
```



## J

J can build ranges.

```txt

   NB. rank 3 integers with a bit of inversion
   i. 2 _3 6
12 13 14 15 16 17
 6  7  8  9 10 11
 0  1  2  3  4  5

30 31 32 33 34 35
24 25 26 27 28 29
18 19 20 21 22 23


   NB. from _3 to 3 in 12 steps
   i: 3j12
_3 _2.5 _2 _1.5 _1 _0.5 0 0.5 1 1.5 2 2.5 3

```

The loops with multiple ranges shall use this version of range.  Or so it is in on 2019, March 6.
http://rosettacode.org/wiki/Loops/with_multiple_ranges#J


```txt

R =: ".;._2 [ 0 : 0
    9      2      _2   NB. valid descending range
   _2      2       1   NB. valid ascending range
   _2      2       0
   _2      2       _1
   _2      2       10
   2       _2      1
   2       2       1
   2       2       _1
   2       2       0
   0       0       0
)

NB. define range as a linear polynomial
start =: 0&{
stop =: 1&{
increment =: 2&{ :: 1:  NB. on error use 1
range =: (start , increment) p. [: i. [: >: [: <. (stop - start) % increment

   NB. the first two of these are ranges with valid arguments
   (; range :: ('*error*'"_))"1 R
+-------+-----------+
|9 2 _2 |9 7 5 3    |
+-------+-----------+
|_2 2 1 |_2 _1 0 1 2|
+-------+-----------+
|_2 2 0 |*error*    |
+-------+-----------+
|_2 2 _1|_4 _3 _2   |
+-------+-----------+
|_2 2 10|_2         |
+-------+-----------+
|2 _2 1 |4 3 2      |
+-------+-----------+
|2 2 1  |2          |
+-------+-----------+
|2 2 _1 |2          |
+-------+-----------+
|2 2 0  |2          |
+-------+-----------+
|0 0 0  |0          |
+-------+-----------+


```



## Julia

Julia has a start:increment:stop iterator syntax, which allows negative increments, but not zero increments.

```Julia

collect(-2:1:2)   # → [-2, -1, 0, 1, 2]
collect(-2:0:2)   # fails
collect(-2:-1:2)  # → []
collect(-2:10:2)  # → [-2]
collect(2:1:-2)   # → []
collect(2:1:2)    # → [2]
collect(2:-1:2)   # → [2]
collect(2:0:2)    # fails
collect(0:0:0)    # fails

```



## Kotlin

Although Kotlin's 'for' statement can deal with a range of integers, the increment must be positive and so it cannot be used for this task. We therefore use instead a 'while' statement to generate the same sequence as a C language 'for' statement would (limited to a maximum of 10 elements as some sequences will be infinite) and wrap it in a function. 

```scala
// Version 1.2.70

class Example(val start: Int, val stop: Int, val incr: Int, val comment: String)

var examples = listOf(
    Example(-2, 2, 1, "Normal"),
    Example(-2, 2, 0, "Zero increment"),
    Example(-2, 2, -1, "Increments away from stop value"),
    Example(-2, 2, 10, "First increment is beyond stop value"),
    Example(2, -2, 1, "Start more than stop: positive increment"),
    Example(2, 2, 1, "Start equal stop: positive increment"),
    Example(2, 2, -1, "Start equal stop: negative increment"),
    Example(2, 2, 0, "Start equal stop: zero increment"),
    Example(0, 0, 0, "Start equal stop equal zero: zero increment")
)

fun sequence(ex: Example, limit: Int) =
    if (ex.incr == 0) {
        List(limit) { ex.start }
    }
    else {
        val res = mutableListOf<Int>()
        var c = 0
        var i = ex.start
        while (i <= ex.stop && c < limit) {
            res.add(i)
            i += ex.incr
            c++
        }
        res
    }

fun main(args: Array<String>) {
    for (ex in examples) {
        println(ex.comment)
        System.out.printf("Range(%d, %d, %d) -> ", ex.start, ex.stop, ex.incr)
        println(sequence(ex, 10))
        println()
    }
}
```


{{output}}

```txt

Normal
Range(-2, 2, 1) -> [-2, -1, 0, 1, 2]

Zero increment
Range(-2, 2, 0) -> [-2, -2, -2, -2, -2, -2, -2, -2, -2, -2]

Increments away from stop value
Range(-2, 2, -1) -> [-2, -3, -4, -5, -6, -7, -8, -9, -10, -11]

First increment is beyond stop value
Range(-2, 2, 10) -> [-2]

Start more than stop: positive increment
Range(2, -2, 1) -> []

Start equal stop: positive increment
Range(2, 2, 1) -> [2]

Start equal stop: negative increment
Range(2, 2, -1) -> [2, 1, 0, -1, -2, -3, -4, -5, -6, -7]

Start equal stop: zero increment
Range(2, 2, 0) -> [2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

Start equal stop equal zero: zero increment
Range(0, 0, 0) -> [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

```



## Perl

None of these sequences are 'errors', though some are of infinite length, and #5 has a length of zero.

```perl
for $i (
     [ -2,    2,    1], #1 Normal
     [ -2,    2,    0], #2 Zero increment
     [ -2,    2,   -1], #3 Increments away from stop value
     [ -2,    2,   10], #4 First increment is beyond stop value
     [  2,   -2,    1], #5 Start more than stop: positive increment
     [  2,    2,    1], #6 Start equal stop: positive increment
     [  2,    2,   -1], #7 Start equal stop: negative increment
     [  2,    2,    0], #8 Start equal stop: zero increment
     [  0,    0,    0], #9 Start equal stop equal zero: zero increment
) {
    $iter = gen_seq(@$i);
    printf "start: %3d  stop: %3d  incr: %3d | ", @$i;
    printf "%4s", &$iter for 1..10;
    print "\n";
}

sub gen_seq {
    my($start,$stop,$increment) = @_;
    $n = 0;
    return sub {
        $term = $start + $n++ * $increment;
        return $term > $stop ? '' : $term;
    }
}
```

{{out}}

```txt
start:  -2  stop:   2  incr:   1 |   -2  -1   0   1   2
start:  -2  stop:   2  incr:   0 |   -2  -2  -2  -2  -2  -2  -2  -2  -2  -2
start:  -2  stop:   2  incr:  -1 |   -2  -3  -4  -5  -6  -7  -8  -9 -10 -11
start:  -2  stop:   2  incr:  10 |   -2
start:   2  stop:  -2  incr:   1 |
start:   2  stop:   2  incr:   1 |    2
start:   2  stop:   2  incr:  -1 |    2   1   0  -1  -2  -3  -4  -5  -6  -7
start:   2  stop:   2  incr:   0 |    2   2   2   2   2   2   2   2   2   2
start:   0  stop:   0  incr:   0 |    0   0   0   0   0   0   0   0   0   0
```



## Perl 6

{{works with|Rakudo|2018.08}}

It would be odd to call ANY of these sequences "wrong" in Perl 6. Perl 6 specifically has built in capability of working with infinite sequences. Just because a sequence is infinite, doesn't mean you can't define it, work with it or use values from it. Sure, if you try to reify the whole thing you may be waiting a while, but there is nothing preventing you from using a portion of it.

Perl 6 sequence definitions ''specifically'' allow "ending points" that may never occur in the sequence. Since that is the case, you don't even really '''need''' to specify a stop value. You can just say stop at "whatever". Whatever is spelled "'''*'''" in Perl 6. 

There is additional syntax you can add to stop at the nearest value, last value previous or first value successor to the "stop value" (Note I didn't say less than or greater than the stop value since the sequence can be ascending, descending or non-monotonic). 

Also note: The iterator function for the sequence is literally a function. It is any expression that produces a value. These sequences all use simple arithmatic increments but that is not a limitation of the sequence operator.  


```perl6
# Given sequence definitions
#   start  stop  inc.   Comment
for   -2,    2,    1, # Normal
      -2,    2,    0, # Zero increment
      -2,    2,   -1, # Increments away from stop value
      -2,    2,   10, # First increment is beyond stop value
       2,   -2,    1, # Start more than stop: positive increment
       2,    2,    1, # Start equal stop: positive increment
       2,    2,   -1, # Start equal stop: negative increment
       2,    2,    0, # Start equal stop: zero increment
       0,    0,    0, # Start equal stop equal zero: zero increment

# Additional "problematic" sequences
       1,  Inf,    3, # Endpoint literally at infinity
       0,    π,  τ/8, # Floating point numbers
     1.4,    *, -7.1  # Whatever

  -> $start, $stop, $inc {
    my $seq = flat ($start, *+$inc … $stop);
    printf "Start: %3s, Stop: %3s, Increment: %3s | ", $start, $stop.Str, $inc;
    # only show up to the first 15 elements of possibly infinite sequences
    put $seq[^15].grep: +*.defined
}

# For that matter the start and end values don't need to be numeric either. Both
# or either can be a function, list, or other object. Really anything that a
# "successor" function can be defined for and produces a value.
say "\nDemonstration of some other specialized sequence operator functionality:";
# Start with a list, iterate by multiplying the previous 3 terms together
# and end with a term defined by a function.
put 1, -.5, 2.sqrt, * * * * * … *.abs < 1e-2;

# Start with an array, iterate by rotating, end when 0 is in the last place.
say [0,1,2,3,4,5], *.rotate(-1) … !*.tail;

# Iterate strings backwards.
put 'xp' … 'xf';
```

{{out}}

```txt
Start:  -2, Stop:   2, Increment:   1 | -2 -1 0 1 2
Start:  -2, Stop:   2, Increment:   0 | -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2
Start:  -2, Stop:   2, Increment:  -1 | -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16
Start:  -2, Stop:   2, Increment:  10 | -2 8 18 28 38 48 58 68 78 88 98 108 118 128 138
Start:   2, Stop:  -2, Increment:   1 | 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
Start:   2, Stop:   2, Increment:   1 | 2
Start:   2, Stop:   2, Increment:  -1 | 2
Start:   2, Stop:   2, Increment:   0 | 2
Start:   0, Stop:   0, Increment:   0 | 0
Start:   1, Stop: Inf, Increment:   3 | 1 4 7 10 13 16 19 22 25 28 31 34 37 40 43
Start:   0, Stop: 3.141592653589793, Increment: 0.7853981633974483 | 0 0.7853981633974483 1.5707963267948966 2.356194490192345 3.141592653589793
Start: 1.4, Stop:   *, Increment: -7.1 | 1.4 -5.7 -12.8 -19.9 -27 -34.1 -41.2 -48.3 -55.4 -62.5 -69.6 -76.7 -83.8 -90.9 -98

Demonstration of some other specialized sequence operator functionality:
1 -0.5 1.4142135623730951 -0.7071067811865476 0.5000000000000001 -0.5000000000000002 0.176776695296637 -0.04419417382415928 0.0039062500000000095
([0 1 2 3 4 5] [5 0 1 2 3 4] [4 5 0 1 2 3] [3 4 5 0 1 2] [2 3 4 5 0 1] [1 2 3 4 5 0])
xp xo xn xm xl xk xj xi xh xg xf
```



## Phix

Phix for loops do not allow a zero step (neither are any floating point values permitted).

The following shows the behaviour of both for and while loops, and the latter has a couple of
additional commented out termination checks that might be appropriate in some cases.

```Phix
procedure test(integer start, stop, step, string legend, bool bFor)
    sequence res = {}
    if bFor then
        try
            for i=start to stop by step do
                res &= i
                if length(res)>9 then exit end if
            end for
            res = sprint(res)
        catch e
            res = e[E_USER]
        end try
    else
        integer i = start
        while (step>=0 and i<=stop)
           or (step<=0 and i>=stop) do
            res &= i
            if length(res)>9 then exit end if
--          if i=stop then exit end if
--          if step=0 then exit end if
            i += step
        end while
        res = sprint(res)
    end if
    printf(1,"%-43s: %s\n",{legend,res})
end procedure

for i=1 to 2 do
    ?iff(i=1?"for":"while")
    test(-2, 2, 1, "Normal"                                     ,i=1)
    test(-2, 2, 0, "Zero increment"                             ,i=1)
    test(-2, 2,-1, "Increments away from stop value"            ,i=1)
    test(-2, 2,10, "First increment is beyond stop value"       ,i=1)
    test( 2,-2, 1, "Start more than stop: positive increment"   ,i=1)
    test( 2, 2, 1, "Start equal stop: positive increment"       ,i=1)
    test( 2, 2,-1, "Start equal stop: negative increment"       ,i=1)
    test( 2, 2, 0, "Start equal stop: zero increment"           ,i=1)
    test( 0, 0, 0, "Start equal stop equal zero: zero increment",i=1)
    puts(1,"\n")
end for
```

{{out}}

```txt

"for"
Normal                                     : {-2,-1,0,1,2}
Zero increment                             : for loop error, step is 0
Increments away from stop value            : {}
First increment is beyond stop value       : {-2}
Start more than stop: positive increment   : {}
Start equal stop: positive increment       : {2}
Start equal stop: negative increment       : {2}
Start equal stop: zero increment           : for loop error, step is 0
Start equal stop equal zero: zero increment: for loop error, step is 0

"while"
Normal                                     : {-2,-1,0,1,2}
Zero increment                             : {-2,-2,-2,-2,-2,-2,-2,-2,-2,-2}
Increments away from stop value            : {}
First increment is beyond stop value       : {-2}
Start more than stop: positive increment   : {}
Start equal stop: positive increment       : {2}
Start equal stop: negative increment       : {2}
Start equal stop: zero increment           : {2,2,2,2,2,2,2,2,2,2}
Start equal stop equal zero: zero increment: {0,0,0,0,0,0,0,0,0,0}

```



## Python

Python has the [https://docs.python.org/3/library/functions.html#func-range range] function.

```python
import re
from itertools import islice # To limit execution if it would generate huge values 
# list(islice('ABCDEFG', 2)) --> ['A', 'B']
# list(islice('ABCDEFG', 4)) --> ['A', 'B', 'C', 'D']


data = '''
start 	stop 	increment 	Comment
-2 	2 	1 	Normal
-2 	2 	0 	Zero increment
-2 	2 	-1 	Increments away from stop value
-2 	2 	10 	First increment is beyond stop value
2 	-2 	1 	Start more than stop: positive increment
2 	2 	1 	Start equal stop: positive increment
2 	2 	-1 	Start equal stop: negative increment
2 	2 	0 	Start equal stop: zero increment
0 	0 	0 	Start equal stop equal zero: zero increment 
'''

table = [re.split(r'\s\s+', line.strip()) for line in data.strip().split('\n')]
#%%
for _start, _stop, _increment, comment in table[1:]:
    start, stop, increment = [int(x) for x in (_start, _stop, _increment)]
    print(f'{comment.upper()}:\n  range({start}, {stop}, {increment})')
    values = None
    try: 
        values = list(islice(range(start, stop, increment), 999))
    except ValueError as e:
        print('  !!ERROR!!', e)
    if values is not None:
        if len(values) < 22:
            print('    =', values)
        else:
            print('    =', str(values[:22])[:-1], '...')

```


{{out}}

```txt
NORMAL:
  range(-2, 2, 1)
    = [-2, -1, 0, 1]
ZERO INCREMENT:
  range(-2, 2, 0)
  !!ERROR!! range() arg 3 must not be zero
INCREMENTS AWAY FROM STOP VALUE:
  range(-2, 2, -1)
    = []
FIRST INCREMENT IS BEYOND STOP VALUE:
  range(-2, 2, 10)
    = [-2]
START MORE THAN STOP: POSITIVE INCREMENT:
  range(2, -2, 1)
    = []
START EQUAL STOP: POSITIVE INCREMENT:
  range(2, 2, 1)
    = []
START EQUAL STOP: NEGATIVE INCREMENT:
  range(2, 2, -1)
    = []
START EQUAL STOP: ZERO INCREMENT:
  range(2, 2, 0)
  !!ERROR!! range() arg 3 must not be zero
START EQUAL STOP EQUAL ZERO: ZERO INCREMENT:
  range(0, 0, 0)
  !!ERROR!! range() arg 3 must not be zero
```



## Racket



```racket
#lang racket

(require racket/sandbox)

(define tests '([-2  2  1 "Normal"]
                [-2  2  0 "Zero increment"]
                [-2  2 -1 "Increments away from stop value"]
                [-2  2 10 "First increment is beyond stop value"]
                [2  -2  1 "Start more than stop: positive increment"]
                [2   2  1 "Start equal stop: positive increment"]
                [2   2 -1 "Start equal stop: negative increment"]
                [2   2  0 "Start equal stop: zero increment"]
                [0   0  0 "Start equal stop equal zero: zero increment"]))

(for ([test (in-list tests)])
  (match-define (list st ed inc desc) test)
  (printf "~a:\n  (in-range ~a ~a ~a) = ~a\n\n"
          desc st ed inc
          (with-handlers ([exn:fail:resource? (thunk* 'timeout)])
            (with-limits 1 #f
              (sequence->list (in-range st ed inc))))))
```


{{out}}

```txt

Normal:
  (in-range -2 2 1) = (-2 -1 0 1)

Zero increment:
  (in-range -2 2 0) = timeout

Increments away from stop value:
  (in-range -2 2 -1) = ()

First increment is beyond stop value:
  (in-range -2 2 10) = (-2)

Start more than stop: positive increment:
  (in-range 2 -2 1) = ()

Start equal stop: positive increment:
  (in-range 2 2 1) = ()

Start equal stop: negative increment:
  (in-range 2 2 -1) = ()

Start equal stop: zero increment:
  (in-range 2 2 0) = ()

Start equal stop equal zero: zero increment:
  (in-range 0 0 0) = ()


```



## REXX

Note that a '''do''' loop with zero '''by''' value, or a '''do''' loop that goes in the "wrong" direction is not considered an error in REXX as there are other methods of limiting the range (or stopping condition) within the loop body.   A special check was made in this REXX version to check for a runaway (race) condition.

The REXX language will cause the '''do''' loop index to be checked at the "head" of the '''do''' loop to see if the index falls within the specified iteration range   (if there is one).

```rexx
/*REXX program demonstrates several versions of  DO  loops with  "unusual"  iterations. */
@.=;      @.1=  '  -2      2       1  '      /*"normal".                                */
          @.2=  '  -2      2       0  '      /*"normal",                zero  increment.*/
          @.3=  '  -2      2      -1  '      /*increases away from stop, neg  increment.*/
          @.4=  '  -2      2      10  '      /*1st increment > stop, positive increment.*/
          @.5=  '   2     -2       1  '      /*start > stop,         positive increment.*/
          @.6=  '   2      2       1  '      /*start equals stop,    positive increment.*/
          @.7=  '   2      2      -1  '      /*start equals stop,    negative increment.*/
          @.8=  '   2      2       0  '      /*start equals stop,       zero  increment.*/
          @.9=  '   0      0       0  '      /*start equals stop,       zero  increment.*/
zLim= 10                                     /*a limit to check for runaway (race) loop.*/
                                             /*a zero increment is not an error in REXX.*/
  do k=1  while  @.k\==''                    /*perform a  DO  loop with several ranges. */
  parse var   @.k    x  y  z  .              /*obtain the three values for a DO loop.   */
  say
  say center('start of performing DO loop number '   k   " with range: "  x y z,  79, '═')
  zz= 0
        do  j=x   to y   by z   until zz>=zLim           /* ◄───  perform the  DO  loop.*/
        say '   j ───►'  right(j, max(3, length(j) ) )   /*right justify J for alignment*/
        if z==0  then zz= zz + 1                         /*if zero inc, count happenings*/
        end   /*j*/

  if zz>=zLim  then say 'the DO loop for the '    k    " entry was terminated (runaway)."
  say center(' end  of performing DO loop number '   k   " with range: "  x y z,  79, '─')
  say
  end         /*k*/                              /*stick a fork in it,  we're all done. */
```

{{out|output|:}}

```txt

══════════start of performing DO loop number  1  with range:  -2 2 1═══════════
   j ───►  -2
   j ───►  -1
   j ───►   0
   j ───►   1
   j ───►   2
────────── end  of performing DO loop number  1  with range:  -2 2 1───────────


══════════start of performing DO loop number  2  with range:  -2 2 0═══════════
   j ───►  -2
   j ───►  -2
   j ───►  -2
   j ───►  -2
   j ───►  -2
   j ───►  -2
   j ───►  -2
   j ───►  -2
   j ───►  -2
   j ───►  -2
the DO loop for the  2  entry was terminated (runaway).
────────── end  of performing DO loop number  2  with range:  -2 2 0───────────


══════════start of performing DO loop number  3  with range:  -2 2 -1══════════
────────── end  of performing DO loop number  3  with range:  -2 2 -1──────────


══════════start of performing DO loop number  4  with range:  -2 2 10══════════
   j ───►  -2
────────── end  of performing DO loop number  4  with range:  -2 2 10──────────


══════════start of performing DO loop number  5  with range:  2 -2 1═══════════
────────── end  of performing DO loop number  5  with range:  2 -2 1───────────


═══════════start of performing DO loop number  6  with range:  2 2 1═══════════
   j ───►   2
─────────── end  of performing DO loop number  6  with range:  2 2 1───────────


══════════start of performing DO loop number  7  with range:  2 2 -1═══════════
   j ───►   2
────────── end  of performing DO loop number  7  with range:  2 2 -1───────────


═══════════start of performing DO loop number  8  with range:  2 2 0═══════════
   j ───►   2
   j ───►   2
   j ───►   2
   j ───►   2
   j ───►   2
   j ───►   2
   j ───►   2
   j ───►   2
   j ───►   2
   j ───►   2
the DO loop for the  8  entry was terminated (runaway).
─────────── end  of performing DO loop number  8  with range:  2 2 0───────────


═══════════start of performing DO loop number  9  with range:  0 0 0═══════════
   j ───►   0
   j ───►   0
   j ───►   0
   j ───►   0
   j ───►   0
   j ───►   0
   j ───►   0
   j ───►   0
   j ───►   0
   j ───►   0
the DO loop for the  9  entry was terminated (runaway).
─────────── end  of performing DO loop number  9  with range:  0 0 0───────────

```



## Ruby

A Range with a step (without a block) results in an ArthmeticSequence (an object). A step size of zero is perfectly valid.
To illustrate, a representation of the ArithmicSequence and it's size are shown.

```ruby
examples = [
     [ -2,    2,    1],
     [ -2,    2,    0], 
     [ -2,    2,   -1],
     [ -2,    2,   10], 
     [  2,   -2,    1], 
     [  2,    2,    1], 
     [  2,    2,   -1], 
     [  2,    2,    0], 
     [  0,    0,    0]
     ]

examples.each do |start, stop, step|
  as = (start..stop).step(step)
  puts "#{as.inspect} size: #{as.size}"
end

```

{{out}}
```txt
((-2..2).step(1)) size: 5
((-2..2).step(0)) size: Infinity
((-2..2).step(-1)) size: 0
((-2..2).step(10)) size: 1
((2..-2).step(1)) size: 0
((2..2).step(1)) size: 1
((2..2).step(-1)) size: 1
((2..2).step(0)) size: Infinity
((0..0).step(0)) size: Infinity

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: testLoop (in integer: start, in integer: stop, in integer: incr, in string: comment) is func
  local
    const integer: limit is 10;
    var integer: number is 0;
    var integer: count is 0;
  begin
    writeln(comment);
    write("Range(" <& start <& ", " <& stop <& ", " <& incr <& ") -> [ ");
    block
      for number range start to stop step incr do
        write(number <& " ");
        incr(count);
        if count >= limit then
          raise RANGE_ERROR;
        end if;
      end for;
    exception
      catch RANGE_ERROR: noop;
    end block;
    writeln("]");
    writeln;
  end func;

const proc: main is func
  begin
    testLoop(-2,  2,  1, "Normal");
    testLoop(-2,  2,  0, "Zero increment");
    testLoop(-2,  2, -1, "Increments away from stop value");
    testLoop(-2,  2, 10, "First increment is beyond stop value");
    testLoop( 2, -2,  1, "Start more than stop: positive increment");
    testLoop( 2,  2,  1, "Start equal stop: positive increment");
    testLoop( 2,  2, -1, "Start equal stop: negative increment");
    testLoop( 2,  2,  0, "Start equal stop: zero increment");
    testLoop( 0,  0,  0, "Start equal stop equal zero: zero increment");
  end func;
```


{{out}}

```txt

Normal
Range(-2, 2, 1) -> [ -2 -1 0 1 2 ]

Zero increment
Range(-2, 2, 0) -> [ -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 ]

Increments away from stop value
Range(-2, 2, -1) -> [ -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 ]

First increment is beyond stop value
Range(-2, 2, 10) -> [ -2 ]

Start more than stop: positive increment
Range(2, -2, 1) -> [ ]

Start equal stop: positive increment
Range(2, 2, 1) -> [ 2 ]

Start equal stop: negative increment
Range(2, 2, -1) -> [ 2 1 0 -1 -2 -3 -4 -5 -6 -7 ]

Start equal stop: zero increment
Range(2, 2, 0) -> [ 2 2 2 2 2 2 2 2 2 2 ]

Start equal stop equal zero: zero increment
Range(0, 0, 0) -> [ 0 0 0 0 0 0 0 0 0 0 ]

```



## VBA


```vb
Public Sub LoopsWrongRanges()
    Call Example(-2, 2, 1, "Normal")
    Call Example(-2, 2, 0, "Zero increment")
    Call Example(-2, 2, -1, "Increments away from stop value")
    Call Example(-2, 2, 10, "First increment is beyond stop value")
    Call Example(2, -2, 1, "Start more than stop: positive increment")
    Call Example(2, 2, 1, "Start equal stop: positive increment")
    Call Example(2, 2, -1, "Start equal stop: negative increment")
    Call Example(2, 2, 0, "Start equal stop: zero increment")
    Call Example(0, 0, 0, "Start equal stop equal zero: zero increment")
End Sub
Private Sub Example(start As Integer, stop_ As Integer, by As Integer, comment As String)
    Dim i As Integer
    Dim c As Integer
    Const limit = 10
    c = 0
    Debug.Print start; " "; stop_; " "; by; " | ";
    For i = start To stop_ Step by
        Debug.Print i & ",";
        c = c + 1
        If c > limit Then Exit For
    Next i
    Debug.Print
    Debug.Print comment & vbCrLf
End Sub

```
{{out}}
```txt
-2   2   1  | -2,-1,0,1,2,
Normal

-2   2   0  | -2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,
Zero increment

-2   2  -1  | 
Increments away from stop value

-2   2   10  | -2,
First increment is beyond stop value

2  -2   1  | 
Start more than stop: positive increment

2   2   1  | 2,
Start equal stop: positive increment

2   2  -1  | 2,
Start equal stop: negative increment

2   2   0  | 2,2,2,2,2,2,2,2,2,2,2,
Start equal stop: zero increment

0   0   0  | 0,0,0,0,0,0,0,0,0,0,0,
Start equal stop equal zero: zero increment
```



## Visual Basic .NET

'''Compiler:''' >= Visual Studio 2012

VB.NET's For loop accepts a starting and ending value and optional step.

Since the task mentions generators and a range of values, this implementation places the for loop in an iterator function (called a generator in many other languages) and yields the iteration variable in every iteration. The resulting IEnumerable object (whose actual class is generated by the compiler) is lazily-evaluated (i.e., it is run only when a new value is requested, and only until the next Yield statement).

The number of iterations is limited to 10 by the test code.


```vbnet
Module Program
    Sub Main()
        Example(-2, 2, 1, "Normal")
        Example(-2, 2, 0, "Zero increment")
        Example(-2, 2, -1, "Increments away from stop value")
        Example(-2, 2, 10, "First increment is beyond stop value")
        Example(2, -2, 1, "Start more than stop: positive increment")
        Example(2, 2, 1, "Start equal stop: positive increment")
        Example(2, 2, -1, "Start equal stop: negative increment")
        Example(2, 2, 0, "Start equal stop: zero increment")
        Example(0, 0, 0, "Start equal stop equal zero: zero increment")
    End Sub

    ' Stop is a keyword and must be escaped using brackets.
    Iterator Function Range(start As Integer, [stop] As Integer, increment As Integer) As IEnumerable(Of Integer)
        For i = start To [stop] Step increment
            Yield i
        Next
    End Function

    Sub Example(start As Integer, [stop] As Integer, increment As Integer, comment As String)
        ' Add a space, pad to length 50 with hyphens, and add another space.
        Console.Write((comment & " ").PadRight(50, "-"c) & " ")

        Const MAX_ITER = 9

        Dim iteration = 0
        ' The For Each loop enumerates the IEnumerable.
        For Each i In Range(start, [stop], increment)
            Console.Write("{0,2} ", i)

            iteration += 1
            If iteration > MAX_ITER Then Exit For
        Next

        Console.WriteLine()
    End Sub
End Module
```


{{out}}

```txt
Normal ------------------------------------------- -2 -1  0  1  2
Zero increment ----------------------------------- -2 -2 -2 -2 -2 -2 -2 -2 -2 -2
Increments away from stop value ------------------
First increment is beyond stop value ------------- -2
Start more than stop: positive increment ---------
Start equal stop: positive increment -------------  2
Start equal stop: negative increment -------------  2
Start equal stop: zero increment -----------------  2  2  2  2  2  2  2  2  2  2
Start equal stop equal zero: zero increment ------  0  0  0  0  0  0  0  0  0  0
```



## zkl


```zkl
// zero increment (ie infnite loop) throws an error
// if stop is "*", the loop is has no end (ie infinite)
// stop is included unless step steps skips it
// if start > stop is a dead loop
// ranges ([a..b,c]) are lazy lists
fcn looper([(start,stop,increment)]){
   print(" %3s  %3s\t%2d --> ".fmt(start,stop,increment));
   try{ foreach n in ([start..stop,increment]){ print(n," ") } }
   catch{ print(__exception) }
   println();
}
println("start stop  increment");
T( T(-2,2,1),T(-2,2,0),T(-2,2,-1),T(-2,2,10),T( 2,-2,1),
   T( 2,2,1),T( 2,2,-1),T( 2,2,0),T( 0,0,0), 
   T(0.0, (0.0).pi, 0.7853981633974483), T("a","e",1), T("e","a",1) )
.apply2(looper);  // apply2 is apply (map) without saving results
```

{{out}}

```txt

start stop  increment
  -2    2	 1 --> -2 -1 0 1 2 
  -2    2	 0 --> ValueError(range: step == 0)
  -2    2	-1 --> 
  -2    2	10 --> -2 
   2   -2	 1 --> 
   2    2	 1 --> 2 
   2    2	-1 --> 2 
   2    2	 0 --> ValueError(range: step == 0)
   0    0	 0 --> ValueError(range: step == 0)
   0  3.14159	 0 --> 0 0.785398 1.5708 2.35619 3.14159 
   a    e	 1 --> a b c d e 
   e    a	 1 --> 

```


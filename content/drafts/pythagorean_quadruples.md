+++
title = "Pythagorean quadruples"
description = ""
date = 2019-10-22T00:57:21Z
aliases = []
[extra]
id = 21516
[taxonomies]
categories = []
tags = []
+++

{{task}}


One form of   '''Pythagorean quadruples'''   is   (for positive integers   '''a''',   '''b''',   '''c''',   and   '''d'''):


::::::::   <big><big> a<sup>2</sup>   +   b<sup>2</sup>   +   c<sup>2</sup>     =     d<sup>2</sup> </big></big>


An example:

::::::::   <big><big> 2<sup>2</sup>   +   3<sup>2</sup>   +   6<sup>2</sup>     =     7<sup>2</sup> </big></big>

::::: which is:

::::::::   <big><big> 4    +   9    +   36     =     49 </big></big>


;Task:

For positive integers up   '''2,200'''   (inclusive),   for all values of   '''a''',
'''b''',   '''c''',   and   '''d''',

find   (and show here)   those values of   '''d'''   that   ''can't''   be represented.

Show the values of   '''d'''   on one line of output   (optionally with a title).


;Related tasks:
*   [[Euler's sum of powers conjecture]].
*   [[Pythagorean triples]].


;Reference:
:*   the Wikipedia article:   [https://en.wikipedia.org/wiki/Pythagorean_quadruple Pythagorean quadruple].





## ALGOL 68

As with the optimised REXX solution, we find the values of d for which there are no a^2 + b^2 = d^2 - c^2.

```algol68
BEGIN
    # find values of d where d^2 =/= a^2 + b^2 + c^2 for any integers a, b, c #
    # where d in [1..2200], a, b, c =/= 0                                     #
    # max number to check #
    INT max number = 2200;
    INT max square = max number * max number;
    # table of numbers that can be the sum of two squares #
    [ 1 : max square ]BOOL sum of two squares; FOR n TO max square DO sum of two squares[ n ] := FALSE OD;
    FOR a TO max number DO
        INT a2 = a * a;
        FOR b FROM a TO max number WHILE INT sum2 = ( b * b ) + a2;
                                         sum2 <= max square DO
            sum of two squares[ sum2 ] := TRUE
        OD
    OD;
    # now find d such that d^2 - c^2 is in sum of two squares #
    [ 1 : max number ]BOOL solution; FOR n TO max number DO solution[ n ] := FALSE OD;
    FOR d TO max number DO
        INT d2 = d * d;
        FOR c TO d - 1 WHILE NOT solution[ d ] DO
            INT diff2 = d2 - ( c * c );
            IF sum of two squares[ diff2 ] THEN
                solution[ d ] := TRUE
            FI
        OD
    OD;
    # print the numbers whose squares are not the sum of three squares #
    FOR d TO max number DO
        IF NOT solution[ d ] THEN
            print( ( " ", whole( d, 0 ) ) )
        FI
    OD;
    print( ( newline ) )
END
```

{{out}}

```txt

 1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```



## AppleScript


```AppleScript
-- double :: Num -> Num
on double(x)
    x + x
end double

-- powersOfTwo :: Generator [Int]
on powersOfTwo()
    iterate(double, 1)
end powersOfTwo

on run
    -- Two infinite lists, from each of which we can draw an arbitrary number of initial terms

    set xs to powersOfTwo() -- {1, 2, 4, 8, 16, 32 ...

    set ys to fmapGen(timesFive, powersOfTwo()) -- {5, 10, 20, 40, 80, 160 ...


    -- Another infinite list, derived from the first two (sorted in rising value)

    set zs to mergeInOrder(xs, ys) -- {1, 2, 4, 5, 8, 10 ...


    -- Taking terms from the derived list while their value is below 2200 ...

    takeWhileGen(le2200, zs)

    --> {1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048}
end run


-- le2200 :: Num -> Bool
on le2200(x)
    x ≤ 2200
end le2200

-- timesFive :: Num -> Num
on timesFive(x)
    5 * x
end timesFive


-- mergeInOrder :: Generator [Int] -> Generator [Int] -> Generator [Int]
on mergeInOrder(ga, gb)
    script
        property a : uncons(ga)
        property b : uncons(gb)
        on |λ|()
            if (Nothing of a or Nothing of b) then
                missing value
            else
                set ta to Just of a
                set tb to Just of b
                if |1| of ta < |1| of tb then
                    set a to uncons(|2| of ta)
                    return |1| of ta
                else
                    set b to uncons(|2| of tb)
                    return |1| of tb
                end if
            end if
        end |λ|
    end script
end mergeInOrder


-- GENERIC -----------------------------------------------------------------

-- fmapGen <$> :: (a -> b) -> Gen [a] -> Gen [b]
on fmapGen(f, gen)
    script
        property g : gen
        property mf : mReturn(f)'s |λ|
        on |λ|()
            set v to g's |λ|()
            if v is missing value then
                v
            else
                mf(v)
            end if
        end |λ|
    end script
end fmapGen

-- iterate :: (a -> a) -> a -> Gen [a]
on iterate(f, x)
    script
        property v : missing value
        property g : mReturn(f)'s |λ|
        on |λ|()
            if missing value is v then
                set v to x
            else
                set v to g(v)
            end if
            return v
        end |λ|
    end script
end iterate

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- length :: [a] -> Int
on |length|(xs)
    set c to class of xs
    if list is c or string is c then
        length of xs
    else
        (2 ^ 29 - 1) -- (maxInt - simple proxy for non-finite)
    end if
end |length|

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take

-- takeWhileGen :: (a -> Bool) -> Gen [a] -> [a]
on takeWhileGen(p, xs)
    set ys to {}
    set v to |λ|() of xs
    tell mReturn(p)
        repeat while (|λ|(v))
            set end of ys to v
            set v to xs's |λ|()
        end repeat
    end tell
    return ys
end takeWhileGen

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    set lng to |length|(xs)
    if 0 = lng then
        Nothing()
    else
        if (2 ^ 29 - 1) as integer > lng then
            if class of xs is string then
                set cs to text items of xs
                Just(Tuple(item 1 of cs, rest of cs))
            else
                Just(Tuple(item 1 of xs, rest of xs))
            end if
        else
            Just(Tuple(item 1 of take(1, xs), xs))
        end if
    end if
end uncons
```

{{Out}}

```txt
{1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048}
```


## AWK


```AWK

# syntax: GAWK -f PYTHAGOREAN_QUADRUPLES.AWK
# converted from Go
BEGIN {
    n = 2200
    s = 3
    for (a=1; a<=n; a++) {
      a2 = a * a
      for (b=a; b<=n; b++) {
        ab[a2 + b * b] = 1
      }
    }
    for (c=1; c<=n; c++) {
      s1 = s
      s += 2
      s2 = s
      for (d=c+1; d<=n; d++) {
        if (ab[s1]) {
          r[d] = 1
        }
        s1 += s2
        s2 += 2
      }
    }
    for (d=1; d<=n; d++) {
      if (!r[d]) {
        printf("%d ",d)
      }
    }
    printf("\n")
    exit(0)
}

```

{{out}}

```txt

1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```



## C


### Version 1

Five seconds on my Intel Linux box.

```c
#include <stdio.h>
#include <math.h>
#include <string.h>

#define N 2200

int main(int argc, char **argv){
   int a,b,c,d;
   int r[N+1];
   memset(r,0,sizeof(r));	// zero solution array
   for(a=1; a<=N; a++){
      for(b=a; b<=N; b++){
	 int aabb;
	 if(a&1 && b&1) continue;  // for positive odd a and b, no solution.
	 aabb=a*a + b*b;
	 for(c=b; c<=N; c++){
	    int aabbcc=aabb + c*c;
	    d=(int)sqrt((float)aabbcc);
	    if(aabbcc == d*d && d<=N) r[d]=1;	// solution
	 }
      }
   }
   for(a=1; a<=N; a++)
      if(!r[a]) printf("%d ",a);	// print non solution
   printf("\n");
}
```

{{out}}

```txt

$ clang -O3 foo.c -lm
$ ./a.out
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```

===Version 2 (much faster)===
Translation of second version of FreeBASIC entry. Runs in about 0.15 seconds.

```cpp
#include <iostream>
#include <stdio.h>
#include <string.h>

#define N 2200
#define N2 2200 * 2200 * 2

int main(int argc, char **argv) {
    int a, b, c, d, a2, s = 3, s1, s2;
    int r[N + 1];
    memset(r, 0, sizeof(r));
    int *ab = calloc(N2 + 1, sizeof(int));  // allocate on heap, zero filled

    for (a = 1; a <= N; a++) {
        a2 = a * a;
        for (b = a; b <= N; b++) ab[a2 + b * b] = 1;
    }

    for (c = 1; c <= N; c++) {
        s1 = s;
        s += 2;
        s2 = s;
        for (d = c + 1; d <= N; d++) {
            if (ab[s1]) r[d] = 1;
            s1 += s2;
            s2 += 2;
        }
    }

    for (d = 1; d <= N; d++) {
        if (!r[d]) printf("%d ", d);
    }
    printf("\n");
    free(ab);
    return 0;
}
```

{{out}}

```txt

Same as first version.

```



## C++

{{trans|D}}

```cpp
#include <iostream>
#include <vector>

constexpr int N = 2200;
constexpr int N2 = 2 * N * N;

int main() {
    using namespace std;

    vector<bool> found(N + 1);
    vector<bool> aabb(N2 + 1);

    int s = 3;

    for (int a = 1; a < N; ++a) {
        int aa = a * a;
        for (int b = 1; b < N; ++b) {
            aabb[aa + b * b] = true;
        }
    }

    for (int c = 1; c <= N; ++c) {
        int s1 = s;
        s += 2;
        int s2 = s;
        for (int d = c + 1; d <= N; ++d) {
            if (aabb[s1]) {
                found[d] = true;
            }
            s1 += s2;
            s2 += 2;
        }
    }

    cout << "The values of d <= " << N << " which can't be represented:" << endl;
    for (int d = 1; d <= N; ++d) {
        if (!found[d]) {
            cout << d << " ";
        }
    }
    cout << endl;

    return 0;
}
```

{{out}}

```txt
The values of d <= 2200 which can't be represented:
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;

namespace PythagoreanQuadruples {
    class Program {
        const int MAX = 2200;
        const int MAX2 = MAX * MAX * 2;

        static void Main(string[] args) {
            bool[] found = new bool[MAX + 1]; // all false by default
            bool[] a2b2 = new bool[MAX2 + 1]; // ditto
            int s = 3;

            for(int a = 1; a <= MAX; a++) {
                int a2 = a * a;
                for (int b=a; b<=MAX; b++) {
                    a2b2[a2 + b * b] = true;
                }
            }

            for (int c = 1; c <= MAX; c++) {
                int s1 = s;
                s += 2;
                int s2 = s;
                for (int d = c + 1; d <= MAX; d++) {
                    if (a2b2[s1]) found[d] = true;
                    s1 += s2;
                    s2 += 2;
                }
            }

            Console.WriteLine("The values of d <= {0} which can't be represented:", MAX);
            for (int d = 1; d < MAX; d++) {
                if (!found[d]) Console.Write("{0}  ", d);
            }
            Console.WriteLine();
        }
    }
}
```

{{out}}

```txt
The values of d <= 2200 which can't be represented:
1  2  4  5  8  10  16  20  32  40  64  80  128  160  256  320  512  640  1024  1280  2048
```



## Crystal

{{trans|Ruby}}

```Ruby
n = 2200
l_add, l = Hash(Int32, Bool).new(false), Hash(Int32, Bool).new(false)
(1..n).each do |x|
  x2 = x * x
  (x..n).each { |y| l_add[x2 + y * y] = true }
end

s = 3
(1..n).each do |x|
  s1 = s
  s += 2
  s2 = s
  ((x+1)..n).each do |y|
    l[y] = true if l_add[s1]
    s1 += s2
    s2 += 2
  end
end

puts (1..n).reject{ |x| l[x] }.join(" ")
```

{{out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```



## D

{{trans|C}}

```D
import std.bitmanip : BitArray;
import std.stdio;

enum N = 2_200;
enum N2 = 2*N*N;

void main() {
    BitArray found;
    found.length = N+1;

    BitArray aabb;
    aabb.length = N2+1;

    uint s=3;

    for (uint a=1; a<=N; ++a) {
        uint aa = a*a;
        for (uint b=1; b<N; ++b) {
            aabb[aa + b*b] = true;
        }
    }

    for (uint c=1; c<=N; ++c) {
        uint s1 = s;
        s += 2;
        uint s2 = s;
        for (uint d=c+1; d<=N; ++d) {
            if (aabb[s1]) {
                found[d] = true;
            }
            s1 += s2;
            s2 += 2;
        }
    }

    writeln("The values of d <= ", N, " which can't be represented:");
    for (uint d=1; d<=N; ++d) {
        if (!found[d]) {
            write(d, ' ');
        }
    }
    writeln;
}
```


{{out}}

```txt
The values of d <= 2200 which can't be represented:
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```



## FreeBASIC


### From the Wikipedia page

[https://en.wikipedia.org/wiki/Pythagorean_quadruple Alternate parametrization, second version both A and B even.]Time just less then 0.7 second on a AMD Athlon II X4 645 3.34GHz win7 64bit. Program uses one core. When the limit is set to 576 (abs. minimum for 2200), the time is about 0.85 sec.

```freebasic
' version 12-08-2017
' compile with: fbc -s console

#Define max 2200

Dim As UInteger l, m, n, l2, l2m2
Dim As UInteger limit = max * 4 \ 15
Dim As UInteger max2 = limit * limit * 2
ReDim As Ubyte list_1(max2), list_2(max2 +1)

' prime sieve, list_2(l) contains a 0 if l = prime
For l = 4 To max2 Step 2
    list_1(l) = 1
Next
For l = 3 To max2 Step 2
    If list_1(l) = 0 Then
        For m = l * l To max2 Step l * 2
            list_1(m) = 1
        Next
    End If
Next

' we do not need a and b (a and b are even, l = a \ 2, m = b \ 2)
' we only need to find d
For l = 1 To limit
    l2 = l * l
    For m = l To limit
        l2m2 = l2 + m * m
        list_2(l2m2 +1) = 1
        ' if l2m2 is a prime, no other factors exits
        If list_1(l2m2) = 0 Then Continue For
        ' find possible factors of l2m2
        ' if l2m2 is odd, we need only to check the odd divisors
        For n = 2 + (l2m2 And 1) To Fix(Sqr(l2m2 -1)) Step 1 + (l2m2 And 1)
            If l2m2 Mod n = 0 Then
                ' set list_2(x) to 1 if solution is found
                list_2(l2m2 \ n + n) = 1
            End If
        Next
    Next
Next

For l = 1 To max
    If list_2(l) = 0 Then Print l; " ";
Next
Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```


### Brute force

Based on the second REXX version: A^2 + B^2 = D^2 - C^2. Faster then the first version, about 0.2 second

```freebasic
' version 14-08-2017
' compile with: fbc -s console

#Define n 2200

Dim As UInteger s = 3, s1, s2, x, x2, y
ReDim As Ubyte l(n), l_add(n * n * 2)

For x = 1 To n
    x2 = x * x
    For y = x To n
        l_add(x2 + y * y) = 1
    Next
Next

For x = 1 To n
    s1 = s
    s += 2
    s2 = s
    For y = x +1 To n
        If l_add(s1) = 1 Then l(y) = 1
        s1 += s2
        s2 += 2
    Next
Next

For x = 1 To n
    If l(x) = 0 Then Print Str(x); " ";
Next
Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```



## Go

{{trans|FreeBASIC}}

```go
package main

import "fmt"

const (
    N = 2200
    N2 = N * N * 2
)

func main() {
    s  := 3
    var s1, s2 int
    var r  [N + 1]bool
    var ab [N2 + 1]bool

    for a := 1; a <= N; a++ {
        a2 := a * a
        for b := a; b <= N; b++ {
            ab[a2 + b * b] = true
        }
    }

    for c := 1; c <= N; c++ {
        s1 = s
        s += 2
        s2 = s
        for d := c + 1; d <= N; d++ {
            if ab[s1] {
                r[d] = true
            }
            s1 += s2
            s2 += 2
        }
    }

    for d := 1; d <= N; d++ {
        if !r[d] {
            fmt.Printf("%d ", d)
        }
    }
    fmt.Println()
}
```


{{out}}

```txt

1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```



## Haskell


```Haskell
powersOfTwo :: [Int]
powersOfTwo = iterate (2 *) 1

unrepresentable :: [Int]
unrepresentable = merge powersOfTwo ((5 *) <$> powersOfTwo)

merge :: [Int] -> [Int] -> [Int]
merge xxs@(x:xs) yys@(y:ys)
  | x < y = x : merge xs yys
  | otherwise = y : merge xxs ys

main :: IO ()
main = do
  putStrLn "The values of d <= 2200 which can't be represented."
  print $ takeWhile (<= 2200) unrepresentable
```

{{out}}

```txt
The values of d <= 2200 which can't be represented.
[1,2,4,5,8,10,16,20,32,40,64,80,128,160,256,320,512,640,1024,1280,2048]
```



## J

Approach: generate the set of all triple sums of squares, then select the legs for which there aren't any squared "d"s.  The solution is straightforward interactive play.

```j


   Filter =: (#~`)(`:6)

   B =: *: A =: i. >: i. 2200

   S1 =: , B +/ B             NB. S1 is a raveled table of the sums of squares
   S1 =: <:&({:B)Filter S1    NB. remove sums of squares exceeding bound
   S1 =: ~. S1                NB. remove duplicate entries

   S2 =: , B +/ S1
   S2 =: <:&({:B)Filter S2
   S2 =: ~. S2

   RESULT =: (B -.@:e. S2) # A
   RESULT
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048


```



## Java

{{trans|Kotlin}}

```java
public class PythagoreanQuadruple {

    static final int MAX = 2200;
    static final int MAX2 = MAX * MAX * 2;

    public static void main(String[] args) {
        boolean[] found = new boolean[MAX + 1];   // all false by default
        boolean[] a2b2  = new boolean[MAX2 + 1];  // ditto
        int s = 3;

        for (int a = 1; a <= MAX; a++) {
            int a2 = a * a;
            for (int b = a; b <= MAX; b++) a2b2[a2 + b * b] = true;
        }

        for (int c = 1; c <= MAX; c++) {
            int s1 = s;
            s += 2;
            int s2 = s;
            for (int d  = c + 1; d <= MAX; d++) {
                if (a2b2[s1]) found[d] = true;
                s1 += s2;
                s2 += 2;
            }
        }

        System.out.printf("The values of d <= %d which can't be represented:\n", MAX);
        for (int d = 1; d <= MAX; d++) {
            if (!found[d]) System.out.printf("%d  ", d);
        }
        System.out.println();
    }
}
```


{{out}}

```txt

The values of d <= 2200 which can't be represented:
1  2  4  5  8  10  16  20  32  40  64  80  128  160  256  320  512  640  1024  1280  2048

```



## JavaScript

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {
        const xs = takeWhileGen(
            x => 2200 >= x,
            mergeInOrder(
                powersOfTwo(),
                fmapGen(x => 5 * x, powersOfTwo())
            )
        );

        return (
            console.log(JSON.stringify(xs)),
            xs
        );
    }

    // powersOfTwo :: Gen [Int]
    const powersOfTwo = () =>
        iterate(x => 2 * x, 1);

    // mergeInOrder :: Gen [Int] -> Gen [Int] -> Gen [Int]
    const mergeInOrder = (ga, gb) => {
        function* go(ma, mb) {
            let
                a = ma,
                b = mb;
            while (!a.Nothing && !b.Nothing) {
                let
                    ta = a.Just,
                    tb = b.Just;
                if (fst(ta) < fst(tb)) {
                    yield(fst(ta));
                    a = uncons(snd(ta))
                } else {
                    yield(fst(tb));
                    b = uncons(snd(tb))
                }
            }
        }
        return go(uncons(ga), uncons(gb))
    };


    // GENERIC FUNCTIONS ----------------------------

    // fmapGen <$> :: (a -> b) -> Gen [a] -> Gen [b]
    function* fmapGen(f, gen) {
        const g = gen;
        let v = take(1, g);
        while (0 < v.length) {
            yield(f(v))
            v = take(1, g)
        }
    }

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // iterate :: (a -> a) -> a -> Generator [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Returns Infinity over objects without finite length
    // this enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs => xs.length || Infinity;

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        xs.constructor.constructor.name !== 'GeneratorFunction' ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // takeWhileGen :: (a -> Bool) -> Generator [a] -> [a]
    const takeWhileGen = (p, xs) => {
        const ys = [];
        let
            nxt = xs.next(),
            v = nxt.value;
        while (!nxt.done && p(v)) {
            ys.push(v);
            nxt = xs.next();
            v = nxt.value
        }
        return ys;
    };

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // uncons :: [a] -> Maybe (a, [a])
    const uncons = xs => {
        const lng = length(xs);
        return (0 < lng) ? (
            lng < Infinity ? (
                Just(Tuple(xs[0], xs.slice(1))) // Finite list
            ) : (() => {
                const nxt = take(1, xs);
                return 0 < nxt.length ? (
                    Just(Tuple(nxt[0], xs))
                ) : Nothing();
            })() // Lazy generator
        ) : Nothing();
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
[1,2,4,5,8,10,16,20,32,40,64,80,128,160,256,320,512,640,1024,1280,2048]
```



## jq


The following is a direct solution but with some obvious optimizations.
Its main value may be to illustrate how looping with breaks can
be accomplished in jq without `foreach`.  Notice also how
`first/1` is used in `is_pythagorean_quad/0` to avoid unnecessary computation.

```jq
# Emit a proof that the input is a pythagorean quad, or else false
def is_pythagorean_quad:
  . as $d
  | (.*.) as $d2
  | first(
      label $continue_a | range(1; $d) | . as $a | (.*.) as $a2
    |   if 3*$a2 > $d2 then break $continue_a else . end
    | label $continue_b | range($a; $d) | . as $b | (.*.) as $b2
    |   if $a2  + 2 * $b2  > $d2 then break $continue_b else . end
    | (($d2-($a2+$b2)) | sqrt) as $c
    | if ($c | floor) == $c then [$a, $b, $c] else empty end )
  // false;

# The specific task:

[range(1; 2201) | select( is_pythagorean_quad | not )] | join(" ")
```


'''Invocation and Output'''


```txt
jq -r -n -f program.jq
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```



## Julia

{{works with|Julia|0.6}}
{{trans|C}}


```julia
function quadruples(N::Int=2200)
    r  = falses(N)
    ab = falses(2N ^ 2)

    for a in 1:N, b in a:N
        ab[a ^ 2 + b ^ 2] = true
    end

    s = 3
    for c in 1:N
        s1, s, s2 = s, s + 2, s + 2
        for d in c+1:N
            if ab[s1] r[d] = true end
            s1 += s2
            s2 += 2
        end
    end

    return find(.! r)
end

println("Pythagorean quadruples up to 2200: ", join(quadruples(), ", "))
```


{{out}}

```txt
Pythagorean quadruples up to 2200: 1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048
```



## Kotlin


### Version 1

This uses a similar approach to the REXX optimized version. It also takes advantage of a hint in the C entry that there is no solution if both a and b are odd (confirmed by Wikipedia article). Runs in about 7 seconds on my modest laptop which is more than 4 times faster than the brute force version would have been:

```scala
// version 1.1.3

const val MAX = 2200
const val MAX2 = MAX * MAX - 1

fun main(args: Array<String>) {
    val found = BooleanArray(MAX + 1)       // all false by default
    val p2 = IntArray(MAX + 1) { it * it }  // pre-compute squares

    // compute all possible positive values of d * d - c * c and map them back to d
    val dc = mutableMapOf<Int, MutableList<Int>>()
    for (d in 1..MAX) {
        for (c in 1 until d) {
            val diff = p2[d] - p2[c]
            val v = dc[diff]
            if (v == null)
                dc.put(diff, mutableListOf(d))
            else if (d !in v)
                v.add(d)
        }
    }

    for (a in 1..MAX) {
        for (b in 1..a) {
            if ((a and 1) != 0 && (b and 1) != 0) continue
            val sum = p2[a] + p2[b]
            if (sum > MAX2) continue
            val v = dc[sum]
            if (v != null) v.forEach { found[it] = true }
        }
    }
    println("The values of d <= $MAX which can't be represented:")
    for (i in 1..MAX) if (!found[i]) print("$i ")
    println()
}
```


{{out}}

```txt

The values of d <= 2200 which can't be represented:
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```

===Version 2 (much faster)===
This is a translation of the second FreeBASIC version and runs in about the same time (0.2 seconds).

One thing I've noticed about the resulting sequence is that it appears to be an interleaving of the two series 2 ^ n and 5 * (2 ^ n) for n >= 0 though whether it's possible to prove this mathematically I don't know.

```scala
// version 1.1.3

const val MAX = 2200
const val MAX2 = MAX * MAX * 2

fun main(args: Array<String>) {
    val found = BooleanArray(MAX + 1)   // all false by default
    val a2b2  = BooleanArray(MAX2 + 1)  // ditto
    var s = 3

    for (a in 1..MAX) {
        val a2 = a * a
        for (b in a..MAX) a2b2[a2 + b * b] = true
    }

    for (c in 1..MAX) {
        var s1 = s
        s += 2
        var s2 = s
        for (d in (c + 1)..MAX) {
            if (a2b2[s1]) found[d] = true
            s1 += s2
            s2 += 2
        }
    }

    println("The values of d <= $MAX which can't be represented:")
    for (d in 1..MAX) if (!found[d]) print("$d ")
    println()
}
```


{{out}}

```txt

Same as Version 1.

```



## Lua


```lua
-- initialize
local N = 2200
local ar = {}
for i=1,N do
    ar[i] = false
end

-- process
for a=1,N do
    for b=a,N do
        if (a % 2 ~= 1) or (b % 2 ~= 1) then
            local aabb = a * a + b * b
            for c=b,N do
                local aabbcc = aabb + c * c
                local d = math.floor(math.sqrt(aabbcc))
                if (aabbcc == d * d) and (d <= N) then
                    ar[d] = true
                end
            end
        end
    end
    -- print('done with a='..a)
end

-- print
for i=1,N do
    if not ar[i] then
        io.write(i.." ")
    end
end
print()
```

{{out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```


=={{header|Modula-2}}==
{{trans|C}}

```modula2
MODULE PythagoreanQuadruples;
FROM FormatString IMPORT FormatString;
FROM RealMath IMPORT sqrt;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInteger(i : INTEGER);
VAR buffer : ARRAY[0..16] OF CHAR;
BEGIN
    FormatString("%i", buffer, i);
    WriteString(buffer)
END WriteInteger;

(* Main *)
CONST N = 2200;
VAR
    r : ARRAY[0..N] OF BOOLEAN;
    a,b,c,d : INTEGER;
    aabb,aabbcc : INTEGER;
BEGIN
    (* Initialize *)
    FOR a:=0 TO HIGH(r) DO
        r[a] := FALSE
    END;

    (* Process *)
    FOR a:=1 TO N DO
        FOR b:=a TO N DO
            IF (a MOD 2 = 1) AND (b MOD 2 = 1) THEN
                (* For positive odd a and b, no solution *)
                CONTINUE
            END;
            aabb := a*a + b*b;
            FOR c:=b TO N DO
                aabbcc := aabb + c*c;
                d := INT(sqrt(FLOAT(aabbcc)));
                IF (aabbcc = d*d) AND (d <= N) THEN
                    (* solution *)
                    r[d] := TRUE
                END
            END
        END
    END;

    FOR a:=1 TO N DO
        IF NOT r[a] THEN
            (* pritn non-solution *)
            WriteInteger(a);
            WriteString(" ")
        END
    END;
    WriteLn;

    ReadChar
END PythagoreanQuadruples.
```

{{out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```



## Nim

{{trans|FreeBasic}}


### Version 1

<lang>import math
import sequtils

const N = 2_200

var
  d, b = 0
  r = newSeq[bool](N + 1)

for a in 1..N:
  for b in a..N:
    var aabb = 0
    if (a and 1).bool and (b and 1).bool: continue
    aabb = a * a + b * b
    for c in b..N:
      var aabbcc = 0
      aabbcc = aabb + c * c
      d = sqrt(aabbcc.float).int
      if aabbcc == d * d and d <= N: r[d] = true
for i in 1..N:
  if not r[I]: stdout.write i, " "
```


{{out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```



### Version 2

<lang>import sequtils

const N = 2_200
const N2 = N * N * 2

var
  a2, d, s1, s2 = 0
  s = 3
  r = newSeq[bool](N + 1)
  ab = newSeq[bool](N2 + 1)

for a in 1..N:
  a2 = a * a
  for b in a..N:
    ab[a2 + b * b] = true

for c in 1..N:
  s1 = s
  s += 2
  s2 = s
  for d in c+1..N:
    if ab[s1]: r[d] = true
    s1 += s2
    s2 += 2

for d in 1..N:
  if not r[d]: stdout.write d, " "
```


{{out}}

```txt

Same as Version 1.

```


## Pascal

{{works with|Free Pascal}} compiled with fpc 3.2.0 ( 2019.01.10 ) -O4 -Xs

### version 1

Brute froce, but not as brute as [http://rosettacode.org/mw/index.php?title=Pythagorean_quadruples#Ring Ring].Did it ever run?<BR>
Stopping search if limit is reached<BR>

```pascal
program pythQuad;
//find phythagorean Quadrupel up to a,b,c,d <= 2200
//a^2 + b^2 +c^2 = d^2
//find all values of d which are not possible
//brute force
//split in two procedure to reduce register pressure for CPU32

const
  MaxFactor =2200;
  limit = MaxFactor*MaxFactor;
type
  tIdx = NativeUint;
  tSum = NativeUint;

var
  check : array[0..MaxFactor] of boolean;
  checkCnt : LongWord;

procedure Find2(s:tSum;idx:tSum);
//second sum (a*a+b*b) +c*c =?= d*d
var
  s1 : tSum;
  d : tSum;
begin
  d := trunc(sqrt(s+idx*idx));// calculate first sqrt
  For idx := idx to MaxFactor do
  Begin
    s1 := s+idx*idx;
    If s1 <= limit then
    Begin
      while s1 > d*d do //adjust sqrt
        inc(d);
      inc(checkCnt);
      IF s1=d*d then
        check[d] := true;
    end
    else
      Break;
  end;
end;

procedure Find1;
//first sum a*a+b*b
var
  a,b : tIdx;
  s : tSum;
begin
  For a := 1 to MaxFactor do
    For b := a to MaxFactor do
    Begin
      s := a*a+b*b;
      if s < limit then
        Find1(s,b)
      else
        break;
     end;
end;

var
  i : NativeUint;
begin
  Find1;

  For i := 1 to MaxFactor do
    If Not(Check[i]) then
      write(i,' ');
  writeln;
  writeln(CheckCnt,' checks were done');
end.

```

{{out}}

```txt

1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
929605937 checks were done
real    0m2.323s -> 9 cpu-cycles per check on Ryzen 5 1600 3,7 Ghz ( Turbo )

```


### version 2

Using a variant of [http://rosettacode.org/wiki/Pythagorean_quadruples#optimized REXX optimized] optimized<BR>
As I now see the same as [http://rosettacode.org/wiki/Pythagorean_quadruples#ALGOL_68 Algol68]<BR>
Quite fast.

```pascal
program pythQuad_2;
//find phythagorean Quadrupel up to a,b,c,d <= 2200
//a^2 + b^2 +c^2 = d^2
//a^2 + b^2 = d^2-c^2

const
  MaxFactor =2200;
  limit = MaxFactor*MaxFactor;
type
  tIdx = NativeUint;
  tSum = NativeUint;
var
// global variables are initiated with 0 at startUp
  sumA2B2 :array[0..limit] of byte;
  check :  array[0..MaxFactor] of byte;

procedure BuildSumA2B2;
var
  a,b : tIdx;
  s : tSum;
begin
  For a := 1 to MaxFactor do
    For b := 1 to a do
    Begin
      s := a*a+b*b;
      if s < limit then
        sumA2B2[s] := 1
      else
        break;
     end;
end;

procedure CheckDifD2C2;
var
  d,c : tIdx;
  s : tSum;
begin
  For d := 1 to MaxFactor do
    //c < d => (d*d-c*c) > 0
    For c := d-1 downto 1 do
    Begin
      s := d*d-c*c;
      if sumA2B2[s] <> 0 then
        Check[d] := 1;
    end;
end;

var
  i : NativeUint;
begin
  BuildSumA2B2;
  CheckDifD2C2;
  //FindHoles;
  For i := 1 to MaxFactor do
    If Check[i] = 0  then
      write(i,' ');
  writeln;
end.
```

{{Out}}

```txt

1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
real    0m0.018s //4.8 Mb -> Level 3 cache 16 Mb ( Ryzen 5 1600 )

//MaxFactor =22000;484 Mb -> no level X Cache
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048 2560 4096 5120 8192 10240 16384 20480
real    0m4.184s

```



## Perl

{{trans|Perl 6}}

```perl
my $N = 2200;
push @sq, $_**2 for 0 .. $N;
my @not = (0) x $N;
@not[0] = 1;


for my $d (1 .. $N) {
    my $last = 0;
    for my $a (reverse ceiling($d/3) .. $d) {
        for my $b (1 .. ceiling($a/2)) {
            my $ab = $sq[$a] + $sq[$b];
            last if $ab > $sq[$d];
            my $x = sqrt($sq[$d] - $ab);
            if ($x == int $x) {
                $not[$d] = 1;
                $last = 1;
                last
            }
        }
        last if $last;
    }
}

sub ceiling { int $_[0] + 1 - 1e-15 }

for (0 .. $#not) {
    $result .= "$_ " unless $not[$_]
}
print "$result\n"
```

{{out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048
```



## Perl 6

{{works with|Rakudo|2018.09}}


```perl6
my \N = 2200;
my @sq = (0 .. N)»²;
my @not = False xx N;
@not[0] = True;

(1 .. N).race.map: -> $d {
    my $last = 0;
    for $d ... ($d/3).ceiling -> $a {
        for 1 .. ($a/2).ceiling -> $b {
            last if (my $ab = @sq[$a] + @sq[$b]) > @sq[$d];
            if (@sq[$d] - $ab).sqrt.narrow ~~ Int {
                @not[$d] = True;
                $last = 1;
                last
            }
        }
        last if $last;
    }
}

say @not.grep( *.not, :k );
```

{{out}}

```txt
(1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048)
```



## Phix


```Phix
constant N = 2200,
         N2 = N*N*2
sequence found = repeat(false,N),
         squares = repeat(false,N2)
-- first mark all numbers that can be the sum of two squares
for a=1 to N do
    integer a2 = a*a
    for b=a to N do
        squares[a2+b*b] = true
    end for
end for
-- now find all d such that d^2 - c^2 is in squares
for d=1 to N do
    integer d2 = d*d
    for c=1 to d-1 do
        if squares[d2-c*c] then
            found[d] = true
            exit
        end if
    end for
end for

sequence res = {}
for i=1 to N do
    if not found[i] then res &= i end if
end for
?res
```

{{out}}

```txt

{1,2,4,5,8,10,16,20,32,40,64,80,128,160,256,320,512,640,1024,1280,2048}

```



## PicoLisp

{{trans|C}}

```PicoLisp
(de quadruples (N)
   (let (AB NIL  S 3  R)
      (for A N
         (for (B A (>= N B) (inc B))
            (idx
               'AB
               (+ (* A A) (* B B))
               T ) ) )
      (for C N
         (let (S1 S  S2)
            (inc 'S 2)
            (setq S2 S)
            (for (D (+ C 1) (>= N D) (inc D))
               (and (idx 'AB S1) (idx 'R D T))
               (inc 'S1 S2)
               (inc 'S2 2) ) ) )
      (make
         (for A N
            (or (idx 'R A) (link A)) ) ) ) )

(println (quadruples 2200))
```


{{out}}

```txt
(1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048)
```



## Python


### Search

{{trans|Julia}}

```Python
def quad(top=2200):
    r = [False] * top
    ab = [False] * (top * 2)**2
    for a in range(1, top):
        for b in range(a, top):
            ab[a * a + b * b] = True
    s = 3
    for c in range(1, top):
        s1, s, s2 = s, s + 2, s + 2
        for d in range(c + 1, top):
            if ab[s1]:
                r[d] = True
            s1 += s2
            s2 += 2
    return [i for i, val in enumerate(r) if not val and i]

if __name__ == '__main__':
    n = 2200
    print(f"Those values of d in 1..{n} that can't be represented: {quad(n)}")
```


{{out}}

```txt
Those values of d in 1..2200 that can't be represented: [1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048]
```



### Composition of simpler generators

Or, as an alternative to search – a generative solution (defining the generator we need as a composition of simpler generators):
{{Trans|Haskell}}
{{Trans|JavaScript}}
{{Trans|AppleScript}}

```Python
from itertools import (islice)


# main :: IO ()
def main():
    print (
        takeWhileGen(lambda x: 2200 > x)(
            mergeInOrder(powersOfTwo())(
                map(lambda x: 5 * x, powersOfTwo())
            )
        )
    )


# powersOfTwo :: Gen [Int]
def powersOfTwo():
    return iterate(lambda x: 2 * x)(1)


# mergeInOrder :: Gen [Int] -> Gen [Int] -> Gen [Int]
def mergeInOrder(ga):
    def go(ma, mb):
        a = ma
        b = mb
        while not a['Nothing'] and not b['Nothing']:
            ta = a['Just']
            tb = b['Just']
            if ta[0] < tb[0]:
                yield(ta[0])
                a = uncons(ta[1])
            else:
                yield(tb[0])
                b = uncons(tb[1])

    return lambda gb: go(uncons(ga), uncons(gb))


# GENERIC ABSTRACTIONS ------------------------------------


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    def go(x):
        v = x
        while True:
            yield(v)
            v = f(v)
    return lambda x: go(x)


# Just :: a -> Maybe a
def Just(x):
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    return {'type': 'Maybe', 'Nothing': True}


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# takeWhileGen :: (a -> Bool) -> Gen [a] -> [a]
def takeWhileGen(p):
    def go(xs):
        vs = []
        v = next(xs)
        while (None is not v and p(v)):
            vs.append(v)
            v = next(xs)
        return vs
    return lambda xs: go(xs)


# uncons :: [a] -> Maybe (a, [a])
def uncons(xs):
    if isinstance(xs, list):
        return Just((xs[0], xs[1:])) if 0 < len(xs) else Nothing()
    else:
        nxt = take(1)(xs)
        return Just((nxt[0], xs)) if 0 < len(nxt) else Nothing()


# MAIN ---
main()
```

{{Out}}

```txt
[1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048]
```



## Racket


{{trans|Python}}


```racket
#lang racket

(require data/bit-vector)

(define (quadruples top)
  (define top+1 (add1 top))
  (define 1..top (in-range 1 top+1))
  (define r (make-bit-vector top+1))
  (define ab (make-bit-vector (add1 (sqr (* top 2)))))
  (for* ((a 1..top) (b (in-range a top+1))) (bit-vector-set! ab (+ (sqr a) (sqr b)) #t))

  (for/fold ((s 3))
            ((c 1..top))
    (for/fold ((s1 s) (s2 (+ s 2)))
              ((d (in-range (add1 c) top+1)))
      (when (bit-vector-ref ab s1)
        (bit-vector-set! r d #t))
      (values (+ s1 s2) (+ s2 2)))
    (+ 2 s))

  (for/list ((i (in-naturals 1)) (v (in-bit-vector r 1)) #:unless v) i))

(define (report n)
  (printf "Those values of d in 1..~a that can't be represented: ~a~%" n (quadruples n)))

(report 2200)
```


{{out}}


```txt
Those values of d in 1..2200 that can't be represented: (1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048)
```



## REXX


### brute force

This version is a brute force algorithm, with some optimization (to save compute time)

which pre-computes some of the squares of the positive integers used in the search.

```rexx
/*REXX pgm computes/shows (integers),  D  that aren't possible for: a² + b² + c²  =  d² */
parse arg hi .                                   /*obtain optional argument from the CL.*/
if hi=='' | hi==","  then hi=2200;  high= 3 * hi /*Not specified?  Then use the default.*/
@.=.                                             /*array of integers to be squared.     */
!.=.                                             /*  "    "     "    squared.           */
       do j=1  for high                          /*precompute possible squares (to max).*/
       _= j*j;   !._= j;   if j<=hi  then @.j= _ /*define a square; D  value; squared # */
       end   /*j*/
d.=.                                             /*array of possible solutions  (D)     */
       do       a=1  for hi-2;  aodd= a//2       /*go hunting for solutions to equation.*/
          do    b=a   to hi-1;
          if aodd  then  if b//2  then iterate   /*Are  A  and  B  both odd?  Then skip.*/
          ab = @.a + @.b                         /*calculate sum of  2  (A,B)   squares.*/
             do c=b   to hi;     abc= ab  + @.c  /*    "      "   "  3  (A,B,C)    "    */
             if !.abc==.  then iterate           /*Not a square? Then skip it*/
             s=!.abc;    d.s=                    /*define this D solution as being found*/
             end   /*c*/
          end      /*b*/
       end         /*a*/
say
say 'Not possible positive integers for   d ≤' hi "  using equation:  a² + b² + c²  =  d²"
say
$=                                               /* [↓]  find all the  "not possibles". */
       do p=1  for hi;   if d.p==.  then $=$ p   /*Not possible? Then add it to the list*/
       end   /*p*/                               /* [↓]  display list of not-possibles. */
say substr($, 2)                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

Not possible positive integers for   d ≤ 2200   using equation:  a² + b² + c²  =  d²

1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```



### optimized

This REXX version is an optimized version, it solves the formula:

::::::::   <big><big> a<sup>2</sup>   +   b<sup>2</sup>         =         d<sup>2</sup>   -   c<sup>2</sup>
</big></big>

This REXX version is around   '''60'''   times faster then the previous version.

Programming note:   testing for   '''a'''   and   '''b'''   both being   <big>odd</big>   (lines '''15''' and '''16'''   that each contain a   '''do'''   loop)   as

being a case that won't produce any solutions actually slows up the calculations and makes the program execute slower.

```rexx
/*REXX pgm computes/shows (integers),  D  that aren't possible for: a² + b² + c²  =  d² */
parse arg hi .                                   /*obtain optional argument from the CL.*/
if hi=='' | hi==","  then hi=2200                /*Not specified?  Then use the default.*/
high= hi * 3                                     /*D  can be three times the  HI  (max).*/
@.= .                                            /*array of integers  (≤ hi)    squared.*/
      do s=1  for high;  _= s*s;  r._= s;  @.s=_ /*precompute squares and square roots. */
      end  /*s*/
!.=                                              /*array of differences between squares.*/
      do    c=1   for high;       cc = @.c       /*precompute possible differences.     */
         do d=c+1  to high;       dif= @.d - cc  /*process  D  squared; calc differences*/
         !.dif= !.dif cc                         /*add    CC    to the    !.DIF   list. */
         end   /*d*/
      end      /*c*/
d.=.                                             /*array of the possible solutions (D). */
      do     a=1  for hi-2                       /*go hunting for solutions to equation.*/
         do  b=a   to hi-1;        ab= @.a + @.b /*calculate sum of two  (A,B)  squares.*/
         if !.ab==''  then iterate               /*Not a difference?   Then ignore it.  */
            do n=1  for words(!.ab)              /*handle all ints that satisfy equation*/
            abc= ab  +  word(!.ab, n)            /*add the  C²  integer  to  A²  +  B²  */
            _= r.abc                             /*retrieve the square root  of  C²     */
            d._=                                 /*mark the  D  integer as being found. */
            end   /*n*/
         end      /*b*/
      end         /*a*/
say
say 'Not possible positive integers for   d ≤' hi "  using equation:  a² + b² + c²  =  d²"
say
$=                                               /* [↓]  find all the  "not possibles". */
       do p=1  for hi;   if d.p==.  then $= $ p  /*Not possible? Then add it to the list*/
       end   /*p*/                               /* [↓]  display list of not-possibles. */
say substr($, 2)                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is the same as the 1<sup>st</sup> REXX version.}}




## Ring


```ring
# Project : Pythagorean quadruples

limit = 2200
pq = list(limit)
for n = 1 to limit
      for m = 1 to limit
           for p = 1 to limit
                 for x = 1 to limit
                       if pow(x,2) = pow(n,2) + pow(m,2) + pow(p,2)
                          pq[x] = 1
                       ok
                 next
           next
      next
next
pqstr = ""
for d = 1 to limit
      if pq[d] = 0
         pqstr = pqstr + d + " "
      ok
next
see pqstr + nl

```

{{Out}}
 1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048


## Ruby

{{trans|VBA}}

```Ruby
n = 2200
l_add, l = {}, {}
1.step(n) do |x|
  x2 = x*x
  x.step(n) {|y| l_add[x2 + y*y] = true}
end

s = 3
1.step(n) do |x|
  s1 = s
  s += 2
  s2 = s
  (x+1).step(n) do |y|
    l[y] = true if l_add[s1]
    s1 += s2
    s2 += 2
  end
end

puts (1..n).reject{|x| l[x]}.join(" ")

```

{{Out}}

```txt
1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```

Considering the observations in the Rust and Sidef sections and toying with Enumerators :

```Ruby
squares  = Enumerator.new{|y| (0..).each{|n| y << 2**n} }
squares5 = Enumerator.new{|y| (0..).each{|n| y << 2**n*5} }

pyth_quad = Enumerator.new do |y|
  n = squares.next
  m = squares5.next
  loop do
    if n < m
      y << n
      n = squares.next
    else
      y << m
      m = squares5.next
    end
  end
end
# this takes less than a millisecond
puts pyth_quad.take_while{|n| n <= 1000000000}.join(" ")
```

{{Out}}

```txt

1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048 2560 4096 5120 8192 10240 16384 20480 32768 40960 65536 81920 131072 163840 262144 327680 524288 655360 1048576 1310720 2097152 2621440 4194304 5242880 8388608 10485760 16777216 20971520 33554432 41943040 67108864 83886080 134217728 167772160 268435456 335544320 536870912 671088640

```



## Rust


{{output?}}

This is equivalent to https://oeis.org/A094958
which simply contains positive integers of the form 2^n or 5*2^n. Multiple implementations are provided.


```rust

use std::collections::BinaryHeap;

fn a094958_iter() -> Vec<u16> {
    (0..12)
        .map(|n| vec![1 << n, 5 * (1 << n)])
        .flatten()
        .filter(|x| x < &2200)
        .collect::<BinaryHeap<u16>>()
        .into_sorted_vec()
}

fn a094958_filter() -> Vec<u16> {
    (1..2200) // ported from Sidef
        .filter(|n| ((n & (n - 1) == 0) || (n % 5 == 0 && ((n / 5) & (n / 5 - 1) == 0))))
        .collect()
}

fn a094958_loop() -> Vec<u16> {
    let mut v = vec![];
    for n in 0..12 {
        v.push(1 << n);
        if 5 * (1 << n) < 2200 {
            v.push(5 * (1 << n));
        }
    }
    v.sort();
    return v;
}

fn main() {
    println!("{:?}", a094958_iter());
    println!("{:?}", a094958_loop());
    println!("{:?}", a094958_filter());
}

#[cfg(test)]
mod tests {
    use super::*;
    static HAPPY: &str = "[1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048]";
    #[test]
    fn test_a094958_iter() {
        assert!(format!("{:?}", a094958_iter()) == HAPPY);
    }
    #[test]
    fn test_a094958_loop() {
        assert!(format!("{:?}", a094958_loop()) == HAPPY);
    }
    #[test]
    fn test_a094958_filter() {
        assert!(format!("{:?}", a094958_filter()) == HAPPY);
    }
}

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/drfij1d/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/6AHn7YXSRbKHzmOY5rWAwg Scastie (remote JVM)].

```Scala
object PythagoreanQuadruple extends App {
  val MAX = 2200
  val MAX2: Int = MAX * MAX * 2
  val found = Array.ofDim[Boolean](MAX + 1)
  val a2b2 = Array.ofDim[Boolean](MAX2 + 1)
  var s = 3
  for (a <- 1 to MAX) {
    val a2 = a * a

    for (b <- a to MAX) a2b2(a2 + b * b) = true
  }

  for (c <- 1 to MAX) {
    var s1 = s
    s += 2
    var s2 = s
    for (d <- (c + 1) to MAX) {
      if (a2b2(s1)) found(d) = true
      s1 += s2
      s2 += 2
    }
  }

  println(f"The values of d <= ${MAX}%d which can't be represented:")
  val notRepresented = (1 to MAX).filterNot(d =>  found(d) )
  println(notRepresented.mkString(" "))

}
```



## Sidef


```ruby
# Finds all solutions (a,b) such that: a^2 + b^2 = n^2
func sum_of_two_squares(n) is cached {

    n == 0 && return [[0, 0]]

    var prod1 = 1
    var prod2 = 1

    var prime_powers = []

    for p,e in (n.factor_exp) {
        if (p % 4 == 3) {                  # p = 3 (mod 4)
            e.is_even || return []         # power must be even
            prod2 *= p**(e >> 1)
        }
        elsif (p == 2) {                   # p = 2
            if (e.is_even) {               # power is even
                prod2 *= p**(e >> 1)
            }
            else {                         # power is odd
                prod1 *= p
                prod2 *= p**((e - 1) >> 1)
                prime_powers.append([p, 1])
            }
        }
        else {                             # p = 1 (mod 4)
            prod1 *= p**e
            prime_powers.append([p, e])
        }
    }

    prod1 == 1 && return [[prod2, 0]]
    prod1 == 2 && return [[prod2, prod2]]

    # All the solutions to the congruence: x^2 = -1 (mod prod1)
    var square_roots = gather {
        gather {
            for p,e in (prime_powers) {
                var pp = p**e
                var r = sqrtmod(-1, pp)
                take([[r, pp], [pp - r, pp]])
            }
        }.cartesian { |*a|
            take(Math.chinese(a...))
        }
    }

    var solutions = []

    for r in (square_roots) {

        var s = r
        var q = prod1

        while (s*s > prod1) {
            (s, q) = (q % s, s)
        }

        solutions.append([prod2 * s, prod2 * (q % s)])
    }

    for p,e in (prime_powers) {
        for (var i = e%2; i < e; i += 2) {

            var sq = p**((e - i) >> 1)
            var pp = p**(e - i)

            solutions += (
                __FUNC__(prod1 / pp).map { |pair|
                    pair.map {|r| sq * prod2 * r }
                }
            )
        }
    }

    solutions.map     {|pair| pair.sort } \
             .uniq_by {|pair| pair[0]   } \
             .sort_by {|pair| pair[0]   }
}

# Finds all solutions (a,b,c) such that: a^2 + b^2 + c^2 = n^2
func sum_of_three_squares(n) {
    gather {
        for k in (1 .. n//3) {
            var t = sum_of_two_squares(n**2 - k**2) || next
            take(t.map { [k, _...] }...)
        }
    }
}

say gather {
    for n in (1..2200) {
        sum_of_three_squares(n) || take(n)
    }
}
```

{{out}}

```txt

[1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048]

```


Numbers d that cannot be expressed as a^2 + b^2 + c^2 = d^2, are numbers of the form 2^n or 5*2^n:

```ruby
say gather {
    for n in (1..2200) {
        if ((n & (n-1) == 0) || (n%%5 && ((n/5) & (n/5 - 1) == 0))) {
            take(n)
        }
    }
}
```

{{out}}

```txt

[1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048]

```



## Swift

{{trans|C}}

```swift
func missingD(upTo n: Int) -> [Int] {
  var a2 = 0, s = 3, s1 = 0, s2 = 0
  var res = [Int](repeating: 0, count: n + 1)
  var ab = [Int](repeating: 0, count: n * n * 2 + 1)

  for a in 1...n {
    a2 = a * a

    for b in a...n {
      ab[a2 + b * b] = 1
    }
  }

  for c in 1..<n {
    s1 = s
    s += 2
    s2 = s

    for d in c+1...n {
      if ab[s1] != 0 {
        res[d] = 1
      }

      s1 += s2
      s2 += 2
    }
  }

  return (1...n).filter({ res[$0] == 0 })
}

print(missingD(upTo: 2200))
```


{{out}}


```txt
[1, 2, 4, 5, 8, 10, 16, 20, 32, 40, 64, 80, 128, 160, 256, 320, 512, 640, 1024, 1280, 2048]
```



## VBA

{{trans|FreeBasic}}

```vb
Const n = 2200
Public Sub pq()
    Dim s As Long, s1 As Long, s2 As Long, x As Long, x2 As Long, y As Long: s = 3
    Dim l(n) As Boolean, l_add(9680000) As Boolean '9680000=n * n * 2
    For x = 1 To n
        x2 = x * x
        For y = x To n
            l_add(x2 + y * y) = True
        Next y
    Next x
    For x = 1 To n
        s1 = s
        s = s + 2
        s2 = s
        For y = x + 1 To n
            If l_add(s1) Then l(y) = True
            s1 = s1 + s2
            s2 = s2 + 2
        Next
    Next
    For x = 1 To n
        If Not l(x) Then Debug.Print x;
    Next
    Debug.Print
End Sub
```
{{out}}

```txt
 1  2  4  5  8  10  16  20  32  40  64  80  128  160  256  320  512  640  1024  1280  2048
```


## zkl

{{trans|ALGOL 68}}

```zkl
# find values of d where d^2 =/= a^2 + b^2 + c^2 for any integers a, b, c #
# where d in [1..2200], a, b, c =/= 0                                     #
# max number to check #
const max_number = 2200;
const max_square = max_number * max_number;
# table of numbers that can be the sum of two squares #
sum_of_two_squares:=Data(max_square+1,Int).fill(0);  # 4 meg byte array
foreach a in ([1..max_number]){
   a2 := a * a;
   foreach b in ([a..max_number]){
      sum2 := ( b * b ) + a2;
      if(sum2 <= max_square) sum_of_two_squares[ sum2 ] = True;  # True-->1
   }
}
# now find d such that d^2 - c^2 is in sum of two squares #
solution:=Data(max_number+1,Int).fill(0);	# another byte array
foreach d in ([1..max_number]){
   d2 := d * d;
   foreach c in ([1..d-1]){
      diff2 := d2 - ( c * c );
      if(sum_of_two_squares[ diff2 ]){ solution[ d ] = True; break; }
   }
}
# print the numbers whose squares are not the sum of three squares #
foreach d in ([1..max_number]){
   if(not solution[ d ]) print(d, " ");
}
println();
```

{{out}}

```txt

1 2 4 5 8 10 16 20 32 40 64 80 128 160 256 320 512 640 1024 1280 2048

```


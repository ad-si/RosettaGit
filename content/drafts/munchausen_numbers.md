+++
title = "Munchausen numbers"
description = ""
date = 2019-09-10T20:53:28Z
aliases = []
[extra]
id = 21120
[taxonomies]
categories = []
tags = []
+++

{{task}}

A [[wp:Munchausen number|Munchausen number]] is a natural number ''n'' the sum of whose digits (in base 10), each raised to the power of itself, equals ''n''.

('''Munchausen''' is also spelled: '''Münchhausen'''.)

For instance:   <big> 3435 = 3<sup>3</sup> + 4<sup>4</sup> + 3<sup>3</sup> + 5<sup>5</sup> </big>


;Task
Find all Munchausen numbers between 1 and 5000


;Also see:
:* The OEIS entry: [[oeis:A046253| A046253]]
:* The Wikipedia entry: [[wp:Perfect_digit-to-digit_invariant| Perfect digit-to-digit invariant, redirected from ''Munchausen Number'']]


## 360 Assembly


```360asm
*        Munchausen numbers        16/03/2019
MUNCHAU  CSECT
         USING  MUNCHAU,R12        base register
         LR     R12,R15            set addressability
         L      R3,=F'5000'        for do i=1 to 5000
         LA     R6,1               i=1
LOOPI    SR     R10,R10              s=0
         LR     R0,R6                ii=i
         LA     R11,4                for do j=1 to 4
         LA     R7,P10               j=1
LOOPJ    L      R8,0(R7)               d=p10(j)
         LR     R4,R0                  ii
         SRDA   R4,32                  ~
         DR     R4,R8                  (n,r)=ii/d
         SLA    R5,2                   ~
         L      R1,POW(R5)             pow(n+1)
         AR     R10,R1                 s=s+pow(n+1)
         LR     R0,R4                  ii=r
         LA     R7,4(R7)               j++
         BCT    R11,LOOPJ            enddo j
         CR     R10,R6               if s=i
         BNE    SKIP                 then
         XDECO  R6,PG                  edit i
         XPRNT  PG,L'PG                print i
SKIP     LA     R6,1(R6)             i++
         BCT    R3,LOOPI           enddo i
         BR     R14                return to caller
POW      DC     F'0',F'1',F'4',F'27',F'256',F'3125',4F'0'
P10      DC     F'1000',F'100',F'10',F'1'
PG       DC     CL12' '            buffer
         REGEQU
         END    MUNCHAU
```

{{out}}

```txt

           1
        3435

```



## ALGOL 68


```algol68
# Find Munchausen Numbers between 1 and 5000                                        #
# note that 6^6 is 46 656 so we only need to consider numbers consisting of 0 to 5   #

# table of Nth powers - note 0^0 is 0 for Munchausen numbers, not 1                 #
[]INT nth power = ([]INT( 0, 1, 2 * 2, 3 * 3 * 3, 4 * 4 * 4 * 4, 5 * 5 * 5 * 5 * 5 ))[ AT 0 ];

INT d1 := 0; INT d1 part := 0;
INT d2 := 0; INT d2 part := 0;
INT d3 := 0; INT d3 part := 0;
INT d4 := 1;
WHILE d1 < 6 DO
    INT number           = d1 part + d2 part + d3 part + d4;
    INT digit power sum := nth power[ d1 ]
                         + nth power[ d2 ]
                         + nth power[ d3 ]
                         + nth power[ d4 ];
    IF digit power sum = number THEN
        print( ( whole( number, 0 ), newline ) )
    FI;
    d4 +:= 1;
    IF d4 > 5 THEN
        d4       := 0;
        d3      +:= 1;
        d3 part +:= 10;
        IF d3 > 5 THEN
            d3       := 0;
            d3 part  := 0;
            d2      +:= 1;
            d2 part +:= 100;
            IF d2 > 5 THEN
                d2       := 0;
                d2 part  := 0;
                d1      +:= 1;
                d1 part +:= 1000;
            FI
        FI
    FI
OD

```

{{out}}

```txt

1
3435

```


Alternative that finds all 4 Munchausen numbers. As noted by the Pascal sample, we only need to consider one arrangement of the digits of each number (e.g. we only need to consider 3345, not 3435, 3453, etc.). This also relies on the non-standard 0^0 = 0.

```algol68
# Find all Munchausen numbers - note 11*(9^9) has only 10 digits so there are no    #
# Munchausen numbers with 11+ digits                                                #
# table of Nth powers - note 0^0 is 0 for Munchausen numbers, not 1                 #
[]INT nth power = ([]INT( 0, 1, 2 ^ 2, 3 ^ 3, 4 ^ 4, 5 ^ 5, 6 ^ 6, 7 ^ 7, 8 ^ 8, 9 ^ 9 ) )[ AT 0 ];

[       ]INT z count  = []INT( ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ) )[ AT 0 ];
[ 0 : 9 ]INT d count := z count;

# as the digit power sum is independent of the order of the digits, we need only    #
# consider one arrangement of each possible combination of digits                   #
FOR d1 FROM 0 TO 9 DO
    FOR d2 FROM 0 TO d1 DO
        FOR d3 FROM 0 TO d2 DO
            FOR d4 FROM 0 TO d3 DO
                FOR d5 FROM 0 TO d4 DO
                    FOR d6 FROM 0 TO d5 DO
                        FOR d7 FROM 0 TO d6 DO
                            FOR d8 FROM 0 TO d7 DO
                                FOR d9 FROM 0 TO d8 DO
                                    FOR da FROM 0 TO d9 DO
                                        LONG INT digit power sum  := nth power[ d1 ] + nth power[ d2 ];
                                        digit power sum          +:= nth power[ d3 ] + nth power[ d4 ];
                                        digit power sum          +:= nth power[ d5 ] + nth power[ d6 ];
                                        digit power sum          +:= nth power[ d7 ] + nth power[ d8 ];
                                        digit power sum          +:= nth power[ d9 ] + nth power[ da ];
                                        # count the occurrences of each digit (including leading zeros #
                                        d count        := z count;
                                        d count[ d1 ] +:= 1; d count[ d2 ] +:= 1; d count[ d3 ] +:= 1;
                                        d count[ d4 ] +:= 1; d count[ d5 ] +:= 1; d count[ d6 ] +:= 1;
                                        d count[ d7 ] +:= 1; d count[ d8 ] +:= 1; d count[ d9 ] +:= 1;
                                        d count[ da ] +:= 1;
                                        # subtract the occurrences of each digit in the power sum      #
                                        # (also including leading zeros) - if all counts drop to 0 we  #
                                        # have a Munchausen number                                     #
                                        LONG INT number        := digit power sum;
                                        INT      leading zeros := 10;
                                        WHILE number > 0 DO
                                            d count[ SHORTEN ( number MOD 10 ) ] -:= 1;
                                            leading zeros -:= 1;
                                            number OVERAB 10
                                        OD;
                                        d count[ 0 ] -:= leading zeros;
                                        IF  d count[ 0 ] = 0 AND d count[ 1 ] = 0 AND d count[ 2 ] = 0
                                        AND d count[ 3 ] = 0 AND d count[ 4 ] = 0 AND d count[ 5 ] = 0
                                        AND d count[ 6 ] = 0 AND d count[ 7 ] = 0 AND d count[ 8 ] = 0
                                        AND d count[ 9 ] = 0
                                        THEN
                                            print( ( digit power sum, newline ) )
                                        FI
                                    OD
                                OD
                            OD
                        OD
                    OD
                OD
            OD
        OD
    OD
OD
```

{{out}}

```txt

                                  +0
                                  +1
                               +3435
                          +438579088

```



## ALGOL W

{{Trans|ALGOL 68}}

```algolw
% Find Munchausen Numbers between 1 and 5000                                         %
% note that 6^6 is 46 656 so we only need to consider numbers consisting of 0 to 5   %
begin

    % table of nth Powers - note 0^0 is 0 for Munchausen numbers, not 1              %
    integer array nthPower( 0 :: 5 );
    integer d1, d2, d3, d4, d1Part, d2Part, d3Part;
    nthPower( 0 ) := 0;             nthPower( 1 ) := 1;
    nthPower( 2 ) := 2 * 2;         nthPower( 3 ) := 3 * 3 * 3;
    nthPower( 4 ) := 4 * 4 * 4 * 4; nthPower( 5 ) := 5 * 5 * 5 * 5 * 5;
    d1 := d2 := d3 := d1Part := d2Part := d3Part := 0;
    d4 := 1;
    while d1 < 6 do begin
        integer number, digitPowerSum;
        number        := d1Part + d2Part + d3Part + d4;
        digitPowerSum := nthPower( d1 )
                       + nthPower( d2 )
                       + nthPower( d3 )
                       + nthPower( d4 );
        if digitPowerSum = number then begin
            write( i_w := 1, number )
        end;
        d4 := d4 + 1;
        if d4 > 5 then begin
            d4     := 0;
            d3     := d3 + 1;
            d3Part := d3Part + 10;
            if d3 > 5 then begin
                d3     := 0;
                d3Part := 0;
                d2     := d2 + 1;
                d2Part := d2Part + 100;
                if d2 > 5 then begin
                    d2     := 0;
                    d2Part := 0;
                    d1     := d1 + 1;
                    d1Part := d1Part + 1000;
                end
            end
        end
    end

end.
```

{{out}}

```txt

1
3435

```



## AppleScript



```AppleScript
-- MUNCHAUSEN NUMBER ? -------------------------------------------------------

-- isMunchausen :: Int -> Bool
on isMunchausen(n)

    -- digitPowerSum :: Int -> Character -> Int
    script digitPowerSum
        on |λ|(a, c)
            set d to c as integer
            a + (d ^ d)
        end |λ|
    end script

    (class of n is integer) and ¬
        foldl(digitPowerSum, 0, characters of (n as string)) = n

end isMunchausen


-- TEST ----------------------------------------------------------------------
on run

    filter(isMunchausen, enumFromTo(1, 5000))

    --> {1, 3435}

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

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
```

{{Out}}

```AppleScript
{1, 3435}
```



## AWK


```AWK

# syntax: GAWK -f MUNCHAUSEN_NUMBERS.AWK
BEGIN {
    for (i=1; i<=5000; i++) {
      sum = 0
      for (j=1; j<=length(i); j++) {
        digit = substr(i,j,1)
        sum += digit ^ digit
      }
      if (i == sum) {
        printf("%d\n",i)
      }
    }
    exit(0)
}

```

{{out}}

```txt

1
3435

```



## BASIC

This should need only minimal modification to work with any old-style BASIC that supports user-defined functions. The call to <code>INT</code> in line 10 is needed because the exponentiation operator may return a (floating-point) value that is slightly too large.

```basic
10 DEF FN P(X)=INT(X^X*SGN(X))
20 FOR I=0 TO 5
30 FOR J=0 TO 5
40 FOR K=0 TO 5
50 FOR L=0 TO 5
60 M=FN P(I)+FN P(J)+FN P(K)+FN P(L)
70 N=1000*I+100*J+10*K+L
80 IF M=N AND M>0 THEN PRINT M
90 NEXT L
100 NEXT K
110 NEXT J
120 NEXT I
```

{{out}}

```txt
 1
 3435
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM. The word <code>FAST</code> in line 10 shouldn't be taken <i>too</i> literally. We don't have <code>DEF FN</code>, so the expression for exponentiation-where-zero-to-the-power-zero-equals-zero is written out inline.

```basic
 10 FAST
 20 FOR I=0 TO 5
 30 FOR J=0 TO 5
 40 FOR K=0 TO 5
 50 FOR L=0 TO 5
 60 LET M=INT (I**I*SGN I+J**J*SGN J+K**K*SGN K+L**L*SGN L)
 70 LET N=1000*I+100*J+10*K+L
 80 IF M=N AND M>0 THEN PRINT M
 90 NEXT L
100 NEXT K
110 NEXT J
120 NEXT I
130 SLOW
```

{{out}}

```txt
1
3435
```



## BBC BASIC


```bbcbasic>REM
munchausen
FOR i% = 0 TO 5
  FOR j% = 0 TO 5
    FOR k% = 0 TO 5
      FOR l% = 0 TO 5
        m% = FNexp(i%) + FNexp(j%) + FNexp(k%) + FNexp(l%)
        n% = 1000 * i% + 100 * j% + 10 * k% + l%
        IF m% = n% AND m% > 0 THEN PRINT m%
      NEXT
    NEXT
  NEXT
NEXT
END
:
DEF FNexp(x%)
IF x% = 0 THEN
  = 0
ELSE
  = x% ^ x%
```

{{out}}

```txt
         1
      3435
```



## C

Adapted from Zack Denton's code posted on [https://zach.se/munchausen-numbers-and-how-to-find-them/ Munchausen Numbers and How to Find Them].

```c
#include <stdio.h>
#include <math.h>

int main() {
    for (int i = 1; i < 5000; i++) {
        // loop through each digit in i
        // e.g. for 1000 we get 0, 0, 0, 1.
        int sum = 0;
        for (int number = i; number > 0; number /= 10) {
            int digit = number % 10;
            // find the sum of the digits
            // raised to themselves
            sum += pow(digit, digit);
        }
        if (sum == i) {
            // the sum is equal to the number
            // itself; thus it is a
            // munchausen number
            printf("%i\n", i);
        }
    }
    return 0;
}
```

{{out}}

```txt
1
3435
```


## C#

```c#
Func<char, int> toInt = c => c-'0';

foreach (var i in Enumerable.Range(1,5000)
	.Where(n => n == n.ToString()
		.Sum(x => Math.Pow(toInt(x), toInt(x)))))
	Console.WriteLine(i);
```

{{out}}

```txt
1
3435
```



###  Faster version

{{Trans|Kotlin}}

```c#
using System;

namespace Munchhausen
{
    class Program
    {
        static readonly long[] cache = new long[10];

        static void Main()
        {
            // Allow for 0 ^ 0 to be 0
            for (int i = 1; i < 10; i++)
            {
                cache[i] = (long)Math.Pow(i, i);
            }

            for (long i = 0L; i <= 500_000_000L; i++)
            {
                if (IsMunchhausen(i))
                {
                    Console.WriteLine(i);
                }
            }
            Console.ReadLine();
        }

        private static bool IsMunchhausen(long n)
        {
            long sum = 0, nn = n;
            do
            {
                sum += cache[(int)(nn % 10)];
                if (sum > n)
                {
                    return false;
                }
                nn /= 10;
            } while (nn > 0);

            return sum == n;
        }
    }
}
```


```txt
0
1
3435
438579088
```


###  Faster version alternate

{{trans|Visual Basic .NET}}
Search covers all 11 digit numbers (as pointed out elsewhere, 11*(9^9) has only 10 digits, so there are no Munchausen numbers with 11+ digits), not just the first half of the 9 digit numbers.  Computation time is under 1.5 seconds.

```c#
using System;

static class Program
{
    public static void Main()
    {
        long sum, ten1 = 0, ten2 = 10; byte [] num; int [] pow = new int[10];
        int i, j, n, n1, n2, n3, n4, n5, n6, n7, n8, n9, s2, s3, s4, s5, s6, s7, s8;
        for (i = 1; i <= 9; i++) { pow[i] = i; for (j = 2; j <= i; j++) pow[i] *= i; }
        for (n = 1; n <= 11; n++) { for (n9 = 0; n9 <= n; n9++) { for (n8 = 0; n8 <= n - n9; n8++) {
              for (n7 = 0; n7 <= n - (s8 = n9 + n8); n7++) { for (n6 = 0; n6 <= n - (s7 = s8 + n7); n6++) {
                  for (n5 = 0; n5 <= n - (s6 = s7 + n6); n5++) { for (n4 = 0; n4 <= n - (s5 = s6 + n5); n4++) {
                      for (n3 = 0; n3 <= n - (s4 = s5 + n4); n3++) { for (n2 = 0; n2 <= n - (s3 = s4 + n3); n2++) {
                          for (n1 = 0; n1 <= n - (s2 = s3 + n2); n1++) {
                            sum = n1 * pow[1] + n2 * pow[2] + n3 * pow[3] + n4 * pow[4] +
                                  n5 * pow[5] + n6 * pow[6] + n7 * pow[7] + n8 * pow[8] + n9 * pow[9];
                            if (sum < ten1 || sum >= ten2) continue;
                            num = new byte[10]; foreach (char ch in sum.ToString()) num[Convert.ToByte(ch) - 48] += 1;
                            if (n - (s2 + n1) == num[0] && n1 == num[1] && n2 == num[2]
                              && n3 == num[3] && n4 == num[4] && n5 == num[5] && n6 == num[6]
                              && n7 == num[7] && n8 == num[8] && n9 == num[9]) Console.WriteLine(sum);
                          } } } } } } } } }
          ten1 = ten2; ten2 *= 10;
        }
    }
}
```

{{out}}

```txt
0
1
3435
438579088
```



## C++


```cpp

#include <math.h>
#include <iostream>

unsigned pwr[10];

unsigned munch( unsigned i ) {
    unsigned sum = 0;
    while( i ) {
        sum += pwr[(i % 10)];
        i /= 10;
    }
    return sum;
}

int main( int argc, char* argv[] ) {
    for( int i = 0; i < 10; i++ )
        pwr[i] = (unsigned)pow( (float)i, (float)i );
    std::cout << "Munchausen Numbers\n
### ============
\n";
    for( unsigned i = 1; i < 5000; i++ )
        if( i == munch( i ) ) std::cout << i << "\n";
    return 0;
}

```

{{out}}

```txt

Munchausen Numbers

### ============

1
3435

```



## Clojure


```lisp
(ns async-example.core
  (:require [clojure.math.numeric-tower :as math])
  (:use [criterium.core])
  (:gen-class))

(defn get-digits [n]
  " Convert number of a list of digits  (e.g. 545 -> ((5), (4), (5)) "
  (map #(Integer/valueOf (str %)) (String/valueOf n)))

(defn sum-power [digits]
  " Convert digits such as abc... to a^a + b^b + c^c ..."
  (let [digits-pwr (fn [n]
                     (apply + (map #(math/expt % %) digits)))]
    (digits-pwr digits)))

(defn find-numbers [max-range]
  " Filters for Munchausen numbers "
  (->>
    (range 1 (inc max-range))
    (filter #(= (sum-power (get-digits %)) %))))


(println (find-numbers 5000))

```

{{Output}}

```txt

(1 3435)

```




## Common Lisp


```lisp

;;; check4munch maximum &optional b
;;; Return a list with all Munchausen numbers less then or equal to maximum.
;;; Checks are done in base b (<=10, dpower is the limiting factor here).
(defun check4munch (maximum &optional (base 10))
  (do ((n 1 (1+ n))
       (result NIL (if (munchp n base) (cons n result) result)))
      ((> n maximum)
       (nreverse result))))

;;;
;;; munchp n &optional b
;;; Return T if n is a Munchausen number in base b.
(defun munchp (n &optional (base 10))
   (if (= n (apply #'+ (mapcar #'dpower (n2base n base)))) T NIL))

;;; dpower d
;;; Returns d^d. I.e. the digit to the power of itself.
;;; 0^0 is set to 0. For discussion see e.g. the wikipedia entry.
;;; This function is mainly performance optimization.
(defun dpower (d)
  (aref #(0 1 4 27 256 3125 45556 823543 16777216 387420489) d))

;;; divmod a b
;;; Return (q,k) such that a = b*q + k and k>=0.
(defun divmod (a b)
  (let ((foo (mod a b)))
    (list (/ (- a foo) b) foo)))

;;; n2base n &optional b
;;; Return a list with the digits of n in base b representation.
(defun n2base (n &optional (base 10) (digits NIL))
  (if (zerop n) digits
                (let ((dm (divmod n base)))
                  (n2base (car dm) base (cons (cadr dm) digits)))))

```


{{Out}}

```txt

> (check4munch 5000)
(1 3435)
> (munchp 438579088)
T

```



## D

{{trans|C}}

```D
import std.stdio;

void main() {
    for (int i=1; i<5000; i++) {
        // loop through each digit in i
        // e.g. for 1000 we get 0, 0, 0, 1.
        int sum = 0;
        for (int number=i; number>0; number/=10) {
            int digit = number % 10;
            // find the sum of the digits
            // raised to themselves
            sum += digit ^^ digit;
        }
        if (sum == i) {
            // the sum is equal to the number
            // itself; thus it is a
            // munchausen number
            writeln(i);
        }
    }
}
```

{{out}}

```txt
1
3435
```



## Dc

Needs a modern Dc due to <code>~</code>.
Use <code>S1S2l2l1/L2L1%</code> instead of <code>~</code> to run it in older Dcs.

```dc
[ O ~ S! d 0!=M L! d ^ + ] sM
[p] sp
[z d d lM x =p z 5001>L ] sL
lL x
```

Cosmetic: The stack is dirty after execution. The loop <code>L</code> needs a fix if that is a problem.


## Elixir


```elixir
defmodule Munchausen do
  @pow  for i <- 0..9, into: %{}, do: {i, :math.pow(i,i) |> round}

  def number?(n) do
    n == Integer.digits(n) |> Enum.reduce(0, fn d,acc -> @pow[d] + acc end)
  end
end

Enum.each(1..5000, fn i ->
  if Munchausen.number?(i), do: IO.puts i
end)
```


{{out}}

```txt

1
3435

```



## Factor


```factor

USING: kernel math.functions math.ranges math.text.utils
prettyprint sequences ;
IN: rosetta-code.munchausen

: munchausen? ( n -- ? )
    dup 1 digit-groups dup [ ^ ] 2map sum = ;

: main ( -- ) 5000 [1,b] [ munchausen? ] filter . ;

MAIN: main

```

{{out}}

```txt

V{ 1 3435 }

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Munchausen_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

{{works with|GNU Forth|0.7.0}}

```forth

 : dig.num                                       \ returns input number and the number of its digits ( n -- n n1 )
	 dup
	 0 swap
     begin
	 swap 1 + swap
	 dup 10 >= while
	 10 /
     repeat
	 drop ;

 : to.self                                        \ returns input number raised to the power of itself ( n -- n^n  )
	 dup 1 = if drop 1 else                   \ positive numbers only, zero and negative returns zero
	 dup 0 <= if drop 0 else
	 dup
         1 do
	 dup
	 loop
	 dup
	 1 do
	 *
         loop
	 then then ;

 : ten.to			                    \ ( n -- 10^n ) returns 1 for zero and negative
	 dup 0 <= if drop 1 else
	 dup 1 = if drop 10 else
	 10 swap
         1 do
	 10 *
         loop then then ;

 : zero.divmod                                       \ /mod that returns zero if number is zero
	  dup
	  0 = if drop 0
          else /mod
	  then ;

 : split.div                                         \ returns input number and its digits ( n -- n n1 n2 n3....)
	  dup 10 < if dup 0 else		     \ duplicates single digit numbers adds 0 for add.pow
	  dig.num			             \ provides number of digits
	  swap dup rot dup 1 - ten.to swap           \ stack juggling, ten raised to number of digits - 1...
          1 do                                       \ ... is the needed divisor, counter on top and ...
	  dup rot swap zero.divmod swap rot 10 /     \ ...division loop
          loop drop then ;

 : add.pow	  				     \ raises each number on the stack except last one to ...
	  to.self                                    \ ...the power of itself and adds them
	  depth					     \ needs at least 3 numbers on the stack
          2 do
	  swap to.self +
          loop ;

 : check.num
	 split.div add.pow ;

 : munch.num                                         \ ( n -- ) displays Munchausen numbers between 1 and n
         1 +
	 page
         1 do
         i check.num = if i . cr
         then loop ;

```

{{out}}

```txt

1
3435
 ok

```



## Fortran

{{trans|360 Assembly}}

### Fortran IV


```fortran
C MUNCHAUSEN NUMBERS - FORTRAN IV
      DO 2 I=1,5000
        IS=0
        II=I
        DO 1 J=1,4
          ID=10**(4-J)
          N=II/ID
          IR=MOD(II,ID)
          IF(N.NE.0) IS=IS+N**N
  1       II=IR
  2     IF(IS.EQ.I) WRITE(*,*) I
      END
```

{{out}}

```txt

           1
        3435

```


### Fortran 77


```fortran
! MUNCHAUSEN NUMBERS - FORTRAN 77
      DO I=1,5000
        IS=0
        II=I
        DO J=1,4
          ID=10**(4-J)
          N=II/ID
          IR=MOD(II,ID)
          IF(N.NE.0) IS=IS+N**N
          II=IR
        END DO
        IF(IS.EQ.I) WRITE(*,*) I
      END DO
      END
```

{{out}}

```txt

           1
        3435

```




## FreeBASIC


### Version 1


```freebasic
' FB 1.05.0 Win64
' Cache n ^ n for the digits 1 to 9
' Note than 0 ^ 0 specially treated as 0 (not 1) for this purpose
Dim Shared powers(1 To 9) As UInteger
For i As UInteger = 1 To 9
  Dim power As UInteger = i
  For j As UInteger = 2 To i
     power *= i
  Next j
  powers(i) = power
Next i

Function isMunchausen(n As UInteger) As Boolean
  Dim p As UInteger = n
  Dim As UInteger digit, sum
  While p > 0
    digit = p Mod 10
    If digit > 0 Then sum += powers(digit)
    p \= 10
  Wend
  Return n = sum
End Function

Print "The Munchausen numbers between 0 and 500000000 are : "
For i As UInteger = 0 To 500000000
  If isMunchausen(i) Then Print i
Next

Print
Print "Press any key to quit"

Sleep
```

{{out}}

```txt
The Munchausen numbers between 0 and 500000000 are :
0
1
3435
438579088
```


### Version 2


```freebasic
' version 12-10-2017
' compile with: fbc -s console

Dim As UInteger i, j, n, sum, ten1, ten2 = 10
Dim As UInteger n0, n1, n2, n3, n4, n5, n6, n7, n8, n9
Dim As UInteger     s1, s2, s3, s4, s5, s6, s7, s8
Dim As UInteger pow(9), num()
Dim As String number

For i = 1 To 9
  pow(i) = i
  For j = 2 To i
    pow(i) *= i
  Next
Next

For n = 1 To 11
  For n9 = 0 To n
    For n8 = 0 To n - n9
      s8 = n9 + n8
      For n7 = 0 To n - s8
        s7 = s8 + n7
        For n6 = 0 To n - s7
          s6 = s7 + n6
          For n5 = 0 To n - s6
            s5 = s6 + n5
            For n4 = 0 To n - s5
              s4 = s5 + n4
              For n3 = 0 To n - s4
                s3 = s4 + n3
                For n2 = 0 To n - s3
                  s2 = s3 + n2
                  For n1 = 0 To n - s2
                    n0 = n - (s2 + n1)
                    sum = n1 * pow(1) + n2 * pow(2) + n3 * pow(3) + _
                          n4 * pow(4) + n5 * pow(5) + n6 * pow(6) + _
                          n7 * pow(7) + n8 * pow(8) + n9 * pow(9)
                    If sum < ten1 Or sum >= ten2 Then Continue For
                    ReDim num(9) : number = Str(sum)
                    For i = 0 To n -1
                      j = number[i] -48
                      num(j) += 1
                    Next i
                    If n0 = num(0) AndAlso n1 = num(1) AndAlso n2 = num(2) AndAlso _
                       n3 = num(3) AndAlso n4 = num(4) AndAlso n5 = num(5) AndAlso _
                       n6 = num(6) AndAlso n7 = num(7) AndAlso n8 = num(8) AndAlso _
                       n9 = num(9) Then Print sum
                  Next n1
                Next n2
              Next n3
            Next n4
          Next n5
        Next n6
      Next n7
    Next n8
  Next n9
  ten1 = ten2
  ten2 *= 10
Next n

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
0
1
3435
438579088
```


=={{header|F sharp|F#}}==

```fsharp
let toFloat x = x |> int |> fun n -> n - 48 |> float
let power x = toFloat x ** toFloat x |> int
let isMunchausen n = n = (string n |> Seq.map char |> Seq.map power |> Seq.sum)

printfn "%A" ([1..5000] |> List.filter isMunchausen)
```

{{out}}

```txt
[1; 3435]
```



## Go

{{trans|Kotlin}}

```go
package main

import(
    "fmt"
    "math"
)

var powers [10]int

func isMunchausen(n int) bool {
    if n < 0 { return false }
    n64 := int64(n)
    nn  := n64
    var sum int64 = 0
    for nn > 0 {
        sum += int64(powers[nn % 10])
        if sum > n64 { return false }
        nn /= 10
    }
    return sum == n64
}

func main() {
    // cache n ^ n for n in 0..9, defining 0 ^ 0 = 0 for this purpose
    for i := 1; i <= 9; i++ {
        d := float64(i)
        powers[i] = int(math.Pow(d, d))
    }

    // check numbers 0 to 500 million
    fmt.Println("The Munchausen numbers between 0 and 500 million are:")
    for i := 0; i <= 500000000; i++ {
        if isMunchausen(i) { fmt.Printf("%d ", i) }
    }
    fmt.Println()
}
```


{{out}}

```txt

0 1 3435 438579088

```



## Haskell


```haskell
import Data.List (unfoldr)

isMunchausen :: Integer -> Bool
isMunchausen n = (n ==) $ sum $ map (\x -> x^x) $ unfoldr digit n where
  digit 0 = Nothing
  digit n = Just (r,q) where (q,r) = n `divMod` 10

main :: IO ()
main = print $ filter isMunchausen [1..5000]
```

{{out}}

```txt
[1,3435]
```


The Haskell libraries provide a lot of flexibility – we could also reduce the sum and map (above) down to a single foldr:


```haskell
import Data.Char (digitToInt)

isMunchausen :: Int -> Bool
isMunchausen = (==) <*> foldr ((+) . (id >>=) (^) . digitToInt) 0 . show

main :: IO ()
main = print $ filter isMunchausen [1 .. 5000]
```

{{Out}}

```txt
[1,3435]
```



## J


Here, it would be useful to have a function which sums the powers of the digits of a number. Once we have that we can use it with an equality test to filter those integers:


```J
   munch=: +/@(^~@(10&#.inv))
   (#~ ] = munch"0) 1+i.5000
1 3435
```


Note that [[wp:Munchausen_number|wikipedia]] claims that 0=0^0 in the context of Munchausen numbers. It's not clear why this should be (1 is the multiplicative identity and if you do not multiply it by zero it should still be 1), but it's easy enough to implement. Note also that this does not change the result for this task:


```J
   munch=: +/@((**^~)@(10&#.inv))
   (#~ ] = munch"0) 1+i.5000
1 3435
```



## Java

Adapted from Zack Denton's code posted on [https://zach.se/munchausen-numbers-and-how-to-find-them/ Munchausen Numbers and How to Find Them].

```Java

public class Main {
    public static void main(String[] args) {
        for(int i = 0 ; i <= 5000 ; i++ ){
            int val = String.valueOf(i).chars().map(x -> (int) Math.pow( x-48 ,x-48)).sum();
            if( i == val){
                System.out.println( i + " (munchausen)");
            }
        }
    }
}


```

{{out}}

```txt
1 (munchausen)
3435 (munchausen)
```



###  Faster version

{{trans|Kotlin}}

```java
public class Munchhausen {

    static final long[] cache = new long[10];

    public static void main(String[] args) {
        // Allowing 0 ^ 0 to be 0
        for (int i = 1; i < 10; i++) {
            cache[i] = (long) Math.pow(i, i);
        }
        for (long i = 0L; i <= 500_000_000L; i++) {
            if (isMunchhausen(i)) {
                System.out.println(i);
            }
        }
    }

    private static boolean isMunchhausen(long n) {
        long sum = 0, nn = n;
        do {
            sum += cache[(int)(nn % 10)];
            if (sum > n) {
                return false;
            }
            nn /= 10;
        } while (nn > 0);

        return sum == n;
    }
}
```


```txt
0
1
3435
438579088
```



## JavaScript



### ES6



```javascript
for (let i of [...Array(5000).keys()]
	.filter(n => n == n.toString().split('')
	.reduce((a, b) => a+Math.pow(parseInt(b),parseInt(b)), 0)))
    console.log(i);
```

{{out}}

```txt
1
3435
```



Or, composing reusable primitives:


```JavaScript
(() => {
    'use strict';

    const main = () =>
        filter(isMunchausen, enumFromTo(1, 5000));

    // isMunchausen :: Int -> Bool
    const isMunchausen = n =>
        n.toString()
        .split('')
        .reduce(
            (a, c) => (
                d => a + Math.pow(d, d)
            )(parseInt(c, 10)),
            0
        ) === n;

    // GENERIC ---------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);


    // MAIN ---
    return main();
})();
```

{{Out}}

```JavaScript
[1, 3435]
```



## jq

{{works with|jq|1.5}}

```jq
def sigma( stream ): reduce stream as $x (0; . + $x ) ;

def ismunchausen:
   def digits: tostring | split("")[] | tonumber;
   . == sigma(digits | pow(.;.));

# Munchausen numbers from 1 to 5000 inclusive:
range(1;5001) | select(ismunchausen)
```

{{out}}

```jq
1
3435
```



## Julia

{{works with|Julia|1.0}}

```julia
println([n for n = 1:5000 if sum(d^d for d in digits(n)) == n])
```


{{out}}

```txt
[1, 3435]
```



## Kotlin

As it doesn't take long to find all 4 known Munchausen numbers, we will test numbers up to 500 million here rather than just 5000:

```scala
// version 1.0.6

val powers = IntArray(10)

fun isMunchausen(n: Int): Boolean {
    if (n < 0) return false
    var sum = 0L
    var nn = n
    while (nn > 0) {
        sum += powers[nn % 10]
        if (sum > n.toLong()) return false
        nn /= 10
    }
    return sum == n.toLong()
}

fun main(args: Array<String>) {
   // cache n ^ n for n in 0..9, defining 0 ^ 0 = 0 for this purpose
   for (i in 1..9) powers[i] = Math.pow(i.toDouble(), i.toDouble()).toInt()

   // check numbers 0 to 500 million
   println("The Munchausen numbers between 0 and 500 million are:")
   for (i in 0..500000000) if (isMunchausen(i))print ("$i ")
   println()
}
```


{{out}}

```txt

The Munchausen numbers between 0 and 500 million are:
0 1 3435 438579088

```



## Langur

{{trans|C#}}

```Langur
# sum power of digits
val .spod = f(.n) fold f .x + .y, map(f (.x-'0') ^ (.x-'0'), s2cp toString .n)

# Munchausen
writeln "Answers: ", where f(.n) .n == .spod(.n), series 0..5000
```


{{out}}

```txt
Answers: [1, 3435]
```



## Lua


```Lua
function isMunchausen (n)
    local sum, nStr, digit = 0, tostring(n)
    for pos = 1, #nStr do
        digit = tonumber(nStr:sub(pos, pos))
        sum = sum + digit ^ digit
    end
    return sum == n
end

for i = 1, 5000 do
    if isMunchausen(i) then print(i) end
end
```

{{out}}

```txt
1
3435
```



## Nim


```nim
import math

for i in 1..<5000:
  var sum: int64 = 0
  var number = i
  while number > 0:
    var digit = number mod 10
    sum += digit ^ digit
    number = number div 10
  if sum == i:
    echo i
```

{{out}}

```txt
1
3435
```



## M2000 Interpreter


```M2000 Interpreter

Module Munchausen {
      Inventory p=0:=0,1:=1
      for i=2 to 9 {Append p, i:=i**i}
      Munchausen=lambda p (x)-> {
            m=0
            t=x
            do {
                  m+=p(x mod 10)
                  x=x div 10
            } until x=0
            =m=t
      }
      For i=1 to 5000
            If Munchausen(i) then print i,
      Next i
      Print
}
Munchausen

```

Using Array instead of Inventory

```M2000 Interpreter

Module Münchhausen {
      Dim p(0 to 9)
      p(0)=0, 1
      for i=2 to 9 {p(i)=i**i}
      Münchhausen=lambda p() (x)-> {
            m=0
            t=x
            do {
                  m+=p(x mod 10)
                  x=x div 10
            } until x=0
            =m=t
      }
      For i=1 to 5000
            If Münchhausen(i) then print i,
      Next i
      Print
}
Münchhausen

```

{{out}}

```txt

       1     3435

```



## Maple


```Maple
isMunchausen := proc(n::posint)
local num_digits;
num_digits := map(x -> StringTools:-Ord(x) - 48, StringTools:-Explode(convert(n, string)));
return evalb(n = convert(map(x -> x^x, num_digits), `+`));
end proc;

Munchausen_upto := proc(n::posint) local k, count, list_num;
list_num := [];
for k to n do
    if isMunchausen(k) then
       list_num := [op(list_num), k];
    end if;
end do;
return list_num;
end proc;

Munchausen_upto(5000);
```

{{out}}

```txt
[1, 3435]
```



## Mathematica


```Mathematica
Off[Power::indet];(*Supress 0^0 warnings*)
Select[Range[5000], Total[IntegerDigits[#]^IntegerDigits[#]] == # &]
```

{{out}}

```txt
{1,3435}
```



## min

{{works with|min|0.19.3}}

```min
(dup string "" split (int dup pow) (+) map-reduce ==) :munchausen?
1 :i
(i 5000 <=) ((i munchausen?) (i puts!) when i succ @i) while
```

{{out}}

```txt

1
3435

```


=={{header|Modula-2}}==

```modula2
MODULE MunchausenNumbers;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

(* Simple power function, does not handle negatives *)
PROCEDURE Pow(b,e : INTEGER) : INTEGER;
VAR result : INTEGER;
BEGIN
    IF e=0 THEN
        RETURN 1;
    END;
    IF b=0 THEN
        RETURN 0;
    END;

    result := b;
    DEC(e);
    WHILE e>0 DO
        result := result * b;
        DEC(e);
    END;
    RETURN result;
END Pow;

VAR
    buf : ARRAY[0..31] OF CHAR;
    i,sum,number,digit : INTEGER;
BEGIN
    FOR i:=1 TO 5000 DO
        (* Loop through each digit in i
           e.g. for 1000 we get 0, 0, 0, 1. *)
        sum := 0;
        number := i;
        WHILE number>0 DO
            digit := number MOD 10;
            sum := sum + Pow(digit, digit);
            number := number DIV 10;
        END;
        IF sum=i THEN
            FormatString("%i\n", buf, i);
            WriteString(buf);
        END;
    END;

    ReadChar;
END MunchausenNumbers.
```



## Pascal

{{works with|Free Pascal}}
{{works with|Delphi}}
tried to speed things up.Only checking one arrangement of 123456789 instead of all 9! = 362880 permutations.This ist possible, because summing up is commutative.
So I only have to create [http://rosettacode.org/wiki/Combinations_with_repetitions Combinations_with_repetitions] and need to check, that the number and the sum of power of digits have the same amount in every possible digit. This means, that a combination of the digits of number leads to the sum of power of digits. Therefore I need leading zero's.

```pascal
{$IFDEF FPC}{$MODE objFPC}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}
uses
  sysutils;
type
  tdigit  = byte;
const
  base = 10;
  maxDigits = base-1;// set for 32-compilation otherwise overflow.

var
  DgtPotDgt : array[0..base-1] of NativeUint;
  cnt: NativeUint;

function CheckSameDigits(n1,n2:NativeUInt):boolean;
var
  dgtCnt : array[0..Base-1] of NativeInt;
  i : NativeUInt;
Begin
  fillchar(dgtCnt,SizeOf(dgtCnt),#0);
  repeat
    //increment digit of n1
    i := n1;n1 := n1 div base;i := i-n1*base;inc(dgtCnt[i]);
    //decrement digit of n2
    i := n2;n2 := n2 div base;i := i-n2*base;dec(dgtCnt[i]);
  until (n1=0) AND (n2= 0 );
  result := true;
  For i := 0 to Base-1 do
    result := result AND (dgtCnt[i]=0);
end;

procedure Munch(number,DgtPowSum,minDigit:NativeUInt;digits:NativeInt);
var
  i: NativeUint;
begin
  inc(cnt);
  number := number*base;
  IF digits > 1 then
  Begin
    For i := minDigit to base-1 do
      Munch(number+i,DgtPowSum+DgtPotDgt[i],i,digits-1);
  end
  else
    For i := minDigit to base-1 do
      //number is always the arrangement of the digits leading to smallest number
      IF (number+i)<= (DgtPowSum+DgtPotDgt[i]) then
        IF CheckSameDigits(number+i,DgtPowSum+DgtPotDgt[i]) then
          iF number+i>0 then
            writeln(Format('%*d  %.*d',
             [maxDigits,DgtPowSum+DgtPotDgt[i],maxDigits,number+i]));
end;

procedure InitDgtPotDgt;
var
  i,k,dgtpow: NativeUint;
Begin
  // digit ^ digit ,special case 0^0 here 0
  DgtPotDgt[0]:= 0;
  For i := 1 to Base-1 do
  Begin
    dgtpow := i;
    For k := 2 to i do
      dgtpow := dgtpow*i;
    DgtPotDgt[i] := dgtpow;
  end;
end;

begin
  cnt := 0;
  InitDgtPotDgt;
  Munch(0,0,0,maxDigits);
  writeln('Check Count ',cnt);
end.

```

{{Out}}

```txt
         1  000000001
      3435  000003345
 438579088  034578889

Check Count 43758 ==
n= maxdigits = 9,k = 10;CombWithRep = (10+9-1))!/(10!*(9-1)!)=43758

real    0m0.002s

```



## Perl


```perl
use List::Util "sum";
for my $n (1..5000) {
  print "$n\n" if $n == sum( map { $_**$_ } split(//,$n) );
}
```

{{out}}

```txt
1
3435
```



## Perl 6


```perl6
sub is_munchausen ( Int $n ) {
    constant @powers = 0, |map { $_ ** $_ }, 1..9;
    $n == @powers[$n.comb].sum;
}
.say if .&is_munchausen for 1..5000;
```

{{out}}

```txt
1
3435
```



## Phix


```Phix
sequence powers = 0&sq_power(tagset(9),tagset(9))

function munchausen(integer n)
    integer n0 = n
    atom summ = 0
    while n!=0 do
        summ += powers[remainder(n,10)+1]
        n = floor(n/10)
    end while
    return summ=n0
end function

for i=1 to 5000 do
    if munchausen(i) then ?i end if
end for
```

{{out}}

```txt

1
3435

```



## PicoLisp


```PicoLisp
(for N 5000
   (and
      (=
         N
         (sum
            '((N) (** N N))
            (mapcar format (chop N)) ) )
      (println N) ) )
```

{{out}}

```txt

1
3435
```



## PowerBASIC

{{trans|FreeBASIC}}(Translated from the FreeBasic Version 2 example.)

```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6

DECLARE FUNCTION GetTickCount LIB "kernel32.dll" ALIAS "GetTickCount"() AS DWORD

FUNCTION PBMAIN () AS LONG
LOCAL i, j, n, sum, ten1, ten2, t AS DWORD
LOCAL n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 AS DWORD
LOCAL s1, s2, s3, s4, s5, s6, s7, s8 AS DWORD
DIM pow(9) AS DWORD, num(9) AS DWORD
LOCAL pb AS BYTE PTR
LOCAL number AS STRING

  t = GetTickCount()
  ten2 = 10
  FOR i = 1 TO 9
    pow(i) = i
    FOR j = 2 TO i
      pow(i) *= i
    NEXT j
  NEXT i
  FOR n = 1 TO 11
    FOR n9 = 0 TO n
      FOR n8 = 0 TO n - n9
        s8 = n9 + n8
        FOR n7 = 0 TO n - s8
          s7 = s8 + n7
          FOR n6 = 0 TO n - s7
            s6 = s7 + n6
            FOR n5 = 0 TO n - s6
              s5 = s6 + n5
              FOR n4 = 0 TO n - s5
                s4 = s5 + n4
                FOR n3 = 0 TO n - s4
                  s3 = s4 + n3
                  FOR n2 = 0 TO n - s3
                    s2 = s3 + n2
                    FOR n1 = 0 TO n - s2
                      n0 = n - (s2 + n1)
                      sum = n1 * pow(1) + n2 * pow(2) + n3 * pow(3) + _
                            n4 * pow(4) + n5 * pow(5) + n6 * pow(6) + _
                            n7 * pow(7) + n8 * pow(8) + n9 * pow(9)
                      SELECT CASE AS LONG sum
                      CASE ten1 TO ten2 - 1
                        number = LTRIM$(STR$(sum))
                        pb = STRPTR(number)
                        MAT num() = ZER
                        FOR i = 0 TO n -1
                          j = @pb[i] - 48
                          INCR num(j)
                        NEXT i
                        IF n0 = num(0) AND n1 = num(1) AND n2 = num(2) AND _
                           n3 = num(3) AND n4 = num(4) AND n5 = num(5) AND _
                           n6 = num(6) AND n7 = num(7) AND n8 = num(8) AND _
                           n9 = num(9) THEN CON.PRINT STR$(sum)
                      END SELECT
                    NEXT n1
                  NEXT n2
                NEXT n3
              NEXT n4
            NEXT n5
          NEXT n6
        NEXT n7
      NEXT n8
    NEXT n9
    ten1 = ten2
    ten2 *= 10
  NEXT n
  t = GetTickCount() - t
  CON.PRINT "execution time:" & STR$(t) & " ms; hit any key to end program"
  CON.WAITKEY$
END FUNCTION
```

{{out}}

```txt
 0
 1
 3435
 438579088
execution time: 78 ms; hit any key to end program
```



## Pure


```Pure
// split numer into digits
digits n::number = loop n [] with
                     loop n l = loop (n div 10) ((n mod 10):l) if n > 0;
                              = l otherwise; end;

munchausen n::int = (filter isMunchausen list) when
                      list = 1..n; end with
                      isMunchausen n = n == foldl (+) 0
                                       (map (\d -> d^d)
                                        (digits n)); end;
munchausen 5000;
```

{{out}}

```txt
[1,3435]
```



## PureBasic

{{trans|C}}

```PureBasic
EnableExplicit
Declare main()

If OpenConsole("Munchausen_numbers")
  main() : Input() : End
EndIf

Procedure main()
  Define i.i,
         sum.i,
         number.i,
         digit.i
  For i = 1 To 5000
    sum = 0
    number = i
    While number > 0
      digit = number % 10
      sum + Pow(digit, digit)
      number / 10
    Wend
    If sum = i
      PrintN(Str(i))
    EndIf
  Next
EndProcedure
```

{{out}}

```txt
1
3435
```



## Python


```python
for i in range(5000):
    if i == sum(int(x) ** int(x) for x in str(i)):
        print(i)
```

{{out}}

```txt
1
3435
```



Or, defining an '''isMunchausen''' predicate in terms of a single fold – rather than a two-pass ''sum'' after ''map'' (or comprehension) –

and reaching for a specialised '''digitToInt''', which turns out to be a little faster than type coercion with the more general built-in '''int()''':

{{Works with|Python|3}}

```python
'''Munchausen numbers'''

from functools import (reduce)


# isMunchausen :: Int -> Bool
def isMunchausen(n):
    '''True if n equals the sum of
       each of its digits raised to
       the power of itself.'''
    def powerOfSelf(d):
        i = digitToInt(d)
        return i**i
    return n == reduce(
        lambda n, c: n + powerOfSelf(c),
        str(n), 0
    )


# main :: IO ()
def main():
    '''Test'''
    print(list(filter(
        isMunchausen,
        enumFromTo(1)(5000)
    )))


# GENERIC -------------------------------------------------

# digitToInt :: Char -> Int
def digitToInt(c):
    '''The integer value of any digit character
       drawn from the 0-9, A-F or a-f ranges.'''
    oc = ord(c)
    if 48 > oc or 102 < oc:
        return None
    else:
        dec = oc - 48   # ord('0')
        hexu = oc - 65  # ord('A')
        hexl = oc - 97  # ord('a')
    return dec if 9 >= dec else (
        10 + hexu if 0 <= hexu <= 5 else (
            10 + hexl if 0 <= hexl <= 5 else None
        )
    )


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


if __name__ == '__main__':
    main()
```


```txt
[1, 3435]
```



## Racket


<lang>#lang racket

(define (expt:0^0=1 r p)
  (if (zero? r) 0 (expt r p)))

(define (munchausen-number? n (t n))
  (if (zero? n)
      (zero? t)
      (let-values (([q r] (quotient/remainder n 10)))
        (munchausen-number? q (- t (expt:0^0=1 r r))))))

(module+ main
  (for-each displayln (filter munchausen-number? (range 1 (add1 5000)))))

(module+ test
  (require rackunit)
  ;; this is why we have the (if (zero? r)...) test
  (check-equal? (expt 0 0) 1)
  (check-equal? (expt:0^0=1 0 0) 0)
  (check-equal? (expt:0^0=1 0 4) 0)
  (check-equal? (expt:0^0=1 3 4) (expt 3 4))
  ;; given examples
  (check-true (munchausen-number? 1))
  (check-true (munchausen-number? 3435))
  (check-false (munchausen-number? 3))
  (check-false (munchausen-number? -45) "no recursion on -ve numbers"))
```


{{out}}

```txt
1
3435
```



## REXX


### version 1


```rexx
Do n=0 To 10000
  If n=m(n) Then
    Say n
  End
Exit
m: Parse Arg z
res=0
Do While z>''
  Parse Var z c +1 z
  res=res+c**c
  End
Return res
```

{{out}}

```txt
D:\mau>rexx munch
1
3435

```



### version 2

This REXX version uses the requirement that   '''0**0'''   equals zero.

It is about 2.5 times faster than version 1.

For the high limit of   '''5,000''',   optimization isn't needed.   But for much higher limits, optimization becomes significant.

```rexx
/*REXX program finds and displays Münchhausen numbers from one to a specified number (Z)*/
@.=0;          do i=1  for 9;  @.i=i**i;  end    /*precompute powers for non-zero digits*/
parse arg z .                                    /*obtain optional argument from the CL.*/
if z=='' | z==","  then z=5000                   /*Not specified?  Then use the default.*/
@is='is a Münchhausen number.';   do j=1  for z  /* [↓]  traipse through all the numbers*/
                                  if isMunch(j)  then say  right(j, 11)    @is
                                  end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isMunch: parse arg x 1 ox;  $=0;  do  until  x==''  |  $>ox         /*stop if too large.*/
                                  parse var x _ +1 x;  $=$ + @._    /*add the next power*/
                                  end   /*while*/                   /* [↑]  get a digit.*/
         return $==ox                                               /*it is or it ain't.*/
```

'''output'''

```txt

          1 is a Münchhausen number.
       3435 is a Münchhausen number.

```



### version 3

It is about 3 times faster than version 1.

```rexx
/*REXX program finds and displays Münchhausen numbers from one to a specified number (Z)*/
@.=0;          do i=1  for 9;  @.i=i**i;  end    /*precompute powers for non-zero digits*/
parse arg z .                                    /*obtain optional argument from the CL.*/
if z=='' | z==","  then z=5000                   /*Not specified?  Then use the default.*/
@is='is a Münchhausen number.';   do j=1  for z  /* [↓]  traipse through all the numbers*/
                                  if isMunch(j)  then say  right(j, 11)    @is
                                  end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isMunch: parse arg a 2 b 3 c 4 d 5 e 6 x 1 ox; $=@.a+@.b+@.c+@.d+@.e /*sum 1st 5 digits.*/
         if $>ox  then return 0                                      /*is sum too large?*/
                                  do  while  x\==''  &  $<=ox        /*any more digits ?*/
                                  parse var x _ +1 x;   $=$ + @._    /*sum 6th & up digs*/
                                  end   /*while*/
         return $==ox                                                /*it is or it ain't*/
```

'''output'''   is the same as the 2<sup>nd</sup> REXX version.




## Ring


```ring

# Project : Munchausen numbers

limit = 5000

for n=1 to limit
    sum = 0
    msum = string(n)
    for m=1 to len(msum)
        ms = number(msum[m])
        sum = sum + pow(ms, ms)
    next
    if sum = n
       see n + nl
    ok
next

```

Output:

```txt

1
3435

```



## Ruby


```ruby
class Integer

  def munchausen?
    self.digits.map{|d| d**d}.sum == self
  end

end

puts (1..5000).select(&:munchausen?)
```

{{out}}

```txt

1
3435

```



## Rust


```rust
fn main() {
    let mut solutions = Vec::new();

    for num in 1..5_000 {
        let power_sum = num.to_string()
            .chars()
            .map(|c| {
                let digit = c.to_digit(10).unwrap();
                (digit as f64).powi(digit as i32) as usize
            })
            .sum::<usize>();

        if power_sum == num {
            solutions.push(num);
        }
    }

    println!("Munchausen numbers below 5_000 : {:?}", solutions);
}
```

{{out}}

```txt

Munchausen numbers below 5_000 : [1, 3435]

```



## Scala

Adapted from Zack Denton's code posted on [https://zach.se/munchausen-numbers-and-how-to-find-them/ Munchausen Numbers and How to Find Them].

```Scala

object Munch {
  def main(args: Array[String]): Unit = {
    import scala.math.pow
    (1 to 5000).foreach {
      i => if (i == (i.toString.toCharArray.map(d => pow(d.asDigit,d.asDigit))).sum)
        println( i + " (munchausen)")
    }
  }
}

```

{{out}}

```txt
1 (munchausen)
3435 (munchausen)
```



## Sidef


```ruby
func is_munchausen(n) {
    n.digits.map{|d| d**d }.sum == n
}

say (1..5000 -> grep(is_munchausen))
```

{{out}}

```txt

[1, 3435]

```



## SuperCollider


```supercollider
(1..5000).select { |n| n == n.asDigits.sum { |x| pow(x, x) } }
```



```txt

[1, 3435]

```



## Swift


```swift
import Foundation

func isMünchhausen(_ n: Int) -> Bool {
  let nums = String(n).map(String.init).compactMap(Int.init)

  return Int(nums.map({ pow(Double($0), Double($0)) }).reduce(0, +)) == n
}

for i in 1...5000 where isMünchhausen(i) {
  print(i)
}
```

{{out}}

```txt
1
3435
```


=={{header|TI-83 BASIC}}==
{{works with|TI-83 BASIC|TI-84Plus 2.55MP}}
{{trans|Fortran}}

```ti83b
  For(I,1,5000)
    0→S:I→K
    For(J,1,4)
      10^(4-J)→D
      iPart(K/D)→N
      remainder(K,D)→R
      If N≠0:S+N^N→S
      R→K
    End
    If S=I:Disp I
  End
```

{{out}}

```txt

           1
        3435

```

Execution time: 15 min


## VBA



```vb

Option Explicit

Sub Main_Munchausen_numbers()
Dim i&

    For i = 1 To 5000
        If IsMunchausen(i) Then Debug.Print i & " is a munchausen number."
    Next i
End Sub

Function IsMunchausen(Number As Long) As Boolean
Dim Digits, i As Byte, Tot As Long

    Digits = Split(StrConv(Number, vbUnicode), Chr(0))
    For i = 0 To UBound(Digits) - 1
        Tot = (Digits(i) ^ Digits(i)) + Tot
    Next i
    IsMunchausen = (Tot = Number)
End Function

```

{{out}}

```txt
1 is a munchausen number.
3435 is a munchausen number.
```



## VBScript


```vbscript

for i = 1 to 5000
    if Munch(i) Then
        Wscript.Echo i, "is a Munchausen number"
    end if
next

'Returns True if num is a Munchausen number. This is true if the sum of
'each digit raised to that digit's power is equal to the given number.
'Example: 3435 = 3^3 + 4^4 + 3^3 + 5^5

Function Munch (num)

    dim str: str = Cstr(num)    'input num as a string
    dim sum: sum = 0            'running sum of n^n
    dim i                       'loop index
    dim n                       'extracted digit

    for i = 1 to len(str)
        n = CInt(Mid(str,i,1))
        sum = sum + n^n
    next

    Munch = (sum = num)

End Function

```

{{out}}

```txt

1 is a Munchausen number
3435 is a Munchausen number

```



## Visual Basic

{{trans|FreeBASIC}}(Translated from the FreeBasic Version 2 example.)

```vb
Option Explicit

Declare Function GetTickCount Lib "kernel32.dll" () As Long
Declare Sub ZeroMemory Lib "kernel32.dll" Alias "RtlZeroMemory" (ByRef Destination As Any, ByVal Length As Long)


Sub Main()
Dim i As Long, j As Long, n As Long, t As Long
Dim sum As Double
Dim n0 As Double
Dim n1 As Double
Dim n2 As Double
Dim n3 As Double
Dim n4 As Double
Dim n5 As Double
Dim n6 As Double
Dim n7 As Double
Dim n8 As Double
Dim n9 As Double
Dim ten1 As Double
Dim ten2 As Double
Dim s1 As Long
Dim s2 As Long
Dim s3 As Long
Dim s4 As Long
Dim s5 As Long
Dim s6 As Long
Dim s7 As Long
Dim s8 As Long
Dim pow(9) As Long, num(9) As Long
Dim number As String, res As String

  t = GetTickCount()
  ten2 = 10
  For i = 1 To 9
    pow(i) = i
    For j = 2 To i
      pow(i) = i * pow(i)
    Next j
  Next i
  For n = 1 To 11
    For n9 = 0 To n
      For n8 = 0 To n - n9
        s8 = n9 + n8
        For n7 = 0 To n - s8
          s7 = s8 + n7
          For n6 = 0 To n - s7
            s6 = s7 + n6
            For n5 = 0 To n - s6
              s5 = s6 + n5
              For n4 = 0 To n - s5
                s4 = s5 + n4
                For n3 = 0 To n - s4
                  s3 = s4 + n3
                  For n2 = 0 To n - s3
                    s2 = s3 + n2
                    For n1 = 0 To n - s2
                      n0 = n - (s2 + n1)
                      sum = n1 * pow(1) + n2 * pow(2) + n3 * pow(3) + _
                            n4 * pow(4) + n5 * pow(5) + n6 * pow(6) + _
                            n7 * pow(7) + n8 * pow(8) + n9 * pow(9)
                      Select Case sum
                      Case ten1 To ten2 - 1
                        number = CStr(sum)
                        ZeroMemory num(0), 40
                        For i = 1 To n
                          j = Asc(Mid$(number, i, 1)) - 48
                          num(j) = num(j) + 1
                        Next i
                        If n0 = num(0) Then
                          If n1 = num(1) Then
                            If n2 = num(2) Then
                              If n3 = num(3) Then
                                If n4 = num(4) Then
                                  If n5 = num(5) Then
                                    If n6 = num(6) Then
                                      If n7 = num(7) Then
                                        If n8 = num(8) Then
                                          If n9 = num(9) Then
                                            res = res & CStr(sum) & vbNewLine
                                          End If
                                        End If
                                      End If
                                    End If
                                  End If
                                End If
                              End If
                            End If
                          End If
                        End If
                      End Select
                    Next n1
                  Next n2
                Next n3
              Next n4
            Next n5
          Next n6
        Next n7
      Next n8
    Next n9
    ten1 = ten2
    ten2 = ten2 * 10
  Next n
  t = GetTickCount() - t
  res = res & "execution time:" & Str$(t) & " ms"
  MsgBox res
End Sub
```

{{out}}

```txt
 0
 1
 3435
 438579088
execution time: 156 ms
```



## Visual Basic .NET

{{trans|FreeBASIC}}(Translated from the FreeBasic Version 2 example.)<br/>
Computation time is under 4 seconds on tio.run.

```vbnet
Imports System

Module Program
    Sub Main()
        Dim i, j, n, n1, n2, n3, n4, n5, n6, n7, n8, n9, s2, s3, s4, s5, s6, s7, s8 As Integer,
            sum, ten1 As Long, ten2 As Long = 10
        Dim pow(9) As Long, num() As Byte
        For i = 1 To 9 : pow(i) = i : For j = 2 To i : pow(i) *= i : Next : Next
        For n = 1 To 11 : For n9 = 0 To n : For n8 = 0 To n - n9 : s8 = n9 + n8 : For n7 = 0 To n - s8
                s7 = s8 + n7 : For n6 = 0 To n - s7 : s6 = s7 + n6 : For n5 = 0 To n - s6
                    s5 = s6 + n5 : For n4 = 0 To n - s5 : s4 = s5 + n4 : For n3 = 0 To n - s4
                        s3 = s4 + n3 : For n2 = 0 To n - s3 : s2 = s3 + n2 : For n1 = 0 To n - s2
                            sum = n1 * pow(1) + n2 * pow(2) + n3 * pow(3) + n4 * pow(4) +
                                  n5 * pow(5) + n6 * pow(6) + n7 * pow(7) + n8 * pow(8) + n9 * pow(9)
                            If sum < ten1 OrElse sum >= ten2 Then Continue For
                            redim num(9)
                            For Each ch As Char In sum.ToString() : num(Convert.ToByte(ch) - 48) += 1 : Next
                            If n - (s2 + n1) = num(0) AndAlso n1 = num(1) AndAlso n2 = num(2) AndAlso
                                n3 = num(3) AndAlso n4 = num(4) AndAlso n5 = num(5) AndAlso n6 = num(6) AndAlso
                                n7 = num(7) AndAlso n8 = num(8) AndAlso n9 = num(9) Then Console.WriteLine(sum)
                          Next : Next : Next : Next : Next : Next : Next : Next : Next
            ten1 = ten2 : ten2 *= 10
       Next
    End Sub
End Module
```

{{out}}

```txt
0
1
3435
438579088
```


## zkl


```zkl
[1..5000].filter(fcn(n){ n==n.split().reduce(fcn(s,n){ s + n.pow(n) },0) })
.println();
```

{{out}}

```txt

L(1,3435)

```


+++
title = "Amicable pairs"
description = ""
date = 2019-10-02T12:51:41Z
aliases = []
[extra]
id = 18389
[taxonomies]
categories = []
tags = []
+++

{{task}}

Two integers <math>N</math> and <math>M</math> are said to be [[wp:Amicable numbers|amicable pairs]] if <math>N \neq M</math> and the sum of the [[Proper divisors|proper divisors]] of <math>N</math> (<math>\mathrm{sum}(\mathrm{propDivs}(N))</math>) <math>= M</math> as well as <math>\mathrm{sum}(\mathrm{propDivs}(M)) = N</math>.


;Example:
'''1184''' and '''1210''' are an amicable pair, with proper divisors:
*   1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592   and
*   1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605    respectively.


;Task:
Calculate and show here the Amicable pairs below 20,000; (there are eight).


;Related tasks
* [[Proper divisors]]
* [[Abundant, deficient and perfect number classifications]]
* [[Aliquot sequence classifications]] and its amicable ''classification''.





## 11l


```11l
F sum_proper_divisors(n)
   R I n < 2 {0} E sum((1 .. n I/ 2).filter(it -> (@n % it) == 0))

L(n) 1..20000
   V m = sum_proper_divisors(n)
   I m > n & sum_proper_divisors(m) == n
      print(n"\t"m)
```



## Ada


This solution uses the package ''Generic_Divisors'' from the Proper Divisors task
[[http://rosettacode.org/wiki/Proper_divisors#Ada]].


```Ada
with Ada.Text_IO, Generic_Divisors; use Ada.Text_IO;

procedure Amicable_Pairs is

   function Same(P: Positive) return Positive is (P);

   package Divisor_Sum is new Generic_Divisors
     (Result_Type => Natural, None => 0, One => Same, Add =>  "+");

   Num2 : Integer;
begin
   for Num1 in 4 .. 20_000 loop
      Num2 := Divisor_Sum.Process(Num1);
      if Num1 < Num2 then
	 if Num1 = Divisor_Sum.Process(Num2) then
	   Put_Line(Integer'Image(Num1) & "," & Integer'Image(Num2));
	 end if;
      end if;
   end loop;
end Amicable_Pairs;
```

{{Out}}

```txt

 220, 284
 1184, 1210
 2620, 2924
 5020, 5564
 6232, 6368
 10744, 10856
 12285, 14595
 17296, 18416

```



## ALGOL 68


```algol68
# resturns the sum of the proper divisors of n                    #
# if n = 1, 0 or -1, we return 0                                  #
PROC sum proper divisors = ( INT n )INT:
     BEGIN
         INT result := 0;
         INT abs n = ABS n;
         IF abs n > 1 THEN
             FOR d FROM ENTIER sqrt( abs n ) BY -1 TO 2 DO
                 IF abs n MOD d = 0 THEN
                     # found another divisor                      #
                     result +:= d;
                     IF d * d /= n THEN
                         # include the other divisor              #
                         result +:= n OVER d
                     FI
                 FI
             OD;
             # 1 is always a proper divisor of numbers > 1        #
             result +:= 1
         FI;
         result
     END # sum proper divisors # ;

# construct a table of the sum of the proper divisors of numbers  #
# up to 20 000                                                    #
INT max number = 20 000;
[ 1 : max number ]INT proper divisor sum;
FOR n TO UPB proper divisor sum DO proper divisor sum[ n ] := sum proper divisors( n ) OD;

# returns TRUE if n1 and n2 are an amicable pair FALSE otherwise  #
#         n1 and n2 are amicable if the sum of the proper diviors #
#         n1 = n2 and the sum of the proper divisors of n2 = n1   #
PROC is an amicable pair = ( INT n1, n2 )BOOL:
     ( proper divisor sum[ n1 ] = n2 AND proper divisor sum[ n2 ] = n1 );

# find the amicable pairs up to 20 000                            #
FOR p1 TO max number DO
    FOR p2 FROM p1 + 1 TO max number DO
        IF is an amicable pair( p1, p2 ) THEN
            print( ( whole( p1, -6 ), " and ", whole( p2, -6 ), " are a amicable pair", newline ) )
        FI
    OD
OD
```

{{out}}

```txt

   220 and    284 are a amicable pair
  1184 and   1210 are a amicable pair
  2620 and   2924 are a amicable pair
  5020 and   5564 are a amicable pair
  6232 and   6368 are a amicable pair
 10744 and  10856 are a amicable pair
 12285 and  14595 are a amicable pair
 17296 and  18416 are a amicable pair

```


## ANSI Standard BASIC


{{Trans|GFA Basic}}


```ANSI Standard BASIC
100 DECLARE EXTERNAL FUNCTION sum_proper_divisors
110 CLEAR
120 !
130 DIM f(20001)      ! sum of proper factors for each n
140 FOR i=1 TO 20000
150    LET f(i)=sum_proper_divisors(i)
160 NEXT i
170 ! look for pairs
180 FOR i=1 TO 20000
190    FOR j=i+1 TO 20000
200       IF f(i)=j AND i=f(j) THEN
210          PRINT "Amicable pair ";i;" ";j
220       END IF
230    NEXT j
240 NEXT i
250 !
260 PRINT
270 PRINT "-- found all amicable pairs"
280 END
290 !
300 ! Compute the sum of proper divisors of given number
310 !
320 EXTERNAL FUNCTION sum_proper_divisors(n)
330 !
340 IF n>1 THEN ! n must be 2 or larger
350    LET sum=1 ! start with 1
360    LET root=SQR(n)    ! note that root is an integer
370    ! check possible factors, up to sqrt
380    FOR i=2 TO root
390       IF MOD(n,i)=0 THEN
400          LET sum=sum+i     ! i is a factor
410          IF i*i<>n THEN    ! check i is not actual square root of n
420             LET sum=sum+n/i  ! so n/i will also be a factor
430          END IF
440       END IF
450    NEXT i
460 END IF
470 LET sum_proper_divisors = sum
480 END FUNCTION
```



## AppleScript


{{Trans|JavaScript}}


```AppleScript
-- AMICABLE PAIRS ------------------------------------------------------------

-- amicablePairsUpTo :: Int -> Int
on amicablePairsUpTo(max)

    -- amicable :: [Int] -> Int -> Int -> [Int] -> [Int]
    script amicable
        on |λ|(a, m, n, lstSums)
            if (m > n) and (m ≤ max) and ((item m of lstSums) = n) then
                a & [[n, m]]
            else
                a
            end if
        end |λ|
    end script

    -- divisorsSummed :: Int -> Int
    script divisorsSummed
        -- sum :: Int -> Int -> Int
        script sum
            on |λ|(a, b)
                a + b
            end |λ|
        end script

        on |λ|(n)
            foldl(sum, 0, properDivisors(n))
        end |λ|
    end script

    foldl(amicable, {}, ¬
        map(divisorsSummed, enumFromTo(1, max)))
end amicablePairsUpTo


-- TEST ----------------------------------------------------------------------
on run

    amicablePairsUpTo(20000)

end run


-- PROPER DIVISORS -----------------------------------------------------------

-- properDivisors :: Int -> [Int]
on properDivisors(n)

    -- isFactor :: Int -> Bool
    script isFactor
        on |λ|(x)
            n mod x = 0
        end |λ|
    end script

    -- integerQuotient :: Int -> Int
    script integerQuotient
        on |λ|(x)
            (n / x) as integer
        end |λ|
    end script

    if n = 1 then
        {1}
    else
        set realRoot to n ^ (1 / 2)
        set intRoot to realRoot as integer
        set blnPerfectSquare to intRoot = realRoot

        -- Factors up to square root of n,
        set lows to filter(isFactor, enumFromTo(1, intRoot))

        -- and quotients of these factors beyond the square root,
        -- excluding n itself (last item)
        items 1 thru -2 of (lows & map(integerQuotient, ¬
            items (1 + (blnPerfectSquare as integer)) thru -1 of reverse of lows))
    end if
end properDivisors

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
```

{{Out}}

```AppleScript
{{220, 284}, {1184, 1210}, {2620, 2924}, {5020, 5564},
{6232, 6368}, {10744, 10856}, {12285, 14595}, {17296, 18416}}
```



## ATS


```ATS

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)
//
fun
sum_list_vt
  (xs: List_vt(int)): int =
(
  case+ xs of
  | ~list_vt_nil() => 0
  | ~list_vt_cons(x, xs) => x + sum_list_vt(xs)
)
//
(* ****** ****** *)

fun
propDivs
(
  x0: int
) : List0_vt(int) =
  loop(x0, 2, list_vt_sing(1)) where
{
//
fun
loop
(
x0: int, i: int, res: List0_vt(int)
) : List0_vt(int) =
(
if
(i * i) > x0
then list_vt_reverse(res)
else
(
  if x0 % i != 0
    then
      loop(x0, i+1, res)
    // end of [then]
    else let
      val res =
        cons_vt(i, res)
      // end of [val]
      val res =
      (
        if i * i = x0 then res else cons_vt(x0 / i, res)
      ) : List0_vt(int) // end of [val]
    in
      loop(x0, i+1, res)
    end // end of [else]
  // end of [if]
)
) (* end of [loop] *)
//
} // end of [propDivs]

(* ****** ****** *)

fun
sum_propDivs(x: int): int = sum_list_vt(propDivs(x))

(* ****** ****** *)

val
theNat2 = auxmain(2) where
{
fun
auxmain
(
 n: int
) : stream_vt(int) = $ldelay(stream_vt_cons(n, auxmain(n+1)))
}

(* ****** ****** *)
//
val
theAmicable =
(
stream_vt_takeLte(theNat2, 20000)
).filter()
(
lam x =>
let
  val x2 = sum_propDivs(x)
in x < x2 && x = sum_propDivs(x2) end
)
//
(* ****** ****** *)

val () =
theAmicable.foreach()
(
  lam x => println! ("(", x, ", ", sum_propDivs(x), ")")
)

(* ****** ****** *)

implement main0 () = ()

(* ****** ****** *)

```


{{out}}

```txt

(220, 284)
(1184, 1210)
(2620, 2924)
(5020, 5564)
(6232, 6368)
(10744, 10856)
(12285, 14595)
(17296, 18416)

```



## AutoHotkey


```d
SetBatchLines -1
Loop, 20000
{
	m := A_index

	; Getting factors
	loop % floor(sqrt(m))
	{
		if ( mod(m, A_index) = 0 )
		{
			if ( A_index ** 2 == m )
			{
				sum += A_index
				continue
			} else if ( A_index != 1 )
			{
				sum += A_index + m//A_index
			} else if ( A_index = 1 )
			{
				sum += A_index
			}
		}
	} ; Factors obtained

	; Checking factors of sum
	if ( sum > 1 )
	{
		loop % floor(sqrt(sum))
		{
			if ( mod(sum, A_index) = 0 )
			{
				if ( A_index ** 2 == sum )
				{
					sum2 += A_index
					continue
				} else if ( A_index != 1 )
				{
					sum2 += A_index + sum//A_index
				} else if ( A_index = 1 )
				{
					sum2 += A_index
				}
			}
		}
		if ( m = sum2 ) && ( m != sum ) && ( m < sum )
			final .= m . ":" . sum . "`n"
	} ; Checked

	sum := 0
	sum2 := 0
}
MsgBox % final
ExitApp
```

{{out}}

```txt

220:284
1184:1210
2620:2924
5020:5564
6232:6368
10744:10856
12285:14595
17296:18416

```



## AWK


```awk

#!/bin/awk -f
function sumprop(num,   i,sum,root) {
if (num < 2) return 0
sum=1
root=sqrt(num)
for ( i=2; i < root; i++) {
    if (num % i == 0 )
        {
          sum = sum + i + num/i
          }
     }
     if (num % root == 0)
        {
         sum = sum + root
         }
 return sum
 }

BEGIN{
limit=20000
print "Amicable pairs < ",limit
for (n=1; n < limit+1; n++)
    {
    m=sumprop(n)
    if (n == sumprop(m) && n < m) print n,m
    }
}
}
```

{{out}}

```txt

# ./amicable
Amicable pairs < 20000
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416

```



## Befunge



```befunge
v_@#-*8*:"2":$_:#!2#*8#g*#6:#0*#!:#-*#<v>*/.55+,
1>$$:28*:*:*%\28*:*:*/`06p28*:*:*/\2v %%^:*:<>*v
+|!:-1g60/*:*:*82::+**:*:<<>:#**#8:#<*^>.28*^8 :
:v>>*:*%/\28*:*:*%+\v>8+#$^#_+#`\:#0<:\`1/*:*2#<
2v^:*82\/*:*:*82:::_v#!%%*:*:*82\/*:*:*82::<_^#<
>>06p:28*:*:**1+01-\>1+::28*:*:*/\28*:*:*%:*\`!^
```


{{out}}

```txt
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
```



## C

Remark:
Look at Pascal Alternative [[http://rosettacode.org/wiki/Amicable_pairs#Alternative]].You are using the same principle, so here too both numbers of the pair must be < top.

The program will overflow and error in all sorts of ways when given a commandline argument >= UINT_MAX/2 (generally 2^31)

```c
#include <stdio.h>
#include <stdlib.h>

typedef unsigned int uint;

int main(int argc, char **argv)
{
  uint top = atoi(argv[1]);
  uint *divsum = malloc((top + 1) * sizeof(*divsum));
  uint pows[32] = {1, 0};

  for (uint i = 0; i <= top; i++) divsum[i] = 1;

  // sieve
  // only sieve within lower half , the modification starts at 2*p
  for (uint p = 2; p+p <= top; p++) {
    if (divsum[p] > 1) {
      divsum[p] -= p;// subtract number itself from divisor sum ('proper')
      continue;}     // p not prime

    uint x; // highest power of p we need
    //checking x <= top/y instead of x*y <= top to avoid overflow
    for (x = 1; pows[x - 1] <= top/p; x++)
      pows[x] = p*pows[x - 1];

    //counter where n is not a*p with a = ?*p, useful for most p.
    //think of p>31 seldom divisions or p>sqrt(top) than no division is needed
    //n = 2*p, so the prime itself is left unchanged => k=p-1
    uint k= p-1;
    for (uint n = p+p; n <= top; n += p) {
      uint s=1+pows[1];
      k--;
      // search the right power only if needed
      if ( k==0) {
        for (uint i = 2; i < x && !(n%pows[i]); s += pows[i++]);
        k = p; }
      divsum[n] *= s;
    }
  }

  //now correct the upper half
  for (uint p = (top >> 1)+1; p <= top; p++) {
    if (divsum[p] > 1){
      divsum[p] -= p;}
  }

  uint cnt = 0;
  for (uint a = 1; a <= top; a++) {
    uint b = divsum[a];
    if (b > a && b <= top && divsum[b] == a){
      printf("%u %u\n", a, b);
      cnt++;}
  }
  printf("\nTop %u count : %u\n",top,cnt);
  return 0;
}
```

{{out}}

```txt

% ./a.out 20000
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416

Top 20000 count : 8


% ./a.out 524000000
..
475838415 514823985
491373104 511419856
509379344 523679536

Top 524000000 count : 442

real  0m16.285s
user  0m16.156s

```



## C++


```cpp

#include <vector>
#include <unordered_map>
#include <iostream>

int main() {
    std::vector<int> alreadyDiscovered;
    std::unordered_map<int, int> divsumMap;
    int count = 0;

    for (int N = 1; N <= 20000; ++N)
    {
        int divSumN = 0;

        for (int i = 1; i <= N / 2; ++i)
        {
            if (fmod(N, i) == 0)
            {
                divSumN += i;
            }
        }

        // populate map of integers to the sum of their proper divisors
        if (divSumN != 1) // do not include primes
            divsumMap[N] = divSumN;

        for (std::unordered_map<int, int>::iterator it = divsumMap.begin(); it != divsumMap.end(); ++it)
        {
            int M = it->first;
            int divSumM = it->second;
            int divSumN = divsumMap[N];

            if (N != M && divSumM == N && divSumN == M)
            {
                // do not print duplicate pairs
                if (std::find(alreadyDiscovered.begin(), alreadyDiscovered.end(), N) != alreadyDiscovered.end())
                    break;

                std::cout << "[" << M << ", " << N << "]" << std::endl;

                alreadyDiscovered.push_back(M);
                alreadyDiscovered.push_back(N);
                count++;
            }
        }
    }

    std::cout << count << " amicable pairs discovered" << std::endl;
}

```

{{out}}

```txt

[220, 284]
[1184, 1210]
[2620, 2924]
[5020, 5564]
[6232, 6368]
[10744, 10856]
[12285, 14595]
[17296, 18416]
8 amicable pairs discovered

```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace RosettaCode.AmicablePairs
{
    internal static class Program {
        private const int Limit = 20000;

        private static void Main()
        {
            foreach (var pair in GetPairs(Limit))
            {
                Console.WriteLine("{0} {1}", pair.Item1, pair.Item2);
            }
        }

        private static IEnumerable<Tuple<int, int>> GetPairs(int max)
        {
            List<int> divsums =
                Enumerable.Range(0, max + 1).Select(i => ProperDivisors(i).Sum()).ToList();
            for(int i=1; i<divsums.Count; i++) {
                int sum = divsums[i];
                if(i < sum && sum <= divsums.Count && divsums[sum] == i) {
                    yield return new Tuple<int, int>(i, sum);
                }
            }
        }

        private static IEnumerable<int> ProperDivisors(int number)
        {
            return
                Enumerable.Range(1, number / 2)
                    .Where(divisor => number % divisor == 0);
        }
    }
}
```

{{out}}

```txt

220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416

```



## Clojure


```lisp

(ns example
  (:gen-class))

(defn factors [n]
  " Find the proper factors of a number "
  (into (sorted-set)
        (mapcat (fn [x] (if (= x 1) [x] [x (/ n x)]))
                (filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n)))) )))


(def find-pairs (into #{}
               (for [n (range  2 20000)
                  :let [f (factors n)     ; Factors of n
                        M (apply + f)     ; Sum of factors
                        g (factors M)     ; Factors of sum
                        N (apply + g)]    ; Sum of Factors of sum
                  :when (= n N)           ; (sum(proDivs(N)) = M and sum(propDivs(M)) = N
                  :when (not= M N)]       ; N not-equal M
                 (sorted-set n M))))      ; Found pair

;; Output Results
(doseq [q find-pairs]
  (println q))

```

{{Out}}

```txt

#{220 284}
#{6232 6368}
#{1184 1210}
#{5020 5564}
#{2620 2924}
#{12285 14595}
#{17296 18416}
#{10744 10856}

```



## Common Lisp


```lisp
(let ((cache (make-hash-table)))
  (defun sum-proper-divisors (n)
    (or (gethash n cache)
        (setf (gethash n cache)
              (loop for x from 1 to (/ n 2)
                    when (zerop (rem n x))
                      sum x)))))

(defun amicable-pairs-up-to (n)
  (loop for x from 1 to n
        for sum-divs = (sum-proper-divisors x)
        when (and (< x sum-divs) (= x (sum-proper-divisors sum-divs)))
          collect (list x sum-divs)))

(amicable-pairs-up-to 20000)
```

{{out}}

```txt
((220 284) (1184 1210) (2620 2924) (5020 5564) (6232 6368) (10744 10856)
 (12285 14595) (17296 18416))
```



## Crystal


```Crystal

MX = 524_000_000
N = Math.sqrt(MX).to_u32
x = Array(Int32).new(MX+1, 1)

(2..N).each { |i|
    p = i*i
    x[p] += i
    k = i+i+1
    (p+i..MX).step(i) { |j|
        x[j] += k
        k += 1
    }
}

(4..MX).each { |m|
    n = x[m]
    if n < m && n != 0 && m == x[n]
        puts "#{n} #{m}"
    end
}

```

{{out}}

```txt

220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
...... ......
....... .......
426191535 514780497
475838415 514823985
509379344 523679536

```



## D

{{trans|Python}}

```d
void main() @safe /*@nogc*/ {
    import std.stdio, std.algorithm, std.range, std.typecons, std.array;

    immutable properDivs = (in uint n) pure nothrow @safe /*@nogc*/ =>
        iota(1, (n + 1) / 2 + 1).filter!(x => n % x == 0);

    enum rangeMax = 20_000;
    auto n2d = iota(1, rangeMax + 1).map!(n => properDivs(n).sum);

    foreach (immutable n, immutable divSum; n2d.enumerate(1))
        if (n < divSum && divSum <= rangeMax && n2d[divSum - 1] == n)
            writefln("Amicable pair: %d and %d with proper divisors:\n    %s\n    %s",
                     n, divSum, properDivs(n), properDivs(divSum));
}
```

{{out}}

```txt
Amicable pair: 220 and 284 with proper divisors:
    [1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110]
    [1, 2, 4, 71, 142]
Amicable pair: 1184 and 1210 with proper divisors:
    [1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592]
    [1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605]
Amicable pair: 2620 and 2924 with proper divisors:
    [1, 2, 4, 5, 10, 20, 131, 262, 524, 655, 1310]
    [1, 2, 4, 17, 34, 43, 68, 86, 172, 731, 1462]
Amicable pair: 5020 and 5564 with proper divisors:
    [1, 2, 4, 5, 10, 20, 251, 502, 1004, 1255, 2510]
    [1, 2, 4, 13, 26, 52, 107, 214, 428, 1391, 2782]
Amicable pair: 6232 and 6368 with proper divisors:
    [1, 2, 4, 8, 19, 38, 41, 76, 82, 152, 164, 328, 779, 1558, 3116]
    [1, 2, 4, 8, 16, 32, 199, 398, 796, 1592, 3184]
Amicable pair: 10744 and 10856 with proper divisors:
    [1, 2, 4, 8, 17, 34, 68, 79, 136, 158, 316, 632, 1343, 2686, 5372]
    [1, 2, 4, 8, 23, 46, 59, 92, 118, 184, 236, 472, 1357, 2714, 5428]
Amicable pair: 12285 and 14595 with proper divisors:
    [1, 3, 5, 7, 9, 13, 15, 21, 27, 35, 39, 45, 63, 65, 91, 105, 117, 135, 189, 195, 273, 315, 351, 455, 585, 819, 945, 1365, 1755, 2457, 4095]
    [1, 3, 5, 7, 15, 21, 35, 105, 139, 417, 695, 973, 2085, 2919, 4865]
Amicable pair: 17296 and 18416 with proper divisors:
    [1, 2, 4, 8, 16, 23, 46, 47, 92, 94, 184, 188, 368, 376, 752, 1081, 2162, 4324, 8648]
    [1, 2, 4, 8, 16, 1151, 2302, 4604, 9208]
```



## EchoLisp


```scheme

;; using (sum-divisors) from math.lib

(lib 'math)
(define (amicable N)
(define n 0)
	(for/list ((m (in-range 2 N)))
		(set! n (sum-divisors m))
		#:continue (>= n (* 1.5 m))  ;; assume n/m < 1.5
		#:continue (<= n m) ;; prevent perfect numbers
		#:continue (!= (sum-divisors n) m)
		(cons m n)))

(amicable 20000)
    → ((220 . 284) (1184 . 1210) (2620 . 2924) (5020 . 5564) (6232 . 6368) (10744 . 10856) (12285 . 14595) (17296 . 18416))

(amicable 1_000_000) ;; 42 pairs
   → (... (802725 . 863835) (879712 . 901424) (898216 . 980984) (947835 . 1125765) (998104 . 1043096))


```



## Ela

{{trans|Haskell}}

```ela
open monad io number list

divisors n = filter ((0 ==) << (n `mod`)) [1..(n `div` 2)]
range = [1 .. 20000]
divs = zip range $ map (sum << divisors) range
pairs = [(n, m) \\ (n, nd) <- divs, (m, md) <- divs | n < m && nd == m && md == n]

do putLn pairs ::: IO
```


{{out}}

```txt

[(220,284),(1184,1210),(2620,2924),(5020,5564),(6232,6368),(10744,10856),(12285,14595),(17296,18416)]
```


## Elena

{{trans|C#}}
ELENA 4.1 :

```elena
import extensions;
import system'routines;
import system'math;

const int N = 20000;

extension op
{
    ProperDivisors
        = Range.new(1,self / 2).filterBy:(n => self.mod:n == 0);

    get AmicablePairs()
    {
        var divsums := Range
                         .new(0, self + 1)
                         .selectBy:(i => i.ProperDivisors.summarize(new Integer()))
                         .toArray();

        ^ 1.repeatTill(divsums.Length)
            .filterBy:(i)
            {
                var ii := i;

                var sum := divsums[i];
                ^ (i < sum) && (sum < divsums.Length) && (divsums[sum] == i)
            }
            .selectBy:(i => new::{ Item1 = i; Item2 = divsums[i]; })
    }
}

public program()
{
    N.AmicablePairs.forEach:(pair)
    {
        console.printLine(pair.Item1, " ", pair.Item2)
    }
}
```

{{out}}

```txt

220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416

```

=== Alternative variant using strong-typed closures ===

```elena
import extensions;
import system'routines'stex;
import system'math;
import system'collections;

const int N = 20000;

extension op : IntNumber
{
    Enumerator<int> ProperDivisors
        = new Range(1,self / 2).filterBy:(int n => self.mod:n == 0);

    get AmicablePairs()
    {
        auto divsums := new List<int>(cast Enumerator<int>(new Range(0, self).selectBy:(int i => i.ProperDivisors.summarize(0))));

        ^ new Range(0, divsums.Length)
            .filterBy:(int i)
            {
                auto sum := divsums[i];
                ^ (i < sum) && (sum < divsums.Length) && (divsums[sum] == i)
            }
            .selectBy:(int i => new Tuple<int,int>(i,divsums[i]));
    }
}

public program()
{
    N.AmicablePairs.forEach:(var Tuple<int,int> pair)
    {
        console.printLine(pair.Item1, " ", pair.Item2)
    }
}
```

{{out}}

```txt

220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416

```



## Elixir

{{works with|Elixir|1.2}}
With [[proper_divisors#Elixir]] in place:

```elixir
defmodule Proper do
  def divisors(1), do: []
  def divisors(n), do: [1 | divisors(2,n,:math.sqrt(n))] |> Enum.sort

  defp divisors(k,_n,q) when k>q, do: []
  defp divisors(k,n,q) when rem(n,k)>0, do: divisors(k+1,n,q)
  defp divisors(k,n,q) when k * k == n, do: [k | divisors(k+1,n,q)]
  defp divisors(k,n,q)                , do: [k,div(n,k) | divisors(k+1,n,q)]
end

map = Map.new(1..20000, fn n -> {n, Proper.divisors(n) |> Enum.sum} end)
Enum.filter(map, fn {n,sum} -> map[sum] == n and n < sum end)
|> Enum.sort
|> Enum.each(fn {i,j} -> IO.puts "#{i} and #{j}" end)
```


{{out}}

```txt

220 and 284
1184 and 1210
2620 and 2924
5020 and 5564
6232 and 6368
10744 and 10856
12285 and 14595
17296 and 18416

```



## Erlang

===Erlang, slow===
Very slow solution. Same functions by and large as in proper divisors and co.


```erlang

-module(properdivs).
-export([amicable/1,divs/1,sumdivs/1]).

amicable(Limit) -> amicable(Limit,[],3,2).

amicable(Limit,List,_Current,Acc) when Acc >= Limit -> List;
amicable(Limit,List,Current,Acc) when Current =< Acc/2  ->
    amicable(Limit,List,Acc,Acc+1);
amicable(Limit,List,Current,Acc) ->
    CS = sumdivs(Current),
    AS = sumdivs(Acc),
    if
        CS == Acc andalso AS == Current andalso Acc =/= Current ->
          io:format("A: ~w, B: ~w, ~nL: ~w~w~n",  [Current,Acc,divs(Current),divs(Acc)]),
          NL = List ++ [{Current,Acc}],
          amicable(Limit,NL,Acc+1,Acc+1);
        true ->
          amicable(Limit,List,Current-1,Acc) end.

divs(0) -> [];
divs(1) -> [];
divs(N) -> lists:sort(divisors(1,N)).

divisors(1,N) ->
     [1] ++ divisors(2,N,math:sqrt(N)).

divisors(K,_N,Q) when K > Q -> [];
divisors(K,N,_Q) when N rem K =/= 0 ->
    [] ++ divisors(K+1,N,math:sqrt(N));
divisors(K,N,_Q) when K * K  == N ->
    [K] ++ divisors(K+1,N,math:sqrt(N));
divisors(K,N,_Q) ->
    [K, N div K] ++ divisors(K+1,N,math:sqrt(N)).

sumdivs(N) -> lists:sum(divs(N)).

```

{{out}}

```txt

3> properdivs:amicable(20000).
A: 220, B: 284,
L: [1,2,4,5,10,11,20,22,44,55,110][1,2,4,71,142]
A: 1184, B: 1210,
L: [1,2,4,8,16,32,37,74,148,296,592][1,2,5,10,11,22,55,110,121,242,605]
A: 2620, B: 2924,
L: [1,2,4,5,10,20,131,262,524,655,1310][1,2,4,17,34,43,68,86,172,731,1462]
A: 5020, B: 5564,
L: [1,2,4,5,10,20,251,502,1004,1255,2510][1,2,4,13,26,52,107,214,428,1391,2782]
A: 6232, B: 6368,
L: [1,2,4,8,19,38,41,76,82,152,164,328,779,1558,3116][1,2,4,8,16,32,199,398,796,1592,3184]
A: 10744, B: 10856,
L: [1,2,4,8,17,34,68,79,136,158,316,632,1343,2686,5372][1,2,4,8,23,46,59,92,118,184,236,472,1357,2714,5428]
A: 12285, B: 14595,
L: [1,3,5,7,9,13,15,21,27,35,39,45,63,65,91,105,117,135,189,195,273,315,351,455,585,819,945,1365,1755,2457,4095][1,3,5,7,15,21,35,105,139,417,695,973,2085,2919,4865]
A: 17296, B: 18416,
L: [1,2,4,8,16,23,46,47,92,94,184,188,368,376,752,1081,2162,4324,8648][1,2,4,8,16,1151,2302,4604,9208]
[{220,284},
 {1184,1210},
 {2620,2924},
 {5020,5564},
 {6232,6368},
 {10744,10856},
 {12285,14595},
 {17296,18416}]

```


===Erlang, faster===
This is lazy AND depends on the fun fact that we're not really identifying pairs. They just happen to order.
Probably, this answer is false in some sense. But a good deal faster :) As above with the additional function.

[See the talk section   '''amicable pairs, out of order'''   for this Rosetta Code task.]


```erlang

friendly(Limit) ->
    List = [{X,properdivs:sumdivs(X)} || X <- lists:seq(3,Limit)],
    Final = [ X ||
        X <- lists:seq(3,Limit),
        X == properdivs:sumdivs(proplists:get_value(X,List))
        andalso X =/= proplists:get_value(X,List)],
    io:format("L: ~w~n", [Final]).


```

{{output}}


```txt

45> properdivs:friendly(20000).
L: [220,284,1184,1210,2620,2924,5020,5564,6232,6368,10744,10856,12285,14595,17296,18416]
ok

```


We might answer a challenge by saying:

```erlang

friendly(Limit) ->
    List = [{X,properdivs:sumdivs(X)} || X <- lists:seq(3,Limit)],
    Final = [ X || X <- lists:seq(3,Limit), X == properdivs:sumdivs(proplists:get_value(X,List))
            andalso X =/= proplists:get_value(X,List)],
    findfriendlies(Final,[]).


findfriendlies(List,Acc) when length(List) =< 0 -> Acc;
findfriendlies(List,Acc) ->
    A = lists:nth(1,List),
    AS = sumdivs(A),
    B = lists:nth(2,List),
    BS = sumdivs(B),
    if
        AS == B andalso BS == A ->
          {_,BL} = lists:split(2,List),
          findfriendlies(BL,Acc++[{A,B}]);
        true -> false
    end.


```

{{out}}

```txt

94>  properdivs:friendly(20000).
[{220,284},
 {1184,1210},
 {2620,2924},
 {5020,5564},
 {6232,6368},
 {10744,10856},
 {12285,14595},
 {17296,18416}]

```


In either case, it's a lot faster than the recursion in my first example.


## ERRE


```ERRE
PROGRAM AMICABLE

CONST LIMIT=20000

PROCEDURE SUMPROP(NUM->M)
  IF NUM<2 THEN M=0 EXIT PROCEDURE
  SUM=1
  ROOT=SQR(NUM)
  FOR I=2 TO ROOT-1 DO
     IF (NUM=I*INT(NUM/I)) THEN
         SUM=SUM+I+NUM/I
     END IF
     IF (NUM=ROOT*INT(NUM/ROOT)) THEN
         SUM=SUM+ROOT
     END IF
  END FOR
  M=SUM
END PROCEDURE

BEGIN
  PRINT(CHR$(12);) ! CLS
  PRINT("Amicable pairs < ";LIMIT)
  FOR N=1 TO LIMIT DO
    SUMPROP(N->M1)
    SUMPROP(M1->M2)
    IF (N=M2 AND N<M1) THEN PRINT(N,M1)
  END FOR
END PROGRAM
```

{{out}}

```txt
Amicable pairs <  20000
 220           284
 1184          1210
 2620          2924
 5020          5564
 6232          6368
 10744         10856
 12285         14595
 17296         18416

```



## F#


```fsharp

[2..20000 - 1]
|> List.map (fun n-> n, ([1..n/2] |> List.filter (fun x->n % x = 0) |> List.sum))
|> List.map (fun (a,b) ->if a<b then (a,b) else (b,a))
|> List.groupBy id
|> List.map snd
|> List.filter (List.length >> ((=) 2))
|> List.map List.head
|> List.iter (printfn "%A")

```

{{out}}

```txt

(220, 284)
(1184, 1210)
(2620, 2924)
(5020, 5564)
(6232, 6368)
(10744, 10856)
(12285, 14595)
(17296, 18416)

```



## Factor

This solution focuses on the language's namesake: factoring code into small words which are subsequently composed to form more powerful — yet just as simple — words. Using this approach, the final word naturally arrives at the solution. This is often referred to as the bottom-up approach, which is a way in which Factor (and other concatenative languages) commonly differs from other languages.


```Factor

USING: grouping math.primes.factors math.ranges ;

: pdivs      ( n -- seq )   divisors but-last ;
: dsum       ( n -- sum )   pdivs sum ;
: dsum=      ( n m -- ? )   dsum = ;
: both-dsum= ( n m -- ? )   [ dsum= ] [ swap dsum= ] 2bi and ;
: amicable?  ( n m -- ? )   [ both-dsum= ] [ = not ] 2bi and ;
: drange     ( -- seq )     2 20000 [a,b) ;
: dsums      ( -- seq )     drange [ dsum ] map ;
: is-am?-seq ( -- seq )     dsums drange [ amicable? ] 2map ;
: am-nums    ( -- seq )     t is-am?-seq indices ;
: am-nums-c  ( -- seq )     am-nums [ 2 + ] map ;
: am-pairs   ( -- seq )     am-nums-c 2 group ;
: print-am   ( -- )         am-pairs [ >array . ] each ;

print-am

```

{{out}}

```txt

{ 220 284 }
{ 1184 1210 }
{ 2620 2924 }
{ 5020 5564 }
{ 6232 6368 }
{ 10744 10856 }
{ 12285 14595 }
{ 17296 18416 }

```



## Fortran

This version uses some latter-day facilities such as array assignment that could be replaced by an ordinary DO-loop, as could the FOR ALL statement that for two adds two to every second element, for three adds three to every third, etc. Each FORALL statement applies its DO-given increment to all the selected array elements potentially in any order or even simultaneously. Likewise, the "MODULE" protocol could be abandoned, which would mean that the KNOWNSUM array would have to be declared COMMON for access across routines - or the whole re-written as a single mainline. And if the PARAMETER statements were replaced appropriately, this source could be compiled using Fortran 77.

Output:
 Perfect!!           6
 Perfect!!          28
 Amicable!         220         284
 Perfect!!         496
 Amicable!        1184        1210
 Amicable!        2620        2924
 Amicable!        5020        5564
 Amicable!        6232        6368
 Perfect!!        8128
 Amicable!       10744       10856
 Amicable!       12285       14595
 Amicable!       17296       18416


```FORTRAN

      MODULE FACTORSTUFF	!This protocol evades the need for multiple parameters, or COMMON, or one shapeless main line...
Concocted by R.N.McLean, MMXV.
       INTEGER LOTS,ILIMIT		!Some bounds.
       PARAMETER (ILIMIT = 2147483647)	!Computer arithmetic is not with real numbers.
       PARAMETER (LOTS = 22000)	!Nor is computer storage infinite.
       INTEGER KNOWNSUM(LOTS)		!Calculate these once as multiple references are expected.
       CONTAINS			!Assistants.
        INTEGER FUNCTION SUMF(N)	!Sum of the proper divisors of N.
         INTEGER N			!The number in question.
         INTEGER S,F,F2,INC,BOOST	!Assistants.
          IF (N.LE.LOTS) THEN		!If we're within reach,
            SUMF = KNOWNSUM(N)			!The result is to hand.
           ELSE			!Otherwise, some on-the-spot effort ensues.
Could use SUMF in place of S, but some compilers have been confused by such usage.
            S = 1			!1 is always a factor of N, but N is deemed not.
            F = 1			!Prepare a crude search for factors.
            INC = 1			!One by plodding one.
            IF (MOD(N,2) .EQ. 1) INC = 2!Ah, but an odd number cannot have an even number as a divisor.
    1       F = F + INC			!So half the time we can doubleplod.
            F2 = F*F				!Up to F2 < N rather than F < SQRT(N) and worries over inexact arithmetic.
            IF (F2 .LT. N) THEN			!F2 = N handled below.
              IF (MOD(N,F) .EQ. 0) THEN		!Does F divide N?
                BOOST = F + N/F			!Yes. The divisor and its counterpart.
                IF (S .GT. ILIMIT - BOOST) GO TO 666	!Would their augmentation cause an overflow?
                S = S + BOOST			!No, so count in the two divisors just discovered.
              END IF				!So much for a divisor discovered.
              GO TO 1				!Try for another.
            END IF			!So much for the horde.
            IF (F2 .EQ. N) THEN	!Special case: N may be a perfect square, not necessarily of a prime number.
              IF (S .GT. ILIMIT - F) GO TO 666	!It is. And it too might cause overflow.
              S = S + F			!But if not, count F once only.
            END IF			!All done.
            SUMF = S			!This is the result.
          END IF			!Whichever way obtained,
         RETURN			!Done.
Cannot calculate the sum, because it exceeds the integer limit.
  666     SUMF = -666		!An expression of dismay that the caller will notice.
        END FUNCTION SUMF	!Alternatively, find the prime factors, and combine them...
         SUBROUTINE PREPARESUMF	!Initialise the KNOWNSUM array.
Convert the Sieve of Eratoshenes to have each slot contain the sum of the proper divisors of its slot number.
Changes to instead count the number of factors, or prime factors, etc. would be simple enough.
         INTEGER F		!A factor for numbers such as 2F, 3F, 4F, 5F, ...
          KNOWNSUM(1) = 0		!Proper divisors of N do not include N.
          KNOWNSUM(2:LOTS) = 1		!So, although 1 is a proper divisor of all N, 1 is excluded for itself.
          DO F = 2,LOTS/2		!Step through all the possible divisors of numbers not exceeding LOTS.
            FOR ALL(I = F + F:LOTS:F) KNOWNSUM(I) = KNOWNSUM(I) + F	!And augment each corresponding slot.
          END DO			!Different divisors can hit the same slot. For instance, 6 by 2 and also by 3.
        END SUBROUTINE PREPARESUMF	!Could alternatively generate all products of prime numbers.
      END MODULE FACTORSTUFF	!Enough assistants.
       PROGRAM AMICABLE		!Seek N such that SumF(SumF(N)) = N, for N up to 20,000.
       USE FACTORSTUFF		!This should help.
       INTEGER I,N		!Steppers.
       INTEGER S1,S2		!Sums of factors.
        CALL PREPARESUMF		!Values for every N up to the search limit will be called for at least once.
c        WRITE (6,66) (I,KNOWNSUM(I), I = 1,48)
c   66   FORMAT (10(I3,":",I5,"|"))
        DO N = 2,20000		!Step through the specified search space.
          S1 = SUMF(N)			!Only even numbers appear in the results, but check every one anyway.
          IF (S1 .EQ. N) THEN		!Catch a tight loop.
            WRITE (6,*) "Perfect!!",N	!Self amicable! Would otherwise appear as Amicable! n,n.
          ELSE IF (S1 .GT. N) THEN	!Look for a pair going upwards only.
            S2 = SUMF(S1)		!Since otherwise each would appear twice.
            IF (S2.EQ.N) WRITE (6,*) "Amicable!",N,S1	!Aha!
          END IF			!So much for that candidate.
        END DO			!On to the next.
      END			!Done.

```



## FreeBASIC


### using Mod


```freebasic

' FreeBASIC v1.05.0 win64

Function SumProperDivisors(number As Integer) As Integer
  If number < 2 Then Return 0
  Dim sum As Integer = 0
  For i As Integer = 1 To number \ 2
    If number Mod i = 0 Then sum += i
  Next
  Return sum
End Function

Dim As Integer n, f
Dim As Integer sum(19999)

For n = 1 To 19999
  sum(n) = SumProperDivisors(n)
Next

Print "The pairs of amicable numbers below 20,000 are :"
Print

For n = 1 To 19998
  ' f = SumProperDivisors(n)
  f = sum(n)
  If f <= n OrElse f < 1 OrElse f > 19999 Then Continue For
  If f = sum(n) AndAlso n = sum(f) Then
    Print Using "#####"; n;
    Print " and "; Using "#####"; sum(n)
  End If
Next

Print
Print "Press any key to exit the program"
Sleep
End

```


{{out}}

```txt

The pairs of amicable numbers below 20,000 are :

  220 and   284
 1184 and  1210
 2620 and  2924
 5020 and  5564
 6232 and  6368
10744 and 10856
12285 and 14595
17296 and 18416

```

===using "Sieve of Erathosthenes" style===

```freebasic
' version 04-10-2016
' compile with: fbc -s console
' replaced the function with 2 FOR NEXT loops

#Define max 20000      ' test for pairs below max
#Define max_1 max -1

Dim As String u_str = String(Len(Str(max))+1,"#")
Dim As UInteger n, f
Dim Shared As UInteger sum(max_1)

For n = 2 To max_1
  sum(n) = 1
Next

For n = 2 To max_1 \ 2
  For f  = n * 2 To max_1 Step n
    sum(f) += n
  Next
Next

Print
Print Using " The pairs of amicable numbers below" & u_str & ", are :"; max
Print

For n = 1 To max_1 -1
  f = Sum(n)
  If f <= n OrElse f > max Then Continue For
  If f = sum(n) AndAlso n = sum(f) Then
    Print Using u_str & " and" & u_str ; n; f
  End If
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print : Print " Hit any key to end program"
Sleep
End
```


```txt

 The pairs of amicable numbers below 20,000 are :

   220 and   284
  1184 and  1210
  2620 and  2924
  5020 and  5564
  6232 and  6368
 10744 and 10856
 12285 and 14595
 17296 and 18416
```



## Frink

This example uses Frink's built-in efficient factorization algorithms.  It can work for arbitrarily large numbers.

```frink

n = 1
seen = new set

do
{
   n = n + 1
   if seen.contains[n]
      next

   sum = sum[allFactors[n, true, false, false]]
   if sum != n and sum[allFactors[sum, true, false, false]] == n
   {
      println["$n, $sum"]
      seen.put[sum]
   }
} while n <= 20000

```

{{out}}

```txt

220, 284
1184, 1210
2620, 2924
5020, 5564
6232, 6368
10744, 10856
12285, 14595
17296, 18416

```



## Futhark


{{output?}}

This program is much too parallel and manifests all the pairs, which requires a giant amount of memory.

<lang>
fun divisors(n: int): []int =
  filter (fn x => n%x == 0) (map (1+) (iota (n/2)))

fun amicable((n: int, nd: int), (m: int, md: int)): bool =
  n < m && nd == m && md == n

fun getPair (divs: [upper](int, int)) (flat_i: int): ((int,int), (int,int)) =
  let i = flat_i / upper
  let j = flat_i % upper
  in unsafe (divs[i], divs[j])

fun main(upper: int): [][2]int =
  let range = map (1+) (iota upper)
  let divs = zip range (map (fn n => reduce (+) 0 (divisors n)) range)
  let amicable = filter amicable (map (getPair divs) (iota (upper*upper)))
  in map (fn (np,mp) => [#1 np, #1 mp]) amicable

```



## GFA Basic


<lang>
OPENW 1
CLEARW 1
'
DIM f%(20001) ! sum of proper factors for each n
FOR i%=1 TO 20000
  f%(i%)=@sum_proper_divisors(i%)
NEXT i%
' look for pairs
FOR i%=1 TO 20000
  FOR j%=i%+1 TO 20000
    IF f%(i%)=j% AND i%=f%(j%)
      PRINT "Amicable pair ";i%;" ";j%
    ENDIF
  NEXT j%
NEXT i%
'
PRINT
PRINT "-- found all amicable pairs"
~INP(2)
CLOSEW 1
'
' Compute the sum of proper divisors of given number
'
FUNCTION sum_proper_divisors(n%)
  LOCAL i%,sum%,root%
  '
  IF n%>1 ! n% must be 2 or larger
    sum%=1 ! start with 1
    root%=SQR(n%) ! note that root% is an integer
    ' check possible factors, up to sqrt
    FOR i%=2 TO root%
      IF n% MOD i%=0
        sum%=sum%+i% ! i% is a factor
        IF i%*i%<>n% ! check i% is not actual square root of n%
          sum%=sum%+n%/i% ! so n%/i% will also be a factor
        ENDIF
      ENDIF
    NEXT i%
  ENDIF
  RETURN sum%
ENDFUNC

```


Output is:

```txt

Amicable pair: 220 284
Amicable pair: 1184 1210
Amicable pair: 2620 2924
Amicable pair: 5020 5564
Amicable pair: 6232 6368
Amicable pair: 10744 10856
Amicable pair: 12285 14595
Amicable pair: 17296 18416

-- found all amicable pairs

```


## Go


```Go

package main
import "fmt"
import "math"
func main() {
var i int
var a [200001]int
a[0]=0
for i=1;i<=20000;i++{
a[i]=pfac_sum(i)
}
fmt.Println("The amicable pairs are:")
for i=1;i<=20000;i++{
if (i==a[a[i]])&&(i<a[i]){
  fmt.Printf("%d , %d\n",i,a[i])
}
}
}
func pfac_sum(i int) int {
	var p,sum=1,0

	for p=1;p<=i/2;p++{
	x := float64(i)
	y := float64(p)
	  if math.Mod(x,y)==0{
	   sum= sum+p
	  }
	}
	return sum
}

```

Output:

```txt

The amicable pairs are:
220 , 284
1184 , 1210
2620 , 2924
5020 , 5564
6232 , 6368
10744 , 10856
12285 , 14595
17296 , 18416

```


## Haskell


```Haskell
divisors :: (Integral a) => a -> [a]
divisors n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

main :: IO ()
main = do
  let range = [1 .. 20000 :: Int]
      divs = zip range $ map (sum . divisors) range
      pairs = [(n, m) | (n, nd) <- divs, (m, md) <- divs,
               n < m, nd == m, md == n]
  print pairs
```

{{out}}

```txt
[(220,284),(1184,1210),(2620,2924),(5020,5564),(6232,6368),(10744,10856),(12285,14595),(17296,18416)]
```



Or, deriving proper divisors above the square root as cofactors (for better performance)


```Haskell
import Data.Bool (bool)

amicablePairsUpTo :: Int -> [(Int, Int)]
amicablePairsUpTo n =
  let sigma = sum . properDivisors
  in [1 .. n] >>=
     (\x ->
         let y = sigma x
         in bool [] [(x, y)] (x < y && x == sigma y))

properDivisors
  :: Integral a
  => a -> [a]
properDivisors n =
  let root = (floor . sqrt) (fromIntegral n :: Double)
      lows = filter ((0 ==) . rem n) [1 .. root]
  in init $
     lows ++ drop (bool 0 1 (root * root == n)) (reverse (quot n <$> lows))

main :: IO ()
main = mapM_ print $ amicablePairsUpTo 20000
```

{{Out}}

```txt
(220,284)
(1184,1210)
(2620,2924)
(5020,5564)
(6232,6368)
(10744,10856)
(12285,14595)
(17296,18416)
```



## J


[[Proper divisors#J|Proper Divisor implementation]]:


```J
factors=: [: /:~@, */&>@{@((^ i.@>:)&.>/)@q:~&__
properDivisors=: factors -. -.&1
```


Amicable pairs:


```J
   1 + 0 20000 #: I. ,(</~@i.@# * (* |:))(=/ +/@properDivisors@>) 1 + i.20000
  220   284
 1184  1210
 2620  2924
 5020  5564
 6232  6368
10744 10856
12285 14595
17296 18416
```



## Java

{{works with|Java|8}}

```java
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

public class AmicablePairs {

    public static void main(String[] args) {
        int limit = 20_000;

        Map<Long, Long> map = LongStream.rangeClosed(1, limit)
                .parallel()
                .boxed()
                .collect(Collectors.toMap(Function.identity(), AmicablePairs::properDivsSum));

        LongStream.rangeClosed(1, limit)
                .forEach(n -> {
                    long m = map.get(n);
                    if (m > n && m <= limit && map.get(m) == n)
                        System.out.printf("%s %s %n", n, m);
                });
    }

    public static Long properDivsSum(long n) {
        return LongStream.rangeClosed(1, (n + 1) / 2).filter(i -> n % i == 0).sum();
    }
}
```



```txt
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
```



## JavaScript



### ES5



```JavaScript
(function (max) {

    // Proper divisors
    function properDivisors(n) {
        if (n < 2) return [];
        else {
            var rRoot = Math.sqrt(n),
                intRoot = Math.floor(rRoot),

                lows = range(1, intRoot).filter(function (x) {
                    return (n % x) === 0;
                });

            return lows.concat(lows.slice(1).map(function (x) {
                return n / x;
            }).reverse().slice((rRoot === intRoot) | 0));
        }
    }

    // [m..n]
    function range(m, n) {
        var a = Array(n - m + 1),
            i = n + 1;
        while (i--) a[i - 1] = i;
        return a;
    }

    // Filter an array of proper divisor sums,
    // reading the array index as a function of N (N-1)
    // and the sum of proper divisors as a potential M

    var pairs = range(1, max).map(function (x) {
        return properDivisors(x).reduce(function (a, d) {
            return a + d;
        }, 0)
    }).reduce(function (a, m, i, lst) {
        var n = i + 1;

        return (m > n) && lst[m - 1] === n ? a.concat([[n, m]]) : a;
    }, []);

    // [[a]] -> bool -> s -> s
    function wikiTable(lstRows, blnHeaderRow, strStyle) {
        return '{| class="wikitable" ' + (
            strStyle ? 'style="' + strStyle + '"' : ''
        ) + lstRows.map(function (lstRow, iRow) {
            var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

            return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
                return typeof v === 'undefined' ? ' ' : v;
            }).join(' ' + strDelim + strDelim + ' ');
        }).join('') + '\n|}';
    }

    return wikiTable(
        [['N', 'M']].concat(pairs),
        true,
        'text-align:center'
    ) + '\n\n' + JSON.stringify(pairs);

})(20000);
```


{{out}}

{| class="wikitable" style="text-align:center"
|-
! N !! M
|-
| 220 || 284
|-
| 1184 || 1210
|-
| 2620 || 2924
|-
| 5020 || 5564
|-
| 6232 || 6368
|-
| 10744 || 10856
|-
| 12285 || 14595
|-
| 17296 || 18416
|}


```JavaScript
[[220,284],[1184,1210],[2620,2924],[5020,5564],
 [6232,6368],[10744,10856],[12285,14595],[17296,18416]]
```



### ES6



```JavaScript
(() => {
    'use strict';

    // amicablePairsUpTo :: Int -> [(Int, Int)]
    const amicablePairsUpTo = n => {
        const sigma = compose(sum, properDivisors);
        return enumFromTo(1)(n).flatMap(x => {
            const y = sigma(x);
            return x < y && x === sigma(y) ? ([
                [x, y]
            ]) : [];
        });
    };

    // properDivisors :: Int -> [Int]
    const properDivisors = n => {
        const
            rRoot = Math.sqrt(n),
            intRoot = Math.floor(rRoot),
            lows = enumFromTo(1)(intRoot)
            .filter(x => 0 === (n % x));
        return lows.concat(lows.map(x => n / x)
            .reverse()
            .slice((rRoot === intRoot) | 0, -1));
    };


    // TEST -----------------------------------------------

    // main :: IO ()
    const main = () =>
        console.log(unlines(
            amicablePairsUpTo(20000).map(JSON.stringify)
        ));


    // GENERIC FUNCTIONS ----------------------------------

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (...fs) =>
        x => fs.reduceRight((a, f) => f(a), x);


    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = m => n =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);


    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);


    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');


    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
[220,284]
[1184,1210]
[2620,2924]
[5020,5564]
[6232,6368]
[10744,10856]
[12285,14595]
[17296,18416]
```



## jq


```jq
# unordered
def proper_divisors:
  . as $n
  | if $n > 1 then 1,
      (sqrt|floor as $s
      | range(2; $s+1) as $i
      | if ($n % $i) == 0 then $i,
           (if $i * $i == $n then empty else ($n / $i) end)
	else empty
	end)
    else empty
    end;

def addup(stream): reduce stream as $i (0; . + $i);

def task(n):
  (reduce range(0; n+1) as $n
    ( [];  . + [$n | addup(proper_divisors)] )) as $listing
  | range(1;n+1) as $j
  | range(1;$j) as $k
  | if $listing[$j] == $k and $listing[$k] == $j
    then "\($k) and \($j) are amicable"
    else empty
    end ;

task(20000)
```

{{out}}

```sh
$ jq -c -n -f amicable_pairs.jq
220 and 284 are amicable
1184 and 1210 are amicable
2620 and 2924 are amicable
5020 and 5564 are amicable
6232 and 6368 are amicable
10744 and 10856 are amicable
12285 and 14595 are amicable
17296 and 18416 are amicable
```



## Julia

Given <code>factor</code>, it is not necessary to calculate the individual divisors to compute their sum.  See [[Abundant,_deficient_and_perfect_number_classifications#Julia|Abundant, deficient and perfect number classifications]] for the details.

'''Functions'''

```Julia

function pcontrib(p::Int64, a::Int64)
    n = one(p)
    pcon = one(p)
    for i in 1:a
        n *= p
        pcon += n
    end
    return pcon
end

function divisorsum(n::Int64)
    dsum = one(n)
    for (p, a) in factor(n)
        dsum *= pcontrib(p, a)
    end
    dsum -= n
end

```

<code>pcontrib</code> is a good candidate for [[Memoization]] should the performance of <code>divisorsum</code> become an issue.

'''Main'''

It is safe to exclude primes from consideration; their proper divisor sum is always 1.  Also, this code uses a minor trick to ensure that none of the numbers identified are above the limit.  All numbers in the range are checked for an amicable partner, but the pair is cataloged only when the greater member is reached.

```Julia


using Primes

const L = 2*10^4
acnt = 0

println("Amicable pairs not greater than ", L)

for i in 2:L
    !isprime(i) || continue
    j = divisorsum(i)
    j < i && divisorsum(j) == i || continue
    acnt += 1
    println(@sprintf("%4d", acnt), " => ", j, ", ", i)
end

```


{{out}}

```txt

Amicable pairs not greater than 20000
   1 => 220, 284
   2 => 1184, 1210
   3 => 2620, 2924
   4 => 5020, 5564
   5 => 6232, 6368
   6 => 10744, 10856
   7 => 12285, 14595
   8 => 17296, 18416

```



## K


```k

  propdivs:{1+&0=x!'1+!x%2}
  (8,2)#v@&{(x=+/propdivs[a])&~x=a:+/propdivs[x]}' v:1+!20000
(220 284
 1184 1210
 2620 2924
 5020 5564
 6232 6368
 10744 10856
 12285 14595
 17296 18416)

```



## Kotlin


```scala
// version 1.1

fun sumProperDivisors(n: Int): Int {
    if (n < 2) return 0
    return (1..n / 2).filter{ (n % it) == 0 }.sum()
}

fun main(args: Array<String>) {
    val sum = IntArray(20000, { sumProperDivisors(it) } )
    println("The pairs of amicable numbers below 20,000 are:\n")
    for(n in 2..19998) {
        val m = sum[n]
        if (m > n && m < 20000 && n == sum[m]) {
            println(n.toString().padStart(5) + " and " + m.toString().padStart(5))
        }
    }
}
```


{{out}}

```txt

The pairs of amicable numbers below 20,000 are:

  220 and   284
 1184 and  1210
 2620 and  2924
 5020 and  5564
 6232 and  6368
10744 and 10856
12285 and 14595
17296 and 18416

```



## Lua

0.02 of a second in 16 lines of code.
The vital trick is to just set m to the sum of n's proper divisors each time.  That way you only have to test the reverse, dividing your run time by half the loop limit (ie. 10,000)!

```lua
function sumDivs (n)
    local sum = 1
    for d = 2, math.sqrt(n) do
        if n % d == 0 then
            sum = sum + d
            sum = sum + n / d
        end
    end
    return sum
end

for n = 2, 20000 do
    m = sumDivs(n)
    if m > n then
        if sumDivs(m) == n then print(n, m) end
    end
end
```


{{out}}
```txt

220	284
1184	1210
2620	2924
5020	5564
6232	6368
10744	10856
12285	14595
17296	18416

```



## Maple


{{output?}}


```Maple

with(NumberTheory):
pairs:=[];
for i from 1 to 20000 do
	for j from i+1 to 20000 do
		sum1:=SumOfDivisors(j)-j;
		sum2:=SumOfDivisors(i)-i;
		if sum1=i and sum2=j and i<>j then
			pairs:=[op(pairs),[i,j]];
			printf("%a", pairs);
		end if;
	end do;
end do;
pairs;

```



## Matlab


```Matlab

function amicable
    tic
    N=2:1:20000; aN=[];
    N(isprime(N))=[]; %erase prime numbers
    I=1;
    a=N(1); b=sum(pd(a));
    while length(N)>1
        if a==b %erase perfect numbers;
            N(N==a)=[]; a=N(1); b=sum(pd(a));
        elseif b<a %the first member of an amicable pair is abundant not defective
            N(N==a)=[]; a=N(1); b=sum(pd(a));
        elseif ~ismember(b,N) %the other member was previously erased
            N(N==a)=[]; a=N(1); b=sum(pd(a));
        else
            c=sum(pd(b));
            if a==c
                aN(I,:)=[I a b]; I=I+1;
                N(N==b)=[];
            else
                if ~ismember(c,N) %the other member was previously erased
                    N(N==b)=[];
                end
            end
            N(N==a)=[]; a=N(1); b=sum(pd(a));
            clear c
        end
    end
    disp(array2table(aN,'Variablenames',{'N','Amicable1','Amicable2'}))
    toc
end

function D=pd(x)
    K=1:ceil(x/2);
    D=K(~(rem(x, K)));
end

```

{{out}}

```txt

    N    Amicable1    Amicable2
    _    _________    _________

    1      220          284
    2     1184         1210
    3     2620         2924
    4     5020         5564
    5     6232         6368
    6    10744        10856
    7    12285        14595
    8    17296        18416

Elapsed time is 8.958720 seconds.

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
amicableQ[n_] :=
 Module[{sum = Total[Most@Divisors@n]},
  sum != n && n == Total[Most@Divisors@sum]]

Grid@Partition[Cases[Range[4, 20000], _?(amicableQ@# &)], 2]
```


{{out}}
```txt

220	284
1184	1210
2620	2924
5020	5564
6232	6368
10744	10856
12285	14595
17296	18416

```


## Nim

Being a novice, I submitted my code to the Nim community for review and received much feedback and advice.
They were instrumental in fine-tuning this code for style and readability, I can't thank them enough.

```Nim

from math import sqrt

const N = 524_000_000.int32

proc sumProperDivisors(someNum: int32, chk4less: bool): int32 =
    result = 1
    let maxPD = sqrt(someNum.float).int32
    let offset = someNum mod 2
    for divNum in countup(2 + offset, maxPD, 1 + offset):
        if someNum mod divNum == 0:
            result += divNum + someNum div divNum
            if chk4less and result >= someNum:
                return 0

for n in countdown(N, 2):
    let m = sumProperDivisors(n, true)
    if m != 0 and n == sumProperDivisors(m, false):
        echo $n, " ", $m

```

{{out}}

```txt

523679536 509379344
511419856 491373104
514823985 475838415
...... ......
..... .....
18416 17296
14595 12285
10856 10744
6368 6232
5564 5020
2924 2620
1210 1184
284 220

```

Total number of pairs is 442, on my machine the code takes ~389 minutes to run.

Here's a second version that uses a large amount of memory but runs in 2m32seconds.
Again, thanks to the Nim community

```Nim

from math import sqrt

const N = 524_000_000.int32
var x = newSeq[int32](N+1)

for i in 2..sqrt(N.float).int32:
  var p = i*i
  x[p] += i
  var j = i + i
  while (p += i; p <= N):
    j.inc
    x[p] += j

for m in 4..N:
  let n = x[m] + 1
  if n < m and n != 0 and m == x[n] + 1:
      echo n, " ", m

```

{{out}}

```txt

220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
..... .....
...... ......
426191535 514780497
475838415 514823985
509379344 523679536

```


=={{header|Oberon-2}}==

```Oberon2

MODULE AmicablePairs;
IMPORT
  Out;
CONST
  max = 20000;

VAR
  i,j: INTEGER;
  pd: ARRAY max + 1 OF LONGINT;

PROCEDURE ProperDivisorsSum(n: LONGINT): LONGINT;
VAR
   i,sum: LONGINT;
BEGIN
  sum := 0;
  IF n > 1 THEN
    INC(sum,1);i := 2;
    WHILE (i < n) DO
      IF (n MOD i) = 0 THEN INC(sum,i) END;
      INC(i)
    END
  END;
  RETURN sum
END ProperDivisorsSum;

BEGIN
  FOR i := 0 TO max DO
    pd[i] := ProperDivisorsSum(i)
  END;

  FOR i := 2 TO max DO
    FOR j := i + 1 TO max DO
      IF (pd[i] = j) & (pd[j] = i) THEN
         Out.Char('[');Out.Int(i,0);Out.Char(',');Out.Int(j,0);Out.Char("]");Out.Ln
      END
    END
  END
END AmicablePairs.

```

{{Out}}

```txt

[220,284]
[1184,1210]
[2620,2924]
[5020,5564]
[6232,6368]
[10744,10856]
[12285,14595]
[17296,18416]

```



## Oforth


Using properDivs implementation tasks without optimization (calculating proper divisors sum without returning a list for instance) :


```Oforth
import: mapping

Integer method: properDivs -- []
   #[ self swap mod 0 == ] self 2 / seq filter ;

: amicables
| i j |
   Array new
   20000 loop: i [
      i properDivs sum dup ->j i <= if continue then
      j properDivs sum i <> if continue then
      [ i, j ] over add
      ]
;
```


{{out}}

```txt

amicables .
[[220, 284], [1184, 1210], [2620, 2924], [5020, 5564], [6232, 6368], [10744, 10856], [12285, 14595], [17296, 18416]]

```



## PARI/GP


```parigp
for(x=1,20000,my(y=sigma(x)-x); if(y>x && x == sigma(y)-y,print(x" "y)))
```

{{out}}

```txt
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
```



## Pascal


### Direct approach

{{works with|Turbo Pascal}}{{works with| Free Pascal}}
This version mutates the Sieve of Eratoshenes from striking out factors into summing factors. The Pascal source compiles with Turbo Pascal (7, patched to avoid the zero divide problem for cpu speeds better than ~150MHz) except that the array limit is too large: 15,000 works but does not reach 20,000. The Free Pascal compiler however can handle an array of 20,000 elements. Because the sum of factors of N can exceed N an ad-hoc SumF procedure is provided, thus the search could continue past the table limit, but at a cost in calculation time.

Output is
 Chasing Chains of Sums of Factors of Numbers.
 Perfect!! 6,
 Perfect!! 28,
 Amicable! 220,284,
 Perfect!! 496,
 Amicable! 1184,1210,
 Amicable! 2620,2924,
 Amicable! 5020,5564,
 Amicable! 6232,6368,
 Perfect!! 8128,
 Amicable! 10744,10856,
 Amicable! 12285,14595,
 Sociable: 12496,14288,15472,14536,14264,
 Sociable: 14316,19116,31704,47616,83328,177792,295488,629072,589786,294896,358336,418904,366556,274924,275444,243760,376736,381028,285778,152990,122410,97946,48976,45946,22976,22744,19916,17716,
 Amicable! 17296,18416,

Source file:
```pascal

 Program SumOfFactors; uses crt; {Perpetrated by R.N.McLean, December MCMXCV}
//{$DEFINE ShowOverflow}
{$IFDEF FPC}
  {$MODE DELPHI}//tested with lots = 524*1000*1000 takes 75 secs generating KnownSum
{$ENDIF}
  var outf: text;
  const Limit = 2147483647;
  const lots = 20000;       {This should be much bigger, but problems apply.}
  var KnownSum: array[1..lots] of longint;
  Function SumF(N: Longint): Longint;
   var f,f2,s,ulp: longint;
   Begin
    if n <= lots then SumF:=KnownSum[N] {Hurrah!}
     else
      begin      {This is really crude...}
       s:=1;     {1 is always a factor, but N is not.}
       f:=2;
       f2:=f*f;
       while f2 < N do
        begin
         if N mod f = 0 then
          begin  {We have a divisor, and its friend.}
           ulp:=f + (N div f);
           if s > Limit - ulp then begin SumF:=-666; exit; end;
           s:=s + ulp;
          end;
         f:=f + 1;
         f2:=f*f;
        end;
        if f2 = N then {A perfect square gets its factor in once only.}
         if s <= Limit - f then s:=s + f
          else begin SumF:=-667; exit; end;
       SumF:=s;
      end;
   End;
  var i,j,l,sf,fs: LongInt;
  const enuff = 666; {Only so much sociability.}
  var trail: array[0..enuff] of longint;
  BEGIN
   ClrScr;
   WriteLn('Chasing Chains of Sums of Factors of Numbers.');
   for i:=1 to lots do KnownSum[i]:=1; {Sigh. KnownSum:=1;}

{start summing every divisor }
   for i:=2 to lots do
    begin
     j:=i + i;
     While j <= lots do    {Sigh. For j:=i + i:Lots:i do KnownSum[j]:=KnownSum[j] + i;}
     begin
       KnownSum[j]:=KnownSum[j] + i;
       j:=j + i;
      end;
    end;

 {Enough preparation.}
   Assign(outf,'Factors.txt'); ReWrite(Outf);
   WriteLn(Outf,'Chasing Chains of Sums of Factors of Numbers.');

   for i:=2 to lots do    {Search.}
    begin
     l:=0;
     sf:=SumF(i);
     while (sf > i) and (l < enuff) do
      begin
       l:=l + 1;
       trail[l]:=sf;
       sf:=SumF(sf);
      end;
     if l >= enuff then writeln('Rope ran out! ',i);
{$IFDEF ShowOverflow}
     if sf < 0 then writeln('Overflow with ',i);
{$ENDIF}
     if i = sf then      {A loop?}
      begin              {Yes. Reveal its members.}
       trail[0]:=i;      {The first.}
       if l = 0 then write('Perfect!! ')
        else if l = 1 then write('Amicable! ')
         else write('Sociable: ');
       for j:=0 to l do Write(Trail[j],',');
       WriteLn;
       if l = 0 then write(outf,'Perfect!! ')
        else if l = 1 then write(outf,'Amicable! ')
         else write(outf,'Sociable: ');
       for j:=0 to l do write(outf,Trail[j],',');
       WriteLn(outf);
      end;
    end;
   Close (outf);
  END.
```



### More expansive.

a "normal" Version. Nearly fast as perl using nTheory.

```pascal
program AmicablePairs;
{$IFDEF FPC}
   {$MODE DELPHI}
   {$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  sysutils;
const
  MAX = 20000;
//MAX = 20*1000*1000;
type
  tValue = LongWord;
  tpValue = ^tValue;
  tPower = array[0..31] of tValue;
  tIndex = record
             idxI,
             idxS : Uint64;
           end;

var
  Indices      : array[0..511] of tIndex;
  //primes up to 65536 enough until 2^32
  primes       : array[0..6542] of tValue;

procedure InitPrimes;
// sieve of erathosthenes without multiples of 2
type
  tSieve = array[0..(65536-1) div 2] of char;
var
  ESieve : ^tSieve;
  idx,i,j,p : LongINt;
Begin
  new(ESieve);
  fillchar(ESieve^[0],SizeOF(tSieve),#1);
  primes[0] := 2;
  idx := 1;

  //sieving
  j := 1;
  p := 2*j+1;
  repeat
    if Esieve^[j] = #1 then
    begin
      i := (2*j+2)*j;// i := (sqr(p) -1) div 2;
      if i > High(tSieve) then
        BREAK;
      repeat
        ESIeve^[i] := #0;
        inc(i,p);
      until i > High(tSieve);
    end;
    inc(j);
    inc(p,2);
  until j >High(tSieve);

  //collecting
  For i := 1 to High(tSieve) do
    IF Esieve^[i] = #1 then
    Begin
      primes[idx] := 2*i+1;
      inc(idx);
      IF idx>High(primes) then
        BREAK;
    end;
  dispose(Esieve);
end;

procedure Su_append(n,factor:tValue;var su:string);
var
  q,p : tValue;
begin
  p := 0;
  repeat
    q := n div factor;
    IF q*factor<>n then
      Break;
    inc(p);
    n := q;
  until false;
  IF p > 0 then
    IF p= 1 then
      su:= su+IntToStr(factor)+'*'
    else
      su:= su+IntToStr(factor)+'^'+IntToStr(p)+'*';
end;

procedure ProperDivs(n: Uint64);
//output of prime factorization
var
  su : string;
  primNo : tValue;
  p:tValue;

begin
  str(n:8,su);
  su:= su +' [';
  primNo := 0;
  p := primes[0];
  repeat
    Su_Append(n,p,su);
    inc(primNo);
    p := primes[primNo];
  until (p=0) OR (p*p >= n);
  p := n;
  Su_Append(n,p,su);
  su[length(su)] := ']';
  writeln(su);
end;

procedure AmPairOutput(cnt:tValue);
var
  i : tValue;
  r_max,r_min,r : double;
begin
  r_max := 1.0;
  r_min := 16.0;
  For i := 0 to cnt-1 do
    with Indices[i] do
    begin
      r := IdxS/IDxI;
      writeln(i+1:4,IdxI:16,IDxS:16,' ratio ',r:10:7);
      IF r < 1 then
      begin
        writeln(i);
        readln;
        halt;
      end;
      if r_max < r then
        r_max := r
      else
        if r_min > r then
          r_min := r;
    IF cnt < 20 then
      begin
        ProperDivs(IdxI);
        ProperDivs(IdxS);
      end;
    end;
  writeln(' min ratio ',r_min:12:10);  writeln(' max ratio ',r_max:12:10);
end;

procedure SumOFProperDiv(n: tValue;var SumOfProperDivs:tValue);
// calculated by prime factorization
var
  i,q, primNo, Prime,pot : tValue;
  SumOfDivs: tValue;
begin
  i := N;
  SumOfDivs := 1;
  primNo := 0;
  Prime := Primes[0];
  q := i DIV Prime;
  repeat
    if q*Prime = i then
    Begin
      pot := 1;
      repeat
        i := q;
        q := i div Prime;
        Pot := Pot * Prime+1;
      until q*Prime <> i;
      SumOfDivs := SumOfDivs * pot;
    end;
    Inc(primNo);
    Prime := Primes[primNo];
    q := i DIV Prime;

    {check if i already prime}
    if Prime > q then
    begin
      prime := i;
      q := 1;
    end;
  until i = 1;
  SumOfProperDivs := SumOfDivs - N;
end;

function Check:tValue;
const
  //going backwards
  DIV23 : array[0..5] of byte =
           //== 5,4,3,2,1,0
               (1,0,0,0,1,0);

var
  i,s,k,n : tValue;
  idx : nativeInt;
begin
  n := 0;
  idx := 3;
  For i := 2 to MAX do
  begin
    //must be divisble by 2 or 3 ( n < High(tValue) < 1e14 )
    IF DIV23[idx] = 0 then
    begin
      SumOFProperDiv(i,s);
      //only 24.7...%
      IF s>i then
      Begin
        SumOFProperDiv(s,k);
        IF k = i then
        begin
          With indices[n] do
          begin
            idxI := i;
            idxS := s;
          end;
          inc(n);
        end;
      end;
    end;
    dec(idx);
    IF idx < 0 then
      idx := high(DIV23);
  end;
  result := n;
end;

var
  T2,T1: TDatetime;
  APcnt: tValue;
begin
  InitPrimes;
  T1:= time;
  APCnt:= Check;
  T2:= time;
  AmPairOutput(APCnt);
  writeln('Time to find amicable pairs ',FormatDateTime('HH:NN:SS.ZZZ' ,T2-T1));
  {$IFNDEF UNIX} readln;{$ENDIF}
end.
```

Output

```txt
   1             220             284 ratio  1.2909091
     220 [2^2*5*11*220]
     284 [2^2*284]
   2            1184            1210 ratio  1.0219595
    1184 [2^5*1184]
    1210 [2*5*11^2*1210]
   3            2620            2924 ratio  1.1160305
    2620 [2^2*5*2620]
    2924 [2^2*17*43*2924]
   4            5020            5564 ratio  1.1083665
    5020 [2^2*5*5020]
    5564 [2^2*13*5564]
   5            6232            6368 ratio  1.0218228
    6232 [2^3*19*41*6232]
    6368 [2^5*6368]
   6           10744           10856 ratio  1.0104244
   10744 [2^3*17*79*10744]
   10856 [2^3*23*59*10856]
   7           12285           14595 ratio  1.1880342
   12285 [3^3*5*7*13*12285]
   14595 [3*5*7*14595]
   8           17296           18416 ratio  1.0647549
   17296 [2^4*23*47*17296]
   18416 [2^4*18416]
```


### Alternative

about 25-times faster.
This will not output the amicable number unless both! numbers are under the given limit.

So there will be differences to "Table of n, a(n) for n=1..39374"  https://oeis.org/A002025/b002025.txt
Up to 524'000'000 the pairs found are only correct by number up to no. 437  460122410 and only 442 out of 455 are found, because some pairs exceed the limit.
The limits of the ratio between the numbers of the amicable pair up to 1E14 are, based on b002025.txt:

```txt

No.    lower            upper
31447  52326552030976  52326637800704 ratio  1.0000016
52326552030976 [2^8*563*6079*59723]
52326637800704 [2^8*797*1439*178223]

38336  92371445691525 154378742017851 ratio  1.6712821
 92371445691525 [3^2*5^2*7^2*11*13^2*23*29^2*233]
154378742017851 [3^2*13^2*53*337*5682671]

```



The distance check is being corrected, the lower number is now not limited.
The used method is not useful for very high limits.

n = p[1]^a[1]*p[2]^a[2]*...p[l]^a[l]

sum of divisors(n) = s(n) = (p[1]^(a[1]+1) -1) / (p[1] -1)  * ... * (p[l]^(a[l]+1) -1) / (p[l] -1) with

p[k]^(a[k]+1) -1) / (p[k] -1) = sum (i= [1..a[k]])(p[k]^i)

Using "Sieve of Erathosthenes"-style


```pascal
program AmicPair;
{find amicable pairs in a limited region 2..MAX
beware that >both< numbers must be smaller than MAX
there are 455 amicable pairs up to 524*1000*1000
correct up to
#437 460122410
}
//optimized for freepascal 2.6.4 32-Bit
{$IFDEF FPC}
   {$MODE DELPHI}
   {$OPTIMIZATION ON,peephole,cse,asmcse,regvar}
   {$CODEALIGN loop=1,proc=8}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  sysutils;

type
  tValue = LongWord;
  tpValue = ^tValue;
  tDivSum = array[0..0] of tValue;// evil, but dynamic arrays are slower
  tpDivSum = ^tDivSum;
  tPower = array[0..31] of tValue;
  tIndex = record
             idxI,
             idxS : tValue;
           end;
var
  power,
  PowerFac     : tPower;
  ds           : array of tValue;
  Indices      : array[0..511] of tIndex;
  DivSumField  : tpDivSum;
  MAX : tValue;

procedure Init;
var
  i : LongInt;
begin
  DivSumField[0]:= 0;
  For i := 1 to MAX do
    DivSumField[i]:= 1;
end;

procedure ProperDivs(n: tValue);
//Only for output, normally a factorication would do
var
  su,so : string;
  i,q : tValue;
begin
  su:= '1';
  so:= '';
  i := 2;
  while i*i <= n do
  begin
    q := n div i;
    IF q*i -n = 0 then
    begin
      su:= su+','+IntToStr(i);
      IF q <> i then
        so:= ','+IntToStr(q)+so;
    end;
    inc(i);
  end;
  writeln('  [',su+so,']');
end;

procedure AmPairOutput(cnt:tValue);
var
  i : tValue;
  r : double;
begin
  r := 1.0;
  For i := 0 to cnt-1 do
  with Indices[i] do
  begin
    writeln(i+1:4,IdxI:12,IDxS:12,' ratio ',IdxS/IDxI:10:7);
    if r < IdxS/IDxI then
      r := IdxS/IDxI;
      IF cnt < 20 then
      begin
        ProperDivs(IdxI);
        ProperDivs(IdxS);
      end;
  end;
  writeln(' max ratio ',r:10:4);
end;

function Check:tValue;
var
  i,s,n : tValue;
begin
  n := 0;
  For i := 1 to MAX do
  begin
    //s = sum of proper divs (I)  == sum of divs (I) - I
    s := DivSumField^[i];
    IF (s <=MAX) AND (s>i) AND (DivSumField^[s]= i)then
    begin
      With indices[n] do
      begin
        idxI := i;
        idxS := s;
      end;
      inc(n);
    end;
  end;
  result := n;
end;

Procedure CalcPotfactor(prim:tValue);
//PowerFac[k] = (prim^(k+1)-1)/(prim-1) == Sum (i=0..k) prim^i
var
  k: tValue;
  Pot,       //== prim^k
  PFac : Int64;
begin
  Pot := prim;
  PFac := 1;
  For k := 0 to High(PowerFac) do
  begin
    PFac := PFac+Pot;
    IF (POT > MAX) then
      BREAK;
    PowerFac[k] := PFac;
    Pot := Pot*prim;
  end;
end;

procedure InitPW(prim:tValue);
begin
  fillchar(power,SizeOf(power),#0);
  CalcPotfactor(prim);
end;

function NextPotCnt(p: tValue):tValue;
//return the first power <> 0
//power == n to base prim
var
  i : tValue;
begin
  result := 0;
  repeat
    i := power[result];
    Inc(i);
    IF i < p then
      BREAK
    else
    begin
      i := 0;
      power[result]  := 0;
      inc(result);
    end;
  until false;
  power[result] := i;
end;

procedure Sieve(prim: tValue);
var
  actNumber,idx : tValue;
begin
  //sieve with "small" primes
  while prim*prim <= MAX do
  begin
    InitPW(prim);
    Begin
      //actNumber = actual number = n*prim
      actNumber := prim;
      idx := prim;
      while actNumber <= MAX do
      begin
        dec(idx);
        IF idx > 0 then
          DivSumField^[actNumber] *= PowerFac[0]
        else
        Begin
          DivSumField^[actNumber] *= PowerFac[NextPotCnt(prim)+1];
          idx := Prim;
        end;
        inc(actNumber,prim);
      end;
    end;
    //next prime
    repeat
      inc(prim);
    until DivSumField^[prim]= 1;//(DivSumField[prim] = 1);
  end;

  //sieve with "big" primes, only one factor is possible
  while 2*prim <= MAX do
  begin
    InitPW(prim);
    Begin
      actNumber := prim;
      idx := PowerFac[0];
      while actNumber <= MAX do
      begin
        DivSumField^[actNumber] *= idx;
        inc(actNumber,prim);
      end;
    end;
    repeat
      inc(prim);
    until DivSumField^[prim]= 1;
  end;

  For idx := 2 to MAX do
    dec(DivSumField^[idx],idx);
end;

var
  T2,T1,T0: TDatetime;
  APcnt: tValue;
  i: NativeInt;
begin
  MAX := 20000;
  IF  ParamCount > 0 then
    MAX := StrToInt(ParamStr(1));
  setlength(ds,MAX);
  DivSumField := @ds[0];
  T0:= time;
  For i := 1 to 1 do
  Begin
    Init;
    Sieve(2);
  end;
  T1:= time;

  APCnt := Check;
  T2:= time;
  AmPairOutput(APCnt);
  writeln(APCnt,' amicable pairs til ',MAX);
  writeln('Time to calc sum of divs    ',FormatDateTime('HH:NN:SS.ZZZ' ,T1-T0));
  writeln('Time to find amicable pairs ',FormatDateTime('HH:NN:SS.ZZZ' ,T2-T1));
  setlength(ds,0);
  {$IFNDEF UNIX}
    readln;
  {$ENDIF}
end.
```

output

```txt

       220       284
  [1,2,4,5,10,11,20,22,44,55,110]
  [1,2,4,71,142]

      1184      1210
  [1,2,4,8,16,32,37,74,148,296,592]
  [1,2,5,10,11,22,55,110,121,242,605]

      2620      2924
  [1,2,4,5,10,20,131,262,524,655,1310]
  [1,2,4,17,34,43,68,86,172,731,1462]

      5020      5564
  [1,2,4,5,10,20,251,502,1004,1255,2510]
  [1,2,4,13,26,52,107,214,428,1391,2782]

      6232      6368
  [1,2,4,8,19,38,41,76,82,152,164,328,779,1558,3116]
  [1,2,4,8,16,32,199,398,796,1592,3184]

     10744     10856
  [1,2,4,8,17,34,68,79,136,158,316,632,1343,2686,5372]
  [1,2,4,8,23,46,59,92,118,184,236,472,1357,2714,5428]

     12285     14595
  [1,3,5,7,9,13,15,21,27,35,39,45,63,65,91,105,117,135,189,195,273,315,351,455,585,819,945,1365,1755,2457,4095]
  [1,3,5,7,15,21,35,105,139,417,695,973,2085,2919,4865]

     17296     18416
  [1,2,4,8,16,23,46,47,92,94,184,188,368,376,752,1081,2162,4324,8648]
  [1,2,4,8,16,1151,2302,4604,9208]

8 amicable numbers up to 20000
00:00:00.000

.... Test with 524*1000*1000 Linux32, FPC 3.0.1, i4330 3.5 Ghz //Win32 swaps first to allocate 2 GB )
 440   475838415   514823985 ratio  1.0819303
 441   491373104   511419856 ratio  1.0407974
 442   509379344   523679536 ratio  1.0280738
 max ratio     1.3537
442 amicable pairs til 524000000
Time to calc sum of divs    00:00:12.601
Time to find amicable pairs 00:00:02.557

```



## Perl

Not particularly clever, but instant for this example, and does up to 20 million in 11 seconds.
{{libheader|ntheory}}

```perl
use ntheory qw/divisor_sum/;
for my $x (1..20000) {
  my $y = divisor_sum($x)-$x;
  say "$x $y" if $y > $x && $x == divisor_sum($y)-$y;
}
```

{{out}}

```txt
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
```



## Perl 6

{{Works with|Rakudo|2019.03.1}}

```perl6
sub propdivsum (\x) {
    my @l = 1 if x > 1;
    (2 .. x.sqrt.floor).map: -> \d {
        unless x % d { @l.push: d; my \y = x div d; @l.push: y if y != d }
    }
    sum @l
}

(1..20000).race.map: -> $i {
    my $j = propdivsum($i);
    say "$i $j" if $j > $i and $i == propdivsum($j);
}
```

{{out}}

```txt
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
```



## Phix


```Phix
integer n
for m=1 to 20000 do
    n = sum(factors(m,-1))
    if m<n and m=sum(factors(n,-1)) then ?{m,n} end if
end for
```

{{out}}

```txt

{220,284}
{1184,1210}
{2620,2924}
{5020,5564}
{6232,6368}
{10744,10856}
{12285,14595}
{17296,18416}

```



## PicoLisp


```PicoLisp
(de accud (Var Key)
   (if (assoc Key (val Var))
      (con @ (inc (cdr @)))
      (push Var (cons Key 1)) )
   Key )
(de **sum (L)
   (let S 1
      (for I (cdr L)
         (inc 'S (** (car L) I)) )
      S ) )
(de factor-sum (N)
   (if (=1 N)
      0
      (let
         (R NIL
            D 2
            L (1 2 2 . (4 2 4 2 4 6 2 6 .))
            M (sqrt N)
            N1 N
            S 1 )
         (while (>= M D)
            (if (=0 (% N1 D))
               (setq M
                  (sqrt (setq N1 (/ N1 (accud 'R D)))) )
               (inc 'D (pop 'L)) ) )
         (accud 'R N1)
         (for I R
            (setq S (* S (**sum I))) )
         (- S N) ) ) )
(bench
   (for I 20000
      (let X (factor-sum I)
         (and
            (< I X)
            (= I (factor-sum X))
            (println I X) ) ) ) )
```

{{output}}

```txt

220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
0.101 sec

```



## PL/I

{{trans|REXX}}

```pli
*process source xref;
 ami: Proc Options(main);
 p9a=time();
 Dcl (p9a,p9b,p9c) Pic'(9)9';
 Dcl sumpd(20000) Bin Fixed(31);
 Dcl pd(300) Bin Fixed(31);
 Dcl npd     Bin Fixed(31);
 Dcl (x,y)   Bin Fixed(31);

 Do x=1 To 20000;
   Call proper_divisors(x,pd,npd);
   sumpd(x)=sum(pd,npd);
   End;
 p9b=time();
 Put Edit('sum(pd) computed in',(p9b-p9a)/1000,' seconds elapsed')
         (Skip,col(7),a,f(6,3),a);

 Do x=1 To 20000;
   Do y=x+1 To 20000;
     If y=sumpd(x) &
        x=sumpd(y) Then
       Put Edit(x,y,' found after ',elapsed(),' seconds')
               (Skip,2(f(6)),a,f(6,3),a);
     End;
   End;
 Put Edit(elapsed(),' seconds total search time')(Skip,f(6,3),a);

 proper_divisors: Proc(n,pd,npd);
 Dcl (n,pd(300),npd) Bin Fixed(31);
 Dcl (d,delta)       Bin Fixed(31);
 npd=0;
 If n>1 Then Do;
   If mod(n,2)=1 Then  /* odd number  */
     delta=2;
   Else                /* even number */
     delta=1;
   Do d=1 To n/2 By delta;
     If mod(n,d)=0 Then Do;
       npd+=1;
       pd(npd)=d;
       End;
     End;
   End;
 End;

 sum: Proc(pd,npd) Returns(Bin Fixed(31));
 Dcl (pd(300),npd) Bin Fixed(31);
 Dcl sum Bin Fixed(31) Init(0);
 Dcl i   Bin Fixed(31);
 Do i=1 To npd;
   sum+=pd(i);
   End;
 Return(sum);
 End;

 elapsed: Proc Returns(Dec Fixed(6,3));
 p9c=time();
 Return((p9c-p9b)/1000);
 End;
 End;
```

{{out}}

```txt
      sum(pd) computed in 0.510 seconds elapsed
   220   284 found after  0.010 seconds
  1184  1210 found after  0.060 seconds
  2620  2924 found after  0.110 seconds
  5020  5564 found after  0.210 seconds
  6232  6368 found after  0.260 seconds
 10744 10856 found after  2.110 seconds
 12285 14595 found after  2.150 seconds
 17296 18416 found after  2.240 seconds
 2.250 seconds total search time
```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

function Get-ProperDivisorSum ( [int]$N )
    {
    $Sum = 1
    If ( $N -gt 3 )
        {
        $SqrtN = [math]::Sqrt( $N )
        ForEach ( $Divisor1 in 2..$SqrtN )
            {
            $Divisor2 = $N / $Divisor1
            If ( $Divisor2 -is [int] ) { $Sum += $Divisor1 + $Divisor2 }
            }
        If ( $SqrtN -is [int] ) { $Sum -= $SqrtN }
        }
    return $Sum
    }

function Get-AmicablePairs ( $N = 300 )
    {
    ForEach ( $X in 1..$N )
        {
        $Sum = Get-ProperDivisorSum $X
        If ( $Sum -gt $X -and $X -eq ( Get-ProperDivisorSum $Sum ) )
            {
            "$X, $Sum"
            }
        }
    }

Get-AmicablePairs 20000

```

{{out}}

```txt

220, 284
1184, 1210
2620, 2924
5020, 5564
6232, 6368
10744, 10856
12285, 14595
17296, 18416

```



## Prolog


{{works with|SWI-Prolog 7}}

With some guidance from other solutions here:


```prolog
divisor(N, Divisor) :-
    UpperBound is round(sqrt(N)),
    between(1, UpperBound, D),
    0 is N mod D,
    (
        Divisor = D
    ;
        LargerDivisor is N/D,
        LargerDivisor =\= D,
        Divisor = LargerDivisor
    ).

proper_divisor(N, D) :-
    divisor(N, D),
    D =\= N.

assoc_num_divsSum_in_range(Low, High, Assoc) :-
    findall( Num-DivSum,
             ( between(Low, High, Num),
               aggregate_all( sum(D),
                              proper_divisor(Num, D),
                              DivSum )),
             Pairs ),
    list_to_assoc(Pairs, Assoc).

get_amicable_pair(Assoc, M-N) :-
    gen_assoc(M, Assoc, N),
    M < N,
    get_assoc(N, Assoc, M).

amicable_pairs_under_20000(Pairs) :-
    assoc_num_divsSum_in_range(1,20000, Assoc),
    findall(P, get_amicable_pair(Assoc, P), Pairs).
```


Output:


```prolog
?- amicable_pairs_under_20000(R).
R = [220-284, 1184-1210, 2620-2924, 5020-5564, 6232-6368, 10744-10856, 12285-14595, 17296-18416].
```



## PureBasic


```PureBasic

EnableExplicit

Procedure.i SumProperDivisors(Number)
  If Number < 2 : ProcedureReturn 0 : EndIf
  Protected i, sum = 0
  For i = 1 To Number / 2
    If Number % i = 0
      sum + i
    EndIf
  Next
  ProcedureReturn sum
EndProcedure

Define n, f
Define Dim sum(19999)

If OpenConsole()
  For n = 1 To 19999
    sum(n) = SumProperDivisors(n)
  Next
  PrintN("The pairs of amicable numbers below 20,000 are : ")
  PrintN("")
  For n = 1 To 19998
    f = sum(n)
    If f <= n Or f < 1 Or f > 19999 : Continue : EndIf
    If f = sum(n) And n = sum(f)
      PrintN(RSet(Str(n),5) + " and " + RSet(Str(sum(n)), 5))
    EndIf
  Next
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out}}

```txt

The pairs of amicable numbers below 20,000 are :

  220 and   284
 1184 and  1210
 2620 and  2924
 5020 and  5564
 6232 and  6368
10744 and 10856
12285 and 14595
17296 and 18416

```



## Python

Importing [[Proper_divisors#Python:_From_prime_factors|Proper divisors from prime factors]]:

```python
from proper_divisors import proper_divs

def amicable(rangemax=20000):
    n2divsum = {n: sum(proper_divs(n)) for n in range(1, rangemax + 1)}
    for num, divsum in n2divsum.items():
        if num < divsum and divsum <= rangemax and n2divsum[divsum] == num:
            yield num, divsum

if __name__ == '__main__':
    for num, divsum in amicable():
        print('Amicable pair: %i and %i With proper divisors:\n    %r\n    %r'
              % (num, divsum, sorted(proper_divs(num)), sorted(proper_divs(divsum))))
```


{{out}}

```txt
Amicable pair: 220 and 284 With proper divisors:
    [1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110]
    [1, 2, 4, 71, 142]
Amicable pair: 1184 and 1210 With proper divisors:
    [1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592]
    [1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605]
Amicable pair: 2620 and 2924 With proper divisors:
    [1, 2, 4, 5, 10, 20, 131, 262, 524, 655, 1310]
    [1, 2, 4, 17, 34, 43, 68, 86, 172, 731, 1462]
Amicable pair: 5020 and 5564 With proper divisors:
    [1, 2, 4, 5, 10, 20, 251, 502, 1004, 1255, 2510]
    [1, 2, 4, 13, 26, 52, 107, 214, 428, 1391, 2782]
Amicable pair: 6232 and 6368 With proper divisors:
    [1, 2, 4, 8, 19, 38, 41, 76, 82, 152, 164, 328, 779, 1558, 3116]
    [1, 2, 4, 8, 16, 32, 199, 398, 796, 1592, 3184]
Amicable pair: 10744 and 10856 With proper divisors:
    [1, 2, 4, 8, 17, 34, 68, 79, 136, 158, 316, 632, 1343, 2686, 5372]
    [1, 2, 4, 8, 23, 46, 59, 92, 118, 184, 236, 472, 1357, 2714, 5428]
Amicable pair: 12285 and 14595 With proper divisors:
    [1, 3, 5, 7, 9, 13, 15, 21, 27, 35, 39, 45, 63, 65, 91, 105, 117, 135, 189, 195, 273, 315, 351, 455, 585, 819, 945, 1365, 1755, 2457, 4095]
    [1, 3, 5, 7, 15, 21, 35, 105, 139, 417, 695, 973, 2085, 2919, 4865]
Amicable pair: 17296 and 18416 With proper divisors:
    [1, 2, 4, 8, 16, 23, 46, 47, 92, 94, 184, 188, 368, 376, 752, 1081, 2162, 4324, 8648]
    [1, 2, 4, 8, 16, 1151, 2302, 4604, 9208]
```



Or, supplying our own '''properDivisors''' function, and defining the harvest in terms of a generic '''concatMap''':


```python
'''Amicable pairs'''

from itertools import chain
from math import sqrt


# amicablePairsUpTo :: Int -> [(Int, Int)]
def amicablePairsUpTo(n):
    '''List of all amicable pairs
       of integers below n.
    '''
    sigma = compose(sum)(properDivisors)

    def amicable(x):
        y = sigma(x)
        return [(x, y)] if (x < y and x == sigma(y)) else []

    return concatMap(amicable)(
        enumFromTo(1)(n)
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Amicable pairs of integers up to 20000'''

    for x in amicablePairsUpTo(20000):
        print(x)


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list or string over which a function f
       has been mapped.
       The list monad can be derived by using an (a -> [b])
       function which wraps its output in a list (using an
       empty list to represent computational failure).
    '''
    return lambda xs: (''.join if isinstance(xs, str) else list)(
        chain.from_iterable(map(f, xs))
    )


# enumFromTo :: Int -> Int -> [Int]
def enumFromTo(m):
    '''Enumeration of integer values [m..n]'''
    def go(n):
        return list(range(m, 1 + n))
    return lambda n: go(n)


# properDivisors :: Int -> [Int]
def properDivisors(n):
    '''Positive divisors of n, excluding n itself'''
    root_ = sqrt(n)
    intRoot = int(root_)
    blnSqr = root_ == intRoot
    lows = [x for x in range(1, 1 + intRoot) if 0 == n % x]
    return lows + [
        n // x for x in reversed(
            lows[1:-1] if blnSqr else lows[1:]
        )
    ]


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
(220, 284)
(1184, 1210)
(2620, 2924)
(5020, 5564)
(6232, 6368)
(10744, 10856)
(12285, 14595)
(17296, 18416)
```



## R



```R

divisors <- function (n) {
  Filter( function (m) 0 == n %% m, 1:(n/2) )
}

table = sapply(1:19999, function (n) sum(divisors(n)) )

for (n in 1:19999) {
  m = table[n]
  if ((m > n) && (m < 20000) && (n == table[m]))
    cat(n, " ", m, "\n")
}

```


{{out}}

```txt

220   284
1184   1210
2620   2924
5020   5564
6232   6368
10744   10856
12285   14595
17296   18416

```



## Racket

With [[Proper_divisors#Racket]] in place:

```racket
#lang racket
(require "proper-divisors.rkt")
(define SCOPE 20000)

(define P
  (let ((P-v (vector)))
    (λ (n)
      (set! P-v (fold-divisors P-v n 0 +))
      (vector-ref P-v n))))

;; returns #f if not an amicable number, amicable pairing otherwise
(define (amicable? n)
  (define m (P n))
  (define m-sod (P m))
  (and (= m-sod n)
       (< m n) ; each pair exactly once, also eliminates perfect numbers
       m))

(void (amicable? SCOPE)) ; prime the memoisation

(for* ((n (in-range 1 (add1 SCOPE)))
       (m (in-value (amicable? n)))
       #:when m)
  (printf #<<EOS
amicable pair: ~a, ~a
  ~a: divisors: ~a
  ~a: divisors: ~a


EOS
          n m n (proper-divisors n)  m (proper-divisors m)))

```


{{out}}

```txt
amicable pair: 284, 220
  284: divisors: (1 2 4 71 142)
  220: divisors: (1 2 4 5 10 11 20 22 44 55 110)

amicable pair: 1210, 1184
  1210: divisors: (1 2 5 10 11 22 55 110 121 242 605)
  1184: divisors: (1 2 4 8 16 32 37 74 148 296 592)

amicable pair: 2924, 2620
  2924: divisors: (1 2 4 17 34 43 68 86 172 731 1462)
  2620: divisors: (1 2 4 5 10 20 131 262 524 655 1310)

amicable pair: 5564, 5020
  5564: divisors: (1 2 4 13 26 52 107 214 428 1391 2782)
  5020: divisors: (1 2 4 5 10 20 251 502 1004 1255 2510)

amicable pair: 6368, 6232
  6368: divisors: (1 2 4 8 16 32 199 398 796 1592 3184)
  6232: divisors: (1 2 4 8 19 38 41 76 82 152 164 328 779 1558 3116)

amicable pair: 10856, 10744
  10856: divisors: (1 2 4 8 23 46 59 92 118 184 236 472 1357 2714 5428)
  10744: divisors: (1 2 4 8 17 34 68 79 136 158 316 632 1343 2686 5372)

amicable pair: 14595, 12285
  14595: divisors: (1 3 5 7 15 21 35 105 139 417 695 973 2085 2919 4865)
  12285: divisors: (1 3 5 7 9 13 15 21 27 35 39 45 63 65 91 105 117 135 189 195 273 315 351 455 585 819 945 1365 1755 2457 4095)

amicable pair: 18416, 17296
  18416: divisors: (1 2 4 8 16 1151 2302 4604 9208)
  17296: divisors: (1 2 4 8 16 23 46 47 92 94 184 188 368 376 752 1081 2162 4324 8648)


```



## REXX

===version 1, with factoring===

```rexx
Call time 'R'
Do x=1 To 20000
  pd=proper_divisors(x)
  sumpd.x=sum(pd)
  End
Say 'sum(pd) computed in' time('E') 'seconds'
Call time 'R'
Do x=1 To 20000
  /* If x//1000=0 Then Say x time() */
  Do y=x+1 To 20000
    If y=sumpd.x &,
       x=sumpd.y Then
    Say x y 'found after' time('E') 'seconds'
    End
  End
Say time('E') 'seconds total search time'
Exit

proper_divisors: Procedure
Parse Arg n
Pd=''
If n=1 Then Return ''
If n//2=1 Then  /* odd number  */
  delta=2
Else            /* even number */
  delta=1
Do d=1 To n%2 By delta
  If n//d=0 Then
    pd=pd d
  End
Return space(pd)

sum: Procedure
Parse Arg list
sum=0
Do i=1 To words(list)
  sum=sum+word(list,i)
  End
Return sum
```

{{out}}

```txt
sum(pd) computed in 48.502000 seconds
220 284 found after 3.775000 seconds
1184 1210 found after 21.611000 seconds
2620 2924 found after 46.817000 seconds
5020 5564 found after 84.296000 seconds
6232 6368 found after 100.918000 seconds
10744 10856 found after 150.126000 seconds
12285 14595 found after 162.124000 seconds
17296 18416 found after 185.600000 seconds
188.836000 seconds total search time
```


===version 2, using SIGMA function===
This REXX version allows the specification of the upper limit (for the searching of amicable pairs).

Some optimization was incorporated by using a   '''sigma'''   function,   which was a re-coded   ''proper divisors''   (Pdivs)   function,

which was taken from the REXX language entry for Rosetta Code task   ''integer factors''.

Other optimizations were incorporated which took advantage of several well-known generalizations about amicable pairs.

The generation/summation is about   '''50'''  times faster;   searching is about   '''100'''   times faster.

Time consumption note:   for every doubling of   '''H'''   (the upper limit for searches),   the time consumed triples.

```rexx
/*REXX program  calculates and displays all  amicable pairs  up to  a given number.     */
parse arg H .;   if H=='' | H==","  then H=20000 /*get optional arguments  (high limit).*/
w=length(H)  ;   low=220                         /*W: used for columnar output alignment*/
@.=.                                             /* [↑]  LOW is lowest amicable number. */
     do k=low  for H-low;     _=sigma(k)         /*generate sigma sums for a range of #s*/
     if _>=low  then @.k=_                       /*only keep the pertinent sigma sums.  */
     end   /*k*/                                 /* [↑]   process a range of integers.  */
#=0                                              /*number of amicable pairs found so far*/
     do   m=low  to  H;       n=@.m              /*start the search at the lowest number*/
     if m==@.n  then do                          /*If equal, might be an amicable number*/
                     if m==n  then iterate       /*Is this a perfect number?  Then skip.*/
                     #=#+1                       /*bump the  amicable pair  counter.    */
                     say right(m,w)     ' and '    right(n,w)     " are an amicable pair."
                     m=n                         /*start   M   (DO index)  from  N.     */
                     end
     end    /*m*/
say
say #   'amicable pairs found up to'    H        /*display count of the amicable pairs. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sigma: procedure; parse arg x;   od=x//2         /*use either  EVEN  or  ODD  integers. */
       s=1                                       /*set initial sigma sum to unity.   ___*/
             do j=2+od  by 1+od  while  j*j<x    /*divide by all integers up to the √ x */
             if x//j==0  then  s=s + j + x%j     /*add the two divisors to the sum.     */
             end   /*j*/                         /* [↑]  %  is REXX integer division.   */
       return s                                  /*return the sum of the divisors.      */
```

'''output'''   when using the default input:

```txt

  220  and    284  are an amicable pair.
 1184  and   1210  are an amicable pair.
 2620  and   2924  are an amicable pair.
 5020  and   5564  are an amicable pair.
 6232  and   6368  are an amicable pair.
10744  and  10856  are an amicable pair.
12285  and  14595  are an amicable pair.
17296  and  18416  are an amicable pair.

8 amicable pairs found up to 20000

```


===version 3, SIGMA with limited searches===
This REXX version is optimized to take advantage of the lowest ending-single-digit amicable number,   and

also incorporates the search of amicable numbers into the generation of the sigmas of the integers.

The optimization makes it about another 30% faster when searching for amicable numbers up to one million.

```rexx
/*REXX program  calculates and displays all  amicable pairs  up to  a given number.     */
parse arg H .;   if H=='' | H==","  then H=20000 /*get optional arguments  (high limit).*/
w=length(H)  ;   low=220                         /*W: used for columnar output alignment*/
x=220 34765731 6232 87633 284 12285 10856 36939357 6368 5684679          /*S  minimums. */
   do i=0 for 10;  $.i=word(x,i+1);  end /*i*/   /*minimum amicable #s for last dec dig.*/
#=0                                              /*number of amicable pairs found so far*/
@.=                                              /* [↑]  LOW is lowest amicable number. */
     do k=low  for H-low                         /*generate sigma sums for a range of #s*/
     parse var k  ''  -1  D                      /*obtain last decimal digit of   K.    */
     if k<$.D    then iterate                    /*if no need to compute, then skip it. */
         _=sigma(k)                              /*generate sigma sum for the number  K.*/
     @.k=_                                       /*only keep the pertinent sigma sums.  */
     if k==@._  then do                          /*is it a possible amicable number ?   */
                     if _==k  then iterate       /*Is it a perfect number?  Then skip it*/
                     #=#+1                       /*bump the amicable pair counter.      */
                     say right(_,w)     ' and '     right(k,w)    " are an amicable pair."
                     end
     end   /*k*/                                 /* [↑]   process a range of integers.  */
say
say #   'amicable pairs found up to'    H        /*display the count of amicable pairs. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sigma: procedure; parse arg x;   od=x//2         /*use either  EVEN  or  ODD  integers. */
       s=1                                       /*set initial sigma sum to unity.   ___*/
             do j=2+od  by 1+od  while  j*j<x    /*divide by all integers up to the √ x */
             if x//j==0  then  s=s + j + x%j     /*add the two divisors to the sum.     */
             end   /*j*/                         /* [↑]  %  is REXX integer division.   */
       return s                                  /*return the sum of the divisors.      */
```

'''output'''   is the same as the 2<sup>nd</sup> REXX version.

===version 4, SIGMA using integer SQRT===
This REXX version is optimized to use the   ''integer square root of X''   in the   '''sigma'''   function   (instead of

computing the square of   '''J'''   to see if that value exceeds   '''X''').

The optimization makes it about another 20% faster when searching for amicable numbers up to one million.

```rexx
/*REXX program  calculates and displays all  amicable pairs  up to  a given number.     */
parse arg H .;   if H=='' | H==","  then H=20000 /*get optional arguments  (high limit).*/
w=length(H)  ;   low=220                         /*W: used for columnar output alignment*/
x=220 34765731 6232 87633 284 12285 10856 36939357 6368 5684679          /*S  minimums. */
   do i=0 for 10;  $.i=word(x,i+1);  end  /*i*/  /*minimum amicable #s for last dec dig.*/
#=0                                              /*number of amicable pairs found so far*/
@.=                                              /* [↑]  LOW is lowest amicable number. */
     do k=low  for H-low                         /*generate sigma sums for a range of #s*/
     parse var k  ''  -1  D                      /*obtain last decimal digit of   K.    */
     if k<$.D    then iterate                    /*if no need to compute, then skip it. */
         _=sigma(k)                              /*generate sigma sum for the number  K.*/
     @.k=_                                       /*only keep the pertinent sigma sums.  */
     if k==@._  then do                          /*is it a possible amicable number ?   */
                     if _==k  then iterate       /*Is it a perfect number?  Then skip it*/
                     #=#+1                       /*bump the amicable pair counter.      */
                     say right(_,w)     ' and '     right(k,w)    " are an amicable pair."
                     end
     end   /*k*/                                 /* [↑]   process a range of integers.  */
say
say #   'amicable pairs found up to'    H        /*display the count of amicable pairs. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt: procedure; parse arg x;  r=0;  q=1;             do while q<=x;  q=q*4;  end
                  do while q>1; q=q%4; _=x-r-q; r=r%2; if _>=0 then do;x=_;r=r+q; end; end
       return r
/*──────────────────────────────────────────────────────────────────────────────────────*/
sigma: procedure; parse arg x;   od=x//2         /*use either  EVEN  or  ODD  integers. */
       s=1                                       /*set initial sigma sum to unity.   ___*/
                do j=2+od  by 1+od  to iSqrt(x)  /*divide by all integers up to the √ x */
                if x//j==0  then  s=s + j + x%j  /*add the two divisors to the sum.     */
                end   /*j*/                      /* [↑]  % is the REXX integer division.*/
       return s                                  /*return the sum of the divisors.      */
```

'''output'''   is the same as the 2<sup>nd</sup> REXX version.

===version 5, SIGMA (in-line code)===
This REXX version is optimized by bringing the functions in-line   (which minimizes the overhead of invoking two

internal functions),   and it also pre-computes the powers of four   (for the integer square root code).

This method of coding has the disadvantage in that the code (logic) is less idiomatic and therefore less readable.

The optimization makes it about another 15% faster when searching for amicable numbers up to one million.

```rexx
/*REXX program  calculates and displays all  amicable pairs  up to  a given number.     */
parse arg H .;   if H=='' | H==","  then H=20000 /*get optional arguments  (high limit).*/
w=length(H)  ;   low=220                         /*W: used for columnar output alignment*/
x=220 34765731 6232 87633 284 12285 10856 36939357 6368 5684679           /*S  minimums.*/
   do i=0 for 10;  $.i=word(x,i+1);  end  /*i*/  /*minimum amicable #s for last dec dig.*/
f.=0;   do p=0  until f.p>10**digits();  f.p=4**p;  end  /*p*/         /*calc. pows of 4*/
#=0                                              /*number of amicable pairs found so far*/
@.=                                              /* [↑]  LOW is lowest amicable number. */
     do k=low  for H-low+1                       /*generate sigma sums for a range of #s*/
     parse var k  ''  -1  D                      /*obtain last decimal digit of   K.    */
     if k<$.D     then iterate                   /*if no need to compute, then skip it. */
     od=k//2                                     /*OD:   set to  unity  if   K   is odd.*/
     z=k; q=1;  do p=0  while f.p<=z; q=f.p; end /*R  will end up being the  iSqrt of Z.*/
     r=0;  do while q>1; q=q%4; _=z-r-q; r=r%2;  if _>=0  then do;  z=_; r=r+q;  end;  end
     s=1                                         /*set initial sigma sum to unity.   ___*/
           do j=2+od  by 1+od  to r              /*divide by all integers up to the √ K */
           if k//j==0  then s=s+ j + k%j         /*add the two divisors to the sum.     */
           end   /*j*/                           /* [↑]  %  is REXX integer division.   */
     @.k=s                                       /*only keep the pertinent sigma sums.  */
     if k==@.s  then do                          /*is it a possible amicable number ?   */
                     if s==k  then iterate       /*Is it a perfect number?  Then skip it*/
                     #=#+1                       /*bump the amicable pair counter.      */
                     say right(s,w)     ' and '     right(k,w)    " are an amicable pair."
                     end
     end   /*k*/                                 /* [↑]   process a range of integers.  */
say                                              /*stick a fork in it,  we're all done. */
say #   'amicable pairs found up to'    H        /*display the count of amicable pairs. */
```

'''output'''   is the same as the 2<sup>nd</sup> REXX version.




## Ring


```ring

size = 18500
for n = 1 to size
    m = amicable(n)
    if m>n and amicable(m)=n
       see "" + n + " and " + m + nl ok
next
see "OK" + nl

func amicable nr
     sum = 1
     for d = 2 to sqrt(nr)
         if nr % d = 0
            sum = sum + d
            sum = sum + nr / d ok
     next
     return sum

```



## Ruby

With [[proper_divisors#Ruby]] in place:

```ruby
h = {}
(1..20_000).each{|n| h[n] = n.proper_divisors.sum }
h.select{|k,v| h[v] == k && k < v}.each do |key,val|  # k<v filters out doubles and perfects
  puts "#{key} and #{val}"
end

```

{{out}}
```txt

220 and 284
1184 and 1210
2620 and 2924
5020 and 5564
6232 and 6368
10744 and 10856
12285 and 14595
17296 and 18416

```



## Run BASIC


```Runbasic
size = 18500
for n = 1 to size
    m = amicable(n)
    if m > n and amicable(m) = n then print  n ; " and " ; m
next

function amicable(nr)
     amicable = 1
     for d = 2 to sqr(nr)
         if nr mod d = 0 then amicable = amicable + d + nr / d
     next
 end function
```


```txt

220 and 284
1184 and 1210
2620 and 2924
5020 and 5564
6232 and 6368
10744 and 10856
12285 and 14595
17296 and 18416

```



## Rust



```rust
fn sum_of_divisors(val: u32) -> u32 {
    (1..val/2+1).filter(|n| val % n == 0)
                .fold(0, |sum, n| sum + n)
}

fn main() {
    let iter = (1..20_000).map(|i| (i, sum_of_divisors(i)))
                          .filter(|&(i, div_sum)| i > div_sum);

    for (i, sum1) in iter {
        if sum_of_divisors(sum1) == i {
           println!("{} {}", i, sum1);
        }
    }
}
```

{{out}}

```txt

284 220
1210 1184
2924 2620
5564 5020
6368 6232
10856 10744
14595 12285
18416 17296

```



## Scala


```Scala
def properDivisors(n: Int) = (1 to n/2).filter(i => n % i == 0)
val divisorsSum = (1 to 20000).map(i => i -> properDivisors(i).sum).toMap
val result = divisorsSum.filter(v => v._1 < v._2 && divisorsSum.get(v._2) == Some(v._1))

println( result mkString ", " )
```

{{out}}

```txt
5020 -> 5564, 220 -> 284, 6232 -> 6368, 17296 -> 18416, 2620 -> 2924, 10744 -> 10856, 12285 -> 14595, 1184 -> 1210
```



## Scheme



```scheme

(import (scheme base)
        (scheme inexact)
        (scheme write)
        (only (srfi 1) fold))

;; return a list of the proper-divisors of n
(define (proper-divisors n)
  (let ((root (sqrt n)))
    (let loop ((divisors (list 1))
               (i 2))
      (if (> i root)
        divisors
        (loop (if (zero? (modulo n i))
                (if (= (square i) n)
                  (cons i divisors)
                  (append (list i (quotient n i)) divisors))
                divisors)
              (+ 1 i))))))

(define (sum-proper-divisors n)
  (if (< n 2)
    0
    (fold + 0 (proper-divisors n))))

(define *max-n* 20000)

;; hold sums of proper divisors in a cache, to avoid recalculating
(define *cache* (make-vector (+ 1 *max-n*)))
(for-each (lambda (i) (vector-set! *cache* i (sum-proper-divisors i)))
          (iota *max-n* 1))

(define (amicable-pair? i j)
  (and (not (= i j))
       (= i (vector-ref *cache* j))
       (= j (vector-ref *cache* i))))

;; double loop to *max-n*, displaying all amicable pairs
(let loop-i ((i 1))
  (when (<= i *max-n*)
    (let loop-j ((j i))
      (when (<= j *max-n*)
        (when (amicable-pair? i j)
          (display (string-append "Amicable pair: "
                                  (number->string i)
                                  " "
                                  (number->string j)))
          (newline))
        (loop-j (+ 1 j))))
    (loop-i (+ 1 i))))

```


{{out}}

```txt

Amicable pair: 220 284
Amicable pair: 1184 1210
Amicable pair: 2620 2924
Amicable pair: 5020 5564
Amicable pair: 6232 6368
Amicable pair: 10744 10856
Amicable pair: 12285 14595
Amicable pair: 17296 18416

```



## Sidef


```ruby
func propdivsum(n) {
    n.sigma - n
}

for i in (1..20000) {
    var j = propdivsum(i)
    say "#{i} #{j}" if (j>i && i==propdivsum(j))
}
```

{{out}}

```txt

220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416

```



## Swift


```Swift
import func Darwin.sqrt

func sqrt(x:Int) -> Int { return Int(sqrt(Double(x))) }

func properDivs(n: Int) -> [Int] {

    if n == 1 { return [] }

    var result = [Int]()

    for div in filter (1...sqrt(n), { n % $0 == 0 }) {

        result.append(div)

        if n/div != div && n/div != n { result.append(n/div) }
    }

    return sorted(result)

}


func sumDivs(n:Int) -> Int {

    struct Cache { static var sum = [Int:Int]() }

    if let sum = Cache.sum[n] { return sum }

    let sum = properDivs(n).reduce(0) { $0 + $1 }

    Cache.sum[n] = sum

    return sum
}

func amicable(n:Int, m:Int) -> Bool {

    if n == m { return false }

    if sumDivs(n) != m || sumDivs(m) != n { return false }

    return true
}

var pairs = [(Int, Int)]()

for n in 1 ..< 20_000 {
    for m in n+1 ... 20_000 {
        if amicable(n, m) {
            pairs.append(n, m)
            println("\(n, m)")
        }
    }
}
```


### Alternative

about 800 times faster.
```Swift
import func Darwin.sqrt

func sqrt(x:Int) -> Int { return Int(sqrt(Double(x))) }

func sigma(n: Int) -> Int {

    if n == 1 { return 0 }          // definition of aliquot sum

    var result = 1
    let root = sqrt(n)

    for var div = 2; div <= root; ++div {
        if n % div == 0 {
            result += div + n/div
        }

    }
    if root*root == n { result -= root }

    return (result)
}

func amicables (upTo: Int) -> () {

    var aliquot = Array(count: upTo+1, repeatedValue: 0)

    for i in 1 ... upTo {           // fill lookup array
        aliquot[i] = sigma(i)
    }

 for i in 1 ... upTo {
        let a = aliquot[i]
        if a > upTo {continue}      //second part of pair out-of-bounds

        if a == i {continue}        //skip perfect numbers

        if i == aliquot[a] {
            print("\(i, a)")
            aliquot[a] = upTo+1     //prevent second display of pair
        }
    }
}

amicables(20_000)
```

{{out}}

```txt
(220, 284)
(1184, 1210)
(2620, 2924)
(5020, 5564)
(6232, 6368)
(10744, 10856)
(12285, 14595)
(17296, 18416)

```



## tbas


```vb

dim sums(20000)

sub sum_proper_divisors(n)
	dim sum = 0
	dim i
	if n > 1 then
		for i = 1 to (n \ 2)
			if n %% i = 0 then
				sum = sum + i
			end if
		next
	end if
	return sum
end sub

dim i, j
for i = 1 to 20000
	sums(i) = sum_proper_divisors(i)
	for j = i-1 to 2 by -1
		if sums(i) = j and sums(j) = i then
			print "Amicable pair:";sums(i);"-";sums(j)
			exit for
		end if
	next
next

```


```txt

>tbas amicable_pairs.bas
Amicable pair: 220 - 284
Amicable pair: 1184 - 1210
Amicable pair: 2620 - 2924
Amicable pair: 5020 - 5564
Amicable pair: 6232 - 6368
Amicable pair: 10744 - 10856
Amicable pair: 12285 - 14595
Amicable pair: 17296 - 18416

```



## Tcl


```tcl
proc properDivisors {n} {
    if {$n == 1} return
    set divs 1
    set sum 1
    for {set i 2} {$i*$i <= $n} {incr i} {
	if {!($n % $i)} {
	    lappend divs $i
	    incr sum $i
	    if {$i*$i < $n} {
		lappend divs [set d [expr {$n / $i}]]
		incr sum $d
	    }
	}
    }
    return [list $sum $divs]
}

proc amicablePairs {limit} {
    set result {}
    set sums [set divs {{}}]
    for {set n 1} {$n < $limit} {incr n} {
	lassign [properDivisors $n] sum d
	lappend sums $sum
	lappend divs [lsort -integer $d]
    }
    for {set n 1} {$n < $limit} {incr n} {
	set nsum [lindex $sums $n]
	for {set m 1} {$m < $n} {incr m} {
	    if {$n==[lindex $sums $m] && $m==$nsum} {
		lappend result $m $n [lindex $divs $m] [lindex $divs $n]
	    }
	}
    }
    return $result
}

foreach {m n md nd} [amicablePairs 20000] {
    puts "$m and $n are an amicable pair with these proper divisors"
    puts "\t$m : $md"
    puts "\t$n : $nd"
}
```

{{out}}

```txt

220 and 284 are an amicable pair with these proper divisors
	220 : 1 2 4 5 10 11 20 22 44 55 110
	284 : 1 2 4 71 142
1184 and 1210 are an amicable pair with these proper divisors
	1184 : 1 2 4 8 16 32 37 74 148 296 592
	1210 : 1 2 5 10 11 22 55 110 121 242 605
2620 and 2924 are an amicable pair with these proper divisors
	2620 : 1 2 4 5 10 20 131 262 524 655 1310
	2924 : 1 2 4 17 34 43 68 86 172 731 1462
5020 and 5564 are an amicable pair with these proper divisors
	5020 : 1 2 4 5 10 20 251 502 1004 1255 2510
	5564 : 1 2 4 13 26 52 107 214 428 1391 2782
6232 and 6368 are an amicable pair with these proper divisors
	6232 : 1 2 4 8 19 38 41 76 82 152 164 328 779 1558 3116
	6368 : 1 2 4 8 16 32 199 398 796 1592 3184
10744 and 10856 are an amicable pair with these proper divisors
	10744 : 1 2 4 8 17 34 68 79 136 158 316 632 1343 2686 5372
	10856 : 1 2 4 8 23 46 59 92 118 184 236 472 1357 2714 5428
12285 and 14595 are an amicable pair with these proper divisors
	12285 : 1 3 5 7 9 13 15 21 27 35 39 45 63 65 91 105 117 135 189 195 273 315 351 455 585 819 945 1365 1755 2457 4095
	14595 : 1 3 5 7 15 21 35 105 139 417 695 973 2085 2919 4865
17296 and 18416 are an amicable pair with these proper divisors
	17296 : 1 2 4 8 16 23 46 47 92 94 184 188 368 376 752 1081 2162 4324 8648
	18416 : 1 2 4 8 16 1151 2302 4604 9208

```



## uBasic/4tH

<lang>Input "Limit: ";l
Print "Amicable pairs < ";l

For n = 1 To l
  m = FUNC(_SumDivisors (n))-n
  If m = 0 Then Continue               ' No division by zero, please
  p = FUNC(_SumDivisors (m))-m
  If (n=p) * (n<m) Then Print n;" and ";m
Next

End

_LeastPower Param(2)
  Local(1)

  c@ = a@
  Do While (b@ % c@) = 0
    c@ = c@ * a@
  Loop

Return (c@)


' Return the sum of the proper divisors of a@

_SumDivisors Param(1)
  Local(4)

  b@ = a@
  c@ = 1

  ' Handle two specially

  d@ = FUNC(_LeastPower (2,b@))
  c@ = c@ * (d@ - 1)
  b@ = b@ / (d@ / 2)

  ' Handle odd factors

  For e@ = 3 Step 2 While (e@*e@) < (b@+1)
    d@ = FUNC(_LeastPower (e@,b@))
    c@ = c@ * ((d@ - 1) / (e@ - 1))
    b@ = b@ / (d@ / e@)
  Loop

  ' At this point, t must be one or prime

  If (b@ > 1) c@ = c@ * (b@+1)
Return (c@)
```

{{Out}}

```txt
Limit: 20000
Amicable pairs < 20000
220 and 284
1184 and 1210
2620 and 2924
5020 and 5564
6232 and 6368
10744 and 10856
12285 and 14595
17296 and 18416

0 OK, 0:238
```



## UTFool


```UTFool

···
http://rosettacode.org/wiki/Amicable_pairs
···
■ AmicablePairs
  § static
    ▶ main
    • args⦂ String[]
      ∀ n ∈ 1…20000
        m⦂ int: sumPropDivs n
        if m < n = sumPropDivs m
           System.out.println "⸨m⸩ ; ⸨n⸩"

    ▶ sumPropDivs⦂ int
    • n⦂ int
      m⦂ int: 1
      ∀ i ∈ √n ⋯> 1
        m +: n \ i = 0 ? i + (i = n / i ? 0 ! n / i) ! 0
      ⏎ m

```



## VBA


```vb
Option Explicit

Public Sub AmicablePairs()
Dim a(2 To 20000) As Long, c As New Collection, i As Long, j As Long, t#
    t = Timer
    For i = LBound(a) To UBound(a)
        'collect the sum of the proper divisors
        'of each numbers between 2 and 20000
        a(i) = S(i)
    Next
    'Double Loops to test the amicable
    For i = LBound(a) To UBound(a)
        For j = i + 1 To UBound(a)
            If i = a(j) Then
                If a(i) = j Then
                     On Error Resume Next
                     c.Add i & " : " & j, CStr(i * j)
                     On Error GoTo 0
                     Exit For
                End If
            End If
        Next
    Next
    'End. Return :
    Debug.Print "Execution Time : " & Timer - t & " seconds."
    Debug.Print "Amicable pairs below 20 000 are : "
    For i = 1 To c.Count
        Debug.Print c.Item(i)
    Next i
End Sub

Private Function S(n As Long) As Long
'returns the sum of the proper divisors of n
Dim j As Long
    For j = 1 To n \ 2
        If n Mod j = 0 Then S = j + S
    Next
End Function
```

{{out}}

```txt
Execution Time : 7,95703125 seconds.
Amicable pairs below 20 000 are :
220 : 284
1184 : 1210
2620 : 2924
5020 : 5564
6232 : 6368
10744 : 10856
12285 : 14595
17296 : 18416
```



## VBScript

Not at all optimal. :-(

```VBScript
start = Now
Set nlookup = CreateObject("Scripting.Dictionary")
Set uniquepair = CreateObject("Scripting.Dictionary")

For i = 1 To 20000
	sum = 0
	For n = 1 To 20000
		If n < i Then
			If i Mod n = 0 Then
				sum = sum + n
			End If
		End If
	Next
	nlookup.Add i,sum
Next

For j = 1 To 20000
	sum = 0
	For m = 1 To 20000
		If m < j Then
			If j Mod m = 0 Then
				sum = sum + m
			End If
		End If
	Next
	If nlookup.Exists(sum) And nlookup.Item(sum) = j And j <> sum _
		And uniquepair.Exists(sum) = False Then
			uniquepair.Add j,sum
	End If
Next

For Each key In uniquepair.Keys
	WScript.Echo key & ":" & uniquepair.Item(key)
Next

WScript.Echo "Execution Time: " & DateDiff("s",Start,Now) & " seconds"
```

{{out}}

```txt
220:284
1184:1210
2620:2924
5020:5564
6232:6368
10744:10856
12285:14595
17296:18416
Execution Time: 162 seconds
```



## Yabasic

{{trans|Lua}}

```Yabasic
sub sumDivs(n)
    local sum, d

    sum = 1

    for d = 2 to sqrt(n)
        if not mod(n, d) then
            sum = sum + d
            sum = sum + n / d
        end if
    next
    return sum
end sub

for n = 2 to 20000
    m = sumDivs(n)
    if m > n then
        if sumDivs(m) = n print n, "\t", m
    end if
next

print : print peek("millisrunning"), " ms"
```



## zkl

Slooooow

```zkl
fcn properDivs(n){ [1.. (n + 1)/2 + 1].filter('wrap(x){ n%x==0 and n!=x }) }
const N=20000;
sums:=[1..N].pump(T(-1),fcn(n){ properDivs(n).sum(0) });
[0..].zip(sums).filter('wrap([(n,s)]){ (n<s<=N) and sums[s]==n }).println();
```

{{out}}

```txt

L(L(220,284),L(1184,1210),L(2620,2924),L(5020,5564),L(6232,6368),L(10744,10856),L(12285,14595),L(17296,18416))

```



## ZX Spectrum Basic

{{trans|AWK}}

```zxbasic
10 LET limit=20000
20 PRINT "Amicable pairs < ";limit
30 FOR n=1 TO limit
40 LET num=n: GO SUB 1000
50 LET m=num
60 GO SUB 1000
70 IF n=num AND n<m THEN PRINT n;" ";m
80 NEXT n
90 STOP
1000 REM sumprop
1010 IF num<2 THEN LET num=0: RETURN
1020 LET sum=1
1030 LET root=SQR num
1040 FOR i=2 TO root-.01
1050 IF num/i=INT (num/i) THEN LET sum=sum+i+num/i
1060 NEXT i
1070 IF num/root=INT (num/root) THEN LET sum=sum+root
1080 LET num=sum
1090 RETURN
```

{{out}}

```txt
Amicable pairs < 20000
220 284
1184 1210
2620 2924
5020 5564
6232 6368
10744 10856
12285 14595
17296 18416
```


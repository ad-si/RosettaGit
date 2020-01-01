+++
title = "Abundant, deficient and perfect number classifications"
description = ""
date = 2019-10-14T12:59:19Z
aliases = []
[extra]
id = 18392
[taxonomies]
categories = []
tags = []
+++

{{task}}
These define three classifications of positive integers based on their   [[Proper divisors|proper divisors]].

Let   P(n)   be the sum of the proper divisors of   '''n'''   where the proper divisors are all positive divisors of   '''n'''   other than   '''n'''   itself.
    if   <code> P(n) <  n </code>   then  '''n'''  is classed as  '''deficient'''  ([https://oeis.org/A005100 OEIS A005100]).
    if   <code> P(n) == n </code>   then  '''n'''  is classed as  '''perfect'''    ([https://oeis.org/A000396 OEIS A000396]).
    if   <code> P(n) >  n </code>   then  '''n'''  is classed as  '''abundant'''   ([https://oeis.org/A005101 OEIS A005101]).


;Example:
'''6'''   has proper divisors of   '''1''',   '''2''',   and   '''3'''.

'''1 + 2 + 3 = 6''',   so   '''6'''    is classed as a perfect number.


;Task:
Calculate how many of the integers   '''1'''   to   '''20,000'''   (inclusive) are in each of the three classes.

Show the results here.


;Related tasks:
*   [[Aliquot sequence classifications]].   (The whole series from which this task is a subset.)
*   [[Proper divisors]]
*   [[Amicable pairs]]



## 11l

{{trans|Kotlin}}

```11l
F sum_proper_divisors(n)
   R I n < 2 {0} E sum((1 .. n I/ 2).filter(it -> (@n % it) == 0))

V deficient = 0
V perfect = 0
V abundant = 0

L(n) 1..20000
   V sp = sum_proper_divisors(n)
   I sp < n
      deficient++
   E I sp == n
      perfect++
   E I sp > n
      abundant++

print(‘Deficient = ’deficient)
print(‘Perfect   = ’perfect)
print(‘Abundant  = ’abundant)
```

{{out}}

```txt

Deficient = 15043
Perfect   = 4
Abundant  = 4953

```



## 360 Assembly

{{trans|VBScript}}
For maximum compatibility, this program uses only the basic instruction set (S/360)
with 2 ASSIST macros (XDECO,XPRNT).

```360asm
*        Abundant, deficient and perfect number  08/05/2016
ABUNDEFI CSECT
         USING  ABUNDEFI,R13       set base register
SAVEAR   B      STM-SAVEAR(R15)    skip savearea
         DC     17F'0'             savearea
STM      STM    R14,R12,12(R13)    save registers
         ST     R13,4(R15)         link backward SA
         ST     R15,8(R13)         link forward SA
         LR     R13,R15            establish addressability
         SR     R10,R10            deficient=0
         SR     R11,R11            perfect  =0
         SR     R12,R12            abundant =0
         LA     R6,1               i=1
LOOPI    C      R6,NN              do i=1 to nn
         BH     ELOOPI
         SR     R8,R8              sum=0
         LR     R9,R6              i
         SRA    R9,1               i/2
         LA     R7,1               j=1
LOOPJ    CR     R7,R9              do j=1 to i/2
         BH     ELOOPJ
         LR     R2,R6              i
         SRDA   R2,32
         DR     R2,R7              i//j=0
         LTR    R2,R2              if i//j=0
         BNZ    NOTMOD
         AR     R8,R7              sum=sum+j
NOTMOD   LA     R7,1(R7)           j=j+1
         B      LOOPJ
ELOOPJ   CR     R8,R6              if sum?i
         BL     SLI                      <
         BE     SEI                      =
         BH     SHI                      >
SLI      LA     R10,1(R10)         deficient+=1
         B      EIF
SEI      LA     R11,1(R11)         perfect  +=1
         B      EIF
SHI      LA     R12,1(R12)         abundant +=1
EIF      LA     R6,1(R6)           i=i+1
         B      LOOPI
ELOOPI   XDECO  R10,XDEC           edit deficient
         MVC    PG+10(5),XDEC+7
         XDECO  R11,XDEC           edit perfect
         MVC    PG+24(5),XDEC+7
         XDECO  R12,XDEC           edit abundant
         MVC    PG+39(5),XDEC+7
         XPRNT  PG,80              print buffer
         L      R13,4(0,R13)       restore savearea pointer
         LM     R14,R12,12(R13)    restore registers
         XR     R15,R15            return code = 0
         BR     R14                return to caller
NN       DC     F'20000'
PG       DC     CL80'deficient=xxxxx perfect=xxxxx abundant=xxxxx'
XDEC     DS     CL12
         REGEQU
         END    ABUNDEFI
```

{{out}}

```txt

deficient=15043 perfect=    4 abundant= 4953

```



## Ada

This solution uses the package ''Generic_Divisors'' from the Proper Divisors task
[[http://rosettacode.org/wiki/Proper_divisors#Ada]].


```Ada
with Ada.Text_IO, Generic_Divisors;

procedure ADB_Classification is
   function Same(P: Positive) return Positive is (P);
   package Divisor_Sum is new Generic_Divisors
     (Result_Type => Natural, None => 0, One => Same, Add =>  "+");

   type Class_Type is (Deficient, Perfect, Abundant);

   function Class(D_Sum, N: Natural) return Class_Type is
      (if D_Sum < N then Deficient
       elsif D_Sum = N then Perfect
       else Abundant);

   Cls: Class_Type;
   Results: array (Class_Type) of Natural := (others => 0);

   package NIO is new Ada.Text_IO.Integer_IO(Natural);
   package CIO is new Ada.Text_IO.Enumeration_IO(Class_Type);
begin
   for N in 1 .. 20_000 loop
      Cls := Class(Divisor_Sum.Process(N), N);
      Results(Cls) := Results(Cls)+1;
   end loop;
   for Class in Results'Range loop
      CIO.Put(Class, 12);
      NIO.Put(Results(Class), 8);
      Ada.Text_IO.New_Line;
   end loop;
   Ada.Text_IO.Put_Line("--------------------");
   Ada.Text_IO.Put("Sum         ");
   NIO.Put(Results(Deficient)+Results(Perfect)+Results(Abundant), 8);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("
### ==============
");
end ADB_Classification;
```


{{out}}

```txt
DEFICIENT      15043
PERFECT            4
ABUNDANT        4953
--------------------
Sum            20000

### ==============

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

# classify the numbers 1 : 20 000 as abudant, deficient or perfect #
INT abundant count    := 0;
INT deficient count   := 0;
INT perfect count     := 0;
INT abundant example  := 0;
INT deficient example := 0;
INT perfect example   := 0;
INT max number         = 20 000;
FOR n TO max number DO
    IF     INT pd sum = sum proper divisors( n );
           pd sum < n
    THEN
        # have a deficient number                                  #
        deficient count    +:= 1;
        deficient example   := n
    ELIF   pd sum = n
    THEN
        # have a perfect number                                    #
        perfect count      +:= 1;
        perfect example     := n
    ELSE # pd sum > n #
        # have an abundant number                                  #
        abundant count     +:= 1;
        abundant example    := n
    FI
OD;

# show how many of each type of number there are and an example    #

# displays the classification, count and example                   #
PROC show result = ( STRING classification, INT count, example )VOID:
     print( ( "There are "
            , whole( count, -8 )
            , " "
            , classification
            , " numbers up to "
            , whole( max number, 0 )
            , " e.g.: "
            , whole( example, 0 )
            , newline
            )
          );

show result( "abundant ",  abundant count,  abundant example  );
show result( "deficient", deficient count, deficient example );
show result( "perfect  ",   perfect count,   perfect example   )
```

{{out}}

```txt

There are     4953 abundant  numbers up to 20000 e.g.: 20000
There are    15043 deficient numbers up to 20000 e.g.: 19999
There are        4 perfect   numbers up to 20000 e.g.: 8128

```



## AutoHotkey


```autohotkey
Loop
{
    m := A_index
    ; getting factors
### ===============

    loop % floor(sqrt(m))
    {
        if ( mod(m, A_index) == "0" )
        {
            if ( A_index ** 2 == m )
            {
                list .= A_index . ":"
                sum := sum + A_index
                continue
            }
            if ( A_index != 1 )
            {
                list .= A_index . ":" . m//A_index . ":"
                sum := sum + A_index + m//A_index
            }
            if ( A_index == "1" )
            {
                list .= A_index . ":"
                sum := sum + A_index
            }
        }
    }
    ; Factors obtained above
### =========

    if ( sum == m ) && ( sum != 1 )
    {
        result := "perfect"
        perfect++
    }
    if ( sum > m )
    {
        result := "Abundant"
        Abundant++
    }
    if ( sum < m ) or ( m == "1" )
    {
        result := "Deficient"
        Deficient++
    }
    if ( m == 20000 )
    {
        MsgBox % "number: " . m . "`nFactors:`n" . list . "`nSum of Factors: " . Sum . "`nResult: " . result . "`n_______________________`nTotals up to: " . m . "`nPerfect: " . perfect . "`nAbundant: " . Abundant . "`nDeficient: " . Deficient
        ExitApp
    }
    list := ""
    sum := 0
}

esc::ExitApp

```

{{out}}

```txt

number: 20000
Factors:
1:2:10000:4:5000:5:4000:8:2500:10:2000:16:1250:20:1000:25:800:32:625:40:500:50:400:80:250:100:200:125:160:
Sum of Factors: 29203
Result: Abundant
_______________________
Totals up to: 20000
Perfect: 4
Abundant: 4953
Deficient: 15043

```



## AWK

works with GNU Awk 3.1.5 and with BusyBox v1.21.1

```AWK

#!/bin/gawk -f
function sumprop(num,   i,sum,root) {
if (num == 1) return 0
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
limit = 20000
abundant = 0
defiecient =0
perfect = 0

for (j=1; j < limit+1; j++)
    {
    sump = sumprop(j)
    if (sump < j) deficient = deficient + 1
    if (sump == j) perfect = perfect + 1
    if (sump > j) abundant = abundant + 1
    }
print "For 1 through " limit
print "Perfect: " perfect
print "Abundant: " abundant
print "Deficient: " deficient
}

```


{{out}}

```txt

For 1 through 20000
Perfect: 4
Abundant: 4953
Deficient: 15043

```



## Batch File

As batch files aren't particularly well-suited to increasingly large arrays of data, this code will chew through processing power.

```dos

@echo off
setlocal enabledelayedexpansion

:_main

for /l %%i in (1,1,20000) do (

  echo Processing %%i

  call:_P %%i
  set Pn=!errorlevel!
  if !Pn! lss %%i set /a deficient+=1
  if !Pn!==%%i set /a perfect+=1
  if !Pn! gtr %%i set /a abundant+=1
  cls
)

echo Deficient - %deficient% ^| Perfect - %perfect% ^| Abundant - %abundant%
pause>nul


:_P
setlocal enabledelayedexpansion
set sumdivisers=0

set /a upperlimit=%1-1

for /l %%i in (1,1,%upperlimit%) do (
  set /a isdiviser=%1 %% %%i
  if !isdiviser!==0 set /a sumdivisers+=%%i
)

exit /b %sumdivisers%

```



## Befunge


This is not a particularly efficient implementation, so unless you're using a compiler, you can expect it to take a good few minutes to complete. But you can always test with a shorter range of numbers by replacing the 20000 (<tt>"2":*8*</tt>) near the start of the first line.


```befunge
p0"2":*8*>::2/\:2/\28*:*:**+>::28*:*:*/\28*:*:*%%#v_\:28*:*:*%v>00p:0`\0\`-1v
++\1-:1`#^_$:28*:*:*/\28*vv_^#<<<!%*:*:*82:-1\-1\<<<\+**:*:*82<+>*:*:**\2-!#+
v"There are "0\g00+1%*:*:<>28*:*:*/\28*:*:*/:0\`28*:*:**+-:!00g^^82!:g01\p01<
>:#,_\." ,tneicifed">:#,_\." dna ,tcefrep">:#,_\.55+".srebmun tnadnuba">:#,_@
```


{{out}}


```txt
There are 15043 deficient, 4 perfect, and 4953 abundant numbers.
```



## Bracmat

Two solutions are given. The first solution first decomposes the current number into a multiset of prime factors and then constructs the proper divisors. The second solution finds proper divisors by checking all candidates from 1 up to the square root of the given number. The first solution is a few times faster, because establishing the prime factors of a small enough number (less than 2^32 or less than 2^64, depending on the bitness of Bracmat) is fast.

```bracmat
( clk$:?t0
& ( multiples
  =   prime multiplicity
    .     !arg:(?prime.?multiplicity)
        & !multiplicity:0
        & 1
      |   !prime^!multiplicity*(.!multiplicity)
        + multiples$(!prime.-1+!multiplicity)
  )
& ( P
  =   primeFactors prime exp poly S
    .   !arg^1/67:?primeFactors
      & ( !primeFactors:?^1/67&0
        |   1:?poly
          &   whl
            ' ( !primeFactors:%?prime^?exp*?primeFactors
              & !poly*multiples$(!prime.67*!exp):?poly
              )
          & -1+!poly+1:?poly
          & 1:?S
          & (   !poly
              :   ?
                + (#%@?s*?&!S+!s:?S&~)
                + ?
            | 1/2*!S
            )
        )
  )
& 0:?deficient:?perfect:?abundant
& 0:?n
&   whl
  ' ( 1+!n:~>20000:?n
    &   P$!n
      : ( <!n&1+!deficient:?deficient
        | !n&1+!perfect:?perfect
        | >!n&1+!abundant:?abundant
        )
    )
& out$(deficient !deficient perfect !perfect abundant !abundant)
& clk$:?t1
& out$(flt$(!t1+-1*!t0,2) sec)
& clk$:?t2
& ( P
  =   f h S
    .   0:?f
      & 0:?S
      &   whl
        ' ( 1+!f:?f
          & !f^2:~>!n
          & (   !arg*!f^-1:~/:?g
              & !S+!f:?S
              & ( !g:~!f&!S+!g:?S
                |
                )
            |
            )
          )
      & 1/2*!S
  )
& 0:?deficient:?perfect:?abundant
& 0:?n
&   whl
  ' ( 1+!n:~>20000:?n
    &   P$!n
      : ( <!n&1+!deficient:?deficient
        | !n&1+!perfect:?perfect
        | >!n&1+!abundant:?abundant
        )
    )
& out$(deficient !deficient perfect !perfect abundant !abundant)
& clk$:?t3
& out$(flt$(!t3+-1*!t2,2) sec)
);
```

Output:

```txt
deficient 15043 perfect 4 abundant 4953
4,27*10E0 sec
deficient 15043 perfect 4 abundant 4953
1,63*10E1 sec
```



## C


```c

#include<stdio.h>
#define de 0
#define pe 1
#define ab 2

int main(){
	int sum = 0, i, j;
	int try_max = 0;
	//1 is deficient by default and can add it deficient list
	int   count_list[3] = {1,0,0};
	for(i=2; i <= 20000; i++){
		//Set maximum to check for proper division
		try_max = i/2;
		//1 is in all proper division number
		sum = 1;
		for(j=2; j<try_max; j++){
			//Check for proper division
			if (i % j)
				continue; //Pass if not proper division
			//Set new maximum for divisibility check
			try_max = i/j;
			//Add j to sum
			sum += j;
			if (j != try_max)
				sum += try_max;
		}
		//Categorize summation
		if (sum < i){
			count_list[de]++;
			continue;
		}
		if (sum > i){
			count_list[ab]++;
			continue;
		}
		count_list[pe]++;
	}
	printf("\nThere are %d deficient," ,count_list[de]);
	printf(" %d perfect," ,count_list[pe]);
	printf(" %d abundant numbers between 1 and 20000.\n" ,count_list[ab]);
return 0;
}

```

{{out}}

```txt

There are 15043 deficient, 4 perfect, 4953 abundant numbers between 1 and 20000.

```



## C sharp


```csharp
using System;
using System.Linq;

public class Program
{
    public static void Main()
    {
        int abundant, deficient, perfect;
        ClassifyNumbers.UsingSieve(20000, out abundant, out deficient, out perfect);
        Console.WriteLine($"Abundant: {abundant}, Deficient: {deficient}, Perfect: {perfect}");

        ClassifyNumbers.UsingDivision(20000, out abundant, out deficient, out perfect);
        Console.WriteLine($"Abundant: {abundant}, Deficient: {deficient}, Perfect: {perfect}");
    }
}

public static class ClassifyNumbers
{
    //Fastest way
    public static void UsingSieve(int bound, out int abundant, out int deficient, out int perfect) {
        int a = 0, d = 0, p = 0;
        //For very large bounds, this array can get big.
        int[] sum = new int[bound + 1];
        for (int divisor = 1; divisor <= bound / 2; divisor++) {
            for (int i = divisor + divisor; i <= bound; i += divisor) {
                sum[i] += divisor;
            }
        }
        for (int i = 1; i <= bound; i++) {
            if (sum[i] < i) d++;
            else if (sum[i] > i) a++;
            else p++;
        }
        abundant = a;
        deficient = d;
        perfect = p;
    }

    //Much slower, but doesn't use storage
    public static void UsingDivision(int bound, out int abundant, out int deficient, out int perfect) {
        int a = 0, d = 0, p = 0;
        for (int i = 1; i < 20001; i++) {
            int sum = Enumerable.Range(1, (i + 1) / 2)
                .Where(div => div != i && i % div == 0).Sum();
            if (sum < i) d++;
            else if (sum > i) a++;
            else p++;
        }
        abundant = a;
        deficient = d;
        perfect = p;
    }
}
```

{{out}}

```txt

Abundant: 4953, Deficient: 15043, Perfect: 4
Abundant: 4953, Deficient: 15043, Perfect: 4

```



## C++


```cpp
#include <iostream>
#include <algorithm>
#include <vector>

std::vector<int> findProperDivisors ( int n ) {
   std::vector<int> divisors ;
   for ( int i = 1 ; i < n / 2 + 1 ; i++ ) {
      if ( n % i == 0 )
	 divisors.push_back( i ) ;
   }
   return divisors  ;
}

int main( ) {
   std::vector<int> deficients , perfects , abundants , divisors ;
   for ( int n = 1 ; n < 20001 ; n++ ) {
      divisors = findProperDivisors( n ) ;
      int sum = std::accumulate( divisors.begin( ) , divisors.end( ) , 0 ) ;
      if ( sum < n ) {
	 deficients.push_back( n ) ;
      }
      if ( sum == n ) {
	 perfects.push_back( n ) ;
      }
      if ( sum > n ) {
	 abundants.push_back( n ) ;
      }
   }
   std::cout << "Deficient : " << deficients.size( ) << std::endl ;
   std::cout << "Perfect   : " << perfects.size( ) << std::endl ;
   std::cout << "Abundant  : " << abundants.size( ) << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
Deficient : 15043
Perfect   : 4
Abundant  : 4953

```



## Ceylon


```ceylon
shared void run() {

	function divisors(Integer int) =>
			if(int <= 1) then {} else (1..int / 2).filter((Integer element) => element.divides(int));

	function classify(Integer int) => sum {0, *divisors(int)} <=> int;

	value counts = (1..20k).map(classify).frequencies();

	print("deficient: ``counts[smaller] else "none"``");
	print("perfect:   ``counts[equal] else "none"``");
	print("abundant:  ``counts[larger] else "none"``");
}
```

{{out}}

```txt

deficient: 15043
perfect:   4
abundant:  4953
```



## Clojure


```clojure
(defn pad-class
  [n]
  (let [divs (filter #(zero? (mod n %)) (range 1 n))
        divs-sum (reduce + divs)]
    (cond
      (< divs-sum n) :deficient
      (= divs-sum n) :perfect
      (> divs-sum n) :abundant)))

(def pad-classes (map pad-class (map inc (range))))

(defn count-classes
  [n]
  (let [classes (take n pad-classes)]
    {:perfect (count (filter #(= % :perfect) classes))
     :abundant (count (filter #(= % :abundant) classes))
     :deficient (count (filter #(= % :deficient) classes))}))
```


Example:


```clojure
(count-classes 20000)
;=> {:perfect 4,
;    :abundant 4953,
;    :deficient 15043}
```



## Common Lisp



```lisp
(defun number-class (n)
  (let ((divisor-sum (sum-divisors n)))
    (cond ((< divisor-sum n) :deficient)
          ((= divisor-sum n) :perfect)
          ((> divisor-sum n) :abundant))))

(defun sum-divisors (n)
  (loop :for i :from 1 :to (/ n 2)
        :when (zerop (mod n i))
        :sum i))

(defun classification ()
  (loop :for n :from 1 :to 20000
        :for class := (number-class n)
        :count (eq class :deficient) :into deficient
        :count (eq class :perfect) :into perfect
        :count (eq class :abundant) :into abundant
        :finally (return (values deficient perfect abundant))))
```


Output:


```txt
CL-USER> (classification)
15043
4
4953
```



## D


```d
void main() /*@safe*/ {
    import std.stdio, std.algorithm, std.range;

    static immutable properDivs = (in uint n) pure nothrow @safe /*@nogc*/ =>
        iota(1, (n + 1) / 2 + 1).filter!(x => n % x == 0 && n != x);

    enum Class { deficient, perfect, abundant }

    static Class classify(in uint n) pure nothrow @safe /*@nogc*/ {
        immutable p = properDivs(n).sum;
        with (Class)
            return (p < n) ? deficient : ((p == n) ? perfect : abundant);
    }

    enum rangeMax = 20_000;
    //iota(1, 1 + rangeMax).map!classify.hashGroup.writeln;
    iota(1, 1 + rangeMax).map!classify.array.sort().group.writeln;
}
```

{{out}}

```txt
[Tuple!(Class, uint)(deficient, 15043), Tuple!(Class, uint)(perfect, 4), Tuple!(Class, uint)(abundant, 4953)]
```



## Dyalect


{{trans|C#}}


```dyalect
func sieve(bound) {
    var (a, d, p) = (0, 0, 0)
    var sum = Array.empty(bound + 1, 0)

    for divisor in 1..(bound / 2) {
        var i = divisor + divisor
        while i <= bound {
            sum[i] += divisor
            i += divisor
        }
    }
    for i in 1..bound {
        if sum[i] < i {
            d += 1
        } else if sum[i] > i {
            a += 1
        } else {
            p += 1
        }
    }

    (abundant: a, deficient: d, perfect: p)
}

func division(bound) {
    func Iterator.where(fn) {
        for x in this {
            if fn(x) {
                yield x
            }
        }
    }
    func Iterator.sum() {
        var sum = 0
        for x in this {
            sum += x
        }
        return sum
    }
    var (a, d, p) = (0, 0, 0)
    for i in 1..20000 {
        var sum = ( 1 .. ((i + 1) / 2) )
            .where(div => div != i && i % div == 0)
            .sum()
        if sum < i {
            d += 1
        } else if sum > i {
            a += 1
        } else {
            p += 1
        }
    }

    (abundant: a, deficient: d, perfect: p)
}

func out(res) {
    print("Abundant: \(res.abundant), Deficient: \(res.deficient), Perfect: \(res.perfect)");
}

out( sieve(20000) )
out( division(20000) )
```


{{out}}


```txt
Abundant: 4953, Deficient: 15043, Perfect: 4
Abundant: 4953, Deficient: 15043, Perfect: 4
```



## EchoLisp


```scheme

(lib 'math) ;; sum-divisors function

(define-syntax-rule (++ a) (set! a (1+ a)))

(define (abondance (N 20000))
    (define-values (delta abondant deficient perfect) '(0 0 0 0))
    (for ((n (in-range 1 (1+ N))))
	 (set! delta (- (sum-divisors n) n))
	 (cond
	 	((< delta 0) (++ deficient))
	 	((> delta 0) (++ abondant))
	 	(else (writeln 'perfect→ n) (++ perfect))))

	(printf "In range 1.. %d" N)
	(for-each (lambda(x) (writeln x (eval x))) '(abondant deficient perfect)))

(abondance)
    perfect→     6
    perfect→     28
    perfect→     496
    perfect→     8128
    In range 1.. 20000
    abondant     4953
    deficient     15043
    perfect     4

```



## Ela

{{trans|Haskell}}


```ela
open monad io number list

divisors n = filter ((0 ==) << (n `mod`)) [1 .. (n `div` 2)]
classOf n = compare (sum $ divisors n) n

do
  let classes = map classOf [1 .. 20000]
  let printRes w c = putStrLn $ w ++ (show << length $ filter (== c) classes)
  printRes "deficient: " LT
  printRes "perfect:   " EQ
  printRes "abundant:  " GT
```


{{out}}

```txt
deficient: 15043
perfect:   4
abundant:  4953
```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import extensions;

classifyNumbers(int bound, ref int abundant, ref int deficient, ref int perfect)
{
    int a := 0;
    int d := 0;
    int p := 0;
    int[] sum := new int[](bound + 1);

    for(int divisor := 1, divisor <= bound / 2, divisor += 1)
    {
        for(int i := divisor + divisor, i <= bound, i += divisor)
        {
            sum[i] := sum[i] + divisor
        }
    };

    for(int i := 1, i <= bound, i += 1)
    {
        int t := sum[i];

        if (sum[i]<i)
        {
            d += 1
        }
        else
        {
            if (sum[i]>i)
            {
                a += 1
            }
            else
            {
                p += 1
            }
        }
    };

    abundant := a;
    deficient := d;
    perfect := p
}

public program()
{
    int abundant := 0;
    int deficient := 0;
    int perfect := 0;
    classifyNumbers(20000, ref abundant, ref deficient, ref perfect);
    console.printLine("Abundant: ",abundant,", Deficient: ",deficient,", Perfect: ",perfect)
}
```

{{out}}

```txt

Abundant: 4953, Deficient: 15043, Perfect: 4

```



## Elixir


```elixir
defmodule Proper do
  def divisors(1), do: []
  def divisors(n), do: [1 | divisors(2,n,:math.sqrt(n))] |> Enum.sort

  defp divisors(k,_n,q) when k>q, do: []
  defp divisors(k,n,q) when rem(n,k)>0, do: divisors(k+1,n,q)
  defp divisors(k,n,q) when k * k == n, do: [k | divisors(k+1,n,q)]
  defp divisors(k,n,q)                , do: [k,div(n,k) | divisors(k+1,n,q)]
end

{abundant, deficient, perfect} = Enum.reduce(1..20000, {0,0,0}, fn n,{a, d, p} ->
  sum = Proper.divisors(n) |> Enum.sum
  cond do
    n < sum -> {a+1, d, p}
    n > sum -> {a, d+1, p}
    true    -> {a, d, p+1}
  end
end)
IO.puts "Deficient: #{deficient}   Perfect: #{perfect}   Abundant: #{abundant}"
```


{{out}}

```txt

Deficient: 15043   Perfect: 4   Abundant: 4953

```



## Erlang


```erlang

-module(properdivs).
-export([divs/1,sumdivs/1,class/1]).

divs(0) -> [];
divs(1) -> [];
divs(N) -> lists:sort(divisors(1,N)).

divisors(1,N) ->
      divisors(2,N,math:sqrt(N),[1]).

divisors(K,_N,Q,L) when K > Q -> L;
divisors(K,N,_Q,L) when N rem K =/= 0 ->
    divisors(K+1,N,_Q,L);
divisors(K,N,_Q,L) when K * K  =:= N ->
    divisors(K+1,N,_Q,[K|L]);
divisors(K,N,_Q,L) ->
    divisors(K+1,N,_Q,[N div K, K|L]).

sumdivs(N) -> lists:sum(divs(N)).

class(Limit) -> class(0,0,0,sumdivs(2),2,Limit).

class(D,P,A,_Sum,Acc,L) when Acc > L +1->
    io:format("Deficient: ~w, Perfect: ~w, Abundant: ~w~n", [D,P,A]);

class(D,P,A,Sum,Acc,L) when Acc < Sum ->
       class(D,P,A+1,sumdivs(Acc+1),Acc+1,L);
class(D,P,A,Sum,Acc,L) when Acc == Sum ->
       class(D,P+1,A,sumdivs(Acc+1),Acc+1,L);
class(D,P,A,Sum,Acc,L) when Acc > Sum  ->
       class(D+1,P,A,sumdivs(Acc+1),Acc+1,L).

```


{{out}}

```txt

24> c(properdivs).
{ok,properdivs}
25> properdivs:class(20000).
Deficient: 15043, Perfect: 4, Abundant: 4953
ok

```


The above divisors method was slightly rewritten to satisfy the observation below but preserve the different programming style.
Now has comparable performance.


### Erlang 2

The version above is not tail-call recursive, and so cannot classify large ranges. Here is a more optimal solution.

```erlang

-module(proper_divisors).
-export([classify_range/2]).

classify_range(Start, Stop) ->
    lists:foldl(fun (X, A) ->
                  Class = classify(X),
                  A#{Class => maps:get(Class, A, 0)+1} end,
                #{},
                lists:seq(Start, Stop)).

classify(N) ->
    SumPD = lists:sum(proper_divisors(N)),
    if
        SumPD  <  N -> deficient;
        SumPD =:= N -> perfect;
        SumPD  >  N -> abundant
    end.

proper_divisors(1) -> [];
proper_divisors(N) when N > 1, is_integer(N) ->
    proper_divisors(2, math:sqrt(N), N, [1]).

proper_divisors(I, L, _, A) when I > L -> lists:sort(A);
proper_divisors(I, L, N, A) when N rem I =/= 0 ->
    proper_divisors(I+1, L, N, A);
proper_divisors(I, L, N, A) when I * I =:= N ->
    proper_divisors(I+1, L, N, [I|A]);
proper_divisors(I, L, N, A) ->
    proper_divisors(I+1, L, N, [N div I, I|A]).

```

{{output}}

```txt

8>proper_divisors:classify_range(1,20000).
#{abundant => 4953,deficient => 15043,perfect => 4}

```



## F#


```F#

let mutable a=0
let mutable b=0
let mutable c=0
let mutable d=0
let mutable e=0
let mutable f=0
for i=1 to 20000 do
    b <- 0
    f <- i/2
    for j=1 to f do
        if i%j=0 then
           b <- b+i
    if b<i then
       c <- c+1
    if b=i then
       d <- d+1
    if b>i then
       e <- e+1
printfn " deficient %i"c
printfn "perfect %i"d
printfn "abundant %i"e

```


An immutable solution.

```fsharp

let deficient, perfect, abundant = 0,1,2

let classify n = ([1..n/2] |> List.filter (fun x->n % x = 0) |> List.sum) |> function
  | x when x<n -> deficient | x when x>n -> abundant | _ -> perfect

let incClass xs n =
  let cn = n |> classify
  xs |> List.mapi (fun i x->if i=cn then x + 1 else x)

[1..20000]
|> List.fold incClass [0;0;0]
|> List.zip [ "deficient"; "perfect"; "abundant" ]
|> List.iter (fun (label, count) -> printfn "%s: %d" label count)

```



## Factor


```factor

USING: fry math.primes.factors math.ranges ;
: psum     ( n -- m )   divisors but-last sum ;
: pcompare ( n -- <=> ) dup psum swap <=> ;
: classify ( -- seq )   20,000 [1,b] [ pcompare ] map ;
: pcount   ( <=> -- n ) '[ _ = ] count ;
classify [ +lt+ pcount "Deficient: " write . ]
         [ +eq+ pcount "Perfect: "   write . ]
         [ +gt+ pcount "Abundant: "  write . ] tri

```

{{out}}

```txt

Deficient: 15043
Perfect: 4
Abundant: 4953

```



## Forth

{{works with|Gforth|0.7.3}}

```Forth
CREATE A 0 ,
: SLOT ( x y -- 0|1|2)  OVER OVER < -ROT > -  1+ ;
: CLASSIFY ( n -- n')  \ 0 == deficient, 1 == perfect, 2 == abundant
   DUP A !  \ we'll be accessing this often, so save somewhere convenient
   2 / >R   \ upper bound
   1        \ starting sum, 1 is always a divisor
   2        \ current check
   BEGIN DUP R@ < WHILE
     A @ OVER /MOD SWAP ( s c d m)
     IF DROP ELSE
       R> DROP DUP >R  ( R: d n)
       OVER TUCK OVER <> * -  ( s c c+?d)
       ROT + SWAP ( s' c)
     THEN 1+
   REPEAT  DROP R> DROP A @  ( sum n)  SLOT ;
CREATE COUNTS 0 , 0 , 0 ,
: INIT   COUNTS 3 CELLS ERASE  1 COUNTS ! ;
: CLASSIFY-NUMBERS ( n --)  INIT
   BEGIN DUP WHILE
     1 OVER CLASSIFY  CELLS COUNTS + +!  1-
   REPEAT  DROP ;
: .COUNTS
   ." Deficient : " [ COUNTS ]L           @ . CR
   ." Perfect   : " [ COUNTS 1 CELLS + ]L @ . CR
   ." Abundant  : " [ COUNTS 2 CELLS + ]L @ . CR ;
20000 CLASSIFY-NUMBERS .COUNTS BYE
```

{{out}}

```txt
Deficient : 15043
Perfect   : 5
Abundant  : 4953
```



## Fortran

Although Fortran offers an intrinsic function SIGN(a,b) which returns the absolute value of ''a'' with the sign of ''b'', it does '''not''' recognise zero as a special case, instead distinguishing only the two conditions b < 0 and b >= 0. Rather than a mess such as SIGN(a*b,b), a suitable SIGN3 function is needed. For it to be acceptable in whole-array expressions, it must have the PURE attribute asserted (signifying that it it may be treated as having a value dependent only on its explicit parameters) and further, that parameters must be declared with the (verbose) new protocol that enables the use of INTENT(IN) as further assurance to the compiler. Finally, such a function must be associated with INTERFACE arrangements, easily done here merely by placing it within a MODULE.

Alternatively, an explicit DO-loop could simply inspect the KnownSum array and maintain three counts, moreover, doing so in a single pass rather than the three passes needed for the three COUNT statements.

Output:
 Inspecting sums of proper divisors for 1 to       20000
 Deficient       15043
 Perfect!            4
 Abundant         4953


```Fortran

      MODULE FACTORSTUFF	!This protocol evades the need for multiple parameters, or COMMON, or one shapeless main line...
Concocted by R.N.McLean, MMXV.
       INTEGER LOTS		!The span..
       PARAMETER (LOTS = 20000)!Nor is computer storage infinite.
       INTEGER KNOWNSUM(LOTS)	!Calculate these once.
       CONTAINS		!Assistants.
        SUBROUTINE PREPARESUMF	!Initialise the KNOWNSUM array.
Convert the Sieve of Eratoshenes to have each slot contain the sum of the proper divisors of its slot number.
Changes to instead count the number of factors, or prime factors, etc. would be simple enough.
         INTEGER F		!A factor for numbers such as 2F, 3F, 4F, 5F, ...
          KNOWNSUM(1) = 0		!Proper divisors of N do not include N.
          KNOWNSUM(2:LOTS) = 1		!So, although 1 divides all N without remainder, 1 is excluded for itself.
          DO F = 2,LOTS/2		!Step through all the possible divisors of numbers not exceeding LOTS.
            FORALL(I = F + F:LOTS:F) KNOWNSUM(I) = KNOWNSUM(I) + F	!And augment each corresponding slot.
          END DO			!Different divisors can hit the same slot. For instance, 6 by 2 and also by 3.
        END SUBROUTINE PREPARESUMF	!Could alternatively generate all products of prime numbers.
         PURE INTEGER FUNCTION SIGN3(N)	!Returns -1, 0, +1 according to the sign of N.
Confounded by the intrinsic function SIGN distinguishing only two states: < 0 from >= 0. NOT three-way.
         INTEGER, INTENT(IN):: N	!The number.
          IF (N) 1,2,3	!A three-way result calls for a three-way test.
    1     SIGN3 = -1	!Negative.
          RETURN
    2     SIGN3 = 0	!Zero.
          RETURN
    3     SIGN3 = +1	!Positive.
        END FUNCTION SIGN3	!Rather basic.
      END MODULE FACTORSTUFF	!Enough assistants.
       PROGRAM THREEWAYS	!Classify N against the sum of proper divisors of N, for N up to 20,000.
       USE FACTORSTUFF		!This should help.
       INTEGER I		!Stepper.
       INTEGER TEST(LOTS)	!Assesses the three states in one pass.
        WRITE (6,*) "Inspecting sums of proper divisors for 1 to",LOTS
        CALL PREPARESUMF		!Values for every N up to the search limit will be called for at least once.
        FORALL(I = 1:LOTS) TEST(I) = SIGN3(KNOWNSUM(I) - I)	!How does KnownSum(i) compare to i?
        WRITE (6,*) "Deficient",COUNT(TEST .LT. 0)	!This means one pass through the array
        WRITE (6,*) "Perfect! ",COUNT(TEST .EQ. 0)	!For each of three types.
        WRITE (6,*) "Abundant ",COUNT(TEST .GT. 0)	!Alternatively, make one pass with three counts.
      END			!Done.

```



## FreeBASIC


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

Dim As Integer sum, deficient, perfect, abundant

For n As Integer = 1 To 20000
  sum = SumProperDivisors(n)
  If sum < n Then
    deficient += 1
  ElseIf sum = n Then
    perfect += 1
  Else
    abundant += 1
  EndIf
Next

Print "The classification of the numbers from 1 to 20,000 is as follows : "
Print
Print "Deficient = "; deficient
Print "Perfect   = "; perfect
Print "Abundant  = "; abundant
Print
Print "Press any key to exit the program"
Sleep
End

```


{{out}}

```txt

The classification of the numbers from 1 to 20,000 is as follows :

Deficient =  15043
Perfect   =  4
Abundant  =  4953

```



## Frink


```frink

d = new	dict
for n =	1 to 20000
{
   s = sum[allFactors[n, true, false, true], 0]
   rel = s <=> n
   d.increment[rel, 1]
}

println["Deficient: " + d@(-1)]
println["Perfect:   " + d@0]
println["Abundant:  " + d@1]

```

{{out}}

```txt

Deficient: 15043
Perfect:   4
Abundant:  4953

```


## GFA Basic


<lang>
num_deficient%=0
num_perfect%=0
num_abundant%=0
'
FOR current%=1 TO 20000
  sum_divisors%=@sum_proper_divisors(current%)
  IF sum_divisors%<current%
    num_deficient%=num_deficient%+1
  ELSE IF sum_divisors%=current%
    num_perfect%=num_perfect%+1
  ELSE ! sum_divisors%>current%
    num_abundant%=num_abundant%+1
  ENDIF
NEXT current%
'
' Display results on a window
'
OPENW 1
CLEARW 1
PRINT "Number deficient ";num_deficient%
PRINT "Number perfect   ";num_perfect%
PRINT "Number abundant  ";num_abundant%
~INP(2)
CLOSEW 1
'
' Compute the sum of proper divisors of given number
'
FUNCTION sum_proper_divisors(n%)
  LOCAL i%,sum%,root%
  '
  IF n%>1 ! n% must be 2 or higher
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

Number deficient 15043
Number perfect   4
Number abundant  4953

```



## Go


```go
package main

import "fmt"

func pfacSum(i int) int {
    sum := 0
    for p := 1; p <= i/2; p++ {
        if i%p == 0 {
            sum += p
        }
    }
    return sum
}

func main() {
    var d, a, p = 0, 0, 0
    for i := 1; i <= 20000; i++ {
        j := pfacSum(i)
        if j < i {
            d++
        } else if j == i {
            p++
        } else {
            a++
        }
    }
    fmt.Printf("There are %d deficient numbers between 1 and 20000\n", d)
    fmt.Printf("There are %d abundant numbers  between 1 and 20000\n", a)
    fmt.Printf("There are %d perfect numbers between 1 and 20000\n", p)
}
```


{{out}}

```txt

There are 15043 deficient numbers between 1 and 20000
There are 4953 abundant numbers  between 1 and 20000
There are 4 perfect numbers between 1 and 20000

```



## Groovy


### ==Solution:==

Uses the "factorize" closure from [[Factors of an integer]]

```Groovy
def dpaCalc = { factors ->
    def n = factors.pop()
    def fSum = factors.sum()
    fSum < n
        ? 'deficient'
        : fSum > n
            ? 'abundant'
            : 'perfect'
}

(1..20000).inject([deficient:0, perfect:0, abundant:0]) { map, n ->
    map[dpaCalc(factorize(n))]++
    map
}
.each { e -> println e }
```

{{out}}

```txt
deficient=15043
perfect=4
abundant=4953
```


## Haskell


```Haskell
divisors :: (Integral a) => a -> [a]
divisors n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

classOf :: (Integral a) => a -> Ordering
classOf n = compare (sum $ divisors n) n

main :: IO ()
main = do
  let classes = map classOf [1 .. 20000 :: Int]
      printRes w c = putStrLn $ w ++ (show . length $ filter (== c) classes)
  printRes "deficient: " LT
  printRes "perfect:   " EQ
  printRes "abundant:  " GT
```

{{out}}

```txt
deficient: 15043
perfect:   4
abundant:  4953
```



## J

[[Proper divisors#J|Supporting implementation]]:


```J
factors=: [: /:~@, */&>@{@((^ i.@>:)&.>/)@q:~&__
properDivisors=: factors -. ]
```


We can subtract the sum of a number's proper divisors from itself to classify the number:


```J
   (- +/@properDivisors&>) 1+i.10
1 1 2 1 4 0 6 1 5 2
```


Except, we are only concerned with the sign of this difference:


```J
   *(- +/@properDivisors&>) 1+i.30
1 1 1 1 1 0 1 1 1 1 1 _1 1 1 1 1 1 _1 1 _1 1 1 1 _1 1 1 1 0 1 _1
```


Also, we do not care about the individual classification but only about how many numbers fall in each category:


```J
   #/.~ *(- +/@properDivisors&>) 1+i.20000
15043 4 4953
```


So: 15043 deficient, 4 perfect and 4953 abundant numbers in this range.

How do we know which is which? We look at the unique values (which are arranged by their first appearance, scanning the list left to right):


```J
   ~. *(- +/@properDivisors&>) 1+i.20000
1 0 _1
```


The sign of the difference is negative for the abundant case - where the sum is greater than the number. And we rely on order being preserved in sequences (this happens to be a fundamental property of computer memory, also).


## Java

{{works with|Java|8}}

```java
import java.util.stream.LongStream;

public class NumberClassifications {

    public static void main(String[] args) {
        int deficient = 0;
        int perfect = 0;
        int abundant = 0;

        for (long i = 1; i <= 20_000; i++) {
            long sum = properDivsSum(i);
            if (sum < i)
                deficient++;
            else if (sum == i)
                perfect++;
            else
                abundant++;
        }
        System.out.println("Deficient: " + deficient);
        System.out.println("Perfect: " + perfect);
        System.out.println("Abundant: " + abundant);
    }

    public static long properDivsSum(long n) {
        return LongStream.rangeClosed(1, (n + 1) / 2).filter(i -> n != i && n % i == 0).sum();
    }
}
```



```txt
Deficient: 15043
Perfect: 4
Abundant: 4953
```



## JavaScript



### ES5


```Javascript
for (var dpa=[1,0,0], n=2; n<=20000; n+=1) {
    for (var ds=0, d=1, e=n/2+1; d<e; d+=1) if (n%d==0) ds+=d
    dpa[ds<n ? 0 : ds==n ? 1 : 2]+=1
}
document.write('Deficient:',dpa[0], ', Perfect:',dpa[1], ', Abundant:',dpa[2], '
' )
```

'''Or:'''

```Javascript
for (var dpa=[1,0,0], n=2; n<=20000; n+=1) {
    for (var ds=1, d=2, e=Math.sqrt(n); d<e; d+=1) if (n%d==0) ds+=d+n/d
    if (n%e==0) ds+=e
    dpa[ds<n ? 0 : ds==n ? 1 : 2]+=1
}
document.write('Deficient:',dpa[0], ', Perfect:',dpa[1], ', Abundant:',dpa[2], '
' )
```

'''Or:'''

```Javascript
function primes(t) {
    var ps = {2:true, 3:true}
    next: for (var n=5, i=2; n<=t; n+=i, i=6-i) {
        var s = Math.sqrt( n )
        for ( var p in ps ) {
            if ( p > s ) break
            if ( n % p ) continue
            continue next
        }
        ps[n] = true
    }
    return ps
}

function factorize(f, t) {
    var cs = {}, ps = primes(t)
    for (var n=f; n<=t; n++) if (!ps[n]) cs[n] = factors(n)
    return cs
    function factors(n) {
        for ( var p in ps ) if ( n % p == 0 ) break
        var ts = {}
        ts[p] = 1
        if ( ps[n /= p] ) {
            if ( !ts[n]++ ) ts[n]=1
        }
        else {
            var fs = cs[n]
            if ( !fs ) fs = cs[n] = factors(n)
            for ( var e in fs ) ts[e] = fs[e] + (e==p)
        }
        return ts
    }
}

function pContrib(p, e) {
    for (var pc=1, n=1, i=1; i<=e; i+=1) pc+=n*=p;
    return pc
}

for (var dpa=[1,0,0], t=20000, cs=factorize(2,t), n=2; n<=t; n+=1) {
    var ds=1, fs=cs[n]
    if (fs) {
        for (var p in fs) ds *= pContrib(p, fs[p])
        ds -= n
    }
    dpa[ds<n ? 0 : ds==n ? 1 : 2]+=1
}
document.write('Deficient:',dpa[0], ', Perfect:',dpa[1], ', Abundant:',dpa[2], '
' )
```

{{output}}

```txt
Deficient:15043, Perfect:4, Abundant:4953
```



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    const
    // divisors :: (Integral a) => a -> [a]
        divisors = n => range(1, Math.floor(n / 2))
            .filter(x => n % x === 0),

        // classOf :: (Integral a) => a -> Ordering
        classOf = n => compare(divisors(n)
            .reduce((a, b) => a + b, 0), n),

        classTypes = {
            deficient: -1,
            perfect: 0,
            abundant: 1
        };

    // GENERIC FUNCTIONS
    const
    // compare :: Ord a => a -> a -> Ordering
        compare = (a, b) =>
            a < b ? -1 : (a > b ? 1 : 0),

        // range :: Int -> Int -> [Int]
        range = (m, n) =>
            Array.from({
                length: Math.floor(n - m) + 1
            }, (_, i) => m + i);

    // TEST

    // classes :: [Ordering]
    const classes = range(1, 20000)
        .map(classOf);

    return Object.keys(classTypes)
        .map(k => k + ": " + classes
            .filter(x => x === classTypes[k])
            .length.toString())
        .join('\n');
})();
```


{{Out}}

```txt
deficient: 15043
perfect: 4
abundant: 4953
```



## Jsish

From Javascript ES5 entry.


```javascript
/* Classify Deficient, Perfect and Abdundant integers */
function classifyDPA(stop:number, start:number=0, step:number=1):array {
    var dpa = [1, 0, 0];
    for (var n=start; n<=stop; n+=step) {
        for (var ds=0, d=1, e=n/2+1; d<e; d+=1) if (n%d == 0) ds += d;
        dpa[ds < n ? 0 : ds==n ? 1 : 2] += 1;
    }
    return dpa;
}

var dpa = classifyDPA(20000, 2);
printf('Deficient: %d, Perfect: %d, Abundant: %d\n', dpa[0], dpa[1], dpa[2]);
```


{{out}}

```txt
prompt$ jsish classifyDPA.jsi
Deficient: 15043, Perfect: 4, Abundant: 4953
```



## Julia


This post was created with <code>Julia</code> version <code>0.3.6</code>.  The code uses no exotic features and should work for a wide range of <code>Julia</code> versions.

'''The Math'''

A natural number can be written as a product of powers of its prime factors,
<math>
\prod_{i} p_{i}^{a_{i}}
</math>.  Handily <code>Julia</code> has the <code>factor</code> function, which provides these parameters.  The sum of n's divisors (n inclusive) is
<math>
\prod_{i} \frac{p_{i}^{a_{i}+1} - 1}{p_{i} - 1} = \prod_{i} p_{i}^{a_{i}} + p_{i}^{a_{i}-1} + \cdots + p_{i} + 1
</math>.

'''Functions'''

<code>divisorsum</code> calculates the sum of aliquot divisors.  It uses <code>pcontrib</code> to calculate the contribution of each prime factor.


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

Perhaps <code>pcontrib</code> could be made more efficient by caching results to avoid repeated calculations.

'''Main'''

Use a three element array, <code>iclass</code>, rather than three separate variables to tally the classifications. Take advantage of the fact that the sign of <code>divisorsum(n) - n</code> depends upon its class to increment <code>iclass</code>.  1 is a difficult case, it is deficient by convention, so I manually add its contribution and start the accumulation with 2.  All primes are deficient, so I test for those and tally accordingly, bypassing <code>divisorsum</code>.


```Julia

const L = 2*10^4
iclasslabel = ["Deficient", "Perfect", "Abundant"]
iclass = zeros(Int64, 3)
iclass[1] = one(Int64) #by convention 1 is deficient

for n in 2:L
    if isprime(n)
        iclass[1] += 1
    else
        iclass[sign(divisorsum(n)-n)+2] += 1
    end
end

println("Classification of integers from 1 to ", L)
for i in 1:3
    println("   ", iclasslabel[i], ", ", iclass[i])
end

```


{{out}}
<code>
   Classification of integers from 1 to 20000
      Deficient, 15043
      Perfect, 4
      Abundant, 4953
</code>


## jq

{{works with|jq|1.4}}
The definition of proper_divisors is taken from [[Proper_divisors#jq]]:

```jq
# unordered
def proper_divisors:
  . as $n
  | if $n > 1 then 1,
      ( range(2; 1 + (sqrt|floor)) as $i
        | if ($n % $i) == 0 then $i,
            (($n / $i) | if . == $i then empty else . end)
	  else empty
	  end)
    else empty
    end;
```

'''The task:'''

```jq
def sum(stream): reduce stream as $i (0; . + $i);

def classify:
  . as $n
  | sum(proper_divisors)
  | if . < $n then "deficient" elif . == $n then "perfect" else "abundant" end;

reduce (range(1; 20001) | classify) as $c ({}; .[$c] += 1 )
```

{{out}}

```sh
$ jq -n -c -f AbundantDeficientPerfect.jq
{"deficient":15043,"perfect":4,"abundant":4953}
```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.1

fun sumProperDivisors(n: Int) =
    if (n < 2) 0 else (1..n / 2).filter { (n % it) == 0 }.sum()

fun main(args: Array<String>) {
    var sum: Int
    var deficient = 0
    var perfect = 0
    var abundant = 0

    for (n in 1..20000) {
        sum = sumProperDivisors(n)
        when {
            sum < n -> deficient++
            sum == n -> perfect++
            sum > n -> abundant++
        }
    }

    println("The classification of the numbers from 1 to 20,000 is as follows:\n")
    println("Deficient = $deficient")
    println("Perfect   = $perfect")
    println("Abundant  = $abundant")
}
```


{{out}}

```txt

The classification of the numbers from 1 to 20,000 is as follows:

Deficient = 15043
Perfect   = 4
Abundant  = 4953

```



## K


```K

/Classification of numbers into abundant, perfect and deficient
/ numclass.k

/return 0,1 or -1 if perfect or abundant or deficient respectively
numclass: {s:(+/&~x!'!1+x)-x; :[s>x;:1;:[s<x;:-1;:0]]}
/classify numbers from 1 to 20000 into respective groups
c: =numclass' 1+!20000
/print statistics
`0: ,"Deficient = ", $(#c[0])
`0: ,"Perfect   = ", $(#c[1])
`0: ,"Abundant  = ", $(#c[2])

```

{{out}}

```txt

Deficient = 15043
Perfect   = 4
Abundant  = 4953


```



## Liberty BASIC


```lb

print "ROSETTA CODE - Abundant, deficient and perfect number classifications"
print
for x=1 to 20000
    x$=NumberClassification$(x)
    select case x$
        case "deficient": de=de+1
        case "perfect": pe=pe+1: print x; " is a perfect number"
        case "abundant": ab=ab+1
    end select
    select case x
        case 2000: print "Checking the number classifications of 20,000 integers..."
        case 4000: print "Please be patient."
        case 7000: print "7,000"
        case 10000: print "10,000"
        case 12000: print "12,000"
        case 14000: print "14,000"
        case 16000: print "16,000"
        case 18000: print "18,000"
        case 19000: print "Almost done..."
    end select
next x
print "Deficient numbers = "; de
print "Perfect numbers = "; pe
print "Abundant numbers = "; ab
print "TOTAL = "; pe+de+ab
[Quit]
print "Program complete."
end

function NumberClassification$(n)
    x=ProperDivisorCount(n)
    for y=1 to x
        PDtotal=PDtotal+ProperDivisor(y)
    next y
    if PDtotal=n then NumberClassification$="perfect": exit function
    if PDtotal<n then NumberClassification$="deficient": exit function
    if PDtotal>n then NumberClassification$="abundant": exit function
end function

function ProperDivisorCount(n)
    n=abs(int(n)): if n=0 or n>20000 then exit function
    dim ProperDivisor(100)
    for y=2 to n
        if (n mod y)=0 then
            ProperDivisorCount=ProperDivisorCount+1
            ProperDivisor(ProperDivisorCount)=n/y
        end if
    next y
end function

```

{{out}}

```txt

ROSETTA CODE - Abundant, deficient and perfect number classifications

6 is a perfect number
28 is a perfect number
496 is a perfect number
Checking the number classifications of 20,000 integers...
Please be patient.
7,000
8128 is a perfect number
10,000
12,000
14,000
16,000
18,000
Almost done...
Deficient numbers = 15043
Perfect numbers = 4
Abundant numbers = 4953
TOTAL = 20000
Program complete.

```



## Lua


```Lua
function sumDivs (n)
    if n < 2 then return 0 end
    local sum, sr = 1, math.sqrt(n)
    for d = 2, sr do
        if n % d == 0 then
            sum = sum + d
            if d ~= sr then sum = sum + n / d end
        end
    end
    return sum
end

local a, d, p, Pn = 0, 0, 0
for n = 1, 20000 do
    Pn = sumDivs(n)
    if Pn > n then a = a + 1 end
    if Pn < n then d = d + 1 end
    if Pn == n then p = p + 1 end
end
print("Abundant:", a)
print("Deficient:", d)
print("Perfect:", p)
```

{{out}}

```txt
Abundant:       4953
Deficient:      15043
Perfect:        4
```



## Maple


```Maple
  classify_number := proc(n::posint);
  if evalb(NumberTheory:-SumOfDivisors(n) < 2*n) then
     return "Deficient";
  elif evalb(NumberTheory:-SumOfDivisors(n) = 2*n) then
     return "Perfect";
  else
     return "Abundant";
  end if;
  end proc:

  classify_sequence := proc(k::posint)
  local num_list;
  num_list := map(classify_number, [seq(1..k)]);
  return Statistics:-Tally(num_list)
  end proc:
```


{{out}}
```txt
["Perfect" = 4, "Abundant" = 4953, "Deficient" = 15043]
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
classify[n_Integer] := Sign[Total[Most@Divisors@n] - n]

StringJoin[
 Flatten[Tally[
     Table[classify[n], {n, 20000}]] /. {-1 -> "deficient: ",
     0 -> "  perfect: ", 1 -> "  abundant: "}] /.
  n_Integer :> ToString[n]]
```


{{out}}
```txt
deficient: 15043  perfect: 4  abundant: 4953
```



## MatLab


```Matlab

abundant=0; deficient=0; perfect=0; p=[];
for N=2:20000
    K=1:ceil(N/2);
    D=K(~(rem(N, K)));
    sD=sum(D);
    if sD<N
        deficient=deficient+1;
    elseif sD==N
        perfect=perfect+1;
    else
        abundant=abundant+1;
    end
end
disp(table([deficient;perfect;abundant],'RowNames',{'Deficient','Perfect','Abundant'},'VariableNames',{'Quantities'}))

```

{{out}}

```txt

                Quantities
                 __________

    Deficient    15042
    Perfect          4
    Abundant      4953

```


## ML

=
## mLite
=

```ocaml
fun proper
		(number, count, limit, remainder, results) where (count > limit) = rev results
	|	(number, count, limit, remainder, results) =
			proper (number, count + 1, limit, number rem (count+1), if remainder = 0 then
				count :: results
			else
				results)
	|	number = (proper (number, 1, number div 2, 0, []))
;

fun is_abundant  number = number < (fold (op +, 0) ` proper number);
fun is_deficient number = number > (fold (op +, 0) ` proper number);
fun is_perfect   number = number = (fold (op +, 0) ` proper number);

val one_to_20000 = iota 20000;

print "Abundant numbers between 1 and 20000: ";
println ` fold (op +, 0) ` map ((fn n = if n then 1 else 0) o is_abundant) one_to_20000;

print "Deficient numbers between 1 and 20000: ";
println ` fold (op +, 0) ` map ((fn n = if n then 1 else 0) o is_deficient) one_to_20000;

print "Perfect numbers between 1 and 20000: ";
println ` fold (op +, 0) ` map ((fn n = if n then 1 else 0) o is_perfect) one_to_20000;

```

Output

```txt

Abundant numbers between 1 and 20000: 4953
Deficient numbers between 1 and 20000: 15043
Perfect numbers between 1 and 20000: 4

```


=={{header|Modula-2}}==

```modula2
MODULE ADP;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE ProperDivisorSum(n : INTEGER) : INTEGER;
VAR i,sum : INTEGER;
BEGIN
    sum := 0;
    IF n<2 THEN
        RETURN 0
    END;
    FOR i:=1 TO (n DIV 2) DO
        IF n MOD i = 0 THEN
            INC(sum,i)
        END
    END;
    RETURN sum
END ProperDivisorSum;

VAR
    buf : ARRAY[0..63] OF CHAR;
    n : INTEGER;
    d,p,a : INTEGER = 0;
    sum : INTEGER;
BEGIN
    FOR n:=1 TO 20000 DO
        sum := ProperDivisorSum(n);
        IF sum<n THEN
            INC(d)
        ELSIF sum=n THEN
            INC(p)
        ELSIF sum>n THEN
            INC(a)
        END
    END;

    WriteString("The classification of the numbers from 1 to 20,000 is as follows:");
    WriteLn;

    FormatString("Deficient = %i\n", buf, d);
    WriteString(buf);
    FormatString("Perfect = %i\n", buf, p);
    WriteString(buf);
    FormatString("Abundant = %i\n", buf, a);
    WriteString(buf);
    ReadChar
END ADP.
```



## Nim


```nim

proc sumProperDivisors(number: int) : int =
  if number < 2 : return 0
  for i in 1 .. number div 2 :
    if number mod i == 0 : result += i

var
  sum : int
  deficient = 0
  perfect = 0
  abundant = 0

for n in 1 .. 20000 :
  sum = sumProperDivisors(n)
  if sum < n :
    inc(deficient)
  elif sum == n :
    inc(perfect)
  else :
    inc(abundant)

echo "The classification of the numbers between 1 and 20,000 is as follows :\n"
echo "  Deficient = " , deficient
echo "  Perfect   = " , perfect
echo "  Abundant  = " , abundant

```


{{out}}

```txt

The classification of the numbers between 1 and 20,000 is as follows :

  Deficient = 15043
  Perfect   = 4
  Abundant  = 4953

```



## Oforth



```Oforth
import: mapping

Integer method: properDivs -- []
    self 2 / seq  filter( #[ self swap mod 0 == ] ) ;

: numberClasses
| i deficient perfect s |
   0 0 ->deficient ->perfect
   0 20000 loop: i [
      0 #+ i properDivs apply ->s
      s i <  ifTrue: [ deficient 1+ ->deficient continue ]
      s i == ifTrue: [ perfect 1+ ->perfect continue ]
      1+
      ]
   "Deficients :" . deficient .cr
   "Perfects   :" . perfect   .cr
   "Abundant   :" . .cr
;
```


{{out}}

```txt

numberClasses
Deficients : 15043
Perfects   : 4
Abundant   : 4953

```



## PARI/GP


```parigp
classify(k)=
{
  my(v=[0,0,0],t);
  for(n=1,k,
    t=sigma(n,-1);
    if(t<2,v[1]++,t>2,v[3]++,v[2]++)
  );
  v;
}
classify(20000)
```

{{out}}

```txt
%1 = [15043, 4, 4953]
```



## Pascal

using the slightly modified http://rosettacode.org/wiki/Amicable_pairs#Alternative

```pascal
program AmicablePairs;
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
const
  MAX = 20000;
//{$IFDEF UNIX} MAX = 524*1000*1000;{$ELSE}MAX = 499*1000*1000;{$ENDIF}
type
  tValue = LongWord;
  tpValue = ^tValue;
  tPower = array[0..31] of tValue;
  tIndex = record
             idxI,
             idxS : tValue;
           end;
  tdpa   = array[0..2] of LongWord;
var
  power        : tPower;
  PowerFac     : tPower;
  DivSumField  : array[0..MAX] of tValue;
  Indices      : array[0..511] of tIndex;
  DpaCnt       : tdpa;

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
  fillchar(DpaCnt,SizeOf(dpaCnt),#0);
  n := 0;
  For i := 1 to MAX do
  begin
    //s = sum of proper divs (I)  == sum of divs (I) - I
    s := DivSumField[i]-i;
    IF (s <=MAX) AND (s>i) then
    begin
      IF DivSumField[s]-s = i then
      begin
        With indices[n] do
        begin
          idxI := i;
          idxS := s;
        end;
        inc(n);
      end;
    end;
    inc(DpaCnt[Ord(s>=i)-Ord(s<=i)+1]);
  end;
  result := n;
end;

Procedure CalcPotfactor(prim:tValue);
//PowerFac[k] = (prim^(k+1)-1)/(prim-1) == Sum (i=1..k) prim^i
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

function NextPotCnt(p: tValue):tValue;inline;
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

function Sieve(prim: tValue):tValue;
//simple version
var
  actNumber : tValue;
begin
  while prim <= MAX do
  begin
    InitPW(prim);
    //actNumber = actual number = n*prim
    //power == n to base prim
    actNumber := prim;
    while actNumber < MAX do
    begin
      DivSumField[actNumber] := DivSumField[actNumber] *PowerFac[NextPotCnt(prim)];
      inc(actNumber,prim);
    end;
    //next prime
    repeat
      inc(prim);
    until (DivSumField[prim] = 1);
  end;
  result := prim;
end;

var
  T2,T1,T0: TDatetime;
  APcnt: tValue;

begin
  T0:= time;
  Init;
  Sieve(2);
  T1:= time;
  APCnt := Check;
  T2:= time;

  //AmPairOutput(APCnt);
  writeln(Max:10,' upper limit');
  writeln(DpaCnt[0]:10,' deficient');
  writeln(DpaCnt[1]:10,' perfect');
  writeln(DpaCnt[2]:10,' abundant');
  writeln(DpaCnt[2]/Max:14:10,' ratio abundant/upper Limit ');
  writeln(DpaCnt[0]/Max:14:10,' ratio abundant/upper Limit ');
  writeln(DpaCnt[2]/DpaCnt[0]:14:10,' ratio abundant/deficient   ');
  writeln('Time to calc sum of divs    ',FormatDateTime('HH:NN:SS.ZZZ' ,T1-T0));
  writeln('Time to find amicable pairs ',FormatDateTime('HH:NN:SS.ZZZ' ,T2-T1));
  {$IFNDEF UNIX}
    readln;
  {$ENDIF}
end.

```

output

```txt

     20000 upper limit
     15043 deficient
         4 perfect
      4953 abundant
  0.2476500000 ratio abundant/upper Limit
  0.7521500000 ratio abundant/upper Limit
  0.3292561324 ratio abundant/deficient
Time to calc sum of divs    00:00:00.000
Time to find amicable pairs 00:00:00.000

...
 524000000 upper limit
 394250308 deficient
         5 perfect
 129749687 abundant
  0.2476139065 ratio abundant/upper Limit
  0.7523860840 ratio abundant/upper Limit
  0.3291048463 ratio abundant/deficient
Time to calc sum of divs    00:00:12.597
Time to find amicable pairs 00:00:04.064

```



## Perl


### Using a module

{{libheader|ntheory}}
Use the <tt>&lt;=&gt;</tt> operator to return a comparison of -1, 0, or 1, which classifies the results.
1 is classified as a [[wp:Deficient_number|deficient number]], 6 is a [[wp:Perfect_number|perfect number]], 12 is an [[wp:Abundant_number|abundant number]]. As per task spec, also showing the totals for the first 20,000 numbers.


```perl
use ntheory qw/divisor_sum/;
my @type = <Perfect Abundant Deficient>;
say join "\n", map { sprintf "%2d %s", $_, $type[divisor_sum($_)-$_ <=> $_] } 1..12;
my %h;
$h{divisor_sum($_)-$_ <=> $_}++ for 1..20000;
say "Perfect: $h{0}    Deficient: $h{-1}    Abundant: $h{1}";
```

{{out}}

```txt
 1 Deficient
 2 Deficient
 3 Deficient
 4 Deficient
 5 Deficient
 6 Perfect
 7 Deficient
 8 Deficient
 9 Deficient
10 Deficient
11 Deficient
12 Abundant

Perfect: 4    Deficient: 15043    Abundant: 4953
```



### Not using a module

Everything as above, but done more slowly with <code>div_sum</code> providing sum of proper divisors.

```perl
sub div_sum {
    my($n) = @_;
    my $sum = 0;
    map { $sum += $_ unless $n % $_ } 1 .. $n-1;
    $sum;
}

my @type = <Perfect Abundant Deficient>;
say join "\n", map { sprintf "%2d %s", $_, $type[div_sum($_) <=> $_] } 1..12;
my %h;
$h{div_sum($_) <=> $_}++ for 1..20000;
say "Perfect: $h{0}    Deficient: $h{-1}    Abundant: $h{1}";
```



## Perl 6

{{Works with|rakudo|2018.12}}

```perl6
sub propdivsum (\x) {
    my @l = 1 if x > 1;
    (2 .. x.sqrt.floor).map: -> \d {
        unless x % d { @l.push: d; my \y = x div d; @l.push: y if y != d }
    }
    sum @l
}

say bag (1..20000).map: { propdivsum($_) <=> $_ }
```

{{out}}

```txt
Bag(Less(15043), More(4953), Same(4))
```



## Phix

I cheated a little and added a new factors() builtin, but it's there for good now.

```Phix
integer deficient=0, perfect=0, abundant=0, N
for i=1 to 20000 do
    N = sum(factors(i))+(i!=1)
    if N=i then
        perfect += 1
    elsif N<i then
        deficient += 1
    else
        abundant += 1
    end if
end for
printf(1,"deficient:%d, perfect:%d, abundant:%d\n",{deficient, perfect, abundant})
```

{{out}}

```txt

deficient:15043, perfect:4, abundant:4953

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
   (let
      (A 0
         D 0
         P 0 )
      (for I 20000
         (setq @@ (factor-sum I))
         (cond
            ((< @@ I) (inc 'D))
            ((= @@ I) (inc 'P))
            ((> @@ I) (inc 'A)) ) )
      (println D P A) ) )
(bye)
```

{{Output}}

```txt

15043 4 4953
0.110 sec

```



## PL/I


```pli
*process source xref;
 apd: Proc Options(main);
 p9a=time();
 Dcl (p9a,p9b) Pic'(9)9';
 Dcl cnt(3) Bin Fixed(31) Init((3)0);
 Dcl x Bin Fixed(31);
 Dcl pd(300) Bin Fixed(31);
 Dcl sumpd   Bin Fixed(31);
 Dcl npd     Bin Fixed(31);
 Do x=1 To 20000;
   Call proper_divisors(x,pd,npd);
   sumpd=sum(pd,npd);
   Select;
     When(x<sumpd) cnt(1)+=1; /* abundant  */
     When(x=sumpd) cnt(2)+=1; /* perfect   */
     Otherwise     cnt(3)+=1; /* deficient */
     End;
   End;

 Put Edit('In the range 1 - 20000')(Skip,a);
 Put Edit(cnt(1),' numbers are abundant ')(Skip,f(5),a);
 Put Edit(cnt(2),' numbers are perfect  ')(Skip,f(5),a);
 Put Edit(cnt(3),' numbers are deficient')(Skip,f(5),a);
 p9b=time();
 Put Edit((p9b-p9a)/1000,' seconds elapsed')(Skip,f(6,3),a);
 Return;

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

 End;
```

{{out}}

```txt
In the range 1 - 20000
 4953 numbers are abundant
    4 numbers are perfect
15043 numbers are deficient
 0.560 seconds elapsed

```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

function Get-ProperDivisorSum ( [int]$N )
    {
    If ( $N -lt 2 ) { return 0 }

    $Sum = 1
    If ( $N -gt 3 )
        {
        $SqrtN = [math]::Sqrt( $N )
        ForEach ( $Divisor in 2..$SqrtN )
            {
            If ( $N % $Divisor -eq 0 ) { $Sum += $Divisor + $N / $Divisor }
            }
        If ( $N % $SqrtN -eq 0 ) { $Sum -= $SqrtN }
        }
    return $Sum
    }


$Deficient = $Perfect = $Abundant = 0

ForEach ( $N in 1..20000 )
    {
    Switch ( [math]::Sign( ( Get-ProperDivisorSum $N ) - $N ) )
        {
        -1 { $Deficient++ }
         0 { $Perfect++   }
         1 { $Abundant++  }
        }
    }

"Deficient: $Deficient"
"Perfect  : $Perfect"
"Abundant : $Abundant"

```

{{out}}

```txt

Deficient: 15043
Perfect  : 4
Abundant : 4953

```



### As a single function

Using the <code>Get-ProperDivisorSum</code> as a helper function in an advanced function:

```PowerShell

function Get-NumberClassification
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [int]
        $Number
    )

    Begin
    {
        function Get-ProperDivisorSum ([int]$Number)
        {
            if ($Number -lt 2) {return 0}

            $sum = 1

            if ($Number -gt 3)
            {
                $sqrtNumber = [Math]::Sqrt($Number)

                foreach ($divisor in 2..$sqrtNumber)
                {
                    if ($Number % $divisor -eq 0) {$sum += $divisor + $Number / $divisor}
                }

                if ($Number % $sqrtNumber -eq 0) {$sum -= $sqrtNumber}
            }

            $sum
        }

        [System.Collections.ArrayList]$numbers = @()
    }
    Process
    {
        switch ([Math]::Sign((Get-ProperDivisorSum $Number) - $Number))
        {
            -1 { [void]$numbers.Add([PSCustomObject]@{Class="Deficient"; Number=$Number}) }
             0 { [void]$numbers.Add([PSCustomObject]@{Class="Perfect"  ; Number=$Number}) }
             1 { [void]$numbers.Add([PSCustomObject]@{Class="Abundant" ; Number=$Number}) }
        }
    }
    End
    {
        $numbers | Group-Object  -Property Class |
                   Select-Object -Property Count,
                                           @{Name='Class' ; Expression={$_.Name}},
                                           @{Name='Number'; Expression={$_.Group.Number}}
    }
}

```


```PowerShell

1..20000 | Get-NumberClassification

```

{{Out}}

```txt

Count Class     Number
----- -----     ------
15043 Deficient {1, 2, 3, 4...}
    4 Perfect   {6, 28, 496, 8128}
 4953 Abundant  {12, 18, 20, 24...}

```



## Prolog


```prolog

proper_divisors(1, []) :- !.
proper_divisors(N, [1|L]) :-
	FSQRTN is floor(sqrt(N)),
	proper_divisors(2, FSQRTN, N, L).

proper_divisors(M, FSQRTN, _, []) :-
	M > FSQRTN,
	!.
proper_divisors(M, FSQRTN, N, L) :-
	N mod M =:= 0, !,
	MO is N//M, % must be integer
	L = [M,MO|L1], % both proper divisors
	M1 is M+1,
	proper_divisors(M1, FSQRTN, N, L1).
proper_divisors(M, FSQRTN, N, L) :-
	M1 is M+1,
	proper_divisors(M1, FSQRTN, N, L).

dpa(1, [1], [], []) :-
	!.
dpa(N, D, P, A) :-
	N > 1,
	proper_divisors(N, PN),
	sum_list(PN, SPN),
	compare(VGL, SPN, N),
	dpa(VGL, N, D, P, A).

dpa(<, N, [N|D], P, A) :- N1 is N-1, dpa(N1, D, P, A).
dpa(=, N, D, [N|P], A) :- N1 is N-1, dpa(N1, D, P, A).
dpa(>, N, D, P, [N|A]) :- N1 is N-1, dpa(N1, D, P, A).


dpa(N) :-
	T0 is cputime,
	dpa(N, D, P, A),
	Dur is cputime-T0,
	length(D, LD),
	length(P, LP),
	length(A, LA),
	format("deficient: ~d~n abundant: ~d~n  perfect: ~d~n",
		   [LD, LA, LP]),
	format("took ~f seconds~n", [Dur]).

```

{{out}}

```txt

?- dpa(20000).
deficient: 15036
 abundant: 4960
  perfect: 4
took 0.802559 seconds

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

Define n, sum, deficient, perfect, abundant

If OpenConsole()
  For n = 1 To 20000
    sum = SumProperDivisors(n)
    If sum < n
      deficient + 1
    ElseIf sum = n
      perfect + 1
    Else
      abundant + 1
    EndIf
  Next
  PrintN("The breakdown for the numbers 1 to 20,000 is as follows : ")
  PrintN("")
  PrintN("Deficient = " + deficient)
  PrintN("Pefect    = " + perfect)
  PrintN("Abundant  = " + abundant)
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out}}

```txt

The breakdown for the numbers 1 to 20,000 is as follows :

Deficient = 15043
Pefect    = 4
Abundant  = 4953

```



## Python


Importing [[Proper_divisors#Python:_From_prime_factors|Proper divisors from prime factors]]:

```python>>>
 from proper_divisors import proper_divs
>>> from collections import Counter
>>>
>>> rangemax = 20000
>>>
>>> def pdsum(n):
...     return sum(proper_divs(n))
...
>>> def classify(n, p):
...     return 'perfect' if n == p else 'abundant' if p > n else 'deficient'
...
>>> classes = Counter(classify(n, pdsum(n)) for n in range(1, 1 + rangemax))
>>> classes.most_common()
[('deficient', 15043), ('abundant', 4953), ('perfect', 4)]
>>>
```


{{out}}

```txt

Between 1 and 20000:
  4953 abundant numbers
  15043 deficient numbers
  4 perfect numbers

```



## R


{{Works with|R|3.3.2 and above}}


```r

# Abundant, deficient and perfect number classifications. 12/10/16 aev
require(numbers);
propdivcls <- function(n) {
  V <- sapply(1:n, Sigma, proper = TRUE);
  c1 <- c2 <- c3 <- 0;
  for(i in 1:n){
    if(V[i]<i){c1 = c1 +1} else if(V[i]==i){c2 = c2 +1} else{c3 = c3 +1}
  }
  cat(" *** Between 1 and ", n, ":\n");
  cat("   * ", c1, "deficient numbers\n");
  cat("   * ", c2, "perfect numbers\n");
  cat("   * ", c3, "abundant numbers\n");
}
propdivcls(20000);

```


{{Output}}


```txt

> require(numbers)
Loading required package: numbers
> propdivcls(20000);
 *** Between 1 and  20000 :
   *  15043 deficient numbers
   *  4 perfect numbers
   *  4953 abundant numbers
>

```



## Racket



```racket
#lang racket
(require math)
(define (proper-divisors n) (drop-right (divisors n) 1))
(define classes '(deficient perfect abundant))
(define (classify n)
  (list-ref classes (add1 (sgn (- (apply + (proper-divisors n)) n)))))

(let ([N 20000])
  (define t (make-hasheq))
  (for ([i (in-range 1 (add1 N))])
    (define c (classify i))
    (hash-set! t c (add1 (hash-ref t c 0))))
  (printf "The range between 1 and ~a has:\n" N)
  (for ([c classes]) (printf "  ~a ~a numbers\n" (hash-ref t c 0) c)))
```


{{out}}

```txt

The range between 1 and 20000 has:
  15043 deficient numbers
  4 perfect numbers
  4953 abundant numbers

```



## REXX


### version 1


```rexx
/*REXX program counts the number of  abundant/deficient/perfect  numbers within a range.*/
parse arg low high .                             /*obtain optional arguments from the CL*/
high=word(high low 20000,1);  low= word(low 1,1) /*obtain the   LOW  and  HIGH   values.*/
say center('integers from '   low   " to "   high,  45,  "═")        /*display a header.*/
!.= 0                                            /*define all types of  sums  to zero.  */
      do j=low  to high;           $= sigma(j)   /*get sigma for an integer in a range. */
      if $<j  then               !.d= !.d + 1    /*Less?      It's a  deficient  number.*/
              else if $>j  then  !.a= !.a + 1    /*Greater?     "  "  abundant      "   */
                           else  !.p= !.p + 1    /*Equal?       "  "  perfect       "   */
      end  /*j*/                                 /* [↑]  IFs are coded as per likelihood*/

say '   the number of perfect   numbers: '       right(!.p, length(high) )
say '   the number of abundant  numbers: '       right(!.a, length(high) )
say '   the number of deficient numbers: '       right(!.d, length(high) )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sigma: procedure; parse arg x; if x<2  then return 0;  odd=x // 2    /* // ◄──remainder.*/
       s= 1                                      /* [↓]  only use  EVEN or ODD integers.*/
             do k=2+odd  by 1+odd  while k*k<x   /*divide by all integers up to  √x.    */
             if x//k==0  then  s= s + k +  x % k /*add the two divisors to (sigma) sum. */
             end   /*k*/                         /* [↑]  %  is the REXX integer division*/
       if k*k==x  then  return s + k             /*Was  X  a square?   If so, add  √ x  */
                        return s                 /*return (sigma) sum of the divisors.  */
```

{{out|output|text=  when using the default input:}}

```txt

═════════integers from  1  to  20000═════════
   the number of perfect   numbers:      4
   the number of abundant  numbers:   4953
   the number of deficient numbers:  15043

```



### version 1.5

This version is pretty much identical to the 1<sup>st</sup> version but uses an   ''integer square root''   calculation to find the

limit of the   '''do'''   loop in the   '''sigma'''   function.

  For    20k  integers,  it's approximately  '''12%'''  faster.
   "    100k     "         "        "        '''20%'''    "
   "      1m     "         "        "        '''30%'''    "

```rexx
/*REXX program counts the number of  abundant/deficient/perfect  numbers within a range.*/
parse arg low high .                             /*obtain optional arguments from the CL*/
high=word(high low 20000,1);  low=word(low 1, 1) /*obtain the   LOW  and  HIGH   values.*/
say center('integers from '   low    " to "    high,  45,  "═")      /*display a header.*/
!.= 0                                            /*define all types of  sums  to zero.  */
      do j=low  to high;           $= sigma(j)   /*get sigma for an integer in a range. */
      if $<j  then               !.d= !.d + 1    /*Less?      It's a  deficient  number.*/
              else if $>j  then  !.a= !.a + 1    /*Greater?     "  "  abundant      "   */
                           else  !.p= !.p + 1    /*Equal?       "  "  perfect       "   */
      end  /*j*/                                 /* [↑]  IFs are coded as per likelihood*/

say '   the number of perfect   numbers: '       right(!.p, length(high) )
say '   the number of abundant  numbers: '       right(!.a, length(high) )
say '   the number of deficient numbers: '       right(!.d, length(high) )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sigma: procedure; parse arg x 1 z;  if x<5  then return max(0, x-1)  /*sets X&Z to arg1.*/
       q=1;  do  while q<=z;  q= q * 4;     end  /* ◄──↓  compute integer sqrt of Z (=R)*/
       r=0;  do  while q>1; q=q%4; _=z-r-q; r=r%2; if _>=0  then do; z=_; r=r+q; end;  end
       odd= x//2                                 /* [↓]  only use EVEN | ODD ints.   ___*/
       s= 1;     do k=2+odd  by 1+odd  to r      /*divide by  all  integers up to   √ x */
                 if x//k==0  then  s=s + k + x%k /*add the two divisors to (sigma) sum. */
                 end   /*k*/                     /* [↑]  %  is the REXX integer division*/
       if r*r==x  then  return s - k             /*Was X a square?  If so, subtract √ x */
                        return s                 /*return (sigma) sum of the divisors.  */
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}




### version 2


```rexx
/* REXX */
Call time 'R'
cnt.=0
Do x=1 To 20000
  pd=proper_divisors(x)
  sumpd=sum(pd)
  Select
    When x<sumpd Then cnt.abundant =cnt.abundant +1
    When x=sumpd Then cnt.perfect  =cnt.perfect  +1
    Otherwise         cnt.deficient=cnt.deficient+1
    End
  Select
    When npd>hi Then Do
      list.npd=x
      hi=npd
      End
    When npd=hi Then
      list.hi=list.hi x
    Otherwise
      Nop
    End
  End

Say 'In the range 1 - 20000'
Say format(cnt.abundant ,5) 'numbers are abundant  '
Say format(cnt.perfect  ,5) 'numbers are perfect   '
Say format(cnt.deficient,5) 'numbers are deficient '
Say time('E') 'seconds elapsed'
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
In the range 1 - 20000
 4953 numbers are abundant
    4 numbers are perfect
15043 numbers are deficient
28.392000 seconds elapsed
```



## Ring


```ring

n = 30
perfect(n)

func perfect n
for i = 1 to n
    sum = 0
    for j = 1 to i - 1
        if i % j = 0 sum = sum + j ok
    next
    see i
    if sum = i see " is a perfect number" + nl
    but sum < i see " is a deficient number" + nl
    else see " is a abundant number" + nl ok
next

```



## Rust


With [[proper_divisors#Rust]] in place:

```rust
fn main() {
    // deficient starts at 1 because 1 is deficient but proper_divisors returns
    // and empty Vec
    let (mut abundant, mut deficient, mut perfect) = (0u32, 1u32, 0u32);
    for i in 1..20_001 {
        if let Some(divisors) = i.proper_divisors() {
            let sum: u64 = divisors.iter().sum();
            if sum < i {
                deficient += 1
            } else if sum > i {
                abundant += 1
            } else {
                perfect += 1
            }
        }
    }
    println!("deficient:\t{:5}\nperfect:\t{:5}\nabundant:\t{:5}",
             deficient, perfect, abundant);
}

```


{{out}}

```txt

deficient:      15043
perfect:            4
abundant:        4953

```



## Ruby

With [[proper_divisors#Ruby]] in place:

```ruby
res = Hash.new(0)
(1 .. 20_000).each{|n| res[n.proper_divisors.sum <=> n] += 1}
puts "Deficient: #{res[-1]}   Perfect: #{res[0]}   Abundant: #{res[1]}"

```

{{out}}
```txt

Deficient: 15043   Perfect: 4   Abundant: 4953

```



## Scala


```Scala
def properDivisors(n: Int) = (1 to n/2).filter(i => n % i == 0)
def classifier(i: Int) = properDivisors(i).sum compare i
val groups = (1 to 20000).groupBy( classifier )
println("Deficient: " + groups(-1).length)
println("Abundant: " + groups(1).length)
println("Perfect: " + groups(0).length + " (" + groups(0).mkString(",") + ")")
```

{{out}}

```txt
Deficient: 15043
Abundant: 4953
Perfect: 4 (6,28,496,8128)
```



## Scheme


```scheme

(define (classify n)
 (define (sum_of_factors x)
  (cond ((= x 1) 1)
        ((= (remainder n x) 0) (+ x (sum_of_factors (- x 1))))
        (else (sum_of_factors (- x 1)))))
 (cond ((or (= n 1) (< (sum_of_factors (floor (/ n 2))) n)) -1)
       ((= (sum_of_factors (floor (/ n 2))) n) 0)
       (else 1)))
(define n_perfect 0)
(define n_abundant 0)
(define n_deficient 0)
(define (count n)
 (cond ((= n 1) (begin (display "perfect ")
                       (display n_perfect)
                       (newline)
                       (display "abundant")
                       (display n_abundant)
                       (newline)
                       (display "deficinet")
                       (display n_perfect)
                       (newline)))
       ((equal? (classify n) 0) (begin (set! n_perfect (+ 1 n_perfect)) (display n) (newline) (count (- n 1))))
       ((equal? (classify n) 1) (begin (set! n_abundant (+ 1 n_abundant)) (count (- n 1))))
       ((equal? (classify n) -1) (begin (set! n_deficient (+ 1 n_deficient)) (count (- n 1))))))

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: sumProperDivisors (in integer: number) is func
  result
    var integer: sum is 0;
  local
    var integer: num is 0;
  begin
    if number >= 2 then
      for num range 1 to number div 2 do
        if number rem num = 0 then
	  sum +:= num;
	end if;
      end for;
    end if;
  end func;

const proc: main is func
  local
    var integer: sum is 0;
    var integer: deficient is 0;
    var integer: perfect is 0;
    var integer: abundant is 0;
    var integer: number is 0;
  begin
    for number range 1 to 20000 do
      sum := sumProperDivisors(number);
      if sum < number then
        incr(deficient);
      elsif sum = number then
        incr(perfect);
      else
        incr(abundant);
      end if;
    end for;
    writeln("Deficient: " <& deficient);
    writeln("Perfect:   " <& perfect);
    writeln("Abundant:  " <& abundant);
  end func;
```


{{out}}

```txt

Deficient: 15043
Perfect:   4
Abundant:  4953

```



## Sidef


```ruby
func propdivsum(n) { n.sigma - n }

var h = Hash()
{|i| ++(h{propdivsum(i) <=> i} := 0) } << 1..20000
say "Perfect: #{h{0}}    Deficient: #{h{-1}}    Abundant: #{h{1}}"
```

{{out}}

```txt

Perfect: 4    Deficient: 15043    Abundant: 4953

```



## Swift

{{trans|C}}

```swift
var deficients = 0 // sumPd < n
var perfects = 0 // sumPd = n
var abundants = 0 // sumPd > n

// 1 is deficient (no proper divisor)
deficients++


for i in 2...20000 {

    var sumPd = 1 // 1 is a proper divisor of all integer above 1

    var maxPdToTest = i/2 // the max divisor to test

    for var j = 2; j < maxPdToTest; j++ {

        if (i%j) == 0 {
            // j is a proper divisor
            sumPd += j

            // New maximum for divisibility check
            maxPdToTest = i / j

            // To add to sum of proper divisors unless already done
            if maxPdToTest != j {
                sumPd += maxPdToTest
            }
        }
    }

    // Select type according to sum of Proper divisors
    if sumPd < i {
        deficients++
    } else if sumPd > i {
        abundants++
    } else {
        perfects++
    }
}

println("There are \(deficients) deficient, \(perfects) perfect and \(abundants) abundant integers from 1 to 20000.")
```

{{out}}
```txt
There are 15043 deficient, 4 perfect and 4953 abundant integers from 1 to 20000.
```



## Tcl



```Tcl
proc ProperDivisors {n} {
    if {$n == 1} {return 0}
    set divs 1
    set sum 1
    for {set i 2} {$i*$i <= $n} {incr i} {
        if {! ($n % $i)} {
            lappend divs $i
            incr sum $i
            if {$i*$i<$n} {
                lappend divs [set d [expr {$n / $i}]]
                incr sum $d
            }
        }
    }
    list $sum $divs
}

proc cmp {i j} {    ;# analogous to [string compare], but for numbers
    if {$i == $j} {return 0}
    if {$i > $j} {return 1}
    return -1
}

proc classify {k} {
    lassign [ProperDivisors $k] p    ;# we only care about the first part of the result
    dict get {
        1   abundant
        0   perfect
       -1   deficient
    } [cmp $k $p]
}

puts "Classifying the integers in \[1, 20_000\]:"
set classes {}    ;# this will be a dict

for {set i 1} {$i <= 20000} {incr i} {
    set class [classify $i]
    dict incr classes $class
}

# using [lsort] to order the dictionary by value:
foreach {kind count} [lsort -stride 2 -index 1 -integer $classes] {
    puts "$kind: $count"
}
```


{{out}}

```txt
Classifying the integers in [1, 20_000]:
perfect: 4
deficient: 4953
abundant: 15043
```



## TypeScript



```txt
function integer_classification(){
	var sum:number=0, i:number,j:number;
	var try:number=0;
	var number_list:number[]={1,0,0};
	for(i=2;i<=20000;i++){
		try=i/2;
		sum=1;
		for(j=2;j<try;j++){
			if (i%j)
				continue;
			try=i/j;
			sum+=j;
			if (j!=try)
				sum+=try;
		}
		if (sum<i){
			number_list[d]++;
			continue;
		}
		else if (sum>i){
			number_list[a]++;
			continue;
		}
		number_list[p]++;
	}
	console.log('There are '+number_list[d]+ ' deficient , ' + 'number_list[p] + ' perfect and '+ number_list[a]+ ' abundant numbers
between 1 and 20000');
}

```



## uBasic/4tH

This is about the limit of what is feasible with uBasic/4tH performance wise, since a full run takes over 5 minutes.
<lang>P = 0 : D = 0 : A = 0

For n= 1 to 20000
  s = FUNC(_SumDivisors(n))-n
  If s = n Then P = P + 1
  If s < n Then D = D + 1
  If s > n Then A = A + 1
Next

Print "Perfect: ";P;" Deficient: ";D;" Abundant: ";A
End

' Return the least power of a@ that does not divide b@

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

{{out}}

```txt
Perfect: 4 Deficient: 15043 Abundant: 4953

0 OK, 0:210
```



## VBA


```VB

Option Explicit

Public Sub Nb_Classifications()
Dim A As New Collection, D As New Collection, P As New Collection
Dim n As Long, l As Long, s As String, t As Single

    t = Timer
    'Start
    For n = 1 To 20000
        l = SumPropers(n): s = CStr(n)
        Select Case n
            Case Is > l: D.Add s, s
            Case Is < l: A.Add s, s
            Case l: P.Add s, s
        End Select
    Next

    'End. Return :
    Debug.Print "Execution Time : " & Timer - t & " seconds."
    Debug.Print "-------------------------------------------"
    Debug.Print "Deficient := " & D.Count
    Debug.Print "Perfect := " & P.Count
    Debug.Print "Abundant := " & A.Count
End Sub

Private Function SumPropers(n As Long) As Long
'returns the sum of the proper divisors of n
Dim j As Long
    For j = 1 To n \ 2
        If n Mod j = 0 Then SumPropers = j + SumPropers
    Next
End Function
```

{{out}}

```txt
Execution Time : 2,6875 seconds.
-------------------------------------------
Deficient := 15043
Perfect := 4
Abundant := 4953
```



## VBScript


```VBScript
Deficient = 0
Perfect = 0
Abundant = 0
For i = 1 To 20000
	sum = 0
	For n = 1 To 20000
		If n < i Then
			If i Mod n = 0 Then
				sum = sum + n
			End If
		End If
	Next
	If sum < i Then
		Deficient = Deficient + 1
	ElseIf sum = i Then
		Perfect = Perfect + 1
	ElseIf sum > i Then
		Abundant = Abundant + 1
	End If
Next
WScript.Echo "Deficient = " & Deficient & vbCrLf &_
			 "Perfect = " & Perfect & vbCrLf &_
			 "Abundant = " & Abundant
```

{{out}}

```txt
Deficient = 15043
Perfect = 4
Abundant = 4953
```



## Visual Basic .NET

{{trans|FreeBASIC}}

```vbnet
Module Module1

    Function SumProperDivisors(number As Integer) As Integer
        If number < 2 Then Return 0
        Dim sum As Integer = 0
        For i As Integer = 1 To number \ 2
            If number Mod i = 0 Then sum += i
        Next
        Return sum
    End Function

    Sub Main()
        Dim sum, deficient, perfect, abundant As Integer

        For n As Integer = 1 To 20000
            sum = SumProperDivisors(n)
            If sum < n Then
                deficient += 1
            ElseIf sum = n Then
                perfect += 1
            Else
                abundant += 1
            End If
        Next

        Console.WriteLine("The classification of the numbers from 1 to 20,000 is as follows : ")
        Console.WriteLine()
        Console.WriteLine("Deficient = {0}", deficient)
        Console.WriteLine("Perfect   = {0}", perfect)
        Console.WriteLine("Abundant  = {0}", abundant)
    End Sub

End Module
```

{{out}}

```txt
The classification of the numbers from 1 to 20,000 is as follows :

Deficient = 15043
Perfect   = 4
Abundant  = 4953
```



## Yabasic

{{trans|AWK}}

```Yabasic
clear screen

Deficient = 0
Perfect = 0
Abundant = 0
For j=1 to 20000
	sump = sumprop(j)
	If sump < j Then
		Deficient = Deficient + 1
	ElseIf sump = j Then
		Perfect = Perfect + 1
	ElseIf sump > j Then
		Abundant = Abundant + 1
	End If
Next j

PRINT "Number deficient: ",Deficient
PRINT "Number perfect:   ",Perfect
PRINT "Number abundant:  ",Abundant

sub sumprop(num)
	local i, sum, root

	if num>1 then
		sum=1
		root=sqrt(num)
		for i=2 to root
			if mod(num,i) = 0 then
				sum=sum+i
				if (i*i)<>num sum=sum+num/i
			end if
		next i
	end if
	return sum
end sub
```



## zkl

{{trans|D}}

```zkl
fcn properDivs(n){ [1.. (n + 1)/2 + 1].filter('wrap(x){ n%x==0 and n!=x }) }

fcn classify(n){
   p:=properDivs(n).sum();
   return(if(p<n) -1 else if(p==n) 0 else 1);
}

const rangeMax=20_000;
classified:=[1..rangeMax].apply(classify);
perfect   :=classified.filter('==(0)).len();
abundant  :=classified.filter('==(1)).len();
println("Deficient=%d, perfect=%d, abundant=%d".fmt(
   classified.len()-perfect-abundant, perfect, abundant));
```

{{out}}
```txt
Deficient=15043, perfect=4, abundant=4953
```




## ZX Spectrum Basic

Solution 1:

```zxbasic
  10 LET nd=1: LET np=0: LET na=0
  20 FOR i=2 TO 20000
  30 LET sum=1
  40 LET max=i/2
  50 LET n=2: LET l=max-1
  60 IF n>l THEN GO TO 90
  70 IF i/n=INT (i/n) THEN LET sum=sum+n: LET max=i/n: IF max<>n THEN LET sum=sum+max: LET l=max-1
  80 LET n=n+1: GO TO 60
  90 IF sum<i THEN LET nd=nd+1: GO TO 120
 100 IF sum=i THEN LET np=np+1: GO TO 120
 110 LET na=na+1
 120 NEXT i
 130 PRINT "Number deficient: ";nd
 140 PRINT "Number perfect:   ";np
 150 PRINT "Number abundant:  ";na
```


Solution 2 (more efficient):

```zxbasic
  10 LET abundant=0: LET deficient=0: LET perfect=0
  20 FOR j=1 TO 20000
  30 GO SUB 120
  40 IF sump<j THEN LET deficient=deficient+1: GO TO 70
  50 IF sump=j THEN LET perfect=perfect+1: GO TO 70
  60 LET abundant=abundant+1
  70 NEXT j
  80 PRINT "Perfect: ";perfect
  90 PRINT "Abundant: ";abundant
 100 PRINT "Deficient: ";deficient
 110 STOP
 120 IF j=1 THEN LET sump=0: RETURN
 130 LET sum=1
 140 LET root=SQR j
 150 FOR i=2 TO root
 160 IF j/i=INT (j/i) THEN LET sum=sum+i: IF (i*i)<>j THEN LET sum=sum+j/i
 170 NEXT i
 180 LET sump=sum
 190 RETURN
```


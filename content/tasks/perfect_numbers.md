+++
title = "Perfect numbers"
description = ""
date = 2019-09-10T13:31:46Z
aliases = []
[extra]
id = 2999
[taxonomies]
categories = ["task", "Discrete math"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "algol_w",
  "applescript",
  "autohotkey",
  "awk",
  "axiom",
  "basic",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dyalect",
  "e",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
  "erre",
  "factor",
  "false",
  "forth",
  "fortran",
  "freebasic",
  "funl",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "labview",
  "lasso",
  "liberty_basic",
  "lingo",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "microsoft_small_basic",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sasl",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "simula",
  "sinclair_zx81_basic",
  "slate",
  "smalltalk",
  "swift",
  "tcl",
  "ursala",
  "vba",
  "vbscript",
  "xpl0",
  "zkl",
]
+++

## Task

Write a function which says whether a number is perfect.



[[wp:Perfect_numbers|A perfect number]] is a positive integer that is the sum of its proper positive divisors excluding the number itself.

Equivalently, a perfect number is a number that is half the sum of all of its positive divisors (including itself).


Note:   The faster   [[Lucas-Lehmer test]]   is used to find primes of the form   <big> 2<sup>''n''</sup>-1</big>,   all ''known'' perfect numbers can be derived from these primes
using the formula   <big> (2<sup>''n''</sup> - 1) × 2<sup>''n'' - 1</sup></big>.

It is not known if there are any odd perfect numbers (any that exist are larger than <big>10<sup>2000</sup></big>).

The number of   ''known''   perfect numbers is   '''50'''   (as of September, 2018),   and the largest known perfect number contains over '''46''' million decimal digits.


## See also

:*   [[Rational Arithmetic]]
:*   [[oeis:A000396|Perfect numbers on OEIS]]
:*   [http://www.oddperfect.org/ Odd Perfect] showing the current status of bounds on odd perfect numbers.





## 360 Assembly


### Simple code

For maximum compatibility, this program uses only the basic instruction set (S/360)
and two ASSIST macros (XDECO,XPRNT) to keep it as short as possible.
The only added optimization is the loop up to n/2 instead of n-1.
With 31 bit integers the limit is 2,147,483,647.

```360asm
*        Perfect numbers           15/05/2016
PERFECTN CSECT
         USING  PERFECTN,R13       prolog
SAVEAREA B      STM-SAVEAREA(R15)  "
         DC     17F'0'             "
STM      STM    R14,R12,12(R13)    "
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R6,2               i=2
LOOPI    C      R6,NN              do i=2 to nn
         BH     ELOOPI
         LR     R1,R6              i
         BAL    R14,PERFECT
         LTR    R0,R0              if perfect(i)
         BZ     NOTPERF
         XDECO  R6,PG              edit i
         XPRNT  PG,L'PG            print i
NOTPERF  LA     R6,1(R6)           i=i+1
         B      LOOPI
ELOOPI   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
PERFECT  SR     R9,R9              function perfect(n); sum=0
         LA     R7,1               j
         LR     R8,R1              n
         SRA    R8,1               n/2
LOOPJ    CR     R7,R8              do j=1 to n/2
         BH     ELOOPJ
         LR     R4,R1              n
         SRDA   R4,32
         DR     R4,R7              n/j
         LTR    R4,R4              if mod(n,j)=0
         BNZ    NOTMOD
         AR     R9,R7              sum=sum+j
NOTMOD   LA     R7,1(R7)           j=j+1
         B      LOOPJ
ELOOPJ   SR     R0,R0              r0=false
         CR     R9,R1              if sum=n
         BNE    NOTEQ
         BCTR   R0,0               r0=true
NOTEQ    BR     R14                return(r0); end perfect
NN       DC     F'10000'
PG       DC     CL12' '            buffer
         YREGS
         END    PERFECTN
```

```txt

           6
          28
         496
        8128

```


### Some optimizations

Use of optimizations found in Rexx algorithms and use of packed decimal to have bigger numbers.
With 15 digit decimal integers the limit is 999,999,999,999,999.

```360asm
*        Perfect numbers           15/05/2016
PERFECPO CSECT
         USING  PERFECPO,R13       prolog
SAVEAREA B      STM-SAVEAREA(R15)  "
         DC     17F'0'             "
STM      STM    R14,R12,12(R13)    "
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         ZAP    I,I1               i=i1
LOOPI    CP     I,I2               do i=i1 to i2
         BH     ELOOPI
         LA     R1,I               r1=@i
         BAL    R14,PERFECT        perfect(i)
         LTR    R0,R0              if perfect(i)
         BZ     NOTPERF
         UNPK   PG(16),I           unpack i
         OI     PG+15,X'F0'
         XPRNT  PG,16              print i
NOTPERF  AP     I,=P'1'            i=i+1
         B      LOOPI
ELOOPI   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
PERFECT  EQU    *                  function perfect(n);
         ZAP    N,0(8,R1)          n=%r1
         CP     N,=P'6'            if n=6
         BNE    NOT6
         L      R0,=F'-1'          r0=true
         B      RETURN             return(true)
NOT6     ZAP    PW,N               n
         SP     PW,=P'1'           n-1
         ZAP    PW2,PW             n-1
         DP     PW2,=PL8'9'        (n-1)/9
         ZAP    R,PW2+8(8)         if mod((n-1),9)<>0
         BZ     ZERO
         SR     R0,R0              r0=false
         B      RETURN             return(false)
ZERO     ZAP    PW2,N              n
         DP     PW2,=PL8'2'        n/2
         ZAP    SUM,PW2(8)         sum=n/2
         AP     SUM,=P'3'          sum=n/2+3
         ZAP    J,=P'3'            j=3
LOOPJ    ZAP    PW,J               do loop on j
         MP     PW,J               j*j
         CP     PW,N               while j*j<=n
         BH     ELOOPJ
         ZAP    PW2,N              n
         DP     PW2,J              n/j
         CP     PW2+8(8),=P'0'     if mod(n,j)<>0
         BNE    NEXTJ
         AP     SUM,J              sum=sum+j
         ZAP    PW2,N              n
         DP     PW2,J              n/j
         AP     SUM,PW2(8)         sum=sum+j+n/j
NEXTJ    AP     J,=P'1'            j=j+1
         B      LOOPJ              next j
ELOOPJ   SR     R0,R0              r0=false
         CP     SUM,N              if sum=n
         BNE    RETURN
         BCTR   R0,0               r0=true
RETURN   BR     R14                return(r0); end perfect
I1       DC     PL8'1'
I2       DC     PL8'200000000000'
I        DS     PL8
PG       DC     CL16' '            buffer
N        DS     PL8
SUM      DS     PL8
J        DS     PL8
R        DS     PL8
C        DS     CL16
PW       DS     PL8
PW2      DS     PL16
         YREGS
         END    PERFECPO
```

```txt

0000000000000006
0000000000000028
0000000000000496
0000000000008128
0000000033550337
0000008589869056
0000137438691328

```



## Ada


```ada
function Is_Perfect(N : Positive) return Boolean is
   Sum : Natural := 0;
begin
   for I in 1..N - 1 loop
      if N mod I = 0 then
         Sum := Sum + I;
      end if;
   end loop;
   return Sum = N;
end Is_Perfect;
```



## ALGOL 68

```algol68
PROC is perfect = (INT candidate)BOOL: (
  INT sum :=1;
  FOR f1 FROM 2 TO ENTIER ( sqrt(candidate)*(1+2*small real) ) WHILE
    IF candidate MOD f1 = 0 THEN
      sum +:= f1;
      INT f2 = candidate OVER f1;
      IF f2 > f1 THEN
        sum +:= f2
      FI
    FI;
# WHILE # sum <= candidate DO
    SKIP
  OD;
  sum=candidate
);

test:(
  FOR i FROM 2 TO 33550336 DO
    IF is perfect(i) THEN print((i, new line)) FI
  OD
)
```

```txt

         +6
        +28
       +496
      +8128
  +33550336

```



## ALGOL W

Based on the Algol 68 version.

```algolw
begin
    % returns true if n is perfect, false otherwise                %
    % n must be > 0                                                %
    logical procedure isPerfect ( integer value candidate ) ;
        begin
            integer sum;
            sum    := 1;
            for f1 := 2 until round( sqrt( candidate ) ) do begin
                if candidate rem f1 = 0 then begin
                    integer f2;
                    sum := sum + f1;
                    f2  := candidate div f1;
                    % avoid e.g. counting 2 twice as a factor of 4 %
                    if f2 > f1 then sum := sum + f2
                end if_candidate_rem_f1_eq_0 ;
            end for_f1 ;
            sum = candidate
        end isPerfect ;

    % test isPerfect                                               %
    for n := 2 until 10000 do if isPerfect( n ) then write( n );
end.
```

```txt

             6
            28
           496
          8128

```



## AppleScript


```AppleScript
-- PERFECT NUMBERS -----------------------------------------------------------

-- perfect :: integer -> bool
on perfect(n)

    -- isFactor :: integer -> bool
    script isFactor
        on |λ|(x)
            n mod x = 0
        end |λ|
    end script

    -- quotient :: number -> number
    script quotient
        on |λ|(x)
            n / x
        end |λ|
    end script

    -- sum :: number -> number -> number
    script sum
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    -- Integer factors of n below the square root
    set lows to filter(isFactor, enumFromTo(1, (n ^ (1 / 2)) as integer))

    -- low and high factors (quotients of low factors) tested for perfection
    (n > 1) and (foldl(sum, 0, (lows & map(quotient, lows))) / 2 = n)
end perfect


-- TEST ----------------------------------------------------------------------
on run

    filter(perfect, enumFromTo(1, 10000))

    --> {6, 28, 496, 8128}

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

```AppleScript
{6, 28, 496, 8128}
```



## AutoHotkey

This will find the first 8 perfect numbers.

```autohotkey
Loop, 30 {
  If isMersennePrime(A_Index + 1)
    res .= "Perfect number: " perfectNum(A_Index + 1) "`n"
}

MsgBox % res

perfectNum(N) {
  Return 2**(N - 1) * (2**N - 1)
}

isMersennePrime(N) {
  If (isPrime(N)) && (isPrime(2**N - 1))
    Return true
}

isPrime(N) {
  Loop, % Floor(Sqrt(N))
    If (A_Index > 1 && !Mod(N, A_Index))
      Return false
  Return true
}
```



## AWK


```awk
$ awk 'func perf(n){s=0;for(i=1;i<n;i++)if(n%i==0)s+=i;return(s==n)}
BEGIN{for(i=1;i<10000;i++)if(perf(i))print i}'
6
28
496
8128
```


## Axiom

Using the interpreter, define the function:

```Axiom
perfect?(n:Integer):Boolean == reduce(+,divisors n) = 2*n
```

Alternatively, using the Spad compiler:

```Axiom
)abbrev package TESTP TestPackage
TestPackage() : withma
    perfect?: Integer -> Boolean
  ==
    add
      import IntegerNumberTheoryFunctions
      perfect? n == reduce("+",divisors n) = 2*n
```


Examples (testing 496, testing 128, finding all perfect numbers in 1...10000):

```Axiom
perfect? 496
perfect? 128
[i for i in 1..10000 | perfect? i]
```

```Axiom
true
false
[6,28,496,8128]
```



## BASIC

```qbasic
FUNCTION perf(n)
	sum = 0
	for i = 1 to n - 1
		IF n MOD i = 0 THEN
			sum = sum + i
		END IF
	NEXT i
	IF sum = n THEN
		perf = 1
	ELSE
		perf = 0
	END IF
END FUNCTION
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "PerfectN.bas"
110 FOR X=1 TO 10000
120   IF PERFECT(X) THEN PRINT X;
130 NEXT
140 DEF PERFECT(N)
150   IF N<2 OR MOD(N,2)<>0 THEN LET PERFECT=0:EXIT DEF
160   LET S=1
170   FOR I=2 TO SQR(N)
180     IF MOD(N,I)=0 THEN LET S=S+I+N/I
190   NEXT
200   LET PERFECT=N=S
210 END DEF
```


=
## Sinclair ZX81 BASIC
=
Call this subroutine and it will (eventually) return <tt>PERFECT</tt> = 1 if <tt>N</tt> is perfect or <tt>PERFECT</tt> = 0 if it is not.

```basic
2000 LET SUM=0
2010 FOR F=1 TO N-1
2020 IF N/F=INT (N/F) THEN LET SUM=SUM+F
2030 NEXT F
2040 LET PERFECT=SUM=N
2050 RETURN
```



## BBC BASIC


### BASIC version


```bbcbasic
      FOR n% = 2 TO 10000 STEP 2
        IF FNperfect(n%) PRINT n%
      NEXT
      END

      DEF FNperfect(N%)
      LOCAL I%, S%
      S% = 1
      FOR I% = 2 TO SQR(N%)-1
        IF N% MOD I% = 0 S% += I% + N% DIV I%
      NEXT
      IF I% = SQR(N%) S% += I%
      = (N% = S%)
```

```txt

         6
        28
       496
      8128

```



### Assembler version

```bbcbasic
      DIM P% 100
      [OPT 2 :.S% xor edi,edi
      .perloop mov eax,ebx : cdq : div ecx : or edx,edx : loopnz perloop : inc ecx
      add edi,ecx : add edi,eax : loop perloop : mov eax,edi : shr eax,1 : ret : ]

      FOR B% = 2 TO 35000000 STEP 2
        C% = SQRB%
        IF B% = USRS% PRINT B%
      NEXT
      END
```

```txt

         4
         6
        28
       496
      8128
  33550336

```



## Bracmat


```bracmat
( ( perf
  =   sum i
    .   0:?sum
      & 0:?i
      &   whl
        ' ( !i+1:<!arg:?i
          & ( mod$(!arg.!i):0&!sum+!i:?sum
            |
            )
          )
      & !sum:!arg
  )
& 0:?n
&   whl
  ' ( !n+1:~>10000:?n
    & (perf$!n&out$!n|)
    )
);
```

```txt
6
28
496
8128
```



## C

```c
#include "stdio.h"
#include "math.h"

int perfect(int n) {
    int max = (int)sqrt((double)n) + 1;
    int tot = 1;
    int i;

    for (i = 2; i < max; i++)
        if ( (n % i) == 0 ) {
            tot += i;
            int q = n / i;
            if (q > i)
                tot += q;
        }

    return tot == n;
}

int main() {
    int n;
    for (n = 2; n < 33550337; n++)
        if (perfect(n))
            printf("%d\n", n);

    return 0;
}
```

Using functions from [[Factors of an integer#Prime factoring]]:

```c
int main()
{
	int j;
	ulong fac[10000], n, sum;

	sieve();

	for (n = 2; n < 33550337; n++) {
		j = get_factors(n, fac) - 1;
		for (sum = 0; j && sum <= n; sum += fac[--j]);
		if (sum == n) printf("%lu\n", n);
	}

	return 0;
}
```


## C#
```c#
static void Main(string[] args)
{
	Console.WriteLine("Perfect numbers from 1 to 33550337:");

	for (int x = 0; x < 33550337; x++)
	{
		if (IsPerfect(x))
			Console.WriteLine(x + " is perfect.");
	}

	Console.ReadLine();
}

static bool IsPerfect(int num)
{
	int sum = 0;
	for (int i = 1; i < num; i++)
	{
		if (num % i == 0)
			sum += i;
	}

	return sum == num ;
}
```

===Version using Lambdas, will only work from version 3 of C# on===

```c#
static void Main(string[] args)
{
	Console.WriteLine("Perfect numbers from 1 to 33550337:");

	for (int x = 0; x < 33550337; x++)
	{
		if (IsPerfect(x))
			Console.WriteLine(x + " is perfect.");
	}

	Console.ReadLine();
}

static bool IsPerfect(int num)
{
	return Enumerable.Range(1, num - 1).Sum(n => num % n == 0 ? n : 0 ) == num;
}
```



## C++

```cpp
#include <iostream>
using namespace std ;

int divisor_sum( int number ) {
   int sum = 0 ;
   for ( int i = 1 ; i < number ; i++ )
      if ( number % i == 0 )
         sum += i ;
   return sum;
}

int main( ) {
   cout << "Perfect numbers from 1 to 33550337:\n" ;
   for ( int num = 1 ; num < 33550337 ; num++ ) {
      if (divisor_sum(num) == num)
         cout << num << '\n' ;
   }
   return 0 ;
}

```



## Clojure


```clojure
(defn proper-divisors [n]
  (if (< n 4)
    [1]
    (->> (range 2 (inc (quot n 2)))
         (filter #(zero? (rem n %)))
         (cons 1))))

(defn perfect? [n]
  (= (reduce + (proper-divisors n)) n))
```


```clojure
(defn perfect? [n]
  (->> (for [i (range 1 n)] :when (zero? (rem n i))] i)
       (reduce +)
       (= n)))
```



### Functional version


```clojure
(defn perfect? [n]
	(= (reduce + (filter #(zero? (rem n %)) (range 1 n))) n))
```



## CoffeeScript

Optimized version, for fun.

```coffeescript
is_perfect_number = (n) ->
  do_factors_add_up_to n, 2*n

do_factors_add_up_to = (n, desired_sum) ->
  # We mildly optimize here, by taking advantage of
  # the fact that the sum_of_factors( (p^m) * x)
  # is (1 + ... + p^m-1 + p^m) * sum_factors(x) when
  # x is not itself a multiple of p.

  p = smallest_prime_factor(n)
  if p == n
    return desired_sum == p + 1

  # ok, now sum up all powers of p that
  # divide n
  sum_powers = 1
  curr_power = 1
  while n % p == 0
    curr_power *= p
    sum_powers += curr_power
    n /= p

  # if desired_sum does not divide sum_powers, we
  # can short circuit quickly
  return false unless desired_sum % sum_powers == 0

  # otherwise, recurse
  do_factors_add_up_to n, desired_sum / sum_powers

smallest_prime_factor = (n) ->
  for i in [2..n]
    return n if i*i > n
    return i if n % i == 0

# tests
do ->
  # This is pretty fast...
  for n in [2..100000]
    console.log n if is_perfect_number n

  # For big numbers, let's just sanity check the known ones.
  known_perfects = [
    33550336
    8589869056
    137438691328
  ]
  for n in known_perfects
    throw Error("fail") unless is_perfect_number(n)
    throw Error("fail") if is_perfect_number(n+1)
```

```txt

> coffee perfect_numbers.coffee
6
28
496
8128

```



## COBOL

main.cbl:

```cobol
      $set REPOSITORY "UPDATE ON"

       IDENTIFICATION DIVISION.
       PROGRAM-ID. perfect-main.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION perfect
           .
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i                      PIC 9(8).

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 2 BY 1 UNTIL 33550337 = i
               IF FUNCTION perfect(i) = 0
                   DISPLAY i
               END-IF
           END-PERFORM

           GOBACK
           .
       END PROGRAM perfect-main.
```


perfect.cbl:

```cobol
       IDENTIFICATION DIVISION.
       FUNCTION-ID. perfect.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  max-val                 PIC 9(8).
       01  total                   PIC 9(8) VALUE 1.
       01  i                       PIC 9(8).
       01  q                       PIC 9(8).

       LINKAGE SECTION.
       01  n                       PIC 9(8).
       01  is-perfect              PIC 9.

       PROCEDURE DIVISION USING VALUE n RETURNING is-perfect.
           COMPUTE max-val = FUNCTION INTEGER(FUNCTION SQRT(n)) + 1

           PERFORM VARYING i FROM 2 BY 1 UNTIL i = max-val
               IF FUNCTION MOD(n, i) = 0
                   ADD i TO total

                   DIVIDE n BY i GIVING q
                   IF q > i
                       ADD q TO total
                   END-IF
               END-IF
           END-PERFORM

           IF total = n
               MOVE 0 TO is-perfect
           ELSE
               MOVE 1 TO is-perfect
           END-IF

           GOBACK
           .
       END FUNCTION perfect.
```



## Common Lisp

```lisp
(defun perfectp (n)
  (= n (loop for i from 1 below n when (= 0 (mod n i)) sum i)))
```



## D


### Functional Version


```d
import std.stdio, std.algorithm, std.range;

bool isPerfectNumber1(in uint n) pure nothrow
in {
    assert(n > 0);
} body {
    return n == iota(1, n - 1).filter!(i => n % i == 0).sum;
}

void main() {
    iota(1, 10_000).filter!isPerfectNumber1.writeln;
}
```

```txt
[6, 28, 496, 8128]
```



### Faster Imperative Version

```d
import std.stdio, std.math, std.range, std.algorithm;

bool isPerfectNumber2(in int n) pure nothrow {
    if (n < 2)
        return false;

    int total = 1;
    foreach (immutable i; 2 .. cast(int)real(n).sqrt + 1)
        if (n % i == 0) {
            immutable int q = n / i;
            total += i;
            if (q > i)
                total += q;
        }

    return total == n;
}

void main() {
    10_000.iota.filter!isPerfectNumber2.writeln;
}
```

```txt
[6, 28, 496, 8128]
```

With a <code>33_550_337.iota</code> it outputs:

```txt
[6, 28, 496, 8128, 33550336]
```



## Dart


###  Explicit Iterative Version


```d
/*
 * Function to test if a number is a perfect number
 * A number is a perfect number if it is equal to the sum of all its divisors
 * Input: Positive integer n
 * Output: true if n is a perfect number, false otherwise
 */
bool isPerfect(int n){
    //Generate a list of integers in the range 1 to n-1 : [1, 2, ..., n-1]
    List<int> range = new List<int>.generate(n-1, (int i) => i+1);

    //Create a list that filters the divisors of n from range
    List<int> divisors = new List.from(range.where((i) => n%i == 0));

    //Sum the all the divisors
    int sumOfDivisors = 0;
    for (int i = 0; i < divisors.length; i++){
        sumOfDivisors = sumOfDivisors + divisors[i];
    }

    // A number is a perfect number if it is equal to the sum of its divisors
    // We return the test if n is equal to sumOfDivisors
    return n == sumOfDivisors;
}
```



###  Compact Version

```d
isPerfect(n) =>
    n == new List.generate(n-1, (i) => n%(i+1) == 0 ? i+1 : 0).fold(0, (p,n)=>p+n);
```


In either case, if we test to find all the perfect numbers up to 1000, we get:

```d
main() =>
    new List.generate(1000,(i)=>i+1).where(isPerfect).forEach(print);
```

```txt
6
28
496
```



## Dyalect



```dyalect
func isPerfect(num) {
    var sum = 0
    for i in 1..(num - 1) {
        if !i {
            break
        }
        if num % i == 0 {
            sum += i
        }
    }
    return sum == num
}

const max = 33550337
print("Perfect numbers from 0 to \(max):")

for x in 0..max {
    if isPerfect(x) {
        print("\(x) is perfect")
    }
}
```



## E


```e
pragma.enable("accumulator")
def isPerfectNumber(x :int) {
  var sum := 0
  for d ? (x % d <=> 0) in 1..!x {
    sum += d
    if (sum > x) { return false }
  }
  return sum <=> x
}
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			io.put_string ("  6 is perfect...%T")
			io.put_boolean (is_perfect_number (6))
			io.new_line
			io.put_string (" 77 is perfect...%T")
			io.put_boolean (is_perfect_number (77))
			io.new_line
			io.put_string ("128 is perfect...%T")
			io.put_boolean (is_perfect_number (128))
			io.new_line
			io.put_string ("496 is perfect...%T")
			io.put_boolean (is_perfect_number (496))
		end

	is_perfect_number (n: INTEGER): BOOLEAN
			-- Is 'n' a perfect number?
		require
			n_positive: n > 0
		local
			sum: INTEGER
		do
			across
				1 |..| (n - 1) as c
			loop
				if n \\ c.item = 0 then
					sum := sum + c.item
				end
			end
			Result := sum = n
		end

end

```

```txt

  6 is perfect...      True
 77 is perfect...      False
128 is perfect...      False
496 is perfect...      True

```


## Elena

ELENA 4.x:

```elena
import system'routines;
import system'math;
import extensions;

extension extension
{
    isPerfect()
        = new Range(1, self - 1).selectBy:(n => (self.mod:n == 0).iif(n,0) ).summarize(new Integer()) == self;
}

public program()
{
    for(int n := 1, n < 10000, n += 1)
    {
        if(n.isPerfect())
            { console.printLine(n," is perfect") }
    };

    console.readChar()
}
```

```txt

6 is perfect
28 is perfect
496 is perfect
8128 is perfect

```



## Elixir


```elixir
defmodule RC do
  def is_perfect(1), do: false
  def is_perfect(n) when n > 1 do
    Enum.sum(factor(n, 2, [1])) == n
  end

  defp factor(n, i, factors) when n <  i*i   , do: factors
  defp factor(n, i, factors) when n == i*i   , do: [i | factors]
  defp factor(n, i, factors) when rem(n,i)==0, do: factor(n, i+1, [i, div(n,i) | factors])
  defp factor(n, i, factors)                 , do: factor(n, i+1, factors)
end

IO.inspect (for i <- 1..10000, RC.is_perfect(i), do: i)
```


```txt

[6, 28, 496, 8128]

```



## Erlang


```erlang
is_perfect(X) ->
    X == lists:sum([N || N <- lists:seq(1,X-1), X rem N == 0]).
```



## ERRE


```ERRE
PROGRAM PERFECT

PROCEDURE PERFECT(N%->OK%)
      LOCAL I%,S%
      S%=1
      FOR I%=2 TO SQR(N%)-1 DO
        IF N% MOD I%=0 THEN S%+=I%+N% DIV I%
      END FOR
      IF I%=SQR(N%) THEN S%+=I%
      OK%=(N%=S%)
END PROCEDURE

BEGIN
    PRINT(CHR$(12);) ! CLS
    FOR N%=2 TO 10000 STEP 2 DO
       PERFECT(N%->OK%)
       IF OK% THEN PRINT(N%)
    END FOR
END PROGRAM
```

```txt

         6
        28
       496
      8128

```


=={{header|F_Sharp|F#}}==

```fsharp
let perf n = n = List.fold (+) 0 (List.filter (fun i -> n % i = 0) [1..(n-1)])

for i in 1..10000 do if (perf i) then printfn "%i is perfect" i
```

```txt
6 is perfect
28 is perfect
496 is perfect
8128 is perfect
```



## FALSE


```false
[0\1[\$@$@-][\$@$@$@$@\/*=[@\$@+@@]?1+]#%=]p:
45p;!." "28p;!.   { 0 -1 }
```



## Factor


```factor
USING: kernel math math.primes.factors sequences ;
IN: rosettacode.perfect-numbers

: perfect? ( n -- ? )  [ divisors sum ] [ 2 * ] bi = ;
```



## Forth


```forth
: perfect? ( n -- ? )
  1
  over 2/ 1+ 2 ?do
    over i mod 0= if i + then
  loop
  = ;
```



## Fortran

```fortran
FUNCTION isPerfect(n)
  LOGICAL :: isPerfect
  INTEGER, INTENT(IN) :: n
  INTEGER :: i, factorsum

  isPerfect = .FALSE.
  factorsum = 1
  DO i = 2, INT(SQRT(REAL(n)))
     IF(MOD(n, i) == 0) factorsum = factorsum + i + (n / i)
  END DO
  IF (factorsum == n) isPerfect = .TRUE.
END FUNCTION isPerfect
```



## FreeBASIC

```freebasic
' FB 1.05.0 Win64

Function isPerfect(n As Integer) As Boolean
   If n < 2 Then Return False
   If n Mod 2 = 1 Then Return False '' we can assume odd numbers are not perfect
   Dim As Integer sum = 1, q
   For i As Integer = 2 To Sqr(n)
     If n Mod i = 0 Then
       sum += i
       q = n \ i
       If q > i Then sum += q
     End If
   Next
   Return n = sum
End Function

Print "The first 5 perfect numbers are : "
For i As Integer = 2 To 33550336
  If isPerfect(i) Then Print i; " ";
Next

Print
Print "Press any key to quit"
Sleep
```


```txt

The first 5 perfect numbers are :
 6  28  496  8128  33550336

```



## FunL


```funl
def perfect( n ) = sum( d | d <- 1..n if d|n ) == 2n

println( (1..500).filter(perfect) )
```


```txt

(6, 28, 496)

```



## GAP


```gap
Filtered([1 .. 10000], n -> Sum(DivisorsInt(n)) = 2*n);
# [ 6, 28, 496, 8128 ]
```



## Go



```go
package main

import "fmt"

func computePerfect(n int64) bool {
    var sum int64
    for i := int64(1); i < n; i++ {
        if n%i == 0 {
            sum += i
        }
    }
    return sum == n
}

// following function satisfies the task, returning true for all
// perfect numbers representable in the argument type
func isPerfect(n int64) bool {
    switch n {
    case 6, 28, 496, 8128, 33550336, 8589869056,
        137438691328, 2305843008139952128:
        return true
    }
    return false
}

// validation
func main() {
    for n := int64(1); ; n++ {
        if isPerfect(n) != computePerfect(n) {
            panic("bug")
        }
        if n%1e3 == 0 {
            fmt.Println("tested", n)
        }
    }
}


```

```txt

tested 1000
tested 2000
tested 3000
...

```



## Groovy

Solution:

```groovy
def isPerfect = { n ->
    n > 4 && (n == (2..Math.sqrt(n)).findAll { n % it == 0 }.inject(1) { factorSum, i -> factorSum += i + n/i })
}
```

Test program:

```groovy
(0..10000).findAll { isPerfect(it) }.each { println it }
```

```txt
6
28
496
8128
```



## Haskell


```haskell
perfect n =
    n == sum [i | i <- [1..n-1], n `mod` i == 0]
```


Create a list of known perfects:

```haskell
perfect =
  (\x -> (2 ^ x - 1) * (2 ^ (x - 1))) <$>
  filter (\x -> isPrime x && isPrime (2 ^ x - 1)) maybe_prime
  where
    maybe_prime = scanl1 (+) (2 : 1 : cycle [2, 2, 4, 2, 4, 2, 4, 6])
    isPrime n = all ((/= 0) . (n `mod`)) $ takeWhile (\x -> x * x <= n) maybe_prime

isPerfect n = f n perfect
  where
    f n (p:ps) =
      case compare n p of
        EQ -> True
        LT -> False
        GT -> f n ps

main :: IO ()
main = do
  mapM_ print $ take 10 perfect
  mapM_ (print . (\x -> (x, isPerfect x))) [6, 27, 28, 29, 496, 8128, 8129]
```



or, restricting the search space to improve performance:

```haskell
isPerfect :: Int -> Bool
isPerfect n =
  let lows = filter ((0 ==) . rem n) [1 .. floor (sqrt (fromIntegral n))]
  in 1 < n &&
     n ==
     quot
       (sum
          (lows ++
           [ y
           | x <- lows
           , let y = quot n x
           , x /= y ]))
       2

main :: IO ()
main = print $ filter isPerfect [1 .. 10000]
```

```txt
[6,28,496,8128]
```



## HicEst


```HicEst
   DO i = 1, 1E4
      IF( perfect(i) ) WRITE() i
   ENDDO
END ! end of "main"

FUNCTION perfect(n)
   sum = 1
   DO i = 2, n^0.5
      sum = sum + (MOD(n, i) == 0) * (i + INT(n/i))
   ENDDO
   perfect = sum == n
END
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
limit := \arglist[1] | 100000
write("Perfect numbers from 1 to ",limit,":")
every write(isperfect(1 to limit))
write("Done.")
end

procedure isperfect(n)         #: returns n if n is perfect
local sum,i

every (sum := 0) +:= (n ~= divisors(n))
if sum = n then return n
end

link factors
```


{{libheader|Icon Programming Library}} [http://www.cs.arizona.edu/icon/library/src/procs/factors.icn Uses divisors from factors]

```txt
Perfect numbers from 1 to 100000:
6
28
496
8128
Done.
```



## J


```j
is_perfect=: +: = >:@#.~/.~&.q:@(6>.<.)
```


Examples of use, including extensions beyond those assumptions:

```j
   is_perfect 33550336
1
   I. is_perfect i. 100000
6 28 496 8128

   ] zero_through_twentynine =. i. 3 10
 0  1  2  3  4  5  6  7  8  9
10 11 12 13 14 15 16 17 18 19
20 21 22 23 24 25 26 27 28 29
   is_perfect zero_through_twentynine
0 0 0 0 0 0 1 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 1 0
   is_perfect 191561942608236107294793378084303638130997321548169216x
1
```


More efficient version based on [http://jsoftware.com/pipermail/programming/2014-June/037695.html comments] by Henry Rich and Roger Hui (comment train seeded by Jon Hough).


## Java


```java
public static boolean perf(int n){
	int sum= 0;
	for(int i= 1;i < n;i++){
		if(n % i == 0){
			sum+= i;
		}
	}
	return sum == n;
}
```

Or for arbitrary precision:[[Category:Arbitrary precision]]

```java
import java.math.BigInteger;

public static boolean perf(BigInteger n){
	BigInteger sum= BigInteger.ZERO;
	for(BigInteger i= BigInteger.ONE;
	i.compareTo(n) < 0;i=i.add(BigInteger.ONE)){
		if(n.mod(i).equals(BigInteger.ZERO)){
			sum= sum.add(i);
		}
	}
	return sum.equals(n);
}
```



## JavaScript



### Imperative


```javascript
function is_perfect(n)
{
 var sum = 1, i, sqrt=Math.floor(Math.sqrt(n));
 for (i = sqrt-1; i>1; i--)
 {
  if (n % i == 0) {
   sum += i + n/i;
  }
 }
 if(n % sqrt == 0)
  sum += sqrt + (sqrt*sqrt == n ? 0 : n/sqrt);
 return sum === n;
}


var i;
for (i = 1; i < 10000; i++)
{
 if (is_perfect(i))
  print(i);
}
```


```txt
6
28
496
8128
```



### Functional



### =ES5=


Naive version (brute force)


```JavaScript
(function (nFrom, nTo) {

  function perfect(n) {
    return n === range(1, n - 1).reduce(
      function (a, x) {
        return n % x ? a : a + x;
      }, 0
    );
  }

  function range(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
      return m + i;
    });
  }

  return range(nFrom, nTo).filter(perfect);

})(1, 10000);
```


Output:


```JavaScript
[6, 28, 496, 8128]
```


Much faster (more efficient factorisation)


```JavaScript
(function (nFrom, nTo) {

  function perfect(n) {
    var lows = range(1, Math.floor(Math.sqrt(n))).filter(function (x) {
      return (n % x) === 0;
    });

    return n > 1 && lows.concat(lows.map(function (x) {
      return n / x;
    })).reduce(function (a, x) {
      return a + x;
    }, 0) / 2 === n;
  }

  function range(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
      return m + i;
    });
  }

  return range(nFrom, nTo).filter(perfect)

})(1, 10000);
```


Output:


```JavaScript
[6, 28, 496, 8128]
```


Note that the filter function, though convenient and well optimised, is not strictly necessary.
We can always replace it with a more general monadic bind (chain) function, which is essentially just concat map
(Monadic return/inject for lists is simply lambda x --> [x], inlined here, and fail is [].)


```JavaScript
(function (nFrom, nTo) {

  // MONADIC CHAIN (bind) IN LIEU OF FILTER
  // ( monadic return for lists is just lambda x -> [x] )

  return chain(
    rng(nFrom, nTo),

    function mPerfect(n) {
      return (chain(
        rng(1, Math.floor(Math.sqrt(n))),
        function (y) {
          return (n % y) === 0 && n > 1 ? [y, n / y] : [];
        }
      ).reduce(function (a, x) {
        return a + x;
      }, 0) / 2 === n) ? [n] : [];
    }

  );

  /******************************************************************/

  // Monadic bind (chain) for lists
  function chain(xs, f) {
    return [].concat.apply([], xs.map(f));
  }

  function rng(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
      return m + i;
    });
  }

})(1, 10000);
```


Output:

```JavaScript
[6, 28, 496, 8128]
```




### =ES6=



```JavaScript
(() => {
    const main = () =>
        enumFromTo(1, 10000).filter(perfect);

    // perfect :: Int -> Bool
    const perfect = n => {
        const
            lows = enumFromTo(1, Math.floor(Math.sqrt(n)))
            .filter(x => (n % x) === 0);

        return n > 1 && lows.concat(lows.map(x => n / x))
            .reduce((a, x) => (a + x), 0) / 2 === n;
    };

    // GENERIC --------------------------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: n - m + 1
        }, (_, i) => i + m)

    // MAIN ---
    return main();
})();
```


```JavaScript
[6, 28, 496, 8128]
```



## jq


```jq

def is_perfect:
  . as $in
  | $in == reduce range(1;$in) as $i
      (0; if ($in % $i) == 0 then $i + . else . end);

# Example:
range(1;10001) | select( is_perfect )
```

 $ jq -n -f is_perfect.jq
 6
 28
 496
 8128


## Julia

```julia
isperfect(n::Integer) = n == sum([n % i == 0 ? i : 0 for i = 1:(n - 1)])
perfects(n::Integer) = filter(isperfect, 1:n)

@show perfects(10000)
```


```txt
perfects(10000) = [6, 28, 496, 8128]
```



## K

```K
   perfect:{(x>2)&x=+/-1_{d:&~x!'!1+_sqrt x;d,_ x%|d}x}
   perfect 33550336
1

   a@&perfect'a:!10000
6 28 496 8128

   m:3 10#!30
(0 1 2 3 4 5 6 7 8 9
 10 11 12 13 14 15 16 17 18 19
 20 21 22 23 24 25 26 27 28 29)

   perfect'/: m
(0 0 0 0 0 0 1 0 0 0
 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 1 0)
```



## Kotlin

```scala
// version 1.0.6

fun isPerfect(n: Int): Boolean = when {
        n < 2      -> false
        n % 2 == 1 -> false  // there are no known odd perfect numbers
        else       -> {
            var tot = 1
            var q: Int
            for (i in 2 .. Math.sqrt(n.toDouble()).toInt()) {
                if (n % i == 0) {
                    tot += i
                    q = n / i
                    if (q > i) tot += q
                }
            }
            n == tot
        }
    }

fun main(args: Array<String>) {
    // expect a run time of about 6 minutes on a typical laptop
    println("The first five perfect numbers are:")
    for (i in 2 .. 33550336) if (isPerfect(i)) print("$i ")
}
```


```txt

The first five perfect numbers are:
6 28 496 8128 33550336

```



## LabVIEW

## Lasso


```lasso
#!/usr/bin/lasso9

define isPerfect(n::integer) => {
  #n < 2 ? return false
  return #n == (
    with i in generateSeries(1, math_floor(math_sqrt(#n)) + 1)
      where #n % #i == 0
      let q = #n / #i
    sum (#q > #i ? (#i == 1 ? 1 | #q + #i) | 0)
  )
}

with x in generateSeries(1, 10000)
  where isPerfect(#x)
select #x
```

```lasso
6, 28, 496, 8128
```



## Liberty BASIC


```lb
for n =1 to 10000
    if perfect( n) =1 then print n; " is perfect."
next n

end

function perfect( n)
    sum =0
    for i =1 TO n /2
        if n mod i =0 then
            sum =sum +i
        end if
    next i
    if sum =n then
        perfect= 1
    else
        perfect =0
    end if
end function
```



## Lingo


```lingo
on isPercect (n)
  sum = 1
  cnt = n/2
  repeat with i = 2 to cnt
    if n mod i = 0 then sum = sum + i
  end repeat
  return sum=n
end
```



## Logo


```logo
to perfect? :n
  output equal? :n  apply "sum  filter [equal? 0  modulo :n ?]  iseq 1 :n/2
end
```



## Lua


```Lua
function isPerfect(x)
    local sum = 0
    for i = 1, x-1 do
	sum = (x % i) == 0 and sum + i or sum
    end
    return sum == x
end
```



## M2000 Interpreter


```M2000 Interpreter

Module PerfectNumbers {
      Function Is_Perfect(n as decimal) {
            s=1 : sN=Sqrt(n)
            last= n=sN*sN
            t=n
            If n mod 2=0 then s+=2+n div 2
            i=3 : sN--
            While i<sN {
            if  n mod i=0 then t=n div i :i=max.data(n div t, i): s+=t+ i
            i++
            }
            =n=s
      }
      Inventory Known1=2@, 3@
      IsPrime=lambda  Known1 (x as decimal) -> {
                  =0=1
                  if exist(Known1, x) then =1=1 : exit
                  if x<=5 OR frac(x) then {if x == 2 OR x == 3 OR x == 5 then Append Known1, x  : =1=1
                  Break}
                  if frac(x/2) else exit
                  if frac(x/3) else exit
                  x1=sqrt(x):d = 5@
                  {if frac(x/d ) else exit
                        d += 2: if d>x1 then Append Known1, x : =1=1 : exit
                        if frac(x/d) else exit
                        d += 4: if d<= x1 else Append Known1, x :  =1=1: exit
                   loop}
            }
      \\ Check a perfect and a non perfect number
      p=2 : n=3 : n1=2
      Document Doc$
      IsPerfect( 0, 28)
      IsPerfect( 0, 1544)
      While p<32  { ' max 32
            if isprime(2^p-1@) then {
                   perf=(2^p-1@)*2@^(p-1@)
                   Rem  Print perf
                   \\ decompose pretty fast the Perferct Numbers
                   \\ all have a series of 2 and last a prime equal to perf/2^(p-1)
                   inventory queue factors
                   For i=1 to p-1 {
                         Append factors, 2@
                  }
                  Append factors, perf/2^(p-1)
                  \\ end decompose
                  Rem Print factors
                  IsPerfect(factors, Perf)
            }
            p++
      }

      Clipboard Doc$
      \\ exit here. No need for Exit statement
      Sub IsPerfect(factors, n)
            s=false
            if n<10000 or type$(factors)<>"Inventory" then {
                  s=Is_Perfect(n)
            } else {
                  local mm=each(factors, 1, -2), f =true
                  while mm {if eval(mm)<>2 then f=false
                  }
                  if f then if n/2@**(len(mm)-1)= factors(len(factors)-1!) then s=true
            }
            Local a$=format$("{0} is {1}perfect number", n, If$(s->"", "not "))
            Doc$=a$+{
            }
            Print a$
      End Sub
}

PerfectNumbers

```


<pre style="height:30ex;overflow:scroll">
28 is perfect number
1544 is not perfect number
6 is perfect number
28 is perfect number
496 is perfect number
8128 is perfect number
33550336 is perfect number
8589869056 is perfect number
137438691328 is perfect number
2305843008139952128 is perfect number


</pre >


## M4


```M4
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')dnl

define(`ispart',
   `ifelse(eval($2*$2<=$1),1,
      `ifelse(eval($1%$2==0),1,
         `ifelse(eval($2*$2==$1),1,
            `ispart($1,incr($2),eval($3+$2))',
            `ispart($1,incr($2),eval($3+$2+$1/$2))')',
         `ispart($1,incr($2),$3)')',
      $3)')
define(`isperfect',
   `eval(ispart($1,2,1)==$1)')

for(`x',`2',`33550336',
   `ifelse(isperfect(x),1,`x
')')
```



## Maple


```Maple
isperfect := proc(n) return evalb(NumberTheory:-SumOfDivisors(n) = 2*n); end proc:
isperfect(6);
                              true
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Custom function:

```Mathematica
PerfectQ[i_Integer] := Total[Divisors[i]] == 2 i
```

Examples (testing 496, testing 128, finding all perfect numbers in 1...10000):

```Mathematica
PerfectQ[496]
PerfectQ[128]
Flatten[PerfectQ/@Range[10000]//Position[#,True]&]
```

gives back:

```Mathematica
True
False
{6,28,496,8128}
```



## MATLAB

Standard algorithm:

```MATLAB
function perf = isPerfect(n)
    total = 0;
    for k = 1:n-1
        if ~mod(n, k)
            total = total+k;
        end
    end
    perf = total == n;
end
```

Faster algorithm:

```MATLAB
function perf = isPerfect(n)
    if n < 2
        perf = false;
    else
        total = 1;
        k = 2;
        quot = n;
        while k < quot && total <= n
            if ~mod(n, k)
                total = total+k;
                quot = n/k;
                if quot ~= k
                    total = total+quot;
                end
            end
            k = k+1;
        end
        perf = total == n;
    end
end
```



## Maxima


```maxima
".."(a, b) := makelist(i, i, a, b)$
infix("..")$

perfectp(n) := is(divsum(n) = 2*n)$

sublist(1 .. 10000, perfectp);
/* [6, 28, 496, 8128] */
```



## MAXScript


```maxscript
fn isPerfect n =
(
    local sum = 0
    for i in 1 to (n-1) do
    (
        if mod n i == 0 then
        (
            sum += i
        )
    )
    sum == n
)
```



## Microsoft Small Basic

```microsoftsmallbasic

For n = 2 To 10000 Step 2
  VerifyIfPerfect()
  If isPerfect = 1 Then
    TextWindow.WriteLine(n)
  EndIf
EndFor

Sub VerifyIfPerfect
  s = 1
  sqrN = Math.SquareRoot(n)
  If Math.Remainder(n, 2) = 0 Then
    s = s + 2 + Math.Floor(n / 2)
  EndIf
  i = 3
  while i <= sqrN - 1
    If Math.Remainder(n, i) = 0 Then
      s = s + i + Math.Floor(n / i)
    EndIf
    i = i + 1
  EndWhile
  If i * i = n Then
    s = s + i
  EndIf
  If n = s Then
    isPerfect = 1
  Else
    isPerfect = 0
  EndIf
EndSub

```


=={{header|Modula-2}}==
```modula2

MODULE PerfectNumbers;

FROM SWholeIO IMPORT
  WriteCard;
FROM STextIO IMPORT
  WriteLn;
FROM RealMath IMPORT
  sqrt;

VAR
  N: CARDINAL;

PROCEDURE IsPerfect(N: CARDINAL): BOOLEAN;
VAR
  S, I: CARDINAL;
  SqrtN: REAL;
BEGIN
  S := 1;
  SqrtN := sqrt(FLOAT(N));
  IF N REM 2 = 0 THEN
    S := S + 2 + N / 2;
  END;
  I := 3;
  WHILE FLOAT(I) <= SqrtN - 1.0 DO
    IF N REM I = 0 THEN
      S := S + I + N / I;
    END;
    I := I + 1;
  END;
  IF I * I = N THEN
    S := S + I;
  END;
  RETURN (N = S);
END IsPerfect;

BEGIN
  FOR N := 2 TO 10000 BY 2 DO
    IF IsPerfect(N) THEN
      WriteCard(N, 5);
      WriteLn;
    END;
  END;
END PerfectNumbers.

```



## Nim


```nim
import math

proc isPerfect(n: int): bool =
    var sum: int = 1
    for i in 2 .. <(n.toFloat.sqrt+1).toInt:
        if n mod i == 0:
            sum += (i + n div i)
    return (n == sum)

for i in 2..10_000:
    if isPerfect(i):
        echo(i)
```



## Objeck


```objeck
bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      "Perfect numbers from 1 to 33550337:"->PrintLine();
      for(num := 1 ; num < 33550337; num += 1;) {
        if(IsPerfect(num)) {
          num->PrintLine();
        };
      };
    }

    function : native : IsPerfect(number : Int) ~ Bool {
      sum := 0 ;
      for(i := 1; i < number; i += 1;) {
        if (number % i = 0) {
          sum += i;
        };
      };

      return sum = number;
    }
  }
}
```



## OCaml


```ocaml
let perf n =
  let sum = ref 0 in
    for i = 1 to n-1 do
      if n mod i = 0 then
        sum := !sum + i
    done;
    !sum = n
```

Functional style:

```ocaml
(* range operator *)
let rec (--) a b =
  if a > b then
    []
  else
    a :: (a+1) -- b

let perf n = n = List.fold_left (+) 0 (List.filter (fun i -> n mod i = 0) (1 -- (n-1)))
```




## Oforth



```Oforth
: isPerfect(n)  | i | 0 n 2 / loop: i [ n i mod ifZero: [ i + ] ] n == ;
```


```txt

#isPerfect 10000 seq filter .
[6, 28, 496, 8128]

```



## ooRexx


```ooRexx
-- first perfect number over 10000 is 33550336...let's not be crazy
loop i = 1 to 10000
    if perfectNumber(i) then say i "is a perfect number"
end

::routine perfectNumber
  use strict arg n

  sum = 0

  -- the largest possible factor is n % 2, so no point in
  -- going higher than that
  loop i = 1 to n % 2
      if n // i == 0 then sum += i
  end

  return sum = n
```

```txt
6 is a perfect number
28 is a perfect number
496 is a perfect number
8128 is a perfect number
```



## Oz


```oz
declare
  fun {IsPerfect N}
     fun {IsNFactor I} N mod I == 0 end
     Factors = {Filter {List.number 1 N-1 1} IsNFactor}
  in
     {Sum Factors} == N
  end

  fun {Sum Xs} {FoldL Xs Number.'+' 0} end
in
  {Show {Filter {List.number 1 10000 1} IsPerfect}}
  {Show {IsPerfect 33550336}}
```



## PARI/GP

Uses built-in method. Faster tests would use the LL test for evens and myriad results on OPNs otherwise.

```parigp
isPerfect(n)=sigma(n,-1)==2
```

Show perfect numbers

```parigp
forprime(p=2, 2281,
	if(isprime(2^p-1),
		print(p"\t",(2^p-1)*2^(p-1))))
```

Faster with Lucas-Lehmer test

```parigp
p=2;n=3;n1=2;
while(p<2281,
	if(isprime(p),
		s=Mod(4,n);
		for(i=3,p,
			s=s*s-2);
		if(s==0 || p==2,
			print("(2^"p"-1)2^("p"-1)=\t"n1*n"\n")));
	p++; n1=n+1; n=2*n+1)
```

```txt
(2^2-1)2^(2-1)= 6
(2^3-1)2^(3-1)= 28
(2^5-1)2^(5-1)= 496
(2^7-1)2^(7-1)= 8128
(2^13-1)2^(13-1)=       33550336
(2^17-1)2^(17-1)=       8589869056
(2^19-1)2^(19-1)=       137438691328
(2^31-1)2^(31-1)=       2305843008139952128
(2^61-1)2^(61-1)=       2658455991569831744654692615953842176
(2^89-1)2^(89-1)=       191561942608236107294793378084303638130997321548169216
```



## Pascal


```pascal
program PerfectNumbers;

 function isPerfect(number: longint): boolean;
 var
  i, sum: longint;

 begin
  sum := 1;
  for i := 2 to round(sqrt(real(number))) do
    if (number mod i = 0) then
     sum := sum + i + (number div i);
  isPerfect := (sum = number);
 end;

var
 candidate: longint;

begin
 writeln('Perfect numbers from 1 to 33550337:');
 for candidate := 2 to 33550337 do
   if isPerfect(candidate) then
    writeln (candidate, ' is a perfect number.');
end.
```

```txt

Perfect numbers from 1 to 33550337:
6 is a perfect number.
28 is a perfect number.
496 is a perfect number.
8128 is a perfect number.
33550336 is a perfect number.

```



## Perl


###  Functions


```perl
sub perf {
    my $n = shift;
    my $sum = 0;
    foreach my $i (1..$n-1) {
        if ($n % $i == 0) {
            $sum += $i;
        }
    }
    return $sum == $n;
}
```

Functional style:

```perl
use List::Util qw(sum);

sub perf {
    my $n = shift;
    $n == sum(0, grep {$n % $_ == 0} 1..$n-1);
}
```


###  Modules

The functions above are terribly slow.  As usual, this is easier and faster with modules.  Both ntheory and Math::Pari have useful functions for this.
A simple predicate:

```perl
use ntheory qw/divisor_sum/;
sub is_perfect { my $n = shift;  divisor_sum($n) == 2*$n; }
```

Use this naive method to show the first 5.  Takes about 15 seconds:

```perl
use ntheory qw/divisor_sum/;
for (1..33550336) {
  print "$_\n" if divisor_sum($_) == 2*$_;
}
```

Or we can be clever and look for 2^(p-1) * (2^p-1) where 2^p -1 is prime.  The first 20 takes about a second.

```perl
use ntheory qw/forprimes is_prime/;
use bigint;
forprimes {
  my $n = 2**$_ - 1;
  print "$_\t", $n * 2**($_-1),"\n"   if is_prime($n);
} 2, 4500;
```

```txt

2	6
3	28
5	496
7	8128
13	33550336
17	8589869056
19	137438691328
31	2305843008139952128
61	2658455991569831744654692615953842176
89	191561942608236107294793378084303638130997321548169216
... 107, 127, 521, 607, 1279, 2203, 2281, 3217, 4253, 4423 ...

```


We can speed this up even more using a faster program for printing the large results, as well as a faster primality solution.  The first 38 in about 1 second with most of the time printing the large results.  Caveat: this goes well past the current bound for odd perfect numbers and does not check for them.

```perl
use ntheory qw/forprimes is_mersenne_prime/;
use Math::GMP qw/:constant/;
forprimes {
  print "$_\t", (2**$_-1)*2**($_-1),"\n"  if is_mersenne_prime($_);
} 7_000_000;
```


In addition to generating even perfect numbers, we can also have a fast function which returns true when a given even number is perfect:

```perl
use ntheory qw(is_mersenne_prime valuation);

sub is_even_perfect {
    my ($n) = @_;
    my $v = valuation($n, 2) || return;
    my $m = ($n >> $v);
    ($m & ($m + 1)) && return;
    ($m >> $v) == 1 || return;
    is_mersenne_prime($v + 1);
}
```



## Perl 6

Naive (very slow) version

```perl6
sub is-perf($n) { $n == [+] grep $n %% *, 1 .. $n div 2 }

# used as
put ((1..Inf).hyper.grep: {.&is-perf})[^4];
```

```txt
6 28 496 8128
```

Much, much faster version:

```perl6
my @primes   = lazy (2,3,*+2 … Inf).grep: { .is-prime };
my @perfects = lazy gather for @primes {
    my $n = 2**$_ - 1;
    take $n * 2**($_ - 1) if $n.is-prime;
}

.put for @perfects[^12];
```


```txt
6
28
496
8128
33550336
8589869056
137438691328
2305843008139952128
2658455991569831744654692615953842176
191561942608236107294793378084303638130997321548169216
13164036458569648337239753460458722910223472318386943117783728128
14474011154664524427946373126085988481573677491474835889066354349131199152128
```



## Phix


```Phix
function is_perfect(integer n)
    return sum(factors(n,-1))=n
end function

for i=2 to 100000 do
    if is_perfect(i) then ?i end if
end for
```

```txt

6
28
496
8128

```


###  gmp version

```Phix
include mpfr.e
mpz n = mpz_init(), p = mpz_init()
randstate state =  gmp_randinit_mt()
for i=2 to 159 do
    mpz_ui_pow_ui(n, 2, i)
    mpz_sub_ui(n, n, 1)
    if mpz_probable_prime_p(n, state) then
        mpz_ui_pow_ui(p,2,i-1)
        mpz_mul(n,n,p)
        printf(1, "%d  %s\n",{i,mpz_get_str(n,comma_fill:=true)})
    end if
end for
n = mpz_free(n)
state =  gmp_randclear(state)
```

```txt

2  6
3  28
5  496
7  8,128
13  33,550,336
17  8,589,869,056
19  137,438,691,328
31  2,305,843,008,139,952,128
61  2,658,455,991,569,831,744,654,692,615,953,842,176
89  191,561,942,608,236,107,294,793,378,084,303,638,130,997,321,548,169,216
107  13,164,036,458,569,648,337,239,753,460,458,722,910,223,472,318,386,943,117,783,728,128
127  14,474,011,154,664,524,427,946,373,126,085,988,481,573,677,491,474,835,889,066,354,349,131,199,152,128

```



## PHP

```php
function is_perfect($number)
{
    $sum = 0;
    for($i = 1; $i < $number; $i++)
    {
        if($number % $i == 0)
            $sum += $i;
    }
    return $sum == $number;
}

echo "Perfect numbers from 1 to 33550337:" . PHP_EOL;
for($num = 1; $num < 33550337; $num++)
{
    if(is_perfect($num))
        echo $num . PHP_EOL;
}
```



## PicoLisp


```PicoLisp
(de perfect (N)
   (let C 0
      (for I (/ N 2)
         (and (=0 (% N I)) (inc 'C I)) )
      (= C N) ) )
```



```PicoLisp
(de faster (N)
   (let (C 1  Stop (sqrt N))
      (for (I 2 (<= I Stop) (inc I))
         (and
            (=0 (% N I))
            (inc 'C (+ (/ N I) I)) ) )
      (= C N) ) )
```



## PL/I


```PL/I
perfect: procedure (n) returns (bit(1));
   declare n fixed;
   declare sum fixed;
   declare i fixed binary;

   sum = 0;
   do i = 1 to n-1;
      if mod(n, i) = 0 then sum = sum + i;
   end;
   return (sum=n);
end perfect;
```



## PowerShell


```powershell
Function IsPerfect($n)
{
$sum=0
 for($i=1;$i-lt$n;$i++)
 {
  if($n%$i -eq 0)
  {
  $sum += $i
  }
 }
return $sum -eq $n
}

Returns "True" if the given number is perfect and "False" if it's not.
```



## Prolog


### Classic approach

Works with SWI-Prolog

```Prolog
tt_divisors(X, N, TT) :-
	Q is X / N,
	(   0 is X mod N -> (Q = N -> TT1 is N + TT;
                             TT1 is N + Q + TT);
            TT = TT1),
	(   sqrt(X) > N + 1 -> N1 is N+1, tt_divisors(X, N1, TT1);
	    TT1 = X).

perfect(X) :-
	tt_divisors(X, 2, 1).

perfect_numbers(N, L) :-
	numlist(2, N, LN),
	include(perfect, LN, L).
```



### Faster method

Since a perfect number is of the form 2^(n-1) * (2^n - 1), we can eliminate a lot of candidates by merely factoring out the 2s and seeing if the odd portion is (2^(n+1)) - 1.

```Prolog

perfect(N) :-
   factor_2s(N, Chk, Exp),
   Chk =:= (1 << (Exp+1)) - 1,
   prime(Chk).

factor_2s(N, S, D) :- factor_2s(N, 0, S, D).

factor_2s(D, S, D, S) :- getbit(D, 0) =:= 1, !.
factor_2s(N, E, D, S) :-
   E2 is E + 1, N2 is N >> 1, factor_2s(N2, E2, D, S).

% check if a number is prime
%
wheel235(L) :-
   W = [4, 2, 4, 2, 4, 6, 2, 6 | W],
   L = [1, 2, 2 | W].

prime(N) :- N < 2, !, false.
prime(N) :-
   wheel235(W),
   prime(N, 2, W).

prime(N, D, _) :- D*D > N, !.
prime(N, D, _) :- N mod D =:= 0, !, false.
prime(N, D, [A|As]) :- D2 is D + A, prime(N, D2, As).

```

```txt

?- between(1, 10_000, N), perfect(N).
N = 6 ;
N = 28 ;
N = 496 ;
N = 8128 ;
false.

```



### Functional approach

Works with SWI-Prolog and module lambda, written by <b>Ulrich Neumerkel</b> found there http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl

```Prolog
:- use_module(library(lambda)).

is_divisor(V, N) :-
	0 =:= V mod N.

is_perfect(N) :-
	N1 is floor(N/2),
	numlist(1, N1, L),
	f_compose_1(foldl((\X^Y^Z^(Z is X+Y)), 0), filter(is_divisor(N)), F),
	call(F, L, N).

f_perfect_numbers(N, L) :-
	numlist(2, N, LN),
	filter(is_perfect, LN, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionnal predicates

%% foldl(Pred, Init, List, R).
%
foldl(_Pred, Val, [], Val).
foldl(Pred, Val, [H | T], Res) :-
	call(Pred, Val, H, Val1),
	foldl(Pred, Val1, T, Res).

%% filter(Pred, LstIn, LstOut)
%
filter(_Pre, [], []).

filter(Pred, [H|T], L) :-
	filter(Pred, T, L1),
	(   call(Pred,H) -> L = [H|L1]; L = L1).

%% f_compose_1(Pred1, Pred2, Pred1(Pred2)).
%
f_compose_1(F,G, \X^Z^(call(G,X,Y), call(F,Y,Z))).
```



## PureBasic


```PureBasic
Procedure is_Perfect_number(n)
  Protected summa, i=1, result=#False
  Repeat
    If Not n%i
      summa+i
    EndIf
    i+1
  Until i>=n
  If summa=n
    result=#True
  EndIf
  ProcedureReturn result
EndProcedure
```



## Python


### Procedural


```python
def perf(n):
    sum = 0
    for i in xrange(1, n):
        if n % i == 0:
            sum += i
    return sum == n
```



### Functional


```python
def perf(n):
    return n == sum(i for i in range(1, n) if n % i == 0)

print (
    list(filter(perf, range(1, 10001)))
)
```



Or, over 50X faster, as measured by ''time.time()'':


```python
'''Perfect numbers'''

from math import sqrt


# perfect :: Int - > Bool
def perfect(n):
    '''Is n the sum of its proper divisors other than 1 ?'''

    root = sqrt(n)
    lows = [x for x in enumFromTo(2)(int(root)) if 0 == (n % x)]
    return 1 < n and (
        n == 1 + sum(lows + [n / x for x in lows if root != x])
    )


# main :: IO ()
def main():
    '''Test'''

    print([
        x for x in enumFromTo(1)(10000) if perfect(x)
    ])


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


if __name__ == '__main__':
    main()
```

```txt
[6, 28, 496, 8128]
```



## R


```R
is.perf <- function(n){
	if (n==0|n==1) return(FALSE)
	s <- seq (1,n-1)
	x <- n %% s
	m <- data.frame(s,x)
	out <- with(m, s[x==0])
	return(sum(out)==n)
}
# Usage - Warning High Memory Usage
is.perf(28)
sapply(c(6,28,496,8128,33550336),is.perf)
```



## Racket


```racket
#lang racket
(require math)

(define (perfect? n)
  (=
   (* n 2)
   (sum (divisors n))))

; filtering to only even numbers for better performance
(filter perfect? (filter even? (range 1e5)))
;-> '(0 6 28 496 8128)
```



## REBOL


```rebol
perfect?:  func [n [integer!] /local sum] [
    sum: 0
    repeat i (n - 1) [
        if zero? remainder n i [
            sum: sum + i
        ]
    ]
    sum = n
]
```



## REXX


### Classic REXX version of ooRexx

This version is a '''Classic Rexx''' version of the '''ooRexx''' program as of 14-Sep-2013.

```rexx
/*REXX version of the  ooRexx  program (the code was modified to run with Classic REXX).*/
      do i=1  to 10000                                 /*statement changed:  LOOP ──► DO*/
      if perfectNumber(i)  then say  i   "is a perfect number"
      end
exit

perfectNumber: procedure; parse arg n                  /*statements changed: ROUTINE,USE*/
sum=0
             do i=1  to n%2                            /*statement changed:  LOOP ──► DO*/
             if n//i==0 then sum=sum+i                 /*statement changed:  sum += i   */
             end
return sum=n
```

'''output'''   when using the default of 10000:

```txt

6 is a perfect number
28 is a perfect number
496 is a perfect number
8128 is a perfect number

```



### Classic REXX version of PL/I

This version is a '''Classic REXX''' version of the '''PL/I''' program as of 14-Sep-2013,   a REXX   '''say'''   statement

was added to display the perfect numbers.   Also, an epilog was written for the re-worked function.

```rexx
/*REXX version of the  PL/I  program  (code was modified to run with Classic REXX).     */
parse arg low high .                                   /*obtain the specified number(s).*/
if high=='' & low==''  then high=34000000              /*if no arguments,  use a range. */
if  low==''            then  low=1                     /*if no   LOW, then assume unity.*/
if high==''            then high=low                   /*if no  HIGH, then assume  LOW. */

               do i=low  to high                       /*process the single # or range. */
               if perfect(i)  then say  i  'is a perfect number.'
               end   /*i*/
exit

perfect: procedure;  parse arg n                       /*get the number to be tested.   */
sum=0                                                  /*the sum of the factors so far. */
             do i=1  for n-1                           /*starting at 1, find all factors*/
             if n//i==0 then sum=sum+i                 /*I is a factor of N,  so add it.*/
             end   /*i*/
return sum=n                                           /*if the sum matches N, perfect! */
```

'''output'''   when using the input defaults of:   <tt> 1    10000 </tt>

The output is the same as for the ooRexx version (above).


### traditional method

Programming note:   this traditional method takes advantage of a few shortcuts:
:::*   testing only goes up to the (integer) square root of   '''X'''
:::*   testing bypasses the test of the first and last factors
:::*   the   ''corresponding factor''   is also used when a factor is found

```rexx
/*REXX program  tests  if a  number  (or a range of numbers)  is/are  perfect.          */
parse arg low high .                             /*obtain optional arguments from the CL*/
if high=='' & low==""  then high=34000000        /*if no arguments, then use a  range.  */
if  low==''            then  low=1               /*if no   LOW,  then assume  unity.    */
if high==''            then high=low             /*if no  HIGH,  then assume   LOW.     */
w=length(high)                                   /*use   W   for formatting the output. */
numeric digits max(9,w+2)                        /*ensure enough digits to handle number*/

            do i=low  to high                    /*process the single number or a range.*/
            if isPerfect(i)  then say  right(i,w)   'is a perfect number.'
            end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPerfect: procedure;  parse arg x               /*obtain the number to be tested.      */
           if x<6  then return 0                 /*perfect numbers can't be  <  six.    */
           s=1                                   /*the first factor of  X.           ___*/
                       do j=2  while  j*j<=x     /*starting at 2, find the factors ≤√ X */
                       if x//j\==0  then iterate /*J  isn't a factor of  X,  so skip it.*/
                       s = s + j + x%j           /* ··· add it  and  the other factor.  */
                       end   /*j*/               /*(above)  is marginally faster.       */
          return s==x                            /*if the sum matches  X, it's perfect! */
```

'''output'''   when using the default inputs:

```txt

       6 is a perfect number.
      28 is a perfect number.
     496 is a perfect number.
    8128 is a perfect number.
33550336 is a perfect number.

```

For 10,000 numbers tested, this version is   '''19.6'''   times faster than the ooRexx program logic.

For 10,000 numbers tested, this version is   '''25.6'''   times faster than the   PL/I   program logic.


Note:   For the above timings, only 10,000 numbers were tested.


### optimized using digital root

This REXX version makes use of the fact that all   ''known''   perfect numbers > 6 have a   ''digital root''   of   '''1'''.

```rexx
/*REXX program  tests  if a number  (or a range of numbers)  is/are  perfect.           */
parse arg low high .                             /*obtain the specified number(s).      */
if high=='' & low==""  then high=34000000        /*if no arguments,  then use a range.  */
if  low==''            then  low=1               /*if no   LOW,  then assume unity.     */
if high==''            then high=low             /*if no  HIGH,  then assume  LOW.      */
w=length(high)                                   /*use  W  for formatting the output.   */
numeric digits max(9,w+2)                        /*ensure enough digits to handle number*/

             do i=low  to high                   /*process the single number or a range.*/
             if isPerfect(i)  then say  right(i,w)  'is a perfect number.'
             end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPerfect: procedure;  parse arg x 1 y           /*obtain the number to be tested.      */
           if x==6  then return 1                /*handle the special case of  six.     */
                                                 /*[↓]  perfect number's digitalRoot = 1*/
                 do  until  y<10                 /*find the digital root of  Y.         */
                 parse var y r 2;   do k=2  for length(y)-1; r=r+substr(y,k,1); end  /*k*/
                 y=r                             /*find digital root of the digit root. */
                 end   /*until*/                 /*wash, rinse, repeat ···              */

           if r\==1  then return 0               /*Digital root ¬ 1?   Then  ¬ perfect. */
           s=1                                   /*the first factor of  X.           ___*/
                       do j=2  while  j*j<=x     /*starting at 2, find the factors ≤√ X */
                       if x//j\==0  then iterate /*J  isn't a factor of X,  so skip it. */
                       s = s + j + x%j           /*··· add it  and  the other factor.   */
                       end   /*j*/               /*(above)  is marginally faster.       */
           return s==x                           /*if the sum matches  X, it's perfect! */
```

'''output'''   is the same as the traditional version   and is about   '''5.3'''   times faster   (testing '''34,000,000''' numbers).


### optimized using only even numbers

This REXX version uses the fact that all   ''known''   perfect numbers are   ''even''.

```rexx
/*REXX program  tests  if a number  (or a range of numbers)  is/are  perfect.           */
parse arg low high .                             /*obtain optional arguments from the CL*/
if high=='' & low==""  then high=34000000        /*if no arguments,  then use a  range. */
if  low==''            then  low=1               /*if no   LOW,  then assume unity.     */
low=low+low//2                                   /*if LOW is odd,   bump it by  one.    */
if high==''            then high=low             /*if no  HIGH,   then assume  LOW.     */
w=length(high)                                   /*use  W  for formatting the output.   */
numeric digits max(9,w+2)                        /*ensure enough digits to handle number*/

            do i=low  to high  by 2              /*process the single number or a range.*/
            if isPerfect(i)  then say  right(i,w)   'is a perfect number.'
            end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPerfect: procedure;  parse arg x 1 y           /*obtain the number to be tested.      */
           if x==6  then return 1                /*handle the special case  of  six.    */

               do  until  y<10                   /*find the digital root of  Y.         */
               parse var y 1 r 2;    do k=2  for length(y)-1; r=r+substr(y,k,1); end /*k*/
               y=r                               /*find digital root of the digital root*/
               end   /*until*/                   /*wash, rinse, repeat ···              */

           if r\==1  then return 0               /*Digital root ¬ 1 ?    Then ¬ perfect.*/
           s=3 + x%2                             /*the first 3 factors of X.         ___*/
                       do j=3  while  j*j<=x     /*starting at 3, find the factors ≤√ X */
                       if x//j\==0  then iterate /*J  isn't a factor o f X,  so skip it.*/
                       s = s + j + x%j           /*  ··· add it  and  the other factor. */
                        end   /*j*/               /*(above)  is marginally faster.       */
           return s==x                           /*if sum matches  X, then it's perfect!*/
```

'''output'''   is the same as the traditional version   and is about   '''11.5'''   times faster   (testing '''34,000,000''' numbers).

===Lucas-Lehmer method===
This version uses memoization to implement a fast version of the Lucas-Lehmer test.

```rexx
/*REXX program  tests  if a number  (or a range of numbers)  is/are  perfect.           */
parse arg low high .                             /*obtain the optional arguments from CL*/
if high=='' & low==""  then high=34000000        /*if no arguments,  then use a range.  */
if  low==''            then  low=1               /*if no   LOW,  then assume  unity.    */
low=low+low//2                                   /*if LOW is odd,  bump it by  one.     */
if high==''            then high=low             /*if no  HIGH,  then assume  LOW.      */
w=length(high)                                   /*use   W   for formatting the output. */
numeric digits max(9,w+2)                        /*ensure enough digits to handle number*/
@.=0;   @.1=2                                    /*highest magic number  and its index. */

            do i=low  to high  by 2              /*process the single number or a range.*/
            if isPerfect(i)  then say  right(i,w)   'is a perfect number.'
            end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPerfect: procedure expose @.;  parse arg x     /*obtain the number to be tested.      */
                                                 /*Lucas-Lehmer know that perfect       */
                                                 /*    numbers can be expressed as:     */
                                                 /*    [2**n - 1]  *  [2** (n-1) ]      */

           if @.0<x then do @.1=@.1  while @._<=x; _=(2**@.1-1)*2**(@.1-1);  @.0=_;  @._=_
                         end   /*@.1*/           /*uses memoization for the formula.    */

           if @.x==0  then return 0              /*Didn't pass Lucas-Lehmer test?       */
           s = 3 + x%2                           /*we know the following factors:       */
                                                 /*  1      ('cause Mama said so.)      */
                                                 /*  2      ('cause it's even.)         */
                                                 /* x÷2     (   "     "    "  )      ___*/
                       do j=3  while  j*j<=x     /*starting at 3, find the factors ≤√ X */
                       if x//j\==0  then iterate /*J  divides  X  evenly,  so ···       */
                       s=s + j + x%j             /*···  add it  and  the other factor.  */
                       end   /*j*/               /*(above)  is marginally faster.       */
           return s==x                           /*if the sum matches  X,  it's perfect!*/
```

'''output'''   is the same as the traditional version   and is about   '''75'''   times faster   (testing '''34,000,000''' numbers).

===Lucas-Lehmer + other optimizations===
This version uses the Lucas-Lehmer method, digital roots, and restricts itself to   ''even''   numbers, and

also utilizes a check for the last-two-digits as per François Édouard Anatole Lucas (in 1891).

Also, in the first   '''do'''   loop, the index   <big>'''i'''</big>   is   ''fast advanced''   according to the last number tested.

An integer square root function was added to limit the factorization of a number.

```rexx
/*REXX program tests if a number  (or a range of numbers)   is/are  perfect.            */
parse arg low high .                             /*obtain optional arguments from the CL*/
if high=='' & low==""  then high=34000000        /*No arguments?    Then use a range.   */
if  low==''            then  low=1               /*if no   LOW,  then assume unity.     */
low=low+low//2                                   /*if LOW is odd,  bump it by one.      */
if high==''            then high=low             /*if no  HIGH,  then assume  LOW.      */
w=length(high)                                   /*use   W   for formatting the output. */
numeric digits max(9,w+2)                        /*ensure enough decimal digits for nums*/
@. =0;    @.1=2;     !.=2;     _=' 6'            /*highest  magic number  and its index.*/
!._=22;   !.16=12;   !.28=8;   !.36=20;   !.56=20;   !.76=20;   !.96=20
                                                 /* [↑]   "Lucas' numbers,  in 1891.    */
            do i=low  to high  by 0              /*process the single number or a range.*/
            if isPerfect(i)  then say  right(i,w)   'is a perfect number.'
            i=i+!.?                              /*use a fast advance for the DO index. */
            end   /*i*/                          /* [↑]  note: the DO index is modified.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPerfect: procedure expose @. !. ?              /*expose (make global) some variables. */
           parse arg  x  1  y  ''  -2  ?         /*#  (and copy), and the last 2 digits.*/
           if x==6    then return 1              /*handle the special case of  six.     */
           if !.?==2  then return 0              /*test last two digits: François Lucas.*/
                                       /*╔═════════════════════════════════════════════╗
                                         ║ Lucas─Lehmer know that perfect numbers can  ║
                                         ║ be expressed as:    [2^n -1] * {2^(n-1) }   ║
                                         ╚═════════════════════════════════════════════╝*/
           if @.0<x  then do @.1=@.1  while @._<=x;  _=(2**@.1-1)*2**(@.1-1); @.0=_; @._=_
                          end   /*@.1*/          /* [↑]  uses memoization for formula.  */

           if @.x==0  then return 0              /*Didn't pass Lucas-Lehmer? Not perfect*/
                                                 /*[↓]  perfect numbers digital root = 1*/
                 do  until  y<10                 /*find the digital root of  Y.         */
                 parse var y d 2;  do k=2  for length(y)-1; d=d+substr(y,k,1);  end  /*k*/
                 y=d                             /*find digital root of the digital root*/
                 end   /*until*/                 /*wash, rinse, repeat ···              */

           if d\==1  then return 0               /*Is digital root ¬ 1?  Then ¬ perfect.*/
           s=3 + x%2                             /*we know the following factors: unity,*/
           z=x                                   /*2,  and  x÷2   (x is even).          */
           q=1;  do  while q<=z;   q=q*4 ;  end  /*while q≤z*/            /*       _____*/
           r=0                                   /* [↓]    R  will be the integer √  X  */
                 do  while q>1;  q=q%4; _=z-r-q; r=r%2;  if _>=0  then do; z=_; r=r+q; end
                 end   /*while q>1*/             /* [↑]  compute the integer SQRT of  X.*/
                                                 /*                                _____*/
                      do j=3  to r               /*starting at 3,  find factors ≤ √  X  */
                      if x//j==0  then s=s+j+x%j /*J divisible by X? Then add J and  X÷J*/
                      end   /*j*/
           return s==x                           /*if the sum matches X,  then perfect! */
```

'''output'''   is the same as the traditional version   and is about   '''500'''   times faster   (testing '''34,000,000''' numbers).




## Ring


```ring

for i = 1 to 10000
    if perfect(i) see i + nl ok
next

func perfect n
     sum = 0
     for i = 1 to n - 1
         if n % i = 0 sum = sum + i ok
     next
if sum = n return 1 else return 0 ok
return sum

```



## Ruby


```ruby
def perf(n)
  sum = 0
  for i in 1...n
    sum += i  if n % i == 0
  end
  sum == n
end
```

Functional style:

```ruby
def perf(n)
  n == (1...n).select {|i| n % i == 0}.inject(:+)
end
```

Faster version:

```ruby
def perf(n)
  divisors = []
  for i in 1..Math.sqrt(n)
    divisors << i << n/i  if n % i == 0
  end
  divisors.uniq.inject(:+) == 2*n
end
```

Test:

```ruby
for n in 1..10000
  puts n if perf(n)
end
```

```txt

6
28
496
8128

```

===Fast (Lucas-Lehmer)===
Generate and memoize perfect numbers as needed.

```ruby
require "prime"

def mersenne_prime_pow?(p)
  # Lucas-Lehmer test; expects prime as argument
  return true  if p == 2
  m_p = ( 1 << p ) - 1
  s = 4
  (p-2).times{ s = (s**2 - 2) % m_p }
  s == 0
end

@perfect_numerator = Prime.each.lazy.select{|p| mersenne_prime_pow?(p)}.map{|p| 2**(p-1)*(2**p-1)}
@perfects = @perfect_numerator.take(1).to_a

def perfect?(num)
  @perfects << @perfect_numerator.next until @perfects.last >= num
  @perfects.include? num
end

# demo
p (1..10000).select{|num| perfect?(num)}
t1 = Time.now
p perfect?(13164036458569648337239753460458722910223472318386943117783728128)
p Time.now - t1

```

```txt

[6, 28, 496, 8128]
true
0.001053954

```

As the task states, it is not known if there are any odd perfect numbers (any that exist are larger than 10**2000). This program tests 10**2001 in about 30 seconds - but only for even perfects.


## Run BASIC


```runbasic
for i = 1 to 10000
 if perf(i) then print i;" ";
next i

FUNCTION perf(n)
for i = 1 TO n - 1
  IF n MOD i = 0 THEN sum = sum + i
next i
IF sum = n THEN perf = 1
END FUNCTION
```

```txt
6 28 496 8128
```



## Rust


```rust

fn main ( ) {
	fn factor_sum(n: i32) -> i32 {
	    let mut v = Vec::new(); //create new empty array
	    for  x in 1..n-1 {      //test vaules 1 to n-1
	    	if n%x == 0 {   //if current x is a factor of n
	    		v.push(x);      //add x to the array
	    	}
	    }
    let mut sum = v.iter().sum(); //iterate over array and sum it up
    return sum;
    }

    fn perfect_nums(n: i32) {
    	for x in 2..n {       //test numbers from 1-n
    		if factor_sum(x) == x {//call factor_sum on each value of x, if return value is = x
    			println!("{} is a perfect number.", x); //print value of x
    		}
    	}
    }
    perfect_nums(10000);
}

```



## SASL

Copied from the SASL manual, page 22:

```SASL

|| The function which takes a number and returns a list of its factors (including one but excluding itself)
|| can be written
factors n = { a <- 1.. n/2; n rem a = 0 }
|| If we define a perfect number as one which is equal to the sum of its factors (for example 6 = 3 + 2 + 1 is perfect)
|| we can write the list of all perfect numbers as
perfects = { n <- 1... ; n = sum(factors n) }

```



## Scala


```scala
def perfectInt(input: Int) = ((2 to sqrt(input).toInt).collect {case x if input % x == 0 => x + input / x}).sum == input - 1
```


'''or'''


```scala
def perfect(n: Int) =
  (for (x <- 2 to n/2 if n % x == 0) yield x).sum + 1 == n

```



## Scheme


```scheme
(define (perf n)
  (let loop ((i 1)
             (sum 0))
    (cond ((= i n)
           (= sum n))
          ((= 0 (modulo n i))
           (loop (+ i 1) (+ sum i)))
          (else
           (loop (+ i 1) sum)))))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: isPerfect (in integer: n) is func
  result
    var boolean: isPerfect is FALSE;
  local
    var integer: i is 0;
    var integer: sum is 1;
    var integer: q is 0;
  begin
    for i range 2 to sqrt(n) do
      if n rem i = 0 then
        sum +:= i;
        q := n div i;
        if q > i then
          sum +:= q;
        end if;
      end if;
    end for;
    isPerfect := sum = n;
  end func;

const proc: main is func
  local
    var integer: n is 0;
  begin
    for n range 2 to 33550336 do
      if isPerfect(n) then
        writeln(n);
      end if;
    end for;
  end func;
```

```txt

6
28
496
8128
33550336

```



## Sidef


```ruby
func is_perfect(n) {
    n.sigma == 2*n
}

for n in (1..10000) {
    say n if is_perfect(n)
}
```


Alternatively, a more efficient check for even perfect numbers:

```ruby
func is_even_perfect(n) {

    var square = (8*n + 1)
    square.is_square || return false

    var t = ((square.isqrt + 1) / 2)
    t.is_smooth(2) || return false

    t-1 -> is_prime
}

for n in (1..10000) {
    say n if is_even_perfect(n)
}
```


```txt

6
28
496
8128

```



## Simula


```simula
BOOLEAN PROCEDURE PERF(N); INTEGER N;
BEGIN
    INTEGER SUM;
    FOR I := 1 STEP 1 UNTIL N-1 DO
        IF MOD(N, I) = 0 THEN
            SUM := SUM + I;
    PERF := SUM = N;
END PERF;
```



## Slate


```slate
n@(Integer traits) isPerfect
[
  (((2 to: n // 2 + 1) select: [| :m | (n rem: m) isZero])
    inject: 1 into: #+ `er) = n
].
```



## Smalltalk


```smalltalk
Integer extend [

  "Translation of the C version; this is faster..."
  isPerfectC [ |tot| tot := 1.
     (2 to: (self sqrt) + 1) do: [ :i |
        (self rem: i) = 0
        ifTrue: [ |q|
                  tot := tot + i.
                  q := self // i.
                  q > i ifTrue: [ tot := tot + q ]
        ]
     ].
     ^ tot = self
  ]

  "... but this seems more idiomatic"
  isPerfect [
     ^ ( ( ( 2 to: self // 2 + 1) select: [ :a | (self rem: a) = 0 ] )
         inject: 1 into: [ :a :b | a + b ] ) = self
  ]
].
```



```smalltalk
1 to: 9000 do: [ :p | (p isPerfect) ifTrue: [ p printNl ] ]
```


## Swift

```Swift
func perfect(n:Int) -> Bool {
    var sum = 0
    for i in 1..<n {
        if n % i == 0 {
            sum += i
        }
    }
    return sum == n
}

for i in 1..<10000 {
    if perfect(i) {
        println(i)
    }
}
```

```txt

6
28
496
8128

```



## Tcl


```tcl
proc perfect n {
    set sum 0
    for {set i 1} {$i <= $n} {incr i} {
        if {$n % $i == 0} {incr sum $i}
    }
    expr {$sum == 2*$n}
}
```



## Ursala


```Ursala
#import std
#import nat

is_perfect = ~&itB&& ^(~&,~&t+ iota); ^E/~&l sum:-0+ ~| not remainder
```

This test program applies the function to a list of the first five hundred natural
numbers and deletes the imperfect ones.

```Ursala
#cast %nL

examples = is_perfect*~ iota 500
```

```txt
<6,28,496>
```



## VBA

Using [[Factors_of_an_integer#VBA]], slightly adapted.

```vb
Private Function Factors(x As Long) As String
    Application.Volatile
    Dim i As Long
    Dim cooresponding_factors As String
    Factors = 1
    corresponding_factors = x
    For i = 2 To Sqr(x)
        If x Mod i = 0 Then
            Factors = Factors & ", " & i
            If i <> x / i Then corresponding_factors = x / i & ", " & corresponding_factors
        End If
    Next i
    If x <> 1 Then Factors = Factors & ", " & corresponding_factors
End Function
Private Function is_perfect(n As Long)
    fs = Split(Factors(n), ", ")
    Dim f() As Long
    ReDim f(UBound(fs))
    For i = 0 To UBound(fs)
        f(i) = Val(fs(i))
    Next i
    is_perfect = WorksheetFunction.Sum(f) - n = n
End Function
Public Sub main()
    Dim i As Long
    For i = 2 To 100000
        If is_perfect(i) Then Debug.Print i
    Next i
End Sub
```
```txt
 6
 28
 496
 8128
```


## VBScript


```vb
Function IsPerfect(n)
	IsPerfect = False
	i = n - 1
	sum = 0
	Do While i > 0
		If n Mod i = 0 Then
			sum = sum + i
		End If
		i = i - 1
	Loop
	If sum = n Then
		IsPerfect = True
	End If
End Function

WScript.StdOut.Write IsPerfect(CInt(WScript.Arguments(0)))
WScript.StdOut.WriteLine
```


```txt

C:\>cscript /nologo perfnum.vbs 6
True

C:\>cscript /nologo perfnum.vbs 29
False

C:\>

```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func Perfect(N);        \Return 'true' if N is a perfect number
int  N, S, I, Q;
[S:= 1;
for I:= 2 to sqrt(N) do
        [Q:= N/I;
        if rem(0)=0 then S:= S+I+Q;
        ];
return S=N & N#1;
];

int  A, N;
[for A:= 1 to 16 do
        [N:= (1<<A - 1) * 1<<(A-1);
        if Perfect(N) then [IntOut(0, N);  CrLf(0)];
        ];
]
```


```txt

6
28
496
8128
33550336

```



## zkl

```zkl
fcn isPerfectNumber1(n)
   { n == [1..n-1].filter('wrap(i){ n % i == 0 }).sum(); }
```

```txt

[1..0d10_000].filter(isPerfectNumber1).println();
L(6,28,496,8128)

```


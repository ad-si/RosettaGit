+++
title = "Linear congruential generator"
description = ""
date = 2019-08-04T03:57:52Z
aliases = []
[extra]
id = 10002
[taxonomies]
categories = ["task", "Randomness"]
tags = []
+++

## Task

The [[wp:linear congruential generator|linear congruential generator]] is a very simple example of a [[random number generator]].

All linear congruential generators use this formula:
* <math>r_{n + 1} = a \times r_n + c \pmod m</math>


Where:
* <math>r_0</math> is a seed.
* <math>r_1</math>, <math>r_2</math>, <math>r_3</math>, ..., are the random numbers.
* <math>a</math>, <math>c</math>, <math>m</math> are constants.


If one chooses the values of <math>a</math>, <math>c</math> and <math>m</math> with care, then the generator produces a uniform distribution of integers from <math>0</math> to <math>m - 1</math>.

LCG numbers have poor quality. <math>r_n</math> and <math>r_{n + 1}</math> are not independent, as true random numbers would be. Anyone who knows <math>r_n</math> can predict <math>r_{n + 1}</math>, therefore LCG is not cryptographically secure. The LCG is still good enough for simple tasks like [[Miller-Rabin primality test]], or [[deal cards for FreeCell|FreeCell deals]]. Among the benefits of the LCG, one can easily reproduce a sequence of numbers, from the same <math>r_0</math>. One can also reproduce such sequence with a different programming language, because the formula is so simple.

The task is to replicate two historic random number generators. One is the <code>rand()</code> function from [[:Category:BSD libc|BSD libc]], and the other is the <code>rand()</code> function from the Microsoft C Runtime (MSCVRT.DLL). Each replica must yield the same sequence of integers as the original generator, when starting from the same seed.

In these formulas, the seed becomes <math>state_0</math>. The random sequence is <math>rand_1</math>, <math>rand_2</math> and so on.


;BSD formula:
* <math>state_{n + 1} = 1103515245 \times state_n + 12345 \pmod{2^{31}}</math>
* <math>rand_n = state_n</math>
* <math>rand_n</math> is in range 0 to 2147483647.


;Microsoft formula:
* <math>state_{n + 1} = 214013 \times state_n + 2531011 \pmod{2^{31}}</math>
* <math>rand_n = state_n \div 2^{16}</math>
* <math>rand_n</math> is in range 0 to 32767.


The BSD formula was so awful that FreeBSD switched to a different formula.

More info is at [[Random number generator (included)#C]].





## 360 Assembly


```360asm
*        Linear congruential generator   07/03/2017
LINCONG  CSECT
         USING  LINCONG,R12
         LR     R12,R15            set base register
BEGIN    SR     R5,R5              bsdseed=0
         SR     R7,R7              msseed=0
         LA     R8,1               i=1
         L      R9,=F'10'          number of loop
LOOP     M      R4,=F'1103515245'  bsdseed*=1103515245
         A      R5,=F'12345'       bsdseed+=12345
         LR     R3,R5              bsdrand=bsdseed
         LTR    R5,R5              if bsdseed<0
         BP     CONT               then
         L      R3,COMP2             -2**31
         SR     R3,R5                -bsdseed
         LPR    R3,R3                bsdrand=abs(-2**31-bsdseed)
CONT     M      R6,=F'214013'      msseed*=214013
         A      R7,=F'2531011'     msseed+=2531011
         XR     R6,R6
         D      R6,TWO16           /2**16
         XDECO  R8,XDEC            i
         MVC    PG(4),XDEC+8
         XDECO  R3,XDEC            bsdrand
         MVC    PG+4(12),XDEC
         XDECO  R7,XDEC            msseed
         MVC    PG+16(7),XDEC+5
         XPRNT  PG,L'PG            print buffer
         LA     R8,1(R8)           i=i+1
         BCT    R9,LOOP            loop
RETURN   XR     R15,R15            set return code
         BR     R14                return to caller
         DS     0F                 alignment
TWO16    DC     XL4'00010000'      2**16
COMP2    DC     XL4'80000000'      -2**31
PG       DC     CL80' '
XDEC     DS     CL12
         YREGS
         END    LINCONG
```

```txt

   1       12345     38
   2  1406932606    162
   3   654583775    567
   4  1449466924   1890
   5   229283573   6210
   6  1109335178  20317
   7  1051550459    849
   8  1293799192   2811
   9   794471793   9218
  10   551188310  30140

```



## Ada


We first specify a generic package LCG:


```Ada
generic
   type Base_Type is mod <>;
   Multiplyer, Adder: Base_Type;
   Output_Divisor: Base_Type := 1;
package LCG is

   procedure Initialize(Seed: Base_Type);
   function Random return Base_Type;
   -- changes the state and outputs the result

end LCG;
```


Then we provide a generic implementation:


```Ada
package body LCG is

   State: Base_Type := Base_Type'First;

   procedure Initialize(Seed: Base_Type) is
   begin
      State := Seed;
   end Initialize;

   function Random return Base_Type is
   begin
      State := State * Multiplyer + Adder;
      return State / Output_Divisor;
   end Random;

end LCG;
```


Next, we define the MS- and BSD-instantiations of the generic package:


```Ada
with Ada.Text_IO, LCG;

procedure Run_LCGs is

   type M31 is mod 2**31;

   package BSD_Rand is new LCG(Base_Type => M31, Multiplyer => 1103515245,
                               Adder => 12345);

   package MS_Rand  is new LCG(Base_Type => M31, Multiplyer => 214013,
                               Adder => 2531011, Output_Divisor => 2**16);

begin
   for I in 1 .. 10 loop
      Ada.Text_IO.Put_Line(M31'Image(BSD_Rand.Random));
   end loop;
   for I in 1 .. 10 loop
       Ada.Text_IO.Put_Line(M31'Image(MS_Rand.Random));
   end loop;
end Run_LCGs;
```


Finally, we run the program, which generates the following output (note that the first ten lines are from the BSD generator, the next ten from the MS generator):


```txt
 12345
 1406932606
 654583775
 1449466924
 229283573
 1109335178
 1051550459
 1293799192
 794471793
 551188310
 38
 7719
 21238
 2437
 8855
 11797
 8365
 32285
 10450
 30612
```



## ALGOL 68


```algol68

BEGIN
COMMENT
   Algol 68 Genie checks for integer overflow whereas the reference
   language leaves the result undefined so for portability we need to
   see how wide a variable must be to hold the maximum possible value
   before range reduction. This occurs in the BSD RNG when
   rseed=2147483647 and is therefore 2147483647 * 1103515245 + 12345 =
   2369780942852710860, which itself is 19 decimal digits.  Use
   evironmental queries to determine the width needed.
COMMENT
   MODE RANDINT = UNION (INT, LONG INT, LONG LONG INT);
   RANDINT rseed := (int width > 18 | 0 |:
		     long int width > 18 |
		     LONG 0 | LONG LONG 0);
   PROC srand = (INT x) VOID :
   (rseed | (INT): rseed := x,
    (LONG INT): rseed := LENG x | rseed := LENG LENG x);
   PROC bsd rand = INT :
   BEGIN
      CASE rseed IN
      (INT ri):
      BEGIN
	 INT a = 1103515245, c = 12345, m1 = 2^16, m2 = 2^15;
COMMENT
   That curious declaration is because 2^31 might overflow during
   compilation but the MODE declaration for RANDINT guarantees that it
   will not overflow at run-time.  We assume that an INT is at least
   32 bits wide, otherwise a similar workaround would be needed for
   the declaration of a.
COMMENT
	 INT result = (ri * a + c) MOD (m1 * m2); rseed := result;
	 result
      END,
      (LONG INT rli):
      BEGIN
	 LONG INT a = LONG 1103515245, c = LONG 12345, m = LONG 2^31;
	 LONG INT result = (rli * a + c) MOD m; rseed := result;
	 SHORTEN result
      END,
      (LONG LONG INT rlli) :
      BEGIN
	 LONG LONG INT a = LONG LONG 1103515245,
	 c = LONG LONG 12345, m = LONG LONG 2^31;
	 LONG LONG INT result = (rlli * a + c) MOD  m; rseed := result;
	 SHORTEN SHORTEN result
      END
      ESAC
   END;
   PROC ms rand = INT :
   BEGIN
      CASE rseed IN
      (INT ri):
      BEGIN
	 INT a = 214013, c = 2531011, m1 = 2^15, m2 = 2^16;
	 INT result = (ri * a + c) MOD (m1 * m2); rseed := result;
	 result % m2
      END,
      (LONG INT rli):
      BEGIN
	 LONG INT a = LONG 214013, c = LONG 2531011, m = LONG 2^31, m2 = LONG 2^16;
	 LONG INT result = (rli * a + c) MOD m; rseed := result;
	 SHORTEN (result % m2)
      END,
      (LONG LONG INT rlli) :
      BEGIN
	 LONG LONG INT a = LONG LONG 214013,
	 c = LONG LONG 2531011, m = LONG LONG 2^31, m2 = LONG LONG 2^16;
	 LONG LONG INT result = (rlli * a + c) MOD m; rseed := result;
	 SHORTEN SHORTEN (result % m2)
      END
      ESAC
   END;
   srand (0);
   TO 10 DO printf (($g(0)l$, bsd rand)) OD;
   print (newline);
   srand (0);
   TO 10 DO printf (($g(0)l$, ms rand)) OD;
   srand (0)
END

```

```txt

12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

38
7719
21238
2437
8855
11797
8365
32285
10450
30612

```



## AutoHotkey


```AutoHotkey
a := 0, b:= [0]
Loop, 10
	BSD .= "`t" (a :=  BSD(a)) "`n"
,	b := MS(b[1])
,	MS .= "`t" (b[2]) "`n"

MsgBox, % "BSD:`n" BSD "`nMS:`n" MS

BSD(Seed) {
	return, Mod(1103515245 * Seed + 12345, 2147483648)
}

MS(Seed) {
	Seed := Mod(214013 * Seed + 2531011, 2147483648)
	return, [Seed, Seed // 65536]
}
```

'''Output:'''

```txt
BSD:
	12345
	1406932606
	654583775
	1449466924
	229283573
	1109335178
	1051550459
	1293799192
	794471793
	551188310

MS:
	38
	7719
	21238
	2437
	8855
	11797
	8365
	32285
	10450
	30612
```



## Batch


```batch

@echo off & setlocal enabledelayedexpansion

echo BSD Rand
set /a a=0,cnt=1
:b
set /a "a=1103515245 *a+12345,a&=0x7fffffff, cnt+=1"
call:prettyprint !cnt! !a!
if !cnt! leq 10 goto :b

echo.
echo Microsoft Rand
set /a a=0,cnt=1
:c
set /a "a=214013 *a+2531011,a&=0x7fffffff, b=a>>16,cnt+=1"
call:prettyprint !cnt! !b!
if !cnt! lss 10 goto :c
pause
goto:eof

:prettyprint
set p1= %1
set p2=        %2
echo %p1:~-2%  %p2:~-10%
goto:eof


```

'''Output:'''

```txt

BSD Rand
 2       12345
 3  1406932606
 4   654583775
 5  1449466924
 6   229283573
 7  1109335178
 8  1051550459
 9  1293799192
10   794471793
11   551188310

Microsoft Rand
 2          38
 3        7719
 4       21238
 5        2437
 6        8855
 7       11797
 8        8365
 9       32285
10       10450

```



## BBC BASIC

```bbcbasic
      @% = &D0D
      PRINT "MS generator:"
      dummy% = FNrandMS(0)
      FOR i% = 1 TO 10
        PRINT FNrandMS(-1)
      NEXT
      PRINT '"BSD generator:"
      dummy% = FNrandBSD(0)
      FOR i% = 1 TO 10
        PRINT FNrandBSD(-1)
      NEXT
      END

      DEF FNrandMS(seed%)
      PRIVATE state%
      IF seed% >= 0 THEN
        state% = seed%
      ELSE
        state% = FNmuladd(state%, 214013, 2531011)
      ENDIF
      = state% >> 16

      DEF FNrandBSD(seed%)
      PRIVATE state%
      IF seed% >= 0 THEN
        state% = seed%
      ELSE
        state% = FNmuladd(state%, 1103515245, 12345)
      ENDIF
      = state%

      DEF FNmuladd(A%,B%,C%) : PRIVATE M% : LOCAL P% : IF M% = 0 DIM P% 8
      IF P% THEN [OPT 0 : .M% mul ebx : add eax,ecx : btr eax,31 : ret :]
      = USR M%
```

'''Output:'''

```txt

MS generator:
           38
         7719
        21238
         2437
         8855
        11797
         8365
        32285
        10450
        30612

BSD generator:
        12345
   1406932606
    654583775
   1449466924
    229283573
   1109335178
   1051550459
   1293799192
    794471793
    551188310

```



## bc

As with dc, bc has no bitwise operators.

```bc
/* BSD rand */

define rand() {
	randseed = (randseed * 1103515245 + 12345) % 2147483648
	return randseed
}

randseed = 1
rand(); rand(); rand(); print "\n"

/* Microsoft rand */

define rand() {
	randseed = (randseed * 214013 + 2531011) % 2147483648
	return randseed / 65536
}

randseed = 1
rand(); rand(); rand(); print "\n"
```



## Befunge

This required a bit of trickery to handle signed overflow and negative division in a portable way. It still won't work on all implementations, though. In particular Javascript-based interpreters can't handle the BSD formula because of the way Javascript numbers lose their least significant digits when they become too large.


```befunge>
025*>\::0\`288*::*:****+.55+,"iQ"5982156*:v
v $$_^#!\-1:\%***:*::*882 ++*"yf"3***+***+*<
>025*>\:488**:*/:0\`6"~7"+:*+01-2/-*+."O?+"55v
@ $$_^#!\-1:\%***:*::*882 ++***" ''4C"*+2**,+<
```


```txt
0
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310
0
38
7719
21238
2437
8855
11797
8365
32285
10450
30612
```



## Bracmat


```bracmat
( 2^31:?RANDMAX
& 2^-16:?rshift
& (randBSD=mod$(!seed*1103515245+12345.!RANDMAX):?seed)
& ( randMS
  =   div
    $ ((mod$(!seed*214013+2531011.!RANDMAX):?seed)*!rshift.1)
  )
& out$\nBSD
& 0:?seed
& 0:?i
& whl'(1+!i:~>10:?i&out$!randBSD)
& out$\nMicrosoft
& 0:?seed
& 0:?i
& whl'(1+!i:~>10:?i&out$!randMS)
)
```


Output:

```txt
BSD
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

Microsoft
38
7719
21238
2437
8855
11797
8365
32285
10450
30612
```



## C

In a pretended lib style, this code produces a rand() function depends on compiler macro: <code>gcc -DMS_RAND</code> uses MS style, otherwise it's BSD rand by default.

```c
#include <stdio.h>

/* always assuming int is at least 32 bits */
int rand();
int rseed = 0;

inline void srand(int x)
{
	rseed = x;
}

#ifndef MS_RAND
#define RAND_MAX ((1U << 31) - 1)

inline int rand()
{
	return rseed = (rseed * 1103515245 + 12345) & RAND_MAX;
}

#else /* MS rand */

#define RAND_MAX_32 ((1U << 31) - 1)
#define RAND_MAX ((1U << 15) - 1)

inline int rand()
{
	return (rseed = (rseed * 214013 + 2531011) & RAND_MAX_32) >> 16;
}

#endif/* MS_RAND */

int main()
{
	int i;
	printf("rand max is %d\n", RAND_MAX);

	for (i = 0; i < 100; i++)
		printf("%d\n", rand());

	return 0;
}
```



## C++


```cpp
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class mRND
{
public:
    void seed( unsigned int s ) { _seed = s; }

protected:
    mRND() : _seed( 0 ), _a( 0 ), _c( 0 ), _m( 2147483648 ) {}
    int rnd() { return( _seed = ( _a * _seed + _c ) % _m ); }

    int _a, _c;
    unsigned int _m, _seed;
};
//--------------------------------------------------------------------------------------------------
class MS_RND : public mRND
{
public:
    MS_RND()  { _a = 214013; _c = 2531011; }
    int rnd() { return mRND::rnd() >> 16; }
};
//--------------------------------------------------------------------------------------------------
class BSD_RND : public mRND
{
public:
    BSD_RND() { _a = 1103515245; _c = 12345; }
    int rnd() { return mRND::rnd(); }
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    BSD_RND bsd_rnd;
    MS_RND ms_rnd;

    cout << "MS RAND:" << endl << "
### ==
" << endl;
    for( int x = 0; x < 10; x++ )
	cout << ms_rnd.rnd() << endl;

    cout << endl  << "BSD RAND:" << endl << "
### ===
" << endl;
    for( int x = 0; x < 10; x++ )
	cout << bsd_rnd.rnd() << endl;

    cout << endl << endl;
    system( "pause" );
    return 0;
}
//--------------------------------------------------------------------------------------------------
```

Output:

```txt

MS RAND:

### ==

38
7719
21238
2437
8855
11797
8365
32285
10450
30612

BSD RAND:

### ===

12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

```


; C++11
```cpp
#include <iostream>
#include <random>

int main() {

  std::linear_congruential_engine<std::uint_fast32_t, 1103515245, 12345, 1 << 31> bsd_rand(0);
  std::linear_congruential_engine<std::uint_fast32_t, 214013, 2531011, 1 << 31> ms_rand(0);

  std::cout << "BSD RAND:" << std::endl << "
### ==
" << std::endl;
  for (int i = 0; i < 10; i++) {
    std::cout << bsd_rand() << std::endl;
  }
  std::cout << std::endl;
  std::cout << "MS RAND:" << std::endl << "
### ==
" << std::endl;
  for (int i = 0; i < 10; i++) {
    std::cout << (ms_rand() >> 16) << std::endl;
  }

  return 0;
}
```

Output:

```txt

BSD RAND:

### ==

12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

MS RAND:

### ==

38
7719
21238
2437
8855
11797
8365
32285
10450
30612

```



## Clojure



```Clojure


(defn iterator [a b]
  (fn[x] (mod (+ (* a x) b) (bit-shift-left 1 31))))

(def bsd (drop 1 (iterate (iterator 1103515245 12345) 0)))

(def ms (drop 1 (for [x (iterate  (iterator 214013 2531011) 0)] (bit-shift-right x 16))))

(take 10 bsd) ;-> (12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 1293799192 794471793 551188310)
(take 10 ms) ;-> (38 7719 21238 2437 8855 11797 8365 32285 10450 30612)


```



## Common Lisp


```lisp
(defun make-rng (&key (seed 0) (mode nil))
  "returns an RNG according to :seed and :mode keywords
  default mode: bsd
  default seed: 0 (should be 1 actually)"
  (if (eql mode 'ms)
    #'(lambda ()
	(ash (setf seed (mod (+ (* 214013 seed) 2531011) (expt 2 31))) -16))
    #'(lambda () (setf seed (mod (+ (* seed 1103515245) 12345) (expt 2 31))))))

(let ((rng (make-rng)))
      (dotimes (x 10) (format t "BSD: ~d~%" (funcall rng))))

(let ((rng (make-rng :mode 'ms :seed 1)))
      (dotimes (x 10) (format t "MS: ~d~%" (funcall rng))))
```



Another solution could be:

```lisp
(defun linear-random (seed &key (times 1) (bounds (expt 2 31)) (multiplier 1103515245) (adder 12345) (divisor 1) (max 2147483647) (min 0))
  (loop for candidate = seed then (mod (+ (* multiplier candidate) adder) bounds)
     for result = candidate then (floor (/ candidate divisor))
     when (and (< result max) (> result min)) collect result into valid-numbers
     when (> (length valid-numbers) times) return result))
```


Which defaults to the BSD formula, but can be customized to any formula with keyword arguments, for example:

```lisp
(format t "Count:~15tBSD:~30tMS:~%~{~{~a~15t~a~30t~a~%~}~}"
        (loop for i from 0 upto 5 collect
             (list i
                   (linear-random 0 :times i)
                   (linear-random 0 :times i :multiplier 214013 :adder 2531011 :max 32767 :divisor (expt 2 16)))))
```


Outputs:

```txt
Count:         BSD:           MS:
0              12345          38
1              1406932606     7719
2              654583775      21238
3              1449466924     2437
4              229283573      8855
5              1109335178     11797
```


## C#
<!-- By Martin Freedman, 17/01/2018 -->

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using static System.Console;

namespace LinearCongruentialGenerator
{
    static class LinearCongruentialGenerator
    {
        static int _seed = (int)DateTime.Now.Ticks; // from bad random gens might as well have bad seed!
        static int _bsdCurrent = _seed;
        static int _msvcrtCurrent = _seed;

        static int Next(int seed, int a, int b) => (a * seed + b) & int.MaxValue;

        static int BsdRand() => _bsdCurrent = Next(_bsdCurrent, 1103515245, 12345);

        static int MscvrtRand() => _msvcrtCurrent = Next (_msvcrtCurrent << 16,214013,2531011) >> 16;

        static void PrintRandom(int count, bool isBsd)
        {
            var name = isBsd ? "BSD" : "MS";
            WriteLine($"{name} next {count} Random");
            var gen = isBsd ? (Func<int>)(BsdRand) : MscvrtRand;
            foreach (var r in Enumerable.Repeat(gen, count))
                WriteLine(r.Invoke());
        }

        static void Main(string[] args)
        {
            PrintRandom(10, true);
            PrintRandom(10, false);
            Read();
        }
    }
}
```

Produces:

```txt
BSD next 10 Random
1587930915
19022880
1025044953
1143293854
1642451583
1110934092
773706389
1830436778
1527715739
2072016696
MS next 10 Random
24368
8854
28772
16122
11064
24190
23724
6690
14784
21222

```

From a Free Cell Deal solution

```c#

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FreeCellDeals
{
    public class LCG
    {
        private int _state;
        public bool Microsoft { get; set;}
        public bool BSD
        {
            get
            {
                return !Microsoft;
            }
            set
            {
                Microsoft = !value;
            }
        }

        public LCG(bool microsoft = true)
        {
            _state = (int)DateTime.Now.Ticks;
            Microsoft = microsoft;
        }

        public LCG(int n, bool microsoft = true)
        {
            _state = n;
            Microsoft = microsoft;
        }

        public int Next()
        {
            if (BSD)
            {
                return _state = (1103515245 * _state + 12345) & int.MaxValue;
            }
            return ((_state = 214013 * _state + 2531011) & int.MaxValue) >> 16;
        }

        public IEnumerable<int> Seq()
        {
            while (true)
            {
                yield return Next();
            }
        }
    }

    class Program
    {
        static void Main()
        {
            LCG ms = new LCG(0, true);
            LCG bsd = new LCG(0,false);
            Console.WriteLine("Microsoft");
            ms.Seq().Take(10).ToList().ForEach(Console.WriteLine);
            Console.WriteLine("\nBSD");
            bsd.Seq().Take(10).ToList().ForEach(Console.WriteLine);
            Console.ReadKey();
        }
    }
}

```

Output:

```txt
Microsoft
38
7719
21238
2437
8855
11797
8365
32285
10450
30612

BSD
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

```



## D


```d
struct LinearCongruentialGenerator {
    enum uint RAND_MAX = (1U << 31) - 1;
    uint seed = 0;

    uint randBSD() pure nothrow @nogc {
        seed = (seed * 1_103_515_245 + 12_345) & RAND_MAX;
        return seed;
    }

    uint randMS() pure nothrow @nogc {
        seed = (seed * 214_013 + 2_531_011) & RAND_MAX;
        return seed >> 16;
    }
}

void main() {
    import std.stdio;

    LinearCongruentialGenerator rnd;

    foreach (immutable i; 0 .. 10)
        writeln(rnd.randBSD);
    writeln;

    rnd.seed = 0;
    foreach (immutable i; 0 .. 10)
        writeln(rnd.randMS);
}
```

Output:

```txt
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

38
7719
21238
2437
8855
11797
8365
32285
10450
30612
```



## dc

''dc'' has no bitwise operations, so this program uses the modulus operator (<code>2147483648 %</code>) and division (<code>65536 /</code>). Fortunately, ''dc'' numbers cannot overflow to negative, so the modulus calculation involves only non-negative integers.

For BSD rand():
```dc
[*
 * lrx -- (random number from 0 to 2147483647)
 *
 * Returns a number from the BSD rand() sequence.
 * Seeded by storing a seed in register R.
 *]sz
[lR 1103515245 * 12345 + 2147483648 % d sR]sr

[* Set seed to 1, then print the first 3 random numbers. *]sz
1 sR
lrx psz lrx psz lrx psz
```



```txt
1103527590
377401575
662824084
```


For Microsoft rand():
```dc
[*
 * lrx -- (random number from 0 to 32767)
 *
 * Returns a number from the Microsoft rand() sequence.
 * Seeded by storing a seed in register R.
 *]sz
[lR 214013 * 2531011 + 2147483648 % d sR 65536 /]sr

[* Set seed to 1, then print the first 3 random numbers. *]sz
1 sR
lrx psz lrx psz lrx psz
```



```txt
41
18467
6334
```



## Elixir


```elixir
defmodule LCG do
  def ms_seed(seed) do
    Process.put(:ms_state, seed)
    ms_rand
    Process.put(:ms_seed, seed)
  end

  def ms_rand do
    state = Process.get(:ms_state)
    state2 = rem(214013 * state + 2531011, 2147483648)
    Process.put(:ms_state, state2)
    div(state, 65536)
  end

  def bsd_seed(seed) do
    Process.put(:bsd_state, seed)
    Process.put(:bsd_seed, seed)
  end

  def bsd_rand do
    state = Process.get(:bsd_state)
    state2 = rem(1103515245 * state + 12345, 2147483648)
    Process.put(:bsd_state, state2)
    state2
  end
end

Enum.each([0,1], fn i ->
  IO.puts "\nRandom seed: #{i}\n        BSD      MS"
  LCG.bsd_seed(i)
  LCG.ms_seed(i)
  Enum.each(1..10, fn _ ->
    :io.format "~11w~8w~n", [LCG.bsd_rand, LCG.ms_rand]
  end)
end)
```


```txt

Random seed: 0
        BSD      MS
      12345      38
 1406932606    7719
  654583775   21238
 1449466924    2437
  229283573    8855
 1109335178   11797
 1051550459    8365
 1293799192   32285
  794471793   10450
  551188310   30612

Random seed: 1
        BSD      MS
 1103527590      41
  377401575   18467
  662824084    6334
 1147902781   26500
 2035015474   19169
  368800899   15724
 1508029952   11478
  486256185   29358
 1062517886   26962
  267834847   24464

```



## Erlang

```erlang
-module(lcg).
-export([bsd_seed/1, ms_seed/1, bsd_rand/0, ms_rand/0]).

bsd_seed(Seed) -> put(bsd_state, Seed).
ms_seed(Seed)  -> put(ms_state, Seed).

bsd_rand() ->
  State = (get(bsd_state) * 1103515245 + 12345) rem 2147483648,
  put(bsd_state,State),
  State.

ms_rand() ->
  State = (get(ms_state) * 214013 + 2531011) rem 2147483648,
  put(ms_state,State),
  State div 65536.

main(_) ->
  bsd_seed(0),
  ms_seed(0),
  io:fwrite("~10s~c~5s~n", ["BSD", 9, "MS"]),
  lists:map(fun(_) -> io:fwrite("~10w~c~5w~n", [bsd_rand(),9,ms_rand()]) end, lists:seq(1,10)).
```


```txt
       BSD         MS
     12345         38
1406932606       7719
 654583775      21238
1449466924       2437
 229283573       8855
1109335178      11797
1051550459       8365
1293799192      32285
 794471793      10450
 551188310      30612
```



## ERRE

ERRE doesn't generate the proper output from the BSD constants; it uses double-precision floating point, which is not enough for some of the intermediate products: for exact computation you can use MULPREC program. The BSD series deviates starting with the third value (see sample output below).

```ERRE
PROGRAM RNG

!$DOUBLE

DIM CARDS%[52]

PROCEDURE XRANDOM(SEED->XRND)
   POW31=2^31
   POW16=2^16
   SEED=SEED*214013+2531011
   SEED=SEED-POW31*INT(SEED/POW31)
   XRND=INT(SEED/POW16)
END PROCEDURE

PROCEDURE YRANDOM(SEED->YRND)
   POW31=2^31
   SEED=SEED*1103515245+12345
   SEED=SEED-POW31*INT(SEED/POW31)
   YRND=SEED
END PROCEDURE

BEGIN
    PRINT(CHR$(12);)
    SEED=0  PRINT("BSD:")
    FOR I%=1 TO 10 DO
       YRANDOM(SEED->YRND)
       PRINT(TAB(10);YRND)
    END FOR
    SEED=0  PRINT("MSD:")
    FOR I%=1 TO 10 DO
       XRANDOM(SEED->XRND)
       PRINT(TAB(10);XRND)
    END FOR
END PROGRAM
```

```txt

BSD:
          12345
          1406932606
          654583776
          405498528
          481908312
          1397277616
          733684288
          1620919680
          1327744960
          1469627648
MSD:
          38
          7719
          21238
          2437
          8855
          11797
          8365
          32285
          10450
          30612

```


=={{header|F_Sharp|F#}}==


```fsharp
module lcg =
    let bsd seed =
        let state = ref seed
        (fun (_:unit) ->
            state := (1103515245 * !state + 12345) &&& System.Int32.MaxValue
            !state)

    let ms seed =
        let state = ref seed
        (fun (_:unit) ->
            state := (214013 * !state + 2531011) &&& System.Int32.MaxValue
            !state / (1<<<16))

```


```txt
let rndBSD = lcg.bsd 0;;
let BSD=[for n in [0 .. 9] -> rndBSD()];;

let rndMS = lcg.ms 0;;
let MS=[for n in [0 .. 9] -> rndMS()];;

val BSD : int list =
  [12345; 1406932606; 654583775; 1449466924; 229283573; 1109335178; 1051550459;
   1293799192; 794471793; 551188310]
val MS : int list =
  [38; 7719; 21238; 2437; 8855; 11797; 8365; 32285; 10450; 30612]
```



## Forth


```forth
1 31 lshift 1- constant MAX-RAND-BSD
1 15 lshift 1- constant MAX-RAND-MS

variable seed                         \ seed variable

: (random) seed @ * + dup seed ! ;    ( -- n)
: BSDrandom MAX-RAND-BSD 12345 1103515245 (random) and ;
: MSrandom MAX-RAND-MS 2531011 214013 (random) 16 rshift and ;

: test-random
  1 seed ! cr ." BSD (seed=1)" cr
  5 0 do BSDrandom . cr loop
  1 seed ! cr ." MS  (seed=1)" cr
  5 0 do MSrandom . cr loop
;

test-random
```


Output:

```txt
BSD (seed=1)
1103527590
377401575
662824084
1147902781
2035015474

MS  (seed=1)
41
18467
6334
26500
19169
```



## Fortran

```fortran
module lcgs
  implicit none

  integer, parameter :: i64 = selected_int_kind(18)
  integer, parameter :: a1 = 1103515245, a2 = 214013
  integer, parameter :: c1 = 12345, c2 = 2531011
  integer, parameter :: div = 65536
  integer(i64), parameter :: m = 2147483648_i64  ! need to go to 64 bits because
                                                 ! of the use of signed integers
contains

function bsdrand(seed)
  integer :: bsdrand
  integer, optional, intent(in) :: seed
  integer(i64) :: x = 0

  if(present(seed)) x = seed
  x = mod(a1 * x + c1, m)
  bsdrand = x
end function

function msrand(seed)
  integer :: msrand
  integer, optional, intent(in) :: seed
  integer(i64) :: x = 0

  if(present(seed)) x = seed
  x = mod(a2 * x + c2, m)
  msrand = x / div
end function
end module

program lcgtest
  use lcgs
  implicit none
  integer :: i

  write(*, "(a)") "      BSD            MS"
  do i = 1, 10
    write(*, "(2i12)") bsdrand(), msrand()
  end do
end program
```

Output

```txt
      BSD            MS
       12345          38
  1406932606        7719
   654583775       21238
  1449466924        2437
   229283573        8855
  1109335178       11797
  1051550459        8365
  1293799192       32285
   794471793       10450
   551188310       30612
```


## FreeBASIC


```freebasic
' version 04-11-2016
' compile with: fbc -s console

' to seed BSD_lcg(seed > -1)
' to get random number BSD_lcg(-1) or BSD_lcg() or just BSD_lcg
Function BSD_lcg(seed As UInteger = -1) As UInteger

    Static As UInteger bsd_state

    If seed <> -1 Then
        bsd_state = seed Mod 2 ^ 31
    Else
        bsd_state = (1103515245 * bsd_state + 12345) Mod 2 ^ 31
    End If

    Return bsd_state

End Function

' to seed ms_lcg(seed > -1)
' to get random number ms_lcg(-1) or ms_lcg() or just ms_lcg
Function ms_lcg(seed As Integer = -1) As UInteger

    Static As UInteger ms_state

    If seed <> -1 Then
        ms_state = seed Mod 2 ^ 31
    Else
        ms_state = (214013 * ms_state + 2531011) Mod 2 ^ 31
    End If

    Return ms_state Shr 16

End Function

' ------=< MAIN >=------

Dim As Long i

Print "MS generator"
' ms_lcg(0)      ' state = 0 at the start of the program
For i = 1 To 10
    Print Using "###########"; ms_lcg
Next

Print
Print "BSD generator"
' BSD_lcg(0)     ' state = 0 at the start of the program
For i  = 1 To 10
    Print Using "###########"; BSD_lcg
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
MS generator
         38
       7719
      21238
       2437
       8855
      11797
       8365
      32285
      10450
      30612

BSD generator
      12345
 1406932606
  654583775
 1449466924
  229283573
 1109335178
 1051550459
 1293799192
  794471793
  551188310
```



## Go


```go
package main

import "fmt"

// basic linear congruential generator
func lcg(a, c, m, seed uint32) func() uint32 {
    r := seed
    return func() uint32 {
        r = (a*r + c) % m
        return r
    }
}

// microsoft generator has extra division step
func msg(seed uint32) func() uint32 {
    g := lcg(214013, 2531011, 1<<31, seed)
    return func() uint32 {
        return g() / (1 << 16)
    }
}

func example(seed uint32) {
    fmt.Printf("\nWith seed = %d\n", seed)
    bsd := lcg(1103515245, 12345, 1<<31, seed)
    msf := msg(seed)
    fmt.Println("       BSD  Microsoft")
    for i := 0; i < 5; i++ {
        fmt.Printf("%10d    %5d\n", bsd(), msf())
    }
}

func main() {
    example(0)
    example(1)
}
```

Output:

```txt

With seed = 0
       BSD  Microsoft
     12345       38
1406932606     7719
 654583775    21238
1449466924     2437
 229283573     8855

With seed = 1
       BSD  Microsoft
1103527590       41
 377401575    18467
 662824084     6334
1147902781    26500
2035015474    19169

```



## Haskell


```haskell
bsd = tail . iterate (\n -> (n * 1103515245 + 12345) `mod` 2^31)
msr = map (`div` 2^16) . tail . iterate (\n -> (214013 * n + 2531011) `mod` 2^31)

main = do
	print $ take 10 $ bsd 0 -- can take seeds other than 0, of course
	print $ take 10 $ msr 0
```


=={{header|Icon}} and {{header|Unicon}}==
The following LCRNG's behave in the same way maintaining the state (seed) from round to round.  There is an srand procedure for each lcrng that maintains the seed state and allows the user to assign a new state.

```Icon
link printf

procedure main()
   printf("       BSD        MS\n")
   every 1 to 10 do
      printf("%10s %10s\n",rand_BSD(),rand_MS())
end

procedure srand_BSD(x)             #: seed random
static seed
   return seed := \x | \seed | 0   # parm or seed or zero if none
end

procedure rand_BSD()               #: lcrng
   return srand_BSD((1103515245 * srand_BSD() + 12345) % 2147483648)
end

procedure srand_MS(x)              #: seed random
static seed
   return seed := \x | \seed | 0   # parm or seed or zero if none
end

procedure rand_MS()                #: lcrng
   return ishift(srand_MS((214013 * srand_MS() + 2531011) % 2147483648),-16)
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf]


## J

'''Solution:'''

```j
lcg=: adverb define
 0 m lcg y                     NB. default seed of 0
:
 'a c mod'=. x: m
 }. (mod | c + a * ])^:(<y+1) x
)

rand_bsd=: (1103515245 12345 , <.2^31) lcg
rand_ms=: (2^16) <.@:%~ (214013 2531011 , <.2^31) lcg
```

'''Example Use:'''

```j
   rand_bsd 10
12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 1293799192 794471793 551188310
   654583775 rand_bsd 4
1449466924 229283573 1109335178 1051550459
   rand_ms 10
38 7719 21238 2437 8855 11797 8365 32285 10450 30612
   1 rand_ms 5                  NB. seed of 1
41 18467 6334 26500 19169
```



## Java

```java
import java.util.stream.IntStream;
import static java.util.stream.IntStream.iterate;

public class LinearCongruentialGenerator {
    final static int mask = (1 << 31) - 1;

    public static void main(String[] args) {
        System.out.println("BSD:");
        randBSD(0).limit(10).forEach(System.out::println);

        System.out.println("\nMS:");
        randMS(0).limit(10).forEach(System.out::println);
    }

    static IntStream randBSD(int seed) {
        return iterate(seed, s -> (s * 1_103_515_245 + 12_345) & mask).skip(1);
    }

    static IntStream randMS(int seed) {
        return iterate(seed, s -> (s * 214_013 + 2_531_011) & mask).skip(1)
                .map(i -> i >> 16);
    }
}
```



```txt
BSD:
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

MS:
38
7719
21238
2437
8855
11797
8365
32285
10450
30612
```



## jq

Currently, jq arithmetic is based on IEEE 754 64-bit numbers. As a result, it is trivial to implement the Microsoft linear congruential generator (LCG), but the BSD generator requires some kind of "big integer" support.  In this section, therefore, we first present functions to support the Microsoft LCG, and then present functions to support the LCG on the assumption that a suitable jq "BigInt" library is available.

### =Microsoft LCG=


```jq
# 15-bit integers generated using the same formula as rand()
# from the Microsoft C Runtime.
# Input: [ count, state, rand ]
def next_rand_Microsoft:
  .[0] as $count | .[1] as $state
  | ( (214013 * $state) + 2531011) % 2147483648 # mod 2^31
  | [$count+1 , ., (. / 65536 | floor) ];

# Generate the first n pseudo-random numbers:
def rand_Microsoft(seed; n):
  [0,seed]
  | next_rand_Microsoft  # the seed is not so random
  | recurse(if .[0] < n then next_rand_Microsoft else empty end)
  | .[2];
```

'''Example''':
 rand_Microsoft(1;5)
```sh
41
18467
6334
26500
19169
```


### =BSD LCG=

The following code has been tested with the "BigInt" library at [https://gist.github.com/pkoppstein/d06a123f30c033195841].

```jq
# BSD rand()
# Input: [count, previous]
def next_rand_berkeley:
  long_multiply("1103515245" ; .[1]|tostring) as $lm
  | long_add( $lm; "12345") as $la
  # mod 2^31
  | [.[0] + 1, (long_mod( $la; "2147483648") | tonumber) ];

# Generate n values
def rand_berkeley(seed; n):
  [0, seed]
  | next_rand_berkeley # skip the seed itself
  | recurse(if .[0] < n then next_rand_berkeley else empty end)
  | .[1];
```

'''Example''':
 rand_berkeley(1;5)
```sh
1103527590
377401575
662824084
1147902781
2035015474
```



## Julia

<tt>getlgc</tt> creates a linear congruential generator as a closure.  This function is used to create the two generators called for by the task.

```julia
function getlgc(r::Integer, a::Integer, c::Integer, m::Integer, sh::Integer)
    state = r
    return function lgcrand()
        state = mod(a * state + c, m)
        return state >> sh
    end
end

seed, nrep = 0, 10
bsdrand = getlgc(seed, 1103515245, 12345, 2 ^ 31, 0)

println("The first $nrep results for a BSD rand seeded with $seed:")
for _ in 1:nrep
    @printf("%14d\n", bsdrand())
end

msrand = getlgc(seed, 214013, 2531011, 2 ^ 31, 16)

println("\nThe first $nrep results for a M\$ rand seeded with $seed:")
for _ in 1:nrep
    @printf("%14d\n", msrand())
end
```


```txt
The first 10 results for a BSD rand seeded with 0:
         12345
    1406932606
     654583775
    1449466924
     229283573
    1109335178
    1051550459
    1293799192
     794471793
     551188310

The first 10 results for a M$ rand seeded with 0:
            38
          7719
         21238
          2437
          8855
         11797
          8365
         32285
         10450
         30612
```



## K


```K
   bsd:{1_ y{((1103515245*x)+12345)!(_2^31)}\x}
   ms:{1_(y{_(((214013*x)+2531011)!(_2^31))}\x)%(_2^16)}

   bsd[0;10]
12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 1293799192 794471793 551188310
   ms[0;10]
38 7719 21238 2437 8855 11797 8365 32285 10450 30612
```



## Kotlin


```scala
// version 1.1.3

class Lcg(val a: Long, val c: Long, val m: Long, val d: Long, val s: Long) {
    private var state = s

    fun nextInt(): Long {
        state = (a * state + c) % m
        return state / d
    }
}

fun main(args: Array<String>) {
    println("First 10 BSD random numbers - seed 0")
    val bsd = Lcg(1103515245, 12345, 1 shl 31, 1, 0)
    for (i in 1..10) println("${bsd.nextInt()}")
    println("\nFirst 10 MSC random numbers - seed 0")
    val msc = Lcg(214013, 2531011, 1 shl 31, 1 shl 16, 0)
    for (i in 1..10) println("${msc.nextInt()}")
}
```


```txt

First 10 BSD random numbers - seed 0
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

First 10 MSC random numbers - seed 0
38
7719
21238
2437
8855
11797
8365
32285
10450
30612

```



## Liberty BASIC


```lb

'by default these are 0
global BSDState
global MSState

for i = 1 to 10
    print randBSD()
next i

print

for i = 1 to 10
    print randMS()
next i

function randBSD()
    randBSD = (1103515245 * BSDState + 12345) mod (2 ^ 31)
    BSDState = randBSD
end function

function randMS()
    MSState = (214013 * MSState + 2531011) mod (2 ^ 31)
    randMS = int(MSState / 2 ^ 16)
end function

```



## Logo


Note that, perhaps ironically, [[UCB Logo]], as of version 6.0, doesn't generate the proper output from the BSD constants; it uses double-precision floating point, which is not enough for some of the intermediate products.  In UCBLogo, the BSD series deviates starting with the third value (see sample output below).


```Logo
; Configuration parameters for Microsoft and BSD implementations
make "LCG_MS [214013 2531011 65536 2147483648]
make "LCG_BSD [1103515245 12345 1 2147483648]

; Default seed is 0
make "_lcg_value 0

; set the seed
to lcg_seed :seed
  make "_lcg_value :seed
end

; generate the next number in the series using the given parameters
to lcg_rand [:config :LCG_MS]
  local "a local "c local "d local "m
  foreach [a c d m] [
    make ? item # :config
  ]
  make "_lcg_value (modulo (sum (product :a :_lcg_value) :c) :m)
  output int quotient :_lcg_value :d
end

foreach (list :LCG_BSD :LCG_MS) [
  lcg_seed 0
  repeat 10 [
    print (lcg_rand ?)
  ]
  print []
]
bye
```


Output:
```txt
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

38
7719
21238
2437
8855
11797
8365
32285
10450
30612

```


UCBLogo output for the BSD section:
```txt
12345
1406932606
654583808
1358247936
2138638336
1459132416
1445521408
370866176
1896597568
1518859008
```



## Lua

This requires Lua 5.3 or later because previous versions didn't have support for large integers or integral arithmetic operations.


```lua
local RNG = {
  new = function(class, a, c, m, rand)
    local self = setmetatable({}, class)
    local state = 0
    self.rnd = function()
      state = (a * state + c) % m
      return rand and rand(state) or state
    end
    self.seed = function(new_seed)
      state = new_seed % m
    end
    return self
  end
}

bsd = RNG:new(1103515245, 12345, 1<<31)
ms = RNG:new(214013, 2531011, 1<<31, function(s) return s>>16 end)

print"BSD:"
for _ = 1,10 do
  print(("\t%10d"):format(bsd.rnd()))
end
print"Microsoft:"
for _ = 1,10 do
  print(("\t%10d"):format(ms.rnd()))
end

```


```txt
BSD:
	     12345
	1406932606
	 654583775
	1449466924
	 229283573
	1109335178
	1051550459
	1293799192
	 794471793
	 551188310
Microsoft:
	        38
	      7719
	     21238
	      2437
	      8855
	     11797
	      8365
	     32285
	     10450
	     30612

```



## Mathematica


```Mathematica
BSDrand[x_] := Mod[x*1103515245 + 12345, 2147483648]
NestList[BSDrand, 0, 10]
-> {0, 12345, 1406932606, 654583775, 1449466924, 229283573, 1109335178, 1051550459, 1293799192, 794471793, 551188310}

MSrand[x_] := Mod[x*214013 + 2531011, 2147483648]
BitShiftRight[ NestList[MSrand, 0, 10], 16]
-> {0, 38, 7719, 21238, 2437, 8855, 11797, 8365, 32285, 10450, 30612}
```



## Maxima


```maxima
seed: 0$
ms_rand() := quotient(seed: mod(214013 * seed + 2531011, 2147483648), 65536)$
makelist(ms_rand(), 20); /* see http://oeis.org/A096558 */

[38, 7719, 21238, 2437, 8855, 11797, 8365, 32285, 10450, 30612, 5853, 28100, 1142, 281,
20537, 15921, 8945, 26285, 2997, 14680]

seed: 0$
bsd_rand() := seed: mod(1103515245 * seed + 12345, 2147483648)$
makelist(bsd_rand(), 20); /* see http://www.randomwalk.de/scimath/prngseqs.txt */

[12345, 1406932606, 654583775, 1449466924, 229283573, 1109335178, 1051550459,
1293799192, 794471793, 551188310, 803550167, 1772930244, 370913197, 639546082, 1381971571,
1695770928, 2121308585, 1719212846, 996984527, 1157490780]
```



## Nim


```nim
proc bsdRand(seed: int): iterator: int =
  var seed = seed
  result = iterator: int =
    while true:
      seed = (1103515245 * seed + 12345) and 0x7fffffff
      yield seed

proc msvcrtRand(seed: int): iterator: int =
  var seed = seed
  result = iterator: int =
    while true:
      seed = (214013 * seed + 2531011) and 0x7fffffff
      yield seed
```



## Oforth


Function genLCG returns a block object that, when performed, will return the next random number from the LCG.


```Oforth
: genLCG(a, c, m, seed)
| ch |
   Channel newSize(1) dup send(seed) drop ->ch
   #[ ch receive a * c + m mod dup ch send drop ] ;
```


```txt

genLCG(1103515245, 12345, 2 31 pow asInteger, 0) #[ dup perform println ] times(10) drop
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

genLCG(214013, 2531011, 2 31 pow asInteger, 0) #[ dup perform 65536 / println ] times(10) drop
38
7719
21238
2437
8855
11797
8365
32285
10450
30612

```



## PARI/GP

Note that up to PARI/GP version 2.4.0, <code>random()</code> used a linear congruential generator.

```parigp
BSDseed=Mod(1,1<<31);
MSFTseed=Mod(1,1<<31);
BSD()=BSDseed=1103515245*BSDseed+12345;lift(BSDseed);
MSFT()=MSFTseed=214013*MSFTseed+2531011;lift(MSFTseed)%(1<<31);
```



## Pascal


```pascal
Program LinearCongruentialGenerator(output);
{$mode iso}
var
  x1, x2: int64;

function bsdrand: cardinal;
  const
    a = 1103515245;
    c = 12345;
    m = 2147483648;
  begin
    x1 := (a * x1 + c) mod m;
    bsdrand := x1;
  end;

function msrand: cardinal;
  const
    a = 214013;
    c = 2531011;
    m = 2147483648;
  begin
    x2 := (a * x2 + c) mod m;
    msrand := x2 div 65536;
  end;

var
  i: cardinal;
begin
  writeln('      BSD            MS');
  x1 := 0;
  x2 := 0;
  for i := 1 to 10 do
    writeln(bsdrand:12, msrand:12);
end.

```

Output:

```txt
      BSD            MS
       12345          38
  1406932606        7719
   654583775       21238
  1449466924        2437
   229283573        8855
  1109335178       11797
  1051550459        8365
  1293799192       32285
   794471793       10450
   551188310       30612
```



## Perl

Creates a magic scalar whose value is next in the LCG sequence when read.
```perl
use strict;
package LCG;

use overload '0+'  => \&get;

use integer;
sub gen_bsd { (1103515245 * shift() + 12345) % (1 << 31) }

sub gen_ms  {
	my $s = (214013 * shift() + 2531011) % (1 << 31);
	$s, $s / (1 << 16)
}

sub set { $_[0]->{seed} = $_[1] } # srand
sub get {
	my $o = shift;
	($o->{seed}, my $r) = $o->{meth}->($o->{seed});
	$r //= $o->{seed}
}

sub new {
	my $cls = shift;
	my %opts = @_;
	bless {
		seed => $opts{seed},
		meth => $opts{meth} eq 'MS' ? \&gen_ms : \&gen_bsd,
	}, ref $cls || $cls;
}

package main;

my $rand = LCG->new;

print "BSD:\n";
print "$rand\n" for 1 .. 10;

$rand = LCG->new(meth => 'MS');

print "\nMS:\n";
print "$rand\n" for 1 .. 10;
```
output<lang>BSD:
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

MS:
38
7719
21238
2437
8855
11797
8365
32285
10450
30612
```



## Perl 6


We'll define subroutines implementing the LCG algorithm for each version.  We'll make them return a lazy list.


```perl6
constant modulus = 2**31;
sub bsd  {
    $^seed, ( 1103515245 * * + 12345 ) % modulus ... *
}
sub ms   {
    map * +> 16, (
	$^seed, ( 214013 * * + 2531011 ) % modulus ... *
    )
}

say 'BSD LCG first 10 values (first one is the seed):';
.say for bsd(0)[^10];

say "\nMS LCG first 10 values (first one is the seed):";
.say for ms(0)[^10];
```



```txt
BSD LCG first 10 values (first one is the seed):
0
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793

MS LCG first 10 values (first one is the seed):
0
38
7719
21238
2437
8855
11797
8365
32285
10450
```



## Phix

As per the comments, I had to resort to gmp to get BSDrnd() to work on 32-bit.

```Phix
atom seed

include builtins/mpfr.e

function BSDrnd()
    -- oh dear, native only works on 64-bit,
    -- as per ERRE and UCBLogo above on 32-bit...
--  seed = remainder(1103515245 * seed + 12345, #8000_0000)
    -- so, resort to gmp, with the added twist than both
    -- 1103515245 and #8000_0000 are greater than 1GB and
    -- therefore a smidge too big & need some extra help...
    mpz z = mpz_init(seed),
        h8 = mpz_init("2147483648") -- (ie #8000_0000)
    mpz_mul_si(z,z,5)
    mpz_mul_si(z,z,1103515245/5)    -- (do in two <1GB factors)
    mpz_add_si(z,z,12345)
    mpz_fdiv_r(z,z,h8)
    seed = mpz_get_atom(z)
    return seed
end function

function MSrnd()
    seed = and_bits(seed*214013+2531011,#7FFFFFFF)
    return floor(seed/power(2,16))
end function

seed = 0
?"BSDrnd"
for i=1 to 10 do printf(1,"%d\n",BSDrnd()) end for
seed = 0
?"MSrnd"
for i=1 to 10 do printf(1,"%d\n",MSrnd()) end for
```

```txt

"BSDrnd"
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310
"MSrnd"
38
7719
21238
2437
8855
11797
8365
32285
10450
30612

```



## PHP

```php
<?php
function bsd_rand($seed) {
    return function() use (&$seed) {
        return $seed = (1103515245 * $seed + 12345) % (1 << 31);
    };
}

function msvcrt_rand($seed) {
    return function() use (&$seed) {
        return ($seed = (214013 * $seed + 2531011) % (1 << 31)) >> 16;
    };
}

$lcg = bsd_rand(0);
echo "BSD ";
for ($i = 0; $i < 10; $i++)
    echo $lcg(), " ";
echo "\n";

$lcg = msvcrt_rand(0);
echo "Microsoft ";
for ($i = 0; $i < 10; $i++)
    echo $lcg(), " ";
echo "\n";
?>
```



## PicoLisp


```PicoLisp
(zero *BsdSeed *MsSeed)

(de bsdRand ()
   (setq *BsdSeed
      (& (+ 12345 (* 1103515245 *BsdSeed)) `(dec (** 2 31))) ) )

(de msRand ()
   (>> 16
      (setq *MsSeed
         (& (+ 2531011 (* 214013 *MsSeed)) `(dec (** 2 31))) ) ) )
```

Output:

```txt
: (do 7 (printsp (bsdRand)))
12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 -> 1051550459

: (do 12 (printsp (msRand)))
38 7719 21238 2437 8855 11797 8365 32285 10450 30612 5853 28100 -> 28100
```



## PL/I

<lang>
(nofixedoverflow, nosize):
LCG: procedure options (main);
   declare i fixed binary;

   put skip list ('BSD', 'MS');
   do i = 1 to 20;
      put skip list (BSD(), MS());
   end;

bsd: procedure returns (fixed binary (31));
    declare const fixed binary static initial (12345);
    declare s fixed binary (31) static initial (123456789);

    s = s * 1103515245 + const;
    s = isrl(isll(s,1), 1);
    return (s);
end bsd;
ms: procedure returns (fixed binary (15));
    declare const fixed binary (31) static initial (2531011);
    declare s     fixed binary (31) static initial (123456789);

    s = s * 214013 + const;
    s = isrl(isll(s,1), 1);
    return (isrl(s,16));

end ms;

end LCG;

```

OUTPUT:

```txt

BSD                     MS
     231794730              13259
    1126946331              26974
    1757975480              13551
     850994577              30354
    1634557174              18709
     707246327              15861
    1397699428              16906
    1035569613              21981
    1904890498               8603
    1335160211              12911
    1434329552              18110
    1273099721               3228
    1250890958              27918
    1016516591              17989
    1097566972              22768
     436938117              23599
    1175171034               7712
    1059748875              15601
     308566760               7038
     534615297              21512

```



## PowerShell


```powershell

Function msstate{
    Param($current_seed)
    Return (214013*$current_seed+2531011)%2147483648}

Function randMS{
    Param($MSState)
    Return [int]($MSState/65536)}

Function randBSD{
    Param($BSDState)
    Return (1103515245*$BSDState+12345)%2147483648}

Write-Host "MS: seed=0"
$seed=0 #initialize seed
For($i=1;$i-le5;$i++){
    $seed = msstate($seed)
    $rand = randMS($seed)
    Write-Host $rand}

Write-Host "BSD: seed=0"
$seed=0 #initialize seed
For($j=1;$j-le5;$j++){
    $seed = randBSD($seed)
    Write-Host $seed}

```


```txt

MS: seed=0
39
7720
21238
2437
8855
BSD: seed=0
12345
1406932606
654583775
1449466924
229283573

```



## PureBasic


```purebasic
Procedure ms_LCG(seed.q = -1)
  Static state.q
  If seed >= 0
    state = seed
  Else
    state = (state * 214013 + 2531011) % (1 << 31)
    ProcedureReturn state >> 16
  EndIf
EndProcedure

Procedure.q bsd_LCG(seed.q = -1)
  Static state.q
  If seed >= 0
    state = seed
  Else
    state = (state * 1103515245 + 12345) % (1 << 31)
    ProcedureReturn state
  EndIf
EndProcedure

If OpenConsole()
  Define i
  PrintN("BSD (seed = 1)")
  bsd_LCG(1)
  For i = 1 To 5
    PrintN(Str(bsd_LCG()))
  Next

  PrintN(#CRLF$ + "MS (seed = 1)")
  ms_LCG(1)
  For i = 1 To 5
    PrintN(Str(ms_LCG()))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
BSD (seed = 1)
1103527590
377401575
662824084
1147902781
2035015474

MS (seed = 1)
41
18467
6334
26500
19169
```



## Python


```python
def bsd_rand(seed):
   def rand():
      rand.seed = (1103515245*rand.seed + 12345) & 0x7fffffff
      return rand.seed
   rand.seed = seed
   return rand

def msvcrt_rand(seed):
   def rand():
      rand.seed = (214013*rand.seed + 2531011) & 0x7fffffff
      return rand.seed >> 16
   rand.seed = seed
   return rand
```

```python
def bsd_rand(seed):
   def rand():
      nonlocal seed
      seed = (1103515245*seed + 12345) & 0x7fffffff
      return seed
   return rand

def msvcrt_rand(seed):
   def rand():
      nonlocal seed
      seed = (214013*seed + 2531011) & 0x7fffffff
      return seed >> 16
   return rand
```



## Racket


The following solution uses generators and transcribes the mathematical formulas above directly. It does not attempt to be efficient.


```racket

#lang racket
(require racket/generator)

(define (bsd-update state_n)
  (modulo (+ (* 1103515245 state_n) 12345)
          (expt 2 31)))

(define (ms-update state_n)
  (modulo (+ (* 214013 state_n) 2531011)
          (expt 2 31)))

(define ((rand update ->rand) seed)
  (generator ()
   (let loop ([state_n seed])
     (define state_n+1 (update state_n))
     (yield (->rand state_n+1))
     (loop state_n+1))))

(define bsd-rand (rand bsd-update identity))
(define ms-rand (rand ms-update (λ (x) (quotient x (expt 2 16)))))

```



## REXX


```rexx
/*REXX program uses a congruential generator that simulates the old BSD  and  MS random */
/*──────────── number generators.    BSD= 0 ──► (2**31)-1           MS= 0 ──► (2**16)-1 */
numeric digits 20                                /*use enough dec. digs for the multiply*/

  do seed=0  to 1                                /*perform for seed=0  and also  seed=1.*/
  bsd= seed;    ms= seed                         /*assign  SEED  to  two REXX variables.*/
  say center('seed='seed, 79, "─")               /*display the seed in a title/separator*/
                                                 /* [↓]  show 20 rand #'s for each seed.*/
      do j=1  for 20                             /*generate and display 20 rand numbers.*/
      bsd = (1103515245 * bsd  +    12345)    //    2**31
      ms  = (    214013 *  ms  +  2531011)    //    2**31
      say '  state'   right(j,3)   " BSD"   right(bsd,     11)   left('', 13),
                                   " MS"    right( ms,     11)   left('',  5),
                                   " rand"  right(ms%2**16, 6)
      end   /*j*/
  end       /*seed*/                             /*stick a fork in it,  we're all done. */
```

<pre style="font-size:84%">
────────────────────────────────────seed=0─────────────────────────────────────
  state   1  BSD       12345                MS     2531011        rand     38
  state   2  BSD  1406932606                MS   505908858        rand   7719
  state   3  BSD   654583775                MS  1391876949        rand  21238
  state   4  BSD  1449466924                MS   159719620        rand   2437
  state   5  BSD   229283573                MS   580340855        rand   8855
  state   6  BSD  1109335178                MS   773150046        rand  11797
  state   7  BSD  1051550459                MS   548247209        rand   8365
  state   8  BSD  1293799192                MS  2115878600        rand  32285
  state   9  BSD   794471793                MS   684884587        rand  10450
  state  10  BSD   551188310                MS  2006221698        rand  30612
  state  11  BSD   803550167                MS   383622205        rand   5853
  state  12  BSD  1772930244                MS  1841626636        rand  28100
  state  13  BSD   370913197                MS    74896543        rand   1142
  state  14  BSD   639546082                MS    18439398        rand    281
  state  15  BSD  1381971571                MS  1345953809        rand  20537
  state  16  BSD  1695770928                MS  1043415696        rand  15921
  state  17  BSD  2121308585                MS   586225427        rand   8945
  state  18  BSD  1719212846                MS  1722639754        rand  26285
  state  19  BSD   996984527                MS   196417061        rand   2997
  state  20  BSD  1157490780                MS   962080852        rand  14680
────────────────────────────────────seed=1─────────────────────────────────────
  state   1  BSD  1103527590                MS     2745024        rand     41
  state   2  BSD   377401575                MS  1210316419        rand  18467
  state   3  BSD   662824084                MS   415139642        rand   6334
  state   4  BSD  1147902781                MS  1736732949        rand  26500
  state   5  BSD  2035015474                MS  1256316804        rand  19169
  state   6  BSD   368800899                MS  1030492215        rand  15724
  state   7  BSD  1508029952                MS   752224798        rand  11478
  state   8  BSD   486256185                MS  1924036713        rand  29358
  state   9  BSD  1062517886                MS  1766988168        rand  26962
  state  10  BSD   267834847                MS  1603301931        rand  24464
  state  11  BSD   180171308                MS   373929026        rand   5705
  state  12  BSD   836760821                MS  1844513277        rand  28145
  state  13  BSD   595337866                MS  1525789900        rand  23281
  state  14  BSD   790425851                MS  1102819423        rand  16827
  state  15  BSD  2111915288                MS   652855718        rand   9961
  state  16  BSD  1149758321                MS    32201169        rand    491
  state  17  BSD  1644289366                MS   196285776        rand   2995
  state  18  BSD  1388290519                MS   782671571        rand  11942
  state  19  BSD  1647418052                MS   316395082        rand   4827
  state  20  BSD  1675546029                MS   356309989        rand   5436

```



## Ruby

You can create multiple instances of LCG::Berkeley or LCG::Microsoft. Each instance privately keeps the original seed in @seed, and the current state in @r. Each class resembles the core Random class, but with fewer features. The .new method takes a seed. The #rand method returns the next random number. The #seed method returns the original seed.


```ruby
module LCG
  module Common
    # The original seed of this generator.
    attr_reader :seed

    # Creates a linear congruential generator with the given _seed_.
    def initialize(seed)
      @seed = @r = seed
    end
  end

  # LCG::Berkeley generates 31-bit integers using the same formula
  # as BSD rand().
  class Berkeley
    include Common
    def rand
      @r = (1103515245 * @r + 12345) & 0x7fff_ffff
    end
  end

  # LCG::Microsoft generates 15-bit integers using the same formula
  # as rand() from the Microsoft C Runtime.
  class Microsoft
    include Common
    def rand
      @r = (214013 * @r + 2531011) & 0x7fff_ffff
      @r >> 16
    end
  end
end
```


The next example sets the seed to 1, and prints the first 5 random numbers.


```ruby
lcg = LCG::Berkeley.new(1)
p (1..5).map {lcg.rand}
# prints [1103527590, 377401575, 662824084, 1147902781, 2035015474]

lcg = LCG::Microsoft.new(1)
p (1..5).map {lcg.rand}
# prints [41, 18467, 6334, 26500, 19169]
```




## Run BASIC


```runbasic
global bsd
global ms
print "Num  ___Bsd___";chr$(9);"__Ms_"
for i = 1 to 10
    print using("##",i);using("############",bsdRnd());chr$(9);using("#####",msRnd())
next i

function bsdRnd()
    bsdRnd = (1103515245 * bsd + 12345) mod (2 ^ 31)
    bsd = bsdRnd
end function

function msRnd()
    ms = (214013 * ms + 2531011) mod (2 ^ 31)
    msRnd = int(ms / 2 ^ 16)
end function
```


```txt

Num  ___Bsd___	__Ms_
 1       12345	   38
 2  1406932606	 7719
 3   654583775	21238
 4  1449466924	 2437
 5   229283573	 8855
 6  1109335178	11797
 7  1051550459	 8365
 8  1293799192	32285
 9   794471793	10450
10   551188310	30612
```




## Rust


```rust
extern crate rand;

pub use rand::{Rng, SeedableRng};

pub struct BsdLcg {
    state: u32,
}

impl Rng for BsdLcg {
    // Because the output is in the range [0, 2147483647], this should technically be `next_u16`
    // (the largest integer size which is fully covered, as `rand::Rng` assumes).  The `rand`
    // crate does not provide it however.  If serious usage is required, implementing this
    // function as a concatenation of two `next_u16`s (elsewhere defined) should work.
    fn next_u32(&mut self) -> u32 {
        self.state = self.state.wrapping_mul(1_103_515_245).wrapping_add(12_345);
        self.state %= 1 << 31;
        self.state
    }
}

impl SeedableRng<u32> for BsdLcg {
    fn from_seed(seed: u32) -> Self {
        Self { state: seed }
    }
    fn reseed(&mut self, seed: u32) {
        self.state = seed;
    }
}

pub struct MsLcg {
    state: u32,
}

impl Rng for MsLcg {
    // Similarly, this outputs in the range [0, 32767] and should output a `u8`.  Concatenate
    // four `next_u8`s for serious usage.
    fn next_u32(&mut self) -> u32 {
        self.state = self.state.wrapping_mul(214_013).wrapping_add(2_531_011);
        self.state %= 1 << 31;
        self.state >> 16 // rand_n = state_n / 2^16
    }
}

impl SeedableRng<u32> for MsLcg {
    fn from_seed(seed: u32) -> Self {
        Self { state: seed }
    }
    fn reseed(&mut self, seed: u32) {
        self.state = seed;
    }
}

fn main() {
    println!("~~~ BSD ~~~");
    let mut bsd = BsdLcg::from_seed(0);
    for _ in 0..10 {
        println!("{}", bsd.next_u32());
    }

    println!("~~~ MS ~~~");
    let mut ms = MsLcg::from_seed(0);
    for _ in 0..10 {
        println!("{}", ms.next_u32());
    }

    // Because we have implemented the `rand::Rng` trait, we can generate a variety of other types.
    println!("~~~ Others ~~~");
    println!("{:?}", ms.gen::<[u32; 5]>());
    println!("{}", ms.gen::<bool>());
    println!("{}", ms.gen_ascii_chars().take(15).collect::<String>());
}
```



## Scala


```scala
object LinearCongruentialGenerator {
  def bsdRandom(rseed:Int):Iterator[Int]=new Iterator[Int]{
    var seed=rseed
    override def hasNext:Boolean=true
    override def next:Int={seed=(seed * 1103515245 + 12345) & Int.MaxValue; seed}
  }

  def msRandom(rseed:Int):Iterator[Int]=new Iterator[Int]{
    var seed=rseed
    override def hasNext:Boolean=true
    override def next:Int={seed=(seed * 214013 + 2531011) & Int.MaxValue; seed >> 16}
  }

  def toString(it:Iterator[Int], n:Int=20)=it take n mkString ", "

  def main(args:Array[String]){
    println("-- seed 0 --")
    println("BSD: "+ toString(bsdRandom(0)))
    println("MS : "+ toString(msRandom(0)))

    println("-- seed 1 --")
    println("BSD: "+ toString(bsdRandom(1)))
    println("MS : "+ toString( msRandom(1)))
  }
}
```

```txt
-- seed 0 --
BSD: 12345, 1406932606, 654583775, 1449466924, 229283573, 1109335178, 1051550459, 1293799192,
794471793, 551188310, 803550167, 1772930244, 370913197, 639546082, 1381971571, 1695770928,
2121308585, 1719212846, 996984527, 1157490780

MS : 38, 7719, 21238, 2437, 8855, 11797, 8365, 32285, 10450, 30612, 5853, 28100, 1142, 281, 20537,
15921, 8945, 26285, 2997, 14680

-- seed 1 --
BSD: 1103527590, 377401575, 662824084, 1147902781, 2035015474, 368800899, 1508029952, 486256185,
1062517886, 267834847, 180171308, 836760821, 595337866, 790425851, 2111915288, 1149758321,
1644289366, 1388290519, 1647418052, 1675546029

MS : 41, 18467, 6334, 26500, 19169, 15724, 11478, 29358, 26962, 24464, 5705, 28145, 23281, 16827,
9961, 491, 2995, 11942, 4827, 5436
```



## Scheme


```scheme
(define ((bsd-rand seed)) (set! seed (remainder (+ (* 1103515245 seed) 12345) 2147483648)) seed)

(define ((msvcrt-rand seed)) (set! seed (remainder (+ (* 214013 seed) 2531011) 2147483648)) (quotient seed 65536))

; auxiliary function to get a list of 'n random numbers from generator 'r
(define (rand-list r n) = (if (zero? n) '() (cons (r) (rand-list r (- n 1)))))

(rand-list (bsd-rand 0) 10)
; (12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 1293799192 794471793 551188310)

(rand-list (msvcrt-rand 0) 10)
; (38 7719 21238 2437 8855 11797 8365 32285 10450 30612)
```



## Seed7

Seed7 provides also a random number generator.
The random function is overloaded for many types. E.g.: The library [http://seed7.sourceforge.net/libraries/integer.htm integer.s7i]
defines [http://seed7.sourceforge.net/libraries/integer.htm#rand%28in_integer,in_integer%29 rand(lower, upper)].
The parameters specifiy the lower and upper bound of the desired random value.
The library [http://seed7.sourceforge.net/libraries/array.htm array.s7i] defines
[http://seed7.sourceforge.net/libraries/array.htm#rand%28in_arrayType%29 rand(arr)]. This function selects a random element from an array.


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

var bigInteger: bsdSeed is 0_;
var bigInteger: msSeed is 0_;

const func integer: bsdRand is func
  result
    var integer: bsdRand is 0;
  begin
    bsdSeed := (1103515245_ * bsdSeed + 12345_) mod 2147483648_;
    bsdRand := ord(bsdSeed);
  end func;

const func integer: msRand is func
  result
    var integer: msRand is 0;
  begin
    msSeed := (214013_ * msSeed + 2531011_) mod 2147483648_;
    msRand := ord(msSeed) mdiv 65536;
  end func;

const proc: main is func
  local
    var integer: i is 0;
  begin
    writeln("         BSD          MS");
    for i range 1 to 10 do
      writeln(bsdRand lpad 12 <& msRand lpad 12);
    end for;
  end func;
```


Output:

```txt

         BSD          MS
       12345          38
  1406932606        7719
   654583775       21238
  1449466924        2437
   229283573        8855
  1109335178       11797
  1051550459        8365
  1293799192       32285
   794471793       10450
   551188310       30612

```



## SequenceL

Uses the Random library provided by SequenceL to create new Random Number Generators


```sequenceL

import <Utilities/Random.sl>;

main(args(2)) :=
	let
		bsdRandomGenerator := newRandomGenerator(0, 0, 2147483647, bsdNext);
		msRandomGenerator := newRandomGenerator(0, 0, 32767, msNext);

		// Create a random sequence with each one of the generators
		numbers := getRandomSequence([bsdRandomGenerator, msRandomGenerator], 10).Value;
	in
		"BSD Values: " ++ toString(numbers[1]) ++
		"\nMS Values: " ++ toString(numbers[2]);

bsdNext(RG) :=
	let
		newSeed := ((1103515245 -> int64 * RG.Seed + 12345) mod 2147483648) -> int32;
	in
		(Value : newSeed,
		Generator : (Seed : newSeed, RandomMin : RG.RandomMin, RandomMax : RG.RandomMax, NextFunction : RG.NextFunction));

msNext(RG) :=
	let
		newSeed := ((214013 -> int64 * RG.Seed + 2531011) mod 2147483648) -> int32;
	in
		(Value : newSeed / 65536,
		Generator : (Seed : newSeed, RandomMin : RG.RandomMin, RandomMax : RG.RandomMax, NextFunction : RG.NextFunction));

```

Output

```txt

BSD Values: [12345,1406932606,654583775,1449466924,229283573,1109335178,1051550459,1293799192,794471793,551188310]
MS Values: [38,7719,21238,2437,8855,11797,8365,32285,10450,30612]

```


## Sidef

```ruby
module LCG {

  # Creates a linear congruential generator and remembers the initial seed.
  class Common(r) {
     has seed = r
  }

  # LCG::Berkeley generates 31-bit integers using the same formula
  # as BSD rand().
  class Berkeley < Common {
    method rand {
      self.r = ((1103515245 * self.r + 12345) & 0x7fff_ffff);
    }
  }

  # LCG::Microsoft generates 15-bit integers using the same formula
  # as rand() from the Microsoft C Runtime.
  class Microsoft < Common {
    method rand {
      self.r = ((214013 * self.r + 2531011) & 0x7fff_ffff);
      self.r >> 16;
    }
  }
}

var lcg1 = LCG::Berkeley(1)
say 5.of { lcg1.rand }

var lcg2 = LCG::Microsoft(1)
say 5.of { lcg2.rand }
```

```txt

[1103527590, 377401575, 662824084, 1147902781, 2035015474]
[41, 18467, 6334, 26500, 19169]

```



## Sparkling


```sparkling
var states = {
	"BSD": 0,
	"MS": 0
};

function BSD_seed(n) {
	states.BSD = n;
}

function BSD_rand() {
	return states.BSD = (1103515245 * states.BSD + 12345) % (1 << 31);
}

function Microsoft_seed(n) {
	states.MS = n;
}

function Microsoft_rand() {
	return (states.MS = (214013 * states.MS + 2531011) % (1 << 31)) % (1 << 15);
}
```


Output seen after seeding both generators with 0:


```sparkling>spn:8
 Microsoft_seed(0);
spn:9> Microsoft_rand()
= 7875
spn:10> Microsoft_rand()
= 3706
spn:11> Microsoft_rand()
= 23381
spn:12> Microsoft_rand()
= 8388
spn:13> Microsoft_rand()
= 19575
spn:14> BSD_seed(0);
spn:15> BSD_rand()
= 12345
spn:16> BSD_rand()
= 1406932606
spn:17> BSD_rand()
= 654583775
spn:18> BSD_rand()
= 1449466924
spn:19> BSD_rand()
= 229283573
```



## Stata



```stata
mata
function rand_bsd(u) {
	m = 65536
	u1 = floor(u/m)
	u2 = mod(u,m)
	a1 = 16838
	a2 = 20077
	b = 12345
	u = mod((a1*u2+a2*u1)*m+a2*u2+b,2147483648)
	return(u)
}

function rand_ms(u) {
	u = mod(214013*u+2531011,2147483648)
	return(floor(u/65536))
}

function rand_seq(f,seed,n) {
	a = J(n,1,.)
	for (i=1; i<=n; i++) a[i] = (*f)(seed)
	return(a)
}

rand_seq(&rand_bsd(),1,10)
rand_seq(&rand_ms(),0,10)
```


'''Output''': compare with OEIS '''[http://oeis.org/A096553 A096553]''' and '''[http://oeis.org/A096558 A096558]'''.


```txt
                 1
     +--------------+
   1 |  1103527590  |
   2 |   377401575  |
   3 |   662824084  |
   4 |  1147902781  |
   5 |  2035015474  |
   6 |   368800899  |
   7 |  1508029952  |
   8 |   486256185  |
   9 |  1062517886  |
  10 |   267834847  |
     +--------------+


            1
     +---------+
   1 |     38  |
   2 |   7719  |
   3 |  21238  |
   4 |   2437  |
   5 |   8855  |
   6 |  11797  |
   7 |   8365  |
   8 |  32285  |
   9 |  10450  |
  10 |  30612  |
     +---------+
```



## Swift



```Swift
import Cocoa

class LinearCongruntialGenerator {

    var state = 0 //seed of 0 by default
    let a, c, m, shift: Int

    //we will use microsoft random by default
    init() {
        self.a = 214013
        self.c = 2531011
        self.m = Int(pow(2.0, 31.0)) //2^31 or 2147483648
        self.shift = 16
    }

    init(a: Int, c: Int, m: Int, shift: Int) {
        self.a = a
        self.c = c
        self.m = m //2^31 or 2147483648
        self.shift = shift
    }

    func seed(seed: Int) -> Void {
        state = seed;
    }

    func random() -> Int {
        state = (a * state + c) % m
        return state >> shift
    }
}

let microsoftLinearCongruntialGenerator = LinearCongruntialGenerator()
let BSDLinearCongruntialGenerator = LinearCongruntialGenerator(a: 1103515245, c: 12345, m: 2147483648, shift: 0)

print("Microsft Rand:")
for(var i = 0; i < 10; i++)
{
    print(microsoftLinearCongruntialGenerator.random())
}

print("") //new line for readability
print("BSD Rand:")
for(var i = 0; i < 10; i++)
{
    print(BSDLinearCongruntialGenerator.random())
}
```

```txt
Microsft Rand:
38
7719
21238
2437
8855
11797
8365
32285
10450
30612

BSD Rand:
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310
```



## Tcl

Using an object-oriented solution, inspired by (but not a translation of) the [[#Ruby|Ruby]] solution above.

```tcl
package require Tcl 8.6

# General form of a linear-congruential RNG
oo::class create LCRNG {
    variable seed A B C D
    constructor {init a b c d} {
	if {$init < 1} {set init [clock clicks]}
	variable seed $init A $a B $b C $c D $d
    }
    method rand {} {
	set seed [expr {($A * $seed + $B) % $C}]
	return [expr {$seed / $D}]
    }
    method srand x {
	set seed $x
    }
}
# Subclass to introduce constants
oo::class create BSDRNG {
    superclass LCRNG
    constructor {{initialSeed -1}} {
	next $initialSeed 1103515245 12345 [expr {2**31}] 1
    }
}
oo::class create MSRNG {
    superclass LCRNG
    constructor {{initialSeed -1}} {
	next $initialSeed 214013 2531011 [expr {2**31}] [expr {2**16}]
    }
}
```

Demo code:

```tcl
proc sample rng {foreach - {1 2 3 4 5} {lappend r [$rng rand]}; join $r ", "}
puts BSD:\t\[[sample [BSDRNG new 1]]\]
puts MS:\t\[[sample [MSRNG new 1]]\]
```

Output:

```txt

BSD:	[1103527590, 377401575, 662824084, 1147902781, 2035015474]
MS:	[41, 18467, 6334, 26500, 19169]

```



## uBasic/4tH

uBasic is an integer BASIC without any bitwise operations. That's why a trick is used when it enters the negative domain. Unfortunately, it is not portable and must be adjusted for different integer widths. This 32-bit version produces the proper result, though.
<lang>w = 32                                 ' Change for different integer size
b = 0                                  ' Initial BSD seed
m = 0                                  ' Initial MS seed

Print "BSD"                            ' Get the first 10 numbers from BSD
For i = 1 To 10
    GoSub _randBSD
    Print Pop()
Next i

Print

Print "Microsoft"                      ' Get the first 10 numbers from MS
For i = 1 To 10
    GoSub _randMS
    Print Pop()
Next i

End


_randBSD                               ' ( n1 -- n2)
    Push (1103515245 * b + 12345)      ' Compensate for the sign bit
    If Tos() < 0 Then Push (Pop() - (2 ^ (w-1)))
    b = Pop() % (2 ^ 31)               ' Now we got a number less than 2^31
    Push b                             ' So we can complete the operation
Return


_randMS                                ' ( n1 -- n2)
    Push (214013 * m + 2531011)        ' Compensate for the sign bit
    If Tos() < 0 Then Push (Pop() - (2 ^ (w-1)))
    m =  Pop() % (2 ^ 31)              ' Now we got a number less than 2^31
    Push m / (2 ^ 16)                  ' So we can complete the operation
Return
```

```txt
BSD
12345
1406932606
654583775
1449466924
229283573
1109335178
1051550459
1293799192
794471793
551188310

Microsoft
38
7719
21238
2437
8855
11797
8365
32285
10450
30612

0 OK, 0:908

```



## UNIX Shell



```bash
#! /bin/bash

function BSD() {
  SEED=$(((1103515245 * $SEED + 12345) % 2**31))
  echo "  $SEED"
}

function MS() {
  SEED=$(((214013 * $SEED + 2531011) % 2**31))
  echo "  $(($SEED / 2**16))"
}

function output() {
  SEED=0
  echo "$1"

  for i in {1..10}; do
    eval "$1"
  done

  echo ""
}

output BSD
output MS
```


```txt
BSD
  12345
  1406932606
  654583775
  1449466924
  229283573
  1109335178
  1051550459
  1293799192
  794471793
  551188310

MS
  38
  7719
  21238
  2437
  8855
  11797
  8365
  32285
  10450
  30612


```



## VBA


```vb
Public stateBSD As Variant
Public stateMS As Variant
Private Function bsd() As Long
    Dim temp As Variant
    temp = CDec(1103515245 * stateBSD + 12345)
    temp2 = temp / 2 ^ 31
    temp3 = CDec(WorksheetFunction.Floor_Precise(temp2))
    stateBSD = temp - (2 ^ 31) * temp3
    bsd = stateBSD
End Function
Private Function ms() As Integer
    Dim temp As Variant
    temp = CDec(214013 * stateMS + 2531011)
    temp2 = temp / 2 ^ 31
    temp3 = CDec(WorksheetFunction.Floor_Precise(temp2))
    stateMS = temp - (2 ^ 31) * temp3
    ms = stateMS \ 2 ^ 16
End Function
Public Sub main()
    stateBSD = CDec(0)
    stateMS = CDec(0)
    Debug.Print "       BSD", "   MS"
    For i = 1 To 10
        Debug.Print Format(bsd, "@@@@@@@@@@"), Format(ms, "@@@@@")
    Next i
End Sub
```
```txt
       BSD       MS
     12345       38
1406932606     7719
 654583775    21238
1449466924     2437
 229283573     8855
1109335178    11797
1051550459     8365
1293799192    32285
 794471793    10450
 551188310    30612
```


## X86 Assembly


These programs are based off of the implementations described in this article: "https://software.intel.com/en-us/articles/fast-random-number-generator-on-the-intel-pentiumr-4-processor", using the Microsoft equation.

First example using integer instructions.

```asm
;x86-64 assembly code for Microsoft Windows
;Tested in windows 7 Enterprise Service Pack 1 64 bit
;With the AMD FX(tm)-6300 processor
;Assembled with NASM version 2.11.06
;Linked to C library with gcc version 4.9.2 (x86_64-win32-seh-rev1, Built by MinGW-W64 project)

;Assembled and linked with the following commands:
;nasm -f win64 <filename>.asm -o <filename>.obj
;gcc <filename>.obj -o <filename>

;Takes number of iterations to run RNG loop as command line parameter.

extern printf,puts,atoi,exit,time,malloc

section .data
align 64
errmsg_argnumber: db "There should be no more than one argument.",0
align 64
errmsg_noarg: db "Number of iterations was not specified.",0
align 64
errmsg_zeroiterations: db "Zero iterations of RNG loop specified.",0

align 64
errmsg_timefail: db "Unable to retrieve calender time.",0
align 64
errmsg_mallocfail: db "Unable to allocate memory for array of random numbers.",0

align 64
fmt_random: db "The %u number generated is %d",0xa,0xd,0

section .bss

section .text
global main

main:

;check for argument
cmp rcx,1
jle err_noarg

;ensure that only one argument was entered
cmp rcx,2
jg err_argnumber


;get number of times to iterate get_random
mov rcx,[rdx + 8]
call atoi


;ensure that number of iterations is greater than 0
cmp rax,0
jle err_zeroiterations
mov rcx,rax


;calculate space needed for an array containing the random numbers
shl rcx,2

;move size of array into r14
mov r14,rcx

;reserve memory for array of random numbers with malloc
call malloc

cmp rax,0
jz err_mallocfail

;pointer to array in r15
mov r15,rax


;seed the RNG using time()
xor rcx,rcx
call time

;ensure that time returns valid output
cmp rax,-1
jz err_timefail

;calculate address of end of array in r14
add r14,r15


;pointer to array of random numbers in r15
;address of end of array in r14
;current address in array in rdi
;multiplier in rbx
;seed in rax
;current random number in rcx


;prepare random number generator

mov rdi,r15

mov rbx,214013


get_random:

;multiply by 214013 and add 2561011 to get next state
mul ebx
add eax,2531011

;shr by 16 and AND with 0x7FFF to get current random number
mov ecx,eax
shr ecx,16
and ecx,0x7fff

;store random number in array
mov [rdi],ecx

add rdi,4
cmp rdi,r14
jl get_random


;pointer to array of random numbers in r15
;address of end of array in r14
;current address in array in rdi
;array index in rsi


xor rsi,rsi
mov rdi,r15

print_random:

mov rcx,fmt_random
mov rdx,rsi
mov r8d,[rdi]
call printf

add rsi,1
add rdi,4
cmp rdi,r14
jl print_random

xor rcx,rcx
call exit


;;;;;;;;;;ERROR MESSAGES;;;;;;;;;;;;;;;;

err_argnumber:

mov rcx,errmsg_argnumber
call puts

jmp exit_one


err_noarg:

mov rcx,errmsg_noarg
call puts

jmp exit_one


err_zeroiterations:

mov rcx,errmsg_zeroiterations
call puts

jmp exit_one


err_timefail:

mov rcx,errmsg_timefail
call puts

jmp exit_one


err_mallocfail:

mov rcx,errmsg_mallocfail
call puts


exit_one:

mov rcx,1
call exit
```


Second example using AVX instructions.
```asm
;x86-64 assembly code for Microsoft Windows
;Tested in windows 7 Enterprise Service Pack 1 64 bit
;With the AMD FX(tm)-6300 processor
;Assembled with NASM version 2.11.06
;Linked to C library with gcc version 4.9.2 (x86_64-win32-seh-rev1, Built by MinGW-W64 project)

;Assembled and linked with the following commands:
;nasm -f win64 <filename>.asm -o <filename>.obj
;gcc <filename>.obj -o <filename>

;Takes number of iterations to run RNG loop as command line parameter.

extern printf,puts,atoi,exit,time,_aligned_malloc

section .data
align 64
errmsg_argnumber: db "There should be no more than one argument.",0
align 64
errmsg_noarg: db "Number of iterations was not specified.",0
align 64
errmsg_zeroiterations: db "Zero iterations of RNG loop specified.",0

align 64
errmsg_timefail: db "Unable to retrieve calender time.",0
align 64
errmsg_mallocfail: db "Unable to allocate memory for array of random numbers.",0

align 64
fmt_random: db "The %u number generated is %d",0xa,0xd,0

align 16
multiplier: dd 214013,17405,214013,69069
align 16
addend: dd 2531011, 10395331, 13737667, 1
align 16
mask: dd  0xffffffff,0,0xffffffff,0
align 16
masklo: dd 0x7fff,0x7fff,0x7fff,0x7fff

section .bss

section .text
global main

main:

;check for argument
cmp rcx,1
jle err_noarg

;ensure that only one argument was entered
cmp rcx,2
jg err_argnumber


;get number of times to iterate get_random
mov rcx,[rdx + 8]
call atoi


;ensure that number of iterations is greater than 0
cmp rax,0
jle err_zeroiterations
mov rcx,rax


;calculate space needed for an array containing the random numbers
shl rcx,4

;move size of array into r14
mov r14,rcx

;16 byte alignment boundary
mov rdx,16

;reserve memory aligned to 16 byte boundary for array with _aligned_malloc
call _aligned_malloc

cmp rax,0
jz err_mallocfail

;pointer to array in r15
mov r15,rax


;seed the RNG using time()
xor rcx,rcx
call time

;ensure that time returns valid output
cmp rax,-1
jz err_timefail


;pointer to array of random numbers in r15
;address of end of array at in r14
;states stored in xmm0

;calculate address of end of array in r14
add r14,r15

;load seed,seed+1,seed,seed+1 into xmm0
lea rbx,[rax - 1]
shl rax,32
or rax,rbx

movq xmm0,rax
vpslldq xmm1,xmm0,8
vpor xmm0,xmm0,xmm1


;pointer to array of random numbers in r15
;address of end of array in r14
;current address in array in rdi
;current states in xmm0
;multiplier in xmm1
;addened in xmm2
;mask in xmm3
;masklo in xmm4
;split seed in xmm5
;current set of random numbers in xmm6

;prepare random number generator

mov rdi,r15

vmovdqa xmm1,[multiplier]
vmovdqa xmm2,[addend]
vmovdqa xmm3,[mask]
vmovdqa xmm4,[masklo]


get_random:

;arrange order of current states to 2,3,0,1 and store in split seed
vpshufd xmm5,xmm0,10110001b

;multiply current states by multiplier
vpmulld xmm0,xmm0,xmm1

;set order of multiplier to 2,3,0,1
vpshufd xmm1,xmm1,10110001b

;multiply split seed by multiplier
vpmulld xmm5,xmm5,xmm1

;and current states with mask
vpand xmm0,xmm0,xmm3

;and current split seed with mask
vpand xmm5,xmm5,xmm3

;set order of split seed to 2,3,0,1
vpshufd xmm5,xmm5,10110001b

;or current states with split seed
vpor xmm0,xmm0,xmm5

;add adder to current states
vpaddd xmm0,xmm0,xmm2


;shift vector right by two bytes
vpsrldq xmm6,xmm0,2

;and each state with 0x7fff
vpand xmm6,xmm6,xmm4

vmovdqa [rdi],xmm6

add rdi,16
cmp rdi,r14
jl get_random


;pointer to array of random numbers in r15
;address of end of array in r14
;current address in array in rdi
;array index in rsi


xor rsi,rsi
mov rdi,r15

print_random:

mov rcx,fmt_random
mov rdx,rsi
mov r8d,[rdi]
call printf

add rsi,1
add rdi,4
cmp rdi,r14
jl print_random

xor rcx,rcx
call exit


;;;;;;;;;;ERROR MESSAGES;;;;;;;;;;;;;;;;

err_argnumber:

mov rcx,errmsg_argnumber
call puts

jmp exit_one


err_noarg:

mov rcx,errmsg_noarg
call puts

jmp exit_one


err_zeroiterations:

mov rcx,errmsg_zeroiterations
call puts

jmp exit_one


err_timefail:

mov rcx,errmsg_timefail
call puts

jmp exit_one


err_mallocfail:

mov rcx,errmsg_mallocfail
call puts


exit_one:

mov rcx,1
call exit
```


Integer instruction example:

```txt
F:\>lcgint.exe 20
The 0 number generated is 20272
The 1 number generated is 4467
The 2 number generated is 8618
The 3 number generated is 1587
The 4 number generated is 2687
The 5 number generated is 21398
The 6 number generated is 29522
The 7 number generated is 27724
The 8 number generated is 23875
The 9 number generated is 2399
The 10 number generated is 4086
The 11 number generated is 923
The 12 number generated is 23002
The 13 number generated is 11586
The 14 number generated is 13200
The 15 number generated is 22090
The 16 number generated is 26528
The 17 number generated is 14271
The 18 number generated is 10476
The 19 number generated is 9981

F:\>
```


AVX instruction example:

```txt
F:\>lcgavx.exe 5
The 0 number generated is 20370
The 1 number generated is 45
The 2 number generated is 20541
The 3 number generated is 15699
The 4 number generated is 23637
The 5 number generated is 30131
The 6 number generated is 26151
The 7 number generated is 27319
The 8 number generated is 26933
The 9 number generated is 28417
The 10 number generated is 16647
The 11 number generated is 14840
The 12 number generated is 29228
The 13 number generated is 16968
The 14 number generated is 1027
The 15 number generated is 12099
The 16 number generated is 17170
The 17 number generated is 23893
The 18 number generated is 18556
The 19 number generated is 16434

F:\>
```



## XPL0

It's not easy just by looking at the numbers generated if they are
sufficiently random. You might notice that the BSD numbers alternate odd
and even, which is pretty bad. A simple but effective test is to
simulate falling snowflakes.

[[File:LCG1XPL0.gif|right]]
[[File:LCG2XPL0.gif|right]]


```XPL0
include c:\cxpl\codes;
int R;

func BSD;
[R:= (1103515245*R + 12345) & $7FFF_FFFF;
return R;
]; \BSD


func MSFT;
[R:= (214013*R + 2531011) & $7FFF_FFFF;
return R>>16;
]; \MSFT


int N;
[SetVid(4);             \320x200x2 graphics
R:= 0;                  \initialize seed
for N:= 0 to 5000 do
        Point(rem(BSD/180), rem(BSD/180), 3);
N:= ChIn(1);            \wait for keystoke

SetVid(4);              \320x200x2 graphics
R:= 0;                  \initialize seed
for N:= 0 to 5000 do
        Point(rem(MSFT/180), rem(MSFT/180), 3);
N:= ChIn(1);            \wait for keystoke
SetVid(3);              \restore normal text mode
]
```



## zkl


```zkl
var [private] seed = 0;
fcn srand(s){ seed = s }

const TWO31=(1).shiftLeft(31);

//#define BSD_RAND 1

#ifdef BSD_RAND
   const A=1103515245, C=12345;
   fcn rand{ seed = (seed * A + C) % TWO31 }
#else  // MS rand
   const A=214013, C=2531011, TWO16=(1).shiftLeft(16);
   fcn rand{ (seed = (seed * A + C) % TWO31) / TWO16 }
#endif
```


```zkl
srand(0);
println(rand(),",",rand(),",",rand());
```

```txt

MS:  38,7719,21238
BSD: 12345,1406932606,654583775

```


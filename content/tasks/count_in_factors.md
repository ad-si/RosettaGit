+++
title = "Count in factors"
description = ""
date = 2019-10-08T02:08:00Z
aliases = []
[extra]
id = 9012
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dcl",
  "dwscript",
  "echolisp",
  "eiffel",
  "elixir",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "tcl",
  "vbscript",
  "visual_basic_.net",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Write a program which counts up from   '''1''',   displaying each number as the multiplication of its prime factors.

For the purpose of this task,   '''1'''   (unity)   may be shown as itself.


;Example:
      '''2'''   is prime,   so it would be shown as itself.

      '''6'''   is not prime;   it would be shown as   '''<math>2\times3</math>.'''

'''2144'''   is not prime;   it would be shown as   '''<math>2\times2\times2\times2\times2\times67</math>.'''


## Related tasks

*   [[prime decomposition]]
*   [[factors of an integer]]
*   [[Sieve of Eratosthenes]]
*   [[primality by trial division]]
*   [[factors of a Mersenne number]]
*   [[trial factoring of a Mersenne number]]
*   [[partition an integer X into N primes]]





## 11l

```11l
F get_prime_factors(=li)
   I li == 1
      R ‘1’
   E
      V res = ‘’
      V f = 2
      L
         I li % f == 0
            res ‘’= f
            li /= f
            I li == 1
               L.break
            res ‘’= ‘ x ’
         E
            f++
      R res

L(x) 1..17
   print(‘#4: #.’.format(x, get_prime_factors(x)))
print(‘2144: ’get_prime_factors(2144))
```


```txt

   1: 1
   2: 2
   3: 3
   4: 2 x 2
   5: 5
   6: 2 x 3
   7: 7
   8: 2 x 2 x 2
   9: 3 x 3
  10: 2 x 5
  11: 11
  12: 2 x 2 x 3
  13: 13
  14: 2 x 7
  15: 3 x 5
  16: 2 x 2 x 2 x 2
  17: 17
2144: 2 x 2 x 2 x 2 x 2 x 67

```



## 360 Assembly


```360asm
*        Count in factors          24/03/2017
COUNTFAC CSECT                     assist plig\COUNTFAC
         USING  COUNTFAC,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         L      R6,=F'1'           i=1
       DO WHILE=(C,R6,LE,=F'40')   do i=1 to 40
         LR     R7,R6                n=i
         MVI    F,X'01'              f=true
         MVC    PG,=CL80' '          clear buffer
         LA     R10,PG               pgi=0
         XDECO  R6,XDEC              edit i
         MVC    0(12,R10),XDEC       output i
         LA     R10,12(R10)          pgi=pgi+12
         MVC    0(1,R10),=C'='       output '='
         LA     R10,1(R10)           pgi=pgi+1
       IF C,R7,EQ,=F'1' THEN         if n=1 then
         MVI    0(R10),C'1'            output n
       ELSE     ,                    else
         LA     R8,2                   p=2
       DO WHILE=(CR,R8,LE,R7)          do while p<=n
         LR     R4,R7                    n
         SRDA   R4,32                    ~
         DR     R4,R8                    /p
       IF LTR,R4,Z,R4 THEN               if n//p=0 then
       IF CLI,F,EQ,X'00' THEN              if not f then
         MVC    0(1,R10),=C'*'               output '*'
         LA     R10,1(R10)                   pgi=pgi+1
       ELSE     ,                          else
         MVI    F,X'00'                      f=false
       ENDIF    ,                          endif
         CVD    R8,PP                      convert bin p to packed pp
         MVC    WORK12,MASX12              in fact L13
         EDMK   WORK12,PP+2                edit and mark
         LA     R9,WORK12+12               end of string(p)
         SR     R9,R1                      li=lengh(p)  {r1 from edmk}
         MVC    EDIT12,WORK12              L12<-L13
         LA     R4,EDIT12+12               source+12
         SR     R4,R9                      -lengh(p)
         LR     R5,R9                      lengh(p)
         LR     R2,R10                     target ix
         LR     R3,R9                      lengh(p)
         MVCL   R2,R4                      f=f||p
         AR     R10,R9                     ix=ix+lengh(p)
         LR     R4,R7                      n
         SRDA   R4,32                      ~
         DR     R4,R8                      /p
         LR     R7,R5                      n=n/p
       ELSE     ,                        else
         LA     R8,1(R8)                   p=p+1
       ENDIF    ,                        endif
       ENDDO    ,                      enddo while
       ENDIF    ,                    endif
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
F        DS     X                  flag first factor
         DS     0D                 alignment for cvd
PP       DS     PL8                packed  CL8
EDIT12   DS     CL12               target  CL12
WORK12   DS     CL13               char    CL13
MASX12   DC     X'40',9X'20',X'212060'     CL13
XDEC     DS     CL12               temp
PG       DS     CL80               buffer
         YREGS
         END    COUNTFAC
```

```txt
           1=1
           2=2
           3=3
           4=2*2
           5=5
           6=2*3
           7=7
           8=2*2*2
           9=3*3
          10=2*5
          11=11
          12=2*2*3
          13=13
          14=2*7
          15=3*5
          16=2*2*2*2
          17=17
          18=2*3*3
          19=19
          20=2*2*5
          21=3*7
          22=2*11
          23=23
          24=2*2*2*3
          25=5*5
          26=2*13
          27=3*3*3
          28=2*2*7
          29=29
          30=2*3*5
          31=31
          32=2*2*2*2*2
          33=3*11
          34=2*17
          35=5*7
          36=2*2*3*3
          37=37
          38=2*19
          39=3*13
          40=2*2*2*5

```




## Ada


The solution uses the generic package Prime_Numbers from [[Prime decomposition#Ada]]

;count.adb:

```Ada
with Ada.Command_Line, Ada.Text_IO, Prime_Numbers;

procedure Count is
   package Prime_Nums is new Prime_Numbers
     (Number => Natural, Zero => 0, One => 1, Two => 2); use Prime_Nums;

   procedure Put (List : Number_List) is
   begin
      for Index in List'Range loop
         Ada.Text_IO.Put (Integer'Image (List (Index)));
         if Index /= List'Last then
            Ada.Text_IO.Put (" x");
         end if;
      end loop;
   end Put;

   N     : Natural := 1;
   Max_N : Natural := 15; -- the default for Max_N
begin
   if Ada.Command_Line.Argument_Count = 1 then -- read Max_N from command line
      Max_N := Integer'Value (Ada.Command_Line.Argument (1));
   end if; -- else use the default
   loop
      Ada.Text_IO.Put (Integer'Image (N) & ": ");
      Put (Decompose (N));
      Ada.Text_IO.New_Line;
      N := N + 1;
      exit when N > Max_N;
   end loop;
end Count;
```


```txt
 1:  1
 2:  2
 3:  3
 4:  2 x 2
 5:  5
 6:  2 x 3
 7:  7
 8:  2 x 2 x 2
 9:  3 x 3
 10:  2 x 5
 11:  11
 12:  2 x 2 x 3
 13:  13
 14:  2 x 7
 15:  3 x 5
```



## ALGOL 68

```ALGOL68
OP +:= = (REF FLEX []INT a, INT b) VOID:
   BEGIN
      [⌈a + 1] INT c;
      c[:⌈a] := a;
      c[⌈a+1:] := b;
      a := c
   END;


PROC factorize = (INT nn) []INT:
   BEGIN
      IF nn = 1 THEN (1)
      ELSE
	  INT k := 2, n := nn;
	  FLEX[0]INT result;
	  WHILE n > 1 DO
	      WHILE n MOD k = 0 DO
		  result +:= k;
		  n := n % k
	      OD;
	      k +:= 1
	  OD;
	  result
      FI
   END;

FLEX[0]INT factors;
FOR i TO 22 DO
    factors := factorize (i);
    print ((whole (i, 0), " = "));
    FOR j TO UPB factors DO
       (j /= 1 | print (" × "));
	print ((whole (factors[j], 0)))
    OD;
    print ((new line))
OD
```

```txt
1 = 1
2 = 2
3 = 3
4 = 2 × 2
5 = 5
6 = 2 × 3
7 = 7
8 = 2 × 2 × 2
9 = 3 × 3
10 = 2 × 5
11 = 11
12 = 2 × 2 × 3
13 = 13
14 = 2 × 7
15 = 3 × 5
16 = 2 × 2 × 2 × 2
17 = 17
18 = 2 × 3 × 3
19 = 19
20 = 2 × 2 × 5
21 = 3 × 7
22 = 2 × 11
```


## AutoHotkey

```AutoHotkey
factorize(n){
	if n = 1
		return 1
	if n < 1
		return false
	result := 0, m := n, k := 2
	While n >= k{
		while !Mod(m, k){
			result .= " * " . k, m /= k
		}
		k++
	}
	return SubStr(result, 5)
}
Loop 22
   out .= A_Index ": " factorize(A_index) "`n"
MsgBox % out
```

```txt
1: 1
2: 2
3: 3
4: 2 * 2
5: 5
6: 2 * 3
7: 7
8: 2 * 2 * 2
9: 3 * 3
10: 2 * 5
11: 11
12: 2 * 2 * 3
13: 13
14: 2 * 7
15: 3 * 5
16: 2 * 2 * 2 * 2
17: 17
18: 2 * 3 * 3
19: 19
20: 2 * 2 * 5
21: 3 * 7
22: 2 * 11
```



## AWK


```AWK

# syntax: GAWK -f COUNT_IN_FACTORS.AWK
BEGIN {
    fmt = "%d=%s\n"
    for (i=1; i<=16; i++) {
      printf(fmt,i,factors(i))
    }
    i = 2144; printf(fmt,i,factors(i))
    i = 6358; printf(fmt,i,factors(i))
    exit(0)
}
function factors(n,  f,p) {
    if (n == 1) {
      return(1)
    }
    p = 2
    while (p <= n) {
      if (n % p == 0) {
        f = sprintf("%s%s*",f,p)
        n /= p
      }
      else {
        p++
      }
    }
    return(substr(f,1,length(f)-1))
}

```

<p>output:</p>

```txt

1=1
2=2
3=3
4=2*2
5=5
6=2*3
7=7
8=2*2*2
9=3*3
10=2*5
11=11
12=2*2*3
13=13
14=2*7
15=3*5
16=2*2*2*2
2144=2*2*2*2*2*67
6358=2*11*17*17

```


## BBC BASIC


```bbcbasic
      FOR i% = 1 TO 20
        PRINT i% " = " FNfactors(i%)
      NEXT
      END

      DEF FNfactors(N%)
      LOCAL P%, f$
      IF N% = 1 THEN = "1"
      P% = 2
      WHILE P% <= N%
        IF (N% MOD P%) = 0 THEN
          f$ += STR$(P%) + " x "
          N% DIV= P%
        ELSE
          P% += 1
        ENDIF
      ENDWHILE
      = LEFT$(f$, LEN(f$) - 3)

```

Output:

```txt
         1 = 1
         2 = 2
         3 = 3
         4 = 2 x 2
         5 = 5
         6 = 2 x 3
         7 = 7
         8 = 2 x 2 x 2
         9 = 3 x 3
        10 = 2 x 5
        11 = 11
        12 = 2 x 2 x 3
        13 = 13
        14 = 2 x 7
        15 = 3 x 5
        16 = 2 x 2 x 2 x 2
        17 = 17
        18 = 2 x 3 x 3
        19 = 19
        20 = 2 x 2 x 5
```



## Befunge

Lists the first 100 entries in the sequence. If you wish to extend that, the upper limit is implementation dependent, but may be as low as 130 for an interpreter with signed 8 bit data cells (131 is the first prime outside that range).


```befunge
1>>>
:.48*"=",,::1-#v_.v
$<<<^_@#-"e":+1,+55$2<<<
v4_^#-1:/.:g00_00g1+>>0v
>8*"x",,:00g%!^!%g00:p0<
```


```txt
1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
11 = 11
12 = 2 x 2 x 3
13 = 13
14 = 2 x 7
.
.
.
```



## C

Code includes a dynamically extending prime number list. The program doesn't stop until you kill it, or it runs out of memory, or it overflows.

```c
#include <stdio.h>
#include <stdlib.h>

typedef unsigned long long ULONG;

ULONG get_prime(int idx)
{
        static long n_primes = 0, alloc = 0;
        static ULONG *primes = 0;
        ULONG last, p;
        int i;

        if (idx >= n_primes) {
                if (n_primes >= alloc) {
                        alloc += 16; /* be conservative */
                        primes = realloc(primes, sizeof(ULONG) * alloc);
                }
                if (!n_primes) {
                        primes[0] = 2;
                        primes[1] = 3;
                        n_primes = 2;
                }

                last = primes[n_primes-1];
                while (idx >= n_primes) {
                        last += 2;
                        for (i = 0; i < n_primes; i++) {
                                p = primes[i];
                                if (p * p > last) {
                                        primes[n_primes++] = last;
                                        break;
                                }
                                if (last % p == 0) break;
                        }
                }
        }
        return primes[idx];
}

int main()
{
        ULONG n, x, p;
        int i, first;

        for (x = 1; ; x++) {
                printf("%lld = ", n = x);

                for (i = 0, first = 1; ; i++) {
                        p = get_prime(i);
                        while (n % p == 0) {
                                n /= p;
                                if (!first) printf(" x ");
                                first = 0;
                                printf("%lld", p);
                        }
                        if (n <= p * p) break;
                }

                if (first)      printf("%lld\n", n);
                else if (n > 1) printf(" x %lld\n", n);
                else            printf("\n");
        }
        return 0;
}
```

```txt
1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
11 = 11
12 = 2 x 2 x 3
13 = 13
14 = 2 x 7
.
.
.
```



## C++


```Cpp

#include <iostream>
#include <sstream>
#include <iomanip>
using namespace std;

void getPrimeFactors( int li )
{
    int f = 2; string res;
    if( li == 1 ) res = "1";
    else
    {
	while( true )
	{
	    if( !( li % f ) )
	    {
		stringstream ss; ss << f;
		res += ss.str();
		li /= f; if( li == 1 ) break;
		res += " x ";
	    }
	    else f++;
	}
    }
    cout << res << "\n";
}

int main( int argc, char* argv[] )
{
    for( int x = 1; x < 101; x++ )
    {
	cout << right << setw( 4 ) << x << ": ";
	getPrimeFactors( x );
    }
    cout << 2144 << ": "; getPrimeFactors( 2144 );
    cout << "\n\n";
    return system( "pause" );
}

```

```txt

   1: 1
   2: 2
   3: 3
   4: 2 x 2
   5: 5
   6: 2 x 3
   7: 7
   8: 2 x 2 x 2
   9: 3 x 3
  10: 2 x 5
  11: 11
  12: 2 x 2 x 3
  13: 13
  14: 2 x 7
  15: 3 x 5
  16: 2 x 2 x 2 x 2
  17: 17
  18: 2 x 3 x 3
  19: 19
  20: 2 x 2 x 5
  21: 3 x 7
  22: 2 x 11
  23: 23
  24: 2 x 2 x 2 x 3
  .
  .
  .

```


## C#

```c#
using System;
using System.Collections.Generic;

namespace prog
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			for( int i=1; i<=22; i++ )
			{
				List<int> f = Factorize(i);
				Console.Write( i + ":  " + f[0] );
				for( int j=1; j<f.Count; j++ )
				{
					Console.Write( " * " + f[j] );
				}
				Console.WriteLine();
			}
		}

		public static List<int> Factorize( int n )
		{
			List<int> l = new List<int>();

			if ( n == 1 )
			{
				l.Add(1);
			}
			else
			{
				int k = 2;
				while( n > 1 )
				{
					while( n % k == 0 )
					{
						l.Add( k );
						n /= k;
					}
					k++;
				}
			}
			return l;
		}
	}
}
```



## Clojure


```lisp
(ns listfactors
  (:gen-class))

(defn factors
  "Return a list of factors of N."
  ([n]
   (factors n 2 ()))
  ([n k acc]
   (cond
     (= n 1) (if (empty? acc)
               [n]
               (sort acc))
     (>= k n) (if (empty? acc)
                    [n]
                    (sort (cons n acc)))
    (= 0 (rem n k)) (recur (quot n k) k (cons k acc))
    :else (recur n (inc k) acc))))

(doseq [q (range 1 26)]
  (println q " = " (clojure.string/join " x "(factors q))))

```

```txt

1  =  1
2  =  2
3  =  3
4  =  2 x 2
5  =  5
6  =  2 x 3
7  =  7
8  =  2 x 2 x 2
9  =  3 x 3
10  =  2 x 5
11  =  11
12  =  2 x 2 x 3
13  =  13
14  =  2 x 7
15  =  3 x 5
16  =  2 x 2 x 2 x 2
17  =  17
18  =  2 x 3 x 3
19  =  19
20  =  2 x 2 x 5
21  =  3 x 7
22  =  2 x 11
23  =  23
24  =  2 x 2 x 2 x 3
25  =  5 x 5

```



## CoffeeScript


```coffeescript
count_primes = (max) ->
  # Count through the natural numbers and give their prime
  # factorization.  This algorithm uses no division.
  # Instead, each prime number starts a rolling odometer
  # to help subsequent factorizations.  The algorithm works similar
  # to the Sieve of Eratosthenes, as we note when each prime number's
  # odometer rolls a digit.  (As it turns out, as long as your computer
  # is not horribly slow at division, you're better off just doing simple
  # prime factorizations on each new n vs. using this algorithm.)
  console.log "1 = 1"
  primes = []
  n = 2
  while n <= max
    factors = []
    for prime_odometer in primes
      # digits are an array w/least significant digit in
      # position 0;  for example, [3, [0]] will roll as
      # follows:
      #    [0] -> [1] -> [2] -> [0, 1]
      [base, digits] = prime_odometer
      i = 0
      while true
        digits[i] += 1
        break if digits[i] < base
        digits[i] = 0
        factors.push base
        i += 1
        if i >= digits.length
          digits.push 0

    if factors.length == 0
      primes.push [n, [0, 1]]
      factors.push n
    console.log "#{n} = #{factors.join('*')}"
    n += 1

  primes.length

num_primes = count_primes 10000
console.log num_primes
```



## Common Lisp

Auto extending prime list:

```lisp
(defparameter *primes*
  (make-array 10 :adjustable t :fill-pointer 0 :element-type 'integer))

(mapc #'(lambda (x) (vector-push x *primes*)) '(2 3 5 7))

(defun extend-primes (n)
  (let ((p (+ 2 (elt *primes* (1- (length *primes*))))))
    (loop for i = p then (+ 2 i)
	  while (<= (* i i) n) do
	  (if (primep i t) (vector-push-extend i *primes*)))))

(defun primep (n &optional skip)
  (if (not skip) (extend-primes n))
  (if (= n 1) nil
      (loop for p across *primes* while (<= (* p p) n)
	    never (zerop (mod n p)))))

(defun factors (n)
  (extend-primes n)
  (loop with res for x across *primes* while (> n (* x x)) do
	(loop while (zerop (rem n x)) do
	      (setf n (/ n x))
	      (push x res))
	finally (return (if (> n 1) (cons n res) res))))

(loop for n from 1 do
      (format t "~a: ~{~a~^ × ~}~%" n (reverse (factors n))))
```

```txt
1:
2: 2
3: 3
4: 4
5: 5
6: 2 × 3
7: 7
8: 2 × 2 × 2
9: 9
10: 2 × 5
11: 11
12: 2 × 2 × 3
13: 13
14: 2 × 7
...
```

Without saving the primes, and not all that much slower (probably because above code was not well-written):

```lisp
(defun factors (n)
  (loop with res for x from 2 to (isqrt n) do
	(loop while (zerop (rem n x)) do
	      (setf n (/ n x))
	      (push x res))
	finally (return (if (> n 1) (cons n res) res))))

(loop for n from 1 do
      (format t "~a: ~{~a~^ × ~}~%" n (reverse (factors n))))
```



## D


```d
int[] factorize(in int n) pure nothrow
in {
    assert(n > 0);
} body {
    if (n == 1) return [1];
    int[] result;
    int m = n, k = 2;
    while (n >= k) {
        while (m % k == 0) {
            result ~= k;
            m /= k;
        }
        k++;
    }
    return result;
}

void main() {
    import std.stdio;
    foreach (i; 1 .. 22)
        writefln("%d: %(%d × %)", i, i.factorize());
}
```

```txt
1: 1
2: 2
3: 3
4: 2 × 2
5: 5
6: 2 × 3
7: 7
8: 2 × 2 × 2
9: 3 × 3
10: 2 × 5
11: 11
12: 2 × 2 × 3
13: 13
14: 2 × 7
15: 3 × 5
16: 2 × 2 × 2 × 2
17: 17
18: 2 × 3 × 3
19: 19
20: 2 × 2 × 5
21: 3 × 7
```


### Alternative Version

{{libheader|uiprimes}} Library ''uiprimes'' is a homebrew library to generate prime numbers upto the maximum 32bit unsigned integer range 2^32-1, by using a pre-generated bit array of [[Sieve of Eratosthenes]] (a dll in size of ~256M bytes :p ).

```d
import std.stdio, std.math, std.conv, std.algorithm,
       std.array, std.string, import xt.uiprimes;

pragma(lib, "uiprimes.lib");

// function _factorize_ included in uiprimes.lib
ulong[] factorize(ulong n) {
    if (n == 0) return [];
    if (n == 1) return [1];
    ulong[] res;
    uint limit = cast(uint)(1 + sqrt(n));
    foreach (p; Primes(limit)) {
        if (n == 1) break;
        if (0UL == (n % p))
            while((n > 1) && (0UL == (n % p ))) {
                res ~= p;
                n /= p;
            }
    }
    if (n > 1)
        res ~= [n];
    return res;
}

string productStr(T)(in T[] nums) {
    return nums.map!text().join(" x ");
}

void main() {
    foreach (i; 1 .. 21)
        writefln("%2d = %s", i, productStr(factorize(i)));
}
```


## DCL

Assumes file primes.txt is a list of prime numbers;

```DCL
$ close /nolog primes
$ on control_y then $ goto clean
$
$ n = 1
$ outer_loop:
$  x = n
$  open primes primes.txt
$
$  loop1:
$   read /end_of_file = prime primes prime
$   prime = f$integer( prime )
$   loop2:
$    t = x / prime
$    if t * prime .eq. x
$    then
$     if f$type( factorization ) .eqs. ""
$     then
$      factorization = f$string( prime )
$     else
$      factorization = factorization + "*" + f$string( prime )
$     endif
$     if t .eq. 1 then $ goto done
$     x = t
$     goto loop2
$    else
$     goto loop1
$    endif
$ prime:
$  if f$type( factorization ) .eqs. ""
$  then
$   factorization = f$string( x )
$  else
$   factorization = factorization + "*" + f$string( x )
$  endif
$ done:
$  write sys$output f$fao( "!4SL = ", n ), factorization
$  delete /symbol factorization
$  close primes
$  n = n + 1
$  if n .le. 2144 then $ goto outer_loop
$  exit
$
$ clean:
$ close /nolog primes
```

```txt
$ @count_in_factors
   1 = 1
   2 = 2
   3 = 3
   4 = 2*2
   5 = 5
   6 = 2*3
...
2144 = 2*2*2*2*2*67
```



## DWScript


```delphi
function Factorize(n : Integer) : String;
begin
   if n <= 1 then
      Exit('1');
   var k := 2;
   while n >= k do begin
      while (n mod k) = 0 do begin
         Result += ' * '+IntToStr(k);
         n := n div k;
      end;
      Inc(k);
   end;
   Result:=SubStr(Result, 4);
end;

var i : Integer;
for i := 1 to 22 do
   PrintLn(IntToStr(i) + ': ' + Factorize(i));
```

```txt
1: 1
2: 2
3: 3
4: 2 * 2
5: 5
6: 2 * 3
7: 7
8: 2 * 2 * 2
9: 3 * 3
10: 2 * 5
11: 11
12: 2 * 2 * 3
13: 13
14: 2 * 7
15: 3 * 5
16: 2 * 2 * 2 * 2
17: 17
18: 2 * 3 * 3
19: 19
20: 2 * 2 * 5
21: 3 * 7
22: 2 * 11
```



## EchoLisp


```scheme

(define (task (nfrom 2) (range 20))
 (for ((i (in-range nfrom (+ nfrom range))))
     (writeln i "=" (string-join (prime-factors i) " x "))))


```

```txt

(task 1_000_000_000)

1000000000     =     2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5
1000000001     =     7 x 11 x 13 x 19 x 52579
1000000002     =     2 x 3 x 43 x 983 x 3943
1000000003     =     23 x 307 x 141623
1000000004     =     2 x 2 x 41 x 41 x 148721
1000000005     =     3 x 5 x 66666667
1000000006     =     2 x 500000003
1000000007     =     1000000007
1000000008     =     2 x 2 x 2 x 3 x 3 x 7 x 109 x 109 x 167
1000000009     =     1000000009
1000000010     =     2 x 5 x 17 x 5882353
1000000011     =     3 x 29 x 11494253
1000000012     =     2 x 2 x 11 x 47 x 79 x 6121
1000000013     =     7699 x 129887
1000000014     =     2 x 3 x 13 x 103 x 124471
1000000015     =     5 x 7 x 31 x 223 x 4133
1000000016     =     2 x 2 x 2 x 2 x 62500001
1000000017     =     3 x 3 x 111111113
1000000018     =     2 x 500000009
1000000019     =     83 x 12048193

```



## Eiffel


```Eiffel


class
	COUNT_IN_FACTORS

feature

	display_factor (p: INTEGER)
			-- Factors of all integers up to 'p'.
		require
			p_positive: p > 0
		local
			factors: ARRAY [INTEGER]
		do
			across
				1 |..| p as c
			loop
				io.new_line
				io.put_string (c.item.out + "%T")
				factors := factor (c.item)
				across
					factors as f
				loop
					io.put_integer (f.item)
					if f.is_last = False then
						io.put_string (" x ")
					end
				end
			end
		end


        factor (p: INTEGER): ARRAY [INTEGER]
			-- Prime decomposition of 'p'.
		require
			p_positive: p > 0
		local
			div, i, next, rest: INTEGER
		do
			create Result.make_empty
			if p = 1 then
				Result.force (1, 1)
			end
			div := 2
			next := 3
			rest := p
			from
				i := 1
			until
				rest = 1
			loop
				from
				until
					rest \\ div /= 0
				loop
					Result.force (div, i)
					rest := (rest / div).floor
					i := i + 1
				end
				div := next
				next := next + 2
			end
		ensure
			is_divisor: across Result as r all p \\ r.item = 0 end
		end
end


```

Test Output:


```txt

   1       1
   2       2
   3       3
   4       2 x 2
   5       5
   6       2 x 3
   7       7
   8       2 x 2 x 2
   9       3 x 3
  10       2 x 5
...
4990       2 x 5 x 499
4991       7 x 23 x 31
4992       2 x 2 x 2 x 2 x 2 x 2 x 2 x 3 x 13
4993       4993
4994       2 x 11 x 227
4995       3 x 3 x 3 x 5 x 37
4996       2 x 2 x 1249
4997       19 x 263
4998       2 x 3 x 7 x 7 x 17
4999       4999
5000       2 x 2 x 2 x 5 x 5 x 5 x 5


```



## Elixir


```elixir
defmodule RC do
  def factor(n), do: factor(n, 2, [])

  def factor(n, i, fact) when n < i*i, do: Enum.reverse([n|fact])
  def factor(n, i, fact) do
    if rem(n,i)==0, do: factor(div(n,i), i, [i|fact]),
                    else: factor(n, i+1, fact)
  end
end

Enum.each(1..20, fn n ->
  IO.puts "#{n}: #{Enum.join(RC.factor(n)," x ")}" end)
```


```txt

1: 1
2: 2
3: 3
4: 2 x 2
5: 5
6: 2 x 3
7: 7
8: 2 x 2 x 2
9: 3 x 3
10: 2 x 5
11: 11
12: 2 x 2 x 3
13: 13
14: 2 x 7
15: 3 x 5
16: 2 x 2 x 2 x 2
17: 17
18: 2 x 3 x 3
19: 19
20: 2 x 2 x 5

```



## Euphoria


```euphoria
function factorize(integer n)
    sequence result
    integer k
    if n = 1 then
        return {1}
    else
        k = 2
        result = {}
        while n > 1 do
            while remainder(n, k) = 0 do
                result &= k
                n /= k
            end while
            k += 1
        end while
        return result
    end if
end function

sequence factors
for i = 1 to 22 do
    printf(1, "%d: ", i)
    factors = factorize(i)
    for j = 1 to length(factors)-1 do
        printf(1, "%d * ", factors[j])
    end for
    printf(1, "%d\n", factors[$])
end for
```

```txt
1: 1
2: 2
3: 3
4: 2 * 2
5: 5
6: 2 * 3
7: 7
8: 2 * 2 * 2
9: 3 * 3
10: 2 * 5
11: 11
12: 2 * 2 * 3
13: 13
14: 2 * 7
15: 3 * 5
16: 2 * 2 * 2 * 2
17: 17
18: 2 * 3 * 3
19: 19
20: 2 * 2 * 5
21: 3 * 7
22: 2 * 11

```


=={{header|F_Sharp|F#}}==

```fsharp
let factorsOf (num) =
    Seq.unfold (fun (f, n) ->
        let rec genFactor (f, n) =
            if f > n then None
            elif n % f = 0 then Some (f, (f, n/f))
            else genFactor (f+1, n)
        genFactor (f, n)) (2, num)

let showLines = Seq.concat (seq { yield seq{ yield(Seq.singleton 1)}; yield (Seq.skip 2 (Seq.initInfinite factorsOf))})

showLines |> Seq.iteri (fun i f -> printfn "%d = %s" (i+1) (String.Join(" * ", Seq.toArray f)))
```

```txt
1 = 1
2 = 2
3 = 3
4 = 2 * 2
5 = 5
6 = 2 * 3
7 = 7
8 = 2 * 2 * 2
9 = 3 * 3
10 = 2 * 5
:
2140 = 2 * 2 * 5 * 107
2141 = 2141
2142 = 2 * 3 * 3 * 7 * 17
2143 = 2143
2144 = 2 * 2 * 2 * 2 * 2 * 67
2145 = 3 * 5 * 11 * 13
2146 = 2 * 29 * 37
2147 = 19 * 113
:

```



## Factor


```factor

USING: math.parser math.primes.factors math.ranges ;
IN: scratchpad "1: 1" print 2 20 [a,b] [ dup pprint ": " write factors [ number>string ] map " x " join print ] each

```

```txt

1: 1
2: 2
3: 3
4: 2 x 2
5: 5
6: 2 x 3
7: 7
8: 2 x 2 x 2
9: 3 x 3
10: 2 x 5
11: 11
12: 2 x 2 x 3
13: 13
14: 2 x 7
15: 3 x 5
16: 2 x 2 x 2 x 2
17: 17
18: 2 x 3 x 3
19: 19
20: 2 x 2 x 5

```



## Forth


```forth
: .factors ( n -- )
  2
  begin  2dup dup * >=
  while  2dup /mod swap
         if   drop  1+ 1 or    \ next odd number
         else -rot nip  dup . ." x "
         then
  repeat
  drop . ;

: main ( n -- )
  ." 1 : 1" cr
  1+ 2 ?do i . ." : " i .factors cr loop ;

15 main bye
```




## Fortran

Please find the example output along with the build instructions in the comments at the start of the FORTRAN 2008 source. Compiler: gfortran from the GNU compiler collection. Command interpreter: bash.  The code writes j assertions which don't prove primality of the factors but does prove they are the factors.

This algorithm creates a sieve of Eratosthenes, storing the largest prime factor to mark composites.  It then finds prime factors by repeatedly looking up the value in the sieve, then dividing by the factor found until the value is itself prime.  Using the sieve table to store factors rather than as a plain bitmap was to me a novel idea.


```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Thu Jun  6 23:29:06
!
!a=./f && make $a && echo -2 | OMP_NUM_THREADS=2 $a
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
! assert           1 = */           1
! assert           2 = */           2
! assert           3 = */           3
! assert           4 = */           2           2
! assert           5 = */           5
! assert           6 = */           2           3
! assert           7 = */           7
! assert           8 = */           2           2           2
! assert           9 = */           3           3
! assert          10 = */           2           5
! assert          11 = */          11
! assert          12 = */           3           2           2
! assert          13 = */          13
! assert          14 = */           2           7
! assert          15 = */           3           5
! assert          16 = */           2           2           2           2
! assert          17 = */          17
! assert          18 = */           3           2           3
! assert          19 = */          19
! assert          20 = */           2           2           5
! assert          21 = */           3           7
! assert          22 = */           2          11
! assert          23 = */          23
! assert          24 = */           3           2           2           2
! assert          25 = */           5           5
! assert          26 = */           2          13
! assert          27 = */           3           3           3
! assert          28 = */           2           2           7
! assert          29 = */          29
! assert          30 = */           5           2           3
! assert          31 = */          31
! assert          32 = */           2           2           2           2           2
! assert          33 = */           3          11
! assert          34 = */           2          17
! assert          35 = */           5           7
! assert          36 = */           3           3           2           2
! assert          37 = */          37
! assert          38 = */           2          19
! assert          39 = */           3          13
! assert          40 = */           5           2           2           2

module prime_mod

  ! sieve_table stores 0 in prime numbers, and a prime factor in composites.
  integer, dimension(:), allocatable :: sieve_table
  private :: PrimeQ

contains

  ! setup routine must be called first!
  subroutine sieve(n) ! populate sieve_table.  If n is 0 it deallocates storage, invalidating sieve_table.
    integer, intent(in) :: n
    integer :: status, i, j
    if ((n .lt. 1) .or. allocated(sieve_table)) deallocate(sieve_table)
    if (n .lt. 1) return
    allocate(sieve_table(n), stat=status)
    if (status .ne. 0) stop 'cannot allocate space'
    sieve_table(1) = 1
    do i=2,int(sqrt(real(n)))+1
       if (sieve_table(i) .eq. 0) then
          do j = i*i, n, i
             sieve_table(j) = i
          end do
       end if
    end do
  end subroutine sieve

  subroutine check_sieve(n)
    integer, intent(in) :: n
    if (.not. (allocated(sieve_table) .and. ((1 .le. n) .and. (n .le. size(sieve_table))))) stop 'Call sieve first'
  end subroutine check_sieve

  logical function isPrime(p)
    integer, intent(in) :: p
    call check_sieve(p)
    isPrime = PrimeQ(p)
  end function isPrime

  logical function isComposite(p)
    integer, intent(in) :: p
    isComposite = .not. isPrime(p)
  end function isComposite

  logical function PrimeQ(p)
    integer, intent(in) :: p
    PrimeQ = sieve_table(p) .eq. 0
  end function PrimeQ

  subroutine prime_factors(p, rv, n)
    integer, intent(in) :: p ! number to factor
    integer, dimension(:), intent(out) :: rv ! the prime factors
    integer, intent(out) :: n ! number of factors returned
    integer :: i, m
    call check_sieve(p)
    m = p
    i = 1
    if (p .ne. 1) then
       do while ((.not. PrimeQ(m)) .and. (i .lt. size(rv)))
          rv(i) = sieve_table(m)
          m = m/rv(i)
          i = i+1
       end do
    end if
    if (i .le. size(rv)) rv(i) = m
    n = i
  end subroutine prime_factors

end module prime_mod

program count_in_factors
  use prime_mod
  integer :: i, n
  integer, dimension(8) :: factors
  call sieve(40)                ! setup
  do i=1,40
     factors = 0
     call prime_factors(i, factors, n)
     write(6,*)'assert',i,'= */',factors(:n)
  end do
  call sieve(0)                 ! release memory
end program count_in_factors

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub getPrimeFactors(factors() As UInteger, n As UInteger)
  If n < 2 Then Return
  Dim factor As UInteger = 2
  Do
    If n Mod factor = 0 Then
      Redim Preserve factors(0 To UBound(factors) + 1)
      factors(UBound(factors)) = factor
      n \= factor
      If n = 1 Then Return
    Else
      factor += 1
    End If
  Loop
End Sub

Dim factors() As UInteger

For i As UInteger = 1 To 20
  Print Using "##"; i;
  Print " = ";
  If i > 1 Then
    Erase factors
    getPrimeFactors factors(), i
    For j As Integer = LBound(factors) To UBound(factors)
      Print factors(j);
      If j < UBound(factors) Then Print " x ";
    Next j
    Print
  Else
    Print i
  End If
Next i

Print
Print "Press any key to quit"
Sleep
```


```txt

 1 = 1
 2 = 2
 3 = 3
 4 = 2 x 2
 5 = 5
 6 = 2 x 3
 7 = 7
 8 = 2 x 2 x 2
 9 = 3 x 3
10 = 2 x 5
11 = 11
12 = 2 x 2 x 3
13 = 13
14 = 2 x 7
15 = 3 x 5
16 = 2 x 2 x 2 x 2
17 = 17
18 = 2 x 3 x 3
19 = 19
20 = 2 x 2 x 5

```



## Frink

Frink's factoring routines work on arbitrarily-large integers.

```frink
i = 1
while true
{
    println[join[" x ", factorFlat[i]]]
    i = i + 1
}
```



## Go


```go
package main

import "fmt"

func main() {
    fmt.Println("1: 1")
    for i := 2; ; i++ {
        fmt.Printf("%d: ", i)
        var x string
        for n, f := i, 2; n != 1; f++ {
            for m := n % f; m == 0; m = n % f {
                fmt.Print(x, f)
                x = "×"
                n /= f
            }
        }
        fmt.Println()
    }
}
```

```txt

1: 1
2: 2
3: 3
4: 2×2
5: 5
6: 2×3
7: 7
8: 2×2×2
9: 3×3
10: 2×5
...

```



## Groovy


```groovy
def factors(number) {
    if (number == 1) {
        return [1]
    }
    def factors = []
    BigInteger value = number
    BigInteger possibleFactor = 2
    while (possibleFactor <= value) {
        if (value % possibleFactor == 0) {
            factors << possibleFactor
            value /= possibleFactor
        } else {
            possibleFactor++
        }
    }
    factors
}
Number.metaClass.factors = { factors(delegate) }

((1..10) + (6351..6359)).each { number ->
    println "$number = ${number.factors().join(' x ')}"
}
```

```txt
1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
6351 = 3 x 29 x 73
6352 = 2 x 2 x 2 x 2 x 397
6353 = 6353
6354 = 2 x 3 x 3 x 353
6355 = 5 x 31 x 41
6356 = 2 x 2 x 7 x 227
6357 = 3 x 13 x 163
6358 = 2 x 11 x 17 x 17
6359 = 6359
```



## Haskell

Using <code>factorize</code> function from the [[Prime_decomposition#Haskell|prime decomposition]] task,

```haskell
import Data.List (intercalate)

showFactors n = show n ++ " = " ++ (intercalate " * " . map show . factorize) n
-- Pointfree form
showFactors = ((++) . show) <*> ((" = " ++) . intercalate " * " . map show . factorize)
```

isPrime n = n > 1 && noDivsBy primeNums n
<small>
```haskell
Main> print 1 >
 mapM_ (putStrLn . showFactors) [2..]
1
2 = 2
3 = 3
4 = 2 * 2
5 = 5
6 = 2 * 3
7 = 7
8 = 2 * 2 * 2
9 = 3 * 3
10 = 2 * 5
11 = 11
12 = 2 * 2 * 3
. . .

Main> mapM_ (putStrLn . showFactors) [2144..]
2144 = 2 * 2 * 2 * 2 * 2 * 67
2145 = 3 * 5 * 11 * 13
2146 = 2 * 29 * 37
2147 = 19 * 113
2148 = 2 * 2 * 3 * 179
2149 = 7 * 307
2150 = 2 * 5 * 5 * 43
2151 = 3 * 3 * 239
2152 = 2 * 2 * 2 * 269
2153 = 2153
2154 = 2 * 3 * 359
. . .

Main> mapM_ (putStrLn . showFactors) [121231231232155..]
121231231232155 = 5 * 11 * 419 * 5260630559
121231231232156 = 2 * 2 * 97 * 1061 * 294487867
121231231232157 = 3 * 3 * 3 * 131 * 34275157261
121231231232158 = 2 * 19 * 67 * 1231 * 38681033
121231231232159 = 121231231232159
121231231232160 = 2 * 2 * 2 * 2 * 2 * 3 * 5 * 7 * 7 * 5154389083
121231231232161 = 121231231232161
121231231232162 = 2 * 60615615616081
121231231232163 = 3 * 13 * 83 * 191089 * 195991
121231231232164 = 2 * 2 * 253811 * 119410931
121231231232165 = 5 * 137 * 176979899609
. . .
```
</small>
The real solution seems to have to be some sort of a segmented offset sieve of Eratosthenes, storing factors in array's cells instead of just marks. That way the speed of production might not be diminishing as much.

=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
write("Press ^C to terminate")
every f := [i:= 1] | factors(i := seq(2)) do {
   writes(i," : [")
   every writes(" ",!f|"]\n")
   }
end

link factors
```

[http://www.cs.arizona.edu/icon/library/src/procs/factors.icn factors.icn provides factors]
```txt
1 : [ 1 ]
2 : [ 2 ]
3 : [ 3 ]
4 : [ 2 2 ]
5 : [ 5 ]
6 : [ 2 3 ]
7 : [ 7 ]
8 : [ 2 2 2 ]
9 : [ 3 3 ]
10 : [ 2 5 ]
11 : [ 11 ]
12 : [ 2 2 3 ]
13 : [ 13 ]
14 : [ 2 7 ]
15 : [ 3 5 ]
16 : [ 2 2 2 2 ]
...
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Factors.bas"
110 FOR I=1 TO 30
120   PRINT I;"= ";FACTORS$(I)
130 NEXT
140 DEF FACTORS$(N)
150   LET F$=""
160   IF N=1 THEN
170     LET FACTORS$="1"
180   ELSE
190     LET P=2
200     DO WHILE P<=N
210       IF MOD(N,P)=0 THEN
220         LET F$=F$&STR$(P)&"*"
230         LET N=INT(N/P)
240       ELSE
250         LET P=P+1
260       END IF
270     LOOP
280     LET FACTORS$=F$(1:LEN(F$)-1)
290   END IF
300 END DEF
```

```txt
 1 = 1
 2 = 2
 3 = 3
 4 = 2*2
 5 = 5
 6 = 2*3
 7 = 7
 8 = 2*2*2
 9 = 3*3
 10 = 2*5
 11 = 11
 12 = 2*2*3
 13 = 13
 14 = 2*7
 15 = 3*5
 16 = 2*2*2*2
 17 = 17
 18 = 2*3*3
 19 = 19
 20 = 2*2*5
 21 = 3*7
 22 = 2*11
 23 = 23
 24 = 2*2*2*3
 25 = 5*5
 26 = 2*13
 27 = 3*3*3
 28 = 2*2*7
 29 = 29
 30 = 2*3*5
```



## J

'''Solution''':Use J's factoring primitive,
```j
q:
```

'''Example''' (including formatting):
```j
   ('1 : 1',":&> ,"1 ': ',"1 ":@q:) 2+i.10
1 : 1
2 : 2
3 : 3
4 : 2 2
5 : 5
6 : 2 3
7 : 7
8 : 2 2 2
9 : 3 3
10: 2 5
11: 11
```



## Java

```java
public class CountingInFactors{
    public static void main(String[] args){
        for(int i = 1; i<= 10; i++){
            System.out.println(i + " = "+ countInFactors(i));
        }

        for(int i = 9991; i <= 10000; i++){
        	System.out.println(i + " = "+ countInFactors(i));
        }
    }

    private static String countInFactors(int n){
        if(n == 1) return "1";

        StringBuilder sb = new StringBuilder();

        n = checkFactor(2, n, sb);
        if(n == 1) return sb.toString();

        n = checkFactor(3, n, sb);
        if(n == 1) return sb.toString();

        for(int i = 5; i <= n; i+= 2){
            if(i % 3 == 0)continue;

            n = checkFactor(i, n, sb);
            if(n == 1)break;
        }

        return sb.toString();
    }

    private static int checkFactor(int mult, int n, StringBuilder sb){
        while(n % mult == 0 ){
            if(sb.length() > 0) sb.append(" x ");
            sb.append(mult);
            n /= mult;
        }
        return n;
    }
}
```

```txt
1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
9991 = 97 x 103
9992 = 2 x 2 x 2 x 1249
9993 = 3 x 3331
9994 = 2 x 19 x 263
9995 = 5 x 1999
9996 = 2 x 2 x 3 x 7 x 7 x 17
9997 = 13 x 769
9998 = 2 x 4999
9999 = 3 x 3 x 11 x 101
10000 = 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5
```



## JavaScript


```javascript
for(i = 1; i <= 10; i++)
    console.log(i + " : " + factor(i).join(" x "));

function factor(n) {
    var factors = [];
    if (n == 1) return [1];
    for(p = 2; p <= n; ) {
	if((n % p) == 0) {
	    factors[factors.length] = p;
	    n /= p;
	}
	else p++;
    }
    return factors;
}
```

```txt

1 : 1
2 : 2
3 : 3
4 : 2 x 2
5 : 5
6 : 2 x 3
7 : 7
8 : 2 x 2 x 2
9 : 3 x 3
10 : 2 x 5

```



## Julia


```julia
using Primes, Printf
function strfactor(n::Integer)
    n > -2 || return "-1 × " * strfactor(-n)
    isprime(n) || n < 2 && return dec(n)
    f = factor(Vector{typeof(n)}, n)
    return join(f, " × ")
end

lo, hi = -4, 40
println("Factor print $lo to $hi:")
for n in lo:hi
    @printf("%5d = %s\n", n, strfactor(n))
end
```


```txt
Factor print -4 to 40:
   -4 = -1 × 2 × 2
   -3 = -1 × 3
   -2 = -1 × 2
   -1 = -1
    0 = 0
    1 = 1
    2 = 2
    3 = 3
    4 = 2 × 2
    5 = 5
    6 = 2 × 3
    7 = 7
    8 = 2 × 2 × 2
    9 = 3 × 3
   10 = 2 × 5
   11 = 11
   12 = 2 × 2 × 3
   13 = 13
   14 = 2 × 7
   15 = 3 × 5
   16 = 2 × 2 × 2 × 2
   17 = 17
   18 = 2 × 3 × 3
   19 = 19
   20 = 2 × 2 × 5
   21 = 3 × 7
   22 = 2 × 11
   23 = 23
   24 = 2 × 2 × 2 × 3
   25 = 5 × 5
   26 = 2 × 13
   27 = 3 × 3 × 3
   28 = 2 × 2 × 7
   29 = 29
   30 = 2 × 3 × 5
   31 = 31
   32 = 2 × 2 × 2 × 2 × 2
   33 = 3 × 11
   34 = 2 × 17
   35 = 5 × 7
   36 = 2 × 2 × 3 × 3
   37 = 37
   38 = 2 × 19
   39 = 3 × 13
   40 = 2 × 2 × 2 × 5
```



## Kotlin


```scala
// version 1.1.2

fun isPrime(n: Int) : Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun getPrimeFactors(n: Int): List<Int> {
    val factors = mutableListOf<Int>()
    if (n < 1) return factors
    if (n == 1 || isPrime(n)) {
        factors.add(n)
        return factors
    }
    var factor = 2
    var nn = n
    while (true) {
        if (nn % factor == 0) {
            factors.add(factor)
            nn /= factor
            if (nn == 1) return factors
            if (isPrime(nn)) factor = nn
        }
        else if (factor >= 3) factor += 2
        else factor = 3
    }
}

fun main(args: Array<String>) {
    val list = (MutableList(22) { it + 1 } + 2144) + 6358
    for (i in list)
        println("${"%4d".format(i)} = ${getPrimeFactors(i).joinToString(" * ")}")
}
```


```txt

   1 = 1
   2 = 2
   3 = 3
   4 = 2 * 2
   5 = 5
   6 = 2 * 3
   7 = 7
   8 = 2 * 2 * 2
   9 = 3 * 3
  10 = 2 * 5
  11 = 11
  12 = 2 * 2 * 3
  13 = 13
  14 = 2 * 7
  15 = 3 * 5
  16 = 2 * 2 * 2 * 2
  17 = 17
  18 = 2 * 3 * 3
  19 = 19
  20 = 2 * 2 * 5
  21 = 3 * 7
  22 = 2 * 11
2144 = 2 * 2 * 2 * 2 * 2 * 67
6358 = 2 * 11 * 17 * 17

```



## Liberty BASIC


```lb

'see Run BASIC solution
for i = 1000 to 1016
  print i;" = "; factorial$(i)
next
wait
function factorial$(num)
 if num = 1 then factorial$ = "1"
 fct = 2
 while fct <= num
 if (num mod fct) = 0 then
   factorial$ = factorial$ ; x$ ; fct
   x$  = " x "
   num = num / fct
  else
   fct = fct + 1
 end if
 wend
end function
```

```txt

1000 = 2 x 2 x 2 x 5 x 5 x 5
1001 = 7 x 11 x 13
1002 = 2 x 3 x 167
1003 = 17 x 59
1004 = 2 x 2 x 251
1005 = 3 x 5 x 67
1006 = 2 x 503
1007 = 19 x 53
1008 = 2 x 2 x 2 x 2 x 3 x 3 x 7
1009 = 1009
1010 = 2 x 5 x 101
1011 = 3 x 337
1012 = 2 x 2 x 11 x 23
1013 = 1013
1014 = 2 x 3 x 13 x 13
1015 = 5 x 7 x 29
1016 = 2 x 2 x 2 x 127

```



## Lua


```Lua
function factorize( n )
    if n == 1 then return {1} end

    local k = 2
    res = {}
    while n > 1 do
	while n % k == 0 do
	    res[#res+1] = k
 	    n = n / k
	end
 	k = k + 1
    end
    return res
end

for i = 1, 22 do
    io.write( i, ":  " )
    fac = factorize( i )
    io.write( fac[1] )
    for j = 2, #fac do
	io.write( " * ", fac[j] )
    end
    print ""
end
```



## M2000 Interpreter

Decompose function now return array (in number decomposition task return an inventory list).


```M2000 Interpreter

Module Count_in_factors    {
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
			loop
		}
	}
	decompose=lambda IsPrime (n as decimal) -> {
		Factors=(,)
		{
			k=2@
			While frac(n/k)=0
				n/=k
				Append Factors, (k,)
			End While
			if n=1 then exit
			k++
			While frac(n/k)=0
				n/=k
				Append Factors, (k,)
			End While
			if n=1 then exit
			{
				k+=2
				while not isprime(k) {k+=2}
				While frac(n/k)=0
					n/=k : Append Factors, (k,)
				End While
				if n=1 then exit
				loop
			}
		}
		=Factors
	}
	fold=lambda (a, f$)->{
		Push if$(len(f$)=0->f$, f$+"x")+str$(a,"")
	}
	Print "1=1"
	i=1@
	do
		i++
		Print str$(i,"")+"="+Decompose(i)#fold$(fold,"")
	always
}
Count_in_factors

```



## M4


```M4
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$5`'popdef(`$1')$0(`$1',eval($2+$4),$3,$4,`$5')')')')dnl
define(`by',
   `ifelse($1,$2,
      $1,
      `ifelse(eval($1%$2==0),1,
         `$2 x by(eval($1/$2),$2)',
         `by($1,eval($2+1))') ') ')dnl
define(`wby',
   `$1 = ifelse($1,1,
      $1,
      `by($1,2)') ')dnl

for(`y',1,25,1, `wby(y)
')
```


```txt

1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
11 = 11
12 = 2 x 2 x 3
13 = 13
14 = 2 x 7
15 = 3 x 5
16 = 2 x 2 x 2 x 2
17 = 17
18 = 2 x 3 x 3
19 = 19
20 = 2 x 2 x 5
21 = 3 x 7
22 = 2 x 11
23 = 23
24 = 2 x 2 x 2 x 3
25 = 5 x 5

```



## Maple


```maple
factorNum := proc(n)
	local i, j, firstNum;
	if n = 1 then
		printf("%a", 1);
	end if;
	firstNum := true:
	for i in ifactors(n)[2] do
		for j to i[2] do
			if firstNum then
				printf ("%a", i[1]);
				firstNum := false:
			else
				printf(" x %a", i[1]);
			end if;
		end do;
	end do;
	printf("\n");
	return NULL;
end proc:

for i from 1 to 10 do
	printf("%2a: ", i);
	factorNum(i);
end do;
```

```txt

 1: 1
 2: 2
 3: 3
 4: 2 x 2
 5: 5
 6: 2 x 3
 7: 7
 8: 2 x 2 x 2
 9: 3 x 3
10: 2 x 5

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
n = 2;
While[n < 100,
 Print[Row[Riffle[Flatten[Map[Apply[ConstantArray, #] &, FactorInteger[n]]],"*"]]];
 n++]
```



## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method factor(val) public static
  rv = 1
  if val > 1 then do
    rv = ''
    loop n_ = val until n_ = 1
      parse checkFactor(2, n_, rv) n_ rv
      if n_ = 1 then leave n_
      parse checkFactor(3, n_, rv) n_ rv
      if n_ = 1 then leave n_
      loop m_ = 5 to n_ by 2 until n_ = 1
        if m_ // 3 = 0 then iterate m_
        parse checkFactor(m_, n_, rv) n_ rv
        end m_
      end n_
    end
  return rv

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method checkFactor(mult = long, n_ = long, fac) private static binary
  msym = 'x'
  loop while n_ // mult = 0
    fac = fac msym mult
    n_ = n_ % mult
    end
  fac = (fac.strip).strip('l', msym).space
  return n_ fac

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  -- input is a list of pairs of numbers - no checking is done
  if arg = '' then arg = '1 11    89 101    1000 1020    10000 10010'
  loop while arg \= ''
    parse arg lv rv arg
    say
    say '-'.copies(60)
    say lv.right(8) 'to' rv
    say '-'.copies(60)
    loop fv = lv to rv
      fac = factor(fv)
      pv = ''
      if fac.words = 1 & fac \= 1 then pv = '<prime>'
      say fv.right(8) '=' fac pv
      end fv
    end
  return

```

```txt
------------------------------------------------------------
       1 to 11
------------------------------------------------------------
       1 = 1
       2 = 2 <prime>
       3 = 3 <prime>
       4 = 2 x 2
       5 = 5 <prime>
       6 = 2 x 3
       7 = 7 <prime>
       8 = 2 x 2 x 2
       9 = 3 x 3
      10 = 2 x 5
      11 = 11 <prime>

------------------------------------------------------------
      89 to 101
------------------------------------------------------------
      89 = 89 <prime>
      90 = 2 x 3 x 3 x 5
      91 = 7 x 13
      92 = 2 x 2 x 23
      93 = 3 x 31
      94 = 2 x 47
      95 = 5 x 19
      96 = 2 x 2 x 2 x 2 x 2 x 3
      97 = 97 <prime>
      98 = 2 x 7 x 7
      99 = 3 x 3 x 11
     100 = 2 x 2 x 5 x 5
     101 = 101 <prime>

------------------------------------------------------------
    1000 to 1020
------------------------------------------------------------
    1000 = 2 x 2 x 2 x 5 x 5 x 5
    1001 = 7 x 11 x 13
    1002 = 2 x 3 x 167
    1003 = 17 x 59
    1004 = 2 x 2 x 251
    1005 = 3 x 5 x 67
    1006 = 2 x 503
    1007 = 19 x 53
    1008 = 2 x 2 x 2 x 2 x 3 x 3 x 7
    1009 = 1009 <prime>
    1010 = 2 x 5 x 101
    1011 = 3 x 337
    1012 = 2 x 2 x 11 x 23
    1013 = 1013 <prime>
    1014 = 2 x 3 x 13 x 13
    1015 = 5 x 7 x 29
    1016 = 2 x 2 x 2 x 127
    1017 = 3 x 3 x 113
    1018 = 2 x 509
    1019 = 1019 <prime>
    1020 = 2 x 2 x 3 x 5 x 17

------------------------------------------------------------
   10000 to 10010
------------------------------------------------------------
   10000 = 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5
   10001 = 73 x 137
   10002 = 2 x 3 x 1667
   10003 = 7 x 1429
   10004 = 2 x 2 x 41 x 61
   10005 = 3 x 5 x 23 x 29
   10006 = 2 x 5003
   10007 = 10007 <prime>
   10008 = 2 x 2 x 2 x 3 x 3 x 139
   10009 = 10009 <prime>
   10010 = 2 x 5 x 7 x 11 x 13

```



## Nim

```nim
var primes = newSeq[int]()

proc getPrime(idx: int): int =
  if idx >= primes.len:
    if primes.len == 0:
      primes.add 2
      primes.add 3

    var last = primes[primes.high]
    while idx >= primes.len:
      last += 2
      for i, p in primes:
        if p * p > last:
          primes.add last
          break
        if last mod p == 0:
          break

  return primes[idx]

for x in 1 ..< int32.high.int:
  stdout.write x, " = "
  var n = x
  var first = 1

  for i in 0 ..< int32.high:
    let p = getPrime(i)
    while n mod p == 0:
      n = n div p
      if first == 0: stdout.write " x "
      first = 0
      stdout.write p

    if n <= p * p:
      break

  if first > 0: echo n
  elif n > 1:   echo " x ", n
  else:         echo ""
```


```txt
1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
11 = 11
12 = 2 x 2 x 3
13 = 13
14 = 2 x 7
...
```



## Objeck


```objeck

class CountingInFactors {
  function : Main(args : String[]) ~ Nil {
    for(i := 1; i <= 10; i += 1;){
    count := CountInFactors(i);
    ("{$i} = {$count}")->PrintLine();
  };

  for(i := 9991; i <= 10000; i += 1;){
    count := CountInFactors(i);
    ("{$i} = {$count}")->PrintLine();
    };
  }

  function : CountInFactors(n : Int) ~ String {
    if(n = 1) {
      return "1";
    };

    sb := "";
    n := CheckFactor(2, n, sb);
    if(n = 1) {
      return sb;
    };

    n := CheckFactor(3, n, sb);
    if(n = 1) {
      return sb;
    };

    for(i := 5; i <= n; i += 2;) {
      if(i % 3 <> 0) {
        n := CheckFactor(i, n, sb);
        if(n = 1) {
          break;
        };
      };
    };

    return sb;
  }

  function : CheckFactor(mult : Int, n : Int, sb : String) ~ Int {
    while(n % mult = 0 ) {
      if(sb->Size() > 0) {
        sb->Append(" x ");
      };
      sb->Append(mult);
      n /= mult;
    };

    return n;
  }
}

```

Output:

```txt

1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
9991 = 97 x 103
9992 = 2 x 2 x 2 x 1249
9993 = 3 x 3331
9994 = 2 x 19 x 263
9995 = 5 x 1999
9996 = 2 x 2 x 3 x 7 x 7 x 17
9997 = 13 x 769
9998 = 2 x 4999
9999 = 3 x 3 x 11 x 101
10000 = 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5

```



## OCaml


```ocaml
open Big_int

let prime_decomposition x =
  let rec inner c p =
    if lt_big_int p (square_big_int c) then
      [p]
    else if eq_big_int (mod_big_int p c) zero_big_int then
      c :: inner c (div_big_int p c)
    else
      inner (succ_big_int c) p
  in
  inner (succ_big_int (succ_big_int zero_big_int)) x

let () =
  let rec aux v =
    let ps = prime_decomposition v in
    print_string (string_of_big_int v);
    print_string " = ";
    print_endline (String.concat " x " (List.map string_of_big_int ps));
    aux (succ_big_int v)
  in
  aux unit_big_int
```

```txt
$ ocamlopt -o count.opt nums.cmxa count.ml
$ ./count.opt
1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
...
6351 = 3 x 29 x 73
6352 = 2 x 2 x 2 x 2 x 397
6353 = 6353
6354 = 2 x 3 x 3 x 353
6355 = 5 x 31 x 41
6356 = 2 x 2 x 7 x 227
6357 = 3 x 13 x 163
6358 = 2 x 11 x 17 x 17
6359 = 6359
^C
```



## Octave

Octave's factor function returns an array:

```octave
for (n = 1:20)
    printf ("%i: ", n)
    printf ("%i ", factor (n))
    printf ("\n")
endfor
```

```txt
1: 1
2: 2
3: 3
4: 2 2
5: 5
6: 2 3
7: 7
8: 2 2 2
9: 3 3
10: 2 5
11: 11
12: 2 2 3
13: 13
14: 2 7
15: 3 5
16: 2 2 2 2
17: 17
18: 2 3 3
19: 19
20: 2 2 5
```



## PARI/GP


```parigp
fnice(n)={
	my(f,s="",s1);
	if (n < 2, return(n));
	f = factor(n);
	s = Str(s, f[1,1]);
	if (f[1, 2] != 1, s=Str(s, "^", f[1,2]));
	for(i=2,#f[,1],
		s1 = Str(" * ", f[i, 1]);
		if (f[i, 2] != 1, s1 = Str(s1, "^", f[i, 2]));
		s = Str(s, s1)
	);
	s
};
n=0;while(n++, print(fnice(n)))
```



## Pascal

```pascal
program CountInFactors(output);

type
  TdynArray = array of integer;

function factorize(number: integer): TdynArray;
  var
    k: integer;
  begin
    if number = 1 then
    begin
      setlength(factorize, 1);
      factorize[0] := 1
    end
    else
    begin
      k := 2;
      while number > 1 do
      begin
	while number mod k = 0 do
	begin
	  setlength(factorize, length(factorize) + 1);
	  factorize[high(factorize)] := k;
	  number := number div k;
	end;
	inc(k);
      end;
    end
  end;

var
  i, j: integer;
  fac: TdynArray;

begin
  for i := 1 to 22 do
  begin
    write(i, ':  ' );
    fac := factorize(i);
    write(fac[0]);
    for j := 1 to high(fac) do
      write(' * ', fac[j]);
    writeln;
  end;
end.
```

```txt

1:  1
2:  2
3:  3
4:  2 * 2
5:  5
6:  2 * 3
7:  7
8:  2 * 2 * 2
9:  3 * 3
10:  2 * 5
11:  11
12:  2 * 2 * 3
13:  13
14:  2 * 7
15:  3 * 5
16:  2 * 2 * 2 * 2
17:  17
18:  2 * 3 * 3
19:  19
20:  2 * 2 * 5
21:  3 * 7
22:  2 * 11

```



## Perl

Typically one would use a module for this.  Note that these modules all return an empty list for '1'.  This should be efficient to 50+ digits:{{libheader|ntheory}}

```perl
use ntheory qw/factor/;
print "$_ = ", join(" x ", factor($_)), "\n" for 1000000000000000000 .. 1000000000000000010;
```

```txt
1000000000000000000 = 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5
1000000000000000001 = 101 x 9901 x 999999000001
1000000000000000002 = 2 x 3 x 17 x 131 x 1427 x 52445056723
1000000000000000003 = 1000000000000000003
1000000000000000004 = 2 x 2 x 1801 x 246809 x 562425889
1000000000000000005 = 3 x 5 x 44087 x 691381 x 2187161
1000000000000000006 = 2 x 7 x 919 x 77724234416291
1000000000000000007 = 1370531 x 729644203597
1000000000000000008 = 2 x 2 x 2 x 3 x 3 x 97 x 26209 x 32779 x 166667
1000000000000000009 = 1000000000000000009
1000000000000000010 = 2 x 5 x 11 x 103 x 4013 x 21993833369
```


Giving similar output and also good for large inputs:

```perl
use Math::Pari qw/factorint/;
sub factor {
  my ($pn,$pc) = @{Math::Pari::factorint(shift)};
  return map { ($pn->[$_]) x $pc->[$_] } 0 .. $#$pn;
}
print "$_ = ", join(" x ", factor($_)), "\n" for 1000000000000000000 .. 1000000000000000010;
```


or, somewhat slower and limited to native 32-bit or 64-bit integers only:

```perl
use Math::Factor::XS qw/prime_factors/;
print "$_ = ", join(" x ", prime_factors($_)), "\n" for 1000000000000000000 .. 1000000000000000010;
```



If we want to implement it self-contained, we could use the prime decomposition routine from the [[Prime_decomposition]] task.  This is reasonably fast and small, though much slower than the modules and certainly could have more optimization.

```perl
sub factors {
  my($n, $p, @out) = (shift, 3);
  return if $n < 1;
  while (!($n&1)) { $n >>= 1; push @out, 2; }
  while ($n > 1 && $p*$p <= $n) {
    while ( ($n % $p) == 0) {
      $n /= $p;
      push @out, $p;
    }
    $p += 2;
  }
  push @out, $n if $n > 1;
  @out;
}

print "$_ = ", join(" x ", factors($_)), "\n" for 100000000000 .. 100000000100;
```


We could use the second extensible sieve from [[Sieve_of_Eratosthenes#Extensible_sieves]] to only divide by primes.

```perl
tie my @primes, 'Tie::SieveOfEratosthenes';

sub factors {
  my($n, $i, $p, @out) = (shift, 0, 2);
  while ($n >= $p * $p) {
    while ($n % $p == 0) {
      push @out, $p;
      $n /= $p;
    }
    $p = $primes[++$i];
  }
  push @out, $n  if $n > 1 || !@out;
  @out;
}

print "$_ = ", join(" x ", factors($_)), "\n" for 100000000000 .. 100000000010;
```

```txt
100000000000 = 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5
100000000001 = 11 x 11 x 23 x 4093 x 8779
100000000002 = 2 x 3 x 7 x 1543 x 1543067
100000000003 = 100000000003
100000000004 = 2 x 2 x 17573 x 1422637
100000000005 = 3 x 5 x 19 x 1627 x 215659
100000000006 = 2 x 3947 x 12667849
100000000007 = 353 x 283286119
100000000008 = 2 x 2 x 2 x 3 x 3 x 3 x 462962963
100000000009 = 7 x 13 x 53 x 1979 x 10477
100000000010 = 2 x 5 x 101 x 3541 x 27961
```


This next example isn't quite as fast and uses much more memory, but it is self-contained and shows a different approach.  As written it must start at 1, but a range can be handled by using a <code>map</code> to prefill the <tt>p_and_sq</tt> array.

```perl
#!perl -C
use utf8;
use strict;
use warnings;

my $limit = 1000;

print "$_ = $_\n" for 1..3;

my @p_and_sq = ( [2, 4], [3, 9] );

N: for my $n ( 4 .. 1000 ) {
	print $n, " = ";
	for( my $i = 0; $i <= $#p_and_sq; ++$i ) {
		my ($p, $sq) = @{ $p_and_sq[$i] };
		if( $sq > $n ) {
			print $n, "\n";
			push @p_and_sq, [ $n, $n*$n ];
			next N;
		}
		while( 0 == ($n % $p) ) {
			print $p;
			$n /= $p;
			if( $n == 1 ) {
				print "\n";
				next N;
			}
			print " × ";
		}
	}
	die "Ran out of primes?!";
}
```



## Perl 6

```perl6
constant @primes = 2, |(3, 5, 7 ... *).grep: *.is-prime;

multi factors(1) { 1 }
multi factors(Int $remainder is copy) {
  gather for @primes -> $factor {

    # if remainder < factor², we're done
    if $factor * $factor > $remainder {
      take $remainder if $remainder > 1;
      last;
    }

    # How many times can we divide by this prime?
    while $remainder %% $factor {
        take $factor;
        last if ($remainder div= $factor) === 1;
    }
  }
}

say "$_: ", factors($_).join(" × ") for 1..*;
```

The first twenty numbers:

```txt
1: 1
2: 2
3: 3
4: 2 × 2
5: 5
6: 2 × 3
7: 7
8: 2 × 2 × 2
9: 3 × 3
10: 2 × 5
11: 11
12: 2 × 2 × 3
13: 13
14: 2 × 7
15: 3 × 5
16: 2 × 2 × 2 × 2
17: 17
18: 2 × 3 × 3
19: 19
20: 2 × 2 × 5
```

Here we use a <tt>multi</tt> declaration with a constant parameter to match the degenerate case.  We use <tt>copy</tt> parameters when we wish to reuse the formal parameter as a mutable variable within the function. (Parameters default to readonly in Perl 6.) Note the use of <tt>gather</tt>/<tt>take</tt> as the final statement in the function, which is a common Perl 6 idiom to set up a coroutine within a function to return a lazy list on demand.

Note also the '×' above is not ASCII 'x', but U+00D7 MULTIPLICATION SIGN.  Perl 6 does Unicode natively.

Here is a solution inspired from [[Almost_prime#C]].  It doesn't use &is-prime.


```perl6
sub factor($n is copy) {
    $n == 1 ?? 1 !!
    gather {
	$n /= take 2 while $n %% 2;
	$n /= take 3 while $n %% 3;
	loop (my $p = 5; $p*$p <= $n; $p+=2) {
	    $n /= take $p while $n %% $p;
	}
	take $n unless $n == 1;
    }
}

say "$_ == ", join " \x00d7 ", factor $_ for 1 .. 20;
```


Same output as above.


Alternately, use a module:


```perl6
use Prime::Factor;

say "$_ = {(.&prime-factors || 1).join: ' x ' }" for flat 1 .. 10, 10**20 .. 10**20 + 10;
```

```txt
1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
100000000000000000000 = 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5 x 5
100000000000000000001 = 73 x 137 x 1676321 x 5964848081
100000000000000000002 = 2 x 3 x 155977777 x 106852828571
100000000000000000003 = 373 x 155773 x 1721071782307
100000000000000000004 = 2 x 2 x 13 x 1597 x 240841 x 4999900001
100000000000000000005 = 3 x 5 x 7 x 7 x 83 x 1663 x 985694468327
100000000000000000006 = 2 x 31 x 6079 x 265323774602147
100000000000000000007 = 67 x 166909 x 8942221889969
100000000000000000008 = 2 x 2 x 2 x 3 x 3 x 3 x 233 x 1986965506278811
100000000000000000009 = 557 x 72937 x 2461483384901
100000000000000000010 = 2 x 5 x 11 x 909090909090909091
```



## Phix


```Phix
function factorise(atom n)
-- returns a list of all integer factors of n, that when multiplied together equal n
--  (adapted from the standard builtin factors(), which does not return duplicates)
sequence res = {}
integer p = 2,
        step = 1,
        lim = floor(sqrt(n))

    while p<=lim do
        while remainder(n,p)=0 do
            res = append(res,sprintf("%d",p))
            n = n/p
            if n=p then exit end if
            lim = floor(sqrt(n))
        end while
        p += step
        step = 2
    end while
    return join(append(res,sprintf("%d",n))," x ")
end function

for i=1 to 10 do
    printf(1,"%2d: %s\n",{i,factorise(i)})
end for
```

```txt

 1: 1
 2: 2
 3: 3
 4: 2 x 2
 5: 5
 6: 2 x 3
 7: 7
 8: 2 x 2 x 2
 9: 3 x 3
10: 2 x 5

```



## PicoLisp

This is the 'factor' function from [[Prime decomposition#PicoLisp]].

```PicoLisp
(de factor (N)
   (make
      (let (D 2  L (1 2 2 . (4 2 4 2 4 6 2 6 .))  M (sqrt N))
         (while (>= M D)
            (if (=0 (% N D))
               (setq M (sqrt (setq N (/ N (link D)))))
               (inc 'D (pop 'L)) ) )
         (link N) ) ) )

(for N 20
   (prinl N ": " (glue " * " (factor N))) )
```

```txt
1: 1
2: 2
3: 3
4: 2 * 2
5: 5
6: 2 * 3
7: 7
8: 2 * 2 * 2
9: 3 * 3
10: 2 * 5
11: 11
12: 2 * 2 * 3
13: 13
14: 2 * 7
15: 3 * 5
16: 2 * 2 * 2 * 2
17: 17
18: 2 * 3 * 3
19: 19
20: 2 * 2 * 5
```



## PL/I


```PL/I

cnt: procedure options (main);
	declare (i, k, n) fixed binary;
	declare first bit (1) aligned;

   do n = 1 to 40;
      put skip list (n || ' =');
      k = n; first = '1'b;
repeat:
      do i = 2 to k-1;
		if mod(k, i) = 0 then
			do;
				k = k/i;
                                if ^first then put edit (' x ')(A);
                                first = '0'b;
                                put edit (trim(i)) (A);
				go to repeat;
			end;

	end;
        if ^first then put edit (' x ')(A);
        if n = 1 then i = 1;
        put edit (trim(i)) (A);
   end;
end cnt;

```

Results:

```txt
        1 = 1
        2 = 2
        3 = 3
        4 = 2 x 2
        5 = 5
        6 = 2 x 3
        7 = 7
        8 = 2 x 2 x 2
        9 = 3 x 3
       10 = 2 x 5
       11 = 11
       12 = 2 x 2 x 3
       13 = 13
       14 = 2 x 7
       15 = 3 x 5
       16 = 2 x 2 x 2 x 2
       17 = 17
       18 = 2 x 3 x 3
       19 = 19
       20 = 2 x 2 x 5
       21 = 3 x 7
       22 = 2 x 11
       23 = 23
       24 = 2 x 2 x 2 x 3
       25 = 5 x 5
       26 = 2 x 13
       27 = 3 x 3 x 3
       28 = 2 x 2 x 7
       29 = 29
       30 = 2 x 3 x 5
       31 = 31
       32 = 2 x 2 x 2 x 2 x 2
       33 = 3 x 11
       34 = 2 x 17
       35 = 5 x 7
       36 = 2 x 2 x 3 x 3
       37 = 37
       38 = 2 x 19
       39 = 3 x 13
       40 = 2 x 2 x 2 x 5

```



## PowerShell


```PowerShell

function eratosthenes ($n) {
    if($n -ge 1){
        $prime = @(1..($n+1) | foreach{$true})
        $prime[1] = $false
        $m = [Math]::Floor([Math]::Sqrt($n))
        for($i = 2; $i -le $m; $i++) {
            if($prime[$i]) {
                for($j = $i*$i; $j -le $n; $j += $i) {
                    $prime[$j] = $false
                }
            }
        }
        1..$n | where{$prime[$_]}
    } else {
        "$n must be equal or greater than 1"
    }
}
function prime-decomposition ($n) {
    $array = eratosthenes $n
    $prime = @()
    foreach($p in $array) {
        while($n%$p -eq 0) {
            $n /= $p
            $prime += @($p)
        }
    }
    $prime
}
$OFS = " x "
"$(prime-decomposition  2144)"
"$(prime-decomposition  100)"
"$(prime-decomposition  12)"

```

<b>Output:</b>

```txt

2 x 2 x 2 x 2 x 2 x 67
2 x 2 x 5 x 5
2 x 2 x 3

```



## PureBasic


```PureBasic
Procedure Factorize(Number, List Factors())
  Protected I = 3, Max
  ClearList(Factors())
  While Number % 2 = 0
    AddElement(Factors())
    Factors() = 2
    Number / 2
  Wend
  Max = Number
  While I <= Max And Number > 1
    While Number % I = 0
      AddElement(Factors())
      Factors() = I
      Number / I
    Wend
    I + 2
  Wend
EndProcedure

If OpenConsole()
  NewList n()
  For a=1 To 20
    text$=RSet(Str(a),2)+"= "
    Factorize(a,n())
    If ListSize(n())
      ResetList(n())
      While NextElement(n())
        text$ + Str(n())
        If ListSize(n())-ListIndex(n())>1
          text$ + "*"
        EndIf
      Wend
    Else
      text$+Str(a) ; To handle the '1', which is not really a prime...
    EndIf
    PrintN(text$)
  Next a
EndIf
```

```txt
 1= 1
 2= 2
 3= 3
 4= 2*2
 5= 5
 6= 2*3
 7= 7
 8= 2*2*2
 9= 3*3
10= 2*5
11= 11
12= 2*2*3
13= 13
14= 2*7
15= 3*5
16= 2*2*2*2
17= 17
18= 2*3*3
19= 19
20= 2*2*5
```



## Python

This uses the [http://docs.python.org/dev/library/functools.html#functools.lru_cache functools.lru_cache] standard library module to cache intermediate results.

```python
from functools import lru_cache

primes = [2, 3, 5, 7, 11, 13, 17]    # Will be extended

@lru_cache(maxsize=2000)
def pfactor(n):
    if n == 1:
        return [1]
    n2 = n // 2 + 1
    for p in primes:
        if p <= n2:
            d, m = divmod(n, p)
            if m == 0:
                if d > 1:
                    return [p] + pfactor(d)
                else:
                    return [p]
        else:
            if n > primes[-1]:
                primes.append(n)
            return [n]

if __name__ == '__main__':
    mx = 5000
    for n in range(1, mx + 1):
        factors = pfactor(n)
        if n <= 10 or n >= mx - 20:
            print( '%4i %5s %s' % (n,
                                   '' if factors != [n] or n == 1 else 'prime',
                                   'x'.join(str(i) for i in factors)) )
        if n == 11:
            print('...')

    print('\nNumber of primes gathered up to', n, 'is', len(primes))
    print(pfactor.cache_info())
```

```txt
   1       1
   2 prime 2
   3 prime 3
   4       2x2
   5 prime 5
   6       2x3
   7 prime 7
   8       2x2x2
   9       3x3
  10       2x5
...
4980       2x2x3x5x83
4981       17x293
4982       2x47x53
4983       3x11x151
4984       2x2x2x7x89
4985       5x997
4986       2x3x3x277
4987 prime 4987
4988       2x2x29x43
4989       3x1663
4990       2x5x499
4991       7x23x31
4992       2x2x2x2x2x2x2x3x13
4993 prime 4993
4994       2x11x227
4995       3x3x3x5x37
4996       2x2x1249
4997       19x263
4998       2x3x7x7x17
4999 prime 4999
5000       2x2x2x5x5x5x5

Number of primes gathered up to 5000 is 669
CacheInfo(hits=3935, misses=7930, maxsize=2000, currsize=2000)
```



## R


```R

#initially I created a function which returns prime factors then I have created another function counts in the factors and #prints the values.

findfactors <- function(num) {
  x <- c()
  p1<- 2
  p2 <- 3
  everyprime <- num
  while( everyprime != 1 ) {
    while( everyprime%%p1 == 0 ) {
      x <- c(x, p1)
      everyprime <- floor(everyprime/ p1)
    }
    p1 <- p2
    p2 <- p2 + 2
  }
  x
}
count_in_factors=function(x){
  primes=findfactors(x)
  x=c(1)
  for (i in 1:length(primes)) {
    x=paste(primes[i],"x",x)
  }
  return(x)
}
count_in_factors(72)

```


```txt

[1] "3 x 3 x 2 x 2 x 2 x 1"

```



## Racket

See also [[#Scheme]]. This uses Racket&rsquo;s <code>math/number-theory</code> package


```racket
#lang typed/racket

(require math/number-theory)

(define (factorise-as-primes [n : Natural])
  (if
   (= n 1)
   '(1)
   (let ((F (factorize n)))
     (append*
      (for/list : (Listof (Listof Natural))
        ((f (in-list F)))
        (make-list (second f) (first f)))))))

(define (factor-count [start-inc : Natural] [end-inc : Natural])
  (for ((i : Natural (in-range start-inc (add1 end-inc))))
    (define f (string-join (map number->string (factorise-as-primes i)) " × "))
    (printf "~a:\t~a~%" i f)))

(factor-count 1 22)
(factor-count 2140 2150)
; tb
```


```txt
1:	1
2:	2
3:	3
4:	2 × 2
5:	5
6:	2 × 3
7:	7
8:	2 × 2 × 2
9:	3 × 3
10:	2 × 5
11:	11
12:	2 × 2 × 3
13:	13
14:	2 × 7
15:	3 × 5
16:	2 × 2 × 2 × 2
17:	17
18:	2 × 3 × 3
19:	19
20:	2 × 2 × 5
21:	3 × 7
22:	2 × 11
2140:	2 × 2 × 5 × 107
2141:	2141
2142:	2 × 3 × 3 × 7 × 17
2143:	2143
2144:	2 × 2 × 2 × 2 × 2 × 67
2145:	3 × 5 × 11 × 13
2146:	2 × 29 × 37
2147:	19 × 113
2148:	2 × 2 × 3 × 179
2149:	7 × 307
2150:	2 × 5 × 5 × 43
```



## REXX


### simple approach

As per the task's requirements, the prime factors of   '''1'''   (unity) will be listed as   '''1''',

even though, strictly speaking, it should be   '''null'''.         The same applies to   '''0'''.

Programming note:   if the   '''high'''   argument is negative, its positive value is used and no displaying of the

prime factors are listed, but the number of primes found is always shown.   The showing of the count of

primes was included to help verify the factoring (of composites).

```rexx
/*REXX program lists the prime factors of a specified integer  (or a range of integers).*/
@.=left('', 8);  @.0="{unity} ";  @.1='[prime] ' /*some tags  and  handy-dandy literals.*/
parse arg LO HI @ .                              /*get optional arguments from the C.L. */
if LO=='' | LO==","  then do; LO=1; HI=40;  end  /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI= LO                 /* "      "         "   "   "     "    */
if  @==''            then  @= 'x'                /* "      "         "   "   "     "    */
if length(@)\==1  then @= x2c(@)                 /*Not length 1?  Then use hexadecimal. */
tell= (HI>0)                                     /*if  HIGH  is positive, then show #'s.*/
HI= abs(HI)                                      /*use the absolute value for  HIGH.    */
w= length(HI)                                    /*get maximum width for pretty output. */
numeric digits max(9, w + 1)                     /*maybe bump the precision of numbers. */
#= 0                                             /*the number of primes found (so far). */
     do n=abs(LO)  to HI;          f= factr(n)   /*process a single number  or  a range.*/
     p= words( translate(f, ,@) )  -  (n==1)     /*P:  is the number of prime factors.  */
     if p==1  then #= # + 1                      /*bump the primes counter (exclude N=1)*/
     if tell  then say right(n, w)  '='  @.p  f  /*display if a prime, plus its factors.*/
     end   /*n*/
say
say right(#, w)          ' primes found.'        /*display the number of primes found.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
factr: procedure expose @; parse arg z 1 n,$;  if z<2  then return z   /*is Z too small?*/
           do  while z//2==0;   $= $||@||2;   z= z%2;    end  /*maybe add factor of   2 */
           do  while z//3==0;   $= $||@||3;   z= z%3;    end  /*  "    "     "    "   3 */
           do  while z//5==0;   $= $||@||5;   z= z%5;    end  /*  "    "     "    "   5 */
           do  while z//7==0;   $= $||@||7;   z= z%7;    end  /*  "    "     "    "   7 */

         do j=11  by 6  while j<=z               /*insure that  J  isn't divisible by 3.*/
         parse var j  ''  -1  _                  /*get the last decimal digit of  J.    */
         if _\==5  then do while  z//j==0;  $=$||@||j;  z= z%j;  end   /*maybe reduce Z.*/
         if _ ==3  then iterate                  /*Next # ÷ by 5?  Skip.     ___        */
         if j*j>n  then leave                    /*are we higher than the   √ N   ?     */
         y= j + 2                                /*obtain the next odd divisor.         */
                        do while  z//y==0;  $=$||@||y;  z= z%y;   end  /*maybe reduce Z.*/
         end   /*j*/
       if z==1  then return substr($,       1+length(@) )  /*Is residual=1?  Don't add 1*/
                     return substr($||@||z, 1+length(@) )  /*elide superfluous header.  */
```

```txt

 1 = {unity}  1
 2 = [prime]  2
 3 = [prime]  3
 4 =          2x2
 5 = [prime]  5
 6 =          2x3
 7 = [prime]  7
 8 =          2x2x2
 9 =          3x3
10 =          2x5
11 = [prime]  11
12 =          2x2x3
13 = [prime]  13
14 =          2x7
15 =          3x5
16 =          2x2x2x2
17 = [prime]  17
18 =          2x3x3
19 = [prime]  19
20 =          2x2x5
21 =          3x7
22 =          2x11
23 = [prime]  23
24 =          2x2x2x3
25 =          5x5
26 =          2x13
27 =          3x3x3
28 =          2x2x7
29 = [prime]  29
30 =          2x3x5
31 = [prime]  31
32 =          2x2x2x2x2
33 =          3x11
34 =          2x17
35 =          5x7
36 =          2x2x3x3
37 = [prime]  37
38 =          2x19
39 =          3x13
40 =          2x2x2x5

12  primes found.

```

{{out|output|text=  when the following input was used:     <tt> 1   12   207820 </tt>

```txt

 1 = {unity}  1
 2 = [prime]  2
 3 = [prime]  3
 4 =          2 x 2
 5 = [prime]  5
 6 =          2 x 3
 7 = [prime]  7
 8 =          2 x 2 x 2
 9 =          3 x 3
10 =          2 x 5
11 = [prime]  11
12 =          2 x 2 x 3

 5  primes found.

```

{{out|output|text=  when the following input was used:     <tt> 1   -10000 </tt>

```txt

  1229  primes found.

```

{{out|output|text=  when the following input was used:     <tt> 1   -100000 </tt>

```txt

  9592  primes found.

```



### using integer SQRT

This REXX version computes the   ''integer square root''   of the integer being factor   (to limit the range of factors),

this makes this version about   '''50%'''   faster than the 1<sup>st</sup> REXX version.

Also, the number of early testing of prime factors was expanded.

Note that the   '''integer square root'''   section of code doesn't use any floating point numbers, just integers.

```rexx
/*REXX program lists the prime factors of a specified integer  (or a range of integers).*/
@.=left('', 8);  @.0="{unity} ";  @.1='[prime] ' /*some tags  and  handy-dandy literals.*/
parse arg LO HI @ .                              /*get optional arguments from the C.L. */
if LO=='' | LO==","  then do; LO=1; HI=40;  end  /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI= LO                 /* "      "         "   "   "     "    */
if  @==''            then  @= 'x'                /* "      "         "   "   "     "    */
if length(@)\==1  then @= x2c(@)                 /*Not length 1?  Then use hexadecimal. */
tell= (HI>0)                                     /*if  HIGH  is positive, then show #'s.*/
HI= abs(HI)                                      /*use the absolute value for  HIGH.    */
w= length(HI)                                    /*get maximum width for pretty output. */
numeric digits max(9, w + 1)                     /*maybe bump the precision of numbers. */
#= 0                                             /*the number of primes found (so far). */
     do n=abs(LO)  to HI;          f= factr(n)   /*process a single number  or  a range.*/
     p= words( translate(f, ,@) )  -  (n==1)     /*P:  is the number of prime factors.  */
     if p==1  then #= # + 1                      /*bump the primes counter (exclude N=1)*/
     if tell  then say right(n, w)  '='  @.p  f  /*display if a prime, plus its factors.*/
     end   /*n*/
say
say right(#, w)          ' primes found.'        /*display the number of primes found.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
factr: procedure expose @; parse arg z 1 n,$;  if z<2  then return z   /*is Z too small?*/
           do  while z// 2==0;  $= $||@||2 ;   z= z%2 ;   end /*maybe add factor of   2 */
           do  while z// 3==0;  $= $||@||3 ;   z= z%3 ;   end /*  "    "     "    "   3 */
           do  while z// 5==0;  $= $||@||5 ;   z= z%5 ;   end /*  "    "     "    "   5 */
           do  while z// 7==0;  $= $||@||7 ;   z= z%7 ;   end /*  "    "     "    "   7 */
           do  while z//11==0;  $= $||@||11;   z= z%11;   end /*  "    "     "    "  11 */
           do  while z//13==0;  $= $||@||13;   z= z%13;   end /*  "    "     "    "  13 */
           do  while z//17==0;  $= $||@||17;   z= z%17;   end /*  "    "     "    "  17 */
           do  while z//19==0;  $= $||@||19;   z= z%19;   end /*  "    "     "    "  19 */
           do  while z//23==0;  $= $||@||23;   z= z%23;   end /*  "    "     "    "  23 */
           do  while z//29==0;  $= $||@||29;   z= z%29;   end /*  "    "     "    "  29 */
           do  while z//31==0;  $= $||@||31;   z= z%31;   end /*  "    "     "    "  31 */
           do  while z//37==0;  $= $||@||37;   z= z%37;   end /*  "    "     "    "  37 */
       if z>40 then do;    t= z;    q= 1;    r= 0;              do while q<=t;    q= q * 4
                                                                end   /*while*/
                      do while q>1; q=q%4;  _=t-r-q;  r=r%2; if _>=0  then do;  t=_; r=r+q
                                                                           end
                      end   /*while*/                    /* [↑]  find integer SQRT(z).  */
                                                         /*R:  is the integer SQRT of Z.*/
                      do j=41  by 6  to  r  while j<=z   /*insure J isn't divisible by 3*/
                      parse var j  ''  -1  _             /*get last decimal digit of  J.*/
                      if _\==5  then do  while z//j==0;      $=$||@||j;     z= z%j;    end
                      if _ ==3  then iterate             /*Next number  ÷  by 5 ?  Skip.*/
                      y= j + 2                           /*use the next (odd) divisor.  */
                                     do  while z//y==0;      $=$||@||y;     z= z%y;    end
                      end   /*j*/                        /* [↑]  reduce  Z  by  Y ?     */
                    end     /*if z>40*/

       if z==1  then return substr($,       1+length(@) )  /*Is residual=1?  Don't add 1*/
                     return substr($||@||z, 1+length(@) )  /*elide superfluous header.  */
```

```txt

 1 = {unity}  1
 2 = [prime]  2
 3 = [prime]  3
 4 =          2∙2
 5 = [prime]  5
 6 =          2∙3
 7 = [prime]  7
 8 =          2∙2∙2
 9 =          3∙3
10 =          2∙5
11 = [prime]  11
12 =          2∙2∙3
13 = [prime]  13
14 =          2∙7
15 =          3∙5
16 =          2∙2∙2∙2
17 = [prime]  17
18 =          2∙3∙3
19 = [prime]  19
20 =          2∙2∙5
21 =          3∙7
22 =          2∙11
23 = [prime]  23
24 =          2∙2∙2∙3
25 =          5∙5
26 =          2∙13
27 =          3∙3∙3
28 =          2∙2∙7
29 = [prime]  29
30 =          2∙3∙5
31 = [prime]  31
32 =          2∙2∙2∙2∙2
33 =          3∙11
34 =          2∙17
35 =          5∙7
36 =          2∙2∙3∙3
37 = [prime]  37
38 =          2∙19
39 =          3∙13
40 =          2∙2∙2∙5

12  primes found.

```



## Ring


```ring

for i = 1 to 20
    see "" + i + " = " + factors(i) + nl
next

func factors n
     f = ""
     if n = 1 return "1" ok
     p = 2
     while p <= n
           if (n % p) = 0
              f += string(p) + " x "
              n = n/p
           else p += 1 ok
     end
     return left(f, len(f) - 3)

```

Output:

```txt

1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
11 = 11
12 = 2 x 2 x 3
13 = 13
14 = 2 x 7
15 = 3 x 5
16 = 2 x 2 x 2 x 2
17 = 17
18 = 2 x 3 x 3
19 = 19
20 = 2 x 2 x 5

```



## Ruby

Starting with Ruby 1.9, 'prime' is part of the standard library and provides Integer#prime_division.

```ruby
require 'optparse'
require 'prime'

maximum = 10
OptionParser.new do |o|
  o.banner = "Usage: #{File.basename $0} [-m MAXIMUM]"
  o.on("-m MAXIMUM", Integer,
       "Count up to MAXIMUM [#{maximum}]") { |m| maximum = m }
  o.parse! rescue ($stderr.puts $!, o; exit 1)
  ($stderr.puts o; exit 1) unless ARGV.size == 0
end

# 1 has no prime factors
puts "1 is 1" unless maximum < 1

2.upto(maximum) do |i|
  # i is 504 => i.prime_division is [[2, 3], [3, 2], [7, 1]]
  f = i.prime_division.map! do |factor, exponent|
    # convert [2, 3] to "2 x 2 x 2"
    ([factor] * exponent).join " x "
  end.join " x "
  puts "#{i} is #{f}"
end
```

```txt
$ ruby prime-count.rb -h
Usage: prime-count.rb [-m MAXIMUM]
    -m MAXIMUM                       Count up to MAXIMUM [10]
$ ruby prime-count.rb -m 10000 | sed -e '11,9990d'
1 is 1
2 is 2
3 is 3
4 is 2 x 2
5 is 5
6 is 2 x 3
7 is 7
8 is 2 x 2 x 2
9 is 3 x 3
10 is 2 x 5
9991 is 97 x 103
9992 is 2 x 2 x 2 x 1249
9993 is 3 x 3331
9994 is 2 x 19 x 263
9995 is 5 x 1999
9996 is 2 x 2 x 3 x 7 x 7 x 17
9997 is 13 x 769
9998 is 2 x 4999
9999 is 3 x 3 x 11 x 101
10000 is 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5
```



## Run BASIC


```runbasic
for i = 1000 to 1016
  print i;" = "; factorial$(i)
next
wait
function factorial$(num)
 if num = 1 then factorial$ = "1"
 fct = 2
 while fct <= num
 if (num mod fct) = 0 then
   factorial$ = factorial$ ; x$ ; fct
   x$  = " x "
   num = num / fct
  else
   fct = fct + 1
 end if
 wend
end function
```

```txt
1000 = 2 x 2 x 2 x 5 x 5 x 5
1001 = 7 x 11 x 13
1002 = 2 x 3 x 167
1003 = 17 x 59
1004 = 2 x 2 x 251
1005 = 3 x 5 x 67
1006 = 2 x 503
1007 = 19 x 53
1008 = 2 x 2 x 2 x 2 x 3 x 3 x 7
1009 = 1009
1010 = 2 x 5 x 101
1011 = 3 x 337
1012 = 2 x 2 x 11 x 23
1013 = 1013
1014 = 2 x 3 x 13 x 13
1015 = 5 x 7 x 29
1016 = 2 x 2 x 2 x 127
```



## Rust

You can run and experiment with this code at https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=b66c14d944ff0472d2460796513929e2

```rust
use std::env;

fn main() {
    let args: Vec<_> = env::args().collect();
    let n = if args.len() > 1 {
        args[1].parse().expect("Not a valid number to count to")
    }
    else {
        20
    };
    count_in_factors_to(n);
}

fn count_in_factors_to(n: u64) {
    println!("1");
    let mut primes = vec![];
    for i in 2..=n {
        let fs = factors(&primes, i);
        if fs.len() <= 1 {
            primes.push(i);
            println!("{}", i);
        }
        else {
            println!("{} = {}", i, fs.iter().map(|f| f.to_string()).collect::<Vec<String>>().join(" x "));
        }
    }
}

fn factors(primes: &[u64], mut n: u64) -> Vec<u64> {
    let mut result = Vec::new();
    for p in primes {
        while n % p == 0 {
            result.push(*p);
            n /= p;
        }
        if n == 1 {
            return result;
        }
    }
    vec![n]
}
```

```txt
1
2
3
4 = 2 x 2
5
6 = 2 x 3
7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
11
12 = 2 x 2 x 3
13
14 = 2 x 7
15 = 3 x 5
16 = 2 x 2 x 2 x 2
17
18 = 2 x 3 x 3
19
20 = 2 x 2 x 5

```



## Scala


```scala
def primeFactors( n:Int ) = {

  def primeStream(s: Stream[Int]): Stream[Int] = {
    s.head #:: primeStream(s.tail filter { _ % s.head != 0 })
  }

  val primes = primeStream(Stream.from(2))

  def factors( n:Int ) : List[Int] = primes.takeWhile( _ <= n ).find( n % _ == 0 ) match {
    case None => Nil
    case Some(p) => p :: factors( n/p )
  }

  if( n == 1 ) List(1) else factors(n)
}

// A little test...
{
  val nums = (1 to 12).toList :+ 2144 :+ 6358
  nums.foreach( n => println( "%6d : %s".format( n, primeFactors(n).mkString(" * ") ) ) )
}

```

```txt
     1 : 1
     2 : 2
     3 : 3
     4 : 2 * 2
     5 : 5
     6 : 2 * 3
     7 : 7
     8 : 2 * 2 * 2
     9 : 3 * 3
    10 : 2 * 5
    11 : 11
    12 : 2 * 2 * 3
  2144 : 2 * 2 * 2 * 2 * 2 * 67
  6358 : 2 * 11 * 17 * 17
```



## Scheme


```lisp
(define (factors n)
  (let facs ((l '()) (d 2) (x n))
    (cond ((= x 1) (if (null? l) '(1) l))
	  ((< x (* d d)) (cons x l))
	  (else (if (= 0 (modulo x d))
		  (facs (cons d l) d (/ x d))
		  (facs l (+ 1 d) x))))))

(define (show l)
  (display (car l))
  (if (not (null? (cdr l)))
    (begin
      (display " × ")
      (show (cdr l)))
    (display "\n")))

(do ((i 1 (+ i 1))) (#f)
  (display i)
  (display " = ")
  (show (reverse (factors i))))
```

```txt
1 = 1
2 = 2
3 = 3
4 = 2 × 2
5 = 5
6 = 2 × 3
7 = 7
8 = 2 × 2 × 2
9 = 3 × 3
10 = 2 × 5
11 = 11
12 = 2 × 2 × 3
...
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: writePrimeFactors (in var integer: number) is func
  local
    var boolean: laterElement is FALSE;
    var integer: checker is 2;
  begin
    while checker * checker <= number do
      if number rem checker = 0 then
        if laterElement then
          write(" * ");
        end if;
        laterElement := TRUE;
        write(checker);
        number := number div checker;
      else
        incr(checker);
      end if;
    end while;
    if number <> 1 then
      if laterElement then
        write(" * ");
      end if;
      laterElement := TRUE;
      write(number);
    end if;
  end func;

const proc: main is func
  local
    var integer: number is 0;
  begin
    writeln("1: 1");
    for number range 2 to 2147483647 do
      write(number <& ": ");
      writePrimeFactors(number);
      writeln;
    end for;
  end func;
```

```txt

1: 1
2: 2
3: 3
4: 2 * 2
5: 5
6: 2 * 3
7: 7
8: 2 * 2 * 2
9: 3 * 3
10: 2 * 5
11: 11
12: 2 * 2 * 3
13: 13
14: 2 * 7
15: 3 * 5
. . .

```



## Sidef


```ruby
class Counter {
    method factors(n, p=2) {
        var a = gather {
            while (n >= p*p) {
                while (p `divides` n) {
                    take(p)
                    n //= p
                }
                p = self.next_prime(p)
            }
        }
        (n > 1 || a.is_empty) ? (a << n) : a
    }
 
    method is_prime(n) {
        self.factors(n).len == 1
    }
 
    method next_prime(p) {
        do {
            p == 2 ? (p = 3) : (p+=2)
        } while (!self.is_prime(p))
        return p
    }
}
 
for i in (1..100) {
    say "#{i} = #{Counter().factors(i).join(' × ')}"
}
```



## Tcl

This factorization code is based on the same engine that is used in the [[Parallel calculations#Tcl|parallel computation task]].

```tcl
package require Tcl 8.5

namespace eval prime {
    variable primes [list 2 3 5 7 11]
    proc restart {} {
	variable index -1
	variable primes
	variable current [lindex $primes end]
    }

    proc get_next_prime {} {
	variable primes
	variable index
	if {$index < [llength $primes]-1} {
	    return [lindex $primes [incr index]]
	}
	variable current
	while 1 {
	    incr current 2
	    set p 1
	    foreach prime $primes {
		if {$current % $prime} {} else {
		    set p 0
		    break
		}
	    }
	    if {$p} {
		return [lindex [lappend primes $current] [incr index]]
	    }
	}
    }

    proc factors {num} {
	restart
	set factors [dict create]
	for {set i [get_next_prime]} {$i <= $num} {} {
	    if {$num % $i == 0} {
		dict incr factors $i
		set num [expr {$num / $i}]
		continue
	    } elseif {$i*$i > $num} {
		dict incr factors $num
		break
	    } else {
		set i [get_next_prime]
	    }
	}
	return $factors
    }

    # Produce the factors in rendered form
    proc factors.rendered {num} {
	set factorDict [factors $num]
	if {[dict size $factorDict] == 0} {
	    return 1
	}
	dict for {factor times} $factorDict {
	    lappend v {*}[lrepeat $times $factor]
	}
	return [join $v "*"]
    }
}
```

Demonstration code:

```tcl
set max 20
for {set i 1} {$i <= $max} {incr i} {
    puts [format "%*d = %s" [string length $max] $i [prime::factors.rendered $i]]
}
```



## VBScript

Made minor modifications on the code I posted under Prime Decomposition.

```vb
Function CountFactors(n)
	If n = 1 Then
		CountFactors = 1
	Else
		arrP = Split(ListPrimes(n)," ")
		Set arrList = CreateObject("System.Collections.ArrayList")
		divnum = n
		Do Until divnum = 1
			'The -1 is to account for the null element of arrP
			For i = 0 To UBound(arrP)-1
				If divnum = 1 Then
					Exit For
				ElseIf divnum Mod arrP(i) = 0 Then
					divnum = divnum/arrP(i)
					arrList.Add arrP(i)
				End If
			Next
		Loop
		arrList.Sort
		For i = 0 To arrList.Count - 1
			If i = arrList.Count - 1 Then
				CountFactors = CountFactors & arrList(i)
			Else
				CountFactors = CountFactors & arrList(i) & " * "
			End If
		Next
	End If
End Function

Function IsPrime(n)
	If n = 2 Then
		IsPrime = True
	ElseIf n <= 1 Or n Mod 2 = 0 Then
		IsPrime = False
	Else
		IsPrime = True
		For i = 3 To Int(Sqr(n)) Step 2
			If n Mod i = 0 Then
				IsPrime = False
				Exit For
			End If
		Next
	End If
End Function

Function ListPrimes(n)
	ListPrimes = ""
	For i = 1 To n
		If IsPrime(i) Then
			ListPrimes = ListPrimes & i & " "
		End If
	Next
End Function

'Testing the fucntions.
WScript.StdOut.Write "2 = " & CountFactors(2)
WScript.StdOut.WriteLine
WScript.StdOut.Write "2144 = " & CountFactors(2144)
WScript.StdOut.WriteLine
```


```txt

2 = 2
2144 = 2 * 2 * 2 * 2 * 2 * 67

```




## Visual Basic .NET


```vbnet
Module CountingInFactors

    Sub Main()
        For i As Integer = 1 To 10
            Console.WriteLine("{0} = {1}", i, CountingInFactors(i))
        Next

        For i As Integer = 9991 To 10000
            Console.WriteLine("{0} = {1}", i, CountingInFactors(i))
        Next
    End Sub

    Private Function CountingInFactors(ByVal n As Integer) As String
        If n = 1 Then Return "1"

        Dim sb As New Text.StringBuilder()

        CheckFactor(2, n, sb)
        If n = 1 Then Return sb.ToString()

        CheckFactor(3, n, sb)
        If n = 1 Then Return sb.ToString()

        For i As Integer = 5 To n Step 2
            If i Mod 3 = 0 Then Continue For

            CheckFactor(i, n, sb)
            If n = 1 Then Exit For
        Next

        Return sb.ToString()
    End Function

    Private Sub CheckFactor(ByVal mult As Integer, ByRef n As Integer, ByRef sb As Text.StringBuilder)
        Do While n Mod mult = 0
            If sb.Length > 0 Then sb.Append(" x ")
            sb.Append(mult)
            n = n / mult
        Loop
    End Sub

End Module
```

```txt

1 = 1
2 = 2
3 = 3
4 = 2 x 2
5 = 5
6 = 2 x 3
7 = 7
8 = 2 x 2 x 2
9 = 3 x 3
10 = 2 x 5
9991 = 97 x 103
9992 = 2 x 2 x 2 x 1249
9993 = 3 x 3331
9994 = 2 x 19 x 263
9995 = 5 x 1999
9996 = 2 x 2 x 3 x 7 x 7 x 17
9997 = 13 x 769
9998 = 2 x 4999
9999 = 3 x 3 x 11 x 101
10000 = 2 x 2 x 2 x 2 x 5 x 5 x 5 x 5

```



## XPL0


```XPL0
include c:\cxpl\codes;
int     N0, N, F;
[N0:= 1;
repeat  IntOut(0, N0);  Text(0, " = ");
        F:= 2;  N:= N0;
        repeat  if rem(N/F) = 0 then
                        [if N # N0 then Text(0, " * ");
                        IntOut(0, F);
                        N:= N/F;
                        ]
                else F:= F+1;
        until F>N;
        if N0=1 then IntOut(0, 1);      \1 = 1
        CrLf(0);
        N0:= N0+1;
until KeyHit;
]
```


Example output:

```txt

1 = 1
2 = 2
3 = 3
4 = 2 * 2
5 = 5
6 = 2 * 3
7 = 7
8 = 2 * 2 * 2
9 = 3 * 3
10 = 2 * 5
11 = 11
12 = 2 * 2 * 3
13 = 13
14 = 2 * 7
15 = 3 * 5
16 = 2 * 2 * 2 * 2
17 = 17
18 = 2 * 3 * 3
. . .
57086 = 2 * 17 * 23 * 73
57087 = 3 * 3 * 6343
57088 = 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 223
57089 = 57089
57090 = 2 * 3 * 5 * 11 * 173
57091 = 37 * 1543
57092 = 2 * 2 * 7 * 2039
57093 = 3 * 19031
57094 = 2 * 28547
57095 = 5 * 19 * 601
57096 = 2 * 2 * 2 * 3 * 3 * 13 * 61
57097 = 57097

```



## zkl


```zkl
foreach n in ([1..*]){ println(n,": ",primeFactors(n).concat("\U2715;")) }
```

Using the fixed size integer (64 bit) solution from [[Prime decomposition#zkl]]

```zkl
fcn primeFactors(n){  // Return a list of factors of n
   acc:=fcn(n,k,acc,maxD){  // k is 2,3,5,7,9,... not optimum
      if(n==1 or k>maxD) acc.close();
      else{
	 q,r:=n.divr(k);   // divr-->(quotient,remainder)
	 if(r==0) return(self.fcn(q,k,acc.write(k),q.toFloat().sqrt()));
	 return(self.fcn(n,k+1+k.isOdd,acc,maxD))
      }
   }(n,2,Sink(List),n.toFloat().sqrt());
   m:=acc.reduce('*,1);      // mulitply factors
   if(n!=m) acc.append(n/m); // opps, missed last factor
   else acc;
}
```

```txt

1:
2: 2
3: 3
4: 2✕2
5: 5
6: 2✕3
...
591885: 3✕3✕5✕7✕1879
591886: 2✕295943
591887: 591887
591888: 2✕2✕2✕2✕3✕11✕19✕59
...

```



## ZX Spectrum Basic

```zxbasic
10 FOR i=1 TO 20
20 PRINT i;" = ";
30 IF i=1 THEN PRINT 1: GO TO 90
40 LET p=2: LET n=i: LET f$=""
50 IF p>n THEN GO TO 80
60 IF NOT FN m(n,p) THEN LET f$=f$+STR$ p+" x ": LET n=INT (n/p): GO TO 50
70 LET p=p+1: GO TO 50
80 PRINT f$( TO LEN f$-3)
90 NEXT i
100 STOP
110 DEF FN m(a,b)=a-INT (a/b)*b
```


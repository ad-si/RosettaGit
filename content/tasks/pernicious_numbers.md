+++
title = "Pernicious numbers"
description = ""
date = 2019-09-10T18:35:58Z
aliases = []
[extra]
id = 17363
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "befunge",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "eiffel",
  "elixir",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "maple",
  "mathematica",
  "nim",
  "panda",
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
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "seed7",
  "sidef",
  "symsyn",
  "tcl",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "wortel",
  "zkl",
]
+++

A   [[wp:Pernicious number|pernicious number]]   is a positive integer whose   [[population count]]   is a prime.

The population count is the number of   ''ones''   in the binary representation of a non-negative integer.


;Example
'''22'''   (which is   '''10110'''   in binary)   has a population count of   '''3''',   which is prime, and therefore   '''22'''   is a pernicious number.


## Task

* display the first   '''25'''   pernicious numbers   (in decimal).
* display all pernicious numbers between   '''888,888,877'''   and   '''888,888,888'''   (inclusive).
* display each list of integers on one line   (which may or may not include a title).


## See also

* Sequence   [[oeis:A052294|A052294 pernicious numbers]] on The On-Line Encyclopedia of Integer Sequences.
* Rosetta Code entry   [[Population_count|population count, evil numbers, odious numbers]].





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360)
with 2 ASSIST macros (XDECO,XPRNT).

```360asm
*        Pernicious numbers        04/05/2016
PERNIC   CSECT
         USING  PERNIC,R13         base register and savearea pointer
SAVEAREA B      STM-SAVEAREA(R15)
         DC     17F'0'
STM      STM    R14,R12,12(R13)    save registers
         ST     R13,4(R15)         link backward SA
         ST     R15,8(R13)         link forward SA
         LR     R13,R15            establish addressability
         SR     R7,R7              n=0
         MVC    PG,=CL80' '        clear buffer
         LA     R10,PG             pgi
         LA     R6,1               i=1
LOOPI1   C      R7,=F'25'          do i=1 while(n<25)
         BNL    ELOOPI1
         LR     R1,R6              i
         BAL    R14,POPCOUNT
         LR     R1,R0              popcount(i)
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(popcount(i))=1
         BNE    NOTPRIM1
         XDECO  R6,XDEC            edit i
         MVC    0(3,R10),XDEC+9    output i format I3
         LA     R10,3(R10)         pgi=pgi+3
         LA     R7,1(R7)           n=n+1
NOTPRIM1 LA     R6,1(R6)           i=i+1
         B      LOOPI1
ELOOPI1  XPRNT  PG,80              print buffer
         MVC    PG,=CL80' '        clear buffer
         LA     R10,PG             pgi
         L      R6,=F'888888877'   i=888888877
LOOPI2   C      R6,=F'888888888'   do i to 888888888
         BH     ELOOPI2
         LR     R1,R6              i
         BAL    R14,POPCOUNT
         LR     R1,R0              popcount(i)
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(popcount(i))=1
         BNE    NOTPRIM2
         XDECO  R6,XDEC            edit i
         MVC    0(10,R10),XDEC+2   output i format I10
         LA     R10,10(R10)        pgi=pgi+10
NOTPRIM2 LA     R6,1(R6)           i=i+1
         B      LOOPI2
ELOOPI2  XPRNT  PG,80              print buffer
         L      R13,4(0,R13)       restore savearea pointer
         LM     R14,R12,12(R13)    restore registers
         XR     R15,R15            return code = 0
         BR     R14 -------------- end main
POPCOUNT CNOP   0,4 -------------- popcount(xx) [R8,R11]
         ST     R14,POPCOUSA       save return address
         ST     R1,XX              store argument
         SR     R11,R11            rr=0
         SR     R8,R8              ii=0
LOOPII   C      R8,=F'31'          do ii=0 to 31
         BH     ELOOPII
         L      R1,XX              xx
         LR     R2,R8              ii
         BAL    R14,BTEST
         C      R0,=F'1'           if btest(xx,ii)=1
         BNE    NOTBTEST
         LA     R11,1(R11)         rr=rr+1
NOTBTEST LA     R8,1(R8)           ii=ii+1
         B      LOOPII
ELOOPII  LR     R0,R11             return(rr)
         L      R14,POPCOUSA
         BR     R14 -------------- end popcount
ISPRIME  CNOP   0,4 -------------- isprime(number) [R9]
         ST     R14,ISPRIMSA       save return address
         ST     R1,NUMBER          store argument
         C      R1,=F'2'           if number=2
         BNE    ELSE1
         MVC    ISPRIMEX,=F'1'     isprimex=1
         B      ELOOPJJ
ELSE1    L      R1,NUMBER
         C      R1,=F'2'           if number<2
         BL     EVEN
         L      R4,NUMBER
         SRDA   R4,32
         D      R4,=F'2'           mod(number,2)
         C      R4,=F'0'           if mod(number,2)=0
         BNE    ELSE2
EVEN     MVC    ISPRIMEX,=F'0'     isprimex=0
         B      ELOOPJJ
ELSE2    MVC    ISPRIMEX,=F'1'     isprimex=1
         LA     R9,3               jj=3
LOOPJJ   LR     R5,R9              jj
         MR     R4,R9              jj*jj
         C      R5,NUMBER          do jj=3 by 1 while jj*jj<=number
         BH     ELOOPJJ
         L      R4,NUMBER
         SRDA   R4,32
         DR     R4,R9              mod(number,jj)
         LTR    R4,R4              if mod(number,jj)=0
         BNZ    ITERJJ
         MVC    ISPRIMEX,=F'0'     isprimex=0
         L      R0,ISPRIMEX        return(isprimex)
         B      ISPRIMRT
ITERJJ   LA     R9,1(R9)           jj=jj+1
         B      LOOPJJ
ELOOPJJ  L      R0,ISPRIMEX        return(isprimex)
ISPRIMRT L      R14,ISPRIMSA
         BR     R14 -------------- end isprime
BTEST    CNOP   0,4 -------------- btest(word,n) [R0:R3]
         LA     R0,1               ok=1; return(1) if word(n)='1'b
         LR     R3,R2              i=n
LOOPB    LTR    R3,R3              if i=0
         BZ     ELOOPB
         SRL    R1,1               Shift Right Logical
         BCTR   R3,0               i=i-1
         B      LOOPB
ELOOPB   STC    R1,BTESTX          x=word
         TM     BTESTX,B'00000001' if bit(word,n)='1'b
         BO     BTESTRET
         LA     R0,0               ok=0; return(0) if word(n)='0'b
BTESTRET BR     R14 -------------- end btest
XX       DS     F                  paramter of popcount
NUMBER   DS     F                  paramter of isprime
ISPRIMEX DS     F                  return value of isprime
BTESTX   DS     X                  byte to see in btest
POPCOUSA DS     A                  return address of popcount
ISPRIMSA DS     A                  return address of isprime
PG       DS     CL80               buffer
XDEC     DS     CL12               edit zone
         YREGS
         END    PERNIC
```

```txt

  3  5  6  7  9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
 888888877 888888878 888888880 888888883 888888885 888888886

```



## Ada


Uses package Population_Count from [[Population count#Ada]].


```Ada
with Ada.Text_IO, Population_Count; use Population_Count;

procedure Pernicious is

   Prime: array(0 .. 64) of Boolean;
     -- we are using 64-bit numbers, so the population count is between 0 and 64
   X: Num; use type Num;
   Cnt: Positive;
begin
   -- initialize array Prime; Prime(I) must be true if and only if I is a prime
   Prime := (0 => False, 1 => False, others => True);
   for I in 2 .. 8 loop
      if Prime(I) then
	 Cnt := I + I;
	 while Cnt <= 64 loop
	    Prime(Cnt) := False;
	    Cnt := Cnt + I;
	 end loop;
      end if;
   end loop;

   -- print first 25 pernicious numbers
   X := 1;
   for I in 1 .. 25 loop
      while not Prime(Pop_Count(X)) loop
	 X := X + 1;
      end loop;
      Ada.Text_IO.Put(Num'Image(X));
      X := X + 1;
   end loop;
   Ada.Text_IO.New_Line;

   -- print pernicious numbers between  888_888_877 and 888_888_888 (inclusive)
   for Y in Num(888_888_877) .. 888_888_888 loop
      if Prime(Pop_Count(Y)) then
	 Ada.Text_IO.Put(Num'Image(Y));
      end if;
   end loop;
   Ada.Text_IO.New_Line;
end;
```



```txt
 3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
 888888877 888888878 888888880 888888883 888888885 888888886
```


A small modification allows to count all the pernicious numbers between 1 and 2**32 in about 32 seconds:


```Ada
   Counter: Natural;
begin
   -- initialize array Prime; Prime(I) must be true if and only if I is a prime
   ...

   Counter := 0;
   -- count p. numbers below 2**32
   for Y in Num(2) .. 2**32 loop
      if Prime(Pop_Count(Y)) then
	 Counter := Counter + 1;
      end if;
   end loop;
   Ada.Text_IO.Put_Line(Natural'Image(Counter));
end Count_Pernicious;
```


```txt
> time ./count_pernicious
 1421120880

real    0m33.375s
user    0m33.372s
sys     0m0.000s
```



## ALGOL 68


```algol68
# calculate various pernicious numbers                          #

# returns the population (number of bits on) of the non-negative integer n    #
PROC population = ( INT n )INT:
     BEGIN
        INT    number := n;
        INT    result := 0;
        WHILE number > 0 DO
            IF ODD number THEN result +:= 1 FI;
            number OVERAB 2
        OD;
        result
     END # population # ;

# as we are dealing with 32 bit numbers, the maximum possible population is 32 #
# so we only need a table of whether the integers 0 : 32 are prime or not      #
# we use the sieve of Eratosthenes...                                          #
INT max number = 32;
[ 0 : max number ]BOOL is prime;
is prime[ 0 ] := FALSE;
is prime[ 1 ] := FALSE;
FOR i FROM 2 TO max number DO is prime[ i ] := TRUE OD;
FOR i FROM 2 TO ENTIER sqrt( max number ) DO
    IF is prime[ i ] THEN FOR p FROM i * i BY i TO max number DO is prime[ p ] := FALSE OD FI
OD;

# returns TRUE if n is pernicious, FALSE otherwise                             #
PROC is pernicious = ( INT n )BOOL: is prime[ population( n ) ];

# find the first 25 pernicious numbers, 0 and 1 are not pernicious             #
INT pernicious count := 0;
FOR i FROM 2 WHILE pernicious count < 25 DO
    IF is pernicious( i ) THEN
        # found a pernicious number #
        print( ( whole( i, 0 ), " " ) );
        pernicious count +:= 1
    FI
OD;
print( ( newline ) );

# find the pernicious numbers between 888 888 877 and 888 888 888              #
FOR i FROM 888 888 877 TO 888 888 888 DO
    IF is pernicious( i ) THEN
        print( ( whole( i, 0 ), " " ) )
    FI
OD;
print( ( newline ) )

```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## AutoHotkey

```AutoHotkey
c := 0
while c < 25
	if IsPern(A_Index)
		Out1 .= A_Index " ", c++
Loop, 12
	if IsPern(n := 888888876 + A_Index)
		Out2 .= n " "
MsgBox, % Out1 "`n" Out2

IsPern(x) {	;https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation
	static p := {2:1, 3:1, 5:1, 7:1, 11:1, 13:1, 17:1, 19:1, 23:1, 29:1, 31:1, 37:1, 41:1, 43:1, 47:1, 53:1, 59:1, 61:1}
	x -= (x >> 1) & 0x5555555555555555
	, x := (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333)
	, x := (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0f
	return p[(x * 0x0101010101010101) >> 56]
}
```

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```


## AWK


```AWK

# syntax: GAWK -f PERNICIOUS_NUMBERS.AWK
BEGIN {
    pernicious(25)
    pernicious(888888877,888888888)
    exit(0)
}
function pernicious(x,y,  count,n) {
    if (y == "") { # print first X pernicious numbers
      while (count < x) {
        if (is_prime(pop_count(++n)) == 1) {
          printf("%d ",n)
          count++
        }
      }
    }
    else { # print pernicious numbers in X-Y range
      for (n=x; n<=y; n++) {
        if (is_prime(pop_count(n)) == 1) {
          printf("%d ",n)
        }
      }
    }
    print("")
}
function dec2bin(n,  str) {
    while (n) {
      if (n%2 == 0) {
        str = "0" str
      }
      else {
        str = "1" str
      }
      n = int(n/2)
    }
    if (str == "") {
      str = "0"
    }
    return(str)
}
function is_prime(x,  i) {
    if (x <= 1) {
      return(0)
    }
    for (i=2; i<=int(sqrt(x)); i++) {
      if (x % i == 0) {
        return(0)
      }
    }
    return(1)
}
function pop_count(n) {
    n = dec2bin(n)
    return gsub(/1/,"&",n)
}

```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## Befunge

Based more or less on the '''[[Pernicious_numbers#C|C]]''' implementation, although we don't bother supporting ''n'' = 0, so we can use a smaller prime bit set that fits inside a signed 32 bit int (most Befunge implementations wouldn't support anything higher).

Also note that the extra spaces in the output are just to ensure it's readable on buggy interpreters that don't include a space after numeric output. They can easily be removed by replacing the comma on line 3 with a dollar.


```befunge
55*00p1>:"ZOA>/"***7-*>\:2>/\v
>8**`!#^_$@\<(^v^)>/#2^#\<2  2
^+**"X^yYo":+1<_:.48*,00v|: <%
v".D}Tx"$,+55_^#!p00:-1g<v  |<
> * + : * * + ^^ ! % 2 $ <^ <^
```


```txt
3  5  6  7  9  10  11  12  13  14  17  18  19  20  21  22  24  25  26  28  31  33  34  35  36
888888877  888888878  888888880  888888883  888888885  888888886
```



## C


```c
#include <stdio.h>

typedef unsigned uint;
uint is_pern(uint n)
{
        uint c = 2693408940u; // int with all prime-th bits set
        while (n) c >>= 1, n &= (n - 1); // take out lowerest set bit one by one
        return c & 1;
}

int main(void)
{
        uint i, c;
        for (i = c = 0; c < 25; i++)
                if (is_pern(i))
                        printf("%u ", i), ++c;
        putchar('\n');

        for (i = 888888877u; i <= 888888888u; i++)
                if (is_pern(i))
                        printf("%u ", i);
        putchar('\n');

        return 0;
}
```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## C++


```cpp

#include <iostream>
#include <algorithm>
#include <bitset>

using namespace std;

class pernNumber
{
public:
    void displayFirst( unsigned cnt )
    {
	unsigned pn = 3;
	while( cnt )
	{
	    if( isPernNumber( pn ) )
	    {
		cout << pn << " "; cnt--;
	    }
	    pn++;
	}
    }
    void displayFromTo( unsigned a, unsigned b )
    {
	for( unsigned p = a; p <= b; p++ )
	    if( isPernNumber( p ) )
		cout << p << " ";
    }

private:
    bool isPernNumber( unsigned p )
    {
	string bin = bitset<64>( p ).to_string();
	unsigned c = count( bin.begin(), bin.end(), '1' );
	return isPrime( c );
    }
    bool isPrime( unsigned p )
    {
	if( p == 2 ) return true;
	if( p < 2 || !( p % 2 ) ) return false;
	for( unsigned x = 3; ( x * x ) <= p; x += 2 )
	    if( !( p % x ) ) return false;
	return true;
    }
};
int main( int argc, char* argv[] )
{
    pernNumber p;
    p.displayFirst( 25 ); cout << endl;
    p.displayFromTo( 888888877, 888888888 ); cout << endl;
    return 0;
}

```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```


## C#

```c#
using System;
using System.Linq;

namespace PerniciousNumbers
{
    class Program
    {
        public static int PopulationCount(long n)
        {
            int cnt = 0;
            do
            {
                if ((n & 1) != 0)
                {
                    cnt++;
                }
            } while ((n >>= 1) > 0);

            return cnt;
        }

         public static bool isPrime(int x)
        {
            if (x <= 2 || (x & 1) == 0)
            {
                return x == 2;
            }

            var limit = Math.Sqrt(x);
            for (int i = 3; i <= limit; i += 2)
            {
                if (x % i == 0)
                {
                    return false;
                }
            }

            return true;
        }

        private static IEnumerable<int> Pernicious(int start, int count, int take)
        {
            return Enumerable.Range(start, count).Where(n => isPrime(PopulationCount(n))).Take(take);
        }

        static void Main(string[] args)
        {
            foreach (var n in Pernicious(0, int.MaxValue, 25))
            {
                Console.Write("{0} ", n);
            }

            Console.WriteLine();

            foreach (var n in Pernicious(888888877, 11, 11))
            {
                Console.Write("{0} ", n);
            }

            Console.ReadKey();
        }
    }
}
```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## Clojure


```clojure
(defn counting-numbers
 ([] (counting-numbers 1))
 ([n] (lazy-seq (cons n (counting-numbers (inc n))))))
(defn divisors [n] (filter #(zero? (mod n %)) (range 1 (inc n))))
(defn prime? [n] (= (divisors n) (list 1 n)))
(defn pernicious? [n]
  (prime? (count (filter #(= % \1) (Integer/toString n 2)))))
(println (take 25 (filter pernicious? (counting-numbers))))
(println (filter pernicious? (range 888888877  888888889)))
```

```txt
(3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36)
(888888877 888888878 888888880 888888883 888888885 888888886)
```



## Common Lisp

Using <code>primep</code> from [[Primality_by_trial_division#Common_Lisp|Primality by trial division]] task.


```lisp
(format T "~{~a ~}~%"
        (loop for n = 1 then (1+ n)
              when (primep (logcount n))
                collect n into numbers
              when (= (length numbers) 25)
                return numbers))

(format T "~{~a ~}~%"
        (loop for n from 888888877 to 888888888
              when (primep (logcount n))
                collect n))
```


```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```



## D


```d
void main() {
    import std.stdio, std.algorithm, std.range, core.bitop;

    immutable pernicious = (in uint n) => (2 ^^ n.popcnt) & 0xA08A28AC;
    uint.max.iota.filter!pernicious.take(25).writeln;
    iota(888_888_877, 888_888_889).filter!pernicious.writeln;
}
```

```txt
[3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36]
[888888877, 888888878, 888888880, 888888883, 888888885, 888888886]
```

Where <code>0xA08A28AC == 0b_1010_0000__1000_1010__0010_1000__1010_1100</code>, that is a bit set equivalent to the prime numbers [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31] of the range (0, 31].

This high-level code is fast enough to allow to count all the
1_421_120_880 Pernicious numbers in the unsigned 32 bit range in less than 48 seconds with this line:

```d
uint.max.iota.filter!pernicious.walkLength.writeln;
```



## EchoLisp


```scheme

(lib 'sequences)

(define (pernicious? n) (prime? (bit-count n)))

(define pernicious (filter pernicious? [1 .. ]))
(take pernicious 25)
    → (3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36)

(take (filter pernicious? [888888877 .. 888888889]) #:all)
    → (888888877 888888878 888888880 888888883 888888885 888888886)

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Test of is_pernicious_number.
		local
			test: LINKED_LIST [INTEGER]
			i: INTEGER
		do
			create test.make
			from
				i := 1
			until
				test.count = 25
			loop
				if is_pernicious_number (i) then
					test.extend (i)
				end
				i := i + 1
			end
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
			io.new_line
			across
				888888877 |..| 888888888 as c
			loop
				if is_pernicious_number (c.item) then
					io.put_string (c.item.out + " ")
				end
			end
		end

	is_pernicious_number (n: INTEGER): BOOLEAN
			-- Is 'n' a pernicious_number?
		require
			positiv_input: n > 0
		do
			Result := is_prime (count_population (n))
		end

feature{NONE}

	count_population (n: INTEGER): INTEGER
			-- Population count of 'n'.
		require
			positiv_input: n > 0
		local
			j: INTEGER
			math: DOUBLE_MATH
		do
			create math
			j := math.log_2 (n).ceiling + 1
			across
				0 |..| j as c
			loop
				if n.bit_test (c.item) then
					Result := Result + 1
				end
			end
		end

	is_prime (n: INTEGER): BOOLEAN
			--Is 'n' a prime number?
		require
			positiv_input: n > 0
		local
			i: INTEGER
			max: REAL_64
			math: DOUBLE_MATH
		do
			create math
			if n = 2 then
				Result := True
			elseif n <= 1 or n \\ 2 = 0 then
				Result := False
			else
				Result := True
				max := math.sqrt (n)
				from
					i := 3
				until
					i > max
				loop
					if n \\ i = 0 then
						Result := False
					end
					i := i + 2
				end
			end
		end

end

```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## Elixir


```Elixir

defmodule SieveofEratosthenes do
  def init(lim) do
    find_primes(2,lim,(2..lim))
  end

  def find_primes(count,lim,nums) when (count * count) > lim do
    nums
  end

  def find_primes(count,lim,nums) when (count * count) <= lim do
    e = Enum.reject(nums,&(rem(&1,count) == 0 and &1 > count))
    find_primes(count+1,lim,e)
  end
end

defmodule PerniciousNumbers do
  def take(n) do
    primes = SieveofEratosthenes.init(100)
    Stream.iterate(1,&(&1+1))
      |> Stream.filter(&(pernicious?(&1,primes)))
      |> Enum.take(n)
      |> IO.inspect
  end

  def between(a..b) do
    primes = SieveofEratosthenes.init(100)
    a..b
      |> Stream.filter(&(pernicious?(&1,primes)))
      |> Enum.to_list
      |> IO.inspect
  end

  def ones(num) do
     num
      |> Integer.to_string(2)
      |> String.codepoints
      |> Enum.count(fn n -> n == "1" end)
  end

   def pernicious?(n,primes), do: Enum.member?(primes,ones(n))
end

```



```Elixir

PerniciousNumbers.take(25)
PerniciousNumbers.between(888_888_877..888_888_888)

```


[3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36]

[888888877, 888888878, 888888880, 888888883, 888888885, 888888886]

=={{header|F#|F sharp}}==

```fsharp
open System

//Taken from https://gist.github.com/rmunn/bc49d32a586cdfa5bcab1c3e7b45d7ac
let bitcount (n : int) =
    let count2 = n - ((n >>> 1) &&& 0x55555555)
    let count4 = (count2 &&& 0x33333333) + ((count2 >>> 2) &&& 0x33333333)
    let count8 = (count4 + (count4 >>> 4)) &&& 0x0f0f0f0f
    (count8 * 0x01010101) >>> 24

//Modified from other examples to actually state the 1 is not prime
let isPrime n =
    if n < 2 then
        false
    else
        let sqrtn n = int <| sqrt (float n)
        seq { 2 .. sqrtn n } |> Seq.exists(fun i -> n % i = 0) |> not

[<EntryPoint>]
let main _ =
    [1 .. 100] |> Seq.filter (bitcount >> isPrime) |> Seq.take 25 |> Seq.toList |> printfn "%A"
    [888888877 .. 888888888] |> Seq.filter (bitcount >> isPrime) |> Seq.toList |> printfn "%A"
    0 // return an integer exit code
```

```txt
[3; 5; 6; 7; 9; 10; 11; 12; 13; 14; 17; 18; 19; 20; 21; 22; 24; 25; 26; 28; 31; 33; 34; 35; 36]
[888888877; 888888878; 888888880; 888888883; 888888885; 888888886]
```



## Factor


```factor

USING: lists lists.lazy math.bits math.primes math.ranges ;

: pernicious? ( n -- ? ) make-bits [ t = ] count prime? ;

0 lfrom [ pernicious? ] lfilter 25 swap ltake list>array . ! print first 25 pernicious numbers
888,888,877 888,888,888 [a,b] [ pernicious? ] filter .     ! print pernicious numbers in range

```

```txt

{ 3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36 }
{ 888888877 888888878 888888880 888888883 888888885 888888886 }

```



## Fortran

```fortran
program pernicious
  implicit none

  integer :: i, n

  i = 1
  n = 0
  do
    if(isprime(popcnt(i))) then
      write(*, "(i0, 1x)", advance = "no") i
      n = n + 1
      if(n == 25) exit
    end if
    i = i + 1
  end do

  write(*,*)
  do i = 888888877, 888888888
    if(isprime(popcnt(i))) write(*, "(i0, 1x)", advance = "no") i
  end do

contains

function popcnt(x)
  integer :: popcnt
  integer, intent(in) :: x
  integer :: i

  popcnt = 0
  do i = 0, 31
    if(btest(x, i)) popcnt = popcnt + 1
  end do

end function

function isprime(number)
  logical :: isprime
  integer, intent(in) :: number
  integer :: i

  if(number == 2) then
    isprime = .true.
  else if(number < 2 .or. mod(number,2) == 0) then
    isprime = .false.
  else
    isprime = .true.
    do i = 3, int(sqrt(real(number))), 2
      if(mod(number,i) == 0) then
        isprime = .false.
        exit
      end if
    end do
  end if
end function
end program
```

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```



## FreeBASIC

```freebasic

' FreeBASIC v1.05.0 win64

Function SumBinaryDigits(number As Integer) As Integer
  If number < 0 Then number = -number ' convert negative numbers to positive
  Var sum = 0
  While number > 0
    sum += number Mod 2
    number \= 2
  Wend
  Return sum
End Function

Function IsPrime(number As Integer) As Boolean
  If number <= 1 Then
    Return false
  ElseIf number <= 3 Then
    Return true
  ElseIf number Mod 2 = 0 OrElse number Mod 3 = 0 Then
    Return false
  End If
  Var i = 5
  While i * i <= number
    If number Mod i = 0 OrElse number Mod (i + 2) = 0 Then
      Return false
    End If
    i += 6
  Wend
  Return True
End Function

Function IsPernicious(number As Integer) As Boolean
  Dim popCount As Integer = SumBinaryDigits(number)
  Return IsPrime(popCount)
End Function

Dim As Integer n = 1, count = 0
Print "The following are the first 25 pernicious numbers :"
Print

Do
  If IsPernicious(n) Then
    Print Using "###"; n;
    count += 1
  End If
  n += 1
Loop Until count = 25

Print : Print
Print "The pernicious numbers between 888,888,877 and 888,888,888 inclusive are :"
Print
For n = 888888877 To 888888888
  If IsPernicious(n) Then Print Using "##########"; n;
Next
Print : Print
Print "Press any key to exit the program"
Sleep
End

```


```txt

The following are the first 25 pernicious numbers :

  3  5  6  7  9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36

The pernicious numbers between 888,888,877 and 888,888,888 inclusive are :

 888888877 888888878 888888880 888888883 888888885 888888886

```



## Go


```go
package main

import "fmt"

func pernicious(w uint32) bool {
    const (
        ff    = 1<<32 - 1
        mask1 = ff / 3
        mask3 = ff / 5
        maskf = ff / 17
        maskp = ff / 255
    )
    w -= w >> 1 & mask1
    w = w&mask3 + w>>2&mask3
    w = (w + w>>4) & maskf
    return 0xa08a28ac>>(w*maskp>>24)&1 != 0
}

func main() {
    for i, n := 0, uint32(1); i < 25; n++ {
        if pernicious(n) {
            fmt.Printf("%d ", n)
            i++
        }
    }
    fmt.Println()
    for n := uint32(888888877); n <= 888888888; n++ {
        if pernicious(n) {
            fmt.Printf("%d ", n)
        }
    }
    fmt.Println()
}
```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## Groovy


```Groovy

class example{
static void main(String[] args){
def n=0;
def counter=0;
while(counter<25){
if(print(n)){
counter++;}
n=n+1;
}
println();
def x=888888877;
while(x<888888889){
print(x);
x++;}
}
static def print(def a){
def primes=[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47];
def c=Integer.toBinaryString(a);
String d=c;
def e=0;
for(i in d){if(i=='1'){e++;}}
if(e in primes){printf(a+" ");return 1;}
}
}

```

```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## Haskell


```Haskell
module Pernicious
   where

isPernicious :: Integer -> Bool
isPernicious num = isPrime $ toInteger $ length $ filter ( == 1 ) $ toBinary num

isPrime :: Integer -> Bool
isPrime number = divisors number == [1, number]
   where
      divisors :: Integer -> [Integer]
      divisors number = [ m | m <- [1 .. number] , number `mod` m == 0 ]

toBinary :: Integer -> [Integer]
toBinary num = reverse $ map ( `mod` 2 ) ( takeWhile ( /= 0 ) $ iterate ( `div` 2 ) num )

solution1 = take 25 $ filter isPernicious [1 ..]
solution2 = filter isPernicious [888888877 .. 888888888]
```

```txt
[3,5,6,7,9,10,11,12,13,14,17,18,19,20,21,22,24,25,26,28,31,33,34,35,36]
[888888877,888888878,888888880,888888883,888888885,888888886]
```


Or, in a point-free and applicative style, using unfoldr for the population count:


```Haskell
import Data.Numbers.Primes (isPrime)
import Data.List (unfoldr)
import Data.Tuple (swap)
import Data.Bool (bool)

isPernicious :: Int -> Bool
isPernicious = isPrime . popCount

popCount :: Int -> Int
popCount =
  sum . unfoldr ((flip bool Nothing . Just . swap . flip quotRem 2) <*> (0 ==))

main :: IO ()
main =
  mapM_
    print
    [ take 25 $ filter isPernicious [1 ..]
    , filter isPernicious [888888877 .. 888888888]
    ]
```

```txt
[3,5,6,7,9,10,11,12,13,14,17,18,19,20,21,22,24,25,26,28,31,33,34,35,36]
[888888877,888888878,888888880,888888883,888888885,888888886]
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
link "factors"

procedure main(A)
    every writes((pernicious(seq())\25||" ") | "\n")
    every writes((pernicious(888888877 to 888888888)||" ") | "\n")
end

procedure pernicious(n)
    return (isprime(c1bits(n)),n)
end

procedure c1bits(n)
    c := 0
    while n > 0 do c +:= 1(n%2, n/:=2)
    return c
end
```


```txt

->pn
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
->

```



## J


Implementation:


```J
ispernicious=: 1 p: +/"1@#:
```


Task  (thru taken from the [[Loops/Downward_for#J|Loops/Downward for]] task).:


```J
   25{.I.ispernicious i.100
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36

   thru=: <. + i.@(+*)@-~
   888888877 + I. ispernicious 888888877 thru 888888888
888888877 888888878 888888880 888888883 888888885 888888886
```



## Java


```java
public class Pernicious{
    //very simple isPrime since x will be <= Long.SIZE
    public static boolean isPrime(int x){
        if(x < 2) return false;
        for(int i = 2; i < x; i++){
            if(x % i == 0) return false;
        }
        return true;
    }

    public static int popCount(long x){
        return Long.bitCount(x);
    }

    public static void main(String[] args){
        for(long i = 1, n = 0; n < 25; i++){
            if(isPrime(popCount(i))){
                System.out.print(i + " ");
                n++;
            }
        }

        System.out.println();

        for(long i = 888888877; i <= 888888888; i++){
            if(isPrime(popCount(i))) System.out.print(i + " ");
        }
    }
}
```

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```


## jq

The most interesting detail in the following is perhaps the use of ''recurse/1'' to define the helper function ''bin'', which generates the binary bits.

```jq
# is_prime is designed to work with jq 1.4
def is_prime:
  if . == 2 then true
  else 2 < . and . % 2 == 1 and
       . as $in
       | (($in + 1) | sqrt) as $m
       | (((($m - 1) / 2) | floor) + 1) as $max
       | reduce range(1; $max) as $i
           (true; if . then ($in % ((2 * $i) + 1)) > 0 else false end)
  end;

def popcount:
  def bin: recurse( if . == 0 then empty else ./2 | floor end ) % 2;
  [bin] | add;

def is_pernicious: popcount | is_prime;

# Emit a stream of "count" pernicious numbers greater than
# or equal to m:
def pernicious(m; count):
   if count > 0 then
     if m | is_pernicious then m, pernicious(m+1; count -1)
     else pernicious(m+1; count)
     end
   else empty
   end;

def task:
  # display the first 25 pernicious numbers:
  [ pernicious(1;25) ],

  # display all pernicious numbers between
  #     888,888,877 and 888,888,888 (inclusive).
  [ range(888888877; 888888889) | select( is_pernicious ) ]
;

task
```

 [3,5,6,7,9,10,11,12,13,14,17,18,19,20,21,22,24,25,26,28,31,33,34,35,36]
 [888888877,888888878,888888880,888888883,888888885,888888886]


## Julia

```julia
using Primes

ispernicious(n::Integer) = isprime(count_ones(n))
nextpernicious(n::Integer) = begin n += 1; while !ispernicious(n) n += 1 end; return n end
function perniciouses(n::Int)
    rst = Vector{Int}(n)
    rst[1] = 3
    for i in 2:n
        rst[i] = nextpernicious(rst[i-1])
    end
    return rst
end
perniciouses(a::Integer, b::Integer) = filter(ispernicious, a:b)

println("First 25 pernicious numbers: ", join(perniciouses(25), ", "))
println("Perniciouses in [888888877, 888888888]: ", join(perniciouses(888888877, 888888888), ", "))
```


```txt
First 25 pernicious numbers: 3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36
Perniciouses in [888888877, 888888888]: 888888877, 888888878, 888888880, 888888883, 888888885, 888888886
```



## Kotlin


```scala
//  version 1.0.5-2

fun isPrime(n: Int): Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun getPopulationCount(n: Int): Int {
    if (n <= 0) return 0
    var nn = n
    var sum = 0
    while (nn > 0) {
        sum += nn % 2
        nn /= 2
    }
    return sum
}

fun isPernicious(n: Int): Boolean = isPrime(getPopulationCount(n))

fun main(args: Array<String>) {
    var n = 1
    var count = 0
    println("The first 25 pernicious numbers are:\n")
    do {
        if (isPernicious(n)) {
           print("$n ")
           count++
        }
        n++
    }
    while (count < 25)
    println("\n")
    println("The pernicious numbers between 888,888,877 and 888,888,888 inclusive are:\n")
    for (i in 888888877..888888888) {
        if (isPernicious(i)) print("$i ")
    }
}
```


```txt

The first 25 pernicious numbers are:

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36

The pernicious numbers between 888,888,877 and 888,888,888 inclusive are:

888888877 888888878 888888880 888888883 888888885 888888886

```



## Lua


```Lua
-- Test primality by trial division
function isPrime (x)
    if x < 2 then return false end
    if x < 4 then return true end
    if x % 2 == 0 then return false end
    for d = 3, math.sqrt(x), 2 do
        if x % d == 0 then return false end
    end
    return true
end

-- Take decimal number, return binary string
function dec2bin (n)
    local bin, bit = ""
    while n > 0 do
        bit = n % 2
        n = math.floor(n / 2)
        bin = bit .. bin
    end
    return bin
end

-- Take decimal number, return population count as number
function popCount (n)
    local bin, count = dec2bin(n), 0
    for pos = 1, bin:len() do
        if bin:sub(pos, pos) == "1" then count = count + 1 end
    end
    return count
end

-- Print pernicious numbers in range if two arguments provided, or
function pernicious (x, y) -- the first 'x' if only one argument.
    if y then
        for n = x, y do
            if isPrime(popCount(n)) then io.write(n .. " ") end
        end
    else
        local n, count = 0, 0
        while count < x do
            if isPrime(popCount(n)) then
                io.write(n .. " ")
                count = count + 1
            end
            n = n + 1
        end
    end
    print()
end

-- Main procedure
pernicious(25)
pernicious(888888877, 888888888)
```

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```



## Maple


```Maple
ispernicious := proc(n::posint)
  return evalb(isprime(rhs(Statistics:-Tally(StringTools:-Explode(convert(convert(n, binary), string)))[-1])));
end proc;

print_pernicious := proc(n::posint)
local k, count, list_num;
count := 0;
list_num := [];
for k while count < n do
    if ispernicious(k) then
       count := count + 1;
       list_num := [op(list_num), k];
    end if;
end do;
return list_num;
end proc:

range_pernicious := proc(n::posint, m::posint)
local k, list_num;
list_num := [];
for k from n to m do
    if ispernicious(k) then
       list_num := [op(list_num), k];
    end if;
end do;
return list_num;
end proc:
```

```txt
[3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36]
[888888877, 888888878, 888888880, 888888883, 888888885, 888888886]

```



## Mathematica


```Mathematica
popcount[n_Integer] := IntegerDigits[n, 2] // Total
perniciousQ[n_Integer] := popcount[n] // PrimeQ
perniciouscount = 0;
perniciouslist = {};
i = 0;
While[perniciouscount < 25,
 If[perniciousQ[i], AppendTo[perniciouslist, i]; perniciouscount++];
 i++]
Print["first 25 pernicious numbers"]
perniciouslist
(*******)
perniciouslist2 = {};
Do[
 If[perniciousQ[i], AppendTo[perniciouslist2, i]]
 , {i, 888888877, 888888888}]
Print["Pernicious numbers between 888,888,877 and 888,888,888 (inclusive)"]
perniciouslist2
```

```txt
first 25 pernicious numbers
{3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36}
Pernicious numbers between 888,888,877 and 888,888,888 (inclusive)
{888888877, 888888878, 888888880, 888888883, 888888885, 888888886}

```


### Alternate Code

test function

```Mathematica
perniciousQ[n_Integer] := PrimeQ@Total@IntegerDigits[n, 2]
```

First 25 pernicious numbers

```Mathematica
n = 0; NestWhile[Flatten@{#, If[perniciousQ[++n], n, {}]} &, {}, Length@# < 25 &]
```

Pernicious numbers betweeen 888888877 and 888888888 inclusive

```Mathematica
Cases[Range[888888877, 888888888], _?(perniciousQ@# &)]
```


=={{header|Modula-2}}==

```modula2
MODULE Pernicious;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE IsPrime(x : LONGINT) : BOOLEAN;
VAR i : LONGINT;
BEGIN
    IF x<2 THEN RETURN FALSE END;
    FOR i:=2 TO x-1 DO
        IF x MOD i = 0 THEN RETURN FALSE END
    END;
    RETURN TRUE
END IsPrime;

PROCEDURE BitCount(x : LONGINT) : LONGINT;
VAR count : LONGINT;
BEGIN
    count := 0;
    WHILE x>0 DO
        x := x BAND (x-1);
        INC(count)
    END;
    RETURN count
END BitCount;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i,n : LONGINT;
BEGIN
    i := 1;
    n := 0;
    WHILE n<25 DO
        IF IsPrime(BitCount(i)) THEN
            FormatString("%l ", buf, i);
            WriteString(buf);
            INC(n)
        END;
        INC(i)
    END;
    WriteLn;

    FOR i:=888888877 TO 888888888 DO
        IF IsPrime(BitCount(i)) THEN
            FormatString("%l ", buf, i);
            WriteString(buf)
        END;
    END;

    ReadChar
END Pernicious.
```



## Nim

```nim
import strutils

proc count(s: string, sub: char): int =
  var i = 0
  while true:
    i = s.find(sub, i)
    if i < 0:
      break
    inc i
    inc result

proc popcount(n): int = n.toBin(64).count('1')

const primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61}

var p = newSeq[int]()
var i = 0
while p.len < 25:
  if popcount(i) in primes: p.add i
  inc i

echo p

p = @[]
i = 888_888_877
while i <= 888_888_888:
  if popcount(i) in primes: p.add i
  inc i

echo p
```

```txt
@[3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36]
@[888888877, 888888878, 888888880, 888888883, 888888885, 888888886]
```



## PARI/GP


```parigp
pern(n)=isprime(hammingweight(n))
select(pern, [1..36])
select(pern,[888888877..888888888])
```

```txt
%1 = [3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36]
%2 = [888888877, 888888878, 888888880, 888888883, 888888885, 888888886]
```



## Panda


```panda
fun prime(a) type integer->integer
  a where count{{a.factor}}==2
fun pernisc(a) type integer->integer
  a where sum{{a.radix:2 .char.integer}}.integer.prime

1..36.pernisc
888888877..888888888.pernisc
```


```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```



## Pascal

Inspired by [[Pernicious numbers#Ada|Ada]], using array of primes to simply add.An if-then takes to long.

Added easy counting of pernicious numbers for full Bit ranges like 32-Bit

```pascal
program pernicious;
{$IFDEF FPC}
   {$OPTIMIZATION ON,Regvar,ASMCSE,CSE,PEEPHOLE}// 3x speed up
{$ENDIF}
uses
  sysutils;//only used for time

type
  tbArr    = array[0..64] of byte;
{
  PrimeTil64 : array[0..64] of byte =
  (0,0,2,3,0,5,0, 7,0,0,0,11,0,13,0,0,0,17,0,19,0,0,0,23,0,0,0,0,0,29,0,
    31,0,0,0,0,0,37,0,0,0,41,0,43,0,0,0,47,0, 0,0,0,0,53,0,0,0,0,0,59,0,
    61,0,0,0);
}
const
  PrimeTil64 : tbArr =
  (0,0,1,1,0,1,0, 1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,
     1,0,0,0,0,0, 1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,
     1,0,0,0);

function n_beyond_k(n,k: NativeInt):Uint64;
var
  i : NativeInt;
Begin
  result := 1;
  IF 2*k>= n  then
    k := n-k;
  For i := 1 to k do
  Begin
    result := result *n DIV i;
    dec(n);
  end;
end;

function popcnt32(n:Uint32):NativeUint;
//https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation
const
  K1  = $0101010101010101;
  K33 = $3333333333333333;
  K55 = $5555555555555555;
  KF1 = $0F0F0F0F0F0F0F0F;
begin
  n := n- (n shr 1) AND NativeUint(K55);
  n := (n AND NativeUint(K33))+ ((n shr 2) AND NativeUint(K33));
  n := (n + (n shr 4)) AND NativeUint(KF1);
  n := (n*NativeUint(K1)) SHR 24;
  popcnt32 := n;
end;

var
  bit1cnt,
  k : LongWord;
  PernCnt : Uint64;
Begin
  writeln('the 25 first pernicious numbers');
  k:=1;
  PernCnt:=0;
  repeat
    IF PrimeTil64[popCnt32(k)] <> 0 then Begin
      inc(PernCnt); write(k,' ');end;
    inc(k);
  until PernCnt >= 25;
  writeln;

  writeln('pernicious numbers in [888888877..888888888]');
  For k :=  888888877 to 888888888 do
    IF PrimeTil64[popCnt32(k)] <> 0  then
      write(k,' ');
  writeln(#13#10);

  k := 8;
  repeat
    PernCnt := 0;
    For bit1cnt := 0 to k do
    Begin
      //i == number of Bits set,n_beyond_k(k,i) == number of arrangements
      IF PrimeTil64[bit1cnt] <> 0 then
        inc(PernCnt,n_beyond_k(k,bit1cnt));
    end;
    writeln(PernCnt,' pernicious numbers in [0..2^',k,'-1]');
    inc(k,k);
  until k>64;
end.
```

```txt

the 25 first pernicious numbers
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
pernicious numbers in [888888877..888888888]
888888877 888888878 888888880 888888883 888888885 888888886

148 pernicious numbers in [0..2^8-1]
21416 pernicious numbers in [0..2^16-1]
1421120880 pernicious numbers in [0..2^32-1]
1214766910143514374 pernicious numbers in [0..2^64-1]
```



## Perl

```perl
sub is_pernicious {
    my $n = shift;
    my $c = 2693408940;  # primes < 32 as set bits
    while ($n) { $c >>= 1; $n &= ($n - 1); }
    $c & 1;
}

my ($i, @p) = 0;
while (@p < 25) {
    push @p, $i if is_pernicious($i);
    $i++;
}

print join ' ', @p;
print "\n";
($i, @p) = (888888877,);
while ($i < 888888888) {
    push @p, $i if is_pernicious($i);
    $i++;
}

print join ' ', @p;
```

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```


Alternately, generating the same output using a method similar to Pari/GP:
```perl
use ntheory qw/is_prime hammingweight/;
my $i = 1;
my @pern = map { $i++ while !is_prime(hammingweight($i)); $i++; } 1..25;
print "@pern\n";
print join(" ", grep { is_prime(hammingweight($_)) } 888888877 .. 888888888), "\n";
```



## Perl 6

Straightforward implementation using Perl 6's ''is-prime'' built-in subroutine.

```perl6
sub is-pernicious(Int $n --> Bool) {
    is-prime [+] $n.base(2).comb;
}

say (grep &is-pernicious, 0 .. *)[^25];
say grep &is-pernicious, 888_888_877 .. 888_888_888;
```

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```



## Phix


```Phix
function is_prime(atom n)
    if n<2 then return false end if
    for i=2 to floor(sqrt(n)) do
        if mod(n,i)=0 then return false end if
    end for
    return true
end function

function pernicious(integer n)
    return is_prime(sum(int_to_bits(n,32)))
end function

sequence s = {}
integer n = 1
while length(s)<25 do
    if pernicious(n) then
        s &= n
    end if
    n += 1
end while
?s
s = {}
for i=888_888_877 to 888_888_888 do
    if pernicious(i) then
        s &= i
    end if
end for
?s
```

```txt

{3,5,6,7,9,10,11,12,13,14,17,18,19,20,21,22,24,25,26,28,31,33,34,35,36}
{888888877,888888878,888888880,888888883,888888885,888888886}

```



## PicoLisp

Using 'prime?' from [[Primality by trial division#PicoLisp]].

```PicoLisp
(de pernicious? (N)
   (prime? (cnt = (chop (bin N)) '("1" .))) )
```

Test:

```PicoLisp
: (let N 0
   (do 25
      (until (pernicious? (inc 'N)))
      (printsp N) ) )
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36 -> 36

: (filter pernicious? (range 888888877 888888888))
-> (888888877 888888878 888888880 888888883 888888885 888888886)
```



## PL/I


```PL/I

pern: procedure options (main);
   declare (i, n) fixed binary (31);

   n = 3;
   do i = 1 to 25, 888888877 to 888888888;
      if i = 888888877 then do; n = i ; put skip; end;
      do while ( ^is_prime ( tally(bit(n), '1'b) ) );
         n = n + 1;
      end;
      put edit( trim(n), ' ') (a);
      n = n + 1;
   end;

is_prime: procedure (n) returns (bit(1));
   declare n fixed (15);
   declare i fixed (10);

   if n < 2 then return ('0'b);
   if n = 2 then return ('1'b);
   if mod(n, 2) = 0 then return ('0'b);

   do i = 3 to sqrt(n) by 2;
      if mod(n, i) = 0 then return ('0'b);
   end;
   return ('1'b);
end is_prime;

end pern;

```

Results:

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886 888888889 888888890 888888892 888888897 888888898 888888900

```



## PowerShell


```PowerShell

function pop-count($n) {
    (([Convert]::ToString($n, 2)).toCharArray() | where {$_ -eq '1'}).count
}

function isPrime ($n) {
    if ($n -eq 1) {$false}
    elseif ($n -eq 2) {$true}
    elseif ($n -eq 3) {$true}
    else{
        $m = [Math]::Floor([Math]::Sqrt($n))
        (@(2..$m | where {($_ -lt $n)  -and ($n % $_ -eq 0) }).Count -eq 0)
    }
}

$i = 0
$num = 1
$arr = while($i -lt 25) {
    if((isPrime (pop-count $num))) {
        $i++
        $num
    }
    $num++
}
"first 25 pernicious numbers"
"$arr"
""
"pernicious numbers between 888,888,877 and 888,888,888"
"$(888888877..888888888 | where{isprime(pop-count $_)})"

```

<b>Output:</b>

```txt

First 25 pernicious numbers
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36

Pernicious numbers between 888,888,877 and 888,888,888
888888877 888888878 888888880 888888883 888888885 888888886

```



### As An Advanced Function

Just an exercise in how to make the input more "PowerShelly".

The '''PopCount''' property is available in each of the returned integers.

```PowerShell

function Select-PerniciousNumber
{
    [CmdletBinding()]
    [OutputType([int])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        $InputObject
    )

    Begin
    {
        function Test-Prime ([int]$n)
        {
            $n = [Math]::Abs($n)

            if ($n -eq 0 -or $n -eq 1) {return $false}

            for ($m = 2; $m -le [Math]::Sqrt($n); $m++)
            {
                if (($n % $m) -eq 0) {return $false}
            }

            return $true
        }

        [scriptblock]$popCount = {(([Convert]::ToString($this, 2)).ToCharArray() | Where-Object {$_ -eq '1'}).Count}
    }
    Process
    {
        foreach ($object in $InputObject)
        {
            $object | Add-Member -MemberType ScriptProperty -Name PopCount -Value $popCount -Force -PassThru | ForEach-Object {
                if (Test-Prime $_.PopCount)
                {
                    $_
                }
            }
        }
    }
}

```


```PowerShell

$start, $end = 0, 999999
$range1 = $start..$end | Select-PerniciousNumber | Select-Object -First 25

"First {0} pernicious numbers:`n{1}`n" -f $range1.Count, ($range1 -join ", ")

$start, $end = 888888877, 888888888
$range2 = $start..$end | Select-PerniciousNumber

"Pernicious numbers between {0} and {1}:`n{2}`n" -f $start, $end, ($range2 -join ", ")

```

```txt

First 25 pernicious numbers:
3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36

Pernicious numbers between 888888877 and 888888888:
888888877, 888888878, 888888880, 888888883, 888888885, 888888886

```



## PureBasic


```PureBasic

EnableExplicit

Procedure.i SumBinaryDigits(Number)
  If Number < 0 : number = -number : EndIf; convert negative numbers to positive
  Protected sum = 0
  While Number > 0
    sum + Number % 2
    Number / 2
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.i IsPrime(Number)
  If Number <= 1
    ProcedureReturn #False
  ElseIf Number <= 3
    ProcedureReturn #True
  ElseIf Number % 2 = 0 Or Number % 3 = 0
    ProcedureReturn #False
  EndIf
  Protected i = 5
  While i * i <= Number
    If Number % i = 0 Or Number % (i + 2) = 0
      ProcedureReturn #False
    EndIf
    i + 6
  Wend
  ProcedureReturn #True
EndProcedure

Procedure.i IsPernicious(Number)
  Protected popCount = SumBinaryDigits(Number)
  ProcedureReturn Bool(IsPrime(popCount))
EndProcedure

Define n = 1, count = 0
If OpenConsole()
  PrintN("The following are the first 25 pernicious numbers :")
  PrintN("")
  Repeat
    If IsPernicious(n)
      Print(RSet(Str(n), 3))
      count + 1
    EndIf
    n + 1
  Until count = 25
  PrintN("")
  PrintN("")
  PrintN("The pernicious numbers between 888,888,877 and 888,888,888 inclusive are : ")
  PrintN("")
  For n = 888888877 To 888888888
    If IsPernicious(n)
      Print(RSet(Str(n), 10))
    EndIf
  Next
  PrintN("")
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


```txt

The following are the first 25 pernicious numbers :

  3  5  6  7  9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36

The pernicious numbers between 888,888,877 and 888,888,888 inclusive are :

 888888877 888888878 888888880 888888883 888888885 888888886

```



## Python


```python
>>>
 def popcount(n): return bin(n).count("1")

>>> primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61}
>>> p, i = [], 0
>>> while len(p) < 25:
        if popcount(i) in primes: p.append(i)
        i += 1


>>> p
[3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36]
>>> p, i = [], 888888877
>>> while i <= 888888888:
        if popcount(i) in primes: p.append(i)
        i += 1


>>> p
[888888877, 888888878, 888888880, 888888883, 888888885, 888888886]
>>>
```



## Racket



```racket
#lang racket
(require math/number-theory rnrs/arithmetic/bitwise-6)

(define pernicious? (compose prime? bitwise-bit-count))

(define (dnl . strs)
  (for-each displayln strs))

(define (show-sequence seq)
  (string-join (for/list ((v (in-values*-sequence seq))) (~a ((if (list? v) car values) v))) ", "))

(dnl
 "Task requirements:"
 "display the first 25 pernicious numbers."
 (show-sequence (in-parallel (sequence-filter pernicious? (in-naturals 1)) (in-range 25)))
 "display all pernicious numbers between 888,888,877 and 888,888,888 (inclusive)."
 (show-sequence (sequence-filter pernicious? (in-range 888888877 (add1 888888888)))))

(module+ test
  (require rackunit)
  (check-true (pernicious? 22)))
```


```txt
Task requirements:
display the first 25 pernicious numbers.
3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36
display all pernicious numbers between 888,888,877 and 888,888,888 (inclusive).
888888877, 888888878, 888888880, 888888883, 888888885, 888888886
```



## REXX

Programming note:   to increase the size of the numbers being tested   (to greater than 100 decimal digits),

all that is needed is to extend the list of low primes in the    2<sup>nd</sup>   line in the    '''pernicious'''   procedure (below);

the highest prime (Hprime) should exceed the number of decimal digits in   <big> 2<sup>Hprime</sup>.</big>

The program could be easily extended by programmatically generating enough primes to handle much larger numbers.

```txt

╔════════════════════════════════════════════════════════════════════════════════════════╗
╠═════ How the ─── popCount ─── function works (working from the inner─most level): ═════╣
║                                                                                        ║
║ arg(1)     obtains the value of the 1st argument passed to the  (popCount)  function.  ║
║ d2x        converts a decimal string  ──►  heXadecimal  (it may have a leading zeroes).║
║ +0         adds zero to the (above) string,  removing any superfluous leading zeroes.  ║
║ translate  converts all zeroes to blanks    (the 2nd argument defaults to a blank).    ║
║ space      removes all blanks from the character string  (now only containing '1's).   ║
║ length     counts the number of characters in the string.                              ║
║ return     returns the above value to the invoker.                                     ║
║                                                                                        ║
║            Note that    all    values in REXX are stored as  (eight─bit)  characters.  ║
╚════════════════════════════════════════════════════════════════════════════════════════╝

```


```rexx
/*REXX program computes and displays a number (and also a range) of  pernicious numbers.*/
numeric digits 100                               /*be able to handle large numbers.     */
parse arg N L H .                                /*obtain optional arguments from the CL*/
if N=='' | N==','  then N=25                     /*N  not given?  Then use the default. */
if L=='' | L==','  then L=888888877              /*L   "    "       "   "   "     "     */
if H=='' | H==','  then H=888888888              /*H   "    "       "   "   "     "     */
say 'The 1st '   N    " pernicious numbers are:" /*display a nice title for the numbers.*/
say  pernicious(1,,N)                            /*get all pernicious # from  1 ─~─► N. */
say                                              /*display a blank line for a separator.*/
say 'Pernicious numbers between '      L       " and "       H        ' (inclusive) are:'
say  pernicious(L,H)                             /*get all pernicious # from  L ───► H. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
pernicious: procedure;  parse arg bot,top,lim    /*obtain the bot and top numbers, limit*/
            p='2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101'
            @.=0
                  do k=1    until  _==''         /*examine the  list of some low primes.*/
                  _=word(p, k);  @._=1           /*generate an array  "   "   "     "   */
                  end   /*k*/
            $=                                   /*list of pernicious numbers (so far). */
            if m==''    then   m=999999999       /*Not given?  Then use a gihugic limit.*/
            if top==''  then top=999999999       /* "    "       "   "  "    "      "   */
            #=0                                  /*number of pernicious numbers (so far)*/
                  do j=bot  to top  until #==lim /*generate pernicious #s 'til satisfied*/
                  pc=popCount(j)                 /*obtain the population count for   J. */
                  if \@.pc  then iterate         /*if popCount not in @.prime,  skip it.*/
                  $=$ j                          /*append a pernicious number  to list. */
                  #=#+1                          /*bump the pernicious number  count.   */
                  end   /*j*/
            return substr($, 2)                  /*return the results,  sans 1st blank. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
popCount:   return length( space( translate( x2b( d2x(arg(1))) +0,, 0), 0)) /*count 1's.*/
```

'''output'''   when the default inputs are used:

```txt

The 1st  25  pernicious numbers are:
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36

Pernicious numbers between  888888877  and  888888888  (inclusive) are:
888888877 888888878 888888880 888888883 888888885 888888886

```



## Ring

Programming note:   as written, this program can't handle the large numbers required for the 2<sup>nd</sup> task requirement   (it receives a '''Numeric Overflow''').

```ring

# Project : Pernicious numbers

see "The first 25 pernicious numbers:" + nl
nr = 0
for n=1 to 50
    sum = 0
    str = decimaltobase(n, 2)
    for m=1 to len(str)
        if str[m] = "1"
           sum = sum + 1
        ok
    next
    if isprime(sum)
       nr = nr + 1
       see "" + n + " "
    ok
    if nr = 25
       exit
    ok
next

func decimaltobase(nr, base)
     binary = 0
     i = 1
     while(nr != 0)
           remainder = nr % base
           nr = floor(nr/base)
           binary= binary + (remainder*i)
           i = i*10
     end
     return string(binary)

func isprime num
     if (num <= 1) return 0 ok
     if (num % 2 = 0 and num != 2) return 0 ok
     for i = 3 to floor(num / 2) -1 step 2
         if (num % i = 0) return 0 ok
     next
     return 1

```

Output:

```txt

The first 25 pernicious numbers:
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36

```



## Ruby


```ruby
require "prime"

class Integer

  def popcount
    to_s(2).count("1")   #Ruby 2.4:  digits(2).count(1)
  end

  def pernicious?
    popcount.prime?
  end

end

p 1.step.lazy.select(&:pernicious?).take(25).to_a
p ( 888888877..888888888).select(&:pernicious?)
```

```txt

[3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36]
[888888877, 888888878, 888888880, 888888883, 888888885, 888888886]

```


=={{header|S-lang}}==
<lang S-lang>% Simplistic prime-test from prime-by-trial-division:
define is_prime(n)
{
   if (n <= 1) return(0);
   if (n == 2) return(1);
   if ((n & 1) == 0) return(0);

   variable mx = int(sqrt(n)), i;

   _for i (3, mx, 1) {
     if ((n mod i) == 0)
       return(0);
   }
   return(1);
}

define population(n)
{
   variable pc = 0;
   do {
      if (n & 1) pc++;
      n /= 2;
   }
   while (n);
   return(pc);
}

define is_pernicious(n)
{
   return(is_prime(population(n)));
}

variable plist = {}, n = 0;
while (length(plist) < 25) {
   n++;
   if (is_pernicious(n))
     list_append(plist, string(n));
}
print(strjoin(list_to_array(plist), " "));

plist = {};
_for n (888888877, 888888888, 1) {
   if (is_pernicious(n))
     list_append(plist, string(n));
}
print(strjoin(list_to_array(plist), " "));

```

"3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36"
"888888877 888888878 888888880 888888883 888888885 888888886"


## Scala


```scala
def isPernicious( v:Long ) : Boolean = BigInt(v.toBinaryString.toList.filter( _ == '1' ).length).isProbablePrime(16)

// Generate the output
{
  val (a,b1,b2) = (25,888888877L,888888888L)
  println( Stream.from(2).filter( isPernicious(_) ).take(a).toList.mkString(",") )
  println( {for( i <- b1 to b2 if( isPernicious(i) ) ) yield i}.mkString(",") )
}
```

```txt
3,5,6,7,9,10,11,12,13,14,17,18,19,20,21,22,24,25,26,28,31,33,34,35,36
888888877,888888878,888888880,888888883,888888885,888888886
```



## Seed7

The function <code>popcount</code> below [http://seed7.sourceforge.net/libraries/bitset.htm#bitset(in_integer) converts]
the integer into a [http://seed7.sourceforge.net/libraries/bitset.htm bitset].
The function [http://seed7.sourceforge.net/libraries/bitset.htm#card(in_bitset) card]
is used to compute the population count of the bitset.


```seed7
$ include "seed7_05.s7i";

const set of integer: primes is {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61};

const func integer: popcount (in integer: number) is
  return card(bitset(number));

const proc: main is func
  local
    var integer: num is 0;
    var integer: count is 0;
  begin
    for num range 0 to integer.last until count >= 25 do
      if popcount(num) in primes then
        write(num <& " ");
	incr(count);
      end if;
    end for;
    writeln;
    for num range 888888877 to 888888888 do
      if popcount(num) in primes then
        write(num <& " ");
      end if;
    end for;
    writeln;
  end func;
```


```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## Sidef

```ruby
func is_pernicious(n) {
    var c = 2693408940;  # primes < 32 as set bits
    while (n > 0) { c >>= 1; n &= (n - 1) }
    c & 1;
}

var (i, *p) = 0;
while (p.len < 25) {
    p << i if is_pernicious(i);
    ++i;
}

say p.join(' ');

var (i, *p) = 888888877;
while (i < 888888888) {
    p << i if is_pernicious(i);
    ++i;
}

say p.join(' ');
```


```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886

```



## Symsyn



```symsyn


primes : 0b0010100000100000100010100010000010100000100010100010100010101100

| the first 25 pernicious numbers


       $T                                | clear string
       num_pn                            | set to zero
       2 n                               | start at 2
       5 hi_bit
       if num_pn LT 25
          call popcount                  | count ones
          if primes bit pop_cnt          | if pop_cnt bit of bit vector primes is one
             + num_pn                    | inc number of pernicious numbers
             ~ n $S                      | convert to decimal string
             + ' ' $S                    | pad a space
             + $S $T                     | add to string $T
          endif
          + pop_cnt                      | next number (odd) has one more bit than previous (even)
          + n                            | next number
          if primes bit pop_cnt
             + num_pn
             ~ n $S
             + ' ' $S
             + $S $T
          endif
          + n
          goif                           | go back to if
       endif
       $T []                             | display numbers



| pernicious numbers in range 888888877 .. 888888888

       $T                                | clear string
       num_pn                            | set to zero
       888888876 n                       | start at 888888876
       29 hi_bit
       if n LE 888888888
          call popcount                  | count ones
          if primes bit pop_cnt          | if pop_cnt bit of bit vector primes is one
             + num_pn                    | inc number of pernicious numbers
             ~ n $S                      | convert to decimal string
             + ' ' $S                    | pad a space
             + $S $T                     | add to string $T
          endif
          + pop_cnt                      | next number (odd) has one more bit than previous (even)
          + n                            | next number
          if primes bit pop_cnt
             + num_pn
             ~ n $S
             + ' ' $S
             + $S $T
          endif
          + n
          goif                           | go back to if
       endif
       $T []                             | display numbers

       stop



popcount                                 | count ones in bit field
       pop_cnt                           | pop_cnt to zero
       1 bit_num                         | only count even numbers so skip bit 0
       if bit_num LE hi_bit
          if n bit bit_num
             + pop_cnt
          endif
          + bit_num
          goif
       endif
       return


```


```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36 37
888888877 888888878 888888880 888888883 888888885 888888886 888888889

```




## Tcl

```tcl
package require math::numtheory

proc pernicious {n} {
    ::math::numtheory::isprime [tcl::mathop::+ {*}[split [format %b $n] ""]]
}

for {set n 0;set p {}} {[llength $p] < 25} {incr n} {
    if {[pernicious $n]} {lappend p $n}
}
puts [join $p ","]
for {set n 888888877; set p {}} {$n <= 888888888} {incr n} {
    if {[pernicious $n]} {lappend p $n}
}
puts [join $p ","]
```

```txt

3,5,6,7,9,10,11,12,13,14,17,18,19,20,21,22,24,25,26,28,31,33,34,35,36
888888877,888888878,888888880,888888883,888888885,888888886

```



## VBA

```vb
Private Function population_count(ByVal number As Long) As Integer
    Dim result As Integer
    Dim digit As Integer
    Do While number > 0
        If number Mod 2 = 1 Then
            result = result + 1
        End If
        number = number \ 2
    Loop
    population_count = result
End Function

Function is_prime(n As Integer) As Boolean
    If n < 2 Then
        is_prime = False
        Exit Function
    End If
    For i = 2 To Sqr(n)
        If n Mod i = 0 Then
            is_prime = False
            Exit Function
        End If
    Next i
    is_prime = True
End Function

Function pernicious(n As Long)
    Dim tmp As Integer
    tmp = population_count(n)
    pernicious = is_prime(tmp)
End Function

Public Sub main()
    Dim count As Integer
    Dim n As Long: n = 1
    Do While count < 25
        If pernicious(n) Then
            Debug.Print n;
            count = count + 1
        End If
        n = n + 1
    Loop
    Debug.Print
    For n = 888888877 To 888888888
        If pernicious(n) Then
            Debug.Print n;
        End If
    Next n
End Sub
```
```txt
 3  5  6  7  9  10  11  12  13  14  17  18  19  20  21  22  24  25  26  28  31  33  34  35  36
 888888877  888888878  888888880  888888883  888888885  888888886
```


## VBScript


```vb
'check if the number is pernicious
Function IsPernicious(n)
	IsPernicious = False
	bin_num = Dec2Bin(n)
	sum = 0
	For h = 1 To Len(bin_num)
		sum = sum + CInt(Mid(bin_num,h,1))
	Next
	If IsPrime(sum) Then
		IsPernicious = True
	End If
End Function

'prime number validation
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

'decimal to binary converter
Function Dec2Bin(n)
	q = n
	Dec2Bin = ""
	Do Until q = 0
		Dec2Bin = CStr(q Mod 2) & Dec2Bin
		q = Int(q / 2)
	Loop
End Function

'display the first 25 pernicious numbers
c = 0
WScript.StdOut.Write "First 25 Pernicious Numbers:"
WScript.StdOut.WriteLine
For k = 1 To 100
	If IsPernicious(k) Then
		WScript.StdOut.Write k & ", "
		c = c + 1
	End If
	If c = 25 Then
		Exit For
	End If
Next
WScript.StdOut.WriteBlankLines(2)

'display the pernicious numbers between  888,888,877 to 888,888,888 (inclusive)
WScript.StdOut.Write "Pernicious Numbers between 888,888,877 to 888,888,888 (inclusive):"
WScript.StdOut.WriteLine
For l = 888888877 To 888888888
	If IsPernicious(l) Then
		WScript.StdOut.Write l & ", "
	End If
Next
WScript.StdOut.WriteLine
```


```txt

First 25 Pernicious Numbers:
3, 5, 6, 7, 9, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 22, 24, 25, 26, 28, 31, 33, 34, 35, 36,

Pernicious Numbers between 888,888,877 to 888,888,888 (inclusive):
888888877, 888888878, 888888880, 888888883, 888888885, 888888886,

```



## Visual Basic .NET

```vbnet
Module Module1

    Function PopulationCount(n As Long) As Integer
        Dim cnt = 0
        Do
            If (n Mod 2) <> 0 Then
                cnt += 1
            End If
            n >>= 1
        Loop While n > 0
        Return cnt
    End Function

    Function IsPrime(x As Integer) As Boolean
        If x <= 2 OrElse (x Mod 2) = 0 Then
            Return x = 2
        End If

        Dim limit = Math.Sqrt(x)
        For i = 3 To limit Step 2
            If x Mod i = 0 Then
                Return False
            End If
        Next

        Return True
    End Function

    Function Pernicious(start As Integer, count As Integer, take As Integer) As IEnumerable(Of Integer)
        Return Enumerable.Range(start, count).Where(Function(n) IsPrime(PopulationCount(n))).Take(take)
    End Function

    Sub Main()
        For Each n In Pernicious(0, Integer.MaxValue, 25)
            Console.Write("{0} ", n)
        Next
        Console.WriteLine()

        For Each n In Pernicious(888888877, 11, 11)
            Console.Write("{0} ", n)
        Next
        Console.WriteLine()
    End Sub

End Module
```

```txt
3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888888877 888888878 888888880 888888883 888888885 888888886
```



## Wortel

The following function returns true if it's argument is a pernicious number:

```wortel
:ispernum ^(@isPrime \@count \=1 @arr &\`![.toString 2])
```

Task:

```wortel
!-ispernum 1..36 ; returns [3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36]
!-ispernum 888888877..888888888 ; returns [888888877 888888878 888888880 888888883 888888885 888888886]
```



## zkl

The largest number of bits is 30.

```zkl
primes:=T(2,3,5,7,11,13,17,19,23,29,31,37,41);
N:=0; foreach n in ([2..]){
   if(n.num1s : primes.holds(_)){
      print(n," ");
      if((N+=1)==25) break;
   }
}
foreach n in ([0d888888877..888888888]){
   if (n.num1s : primes.holds(_)) "%,d; ".fmt(n).print();
}
```

Int.num1s returns the number of 1 bits. eg (3).num1s-->2
```txt

3 5 6 7 9 10 11 12 13 14 17 18 19 20 21 22 24 25 26 28 31 33 34 35 36
888,888,877; 888,888,878; 888,888,880; 888,888,883; 888,888,885; 888,888,886;

```

Or in a more functional style:

```zkl
primes:=T(2,3,5,7,11,13,17,19,23,29,31,37,41);
p:='wrap(n){ primes.holds(n.num1s) };

[1..].filter(25,p).toString(*).println();
[0d888888877..888888888].filter(p).println();
```

'wrap is syntactic sugar for a closure - it creates a function that
wraps local data (variable primes in this case). We assign that function to p.
```txt

L(3,5,6,7,9,10,11,12,13,14,17,18,19,20,21,22,24,25,26,28,31,33,34,35,36)
L(888888877,888888878,888888880,888888883,888888885,888888886)

```


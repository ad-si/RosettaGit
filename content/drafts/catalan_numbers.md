+++
title = "Catalan numbers"
description = ""
date = 2019-10-15T08:18:30Z
aliases = []
[extra]
id = 9246
[taxonomies]
categories = []
tags = []
+++

{{Wikipedia}}
{{task|Arithmetic operations}}



Catalan numbers are a sequence of numbers which can be defined directly:
:<math>C_n = \frac{1}{n+1}{2n\choose n} = \frac{(2n)!}{(n+1)!\,n!} \qquad\mbox{ for }n\ge 0.</math>
Or recursively:
:<math>C_0 = 1 \quad \mbox{and} \quad C_{n+1}=\sum_{i=0}^{n}C_i\,C_{n-i}\quad\text{for }n\ge 0;</math>
Or alternatively (also recursive):
:<math>C_0 = 1 \quad \mbox{and} \quad C_n=\frac{2(2n-1)}{n+1}C_{n-1},</math>


;Task:
Implement at least one of these algorithms and print out the first 15 Catalan numbers with each. 

[[Memoization]]   is not required, but may be worth the effort when using the second method above.


;Related tasks:
*[[Catalan numbers/Pascal's triangle]]
*[[Evaluate binomial coefficients]]





## 11l


```11l
V c = 1
L(n) 1..15
   print(c)
   c = 2 * (2 * n - 1) * c I/ (n + 1)
```

{{out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440

```



## 360 Assembly

Very compact version. 

```360asm
CATALAN  CSECT        08/09/2015
         USING  CATALAN,R15
         LA     R7,1               c=1
         LA     R6,1               i=1
LOOPI    CH     R6,=H'15'          do i=1 to 15 
         BH     ELOOPI
         XDECO  R6,PG              edit i
         LR     R5,R6              i
         SLA    R5,1               *2
         BCTR   R5,0               -1
         SLA    R5,1               *2
         MR     R4,R7              *c
         LA     R6,1(R6)           i=i+1
         DR     R4,R6              /i
         LR     R7,R5              c=2*(2*i-1)*c/(i+1)
         XDECO  R7,PG+12           edit c
         XPRNT  PG,24              print
         B      LOOPI              next i
ELOOPI   BR     R14
PG       DS     CL24
         YREGS 
         END    CATALAN
```

{{out}}

```txt

           1           1
           2           2
           3           5
           4          14
           5          42
           6         132
           7         429
           8        1430
           9        4862
          10       16796
          11       58786
          12      208012
          13      742900
          14     2674440
          15     9694845

```



## ABAP

This works for ABAP Version 7.40 and above


```ABAP

report z_catalan_numbers.

class catalan_numbers definition.
  public section.
    class-methods:
      get_nth_number
        importing
          i_n                     type int4
        returning
          value(r_catalan_number) type int4.
endclass.

class catalan_numbers implementation.
  method get_nth_number.
    r_catalan_number = cond int4(
      when i_n eq 0
      then 1
      else reduce int4(
        init
          result = 1
          index = 1
        for position = 1 while position <= i_n
        next
          result = result * 2 * ( 2 * index - 1 ) div ( index + 1 )
          index = index + 1 ) ).
  endmethod.
endclass.

start-of-selection.
  do 15 times.
    write / |C({ sy-index - 1 }) = { catalan_numbers=>get_nth_number( sy-index - 1 ) }|.
  enddo.

```


{{out}}


```txt

C(0) = 1
C(1) = 1
C(2) = 2
C(3) = 5
C(4) = 14
C(5) = 42
C(6) = 132
C(7) = 429
C(8) = 1430
C(9) = 4862
C(10) = 16796
C(11) = 58786
C(12) = 208012
C(13) = 742900
C(14) = 2674440

```



## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Catalan is
   function Catalan (N : Natural) return Natural is
      Result : Positive := 1;
   begin
      for I in 1..N loop
         Result := Result * 2 * (2 * I - 1) / (I + 1);
      end loop;
      return Result;
   end Catalan;
begin
   for N in 0..15 loop
      Put_Line (Integer'Image (N) & " =" & Integer'Image (Catalan (N)));
   end loop;
end Test_Catalan;
```

{{out|Sample output}}

```txt

 0 = 1
 1 = 1
 2 = 2
 3 = 5
 4 = 14
 5 = 42
 6 = 132
 7 = 429
 8 = 1430
 9 = 4862
 10 = 16796
 11 = 58786
 12 = 208012
 13 = 742900
 14 = 2674440
 15 = 9694845

```



## ALGOL 68


```algol68
# calculate the first few catalan numbers, using LONG INT values        #
# (64-bit quantities in Algol 68G which can handle up to C23)           #

# returns n!/k!                                                         #
PROC factorial over factorial = ( INT n, k )LONG INT:
     IF      k > n THEN 0
     ELIF    k = n THEN 1
     ELSE #  k < n #
         LONG INT f := 1;
         FOR i FROM k + 1 TO n DO f *:= i OD;
         f
     FI # factorial over factorial # ;

# returns n!                                                             #
PROC factorial = ( INT n )LONG INT:
     BEGIN
         LONG INT f := 1;
         FOR i FROM 2 TO n DO f *:= i OD;
         f
     END # factorial # ;

# returnss the nth Catalan number using binomial coefficeients            #
# uses the factorial over factorial procedure for a slight optimisation   #
# note:     Cn = 1/(n+1)(2n n)                                            #
#              = (2n)!/((n+1)!n!)                                         #
#              = factorial over factorial( 2n, n+1 )/n!                   #
PROC catalan = ( INT n )LONG INT: IF n < 2 THEN 1 ELSE factorial over factorial( n + n, n + 1 ) OVER factorial( n ) FI; 

# show the first few catalan numbers                                      #
FOR i FROM 0 TO 15 DO
    print( ( whole( i, -2 ), ": ", whole( catalan( i ), 0 ), newline ) )
OD
```

{{out}}

```txt

 0: 1
 1: 1
 2: 2
 3: 5
 4: 14
 5: 42
 6: 132
 7: 429
 8: 1430
 9: 4862
10: 16796
11: 58786
12: 208012
13: 742900
14: 2674440
15: 9694845

```



## ALGOL W


```algolw
begin
    % print the catalan numbers up to C15 %
    integer Cprev;
    Cprev := 1; % C0 %
    write(     s_w := 0, i_w := 3, 0, ": ", i_w := 9, Cprev );
    for n := 1 until 15 do begin
        Cprev := round( ( ( ( 4 * n ) - 2 ) / ( n + 1 ) ) * Cprev );
        write( s_w := 0, i_w := 3, n, ": ", i_w := 9, Cprev );
    end for_n
end.
```

{{out}}

```txt

  0:         1
  1:         1
  2:         2
  3:         5
  4:        14
  5:        42
  6:       132
  7:       429
  8:      1430
  9:      4862
 10:     16796
 11:     58786
 12:    208012
 13:    742900
 14:   2674440
 15:   9694845

```



## APL


```apl
      {(!2×⍵)÷(!⍵+1)×!⍵}(⍳15)-1
```

{{out}}

```txt
1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440
```



## Arturo



```arturo
Catalan [n]{
	if n=0 { 
		return 1 
	} {
		return ((4*n-2)*$(Catalan n-1))/(n+1)
	}
}

loop $(range 0 15) {
	print $(padRight $(toString &) 5) + "  " + $(padLeft $(toString $(Catalan &)) 20)
}
```


{{out}}


```txt
    0  1                   
    1  1                   
    2  2                   
    3  5                   
    4  14                  
    5  42                  
    6  132                 
    7  429                 
    8  1430                
    9  4862                
   10  16796               
   11  58786               
   12  208012              
   13  742900              
   14  2674440             
   15  9694845
```



## AutoHotkey

As AutoHotkey has no BigInt, the formula had to be tweaked to prevent overflow. It still fails after n=22

```AHK
Loop 15
   out .= "`n" Catalan(A_Index)
Msgbox % clipboard := SubStr(out, 2)
catalan( n ) {
; By [VxE]. Returns ((2n)! / ((n + 1)! * n!)) if 0 <= N <= 22 (higher than 22 results in overflow)
If ( n < 3 ) ; values less than 3 are handled specially
   Return n < 0 ? "" : n = 0 ? 1 : n

i := 1 ; initialize the accumulator to 1

Loop % n - 1 >> 1 ; build the numerator by multiplying odd values between 2N and N+1
   i *= 1 + ( n - A_Index << 1 )

i <<= ( n - 2 >> 1 ) ; multiply the numerator by powers of 2 according to N

Loop % n - 3 >> 1 ; finish up by (integer) dividing by each of the non-cancelling factors
   i //= A_Index + 2

Return i
}
```

{{out}}

```txt
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845
```



## AWK


```AWK
# syntax: GAWK -f CATALAN_NUMBERS.AWK
BEGIN {
    for (i=0; i<=15; i++) {
      printf("%2d %10d\n",i,catalan(i))
    }
    exit(0)
}
function catalan(n,  ans) {
    if (n == 0) {
      ans = 1
    }
    else {
      ans = ((2*(2*n-1))/(n+1))*catalan(n-1)
    }
    return(ans)
}
```

{{out}}

```txt

 0          1
 1          1
 2          2
 3          5
 4         14
 5         42
 6        132
 7        429
 8       1430
 9       4862
10      16796
11      58786
12     208012
13     742900
14    2674440
15    9694845

```



## BASIC

{{works with|FreeBASIC}}
{{works with|QuickBASIC|4.5 (untested)}}

Use of <code>REDIM PRESERVE</code> means this will not work in QBasic (although that could be worked around if desired).


```qbasic
DECLARE FUNCTION catalan (n as INTEGER) AS SINGLE

REDIM SHARED results(0) AS SINGLE

FOR x% = 1 TO 15
    PRINT x%, catalan (x%)
NEXT

FUNCTION catalan (n as INTEGER) AS SINGLE
    IF UBOUND(results) < n THEN REDIM PRESERVE results(n)

    IF 0 = n THEN
    	results(0) = 1
    ELSE
    	results(n) = ((2 * ((2 * n) - 1)) / (n + 1)) * catalan(n - 1)
    END IF
    catalan = results(n)
END FUNCTION
```

{{out}}
 1             1
 2             2
 3             5
 4             14
 5             42
 6             132
 7             429
 8             1430
 9             4862
 10            16796
 11            58786
 12            208012
 13            742900
 14            2674440
 15            9694845

=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

The specification asks for the first 15 Catalan numbers. A lot of the other implementations produce either C(0) to C(15), which is 16 numbers, or else C(1) to C(15)—which is 15 numbers, but I'm not convinced they're the first 15. This program produces C(0) to C(14).


```basic
 10 FOR N=0 TO 14
 20 LET X=N
 30 GOSUB 130
 40 LET A=FX
 50 LET X=N+1
 60 GOSUB 130
 70 LET B=FX
 80 LET X=2*N
 90 GOSUB 130
100 PRINT N,FX/(B*A)
110 NEXT N
120 STOP
130 LET FX=1
140 FOR I=1 TO X
150 LET FX=FX*I
160 NEXT I
170 RETURN
```

{{out}}

```txt
0               1
1               1
2               2
3               5
4               14
5               42
6               132
7               429
8               1430
9               4862
10              16796
11              58786
12              208012
13              742900
14              2674440
```



## BBC BASIC



```bbcbasic
      10 FOR i% = 1 TO 15
      20   PRINT FNcatalan(i%)
      30 NEXT
      40 END
      50 DEF FNcatalan(n%)
      60   IF n% = 0 THEN = 1
      70   = 2 * (2 * n% - 1) * FNcatalan(n% - 1) / (n% + 1)
```

{{out}}

```txt
         1
         1
         2
         5
        14
        42
       132
       429
      1430
      4862
     16796
     58786
    208012
    742900
   2674440
```



## Befunge


{{trans|Ada}}

```befunge>0>:.:000p1
\:00g-#v_v
v 2-1*2p00 :+1g00\< $
> **00g1+/^v,*84,"="<
_^#<`*53:+1>#,.#+5< @
```


{{out}}

```txt
0 = 1
1 = 1
2 = 2
3 = 5
4 = 14
5 = 42
6 = 132
7 = 429
8 = 1430
9 = 4862
10 = 16796
11 = 58786
12 = 208012
13 = 742900
14 = 2674440
15 = 9694845
```



## Bracmat


```bracmat
( out$straight
& ( C
  =   
    .   ( F
        =   i prod
          .   !arg:0&1
            |   1:?prod
              & 0:?i
              &   whl
                ' ( 1+!i:~>!arg:?i
                  & !i*!prod:?prod
                  )
              & !prod
        )
      & F$(2*!arg)*(F$(!arg+1)*F$!arg)^-1
  )
& -1:?n
&   whl
  ' ( 1+!n:~>15:?n
    & out$(str$(C !n " = " C$!n))
    )
& out$"recursive, with memoization, without fractions"
& :?seenCs
& ( C
  =   i sum
    .   !arg:0&1
      |   ( !seenCs:? (!arg.?sum) ?
          |   0:?sum
            & -1:?i
            &   whl
              ' ( 1+!i:<!arg:?i
                & C$!i*C$(-1+!arg+-1*!i)+!sum:?sum
                )
            & (!arg.!sum) !seenCs:?seenCs
          )
        & !sum
  )
& -1:?n
&   whl
  ' ( 1+!n:~>15:?n
    & out$(str$(C !n " = " C$!n))
    )
& out$"recursive, without memoization, with fractions"
& ( C
  =   
    .   !arg:0&1
      | 2*(2*!arg+-1)*(!arg+1)^-1*C$(!arg+-1)
  )
& -1:?n
&   whl
  ' ( 1+!n:~>15:?n
    & out$(str$(C !n " = " C$!n))
    )
& out$"Using taylor expansion of sqrt(1-4X). (See http://bababadalgharaghtakamminarronnkonnbro.blogspot.in/2012/10/algebraic-type-systems-combinatorial.html)"
& out$(1+(1+-1*tay$((1+-4*X)^1/2,X,16))*(2*X)^-1+-1)
& out$
);
```

{{out}}

```bracmat
straight
C0 = 1
C1 = 1
C2 = 2
C3 = 5
C4 = 14
C5 = 42
C6 = 132
C7 = 429
C8 = 1430
C9 = 4862
C10 = 16796
C11 = 58786
C12 = 208012
C13 = 742900
C14 = 2674440
C15 = 9694845
recursive, with memoization, without fractions
C0 = 1
C1 = 1
C2 = 2
C3 = 5
C4 = 14
C5 = 42
C6 = 132
C7 = 429
C8 = 1430
C9 = 4862
C10 = 16796
C11 = 58786
C12 = 208012
C13 = 742900
C14 = 2674440
C15 = 9694845
recursive, without memoization, with fractions
C0 = 1
C1 = 1
C2 = 2
C3 = 5
C4 = 14
C5 = 42
C6 = 132
C7 = 429
C8 = 1430
C9 = 4862
C10 = 16796
C11 = 58786
C12 = 208012
C13 = 742900
C14 = 2674440
C15 = 9694845
Using taylor expansion of sqrt(1-4X). (See http://bababadalgharaghtakamminarronnkonnbro.blogspot.in/2012/10/algebraic-type-systems-combinatorial.html)
  1
+ X
+ 2*X^2
+ 5*X^3
+ 14*X^4
+ 42*X^5
+ 132*X^6
+ 429*X^7
+ 1430*X^8
+ 4862*X^9
+ 16796*X^10
+ 58786*X^11
+ 208012*X^12
+ 742900*X^13
+ 2674440*X^14
+ 9694845*X^15

```



## Brat


```brat
catalan = { n |
  true? n == 0
    { 1 }
    { (2 * ( 2 * n - 1) / ( n + 1 )) * catalan(n - 1) }
}

0.to 15 { n |
  p "#{n} - #{catalan n}"
}
```

{{out}}

```txt
0 - 1
1 - 1
2 - 2
3 - 5
4 - 14
5 - 42
6 - 132
7 - 429
8 - 1430
9 - 4862
10 - 16796
11 - 58786
12 - 208012
13 - 742900
14 - 2674440
15 - 9694845

```



## C

All three methods mentioned in the task:

```c>#include <stdio.h


typedef unsigned long long ull;

ull binomial(ull m, ull n)
{
	ull r = 1, d = m - n;
	if (d > n) { n = d; d = m - n; }

	while (m > n) {
		r *= m--;
		while (d > 1 && ! (r%d) ) r /= d--;
	}

	return r;
}

ull catalan1(int n) {
	return binomial(2 * n, n) / (1 + n);
}

ull catalan2(int n) {
	int i;
	ull r = !n;

	for (i = 0; i < n; i++)
		r += catalan2(i) * catalan2(n - 1 - i);
	return r;
}

ull catalan3(int n)
{
	return n ? 2 * (2 * n - 1) * catalan3(n - 1) / (1 + n) : 1;
}

int main(void)
{
	int i;
	puts("\tdirect\tsumming\tfrac");
	for (i = 0; i < 16; i++) {
		printf("%d\t%llu\t%llu\t%llu\n", i,
			catalan1(i), catalan2(i), catalan3(i));
	}

	return 0;
}
```

{{out}}
        direct  summing frac
 0      1       1       1
 1      1       1       1
 2      2       2       2
 3      5       5       5
 4      14      14      14
 5      42      42      42
 6      132     132     132
 7      429     429     429
 8      1430    1430    1430
 9      4862    4862    4862
 10     16796   16796   16796
 11     58786   58786   58786
 12     208012  208012  208012
 13     742900  742900  742900
 14     2674440 2674440 2674440
 15     9694845 9694845 9694845

== {{header|C sharp}} ==

```csharp
namespace CatalanNumbers
{
    /// <summary>
    /// Class that holds all options.
    /// </summary>
    public class CatalanNumberGenerator
    {
        private static double Factorial(double n)
        {
            if (n == 0)
                return 1;

            return n * Factorial(n - 1);
        }

        public double FirstOption(double n)
        {
            const double topMultiplier = 2;
            return Factorial(topMultiplier * n) / (Factorial(n + 1) * Factorial(n));
        }

        public double SecondOption(double n)
        {
            if (n == 0)
            {
                return 1;
            }
            double sum = 0;
            double i = 0;
            for (; i <= (n - 1); i++)
            {
                sum += SecondOption(i) * SecondOption((n - 1) - i);
            }
            return sum;
        }

        public double ThirdOption(double n)
        {
            if (n == 0)
            {
                return 1;
            }
            return ((2 * (2 * n - 1)) / (n + 1)) * ThirdOption(n - 1);
        }
    }
}


// Program.cs
using System;
using System.Configuration;

// Main program
// Be sure to add the following to the App.config file and add a reference to System.Configuration:
// <?xml version="1.0" encoding="utf-8" ?>
// <configuration>
//  <appSettings>
//    <clear/>
//    <add key="MaxCatalanNumber" value="50"/>
//  </appSettings>
// </configuration>
namespace CatalanNumbers
{
    class Program
    {
        static void Main(string[] args)
        {
            CatalanNumberGenerator generator = new CatalanNumberGenerator();
            int i = 0;
            DateTime initial;
            DateTime final;
            TimeSpan ts;

            try
            {
                initial = DateTime.Now;
                for (; i <= Convert.ToInt32(ConfigurationManager.AppSettings["MaxCatalanNumber"]); i++)
                {
                    Console.WriteLine("CatalanNumber({0}):{1}", i, generator.FirstOption(i));
                }
                final = DateTime.Now;
                ts = final - initial;
                Console.WriteLine("It took {0}.{1} to execute\n", ts.Seconds, ts.Milliseconds);

                i = 0;
                initial = DateTime.Now;
                for (; i <= Convert.ToInt32(ConfigurationManager.AppSettings["MaxCatalanNumber"]); i++)
                {
                    Console.WriteLine("CatalanNumber({0}):{1}", i, generator.SecondOption(i));
                }
                final = DateTime.Now;
                ts = final - initial;
                Console.WriteLine("It took {0}.{1} to execute\n", ts.Seconds, ts.Milliseconds);   

                i = 0;
                initial = DateTime.Now;
                for (; i <= Convert.ToInt32(ConfigurationManager.AppSettings["MaxCatalanNumber"]); i++)
                {
                    Console.WriteLine("CatalanNumber({0}):{1}", i, generator.ThirdOption(i));
                }
                final = DateTime.Now;
                ts = final - initial;
                Console.WriteLine("It took {0}.{1} to execute", ts.Seconds, ts.Milliseconds, ts.TotalMilliseconds);
                Console.ReadLine();
            }
            catch (Exception ex)
            {
                Console.WriteLine("Stopped at index {0}:", i);
                Console.WriteLine(ex.Message);
                Console.ReadLine();
            }
        }
    }
}
```

{{out}}

```txt

CatalanNumber(0):1
CatalanNumber(1):1
CatalanNumber(2):2
CatalanNumber(3):5
CatalanNumber(4):14
CatalanNumber(5):42
CatalanNumber(6):132
CatalanNumber(7):429
CatalanNumber(8):1430
CatalanNumber(9):4862
CatalanNumber(10):16796
CatalanNumber(11):58786
CatalanNumber(12):208012
CatalanNumber(13):742900
CatalanNumber(14):2674440
CatalanNumber(15):9694845
It took 0.14 to execute

CatalanNumber(0):1
CatalanNumber(1):1
CatalanNumber(2):2
CatalanNumber(3):5
CatalanNumber(4):14
CatalanNumber(5):42
CatalanNumber(6):132
CatalanNumber(7):429
CatalanNumber(8):1430
CatalanNumber(9):4862
CatalanNumber(10):16796
CatalanNumber(11):58786
CatalanNumber(12):208012
CatalanNumber(13):742900
CatalanNumber(14):2674440
CatalanNumber(15):9694845
It took 0.922 to execute

CatalanNumber(0):1
CatalanNumber(1):1
CatalanNumber(2):2
CatalanNumber(3):5
CatalanNumber(4):14
CatalanNumber(5):42
CatalanNumber(6):132
CatalanNumber(7):429
CatalanNumber(8):1430
CatalanNumber(9):4862
CatalanNumber(10):16796
CatalanNumber(11):58786
CatalanNumber(12):208012
CatalanNumber(13):742900
CatalanNumber(14):2674440
CatalanNumber(15):9694845
It took 0.3 to execute

```



## C++


### 4 Classes

We declare 4 classes representing the four different algorithms for calculating Catalan numbers as given in the description of the task. In addition, we declare two supporting classes for the calculation of factorials and binomial coefficients. Because these two are only internal supporting code they are hidden in namespace 'detail'. Overloading the function call operator to execute the calculation is an obvious decision when using C++. (algorithms.h)

```cpp
#if !defined __ALGORITHMS_H__
#define __ALGORITHMS_H__

namespace rosetta
  {
  namespace catalanNumbers
    {
    namespace detail
      {

      class Factorial
        {
        public:
          unsigned long long operator()(unsigned n)const;
        };

      class BinomialCoefficient
        {
        public:
          unsigned long long operator()(unsigned n, unsigned k)const;
        };

      } //namespace detail

    class CatalanNumbersDirectFactorial
      {
      public:
        CatalanNumbersDirectFactorial();
        unsigned long long operator()(unsigned n)const;
      private:
        detail::Factorial factorial;
      };

    class CatalanNumbersDirectBinomialCoefficient
      {
      public:
        CatalanNumbersDirectBinomialCoefficient();
        unsigned long long operator()(unsigned n)const;
      private:
        detail::BinomialCoefficient binomialCoefficient;
      };

    class CatalanNumbersRecursiveSum
      {
      public:
        CatalanNumbersRecursiveSum();
        unsigned long long operator()(unsigned n)const;
      };

    class CatalanNumbersRecursiveFraction
      {
      public:
        CatalanNumbersRecursiveFraction();
        unsigned long long operator()(unsigned n)const;
      };

    }   //namespace catalanNumbers
  }     //namespace rosetta

#endif //!defined __ALGORITHMS_H__
```

Here is the implementation of the algorithms. The c'tor of each class tells us the algorithm which will be used. (algorithms.cpp)

```cpp>#include <iostream

using std::cout;
using std::endl;
#include <cmath>
using std::floor;

#include "algorithms.h"
using namespace rosetta::catalanNumbers;


CatalanNumbersDirectFactorial::CatalanNumbersDirectFactorial()
  {
  cout<<"Direct calculation using the factorial"<<endl;
  }

unsigned long long CatalanNumbersDirectFactorial::operator()(unsigned n)const
  {
  if(n>1)
    {
    unsigned long long nFac = factorial(n);
    return factorial(2 * n) / ((n + 1) * nFac * nFac);
    }
  else
    {
    return 1;
    }
  }


CatalanNumbersDirectBinomialCoefficient::CatalanNumbersDirectBinomialCoefficient()
  {
  cout<<"Direct calculation using a binomial coefficient"<<endl;
  }

unsigned long long CatalanNumbersDirectBinomialCoefficient::operator()(unsigned n)const
  {
  if(n>1)
    return double(1) / (n + 1) * binomialCoefficient(2 * n, n);
  else
    return 1;
  }


CatalanNumbersRecursiveSum::CatalanNumbersRecursiveSum()
  {
  cout<<"Recursive calculation using a sum"<<endl;
  }

unsigned long long CatalanNumbersRecursiveSum::operator()(unsigned n)const
  {
  if(n>1)
    {
    const unsigned n_ = n - 1;
    unsigned long long sum = 0;
    for(unsigned i = 0; i <= n_; i++)
      sum += operator()(i) * operator()(n_ - i);
    return sum;
    }
  else
    {
    return 1;
    }
  }


CatalanNumbersRecursiveFraction::CatalanNumbersRecursiveFraction()
  {
  cout<<"Recursive calculation using a fraction"<<endl;
  }

unsigned long long CatalanNumbersRecursiveFraction::operator()(unsigned n)const
  {
  if(n>1)
    return (double(2 * (2 * n - 1)) / (n + 1)) * operator()(n-1);
  else
    return 1;
  }


unsigned long long detail::Factorial::operator()(unsigned n)const
  {
  if(n>1)
    return n * operator()(n-1);
  else
    return 1;
  }


unsigned long long detail::BinomialCoefficient::operator()(unsigned n, unsigned k)const
  {
  if(k == 0)
    return 1;
  
  if(n == 0)
    return 0;

  double product = 1;
  for(unsigned i = 1; i <= k; i++)
    product *= (double(n - (k - i)) / i);
  return (unsigned long long)(floor(product + 0.5));
  }
```

In order to test what we have done, a class Test is created. Using the template parameters N (number of Catalan numbers to be calculated) and A (the kind of algorithm to be used) the compiler will create code for all the test cases we need. What would C++ be without templates ;-) (tester.h)

```cpp
#if !defined __TESTER_H__
#define __TESTER_H__

#include <iostream>

namespace rosetta
  {
  namespace catalanNumbers
    {

    template <int N, typename A>
    class Test
      {
      public:
        static void Do()
          {
          A algorithm;
          for(int i = 0; i <= N; i++)
            std::cout<<"C("<<i<<")\t= "<<algorithm(i)<<std::endl;
          }
      };

    } //namespace catalanNumbers
  }   //namespace rosetta

#endif //!defined __TESTER_H__
```

Finally, we test the four different algorithms. Note that the first one (direct calculation using the factorial) only works up to N = 10 because some intermediate result (namely (2n)! with n = 11) exceeds the boundaries of an unsigned 64 bit integer. (catalanNumbersTest.cpp)

```cpp
#include "algorithms.h"
#include "tester.h"
using namespace rosetta::catalanNumbers;

int main(int argc, char* argv[])
  {
  Test<10, CatalanNumbersDirectFactorial>::Do();
  Test<15, CatalanNumbersDirectBinomialCoefficient>::Do();
  Test<15, CatalanNumbersRecursiveFraction>::Do();
  Test<15, CatalanNumbersRecursiveSum>::Do();
  return 0;
  }
```

{{out}}
(source code is compiled both by MS Visual C++ 10.0 (WinXP 32 bit) and GNU g++ 4.4.3 (Ubuntu 10.04 64 bit) compilers)

```txt

Direct calculation using the factorial
C(0)    = 1
C(1)    = 1
C(2)    = 2
C(3)    = 5
C(4)    = 14
C(5)    = 42
C(6)    = 132
C(7)    = 429
C(8)    = 1430
C(9)    = 4862
C(10)   = 16796
Direct calculation using a binomial coefficient
C(0)    = 1
C(1)    = 1
C(2)    = 2
C(3)    = 5
C(4)    = 14
C(5)    = 42
C(6)    = 132
C(7)    = 428
C(8)    = 1430
C(9)    = 4862
C(10)   = 16796
C(11)   = 58786
C(12)   = 208012
C(13)   = 742900
C(14)   = 2674440
C(15)   = 9694845
Recursive calculation using a fraction
C(0)    = 1
C(1)    = 1
C(2)    = 2
C(3)    = 5
C(4)    = 14
C(5)    = 42
C(6)    = 132
C(7)    = 429
C(8)    = 1430
C(9)    = 4862
C(10)   = 16796
C(11)   = 58786
C(12)   = 208012
C(13)   = 742900
C(14)   = 2674440
C(15)   = 9694845
Recursive calculation using a sum
C(0)    = 1
C(1)    = 1
C(2)    = 2
C(3)    = 5
C(4)    = 14
C(5)    = 42
C(6)    = 132
C(7)    = 429
C(8)    = 1430
C(9)    = 4862
C(10)   = 16796
C(11)   = 58786
C(12)   = 208012
C(13)   = 742900
C(14)   = 2674440
C(15)   = 9694845

```



## Clojure


```Clojure
(def ! (memoize #(apply * (range 1 (inc %)))))

(defn catalan-numbers-direct []
  (map #(/ (! (* 2 %))
	   (* (! (inc %)) (! %))) (range)))

(def catalan-numbers-recursive
     #(->> [1 1] ; [c0 n1]
	   (iterate (fn [[c n]]
		      [(* 2 (dec (* 2 n)) (/ (inc n)) c) (inc n)]) ,)
	   (map first ,)))

user> (take 15 (catalan-numbers-direct))
(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440)

user> (take 15 (catalan-numbers-recursive))
(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440)
```



## Common Lisp

With all three methods defined.

```lisp
(defun catalan1 (n)
  ;; factorial. CLISP actually has "!" defined for this
  (labels ((! (x) (if (zerop x) 1 (* x (! (1- x))))))
    (/ (! (* 2 n)) (! (1+ n)) (! n))))

;; cache
(defparameter *catalans* (make-array 5
				     :fill-pointer 0
				     :adjustable t
				     :element-type 'integer))
(defun catalan2 (n)
    (if (zerop n) 1
    ;; check cache
    (if (< n (length *catalans*)) (aref *catalans* n)
      (loop with c = 0 for i from 0 to (1- n) collect
	    (incf c (* (catalan2 i) (catalan2 (- n 1 i))))
	    ;; lower values always get calculated first, so
	    ;; vector-push-extend is safe
	    finally (progn (vector-push-extend c *catalans*) (return c))))))

(defun catalan3 (n)
  (if (zerop n) 1 (/ (* 2 (+ n n -1) (catalan3 (1- n))) (1+ n))))

;;; test all three methods
(loop for f in (list #'catalan1 #'catalan2 #'catalan3)
      for i from 1 to 3 do
      (format t "~%Method ~d:~%" i)
      (dotimes (i 16) (format t "C(~2d) = ~d~%" i (funcall f i))))
```



## D


```d
import std.stdio, std.algorithm, std.bigint, std.functional, std.range;

auto product(R)(R r) { return reduce!q{a * b}(1.BigInt, r); }

const cats1 = sequence!((a, n) => iota(n+2, 2*n+1).product / iota(1, n+1).product)(1);

BigInt cats2a(in uint n) {
    alias mcats2a = memoize!cats2a;
    if (n == 0) return 1.BigInt;
    return n.iota.map!(i => mcats2a(i) * mcats2a(n - 1 - i)).sum;
}

const cats2 = sequence!((a, n) => n.cats2a);

const cats3 = recurrence!q{ (4*n - 2) * a[n - 1] / (n + 1) }(1.BigInt);

void main() {
    foreach (cats; TypeTuple!(cats1, cats2, cats3))
        cats.take(15).writeln;
}
```

{{out}}

```txt
[1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440]
[1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440]
[1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440]
```



## EasyLang

<lang>func catalan n . ans .
  if n = 0
    ans = 1
  else
    call catalan n - 1 h
    ans = 2 * (2 * n - 1) * h / (1 + n)
  .
.
for i range 15
  call catalan i h
  print h
.
```

{{out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440

```



## EchoLisp

{{incorrect|Echolisp|series starts 1, 1, 2, ...}}

```scheme

(lib 'sequences)
(lib 'bigint)
(lib 'math)

;; function definition
(define (C1 n) (/ (factorial (* n 2)) (factorial (1+ n)) (factorial n)))
(for ((i [1 .. 16])) (write (C1 i)))
    → 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845 

;; using a recursive procedure with memoization
(define (C2 n) ;; ( Σ  ...)is the same as (sigma ..)
	(Σ (lambda(i) (* (C2 i) (C2 (- n i 1))))  0 (1- n)))
(remember 'C2 #(1)) ;; first term defined here

(for ((i [1 .. 16])) (write (C2 i)))
    → 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845 


;; using procrastinators = infinite sequence
(define (catalan n acc) (/ (* acc 2 (1- (* 2 n))) (1+ n)))
(define C3 (scanl catalan 1 [1 ..]))
(take C3 15)
    → (1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845)
 

;; the same, using infix notation
(lib 'match)
(load 'infix.glisp)

(define (catalan n acc) ((2 * acc * ( 2 * n - 1)) / (n + 1)))
(define C3 (scanl catalan 1 [1 ..]))

(take C3 15)
    → (1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845)
;; or
(for ((c C3) (i 15)) (write c))
    → 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845 

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE}

	make
		do
			across
				0 |..| 14 as c
			loop
				io.put_double (nth_catalan_number (c.item))
				io.new_line
			end
		end

	nth_catalan_number (n: INTEGER): DOUBLE
			--'n'th number in the sequence of Catalan numbers.
		require
			n_not_negative: n >= 0
		local
			s, t: DOUBLE
		do
			if n = 0 then
				Result := 1.0
			else
				t := 4 * n.to_double - 2
				s := n.to_double + 1
				Result := t / s * nth_catalan_number (n - 1)
			end
		end

end



```

{{out}}

```txt

1 
1 
2 
5 
14 
42 
132 
429 
1430 
4862 
16796 
58786 
208012 
742900 
2674440

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Catalan do
  def cat(n), do: div( factorial(2*n), factorial(n+1) * factorial(n) )
  
  defp factorial(n), do: fac1(n,1)
  
  defp fac1(0, acc), do: acc
  defp fac1(n, acc), do: fac1(n-1, n*acc)
  
  def cat_r1(0), do: 1
  def cat_r1(n), do: Enum.sum(for i <- 0..n-1, do: cat_r1(i) * cat_r1(n-1-i))
  
  def cat_r2(0), do: 1
  def cat_r2(n), do: div(cat_r2(n-1) * 2 * (2*n - 1), n + 1)
  
  def test do
    range = 0..14
    :io.format "Directly:~n~p~n",            [(for n <- range, do: cat(n))]
    :io.format "1st recusive method:~n~p~n", [(for n <- range, do: cat_r1(n))]
    :io.format "2nd recusive method:~n~p~n", [(for n <- range, do: cat_r2(n))]
  end
end

Catalan.test
```


{{out}}

```txt

Directly:
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]
1st recusive method:
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]
2nd recusive method:
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]

```



## Erlang


```erlang
-module(catalan).

-export([test/0]).

cat(N) -> 
   factorial(2 * N) div (factorial(N+1) * factorial(N)).

factorial(N) -> 
   fac1(N,1).

fac1(0,Acc) -> 
   Acc; 
fac1(N,Acc) -> 
   fac1(N-1, N * Acc).
 
cat_r1(0) ->
   1;
cat_r1(N) ->
   lists:sum([cat_r1(I)*cat_r1(N-1-I) || I <- lists:seq(0,N-1)]).
   
cat_r2(0) ->
   1;
cat_r2(N) ->
   cat_r2(N - 1) * (2 * ((2 * N) - 1)) div (N + 1).

test() -> 
    TestList = lists:seq(0,14),
    io:format("Directly:\n~p\n",[[cat(N) || N <- TestList]]),
    io:format("1st recusive method:\n~p\n",[[cat_r1(N) || N <- TestList]]),
    io:format("2nd recusive method:\n~p\n",[[cat_r2(N) || N <- TestList]]).
```

{{out}}

```txt

Directly:
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]
1st recusive method:
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]
2nd recusive method:
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]

```



## ERRE

<lang>PROGRAM CATALAN

PROCEDURE CATALAN(N->RES)
   RES=1
   FOR I=1 TO N DO
      RES=RES*2*(2*I-1)/(I+1)
   END FOR
END PROCEDURE

BEGIN
   FOR N=0 TO 15 DO
      CATALAN(N->RES)    
      PRINT(N;"=";RES)
   END FOR
END PROGRAM

```

{{out}}

```txt

 0 = 1
 1 = 1
 2 = 2
 3 = 5
 4 = 14
 5 = 42
 6 = 132
 7 = 429
 8 = 1430
 9 = 4862
 10 = 16796
 11 = 58786
 12 = 208012
 13 = 742900
 14 = 2674440
 15 = 9694845

```




## Euphoria


```Euphoria
--Catalan number task from Rosetta Code wiki
--User:Lnettnay

--function from factorial task
function factorial(integer n)
atom f = 1
while n > 1 do
        f *= n
        n -= 1
end while

return f
end function

function catalan(integer n)  
atom numerator = factorial(2 * n)
atom denominator = factorial(n+1)*factorial(n)
return numerator/denominator
end function
  
for i = 0 to 15 do
        ? catalan(i)
end for
```

{{out}}

```txt

1                                                                                                                                                                             
1                                                                                                                                                                             
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845

```


=={{header|F_Sharp|F#}}==
<p>In the REPL (with 3rd equation):</p>

```txt
> Seq.unfold(fun (c,n) -> let cc = 2*(2*n-1)*c/(n+1) in Some(c,(cc,n+1))) (1,1) |> Seq.take 15 |> Seq.iter (printf "%i, ");;
1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, val it : unit = ()

```



## Factor

This is the last solution, memoized by using arrays. Run in scratchpad.

```factor
: next ( seq -- newseq )
  [ ] [ last ] [ length ] tri
  [ 2 * 1 - 2 * ] [ 1 + ] bi /
  * suffix ;
: Catalan ( n -- seq )  V{ 1 } swap 1 - [ next ] times ;
15 Catalan .
V{
    1
    1
    2
    5
    14
    42
    132
    429
    1430
    4862
    16796
    58786
    208012
    742900
    2674440
}
```



## Fantom

{{incorrect|Fantom|series starts 1, 1, 2, ...}}

```fantom
class Main
{
  static Int factorial (Int n)
  {
    Int res := 1
    if (n>1)
      (2..n).each |i| { res *= i }
    return res
  }

  static Int catalanA (Int n)
  { 
    return factorial(2*n)/(factorial(n+1) * factorial(n))
  }

  static Int catalanB (Int n)
  {
    if (n == 0)
    {
      return 1
    }
    else
    {
      sum := 0
      n.times |i| { sum += catalanB(i) * catalanB(n-1-i) } 
      return sum
    }
  }

  static Int catalanC (Int n)
  {
    if (n == 0)
    {
      return 1
    }
    else
    {
      return catalanC(n-1)*2*(2*n-1)/(n+1)
    }
  }

  public static Void main ()
  {
    (1..15).each |n|
    {
      echo (n.toStr.padl(4) + 
            catalanA(n).toStr.padl(10) +
            catalanB(n).toStr.padl(10) +
            catalanC(n).toStr.padl(10))
    }
  }
}
```

22! exceeds the range of Fantom's Int class, so the first technique fails afer n=10

```txt

   1         1         1         1
   2         2         2         2
   3         5         5         5
   4        14        14        14
   5        42        42        42
   6       132       132       132
   7       429       429       429
   8      1430      1430      1430
   9      4862      4862      4862
  10     16796     16796     16796
  11       -65     58786     58786
  12        -2    208012    208012
  13         0    742900    742900
  14        97   2674440   2674440
  15        -2   9694845   9694845

```



## Forth


```forth
: catalan ( n -- )  1 swap 1+ 1 do dup cr .  i 2* 1- 2*  i 1+ */ loop drop ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program main
  !
### =================================================================================

  implicit none

  !=== Local data
  integer                      :: n

  !=== External procedures
  double precision, external   :: catalan_numbers         
  
  !
###  Execution ======================================================================


  write(*,'(1x,a)')'
### =========
'
  write(*,'(5x,a,6x,a)')'n','c(n)'
  write(*,'(1x,a)')'---------------'

  do n = 0, 14
    write(*,'(1x,i5,i10)') n, int(catalan_numbers(n))
  enddo

  write(*,'(1x,a)')'
### =========
'

  !
### =================================================================================

end program main
!BL
!BL
!BL
double precision recursive function catalan_numbers(n) result(value)
  !
### =================================================================================

  implicit none

  !=== Input, ouput data
  integer, intent(in)          :: n

  !
###  Execution ======================================================================


  if ( n .eq. 0 ) then
    value = 1
  else 
    value = ( 2.0d0 * dfloat(2 * n - 1) / dfloat( n + 1 ) ) * catalan_numbers(n-1)
  endif

  !
### =================================================================================

end function catalan_numbers
```

{{out}}

```txt

 
### =========

     n      c(n)
 ---------------
     0         1
     1         1
     2         2
     3         5
     4        14
     5        42
     6       132
     7       429
     8      1430
     9      4862
    10     16796
    11     58786
    12    208012
    13    742900
    14   2674440
 
### =========


```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function factorial(n As UInteger) As UInteger
  If n = 0 Then Return 1
  Return n * factorial(n - 1)
End Function

Function catalan1(n As UInteger) As UInteger
  Dim prod As UInteger = 1
  For i As UInteger = n + 2 To 2 * n
     prod *= i
  Next
  Return prod / factorial(n)
End Function

Function catalan2(n As UInteger) As UInteger
  If n = 0 Then Return 1
  Dim sum As UInteger = 0
  For i As UInteger = 0 To n - 1
    sum += catalan2(i) * catalan2(n - 1 - i)
  Next
  Return sum
End Function

Function catalan3(n As UInteger) As UInteger
  If n = 0 Then Return 1
  Return catalan3(n - 1) * 2 * (2 * n - 1) \ (n + 1)
End Function 

Print "n", "First", "Second", "Third"
Print "-", "-----", "------", "-----"
Print
For i As UInteger = 0 To 15
  Print i,  catalan1(i), catalan2(i), catalan3(i)
Next
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

n             First         Second        Third
-             -----         ------        -----

0             1             1             1
1             1             1             1
2             2             2             2
3             5             5             5
4             14            14            14
5             42            42            42
6             132           132           132
7             429           429           429
8             1430          1430          1430
9             4862          4862          4862
10            16796         16796         16796
11            58786         58786         58786
12            208012        208012        208012
13            742900        742900        742900
14            2674440       2674440       2674440
15            9694845       9694845       9694845

```



## Frink

Frink includes efficient algorithms for calculating arbitrarily-large binomial coefficients and automatically caches factorials.

```frink
catalan[n] := binomial[2n,n]/(n+1)
for n = 0 to 15
   println[catalan[n]]
```



## FunL


```funl
import integers.choose
import util.TextTable

def
  catalan( n ) = choose( 2n, n )/(n + 1)

  catalan2( n ) = product( (n + k)/k | k <- 2..n )

  catalan3( 0 ) = 1
  catalan3( n ) = 2*(2n - 1)/(n + 1)*catalan3( n - 1 )

t = TextTable()
t.header( 'n', 'definition', 'product', 'recursive' )
t.line()

for i <- 1..4
  t.rightAlignment( i )

for i <- 0..15
  t.row( i, catalan(i), catalan2(i), catalan3(i) )
  
println( t )
```


{{out}}


```txt

+----+------------+---------+-----------+
| n  | definition | product | recursive |
+----+------------+---------+-----------+
|  0 |          1 |       1 |         1 |
|  1 |          1 |       1 |         1 |
|  2 |          2 |       2 |         2 |
|  3 |          5 |       5 |         5 |
|  4 |         14 |      14 |        14 |
|  5 |         42 |      42 |        42 |
|  6 |        132 |     132 |       132 |
|  7 |        429 |     429 |       429 |
|  8 |       1430 |    1430 |      1430 |
|  9 |       4862 |    4862 |      4862 |
| 10 |      16796 |   16796 |     16796 |
| 11 |      58786 |   58786 |     58786 |
| 12 |     208012 |  208012 |    208012 |
| 13 |     742900 |  742900 |    742900 |
| 14 |    2674440 | 2674440 |   2674440 |
| 15 |    9694845 | 9694845 |   9694845 |
+----+------------+---------+-----------+

```



## GAP


```gap
Catalan1 := n -> Binomial(2*n, n) - Binomial(2*n, n - 1);

Catalan2 := n -> Binomial(2*n, n)/(n + 1);

Catalan3 := function(n)
    local k, c;
    c := 1;
    k := 0;
    while k < n do
        k := k + 1;
        c := 2*(2*k - 1)*c/(k + 1);
    od;
    return c;
end;

Catalan4_memo := [1];
Catalan4 := function(n)
    if not IsBound(Catalan4_memo[n + 1]) then
        Catalan4_memo[n + 1] := Sum([0 .. n - 1], i -> Catalan4(i)*Catalan4(n - 1 - i));
    fi;
    return Catalan4_memo[n + 1];
end;


# The first fifteen: 0 to 14 !
List([0 .. 14], Catalan1);
List([0 .. 14], Catalan2);
List([0 .. 14], Catalan3);
List([0 .. 14], Catalan4);
# Same output for all four:
# [ 1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440 ]
```



## Go

Direct:

```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    var b, c big.Int
    for n := int64(0); n < 15; n++ {
        fmt.Println(c.Div(b.Binomial(n*2, n), c.SetInt64(n+1)))
    }
}
```

{{out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440

```



## Groovy


```Groovy

class Catalan
{
 public static void main(String[] args)
  {
    BigInteger N = 15;
    BigInteger k,n,num,den;
    BigInteger  catalan;
      print(1);
       for(n=2;n<=N;n++)
          {
            num = 1;
            den = 1;
              for(k=2;k<=n;k++)
                 {
                    num = num*(n+k);
                    den = den*k;
                    catalan = num/den; 
                 }
            println(catalan);
          }
    
  }
}

```

{{out}}

```txt

1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845

```


## Harbour


```visualfoxpro

PROCEDURE Main()
   LOCAL i

   FOR i := 0 to 15
      ? PadL( i, 2 ) + ": " + hb_StrFormat("%d", Catalan( i ))
   NEXT

   RETURN

STATIC FUNCTION Catalan( n )
   LOCAL i, nCatalan := 1

   FOR i := 1 TO n 
      nCatalan := nCatalan * 2 * (2 * i - 1) / (i + 1)
   NEXT

   RETURN nCatalan

```

{{out}}

```txt

0: 1       
1: 1       
2: 2       
3: 5       
4: 14      
5: 42      
6: 132     
7: 429     
8: 1430    
9: 4862    
0: 16796   
1: 58786   
2: 208012  
3: 742900  
4: 2674440 
5: 9694845 

```



## Haskell


```haskell
-- Three infinite lists, corresponding to the three definitions in the problem
-- statement.

cats1 :: [Integer]
cats1 = (\n -> product [n + 2 .. 2 * n] `div` product [1 .. n]) <$> [0 ..]

cats2 :: [Integer]
cats2 = 1 : fmap (\n -> sum $ zipWith (*) (reverse (take n cats2)) cats2) [1 ..]

cats3 :: [Integer]
cats3 = scanl (\c n -> c * 2 * (2 * n - 1) `div` (n + 1)) 1 [1 ..]

main :: IO ()
main = mapM_ (print . take 15) [cats1, cats2, cats3]
```

{{out}}

```txt
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]
[1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440]
```


=={{header|Icon}} and {{header|Unicon}}==
{{incorrect|Icon|series starts 1, 1, 2, ...}}

```Icon
procedure main(arglist)
every writes(catalan(i)," ")
end

procedure catalan(n) # return catalan(n) or fail
static M
initial M := table()

if n > 0 then 
   return (n = 1) | \M[n] | ( M[n] := (2*(2*n-1)*catalan(n-1))/(n+1))
end
```

{{out}}

```txt
1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440 9694845
```



## J


```j
   ((! +:) % >:) i.15x
1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440
```



## Java

Accuracy may be lost for larger n's due to floating-point arithmetic (seen for C(15) here). This implementation is memoized (factorial and Catalan numbers are stored in the Maps "facts", "catsI", "catsR1", and "catsR2"). [[Category:Memoization]]

```java5
import java.util.HashMap;
import java.util.Map;

public class Catalan {
	private static final Map<Long, Double> facts = new HashMap<Long, Double>();
	private static final Map<Long, Double> catsI = new HashMap<Long, Double>();
	private static final Map<Long, Double> catsR1 = new HashMap<Long, Double>();
	private static final Map<Long, Double> catsR2 = new HashMap<Long, Double>();
	
	static{//pre-load the memoization maps with some answers 
		facts.put(0L, 1D);
		facts.put(1L, 1D);
		facts.put(2L, 2D);
		
		catsI.put(0L, 1D);
		catsR1.put(0L, 1D);
		catsR2.put(0L, 1D);
	}

	private static double fact(long n){
		if(facts.containsKey(n)){
			return facts.get(n);
		}
		double fact = 1;
		for(long i = 2; i <= n; i++){
			fact *= i; //could be further optimized, but it would probably be ugly
		}
		facts.put(n, fact);
		return fact;
	}

	private static double catI(long n){
		if(!catsI.containsKey(n)){
			catsI.put(n, fact(2 * n)/(fact(n+1)*fact(n)));
		}
		return catsI.get(n);
	}
	
	private static double catR1(long n){
		if(catsR1.containsKey(n)){
			return catsR1.get(n);
		}
		double sum = 0;
		for(int i = 0; i < n; i++){
			sum += catR1(i) * catR1(n - 1 - i);
		}
		catsR1.put(n, sum);
		return sum;
	}
	
	private static double catR2(long n){
		if(!catsR2.containsKey(n)){
			catsR2.put(n, ((2.0*(2*(n-1) + 1))/(n + 1)) * catR2(n-1));
		}
		return catsR2.get(n);
	}
	
	public static void main(String[] args){
		for(int i = 0; i <= 15; i++){
			System.out.println(catI(i));
			System.out.println(catR1(i));
			System.out.println(catR2(i));
		}
	}
}
```

{{out}}

```txt
1.0
1.0
1.0
1.0
1.0
1.0
2.0
2.0
2.0
5.0
5.0
5.0
14.0
14.0
14.0
42.0
42.0
42.0
132.0
132.0
132.0
429.0
429.0
429.0
1430.0
1430.0
1430.0
4862.0
4862.0
4862.0
16796.0
16796.0
16796.0
58786.0
58786.0
58786.0
208012.0
208012.0
208012.0
742900.0
742900.0
742900.0
2674439.9999999995
2674440.0
2674440.0
9694844.999999998
9694845.0
9694845.0
```



## JavaScript


```javascript><html><head><title>Catalan</title></head

<body><pre id='x'>
```
<script type="application/javascript">
function disp(x) {
	var e = document.createTextNode(x + '\n');
	document.getElementById('x').appendChild(e);
}

var fc = [], c2 = [], c3 = [];
function fact(n) { return fc[n] ? fc[n] : fc[n] = (n ? n * fact(n - 1) : 1); }
function cata1(n) { return Math.floor(fact(2 * n) / fact(n + 1) / fact(n) + .5); }
function cata2(n) {
	if (n == 0) return 1;
	if (!c2[n]) {
		var s = 0;
		for (var i = 0; i < n; i++) s += cata2(i) * cata2(n - i - 1);
		c2[n] = s;
	}
	return c2[n];
}
function cata3(n) {
	if (n == 0) return 1;
	return c3[n] ? c3[n] : c3[n] = (4 * n - 2) * cata3(n - 1) / (n + 1);
}

disp("       meth1   meth2   meth3");
for (var i = 0; i <= 15; i++)
	disp(i + '\t' + cata1(i) + '\t' + cata2(i) + '\t' + cata3(i));

</script></body></html>
```

{{out}}

```txt
       meth1   meth2   meth3
0	1	1	1
1	1	1	1
2	2	2	2
3	5	5	5
4	14	14	14
5	42	42	42
6	132	132	132
7	429	429	429
8	1430	1430	1430
9	4862	4862	4862
10	16796	16796	16796
11	58786	58786	58786
12	208012	208012	208012
13	742900	742900	742900
14	2674440	2674440	2674440
15	9694845	9694845	9694845
```



## jq

{{ works with|jq|1.4 }}
The recursive formula for C(n) in terms of C(n-1) lends itself
directly to efficient implementations in jq so in this section,
that formula is used (a) to define a function for computing a single Catalan number; (b) to define a function for generating a sequence of Catalan numbers; and (c) to write a single expression for generating a sequence of Catalan numbers using jq's builtin "recurse/1" filter.

### = Compute a single Catalan number=


```jq
def catalan:
  if . == 0 then 1
  elif . < 0 then error("catalan is not defined on \(.)")
  else (2 * (2*. - 1) * ((. - 1) | catalan)) / (. + 1)
  end;
```

'''Example 1'''

```jq
(range(0; 16), 100) as $i | $i | catalan | [$i, .]
```

{{Out}}
<div style="overflow:scroll; height:150px;">

```sh
$ jq -M -n -c -f Catalan_numbers.jq
[0,1]
[1,1]
[2,2]
[3,5]
[4,14]
[5,42]
[6,132]
[7,429]
[8,1430]
[9,4862]
[10,16796]
[11,58786]
[12,208012]
[13,742900]
[14,2674440]
[15,9694845]
[100,8.96519947090131e+56]

```
</div>


### = Generate a sequence of Catalan numbers =


```jq
def catalan_series(max):
  def _catalan: # state: [n, catalan(n)]
    if .[0] > max then empty 
    else .,
      ((.[0] + 1) as $n | .[1] as $cp
       | [$n,  (2 * (2*$n - 1) * $cp) / ($n + 1) ] | _catalan)
    end;
  [0,1] | _catalan;

```

'''Example 2''':

```jq
catalan_series(15)
```

{{Out}}
 As above for 0 to 15.

### = An expression to generate Catalan numbers =


```jq

  [0,1]
  | recurse( if .[0] == 15 then empty
             else .[1] as $c | (.[0] + 1) | [ ., (2 * (2*. - 1) * $c) / (. + 1) ] 
             end )
```

{{out}}
 As above for 0 to 15.


## Julia

{{works with|Julia|0.6}}


```julia
catalannum(n::Integer) = binomial(2n, n) ÷ (n + 1)

@show catalannum.(1:15)
@show catalannum(big(100))
```


{{out}}

```txt
catalannum.(1:15) = [1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]
catalannum(big(100)) = 896519947090131496687170070074100632420837521538745909320
```


(In the second example, we have used arbitrary-precision integers to avoid overflow for large Catalan numbers.)


## K


```k
  catalan: {_{*/(x-i)%1+i:!y-1}[2*x;x+1]%x+1}
  catalan'!:15
1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440
```



## Kotlin

{{works with|Java|1.7.0}}
{{works with|Kotlin|1.1.4}}

```scala
abstract class Catalan {
    abstract operator fun invoke(n: Int) : Double

    protected val m = mutableMapOf(0 to 1.0)
}

object CatalanI : Catalan() {
    override fun invoke(n: Int): Double {
        if (n !in m)
            m[n] = Math.round(fact(2 * n) / (fact(n + 1) * fact(n))).toDouble()
        return m[n]!!
    }

    private fun fact(n: Int): Double {
        if (n in facts)
            return facts[n]!!
        val f = n * fact(n -1)
        facts[n] = f
        return f
    }

    private val facts = mutableMapOf(0 to 1.0, 1 to 1.0, 2 to 2.0)
}

object CatalanR1 : Catalan() {
    override fun invoke(n: Int): Double {
        if (n in m)
            return m[n]!!

        var sum = 0.0
        for (i in 0..n - 1)
            sum += invoke(i) * invoke(n - 1 - i)
        sum = Math.round(sum).toDouble()
        m[n] = sum
        return sum
    }
}

object CatalanR2 : Catalan() {
    override fun invoke(n: Int): Double {
        if (n !in m)
            m[n] = Math.round(2.0 * (2 * (n - 1) + 1) / (n + 1) * invoke(n - 1)).toDouble()
        return m[n]!!
    }
}

fun main(args: Array<String>) {
    val c = arrayOf(CatalanI, CatalanR1, CatalanR2)
    for(i in 0..15) {
        c.forEach { print("%9d".format(it(i).toLong())) }
        println()
    }
}
```

{{out}}

```txt
        1        1        1
        1        1        1
        2        2        2
        5        5        5
       14       14       14
       42       42       42
      132      132      132
      429      429      429
     1430     1430     1430
     4862     4862     4862
    16796    16796    16796
    58786    58786    58786
   208012   208012   208012
   742900   742900   742900
  2674440  2674440  2674440
  9694845  9694845  9694845
```



## Liberty BASIC


```lb
print "non-recursive version"
print catNonRec(5)
for i = 0 to 15
    print i;"   =   "; catNonRec(i)
next
print

print "recursive version"
print catRec(5)
for i = 0 to 15
    print i;"   =   "; catRec(i)
next
print

print "recursive with memoisation"
redim cats(20)  'clear the array
print catRecMemo(5)
for i = 0 to 15
    print i;"   =   "; catRecMemo(i)
next
print


wait

function catNonRec(n)   'non-recursive version
    catNonRec=1
    for i=1 to n
        catNonRec=((2*((2*i)-1))/(i+1))*catNonRec
    next
end function

function catRec(n)  'recursive version
    if n=0 then
        catRec=1
    else
        catRec=((2*((2*n)-1))/(n+1))*catRec(n-1)
    end if
end function

function catRecMemo(n)  'recursive version with memoisation
    if n=0 then
        catRecMemo=1
    else
        if cats(n-1)=0 then    'call it recursively only if not already calculated
            prev = catRecMemo(n-1)
        else
            prev = cats(n-1)
        end if
        catRecMemo=((2*((2*n)-1))/(n+1))*prev
    end if
    cats(n) = catRecMemo    'memoisation for future use
end function
```

{{out}}

```txt

non-recursive version
42
0   =   1
1   =   1
2   =   2
3   =   5
4   =   14
5   =   42
6   =   132
7   =   429
8   =   1430
9   =   4862
10   =   16796
11   =   58786
12   =   208012
13   =   742900
14   =   2674440
15   =   9694845

recursive version
42
0   =   1
1   =   1
2   =   2
3   =   5
4   =   14
5   =   42
6   =   132
7   =   429
8   =   1430
9   =   4862
10   =   16796
11   =   58786
12   =   208012
13   =   742900
14   =   2674440
15   =   9694845

recursive with memoisation
42
0   =   1
1   =   1
2   =   2
3   =   5
4   =   14
5   =   42
6   =   132
7   =   429
8   =   1430
9   =   4862
10   =   16796
11   =   58786
12   =   208012
13   =   742900
14   =   2674440
15   =   9694845
```



## Lua


```Lua
-- recursive with memoization
catalan = {[0] = 1}
setmetatable(catalan, {
	__index = function(c, n)
			c[n] = c[n-1]*2*(2*n-1)/(n+1)
			return c[n]
		end
	}
)

for i=0,14 do
	print(catalan[i])
end
```

{{out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
```


## Logo


```logo
to factorial :n
  output ifelse [less? :n 1] 1 [product :n factorial difference :n 1]
end
to choose :n :r
  output quotient factorial :n product factorial :r factorial difference :n :r
end
to catalan :n
  output product (quotient sum :n 1) choose product 2 :n :n
end

repeat 15 [print catalan repcount]
```

{{out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
```



## Maple


```Maple
CatalanNumbers := proc( n::posint )
    return seq( (2*i)!/((i + 1)!*i!), i = 0 .. n - 1 );
end proc:
CatalanNumbers(15);

```

Output:

```txt

1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
CatalanN[n_Integer /; n >= 0] := (2 n)!/((n + 1)! n!)
```

{{out|Sample Output}}

```Mathematica
TableForm[CatalanN/@Range[0,15]]
//TableForm= 
1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function n = catalanNumber(n)
    for i = (1:length(n))
        n(i) = (1/(n(i)+1))*nchoosek(2*n(i),n(i));
    end
end
```

The following version computes at the same time the n first Catalan numbers (including C0).

```MATLAB
function n = catalanNumbers(n)
    n = [1 cumprod((2:4:4*n-6) ./ (2:n))];
end
```

{{out|Sample Output}}

```MATLAB>>
 catalanNumber(14)

ans =

     2674440

>> catalanNumbers(18)'

ans =

           1
           1
           2
           5
          14
          42
         132
         429
        1430
        4862
       16796
       58786
      208012
      742900
     2674440
     9694845
    35357670
   129644790
```



## Maxima


```maxima
/* The following is an array function, hence the square brackets. It uses memoization automatically */
cata[n] := sum(cata[i]*cata[n - 1 - i], i, 0, n - 1)$
cata[0]: 1$

cata2(n) := binomial(2*n, n)/(n + 1)$

makelist(cata[n], n, 0, 14);

makelist(cata2(n), n, 0, 14);

/* both return [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440] */
```


=={{header|Modula-2}}==

```modula2
MODULE CatalanNumbers;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE binomial(m,n : LONGCARD) : LONGCARD;
VAR r,d : LONGCARD;
BEGIN
    r := 1;
    d := m - n;
    IF d>n THEN
        n := d;
        d := m - n;
    END;
    WHILE m>n DO
        r := r * m;
        DEC(m);
        WHILE (d>1) AND NOT (r MOD d # 0) DO
            r := r DIV d;
            DEC(d)
        END
    END;
    RETURN r
END binomial;

PROCEDURE catalan1(n : LONGCARD) : LONGCARD;
BEGIN
    RETURN binomial(2*n,n) DIV (1+n)
END catalan1;

PROCEDURE catalan2(n : LONGCARD) : LONGCARD;
VAR i,sum : LONGCARD;
BEGIN
    IF n>1 THEN
        sum := 0;
        FOR i:=0 TO n-1 DO
            sum := sum + catalan2(i) * catalan2(n - 1 - i)
        END;
        RETURN sum
    ELSE
        RETURN 1
    END
END catalan2;

PROCEDURE catalan3(n : LONGCARD) : LONGCARD;
BEGIN
    IF n#0 THEN
        RETURN 2  *(2 * n - 1) * catalan3(n - 1) DIV (1 + n)
    ELSE
        RETURN 1
    END
END catalan3;

VAR
    blah : LONGCARD = 123;
    buf : ARRAY[0..63] OF CHAR;
    i : LONGCARD;
BEGIN
    FormatString("\tdirect\tsumming\tfrac\n", buf);
    WriteString(buf);
    FOR i:=0 TO 15 DO
        FormatString("%u\t%u\t%u\t%u\n", buf, i, catalan1(i), catalan2(i), catalan3(i));
        WriteString(buf)
    END;
    ReadChar
END CatalanNumbers.
```



## Nim


```nim
import strutils

proc binomial(m, n): auto =
  result = 1
  var
    d = m - n
    n = n
    m = m
  if d > n:
    n = d

  while m > n:
    result *= m
    dec m
    while d > 1 and (result mod d) == 0:
      result = result div d
      dec d

proc catalan1(n): auto =
  binomial(2 * n, n) div (n + 1)

proc catalan2(n): auto =
  if n == 0:
    result = 1
  for i in 0 .. <n:
    result += catalan2(i) * catalan2(n - 1 - i)

proc catalan3(n): int =
  if n > 0: 2 * (2 * n - 1) * catalan3(n - 1) div (1 + n)
  else: 1

for i in 0..15:
  echo align($i, 7), " ", align($catalan1(i), 7), " ", align($catalan2(i), 7), " ", align($catalan3(i), 7)
```

Output:

```txt
      0       1       1       1
      1       1       1       1
      2       2       2       2
      3       5       5       5
      4      14      14      14
      5      42      42      42
      6     132     132     132
      7     429     429     429
      8    1430    1430    1430
      9    4862    4862    4862
     10   16796   16796   16796
     11   58786   58786   58786
     12  208012  208012  208012
     13  742900  742900  742900
     14 2674440 2674440 2674440
     15 9694845 9694845 9694845
```



## OCaml


```OCaml
let imp_catalan n =
  let return = ref 1 in
  for i = 1 to n do
    return := !return * 2 * (2 * i - 1) / (i + 1)
  done;
  !return

let rec catalan = function
  | 0 -> 1
  | n -> catalan (n - 1) * 2 * (2 * n - 1) / (n + 1)

let memoize f =
  let cache = Hashtbl.create 20 in
  fun n ->
    match Hashtbl.find_opt cache n with
    | None ->
      let x = f n in
      Hashtbl.replace cache n x;
      x
    | Some x -> x

let catalan_cache = Hashtbl.create 20

let rec memo_catalan n =
  if n = 0 then 1 else
    match Hashtbl.find_opt catalan_cache n with
    | None ->
      let x = memo_catalan (n - 1) * 2 * (2 * n - 1) / (n + 1) in
      Hashtbl.replace catalan_cache n x;
      x
    | Some x -> x

let () =
  if not !Sys.interactive then
    let bench label f n times =
      let start = Unix.gettimeofday () in
      begin
        for i = 1 to times do f n done;
        let stop = Unix.gettimeofday () in
        Printf.printf "%s (%d runs) : %.3f\n"
          label times (stop -. start)
      end in
    let show f g h f' n =
      for i = 0 to n do
        Printf.printf "%2d %7d %7d %7d %7d\n"
          i (f i) (g i) (h i) (f' i)
      done
    in
    List.iter (fun (l, f) -> bench l f 15 10_000_000)
      ["imperative", imp_catalan;
       "recursive", catalan;
       "hand-memoized", memo_catalan;
       "memoized", (memoize catalan)];
    show imp_catalan catalan memo_catalan (memoize catalan) 15

```

{{out}}

```txt
$ ocaml unix.cma catalan.ml
imperative (10000000 runs) : 3.420
recursive (10000000 runs) : 3.821
hand-memoized (10000000 runs) : 0.531
memoized (10000000 runs) : 0.515
 0       1       1       1       1
 1       1       1       1       1
 2       2       2       2       2
 3       5       5       5       5
 4      14      14      14      14
 5      42      42      42      42
 6     132     132     132     132
 7     429     429     429     429
 8    1430    1430    1430    1430
 9    4862    4862    4862    4862
10   16796   16796   16796   16796
11   58786   58786   58786   58786
12  208012  208012  208012  208012
13  742900  742900  742900  742900
14 2674440 2674440 2674440 2674440
15 9694845 9694845 9694845 9694845

$ ocamlopt -O2 unix.cmxa catalan.ml -o catalan
$ ./catalan
imperative (10000000 runs) : 2.020
recursive (10000000 runs) : 2.283
hand-memoized (10000000 runs) : 0.159
memoized (10000000 runs) : 0.167
...
```



## Oforth



```Oforth
: catalan( n -- m ) 
    n ifZero: [ 1 ] else: [ catalan( n 1- ) 2 n * 1- * 2 * n 1+ / ] ;
```


{{out}}

```txt

import: mapping
seqFrom(0, 15) map( #catalan ) .
[1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845]

```



## ooRexx

Three versions of this. 

```ooRexx
loop i = 0 to 15
    say "catI("i") =" .catalan~catI(i)
    say "catR1("i") =" .catalan~catR1(i)
    say "catR2("i") =" .catalan~catR2(i)
end

-- This is implemented as static members on a class object
-- so that the code is able to keep state information between calls.  This
-- memoization will speed up things like factorial calls by remembering previous
-- results.
::class catalan
-- initialize the class object
::method init class
  expose facts catI catR1 catR2
         facts = .table~new
         catI = .table~new
         catR1 = .table~new
         catR2 = .table~new
         -- seed a few items
         facts[0] = 1
         facts[1] = 1
         facts[2] = 2
         catI[0] = 1
         catR1[0] = 1
         catR2[0] = 1

-- private factorial method
::method fact private class
  expose facts
  use arg n
  -- see if we've calculated this before
  if facts~hasIndex(n) then return facts[n]
  numeric digits 120

  fact = 1
  loop i = 2 to n
      fact *= i
  end
  -- save this result
  facts[n] = fact
  return fact

::method catI class
  expose catI
  use arg n
  numeric digits 20

  res = catI[n]
  if res == .nil then do
      -- dividing by 1 removes insignificant trailing 0s
      res = (self~fact(2 * n)/(self~fact(n + 1) * self~fact(n))) / 1
      catI[n] = res
  end
  return res

::method catR1 class
  expose catR1
  use arg n
  numeric digits 20

  if catR1~hasIndex(n) then return catR1[n]
  sum = 0
  loop i = 0 to n - 1
      sum += self~catR1(i) * self~catR1(n - 1 - i)
  end
  -- remove insignificant trailing 0s
  sum = sum / 1
  catR1[n] = sum
  return sum

::method catR2 class
  expose catR2
  use arg n
  numeric digits 20

  res = catR2[n]
  if res == .nil then do
     res = ((2 * (2 * n - 1) * self~catR2(n - 1)) /  (n + 1))
     catR2[n] = res
  end
  return res
```

{{out}}

```txt
catI(0) = 1
catR1(0) = 1
catR2(0) = 1
catI(1) = 1
catR1(1) = 1
catR2(1) = 1
catI(2) = 2
catR1(2) = 2
catR2(2) = 2
catI(3) = 5
catR1(3) = 5
catR2(3) = 5
catI(4) = 14
catR1(4) = 14
catR2(4) = 14
catI(5) = 42
catR1(5) = 42
catR2(5) = 42
catI(6) = 132
catR1(6) = 132
catR2(6) = 132
catI(7) = 429
catR1(7) = 429
catR2(7) = 429
catI(8) = 1430
catR1(8) = 1430
catR2(8) = 1430
catI(9) = 4862
catR1(9) = 4862
catR2(9) = 4862
catI(10) = 16796
catR1(10) = 16796
catR2(10) = 16796
catI(11) = 58786
catR1(11) = 58786
catR2(11) = 58786
catI(12) = 208012
catR1(12) = 208012
catR2(12) = 208012
catI(13) = 742900
catR1(13) = 742900
catR2(13) = 742900
catI(14) = 2674440
catR1(14) = 2674440
catR2(14) = 2674440
catI(15) = 9694845
catR1(15) = 9694845
catR2(15) = 9694845
```



## PARI/GP

Memoization is not worthwhile; PARI has fast built-in facilities for calculating binomial coefficients and factorials.

```parigp
catalan(n)=binomial(2*n,n+1)/n
```

A second version:

```parigp
catalan(n)=(2*n)!/(n+1)!/n!
```

Naive version with binary splitting:

```parigp
catalan(n)=prod(k=n+2,2*n,k)/prod(k=2,n,k)
```

Naive version:

```parigp
catalan(n)={
  my(t=1);
  for(k=n+2,2*n,t*=k);
  for(k=2,n,t/=k);
  t
};
```

The first version takes about 1.5 seconds to compute the millionth Catalan number, while the second takes 3.9 seconds.  The naive implementations, for comparison, take 21 and 45 minutes.  In any case, printing the first 15 is simple:

```parigp
vector(15,n,catalan(n))
```



## Pascal


```pascal
Program CatalanNumbers(output);

function catalanNumber1(n: integer): double;
  begin
    if n = 0 then
      catalanNumber1 := 1.0
    else 
      catalanNumber1 := double(4 * n - 2) / double(n + 1) * catalanNumber1(n-1);
  end;
 
var
  number: integer;

begin
  writeln('Catalan Numbers');
  writeln('Recursion with a fraction:');
  for number := 0 to 14 do
    writeln (number:3, round(catalanNumber1(number)):9);
end.
```

{{out}}

```txt

:> ./CatalanNumbers
Catalan Numbers
Recursion with a fraction:
  0        1
  1        1
  2        2
  3        5
  4       14
  5       42
  6      132
  7      429
  8     1430
  9     4862
 10    16796
 11    58786
 12   208012
 13   742900
 14  2674440

```



## Perl


```perl
sub factorial { my $f = 1; $f *= $_ for 2 .. $_[0]; $f; }
sub catalan {
  my $n = shift;
  factorial(2*$n) / factorial($n+1) / factorial($n);
}

print "$_\t@{[ catalan($_) ]}\n" for 0 .. 20;
```

For computing up to 20 ish, memoization is not needed.  For much bigger numbers, this is faster:

```perl
my @c = (1);
sub catalan { 
        use bigint;
        $c[$_[0]] //= catalan($_[0]-1) * (4 * $_[0]-2) / ($_[0]+1)
}

# most of the time is spent displaying the long numbers, actually
print "$_\t", catalan($_), "\n" for 0 .. 10000;
```


That has two downsides: high memory use and slow access to an isolated large value.  Using a fast binomial function can solve both these issues.  The downside here is if the platform doesn't have the GMP library then binomials won't be fast.
{{libheader|ntheory}}

```perl
use ntheory qw/binomial/;
sub catalan {
  my $n = shift;
  binomial(2*$n,$n)/($n+1);
}
print "$_\t", catalan($_), "\n" for 0 .. 10000;
```



## Perl 6

{{works with|Rakudo|2015.12}}
The recursive formulas are easily written into a constant array, either:


```perl6
constant Catalan = 1, { [+] @_ Z* @_.reverse } ... *;
```


or


```perl6
constant Catalan = 1, |[\*] (2, 6 ... *) Z/ 2 .. *;


# In both cases, the sixteen first values can be seen with:
.say for Catalan[^16];
```

{{out}}

```txt
1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
```



## Phix

See also [[Catalan_numbers/Pascal%27s_triangle#Phix]] which may be faster.

```Phix
-- returns inf/-nan for n>85, and needs the rounding for n>=14, accurate to n=29
function catalan1(integer n)
    return floor(factorial(2*n)/(factorial(n+1)*factorial(n))+0.5)
end function
 
-- returns inf for n>519, accurate to n=30:
function catalan2(integer n) -- NB: very slow!
atom res = not n
    n -= 1
    for i=0 to n do
        res += catalan2(i)*catalan2(n-i)
    end for
    return res
end function
 
-- returns inf for n>514, accurate to n=30:
function catalan3(integer n)
    if n=0 then return 1 end if
    return 2*(2*n-1)/(1+n)*catalan3(n-1)
end function 
 
sequence res = repeat(repeat(0,4),16),
         times = repeat(0,3)
for t=1 to 4 do
    atom t0 = time()
    for i=0 to 15 do
        switch t do
            case 1: res[i+1][2] = catalan1(i)
            case 2: res[i+1][3] = catalan2(i)
            case 3: res[i+1][4] = catalan3(i)
            case 4: res[i+1][1] = i; printf(1,"%2d: %10d %10d %10d\n",res[i+1])
        end switch
    end for
    if t=4 then exit end if
    times[t] = elapsed(time()-t0)
end for
printf(1,"times:%8s %10s %10s\n",times)
```

{{out}} 

```txt

 0:          1          1          1
 1:          1          1          1
 2:          2          2          2
 3:          5          5          5
 4:         14         14         14
 5:         42         42         42
 6:        132        132        132
 7:        429        429        429
 8:       1430       1430       1430
 9:       4862       4862       4862
10:      16796      16796      16796
11:      58786      58786      58786
12:     208012     208012     208012
13:     742900     742900     742900
14:    2674440    2674440    2674440
15:    9694845    9694845    9694845
times:      0s       1.6s         0s

```

As expected, catalan2() is by far the slowest, so let's memoise that one!


###  memoized recursive gmp version 

{{libheader|mpfr}}

```Phix
include builtins\mpfr.e
 
sequence c2cache = {}
 
function catalan2m(integer n)   -- very fast!
object r -- result (a [cached/shared] mpz)
         -- (nb: modifying result will mess up cache)
    if n<=0 then return mpz_init(1) end if
    if n<=length(c2cache) then
        r = c2cache[n]
        if r!=0 then return r end if
    else
        c2cache &= repeat(0,n-length(c2cache))
    end if
    r = mpz_init(0)
    mpz t = mpz_init()
    for i=0 to n-1 do
        mpz_mul(t,catalan2m(i),catalan2m(n-1-i))
        mpz_add(r,r,t)
    end for
    t = mpz_free(t)
    c2cache[n] = r
    return r
end function
 
sequence s = {}
for i=0 to 15 do s = append(s,mpz_get_str(catalan2m(i))) end for
printf(1,"0..15: %s\n",join(s,","))
printf(1,"100: %s\n",{mpz_get_str(catalan2m(100))})
```

{{out}}

```txt

0..15: 1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440,9694845
100: 896519947090131496687170070074100632420837521538745909320

```



## PHP


```php
<?php

class CatalanNumbersSerie
{
  private static $cache = array(0 => 1);
   
  private function fill_cache($i)
  {
    $accum = 0;
    $n = $i-1;
    for($k = 0; $k <= $n; $k++)
    {
      $accum += $this->item($k)*$this->item($n-$k);
    } 
    self::$cache[$i] = $accum;
  }
  function item($i)
  {
    if (!isset(self::$cache[$i]))
    {
      $this->fill_cache($i);
    }
    return self::$cache[$i];
  }
}

$cn = new CatalanNumbersSerie();
for($i = 0; $i <= 15;$i++)
{
  $r = $cn->item($i);
  echo "$i = $r\r\n";
}
?>
```

{{out}}

```txt

0 = 1
1 = 1                                                                                                                                         
2 = 2                                                                                                                                         
3 = 5                                                                                                                                         
4 = 14                                                                                                                                        
5 = 42                                                                                                                                        
6 = 132                                                                                                                                       
7 = 429                                                                                                                                       
8 = 1430                                                                                                                                      
9 = 4862                                                                                                                                      
10 = 16796                                                                                                                                    
11 = 58786                                                                                                                                    
12 = 208012                                                                                                                                   
13 = 742900                                                                                                                                   
14 = 2674440                                                                                                                                  
15 = 9694845 

```


```php

<?php 
$n = 15;
$t[1] = 1;
foreach (range(1, $n+1) as $i) {
    foreach (range($i, 1-1) as $j) {
        $t[$j] += $t[$j - 1];
    }
    $t[$i +1] = $t[$i];
    foreach (range($i+1, 1-1) as $j) {
        $t[$j] += $t[$j -1];
    }
    print ($t[$i+1]-$t[$i])."\t";
}

```

{{out}}

```txt

1	2	5	14	42	132	429	1430	4862	16796	58786	208012	742900	2674440	9694845	35357670

```



## PicoLisp


```PicoLisp
# Factorial
(de fact (N)
   (if (=0 N)
      1
      (* N (fact (dec N))) ) )

# Directly
(de catalanDir (N)
   (/ (fact (* 2 N)) (fact (inc N)) (fact N)) )

# Recursively
(de catalanRec (N)
   (if (=0 N)
      1
      (cache '(NIL) N  # Memoize
         (sum
            '((I) (* (catalanRec I) (catalanRec (- N I 1))))
            (range 0 (dec N)) ) ) ) )

# Alternatively
(de catalanAlt (N)
   (if (=0 N)
      1
      (*/ 2 (dec (* 2 N)) (catalanAlt (dec N)) (inc N)) ) )

# Test
(for (N 0 (> 15 N) (inc N))
   (tab (2 4 8 8 8)
      N
      " => "
      (catalanDir N)
      (catalanRec N)
      (catalanAlt N) ) )
```

{{out}}

```txt
 0 =>        1       1       1
 1 =>        1       1       1
 2 =>        2       2       2
 3 =>        5       5       5
 4 =>       14      14      14
 5 =>       42      42      42
 6 =>      132     132     132
 7 =>      429     429     429
 8 =>     1430    1430    1430
 9 =>     4862    4862    4862
10 =>    16796   16796   16796
11 =>    58786   58786   58786
12 =>   208012  208012  208012
13 =>   742900  742900  742900
14 =>  2674440 2674440 2674440
```



## PL/I


```PL/I
catalan: procedure options (main);   /* 23 February 2012 */
   declare (i, n) fixed;

   put skip list ('How many catalan numbers do you want?');
   get list (n);

   do i = 0 to n;
      put skip list (c(i));
   end;

c: procedure (n) recursive returns (fixed decimal (15));
   declare n fixed;

   if n <= 1 then return (1);

   return ( 2*(2*n-1) * c(n-1) / (n + 1) );
end c;

end catalan;
```

{{out}}

```txt

How many catalan numbers do you want? 

                 1 
                 1 
                 2 
                 5 
                14 
                42 
               132 
               429 
              1430 
              4862 
             16796 
             58786 
            208012 
            742900 
           2674440 
           9694845 
          35357670 
         129644790 
         477638700 
        1767263190 
        6564120420 

```



## PlainTeX


```tex
\newcount\n
\newcount\r
\newcount\x
\newcount\ii

\def\catalan#1{%
	\n#1\advance\n by1\ii1\r1%
	\loop{%
		\x\ii%
		\multiply\x by 2 \advance\x by -1 \multiply\x by 2%
		\global\multiply\r by\x%
		\global\advance\ii by1%
		\global\divide\r by\ii%
	} \ifnum\number\ii<\n\repeat%
	\the\r
}

\rightskip=0pt plus1fil\parindent=0pt
\loop{${\rm Catalan}(\the\x) = \catalan{\the\x}$\hfil\break}%
	\advance\x by 1\ifnum\x<15\repeat

\bye
```



## PowerShell


```PowerShell

function Catalan([uint64]$m) {
    function fact([bigint]$n) {
        if($n -lt 2) {[bigint]::one}
        else{2..$n | foreach -Begin {$prod = [bigint]::one} -Process {$prod = [bigint]::Multiply($prod,$_)} -End {$prod}}
    }
    $fact = fact $m
    $fact1 = [bigint]::Multiply($m+1,$fact)
    [bigint]::divide((fact (2*$m)), [bigint]::Multiply($fact,$fact1))
}
0..15 | foreach {"catalan($_): $(catalan $_)"}

```

<b>Output:</b>

```txt

catalan(0): 1
catalan(1): 1
catalan(2): 2
catalan(3): 5
catalan(4): 14
catalan(5): 42
catalan(6): 132
catalan(7): 429
catalan(8): 1430
catalan(9): 4862
catalan(10): 16796
catalan(11): 58786
catalan(12): 208012
catalan(13): 742900
catalan(14): 2674440
catalan(15): 9694845

```



### An Alternate Version

This version could easily be modified to work with big integers.

```PowerShell

function Get-CatalanNumber
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [uint32[]]
        $InputObject
    )

    Begin
    {
        function Get-Factorial ([int]$Number)
        {
            if ($Number -eq 0)
            {
                return 1
            }

            $factorial = 1

            1..$Number | ForEach-Object {$factorial *= $_}

            $factorial
        }    

        function Get-Catalan ([int]$Number)
        {
            if ($Number -eq 0)
            {
                return 1
            }

            (Get-Factorial (2 * $Number)) / ((Get-Factorial (1 + $Number)) * (Get-Factorial $Number))
        }
    }
    Process
    {
        foreach ($number in $InputObject)
        {
            [PSCustomObject]@{
                Number        = $number
                CatalanNumber = Get-Catalan $number
            }
        }
    }
}

```

Get the first fifteen Catalan numbers as a PSCustomObject:

```PowerShell

0..14 | Get-CatalanNumber

```

{{Out}}

```txt

Number CatalanNumber
------ -------------
     0             1
     1             1
     2             2
     3             5
     4            14
     5            42
     6           132
     7           429
     8          1430
     9          4862
    10         16796
    11         58786
    12        208012
    13        742900
    14       2674440

```

To return only the array of Catalan numbers:

```PowerShell

(0..14 | Get-CatalanNumber).CatalanNumber

```

{{Out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440

```



## Prolog

{{Works with|SWI-Prolog}}

```Prolog
catalan(N) :-
	length(L1, N),
	L = [1 | L1],
	init(1,1,L1),
	numlist(0, N, NL),
	maplist(my_write, NL, L).


init(_, _, []).

init(V, N, [H | T]) :-
	N1 is N+1,
	H is 2 * (2 * N - 1) * V / N1,
	init(H, N1, T).

my_write(N, V) :-
	format('~w : ~w~n', [N, V]).
```

{{out}}

```txt
 ?- catalan(15).
0 : 1
1 : 1
2 : 2
3 : 5
4 : 14
5 : 42
6 : 132
7 : 429
8 : 1430
9 : 4862
10 : 16796
11 : 58786
12 : 208012
13 : 742900
14 : 2674440
15 : 9694845
true .

```



## PureBasic

Using the third formula...

```PureBasic
; saving the division for last ensures we divide the largest
; numerator by the smallest denominator

Procedure.q CatalanNumber(n.q)
If n<0:ProcedureReturn 0:EndIf
If n=0:ProcedureReturn 1:EndIf
ProcedureReturn (2*(2*n-1))*CatalanNumber(n-1)/(n+1)
EndProcedure

ls=25
rs=12

a.s=""
a.s+LSet(RSet("n",rs),ls)+"CatalanNumber(n)"
; cw(a.s)
Debug a.s

For n=0 to 33 ;33 largest correct quad for n 
a.s=""
a.s+LSet(RSet(Str(n),rs),ls)+Str(CatalanNumber(n))
; cw(a.s)
Debug a.s
Next
```

{{out|Sample Output}}

```txt

           n             CatalanNumber(n)
           0             1
           1             1
           2             2
           3             5
           4             14
           5             42
           6             132
           7             429
           8             1430
           9             4862
          10             16796
          11             58786
          12             208012
          13             742900
          14             2674440
          15             9694845
          16             35357670
          17             129644790
          18             477638700
          19             1767263190
          20             6564120420
          21             24466267020
          22             91482563640
          23             343059613650
          24             1289904147324
          25             4861946401452
          26             18367353072152
          27             69533550916004
          28             263747951750360
          29             1002242216651368
          30             3814986502092304
          31             14544636039226909
          32             55534064877048198
          33             212336130412243110

```



## Python

Three algorithms including explicit memoization. (Pythons [http://svn.python.org/view/python/branches/release31-maint/Modules/mathmodule.c?revision=82224&view=markup factorial built-in function] is not memoized internally).

Python will transparently switch to bignum-type integer arithmetic, so the code below works unchanged on computing larger catalan numbers such as cat(50) and beyond.

{{Works with|Python|3}}

```python
from math import factorial
import functools


def memoize(func):
    cache = {}

    def memoized(key):
        # Returned, new, memoized version of decorated function
        if key not in cache:
            cache[key] = func(key)
        return cache[key]
    return functools.update_wrapper(memoized, func)


@memoize
def fact(n):
    return factorial(n)


def cat_direct(n):
    return fact(2 * n) // fact(n + 1) // fact(n)


@memoize
def catR1(n):
    return 1 if n == 0 else (
        sum(catR1(i) * catR1(n - 1 - i) for i in range(n))
    )


@memoize
def catR2(n):
    return 1 if n == 0 else (
        ((4 * n - 2) * catR2(n - 1)) // (n + 1)
    )


if __name__ == '__main__':
    def pr(results):
        fmt = '%-10s %-10s %-10s'
        print((fmt % tuple(c.__name__ for c in defs)).upper())
        print(fmt % (('=' * 10,) * 3))
        for r in zip(*results):
            print(fmt % r)

    defs = (cat_direct, catR1, catR2)
    results = [tuple(c(i) for i in range(15)) for c in defs]
    pr(results)
```

{{out|Sample Output}}

```txt
CAT_DIRECT CATR1      CATR2     

### ======= ========== =======

1          1          1         
1          1          1         
2          2          2         
5          5          5         
14         14         14        
42         42         42        
132        132        132       
429        429        429       
1430       1430       1430      
4862       4862       4862      
16796      16796      16796     
58786      58786      58786     
208012     208012     208012    
742900     742900     742900    
2674440    2674440    2674440
```



## R


```r
catalan <- function(n) choose(2*n, n)/(n + 1)
catalan(0:15)
 [1]       1       1       2       5      14      42     132     429    1430
[10]    4862   16796   58786  208012  742900 2674440 9694845
```



## Racket


```racket
#lang racket
(require planet2)
; (install "this-and-that")  ; uncomment to install
(require memoize/memo)

(define/memo* (catalan m)
  (if (= m 0) 
      1
      (for/sum ([i m]) 
        (* (catalan i) (catalan (- m i 1))))))
      
(map catalan (range 1 15))
```

{{out}}

```txt

'(1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440)

```



## REXX


### version 1

All four methods of calculate the Catalan numbers use independent memoization
for the computation of factorials.

In the 1<sup>st</sup> equation, the 2<sup>nd</sup> version's denominator:
:::::::: <big> (n+1)! n! </big>
has been rearranged to:
:::::::: <big> (n+1) * [fact(n) **2] </big>

```rexx
/*REXX program calculates and displays  Catalan numbers  using  four different methods. */
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then do;  HI=15; LO=0;  end /*No args? Then use a range of 0 ──► 15*/
if HI=='' | HI==","  then      HI=LO             /*No HI?   Then use  LO for the default*/
numeric digits max(20, 5*HI)                     /*this allows gihugic Catalan numbers. */
w=length(HI)                                     /*W:  is used for aligning the output. */
call hdr 1A;  do j=LO  to HI;  say '     Catalan'     right(j, w)": "      Cat1A(j);   end
call hdr 1B;  do j=LO  to HI;  say '     Catalan'     right(j, w)": "      Cat1B(j);   end
call hdr 2 ;  do j=LO  to HI;  say '     Catalan'     right(j, w)": "      Cat2(j) ;   end
call hdr 3 ;  do j=LO  to HI;  say '     Catalan'     right(j, w)": "      Cat3(j) ;   end
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:     arg z; if !.z\==. then return !.z; !=1;  do k=2  to z; !=!*k; end;  !.z=!; return !
Cat1A: procedure expose !.;  parse arg n;     return comb(n+n, n)    %  (n+1)
Cat1B: procedure expose !.;  parse arg n;     return !(n+n) % ((n+1) * !(n)**2)
Cat3:  procedure expose c.;  arg n; if c.n==. then c.n=(4*n-2)*cat3(n-1)%(n+1); return c.n
comb:  procedure;            parse arg x,y;   return pFact(x-y+1, x) % pFact(2, y)
hdr:   !.=.; c.=.; c.0=1; say; say center('Catalan numbers, method' arg(1),79,'─'); return
pFact: procedure;            !=1;      do k=arg(1)  to arg(2);  !=!*k;  end;    return !
/*──────────────────────────────────────────────────────────────────────────────────────*/
Cat2:  procedure expose c.;  parse arg n;  $=0;         if c.n\==.  then return c.n
                                       do k=0  for n;   $=$ + Cat2(k) * Cat2(n-k-1);   end
                             c.n=$;           return $    /*use a memoization technique.*/
```

'''output'''   when using the input of:   <tt> 0   16 </tt>

```txt

───────────────────────── Catalan numbers, method 1A ──────────────────────────
     Catalan  0:  1
     Catalan  1:  1
     Catalan  2:  2
     Catalan  3:  5
     Catalan  4:  14
     Catalan  5:  42
     Catalan  6:  132
     Catalan  7:  429
     Catalan  8:  1430
     Catalan  9:  4862
     Catalan 10:  16796
     Catalan 11:  58786
     Catalan 12:  208012
     Catalan 13:  742900
     Catalan 14:  2674440
     Catalan 15:  9694845 

───────────────────────── Catalan numbers, method 1B ──────────────────────────
···  (elided, same as first method) ··· 

───────────────────────── Catalan numbers, method 2  ──────────────────────────
···  (elided, same as first method) ···

───────────────────────── Catalan numbers, method 3  ──────────────────────────
···  (elided, same as first method) ···

```

'''Timing notes'''   of the four methods:

::* For Catalan numbers   1 ──► 200:
::::* method   '''1A'''   is about   50 times slower than method   '''3'''
::::* method   '''1B'''   is about 100 times slower than method   '''3'''
::::* method   '''2'''     is about   85 times slower than method   '''3'''

::* For Catalan numbers   1 ──► 300: 
::::* method   '''1A'''   is about 100 times slower than method   '''3'''
::::* method   '''1B'''   is about 200 times slower than method   '''3'''
::::* method   '''2'''     is about 200 times slower than method   '''3'''
Method   '''3'''   is really quite fast;   even in the thousands range, computation time is still quite reasonable.


### version 2

Implements the 3 methods shown in the task description

```rexx
/* REXX ---------------------------------------------------------------
* 01.07.2014 Walter Pachl
*--------------------------------------------------------------------*/
Numeric Digits 1000
Parse Arg m .
If m='' Then m=20
Do i=0 To m
  c1.i=c1(i)
  End
c2.=1
Do i=1 To m
  c2.i=c2(i)
  End
c3.=1
Do i=1 To m
  im1=i-1
  c3.i=2*(2*i-1)*c3.im1/(i+1)
  End
l=length(c3.m)
hdr=' n' right('c1.n',l),
         right('c2.n',l),
         right('c3.n',l)
Say hdr
Do i=0 To m
  Say right(i,2) format(c1.i,l),
                 format(c2.i,l),
                 format(c3.i,l)
  End
Say hdr
Exit

c1: Procedure
Parse Arg n
return fact(2*n)/(fact(n)*fact(n+1))

c2: Procedure Expose c2.
Parse Arg n
res=0
Do i=0 To n-1
  nmi=n-i-1
  res=res+c2.i*c2.nmi
  End
Return res

fact: Procedure
Parse Arg n
f=1
Do i=1 To n
  f=f*i
  End
Return f
```

{{out}}

```txt
 n       c1.n       c2.n       c3.n
 0          1          1          1
 1          1          1          1
 2          2          2          2
 3          5          5          5
 4         14         14         14
 5         42         42         42
 6        132        132        132
 7        429        429        429
 8       1430       1430       1430
 9       4862       4862       4862
10      16796      16796      16796
11      58786      58786      58786
12     208012     208012     208012
13     742900     742900     742900
14    2674440    2674440    2674440
15    9694845    9694845    9694845
16   35357670   35357670   35357670
17  129644790  129644790  129644790
18  477638700  477638700  477638700
19 1767263190 1767263190 1767263190
20 6564120420 6564120420 6564120420
 n       c1.n       c2.n       c3.n
```



## Ring


```ring

for n = 1 to 15
    see catalan(n) + nl
next
 
func catalan n
     if n = 0 return 1 ok
     cat = 2 * (2 * n - 1) * catalan(n - 1) / (n + 1)
     return cat

```

Output:

```txt

1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845

```



## Ruby

{{libheader|RubyGems}}

```ruby
def factorial(n)
  (1..n).reduce(1, :*)
end

# direct

def catalan_direct(n)
  factorial(2*n) / (factorial(n+1) * factorial(n))
end

# recursive

def catalan_rec1(n)
  return 1 if n == 0
  (0...n).inject(0) {|sum, i| sum + catalan_rec1(i) * catalan_rec1(n-1-i)}
end

def catalan_rec2(n)
  return 1 if n == 0
  2*(2*n - 1) * catalan_rec2(n-1) / (n+1)
end

# performance and results

require 'benchmark'
require 'memoize'
include Memoize

Benchmark.bm(17) do |b|
  b.report('catalan_direct')    {16.times {|n| catalan_direct(n)} }
  b.report('catalan_rec1')      {16.times {|n| catalan_rec1(n)} }
  b.report('catalan_rec2')      {16.times {|n| catalan_rec2(n)} }
  
  memoize :catalan_rec1
  b.report('catalan_rec1(memo)'){16.times {|n| catalan_rec1(n)} }
end

puts "\n       direct     rec1     rec2"
16.times {|n| puts "%2d :%9d%9d%9d" % [n, catalan_direct(n), catalan_rec1(n), catalan_rec2(n)]}
```

The output shows the dramatic difference memoizing makes.

```txt

                        user     system      total        real
catalan_direct      0.000000   0.000000   0.000000 (  0.000124)
catalan_rec1        6.178000   0.000000   6.178000 (  6.195141)
catalan_rec2        0.000000   0.000000   0.000000 (  0.000023)
catalan_rec1(memo)  0.000000   0.000000   0.000000 (  0.000641)

       direct     rec1     rec2
 0 :        1        1        1
 1 :        1        1        1
 2 :        2        2        2
 3 :        5        5        5
 4 :       14       14       14
 5 :       42       42       42
 6 :      132      132      132
 7 :      429      429      429
 8 :     1430     1430     1430
 9 :     4862     4862     4862
10 :    16796    16796    16796
11 :    58786    58786    58786
12 :   208012   208012   208012
13 :   742900   742900   742900
14 :  2674440  2674440  2674440
15 :  9694845  9694845  9694845

```



## Run BASIC


```Runbasic
FOR i = 1 TO 15
    PRINT i;" ";catalan(i)
NEXT
 
FUNCTION catalan(n) 
 catalan = 1
 if n <> 0 then catalan = ((2 * ((2 * n) - 1)) / (n + 1)) * catalan(n - 1)
END FUNCTION
```


```txt
1 1
2 2
3 5
4 14
5 42
6 132
7 429
8 1430
9 4862
10 16796
11 58786
12 208012
13 742900
14 2674440
15 9694845
```



## Rust


```rust
fn c_n(n: u64) -> u64 {
    match n {
        0 => 1,
        _ => c_n(n - 1) * 2 * (2 * n - 1) / (n + 1)
    }
}

fn main() {
    for i in 1..16 {
        println!("c_n({}) = {}", i, c_n(i));
    }
}
```


{{out}}


```txt
c(1) = 1
c(2) = 2
c(3) = 5
c(4) = 14
c(5) = 42
c(6) = 132
c(7) = 429
c(8) = 1430
c(9) = 4862
c(10) = 16796
c(11) = 58786
c(12) = 208012
c(13) = 742900
c(14) = 2674440
c(15) = 9694845
```



## Scala


Simple and straightforward. Noticeably out of steam without memoizing at about 5000.

```scala
object Catalan {
  def factorial(n: BigInt) = BigInt(1).to(n).foldLeft(BigInt(1))(_ * _)
  def catalan(n: BigInt) = factorial(2 * n) / (factorial(n + 1) * factorial(n))

  def main(args: Array[String]) {
    for (n <- 0 to 15) {
      println("catalan(" + n + ") = " + catalan(n))
    }
  }
}
```

{{out}}

```txt

catalan(0) = 1
catalan(1) = 1
catalan(2) = 2
catalan(3) = 5
catalan(4) = 14
catalan(5) = 42
catalan(6) = 132
catalan(7) = 429
catalan(8) = 1430
catalan(9) = 4862
catalan(10) = 16796
catalan(11) = 58786
catalan(12) = 208012
catalan(13) = 742900
catalan(14) = 2674440
catalan(15) = 9694845
```



## Scheme

Tail recursive implementation.

```scheme
(define (catalan m)
    (let loop ((c 1)(n 0))
        (if (not (eqv? n m))
            (begin
                (display n)(display ": ")(display c)(newline)
                (loop (* (/ (* 2 (- (* 2 (+ n 1)) 1)) (+ (+ n 1) 1)) c) (+ n 1) )))))

(catalan 15)
```

{{out}}

```txt
0: 1
1: 1
2: 2
3: 5
4: 14
5: 42
6: 132
7: 429
8: 1430
9: 4862
10: 16796
11: 58786
12: 208012
13: 742900
14: 2674440
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const proc: main is func
  local
    var bigInteger: n is 0_;
  begin
    for n range 0_ to 15_ do
      writeln((2_ * n) ! n div succ(n));
    end for;
  end func;
```

{{out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440
9694845

```



## Sidef


```ruby
func f(i) { i==0 ? 1 : (i * f(i-1)) }
func c(n) { f(2*n) / f(n) / f(n+1) }
```

With memoization:

```ruby
func c(n) is cached {
    n == 0 ? 1 : (c(n-1) * (4 * n - 2) / (n + 1))
}
```


Calling the function:

```ruby
15.times { |i|
    say "#{i}\t#{c(i)}"
}
```

{{out}}

```txt

0	1
1	1
2	2
3	5
4	14
5	42
6	132
7	429
8	1430
9	4862
10	16796
11	58786
12	208012
13	742900
14	2674440

```



## smart BASIC



```qbasic
PRINT "Recursive:"!PRINT
FOR n = 0 TO 15
    PRINT n,"#######":catrec(n)
NEXT n
PRINT!PRINT

PRINT "Non-recursive:"!PRINT
FOR n = 0 TO 15
    PRINT n,"#######":catnonrec(n)
NEXT n

END
 
DEF catrec(x)
    IF x = 0 THEN
        temp = 1
    ELSE 
        n = x
        temp = ((2*((2*n)-1))/(n+1))*catrec(n-1)
    END IF
    catrec = temp
END DEF

DEF catnonrec(x)
    temp = 1
    FOR n = 1 TO x
         temp = (2*((2*n)-1))/(n+1)*temp
    NEXT n
    catnonrec = temp
END DEF
```



## Standard ML


```sml
(*
 * val catalan : int -> int
 * Returns the nth Catalan number.
 *)
fun catalan 0 = 1
|   catalan n = ((4 * n - 2) * catalan(n - 1)) div (n + 1);

(*
 * val print_catalans : int -> unit
 * Prints out Catalan numbers 0 through 15.
 *)
fun print_catalans(n) =
    if n > 15 then ()
    else (print (Int.toString(catalan n) ^ "\n"); print_catalans(n + 1)); print_catalans(0);
(*
 * 1
 * 1
 * 2
 * 5
 * 14
 * 42
 * 132
 * 429
 * 1430
 * 4862
 * 16796
 * 58786
 * 208012
 * 742900
 * 2674440
 * 9694845
 *)
```



## Stata



```stata
clear
set obs 15
gen catalan=1 in 1
replace catalan=catalan[_n-1]*2*(2*_n-3)/_n in 2/l
list, noobs noh
```


'''Output'''


```txt
  +---------+
  |       1 |
  |       1 |
  |       2 |
  |       5 |
  |      14 |
  |---------|
  |      42 |
  |     132 |
  |     429 |
  |    1430 |
  |    4862 |
  |---------|
  |   16796 |
  |   58786 |
  |  208012 |
  |  742900 |
  | 2674440 |
  +---------+
```



## Swift


{{trans|Rust}}

```swift
func catalan(_ n: Int) -> Int {
  switch n {
  case 0:
    return 1
  case _:
    return catalan(n - 1) * 2 * (2 * n - 1) / (n + 1)
  }
}

for i in 1..<16 {
  print("catalan(\(i)) => \(catalan(i))")
}

```


{{out}}

```txt

catalan(1) => 1
catalan(2) => 2
catalan(3) => 5
catalan(4) => 14
catalan(5) => 42
catalan(6) => 132
catalan(7) => 429
catalan(8) => 1430
catalan(9) => 4862
catalan(10) => 16796
catalan(11) => 58786
catalan(12) => 208012
catalan(13) => 742900
catalan(14) => 2674440
catalan(15) => 9694845

```



## Tcl


```tcl
package require Tcl 8.5

# Memoization wrapper
proc memoize {function value generator} {
    variable memoize
    set key $function,$value
    if {![info exists memoize($key)]} {
	set memoize($key) [uplevel 1 $generator]
    }
    return $memoize($key)
}

# The simplest recursive definition
proc tcl::mathfunc::catalan n {
    if {[incr n 0] < 0} {error "must not be negative"}
    memoize catalan $n {expr {
	$n == 0 ? 1 : 2 * (2*$n - 1) * catalan($n - 1) / ($n + 1)
    }}
}
```

Demonstration:

```tcl
for {set i 0} {$i < 15} {incr i} {
    puts "C_$i = [expr {catalan($i)}]"
}
```

{{out}}

```txt

C_0 = 1
C_1 = 1
C_2 = 2
C_3 = 5
C_4 = 14
C_5 = 42
C_6 = 132
C_7 = 429
C_8 = 1430
C_9 = 4862
C_10 = 16796
C_11 = 58786
C_12 = 208012
C_13 = 742900
C_14 = 2674440

```

Of course, this code also works unchanged (apart from extending the loop) for producing higher Catalan numbers. For example, here is the end of the output when asked to produce the first fifty:

```txt

C_45 = 2257117854077248073253720
C_46 = 8740328711533173390046320
C_47 = 33868773757191046886429490
C_48 = 131327898242169365477991900
C_49 = 509552245179617138054608572

```


=={{header|TI-83 BASIC}}==
This problem is perfectly suited for a TI calculator.
<lang TI-83 BASIC>:For(I,1,15
:Disp (2I)!/((I+1)!I!
:End
```

{{out}}

```txt
               1
               2
               4
              14
              42
             132
             429
            1430
            4862
           16796
           58786
          208012
          742900
         2674440
         9694845
            Done
```



## Ursala


```ursala
#import std
#import nat

catalan = quotient^\successor choose^/double ~&

#cast %nL

t = catalan* iota 16
```

{{out}}

```txt
<
   1,
   1,
   2,
   5,
   14,
   42,
   132,
   429,
   1430,
   4862,
   16796,
   58786,
   208012,
   742900,
   2674440,
   9694845>
```



## VBA


```vb
Public Sub Catalan1(n As Integer)
'Computes the first n Catalan numbers according to the first recursion given
Dim Cat() As Long
Dim sum As Long

ReDim Cat(n)
Cat(0) = 1
For i = 0 To n - 1
  sum = 0
  For j = 0 To i
    sum = sum + Cat(j) * Cat(i - j)
  Next j
  Cat(i + 1) = sum
Next i
Debug.Print
For i = 0 To n
  Debug.Print i, Cat(i)
Next
End Sub

Public Sub Catalan2(n As Integer)
'Computes the first n Catalan numbers according to the second recursion given
Dim Cat() As Long

ReDim Cat(n)
Cat(0) = 1
For i = 1 To n
  Cat(i) = 2 * Cat(i - 1) * (2 * i - 1) / (i + 1)
Next i
Debug.Print
For i = 0 To n
  Debug.Print i, Cat(i)
Next
End Sub
```

{{out|Result}}

```txt

Catalan1 15

 0             1 
 1             1 
 2             2 
 3             5 
 4             14 
 5             42 
 6             132 
 7             429 
 8             1430 
 9             4862 
 10            16796 
 11            58786 
 12            208012 
 13            742900 
 14            2674440 
 15            9694845 

```

(Expect same result with "Catalan2 15")


## VBScript


```vb

Function catalan(n)
	catalan = factorial(2*n)/(factorial(n+1)*factorial(n))
End Function

Function factorial(n)
	If n = 0 Then
		Factorial = 1
	Else
		For i = n To 1 Step -1
			If i = n Then
				factorial = n
			Else
				factorial = factorial * i
			End If
		Next
	End If
End Function

'Find the first 15 Catalan numbers.
For j = 1 To 15
	WScript.StdOut.Write j & " = " & catalan(j)
	WScript.StdOut.WriteLine
Next

```


{{Out}}

```txt

1 = 1
2 = 2
3 = 5
4 = 14
5 = 42
6 = 132
7 = 429
8 = 1430
9 = 4862
10 = 16796
11 = 58786
12 = 208012
13 = 742900
14 = 2674440
15 = 9694845

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function Factorial(n As Double) As Double
        If n < 1 Then
            Return 1
        End If

        Dim result = 1.0
        For i = 1 To n
            result = result * i
        Next

        Return result
    End Function

    Function FirstOption(n As Double) As Double
        Return Factorial(2 * n) / (Factorial(n + 1) * Factorial(n))
    End Function

    Function SecondOption(n As Double) As Double
        If n = 0 Then
            Return 1
        End If

        Dim sum = 0
        For i = 0 To n - 1
            sum = sum + SecondOption(i) * SecondOption((n - 1) - i)
        Next
        Return sum
    End Function

    Function ThirdOption(n As Double) As Double
        If n = 0 Then
            Return 1
        End If

        Return ((2 * (2 * n - 1)) / (n + 1)) * ThirdOption(n - 1)
    End Function

    Sub Main()
        Const MaxCatalanNumber = 15

        Dim initial As DateTime
        Dim final As DateTime
        Dim ts As TimeSpan

        initial = DateTime.Now
        For i = 0 To MaxCatalanNumber
            Console.WriteLine("CatalanNumber({0}:{1})", i, FirstOption(i))
        Next
        final = DateTime.Now
        ts = final - initial
        Console.WriteLine("It took {0}.{1} to execute", ts.Seconds, ts.Milliseconds)
        Console.WriteLine()

        initial = DateTime.Now
        For i = 0 To MaxCatalanNumber
            Console.WriteLine("CatalanNumber({0}:{1})", i, SecondOption(i))
        Next
        final = DateTime.Now
        ts = final - initial
        Console.WriteLine("It took {0}.{1} to execute", ts.Seconds, ts.Milliseconds)
        Console.WriteLine()

        initial = DateTime.Now
        For i = 0 To MaxCatalanNumber
            Console.WriteLine("CatalanNumber({0}:{1})", i, ThirdOption(i))
        Next
        final = DateTime.Now
        ts = final - initial
        Console.WriteLine("It took {0}.{1} to execute", ts.Seconds, ts.Milliseconds)
    End Sub

End Module
```

{{out}}

```txt
CatalanNumber(0:1)
CatalanNumber(1:1)
CatalanNumber(2:2)
CatalanNumber(3:5)
CatalanNumber(4:14)
CatalanNumber(5:42)
CatalanNumber(6:132)
CatalanNumber(7:429)
CatalanNumber(8:1430)
CatalanNumber(9:4862)
CatalanNumber(10:16796)
CatalanNumber(11:58786)
CatalanNumber(12:208012)
CatalanNumber(13:742900)
CatalanNumber(14:2674440)
CatalanNumber(15:9694845)
It took 0.19 to execute

CatalanNumber(0:1)
CatalanNumber(1:1)
CatalanNumber(2:2)
CatalanNumber(3:5)
CatalanNumber(4:14)
CatalanNumber(5:42)
CatalanNumber(6:132)
CatalanNumber(7:429)
CatalanNumber(8:1430)
CatalanNumber(9:4862)
CatalanNumber(10:16796)
CatalanNumber(11:58786)
CatalanNumber(12:208012)
CatalanNumber(13:742900)
CatalanNumber(14:2674440)
CatalanNumber(15:9694845)
It took 0.831 to execute

CatalanNumber(0:1)
CatalanNumber(1:1)
CatalanNumber(2:2)
CatalanNumber(3:5)
CatalanNumber(4:14)
CatalanNumber(5:42)
CatalanNumber(6:132)
CatalanNumber(7:429)
CatalanNumber(8:1430)
CatalanNumber(9:4862)
CatalanNumber(10:16796)
CatalanNumber(11:58786)
CatalanNumber(12:208012)
CatalanNumber(13:742900)
CatalanNumber(14:2674440)
CatalanNumber(15:9694845)
It took 0.8 to execute
```



## Wortel


```wortel
; the following number expression calculcates the nth Catalan number
#~ddiFSFmSoFSn
; which stands for: dup dup inc fac swap fac mult swap double fac swap divide
; to get the first 15 Catalan numbers we map this function over a list from 0 to 15
!*#~ddiFSFmSoFSn @til 15
; returns [1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674439.9999999995]
```



## XLISP


```lisp
(defun catalan (n)
	(if (= n 0)
		1
		(* (/ (* 2 (- (* 2 n) 1)) (+ n 1)) (catalan (- n 1))) ) )

(defun range (x y)
	(cons x
		(if (< x y)
			(range (+ x 1) y) ) ) )

(print (mapcar catalan (range 0 14)))
```

{{out}}

```txt
(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440)
```



## XPL0


```XPL0
code CrLf=9, IntOut=11;
int  C, N;
[C:= 1;
IntOut(0, C);  CrLf(0);
for N:= 1 to 14 do
    [C:= C*2*(2*N-1)/(N+1);
    IntOut(0, C);  CrLf(0);
    ];
]
```

{{out}}

```txt

1
1
2
5
14
42
132
429
1430
4862
16796
58786
208012
742900
2674440

```



## zkl

Uses GMP to calculate big factorials.

```zkl
var BN=Import("zklBigNum");
fcn catalan(n){
   BN(2*n).factorial() / BN(n+1).factorial() / BN(n).factorial();
}

foreach n in (16){
   println("%2d --> %,d".fmt(n, catalan(n)));
}
println("%2d --> %,d".fmt(100, catalan(100)));
```

And an iterative solution at works up to the limit of 64 bit ints (n=33). Would be 35 but need to avoid factional intermediate results.

```zkl
fcn catalan(n){ (1).reduce(n,fcn(p,n){ 2*(2*n-1)*p/(n+1) },1) }
```

{{out}}

```txt

 0 --> 1
 1 --> 1
 2 --> 2
 3 --> 5
 4 --> 14
 5 --> 42
 6 --> 132
 7 --> 429
 8 --> 1,430
 9 --> 4,862
10 --> 16,796
11 --> 58,786
12 --> 208,012
13 --> 742,900
14 --> 2,674,440
15 --> 9,694,845
100 --> 896,519,947,090,131,496,687,170,070,074,100,632,420,837,521,538,745,909,320

```



## ZX Spectrum Basic

{{trans|C}}

```zxbasic
10 FOR i=0 TO 15
20 LET n=i: LET m=2*n
30 LET r=1: LET d=m-n
40 IF d>n THEN LET n=d: LET d=m-n
50 IF m<=n THEN GO TO 90
60 LET r=r*m: LET m=m-1
70 IF (d>1) AND NOT FN m(r,d) THEN LET r=r/d: LET d=d-1: GO TO 70
80 GO TO 50
90 PRINT i;TAB 4;r/(1+n)
100 NEXT i
110 STOP 
120 DEF FN m(a,b)=a-INT (a/b)*b: REM Modulus function

```


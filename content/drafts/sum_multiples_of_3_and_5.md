+++
title = "Sum multiples of 3 and 5"
description = ""
date = 2019-10-14T16:05:59Z
aliases = []
[extra]
id = 13489
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
The objective is to write a function that finds the sum of all positive multiples of 3 or 5 below ''n''.

Show output for ''n'' = 1000.


'''Extra credit:''' do this efficiently for ''n'' = 1e20 or higher.





## 360 Assembly


```360asm
*        Sum multiples of 3 and 5
SUM35    CSECT
         USING  SUM35,R13          base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R9,1               n=1
         LA     R7,7               do j=7 to 1 step -1
LOOPJ    MH     R9,=H'10'            n=n*10
         LR     R10,R9               n
         BCTR   R10,0                n-1
         ZAP    SUM,=PL8'0'          sum=0
         LA     R6,3                 i=3
       DO WHILE=(CR,R6,LE,R10)       do i=3 to n-1
         LR     R4,R6                  i
         SRDA   R4,32
         D      R4,=F'3'               i/3
         LTR    R4,R4                  if mod(i,3)=0
         BZ     CVD
         LR     R4,R6                  i
         SRDA   R4,32
         D      R4,=F'5'               i/5
         LTR    R4,R4                  if  mod(i,5)=0
         BNZ    ITERI
CVD      CVD    R6,IP                  ip=p
         AP     SUM,IP                 sum=sum+i
ITERI    LA     R6,1(R6)               i++
       ENDDO    ,                    enddo i
         XDECO  R9,PG                n
         MVC    PG+15(16),EM16       load mask
         ED     PG+15(16),SUM        packed dec (PL8) to char (CL16)
         XPRNT  PG,L'PG              print
         BCT    R7,LOOPJ           enddo j
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
SUM      DS     PL8
IP       DS     PL8
EM16     DC     X'40202020202020202020202020202120'  mask CL16 15num
PG       DC     CL80'123456789012 : 1234567890123456'
         YREGS
         END    SUM35
```

{{out}}

```txt

          10 :               23
         100 :             2318
        1000 :           233168
       10000 :         23331668
      100000 :       2333316668
     1000000 :     233333166668
    10000000 :   23333331666668

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
Uses Algol 68G's LONG LONG INT to handle large numbers.

```algol68
# returns the sum of the multiples of 3 and 5 below n #
PROC sum of multiples of 3 and 5 below = ( LONG LONG INT n )LONG LONG INT:
     BEGIN
        # calculate the sum of the multiples of 3 below n #
        LONG LONG INT multiples of  3 = ( n - 1 ) OVER  3;
        LONG LONG INT multiples of  5 = ( n - 1 ) OVER  5;
        LONG LONG INT multiples of 15 = ( n - 1 ) OVER 15;
        ( # twice the sum of multiples of  3 #
          (  3 * multiples of  3 * ( multiples of  3 + 1 ) )
          # plus twice the sum of multiples of  5 #
        + (  5 * multiples of  5 * ( multiples of  5 + 1 ) )
          # less twice the sum of multiples of 15 #
        - ( 15 * multiples of 15 * ( multiples of 15 + 1 ) )
        ) OVER 2
    END # sum of multiples of 3 and 5 below # ;

print( ( "Sum of multiples of 3 and 5 below 1000: "
       , whole( sum of multiples of 3 and 5 below( 1000 ), 0 )
       , newline
       )
     );
print( ( "Sum of multiples of 3 and 5 below 1e20: "
       , whole( sum of multiples of 3 and 5 below( 100 000 000 000 000 000 000 ), 0 )
       , newline
       )
     )
```

{{out}}

```txt

Sum of multiples of 3 and 5 below 1000: 233168
Sum of multiples of 3 and 5 below 1e20: 2333333333333333333316666666666666666668

```


== {{header|APL}} ==

```apl
⎕IO←0
{+/((0=3|a)∨0=5|a)/a←⍳⍵} 1000
```
[http://ngn.github.io/apl/web/index.html#code=%7B+/%28%280%3D3%7Ca%29%u22280%3D5%7Ca%29/a%u2190%u2373%u2375%7D%201000,run=1 run]
{{out}}

```txt
233168
```


== {{header|AppleScript}} ==
{{Trans|JavaScript}}

```AppleScript
-- SUM MULTIPLES OF 3 AND 5 --------------------------------------------------

-- sums of all multiples of 3 or 5 below or equal to N
-- for N = 10 to N = 10E8 (limit of AS integers)

-- sum35Result :: String -> Int -> Int -> String
script sum35Result

    -- sum35 :: Int -> Int
    on sum35(n)
        sumMults(n, 3) + sumMults(n, 5) - sumMults(n, 15)
    end sum35

    -- Area under straight line between first multiple and last:

    -- sumMults :: Int -> Int -> Int
    on sumMults(n, f)
        set n1 to (n - 1) div f

        f * n1 * (n1 + 1) div 2
    end sumMults

    on |λ|(a, x, i)
        a & "10<sup>" & i & "</sup> -> " & ¬
            sum35(10 ^ x) & "
"
    end |λ|
end script


-- TEST ----------------------------------------------------------------------
on run

    foldl(sum35Result, "", enumFromTo(1, 8))

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
10<sup>1</sup> -> 23
10<sup>2</sup> -> 2318
10<sup>3</sup> -> 233168
10<sup>4</sup> -> 23331668
10<sup>5</sup> -> 2.333316668E+9
10<sup>6</sup> -> 2.33333166668E+11
10<sup>7</sup> -> 2.333333166667E+13
10<sup>8</sup> -> 2.333333316667E+15



## Arturo



```arturo
sumMul35 [n]{ $(sum $(filter $(range 1 n-1) { $(or &%3=0 &%5=0) })) }

print $(sumMul35 1000)
```


{{out}}


```txt
233168
```



## AutoHotkey



```AutoHotkey
n := 1000

msgbox % "Sum is " . Sum3_5(n)   . " for n = " . n
msgbox % "Sum is " . Sum3_5_b(n) . " for n = " . n

;Standard simple Implementation.
Sum3_5(n) {
	sum := 0
	loop % n-1 {
		if (!Mod(a_index,3) || !Mod(a_index,5))
		sum:=sum+A_index
	}
	return sum
}

;Translated from the C++ version.
Sum3_5_b( i ) {
	sum := 0, a := 0
	while (a < 28)
	{
		if (!Mod(a,3) || !Mod(a,5))
		{
			sum += a
			s := 30
			while (s < i)
			{
				if (a+s < i)
					sum += (a+s)
				s+=30
			}
		}
		a+=1
	}
	return sum
}
```

'''Output:'''
```txt
Sum is 233168 for n = 1000
Sum is 233168 for n = 1000
```



## AWK

Save this into file "sum_multiples_of3and5.awk"

```AWK
#!/usr/bin/awk -f
{
	n = $1-1;
	print sum(n,3)+sum(n,5)-sum(n,15);
}
function sum(n,d) {
	m = int(n/d);
	return (d*m*(m+1)/2);
}
```


{{Out}}

```txt
$ echo 1000 |awk -f sum_multiples_of3and5.awk
233168
```



### Extra credit

{{Works with|Gawk|4.1}}
In Awk, all numbers are represented internally as double precision floating-point numbers. Thus the result for the extra credit is unprecise. Since version 4.1, GNU Awk supports high precision arithmetic (using [http://www.mpfr.org/ GNU MPFR] and [[GMP]]) which is turned on with the <code>-M / --bignum</code> option. The variable <code>PREC</code> sets the working precision for arithmetic operations (here 80 bits):


```txt
$ echo -e "1000\n1e20" | gawk -M -v PREC=80 -f sum_multiples_of3and5.awk
233168
2333333333333333333316666666666666666668
```


== {{header|BASIC}} ==
{{works with|FreeBASIC}}

```freebasic
Declare function mulsum35(n as integer) as integer
Function mulsum35(n as integer) as integer
    Dim s as integer
    For i as integer = 1 to n - 1
        If (i mod 3 = 0) or (i mod 5 = 0) then
            s += i
        End if
    Next i
    Return s
End Function
Print mulsum35(1000)
Sleep
End
```

{{out}}

```txt
233168
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PRINT MULTSUM35(1000)
110 DEF MULTSUM35(N)
120   LET S=0
130   FOR I=1 TO N-1
140     IF MOD(I,3)=0 OR MOD(I,5)=0 THEN LET S=S+I
150   NEXT
160   LET MULTSUM35=S
170 END DEF
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

The ZX81 doesn't offer enough numeric precision to try for the extra credit. This program is pretty unsophisticated; the only optimization is that we skip testing whether <math>i</math> is divisible by 5 if we already know it's divisible by 3. (ZX81 BASIC doesn't do this automatically: both sides of an <code>OR</code> are evaluated, even if we don't need the second one.) Even so, with <math>n</math> = 1000 the performance is pretty acceptable.

```basic
 10 INPUT N
 20 FAST
 30 LET SUM=0
 40 FOR I=3 TO N-1
 50 IF I/3=INT (I/3) THEN GOTO 70
 60 IF I/5<>INT (I/5) THEN GOTO 80
 70 LET SUM=SUM+I
 80 NEXT I
 90 SLOW
100 PRINT SUM
```

{{in}}

```txt
1000
```

{{out}}

```txt
233168
```



## bc

{{trans|Groovy}}

```bc
define t(n, f) {
    auto m

    m = (n - 1) / f
    return(f * m * (m + 1) / 2)
}

define s(l) {
    return(t(l, 3) + t(l, 5) - t(l, 15))
}

s(1000)
s(10 ^ 20)
```

{{Out}}

```txt
233168
2333333333333333333316666666666666666668
```


== {{header|Befunge}} ==
Slow (iterative) version:

```Befunge
&1-:!#v_:3%#v_     >:>#
      >+\:v >:5%#v_^
  @.$_^#! <      >   ^
```

{{Out}}

```txt
233168
```

Fast (analytic) version:

```Befunge
&1-::3/:1+*3*2/\5/:1+*5*2/+\96+/:1+*96+*2/-.@
```

{{Out}}

```txt
233168
```



## C


### Simple version


```c
#include <stdio.h>
#include <stdlib.h>

unsigned long long sum35(unsigned long long limit)
{
    unsigned long long sum = 0;
    for (unsigned long long i = 0; i < limit; i++)
        if (!(i % 3) || !(i % 5))
            sum += i;
    return sum;
}

int main(int argc, char **argv)
{
    unsigned long long limit;

    if (argc == 2)
        limit = strtoull(argv[1], NULL, 10);
    else
        limit = 1000;

    printf("%lld\n", sum35(limit));
    return 0;
}
```

{{Out}}

```txt
$ ./a.out
233168
$ ./a.out 12345
35553600
```



### Fast version with arbitrary precision

{{libheader|GMP}}

```c
#include <stdio.h>
#include <gmp.h>

void sum_multiples(mpz_t result, const mpz_t limit, const unsigned f)
{
    mpz_t m;
    mpz_init(m);
    mpz_sub_ui(m, limit, 1);
    mpz_fdiv_q_ui(m, m, f);

    mpz_init_set(result, m);
    mpz_add_ui(result, result, 1);
    mpz_mul(result, result, m);
    mpz_mul_ui(result, result, f);
    mpz_fdiv_q_2exp(result, result, 1);

    mpz_clear(m);
}

int main(int argc, char **argv)
{
    mpf_t temp;
    mpz_t limit;

    if (argc == 2)
    {
        mpf_init_set_str(temp, argv[1], 10);
        mpz_init(limit);
        mpz_set_f(limit, temp);
        mpf_clear(temp);
    }
    else
        mpz_init_set_str(limit, "1000000000000000000000", 10);

    mpz_t temp_sum;
    mpz_t sum35;

    mpz_init(temp_sum);
    sum_multiples(temp_sum, limit, 3);
    mpz_init_set(sum35, temp_sum);
    sum_multiples(temp_sum, limit, 5);
    mpz_add(sum35, sum35, temp_sum);
    sum_multiples(temp_sum, limit, 15);
    mpz_sub(sum35, sum35, temp_sum);

    mpz_out_str(stdout, 10, sum35);
    puts("");

    mpz_clear(temp_sum);
    mpz_clear(sum35);
    mpz_clear(limit);
    return 0;
}
```

{{Out}}

```txt
$ ./a.out
233333333333333333333166666666666666666668
$ ./a.out 23e45
123433333333333333333333333333333333333333333314166666666666666666666666666666666666666666668
```


## C#
The following C# 5 / .Net 4 code is an <i>efficient solution</i> in that it does not iterate through the numbers 1 ... n - 1 in order to calculate the answer. On the other hand, the System.Numerics.BigInteger class (.Net 4 and upwards) is not itself efficient because calculations take place in software instead of hardware. Consequently, it may be <i>faster</i> to conduct the calculation for smaller values with native ("primitive") types using a 'brute force' iteration approach.


```c#

using System;
using System.Collections.Generic;
using System.Numerics;

namespace RosettaCode
{
    class Program
    {
        static void Main()
        {
            List<BigInteger> candidates = new List<BigInteger>(new BigInteger[] { 1000, 100000, 10000000, 10000000000, 1000000000000000 });
            candidates.Add(BigInteger.Parse("100000000000000000000"));

            foreach (BigInteger candidate in candidates)
            {
                BigInteger c = candidate - 1;
                BigInteger answer3 = GetSumOfNumbersDivisibleByN(c, 3);
                BigInteger answer5 = GetSumOfNumbersDivisibleByN(c, 5);
                BigInteger answer15 = GetSumOfNumbersDivisibleByN(c, 15);

                Console.WriteLine("The sum of numbers divisible by 3 or 5 between 1 and {0} is {1}", c, answer3 + answer5 - answer15);
            }

            Console.ReadKey(true);
        }

        private static BigInteger GetSumOfNumbersDivisibleByN(BigInteger candidate, uint n)
        {
            BigInteger largest = candidate;
            while (largest % n > 0)
                largest--;
            BigInteger totalCount = (largest / n);
            BigInteger pairCount = totalCount / 2;
            bool unpairedNumberOnFoldLine = (totalCount % 2 == 1);
            BigInteger pairSum = largest + n;
            return pairCount * pairSum + (unpairedNumberOnFoldLine ? pairSum / 2 : 0);
        }

    }
}

```

{{out}}
The sum of numbers divisible by 3 or 5 between 1 and 999 is 233168

The sum of numbers divisible by 3 or 5 between 1 and 99999 is 2333316668

The sum of numbers divisible by 3 or 5 between 1 and 9999999 is 23333331666668

The sum of numbers divisible by 3 or 5 between 1 and 9999999999 is 23333333331666666668

The sum of numbers divisible by 3 or 5 between 1 and 999999999999999 is 233333333333333166666666666668

The sum of numbers divisible by 3 or 5 between 1 and 99999999999999999999 is 2333333333333333333316666666666666666668


## C++


```cpp

#include <iostream>

//--------------------------------------------------------------------------------------------------
typedef unsigned long long bigInt;

using namespace std;
//--------------------------------------------------------------------------------------------------
class m35
{
public:
    void doIt( bigInt i )
    {
	bigInt sum = 0;
	for( bigInt a = 1; a < i; a++ )
	    if( !( a % 3 ) || !( a % 5 ) ) sum += a;

	cout << "Sum is " << sum << " for n = " << i << endl << endl;
    }

    // this method uses less than half iterations than the first one
    void doIt_b( bigInt i )
    {
	bigInt sum = 0;
	for( bigInt a = 0; a < 28; a++ )
	{
	    if( !( a % 3 ) || !( a % 5 ) )
	    {
		sum += a;
		for( bigInt s = 30; s < i; s += 30 )
		    if( a + s < i ) sum += ( a + s );

	    }
	}
	cout << "Sum is " << sum << " for n = " << i << endl << endl;
    }
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    m35 m; m.doIt( 1000 );
    return system( "pause" );
}

```

{{out}}
 Sum is 233168 for n = 1000


## Clojure

Quick, concise way:

```clojure
(defn sum-mults [n & mults]
  (let [pred (apply some-fn
               (map #(fn [x] (zero? (mod x %))) mults))]
    (->> (range n) (filter pred) (reduce +))))

(println (sum-mults 1000 3 5))
```

Or optimized (translated from Groovy):

```clojure
(defn sum-mul [n f]
  (let [n1 (/' (inc' n) f)]
    (*' f n1 (inc' n1) 1/2)

(def sum-35 #(-> % (sum-mul 3) (+ (sum-mul % 5)) (- (sum-mul % 15))))
(println (sum-35 1000000000))
```



## COBOL


Using OpenCOBOL.


```cobol

Identification division.
Program-id. three-five-sum.

Data division.
Working-storage section.
01 ws-the-limit  pic 9(18) value 1000.
01 ws-the-number pic 9(18).
01 ws-the-sum    pic 9(18).
01 ws-sum-out    pic z(18).

Procedure division.
Main-program.
    Perform Do-sum
        varying ws-the-number from 1 by 1
        until ws-the-number = ws-the-limit.
    Move ws-the-sum to ws-sum-out.
    Display "Sum = " ws-sum-out.
    End-run.

Do-sum.
    If function mod(ws-the-number, 3) = zero
       or function mod(ws-the-number, 5) = zero
       then add ws-the-number to ws-the-sum.

```


Output:

```txt

Sum =             233168

```


Using triangular numbers:

```cobol

Identification division.
Program-id. three-five-sum-fast.

Data division.
Working-storage section.
01 ws-num     pic 9(18) value 1000000000.
01 ws-n5      pic 9(18).
01 ws-n3      pic 9(18).
01 ws-n15     pic 9(18).
01 ws-sum     pic 9(18).
01 ws-out.
    02 ws-out-num pic z(18).
    02 filler pic x(3) value " = ".
    02 ws-out-sum pic z(18).

Procedure division.
Main-program.
    Perform
        call "tri-sum" using ws-num 3  by reference ws-n3
        call "tri-sum" using ws-num 5  by reference ws-n5
        call "tri-sum" using ws-num 15  by reference ws-n15
    end-perform.
    Compute ws-sum = ws-n3 + ws-n5 - ws-n15.
    Move ws-sum to ws-out-sum.
    Move ws-num to ws-out-num.
    Display ws-out.



Identification division.
Program-id. tri-sum.

Data division.
Working-storage section.
01 ws-n1 pic 9(18).
01 ws-n2 pic 9(18).

Linkage section.
77 ls-num pic 9(18).
77 ls-fac pic 9(18).
77 ls-ret pic 9(18).

Procedure division using ls-num, ls-fac, ls-ret.
    Compute ws-n1 = (ls-num - 1) / ls-fac.
    Compute ws-n2 = ws-n1 + 1.
    Compute ls-ret = ls-fac * ws-n1 * ws-n2 / 2.
    goback.

```


Output:

```txt

        1000000000 = 233333333166666668

```



A brute-method using only comparisons and adds. Compiles and runs as is in GnuCOBOL 2.0 and Micro Focus Visual COBOL 2.3. Takes about 7.3 seconds to calculate 1,000,000,000 iterations (AMD A6 quadcore 64bit)


```cobol

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUM35.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  THREE-COUNTER   USAGE BINARY-CHAR value 1.
           88 IS-THREE VALUE 3.
       01  FIVE-COUNTER    USAGE BINARY-CHAR value 1.
           88 IS-FIVE VALUE 5.
       01  SUMMER          USAGE BINARY-DOUBLE value zero.
       01  I               USAGE BINARY-LONG.
       01  N               USAGE BINARY-LONG.

       PROCEDURE DIVISION.
       10-MAIN-PROCEDURE.
           MOVE 1000000000 TO N.
           MOVE 1 TO I.
           PERFORM 20-INNER-LOOP WITH TEST AFTER UNTIL I >= N.
           DISPLAY SUMMER.
           STOP RUN.
       20-INNER-LOOP.
           IF IS-THREE OR IS-FIVE
               ADD I TO SUMMER END-ADD
               IF IS-THREE
                   MOVE 1 TO THREE-COUNTER
               ELSE
                   ADD 1 TO THREE-COUNTER
               END-IF
               IF IS-FIVE
                   MOVE 1 TO FIVE-COUNTER
               ELSE
                   ADD 1 TO FIVE-COUNTER
               END-IF
           ELSE
               ADD 1 TO FIVE-COUNTER END-ADD
               ADD 1 TO THREE-COUNTER END-ADD
           END-IF.
           ADD 1 TO I.
           EXIT.
       END PROGRAM SUM35.

```

Output

```txt
+00233333333166666668
```



## Common Lisp

Slow, naive version:

```lisp
(defun sum-3-5-slow (limit)
  (loop for x below limit
        when (or (zerop (rem x 3)) (zerop (rem x 5)))
          sum x))
```


Fast version (adapted translation of [[#Tcl|Tcl]]):

```lisp
(defun sum-3-5-fast (limit)
  (flet ((triangular (n) (truncate (* n (1+ n)) 2)))
    (let ((n (1- limit)))  ; Sum multiples *below* the limit
      (- (+ (* 3 (triangular (truncate n 3)))
            (* 5 (triangular (truncate n 5))))
         (* 15 (triangular (truncate n 15)))))))
```


{{Out}}

```txt
> (values (sum-3-5-slow 1000) (sum-3-5-fast 1000))
233168 ;
233168
> (sum-3-5-fast 1000000000000000000000)
233333333333333333333166666666666666666668
```





## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Sum3_5;
IMPORT StdLog, Strings, Args;

PROCEDURE DoSum(n: INTEGER):INTEGER;
VAR
	i,sum: INTEGER;
BEGIN
	sum := 0;i := 0;
	WHILE (i < n) DO
		IF  (i MOD 3 = 0) OR (i MOD 5 = 0) THEN INC(sum,i) END;
		INC(i)
	END;
	RETURN sum
END DoSum;

PROCEDURE Compute*;
VAR
	params: Args.Params;
	i,n,res: INTEGER;
BEGIN
	Args.Get(params);
	Strings.StringToInt(params.args[0],n,res);
	StdLog.String("Sum: ");StdLog.Int(DoSum(n)); StdLog.Ln
END Compute;

END Sum3_5.

```

Execute: ^Q Sum3_5.Compute 1000 ~ <br/>
Output:

```txt

Sum:  233168

```



## Crystal

{{trans|Ruby}}
Short, but not optimized.

```ruby
def sum_3_5_multiples(n)
  (0...n).select { |i| i % 3 == 0 || i % 5 == 0 }.sum
end

puts sum_3_5_multiples(1000)
```

{{out}}

```txt

233168

```


Alternative fast version 1.
The Ruby version sums up to and including n.
To conform to task requirements, and other versions,
modified to find sums below n.

```ruby
require "big"

def g(n1, n2, n3)
   g1 = n1*n2; n3 -= 1
   (1..g1).select{|x| x%n1==0 || x%n2==0}.map{|x| g2=(n3-x)/g1; (x+g1*g2+x)*(g2+1)}.sum / 2
end

puts g(3,5,999)
puts g(3,5,1000)

# For extra credit
puts g(3,5,"100000000000000000000".to_big_i - 1)
puts g(3,5,"100000000000000000000".to_big_i)
```

{{out}}

```txt

232169
233168
2333333333333333333216666666666666666669
2333333333333333333316666666666666666668

```


Alternative faster version 2.

```ruby
require "big"

def sumMul(n, f)
  n1 = (n.to_big_i - 1) / f  # number of multiples of f < n
  f * n1 * (n1 + 1) / 2      # f * (sum of number of multiples)
end

def sum35(n)
  sumMul(n, 3) + sumMul(n, 5) - sumMul(n, 15)
end

(1..20).each do |e| limit = 10.to_big_i ** e
  puts "%2d:%22d %s" % [e, limit, sum35(limit)]
end
```

{{out}}

```txt

 1:                    10 23
 2:                   100 2318
 3:                  1000 233168
 4:                 10000 23331668
 5:                100000 2333316668
 6:               1000000 233333166668
 7:              10000000 23333331666668
 8:             100000000 2333333316666668
 9:            1000000000 233333333166666668
10:           10000000000 23333333331666666668
11:          100000000000 2333333333316666666668
12:         1000000000000 233333333333166666666668
13:        10000000000000 23333333333331666666666668
14:       100000000000000 2333333333333316666666666668
15:      1000000000000000 233333333333333166666666666668
16:     10000000000000000 23333333333333331666666666666668
17:    100000000000000000 2333333333333333316666666666666668
18:   1000000000000000000 233333333333333333166666666666666668
19:  10000000000000000000 23333333333333333331666666666666666668
20: 100000000000000000000 2333333333333333333316666666666666666668

```



## D


```d
import std.stdio, std.bigint;

BigInt sum35(in BigInt n) pure nothrow {
    static BigInt sumMul(in BigInt n, in int f) pure nothrow {
        immutable n1 = (f==n?n:(n - 1) ) / f;
        return f * n1 * (n1 + 1) / 2;
    }

    return sumMul(n, 3) + sumMul(n, 5) - sumMul(n, 15);
}

void main() {
    1.BigInt.sum35.writeln;
    3.BigInt.sum35.writeln;
    5.BigInt.sum35.writeln;
    1000.BigInt.sum35.writeln;
    (10.BigInt ^^ 20).sum35.writeln;
}
```

{{out}}

```txt

0
3
8
233168
2333333333333333333316666666666666666668
```


=={{header|Déjà Vu}}==

```dejavu
sum-divisible n:
	0
	for i range 1 -- n:
		if or = 0 % i 3 = 0 % i 5:
			+ i

!. sum-divisible 1000
```

{{out}}

```txt
233168
```



## Delphi


```delphi
program sum35;

{$APPTYPE CONSOLE}

var
  sum: integer;
  i: integer;

function isMultipleOf(aNumber, aDivisor: integer): boolean;
begin
  result := aNumber mod aDivisor = 0
end;

begin
  sum := 0;
  for i := 3 to 999 do
  begin
    if isMultipleOf(i, 3) or isMultipleOf(i, 5) then
      sum := sum + i;
  end;
  writeln(sum);
end.
```

{{out}}

```txt
233168
```



## EchoLisp


```scheme

(lib 'math) ;; divides?
(lib 'sequences) ;; sum/when

(define (task n  (k 3)  (p 5 ))
	 (when (!= (gcd k p) 1) (error "expected coprimes" (list k p)))
		(-
	 	(+ (sum/mults n k) (sum/mults n p)) ;; add multiples of k , multiples of p
	 	(sum/mults n (* k p)))) ;; remove multiples of k * p

;; using sequences
;; sum of multiples of k < n

(define (sum/mults n k)
	(sum/when (rcurry divides? k) [1 .. n]))

(task 1000 3 5)
    → 233168

;; using simple arithmetic - 🎩 young Gauss formula
;; sum of multiples of k < n  =
;; k*m*(m+1)/2 where m = floor(n/k)
(lib 'bigint)

(define (sum/mults n k)
	(set! n (quotient (1- n) k))
	(/ (* k n (1+ n )) 2))

(task 1e20 3 5)
    → 2333333333333333333316666666666666666668

(task 1000 42 666)
    ❌ error: expected coprimes (42 666)


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
			io.put_integer (sum_multiples (1000))
		end

	sum_multiples (n: INTEGER): INTEGER
			-- Sum of all positive multiples of 3 or 5 below 'n'.
		do
			across
				1 |..| (n - 1) as c
			loop
				if c.item \\ 3 = 0 or c.item \\ 5 = 0 then
					Result := Result + c.item
				end
			end
		end

end


```

{{out}}

```txt

233168

```



## Elixir

Simple (but slow)

```elixir
iex(1)> Enum.filter(0..1000-1, fn x -> rem(x,3)==0 or rem(x,5)==0 end) |> Enum.sum
233168
```


Fast version:
{{trans|Ruby}}

```elixir
defmodule RC do
  def sumMul(n, f) do
    n1 = div(n - 1, f)
    div(f * n1 * (n1 + 1), 2)
  end

  def sum35(n) do
    sumMul(n, 3) + sumMul(n, 5) - sumMul(n, 15)
  end
end

Enum.each(1..20, fn i ->
  n = round(:math.pow(10, i))
  IO.puts RC.sum35(n)
end)
```


{{out}}

```txt

23
2318
233168
23331668
2333316668
233333166668
23333331666668
2333333316666668
233333333166666668
23333333331666666668
2333333333316666666668
233333333333166666666668
23333333333331666666666668
2333333333333316666666666668
233333333333333166666666666668
23333333333333331666666666666668
2333333333333333316666666666666668
233333333333333333166666666666666668
23333333333333333331666666666666666668
2333333333333333333316666666666666666668

```



## Emacs Lisp


### version 1


```Emacs Lisp

(defun sum-3-5 (ls)
  (apply '+ (mapcar
	     '(lambda (x) (if (or (= 0 (% x 3) ) (= 0 (% x 5) ))
			      x 0) )
			ls) ))

```


### version 2


```Emacs Lisp

(defun sum-3-5 (ls)
  (apply '+ (seq-filter
	     '(lambda (x) (or (= 0 (% x 3) ) (= 0 (% x 5) )))
	     ls) ))

```

<b>Eval:</b>

```Emacs Lisp

(insert (format "%d" (sum-3-5 (number-sequence 1 100) )))

```

<b>Output:</b>

```txt

2418

```




## Erlang


```erlang
sum_3_5(X) when is_number(X) -> sum_3_5(erlang:round(X)-1, 0).
sum_3_5(X, Total) when X < 3 -> Total;
sum_3_5(X, Total) when X rem 3 =:= 0 orelse X rem 5 =:= 0 ->
  sum_3_5(X-1, Total+X);
sum_3_5(X, Total) ->
  sum_3_5(X-1, Total).

io:format("~B~n", [sum_3_5(1000)]).
```


{{out}}

```txt
233168
```


=={{header|F_Sharp|F#}}==

```fsharp

let sum35 n = Seq.init n (id) |> Seq.reduce (fun sum i -> if i % 3 = 0 || i % 5 = 0 then sum + i else sum)

printfn "%d" (sum35 1000)
printfn "----------"

let sumUpTo (n : bigint) = n * (n + 1I) / 2I

let sumMultsBelow k n = k * (sumUpTo ((n-1I)/k))

let sum35fast n = (sumMultsBelow 3I n) + (sumMultsBelow 5I n) - (sumMultsBelow 15I n)

[for i = 0 to 30 do yield i]
|> List.iter (fun i -> printfn "%A" (sum35fast (bigint.Pow(10I, i))))
```

{{out}}
<pre style="height:5em">233168
----------
0
23
2318
233168
23331668
2333316668
233333166668
23333331666668
2333333316666668
233333333166666668
23333333331666666668
2333333333316666666668
233333333333166666666668
23333333333331666666666668
2333333333333316666666666668
233333333333333166666666666668
23333333333333331666666666666668
2333333333333333316666666666666668
233333333333333333166666666666666668
23333333333333333331666666666666666668
2333333333333333333316666666666666666668
233333333333333333333166666666666666666668
23333333333333333333331666666666666666666668
2333333333333333333333316666666666666666666668
233333333333333333333333166666666666666666666668
23333333333333333333333331666666666666666666666668
2333333333333333333333333316666666666666666666666668
233333333333333333333333333166666666666666666666666668
23333333333333333333333333331666666666666666666666666668
2333333333333333333333333333316666666666666666666666666668
233333333333333333333333333333166666666666666666666666666668
```



## Factor


```factor
USING: formatting kernel math math.functions sequences
tools.time ;
IN: rosetta-code.sum35

: {x+y-z} ( {x,y,z} -- x+y-z ) first3 [ + ] dip - ;

: range-length ( limit multiple -- len ) [ 1 - ] dip /i ;

: triangular ( limit multiple -- sum )
    [ range-length ] [ nip over 1 + ] 2bi * * 2 / ;

: sum35 ( limit -- sum )
    { 3 5 15 } [ triangular ] with map {x+y-z} ;

: msg ( limit sum -- )
    "The sum of multiples of 3 or 5 below %d is %d.\n" printf ;

: output ( limit -- ) dup sum35 msg ;

: main ( -- ) [ 1000 10 20 ^ [ output ] bi@ ] time ;

MAIN: main
```

{{out}}

```txt

The sum of multiples of 3 or 5 below 1000 is 233168.
The sum of multiples of 3 or 5 below 100000000000000000000 is 2333333333333333333316666666666666666668.
Running time: 0.000923753 seconds

```



## FBSL

Derived from BASIC version

```qbasic
#APPTYPE CONSOLE

FUNCTION sumOfThreeFiveMultiples(n AS INTEGER)
    DIM sum AS INTEGER
    FOR DIM i = 1 TO n - 1
        IF (NOT (i MOD 3)) OR (NOT (i MOD 5)) THEN
            INCR(sum, i)
        END IF
    NEXT
    RETURN sum
END FUNCTION

PRINT sumOfThreeFiveMultiples(1000)
PAUSE

```

Output

```txt
233168

Press any key to continue...

```



## Forth


```forth
: main ( n -- )
  0 swap
  3 do
    i 3 mod 0= if
      i +
    else i 5 mod 0= if
      i +
    then then
  loop
  . ;

1000 main    \ 233168  ok
```


Another FORTH version using the Inclusion/Exclusion Principle.  The result is a double precision integer (128 bits on a 64 bit computer) which lets us calculate up to 10^18 (the max precision of a single precision 64 bit integer)  Since this is Project Euler problem 1, the name of the main function is named euler1tower.


```forth
: third  2 pick ;

: >dtriangular ( n -- d )
    dup 1+ m* d2/ ;

: sumdiv ( n m -- d )
    dup >r / >dtriangular r> 1 m*/ ;

: sumdiv_3,5 ( n -- n )
    dup 3 sumdiv third 5 sumdiv d+ rot 15 sumdiv d- ;

: euler1 ( -- n )
    999 sumdiv_3,5 drop ;

: euler1tower ( -- )
    1  \ power of 10
    19 0 DO
        cr dup 19 .r space dup 1- sumdiv_3,5 d.
        10 *
    LOOP drop ;

euler1 . 233168  ok
euler1tower
                  1 0
                 10 23
                100 2318
               1000 233168
              10000 23331668
             100000 2333316668
            1000000 233333166668
           10000000 23333331666668
          100000000 2333333316666668
         1000000000 233333333166666668
        10000000000 23333333331666666668
       100000000000 2333333333316666666668
      1000000000000 233333333333166666666668
     10000000000000 23333333333331666666666668
    100000000000000 2333333333333316666666666668
   1000000000000000 233333333333333166666666666668
  10000000000000000 23333333333333331666666666666668
 100000000000000000 2333333333333333316666666666666668
1000000000000000000 233333333333333333166666666666666668  ok

```


## Fortran

The method here recalls the story of the young Gauss being set the problem of adding up all the integers from one to a hundred by a master who wanted some peace and quiet from his class. The trick here is to apply the formula for multiples of three and for five, then remember that multiples of fifteen will have been counted twice.

Early Fortrans did not offer such monsters as INTEGER*8 but the F95 compiler I have does so. Even so, the source is in the style of F77 which means that in the absence of the MODULE protocol, the types of the functions must be specified if they are not default types. F77 also does not accept the <code>END FUNCTION ''name''</code> protocol that F90 does, but such documentation enables compiler checks and not using it makes me wince.

```Fortran

      INTEGER*8 FUNCTION SUMI(N)	!Sums the integers 1 to N inclusive.
Calculates as per the young Gauss: N*(N + 1)/2 = 1 + 2 + 3 + ... + N.
       INTEGER*8 N	!The number. Possibly large.
        IF (MOD(N,2).EQ.0) THEN	!So, I'm worried about overflow with N*(N + 1)
          SUMI = N/2*(N + 1)		!But since N is even, N/2 is good.
         ELSE			!Otherwise, if N is odd,
          SUMI = (N + 1)/2*N		!(N + 1) must be even.
        END IF			!Either way, the /2 reduces the result.
      END FUNCTION SUMI		!So overflow of intermediate results is avoided.

      INTEGER*8 FUNCTION SUMF(N,F)	!Sum of numbers up to N divisible by F.
       INTEGER*8 N,F		!The selection.
       INTEGER*8 L		!The last in range. N itself is excluded.
       INTEGER*8 SUMI		!Known type of the function.
        L = (N - 1)/F		!Truncates fractional parts.
        SUMF = F*SUMI(L)	!3 + 6 + 9 + ... = 3(1 + 2 + 3 + ...)
      END FUNCTION SUMF		!Could just put SUMF = F*SUMI((N - 1)/F).

      INTEGER*8 FUNCTION SUMBFI(N)	!Brute force and ignorance summation.
       INTEGER*8 N	!The number.
       INTEGER*8 I,S	!Stepper and counter.
        S = 0		!So, here we go.
        DO I = 3,N - 1	!N itself is not a candidate.
          IF (MOD(I,3).EQ.0 .OR. MOD(I,5).EQ.0) S = S + I	!Whee!
        END DO		!On to the next.
        SUMBFI = S		!The result.
      END FUNCTION SUMBFI	!Oh well, computers are fast these days.

      INTEGER*8 SUMF,SUMBFI	!Known type of the function.
      INTEGER*8 N	!The number.
      WRITE (6,*) "Sum multiples of 3 and 5 up to N"
   10 WRITE (6,11)		!Ask nicely.
   11 FORMAT ("Specify N: ",$)	!Obviously, the $ says 'stay on this line'.
      READ (5,*) N		!If blank input is given, further input will be requested.
      IF (N.LE.0) STOP		!Good enough.
      WRITE (6,*) "By Gauss:",SUMF(N,3) + SUMF(N,5) - SUMF(N,15)
      WRITE (6,*) "BFI sum :",SUMBFI(N)		!This could be a bit slow.
      GO TO 10			!Have another go.
      END	!So much for that.

```

Sample output:

```txt

 Sum multiples of 3 and 5 up to N
Specify N: 1000
 By Gauss:                233168
 BFI sum :                233168
Specify N: 1001
 By Gauss:                234168
 BFI sum :                234168
Specify N: 1002
 By Gauss:                234168
 BFI sum :                234168
Specify N: 1003
 By Gauss:                235170
 BFI sum :                235170
Specify N: 1000000000
 By Gauss:    233333333166666668
 BFI sum :    233333333166666668

```

The result for a thousand million took about a minute for the brute-force-and-ignorance calculation. For much larger values of N, it should be discarded! Integer overflow even for 64-bit integers impends. The calculations could be conducted in double precision (or better, quadruple precision), a trivial modification to the source. Precise results would require the introduction of multi-precision arithmetic.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function sum35 (n As UInteger) As UInteger
  If n = 0 Then Return 0
  Dim As UInteger i, sum = 0
  For i = 1 To n
    If (i Mod 3 = 0) OrElse (i Mod 5 = 0) Then sum += i
  Next
  Return sum
End Function

Print "Sum of positive integers below 1000 divisible by 3 or 5 is : "; sum35(999)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Sum of positive integers below 1000 divisible by 3 or 5 is : 233168

```



## Go


```go
package main

import "fmt"

func main() {
    fmt.Println(s35(1000))
}

func s35(n int) int {
    n--
    threes := n / 3
    fives := n / 5
    fifteen := n / 15

    threes = 3 * threes * (threes + 1)
    fives = 5 * fives * (fives + 1)
    fifteen = 15 * fifteen * (fifteen + 1)

    n = (threes + fives - fifteen) / 2

    return n
}
```

{{out}}

```txt

233168

```

Extra credit:

```go
package main

import (
    "fmt"
    "math/big"
)

var (
    b1  = big.NewInt(1)
    b3  = big.NewInt(3)
    b5  = big.NewInt(5)
    b10 = big.NewInt(10)
    b15 = big.NewInt(15)
    b20 = big.NewInt(20)
)

func main() {
    fmt.Println(s35(new(big.Int).Exp(b10, b3, nil)))
    fmt.Println(s35(new(big.Int).Exp(b10, b20, nil)))
}

func s35(i *big.Int) *big.Int {
    j := new(big.Int).Sub(i, b1)
    sum2 := func(d *big.Int) *big.Int {
        n := new(big.Int).Quo(j, d)
        p := new(big.Int).Add(n, b1)
        return p.Mul(d, p.Mul(p, n))
    }
    s := sum2(b3)
    return s.Rsh(s.Sub(s.Add(s, sum2(b5)), sum2(b15)), 1)
}
```

{{out}}

```txt

233168
2333333333333333333316666666666666666668

```



## Groovy


```groovy
def sumMul = { n, f -> BigInteger n1 = (n - 1) / f; f * n1 * (n1 + 1) / 2 }
def sum35 = { sumMul(it, 3) + sumMul(it, 5) - sumMul(it, 15) }
```

Test Code:

```groovy
[(1000): 233168, (10e20): 233333333333333333333166666666666666666668].each { arg, value ->
    println "Checking $arg == $value"
    assert sum35(arg) == value
}
```

{{out}}

```txt
Checking 1000 == 233168
Checking 1.0E+21 == 233333333333333333333166666666666666666668
```



## Haskell

Also a method for calculating sum of multiples of any list of numbers.

```haskell
import Data.List (nub)

sum35 :: Integral a => a -> a
sum35 n = sumMul n 3 + sumMul n 5 - sumMul n 15

sumMul :: Integral a => a -> a -> a
sumMul n f = f * n1 * (n1 + 1) `div` 2
  where
    n1 = (n - 1) `div` f

-- Functions below are for variable length inputs

pairLCM :: Integral a => [a] -> [a]
pairLCM [] = []
pairLCM (x:xs) = (lcm x <$> xs) ++ pairLCM xs

sumMulS :: Integral a => a -> [a] -> a
sumMulS _ [] = 0
sumMulS n s = sum (sumMul n <$> ss) - sumMulS n (pairLCM ss)
  where
    ss = nub s

main :: IO ()
main =
  mapM_
    print
    [ sum35 1000
    , sum35 100000000000000000000000000000000
    , sumMulS 1000 [3, 5]
    , sumMulS 10000000 [2, 3, 5, 7, 11, 13]
    ]
```

{{out}}

```txt
233168
2333333333333333333333333333333316666666666666666666666666666668
233168
41426953573049
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both langauges.


```unicon
procedure main(A)
    n := (integer(A[1]) | 1000)-1
    write(sum(n,3)+sum(n,5)-sum(n,15))
end

procedure sum(n,m)
    return m*((n/m)*(n/m+1)/2)
end
```


Sample output:


```txt

->sm35
233168
->sm35 100000000000000000000
2333333333333333333316666666666666666668
->

```



## J



```J

mp =: $:~ :(+/ .*)  NB. matrix product
f =: (mp 0 = [: */ 3 5 |/ ])@:i.
assert 233168 -: f 1000   NB.  ******************  THIS IS THE ANSWER FOR 1000

```

For the efficient computation with large n, we start with observation that the sum of these multiples with the reversed list follows a pattern.

```J

g =: #~ (0 = [: */ 3 5&(|/))
assert  0  3  5  6  9 10 12 15 18 20 21 24 25 27 30 33 35 36 39 40 42 45 48 -: g i. 50
assert 48 48 47 46 48 46 47 48 48 47 46 48 46 47 48 48 47 46 48 46 47 48 48 -: (+ |.)g i. 50  NB. the pattern

assert (f -: -:@:(+/)@:(+|.)@:g@:i.) 50  NB. half sum of the pattern.

NB. continue...

```

Stealing the idea from the python implementation to use 3 simple patterns rather than 1 complicated pattern,

```J

   first =: 0&{
   last =: first + skip * <.@:(skip %~ <:@:(1&{) - first)
   skip =: 2&{
   terms =: >:@:<.@:(skip %~ last - first)
   sum_arithmetic_series =: -:@:(terms * first + last)  NB. sum_arithmetic_series FIRST LAST SKIP
                                                        NB. interval is [FIRST, LAST)
                                                        NB. sum_arithmetic_series is more general than required.

   (0,.10 10000 10000000000000000000x)(,"1 0"1 _)3 5 15x  NB. demonstration: form input vectors for 10, ten thousand, and 1*10^(many)
0                   10  3
0                   10  5
0                   10 15

0                10000  3
0                10000  5
0                10000 15

0 10000000000000000000  3
0 10000000000000000000  5
0 10000000000000000000 15



   (0,.10 10000 10000000000000000000x)+`-/"1@:(sum_arithmetic_series"1@:(,"1 0"1 _))3 5 15x
23 23331668 23333333333333333331666666666666666668

```




## JavaScript



### ES5


JavaScript is better equipped for flexibility than for scale. The value of
```JavaScript
 Number.MAX_SAFE_INTEGER
```
 is 9007199254740991, or 2^53 - 1 – resulting from an IEEE 754 double-precision floating point representation of numeric values).

As ''Number.MAX_SAFE_INTEGER < 1E20'' evaluates to ''true'', the most obvious JS attack on a solution for 1E20 might involve some string processing …

At more modest scales, however, we can generalise a little to allow for an arbitrary list of integer factors, and write a simple generate, filter and sum approach:


```JavaScript
(function (lstFactors, intExponent) {

    // [n] -> n -> n
    function sumMultiplesBelow(lstIntegers, limit) {
        return range(1, limit - 1).filter(function (x) {
            return isMultiple(lstIntegers, x);
        }).reduce(function (a, n) {
            return a + n;
        }, 0)
    }

    // [n] -> n -> bool
    function isMultiple(lst, n) {
        var i = lng;
        while (i--)
            if (n % (lst[i]) === 0) return true;
        return false;
    }

    // [m..n]
    function range(m, n) {
        var a = Array(n - m + 1),
            i = n + 1;
        while (i--) a[i - 1] = i;
        return a;
    }


    /*      TESTING     */

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

    var lng = lstFactors.length,
        lstSorted = lstFactors.slice(0).sort();

    var lstTable = [['Below', 'Sum']].concat(
        range(1, intExponent).map(function (x) {
            var pwr = Math.pow(10, x);

            return ['10^' + x, sumMultiplesBelow(lstSorted, pwr)];
        })
    );

    return 'For ' + JSON.stringify(lstFactors) + ':\n\n' +
        wikiTable(lstTable, true) + '\n\n' +
        JSON.stringify(lstTable);

})([3, 5], 8);
```



For [3,5]:

{| class="wikitable"
|-
! Below !! Sum
|-
| 10^1 || 23
|-
| 10^2 || 2318
|-
| 10^3 || 233168
|-
| 10^4 || 23331668
|-
| 10^5 || 2333316668
|-
| 10^6 || 233333166668
|-
| 10^7 || 23333331666668
|-
| 10^8 || 2333333316666668
|}


```JavaScript
 [["Below","Sum"],["10^1",23],["10^2",2318],["10^3",233168],
 ["10^4",23331668],["10^5",2333316668],["10^6",233333166668],
 ["10^7",23333331666668],["10^8",2333333316666668]]
```


### =With wheel increments=


```JavaScript
function sm35(n){
	var s=0, inc=[3,2,1,3,1,2,3]
	for (var j=6, i=0; i<n; j+=j==6?-j:1, i+=inc[j]) s+=i
	return s
}
```


### =With triangular numbers=


```JavaScript
function sm35(n){
	return tri(n,3) + tri(n,5) - tri(n,15)
	function tri(n, f) {
		n = Math.floor((n-1) / f)
		return f * n * (n+1) / 2
	}
}
```

'''This:'''

```JavaScript
for (var i=1, n=10; i<9; n*=10, i+=1) {
	document.write(10, '<sup>', i, '</sup> ',  sm35(n), '
')
}
```

{{out}}
 10<sup>1</sup> 23
 10<sup>2</sup> 2318
 10<sup>3</sup> 233168
 10<sup>4</sup> 23331668
 10<sup>5</sup> 2333316668
 10<sup>6</sup> 233333166668
 10<sup>7</sup> 23333331666668
 10<sup>8</sup> 2333333316666668



### ES6



```JavaScript
(() => {

    // Area under straight line
    // between first multiple and last.

    // sumMults :: Int -> Int -> Int
    const sumMults = (n, factor) => {
        const n1 = quot(n - 1, factor);
        return quot(factor * n1 * (n1 + 1), 2);
    };

    // sum35 :: Int -> Int
    const sum35 = n => sumMults(n, 3) + sumMults(n, 5) - sumMults(n, 15);


    // GENERIC ----------------------------------------------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // Integral a => a -> a -> a
    const quot = (n, m) => Math.floor(n / m);

    // TEST -------------------------------------------------------------------

    // Sums for 10^1 thru 10^8
    return enumFromTo(1, 8)
        .map(n => Math.pow(10, n))
        .reduce((a, x) => (
            a[x.toString()] = sum35(x),
            a
        ), {});
})();
```

{{Out}}

```JavaScript
{"10":23, "100":2318, "1000":233168, "10000":23331668,
"100000":2333316668, "1000000":233333166668, "10000000":23333331666668,
"100000000":2333333316666668}
```



## Java


```Java
class SumMultiples {
	public static long getSum(long n) {
		long sum = 0;
		for (int i = 3; i < n; i++) {
			if (i % 3 == 0 || i % 5 == 0) sum += i;
		}
		return sum;
	}
	public static void main(String[] args) {
		System.out.println(getSum(1000));
	}
}
```

{{out}}

```txt
233168
```



## jq


```jq

def sum_multiples(d):
 ((./d) | floor) |  (d * . * (.+1))/2 ;

# Sum of multiples of a or b that are less than . (the input)
def task(a;b):
 . - 1
 | sum_multiples(a) + sum_multiples(b) - sum_multiples(a*b);
```
Examples:

jq does not (yet) support arbitrary-precision integer arithmetic but converts large integers to floats, so:

```jq

1000 | task(3;5)  # => 233168

10e20 | task(3;5) # => 2.333333333333333e+41
```



## Julia

sum multiples of each, minus multiples of the least common multiple (lcm). Similar to MATLAB's version.

```Julia
multsum(n, m, lim) = sum(0:n:lim-1) + sum(0:m:lim-1) - sum(0:lcm(n,m):lim-1)
```

Output:

```txt
julia> multsum(3, 5, 1000)
233168

julia> multsum(3, 5, BigInt(10)^20)
2333333333333333333316666666666666666668

julia> @time multsum(3, 5, BigInt(10)^20)
elapsed time: 5.8114e-5 seconds seconds (3968 bytes allocated)
2333333333333333333316666666666666666668

julia> [(BigInt(10)^n, multsum(3, 5, BigInt(10)^n)) for n=0:20]
21-element Array{(BigInt,BigInt),1}:
 (1,0)
 (10,23)
 (100,2318)
 (1000,233168)
 (10000,23331668)
 (100000,2333316668)
 (1000000,233333166668)
 (10000000,23333331666668)
 (100000000,2333333316666668)
 (1000000000,233333333166666668)
 (10000000000,23333333331666666668)
 (100000000000,2333333333316666666668)
 (1000000000000,233333333333166666666668)
 (10000000000000,23333333333331666666666668)
 (100000000000000,2333333333333316666666666668)
 (1000000000000000,233333333333333166666666666668)
 (10000000000000000,23333333333333331666666666666668)
 (100000000000000000,2333333333333333316666666666666668)
 (1000000000000000000,233333333333333333166666666666666668)
 (10000000000000000000,23333333333333333331666666666666666668)
 (100000000000000000000,2333333333333333333316666666666666666668)
```

a slightly more efficient version

```Julia
multsum(n, lim) = (occ = div(lim-1, n); div(n*occ*(occ+1), 2))
multsum(n, m, lim) = multsum(n, lim) + multsum(m, lim) - multsum(lcm(n,m), lim)
```



## Kotlin


```scala
// version 1.1.2

import java.math.BigInteger

val big2  = BigInteger.valueOf(2)
val big3  = BigInteger.valueOf(3)
val big5  = BigInteger.valueOf(5)
val big15 = big3 * big5

fun sum35(n: Int) = (3 until n).filter { it % 3 == 0 || it % 5 == 0}.sum()

fun sum35(n: BigInteger): BigInteger {
    val nn    = n - BigInteger.ONE
    val num3  = nn / big3
    val end3  = num3 * big3
    val sum3  = (big3 + end3) * num3 / big2
    val num5  = nn / big5
    val end5  = num5 * big5
    val sum5  = (big5 + end5) * num5 / big2
    val num15 = nn / big15
    val end15 = num15 * big15
    val sum15 = (big15 + end15) * num15 / big2
    return sum3 + sum5 - sum15
}

fun main(args: Array<String>) {
    println("The sum of multiples of 3 or 5 below 1000 is ${sum35(1000)}")
    val big100k = BigInteger.valueOf(100_000L)
    val e20 = big100k * big100k * big100k * big100k
    println("The sum of multiples of 3 or 5 below 1e20 is ${sum35(e20)}")
}
```


{{out}}

```txt

The sum of multiples of 3 or 5 below 1000 is 233168
The sum of multiples of 3 or 5 below 1e20 is 2333333333333333333316666666666666666668

```



## Lasso


```Lasso
local(limit = 1)
while(#limit <= 100000) => {^
	local(s = 0)
	loop(-from=3,-to=#limit-1) => {
		not (loop_count % 3) || not (loop_count % 5) ? #s += loop_count
	}
	'The sum of multiples of 3 or 5 between 1 and '+(#limit-1)+' is: '+#s+'\r'
	#limit = integer(#limit->asString + '0')
^}
```

{{out}}

```txt
The sum of multiples of 3 or 5 between 1 and 0 is: 0
The sum of multiples of 3 or 5 between 1 and 9 is: 23
The sum of multiples of 3 or 5 between 1 and 99 is: 2318
The sum of multiples of 3 or 5 between 1 and 999 is: 233168
The sum of multiples of 3 or 5 between 1 and 9999 is: 23331668
The sum of multiples of 3 or 5 between 1 and 99999 is: 2333316668
```



## Limbo


Uses the IPints library when the result will be very large.


```Limbo
implement Sum3and5;

include "sys.m"; sys: Sys;
include "draw.m";
include "ipints.m"; ipints: IPints;
	IPint: import ipints;

Sum3and5: module {
	init: fn(nil: ref Draw->Context, args: list of string);
};

ints: array of ref IPint;

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	ipints = load IPints IPints->PATH;

	# We use 1, 2, 3, 5, and 15:
	ints = array[16] of ref IPint;
	for(i := 0; i < len ints; i++)
		ints[i] = IPint.inttoip(i);

	args = tl args;
	while(args != nil) {
		h := hd args;
		args = tl args;
		# If it's big enough that the result might not
		# fit inside a big, we use the IPint version.
		if(len h > 9) {
			sys->print("%s\n", isum3to5(IPint.strtoip(h, 10)).iptostr(10));
		} else {
			sys->print("%bd\n", sum3to5(big h));
		}
	}
}

triangle(n: big): big
{
	return((n * (n + big 1)) / big 2);
}

sum_multiples(n: big, limit: big): big
{
	return(n * triangle((limit - big 1) / n));
}

sum3to5(limit: big): big
{
	return(
		sum_multiples(big 3, limit) +
		sum_multiples(big 5, limit) -
		sum_multiples(big 15, limit));
}

itriangle(n: ref IPint): ref IPint
{
	return n.mul(n.add(ints[1])).div(ints[2]).t0;
}

isum_multiples(n: ref IPint, limit: ref IPint): ref IPint
{
	return n.mul(itriangle(limit.sub(ints[1]).div(n).t0));
}

isum3to5(limit: ref IPint): ref IPint
{
	return(
		isum_multiples(ints[3], limit).
		add(isum_multiples(ints[5], limit)).
		sub(isum_multiples(ints[15], limit)));
}

```


{{out}}

```txt
% sum3and5 1000 100000000000000000000
233168
2333333333333333333316666666666666666668
```



## Lingo


```lingo
on sum35 (n)
  res = 0
  repeat with i = 0 to (n-1)
    if i mod 3=0 OR i mod 5=0 then
      res = res + i
    end if
  end repeat
  return res
end
```



```lingo
put sum35(1000)
-- 233168
```



## LiveCode


```LiveCode
function sumUntil n
    repeat with i = 0 to (n-1)
        if i mod 3 = 0 or i mod 5 = 0 then
            add i to m
        end if
    end repeat
    return m
end sumUntil

put sumUntil(1000)  // 233168
```



## Lua

{{trans|Tcl}}

```Lua

function tri (n) return n * (n + 1) / 2 end

function sum35 (n)
	n = n - 1
	return	(	3 * tri(math.floor(n / 3)) +
			5 * tri(math.floor(n / 5)) -
			15 * tri(math.floor(n / 15))
		)
end

print(sum35(1000))
print(sum35(1e+20))

```

{{out}}

```txt

233168
2.3333333333333e+39

```



## Maple

By using symbolic function <code>sum</code> instead of numeric function <code>add</code> the program <code>F</code> will run O(1) rather than O(n).

```Maple

F := unapply(  sum(3*i,i=1..floor((n-1)/3))
             + sum(5*i,i=1..floor((n-1)/5))
             - sum(15*i,i=1..floor((n-1)/15)), n);

F(1000);

F(10^20);

```

Output:

```txt

                               2                                      2
               3      /1     2\    3      /1     2\   5      /1     4\
     F := n -> - floor|- n + -|  - - floor|- n + -| + - floor|- n + -|
               2      \3     3/    2      \3     3/   2      \5     5/

                                                2
          5      /1     4\   15      /1      14\    15      /1      14\
        - - floor|- n + -| - -- floor|-- n + --|  + -- floor|-- n + --|
          2      \5     5/   2       \15     15/    2       \15     15/


                                   233168

                  2333333333333333333316666666666666666668

```



## Mathematica


```mathematica
sum35[n_] :=
 Sum[k, {k, 3, n - 1, 3}] + Sum[k, {k, 5, n - 1, 5}] -
  Sum[k, {k, 15, n - 1, 15}]

sum35[1000]
```

{{out}}

```txt
233168
```


```mathematica
sum35[10^20]
```

{{out}}

```txt
233333333333333333333166666666666666666668
```


Another alternative is

```mathematica
 Union @@ Range[0, 999, {3, 5}] // Tr
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
n=1:999; sum(n(mod(n,3)==0 | mod(n,5)==0))
```


```txt
ans =  233168
```

Another alternative is

```MATLAB
n=1000; sum(0:3:n-1)+sum(0:5:n-1)-sum(0:15:n-1)
```

Of course, it's more efficient to use [http://mathforum.org/library/drmath/view/57919.html Gauss' approach] of adding subsequent integers:

```MATLAB
n=999;
n3=floor(n/3);
n5=floor(n/5);
n15=floor(n/15);
(3*n3*(n3+1) + 5*n5*(n5+1) - 15*n15*(n15+1))/2
```


```txt
ans =  233168
```



## Maxima


```Maxima
sumi(n, incr):= block([kmax: quotient(n, incr)],
  ''(ev(sum(incr*k, k, 1, kmax), simpsum)));

sum35(n):=sumi(n, 3) + sumi(n, 5) - sumi(n, 15);

sum35(1000);
sum35(10^20);
```

Output:

```txt
(%i16) sum35(1000);
(%o16)                              234168
(%i17) sum35(10^20);
(%o17)             2333333333333333333416666666666666666668
```


=={{header|MK-61/52}}==
<lang>П1	0	П0	3	П4	ИП4	3	/	{x}	x#0
17	ИП4	5	/	{x}	x=0	21	ИП0	ИП4	+
П0	КИП4	ИП1	ИП4	-	x=0	05	ИП0	С/П
```


Input: ''n''.

Output for n = 1000: ''233168''.


## NetRexx

Portions translation of [[#Perl 6|Perl 6]]

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary
numeric digits 40

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method summing(maxLimit = 1000) public static
  mult = 0
  loop mv = 0 while mv < maxLimit
    if mv // 3 = 0 | mv // 5 = 0 then
      mult = mult + mv
    end mv
  return mult

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- translation of perl 6
method sum_mults(first, limit) public static
  last = limit - 1
  last = last - last // first
  sum = (last / first) * (first + last) % 2
  return sum

method sum35(maxLimit) public static
  return sum_mults(3, maxLimit) + sum_mults(5, maxLimit) - sum_mults(15, maxLimit)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static

  offset = 30
  incr = 10

  say 'Limit'.right(offset) || '|' || 'Sum'
  say '-'.copies(offset)    || '+' || '-'.copies(60)
  timing = System.nanoTime
  sum = summing()
  timing = System.nanoTime - timing
  say 1000.format.right(offset)'|'sum
  say 'Elapsed time:' Rexx(timing * 1e-9).format(4, 6)'s'
  say

  say 'Limit'.right(offset) || '|' || 'Sum'
  say '-'.copies(offset)    || '+' || '-'.copies(60)
  tmax = 1e+6
  timing = System.nanoTime
  mm = 1
  loop while mm <= tmax
    say mm.right(offset)'|'summing(mm)
    mm = mm * incr
    end
  timing = System.nanoTime - timing
  say 'Elapsed time:' Rexx(timing * 1e-9).format(4, 6)'s'
  say

  say 'Limit'.right(offset) || '|' || 'Sum'
  say '-'.copies(offset)    || '+' || '-'.copies(60)
  timing = System.nanoTime
  sum = sum35(1000)
  timing = System.nanoTime - timing
  say 1000.format.right(offset)'|'sum
  say 'Elapsed time:' Rexx(timing * 1e-9).format(4, 6)'s'
  say

  say 'Limit'.right(offset) || '|' || 'Sum'
  say '-'.copies(offset)    || '+' || '-'.copies(60)
  tmax = 1e+27
  timing = System.nanoTime
  mm = 1
  loop while mm <= tmax
    say mm.right(offset)'|'sum35(mm)
    mm = mm * incr
    end
  timing = System.nanoTime - timing
  say 'Elapsed time:' Rexx(timing * 1e-9).format(4, 6)'s'
  say
  return

```

{{out}}

```txt

                         Limit|Sum
------------------------------+------------------------------------------------------------
                          1000|233168
Elapsed time:    0.097668s

                         Limit|Sum
------------------------------+------------------------------------------------------------
                             1|0
                            10|23
                           100|2318
                          1000|233168
                         10000|23331668
                        100000|2333316668
                       1000000|233333166668
Elapsed time:   11.593837s

                         Limit|Sum
------------------------------+------------------------------------------------------------
                          1000|233168
Elapsed time:    0.000140s

                         Limit|Sum
------------------------------+------------------------------------------------------------
                             1|0
                            10|23
                           100|2318
                          1000|233168
                         10000|23331668
                        100000|2333316668
                       1000000|233333166668
                      10000000|23333331666668
                     100000000|2333333316666668
                    1000000000|233333333166666668
                   10000000000|23333333331666666668
                  100000000000|2333333333316666666668
                 1000000000000|233333333333166666666668
                10000000000000|23333333333331666666666668
               100000000000000|2333333333333316666666666668
              1000000000000000|233333333333333166666666666668
             10000000000000000|23333333333333331666666666666668
            100000000000000000|2333333333333333316666666666666668
           1000000000000000000|233333333333333333166666666666666668
          10000000000000000000|23333333333333333331666666666666666668
         100000000000000000000|2333333333333333333316666666666666666668
        1000000000000000000000|233333333333333333333166666666666666666668
       10000000000000000000000|23333333333333333333331666666666666666666668
      100000000000000000000000|2333333333333333333333316666666666666666666668
     1000000000000000000000000|233333333333333333333333166666666666666666666668
    10000000000000000000000000|23333333333333333333333331666666666666666666666668
   100000000000000000000000000|2333333333333333333333333316666666666666666666666668
  1000000000000000000000000000|233333333333333333333333333166666666666666666666666668
Elapsed time:    0.005545s

```



## Nim


```nim
proc sum35(n: int): int =
  for x in 0 .. <n:
    if x mod 3 == 0 or x mod 5 == 0:
      result += x

echo sum35(1000)
```


With BigInts:
{{trans|Perl 6}}

```nim
import bigints

proc sumMults(first: int32, limit: BigInt): BigInt =
  var last = limit - 1
  last -= last mod first
  (last div first) * (last + first) div 2

proc sum35(n: BigInt): BigInt =
  result = sumMults(3, n)
  result += sumMults(5, n)
  result -= sumMults(15, n)

var x = 1.initBigInt
while x < "1000000000000000000000000000000".initBigInt:
  echo sum35 x
  x *= 10
```

Output:

```txt
-0
23
2318
233168
23331668
2333316668
233333166668
23333331666668
2333333316666668
233333333166666668
23333333331666666668
2333333333316666666668
233333333333166666666668
23333333333331666666666668
2333333333333316666666666668
233333333333333166666666666668
23333333333333331666666666666668
2333333333333333316666666666666668
233333333333333333166666666666666668
23333333333333333331666666666666666668
2333333333333333333316666666666666666668
233333333333333333333166666666666666666668
23333333333333333333331666666666666666666668
2333333333333333333333316666666666666666666668
233333333333333333333333166666666666666666666668
23333333333333333333333331666666666666666666666668
2333333333333333333333333316666666666666666666666668
233333333333333333333333333166666666666666666666666668
23333333333333333333333333331666666666666666666666666668
2333333333333333333333333333316666666666666666666666666668
```



## Objeck

{{trans|Java}}

```objeck
class SumMultiples {
  function : native : GetSum(n : Int) ~ Int {
    sum := 0;
    for(i := 3; i < n; i++;) {
      if(i % 3 = 0 | i % 5 = 0) {
        sum += i;
      };
    };

    return sum;
  }

  function : Main(args : String[]) ~ Nil {
    GetSum(1000)->PrintLine();
  }
}

```


Output:

```txt

233168

```



## OCaml


```ocaml
let sum_mults n =
        let sum = ref 0 in
        for i = 3 to (n - 1) do
                if (i mod 3) = 0 || (i mod 5) = 0 then
                        sum := !sum + i;
        done;
        !sum;;

print_endline (string_of_int (sum_mults 1000));;

```

{{out}}

```txt
233168
```



## Oforth



```Oforth
999 seq filter(#[ dup 3 mod 0 == swap 5 mod 0 == or ]) sum println
```


Output:

```txt

233168

```



## Ol


```scheme

(print
(fold (lambda (s x)
         (+ s (if (or (zero? (remainder x 3)) (zero? (remainder x 5))) x 0)))
   0 (iota 1000)))
; ==> 233168

```



## PARI/GP


```parigp
ct(n,k)=n=n--\k;k*n*(n+1)/2;
a(n)=ct(n,3)+ct(n,5)-ct(n,15);
a(1000)
a(1e20)
```

{{output}}

```txt

%1 = 233168
%2 = 2333333333333333333316666666666666666668

```



## Pascal


{{works with|Free Pascal|2.6.2}}


```Pascal
program Sum3sAnd5s;

function Multiple(x, y: integer): Boolean;
  { Is X a multiple of Y? }
   begin
      Multiple := (X mod Y) = 0
   end;

function SumMultiples(n: integer): longint;
  { Return the sum of all multiples of 3 or 5. }
   var i: integer; sum: longint;
   begin
      sum := 0;
      for i := 1 to pred(n) do
         if Multiple(i, 3) or Multiple(i, 5) then
           sum := sum + i;
      SumMultiples := sum
   end;

begin
   { Show sum of all multiples less than 1000. }
   writeln(SumMultiples(1000))
end.
```


### alternative

using gauss summation formula, but subtract double counted.
adapted translation of [[#Tcl|Tcl]]

```Pascal
program sum35;
//sum of all positive multiples of 3 or 5 below n

function cntSumdivisibleBelowN(n: Uint64;b:Uint64):Uint64;
var
  cnt : Uint64;
Begin
  cnt := (n-1) DIV b;
// Gauß summation formula * b
  cntSumdivisibleBelowN := (cnt*(cnt+1) DIV 2 ) *b;
end;
const
  n = 1000;

var
  sum: Uint64;
begin
  sum := cntSumdivisibleBelowN(n,3)+cntSumdivisibleBelowN(n,5);
//subtract double counted like 15
  sum := sum-cntSumdivisibleBelowN(n,3*5);
  writeln(sum);
end.
```

output

```txt
233168
```



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;
use List::Util qw( sum ) ;

sub sum_3_5 {
   my $limit = shift ;
   return sum grep { $_ % 3 == 0 || $_ % 5 == 0 } ( 1..$limit - 1 ) ;
}

print "The sum is " . sum_3_5( 1000 ) . " !\n" ;
```

{{Out}}

```txt
The sum is 233168 !
```


{{Trans|Tcl}}
An alternative approach, using the analytical solution from the Tcl example.

```Perl
use feature 'say';
sub tri
{
    my $n = shift;
    return $n*($n+1) / 2;
}

sub sum
{
    my $n = (shift) - 1;
    (3 * tri( int($n/3) ) + 5 * tri( int($n/5) ) - 15 * tri( int($n/15) ) );
}

say sum(1e3);
use bigint; # Machine precision was sufficient for the first calculation
say sum(1e20);
```

{{Out}}

```txt
233168
2333333333333333333316666666666666666668
```

Interestingly, the prime factorization of the second result produces a 35 digit prime number.


## Perl 6


```perl6
sub sum35($n) { [+] grep * %% (3|5), ^$n; }

say sum35 1000;
```

{{out}}

```txt
233168
```

Here's an analytical approach that scales much better for large values.

```perl6
sub sum-mults($first, $limit) {
    (my $last = $limit - 1) -= $last % $first;
    ($last div $first) * ($first + $last) div 2;
}

sub sum35(\n) {
    sum-mults(3,n) + sum-mults(5,n) - sum-mults(15,n);
}

say sum35($_) for 1,10,100...10**30;
```

{{out}}

```txt
0
23
2318
233168
23331668
2333316668
233333166668
23333331666668
2333333316666668
233333333166666668
23333333331666666668
2333333333316666666668
233333333333166666666668
23333333333331666666666668
2333333333333316666666666668
233333333333333166666666666668
23333333333333331666666666666668
2333333333333333316666666666666668
233333333333333333166666666666666668
23333333333333333331666666666666666668
2333333333333333333316666666666666666668
233333333333333333333166666666666666666668
23333333333333333333331666666666666666666668
2333333333333333333333316666666666666666666668
233333333333333333333333166666666666666666666668
23333333333333333333333331666666666666666666666668
2333333333333333333333333316666666666666666666666668
233333333333333333333333333166666666666666666666666668
23333333333333333333333333331666666666666666666666666668
2333333333333333333333333333316666666666666666666666666668
233333333333333333333333333333166666666666666666666666666668
```



## Phix

{{trans|C}}
{{libheader|mpfr}}
Fast analytical version with arbitrary precision

```Phix
include mpfr.e

procedure sum_multiples(mpz result, limit, integer f)
    mpz m = mpz_init()
    mpz_sub_ui(m, limit, 1)
    {} = mpz_fdiv_q_ui(m, m, f)
    mpz_set(result, m)
    mpz_add_ui(result, result, 1);
    mpz_mul(result, result, m)
    mpz_mul_si(result, result, f)
    mpz_fdiv_q_2exp(result, result, 1)
    m = mpz_free(m)
end procedure

mpz {res,tmp,limit} = mpz_inits(3)
for i=0 to 20 do
    string sp = repeat(' ',20-i)
    printf(1,sp&"1"&repeat('0',i)&sp)
    mpz_ui_pow_ui(limit,10,i)
    sum_multiples(res, limit, 3)
    sum_multiples(tmp, limit, 5)
    mpz_add(res,res,tmp)
    sum_multiples(tmp, limit, 15)
    mpz_sub(res,res,tmp)
    printf(1," %s\n",mpz_get_str(res))
end for
{res,tmp,limit} = mpz_free({res,tmp,limit})
```

{{Out}}

```txt

                    1                     0
                   10                    23
                  100                   2318
                 1000                  233168
                10000                 23331668
               100000                2333316668
              1000000               233333166668
             10000000              23333331666668
            100000000             2333333316666668
           1000000000            233333333166666668
          10000000000           23333333331666666668
         100000000000          2333333333316666666668
        1000000000000         233333333333166666666668
       10000000000000        23333333333331666666666668
      100000000000000       2333333333333316666666666668
     1000000000000000      233333333333333166666666666668
    10000000000000000     23333333333333331666666666666668
   100000000000000000    2333333333333333316666666666666668
  1000000000000000000   233333333333333333166666666666666668
 10000000000000000000  23333333333333333331666666666666666668
100000000000000000000 2333333333333333333316666666666666666668

```



## PicoLisp


```PicoLisp
(de sumMul (N F)
   (let N1 (/ (dec N) F)
      (*/ F N1 (inc N1) 2) ) )

(for I 20
   (let N (** 10 I)
      (println
         (-
            (+ (sumMul N 3) (sumMul N 5))
            (sumMul N 15) ) ) ) )

(bye)
```



## PL/I


```PL/I
threeor5: procedure options (main);      /* 8 June 2014 */
   declare (i, n) fixed(10), sum fixed (31) static initial (0);

   get (n);
   put ('The number of multiples of 3 or 5 below ' || trim(n) || ' is');

   do i = 1 to n-1;
      if mod(i, 3) = 0 | mod(i, 5) = 0 then sum = sum + i;
   end;

   put edit ( trim(sum) ) (A);

end threeor5;
```

Outputs:

```txt

The number of multiples of 3 or 5 below 1000 is 233168
The number of multiples of 3 or 5 below 10000 is 23331668
The number of multiples of 3 or 5 below 100000 is 2333316668
The number of multiples of 3 or 5 below 1000000 is 233333166668
The number of multiples of 3 or 5 below 10000000 is 23333331666668
The number of multiples of 3 or 5 below 100000000 is 2333333316666668
```



## PowerShell


```powershell

function SumMultiples ( [int]$Base, [int]$Upto )
    {
    $X = ( $Upto - ( $Upto % $Base ) ) / $Base
    $Sum = ( $X * $X + $X ) * $Base / 2
    Return $Sum
    }

#  Calculate the sum of the multiples of 3 and 5 up to 1000
( SumMultiples -Base 3 -Upto 1000 ) + ( SumMultiples -Base 5 -Upto 1000 ) - ( SumMultiples -Base 15 -Upto 1000 )

```

{{out}}

```txt

234168

```

Simply change the variable type to handle really, really big number.

```powershell

function SumMultiples ( [bigint]$Base, [bigint]$Upto )
    {
    $X = ( $Upto - ( $Upto % $Base ) ) / $Base
    $Sum = ( $X * $X + $X ) * $Base / 2
    Return $Sum
    }

#  Calculate the sum of the multiples of 3 and 5 up to 10 ^ 210
$Upto = [bigint]::Pow( 10, 210 )
( SumMultiples -Base 3 -Upto $Upto ) + ( SumMultiples -Base 5 -Upto $Upto ) - ( SumMultiples -Base 15 -Upto $Upto )

```

{{out}}

```txt

233333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334166666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666668

```

Here is a cmdlet that will provide the sum of unique multiples of any group of numbers below a given limit. I haven't attempted the extra credit here as the math is too complex for me at the moment.

```Powershell
function Get-SumOfMultiples
{
    Param
    (
        [Parameter(
        Position=0)]
        $Cap = 1000,

        [Parameter(
        ValueFromRemainingArguments=$True)]
        $Multiplier = (3,5)
    )

    $Multiples = @()
    $Sum = 0
    $multiplier |
      ForEach-Object {
        For($i = 1; $i -lt $Cap; $i ++)
        {
          If($i % $_ -eq 0)
          {$Multiples += $i}
        }
      }

     $Multiples |
         select -Unique |
         ForEach-Object {
            $Sum += $_
         }
     $Sum
}
```

{{out}}

```txt
Get-SumOfMultiples
```


```txt
233168
```

{{out}}

```txt
Get-SumOfMultiples 1500 3 5 7 13
```


```txt
649444
```



## Prolog


### Slow version


```Prolog
sum_of_multiples_of_3_and_5_slow(N, TT) :-
	sum_of_multiples_of_3_and_5(N, 1, 0, TT).

sum_of_multiples_of_3_and_5(N, K, S, S) :-
	3 * K >= N.

sum_of_multiples_of_3_and_5(N, K, C, S) :-
	T3 is 3 * K, T3 < N,
	C3 is C + T3,
	T5 is 5 * K,
	(   (T5 < N, K mod 3 =\= 0)
	->  C5 is C3 + T5
	;   C5 = C3),
	K1 is K+1,
	sum_of_multiples_of_3_and_5(N, K1, C5, S).


```



### Fast version


```Polog
sum_of_multiples_of_3_and_5_fast(N, TT):-
	maplist(compute_sum(N), [3,5,15], [TT3, TT5, TT15]),
	TT is TT3 + TT5 - TT15.

compute_sum(N, N1, Sum) :-
	(   N mod N1 =:= 0
	->  N2 is N div N1 - 1
	;   N2 is N div N1),
	Sum is N1 * N2 * (N2 + 1) / 2.

```


Output :

```txt
 ?- sum_of_multiples_of_3_and_5_slow(1000, TT).
TT = 233168 .

 ?- sum_of_multiples_of_3_and_5_fast(100000000000000000000, TT).
TT = 2333333333333333333316666666666666666668.

```



## PureBasic


```PureBasic

EnableExplicit

Procedure.q SumMultiples(Limit.q)
  If Limit < 0 : Limit = -Limit : EndIf; convert negative numbers to positive
  Protected.q i, sum = 0
  For i = 3 To Limit - 1
    If i % 3 = 0 Or i % 5 = 0
      sum + i
    EndIf
  Next
  ProcedureReturn sum
EndProcedure

If OpenConsole()
  PrintN("Sum of numbers below 1000 which are multiples of 3 or 5 is : " + SumMultiples(1000))
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


{{out}}

```txt

Sum of numbers below 1000 which are multiples of 3 or 5 is : 233168

```



## Python

Three ways of performing the calculation are shown including direct calculation of the value without having to do explicit sums in sum35c()

```python
def sum35a(n):
    'Direct count'
    # note: ranges go to n-1
    return sum(x for x in range(n) if x%3==0 or x%5==0)

def sum35b(n):
    "Count all the 3's; all the 5's; minus double-counted 3*5's"
    # note: ranges go to n-1
    return sum(range(3, n, 3)) + sum(range(5, n, 5)) - sum(range(15, n, 15))

def sum35c(n):
    'Sum the arithmetic progressions: sum3 + sum5 - sum15'
    consts = (3, 5, 15)
    # Note: stop at n-1
    divs = [(n-1) // c for c in consts]
    sums = [d*c*(1+d)/2 for d,c in zip(divs, consts)]
    return sums[0] + sums[1] - sums[2]

#test
for n in range(1001):
    sa, sb, sc = sum35a(n), sum35b(n), sum35c(n)
    assert sa == sb == sc  # python tests aren't like those of c.

print('For n = %7i -> %i\n' % (n, sc))

# Pretty patterns
for p in range(7):
    print('For n = %7i -> %i' % (10**p, sum35c(10**p)))

# Scalability
p = 20
print('\nFor n = %20i -> %i' % (10**p, sum35c(10**p)))
```


{{out}}

```txt
For n =    1000 -> 233168

For n =       1 -> 0
For n =      10 -> 23
For n =     100 -> 2318
For n =    1000 -> 233168
For n =   10000 -> 23331668
For n =  100000 -> 2333316668
For n = 1000000 -> 233333166668

For n = 100000000000000000000 -> 2333333333333333333316666666666666666668
```



Or, more generally – taking the area under the straight line between the first multiple and the last:
{{Works with|Python|3.7}}

```python
'''Summed multiples of 3 and 5 up to n'''


# sum35 :: Int -> Int
def sum35(n):
    '''Sum of all positive multiples
       of 3 or 5 below n.
    '''
    f = sumMults(n)
    return f(3) + f(5) - f(15)


# sumMults :: Int -> Int -> Int
def sumMults(n):
    '''Area under a straight line between
       the first multiple and the last.
    '''
    def go(n, m):
        n1 = (n - 1) // m
        return (m * n1 * (n1 + 1)) // 2
    return lambda x: go(n, x)


# TEST ----------------------------------------------------
def main():
    '''Tests for [10^1 .. 10^5], and [10^8 .. 10^25]
    '''
    print(
        fTable(__doc__ + ':\n')(lambda x: '10E' + str(x))(
            str
        )(compose(sum35)(lambda x: 10**x))(
            enumFromTo(1)(5) + enumFromTo(18)(25)
        )
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Summed multiples of 3 and 5 up to n:

 10E1 -> 23
 10E2 -> 2318
 10E3 -> 233168
 10E4 -> 23331668
 10E5 -> 2333316668
10E18 -> 233333333333333333166666666666666668
10E19 -> 23333333333333333331666666666666666668
10E20 -> 2333333333333333333316666666666666666668
10E21 -> 233333333333333333333166666666666666666668
10E22 -> 23333333333333333333331666666666666666666668
10E23 -> 2333333333333333333333316666666666666666666668
10E24 -> 233333333333333333333333166666666666666666666668
10E25 -> 23333333333333333333333331666666666666666666666668
```



## Q


```q
s35:{sum {?[(0=x mod 3) | 0=x mod 5;x;0]} each 1+til x - 1}
s35 each 10 100 1000 10000 1000000
```


Extra credit, using the summation formula:


```q
sn:{x*(x+1)%2}      / Sum of 1 to n
s35:{a:x-1; (3*sn floor a%3) + (5*sn floor a%5) - (15*sn floor a%15)}
s35 e+10
```



## R



```rsplus
m35 = function(n) sum(unique(c(
    seq(3, n-1, by = 3), seq(5, n-1, by = 5))))
m35(1000)   # 233168
```



## Racket


```racket

#lang racket
(require math)

;;; A naive solution
(define (naive k)
  (for/sum ([n (expt 10 k)]
            #:when (or (divides? 3 n) (divides? 5 n)))
    n))

(for/list ([k 7]) (naive k))


;;; Using the formula for an arithmetic sum
(define (arithmetic-sum a1 n Δa)
  ; returns a1+a2+...+an
  (define an (+ a1 (* (- n 1) Δa)))
  (/ (* n (+ a1 an)) 2))

(define (analytical k)
  (define 10^k (expt 10 k))
  (define (n d) (quotient (- 10^k 1) d))
  (+    (arithmetic-sum  3 (n  3)  3)
        (arithmetic-sum  5 (n  5)  5)
     (- (arithmetic-sum 15 (n 15) 15))))

(for/list ([k 20]) (analytical k))

```

Output:

```racket

'(0 23 2318 233168 23331668 2333316668 233333166668)
'(0
  23
  2318
  233168
  23331668
  2333316668
  233333166668
  23333331666668
  2333333316666668
  233333333166666668
  23333333331666666668
  2333333333316666666668
  233333333333166666666668
  23333333333331666666666668
  2333333333333316666666666668
  233333333333333166666666666668
  23333333333333331666666666666668
  2333333333333333316666666666666668
  233333333333333333166666666666666668
  23333333333333333331666666666666666668)

```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* 14.05.2013 Walter Pachl
**********************************************************************/
Say mul35()
exit
mul35:
s=0
Do i=1 To 999
  If i//3=0 | i//5=0 Then
    s=s+i
  End
Return s
```

Output:

```txt
233168
```



### version 2


```rexx
/* REXX ***************************************************************
* Translation from Perl6->NetRexx->REXX
* 15.05.2013 Walter Pachl
**********************************************************************/
Numeric Digits 100
call time 'R'
n=1
Do i=1 To 30
  Say right(n,30) sum35(n)
  n=n*10
  End
Say time('E') 'seconds'
Exit

sum35: Procedure
  Parse Arg maxLimit
  return sum_mults(3, maxLimit) + sum_mults(5, maxLimit) - sum_mults(15, maxLimit)

sum_mults: Procedure
  Parse Arg first, limit
  last = limit - 1
  last = last - last // first
  sum = (last % first) * (first + last) % 2
  return sum
```

Output:

```txt
                             1 0
                            10 23
                           100 2318
                          1000 233168
                         10000 23331668
                        100000 2333316668
                       1000000 233333166668
                      10000000 23333331666668
                     100000000 2333333316666668
                    1000000000 233333333166666668
                   10000000000 23333333331666666668
                  100000000000 2333333333316666666668
                 1000000000000 233333333333166666666668
                10000000000000 23333333333331666666666668
               100000000000000 2333333333333316666666666668
              1000000000000000 233333333333333166666666666668
             10000000000000000 23333333333333331666666666666668
            100000000000000000 2333333333333333316666666666666668
           1000000000000000000 233333333333333333166666666666666668
          10000000000000000000 23333333333333333331666666666666666668
         100000000000000000000 2333333333333333333316666666666666666668
        1000000000000000000000 233333333333333333333166666666666666666668
       10000000000000000000000 23333333333333333333331666666666666666666668
      100000000000000000000000 2333333333333333333333316666666666666666666668
     1000000000000000000000000 233333333333333333333333166666666666666666666668
    10000000000000000000000000 23333333333333333333333331666666666666666666666668
   100000000000000000000000000 2333333333333333333333333316666666666666666666666668
  1000000000000000000000000000 233333333333333333333333333166666666666666666666666668
 10000000000000000000000000000 23333333333333333333333333331666666666666666666666666668
100000000000000000000000000000 2333333333333333333333333333316666666666666666666666666668
0 milliseconds with rexx m35a > m35a.txt
46 millisecond with rexx m35a
```



### version 3

This version automatically adjusts the numeric digits. and a little extra code was added to format the output nicely.

The formula used is a form of the Gauss Summation formula.

```rexx
/*REXX program counts all  integers  from  1 ──► N─1   that are multiples of  3  or  5. */
parse arg N t .                                  /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=1000                   /*Not specified?  Then use the default.*/
if t=='' | t==","  then t=   1                   /* "      "         "   "   "      "   */
numeric digits 1000;    w=2+length(t)            /*W: used for formatting 'e' part of Y.*/
say 'The sum of all positive integers that are a multiple of  3  and  5  are:'
say                                              /* [↓]  change the format/look of nE+nn*/
     do t;   parse value format(N,2,1,,0) 'E0'  with  m 'E' _ .      /*get the exponent.*/
     y=right((m/1)'e' || (_+0), w)"-1"           /*this fixes a bug in a certain REXX.  */
     z=n-1;  if t==1  then y=z                   /*handle a special case of a one─timer.*/
     say 'integers from 1 ──►'    y    " is "    sumDiv(z,3) + sumDiv(z,5) - sumDiv(z,3*5)
     N=N'0'                                      /*fast *10 multiply for next iteration.*/
     end   /*t*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sumDiv: procedure;  parse arg x,d;     $=x % d;            return d * $ * ($+1) % 2
```

'''output'''   when using the default input:

```txt

The sum of all positive integers that are a multiple of  3  and  5  are:

integers from 1 ──► 999  is  233168

```

'''output'''   when using the input of:   <tt> 1   85 </tt>
<pre style="height:80ex">
The sum of all positive integers that are a multiple of  3  and  5  are:

integers from 1 ──►  1e0-1  is  0
integers from 1 ──►  1e1-1  is  23
integers from 1 ──►  1e2-1  is  2318
integers from 1 ──►  1e3-1  is  233168
integers from 1 ──►  1e4-1  is  23331668
integers from 1 ──►  1e5-1  is  2333316668
integers from 1 ──►  1e6-1  is  233333166668
integers from 1 ──►  1e7-1  is  23333331666668
integers from 1 ──►  1e8-1  is  2333333316666668
integers from 1 ──►  1e9-1  is  233333333166666668
integers from 1 ──► 1e10-1  is  23333333331666666668
integers from 1 ──► 1e11-1  is  2333333333316666666668
integers from 1 ──► 1e12-1  is  233333333333166666666668
integers from 1 ──► 1e13-1  is  23333333333331666666666668
integers from 1 ──► 1e14-1  is  2333333333333316666666666668
integers from 1 ──► 1e15-1  is  233333333333333166666666666668
integers from 1 ──► 1e16-1  is  23333333333333331666666666666668
integers from 1 ──► 1e17-1  is  2333333333333333316666666666666668
integers from 1 ──► 1e18-1  is  233333333333333333166666666666666668
integers from 1 ──► 1e19-1  is  23333333333333333331666666666666666668
integers from 1 ──► 1e20-1  is  2333333333333333333316666666666666666668
integers from 1 ──► 1e21-1  is  233333333333333333333166666666666666666668
integers from 1 ──► 1e22-1  is  23333333333333333333331666666666666666666668
integers from 1 ──► 1e23-1  is  2333333333333333333333316666666666666666666668
integers from 1 ──► 1e24-1  is  233333333333333333333333166666666666666666666668
integers from 1 ──► 1e25-1  is  23333333333333333333333331666666666666666666666668
integers from 1 ──► 1e26-1  is  2333333333333333333333333316666666666666666666666668
integers from 1 ──► 1e27-1  is  233333333333333333333333333166666666666666666666666668
integers from 1 ──► 1e28-1  is  23333333333333333333333333331666666666666666666666666668
integers from 1 ──► 1e29-1  is  2333333333333333333333333333316666666666666666666666666668
integers from 1 ──► 1e30-1  is  233333333333333333333333333333166666666666666666666666666668
integers from 1 ──► 1e31-1  is  23333333333333333333333333333331666666666666666666666666666668
integers from 1 ──► 1e32-1  is  2333333333333333333333333333333316666666666666666666666666666668
integers from 1 ──► 1e33-1  is  233333333333333333333333333333333166666666666666666666666666666668
integers from 1 ──► 1e34-1  is  23333333333333333333333333333333331666666666666666666666666666666668
integers from 1 ──► 1e35-1  is  2333333333333333333333333333333333316666666666666666666666666666666668
integers from 1 ──► 1e36-1  is  233333333333333333333333333333333333166666666666666666666666666666666668
integers from 1 ──► 1e37-1  is  23333333333333333333333333333333333331666666666666666666666666666666666668
integers from 1 ──► 1e38-1  is  2333333333333333333333333333333333333316666666666666666666666666666666666668
integers from 1 ──► 1e39-1  is  233333333333333333333333333333333333333166666666666666666666666666666666666668
integers from 1 ──► 1e40-1  is  23333333333333333333333333333333333333331666666666666666666666666666666666666668
integers from 1 ──► 1e41-1  is  2333333333333333333333333333333333333333316666666666666666666666666666666666666668
integers from 1 ──► 1e42-1  is  233333333333333333333333333333333333333333166666666666666666666666666666666666666668
integers from 1 ──► 1e43-1  is  23333333333333333333333333333333333333333331666666666666666666666666666666666666666668
integers from 1 ──► 1e44-1  is  2333333333333333333333333333333333333333333316666666666666666666666666666666666666666668
integers from 1 ──► 1e45-1  is  233333333333333333333333333333333333333333333166666666666666666666666666666666666666666668
integers from 1 ──► 1e46-1  is  23333333333333333333333333333333333333333333331666666666666666666666666666666666666666666668
integers from 1 ──► 1e47-1  is  2333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666668
integers from 1 ──► 1e48-1  is  233333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666668
integers from 1 ──► 1e49-1  is  23333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666668
integers from 1 ──► 1e50-1  is  2333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666668
integers from 1 ──► 1e51-1  is  233333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666668
integers from 1 ──► 1e52-1  is  23333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e53-1  is  2333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e54-1  is  233333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e55-1  is  23333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e56-1  is  2333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e57-1  is  233333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e58-1  is  23333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e59-1  is  2333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e60-1  is  233333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e61-1  is  23333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e62-1  is  2333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e63-1  is  233333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e64-1  is  23333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e65-1  is  2333333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e66-1  is  233333333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e67-1  is  23333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e68-1  is  2333333333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e69-1  is  233333333333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e70-1  is  23333333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e71-1  is  2333333333333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e72-1  is  233333333333333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e73-1  is  23333333333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e74-1  is  2333333333333333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e75-1  is  233333333333333333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e76-1  is  23333333333333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e77-1  is  2333333333333333333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e78-1  is  233333333333333333333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e79-1  is  23333333333333333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e80-1  is  2333333333333333333333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e81-1  is  233333333333333333333333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e82-1  is  23333333333333333333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e83-1  is  2333333333333333333333333333333333333333333333333333333333333333333333333333333333316666666666666666666666666666666666666666666666666666666666666666666666666666666668
integers from 1 ──► 1e84-1  is  233333333333333333333333333333333333333333333333333333333333333333333333333333333333166666666666666666666666666666666666666666666666666666666666666666666666666666666668

```



## Ring


```ring

see sum35(1000) + nl

func sum35 n
     n = n - 1
     return(3 * tri(floor(n / 3)) +
	    5 * tri(floor(n / 5)) -
	    15 * tri(floor(n / 15)))

func tri n
     return n * (n + 1) / 2

```



## Ruby

Simple Version (Slow):

```ruby
def sum35(n)
  (1...n).select{|i|i%3==0 or i%5==0}.inject(:+)
end
puts sum35(1000)      #=> 233168
```


Fast Version:

```ruby
# Given two integers n1,n2 return sum of multiples upto n3
#
#  Nigel_Galloway
#  August 24th., 2013.
def g(n1, n2, n3)
   g1 = n1*n2
   (1..g1).select{|x| x%n1==0 or x%n2==0}.collect{|x| g2=(n3-x)/g1; (x+g1*g2+x)*(g2+1)}.inject{|sum,x| sum+x}/2
end

puts g(3,5,999)

# For extra credit
puts g(3,5,100000000000000000000-1)
```


{{out}}

```txt

233168
2333333333333333333316666666666666666668

```


Other way:
{{trans|D}}

```ruby
def sumMul(n, f)
  n1 = (n - 1) / f
  f * n1 * (n1 + 1) / 2
end

def sum35(n)
  sumMul(n, 3) + sumMul(n, 5) - sumMul(n, 15)
end

for i in 1..20
  puts "%2d:%22d %s" % [i, 10**i, sum35(10**i)]
end
```


{{out}}

```txt

 1:                    10 23
 2:                   100 2318
 3:                  1000 233168
 4:                 10000 23331668
 5:                100000 2333316668
 6:               1000000 233333166668
 7:              10000000 23333331666668
 8:             100000000 2333333316666668
 9:            1000000000 233333333166666668
10:           10000000000 23333333331666666668
11:          100000000000 2333333333316666666668
12:         1000000000000 233333333333166666666668
13:        10000000000000 23333333333331666666666668
14:       100000000000000 2333333333333316666666666668
15:      1000000000000000 233333333333333166666666666668
16:     10000000000000000 23333333333333331666666666666668
17:    100000000000000000 2333333333333333316666666666666668
18:   1000000000000000000 233333333333333333166666666666666668
19:  10000000000000000000 23333333333333333331666666666666666668
20: 100000000000000000000 2333333333333333333316666666666666666668

```



## Run BASIC


```runbasic
print multSum35(1000)
end
function multSum35(n)
    for i = 1 to n - 1
        If (i mod 3 = 0) or (i mod 5 = 0) then  multSum35 = multSum35 + i
    next i
end function
```

```txt
233168
```



## Scala


```scala
def sum35( max:BigInt ) : BigInt = max match {

  // Simplest solution but limited to Ints only
  case j if j < 100000 => (1 until j.toInt).filter( i => i % 3 == 0 || i % 5 == 0 ).sum

  // Using a custom iterator that takes Longs
  case j if j < 10e9.toLong => {
    def stepBy( step:Long ) : Iterator[Long] = new Iterator[Long] { private var i = step; def hasNext = true; def next() : Long = { val result = i; i = i + step; result } }
    stepBy(3).takeWhile( _< j ).sum + stepBy(5).takeWhile( _< j ).sum - stepBy(15).takeWhile( _< j ).sum
  }

  // Using the formula for a Triangular number
  case j => {
    def triangle( i:BigInt ) = i * (i+1) / BigInt(2)
    3 * triangle( (j-1)/3 ) + 5 * triangle( (j-1)/5 ) - 15 * triangle( (j-1)/15 )
  }
}

{
for( i <- (0 to 20); n = "1"+"0"*i ) println( (" " * (21 - i)) + n + " => " + (" " * (21 - i)) + sum35(BigInt(n)) )
}
```

{{out}}

```txt
                     1 =>                      0
                    10 =>                     23
                   100 =>                    2318
                  1000 =>                   233168
                 10000 =>                  23331668
                100000 =>                 2333316668
               1000000 =>                233333166668
              10000000 =>               23333331666668
             100000000 =>              2333333316666668
            1000000000 =>             233333333166666668
           10000000000 =>            23333333331666666668
          100000000000 =>           2333333333316666666668
         1000000000000 =>          233333333333166666666668
        10000000000000 =>         23333333333331666666666668
       100000000000000 =>        2333333333333316666666666668
      1000000000000000 =>       233333333333333166666666666668
     10000000000000000 =>      23333333333333331666666666666668
    100000000000000000 =>     2333333333333333316666666666666668
   1000000000000000000 =>    233333333333333333166666666666666668
  10000000000000000000 =>   23333333333333333331666666666666666668
 100000000000000000000 =>  2333333333333333333316666666666666666668
```



## Rust


```rust

extern crate rug;

use rug::Integer;
use rug::ops::Pow;

fn main() {
    for i in [3, 20, 100, 1_000].iter() {
        let ten = Integer::from(10);
        let mut limit = Integer::from(Integer::from(&ten.pow(*i as u32)) - 1);
        let mut aux_3_1 = &limit.mod_u(3u32);
        let mut aux_3_2 = Integer::from(&limit - aux_3_1);
        let mut aux_3_3 = Integer::from(&aux_3_2/3);
        let mut aux_3_4 = Integer::from(3 + aux_3_2);
        let mut aux_3_5 = Integer::from(&aux_3_3*&aux_3_4);
        let mut aux_3_6 = Integer::from(&aux_3_5/2);

        let mut aux_5_1 = &limit.mod_u(5u32);
        let mut aux_5_2 = Integer::from(&limit - aux_5_1);
        let mut aux_5_3 = Integer::from(&aux_5_2/5);
        let mut aux_5_4 = Integer::from(5 + aux_5_2);
        let mut aux_5_5 = Integer::from(&aux_5_3*&aux_5_4);
        let mut aux_5_6 = Integer::from(&aux_5_5/2);

        let mut aux_15_1 = &limit.mod_u(15u32);
        let mut aux_15_2 = Integer::from(&limit - aux_15_1);
        let mut aux_15_3 = Integer::from(&aux_15_2/15);
        let mut aux_15_4 = Integer::from(15 + aux_15_2);
        let mut aux_15_5 = Integer::from(&aux_15_3*&aux_15_4);
        let mut aux_15_6 = Integer::from(&aux_15_5/2);

        let mut result_aux_1 = Integer::from(&aux_3_6 + &aux_5_6);
        let mut result = Integer::from(&result_aux_1 - &aux_15_6);

        println!("Sum for 10^{} : {}",i,result);
    }
}

```

Output :

```txt

Sum for 10^3 : 233168
Sum for 10^20 : 2333333333333333333316666666666666666668
Sum for 10^100 : 23333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666668
Sum for 10^1000 : 23333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333331666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666668

real	0m0.002s
user	0m0.002s
sys	0m0.000s


```




## Scheme


```scheme
(fold (lambda (x tot) (+ tot (if (or (zero? (remainder x 3)) (zero? (remainder x 5))) x 0))) 0 (iota 1000))
```


Output:

```txt

233168

```


Or, more clearly by decomposition:


```scheme
(define (fac35? x)
    (or (zero? (remainder x 3))
        (zero? (remainder x 5))))

(define (fac35filt x tot)
    (+ tot (if (fac35? x) x 0)))

(fold fac35filt 0 (iota 1000))
```


Output:

```txt

233168

```


For larger numbers iota can take quite a while just to build the list -- forget about waiting for all the computation to finish!


```scheme
(define (trisum n fac)
    (let* ((n1 (quotient (- n 1) fac))
           (n2 (+ n1 1)))
        (quotient (* fac n1 n2) 2)))

(define (fast35sum n)
    (- (+ (trisum n 5) (trisum n 3)) (trisum n 15)))

(fast35sum 1000)
(fast35sum 100000000000000000000)

```


Output:

```txt

233168
2333333333333333333316666666666666666668

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func bigInteger: sum35 (in bigInteger: n) is func
  result
    var bigInteger: sum35 is 0_;
  local
    const func bigInteger: sumMul (in bigInteger: n, in bigInteger: f) is func
      result
        var bigInteger: sumMul is 0_;
      local
        var bigInteger: n1 is 0_;
      begin
        n1 := pred(n) div f;
        sumMul := f * n1 * succ(n1) div 2_;
      end func;
   begin
     sum35 := sumMul(n, 3_) + sumMul(n, 5_) - sumMul(n, 15_);
   end func;

const proc: main is func
  begin
    writeln(sum35(1000_));
    writeln(sum35(10_ ** 20));
  end func;
```


{{out}}

```txt

233168
2333333333333333333316666666666666666668

```



## Sidef

{{trans|Ruby}}

```ruby
func sumMul(n, f) {
    var m = int((n - 1) / f)
    f * m * (m + 1) / 2
}

func sum35(n) {
    sumMul(n, 3) + sumMul(n, 5) - sumMul(n, 15)
}

for i in (1..20) {
    printf("%2s:%22s %s\n", i, 10**i, sum35(10**i))
}
```

{{out}}

```txt

 1:                    10 23
 2:                   100 2318
 3:                  1000 233168
 4:                 10000 23331668
 5:                100000 2333316668
 6:               1000000 233333166668
 7:              10000000 23333331666668
 8:             100000000 2333333316666668
 9:            1000000000 233333333166666668
10:           10000000000 23333333331666666668
11:          100000000000 2333333333316666666668
12:         1000000000000 233333333333166666666668
13:        10000000000000 23333333333331666666666668
14:       100000000000000 2333333333333316666666666668
15:      1000000000000000 233333333333333166666666666668
16:     10000000000000000 23333333333333331666666666666668
17:    100000000000000000 2333333333333333316666666666666668
18:   1000000000000000000 233333333333333333166666666666666668
19:  10000000000000000000 23333333333333333331666666666666666668
20: 100000000000000000000 2333333333333333333316666666666666666668

```



## Simula

(referenced from [[Greatest common divisor#Simula|Greatest common divisor]])

```algol68
! Find the sum of multiples of two factors below a limit -
! Project Euler problem 1: multiples of 3 or 5 below 1000 & 10**20;
BEGIN
    INTEGER PROCEDURE GCD(a, b); INTEGER a, b;
        GCD := IF b = 0 THEN a ELSE GCD(b, MOD(a, b));

    ! sum of multiples of n up to limit;
    INTEGER PROCEDURE multiples(n, limit); INTEGER n, limit;
    BEGIN
        INTEGER m;
        m := limit // n;
    ! moving //2 to sumMultiples() looked just too silly    ;
        multiples := n*((m*(m+1)) // 2) ! and risks overflow;
    END
    ! sum of multiples of n or m below limit;
    INTEGER PROCEDURE sumMultiples(n, m, limit);
        INTEGER n, m, limit;
    BEGIN
        INTEGER LCM;
        LCM:= (n // GCD(n, m)) * m;
        limit := limit-1;
        sumMultiples := multiples(n, limit) + multiples(m, limit)
                        - multiples(LCM, limit)
    END sumMultiples;

    ! Extra creditable: math is about avoiding calculation tedium;
    TEXT PROCEDURE repeat(c, n); CHARACTER c; INTEGER n; BEGIN
        TEXT r; r :- BLANKS(n);
        FOR n := n STEP -1 UNTIL 1 DO r.PUTCHAR(c);
        repeat :- r;
    END;
    TEXT PROCEDURE sumOfMultiplesOf3or5below10toThePowerOf(e);
        INTEGER e;
    sumOfMultiplesOf3or5below10toThePowerOf :-
        IF e < 1 THEN "0" ELSE IF e = 1 THEN "23"
        ELSE "23" & repeat('3', e-2)
            & "1" & repeat('6', e-2) & "8";

    INTEGER factor, n;
    FOR factor := 5 !, 2, 6;
                    DO BEGIN
        OUTTEXT("sum of positive multiples of 3 and");
        OUTINT(factor, 2); OUTCHAR(':');
        FOR n := ! 1 STEP 1 UNTIL 15, 100,;
                 1000 DO BEGIN
            OUTCHAR(' '); OUTINT(sumMultiples(3, factor, n), 0)
        END;
        OUTIMAGE
    END;
    FOR n := 0, 1, 3, 5, 10, 20, 40 DO BEGIN
        OUTTEXT(sumOfMultiplesOf3or5below10toThePowerOf(n));
        OUTIMAGE
    END
END
```

{{out}}
sum of positive multiples of 3 and 5: 233168<br />
0<br />
23<br />
233168<br />
2333316668<br />
23333333331666666668<br />
2333333333333333333316666666666666666668<br />
23333333333333333333333333333333333333331666666666666666666666666666666666666668


## Stata


###  With a dataset


```stata
clear all
set obs 999
gen a=_n
tabstat a if mod(a,3)==0 | mod(a,5)==0, statistic(sum)
```



###  With Mata


```stata
mata
a=1..999
sum(a:*(mod(a,3):==0 :| mod(a,5):==0))
```



## Swift


```swift



var n:Int=1000

func sum(x:Int)->Int{

	var s:Int=0
	for i in 0...x{
		if i%3==0 || i%5==0
		{
			s=s+i
		}

	}
	return s
}

var sumofmult:Int=sum(x:n)
print(sumofmult)


```



## Tcl


```tcl
# Fairly simple version; only counts by 3 and 5, skipping intermediates
proc mul35sum {n} {
    for {set total [set threes [set fives 0]]} {$threes<$n||$fives<$n} {} {
	if {$threes<$fives} {
	    incr total $threes
	    incr threes 3
	} elseif {$threes>$fives} {
	    incr total $fives
	    incr fives 5
	} else {
	    incr total $threes
	    incr threes 3
	    incr fives 5
	}
    }
    return $total
}
```

However, that's pretty dumb. We can do much better by observing that the sum of the multiples of <math>k</math> below some <math>n+1</math> is <math>k T_{n/k}</math>, where <math>T_i</math> is the <math>i</math>'th [[wp:Triangular number|triangular number]], for which there exists a trivial formula. Then we simply use an overall formula of <math>3T_{n/3} + 5T_{n/5} - 15T_{n/15}</math> (that is, summing the multiples of three and the multiples of five, and then subtracting the multiples of 15 which were double-counted).

```tcl
# Smart version; no iteration so very scalable!
proc tcl::mathfunc::triangle {n} {expr {
    $n * ($n+1) / 2
}}
# Note that the rounding on integer division is exactly what we need here.
proc sum35 {n} {
    incr n -1
    expr {3*triangle($n/3) + 5*triangle($n/5) - 15*triangle($n/15)}
}
```

Demonstrating:

```tcl
puts [mul35sum 1000],[sum35 1000]
puts [mul35sum 10000000],[sum35 10000000]
# Just the quick one; waiting for the other would get old quickly...
puts [sum35 100000000000000000000]
```

{{out}}

```txt
233168,233168
23333331666668,23333331666668
2333333333333333333316666666666666666668

```



## VBA

{{trans|VBScript}}

```vb
Private Function SumMult3and5VBScript(n As Double) As Double
Dim i As Double
    For i = 1 To n - 1
        If i Mod 3 = 0 Or i Mod 5 = 0 Then
            SumMult3and5VBScript = SumMult3and5VBScript + i
        End If
    Next
End Function
```

Other way :

```vb
Private Function SumMult3and5(n As Double) As Double
Dim i As Double
    For i = 3 To n - 1 Step 3
        SumMult3and5 = SumMult3and5 + i
    Next
    For i = 5 To n - 1 Step 5
        If i Mod 15 <> 0 Then SumMult3and5 = SumMult3and5 + i
    Next
End Function
```

Better way :

```vb
Private Function SumMult3and5BETTER(n As Double) As Double
Dim i As Double
    For i = 3 To n - 1 Step 3
        SumMult3and5BETTER = SumMult3and5BETTER + i
    Next
    For i = 5 To n - 1 Step 5
        SumMult3and5BETTER = SumMult3and5BETTER + i
    Next
    For i = 15 To n - 1 Step 15
        SumMult3and5BETTER = SumMult3and5BETTER - i
    Next
End Function
```


Call :

```vb
Option Explicit

Sub Main()
Dim T#
    T = Timer
    Debug.Print SumMult3and5VBScript(100000000) & "   " & Format(Timer - T, "0.000 sec.")
    T = Timer
    Debug.Print SumMult3and5(100000000) & "   " & Format(Timer - T, "0.000 sec.")
    T = Timer
    Debug.Print SumMult3and5BETTER(100000000) & "   " & Format(Timer - T, "0.000 sec.")
    Debug.Print "-------------------------"
    Debug.Print SumMult3and5BETTER(1000)
End Sub
```

{{Out}}

```txt
2,33333331666667E+15   9,059 sec.
2,33333331666667E+15   2,107 sec.
2,33333331666667E+15   1,799 sec.
-------------------------
233168

```



## VBScript

{{trans|Run BASIC}}

```vb

Function multsum35(n)
	For i = 1 To n - 1
		If i Mod 3 = 0 Or i Mod 5 = 0 Then
			multsum35 = multsum35 + i
		End If
	Next
End Function

WScript.StdOut.Write multsum35(CLng(WScript.Arguments(0)))
WScript.StdOut.WriteLine

```


{{Out}}

```txt

F:\>cscript /nologo multsum35.vbs 1000
233168

```



## Wortel


```wortel
@let {
  sum35 ^(@sum \!-@(\~%%3 || \~%%5) @til)

  !sum35 1000 ; returns 233168
}
```



## XPL0


```XPL0
include c:\cxpl\stdlib;

func Sum1;      \Return sum the straightforward way
int  N, S;
[S:= 0;
for N:= 1 to 999 do
    if rem(N/3)=0 or rem(N/5)=0 then S:= S+N;
return S;
];

func Sum2(D);   \Return sum of sequence using N*(N+1)/2
int  D;
int  Q;
[Q:= (1000-1)/D;
return Q*(Q+1)/2*D;
];

func Sum3(D);   \Return sum of sequence for really big number
string 0;       \don't terminate strings by setting most significant bit
int  D;         \divisor
int  I;
char P(40), Q(40), R(40);       \product, quotient, result
[StrNDiv("99999999999999999999", D, Q, 20);     \Q:= (1E20-1)/D
for I:= 0 to 17 do R(I):= ^0;                   \R:= D
R(18):= D/10 +^0;
R(19):= rem(0) +^0;
StrNMul(Q, R, P, 20);                           \P:= Q*R = Q*D
StrNAdd("00000000000000000001", Q, 20);         \Q:= Q+1
StrNMul(P+20, Q, R, 20);                        \R:= P*Q = Q*D*(Q+1)
StrNDiv(R, 2, Q, 40);                           \Q:= P/2 = Q*D*(Q+1)/2
return Q;                                       \(very temporary location)
];

char S(40), T;
[IntOut(0, Sum1);  CrLf(0);
 IntOut(0, Sum2(3) + Sum2(5) - Sum2(3*5));  CrLf(0);
StrNCopy(Sum3(3), S, 40);
StrNAdd(Sum3(5), S, 40);
T:= Sum3(3*5);
StrNSub(S, T, 40);
TextN(0, T, 40);  CrLf(0);
]
```


{{out}}

```txt

233168
233168
2333333333333333333316666666666666666668

```



## zkl

Brute force:

```zkl
[3..999,3].reduce('+,0) + [5..999,5].reduce('+,0) - [15..999,15].reduce('+,0)
233168
```

{{trans|Groovy}}
Using a formula, making sure the input will cast the result to the same type (ie if called with a BigNum, the result is a BigNum).

```zkl
fcn sumMul(N,m){N=(N-1)/m; N*(N+1)*m/2}
fcn sum35(N){sumMul(N,3) + sumMul(N,5) - sumMul(N,15)}
```

{{out}}

```txt

zkl: sum35(1000)  // int-->int
233168

zkl: var BN=Import("zklBigNum");
zkl: sum35(BN("1"+"0"*21))  // 1 with 21 zeros, BigNum-->BigNum
233333333333333333333166666666666666666668
sum35(BN("1"+"0"*15)) : "%,d".fmt(_)// 1e15, BigNum don't like float format input
233,333,333,333,333,166,666,666,666,668

```


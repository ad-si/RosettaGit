+++
title = "Semiprime"
description = ""
date = 2019-09-16T21:06:35Z
aliases = []
[extra]
id = 17215
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}} 

Semiprime numbers are natural numbers that are products of exactly two (possibly equal) [[prime_number|prime numbers]]. 


'''Semiprimes'''   are also known as:
:::*   '''semi-primes'''
:::*   '''biprimes'''
:::*   '''bi-primes'''
:::*   ''' ''2-almost'' '''   primes
:::*   or simply:   ''' ''P<sub>2</sub> '' '''



;Example: 
   <big> 1679  =  23 &times; 73 </big> 

(This particular number was chosen as the length of the [http://en.wikipedia.org/wiki/Arecibo_message Arecibo message]). 


;Task;
Write a function determining whether a given number is semiprime.


;See also:
* The Wikipedia article:   [http://mathworld.wolfram.com/Semiprime.html semiprime].
* The Wikipedia article:   [http://mathworld.wolfram.com/AlmostPrime.html almost prime].
* The OEIS article:   [http://oeis.org/A001358 semiprimes]   which has a shorter definition: ''the product of two primes''.





## 360 Assembly

{{trans|C}}

```360asm
*        Semiprime                 14/03/2017
SEMIPRIM CSECT
         USING  SEMIPRIM,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R10,PG             pgi=0
         LA     R8,0               m=0         
         L      R6,=F'2'           i=2
       DO WHILE=(C,R6,LE,=F'100')  do i=2 to 100
         ST     R6,N                 n=i
         LA     R9,0                 f=0
         LA     R7,2                 j=2
LOOPJ    EQU    *                    do j=2 while f<2 and j*j<=n
         C      R9,=F'2'               if f<2
         BNL    EXITJ                  then exit do j
         LR     R5,R7                  j
         MR     R4,R7                  *j
         C      R5,N                   if j*j<=n
         BH     EXITJ                  then exit do j
LOOPK    EQU    *                      do while n mod j=0 
         L      R4,N                     n
         SRDA   R4,32                    ~
         DR     R4,R7                    /j
         LTR    R4,R4                    if n mod <>0
         BNZ    EXITK                    then exit do j
         ST     R5,N                     n=n/j
         LA     R9,1(R9)                 f=f+1
         B      LOOPK                  enddo k
EXITK    LA     R7,1(R7)               j++
         B      LOOPJ                enddo j
EXITJ    L      R4,N                 n
       IF C,R4,GT,=F'1' THEN         if n>1 then
         LA     R2,1                   g=1
       ELSE     ,                    else
         LA     R2,0                   g=0
       ENDIF    ,                    endif
         AR     R2,R9                +f
       IF C,R2,EQ,=F'2' THEN         if f+(n>1)=2 then
         XDECO  R6,XDEC                edit i
         MVC    0(5,R10),XDEC+7        output i
         LA     R10,5(R10)             pgi=pgi+10
         LA     R8,1(R8)               m=m+1
         LR     R4,R8                  m
         SRDA   R4,32                  ~
         D      R4,=F'16'              m/16
       IF LTR,R4,Z,R4 THEN             if m mod 16=0 then
         XPRNT  PG,L'PG                  print buffer
         MVC    PG,=CL80' '              clear buffer
         LA     R10,PG                   pgi=0
       ENDIF    ,                      endif
       ENDIF    ,                    endif
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         XPRNT  PG,L'PG            print buffer
         MVC    PG,=CL80'..... semiprimes'  init buffer
         XDECO  R8,XDEC            edit m
         MVC    PG(5),XDEC+7       output m
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
N        DS     F                  n
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp
         YREGS
         END    SEMIPRIM
```

{{out}}

```txt

    4    6    9   10   14   15   21   22   25   26   33   34   35   38   39   46
   49   51   55   57   58   62   65   69   74   77   82   85   86   87   91   93
   94   95
   34 semiprimes

```




## Ada


This imports the package '''Prime_Numbers''' from [[Prime decomposition#Ada]]. 


```ada
with Prime_Numbers, Ada.Text_IO; 
 
procedure Test_Semiprime is
   
   package Integer_Numbers is new 
     Prime_Numbers (Natural, 0, 1, 2); 
   use Integer_Numbers;
   
begin
   for N in 1 .. 100 loop
      if Decompose(N)'Length = 2 then -- N is a semiprime;
	 Ada.Text_IO.Put(Integer'Image(Integer(N)));
      end if;
   end loop;
   Ada.Text_IO.New_Line;
   for N in 1675 .. 1680 loop
      if Decompose(N)'Length = 2 then -- N is a semiprime;
	 Ada.Text_IO.Put(Integer'Image(Integer(N)));
      end if;
   end loop; 
end Test_Semiprime;
```


It outputs all semiprimes below 100 and all semiprimes between 1675 and 1680:
{{output}}
 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
 1678 1679 

Note that  
 1675 = 5 * 5 * 67, 
 1676 = 2 * 2 * 419, 
 1677 = 3 * 13 * 43,
 1678 = 2 * 839,
 1679 = 23 * 73,
 1680 = 2 * 2 * 2 * 2 * 3 * 5 * 7,
so the result printed is actually correct.


## ALGOL 68


```algol68
# returns TRUE if n is semi-prime, FALSE otherwise            #
#         n is semi prime if it has exactly two prime factors #
PROC is semiprime = ( INT n )BOOL:
     BEGIN
         # We only need to consider factors between 2 and     #
         # sqrt( n ) inclusive. If there is only one of these #
         # then it must be a prime factor and so the number   #
         # is semi prime                                      #
         INT factor count := 0;
         FOR factor FROM 2 TO ENTIER sqrt( ABS n )
         WHILE IF n MOD factor = 0 THEN
                   factor count +:= 1;
                   # check the factor isn't a repeated factor #
                   IF n /= factor * factor THEN
                       # the factor isn't the square root     #
                       INT other factor = n OVER factor;
                       IF other factor MOD factor = 0 THEN
                           # have a repeated factor           #
                           factor count +:= 1
                       FI
                   FI
               FI;
               factor count < 2
         DO SKIP OD;
         factor count = 1
     END # is semiprime # ;

# determine the first few semi primes                          #
print( ( "semi primes below 100: " ) );
FOR i TO 99 DO
    IF is semi prime( i ) THEN print( ( whole( i, 0 ), " " ) ) FI
OD;
print( ( newline ) );
print( ( "semi primes below between 1670 and 1690: " ) );
FOR i FROM 1670 TO 1690 DO
    IF is semi prime( i ) THEN print( ( whole( i, 0 ), " " ) ) FI
OD;
print( ( newline ) )

```

{{out}}

```txt

semi primes below 100: 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
semi primes below between 1670 and 1690: 1671 1673 1678 1679 1681 1685 1687 1689

```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
SetBatchLines -1
k := 1
loop, 100
{
	m := semiprime(k)
	StringSplit, m_m, m, -
		if ( m_m1 = "yes" )
			list .= k . " "
	k++
}
MsgBox % list
list :=
;
### =============================================================================================================================

k := 1675
loop, 5
{
	m := semiprime(k)
	StringSplit, m_m, m, -
		if ( m_m1 = "yes" )
			list1 .= semiprime(k) . "`n"
		else
			list1 .= semiprime(k) . "`n"
	k++
}
MsgBox % list1
list1 :=
;
### =============================================================================================================================

; The function
### ====================================================================================================================

semiprime(k)
{
		start := floor(sqrt(k))
				loop, % floor(sqrt(k)) - 1
					{
							if ( mod(k, start) = 0 )
								new .= floor(start) . "*" . floor(k//start) . ","
						start--
					}
		
		StringSplit, index, new, `,
		
			if ( index0 = 2 )
				{
					StringTrimRight, new, new, 1
					StringSplit, 2_ind, new, *
						if (mod(2_ind2, 2_ind1) = 0) && ( 2_ind1 != 2_ind2 )
							new := "N0- " . k . "  -  " . new
						else
							new := "yes- " . k . "  -  " . new
				}
			else
				new := "N0- " . k . "  -  " . new
return new
}
;
### ===========================================================================================================================================

esc::Exitapp
```

{{output}}
<Pre>
4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
N0- 1675  -  25*67,5*335,
N0- 1676  -  4*419,2*838,
N0- 1677  -  39*43,13*129,3*559,
yes- 1678  -  2*839
yes- 1679  -  23*73</Pre>

## AWK


```AWK

# syntax: GAWK -f SEMIPRIME.AWK
BEGIN {
    main(0,100)
    main(1675,1680)
    exit(0)
}
function main(lo,hi,  i) {
    printf("%d-%d:",lo,hi)
    for (i=lo; i<=hi; i++) {
      if (is_semiprime(i)) {
        printf(" %d",i)
      }
    }
    printf("\n")
}
function is_semiprime(n,  i,nf) {
    nf = 0
    for (i=2; i<=n; i++) {
      while (n % i == 0) {
        if (nf == 2) {
          return(0)
        }
        nf++
        n /= i
      }
    }
    return(nf == 2)
}

```

{{out}}

```txt

0-100: 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
1675-1680: 1678 1679

```



## Bracmat

When Bracmat is asked to take the square (or any other) root of a number, it does so by first finding the number's prime factors. It can do that for numbers up to 2^32 or 2^64 (depending on compiler and processor). 

```bracmat
semiprime=
  m n a b
.   2^-64:?m
  & 2*!m:?n
  &   !arg^!m
    : (#%?a^!m*#%?b^!m|#%?a^!n&!a:?b)
  & (!a.!b);
```


Test with numbers < 2^63:

```bracmat
  2^63:?u
&   whl
  ' ( -1+!u:>2:?u
    & ( semiprime$!u:?R&out$(!u ":" !R)
      | 
      )
    );
```


Output:

```txt
9223372036854775797 : (3.3074457345618258599)
9223372036854775777 : (584911.15768846947407)
9223372036854775771 : (19.485440633518672409)
9223372036854775753 : (266416229.34620158357)
9223372036854775727 : (11113.829962389710679)
9223372036854775717 : (59.156328339607708063)
9223372036854775715 : (5.1844674407370955143)
9223372036854775703 : (9648151.955973018753)
9223372036854775694 : (2.4611686018427387847)
9223372036854775691 : (37.249280325320399343)
9223372036854775687 : (1303.7078566413549329)
9223372036854775685 : (5.1844674407370955137)
9223372036854775673 : (175934777.52424950849)
9223372036854775634 : (2.4611686018427387817)
9223372036854775633 : (421741.21869754273013)
9223372036854775627 : (6277.1469391753521551)
9223372036854775609 : (172153.53576597775553)
9223372036854775601 : (1045692671.8820346831)
9223372036854775589 : (563.16382543582335303)
9223372036854775577 : (267017141.34542246997)
9223372036854775574 : (2.4611686018427387787)
9223372036854775571 : (1951.4727510013764621)
9223372036854775537 : (47.196241958230952671)
9223372036854775531 : (1677122561.5499521771)
9223372036854775522 : (2.4611686018427387761)
9223372036854775511 : (29305709.314729530579)
9223372036854775502 : (2.4611686018427387751)
9223372036854775489 : (9413717.979780041917)
9223372036854775474 : (2.4611686018427387737)
9223372036854775466 : (2.4611686018427387733)
9223372036854775461 : (3.3074457345618258487)
9223372036854775451 : (545369243.16912160257)
9223372036854775439 : (11380717.810438572267)
9223372036854775418 : (2.4611686018427387709)
9223372036854775411 : (1420967.6490912200533)
9223372036854775409 : (15060911.612404657119)
9223372036854775407 : (3.3074457345618258469)
9223372036854775402 : (2.4611686018427387701)
9223372036854775389 : (3.3074457345618258463)
9223372036854775385 : (5.1844674407370955077)
9223372036854775383 : (3.3074457345618258461)
9223372036854775381 : (683.13504205031998207)
9223372036854775379 : (43.214497024112901753)
9223372036854775357 : (17.542551296285575021)
9223372036854775355 : (5.1844674407370955071)
^CTerminate batch job (Y/N)? Y
```



## C


```c>#include <stdio.h


int semiprime(int n)
{
	int p, f = 0;
	for (p = 2; f < 2 && p*p <= n; p++)
		while (0 == n % p)
			n /= p, f++;

	return f + (n > 1) == 2;
}

int main(void)
{
	int i;
	for (i = 2; i < 100; i++)
		if (semiprime(i)) printf(" %d", i);
	putchar('\n');

	return 0;
}
```

{{out}}

```txt
 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
```



## C++


```cpp

#include <iostream>

bool isSemiPrime( int c )
{
    int a = 2, b = 0;
    while( b < 3 && c != 1 )
    {
	if( !( c % a ) ) 
	{ c /= a; b++; }
	else a++;
    }
    return b == 2;
}
int main( int argc, char* argv[] )
{
    for( int x = 2; x < 100; x++ )
	if( isSemiPrime( x ) )
	    std::cout << x << " ";

    return 0;
}

```

{{out}}

```txt

4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95 

```



## C#


```c#

static void Main(string[] args)
{
    //test some numbers
    for (int i = 0; i < 50; i++)
    {
        Console.WriteLine("{0}\t{1} ", i,isSemiPrime(i));
    }
    Console.ReadLine();
}

//returns true or false depending if input was considered semiprime
private static bool isSemiPrime(int c)
{
    int a = 2, b = 0;
    while (b < 3 && c != 1)
    {
        if ((c % a) == 0)
        {
            c /= a;
            b++;
        }
        else
        {
            a++;
        };
    }
    return b == 2;
}

```

{{out}}

```txt

0       False
1       False
2       False
3       False
4       True
5       False
6       True
7       False
8       False
9       True
10      True
11      False
12      False
13      False
14      True
15      True
16      False
17      False
18      False
19      False
20      False
21      True
22      True
23      False
24      False
25      True
26      True
27      False
28      False
29      False
30      False
31      False
32      False
33      True
34      True
35      True
36      False
37      False
38      True
39      True
40      False
41      False
42      False
43      False
44      False
45      False
46      True
47      False
48      False
49      True

```



## Clojure

{{trans|C}}

```lisp

(ns example
  (:gen-class))

(defn semi-prime? [n]
  (loop [a 2
         b 0
         c n]
    (cond
      (> b 2) false
      (<= c 1) (= b 2)
      (= 0 (rem c a)) (recur a (inc b) (int (/ c a)))
      :else (recur (inc a) b c))))

(println (filter semi-prime? (range 1 100)))

```

{{Out}}

```txt

(4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95)

```



## Common Lisp


```lisp
(defun semiprimep (n &optional (a 2))
  (cond ((> a (isqrt n)) nil)
        ((zerop (rem n a)) (and (primep a) (primep (/ n a))))
        (t (semiprimep n (+ a 1)))))

(defun primep (n &optional (a 2))
  (cond ((> a (isqrt n)) t)
        ((zerop (rem n a)) nil)
        (t (primep n (+ a 1)))))
```


Example Usage:


```txt
CL-USER> (semiprimep 1234567)
T
CL-USER> (semiprimep 9876543)
NIL
```



## D

{{trans|Go}}

```d
bool semiprime(long n) pure nothrow @safe @nogc {
    auto nf = 0;
    foreach (immutable i; 2 .. n + 1) {
        while (n % i == 0) {
            if (nf == 2)
                return false;
            nf++;
            n /= i;
        }
    }
    return nf == 2;
}

void main() {
    import std.stdio;

    foreach (immutable n; 1675 .. 1681)
        writeln(n, " -> ", n.semiprime);
}
```

{{out}}

```txt
1675 -> false
1676 -> false
1677 -> false
1678 -> true
1679 -> true
1680 -> false
```



## DCL

Given a file primes.txt is the list of primes up to the sqrt(2^31-1), i.e. 46337;

```DCL
$ p1 = f$integer( p1 )
$ if p1 .lt. 2
$ then
$  write sys$output "out of range 2 thru 2^31-1"
$  exit
$ endif
$
$ close /nolog primes
$ on control_y then $ goto clean
$ open primes primes.txt
$
$ loop1:
$  read /end_of_file = prime primes prime
$  prime = f$integer( prime )
$  loop2:
$   t = p1 / prime
$   if t * prime .eq. p1
$   then
$    if f$type( factorization ) .eqs. ""
$    then
$     factorization = f$string( prime )
$    else
$     factorization = factorization + "*" + f$string( prime )
$    endif
$    if t .eq. 1 then $ goto done
$    p1 = t
$    goto loop2
$   else
$    goto loop1
$   endif
$ prime:
$ if f$type( factorization ) .eqs. ""
$ then
$  factorization = f$string( p1 )
$ else
$  factorization = factorization + "*" + f$string( p1 )
$ endif
$ done:
$ show symbol factorization
$ if f$locate( "*", factorization ) .eq. f$length( factorization )
$ then
$  write sys$output "so, it is prime"
$ else
$  if f$element( 2, "*", factorization ) .eqs. "*" then $ write sys$output "so, it is semiprime"
$ endif
$
$ clean:
$ close primes
```

{{out}}

```txt
$ @factor 6
  FACTORIZATION = "2*3"
so, it is semiprime
$ @factor 11
  FACTORIZATION = "11"
so, it is prime
$ @factor 2147483646
  FACTORIZATION = "2*3*3*7*11*31*151*331"
```



## EchoLisp


```scheme

(lib 'math)
(define (semi-prime? n) 
   (= (length (prime-factors n)) 2))

(for ((i 100)) 
    (when (semi-prime? i) (write i)))

4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95

(lib 'bigint)
(define N (* (random-prime 10000000) (random-prime 10000000)))
   → 6764578882969
(semi-prime? N)
    → #t

;; a pair n,n+1 of semi-primes
(prime-factors 100000000041)
    → (3 33333333347)
(prime-factors 100000000042)
    → (2 50000000021)

```



## Elixir


```elixir
defmodule Prime do
  def semiprime?(n), do: length(decomposition(n)) == 2
  
  def decomposition(n), do: decomposition(n, 2, [])
  
  defp decomposition(n, k, acc) when n < k*k, do: Enum.reverse(acc, [n])
  defp decomposition(n, k, acc) when rem(n, k) == 0, do: decomposition(div(n, k), k, [k | acc])
  defp decomposition(n, k, acc), do: decomposition(n, k+1, acc)
end

IO.inspect Enum.filter(1..100, &Prime.semiprime?(&1))
Enum.each(1675..1680, fn n ->
  :io.format "~w -> ~w\t~s~n", [n, Prime.semiprime?(n), Prime.decomposition(n)|>Enum.join(" x ")]
end)
```


{{out}}

```txt

[4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 55, 57,
 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95]
1675 -> false	5 x 5 x 67
1676 -> false	2 x 2 x 419
1677 -> false	3 x 13 x 43
1678 -> true	2 x 839
1679 -> true	23 x 73
1680 -> false	2 x 2 x 2 x 2 x 3 x 5 x 7

```



## Erlang

Another using prime factors from [[Prime_decomposition#Erlang]] :


```erlang

-module(factors).
-export([factors/1,kthfactor/2]).

factors(N) ->
     factors(N,2,[]).

factors(1,_,Acc) -> Acc;
factors(N,K,Acc) when N rem K == 0 ->
    factors(N div K,K, [K|Acc]);
factors(N,K,Acc) ->
    factors(N,K+1,Acc).


% is integer N factorable into M primes?
kthfactor(N,M) ->             
    case length(factors(N)) of M ->
      factors(N);
      _ ->
      false end.                      

```

{out}

```txt

17> factors:kthfactor(1679,2).
[73,23]
18> factors:kthfactor(1679,4).
false
23> FS = [{X,factors:kthfactor(X,2)} || X <- lists:seq(50,500), factors:kthfactor(X,2) =/= false]. 
[{51,[17,3]},
 {55,[11,5]},
 {57,[19,3]},
 {58,[29,2]},
 {62,[31,2]},
 {65,[13,5]},
 {69,[23,3]},
 {74,[37,2]},
 {77,[11,7]},
 {82,[41,2]},
 {85,[17,5]},
 {86,[43,2]},
 {87,[29,3]},
 {91,[13,7]},
 {93,[31,3]},
 {94,[47,2]},
 {95,[19,5]},
 {106,[53,2]},
 {111,[37,3]},
 {115,[23,5]},
 {118,[59,2]},
 {119,[17,7]},
 {121,"\v\v"},
 {122,[61,2]},
 {123,[41,3]},
 {129,[43|...]},
 {133,[...]},
 {134,...},
 {...}|...]


```

Note, there is some junk character data in the output since we 'usually' have to filter for char sequences (it's not a bug, it's a feature!).


## ERRE

<lang>
PROGRAM SEMIPRIME_NUMBER

!VAR I%

PROCEDURE SEMIPRIME(N%->RESULT%)
   LOCAL F%,P%
   P%=2
   LOOP
       EXIT IF NOT(F%<2 AND P%*P%<=N%)
       WHILE (N% MOD P%)=0 DO
            N%=N% DIV P%
            F%+=1
       END WHILE
       P%+=1
    END LOOP
    RESULT%=F%-(N%>1)=2
END PROCEDURE

BEGIN
    PRINT(CHR$(12);) !CLS
    FOR I%=2 TO 100 DO
         SEMIPRIME(I%->RESULT%)
         IF RESULT% THEN PRINT(I%;) END IF
    END FOR
    PRINT
END PROGRAM

```

Output is the same of "C" version.

=={{header|F_Sharp|F#}}==

```fsharp
let isSemiprime (n: int) =
    let rec loop currentN candidateFactor numberOfFactors =
        if numberOfFactors > 2 then numberOfFactors
        elif currentN = candidateFactor then numberOfFactors+1
        elif currentN % candidateFactor = 0 then loop (currentN/candidateFactor) candidateFactor (numberOfFactors+1)
        else loop currentN (candidateFactor+1) numberOfFactors
    if n < 2 then false else 2 = loop n 2 0

seq { 1 .. 100 } |> Seq.choose (fun n -> if isSemiprime n then Some(n) else None)
|> Seq.toList |> printfn "%A"

seq { 1675 .. 1680 }
|> Seq.choose (fun n -> if isSemiprime n then Some(n) else None)
|> Seq.toList
|> printfn "%A"
```

{{out}}

```txt
[4; 6; 9; 10; 14; 15; 21; 22; 25; 26; 33; 34; 35; 38; 39; 46; 49; 51; 55; 57; 58; 62; 65; 69; 74; 77; 82; 85; 86; 87; 91; 93; 94; 95]
[1678; 1679]

```


=={{Header|Factor}}==
{{works with|Factor|0.98}}
<lang>USING: kernel math.combinatorics math.primes.factors sequences ;

: semiprime? ( n -- ? )
    [ factors 2 <combinations> [ product ] map ]
    [ [ = ] curry ] bi any? ;
```


Displaying the semiprimes under 100:

<lang>100 <iota> [ semiprime? ] filter [ pprint bl ] each nl
```

{{out}}

```txt

4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95

```


=={{Header|Forth}}==

```forth
: semiprime?
  0 swap dup 2 do
    begin dup i mod 0= while i / swap 1+ swap repeat
    over 1 > over i dup * < or if leave then
  loop 1 > abs + 2 =
;

: test 100 2 do i semiprime? if i . then loop cr ;
```

{{out}}

```txt

test 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
 ok

```


=={{Header|Go}}==

```go
package main

import "fmt"

func semiprime(n int) bool {
    nf := 0
    for i := 2; i <= n; i++ {
        for n%i == 0 {
            if nf == 2 {
                return false
            }
            nf++
            n /= i
        }
    }
    return nf == 2
}

func main() {
    for v := 1675; v <= 1680; v++ {
        fmt.Println(v, "->", semiprime(v))
    }
}
```

{{out}}

```txt

1675 -> false
1676 -> false
1677 -> false
1678 -> true
1679 -> true
1680 -> false

```



## Haskell

{{libheader|Data.Numbers.Primes}}

```Haskell
isSemiprime :: Int -> Bool
isSemiprime n = (length factors) == 2 && (product factors) == n ||
                (length factors) == 1 && (head factors) ^ 2 == n
                    where factors = primeFactors n
```


Alternative (and faster) implementation using pattern matching:

```Haskell
isSemiprime :: Int -> Bool
isSemiprime n = case (primeFactors n) of
                   [f1, f2] -> f1 * f2 == n
                   otherwise -> False
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
link "factors"

procedure main(A)
    every nf := semiprime(n := !A) do write(n," = ",nf[1]," * ",nf[2])
end

procedure semiprime(n)  # Succeeds and produces the factors only if n is semiprime.
    return (2 = *(nf := factors(n)), nf)
end
```


{{Out}}

```txt

->semiprime 1676 1677 1678 1679 1680
1678 = 2 * 839
1679 = 23 * 73
->

```



## J


Implementation: 


```J
isSemiPrime=: 2 = #@q: ::0:"0
```


Example use: find all semiprimes less than 100:


```J
   I. isSemiPrime i.100
4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
```


Description: factor the number and count the primes in the factorization, is it 2?


## Java

{{works with|Java|1.5+}}
'''Inspired by:''' [[#Ada]]

Like the Ada example here, this borrows from [[Prime decomposition#Java|Prime decomposition]] and shows the semiprimes below 100 and from 1675 to 1680.

```java5
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class SemiPrime{
	private static final BigInteger TWO = BigInteger.valueOf(2);
	 
	public static List<BigInteger> primeDecomp(BigInteger a){
	    // impossible for values lower than 2
	    if(a.compareTo(TWO) < 0){
	        return null; 
	    }
	 
	    //quickly handle even values
	    List<BigInteger> result = new ArrayList<BigInteger>();
	    while(a.and(BigInteger.ONE).equals(BigInteger.ZERO)){
	        a = a.shiftRight(1);
	        result.add(TWO);
	    }
	 
	    //left with odd values
	    if(!a.equals(BigInteger.ONE)){
	        BigInteger b = BigInteger.valueOf(3);
	        while(b.compareTo(a) < 0){
	            if(b.isProbablePrime(10)){
	                BigInteger[] dr = a.divideAndRemainder(b);
	                if(dr[1].equals(BigInteger.ZERO)){
	                    result.add(b);
	                    a = dr[0];
	                }
	            }
	            b = b.add(TWO);
	        }
	        result.add(b); //b will always be prime here...
	    }
	    return result;
	}
	
	public static boolean isSemi(BigInteger x){
		List<BigInteger> decomp = primeDecomp(x);
		return decomp != null && decomp.size() == 2;
	}
	
	public static void main(String[] args){
		for(int i = 2; i <= 100; i++){
			if(isSemi(BigInteger.valueOf(i))){
				System.out.print(i + " ");
			}
		}
		System.out.println();
		for(int i = 1675; i <= 1680; i++){
			if(isSemi(BigInteger.valueOf(i))){
				System.out.print(i + " ");
			}
		}
	}
}
```

{{out}}

```txt
4 6 9 10 14 15 21 22 25 26 27 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 81 82 85 86 87 91 93 94 95 
1678 1679
```



## Julia

{{works with|Julia|0.6}}


```julia
using Primes
issemiprime(n::Integer) = sum(values(factor(n))) == 2
@show filter(issemiprime, 1:100)
```


{{out}}

```txt
filter(issemiprime, 1:100) = [4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95]
```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.2

fun isSemiPrime(n: Int): Boolean {
    var nf = 0
    var nn = n
    for (i in 2..nn)
        while (nn % i == 0) {
            if (nf == 2) return false
            nf++
            nn /= i
        }
    return nf == 2
}

fun main(args: Array<String>) {
    for (v in 1675..1680)
        println("$v ${if (isSemiPrime(v)) "is" else "isn't"} semi-prime")
}
```


{{out}}

```txt

1675 isn't semi-prime
1676 isn't semi-prime
1677 isn't semi-prime
1678 is semi-prime
1679 is semi-prime
1680 isn't semi-prime

```



## Lingo


```Lingo
on isSemiPrime (n)
    div = 2
    cnt = 0
    repeat while cnt < 3 and n <> 1
        if n mod div = 0 then
            n = n / div
            cnt = cnt + 1
        else
            div = div + 1
        end if
    end repeat
    return cnt=2
end
```



```Lingo
res = []
repeat with i = 1 to 100
    if isSemiPrime(i) then res.add(i)
end repeat
put res
```


{{out}}

```txt

-- [4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95]

```



## Lua


```Lua

function semiprime (n)
	local divisor, count = 2, 0
	while count < 3 and n ~= 1 do
		if n % divisor == 0 then
			n = n / divisor
			count = count + 1
		else
			divisor = divisor + 1
		end
	end
	return count == 2
end

for n = 1675, 1680 do
	print(n, semiprime(n))
end

```

{{out}}

```txt

1675    false
1676    false
1677    false
1678    true
1679    true
1680    false

```



## Maple


```Maple
SemiPrimes := proc( n )
    local fact;
    fact := NumberTheory:-Divisors( n ) minus {1, n};
    if numelems( fact ) in {1,2} and not( member( 'false', isprime ~ ( fact ) ) ) then
        return n;
    else
        return NULL;
    end if;
end proc:
{ seq( SemiPrimes( i ), i = 1..100 ) };
```

Output:

```Maple

{ 4,6,9,10,14,15,21,22,25,26,33,34,35,38,39,46,49,51,55,57,58,62,65,69,74,77,82,85,86,87,91,93,94,95 }

```



## Mathematica


```Mathematica
semiPrimeQ[n_Integer] := Module[{factors, numfactors},
  factors = FactorInteger[n] // Transpose;
  numfactors = factors[[2]] // Total  ;
  numfactors == 2
  ]

```

Example use: find all semiprimes less than 100:

```Mathematica
semiPrimeQ[#] & /@ Range[100];
Position[%, True] // Flatten
```

{{output}}

```txt
{4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 
55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95}
```



## MiniScript


```MiniScript
isSemiprime = function(num)
    divisor = 2
    primes = 0
    while primes < 3 and num != 1
        if num % divisor == 0 then
            num = num / divisor;
            primes = primes + 1
        else
            divisor = divisor + 1
        end if
    end while
    return primes == 2
end function

print "Semiprimes up to 100:"
results = []
for i in range(2, 100)
    if isSemiprime(i) then results.push i
end for
print results
```


{{output}}

```txt
Semiprimes up to 100:
[4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 
55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95]
```



## Nim


```Nim
proc isSemiPrime(k: int): string =
 var
  i: int = 2
  compte: int = 0
  x: int = k
 while i<=x and compte<3:
  if (x mod i)==0:
   x = x div i
   compte += 1
  else:
   i += 1
 if compte==2:
  result = "is semi-prime"
 else:
  result = "isn't semi-prime"
  
for k in 1675..1680:
 echo k," ",isSemiPrime(k)
```

{{output}}

```txt
1675 isn't semi-prime
1676 isn't semi-prime
1677 isn't semi-prime
1678 is semi-prime
1679 is semi-prime
1680 isn't semi-prime

```



## Objeck

{{trans|Go}}

```objeck

class SemiPrime {
  function : Main(args : String[]) ~ Nil {
    for(i := 0; i < 100; i+=1;) {
      if(SemiPrime(i)) {
        "{$i} "->Print();
      };
    };
    IO.Console->PrintLine();
  }
  
  function : native : SemiPrime(n : Int) ~ Bool {
    nf := 0;
    for(i := 2; i <= n; i+=1;) {
      while(n%i = 0) {
        if(nf = 2) {
          return false;
        };
        nf+=1;
        n /= i;
      };
    };
    
    return nf = 2;
  }
}
```


Output:

```txt
4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
```



## Oforth



```Oforth
func: semiprime(n)
| i |
   0 2 n sqrt asInteger for: i [ while(n i /mod swap 0 &=) [ ->n 1+ ] drop ]
   n 1 > ifTrue: [ 1+ ] 2 == ; 
```


{{out}}

```txt

100 seq filter(#semiprime) println
[4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95]

```



## PARI/GP


```parigp
issemi(n)=bigomega(n)==2
```


A faster version might use trial division and primality testing:

```parigp
issemi(n)={
  forprime(p=2,97,if(n%p==0, return(isprime(n/p))));
  if(isprime(n), return(0));
  bigomega(n)==2
};
```


To get faster, partial factorization can be used. At this time GP does not have access to meaningful partial factorization (though it can get it to some extent through flags on <code>factorint</code>), so this version is in PARI:

```c
long
issemiprime(GEN n)
{
  if (typ(n) != t_INT)
    pari_err_TYPE("issemiprime", n);
  if (signe(n) <= 0)
    return 0;

  ulong nn = itou_or_0(n);
  if (nn)
    return uissemiprime(nn);

  pari_sp ltop = avma;
  if (!mpodd(n)) {
    long ret = mod4(n) && isprime(shifti(n, -1));
    avma = ltop;
    return ret;
  }


  long p;
  forprime_t primepointer;
  u_forprime_init(&primepointer, 3, 997);
  while ((p = u_forprime_next(&primepointer))) {
    if (dvdis(n, p)) {
      long ret = isprime(diviuexact(n, p));
      avma = ltop;
      return ret;
    }
  }

  if (isprime(n))
    return 0;

  if (DEBUGLEVEL > 3)
    pari_printf("issemi: Number is a composite with no small prime factors; using general factoring mechanisms.");

  GEN fac = Z_factor_until(n, shifti(n, -1));	/* Find a nontrivial factor -- returns just the factored part */
  GEN expo = gel(fac, 2);
  GEN pr = gel(fac, 1);
  long len = glength(expo);
  if (len > 2) {
    avma = ltop;
    return 0;
  }
  if (len == 2) {
    if (cmpis(gel(expo, 1), 1) > 0 || cmpis(gel(expo, 2), 1) > 0) {
      avma = ltop;
      return 0;
    }
    GEN P = gel(pr, 1);
    GEN Q = gel(pr, 2);
    long ret = isprime(P) && isprime(Q) && equalii(mulii(P, Q), n);
    avma = ltop;
    return ret;
  }
  if (len == 1) {
    long e = itos(gel(expo, 1));
    if (e == 2) {
      GEN P = gel(pr, 1);
      long ret = isprime(P) && equalii(sqri(P), n);
      avma = ltop;
      return ret;
    } else if (e > 2) {
      avma = ltop;
      return 0;
    }
    GEN P = gel(pr, 1);
    long ret = isprime(P) && isprime(diviiexact(n, P));
    avma = ltop;
    return ret;
  }

  pari_err_BUG(pari_sprintf("Z_factor_until returned an unexpected value %Ps at n = %Ps, exiting...", fac, n));
  avma = ltop;
  return 0; /* never used */
}
```



## Pascal

{{libheader|primTrial}}{{works with|Free Pascal}} 


```pascal
program SemiPrime;
{$IFDEF FPC}
  {$Mode objfpc}// compiler switch to use result
{$ELSE}
  {$APPTYPE CONSOLE} // for Delphi
{$ENDIF}
uses
  primTrial;

function isSemiprime(n: longWord;doWrite:boolean): boolean;
var
  fac1 : LongWord;
begin
  //a simple isAlmostPrime(n,2) would do without output;
  fac1 := SmallFactor(n);
  IF fac1 < n then
  Begin
    n := n div fac1;
    result := SmallFactor(n) = n;
    if result AND doWrite then
      write(fac1:10,'*',n:11)
  end
  else
    result := false;
end;
var
  i,k : longWord;
BEGIN
  For i := 2 to 97 do
    IF isSemiPrime(i,false) then
      write(i:3);
  writeln;
  //test for big numbers
  k := 4000*1000*1000;
  i := k-100;
  repeat
    IF isSemiPrime(i,true) then
      writeln(' = ',i:10);
    inc(i);
  until i> k;
END.
```

;output:

```txt

 4  6  9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 
 74 77 82 85 86 87 91 93 94 95

        71*   56338027 = 3999999917
     42307*      94547 = 3999999929
        59*   67796609 = 3999999931
         5*  799999987 = 3999999935
         2* 1999999973 = 3999999946
        11*  363636359 = 3999999949
       103*   38834951 = 3999999953
     12007*     333139 = 3999999973
         7*  571428569 = 3999999983
         5*  799999999 = 3999999995

```



## Perl

{{libheader|ntheory}}
With late versions of the ntheory module, we can use <tt>is_semiprime</tt> to get answers for 64-bit numbers in single microseconds.

```perl
use ntheory "is_semiprime";
for ([1..100], [1675..1681], [2,4,99,100,1679,5030,32768,1234567,9876543,900660121]) {
  print join(" ",grep { is_semiprime($_) } @$_),"\n";
}
```

{{out}}

```txt
4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95
1678 1679 1681
4 1679 1234567 900660121
```


One can also use <tt>factor</tt> in scalar context, which gives the number of factors (like <tt>bigomega</tt> in Pari/GP and <tt>PrimeOmega</tt> in Mathematica).  This skips some optimizations but at these small sizes it doesn't matter.

```perl
use ntheory "factor";
print join(" ", grep { scalar factor($_) == 2 } 1..100),"\n";
```


While <tt>is_semiprime</tt> is the fastest way, we can do some of its pre-tests by hand, such as:

```perl
use ntheory qw/factor is_prime trial_factor/;
sub issemi {
  my $n = shift;
  if ((my @p = trial_factor($n,500)) > 1) {
    return 0 if @p > 2;
    return !!is_prime($p[1]) if @p == 2;
  }
  2 == factor($n);
}
```



## Perl 6

Here is a naive, grossly inefficient implementation.

```perl6
sub is-semiprime (Int $n --> Bool) {
    not $n.is-prime and
        .is-prime given 
        $n div first $n %% *, flat grep &is-prime, 2 .. *;
}

use Test;
my @primes = flat grep &is-prime, 2 .. 100;
for ^5 {
    nok is-semiprime([*] my @f1 = @primes.roll(1)), ~@f1;
    ok  is-semiprime([*] my @f2 = @primes.roll(2)), ~@f2;
    nok is-semiprime([*] my @f3 = @primes.roll(3)), ~@f3;
    nok is-semiprime([*] my @f4 = @primes.roll(4)), ~@f4;
}
```

{{out}}

```txt
ok 1 - 17
ok 2 - 47 23
ok 3 - 23 37 41
ok 4 - 53 37 67 47
ok 5 - 5
ok 6 - 73 43
ok 7 - 13 53 71
ok 8 - 7 79 37 71
ok 9 - 41
ok 10 - 71 37
ok 11 - 37 53 43
ok 12 - 3 2 47 67
ok 13 - 17
ok 14 - 41 61
ok 15 - 71 31 79
ok 16 - 97 17 73 17
ok 17 - 61
ok 18 - 73 47
ok 19 - 13 19 5
ok 20 - 37 97 11 31
```



### More efficient example

Here is a more verbose, but MUCH more efficient implementation. Demonstrating using it to find an infinite list of semiprimes and to check a range of integers to find the semiprimes.
{{works with|Rakudo|2017.02}}


```perl6
sub is-semiprime ( Int $n where * > 0 ) {
    return False if $n.is-prime;
    my $factor = find-factor( $n );
    return True if $factor.is-prime && ( $n div $factor ).is-prime;
    False;
}

sub find-factor ( Int $n, $constant = 1 ) {
    my $x      = 2;
    my $rho    = 1;
    my $factor = 1;
    while $factor == 1 {
        $rho *= 2;
        my $fixed = $x;
        for ^$rho {
            $x = ( $x * $x + $constant ) % $n;
            $factor = ( $x - $fixed ) gcd $n;
            last if 1 < $factor;
        }
    }
    $factor = find-factor( $n, $constant + 1 ) if $n == $factor;
    $factor;
}

INIT my $start = now;

# Infinite list of semiprimes
constant @semiprimes = lazy gather for 4 .. * { .take if .&is-semiprime };

# Show the semiprimes < 100
say 'Semiprimes less than 100:';
say @semiprimes[^ @semiprimes.first: * > 100, :k ], "\n";

# Check individual integers, or in this case, a range
my $s = 2⁹⁷ - 1;
say "Is $_ semiprime?: ", .&is-semiprime for $s .. $s + 30;

say 'elapsed seconds: ', now - $start;

```

{{out}}

```txt
Semiprimes less than 100:
(4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95)

Is 158456325028528675187087900671 semiprime?: True
Is 158456325028528675187087900672 semiprime?: False
Is 158456325028528675187087900673 semiprime?: False
Is 158456325028528675187087900674 semiprime?: False
Is 158456325028528675187087900675 semiprime?: False
Is 158456325028528675187087900676 semiprime?: False
Is 158456325028528675187087900677 semiprime?: False
Is 158456325028528675187087900678 semiprime?: False
Is 158456325028528675187087900679 semiprime?: False
Is 158456325028528675187087900680 semiprime?: False
Is 158456325028528675187087900681 semiprime?: False
Is 158456325028528675187087900682 semiprime?: False
Is 158456325028528675187087900683 semiprime?: False
Is 158456325028528675187087900684 semiprime?: False
Is 158456325028528675187087900685 semiprime?: False
Is 158456325028528675187087900686 semiprime?: False
Is 158456325028528675187087900687 semiprime?: False
Is 158456325028528675187087900688 semiprime?: False
Is 158456325028528675187087900689 semiprime?: False
Is 158456325028528675187087900690 semiprime?: False
Is 158456325028528675187087900691 semiprime?: False
Is 158456325028528675187087900692 semiprime?: False
Is 158456325028528675187087900693 semiprime?: False
Is 158456325028528675187087900694 semiprime?: False
Is 158456325028528675187087900695 semiprime?: False
Is 158456325028528675187087900696 semiprime?: False
Is 158456325028528675187087900697 semiprime?: False
Is 158456325028528675187087900698 semiprime?: False
Is 158456325028528675187087900699 semiprime?: False
Is 158456325028528675187087900700 semiprime?: False
Is 158456325028528675187087900701 semiprime?: True
elapsed seconds: 0.0574433
```



## Phix


```Phix
function semiprime(integer n)
    sequence f = prime_factors(n)
    integer l = length(f)
    return (l=2 and n=f[1]*f[2]) or (l=1 and n=power(f[1],2))
end function

procedure test(integer start, integer stop)
sequence s = {}
    for i=start to stop do
        if semiprime(i) then
            s &= i
        end if
    end for
    ?s
    ?length(s)
end procedure
test(1,100)
test(1675,1680)
```


```txt

{4,6,9,10,14,15,21,22,25,26,33,34,35,38,39,46,49,51,55,57,58,62,65,69,74,77,82,85,86,87,91,93,94,95}
34
{1678,1679}
2

```



## PicoLisp


```PicoLisp
(de factor (N)
   (make
      (let
         (D 2
            L (1 2 2 . (4 2 4 2 4 6 2 6 .))
            M (sqrt N) )
         (while (>= M D)
            (if (=0 (% N D))
               (setq M 
                  (sqrt (setq N (/ N (link D)))) )
               (inc 'D (pop 'L)) ) )
         (link N) ) ) )

(println         
   (filter
      '((X) 
         (let L (factor X)
            (and (cdr L) (not (cddr L))) ) )
      (conc (range 1 100) (range 1675 1680)) ) )
      
(bye)
```

{{out}}

```txt
(4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95 1678 1679)
```



## PL/I


```pli
*process source attributes xref nest or(!);
 /*--------------------------------------------------------------------
 * 22.02.2014 Walter Pachl using the is_prime code from
 *                         PL/I 'prime decomposition'
 * 23.02.  WP start test for second prime with 2 or first prime found
 *-------------------------------------------------------------------*/
 spb: Proc options(main);
 Dcl a(10) Bin Fixed(31)
          Init(900660121,2,4,1679,1234567,32768,99,9876543,100,5040);
 Dcl (x,n,nf,i,j) Bin Fixed(31) Init(0);
 Dcl f(3) Bin Fixed(31);
 Dcl txt Char(30) Var;
 Dcl bit Bit(1);
 Do i=1 To hbound(a);
   bit=is_semiprime(a(i));
   Select(nf);
     When(0,1) txt=' is prime';
     When(2)   txt=' is     semiprime '!!factors(a(i));
     Otherwise txt=' is NOT semiprime '!!factors(a(i));
     End;
   Put Edit(a(i),bit,txt)(Skip,f(10),x(1),b(1),a);
   End;

 is_semiprime: Proc(x) Returns(bit(1));
 /*--------------------------------------------------------------------
 * Returns '1'b if x is semiprime, '0'b otherwise
 * in addition
 * it sets f(1) and f(2) to the first (or only) prime factor(s)
 *-------------------------------------------------------------------*/
   Dcl x Bin Fixed(31);
   nf=0;
   f=0;
   x=a(i);
   n=x;
   f(1)=2;
 loop:
   Do While(nf<=2 & n>1);
     If is_prime(n) Then Do;
       Call mem(n);
       Leave loop;
       End;
     Else Do;
 loop2:
       Do j=f(1) By 1 While(j*j<=n);
         If is_prime(j)&mod(n,j)=0 Then Do;
           Call mem(j);
           n=n/j;
           Leave loop2;
           End;
         End;
       End;
     End;
   Return(nf=2);
 End;

 is_prime: Proc(n) Returns(bit(1));
 Dcl n Bin Fixed(31);
 Dcl i Bin Fixed(31);
   If n < 2 Then Return('0'b);
   If n = 2 Then Return('1'b);
   If mod(n,2)=0 Then Return('0'b);
   Do i = 3 by 2 While(i*i<=n);
     If mod(n,i)=0 Then Return('0'b);
     End;
   Return('1'b);
 End is_prime;

 mem: Proc(x);
 Dcl x Bin Fixed(31);
   nf+=1;
   f(nf)=x;
 End;

 factors: Proc(x) Returns(Char(150) Var);
 Dcl x Bin Fixed(31);
 Dcl (res,net) Char(150) Var Init('');
 Dcl (i,f3) Bin Fixed(31);
 res=f(1)!!'*'!!f(2);
 f3=x/(f(1)*f(2));
 If f3>1 Then
   res=res!!'*'!!f3;
 Do i=1 To length(res);
   If substr(res,i,1)>' ' Then
     net=net!!substr(res,i,1);
   End;
 Return(net);
 End;

 End spb;

```

'''Output:'''

```txt
 900660121 1 is     semiprime 30011*30011
         2 0 is prime
         4 1 is     semiprime 2*2
      1679 1 is     semiprime 23*73
   1234567 1 is     semiprime 127*9721
     32768 0 is NOT semiprime 2*2*8192
        99 0 is NOT semiprime 3*3*11
   9876543 0 is NOT semiprime 3*227*14503
       100 0 is NOT semiprime 2*2*25
      5040 0 is NOT semiprime 2*2*1260
```



## PowerShell


```PowerShell

function isPrime ($n) {
    if ($n -le 1) {$false} 
    elseif (($n -eq 2) -or ($n -eq 3)) {$true}
    else{
        $m = [Math]::Floor([Math]::Sqrt($n))
        (@(2..$m | where {($_ -lt $n)  -and ($n % $_ -eq 0) }).Count -eq 0)
    }
}
function semiprime ($n) {
    if($n -gt 3) {
        $lim = [Math]::Floor($n/2)+1
        $i = 2
        while(($i -lt $lim) -and ($n%$i -ne 0)){ $i += 1}
        if($i -eq $lim){@()}
        elseif(-not (isPrime ($n/$i))){@()}
        else{@($i,($n/$i))}
    } else {@()}
}
$OFS = " x "
"1679: $(semiprime 1679)"
"87: $(semiprime   87)"
"25: $(semiprime 25)"
"12: $(semiprime   12)"
"6: $(semiprime   6)"
$OFS = " "
"semiprime form 1 to 100: $(1..100 | where {semiprime $_})"

```

<b>Output:</b>

```txt

1679: 23 x 73
87: 3 x 29
25: 5 x 5
12: 
6: 2 x 3
semiprime form 1 to 100: 4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95

```



## Python

This imports [[Prime decomposition#Python]]

```python
from prime_decomposition import decompose

def semiprime(n):
    d = decompose(n)
    try:
        return next(d) * next(d) == n
    except StopIteration:
        return False
```


{{out}}
From Idle:

```python>>>
 semiprime(1679)
True
>>> [n for n in range(1,101) if semiprime(n)]
[4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95]
>>> 
```



## Racket

The first implementation considers all pairs of factors multiplying up to the given number and determines if any of them is a pair of primes.

```Racket
#lang racket
(require math)

(define (pair-factorize n)
  "Return all two-number factorizations of a number"
  (let ([up-limit (integer-sqrt n)])
    (map (λ (x) (list x (/ n x)))
	 (filter (λ (x) (<= x up-limit)) (divisors n)))))

(define (semiprime n)
  "Determine if a number is semiprime i.e. a product of two primes.
Check if any pair of complete factors consists of primes."
  (for/or ((pair (pair-factorize n)))
    (for/and ((el pair))
      (prime? el))))
```


The alternative implementation operates directly on the list of prime factors and their multiplicities. It is approximately 1.6 times faster than the first one (according to some simple tests of mine).

```Racket
#lang racket
(require math)

(define (semiprime n)
  "Alternative implementation.
Check if there are two prime factors whose product is the argument
or if there is a single prime factor whose square is the argument"
  (let ([prime-factors (factorize n)])
    (or (and (= (length prime-factors) 1)
	     (= (expt (caar prime-factors) (cadar prime-factors)) n))
	(and (= (length prime-factors) 2)
	     (= (foldl (λ (x y) (* (car x) y)) 1 prime-factors) n)))))
```



## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 20.02.2014 Walter Pachl  relying on 'prime decomposition'
* 21.02.2014 WP Clarification: I copied the algorithm created by
*            Gerard Schildberger under the task referred to above
* 21.02.2014 WP Make sure that factr is not called illegally
*--------------------------------------------------------------------*/
Call test 4
Call test 9
Call test 10
Call test 12
Call test 1679
Exit

test:
Parse Arg z
If is_semiprime(z) Then Say z 'is semiprime' fl
                   Else Say z 'is NOT semiprime' fl
Return

is_semiprime:
  Parse Arg z
  If z<1 | datatype(z,'W')=0 Then Do
    Say 'Argument ('z') must be a natural number (1, 2, 3, ...)'
    fl=''
    End
  Else
    fl=factr(z)
  Return words(fl)=2    

/*----------------------------------FACTR subroutine-----------------*/
factr: procedure; parse arg x 1 z,list /*sets X&Z to arg1, LIST=''.  */
if x==1  then return ''             /*handle the special case of X=1.*/
j=2;     call .factr                /*factor for the only even prime.*/
j=3;     call .factr                /*factor for the 1st  odd  prime.*/
j=5;     call .factr                /*factor for the 2nd  odd  prime.*/
j=7;     call .factr                /*factor for the 3rd  odd  prime.*/
j=11;    call .factr                /*factor for the 4th  odd  prime.*/
j=13;    call .factr                /*factor for the 5th  odd  prime.*/
j=17;    call .factr                /*factor for the 6th  odd  prime.*/
                                    /* [?]   could be optimized more.*/
                                    /* [?]   J in loop starts at 17+2*/
     do y=0  by 2;     j=j+2+y//4   /*insure J isn't divisible by 3. */
     if right(j,1)==5  then iterate /*fast check for divisible by 5. */
     if j*j>z          then leave   /*are we higher than the v of Z ?*/
     if j>Z            then leave   /*are we higher than value of Z ?*/
     call .factr                    /*invoke .FACTR for some factors.*/
     end   /*y*/                    /* [?]  only tests up to the v X.*/
                                    /* [?]  LIST has a leading blank.*/
if z==1  then return list           /*if residual=unity, don't append*/
              return list z         /*return list,  append residual. */
/*-------------------------------.FACTR internal subroutine----------*/
.factr:  do  while z//j==0          /*keep dividing until we can't.  */
         list=list j                /*add number to the list  (J).   */
         z=z%j                      /*% (percent)  is integer divide.*/
         end   /*while z··· */      /*  //   ?---remainder integer ÷.*/
return                              /*finished, now return to invoker*/
```

'''Output'''

```txt
4 is semiprime  2 2
9 is semiprime  3 3
10 is semiprime  2 5
12 is NOT semiprime  2 2 3
1679 is semiprime  23 73
```



### version 2

The method used is to examine integers, skipping primes. 

If it's composite (the 1<sup>st</sup> factor is prime), then check if the 2<sup>nd</sup> factor is prime.   If so, the number is a   ''semiprime''. 

The   '''isPrime'''   function could be optimized by utilizing an integer square root function instead of testing if   '''j*j>x'''   for every divisor.

```rexx
/*REXX program determines if any integer  (or a range of integers)  is/are  semiprime.  */
parse arg bot top .                              /*obtain optional arguments from the CL*/
if bot=='' | bot==","  then bot=random()         /*None given?   User wants us to guess.*/
if top=='' | top==","  then top=bot              /*maybe define a range of numbers.     */
tell=  top=>0 |  top==bot                        /*should results be shown to the term? */
w=max(length(bot), length(top)) + 5              /*obtain the maximum width of numbers. */
numeric digits max(9, w)                         /*ensure there're enough decimal digits*/
#=0                                              /*initialize number of semiprimes found*/
             do n=bot  to abs(top)               /*show results for a range of numbers. */
             ?=isSemiPrime(n);      #=#+?        /*Is N a semiprime?; Maybe bump counter*/
             if tell  then say right(n,w)  right(word("isn't" 'is', ?+1), 6)  'semiprime.'
             end   /*n*/
say
if bot\==top  then say 'found '   #   " semiprimes."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure;  parse arg x;               if x<2  then return 0  /*number too low?*/
         if wordpos(x, '2 3 5 7 11 13 17 19 23')\==0    then return 1  /*it's low prime.*/
         if x//2==0  then return 0;     if x//3==0      then return 0  /*÷ by 2; ÷ by 3?*/
           do j=5  by 6  until j*j>x;   if x//j==0      then return 0  /*not a prime.   */
                                        if x//(j+2)==0  then return 0  /* "  "   "      */
           end   /*j*/
         return 1                                /*indicate that  X  is a prime number. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isSemiPrime: procedure;  parse arg x;          if x<4  then return 0

                           do i=2  for 2;  if x//i==0  then if isPrime(x%i)  then return 1
                                                                             else return 0
                           end   /*i*/
                                                                             /*    ___  */
               do   j=5  by 6;         if j*j>x    then  return 0            /* > √ x  ?*/
                 do k=j  by 2  for 2;  if x//k==0  then  if isPrime(x%k)  then return 1
                                                                          else return 0
                 end   /*k*/                     /* [↑]  see if 2nd factor is prime or ¬*/
               end     /*j*/                     /* [↑]  J is never a multiple of three.*/
```

{{out|output|text=  when using the input of:   <tt> -1   106 </tt>}}

(Shown at   <big> '''<sup>5</sup>/<sub>6</sub>''' </big>   size.)
<pre style="font-size:84%;height:100ex">
      -1  isn't semiprime.
       0  isn't semiprime.
       1  isn't semiprime.
       2  isn't semiprime.
       3  isn't semiprime.
       4     is semiprime.
       5  isn't semiprime.
       6     is semiprime.
       7  isn't semiprime.
       8  isn't semiprime.
       9     is semiprime.
      10     is semiprime.
      11  isn't semiprime.
      12  isn't semiprime.
      13  isn't semiprime.
      14     is semiprime.
      15     is semiprime.
      16  isn't semiprime.
      17  isn't semiprime.
      18  isn't semiprime.
      19  isn't semiprime.
      20  isn't semiprime.
      21     is semiprime.
      22     is semiprime.
      23  isn't semiprime.
      24  isn't semiprime.
      25     is semiprime.
      26     is semiprime.
      27  isn't semiprime.
      28  isn't semiprime.
      29  isn't semiprime.
      30  isn't semiprime.
      31  isn't semiprime.
      32  isn't semiprime.
      33     is semiprime.
      34     is semiprime.
      35     is semiprime.
      36  isn't semiprime.
      37  isn't semiprime.
      38     is semiprime.
      39     is semiprime.
      40  isn't semiprime.
      41  isn't semiprime.
      42  isn't semiprime.
      43  isn't semiprime.
      44  isn't semiprime.
      45  isn't semiprime.
      46     is semiprime.
      47  isn't semiprime.
      48  isn't semiprime.
      49     is semiprime.
      50  isn't semiprime.
      51     is semiprime.
      52  isn't semiprime.
      53  isn't semiprime.
      54  isn't semiprime.
      55     is semiprime.
      56  isn't semiprime.
      57     is semiprime.
      58     is semiprime.
      59  isn't semiprime.
      60  isn't semiprime.
      61  isn't semiprime.
      62     is semiprime.
      63  isn't semiprime.
      64  isn't semiprime.
      65     is semiprime.
      66  isn't semiprime.
      67  isn't semiprime.
      68  isn't semiprime.
      69     is semiprime.
      70  isn't semiprime.
      71  isn't semiprime.
      72  isn't semiprime.
      73  isn't semiprime.
      74     is semiprime.
      75  isn't semiprime.
      76  isn't semiprime.
      77     is semiprime.
      78  isn't semiprime.
      79  isn't semiprime.
      80  isn't semiprime.
      81  isn't semiprime.
      82     is semiprime.
      83  isn't semiprime.
      84  isn't semiprime.
      85     is semiprime.
      86     is semiprime.
      87     is semiprime.
      88  isn't semiprime.
      89  isn't semiprime.
      90  isn't semiprime.
      91     is semiprime.
      92  isn't semiprime.
      93     is semiprime.
      94     is semiprime.
      95     is semiprime.
      96  isn't semiprime.
      97  isn't semiprime.
      98  isn't semiprime.
      99  isn't semiprime.
     100  isn't semiprime.
     101  isn't semiprime.
     102  isn't semiprime.
     103  isn't semiprime.
     104  isn't semiprime.
     105  isn't semiprime.
     106     is semiprime.

found  35  semiprimes.

```

{{out|output|text=  when using the input of:   <tt> 99888111555   99888111600 </tt>}}

(Shown at   <big> '''<sup>5</sup>/<sub>6</sub>''' </big>   size.)
<pre style="font-size:84%;height:100ex">
     99888111555  isn't semiprime.
     99888111556  isn't semiprime.
     99888111557  isn't semiprime.
     99888111558  isn't semiprime.
     99888111559  isn't semiprime.
     99888111560  isn't semiprime.
     99888111561  isn't semiprime.
     99888111562  isn't semiprime.
     99888111563     is semiprime.
     99888111564  isn't semiprime.
     99888111565  isn't semiprime.
     99888111566     is semiprime.
     99888111567  isn't semiprime.
     99888111568  isn't semiprime.
     99888111569     is semiprime.
     99888111570  isn't semiprime.
     99888111571  isn't semiprime.
     99888111572  isn't semiprime.
     99888111573  isn't semiprime.
     99888111574     is semiprime.
     99888111575  isn't semiprime.
     99888111576  isn't semiprime.
     99888111577  isn't semiprime.
     99888111578     is semiprime.
     99888111579  isn't semiprime.
     99888111580  isn't semiprime.
     99888111581  isn't semiprime.
     99888111582  isn't semiprime.
     99888111583  isn't semiprime.
     99888111584  isn't semiprime.
     99888111585  isn't semiprime.
     99888111586  isn't semiprime.
     99888111587  isn't semiprime.
     99888111588  isn't semiprime.
     99888111589  isn't semiprime.
     99888111590  isn't semiprime.
     99888111591     is semiprime.
     99888111592  isn't semiprime.
     99888111593     is semiprime.
     99888111594  isn't semiprime.
     99888111595  isn't semiprime.
     99888111596  isn't semiprime.
     99888111597  isn't semiprime.
     99888111598  isn't semiprime.
     99888111599  isn't semiprime.
     99888111600  isn't semiprime.

found  7  semiprimes.

```


===version 3, with memoization===
This REXX version is overt 20% faster than version 2   (when in the   ''millions''   range). 

If the 2<sup>nd</sup> argument   ('''top''')   is negative   (it's absolute value is used),   individual numbers in the range aren't shown, but the   ''count''   of semiprimes found is shown.

It gets its speed increase by the use of memoization of the prime numbers found, an unrolled primality (division) check, and other speed improvements. 

```rexx
/*REXX program determines if any integer  (or a range of integers)  is/are  semiprime.  */
parse arg bot top .                              /*obtain optional arguments from the CL*/
if bot=='' | bot==","  then bot=random()         /*None given?   User wants us to guess.*/
if top=='' | top==","  then top=bot              /*maybe define a range of numbers.     */
tell= bot=>0  &  top=>0                          /*should results be shown to the term? */
w=max(length(bot), length(top))                  /*obtain the maximum width of numbers. */
!.=;  !.2=1; !.3=1; !.5=1; !.7=1; !.11=1; !.13=1; !.17=1; !.19=1; !.23=1; !.29=1;  !.31=1
numeric digits max(9, w)                         /*ensure there're enough decimal digits*/
#=0                                              /*initialize number of semiprimes found*/
             do n=abs(bot)  to abs(top)          /*show results for a range of numbers. */
             ?=isSemiPrime(n);      #=#+?        /*Is N a semiprime?; Maybe bump counter*/
             if tell  then say right(n,w)  right(word("isn't" 'is', ?+1), 6)  'semiprime.'
             end   /*n*/
say
if bot\==top  then say 'found '   #   " semiprimes."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure expose !.;  parse arg x;     if x<2  then return 0  /*number too low?*/
         if !.x==1                                      then return 1  /*a known prime. */
         if x// 2==0  then return 0;    if x//3==0      then return 0  /*÷ by  2;÷by  3?*/
         parse var x '' -1 _;           if _==5         then return 0  /*last digit a 5?*/
         if x// 7==0  then return 0;    if x//11==0     then return 0  /*÷ by  7;÷by 11?*/
         if x//13==0  then return 0;    if x//17==0     then return 0  /*÷ by 13;÷by 17?*/
         if x//19==0  then return 0;    if x//23==0     then return 0  /*÷ by 19;÷by 23?*/
           do j=29  by 6  until j*j>x;  if x//j==0      then return 0  /*not a prime.   */
                                        if x//(j+2)==0  then return 0  /* "  "   "      */
           end   /*j*/
         !.x=1;                return 1          /*indicate that  X  is a prime number. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isSemiPrime: procedure expose !.;  parse arg x;          if x<4  then return 0

                           do i=2  for 2;  if x//i==0  then if isPrime(x%i)  then return 1
                                                                             else return 0
                           end   /*i*/
                                                                             /*    ___  */
               do   j=5  by 6  until j*j>x                                   /* > √ x  ?*/
                 do k=j  by 2  for 2;  if x//k==0  then  if isPrime(x%k)  then return 1
                                                                          else return 0
                 end   /*k*/                     /* [↑]  see if 2nd factor is prime or ¬*/
               end     /*j*/                     /* [↑]  J is never a multiple of three.*/
         return 0
```

{{out|output|text=  is identical to the previous REXX version.}} 




## Ring


```ring

prime = 1679
decomp(prime)

func decomp nr
x = ""
sum = 0
for i = 1 to nr
    if isPrime(i) and nr % i = 0
       sum = sum + 1
       x = x + string(i) + " * " ok
    if i = nr and sum = 2
       x2 = substr(x,1,(len(x)-2))
       see string(nr) + " = " + x2 + "is semiprime" + nl 
    but i = nr and sum != 2 see string(nr) + " is not semiprime" + nl ok
next

func isPrime n
     if n < 2 return false ok
     if n < 4 return true ok
     if n % 2 = 0 and n != 2 return false ok
     for d = 3 to sqrt(n) step 2 
         if n % d = 0 return false ok
     next	
     return true

```



## Ruby


```ruby
require 'prime'
# 75.prime_division # Returns the factorization.75 divides by 3 once and by 5 twice => [[3, 1], [5, 2]]

class Integer
  def semi_prime?
    prime_division.sum(&:last) == 2
  end
end

p 1679.semi_prime? # true
p ( 1..100 ).select( &:semi_prime? )
# [4, 6, 9, 10, 14, 15, 21, 22, 25, 26, 33, 34, 35, 38, 39, 46, 49, 51, 55, 57, 58, 62, 65, 69, 74, 77, 82, 85, 86, 87, 91, 93, 94, 95]

```



## Rust

<lang>extern crate primal;

fn isqrt(n: usize) -> usize {
    (n as f64).sqrt() as usize
}

fn is_semiprime(mut n: usize) -> bool {
    let root = isqrt(n) + 1;
    let primes1 = primal::Sieve::new(root);
    let mut count = 0;

    for i in primes1.primes_from(2).take_while(|&x| x < root) {
        while n % i == 0 {
            n /= i;
            count += 1;
        }
        if n == 1 {
            break;
        }
    }

    if n != 1 {
        count += 1;
    }
    count == 2
}

#[test]
fn test1() {
    assert_eq!((2..10).filter(|&n| is_semiprime(n)).count(), 3);
}

#[test]
fn test2() {
    assert_eq!((2..100).filter(|&n| is_semiprime(n)).count(), 34);
}

#[test]
fn test3() {
    assert_eq!((2..1_000).filter(|&n| is_semiprime(n)).count(), 299);
}

#[test]
fn test4() {
    assert_eq!((2..10_000).filter(|&n| is_semiprime(n)).count(), 2_625);
}

#[test]
fn test5() {
    assert_eq!((2..100_000).filter(|&n| is_semiprime(n)).count(), 23_378);
}

#[test]
fn test6() {
    assert_eq!((2..1_000_000).filter(|&n| is_semiprime(n)).count(), 210_035);
}
```


{{out}}

```txt

running 6 tests
test test1 ... ok
test test2 ... ok
test test3 ... ok
test test4 ... ok
test test5 ... ok
test test6 ... ok

test result: ok. 6 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out

```



## Scala

{{works with|Scala 2.9.1}}

```Scala
object Semiprime extends App {

  def isSP(n: Int): Boolean = {
    var nf: Int = 0
    var l = n
    for (i <- 2 to l/2) {
      while (l % i == 0) {
        if (nf == 2) return false
        nf +=1
        l /= i 
      }
    }
    nf == 2
  }

  (2 to 100) filter {isSP(_) == true} foreach {i => print("%d ".format(i))}
  println
  1675 to 1681 foreach {i => println(i+" -> "+isSP(i))}
  
}
```

{{out}}

```txt
4 6 9 10 14 15 21 22 25 26 33 34 35 38 39 46 49 51 55 57 58 62 65 69 74 77 82 85 86 87 91 93 94 95 
1675 -> false
1676 -> false
1677 -> false
1678 -> true
1679 -> true
1680 -> false
1681 -> true
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: semiPrime (in var integer: n) is func
  result
    var boolean: isSemiPrime is TRUE;
  local
    var integer: p is 2;
    var integer: f is 0;
  begin
    while f < 2 and p**2 <= n do
      while n rem p = 0 do
        n := n div p;
        incr(f);
      end while;
      incr(p);
    end while;
    isSemiPrime := f + ord(n > 1) = 2;
  end func;

const proc: main is func
  local
    var integer: v is 0;
  begin
    for v range 1675 to 1680 do
      writeln(v <& " -> " <& semiPrime(v));
    end for;
  end func;
```


{{out}}

```txt

1675 -> FALSE                                                                                                                                                   
1676 -> FALSE                                                                                                                                                   
1677 -> FALSE                                                                                                                                                   
1678 -> TRUE                                                                                                                                                    
1679 -> TRUE                                                                                                                                                    
1680 -> FALSE

```



## Sidef

Built-in:

```ruby
say is_semiprime(2**128 + 1)   #=> true
say is_semiprime(2**256 - 1)   #=> false
```


User-defined function, with trial division up to a given bound '''B''':

```ruby
func is_semiprime(n, B=1e4) {

    with (n.trial_factor(B)) { |f|
        return false if (f.len > 2)
        return f.all { .is_prime } if (f.len == 2)
    }

    n.factor.len == 2
}

say [2,4,99,100,1679,32768,1234567,9876543,900660121].grep(is_semiprime)
```

{{out}}

```txt

[4, 1679, 1234567, 900660121]

```



## Swift



```swift
import Foundation

func primes(n: Int) -> AnyGenerator<Int> {
  
  var (seive, i) = ([Int](0..<n), 1)
  let lim = Int(sqrt(Double(n)))
  
  return anyGenerator {
    while ++i < n {
      if seive[i] != 0 {
        if i <= lim {
          for notPrime in stride(from: i*i, to: n, by: i) {
            seive[notPrime] = 0
          }
        }
        return i
      }
    }
    return nil
  }
}

func isSemiPrime(n: Int) -> Bool {
  let g = primes(n)
  while let first = g.next() {
    if n % first == 0 {
      if first * first == n {
        return true
      } else {
        while let second = g.next() {
          if first * second == n { return true }
        }
      }
    }
  }
  return false
}
```



## Tcl

{{tcllib|math::numtheory}}

```tcl
package require math::numtheory

proc isSemiprime n {
    if {!($n & 1)} {
	return [::math::numtheory::isprime [expr {$n >> 1}]]
    }
    for {set i 3} {$i*$i < $n} {incr i 2} {
	if {$n / $i * $i != $n && [::math::numtheory::isprime $i]} {
	    if {[::math::numtheory::isprime [expr {$n/$i}]]} {
		return 1
	    }
	}
    }
    return 0
}

for {set n 1675} {$n <= 1680} {incr n} {
    puts -nonewline "$n is ... "
    if {[isSemiprime $n]} {
	puts "a semiprime"
    } else {
	puts "NOT a semiprime"
    }
}
```

{{out}}

```txt

1675 is ... a semiprime
1676 is ... NOT a semiprime
1677 is ... a semiprime
1678 is ... a semiprime
1679 is ... a semiprime
1680 is ... NOT a semiprime

```



## zkl

{{trans|C}}

```zkl
fcn semiprime(n){
   reg f = 0;
   p:=2; while(f < 2 and p*p <= n){
      while(0 == n % p){ n /= p; f+=1; }
      p+=1;
   }
   return(f + (n > 1) == 2);
}
```

{{out}}

```txt

[1675 .. 1681].filter(semiprime).println();
L(1678,1679,1681)

```


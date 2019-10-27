+++
title = "Kaprekar numbers"
description = ""
date = 2019-09-19T11:38:53Z
aliases = []
[extra]
id = 9891
[taxonomies]
categories = []
tags = []
+++

{{task}}

A positive integer is a [[wp:Kaprekar number|Kaprekar number]] if:
* It is 1
* The decimal representation of its square may be split once into two parts consisting of positive integers which sum to the original number. 

Note that a split resulting in a part consisting purely of 0s is not valid, 
as 0 is not considered positive.

;Example Kaprekar numbers:
* <math>2223</math> is a Kaprekar number, as <math>2223 * 2223 = 4941729</math>, <math>4941729</math> may be split to <math>494</math> and <math>1729</math>, and <math>494 + 1729 = 2223</math>.
* The series of Kaprekar numbers is known as [[oeis:A006886|A006886]], and begins as <math>1, 9, 45, 55, ...</math>.

;Example process:
10000 (100<sup>2</sup>) splitting from left to right:
* The first split is [1, 0000], and is invalid; the 0000 element consists entirely of 0s, and 0 is not considered positive.
* Slight optimization opportunity: When splitting from left to right, once the right part consists entirely of 0s, no further testing is needed; all further splits would also be invalid.

;Task description:
Generate and show all Kaprekar numbers less than 10,000. 

;Extra credit:
Optionally, count (and report the count of) how many Kaprekar numbers are less than 1,000,000.

;Extra extra credit:
The concept of Kaprekar numbers is not limited to base 10 (i.e. decimal numbers); 
if you can, show that Kaprekar numbers exist in other bases too.  

For this purpose, do the following:
* Find all Kaprekar numbers for base 17 between 1 and 1,000,000 (one million);
* Display each of them in base 10 representation;
* Optionally, using base 17 representation (use letters 'a' to 'g' for digits 10(10) to 16(10)), display each of the numbers, its square, and where to split the square. 
 

For example, 225(10) is "d4" in base 17, its square "a52g", and a5(17) + 2g(17) = d4(17), so the display would be something like:
```txt
225   d4  a52g  a5 + 2g
```


;Reference:
* [http://www.cs.uwaterloo.ca/journals/JIS/VOL3/iann2a.html The Kaprekar Numbers] by Douglas E. Iannucci (2000). [http://pictor.math.uqam.ca/~plouffe/OEIS/jis/The%20Kaprekar%20Numbers.pdf PDF version]
;related task
[[Casting out nines]]




## 360 Assembly

{{trans|PL/I}}

```360asm
*        Kaprekar numbers          22/03/ 2017
KAPREKAR CSECT
         USING  KAPREKAR,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R10,0              n=0
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=F'1000000')  do i=1 to 1000000
         CVD    R6,PI                pi=i
         ZAP    PS,PI                ps=pi
         MP     PS,PI                ps=pi*pi
         ZAP    PX,PS                ps
         OI     PX+7,X'0F'           zap sign
         UNPK   SW,PX                packed PL8 to zoned CL16
         MVC    SS(16),SW            s=pic(ps,16)
         MVI    OK,X'00'             ok=false
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,=F'15')     do j=1 to 15
         LA     R2,16                  16
         SR     R2,R7                  -j
         ST     R2,LL                  l=16-j
         LA     R2,S1                  @s1
         LA     R3,20                  20
         LA     R4,SS                  @s
         LR     R5,R7                  j
         ICM    R5,B'1000',=C' '       pad
         MVCL   R2,R4                  s1=substr(s,1,j)        
         LA     R2,S2                  @s2
         LA     R3,20                  20
         LA     R4,SS                  @s
         AR     R4,R7                  +j
         L      R5,LL                  l
         ICM    R5,B'1000',=C' '       pad
         MVCL   R2,R4                  s2=substr(s,j+1,l)
         MVC    ZZ,=20C'0'             zw=(20)'0'
         LA     R2,S1                  @s1
         LR     R3,R7                  j
         LA     R4,ZZ                  @zz
         LR     R5,R7                  j
         CLCL   R2,R4                  if substr(s1,1,j)=substr(zz,1,j)
         BE     ITERJ                  then iterate j
         LA     R2,S2                  @s2
         L      R3,LL                  l
         LA     R4,ZZ                  @zz
         L      R5,LL                  l
         CLCL   R2,R4                  if substr(s2,1,l)=substr(zz,1,l)
         BE     EXITJ                  then leave j
         XDECI  R2,S1                  unedit s1
         ST     R2,M1                  m1=s1
         XDECI  R2,S2                  unedit s2
         ST     R2,M2                  m2=s2
         L      R2,M1                  m1
         A      R2,M2                  +m2
         ST     R2,MM                  m=m1+m2
       IF C,R6,EQ,MM THEN              if i=m then
         MVI    OK,X'01'                 ok=true
         B      EXITJ                    leave j
       ENDIF    ,                      end if
ITERJ    LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
EXITJ    EQU    *                    exitj:
       IF CLI,OK,EQ,X'01',OR,C,R6,EQ,=F'1' THEN  if ok or i=1 then
         LA     R10,1(R10)               n=n+1
         XDECO  R10,PG                   edit n
         XDECO  R6,PG+12                 edit i
         XPRNT  PG,L'PG                  print buffer
       ENDIF    ,                      end if
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
OK       DS     X                  ok logical
LL       DS     F                  l  binary
MM       DS     F                  m  "
M1       DS     F                  m1 "
M2       DS     F                  m2 "
         DS     0D                 -- alignment for cvd
PI       DS     PL8                pi fixed decimal(15)
PM       DS     PL8                pm "
PS       DS     PL8                ps "
PX       DS     PL8                px "
SS       DC     CL20' '            s  character(20)
S1       DS     CL20               s1 "
S2       DS     CL20               s2 "
ZZ       DS     CL20               z  "
SW       DS     CL16               sw character(16)
PG       DC     CL80' '            buffer
         YREGS
         END    KAPREKAR
```

{{out}}

```txt

           1           1
           2           9
           3          45
           4          55
           5          99
           6         297
           7         703
           8         999
           9        2223
          10        2728
          11        4879
          12        4950
          13        5050
          14        5292
          15        7272
          16        7777
          17        9999
...
          53      994708
          54      999999

```




## Ada

with extra bases from 2 up to 36 (0..9a..z)

task description wasn't clear if 1000000 for base 17 was base 17 or base 10, 
so i chose base 17 (17 ** 6).


```ada
with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Kaprekar2 is
   use Ada.Strings.Fixed;

   To_Digit : constant String := "0123456789abcdefghijklmnopqrstuvwxyz";

   type Int is mod 2 ** 64;
   subtype Base_Number is Int range 2 .. 36;

   From_Digit : constant array (Character) of Int :=
     ('0'    => 0,
      '1'    => 1,
      '2'    => 2,
      '3'    => 3,
      '4'    => 4,
      '5'    => 5,
      '6'    => 6,
      '7'    => 7,
      '8'    => 8,
      '9'    => 9,
      'a'    => 10,
      'b'    => 11,
      'c'    => 12,
      'd'    => 13,
      'e'    => 14,
      'f'    => 15,
      'g'    => 16,
      'h'    => 17,
      'i'    => 18,
      'j'    => 19,
      'k'    => 20,
      'l'    => 21,
      'm'    => 22,
      'n'    => 23,
      'o'    => 24,
      'p'    => 25,
      'q'    => 26,
      'r'    => 27,
      's'    => 28,
      't'    => 29,
      'u'    => 30,
      'v'    => 31,
      'w'    => 32,
      'x'    => 33,
      'y'    => 34,
      'z'    => 35,
      others => 0);

   function To_String (Item : Int; Base : Base_Number := 10) return String is
      Value       : Int := Item;
      Digit_Index : Natural;
      Result      : String (1 .. 64);
      First       : Natural := Result'Last;
   begin
      while Value > 0 loop
         Digit_Index := Natural (Value mod Base);
         Result (First) := To_Digit (Digit_Index + 1);
         Value := Value / Base;
         First := First - 1;
      end loop;
      return Result (First + 1 .. Result'Last);
   end To_String;

   procedure Get (From : String; Item : out Int; Base : Base_Number := 10) is
   begin
      Item := 0;
      for I in From'Range loop
         Item := Item * Base;
         Item := Item + From_Digit (From (I));
      end loop;
   end Get;

   function Is_Kaprekar (N : Int; Base : Base_Number := 10) return Boolean is
      Square : Int;
   begin
      if N = 1 then
         return True;
      else
         Square := N ** 2;
         declare
            Image : String := To_String (Square, Base);
            A, B  : Int;
         begin
            for I in Image'First .. Image'Last - 1 loop
               exit when Count (Image (I + 1 .. Image'Last), "0")
                 = Image'Last - I;
               Get (From => Image (Image'First .. I),
                    Item => A,
                    Base => Base);
               Get (From => Image (I + 1 .. Image'Last),
                    Item => B,
                    Base => Base);
               if A + B = N then
                  return True;
               end if;
            end loop;
         end;
      end if;
      return False;
   end Is_Kaprekar;

   Count : Natural := 0;
begin
   for I in Int range 1 .. 10_000 loop
      if Is_Kaprekar (I) then
         Count := Count + 1;
         Ada.Text_IO.Put (To_String (I) & ",");
      end if;
   end loop;
   Ada.Text_IO.Put_Line (" Total:" & Integer'Image (Count));

   for I in Int range 10_001 .. 1_000_000 loop
      if Is_Kaprekar (I) then
         Count := Count + 1;
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("Kaprekar Numbers below 1000000:" &
                         Integer'Image (Count));

   Count := 0;
   Ada.Text_IO.Put_Line ("Kaprekar Numbers below 1000000 in base 17:");
   for I in Int range 1 .. 17 ** 6 loop
      if Is_Kaprekar (I, 17) then
         Count := Count + 1;
         Ada.Text_IO.Put (To_String (I, 17) & ",");
      end if;
   end loop;
   Ada.Text_IO.Put_Line (" Total:" & Integer'Image (Count));
end Kaprekar2;
```


{{out}}

```txt
1,9,45,55,99,297,703,999,2223,2728,4879,4950,5050,5292,7272,7777,9999, Total: 17
Kaprekar Numbers below 1000000: 54
Kaprekar Numbers below 1000000 in base 17:
1,g,3d,d4,gg,556,bbb,ggg,18bd,1f1f,36db,43cd,61eb,785d,7a96,967b,98b4,af26,cd44,da36,f1f2,f854,gggg,33334,ddddd,fgacc,ggggg,146fca,236985,2b32b3,2gde03,3a2d6f,3fa16d,443ccd,4e9c28,54067b,5aggb6,687534,6f6f6g,7e692a,7f391e,91d7f3,92a7e7,a1a1a1,a89bdd,b6005b,bcga96,c274e9,ccd444,d16fa4,d6e3a2,e032ge,e5de5e,eda78c,fca147,g10645,gggggg, Total: 57
```



## ALGOL 68


```algol68
# find some Kaprekar numbers                              #

# returns TRUE if n is a Kaprekar number, FALSE otherwise #
PROC is kaprekar = ( INT n )BOOL:
     IF n < 1 THEN
         # 0 and -ve numbers are not Kaprekar numbers     #
         FALSE
     ELIF n = 1 THEN
         # 1 is defined to be a Kaprekar number           #
         TRUE
     ELSE
         # n is a Kaprekar number if the digits of its    #
         # square can be partitioned into two numbers     #
         # that sum to n                                  #
         LONG INT  n squared     = LENG n * n;
         LONG INT  power of ten := 10;
         BOOL result            := FALSE;
         WHILE n squared > power of ten AND NOT result DO
             LONG INT left  = n squared OVER power of ten;
             LONG INT right = n squared  MOD power of ten;
             result := ( ( left + right ) = n AND right /= 0 );
             power of ten *:= 10
         OD;
         result
     FI # is kaprekar # ;
         
         
# count the number of Kaprekar numbers up to 1 000 000    #
# printing all those below 10 000                         #
INT max number           = 1 000 000;
INT k count             := 0;
[ 1 : 2 ]LONG INT split := ( 0, 0 );
print( ( "Kaprekar numbers below 10 000: ", newline ) );
FOR n TO max number DO
    IF is kaprekar( n ) THEN
        k count +:= 1;
        IF n < 10 000 THEN
            print( ( " ", whole( n, -4 ) ) )
        FI
    FI
OD;
print( ( newline ) );
print( ( "There are ", whole( k count, 0 ), " Kaprekar numbers below ", whole( max number, 0 ), newline ) )

```

{{out}}

```txt

Kaprekar numbers below 10 000:
    1    9   45   55   99  297  703  999 2223 2728 4879 4950 5050 5292 7272 7777 9999
There are 54 Kaprekar numbers below 1000000

```



## AutoHotkey

Function:

```AutoHotkey
Kaprekar(L) {
    Loop, % L + ( C := 0 ) {
        S := ( N := A_Index ) ** 2
        Loop % StrLen(N) {
            B := ( B := SubStr(S,1+A_Index) ) ? B : 0
            If !B & ( (A := SubStr(S,1,A_Index)) <> 1 )
                Break
            If ( N == A+B ) {
                R .= ", " N , C++
                Break
            }
        }
    }
    Return C " Kaprekar numbers in [1-" L "]:`n" SubStr(R,3)
}
```

Usage:

```AutoHotkey
MsgBox, % Kaprekar(10000)
```

{{out}}

```txt
17 Kaprekar numbers in [1-10000]:
1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999
```



## AWK


```AWK

# syntax: GAWK -f KAPREKAR_NUMBERS.AWK
BEGIN {
    limit = 1000000
    printf("%d\n",1)
    n = 1
    for (i=2; i<limit; i++) {
      squared = sprintf("%.0f",i*i)
      for (j=1; j<=length(squared); j++) {
        L = substr(squared,1,j) + 0
        R = substr(squared,j+1) + 0
        if (R == 0) {
          continue
        }
        if (L + R == i) {
          n++
          if (i <= 10000) {
            printf("%d\n",i)
          }
          break
        }
      }
    }
    printf("%d Kaprekar numbers < %s\n",n,limit)
    exit(0)
}

```

{{out}}

```txt

1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999
54 Kaprekar numbers < 1000000

```


## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

for /l %%i in (1,1,9999) do (
  title Processing - %%i
  call:kaprekar %%i
)

pause>nul
exit /b

:kaprekar
set num=%1
if %num% leq 0 exit /b
set /a num2=%num%*%num%

if %num2% leq 9 (
  if %num2%==%num% (
    echo %num%
    exit /b
  ) else (
    exit /b
  )
)

call:strlength %num2%
set len=%errorlevel%
set /a offset=%len%-1
set tempcount=1

:loop

set /a offset2=%len%-%tempcount%
set numleft=!num2:~0,%tempcount%!
set numright=!num2:~%tempcount%,%offset2%!

for /f "tokens=* delims=0" %%i in ("%numright%") do set "numright=%%i"
if not defined numright exit /b
set /a sum=%numleft%+%numright%

if %sum%==%num% (
  echo %num%
  exit /b
)

if %tempcount%==%len% exit /b
set /a tempcount+=1
goto loop

:strlength
setlocal enabledelayedexpansion
set str=%1
set tempcount=1
:lengthloop
set /a length=%tempcount%-1
if "!str:~%tempcount%,1!"=="" exit /b %tempcount%
set /a tempcount+=1
goto lengthloop

```

{{out}}

```txt

1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999

```




## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      *FLOAT 64
      n% = 0
      FOR i% = 1 TO 999999
        IF FNkaprekar(i%) THEN
          n% += 1
          IF i% < 100001PRINT ; n% ":", i%
        ENDIF
      NEXT
      PRINT "Total Kaprekar numbers under 1,000,000 = "; n%
      END
      
      DEF FNkaprekar(n)
      LOCAL s, t
      s = n^2
      t = 10^(INT(LOG(s)) + 1)
      REPEAT
        t /= 10
        IF t<=n EXIT REPEAT
        IF s-n = INT(s/t)*(t-1) THEN = TRUE
      UNTIL FALSE
      = (n=1)
```

{{out}}

```txt

1:                 1
2:                 9
3:                45
4:                55
5:                99
6:               297
7:               703
8:               999
9:              2223
10:             2728
11:             4879
12:             4950
13:             5050
14:             5292
15:             7272
16:             7777
17:             9999
18:            17344
19:            22222
20:            38962
21:            77778
22:            82656
23:            95121
24:            99999
Total Kaprekar numbers under 1,000,000 = 54

```



## Bracmat


```bracmat
( 0:?n
& 1:?count
& out$(!count 1)
&   whl
  ' ( 1+!n:<1000000:?n
    & ( @( !n^2
         :   #?a
             ( ? (#>0:?b)
             & !a+!b:!n
             & 1+!count:?count
             & (!n:<10000&out$!n|)
             )
         )
      |
      )
    )
& out$(str$("There are " !count " kaprekar numbers less than 1000000"))
);
```

{{out}}

```txt
1 1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999
There are 54 kaprekar numbers less than 1000000
```



## Brat


```Brat
kaprekar = { limit |
  results = []

  1.to limit, { num |
    true? num == 1
    { results << 1 }
    {
      sqr = (num ^ 2).to_s

      0.to (sqr.length - 1) { i |
        lhs = sqr[0,i].to_i
        rhs = sqr[i + 1,-1].to_i

        true? (rhs > 0) && { lhs + rhs == num }
        { results << num }
      }
    }
  }

  results
}

p "Kaprekar numbers below 10,000:"
p kaprekar 10000

p "Number of Kaprekar numbers below 1,000,000:"
p kaprekar(1000000).length
```


{{out}}

```txt

Kaprekar numbers below 10,000:
[1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
Number of Kaprekar numbers below 1,000,000:
54

```



## C

Sample for extra extra credit:

```C>#include <stdio.h

#include <stdint.h>
typedef uint64_t ulong;

int kaprekar(ulong n, int base)
{
	ulong nn = n * n, r, tens = 1;

	if ((nn - n) % (base - 1)) return 0;

	while (tens < n) tens *= base;
	if (n == tens) return 1 == n;

	while ((r = nn % tens) < n) {
		if (nn / tens + r == n) return tens;
		tens *= base;
	}

	return 0;
}

void print_num(ulong n, int base)
{
	ulong q, div = base;

	while (div < n) div *= base;
	while (n && (div /= base)) {
		q = n / div;
		if (q < 10)     putchar(q + '0');
		else            putchar(q + 'a' - 10);
		n -= q * div;
	}
}

int main()
{
	ulong i, tens;
	int cnt = 0;
	int base = 10;

	printf("base 10:\n");
	for (i = 1; i < 1000000; i++)
		if (kaprekar(i, base))
			printf("%3d: %llu\n", ++cnt, i);

	base = 17;
	printf("\nbase %d:\n  1: 1\n", base);
	for (i = 2, cnt = 1; i < 1000000; i++)
		if ((tens = kaprekar(i, base))) {
			printf("%3d: %llu", ++cnt, i);
			printf(" \t"); print_num(i, base);
			printf("\t");  print_num(i * i, base);
			printf("\t");  print_num(i * i / tens, base);
			printf(" + "); print_num(i * i % tens, base);
			printf("\n");
		}

	return 0;
}
```

{{out}}

```txt
base 10:
  1: 1
  2: 9
  3: 45
  4: 55
  5: 99
  6: 297
  7: 703
  8: 999
  9: 2223
 10: 2728
 11: 4879
 12: 4950
 13: 5050
 14: 5292
 15: 7272
 16: 7777
 17: 9999
 ...
 47: 791505
 48: 812890
 49: 818181
 50: 851851
 51: 857143
 52: 961038
 53: 994708
 54: 999999

base 17:
  1: 1
  2: 16 	g	f1	f + 1
  3: 64 	3d	e2g	e + 2g
  4: 225 	d4	a52g	a5 + 2g
  5: 288 	gg	gf01	gf + 1
  6: 1536 	556	1b43b2	1b4 + 3b2
  7: 3377 	bbb	8093b2	809 + 3b2
  8: 4912 	ggg	ggf001	ggf + 1
  9: 7425 	18bd	24e166g	24e + 166g
 10: 9280 	1f1f	39b1b94	39b + 1b94
 ...
 21: 74241 	f1f2	d75f1b94	d75f + 1b94
 22: 76096 	f854	e1f5166g	e1f5 + 166g
 23: 83520 	gggg	gggf0001	gggf + 1
 24: 266224 	33334	a2c52a07g	a2c5 + 2a07g
```



### Factorization

Kaprekar numbers for base <math>b</math> can be directly generated by factorization of <math>b^n - 1</math> and multiplicative inverse of its unitary divisors.  The speed largely depends on how easy the factorization part is, though it's not a problem for relatively small numbers (up to 32-bits, for example).

```c>#include <stdio.h

#include <stdlib.h>
#include <stdint.h>
#include <limits.h>

typedef signed long long xint;

int factorize(xint n, xint* f)
{
	int i = 0;

	inline void get_factor(xint p) {
		if (n % p) return;
		for (f[i] = 1; !(n % p); f[i] *= p, n /= p);
		i++;
	}

	get_factor(2);
	get_factor(3);
	xint p, inc;
	for (p = 5, inc = 4; p * p <= n; p += (inc = 6 - inc))
		get_factor(p);
	if (n > 1) get_factor(n);
	return i;
}

// returns x where a x == 1 mod b
xint mul_inv(xint a, xint b)
{
	xint b0 = b, t, q;
	xint x0 = 0, x1 = 1;
	if (b == 1) return 1;
	while (a > 1) {
		q = a / b;
		t = b, b = a % b, a = t;
		t = x0, x0 = x1 - q * x0, x1 = t;
	}
	if (x1 < 0) x1 += b0;
	return x1;
}

int kaprekars(int base, xint top, xint *out, int max_cnt)
{
	xint f[64], pb;
	int len, cnt = 0;

	if (top >= LLONG_MAX / top) {
		fprintf(stderr, "too large: %lld\n", top);
		abort();
	}

	void kaps(xint a, int i) {
		if (i < len) {
			kaps(a * f[i], i + 1);
			kaps(a, i + 1);
			return;
		}

		xint x = a * mul_inv(a, (pb - 1) / a);
		if (x > 1 && x < top) {
			out[cnt++] = x;
			if (cnt >= max_cnt) {
				fprintf(stderr, "too many results\n");
				abort();
			}
		}
	}

	out[cnt++] = 1;

	for (pb = base; pb <= top * top / base; pb *= base) {
		len = factorize(pb - 1, f);
		if (f[len - 1] <= top) kaps(1, 0);
	}
	return cnt;
}

int main(void)
{
	xint x[1000];
	int len, b;
	
	for (b = 2; b < 99; b++) {
		printf("base %d:\n", b);

		// find all kaprekar numbers that won't overflow
		len = kaprekars(b, INT_MAX, x, 1000);
#if 0
		int i, j;
		xint t;
		for (i = 0; i < len; i++)
			for (j = 0; j < i; j++)
				if (x[i] < x[j])
					t = x[i], x[i] = x[j], x[j] = t;

		for (i = 0; i < len; i++)
			printf("%3d: %lld\n", i + 1, x[i]);
#else
		printf("\t%d kaprepar numbers\n", len);
#endif
	}

	return 0;
}
```



## C++

===Using String Manipulation (very slow)===

```cpp>#include <vector

#include <string>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <utility>

long string2long( const std::string & s ) {
   long result ;
   std::istringstream( s ) >> result ;
   return result ;
}

bool isKaprekar( long number ) {
   long long squarenumber = ((long long)number) * number ;
   std::ostringstream numberbuf ;
   numberbuf << squarenumber ;
   std::string numberstring = numberbuf.str( ) ;
   for ( int i = 0 ; i < numberstring.length( ) ; i++ ) {
      std::string firstpart = numberstring.substr( 0 , i ) ,
                  secondpart = numberstring.substr( i ) ;
      //we do not accept figures ending in a sequence of zeroes
      if ( secondpart.find_first_not_of( "0" ) == std::string::npos ) {
	 return false ;
      }
      if ( string2long( firstpart ) + string2long( secondpart ) == number ) {
	 return true ;
      }
   }
   return false ;
}

int main( ) {
   std::vector<long> kaprekarnumbers ;
   kaprekarnumbers.push_back( 1 ) ;
   for ( int i = 2 ; i < 1000001 ; i++ ) {
      if ( isKaprekar( i ) ) 
	 kaprekarnumbers.push_back( i ) ;
   }
   std::vector<long>::const_iterator svi = kaprekarnumbers.begin( ) ;
   std::cout << "Kaprekar numbers up to 10000: \n" ;
   while ( *svi < 10000 ) {
      std::cout << *svi << " " ;
      svi++ ;
   }
   std::cout << '\n' ;
   std::cout << "All the Kaprekar numbers up to 1000000 :\n" ;
   std::copy( kaprekarnumbers.begin( ) , kaprekarnumbers.end( ) ,
	 std::ostream_iterator<long>( std::cout , "\n" ) ) ;
   std::cout << "There are " << kaprekarnumbers.size( )
      << " Kaprekar numbers less than one million!\n" ;
   return 0 ;
}
```

{{out}}

```txt
Kaprekar numbers up to 10000: 
1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999 
All the Kaprekar numbers up to 1000000 :
1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999
17344
.....
818181
851851
857143
961038
994708
999999
There are 54 Kaprekar numbers less than one million!
```

===Casting Out Nines (fast)===
The  code <code>"if ((k*(k-1))%(Base-1) == 0)"</code> is explained here: [[Casting out nines]].


```cpp

// Generate Kaperkar Numbers
//
// Nigel Galloway. June 24th., 2012
//
#include <iostream>
int main() {
	const int Base = 10;
	const int N = 6;
	int Paddy_cnt = 0;
	for (int nz=1; nz<=N; nz++)
		for (unsigned long long int k=pow((double)Base,nz-1); k<pow((double)Base,nz); k++)
			if ((k*(k-1))%(Base-1) == 0)
				for (int n=nz; n<nz*2; n++){
					const unsigned long long int B = pow((double)Base,n);
					const double nr = k*(B-k)/(B-1);
					const int q = k-nr;
					if ((k*k==q*B+nr && 0<nr)){
						std::cout << std::dec << ++Paddy_cnt << ": " << k << " is "  << q << " + " << (int)nr << " and squared is " << k*k << ". It is a member of Residual Set " << k%(Base-1) << "\n";
						break;
				}}
	return 0;
}

```

Produces:

```txt
1: 1 is 0 + 1 and squared is 1. It is a member of Residual Set 1
2: 9 is 8 + 1 and squared is 81. It is a member of Residual Set 0
3: 45 is 20 + 25 and squared is 2025. It is a member of Residual Set 0
4: 55 is 30 + 25 and squared is 3025. It is a member of Residual Set 1
5: 99 is 98 + 1 and squared is 9801. It is a member of Residual Set 0
6: 297 is 88 + 209 and squared is 88209. It is a member of Residual Set 0
7: 703 is 494 + 209 and squared is 494209. It is a member of Residual Set 1
8: 999 is 998 + 1 and squared is 998001. It is a member of Residual Set 0
9: 2223 is 494 + 1729 and squared is 4941729. It is a member of Residual Set 0
10: 2728 is 744 + 1984 and squared is 7441984. It is a member of Residual Set 1
11: 4879 is 238 + 4641 and squared is 23804641. It is a member of Residual Set 1
12: 4950 is 2450 + 2500 and squared is 24502500. It is a member of Residual Set 0
13: 5050 is 2550 + 2500 and squared is 25502500. It is a member of Residual Set 1
14: 5292 is 28 + 5264 and squared is 28005264. It is a member of Residual Set 0
15: 7272 is 5288 + 1984 and squared is 52881984. It is a member of Residual Set 0
16: 7777 is 6048 + 1729 and squared is 60481729. It is a member of Residual Set 1
17: 9999 is 9998 + 1 and squared is 99980001. It is a member of Residual Set 0
18: 17344 is 3008 + 14336 and squared is 300814336. It is a member of Residual Set 1
19: 22222 is 4938 + 17284 and squared is 493817284. It is a member of Residual Set 1
20: 38962 is 1518 + 37444 and squared is 1518037444. It is a member of Residual Set 1
21: 77778 is 60494 + 17284 and squared is 6049417284. It is a member of Residual Set 0
22: 82656 is 68320 + 14336 and squared is 6832014336. It is a member of Residual Set 0
23: 95121 is 90480 + 4641 and squared is 9048004641. It is a member of Residual Set 0
24: 99999 is 99998 + 1 and squared is 9999800001. It is a member of Residual Set 0
25: 142857 is 20408 + 122449 and squared is 20408122449. It is a member of Residual Set 0
26: 148149 is 21948 + 126201 and squared is 21948126201. It is a member of Residual Set 0
27: 181819 is 33058 + 148761 and squared is 33058148761. It is a member of Residual Set 1
28: 187110 is 35010 + 152100 and squared is 35010152100. It is a member of Residual Set 0
29: 208495 is 43470 + 165025 and squared is 43470165025. It is a member of Residual Set 1
30: 318682 is 101558 + 217124 and squared is 101558217124. It is a member of Residual Set 1
31: 329967 is 108878 + 221089 and squared is 108878221089. It is a member of Residual Set 0
32: 351352 is 123448 + 227904 and squared is 123448227904. It is a member of Residual Set 1
33: 356643 is 127194 + 229449 and squared is 127194229449. It is a member of Residual Set 0
34: 390313 is 152344 + 237969 and squared is 152344237969. It is a member of Residual Set 1
35: 461539 is 213018 + 248521 and squared is 213018248521. It is a member of Residual Set 1
36: 466830 is 217930 + 248900 and squared is 217930248900. It is a member of Residual Set 0
37: 499500 is 249500 + 250000 and squared is 249500250000. It is a member of Residual Set 0
38: 500500 is 250500 + 250000 and squared is 250500250000. It is a member of Residual Set 1
39: 533170 is 284270 + 248900 and squared is 284270248900. It is a member of Residual Set 1
40: 538461 is 289940 + 248521 and squared is 289940248521. It is a member of Residual Set 0
41: 609687 is 371718 + 237969 and squared is 371718237969. It is a member of Residual Set 0
42: 627615 is 39390 + 588225 and squared is 393900588225. It is a member of Residual Set 0
43: 643357 is 413908 + 229449 and squared is 413908229449. It is a member of Residual Set 1
44: 648648 is 420744 + 227904 and squared is 420744227904. It is a member of Residual Set 0
45: 670033 is 448944 + 221089 and squared is 448944221089. It is a member of Residual Set 1
46: 681318 is 464194 + 217124 and squared is 464194217124. It is a member of Residual Set 0
47: 791505 is 626480 + 165025 and squared is 626480165025. It is a member of Residual Set 0
48: 812890 is 660790 + 152100 and squared is 660790152100. It is a member of Residual Set 1
49: 818181 is 669420 + 148761 and squared is 669420148761. It is a member of Residual Set 0
50: 851851 is 725650 + 126201 and squared is 725650126201. It is a member of Residual Set 1
51: 857143 is 734694 + 122449 and squared is 734694122449. It is a member of Residual Set 1
52: 961038 is 923594 + 37444 and squared is 923594037444. It is a member of Residual Set 0
53: 994708 is 989444 + 5264 and squared is 989444005264. It is a member of Residual Set 1
54: 999999 is 999998 + 1 and squared is 999998000001. It is a member of Residual Set 0
```

The code may be modified to use a base other than 10:

```cpp

const int Base = 16;
const int N = 4;
std::cout << std::dec << ++Paddy_cnt << ": " << std::hex << k << " is "  << q << " + " << (int)nr << " and squared is " << k*k << ". It is a member of Residual Set " << k%(Base-1) << "\n";

```

Which produces:

```txt
1: 1 is 0 + 1 and squared is 1. It is a member of Residual Set 1
2: 6 is 2 + 4 and squared is 24. It is a member of Residual Set 6
3: a is 6 + 4 and squared is 64. It is a member of Residual Set a
4: f is e + 1 and squared is e1. It is a member of Residual Set 0
5: 33 is a + 29 and squared is a29. It is a member of Residual Set 6
6: 55 is 1c + 39 and squared is 1c39. It is a member of Residual Set a
7: 5b is 2 + 59 and squared is 2059. It is a member of Residual Set 1
8: 78 is 38 + 40 and squared is 3840. It is a member of Residual Set 0
9: 88 is 48 + 40 and squared is 4840. It is a member of Residual Set 1
10: ab is 72 + 39 and squared is 7239. It is a member of Residual Set 6
11: cd is a4 + 29 and squared is a429. It is a member of Residual Set a
12: ff is fe + 1 and squared is fe01. It is a member of Residual Set 0
13: 15f is 1e + 141 and squared is 1e141. It is a member of Residual Set 6
14: 334 is a4 + 290 and squared is a4290. It is a member of Residual Set a
15: 38e is ca + 2c4 and squared is ca2c4. It is a member of Residual Set a
16: 492 is 14e + 344 and squared is 14e344. It is a member of Residual Set 0
17: 4ed is 184 + 369 and squared is 184369. It is a member of Residual Set 1
18: 7e0 is 3e0 + 400 and squared is 3e0400. It is a member of Residual Set 6
19: 820 is 420 + 400 and squared is 420400. It is a member of Residual Set a
20: b13 is 7aa + 369 and squared is 7aa369. It is a member of Residual Set 0
21: b6e is 82a + 344 and squared is 82a344. It is a member of Residual Set 1
22: c72 is 9ae + 2c4 and squared is 9ae2c4. It is a member of Residual Set 6
23: ccc is a3c + 290 and squared is a3c290. It is a member of Residual Set 6
24: ea1 is d60 + 141 and squared is d60141. It is a member of Residual Set a
25: fa5 is f4c + 59 and squared is f4c059. It is a member of Residual Set 0
26: fff is ffe + 1 and squared is ffe001. It is a member of Residual Set 0
27: 191a is 276 + 16a4 and squared is 27616a4. It is a member of Residual Set 6
28: 2a2b is 6f2 + 2339 and squared is 6f22339. It is a member of Residual Set a
29: 3c3c is e2c + 2e10 and squared is e2c2e10. It is a member of Residual Set 0
30: 4444 is 1234 + 3210 and squared is 12343210. It is a member of Residual Set 1
31: 5556 is 1c72 + 38e4 and squared is 1c7238e4. It is a member of Residual Set 6
32: 6667 is 28f6 + 3d71 and squared is 28f63d71. It is a member of Residual Set a
33: 7f80 is 3f80 + 4000 and squared is 3f804000. It is a member of Residual Set 0
34: 8080 is 4080 + 4000 and squared is 40804000. It is a member of Residual Set 1
35: 9999 is 5c28 + 3d71 and squared is 5c283d71. It is a member of Residual Set 6
36: aaaa is 71c6 + 38e4 and squared is 71c638e4. It is a member of Residual Set a
37: bbbc is 89ac + 3210 and squared is 89ac3210. It is a member of Residual Set 0
38: c3c4 is 95b4 + 2e10 and squared is 95b42e10. It is a member of Residual Set 1
39: d5d5 is b29c + 2339 and squared is b29c2339. It is a member of Residual Set 6
40: e6e6 is d042 + 16a4 and squared is d04216a4. It is a member of Residual Set a
41: ffff is fffe + 1 and squared is fffe0001. It is a member of Residual Set 0
```

===Casting Out Nines C++11 For Each Generator (v.fast)===
For details of ran and co9 see: http://rosettacode.org/wiki/Casting_out_nines#C.2B.2B11_For_Each_Generator

```cpp
// Generate Kaprekar Numbers using Casting Out Nines Generator
//
// Nigel Galloway. July 13th., 2012
//
#include <cmath>
int main() {
	const ran r(10);
	int Paddy_cnt = 0;
	for (int nz=1; nz<=6; nz++)
		for (unsigned long long int k : co9(std::pow(r.base,nz-1),std::pow(r.base,nz)-1,&r))
			for (int n=nz; n<nz*2; n++) {
				const unsigned long long int B = pow(r.base,n);
				const double nr = k*(B-k)/(B-1);
				const int q = k-nr;
				if ((k*k==q*B+nr && 0<nr)) {
					std::cout << ++Paddy_cnt << ": " << k << " is "  << q << " + " << (int)nr << " and squared is " << k*k << ". It is a member of Residual Set " << k%(r.base-1) << "\n";
			}}
	return 0;
}
```

Produces:

```txt
1: 1 is 0 + 1 and squared is 1. It is a member of Residual Set 1
2: 9 is 8 + 1 and squared is 81. It is a member of Residual Set 0
3: 45 is 20 + 25 and squared is 2025. It is a member of Residual Set 0
4: 55 is 30 + 25 and squared is 3025. It is a member of Residual Set 1
5: 99 is 98 + 1 and squared is 9801. It is a member of Residual Set 0
6: 297 is 88 + 209 and squared is 88209. It is a member of Residual Set 0
7: 703 is 494 + 209 and squared is 494209. It is a member of Residual Set 1
8: 999 is 998 + 1 and squared is 998001. It is a member of Residual Set 0
9: 2223 is 494 + 1729 and squared is 4941729. It is a member of Residual Set 0
10: 2728 is 744 + 1984 and squared is 7441984. It is a member of Residual Set 1
11: 4879 is 238 + 4641 and squared is 23804641. It is a member of Residual Set 1
12: 4950 is 2450 + 2500 and squared is 24502500. It is a member of Residual Set 0
13: 5050 is 2550 + 2500 and squared is 25502500. It is a member of Residual Set 1
14: 5292 is 28 + 5264 and squared is 28005264. It is a member of Residual Set 0
15: 7272 is 5288 + 1984 and squared is 52881984. It is a member of Residual Set 0
16: 7777 is 6048 + 1729 and squared is 60481729. It is a member of Residual Set 1
17: 9999 is 9998 + 1 and squared is 99980001. It is a member of Residual Set 0
18: 17344 is 3008 + 14336 and squared is 300814336. It is a member of Residual Set 1
19: 22222 is 4938 + 17284 and squared is 493817284. It is a member of Residual Set 1
20: 38962 is 1518 + 37444 and squared is 1518037444. It is a member of Residual Set 1
21: 77778 is 60494 + 17284 and squared is 6049417284. It is a member of Residual Set 0
22: 82656 is 68320 + 14336 and squared is 6832014336. It is a member of Residual Set 0
23: 95121 is 90480 + 4641 and squared is 9048004641. It is a member of Residual Set 0
24: 99999 is 99998 + 1 and squared is 9999800001. It is a member of Residual Set 0
25: 142857 is 20408 + 122449 and squared is 20408122449. It is a member of Residual Set 0
26: 148149 is 21948 + 126201 and squared is 21948126201. It is a member of Residual Set 0
27: 181819 is 33058 + 148761 and squared is 33058148761. It is a member of Residual Set 1
28: 187110 is 35010 + 152100 and squared is 35010152100. It is a member of Residual Set 0
29: 208495 is 43470 + 165025 and squared is 43470165025. It is a member of Residual Set 1
30: 318682 is 101558 + 217124 and squared is 101558217124. It is a member of Residual Set 1
31: 329967 is 108878 + 221089 and squared is 108878221089. It is a member of Residual Set 0
32: 351352 is 123448 + 227904 and squared is 123448227904. It is a member of Residual Set 1
33: 356643 is 127194 + 229449 and squared is 127194229449. It is a member of Residual Set 0
34: 390313 is 152344 + 237969 and squared is 152344237969. It is a member of Residual Set 1
35: 461539 is 213018 + 248521 and squared is 213018248521. It is a member of Residual Set 1
36: 466830 is 217930 + 248900 and squared is 217930248900. It is a member of Residual Set 0
37: 499500 is 249500 + 250000 and squared is 249500250000. It is a member of Residual Set 0
38: 500500 is 250500 + 250000 and squared is 250500250000. It is a member of Residual Set 1
39: 533170 is 284270 + 248900 and squared is 284270248900. It is a member of Residual Set 1
40: 538461 is 289940 + 248521 and squared is 289940248521. It is a member of Residual Set 0
41: 609687 is 371718 + 237969 and squared is 371718237969. It is a member of Residual Set 0
42: 627615 is 39390 + 588225 and squared is 393900588225. It is a member of Residual Set 0
43: 643357 is 413908 + 229449 and squared is 413908229449. It is a member of Residual Set 1
44: 648648 is 420744 + 227904 and squared is 420744227904. It is a member of Residual Set 0
45: 670033 is 448944 + 221089 and squared is 448944221089. It is a member of Residual Set 1
46: 681318 is 464194 + 217124 and squared is 464194217124. It is a member of Residual Set 0
47: 791505 is 626480 + 165025 and squared is 626480165025. It is a member of Residual Set 0
48: 812890 is 660790 + 152100 and squared is 660790152100. It is a member of Residual Set 1
49: 818181 is 669420 + 148761 and squared is 669420148761. It is a member of Residual Set 0
50: 851851 is 725650 + 126201 and squared is 725650126201. It is a member of Residual Set 1
51: 857143 is 734694 + 122449 and squared is 734694122449. It is a member of Residual Set 1
52: 961038 is 923594 + 37444 and squared is 923594037444. It is a member of Residual Set 0
53: 994708 is 989444 + 5264 and squared is 989444005264. It is a member of Residual Set 1
54: 999999 is 999998 + 1 and squared is 999998000001. It is a member of Residual Set 0
```

Changing main:

```cpp

	const ran r = ran(16);
					std::cout << std::dec << ++Paddy_cnt << ": " << std::hex << k << " is "  << q << " + " << (int)nr << " and squared is " << k*k << ". It is a member of Residual Set " << k%(r.base-1) << "\n";

```

Produces:

```txt
1: 1 is 0 + 1 and squared is 1. It is a member of Residual Set 1
2: 6 is 2 + 4 and squared is 24. It is a member of Residual Set 6
3: a is 6 + 4 and squared is 64. It is a member of Residual Set a
4: f is e + 1 and squared is e1. It is a member of Residual Set 0
5: 33 is a + 29 and squared is a29. It is a member of Residual Set 6
6: 55 is 1c + 39 and squared is 1c39. It is a member of Residual Set a
7: 5b is 2 + 59 and squared is 2059. It is a member of Residual Set 1
8: 78 is 38 + 40 and squared is 3840. It is a member of Residual Set 0
9: 88 is 48 + 40 and squared is 4840. It is a member of Residual Set 1
10: ab is 72 + 39 and squared is 7239. It is a member of Residual Set 6
11: cd is a4 + 29 and squared is a429. It is a member of Residual Set a
12: ff is fe + 1 and squared is fe01. It is a member of Residual Set 0
13: 15f is 1e + 141 and squared is 1e141. It is a member of Residual Set 6
14: 334 is a4 + 290 and squared is a4290. It is a member of Residual Set a
15: 38e is ca + 2c4 and squared is ca2c4. It is a member of Residual Set a
16: 492 is 14e + 344 and squared is 14e344. It is a member of Residual Set 0
17: 4ed is 184 + 369 and squared is 184369. It is a member of Residual Set 1
18: 7e0 is 3e0 + 400 and squared is 3e0400. It is a member of Residual Set 6
19: 820 is 420 + 400 and squared is 420400. It is a member of Residual Set a
20: b13 is 7aa + 369 and squared is 7aa369. It is a member of Residual Set 0
21: b6e is 82a + 344 and squared is 82a344. It is a member of Residual Set 1
22: c72 is 9ae + 2c4 and squared is 9ae2c4. It is a member of Residual Set 6
23: ccc is a3c + 290 and squared is a3c290. It is a member of Residual Set 6
24: ea1 is d60 + 141 and squared is d60141. It is a member of Residual Set a
25: fa5 is f4c + 59 and squared is f4c059. It is a member of Residual Set 0
26: fff is ffe + 1 and squared is ffe001. It is a member of Residual Set 0
27: 191a is 276 + 16a4 and squared is 27616a4. It is a member of Residual Set 6
28: 2a2b is 6f2 + 2339 and squared is 6f22339. It is a member of Residual Set a
29: 3c3c is e2c + 2e10 and squared is e2c2e10. It is a member of Residual Set 0
30: 4444 is 1234 + 3210 and squared is 12343210. It is a member of Residual Set 1
31: 5556 is 1c72 + 38e4 and squared is 1c7238e4. It is a member of Residual Set 6
32: 6667 is 28f6 + 3d71 and squared is 28f63d71. It is a member of Residual Set a
33: 7f80 is 3f80 + 4000 and squared is 3f804000. It is a member of Residual Set 0
34: 8080 is 4080 + 4000 and squared is 40804000. It is a member of Residual Set 1
35: 9999 is 5c28 + 3d71 and squared is 5c283d71. It is a member of Residual Set 6
36: aaaa is 71c6 + 38e4 and squared is 71c638e4. It is a member of Residual Set a
37: bbbc is 89ac + 3210 and squared is 89ac3210. It is a member of Residual Set 0
38: c3c4 is 95b4 + 2e10 and squared is 95b42e10. It is a member of Residual Set 1
39: d5d5 is b29c + 2339 and squared is b29c2339. It is a member of Residual Set 6
40: e6e6 is d042 + 16a4 and squared is d04216a4. It is a member of Residual Set a
41: ffff is fffe + 1 and squared is fffe0001. It is a member of Residual Set 0
```



## C sharp


```csharp
using System;
using System.Collections.Generic;

public class KaprekarNumbers {

    /// <summary>
    /// The entry point of the program, where the program control starts and ends.
    /// </summary>
    public static void Main() {
        int count = 0;

        foreach ( ulong i in _kaprekarGenerator(999999) ) {
            Console.WriteLine(i);
            count++;
        }

        Console.WriteLine("There are {0} Kaprekar numbers less than 1000000.", count);
    }

    /// <summary>
    /// Generator function which generates the Kaprekar numbers.
    /// </summary>
    /// <returns>The generator.</returns>
    /// <param name="max">The maximum value of the numbers generated.</param>
    private static IEnumerable<ulong> _kaprekarGenerator(ulong max) {

        ulong next = 1;

        // 1 is always a Kaprekar number.
        yield return next;

        for ( next = 2; next <= max; next++ ) {

            ulong square = next * next;

            for ( ulong check = 10; check <= 10000000000000000000; check *= 10 ) {
                // Check the square against each power of 10 from 10^1 to 10^19 (highest which can be
                // represented by a ulong)

                // If the power of 10 to be checked against is greater than or equal to the square, stop checking
                if ( square <= check )
                    break;

                // Given a power of 10 as 10^n, the remainder when dividing the square number by that power
                // of 10 is equal to the last n digits of the number (starting from the right) and the
                // quotient gives the remaining digits.
                // If the last n digits are all zeroes, then the remainder will be zero, which is not
                // accepted.

                ulong r = square % check;
                ulong q = (square - r) / check;

                if ( r != 0 && q + r == next ) {
                    yield return next;
                    break;
                }
            }

        }

    }

}
```


{{out}}

```txt
1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999
17344
22222
38962
77778
82656
95121
99999
142857
148149
181819
187110
208495
318682
329967
351352
356643
390313
461539
466830
499500
500500
533170
538461
609687
627615
643357
648648
670033
681318
791505
812890
818181
851851
857143
961038
994708
999999
There are 54 Kaprekar numbers less than 1000000.
```



## CoffeeScript

{{trans|Kotlin}}

```coffeescript
splitAt = (str, idx) ->
    ans = [ str.substring(0, idx), str.substring(idx) ]
    if ans[0] == ""
        ans[0] = "0"
    ans

getKaprekarParts = (longValue, sqrStr, base) ->
    for j in [ 0 .. sqrStr.length / 2 ]
        parts = splitAt(sqrStr, j)
        nums = (parseInt(n, base) for n in parts)

        # if the right part is all zeroes, then it will be forever, so break
        if nums[1] == 0
            return null
        if nums[0] + nums[1] == longValue
            return parts
    null

base = 10
count = 0
max = 1000000
for i in [1..max]
    i2 = i * i
    s = i2.toString(base)
    p = getKaprekarParts i, s, base
    if p
        console.log i, i.toString(base), s, p.join '+'
        count++
console.log "#{count} Kaprekar numbers < #{max} (base 10) in base #{base}"
```

{{out}}

```txt
1 '1' '1' '0+1'
9 '9' '81' '8+1'
45 '45' '2025' '20+25'
55 '55' '3025' '30+25'
99 '99' '9801' '98+01'
297 '297' '88209' '88+209'
703 '703' '494209' '494+209'
999 '999' '998001' '998+001'
...
999999 '999999' '999998000001' '999998+000001'
54 Kaprekar numbers < 1000000 (base 10) in base 10
```



## Common Lisp


###  Fast solution using casting out nines filter 


```lisp
;; make an infinite list whose accumulated sums give all
;; numbers n where n mod (base - 1) == n^2 mod (base - 1)
(defun res-list (base)
    (let* ((b (- base 1))
           (l (remove-if-not
		    (lambda (x) (= (rem x b) (rem (* x x) b)))
		    (loop for x from 0 below b collect x)))
	   (ret (append l (list b)))
	   (cycle (mapcar #'- (cdr ret) ret)))
	(setf (cdr (last cycle)) cycle)))
 
(defun kaprekar-p (n &optional (base 10))
   "tests if n is kaprekar in base; if so, return left and right half"
   (let ((nn (* n n)) (tens 1))
	; Find a start value for base power.  nn/tens + (nn mod tens) == n
	; can't be sastified if tens <= n: nn/tens = n * n / tens > n
	(loop while (< tens n) do
	      (setf tens (* tens base)))
	(if (= tens n)  ; n a power of base, can't be a solution except 1
	    (if (= n 1) (values T 0 1))
	    (loop
	       (let ((left (truncate nn tens)) (right (mod nn tens)))
		    (cond ((>= right n) (return nil))
			  ((= n (+ left right)) (return (values T left right))))
		    (setf tens (* base tens)))))))
 
(defun ktest (top &optional (base 10))
   (format t "   #    Value     Left    Right       Squared (base ~D)~%" base)
   (let ((fmt (format nil "~~4D ~~~D,8R ~~~D,8R ~~~D,8R ~~~D,13R~~%"
                      base base base base base))
	 (res (res-list base))
	 (n 0))
   	 (loop with cnt = 0 while (<= n top) do
	 	(setf n (+ n (car res)))
		(setf res (cdr res))
	 	(multiple-value-bind (k l r) (kaprekar-p n base)
		   (when k (format t fmt (incf cnt) n l r (* n n)))))))
 
(ktest 1000000)
(terpri)
(ktest 1000000 17)

```

{{out}}

```txt

   #      Value       Left      Right         Squared (base 10)
   1          1          0          1               1
   2          9          8          1              81
   3         45         20         25            2025
   4         55         30         25            3025
   5         99         98          1            9801
...
  52     961038     923594      37444    923594037444
  53     994708     989444       5264    989444005264
  54     999999     999998          1    999998000001

   #      Value       Left      Right         Squared (base 17)
   1          1          0          1               1
   2          G          F          1              F1
   3         3D          E         2G             E2G
...
  22       F854       E1F5       166G        E1F5166G
  23       GGGG       GGGF          1        GGGF0001
  24      33334       A2C5      2A07G       A2C52A07G

```


### In the style of the C++ generator


```lisp

;; Generate Kaprekar Numbers using Casting Out Nines Generator
;;
;; Nigel Galloway - October 1st., 2012
;;
(defconstant Base 10)
(defconstant MAX 1000000)
(defconstant ran (let ((N ()) (Base-1 (- Base 1))) (do ((cnt Base-1 (- cnt 1))) ((zerop cnt) (return N))
   (if (= (mod (* cnt (- cnt 1)) Base-1) 0) (setf N (cons cnt N))))))

(defun kap () (let ((Paddy_cnt 0) (Base-1 (- Base 1))) (do ((n 0 (+ n Base-1))) ((> n MAX) ()) (dolist (G ran)
   (let ((N (+ G n))) (if (>= MAX N) (let ((kk (* N N))) (do ((B Base (* B Base))) (nil)
     (let (( nr (/ (* N (- B N)) (- B 1)))) (if (< 0 nr) (let ((q (floor (- N nr)))) (if (= kk (+ nr (* q B)))
       (format t "~3d: ~8d is ~8d + ~8d and squared is ~8d~&" (incf Paddy_cnt) N q nr kk))
     (if (> B kk) (return)))))))))))))

```

{{out}}

```txt

  1:        1 is        0 +        1 and squared is 1
  2:        9 is        8 +        1 and squared is 81
  3:       45 is       20 +       25 and squared is 2025
  4:       55 is       30 +       25 and squared is 3025
  5:       99 is       98 +        1 and squared is 9801
  6:      297 is       88 +      209 and squared is 88209
  7:      703 is      494 +      209 and squared is 494209
  8:      999 is      998 +        1 and squared is 998001
  9:     2223 is      494 +     1729 and squared is 4941729
 10:     2728 is      744 +     1984 and squared is 7441984
 11:     4879 is      238 +     4641 and squared is 23804641
 12:     4950 is     2450 +     2500 and squared is 24502500
 13:     5050 is     2550 +     2500 and squared is 25502500
 14:     5292 is       28 +     5264 and squared is 28005264
 15:     7272 is     5288 +     1984 and squared is 52881984
 16:     7777 is     6048 +     1729 and squared is 60481729
 17:     9999 is     9998 +        1 and squared is 99980001
 18:    17344 is     3008 +    14336 and squared is 300814336
 19:    22222 is     4938 +    17284 and squared is 493817284
 20:    38962 is     1518 +    37444 and squared is 1518037444
 21:    77778 is    60494 +    17284 and squared is 6049417284
 22:    82656 is    68320 +    14336 and squared is 6832014336
 23:    95121 is    90480 +     4641 and squared is 9048004641
 24:    99999 is    99998 +        1 and squared is 9999800001
 25:   142857 is    20408 +   122449 and squared is 20408122449
 26:   148149 is    21948 +   126201 and squared is 21948126201
 27:   181819 is    33058 +   148761 and squared is 33058148761
 28:   187110 is    35010 +   152100 and squared is 35010152100
 29:   208495 is    43470 +   165025 and squared is 43470165025
 30:   318682 is   101558 +   217124 and squared is 101558217124
 31:   329967 is   108878 +   221089 and squared is 108878221089
 32:   351352 is   123448 +   227904 and squared is 123448227904
 33:   356643 is   127194 +   229449 and squared is 127194229449
 34:   390313 is   152344 +   237969 and squared is 152344237969
 35:   461539 is   213018 +   248521 and squared is 213018248521
 36:   466830 is   217930 +   248900 and squared is 217930248900
 37:   499500 is   249500 +   250000 and squared is 249500250000
 38:   500500 is   250500 +   250000 and squared is 250500250000
 39:   533170 is   284270 +   248900 and squared is 284270248900
 40:   538461 is   289940 +   248521 and squared is 289940248521
 41:   609687 is   371718 +   237969 and squared is 371718237969
 42:   627615 is    39390 +   588225 and squared is 393900588225
 43:   643357 is   413908 +   229449 and squared is 413908229449
 44:   648648 is   420744 +   227904 and squared is 420744227904
 45:   670033 is   448944 +   221089 and squared is 448944221089
 46:   681318 is   464194 +   217124 and squared is 464194217124
 47:   791505 is   626480 +   165025 and squared is 626480165025
 48:   812890 is   660790 +   152100 and squared is 660790152100
 49:   818181 is   669420 +   148761 and squared is 669420148761
 50:   851851 is   725650 +   126201 and squared is 725650126201
 51:   857143 is   734694 +   122449 and squared is 734694122449
 52:   961038 is   923594 +    37444 and squared is 923594037444
 53:   994708 is   989444 +     5264 and squared is 989444005264
 54:   999999 is   999998 +        1 and squared is 999998000001

```

With the following timings:

```txt

; cpu time (non-gc) 4.680028 sec user, 0.140401 sec system
; cpu time (gc)     3.369624 sec user, 0.015600 sec system
; cpu time (total)  8.049652 sec user, 0.156001 sec system
; real time  8.196000 sec
; space allocation:
;  1,901 cons cells, 674,485,072 other bytes, 0 static bytes

```

Changing (defconstant MAX 10000000) adds the following:

```txt

 55:  4444444 is  1975308 +  2469136 and squared is 19753082469136
 56:  4927941 is  2428460 +  2499481 and squared is 24284602499481
 57:  5072059 is  2572578 +  2499481 and squared is 25725782499481
 58:  5479453 is   300244 +  5179209 and squared is 30024405179209
 59:  5555556 is  3086420 +  2469136 and squared is 30864202469136
 60:  8161912 is   666168 +  7495744 and squared is 66616807495744
 61:  9372385 is  8784160 +   588225 and squared is 87841600588225
 62:  9999999 is  9999998 +        1 and squared is 99999980000001

```

With the following timings:

```txt

; cpu time (non-gc) 73.850878 sec (00:01:13.850878) user, 0.140400 sec system
; cpu time (gc)     43.274673 sec user, 0.000000 sec system
; cpu time (total)  117.125551 sec (00:01:57.125551) user, 0.140400 sec system
; real time  117.441000 sec (00:01:57.441000)
; space allocation:
;  3,224 cons cells, 242,904,552 other bytes, 0 static bytes

```

Changing:
:(defconstant Base 16)
:(defconstant MAX (* 256 256))
:(format t "~3d: ~6x is ~6x + ~6x and squared is ~6x~&" Paddy_cnt Nigel q nr kk)
Produces:

```txt

  1:      1 is      0 +      1 and squared is      1
  2:      6 is      2 +      4 and squared is     24
  3:      a is      6 +      4 and squared is     64
  4:      f is      e +      1 and squared is     e1
  5:     33 is      a +     29 and squared is    a29
  6:     55 is     1c +     39 and squared is   1c39
  7:     5b is      2 +     59 and squared is   2059
  8:     78 is     38 +     40 and squared is   3840
  9:     88 is     48 +     40 and squared is   4840
 10:     ab is     72 +     39 and squared is   7239
 11:     cd is     a4 +     29 and squared is   a429
 12:     ff is     fe +      1 and squared is   fe01
 13:    15f is     1e +    141 and squared is  1e141
 14:    334 is     a4 +    290 and squared is  a4290
 15:    38e is     ca +    2c4 and squared is  ca2c4
 16:    492 is    14e +    344 and squared is 14e344
 17:    4ed is    184 +    369 and squared is 184369
 18:    7e0 is    3e0 +    400 and squared is 3e0400
 19:    820 is    420 +    400 and squared is 420400
 20:    b13 is    7aa +    369 and squared is 7aa369
 21:    b6e is    82a +    344 and squared is 82a344
 22:    c72 is    9ae +    2c4 and squared is 9ae2c4
 23:    ccc is    a3c +    290 and squared is a3c290
 24:    ea1 is    d60 +    141 and squared is d60141
 25:    fa5 is    f4c +     59 and squared is f4c059
 26:    fff is    ffe +      1 and squared is ffe001
 27:   191a is    276 +   16a4 and squared is 27616a4
 28:   2a2b is    6f2 +   2339 and squared is 6f22339
 29:   3c3c is    e2c +   2e10 and squared is e2c2e10
 30:   4444 is   1234 +   3210 and squared is 12343210
 31:   5556 is   1c72 +   38e4 and squared is 1c7238e4
 32:   6667 is   28f6 +   3d71 and squared is 28f63d71
 33:   7f80 is   3f80 +   4000 and squared is 3f804000
 34:   8080 is   4080 +   4000 and squared is 40804000
 35:   9999 is   5c28 +   3d71 and squared is 5c283d71
 36:   aaaa is   71c6 +   38e4 and squared is 71c638e4
 37:   bbbc is   89ac +   3210 and squared is 89ac3210
 38:   c3c4 is   95b4 +   2e10 and squared is 95b42e10
 39:   d5d5 is   b29c +   2339 and squared is b29c2339
 40:   e6e6 is   d042 +   16a4 and squared is d04216a4
 41:   ffff is   fffe +      1 and squared is fffe0001

```

With the following timings:

```txt

; cpu time (non-gc) 0.109200 sec user, 0.140401 sec system
; cpu time (gc)     0.109202 sec user, 0.000000 sec system
; cpu time (total)  0.218402 sec user, 0.140401 sec system
; real time  0.345000 sec
; space allocation:
;  1,253 cons cells, 22,987,752 other bytes, 0 static bytes

```



## D


### Straightforward Version


```d
import std.stdio, std.conv, std.algorithm, std.range;

bool isKaprekar(in long n) pure /*nothrow*/ @safe
in {
    assert(n > 0, "isKaprekar(n) is defined for n > 0.");
} body {
    if (n == 1)
        return true;
    immutable sn = text(n ^^ 2);

    foreach (immutable i; 1 .. sn.length) {
        immutable a = sn[0 .. i].to!long;
        immutable b = sn[i .. $].to!long;
        if (b && a + b == n)
            return true;
    }

    return false;
}

void main() {
    iota(1, 10_000).filter!isKaprekar.writeln;
    iota(1, 1_000_000).count!isKaprekar.writeln;
}
```

{{out}}

```txt
[1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
54
```



### Faster Version

Right to left:

```d
bool isKaprekar(in uint n) pure nothrow @nogc @safe {
    ulong powr = n ^^ 2UL;
    ulong r, l, tens = 10;
    while (r < n) {
        r = powr % tens;
        l = powr / tens;
        if (r && (l + r == n))
            return true;
        tens *= 10;
    }
    return false;
}

void main() {
    import std.stdio;

    int count = 1;
    foreach (immutable i; 1 .. 1_000_000)
        if (i.isKaprekar)
            writefln("%d: %d", count++, i);
}
```

{{out}}

```txt
1: 1
2: 9
3: 45
4: 55
5: 99
6: 297
7: 703
8: 999
9: 2223
10: 2728
11: 4879
12: 4950
13: 5050
14: 5292
15: 7272
16: 7777
17: 9999
...
51: 857143
52: 961038
53: 994708
54: 999999
```


## Dart


```dart

import 'dart:math';
  void main()
{
  
  int x1;
  for(x1=1;x1<1000000;x1++){
  int x;
  int i,y,y1,l1,z,l;
  double o,o1,o2,o3;
   x=pow(x1,2);
  for(i=0;;i++)
  {z=pow(10,i);
  if(x%z==x)break;}
if(i.isEven)
{
  y=pow(10,i/2);
  l=x%y;
  o=x/y;
  o=o-l/y;
  o3=o;
 for(int j=0;j<4;j++)
 {
   if(o%10==0)
     o=o/10;
   if(o%10!=0)
     break;
 }
  if(o+l==x1 ||o3+l==x1 )
     print('$x1');
  
}
  else

  {  y1=pow(10,i/2+0.5);
  l1=x%y1;
  o1=x/y1;
  o1=o1-l1/y1;
   o2=o1;
   for(int j=0;j<4;j++)
 {
   if(o1%10==0)
     o1=o1/10;
   else break;
 }
  if(o1+l1==x1 ||o2+l1==x1 )
    print('$x1');
  }
}
}



```


## Elixir

{{trans|Ruby}}

```elixir
defmodule KaprekarNumber do
  def check(n), do: check(n, 10)
  
  def check(1,_base), do: {"1", ""}
  def check(n, base) when rem(n*(n-1), (base-1)) != 0, do: false      # casting out nine
  def check(n, base) do
    square = Integer.to_string(n*n, base)
    check(n, base, square, 1, String.length(square)-1)
  end
  
  defp check(_, _, _, _, 0), do: false
  defp check(n, base, square, i, remainder) do
    {a, b} = String.split_at(square, i)
    if String.to_integer(b, base) == 0 do
      false
    else
      sum = String.to_integer(a, base) + String.to_integer(b, base)
      if n == sum, do: {a, b}, else: check(n, base, square, i+1, remainder-1)
    end
  end
end

Enum.each(1..9_999, fn n ->
  if result = KaprekarNumber.check(n) do
    {a, b} = result
    :io.fwrite "~6w  ~8s  ~s + ~s~n", [n, a<>b, a, b]
  end
end)

# Extra credit
count = Enum.reduce(1..999_999, 0, fn n,acc ->
  if KaprekarNumber.check(n), do: acc + 1, else: acc
end)
IO.puts "\n#{count} kaprekar numbers under 1,000,000"

# Extra extra credit
base = 17
IO.puts "\nbase #{base} kaprekar numbers under 1,000,000(base10)"
Enum.each(1..999_999, fn n ->
  if result = KaprekarNumber.check(n, base) do
    {a, b} = result
    :io.fwrite "~7w  ~5s  ~9s  ~s + ~s~n", [n, Integer.to_string(n,base), a<>b, a, b]
  end
end)
```


{{out}}

```txt

     1         1  1 +
     9        81  8 + 1
    45      2025  20 + 25
    55      3025  30 + 25
    99      9801  98 + 01
   297     88209  88 + 209
   703    494209  494 + 209
   999    998001  998 + 001
  2223   4941729  494 + 1729
  2728   7441984  744 + 1984
  4879  23804641  238 + 04641
  4950  24502500  2450 + 2500
  5050  25502500  2550 + 2500
  5292  28005264  28 + 005264
  7272  52881984  5288 + 1984
  7777  60481729  6048 + 1729
  9999  99980001  9998 + 0001

54 kaprekar numbers under 1,000,000

base 17 kaprekar numbers under 1,000,000(base10)
      1      1          1  1 +
     16      G         F1  F + 1
     64     3D        E2G  E + 2G
    225     D4       A52G  A5 + 2G
    288     GG       GF01  GF + 01
   1536    556     1B43B2  1B4 + 3B2
   3377    BBB     8093B2  809 + 3B2
   4912    GGG     GGF001  GGF + 001
   7425   18BD    24E166G  24E + 166G
   9280   1F1F    39B1B94  39B + 1B94
  16705   36DB    B992C42  B99 + 2C42
  20736   43CD   10DE32FG  10DE + 32FG
  30016   61EB   23593F92  2359 + 3F92
  36801   785D   351E433G  351E + 433G
  37440   7A96   37144382  3714 + 4382
  46081   967B   52G94382  52G9 + 4382
  46720   98B4   5575433G  5575 + 433G
  53505   AF26   6GA43F92  6GA4 + 3F92
  62785   CD44   9A5532FG  9A55 + 32FG
  66816   DA36   AEG42C42  AEG4 + 2C42
  74241   F1F2   D75F1B94  D75F + 1B94
  76096   F854   E1F5166G  E1F5 + 166G
  83520   GGGG   GGGF0001  GGGF + 0001
 266224  33334  A2C52A07G  A2C5 + 2A07G
```



## Erlang


```Erlang

-mode(compile).
-import(lists, [seq/2]).

kaprekar(1) -> true;
kaprekar(N) when N < 1 -> false;
kaprekar(N) ->
    Sq = N*N,
    if
        (N rem 9) =/= (Sq rem 9) -> false;
        true -> kaprekar(N, Sq, 10)
    end.

kaprekar(_, Sq,  M) when (Sq div M) =:= 0 -> false;
kaprekar(N, Sq, M) ->
    L = Sq div M,
    R = Sq rem M,
    if
        R =/= 0 andalso (L + R) =:= N -> true;
        true -> kaprekar(N, Sq, M * 10)
    end.

main(_) ->
    Numbers = [N || N <- seq(1, 9999), kaprekar(N)],
    io:format("The Kaprekar numbers < 10,000 are ~p~n", [Numbers]),

    CountTo1e6 = length(Numbers) + length([N || N <- seq(10001, 999999), kaprekar(N)]),
    io:format("There are ~p Kaprekar numbers < 1,000,000", [CountTo1e6]).

```

{{out}}

```txt

The Kaprekar numbers < 10,000 are [1,9,45,55,99,297,703,999,2223,2728,4879,
                                   4950,5050,5292,7272,7777,9999]
There are 54 Kaprekar numbers < 1,000,000

```



## Euler Math Toolbox

{{incorrect|Euler Math Toolbox|4950 and 5050 are missing because j=0 should not return in the loop.}}


```Euler Math Toolbox

>function map kaprekarp (n) ...
$  m=n*n;
$  p=10;
$  repeat
$    i=floor(m/p);
$    j=mod(m,p);
$    if j==0 then return 0; endif;
$    if i+j==n then return 1; endif;
$    p=p*10;
$    until p>m;
$  end;
$  return 0;
$endfunction
>nonzeros(kaprekarp(1:100000))
 [ 1  9  45  55  99  297  703  999  2223  2728  4879  5292  7272  7777
 9999  17344  22222  38962  77778  82656  95121  99999 ]

```



## F Sharp



```F Sharp

// Count digits in number
let digits x =
    let rec digits' p x =
        if 10.**p > x then p else digits' (p + 1.) x
    digits' 1. x


// Is n a Kaprekar number?
let isKaprekar n =
    // Reference: http://oeis.org/A006886
    // Positive numbers n such that n=q+r
    // And n^2=q*10^m+r,
    //  for some m >= 1,
    //  q>=0 and 0<=r<10^m,
    //  with n != 10^a, a>=1.
    let nSquared = n * n
    let a = float((digits n) - 1.)

    // Create a list of tuples from the nSquared digit splits
    [1. .. float (digits nSquared)]
    |> List.map (fun e ->
        // Splits the nSquared digits into 2 parts
        let x = 10.**e
        let q = float(int(Math.Floor (nSquared / x)))
        let r = nSquared - (q * x)
        (q, r))
    // Filter results based on rules
    |> List.exists (fun (q, r) ->
        q + r = n &&
        if a >= 1. then n % 10.**a <> 0. else true)


// List Kaprekar numbers from 1 to 10,000
[1 .. 10000]
|> List.filter (float >> isKaprekar)

```



## Factor

This solution is based on the following Haskell code: [https://dev.to/heikodudzus/comment/1cl6].

```factor
USING: io kernel lists lists.lazy locals math math.functions
math.ranges prettyprint sequences ;

:: kaprekar? ( n -- ? )
    n sq :> sqr
    1 lfrom
    [ 10 swap ^ ] lmap-lazy
    [ n > ] lfilter
    [ sqr swap mod n < ] lwhile
    list>array
    [ 1 - sqr n - swap mod zero? ] any?
    n 1 = or ;

1,000,000 [1,b] [ kaprekar? ] filter dup . length
"Count of Kaprekar numbers <= 1,000,000: " write .
```

{{out}}

```txt

V{
    1
    9
    45
    55
    99
    297
    703
    999
    2223
    2728
    4879
    4950
    5050
    5292
    7272
    7777
    9999
    ...
    851851
    857143
    961038
    994708
    999999
}
Count of Kaprekar numbers <= 1,000,000: 54

```



## Forth

This one takes the internal Forth variable BASE into account. Since Forth is perfectly suited to work with any base between 2 and 36, this works just fine.

```forth
: square ( n - n^2)   dup * ; 

\ Return nonzero if n is a Kaprekar number for tens, where tens is a 
\ nonzero power of base. 
: is-kaprekar? ( tens n n^2 - t)   rot /mod  over >r  + =  r> and ; 

\ If n is a Kaprekar number, return is the power of base for which it 
\ is Kaprekar.  If n is not a Kaprekar number, return zero. 
: kaprekar ( +n - +n1) 
    dup square >r 
    base @ swap 
    begin ( tens n) ( R: n^2) 
        over r@ < while 
            2dup r@ is-kaprekar? if 
                drop  r> drop  exit  then 
            swap  base @ *  swap 
    repeat 
    r> drop  1 = and ;

```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program Karpekar_Numbers
  implicit none
   
  integer, parameter :: i64 = selected_int_kind(18)
  integer :: count 
 
  call karpekar(10000_i64, .true.)
  write(*,*)
  call karpekar(1000000_i64, .false.)
  
contains

subroutine karpekar(n, printnums)

  integer(i64), intent(in) :: n
  logical, intent(in) :: printnums
  integer(i64) :: c, i, j, n1, n2
  character(19) :: str, s1, s2
  
  c = 0
  do i = 1, n
    write(str, "(i0)") i*i
    do j = 0, len_trim(str)-1
      s1 = str(1:j)
      s2 = str(j+1:len_trim(str)) 
      read(s1, "(i19)") n1
      read(s2, "(i19)") n2
      if(n2 == 0) cycle
      if(n1 + n2 == i) then
        c = c + 1
        if (printnums .eqv. .true.) write(*, "(i0)") i
        exit
      end if
    end do    
  end do
  if (printnums .eqv. .false.) write(*, "(i0)") c
end subroutine
end program
```

{{out}}

```txt
1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999

54
```


## FreeBASIC


```freebasic
' version 04-12-2016
' compile with: fbc -s console

' define true and false for older versions
#Ifndef TRUE
#Define FALSE 0
#Define TRUE Not FALSE
#EndIf

#Define max 1000000   ' maximum for number to be tested

Function kaprekar(n As ULong) As ULong

    If n = 1 Then Return TRUE

    Dim As ULong x, p1, p2
    Dim As ULongInt sq = CLngInt(n) * n
    Dim As String sq_str = Str(sq)
    Dim As ULong l = Len(sq_str)

    ' decrease the lenght l for every "0"
    ' at the end of the string
    For x = l -1 To 1 Step -1
        If sq_str[x] = Asc("0") Then
            l = l -1
        Else
            Exit For
        End If
    Next

    For x = 1 To l -1
        p2 = Val(Mid(sq_str, x +1))
        If p2 > n Then
            Continue For
        End If
        p1 = Val(Left(sq_str, x))
        If p1 > n Then Return FALSE  ' p1 > n leave
        If (p1 + p2) = n Then Return TRUE 
    Next

End Function

' ------=< MAIN >=------

Dim As ULong n, count

Print "Kaprekar numbers below 10000"

For n = 1 To max -1
    If kaprekar(n) = TRUE Then
        count = count + 1
        If n < 10000 Then
            Print count, n
        End If
    End If
Next

Print
Print count;" numbers below "; Str(max);" are Kaprekar numbers"

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Kaprekar numbers below 10000
1             1
2             9
3             45
4             55
5             99
6             297
7             703
8             999
9             2223
10            2728
11            4879
12            4950
13            5050
14            5292
15            7272
16            7777
17            9999

54 numbers below 1000000 are Kaprekar numbers
```



## GAP


```gap
IsKaprekar := function(n)
	local a, b, p, q;
	if n = 1 then
		return true;
	fi;
	q := n*n;
	p := 10;
	while p < q do
		a := RemInt(q, p);
		b := QuoInt(q, p);
		if a > 0 and a + b = n then
			return true;
		fi;
		p := p*10;
	od;
	return false;
end;

Filtered([1 .. 10000], IsKaprekar);
# [ 1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 
#   7777, 9999 ]

Size(last);
# 17

Filtered([1 .. 1000000], IsKaprekar);
# [ 1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 
#   7777, 9999, 17344, 22222, 38962, 77778, 82656, 95121, 99999, 142857, 
#   148149, 181819, 187110, 208495, 318682, 329967, 351352, 356643, 390313, 
#   461539, 466830, 499500, 500500, 533170, 538461, 609687, 627615, 643357, 
#   648648, 670033, 681318, 791505, 812890, 818181, 851851, 857143, 961038, 
#   994708, 999999 ]

Size(last);
# 54


IsKaprekarAndHow := function(n, base)
	local a, b, p, q;
	if n = 1 then
		return true;
	fi;
	q := n*n;
	p := base;
	while p < q do
		a := RemInt(q, p);
		b := QuoInt(q, p);
		if a > 0 and a + b = n then
			return [a, b];
		fi;
		p := p*base;
	od;
	return false;
end;

IntegerToBaseRep := function(n, base)
	local s, digit;
	if base > 36 then
		return fail;
	elif n = 0 then
		return "0";
	else
		s := "";
		digit := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		while n <> 0 do
			Add(s, digit[RemInt(n, base) + 1]);
			n := QuoInt(n, base);
		od;
		return Reversed(s);
	fi;
end;

PrintIfKaprekar := function(n, base)
	local v;
	v := IsKaprekarAndHow(n, base);
	if IsList(v) then
		Print(n, "(10) or in base ", base, ", ",
			IntegerToBaseRep(n, base), "^2 = ",
			IntegerToBaseRep(n^2, base), " and ",
			IntegerToBaseRep(v[2], base), " + ",
			IntegerToBaseRep(v[1], base), " = ",
			IntegerToBaseRep(n, base), "\n");
	fi;
	return fail;
end;

# In base 17...
Perform([1 .. 1000000], n -> PrintIfKaprekar(n, 17));
# 16(10) or in base 17, G^2 = F1 and F + 1 = G
# 64(10) or in base 17, 3D^2 = E2G and E + 2G = 3D
# 225(10) or in base 17, D4^2 = A52G and A5 + 2G = D4
# 288(10) or in base 17, GG^2 = GF01 and GF + 1 = GG
# 1536(10) or in base 17, 556^2 = 1B43B2 and 1B4 + 3B2 = 556
# 3377(10) or in base 17, BBB^2 = 8093B2 and 809 + 3B2 = BBB
# 4912(10) or in base 17, GGG^2 = GGF001 and GGF + 1 = GGG
# 7425(10) or in base 17, 18BD^2 = 24E166G and 24E + 166G = 18BD
# 9280(10) or in base 17, 1F1F^2 = 39B1B94 and 39B + 1B94 = 1F1F
# 16705(10) or in base 17, 36DB^2 = B992C42 and B99 + 2C42 = 36DB
# 20736(10) or in base 17, 43CD^2 = 10DE32FG and 10DE + 32FG = 43CD
# 30016(10) or in base 17, 61EB^2 = 23593F92 and 2359 + 3F92 = 61EB
# 36801(10) or in base 17, 785D^2 = 351E433G and 351E + 433G = 785D
# 37440(10) or in base 17, 7A96^2 = 37144382 and 3714 + 4382 = 7A96
# 46081(10) or in base 17, 967B^2 = 52G94382 and 52G9 + 4382 = 967B
# 46720(10) or in base 17, 98B4^2 = 5575433G and 5575 + 433G = 98B4
# 53505(10) or in base 17, AF26^2 = 6GA43F92 and 6GA4 + 3F92 = AF26
# 62785(10) or in base 17, CD44^2 = 9A5532FG and 9A55 + 32FG = CD44
# 66816(10) or in base 17, DA36^2 = AEG42C42 and AEG4 + 2C42 = DA36
# 74241(10) or in base 17, F1F2^2 = D75F1B94 and D75F + 1B94 = F1F2
# 76096(10) or in base 17, F854^2 = E1F5166G and E1F5 + 166G = F854
# 83520(10) or in base 17, GGGG^2 = GGGF0001 and GGGF + 1 = GGGG
# 266224(10) or in base 17, 33334^2 = A2C52A07G and A2C5 + 2A07G = 33334
```



## Go

Using the Ada interpretation of 1000000 base 17:

```go
package main

import (
    "fmt"
    "strconv"
)

func kaprekar(n uint64, base uint64) (bool, int) {
    order := 0
    if n == 1 {
        return true, -1
    }

    nn, power := n*n, uint64(1)
    for power <= nn {
        power *= base
        order++
    }

    power /= base
    order--
    for ; power > 1; power /= base {
        q, r := nn/power, nn%power
        if q >= n {
            return false, -1
        }

        if q+r == n {
            return true, order
        }

        order--
    }

    return false, -1
}

func main() {
    max := uint64(10000)
    fmt.Printf("Kaprekar numbers < %d:\n", max)
    for m := uint64(0); m < max; m++ {
        if is, _ := kaprekar(m, 10); is {
            fmt.Println("  ", m)
        }
    }

    // extra credit
    max = 1e6
    var count int
    for m := uint64(0); m < max; m++ {
        if is, _ := kaprekar(m, 10); is {
            count++
        }
    }
    fmt.Printf("\nThere are %d Kaprekar numbers < %d.\n", count, max)

    // extra extra credit
    const base = 17
    maxB := "1000000"
    fmt.Printf("\nKaprekar numbers between 1 and %s(base %d):\n", maxB, base)
    max, _ = strconv.ParseUint(maxB, base, 64)
    fmt.Printf("\n Base 10  Base %d        Square       Split\n", base)
    for m := uint64(2); m < max; m++ {
        is, pos := kaprekar(m, base)
        if !is {
            continue
        }
        sq := strconv.FormatUint(m*m, base)
        str := strconv.FormatUint(m, base)
        split := len(sq)-pos
        fmt.Printf("%8d  %7s  %12s  %6s + %s\n", m,
            str, sq, sq[:split], sq[split:]) // optional extra extra credit
    }
}
```

{{out}}

```txt
Kaprekar numbers < 10000:
   1
   9
   45
   55
   99
   297
   703
   999
   2223
   2728
   4879
   4950
   5050
   5292
   7272
   7777
   9999

There are 54 Kaprekar numbers < 1000000.

Kaprekar numbers between 1 and 1000000(base 17):

 Base 10  Base 17        Square       Split
      16        g            f1       f + 1
      64       3d           e2g       e + 2g
     225       d4          a52g      a5 + 2g
     288       gg          gf01      gf + 01
    1536      556        1b43b2     1b4 + 3b2
    3377      bbb        8093b2     809 + 3b2
    4912      ggg        ggf001     ggf + 001
    7425     18bd       24e166g     24e + 166g
    9280     1f1f       39b1b94     39b + 1b94
   16705     36db       b992c42     b99 + 2c42
   20736     43cd      10de32fg    10de + 32fg
   30016     61eb      23593f92    2359 + 3f92
   36801     785d      351e433g    351e + 433g
   37440     7a96      37144382    3714 + 4382
   46081     967b      52g94382    52g9 + 4382
   46720     98b4      5575433g    5575 + 433g
   53505     af26      6ga43f92    6ga4 + 3f92
   62785     cd44      9a5532fg    9a55 + 32fg
   66816     da36      aeg42c42    aeg4 + 2c42
   74241     f1f2      d75f1b94    d75f + 1b94
   76096     f854      e1f5166g    e1f5 + 166g
   83520     gggg      gggf0001    gggf + 0001
  266224    33334     a2c52a07g    a2c5 + 2a07g
 1153633    ddddd    b3d5e2a07g   b3d5e + 2a07g
 1334529    fgacc    f0540f1a78    f054 + 0f1a78
 1419856    ggggg    ggggf00001   ggggf + 00001
 1787968   146fca   19g4c12dg7f   19g4c + 12dg7f
 3122497   236985   4e3be1f95d8   4e3be + 1f95d8
 3773952   2b32b3   711cb2420f9   711cb + 2420f9
 4243968   2gde03   8fegb27eg09   8fegb + 27eg09
 5108481   3a2d6f   cg10b2e3c64   cg10b + 2e3c64
 5561920   3fa16d   f5eae3043cg   f5eae + 3043cg
 6031936   443ccd  110dde332ffg  110dde + 332ffg
 6896449   4e9c28  16a10c37gb1d  16a10c + 37gb1d
 7435233   54067b  1a72g93aa382  1a72g9 + 3aa382
 8017920   5aggb6  1ef1d43d1ef2  1ef1d4 + 3d1ef2
 9223201   687534  2835c5403g7g  2835c5 + 403g7g
 9805888   6f6f6g  2dbe3f41c131  2dbe3f + 41c131
11140416   7e692a  3a997c43dgbf  3a997c + 43dgbf
11209185   7f391e  3b58d543f059  3b58d5 + 43f059
12928384   91d7f3  4ef79b43f059  4ef79b + 43f059
12997153   92a7e7  4fd82943dgbf  4fd829 + 43dgbf
14331681   a1a1a1  5gf07041c131  5gf070 + 41c131
14914368   a89bdd  685c5e403g7g  685c5e + 403g7g
16119649   b6005b  79f2793d1ef2  79f279 + 3d1ef2
16702336   bcga96  8267143aa382  826714 + 3aa382
17241120   c274e9  8b7acd37gb1d  8b7acd + 37gb1d
18105633   ccd444  99a555332ffg  99a555 + 332ffg
18575649   d16fa4  a12be53043cg  a12be5 + 3043cg
19029088   d6e3a2  a9a83f2e3c64  a9a83f + 2e3c64
19893601   e032ge  b953g527eg09  b953g5 + 27eg09
20363617   e5de5e  c1bd752420f9  c1bd75 + 2420f9
21015072   eda78c  cf11c41f95d8  cf11c4 + 1f95d8
22349601   fca147  e9d1d912dg7f  e9d1d9 + 12dg7f
22803040   g10645  f2fcde0f1a78  f2fcde + 0f1a78
24137568   gggggg  gggggf000001  gggggf + 000001
```



## Haskell


```Haskell
import Text.Printf (printf)
import Data.Maybe (mapMaybe)
import Numeric (showIntAtBase)

kaprekars :: Integer -> Integer -> [(Integer, Integer, Integer)]
kaprekars base top = (1, 0, 1) : mapMaybe kap (filter res [2 .. top])
  where
    res x = x * (x - 1) `mod` (base - 1) == 0
    kap n =
      getSplit $
      takeWhile (<= nn) $ dropWhile (< n) $ iterate (* toInteger base) 1
      where
        nn = n * n
        getSplit [] = Nothing
        getSplit (p:ps)
          | p == n = Nothing
          | q + r == n = Just (n, q, r)
          | r > n = Nothing
          | otherwise = getSplit ps
          where
            (q, r) = nn `divMod` p

heading :: Int -> String
heading = printf (h ++ d)
  where
    h = " #    Value (base 10)         Sum (base %d)             Square\n"
    d = " -    ---------------         -------------             ------"

printKap :: Integer -> (Int, (Integer, Integer, Integer)) -> String
printKap b (i, (n, l, r)) =
  printf "%2d %13s %26s %16s" i (show n) ss (base b (n * n))
  where
    ss = base b n ++ " = " ++ base b l ++ " + " ++ base b r
    base b n = showIntAtBase b (("0123456789" ++ ['a' .. 'z']) !!) n ""

main :: IO ()
main = do
  putStrLn $ heading 10
  mapM_ (putStrLn . printKap 10) $ zip [1 ..] (kaprekars 10 1000000)
  putStrLn ""
  putStrLn $ heading 17
  mapM_ (putStrLn . printKap 17) $ zip [1 ..] (kaprekars 17 1000000)
```

{{out}}

```txt
 #    Value (base 10)         Sum (base 10)             Square
 -    ---------------         -------------             ------

 1             1                       1 = 1 + 0                1
 2             9                       9 = 8 + 1               81
 3            45                    45 = 20 + 25             2025
 4            55                    55 = 30 + 25             3025
 5            99                     99 = 98 + 1             9801
 6           297                  297 = 88 + 209            88209
 7           703                 703 = 494 + 209           494209
 8           999                   999 = 998 + 1           998001
 9          2223               2223 = 494 + 1729          4941729
10          2728               2728 = 744 + 1984          7441984
11          4879               4879 = 238 + 4641         23804641
12          4950              4950 = 2450 + 2500         24502500
13          5050              5050 = 2550 + 2500         25502500
14          5292                5292 = 28 + 5264         28005264
15          7272              7272 = 5288 + 1984         52881984
16          7777              7777 = 6048 + 1729         60481729
17          9999                 9999 = 9998 + 1         99980001
18         17344            17344 = 3008 + 14336        300814336
19         22222            22222 = 4938 + 17284        493817284
20         38962            38962 = 1518 + 37444       1518037444
21         77778           77778 = 60494 + 17284       6049417284
22         82656           82656 = 68320 + 14336       6832014336
23         95121            95121 = 90480 + 4641       9048004641
24         99999               99999 = 99998 + 1       9999800001
                      .
                      .
                      .
51        857143        857143 = 734694 + 122449     734694122449
52        961038         961038 = 923594 + 37444     923594037444
53        994708          994708 = 989444 + 5264     989444005264
54        999999             999999 = 999998 + 1     999998000001

 #    Value (base 10)         Sum (base 17)             Square
 -    ---------------         -------------             ------

 1             1                       1 = 1 + 0                1
 2            16                       g = f + 1               f1
 3            64                     3d = e + 2g              e2g
 4           225                    d4 = a5 + 2g             a52g
 5           288                     gg = gf + 1             gf01
                      .
                      .
                      .
21         74241              f1f2 = d75f + 1b94         d75f1b94
22         76096              f854 = e1f5 + 166g         e1f5166g
23         83520                 gggg = gggf + 1         gggf0001
24        266224            33334 = a2c5 + 2a07g        a2c52a07g
```


Generating Kaprekar numbers by factorizing b^n - 1:

```haskell
import Control.Monad (foldM, join)
import Data.List (group, nub, sort)

primes :: [Int]
primes = 2 : 3 : filter isPrime (scanl (+) 5 $ cycle [2, 4])
  where
    isPrime x = all ((0 /=) . mod x) $ takeWhile ((<= x) . join (*)) primes

unitFactors :: Int -> [Int]
unitFactors n = map product $ group $ f n $ takeWhile ((<= n) . join (*)) primes
  where
    f 1 [] = []
    f n [] = [n]
    f n (p:ps)
      | n `mod` p == 0 = p : f (n `div` p) (p : ps)
      | otherwise = f n ps

-- all factors x of n where x and n/x are coprime
factors :: Int -> [Int]
factors = foldM f 1 . unitFactors
  where
    f x a = [x, x * a]

-- modulo multiplication inverse: returns a where a x + b y == 1
inverse :: Int -> Int -> Int
inverse x y =
  if a < 0
    then a + y
    else a
  where
    (a, b) = extEuclid x y
    extEuclid _ 0 = (1, 0)
    extEuclid x y = (t, s - q * t)
      where
        (s, t) = extEuclid y r
        (q, r) = x `divMod` y

kaprekars :: Int -> Int -> [Int]
kaprekars base top =
  nub . sort . concatMap kaps $
  takeWhile (<= top * top `div` base ^ 2) $ (\x -> base ^ x - 1) <$> [1 ..]
  where
    kaps pb = filter (<= top) $ f <$> factors pb
      where
        f x
          | x == pb = pb
          | otherwise = x * inverse x (pb `div` x)

main :: IO ()
main = mapM_ print $ kaprekars 10 10000000
```


=={{header|Icon}} and {{header|Unicon}}==


```Icon
procedure is_kaprekar(n)  #: return n if n is a kaprekar number
if ( n = 1 ) |
   ( n^2 ? ( n = move(1 to *&subject-1) + (0 ~= tab(0)) | fail )) then
   return n 
end

procedure main()
every write(is_kaprekar(1 to 10000))                        # primary goal
every (count := 0, is_kaprekar(1 to 999999), count +:= 1)   # stretch goal
write ("Number of Kaprekar numbers less than 1000000 is ", count)
end
```


{{out}}

```txt
1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999
Number of Kaprekar numbers less than 1000000 is 54
```



## J

'''Solution:'''

```j
kapbase=: 0,. [ ^ 1 + [: i. 1 + [ <.@^.  >.&1
isKap=: 1 e. ] ((0 < {:"1@]) *. [ = +/"1@]) kapbase #: *:@]
```


Example use:


```j
   I. 10 isKap"0 i.1e6
1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999 17344 22222 38962 77778 82656 95121 99999 142857 148149 181819 187110 208495 318682 329967 351352 356643 390313 461539 466830 499500 500500 533170 538461 609687 627615 643357 648648 670033 681318 791505 812890 818181 851851 857143 961038 994708 999999
```


"Extra credit":  (text representing numbers left in boxes for alignment purposes)

```j

   ]K17=: I. 17 isKap"0 i.1e6
1 16 64 225 288 1536 3377 4912 7425 9280 16705 20736 30016 36801 37440 46081 46720 53505 62785 66816 74241 76096 83520 266224
   base=: [: (] u:@+ 39 * 57 < ]) 48 + #.inv 
   17 ([ base&.> ],*:@],] (] {:@,@#~ (0 < {:"1@]) *. [ = +/"1@]) kapbase #: *:@])"0 x:K17
┌─────┬─────────┬─────┐
│1    │1        │1    │
├─────┼─────────┼─────┤
│g    │f1       │1    │
├─────┼─────────┼─────┤
│3d   │e2g      │2g   │
├─────┼─────────┼─────┤
│d4   │a52g     │2g   │
├─────┼─────────┼─────┤
│gg   │gf01     │1    │
├─────┼─────────┼─────┤
│556  │1b43b2   │3b2  │
├─────┼─────────┼─────┤
│bbb  │8093b2   │3b2  │
├─────┼─────────┼─────┤
│ggg  │ggf001   │1    │
├─────┼─────────┼─────┤
│18bd │24e166g  │166g │
├─────┼─────────┼─────┤
│1f1f │39b1b94  │1b94 │
├─────┼─────────┼─────┤
│36db │b992c42  │2c42 │
├─────┼─────────┼─────┤
│43cd │10de32fg │32fg │
├─────┼─────────┼─────┤
│61eb │23593f92 │3f92 │
├─────┼─────────┼─────┤
│785d │351e433g │433g │
├─────┼─────────┼─────┤
│7a96 │37144382 │4382 │
├─────┼─────────┼─────┤
│967b │52g94382 │4382 │
├─────┼─────────┼─────┤
│98b4 │5575433g │433g │
├─────┼─────────┼─────┤
│af26 │6ga43f92 │3f92 │
├─────┼─────────┼─────┤
│cd44 │9a5532fg │32fg │
├─────┼─────────┼─────┤
│da36 │aeg42c42 │2c42 │
├─────┼─────────┼─────┤
│f1f2 │d75f1b94 │1b94 │
├─────┼─────────┼─────┤
│f854 │e1f5166g │166g │
├─────┼─────────┼─────┤
│gggg │gggf0001 │1    │
├─────┼─────────┼─────┤
│33334│a2c52a07g│2a07g│
└─────┴─────────┴─────┘
```


The fastest times can be obtained by two optimizations: first, partitions with over half of the digits on the left (i.e. more than 3 for a 5-digit number) will not be considered because the left half is mathematically guaranteed to be greater than the original number in this case. Second, the numbers are computed in groups corresponding to the number of digits in their squares to allow isKap to be computed at full rank. Note that this method excludes 1, so it must be added manually to the list of solutions.

```j
   kapbase=: 0,.10 ^ [: (<.+i.@>.)@(-:&.<:) 10 <.@^.  >.&1
   isKapGroup=: [: +./"1 (((0 < {:"1@]) *. [ = +/"1@]) (kapbase@{: #:"2 0 ])@:*:)
   6!:2 'a=.1, I. ([:; (<@isKapGroup/.~ 10<.@^.*:)) i.1e6'
12.3963
   #a
54
```


'''Alternative solution:'''
The following is a more naive, mechanical solution

```j
splitNum=: {. ,&(_&".) }.
allSplits=: (i.&.<:@# splitNum"0 1 ])@":
sumValidSplits=: +/"1@:(#~ 0 -.@e."1 ])
filterKaprekar=: #~ ] e."0 1 [: sumValidSplits@allSplits"0 *:
```


Example use:

```j
   filterKaprekar i. 10000
0 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999
   #filterKaprekar i. 1e6
54
```



## Java


```java
public class Kaprekar {
    private static String[] splitAt(String str, int idx){
        String[] ans = new String[2];
        ans[0] = str.substring(0, idx);
        if(ans[0].equals("")) ans[0] = "0"; //parsing "" throws an exception
        ans[1] = str.substring(idx);
        return ans;
    }
        
    public static void main(String[] args){
        int count = 0;
        int base = (args.length > 0) ? Integer.parseInt(args[0]) : 10;
        for(long i = 1; i <= 1000000; i++){
            String sqrStr = Long.toString(i * i, base);
            for(int j = 0; j < sqrStr.length() / 2 + 1; j++){
                String[] parts = splitAt(sqrStr, j);
                long firstNum = Long.parseLong(parts[0], base);
                long secNum = Long.parseLong(parts[1], base);
                //if the right part is all zeroes, then it will be forever, so break
                if(secNum == 0) break;
                if(firstNum + secNum == i){
                    System.out.println(i + "\t" + Long.toString(i, base) +
                            "\t" + sqrStr + "\t" + parts[0] + " + " + parts[1]);
                    count++;
                    break;
                }
            }
        }
        System.out.println(count + " Kaprekar numbers < 1000000 (base 10) in base "+base);
    }
}
```

{{out}} (base 10, shortened):

```txt
1	1	1	0 + 1
9	9	81	8 + 1
45	45	2025	20 + 25
55	55	3025	30 + 25
99	99	9801	98 + 01
297	297	88209	88 + 209
703	703	494209	494 + 209
999	999	998001	998 + 001
2223	2223	4941729	494 + 1729
2728	2728	7441984	744 + 1984
4879	4879	23804641	238 + 04641
4950	4950	24502500	2450 + 2500
5050	5050	25502500	2550 + 2500
5292	5292	28005264	28 + 005264
7272	7272	52881984	5288 + 1984
7777	7777	60481729	6048 + 1729
9999	9999	99980001	9998 + 0001
...
818181	818181	669420148761	669420 + 148761
851851	851851	725650126201	725650 + 126201
857143	857143	734694122449	734694 + 122449
961038	961038	923594037444	923594 + 037444
994708	994708	989444005264	989444 + 005264
999999	999999	999998000001	999998 + 000001
54 Kaprekar numbers < 1000000 (base 10) in base 10
```

{{out}} (base 17, shortened):

```txt
1	1	1	0 + 1
16	g	f1	f + 1
64	3d	e2g	e + 2g
225	d4	a52g	a5 + 2g
288	gg	gf01	gf + 01
1536	556	1b43b2	1b4 + 3b2
3377	bbb	8093b2	809 + 3b2
4912	ggg	ggf001	ggf + 001
7425	18bd	24e166g	24e + 166g
9280	1f1f	39b1b94	39b + 1b94
...
76096	f854	e1f5166g	e1f5 + 166g
83520	gggg	gggf0001	gggf + 0001
266224	33334	a2c52a07g	a2c5 + 2a07g
24 Kaprekar numbers < 1000000 (base 10) in base 17
```


## JavaScript

'''This string version'''

```JavaScript
function isKaprekar( n, bs ) {
	if ( n < 1 ) return false
	if ( n == 1 ) return true
	bs = bs || 10
	var s = (n * n).toString(bs)
	for (var i=1, e=s.length; i<e; i+=1) {
		var a = parseInt(s.substr(0, i), bs)
		var b = parseInt(s.substr(i), bs)
		if (b && a + b == n) return true
	}
	return false
}
```

'''or this numeric version'''

```JavaScript
function isKaprekar( n, bs ) {
	if ( n < 1 ) return false
	if ( n == 1 ) return true
	bs = bs || 10
	for (var a=n*n, b=0, s=1; a; s*=bs) {
		b += a%bs*s
		a = Math.floor(a/bs)
		if (b && a + b == n) return true
	}
	return false
}
```

'''with'''

```JavaScript
function kaprekar( s, e, bs, pbs ) {
	bs = bs || 10; pbs = pbs || 10
	const toString = n => n.toString(pbs).toUpperCase()
	document.write('start:',toString(s), ' end:',toString(e), ' base:',bs, ' printBase:',pbs, '
' )
	for (var k=0, n=s; n<=e; n+=1) if (isKaprekar(n, bs)) k+=1, document.write(toString(n), ' ') 
	document.write('
found ', k, ' numbers

')
}

kaprekar( 1, 99 )
kaprekar( 1, 255, 16)
kaprekar( 1, 255, 16, 16)
kaprekar( 1, 288, 17, 17)
```

{{out}}

```txt

start:1 end:99 base:10 printBase:10
1 9 45 55 99
found 5 numbers

start:1 end:255 base:16 printBase:10
1 6 10 15 51 85 91 120 136 171 205 255
found 12 numbers

start:1 end:FF base:16 printBase:16
1 6 A F 33 55 5B 78 88 AB CD FF
found 12 numbers

start:1 end:GG base:17 printBase:17
1 G 3D D4 GG
found 5 numbers

```



## jq

{{Works with|jq|1.4}}

```jq
# Is the input integer a Kaprekar integer?
def is_kaprekar:
    # The helper function acts like a loop:
    # input is [n, number, str]
    # where n is the position to be considered next,
    # number is the integer under consideration,
    # and str is the string representing number*number
    def _try:  
      .[0] as $n | .[1] as $number | .[2] as $str
      | if $n >= ($str|length) then null
        else   ($str[0:$n] | tonumber) as $left
             | ($str[$n:]  | tonumber) as $right
             | if $left > $number then null
               elif $right == 0 then null
               elif ($left + $right) == $number then $n
               else [($n + 1), $number, $str] | _try
               end
        end;
    . as $in
    | if . == 1 then true
      elif . < 1 then false
      else null != ([1, $in, ($in*$in|tostring)] | _try)
      end ;

# Useful for counting how many times the condition is satisfied:
def count(generator; condition):
  reduce generator as $i (0; if ($i|condition ) then .+1 else . end);

def task:
  [ range(1;10000) | select( is_kaprekar ) ],
  count( range(1;1000000); is_kaprekar )
;
```

{{Out}}

```sh
$ jq -n -c -f is_kaprekar.jq
[1,9,45,55,99,297,703,999,2223,2728,4879,4950,5050,5292,7272,7777,9999]
54
```



## Julia

{{works with|Julia|1.2}}


```julia
function iskaprekar(n::Integer)
    n == 1 && return true
    test(a, b) = n == a + b && b ≠ 0
    str = string(n^2)
    any(test(parse(Int, str[1:i]), parse(Int, str[i+1:end])) for i = 1:length(str)-1)
end

@show filter(iskaprekar, 1:10000)
@show count(iskaprekar, 1:10000)
```


{{out}}

```txt
filter(iskaprekar, 1:10000) = [1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
count(iskaprekar, 1:10000) = 17
```



## Kotlin

{{trans|Java}}

```scala
import java.lang.Long.parseLong
import java.lang.Long.toString

fun String.splitAt(idx: Int): Array<String> {
    val ans = arrayOf(substring(0, idx), substring(idx))
    if (ans.first() == "") ans[0] = "0" // parsing "" throws an exception
    return ans
}

fun Long.getKaprekarParts(sqrStr:  String, base: Int): Array<String>? {
    for (j in 0..sqrStr.length / 2) {
        val parts = sqrStr.splitAt(j)
        val (first, second) = parts.map { parseLong(it, base) }

        // if the right part is all zeroes, then it will be forever, so break
        if (second == 0L) return null
        if (first + second == this) return parts
    }
    return null
}

fun main(args: Array<String>) {
    val base = if (args.isNotEmpty()) args[0].toInt() else 10
    var count = 0
    val max = 1000000L
    for (i in 1..max) {
        val s = toString(i * i, base)
        val p = i.getKaprekarParts(s, base)
        if (p != null) {
            println("%6d\t%6s\t%12s\t%7s + %7s".format(i, toString(i, base), s, p[0], p[1]))
            count++
        }
    }
    println("$count Kaprekar numbers < $max (base 10) in base $base")
}
```

{{out}}

```txt
     1	     1	           1	      0 +       1
     9	     9	          81	      8 +       1
    45	    45	        2025	     20 +      25
    55	    55	        3025	     30 +      25
    99	    99	        9801	     98 +      01
   297	   297	       88209	     88 +     209
   703	   703	      494209	    494 +     209
   999	   999	      998001	    998 +     001
  2223	  2223	     4941729	    494 +    1729
  2728	  2728	     7441984	    744 +    1984
  4879	  4879	    23804641	    238 +   04641
  4950	  4950	    24502500	   2450 +    2500
  5050	  5050	    25502500	   2550 +    2500
  5292	  5292	    28005264	     28 +  005264
  7272	  7272	    52881984	   5288 +    1984
  7777	  7777	    60481729	   6048 +    1729
  9999	  9999	    99980001	   9998 +    0001
 17344	 17344	   300814336	   3008 +   14336
 22222	 22222	   493817284	   4938 +   17284
 38962	 38962	  1518037444	   1518 +  037444
 77778	 77778	  6049417284	  60494 +   17284
 82656	 82656	  6832014336	  68320 +   14336
 95121	 95121	  9048004641	  90480 +   04641
 99999	 99999	  9999800001	  99998 +   00001
142857	142857	 20408122449	  20408 +  122449
148149	148149	 21948126201	  21948 +  126201
181819	181819	 33058148761	  33058 +  148761
187110	187110	 35010152100	  35010 +  152100
208495	208495	 43470165025	  43470 +  165025
318682	318682	101558217124	 101558 +  217124
329967	329967	108878221089	 108878 +  221089
351352	351352	123448227904	 123448 +  227904
356643	356643	127194229449	 127194 +  229449
390313	390313	152344237969	 152344 +  237969
461539	461539	213018248521	 213018 +  248521
466830	466830	217930248900	 217930 +  248900
499500	499500	249500250000	 249500 +  250000
500500	500500	250500250000	 250500 +  250000
533170	533170	284270248900	 284270 +  248900
538461	538461	289940248521	 289940 +  248521
609687	609687	371718237969	 371718 +  237969
627615	627615	393900588225	  39390 + 0588225
643357	643357	413908229449	 413908 +  229449
648648	648648	420744227904	 420744 +  227904
670033	670033	448944221089	 448944 +  221089
681318	681318	464194217124	 464194 +  217124
791505	791505	626480165025	 626480 +  165025
812890	812890	660790152100	 660790 +  152100
818181	818181	669420148761	 669420 +  148761
851851	851851	725650126201	 725650 +  126201
857143	857143	734694122449	 734694 +  122449
961038	961038	923594037444	 923594 +  037444
994708	994708	989444005264	 989444 +  005264
999999	999999	999998000001	 999998 +  000001
54 Kaprekar numbers < 1000000 (base 10) in base 10
```



## Liberty BASIC


```lb

 
 For i = 1 To 10000  '1000000 - Changing to one million takes a long time to complete!!!!
    Kaprekar = isKaprekar(i)
    If Kaprekar Then numKaprekar = (numKaprekar + 1) : Print Kaprekar
Next i

Print numKaprekar
End

Function isKaprekar(num)
    If num < 1 Then isKaprekar = 0 : Exit Function
    If num = 1 Then isKaprekar = num : Exit Function
    squarenum$ = str$(num ^ 2)
    For i = 1 To Len(squarenum$)
        If Val(Mid$(squarenum$, i)) = 0 Then isKaprekar = 0 : Exit Function
        If (Val(Left$(squarenum$, (i - 1))) + Val(Mid$(squarenum$, i)) = num) Then isKaprekar = num : Exit Function
    Next i
End Function 

```



## Lua


```Lua
-- Return length of an integer without string conversion
function numLength (n)
    local length = 0
    repeat
        n = math.floor(n / 10)
        length = length + 1
    until n == 0
    return length
end

-- Return a boolean indicating whether n is a Kaprekar number
function isKaprekar (n)
    if n == 1 then return true end
    local nSquared, a, b = n * n
    for splitPoint = 1, numLength(nSquared) - 1 do
        a = math.floor(nSquared / 10^splitPoint)
        b = nSquared % 10^splitPoint
        if a > 0 and b > 0 and a + b == n then return true end
    end
    return false
end

-- Main task
for n = 1, 10^4 do
    if isKaprekar(n) then io.write(n .. " ") end
end
 
-- Extra credit
local count = 0
for n = 1, 10^6 do
    if isKaprekar(n) then count = count + 1 end
end
print("\nThere are " .. count .. " Kaprekar numbers under one million.")
```

{{out}}

```txt
1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999
There are 54 Kaprekar numbers under one million.
```




## Maple

For a number x to be Kaprekar, it must have x^2 congruent to x mod 9. 
Which is only achievable when x has remainder 1 or 0 mod 9. So we only check for these cases.

```Maple

isKaprekar := proc(n::posint) 
local holder, square, num_of_digits, k, left, right;
holder := true;
if n = 1 then 
 holder := true; 
else 
 holder := false; 
 square := n^2; 
 num_of_digits := length(n^2); 
 for k to num_of_digits do left := floor(square/10^k); 
     right := irem(square, 10^k); 
     if left + right = n and right <> 0 then 
        holder := true; 
     break; 
     end if; 
 end do; 
end if;
return holder; 
end proc;

showKaprekar := n -> select(isKaprekar, select(x -> irem(x, 9) = 1 or irem(x, 9) = 0, [seq(1 .. n - 1)]));

countKaprekar := n -> nops(showKaprekar(n));

showKaprekar(10000);
countKaprekar(1000000);
```


{{out}}

```txt
[1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
54
```



## Mathematica


```Mathematica
KaprekaQ[1] = True;
KaprekaQ[n_Integer] :=  Block[{data = IntegerDigits[n^2], last = False, i = 1},
  While[i < Length[data] && FromDigits[data[[i + 1 ;;]]] =!= 0 &&  Not[last],
   last = FromDigits[data[[;; i]]] + FromDigits[data[[i + 1 ;;]]] == n;
   i++]; last];

Select[Range[10000], KaprekaQ]

Length[Select[Range[1000000], KaprekaQ]]
```


{{out}}

```txt
{1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999}

54
```



## Maxima


```maxima
kaprekarp(n) := block(
   [p, q, a, b],
   if n = 1 then true else (
      q: n * n,
      p: 10,
      catch(
         while p < q do (
            [a, b]: divide(q, p),
            if b > 0 and a + b = n then throw(true),
            p: 10 * p
         ),
         false
      )
   )
)$

sublist(makelist(i, i, 1, 10^6), kaprekarp);
[1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999,
17344, 22222, 38962, 77778, 82656, 95121, 99999, 142857, 148149, 181819, 187110, 208495,
318682, 329967, 351352, 356643, 390313, 461539, 466830, 499500, 500500, 533170, 538461,
609687, 627615, 643357, 648648, 670033, 681318, 791505, 812890, 818181, 851851, 857143,
961038, 994708, 999999]

length(%);
54
```



## ML

=
## mLite
=

```ocaml
local 
  val base = 10;
  fun kaprekar
        (num, numSquared, numDiv, numRem, power) where (base ^ power >= numSquared) = ()

    |   (num, numSquared, numDiv, numRem, power) where ((numDiv = 0) or (numRem = 0))= 
          kaprekar (num, numSquared, numSquared div (base ^ power ), numSquared rem (base ^ power), power + 1)

    |   (num, numSquared, numDiv, numRem, power) = 
          if ((numDiv + numRem) = num) then
            num
          else
            kaprekar (num, numSquared, numSquared div (base ^ power ), numSquared rem (base ^ power), power + 1)

    |   num = 
          if (num = 1) then
            num
          else
            kaprekar (num, num * num, (num * num) div base, (num * num) rem base, 1)
        
in
  fun kaprekar_list
       ([], collector) = rev collector
    |  (num :: nums, collector ) = 
         let
           val k = kaprekar num
         in 
           if (k = ()) then
             kaprekar_list (nums, collector)
           else
             kaprekar_list (nums, num :: collector)
         end
    |  (num :: nums) = kaprekar_list (num :: nums, []) 
end
;
```

Generate and show all Kaprekar numbers less than 10,000.

```ocaml
print "kaprekar numbers < 10_000: "; println ` kaprekar_list (iota 10000);
```

Optionally, count (and report the count of) how many Kaprekar numbers are less than 1,000,000.

```ocaml
print "number of kaprekar numbers < 1_000_000: "; println ` len ` kaprekar_list (iota 1000000);
```

Output:

```txt
kaprekar numbers < 10_000: [1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
number of kaprekar numbers < 1_000_000: 54
```


=={{header|Modula-2}}==

```modula2
MODULE Kaprekar;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT Write,WriteString,WriteLn,ReadChar;

PROCEDURE kaprekar(n,base : LONGCARD) : BOOLEAN;
VAR
    nn,r,tens : LONGCARD;
BEGIN
    nn := n*n;
    tens := 1;
    IF ((nn - n) MOD (base - 1)) # 0 THEN RETURN FALSE END;

    WHILE tens < n DO tens := tens * base END;
    IF n = tens THEN
        IF 1 = n THEN RETURN TRUE END;
        RETURN FALSE
    END;

    LOOP
        r := nn MOD tens;
        IF r >= n THEN BREAK END;
        IF nn DIV tens + r = n THEN RETURN tens#0 END;
        tens := tens * base;
    END;

    RETURN FALSE
END kaprekar;

PROCEDURE print_num(n,base : LONGCARD);
VAR q,d : LONGCARD;
BEGIN
    d := base;

    WHILE d<n DO d := d * base END;
    LOOP
        d := d DIV base;
        IF n BAND d = 0 THEN RETURN END;
        q := n DIV d;
        IF q<10 THEN
            Write(CHR(INT(q) + INT(ORD('0'))))
        ELSE
            Write(CHR(INT(q) + INT(ORD('a')) - 10))
        END;
        n := n - q * d
    END
END print_num;

VAR
    buf : ARRAY[0..63] OF CHAR;
    i,tens,cnt,base : LONGCARD;
BEGIN
    cnt := 0;
    base := 10;
    FOR i:=1 TO 1000000 DO
        IF kaprekar(i,base) THEN
            INC(cnt);
            FormatString("%3u: %u\n", buf, cnt, i);
            WriteString(buf)
        END
    END;

    ReadChar
END Kaprekar.
```



## Nim


```nim
import strutils, sequtils

proc k(n): bool =
  let n2 = $(n.in64 * n)
  for i in 0 .. <n2.len:
    let a = if i > 0: parseBiggestInt n2[0 .. <i] else: 0
    let b = parseBiggestInt n2[i .. n2.high]
    if b > 0 and a + b == n:
      return true

echo toSeq(1..10_000).filter(k)
echo len toSeq(1..1_000_000).filter(k)
```

{{out}}

```txt
[1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
54
```



## PARI/GP


```parigp
K(d)={
  my(D=10^d,DD,t,v=List());
  for(n=D/10+1,D-1,
    t=divrem(n^2,D);
    if(t[2]&t[1]+t[2]==n,listput(v,n);next);
    DD=D;
    while(t[2]<n,
      t=divrem(n^2,DD*=10);
      if(t[2]&t[1]+t[2]==n,listput(v,n);next(2))
    );
    DD=D;
    while(t[1]<n,
      t=divrem(n^2,DD/=10);
      if(t[2]&t[1]+t[2]==n,listput(v,n);next(2))
    )
  );
  Vec(v)
};
upTo(d)=my(v=[1]);for(n=1,d,v=concat(v,K(n)));v;
upTo(4)
v=upTo(6);v
#v
```

{{out}}

```txt
%1 = [1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]

%2 = [1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999, 17344, 22222, 38962, 77778, 82656, 95121, 99999, 142857, 148149, 181819, 187110, 208495, 318682, 329967, 351352, 356643, 390313, 461539, 466830, 499500, 500500, 533170, 538461, 609687, 627615, 643357, 648648, 670033, 681318, 791505, 812890, 818181, 851851, 857143, 961038, 994708, 999999]

%3 = 54
```



## Perl

===Numeric with casting out nines (fast)===

```Perl
sub isKap {
  my $k = shift;
  return if $k*($k-1) % 9;  # Fast return "casting out nines"
  my($k2, $p) = ($k*$k, 10);
  do {
    my $i = int($k2/$p);
    my $j = $k2 % $p;
    return 1 if $j && $i+$j == $k;
    $p *= 10;
  } while $p <= $k2;
  0;
}

print "[", join(" ", grep { isKap($_) } 1..9999), "]\n\n";
my @kaprekar;
isKap($_) && push @kaprekar,$_ for 1..1_000_000;
print "Kaprekar Numbers below 1000000: ", scalar(@kaprekar), "\n";
```

{{out}}

```txt

[1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999]

Kaprekar Numbers below 1000000: 54

```


===Iannucci factoring method (extremely fast)===
We can also use the method of [https://cs.uwaterloo.ca/journals/JIS/VOL3/iann2a.html Douglas Iannucci] to get much faster results (the below takes only a few milliseconds).  This works for bigints as well.
{{libheader|ntheory}}

```perl
use ntheory qw/fordivisors gcd invmod/;

my %kap;
for my $n (1..15) {
  my $np = int(10**$n)-1;
  fordivisors {
    my($d, $dp) = ($_, $np/$_);
    $kap{ $dp==1 ? $d : invmod($d,$dp)*$d }++
      if  gcd($d, $dp) == 1;
  } $np;
}
my @kap = sort { $a<=>$b } keys %kap;
for my $n (6 .. 14) {
  my $np = int(10**$n)-1;
  printf "Kaprekar numbers <= 10^%2d:  %5d\n",
         $n, scalar(grep { $_ <= $np } @kap);
}
```

{{out}}

```txt

Kaprekar numbers <= 10^ 6:     54
Kaprekar numbers <= 10^ 7:     62
Kaprekar numbers <= 10^ 8:     91
Kaprekar numbers <= 10^ 9:    102
Kaprekar numbers <= 10^10:    132
Kaprekar numbers <= 10^11:    149
Kaprekar numbers <= 10^12:    264
Kaprekar numbers <= 10^13:    281
Kaprekar numbers <= 10^14:    316

```



## Perl 6

===String version (slow)===

```perl6
sub kaprekar( Int $n ) {
    my $sq = $n ** 2;
    for 0 ^..^ $sq.chars -> $i {
        my $x = +$sq.substr(0, $i);
        my $y = +$sq.substr($i) || return;
        return True if $x + $y == $n;
    }
    False;
}

print 1;
print " $_" if .&kaprekar for ^10000;
print "\n";
```

{{out}}

```txt
1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999
```

===Numeric version (medium)===
The addition of <tt>'''race'''</tt>, in two places, allows for concurrent computation, and brings a significant speed-up in running time. 
```perl6
sub kaprekar( Int $n, Int :$base = 10 ) {
    my $hi = $n ** 2;
    my $lo = 0;
    loop (my $s = 1; $hi; $s *= $base) {
        $lo += ($hi % $base) * $s;
        $hi div= $base;
        return $hi,$lo if $lo + $hi == $n and $lo;
    }
    ();
}

print " $_" if .&kaprekar for ^10_000;

my $n;
(^1_000_000).race.map: { $n++ if kaprekar $_ }
say "\n\nBase 10 Kaprekar numbers < :10<1_000_000> = $n";

say "\nBase 17 Kaprekar numbers < :17<1_000_000>";

my &k17 = &kaprekar.assuming(:base(17));

my @results;
(^:17<1_000_000>).race.map: -> $n {
    my ($h,$l) = k17 $n;
    next unless $l;
    my $n17 = $n.base(17);
    my $s17 = ($n * $n).base(17);
    my $h17 = $h.base(17);
    @results.push: "$n $n17 $s17 ($h17 + $s17.substr(* - max(1,($s17.chars - $h17.chars))))";
}

.say for @results.sort({$^a.chars <=> $^b.chars});
```

{{out}}
<pre style="height:35ex">1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999

Base 10 Kaprekar numbers < :10<1_000_000> = 54

Base 17 Kaprekar numbers < :17<1_000_000>
1 1 1 (0 + 1)
16 G F1 (F + 1)
64 3D E2G (E + 2G)
225 D4 A52G (A5 + 2G)
288 GG GF01 (GF + 01)
1536 556 1B43B2 (1B4 + 3B2)
3377 BBB 8093B2 (809 + 3B2)
4912 GGG GGF001 (GGF + 001)
7425 18BD 24E166G (24E + 166G)
9280 1F1F 39B1B94 (39B + 1B94)
16705 36DB B992C42 (B99 + 2C42)
20736 43CD 10DE32FG (10DE + 32FG)
30016 61EB 23593F92 (2359 + 3F92)
36801 785D 351E433G (351E + 433G)
37440 7A96 37144382 (3714 + 4382)
46081 967B 52G94382 (52G9 + 4382)
46720 98B4 5575433G (5575 + 433G)
53505 AF26 6GA43F92 (6GA4 + 3F92)
62785 CD44 9A5532FG (9A55 + 32FG)
66816 DA36 AEG42C42 (AEG4 + 2C42)
74241 F1F2 D75F1B94 (D75F + 1B94)
76096 F854 E1F5166G (E1F5 + 166G)
83520 GGGG GGGF0001 (GGGF + 0001)
266224 33334 A2C52A07G (A2C5 + 2A07G)
1153633 DDDDD B3D5E2A07G (B3D5E + 2A07G)
1334529 FGACC F0540F1A78 (F054 + 0F1A78)
1419856 GGGGG GGGGF00001 (GGGGF + 00001)
1787968 146FCA 19G4C12DG7F (19G4C + 12DG7F)
3122497 236985 4E3BE1F95D8 (4E3BE + 1F95D8)
3773952 2B32B3 711CB2420F9 (711CB + 2420F9)
4243968 2GDE03 8FEGB27EG09 (8FEGB + 27EG09)
5108481 3A2D6F CG10B2E3C64 (CG10B + 2E3C64)
5561920 3FA16D F5EAE3043CG (F5EAE + 3043CG)
6031936 443CCD 110DDE332FFG (110DDE + 332FFG)
6896449 4E9C28 16A10C37GB1D (16A10C + 37GB1D)
7435233 54067B 1A72G93AA382 (1A72G9 + 3AA382)
8017920 5AGGB6 1EF1D43D1EF2 (1EF1D4 + 3D1EF2)
9223201 687534 2835C5403G7G (2835C5 + 403G7G)
9805888 6F6F6G 2DBE3F41C131 (2DBE3F + 41C131)
11140416 7E692A 3A997C43DGBF (3A997C + 43DGBF)
11209185 7F391E 3B58D543F059 (3B58D5 + 43F059)
12928384 91D7F3 4EF79B43F059 (4EF79B + 43F059)
12997153 92A7E7 4FD82943DGBF (4FD829 + 43DGBF)
14331681 A1A1A1 5GF07041C131 (5GF070 + 41C131)
14914368 A89BDD 685C5E403G7G (685C5E + 403G7G)
16119649 B6005B 79F2793D1EF2 (79F279 + 3D1EF2)
16702336 BCGA96 8267143AA382 (826714 + 3AA382)
17241120 C274E9 8B7ACD37GB1D (8B7ACD + 37GB1D)
18105633 CCD444 99A555332FFG (99A555 + 332FFG)
18575649 D16FA4 A12BE53043CG (A12BE5 + 3043CG)
19029088 D6E3A2 A9A83F2E3C64 (A9A83F + 2E3C64)
19893601 E032GE B953G527EG09 (B953G5 + 27EG09)
20363617 E5DE5E C1BD752420F9 (C1BD75 + 2420F9)
21015072 EDA78C CF11C41F95D8 (CF11C4 + 1F95D8)
22349601 FCA147 E9D1D912DG7F (E9D1D9 + 12DG7F)
22803040 G10645 F2FCDE0F1A78 (F2FCDE + 0F1A78)
24137568 GGGGGG GGGGGF000001 (GGGGGF + 000001)
```

Note that this algorithm allows the null string on the left, taken as zero, which automatically includes 1 as the first element of the sequence.

===Casting out nines (fast)===

```perl6
sub kaprekar-generator( :$base = 10 ) {
    my $base-m1 = $base - 1;
    gather loop (my $place = 1; ; ++$place) {
        my $nend = $base ** $place;
        loop (my $n = $base ** ($place - 1); $n < $nend; ++$n) {
            if $n * ($n - 1) %% $base-m1 {
                my $pend = $place * 2;
                loop (my $p = $place; $p < $pend; ++$p) {
                    my $B = $base ** $p;
                    my $lo = $n * ($B - $n) div ($B - 1);
                    my $hi = floor $n - $lo;
                    if $n * $n == $hi * $B + $lo and $lo {
                        take [$n, $hi, $lo];
                        last;
                    }
                }
            }
        }
    }
}

print " $_[0]" for kaprekar-generator() ...^ *.[0] >= 10_000;
say "\n";

say "Base 10 Kaprekar numbers < :10<1_000_000> = ", +(kaprekar-generator() ...^ *.[0] >= 1000000);
say '';

say "Base 17 Kaprekar numbers < :17<1_000_000>";

my &k17-gen = &kaprekar-generator.assuming(:base(17));

for k17-gen() ...^ *.[0] >= :17<1000000> -> @r {
    my ($n,$h,$l) = @r;
    my $n17 = $n.base(17);
    my $s = $n * $n;
    my $s17 = $s.base(17);
    my $h17 = $h.base(17);
    my $l17 = $l.base(17);
    $l17 = '0' x ($s17.chars - $h17.chars - $l17.chars) ~ $l17;
    say "$n $n17 $s17 ($h17 + $l17)";
}
```

(Same output.)


## Phix


```Phix
atom r, l
function Kaprekar(integer n, base=10)
atom sq=n*n, basen = base
    if n=1 then return true end if
    r=0
    while r<n do
        r = mod(sq,basen)
        l = floor(sq/basen)
        if r and l and l+r=n then return true end if
        basen *= base
    end while
    return false
end function

sequence s = {}
for i=1 to 10000 do
    if Kaprekar(i) then
        s &= i
    end if
end for
printf(1,"There are %d Kaprekar numbers between 1 and 10,000:\n",length(s))
?s
integer c = 0
for i=1 to 1000000 do
    c += Kaprekar(i)
end for
printf(1,"There are %d Kaprekar numbers between 1 and 1,000,000\n",c)

function base17(sequence s)
    for i=1 to length(s) do
        atom si = s[i]
        string num = ""
        while si do
            integer digit = mod(si,17)
            si = floor(si/17)
            num = digit+iff(digit<=9?'0':87)&num
        end while
        s[i] = num
    end for
    return s
end function

s = {} r = 1 l = 1
for i=1 to 1000000 do
    if Kaprekar(i,17) then
        s = append(s,{i,i*i,l,r})
    end if
end for
printf(1,"There are %d Kaprekar base 17 numbers between 1 and 1,000,000 (decimal):\n",length(s))
s[5..-5] = {}
for i=1 to length(s) do
    printf(1,"%s squared %s, split %s+%s\n",base17(s[i]))
    if i=4 then printf(1," ...\n") end if
end for
```

{{out}}

```txt

There are 17 Kaprekar numbers between 1 and 10,000:
{1,9,45,55,99,297,703,999,2223,2728,4879,4950,5050,5292,7272,7777,9999}
There are 54 Kaprekar numbers between 1 and 1,000,000
There are 24 Kaprekar base 17 numbers between 1 and 1,000,000 (decimal):
1 squared 1, split 1+1
g squared f1, split f+1
3d squared e2g, split e+2g
d4 squared a52g, split a5+2g
 ...
f1f2 squared d75f1b94, split d75f+1b94
f854 squared e1f5166g, split e1f5+166g
gggg squared gggf0001, split gggf+1
33334 squared a2c52a07g, split a2c5+2a07g

```



## PHP


```php
set_time_limit(300);

print_r(array_filter(range(1, 10000), 'isKaprekar'));
echo count(array_filter(range(1, 1000000), 'isKaprekar'));

function isKaprekar($n) {
    $a = $n * $n; 
    $b = bcmod("$a", "10");
    for ($d = 1, $t = 0; $a > 0; $d *= 10) {
        $b += $t * $d;
        if ($b > $n) break;
        $a = floor($a / 10);
        if ($b && $a + $b == $n) 
            return true;
        $t = bcmod("$a", "10");
    }
    return false;
}
```



```txt
Array
(
    [0] => 1
    [8] => 9
    [44] => 45
    [54] => 55
    [98] => 99
    [296] => 297
    [702] => 703
    [998] => 999
    [2222] => 2223
    [2727] => 2728
    [4878] => 4879
    [4949] => 4950
    [5049] => 5050
    [5291] => 5292
    [7271] => 7272
    [7776] => 7777
    [9998] => 9999
)
54
```



## PicoLisp


```PicoLisp
(de kaprekar (N)
   (let L (cons 0 (chop (* N N)))
      (for ((I . R) (cdr L) R (cdr R))
         (NIL (gt0 (format R)))
         (T (= N (+ @ (format (head I L)))) N) ) ) )
```

{{out}}

```txt
: (filter kaprekar (range 1 10000))
-> (1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999)

: (cnt kaprekar (range 1 1000000))
-> 54
```



## PL/I


```PL/I

kaprekar: procedure options (main);  /* 22 January 2012 */
   declare i fixed decimal (9), j fixed binary;
   declare s character (20) character varying;
   declare m fixed decimal (9);
   declare (z, zeros) character (20) varying;

   zeros = '00000000000000000000';

   put skip list (1);
   do i = 2 to 100000;
      s = i*i;
      s = trim(s);
      z = substr(zeros, 1, length(s));
      do j = 1 to length(s)-1;
         if substr(s, j+1) = substr(z, j+1) then leave;
         m = substr(s, 1, j) + substr(s, j+1);
         if i = m then put skip list (i);
      end;
   end;

end kaprekar;

```

{{out}}

```txt

   1 
        9 
       45 
       55 
       99 
      297 
      703 
      999 
     2223 
     2728 
     4879 
     4950 
     5050 
     5292 
     7272 
     7777 
     9999

```



## PowerShell


```PowerShell

function Test-Kaprekar ([int]$Number)
{
    if ($Number -eq 1)
    {
        return $true
    }

    [int64]$a = $Number * $Number
    [int64]$b = 10

    while ($b -lt $a)
    {
        [int64]$remainder = $a % $b
        [int64]$quotient  = ($a - $remainder) / $b

        if ($remainder -gt 0 -and $remainder + $quotient -eq $Number)
        {
            return $true
        }

        $b *= 10
    }

    return $false
}

```


```PowerShell

"Kaprekar numbers less than 10,000:"
1..10000 | ForEach-Object {if (Test-Kaprekar -Number $_) {"{0,6}" -f $_}} | Format-Wide {$_} -Column 17 -Force

"Kaprekar numbers less than 1,000,000:"
1..1000000 | ForEach-Object {if (Test-Kaprekar -Number $_) {"{0,6}" -f $_}} | Format-Wide {$_} -Column 18 -Force

```

{{Out}}

```txt

Kaprekar numbers less than 10,000:


     1       9      45      55      99     297     703     999    2223    2728    4879    4950    5050    5292    7272    7777   9999


Kaprekar numbers less than 1,000,000:


     1       9      45      55      99     297     703     999    2223   2728   4879   4950   5050   5292   7272   7777   9999  17344
 22222   38962   77778   82656   95121   99999  142857  148149  181819 187110 208495 318682 329967 351352 356643 390313 461539 466830
499500  500500  533170  538461  609687  627615  643357  648648  670033 681318 791505 812890 818181 851851 857143 961038 994708 999999

```



## Prolog

Works with SWI-Prolog, uses a list comprehension : [[List comprehensions#Prolog]]

```Prolog
kaprekar_(Z, X) :-
	split_number(Z, 10, X).


split_number(Z, N, X) :-
	N < Z,
	A is Z // N,
	B is Z mod N,
	(   (X is A+B,  B\= 0)-> true; N1 is N*10, split_number(Z, N1, X)).

kaprekar(N, V) :-
	V <- {X & X <- 1 .. N & ((Z is X * X, kaprekar_(Z, X)); X = 1) }.

```

{{out}}

```txt
 ?- kaprekar(1000, V).
V = [1,9,45,55,99,297,703,999]

 ?- kaprekar(1000000, V), length(V, N), format('Numbers of kaprekar numbers under 1000000 : ~w~n', [N]).
Numbers of kaprekar numbers under 1000000 : 54
V = [1,9,45,55,99,297,703,999,2223,2728,4879,4950,5050,5292,7272,7777,9999,17344,22222,38962,77778,82656,95121,99999,142857,
148149,181819,187110,208495,318682,329967,351352,356643,390313,461539,466830,499500,500500,533170,538461,609687,627615,
643357,648648,670033,681318,791505,812890,818181,851851,857143,961038,994708,999999],
N = 54 .
```



## PureBasic

{{trans|C}}

```PureBasic
Procedure Kaprekar(n.i)
  nn.q  = n*n
  tens.q= 1
  While tens<nn: tens*10: Wend  
  Repeat
    tens/10
    If tens<=n: Break: EndIf
    If nn-n = (nn/tens) * (tens-1)
      ProcedureReturn #True
    EndIf
  ForEver
  If n=1
    ProcedureReturn #True
  EndIf
EndProcedure

If OpenConsole()
  For i=1 To 1000000
    If Kaprekar(i)  
      cnt+1
      PrintN(RSet(Str(cnt),3)+":"+RSet(Str(i),8))
    EndIf
  Next
  ;
  Print("Press ENTER to exit")
  Input()
EndIf
```


```txt
  1:       1
  2:       9
  3:      45
  4:      55
  5:      99
  6:     297
  7:     703
  8:     999
 ...........
 51:  857143
 52:  961038
 53:  994708
 54:  999999
Press ENTER to exit
```



## Python


### Splitting strings in a loop

(Swap the commented return statement to return the split information).

```python>>>
 def k(n):
	n2 = str(n**2)
	for i in range(len(n2)):
		a, b = int(n2[:i] or 0), int(n2[i:])
		if b and a + b == n:
			return n
			#return (n, (n2[:i], n2[i:]))

		
>>> [x for x in range(1,10000) if k(x)]
[1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
>>> len([x for x in range(1,1000000) if k(x)])
54
>>> 
```


A stronger code that gives a list of Kaprekar numbers within a range in a given base.
The range must be given as a decimal number.

```python
def encode(n, base):
    result = ""
    while n:
        n, d = divmod(n, base)
        if d < 10:
            result += str(d)
        else:
            result += chr(d - 10 + ord("a"))
    return result[::-1]
def Kaprekar(n, base):
    if n == '1':
        return True
    sq = encode((int(n, base)**2), base)
    for i in range(1,len(sq)):
        if (int(sq[:i], base) + int(sq[i:], base) == int(n, base)) and (int(sq[:i], base) > 0) and (int(sq[i:], base)>0):
            return True
    return False
def Find(m, n, base):
    return [encode(i, base) for i in range(m,n+1) if Kaprekar(encode(i, base), base)]

m = int(raw_input('Where to start?\n'))
n = int(raw_input('Where to stop?\n'))
base = int(raw_input('Enter base:'))
KNumbers = Find(m, n, base)
for i in KNumbers:
    print i
print 'The number of Kaprekar Numbers found are',
print len(KNumbers)
raw_input()
```


### Using Casting Out Nines Generator

See: http://rosettacode.org/wiki/Casting_out_nines#Python for explanation and code for CastOut

```python

Base = 10
N = 6
Paddy_cnt = 1
for n in range(N):
  for V in CastOut(Base,Start=Base**n,End=Base**(n+1)):
    for B in range(n+1,n*2+2):
      x,y = divmod(V*V,Base**B)
      if V == x+y and 0<y:
        print('{1}: {0}'.format(V, Paddy_cnt))
        Paddy_cnt += 1
        break

```

Produces:

```txt

1: 1
2: 9
3: 45
4: 55
5: 99
6: 297
7: 703
8: 999
9: 2223
10: 2728
11: 4879
12: 4950
13: 5050
14: 5292
15: 7272
16: 7777
17: 9999
18: 17344
19: 22222
20: 38962
21: 77778
22: 82656
23: 95121
24: 99999
25: 142857
26: 148149
27: 181819
28: 187110
29: 208495
30: 318682
31: 329967
32: 351352
33: 356643
34: 390313
35: 461539
36: 466830
37: 499500
38: 500500
39: 533170
40: 538461
41: 609687
42: 627615
43: 643357
44: 648648
45: 670033
46: 681318
47: 791505
48: 812890
49: 818181
50: 851851
51: 857143
52: 961038
53: 994708
54: 999999

```

Other bases may be used e.g.:

```python

Base = 16
N = 4
Paddy_cnt = 1
for V in CastOut(Base,Start=1,End=Base**N):
  for B in range(1,N*2-1):
    x,y = divmod(V*V,Base**B)
    if V == x+y and 0<y:
      print('{1}: {0:x}'.format(V, Paddy_cnt))
      Paddy_cnt += 1
      break

```

Produces:

```txt

1: 1
2: 6
3: a
4: f
5: 33
6: 55
7: 5b
8: 78
9: 88
10: ab
11: cd
12: ff
13: 15f
14: 334
15: 38e
16: 492
17: 4ed
18: 7e0
19: 820
20: b13
21: b6e
22: c72
23: ccc
24: ea1
25: fa5
26: fff
27: 191a
28: 2a2b
29: 3c3c
30: 4444
31: 5556
32: 6667
33: 7f80
34: 8080
35: 9999
36: aaaa
37: bbbc
38: c3c4
39: d5d5
40: e6e6
41: ffff

```



## REXX


```rexx
/*REXX pgm generates & counts (+ maybe shows) some Kaprekar #s using the cast─out─9 test*/
                 /*╔═══════════════════════════════════════════════════════════════════╗
                   ║ Kaprekar numbers were thought of by the mathematician from India, ║
                   ║ Shri Dattathreya Ramachardra Kaprekar  (1905 ───► 1986).          ║
                   ╚═══════════════════════════════════════════════════════════════════╝*/
parse arg A B .                                  /*obtain optional arguments from the CL*/
if A=='' | A=","  then A=    10000               /*Not specified?  Then use the default.*/
if B=='' | B=","  then B= -1000000               /* "      "         "   "   "     "    */
call Kaprekar          A                         /*gen Kaprekar numbers,        show 'em*/
call Kaprekar          B                         /* "     "        "      don't show 'em*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Kaprekar: procedure; parse arg N; #=0; aN=abs(N) /*set counter to zero; use  │N│  value.*/
          numeric digits max(9, 2*length(N) )    /*use enough decimal digits for square.*/
          if aN>0  then call tell 1              /*unity is defined to be a Kaprekar #. */
                                                 /* [↑]  handle case of  N  being unity.*/
          if aN>1  then do j=9  for aN-9         /*calculate the  square  of  J   (S).  */
                        if j//9 >2  then iterate /*Is  J mod 9 > two?  Then skip this J.*/
                        s= j*j                   /*calculate the  square  of  J   (S).  */
                        if j//9==s//9  then do k=1  for length(s)%2  /*≡ casted out 9's?*/
                                            parse var    s      L   +(k)   R
                                            if j==L+R  then do;  call tell j;  leave;  end
                                            end   /*k*/
                        end   /*j*/
          say
          say center(" There're "    #    ' Kaprekar numbers below '     aN || ., 79, "═")
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:     #=#+1;  if N>0  then say right(arg(1), digits());   return  /*maybe display it*/
```

{{out|output|text=  when using the default inputs of:     <tt>   10000   -1000000 </tt>}}

```txt

                 1
                 9
                45
                55
                99
               297
               703
               999
              2223
              2728
              4879
              4950
              5050
              5292
              7272
              7777
              9999

═════════════════ There're  17  Kaprekar numbers below  10000.═════════════════

════════════════ There're  54  Kaprekar numbers below  1000000.════════════════

```



## Racket


```racket

#lang racket
(define (kaprekar? n)
  (or (= n 1)
      (let ([q (sqr n)])
        (let loop ((p 10))
          (and (<= p q)
               (or (let-values  ([(b a) (quotient/remainder q p)])
                     (and (> a 0) (= n (+ a b))))
                   (loop (* p 10))))))))

(filter kaprekar? (range 1 10000))

```


```txt

'(1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999)

```



## Ring


```ring

nr = 0
for i = 1 to 200
    if kaprekar(i)
       nr += 1
       if i < 201 see "" + nr + " : " + i + nl ok ok
next
see "total kaprekar numbers under 200 = " + nr + nl
 
func kaprekar n
     s = pow(n,2)
     x = floor(log(s)) + 1
     t = pow(10,x) 
     while true
           t /= 10
           if t<=n exit ok
           if s-n = floor(s/t)*(t-1) n = true ok
     end
     return (n = 1)

```

Output:

```txt

1 : 1
2 : 9
3 : 45
4 : 55
5 : 99
total kaprekar numbers under 200 = 5

```



## Ruby

with extra extra credit

```ruby
def kaprekar(n, base = 10)
  return [1, 1, 1, ""] if n == 1 
  return if n*(n-1) % (base-1) != 0     # casting out nine
  sqr = (n ** 2).to_s(base)
  (1...sqr.length).each do |i|
    a = sqr[0 ... i]
    b = sqr[i .. -1]
    break if b.delete("0").empty?
    sum = a.to_i(base) + b.to_i(base)
    return n.to_s(base), sqr, a, b if sum == n
  end
  nil
end

count = 0
1.upto(10_000 - 1) do |i| 
  if result = kaprekar(i)
    puts "%4d  %8d  %s + %s" % result
    count += 1
  end
end

10_000.upto(1_000_000 - 1) {|i| count += 1 if kaprekar(i)}
puts "#{count} kaprekar numbers under 1,000,000"

puts "\nbase17 kaprekar numbers under (base10)1,000,000"
base = 17
1.upto(1_000_000) do |decimal|
  if result = kaprekar(decimal, base)
    puts "%7s  %5s  %9s  %s + %s" % [decimal, *result]
  end
end
```


{{out}}
<pre style="height: 40ex; overflow: scroll">
   1         1  1 + 
   9        81  8 + 1
  45      2025  20 + 25
  55      3025  30 + 25
  99      9801  98 + 01
 297     88209  88 + 209
 703    494209  494 + 209
 999    998001  998 + 001
2223   4941729  494 + 1729
2728   7441984  744 + 1984
4879  23804641  238 + 04641
4950  24502500  2450 + 2500
5050  25502500  2550 + 2500
5292  28005264  28 + 005264
7272  52881984  5288 + 1984
7777  60481729  6048 + 1729
9999  99980001  9998 + 0001
54 kaprekar numbers under 1,000,000

base17 kaprekar numbers under (base10)1,000,000
      1      1          1  1 + 
     16      g         f1  f + 1
     64     3d        e2g  e + 2g
    225     d4       a52g  a5 + 2g
    288     gg       gf01  gf + 01
   1536    556     1b43b2  1b4 + 3b2
   3377    bbb     8093b2  809 + 3b2
   4912    ggg     ggf001  ggf + 001
   7425   18bd    24e166g  24e + 166g
   9280   1f1f    39b1b94  39b + 1b94
  16705   36db    b992c42  b99 + 2c42
  20736   43cd   10de32fg  10de + 32fg
  30016   61eb   23593f92  2359 + 3f92
  36801   785d   351e433g  351e + 433g
  37440   7a96   37144382  3714 + 4382
  46081   967b   52g94382  52g9 + 4382
  46720   98b4   5575433g  5575 + 433g
  53505   af26   6ga43f92  6ga4 + 3f92
  62785   cd44   9a5532fg  9a55 + 32fg
  66816   da36   aeg42c42  aeg4 + 2c42
  74241   f1f2   d75f1b94  d75f + 1b94
  76096   f854   e1f5166g  e1f5 + 166g
  83520   gggg   gggf0001  gggf + 0001
 266224  33334  a2c52a07g  a2c5 + 2a07g

```



## Run BASIC


```runbasic
for i   = 1 to 5000
  x$    = str$(i * i)
  if i  = 1 then x$ = "10"
  for j = 1 to len(x$) - 1
   if (val(left$(x$,j)) + val(mid$(x$,j+1)) = i and val(mid$(x$,j+1)) <> 0) or i = 1  then print "Kaprekar :";left$(x$,j);" + ";mid$(x$,j+1);" = ";i
  next j
next i
```

```txt
Kaprekar :1 + 0 = 1
Kaprekar :8 + 1 = 9
Kaprekar :20 + 25 = 45
Kaprekar :30 + 25 = 55
Kaprekar :98 + 01 = 99
Kaprekar :88 + 209 = 297
Kaprekar :494 + 209 = 703
Kaprekar :998 + 001 = 999
Kaprekar :494 + 1729 = 2223
Kaprekar :744 + 1984 = 2728
Kaprekar :238 + 04641 = 4879
```



## Scala

{{works with|Scala|2.9.1}}

```Scala
object Kaprekar extends App {

  def isKaprekar(n: Int, base: Int = 10):Option[Triple[String,String,String]] = {
    val check: Long => Option[Triple[String,String,String]] = n => {
      val split: Pair[String, Int] => Pair[String, String] = p => (p._1.slice(0,p._2),p._1.slice(p._2,p._1.size).padTo[Char,String](1,'0'))
      val pwr = n*n
      val sN = java.lang.Long.toString(n, base)
      val sPwr = java.lang.Long.toString(pwr, base)
      for (i <- 1 to sPwr.size) {
        val (a, b) = split(sPwr,i)
        val la = java.lang.Long.parseLong(a, base)
        val lb = java.lang.Long.parseLong(b, base)
        if (lb==0) return None
        if (la+lb==n) return Some(Triple(sPwr,a,b))
      }
      None    
    }
    n match {
      case 1 => Some(Triple("1","0","1"))
      case n if (n>1) => check(n)
      case _ => None
    }
  }

  def kaprekars(n: Int,base: Int=10) = (1 to n).map(isKaprekar(_,base)).zip(1 to n).filter(_._1!=None).map(p=>Triple(base,p._2,p._1 match {case Some(t) => t; case _ => Nil}))

  val k1 = kaprekars(10000)
  k1 foreach {p=>println(p._2)}
  println(k1.size + " Kaprekar numbers < 10000 (b:10) for base 10"+"\n"*2)

  val k2 = kaprekars(1000000)
  k2 foreach {p => println(p._2+"\t"+java.lang.Long.toString(p._2,p._1)+"\t"+p._3.productElement(0)+"\t"+p._3.productElement(1)+" + "+p._3.productElement(2))}
  println(k2.size + " Kaprekar numbers < 1000000 (b:10) for base 10"+"\n"*2)

  val k3 = kaprekars(1000000,17)
  k3 foreach {p => println(p._2+"\t"+java.lang.Long.toString(p._2,p._1)+"\t"+p._3.productElement(0)+"\t"+p._3.productElement(1)+" + "+p._3.productElement(2))}
  println(k3.size + " Kaprekar numbers < 1000000 (b:10) for base 17"+"\n"*2)

}
```

{{out}}
<pre style="height: 40ex; overflow: scroll">1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999
17 Kaprekar numbers < 10000 (b:10) for base 10


1       1       1       0 + 1
9       9       81      8 + 1
45      45      2025    20 + 25
55      55      3025    30 + 25
99      99      9801    98 + 01
297     297     88209   88 + 209
703     703     494209  494 + 209
999     999     998001  998 + 001
2223    2223    4941729 494 + 1729
2728    2728    7441984 744 + 1984
4879    4879    23804641        238 + 04641
4950    4950    24502500        2450 + 2500
5050    5050    25502500        2550 + 2500
5292    5292    28005264        28 + 005264
7272    7272    52881984        5288 + 1984
7777    7777    60481729        6048 + 1729
9999    9999    99980001        9998 + 0001
17344   17344   300814336       3008 + 14336
22222   22222   493817284       4938 + 17284
38962   38962   1518037444      1518 + 037444
77778   77778   6049417284      60494 + 17284
82656   82656   6832014336      68320 + 14336
95121   95121   9048004641      90480 + 04641
99999   99999   9999800001      99998 + 00001
142857  142857  20408122449     20408 + 122449
148149  148149  21948126201     21948 + 126201
181819  181819  33058148761     33058 + 148761
187110  187110  35010152100     35010 + 152100
208495  208495  43470165025     43470 + 165025
318682  318682  101558217124    101558 + 217124
329967  329967  108878221089    108878 + 221089
351352  351352  123448227904    123448 + 227904
356643  356643  127194229449    127194 + 229449
390313  390313  152344237969    152344 + 237969
461539  461539  213018248521    213018 + 248521
466830  466830  217930248900    217930 + 248900
499500  499500  249500250000    249500 + 250000
500500  500500  250500250000    250500 + 250000
533170  533170  284270248900    284270 + 248900
538461  538461  289940248521    289940 + 248521
609687  609687  371718237969    371718 + 237969
627615  627615  393900588225    39390 + 0588225
643357  643357  413908229449    413908 + 229449
648648  648648  420744227904    420744 + 227904
670033  670033  448944221089    448944 + 221089
681318  681318  464194217124    464194 + 217124
791505  791505  626480165025    626480 + 165025
812890  812890  660790152100    660790 + 152100
818181  818181  669420148761    669420 + 148761
851851  851851  725650126201    725650 + 126201
857143  857143  734694122449    734694 + 122449
961038  961038  923594037444    923594 + 037444
994708  994708  989444005264    989444 + 005264
999999  999999  999998000001    999998 + 000001
54 Kaprekar numbers < 1000000 (b:10) for base 10


1       1       1       0 + 1
16      g       f1      f + 1
64      3d      e2g     e + 2g
225     d4      a52g    a5 + 2g
288     gg      gf01    gf + 01
1536    556     1b43b2  1b4 + 3b2
3377    bbb     8093b2  809 + 3b2
4912    ggg     ggf001  ggf + 001
7425    18bd    24e166g 24e + 166g
9280    1f1f    39b1b94 39b + 1b94
16705   36db    b992c42 b99 + 2c42
20736   43cd    10de32fg        10de + 32fg
30016   61eb    23593f92        2359 + 3f92
36801   785d    351e433g        351e + 433g
37440   7a96    37144382        3714 + 4382
46081   967b    52g94382        52g9 + 4382
46720   98b4    5575433g        5575 + 433g
53505   af26    6ga43f92        6ga4 + 3f92
62785   cd44    9a5532fg        9a55 + 32fg
66816   da36    aeg42c42        aeg4 + 2c42
74241   f1f2    d75f1b94        d75f + 1b94
76096   f854    e1f5166g        e1f5 + 166g
83520   gggg    gggf0001        gggf + 0001
266224  33334   a2c52a07g       a2c5 + 2a07g
24 Kaprekar numbers < 1000000 (b:10) for base 17
```



## Scheme


```scheme
; auxiliary functions : range, filter
(define (range a b)
(let loop ((v '()) (i b))
(if (< i a)
    v
    (loop (cons i v)
          (- i 1)))))

(define (filter p u)
(if (equal? u '())
    '()
    (let ((x (car u)) (v (filter p (cdr u))))
         (if (p x)
             (cons x v)
             v))))

(define (kaprekar? n)
(or (= n 1)
    (let ((q (* n n)))
    (let loop ((p 10))
         (cond ((> p q) #f)
               ((let ((a (remainder q p)) (b (quotient q p)))
                     (and (> a 0) (= n (+ a b)))) #t)
               (else (loop (* p 10))))))))

(filter kaprekar? (range 1 10000))
; (1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999)
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func bigInteger: kaprekar (in bigInteger: n, in bigInteger: base) is func
  result
    var bigInteger: kaprekar is 0_;
  local
    var bigInteger: nn is 0_;
    var bigInteger: r is 0_;
    var bigInteger: powerOfBase is 1_;
  begin
    nn := n ** 2;
    while powerOfBase < n do
      powerOfBase *:= base;
    end while;
    if n = powerOfBase then
      kaprekar := bigInteger conv ord(n = 1_);
    else
      r := nn rem powerOfBase;
      while r < n do
        if nn div powerOfBase + r = n then
          kaprekar := powerOfBase;
          r := n;
        else
	  powerOfBase *:= base;
          r := nn rem powerOfBase;
        end if;
      end while;
    end if;
  end func;

const proc: main is func
  local
    var bigInteger: aNumber is 0_;
    var integer: count is 0;
    var bigInteger: powerOfBase is 1_;
    const integer: base is 17;
  begin
    writeln("base 10:");
    for aNumber range 1_ to 1000000_ do
      if kaprekar(aNumber, 10_) <> 0_ then
        incr(count);
        writeln(count lpad 3 <& ": " <& aNumber);
      end if;
    end for;
    writeln;
    writeln("base " <& base <& ":");
    writeln("  1: 1");
    count := 1;
    for aNumber range 2_ to 1000000_ do
      powerOfBase := kaprekar(aNumber, bigInteger conv base);
      if powerOfBase <> 0_ then
        incr(count);
        write(count lpad 3 <& ": " <& aNumber);
        write(" \t" <& aNumber radix base);
        write("\t"  <& aNumber ** 2 radix base);
        write("\t"  <& aNumber ** 2 mdiv powerOfBase radix base);
        write(" + " <& aNumber ** 2 mod powerOfBase radix base);
        writeln;
      end if;
    end for;
  end func;
```


{{out}}

```txt

base 10:
  1: 1
  2: 9
  3: 45
  4: 55
  5: 99
  6: 297
  7: 703
  8: 999
  9: 2223
 10: 2728
 11: 4879
 12: 4950
 13: 5050
 14: 5292
 15: 7272
 16: 7777
 17: 9999
 18: 17344
 19: 22222
 20: 38962
 21: 77778
 22: 82656
 23: 95121
 24: 99999
 25: 142857
 26: 148149
 27: 181819
 28: 187110
 29: 208495
 30: 318682
 31: 329967
 32: 351352
 33: 356643
 34: 390313
 35: 461539
 36: 466830
 37: 499500
 38: 500500
 39: 533170
 40: 538461
 41: 609687
 42: 627615
 43: 643357
 44: 648648
 45: 670033
 46: 681318
 47: 791505
 48: 812890
 49: 818181
 50: 851851
 51: 857143
 52: 961038
 53: 994708
 54: 999999

base 17:
  1: 1
  2: 16 	G	F1	F + 1
  3: 64 	3D	E2G	E + 2G
  4: 225 	D4	A52G	A5 + 2G
  5: 288 	GG	GF01	GF + 1
  6: 1536 	556	1B43B2	1B4 + 3B2
  7: 3377 	BBB	8093B2	809 + 3B2
  8: 4912 	GGG	GGF001	GGF + 1
  9: 7425 	18BD	24E166G	24E + 166G
 10: 9280 	1F1F	39B1B94	39B + 1B94
 11: 16705 	36DB	B992C42	B99 + 2C42
 12: 20736 	43CD	10DE32FG	10DE + 32FG
 13: 30016 	61EB	23593F92	2359 + 3F92
 14: 36801 	785D	351E433G	351E + 433G
 15: 37440 	7A96	37144382	3714 + 4382
 16: 46081 	967B	52G94382	52G9 + 4382
 17: 46720 	98B4	5575433G	5575 + 433G
 18: 53505 	AF26	6GA43F92	6GA4 + 3F92
 19: 62785 	CD44	9A5532FG	9A55 + 32FG
 20: 66816 	DA36	AEG42C42	AEG4 + 2C42
 21: 74241 	F1F2	D75F1B94	D75F + 1B94
 22: 76096 	F854	E1F5166G	E1F5 + 166G
 23: 83520 	GGGG	GGGF0001	GGGF + 1
 24: 266224 	33334	A2C52A07G	A2C5 + 2A07G

```



## Sidef

{{trans|Perl}}

```ruby
var kapr = Set()

for n in (1..15) {
    var k = (10**n - 1)
    k.udivisors.each {|d|
        var dp = k/d
        kapr << (dp == 1 ? d : d*invmod(d, dp))
    }
}

say kapr.grep { .<= 1e4 }.sort

for n in (6 .. 14) {
    var k = (10**n - 1)
    printf("Kaprekar numbers <= 10^%2d:  %5d\n", n, kapr.count_by { .<= k })
}
```


{{out}}

```txt

[1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999]
Kaprekar numbers <= 10^ 6:     54
Kaprekar numbers <= 10^ 7:     62
Kaprekar numbers <= 10^ 8:     91
Kaprekar numbers <= 10^ 9:    102
Kaprekar numbers <= 10^10:    132
Kaprekar numbers <= 10^11:    149
Kaprekar numbers <= 10^12:    264
Kaprekar numbers <= 10^13:    281
Kaprekar numbers <= 10^14:    316

```



## SPL


```spl
kap,n = getkap(1000000)
> i, 1..n
  << kap[i]!<10000
  #.output(kap[i])
<
#.output(n," Kaprekar numbers < 1000000")

getkap(x)=
  > k, 1..x
    n = #.lower(#.log10(k^2))+1
    > i, 1..n
      r = k^2%10^i
      << r>k
      >> r=0
      l = #.lower(k^2/10^i)
      ? r+l=k, kap[#.size(kap,1)+1] = k
    <
  <
  <= kap,#.size(kap,1)
.
```

{{out}}

```txt

1
9
45
55
99
297
703
999
2223
2728
4879
4950
5050
5292
7272
7777
9999
54 Kaprekar numbers < 1000000

```



## Tcl


```tcl
package require Tcl 8.5;   # Arbitrary precision arithmetic, for stretch goal only
proc kaprekar n {
    if {$n == 1} {return 1}
    set s [expr {$n * $n}]
    for {set i 1} {$i < [string length $s]} {incr i} {
	scan $s "%${i}d%d" a b
	if {$b && $n == $a + $b} {
	    return 1
	    #return [list 1 $a $b]
	}
    }
    return 0
}

# Base goal
for {set i 1} {$i < 10000} {incr i} {
    if {[kaprekar $i]} {lappend klist $i}
}
puts [join $klist ", "]

# Stretch goal
for {set i 1} {$i < 1000000} {incr i} {
    incr kcount [kaprekar $i]
}
puts "$kcount Kaprekar numbers less than 1000000"
```

{{out}}

```txt

1, 9, 45, 55, 99, 297, 703, 999, 2223, 2728, 4879, 4950, 5050, 5292, 7272, 7777, 9999
54 Kaprekar numbers less than 1000000

```



## Ursala

First we define a function <code>kd</code> parameterized by a pair of functions <code>p</code> and <code>r</code> for printing and reading natural numbers, which takes a natural number to its Kaprekar decomposition if any.

```Ursala
#import std
#import nat

kd("p","r") = ~&ihB+ (~&rr&& ^|E/~& sum)~|^/~& "r"~~*hNCtXS+ cuts\1+ "p"+ product@iiX

#cast %nLnX

t = ^|(~&,length) (iota; :/1+ ~&rFlS+ * ^/~& kd\%np ~&h+ %nP)~~/10000 1000000
```

The <code>kd</code> function parameterized by the built in decimal printing and reading functions is applied to the sequences from zero to 10000 and zero to 1000000, with the results filtered according to whether the decomposition exists. The inputs in the former case and the length in the latter are shown.

```txt

(
   <
      1,
      9,
      45,
      55,
      99,
      297,
      703,
      999,
      2223,
      2728,
      4879,
      4950,
      5050,
      5292,
      7272,
      7777,
      9999>,
   54)

```

For the rest of the task, functions <code>p</code> and <code>r</code> are defined for numbers in base 17.

```Ursala
p = ||'0'! ~&a^& ^|J(~&,division\17); ^lrNCT/~&falPR @ar -$/iota17 digits--'abcdefg'

r = sum^|(~&,product/17)=>0+ *x -$/digits--'abcdefg' iota17

#show+

t = mat` *K7 pad` *K7 ^C(~&h+ %nP@l,p*+ <.~&l,product@llX,~&rl,~&rr>)*rF ^(~&,kd/p r@h)* iota 1000000
```

The <code>kd</code> function is parameterized by them and a table of results for numbers between 1 and 1000000 is displayed.

```txt

16     g     f1        f    1    
64     3d    e2g       e    2g   
225    d4    a52g      a5   2g   
288    gg    gf01      gf   1    
1536   556   1b43b2    1b4  3b2  
3377   bbb   8093b2    809  3b2  
4912   ggg   ggf001    ggf  1    
7425   18bd  24e166g   24e  166g 
9280   1f1f  39b1b94   39b  1b94 
16705  36db  b992c42   b99  2c42 
20736  43cd  10de32fg  10de 32fg 
30016  61eb  23593f92  2359 3f92 
36801  785d  351e433g  351e 433g 
37440  7a96  37144382  3714 4382 
46081  967b  52g94382  52g9 4382 
46720  98b4  5575433g  5575 433g 
53505  af26  6ga43f92  6ga4 3f92 
62785  cd44  9a5532fg  9a55 32fg 
66816  da36  aeg42c42  aeg4 2c42 
74241  f1f2  d75f1b94  d75f 1b94 
76096  f854  e1f5166g  e1f5 166g 
83520  gggg  gggf0001  gggf 1    
266224 33334 a2c52a07g a2c5 2a07g

```



## Visual Basic .NET

{{trans|FreeBASIC}}

```vbnet
Module Module1

    ReadOnly max As ULong = 1000000

    Function Kaprekar(n As ULong) As Boolean
        If n = 1 Then Return True

        Dim sq = n * n
        Dim sq_str = Str(sq)
        Dim l = Len(sq_str)

        For x = l - 1 To 1 Step -1
            If sq_str(x) = "0" Then
                l = l - 1
            Else
                Exit For
            End If
        Next

        For x = 1 To l - 1
            Dim p2 = Val(Mid(sq_str, x + 1))
            If p2 > n Then
                Continue For
            End If
            Dim p1 = Val(Left(sq_str, x))
            If p1 > n Then Return False
            If (p1 + p2) = n Then Return True
        Next

        Return False
    End Function

    Sub Main()
        Dim count = 0

        Console.WriteLine("Kaprekar numbers below 10000")

        For n = 1 To max - 1
            If Kaprekar(n) Then
                count = count + 1
                If n < 10000 Then
                    Console.WriteLine("{0,2} {1,4}", count, n)
                End If
            End If
        Next

        Console.WriteLine()
        Console.WriteLine("{0} numbers below {1} are kaprekar numbers", count, max)
    End Sub

End Module
```

{{out}}

```txt
Kaprekar numbers below 10000
 1    1
 2    9
 3   45
 4   55
 5   99
 6  297
 7  703
 8  999
 9 2223
10 2728
11 4879
12 4950
13 5050
14 5292
15 7272
16 7777
17 9999

54 numbers below 1000000 are kaprekar numbers
```



## Wortel


```wortel
@let {
  isKap &n [
    @var s +'' *n n
    @for i til +1/#s 2 [
      @vars {
        fn @+!!s.slice 0 i 
        sn @+!s.slice i
      }
      @if =0 sn @break
      @if =n +fn sn @return [fn sn]
    ]
    false
  ]

  ~[
    !console.log "Kaprekar numbers below 10000: {!-isKap @to 1TK}"
    !console.log "Number of Kaprekar numbers below 1000000: {#!-isKap @to 1M}"
  ]
}
```

{{out}}

```txt
Kaprekar numbers below 10000: 1,9,45,55,99,297,703,999,2223,2728,4879,4950,5050,5292,7272,7777,9999
Number of Kaprekar numbers below 1000000: 54
```



## XPL0

Since integers are only 32 bits (there is no long integer), floating
point is used to get the extra precision needed to calculate Kaprekar
numbers up to 1,000,000. Floating point (double) provides 15 decimal digits of precision.


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func Kaprekar(N, B);    \Returns 'true' if N is a Kaprekar number in base B
int  N, B;
real N2, D; int Q, R;
[N2:= sq(float(N));                     \N squared
D:= float(B);                           \(divider)
loop    [Q:= fix(N2/D - Mod(N2,1.));    \get left part (quotient)
        R:= fix(Mod(N2,D));             \get right part (remainder)
        if Q=0 then return false;
        if Q+R=N & R#0 then return true;
        D:= D * float(B);
        ];
];

int N, C;
[Format(1,0);           \show one place before decimal point and none after it
RlOut(0, 1.);           \show Kaprekar numbers less than ten thousand
for N:= 2 to 10_000-1 do
        if Kaprekar(N, 10) then
                [Text(0, " ");  IntOut(0, N)];
CrLf(0);
C:= 1;                  \show count of Kaprekar numbers less than one million
for N:= 2 to 1_000_000-1 do
        if Kaprekar(N, 10) then C:= C+1;
IntOut(0, C);
CrLf(0);
RlOut(0, 1.);           \show Kaprekar numbers less than one million in base 17
for N:= 2 to 1_000_000-1 do
        if Kaprekar(N, 17) then
                [Text(0, " ");  IntOut(0, N)];
CrLf(0);
]
```


{{out}}

```txt

1 9 45 55 99 297 703 999 2223 2728 4879 4950 5050 5292 7272 7777 9999
54
1 16 64 225 288 1536 3377 4912 7425 9280 16705 20736 30016 36801 37440 46081 46720 53505 62785 66816 74241 76096 83520 266224

```



## Yabasic

{{trans|BBC BASIC}}

```Yabasic
clear screen
n = 0
FOR i = 1 TO 999999
    IF FNkaprekar(i) THEN
	n = n + 1
	IF i < 100001 PRINT n, ": ", i
    ENDIF
NEXT i
PRINT "Total Kaprekar numbers under 1,000,000 = ", n
END

sub FNkaprekar(n)
    LOCAL s, t
	
    s = n^2
    t = 10^(INT(LOG(s)) + 1)
    do
	t=t/10
	IF t<=n break
	IF s-n = INT(s/t)*(t-1) return TRUE
    loop
    return (n=1)
end sub
```



## zkl

{{trans|D}}

```zkl
fcn isKaprekarB(n,b=10){
   powr:=n*n;
   r:=l:=0; tens:=b;
   while(r<n){
      r = powr % tens;
      l = powr / tens;
      if (r and (l + r == n)) return(True);
      tens *= b;
   }
   return(False);
}
```


```zkl
println("Kaprekar number <= 10,000:\n",
   [1..].filter(T(isKaprekarB, fcn(n){ if(n>=10000) Void.Stop else True })));

rc:=Ref(0);
[1 .. 0d1_000_000].pump(rc.inc,isKaprekarB,Void.Filter); // if(filter) rc++ 
rc.value.println(" Kaprekar numbers are less than 1,000,000");  // 54

kbs:=[1..].filter(T(isKaprekarB.fp1(17),
                  fcn(n){ if(n>=0d1_000_000) Void.Stop else True }));
Utils.zipWith(fcn(k,n){ "%3d: %7d == %.17B".fmt(n,k,k).println() },kbs,[1..]);
```

{{out}}

```txt

Kaprekar number <= 10,000:
L(1,9,45,55,99,297,703,999,2223,2728,4879,4950,5050,5292,7272,7777,9999)

54 Kaprekar numbers are less than 1,000,000

  1:       1 == 1
  2:      16 == g
  3:      64 == 3d
  4:     225 == d4
  5:     288 == gg
  6:    1536 == 556
  7:    3377 == bbb
  8:    4912 == ggg
  9:    7425 == 18bd
 10:    9280 == 1f1f
 11:   16705 == 36db
 12:   20736 == 43cd
 13:   30016 == 61eb
 14:   36801 == 785d
 15:   37440 == 7a96
 16:   46081 == 967b
 17:   46720 == 98b4
 18:   53505 == af26
 19:   62785 == cd44
 20:   66816 == da36
 21:   74241 == f1f2
 22:   76096 == f854
 23:   83520 == gggg
 24:  266224 == 33334

```


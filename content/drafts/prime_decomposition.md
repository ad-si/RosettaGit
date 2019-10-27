+++
title = "Prime decomposition"
description = ""
date = 2019-10-12T10:59:58Z
aliases = []
[extra]
id = 2502
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}
[[Category:Arbitrary precision]]
{{omit from|GUISS}}

The prime decomposition of a number is defined as a list of prime numbers 
which when all multiplied together, are equal to that number. 


;Example:
  12 = 2 &times; 2 &times; 3,  so its prime decomposition is  {2, 2, 3}


;Task:
Write a function which returns an [[Arrays|array]] or [[Collections|collection]] which contains the prime decomposition of a given number   <big><big><math>n</math></big></big>   greater than   '''1'''. 

If your language does not have an isPrime-like function available, 
you may assume that you have a function which determines 
whether a number is prime (note its name before your code). 

If you would like to test code from this task, you may use code from [[Primality by trial division|trial division]] or the [[Sieve of Eratosthenes]].

Note: The program must not be limited by the word size of your computer or some other artificial limit; it should work for any number regardless of size (ignoring the physical limits of RAM etc).


;Related tasks:
*   [[count in factors]]
*   [[factors of an integer]]
*   [[Sieve of Eratosthenes]]
*   [[primality by trial division]]
*   [[factors of a Mersenne number]]
*   [[trial factoring of a Mersenne number]]
*   [[partition an integer X into N primes]]
*   [[sequence of primes by Trial Division]]





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set.

```360asm
PRIMEDE  CSECT  
         USING  PRIMEDE,R13
         B      80(R15)            skip savearea
         DC     17F'0'             savearea
         DC     CL8'PRIMEDE'
         STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15            end prolog
         LA     R2,0
         LA     R3,1023
         LA     R4,1024
         MR     R2,R4
         ST     R3,N               n=1023*1024
         LA     R5,WBUFFER
         LA     R6,0
         L      R1,N               n
         XDECO  R1,0(R5)
         LA     R5,12(R5)
         MVC    0(3,R5),=C' : '
         LA     R5,3(R5)
         LA     R0,2
         ST     R0,I               i=2
WHILE1   EQU    *                  do while(i<=n/2)
         L      R2,N
         SRA    R2,1
         L      R4,I
         CR     R4,R2              i<=n/2
         BH     EWHILE1
WHILE2   EQU    *                  do while(n//i=0)
         L      R3,N
         LA     R2,0
         D      R2,I
         LTR    R2,R2              n//i=0
         BNZ    EWHILE2
         ST     R3,N               n=n/i
         ST     R3,M               m=n
         L      R1,I               i
         XDECO  R1,WDECO
         MVC    0(5,R5),WDECO+7
         LA     R5,5(R5)
         MVI    OK,X'01'           ok
         B      WHILE2
EWHILE2  EQU    *
         L      R4,I
         CH     R4,=H'2'           if i=2 then
         BNE    NE2
         LA     R0,3
         ST     R0,I               i=3
         B      EIFNE2
NE2      L      R2,I               else
         LA     R2,2(R2)
         ST     R2,I               i=i+2
EIFNE2   B      WHILE1        
EWHILE1  EQU    *
         CLI    OK,X'01'           if ^ok then
         BE     NOTPRIME
         MVC    0(7,R5),=C'[prime]'
         LA     R5,7(R5)
         B      EPRIME
NOTPRIME L      R1,M               m
         XDECO  R1,WDECO
         MVC    0(5,R5),WDECO+7
EPRIME   XPRNT  WBUFFER,80         put 
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
N        DS     F
I        DS     F
M        DS     F
OK       DC     X'00'
WBUFFER  DC     CL80' '
WDECO    DS     CL16
         YREGS  
         END    PRIMEDE
```

{{out}}

```txt

     1047552 :     2    2    2    2    2    2    2    2    2    2    3   11   31

```



## ABAP


```ABAP
class ZMLA_ROSETTA definition
  public
  create public .

  public section.

    types:
      enumber         TYPE          N  LENGTH 60,
      listof_enumber  TYPE TABLE OF enumber .

    class-methods FACTORS
      importing
        value(N) type ENUMBER
      exporting
        value(ORET) type LISTOF_ENUMBER .
  protected section.
  private section.
ENDCLASS.



CLASS ZMLA_ROSETTA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZMLA_ROSETTA=>FACTORS
* +-------------------------------------------------------------------------------------------------+
* | [--->] N                              TYPE        ENUMBER
* | [<---] ORET                           TYPE        LISTOF_ENUMBER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method FACTORS.
    CLEAR oret.
    WHILE n mod 2 = 0.
      n = n / 2.
      APPEND 2 to oret.
    ENDWHILE.
    DATA: lim type enumber,
          i   type enumber.
    lim = sqrt( n ).
    i   = 3.
    WHILE i <= lim.
      WHILE n mod i = 0.
        APPEND i to oret.
        n = n / i.
        lim = sqrt( n ).
      ENDWHILE.
      i = i + 2.
    ENDWHILE.
    IF n > 1.
      APPEND n to oret.
    ENDIF.
  endmethod.
ENDCLASS.
```



## ACL2


```Lisp
(include-book "arithmetic-3/top" :dir :system)

(defun prime-factors-r (n i)
   (declare (xargs :mode :program))
   (cond ((or (zp n) (zp (- n i)) (zp i) (< i 2) (< n 2))
          (list n))
         ((= (mod n i) 0)
          (cons i (prime-factors-r (floor n i) 2)))
         (t (prime-factors-r n (1+ i)))))

(defun prime-factors (n)
   (declare (xargs :mode :program))
   (prime-factors-r n 2))
```



## Ada


The solution is generic. 

The package '''Prime_Numbers''' is instantiated by a type that supports necessary operations +, *, /, mod, >. The constants 0, 1, 2 are parameters too, because the type might have no literals.  The same package is used for [[Almost prime#Ada]], [[Semiprime#Ada]], [[Count in factors#Ada]], [[Primality by Trial Division#Ada]], [[Sequence of primes by Trial Division#Ada]], and [[Ulam_spiral_(for_primes)#Ada]].

This is the specification of the generic package '''Prime_Numbers''':


```ada
generic
   type Number is private;
   Zero : Number;
   One  : Number;
   Two  : Number;
   with function "+"   (X, Y : Number) return Number is <>;
   with function "*"   (X, Y : Number) return Number is <>;
   with function "/"   (X, Y : Number) return Number is <>;
   with function "mod" (X, Y : Number) return Number is <>;
   with function ">"   (X, Y : Number) return Boolean is <>;
package Prime_Numbers is
   type Number_List is array (Positive range <>) of Number;
   function Decompose (N : Number) return Number_List;
   function Is_Prime (N : Number) return Boolean;
end Prime_Numbers;
```


The function Decompose first estimates the maximal result length as log<sub>2</sub> of the argument. Then it allocates the result and starts to enumerate divisors. It does not care to check if the divisors are prime, because non-prime divisors will be automatically excluded. 

This is the implementation of the generic package '''Prime_Numbers''':


```ada
package body Prime_Numbers is
 -- auxiliary (internal) functions
   function First_Factor (N : Number; Start : Number) return Number is
      K    : Number  := Start;
   begin
      while ((N mod K) /= Zero) and then (N > (K*K))  loop 
         K := K + One;
      end loop;
      if (N mod K) = Zero then
         return K;
      else
         return N;
      end if;
   end First_Factor;
   
   function Decompose (N : Number; Start : Number) return Number_List is
      F: Number := First_Factor(N, Start);
      M: Number := N / F;
   begin
      if M = One then -- F is the last factor
         return (1 => F);
      else
         return F & Decompose(M, Start);
      end if;
   end Decompose;
   
 -- functions visible from the outside
   function Decompose (N : Number) return Number_List is (Decompose(N, Two));
   function Is_Prime (N : Number) return Boolean is
      (N > One and then First_Factor(N, Two)=N);
end Prime_Numbers;
```
 

In the example provided, the package '''Prime_Numbers''' is instantiated with plain integer type:


```ada
with Prime_Numbers, Ada.Text_IO; 
 
procedure Test_Prime is

   package Integer_Numbers is new 
     Prime_Numbers (Natural, 0, 1, 2); 
   use Integer_Numbers;
     
   procedure Put (List : Number_List) is
   begin
      for Index in List'Range loop
         Ada.Text_IO.Put (Positive'Image (List (Index)));
      end loop;
   end Put;
     
begin
   Put (Decompose (12));
end Test_Prime;
```


{{out}} (decomposition of 12):

```txt

 2 2 3

```



## ALGOL 68

{{trans|Python}} - note: This specimen retains the original [[Prime decomposition#Python|Python]] coding style. 

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}


```algol68
#IF long int possible THEN #

MODE LINT = LONG INT;
LINT lmax int = long max int;
OP LLENG = (INT i)LINT: LENG i,
   LSHORTEN = (LINT i)INT: SHORTEN i;

#ELSE

MODE LINT = INT;
LINT lmax int = max int;
OP LLENG = (INT i)LINT: i,
   LSHORTEN = (LINT i)INT: i;

FI#

OP LLONG = (INT i)LINT: LLENG i;

MODE YIELDLINT = PROC(LINT)VOID;

PROC (LINT, YIELDLINT)VOID gen decompose;

INT upb cache = bits width;

BITS cache := 2r0;
BITS cached := 2r0;

PROC is prime = (LINT n)BOOL: (
    BOOL
        has factor := FALSE,
        out := TRUE;
  # FOR LINT factor IN # gen decompose(n, # ) DO ( #
  ##   (LINT factor)VOID:(
      IF has factor THEN out := FALSE; GO TO done FI;
      has factor := TRUE
  # OD # ));
    done: out
);

PROC is prime cached := (LINT n)BOOL: (
    LINT l half n = n OVER LLONG 2 - LLONG 1;
    IF l half n <= LLENG upb cache THEN
        INT half n = LSHORTEN l half n;
        IF half n ELEM cached THEN
            BOOL(half n ELEM cache)
        ELSE
            BOOL out = is prime(n);
            BITS mask = 2r1 SHL (upb cache - half n);
            cached := cached OR mask;
            IF out THEN cache := cache OR mask FI;
            out
        FI
    ELSE
        is prime(n) # above useful cache limit #
    FI
);


PROC gen primes := (YIELDLINT yield)VOID:(
    yield(LLONG 2);
    LINT n := LLONG 3;
    WHILE n < l maxint - LLONG 2 DO
        yield(n);
        n +:= LLONG 2;
        WHILE n < l maxint - LLONG 2 AND NOT is prime cached(n) DO
            n +:= LLONG 2
        OD
    OD
);

# PROC # gen decompose := (LINT in n, YIELDLINT yield)VOID: (
    LINT n := in n;
  # FOR LINT p IN # gen primes( # ) DO ( #
  ##   (LINT p)VOID:
        IF p*p > n THEN
            GO TO done
        ELSE
            WHILE n MOD p = LLONG 0 DO
                yield(p);
                n := n OVER p
            OD
        FI
  # OD #  );
    done:
    IF n > LLONG 1 THEN
        yield(n)
    FI
);

main:(
# FOR LINT m IN # gen primes( # ) DO ( #
##   (LINT m)VOID:(
      LINT p = LLONG 2 ** LSHORTEN m - LLONG 1;
      print(("2**",whole(m,0),"-1 = ",whole(p,0),", with factors:"));
    # FOR LINT factor IN # gen decompose(p, # ) DO ( #
    ##   (LINT factor)VOID:
          print((" ",whole(factor,0)))
    # OD # );
      print(new line);
      IF m >= LLONG 59 THEN GO TO done FI
# OD #  ));
  done: EMPTY
)
```

{{out}}

```txt

2**2-1 = 3, with factors: 3
2**3-1 = 7, with factors: 7
2**5-1 = 31, with factors: 31
2**7-1 = 127, with factors: 127
2**11-1 = 2047, with factors: 23 89
2**13-1 = 8191, with factors: 8191
2**17-1 = 131071, with factors: 131071
2**19-1 = 524287, with factors: 524287
2**23-1 = 8388607, with factors: 47 178481
2**29-1 = 536870911, with factors: 233 1103 2089
2**31-1 = 2147483647, with factors: 2147483647
2**37-1 = 137438953471, with factors: 223 616318177
2**41-1 = 2199023255551, with factors: 13367 164511353
2**43-1 = 8796093022207, with factors: 431 9719 2099863
2**47-1 = 140737488355327, with factors: 2351 4513 13264529
2**53-1 = 9007199254740991, with factors: 6361 69431 20394401
2**59-1 = 576460752303423487, with factors: 179951 3203431780337

```

Note: [[ALGOL 68G]] took 49,109,599 BogoMI and [[ELLA ALGOL 68RS]] took 1,127,634 BogoMI to complete the example.

## Applesoft BASIC


```ApplesoftBasic
9040 PF(0) = 0 : SC = 0
9050 FOR CA = 2 TO INT( SQR(I))
9060     IF I = 1 THEN RETURN
9070     IF INT(I / CA) * CA = I THEN GOSUB 9200 : GOTO 9060
9080     CA = CA + SC : SC = 1
9090 NEXT CA
9100 IF I = 1 THEN RETURN
9110 CA = I

9200 PF(0) = PF(0) + 1
9210 PF(PF(0)) = CA
9220 I = I / CA
9230 RETURN
```



## Arturo



```arturo
loop $(filter $(range 2 60) { isPrime & }) [num]{
	n 2^num-1
	print "2^" + num + "-1 = " + n + " => prime decomposition: " + $(primeFactors n)
}
```


{{out}}


```txt
2^2-1 = 3 => prime decomposition: #(3)
2^3-1 = 7 => prime decomposition: #(7)
2^5-1 = 31 => prime decomposition: #(31)
2^7-1 = 127 => prime decomposition: #(127)
2^11-1 = 2047 => prime decomposition: #(23 89)
2^13-1 = 8191 => prime decomposition: #(8191)
2^17-1 = 131071 => prime decomposition: #(131071)
2^19-1 = 524287 => prime decomposition: #(524287)
2^23-1 = 8388607 => prime decomposition: #(47 178481)
2^29-1 = 536870911 => prime decomposition: #(233 1103 2089)
2^31-1 = 2147483647 => prime decomposition: #(2147483647)
2^37-1 = 137438953471 => prime decomposition: #(223 616318177)
2^41-1 = 2199023255551 => prime decomposition: #(13367 164511353)
2^43-1 = 8796093022207 => prime decomposition: #(431 9719 2099863)
2^47-1 = 140737488355327 => prime decomposition: #(2351 4513 13264529)
2^53-1 = 9007199254740991 => prime decomposition: #(6361 69431 20394401)
2^59-1 = 576460752303423487 => prime decomposition: #(179951 3203431780337)
```



## AutoHotkey


```AutoHotkey
MsgBox % factor(8388607)   ; 47 * 178481
 
factor(n)
{
    if (n = 1)
        return
    f = 2
    while (f <= n)
    {
        if (Mod(n, f) = 0)
        {
            next := factor(n / f)
            return, % f "`n" next
        }
        f++
    }
}
```



## AWK

As the examples show, pretty large numbers can be factored in tolerable time:


```awk
# Usage:  awk -f primefac.awk
function pfac(n,    r, f){
	r = ""; f = 2
	while (f <= n) {
		while(!(n % f)) {
			n = n / f
			r = r " " f
		}
		f = f + 2 - (f == 2)
	}
	return r
}

# For each line of input, print the prime factors.
{ print pfac($1) }

```

{{out}} entering input on stdin:

```txt
$
36
 2 2 3 3
77
 7 11
536870911
 233 1103 2089
8796093022207
 431 9719 2099863
```


## Batch file

Unfortunately Batch does'nt have a BigNum library so the maximum number that can be decomposed is 2^31-1

```Batch file

@echo off
::usage: cmd /k primefactor.cmd number 
setlocal enabledelayedexpansion

set /a compo=%1
if "%compo%"=="" goto:eof
set list=%compo%= (

set /a div=2 & call :loopdiv
set /a div=3 & call :loopdiv
set /a div=5,inc=2

:looptest
call :loopdiv
set /a div+=inc,inc=6-inc,div2=div*div
if %div2% lss %compo% goto looptest
if %compo% neq 1 set list= %list% %compo%
echo %list%)   & goto:eof

:loopdiv
set /a "res=compo%%div
if %res% neq 0 goto:eof
set list=%list% %div%,
set/a compo/=div
goto:loopdiv

```



## Befunge

{{works_with|befungee}}
Handles safely integers only up to 250 (or ones which don't have prime divisors greater than 250).

```Befunge
& 211p > : 1 - #v_ 25*, @ > 11g:. /    v
                > : 11g %!|
                          > 11g 1+ 11p v
       ^                               <
```



## Burlesque



```burlesque

blsq ) 12fC
{2 2 3}

```



## C

Relatively sophiscated sieve method based on size 30 prime wheel. The code does not pretend to handle prime factors larger than 64 bits. All 32-bit primes are cached with 137MB data. Cache data takes about a minute to compute the first time the program is run, which is also saved to the current directory, and will be loaded in a second if needed again. 

```c>#include <inttypes.h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef uint32_t pint;
typedef uint64_t xint;
typedef unsigned int uint;
#define PRIuPINT PRIu32		/* printf macro for pint */
#define PRIuXINT PRIu64		/* printf macro for xint */
#define MAX_FACTORS 63		/* because 2^64 is too large for xint */

uint8_t *pbits;

#define MAX_PRIME (~(pint)0)
#define MAX_PRIME_SQ 65535U
#define PBITS (MAX_PRIME / 30 + 1)

pint next_prime(pint);
int is_prime(xint);
void sieve(pint);

uint8_t bit_pos[30] = {
	0, 1<<0, 0, 0, 0,    0,
	0, 1<<1, 0, 0, 0, 1<<2,
	0, 1<<3, 0, 0, 0, 1<<4,
	0, 1<<5, 0, 0, 0, 1<<6,
	0,    0, 0, 0, 0, 1<<7,
};

uint8_t rem_num[] = { 1, 7, 11, 13, 17, 19, 23, 29 };

void init_primes()
{
	FILE *fp;
	pint s, tgt = 4;

	if (!(pbits = malloc(PBITS))) {
		perror("malloc");
		exit(1);
	}

	if ((fp = fopen("primebits", "r"))) {
		fread(pbits, 1, PBITS, fp);
		fclose(fp);
		return;
	}

	memset(pbits, 255, PBITS);
	for (s = 7; s <= MAX_PRIME_SQ; s = next_prime(s)) {
		if (s > tgt) {
			tgt *= 2;
			fprintf(stderr, "sieve %"PRIuPINT"\n", s);
		}
		sieve(s);
	}
	fp = fopen("primebits", "w");
	fwrite(pbits, 1, PBITS, fp);
	fclose(fp);
}

int is_prime(xint x)
{
	pint p;
	if (x > 5) {
		if (x < MAX_PRIME)
			return pbits[x/30] & bit_pos[x % 30];

		for (p = 2; p && (xint)p * p <= x; p = next_prime(p))
			if (x % p == 0) return 0;

		return 1;
	}
	return x == 2 || x == 3 || x == 5;
}

void sieve(pint p)
{
	unsigned char b[8];
	off_t ofs[8];
	int i, q;

	for (i = 0; i < 8; i++) {
		q = rem_num[i] * p;
		b[i] = ~bit_pos[q % 30];
		ofs[i] = q / 30;
	}

	for (q = ofs[1], i = 7; i; i--)
		ofs[i] -= ofs[i-1];

	for (ofs[0] = p, i = 1; i < 8; i++)
		ofs[0] -= ofs[i];

	for (i = 1; q < PBITS; q += ofs[i = (i + 1) & 7])
		pbits[q] &= b[i];
}

pint next_prime(pint p)
{
	off_t addr;
	uint8_t bits, rem;

	if (p > 5) {
		addr = p / 30;
		bits = bit_pos[ p % 30 ] << 1;
		for (rem = 0; (1 << rem) < bits; rem++);
		while (pbits[addr] < bits || !bits) {
			if (++addr >= PBITS) return 0;
			bits = 1;
			rem = 0;
		}
		if (addr >= PBITS) return 0;
		while (!(pbits[addr] & bits)) {
			rem++;
			bits <<= 1;
		}
		return p = addr * 30 + rem_num[rem];
	}

	switch(p) {
		case 2: return 3;
		case 3: return 5;
		case 5: return 7;
	}
	return 2;
}

int decompose(xint n, xint *f)
{
	pint p = 0;
	int i = 0;

	/* check small primes: not strictly necessary */
	if (n <= MAX_PRIME && is_prime(n)) {
		f[0] = n;
		return 1;
	}

	while (n >= (xint)p * p) {
		if (!(p = next_prime(p))) break;
		while (n % p == 0) {
			n /= p;
			f[i++] = p;
		}
	}
	if (n > 1) f[i++] = n;
	return i;
}

int main()
{
	int i, len;
	pint p = 0;
	xint f[MAX_FACTORS], po;

	init_primes();

	for (p = 1; p < 64; p++) {
		po = (1LLU << p) - 1;
		printf("2^%"PRIuPINT" - 1 = %"PRIuXINT, p, po);
		fflush(stdout);
		if ((len = decompose(po, f)) > 1)
			for (i = 0; i < len; i++)
				printf(" %c %"PRIuXINT, i?'x':'=', f[i]);
		putchar('\n');
	}

	return 0;
}
```



### Using GNU Compiler Collection gcc extensions


{{trans|ALGOL 68}}

{{works with|gcc|4.3.0 20080428 (Red Hat 4.3.0-8)}}

Note: The following code sample is experimental as it implements python style 
iterators for (potentially) infinite sequences.  C is not normally written this 
way, and in the case of this sample it requires the GCC "nested procedure" 
extension to the C language.

```c>#include <limits.h

#include <stdio.h>
#include <math.h>

typedef enum{false=0, true=1}bool;
const int max_lint = LONG_MAX;

typedef long long int lint;
#assert sizeof_long_long_int (LONG_MAX>=8) /* XXX */

/* the following line is the only time I have ever required "auto" */
#define FOR(i,iterator) auto bool lambda(i); yield_init = (void *)&lambda; iterator; bool lambda(i)
#define DO {
#define     YIELD(x) if(!yield(x))return
#define     BREAK return false
#define     CONTINUE return true
#define OD CONTINUE; }
/* Warning: _Most_ FOR(,){ } loops _must_ have a CONTINUE as the last statement. 
 *   Otherwise the lambda will return random value from stack, and may terminate early */

typedef void iterator, lint_iterator; /* hint at procedure purpose */
static volatile void *yield_init; /* not thread safe */
#define YIELDS(type) bool (*yield)(type) = yield_init

typedef unsigned int bits;
#define ELEM(shift, bits) ( (bits >> shift) & 0b1 )

bits cache = 0b0, cached = 0b0;
const lint upb_cache = 8 * sizeof(cache);

lint_iterator decompose(lint); /* forward declaration */

bool is_prime(lint n){
   bool has_factor = false, out = true;
/* for factor in decompose(n) do */
   FOR(lint factor, decompose(n)){
       if( has_factor ){ out = false; BREAK; }
       has_factor = true;
       CONTINUE;
   }
   return out;
}

bool is_prime_cached (lint n){
    lint half_n = n / 2 - 2;
    if( half_n <= upb_cache){
        /* dont cache the initial four, nor the even numbers */
        if (ELEM(half_n,cached)){
            return ELEM(half_n,cache);
        } else {
            bool out = is_prime(n);
            cache = cache | out << half_n;
            cached = cached | 0b1 << half_n;
            return out;
        }
    } else {
        return is_prime(n);
    }
}

lint_iterator primes (){ 
    YIELDS(lint);
    YIELD(2);
    lint n = 3;
    while( n < max_lint - 2 ){
        YIELD(n);
        n += 2;
        while( n < max_lint - 2 && ! is_prime_cached(n) ){
            n += 2;
        }
    }
}

lint_iterator decompose (lint in_n){
    YIELDS(lint);
    lint n = in_n;
 /* for p in primes do */
    FOR(lint p, primes()){
        if( p*p > n ){
            BREAK;
        } else {
            while( n % p == 0 ){
                YIELD(p);
                n = n / p;
            }
        }
        CONTINUE;
    }
    if( n > 1 ){
        YIELD(n);
    }
}

main(){
    FOR(lint m, primes()){
        lint p = powl(2, m) - 1;
        printf("2**%lld-1 = %lld, with factors:",m,p);
        FOR(lint factor, decompose(p)){
            printf(" %lld",factor);
            fflush(stdout);
            CONTINUE;
        }
        printf("\n",m);
        if( m >= 59 )BREAK;
        CONTINUE;
    }
}
```

{{out}}

```txt

2**2-1 = 3, with factors: 3
2**3-1 = 7, with factors: 7
2**5-1 = 31, with factors: 31
2**7-1 = 127, with factors: 127
2**11-1 = 2047, with factors: 23 89
2**13-1 = 8191, with factors: 8191
2**17-1 = 131071, with factors: 131071
2**19-1 = 524287, with factors: 524287
2**23-1 = 8388607, with factors: 47 178481
2**29-1 = 536870911, with factors: 233 1103 2089
2**31-1 = 2147483647, with factors: 2147483647
2**37-1 = 137438953471, with factors: 223 616318177
2**41-1 = 2199023255551, with factors: 13367 164511353
2**43-1 = 8796093022207, with factors: 431 9719 2099863
2**47-1 = 140737488355327, with factors: 2351 4513 13264529
2**53-1 = 9007199254740991, with factors: 6361 69431 20394401
2**59-1 = 576460752303423487, with factors: 179951 3203431780337

```

Note: gcc took 487,719 BogoMI to complete the example.

To understand what was going on with the above code, pass it through <code>cpp</code> and read the outcome.  Translated into normal C code sans the function call overhead, it's really this (the following uses a adjustable cache, although setting it beyond a few thousands doesn't gain further benefit):
```c>#include <stdio.h

#include <stdlib.h>
#include <stdint.h>
 
typedef uint32_t pint;
typedef uint64_t xint;
typedef unsigned int uint;
 
int is_prime(xint);
 
inline int next_prime(pint p)
{
	if (p == 2) return 3;
	for (p += 2; p > 1 && !is_prime(p); p += 2);
	if (p == 1) return 0;
	return p;
}
 
int is_prime(xint n)
{
#	define NCACHE 256
#	define S (sizeof(uint) * 2)
	static uint cache[NCACHE] = {0};
 
	pint p = 2;
	int ofs, bit = -1;
 
	if (n < NCACHE * S) {
		ofs = n / S;
		bit = 1 << ((n & (S - 1)) >> 1);
		if (cache[ofs] & bit) return 1;
	}
 
	do {
		if (n % p == 0) return 0;
		if (p * p > n) break;
	} while ((p = next_prime(p)));
 
	if (bit != -1) cache[ofs] |= bit;
	return 1;
}
 
int decompose(xint n, pint *out)
{
	int i = 0;
	pint p = 2;
	while (n > p * p) {
		while (n % p == 0) {
			out[i++] = p;
			n /= p;
		}
		if (!(p = next_prime(p))) break;
	}
	if (n > 1) out[i++] = n;
	return i;
}
 
int main()
{
	int i, j, len;
	xint z;
	pint out[100];
	for (i = 2; i < 64; i = next_prime(i)) {
		z = (1ULL << i) - 1;
		printf("2^%d - 1 = %llu = ", i, z);
		fflush(stdout);
		len = decompose(z, out);
		for (j = 0; j < len; j++)
			printf("%u%s", out[j], j < len - 1 ? " x " : "\n");
	}
 
	return 0;
}
```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;

namespace PrimeDecomposition
{
    class Program
    {
        static void Main(string[] args)
        {
            GetPrimes(12);
        }

        static List<int> GetPrimes(decimal n)
        {
            List<int> storage = new List<int>();
            while (n > 1)
            {
                int i = 1;
                while (true)
                {
                    if (IsPrime(i))
                    {                        
                        if (((decimal)n / i) == Math.Round((decimal) n / i))
                        {
                            n /= i;
                            storage.Add(i);                            
                            break;
                        }
                    }
                    i++;
                }
            }
            return storage;
        }

        static bool IsPrime(int n)
        {
            if (n <= 1) return false;
            for (int i = 2; i <= Math.Sqrt(n); i++)
                if (n % i == 0) return false;
            return true;
        }
    }
}
```



### Simple trial division

This version a translation from Java of the sample presented by Robert C. Martin during a TDD talk at NDC 2011.
Although this three-line algorithm does not mention anything about primes, the fact that factors are taken out of the number <code>n</code> in ascending order garantees the list will only contain primes.


```csharp
using System.Collections.Generic;

namespace PrimeDecomposition
{
	public class Primes
	{
 		public List<int> FactorsOf(int n)
		{
			var factors = new List<int>();

			for (var divisor = 2; n > 1; divisor++)
				for (; n % divisor == 0; n /= divisor)
					factors.Add(divisor);

			return factors;
		}
}
```



## C++

{{works with|g++|4.1.2 20061115 (prerelease) (Debian 4.1.1-21)}}
{{libheader|GMP}}

```cpp>#include <iostream

#include <gmpxx.h>

// This function template works for any type representing integers or
// nonnegative integers, and has the standard operator overloads for
// arithmetic and comparison operators, as well as explicit conversion
// from int.
//
// OutputIterator must be an output iterator with value_type Integer. 
// It receives the prime factors.
template<typename Integer, typename OutputIterator>
 void decompose(Integer n, OutputIterator out)
{
  Integer i(2);

  while (n != 1)
  {
    while (n % i == Integer(0))
    {
      *out++ = i;
      n /= i;
    }
    ++i;
  }
}

// this is an output iterator similar to std::ostream_iterator, except
// that it outputs the separation string *before* the value, but not
// before the first value (i.e. it produces an infix notation).
template<typename T> class infix_ostream_iterator:
  public std::iterator<T, std::output_iterator_tag>
{
  class Proxy;
  friend class Proxy;
  class Proxy
  {
  public:
    Proxy(infix_ostream_iterator& iter): iterator(iter) {}
    Proxy& operator=(T const& value)
    {
      if (!iterator.first)
      {
        iterator.stream << iterator.infix;
      }
      iterator.stream << value;
    }
  private:
    infix_ostream_iterator& iterator;
  };
public:
  infix_ostream_iterator(std::ostream& os, char const* inf):
    stream(os),
    first(true),
    infix(inf)
  {
  }
  infix_ostream_iterator& operator++() { first = false; return *this; }
  infix_ostream_iterator operator++(int)
  {
    infix_ostream_iterator prev(*this);
    ++*this;
    return prev;
  }
  Proxy operator*() { return Proxy(*this); }
private:
  std::ostream& stream;
  bool first;
  char const* infix;
};

int main()
{
  std::cout << "please enter a positive number: ";
  mpz_class number;
  std::cin >> number;
  
  if (number <= 0)
    std::cout << "this number is not positive!\n;";
  else
  {
    std::cout << "decomposition: ";
    decompose(number, infix_ostream_iterator<mpz_class>(std::cout, " * "));
    std::cout << "\n";
  }
}
```



## Clojure


```clojure
;;; No stack consuming algorithm
(defn factors
  "Return a list of factors of N."
  ([n]
    (factors n 2 ()))
  ([n k acc]
    (if (= 1 n)      
      acc
      (if (= 0 (rem n k))        
        (recur (quot n k) k (cons k acc))
        (recur n (inc k) acc)))))
```



## Commodore BASIC

{{works_with|Commodore BASIC|2.0}}
It's not easily possible to have arbitrary precision integers in PET basic, so here is at least a version using built-in data types (reals). On return from the subroutine starting at 9000 the global array pf contains the number of factors followed by the factors themselves:

```zxbasic
9000 REM ----- function generate
9010 REM in ... i ... number
9020 REM out ... pf() ... factors
9030 REM mod ... ca ... pf candidate
9040 pf(0)=0 : ca=2 : REM special case
9050 IF i=1 THEN RETURN
9060 IF INT(i/ca)*ca=i THEN GOSUB 9200 : GOTO 9050
9070 FOR ca=3 TO INT( SQR(i)) STEP 2
9080 IF i=1 THEN RETURN
9090 IF INT(i/ca)*ca=i THEN GOSUB 9200 : GOTO 9080
9100 NEXT 
9110 IF i>1 THEN ca=i : GOSUB 9200
9120 RETURN
9200 pf(0)=pf(0)+1
9210 pf(pf(0))=ca
9220 i=i/ca
9230 RETURN
```



## Common Lisp


```Lisp
;;; Recursive algorithm
(defun factor (n)
  "Return a list of factors of N."
  (when (> n 1)
    (loop with max-d = (isqrt n)
	  for d = 2 then (if (evenp d) (+ d 1) (+ d 2)) do
	  (cond ((> d max-d) (return (list n))) ; n is prime
		((zerop (rem n d)) (return (cons d (factor (truncate n d)))))))))
```



```Lisp
;;; Tail-recursive version
(defun factor (n &optional (acc '()))
  (when (> n 1) (loop with max-d = (isqrt n)
		   for d = 2 then (if (evenp d) (1+ d) (+ d 2)) do
		     (cond ((> d max-d) (return (cons (list n 1) acc)))
			   ((zerop (rem n d)) 
			    (return (factor (truncate n d) (if (eq d (caar acc))
							       (cons 
								(list (caar acc) (1+ (cadar acc)))
								(cdr acc))
							       (cons (list d 1) acc)))))))))
```



## D


```d
import std.stdio, std.bigint, std.algorithm, std.traits, std.range;

Unqual!T[] decompose(T)(in T number) pure nothrow
in {
    assert(number > 1);
} body {
    typeof(return) result;
    Unqual!T n = number;

    for (Unqual!T i = 2; n % i == 0; n /= i)
        result ~= i;
    for (Unqual!T i = 3; n >= i * i; i += 2)
        for (; n % i == 0; n /= i)
            result ~= i;

    if (n != 1)
        result ~= n;
    return result;
}

void main() {
    writefln("%(%s\n%)", iota(2, 10).map!decompose);
    decompose(1023 * 1024).writeln;
    BigInt(2 * 3 * 5 * 7 * 11 * 11 * 13 * 17).decompose.writeln;
    decompose(16860167264933UL.BigInt * 179951).writeln;
    decompose(2.BigInt ^^ 100_000).group.writeln;
}
```

{{out}}

```txt
[2]
[3]
[2, 2]
[5]
[2, 3]
[7]
[2, 2, 2]
[3, 3]
[2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 11, 31]
[2, 3, 5, 7, 11, 11, 13, 17]
[179951, 16860167264933]
[Tuple!(BigInt, uint)(2, 100000)]
```



## E


This example assumes a function <code>isPrime</code> and was tested with [[Primality by Trial Division#E|this one]]. It could use a self-referential implementation such as the Python task, but the original author of this example did not like the ordering dependency involved.


```e
def primes := {
    var primesCache := [2]
    /** A collection of all prime numbers. */
    def primes {
        to iterate(f) {
            primesCache.iterate(f)
            for x in (int > primesCache.last()) {
                if (isPrime(x)) {
                    f(primesCache.size(), x)
                    primesCache with= x
                }
            }
        }
    }
}

def primeDecomposition(var x :(int > 0)) {
    var factors := []
    for p in primes {
        while (x % p <=> 0) {
            factors with= p
            x //= p
        }
        if (x <=> 1) {
            break
        }
    }
    return factors
}
```



## EchoLisp

The built-in '''prime-factors''' function performs the task.

```lisp
(prime-factors 1024)
   → (2 2 2 2 2 2 2 2 2 2)

(lib 'bigint)
;; 2^59 - 1
(prime-factors (1- (expt 2 59)))
    → (179951 3203431780337)

(prime-factors 100000000000000000037)
    → (31 821 66590107 59004541)
```



## Eiffel

Uses the feature prime from the Task Primality by Trial Devision in the contract to check if the Result contains only prime numbers. 

```Eiffel
class
	PRIME_DECOMPOSITION

feature

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
			is_prime: across Result as r all prime (r.item) end
		end
```


The test was done in an application class. (Similar as in other Eiffel examples (ex. Selectionsort).)

factor(5000)
{{out}}

```txt

2x2x2x5x5x5x5

```



## Ela

{{trans|F#}}

```ela
open integer //arbitrary sized integers

decompose_prime n = loop n 2I
  where 
    loop c p | c < (p * p) = [c]
             | c % p == 0I = p :: (loop (c / p) p)
             | else = loop c (p + 1I)

decompose_prime 600851475143I
```


{{out}}
```txt
[71,839,1471,6857]
```



## Elixir


```elixir
defmodule Prime do
  def decomposition(n), do: decomposition(n, 2, [])
  
  defp decomposition(n, k, acc) when n < k*k, do: Enum.reverse(acc, [n])
  defp decomposition(n, k, acc) when rem(n, k) == 0, do: decomposition(div(n, k), k, [k | acc])
  defp decomposition(n, k, acc), do: decomposition(n, k+1, acc)
end

prime = Stream.iterate(2, &(&1+1)) |> 
        Stream.filter(fn n-> length(Prime.decomposition(n)) == 1 end) |>
        Enum.take(17)
mersenne = Enum.map(prime, fn n -> {n, round(:math.pow(2,n)) - 1} end)
Enum.each(mersenne, fn {n,m} ->
  :io.format "~3s :~20w = ~s~n", ["M#{n}", m, Prime.decomposition(m) |> Enum.join(" x ")]
end)
```


{{out}}

```txt

 M2 :                   3 = 3
 M3 :                   7 = 7
 M5 :                  31 = 31
 M7 :                 127 = 127
M11 :                2047 = 23 x 89
M13 :                8191 = 8191
M17 :              131071 = 131071
M19 :              524287 = 524287
M23 :             8388607 = 47 x 178481
M29 :           536870911 = 233 x 1103 x 2089
M31 :          2147483647 = 2147483647
M37 :        137438953471 = 223 x 616318177
M41 :       2199023255551 = 13367 x 164511353
M43 :       8796093022207 = 431 x 9719 x 2099863
M47 :     140737488355327 = 2351 x 4513 x 13264529
M53 :    9007199254740991 = 6361 x 69431 x 20394401
M59 :  576460752303423487 = 179951 x 3203431780337
```



## Erlang


```erlang
% no stack consuming version

factors(N) ->
     factors(N,2,[]).

factors(1,_,Acc) -> Acc;
factors(N,K,Acc) when N < K*K -> [N|Acc];
factors(N,K,Acc) when N rem K == 0 ->
    factors(N div K,K, [K|Acc]);
factors(N,K,Acc) ->
    factors(N,K+1,Acc).
```



## ERRE


```ERRE

PROGRAM DECOMPOSE


!
! for rosettacode.org
!

!VAR NUM,J

DIM PF[100]

PROCEDURE STORE_FACTOR
   PF[0]=PF[0]+1
   PF[PF[0]]=CA
   I=I/CA
END PROCEDURE

PROCEDURE DECOMP(I)
  PF[0]=0  CA=2 ! special case
  LOOP
     IF I=1 THEN EXIT PROCEDURE END IF
     EXIT IF INT(I/CA)*CA<>I
     STORE_FACTOR
  END LOOP
  FOR CA=3 TO INT(SQR(I)) STEP 2 DO
     LOOP
        IF I=1 THEN EXIT PROCEDURE END IF
        EXIT IF INT(I/CA)*CA<>I
        STORE_FACTOR
     END LOOP
  END FOR
  IF I>1 THEN CA=I STORE_FACTOR END IF
END PROCEDURE

BEGIN
 ! ----- function generate
 ! in ...  I     ... number
 ! out ... PF[]  ... factors
 !         PF[0] ... # of factors
 ! mod ... CA    ... pr.fact. candidate
 PRINT(CHR$(12);) !CLS
 INPUT("Numero ",NUM)
 DECOMP(NUM)
 PRINT(NUM;"=";)
 FOR J=1 TO PF[0] DO
    PRINT(PF[J];)
 END FOR
 PRINT
END PROGRAM

```

This version is a translation from Commodore BASIC program.


## Ezhil


```Ezhil

## இந்த நிரல் தரப்பட்ட எண்ணின் பகாஎண் கூறுகளைக் கண்டறியும்

நிரல்பாகம் பகாஎண்ணா(எண்1)

  ## இந்த நிரல்பாகம் தரப்பட்ட எண் பகு எண்ணா அல்லது பகா எண்ணா என்று கண்டறிந்து சொல்லும்
  ## பகுஎண் என்றால் 0 திரும்பத் தரப்படும்
  ## பகாஎண் என்றால் 1 திரும்பத் தரப்படும்

  @(எண்1 < 0) ஆனால்

   ## எதிர்மறை எண்களை நேராக்குதல்

    எண்1 = எண்1 * (-1)

  முடி

  @(எண்1 < 2) ஆனால்

   ## பூஜ்ஜியம், ஒன்று ஆகியவை பகா எண்கள் அல்ல

    பின்கொடு 0

  முடி

  @(எண்1 == 2) ஆனால்

    ## இரண்டு என்ற எண் ஒரு பகா எண்

    பின்கொடு 1

  முடி

  மீதம் = எண்1%2

  @(மீதம் == 0) ஆனால்

    ## இரட்டைப்படை எண், ஆகவே, இது பகா எண் அல்ல

    பின்கொடு 0

  முடி

    எண்1வர்க்கமூலம் = எண்1^0.5

    @(எண்2 = 3, எண்2 <= எண்1வர்க்கமூலம், எண்2 = எண்2 + 2) ஆக

      மீதம்1 = எண்1%எண்2

      @(மீதம்1 == 0) ஆனால்

        ## ஏதேனும் ஓர் எண்ணால் முழுமையாக வகுபட்டுவிட்டது, ஆகவே அது பகா எண் அல்ல

        பின்கொடு 0

      முடி

    முடி

    பின்கொடு 1

முடி

நிரல்பாகம் பகுத்தெடு(எண்1)

  ## இந்த எண் தரப்பட்ட எண்ணின் பகா எண் கூறுகளைக் கண்டறிந்து பட்டியல் இடும்

  கூறுகள் = பட்டியல்()

  @(எண்1 < 0) ஆனால்

    ## எதிர்மறை எண்களை நேராக்குதல்

    எண்1 = எண்1 * (-1)

  முடி

  @(எண்1 <= 1) ஆனால்

    ## ஒன்று அல்லது அதற்குக் குறைவான எண்களுக்குப் பகா எண் விகிதம் கண்டறியமுடியாது

    பின்கொடு கூறுகள்

  முடி
  
  @(பகாஎண்ணா(எண்1) == 1) ஆனால்

    ## தரப்பட்ட எண்ணே பகா எண்ணாக அமைந்துவிட்டால், அதற்கு அதுவே பகாஎண் கூறு ஆகும்

    பின்இணை(கூறுகள், எண்1)
    பின்கொடு கூறுகள்

  முடி

  தாற்காலிகஎண் = எண்1

  எண்2 = 2

  @(எண்2 <= தாற்காலிகஎண்) வரை

    விடை1 = பகாஎண்ணா(எண்2)
    மீண்டும்தொடங்கு = 0

    @(விடை1 == 1) ஆனால்

      விடை2 = தாற்காலிகஎண்%எண்2
      
      @(விடை2 == 0) ஆனால்

        ## பகா எண்ணால் முழுமையாக வகுபட்டுள்ளது, அதனைப் பட்டியலில் இணைக்கிறோம்

        பின்இணை(கூறுகள், எண்2)
        தாற்காலிகஎண் = தாற்காலிகஎண்/எண்2

        ## மீண்டும் இரண்டில் தொடங்கி இதே கணக்கிடுதலைத் தொடரவேண்டும்

        எண்2 = 2
        மீண்டும்தொடங்கு = 1

      முடி
      
    முடி

    @(மீண்டும்தொடங்கு == 0) ஆனால்

      ## அடுத்த எண்ணைத் தேர்ந்தெடுத்துக் கணக்கிடுதலைத் தொடரவேண்டும்

      எண்2 = எண்2 + 1

    முடி

  முடி

  பின்கொடு கூறுகள்

முடி

அ = int(உள்ளீடு("உங்களுக்குப் பிடித்த ஓர் எண்ணைத் தாருங்கள்: "))

பகாஎண்கூறுகள் = பட்டியல்()

பகாஎண்கூறுகள் = பகுத்தெடு(அ)

பதிப்பி "நீங்கள் தந்த எண்ணின் பகா எண் கூறுகள் இவை: ", பகாஎண்கூறுகள் 

```


=={{header|F_Sharp|F#}}==

```Fsharp
let decompose_prime n = 
  let rec loop c p =
    if c < (p * p) then [c]
    elif c % p = 0I then p :: (loop (c/p) p)
    else loop c (p + 1I)
 
  loop n 2I
  
printfn "%A" (decompose_prime 600851475143I)
```

{{out}}

```txt
[71; 839; 1471; 6857]
```



## Factor

<code>factors</code> from the <code>math.primes.factors</code> vocabulary converts a number into a sequence of its prime divisors; the rest of the code prints this sequence.

```factor
USING: io kernel math math.parser math.primes.factors sequences ;

27720 factors 
[ number>string ] map
" " join print ;
```



## FALSE


```false
[2[\$@$$*@>~][\$@$@$@$@\/*=$[%$." "$@\/\0~]?~[1+1|]?]#%.]d:
27720d;!   {2 2 2 3 3 5 7 11}
```



## Forth


```forth
: decomp ( n -- )
  2
  begin  2dup dup * >=
  while  2dup /mod swap
         if   drop  1+ 1 or    \ next odd number
         else -rot nip  dup .
         then
  repeat
  drop . ;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
module PrimeDecompose
  implicit none

  integer, parameter :: huge = selected_int_kind(18)
  ! => integer(8) ... more fails on my 32 bit machine with gfortran(gcc) 4.3.2

contains

  subroutine find_factors(n, d)
    integer(huge), intent(in) :: n
    integer, dimension(:), intent(out) :: d

    integer(huge) :: div, next, rest
    integer :: i

    i = 1
    div = 2; next = 3; rest = n
    
    do while ( rest /= 1 )
       do while ( mod(rest, div) == 0 ) 
          d(i) = div
          i = i + 1
          rest = rest / div
       end do
       div = next
       next = next + 2
    end do

  end subroutine find_factors

end module PrimeDecompose
```



```fortran
program Primes
  use PrimeDecompose
  implicit none

  integer, dimension(100) :: outprimes
  integer i

  outprimes = 0

  call find_factors(12345649494449_huge, outprimes)

  do i = 1, 100
     if ( outprimes(i) == 0 ) exit
     print *, outprimes(i)
  end do

end program Primes
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isPrime(n As Integer) As Boolean
  If n Mod 2 = 0 Then Return n = 2
  If n Mod 3 = 0 Then Return n = 3
  Dim d As Integer = 5
  While d * d <= n
    If n Mod d = 0 Then Return False
    d += 2
    If n Mod d = 0 Then Return False
    d += 4
  Wend
  Return True
End Function

Sub getPrimeFactors(factors() As UInteger, n As UInteger)
  If n < 2 Then Return
  If isPrime(n) Then
    Redim factors(0 To 0)
    factors(0) = n
    Return
  End If
  Dim factor As UInteger = 2
  Do
    If n Mod factor = 0 Then
      Redim Preserve factors(0 To UBound(factors) + 1)
      factors(UBound(factors)) = factor
      n \= factor     
      If n = 1 Then Return
      If isPrime(n) Then factor = n
    Else
      factor += 1  
    End If    
  Loop
End Sub 

Dim factors() As UInteger
Dim primes(1 To 17) As UInteger = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59}
Dim n As UInteger 
For i As UInteger = 1 To 17
  Erase factors
  n = 1 Shl primes(i) - 1
  getPrimeFactors factors(), n
  Print "2^";Str(primes(i)); Tab(5); " - 1 = "; Str(n); Tab(30);" => ";
  For j As UInteger = LBound(factors) To UBound(factors)
     Print factors(j);
     If j < UBound(factors) Then Print " x ";
  Next j
  Print
Next i
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

2^2  - 1 = 3                  => 3
2^3  - 1 = 7                  => 7
2^5  - 1 = 31                 => 31
2^7  - 1 = 127                => 127
2^11 - 1 = 2047               => 23 x 89
2^13 - 1 = 8191               => 8191
2^17 - 1 = 131071             => 131071
2^19 - 1 = 524287             => 524287
2^23 - 1 = 8388607            => 47 x 178481
2^29 - 1 = 536870911          => 233 x 1103 x 2089
2^31 - 1 = 2147483647         => 2147483647
2^37 - 1 = 137438953471       => 223 x 616318177
2^41 - 1 = 2199023255551      => 13367 x 164511353
2^43 - 1 = 8796093022207      => 431 x 9719 x 2099863
2^47 - 1 = 140737488355327    => 2351 x 4513 x 13264529
2^53 - 1 = 9007199254740991   => 6361 x 69431 x 20394401
2^59 - 1 = 576460752303423487 => 179951 x 3203431780337

```



## Frink

Frink has a built-in factoring function which uses wheel factoring, trial division, Pollard p-1 factoring, and Pollard rho factoring.  
It also recognizes some special forms (e.g. Mersenne numbers) and handles them efficiently.

```frink
println[factor[2^508-1]]
```


{{out}} (total process time including JVM startup = 1.515 s):

```txt

[[3, 1], [5, 1], [509, 1], [18797, 1], [26417, 1], [72118729, 1], [140385293, 1], [2792688414613, 1], [8988357880501, 1], [90133566917913517709497, 1], [56713727820156410577229101238628035243, 1], [170141183460469231731687303715884105727, 1]]

```


Note that this means 3<SUP>1</SUP> * 5<SUP>1</SUP> * ...


## GAP

Built-in function :

```gap
FactorsInt(2^67-1); 
# [ 193707721, 761838257287 ]
```

Or using the [http://www.gap-system.org/Manuals/pkg/factint/doc/chap0.html FactInt] package :

```gap
FactInt(2^67-1);    
# [ [ 193707721, 761838257287 ], [  ] ]
```



## Go


```go
package main

import (
	"fmt"
	"math/big"
)

var (
	ZERO = big.NewInt(0)
	ONE  = big.NewInt(1)
)

func Primes(n *big.Int) []*big.Int {
	res := []*big.Int{}
	mod, div := new(big.Int), new(big.Int)
	for i := big.NewInt(2); i.Cmp(n) != 1; {
		div.DivMod(n, i, mod)
		for mod.Cmp(ZERO) == 0 {
			res = append(res, new(big.Int).Set(i))
			n.Set(div)
			div.DivMod(n, i, mod)
		}
		i.Add(i, ONE)
	}
	return res
}

func main() {
	vals := []int64{
		1 << 31,
		1234567,
		333333,
		987653,
		2 * 3 * 5 * 7 * 11 * 13 * 17,
	}
	for _, v := range vals {
		fmt.Println(v, "->", Primes(big.NewInt(v)))
	}
}
```

{{out}}

```txt
2147483648 -> [2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]
1234567 -> [127 9721]
333333 -> [3 3 7 11 13 37]
987653 -> [29 34057]
510510 -> [2 3 5 7 11 13 17]
```



## Groovy

This solution uses the fact that a given factor must be prime 
if no smaller factor divides it evenly, 
so it does not require an "isPrime-like function", 
assumed or otherwise.

```groovy
def factorize = { long target -> 
 
    if (target == 1) return [1L]
 
    if (target < 4) return [1L, target]
 
    def targetSqrt = Math.sqrt(target)
    def lowfactors = (2L..targetSqrt).findAll { (target % it) == 0 }
    if (lowfactors == []) return [1L, target]
    def nhalf = lowfactors.size() - ((lowfactors[-1]**2 == target) ? 1 : 0)
 
    [1] + lowfactors + (0..<nhalf).collect { target.intdiv(lowfactors[it]) }.reverse() + [target]
}

def decomposePrimes = { target ->
    def factors = factorize(target) - [1]
    def primeFactors = []
    factors.eachWithIndex { f, i ->
        if (i==0 || factors[0..<i].every {f % it != 0}) {
            primeFactors << f
            def pfPower = f*f
            while (target % pfPower == 0) {
                primeFactors << f
                pfPower *= f
            }
        }
    }
    primeFactors
}
```


{{out|Test #1}}

```groovy
((1..30) + [97*4, 1000, 1024, 333333]).each { println ([number:it, primes:decomposePrimes(it)]) }
```


{{out|Output #1}}
<pre style="height:30ex;overflow:scroll;">[number:1, primes:[]]
[number:2, primes:[2]]
[number:3, primes:[3]]
[number:4, primes:[2, 2]]
[number:5, primes:[5]]
[number:6, primes:[2, 3]]
[number:7, primes:[7]]
[number:8, primes:[2, 2, 2]]
[number:9, primes:[3, 3]]
[number:10, primes:[2, 5]]
[number:11, primes:[11]]
[number:12, primes:[2, 2, 3]]
[number:13, primes:[13]]
[number:14, primes:[2, 7]]
[number:15, primes:[3, 5]]
[number:16, primes:[2, 2, 2, 2]]
[number:17, primes:[17]]
[number:18, primes:[2, 3, 3]]
[number:19, primes:[19]]
[number:20, primes:[2, 2, 5]]
[number:21, primes:[3, 7]]
[number:22, primes:[2, 11]]
[number:23, primes:[23]]
[number:24, primes:[2, 2, 2, 3]]
[number:25, primes:[5, 5]]
[number:26, primes:[2, 13]]
[number:27, primes:[3, 3, 3]]
[number:28, primes:[2, 2, 7]]
[number:29, primes:[29]]
[number:30, primes:[2, 3, 5]]
[number:388, primes:[2, 2, 97]]
[number:1000, primes:[2, 2, 2, 5, 5, 5]]
[number:1024, primes:[2, 2, 2, 2, 2, 2, 2, 2, 2, 2]]
[number:333333, primes:[3, 3, 7, 11, 13, 37]]
```


{{out|Test #2}}

```groovy
def isPrime = {factorize(it).size() == 2}
(1..60).step(2).findAll(isPrime).each { println ([number:"2**${it}-1", value:2**it-1, primes:decomposePrimes(2**it-1)]) }
```


{{out|Output #2}}
<pre style="height:30ex;overflow:scroll;">[number:2**3-1, value:7, primes:[7]]
[number:2**5-1, value:31, primes:[31]]
[number:2**7-1, value:127, primes:[127]]
[number:2**11-1, value:2047, primes:[23, 89]]
[number:2**13-1, value:8191, primes:[8191]]
[number:2**17-1, value:131071, primes:[131071]]
[number:2**19-1, value:524287, primes:[524287]]
[number:2**23-1, value:8388607, primes:[47, 178481]]
[number:2**29-1, value:536870911, primes:[233, 1103, 2089]]
[number:2**31-1, value:2147483647, primes:[2147483647]]
[number:2**37-1, value:137438953471, primes:[223, 616318177]]
[number:2**41-1, value:2199023255551, primes:[13367, 164511353]]
[number:2**43-1, value:8796093022207, primes:[431, 9719, 2099863]]
[number:2**47-1, value:140737488355327, primes:[2351, 4513, 13264529]]
[number:2**53-1, value:9007199254740991, primes:[6361, 69431, 20394401]]
[number:2**59-1, value:576460752303423487, primes:[179951, 3203431780337]]
```


Perhaps a more sophisticated algorithm is in order. It took well over 1 hour to calculate the last three decompositions using this solution.


## Haskell

The task description hints at using the <code>isPrime</code> function from the [[Primality by trial division#Haskell|trial division]] task:


```haskell
factorize n = [ d | p <- [2..n], isPrime p, d <- divs n p ]
           -- [2..n] >>= (\p-> [p|isPrime p]) >>= divs n
    where
    divs n p | rem n p == 0 = p : divs (quot n p) p 
             | otherwise    = []
```


but it is not very efficient, to put it mildly. Inlining and fusing gets us the progressively more optimized

```haskell
import Data.Maybe (listToMaybe)
import Data.List (unfoldr)

factorize :: Integer -> [Integer]
factorize n 
  = unfoldr (\n     -> listToMaybe [(x, div n x)      | x <- [2..n], mod n x==0]) n
  = unfoldr (\(d,n) -> listToMaybe [(x, (x, div n x)) | x <- [d..n], mod n x==0]) (2,n)
  = unfoldr (\(d,n) -> listToMaybe [(x, (x, div n x)) | x <- 
                    takeWhile ((<=n).(^2)) [d..] ++ [n|n>1], mod n x==0]) (2,n)
  = unfoldr (\(ds,n) -> listToMaybe [(x, (dropWhile (< x) ds, div n x)) | n>1, x <-
                    takeWhile ((<=n).(^2)) ds ++ [n|n>1], mod n x==0]) (primesList,n)
```


The library function <code>listToMaybe</code> gets at most one element from its list argument. The last variant can be written as the optimal


```haskell
factorize n = divs n primesList
     where
     divs n ds@(d:t) | d*d > n    = [n | n > 1]
                     | r == 0     =  d : divs q ds
                     | otherwise  =      divs n t
            where  (q,r) = quotRem n d
```


See [[Sieve of Eratosthenes]] or [[Primality by trial division]] for a source of primes to use with this function. 
Actually as some other entries notice, with any ascending order list containing all primes (e.g. <code>2:[3,5..]</code>) used in place of <code>primesList</code>, the factors found by this function are guaranteed to be prime, so no separate testing for primality is strictly needed; however using just primes is more efficient, if we already have them.

{{out}}

```txt
λ> mapM_ (print . factorize) $ take 11 [123123451..]
[11,41,273001]
[2,2,17,53,127,269]
[3,229,277,647]
[2,61561727]
[5,7,13,270601]
[2,2,2,2,2,2,2,2,3,3,3,47,379]
[37,109,30529]
[2,19,97,33403]
[3,3167,12959]
[2,2,5,6156173]
[123123461]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
factors := primedecomp(2^43-1)   # a big int
end

procedure primedecomp(n)         #: return a list of factors
local F,o,x
F := []

every writes(o,n|(x := genfactors(n))) do {
   \o := "*"
   /o := "="
   put(F,x)   # build a list of factors to satisfy the task
   }
write()
return F
end

link factors
```


{{libheader|Icon Programming Library}} [http://www.cs.arizona.edu/icon/library/src/procs/factors.icn Uses genfactors and prime from factors] 

Sample Output showing factors of a large integer:

```txt
8796093022207=431*9719*2099863
```



## J


```j>q:</lang


{{out|Example use}}

```j
   q: 3684
2 2 3 307
```


and, more elaborately:


```j
   _1+2^128x
340282366920938463463374607431768211455
   q: _1+2^128x
3 5 17 257 641 65537 274177 6700417 67280421310721
   */ q: _1+2^128x
340282366920938463463374607431768211455
```



## Java

{{works with|Java|1.5+}}
This is a version for arbitrary-precision integers 
which assumes the existence of a function with the signature:

```java
public boolean prime(BigInteger i);
```

You will need to import java.util.List, java.util.LinkedList, and java.math.BigInteger.

```java>public static List<BigInteger
 primeFactorBig(BigInteger a){
    List<BigInteger> ans = new LinkedList<BigInteger>();
    //loop until we test the number itself or the number is 1
    for (BigInteger i = BigInteger.valueOf(2); i.compareTo(a) <= 0 && !a.equals(BigInteger.ONE);
         i = i.add(BigInteger.ONE)){
        while (a.remainder(i).equals(BigInteger.ZERO) && prime(i)) { //if we have a prime factor
            ans.add(i); //put it in the list
            a = a.divide(i); //factor it out of the number
        }
    }
    return ans;
}
```


Alternate version, optimised to be faster.

```java
private static final BigInteger two = BigInteger.valueOf(2);

public List<BigInteger> primeDecomp(BigInteger a) {
    // impossible for values lower than 2
    if (a.compareTo(two) < 0) {
        return null; 
    }

    //quickly handle even values
    List<BigInteger> result = new ArrayList<BigInteger>();
    while (a.and(BigInteger.ONE).equals(BigInteger.ZERO)) {
        a = a.shiftRight(1);
        result.add(two);
    }

    //left with odd values
    if (!a.equals(BigInteger.ONE)) {
        BigInteger b = BigInteger.valueOf(3);
        while (b.compareTo(a) < 0) {
            if (b.isProbablePrime(10)) {
                BigInteger[] dr = a.divideAndRemainder(b);
                if (dr[1].equals(BigInteger.ZERO)) {
                    result.add(b);
                    a = dr[0];
                }
            }
            b = b.add(two);
        }
        result.add(b); //b will always be prime here...
    }
    return result;
}
```


Another alternate version designed to make fewer modular calculations:

```java

private static final BigInteger TWO = BigInteger.valueOf(2);
private static final BigInteger THREE = BigInteger.valueOf(3);
private static final BigInteger FIVE = BigInteger.valueOf(5);

public static ArrayList<BigInteger> primeDecomp(BigInteger n){
	if(n.compareTo(TWO) < 0) return null;
	ArrayList<BigInteger> factors = new ArrayList<BigInteger>();
	
	// handle even values
	while(n.and(BigInteger.ONE).equals(BigInteger.ZERO)){
		n = n.shiftRight(1);
		factors.add(TWO);
	}
	
	// handle values divisible by three
	while(n.mod(THREE).equals(BigInteger.ZERO)){
		factors.add(THREE);
		n = n.divide(THREE);
	}
	
	// handle values divisible by five
	while(n.mod(FIVE).equals(BigInteger.ZERO)){
		factors.add(FIVE);
		n = n.divide(FIVE);
	}
	
	// much like how we can skip multiples of two, we can also skip
	// multiples of three and multiples of five. This increment array
	// helps us to accomplish that
	int[] pattern = {4,2,4,2,4,6,2,6};
	int pattern_index = 0;
	BigInteger current_test = BigInteger.valueOf(7);
	while(!n.equals(BigInteger.ONE)){
		while(n.mod(current_test).equals(BigInteger.ZERO)){
			factors.add(current_test);
			n = n.divide(current_test);
		}
		current_test = current_test.add(BigInteger.valueOf(pattern[pattern_index]));
		pattern_index = (pattern_index + 1) & 7;
	}
	
	return factors;
}

```

{{trans|C#}}
Simple but very inefficient method, 
because it will test divisibility of all numbers from 2 to max prime factor.  
When decomposing a large prime number this will take O(n) trial divisions instead of more common O(log n).

```java>public static List<BigInteger
 primeFactorBig(BigInteger a){
    List<BigInteger> ans = new LinkedList<BigInteger>();

    for(BigInteger divisor = BigInteger.valueOf(2);
    	a.compareTo(ONE) > 0; divisor = divisor.add(ONE))
		while(a.mod(divisor).equals(ZERO)){
			 ans.add(divisor);
			 a = a.divide(divisor);
		}
    return ans;
}
```



## JavaScript

This code uses the BigInteger Library [http://xenon.stanford.edu/~tjw/jsbn/jsbn.js jsbn] and [http://xenon.stanford.edu/~tjw/jsbn/jsbn2.js jsbn2]

```javascript
function run_factorize(input, output) {
    var n = new BigInteger(input.value, 10);
    var TWO = new BigInteger("2", 10);
    var divisor = new BigInteger("3", 10);
    var prod = false;

    if (n.compareTo(TWO) < 0) 
        return; 

    output.value = "";

    while (true) {
        var qr = n.divideAndRemainder(TWO);
        if (qr[1].equals(BigInteger.ZERO)) {
            if (prod) 
                output.value += "*"; 
            else 
                prod = true; 
            output.value += "2";
            n = qr[0];
        }
        else 
            break; 
    }

    while (!n.equals(BigInteger.ONE)) {
        var qr = n.divideAndRemainder(divisor);
        if (qr[1].equals(BigInteger.ZERO)) {
            if (prod) 
                output.value += "*"; 
            else 
                prod = true; 
            output.value += divisor;
            n = qr[0];
        }
        else 
            divisor = divisor.add(TWO); 
    }
}
```


Without any library.

```javascript
function run_factorize(n) {
    if (n <= 3)
        return [n];

    var ans = [];
    var done = false;
    while (!done) {
        if (n % 2 === 0) {
            ans.push(2);
            n /= 2;
            continue;
        }
        if (n % 3 === 0) {
            ans.push(3);
            n /= 3;
            continue;
        }
        if (n === 1)
            return ans;
        var sr = Math.sqrt(n);
        done = true;
        // try to divide the checked number by all numbers till its square root.
        for (var i = 6; i <= (sr + 6); i += 6) {
            if (n % (i - 1) === 0) { // is n divisible by i-1?
                ans.push((i - 1));
                n /= (i - 1);
                done = false;
                break;
            }
            if (n % (i + 1) === 0) { // is n divisible by i+1?
                ans.push((i + 1));
                n /= (i + 1);
                done = false;
                break;
            }
        }
    }
    ans.push(n);
    return ans;
}
```


TDD using Jasmine

PrimeFactors.js

```javascript
function factors(n) {
  if (!n || n < 2)
    return [];

  var f = [];
  for (var i = 2; i <= n; i++){
    while (n % i === 0){
      f.push(i);
      n /= i;
    }
  }

  return f;
};

```


SpecPrimeFactors.js (with tag for Chutzpah)

```javascript
/// <reference path="PrimeFactors.js" />

describe("Prime Factors", function() {
  it("Given nothing, empty is returned", function() {
    expect(factors()).toEqual([]);
  });

  it("Given 1, empty is returned", function() {
    expect(factors(1)).toEqual([]);
  });

  it("Given 2, 2 is returned", function() {
    expect(factors(2)).toEqual([2]);
  });

  it("Given 3, 3 is returned", function() {
    expect(factors(3)).toEqual([3]);
  });

  it("Given 4, 2 and 2 is returned", function() {
    expect(factors(4)).toEqual([2, 2]);
  });

  it("Given 5, 5 is returned", function() {
    expect(factors(5)).toEqual([5]);
  });

  it("Given 6, 2 and 3 is returned", function() {
    expect(factors(6)).toEqual([2, 3]);
  });

  it("Given 7, 7 is returned", function() {
    expect(factors(7)).toEqual([7]);
  });

  it("Given 8; 2, 2, and 2 is returned", function() {
    expect(factors(8)).toEqual([2, 2, 2]);
  });

  it("Given a large number, many primes factors are returned", function() {
    expect(factors(2*2*2*3*3*7*11*17))
      .toEqual([2, 2, 2, 3, 3, 7, 11, 17]);
  });

  it("Given a large prime number, that number is returned", function() {
    expect(factors(997)).toEqual([997]);
  });
});

```



## jq

{{works with|jq|1.4}}

"factors" as defined below emits a stream of all the prime factors of the input integer.
The implementation is compact, fast and highly space-efficient:
no space is required to store the primes or factors already computed, 
there is no reliance on an "is_prime" function, and square roots are only computed if needed.

The economy comes about through the use of the builtin filter recurse/1,
and the use of the state vector: [p, n, valid, sqrt],
where p is the candidate factor, n is the number still to be factored,
valid is a flag, and sqrt is either null or the square root of n.

The caveat is that the program uses jq's builtin arithmetic operations.  Since
jq currently uses IEEE 754 64-bit numbers, the following program will only be
reliable for integers up to and including 9,007,199,254,740,992 (2^53).  However, "factors"
could be easily modified to work with a "BigInt" library for jq, such as [https://gist.github.com/pkoppstein/d06a123f30c033195841 BigInt.jq].

```jq
def factors:
  . as $in 
  | [2, $in, false]
  | recurse( .[0] as $p |.[1] as $q | .[2] as $valid | .[3] as $s
             | if $q == 1        then empty
	       elif $q % $p == 0 then [$p, $q/$p, true]
               elif $p == 2      then [3, $q, false, $s]
               else
	         ($s // ($q | sqrt)) as $s
	         | if $p + 2 <= $s then [$p + 2, $q, false, $s]
      	           else [$q, 1, true]
		   end
	       end )
   | if .[2] then .[0] else empty end ;
```
 
'''Examples''':

```jq
[9007199254740992 | factors] | length
#=> 53

# 2**29-1 = 536870911
[ 536870911 | factors ]

#=> [233,1103,2089]
```



## Julia

using package Primes.jl:

```julia

julia> Pkg.add("Primes")
julia> factor(8796093022207)
[9719=>1,431=>1,2099863=>1]

```

(The <code>factor</code> function returns a dictionary 
whose keys are the factors and whose values are the multiplicity of each factor.)


## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

val bigTwo   = BigInteger.valueOf(2L)
val bigThree = BigInteger.valueOf(3L)

fun getPrimeFactors(n: BigInteger): MutableList<BigInteger> {
    val factors = mutableListOf<BigInteger>()
    if (n < bigTwo) return factors
    if (n.isProbablePrime(20)) {
        factors.add(n)
        return factors
    }
    var factor = bigTwo
    var nn = n
    while (true) {
        if (nn % factor == BigInteger.ZERO) {
            factors.add(factor)
            nn /= factor
            if (nn == BigInteger.ONE) return factors
            if (nn.isProbablePrime(20)) factor = nn
        }
        else if (factor >= bigThree) factor += bigTwo 
        else factor = bigThree
    }
}

fun main(args: Array<String>) {
    val primes = intArrayOf(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
    for (prime in primes) {
        val bigPow2 = bigTwo.pow(prime) - BigInteger.ONE
        println("2^${"%2d".format(prime)} - 1 = ${bigPow2.toString().padEnd(30)} => ${getPrimeFactors(bigPow2)}")
    }
}
```


{{out}}

```txt

2^ 2 - 1 = 3                              => [3]
2^ 3 - 1 = 7                              => [7]
2^ 5 - 1 = 31                             => [31]
2^ 7 - 1 = 127                            => [127]
2^11 - 1 = 2047                           => [23, 89]
2^13 - 1 = 8191                           => [8191]
2^17 - 1 = 131071                         => [131071]
2^19 - 1 = 524287                         => [524287]
2^23 - 1 = 8388607                        => [47, 178481]
2^29 - 1 = 536870911                      => [233, 1103, 2089]
2^31 - 1 = 2147483647                     => [2147483647]
2^37 - 1 = 137438953471                   => [223, 616318177]
2^41 - 1 = 2199023255551                  => [13367, 164511353]
2^43 - 1 = 8796093022207                  => [431, 9719, 2099863]
2^47 - 1 = 140737488355327                => [2351, 4513, 13264529]
2^53 - 1 = 9007199254740991               => [6361, 69431, 20394401]
2^59 - 1 = 576460752303423487             => [179951, 3203431780337]
2^61 - 1 = 2305843009213693951            => [2305843009213693951]
2^67 - 1 = 147573952589676412927          => [193707721, 761838257287]
2^71 - 1 = 2361183241434822606847         => [228479, 48544121, 212885833]
2^73 - 1 = 9444732965739290427391         => [439, 2298041, 9361973132609]
2^79 - 1 = 604462909807314587353087       => [2687, 202029703, 1113491139767]
2^83 - 1 = 9671406556917033397649407      => [167, 57912614113275649087721]
2^89 - 1 = 618970019642690137449562111    => [618970019642690137449562111]
2^97 - 1 = 158456325028528675187087900671 => [11447, 13842607235828485645766393]

```



## LFE



```lisp

(defun factors (n)
  (factors n 2 '()))

(defun factors
  ((1 _ acc)
    acc)
  ((n k acc) (when (== 0 (rem n k)))
    (factors (div n k) k (cons k acc)))
  ((n k acc)
    (factors n (+ k 1) acc)))

```



## Lingo


```lingo
-- Returns list of prime factors for given number.
-- To overcome the limits of integers (signed 32-bit in Lingo),
-- the number can be specified as float (which works up to 2^53).
-- For the same reason, values in returned list are floats, not integers.
on getPrimeFactors (n)
  f = []
  f.sort()
  c = sqrt(n)
  i = 1.0
  repeat while TRUE
    i=i+1
    if i>c then exit repeat
    check = n/i
    if bitOr(check,0)=check then
      f.add(i)
      n = check
      c = sqrt(n)
      i = 1.0
    end if
  end repeat
  f.add(n)
  return f
end
```


```lingo
put getPrimeFactors(12)
-- [2.0000, 2.0000, 3.0000]

-- print floats without fractional digits
the floatPrecision=0

put getPrimeFactors(12)
-- [2, 2, 3]

put getPrimeFactors(1125899906842623.0)
-- [3, 251, 601, 4051, 614141]
```



## Logo


```logo
to decompose :n [:p 2]
  if :p*:p > :n [output (list :n)]
  if less? 0 modulo :n :p [output (decompose :n bitor 1 :p+1)]
  output fput :p (decompose :n/:p :p)
end
```



## Lua

The code of the used auxiliary function "IsPrime(n)" 
is located at [[Primality by trial division#Lua]]


```lua
function PrimeDecomposition( n )
    local f = {}
    
    if IsPrime( n ) then
        f[1] = n
        return f
    end

    local i = 2
    repeat
        while n % i == 0 do
            f[#f+1] = i
            n = n / i
        end
        
        repeat
            i = i + 1
        until IsPrime( i )       
    until n == 1
    
    return f
end
```


## M2000 Interpreter


```M2000 Interpreter

Module  Prime_decomposition    {
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
      decompose=lambda IsPrime (n as decimal) -> {
            Inventory queue Factors
            {
                 k=2@
                 While frac(n/k)=0 {
                 n/=k
                      Append Factors, k
                 }
                 if n=1 then exit
                 k++ 
                 While frac(n/k)=0 {
                 n/=k
                        Append Factors, k
                 }   
                 if n=1 then exit
                 {
                 k+=2
                 while not isprime(k) {k+=2}
                 While frac(n/k)=0 {
                 n/=k
                        Append Factors, k
                 }
                      if n=1 then exit
                      loop
                 }             
            }
            =Factors
      }
      Data 10, 100, 12, 144, 496, 1212454
      while not empty {
        Print Decompose(Number)
      }
}
Prime_decomposition

```



## Maple

Maple has two commands for integer factorization: '''ifactor''', 
which returns results in a form resembling textbook presentation 
and '''ifactors''', which returns a list of two-element lists 
of prime factors and their multiplicities:


```Maple>
 ifactor(1337);
                                   (7)  (191)

```


```Maple>
 ifactors(1337);
                            [1, [[7, 1], [191, 1]]]

```



## Mathematica

Bare built-in function does:

```Mathematica
 FactorInteger[2016] => {{2, 5}, {3, 2}, {7, 1}}
```


Read as: 2 to the power 5 times 3 squared times 7 (to the power 1). 
To show them nicely we could use the following functions:

```Mathematica
supscript[x_,y_]:=If[y==1,x,Superscript[x,y]]
ShowPrimeDecomposition[input_Integer]:=Print@@{input," = ",Sequence@@Riffle[supscript@@@FactorInteger[input]," "]}
```


Example for small prime:

```Mathematica
 ShowPrimeDecomposition[1337]
```

gives:

```Mathematica> 1337 = 7 191</lang


Examples for large primes:

```Mathematica
 Table[AbsoluteTiming[ShowPrimeDecomposition[2^a-1]]//Print[#[[1]]," sec"]&,{a,50,150,10}];
```

gives back:

```Mathematica
1125899906842623 = 3 11 31 251 601 1801 4051
0.000231 sec
1152921504606846975 = 3^2 5^2 7 11 13 31 41 61 151 331 1321
0.000146 sec
1180591620717411303423 = 3 11 31 43 71 127 281 86171 122921
0.001008 sec
1208925819614629174706175 = 3 5^2 11 17 31 41 257 61681 4278255361
0.000340 sec
1237940039285380274899124223 = 3^3 7 11 19 31 73 151 331 631 23311 18837001
0.000192 sec
1267650600228229401496703205375 = 3 5^3 11 31 41 101 251 601 1801 4051 8101 268501
0.000156 sec
1298074214633706907132624082305023 = 3 11^2 23 31 89 683 881 2971 3191 201961 48912491
0.001389 sec
1329227995784915872903807060280344575 = 3^2 5^2 7 11 13 17 31 41 61 151 241 331 1321 61681 4562284561
0.000374 sec
1361129467683753853853498429727072845823 = 3 11 31 131 2731 8191 409891 7623851 145295143558111
0.024249 sec
1393796574908163946345982392040522594123775 = 3 5^2 11 29 31 41 43 71 113 127 281 86171 122921 7416361 47392381
0.009419 sec
1427247692705959881058285969449495136382746623 = 3^2 7 11 31 151 251 331 601 1801 4051 100801 10567201 1133836730401
0.007705 sec
```



## MATLAB


```Matlab
function [outputPrimeDecomposition] = primedecomposition(inputValue)
   outputPrimeDecomposition = factor(inputValue);
```



## Maxima

Using the built-in function:

```maxima
(%i1) display2d: false$ /* disable rendering exponents as superscripts */
(%i2) factor(2016);
(%o2) 2^5*3^2*7

```

Using the underlying language:

```maxima
prime_dec(n) := flatten(create_list(makelist(first(a), second(a)), a, ifactors(n)))$

/* or, slighlty more "functional" */
prime_dec(n) := flatten(map(lambda([a], apply(makelist, a)), ifactors(n)))$

prime_dec(2^4*3^5*5*7^2);
/* [2, 2, 2, 2, 3, 3, 3, 3, 3, 5, 7, 7] */
```



## MUMPS


```MUMPS
ERATO1(HI)
 SET HI=HI\1
 KILL ERATO1 ;Don't make it new - we want it to remain after the quit
 NEW I,J,P
 FOR I=2:1:(HI**.5)\1 DO
 .FOR J=I*I:I:HI DO
 ..SET P(J)=1 ;$SELECT($DATA(P(J))#10:P(J)+1,1:1)
 ;WRITE !,"Prime numbers between 2 and ",HI,": "
 FOR I=2:1:HI DO
 .S:'$DATA(P(I)) ERATO1(I)=I ;WRITE $SELECT((I<3):"",1:", "),I
 KILL I,J,P
 QUIT
PRIMDECO(N)
 ;Returns its results in the string PRIMDECO
 ;Kill that before the first call to this recursive function
 QUIT:N<=1
 IF $D(PRIMDECO)=1 SET PRIMDECO="" D ERATO1(N)
 SET N=N\1,I=0
 FOR  SET I=$O(ERATO1(I)) Q:+I<1  Q:'(N#I)
 IF I>1 SET PRIMDECO=$S($L(PRIMDECO)>0:PRIMDECO_"^",1:"")_I D PRIMDECO(N/I)
 ;that is, if I is a factor of N, add it to the string
 QUIT
```

{{out|Usage}}

```txt
USER>K ERATO1,PRIMDECO D PRIMDECO^ROSETTA(31415) W PRIMDECO
5^61^103
USER>K ERATO,PRIMDECO D PRIMDECO^ROSETTA(31318) W PRIMDECO
2^7^2237
USER>K ERATO,PRIMDECO D PRIMDECO^ROSETTA(34) W PRIMDECO
2^17
USER>K ERATO,PRIMDECO D PRIMDECO^ROSETTA(68) W PRIMDECO
2^2^17
USER>K ERATO,PRIMDECO D PRIMDECO^ROSETTA(7) W PRIMDECO
7
USER>K ERATO,PRIMDECO D PRIMDECO^ROSETTA(777) W PRIMDECO
3^7^37
```



## Nim

Based on python solution:

```nim
import strutils, math, sequtils, times
       
proc getStep(n: int64) : int64 {.inline.} =
   result = 1 + n*4 - int64(n /% 2)*2

proc primeFac(n: int64): seq[int64] =    
    var res: seq[int64] = @[]
    var maxq = int64(floor(sqrt(float(n))))
    var d = 1
    var q: int64 = (n %% 2) and 2 or 3    # either 2 or 3, alternating
    while (q <= maxq) and ((n %% q) != 0):
        q = getStep(d)
        d += 1
    if q <= maxq:        
        var q1: seq[int64] = primeFac(n /% q)
        var q2: seq[int64] = primeFac(q)
        res = concat(q2, q1, res)
    else: 
        res.add(n)    
    result = res

var is_prime: seq[Bool] = @[]
is_prime.add(False)
is_prime.add(False)
    
iterator primes(limit: int): int =
    for n in high(is_prime) .. limit+2: is_prime.add(True)    
    for n in 2 .. limit + 1:
        if is_prime[n]:
            yield n
            for i in countup((n *% n), limit+1, n): # start at ``n`` squared
                try:
                    is_prime[i] = False
                except EInvalidIndex: break
   
# Example: calculate factors of Mersenne numbers to M59 #

for m in primes(59):
    var p = int64(pow(2.0,float(m)) - 1) 
    write(stdout,"2**$1-1 = $2, with factors: " % [$m, $p] )
    var start = cpuTime()
    var f = primeFac(p)
    for factor in f:
        write(stdout, factor)
        write(stdout, ", ")
        FlushFile(stdout)
    writeln(stdout, "=> $#ms" % $int(1000*(cpuTime()-start)) )
```

{{out}}
compiled with options     -x:off -opt:speed

```txt
2**2-1 = 3, with factors: 3, => 0ms
2**3-1 = 7, with factors: 7, => 0ms
2**5-1 = 31, with factors: 31, => 0ms
2**7-1 = 127, with factors: 127, => 0ms
2**11-1 = 2047, with factors: 23, 89, => 0ms
2**13-1 = 8191, with factors: 8191, => 0ms
2**17-1 = 131071, with factors: 131071, => 0ms
2**19-1 = 524287, with factors: 524287, => 0ms
2**23-1 = 8388607, with factors: 47, 178481, => 0ms
2**29-1 = 536870911, with factors: 233, 1103, 2089, => 0ms
2**31-1 = 2147483647, with factors: 2147483647, => 0ms
2**37-1 = 137438953471, with factors: 223, 616318177, => 0ms
2**41-1 = 2199023255551, with factors: 13367, 164511353, => 0ms
2**43-1 = 8796093022207, with factors: 431, 9719, 2099863, => 0ms
2**47-1 = 140737488355327, with factors: 2351, 4513, 13264529, => 0ms
2**53-1 = 9007199254740991, with factors: 6361, 69431, 20394401, => 0ms
2**59-1 = 576460752303423487, with factors: 179951, 3203431780337, => 40ms
```



## OCaml


```ocaml
open Big_int;;

let prime_decomposition x =
  let rec inner c p =
    if lt_big_int p (square_big_int c) then
      [p]
    else if eq_big_int (mod_big_int p c) zero_big_int then
      c :: inner c (div_big_int p c)
    else
      inner (succ_big_int c) p
  in
  inner (succ_big_int (succ_big_int zero_big_int)) x;;
```



## Octave


```octave
r = factor(120202039393)
```



## Oforth


Oforth handles aribitrary precision integers.


```Oforth
: factors(n) 	// ( aInteger -- aList )
| k p |
   ListBuffer new
   2 ->k
   n nsqrt ->p
   while( k p <= ) [
      n k /mod swap ifZero: [ 
         dup ->n nsqrt ->p 
         k over add continue
         ]
      drop k 1+ ->k
      ]
   n 1 > ifTrue: [ n over add ] 
   dup freeze ;
```


{{out}}

```txt

>2 128 pow 1 - dup println factors println
340282366920938463463374607431768211455
[3, 5, 17, 257, 641, 65537, 274177, 6700417, 67280421310721]
ok

```



## PARI/GP

GP normally returns factored integers as a matrix 
with the first column representing the primes 
and the second their exponents.  
Thus <code>factor(12)==[2,2;3,1]</code> is true.  
But it's simple enough to convert this to a vector with repetition:

```parigp
pd(n)={
  my(f=factor(n),v=f[,1]~);
  for(i=1,#v,
    while(f[i,2]--,
      v=concat(v,f[i,1])
    )
  );
  vecsort(v)
};
```



## Pascal


```pascal
Program PrimeDecomposition(output);

type
  DynArray = array of integer;

procedure findFactors(n: Int64; var d: DynArray);
  var
    divisor, next, rest: Int64;
    i: integer;
 begin
    i := 0;
    divisor := 2;
    next := 3;
    rest := n;
    while (rest <> 1) do
    begin
      while (rest mod divisor = 0) do
      begin
        setlength(d, i+1);
        d[i] := divisor;
        inc(i);
        rest := rest div divisor;
      end;
      divisor := next;
      next := next + 2;
    end;
  end;

var
  factors: DynArray;
  j: integer;

begin
  setlength(factors, 1);
  findFactors(1023*1024, factors);
  for j := low(factors) to high(factors) do
    writeln (factors[j]);
end.
```

{{out}}

```txt
% ./PrimeDecomposition
2
2
2
2
2
2
2
2
2
2
3
11
31
```


'''Optimization:'''


```pascal
Program PrimeDecomposition(output);

type
  DynArray = array of integer;

procedure findFactors(n: Int64; var d: DynArray);
  var
    divisor, next, rest: Int64;
    i: integer;
 begin
    i := 0;
    divisor := 2;
    next := 3;
    rest := n;
    while (rest <> 1) do
    begin
      while (rest mod divisor = 0) do
      begin
        setlength(d, i+1);
        d[i] := divisor;
        inc(i);
        rest := rest div divisor;
      end;
      divisor := next;
      next := next + 2;  // try only odd numbers
      // cut condition: avoid many useless iterations
      if (rest < divisor * divisor) then
        begin
          setlength(d, i+1);
          d[i] := rest;
          rest := 1;
        end;
    end;
  end;

var
  factors: DynArray;
  j: integer;

begin
  setlength(factors, 1);
  findFactors(1023*1024, factors);
  for j := low(factors) to high(factors) do
    writeln (factors[j]);
  readln;
end.
```



## Perl

These will work for large integers 
by adding the <tt>use bigint;</tt> clause.

===Trivial trial division (very slow)===

```perl
sub prime_factors {
	my ($n, $d, @out) = (shift, 1);
	while ($n > 1 && $d++) {
		$n /= $d, push @out, $d until $n % $d;
	}
	@out
}

print "@{[prime_factors(1001)]}\n";
```



### Better trial division

This is ''much'' faster than the trivial version above.

```perl
sub prime_factors {
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
```



### Modules

As usual, there are CPAN modules for this that will be much faster.  
These both take about 1 second to factor all Mersenne numbers from M_1 to M_150.
{{libheader|ntheory}}

```perl
use ntheory qw/factor forprimes/;
use bigint;

forprimes {
  my $p = 2 ** $_ - 1;
  print "2**$_-1: ", join(" ", factor($p)), "\n";
} 100, 150;
```

{{out}}

```txt

2^101-1: 7432339208719 341117531003194129
2^103-1: 2550183799 3976656429941438590393
2^107-1: 162259276829213363391578010288127
2^109-1: 745988807 870035986098720987332873
2^113-1: 3391 23279 65993 1868569 1066818132868207
2^127-1: 170141183460469231731687303715884105727
2^131-1: 263 10350794431055162386718619237468234569
2^137-1: 32032215596496435569 5439042183600204290159
2^139-1: 5625767248687 123876132205208335762278423601
2^149-1: 86656268566282183151 8235109336690846723986161

```


{{libheader|Math::Pari}}

```perl
use Math::Pari qw/:int factorint isprime/;

# Convert Math::Pari's format into simple vector
sub factor {
  my ($pn,$pc) = @{Math::Pari::factorint(shift)};
  map { ($pn->[$_]) x $pc->[$_] } 0 .. $#$pn;
}

for (100 .. 150) {
  next unless isprime($_);
  my $p = 2 ** $_ - 1;
  print "2^$_-1: ", join(" ", factor($p)), "\n";
}
```

With the same output.


## Perl 6


### Pure Perl 6

This is a pure perl 6 version that uses no outside libraries. It uses a variant of Pollard's rho factoring algorithm and is fairly performent when factoring numbers < 2⁸⁰; typically taking well under a second on an i7. It starts to slow down with larger numbers, but really bogs down factoring numbers that have more than 1 factor larger than about 2⁴⁰.


```perl6
sub prime-factors ( Int $n where * > 0 ) {
    return $n if $n.is-prime;
    return () if $n == 1;
    my $factor = find-factor( $n );
    sort flat ( $factor, $n div $factor ).map: *.&prime-factors;
}

sub find-factor ( Int $n, $constant = 1 ) {
    return 2 unless $n +& 1;
    if (my $gcd = $n gcd 6541380665835015) > 1 { # magic number: [*] primes 3 .. 43
        return $gcd if $gcd != $n
    }
    my $x      = 2;
    my $rho    = 1;
    my $factor = 1;
    while $factor == 1 {
        $rho = $rho +< 1;
        my $fixed = $x;
        my int $i = 0;
        while $i < $rho {
            $x = ( $x * $x + $constant ) % $n;
            $factor = ( $x - $fixed ) gcd $n;
            last if 1 < $factor;
            $i = $i + 1;
        }
    }
    $factor = find-factor( $n, $constant + 1 ) if $n == $factor;
    $factor;
}

.put for (2²⁹-1, 2⁴¹-1, 2⁵⁹-1, 2⁷¹-1, 2⁷⁹-1, 2⁹⁷-1, 2¹¹⁷-1, 2²⁴¹-1,
5465610891074107968111136514192945634873647594456118359804135903459867604844945580205745718497)\
.hyper(:1batch).map: -> $n {
    my $start = now;
   "factors of $n: ",
    prime-factors($n).join(' × '), " \t in ", (now - $start).fmt("%0.3f"), " sec."
}
```


{{out}}

```txt
factors of 536870911:  233 × 1103 × 2089  	 in  0.004  sec.
factors of 2199023255551:  13367 × 164511353  	 in  0.011  sec.
factors of 576460752303423487:  179951 × 3203431780337  	 in  0.023  sec.
factors of 2361183241434822606847:  228479 × 48544121 × 212885833  	 in  0.190  sec.
factors of 604462909807314587353087:  2687 × 202029703 × 1113491139767  	 in  0.294  sec.
factors of 158456325028528675187087900671:  11447 × 13842607235828485645766393  	 in  0.005  sec.
factors of 166153499473114484112975882535043071:  7 × 73 × 79 × 937 × 6553 × 8191 × 86113 × 121369 × 7830118297  	 in  0.022  sec.
factors of 3533694129556768659166595001485837031654967793751237916243212402585239551:  22000409 × 160619474372352289412737508720216839225805656328990879953332340439  	 in  0.085  sec.
factors of 5465610891074107968111136514192945634873647594456118359804135903459867604844945580205745718497:  165901 × 10424087 × 18830281 × 53204737 × 56402249 × 59663291 × 91931221 × 95174413 × 305293727939 × 444161842339 × 790130065009  	 in  28.427  sec.
```

There is a Perl 6 module available: Prime::Factor, that uses essentially this algorithm with some minor performance tweaks.


### External library

If you really need a speed boost, load the highly optimized Perl 5 ntheory module. It needs a little extra plumbing to deal with the lack of built-in big integer support, but for large number factoring the interface overhead is worth it.

```perl6
use Inline::Perl5;
my $p5 = Inline::Perl5.new();
$p5.use( 'ntheory' );

sub prime-factors ($i) {
    my &primes = $p5.run('sub { map { ntheory::todigitstring $_ } sort {$a <=> $b} ntheory::factor $_[0] }');
    primes("$i");
}

for 2²⁹-1, 2⁴¹-1, 2⁵⁹-1, 2⁷¹-1, 2⁷⁹-1, 2⁹⁷-1, 2¹¹⁷-1,
5465610891074107968111136514192945634873647594456118359804135903459867604844945580205745718497
 ->  $n {
    my $start = now;
    say "factors of $n: ",
    prime-factors($n).join(' × '), " \t in ", (now - $start).fmt("%0.3f"), " sec."
}
```

{{out}}

```txt
factors of 536870911: 233 × 1103 × 2089 	 in 0.001 sec.
factors of 2199023255551: 13367 × 164511353 	 in 0.001 sec.
factors of 576460752303423487: 179951 × 3203431780337 	 in 0.001 sec.
factors of 2361183241434822606847: 228479 × 48544121 × 212885833 	 in 0.012 sec.
factors of 604462909807314587353087: 2687 × 202029703 × 1113491139767 	 in 0.003 sec.
factors of 158456325028528675187087900671: 11447 × 13842607235828485645766393 	 in 0.001 sec.
factors of 166153499473114484112975882535043071: 7 × 73 × 79 × 937 × 6553 × 8191 × 86113 × 121369 × 7830118297 	 in 0.001 sec.
factors of 5465610891074107968111136514192945634873647594456118359804135903459867604844945580205745718497: 165901 × 10424087 × 18830281 × 53204737 × 56402249 × 59663291 × 91931221 × 95174413 × 305293727939 × 444161842339 × 790130065009 	 in 0.064 sec.

```



## Phix

{{libheader|mpfr}}

```Phix
include mpfr.e
atom t0 = time()
mpz z = mpz_init()
for i=1 to 17 do
    integer pi = get_prime(i)
    mpz_ui_pow_ui(z,2,pi)
    mpz_sub_ui(z,z,1)
    string zs = mpz_get_str(z),
           fs = mpz_factorstring(mpz_prime_factors(z,20000))
    if fs!=zs then zs &= " = "&fs end if
    printf(1,"2^%d-1 = %s\n",{pi,zs})
end for
string s = "600851475143"
mpz_set_str(z,s)
printf(1,"%s = %s\n",{s,mpz_factorstring(mpz_prime_factors(z,500))})
?elapsed(time()-t0)
```

{{out}}

```txt

2^2-1 = 3
2^3-1 = 7
2^5-1 = 31
2^7-1 = 127
2^11-1 = 2047 = 23*89
2^13-1 = 8191
2^17-1 = 131071
2^19-1 = 524287
2^23-1 = 8388607 = 47*178481
2^29-1 = 536870911 = 233*1103*2089
2^31-1 = 2147483647
2^37-1 = 137438953471 = 223*616318177
2^41-1 = 2199023255551 = 13367*164511353
2^43-1 = 8796093022207 = 431*9719*2099863
2^47-1 = 140737488355327 = 2351*4513*13264529
2^53-1 = 9007199254740991 = 6361*69431*20394401
2^59-1 = 576460752303423487 = 179951*3203431780337
600851475143 = 71*839*1471*6857
"0.1s"

```

Note that mpz_prime_factors() needs to be told how far to push things before giving up, but if 
pushed to (say) 20,000,000 primes, performance can suffer quite dramatically.

```Phix
t0 = time()
for i=18 to 25 do
    integer pi = get_prime(i)
    mpz_ui_pow_ui(z,2,pi)
    mpz_sub_ui(z,z,1)
    string zs = mpz_get_str(z),
           fs = mpz_factorstring(mpz_prime_factors(z,20000000))
    if fs!=zs then zs &= " = "&fs end if
    printf(1,"2^%d-1 = %s\n",{pi,zs})
end for
s = "100000000000000000037"
mpz_set_str(z,s)
printf(1,"%s = %s\n",{s,mpz_factorstring(mpz_prime_factors(z,5000000))})
?elapsed(time()-t0)
```

{{out}}

```txt

2^61-1 = 2305843009213693951
2^67-1 = 147573952589676412927 = 193707721*761838257287
2^71-1 = 2361183241434822606847 = 228479*48544121*212885833
2^73-1 = 9444732965739290427391 = 439*2298041*9361973132609
2^79-1 = 604462909807314587353087 = 2687*202029703*1113491139767
2^83-1 = 9671406556917033397649407 = 167*57912614113275649087721
2^89-1 = 618970019642690137449562111
2^97-1 = 158456325028528675187087900671 = 11447*13842607235828485645766393
100000000000000000037 = 31*821*59004541*66590107
"23.1s"

```

The default of 100 (as in get_prime(100) yields 541) is quite low, but fast (as is that 20,000 above):

```Phix
... -- <as above except for>
           fs = mpz_factorstring(mpz_prime_factors(z))
...
printf(1,"%s = %s\n",{s,mpz_factorstring(mpz_prime_factors(z))})
...
```

{{out}}

```txt

2^61-1 = 2305843009213693951
2^67-1 = 147573952589676412927
2^71-1 = 2361183241434822606847
2^73-1 = 9444732965739290427391 = 439*21514198099633918969
2^79-1 = 604462909807314587353087
2^83-1 = 9671406556917033397649407 = 167*57912614113275649087721
2^89-1 = 618970019642690137449562111
2^97-1 = 158456325028528675187087900671
100000000000000000037 = 31*3225806451612903227
"0.1s"

```

Obviously, were you not actually going to make any use of factors>541, then that's all you'd need.


## PicoLisp

The following solution generates a sequence of "trial divisors" (2 3 5 7 11 13
17 19 23 29 31 37 ..), as described by Donald E. Knuth, "The Art of Computer
Programming", Vol.2, p.365.

```PicoLisp
(de factor (N)
   (make
      (let (D 2  L (1 2 2 . (4 2 4 2 4 6 2 6 .))  M (sqrt N))
         (while (>= M D)
            (if (=0 (% N D))
               (setq M (sqrt (setq N (/ N (link D)))))
               (inc 'D (pop 'L)) ) )
         (link N) ) ) )

(factor 1361129467683753853853498429727072845823)
```

{{out}}

```txt
-> (3 11 31 131 2731 8191 409891 7623851 145295143558111)
```



## PL/I


```pli

test: procedure options (main, reorder);
   declare (n, i) fixed binary (31);

   get list (n);
   put edit ( n, '[' ) (x(1), a);
restart:
   if is_prime(n) then
      do;
         put edit (trim(n), ']' ) (x(1), a);
         stop;
      end;
   do i = n/2 to 2 by -1;
      if is_prime(i) then
         if (mod(n, i) = 0) then
            do;
               put edit ( trim(i) ) (x(1), a);
               n = n / i;
               go to restart;
            end;
   end;
   put edit ( ' ]' ) (a);

is_prime: procedure (n) options (reorder) returns (bit(1));
   declare n fixed binary (31);
   declare i fixed binary (31);

   if n < 2 then return ('0'b);
   if n = 2 then return ('1'b);
   if mod(n, 2) = 0 then return ('0'b);

   do i = 3 to sqrt(n) by 2;
      if mod(n, i) = 0 then return ('0'b);
   end;
   return ('1'b);
end is_prime;

end test;

```

{{out|Results from various runs}}

```txt

        1234567 [ 9721 127 ]
          32768 [ 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ]
             99 [ 11 3 3 ]
        9876543 [ 14503 227 3 ]
            100 [ 5 5 2 2 ]
        9999999 [ 4649 239 3 3 ]
           5040 [ 7 5 3 3 2 2 2 2 ]

```


## PowerShell


```PowerShell

function eratosthenes ($n) {
    if($n -gt 1){
        $prime = @(1..($n+1) | foreach{$true})
        $prime[1] = $false
        $m = [Math]::Floor([Math]::Sqrt($n))
        function multiple($i) {
            for($j = $i*$i; $j -le $n; $j += $i) {
                $prime[$j] = $false
            }
        }
        multiple 2
        for($i = 3; $i -le $m; $i += 2) {
            if($prime[$i]) {multiple $i}
        }
        1..$n | where{$prime[$_]}
    } else {
        Write-Error "$n is not greater than 1"
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
"$(prime-decomposition  12)"
"$(prime-decomposition  100)"

```

<b>Output:</b>

```txt

2 2 3
2 2 5 5

```



## Prolog


```Prolog
prime_decomp(N, L) :-
	SN is sqrt(N),
	prime_decomp_1(N, SN, 2, [], L).


prime_decomp_1(1, _, _, L, L) :- !.

% Special case for 2, increment 1
prime_decomp_1(N, SN, D, L, LF) :-
	(   0 is N mod D ->
	    Q is N / D,
	    SQ is sqrt(Q),
	    prime_decomp_1(Q, SQ, D, [D |L], LF)
	;
	    D1 is D+1,
	    (	D1 > SN ->
	        LF = [N |L]
	    ;
	        prime_decomp_2(N, SN, D1, L, LF)
	    )
	).

% General case, increment 2
prime_decomp_2(1, _, _, L, L) :- !.

prime_decomp_2(N, SN, D, L, LF) :-
	(   0 is N mod D ->
	    Q is N / D,
	    SQ is sqrt(Q),
	    prime_decomp_2(Q, SQ, D, [D |L], LF);
	    D1 is D+2,
	    (	D1 > SN ->
	        LF = [N |L]
	    ;
	        prime_decomp_2(N, SN, D1, L, LF)
	    )
	).
```


{{out}}

```Prolog
 ?- time(prime_decomp(9007199254740991, L)).
% 138,882 inferences, 0.344 CPU in 0.357 seconds (96% CPU, 404020 Lips)
L = [20394401,69431,6361].

 ?- time(prime_decomp(576460752303423487, L)).
% 2,684,734 inferences, 0.672 CPU in 0.671 seconds (100% CPU, 3995883 Lips)
L = [3203431780337,179951].

 ?- time(prime_decomp(1361129467683753853853498429727072845823, L)).
% 18,080,807 inferences, 7.953 CPU in 7.973 seconds (100% CPU, 2273422 Lips)
L = [145295143558111,7623851,409891,8191,2731,131,31,11,3].
```



### =Simple version=

 {{trans|Erlang}}
Optimized to stop on square root, and count by +2 on odds, above 2.


```Prolog
factors( N, FS):- 
    factors2( N, FS).
 
factors2( N, FS):-
    ( N < 2        -> FS = [] 
    ; 4 > N        -> FS = [N] 
    ; 0 is N rem 2 -> FS = [K|FS2], N2 is N div 2, factors2( N2, FS2)
    ;                 factors( N, 3, FS)
    ).
 
factors( N, K, FS):-
    ( N < 2        -> FS = [] 
    ; K*K > N      -> FS = [N] 
    ; 0 is N rem K -> FS = [K|FS2], N2 is N div K, factors( N2, K, FS2)
    ;                 K2 is K+2, factors( N, K2, FS)
    ).
```



## Pure


```pure
factor n = factor 2 n with
  factor k n = k : factor k (n div k) if n mod k == 0;
	     = if n>1 then [n] else [] if k*k>n;
	     = factor (k+1) n if k==2;
	     = factor (k+2) n otherwise;
end;
```



## PureBasic

{{works with|PureBasic|4.41}}

```PureBasic

CompilerIf #PB_Compiler_Debugger
  CompilerError "Turn off the debugger if you want reasonable speed in this example."
CompilerEndIf

Define.q

Procedure Factor(Number, List Factors())
  Protected I = 3
  While Number % 2 = 0
    AddElement(Factors())
    Factors() = 2
    Number / 2
  Wend
  Protected Max = Number
  While I <= Max And Number > 1
    While Number % I = 0
      AddElement(Factors())
      Factors() = I
      Number/I
    Wend
    I + 2
  Wend
EndProcedure

Number = 9007199254740991
NewList Factors()
time = ElapsedMilliseconds()
Factor(Number, Factors())
time = ElapsedMilliseconds()-time
S.s = "Factored " + Str(Number) + " in " + StrD(time/1000, 2) + " seconds."
ForEach Factors()
  S + #CRLF$ + Str(Factors())
Next
MessageRequester("", S)
```

{{out}}

```txt
Factored 9007199254740991 in 0.27 seconds.
6361
69431
20394401
```



## Python



### Python: Using Croft Spiral sieve

Note: the program below is saved to file <code>prime_decomposition.py</code> and imported as a library [[Least_common_multiple#Python|here]], [[Semiprime#Python|here]], [[Almost_prime#Python|here]], [[Emirp primes#Python|here]] and [[Extensible_prime_generator#Python|here]].


```python
from __future__ import print_function

import sys
from itertools import islice, cycle, count

try:
    from itertools import compress
except ImportError:
    def compress(data, selectors):
        """compress('ABCDEF', [1,0,1,0,1,1]) --> A C E F"""
        return (d for d, s in zip(data, selectors) if s)


def is_prime(n):
    return list(zip((True, False), decompose(n)))[-1][0]
 
class IsPrimeCached(dict):
    def __missing__(self, n):
        r = is_prime(n)
        self[n] = r
        return r
 
is_prime_cached = IsPrimeCached()
 
def croft():
    """Yield prime integers using the Croft Spiral sieve.

    This is a variant of wheel factorisation modulo 30.
    """
    # Copied from:
    #   https://code.google.com/p/pyprimes/source/browse/src/pyprimes.py
    # Implementation is based on erat3 from here:
    #   http://stackoverflow.com/q/2211990
    # and this website:
    #   http://www.primesdemystified.com/
    # Memory usage increases roughly linearly with the number of primes seen.
    # dict ``roots`` stores an entry x:p for every prime p.
    for p in (2, 3, 5):
        yield p
    roots = {9: 3, 25: 5}  # Map d**2 -> d.
    primeroots = frozenset((1, 7, 11, 13, 17, 19, 23, 29))
    selectors = (1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)
    for q in compress(
            # Iterate over prime candidates 7, 9, 11, 13, ...
            islice(count(7), 0, None, 2),
            # Mask out those that can't possibly be prime.
            cycle(selectors)
            ):
        # Using dict membership testing instead of pop gives a
        # 5-10% speedup over the first three million primes.
        if q in roots:
            p = roots[q]
            del roots[q]
            x = q + 2*p
            while x in roots or (x % 30) not in primeroots:
                x += 2*p
            roots[x] = p
        else:
            roots[q*q] = q
            yield q
primes = croft
 
def decompose(n):
    for p in primes():
        if p*p > n: break
        while n % p == 0:
            yield p
            n //=p
    if n > 1:
        yield n


if __name__ == '__main__':
    # Example: calculate factors of Mersenne numbers to M59 #
 
    import time
 
    for m in primes():
        p = 2 ** m - 1
        print( "2**{0:d}-1 = {1:d}, with factors:".format(m, p) )
        start = time.time()
        for factor in decompose(p):
            print(factor, end=' ')
            sys.stdout.flush()
 
        print( "=> {0:.2f}s".format( time.time()-start ) )
        if m >= 59:
            break
```

{{out}}

```txt
2**2-1 = 3, with factors:
3 => 0.00s
2**3-1 = 7, with factors:
7 => 0.01s
2**5-1 = 31, with factors:
31 => 0.00s
2**7-1 = 127, with factors:
127 => 0.00s
2**11-1 = 2047, with factors:
23 89 => 0.00s
2**13-1 = 8191, with factors:
8191 => 0.00s
2**17-1 = 131071, with factors:
131071 => 0.00s
2**19-1 = 524287, with factors:
524287 => 0.00s
2**23-1 = 8388607, with factors:
47 178481 => 0.01s
2**29-1 = 536870911, with factors:
233 1103 2089 => 0.01s
2**31-1 = 2147483647, with factors:
2147483647 => 0.03s
2**37-1 = 137438953471, with factors:
223 616318177 => 0.02s
2**41-1 = 2199023255551, with factors:
13367 164511353 => 0.01s
2**43-1 = 8796093022207, with factors:
431 9719 2099863 => 0.01s
2**47-1 = 140737488355327, with factors:
2351 4513 13264529 => 0.01s
2**53-1 = 9007199254740991, with factors:
6361 69431 20394401 => 0.04s
2**59-1 = 576460752303423487, with factors:
179951 3203431780337 => 1.22s
```



### Python: Using floating point

Here a shorter and marginally faster algorithm:


```python
from math import floor, sqrt
try: 
    long
except NameError: 
    long = int

def fac(n):
    step = lambda x: 1 + (x<<2) - ((x>>1)<<1)
    maxq = long(floor(sqrt(n)))
    d = 1
    q = n % 2 == 0 and 2 or 3 
    while q <= maxq and n % q != 0:
        q = step(d)
        d += 1
    return q <= maxq and [q] + fac(n//q) or [n]

if __name__ == '__main__':
    import time
    start = time.time()
    tocalc =  2**59-1
    print("%s = %s" % (tocalc, fac(tocalc)))
    print("Needed %ss" % (time.time() - start))
```


{{out}}

```txt
576460752303423487 = [3203431780337, 179951]
Needed 0.9240529537200928s
```



## R


```R
findfactors <- function(num) {
  x <- NULL
  firstprime<- 2; secondprime <- 3; everyprime <- num
  while( everyprime != 1 ) {
    while( everyprime%%firstprime == 0 ) {
      x <- c(x, firstprime)
      everyprime <- floor(everyprime/ firstprime)
    }
    firstprime <- secondprime
    secondprime <- secondprime + 2
  }
  x
}

print(findfactors(1027*4))
```


Or a more explicit (but less efficient) recursive approach: 

===Recursive Approach (Less efficient for large numbers)===

```R

primes <- as.integer(c())

max_prime_checker <- function(n){
  divisor <<- NULL

  primes <- primes[primes <= n]

  for(i in 1:length(primes)){
    if((n/primes[i]) %% 1 == 0){
      divisor[i]<<-1
    } else {
      divisor[i]<<-0
    } 
  }
  num_find <<- primes*as.integer(divisor)
  
  return(max(num_find))
}

#recursive prime finder
prime_factors <- function(n){
  
  factors <- NULL
  
  large <- max_prime_checker(n)
  n1 <- n/large 
  
  if(max_prime_checker(n1) == n1){
    factors <- c(large,n1)
    return(factors)
  } else {
    factors <- c(large, prime_factors(n1))
    return(factors)
  }
}

```



## Racket


```Racket

#lang racket
(require math)
(define (factors n)
  (append-map (λ (x) (make-list (cadr x) (car x))) (factorize n)))

```


Or, an explicit (and less efficient) computation:

```Racket

#lang racket
(define (factors number)
  (let loop ([n number] [i 2])
    (if (= n 1)
      '()
      (let-values ([(q r) (quotient/remainder n i)])
        (if (zero? r) (cons i (loop q i)) (loop n (add1 i)))))))

```



## REXX


### optimized slightly

No (error) checking was done for the input arguments to test their validity.

The number of decimal digits is adjusted to match the size of the top-of-the-range ('''top''').

Also, a count of primes found is shown.

If the   ''top''   number is negative, only the number of primes up to   '''abs(top)'''   is shown.

A method exists in this REXX program to also test Mersenne-type numbers   <big>(2<sup>n</sup> - 1)</big>.

Since the majority of computing time is spent looking for primes, that part of the program was 

optimized somewhat (but could be extended if more optimization is wanted).

```rexx
/*REXX pgm does prime decomposition of a range of positive integers (with a prime count)*/
numeric digits 1000                              /*handle thousand digits for the powers*/
parse arg  bot  top  step   base  add            /*get optional arguments from the C.L. */
if  bot==''   then do;  bot=1;  top=100;  end    /*no  BOT given?  Then use the default.*/
if  top==''   then              top=bot          /* "  TOP?  "       "   "   "     "    */
if step==''   then step=  1                      /* " STEP?  "       "   "   "     "    */
if add ==''   then  add= -1                      /* "  ADD?  "       "   "   "     "    */
tell= top>0;       top=abs(top)                  /*if TOP is negative, suppress displays*/
w=length(top)                                    /*get maximum width for aligned display*/
if base\==''  then w=length(base**top)           /*will be testing powers of two later? */
@.=left('', 7);   @.0="{unity}";   @.1='[prime]' /*some literals:  pad;  prime (or not).*/
numeric digits max(9, w+1)                       /*maybe increase the digits precision. */
#=0                                              /*#:    is the number of primes found. */
        do n=bot  to top  by step                /*process a single number  or  a range.*/
        ?=n;  if base\==''  then ?=base**n + add /*should we perform a "Mercenne" test? */
        pf=factr(?);      f=words(pf)            /*get prime factors; number of factors.*/
        if f==1  then #=#+1                      /*Is N prime?  Then bump prime counter.*/
        if tell  then say right(?,w)   right('('f")",9)   'prime factors: '     @.f     pf
        end   /*n*/
say
ps= 'primes';    if p==1  then ps= "prime"       /*setup for proper English in sentence.*/
say right(#, w+9+1)       ps       'found.'      /*display the number of primes found.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
factr: procedure;  parse arg x 1 d,$             /*set X, D  to argument 1;  $  to null.*/
if x==1  then return ''                          /*handle the special case of   X = 1.  */
       do  while x//2==0;  $=$ 2;  x=x%2;  end   /*append all the  2  factors of new  X.*/
       do  while x//3==0;  $=$ 3;  x=x%3;  end   /*   "    "   "   3     "     "  "   " */
       do  while x//5==0;  $=$ 5;  x=x%5;  end   /*   "    "   "   5     "     "  "   " */
       do  while x//7==0;  $=$ 7;  x=x%7;  end   /*   "    "   "   7     "     "  "   " */
                                                 /*                                  ___*/
q=1;   do  while q<=x;  q=q*4;  end              /*these two lines compute integer  √ X */
r=0;   do  while q>1;   q=q%4;  _=d-r-q;  r=r%2;   if _>=0  then do; d=_; r=r+q; end;  end

       do j=11  by 6  to r                       /*insure that  J  isn't divisible by 3.*/
       parse var j  ''  -1  _                    /*obtain the last decimal digit of  J. */
       if _\==5  then  do  while x//j==0;  $=$ j;  x=x%j;  end     /*maybe reduce by J. */
       if _ ==3  then iterate                    /*Is next  Y  is divisible by 5?  Skip.*/
       y=j+2;          do  while x//y==0;  $=$ y;  x=x%y;  end     /*maybe reduce by J. */
       end   /*j*/
                                                 /* [↓]  The $ list has a leading blank.*/
if x==1  then return $                           /*Is residual=unity? Then don't append.*/
              return $ x                         /*return   $   with appended residual. */
```

'''output'''   when using the default input of:   <tt> 1   100 </tt>
<pre style="height:60ex">
  1       (0) prime factors:  {unity}
  2       (1) prime factors:  [prime]  2
  3       (1) prime factors:  [prime]  3
  4       (2) prime factors:           2 2
  5       (1) prime factors:  [prime]  5
  6       (2) prime factors:           2 3
  7       (1) prime factors:  [prime]  7
  8       (3) prime factors:           2 2 2
  9       (2) prime factors:           3 3
 10       (2) prime factors:           2 5
 11       (1) prime factors:  [prime]  11
 12       (3) prime factors:           2 2 3
 13       (1) prime factors:  [prime]  13
 14       (2) prime factors:           2 7
 15       (2) prime factors:           3 5
 16       (4) prime factors:           2 2 2 2
 17       (1) prime factors:  [prime]  17
 18       (3) prime factors:           2 3 3
 19       (1) prime factors:  [prime]  19
 20       (3) prime factors:           2 2 5
 21       (2) prime factors:           3 7
 22       (2) prime factors:           2 11
 23       (1) prime factors:  [prime]  23
 24       (4) prime factors:           2 2 2 3
 25       (2) prime factors:           5 5
 26       (2) prime factors:           2 13
 27       (3) prime factors:           3 3 3
 28       (3) prime factors:           2 2 7
 29       (1) prime factors:  [prime]  29
 30       (3) prime factors:           2 3 5
 31       (1) prime factors:  [prime]  31
 32       (5) prime factors:           2 2 2 2 2
 33       (2) prime factors:           3 11
 34       (2) prime factors:           2 17
 35       (2) prime factors:           5 7
 36       (4) prime factors:           2 2 3 3
 37       (1) prime factors:  [prime]  37
 38       (2) prime factors:           2 19
 39       (2) prime factors:           3 13
 40       (4) prime factors:           2 2 2 5
 41       (1) prime factors:  [prime]  41
 42       (3) prime factors:           2 3 7
 43       (1) prime factors:  [prime]  43
 44       (3) prime factors:           2 2 11
 45       (3) prime factors:           3 3 5
 46       (2) prime factors:           2 23
 47       (1) prime factors:  [prime]  47
 48       (5) prime factors:           2 2 2 2 3
 49       (2) prime factors:           7 7
 50       (3) prime factors:           2 5 5
 51       (2) prime factors:           3 17
 52       (3) prime factors:           2 2 13
 53       (1) prime factors:  [prime]  53
 54       (4) prime factors:           2 3 3 3
 55       (2) prime factors:           5 11
 56       (4) prime factors:           2 2 2 7
 57       (2) prime factors:           3 19
 58       (2) prime factors:           2 29
 59       (1) prime factors:  [prime]  59
 60       (4) prime factors:           2 2 3 5
 61       (1) prime factors:  [prime]  61
 62       (2) prime factors:           2 31
 63       (3) prime factors:           3 3 7
 64       (6) prime factors:           2 2 2 2 2 2
 65       (2) prime factors:           5 13
 66       (3) prime factors:           2 3 11
 67       (1) prime factors:  [prime]  67
 68       (3) prime factors:           2 2 17
 69       (2) prime factors:           3 23
 70       (3) prime factors:           2 5 7
 71       (1) prime factors:  [prime]  71
 72       (5) prime factors:           2 2 2 3 3
 73       (1) prime factors:  [prime]  73
 74       (2) prime factors:           2 37
 75       (3) prime factors:           3 5 5
 76       (3) prime factors:           2 2 19
 77       (2) prime factors:           7 11
 78       (3) prime factors:           2 3 13
 79       (1) prime factors:  [prime]  79
 80       (5) prime factors:           2 2 2 2 5
 81       (4) prime factors:           3 3 3 3
 82       (2) prime factors:           2 41
 83       (1) prime factors:  [prime]  83
 84       (4) prime factors:           2 2 3 7
 85       (2) prime factors:           5 17
 86       (2) prime factors:           2 43
 87       (2) prime factors:           3 29
 88       (4) prime factors:           2 2 2 11
 89       (1) prime factors:  [prime]  89
 90       (4) prime factors:           2 3 3 5
 91       (2) prime factors:           7 13
 92       (3) prime factors:           2 2 23
 93       (2) prime factors:           3 31
 94       (2) prime factors:           2 47
 95       (2) prime factors:           5 19
 96       (6) prime factors:           2 2 2 2 2 3
 97       (1) prime factors:  [prime]  97
 98       (3) prime factors:           2 7 7
 99       (3) prime factors:           3 3 11
100       (4) prime factors:           2 2 5 5

           25 primes found.

```

'''output'''   when using the input of:   <tt> 9007199254740991 </tt>

```txt

9007199254740991       (3) prime factors:           6361 69431 20394401

              0 primes found.

```

'''output'''   when using the input of:   <tt> 2543821448263974486045199 </tt>

```txt

2543821448263974486045199       (6) prime factors:           701 1123 1123 2411 1092461 1092461

              0 primes found.

```

'''output'''   when using the input of:   <tt> 1   -1000000 </tt>

```txt

            78498 primes found.

```

'''output'''   when using the input of:   <tt> 2   50   1   2   -1 </tt>

(essentially testing for Mersenne primes:   2<sup>n</sup> -1)

```txt

               3       (1) prime factors:  [prime]  3
               7       (1) prime factors:  [prime]  7
              15       (2) prime factors:           3 5
              31       (1) prime factors:  [prime]  31
              63       (3) prime factors:           3 3 7
             127       (1) prime factors:  [prime]  127
             255       (3) prime factors:           3 5 17
             511       (2) prime factors:           7 73
            1023       (3) prime factors:           3 11 31
            2047       (2) prime factors:           23 89
            4095       (5) prime factors:           3 3 5 7 13
            8191       (1) prime factors:  [prime]  8191
           16383       (2) prime factors:           3 5461
           32767       (2) prime factors:           7 4681
           65535       (4) prime factors:           3 5 17 257
          131071       (1) prime factors:  [prime]  131071
          262143       (5) prime factors:           3 3 3 7 1387
          524287       (1) prime factors:  [prime]  524287
         1048575       (6) prime factors:           3 5 5 11 41 31
         2097151       (3) prime factors:           7 7 42799
         4194303       (4) prime factors:           3 23 89 683
         8388607       (2) prime factors:           47 178481
        16777215       (7) prime factors:           3 3 5 7 13 17 241
        33554431       (1) prime factors:  [prime]  33554431
        67108863       (2) prime factors:           3 22369621
       134217727       (2) prime factors:           7 19173961
       268435455       (5) prime factors:           3 5 29 113 5461
       536870911       (3) prime factors:           233 1103 2089
      1073741823       (5) prime factors:           3 3 7 11 1549411
      2147483647       (1) prime factors:  [prime]  2147483647
      4294967295       (5) prime factors:           3 5 17 257 65537
      8589934591       (4) prime factors:           7 23 89 599479
     17179869183       (3) prime factors:           3 43691 131071
     34359738367       (3) prime factors:           71 122921 3937
     68719476735       (7) prime factors:           3 3 3 5 7 13 5593771
    137438953471       (1) prime factors:  [prime]  137438953471
    274877906943       (2) prime factors:           3 91625968981
    549755813887       (2) prime factors:           7 78536544841
   1099511627775       (7) prime factors:           3 5 5 11 17 41 1912111
   2199023255551       (2) prime factors:           13367 164511353
   4398046511103       (5) prime factors:           3 3 7 7 9972894583
   8796093022207       (3) prime factors:           431 9719 2099863
  17592186044415       (6) prime factors:           3 5 23 89 683 838861
  35184372088831       (2) prime factors:           7 5026338869833
  70368744177663       (4) prime factors:           3 47 178481 2796203
 140737488355327       (2) prime factors:           2351 59862819377
 281474976710655       (8) prime factors:           3 3 5 7 13 17 257 15732721
 562949953421311       (1) prime factors:  [prime]  562949953421311
1125899906842623       (4) prime factors:           3 11 251 135928999981

                        11 primes found.

```

'''output'''   when using the input of:   <tt> 1   50   1   2   +1 </tt>

(essentially testing for   2<sup>n</sup> +1)

```txt

               3       (1) prime factors:  [prime]  3
               5       (1) prime factors:  [prime]  5
               9       (2) prime factors:           3 3
              17       (1) prime factors:  [prime]  17
              33       (2) prime factors:           3 11
              65       (2) prime factors:           5 13
             129       (2) prime factors:           3 43
             257       (1) prime factors:  [prime]  257
             513       (4) prime factors:           3 3 3 19
            1025       (3) prime factors:           5 5 41
            2049       (2) prime factors:           3 683
            4097       (2) prime factors:           17 241
            8193       (2) prime factors:           3 2731
           16385       (3) prime factors:           5 29 113
           32769       (4) prime factors:           3 3 11 331
           65537       (1) prime factors:  [prime]  65537
          131073       (2) prime factors:           3 43691
          262145       (3) prime factors:           5 13 4033
          524289       (2) prime factors:           3 174763
         1048577       (2) prime factors:           17 61681
         2097153       (3) prime factors:           3 3 233017
         4194305       (2) prime factors:           5 838861
         8388609       (2) prime factors:           3 2796203
        16777217       (2) prime factors:           257 65281
        33554433       (4) prime factors:           3 11 251 4051
        67108865       (4) prime factors:           5 53 1613 157
       134217729       (5) prime factors:           3 3 3 3 1657009
       268435457       (2) prime factors:           17 15790321
       536870913       (3) prime factors:           3 59 3033169
      1073741825       (5) prime factors:           5 5 13 41 80581
      2147483649       (2) prime factors:           3 715827883
      4294967297       (2) prime factors:           641 6700417
      8589934593       (4) prime factors:           3 3 683 1397419
     17179869185       (4) prime factors:           5 137 953 26317
     34359738369       (5) prime factors:           3 11 281 86171 43
     68719476737       (2) prime factors:           17 4042322161
    137438953473       (2) prime factors:           3 45812984491
    274877906945       (2) prime factors:           5 54975581389
    549755813889       (3) prime factors:           3 3 61083979321
   1099511627777       (2) prime factors:           257 4278255361
   2199023255553       (3) prime factors:           3 83 8831418697
   4398046511105       (5) prime factors:           5 13 29 113 20647621
   8796093022209       (2) prime factors:           3 2932031007403
  17592186044417       (3) prime factors:           17 353 2931542417
  35184372088833       (5) prime factors:           3 3 3 11 118465899289
  70368744177665       (4) prime factors:           5 1013 30269 458989
 140737488355329       (2) prime factors:           3 46912496118443
 281474976710657       (2) prime factors:           65537 4294901761
 562949953421313       (2) prime factors:           3 187649984473771
1125899906842625       (6) prime factors:           5 5 5 41 101 2175126601

                         5 primes found.

```



### optimized more

This REXX version is about   '''20%'''   faster than the 1<sup>st</sup> REXX version when factoring one million numbers.

```rexx
/*REXX pgm does prime decomposition of a range of positive integers (with a prime count)*/
numeric digits 1000                              /*handle thousand digits for the powers*/
parse arg  bot  top  step   base  add            /*get optional arguments from the C.L. */
if  bot==''   then do;  bot=1;  top=100;  end    /*no  BOT given?  Then use the default.*/
if  top==''   then              top=bot          /* "  TOP?  "       "   "   "     "    */
if step==''   then step=  1                      /* " STEP?  "       "   "   "     "    */
if add ==''   then  add= -1                      /* "  ADD?  "       "   "   "     "    */
tell= top>0;       top=abs(top)                  /*if TOP is negative, suppress displays*/
w=length(top)                                    /*get maximum width for aligned display*/
if base\==''  then w=length(base**top)           /*will be testing powers of two later? */
@.=left('', 7);   @.0="{unity}";   @.1='[prime]' /*some literals:  pad;  prime (or not).*/
numeric digits max(9, w+1)                       /*maybe increase the digits precision. */
#=0                                              /*#:    is the number of primes found. */
        do n=bot  to top  by step                /*process a single number  or  a range.*/
        ?=n;  if base\==''  then ?=base**n + add /*should we perform a "Mercenne" test? */
        pf=factr(?);      f=words(pf)            /*get prime factors; number of factors.*/
        if f==1  then #=#+1                      /*Is N prime?  Then bump prime counter.*/
        if tell  then say right(?,w)   right('('f")",9)   'prime factors: '     @.f     pf
        end   /*n*/
say
ps= 'primes';    if p==1  then ps= "prime"       /*setup for proper English in sentence.*/
say right(#, w+9+1)       ps       'found.'      /*display the number of primes found.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
factr: procedure;  parse arg x 1 d,$             /*set X, D  to argument 1;  $  to null.*/
if x==1  then return ''                          /*handle the special case of   X = 1.  */
       do  while x// 2==0;  $=$  2;  x=x%2;  end /*append all the  2  factors of new  X.*/
       do  while x// 3==0;  $=$  3;  x=x%3;  end /*   "    "   "   3     "     "  "   " */
       do  while x// 5==0;  $=$  5;  x=x%5;  end /*   "    "   "   5     "     "  "   " */
       do  while x// 7==0;  $=$  7;  x=x%7;  end /*   "    "   "   7     "     "  "   " */
       do  while x//11==0;  $=$ 11;  x=x%11; end /*   "    "   "  11     "     "  "   " */    /* ◄■■■■ added.*/
       do  while x//13==0;  $=$ 13;  x=x%13; end /*   "    "   "  13     "     "  "   " */    /* ◄■■■■ added.*/
       do  while x//17==0;  $=$ 17;  x=x%17; end /*   "    "   "  17     "     "  "   " */    /* ◄■■■■ added.*/
       do  while x//19==0;  $=$ 19;  x=x%19; end /*   "    "   "  19     "     "  "   " */    /* ◄■■■■ added.*/
       do  while x//23==0;  $=$ 23;  x=x%23; end /*   "    "   "  23     "     "  "   " */    /* ◄■■■■ added.*/
                                                 /*                                  ___*/
q=1;   do  while q<=x;  q=q*4;  end              /*these two lines compute integer  √ X */
r=0;   do  while q>1;   q=q%4;  _=d-r-q;  r=r%2;   if _>=0  then do; d=_; r=r+q; end;  end

       do j=29  by 6  to r                       /*insure that  J  isn't divisible by 3.*/    /* ◄■■■■ changed.*/
       parse var j  ''  -1  _                    /*obtain the last decimal digit of  J. */
       if _\==5  then  do  while x//j==0;  $=$ j;  x=x%j;  end     /*maybe reduce by J. */
       if _ ==3  then iterate                    /*Is next  Y  is divisible by 5?  Skip.*/
       y=j+2;          do  while x//y==0;  $=$ y;  x=x%y;  end     /*maybe reduce by J. */
       end   /*j*/
                                                 /* [↓]  The $ list has a leading blank.*/
if x==1  then return $                           /*Is residual=unity? Then don't append.*/
              return $ x                         /*return   $   with appended residual. */
```

'''output'''   is identical to the 1<sup>st</sup> REXX version. 




## Ring


```ring

prime = 18705
decomp(prime)

func decomp nr
x = ""
for i = 1 to nr
    if isPrime(i) and nr % i = 0
       x = x + string(i) + " * " ok
    if i = nr
       x2 = substr(x,1,(len(x)-2))
       see string(nr) + " = " + x2 + nl ok
next 

func isPrime num
     if (num <= 1) return 0 ok
     if (num % 2 = 0) and num != 2 return 0 ok
     for i = 3 to floor(num / 2) -1 step 2
         if (num % i = 0) return 0 ok
     next
     return 1

```



## Ruby


### Built in


```ruby
irb(main):001:0> require 'prime'
=> true
irb(main):003:0> 2543821448263974486045199.prime_division
=> [[701, 1], [1123, 2], [2411, 1], [1092461, 2]]
```



### Simple algorithm


```ruby
# Get prime decomposition of integer _i_.
# This routine is terribly inefficient, but elegance rules.
def prime_factors(i)
  v = (2..i-1).detect{|j| i % j == 0} 
  v ? ([v] + prime_factors(i/v)) : [i]
end

# Example: Decompose all possible Mersenne primes up to 2**31-1.
# This may take several minutes to show that 2**31-1 is prime.
(2..31).each do |i|
  factors = prime_factors(2**i-1)
  puts "2**#{i}-1 = #{2**i-1} = #{factors.join(' * ')}"
end
```

{{out}}

```txt
...
2**28-1 = 268435455 = 3 * 5 * 29 * 43 * 113 * 127
2**29-1 = 536870911 = 233 * 1103 * 2089
2**30-1 = 1073741823 = 3 * 3 * 7 * 11 * 31 * 151 * 331
2**31-1 = 2147483647 = 2147483647
```



### Faster algorithm


```ruby
# Get prime decomposition of integer _i_.
# This routine is more efficient than prime_factors,
# and quite similar to Integer#prime_division of MRI 1.9.
def prime_factors_faster(i)
  factors = []
  check = proc do |p|
    while(q, r = i.divmod(p)
          r.zero?)
      factors << p
      i = q
    end
  end
  check[2]
  check[3]
  p = 5
  while p * p <= i
    check[p]
    p += 2
    check[p]
    p += 4    # skip multiples of 2 and 3
  end
  factors << i if i > 1
  factors
end

# Example: Decompose all possible Mersenne primes up to 2**70-1.
# This may take several minutes to show that 2**61-1 is prime,
# but 2**62-1 and 2**67-1 are not prime.
(2..70).each do |i|
  factors = prime_factors_faster(2**i-1)
  puts "2**#{i}-1 = #{2**i-1} = #{factors.join(' * ')}"
end
```

{{out}}

```txt
...
2**67-1 = 147573952589676412927 = 193707721 * 761838257287
2**68-1 = 295147905179352825855 = 3 * 5 * 137 * 953 * 26317 * 43691 * 131071
2**69-1 = 590295810358705651711 = 7 * 47 * 178481 * 10052678938039
2**70-1 = 1180591620717411303423 = 3 * 11 * 31 * 43 * 71 * 127 * 281 * 86171 * 122921
```


This benchmark compares the different implementations.


```ruby
require 'benchmark'
require 'mathn'
Benchmark.bm(24) do |x|
  [2**25 - 6, 2**35 - 7].each do |i|
    puts "#{i} = #{prime_factors_faster(i).join(' * ')}"
    x.report("  prime_factors") { prime_factors(i) }
    x.report("  prime_factors_faster") { prime_factors_faster(i) }
    x.report("  Integer#prime_division") { i.prime_division }
  end
end
```


With [[MRI]] 1.8, ''prime_factors'' is slow, ''Integer#prime_division'' is fast, and ''prime_factors_faster'' is very fast. With MRI 1.9, Integer#prime_division is also very fast.


## Scala

{{libheader|Scala}}

```Scala
import annotation.tailrec
import collection.parallel.mutable.ParSeq

object PrimeFactors extends App {
  def factorize(n: Long): List[Long] = {
    @tailrec
    def factors(tuple: (Long, Long, List[Long], Int)): List[Long] = {
      tuple match {
        case (1, _, acc, _)                 => acc
        case (n, k, acc, _) if (n % k == 0) => factors((n / k, k, acc ++ ParSeq(k), Math.sqrt(n / k).toInt))
        case (n, k, acc, sqr) if (k < sqr)  => factors(n, k + 1, acc, sqr)
        case (n, k, acc, sqr) if (k >= sqr) => factors((1, k, acc ++ ParSeq(n), 0))
      }
    }
    factors((n, 2, List[Long](), Math.sqrt(n).toInt))
  }

  def mersenne(p: Int): BigInt = (BigInt(2) pow p) - 1

  def sieve(nums: Stream[Int]): Stream[Int] =
    Stream.cons(nums.head, sieve((nums.tail) filter (_ % nums.head != 0)))
  // An infinite stream of primes, lazy evaluation and memo-ized
  val oddPrimes = sieve(Stream.from(3, 2))
  def primes = sieve(2 #:: oddPrimes)

  oddPrimes takeWhile (_ <= 59) foreach { p =>
    { // Needs some intermediate results for nice formatting
      val numM = s"M${p}"
      val nMersenne = mersenne(p).toLong
      val lit = f"${nMersenne}%30d"

      val datum = System.nanoTime
      val result = factorize(nMersenne)
      val mSec = ((System.nanoTime - datum) / 1.0e+6).round

      def decStr = { if (lit.length > 30) f"(M has ${lit.length}%3d dec)" else "" }
      def sPrime = { if (result.isEmpty) " is a prime number." else "" }

      println(
        f"$numM%4s = 2^$p%03d - 1 = ${lit}%s${sPrime} ($mSec%,4d msec) composed of ${result.mkString(" × ")}")
    }
  }
}
```

{{out}}

```txt

  M3 = 2^003 - 1 =                              7 (  23 msec) composed of 7
  M5 = 2^005 - 1 =                             31 (   0 msec) composed of 31
  M7 = 2^007 - 1 =                            127 (   0 msec) composed of 127
 M11 = 2^011 - 1 =                           2047 (   0 msec) composed of 23 × 89
 M13 = 2^013 - 1 =                           8191 (   0 msec) composed of 8191
 M17 = 2^017 - 1 =                         131071 (   1 msec) composed of 131071
 M19 = 2^019 - 1 =                         524287 (   1 msec) composed of 524287
 M23 = 2^023 - 1 =                        8388607 (   1 msec) composed of 47 × 178481
 M29 = 2^029 - 1 =                      536870911 (   2 msec) composed of 233 × 1103 × 2089
 M31 = 2^031 - 1 =                     2147483647 (  39 msec) composed of 2147483647
 M37 = 2^037 - 1 =                   137438953471 (   8 msec) composed of 223 × 616318177
 M41 = 2^041 - 1 =                  2199023255551 (   2 msec) composed of 13367 × 164511353
 M43 = 2^043 - 1 =                  8796093022207 (   2 msec) composed of 431 × 9719 × 2099863
 M47 = 2^047 - 1 =                140737488355327 (   2 msec) composed of 2351 × 4513 × 13264529
 M53 = 2^053 - 1 =               9007199254740991 (   7 msec) composed of 6361 × 69431 × 20394401
 M59 = 2^059 - 1 =             576460752303423487 ( 152 msec) composed of 179951 × 3203431780337
```


Getting the prime factors does not require identifying prime numbers. 
Since the problems seems to ask for it, here is one version that does it:


```Scala
class PrimeFactors(n: BigInt) extends Iterator[BigInt] {
  val zero = BigInt(0)
  val one = BigInt(1)
  val two = BigInt(2)
  def isPrime(n: BigInt) = n.isProbablePrime(10)
  var currentN = n
  var prime = two

  def nextPrime =
    if (prime == two) {
      prime += one
    } else {
      prime += two
      while (!isPrime(prime)) {
        prime += two
        if (prime * prime > currentN)
          prime = currentN
      }
    }

  def next = {
    if (!hasNext)
      throw new NoSuchElementException("next on empty iterator")
      
    while(currentN % prime != zero) {
      nextPrime
    }
    currentN /= prime
    prime
  }

  def hasNext = currentN != one && currentN > zero
}
```


The method isProbablePrime(n) has a chance of 1 - 1/(2^n) of correctly
identifying a prime. 
Next is a version that does not depend on identifying primes, 
and works with arbitrary integral numbers:

```Scala
class PrimeFactors[N](n: N)(implicit num: Integral[N]) extends Iterator[N] {
  import num._
  val two = one + one
  var currentN = n
  var divisor = two

  def next = {
    if (!hasNext)
      throw new NoSuchElementException("next on empty iterator")
      
    while(currentN % divisor != zero) {
      if (divisor == two)
        divisor += one
      else
        divisor += two
        
      if (divisor * divisor > currentN)
        divisor = currentN
    }
    currentN /= divisor
    divisor
  }

  def hasNext = currentN != one && currentN > zero
}
```

{{out}}
Both versions can be rather slow, as they accept arbitrarily big numbers,
as requested. 
{{out|Test}}

```txt

scala> BigInt(2) to BigInt(30) filter (_ isProbablePrime 10) map (p => (p, BigInt(2).pow(p.toInt) - 1)) foreach {
     |   case (prime, n) => println("2**"+prime+"-1 = "+n+", with factors: "+new PrimeFactors(n).mkString(", "))
     | }
2**2-1 = 3, with factors: 3
2**3-1 = 7, with factors: 7
2**5-1 = 31, with factors: 31
2**7-1 = 127, with factors: 127
2**11-1 = 2047, with factors: 23, 89
2**13-1 = 8191, with factors: 8191
2**17-1 = 131071, with factors: 131071
2**19-1 = 524287, with factors: 524287
2**23-1 = 8388607, with factors: 47, 178481
2**29-1 = 536870911, with factors: 233, 1103, 2089
2**31-1 = 2147483647, with factors: 2147483647
2**37-1 = 137438953471, with factors: 223, 616318177
2**41-1 = 2199023255551, with factors: 13367, 164511353
2**43-1 = 8796093022207, with factors: 431, 9719, 2099863
2**47-1 = 140737488355327, with factors: 2351, 4513, 13264529
2**53-1 = 9007199254740991, with factors: 6361, 69431, 20394401
2**59-1 = 576460752303423487, with factors: 179951, 3203431780337

```


Alternatively, Scala LazyLists and Iterators support quite elegant one-line encodings of iterative/recursive algorithms, allowing us to to define the prime factorization like so:

```scala
import spire.math.SafeLong
import spire.implicits._
def pFactors(num: SafeLong): Vector[SafeLong] = Iterator.iterate((Vector[SafeLong](), num, SafeLong(2))){case (ac, n, f) => if(n%f == 0) (ac :+ f, n/f, f) else (ac, n, f + 1)}.dropWhile(_._2 != 1).next._1
```



## Scheme


```scheme
(define (factor number)
  (define (*factor divisor number)
    (if (> (* divisor divisor) number)
        (list number)
        (if (= (modulo number divisor) 0)
            (cons divisor (*factor divisor (/ number divisor)))
            (*factor (+ divisor 1) number))))
  (*factor 2 number))

(display (factor 111111111111))
(newline)
```

{{out}}
 (3 7 11 13 37 101 9901)


## Seed7


```seed7
const func array integer: factorise (in var integer: number) is func
  result
    var array integer: result is 0 times 0;
  local
    var integer: checker is 2;
  begin
    while checker * checker <= number do
      if number rem checker = 0 then
        result &:= [](checker);
        number := number div checker;
      else
        incr(checker);
      end if;
    end while;
    if number <> 1 then
      result &:= [](number);
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#factorise]


## SequenceL

'''Recursive Using isPrime'''


```sequencel
isPrime(n) := n = 2 or (n > 1 and none(n mod ([2]++((1...floor(sqrt(n)/2))*2+1)) = 0));

primeFactorization(num) := primeFactorizationHelp(num, []);

primeFactorizationHelp(num, current(1)) := 
	 let
	 	primeFactors[i] := i when num mod i = 0 and isPrime(i) foreach i within 2 ... num;
	 in
			current when size(primeFactors) = 0
		else
			primeFactorizationHelp(num / product(primeFactors), current ++ primeFactors);
```


Using isPrime Based On: [https://www.youtube.com/watch?v=CsCBkPg1FbE]

'''Recursive Trial Division'''


```sequencel
primeFactorization(num) := primeFactorizationHelp(num, 2, []);

primeFactorizationHelp(num, divisor, factors(1)) :=
		factors when num <= 1
	else
		primeFactorizationHelp(num, divisor + 1, factors) when num mod divisor /= 0
	else
		primeFactorizationHelp(num / divisor, divisor, factors ++ [divisor]);
```



## Sidef

Built-in:

```ruby
say factor(536870911)      #=> [233, 1103, 2089]
say factor_exp(536870911)  #=> [[233, 1], [1103, 1], [2089, 1]]
```


Trial division:

```ruby
func prime_factors(n) {
    return [] if (n < 1)
    gather {
        while (!(n & 1)) {
            n >>= 1
            take(2)
        }
        var p = 3
        while ((n > 1) && (p*p <= n)) {
            while (n %% p) {
                n //= p
                take(p)
            }
            p += 2
        }
        take(n) if (n > 1)
    }
}
```


Calling the function:

```ruby
say prime_factors(536870911)   #=> [233, 1103, 2089]
```



## Simula

Simula has no built-in function to test for prime numbers.

Code for class bignum can be found here: https://rosettacode.org/wiki/Pi#Simula

```simula

EXTERNAL CLASS BIGNUM;
BIGNUM
BEGIN

    CLASS TEXTLIST;
    BEGIN
        CLASS TEXTARRAY(N); INTEGER N;
        BEGIN
            TEXT ARRAY DATA(1:N);
        END TEXTARRAY;
        PROCEDURE EXPAND(N); INTEGER N;
        BEGIN
            REF(TEXTARRAY) NEWARR;
            INTEGER I;
            NEWARR :- NEW TEXTARRAY(20);
            FOR I := 1 STEP 1 UNTIL SIZE DO BEGIN
                NEWARR.DATA(I) :- ARR.DATA(I);
            END;
            ARR :- NEWARR;
        END EXPAND;
        PROCEDURE APPEND(T); TEXT T;
        BEGIN
            IF SIZE = ARR.N THEN
                EXPAND(2*ARR.N);
            SIZE := SIZE+1;
            ARR.DATA(SIZE) :- T;
        END EXPAND;
        TEXT PROCEDURE GET(I); INTEGER I;
            GET :- ARR.DATA(I);
        REF(TEXTARRAY) ARR;
        INTEGER SIZE;
        EXPAND(20);
    END TEXTLIST;

    REF(TEXTLIST) PROCEDURE PRIME_FACTORS(N); TEXT N;
    BEGIN
        REF(TEXTLIST) FACTORS;
        REF(DIVMOD) DM;
        TEXT P;
        FACTORS :- NEW TEXTLIST;
        IF TCMP(N, "1") < 0 THEN
            GOTO RETURN;
        P :- "2";
        FOR DM :- TDIVMOD(N,P) WHILE TISZERO(DM.MOD) DO BEGIN
            N :- DM.DIV;
            FACTORS.APPEND(P);
        END;
        P :- "3";
        WHILE TCMP(N,"1") > 0 AND THEN TCMP(TMUL(P,P),N) <= 0 DO BEGIN
            FOR DM :- TDIVMOD(N, P) WHILE TISZERO(DM.MOD) DO BEGIN
                N :- DM.DIV;
                FACTORS.APPEND(P);
            END;
            P :- TADD(P,"2");
        END;
        IF TCMP(N,"1") > 0 THEN
            FACTORS.APPEND(N);
    RETURN:
        PRIME_FACTORS :- FACTORS;
    END PRIME_FACTORS;

    REF(TEXTLIST) FACTORS;
    TEXT INP;
    INTEGER I;

    FOR INP :- "536870911", "6768768", "1957", "64865899369365843" DO BEGIN
        FACTORS :- PRIME_FACTORS(INP);
        OUTTEXT("PRIME FACTORS OF ");
        OUTTEXT(INP);
        OUTTEXT(" => [");
        FOR I := 1 STEP 1 UNTIL FACTORS.SIZE DO BEGIN
            IF I > 1 THEN
                OUTTEXT(", ");
            OUTTEXT(FACTORS.GET(I));
        END;
        OUTTEXT("]");
        OUTIMAGE;
    END;

END;

```

{{out}}

```txt

PRIME FACTORS OF 536870911 => [233, 1103, 2089]
PRIME FACTORS OF 6768768 => [2, 2, 2, 2, 2, 2, 2, 3, 17627]
PRIME FACTORS OF 1957 => [19, 103]
PRIME FACTORS OF 64865899369365843 => [3, 7, 397, 276229, 28166791]

5320 garbage collection(s) in 1.9 seconds.

```



## Slate

Admittedly, this is just based on the Smalltalk entry below:

```slate
n@(Integer traits) primesDo: block
"Decomposes the Integer into primes, applying the block to each (in increasing
order)."
[| div next remaining |
  div: 2.
  next: 3.
  remaining: n.
  [[(remaining \\ div) isZero]
     whileTrue:
       [block applyTo: {div}.
	remaining: remaining // div].
   remaining = 1] whileFalse:
     [div: next.
      next: next + 2] "Just look at the next odd integer."
].
```



## Smalltalk



```smalltalk
Integer extend [
    primesDo: aBlock [
        | div next rest |
        div := 2. next := 3.
        rest := self.
        [ [ rest \\ div == 0 ]
              whileTrue: [
                  aBlock value: div.
                  rest := rest // div ].
          rest = 1] whileFalse: [
              div := next. next := next + 2 ]
    ]
]
123456 primesDo: [ :each | each printNl ]
```



## SPAD

{{works with|FriCAS, OpenAxiom, Axiom}}

```SPAD


(1) -> factor 102400

         12 2
   (1)  2  5
                                                      Type: Factored(Integer)
(2) -> factor 23193931893819371

   (2)  83 3469 71341 1129153
                                                      Type: Factored(Integer)


```


Domain:[http://fricas.github.io/api/Factored.html?highlight=factor Factored(R)]



## Stata


The following Mata function will factor any representable positive integer (that is, between 1 and 2^53).


```stata
function factor(n_) {
	n = n_
	a = J(0,2,.)
	if (n<2) {
		return(a)
	}
	else if (n<4) {
		return((n,1))
	}
	else {
		if (mod(n,2)==0) {
			for (i=0; mod(n,2)==0; i++) n = floor(n/2)
			a = a\(2,i)
		}
			
		for (k=3; k*k<=n; k=k+2) {
			if (mod(n,k)==0) {
				for (i=0; mod(n,k)==0; i++) n = floor(n/k)
				a = a\(k,i)
			}
		}
		
		if (n>1) a = a\(n,1)
		return(a)
	}
}
```



## Swift

{{trans|Python}}

Uses the sieve of Eratosthenes. This is generic on any type that conforms to BinaryInteger. So in theory any BigInteger library should work with it.


```swift>func primeDecomposition<T: BinaryInteger
(of n: T) -> [T] {
  guard n > 2 else { return [] }

  func step(_ x: T) -> T {
    return 1 + (x << 2) - ((x >> 1) << 1)
  }

  let maxQ = T(Double(n).squareRoot())
  var d: T = 1
  var q: T = n % 2 == 0 ? 2 : 3

  while q <= maxQ && n % q != 0 {
    q = step(d)
    d += 1
  }

  return q <= maxQ ? [q] + primeDecomposition(of: n / q) : [n]
}

for prime in Eratosthenes(upTo: 60) {
  let m = Int(pow(2, Double(prime))) - 1
  let decom = primeDecomposition(of: m)

  print("2^\(prime) - 1 = \(m) => \(decom)")
}
```


{{out}}

```txt
2^2 - 1 = 3 => [3]
2^3 - 1 = 7 => [7]
2^5 - 1 = 31 => [31]
2^7 - 1 = 127 => [127]
2^11 - 1 = 2047 => [23, 89]
2^13 - 1 = 8191 => [8191]
2^17 - 1 = 131071 => [131071]
2^19 - 1 = 524287 => [524287]
2^23 - 1 = 8388607 => [47, 178481]
2^29 - 1 = 536870911 => [233, 1103, 2089]
2^31 - 1 = 2147483647 => [2147483647]
2^37 - 1 = 137438953471 => [223, 616318177]
2^41 - 1 = 2199023255551 => [13367, 164511353]
2^43 - 1 = 8796093022207 => [431, 9719, 2099863]
2^47 - 1 = 140737488355327 => [2351, 4513, 13264529]
2^53 - 1 = 9007199254740991 => [6361, 69431, 20394401]
2^59 - 1 = 576460752303423487 => [179951, 3203431780337]
```



## Tcl


```tcl
proc factors {x} {
    # list the prime factors of x in ascending order
    set result [list]
    while {$x % 2 == 0} {
        lappend result 2
        set x [expr {$x / 2}]
    }
    for {set i 3} {$i*$i <= $x} {incr i 2} {
        while {$x % $i == 0} {
            lappend result $i
            set x [expr {$x / $i}]
        }
    }
    if {$x != 1} {lappend result $x}
    return $result
}

```

Testing

```tcl
foreach m {2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59} {
    set n [expr {2**$m - 1}]
    catch {time {set primes [factors $n]} 1} tm
    puts [format "2**%02d-1 = %-18s = %-22s => %s" $m $n [join $primes *] $tm]
}
```

{{out}}

```txt
2**02-1 = 3                  = 3                      => 184 microseconds per iteration
2**03-1 = 7                  = 7                      => 8 microseconds per iteration
2**05-1 = 31                 = 31                     => 8 microseconds per iteration
2**07-1 = 127                = 127                    => 23 microseconds per iteration
2**11-1 = 2047               = 23*89                  => 12 microseconds per iteration
2**13-1 = 8191               = 8191                   => 22 microseconds per iteration
2**17-1 = 131071             = 131071                 => 69 microseconds per iteration
2**19-1 = 524287             = 524287                 => 131 microseconds per iteration
2**23-1 = 8388607            = 47*178481              => 81 microseconds per iteration
2**29-1 = 536870911          = 233*1103*2089          => 199 microseconds per iteration
2**31-1 = 2147483647         = 2147483647             => 9509 microseconds per iteration
2**37-1 = 137438953471       = 223*616318177          => 4377 microseconds per iteration
2**41-1 = 2199023255551      = 13367*164511353        => 2389 microseconds per iteration
2**43-1 = 8796093022207      = 431*9719*2099863       => 1711 microseconds per iteration
2**47-1 = 140737488355327    = 2351*4513*13264529     => 802 microseconds per iteration
2**53-1 = 9007199254740991   = 6361*69431*20394401    => 13109 microseconds per iteration
2**59-1 = 576460752303423487 = 179951*3203431780337   => 316009 microseconds per iteration
```


=={{header|TI-83 BASIC}}==

```ti83b
::prgmPREMIER
Disp "FACTEURS PREMIER"
Prompt N
If N<1:Stop
ClrList L1,L2
0→K
iPart(√(N))→L
N→M
For(I,2,L)
0→J
While fPart(M/I)=0
J+1→J
M/I→M
End
If J≠0
Then
K+1→K
I→L1(K)
J→L2(K)
I→Z:prgmVSTR
"   "+Str0→Str1
If J≠1
Then
J→Z:prgmVSTR
Str1+"^"+Str0→Str1
End
Disp Str1
End
If M=1:Stop
End
If M≠1
Then
If M≠N
Then
M→Z:prgmVSTR
"   "+Str0→Str1
Disp Str1
Else
Disp "PREMIER"
End
End
::prgmVSTR
{Z,Z}→L5
{1,2}→L6
LinReg(ax+b)L6,L5,Y₀
Equ►String(Y₀,Str0)
length(Str0)→O
sub(Str0,4,O-3)→Str0
ClrList L5,L6
DelVar Y
```

{{out}}

```txt

FACTEURS PREMIER
N=?1047552
   2^10
      3
     11
     31

```



## TXR


{{trans|Common Lisp}}


```txr
@(next :args)
@(do 
  (defun factor (n)
    (if (> n 1)
      (for ((max-d (isqrt n))
            (d 2))
           ()
           ((inc d (if (evenp d) 1 2)))
        (cond ((> d max-d) (return (list n)))
              ((zerop (mod n d)) 
               (return (cons d (factor (trunc n d))))))))))
@{num /[0-9]+/}
@(bind factors @(factor (int-str num 10)))
@(output)
@num -> {@(rep)@factors, @(last)@factors@(end)}
@(end)
```

{{out}}

```txt
$ txr factor.txr 1139423842450982345
1139423842450982345 -> {5, 19, 37, 12782467, 25359769}
$ txr factor.txr 1
1 -> {}
$ txr factor.txr 2
2 -> {2}
$ txr factor.txr 3
3 -> {3}
$ txr factor.txr 2
2 -> {2}
$ txr factor.txr 3
3 -> {3}
$ txr factor.txr 4
4 -> {2, 2}
$ txr factor.txr 5
5 -> {5}
$ txr factor.txr 6
6 -> {2, 3}
```



## V

like in scheme (using variables)

```v
[prime-decomposition
   [inner [c p] let
       [c c * p >]
           [p unit]
           [ [p c % zero?]
                   [c c p c / inner cons]
                   [c 1 + p inner]
             ifte]
       ifte].
   2 swap inner].
```


(mostly) the same thing using stack (with out variables)

```v
[prime-decomposition
   [inner
       [dup * <]
           [pop unit]
           [ [% zero?]
                   [ [p c : [c p c / c]] view i inner cons]
                   [succ inner]
             ifte]
       ifte].
   2 inner].
```


Using it

```v
|1221 prime-decomposition puts
```

 =[3 11 37]


## VBScript


```vb
Function PrimeFactors(n)
	arrP = Split(ListPrimes(n)," ")
	divnum = n
	Do Until divnum = 1
		'The -1 is to account for the null element of arrP
		For i = 0 To UBound(arrP)-1
			If divnum = 1 Then
				Exit For
			ElseIf divnum Mod arrP(i) = 0 Then
				divnum = divnum/arrP(i)
				PrimeFactors = PrimeFactors & arrP(i) & " "
			End If
		Next
	Loop
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

WScript.StdOut.Write PrimeFactors(CInt(WScript.Arguments(0)))
WScript.StdOut.WriteLine
```


{{out}}

```txt

C:\>cscript /nologo primefactors.vbs 12
2 3 2

C:\>cscript /nologo primefactors.vbs 50
2 5 5

```



## XSLT

Let's assume that in XSLT the application of a template is similar to the invocation of a function. So when the following template 

```xml
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

    <xsl:template match="/numbers">
        <html>
            <body>
                <ul>
                    <xsl:apply-templates />
                </ul>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="number">
        <li>
            Number:
            <xsl:apply-templates mode="value" />
            Factors:
            <xsl:apply-templates mode="factors" />
        </li>
    </xsl:template>

    <xsl:template match="value" mode="value">
        <xsl:apply-templates />
    </xsl:template>

    <xsl:template match="value" mode="factors">
        <xsl:call-template name="generate">
            <xsl:with-param name="number" select="number(current())" />
            <xsl:with-param name="candidate" select="number(2)" />
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="generate">
        <xsl:param name="number" />
        <xsl:param name="candidate" />
        <xsl:choose>
            <!-- 1 is no prime and does not have any factors -->
            <xsl:when test="$number = 1"></xsl:when>
            <!-- if the candidate is larger than the sqrt of the number, it's prime and the last factor -->
            <xsl:when test="$candidate * $candidate &gt; $number"> 
                <xsl:value-of select="$number" />
            </xsl:when>
            <!-- if the number is factored by the candidate, add the factor and try again with the same factor -->
            <xsl:when test="$number mod $candidate = 0">
                <xsl:value-of select="$candidate" />
                <xsl:text> </xsl:text>
                <xsl:call-template name="generate">
                    <xsl:with-param name="number" select="$number div $candidate" />
                    <xsl:with-param name="candidate" select="$candidate" />
                </xsl:call-template>
            </xsl:when>
            <!-- else try again with the next factor -->
            <xsl:otherwise>
                <!-- increment by 2 to save stack depth -->
                <xsl:choose>
                    <xsl:when test="$candidate = 2">
                        <xsl:call-template name="generate">
                            <xsl:with-param name="number" select="$number" />
                            <xsl:with-param name="candidate" select="$candidate + 1" />
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="generate">
                            <xsl:with-param name="number" select="$number" />
                            <xsl:with-param name="candidate" select="$candidate + 2" />
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
```

is applied against the document

```xml><numbers

    <number><value>1</value></number>
    <number><value>2</value></number>
    <number><value>4</value></number>
    <number><value>8</value></number>
    <number><value>9</value></number>
    <number><value>255</value></number>
</numbers>
```

then the output contains the prime decomposition of each number:

```html><html

<body>
<ul>
    
<li>
            Number:
            1
            Factors:
            </li>
    
<li>
            Number:
            2
            Factors:
            2</li>
    
<li>
            Number:
            4
            Factors:
            2 2</li>
    
<li>
            Number:
            8
            Factors:
            2 2 2</li>
    
<li>
            Number:
            9
            Factors:
            3 3</li>
    
<li>
            Number:
            255
            Factors:
            3 5 17</li>
    
</ul>
</body>
</html>
```



## zkl

With 64 bit ints:

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


```zkl
foreach n in (T(5,12, 2147483648, 2199023255551, 8796093022207,
	9007199254740991, 576460752303423487)){
   println(n,": ",primeFactors(n).concat(", ")) 
}
```

{{out}}

```txt

5: 5
12: 2, 2, 3
2147483648: 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
2199023255551: 13367, 164511353
8796093022207: 431, 9719, 2099863
9007199254740991: 6361, 69431, 20394401
576460752303423487: 179951, 3203431780337

```

Unfortunately, big ints (GMP) don't have (quite) the same interface as ints (since there is no big float, BI.toFloat() truncates to a double so BI.toFloat().sqrt() is wrong). So mostly duplicate code is needed:

```zkl
fcn factorsBI(n){  // Return a list of factors of n
   acc:=fcn(n,k,acc,maxD){  // k is 2,3,5,7,9,... not optimum
      if(n==1 or k>maxD) acc.close();
      else{
	 q,r:=n.div2(k);   // divr-->(quotient,remainder)
	 if(r==0) return(self.fcn(q,k,acc.write(k),q.root(2)));
	 return(self.fcn(n,k+1+k.isOdd,acc,maxD))
      }
   }(n,2,Sink(List),n.root(2));
   m:=acc.reduce('*,BN(1));  // mulitply factors
   if(n!=m) acc.append(n/m); // opps, missed last factor
   else acc;
}
```


```zkl
var BN=Import("zklBigNum");
foreach n in (T(BN("12"),
	BN("340282366920938463463374607431768211455"))){
   println(n,": ",factorsBI(n).concat(", ")) 
}
```

{{out}}

```txt

12: 2, 2, 3
340282366920938463463374607431768211455: 3, 5, 17, 257, 641, 65537, 274177, 6700417, 67280421310721

```


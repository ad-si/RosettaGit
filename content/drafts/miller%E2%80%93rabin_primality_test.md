+++
title = "Miller–Rabin primality test"
description = ""
date = 2019-08-21T14:07:14Z
aliases = []
[extra]
id = 4106
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}{{Wikipedia|Miller–Rabin primality test}}


The [[wp:Miller–Rabin primality test|Miller–Rabin primality test]] or Rabin–Miller primality test is a primality test: an algorithm which determines whether a given number is prime or not.

The algorithm, as modified by [[wp:Michael O. Rabin|Michael O. Rabin]] to avoid the [[wp:generalized Riemann hypothesis|generalized Riemann hypothesis]], is a probabilistic algorithm.

The pseudocode, from [[wp:Miller-Rabin primality test#Algorithm_and_running_time|Wikipedia]] is:
 '''Input''': ''n'' > 2, an odd integer to be tested for primality;
        ''k'', a parameter that determines the accuracy of the test
 '''Output''': ''composite'' if ''n'' is composite, otherwise ''probably prime''
 write ''n'' − 1 as 2<sup>''s''</sup>·''d'' with ''d'' odd by factoring powers of 2 from ''n'' − 1
 LOOP: '''repeat''' ''k'' times:
    pick ''a'' randomly in the range [2, ''n'' − 1]
    ''x'' ← ''a''<sup>''d''</sup> mod ''n''
    '''if''' ''x'' = 1 or ''x'' = ''n'' − 1 '''then''' '''do''' '''next''' LOOP
    '''for''' ''r'' = 1 .. ''s'' − 1
       ''x'' ← ''x''<sup>2</sup> mod ''n''
       '''if''' ''x'' = 1 '''then''' '''return''' ''composite''
       '''if''' ''x'' = ''n'' − 1 '''then''' '''do''' '''next''' LOOP
    '''return''' ''composite''
 '''return''' ''probably prime''

* The nature of the test involves big numbers, so the use of "big numbers" libraries (or similar features of the language of your choice) are suggested, but '''not''' mandatory.
* Deterministic variants of the test exist and can be implemented as extra (not mandatory to complete the task)





## Ada



### ordinary integers


It's easy to get overflows doing exponential calculations.
Therefore I implemented my own function for that.

For Number types >= 2**64 you may have to use an external library -- see below.

First, a package Miller_Rabin is specified.
The same package is used elsewhere in Rosetta Code,
such as for the [[Carmichael 3 strong pseudoprimes]] the [[Extensible prime generator]], and the [[Emirp primes]].


```Ada
generic
   type Number is range <>;
package Miller_Rabin is

   type Result_Type is (Composite, Probably_Prime);

   function Is_Prime (N : Number; K : Positive := 10) return Result_Type;

end Miller_Rabin;
```


The implementation of that package is as follows:


```Ada
with Ada.Numerics.Discrete_Random;

package body Miller_Rabin is

   function Is_Prime (N : Number; K : Positive := 10)
                     return Result_Type
   is
      subtype Number_Range is Number range 2 .. N - 1;
      package Random is new Ada.Numerics.Discrete_Random (Number_Range);

      function Mod_Exp (Base, Exponent, Modulus : Number) return Number is
         Result : Number := 1;
      begin
         for E in 1 .. Exponent loop
            Result := Result * Base mod Modulus;
         end loop;
         return Result;
      end Mod_Exp;

      Generator : Random.Generator;
      D : Number := N - 1;
      S : Natural := 0;
      X : Number;
   begin
      -- exclude 2 and even numbers
      if N = 2 then
         return Probably_Prime;
      elsif N mod 2 = 0 then
         return Composite;
      end if;

      -- write N-1 as 2**S * D, with D mod 2 /= 0
      while D mod 2 = 0 loop
         D := D / 2;
         S := S + 1;
      end loop;

      -- initialize RNG
      Random.Reset (Generator);
      for Loops in 1 .. K loop
         X := Mod_Exp(Random.Random (Generator), D, N);
         if X /= 1 and X /= N - 1 then
        Inner : for R in 1 .. S - 1 loop
               X := Mod_Exp (X, 2, N);
               if X = 1 then return Composite; end if;
               exit Inner when X = N - 1;
            end loop Inner;
            if X /= N - 1 then return Composite; end if;
         end if;
      end loop;

      return Probably_Prime;
   end Is_Prime;

end Miller_Rabin;
```


Finally, the program itself:


```Ada
with Ada.Text_IO, Miller_Rabin;

procedure Mr_Tst is

   type Number is range 0 .. (2**48)-1;

   package Num_IO is new Ada.Text_IO.Integer_IO (Number);
   package Pos_IO is new Ada.Text_IO.Integer_IO (Positive);
   package MR     is new Miller_Rabin(Number); use MR;

   N : Number;
   K : Positive;

begin
   for I in Number(2) .. 1000 loop
      if Is_Prime (I) = Probably_Prime then
         Ada.Text_IO.Put (Number'Image (I));
      end if;
   end loop;
   Ada.Text_IO.Put_Line (".");

   Ada.Text_IO.Put ("Enter a Number: ");           Num_IO.Get (N);
   Ada.Text_IO.Put ("Enter the count of loops: "); Pos_IO.Get (K);
   Ada.Text_IO.Put_Line ("What is it? " & Result_Type'Image (Is_Prime(N, K)));
end MR_Tst;
```


{{out}}

```txt
 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997.
Enter a Number: 1234567
Enter the count of loops: 20
What is it? COMPOSITE
```



### using an external library to handle big integers


Using the big integer implementation from a cryptographic library [https://github.com/cforler/Ada-Crypto-Library/].


```Ada
with Ada.Text_IO, Crypto.Types.Big_Numbers, Ada.Numerics.Discrete_Random;

procedure Miller_Rabin is

   Bound: constant Positive := 256; -- can be any multiple of 32

   package LN is new Crypto.Types.Big_Numbers (Bound);
   use type LN.Big_Unsigned; -- all computations "mod 2**Bound"

   function "+"(S: String) return LN.Big_Unsigned
     renames LN.Utils.To_Big_Unsigned;

   function Is_Prime (N : LN.Big_Unsigned; K : Positive := 10) return Boolean is

      subtype Mod_32 is Crypto.Types.Mod_Type;
      use type Mod_32;
      package R_32 is new Ada.Numerics.Discrete_Random (Mod_32);
      Generator : R_32.Generator;

      function Random return LN.Big_Unsigned is
         X: LN.Big_Unsigned := LN.Big_Unsigned_Zero;
      begin
         for I in 1 .. Bound/32 loop
            X := (X * 2**16) * 2**16;
            X := X + R_32.Random(Generator);
         end loop;
         return X;
      end Random;

      D:    LN.Big_Unsigned := N - LN.Big_Unsigned_One;
      S:    Natural := 0;
      A, X: LN.Big_Unsigned;
   begin
      -- exclude 2 and even numbers
      if N = 2 then
         return True;
      elsif N mod 2 = LN.Big_Unsigned_Zero then
         return False;
      else

         -- write N-1 as 2**S * D, with odd D
         while D mod 2 = LN.Big_Unsigned_Zero loop
            D := D / 2;
            S := S + 1;
         end loop;

         -- initialize RNG
         R_32.Reset (Generator);

         -- run the real test
         for Loops in 1 .. K loop
            loop
               A := Random;
               exit when (A > 1) and (A < (N - 1));
            end loop;
            X := LN.Mod_Utils.Pow(A, D, N); -- X := (Random**D) mod N
            if X /= 1 and X /= N - 1 then
               Inner:
               for R in 1 .. S - 1 loop
                  X := LN.Mod_Utils.Pow(X, LN.Big_Unsigned_Two, N);
                  if X = 1 then
                     return False;
                  end if;
                  exit Inner when X = N - 1;
               end loop Inner;
               if X /= N - 1 then
                  return False;
               end if;
            end if;
         end loop;
      end if;

      return True;
   end Is_Prime;

   S: constant String :=
     "4547337172376300111955330758342147474062293202868155909489";
   T: constant String :=
     "4547337172376300111955330758342147474062293202868155909393";

   K: constant Positive := 10;

begin
   Ada.Text_IO.Put_Line("Prime(" & S & ")=" & Boolean'Image(Is_Prime(+S, K)));
   Ada.Text_IO.Put_Line("Prime(" & T & ")=" & Boolean'Image(Is_Prime(+T, K)));
end Miller_Rabin;
```


{{out}}

```txt
Prime(4547337172376300111955330758342147474062293202868155909489)=TRUE
Prime(4547337172376300111955330758342147474062293202868155909393)=FALSE
```


Using the built-in Miller-Rabin test from the same library:


```Ada
with Ada.Text_IO, Crypto.Types.Big_Numbers, Ada.Numerics.Discrete_Random;

procedure Miller_Rabin is

   Bound: constant Positive := 256; -- can be any multiple of 32

   package LN is new Crypto.Types.Big_Numbers (Bound);
   use type LN.Big_Unsigned; -- all computations "mod 2**Bound"

   function "+"(S: String) return LN.Big_Unsigned
     renames LN.Utils.To_Big_Unsigned;

   S: constant String :=
     "4547337172376300111955330758342147474062293202868155909489";
   T: constant String :=
     "4547337172376300111955330758342147474062293202868155909393";

   K: constant Positive := 10;


begin
   Ada.Text_IO.Put_Line("Prime(" & S & ")="
       & Boolean'Image (LN.Mod_Utils.Passed_Miller_Rabin_Test(+S, K)));
   Ada.Text_IO.Put_Line("Prime(" & T & ")="
       & Boolean'Image (LN.Mod_Utils.Passed_Miller_Rabin_Test(+T, K)));
end Miller_Rabin;
```


The output is the same.


## ALGOL 68

{{trans|python}}
{{works with|ALGOL 68|Standard - with preludes manually inserted }}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has no FORMATted transput, also it generates a call to undefined C LONG externals }} -->

```algol68
MODE LINT=LONG INT;
MODE LOOPINT = INT;

MODE POWMODSTRUCT = LINT;
PR READ "prelude/pow_mod.a68" PR;

PROC miller rabin = (LINT n, LOOPINT k)BOOL: (
    IF n<=3 THEN TRUE
    ELIF NOT ODD n THEN FALSE
    ELSE
        LINT d := n - 1;
        INT s := 0;
        WHILE NOT ODD d DO
            d := d OVER 2;
            s +:= 1
        OD;
        TO k DO
            LINT a := 2 + ENTIER (random*(n-3));
            LINT x :=  pow mod(a, d, n);
            IF x /= 1 THEN
                TO s DO
                    IF x = n-1 THEN done FI;
                    x := x*x %* n
                OD;
                else: IF x /= n-1 THEN return false FI;
                done: EMPTY
            FI
        OD;
        TRUE EXIT
        return false: FALSE
    FI
);

FOR i FROM 937 TO 1000 DO
  IF miller rabin(i, 10) THEN
    print((" ",whole(i,0)))
  FI
OD
```

{{out}}

```txt

 937 941 947 953 967 971 977 983 991 997

```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/post-276712.html#276712 discussion]

```AutoHotkey
MsgBox % MillerRabin(999983,10) ; 1
MsgBox % MillerRabin(999809,10) ; 1
MsgBox % MillerRabin(999727,10) ; 1
MsgBox % MillerRabin(52633,10)  ; 0
MsgBox % MillerRabin(60787,10)  ; 0
MsgBox % MillerRabin(999999,10) ; 0
MsgBox % MillerRabin(999995,10) ; 0
MsgBox % MillerRabin(999991,10) ; 0

MillerRabin(n,k) { ; 0: composite, 1: probable prime (n < 2**31)
   d := n-1, s := 0
   While !(d&1)
      d>>=1, s++

   Loop %k% {
      Random a, 2, n-2 ; if n < 4,759,123,141, it is enough to test a = 2, 7, and 61.
      x := PowMod(a,d,n)
      If (x=1 || x=n-1)
         Continue
      Cont := 0
      Loop % s-1 {
         x := PowMod(x,2,n)
         If (x = 1)
            Return 0
         If (x = n-1) {
            Cont = 1
            Break
         }
      }
      IfEqual Cont,1, Continue
      Return 0
   }
   Return 1
}

PowMod(x,n,m) { ; x**n mod m
   y := 1, i := n, z := x
   While i>0
      y := i&1 ? mod(y*z,m) : y, z := mod(z*z,m), i >>= 1
   Return y
}
```



## bc

Requires a <tt>bc</tt> with long names.
{{works with|OpenBSD bc}}
(A previous version worked with [[GNU bc]].)

```bc
seed = 1   /* seed of the random number generator */
scale = 0

/* Random number from 0 to 32767. */
define rand() {
  /* Cheap formula (from POSIX) for random numbers of low quality. */
  seed = (seed * 1103515245 + 12345) % 4294967296
  return ((seed / 65536) % 32768)
}

/* Random number in range [from, to]. */
define rangerand(from, to) {
  auto b, h, i, m, n, r

  m = to - from + 1
  h = length(m) / 2 + 1  /* want h iterations of rand() % 100 */
  b = 100 ^ h % m        /* want n >= b */
  while (1) {
    n = 0                /* pick n in range [b, 100 ^ h) */
    for (i = h; i > 0; i--) {
      r = rand()
      while (r < 68) { r = rand(); }  /* loop if the modulo bias */
      n = (n * 100) + (r % 100)       /* append 2 digits to n */
    }
    if (n >= b) { break; }  /* break unless the modulo bias */
  }
  return (from + (n % m))
}



/* n is probably prime? */
define miller_rabin_test(n, k) {
  auto d, r, a, x, s

  if (n <= 3) { return (1); }
  if ((n % 2) == 0) { return (0); }

  /* find s and d so that d * 2^s = n - 1 */
  d = n - 1
  s = 0
  while((d % 2) == 0) {
     d /= 2
     s += 1
  }

  while (k-- > 0) {
    a = rangerand(2, n - 2)
    x = (a ^ d) % n
    if (x != 1) {
      for (r = 0; r < s; r++) {
        if (x == (n - 1)) { break; }
        x = (x * x) % n
      }
      if (x != (n - 1)) {
        return (0)
      }
    }
  }
  return (1)
}

for (i = 1; i < 1000; i++) {
  if (miller_rabin_test(i, 10) == 1) {
    i
  }
}
quit
```


## Bracmat

{{trans|bc}}

```bracmat
( 1:?seed
& ( rand
  =
    .   mod$(!seed*1103515245+12345.4294967296):?seed
      & mod$(div$(!seed.65536).32768)
  )
& ( rangerand
  =   from to b h i m n r length
    .   !arg:(?from,?to)
      & !to+-1*!from+1:?m
      & @(!m:? [?length)
      & div$(!length+1.2)+1:?h
      & 100^mod$(!h.!m):?b
      &   whl
        ' ( 0:?n
          & !h+1:?i
          &   whl
            ' ( !i+-1:>0:?i
              & rand$:?r
              & whl'(!r:<68&rand$:?r)
              & !n*100+mod$(!r.100):?n
              )
          & !n:>!b
          )
      & !from+mod$(!n.!m)
  )
& ( miller-rabin-test
  =   n k d r a x s return
    .   !arg:(?n,?k)
      & ( !n:~>3&1
        | mod$(!n.2):0
        |   !n+-1:?d
          & 0:?s
          &   whl
            ' ( mod$(!d.2):0
              & !d*1/2:?d
              & 1+!s:?s
              )
          & 1:?return
          &   whl
            ' ( !k+-1:?k:~<0
              & rangerand$(2,!n+-2):?a
              & mod$(!a^!d.!n):?x
              & ( !x:1
                |   0:?r
                  &   whl
                    ' ( !r+1:~>!s:?r
                      & !n+-1:~!x
                      & mod$(!x*!x.!n):?x
                      )
                  & ( !n+-1:!x
                    | 0:?return&~
                    )
                )
              )
          & !return
        )
  )
& 0:?i
& :?primes
&   whl
  ' ( 1+!i:<1000:?i
    & (   miller-rabin-test$(!i,10):1
        & !primes !i:?primes
      |
      )
    )
& !primes:? [-11 ?last
& out$!last
);
```

{{out}}

```txt
937 941 947 953 967 971 977 983 991 997
```



## C

{{libheader|GMP}}
'''miller-rabin.h'''

```c
#ifndef _MILLER_RABIN_H_
#define _MILLER_RABIN_H
#include <gmp.h>
bool miller_rabin_test(mpz_t n, int j);
#endif
```

'''miller-rabin.c'''
{{trans|Fortran}}
For <code>decompose</code> (and header <tt>primedecompose.h</tt>),
see [[Prime decomposition#C|Prime decomposition]].

```c
#include <stdbool.h>
#include <gmp.h>
#include "primedecompose.h"

#define MAX_DECOMPOSE 100

bool miller_rabin_test(mpz_t n, int j)
{
  bool res;
  mpz_t f[MAX_DECOMPOSE];
  mpz_t s, d, a, x, r;
  mpz_t n_1, n_3;
  gmp_randstate_t rs;
  int l=0, k;

  res = false;
  gmp_randinit_default(rs);

  mpz_init(s); mpz_init(d);
  mpz_init(a); mpz_init(x); mpz_init(r);
  mpz_init(n_1); mpz_init(n_3);

  if ( mpz_cmp_si(n, 3) <= 0 ) { // let us consider 1, 2, 3 as prime
    gmp_randclear(rs);
    return true;
  }
  if ( mpz_odd_p(n) != 0 ) {
    mpz_sub_ui(n_1, n, 1);         //  n-1
    mpz_sub_ui(n_3, n, 3);         //  n-3
    l = decompose(n_1, f);
    mpz_set_ui(s, 0);
    mpz_set_ui(d, 1);
    for(k=0; k < l; k++) {
      if ( mpz_cmp_ui(f[k], 2) == 0 )
	mpz_add_ui(s, s, 1);
      else
	mpz_mul(d, d, f[k]);
    }                             // 2^s * d = n-1
    while(j-- > 0) {
      mpz_urandomm(a, rs, n_3);     // random from 0 to n-4
      mpz_add_ui(a, a, 2);          // random from 2 to n-2
      mpz_powm(x, a, d, n);
      if ( mpz_cmp_ui(x, 1) == 0 ) continue;
      mpz_set_ui(r, 0);
      while( mpz_cmp(r, s) < 0 ) {
	if ( mpz_cmp(x, n_1) == 0 ) break;
	mpz_powm_ui(x, x, 2, n);
	mpz_add_ui(r, r, 1);
      }
      if ( mpz_cmp(x, n_1) == 0 ) continue;
      goto flush; // woops
    }
    res = true;
  }

flush:
  for(k=0; k < l; k++) mpz_clear(f[k]);
  mpz_clear(s); mpz_clear(d);
  mpz_clear(a); mpz_clear(x); mpz_clear(r);
  mpz_clear(n_1); mpz_clear(n_3);
  gmp_randclear(rs);
  return res;
}
```

'''Testing'''

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gmp.h>
#include "miller-rabin.h"

#define PREC 10
#define TOP  4000

int main()
{
  mpz_t num;

  mpz_init(num);
  mpz_set_ui(num, 1);

  while ( mpz_cmp_ui(num, TOP) < 0 ) {
    if ( miller_rabin_test(num, PREC) ) {
      gmp_printf("%Zd maybe prime\n", num);
    } /*else {
      gmp_printf("%Zd not prime\n", num);
      }*/ // remove the comment iff you're interested in
          // sure non-prime.
    mpz_add_ui(num, num, 1);
  }

  mpz_clear(num);
  return EXIT_SUCCESS;
}
```



===Deterministic up to 341,550,071,728,321===

```c
// calcul a^n%mod
size_t power(size_t a, size_t n, size_t mod)
{
    size_t power = a;
    size_t result = 1;

    while (n)
    {
        if (n & 1)
            result = (result * power) % mod;
        power = (power * power) % mod;
        n >>= 1;
    }
    return result;
}

// n−1 = 2^s * d with d odd by factoring powers of 2 from n−1
bool witness(size_t n, size_t s, size_t d, size_t a)
{
    size_t x = power(a, d, n);
    size_t y;

    while (s) {
        y = (x * x) % n;
        if (y == 1 && x != 1 && x != n-1)
            return false;
        x = y;
        --s;
    }
    if (y != 1)
        return false;
    return true;
}

/*
 * if n < 1,373,653, it is enough to test a = 2 and 3;
 * if n < 9,080,191, it is enough to test a = 31 and 73;
 * if n < 4,759,123,141, it is enough to test a = 2, 7, and 61;
 * if n < 1,122,004,669,633, it is enough to test a = 2, 13, 23, and 1662803;
 * if n < 2,152,302,898,747, it is enough to test a = 2, 3, 5, 7, and 11;
 * if n < 3,474,749,660,383, it is enough to test a = 2, 3, 5, 7, 11, and 13;
 * if n < 341,550,071,728,321, it is enough to test a = 2, 3, 5, 7, 11, 13, and 17.
 */

bool is_prime_mr(size_t n)
{
    if (((!(n & 1)) && n != 2 ) || (n < 2) || (n % 3 == 0 && n != 3))
        return false;
    if (n <= 3)
        return true;

    size_t d = n / 2;
    size_t s = 1;
    while (!(d & 1)) {
        d /= 2;
        ++s;
    }

    if (n < 1373653)
        return witness(n, s, d, 2) && witness(n, s, d, 3);
    if (n < 9080191)
        return witness(n, s, d, 31) && witness(n, s, d, 73);
    if (n < 4759123141)
        return witness(n, s, d, 2) && witness(n, s, d, 7) && witness(n, s, d, 61);
    if (n < 1122004669633)
        return witness(n, s, d, 2) && witness(n, s, d, 13) && witness(n, s, d, 23) && witness(n, s, d, 1662803);
    if (n < 2152302898747)
        return witness(n, s, d, 2) && witness(n, s, d, 3) && witness(n, s, d, 5) && witness(n, s, d, 7) && witness(n, s, d, 11);
    if (n < 3474749660383)
        return witness(n, s, d, 2) && witness(n, s, d, 3) && witness(n, s, d, 5) && witness(n, s, d, 7) && witness(n, s, d, 11) && witness(n, s, d, 13);
    return witness(n, s, d, 2) && witness(n, s, d, 3) && witness(n, s, d, 5) && witness(n, s, d, 7) && witness(n, s, d, 11) && witness(n, s, d, 13) && witness(n, s, d, 17);
}
```

Inspiration from http://stackoverflow.com/questions/4424374/determining-if-a-number-is-prime



=={{header|C sharp|C#}}==

```csharp
public static class RabinMiller
{
    public static bool IsPrime(int n, int k)
    {
        if ((n < 2) || (n % 2 == 0)) return (n == 2);

        int s = n - 1;
        while (s % 2 == 0)  s >>= 1;

        Random r = new Random();
        for (int i = 0; i < k; i++)
        {
            int a = r.Next(n - 1) + 1;
            int temp = s;
            long mod = 1;
            for (int j = 0; j < temp; ++j)  mod = (mod * a) % n;
            while (temp != n - 1 && mod != 1 && mod != n - 1)
            {
                mod = (mod * mod) % n;
                temp *= 2;
            }

            if (mod != n - 1 && temp % 2 == 0) return false;
        }
        return true;
    }
}
```

[https://stackoverflow.com/questions/7860802/miller-rabin-primality-test] Corrections made 6/21/2017




```csharp
// Miller-Rabin primality test as an extension method on the BigInteger type.
// Based on the Ruby implementation on this page.
public static class BigIntegerExtensions
{
  public static bool IsProbablePrime(this BigInteger source, int certainty)
  {
    if(source == 2 || source == 3)
      return true;
    if(source < 2 || source % 2 == 0)
      return false;

    BigInteger d = source - 1;
    int s = 0;

    while(d % 2 == 0)
    {
      d /= 2;
      s += 1;
    }

    // There is no built-in method for generating random BigInteger values.
    // Instead, random BigIntegers are constructed from randomly generated
    // byte arrays of the same length as the source.
    RandomNumberGenerator rng = RandomNumberGenerator.Create();
    byte[] bytes = new byte[source.ToByteArray().LongLength];
    BigInteger a;

    for(int i = 0; i < certainty; i++)
    {
      do
      {
        // This may raise an exception in Mono 2.10.8 and earlier.
        // http://bugzilla.xamarin.com/show_bug.cgi?id=2761
        rng.GetBytes(bytes);
        a = new BigInteger(bytes);
      }
      while(a < 2 || a >= source - 2);

      BigInteger x = BigInteger.ModPow(a, d, source);
      if(x == 1 || x == source - 1)
        continue;

      for(int r = 1; r < s; r++)
      {
        x = BigInteger.ModPow(x, 2, source);
        if(x == 1)
          return false;
        if(x == source - 1)
          break;
      }

      if(x != source - 1)
        return false;
    }

    return true;
  }
}
```



## Clojure


### Random Approach


```lisp
(ns test-p.core
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.set :as set]))

(def WITNESSLOOP "witness")
(def COMPOSITE "composite")

(defn m* [p q m]
  " Computes (p*q) mod m "
  (mod (*' p q) m))

(defn power
  "modular exponentiation (i.e. b^e mod m"
  [b e m]
  (loop [b b, e e, x 1]
    (if (zero? e)
      x
      (if (even? e) (recur (m* b b m) (quot e 2) x)
                    (recur (m* b b m) (quot e 2) (m* b x m))))))

; Sequence of random numbers to use in the test
(defn rand-num [n]
  " random number between 2 and n-2 "
  (bigint (math/floor (+' 2 (*' (- n 4) (rand))))))

; Unique set of random numbers
(defn unique-random-numbers [n k]
  " k unique random numbers between 2 and n-2 "
  (loop [a-set #{}]
    (cond
      (>= (count a-set) k) a-set
      :else (recur (conj a-set (rand-num n))))))

(defn find-d-s [n]
  " write n − 1 as 2s·d with d odd "
  (loop [d (dec n), s 0]
    (if (odd? d)
      [d s]
      (recur (quot d 2) (inc s)))))

(defn random-test
  ([n] (random-test n (min 1000 (bigint (/ n 2)))))
  ([n k]
  " Random version of primality test"
  (let [[d s] (find-d-s n)
        ; Individual Primality Test
        single-test (fn [a s]
                      (let [z (power a d n)]
                       (if (some #{z} [1 (dec n)])
                         WITNESSLOOP
                         (loop [x (power z 2 n), r s]
                           (cond
                             (= x 1) COMPOSITE
                             (= x (dec n)) WITNESSLOOP
                             (= r 0) COMPOSITE
                             :else (recur (power x 2 n) (dec r)))))))]
    ; Apply Test
    ;(not-any? #(= COMPOSITE (local-test % s))
    ;          (unique-random-numbers n k))))
    (not-any? #(= COMPOSITE (single-test % s)) (unique-random-numbers n k)))))

;; Testing
(println "Primes beteen 900-1000:")
(doseq [q (range 900 1000)
        :when (random-test q)]
  (print " " q))
(println)
(println "Is Prime?" 4547337172376300111955330758342147474062293202868155909489 (random-test 4547337172376300111955330758342147474062293202868155909489))
(println "Is Prime?" 4547337172376300111955330758342147474062293202868155909393 (random-test 4547337172376300111955330758342147474062293202868155909393))
(println "Is Prime?" 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (random-test 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))

(println "Is Prime?" 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (random-test 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))

```

{{Output}}

```txt

Primes beteen 900-1000:
  907  911  919  929  937  941  947  953  967  971  977  983  991  997
Is Prime? 4547337172376300111955330758342147474062293202868155909489N true
Is Prime? 4547337172376300111955330758342147474062293202868155909393N false
Is Prime? 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153N true
Is Prime? 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153N false

```


### Deterministic Approach


```lisp
(ns test-p.core
  (:require [clojure.math.numeric-tower :as math]))

(def WITNESSLOOP "witness")
(def COMPOSITE "composite")

(defn m* [p q m]
  " Computes (p*q) mod m "
  (mod (*' p q) m))

(defn power
  "modular exponentiation (i.e. b^e mod m"
  [b e m]
  (loop [b b, e e, x 1]
    (if (zero? e)
      x
      (if (even? e) (recur (m* b b m) (quot e 2) x)
                    (recur (m* b b m) (quot e 2) (m* b x m))))))

(defn find-d-s [n]
" write n − 1 as 2s·d with d odd "
(loop [d (dec n), s 0]
  (if (odd? d)
    [d s]
    (recur (quot d 2) (inc s)))))

;; Deterministic Test
(defn individual-deterministic-test [a d n s]
  " Deterministic Primality Test "
  (let [z (power a d n)]
    (if (= z 1)
      WITNESSLOOP
      (loop [x z, r s]
        (cond
          (= x (dec n)) WITNESSLOOP
          (zero? r) COMPOSITE
          :else (recur (power x 2 n) (dec r)))))))

(defn deterministic-test [n]
  " Sequence of Primality Tests "
  (cond
    (some #{n} [0 1 4]) false
    (some #{n} [2 3]) true
    (even? n) false
    :else (let [[d s] (find-d-s n)]
            (cond
              (< n 2047) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s)) [2 ])
              (< n 1373653) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 3])
              (< n 9090191) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [31 73])
              (< n 25326001) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 3 5])
              (< n 3215031751) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 3 5 7])
              (< n 1122004669633) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 13 23 1662803])
              (< n 2152302898747) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 3 5 7 11])
              (< n 2152302898747) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 3 5 7 11])
              (< n 3474749660383) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 3 5 7 11 13])
              (< n 341550071728321) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s))  [2 3 5 7 11 13 17])
              (< n 3825123056546,413,051) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s)) [2 3 5 7 11 13 17 19 23])
              (< n (math/expt 2 64) ) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s)) [2 3 5 7 11 13 17 19 23 29 31 37])
              (< n 318665857834031151167461) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s)) [2 3 5 7 11 13 17 19 23 29 31 37])
              (< n 3317044064679887385961981) (not-any? #(= COMPOSITE (individual-deterministic-test % d n s)) [2 3 5 7 11 13 17 19 23 29 31 37 41])
              :else (let [k (min (dec n) (int (math/expt (Math/log n) 2)))]
                      (not-any? #(= COMPOSITE (individual-deterministic-test % d n s)) (range 2 (inc k))))))))


;; Testing
(println "Primes beteen 900-1000:")
(doseq [q (range 900 1000)
        :when (deterministic-test q)]
  (print " " q))
(println)
(println "Is Prime?" 4547337172376300111955330758342147474062293202868155909489 (deterministic-test 4547337172376300111955330758342147474062293202868155909489))
(println "Is Prime?" 4547337172376300111955330758342147474062293202868155909393 (deterministic-test 4547337172376300111955330758342147474062293202868155909393))
println "Is Prime?" 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (deterministic-test 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))

(println "Is Prime?" 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (deterministic-test 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))
(println "Is Prime?" 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (deterministic-test 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))

(println "Is Prime?" 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (deterministic-test 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))

```

{{Output}}

```txt

Primes beteen 900-1000:
  907  911  919  929  937  941  947  953  967  971  977  983  991  997
Is Prime? 4547337172376300111955330758342147474062293202868155909489N true
Is Prime? 4547337172376300111955330758342147474062293202868155909393N false
(println "Is Prime?" 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (deterministic-test 643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))

(println "Is Prime?" 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153
         (deterministic-test 743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153))

```



## Common Lisp


```lisp
(defun factor-out (number divisor)
  "Return two values R and E such that NUMBER = DIVISOR^E * R,
  and R is not divisible by DIVISOR."
  (do ((e 0 (1+ e))
       (r number (/ r divisor)))
      ((/= (mod r divisor) 0) (values r e))))

(defun mult-mod (x y modulus) (mod (* x y) modulus))

(defun expt-mod (base exponent modulus)
  "Fast modular exponentiation by repeated squaring."
  (labels ((expt-mod-iter (b e p)
             (cond ((= e 0) p)
                   ((evenp e)
                    (expt-mod-iter (mult-mod b b modulus)
                                   (/ e 2)
                                   p))
                   (t
                    (expt-mod-iter b
                                   (1- e)
                                   (mult-mod b p modulus))))))
    (expt-mod-iter base exponent 1)))

(defun random-in-range (lower upper)
  "Return a random integer from the range [lower..upper]."
  (+ lower (random (+ (- upper lower) 1))))

(defun miller-rabin-test (n k)
  "Test N for primality by performing the Miller-Rabin test K times.
  Return NIL if N is composite, and T if N is probably prime."
  (cond ((= n 1)   nil)
        ((< n 4)     t)
        ((evenp n) nil)
        (t
         (multiple-value-bind (d s) (factor-out (- n 1) 2)
           (labels ((strong-liar? (a)
                      (let ((x (expt-mod a d n)))
                        (or (= x 1)
                            (loop repeat s
                                  for y = x then (mult-mod y y n)
                                  thereis (= y (- n 1)))))))
             (loop repeat k
                   always (strong-liar? (random-in-range 2 (- n 2)))))))))
```


```txt

CL-USER> (last (loop for i from 1 to 1000
                     when (miller-rabin-test i 10)
                     collect i)
               10)
(937 941 947 953 967 971 977 983 991 997)

```



## Crystal

=== This is a correct M-R test implementation for using bases > input. ===

### = It is a direct translation of the Ruby version for arbitrary sized integers. =

==== It is deterministic for all integers < 3_317_044_064_679_887_385_961_981.====
==== Increase 'primes' array members for more "confidence" past this value. ====


```ruby

require "big"

module Primes
  module MillerRabin

    # Returns true if +self+ is a prime number, else returns false.
    def primemr?
      primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47}
      return primes.includes? self if self <= primes.last
      modp47 = 614_889_782_588_491_410               # => primes.reduce(:*), largest < 2^64
      return false if modp47.gcd(self.to_big_i) != 1 # eliminates 86.2% of all integers
      n = typeof(self).new(self)
      # wits = [range, [wit_prms]] or nil
      wits = WITNESS_RANGES.find {|range, wits| range > n}
      witnesses = wits && wits[1] || primes
      miller_rabin_test(witnesses)
    end

    # Returns true if +self+ passes Miller-Rabin Test on witnesses +b+
    private def miller_rabin_test(witnesses) # list of witnesses for testing
      neg_one_mod = n = d = self - 1 # these are even as self is always odd
      while (d & 0xf) == 0; d >>= 4 end             # suck out factors of 2
      (d >>= (d & 3)^2; d >>= (d & 1)^1) if d.even? # 4 bits at a time
      witnesses.each do |b|          # do M-R test with each witness base
        next if (b % self) == 0      # **skip base if a multiple of input**
        y = powmod(b, d, self)       # y = (b**d) mod self
        s = d
        until s == n || y == 1 || y == neg_one_mod
          y = (y * y) % self         # y = (y**2) mod self
          s <<= 1
        end
        return false unless y == neg_one_mod || s.odd?
      end
      true
    end

    # Compute b**e mod m
    private def powmod(b, e, m)
      r = 1.to_big_i; b = b.to_big_i % m
      while e > 0
        r = (r * b) % m if e.odd?
        b = (b * b) % m
        e >>= 1
      end
      r
    end

    # Best known deterministic witnesses for given range and set of bases
    # https://miller-rabin.appspot.com/
    # https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_te
    private WITNESS_RANGES = {
      341_531 => {9345883071009581737},
      1_050_535_501 => {336781006125, 9639812373923155},
      350_269_456_337 => {4230279247111683200, 14694767155120705706, 16641139526367750375},
      55_245_642_489_451 => {2, 141889084524735, 1199124725622454117, 11096072698276303650},
      7_999_252_175_582_851 => {2, 4130806001517, 149795463772692060, 186635894390467037,
                                3967304179347715805},
      585_226_005_592_931_977 => {2, 123635709730000, 9233062284813009, 43835965440333360,
                                  761179012939631437, 1263739024124850375},
      18_446_744_073_709_551_615 => {2, 325, 9375, 28178, 450775, 9780504, 1795265022},
      "318665857834031151167461".to_big_i  => {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37},
      "3317044064679887385961981".to_big_i => {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41}
    }
  end
end

struct Int; include Primes::MillerRabin end

def tm; t = Time.now; yield; Time.now - t end

# 10 digit prime
n = 2147483647
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 18 digit non-prime
n = 844674407370955389
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 19 digit prime
n = 9241386435364257883.to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 20 digit prime; largest < 2^64
n = 18446744073709551533.to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 58 digit prime
n = "4547337172376300111955330758342147474062293202868155909489".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 58 digit non-prime
n = "4547337172376300111955330758342147474062293202868155909393".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 81 digit prime
n = "100000000000000000000000000000000000000000000000000000000000000000000000001309503".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 81 digit non-prime
n = "100000000000000000000000000000000000000000000000000000000000000000000000001309509".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 308 digit prime
n = "94366396730334173383107353049414959521528815310548187030165936229578960209523421808912459795329035203510284576187160076386643700441216547732914250578934261891510827140267043592007225160798348913639472564715055445201512461359359488795427875530231001298552452230535485049737222714000227878890892901228389026881".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = "138028649176899647846076023812164793645371887571371559091892986639999096471811910222267538577825033963552683101137782650479906670021895135954212738694784814783986671046107023185842481502719762055887490765764329237651328922972514308635045190654896041748716218441926626988737664133219271115413563418353821396401".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = "123301261697053560451930527879636974557474268923771832437126939266601921428796348203611050423256894847735769138870460373141723679005090549101566289920247264982095246187318303659027201708559916949810035265951104246512008259674244307851578647894027803356820480862664695522389066327012330793517771435385653616841".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = "119432521682023078841121052226157857003721669633106050345198988740042219728400958282159638484144822421840470442893056822510584029066504295892189315912923804894933736660559950053226576719285711831138657839435060908151231090715952576998400120335346005544083959311246562842277496260598128781581003807229557518839".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = "132082885240291678440073580124226578272473600569147812319294626601995619845059779715619475871419551319029519794232989255381829366374647864619189704922722431776563860747714706040922215308646535910589305924065089149684429555813953571007126408164577035854428632242206880193165045777949624510896312005014225526731".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = "153410708946188157980279532372610756837706984448408515364579602515073276538040155990230789600191915021209039203172105094957316552912585741177975853552299222501069267567888742458519569317286299134843250075228359900070009684517875782331709619287588451883575354340318132216817231993558066067063143257425853927599".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = "103130593592068072608023213244858971741946977638988649427937324034014356815504971087381663169829571046157738503075005527471064224791270584831779395959349442093395294980019731027051356344056416276026592333932610954020105156667883269888206386119513058400355612571198438511950152690467372712488391425876725831041".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = "94366396730334173383107353049414959521528815310548187030165936229578960209523421808912459795329035203510284576187160076386643700441216547732914250578934261891510827140267043592007225160798348913639472564715055445201512461359359488795427875530231001298552452230535485049737222714000227878890892901228389026881".to_big_i
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts


```



## D

{{trans|Ruby}}

```d
import std.random;

bool isProbablePrime(in ulong n, in uint k=10) /*nothrow*/ @safe /*@nogc*/ {
    static ulong modPow(ulong b, ulong e, in ulong m)
    pure nothrow @safe @nogc {
        ulong result = 1;
        while (e > 0) {
            if ((e & 1) == 1)
                result = (result * b) % m;
            b = (b ^^ 2) % m;
            e >>= 1;
        }
        return result;
    }

    if (n < 2 || n % 2 == 0)
        return n == 2;

    ulong d = n - 1;
    ulong s = 0;
    while (d % 2 == 0) {
        d /= 2;
        s++;
    }
    assert(2 ^^ s * d == n - 1);

    outer:
    foreach (immutable _; 0 .. k) {
        immutable ulong a = uniform(2, n);
        ulong x = modPow(a, d, n);
        if (x == 1 || x == n - 1)
            continue;
        foreach (immutable __; 1 .. s) {
            x = modPow(x, 2, n);
            if (x == 1)
                return false;
            if (x == n - 1)
                continue outer;
        }
        return false;
    }

    return true;
}

void main() { // Demo code.
    import std.stdio, std.range, std.algorithm;

    iota(2, 30).filter!isProbablePrime.writeln;
}
```

{{out}}

```txt
[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
```



## E


```e
def millerRabinPrimalityTest(n :(int > 0), k :int, random) :boolean {
  if (n <=> 2 || n <=> 3) { return true }
  if (n <=> 1 || n %% 2 <=> 0) { return false }
  var d := n - 1
  var s := 0
  while (d %% 2 <=> 0) {
    d //= 2
    s += 1
  }
  for _ in 1..k {
     def nextTrial := __continue
     def a := random.nextInt(n - 3) + 2     # [2, n - 2] = [0, n - 4] + 2 = [0, n - 3) + 2
     var x := a**d %% n                     # Note: Will do optimized modular exponentiation
     if (x <=> 1 || x <=> n - 1) { nextTrial() }
     for _ in 1 .. (s - 1) {
        x := x**2 %% n
        if (x <=> 1) { return false }
        if (x <=> n - 1) { nextTrial() }
     }
     return false
  }
  return true
}
```


```e
for i ? (millerRabinPrimalityTest(i, 1, entropy)) in 4..1000 {
  print(i, " ")
}
println()
```



## EchoLisp

EchoLisp natively implement the '''prime?''' function = Miller-Rabin tests for big integers. The definition is as follows :

```scheme

(lib 'bigint)

;; output : #t if n probably prime
(define (miller-rabin n (k 7) (composite #f)(x))
(define d (1- n))
(define s 0)
(define a 0)
(while (even? d)
	(set! s (1+ s))
	(set! d (quotient d 2)))

(for [(i k)]
	(set! a (+ 2 (random (- n 3))))
	(set! x (powmod a d n))
	#:continue (or (= x 1) (= x (1- n)))
	(set! composite
	(for [(r (in-range 1 s))]
		(set! x (powmod x 2 n))
		#:break (= x 1) => #t
		#:break (= x (1- n)) =>  #f
		#t
		))
	 #:break composite => #f )
 (not composite))

;; output
(miller-rabin #101)
    → #t
(miller-rabin #111)
    → #f
(define big-prime (random-prime 1e+100))
3461396142610375479080862553800503306376298093021233334170610435506057862777898396429
6627816219192601527
(miller-rabin big-prime)
    → #t
(miller-rabin (1+ (factorial 100)))
    → #f
(prime? (1+ (factorial 100))) ;; native
    → #f

```



## Elixir


```elixir

defmodule Prime do
  use Application
  alias :math, as: Math
  alias :rand, as: Rand

  def start( _type, _args ) do
    primes = 5..1000
      |> Enum.filter( fn( x ) -> (rem x, 2) == 1 end )
      |> Enum.filter( fn( x ) -> miller_rabin?( x, 10) == True end )
    IO.inspect( primes, label: "Primes: ", limit: :infinity )

    { :ok, self() }
  end

  def modular_exp( x, y, mod ) do
     with [ _ | bits ] = Integer.digits( y, 2 ) do
          Enum.reduce bits, x, fn( bit, acc ) -> acc * acc |> ( &( if bit == 1, do: &1 * x, else: &1 ) ).() |> rem( mod ) end
     end
  end

  def miller_rabin( d, s ) when rem( d, 2 ) == 0, do: { s, d }
  def miller_rabin( d, s ), do: miller_rabin( div( d, 2 ), s + 1 )

  def miller_rabin?( n, g ) do
       { s, d } = miller_rabin( n - 1, 0 )
       miller_rabin( n, g, s, d )
  end

  def miller_rabin( n, 0, _, _ ), do: True
  def miller_rabin( n, g, s, d ) do
    a = 1 + Rand.uniform( n - 3 )
    x = modular_exp( a, d, n )
    if x == 1 or x == n - 1 do
      miller_rabin( n, g - 1, s, d )
    else
      if miller_rabin( n, x, s - 1) == True, do: miller_rabin( n, g - 1, s, d ), else: False
    end
  end

  def miller_rabin( n, x, r ) when r <= 0, do: False
  def miller_rabin( n, x, r ) do
    x = modular_exp( x, 2, n )
    unless x == 1 do
      unless x == n - 1, do: miller_rabin( n, x, r - 1 ), else: True
    else
      False
    end
  end
end

```


{{out}}

```txt

Primes: : [5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79,
 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163,
 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251,
 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443,
 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557,
 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647,
 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757,
 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863,
 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983,
 991, 997]

```

The following larger examples all produce true:

```elixir

miller_rabin?( 94366396730334173383107353049414959521528815310548187030165936229578960209523421808912459795329035203510284576187160076386643700441216547732914250578934261891510827140267043592007225160798348913639472564715055445201512461359359488795427875530231001298552452230535485049737222714000227878890892901228389026881, 1000 )
miller_rabin?( 138028649176899647846076023812164793645371887571371559091892986639999096471811910222267538577825033963552683101137782650479906670021895135954212738694784814783986671046107023185842481502719762055887490765764329237651328922972514308635045190654896041748716218441926626988737664133219271115413563418353821396401, 1000 )
miller_rabin?( 123301261697053560451930527879636974557474268923771832437126939266601921428796348203611050423256894847735769138870460373141723679005090549101566289920247264982095246187318303659027201708559916949810035265951104246512008259674244307851578647894027803356820480862664695522389066327012330793517771435385653616841, 1000 )
miller_rabin?( 119432521682023078841121052226157857003721669633106050345198988740042219728400958282159638484144822421840470442893056822510584029066504295892189315912923804894933736660559950053226576719285711831138657839435060908151231090715952576998400120335346005544083959311246562842277496260598128781581003807229557518839, 1000 )
miller_rabin?( 132082885240291678440073580124226578272473600569147812319294626601995619845059779715619475871419551319029519794232989255381829366374647864619189704922722431776563860747714706040922215308646535910589305924065089149684429555813953571007126408164577035854428632242206880193165045777949624510896312005014225526731, 1000 )
miller_rabin?( 153410708946188157980279532372610756837706984448408515364579602515073276538040155990230789600191915021209039203172105094957316552912585741177975853552299222501069267567888742458519569317286299134843250075228359900070009684517875782331709619287588451883575354340318132216817231993558066067063143257425853927599, 1000 )
miller_rabin?( 103130593592068072608023213244858971741946977638988649427937324034014356815504971087381663169829571046157738503075005527471064224791270584831779395959349442093395294980019731027051356344056416276026592333932610954020105156667883269888206386119513058400355612571198438511950152690467372712488391425876725831041, 1000 )

```



## Erlang


This implementation of a Miller-Rabin method was revised
to permit use of integers of arbitrary precision.


```erlang
-module(miller_rabin).

-export([is_prime/1, power/2]).

is_prime(1) -> false;
is_prime(2) -> true;
is_prime(3) -> true;
is_prime(N) when N > 3, ((N rem 2) == 0) -> false;
is_prime(N) when ((N rem 2) ==1), N < 341550071728321 ->
 			is_mr_prime(N, proving_bases(N));
is_prime(N) when ((N rem 2) == 1) ->
			is_mr_prime(N, random_bases(N, 100)).


proving_bases(N) when N < 1373653 ->
	[2, 3];
proving_bases(N) when N < 9080191 ->
    [31, 73];
proving_bases(N) when N < 25326001 ->
	[2, 3, 5];
proving_bases(N) when N < 3215031751 ->
	[2, 3, 5, 7];
proving_bases(N) when N < 4759123141 ->
    [2, 7, 61];
proving_bases(N) when N < 1122004669633 ->
	[2, 13, 23, 1662803];
proving_bases(N) when N < 2152302898747 ->
	[2, 3, 5, 7, 11];
proving_bases(N) when N < 3474749660383 ->
    [2, 3, 5, 7, 11, 13];
proving_bases(N) when N < 341550071728321 ->
    [2, 3, 5, 7, 11, 13, 17].


is_mr_prime(N, As) when N>2, N rem 2 == 1 ->
    {D, S} = find_ds(N),
         %% this is a test for compositeness; the two case patterns disprove
         %%    compositeness.
    not lists:any(fun(A) ->
                          case mr_series(N, A, D, S) of
                              [1|_] -> false;                     % first elem of list = 1
                              L     -> not lists:member(N-1, L)   % some elem of list = N-1
                          end
                  end,
                  As).


find_ds(N) ->
    find_ds(N-1, 0).


find_ds(D, S) ->
    case D rem 2 == 0 of
        true ->
            find_ds(D div 2, S+1);
        false ->
            {D, S}
    end.


mr_series(N, A, D, S) when N rem 2 == 1 ->
    Js = lists:seq(0, S),
    lists:map(fun(J) -> pow_mod(A, power(2, J)*D, N) end, Js).


pow_mod(B, E, M) ->
    case E of
        0 -> 1;
        _ -> case ((E rem 2) == 0) of
                 true  -> (power(pow_mod(B, (E div 2), M), 2)) rem M;
                 false -> (B*pow_mod(B, E-1, M)) rem M
             end
    end.


random_bases(N, K) ->
    [basis(N) || _ <- lists:seq(1, K)].


basis(N) when N>2 ->
    1 + random:uniform(N-3).  % random:uniform returns a single random number in range 1 -> N-3, to which is added 1, shifting the range to 2 -> N-2


power(B, E) ->
    power(B, E, 1).

power(_, 0, Acc) ->
    Acc;
power(B, E, Acc) ->
    power(B, E - 1, B * Acc).
```



## Fortran


### Direct translation

{{works with|Fortran|95}}
For the module ''PrimeDecompose'', see [[Prime decomposition#Fortran|Prime decomposition]].

```fortran

  module Miller_Rabin
  use PrimeDecompose
  implicit none

  integer, parameter :: max_decompose = 100

  private :: int_rrand, max_decompose

contains

  function int_rrand(from, to)
    integer(huge) :: int_rrand
    integer(huge), intent(in) :: from, to

    real :: o
    call random_number(o)
    int_rrand = floor(from + o * real(max(from,to) - min(from, to)))
  end function int_rrand

  function miller_rabin_test(n, k) result(res)
    logical :: res
    integer(huge), intent(in) :: n
    integer, intent(in) :: k

    integer(huge), dimension(max_decompose) :: f
    integer(huge)                     :: s, d, i, a, x, r

    res = .true.
    f = 0

    if ( (n <= 2) .and. (n > 0) ) return
    if ( mod(n, 2) == 0 ) then
       res = .false.
       return
    end if

    call find_factors(n-1, f)
    s = count(f == 2)
    d = (n-1) / (2 ** s)
    loop:  do i = 1, k
       a = int_rrand(2_huge, n-2)
       x = mod(a ** d, n)

       if ( x == 1 ) cycle
       do r = 0, s-1
          if ( x == ( n - 1 ) ) cycle loop
          x = mod(x*x, n)
       end do
       if ( x == (n-1) ) cycle
       res = .false.
       return
    end do loop
    res = .true.
  end function miller_rabin_test

end module Miller_Rabin
```

'''Testing'''

```fortran
program TestMiller
  use Miller_Rabin
  implicit none

  integer, parameter :: prec = 30
  integer(huge) :: i

  ! this is limited since we're not using a bignum lib
  call do_test( (/ (i, i=1, 29) /) )

contains

  subroutine do_test(a)
    integer(huge), dimension(:), intent(in) :: a

    integer               :: i

    do i = 1, size(a,1)
       print *, a(i), miller_rabin_test(a(i), prec)
    end do

  end subroutine do_test

end program TestMiller
```

''Possible improvements'': create bindings to the [[:Category:GMP|GMP library]], change <code>integer(huge)</code> into something like <code>type(huge_integer)</code>, write a lots of interfaces to allow to use big nums naturally (so that the code will be unchanged, except for the changes said above)


### With some avoidance of overflow

Integer overflow is a severe risk, and even 64-bit integers won't get you far when the formulae are translated as <code>MOD(A**D,N)</code> - what is needed is a method for raising to a power that incorporates the modulus along the way. There is no library routine for that, so...
```Fortran
      MODULE MRTEST	!Try the Miller-Rabin primality test.
       CONTAINS		!Working only with in-built integers.
        LOGICAL FUNCTION MRPRIME(N,TRIALS)	!Could N be a prime number?
         USE DFPORT	!To get RAND.
         INTEGER N	!The number.
         INTEGER TRIALS	!The count of trials to make.
         INTEGER D,S	!Represents a number in a special form.
         INTEGER TRIAL
         INTEGER A,X,R
Catch some annoying cases.
          IF (N .LE. 4) THEN	!A single-digit number?
            MRPRIME = N.GT.1 .AND. N.LE.3	!Yes. Some special values.
            RETURN		!Thus allow 2 to be reported as prime.
          END IF		!Yet, test for 2 as a possible factor for larger numbers.
          MRPRIME = .FALSE.	!Pessimism prevails.
          IF (MOD(N,2).EQ.0 .OR. MOD(N,3).EQ.0) RETURN	!Thus.
Construct D such that N - 1 = D*2**S. By here, N is odd, and greater than three.
          D = N - 1		!Thus, D becomes an even number.
          S = 1			!So, it has at least one power of two.
   10     D = D/2		!Divide it out.
          IF (MOD(D,2).EQ.0) THEN	!If there is another,
            S = S + 1			!Count it,
            GO TO 10			!And divide it out also.
          END IF		!So, D is no longer even. N = 1 + D*2**S
          WRITE (6,11) N,D,S
   11     FORMAT("For ",I0,", D=",I0,",S=",I0)
Convince through repetition..
        T:DO TRIAL = 1,TRIALS	!Some trials yield a definite result.
            A = RAND(0)*(N - 2) + 2	!For small N, the birthday problem.
            X = MODEXP(N,A,D)		!A**D mod N.
            WRITE (6,22) TRIAL,A,X,INT8(A)**D,N,MOD(INT8(A)**D,N)
   22       FORMAT(6X,"Trial ",I0,",A=",I4,",X=",I4,
     1       "=MOD(",I0,",",I0,")=",I0)
            IF (X.EQ.1 .OR. X.EQ.N - 1) CYCLE T	!Pox. A prime yields these.
            DO R = 1,S - 1	!Step through the powers of two in N - 1.
              X = MODEXP(N,X,2)		!X**2 mod N.
              WRITE (6,23) R,X
   23         FORMAT (14X,"R=",I4,",X=",I0)
              IF (X.EQ.1) RETURN	!Definitely composite. No prime does this.
              IF (X.EQ.N - 1) CYCLE T	!Pox. Try something else.
            END DO		!Another power of two?
            RETURN		!Definitely composite.
          END DO T		!Have another go.
          MRPRIME = .TRUE.	!Would further trials yield greater assurance?
        END FUNCTION MRPRIME	!Are some numbers resistant to this scheme?

        INTEGER FUNCTION MODEXP(N,X,P)	!Calculate X**P mod N without overflowing...
C  Relies on a.b mod n = (a mod n)(b mod n) mod n
         INTEGER N,X,P	!All presumed positive, and X < N.
         INTEGER I	!A stepper.
         INTEGER*8 V,W	!Broad scratchpads, otherwise N > 46340 may incur overflow in 32-bit.
          V = 1		!=X**0
          IF (P.GT.0) THEN	!Something to do?
            I = P			!Yes. Get a copy I can mess with.
            W = X			!=X**1, X**2, X**4, X**8, ... except, all are mod N.
    1       IF (MOD(I,2).EQ.1) V = MOD(V*W,N)	!Incorporate W if the low-end calls for it.
            I = I/2			!Used. Shift the next one down.
            IF (I.GT.0) THEN		!Still something to do?
              W = MOD(W**2,N)			!Yes. Square W ready for the next bit up.
              GO TO 1				!Consider it.
            END IF				!Don't square W if nothing remains. It might overflow.
          END IF		!Negative powers are ignored.
          MODEXP = V		!Done, in lb(P) iterations!
        END FUNCTION MODEXP	!"Bit" presence by arithmetic: works for non-binary arithmetic too.

      PROGRAM POKEMR
      USE MRTEST
      INTEGER I
      LOGICAL HIC

      DO I = 3,36,2
        HIC = MRPRIME(I,6)
        WRITE (6,11) I,HIC
   11   FORMAT (I6,1X,L)
      END DO

      END
```

Output:

```txt

     3  T
For 5, D=1,S=2
      Trial 1,A=   2,X=   2=MOD(2,5)=2
              R=   1,X=4
      Trial 2,A=   2,X=   2=MOD(2,5)=2
              R=   1,X=4
      Trial 3,A=   3,X=   3=MOD(3,5)=3
              R=   1,X=4
      Trial 4,A=   4,X=   4=MOD(4,5)=4
      Trial 5,A=   4,X=   4=MOD(4,5)=4
      Trial 6,A=   2,X=   2=MOD(2,5)=2
              R=   1,X=4
     5  T
For 7, D=3,S=1
      Trial 1,A=   4,X=   1=MOD(64,7)=1
      Trial 2,A=   3,X=   6=MOD(27,7)=6
      Trial 3,A=   3,X=   6=MOD(27,7)=6
      Trial 4,A=   5,X=   6=MOD(125,7)=6
      Trial 5,A=   2,X=   1=MOD(8,7)=1
      Trial 6,A=   4,X=   1=MOD(64,7)=1
     7  T
     9  F
For 11, D=5,S=1
      Trial 1,A=   7,X=  10=MOD(16807,11)=10
      Trial 2,A=   9,X=   1=MOD(59049,11)=1
      Trial 3,A=   7,X=  10=MOD(16807,11)=10
      Trial 4,A=   6,X=  10=MOD(7776,11)=10
      Trial 5,A=   9,X=   1=MOD(59049,11)=1
      Trial 6,A=  10,X=  10=MOD(100000,11)=10
    11  T
For 13, D=3,S=2
      Trial 1,A=   9,X=   1=MOD(729,13)=1
      Trial 2,A=  12,X=  12=MOD(1728,13)=12
      Trial 3,A=   5,X=   8=MOD(125,13)=8
              R=   1,X=12
      Trial 4,A=   6,X=   8=MOD(216,13)=8
              R=   1,X=12
      Trial 5,A=  11,X=   5=MOD(1331,13)=5
              R=   1,X=12
      Trial 6,A=   9,X=   1=MOD(729,13)=1
    13  T
    15  F
For 17, D=1,S=4
      Trial 1,A=  15,X=  15=MOD(15,17)=15
              R=   1,X=4
              R=   2,X=16
      Trial 2,A=  16,X=  16=MOD(16,17)=16
      Trial 3,A=   4,X=   4=MOD(4,17)=4
              R=   1,X=16
      Trial 4,A=  14,X=  14=MOD(14,17)=14
              R=   1,X=9
              R=   2,X=13
              R=   3,X=16
      Trial 5,A=  15,X=  15=MOD(15,17)=15
              R=   1,X=4
              R=   2,X=16
      Trial 6,A=   6,X=   6=MOD(6,17)=6
              R=   1,X=2
              R=   2,X=4
              R=   3,X=16
    17  T
For 19, D=9,S=1
      Trial 1,A=  17,X=   1=MOD(118587876497,19)=1
      Trial 2,A=   9,X=   1=MOD(387420489,19)=1
      Trial 3,A=   7,X=   1=MOD(40353607,19)=1
      Trial 4,A=  10,X=  18=MOD(1000000000,19)=18
      Trial 5,A=   8,X=  18=MOD(134217728,19)=18
      Trial 6,A=  15,X=  18=MOD(38443359375,19)=18
    19  T
    21  F
For 23, D=11,S=1
      Trial 1,A=  16,X=   1=MOD(17592186044416,23)=1
      Trial 2,A=  13,X=   1=MOD(1792160394037,23)=1
      Trial 3,A=  14,X=  22=MOD(4049565169664,23)=22
      Trial 4,A=   3,X=   1=MOD(177147,23)=1
      Trial 5,A=  14,X=  22=MOD(4049565169664,23)=22
      Trial 6,A=  11,X=  22=MOD(285311670611,23)=22
    23  T
For 25, D=3,S=3
      Trial 1,A=  15,X=   0=MOD(3375,25)=0
              R=   1,X=0
              R=   2,X=0
    25  F
    27  F
For 29, D=7,S=2
      Trial 1,A=  24,X=   1=MOD(4586471424,29)=1
      Trial 2,A=  15,X=  17=MOD(170859375,29)=17
              R=   1,X=28
      Trial 3,A=  22,X=  28=MOD(2494357888,29)=28
      Trial 4,A=   3,X=  12=MOD(2187,29)=12
              R=   1,X=28
      Trial 5,A=   7,X=   1=MOD(823543,29)=1
      Trial 6,A=   8,X=  17=MOD(2097152,29)=17
              R=   1,X=28
    29  T
For 31, D=15,S=1
      Trial 1,A=  24,X=  30=MOD(6795192965888212992,31)=1
      Trial 2,A=   4,X=   1=MOD(1073741824,31)=1
      Trial 3,A=   7,X=   1=MOD(4747561509943,31)=1
      Trial 4,A=  19,X=   1=MOD(-3265617043834753317,31)=-15
      Trial 5,A=  18,X=   1=MOD(6746640616477458432,31)=1
      Trial 6,A=  23,X=  30=MOD(8380818432457522983,31)=23
    31  T
    33  F
For 35, D=17,S=1
      Trial 1,A=  12,X=  17=MOD(2218611106740436992,35)=17
    35  F

```

In this run, 32-bit integers falter for 19 in calculating 17<sup>9</sup>, and 64-bit integers falter for 31 with 19<sup>15</sup> by showing a negative number. Other 64-bit overflows however do not show a negative (as with 23<sup>15</sup>) because there is about an even chance that the high-order bit will be on or off. The compiler option for checking integer overflow does not report such faults with 64-bit integers, at least with the Compaq V6.6 F90/95 compiler. In this context, one misses the <code>IF OVERFLOW ... </code> that was part of Fortran II but which has been omitted from later versions.

Thus, there is no avoiding a special MODEXP function, even for small test numbers.


## FreeBASIC

Using the task pseudo code
===Up to 2^63-1===

```freebasic
' version 29-11-2016
' compile with: fbc -s console

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' But we have to define them for older versions.
#Ifndef TRUE
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function mul_mod(a As ULongInt, b As ULongInt, modulus As ULongInt) As ULongInt
    ' returns a * b mod modulus
    Dim As ULongInt x, y = a ' a mod modulus, but a is already smaller then modulus

    While b > 0
        If (b And 1) = 1 Then
            x = (x + y) Mod modulus
        End If
        y = (y Shl 1) Mod modulus
        b = b Shr 1
    Wend

    Return x

End Function

Function pow_mod(b As ULongInt, power As ULongInt, modulus As ULongInt) As ULongInt
    ' returns b ^ power mod modulus
    Dim As ULongInt x = 1

    While power > 0
        If (power And 1) = 1 Then
            ' x = (x * b) Mod modulus
            x = mul_mod(x, b, modulus)
        End If
        ' b = (b * b) Mod modulus
        b = mul_mod(b, b, modulus)
        power = power Shr 1
    Wend

    Return x

End Function

Function miller_rabin_test(n As ULongInt, k As Integer) As Byte

    If n > 9223372036854775808ull Then ' limit 2^63, pow_mod/mul_mod can't handle bigger numbers
        Print "number is to big, program will end"
        Sleep
        End
    End If

    ' 2 is a prime, if n is smaller then 2 or n is even then n = composite
    If n = 2 Then Return TRUE
    If (n < 2) OrElse ((n And 1) = 0) Then Return FALSE

    Dim As ULongInt a, x, n_one = n - 1, d = n_one
    Dim As UInteger s

    While (d And 1) = 0
        d = d Shr 1
        s = s + 1
    Wend

    While k > 0
        k = k - 1
        a = Int(Rnd * (n -2)) +2          ' 2 <= a < n
        x = pow_mod(a, d, n)
        If (x = 1) Or (x = n_one) Then Continue While
        For r As Integer = 1 To s -1
            x = pow_mod(x, 2, n)
            If x = 1 Then Return FALSE
            If x = n_one Then Continue While
        Next
        If x <> n_one Then Return FALSE
    Wend
    Return TRUE

End Function
' ------=< MAIN >=------

Randomize Timer

Dim As Integer total
Dim As ULongInt y, limit = 2^63-1

For y = limit - 1000 To limit
    If miller_rabin_test(y, 5) = TRUE Then
        total = total + 1
        Print y,
    End If
Next

Print : Print
Print total; " primes between "; limit - 1000; " and "; y -1

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
9223372036854774893         9223372036854774917         9223372036854774937
9223372036854774959         9223372036854775057         9223372036854775073
9223372036854775097         9223372036854775139         9223372036854775159
9223372036854775181         9223372036854775259         9223372036854775279
9223372036854775291         9223372036854775337         9223372036854775351
9223372036854775399         9223372036854775417         9223372036854775421
9223372036854775433         9223372036854775507         9223372036854775549
9223372036854775643         9223372036854775783

 23 primes between 9223372036854774808 and 9223372036854775808
```


### Using Big Integer library

{{libheader|GMP}}

```freebasic
' version 05-04-2017
' compile with: fbc -s console

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' But we have to define them for older versions.
#Ifndef TRUE
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

#Include Once "gmp.bi"

#Macro big_int(a)
    Dim As Mpz_ptr a = Allocate( Len( __mpz_struct))
    Mpz_init(a)
#EndMacro

Dim Shared As __gmp_randstate_struct rnd_

Function miller_rabin(big_n As Mpz_ptr, num_of_tests As ULong) As Byte

    If mpz_cmp_ui(big_n, 1) < 1 Then
        Print "Numbers smaller then 1 not allowed"
        Sleep  5000
    End If

    If mpz_cmp_ui(big_n, 2) = 0 OrElse mpz_cmp_ui(big_n, 3) = 0 Then
        Return TRUE   ' 2 = prime , 3 = prime
    End If

    If mpz_tstbit(big_n, 0) = 0 Then Return FALSE  ' even number, no prime

    Dim As ULong r, s
    Dim As Byte return_value = TRUE

    big_int(n_1) : big_int(n_2) : big_int(a) : big_int(d) : big_int(x)

    mpz_sub_ui(n_1, big_n, 1) : mpz_sub_ui(n_2, big_n, 2) : mpz_set(d, n_1)

    While mpz_tstbit(d, 0) = 0
        mpz_fdiv_q_2exp(d, d, 1)
        s += 1
    Wend

    While num_of_tests > 0
        num_of_tests -= 1
        mpz_urandomm(a, @rnd_, n_2)
        mpz_add_ui(a, a, 2)
        mpz_powm(x, a, d, big_n)
        If mpz_cmp_ui(x, 1) = 0 Or mpz_cmp(x, n_1) = 0 Then Continue While

        For r = 1 To s -1
            mpz_powm_ui(x, x, 2, big_n)
            If mpz_cmp_ui(x, 1) = 0 Then
                return_value = FALSE
                Exit While
            End If
            If mpz_cmp(x, n_1) = 0 Then Continue While
        Next

        If mpz_cmp(x, n_1) <> 0 Then
            Return_value = FALSE
            Exit while
        End If
    Wend

    mpz_clear(n_1) : mpz_clear(a) : mpz_clear(d)
    mpz_clear(n_2) : mpz_clear(x)

    Return return_value

End Function

' ------=< MAIN >=------

Dim As Long x
Dim As String tmp
Dim As ZString Ptr gmp_str : gmp_str = Allocate(1000000)
big_int(big_n)

Randomize Timer
gmp_randinit_mt(@rnd_)
For x = 0 To 200  'create seed for random generator
    tmp += Str(Int(Rnd * 10))
Next
Mpz_set_str(big_n, tmp, 10)
gmp_randseed(@rnd_, big_n) ' seed the random number generator

For x = 2 To 100
    mpz_set_ui(big_n, x)
    If miller_rabin(big_n, 5) = TRUE Then
        Print Using "####"; x;
    End If
Next

Print : Print
For x = 2 To 3300
    mpz_set_ui(big_n, 1)
    mpz_mul_2exp(big_n, big_n, x)
    mpz_sub_ui(big_n, big_n, 1)
    If miller_rabin(big_n, 5) = TRUE Then
        gmp_str = Mpz_get_str(0, 10, big_n)
        Print "2^";Str(x);"-1 = prime"
    End If
Next

gmp_randclear(@rnd_)
mpz_clear(big_n)
DeAllocate(gmp_str)

' empty keyboard buffer
Print : While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
   2   3   5   7  11  13  17  19  23  29  31  37  41  43  47  53  59  61  67  71  73  79  83  89  97

2^2-1 = prime
2^3-1 = prime
2^5-1 = prime
2^7-1 = prime
2^13-1 = prime
2^17-1 = prime
2^19-1 = prime
2^31-1 = prime
2^61-1 = prime
2^89-1 = prime
2^107-1 = prime
2^127-1 = prime
2^521-1 = prime
2^607-1 = prime
2^1279-1 = prime
2^2203-1 = prime
2^2281-1 = prime
2^3217-1 = prime
```



## FunL

Direct implementation of the task algorithm.


```funl
import util.rnd

def isProbablyPrimeMillerRabin( n, k ) =
  d = n - 1
  s = 0

  while 2|d
    s++
    d /= 2

  repeat k
    a = rnd( 2, n )
    x = a^d mod n

    if x == 1 or x == n - 1 then continue

    repeat s - 1
      x = x^2 mod n

      if x == 1 then return false

      if x == n - 1 then break
    else
      return false

  true

for i <- 3..100
  if isProbablyPrimeMillerRabin( i, 5 )
    println( i )
```


{{out}}


```txt

3
5
7
11
13
17
19
23
29
31
37
41
43
47
53
59
61
67
71
73
79
83
89
97

```



## Go

;Library
Go has it in math/big in standard library as [http://golang.org/pkg/math/big/#Int.ProbablyPrime ProbablyPrime].  The argument n to ProbablyPrime is the input k of the pseudocode in the task description.
;Deterministic
Below is a deterministic test for 32 bit unsigned integers.  Intermediate results in the code below include a 64 bit result from multiplying two 32 bit numbers.  Since 64 bits is the largest fixed integer type in Go, a 32 bit number is the largest that is convenient to test.

The main difference between this algorithm and the pseudocode in the task description is that k numbers are not chosen randomly, but instead are the three numbers 2, 7, and 61.  These numbers provide a deterministic primality test up to 2^32.

```go
package main

import "log"

func main() {
    // max uint32 is not prime
    c := uint32(1<<32 - 1)
    // a few primes near the top of the range.  source: prime pages.
    for _, p := range []uint32{1<<32 - 5, 1<<32 - 17, 1<<32 - 65, 1<<32 - 99} {
        for ; c > p; c-- {
            if prime(c) {
                log.Fatalf("prime(%d) returned true", c)
            }
        }
        if !prime(p) {
            log.Fatalf("prime(%d) returned false", p)
        }
        c--
    }
}

func prime(n uint32) bool {
    // bases of 2, 7, 61 are sufficient to cover 2^32
    switch n {
    case 0, 1:
        return false
    case 2, 7, 61:
        return true
    }
    // compute s, d where 2^s * d = n-1
    nm1 := n - 1
    d := nm1
    s := 0
    for d&1 == 0 {
        d >>= 1
        s++
    }
    n64 := uint64(n)
    for _, a := range []uint32{2, 7, 61} {
        // compute x := a^d % n
        x := uint64(1)
        p := uint64(a)
        for dr := d; dr > 0; dr >>= 1 {
            if dr&1 != 0 {
                x = x * p % n64
            }
            p = p * p % n64
        }
        if x == 1 || uint32(x) == nm1 {
            continue
        }
        for r := 1; ; r++ {
            if r >= s {
                return false
            }
            x = x * x % n64
            if x == 1 {
                return false
            }
            if uint32(x) == nm1 {
                break
            }
        }
    }
    return true
}
```



## Haskell

{{works with|Haskell|7.6.3}}
* Ideas taken from [http://primes.utm.edu/prove/prove2_3.html Primality proving]
* Functions witns and isMillerRabinPrime follow closely the code outlined in [http://www.jsoftware.com/jwiki/Essays/Primality%20Tests#Miller-Rabin J/Essays]
* A useful powerMod function is taken from [[Multiplicative order#Haskell]]
* Original Rosetta code has been simplified to be easier to follow
Another Miller Rabin test can be found in D. Amos's Haskell for Math module [http://www.polyomino.f2s.com/david/haskell/numbertheory.html Primes.hs]

```Haskell
module Primes where

import System.Random
import System.IO.Unsafe

-- Miller-Rabin wrapped up as an (almost deterministic) pure function
isPrime :: Integer -> Bool
isPrime n = unsafePerformIO (isMillerRabinPrime 100 n)


isMillerRabinPrime :: Int -> Integer -> IO Bool
isMillerRabinPrime k n
   | even n    = return (n==2)
   | n < 100   = return (n `elem` primesTo100)
   | otherwise = do ws <- witnesses k n
                    return $ and [test n (pred n) evens (head odds) a | a <- ws]
  where
    (evens,odds) = span even (iterate (`div` 2) (pred n))

test :: Integral nat => nat -> nat -> [nat] -> nat -> nat -> Bool
test n n_1 evens d a = x `elem` [1,n_1] || n_1 `elem` powers
  where
    x = powerMod n a d
    powers = map (powerMod n a) evens

witnesses :: (Num a, Ord a, Random a) => Int -> a -> IO [a]
witnesses k n
  | n < 9080191         = return [31,73]
  | n < 4759123141      = return [2,7,61]
  | n < 3474749660383   = return [2,3,5,7,11,13]
  | n < 341550071728321 = return [2,3,5,7,11,13,17]
  | otherwise           = do g <- newStdGen
                             return $ take k (randomRs (2,n-1) g)

primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- powerMod m x n = x^n `mod` m
powerMod :: Integral nat => nat -> nat -> nat -> nat
powerMod m x n  = f (n - 1) x x `rem` m
  where
  f d a y = if d==0 then y else g d a y
  g i b y | even i    = g (i `quot` 2) (b*b `rem` m) y
          | otherwise = f (i-1) b (b*y `rem` m)

```


{{out|Sample output}}

```txt
Testing in GHCi:
~> isPrime 4547337172376300111955330758342147474062293202868155909489
True

*~> isPrime 4547337172376300111955330758342147474062293202868155909393
False

*~> dropWhile (<900) $ filter isPrime [2..1000]
[907,911,919,929,937,941,947,953,967,971,977,983,991,997]
```



'''Perhaps a slightly clearer (less monadic) version. Transcription of pseudocode.'''
* The code above likely has better complexity.


```Haskell

import Control.Monad (liftM)
import Data.Bits (Bits, testBit, shiftR)
import System.Random (Random, getStdGen, randomRs)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (even, odd)

odd :: (Integral a, Bits a) => a -> Bool
odd = (`testBit` 0)

even :: (Integral a, Bits a) => a -> Bool
even = not . odd

-- modPow - Recursive modular exponentiation by taking successive powers of two
modPow :: (Integral a, Bits a) => a -> a -> a -> a
modPow _ 0 _ = 1
modPow base ex m = let term
                         | testBit ex 0 = base `mod` m
                         | otherwise    = 1
                   in (term * modPow (base^2 `mod` m) (ex `shiftR` 1) m) `mod` m

isPrime :: (Integral a, Bits a, Random a) => a -> a -> Bool
isPrime n k
    | n < 4     = if n > 1 then True else False -- Deal with 0-3.
    | even n    = False
    | otherwise = let randPool = unsafePerformIO $ randNums (n - 2)
                  in witness k randPool
    where
        randNums upper = do
            g <- getStdGen
            return (randomRs (2, upper) g)

        (d, r) = let decompose d r
                        | odd d     = (d, r)
                        | otherwise = decompose (d `shiftR` 1) (r + 1)
                 in decompose (n - 1) 0

        witness 0 _ = True
        witness k (a:rands)
            | x == 1 || x == n - 1 = witness (k - 1) rands
            | otherwise            = check x (r - 1)
            where
                x = modPow a d n

                check _ 0 = False
                check x count
                    | x' == 1     = False
                    | x' == n - 1 = witness (k - 1) rands
                    | otherwise   = check x' (count - 1)
                    where x' = modPow x 2 n

-- main function for testing
main :: IO()
main = do
    [n,k] <- liftM (map (\x -> read x :: Integer) . words) getLine
    print $ isPrime n k

```



{{out|Sample Output}}

```txt

4547337172376300111955330758342147474062293202868155909489 10
True

4547337172376300111955330758342147474062293202868155909393 10
False

226801 7
False

94366396730334173383107353049414959521528815310548187030165936229578960209523421808912459795329035203510284576187160076386643700441216547732914250578934261891510827140267043592007225160798348913639472564715055445201512461359359488795427875530231001298552452230535485049737222714000227878890892901228389026881 50
True

```


=={{header|Icon}} and {{header|Unicon}}==

The following runs in both languages:

```unicon
procedure main(A)
   every n := !A do write(n," is ",(mrp(n,5),"probably prime")|"composite")
end

procedure mrp(n, k)
    if n = 2 then return ""
    if n%2 = 0 then fail
    nm1 := decompose(n-1)
    s := nm1[1]
    d := nm1[2]
    every !k do {
        a := ?(n-2)+1
        x := (a^d)%n
        if x = (1|(n-1)) then next
        every !(s-1) do {
            x := (x*x)%n
            if x = 1 then fail
            if x = (n-1) then break next
            }
        fail
        }
    return ""
end

procedure decompose(nm1)
    s := 1
    d := nm1
    while d%2 = 0 do {
        d /:= 2
        s +:= 1
        }
    return [s,d]
end
```


Sample run:


```txt

->mrp 219 221 223 225 227 229
219 is composite
221 is composite
223 is probably prime
225 is composite
227 is probably prime
229 is probably prime
->

```



## J

See [[j:Essays/Primality%20Tests#Miller-Rabin|Primality Tests essay on the J wiki]].


## Java

The Miller-Rabin primality test is part of the standard library (java.math.BigInteger)

```java
import java.math.BigInteger;

public class MillerRabinPrimalityTest {
  public static void main(String[] args) {
    BigInteger n = new BigInteger(args[0]);
    int certainty = Integer.parseInt(args[1]);
    System.out.println(n.toString() + " is " + (n.isProbablePrime(certainty) ? "probably prime" : "composite"));
  }
}
```

{{out|Sample output}}

```txt
java MillerRabinPrimalityTest 123456791234567891234567 1000000
123456791234567891234567 is probably prime
```


This is a translation of the [http://rosettacode.org/wiki/Miller-Rabin_primality_test#Python:_Proved_correct_up_to_large_N Python solution] for a deterministic test for n < 341,550,071,728,321:

```java
import java.math.BigInteger;

public class Prime {

    // this is the RabinMiller test, deterministically correct for n < 341,550,071,728,321
    // http://rosettacode.org/wiki/Miller-Rabin_primality_test#Python:_Proved_correct_up_to_large_N
    public static boolean isPrime(BigInteger n, int precision) {

        if (n.compareTo(new BigInteger("341550071728321")) >= 0) {
            return n.isProbablePrime(precision);
        }

        int intN = n.intValue();
        if (intN == 1 || intN == 4 || intN == 6 || intN == 8) return false;
        if (intN == 2 || intN == 3 || intN == 5 || intN == 7) return true;

        int[] primesToTest = getPrimesToTest(n);
        if (n.equals(new BigInteger("3215031751"))) {
            return false;
        }
        BigInteger d = n.subtract(BigInteger.ONE);
        BigInteger s = BigInteger.ZERO;
        while (d.mod(BigInteger.valueOf(2)).equals(BigInteger.ZERO)) {
            d = d.shiftRight(1);
            s = s.add(BigInteger.ONE);
        }
        for (int a : primesToTest) {
            if (try_composite(a, d, n, s)) {
                return false;
            }
        }
        return true;
    }

    public static boolean isPrime(BigInteger n) {
        return isPrime(n, 100);
    }

    public static boolean isPrime(int n) {
        return isPrime(BigInteger.valueOf(n), 100);
    }

    public static boolean isPrime(long n) {
        return isPrime(BigInteger.valueOf(n), 100);
    }

    private static int[] getPrimesToTest(BigInteger n) {
        if (n.compareTo(new BigInteger("3474749660383")) >= 0) {
            return new int[]{2, 3, 5, 7, 11, 13, 17};
        }
        if (n.compareTo(new BigInteger("2152302898747")) >= 0) {
            return new int[]{2, 3, 5, 7, 11, 13};
        }
        if (n.compareTo(new BigInteger("118670087467")) >= 0) {
            return new int[]{2, 3, 5, 7, 11};
        }
        if (n.compareTo(new BigInteger("25326001")) >= 0) {
            return new int[]{2, 3, 5, 7};
        }
        if (n.compareTo(new BigInteger("1373653")) >= 0) {
            return new int[]{2, 3, 5};
        }
        return new int[]{2, 3};
    }

    private static boolean try_composite(int a, BigInteger d, BigInteger n, BigInteger s) {
        BigInteger aB = BigInteger.valueOf(a);
        if (aB.modPow(d, n).equals(BigInteger.ONE)) {
            return false;
        }
        for (int i = 0; BigInteger.valueOf(i).compareTo(s) < 0; i++) {
            // if pow(a, 2**i * d, n) == n-1
            if (aB.modPow(BigInteger.valueOf(2).pow(i).multiply(d), n).equals(n.subtract(BigInteger.ONE))) {
                return false;
            }
        }
        return true;
    }
}

```



## JavaScript

For the return values of this function, <code>true</code> means "probably prime" and <code>false</code> means "definitely composite."


```JavaScript
function probablyPrime(n, k) {
	if (n === 2 || n === 3)
		return true;
	if (n % 2 === 0 || n < 2)
		return false;

	// Write (n - 1) as 2^s * d
	var s = 0, d = n - 1;
	while (d % 2 === 0) {
		d /= 2;
		++s;
	}

	WitnessLoop: do {
		// A base between 2 and n - 2
		var x = Math.pow(2 + Math.floor(Math.random() * (n - 3)), d) % n;

		if (x === 1 || x === n - 1)
			continue;

		for (var i = s - 1; i--;) {
			x = x * x % n;
			if (x === 1)
				return false;
			if (x === n - 1)
				continue WitnessLoop;
		}

		return false;
	} while (--k);

	return true;
}
```



## Julia

The built-in <code>isprime</code> function uses the Miller-Rabin primality test.  Here is the implementation of <code>isprime</code> from the Julia standard library (Julia version 0.2):

```julia

witnesses(n::Union(Uint8,Int8,Uint16,Int16)) = (2,3)
witnesses(n::Union(Uint32,Int32)) = n < 1373653 ? (2,3) : (2,7,61)
witnesses(n::Union(Uint64,Int64)) =
        n < 1373653         ? (2,3) :
        n < 4759123141      ? (2,7,61) :
        n < 2152302898747   ? (2,3,5,7,11) :
        n < 3474749660383   ? (2,3,5,7,11,13) :
                              (2,325,9375,28178,450775,9780504,1795265022)

function isprime(n::Integer)
    n == 2 && return true
    (n < 2) | iseven(n) && return false
    s = trailing_zeros(n-1)
    d = (n-1) >>> s
    for a in witnesses(n)
        a < n || break
        x = powermod(a,d,n)
        x == 1 && continue
        t = s
        while x != n-1
            (t-=1) <= 0 && return false
            x = oftype(n, Base.widemul(x,x) % n)
            x == 1 && return false
        end
    end
    return true
end

```



## Kotlin

Translating the pseudo-code directly rather than using the Java library method BigInteger.isProbablePrime(certainty):

```scala
// version 1.1.2

import java.math.BigInteger
import java.util.Random

val bigTwo = BigInteger.valueOf(2L)

fun isProbablyPrime(n: BigInteger, k: Int): Boolean {
    require (n > bigTwo && n % bigTwo == BigInteger.ONE) { "Must be odd and greater than 2" }
    var s = 0
    val nn = n - BigInteger.ONE
    var d: BigInteger
    do {
        s++
        d = nn.shiftRight(s)
    }
    while (d % bigTwo == BigInteger.ZERO)

    val rand = Random()
    loop@ for (i in 1..k) {
        var a: BigInteger
        do {
            a = BigInteger(n.bitLength(), rand)
        }
        while(a < bigTwo || a > nn) // make sure it's in the interval [2, n - 1]

        var x = a.modPow(d, n)
        if (x == BigInteger.ONE || x == nn) continue
        for (r in 1 until s) {
            x =  (x * x) % n
            if (x == BigInteger.ONE) return false
            if (x == nn) break@loop
        }
        return false
    }
    return true
}

fun main(args: Array<String>) {
    val k = 20 // say
    // obtain all primes up to 100
    println("The following numbers less than 100 are prime:")
    for (i in 3..99 step 2)
        if (isProbablyPrime(BigInteger.valueOf(i.toLong()), k)) print("$i ")
    println("\n")
    // check if some big numbers are probably prime
    val bia = arrayOf(
        BigInteger("4547337172376300111955330758342147474062293202868155909489"),
        BigInteger("4547337172376300111955330758342147474062293202868155909393")
    )
    for (bi in bia)
        println("$bi is ${if (isProbablyPrime(bi, k)) "probably prime" else "composite"}")
}
```


{{out}}

```txt

The following numbers less than 100 are prime:
3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97

4547337172376300111955330758342147474062293202868155909489 is probably prime
4547337172376300111955330758342147474062293202868155909393 is composite

```



## Liberty BASIC


```lb

DIM mersenne(11)
mersenne(1)=7
mersenne(2)=31
mersenne(3)=127
mersenne(4)=8191
mersenne(5)=131071
mersenne(6)=524287
mersenne(7)=2147483647
mersenne(8)=2305843009213693951
mersenne(9)=618970019642690137449562111
mersenne(10)=162259276829213363391578010288127
mersenne(11)=170141183460469231731687303715884105727


dim SmallPrimes(1000)
data       2,      3,      5,      7,     11,     13,     17,     19,     23,     29
data      31,     37,     41,     43,     47,     53,     59,     61,     67,     71
data      73,     79,     83,     89,     97,    101,    103,    107,    109,    113
data     127,    131,    137,    139,    149,    151,    157,    163,    167,    173
data     179,    181,    191,    193,    197,    199,    211,    223,    227,    229
data     233,    239,    241,    251,    257,    263,    269,    271,    277,    281
data     283,    293,    307,    311,    313,    317,    331,    337,    347,    349
data     353,    359,    367,    373,    379,    383,    389,    397,    401,    409
data     419,    421,    431,    433,    439,    443,    449,    457,    461,    463
data     467,    479,    487,    491,    499,    503,    509,    521,    523,    541
data     547,    557,    563,    569,    571,    577,    587,    593,    599,    601
data     607,    613,    617,    619,    631,    641,    643,    647,    653,    659
data     661,    673,    677,    683,    691,    701,    709,    719,    727,    733
data     739,    743,    751,    757,    761,    769,    773,    787,    797,    809
data     811,    821,    823,    827,    829,    839,    853,    857,    859,    863
data     877,    881,    883,    887,    907,    911,    919,    929,    937,    941
data     947,    953,    967,    971,    977,    983,    991,    997,   1009,   1013
data    1019,   1021,   1031,   1033,   1039,   1049,   1051,   1061,   1063,   1069
data    1087,   1091,   1093,   1097,   1103,   1109,   1117,   1123,   1129,   1151
data    1153,   1163,   1171,   1181,   1187,   1193,   1201,   1213,   1217,   1223
data    1229,   1231,   1237,   1249,   1259,   1277,   1279,   1283,   1289,   1291
data    1297,   1301,   1303,   1307,   1319,   1321,   1327,   1361,   1367,   1373
data    1381,   1399,   1409,   1423,   1427,   1429,   1433,   1439,   1447,   1451
data    1453,   1459,   1471,   1481,   1483,   1487,   1489,   1493,   1499,   1511
data    1523,   1531,   1543,   1549,   1553,   1559,   1567,   1571,   1579,   1583
data    1597,   1601,   1607,   1609,   1613,   1619,   1621,   1627,   1637,   1657
data    1663,   1667,   1669,   1693,   1697,   1699,   1709,   1721,   1723,   1733
data    1741,   1747,   1753,   1759,   1777,   1783,   1787,   1789,   1801,   1811
data    1823,   1831,   1847,   1861,   1867,   1871,   1873,   1877,   1879,   1889
data    1901,   1907,   1913,   1931,   1933,   1949,   1951,   1973,   1979,   1987
data    1993,   1997,   1999,   2003,   2011,   2017,   2027,   2029,   2039,   2053
data    2063,   2069,   2081,   2083,   2087,   2089,   2099,   2111,   2113,   2129
data    2131,   2137,   2141,   2143,   2153,   2161,   2179,   2203,   2207,   2213
data    2221,   2237,   2239,   2243,   2251,   2267,   2269,   2273,   2281,   2287
data    2293,   2297,   2309,   2311,   2333,   2339,   2341,   2347,   2351,   2357
data    2371,   2377,   2381,   2383,   2389,   2393,   2399,   2411,   2417,   2423
data    2437,   2441,   2447,   2459,   2467,   2473,   2477,   2503,   2521,   2531
data    2539,   2543,   2549,   2551,   2557,   2579,   2591,   2593,   2609,   2617
data    2621,   2633,   2647,   2657,   2659,   2663,   2671,   2677,   2683,   2687
data    2689,   2693,   2699,   2707,   2711,   2713,   2719,   2729,   2731,   2741
data    2749,   2753,   2767,   2777,   2789,   2791,   2797,   2801,   2803,   2819
data    2833,   2837,   2843,   2851,   2857,   2861,   2879,   2887,   2897,   2903
data    2909,   2917,   2927,   2939,   2953,   2957,   2963,   2969,   2971,   2999
data    3001,   3011,   3019,   3023,   3037,   3041,   3049,   3061,   3067,   3079
data    3083,   3089,   3109,   3119,   3121,   3137,   3163,   3167,   3169,   3181
data    3187,   3191,   3203,   3209,   3217,   3221,   3229,   3251,   3253,   3257
data    3259,   3271,   3299,   3301,   3307,   3313,   3319,   3323,   3329,   3331
data    3343,   3347,   3359,   3361,   3371,   3373,   3389,   3391,   3407,   3413
data    3433,   3449,   3457,   3461,   3463,   3467,   3469,   3491,   3499,   3511
data    3517,   3527,   3529,   3533,   3539,   3541,   3547,   3557,   3559,   3571
data    3581,   3583,   3593,   3607,   3613,   3617,   3623,   3631,   3637,   3643
data    3659,   3671,   3673,   3677,   3691,   3697,   3701,   3709,   3719,   3727
data    3733,   3739,   3761,   3767,   3769,   3779,   3793,   3797,   3803,   3821
data    3823,   3833,   3847,   3851,   3853,   3863,   3877,   3881,   3889,   3907
data    3911,   3917,   3919,   3923,   3929,   3931,   3943,   3947,   3967,   3989
data    4001,   4003,   4007,   4013,   4019,   4021,   4027,   4049,   4051,   4057
data    4073,   4079,   4091,   4093,   4099,   4111,   4127,   4129,   4133,   4139
data    4153,   4157,   4159,   4177,   4201,   4211,   4217,   4219,   4229,   4231
data    4241,   4243,   4253,   4259,   4261,   4271,   4273,   4283,   4289,   4297
data    4327,   4337,   4339,   4349,   4357,   4363,   4373,   4391,   4397,   4409
data    4421,   4423,   4441,   4447,   4451,   4457,   4463,   4481,   4483,   4493
data    4507,   4513,   4517,   4519,   4523,   4547,   4549,   4561,   4567,   4583
data    4591,   4597,   4603,   4621,   4637,   4639,   4643,   4649,   4651,   4657
data    4663,   4673,   4679,   4691,   4703,   4721,   4723,   4729,   4733,   4751
data    4759,   4783,   4787,   4789,   4793,   4799,   4801,   4813,   4817,   4831
data    4861,   4871,   4877,   4889,   4903,   4909,   4919,   4931,   4933,   4937
data    4943,   4951,   4957,   4967,   4969,   4973,   4987,   4993,   4999,   5003
data    5009,   5011,   5021,   5023,   5039,   5051,   5059,   5077,   5081,   5087
data    5099,   5101,   5107,   5113,   5119,   5147,   5153,   5167,   5171,   5179
data    5189,   5197,   5209,   5227,   5231,   5233,   5237,   5261,   5273,   5279
data    5281,   5297,   5303,   5309,   5323,   5333,   5347,   5351,   5381,   5387
data    5393,   5399,   5407,   5413,   5417,   5419,   5431,   5437,   5441,   5443
data    5449,   5471,   5477,   5479,   5483,   5501,   5503,   5507,   5519,   5521
data    5527,   5531,   5557,   5563,   5569,   5573,   5581,   5591,   5623,   5639
data    5641,   5647,   5651,   5653,   5657,   5659,   5669,   5683,   5689,   5693
data    5701,   5711,   5717,   5737,   5741,   5743,   5749,   5779,   5783,   5791
data    5801,   5807,   5813,   5821,   5827,   5839,   5843,   5849,   5851,   5857
data    5861,   5867,   5869,   5879,   5881,   5897,   5903,   5923,   5927,   5939
data    5953,   5981,   5987,   6007,   6011,   6029,   6037,   6043,   6047,   6053
data    6067,   6073,   6079,   6089,   6091,   6101,   6113,   6121,   6131,   6133
data    6143,   6151,   6163,   6173,   6197,   6199,   6203,   6211,   6217,   6221
data    6229,   6247,   6257,   6263,   6269,   6271,   6277,   6287,   6299,   6301
data    6311,   6317,   6323,   6329,   6337,   6343,   6353,   6359,   6361,   6367
data    6373,   6379,   6389,   6397,   6421,   6427,   6449,   6451,   6469,   6473
data    6481,   6491,   6521,   6529,   6547,   6551,   6553,   6563,   6569,   6571
data    6577,   6581,   6599,   6607,   6619,   6637,   6653,   6659,   6661,   6673
data    6679,   6689,   6691,   6701,   6703,   6709,   6719,   6733,   6737,   6761
data    6763,   6779,   6781,   6791,   6793,   6803,   6823,   6827,   6829,   6833
data    6841,   6857,   6863,   6869,   6871,   6883,   6899,   6907,   6911,   6917
data    6947,   6949,   6959,   6961,   6967,   6971,   6977,   6983,   6991,   6997
data    7001,   7013,   7019,   7027,   7039,   7043,   7057,   7069,   7079,   7103
data    7109,   7121,   7127,   7129,   7151,   7159,   7177,   7187,   7193,   7207
data    7211,   7213,   7219,   7229,   7237,   7243,   7247,   7253,   7283,   7297
data    7307,   7309,   7321,   7331,   7333,   7349,   7351,   7369,   7393,   7411
data    7417,   7433,   7451,   7457,   7459,   7477,   7481,   7487,   7489,   7499
data    7507,   7517,   7523,   7529,   7537,   7541,   7547,   7549,   7559,   7561
data    7573,   7577,   7583,   7589,   7591,   7603,   7607,   7621,   7639,   7643
data    7649,   7669,   7673,   7681,   7687,   7691,   7699,   7703,   7717,   7723
data    7727,   7741,   7753,   7757,   7759,   7789,   7793,   7817,   7823,   7829
data    7841,   7853,   7867,   7873,   7877,   7879,   7883,   7901,   7907,   7919


print "Liberty Miller Rabin Demonstration"
print "Loading Small Primes"
for i=1 to 1000:   read x : SmallPrimes(i)=x :next :NoOfSmallPrimes=1000
print NoOfSmallPrimes;" Primes Loaded"

'Prompt "Enter number to test:";resp$
'x=val(resp$)
'goto [Jump]


For i=1 to 11

 x=mersenne(i)


 t1=time$("ms")
 [TryAnother]
 print

 iterations=0
 [Loop]
    iterations=iterations+1

    if MillerRabin(x,7)=1 then
     t2=time$("ms")
     print "Composite, found in ";t2-t1;" milliseconds"
    else
     t2=time$("ms")
     print x;" Probably Prime. Tested in ";t2-t1;" milliseconds"
     playwave "tada.wav", async
 end if
 print

next

END


Function GCD( m,n )
' Find greatest common divisor with Extend Euclidian Algorithm
' Knuth Vol 1 P.13 Algorithm E

ap =1 :b  =1 :a  =0 :bp =0: c  =m :d  =n

[StepE2]
q  = int(c/d) :r  = c-q*d

if r<>0 then
    c=d :d=r :t=ap :ap=a :a=t-q*a :t=bp  :bp=b  :b=t-q*b
    'print ap;" ";b;" ";a;" ";bp;" ";c;" ";d;" ";t;" ";q
    goto [StepE2]
end if

GCD=a*m+b*n

'print ap;" ";b;" ";a;" ";bp;" ";c;" ";d;" ";t;" ";q

End Function 'Extended Euclidian GCD

 function IsEven( x )
    if ( x MOD 2 )=0 then
        IsEven=1
    else
         IsEven=0
    end if
end function


function IsOdd( x )
    if ( x MOD 2 )=0 then
        IsOdd=0
    else
        IsOdd=1
    end if
end function


Function FastExp(x, y, N)

  if (y=1) then                  'MOD(x,N)
      FastExp=x-int(x/N)*N
      goto [ExitFunction]
  end if


  if ( y and 1) = 0  then

     dum1=y/2
     dum2=y-int(y/2)*2              'MOD(y,2)

     temp=FastExp(x,dum1,N)
     z=temp*temp
     FastExp=z-int(z/N)*N            'MOD(temp*temp,N)
     goto [ExitFunction]
  else

     dum1=y-1
     dum1=dum1/2
     temp=FastExp(x,dum1,N)
     dum2=temp*temp
     temp=dum2-int(dum2/N)*N            'MOD(dum2,N)

     z=temp*x
     FastExp=z-int(z/N)*N             'MOD(temp*x,N)
     goto [ExitFunction]
  end if
  [ExitFunction]

end function


Function MillerRabin(n,b)

'print "Miller Rabin"
't1=time$("ms")

  if IsEven(n) then
    MillerRabin=1
    goto [ExtFn]
  end if

  i=0
  [Loop]
    i=i+1
    if i>1000 then goto [Continue]
    if ( n MOD SmallPrimes(i) )=0 then
      MillerRabin=0
      goto [ExtFn]
    end if
  goto [Loop]
  [Continue]

  if GCD(n,b)>1 then
    MillerRabin=1
    goto [ExtFn]
  end if

  q=n-1

  t=0

  while  (int(q) AND 1 )=0
   t=t+1
   q=int(q/2)
  wend


  r=FastExp(b, q, n)

  if ( r <> 1 ) then
    e=0
    while ( e < (t-1) )
      if ( r <> (n-1) ) then
        r=FastExp(r, r, n)
        else
        Exit While
      end if

      e=e+1
    wend
    [ExitLoop]
  end if


  if ( (r=1) OR (r=(n-1)) ) then
      MillerRabin=0
    else
      MillerRabin=1
  end if

[ExtFn]

End Function

```


## Mathematica


```Mathematica
MillerRabin[n_,k_]:=Module[{d=n-1,s=0,test=True},While[Mod[d,2]==0 ,d/=2 ;s++]
Do[
  a=RandomInteger[{2,n-1}]; x=PowerMod[a,d,n];
  If[x!=1,
   For[ r = 0, r < s, r++, If[x==n-1, Continue[]]; x = Mod[x*x, n]; ];
   If[ x != n-1, test=False ];
  ];
,{k}];
Print[test] ]
```

{{out|Example output (not using the PrimeQ builtin)}}

```mathematica
MillerRabin[17388,10]
->False
```



## Maxima



```maxima
/* Miller-Rabin algorithm is builtin, see function primep. Here is another implementation */


/* find highest power of p, p^s, that divide n, and return s and n / p^s */

facpow(n, p) := block(
   [s: 0],
   while mod(n, p) = 0 do (s: s + 1, n: quotient(n, p)),
   [s, n]
)$

/* check whether n is a strong pseudoprime to base a; s and d are given by facpow(n - 1, 2) */

sppp(n, a, s, d) := block(
   [x: power_mod(a, d, n), q: false],
   if x = 1 or x = n - 1 then true else (
      from 2 thru s do (
         x: mod(x * x, n),
         if x = 1 then return(q: false) elseif x = n - 1 then return(q: true)
      ),
      q
   )
)$

/* Miller-Rabin primality test. For n < 341550071728321, the test is deterministic;
   for larger n, the number of bases tested is given by the option variable
   primep_number_of_tests, which is used by Maxima in primep. The bound for deterministic
   test is also the same as in primep. */

miller_rabin(n) := block(
   [v: [2, 3, 5, 7, 11, 13, 17], s, d, q: true, a],
   if n < 19 then member(n, v) else (
      [s, d]: facpow(n - 1, 2),
      if n < 341550071728321 then (    /* see http://oeis.org/A014233 */
         for a in v do (
            if not sppp(n, a, s, d) then return(q: false)
         ),
         q
      ) else (
         thru primep_number_of_tests do (
            a: 2 + random(n - 3),
            if not sppp(n, a, s, d) then return(q: false)
         ),
         q
      )
   )
)$
```



## Mercury


This naive implementation of a Miller-Rabin method is adapted from
the Prolog version on this page. The use of the form integer(N) to
permit use of integers of arbitrary precision as done here is not
efficient. It is better to define a tabled version of each known
integer and to use the tabled versions. For example, suppose you want
integer(2), then do

    :- func n2 = integer.integer.
    :- pragma memo(n2/0).
    n2 = integer.integer(2).

and use n2 as the integer in your code. Performance will be greatly
improved. Also Mercury has a package using Tom's Math for integers of
arbitrary precision and another package to some of the functions of the
GMP library for much faster operation with long integers. These can be
found with instructions for use in Github.


```Mercury

%----------------------------------------------------------------------%
:- module primality.

:- interface.

:- import_module integer.
:- pred is_prime(integer::in, integer::out) is multi.

%----------------------------------------------------------------------%
:- implementation.

:- import_module bool, int, list, math, require, string.

%----------------------------------------------------------------------%
        %    is_prime/2 implements a Miller-Rabin primality test, is
        %    deterministic for N < 3.415e+14, and is probabilistic for
        %    larger N. Returns integer(0) if not prime, integer(1) if prime,
        %    and -integer(1) if fails.

% :- pred is_prime(integer::in, integer::out) is multi.

is_prime(N, P) :-
	N < integer(2), P = integer(0).
is_prime(N, P) :-
	N = integer(2), P = integer(1).
is_prime(N, P) :-
	N = integer(3), P = integer(1).
is_prime(N, P) :-      %% even numbers > 3: false
	N > integer(3),
	(N mod integer(2)) = integer(0),
	P = integer(0).
		%%-------------deterministic--------
is_prime(N, P) :-      %% 3 < odd number < 3.415e+14
	N > integer(3),
	(N mod integer(2)) = integer(1),
	N < integer(341550071728321),
	deterministic_witnesses(N, DList),
	is_mr_prime(N, DList, R),
	P = R.
		%%-------------probabilistic--------
is_prime(N, P) :-      %% 3.415e+14 =< odd number
	N > integer(3),
	(N mod integer(2)) = integer(1),
	N >= integer(341550071728321),
	random_witnesses(N, 20, RList),
	is_mr_prime(N, RList, R),
	P = R.
is_prime(_N, P) :- P = -integer(1).

%----------------------------------------------------------------------%
% returns list of deterministic witnesses

:- pred deterministic_witnesses(integer::in,
        list(integer)::out) is multi.

deterministic_witnesses(N, L) :- N < integer(1373653),
	L = [integer(2), integer(3)].
deterministic_witnesses(N, L) :- N < integer(9080191),
	L = [integer(31), integer(73)].
deterministic_witnesses(N, L) :- N < integer(25326001),
	L = [integer(2), integer(3), integer(5)].
deterministic_witnesses(N, L) :- N < integer(3215031751),
	L = [integer(2), integer(3), integer(5), integer(7)].
deterministic_witnesses(N, L) :- N < integer(4759123141),
	L = [integer(2), integer(7), integer(61)].
deterministic_witnesses(N, L) :- N < integer(1122004669633),
	L = [integer(2), integer(13), integer(23), integer(1662803)].
deterministic_witnesses(N, L) :- N < integer(2152302898747),
	L = [integer(2), integer(3), integer(5), integer(7), integer(11)].
deterministic_witnesses(N, L) :- N < integer(3474749660383),
	L = [integer(2), integer(3), integer(5), integer(7), integer(11),
                  integer(13)].
deterministic_witnesses(N, L) :- N < integer(341550071728321),
	L = [integer(2), integer(3), integer(5), integer(7),
	        integer(11), integer(13), integer(17)].
deterministic_witnesses(_N, L) :- L = [].     %% signals failure

%----------------------------------------------------------------------%
	%% random_witnesses/3 receives an integer, X, an int, K, and
	%%   returns a list, P, of K pseudo-random integers in a range
	%%   1 to X-1.

:- pred random_witnesses(integer::in, int::in,
        list(integer)::out) is det.

random_witnesses(X, K, P) :-
	A = integer(6364136223846793005),
	B = integer(1442695040888963407),
	C = X - integer(2),
	rw_loop(X, A, B, C, K, [], P).

:- pred rw_loop(integer::in, integer::in, integer::in, integer::in,
        int::in, list(integer)::in, list(integer)::out) is det.

rw_loop(X, A, B, C, K, L, P) :-
	X1 = (((X * A) + B) mod C) + integer(1),
	( if K = 0 then P = L
	  else rw_loop(X1, A, B, C, K-1, [X1|L], P)
	).

%----------------------------------------------------------------------%
	% is_mr_prime/2 receives integer N and list As and returns true if
	%    N is probably prime, and false otherwise

:- pred is_mr_prime(integer::in, list(integer)::in, integer::out) is nondet.

is_mr_prime(N, As, R) :-
    find_ds(N, L),
    L = [D|T],
    T = [S|_],
    outer_loop(N, As, D, S, R).

:- pred outer_loop(integer::in, list(integer)::in, integer::in,
        integer::in, integer::out) is nondet.

outer_loop(N, As, D, S, R) :-
    As = [A|At],
    Base = powm(A, D, N),   %% = A^D mod N
    inner_loop(Base, N, integer(0), S, U),
	(  if U = integer(0) then R = integer(0)
	   else ( if At = [] then R = integer(1)
                    else            outer_loop(N, At, D, S, R)
                  )
	).

:- pred inner_loop(integer::in, integer::in, integer::in,
        integer::in, integer::out) is multi.

inner_loop(Base, N, Loop, S, U) :-
    Next_Base = (Base * Base) mod N,
    Next_Loop = Loop + integer(1),
	( if Loop = integer(0) then
	  	  ( if Base = integer(1)          then U = integer(1) % true
                    else if Base = N - integer(1) then U = integer(1) % true
                    else if Next_Loop = S         then U = integer(0) % false
                    else               inner_loop(Next_Base, N, Next_Loop, S, U)
                  )
	  else if Base = N - integer(1)       then U = integer(1) % true
	  else if Next_Loop = S               then U = integer(0) % false
	  else                     inner_loop(Next_Base, N, Next_Loop, S, U)
	).

%----------------------------------------------------------------------%
	% find_ds/2 receives odd integer N
	%    and returns [D, S] such that N-1 = 2^S * D

:- pred find_ds(integer::in, list(integer)::out) is multi.

find_ds(N, L) :-
        A = N - integer(1),
        find_ds1(A, integer(0), L).

:- pred find_ds1(integer::in, integer::in, list(integer)::out) is multi.

find_ds1(D, S, L) :-
	D mod integer(2) = integer(0),
	P = D div integer(2),
	Q = S + integer(1),
	find_ds1(P, Q, L).
find_ds1(D, S, L) :-
	L = [D, S].

%----------------------------------------------------------------------%
:- func powm(integer, integer, integer) = integer.

	% computes A^D mod N

powm(A, D, N) =
        ( if   D = integer(0) then integer(1)
          else ( if  (D mod integer(2)) = integer(0) then
                       (integer.pow(powm(A, (D div integer(2)), N),
                            integer(2))) mod N
                    else (A * powm(A, D - integer(1), N)) mod N
                 )
	).

%----------------------------------------------------------------------%
:- end_module primality.

% A means of testing the predicate is_prime/2
%----------------------------------------------------------------------%

:- module test_is_prime.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

%----------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, int, integer, list, math, require, string.
:- import_module primality.

%----------------------------------------------------------------------%
            % TEST THE IS_PRIME PREDICATE
            % $ ./test_is_prime <integer>
       %---------------------------------------------%

main(!IO) :-
    command_line_arguments(Args, !IO),
    filter(is_all_digits, Args, CleanArgs),
    Arg1 = list.det_index0(CleanArgs, 0),
    M = integer.det_from_string(Arg1),
	is_prime(M,P),
	io.format("   is_prime(%s) = ", [s(integer.to_string(M))], !IO),
	( if      P = integer(0) then io.write_string("false.\n", !IO)
	  else if P = integer(1) then io.write_string("true.\n", !IO)
	  else if P = -integer(1) then
                             io.write_string("N fails all tests.\n", !IO)
	  else io.write_string("Has reported neither true nor false
                            nor any error condition.\n", !IO)
	).

%----------------------------------------------------------------------%
:- end_module test_is_prime.


```



## Nim



```nim

## Nim currently doesn't have a BigInt standard library
## so we translate the version from Go which uses a
## deterministic approach, which is correct for all
## possible values in uint32.

proc isPrime*(n: uint32): bool =
  # bases of 2, 7, 61 are sufficient to cover 2^32
  case n
  of 0, 1: return false
  of 2, 7, 61: return true
  else: discard

  var
    nm1 = n-1
    d = nm1.int
    s = 0
    n = n.uint64

  while d mod 2 == 0:
    d = d shr 1
    s += 1

  for a in [2, 7, 61]:
    var
      x = 1.uint64
      p = a.uint64
      dr = d

    while dr > 0:
      if dr mod 2 == 1:
        x = x * p mod n
      p = p * p mod n
      dr = dr shr 1

    if x == 1 or x.uint32 == nm1:
      continue

    var r = 1
    while true:
      if r >= s:
        return false
      x = x * x mod n
      if x == 1:
        return false
      if x.uint32 == nm1:
        break
      r += 1

  return true

proc isPrime*(n: int32): bool =
  ## Overload for int32
  n >= 0 and n.uint32.isPrime

when isMainModule:
  const primeNumber1000 = 7919 # source: https://en.wikipedia.org/wiki/List_of_prime_numbers
  var
    i = 0u32
    numberPrimes = 0
  while true:
    if isPrime(i):
      if numberPrimes == 999:
        break
      numberPrimes += 1
    i += 1

  assert i == primeNumber1000
  assert isPrime(2u32)
  assert isPrime(31u32)
  assert isPrime(37u32)
  assert isPrime(1123u32)
  assert isPrime(492366587u32)
  assert isPrime(1645333507u32)

```


=== This is a correct M-R test implementation for using bases > input. ===
=== It is deterministic for all integers < 2^64.===


```nim


# Compile as: $ nim c -d:release mrtest.nim
# Run using: $ ./mrtest

import math                   # for gcd and mod
import bitops                 # for countTrailingZeroBits
import strutils, typetraits   # for number input
import times, os              # for timing code execution

proc addmod*[T: SomeInteger](a, b, modulus: T): T =
  ## Modular addition
  let a_m = if a < modulus: a else: a mod modulus
  if b == 0.T: return a_m
  let b_m = if b < modulus: b else: b mod modulus

  # Avoid doing a + b that could overflow here
  let b_from_m = modulus - b_m
  if a_m >= b_from_m: return a_m - b_from_m
  return a_m + b_m  # safe to add here; a + b < modulus

proc mulmod*[T: SomeInteger](a, b, modulus: T): T =
  ## Modular multiplication
  var a_m = if a < modulus: a else: a mod modulus
  var b_m = if b < modulus: b else: b mod modulus
  if b_m > a_m: swap(a_m, b_m)
  while b_m > 0.T:
    if (b_m and 1) == 1: result = addmod(result, a_m, modulus)
    a_m = (a_m shl 1) - (if a_m >= (modulus - a_m): modulus else: 0)
    b_m = b_m shr 1

proc expmod*[T: SomeInteger](base, exponent, modulus: T): T =
  ## Modular exponentiation
  result = 1 # (exp 0 = 1)
  var (e, b) = (exponent, base)
  while e > 0.T:
    if (e and 1) == 1: result = mulmod(result, b, modulus)
    e = e shr 1
    b = mulmod(b, b, modulus)

# Returns true if +self+ passes Miller-Rabin Test on witnesses +b+
proc miller_rabin_test[T: SomeInteger](num: T, witnesses: seq[uint64]): bool =
  var d = num - 1
  let (neg_one_mod, n) = (d, d)
  d = d shr countTrailingZeroBits(d) # suck out factors of 2 from d
  for b in witnesses:                # do M-R test with each witness base
    if b.T mod num == 0: continue    # **skip base if a multiple of input**
    var s = d
    var y = expmod(b.T, d, num)
    while s != n and y != 1 and y != neg_one_mod:
      y = mulmod(y, y, num)
      s = s shl 1
    if y != neg_one_mod and (s and 1) != 1: return false
  true

proc selectWitnesses[T: SomeInteger](num: T): seq[uint64] =
  ## Best known deterministic witnesses for given range and number of bases
  ## https://miller-rabin.appspot.com/
  ## https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
  if num < 341_531u:
    result = @[9345883071009581737u64]
  elif num < 1_050_535_501u:
    result = @[336781006125u64, 9639812373923155u64]
  elif num < 350_269_456_337u:
    result = @[4230279247111683200u64, 14694767155120705706u64, 16641139526367750375u64]
  elif num < 55_245_642_489_451u:
    result = @[2u64, 141889084524735u64, 1199124725622454117u64, 11096072698276303650u64]
  elif num < 7_999_252_175_582_851u:
    result = @[2u64, 4130806001517u64, 149795463772692060u64, 186635894390467037u64, 3967304179347715805u64]
  elif num < 585_226_005_592_931_977u:
    result = @[2u64, 123635709730000u64, 9233062284813009u64, 43835965440333360u64, 761179012939631437u64, 1263739024124850375u64]
  elif num.uint64 < 18_446_744_073_709_551_615u64:
    result = @[2u64, 325, 9375, 28178, 450775, 9780504, 1795265022]
  else:
    result = @[2u64, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]

proc primemr*[T: SomeInteger](n: T): bool =
  let primes = @[2u64, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
  if n <= primes[^1].T: return (n in primes) # for n <= primes.last
  let modp47 = 614889782588491410u     # => primes.product, largest < 2^64
  if gcd(n, modp47) != 1: return false # eliminates 86.2% of all integers
  let witnesses = selectWitnesses(n)
  miller_rabin_test(n, witnesses)

echo "\nprimemr?"
echo("n = ", 1645333507u)
var te = epochTime()
echo primemr 1645333507u
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
echo("n = ", 2147483647u)
te = epochTime()
echo primemr 2147483647u
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
echo("n = ", 844674407370955389u)
te = epochTime()
echo primemr 844674407370955389u
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
echo("n = ", 1844674407370954349u)
te = epochTime()
echo primemr 1844674407370954349u
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
echo("n = ", 1844674407370954351u)
te = epochTime()
echo primemr 1844674407370954351u
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
echo("n = ", 9223372036854775783u)
te = epochTime()
echo primemr 9223372036854775783u
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
echo("n = ", 9241386435364257883u64)
te = epochTime()
echo primemr 9241386435364257883u64
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
echo("n = ", 18446744073709551533u64, ", is largest prime < 2^64")
te = epochTime()
echo 18446744073709551533u64.primemr
echo (epochTime()-te).formatFloat(ffDecimal, 6)

echo "\nprimemr?"
let num = 5_000_000u        # => 348_513 primes
var primes: seq[uint] = @[]
echo("find primes < ", num)
te = epochTime()
for n in 0u..num:
  if n.primemr: primes.add(n)
  stdout.write("\r",((float64(n) / float64(num))*100).formatFloat(ffDecimal, 1), "%")
echo("\nnumber of primes < ",num, " are ", primes.len)
echo (epochTime()-te).formatFloat(ffDecimal, 6)

```



## Oz


This naive implementation of a Miller-Rabin method is adapted from
the Mercury and Prolog versions on this page.


```Oz

%--------------------------------------------------------------------------%
% module: Primality
% file: Primality.oz
% version: 17 DEC 2014 @ 6:50AM
%--------------------------------------------------------------------------%

declare
%--------------------------------------------------------------------------%

fun {IsPrime N}			% main interface of module
	if N < 2 then false
	elseif N < 4 then true
	elseif (N mod 2) == 0 then false
	elseif N < 341330071728321 then {IsMRprime N {DetWit N}}
	else {IsMRprime N {ProbWit N 20}}
	end
end
%--------------------------------------------------------------------------%

fun {DetWit N}			% deterministic witnesses
	if N < 1373653 then [2 3]
	elseif N < 9080191 then [31 73]
	elseif N < 25326001 then [2 3 5]
	elseif N < 3215031751 then [2 3 5 7]
	elseif N < 4759123141 then [2 7 61]
	elseif N < 1122004669633 then [2 13 23 1662803]
	elseif N < 2152302898747 then [2 3 5 7 11]
	elseif N < 3474749660383 then [2 3 5 7 11 13]
	elseif N < 341550071728321 then [2 3 5 7 11 13 17]
	else nil
	end
end
%--------------------------------------------------------------------------%

fun {ProbWit N K}		% probabilistic witnesses
	local A B C in
	A = 6364136223846793005
	B = 1442695040888963407
	C = N - 2
	{RWloop N A B C K nil}
	end
end

fun {RWloop N A B C K L}
	local N1 in
		N1 = (((N * A) + B) mod C) + 1
		if K == 0 then L
		else {RWloop N1 A B C (K - 1) N1|L}
		end
	end
end
%--------------------------------------------------------------------------%

fun {IsMRprime N As}	% the Miller-Rabin algorithm
	local D S T Ts in
	{FindDS N} = D|S
	{OuterLoop N As D S}
	end
end

fun {OuterLoop N As D S}
	local A At Base C in
	As = A|At
	Base = {Powm A D N}
	C = {InnerLoop Base N 0 S}
	if {Not C} then false
	elseif {And C (At == nil)} then true
	else {OuterLoop N At D S}
	end
	end
end

fun {InnerLoop Base N Loop S}
	local NextBase NextLoop in
	NextBase = (Base * Base) mod N
	NextLoop = Loop + 1
	   if {And (Loop == 0) (Base == 1)} then true
	   elseif Base == (N - 1) then true
	   elseif NextLoop == S then false
	   else {InnerLoop NextBase N NextLoop S}
	   end
	end
end
%--------------------------------------------------------------------------%

fun {FindDS N}
	{FindDS1 (N - 1) 0}
end

fun {FindDS1 D S}
	if (D mod 2 == 0) then {FindDS1 (D div 2) (S + 1)}
	else D|S
	end
end
%--------------------------------------------------------------------------%

fun {Powm A D N} 		% returns (A ^ D) mod N
	if D == 0 then 1
	elseif (D mod 2) == 0 then {Pow {Powm A (D div 2) N} 2} mod N
	else (A * {Powm A (D - 1) N}) mod N
	end
end
%--------------------------------------------------------------------------%
% end_module Primality


```



## PARI/GP

===Built-in===

```parigp
MR(n,k)=ispseudoprime(n,k);
```


### Custom


```parigp
sprp(n,b)={
	my(s = valuation(n-1, 2), d = Mod(b, n)^(n >> s));
	if (d == 1, return(1));
	for(i=1,s-1,
		if (d == -1, return(1));
		d = d^2;
	);
	d == -1
};

MR(n,k)={
  for(i=1,k,
    if(!sprp(n,random(n-2)+2), return(0))
  );
  1
};
```


### Deterministic version

A basic deterministic test can be obtained by an appeal to the ERH (as proposed by Gary Miller) and a result of Eric Bach (improving on Joseph Oesterlé).  Calculations of Jan Feitsma can be used to speed calculations below 2<sup>64</sup> (by a factor of about 250).

```parigp
A006945=[9, 2047, 1373653, 25326001, 3215031751, 2152302898747, 3474749660383, 341550071728321, 341550071728321, 3825123056546413051];
Miller(n)={
  if (n%2 == 0, return(n == 2)); \\ Handle even numbers
  if (n < 3, return(0)); \\ Handle 0, 1, and negative numbers

  if (n < 1<<64,
    \\ Feitsma
    for(i=1,#A006945,
      if (n < A006945[i], return(1));
      if(!sprp(n, prime(i)), return(0));
    );
    sprp(n,31)&sprp(n,37)
  ,
    \\ Miller + Bach
    for(b=2,2*log(n)^2,
      if(!sprp(n, b), return(0))
    );
    1
  )
};
```



## Perl



### Custom


```perl>use bigint try =
 'GMP';

sub is_prime {
    my ($n, $k) = @_;
    return 1 if $n == 2;
    return 0 if $n < 2 or $n % 2 == 0;

    my $d = $n - 1;
    my $s = 0;

    while (!($d % 2)) {
        $d /= 2;
        $s++;
    }

  LOOP: for (1 .. $k) {
        my $a = 2 + int(rand($n - 2));

        my $x = $a->bmodpow($d, $n);
        next if $x == 1 or $x == $n - 1;

        for (1 .. $s - 1) {
            $x = ($x * $x) % $n;
            return 0  if $x == 1;
            next LOOP if $x == $n - 1;
        }
        return 0;
    }
    return 1;
}

print join ", ", grep { is_prime $_, 10 } (1 .. 1000);
```



### Modules

{{libheader|ntheory}}
While normally one would use <tt>is_prob_prime</tt>, <tt>is_prime</tt>, or <tt>is_provable_prime</tt>, which will do a [[wp:Baillie--PSW_primality_test|BPSW test]] and possibly more, we can use just the Miller-Rabin test if desired.  For large values we can use an object (e.g. bigint, Math::GMP, Math::Pari, etc.) or just a numeric string.

```perl
use ntheory qw/is_strong_pseudoprime miller_rabin_random/;
sub is_prime_mr {
  my $n = shift;
  # If 32-bit, we can do this with 3 bases.
  return is_strong_pseudoprime($n, 2, 7, 61) if ($n >> 32) == 0;
  # If 64-bit, 7 is all we need.
  return is_strong_pseudoprime($n, 2, 325, 9375, 28178, 450775, 9780504, 1795265022) if ($n >> 64) == 0;
  # Otherwise, perform a number of random base tests, and the result is a probable prime test.
  return miller_rabin_random($n, 20);
}
```

Math::Primality also has this functionality, though its function takes only one base and requires the input number to be less than the base.

```perl
use Math::Primality qw/is_strong_pseudoprime/;
sub is_prime_mr {
  my $n = shift;
  return 0 if $n < 2;
  for (2,3,5,7,11,13,17,19,23,29,31,37) {
    return 0 unless $n <= $_ || is_strong_pseudoprime($n,$_);
  }
  1;
}
for (1..100) { say if is_prime_mr($_) }
```

Math::Pari can be used in a fashion similar to the Pari/GP custom function.  The builtin accessed using a second argument to <tt>ispseudoprime</tt> was added to a later version of Pari (the Perl module uses version 2.1.7) so is not accessible directly from Perl.


## Perl 6

{{works with|Rakudo|2015-09-22}}

```perl6
# the expmod-function from: http://rosettacode.org/wiki/Modular_exponentiation
sub expmod(Int $a is copy, Int $b is copy, $n) {
	my $c = 1;
	repeat while $b div= 2 {
		($c *= $a) %= $n if $b % 2;
		($a *= $a) %= $n;
	}
	$c;
}

subset PrimeCandidate of Int where { $_ > 2 and $_ % 2 };

my Bool multi sub is_prime(Int $n, Int $k)            { return False; }
my Bool multi sub is_prime(2, Int $k)                 { return True; }
my Bool multi sub is_prime(PrimeCandidate $n, Int $k) {
	my Int $d = $n - 1;
	my Int $s = 0;

	while $d %% 2 {
		$d div= 2;
		$s++;
	}

	for (2 ..^ $n).pick($k) -> $a {
		my $x = expmod($a, $d, $n);

		# one could just write "next if $x == 1 | $n - 1"
		# but this takes much more time in current rakudo/nom
		next if $x == 1 or $x == $n - 1;

		for 1 ..^ $s {
			$x = $x ** 2 mod $n;
			return False if $x == 1;
			last if $x == $n - 1;
		}
		return False if $x !== $n - 1;
	}

	return True;
}

say (1..1000).grep({ is_prime($_, 10) }).join(", ");
```



## Phix

{{trans|C}}
=== native, determinstic to 94,910,107 ===
Native-types deterministic version, fails (false negative) at 94,910,107 on 32-bit [fully tested, ie from 1],
and at 4,295,041,217 on 64-bit [only tested from 4,290,000,000] - those limits have now been hard-coded below.

```Phix
function powermod(atom a, atom n, atom m)
-- calculate a^n%mod
    atom p = a, res = 1
    while n do
        if and_bits(n,1) then
            res = mod(res*p,m)
        end if
        p = mod(p*p,m)
        n = floor(n/2)
    end while
    return res
end function

function witness(atom n, atom s, atom d, sequence a)
-- n-1 = 2^s * d with d odd by factoring powers of 2 from n-1
    for i=1 to length(a) do
        atom x = powermod(a[i], d, n), y, w=s
        while w do
            y = mod(x*x,n)
            if y == 1 and x != 1 and x != n-1 then
                return false
            end if
            x = y
            w -= 1
        end while
        if y != 1 then
            return false
        end if
    end for
    return true;
end function

function is_prime_mr(atom n)
    if (mod(n,2)==0 and n!=2)
    or (n<2)
    or (mod(n,3)==0 and n!=3) then
        return false
    elsif n<=3 then
        return true
    end if
    atom d = floor(n/2)
    atom s = 1;
    while and_bits(d,1)=0 do
        d /= 2
        s += 1
    end while

    sequence a
    if n < 1373653 then
        a = {2, 3}
    elsif n < 9080191 then
        a = {31, 73}
    elsif (machine_bits()=32 and n < 94910107)
       or (machine_bits()=64 and n < 4295041217) then
        a = {2, 7, 61}
    else
        puts(1,"limits exceeded\n")
        return 0
    end if
    return witness(n, s, d, a)
end function

sequence tests = {999983,999809,999727,52633,60787,999999,999995,999991}
for i=1 to length(tests) do
    printf(1,"%d is %s\n",{tests[i],{"composite","prime"}[is_prime_mr(tests[i])+1]})
end for
```

{{out}}

```txt

999983 is prime
999809 is prime
999727 is prime
52633 is composite
60787 is composite
999999 is composite
999995 is composite
999991 is composite

```



###  gmp version

{{libheader|mpfr}}
completes near-instantly

```Phix
include mpfr.e
mpz b = mpz_init()
randstate state = gmp_randinit_mt()

mpz_set_str(b,"9223372036854774808")
integer c = 0
for i=4808 to 5808 do  -- (b ends thus)
    if mpz_probable_prime_p(b,state) then
        c += 1
        printf(1,"  %s%s",{mpz_get_str(b),"\n"[1..mod(c,5)=0]})
    end if
    mpz_add_ui(b,b,1)
end for
printf(1,"\n%d primes found\n\n",c-1)

constant tests = {"4547337172376300111955330758342147474062293202868155909489",
                  "4547337172376300111955330758342147474062293202868155909393"}
for i=1 to length(tests) do
    mpz_set_str(b,tests[i])
    string p = iff(mpz_probable_prime_p(b,state)?"is prime":"is composite")
    printf(1,"%s %s\n",{tests[i],p})
end for

for i=2 to 1300 do
    mpz_ui_pow_ui(b,2,i)
    mpz_sub_si(b,b,1)
    if mpz_probable_prime_p(b,state) then
        printf(1,"2^%d-1 is prime\n",{i})
    end if
end for
```

{{out}}

```txt

  9223372036854774893  9223372036854774917  9223372036854774937  9223372036854774959  9223372036854775057
  9223372036854775073  9223372036854775097  9223372036854775139  9223372036854775159  9223372036854775181
  9223372036854775259  9223372036854775279  9223372036854775291  9223372036854775337  9223372036854775351
  9223372036854775399  9223372036854775417  9223372036854775421  9223372036854775433  9223372036854775507
  9223372036854775549  9223372036854775643  9223372036854775783
23 primes found

4547337172376300111955330758342147474062293202868155909489 is prime
4547337172376300111955330758342147474062293202868155909393 is composite
2^2-1 is prime
2^3-1 is prime
2^5-1 is prime
2^7-1 is prime
2^13-1 is prime
2^17-1 is prime
2^19-1 is prime
2^31-1 is prime
2^61-1 is prime
2^89-1 is prime
2^107-1 is prime
2^127-1 is prime
2^521-1 is prime
2^607-1 is prime
2^1279-1 is prime

```



## PHP


```php
<?php
function is_prime($n, $k) {
    if ($n == 2)
        return true;
    if ($n < 2 || $n % 2 == 0)
        return false;

    $d = $n - 1;
    $s = 0;

    while ($d % 2 == 0) {
        $d /= 2;
        $s++;
    }

    for ($i = 0; $i < $k; $i++) {
        $a = rand(2, $n-1);

        $x = bcpowmod($a, $d, $n);
        if ($x == 1 || $x == $n-1)
            continue;

        for ($j = 1; $j < $s; $j++) {
            $x = bcmod(bcmul($x, $x), $n);
            if ($x == 1)
                return false;
            if ($x == $n-1)
                continue 2;
        }
        return false;
    }
    return true;
}

for ($i = 1; $i <= 1000; $i++)
    if (is_prime($i, 10))
        echo "$i, ";
echo "\n";
?>
```



## PicoLisp


```PicoLisp
(de longRand (N)
   (use (R D)
      (while (=0 (setq R (abs (rand)))))
      (until (> R N)
         (unless (=0 (setq D (abs (rand))))
            (setq R (* R D)) ) )
      (% R N) ) )

(de **Mod (X Y N)
   (let M 1
      (loop
         (when (bit? 1 Y)
            (setq M (% (* M X) N)) )
         (T (=0 (setq Y (>> 1 Y)))
            M )
         (setq X (% (* X X) N)) ) ) )

(de _prim? (N D S)
   (use (A X R)
      (while (> 2 (setq A (longRand N))))
      (setq R 0  X (**Mod A D N))
      (loop
         (T
            (or
               (and (=0 R) (= 1 X))
               (= X (dec N)) )
            T )
         (T
            (or
               (and (> R 0) (= 1 X))
               (>= (inc 'R) S) )
            NIL )
         (setq X (% (* X X) N)) ) ) )

(de prime? (N K)
   (default K 50)
   (and
      (> N 1)
      (bit? 1 N)
      (let (D (dec N)  S 0)
         (until (bit? 1 D)
            (setq
               D  (>> 1 D)
               S  (inc S) ) )
         (do K
            (NIL (_prim? N D S))
            T ) ) ) )
```

{{out}}

```txt
: (filter '((I) (prime? I)) (range 937 1000))
-> (937 941 947 953 967 971 977 983 991 997)

: (prime? 4547337172376300111955330758342147474062293202868155909489)
-> T

: (prime? 4547337172376300111955330758342147474062293202868155909393)
-> NIL
```



## Pike


```Pike



int pow_mod(int m, int i,int mod){
	int x=1,y=m%mod;
	while(i){
		if(i&1) x = x*y%mod;
		y =  y*y%mod;
		i>>=1;
	}
	return x;
}
bool mr_pass(int a, int s, int d, int n){
    int a_to_power = pow_mod(a, d, n);
    if(a_to_power == 1)
        return true;
    for(int i = 0; i< s - 1; i++){
        if(a_to_power == n - 1)
            return true;
        a_to_power = (a_to_power * a_to_power) % n;
    }
    return a_to_power == n - 1;
}

int is_prime(int n){
	array(int) prime ;
	prime = ({2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 47, 53, 59, 61, 67, 71, 79, 83, 89, 97, 101, 103, 107, 109, 113});
    int idx =  search( prime, n);
    if( n < 113 && n == prime[idx] ){
        return 1;
    }
    if(n < 2047) prime = ({2, 3});
    if(n < 1373653)prime = ({2, 3});
    if(n < 9080191)prime = ({31, 73});
    if(n < 25326001)prime = ({2, 3, 5});
    if(n < 3215031751)prime = ({2, 3, 5, 7});
    if(n < 4759123141)prime = ({2, 7, 61});
    if(n < 1122004669633)prime = ({2, 13, 23, 1662803});
    if(n < 2152302898747)prime = ({2, 3, 5, 7, 11});
    if(n < 3474749660383)prime = ({2, 3, 5, 7, 11, 13});
    if(n < 341550071728321)prime = ({2, 3, 5, 7, 11, 13, 17});
    if(n < 3825123056546413051)prime = ({2, 3, 5, 7, 11, 13, 17, 19, 23});
    if(n < 18446744073709551616)prime = ({2, 3, 5, 7, 11, 13, 17, 19, 23, 29});
    if(n < 318665857834031151167461)prime = ({2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31});
    if(n < 3317044064679887385961981)prime = ({2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37});
    else prime = ({2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 47, 53, 59, 61, 67, 71});
    int d = n - 1;
    int s = 0;
    while(d % 2 == 0){
    	d >>= 1;
    	s += 1;
    }
    for (int repeat=0 ; repeat < sizeof(prime); repeat++){
        int a = prime[repeat];
        if(!mr_pass(a, s, d, n)){
            return 0;
        }
    }
    return 1;
}
int main() {
	array(int) lists;
	lists =({35201546659608842026088328007565866231962578784643756647773109869245232364730066609837018108561065242031153677,
10513733234846849736873637829838635104309714688896631127438692162131857778044158273164093838937083421380041997,
24684249032065892333066123534168930441269525239006410135714283699648991959894332868446109170827166448301044689,
76921421106760125285550929240903354966370431827792714920086011488103952094969175731459908117375995349245839343,
32998886283809577512914881459957314326750856532546837122557861905096711274255431870995137546575954361422085081,
30925729459015376480470394633122420870296099995740154747268426836472045107181955644451858184784072167623952123,
14083359469338511572632447718747493405040362318205860500297736061630222431052998057250747900577940212317413063,
10422980533212493227764163121880874101060283221003967986026457372472702684601194916229693974417851408689550781,
36261430139487433507414165833468680972181038593593271409697364115931523786727274410257181186996611100786935727,
15579763548573297857414066649875054392128789371879472432457450095645164702139048181789700140949438093329334293});
	for(int i=0;i<sizeof(lists);i++){
		int n = lists[i];
		int chk = is_prime(n);
		if(chk == 1) write(sprintf("%d %s\n",n,"PRIME"));
		else write(sprintf("%d %s\n",n,"COMPOSIT"));
	}
}

TEST

35201546659608842026088328007565866231962578784643756647773109869245232364730066609837018108561065242031153677 PRIME
10513733234846849736873637829838635104309714688896631127438692162131857778044158273164093838937083421380041997 PRIME
24684249032065892333066123534168930441269525239006410135714283699648991959894332868446109170827166448301044689 PRIME
76921421106760125285550929240903354966370431827792714920086011488103952094969175731459908117375995349245839343 PRIME
32998886283809577512914881459957314326750856532546837122557861905096711274255431870995137546575954361422085081 PRIME
30925729459015376480470394633122420870296099995740154747268426836472045107181955644451858184784072167623952123 PRIME
14083359469338511572632447718747493405040362318205860500297736061630222431052998057250747900577940212317413063 PRIME
10422980533212493227764163121880874101060283221003967986026457372472702684601194916229693974417851408689550781 PRIME
36261430139487433507414165833468680972181038593593271409697364115931523786727274410257181186996611100786935727 PRIME
15579763548573297857414066649875054392128789371879472432457450095645164702139048181789700140949438093329334293 PRIME

```



## Prolog


This naive implementation of a Miller-Rabin method is adapted
from the Erlang version on this page.


```prolog
:- module(primality, [is_prime/2]).

% is_prime/2 returns false if N is composite, true if N probably prime
%    implements a Miller-Rabin primality test and is deterministic for N < 3.415e+14,
%    and is probabilistic for larger N. Adapted from the Erlang version.
is_prime(1, Ret) :- Ret = false, !.          % 1 is non-prime
is_prime(2, Ret) :- Ret = true, !.           % 2 is prime
is_prime(3, Ret) :- Ret = true, !.           % 3 is prime
is_prime(N, Ret) :-
	N > 3, (N mod 2 =:= 0), Ret = false, !.  % even number > 3 is composite
is_prime(N, Ret) :-
	N > 3, (N mod 2 =:= 1),                  % odd number > 3
	N < 341550071728321,
	deterministic_witnesses(N, L),
 	is_mr_prime(N, L, Ret), !.               % deterministic test
is_prime(N, Ret) :-
	random_witnesses(N, 100, [], Out),
	is_mr_prime(N, Out, Ret), !.             % probabilistic test

% returns list of deterministic witnesses
deterministic_witnesses(N, L) :- N < 1373653,
	L = [2, 3].
deterministic_witnesses(N, L) :- N < 9080191,
	L = [31, 73].
deterministic_witnesses(N, L) :- N < 25326001,
	L = [2, 3, 5].
deterministic_witnesses(N, L) :- N < 3215031751,
	L = [2, 3, 5, 7].
deterministic_witnesses(N, L) :- N < 4759123141,
	L = [2, 7, 61].
deterministic_witnesses(N, L) :- N < 1122004669633,
	L = [2, 13, 23, 1662803].
deterministic_witnesses(N, L) :- N < 2152302898747,
	L = [2, 3, 5, 7, 11].
deterministic_witnesses(N, L) :- N < 3474749660383,
	L = [2, 3, 5, 7, 11, 13].
deterministic_witnesses(N, L) :- N < 341550071728321,
	L = [2, 3, 5, 7, 11, 13, 17].

% random_witnesses/4 returns a list of K witnesses selected at random with range 2 -> N-2
random_witnesses(_, 0, T, T).
random_witnesses(N, K, T, Out) :-
		G is N - 2,
		H is 1 + random(G),
		I is K - 1,
    	random_witnesses(N, I, [H | T], Out), !.

% find_ds/2 receives odd integer N and returns [D, S] s.t. N-1 = 2^S * D
find_ds(N, L) :-
	A is N - 1,
    find_ds(A, 0, L), !.

find_ds(D, S, L) :-
	D mod 2 =:= 0,
	P is D // 2,
	Q is S + 1,
	find_ds(P, Q, L), !.
find_ds(D, S, L) :-
	L = [D, S].

is_mr_prime(N, As, Ret) :-
    find_ds(N, L),
    L = [D | T],
    T = [S | _],
    outer_loop(N, As, D, S, Ret), !.

outer_loop(N, As, D, S, Ret) :-
    As = [A | At],
    Base is powm(A, D, N),
    inner_loop(Base, N, 0, S, Result),
	(  Result == false           -> Ret = false
	;  Result == true, At == []  -> Ret = true
	;  outer_loop(N, At, D, S, Ret)
	).

inner_loop(Base, N, Loop, S, Result) :-
    Next_Base is (Base * Base) mod N,
    Next_Loop is Loop + 1,
	( Loop =:= 0, Base =:= 1   -> Result = true
	;             Base =:= N-1 -> Result = true
	; Next_Loop =:= S          -> Result = false
	; inner_loop(Next_Base, N, Next_Loop, S, Result)
	).
```



## PureBasic


```PureBasic
Enumeration
  #Composite
  #Probably_prime
EndEnumeration

Procedure Miller_Rabin(n, k)
  Protected d=n-1, s, x, r
  If n=2
    ProcedureReturn #Probably_prime
  ElseIf n%2=0 Or n<2
    ProcedureReturn #Composite
  EndIf
  While d%2=0
    d/2
    s+1
  Wend
  While k>0
    k-1
    x=Int(Pow(2+Random(n-4),d))%n
    If x=1 Or x=n-1: Continue: EndIf
    For r=1 To s-1
      x=(x*x)%n
      If x=1: ProcedureReturn #Composite: EndIf
      If x=n-1: Break: EndIf
    Next
    If x<>n-1: ProcedureReturn #Composite: EndIf
  Wend
  ProcedureReturn #Probably_prime
EndProcedure
```



## Python




### Python: Probably correct answers

This versions  will give answers with a very small probability of being false. That probability being dependent number of trials (automatically set to 8).


```python


import random

def is_Prime(n):
    """
    Miller-Rabin primality test.

    A return value of False means n is certainly not prime. A return value of
    True means n is very likely a prime.
    """
    if n!=int(n):
        return False
    n=int(n)
    #Miller-Rabin test for prime
    if n==0 or n==1 or n==4 or n==6 or n==8 or n==9:
        return False

    if n==2 or n==3 or n==5 or n==7:
        return True
    s = 0
    d = n-1
    while d%2==0:
        d>>=1
        s+=1
    assert(2**s * d == n-1)

    def trial_composite(a):
        if pow(a, d, n) == 1:
            return False
        for i in range(s):
            if pow(a, 2**i * d, n) == n-1:
                return False
        return True

    for i in range(8):#number of trials
        a = random.randrange(2, n)
        if trial_composite(a):
            return False

    return True
```



### Python: Proved correct up to large N

This versions will give correct answers for <code>n</code> less than 341550071728321 and then reverting to the probabilistic form of the first solution. By selecting [http://primes.utm.edu/prove/prove2_3.html predetermined values] for the <code>a</code> values to use instead of random values, the results can be shown to be deterministically correct below certain thresholds.

For 341550071728321 and beyond, I have followed the pattern in choosing <code>a</code> from the set of prime numbers.
While this uses the best sets known in 1993, there are [http://miller-rabin.appspot.com/ better sets known], and at most 7 are needed for 64-bit numbers.


```python
def _try_composite(a, d, n, s):
    if pow(a, d, n) == 1:
        return False
    for i in range(s):
        if pow(a, 2**i * d, n) == n-1:
            return False
    return True # n  is definitely composite

def is_prime(n, _precision_for_huge_n=16):
    if n in _known_primes:
        return True
    if any((n % p) == 0 for p in _known_primes) or n in (0, 1):
        return False
    d, s = n - 1, 0
    while not d % 2:
        d, s = d >> 1, s + 1
    # Returns exact according to http://primes.utm.edu/prove/prove2_3.html
    if n < 1373653:
        return not any(_try_composite(a, d, n, s) for a in (2, 3))
    if n < 25326001:
        return not any(_try_composite(a, d, n, s) for a in (2, 3, 5))
    if n < 118670087467:
        if n == 3215031751:
            return False
        return not any(_try_composite(a, d, n, s) for a in (2, 3, 5, 7))
    if n < 2152302898747:
        return not any(_try_composite(a, d, n, s) for a in (2, 3, 5, 7, 11))
    if n < 3474749660383:
        return not any(_try_composite(a, d, n, s) for a in (2, 3, 5, 7, 11, 13))
    if n < 341550071728321:
        return not any(_try_composite(a, d, n, s) for a in (2, 3, 5, 7, 11, 13, 17))
    # otherwise
    return not any(_try_composite(a, d, n, s)
                   for a in _known_primes[:_precision_for_huge_n])

_known_primes = [2, 3]
_known_primes += [x for x in range(5, 1000, 2) if is_prime(x)]
```


;Testing:
Includes test values from other examples:

```txt
>>> is_prime(4547337172376300111955330758342147474062293202868155909489)
True
>>> is_prime(4547337172376300111955330758342147474062293202868155909393)
False
>>> [x for x in range(901, 1000) if is_prime(x)]
[907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997]
>>> is_prime(643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153)
True
>>> is_prime(743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153)
False
>>>
```



## Racket


```Racket
#lang racket
(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1)
               (not (= x 1))
               (not (= x (- m 1))))
          0
          square))
    (check-nontrivial-sqrt1 x (remainder (expt x 2) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (squaremod-with-check
                      (miller-rabin-expmod base (/ exp 2) m)))
        (else
         (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 0)) (= x 1)))
    (check-it (miller-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (remainder (- n 1) 4294967087)))))

(define (fast-prime? n times)
  (for/and ((i (in-range times)))
    (miller-rabin-test n)))

(define (prime? n(times 100))
  (fast-prime? n times))

(prime? 4547337172376300111955330758342147474062293202868155909489) ;-> outputs true

```



## REXX

With a   '''K'''    of   '''1''',   there seems to be a not uncommon number of failures, but
::: with a   '''K ≥ 2''',   the failures are seldom,
::: with a   '''K ≥ 3''',   the failures are rare as hen's teeth.

This would be in the same vein as:
3 is prime,  5 is prime,  7 is prime,  all odd numbers are prime.

The   '''K'''   (above) is signified by the REXX variable   '''times'''   in the REXX program below.


To make the program small, the   ''true prime generator''   ('''GenP''')   was coded to be small, but not particularly fast.

```rexx
/*REXX program puts the  Miller─Rabin  primality test  through its paces.               */
parse arg limit times seed .                     /*obtain optional arguments from the CL*/
if limit=='' | limit==","  then limit= 1000      /*Not specified?  Then use the default.*/
if times=='' | times==","  then times=   10      /*  "      "        "   "   "     "    */
if datatype(seed, 'W')  then call random ,,seed  /*If seed specified, use it for RANDOM.*/
numeric digits max(200, 2*limit)                 /*we're dealing with some ginormous #s.*/
tell= times<0                                    /*display primes  only if times is neg.*/
times= abs(times);         w= length(times)      /*use absolute value of TIMES; get len.*/
call genP  limit                                 /*suspenders now, use a belt later ··· */
@MR= 'Miller─Rabin primality test'               /*define a character literal for  SAY. */
say "There are"     #     'primes ≤'     limit   /*might as well display some stuff.    */
say                                              /* [↓]  (skipping unity); show sep line*/
     do a=2  to times;     say copies('─', 89)   /*(skipping unity)   do range of TIMEs.*/
     p= 0                                        /*the counter of primes for this pass. */
          do z=1  for limit                      /*now, let's get busy and crank primes.*/
          if \M_Rt(z, a)  then iterate           /*Not prime?   Then try another number.*/
          p= p + 1                               /*well, we found another one, by gum!  */
          if tell  then say z   'is prime according to'       @MR       "with K="a
          if !.z   then iterate
          say '[K='a"] "    z   "isn't prime !"  /*oopsy─doopsy  and/or  whoopsy─daisy !*/
          end   /*z*/
     say '        for 1──►'limit", K="right(a,w)',' @MR "found"  p  'primes {out of' #"}."
     end     /*a*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genP: parse arg high;    @.=0;    @.1=2;   @.2=3;    !.=@.;   !.2=1;   !.3=1;     #=2
         do j=@.#+2  by 2  to high               /*just examine odd integers from here. */
           do k=2  while k*k<=j;  if j//@.k==0  then iterate j;   end  /*k*/
         #= # + 1;       @.#= j;        !.j= 1   /*bump prime counter; add prime to the */
         end   /*j*/;      return                /*@. array; define a prime in !. array.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
M_Rt: procedure;  parse arg n,k;  d= n-1;   nL=d /*Miller─Rabin:   A.K.A.  Rabin─Miller.*/
      if n==2           then return 1            /*special case of (the)  even  prime.  */
      if n<2 | n//2==0  then return 0            /*check for too low, or an even number.*/

         do s=-1  while d//2==0;  d= d % 2       /*keep halving  until a zero remainder.*/
         end   /*while*/

                 do k;       ?= random(2, nL)    /* [↓]  perform the DO loop   K  times.*/
                 x= ?**d  //  n                  /*X  can get real gihugeic really fast.*/
                 if x==1 | x==nL  then iterate   /*First or penultimate? Try another pow*/
                   do s;     x= x**2  //  n      /*compute new  X   ≡   X²  modulus  N. */
                   if x==1   then return 0       /*if unity,  it's definitely not prime.*/
                   if x==nL  then leave          /*if  N-1,   then it  could  be prime. */
                   end   /*r*/                   /* [↑]  // is REXX's division remainder*/
                 if x\==nL   then return 0       /*nope, it ain't prime nohows, noway.  */
                 end     /*k*/                   /*maybe it's prime, maybe it ain't ··· */
      return 1                                   /*coulda/woulda/shoulda be prime;  yup.*/
```

{{out|output|text=  when using the input of:     <tt> 10000   10 </tt>}}

```txt

There are 1229 primes ≤ 10000

─────────────────────────────────────────────────────────────────────────────────────────
[K=2]  7201 isn't prime !
        for 1──►10000, K= 2, Miller─Rabin primality test found 1230 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K= 3, Miller─Rabin primality test found 1229 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K= 4, Miller─Rabin primality test found 1229 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K= 5, Miller─Rabin primality test found 1229 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K= 6, Miller─Rabin primality test found 1229 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K= 7, Miller─Rabin primality test found 1229 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K= 8, Miller─Rabin primality test found 1229 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K= 9, Miller─Rabin primality test found 1229 primes {out of 1229}.
─────────────────────────────────────────────────────────────────────────────────────────
        for 1──►10000, K=10, Miller─Rabin primality test found 1229 primes {out of 1229}.

```



## Ring


```ring

# Project : Miller–Rabin primality test

see "Input a number: " give n
see "Input test: " give k

test =  millerrabin(n,k)
if test = 0
   see "Probably Prime" + nl
else
   see "Composite" + nl
ok

func millerrabin(n, k)
       if n = 2
          millerRabin = 0
          return millerRabin
       ok
       if n % 2 = 0 or n < 2
          millerRabin = 1
          return millerRabin
       ok
       d = n - 1
       s = 0
       while d % 2 = 0
               d = d / 2
               s = s + 1
       end
       while k > 0
               k = k - 1
               base = 2 + floor((random(10)/10)*(n-3))
               x = pow(base, d) % n
               if x != 1 and x != n-1
                  for r=1 to s-1
                      x = (x * x) % n
                      if x = 1
                         millerRabin = 1
                         return millerRabin
                      ok
                      if x = n-1
                         exit
                      ok
                  next
                  if x != n-1
                     millerRabin = 1
                     return millerRabin
                  ok
                ok
     end

```

Output:

```txt

Input a number: 17
Input test: 8
Probably Prime

```



## Ruby


### Standard Probabilistic

From 2.5 Ruby has fast modular exponentiation built in. For alternatives prior to 2.5 please see [[Modular_exponentiation#Ruby]]

```ruby

def miller_rabin_prime?(n, g)
  d = n - 1
  s = 0
  while d % 2 == 0
    d /= 2
    s += 1
  end
  g.times do
    a = 2 + rand(n - 4)
    x = a.pow(d, n)  # x = (a**d) % n
    next if x == 1 || x == n - 1
    for r in (1..s - 1)
      x = x.pow(2, n) # x = (x**2) % n
      return false if x == 1
      break if x == n - 1
    end
    return false if x != n - 1
  end
  true # probably
end

p primes = (3..1000).step(2).find_all {|i| miller_rabin_prime?(i,10)}

```

{{out}}

```txt
[3, 5, 7, 11, 13, 17, ..., 971, 977, 983, 991, 997]
```

The following larger examples all produce true:

```ruby

puts miller_rabin_prime?(94366396730334173383107353049414959521528815310548187030165936229578960209523421808912459795329035203510284576187160076386643700441216547732914250578934261891510827140267043592007225160798348913639472564715055445201512461359359488795427875530231001298552452230535485049737222714000227878890892901228389026881,1000)
puts miller_rabin_prime?(138028649176899647846076023812164793645371887571371559091892986639999096471811910222267538577825033963552683101137782650479906670021895135954212738694784814783986671046107023185842481502719762055887490765764329237651328922972514308635045190654896041748716218441926626988737664133219271115413563418353821396401,1000)
puts miller_rabin_prime?(123301261697053560451930527879636974557474268923771832437126939266601921428796348203611050423256894847735769138870460373141723679005090549101566289920247264982095246187318303659027201708559916949810035265951104246512008259674244307851578647894027803356820480862664695522389066327012330793517771435385653616841,1000)
puts miller_rabin_prime?(119432521682023078841121052226157857003721669633106050345198988740042219728400958282159638484144822421840470442893056822510584029066504295892189315912923804894933736660559950053226576719285711831138657839435060908151231090715952576998400120335346005544083959311246562842277496260598128781581003807229557518839,1000)
puts miller_rabin_prime?(132082885240291678440073580124226578272473600569147812319294626601995619845059779715619475871419551319029519794232989255381829366374647864619189704922722431776563860747714706040922215308646535910589305924065089149684429555813953571007126408164577035854428632242206880193165045777949624510896312005014225526731,1000)
puts miller_rabin_prime?(153410708946188157980279532372610756837706984448408515364579602515073276538040155990230789600191915021209039203172105094957316552912585741177975853552299222501069267567888742458519569317286299134843250075228359900070009684517875782331709619287588451883575354340318132216817231993558066067063143257425853927599,1000)
puts miller_rabin_prime?(103130593592068072608023213244858971741946977638988649427937324034014356815504971087381663169829571046157738503075005527471064224791270584831779395959349442093395294980019731027051356344056416276026592333932610954020105156667883269888206386119513058400355612571198438511950152690467372712488391425876725831041,1000)

```


===Deterministic for integers < 3,317,044,064,679,887,385,961,981===
It extends '''class Integer''' to make it simpler to use.

```ruby

class Integer
    # Returns true if +self+ is a prime number, else returns false.
    def primemr?
      primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
      return primes.include? self if self <= primes.last
      modp47 = 614_889_782_588_491_410      # => primes.reduce(:*), largest < 2^64
      return false if self.gcd(modp47) != 1 # eliminates 86.2% of all integers
      # Choose witness bases for input; wits = [range, [wit_bases]] or nil
      wits = WITNESS_RANGES.find {|range, wits| range > self}
      witnesses = wits && wits[1] || primes
      miller_rabin_test(witnesses)
    end

    private
    # Returns true if +self+ passes Miller-Rabin Test with witness bases +b+
    def miller_rabin_test(witnesses)    # use witness list to test with
      neg_one_mod = n = d = self - 1    # these are even as 'self' always odd
      d >>= 4 while (d & 0xf) == 0                  # suck out factors of 2
      (d >>= (d & 3)^2; d >>= (d & 1)^1) if d.even? # 4 bits at a time
      witnesses.each do |b|             # do M-R test with each witness base
        next if (b % self) == 0         # **skip base if a multiple of input**
        s = d
        y = b.pow(d, self)    # y = (b**d) mod self
        until s == n || y == 1 || y == neg_one_mod
          y = y.pow(2, self)        # y = (y**2) mod self
          s <<= 1
        end
        return false unless y == neg_one_mod || s.odd?
      end
      true
    end

    # Best known deterministic witnesses for given range and set of bases
    # https://miller-rabin.appspot.com/
    # https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
    WITNESS_RANGES = {
      341_531 => [9345883071009581737],
      1_050_535_501 => [336781006125, 9639812373923155],
      350_269_456_337 => [4230279247111683200, 14694767155120705706, 16641139526367750375],
      55_245_642_489_451 => [2, 141889084524735, 1199124725622454117, 11096072698276303650],
      7_999_252_175_582_851 => [2, 4130806001517, 149795463772692060, 186635894390467037,
                                3967304179347715805],
      585_226_005_592_931_977 => [2, 123635709730000, 9233062284813009, 43835965440333360,
                                  761179012939631437, 1263739024124850375],
      18_446_744_073_709_551_615 => [2, 325, 9375, 28178, 450775, 9780504, 1795265022],
      318_665_857_834_031_151_167_461 => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37],
      3_317_044_064_679_887_385_961_981 => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]
    }
end

def tm; t = Time.now; yield; Time.now - t end

# 10 digit primes
n = 2147483647
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 18 digit non-prime
n = 844674407370955389
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 19 digit primes
n = 9241386435364257883
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 20 digit primes; largest < 2^64
n = 18446744073709551533
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 58 digit prime
n = 4547337172376300111955330758342147474062293202868155909489
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 58 digit non-prime
n = 4547337172376300111955330758342147474062293202868155909393
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 81 digit prime
n = 100000000000000000000000000000000000000000000000000000000000000000000000001309503
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 81 digit non-prime
n = 100000000000000000000000000000000000000000000000000000000000000000000000001309509
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

# 308 digit prime
n = 94366396730334173383107353049414959521528815310548187030165936229578960209523421808912459795329035203510284576187160076386643700441216547732914250578934261891510827140267043592007225160798348913639472564715055445201512461359359488795427875530231001298552452230535485049737222714000227878890892901228389026881
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = 138028649176899647846076023812164793645371887571371559091892986639999096471811910222267538577825033963552683101137782650479906670021895135954212738694784814783986671046107023185842481502719762055887490765764329237651328922972514308635045190654896041748716218441926626988737664133219271115413563418353821396401
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = 123301261697053560451930527879636974557474268923771832437126939266601921428796348203611050423256894847735769138870460373141723679005090549101566289920247264982095246187318303659027201708559916949810035265951104246512008259674244307851578647894027803356820480862664695522389066327012330793517771435385653616841
print "\n number = #{n} is prime "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = 119432521682023078841121052226157857003721669633106050345198988740042219728400958282159638484144822421840470442893056822510584029066504295892189315912923804894933736660559950053226576719285711831138657839435060908151231090715952576998400120335346005544083959311246562842277496260598128781581003807229557518839
print "\n number = #{n} is prime "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = 132082885240291678440073580124226578272473600569147812319294626601995619845059779715619475871419551319029519794232989255381829366374647864619189704922722431776563860747714706040922215308646535910589305924065089149684429555813953571007126408164577035854428632242206880193165045777949624510896312005014225526731
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = 153410708946188157980279532372610756837706984448408515364579602515073276538040155990230789600191915021209039203172105094957316552912585741177975853552299222501069267567888742458519569317286299134843250075228359900070009684517875782331709619287588451883575354340318132216817231993558066067063143257425853927599
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = 103130593592068072608023213244858971741946977638988649427937324034014356815504971087381663169829571046157738503075005527471064224791270584831779395959349442093395294980019731027051356344056416276026592333932610954020105156667883269888206386119513058400355612571198438511950152690467372712488391425876725831041
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

n = 94366396730334173383107353049414959521528815310548187030165936229578960209523421808912459795329035203510284576187160076386643700441216547732914250578934261891510827140267043592007225160798348913639472564715055445201512461359359488795427875530231001298552452230535485049737222714000227878890892901228389026881
print "\n number = #{n} is prime? "; print " in ", tm{ print n.primemr? }, " secs"
puts

```



## Run BASIC


''This code has not been fully tested.  Remove this comment after review.''


```runbasic
input "Input a number:";n
input "Input test:";k

test =  millerRabin(n,k)
if test = 0 then
  print "Probably Prime"
 else
  print "Composite"
end if
wait

' ----------------------------------------
' Returns
'  Composite     = 1
'  Probably Prime = 0
' ----------------------------------------
 FUNCTION millerRabin(n, k)
  if n = 2 then
    millerRabin = 0 'probablyPrime
    goto [funEnd]
  end if

  if n mod 2 = 0 or n < 2 then
    millerRabin = 1 'composite
    goto [funEnd]
  end if

d = n - 1
while d mod 2 = 0
  d = d / 2
  s = s + 1
wend

while k > 0
  k = k - 1
  base = 2 + int(rnd(1)*(n-3))
  x = (base^d) mod n
  if x <> 1 and x <> n-1 then
    for r=1 To s-1
      x =(x * x) mod n
      if x=1 then
       millerRabin = 1 ' composite
       goto [funEnd]
      end if
      if x = n-1 then exit for
    next r

    if x<>n-1 then
      millerRabin =  1 ' composite
      goto [funEnd]
    end if
  end if
wend
[funEnd]
END FUNCTION
```



## Rust



```rust
/* Add these lines to the [dependencies] section of your Cargo.toml file:
num = "0.2.0"
rand = "0.6.5"
*/

use num::bigint::BigInt;
use num::bigint::ToBigInt;


// The modular_exponentiation() function takes three identical types
// (which get cast to BigInt), and returns a BigInt:
fn modular_exponentiation<T: ToBigInt>(n: &T, e: &T, m: &T) -> BigInt {
    // Convert n, e, and m to BigInt:
    let n = n.to_bigint().unwrap();
    let e = e.to_bigint().unwrap();
    let m = m.to_bigint().unwrap();

    // Sanity check:  Verify that the exponent is not negative:
    assert!(e >= Zero::zero());

    use num::traits::{Zero, One};

    // As most modular exponentiations do, return 1 if the exponent is 0:
    if e == Zero::zero() {
        return One::one()
    }

    // Now do the modular exponentiation algorithm:
    let mut result: BigInt = One::one();
    let mut base = n % &m;
    let mut exp = e;

    loop {  // Loop until we can return our result.
        if &exp % 2 == One::one() {
            result *= &base;
            result %= &m;
        }

        if exp == One::one() {
            return result
        }

        exp /= 2;
        base *= base.clone();
        base %= &m;
    }
}


// is_prime() checks the passed-in number against many known small primes.
// If that doesn't determine if the number is prime or not, then the number
// will be passed to the is_rabin_miller_prime() function:
fn is_prime<T: ToBigInt>(n: &T) -> bool {
    let n = n.to_bigint().unwrap();
    if n.clone() < 2.to_bigint().unwrap() {
        return false
    }

    let small_primes = vec![2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
                            47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101,
                            103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
                            157, 163, 167, 173, 179, 181, 191, 193, 197, 199,
                            211, 223, 227, 229, 233, 239, 241, 251, 257, 263,
                            269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
                            331, 337, 347, 349, 353, 359, 367, 373, 379, 383,
                            389, 397, 401, 409, 419, 421, 431, 433, 439, 443,
                            449, 457, 461, 463, 467, 479, 487, 491, 499, 503,
                            509, 521, 523, 541, 547, 557, 563, 569, 571, 577,
                            587, 593, 599, 601, 607, 613, 617, 619, 631, 641,
                            643, 647, 653, 659, 661, 673, 677, 683, 691, 701,
                            709, 719, 727, 733, 739, 743, 751, 757, 761, 769,
                            773, 787, 797, 809, 811, 821, 823, 827, 829, 839,
                            853, 857, 859, 863, 877, 881, 883, 887, 907, 911,
                            919, 929, 937, 941, 947, 953, 967, 971, 977, 983,
                            991, 997, 1009, 1013];

    use num::traits::Zero;  // for Zero::zero()

    // Check to see if our number is a small prime (which means it's prime),
    // or a multiple of a small prime (which means it's not prime):
    for sp in small_primes {
        let sp = sp.to_bigint().unwrap();

        if n.clone() == sp {
            return true
        } else if n.clone() % sp == Zero::zero() {
            return false
        }
    }

    is_rabin_miller_prime(&n, None)
}


// Note:  "use bigint::RandBigInt;"  (which is needed for gen_bigint_range())
//        fails to work in the Rust playground ( https://play.rust-lang.org ).
//        Therefore, I'll create my own here:
fn get_random_bigint(low: &BigInt, high: &BigInt) -> BigInt {
    if low == high {  // base case
        return low.clone()
    }

    let middle = (low.clone() + high) / 2.to_bigint().unwrap();

    let go_low: bool = rand::random();

    if go_low {
        return get_random_bigint(low, &middle)
    } else {
        return get_random_bigint(&middle, high)
    }
}


// k is the number of times for testing (pass in None to use 5 (the default)).
fn is_rabin_miller_prime<T: ToBigInt>(n: &T, k: Option<usize>) -> bool {
    let n = n.to_bigint().unwrap();
    let k = k.unwrap_or(5);  // number of times for testing (defaults to 5)

    use num::traits::{Zero, One};  // for Zero::zero() and One::one()
    let zero: BigInt = Zero::zero();
    let one: BigInt = One::one();
    let two: BigInt = 2.to_bigint().unwrap();

    // The call to is_prime() should have already checked this,
    // but check for two, less than two, and multiples of two:
    if n <= one {
        return false
    } else if n == two {
        return true  // 2 is prime
    } else if n.clone() % &two == Zero::zero() {
        return false  // even number (that's not 2) is not prime
    }

    // The following algorithm is ported from:
    // https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
    // and from:
    // https://inventwithpython.com/hacking/chapter23.html
    let mut s: BigInt = Zero::zero();
    let n_minus_one: BigInt = n.clone() - &one;
    let mut d = n_minus_one.clone();
    while d.clone() % &two == One::one() {
        d /= &two;
        s += &one;
    }

    let s_minus_one = s.clone() - &one;

    // Try k times to test if our number is non-prime:
    for _ in 0..k {
        let a = get_random_bigint(&two, &n_minus_one);
        let mut v = modular_exponentiation(&a, &s, &n);
        if v == one {
            continue
        }
        let mut i: BigInt = zero.clone();
        while v != n_minus_one {
            if i == s_minus_one {
                return false
            }
            i += &one;
            v = (v.clone() * &v) % &n;
        }
    }

    // If we get here, then we have a degree of certainty
    // that n really is a prime number, so return true:
    true
}
```


'''Test code:'''


```rust
fn main() {
    let n = 1234687;
    let result = is_prime(&n);
    println!("Q: Is {} prime?  A: {}", n, result);

    let n = 1234689;
    let result = is_prime(&n);
    println!("Q: Is {} prime?  A: {}", n, result);

    let n = BigInt::parse_bytes("123123423463".as_bytes(), 10).unwrap();
    let result = is_prime(&n);
    println!("Q: Is {} prime?  A: {}", n, result);

    let n = BigInt::parse_bytes("123123423465".as_bytes(), 10).unwrap();
    let result = is_prime(&n);
    println!("Q: Is {} prime?  A: {}", n, result);

    let n = BigInt::parse_bytes("123123423467".as_bytes(), 10).unwrap();
    let result = is_prime(&n);
    println!("Q: Is {} prime?  A: {}", n, result);

    let n = BigInt::parse_bytes("123123423469".as_bytes(), 10).unwrap();
    let result = is_prime(&n);
    println!("Q: Is {} prime?  A: {}", n, result);
}
```

{{out}}

```txt
Q: Is 1234687 prime?  A: true
Q: Is 1234689 prime?  A: false
Q: Is 123123423463 prime?  A: true
Q: Is 123123423465 prime?  A: false
Q: Is 123123423467 prime?  A: false
Q: Is 123123423469 prime?  A: true
```



## Scala

{{libheader|Scala}}
```scala
import scala.math.BigInt

object MillerRabinPrimalityTest extends App {
  val (n, certainty )= (BigInt(args(0)), args(1).toInt)
  println(s"$n is ${if (n.isProbablePrime(certainty)) "probably prime" else "composite"}")
}
```


Direct implementation of algorithm:


```scala

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.util.Random

object MillerRabin {

  implicit def int2Bools(b: Int): Seq[Boolean] = 31 to 0 by -1 map isBitSet(b)

  def isBitSet(byte: Int)(bit: Int): Boolean = ((byte >> bit) & 1) == 1

  def mod(num: Int, denom: Int) = if (num % denom >= 0) num % denom else (num % denom) + denom

  @tailrec
  def isSimple(p: Int, s: Int): Boolean = {
    if (s == 0) {
      true
    }
    else if (witness(Random.nextInt(p - 1), p)) {
      false
    }
    else {
      isSimple(p, s - 1)
    }
  }

  def witness(a: Int, p: Int): Boolean = {
    val b: Seq[Boolean] = p - 1

    b.foldLeft(1)((d, b) => if (mod(d * d, p) == 1 && d != 1 && d != p - 1) {
      return true
    } else {
      b match {
        case true => mod(mod(d*d, p)*a,p)
        case false => mod(d*d, p)
      }
    }) != 1
  }
}
```



## Scheme


```scheme
#!r6rs
(import (rnrs base (6))
        (srfi :27 random-bits))

;; Fast modular exponentiation.
(define (modexpt b e M)
  (cond
    ((zero? e) 1)
    ((even? e) (modexpt (mod (* b b) M) (div e 2) M))
    ((odd? e) (mod (* b (modexpt b (- e 1) M)) M))))

;; Return s, d such that d is odd and 2^s * d = n.
(define (split n)
  (let recur ((s 0) (d n))
    (if (odd? d)
        (values s d)
        (recur (+ s 1) (div d 2)))))

;; Test whether the number a proves that n is composite.
(define (composite-witness? n a)
  (let*-values (((s d) (split (- n 1)))
                ((x) (modexpt a d n)))
    (and (not (= x 1))
         (not (= x (- n 1)))
         (let try ((r (- s 1)))
           (set! x (modexpt x 2 n))
           (or (zero? r)
               (= x 1)
               (and (not (= x (- n 1)))
                    (try (- r 1))))))))

;; Test whether n > 2 is a Miller-Rabin pseudoprime, k trials.
(define (pseudoprime? n k)
  (or (zero? k)
      (let ((a (+ 2 (random-integer (- n 2)))))
        (and (not (composite-witness? n a))
             (pseudoprime? n (- k 1))))))

;; Test whether any integer is prime.
(define (prime? n)
  (and (> n 1)
       (or (= n 2)
           (pseudoprime? n 50))))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func boolean: millerRabin (in bigInteger: n, in integer: k) is func
  result
    var boolean: probablyPrime is TRUE;
  local
    var bigInteger: d is 0_;
    var integer: r is 0;
    var integer: s is 0;
    var bigInteger: a is 0_;
    var bigInteger: x is 0_;
    var integer: tests is 0;
  begin
    if n < 2_ or (n > 2_ and not odd(n)) then
      probablyPrime := FALSE;
    elsif n > 3_ then
      d := pred(n);
      s := lowestSetBit(d);
      d >>:= s;
      while tests < k and probablyPrime do
        a := rand(2_, pred(n));
        x := modPow(a, d, n);
        if x <> 1_ and x <> pred(n) then
          r := 1;
          while r < s and x <> 1_ and x <> pred(n) do
            x := modPow(x, 2_, n);
            incr(r);
          end while;
          probablyPrime := x = pred(n);
        end if;
        incr(tests);
      end while;
    end if;
  end func;

const proc: main is func
  local
    var bigInteger: number is 0_;
  begin
    for number range 2_ to 1000_ do
      if millerRabin(number, 10) then
        writeln(number);
      end if;
    end for;
  end func;
```
Original source: [http://seed7.sourceforge.net/algorith/math.htm#millerRabin]


## Sidef


```ruby
func is_prime(n, k) {

    n == 2 && return true
    n <= 1 && return false
    n  & 1 || return false

    var d = n-1
    var s = valuation(d, 2)
    d >>= s

    k.times {
        var a = irand(2, n-1)
        var x = expmod(a, d, n)
        next if (x ~~ [1, n-1])

        (s-1).times {
            x = expmod(x, 2, n)
            return false if x==1
            break if (x == n-1)
        }
        return false if (x != n-1)
    }

    return true
}

say {|n| is_prime(n, 10) }.grep(^1000).join(', ')
```



## Smalltalk

{{works with|GNU Smalltalk}}
Smalltalk handles big numbers naturally and trasparently (the parent class <tt>Integer</tt> has many subclasses, and <cite>a subclass is picked according to the size</cite> of the integer that must be handled)

```smalltalk
Integer extend [
  millerRabinTest: kl [ |k| k := kl.
    self <= 3
      ifTrue: [ ^true ]
      ifFalse: [
         (self even)
           ifTrue: [ ^false ]
           ifFalse: [ |d s|
              d := self - 1.
              s := 0.
              [ (d rem: 2) == 0 ]
                 whileTrue: [
                   d := d / 2.
                   s := s + 1.
                 ].
              [ k:=k-1. k >= 0 ]
                 whileTrue: [ |a x r|
                    a := Random between: 2 and: (self - 2).
                    x := (a raisedTo: d) rem: self.
                    ( x = 1 )
                      ifFalse: [ |r|
		        r := -1.
                          [ r := r + 1. (r < s) & (x ~= (self - 1)) ]
                          whileTrue: [
                     	    x := (x raisedTo: 2) rem: self
                          ].
                        ( x ~= (self - 1) ) ifTrue: [ ^false ]
                      ]
                 ].
              ^true
           ]
      ]
  ]
].
```


```smalltalk
1 to: 1000 do: [ :n |
   (n millerRabinTest: 10) ifTrue: [ n printNl ]
].
```



## Standard ML


```sml
open LargeInt;

val mr_iterations = Int.toLarge 20;
val rng = Random.rand (557216670, 13504100); (* arbitrary pair to seed RNG *)

fun expmod base 0 m = 1
  | expmod base exp m =
      if exp mod 2 = 0
      then let val rt = expmod base (exp div 2) m;
               val sq = (rt*rt) mod m
           in if sq = 1
                 andalso rt <> 1     (* ignore the two *)
                 andalso rt <> (m-1) (* 'trivial' roots *)
              then 0
              else sq
           end
      else (base*(expmod base (exp-1) m)) mod m;

(* arbitrary precision random number [0,n) *)
fun rand n =
  let val base = Int.toLarge(valOf Int.maxInt)+1;
      fun step r lim =
        if lim < n then step (Int.toLarge(Random.randNat rng) + r*base) (lim*base)
                   else r mod n
  in step 0 1 end;

fun miller_rabin n =
  let fun trial n 0 = true
        | trial n t = let val a = 1+rand(n-1)
                      in (expmod a (n-1) n) = 1
                         andalso trial n (t-1)
                      end
  in trial n mr_iterations end;

fun trylist label lst = (label, ListPair.zip (lst, map miller_rabin lst));

trylist "test the first six Carmichael numbers"
        [561, 1105, 1729, 2465, 2821, 6601];

trylist "test some known primes"
        [7369, 7393, 7411, 27367, 27397, 27407];

(* find ten random 30 digit primes (according to Miller-Rabin) *)
let fun findPrime trials = let val t = trials+1;
                               val n = 2*rand(500000000000000000000000000000)+1
                           in if miller_rabin n
                              then (n,t)
                              else findPrime t end
in List.tabulate (10, fn e => findPrime 0) end;
```

{{out|Sample run}}

```txt

...
val it =
  ("test the first six Carmichael numbers",
   [(561,false),(1105,false),(1729,false),(2465,false),(2821,false),
    (6601,false)]) : string * (int * bool) list
val it =
  ("test some known primes",
   [(7369,true),(7393,true),(7411,true),(27367,true),(27397,true),
    (27407,true)]) : string * (int * bool) list
[autoloading]
[autoloading done]
val it =
  [(505776511533674858497882481471,8),(668742242620107711631417930007,111),
   (831749124005136073184150011961,24),(159858916052323079037919394483,14),
   (810857757001516064878680795563,43),(903375242242638088171051457359,6),
   (506008872035764637556989600477,91),(105574439115200786396150347661,29),
   (349239056313926786302179212509,7),(565349019043144709861293116613,126)]
  : (int * int) list

```



## Swift

{{trans|Python}}
{{libheader|Attaswift BigInt}}


```swift
import BigInt

private let numTrails = 5

func isPrime(_ n: BigInt) -> Bool {
  guard n >= 2 else { fatalError() }
  guard n != 2 else { return true }
  guard n % 2 != 0 else { return false }

  var s = 0
  var d = n - 1

  while true {
    let (quo, rem) = (d / 2, d % 2)

    guard rem != 1 else { break }

    s += 1
    d = quo
  }

  func tryComposite(_ a: BigInt) -> Bool {
    guard a.power(d, modulus: n) != 1 else { return false }

    for i in 0..<s where a.power((2 as BigInt).power(i) * d, modulus: n) == n - 1 {
      return false
    }

    return true
  }

  for _ in 0..<numTrails where tryComposite(BigInt(BigUInt.randomInteger(lessThan: BigUInt(n)))) {
    return false
  }

  return true
}
```



## Tcl

Use Tcl 8.5 for large integer support

```tcl
package require Tcl 8.5

proc miller_rabin {n k} {
    if {$n <= 3} {return true}
    if {$n % 2 == 0} {return false}

    # write n - 1 as 2^s·d with d odd by factoring powers of 2 from n − 1
    set d [expr {$n - 1}]
    set s 0
    while {$d % 2 == 0} {
        set d [expr {$d / 2}]
        incr s
    }

    while {$k > 0} {
        incr k -1
        set a [expr {2 + int(rand()*($n - 4))}]
        set x [expr {($a ** $d) % $n}]
        if {$x == 1 || $x == $n - 1} continue
        for {set r 1} {$r < $s} {incr r} {
            set x [expr {($x ** 2) % $n}]
            if {$x == 1} {return false}
            if {$x == $n - 1} break
        }
	if {$x != $n-1} {return false}
    }
    return true
}

for {set i 1} {$i < 1000} {incr i} {
    if {[miller_rabin $i 10]} {
        puts $i
    }
}
```

{{out}}

```txt
1
2
3
5
7
11
...
977
983
991
997
```



## zkl

Using the Miller-Rabin primality test in GMP:

```zkl
zkl: var BN=Import("zklBigNum");
zkl: BN("4547337172376300111955330758342147474062293202868155909489").probablyPrime()
True
zkl: BN("4547337172376300111955330758342147474062293202868155909393").probablyPrime()
False
```


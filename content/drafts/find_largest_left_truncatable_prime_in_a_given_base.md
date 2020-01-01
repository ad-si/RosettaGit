+++
title = "Find largest left truncatable prime in a given base"
description = ""
date = 2019-10-10T02:52:13Z
aliases = []
[extra]
id = 12308
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

A [[Truncatable primes|truncatable prime]] is one where all non-empty substrings that finish at the end of the number (right-substrings) are also primes ''when understood as numbers in a particular base''. The largest such prime in a given (integer) base is therefore computable, provided the base is larger than 2.

Let's consider what happens in base 10. Obviously the right most digit must be prime, so in base 10 candidates are 2,3,5,7. Putting a digit in the range 1 to base-1 in front of each candidate must result in a prime. So 2 and 5, like the whale and the petunias in ''The Hitchhiker's Guide to the Galaxy'', come into existence only to be extinguished before they have time to realize it, because 2 and 5 preceded by any digit in the range 1 to base-1 is not prime. Some numbers formed by preceding 3 or 7 by a digit in the range 1 to base-1 are prime. So 13,17,23,37,43,47,53,67,73,83,97 are candidates. Again, putting a digit in the range 1 to base-1 in front of each candidate must be a prime. Repeating until there are no larger candidates finds the largest left truncatable prime.

Let's work base 3 by hand:

0 and 1 are not prime so the last digit must be 2. 12<sub>3</sub> = 5<sub>10</sub> which is prime, 22<sub>3</sub> = 8<sub>10</sub> which is not so 12<sub>3</sub> is the only candidate. 112<sub>3</sub> = 14<sub>10</sub> which is not prime, 212<sub>3</sub> = 23<sub>10</sub> which is, so 212<sub>3</sub> is the only candidate. 1212<sub>3</sub> = 50<sub>10</sub> which is not prime, 2212<sub>3</sub> = 77<sub>10</sub> which also is not prime. So there are no more candidates, therefore 23 is the largest left truncatable prime in base 3.

The task is to reconstruct as much, and possibly more, of the table in [https://oeis.org/A103443 the OEIS] as you are able.

Related Tasks:
* [[Miller-Rabin primality test]]





## BBC BASIC

{{works with|BBC BASIC for Windows}}
Uses the '''H'''uge '''I'''nteger '''M'''ath & '''E'''ncryption library from http://devotechs.com/

```bbcbasic
      HIMEM = PAGE + 3000000
      INSTALL @lib$+"HIMELIB"
      PROC_himeinit("HIMEkey")

      DIM old$(20000), new$(20000)
      h1% = 1 : h2% = 2 : h3% = 3 : h4% = 4

      FOR base% = 3 TO 17
        PRINT "Base "; base% " : " FN_largest_left_truncated_prime(base%)
      NEXT
      END

      DEF FN_largest_left_truncated_prime(base%)
      LOCAL digit%, i%, new%, old%, prime%, fast%, slow%
      fast% = 1 : slow% = 50
      old$() = ""
      PROC_hiputdec(1, STR$(base%))
      PROC_hiputdec(2, "1")
      REPEAT
        new% = 0 : new$() = ""
        PROC_hiputdec(3, "0")
        FOR digit% = 1 TO base%-1
          SYS `hi_Add`, ^h2%, ^h3%, ^h3%
          FOR i% = 0 TO old%-1
            PROC_hiputdec(4, old$(i%))
            SYS `hi_Add`, ^h3%, ^h4%, ^h4%
            IF old% OR digit% > 1 THEN
              IF old% > 100 THEN
                SYS `hi_IsPrime_RB`, ^fast%, ^h4% TO prime%
              ELSE
                SYS `hi_IsPrime_RB`, ^slow%, ^h4% TO prime%
              ENDIF
              IF prime% THEN new$(new%) = FN_higetdec(4) : new% += 1
            ENDIF
          NEXT
        NEXT
        SYS `hi_Mul`, ^h1%, ^h2%, ^h2%
        SWAP old$(), new$()
        SWAP old%, new%
      UNTIL old% = 0
      = new$(new%-1)
```

'''Output:'''

```txt

Base 3 : 23
Base 4 : 4091
Base 5 : 7817
Base 6 : 4836525320399
Base 7 : 817337
Base 8 : 14005650767869
Base 9 : 1676456897
Base 10 : 357686312646216567629137
Base 11 : 2276005673
Base 12 : 13092430647736190817303130065827539
Base 13 : 812751503
Base 14 : 615419590422100474355767356763
Base 15 : 34068645705927662447286191
Base 16 : 1088303707153521644968345559987
Base 17 : 13563641583101

```



## C


```c
#include <stdio.h>
#include <gmp.h>

typedef unsigned long ulong;

ulong small_primes[] = {2,3,5,7,11,13,17,19,23,29,31,37,41,
			43,47,53,59,61,67,71,73,79,83,89,97};

#define MAX_STACK 128
mpz_t tens[MAX_STACK], value[MAX_STACK], answer;

ulong base, seen_depth;

void add_digit(ulong i)
{
	ulong d;
	for (d = 1; d < base; d++) {
		mpz_set(value[i], value[i-1]);
		mpz_addmul_ui(value[i], tens[i], d);
		if (!mpz_probab_prime_p(value[i], 1)) continue;

		if (i > seen_depth ||
			(i == seen_depth && mpz_cmp(value[i], answer) == 1))
		{
			if (!mpz_probab_prime_p(value[i], 50)) continue;

			mpz_set(answer, value[i]);
			seen_depth = i;
			gmp_fprintf(stderr, "\tb=%lu d=%2lu | %Zd\n", base, i, answer);
		}

		add_digit(i+1);
	}
}

void do_base()
{
	ulong i;
	mpz_set_ui(answer, 0);
	mpz_set_ui(tens[0], 1);
	for (i = 1; i < MAX_STACK; i++)
		mpz_mul_ui(tens[i], tens[i-1], base);

	for (seen_depth = i = 0; small_primes[i] < base; i++) {
		fprintf(stderr, "\tb=%lu digit %lu\n", base, small_primes[i]);
		mpz_set_ui(value[0], small_primes[i]);
		add_digit(1);
	}
	gmp_printf("%d: %Zd\n", base, answer);
}

int main(void)
{
	ulong i;
	for (i = 0; i < MAX_STACK; i++) {
		mpz_init_set_ui(tens[i], 0);
		mpz_init_set_ui(value[i], 0);
	}
	mpz_init_set_ui(answer, 0);

	for (base = 22; base < 30; base++) do_base();

	return 0;
}
```

{{out}}

```txt

$ ./a.out 2&>/dev/null
22: 33389741556593821170176571348673618833349516314271
23: 116516557991412919458949
24: 10594160686143126162708955915379656211582267119948391137176997290182218433
25: 8211352191239976819943978913
26: 12399758424125504545829668298375903748028704243943878467
27: 10681632250257028944950166363832301357693
28: 720639908748666454129676051084863753107043032053999738835994276213
29: 4300289072819254369986567661
...

```


## C#


```c#
using Mpir.NET;  // 0.4.0
using System;   // 4790@3.6
using System.Collections.Generic;
class MaxLftTrP_B
{
    static void Main()
    {
        mpz_t p; var sw = System.Diagnostics.Stopwatch.StartNew(); L(3);
        for (uint b = 3; b < 13; b++)
        {
            sw.Restart(); p = L(b);
            Console.WriteLine("{0} {1,2} {2}", sw.Elapsed, b, p);
        }
        Console.Read();
    }

    static mpz_t L(uint b)
    {
        var p = new List<mpz_t>(); mpz_t np = 0;
        while ((np = nxtP(np)) < b) p.Add(np);
        int i0 = 0, i = 0, i1 = p.Count - 1; mpz_t n0 = b, n, n1 = b * (b - 1);
        for (; i < p.Count; n0 *= b, n1 *= b, i0 = i1 + 1, i1 = p.Count - 1)
            for (n = n0; n <= n1; n += n0)
                for (i = i0; i <= i1; i++)
                    if (mpir.mpz_probab_prime_p(np = n + p[i], 15) > 0) p.Add(np);
        return p[p.Count - 1];
    }

    static mpz_t nxtP(mpz_t n) { mpz_t p = 0; mpir.mpz_nextprime(p, n); return p; }
}
```

{{out}}

```txt

00:00:00.0000082   3  23
00:00:00.0000267   4  4091
00:00:00.0000299   5  7817
00:00:00.0027235   6  4836525320399
00:00:00.0000533   7  817337
00:00:00.0026306   8  14005650767869
00:00:00.0004923   9  1676456897
00:00:00.0514316  10  357686312646216567629137
00:00:00.0003609  11  2276005673
00:00:03.3792076  12  13092430647736190817303130065827539

```



## Eiffel

As there is currently no implementation for arbitrary precision integers this example only works for base 3 to base 9. Respectively for bases where the Result fits into a INTEGER_64.

```Eiffel

class
	LARGEST_LEFT_TRUNCABLE_PRIME

create
	make

feature

	make
			-- Tests find_prime for different bases.
		local
			i: INTEGER
			decimal: INTEGER_64
		do
			from
				i := 3
			until
				i = 10
			loop
				largest := 0
				find_prime ("", i)
				decimal := convert_to_decimal (largest, i)
				io.put_string (i.out + ":%T" + decimal.out)
				io.new_line
				i := i + 1
			end
		end

	find_prime (right_part: STRING; base: INTEGER)
			-- Largest left truncable prime for a given 'base'.
		local
			i, larger, larger_dec: INTEGER_64
			right: STRING
			prime: BOOLEAN
		do
			from
				i := 1
			until
				i = base
			loop
				create right.make_empty
				right.deep_copy (right_part)
				right.prepend (i.out)
				larger := right.to_integer_64
				if base /= 10 then
					larger_dec := convert_to_decimal (larger, base)
					if larger_dec < 0 then
						io.put_string ("overflow")
						prime := False
					else
						prime := is_prime (larger_dec)
					end
				else
					prime := is_prime (larger)
				end
				if prime = TRUE then
					find_prime (larger.out, base)
				else
					if right_part.count > 0 and right_part.to_integer_64 > largest then
						largest := right_part.to_integer_64
					end
				end
				i := i + 1
			end
		end

	largest: INTEGER_64

	convert_to_decimal (given, base: INTEGER_64): INTEGER_64
			-- 'given' converted to base ten.
		require
		local
			n, i: INTEGER
			st_digits: STRING
			dec: REAL_64
		do
			n := given.out.count
			dec := 0
			st_digits := given.out
			from
				i := 1
			until
				n < 0 or i > given.out.count
			loop
				n := n - 1
				dec := dec + st_digits.at (i).out.to_integer * base ^ n
				i := i + 1
			end
			Result := dec.truncated_to_integer_64
		end

	is_prime (n: INTEGER_64): BOOLEAN
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

{{out}}

```txt

3:        23
4:        4091
5:        7817
6:        4836525320399
7:        817337
8:        14005650767869
9:        1676456897

```


=={{header|F_Sharp|F#}}==

```fsharp

(* Find some probable candidates for The Largest Left Trucatable Prime in a given base
   Nigel Galloway: April 25th., 2017 *)
let snF Fbase pZ =
  let rec fn i g (e:bigint) l =
    match e with
    | _ when e.IsZero  -> i=1I
    | _ when e.IsEven  -> fn i ((g*g)%l) (e/2I) l
    | _                -> fn ((i*g)%l) ((g*g)%l) (e/2I) l
  let rec fi n i =
    let g = n|>Array.Parallel.collect(fun n->[|for g in 1I..(Fbase-1I) do yield g*i+n|])|>Array.filter(fun n->fn 1I 2I (n-1I) n)
    if (Array.isEmpty g) then n else (fi g (i*Fbase))
  pZ |> Array.Parallel.map (fun n -> fi [|n|] Fbase)|>Seq.concat|>Seq.max

```

{{out}}

```txt

> printfn "%A" (snF  3I [2I]);;                            -> 23
> printfn "%A" (snF  4I [2I;3I]);;                         -> 4091
> printfn "%A" (snF  5I [2I;3I]);;                         -> 7817
> printfn "%A" (snF  6I [2I;3I;5I]);;                      -> 4836525320399
> printfn "%A" (snF  7I [2I;3I;5I]);;                      -> 817337
> printfn "%A" (snF  8I [2I;3I;5I;7I]);;                   -> 14005650767869
> printfn "%A" (snF  9I [2I;3I;5I;7I]);;                   -> 1676456897
> printfn "%A" (snF 10I [2I;3I;5I;7I]);;                   -> 357686312646216567629137
> printfn "%A" (snF 11I [2I;3I;5I;7I]);;                   -> 2276005673
> printfn "%A" (snF 12I [2I;3I;5I;7I;11I]);;               -> 13092430647736190817303130065827539
Real: 00:00:43.776, CPU: 00:03:43.106, GC gen0: 4381, gen1: 3
> printfn "%A" (snF 13I [2I;3I;5I;7I;11I]);;               -> 812751503
> printfn "%A" (snF 14I [2I;3I;5I;7I;11I;13I]);;           -> 615419590422100474355767356763
> printfn "%A" (snF 15I [2I;3I;5I;7I;11I;13I]);;           -> 34068645705927662447286191
> printfn "%A" (snF 16I [2I;3I;5I;7I;11I;13I]);;           -> 1088303707153521644968345559987
> printfn "%A" (snF 17I [2I;3I;5I;7I;11I;13I]);;           -> 13563641583101
> printfn "%A" (snF 18I [|2I;3I;5I;7I;11I;13I;17I|]);;     -> 571933398724668544269594979167602382822769202133808087
Real: 04:50:58.748, CPU: 14:55:48.221, GC gen0: 1180413, gen1: 62
> printfn "%A" (snF 19I [|2I;3I;5I;7I;11I;13I;17I|]);;     -> 546207129080421139
> printfn "%A" (snF 20I [|2I;3I;5I;7I;11I;13I;17I;19I|]);; -> 1073289911449776273800623217566610940096241078373
Real: 00:38:37.354, CPU: 02:42:24.086, GC gen0: 237504, gen1: 30
> printfn "%A" (snF 21I [|2I;3I;5I;7I;11I;13I;17I;19I|]);; -> 391461911766647707547123429659688417
> printfn "%A" (snF 22I [|2I;3I;5I;7I;11I;13I;17I;19I|]);; -> 33389741556593821170176571348673618833349516314271
Real: 00:22:22.206, CPU: 01:34:02.565, GC gen0: 138489, gen1: 24
> printfn "%A" (snF 23I [|2I;3I;5I;7I;11I;13I;17I;19I|]);; -> 116516557991412919458949

```



## Fortran

The initial idea is to see how far 32-bit integers will suffice, to try out the logic for the search. The basic idea is to maintain a "horde" of digit sequences that represent a prime number, then for each survivor in the horde, try adding a possible digit at the high-order end and checking that the resulting number is a prime. If so, add this sequence to the horde. When all trials have been made, if there was an addition, purge the earlier entries, and have another go, which is the next level up. If no addition had been made then the sequence is ended and the largest value amongst the survivors is printed.

Fortran does not offer a "list" data structure, so as ever, fiddling with arrays is needed. The situation at the end of a level is that there are entries 1:LH, the "starters" for that level, and following that are entries LH + 1:NH, the added entries. The "starters" are no longer needed and to save on storage, this hole is to be filled. The entire horde could be shifted down LH slots, but there could be many of them. Instead, the tail end entries are copied from the end into the hole. There are of course many variations possible, such as using linked-lists with an "available entry" list so that only links need be messed with rather than copying content, etc.

The source file uses the F90 style, mainly because module PRIMEBAG (from [[Extensible_prime_generator#Fortran]]) is available to supply some prime numbers and check whether a number is prime or not. This works up to the 32-bit integer limit: although INTEGER*8 variables are available, that seemed a reasonable stopping point.  Otherwise, the source is older-style, except for a few conveniences: the use of "CYCLE" rather than a "GO TO", some array assignments rather than explicit DO-loops, and the special function MAXLOC to locate the index of the maximum value in an array. Although F90 also allows arrays of compound data, the entries are stored via a two-dimensional array, and to keep related digits adjacent in storage the indexing is (digit,entry) rather than (entry,digit) since fortran uses that ordering.

Unfortunately, the modernisers have abandoned a feature of First Fortran (1957): the <code>IF OVERFLOW ... </code> statement, or similar. In its place are ad-hoc tests on whether a number has suddenly become zero or negative: there is roughly a 50:50 chance that an overflow in two's-complement integer arithmetic will produce such a result - if positive, the value will still be wrong after an overflow. Such checks are tedious, but not bothering to check will mean that odd behaviour will ensue, and worse, incorrect results.
```Fortran
      USE PRIMEBAG	!Gain access to NEXTPRIME and ISPRIME.
Calculates the largest "left-truncatable" digit sequence that is a prime number, in various bases.
      INTEGER LBASE,MANY,ENUFF	!Some sizes.
      PARAMETER (LBASE = 13, MANY = 66666, ENUFF = 66)
      INTEGER NS,START(LBASE)	!A list of single-digit prime numbers for starters.
      INTEGER NH,LH		!Counters for the horde.
      INTEGER N,HORDEN(MANY)		!Numerical value of a digit sequence.
      INTEGER*1 HORDED(ENUFF,MANY)	!Single-digit values only.
      INTEGER B,D,DB	!The base, a digit, some power of the base.
      INTEGER L		!The length of the digit sequence: DB = B**L.
      INTEGER P		!Possibly a prime number.
      INTEGER I		!A stepper.

      MSG = 6	!I/O unit number for "standard output".
      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file!"	!Attempt in hope.
      NS = 0	!No starters.
      P = 1	!Start looking for some primes.
    1 P = NEXTPRIME(P)	!Thus skipping non-primes.
      IF (P.LE.LBASE) THEN	!Beyond the limit?
        NS = NS + 1		!No. Count another starter.
        START(NS) = P		!Save its value.
        GO TO 1			!And seek further.
      END IF		!One could insted prepare some values, the primes being well-known.
      WRITE (MSG,2) LBASE,NS,START(1:NS)	!But, parameterisation is easy enough.
    2 FORMAT ("Working in bases 3 to ",I0," there are ",I0,	!Announce the known.
     * " single-digit primes: ",666(I0:", "))	!The : sez stop if the list is exhausted.
      WRITE (MSG,3)		!Produce a heading for the tabular output.
    3 FORMAT (/"Base Digits Count Max. Value = (in base)")

   10 DO B = 3,LBASE	!Work through the bases.
        NH = 0			!The horde is empty.
        DO I = 1,NS		!Prepare the starters for base B.
          IF (START(I).GE.B) EXIT	!Like, they're single-digits in base B.
          NH = NH + 1			!So, count another in.
          HORDEN(NH) = START(I)		!Its numerical value.
          HORDED(1,NH) = START(I)	!Its digits. Just one.
        END DO			!On to the next single-digit prime number.
        L = 0	!Syncopation. The length of the digit sequences.
        DB = 1	!The power for the incoming digit.

   20   L = L + 1	!We're about to add another digit.
        IF (L.GE.ENUFF) STOP "Too many digits!"	!Hopefully, there's room.
        DB = DB*B	!The new power of B.
        IF (DB.LE.0) GO TO 29	!Integer overflow?
        LH = NH		!The live ones, awaiting extension.
        DO I = 1,LH	!Step through each starter.
          N = HORDEN(I)	!Grab its numerical value.
          DO D = 1,B - 1	!Consider all possible lead digits.
            P = D*DB + N		!Place it at the start of the number.
            IF (P.LE.0) GO TO 29	!Oh for IF OVERFLOW ...
            IF (ISPRIME(P)) THEN	!And if it is a prime,
              IF (NH.GE.MANY) STOP "Too many sequences!"	!Add a sequence.
              NH = NH + 1			!Count in a survivor.
              HORDEN(NH) = P			!The numerical value.
              HORDED(1:L,NH) = HORDED(1:L,I)	!The digits.
              HORDED(L + 1,NH) = D		!Plus the added high-order digit.
            END IF			!So much for persistent primality.
          END DO		!On to the next lead digit.
        END DO	!On to the next starter.

        N = NH - LH		!The number of entries added to the horde.
        IF (N.GT.0) THEN	!Were there any?
          DO I = 1,MIN(LH,N)		!Yes. Overwrite the starters.
            HORDEN(I) = HORDEN(NH)		!From the tail end of the horde.
            HORDED(1:L + 1,I) = HORDED(1:L + 1,NH)	!Digit sequences as well.
            NH = NH - 1				!One snipped off.
          END DO			!Thus fill the gap at the start.
          NH = N			!The new horde count.
          LH = NH			!All are starters for the next level.
          GO TO 20			!See how it goes.
        END IF			!So much for further progress.
        GO TO 30		!But if none, done.
   29   WRITE (MSG,28) B,L,NH,DB,P	!Curses. Offer some details.
   28   FORMAT (I4,I7,I6,28X,"Integer overflow!",2I12)
        CYCLE			!Or, GO TO the end of the loop.

   30   I = MAXLOC(HORDEN(1:NH),DIM = 1)	!Finger the mostest number.
        WRITE (MSG,31) B,L,NH,HORDEN(I),HORDED(L:1:-1,I)	!Results!
   31   FORMAT (I4,I7,I6,I11," = "666(I0:"."))	!See Format 3.

      END DO		!On to the next base.
      END	!Simple enough.
```


Results:

```txt

Working in bases 3 to 13 there are 6 single-digit primes: 2, 3, 5, 7, 11, 13

Base Digits Count Max. Value = (in base)
   3      3     1         23 = 2.1.2
   4      6     3       4091 = 3.3.3.3.2.3
   5      6     1       7817 = 2.2.2.2.3.2
   6     11    42                            Integer overflow!   362797056 -2138904587
   7      7     1     817337 = 6.6.4.2.6.2.3
   8     10    27                            Integer overflow!  1073741824 -1763182509
   9      9     5                            Integer overflow!   387420489 -1971761312
  10      9   546                            Integer overflow!  1000000000 -1299575929
  11      8     2                            Integer overflow!   214358881 -2107742185
  12      8  7712                            Integer overflow!   429981696 -1718612639
  13      8     4                            Integer overflow!   815730721 -1993454625

```


So, there being no type declarations such as INTEGER*600, multi-precision arithmetic is needed to go further. There is no universally-used library for this, but thanks to previous effort in [[Sequence_of_primorial_primes#Fortran]] a collection is available, another F90 "module". This however works with a fixed value of BIGBASE, which is expected to be a large number and a power of ten. While there would be no great difficulty in converting from the digit sequences in the current base into a BIGNUM in base BIGBASE, it is more interesting to work with the desired base so that the digit sequences are manipulated directly. Accordingly, a variation, with the module starting
```Fortran
      MODULE BIGNUMBERVB	!Limited services: integers, no negative numbers, variable base possible.
       INTEGER BIGORDER		!A limited attempt at generality.
       PARAMETER (BIGORDER = 1)	!This is the order of the base of the big number arithmetic.
       INTEGER BIGBASE,BIGLIMIT	!Sized thusly.
c       PARAMETER (BIGBASE = 10**BIGORDER, BIGLIMIT = 8888/BIGORDER)	!Enough?
       PARAMETER (BIGLIMIT = 666)
       TYPE BIGNUM	!So, a big number is simple.
        INTEGER LAST		!This many digits (of size BIGBASE) are in use.
        INTEGER DIGIT(BIGLIMIT)	!The digits, in ascending power order.
       END TYPE BIGNUM	!So much for that.

```


As checked via earlier tests, using a fixed value for BIGLIMIT that is "surely big enough" enables faster execution than variable sizes. Now, BIGBASE is a variable, with a view to <code>DO BIGBASE = 3,17</code> and almost everything else remains the same, though with BIGBASE being a rather small number, there is no need to employ 64-bit variables via INTEGER*8 at certain places. The use of BIGORDER is disrupted and routines employing it should be avoided or adjusted, thus in BIGTASTE, adding
```Fortran
          IF (MOD(BIGBASE,10).NE.0) STOP "BIGTASTE expects powers of 10"	!Alas. Otherwise the "E" formalism fails.
```
 for example. The changes produce
```Fortran
        SUBROUTINE BIGWRITE(F,B)	!Show B.
         INTEGER F	!I/O unit number.
         TYPE(BIGNUM) B	!The number.
          WRITE (F,1,ADVANCE="NO") B.DIGIT(B.LAST:1:-1)	!Roll the digits in base BIGBASE.
    1     FORMAT (666(I0:"."))		!Not bothering with using letters for digits above nine.
        END SUBROUTINE BIGWRITE		!Simple, but messy.

        SUBROUTINE BIGTEN(B,TEXT)	!Produce a base ten digit string.
         TYPE(BIGNUM) B		!The number.
         CHARACTER*(*) TEXT	!The digits.
         TYPE(BIGNUM) N		!A copy I can mess with.
         INTEGER L,D		!Assistants.
          N.LAST = B.LAST	!So, make my copy.
          N.DIGIT(1:N.LAST) = B.DIGIT(1:B.LAST)	!Only the live digits are wanted.
          TEXT = ""		!Clear for action.
          L = LEN(TEXT)		!Find the far end.
   10     D = BIGDIVRN(N,10)	!Digits emerge from the low-order end of the number.
          TEXT(L:L) = CHAR(ICHAR("0") + D)	!Convert a digit to text, usual assumptions.
          IF (N.LAST.EQ.1 .AND. N.DIGIT(1).EQ.0) RETURN	!If zero, N is finished.
          L = L - 1		!Otherwise, another digits will emerge.
          IF (L.GT.0) GO TO 10	!If there is space, go for it.
          TEXT(1:1) = "!"	!Otherwise, signify overflow.
        END SUBROUTINE BIGTEN	!No negatives, so no sign is needed.

        LOGICAL FUNCTION BIGISPRIME(B)	!Ad-hoc report.
         TYPE(BIGNUM) B	!The number.
          BIGISPRIME = ABS(BIGFACTOR(B,2800)).EQ.1	!Condensed report.
        END FUNCTION BIGISPRIME	!Can't be bothered with ISPRIME from PRIMEBAG.

```

Which is to say that BIGWRITE will show the digits of a number as decimal numbers separated by periods rather than involving letters as additional digit symbols, while BIGTEN will prepare a text version in base ten, whatever BIGBASE is. Finally, BIGMRPRIME used to quit if BIGBASE were less than four, because it wants to test numbers not exceeding four by only inspecting a single digit of the big number, so that it can for larger numbers perform a direct test for divisibility by two and three without rejecting those numbers as primes just in case it is invoked for them. So ...
```Fortran
Catch some annoying cases, to protect the direct tests for divisibility by two and three...
          IF (N.LAST.LE.2) THEN	!A smallish number? I want to compare to four, but BIGBASE might be two.
            NR = BIGVALUE(N) 		!Surely so.
            IF (NR.LE.4) THEN		!Some special values are known.
              BIGMRPRIME = NR.GE.2 .AND. NR.LE.3	!Like, the neighbours.
              RETURN		!Thus allow 2 to be reported as prime.
            END IF		!Yet, test for 2 as a possible factor for larger numbers.
          END IF		!Without struggling over SQRT and suchlike.
          BIGMRPRIME = .FALSE.	!Most numbers are not primes.
          IF (BIGMOD2(N).EQ.0) RETURN	!A single expression using .OR. risks always evaluating BOTH parts, damnit,
          IF (BIGMODN(N,3).EQ.0) RETURN	!Even for even numbers. Possibly doing so "in parallel" is no consolation.

```

With all this in hand, the job can be done by
```Fortran
      USE PRIMEBAG	!Gain access to NEXTPRIME and ISPRIME.
      USE BIGNUMBERVB	!Alas, INTEGER*600 is not available.
Calculates the largest "left-truncatable" digit sequence that is a prime number, in various bases.
      INTEGER LBASE,MANY	!Some sizes.
      PARAMETER (LBASE = 17, MANY = 66666)
      INTEGER NS,START(LBASE)	!A list of single-digit prime numbers for starters.
      TYPE(BIGNUM) HORDE(MANY)	!A collection.
      INTEGER N,NH,LH		!Counters for the horde.
      INTEGER L		!The length of the digit sequence.
      INTEGER I,D	!Steppers.
      CHARACTER*42 TEXT	!A scratchpad, for decimal values.
      REAL T0,T1	!In memory of lost time.

      MSG = 6	!I/O unit number for "standard output".
      IF (.NOT.GRASPPRIMEBAG(66)) STOP "Gan't grab my file!"	!Attempt in hope.
      NS = 0	!No starters.
      N = 1	!Start looking for some primes.
    1 N = NEXTPRIME(N)	!Thus skipping non-primes.
      IF (N.LE.LBASE) THEN	!Beyond the limit?
        NS = NS + 1		!No. Count another starter.
        START(NS) = N		!Save its value.
        GO TO 1			!And seek further.
      END IF		!One could insted prepare some values, the primes being well-known.
      WRITE (MSG,2) LBASE,NS,START(1:NS)	!But, parameterisation is easy enough.
    2 FORMAT ("Working in bases 3 to ",I0," there are ",I0,	!Announce the known.
     * " single-digit primes: ",666(I0:", "))	!The : sez stop if the list is exhausted.
      WRITE (MSG,3)		!Produce a heading for the tabular output.
    3 FORMAT (/"Base Digits Count",29X," Maximum Value = (in base)")	!See Format 31.

Chug through the various bases to be used for the numerology.
      CALL CPU_TIME(T0)	!Start the timing.
   10 DO BIGBASE = 3,LBASE	!Not really very BIG bases.
        NH = 0			!The horde is empty.
        DO I = 1,NS		!Prepare the starters for base BIGBASE.
          IF (START(I).GE.BIGBASE) EXIT	!Like, they're single-digits in base BIGBASE which may exceed ten...
          NH = NH + 1			!So, count another in.
          HORDE(NH).DIGIT(1) = START(I)		!Its numerical value.
          HORDE(NH).LAST = 1			!Its digit count. Just one.
        END DO			!On to the next single-digit prime number in BIGBASE.
        L = 1		!The numbers all have one digit.
Consider each starter for extension via another high-order digit, to be placed at DIGIT(L + 1).
   20   L = L + 1	!We're about to add another digit, now at DIGIT(L).
        IF (L.GT.BIGLIMIT) STOP "Too many digits!"	!Hopefully, there's room.
        HORDE(1:NH).LAST = L	!There is. Advise the BIGNUM horde of this.
        LH = NH		!The live ones, awaiting extension.
        DO I = 1,LH	!Step through each starter.
          DO D = 1,BIGBASE - 1	!Consider all possible lead digits.
            HORDE(I).DIGIT(L) = D	!Place it at the start of the number.
            IF (BIGISPRIME(HORDE(I))) THEN	!And if it is a prime, or seems likely to be ...
              IF (NH.GE.MANY) STOP "Too many sequences!"	!Add a sequence.
              NH = NH + 1			!Count in a survivor.
              HORDE(NH).LAST = L		!Its digit count.
              HORDE(NH).DIGIT(1:L) = HORDE(I).DIGIT(1:L)	!Its digits.
            END IF			!So much for persistent primality.
          END DO		!On to the next lead digit.
        END DO		!On to the next starter.
Check for added entries and compact the collection if there are some.
        N = NH - LH		!The number of entries added to the horde.
        IF (N.GT.0) THEN	!Were there any?
          DO I = 1,MIN(LH,N)		!Yes. Overwrite the starters.
            HORDE(I).LAST = HORDE(NH).LAST	!From the tail end of the horde.
            HORDE(I).DIGIT(1:L) = HORDE(NH).DIGIT(1:L)	!Copying only the live digits.
            NH = NH - 1				!One snipped off.
          END DO			!Thus fill the gap at the start.
          NH = N			!The new horde count.
          GO TO 20			!See how it goes.
        END IF			!So much for further progress.
Cast forth the mostest of the starters.
   30   HORDE(1:NH).LAST = L - 1	!The testing involved an extra digit, which was not accepted.
        L = 1		!Now go looking for the mostest of the survivors.
        DO I = 2,NH	!By comparing all the rest.
          IF (BIGSIGN(HORDE(L),HORDE(I)).LT.0) L = I	!Consider A - B.
        END DO		!On to the next.
        CALL BIGTEN(HORDE(L),TEXT)	!Get a decimal digit string.
        WRITE (MSG,31) BIGBASE,HORDE(L).LAST,NH,TEXT	!Some auxiliary details.
   31   FORMAT (I4,I7,I6,1X,A," = ",$)			!See Format 3.
        CALL BIGWRITE(MSG,HORDE(L))		!The number at last!
        WRITE (MSG,*)				!Finish the line.
      END DO		!On to the next base.
      CALL CPU_TIME(T1)	!Completed the run.

Closedown.
  200 WRITE (MSG,201)	!First, some statistics.
  201 FORMAT (/,"The MR prime test makes a series of trials, "
     1 "stopping early",/'only when a "definitely composite" ',
     2 "result is encountered.")
      WRITE (MSG,202) "Trial",(I,I = 1,BIGMRTRIALS)	!Roll the trial number.
      WRITE (MSG,202) "Count",BIGMRCOUNT		!Now the counts.
  202 FORMAT (A6,": ",666I8)	!This should do.
      WRITE (MSG,*) "CPU time:",T1 - T0	!The cost.
      END	!Simple enough.

```


And the results, slightly edited to remove six columns of spaces...

```txt

Working in bases 3 to 17 there are 7 single-digit primes: 2, 3, 5, 7, 11, 13, 17

Base Digits Count                        Maximum Value = (in base)
   3      3     1                                   23 = 2.1.2
   4      6     3                                 4091 = 3.3.3.3.2.3
   5      6     1                                 7817 = 2.2.2.2.3.2
   6     17     1                        4836525320399 = 1.4.1.4.1.5.1.1.4.1.4.4.5.1.4.3.5
   7      7     1                               817337 = 6.6.4.2.6.2.3
   8     15     1                       14005650767869 = 3.1.3.6.3.6.1.6.5.5.3.7.7.7.5
   9     10     3                           1676456897 = 4.2.8.4.4.8.4.4.6.5
  10     24     1             357686312646216567629137 = 3.5.7.6.8.6.3.1.2.6.4.6.2.1.6.5.6.7.6.2.9.1.3.7
  11      9     1                           2276005673 = 10.6.8.8.2.2.8.2.7
  12     32     1  13092430647736190817303130065827539 = 4.7.1.10.3.4.10.1.6.4.2.5.9.11.10.1.6.11.3.2.4.10.11.8.10.3.2.11.7.8.1.7
  13      8     4                            812751503 = 12.12.4.12.8.12.6.5
  14     26     2       615419590422100474355767356763 = 13.9.6.7.12.12.13.6.3.3.8.8.5.2.2.6.1.9.8.8.3.10.7.13.2.3
  15     22     1           34068645705927662447286191 = 6.12.6.12.2.12.14.2.12.14.14.14.10.4.8.2.6.14.6.4.2.11
  16     25     1      1088303707153521644968345559987 = 13.11.12.7.15.11.10.2.4.15.14.6.10.14.12.4.6.2.10.11.15.6.3.11.3
  17     11     1                       13563641583101 = 6.12.6.6.12.12.4.12.12.8.3

The MR prime test makes a series of trials, stopping early
only when a "definitely composite" result is encountered.
 Trial:        1       2       3       4       5       6
 Count:   517641  235380  235380  235380  235380  235380
 CPU time:   599.2188

```


So, once again, it is plain that using a large BIGBASE is beneficial. The plain number version first given works in the computer's own arithmetic base, and preparing such values from the digit strings in the given base is not difficult. Despite the inconvenience of messing with digit sequences not in the same base as used for calculation, a trial run using base 10000 required 260 seconds instead - and gave the same numbers. Bignumber arithmetic via assembler to fully exploit the available hardware would surely do better still.

Going further will require MANY to be enlarged. Already, base twelve required just over nineteen thousand entries, and base eighteen overflowed MANY = 66666. This suggests that a lot of data is being shifted about, so some sort of linked-list scheme might reduce that. Incidentally, in <code>B.LAST = A.LAST; B.DIGIT(1:N) = A.DIGIT(1:N)</code> and similar, because the storage for .DIGIT immediately follows that for .LAST, one might hope that an advanced compiler would combine the two statements into one sequential copy... Alas, the Compaq F90/95 compiler produces two wads of code, of 20 operations and then 92. Bounds checking is active, but still...

And, as one who can recall when one was admitted to the status of "prime" (like, being divisible only by itself and one), what about allowing numbers to end with the digit one...

```txt

Working in bases 3 to 17

Base Digits Count                        Maximum Value = (in base)
   3      2     1                                    7 = 2.1
   4      7     1                                 9829 = 2.1.2.1.2.1.1
   5      4     1                                  311 = 2.2.2.1
   6     19     1                      580639005096133 = 5.4.1.4.5.2.5.5.1.3.1.5.5.1.3.1.4.2.1
   7      8     1                              3602999 = 4.2.4.2.4.2.4.1
   8      9     1                            104056657 = 6.1.4.7.4.3.5.2.1
   9      5     3                                41023 = 6.2.2.4.1
  10     20     1                 89726156799336363541 = 8.9.7.2.6.1.5.6.7.9.9.3.3.6.3.6.3.5.4.1
  11      7     1                             11750399 = 6.6.10.6.2.8.1
  12     30     1     57434208867139354150297607357437 = 2.10.10.1.1.11.6.4.4.7.11.8.2.2.8.10.8.7.6.5.9.5.2.2.10.2.1.5.9.1
  13     10     1                          66073331221 = 6.2.12.12.10.8.12.10.12.1
  14     25     1        39607537776359469390989456509 = 12.4.7.3.5.12.3.11.6.3.11.5.11.9.13.9.3.7.10.12.6.4.3.8.1
  15     20     4             319674182915416424428051 = 14.6.4.8.6.8.12.6.2.4.10.8.6.4.8.14.14.14.10.1
  16     23     2         2208955789035921681292672241 = 7.2.3.3.4.9.5.4.5.1.5.7.8.15.9.3.3.4.3.3.12.15.1
  17     11     2                       16408729108033 = 8.2.6.4.4.6.14.2.12.6.1

The MR prime test makes a series of trials, stopping early
only when a "definitely composite" result is encountered.
 Trial:        1       2       3       4       5       6
 Count:    97667   44905   44904   44904   44904   44904
 CPU time:   111.2656

```



## Go

{{trans|C}}


Note that the use of ProbablyPrime(0) requires Go 1.8 or later.

```go
package main

import (
    "fmt"
    "math/big"
)

var smallPrimes = [...]int{2, 3, 5, 7, 11, 13, 17, 19, 23, 29}

const maxStack = 128

var (
    tens, values    [maxStack]big.Int
    bigTemp, answer = new(big.Int), new(big.Int)
    base, seenDepth int
)

func addDigit(i int) {
    for d := 1; d < base; d++ {
        values[i].Set(&values[i-1])
        bigTemp.SetUint64(uint64(d))
        bigTemp.Mul(bigTemp, &tens[i])
        values[i].Add(&values[i], bigTemp)
        if !values[i].ProbablyPrime(0) {
            continue
        }
        if i > seenDepth || (i == seenDepth && values[i].Cmp(answer) == 1) {
            if !values[i].ProbablyPrime(0) {
                continue
            }
            answer.Set(&values[i])
            seenDepth = i
        }
        addDigit(i + 1)
    }
}

func doBase() {
    answer.SetUint64(0)
    tens[0].SetUint64(1)
    bigTemp.SetUint64(uint64(base))
    seenDepth = 0
    for i := 1; i < maxStack; i++ {
        tens[i].Mul(&tens[i-1], bigTemp)
    }
    for i := 0; smallPrimes[i] < base; i++ {
        values[0].SetUint64(uint64(smallPrimes[i]))
        addDigit(1)
    }
    fmt.Printf("%2d: %s\n", base, answer.String())
}

func main() {
    for base = 3; base <= 17; base++ {
        doBase()
    }
}
```


{{out}}

```txt

 3: 23
 4: 4091
 5: 7817
 6: 4836525320399
 7: 817337
 8: 14005650767869
 9: 1676456897
10: 357686312646216567629137
11: 2276005673
12: 13092430647736190817303130065827539
13: 812751503
14: 615419590422100474355767356763
15: 34068645705927662447286191
16: 1088303707153521644968345559987
17: 13563641583101

```



## Haskell

Miller-Rabin test code from [http://www.haskell.org/haskellwiki/Testing_primality#Miller-Rabin_Primality_Test HaskellWiki], with modifications.

```haskell
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (Int,a)
find2km n = f 0 n
	where f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2

-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a >= n_ = True
    | b0 == 1 || b0 == n_ = True
    | otherwise = iter (tail b)
    where
        n_ = n-1
        (k,m) = find2km n_
        b0 = powMod n a m
        b = take k $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n_ = True
            | otherwise = iter xs

-- (eq. to) pow_ (*) (^2) n k = n^k
pow_ :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow_ _ _ _ 0 = 1
pow_ mul sq x_ n_ = f x_ n_ 1
    where
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x

mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a

-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow_ (mulMod m) (squareMod m)

-- Caller supplies a witness list w, which may be used for MR test.
-- Use faster trial division against a small primes list first, to
-- weed out more obvious composites.
is_prime w n
	| n < 100 = n `elem` primesTo100
	| any ((==0).(n`mod`)) primesTo100 = False
	| otherwise = all (millerRabinPrimality n) w

-- final result gets a more thorough Miller-Rabin
left_trunc base = head $ filter (is_prime primesTo100) (reverse hopeful) where
	hopeful = extend base $ takeWhile (<base) primesTo100 where
	extend b x = if null d then x else extend (b*base) d where
		d = concatMap addDigit [1..base-1]
		-- we do *one* prime test, which seems good enough in practice
		addDigit a = filter (is_prime [3]) $ map (a*b+) x

main = mapM_ print $ map (\x->(x, left_trunc x)) [3..21]
```


```txt

(3,23)
(4,4091)
(5,7817)
(6,4836525320399)
(7,817337)
(8,14005650767869)
(9,1676456897)
(10,357686312646216567629137)
(11,2276005673)
(12,13092430647736190817303130065827539)
(13,812751503)
(14,615419590422100474355767356763)
(15,34068645705927662447286191)
(16,1088303707153521644968345559987)
(17,13563641583101)
(18,571933398724668544269594979167602382822769202133808087)
(19,546207129080421139)
(20,1073289911449776273800623217566610940096241078373)
(21,391461911766647707547123429659688417)

```



## J



```J
ltp=:3 :0
  probe=. i.1 0
  while. #probe do.
    probe=. (#~ 1 p: y #.]),/(}.i.y),"0 _1/have=. probe
  end.
  >./y#.have
)
```


Quick example:


```J
   (,ltp)"0]3 4 5 6 7 8 9 10 11
 3                 23
 4               4091
 5               7817
 6      4836525320399
 7             817337
 8     14005650767869
 9         1676456897
10 992429121339693967
11         2276005673
```


Representation of a current longer effort:


```J
   (,ltp)"0]3}.i.20x
 3                                                     23
 4                                                   4091
 5                                                   7817
 6                                          4836525320399
 7                                                 817337
 8                                         14005650767869
 9                                             1676456897
10                               357686312646216567629137
11                                             2276005673
12                    13092430647736190817303130065827539
13                                              812751503
14                         615419590422100474355767356763
15                             34068645705927662447286191
16                        1088303707153521644968345559987
17                                         13563641583101
18 571933398724668544269594979167602382822769202133808087
19                                     546207129080421139
```



## Java

'''Code:'''

```java
import java.math.BigInteger;
import java.util.*;

class LeftTruncatablePrime
{
  private static List<BigInteger> getNextLeftTruncatablePrimes(BigInteger n, int radix, int millerRabinCertainty)
  {
    List<BigInteger> probablePrimes = new ArrayList<BigInteger>();
    String baseString = n.equals(BigInteger.ZERO) ? "" : n.toString(radix);
    for (int i = 1; i < radix; i++)
    {
      BigInteger p = new BigInteger(Integer.toString(i, radix) + baseString, radix);
      if (p.isProbablePrime(millerRabinCertainty))
        probablePrimes.add(p);
    }
    return probablePrimes;
  }

  public static BigInteger getLargestLeftTruncatablePrime(int radix, int millerRabinCertainty)
  {
    List<BigInteger> lastList = null;
    List<BigInteger> list = getNextLeftTruncatablePrimes(BigInteger.ZERO, radix, millerRabinCertainty);
    while (!list.isEmpty())
    {
      lastList = list;
      list = new ArrayList<BigInteger>();
      for (BigInteger n : lastList)
        list.addAll(getNextLeftTruncatablePrimes(n, radix, millerRabinCertainty));
    }
    if (lastList == null)
      return null;
    Collections.sort(lastList);
    return lastList.get(lastList.size() - 1);
  }

  public static void main(String[] args)
  {
    if (args.length != 2) {
      System.err.println("There must be exactly two command line arguments.");
      return;
    }
    int maxRadix;
    try {
      maxRadix = Integer.parseInt(args[0]);
      if (maxRadix < 3) throw new NumberFormatException();
    } catch (NumberFormatException e) {
      System.err.println("Radix must be an integer greater than 2.");
      return;
    }
    int millerRabinCertainty;
    try {
      millerRabinCertainty = Integer.parseInt(args[1]);
    } catch (NumberFormatException e) {
      System.err.println("Miiller-Rabin Certainty must be an integer.");
      return;
    }
    for (int radix = 3; radix <= maxRadix; radix++)
    {
      BigInteger largest = getLargestLeftTruncatablePrime(radix, millerRabinCertainty);
      System.out.print("n=" + radix + ": ");
      if (largest == null)
        System.out.println("No left-truncatable prime");
      else
        System.out.println(largest + " (in base " + radix + "): " + largest.toString(radix));
    }
  }

}
```


'''Example:'''


```txt
java LeftTruncatablePrime 17 100
n=3: 23 (in base 3): 212
n=4: 4091 (in base 4): 333323
n=5: 7817 (in base 5): 222232
n=6: 4836525320399 (in base 6): 14141511414451435
n=7: 817337 (in base 7): 6642623
n=8: 14005650767869 (in base 8): 313636165537775
n=9: 1676456897 (in base 9): 4284484465
n=10: 357686312646216567629137 (in base 10): 357686312646216567629137
n=11: 2276005673 (in base 11): a68822827
n=12: 13092430647736190817303130065827539 (in base 12): 471a34a164259ba16b324ab8a32b7817
n=13: 812751503 (in base 13): cc4c8c65
n=14: 615419590422100474355767356763 (in base 14): d967ccd63388522619883a7d23
n=15: 34068645705927662447286191 (in base 15): 6c6c2ce2ceeea4826e642b
n=16: 1088303707153521644968345559987 (in base 16): dbc7fba24fe6aec462abf63b3
n=17: 13563641583101 (in base 17): 6c66cc4cc83
```



## Julia

This solution keeps candidate values in an array.  A new digit is added with each generation, removing the previous generation's primes from the front of the list (<tt>popfirst!</tt>) and adding new candidates to the end of the list (<tt>append!</tt>) with each generation.  The maximum value yielded in each generation is tracked as a provisional answer.  Once the array is emptied (because no digit could be added to any of the previous generation's primes to yield a prime), the algorithm is finished and the answer found.

This solution is limited to a base of 17, to keep the processing time to well under a minute (about 15 seconds on an old but decent quality notebook).  (I've let it run to as high as 23, but that took something like 20 minutes as I was otherwise occupied.)  I did attempt a few optimizations of this general approach (such as moving the logic of <tt>addmsdigit</tt> into <tt>lefttruncprime</tt> and being clever about identifying the maximum of a given generation) but none of these tweaks resulted in a significant improvement in efficiency.

```julia
using Primes, Printf

function addmsdigit(p::Integer, b::Integer, s::Integer)
    a = Vector{typeof(p)}()
    q = p
    for i in 1:(b-1)
        q += s
        isprime(q) || continue
        push!(a, q)
    end
    return a
end

function lefttruncprime(pbase::Integer)
    a = Vector{BigInt}()
    append!(a, primes(pbase - 1))
    mlt = zero(BigInt)
    s = one(BigInt)
    while !isempty(a)
        mlt = maximum(a)
        s *= pbase
        for i in 1:length(a)
            p = popfirst!(a)
            append!(a, addmsdigit(p, pbase, s))
        end
    end
    return mlt
end

lo, hi = 3, 17
println("The largest left truncatable primes for bases", @sprintf(" %d to %d.", lo, hi))
for i in lo:hi
    mlt = lefttruncprime(i)
    @printf("%10d %-30d (%s)\n", i, mlt, string(mlt, base=i))
end

```
{{out}}

```txt

The largest left truncatable primes for bases 3 to 17.
     3 23 (212)
     4 4091 (333323)
     5 7817 (222232)
     6 4836525320399 (14141511414451435)
     7 817337 (6642623)
     8 14005650767869 (313636165537775)
     9 1676456897 (4284484465)
    10 357686312646216567629137 (357686312646216567629137)
    11 2276005673 (a68822827)
    12 13092430647736190817303130065827539 (471a34a164259ba16b324ab8a32b7817)
    13 812751503 (cc4c8c65)
    14 615419590422100474355767356763 (d967ccd63388522619883a7d23)
    15 34068645705927662447286191 (6c6c2ce2ceeea4826e642b)
    16 1088303707153521644968345559987 (dbc7fba24fe6aec462abf63b3)
    17 13563641583101 (6c66cc4cc83)

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

import java.math.BigInteger

fun nextLeftTruncatablePrimes(n: BigInteger, radix: Int, certainty: Int): List<BigInteger> {
    val probablePrimes = mutableListOf<BigInteger>()
    val baseString = if (n == BigInteger.ZERO) "" else n.toString(radix)
    for (i in 1 until radix) {
        val p = BigInteger(i.toString(radix) + baseString, radix)
        if (p.isProbablePrime(certainty)) probablePrimes.add(p)
    }
    return probablePrimes
}

fun largestLeftTruncatablePrime(radix: Int, certainty: Int): BigInteger? {
    var lastList: List<BigInteger>? = null
    var list = nextLeftTruncatablePrimes(BigInteger.ZERO, radix, certainty)
    while (!list.isEmpty()) {
        lastList = list
        list = mutableListOf()
        for (n in lastList) list.addAll(nextLeftTruncatablePrimes(n, radix, certainty))
    }
    if (lastList == null) return null
    return lastList.sorted().last()
}

fun main(args: Array<String>) {
    print("Enter maximum radix : ")
    val maxRadix = readLine()!!.toInt()
    print("Enter certainty     : ")
    val certainty = readLine()!!.toInt()
    println()
    for (radix in 3..maxRadix) {
        val largest = largestLeftTruncatablePrime(radix, certainty)
        print("Base = ${"%-2d".format(radix)} : ")
        if (largest == null)
            println("No left truncatable prime")
        else
            println("${largest.toString().padEnd(35)} -> ${largest.toString(radix)}")
    }
}
```


{{out}}
Sampe input/output - expect run time of about 3.5 minutes on a typical laptop:

```txt

Enter maximum radix : 17
Enter certainty     : 100

Base = 3  : 23                                  -> 212
Base = 4  : 4091                                -> 333323
Base = 5  : 7817                                -> 222232
Base = 6  : 4836525320399                       -> 14141511414451435
Base = 7  : 817337                              -> 6642623
Base = 8  : 14005650767869                      -> 313636165537775
Base = 9  : 1676456897                          -> 4284484465
Base = 10 : 357686312646216567629137            -> 357686312646216567629137
Base = 11 : 2276005673                          -> a68822827
Base = 12 : 13092430647736190817303130065827539 -> 471a34a164259ba16b324ab8a32b7817
Base = 13 : 812751503                           -> cc4c8c65
Base = 14 : 615419590422100474355767356763      -> d967ccd63388522619883a7d23
Base = 15 : 34068645705927662447286191          -> 6c6c2ce2ceeea4826e642b
Base = 16 : 1088303707153521644968345559987     -> dbc7fba24fe6aec462abf63b3
Base = 17 : 13563641583101                      -> 6c66cc4cc83

```



## Maple


```maple
MaxLeftTruncatablePrime := proc(b, $)
local i, j, c, p, sdprimes;
local tprimes := table();
    sdprimes := select(isprime, [seq(1..b-1)]);
    for p in sdprimes do
        if assigned(tprimes[p]) then
            next;
        end if;
        i := ilog[b](p)+1;
        j := 1;
        do
            c := j*b^i + p;
            if j >= b then
            # we have tried all 1 digit extensions of p, add p to tprimes and move back 1 digit
                tprimes[p] := p;
                if i = 1 then
                    # if we are at the first digit,  go to the next 1 digit prime
                    break;
                end if;
                i := i - 1;
                j := 1;
                p := p - iquo(p, b^i)*b^i;
            elif assigned(tprimes[c]) then
                j := j + 1;
            elif isprime(c) then
                p := c;
                i := i + 1;
                j := 1;
            else
                j := j+1;
            end if;
        end do;
    end do;
    return max(indices(tprimes, 'nolist'));
end proc;
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

<lang>LargestLeftTruncatablePrimeInBase[n_] :=
 Max[NestWhile[{Select[
       Flatten@Outer[Function[{a, b}, #[[2]] a + b],
         Range[1, n - 1], #[[1]]], PrimeQ], n #[[2]]} &, {{0},
     1}, #[[1]] != {} &, 1, Infinity, -1][[1]]]
```


Example:

<lang>Do[Print[n, "\t", LargestLeftTruncatablePrimeInBase@n], {n, 3, 17}]
```


Output:


```txt
3	23

4	4091

5	7817

6	4836525320399

7	817337

8	14005650767869

9	1676456897

10	357686312646216567629137

11	2276005673

12	13092430647736190817303130065827539

13	812751503

14	615419590422100474355767356763

15	34068645705927662447286191

16	1088303707153521644968345559987

17	13563641583101
```





## PARI/GP

Takes half a second to find the terms up to 10, with proofs of primality. The time can be halved without proofs (use <code>ispseudoprime</code> in place of <code>isprime</code>).

```parigp
a(n)=my(v=primes(primepi(n-1)),u,t,b=1,best); while(#v, best=vecmax(v); b*=n; u=List(); for(i=1,#v,for(k=1,n-1,if(isprime(t=v[i]+k*b), listput(u,t)))); v=Vec(u)); best
```



## Perl

Similar to the Pari solution.  Uses ntheory for primality tests and Math::GMPz for bigints that aren't dog slow.  We use <tt>is_prob_prime</tt> in the loop which does the BPSW test, then generate a proof for the selected result.

a(18) has a max candidate list of 1,449,405 entries and takes a bit over 20 minutes to solve.
{{libheader|ntheory}}

```perl
use ntheory qw/:all/;
use Math::GMPz;

sub lltp {
  my($n, $b, $best) = (shift, Math::GMPz->new(1));
  my @v = map { Math::GMPz->new($_) } @{primes($n-1)};
  while (@v) {
    $best = vecmax(@v);
    $b *= $n;
    my @u;
    foreach my $vi (@v) {
      push @u, grep { is_prob_prime($_) } map { $vi + $_*$b } 1 .. $n-1;
    }
    @v = @u;
  }
  die unless is_provable_prime($best);
  $best;
}

printf "%2d %s\n", $_, lltp($_)  for 3 .. 17;
```

{{out}}

```txt

 3 23
 4 4091
 5 7817
 6 4836525320399
 7 817337
 8 14005650767869
 9 1676456897
10 357686312646216567629137
11 2276005673
12 13092430647736190817303130065827539
13 812751503
14 615419590422100474355767356763
15 34068645705927662447286191
16 1088303707153521644968345559987
17 13563641583101

```



## Perl 6

{{works with|Rakudo|2018.12}}
Pretty fast for bases 3 .. 11. 12 is slow. 18 is glacial.

```perl6
for 3 .. * -> $base {
    say "Starting base $base...";
    my @stems = grep { .is-prime }, ^$base;
    for 1 .. * -> $digits {
        print ' ', @stems.elems;
        my @new;
        my $place = $base ** $digits;
        for 1 ..^ $base -> $digit {
            my $left = $digit * $place;
            @new.append: (@stems »+» $left).race(:8degree, :8batch).grep: *.is-prime
        }
        last unless +@new;
        @stems = @new;
    }
    say "\nLargest ltp in base $base = {@stems.max} or :$base\<@stems.max.base($base)}>\n";
}
```

{{out}}

```txt
Starting base 3...
 1 1 1
Largest ltp in base 3 = 23 or :3<212>

Starting base 4...
 2 2 3 3 3 3
Largest ltp in base 4 = 4091 or :4<333323>

Starting base 5...
 2 4 4 3 1 1
Largest ltp in base 5 = 7817 or :5<222232>

Starting base 6...
 3 4 12 25 44 54 60 62 59 51 35 20 12 7 3 2 1
Largest ltp in base 6 = 4836525320399 or :6<14141511414451435>

Starting base 7...
 3 6 6 4 1 1 1
Largest ltp in base 7 = 817337 or :7<6642623>

Starting base 8...
 4 12 29 50 66 77 61 51 38 27 17 8 3 2 1
Largest ltp in base 8 = 14005650767869 or :8<313636165537775>

Starting base 9...
 4 9 15 17 24 16 9 6 5 3
Largest ltp in base 9 = 1676456897 or :9<4284484465>

Starting base 10...
 4 11 39 99 192 326 429 521 545 517 448 354 276 212 117 72 42 24 13 6 5 4 3 1
Largest ltp in base 10 = 357686312646216567629137 or :10<357686312646216567629137>

Starting base 11...
 4 8 15 18 15 8 4 2 1
Largest ltp in base 11 = 2276005673 or :11<A68822827>

Starting base 12...
 5 23 119 409 1124 2496 4733 7711 11231 14826 17341 18787 19001 17567 15169 12085 9272 6606 4451 2882 1796 1108 601 346 181 103 49 19 8 2 1 1
Largest ltp in base 12 = 13092430647736190817303130065827539 or :12<471A34A164259BA16B324AB8A32B7817>

Starting base 13...
 5 13 20 23 17 11 7 4
Largest ltp in base 13 = 812751503 or :13<CC4C8C65>

Starting base 14...
 6 26 101 300 678 1299 2093 3017 3751 4196 4197 3823 3206 2549 1908 1269 783 507 322 163 97 55 27 13 5 2
Largest ltp in base 14 = 615419590422100474355767356763 or :14<D967CCD63388522619883A7D23>

Starting base 15...
 6 22 79 207 391 644 934 1177 1275 1167 1039 816 608 424 261 142 74 45 25 13 7 1
Largest ltp in base 15 = 34068645705927662447286191 or :15<6C6C2CE2CEEEA4826E642B>

Starting base 16...
 6 31 124 337 749 1292 1973 2695 3210 3490 3335 2980 2525 1840 1278 878 556 326 174 93 50 25 9 5 1
Largest ltp in base 16 = 1088303707153521644968345559987 or :16<DBC7FBA24FE6AEC462ABF63B3>

Starting base 17...
 6 22 43 55 74 58 41 31 23 8 1
Largest ltp in base 17 = 13563641583101 or :17<6C66CC4CC83>
...
```



## Phix

{{trans|Swift}}
Using is_prime_mr() from [[Miller–Rabin_primality_test#Phix]], unfortunately glacially slow...

(also tried using using Miller_Rabin() from that same page, marginally even slower...)

```Phix
function largestLeftTruncatablePrime(integer base)
integer radix = 0
sequence candidates = {ba_new(0)}
    while true do
        bigatom multiplier = ba_power(base,radix)
        sequence newCandidates = {}
        for i=1 to base-1 do
            bigatom mi = ba_multiply(i,multiplier)
            for j=1 to length(candidates) do
                bigatom cj = candidates[j],
                        cm = ba_add(cj,mi)
                if is_prime_mr(cm) then
                    newCandidates = append(newCandidates,cm)
                end if
            end for
        end for
        if newCandidates={} then exit end if
        candidates = newCandidates
        radix += 1
printf(1,"length %d candidates: %d \r",{radix,length(candidates)})
    end while
printf(1,"                         \r")
    return ba_sprint(candidates[$])
end function

for i=3 to 17 do
    atom t0 = time()
    string r = largestLeftTruncatablePrime(i),
           t = elapsed(time()-t0)
    printf(1,"base %d: %s (%s)\n",{i,r,t})
end for
```

{{out}}

```txt

base 3: 23 (0.0s)
base 4: 4091 (0.3s)
base 5: 7817 (0.2s)
base 6: 4836525320399 (1 minute and 34s)
base 7: 817337 (0.5s)
base 8: 14005650767869 (1 minute and 27s)
base 9: 1676456897 (11.7s)
base 10: 357686312646216567629137 (52 minutes and 30s)
base 11: 2276005673 (5.5s)
length 10 candidates: 14885
<killed>

```



## Python


```python
import random

def is_probable_prime(n,k):
    #this uses the miller-rabin primality test found from rosetta code
    if n==0 or n==1:
        return False
    if n==2:
        return True
    if n % 2 == 0:
        return False
    s = 0
    d = n-1

    while True:
        quotient, remainder = divmod(d, 2)
        if remainder == 1:
            break
        s += 1
        d = quotient

    def try_composite(a):
        if pow(a, d, n) == 1:
            return False
        for i in range(s):
            if pow(a, 2**i * d, n) == n-1:
                return False
        return True # n is definitely composite

    for i in range(k):
        a = random.randrange(2, n)
        if try_composite(a):
            return False

    return True # no base tested showed n as composite


def largest_left_truncatable_prime(base):
    radix = 0
    candidates = [0]
    while True:
        new_candidates=[]
        multiplier = base**radix
        for i in range(1,base):
            new_candidates += [x+i*multiplier for x in candidates if is_probable_prime(x+i*multiplier,30)]
        if len(new_candidates)==0:
            return max(candidates)
        candidates = new_candidates
        radix += 1

for b in range(3,24):
    print("%d:%d\n" % (b,largest_left_truncatable_prime(b)))

```


Output:

```txt
3:23

4:4091

5:7817

6:4836525320399

7:817337

8:14005650767869

9:1676456897

10:357686312646216567629137

11:2276005673

12:13092430647736190817303130065827539

13:812751503

```



## Ruby


### Ruby Ruby


```ruby

# Compute the largest left truncatable prime
#
#  Nigel_Galloway
#  September 15th., 2012.
#
require 'prime'
BASE = 3
MAX = 500
stems = Prime.each(BASE-1).to_a
(1..MAX-1).each {|i|
  print "#{stems.length} "
  t = []
  b = BASE ** i
  stems.each {|z|
    (1..BASE-1).each {|n|
      c = n*b+z
      t.push(c) if c.prime?
  }}
  break if t.empty?
  stems = t
}
puts "The largest left truncatable prime #{"less than #{BASE ** MAX} " if MAX < 500}in base #{BASE} is #{stems.max}"

```

By changing BASE from 3 to 14 this produces the solutions in 'Number of left truncatable primes in a given base' on the Discussion Page for bases except 10, 12 and 14.

The maximum left truncatable prime in bases 10 , 12, and 14 are very large. By changing MAX to 6 and BASE to 10 solves related task 1:

```txt

The largest left truncatable prime less than 1000000 in base 10 is 998443

```



### JRuby

I require a fast probably prime test. Java has one, is it any good? Let's find out. Ruby can borrow from Java using JRuby. Modifying the Ruby solution:

```Ruby

# Compute the largest left truncatable prime
#
#  Nigel_Galloway
#  September 15th., 2012.
#
require 'prime'
require 'java'
BASE = 10
MAX = 500
stems = Prime.each(BASE-1).to_a
(1..MAX-1).each {|i|
  print "#{stems.length} "
  t = []
  b = BASE ** i
  stems.each {|z|
    (1..BASE-1).each {|n|
      c = n*b+z
      t.push(c) if java.math.BigInteger.new(c.to_s).isProbablePrime(100)
  }}
  break if t.empty?
  stems = t
}
puts "\nThe largest left truncatable prime #{"less than #{BASE ** MAX} " if MAX < 500}in base #{BASE} is #{stems.max}"

```

Produces all the reults in 'Number of left truncatable primes in a given base' on the discussion page. For bases 18, 20, and 22 I changed the confidence level from 100 to 5 and checked the final answer. Even so base 18 takes a while. For base 24:

```txt

9 87 677 3808 17096 63509 199432 545332 1319708 2863180 Error: Your application
used more memory than the safety cap of 500m.
Specify -J-Xmx####m to increase it (#### = cap size in MB).
Specify -w for full OutOfMemoryError stack trace

```

That is going to be big!


## Racket


```racket
#lang racket
(require math/number-theory)

(define (prepend-digit b d i n)
  (+ (* d (expt b i)) n))

(define (extend b i ts)
  (define ts*
    (for/list ([t (in-set ts)])
           (for/set ([d (in-range 1 b)]
                     #:when (prime? (prepend-digit b d i t)))
                    (prepend-digit b d i t))))
  (apply set-union ts*))

(define (truncables b n)
  ; return set of truncables of length n in base b
  (if (= n 1)
      (for/set ([d (in-range 1 b)] #:when (prime? d)) d)
      (extend b (- n 1) (truncables b (- n 1)))))

(define (largest b)
  (let loop ([ts (truncables b 1)]
             [n 1])
    (define ts* (extend b n ts))
    (if (set-empty? ts*)
        (apply max (set->list ts))
        (loop ts* (+ n 1)))))


(for/list ([b (in-range 3 18)])
  (define l (largest b))
  ; (displayln (list b l))
  (list b l))

; Output:
'((3 23)
  (4 4091)
  (5 7817)
  (6 4836525320399)
  (7 817337)
  (8 14005650767869)
  (9 1676456897)
  (10 357686312646216567629137)
  (11 2276005673)
  (12 13092430647736190817303130065827539)
  (13 812751503)
  (14 615419590422100474355767356763)
  (15 34068645705927662447286191)
  (16 1088303707153521644968345559987)
  (17 13563641583101))
```


## Scala


```Scala
import scala.collection.parallel.immutable.ParSeq

object LeftTruncatablePrime extends App {
  private def leftTruncatablePrime(maxRadix: Int, millerRabinCertainty: Int) {
    def getLargestLeftTruncatablePrime(radix: Int, millerRabinCertainty: Int): BigInt = {
      def getNextLeftTruncatablePrimes(n: BigInt, radix: Int, millerRabinCertainty: Int) = {
        def baseString = if (n == 0) "" else n.toString(radix)

        for {i <- (1 until radix).par
             p = BigInt(Integer.toString(i, radix) + baseString, radix)
             if p.isProbablePrime(millerRabinCertainty)
        } yield p
      }

      def iter(list: ParSeq[BigInt], lastList: ParSeq[BigInt]): ParSeq[BigInt] = {
        if (list.isEmpty) lastList
        else
          iter((for (n <- list.par) yield getNextLeftTruncatablePrimes(n, radix, millerRabinCertainty)).flatten, list)
      }

      iter(getNextLeftTruncatablePrimes(0, radix, millerRabinCertainty), ParSeq.empty).max
    }

    for (radix <- (3 to maxRadix).par) {
      val largest = getLargestLeftTruncatablePrime(radix, millerRabinCertainty)
      println(f"n=$radix%3d: " +
        (if (largest == null) "No left-truncatable prime"
        else f"$largest%35d (in base $radix%3d) ${largest.toString(radix)}"))

    }
  }

  val argu: Array[String] = if (args.length >=2 ) args.slice(0, 2) else Array("17", "100")
  val maxRadix = argu(0).toInt.ensuring(_ > 2, "Radix must be an integer greater than 2.")

  try {
    val millerRabinCertainty = argu(1).toInt

    println(s"Run with maxRadix = $maxRadix and millerRabinCertainty = $millerRabinCertainty")

    leftTruncatablePrime(maxRadix, millerRabinCertainty)
    println(s"Successfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
  }
  catch {
    case _: NumberFormatException => Console.err.println("Miller-Rabin Certainty must be an integer.")
  }

}
```



## Sidef

{{trans|Perl}}

```ruby
func lltp(n) {
    var b = 1
    var best = nil
    var v = (n-1 -> primes)
 
    while (v) {
        best = v.max
        b *= n
        v.map! { |vi|
            {|i| i*b + vi }.map(1..^n).grep{.is_prime}...
        }
    }
 
    return best
}

for i in (3..17) {
    printf("%2d %s\n", i, lltp(i))
}
```

{{out}}

```txt

 3 23
 4 4091
 5 7817
 6 4836525320399
 7 817337
 8 14005650767869
 9 1676456897
10 357686312646216567629137
11 2276005673
12 13092430647736190817303130065827539
13 812751503
14 615419590422100474355767356763
15 34068645705927662447286191
16 1088303707153521644968345559987
17 13563641583101

```


Alternative solution:

```ruby
func digits2num(digits, base) {
    digits.map_kv {|k,v| base**k * v  }.sum
}

func generate_from_suffix(p, base) {

    var seq = [p]

    for n in (1 ..^ base) {
        var t = [p..., n]
        if (is_prime(digits2num(t, base))) {
            seq << __FUNC__(t, base)...
        }
    }

    return seq
}

func left_truncatable_primes(base) {

    var prime_digits = (base-1 -> primes)

    prime_digits.map  {|p| generate_from_suffix([p], base)... }\
                .map  {|t| digits2num(t, base) }\
                .sort
}

for n in (3..11) {
    var ltp = left_truncatable_primes(n)
    say ("There are #{'%4d' % ltp.len} left-truncatable primes in base #{'%2d' % n}, where largest is #{ltp.max}")
}
```

{{out}}

```txt

There are    3 left-truncatable primes in base  3, where largest is 23
There are   16 left-truncatable primes in base  4, where largest is 4091
There are   15 left-truncatable primes in base  5, where largest is 7817
There are  454 left-truncatable primes in base  6, where largest is 4836525320399
There are   22 left-truncatable primes in base  7, where largest is 817337
There are  446 left-truncatable primes in base  8, where largest is 14005650767869
There are  108 left-truncatable primes in base  9, where largest is 1676456897
There are 4260 left-truncatable primes in base 10, where largest is 357686312646216567629137
There are   75 left-truncatable primes in base 11, where largest is 2276005673

```



## Swift

{{trans|Python}}
{{libheader|Attaswift BigInt}}
<lang>import BigInt

func largestLeftTruncatablePrime(_ base: Int) -> BigInt {
  var radix = 0
  var candidates = [BigInt(0)]

  while true {
    let multiplier = BigInt(base).power(radix)
    var newCandidates = [BigInt]()

    for i in 1..<BigInt(base) {
      newCandidates += candidates.map({ ($0+i*multiplier, ($0+i*multiplier).isPrime(rounds: 30)) })
                                 .filter({ $0.1 })
                                 .map({ $0.0 })
    }

    if newCandidates.count == 0 {
      return candidates.max()!
    }

    candidates = newCandidates
    radix += 1
  }
}

for i in 3..<18 {
  print("\(i): \(largestLeftTruncatablePrime(i))")
}
```


{{out}}

```txt
3: 23
4: 4091
5: 7817
6: 4836525320399
7: 817337
8: 14005650767869
9: 1676456897
10: 357686312646216567629137
11: 2276005673
12: 13092430647736190817303130065827539
13: 812751503
14: 615419590422100474355767356763
15: 34068645705927662447286191
16: 1088303707153521644968345559987
17: 13563641583101

real	1m17.433s
user	1m16.915s
sys	0m0.252s
```



## Tcl


```tcl
package require Tcl 8.5

proc tcl::mathfunc::modexp {a b n} {
    for {set c 1} {$b} {set a [expr {$a*$a%$n}]} {
        if {$b & 1} {
            set c [expr {$c*$a%$n}]
        }
        set b [expr {$b >> 1}]
    }
    return $c
}
# Based on Miller-Rabin primality testing, but with small prime check first
proc is_prime {n {count 10}} {
    # fast check against small primes
    foreach p {
	2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
    } {
	if {$n == $p} {return true}
	if {$n % $p == 0} {return false}
    }

    # write n-1 as 2^s·d with d odd by factoring powers of 2 from n-1
    set d [expr {$n - 1}]
    for {set s 0} {$d & 1 == 0} {incr s} {
        set d [expr {$d >> 1}]
    }

    for {} {$count > 0} {incr count -1} {
        set a [expr {2 + int(rand()*($n - 4))}]
        set x [expr {modexp($a, $d, $n)}]
        if {$x == 1 || $x == $n - 1} continue
        for {set r 1} {$r < $s} {incr r} {
            set x [expr {modexp($x, 2, $n)}]
            if {$x == 1} {return false}
            if {$x == $n - 1} break
        }
	if {$x != $n-1} {return false}
    }
    return true
}

proc max_left_truncatable_prime {base} {
    set stems {}
    for {set i 2} {$i < $base} {incr i} {
	if {[is_prime $i]} {
	    lappend stems $i
	}
    }
    set primes $stems
    set size 0
    for {set b $base} {[llength $stems]} {set b [expr {$b * $base}]} {
	# Progress monitoring is nice once we get to 10 and beyond...
	if {$base > 9} {
	    puts "\t[llength $stems] candidates at length [incr size]"
	}
	set primes $stems
	set certainty [expr {[llength $primes] > 100 ? 1 : 5}]
	set stems {}
	foreach s $primes {
	    for {set i 1} {$i < $base} {incr i} {
		set n [expr {$b*$i + $s}]
		if {[is_prime $n $certainty]} {
		    lappend stems $n
		}
	    }
	}
    }
    # Could be several at same length; choose largest
    return [tcl::mathfunc::max {*}$primes]
}

for {set i 3} {$i <= 20} {incr i} {
    puts "$i: [max_left_truncatable_prime $i]"
}
```

{{out|Output up to base 12 (tab-indented parts are progress messages)}}

```txt

3: 23
4: 4091
5: 7817
6: 4836525320399
7: 817337
8: 14005650767869
9: 1676456897
	4 candidates at length 1
	11 candidates at length 2
	39 candidates at length 3
	99 candidates at length 4
	192 candidates at length 5
	326 candidates at length 6
	429 candidates at length 7
	521 candidates at length 8
	545 candidates at length 9
	517 candidates at length 10
	448 candidates at length 11
	354 candidates at length 12
	276 candidates at length 13
	212 candidates at length 14
	117 candidates at length 15
	72 candidates at length 16
	42 candidates at length 17
	24 candidates at length 18
	13 candidates at length 19
	6 candidates at length 20
	5 candidates at length 21
	4 candidates at length 22
	3 candidates at length 23
	1 candidates at length 24
10: 357686312646216567629137
	4 candidates at length 1
	8 candidates at length 2
	15 candidates at length 3
	18 candidates at length 4
	15 candidates at length 5
	8 candidates at length 6
	4 candidates at length 7
	2 candidates at length 8
	1 candidates at length 9
11: 2276005673
	5 candidates at length 1
	23 candidates at length 2
	119 candidates at length 3
	409 candidates at length 4
	1126 candidates at length 5
	2504 candidates at length 6
	4746 candidates at length 7
	7727 candidates at length 8
	11257 candidates at length 9
	14860 candidates at length 10
	17375 candidates at length 11
	18817 candidates at length 12
	19027 candidates at length 13
	17594 candidates at length 14
	15192 candidates at length 15
	12106 candidates at length 16
	9292 candidates at length 17
	6621 candidates at length 18
	4466 candidates at length 19
	2889 candidates at length 20
	1799 candidates at length 21
	1109 candidates at length 22
	601 candidates at length 23
	346 candidates at length 24
	181 candidates at length 25
	103 candidates at length 26
	49 candidates at length 27
	19 candidates at length 28
	8 candidates at length 29
	2 candidates at length 30
	1 candidates at length 31
	1 candidates at length 32
12: 13092430647736190817303130065827539
	5 candidates at length 1
	13 candidates at length 2
	20 candidates at length 3
	23 candidates at length 4
	17 candidates at length 5
	11 candidates at length 6
	7 candidates at length 7
	4 candidates at length 8
13: 812751503

```

I think I'll need to find a faster computer to calculate much more of the sequence, but memory consumption is currently negligible so there's no reason to expect there to be any major problems.


## zkl


```zkl
var [const] BN=Import("zklBigNum");  // libGMP
fcn largest_lefty_prime(base){
   primes,p:=List(),BN(1); while(p.nextPrime()<base){ primes.append(p.copy()) }
   b,biggest := BN(1),0;
   while(primes){
      b*=base;  // base,base^2,base^3... gets big
      ps:=List();
      foreach p,n in (primes,[1..base-1]){
         if((z:=(p + b*n)).probablyPrime()){
	    ps.append(z);
	    if(z>biggest) biggest=z;
	 }
      }
      primes=ps;  // the number of lists is small
   }
   biggest
}

foreach n in ([3..17]){ println("%2d %s".fmt(n,largest_lefty_prime(n))) }
```

I've included 18,19 & 20 here but 18 & 20 are very very slow to compute, it is seconds to compute all the others.
{{out}}

```txt

 3 23
 4 4091
 5 7817
 6 4836525320399
 7 817337
 8 14005650767869
 9 1676456897
10 357686312646216567629137
11 2276005673
12 13092430647736190817303130065827539
13 812751503
14 615419590422100474355767356763
15 34068645705927662447286191
16 1088303707153521644968345559987
17 13563641583101
18 571933398724668544269594979167602382822769202133808087
19 546207129080421139
20 1073289911449776273800623217566610940096241078373

```


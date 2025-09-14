+++
title = "Random number generator (included)"
description = ""
date = 2019-09-01T16:40:34Z
aliases = []
[extra]
id = 5365
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "8th",
  "actionscript",
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "cmake",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "dwscript",
  "echolisp",
  "elena",
  "elixir",
  "erlang",
  "euler_math_toolbox",
  "factor",
  "fortran",
  "free_pascal",
  "freebasic",
  "futurebasic",
  "gap",
  "go",
  "golfscript",
  "groovy",
  "haskell",
  "inform_7",
  "io",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "matlab",
  "maxima",
  "nemerle",
  "netrexx",
  "nim",
  "ocaml",
  "octave",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pl_sql",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rascal",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "sparkling",
  "stata",
  "tcl",
  "txr",
  "unix_shell",
  "ursa",
  "ursala",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

The task is to:
: State the type of random number generator algorithm used in a language's built-in random number generator. If the language or its immediate libraries don't provide a random number generator, skip this task.
: If possible, give a link to a wider [[wp:List of random number generators|explanation]] of the algorithm used.

<small>Note: the task is ''not'' to create an RNG, but to report on the languages in-built RNG that would be the most likely RNG used.</small>

The main types of pseudo-random number generator ([[wp:PRNG|PRNG]]) that are in use are the [[linear congruential generator|Linear Congruential Generator]] ([[wp:Linear congruential generator|LCG]]), and the Generalized Feedback Shift Register ([[wp:Generalised_feedback_shift_register#Non-binary_Galois_LFSR|GFSR]]), (of which the [[wp:Mersenne twister|Mersenne twister]] generator is a subclass). The last main type is where the output of one of the previous ones (typically a Mersenne twister) is fed through a [[cryptographic hash function]] to maximize unpredictability of individual bits.

Note that neither LCGs nor GFSRs should be used for the most demanding applications (cryptography) without additional steps.


## 8th

The default random number generator in 8th is a cryptographically strong one using [https://en.wikipedia.org/wiki/Fortuna_%28PRNG%29 Fortuna], which is seeded from the system's entropy provider.  An additional random generator (which is considerably faster) is a [http://www.pcg-random.org/ PCG], though it is not cryptographically strong.

## ActionScript

In both Actionscript 2 and 3, the type of pseudorandom number generator is implementation-defined. This number generator is accessed through the Math.random() function, which returns a double greater than or equal to 0 and less than 1.[http://livedocs.adobe.com/flash/9.0/ActionScriptLangRefV3/Math.html#random%28%29][http://flash-reference.icod.de/Math.html#random%28%29] In Actionscript 2, the global random() function returns an integer greater than or equal to 0 and less than the given argument, but it is deprecated and not recommended.[http://flash-reference.icod.de/global_functions.html#random()]


## Ada

The Ada standard defines Random Number Generation in Annex A.5.2. There are two kinds of RNGs, Ada.Numerics.Float_Random for floating point values from 0.0 to 1.0, and Ada.Numerics.Discrete_Random for pseudo-random values of enumeration types (including integer types). It provides facilities to initialize the generator and to save it's state.

The standard requires the implementation to uniformly distribute over the range of the result type.

The used algorithm is implementation defined. The standard says: "To enable the user to determine the suitability of the random number generators for the intended application, the implementation shall describe the algorithm used and shall give its period, if known exactly, or a lower bound on the period, if the exact period is unknown."

* [http://www.adahome.com/rm95/rm9x-A-05-02.html Ada 95 RM - A.5.2 Random Number Generation]
* [http://www.adaic.com/standards/05rm/html/RM-A-5-2.html Ada 2005 RM - A.5.2 Random Number Generation]
* [http://www.adaic.org/resources/add_content/standards/12rm/html/RM-A-5-2.html Ada 2005 RM - A.5.2 Random Number Generation]


## ALGOL 68

Details of the random number generator are in the Revised Reports sections: 10.2.1. and 10.5.1.
* [http://vestein.arb-phys.uni-dortmund.de/~wb/RR/rrA2.html 10.2. The standard prelude - 10.2.1. Environment enquiries]
* [http://vestein.arb-phys.uni-dortmund.de/~wb/RR/rrA5.html 10.5. The particular preludes and postlude - 10.5.1. The particular preludes]


```algol68
PROC ℒ next random = (REF ℒ INT a)ℒ REAL: ( a :=
¢ the next pseudo-random ℒ integral value after 'a' from a
uniformly distributed sequence on the interval [ℒ 0,ℒ maxint] ¢;

¢ the real value corresponding to 'a' according to some mapping of
integral values [ℒ 0, ℒ max int] into real values [ℒ 0, ℒ 1)
i.e. such that -0 <= x < 1 such that the sequence of real
values so produced preserves the properties of pseudo-randomness
and uniform distribution of the sequence of integral values ¢);

INT ℒ last random := # some initial random number #;
PROC ℒ random = ℒ REAL: ℒ next random(ℒ last random);
```


Note the suitable "next random number" is suggested to be: ( a := &cent; the next pseudo-random ℒ integral value after 'a' from a uniformly distributed sequence on the interval [ℒ 0,ℒ maxint] &cent;; &cent; the real value corresponding to 'a' according to some mapping of integral values [ℒ 0, ℒ max int] into real values [ℒ 0, ℒ 1) i.e., such that -0 <= x < 1 such that the sequence of real values so produced preserves the properties of pseudo-randomness and uniform distribution of the sequence of integral values &cent;);

Algol68 supports random number generation for all precisions available for the specific implementation. The prefix '''ℒ real''' indicates all the available precisions.  eg '''short short real''', '''short real''', '''real''', '''long real''', '''long long real''' etc

For an ASCII implementation and for '''long real''' precision these routines would appears as:

```algol68
PROC long next random = (REF LONG INT a)LONG REAL: # some suitable next random number #;
INT long last random := # some initial random number #;
PROC long random = LONG REAL: long next random(long last random);
```



## AutoHotkey

The built-in command [http://www.autohotkey.com/docs/commands/Random.htm Random] generates a pseudo-random number using Mersenne Twister "MT19937" (see documentation).


## AWK

The built-in command "rand" generates a pseudo-random uniform distributed random variable. More information is available from the documentation of
[http://www.gnu.org/software/gawk/manual/html_node/Numeric-Functions.html gawk].

It is important that the RNG is seeded with the funtions "srand", otherwise,
the same random number is produced.

Example usage: see [http://rosettacode.org/wiki/Random_number_generator_(included)#UNIX_Shell #UNIX_Shell]


## BASIC


The RND function generates a pseudo random number greater than or equal to zero, but less than one. The implementation is machine specific based on contents of the ROM and there is no fixed algorithm.


## Batch File

Windows batch files can use the <code>%RANDOM%</code> pseudo-variable which returns a pseudo-random number between 0 and 32767. Behind the scenes this is just a call to the C runtime's <code>rand()</code> function which uses an LCG in this case:
:<math>X_{n+1}=X_n\cdot 214013 + 2531011 \pmod {2^{15}}</math>


## BBC BASIC

The RND function uses a 33-bit maximal-length Linear Feedback Shift Register (LFSR), with 32-bits being used to provide the result.  Hence the sequence length is 2^33-1, during which the value zero is returned once and all non-zero 32-bit values are each returned twice.


## Befunge

The ? instruction usually uses the random number generator in the interpreter's language. The original interpreter is written in C and uses rand().


## C

Standard C has rand(). Some implementations of C have other sources of random numbers, along with rand().

===C rand()===
The C standard specifies the interface to the rand() and srand() functions in <stdlib.h>.

* <code>void srand(unsigned int seed)</code> begins a new sequence of pseudorandom integers.
* <code>int rand(void)</code> returns a pseudorandom integer in the range from 0 to RAND_MAX.
** RAND_MAX must be at least 32767.

The same seed to srand() reproduces the same sequence. The default seed is 1, when a program calls rand() without calling srand(); so srand(1) reproduces the default sequence. ([http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf n1124.pdf])

There are no requirements as to the algorithm to be used for generating the random numbers. All versions of rand() return integers that are uniformly distributed in the interval from 0 to RAND_MAX, but some algorithms have problems in their randomness. For example, the cycle might be too short, or the probabilities might not be independent.

Many popular C libraries implement rand() with a [[linear congruential generator]]. The specific multiplier and constant varies by implementation, as does which subset of bits within the result is returned as the random number. These rand() functions should not be used where a good quality random number generator is required.

====BSD rand()====
Among current systems, [[BSD]] might have the worst algorithm for rand(). BSD rand() sets RAND_MAX to <math>2^{31} - 1</math> and uses this linear congruential formula:

* <math>state_{n + 1} = 1103515245 \times state_n + 12345 \pmod{2^{31}}</math>
* <math>rand_n = state_n</math>

[[FreeBSD]] switched to a different formula, but [[NetBSD]] and [[OpenBSD]] stayed with this formula. ([http://cvsweb.netbsd.org/bsdweb.cgi/src/lib/libc/stdlib/rand.c?only_with_tag=MAIN NetBSD rand.c], [http://www.openbsd.org/cgi-bin/cvsweb/src/lib/libc/stdlib/rand.c OpenBSD rand.c])

BSD rand() produces a cycling sequence of only <math>2^{31}</math> possible states; this is already too short to produce good random numbers. The big problem with BSD rand() is that the low <math>n</math> bits' cycle sequence length is only <math>2^n</math>. (This problem happens because the modulus <math>2^{31}</math> is a power of two.) The worst case, when <math>n = 1</math>, becomes obvious if one uses the low bit to flip a coin.


```c
#include <stdio.h>
#include <stdlib.h>

/* Flip a coin, 10 times. */
int
main()
{
	int i;
	srand(time(NULL));
	for (i = 0; i < 10; i++)
		puts((rand() % 2) ? "heads" : "tails");
	return 0;
}
```


If the C compiler uses BSD rand(), then this program has only two possible outputs.

* At even seconds: heads, tails, heads, tails, heads, tails, heads, tails, heads, tails.
* At odd seconds: tails, heads, tails, heads, tails, heads, tails, heads, tails, heads.

The low bit manages a uniform distribution between heads and tails, but it has a period length of only 2: it can only flip a coin 2 times before it must repeat itself. Therefore it must alternate heads and tails. This is not a real coin, and these are not truly random flips.

In general, the low bits from BSD rand() are much less random than the high bits. This defect of BSD rand() is so famous that some programs ignore the low bits from rand().

====Microsoft rand()====
Microsoft sets RAND_MAX to 32767 and uses this linear congruential formula:

* <math>state_{n + 1} = 214013 \times state_n + 2531011 \pmod{2^{31}}</math>
* <math>rand_n = seed_n \div 2^{16}</math>

===POSIX drand48()===
POSIX adds the drand48() family to <stdlib.h>.

* <code>void srand48(long seed)</code> begins a new sequence.
* <code>double drand48(void)</code> returns a random double in [0.0, 1.0).
* <code>long lrand48(void)</code> returns a random long in [0, 2**31).
* <code>long mrand48(void)</code> returns a random long in [-2**31, 2**31).

This family uses a 48-bit linear congruential generator with this formula:

* <math>r_{n + 1} = 25214903917 \times r_n + 11 \pmod {2^{48}}</math>


## C++

As part of the C++11 specification the language now includes various forms of random number generation.

While the default engine is implementation specific (ex, unspecified), the following Pseudo-random generators are available in the standard:
* Linear congruential (minstd_rand0, minstd_rand)
* Mersenne twister (mt19937, mt19937_64)
* Subtract with carry (ranlux24_base, ranlux48_base)
* Discard block (ranlux24, ranlux48)
* Shuffle order (knuth_b)

Additionally, the following distributions are supported:
* Uniform distributions: uniform_int_distribution, uniform_real_distribution
* Bernoulli distributions: bernoulli_distribution, geometric_distribution, binomial_distribution, negative_binomial_distribution
* Poisson distributions: poisson_distribution, gamma_distribution, exponential_distribution, weibull_distribution, extreme_value_distribution
* Normal distributions: normal_distribution, fisher_f_distribution, cauchy_distribution, lognormal_distribution,  chi_squared_distribution, student_t_distribution
* Sampling distributions: discrete_distribution, piecewise_linear_distribution, piecewise_constant_distribution

Example of use:
```cpp
#include <iostream>
#include <string>
#include <random>

int main()
{
    std::random_device rd;
    std::uniform_int_distribution<int> dist(1, 10);
    std::mt19937 mt(rd());

    std::cout << "Random Number (hardware): " << dist(rd) << std::endl;
    std::cout << "Mersenne twister (hardware seeded): " << dist(mt) << std::endl;
}
```



## C#

The .NET Random class says that it uses Knuth's subtractive random number generator algorithm.[http://msdn.microsoft.com/en-us/library/system.random.aspx#remarksToggle]


## Clojure

See Java.


## CMake

CMake has a random ''string'' generator.


```cmake
# Show random integer from 0 to 9999.
string(RANDOM LENGTH 4 ALPHABET 0123456789 number)
math(EXPR number "${number} + 0")  # Remove extra leading 0s.
message(STATUS ${number})
```


The current implementation (in [http://cmake.org/gitweb?p=cmake.git;a=blob;f=Source/cmStringCommand.cxx;hb=HEAD cmStringCommand.cxx] and [http://cmake.org/gitweb?p=cmake.git;a=blob;f=Source/cmSystemTools.cxx;hb=HEAD cmSystemTools.cxx]) calls [[{{PAGENAME}}#C|rand() and srand() from C]]. It picks random letters from the alphabet. The probability of each letter is near ''1 &divide; length'', but the implementation uses floating-point arithmetic to map ''RAND_MAX + 1'' values onto ''length'' letters, so there is a small modulo bias when ''RAND_MAX + 1'' is not a multiple of ''length''.

CMake 2.6.x has [http://public.kitware.com/Bug/view.php?id=9851 bug #9851]; two random strings might be equal because they use the same seed. CMake 2.8.0 fixes this bug by seeding the random generator only once, during the first call to <code>string(RANDOM ...)</code>.

CMake 2.8.5 tries a [[random number generator (device)|secure seed]] (CryptGenRandom or /dev/urandom) or falls back to high-resolution [[system time]]. Older versions seed the random generator with <code>time(NULL)</code>, the current time in seconds.


## Common Lisp

The easiest way to generate random numbers in Common Lisp is to use the built-in rand function after seeding the random number generator. For example, the first line seeds the random number generator and the second line generates a number from 0 to 9

```lisp
(setf *random-state* (make-random-state t))
(rand 10)
```

[https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node133.html Common Lisp: The Language, 2nd Ed.] does not specify a specific random number generator algorithm.


## D

From std.random:

The generators feature a number of well-known and well-documented methods of generating random numbers. An overall fast and reliable means to generate random numbers is the Mt19937 generator, which derives its name from "[http://en.wikipedia.org/wiki/Mersenne_twister Mersenne Twister] with a period of 2 to the power of 19937". In memory-constrained situations, [http://en.wikipedia.org/wiki/Linear_congruential_generator linear congruential] generators such as MinstdRand0 and MinstdRand might be useful. The standard library provides an alias Random for whichever generator it considers the most fit for the target environment.
=={{header|Déjà Vu}}==
The standard implementation, <code>[[vu]]</code>, uses a Mersenne twister.


```dejavu
!print random-int # prints a 32-bit random integer
```



## Delphi

According to [[wp:Linear_congruential_generator#Parameters_in_common_use|Wikipedia]], Delphi uses a Linear Congruential Generator.

Random functions:
  function Random : Extended;
  function Random ( LimitPlusOne  : Integer ) : Integer;
  procedure Randomize;

Based on the values given in the wikipedia entry here is a Delphi compatible implementation for use in other pascal dialects.

```pascal

{$ifdef fpc}{$mode objfpc}{$endif}

interface

function LCGRandom: extended; overload;inline;
function LCGRandom(const range:longint):longint;overload;inline;

implementation

function IM:cardinal;inline;
begin
  RandSeed := RandSeed * 134775813  + 1;
  Result := RandSeed;
end;

function LCGRandom: extended; overload;inline;
begin
  Result := IM * 2.32830643653870e-10;
end;

function LCGRandom(const range:longint):longint;overload;inline;
begin
  Result := IM * range shr 32;
end;

end.
```



## DWScript

DWScript currently uses a 64bit [[wp:Xorshift|XorShift]] PRNG, which is a fast and light form of GFSR.


## EchoLisp

EchoLisp uses an ARC4 (or RCA4) implementation by David Bau, which replaces the JavaScript Math.random(). Thanks to him. [https://github.com/davidbau/seedrandom].
Some examples :

```lisp

(random-seed "albert")
(random) → 0.9672510261922906 ; random float in [0 ... 1[
(random 1000)  → 726  ; random integer in [0 ... 1000 [
(random -1000) → -936 ; random integer in ]-1000 1000[

(lib 'bigint)
(random 1e200) → 48635656441292641677...3917639734865662239925...9490799697903133046309616766848265781368

```


## Elena

ELENA 4.x :

```elena
import extensions;

public program()
{
    console.printLine(randomGenerator.nextReal());
    console.printLine(randomGenerator.eval(0,100))
}
```

```txt

0.706398
46

```



## Elixir

Elixir does not come with its own module for random number generation. But you can use the appropriate Erlang functions instead. Some examples:

```elixir

# Seed the RNG
:random.seed(:erlang.now())

# Integer in the range 1..10
:random.uniform(10)

# Float between 0.0 and 1.0
:random.uniform()

```

For further information, read the Erlang section.


## Erlang

Random number generator. The method is attributed to B.A. Wichmann and I.D.Hill, in 'An efficient and portable pseudo-random number generator', Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.

The current algorithm is a modification of the version attributed to Richard A O'Keefe in the standard Prolog library.

Every time a random number is requested, a state is used to calculate it, and a new state produced. The state can either be implicit (kept in the process dictionary) or be an explicit argument and return value. In this implementation, the state (the type ran()) consists of a tuple of three integers.

It should be noted that this random number generator is not cryptographically strong. If a strong cryptographic random number generator is needed for example crypto:rand_bytes/1 could be used instead.

Seed with a fixed known value triplet A1, A2, A3:

```Erlang

random:seed(A1, A2, A3)

```

Example with the running time:

```Erlang

...
{A1,A2,A3} = erlang:now(),
random:seed(A1, A2, A3),
...sequence of randoms used
random:seed(A1, A2, A3),
...same sequence of randoms used

```

Get a random float value between 0.0 and 1.0:

```Erlang

Rfloat = random:uniform(),

```

Get a random integer value between 1 and N (N is an integer >= 1):

```Erlang

Rint = random:uniform(N),

```



## Euler Math Toolbox


Bays and Durham as describend in Knuth's book.


## Factor

The default RNG used when the <code>random</code> vocabulary is used, is the [[wp:Mersenne twister|Mersenne twister]] algorithm [http://docs.factorcode.org/content/article-random.html]. But there are other RNGs available, including [[wp:SFMT|SFMT]], the system RNG ([[wp:/dev/random|/dev/random]] on Unix) and [[wp:Blum Blum Shub|Blum Blum Shub]]. It's also very easy to implement your own RNG and integrate it into the system. [http://docs.factorcode.org/content/article-random-protocol.html]


## Fortran

Fortran has intrinsic random_seed() and random_number() subroutines. Used algorithm of the pseudorandom number generator is compiler dependent (not specified in ISO Fortran Standard, see ISO/IEC 1539-1:2010 (E), 13.7.135 RANDOM NUMBER). For algorithm in GNU gfortran see https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fNUMBER.html
Note that with the GNU gfortran compiler program needs to call random_seed with a random PUT= argument to get a pseudorandom number otherwise the sequence always starts with the same number. Intel compiler ifort reinitializes  the seed randomly without PUT argument to random value using the system date and time. Here we are seeding random_seed() with some number obtained from the Linux urandom device.


```fortran

program rosetta_random
   implicit none

   integer, parameter :: rdp = kind(1.d0)
   real(rdp) :: num
   integer, allocatable :: seed(:)
   integer :: un,n, istat

   call random_seed(size = n)
   allocate(seed(n))

   ! Seed with the OS random number generator
   open(newunit=un, file="/dev/urandom", access="stream", &
   form="unformatted", action="read", status="old", iostat=istat)
   if (istat == 0) then
      read(un) seed
      close(un)
   end if
   call random_seed (put=seed)
   call random_number(num)
   write(*,'(E24.16)') num
end program rosetta_random

```



## FreeBASIC

FreeBASIC has a Rnd() function which produces a pseudo-random double precision floating point number in the half-closed interval [0, 1) which can then be easily used to generate pseudo-random numbers (integral or decimal) within any range.

The sequence of pseudo-random numbers can either by seeded by a parameter to the Rnd function itself or to the Randomize statement and, if omitted, uses a seed based on the system timer.

However, a second parameter to the Randomize statement determines which of 5 different algorithms is used to generate the pseudo-random numbers:

1. Uses the C runtime library's rand() function (based on LCG) which differs depending on the platform but produces a low degree of randomness.

2. Uses a fast, platform independent, algorithm with 32 bit granularity and a reasonable degree of randomness. The basis of this algorithm is not specified in the language documentation.

3. Uses the Mersenne Twister algorithm (based on GFSR) which is platform independent, with 32 bit granularity and a high degree of randomness. This is good enough for most non-cryptographic purposes.

4. Uses a QBASIC compatible algorithm  which is platform independent, with 24 bit granularity and a low degree of randomness.

5. Uses system features (Win32 Crypto API or /dev/urandom device on Linux) to generate pseudo-random numbers, with 32 bit granularity and a very high degree of randomness (cryptographic strength).

A parameter of 0 can also be used (and is the default if omitted) which uses algorithm 3 in the -lang fb dialect, 4 in the -lang qb dialect and 1 in the -lang fblite dialect.


## Free Pascal

FreePascal's function random uses the MersenneTwister (for further details, see the file rtl/inc/system.inc).
The random is conform MT19937 and is therefor compatible with e.g. the C++11 MT19937 implementation.


```pascal

program RandomNumbers;
// Program to demonstrate the Random and Randomize functions.
var
  RandomInteger: integer;
  RandomFloat: double;
begin
  Randomize; // generate a new sequence every time the program is run
  RandomFloat := Random();       // 0 <= RandomFloat < 1
  Writeln('Random float between 0 and 1: ', RandomFloat: 5: 3);
  RandomFloat := Random() * 10;  // 0 <= RandomFloat < 10
  Writeln('Random float between 0 and 10: ', RandomFloat: 5: 3);
  RandomInteger := Random(10);   //  0 <= RandomInteger < 10
  Writeln('Random integer between 0 and 9: ', RandomInteger);
  // Wait for <enter>
  Readln;
end.

```



## FutureBasic

Syntax:

```txt

randomInteger = rnd(expr)

```

This function returns a pseudo-random long integer uniformly distributed in the range 1 through expr. The expr parameter should be greater than 1, and must not exceed 65536. If the value returned is to be assigned to a 16-bit integer (randomInteger), expr should not exceed 32767. The actual sequence of numbers returned by rnd depends on the random number generator's "seed" value. (Note that rnd(1) always returns the value 1.)

Syntax:

```txt

random (or randomize) [expr]

```

This statement "seeds" the random number generator: this affects the sequence of values which are subsequently returned by the rnd function and the maybe function. The numbers returned by rnd and maybe are not truly random, but follow a "pseudo-random" sequence which is uniquely determined by the seed number (expr). If you use the same seed number on two different occasions, you'll get the same sequence of "random" numbers both times. When you execute random without any expr parameter, the system's current time is used to seed the random number generator.

Example 1:

```txt

random 375  // using seed number

```

Example 2:

```txt

random      // current system time used as seed

```


Example:
To get a random integer between two arbitrary limits min and max, use the following statement. (Note: max - min must be less than or equal to 65536.):

```txt

randomInteger = rnd(max - min + 1) + min - 1

```

To get a random fraction, greater than or equal to zero and less than 1, use this statement:

```txt

frac! = (rnd(65536)-1)/65536.0

```

To get a random long integer in the range 1 through 2,147,483,647, use this statement:

```txt

randomInteger& = ((rnd(65536) - 1)<<15) + rnd(32767)

```



## GAP

GAP may use two algorithms : MersenneTwister, or algorithm A in section 3.2.2 of TAOCP (which is the default). One may create several ''random sources'' in parallel, or a global one (based on the TAOCP algorithm).

```gap
# Creating a random source
rs := RandomSource(IsMersenneTwister);

# Generate a random number between 1 and 10
Random(rs, 1, 10);

# Same with default random source
Random(1, 10);
```

One can get random elements from many objects, including lists

```gap

Random([1, 10, 100]);

# Random permutation of 1..200
Random(SymmetricGroup(200));

# Random element of Z/23Z :
Random(Integers mod 23);
```



## Go

Go has two random number packages in the standard library and another package in the "subrepository."

# [https://golang.org/pkg/math/rand/ math/rand] in the standard library provides general purpose random number support, implementing some sort of feedback shift register.  (It uses a large array commented "feeback register" and has variables named "tap" and "feed.")  Comments in the code attribute the algorithm to DP Mitchell and JA Reeds.  A little more insight is in [https://github.com/golang/go/issues/21835 this issue] in the Go issue tracker.
# [https://golang.org/pkg/crypto/rand/ crypto/rand], also in the standard library, says it "implements a cryptographically secure pseudorandom number generator."  I think though it should say that it ''accesses'' a cryptographically secure pseudorandom number generator.  It uses <tt>/dev/urandom</tt> on Unix-like systems and the CryptGenRandom API on Windows.
# [https://godoc.org/golang.org/x/exp/rand x/exp/rand] implements the Permuted Congruential Generator which is also described in the issue linked above.


## Golfscript

Golfscript uses Ruby's Mersenne Twister algorithm

<code>~rand</code> produces a random integer between 0 and n-1, where n is a positive integer piped into the program


## Groovy

Same as Java.


## Haskell

The [http://www.haskell.org/onlinereport/random.html Haskell 98 report] specifies an interface for pseudorandom number generation and requires that implementations be minimally statistically robust. It is silent, however, on the choice of algorithm.

=={{header|Icon}} and {{header|Unicon}} ==
Icon and Unicon both use the same linear congruential random number generator  x := (x * 1103515245 + 453816694) mod 2^31.  Icon uses an initial seed value of 0 and Unicon randomizes the initial seed.

This LCRNG has a number of well documented quirks (see [http://www.cs.arizona.edu/icon/analyst/ia.htm The Icon Analyst issues #26, 28, 38]) relating to the choices of an even additive and a power of two modulus.  This LCRNG produces two independent sequences of length 2^30 one of even numbers the other odd.

Additionally, the {{libheader|Icon Programming Library}} [http://www.cs.arizona.edu/icon/library/src/procs/random.icn random] provides related procedures including a parametrized  LCRNG that defaults to the built-in values.


## Io

Io's [http://iolanguage.org/scm/io/docs/reference/index.html#/Math/Random/Random Random object] uses the Mersenne Twister algorithm.


## Inform 7

Inform's random functions are built on the random number generator exposed at runtime by the virtual machine, which is implementation-defined.


## J

By default J's <code>?</code> primitive (Roll/Deal) uses the Mersenne twister algorithm, but can be set to use a number of other algorithms as detailed on the [http://www.jsoftware.com/help/dictionary/d640.htm J Dictionary page for Roll/Deal].


## Java

Java's <code>Random</code> class uses a [[wp:Linear congruential generator|Linear congruential formula]], as described in [http://java.sun.com/javase/6/docs/api/java/util/Random.html its documentation]. The commonly used <code>Math.random()</code> uses a <code>Random</code> object under the hood.


## JavaScript

The only built-in random number generation facility is <code>Math.random()</code>, which returns a floating-point number greater than or equal to 0 and less than 1, with approximately uniform distribution. The standard (ECMA-262) does not specify what algorithm is to be used.


## Julia

Julia's [http://docs.julialang.org/en/latest/stdlib/base/#random-numbers built-in random-number generation functions], <code>rand()</code> etcetera, use the Mersenne Twister algorithm.


## Kotlin

As mentioned in the Java entry, the java.util.Random class uses a linear congruential formula and is not therefore cryptographically secure. However, there is also a derived class, java.security.SecureRandom, which can be used for cryptographic purposes


## Lua

Lua's <code>math.random()</code> is an interface to the C <code>rand()</code> function provided by the OS libc; its implementation varies by platform.


## Mathematica

Mathematica 7, by default, uses an Extended Cellular Automaton method ("ExtendedCA") to generate random numbers. The main PRNG functions are <code>RandomReal[]</code> and <code>RandomInteger[]</code> You can specify alternative generation methods including the Mersenne Twister and a Linear Congruential Generator (the default earlier versions). Information about random number generation is provided at [http://reference.wolfram.com/mathematica/tutorial/RandomNumberGeneration.html#185956823 Mathematica].


## MATLAB

MATLAB uses the Mersenne Twister as its default random number generator. Information about how the "rand()" function is utilized is given at [http://www.mathworks.com/help/techdoc/ref/rand.html MathWorks].


## Maxima

Maxima uses a Lisp implementation of the Mersenne Twister. See <code>? random</code> for help, or file <code>share/maxima/5.28.0-2/src/rand-mt19937.lisp</code> for the source code.

There are also random generators for several [[wp:Probability distribution|distributions]] in package <code>distrib</code> :
* <code>[[wp:Bernoulli distribution|random_bernoulli]]</code>
* <code>[[wp:Beta distribution|random_beta]]</code>
* <code>[[wp:Binomial distribution|random_binomial]]</code>
* <code>[[wp:Cauchy distribution|random_cauchy]]</code>
* <code>[[wp:Chi-squared distribution|random_chi2]]</code>
* <code>[[wp:Uniform distribution (continuous)|random_continuous_uniform]]</code>
* <code>[[wp:Uniform distribution (discrete)|random_discrete_uniform]]</code>
* <code>[[wp:Exponential distribution|random_exp]]</code>
* <code>[[wp:F-distribution|random_f]]</code>
* <code>[[wp:Gamma distribution|random_gamma]]</code>
* <code>[[wp:Categorical distribution|random_general_finite_discrete]]</code>
* <code>[[wp:Geometric distribution|random_geometric]]</code>
* <code>[[wp:Gumbel distribution|random_gumbel]]</code>
* <code>[[wp:Hypergeometric distribution|random_hypergeometric]]</code>
* <code>[[wp:Laplace distribution|random_laplace]]</code>
* <code>[[wp:Logistic distribution|random_logistic]]</code>
* <code>[[wp:Lognormal distribution|random_lognormal]]</code>
* <code>[[wp:Negative binomial distribution|random_negative_binomial]]</code>
* <code>[[wp:Noncentral chi-squared distribution|random_noncentral_chi2]]</code>
* <code>[[wp:Noncentral t-distribution|random_noncentral_student_t]]</code>
* <code>[[wp:Normal distribution|random_normal]]</code>
* <code>[[wp:Pareto distribution|random_pareto]]</code>
* <code>[[wp:Poisson distribution|random_poisson]]</code>
* <code>[[wp:Rayleigh distribution|random_rayleigh]]</code>
* <code>[[wp:Student's t-distribution|random_student_t]]</code>
* <code>[[wp:Weibull distribution|random_weibull]]</code>

Note: the package <code>distrib</code> also has functions starting with <code>pdf</code>, <code>cdf</code>, <code>quantile</code>, <code>mean</code>, <code>var</code>, <code>std</code>, <code>skewness</code> or <code>kurtosis</code> instead of <code>random</code>, except the Cauchy distribution, which does not have [[wp:Moment (mathematics)|moments]].

=={{header|Modula-3}}==
The Random interface in Modula-3 states that it uses "an additive generator based on Knuth's Algorithm 3.2.2A".


## Nemerle

Uses .Net Random class; so, as mentioned under C#, above, implements Knuth's subtractive random number generator algorithm.
Random class documentation at [http://msdn.microsoft.com/en-us/library/system.random.aspx#remarksToggle MSDN].


## NetRexx

As NetRexx runs in the JVM it simply leverages the Java library.  See [[#Java|Java]] for details of the algorithms used.


## Nim

There are two PRNGs provided in the standard library:
* '''random''' : Based on xoroshiro128+ (xor/rotate/shift/rotate), see [http://xoroshiro.di.unimi.it/ here].
* '''mersenne''' : The Mersenne Twister.


## OCaml

OCaml provides a module called [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html Random] in its standard library.  It used to be a "Linear feedback shift register" pseudo-random number generator (References: Robert Sedgewick, "Algorithms", Addison-Wesley). It is now (as of version 3.12.0) a "lagged-Fibonacci F(55, 24, +) with a modified addition function to enhance the mixing of bits."  It passes the Diehard test suite.


## Octave

As explained [https://www.gnu.org/software/octave/doc/interpreter/Special-Utility-Matrices.html#Special-Utility-Matrices here] (see '''rand''' function), Octave uses the "Mersenne Twister with a period of 2^19937-1".


## Oz

Oz provides a binding to the C <code>[http://www.opengroup.org/onlinepubs/000095399/functions/rand.html rand]</code> function as <code>[http://www.mozart-oz.org/home/doc/system/node56.html#label719 OS.rand]</code>.


## PARI/GP

<code>random</code> uses Richard Brent's [http://wwwmaths.anu.edu.au/~brent/random.html xorgens].  It's a member of the xorshift class of PRNGs and provides good, fast pseudorandomness (passing the BigCrush test, unlike the Mersenne twister), but it is not cryptographically strong. As implemented in PARI, its period is "at least <math>2^{4096}-1</math>".


```parigp
setrand(3)
random(6)+1
\\ chosen by fair dice roll.
\\ guaranteed to the random.
```



## Pascal

See [[#Delphi]] and [[#Free Pascal]].

Random functions:
  function  Random(l: LongInt) : LongInt;
  function  Random : Real;
  procedure Randomize;


## Perl

Previous to Perl 5.20.0 (May 2014), Perl's <code>[http://perldoc.perl.org/functions/rand.html rand]</code> function will try and call <code>[http://www.opengroup.org/onlinepubs/007908775/xsh/drand48.html drand48]</code>, <code>[http://www.opengroup.org/onlinepubs/000095399/functions/random.html random]</code> or <code>[http://www.opengroup.org/onlinepubs/000095399/functions/rand.html rand]</code> from the C library <code>[http://www.opengroup.org/onlinepubs/000095399/basedefs/stdlib.h.html stdlib.h]</code> in that order.

Beginning with Perl 5.20.0, a drand48() implementation is built into Perl and used on all platforms.  The implementation is from FreeBSD and uses a 48-bit linear congruential generator with this formula:

* <math>r_{n + 1} = 25214903917 \times r_n + 11 \pmod {2^{48}}</math>
Seeds for drand48 are 32-bit and the initial seed uses 4 bytes of data read from /dev/urandom if possible; a 32-bit mix of various system values otherwise.

Additionally, there are many PRNG's available as modules.  Two good Mersenne Twister modules are [https://metacpan.org/pod/Math::Random::MTwist Math::Random::MTwist] and [https://metacpan.org/pod/Math::Random::MT::Auto Math::Random::MT::Auto].  Modules supporting other distributions can be found in [https://metacpan.org/pod/Math::Random Math::Random] and [https://metacpan.org/pod/Math::GSL::Randist Math::GSL::Randist] among others.  CSPRNGs include [https://metacpan.org/pod/Bytes::Random::Secure Bytes::Random::Secure], [https://metacpan.org/pod/Math::Random::Secure Math::Random::Secure], [https://metacpan.org/pod/Math::Random::ISAAC Math::Random::ISAAC], and many more.


## Perl 6

The implementation underlying the <tt>rand</tt> function is platform and VM dependent. The JVM backend uses that platform's SecureRandom class.


## Phix

The rand(n) routine returns an integer in the range 1 to n, and rnd() returns a floating point number between 0.0 and 1.0.

In both cases the underlying algorithm is just about as trivial as it can be, certainly not suitable for serious cryptographic work.

There are at least a couple of Mersenne twister components in the archive.


## PHP

PHP has two random number generators: <code>[http://us3.php.net/manual/en/function.rand.php rand]</code>, which uses the underlying C library's <code>rand</code> function; and <code>[http://us3.php.net/manual/en/function.mt-rand.php mt_rand]</code>, which uses the [[wp:Mersenne twister|Mersenne twister]] algorithm.


## PicoLisp

PicoLisp uses a linear congruential generator in the built-in (rand) function,
with a multiplier suggested in Knuth's "Seminumerical Algorithms". See the
[http://software-lab.de/doc/refR.html#rand documentation].


## PL/I

Values produced by IBM Visualage PL/I compiler
built-in random number generator are uniformly distributed
between 0 and 1 [0 &lt;= random &lt; 1]

It uses a multiplicative congruential method:

```PL/I
seed(x) = mod(950706376 * seed(x-1), 2147483647)
random(x) = seed(x) / 2147483647
```



## PL/SQL

Oracle Database has two packages that can be used for random numbers generation.

===DBMS_RANDOM===
The DBMS_RANDOM package provides a built-in random number generator. This package is not intended for cryptography.
It will automatically initialize with the date, user ID, and process ID if no explicit initialization is performed.
If this package is seeded twice with the same seed, then accessed in the same way, it will produce the same results in both cases.

```PL/SQL
DBMS_RANDOM.RANDOM --produces integers in [-2^^31, 2^^31).
DBMS_RANDOM.VALUE  --produces numbers in [0,1) with 38 digits of precision.
DBMS_RANDOM.NORMAL --produces normal distributed numbers with a mean of 0 and a variance of 1
```


===DBMS_CRYPTO===
The DBMS_CRYPTO package contains basic cryptographic functions and procedures.
The DBMS_CRYPTO.RANDOMBYTES function returns a RAW value containing a cryptographically secure
pseudo-random sequence of bytes, which can be used to generate random material for encryption keys.
This function is based on the RSA X9.31 PRNG (Pseudo-Random Number Generator).

```PL/SQL
DBMS_CRYPTO.RANDOMBYTES --returns RAW value
DBMS_CRYPTO.RANDOMINTEGER --produces integers in the BINARY_INTEGER datatype
DBMS_CRYPTO.RANDOMNUMBER --produces integer in the NUMBER datatype in the range of [0..2**128-1]
```



## PowerShell

The [http://technet.microsoft.com/en-us/library/dd315402.aspx <code>Get-Random</code>] cmdlet (part of PowerShell 2) uses the .NET-supplied pseudo-random number generator which uses Knuth's subtractive method; see [[#C#|C#]].


## PureBasic

PureBasic has two random number generators, <tt>Random()</tt> and <tt>CryptRandom()</tt>.  <tt>Random()</tt> uses a RANROT type W generator [http://www.agner.org/random/theory/chaosran.pdf]. <tt>CryptRandom()</tt> uses a very strong PRNG that makes use of a cryptographic safe random number generator for its 'seed', and refreshes the seed if such data is available.  The exact method used for <tt>CryptRandom()</tt> is uncertain.


## Python

Python uses the [[wp:Mersenne twister|Mersenne twister]] algorithm accessed via the built-in [http://docs.python.org/library/random.html random module].


## R

For uniform random numbers, R may use Wichmann-Hill, Marsaglia-multicarry, Super-Duper, Mersenne-Twister, or Knuth-TAOCP
(both 1997 and 2002 versions), or a user-defined method. The default is Mersenne Twister.

R is able to generate random numbers from a variety of distributions, e.g.

# Beta
# Binomial
# Cauchy
# Chi-Squared
# Exponential
# F
# Gamma
# Geometric
# Hypergeometric
# Logistic
# Log Normal
# Multinomial
# Negative Binomial
# Normal
# Poisson
# Student t
# Uniform
# Weibull

See R help on [http://pbil.univ-lyon1.fr/library/base/html/Random.html Random number generation], or in the R system type

```R
?RNG
help.search("Distribution", package="stats")
```



## Racket


Racket's random number generator uses a 54-bit version of L’Ecuyer’s MRG32k3a algorithm [L'Ecuyer02],
as specified in the [http://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._random%29%29 docs].
In addition, the "math" library has a bunch of additional
[http://docs.racket-lang.org/math/base.html#%28part._.Random_.Number_.Generation%29 random functions].


## Rascal

Rascal does not have its own arbitrary number generator, but uses the [[Random_number_generator_(included)#Java | Java]]  generator. Nonetheless, you can redefine the arbitrary number generator if needed. Rascal has the following functions connected to the random number generator:

```rascal
import util::Math;
arbInt(int limit); // generates an arbitrary integer below limit
arbRat(int limit, int limit); // generates an arbitrary rational number between the limits
arbReal(); // generates an arbitrary real value in the interval [0.0, 1.0]
arbSeed(int seed);
```

The last function can be used to redefine the arbitrary number generator. This function is also used in the getOneFrom() functions.

```rascal>rascal
import List;
ok
rascal>getOneFrom(["zebra", "elephant", "snake", "owl"]);
str: "owl"

```



## REXX

The   RANDOM   BIF function is a pseudo-random number (non-negative integer) generator,
with a range (spread) limited to   100,000   (but some REXX interpreters support a larger range).


The random numbers generated are not consistent between different REXX interpreters or

even the same REXX interpreters executing on different hardware.

```rexx
     /*(below)  returns a random integer between 100 & 200, inclusive.*/

y = random(100, 200)
```

The random numbers may be repeatable by specifiying a   ''seed''   for the   '''random'''   BIF:

```rexx
call random ,,44    /*the seed in this case is "44". */
  .
  .
  .
y = random(100, 200)
```


Comparison of '''random''' BIF output for different REXX implementations using a deterministic ''seed''.

```rexx
/* REXX ***************************************************************
* 08.09.2013 Walter Pachl
*            Please add the output from other REXXes
* 10.09.2013 Walter Pachl added REXX/TSO
* 01.08.2014 Walter Pachl show what ooRexx supports
**********************************************************************/
Parse Version v
Call random ,,44
ol=v':'
Do i=1 To 10
  ol=ol random(1,10)
  End
If left(v,11)='REXX-ooRexx' Then
  ol=ol random(-999999999,0) /* ooRexx supports negative limits */
Say ol
```

'''outputs''' from various REXX interpreters:

```txt

REXX-ooRexx_4.1.3(MT) 6.03 4 Jul 2013: 3 10 6 8 6 9 9 1 1 6
REXX-ooRexx_4.2.0(MT)_32-bit 6.04 22 Feb 2014: 3 10 6 8 6 9 9 1 1 6 -403019526
REXX/Personal 4.00 21 Mar 1992: 7 7 6 7 8 8 5 9 4 7
REXX-r4 4.00 17 Aug 2013: 8 10 7 5 4 2 10 5 2 4
REXX-roo 4.00 28 Jan 2007: 8 10 7 5 4 2 10 5 2 4
REXX-Regina_3.7(MT) 5.00 14 Oct 2012: 10 2 7 10 1 1 8 2 4 1
are the following necessary??
REXX-Regina_3.4p1 (temp bug fix sf.org 1898218)(MT) 5.00 21 Feb 2008: 10 2 7 10 1 1 8 2 4 1
REXX-Regina_3.2(MT) 5.00 25 Apr 2003: 10 2 7 10 1 1 8 2 4 1
REXX-Regina_3.3(MT) 5.00 25 Apr 2004: 10 2 7 10 1 1 8 2 4 1
REXX-Regina_3.4(MT) 5.00 30 Dec 2007: 10 2 7 10 1 1 8 2 4 1
REXX-Regina_3.5(MT) 5.00 31 Dec 2009: 10 2 7 10 1 1 8 2 4 1
REXX-Regina_3.6(MT) 5.00 31 Dec 2011: 10 2 7 10 1 1 8 2 4 1
REXX370 3.48 01 May 1992: 8 7 3 1 6 5 5 8 3 2

```

Conclusion: It's not safe to transport a program that uses 'reproducable' use of random-bif (i.e. with a seed) from one environment/implementation to another :-(


## Ring


```ring

nr = 10
for i = 1 to nr
     see random(i) + nl
next

```



## Ruby

Ruby's <code>rand</code> function currently uses the [[wp:Mersenne twister|Mersenne twister]] algorithm, as described in [http://www.ruby-doc.org/core/classes/Kernel.html#M005974 its documentation].


## Run BASIC


```runbasic
rmd(0)
```
 - Return a pseudorandom value between 0 and 1


## Rust

Rust's <code>[https://crates.io/crates/rand rand]</code> crate offers several PRNGs.  (It is also available via <code>#![feature(rustc_private)]</code>).  The offering includes some cryptographically secure PRNGs: [https://docs.rs/rand/0.4/rand/isaac/index.html ISAAC] (both 32 and 64-bit variants) and [https://docs.rs/rand/0.4/rand/chacha/struct.ChaChaRng.html ChaCha20].  <code>StdRng</code> is a wrapper of one of those efficient on the current platform.  The crate also provides a weak PRNG: [https://docs.rs/rand/0.4/rand/struct.XorShiftRng.html Xorshift128].  It passes diehard but fails TestU01, replacement is being [https://github.com/dhardy/rand/issues/60 considered].  <code>[https://docs.rs/rand/0.4/rand/fn.thread_rng.html thread_rng]</code> returns a thread local <code>StdRng</code> initialized from the OS.  Other PRNGs can be created from the OS or with <code>thread_rng</code>.

For any other PRNGs not provided, they merely have to implement the <code>[https://docs.rs/rand/0.4/rand/trait.Rng.html Rng]</code> trait.


## Scala

Scala's <code>scala.util.Random</code> class uses a [[wp:Linear congruential generator|Linear congruential formula]] of the JVM run-time libary, as described in [http://java.sun.com/javase/6/docs/api/java/util/Random.html its documentation].
An example can be found here:

```scala
import scala.util.Random

/**
 * Histogram of 200 throws with two dices.
 */
object Throws extends App {
  Stream.continually(Random.nextInt(6) + Random.nextInt(6) + 2)
    .take(200).groupBy(identity).toList.sortBy(_._1)
    .foreach {
      case (a, b) => println(f"$a%2d:" + "X" * b.size)
    }
}
```

```txt
 2:XXX
 3:XXXXXXXXX
 4:XXXXXXXXXXXXX
 5:XXXXXXXXXXXXXXXXXXXXXXXXXX
 6:XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 7:XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 8:XXXXXXXXXXXXXXXXXXXXXXXXXXXX
 9:XXXXXXXXXXXXXXXXXXXXXXXXXXXX
10:XXXXXXXXXXXXXXXXX
11:XXXXXXXXXXXXXX
12:XX
```


## Seed7

Seed7 uses a linear congruential generator to compute pseudorandom numbers.
Usually random number generators deliver a random value in a fixed range,
The Seed7 function [http://seed7.sourceforge.net/libraries/integer.htm#rand%28in_integer,in_integer%29 rand(low, high)]
delivers a random number in the requested range [low, high].
Seed7 overloads the ''rand'' functions for the types char, boolean,
[http://seed7.sourceforge.net/libraries/bigint.htm#rand%28in_bigInteger,in_bigInteger%29 bigInteger],
[http://seed7.sourceforge.net/libraries/float.htm#rand%28ref_float,ref_float%29 float] and others.


## Sidef

Latest versions of Sidef use the Mersenne Twister algorithm to compute pseudorandom numbers, with different initial seeds (and implementations) for floating-points and integers.


```ruby
say 1.rand          # random float in the interval [0,1)
say 100.irand       # random integer in the interval [0,100)
```



## Sparkling

Sparkling uses the built-in PRNG of whichever C library implementation the interpreter is compiled against. The Sparkling library functions <tt>random()</tt> and <tt>seed()</tt> map directly to the C standard library functions <tt>rand()</tt> and <tt>srand()</tt> with only one small difference: the return value of <tt>rand()</tt> is divided by <tt>RAND_MAX</tt> so that the generated number is between 0 and 1.


## Stata


See '''[http://www.stata.com/help.cgi?set%20rng set rng]''' in Stata help. Stata uses the '''[https://en.wikipedia.org/wiki/Mersenne_Twister Mersenne Twister]''' RNG by default, and may use the 32-bit '''[https://en.wikipedia.org/wiki/KISS_(algorithm) KISS]''' RNG for compatibility with versions earlier than Stata 14.


## Tcl

Tcl uses a [[wp:Linear congruential generator|linear congruential generator]] in it's built-in <code>rand()</code> function. This is seeded by default from the system time, and kept per-interpreter so different security contexts and different threads can't affect each other's generators (avoiding key deployment issues with the <tt>rand</tt> function from [[C]]'s math library).

Citations (from Tcl source code):
*  S.K. Park & K.W. Miller, “''Random number generators: good ones are hard to find'',” Comm ACM 31(10):1192-1201, Oct 1988
*  W.H. Press & S.A. Teukolsky, “''Portable random number generators'',” Computers in Physics 6(5):522-524, Sep/Oct 1992.

=={{header|TI-83 BASIC}}==
TI-83 uses L'Ecuyer's algorithm to generate random numbers.
See [https://www.gnu.org/software/gsl/manual/html_node/Random-number-generator-algorithms.html L'Ecuyer's algorithm].
More explainations can be found in this [http://www.iro.umontreal.ca/~lecuyer/myftp/papers/handstat.pdf paper].

Random function:

```ti83b>rand</lang



## TXR

TXR 50 has a PRNG API, and uses a re-implementation of WELL 512 (avoiding contagion by the "contact authors for commercial uses" virus present in the reference implementation, which attacks BSD licenses). Mersenne Twister was a runner up. There is an object of type random-state, and a global variable *random-state* which holds the default random state. Programs can create random states which are snapshots of existing ones, or which are seeded using an integer value (which can be a bignum). The random function produces a random number modulo some integer value, which can have arbitrary precision. The random-fixnum function produces a non-heap-allocated positive integer with random bits.


## UNIX Shell


All '''Bourne Shell''' clones have a very quick pseudo random number generator.


```bash
 echo $RANDOM
```


Rach time $RANDOM is referenced it changes it's value (with it's maximum value 32767).


## Ursa

Standard Ursa defines the <code>ursa.util.random</code> type for random number generators and gives objects of this type a standard interface, but leaves the choice of algorithm up to the implementor.

Cygnus/X Ursa is written in Java and makes calls to java.util.Random, which uses a [[wp:Linear congruential generator|Linear congruential formula]].


## Ursala

Ursala uses the [[wp:Mersenne twister|Mersenne twister]] algorithm as implemented by the [http://www.basis.uklinux.net/avram Avram] run time system for most purposes, except for arbitrary precision floating point random numbers, which are generated by the <code>urandomb</code> function from the
[http://www.mpfr.org mpfr] library.


## XPL0

A 31-bit linear congruential generator is used based on an algorithm by
Donald Knuth in his book "Art of Computer Programming" Vol 2, 3rd ed. p.
185. It passes all tests in the Diehard suite. The seed is initialized
with the system timer count (at 046C) whenever a program starts. The seed
can also be set within a program to give a repeatable sequence of
(pseudo) random numbers. Calls to the random number intrinsic return
values modulo the argument.


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
int I;
[RanSeed(12345);        \set random number generator seed to 12345
for I:= 1 to 5 do
        [IntOut(0, Ran(1_000_000));  CrLf(0)];
]
```


Output:

```txt

905495
181227
755989
244883
213142

```



## zkl

zkl uses the Xorshift (http://en.wikipedia.org/wiki/Xorshift) random number generator. It will also, on occasion, read from /dev/urandom.


## ZX Spectrum Basic


The ZX Spectrum uses a Park-Miller (also called a Lehmer) number generator that produces a number between 0 and nearly 1 from a sequence; the RANDOMIZE command can leap to a new entry in the sequence. Multiply the output of RND by 65536 to see the sequence more clearly. The random numbers produced will repeat after 65536 iterations.





+++
title = "Multiplicative order"
description = ""
date = 2019-08-01T21:07:54Z
aliases = []
[extra]
id = 2400
[taxonomies]
categories = ["task", "Discrete math"]
tags = []
languages = [
  "ada",
  "algol_68",
  "c",
  "clojure",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "maple",
  "mathematica",
  "maxima",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "seed7",
  "sidef",
  "tcl",
  "zkl",
]
+++

The '''multiplicative order''' of ''a'' relative to ''m'' is the least positive integer ''n'' such that ''a^n'' is 1 (modulo ''m'').


;Example:
The multiplicative order of 37 relative to 1000 is 100 because 37^100 is 1 (modulo 1000), and no number smaller than 100 would do.


One possible algorithm that is efficient also for large numbers is the following: By the [[wp:Chinese_Remainder_Theorem|Chinese Remainder Theorem]], it's enough to calculate the multiplicative order for each prime exponent ''p^k'' of ''m'', and
combine the results with the ''[[least common multiple]]'' operation.

Now the order of ''a'' with regard to ''p^k'' must divide ''&Phi;(p^k)''. Call this number ''t'', and determine it's factors ''q^e''. Since each multiple of the order will also yield 1 when used as exponent for ''a'', it's enough to find the least d such that ''(q^d)*(t/(q^e))'' yields 1 when used as exponent.


## Task

Implement a routine to calculate the multiplicative order along these lines. You may assume that routines to determine the factorization into prime powers are available in some library.

----

An algorithm for the multiplicative order can be found in Bach & Shallit, <i>Algorithmic Number Theory, Volume I: Efficient Algorithms</i>, The MIT Press, 1996:

<p>Exercise 5.8, page 115:</p>

<p>Suppose you are given a prime<tt> p </tt>and a complete factorization
of<tt> p-1</tt>.   Show how to compute the order of an
element<tt> a </tt>in<tt> (Z/(p))<sup>*</sup> </tt>using<tt> O((lg p)<sup>4</sup>/(lg lg p)) </tt>bit
operations.</p>

<p>Solution, page 337:</p>

<p>Let the prime factorization of<tt> p-1 </tt> be<tt> q1<sup>e1</sup>q2<sup>e2</sup>...qk<sup>ek</sup></tt> .<tt> </tt>We use the following observation:
if<tt> x^((p-1)/qi<sup>fi</sup>) = 1 (mod p)</tt> ,<tt> </tt>
and<tt> fi=ei </tt>or<tt> x^((p-1)/qi<sup>fi+1</sup>) != 1 (mod p)</tt> ,<tt> </tt>then<tt> qi<sup>ei-fi</sup>||ord<sub>p</sub> x</tt>.   (This follows by combining Exercises 5.1 and 2.10.)

Hence it suffices to find, for each<tt> i</tt> ,<tt> </tt>the exponent<tt> fi </tt> such that the condition above holds.</p>

<p>This can be done as follows: first compute<tt> q1<sup>e1</sup>, q2<sup>e2</sup>, ... ,
qk<sup>ek</sup></tt> .<tt> </tt> This can be done using<tt> O((lg p)<sup>2</sup>) </tt>bit operations. Next, compute<tt> y1=(p-1)/q1<sup>e1</sup>, ... , yk=(p-1)/qk<sup>ek</sup></tt> .<tt> </tt>
This can be done using<tt> O((lg p)<sup>2</sup>) </tt>bit operations. Now, using the binary method,
compute<tt> x1=a<sup>y1</sup>(mod p), ... ,  xk=a<sup>yk</sup>(mod p) </tt>.<tt> </tt>
This can be done using<tt> O(k(lg p)<sup>3</sup>) </tt>bit operations, and<tt> k=O((lg p)/(lg lg p)) </tt>by Theorem 8.8.10.
Finally, for each<tt> i</tt> ,<tt> </tt>repeatedly raise<tt> xi </tt>to the<tt> qi</tt>-th power<tt> (mod p) </tt>(as many as<tt> ei-1 </tt> times), checking to see when 1 is obtained.
This can be done using<tt> O((lg p)<sup>3</sup>) </tt>steps.
The total cost is dominated by<tt> O(k(lg p)<sup>3</sup>)</tt> ,<tt> </tt>which is<tt> O((lg p)<sup>4</sup>/(lg lg p))</tt>.





## Ada

Instead of assuming a library call to factorize the modulus, we assume the caller of our Find_Order function has already factorized it. The Multiplicative_Order package is specified as follows ("multiplicative_order.ads").

```Ada
package Multiplicative_Order is

   type Positive_Array is array (Positive range <>) of Positive;

   function Find_Order(Element, Modulus: Positive) return Positive;
   -- naive algorithm
   -- returns the smallest I such that (Element**I) mod Modulus = 1

   function Find_Order(Element: Positive;
                       Coprime_Factors: Positive_Array) return Positive;
   -- faster algorithm for the same task
   -- computes the order of all Coprime_Factors(I)
   -- and returns their least common multiple
   -- this gives the same result as Find_Order(Element, Modulus)
   -- with Modulus being the product of all the Coprime_Factors(I)
   --
   -- preconditions: (1) 1 = GCD(Coprime_Factors(I), Coprime_Factors(J))
   --                    for all pairs I, J with I /= J
   --                (2) 1 < Coprime_Factors(I)   for all I

end Multiplicative_Order;
```


Here is the implementation ("multiplicative_order.adb"):


```Ada
package body Multiplicative_Order is

   function Find_Order(Element, Modulus: Positive) return Positive is

      function Power(Exp, Pow, M: Positive) return Positive is
         -- computes Exp**Pow mod M;
         -- note that Ada's native integer exponentiation "**" may overflow on
         -- computing Exp**Pow before ever computing the "mod M" part
         Result: Positive := 1;
         E: Positive := Exp;
         P: Natural := Pow;
      begin
         while P > 0 loop
            if P mod 2 = 1 then
               Result := (Result * E) mod M;
            end if;
            E := (E * E) mod M;
            P := P / 2;
         end loop;
         return Result;
      end Power;

   begin -- Find_Order(Element, Modulus)
      for I in 1 .. Modulus loop
         if Power(Element, I, Modulus) = 1 then
            return Positive(I);
         end if;
      end loop;
      raise Program_Error with
        Positive'Image(Element) &" is not coprime to" &Positive'Image(Modulus);
   end Find_Order;

   function Find_Order(Element: Positive;
                       Coprime_Factors: Positive_Array) return Positive is

         function GCD (A, B : Positive) return Integer is
            M : Natural := A;
            N : Natural := B;
            T : Natural;
         begin
            while N /= 0 loop
               T := M;
               M := N;
               N ;:= T mod N;
            end loop;
            return M;
         end GCD; -- from http://rosettacode.org/wiki/Least_common_multiple#Ada

         function LCM (A, B : Natural) return Integer is
         begin
            if A = 0 or B = 0 then
               return 0;
            end if;
            return abs (A * B) / Gcd (A, B);
         end LCM; -- from http://rosettacode.org/wiki/Least_common_multiple#Ada

         Result : Positive := 1;

   begin -- Find_Order(Element, Coprime_Factors)
      for I in Coprime_Factors'Range loop
         Result := LCM(Result, Find_Order(Element, Coprime_Factors(I)));
      end loop;
      return Result;
   end Find_Order;

end Multiplicative_Order;
```


This is a sample program using the Multiplicative_Order package:

```Ada
with Ada.Text_IO, Multiplicative_Order;

procedure Main is
   package IIO is new Ada.Text_IO.Integer_IO(Integer);
   use Multiplicative_Order;
begin
   IIO.Put(Find_Order(3,10));
   IIO.Put(Find_Order(37,1000));
   IIO.Put(Find_Order(37,10_000));
   IIO.Put(Find_Order(37, 3343));
   IIO.Put(Find_Order(37, 3344));
   -- IIO.Put(Find_Order( 2,1000));
     --would raise Program_Error, because there is no I with 2**I=1 mod 1000
   Ada.Text_IO.New_Line;
   IIO.Put(Find_Order(3, (2,5)));           --  3 *   5 = 10
   IIO.Put(Find_Order(37, (8, 125)));       --  8 * 125 = 1000
   IIO.Put(Find_Order(37, (16, 625)));      -- 16 * 625 = 10_000
   IIO.Put(Find_Order(37, (1 => 3343)));    -- 1-element-array: 3343 is a prime
   IIO.Put(Find_Order(37, (11, 19, 16)));   -- 11 * 19 * 16 = 3344

   -- this violates the precondition, because 8 and 2 are not coprime
   -- it gives an incorrect result
   IIO.Put(Find_Order(37, (11, 19, 8, 2)));
end Main;
```


The output from the sample program:

```txt

          4        100        500       1114         20
          4        100        500       1114         20         10

```



## ALGOL 68

<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has no FORMATted transput, also it generates a call to undefined C LONG externals }} -->

```algol68
MODE LOOPINT = INT;

MODE POWMODSTRUCT = LONG INT;
PR READ "prelude/pow_mod.a68" PR;

MODE SORTSTRUCT = LONG INT;
PR READ "prelude/sort.a68" PR;

MODE GCDSTRUCT = LONG INT;
PR READ "prelude/gcd.a68" PR;

PR READ "prelude/iterator.a68" PR;

PROC is prime = (LONG INT p)BOOL:
    ( p > 1 |#ANDF# ALL((YIELDBOOL yield)VOID: factored(p, (LONG INT f, LONG INT e)VOID: yield(f = p))) | FALSE );

FLEX[4]LONG INT prime list := (2,3,5,7);

OP +:= = (REF FLEX[]LONG INT lhs, LONG INT rhs)VOID: (
    [UPB lhs +1] LONG INT next lhs;
    next lhs[:UPB lhs] := lhs;
    lhs := next lhs;
    lhs[UPB lhs] := rhs
);

PROC primes = (PROC (LONG INT)VOID yield)VOID: (
    LONG INT p;
    FOR p index TO UPB prime list DO
        p:= prime list[p index];
        yield(p)
    OD;
    DO
        p +:= 2;
        WHILE NOT is prime(p) DO
            p +:= 2
        OD;
        prime list +:= p;
        yield(p)
    OD
);

PROC factored = (LONG INT in a, PROC (LONG INT,LONG INT)VOID yield)VOID: (
    LONG INT a := in a;
  # FOR          p IN  # primes( # DO #
       (LONG INT p)VOID:(
        LONG INT j := 0;
        WHILE a MOD p = 0 DO
            a := a % p;
            j +:= 1
        OD;
        IF j > 0 THEN yield (p,j) FI;
        IF a < p*p THEN done FI
      )
  # ) OD #  );
    done:
    IF a > 1 THEN yield (a,1) FI
);

PROC mult0rdr1 = (LONG INT a, p, e)LONG INT: (
    LONG INT m := p ** SHORTEN e;
    LONG INT t := (p-1)*(p**SHORTEN (e-1)); #  = Phi(p**e) where p prime #
    LONG INT q;
    FLEX[0]LONG INT qs := (1);
  # FOR          f0,f1 IN  # factored(t # DO #,
       (LONG INT f0,f1)VOID: (
            FLEX[SHORTEN((f1+1)*UPB qs)]LONG INT next qs;
            FOR j TO SHORTEN f1 + 1 DO
                FOR q index TO UPB qs DO
                    q := qs[q index];
                    next qs[(j-1)*UPB qs+q index] := q * f0**(j-1)
                OD
            OD;
            qs := next qs
        )
  #   OD # );
    VOID(in place shell sort(qs));

    FOR q index TO UPB qs DO
        q := qs[q index];
        IF pow mod(a,q,m)=1 THEN done FI
    OD;
    done:
    q
);

PROC reduce = (PROC (LONG INT,LONG INT)LONG INT diadic, FORLONGINT iterator, LONG INT initial value)LONG INT: (
  LONG INT out := initial value;
# FOR          next IN # iterator( # DO #
     (LONG INT next)VOID:
    out := diadic(out, next)
 # OD # );
  out
);

PROC mult order = (LONG INT a, LONG INT m)LONG INT: (
    PROC mofs = (YIELDLONGINT yield)VOID:(
      # FOR          p,          count IN # factored(m, # DO #
           (LONG INT p, LONG INT count)VOID:
            yield(mult0rdr1(a,p,count))
        )
  # OD #  );
    reduce(lcm, mofs, 1)
);

main:(
    FORMAT d = $g(-0)$;
    printf((d, mult order(37, 1000), $l$));        # 100 #
    LONG INT b := LENG 10**20-1;
    printf((d, mult order(2, b), $l$)); # 3748806900 #
    printf((d, mult order(17,b), $l$)); # 1499522760 #
    b := 100001;
    printf((d, mult order(54,b), $l$));
    printf((d, pow mod( 54, mult order(54,b),b), $l$));
    IF ANY( (YIELDBOOL yield)VOID: FOR r FROM 2 TO SHORTEN mult order(54,b)-1 DO yield(1=pow mod(54,r, b)) OD  )
    THEN
        printf(($g$, "Exists a power r < 9090 where pow mod(54,r,b) = 1", $l$))
    ELSE
        printf(($g$, "Everything checks.", $l$))
    FI
)
```

Output:

```txt

100
3748806900
1499522760
9090
1
Everything checks.

```



## C

Uses prime/factor functions from [[Factors of an integer#Prime factoring]].  This implementation is not robust because of integer overflows.  To properly deal with even moderately large numbers, an arbitrary precision integer package is a must.

```c
ulong mpow(ulong a, ulong p, ulong m)
{
	ulong r = 1;
	while (p) {
		if ((1 & p)) r = r * a % m;
		a = a * a % m;
		p >>= 1;
	}
	return r;
}

ulong ipow(ulong a, ulong p) {
	ulong r = 1;
	while (p) {
		if ((1 & p)) r = r * a;
		a *= a;
		p >>= 1;
	}
	return r;
}

ulong gcd(ulong m, ulong n)
{
	ulong t;
	while (m) { t = m; m = n % m; n = t; }
	return n;
}

ulong lcm(ulong m, ulong n)
{
	ulong g = gcd(m, n);
	return m / g * n;
}

ulong multi_order_p(ulong a, ulong p, ulong e)
{
	ulong fac[10000];
	ulong m = ipow(p, e);
	ulong t = m / p * (p - 1);
	int i, len = get_factors(t, fac);
	for (i = 0; i < len; i++)
		if (mpow(a, fac[i], m) == 1)
			return fac[i];
	return 0;
}

ulong multi_order(ulong a, ulong m)
{
	prime_factor pf[100];
	int i, len = get_prime_factors(m, pf);
	ulong res = 1;
	for (i = 0; i < len; i++)
		res = lcm(res, multi_order_p(a, pf[i].p, pf[i].e));
	return res;
}

int main()
{
	sieve();
	printf("%lu\n", multi_order(37, 1000));
	printf("%lu\n", multi_order(54, 100001));
	return 0;
}
```



## C++

```cpp
#include <algorithm>
#include <bitset>
#include <iostream>
#include <vector>

typedef unsigned long ulong;
std::vector<ulong> primes;

typedef struct {
    ulong p, e;
} prime_factor; /* prime, exponent */

void sieve() {
    /* 65536 = 2^16, so we can factor all 32 bit ints */
    constexpr int SIZE = 1 << 16;

    std::bitset<SIZE> bits;
    bits.flip(); // set all bits
    bits.reset(0);
    bits.reset(1);
    for (int i = 0; i < 256; i++) {
        if (bits.test(i)) {
            for (int j = i * i; j < SIZE; j += i) {
                bits.reset(j);
            }
        }
    }

    /* collect primes into a list. slightly faster this way if dealing with large numbers */
    for (int i = 0; i < SIZE; i++) {
        if (bits.test(i)) {
            primes.push_back(i);
        }
    }
}

auto get_prime_factors(ulong n) {
    std::vector<prime_factor> lst;
    ulong e, p;

    for (ulong i = 0; i < primes.size(); i++) {
        p = primes[i];
        if (p * p > n) break;
        for (e = 0; !(n % p); n /= p, e++);
        if (e) {
            lst.push_back({ p, e });
        }
    }

    if (n != 1) {
        lst.push_back({ n, 1 });
    }
    return lst;
}

auto get_factors(ulong n) {
    auto f = get_prime_factors(n);
    std::vector<ulong> lst{ 1 };

    size_t len2 = 1;
    /* L = (1); L = (L, L * p**(1 .. e)) forall((p, e)) */
    for (size_t i = 0; i < f.size(); i++, len2 = lst.size()) {
        for (ulong j = 0, p = f[i].p; j < f[i].e; j++, p *= f[i].p) {
            for (size_t k = 0; k < len2; k++) {
                lst.push_back(lst[k] * p);
            }
        }
    }

    std::sort(lst.begin(), lst.end());
    return lst;
}

ulong mpow(ulong a, ulong p, ulong m) {
    ulong r = 1;
    while (p) {
        if (p & 1) {
            r = r * a % m;
        }
        a = a * a % m;
        p >>= 1;
    }
    return r;
}

ulong ipow(ulong a, ulong p) {
    ulong r = 1;
    while (p) {
        if (p & 1) r *= a;
        a *= a;
        p >>= 1;
    }
    return r;
}

ulong gcd(ulong m, ulong n) {
    ulong t;
    while (m) {
        t = m;
        m = n % m;
        n = t;
    }
    return n;
}

ulong lcm(ulong m, ulong n) {
    ulong g = gcd(m, n);
    return m / g * n;
}

ulong multi_order_p(ulong a, ulong p, ulong e) {
    ulong m = ipow(p, e);
    ulong t = m / p * (p - 1);
    auto fac = get_factors(t);
    for (size_t i = 0; i < fac.size(); i++) {
        if (mpow(a, fac[i], m) == 1) {
            return fac[i];
        }
    }
    return 0;
}

ulong multi_order(ulong a, ulong m) {
    auto pf = get_prime_factors(m);
    ulong res = 1;
    for (size_t i = 0; i < pf.size(); i++) {
        res = lcm(res, multi_order_p(a, pf[i].p, pf[i].e));
    }
    return res;
}

int main() {
    sieve();

    printf("%lu\n", multi_order(37, 1000));   // expect 100
    printf("%lu\n", multi_order(54, 100001)); // expect 9090

    return 0;
}
```

```txt
100
9090
```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Threading;

namespace MultiplicativeOrder {
    // Taken from https://stackoverflow.com/a/33918233
    public static class PrimeExtensions {
        // Random generator (thread safe)
        private static ThreadLocal<Random> s_Gen = new ThreadLocal<Random>(
          () => {
              return new Random();
          }
        );

        // Random generator (thread safe)
        private static Random Gen {
            get {
                return s_Gen.Value;
            }
        }

        public static bool IsProbablyPrime(this BigInteger value, int witnesses = 10) {
            if (value <= 1)
                return false;

            if (witnesses <= 0)
                witnesses = 10;

            BigInteger d = value - 1;
            int s = 0;

            while (d % 2 == 0) {
                d /= 2;
                s += 1;
            }

            byte[] bytes = new byte[value.ToByteArray().LongLength];
            BigInteger a;

            for (int i = 0; i < witnesses; i++) {
                do {
                    Gen.NextBytes(bytes);

                    a = new BigInteger(bytes);
                }
                while (a < 2 || a >= value - 2);

                BigInteger x = BigInteger.ModPow(a, d, value);
                if (x == 1 || x == value - 1)
                    continue;

                for (int r = 1; r < s; r++) {
                    x = BigInteger.ModPow(x, 2, value);

                    if (x == 1)
                        return false;
                    if (x == value - 1)
                        break;
                }

                if (x != value - 1)
                    return false;
            }

            return true;
        }
    }

    static class Helper {
        public static BigInteger Sqrt(this BigInteger self) {
            BigInteger b = self;
            while (true) {
                BigInteger a = b;
                b = self / a + a >> 1;
                if (b >= a) return a;
            }
        }

        public static long BitLength(this BigInteger self) {
            BigInteger bi = self;
            long bitlength = 0;
            while (bi != 0) {
                bitlength++;
                bi >>= 1;
            }
            return bitlength;
        }

        public static bool BitTest(this BigInteger self, int pos) {
            byte[] arr = self.ToByteArray();
            int idx = pos / 8;
            int mod = pos % 8;
            if (idx >= arr.Length) {
                return false;
            }
            return (arr[idx] & (1 << mod)) > 0;
        }
    }

    class PExp {
        public PExp(BigInteger prime, int exp) {
            Prime = prime;
            Exp = exp;
        }

        public BigInteger Prime { get; }

        public int Exp { get; }
    }

    class Program {
        static void MoTest(BigInteger a, BigInteger n) {
            if (!n.IsProbablyPrime(20)) {
                Console.WriteLine("Not computed. Modulus must be prime for this algorithm.");
                return;
            }
            if (a.BitLength() < 100) {
                Console.Write("ord({0})", a);
            } else {
                Console.Write("ord([big])");
            }
            if (n.BitLength() < 100) {
                Console.Write(" mod {0} ", n);
            } else {
                Console.Write(" mod [big] ");
            }
            BigInteger mob = MoBachShallit58(a, n, Factor(n - 1));
            Console.WriteLine("= {0}", mob);
        }

        static BigInteger MoBachShallit58(BigInteger a, BigInteger n, List<PExp> pf) {
            BigInteger n1 = n - 1;
            BigInteger mo = 1;
            foreach (PExp pe in pf) {
                BigInteger y = n1 / BigInteger.Pow(pe.Prime, pe.Exp);
                int o = 0;
                BigInteger x = BigInteger.ModPow(a, y, BigInteger.Abs(n));
                while (x > 1) {
                    x = BigInteger.ModPow(x, pe.Prime, BigInteger.Abs(n));
                    o++;
                }
                BigInteger o1 = BigInteger.Pow(pe.Prime, o);
                o1 = o1 / BigInteger.GreatestCommonDivisor(mo, o1);
                mo = mo * o1;
            }
            return mo;
        }

        static List<PExp> Factor(BigInteger n) {
            List<PExp> pf = new List<PExp>();
            BigInteger nn = n;
            int e = 0;
            while (!nn.BitTest(e)) e++;
            if (e > 0) {
                nn = nn >> e;
                pf.Add(new PExp(2, e));
            }
            BigInteger s = nn.Sqrt();
            BigInteger d = 3;
            while (nn > 1) {
                if (d > s) d = nn;
                e = 0;
                while (true) {
                    BigInteger div = BigInteger.DivRem(nn, d, out BigInteger rem);
                    if (rem.BitLength() > 0) break;
                    nn = div;
                    e++;
                }
                if (e > 0) {
                    pf.Add(new PExp(d, e));
                    s = nn.Sqrt();
                }
                d = d + 2;
            }

            return pf;
        }

        static void Main(string[] args) {
            MoTest(37, 3343);
            MoTest(BigInteger.Pow(10, 100) + 1, 7919);
            MoTest(BigInteger.Pow(10, 1000) + 1, 15485863);
            MoTest(BigInteger.Pow(10, 10000) - 1, 22801763489);
            MoTest(1511678068, 7379191741);
            MoTest(3047753288, 2257683301);
        }
    }
}
```

```txt
ord(37) mod 3343 = 1114
ord([big]) mod 7919 = 3959
ord([big]) mod 15485863 = 15485862
ord([big]) mod 22801763489 = 22801763488
ord(1511678068) mod 7379191741 = 614932645
ord(3047753288) mod 2257683301 = 62713425
```



## D

```d
import std.bigint;
import std.random;
import std.stdio;

struct PExp {
    BigInt prime;
    int exp;
}

BigInt gcd(BigInt x, BigInt y) {
    if (y == 0) {
        return x;
    }
    return gcd(y, x % y);
}

/// https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method
BigInt modPow(BigInt b, BigInt e, BigInt n) {
    if (n == 1) return BigInt(0);
    BigInt result = 1;
    b = b % n;
    while (e > 0) {
        if (e % 2 == 1) {
            result = (result * b) % n;
        }
        e >>= 1;
        b = (b*b) % n;
    }
    return result;
}

BigInt pow(long b, long e) {
    return pow(BigInt(b), BigInt(e));
}
BigInt pow(BigInt b, BigInt e) {
    if (e == 0) {
        return BigInt(1);
    }

    BigInt result = 1;
    while (e > 1) {
        if (e % 2 == 0) {
            b *= b;
            e /= 2;
        } else {
            result *= b;
            b *= b;
            e = (e - 1) / 2;
        }
    }

    return b * result;
}

BigInt sqrt(BigInt self) {
    BigInt b = self;
    while (true) {
        BigInt a = b;
        b = self / a + a >> 1;
        if (b >= a) return a;
    }
}

long bitLength(BigInt self) {
    BigInt bi = self;
    long length;
    while (bi != 0) {
        length++;
        bi >>= 1;
    }
    return length;
}

PExp[] factor(BigInt n) {
    PExp[] pf;
    BigInt nn = n;
    int b = 0;
    int e = 1;
    while ((nn & e) == 0) {
        e <<= 1;
        b++;
    }
    if (b > 0) {
        nn = nn >> b;
        pf ~= PExp(BigInt(2), b);
    }
    BigInt s = nn.sqrt();
    BigInt d = 3;
    while (nn > 1) {
        if (d > s) d = nn;
        e = 0;
        while (true) {
            BigInt div, rem;
            nn.divMod(d, div, rem);
            if (rem.bitLength > 0) break;
            nn = div;
            e++;
        }
        if (e > 0) {
            pf ~= PExp(d, e);
            s = nn.sqrt();
        }
        d += 2;
    }

    return pf;
}

BigInt moBachShallit58(BigInt a, BigInt n, PExp[] pf) {
    BigInt n1 = n - 1;
    BigInt mo = 1;
    foreach(pe; pf) {
        BigInt y = n1 / pe.prime.pow(BigInt(pe.exp));
        int o = 0;
        BigInt x = a.modPow(y, n);
        while (x > 1) {
            x = x.modPow(pe.prime, n);
            o++;
        }
        BigInt o1 = pe.prime.pow(BigInt(o));
        o1 = o1 / gcd(mo, o1);
        mo = mo * o1;
    }
    return mo;
}

void moTest(ulong a, ulong n) {
    moTest(BigInt(a), n);
}
void moTest(BigInt a, ulong n) {
    // Commented out because the implementations tried all failed for the -2 and -3 tests.
    // if (!n.isProbablePrime()) {
        // writeln("Not computed. Modulus must be prime for this algorithm.");
        // return;
    // }
    if (a.bitLength < 100) {
        write("ord(", a, ")");
    } else {
        write("ord([big])");
    }
    write(" mod ", n, " ");
    BigInt nn = n;
    BigInt mob = moBachShallit58(a, nn, factor(nn - 1));
    writeln("= ", mob);
}

void main() {
    moTest(37, 3343);

    moTest(pow(10, 100) + 1, 7919);
    moTest(pow(10, 1000) + 1, 15485863);
    moTest(pow(10, 10000) - 1, 22801763489);

    moTest(1511678068, 7379191741);
    moTest(3047753288, 2257683301);
}
```

```txt
ord(37) mod 3343 = 1114
ord([big]) mod 7919 = 3959
ord([big]) mod 15485863 = 15485862
ord([big]) mod 22801763489 = 22801763488
ord(1511678068) mod 7379191741 = 614932645
ord(3047753288) mod 2257683301 = 62713425
```



## EchoLisp


```scheme

(require 'bigint)

;; factor-exp returns a list ((p k) ..) : a = p1^k1 * p2^k2 ..
(define (factor-exp a)
	(map (lambda (g) (list (first g) (length g)))
					(group* (prime-factors a))))

;; copied from Ruby
(define (_mult_order a p k  (x))
	(define pk (expt p k))
	(define t (* (1- p) (expt p (1- k))))
	(define r 1)
	(for [((q e) (factor-exp t))]
	    (set! x (powmod a (/ t (expt q e)) pk))
	    (while (!= x 1)
	    	(*= r q)
	    	(set! x (powmod x q pk))))
	r)

(define (order a m)
        "multiplicative order : (order a m) →  n : a^n = 1 (mod m)"
	(assert (= 1 (gcd a m)) "a and m must be coprimes")
	(define mopks (for/list [((p k)  (factor-exp m))] (_mult_order a p k)))
	(for/fold (n 1) ((mopk mopks)) (lcm n mopk)))

;; results
order 37 1000)
   → 100
(order (+ (expt 10 100) 1) 7919)
   → 3959
(order (+ (expt 10 1000) 1) 15485863)
   → 15485862

```



## Clojure

Translation of Julie, then revised to be more clojure idiomatic. It references some external modules for factoring and integer exponentiation.

```clojure
(defn gcd [a b]
   (if (zero? b)
      a
      (recur b (mod a b))))

(defn lcm [a b]
   (/ (* a b) (gcd a b)))

(def NaN  (Math/log -1))

(defn ord' [a [p e]]
   (let [m   (imath/expt p e)
         t   (* (quot m p) (dec p))]
            (loop [dv (factor/divisors t)]
               (let [d (first dv)]
                  (if (= (mmath/expm a d m) 1)
                     d
                     (recur (next dv)))))))

(defn ord [a n]
   (if (not= (gcd a n) 1)
      NaN
      (->>
         (factor/factorize n)
         (map (partial ord' a))
         (reduce lcm))))

```

```txt

user=> (ord 37 1000)
100

```



## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    moTest(big.NewInt(37), big.NewInt(3343))
    b := big.NewInt(100)
    moTest(b.Add(b.Exp(ten, b, nil), one), big.NewInt(7919))
    moTest(b.Add(b.Exp(ten, b.SetInt64(1000), nil), one), big.NewInt(15485863))
    moTest(b.Sub(b.Exp(ten, b.SetInt64(10000), nil), one),
        big.NewInt(22801763489))

    moTest(big.NewInt(1511678068), big.NewInt(7379191741))
    moTest(big.NewInt(3047753288), big.NewInt(2257683301))
}

func moTest(a, n *big.Int) {
    if a.BitLen() < 100 {
        fmt.Printf("ord(%v)", a)
    } else {
        fmt.Print("ord([big])")
    }
    if n.BitLen() < 100 {
        fmt.Printf(" mod %v ", n)
    } else {
        fmt.Print(" mod [big] ")
    }
    if !n.ProbablyPrime(20) {
        fmt.Println("not computed.  modulus must be prime for this algorithm.")
        return
    }
    fmt.Println("=", moBachShallit58(a, n, factor(new(big.Int).Sub(n, one))))
}

var one = big.NewInt(1)
var two = big.NewInt(2)
var ten = big.NewInt(10)

func moBachShallit58(a, n *big.Int, pf []pExp) *big.Int {
    n1 := new(big.Int).Sub(n, one)
    var x, y, o1, g big.Int
    mo := big.NewInt(1)
    for _, pe := range pf {
        y.Quo(n1, y.Exp(pe.prime, big.NewInt(pe.exp), nil))
        var o int64
        for x.Exp(a, &y, n); x.Cmp(one) > 0; o++ {
            x.Exp(&x, pe.prime, n)
        }
        o1.Exp(pe.prime, o1.SetInt64(o), nil)
        mo.Mul(mo, o1.Quo(&o1, g.GCD(nil, nil, mo, &o1)))
    }
    return mo
}

type pExp struct {
    prime *big.Int
    exp   int64
}

func factor(n *big.Int) (pf []pExp) {
    var e int64
    for ; n.Bit(int(e)) == 0; e++ {
    }
    if e > 0 {
        n.Rsh(n, uint(e))
        pf = []pExp{{big.NewInt(2), e}}
    }
    s := sqrt(n)
    q, r := new(big.Int), new(big.Int)
    for d := big.NewInt(3); n.Cmp(one) > 0; d.Add(d, two) {
        if d.Cmp(s) > 0 {
            d.Set(n)
        }
        for e = 0; ; e++ {
            q.QuoRem(n, d, r)
            if r.BitLen() > 0 {
                break
            }
            n.Set(q)
        }
        if e > 0 {
            pf = append(pf, pExp{new(big.Int).Set(d), e})
            s = sqrt(n)
        }
    }
    return
}

func sqrt(n *big.Int) *big.Int {
    a := new(big.Int)
    for b := new(big.Int).Set(n); ; {
        a.Set(b)
        b.Rsh(b.Add(b.Quo(n, a), a), 1)
        if b.Cmp(a) >= 0 {
            return a
        }
    }
    return a.SetInt64(0)
}
```

```txt

ord(37) mod 3343 = 1114
ord([big]) mod 7919 = 3959
ord([big]) mod 15485863 = 15485862
ord([big]) mod 22801763489 = 22801763488
ord(1511678068) mod 7379191741 = 614932645
ord(3047753288) mod 2257683301 = 62713425

```



## Haskell


Assuming a function


```haskell
powerMod
  :: (Integral a, Integral b)
  => a -> a -> b -> a
powerMod m _ 0 = 1
powerMod m x n
  | n > 0 = f x_ (n - 1) x_
  where
    x_ = x `rem` m
    f _ 0 y = y
    f a d y = g a d
      where
        g b i
          | even i = g (b * b `rem` m) (i `quot` 2)
          | otherwise = f b (i - 1) (b * y `rem` m)
powerMod m _ _ = error "powerMod: negative exponent"
```


to efficiently calculate powers modulo some <code>Integral</code>, we get


```haskell
import Data.List (foldl1') --'

foldl1_ = foldl1' --'

multOrder a m
  | gcd a m /= 1 = error "Arguments not coprime"
  | otherwise = foldl1_ lcm $ map (multOrder_ a) $ primeFacsExp m

multOrder_ a (p, k) = r
  where
    pk = p ^ k
    t = (p - 1) * p ^ (k - 1) -- totient \Phi(p^k)
    r = product $ map find_qd $ primeFacsExp t
    find_qd (q, e) = q ^ d
      where
        x = powerMod pk a (t `div` (q ^ e))
        d = length $ takeWhile (/= 1) $ iterate (\y -> powerMod pk y q) x
```



## J


The dyadic verb ''mo'' converts its arguments to exact numbers ''a'' and ''m'', executes ''mopk'' on the factorization of ''m'', and combines the result with the ''least common multiple'' operation.


```j
mo=: 4 : 0
 a=. x: x
 m=. x: y
 assert. 1=a+.m
 *./ a mopk"1 |: __ q: m
)
```


The dyadic verb ''mopk'' expects a pair of prime and exponent
in the second argument. It sets up a verb ''pm'' to calculate
powers module ''p^k''. Then calculates ''&Phi;(p^k)'' as ''t'',
factorizes it again into ''q'' and ''e'', and calculates
''a^(t/(q^e))'' as ''x''. Now, it finds the least ''d'' such that subsequent application of ''pm'' yields ''1''. Finally, it combines the
exponents ''q^d'' into a product.


```j
mopk=: 4 : 0
 a=. x: x
 'p k'=. x: y
 pm=. (p^k)&|@^
 t=. (p-1)*p^k-1  NB. totient
 'q e'=. __ q: t
 x=. a pm t%q^e
 d=. (1<x)+x (pm i. 1:)&> (e-1) */\@$&.> q
 */q^d
)
```


For example:


```j
   37 mo 1000
100
   2 mo _1+10^80x
190174169488577769580266953193403101748804183400400
```



## Java

```Java
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class MultiplicativeOrder {
    private static final BigInteger ONE = BigInteger.ONE;
    private static final BigInteger TWO = BigInteger.valueOf(2);
    private static final BigInteger THREE = BigInteger.valueOf(3);
    private static final BigInteger TEN = BigInteger.TEN;

    private static class PExp {
        BigInteger prime;
        long exp;

        PExp(BigInteger prime, long exp) {
            this.prime = prime;
            this.exp = exp;
        }
    }

    private static void moTest(BigInteger a, BigInteger n) {
        if (!n.isProbablePrime(20)) {
            System.out.println("Not computed. Modulus must be prime for this algorithm.");
            return;
        }
        if (a.bitLength() < 100) System.out.printf("ord(%s)", a);
        else System.out.print("ord([big])");
        if (n.bitLength() < 100) System.out.printf(" mod %s ", n);
        else System.out.print(" mod [big] ");
        BigInteger mob = moBachShallit58(a, n, factor(n.subtract(ONE)));
        System.out.println("= " + mob);
    }

    private static BigInteger moBachShallit58(BigInteger a, BigInteger n, List<PExp> pf) {
        BigInteger n1 = n.subtract(ONE);
        BigInteger mo = ONE;
        for (PExp pe : pf) {
            BigInteger y = n1.divide(pe.prime.pow((int) pe.exp));
            long o = 0;
            BigInteger x = a.modPow(y, n.abs());
            while (x.compareTo(ONE) > 0) {
                x = x.modPow(pe.prime, n.abs());
                o++;
            }
            BigInteger o1 = BigInteger.valueOf(o);
            o1 = pe.prime.pow(o1.intValue());
            o1 = o1.divide(mo.gcd(o1));
            mo = mo.multiply(o1);
        }
        return mo;
    }

    private static List<PExp> factor(BigInteger n) {
        List<PExp> pf = new ArrayList<>();
        BigInteger nn = n;
        Long e = 0L;
        while (!nn.testBit(e.intValue())) e++;
        if (e > 0L) {
            nn = nn.shiftRight(e.intValue());
            pf.add(new PExp(TWO, e));
        }
        BigInteger s = sqrt(nn);
        BigInteger d = THREE;
        while (nn.compareTo(ONE) > 0) {
            if (d.compareTo(s) > 0) d = nn;
            e = 0L;
            while (true) {
                BigInteger[] qr = nn.divideAndRemainder(d);
                if (qr[1].bitLength() > 0) break;
                nn = qr[0];
                e++;
            }
            if (e > 0L) {
                pf.add(new PExp(d, e));
                s = sqrt(nn);
            }
            d = d.add(TWO);
        }
        return pf;
    }

    private static BigInteger sqrt(BigInteger n) {
        BigInteger b = n;
        while (true) {
            BigInteger a = b;
            b = n.divide(a).add(a).shiftRight(1);
            if (b.compareTo(a) >= 0) return a;
        }
    }

    public static void main(String[] args) {
        moTest(BigInteger.valueOf(37), BigInteger.valueOf(3343));

        BigInteger b = TEN.pow(100).add(ONE);
        moTest(b, BigInteger.valueOf(7919));

        b = TEN.pow(1000).add(ONE);
        moTest(b, BigInteger.valueOf(15485863));

        b = TEN.pow(10000).subtract(ONE);
        moTest(b, BigInteger.valueOf(22801763489L));

        moTest(BigInteger.valueOf(1511678068), BigInteger.valueOf(7379191741L));
        moTest(BigInteger.valueOf(3047753288L), BigInteger.valueOf(2257683301L));
    }
}
```

```txt
ord(37) mod 3343 = 1114
ord([big]) mod 7919 = 3959
ord([big]) mod 15485863 = 15485862
ord([big]) mod 22801763489 = 22801763488
ord(1511678068) mod 7379191741 = 614932645
ord(3047753288) mod 2257683301 = 62713425
```



## Julia

(Uses the <code>factors</code> function from [[Factors of an integer#Julia]].)

```julia
using Primes

function factors(n)
    f = [one(n)]
    for (p,e) in factor(n)
        f = reduce(vcat, [f*p^j for j in 1:e], init=f)
    end
    return length(f) == 1 ? [one(n), n] : sort!(f)
end

function multorder(a, m)
    gcd(a,m) == 1 || error("$a and $m are not coprime")
    res = one(m)
    for (p,e) in factor(m)
        m = p^e
        t = div(m, p) * (p-1)
        for f in factors(t)
            if powermod(a, f, m) == 1
                res = lcm(res, f)
                break
            end
        end
    end
    res
end
```


Example output (using <code>big</code> to employ arbitrary-precision arithmetic where needed):

```txt

julia> multorder(37, 1000)
100

julia> multorder(big(10)^100 + 1, 7919)
3959

julia> multorder(big(10)^1000 + 1, 15485863)
15485862

julia> multorder(big(10)^10000 - 1, 22801763489)
22801763488

```



## Kotlin

```scala
// version 1.2.10

import java.math.BigInteger

val bigOne   = BigInteger.ONE
val bigTwo   = 2.toBigInteger()
val bigThree = 3.toBigInteger()
val bigTen   = BigInteger.TEN

class PExp(val prime: BigInteger, val exp: Long)

fun moTest(a: BigInteger, n: BigInteger) {
    if (!n.isProbablePrime(20)) {
        println("Not computed. Modulus must be prime for this algorithm.")
        return
    }
    if (a.bitLength() < 100) print("ord($a)") else print("ord([big])")
    if (n.bitLength() < 100) print(" mod $n ") else print(" mod [big] ")
    val mob = moBachShallit58(a, n, factor(n - bigOne))
    println("= $mob")
}

fun moBachShallit58(a: BigInteger, n: BigInteger, pf: List<PExp>): BigInteger {
    val n1 = n - bigOne
    var mo = bigOne
    for (pe in pf) {
        val y = n1 / pe.prime.pow(pe.exp.toInt())
        var o = 0L
        var x = a.modPow(y, n.abs())
        while (x > bigOne) {
            x = x.modPow(pe.prime, n.abs())
            o++
        }
        var o1 = o.toBigInteger()
        o1 = pe.prime.pow(o1.toInt())
        o1 /= mo.gcd(o1)
        mo *= o1
    }
    return mo
}

fun factor(n: BigInteger): List<PExp> {
    val pf = mutableListOf<PExp>()
    var nn = n
    var e = 0L
    while (!nn.testBit(e.toInt())) e++
    if (e > 0L) {
        nn = nn shr e.toInt()
        pf.add(PExp(bigTwo, e))
    }
    var s = bigSqrt(nn)
    var d = bigThree
    while (nn > bigOne) {
        if (d > s) d = nn
        e = 0L
        while (true) {
            val (q, r) = nn.divideAndRemainder(d)
            if (r.bitLength() > 0) break
            nn = q
            e++
        }
        if (e > 0L) {
            pf.add(PExp(d, e))
            s = bigSqrt(nn)
        }
        d += bigTwo
    }
    return pf
}

fun bigSqrt(n: BigInteger): BigInteger {
    var b = n
    while (true) {
        val a = b
        b = (n / a + a) shr 1
        if (b >= a) return a
    }
}

fun main(args: Array<String>) {
    moTest(37.toBigInteger(), 3343.toBigInteger())

    var b = bigTen.pow(100) + bigOne
    moTest(b, 7919.toBigInteger())

    b = bigTen.pow(1000) + bigOne
    moTest(b, BigInteger("15485863"))

    b = bigTen.pow(10000) - bigOne
    moTest(b, BigInteger("22801763489"))

    moTest(BigInteger("1511678068"), BigInteger("7379191741"))
    moTest(BigInteger("3047753288"), BigInteger("2257683301"))
}
```


```txt

ord(37) mod 3343 = 1114
ord([big]) mod 7919 = 3959
ord([big]) mod 15485863 = 15485862
ord([big]) mod 22801763489 = 22801763488
ord(1511678068) mod 7379191741 = 614932645
ord(3047753288) mod 2257683301 = 62713425

```



## Maple


```Maple
numtheory:-order( a, n )
```

For example,

```Maple>
 numtheory:-order( 37, 1000 );
                       100
```



## Mathematica

In Mathematica this is really easy, as this function is built-in:
MultiplicativeOrder[k,n] gives the multiplicative order of k modulo n, defined as the smallest integer m such that k^m == 1 mod n.

MultiplicativeOrder[k,n,{r1,r2,...}] gives the generalized multiplicative order of k modulo n, defined as the smallest integer m such that k^m==ri mod n for some i.<BR>
Examples:

```Mathematica
MultiplicativeOrder[37, 1000]
MultiplicativeOrder[10^100 + 1, 7919]        (*10^3th prime number  Prime[1000]*)
MultiplicativeOrder[10^1000 + 1, 15485863]       (*10^6th prime number*)
MultiplicativeOrder[10^10000 - 1, 22801763489]       (*10^9th prime number*)
MultiplicativeOrder[13, 1 + 10^80]
MultiplicativeOrder[11, 1 + 10^100]
```

gives back:

```txt
100
3959
15485862
22801763488
109609547199756140150989321269669269476675495992554276140800
2583496112724752500580158969425549088007844580826869433740066152289289764829816356800
```



## Maxima


```maxima
zn_order(37, 1000);
/* 100 */

zn_order(10^100 + 1, 7919);
/* 3959 */

zn_order(10^1000 + 1, 15485863);
/* 15485862 */

zn_order(10^10000 - 1, 22801763489);
/* 22801763488 */

zn_order(13, 1 + 10^80);
/* 109609547199756140150989321269669269476675495992554276140800 */

zn_order(11, 1 + 10^100);
/* 2583496112724752500580158969425549088007844580826869433740066152289289764829816356800 */
```



## PARI/GP


```parigp
znorder(Mod(a,n))
```



## Perl

Using modules:
```perl
use ntheory qw/znorder/;
say znorder(54, 100001);
use bigint; say znorder(11, 1 + 10**100);
```

or

```perl
use Math::Pari qw/znorder Mod/;
say znorder(Mod(54, 100001));
say znorder(Mod(11, 1 + Math::Pari::PARI(10)**100));
```



## Perl 6


```perl6
my @primes := 2, |grep *.is-prime, (3,5,7...*);

sub factor($a is copy) {
    gather {
        for @primes -> $p {
            my $j = 0;
            while $a %% $p {
                $a div= $p;
                $j++;
            }
            take $p => $j if $j > 0;
            last if $a < $p * $p;
        }

        take $a => 1 if $a > 1;
    }
}

sub mo-prime($a, $p, $e) {
    my $m = $p ** $e;
    my $t = ($p - 1) * ($p ** ($e - 1)); #  = Phi($p**$e) where $p prime
    my @qs = 1;
    for factor($t) -> $f {
        @qs = flat @qs.map(-> $q { (0..$f.value).map(-> $j { $q * $f.key ** $j }) });
    }

    @qs.sort.first(-> $q { expmod( $a, $q, $m ) == 1});
}

sub mo($a, $m) {
    $a gcd $m == 1 || die "$a and $m are not relatively prime";
    [lcm] flat 1, factor($m).map(-> $r { mo-prime($a, $r.key, $r.value) });
}

sub MAIN("test") {
    use Test;

    for (10, 21, 25, 150, 1231, 123141, 34131) -> $n {
        is ([*] factor($n).map(-> $pair { $pair.key ** $pair.value })), $n, "$n factors correctly";
    }

    is mo(37, 1000), 100, 'mo(37,1000) == 100';
    my $b = 10**20-1;
    is mo(2, $b), 3748806900, 'mo(2,10**20-1) == 3748806900';
    is mo(17, $b), 1499522760, 'mo(17,10**20-1) == 1499522760';
    $b = 100001;
    is mo(54, $b), 9090, 'mo(54,100001) == 9090';
}
```

```txt
ok 1 - 10 factors correctly
ok 2 - 21 factors correctly
ok 3 - 25 factors correctly
ok 4 - 150 factors correctly
ok 5 - 1231 factors correctly
ok 6 - 123141 factors correctly
ok 7 - 34131 factors correctly
ok 8 - mo(37,1000) == 100
ok 9 - mo(2,10**20-1) == 3748806900
ok 10 - mo(17,10**20-1) == 1499522760
ok 11 - mo(54,100001) == 9090
```



## Phix

```Phix
include mpfr.e

procedure multi_order(mpz res, a, sequence p_and_k)
    mpz pk = mpz_init(),
        t = mpz_init(),
        x = mpz_init(),
        q = mpz_init()
    mpz_set_si(res,1)
    if length(p_and_k)=1 then
        string {ps} = p_and_k
        mpz_set_str(pk,ps)
        mpz_sub_ui(t,pk,1)
    else
        atom {p, k} = p_and_k
        mpz_ui_pow_ui(pk,p,k)
        mpz_ui_pow_ui(t,p,k-1)
        mpz_mul_si(t,t,p-1)
    end if
    sequence pf = mpz_prime_factors(t)
    for i=1 to length(pf) do
        if length(pf[i])=1 then
            string {fs} = pf[i]
            mpz_set_str(q,fs)
            mpz_set(x,q)
        else
            {integer qi, integer ei} = pf[i]
            mpz_set_si(q,qi)
            mpz_pow_ui(x,q,ei)
        end if
        mpz_fdiv_q(x, t, x)
        mpz_powm(x,a,x,pk)
        integer guard = 0
        while mpz_cmp_si(x,1)!=0 do
            mpz_mul(res,res,q)
            mpz_powm(x,x,q,pk)
            guard += 1
            if guard>100 then ?9/0 end if -- (increase if rqd)
        end while
    end for
    x = mpz_free(x)
end procedure

function multiplicative_order(mpz a, m)
    mpz res = mpz_init(1),
        ri = mpz_init()
    mpz_gcd(ri,a,m)
    if mpz_cmp_si(ri,1)!=0 then return "(a,m) not coprime" end if
    sequence pf = mpz_prime_factors(m,10000) -- (increase if rqd)
    for i=1 to length(pf) do
        multi_order(ri,a,pf[i])
        mpz_lcm(res,res,ri)
    end for
    return mpz_get_str(res)
end function

function shorta(mpz n)
    string res = mpz_get_str(n)
    integer lr = length(res)
    if lr>80 then
        res[6..-6] = "..."
        res &= sprintf(" (%d digits)",lr)
    end if
    return res
end function

procedure mo_test(mpz a, n)
    string res = multiplicative_order(a, n)
    printf(1,"ord(%s) mod %s = %s\n",{shorta(a),shorta(n),res})
end procedure

function i(atom i) return mpz_init(i) end function -- (ugh)
function p10(integer e,i)   -- init to 10^e+i
    mpz res = mpz_init()
    mpz_ui_pow_ui(res,10,e)
    mpz_add_si(res,res,i)
    return res
end function

atom t = time()
mo_test(i(3), i(10))
mo_test(i(37), i(1000))
mo_test(i(37), i(10000))
mo_test(i(37), i(3343))
mo_test(i(37), i(3344))
mo_test(i(2), i(1000))
mo_test(p10(100,+1), i(7919))
mo_test(p10(1000,+1), i(15485863))
mo_test(p10(10000,-1), i(22801763489))
mo_test(i(1511678068), i(7379191741))
mo_test(i(3047753288), i(2257683301))
?"==="
mpz b = p10(20,-1)
mo_test(i(2), b)
mo_test(i(17),b)
mo_test(i(54),i(100001))
string s9090 = multiplicative_order(mpz_init(54),mpz_init(100001))
if s9090!="9090" then ?9/0 end if
mpz m54 = mpz_init(54),
    m100001 = mpz_init(100001)
mpz_powm_ui(b,m54,9090,m100001)
printf(1,"%s\n",mpz_get_str(b))
bool error = false
for r=1 to 9090-1 do
    mpz_powm_ui(b,m54,r,m100001)
    if mpz_cmp_si(b,1)=0 then
        printf(1,"mpz_powm_ui(54,%d,100001) gives 1!\n",r)
        error = true
        exit
    end if
end for
if not error then
    printf(1,"Everything checks. (%s)\n",{elapsed(time()-t)})
end if
```

```txt

ord(3) mod 10 = 4
ord(37) mod 1000 = 100
ord(37) mod 10000 = 500
ord(37) mod 3343 = 1114
ord(37) mod 3344 = 20
ord(2) mod 1000 = (a,m) not coprime
ord(10000...00001 (101 digits)) mod 7919 = 3959
ord(10000...00001 (1001 digits)) mod 15485863 = 15485862
ord(99999...99999 (10000 digits)) mod 22801763489 = 22801763488
ord(1511678068) mod 7379191741 = 614932645
ord(3047753288) mod 2257683301 = 62713425
"==="
ord(2) mod 99999999999999999999 = 3748806900
ord(17) mod 99999999999999999999 = 1499522760
ord(54) mod 100001 = 9090
1
Everything checks. (0.2s)

```



## Python



```python
def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a

def lcm(a, b):
    return (a*b) / gcd(a, b)

def isPrime(p):
    return (p > 1) and all(f == p for f,e in factored(p))

primeList = [2,3,5,7]
def primes():
    for p in primeList:
        yield p
    while 1:
        p += 2
        while not isPrime(p):
            p += 2
        primeList.append(p)
        yield p

def factored( a):
    for p in primes():
        j = 0
        while a%p == 0:
            a /= p
            j += 1
        if j > 0:
            yield (p,j)
        if a < p*p: break
    if a > 1:
        yield (a,1)


def multOrdr1(a,(p,e) ):
    m = p**e
    t = (p-1)*(p**(e-1)) #  = Phi(p**e) where p prime
    qs = [1,]
    for f in factored(t):
        qs = [ q * f[0]**j for j in range(1+f[1]) for q in qs ]
    qs.sort()

    for q in qs:
        if pow( a, q, m )==1: break
    return q


def multOrder(a,m):
    assert gcd(a,m) == 1
    mofs = (multOrdr1(a,r) for r in factored(m))
    return reduce(lcm, mofs, 1)


if __name__ == "__main__":
    print multOrder(37, 1000)        # 100
    b = 10**20-1
    print multOrder(2, b) # 3748806900
    print multOrder(17,b) # 1499522760
    b = 100001
    print multOrder(54,b)
    print pow( 54, multOrder(54,b),b)
    if any( (1==pow(54,r, b)) for r in range(1,multOrder(54,b))):
        print 'Exists a power r < 9090 where pow(54,r,b)==1'
    else:
        print 'Everything checks.'
```



## Racket

The Racket function unit-group-order from racket/math computes the multiplicative order
of an element a in Zn. An implementation of the algorithm in the tast description is
shown below.


```racket

#lang racket
(require math)

(define (order a n)
  (unless (coprime? a n) (error 'order "arguments must be coprime"))
  (for/fold ([o 1]) ([r (factorize n)])
    (lcm o (order1 a r))))

(define (order1 a p&e)
  (match-define (list p e) p&e)
  (define m (expt p e))
  (define t (* (- p 1) (expt p (- e 1))))
  (define qs
    (for/fold ([qs '(1)]) ([f (factorize t)])
       (match f [(list f0 f1)
                 (for*/list ([q qs] [j (in-range (+ 1 f1))])
                   (* q (expt f0 j)))])))
  (for/or ([q (sort qs <)] #:when (= (modular-expt a q m) 1)) q))


(order 37 1000)
(order (+ (expt 10 100) 1) 7919)
(order (+ (expt 10 1000) 1) 15485863)
(order (- (expt 10 10000) 1) 22801763489)
(order 13 (+ 1 (expt 10 80)))

```

Output:

```racket

100
3959
15485862
22801763488
109609547199756140150989321269669269476675495992554276140800

```



## REXX


```rexx
/*REXX pgm computes multiplicative order of a minimum integer  N  such that  a^n mod m≡1*/
wa= 0;  wm= 0     /*       ═a═   ══m══     */    /*maximum widths of the A and M values.*/
@.=.;                @.1=   3      10
                     @.2=  37    1000
                     @.3=  37   10000
                     @.4=  37    3343
                     @.5=  37    3344
                     @.6=   2    1000
pad= left('', 9)
d= 500                                           /*use 500 decimal digits for a starter.*/
     do w=1  for 2                               /*when W≡1, find max widths of A and M.*/
       do j=1  while @.j\==.;         parse var  @.j     a  .  1  r  m  ,  n
       if w==1  then do;  wa= max(wa, length(a) );     wm= max(wm, length(m) );    iterate
                     end
       if m//a==0  then n= ' [solution not possible]'     /*test co─prime for  A and B. */
       numeric digits d                          /*start with  100  decimal digits.     */
       if n==''  then do n= 2;    p= r * a       /*compute product──may have an exponent*/
                      parse  var  p  'E'  _      /*try to extract the exponent from  P. */
                      if _\==''   then do;  numeric digits _+d  /*bump the decimal digs.*/
                                            p=r*a               /*recalculate integer P.*/
                                       end
                      if p//m==1  then leave     /*now, perform the nitty─gritty modulo.*/
                      r= p                       /*assign product to R for next multiply*/
                      end   /*n*/                /* [↑]    //   is really  ÷  remainder.*/
       say pad  'a='  right(a,wa)  pad  "m=" right(m,wm)  pad  'multiplicative order:'   n
       end   /*j*/
     end     /*w*/                               /*stick a fork in it,  we're all done. */
```

```txt

          a=  3           m=    10           multiplicative order: 4
          a= 37           m=  1000           multiplicative order: 100
          a= 37           m= 10000           multiplicative order: 500
          a= 37           m=  3343           multiplicative order: 1114
          a= 37           m=  3344           multiplicative order: 20
          a=  2           m=  1000           multiplicative order:  [solution not possible]

```



## Ruby



```ruby
require 'prime'

def powerMod(b, p, m)
  p.to_s(2).each_char.inject(1) do |result, bit|
    result = (result * result) % m
    bit=='1' ? (result * b) % m : result
  end
end

def multOrder_(a, p, k)
  pk = p ** k
  t = (p - 1) * p ** (k - 1)
  r = 1
  for q, e in t.prime_division
    x = powerMod(a, t / q**e, pk)
    while x != 1
      r *= q
      x = powerMod(x, q, pk)
    end
  end
  r
end

def multOrder(a, m)
  m.prime_division.inject(1) do |result, f|
    result.lcm(multOrder_(a, *f))
  end
end

puts multOrder(37, 1000)
b = 10**20-1
puts multOrder(2, b)
puts multOrder(17,b)
b = 100001
puts multOrder(54,b)
puts powerMod(54, multOrder(54,b), b)
if (1...multOrder(54,b)).any? {|r| powerMod(54, r, b) == 1}
  puts 'Exists a power r < 9090 where powerMod(54,r,b)==1'
else
  puts 'Everything checks.'
end
```


```txt

100
3748806900
1499522760
9090
1
Everything checks.

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const type: oneFactor is new struct
    var bigInteger: prime is 0_;
    var integer: exp is 0;
  end struct;

const func oneFactor: oneFactor (in bigInteger: prime, in integer: exp) is func
  result
    var oneFactor: aFactor is oneFactor.value;
  begin
    aFactor.prime := prime;
    aFactor.exp := exp;
  end func;

const func array oneFactor: factor (in var bigInteger: n) is func
  result
    var array oneFactor: pf is 0 times oneFactor.value;
  local
    var integer: e is 0;
    var bigInteger: d is 0_;
    var bigInteger: s is 0_;
  begin
    e := lowestSetBit(n);
    if e > 0 then
      n >>:= e;
      pf := [] (oneFactor(2_, e));
    end if;
    s := sqrt(n);
    d := 3_;
    while n > 1_ do
      if d > s then
        d := n;
      end if;
      e := 0;
      while n rem d = 0_ do
        n := n div d;
        incr(e);
      end while;
      if e > 0 then
        pf &:= oneFactor(d, e);
        s := sqrt(n);
      end if;
      d +:= 2_;
    end while;
  end func;

const func bigInteger: moBachShallit58(in bigInteger: a, in bigInteger: n, in array oneFactor: pf) is func
  result
    var bigInteger: mo is 0_;
  local
    var bigInteger: n1 is 0_;
    var oneFactor: pe is oneFactor.value;
    var bigInteger: x is 0_;
    var bigInteger: y is 0_;
    var integer: o is 0;
    var bigInteger: o1 is 0_;
  begin
    n1 := n - 1_;
    mo := 1_;
    for pe range pf do
      y := n1 div pe.prime ** pe.exp;
      x := modPow(a, y, n);
      o := 0;
      while x > 1_ do
        x := modPow(x, pe.prime, n);
        incr(o);
      end while;
      o1 := pe.prime ** o;
      mo *:= o1 div gcd(mo, o1);
    end for;
  end func;

const func boolean: isProbablyPrime (in bigInteger: primeCandidate, in var integer: count) is func
  result
    var boolean: isProbablyPrime is TRUE;
  local
    var bigInteger: aRandomNumber is 0_;
  begin
    while isProbablyPrime and count > 0 do
      aRandomNumber := rand(1_, pred(primeCandidate));
      isProbablyPrime := modPow(aRandomNumber, pred(primeCandidate), primeCandidate) = 1_;
      decr(count);
    end while;
    # writeln(count);
  end func;

const proc: moTest (in bigInteger: a, in bigInteger: n) is func
  begin
    if bitLength(a) < 100 then
      write("ord(" <& a <& ")");
    else
      write("ord([big])");
    end if;
    if bitLength(n) < 100 then
      write(" mod " <& n <& " ");
    else
      write(" mod [big] ");
    end if;
    if not isProbablyPrime(n, 20) then
      writeln("not computed.  modulus must be prime for this algorithm.")
    else
      writeln("= " <& moBachShallit58(a, n, factor(n - 1_)));
    end if;
  end func;

const proc: main is func
  local
    var bigInteger: b is 100_;
  begin
    moTest(37_, 3343_);
    moTest(10_ ** 100 + 1_, 7919_);
    moTest(10_ ** 1000 + 1_, 15485863_);
    moTest(10_ ** 10000 - 1_, 22801763489_);
    moTest(1511678068_, 7379191741_);
    moTest(3047753288_, 2257683301_);
  end func;
```


```txt

ord(37) mod 3343 = 1114
ord([big]) mod 7919 = 3959
ord([big]) mod 15485863 = 15485862
ord([big]) mod 22801763489 = 22801763488
ord(1511678068) mod 7379191741 = 614932645
ord(3047753288) mod 2257683301 = 62713425

```



## Sidef


Built-in:

```ruby
say 37.znorder(1000)     #=> 100
say 54.znorder(100001)   #=> 9090
```


```ruby
func mo_prime(a, p, e) {
    var m  = p**e
    var t  = (p-1)*(p**(e-1))
    var qs = [1]

    for f in (t.factor_exp) {
        qs.map! {|q|
            0..f[1] -> map {|j| q * f[0]**j }...
        }
    }

    qs.sort.first_by {|q| powmod(a, q, m) == 1 }
}

func mo(a, m) {
    gcd(a, m) == 1 || die "#{a} and #{m} are not relatively prime"
    Math.lcm(1, m.factor_exp.map {|r| mo_prime(a, r...) }...)
}

say mo(37, 1000)
say mo(54, 100001)

with (10**20 - 1) {|b|
    say mo(2, b)
    say mo(17, b)
}
```

```txt

100
9090
3748806900
1499522760

```



## Tcl

```tcl
package require Tcl 8.5
package require struct::list

proc multOrder {a m} {
    assert {[gcd $a $m] == 1}
    set mofs [list]
    dict for {p e} [factor_num $m] {
        lappend mofs [multOrdr1 $a $p $e]
    }
    return [struct::list fold $mofs 1 lcm]
}

proc multOrdr1 {a p e} {
    set m [expr {$p ** $e}]
    set t [expr {($p - 1) * ($p ** ($e - 1))}]
    set qs [dict create 1 ""]

    dict for {f0 f1} [factor_num $t] {
        dict for {q -} $qs {
            foreach j [range [expr {1 + $f1}]] {
                dict set qs [expr {$q * $f0 ** $j}] ""
            }
        }
    }

    dict for {q -} $qs {
        if {pypow($a, $q, $m) == 1} break
    }
    return $q
}

####################################################
# utility procs
proc assert {condition {message "Assertion failed!"}} {
    if { ! [uplevel 1 [list expr $condition]]} {
        return -code error $message
    }
}

proc gcd {a b} {
    while {$b != 0} {
        lassign [list $b [expr {$a % $b}]] a b
    }
    return $a
}

proc lcm {a b} {
    expr {$a * $b / [gcd $a $b]}
}

proc factor_num {num} {
    primes::restart
    set factors [dict create]
    for {set i [primes::get_next_prime]} {$i <= $num} {} {
        if {$num % $i == 0} {
            dict incr factors $i
            set num [expr {$num / $i}]
            continue
        } elseif {$i*$i > $num} {
            dict incr factors $num
            break
        } else {
            set i [primes::get_next_prime]
        }
    }
    return $factors
}

####################################################
# a range command akin to Python's
proc range args {
    foreach {start stop step} [switch -exact -- [llength $args] {
        1 {concat 0 $args 1}
        2 {concat   $args 1}
        3 {concat   $args  }
        default {error {wrong # of args: should be "range ?start? stop ?step?"}}
    }] break
    if {$step == 0} {error "cannot create a range when step == 0"}
    set range [list]
    while {$step > 0 ? $start < $stop : $stop < $start} {
        lappend range $start
        incr start $step
    }
    return $range
}

# python's pow()
proc ::tcl::mathfunc::pypow {x y {z ""}} {
    expr {$z eq "" ? $x ** $y : ($x ** $y) % $z}
}

####################################################
# prime number generator
# ref http://wiki.tcl.tk/5996
####################################################
namespace eval primes {}

proc primes::reset {} {
    variable list [list]
    variable current_index end
}

namespace eval primes {reset}

proc primes::restart {} {
    variable list
    variable current_index
    if {[llength $list] > 0} {
        set current_index 0
    }
}

proc primes::is_prime {candidate} {
    variable list

    foreach prime $list {
        if {$candidate % $prime == 0} {
            return false
        }
        if {$prime * $prime > $candidate} {
            return true
        }
    }
    while true {
        set largest [get_next_prime]
        if {$largest * $largest >= $candidate} {
            return [is_prime $candidate]
        }
    }
}

proc primes::get_next_prime {} {
    variable list
    variable current_index

    if {$current_index ne "end"} {
        set p [lindex $list $current_index]
        if {[incr current_index] == [llength $list]} {
            set current_index end
        }
        return $p
    }

    switch -exact -- [llength $list] {
        0 {set candidate 2}
        1 {set candidate 3}
        default {
            set candidate [lindex $list end]
            while true {
                incr candidate 2
                if {[is_prime $candidate]} break
            }
        }
    }
    lappend list $candidate
    return $candidate
}

####################################################
puts [multOrder 37 1000] ;# 100

set b [expr {10**20 - 1}]
puts [multOrder 2 $b] ;# 3748806900
puts [multOrder 17 $b] ;# 1499522760

set a 54
set m 100001
puts [set n [multOrder $a $m]] ;# 9090
puts [expr {pypow($a, $n, $m)}] ;# 1

set lambda {{a n m} {expr {pypow($a, $n, $m) == 1}}}
foreach r [lreverse [range 1 $n]] {
    if {[apply $lambda $a $r $m]} {
        error "Oops, $n is not the smallest:  {$a $r $m} satisfies $lambda"
    }
    if {$r % 1000 == 0} {puts "$r ..."}
}
puts "OK, $n is the smallest n such that {$a $n $m} satisfies $lambda"
```



## zkl

Using [[Extensible prime generator#zkl]] and the GMP library for lcm (least common multiple), pow and powm ((n^e)%m)

It would probably be nice to memoize the prime numbers but that isn't necessary for this task.

```zkl
var BN   =Import("zklBigNum");
var Sieve=Import("sieve");

    // factor n into powers of primes
    // eg 9090 == 2^1 * 3^2 * 5^1 * 101^1
fcn factor2PP(n){ // lazy factors using lazy primes --> (prime,power) ...
   Utils.Generator(fcn(a){
      primes:=Utils.Generator(Sieve.postponed_sieve);
      foreach p in (primes){
	 e:=0; while(a%p == 0){ a /= p; e+=1; }
	 if (e) vm.yield(p,e);
	 if (a<p*p) break;
      }
      if (a>1) vm.yield(a,1);
   },n)
}

fcn _multOrdr1(a,p,e){
   m:=p.pow(e);
   t:=m/p*(p - 1);
   qs:=L(BN(1));
   foreach p2,e2 in (factor2PP(t)){
      qs=[[(e,q); [0..e2]; qs; '{ q*BN(p2).pow(e) }]];
   }
   qs.filter1('wrap(q){ a.powm(q,m)==1 });
}

fcn multiOrder(a,m){
   if (m.gcd(a)!=1) throw(Exception.ValueError("Not co-prime"));
   res:=BN(1);
   foreach p,e in (factor2PP(m)){
      res = res.lcm(_multOrdr1(BN(a),BN(p),e));
   }
   return(res);
}
```


```zkl
multiOrder(37,1000).println();
b:=BN(10).pow(20)-1;
multiOrder(2,b).println();
multiOrder(17,b).println();

b=0d10_0001;
[BN(1)..multiOrder(54,b)-1].filter1('wrap(r,b54){b54.powm(r,b)==1},BN(54)) :
if (_) println("Exists a power r < 9090 where (54^r)%b)==1");
else println("Everything checks.");
```

```txt

100
3748806900
1499522760
Everything checks.

```


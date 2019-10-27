+++
title = "Tonelli-Shanks algorithm"
description = ""
date = 2019-08-03T19:21:16Z
aliases = []
[extra]
id = 20692
[taxonomies]
categories = []
tags = []
+++

{{task}}
'''Tonelli–Shanks algorithm'''
 
In computational number theory, the [https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm  Tonelli–Shanks algorithm] is a technique for solving an equation of the form:

<big><big>
:::          x<sup>2</sup> ≡ n (mod p)
</big></big>

─── where   '''n'''   is an integer which is a quadratic residue (mod p),   '''p'''   is an odd prime,   and

<big><big>
:::          x,n   Є   Fp = {0, 1, ... p-1} 
</big></big>


It is used in [https://en.wikipedia.org/wiki/Rabin_cryptosystem cryptography] techniques.


To apply the algorithm, we need the Legendre symbol.

Legendre symbol

* The Legendre symbol ( a | p) denotes the value of  a ^ ((p-1)/2) (mod p)
* (a | p) ≡    1      if a is a square (mod p)
* (a | p) ≡        -1      if a is not a square (mod p)
* (a | p) ≡    0      if a ≡ 0 


;Algorithm pseudo-code: (copied from Wikipedia):

All   ≡   are taken to mean   (mod p)   unless stated otherwise.

* Input : p an odd prime, and an integer n .
* Step 0. Check that n is indeed a square  : (n | p) must be ≡ 1
* Step 1. [Factors out powers of 2 from p-1] Define q -odd- and s such as p-1 = q * 2^s
** if s = 1 , i.e p ≡  3 (mod 4) , output the two solutions r ≡  +/- n^((p+1)/4) .
* Step 2. Select a non-square z such as (z | p) = -1 , and set c ≡  z^q .
* Step 3. Set r ≡  n ^((q+1)/2) , t ≡ n^q, m = s .
* Step 4. Loop.
** if t ≡  1  output r, p-r .
** Otherwise find, by repeated squaring, the lowest i , 0 < i< m , such as t^(2^i) ≡ 1
** Let b ≡  c^(2^(m-i-1)), and set r ≡  r*b, t ≡ t*b^2 , c ≡  b^2 and m = i.


;Numerical Example:
* n=10, p= 13.   See [https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm Wikipedia]


;Task:
Implement the above. 

Find solutions (if any) for 
* n = 10 p = 13
* n = 56 p = 101
* n = 1030 p = 10009
* n = 1032 p = 10009
* n = 44402 p = 100049  
 

;Extra credit:
* n  =    665820697     p  =   1000000009      
* n  =   881398088036     p   =  1000000000039    
* n  =  41660815127637347468140745042827704103445750172002   p  = 10^50 + 577   	


;See also:
* [[Modular exponentiation]]
* [[Cipolla's algorithm]]




=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Numerics;

namespace TonelliShanks {
    class Solution {
        private readonly BigInteger root1, root2;
        private readonly bool exists;

        public Solution(BigInteger root1, BigInteger root2, bool exists) {
            this.root1 = root1;
            this.root2 = root2;
            this.exists = exists;
        }

        public BigInteger Root1() {
            return root1;
        }

        public BigInteger Root2() {
            return root2;
        }

        public bool Exists() {
            return exists;
        }
    }

    class Program {
        static Solution Ts(BigInteger n, BigInteger p) {
            if (BigInteger.ModPow(n, (p - 1) / 2, p) != 1) {
                return new Solution(0, 0, false);
            }

            BigInteger q = p - 1;
            BigInteger ss = 0;
            while ((q & 1) == 0) {
                ss = ss + 1;
                q = q >> 1;
            }

            if (ss == 1) {
                BigInteger r1 = BigInteger.ModPow(n, (p + 1) / 4, p);
                return new Solution(r1, p - r1, true);
            }

            BigInteger z = 2;
            while (BigInteger.ModPow(z, (p - 1) / 2, p) != p - 1) {
                z = z + 1;
            }
            BigInteger c = BigInteger.ModPow(z, q, p);
            BigInteger r = BigInteger.ModPow(n, (q + 1) / 2, p);
            BigInteger t = BigInteger.ModPow(n, q, p);
            BigInteger m = ss;

            while (true) {
                if (t == 1) {
                    return new Solution(r, p - r, true);
                }
                BigInteger i = 0;
                BigInteger zz = t;
                while (zz != 1 && i < (m - 1)) {
                    zz = zz * zz % p;
                    i = i + 1;
                }
                BigInteger b = c;
                BigInteger e = m - i - 1;
                while (e > 0) {
                    b = b * b % p;
                    e = e - 1;
                }
                r = r * b % p;
                c = b * b % p;
                t = t * c % p;
                m = i;
            }
        }

        static void Main(string[] args) {
            List<Tuple<long, long>> pairs = new List<Tuple<long, long>>() {
                new Tuple<long, long>(10, 13),
                new Tuple<long, long>(56, 101),
                new Tuple<long, long>(1030, 10009),
                new Tuple<long, long>(1032, 10009),
                new Tuple<long, long>(44402, 100049),
                new Tuple<long, long>(665820697, 1000000009),
                new Tuple<long, long>(881398088036, 1000000000039),
            };

            foreach (var pair in pairs) {
                Solution sol = Ts(pair.Item1, pair.Item2);
                Console.WriteLine("n = {0}", pair.Item1);
                Console.WriteLine("p = {0}", pair.Item2);
                if (sol.Exists()) {
                    Console.WriteLine("root1 = {0}", sol.Root1());
                    Console.WriteLine("root2 = {0}", sol.Root2());
                } else {
                    Console.WriteLine("No solution exists");
                }
                Console.WriteLine();
            }

            BigInteger bn = BigInteger.Parse("41660815127637347468140745042827704103445750172002");
            BigInteger bp = BigInteger.Pow(10, 50) + 577;
            Solution bsol = Ts(bn, bp);
            Console.WriteLine("n = {0}", bn);
            Console.WriteLine("p = {0}", bp);
            if (bsol.Exists()) {
                Console.WriteLine("root1 = {0}", bsol.Root1());
                Console.WriteLine("root2 = {0}", bsol.Root2());
            } else {
                Console.WriteLine("No solution exists");
            }
        }
    }
}
```

{{out}}

```txt
n = 10
p = 13
root1 = 7
root2 = 6

n = 56
p = 101
root1 = 37
root2 = 64

n = 1030
p = 10009
root1 = 1632
root2 = 8377

n = 1032
p = 10009
No solution exists

n = 44402
p = 100049
root1 = 30468
root2 = 69581

n = 665820697
p = 1000000009
root1 = 378633312
root2 = 621366697

n = 881398088036
p = 1000000000039
root1 = 791399408049
root2 = 208600591990

n = 41660815127637347468140745042827704103445750172002
p = 100000000000000000000000000000000000000000000000577
root1 = 32102985369940620849741983987300038903725266634508
root2 = 67897014630059379150258016012699961096274733366069
```



## Clojure


```clojure

(defn find-first
 " Finds first element of collection that satisifies predicate function pred "
  [pred coll]
  (first (filter pred coll)))

(defn modpow
  " b^e mod m (using Java which solves some cases the pure clojure method has to be modified to tackle--i.e. with large b & e and
    calculation simplications when gcd(b, m) == 1 and gcd(e, m) == 1) "
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn legendre [a p]
  (modpow a (quot (dec p) 2) p)
)

(defn tonelli [n p]
  " Following Wikipedia https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm "
  (assert (= (legendre n p) 1) "not a square (mod p)")
  (loop [q (dec p)                                                  ; Step 1 in Wikipedia
         s 0]
    (if (zero? (rem q 2))
      (recur (quot q 2) (inc s))
      (if (= s 1)
        (modpow n (quot (inc p) 4) p)
        (let [z (find-first #(= (dec p) (legendre % p)) (range 2 p))] ; Step 2 in Wikipedia
          (loop [
                 M s
                 c (modpow z q p)
                 t (modpow n q p)
                 R (modpow n (quot (inc q) 2) p)]
            (if (= t 1)
              R
              (let [i (long (find-first #(= 1 (modpow t (bit-shift-left 1 %) p)) (range 1 M))) ; Step 3
                    b (modpow c (bit-shift-left 1 (- M i 1)) p)
                    M i
                    c (modpow b 2 p)
                    t (rem (* t c) p)
                    R (rem (* R b) p)]
                (recur M c t R)
                )
              )
            )
          )
        )
      )
    )
  )


; Testing--using Python examples
(doseq [[n p]  [[10, 13], [56, 101], [1030, 10009], [44402, 100049],
                [665820697, 1000000009], [881398088036, 1000000000039],
                [41660815127637347468140745042827704103445750172002, 100000000000000000000000000000000000000000000000577]]
        :let [r (tonelli n p)]]
  (println (format "n: %5d p: %d \n\troots: %5d %5d" (biginteger n) (biginteger p) (biginteger r) (biginteger (- p r)))))


```

{{out}}
n:    10 p: 13 
	roots:     7     6
n:    56 p: 101 
	roots:    37    64
n:  1030 p: 10009 
	roots:  1632  8377
n: 44402 p: 100049 
	roots: 30468 69581
n: 665820697 p: 1000000009 
	roots: 378633312 621366697
n: 881398088036 p: 1000000000039 
	roots: 791399408049 208600591990
n: 41660815127637347468140745042827704103445750172002 p: 100000000000000000000000000000000000000000000000577 
	roots: 32102985369940620849741983987300038903725266634508 67897014630059379150258016012699961096274733366069


## D

{{trans|Kotlin}}

```D
import std.bigint;
import std.stdio;
import std.typecons;

alias Pair = Tuple!(long, "n", long, "p");

enum BIGZERO = BigInt("0");
enum BIGONE = BigInt("1");
enum BIGTWO = BigInt("2");
enum BIGTEN = BigInt("10");

struct Solution {
    BigInt root1, root2;
    bool exists;
}

/// https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method
BigInt modPow(BigInt b, BigInt e, BigInt n) {
    if (n == 1) return BIGZERO;
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

Solution ts(long n, long p) {
    return ts(BigInt(n), BigInt(p));
}

Solution ts(BigInt n, BigInt p) {
    auto powMod(BigInt a, BigInt e) {
        return a.modPow(e, p);
    }

    auto ls(BigInt a) {
        return powMod(a, (p-1)/2);
    }

    if (ls(n) != 1) return Solution(BIGZERO, BIGZERO, false);
    auto q = p - 1;
    auto ss = BIGZERO;
    while ((q & 1) == 0) {
        ss = ss + 1;
        q = q >> 1;
    }

    if (ss == BIGONE) {
        auto r1 = powMod(n, (p + 1) / 4);
        return Solution(r1, p - r1, true);
    }

    auto z = BIGTWO;
    while (ls(z) != p - 1) z = z + 1;
    auto c = powMod(z, q);
    auto r = powMod(n, (q + 1) / 2);
    auto t = powMod(n, q);
    auto m = ss;

    while (true) {
        if (t == 1) return Solution(r, p - r, true);
        auto i = BIGZERO;
        auto zz = t;
        while (zz != 1 && i < m - 1) {
            zz  = zz * zz % p;
            i = i + 1;
        }
        auto b = c;
        auto e = m - i - 1;
        while (e > 0) {
            b = b * b % p;
            e = e - 1;
        }
        r = r * b % p;
        c = b * b % p;
        t = t * c % p;
        m = i;
    }
}

void main() {
    auto pairs = [
        Pair(             10L,                13L),
        Pair(             56L,               101L),
        Pair(          1_030L,            10_009L),
        Pair(          1_032L,            10_009L),
        Pair(         44_402L,           100_049L),
        Pair(    665_820_697L,     1_000_000_009L),
        Pair(881_398_088_036L, 1_000_000_000_039L),
    ];

    foreach (pair; pairs) {
        auto sol = ts(pair.n, pair.p);

        writeln("n = ", pair.n);
        writeln("p = ", pair.p);
        if (sol.exists) {
            writeln("root1 = ", sol.root1);
            writeln("root2 = ", sol.root2);
        }
        else writeln("No solution exists");
        writeln();
    }

    auto bn = BigInt("41660815127637347468140745042827704103445750172002");
    auto bp = BIGTEN ^^ 50 + 577L;
    auto sol = ts(bn, bp);
    writeln("n = ", bn);
    writeln("p = ", bp);
    if (sol.exists) {
        writeln("root1 = ", sol.root1);
        writeln("root2 = ", sol.root2);
    }
    else writeln("No solution exists");
}
```


{{out}}

```txt
n = 10
p = 13
root1 = 7
root2 = 6

n = 56
p = 101
root1 = 37
root2 = 64

n = 1030
p = 10009
root1 = 1632
root2 = 8377

n = 1032
p = 10009
No solution exists

n = 44402
p = 100049
root1 = 30468
root2 = 69581

n = 665820697
p = 1000000009
root1 = 378633312
root2 = 621366697

n = 881398088036
p = 1000000000039
root1 = 791399408049
root2 = 208600591990

n = 41660815127637347468140745042827704103445750172002
p = 100000000000000000000000000000000000000000000000577
root1 = 32102985369940620849741983987300038903725266634508
root2 = 67897014630059379150258016012699961096274733366069
```



## EchoLisp


```scheme

(require 'bigint)
;; test equality mod p
(define-syntax-rule (mod= a b p) 
	 (zero?  (% (- a b) p)))
;; assign mod p
(define-syntax-rule (mod:≡ s v p)
	(set! s (% v p)))

(define (Legendre a p)  
	 (powmod a (/ (1- p) 2) p))

(define (Tonelli n p)
    (unless (= 1 (Legendre n p)) (error "not a square (mod p)" (list n p)))
    (define q (1- p))
    (define s 0)
	(while (even? q)
		(/= q 2)
		(++ s))
	(if (= s 1) (powmod n (/ (1+ p) 4) p)
	(begin
	(define z   
		(for ((z (in-range 2 p)))  
		  #:break (= (1- p)  (Legendre z p)) => z ))

	(define c (powmod z q p))
	(define r (powmod n (/ (1+ q) 2) p))
	(define t (powmod n q p))
	(define m s)
	(define t2 0)
	(while #t
		#:break (mod= 1  t p) => r
		(mod:≡ t2 (* t t) p) 
		(define i 
			(for ((i (in-range 1 m)))
				#:break (mod= t2 1 p) => i
				(mod:≡ t2 (* t2 t2) p)))
		(define b (powmod c (expt 2 (- m i 1)) p))
		(mod:≡ r (* r b) p) 
		(mod:≡ c (* b b) p) 
		(mod:≡ t (* t c) p) 
		(set! m i)))))

```

{{out}}

```txt

(define ttest 
	`((10 13) (56 101) (1030 10009) (44402 100049)  
	(665820697 1000000009) 
	(881398088036  1000000000039)
	(41660815127637347468140745042827704103445750172002  ,(+ 1e50 577))))  
	     	
(define (task ttest)
	(for ((test ttest))
		(define n (first test))
		(define p (second test))
		(define r (Tonelli n p))
		(assert (mod= (* r r) n p))
		(printf "n = %d p = %d" n p)
		(printf "\t  roots : %d %d"  r (- p r))))

(task ttest)
n = 10 p = 13
  roots : 7 6
n = 56 p = 101
  roots : 37 64
n = 1030 p = 10009
  roots : 1632 8377
n = 44402 p = 100049
  roots : 30468 69581
n = 665820697 p = 1000000009
  roots : 378633312 621366697
n = 881398088036 p = 1000000000039
  roots : 791399408049 208600591990
n = 41660815127637347468140745042827704103445750172002 
p = 100000000000000000000000000000000000000000000000577
  roots : 32102985369940620849741983987300038903725266634508    
  67897014630059379150258016012699961096274733366069
(Tonelli 1032 10009)
❌ error: not a square (mod p) (1032 10009)

```


## FreeBASIC


### LongInt version


```FreeBASIC
' version 11-04-2017
' compile with: fbc -s console
' maximum for p is 17 digits to be on the save side

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' But we have to define them for older versions.
#Ifndef TRUE
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function mul_mod(a As ULongInt, b As ULongInt, modulus As ULongInt) As ULongInt
    ' returns a * b mod modulus
    Dim As ULongInt x, y = a Mod modulus

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

Function Isprime(n As ULongInt, k As Long) As Long
    ' miller-rabin prime test
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

Function legendre_symbol (a As LongInt, p As LongInt) As LongInt

    Dim As LongInt x = pow_mod(a, ((p -1) \ 2), p)
    If p -1 = x Then
        Return x - p
    Else
        Return x
    End If

End Function

' ------=< MAIN >=------

Dim As LongInt b, c, i, k, m, n, p, q, r, s, t, z

For k = 1 To 7
    Read n, p
    Print "Find solution for n ="; n; " and p =";p

    If legendre_symbol(n, p) <> 1 Then
        Print n;" is not a quadratic residue"
        Print
        Continue For
    End If

    If p = 2 OrElse Isprime(p, 15) = FALSE Then
        Print p;" is not a odd prime"
        Print
        Continue For
    End If

    s = 0 : q = p -1
    Do
        s += 1
        q \= 2
    Loop Until (q And 1) = 1

    If s = 1 And (p Mod 4) = 3 Then
        r = pow_mod(n, ((p +1) \ 4), p)
        Print "Solution found:"; r; " and"; p - r
        Print
        Continue For
    End If

    z = 1
    Do
        z += 1
    Loop Until legendre_symbol(z, p) = -1
    c = pow_mod(z, q, p)
    r = pow_mod(n, (q +1) \ 2, p)
    t = pow_mod(n, q, p)
    m = s

    Do
        i = 0
        If (t Mod p) = 1 Then
            Print "Solution found:"; r; " and"; p - r
            Print
            Continue For
        End If

        Do
            i += 1
            If i >= m Then Continue For
        Loop Until pow_mod(t, 2 ^ i, p) = 1
        b = pow_mod(c, (2 ^ (m - i -1)), p)
        r = mul_mod(r, b, p)
        c = mul_mod(b, b, p)
        t = mul_mod(t, c, p)' t = t * b ^ 2
        m = i
    Loop

Next

Data 10, 13, 56, 101, 1030, 10009, 1032, 10009, 44402, 100049
Data 665820697, 1000000009, 881398088036, 1000000000039

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Find solution for n = 10 and p = 13
Solution found: 7 and 6

Find solution for n = 56 and p = 101
Solution found: 37 and 64

Find solution for n = 1030 and p = 10009
Solution found: 1632 and 8377

Find solution for n = 1032 and p = 10009
 1032 is not a quadratic residue

Find solution for n = 44402 and p = 100049
Solution found: 30468 and 69581

Find solution for n = 665820697 and p = 1000000009
Solution found: 378633312 and 621366697

Find solution for n = 881398088036 and p = 1000000000039
Solution found: 791399408049 and 208600591990
```


### GMP version

{{libheader|GMP}}

```freebasic
' version 12-04-2017
' compile with: fbc -s console

#Include Once "gmp.bi"

Data "10", "13", "56", "101", "1030", "10009", "1032", "10009"
Data "44402", "100049", "665820697", "1000000009"
Data "881398088036", "1000000000039"
Data "41660815127637347468140745042827704103445750172002"   ' p = 10^50 + 577

' ------=< MAIN >=------

Dim As uLong k
Dim As ZString Ptr zstr
Dim As String n_str, p_str

Dim As Mpz_ptr b, c, i, m, n, p, q, r, s, t, z, tmp
b = Allocate(Len(__Mpz_struct)) : Mpz_init(b)
c = Allocate(Len(__Mpz_struct)) : Mpz_init(c)
i = Allocate(Len(__Mpz_struct)) : Mpz_init(i)
m = Allocate(Len(__Mpz_struct)) : Mpz_init(m)
n = Allocate(Len(__Mpz_struct)) : Mpz_init(n)
p = Allocate(Len(__Mpz_struct)) : Mpz_init(p)
q = Allocate(Len(__Mpz_struct)) : Mpz_init(q)
r = Allocate(Len(__Mpz_struct)) : Mpz_init(r)
s = Allocate(Len(__Mpz_struct)) : Mpz_init(s)
t = Allocate(Len(__Mpz_struct)) : Mpz_init(t)
z = Allocate(Len(__Mpz_struct)) : Mpz_init(z)
tmp = Allocate(Len(__Mpz_struct)) : Mpz_init(tmp)

For k = 1 To 8
    Read n_str
    Mpz_set_str(n, n_str, 10)
    If k < 8 Then
        Read p_str
        Mpz_set_str(p, p_str, 10)
    Else
        p_str = "10^50 + 577"
        Mpz_set_str(p, "1" + String(50, "0"), 10)
        Mpz_add_ui(p, p, 577)
    End If

    Print "Find solution for n = "; n_str; " and p = "; p_str

    If Mpz_legendre(n, p) <> 1 Then
        Print n_str; " is not a quadratic residue"
        Print
        Continue For
    End If

    If Mpz_tstbit(p, 0) = 0 OrElse Mpz_probab_prime_p(p, 20) = 0 Then
        Print p_str; "is not a odd prime"
        Print
        Continue For
    End If

    Mpz_set_ui(s, 0) : Mpz_set(q, p) : Mpz_sub_ui(q, q, 1) ' q = p -1
    Do
        Mpz_add_ui(s, s, 1)
        Mpz_fdiv_q_2exp(q, q, 1)
    Loop Until Mpz_tstbit(q, 0) = 1

    If Mpz_cmp_ui(s, 1) = 0 Then
        If Mpz_tstbit(p, 1) = 1 Then
            Mpz_add_ui(tmp, p, 1)
            Mpz_fdiv_q_2exp(tmp, tmp, 2)         ' tmp = p +1 \ 4
            Mpz_powm(r, n, tmp, p)
            zstr = Mpz_get_str(0, 10, r)
            Print "Solution found: "; *zstr;
            Mpz_sub(r, p, r)
            zstr = Mpz_get_str(0, 10, r)
            Print " and "; *zstr
            Print
            Continue For
        End If
    End If

    Mpz_set_ui(z, 1)
    Do
        Mpz_add_ui(z, z, 1)
    Loop Until Mpz_legendre(z, p) = -1
    Mpz_powm(c, z, q, p)
    Mpz_add_ui(tmp, q, 1)
    Mpz_fdiv_q_2exp(tmp, tmp, 1)
    Mpz_powm(r, n, tmp, p)
    Mpz_powm(t, n, q, p)
    Mpz_set(m, s)

    Do
        Mpz_set_ui(i, 0)
        Mpz_mod(tmp, t, p)
        If Mpz_cmp_ui(tmp, 1) = 0 Then
            zstr = Mpz_get_str(0, 10, r)
            Print "Solution found: "; *zstr;
            Mpz_sub(r, p, r)
            zstr = Mpz_get_str(0, 10, r)
            Print " and "; *zstr
            Print
            Continue For
        End If

        Mpz_set_ui(q, 1)
        Do
            Mpz_add_ui(i, i, 1)
            If Mpz_cmp(i, m) >= 0 Then
                Continue For
            end if
            Mpz_mul_ui(q, q, 2)                  ' q = 2^i
            Mpz_powm(tmp, t, q, p)
        Loop Until Mpz_cmp_ui(tmp, 1) = 0

        Mpz_set_ui(q, 2)
        Mpz_sub(tmp, m, i) : Mpz_sub_ui(tmp, tmp, 1) : Mpz_powm(tmp, q, tmp, p)
        Mpz_powm(b, c, tmp, p)
        Mpz_mul(r, r, b) : Mpz_mod(r, r, p)
        Mpz_mul(tmp, b, b) : Mpz_mod(c, tmp, p)
        Mpz_mul(tmp, t, c) : Mpz_mod(t, tmp, p)
        Mpz_set(m, i)
    Loop

Next

Mpz_clear(b) : Mpz_clear(c) : Mpz_clear(i) : Mpz_clear(m)
Mpz_clear(n) : Mpz_clear(p) : Mpz_clear(q) : Mpz_clear(r)
Mpz_clear(s) : Mpz_clear(t) : Mpz_clear(z) : Mpz_clear(tmp)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Find solution for n = 10 and p = 13
Solution found: 7 and 6

Find solution for n = 56 and p = 101
Solution found: 37 and 64

Find solution for n = 1030 and p = 10009
Solution found: 1632 and 8377

Find solution for n = 1032 and p = 10009
1032 is not a quadratic residue

Find solution for n = 44402 and p = 100049
Solution found: 30468 and 69581

Find solution for n = 665820697 and p = 1000000009
Solution found: 378633312 and 621366697

Find solution for n = 881398088036 and p = 1000000000039
Solution found: 791399408049 and 208600591990

Find solution for n = 41660815127637347468140745042827704103445750172002 and p = 10^50 + 577
Solution found: 32102985369940620849741983987300038903725266634508 and 67897014630059379150258016012699961096274733366069
```



## Go


### int

Implementation following Wikipedia, using similar variable names, and using the int type for simplicity.

```go
package main

import "fmt"

// Arguments n, p as described in WP
// If Legendre symbol != 1, ok return is false.  Otherwise ok return is true,
// R1 is WP return value R and for convenience R2 is p-R1.
func ts(n, p int) (R1, R2 int, ok bool) {
    // a^e mod p
    powModP := func(a, e int) int {
        s := 1
        for ; e > 0; e-- {
            s = s * a % p
        }
        return s
    }
    // Legendre symbol, returns 1, 0, or -1 mod p -- that's 1, 0, or p-1.
    ls := func(a int) int {
        return powModP(a, (p-1)/2)
    }
    // argument validation
    if ls(n) != 1 {
        return 0, 0, false
    }
    // WP step 1, factor out powers two.
    // variables Q, S named as at WP.
    Q := p - 1
    S := 0
    for Q&1 == 0 {
        S++
        Q >>= 1
    }
    // WP step 1, direct solution
    if S == 1 {
        R1 = powModP(n, (p+1)/4)
        return R1, p - R1, true
    }
    // WP step 2, select z, assign c
    z := 2
    for ; ls(z) != p-1; z++ {
    }
    c := powModP(z, Q)
    // WP step 3, assign R, t, M
    R := powModP(n, (Q+1)/2)
    t := powModP(n, Q)
    M := S
    // WP step 4, loop
    for {
        // WP step 4.1, termination condition
        if t == 1 {
            return R, p - R, true
        }
        // WP step 4.2, find lowest i...
        i := 0
        for z := t; z != 1 && i < M-1; {
            z = z * z % p
            i++
        }
        // WP step 4.3, using a variable b, assign new values of R, t, c, M
        b := c
        for e := M - i - 1; e > 0; e-- {
            b = b * b % p
        }
        R = R * b % p
        c = b * b % p // more convenient to compute c before t
        t = t * c % p
        M = i
    }
}

func main() {
    fmt.Println(ts(10, 13))
    fmt.Println(ts(56, 101))
    fmt.Println(ts(1030, 10009))
    fmt.Println(ts(1032, 10009))
    fmt.Println(ts(44402, 100049))
}
```

{{out}}

```txt

7 6 true
37 64 true
1632 8377 true
0 0 false
30468 69581 true

```


### big.Int

For the extra credit, we use big.Int from the math/big package of the Go standard library.  While the method call syntax is not as easy on the eyes as operator syntax, the package provides modular exponentiation and even the Legendre symbol as the Jacobi function.

```go
package main

import (
    "fmt"
    "math/big"
)

func ts(n, p big.Int) (R1, R2 big.Int, ok bool) {
    if big.Jacobi(&n, &p) != 1 {
        return
    }
    var one, Q big.Int
    one.SetInt64(1)
    Q.Sub(&p, &one)
    S := 0
    for Q.Bit(0) == 0 {
        S++
        Q.Rsh(&Q, 1)
    }
    if S == 1 {
        R1.Exp(&n, R1.Rsh(R1.Add(&p, &one), 2), &p)
        R2.Sub(&p, &R1)
        return R1, R2, true
    }
    var z, c big.Int
    for z.SetInt64(2); big.Jacobi(&z, &p) != -1; z.Add(&z, &one) {
    }
    c.Exp(&z, &Q, &p)
    var R, t big.Int
    R.Exp(&n, R.Rsh(R.Add(&Q, &one), 1), &p)
    t.Exp(&n, &Q, &p)
    M := S
    for {
        if t.Cmp(&one) == 0 {
            R2.Sub(&p, &R)
            return R, R2, true
        }
        i := 0
        // reuse z as a scratch variable
        for z.Set(&t); z.Cmp(&one) != 0 && i < M-1; {
            z.Mod(z.Mul(&z, &z), &p)
            i++
        }
        // and instead of a new scratch variable b, continue using z
        z.Set(&c)
        for e := M - i - 1; e > 0; e-- {
            z.Mod(z.Mul(&z, &z), &p)
        }
        R.Mod(R.Mul(&R, &z), &p)
        c.Mod(c.Mul(&z, &z), &p)
        t.Mod(t.Mul(&t, &c), &p)
        M = i
    }
}

func main() {
    var n, p big.Int
    n.SetInt64(665820697)
    p.SetInt64(1000000009)
    R1, R2, ok := ts(n, p)
    fmt.Println(&R1, &R2, ok)

    n.SetInt64(881398088036)
    p.SetInt64(1000000000039)
    R1, R2, ok = ts(n, p)
    fmt.Println(&R1, &R2, ok)
    n.SetString("41660815127637347468140745042827704103445750172002", 10)
    p.SetString("100000000000000000000000000000000000000000000000577", 10)
    R1, R2, ok = ts(n, p)
    fmt.Println(&R1)
    fmt.Println(&R2)
}
```

{{out}}

```txt

378633312 621366697 true
791399408049 208600591990 true
32102985369940620849741983987300038903725266634508
67897014630059379150258016012699961096274733366069

```


### Library

It gets better; the library has a ModSqrt function that uses Tonelli-Shanks internally.  Output is same as above.

```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    var n, p, R1, R2 big.Int
    n.SetInt64(665820697)
    p.SetInt64(1000000009)
    R1.ModSqrt(&n, &p)
    R2.Sub(&p, &R1)
    fmt.Println(&R1, &R2)

    n.SetInt64(881398088036)
    p.SetInt64(1000000000039)
    R1.ModSqrt(&n, &p)
    R2.Sub(&p, &R1)
    fmt.Println(&R1, &R2)

    n.SetString("41660815127637347468140745042827704103445750172002", 10)
    p.SetString("100000000000000000000000000000000000000000000000577", 10)
    R1.ModSqrt(&n, &p)
    R2.Sub(&p, &R1)
    fmt.Println(&R1)
    fmt.Println(&R2)
}
```


## J


Implementation:


```J
leg=: dyad define
  x (y&|)@^ (y-1)%2
)

tosh=:dyad define
  assert. 1=1 p: y [ 'y must be prime'
  assert. 1=x leg y [ 'x must be square mod y'
  pow=. y&|@^
  if. 1=m=. {.1 q: y-1 do.
    r=. x pow (y+1)%4 
  else.
    z=. 1x while. 1>: z leg y do. z=.z+1 end.
    c=. z pow q=. (y-1)%2^m
    r=. x pow (q+1)%2
    t=. x pow q
    while. t~:1 do.
      n=. t
      i=. 0
      whilst. 1~:n do.
        n=. n pow 2
        i=. i+1
      end.
      r=. y|r*b=. c pow 2^m-i+1
      m=. i
      t=. y|t*c=. b pow 2
    end.
  end.
  y|(,-)r
)
```


Task examples:


```J
   10 tosh 13
7 6
   56 tosh 101
37 64
   1030 tosh 10009
1632 8377
   1032 tosh 10009
|assertion failure: tosh
|   1=x leg y['x must be square mod y'
   44402 tosh 100049
30468 69581
   665820697x tosh 1000000009x
378633312 621366697
   881398088036 tosh 1000000000039x
791399408049 208600591990
   41660815127637347468140745042827704103445750172002x tosh (10^50x)+577
32102985369940620849741983987300038903725266634508 67897014630059379150258016012699961096274733366069
```



## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import java.math.BigInteger;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

public class TonelliShanks {
    private static final BigInteger ZERO = BigInteger.ZERO;
    private static final BigInteger ONE = BigInteger.ONE;
    private static final BigInteger TEN = BigInteger.TEN;
    private static final BigInteger TWO = BigInteger.valueOf(2);
    private static final BigInteger FOUR = BigInteger.valueOf(4);

    private static class Solution {
        private BigInteger root1;
        private BigInteger root2;
        private boolean exists;

        Solution(BigInteger root1, BigInteger root2, boolean exists) {
            this.root1 = root1;
            this.root2 = root2;
            this.exists = exists;
        }
    }

    private static Solution ts(Long n, Long p) {
        return ts(BigInteger.valueOf(n), BigInteger.valueOf(p));
    }

    private static Solution ts(BigInteger n, BigInteger p) {
        BiFunction<BigInteger, BigInteger, BigInteger> powModP = (BigInteger a, BigInteger e) -> a.modPow(e, p);
        Function<BigInteger, BigInteger> ls = (BigInteger a) -> powModP.apply(a, p.subtract(ONE).divide(TWO));

        if (!ls.apply(n).equals(ONE)) return new Solution(ZERO, ZERO, false);

        BigInteger q = p.subtract(ONE);
        BigInteger ss = ZERO;
        while (q.and(ONE).equals(ZERO)) {
            ss = ss.add(ONE);
            q = q.shiftRight(1);
        }

        if (ss.equals(ONE)) {
            BigInteger r1 = powModP.apply(n, p.add(ONE).divide(FOUR));
            return new Solution(r1, p.subtract(r1), true);
        }

        BigInteger z = TWO;
        while (!ls.apply(z).equals(p.subtract(ONE))) z = z.add(ONE);
        BigInteger c = powModP.apply(z, q);
        BigInteger r = powModP.apply(n, q.add(ONE).divide(TWO));
        BigInteger t = powModP.apply(n, q);
        BigInteger m = ss;

        while (true) {
            if (t.equals(ONE)) return new Solution(r, p.subtract(r), true);
            BigInteger i = ZERO;
            BigInteger zz = t;
            while (!zz.equals(BigInteger.ONE) && i.compareTo(m.subtract(ONE)) < 0) {
                zz = zz.multiply(zz).mod(p);
                i = i.add(ONE);
            }
            BigInteger b = c;
            BigInteger e = m.subtract(i).subtract(ONE);
            while (e.compareTo(ZERO) > 0) {
                b = b.multiply(b).mod(p);
                e = e.subtract(ONE);
            }
            r = r.multiply(b).mod(p);
            c = b.multiply(b).mod(p);
            t = t.multiply(c).mod(p);
            m = i;
        }
    }

    public static void main(String[] args) {
        List<Map.Entry<Long, Long>> pairs = List.of(
            Map.entry(10L, 13L),
            Map.entry(56L, 101L),
            Map.entry(1030L, 10009L),
            Map.entry(1032L, 10009L),
            Map.entry(44402L, 100049L),
            Map.entry(665820697L, 1000000009L),
            Map.entry(881398088036L, 1000000000039L)
        );

        for (Map.Entry<Long, Long> pair : pairs) {
            Solution sol = ts(pair.getKey(), pair.getValue());
            System.out.printf("n = %s\n", pair.getKey());
            System.out.printf("p = %s\n", pair.getValue());
            if (sol.exists) {
                System.out.printf("root1 = %s\n", sol.root1);
                System.out.printf("root2 = %s\n", sol.root2);
            } else {
                System.out.println("No solution exists");
            }
            System.out.println();
        }

        BigInteger bn = new BigInteger("41660815127637347468140745042827704103445750172002");
        BigInteger bp = TEN.pow(50).add(BigInteger.valueOf(577));
        Solution sol = ts(bn, bp);
        System.out.printf("n = %s\n", bn);
        System.out.printf("p = %s\n", bp);
        if (sol.exists) {
            System.out.printf("root1 = %s\n", sol.root1);
            System.out.printf("root2 = %s\n", sol.root2);
        } else {
            System.out.println("No solution exists");
        }
    }
}
```

{{out}}

```txt
n = 10
p = 13
root1 = 7
root2 = 6

n = 56
p = 101
root1 = 37
root2 = 64

n = 1030
p = 10009
root1 = 1632
root2 = 8377

n = 1032
p = 10009
No solution exists

n = 44402
p = 100049
root1 = 30468
root2 = 69581

n = 665820697
p = 1000000009
root1 = 378633312
root2 = 621366697

n = 881398088036
p = 1000000000039
root1 = 791399408049
root2 = 208600591990

n = 41660815127637347468140745042827704103445750172002
p = 100000000000000000000000000000000000000000000000577
root1 = 32102985369940620849741983987300038903725266634508
root2 = 67897014630059379150258016012699961096274733366069
```



## Julia

{{works with|Julia|0.6}}

'''Module''':

```julia
module TonelliShanks

legendre(a, p) = powermod(a, (p - 1) ÷ 2, p)

function solve(n::T, p::T) where T <: Union{Int, Int128, BigInt}
    legendre(n, p) != 1 && throw(ArgumentError("$n not a square (mod $p)"))
    local q::T = p - one(p)
    local s::T = 0
    while iszero(q % 2)
        q ÷= 2
        s += one(s)
    end
    if s == one(s)
        r = powermod(n, (p + 1) >> 2, p)
        return r, p - r
    end
    local z::T
    for z in 2:(p - 1)
        p - 1 == legendre(z, p) && break
    end
    local c::T = powermod(z, q, p)
    local r::T = powermod(n, (q + 1) >> 1, p)
    local t::T = powermod(n, q, p)
    local m::T = s
    local t2::T = zero(p)
    while !iszero((t - 1) % p)
        t2 = (t * t) % p
        local i::T
        for i in Base.OneTo(m)
            iszero((t2 - 1) % p) && break
            t2 = (t2 * t2) % p
        end
        b = powermod(c, 1 << (m - i - 1), p)
        r = (r * b) % p
        c = (b * b) % p
        t = (t * c) % p
        m = i
    end
    return r, p - r
end

end  # module TonelliShanks
```


'''Main''':

```julia
@show TonelliShanks.solve(10, 13)
@show TonelliShanks.solve(56, 101)
@show TonelliShanks.solve(1030, 10009)
@show TonelliShanks.solve(44402, 100049)
@show TonelliShanks.solve(665820697, 1000000009)
@show TonelliShanks.solve(881398088036, 1000000000039)
@show TonelliShanks.solve(41660815127637347468140745042827704103445750172002, big"10" ^ 50 + 577)
```


{{out}}

```txt
TonelliShanks.solve(10, 13) = (7, 6)
TonelliShanks.solve(56, 101) = (37, 64)
TonelliShanks.solve(1030, 10009) = (1632, 8377)
TonelliShanks.solve(44402, 100049) = (30468, 69581)
TonelliShanks.solve(665820697, 1000000009) = (378633312, 621366697)
TonelliShanks.solve(881398088036, 1000000000039) = (791399408049, 208600591990)
TonelliShanks.solve(@big_str("41660815127637347468140745042827704103445750172002"), @big_str("10") ^ 50 + 577) = (32102985369940620849741983987300038903725266634508, 67897014630059379150258016012699961096274733366069)
```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.3

import java.math.BigInteger

data class Solution(val root1: BigInteger, val root2: BigInteger, val exists: Boolean)
 
val bigZero = BigInteger.ZERO
val bigOne  = BigInteger.ONE
val bigTwo  = BigInteger.valueOf(2L)
val bigFour = BigInteger.valueOf(4L)
val bigTen  = BigInteger.TEN

fun ts(n: Long, p: Long) = ts(BigInteger.valueOf(n), BigInteger.valueOf(p))

fun ts(n: BigInteger, p: BigInteger): Solution {

    fun powModP(a: BigInteger, e: BigInteger) = a.modPow(e, p)

    fun ls(a: BigInteger) = powModP(a, (p - bigOne) / bigTwo)

    if (ls(n) != bigOne) return Solution(bigZero, bigZero, false)
    var q = p - bigOne
    var ss = bigZero
    while (q.and(bigOne) == bigZero) {
        ss = ss + bigOne
        q = q.shiftRight(1)
    }

    if (ss == bigOne) {
        val r1 = powModP(n, (p + bigOne) / bigFour)
        return Solution(r1, p - r1, true)
    }

    var z = bigTwo
    while (ls(z) != p - bigOne) z = z + bigOne
    var c = powModP(z, q)
    var r = powModP(n, (q + bigOne) / bigTwo)
    var t = powModP(n, q)
    var m = ss

    while (true) {
        if (t == bigOne) return Solution(r, p - r, true)
        var i = bigZero
        var zz = t
        while (zz != bigOne && i < m - bigOne) {
            zz  = zz * zz % p
            i = i + bigOne
        }
        var b = c
        var e = m - i - bigOne
        while (e > bigZero) {
            b = b * b % p
            e = e - bigOne
        }
        r = r * b % p
        c = b * b % p
        t = t * c % p
        m = i
    }
}

fun main(args: Array<String>) {
    val pairs = listOf<Pair<Long, Long>>(
        10L to 13L, 
        56L to 101L, 
        1030L to 10009L,
        1032L to 10009L,
        44402L to 100049L,
        665820697L to 1000000009L,
        881398088036L to 1000000000039L
    )

    for (pair in pairs) {
        val (n, p) = pair
        val (root1, root2, exists) = ts(n, p)
        println("n = $n")
        println("p = $p")
        if (exists) {
            println("root1 = $root1")
            println("root2 = $root2")
        }
        else println("No solution exists")
        println()
    }

    val bn = BigInteger("41660815127637347468140745042827704103445750172002")
    val bp = bigTen.pow(50) + BigInteger.valueOf(577L)
    val (broot1, broot2, bexists) = ts(bn, bp)
    println("n = $bn")
    println("p = $bp")
    if (bexists) {
        println("root1 = $broot1")
        println("root2 = $broot2")
    }
    else println("No solution exists")    
}
```


{{out}}

```txt

n = 10
p = 13
root1 = 7
root2 = 6

n = 56
p = 101
root1 = 37
root2 = 64

n = 1030
p = 10009
root1 = 1632
root2 = 8377

n = 1032
p = 10009
No solution exists

n = 44402
p = 100049
root1 = 30468
root2 = 69581

n = 665820697
p = 1000000009
root1 = 378633312
root2 = 621366697

n = 881398088036
p = 1000000000039
root1 = 791399408049
root2 = 208600591990

n = 41660815127637347468140745042827704103445750172002
p = 100000000000000000000000000000000000000000000000577
root1 = 32102985369940620849741983987300038903725266634508
root2 = 67897014630059379150258016012699961096274733366069

```



## Nim


Based algorithm pseudo-code, referencing python 3. 


```Nim

proc pow*[T:SomeInteger](x,n,p:T):T = 
  var t = x mod p 
  var e = n 
  result = 1 
  while e > 0: 
    if (e and 1) == 1: 
      result = result * t mod p 
    t = t * t mod p 
    e = e shr 1 

proc legendre*[T:SomeInteger](a,p:T):T = pow(a, (p-1) shr 1, p) 

proc tonelliShanks*[T:SomeInteger](n,p:T): T =
  # Check that n is indeed a square
  if legendre(n,p) != 1:
    raise newException(ArithmeticError, "Not a square")

  # factor out power of 2 from p-1
  var q = p - 1
  var s = 0
  while (q and 1) == 0:
    s += 1
    q = q shr 1 

  if s == 1: 
    return pow(n, (p+1) shr 2, p)
    
  # Select a non-square z such as (z | p) = -1
  var z = 2 
  while legendre(z,p) != p - 1: 
    z += 1

  var 
    c = pow(z, q, p)
    t = pow(n, q, p)
    m = s
  result = pow(n, (q+1) shr 1, p)
  while t != 1:
    var 
      i = 1
      z = t * t mod p 
    while z != 1 and i < m-1:
      i += 1
      z = z * z mod p 

    var b = pow(c, 1 shl (m-i-1), p)
    c = b * b mod p 
    t = t * c mod p 
    m = i 
    result = result * b mod p 

when isMainModule: 
  proc run(n,p:SomeInteger) = 
    try: 
      let r = tonelliShanks(n,p)
      echo r, " ", p-r
    except ArithmeticError:
      echo getCurrentExceptionMsg()

  run(10,13)
  run(56,101)
  run(1030, 10009)
  run(1032, 10009)
  run(44402, 100049) 
  run(665820697, 1000000009)

```


output:

```txt

7 6
37 64
1632 8377
Not a square
30468 69581
378633312 621366697

```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use bigint;
use ntheory qw(is_prime powmod kronecker);

sub tonelli_shanks {
    my($n,$p) = @_;
    return if kronecker($n,$p) <= 0;
    my $Q = $p - 1;
    my $S = 0;
    $Q >>= 1 and $S++ while 0 == $Q%2;
    return powmod($n,int(($p+1)/4), $p) if $S == 1;

    my $c;
    for $n (2..$p) {
        next if kronecker($n,$p) >= 0;
        $c = powmod($n, $Q, $p);
        last;
    }

    my $R = powmod($n, ($Q+1) >> 1, $p ); # ?
    my $t = powmod($n, $Q, $p );
    while (($t-1) % $p) {
        my $b;
        my $t2 = $t**2 % $p;
        for (1 .. $S) {
            if (0 == ($t2-1)%$p) {
                $b = powmod($c, 1 << ($S-1-$_), $p);
                $S = $_;
                last;
            }
            $t2 = $t2**2 % $p;
        }
        $R = ($R * $b) % $p;
        $c = $b**2 % $p;
        $t = ($t * $c) % $p;
    }
    $R;
}

my @tests = (
    (10, 13),
    (56, 101),
    (1030, 10009),
    (1032, 10009),
    (44402, 100049),
    (665820697, 1000000009),
    (881398088036, 1000000000039),
);

while (@tests) {
    $n = shift @tests;
    $p = shift @tests;
    my $t = tonelli_shanks($n, $p);
    if (!$t or ($t**2 - $n) % $p) {
        printf "No solution for (%d, %d)\n", $n, $p;
    } else {
        printf "Roots of %d are (%d, %d) mod %d\n", $n, $t, $p-$t, $p;
    }
}
```

{{out}}

```txt
Roots of 10 are (7, 6) mod 13
Roots of 56 are (37, 64) mod 101
Roots of 1030 are (1632, 8377) mod 10009
No solution for (1032, 10009)
Roots of 44402 are (30468, 69581) mod 100049
Roots of 665820697 are (378633312, 621366697) mod 1000000009
Roots of 881398088036 are (791399408049, 208600591990) mod 1000000000039
```



## Perl 6

{{works with|Rakudo|2018.04}}
Translation of the Wikipedia pseudocode, heavily influenced by Sidef and Python.


```perl6
#  Legendre operator (𝑛│𝑝)
sub infix:<│> (Int \𝑛, Int \𝑝 where 𝑝.is-prime && (𝑝 != 2)) {
    given 𝑛.expmod( (𝑝-1) div 2, 𝑝 ) {
        when 0  {  0 }
        when 1  {  1 }
        default { -1 }
    }
}

sub tonelli-shanks ( \𝑛, \𝑝 where (𝑛│𝑝) > 0 ) {
    my $𝑄 = 𝑝 - 1;
    my $𝑆 = 0;
    $𝑄 +>= 1 and $𝑆++ while $𝑄 %% 2;
    return 𝑛.expmod((𝑝+1) div 4, 𝑝) if $𝑆 == 1;
    my $𝑐 = ((2..𝑝).first: (*│𝑝) < 0).expmod($𝑄, 𝑝);
    my $𝑅 = 𝑛.expmod( ($𝑄+1) +> 1, 𝑝 );
    my $𝑡 = 𝑛.expmod( $𝑄, 𝑝 );
    while ($𝑡-1) % 𝑝 {
        my $b;
        my $𝑡2 = $𝑡² % 𝑝;
        for 1 .. $𝑆 {
            if ($𝑡2-1) %% 𝑝 {
                $b = $𝑐.expmod(1 +< ($𝑆-1-$_), 𝑝);
                $𝑆 = $_;
                last;
            }
            $𝑡2 = $𝑡2² % 𝑝;
        }
        $𝑅 = ($𝑅 * $b) % 𝑝;
        $𝑐 = $b² % 𝑝;
        $𝑡 = ($𝑡 * $𝑐) % 𝑝;
    }
    $𝑅;
}

my @tests = (
    (10, 13),
    (56, 101),
    (1030, 10009),
    (1032, 10009),
    (44402, 100049),
    (665820697, 1000000009),
    (881398088036, 1000000000039),
    (41660815127637347468140745042827704103445750172002,
      100000000000000000000000000000000000000000000000577)
);

 for @tests -> ($n, $p) {
    try my $t = tonelli-shanks($n, $p);
    say "No solution for ({$n}, {$p})." and next if !$t or ($t² - $n) % $p;
    say "Roots of $n are ($t, {$p-$t}) mod $p";
}
```


{{out}}

```txt
Roots of 10 are (7, 6) mod 13
Roots of 56 are (37, 64) mod 101
Roots of 1030 are (1632, 8377) mod 10009
No solution for (1032, 10009).
Roots of 44402 are (30468, 69581) mod 100049
Roots of 665820697 are (378633312, 621366697) mod 1000000009
Roots of 881398088036 are (791399408049, 208600591990) mod 1000000000039
Roots of 41660815127637347468140745042827704103445750172002 are (32102985369940620849741983987300038903725266634508, 67897014630059379150258016012699961096274733366069) mod 100000000000000000000000000000000000000000000000577
```



## Phix

{{trans|C#}}
{{libheader|mpfr}}

```Phix
include mpfr.e
 
function ts(string ns, ps)
    mpz n = mpz_init(ns),
        p = mpz_init(ps),
        t = mpz_init(),
        r = mpz_init(),
        pm1 = mpz_init(),
        pm2 = mpz_init()
    mpz_sub_ui(pm1,p,1)                 -- pm1 = p-1
    mpz_fdiv_q_2exp(pm2,pm1,1)          -- pm2 = pm1/2
    mpz_powm(t,n,pm2,p)                 -- t = mod(n^pm2,p)
    if mpz_cmp_si(t,1)!=0 then
        return "No solution exists"
    end if
    mpz q = mpz_init_set(pm1)
    integer ss = 0
    while mpz_even(q) do
        ss += 1
        mpz_fdiv_q_2exp(q,q,1)          -- q/=2
    end while
    if ss=1 then
        mpz_add_ui(t,p,1)
        mpz_fdiv_q_2exp(t,t,2)
        mpz_powm(r,n,t,p)               -- r = mod(n^((p+1)/4),p)
    else
        mpz z = mpz_init(2)
        while true do
            mpz_powm(t,z,pm2,p)         -- t = mod(z^pm2,p)
            if mpz_cmp(t,pm1)=0 then exit end if
            mpz_add_ui(z,z,1)           -- z+= 1
        end while
        mpz {b,c,zz} = mpz_inits(3)
        mpz_powm(c,z,q,p)               -- c = mod(z^q,p)
        mpz_add_ui(t,q,1)
        mpz_fdiv_q_2exp(t,t,1)
        mpz_powm(r,n,t,p)               -- r = mod(n^((q+1)/2),p)
        mpz_powm(t,n,q,p)               -- t = mod(n^q,p)
        integer m = ss
        while mpz_cmp_si(t,1) do        -- t!=1
            integer i = 0
            mpz_set(zz,t)
            while mpz_cmp_si(zz,1)!=0 and i<m-1 do
                mpz_powm_ui(zz,zz,2,p)  -- zz = mod(zz^2,p)
                i += 1
            end while
            mpz_set(b,c)
            integer e = m-i-1
            while e>0 do
                mpz_powm_ui(b,b,2,p)    -- b = mod(b^2,p)
                e -= 1
            end while
            mpz_mul(r,r,b)
            mpz_mod(r,r,p)              -- r = mod(r*b,p)
            mpz_powm_ui(c,b,2,p)        -- c = mod(b^2,p)
            mpz_mul(t,t,c)
            mpz_mod(t,t,p)              -- t = mod(t*c,p)
            m = i
        end while
    end if
    mpz_sub(p,p,r)
    return mpz_get_str(r)&" and "&mpz_get_str(p)
end function
 
constant tests = {{"10","13"},
                  {"56","101"},
                  {"1030","10009"},
                  {"1032","10009"},
                  {"44402","100049"},
                  {"665820697","1000000009"},
                  {"881398088036","1000000000039"},
                  {"41660815127637347468140745042827704103445750172002",
                   sprintf("1%s577",repeat('0',47))}} -- 10^50+577
 
for i=1 to length(tests) do
    string {p1,p2} = tests[i]   
    printf(1,"For n = %s and p = %s, %s\n",{p1,p2,ts(p1,p2)})
end for
```

{{out}}

```txt

For n = 10 and p = 13, 7 and 6
For n = 56 and p = 101, 37 and 64
For n = 1030 and p = 10009, 1632 and 8377
For n = 1032 and p = 10009, No solution exists
For n = 44402 and p = 100049, 30468 and 69581
For n = 665820697 and p = 1000000009, 378633312 and 621366697
For n = 881398088036 and p = 1000000000039, 791399408049 and 208600591990
For n = 41660815127637347468140745042827704103445750172002 and p = 100000000000000000000000000000000000000000000000577, 
        32102985369940620849741983987300038903725266634508 and 67897014630059379150258016012699961096274733366069

```



## PicoLisp

{{trans|Go}}

```PicoLisp
# from @lib/rsa.l
(de **Mod (X Y N)
   (let M 1
      (loop
         (when (bit? 1 Y)
            (setq M (% (* M X) N)) )
         (T (=0 (setq Y (>> 1 Y)))
            M )
         (setq X (% (* X X) N)) ) ) )
(de legendre (N P)
   (**Mod N (/ (dec P) 2) P) )
(de ts (N P)
   (and
      (=1 (legendre N P))
      (let
         (Q (dec P)
            S 0
            Z 0
            C 0
            R 0
            D 0
            M 0
            B 0
            I 0 )
         (until (bit? 1 Q)
            (setq Q (>> 1 Q))
            (inc 'S) )
         (if (=1 S)
            (list
               (setq @@ (**Mod N (/ (inc P) 4) P))
               (- P @@) )
            (setq Z 2)
            (until (= (legendre Z P) (dec P))
               (inc 'Z) )
            (setq
               C (**Mod Z Q P)
               R (**Mod N (/ (inc Q) 2) P)
               D (**Mod N Q P)
               M S )
            (until (=1 D)
               (zero I)
               (for
                  (Z
                     D
                     (and (<> Z 1) (< I (dec M)))
                     (setq Z (% (* Z Z) P)) )
                  (inc 'I) )
               (setq B C)
               (for
                  (Z
                     (- M I 1)
                     (> Z 0) (dec Z) )
                  (setq B (% (* B B) P)) )
               (setq
                  R (% (* R B) P)
                  C (% (* B B) P)
                  D (% (* D C) P)
                  M I ) )
            (list R (- P R)) ) ) ) )

(println (ts 10 13))
(println (ts 56 101))
(println (ts 1030 10009))
(println (ts 1032 10009))
(println (ts 44402 100049))
(println (ts 665820697 1000000009))
(println (ts 881398088036 1000000000039))
(println (ts 41660815127637347468140745042827704103445750172002 (+ (** 10 50) 577)))
```


{{out}}

```txt

(7 6)
(37 64)
(1632 8377)
NIL
(30468 69581)
(378633312 621366697)
(791399408049 208600591990)
(32102985369940620849741983987300038903725266634508 67897014630059379150258016012699961096274733366069)

```



## Python

{{trans|EchoLisp}}
{{works with|Python|3}}

```python
def legendre(a, p):
    return pow(a, (p - 1) // 2, p)

def tonelli(n, p):
    assert legendre(n, p) == 1, "not a square (mod p)"
    q = p - 1
    s = 0
    while q % 2 == 0:
        q //= 2
        s += 1
    if s == 1:
        return pow(n, (p + 1) // 4, p)
    for z in range(2, p):
        if p - 1 == legendre(z, p):
            break
    c = pow(z, q, p)
    r = pow(n, (q + 1) // 2, p)
    t = pow(n, q, p)
    m = s
    t2 = 0
    while (t - 1) % p != 0:
        t2 = (t * t) % p
        for i in range(1, m):
            if (t2 - 1) % p == 0:
                break
            t2 = (t2 * t2) % p
        b = pow(c, 1 << (m - i - 1), p)
        r = (r * b) % p
        c = (b * b) % p
        t = (t * c) % p
        m = i
    return r

if __name__ == '__main__':
    ttest = [(10, 13), (56, 101), (1030, 10009), (44402, 100049),
	     (665820697, 1000000009), (881398088036, 1000000000039),
             (41660815127637347468140745042827704103445750172002, 10**50 + 577)]
    for n, p in ttest:
        r = tonelli(n, p)
        assert (r * r - n) % p == 0
        print("n = %d p = %d" % (n, p))
        print("\t  roots : %d %d" % (r, p - r))
```

{{out}}

```txt

n = 10 p = 13
	  roots : 7 6
n = 56 p = 101
	  roots : 37 64
n = 1030 p = 10009
	  roots : 1632 8377
n = 44402 p = 100049
	  roots : 30468 69581
n = 665820697 p = 1000000009
	  roots : 378633312 621366697
n = 881398088036 p = 1000000000039
	  roots : 791399408049 208600591990
n = 41660815127637347468140745042827704103445750172002 p = 100000000000000000000000000000000000000000000000577
	  roots : 32102985369940620849741983987300038903725266634508 67897014630059379150258016012699961096274733366069

```



## Racket

{{trans|EchoLisp}}

```racket
#lang racket

(require math/number-theory)

(define (Legendre a p)  
  (modexpt a (quotient (sub1 p) 2)))
 
(define (Tonelli n p (err (λ (n p) (error "not a square (mod p)" (list n p)))))
  (with-modulus p
    (unless (= 1 (Legendre n p)) (err n p))

    (define-values (q s)
      (let even?-q-loop ((q (sub1 p)) (s 0))
        (if (even? q)
            (even?-q-loop (quotient q 2) (add1 s))
            (values q s))))
    
    (cond
      [(= s 1)
       (modexpt n (/ (add1 p) 4))]
      [else
       (define z (for/first ((z (in-range 2 p)) #:when (= (sub1 p) (Legendre z p))) z)) 
       (let loop ((c (modexpt z q))
                  (r (modexpt n (quotient (add1 q) 2)))
                  (t (modexpt n q))
                  (m s))
         (cond
           [(mod= 1 t)
            r]
           [else
            (define-values (t2 m′) (for/fold ((t2 (modsqr t)) (i 1))
                                             ((j (in-range 1 m)) #:final (mod= t2 1))
                                     (values (modsqr t2) j)))
            (define b (modexpt c (expt 2 (- m m′ 1))))
            (define c′ (modsqr b))
            (loop c′ (mod* r b) (mod* t c′) m′)]))])))

(module+ test
  (require rackunit)

  (define ttest 
    `((10 13)
      (56 101)
      (1030 10009)
      (44402 100049)  
      (665820697 1000000009) 
      (881398088036  1000000000039)
      (41660815127637347468140745042827704103445750172002
       ,(+ #e1e50 577))))  

  (define (task ttest)
    (for ((test ttest))
      (define n (first test))
      (define p (second test))
      (define r (Tonelli n p))
      (printf "n = ~a p = ~a~%  roots : ~a ~a~%" n p r (- p r))))

  (task ttest)

  (check-exn exn:fail? (λ () (Tonelli 1032 1009))))
```


{{out}}


```txt
n = 10 p = 13
  roots : 7 6
n = 56 p = 101
  roots : 37 64
n = 1030 p = 10009
  roots : 1632 8377
n = 44402 p = 100049
  roots : 30468 69581
n = 665820697 p = 1000000009
  roots : 378633312 621366697
n = 881398088036 p = 1000000000039
  roots : 791399408049 208600591990
n = 41660815127637347468140745042827704103445750172002 p = 100000000000000000000000000000000000000000000000577
  roots : 32102985369940620849741983987300038903725266634508 67897014630059379150258016012699961096274733366069
```



## REXX

{{trans|Python}}
The large numbers cannot reasonably be handled by the pow function shown here.

```rexx
/* REXX (required by some interpreters) */
Numeric Digits 1000000
ttest ='[(10, 13), (56, 101), (1030, 10009), (44402, 100049)]'
Do While pos('(',ttest)>0
  Parse Var ttest '(' n ',' p ')' ttest
  r = tonelli(n, p)
  Say "n =" n "p =" p
  Say "          roots :" r (p - r)
  End
Exit

legendre: Procedure
  Parse Arg a, p
  return pow(a, (p - 1) % 2, p)

tonelli: Procedure
  Parse Arg n, p
  q = p - 1
  s = 0
  Do while q // 2 == 0
    q = q % 2
    s = s+1
    End
  if s == 1 Then
    return pow(n, (p + 1) % 4, p)
  Do z=2 To p
    if p - 1 == legendre(z, p) Then
      Leave
    End
  c = pow(z, q, p)
  r = pow(n, (q + 1) / 2, p)
  t = pow(n, q, p)
  m = s
  t2 = 0
  Do while (t - 1) // p <> 0
    t2 = (t * t) // p
    Do i=1 To m
      if (t2 - 1) // p == 0 Then
        Leave
      t2 = (t2 * t2) // p
      End
    y=2**(m - i - 1)
    b = pow(c, y, p)
    If b=10008 Then Trace ?R
    r = (r * b) // p
    c = (b * b) // p
    t = (t * c) // p
    m = i
    End
  return r
pow: Procedure
  Parse Arg x,y,z
  If y>0 Then
    p=x**y
  Else p=x
  If z>'' Then
    p=p//z
  Return p
```

{{out}}

```txt
n = 10 p =  13
          roots : 7 6
n = 56 p =  101
          roots : 37 64
n = 1030 p =  10009
          roots : 1632 8377
n = 44402 p =  100049
          roots : 30468 69581
```



## Sidef

{{trans|Python}}

```ruby
func tonelli(n, p) {
    legendre(n, p) == 1 || die "not a square (mod p)"
    var q = p-1
    var s = valuation(q, 2)
    s == 1 ? return(powmod(n, (p + 1) >> 2, p)) : (q >>= s)
    var c = powmod(2 ..^ p -> first {|z| legendre(z, p) == -1}, q, p)
    var r = powmod(n, (q + 1) >> 1, p)
    var t = powmod(n, q, p)
    var m = s
    var t2 = 0
    while (!p.divides(t - 1)) {
        t2 = ((t * t) % p)
        var b
        for i in (1 ..^ m) {
            if (p.divides(t2 - 1)) {
                b = powmod(c, 1 << (m - i - 1), p)
                m = i
                break
            }
            t2 = ((t2 * t2) % p)
        }

        r = ((r * b) % p)
        c = ((b * b) % p)
        t = ((t * c) % p)
    }
    return r
}

var tests = [
    [10, 13], [56, 101], [1030, 10009], [44402, 100049],
    [665820697, 1000000009], [881398088036, 1000000000039],
    [41660815127637347468140745042827704103445750172002, 10**50 + 577],
]

for n,p in tests {
    var r = tonelli(n, p)
    assert((r*r - n) % p == 0)
    say "Roots of #{n} are (#{r}, #{p-r}) mod #{p}"
}
```

{{out}}

```txt

Roots of 10 are (7, 6) mod 13
Roots of 56 are (37, 64) mod 101
Roots of 1030 are (1632, 8377) mod 10009
Roots of 44402 are (30468, 69581) mod 100049
Roots of 665820697 are (378633312, 621366697) mod 1000000009
Roots of 881398088036 are (791399408049, 208600591990) mod 1000000000039
Roots of 41660815127637347468140745042827704103445750172002 are (32102985369940620849741983987300038903725266634508, 67897014630059379150258016012699961096274733366069) mod 100000000000000000000000000000000000000000000000577

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Numerics

Module Module1

    Class Solution
        ReadOnly root1 As BigInteger
        ReadOnly root2 As BigInteger
        ReadOnly exists As Boolean

        Sub New(r1 As BigInteger, r2 As BigInteger, e As Boolean)
            root1 = r1
            root2 = r2
            exists = e
        End Sub

        Public Function GetRoot1() As BigInteger
            Return root1
        End Function

        Public Function GetRoot2() As BigInteger
            Return root2
        End Function

        Public Function GetExists() As Boolean
            Return exists
        End Function
    End Class

    Function Ts(n As BigInteger, p As BigInteger) As Solution
        If BigInteger.ModPow(n, (p - 1) / 2, p) <> 1 Then
            Return New Solution(0, 0, False)
        End If

        Dim q As BigInteger = p - 1
        Dim ss = BigInteger.Zero
        While (q Mod 2) = 0
            ss += 1
            q >>= 1
        End While

        If ss = 1 Then
            Dim r1 = BigInteger.ModPow(n, (p + 1) / 4, p)
            Return New Solution(r1, p - r1, True)
        End If

        Dim z As BigInteger = 2
        While BigInteger.ModPow(z, (p - 1) / 2, p) <> p - 1
            z += 1
        End While
        Dim c = BigInteger.ModPow(z, q, p)
        Dim r = BigInteger.ModPow(n, (q + 1) / 2, p)
        Dim t = BigInteger.ModPow(n, q, p)
        Dim m = ss

        Do
            If t = 1 Then
                Return New Solution(r, p - r, True)
            End If
            Dim i = BigInteger.Zero
            Dim zz = t
            While zz <> 1 AndAlso i < (m - 1)
                zz = zz * zz Mod p
                i += 1
            End While
            Dim b = c
            Dim e = m - i - 1
            While e > 0
                b = b * b Mod p
                e = e - 1
            End While
            r = r * b Mod p
            c = b * b Mod p
            t = t * c Mod p
            m = i
        Loop
    End Function

    Sub Main()
        Dim pairs = New List(Of Tuple(Of Long, Long)) From {
            New Tuple(Of Long, Long)(10, 13),
            New Tuple(Of Long, Long)(56, 101),
            New Tuple(Of Long, Long)(1030, 10009),
            New Tuple(Of Long, Long)(1032, 10009),
            New Tuple(Of Long, Long)(44402, 100049),
            New Tuple(Of Long, Long)(665820697, 1000000009),
            New Tuple(Of Long, Long)(881398088036, 1000000000039)
        }

        For Each pair In pairs
            Dim sol = Ts(pair.Item1, pair.Item2)
            Console.WriteLine("n = {0}", pair.Item1)
            Console.WriteLine("p = {0}", pair.Item2)
            If sol.GetExists() Then
                Console.WriteLine("root1 = {0}", sol.GetRoot1())
                Console.WriteLine("root2 = {0}", sol.GetRoot2())
            Else
                Console.WriteLine("No solution exists")
            End If
            Console.WriteLine()
        Next

        Dim bn = BigInteger.Parse("41660815127637347468140745042827704103445750172002")
        Dim bp = BigInteger.Pow(10, 50) + 577
        Dim bsol = Ts(bn, bp)
        Console.WriteLine("n = {0}", bn)
        Console.WriteLine("p = {0}", bp)
        If bsol.GetExists() Then
            Console.WriteLine("root1 = {0}", bsol.GetRoot1())
            Console.WriteLine("root2 = {0}", bsol.GetRoot2())
        Else
            Console.WriteLine("No solution exists")
        End If
    End Sub

End Module
```

{{out}}

```txt
n = 10
p = 13
root1 = 7
root2 = 6

n = 56
p = 101
root1 = 37
root2 = 64

n = 1030
p = 10009
root1 = 1632
root2 = 8377

n = 1032
p = 10009
No solution exists

n = 44402
p = 100049
root1 = 30468
root2 = 69581

n = 665820697
p = 1000000009
root1 = 378633312
root2 = 621366697

n = 881398088036
p = 1000000000039
root1 = 791399408049
root2 = 208600591990

n = 41660815127637347468140745042827704103445750172002
p = 100000000000000000000000000000000000000000000000577
root1 = 32102985369940620849741983987300038903725266634508
root2 = 67897014630059379150258016012699961096274733366069
```



## zkl

{{trans|EchoLisp}}

```zkl
var BN=Import("zklBigNum");
fcn modEq(a,b,p) { (a-b)%p==0 }
fcn legendre(a,p){ a.powm((p - 1)/2,p) }
 
fcn tonelli(n,p){ //(BigInt,Int|BigInt)
   _assert_(legendre(n,p)==1, "not a square (mod p)"+vm.arglist);
   q,s:=p-1,0;
   while(q.isEven){ q/=2; s+=1; }
   if(s==1) return(n.powm((p+1)/4,p));
   z:=[BN(2)..p].filter1('wrap(z){ legendre(z,p)==(p-1) });
   c,r,t,m,t2:=z.powm(q,p), n.powm((q+1)/2,p), n.powm(q,p), s, 0;
   while(not modEq(t,1,p)){
      t2=(t*t)%p;
      i:=1; while(not modEq(t2,1,p)){ i+=1; t2=(t2*t2)%p; } // assert(i<m)
      b:=c.powm(BN(1).shiftLeft(m-i-1), p);
      r,c,t,m = (r*b)%p, (b*b)%p, (t*c)%p, i;
   }
   r
}
```


```zkl
ttest:=T(T(10,13), T(56,101), T(1030,10009), T(44402,100049),
   T(665820697,1000000009), T(881398088036,1000000000039),
   T("41660815127637347468140745042827704103445750172002", BN(10).pow(50) + 577),
   T(1032,10009) );
foreach n,p in (ttest){ n=BN(n);
   r:=tonelli(n,p);
   assert((r*r-n)%p == 0,"(r*r-n)%p == 0 : %s,%s,%s-->%s".fmt(r,n,p,(r*r-n)%p));
   println("n=%d p=%d".fmt(n,p));
   println("   roots: %d %d".fmt(r, p-r));
}
```

{{out}}

```txt

n=10 p=13
   roots: 7 6
n=56 p=101
   roots: 37 64
n=1030 p=10009
   roots: 1632 8377
n=44402 p=100049
   roots: 30468 69581
n=665820697 p=1000000009
   roots: 378633312 621366697
n=881398088036 p=1000000000039
   roots: 791399408049 208600591990
n=41660815127637347468140745042827704103445750172002 p=100000000000000000000000000000000000000000000000577
   roots: 32102985369940620849741983987300038903725266634508 67897014630059379150258016012699961096274733366069
VM#1 caught this unhandled exception:
   AssertionError : not a square (mod p)L(1032,10009)
Stack trace for VM#1 ():
   bbb.assert addr:13  args(2) reg(0) 
   bbb.tonelli addr:29  args(2) reg(10) R
...

```


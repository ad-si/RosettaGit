+++
title = "Cipolla's algorithm"
description = ""
date = 2019-08-03T19:30:39Z
aliases = []
[extra]
id = 20672
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

'''Cipolla's algorithm''' 

Solve '''x² ≡ n (mod p)'''

In computational number theory, [https://en.wikipedia.org/wiki/Cipolla's_algorithm Cipolla's algorithm] is a technique for solving an equation of the form '''x² ≡ n (mod p)''', where p is an odd prime and x ,n ∊ Fp = {0, 1, ... p-1}.

To apply the algorithm we need the Legendre symbol, and arithmetic in  Fp².

Legendre symbol

* The Legendre symbol ( a | p) denotes the value of  a ^ ((p-1)/2) (mod p)
* (a | p) ≡  1 if a is a square (mod p)
* (a | p) ≡  -1  if a is not a square (mod p)
* (a | p) ≡ 0 is a ≡ 0 


Arithmetic in Fp²

Let ω a symbol such as ω² is a member of Fp and not a square,  x and y members of Fp. The set Fp² is defined as {x + ω y }. The subset { x + 0 ω} of Fp² is Fp. Fp² is somewhat equivalent to the field of complex number, with  ω analoguous to i, and i² = -1 .  Remembering that all operations are modulo p, addition, multiplication and exponentiation in Fp² are defined as :

* (x1 + ω y1) +  (x2 + ω y2) := (x1 + x2 + ω (y1 + y2))
* (x1 + ω y1) *  (x2 + ω y2) := (x1*x2 + y1*y2*ω²) + ω (x1*y2 + x2*y1)
** (0 + ω) * (0 + ω) := (ω² + 0 ω) ≡ ω² in Fp
* (x1 + ω y1) ^ n := (x + ω y) * (x + ω y) * ... ( n times) (1)


'''Algorithm pseudo-code'''

* Input : p an odd prime, and n ≠ 0 in Fp
* Step 0. Check that n is indeed a square  : (n | p) must be ≡ 1
* Step 1. Find, by trial and error, an a > 0 such as (a² - n) is '''not''' a square : (a²-n | p) must be ≡ -1.
* Step 2.  Let  ω² = a² - n. Compute, in Fp2 :  (a + ω) ^ ((p + 1)/2) (mod p) 
To compute this step, use a pair of numbers, initially [a,1], and use repeated "multiplication" which is defined such that [c,d] times [e,f] is (mod p) [ c*c + ω²*f*f, d*e + c*f ].
* Step 3. Check that the result is ≡  x +  0 * ω  in  Fp2, that is x in Fp.
* Step 4. Output the two positive solutions, x and p - x  (mod p).
* Step 5. Check that  x * x ≡ n (mod p)


'''Example''' from Wikipedia


```txt

n := 10 , p := 13
Legendre(10,13) → 1         // 10 is indeed a square
a := 2                      // try
ω² := a*a - 10             // ≡ 7 ≡ -6
Legendre (ω² , 13) → -1    // ok - not square
(2 + ω) ^ 7 → 6 + 0 ω      // by modular exponentiation (1)
                            // 6 and (13 - 6) = 7 are solutions
(6 * 6) % 13 → 10           // = n . Checked.

```


'''Task'''

Implement the above. 

Find solutions (if any) for 
* n = 10 p = 13
* n = 56 p = 101
* n = 8218 p = 10007
* n = 8219 p = 10007
* n =  331575 p =  1000003


'''Extra credit'''

* n     665165880     p     1000000007    
* n     881398088036     p     1000000000039    
* n  =  34035243914635549601583369544560650254325084643201     p  = 10^50 + 151   	


See also:
* [[Modular exponentiation]]
* [[Tonelli-Shanks algorithm]]




=={{header|C#|C sharp}}==

```csharp
using System;
using System.Numerics;

namespace CipollaAlgorithm {
    class Program {
        static readonly BigInteger BIG = BigInteger.Pow(10, 50) + 151;

        private static Tuple<BigInteger, BigInteger, bool> C(string ns, string ps) {
            BigInteger n = BigInteger.Parse(ns);
            BigInteger p = ps.Length > 0 ? BigInteger.Parse(ps) : BIG;

            // Legendre symbol. Returns 1, 0, or p-1
            BigInteger ls(BigInteger a0) => BigInteger.ModPow(a0, (p - 1) / 2, p);

            // Step 0: validate arguments
            if (ls(n) != 1) {
                return new Tuple<BigInteger, BigInteger, bool>(0, 0, false);
            }

            // Step 1: Find a, omega2
            BigInteger a = 0;
            BigInteger omega2;
            while (true) {
                omega2 = (a * a + p - n) % p;
                if (ls(omega2) == p - 1) {
                    break;
                }
                a += 1;
            }

            // Multiplication in Fp2
            BigInteger finalOmega = omega2;
            Tuple<BigInteger, BigInteger> mul(Tuple<BigInteger, BigInteger> aa, Tuple<BigInteger, BigInteger> bb) {
                return new Tuple<BigInteger, BigInteger>(
                    (aa.Item1 * bb.Item1 + aa.Item2 * bb.Item2 * finalOmega) % p,
                    (aa.Item1 * bb.Item2 + bb.Item1 * aa.Item2) % p
                );
            }

            // Step 2: Compute power
            Tuple<BigInteger, BigInteger> r = new Tuple<BigInteger, BigInteger>(1, 0);
            Tuple<BigInteger, BigInteger> s = new Tuple<BigInteger, BigInteger>(a, 1);
            BigInteger nn = ((p + 1) >> 1) % p;
            while (nn > 0) {
                if ((nn & 1) == 1) {
                    r = mul(r, s);
                }
                s = mul(s, s);
                nn >>= 1;
            }

            // Step 3: Check x in Fp
            if (r.Item2 != 0) {
                return new Tuple<BigInteger, BigInteger, bool>(0, 0, false);
            }

            // Step 5: Check x * x = n
            if (r.Item1 * r.Item1 % p != n) {
                return new Tuple<BigInteger, BigInteger, bool>(0, 0, false);
            }

            // Step 4: Solutions
            return new Tuple<BigInteger, BigInteger, bool>(r.Item1, p - r.Item1, true);
        }

        static void Main(string[] args) {
            Console.WriteLine(C("10", "13"));
            Console.WriteLine(C("56", "101"));
            Console.WriteLine(C("8218", "10007"));
            Console.WriteLine(C("8219", "10007"));
            Console.WriteLine(C("331575", "1000003"));
            Console.WriteLine(C("665165880", "1000000007"));
            Console.WriteLine(C("881398088036", "1000000000039"));
            Console.WriteLine(C("34035243914635549601583369544560650254325084643201", ""));
        }
    }
}
```

{{out}}

```txt
(6, 7, True)
(37, 64, True)
(9872, 135, True)
(0, 0, False)
(855842, 144161, True)
(475131702, 524868305, True)
(791399408049, 208600591990, True)
(82563118828090362261378993957450213573687113690751, 17436881171909637738621006042549786426312886309400, True)
```



## D

{{trans|Kotlin}}

```D
import std.bigint;
import std.stdio;
import std.typecons;

enum BIGZERO = BigInt(0);

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

alias Point = Tuple!(BigInt, "x", BigInt, "y");
alias Triple = Tuple!(BigInt, "x", BigInt, "y", bool, "b");

Triple c(string ns, string ps) {
    auto n = BigInt(ns);
    BigInt p;
    if (ps.length > 0) {
        p = BigInt(ps);
    } else {
        p = BigInt(10)^^50 + 151;
    }

    // Legendre symbol, returns 1, 0 or p - 1
    auto ls = (BigInt a) => modPow(a, (p-1)/2, p);

    // Step 0, validate arguments
    if (ls(n) != 1) return Triple(BIGZERO, BIGZERO, false);

    // Step 1, find a, omega2
    auto a = BIGZERO;
    BigInt omega2;
    while (true) {
        omega2 = (a * a + p - n) % p;
        if (ls(omega2) == p-1) break;
        a++;
    }

    // multiplication in Fp2
    auto mul = (Point aa, Point bb) => Point(
        (aa.x * bb.x + aa.y * bb.y * omega2) % p,
        (aa.x * bb.y + bb.x * aa.y) % p
    );

    // Step 2, compute power
    auto r = Point(BigInt(1), BIGZERO);
    auto s = Point(a, BigInt(1));
    auto nn = ((p+1) >> 1) % p;
    while (nn > 0) {
        if ((nn & 1) == 1) r = mul(r, s);
        s = mul(s, s);
        nn >>= 1;
    }

    // Step 3, check x in Fp
    if (r.y != 0) return Triple(BIGZERO, BIGZERO, false);

    // Step 5, check x * x = n
    if (r.x*r.x%p!=n) return Triple(BIGZERO, BIGZERO, false);

    // Step 4, solutions
    return Triple(r.x, p-r.x, true);
}

void main() {
    writeln(c("10", "13"));
    writeln(c("56", "101"));
    writeln(c("8218", "10007"));
    writeln(c("8219", "10007"));
    writeln(c("331575", "1000003"));
    writeln(c("665165880", "1000000007"));
    writeln(c("881398088036", "1000000000039"));
    writeln(c("34035243914635549601583369544560650254325084643201", ""));
}
```

{{out}}

```txt
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(6, 7, true)
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(37, 64, true)
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(9872, 135, true)
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(0, 0, false)
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(855842, 144161, true)
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(475131702, 524868305, true)
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(791399408049, 208600591990, true)
Tuple!(BigInt, "x", BigInt, "y", bool, "b")(82563118828090362261378993957450213573687113690751, 17436881171909637738621006042549786426312886309400, true)
```



## EchoLisp


```scheme

(lib 'struct)
(lib 'types)
(lib 'bigint)

;; test equality mod p
(define-syntax-rule (mod= a b p) 
	 (zero?  (% (- a b) p)))

(define (Legendre a p)  
	 (powmod a (/ (1- p) 2) p))

;; Arithmetic in Fp² 
(struct Fp² ( x y ))
;; a + b
(define (Fp²-add Fp²:a Fp²:b p ω2)
	(Fp² (% (+ a.x b.x) p) (% (+ a.y b.y) p)))
;; a * b
(define (Fp²-mul Fp²:a Fp²:b p ω2) 
	(Fp² (% (+ (* a.x b.x) (* ω2 a.y b.y)) p) (% (+ (* a.x b.y) (* a.y b.x)) p)))

;; a * a	
(define (Fp²-square Fp²:a p ω2)
	(Fp² (% (+ (* a.x a.x) (* ω2 a.y a.y)) p) (%  (* 2 a.x a.y)  p)))

;; a ^ n
(define (Fp²-pow Fp²:a n p ω2)
	(cond 
	((= 0 n) (Fp² 1 0))
	((= 1 n) (Fp² a.x a.y))
	((= 2 n) (Fp²-mul a a p ω2))
	((even? n) (Fp²-square (Fp²-pow a (/ n 2) p ω2) p ω2))
	(else (Fp²-mul a (Fp²-pow a (1- n) p ω2) p ω2))))

;; x^2 ≡ n (mod p) ?
(define (Cipolla n p) 
;; check n is a square
	(unless (= 1 (Legendre n p)) (error "not a square (mod p)" (list n p)))
;; iterate until suitable 'a' found
	(define a  
		(for ((t (in-range 2 p))) ;; t = tentative a
		  #:break (= (1- p)  (Legendre (- (* t t) n) p)) => t 
		))
	(define ω2 (- (* a a) n))
	;; (writeln 'a-> a 'ω2-> ω2 'ω-> 'ω)
	;; (Fp² a 1) = a + ω
	(define r   (Fp²-pow (Fp² a 1) (/ (1+ p) 2) p ω2))
	;; (writeln 'r r)
	(define x  (Fp²-x r))
	(assert (zero? (Fp²-y r))) ;; hope that ω has vanished
	(assert (mod= n (* x x) p)) ;; checking the result
	(printf "Roots of %d are (%d,%d)  (mod %d)" n  x  (% (- p x) p) p))

```

{{out}}

```txt

(Cipolla 10 13)
Roots of 10 are (6,7) (mod 13)
(% (* 6 6) 13) → 10 ;; checking

(Cipolla 56 101)
Roots of 56 are (37,64) (mod 101)

(Cipolla 8218 10007)
Roots of 8218 are (9872,135) (mod 10007)

Cipolla 8219 10007)
❌ error: not a square (mod p) (8219 10007)

(Cipolla 331575 1000003)
Roots of 331575 are (855842,144161) (mod 1000003)
(% ( * 855842 855842) 1000003) → 331575


```

=={{header|F_Sharp|F#}}==

### The function

This task uses [http://www.rosettacode.org/wiki/Extensible_prime_generator#The_function Extensible Prime Generator (F#)]

```fsharp

// Cipolla's algorithm. Nigel Galloway: June 16th., 2019
let Cipolla n g =
  let rec fN i g e l=match e with n when n=0I->i |_ when e%2I=1I->fN ((i*g)%l) ((g*g)%l) (e/2I) l |_-> fN i ((g*g)%l) (e/2I) l
  let rec fG g=match (n/g+g)>>>1 with n when bigint.Abs(g-n)>>>1<2I->n+1I |g->fG g
  let a,b=let rec fI i=let q=i*i-n in if fN 1I q ((g-1I)/2I) g>1I then (i,q) else fI (i+1I) in fI(fG (bigint(sqrt(double n))))
  let fE=Seq.unfold(fun(n,i)->Some((n,i),((n*n+i*i*b)%g,(2I*n*i)%g)))(a,1I)|>Seq.cache
  let rec fL Πn Πi α β=match 2I**α with
                        Ω when Ω<β->fL Πn Πi (α+1) β
                       |Ω when Ω>β->let n,i=Seq.item (α-1) fE in fL ((Πn*n+Πi*i*b)%g) ((Πn*i+Πi*n)%g) 0 (β-Ω/2I)    
                       |_->let n,i=Seq.item α fE in ((Πn*n+Πi*i*b)%g)
  if fN 1I n ((g-1I)/2I) g<>1I then None else Some(fL 1I 0I 0 ((g+1I)/2I))

```


### The Task


```fsharp

let test=[(10I,13I);(56I,101I);(8218I,10007I);(8219I,10007I);(331575I,1000003I);(665165880I,1000000007I);(881398088036I,1000000000039I);(34035243914635549601583369544560650254325084643201I,10I**50+151I)] 
test|>List.iter(fun(n,g)->match Cipolla n g with Some r->printfn "Cipolla %A %A -> %A (%A) check %A" n g r (g-r) ((r*r)%g) |_->printfn "Cipolla %A %A -> has no result" n g)

```

{{out}}

```txt

Cipolla 10 13 -> 7 (6) check 10
Cipolla 56 101 -> 64 (37) check 56
Cipolla 8218 10007 -> 135 (9872) check 8218
Cipolla 8219 10007 -> has no result
Cipolla 331575 1000003 -> 144161 (855842) check 331575
Cipolla 665165880 1000000007 -> 475131702 (524868305) check 665165880
Cipolla 881398088036 1000000000039 -> 208600591990 (791399408049) check 881398088036
Cipolla 34035243914635549601583369544560650254325084643201 100000000000000000000000000000000000000000000000151 -> 17436881171909637738621006042549786426312886309400 (82563118828090362261378993957450213573687113690751) check 34035243914635549601583369544560650254325084643201
Real: 00:00:00.089, CPU: 00:00:00.090, GC gen0: 2, gen1: 0

```



## FreeBASIC


### LongInt version

Had a close look at the EchoLisp code for step 2.
Used the FreeBASIC code from the Miller-Rabin task for prime testing. 

```freebasic
' version 08-04-2017
' compile with: fbc -s console
' maximum for p is 17 digits to be on the save side

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' But we have to define them for older versions.
#Ifndef TRUE
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Type fp2
    x As LongInt
    y As LongInt
End Type

Function mul_mod(a As ULongInt, b As ULongInt, modulus As ULongInt) As ULongInt
    ' returns a * b mod modulus
    Dim As ULongInt x, y = a mod modulus

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

Function fp2mul(a As fp2, b As fp2, p As LongInt, w2 As LongInt) As fp2

    Dim As fp2 answer
    Dim As ULongInt tmp1, tmp2
    ' needs to be broken down in smaller steps to avoid overflow
    ' answer.x = (a.x * b.x + a.y * b.y * w2) Mod p
    ' answer.y = (a.x * b.y + a.y * b.x) Mod p
    tmp1 = mul_mod(a.x, b.x, p)
    tmp2 = mul_mod(a.y, b.y, p)
    tmp2 = mul_mod(tmp2, w2, p)
    answer.x = (tmp1 + tmp2) Mod p
    tmp1 = mul_mod(a.x, b.y, p)
    tmp2 = mul_mod(a.y, b.x, p)
    answer.y = (tmp1 + tmp2) Mod p

    Return answer

End Function

Function fp2square(a As fp2, p As LongInt, w2 As LongInt) As fp2

    Return fp2mul(a, a, p, w2)

End Function

Function fp2pow(a As fp2, n As LongInt, p As LongInt, w2 As LongInt) As fp2

    If n = 0 Then Return Type (1, 0)
    If n = 1 Then Return a
    If n = 2 Then Return fp2square(a, p, w2)
    If (n And 1) = 0 Then
        Return fp2square(fp2pow(a, n \ 2, p, w2), p , w2)
    Else
        Return fp2mul(a, fp2pow(a, n -1, p, w2), p, w2)
    End If

End Function

' ------=< MAIN >=------

Data 10, 13, 56, 101, 8218, 10007,8219, 10007
Data 331575, 1000003, 665165880, 1000000007
Data 881398088036, 1000000000039

Randomize Timer
Dim As LongInt n, p, a, w2
Dim As LongInt i, x1, x2
Dim As fp2 answer

For i = 1 To 7

    Read n, p
    Print
    Print "Find solution for n =";n ; " and p =";p

    If p = 2 OrElse Isprime(p,15) = FALSE Then
        Print "No solution, p is not a odd prime"
        Continue For
    End If

    ' p is checked and is a odd prime
    If legendre_symbol(n, p) <> 1 Then
        Print n; " is not a square in F";Str(p)
        Continue For
    End If

    Do 
        Do
            a = Rnd * (p -2) +2
            w2 = a * a - n
        Loop Until legendre_symbol(w2, p) = -1

        answer = Type(a, 1)
        answer = fp2pow(answer, (p +1) \ 2, p, w2)
        If answer.y <> 0 Then Continue Do

        x1 = answer.x : x2 = p - x1
        If mul_mod(x1, x1, p) = n AndAlso mul_mod(x2, x2, p) = n Then
            Print "Solution found: x1 ="; x1; ", "; "x2 ="; x2
            Exit Do
        End If
    Loop            ' loop until solution is found

Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Find solution for n = 10 and p = 13
Solution found: x1 = 7, x2 = 6

Find solution for n = 56 and p = 101
Solution found: x1 = 37, x2 = 64

Find solution for n = 8218 and p = 10007
Solution found: x1 = 9872, x2 = 135

Find solution for n = 8219 and p = 10007
 8219 is not a square in F10007

Find solution for n = 331575 and p = 1000003
Solution found: x1 = 144161, x2 = 855842

Find solution for n = 665165880 and p = 1000000007
Solution found: x1 = 475131702, x2 = 524868305

Find solution for n = 881398088036 and p = 1000000000039
Solution found: x1 = 791399408049, x2 = 208600591990
```


### GMP version

{{libheader|GMP}}

```freebasic
' version 12-04-2017
' compile with: fbc -s console

#Include Once "gmp.bi"

Type fp2
    x As Mpz_ptr
    y As Mpz_ptr
End Type

Data "10", "13"
Data "56", "101"
Data "8218", "10007"
Data "8219", "10007"
Data "331575", "1000003"
Data "665165880", "1000000007"
Data "881398088036", "1000000000039"
Data "34035243914635549601583369544560650254325084643201"  ', 10^50 + 151

Function fp2mul(a As fp2, b As fp2, p As Mpz_ptr, w2 As Mpz_ptr) As fp2

    Dim As fp2 r
    r.x = Allocate(Len(__Mpz_struct)) : Mpz_init(r.x)
    r.y = Allocate(Len(__Mpz_struct)) : Mpz_init(r.y)

    Mpz_mul (r.x, a.y, b.y)
    Mpz_mul (r.x, r.x, w2)
    Mpz_addmul(r.x, a.x, b.x)
    Mpz_mod (r.x, r.x, p)
    Mpz_mul (r.y, a.x, b.y)
    Mpz_addmul(r.y, a.y, b.x)
    Mpz_mod (r.y, r.y, p)

    Return r

End Function

Function fp2square(a As fp2, p As Mpz_ptr, w2 As Mpz_ptr) As fp2

    Return fp2mul(a, a, p, w2)

End Function

Function fp2pow(a As fp2, n As Mpz_ptr, p As Mpz_ptr, w2 As Mpz_ptr) As fp2

    If Mpz_cmp_ui(n, 0) = 0 Then
        Mpz_set_ui(a.x, 1)
        Mpz_set_ui(a.y, 0)
        Return a
    End If
    If Mpz_cmp_ui(n, 1) = 0 Then Return a
    If Mpz_cmp_ui(n, 2) = 0 Then Return fp2square(a, p, w2)
    If Mpz_tstbit(n, 0) = 0 Then
        Mpz_fdiv_q_2exp(n, n, 1) ' even
        Return fp2square(fp2pow(a, n, p, w2), p, w2)
    Else
        Mpz_sub_ui(n, n, 1)      ' odd
        Return fp2mul(a, fp2pow(a, n, p, w2), p, w2)
    End If

End Function

' ------=< MAIN >=------

Dim As Long i
Dim As ZString Ptr zstr
Dim As String n_str, p_str

Dim As Mpz_ptr a, n, p, p2, w2, x1, x2
a  = Allocate(Len(__Mpz_struct)) : Mpz_init(a)
n  = Allocate(Len(__Mpz_struct)) : Mpz_init(n)
p  = Allocate(Len(__Mpz_struct)) : Mpz_init(p)
p2 = Allocate(Len(__Mpz_struct)) : Mpz_init(p2)
w2 = Allocate(Len(__Mpz_struct)) : Mpz_init(w2)
x1 = Allocate(Len(__Mpz_struct)) : Mpz_init(x1)
x2 = Allocate(Len(__Mpz_struct)) : Mpz_init(x2)

Dim As fp2 answer
answer.x = Allocate(Len(__Mpz_struct)) : Mpz_init(answer.x)
answer.y = Allocate(Len(__Mpz_struct)) : Mpz_init(answer.y)

For i = 1 To 8
    Read n_str
    Mpz_set_str(n, n_str, 10)
    If i < 8 Then
        Read p_str
        Mpz_set_str(p, p_str, 10)
    Else
        p_str = "10^50 + 151" ' set up last n
        Mpz_set_str(p, "1" + String(50, "0"), 10)
        Mpz_add_ui(p, p, 151)
    End If

    Print "Find solution for n = "; n_str; " and p = "; p_str

    If Mpz_tstbit(p, 0) = 0 OrElse Mpz_probab_prime_p(p, 20) = 0 Then
        Print p_str; "is not a odd prime"
        Print
        Continue For
    End If

    ' p is checked and is a odd prime
    ' legendre symbol needs to be 1
    If Mpz_legendre(n, p) <> 1 Then
        Print n_str; " is not a square in F"; p_str
        Print
        Continue For
    End If

    Mpz_set_ui(a, 1)
    Do
        Do
            Do
                Mpz_add_ui(a, a, 1)
                Mpz_mul(w2, a, a)
                Mpz_sub(w2, w2, n)
            Loop Until Mpz_legendre(w2, p) = -1

            Mpz_set(answer.x, a)
            Mpz_set_ui(answer.y, 1)
            Mpz_add_ui(p2, p, 1)       ' p2 = p + 1
            Mpz_fdiv_q_2exp(p2, p2, 1) ' p2 = p2 \ 2 (p2 shr 1)

            answer = fp2pow(answer, p2, p, w2)

        Loop Until Mpz_cmp_ui(answer.y, 0) = 0
        Mpz_set(x1, answer.x)
        Mpz_sub(x2, p, x1)
        Mpz_powm_ui(a, x1, 2, p)
        Mpz_powm_ui(p2, x2, 2, p)
        If Mpz_cmp(a, n) = 0 AndAlso Mpz_cmp(p2, n) = 0 Then Exit Do
    Loop

    zstr = Mpz_get_str(0, 10, x1)
    Print "Solution found: x1 = "; *zstr;
    zstr = Mpz_get_str(0, 10, x2)
    Print ", x2 = "; *zstr
    Print
Next

Mpz_clear(x1) : Mpz_clear(p2) : Mpz_clear(p) : Mpz_clear(a) : Mpz_clear(n)
Mpz_clear(x2) : Mpz_clear(w2) : Mpz_clear(answer.x) : Mpz_clear(answer.y)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Find solution for n = 10 and p = 13
Solution found: x1 = 6, x2 = 7

Find solution for n = 56 and p = 101
Solution found: x1 = 37, x2 = 64

Find solution for n = 8218 and p = 10007
Solution found: x1 = 9872, x2 = 135

Find solution for n = 8219 and p = 10007
8219 is not a square in F10007

Find solution for n = 331575 and p = 1000003
Solution found: x1 = 855842, x2 = 144161

Find solution for n = 665165880 and p = 1000000007
Solution found: x1 = 524868305, x2 = 475131702

Find solution for n = 881398088036 and p = 1000000000039
Solution found: x1 = 208600591990, x2 = 791399408049

Find solution for n = 34035243914635549601583369544560650254325084643201 and p = 10^50 + 151
Solution found: x1 = 17436881171909637738621006042549786426312886309400, x2 = 82563118828090362261378993957450213573687113690751
```



## Go


### int

Implementation following the pseudocode in the task description.

```go
package main

import "fmt"

func c(n, p int) (R1, R2 int, ok bool) {
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
    // Step 0, validate arguments
    if ls(n) != 1 {
        return
    }
    // Step 1, find a, ω2
    var a, ω2 int
    for a = 0; ; a++ {
        // integer % in Go uses T-division, add p to keep the result positive
        ω2 = (a*a + p - n) % p
        if ls(ω2) == p-1 {
            break
        }
    }
    // muliplication in fp2
    type point struct{ x, y int }
    mul := func(a, b point) point {
        return point{(a.x*b.x + a.y*b.y*ω2) % p, (a.x*b.y + b.x*a.y) % p}
    }
    // Step2, compute power
    r := point{1, 0}
    s := point{a, 1}
    for n := (p + 1) >> 1 % p; n > 0; n >>= 1 {
        if n&1 == 1 {
            r = mul(r, s)
        }
        s = mul(s, s)
    }
    // Step3, check x in Fp
    if r.y != 0 {
        return
    }
    // Step5, check x*x=n
    if r.x*r.x%p != n {
        return
    }
    // Step4, solutions
    return r.x, p - r.x, true
}

func main() {
    fmt.Println(c(10, 13))
    fmt.Println(c(56, 101))
    fmt.Println(c(8218, 10007))
    fmt.Println(c(8219, 10007))
    fmt.Println(c(331575, 1000003))
}
```

{{out}}

```txt

6 7 true
37 64 true
9872 135 true
0 0 false
855842 144161 true

```


### big.Int

Extra credit:

```go
package main

import (
    "fmt"
    "math/big"
)

func c(n, p big.Int) (R1, R2 big.Int, ok bool) {
    if big.Jacobi(&n, &p) != 1 {
        return
    }
    var one, a, ω2 big.Int
    one.SetInt64(1)
    for ; ; a.Add(&a, &one) {
        // big.Int Mod uses Euclidean division, result is always >= 0
        ω2.Mod(ω2.Sub(ω2.Mul(&a, &a), &n), &p)
        if big.Jacobi(&ω2, &p) == -1 {
            break
        }
    }
    type point struct{ x, y big.Int }
    mul := func(a, b point) (z point) {
        var w big.Int
        z.x.Mod(z.x.Add(z.x.Mul(&a.x, &b.x), w.Mul(w.Mul(&a.y, &a.y), &ω2)), &p)
        z.y.Mod(z.y.Add(z.y.Mul(&a.x, &b.y), w.Mul(&b.x, &a.y)), &p)
        return
    }
    var r, s point
    r.x.SetInt64(1)
    s.x.Set(&a)
    s.y.SetInt64(1)
    var e big.Int
    for e.Rsh(e.Add(&p, &one), 1); len(e.Bits()) > 0; e.Rsh(&e, 1) {
        if e.Bit(0) == 1 {
            r = mul(r, s)
        }
        s = mul(s, s)
    }
    R2.Sub(&p, &r.x)
    return r.x, R2, true
}

func main() {
    var n, p big.Int
    n.SetInt64(665165880)
    p.SetInt64(1000000007)
    R1, R2, ok := c(n, p)
    fmt.Println(&R1, &R2, ok)

    n.SetInt64(881398088036)
    p.SetInt64(1000000000039)
    R1, R2, ok = c(n, p)
    fmt.Println(&R1, &R2, ok)

    n.SetString("34035243914635549601583369544560650254325084643201", 10)
    p.SetString("100000000000000000000000000000000000000000000000151", 10)
    R1, R2, ok = c(n, p)
    fmt.Println(&R1)
    fmt.Println(&R2)
}
```

{{out}}

```txt

475131702 524868305 true
791399408049 208600591990 true
82563118828090362261378993957450213573687113690751
17436881171909637738621006042549786426312886309400

```



## J


Based on the echolisp implementation:


```J
leg=: dyad define
  x (y&|)@^ (y-1)%2
)

mul2=: conjunction define
  m| (*&{. + n**&{:), (+/ .* |.)
)

pow2=: conjunction define
:
  if. 0=y do. 1 0
  elseif. 1=y do. x
  elseif. 2=y do. x (m mul2 n) x
  elseif. 0=2|y do. (m mul2 n)~ x (m pow2 n) y%2
  elseif. do. x (m mul2 n) x (m pow2 n) y-1
  end.
)

cipolla=: dyad define
  assert. 1=1 p: y [ 'y must be prime'
  assert. 1= x leg y [ 'x must be square mod y'
  a=.1 
  whilst. (0 ~:{: r) do. a=. a+1
    while. 1>: leg&y@(x -~ *:) a do. a=.a+1 end.
    w2=. y|(*:a) - x
    r=. (a,1) (y pow2 w2) (y+1)%2
  end.
  if. x =&(y&|) *:{.r do.
    y|(,-){.r
  else.
    smoutput 'got ',":~.y|(,-){.r
    assert. 'not a valid square root'
  end.
)
```


Task examples:


```J
   10 cipolla 13
6 7
   56 cipolla 101
37 64
   8218 cipolla 10007
9872 135
   8219 cipolla 10007
|assertion failure: cipolla
|   1=x leg y['x must be square mod y'
   331575 cipolla 1000003
855842 144161
   665165880x cipolla 1000000007x
524868305 475131702
   881398088036x cipolla 1000000000039x
208600591990 791399408049
   34035243914635549601583369544560650254325084643201x cipolla (10^50x) + 151
17436881171909637738621006042549786426312886309400 82563118828090362261378993957450213573687113690751
```



## Java

{{trans|Kotlin}}
{{works with|Java|8}}

```Java
import java.math.BigInteger;
import java.util.function.BiFunction;
import java.util.function.Function;

public class CipollasAlgorithm {
    private static final BigInteger BIG = BigInteger.TEN.pow(50).add(BigInteger.valueOf(151));
    private static final BigInteger BIG_TWO = BigInteger.valueOf(2);

    private static class Point {
        BigInteger x;
        BigInteger y;

        Point(BigInteger x, BigInteger y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return String.format("(%s, %s)", this.x, this.y);
        }
    }

    private static class Triple {
        BigInteger x;
        BigInteger y;
        boolean b;

        Triple(BigInteger x, BigInteger y, boolean b) {
            this.x = x;
            this.y = y;
            this.b = b;
        }

        @Override
        public String toString() {
            return String.format("(%s, %s, %s)", this.x, this.y, this.b);
        }
    }

    private static Triple c(String ns, String ps) {
        BigInteger n = new BigInteger(ns);
        BigInteger p = !ps.isEmpty() ? new BigInteger(ps) : BIG;

        // Legendre symbol, returns 1, 0 or p - 1
        Function<BigInteger, BigInteger> ls = (BigInteger a)
            -> a.modPow(p.subtract(BigInteger.ONE).divide(BIG_TWO), p);

        // Step 0, validate arguments
        if (!ls.apply(n).equals(BigInteger.ONE)) {
            return new Triple(BigInteger.ZERO, BigInteger.ZERO, false);
        }

        // Step 1, find a, omega2
        BigInteger a = BigInteger.ZERO;
        BigInteger omega2;
        while (true) {
            omega2 = a.multiply(a).add(p).subtract(n).mod(p);
            if (ls.apply(omega2).equals(p.subtract(BigInteger.ONE))) {
                break;
            }
            a = a.add(BigInteger.ONE);
        }

        // multiplication in Fp2
        BigInteger finalOmega = omega2;
        BiFunction<Point, Point, Point> mul = (Point aa, Point bb) -> new Point(
            aa.x.multiply(bb.x).add(aa.y.multiply(bb.y).multiply(finalOmega)).mod(p),
            aa.x.multiply(bb.y).add(bb.x.multiply(aa.y)).mod(p)
        );

        // Step 2, compute power
        Point r = new Point(BigInteger.ONE, BigInteger.ZERO);
        Point s = new Point(a, BigInteger.ONE);
        BigInteger nn = p.add(BigInteger.ONE).shiftRight(1).mod(p);
        while (nn.compareTo(BigInteger.ZERO) > 0) {
            if (nn.and(BigInteger.ONE).equals(BigInteger.ONE)) {
                r = mul.apply(r, s);
            }
            s = mul.apply(s, s);
            nn = nn.shiftRight(1);
        }

        // Step 3, check x in Fp
        if (!r.y.equals(BigInteger.ZERO)) {
            return new Triple(BigInteger.ZERO, BigInteger.ZERO, false);
        }

        // Step 5, check x * x = n
        if (!r.x.multiply(r.x).mod(p).equals(n)) {
            return new Triple(BigInteger.ZERO, BigInteger.ZERO, false);
        }

        // Step 4, solutions
        return new Triple(r.x, p.subtract(r.x), true);
    }

    public static void main(String[] args) {
        System.out.println(c("10", "13"));
        System.out.println(c("56", "101"));
        System.out.println(c("8218", "10007"));
        System.out.println(c("8219", "10007"));
        System.out.println(c("331575", "1000003"));
        System.out.println(c("665165880", "1000000007"));
        System.out.println(c("881398088036", "1000000000039"));
        System.out.println(c("34035243914635549601583369544560650254325084643201", ""));
    }
}
```

{{out}}

```txt
(6, 7, true)
(37, 64, true)
(9872, 135, true)
(0, 0, false)
(855842, 144161, true)
(475131702, 524868305, true)
(791399408049, 208600591990, true)
(82563118828090362261378993957450213573687113690751, 17436881171909637738621006042549786426312886309400, true)
```



## Julia

{{trans|Perl}}

```julia
using Primes

function legendre(n, p)
    if p != 2 && isprime(p)
        x = powermod(BigInt(n), div(p - 1, 2), p)
        return x == 0 ? 0 : x == 1 ? 1 : -1
    end
    return -1
end

function cipolla(n, p)
    if legendre(n, p) != 1
        return NaN
    end
    a, w2 = BigInt(0), BigInt(0)
    while true
        w2 = (a^2 + p - n) % p
        if legendre(w2, p) < 0
            break
        end
        a += 1
    end
    r, s, i = (1, 0), (a, 1), p + 1
    while (i >>= 1) > 0
        if isodd(i)
            r = ((r[1] * s[1] + r[2] * s[2] * w2) % p, (r[1] * s[2] + s[1] * r[2]) % p)
        end
        s = ((s[1] * s[1] + s[2] * s[2] * w2) % p, (2 * s[1] * s[2]) % p)
    end
    return r[2] != 0 ? NaN : r[1]
end

const ctests = [(10, 13),
                (56, 101),
                (8218, 10007),
                (8219, 10007),
                (331575, 1000003),
                (665165880, 1000000007),
                (881398088036, 1000000000039),
                (big"34035243914635549601583369544560650254325084643201",
                    big"100000000000000000000000000000000000000000000000151")]

for (n, p) in ctests
   r = cipolla(n, p)
   println(r > 0 ? "Roots of $n are ($r, $(p - r)) mod $p." : "No solution for ($n, $p)")
end

```
{{out}}

```txt

Roots of 10 are (6, 7) mod 13.
Roots of 56 are (37, 64) mod 101.
Roots of 8218 are (9872, 135) mod 10007.
No solution for (8219, 10007)
Roots of 331575 are (855842, 144161) mod 1000003.
Roots of 665165880 are (475131702, 524868305) mod 1000000007.
Roots of 881398088036 are (791399408049, 208600591990) mod 1000000000039.
Roots of 34035243914635549601583369544560650254325084643201 are (82563118828090362261378993957450213573687113690751, 17436881171909637738621006042549786426312886309400) mod 100000000000000000000000000000000000000000000000151.

```






## Kotlin

{{trans|Go}}

```scala
// version 1.2.0

import java.math.BigInteger

class Point(val x: BigInteger, val y: BigInteger)

val bigZero = BigInteger.ZERO
val bigOne  = BigInteger.ONE
val bigTwo  = BigInteger.valueOf(2L)
val bigBig  = BigInteger.TEN.pow(50) + BigInteger.valueOf(151L)

fun c(ns: String, ps: String): Triple<BigInteger, BigInteger, Boolean> {
    val n = BigInteger(ns)
    val p = if (!ps.isEmpty()) BigInteger(ps) else bigBig

    // Legendre symbol, returns 1, 0 or p - 1
    fun ls(a: BigInteger) = a.modPow((p - bigOne) / bigTwo, p)

    // Step 0, validate arguments
    if (ls(n) != bigOne) return Triple(bigZero, bigZero, false)

    // Step 1, find a, omega2
    var a = bigZero
    var omega2: BigInteger
    while (true) {
        omega2 = (a * a + p - n) % p
        if (ls(omega2) == p - bigOne) break
        a++
    }

    // multiplication in Fp2
    fun mul(aa: Point, bb: Point) =
        Point(
            (aa.x * bb.x + aa.y * bb.y * omega2) % p,
            (aa.x * bb.y + bb.x * aa.y) % p
        )

    // Step 2, compute power
    var r = Point(bigOne, bigZero)
    var s = Point(a, bigOne)
    var nn = ((p + bigOne) shr 1) % p
    while (nn > bigZero) {
        if ((nn and bigOne) == bigOne) r = mul(r, s)
        s = mul(s, s)
        nn = nn shr 1
    }

    // Step 3, check x in Fp
    if (r.y != bigZero) return Triple(bigZero, bigZero, false)

    // Step 5, check x * x = n
    if (r.x * r.x % p != n) return Triple(bigZero, bigZero, false)

    // Step 4, solutions
    return Triple(r.x, p - r.x, true)
}

fun main(args: Array<String>) {
    println(c("10", "13"))
    println(c("56", "101"))
    println(c("8218", "10007"))
    println(c("8219", "10007"))
    println(c("331575", "1000003"))
    println(c("665165880", "1000000007"))
    println(c("881398088036", "1000000000039"))
    println(c("34035243914635549601583369544560650254325084643201", ""))
}
```


{{out}}

```txt

(6, 7, true)
(37, 64, true)
(9872, 135, true)
(0, 0, false)
(855842, 144161, true)
(475131702, 524868305, true)
(791399408049, 208600591990, true)
(82563118828090362261378993957450213573687113690751, 17436881171909637738621006042549786426312886309400, true)

```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use bigint;
use ntheory qw(is_prime);

sub Legendre {
    my($n,$p) = @_;
    return -1 unless $p != 2 && is_prime($p);
    my $x = ($n->as_int())->bmodpow(int(($p-1)/2), $p); # $n coerced to BigInt
    if    ($x==0) { return  0 }
    elsif ($x==1) { return  1 }
    else          { return -1 }
}

sub Cipolla {
    my($n, $p) = @_;
    return undef if Legendre($n,$p) != 1;

    my $w2;
    my $a = 0;
    $a++ until Legendre(($w2 = ($a**2 - $n) % $p), $p) < 0;

    my %r = ( x=> 1,  y=> 0 );
    my %s = ( x=> $a, y=> 1 );
    my $i = $p + 1;
    while (1 <= ($i >>= 1)) {
        %r = ( x => (($r{x} * $s{x} + $r{y} * $s{y} * $w2) % $p),
               y => (($r{x} * $s{y} + $s{x} * $r{y})       % $p)
             ) if $i % 2;
        %s = ( x => (($s{x} * $s{x} + $s{y} * $s{y} * $w2) % $p),
               y => (($s{x} * $s{y} + $s{x} * $s{y})       % $p)
             )
    }
    $r{y} ? undef : $r{x}
}

my @tests = (
    (10, 13),
    (56, 101),
    (8218, 10007),
    (8219, 10007),
    (331575, 1000003),
    (665165880, 1000000007),
    (881398088036, 1000000000039),
);

while (@tests) {
   $n = shift @tests;
   $p = shift @tests;
   my $r = Cipolla($n, $p);
   $r ? printf "Roots of %d are (%d, %d) mod %d\n", $n, $r, $p-$r, $p
      : print  "No solution for ($n, $p)\n"
}
```

{{out}}

```txt
Roots of 10 are (6, 7) mod 13
Roots of 56 are (37, 64) mod 101
Roots of 8218 are (9872, 135) mod 10007
No solution for (8219, 10007)
Roots of 331575 are (855842, 144161) mod 1000003
Roots of 665165880 are (475131702, 524868305) mod 1000000007
Roots of 881398088036 are (791399408049, 208600591990) mod 1000000000039
```



## Perl 6

{{works with|Rakudo|2016.10}}
{{trans|Sidef}}


```perl6
#  Legendre operator (𝑛│𝑝)
sub infix:<│> (Int \𝑛, Int \𝑝 where 𝑝.is-prime && (𝑝 != 2)) {
    given 𝑛.expmod( (𝑝-1) div 2, 𝑝 ) {
        when 0  {  0 }
        when 1  {  1 }
        default { -1 }
    }
}

# a coordinate in a Field of p elements
class Fp {
    has Int $.x;
    has Int $.y;
}

sub cipolla ( Int \𝑛, Int \𝑝 ) {
    note "Invalid parameters ({𝑛}, {𝑝})"
      and return Nil if (𝑛│𝑝) != 1;
    my $ω2;
    my $a = 0;
    loop {
        last if ($ω2 = ($a² - 𝑛) % 𝑝)│𝑝 < 0;
        $a++;
    }

    # define a local multiply operator for Field coordinates
    multi sub infix:<*> ( Fp $a, Fp $b ){
        Fp.new: :x(($a.x * $b.x + $a.y * $b.y * $ω2) % 𝑝),
                :y(($a.x * $b.y + $b.x * $a.y)       % 𝑝)
    }

    my $r = Fp.new: :x(1),  :y(0);
    my $s = Fp.new: :x($a), :y(1);

    for (𝑝+1) +> 1, * +> 1 ... 1 {
        $r *= $s if $_ % 2;
        $s *= $s;
    }
    return Nil if $r.y;
    $r.x;
}

my @tests = (
    (10, 13),
    (56, 101),
    (8218, 10007),
    (8219, 10007),
    (331575, 1000003),
    (665165880, 1000000007),
    (881398088036, 1000000000039),
    (34035243914635549601583369544560650254325084643201,
      100000000000000000000000000000000000000000000000151)
);

for @tests -> ($n, $p) {
   my $r = cipolla($n, $p);
   say $r ?? "Roots of $n are ($r, {$p-$r}) mod $p"
          !! "No solution for ($n, $p)"
}

```

{{out}}

```txt
Roots of 10 are (6, 7) mod 13
Roots of 56 are (37, 64) mod 101
Roots of 8218 are (9872, 135) mod 10007
Invalid parameters (8219, 10007)
No solution for (8219, 10007)
Roots of 331575 are (855842, 144161) mod 1000003
Roots of 665165880 are (475131702, 524868305) mod 1000000007
Roots of 881398088036 are (791399408049, 208600591990) mod 1000000000039
Roots of 34035243914635549601583369544560650254325084643201 are (82563118828090362261378993957450213573687113690751, 17436881171909637738621006042549786426312886309400) mod 100000000000000000000000000000000000000000000000151

```



## Phix

{{trans|Kotlin}}
{{libheader|mpfr}}

```Phix
include mpfr.e 

procedure legendre(mpz r, a, p)
-- Legendre symbol, returns 1, 0 or p - 1 (in r)
    mpz_sub_ui(r,p,1)
    {} = mpz_fdiv_q_ui(r, r, 2)
    mpz_powm(r,a,r,p)
end procedure 
 
procedure mul_point(sequence a, b, mpz omega2, p)
-- (modifies a)
    mpz {xa,ya} = a,
        {xb,yb} = b,
        xaxb = mpz_init(),
        yayb = mpz_init(),
        xayb = mpz_init(),
        xbya = mpz_init()
    mpz_mul(xaxb,xa,xb)
    mpz_mul(yayb,ya,yb)
    mpz_mul(xayb,xa,yb)
    mpz_mul(xbya,xb,ya)
    mpz_mul(yayb,yayb,omega2)
    mpz_add(xaxb,xaxb,yayb)
    mpz_mod(xa,xaxb,p)      -- xa := mod(xaxb+yayb*omega2,p)
    mpz_add(xayb,xayb,xbya)
    mpz_mod(ya,xayb,p)      -- ya := mod(xayb+xbya,p)
    {xaxb,yayb,xayb,xbya} = mpz_clear({xaxb,yayb,xayb,xbya})
end procedure

function cipolla(object no, po)
mpz n = mpz_init(no),
    p = mpz_init(po),
    t = mpz_init()
 
    -- Step 0, validate arguments
    legendre(t,n,p)
    if mpz_cmp_si(t,1)!=0 then return {"0","0","false"} end if
 
    -- Step 1, find a, omega2
    integer a = 0
    mpz omega2 = mpz_init(),
        pm1 = mpz_init()
    mpz_sub_ui(pm1,p,1)
    while true do
        mpz_sub(t,p,n)
        mpz_add_ui(t,t,a*a)
        mpz_mod(omega2,t,p)
        legendre(t,omega2,p)
        if mpz_cmp(t,pm1)=0 then exit end if
        a += 1
    end while
 
    -- Step 2, compute power
    sequence r = {mpz_init(1),mpz_init(0)},
             s = {mpz_init(a),mpz_init(1)}
    mpz nn = mpz_init()
    mpz_add_ui(nn,p,1)
    {} = mpz_fdiv_q_ui(nn, nn, 2)
    mpz_mod(nn,nn,p)
    while mpz_cmp_si(nn,0)>0 do
        if mpz_fdiv_ui(nn,2)=1 then
            mul_point(r,s,omega2,p)
        end if
        mul_point(s,s,omega2,p)
        {} = mpz_fdiv_q_ui(nn, nn, 2)
    end while

    -- Step 3, check x in Fp
    if mpz_cmp_si(r[2],0)!=0 then return {"0","0","false"} end if 
 
    -- Step 5, check x * x = n
    mpz_powm_ui(t,r[1],2,p)
    if mpz_cmp(t,n)!=0 then return {"0","0","false"} end if 
 
    -- Step 4, solutions
    mpz_sub(p,p,r[1])
    return {mpz_get_str(r[1]), mpz_get_str(p), "true"}
end function
 
constant tests = {{10,13},
                  {56,101},
                  {8218,10007},
                  {8219,10007},
                  {331575,1000003},
                  {665165880,1000000007},
                  {"881398088036","1000000000039"},
                  {"34035243914635549601583369544560650254325084643201",
                   "100000000000000000000000000000000000000000000000151"}}

for i=1 to length(tests) do
    object {n,p} = tests[i]
    ?{n,p,cipolla(n,p)}
end for
```

Obviously were you to use that in anger, you would probably rip out a few ba_sprint() and return false rather than "false", etc.
{{out}}

```txt

{10,13,{"6","7","true"}}
{56,101,{"37","64","true"}}
{8218,10007,{"9872","135","true"}}
{8219,10007,{"0","0","false"}}
{331575,1000003,{"855842","144161","true"}}
{665165880,1000000007,{"475131702","524868305","true"}}
{"881398088036","1000000000039",{"791399408049","208600591990","true"}}
{"34035243914635549601583369544560650254325084643201","100000000000000000000000000000000000000000000000151",
 {"82563118828090362261378993957450213573687113690751","17436881171909637738621006042549786426312886309400","true"}}

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
(de mul ("A" B P W2)
   (let (A (copy "A")  B (copy B))
      (set
         "A"
         (%
            (+
               (* (car A) (car B))
               (* (cadr A) (cadr B) W2) )
            P )
         (cdr "A")
         (%
            (+
               (* (car A) (cadr B))
               (* (car B) (cadr A)) )
            P ) ) ) )
(de ci (N P)
   (and
      (=1 (legendre N P))
      (let
         (A 0
            W2 0
            R NIL
            S NIL )
         (loop
            (setq W2
               (% (- (+ (* A A) P) N) P) )
            (T (= (dec P) (legendre W2 P)))
            (inc 'A) )
         (setq R (list 1 0)  S (list A 1))
         (for
            (N
               (% (>> 1 (inc P)) P)
               (> N 0)
               (>> 1 N) )
            (and (bit? 1 N) (mul R S P W2))
            (mul S S P W2) )
         (=0 (cadr R))
         (=
            N
            (% (* (car R) (car R)) P) )
         (list (car R) (- P (car R))) ) ) )

(println (ci 10 13))
(println (ci 56 101))
(println (ci 8218 10007))
(println (ci 8219 10007))
(println (ci 331575 1000003))
(println (ci 665165880 1000000007))
(println (ci 881398088036 1000000000039))
(println (ci 34035243914635549601583369544560650254325084643201 (+ (** 10 50) 151)))
```

{{out}}

```txt

(6 7)
(37 64)
(9872 135)
NIL
(855842 144161)
(475131702 524868305)
(791399408049 208600591990)
(82563118828090362261378993957450213573687113690751 17436881171909637738621006042549786426312886309400)

```



## Python


```python

#Converts n to base b as a list of integers between 0 and b-1
#Most-significant digit on the left
def convertToBase(n, b):
	if(n < 2):
		return [n];
	temp = n;
	ans = [];
	while(temp != 0):
		ans = [temp % b]+ ans;
		temp /= b;
	return ans;

#Takes integer n and odd prime p
#Returns both square roots of n modulo p as a pair (a,b)
#Returns () if no root
def cipolla(n,p):
	n %= p
	if(n == 0 or n == 1):
		return (n,-n%p)
	phi = p - 1
	if(pow(n, phi/2, p) != 1):
		return ()
	if(p%4 == 3):
		ans = pow(n,(p+1)/4,p)
		return (ans,-ans%p)
	aa = 0
	for i in xrange(1,p):
		temp = pow((i*i-n)%p,phi/2,p)
		if(temp == phi):
			aa = i
			break;
	exponent = convertToBase((p+1)/2,2)
	def cipollaMult((a,b),(c,d),w,p):
		return ((a*c+b*d*w)%p,(a*d+b*c)%p)
	x1 = (aa,1)
	x2 = cipollaMult(x1,x1,aa*aa-n,p)
	for i in xrange(1,len(exponent)):
		if(exponent[i] == 0):
			x2 = cipollaMult(x2,x1,aa*aa-n,p)
			x1 = cipollaMult(x1,x1,aa*aa-n,p)
		else:
			x1 = cipollaMult(x1,x2,aa*aa-n,p)
			x2 = cipollaMult(x2,x2,aa*aa-n,p)
	return (x1[0],-x1[0]%p)

print "Roots of 2 mod 7: " +str(cipolla(2,7))
print "Roots of 8218 mod 10007: " +str(cipolla(8218,10007))
print "Roots of 56 mod 101: " +str(cipolla(56,101))
print "Roots of 1 mod 11: " +str(cipolla(1,11))
print "Roots of 8219 mod 10007: " +str(cipolla(8219,10007))

```

{{out}}


```txt
Roots of 2 mod 7: (4, 3)
Roots of 8218 mod 10007: (9872, 135)
Roots of 56 mod 101: (37, 64)
Roots of 1 mod 11: (1, 10)
Roots of 8219 mod 10007: ()

```



## Racket

{{trans|EchoLisp}}


```racket
#lang racket

(require math/number-theory)

;; math/number-theory allows us to parameterize a "current-modulus"
;; which obviates the need for p to be passed around constantly
(define (Cipolla n p) (with-modulus p (mod-Cipolla n)))

(define (mod-Legendre a)  
  (modexpt a (/ (sub1 (current-modulus)) 2)))
 
;; Arithmetic in Fp² 
(struct Fp² (x y))

(define-syntax-rule (Fp²-destruct* (a a.x a.y) ...)
  (begin (match-define (Fp² a.x a.y) a) ...)  )

;; a + b
(define (Fp²-add a b ω2)
  (Fp²-destruct* (a a.x a.y) (b b.x b.y))
  (Fp² (mod+ a.x b.x) (mod+ a.y b.y)))

;; a * b
(define (Fp²-mul a b ω2) 
  (Fp²-destruct* (a a.x a.y) (b b.x b.y))
  (Fp² (mod+ (* a.x b.x) (* ω2 a.y b.y)) (mod+ (* a.x b.y) (* a.y b.x))))
 
;; a * a	
(define (Fp²-square a ω2)
  (Fp²-destruct* (a a.x a.y))
  (Fp² (mod+ (sqr a.x) (* ω2 (sqr a.y))) (mod* 2 a.x a.y)))
 
;; a ^ n
(define (Fp²-pow a n ω2)
  (Fp²-destruct* (a a.x a.y))
  (cond 
    ((= 0 n) (Fp² 1 0))
    ((= 1 n) a)
    ((= 2 n) (Fp²-mul a a ω2))
    ((even? n) (Fp²-square (Fp²-pow a (/ n 2) ω2) ω2))
    (else (Fp²-mul a (Fp²-pow a (sub1 n) ω2) ω2))))
 
;; x^2 ≡ n (mod p) ?
(define (mod-Cipolla n) 
  ;; check n is a square
  (unless (= 1 (mod-Legendre n)) (error 'Cipolla "~a not a square (mod ~a)" n (current-modulus)))
  ;; iterate until suitable 'a' found
  (define a (for/first ((t (in-range 2 (current-modulus))) ;; t = tentative a
                        #:when (= (sub1 (current-modulus))
                                  (mod-Legendre (- (* t t) n))))
              t))
  (define ω2 (- (* a a) n))
  ;; (Fp² a 1) = a + ω
  (define r (Fp²-pow (Fp² a 1) (/ (add1 (current-modulus)) 2) ω2))
  (define x (Fp²-x r))
  (unless (zero? (Fp²-y r)) (error 'Cipolla "ω has not vanished")) ;; hope that ω has vanished
  (unless (mod= n (* x x)) (error 'Cipolla "result check failed")) ;; checking the result
  (values x (mod- (current-modulus) x)))

(define (report-Cipolla n p)
  (with-handlers ((exn:fail? (λ (x) (eprintf "Caught error: ~s~%" (exn-message x)))))
    (define-values (r1 r2) (Cipolla n p))
    (printf "Roots of ~a are (~a,~a)  (mod ~a)~%" n  r1 r2 p)))

(module+ test
  (report-Cipolla 10 13)
  (report-Cipolla 56 101)
  (report-Cipolla 8218 10007)
  (report-Cipolla 8219 10007)
  (report-Cipolla 331575 1000003)
  (report-Cipolla 665165880 1000000007)
  (report-Cipolla 881398088036 1000000000039)
  (report-Cipolla 34035243914635549601583369544560650254325084643201
                  100000000000000000000000000000000000000000000000151))
```


{{out}}


```txt
Roots of 10 are (6,7)  (mod 13)
Roots of 56 are (37,64)  (mod 101)
Roots of 8218 are (9872,135)  (mod 10007)
Caught error: "Cipolla: 8219 not a square (mod 10007)"
Roots of 331575 are (855842,144161)  (mod 1000003)
Roots of 665165880 are (524868305,475131702)  (mod 1000000007)
Roots of 881398088036 are (208600591990,791399408049)  (mod 1000000000039)
Roots of 34035243914635549601583369544560650254325084643201 are (17436881171909637738621006042549786426312886309400,82563118828090362261378993957450213573687113690751)  (mod 100000000000000000000000000000000000000000000000151)
```



## Sage

{{works with|Sage|7.6}}

```sage

def eulerCriterion(a, p):
    return -1 if pow(a, int((p-1)/2), p) == p-1 else 1

def cipollaMult(x1, y1, x2, y2, u, p):
    return ((x1*x2 + y1*y2*u) % p), ((x1*y2 + x2*y1) % p)

def cipollaAlgorithm(n, p):
    a = Mod(n, p)
    out = []

    if eulerCriterion(a, p) == -1:
        print "❌ " + str(a) + " is not a quadratic residue modulo " + str(p)
        return False

    if not is_prime(p):
        conglst = []                                    #congruence list
        crtlst = []
        factors = []

        for k in list(factor(p)):
            factors.append(int(k[0]))

        for f in factors:
            conglst.append(cipollaAlgorithm(a, f))

        for i in Permutations([0, 1] * len(factors), len(factors)).list():
            for j in range(len(factors)):
                crtlst.append(int(conglst[ j ][ i[j] ]))

            out.append(crt(crtlst, factors))
            crtlst = []

        return sorted(out)

    if pow(p, 1, 4) == 3:
        temp = pow(a, int((p+1)/4), p)
        return [temp, p - temp]


    t = randrange(2, p)
    u = pow(t**2 - a, 1, p)
    while (eulerCriterion(u, p) == 1):
        t = randrange(2, p)
        u = pow(t**2 - a, 1, p)

    x0, y0 = t, 1
    x, y = t, 1
    for i in range(int((p + 1) / 2) - 1):
        x, y = cipollaMult(x, y, x0, y0, u, p)

    out.extend([x, p - x])

    return sorted(out)

```

{{out}}

```txt

sage: cipollaAlgorithm(10, 13)
[6, 7]
sage: cipollaAlgorithm(56, 101)
[37, 64]
sage: cipollaAlgorithm(8218, 10007)
[135, 9872]
sage: cipollaAlgorithm(331575, 1000003)
[144161, 855842]
sage: cipollaAlgorithm(8219, 10007)
❌ 8219 is not a quadratic residue modulo 10007
False

```



## Scala


### Imperative solution


```Scala
object CipollasAlgorithm extends App {
  private val BIG = BigInt(10).pow(50) + BigInt(151)

  println(c("10", "13"))
  println(c("56", "101"))
  println(c("8218", "10007"))
  println(c("8219", "10007"))
  println(c("331575", "1000003"))
  println(c("665165880", "1000000007"))
  println(c("881398088036", "1000000000039"))
  println(c("34035243914635549601583369544560650254325084643201", ""))

  private def c(ns: String, ps: String): Triple = {
    val (n, p) = (BigInt(ns), if (ps.isEmpty) BIG else BigInt(ps))

    // Legendre symbol, returns 1, 0 or p - 1
    def ls(a: BigInt) = a.modPow((p - BigInt(1)) / BigInt(2), p)

    // multiplication in Fp2
    def mul(aa: Point, bb: Point, omega2: BigInt) =
      new Point((aa.x * bb.x + aa.y * bb.y * omega2) % p, (aa.x * bb.y + (bb.x * aa.y)) % p)

    // Step 0, validate arguments
    if (ls(n) != BigInt(1)) new Triple(0, 0, false)
    else {
      // Step 1, find a, omega2
      var (a, flag, omega2) = (BigInt(0), true, BigInt(0))
      while (flag) {
        omega2 = (a * a + p - n) % p
        if (ls(omega2) == p - BigInt(1)) flag = false else a = a + BigInt(1)
      }

      // Step 2, compute power
      var (nn, r, s) = ((p + BigInt(1) >> 1) % p, new Point(BigInt(1), 0), new Point(a, BigInt(1)))
      while (nn > 0) {
        if ((nn & BigInt(1)) == BigInt(1)) r = mul(r, s, omega2)
        s = mul(s, s, omega2)
        nn = nn >> 1
      }
      // Step 3, check x in Fp
      if (r.y != 0) new Triple(0, 0, false)
      else // Step 5, check x * x = n
      if ((r.x * r.x) % p != n) new Triple(0, 0, false)
      else new Triple(r.x, p - r.x, true) // Step 4, solutions
    }
  }

  private class Point(val x: BigInt, val y: BigInt)

  private class Triple(val x: BigInt, val y: BigInt, val b: Boolean) {
    override def toString: String = f"($x%s, $y%s, $b%s)"
  }

}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/QQBsMza/3 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/NEP5hOWmSBqqpwmF30LpUA Scastie (JVM)].

## Sidef

{{trans|Go}}

```ruby
func cipolla(n, p) {

    legendre(n, p) == 1 || return nil

    var (a = 0, ω2 = 0)
    loop {
        ω2 = ((a*a - n) % p)
        if (legendre(ω2, p) == -1) {
            break
        }
        ++a
    }

    struct point { x, y }

    func mul(a, b) {
        point((a.x*b.x + a.y*b.y*ω2) % p, (a.x*b.y + b.x*a.y) % p)
    }

    var r = point(1, 0)
    var s = point(a, 1)

    for (var n = ((p+1) >> 1); n > 0; n >>= 1) {
        r = mul(r, s) if n.is_odd
        s = mul(s, s)
    }

    r.y == 0 ? r.x : nil
}

var tests = [
    [10, 13],
    [56, 101],
    [8218, 10007],
    [8219, 10007],
    [331575, 1000003],
    [665165880, 1000000007],
    [881398088036 1000000000039],
    [34035243914635549601583369544560650254325084643201, 10**50 + 151],
]

for n,p in tests {
    var r = cipolla(n, p)
    if (defined(r)) {
        say "Roots of #{n} are (#{r} #{p-r}) mod #{p}"
    } else {
        say "No solution for (#{n}, #{p})"
    }
}
```

{{out}}

```txt
Roots of 10 are (6 7) mod 13
Roots of 56 are (37 64) mod 101
Roots of 8218 are (9872 135) mod 10007
No solution for (8219, 10007)
Roots of 331575 are (855842 144161) mod 1000003
Roots of 665165880 are (475131702 524868305) mod 1000000007
Roots of 881398088036 are (791399408049 208600591990) mod 1000000000039
Roots of 34035243914635549601583369544560650254325084643201 are (82563118828090362261378993957450213573687113690751 17436881171909637738621006042549786426312886309400) mod 100000000000000000000000000000000000000000000000151
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Numerics

Module Module1

    ReadOnly BIG = BigInteger.Pow(10, 50) + 151

    Function C(ns As String, ps As String) As Tuple(Of BigInteger, BigInteger, Boolean)
        Dim n = BigInteger.Parse(ns)
        Dim p = If(ps.Length > 0, BigInteger.Parse(ps), BIG)

        ' Legendre symbol. Returns 1, 0, or p-1
        Dim ls = Function(a0 As BigInteger) BigInteger.ModPow(a0, (p - 1) / 2, p)

        ' Step 0: validate arguments
        If ls(n) <> 1 Then
            Return Tuple.Create(BigInteger.Zero, BigInteger.Zero, False)
        End If

        ' Step 1: Find a, omega2
        Dim a = BigInteger.Zero
        Dim omega2 As BigInteger
        Do
            omega2 = (a * a + p - n) Mod p
            If ls(omega2) = p - 1 Then
                Exit Do
            End If
            a += 1
        Loop

        ' Multiplication in Fp2
        Dim mul = Function(aa As Tuple(Of BigInteger, BigInteger), bb As Tuple(Of BigInteger, BigInteger))
                      Return Tuple.Create((aa.Item1 * bb.Item1 + aa.Item2 * bb.Item2 * omega2) Mod p, (aa.Item1 * bb.Item2 + bb.Item1 * aa.Item2) Mod p)
                  End Function

        ' Step 2: Compute power
        Dim r = Tuple.Create(BigInteger.One, BigInteger.Zero)
        Dim s = Tuple.Create(a, BigInteger.One)
        Dim nn = ((p + 1) >> 1) Mod p
        While nn > 0
            If nn Mod 2 = 1 Then
                r = mul(r, s)
            End If
            s = mul(s, s)
            nn >>= 1
        End While

        ' Step 3: Check x in Fp
        If r.Item2 <> 0 Then
            Return Tuple.Create(BigInteger.Zero, BigInteger.Zero, False)
        End If

        ' Step 5: Check x * x = n
        If r.Item1 * r.Item1 Mod p <> n Then
            Return Tuple.Create(BigInteger.Zero, BigInteger.Zero, False)
        End If

        ' Step 4: Solutions
        Return Tuple.Create(r.Item1, p - r.Item1, True)
    End Function

    Sub Main()
        Console.WriteLine(C("10", "13"))
        Console.WriteLine(C("56", "101"))
        Console.WriteLine(C("8218", "10007"))
        Console.WriteLine(C("8219", "10007"))
        Console.WriteLine(C("331575", "1000003"))
        Console.WriteLine(C("665165880", "1000000007"))
        Console.WriteLine(C("881398088036", "1000000000039"))
        Console.WriteLine(C("34035243914635549601583369544560650254325084643201", ""))
    End Sub

End Module
```

{{out}}

```txt
(6, 7, True)
(37, 64, True)
(9872, 135, True)
(0, 0, False)
(855842, 144161, True)
(475131702, 524868305, True)
(791399408049, 208600591990, True)
(82563118828090362261378993957450213573687113690751, 17436881171909637738621006042549786426312886309400, True)
```



## zkl

{{trans|EchoLisp}}
Uses lib GMP (GNU MP Bignum Library).

```zkl
var [const] BN=Import("zklBigNum");   //libGMP
fcn modEq(a,b,p) { (a-b)%p==0 }
fcn Legendre(a,p){ a.powm((p - 1)/2,p) }
 
class  Fp2{  // Arithmetic in Fp^2
   fcn init(_x,_y){ var [const] x=BN(_x), y=BN(_y) }	// two big ints
   //fcn add(b,p){ self((x + b.x)%p,(y + b.y)%p) }	// a + b
   fcn mul(b,p,w2){ self(( x*b.x + y*b.y*w2 )%p, (x*b.y + y*b.x) %p) } // a * b
   fcn square(p,w2){ mul(self,p,w2) }          	// a * a == self.mul(self,p,w2)
   fcn pow(n,p,w2){				// a ^ n
      if     (n==0)     self(1,0);
      else if(n==1)     self;
      else if(n==2)     square(p,w2);
      else if(n.isEven) pow(n/2,p,w2).square(p,w2);
      else 		mul(pow(n-1,p,w2),p,w2)
   }
}

fcn Cipolla(n,p){ n=BN(n);	// x^2 == n (mod p) ?
   if(Legendre(n,p)!=1)   // check n is a square
      throw(Exception.AssertionError("not a square (mod p)"+vm.arglist));
   // iterate until suitable 'a' found (the first one found)
   a:=[BN(2)..p].filter1('wrap(a){ Legendre(a*a-n,p)==(p-1) });
   w2:=a*a - n;
   r:=Fp2(a,1).pow((p + 1)/2,p,w2);	    // (Fp2 a 1) = a + w2
   x:=r.x;
   _assert_(r.y==0,"r.y==0 : "+r.y);	    // hope that w has vanished
   _assert_(modEq(n,x*x,p),"modEq(n,x*x,p)"); // checking the result
   println("Roots of %d are (%d,%d)  (mod %d)".fmt(n,x,(p-x)%p,p));
   return(x,(p-x)%p);
}
```


```zkl
foreach n,p in (T(
	T(10,13),T(56,101),T(8218,10007),T(8219,10007),T(331575,1000003),
	T(665165880,1000000007),T(881398088036,1000000000039),
	T("34035243914635549601583369544560650254325084643201",
	  BN(10).pow(50) + 151) )){
   try{ Cipolla(n,p) }catch{ println(__exception) }
}
```

{{out}}

```txt

Roots of 10 are (6,7)  (mod 13)
Roots of 56 are (37,64)  (mod 101)
Roots of 8218 are (9872,135)  (mod 10007)
AssertionError(not a square (mod p)L(8219,10007))
Roots of 331575 are (855842,144161)  (mod 1000003)
Roots of 665165880 are (524868305,475131702)  (mod 1000000007)
Roots of 881398088036 are (208600591990,791399408049)  (mod 1000000000039)
Roots of 34035243914635549601583369544560650254325084643201 are (17436881171909637738621006042549786426312886309400,82563118828090362261378993957450213573687113690751)  (mod 100000000000000000000000000000000000000000000000151)

```


+++
title = "Montgomery reduction"
description = ""
date = 2019-06-23T20:47:34Z
aliases = []
[extra]
id = 11084
[taxonomies]
categories = []
tags = []
+++

{{clarify task}}{{draft task}}
Implement the [[wp:Montgomery reduction|Montgomery reduction]] algorithm, as explained in "Handbook of Applied Cryptography, Section 14.3.2, page 600. Montgomery reduction calculates <math>T R^{-1} \mathrm{mod}  m</math>, without having to divide by <math>m</math>.
* Let <math>M</math> be a positive integer, and <math>R</math> and <math>T</math> integers such that <math>R > m</math>, <math>\mathrm{gcd}(m, R) = 1</math>, and <math>0 \le T < mR</math>.
* <math>R</math> is usually chosen as <math>b^n</math>, where <math>b</math> = base (radix) in which the numbers in the calculation as represented in (so <math>b = 10</math> in ‘normal’ paper arithmetic, <math>b = 2</math> for computer implementations) and <math>n</math> = number of digits in base <math>m</math>
* The numbers <math>m</math> (<math>n</math> digits long), <math>T</math> (<math>2n</math> digits long), <math>R</math>, <math>b</math>, <math>n</math> are known entities, a number <math>m'</math> (often represented as <code>m_dash</code> in code) = <math>-m^{-1} \mathrm{mod} b</math> is precomputed.

See the <!--<amazon id=0849385237>[%buy%--> Handbook of Applied Cryptography<!--]</amazon>--> for brief introduction to theory and numerical example in radix 10. Individual chapters of the book [http://www.cacr.math.uwaterloo.ca/hac/ can be viewed online] as provided by the authors. The said algorithm can be found at [http://www.cacr.math.uwaterloo.ca/hac/about/chap14.pdf] at page 600 (page 11 of pdf file)

Algorithm:
 A ← T (temporary variable)
 For i from 0 to (n-1) do the following:
    u<sub>i</sub> ← a<sub>i</sub>* m' mod b      <span style="color: gray">// a<sub>i</sub> is the ith digit of A, u<sub>i</sub> is a single digit number in radix b</span>
    A ← A + u<sub>i</sub>*m*b<sup>i</sup>
 A ← A/b<sup>n</sup>
 if A >= m,
    A ← A - m
 Return (A)


## C++


```cpp>#include<iostream

#include<conio.h>
using namespace std;
typedef unsigned long ulong;

int ith_digit_finder(long long n, long b, long i){
 /**
     n = number whose digits we need to extract
     b = radix in which the number if represented
     i = the ith bit (ie, index of the bit that needs to be extracted)
 **/
    while(i>0){
        n/=b;
        i--;
    }
    return (n%b);
}

long eeuclid(long m, long b, long *inverse){        /// eeuclid( modulus, num whose inv is to be found, variable to put inverse )
    /// Algorithm used from Stallings book
    long A1 = 1, A2 = 0, A3 = m,
         B1 = 0, B2 = 1, B3 = b,
         T1, T2, T3, Q;

         cout<<endl<<"eeuclid() started"<<endl;

        while(1){
            if(B3 == 0){
                *inverse = 0;
                return A3;      // A3 = gcd(m,b)
            }

            if(B3 == 1){
                *inverse = B2; // B2 = b^-1 mod m
                return B3;      // A3 = gcd(m,b)
            }

            Q = A3/B3;

            T1 = A1 - Q*B1;
            T2 = A2 - Q*B2;
            T3 = A3 - Q*B3;

            A1 = B1; A2 = B2; A3 = B3;
            B1 = T1; B2 = T2; B3 = T3;

       }
    cout<<endl<<"ending eeuclid() "<<endl;
}

long long mon_red(long m, long m_dash, long T, int n, long b = 2){
/**
    m = modulus
    m_dash = m' = -m^-1 mod b
    T = number whose modular reduction is needed, the o/p of the function is TR^-1 mod m
    n = number of bits in m (2n is the number of bits in T)
    b = radix used (for practical implementations, is equal to 2, which is the default value)
**/
    long long A,ui, temp, Ai;       // Ai is the ith bit of A, need not be llong long probably
    if( m_dash < 0 ) m_dash = m_dash + b;
    A = T;
    for(int i = 0; i<n; i++){
    ///    ui = ( (A%b)*m_dash ) % b;        // step 2.1; A%b gives ai (MISTAKE -- A%b will always give the last digit of A if A is represented in base b); hence we need the function ith_digit_finder()
        Ai = ith_digit_finder(A, b, i);
        ui = ( ( Ai % b) * m_dash ) % b;
        temp  = ui*m*power(b, i);
        A = A + temp;
    }
    A = A/power(b, n);
    if(A >= m) A = A - m;
    return A;
}

int main(){
    long a, b, c, d=0, e, inverse = 0;
    cout<<"m >> ";
    cin >> a;
    cout<<"T >> ";
    cin>>b;
    cout<<"Radix b >> ";
    cin>>c;
    eeuclid(c, a, &d);      // eeuclid( modulus, num whose inverse is to be found, address of variable which is to store inverse)
    e = mon_red(a, -d, b, length_finder(a, c), c);
    cout<<"Montgomery domain representation = "<<e;
    return 0;
}
```


=={{header|C#|C sharp}}==
{{trans|D}}

```csharp
using System;
using System.Numerics;

namespace MontgomeryReduction {
    public static class Helper {
        public static int BitLength(this BigInteger v) {
            if (v < 0) {
                v *= -1;
            }

            int result = 0;
            while (v > 0) {
                v >>= 1;
                result++;
            }

            return result;
        }
    }

    struct Montgomery {
        public static readonly int BASE = 2;

        public BigInteger m;
        public BigInteger rrm;
        public int n;

        public Montgomery(BigInteger m) {
            if (m < 0 || m.IsEven) throw new ArgumentException();

            this.m = m;
            n = m.BitLength();
            rrm = (BigInteger.One << (n * 2)) % m;
        }

        public BigInteger Reduce(BigInteger t) {
            var a = t;

            for (int i = 0; i < n; i++) {
                if (!a.IsEven) a += m;
                a = a >> 1;
            }
            if (a >= m) a -= m;
            return a;
        }
    }

    class Program {
        static void Main(string[] args) {
            var m = BigInteger.Parse("750791094644726559640638407699");
            var x1 = BigInteger.Parse("540019781128412936473322405310");
            var x2 = BigInteger.Parse("515692107665463680305819378593");

            var mont = new Montgomery(m);
            var t1 = x1 * mont.rrm;
            var t2 = x2 * mont.rrm;

            var r1 = mont.Reduce(t1);
            var r2 = mont.Reduce(t2);
            var r = BigInteger.One << mont.n;

            Console.WriteLine("b :  {0}", Montgomery.BASE);
            Console.WriteLine("n :  {0}", mont.n);
            Console.WriteLine("r :  {0}", r);
            Console.WriteLine("m :  {0}", mont.m);
            Console.WriteLine("t1:  {0}", t1);
            Console.WriteLine("t2:  {0}", t2);
            Console.WriteLine("r1:  {0}", r1);
            Console.WriteLine("r2:  {0}", r2);
            Console.WriteLine();
            Console.WriteLine("Original x1       : {0}", x1);
            Console.WriteLine("Recovered from r1 : {0}", mont.Reduce(r1));
            Console.WriteLine("Original x2       : {0}", x2);
            Console.WriteLine("Recovered from r2 : {0}", mont.Reduce(r2));

            Console.WriteLine();
            Console.WriteLine("Montgomery computation of x1 ^ x2 mod m :");
            var prod = mont.Reduce(mont.rrm);
            var @base = mont.Reduce(x1 * mont.rrm);
            var exp = x2;
            while (exp.BitLength() > 0) {
                if (!exp.IsEven) prod = mont.Reduce(prod * @base);
                exp >>= 1;
                @base = mont.Reduce(@base * @base);
            }
            Console.WriteLine(mont.Reduce(prod));
            Console.WriteLine();
            Console.WriteLine("Alternate computation of x1 ^ x2 mod m :");
            Console.WriteLine(BigInteger.ModPow(x1, x2, m));
        }
    }
}
```

{{out}}

```txt
b :  2
n :  100
r :  1267650600228229401496703205376
m :  750791094644726559640638407699
t1:  323165824550862327179367294465482435542970161392400401329100
t2:  308607334419945011411837686695175944083084270671482464168730
r1:  440160025148131680164261562101
r2:  435362628198191204145287283255

Original x1       : 540019781128412936473322405310
Recovered from r1 : 540019781128412936473322405310
Original x2       : 515692107665463680305819378593
Recovered from r2 : 515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m :
151232511393500655853002423778

Alternate computation of x1 ^ x2 mod m :
151232511393500655853002423778
```



## D

{{trans|Kotlin}}

```D
import std.bigint;
import std.stdio;

int bitLength(BigInt v) {
    if (v < 0) {
        v *= -1;
    }

    int result = 0;
    while (v > 0) {
        v >>= 1;
        result++;
    }

    return result;
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

struct Montgomery {
    BigInt m;
    int n;
    BigInt rrm;

    this(BigInt m) in {
        assert(m > 0 && (m & 1) != 0); // must be positive and odd
    } body {
        this.m = m;
        n = m.bitLength();
        rrm = (BigInt(1) << (n * 2)) % m;
    }

    BigInt reduce(BigInt t) {
        auto a = t;

        foreach(i; 0..n) {
            if ((a & 1) == 1) a += m;
            a = a >> 1;
        }
        if (a >= m) a -= m;
        return a;
    }

    enum BASE = 2;
}

void main() {
    auto m = BigInt("750791094644726559640638407699");
    auto x1 = BigInt("540019781128412936473322405310");
    auto x2 = BigInt("515692107665463680305819378593");

    auto mont = Montgomery(m);
    auto t1 = x1 * mont.rrm;
    auto t2 = x2 * mont.rrm;

    auto r1 = mont.reduce(t1);
    auto r2 = mont.reduce(t2);
    auto r = BigInt(1) << mont.n;

    writeln("b :  ", Montgomery.BASE);
    writeln("n :  ", mont.n);
    writeln("r :  ", r);
    writeln("m :  ", mont.m);
    writeln("t1:  ", t1);
    writeln("t2:  ", t2);
    writeln("r1:  ", r1);
    writeln("r2:  ", r2);
    writeln();
    writeln("Original x1       : ", x1);
    writeln("Recovered from r1 : ", mont.reduce(r1));
    writeln("Original x2       : ", x2);
    writeln("Recovered from r2 : ", mont.reduce(r2));

    writeln("\nMontgomery computation of x1 ^ x2 mod m :");
    auto prod = mont.reduce(mont.rrm);
    auto base = mont.reduce(x1 * mont.rrm);
    auto exp = x2;
    while (exp.bitLength() > 0) {
        if ((exp & 1) == 1) prod = mont.reduce(prod * base);
        exp >>= 1;
        base = mont.reduce(base * base);
    }
    writeln(mont.reduce(prod));
    writeln("\nAlternate computation of x1 ^ x2 mod m :");
    writeln(x1.modPow(x2, m));
}
```

{{out}}

```txt
b :  2
n :  100
r :  1267650600228229401496703205376
m :  750791094644726559640638407699
t1:  323165824550862327179367294465482435542970161392400401329100
t2:  308607334419945011411837686695175944083084270671482464168730
r1:  440160025148131680164261562101
r2:  435362628198191204145287283255

Original x1       : 540019781128412936473322405310
Recovered from r1 : 540019781128412936473322405310
Original x2       : 515692107665463680305819378593
Recovered from r2 : 515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m :
151232511393500655853002423778

Alternate computation of x1 ^ x2 mod m :
151232511393500655853002423778
```



## Go


```go
package main

import (
    "fmt"
    "math/big"
    "math/rand"
    "time"
)

// mont holds numbers useful for working in Mongomery representation.
type mont struct {
    n  uint     // m.BitLen()
    m  *big.Int // modulus, must be odd
    r2 *big.Int // (1<<2n) mod m
}

// constructor
func newMont(m *big.Int) *mont {
    if m.Bit(0) != 1 {
        return nil
    }
    n := uint(m.BitLen())
    x := big.NewInt(1)
    x.Sub(x.Lsh(x, n), m)
    return &mont{n, new(big.Int).Set(m), x.Mod(x.Mul(x, x), m)}
}

// Montgomery reduction algorithm
func (m mont) reduce(t *big.Int) *big.Int {
    a := new(big.Int).Set(t)
    for i := uint(0); i < m.n; i++ {
        if a.Bit(0) == 1 {
            a.Add(a, m.m)
        }
        a.Rsh(a, 1)
    }
    if a.Cmp(m.m) >= 0 {
        a.Sub(a, m.m)
    }
    return a
}

// example use:
func main() {
    const n = 100 // bit length for numbers in example

    // generate random n-bit odd number for modulus m
    rnd := rand.New(rand.NewSource(time.Now().UnixNano()))
    one := big.NewInt(1)
    r1 := new(big.Int).Lsh(one, n-1)
    r2 := new(big.Int).Lsh(one, n-2)
    m := new(big.Int)
    m.Or(r1, m.Or(m.Lsh(m.Rand(rnd, r2), 1), one))

    // make Montgomery reduction object around m
    mr := newMont(m)

    // generate a couple more numbers in the range 0..m.
    // these are numbers we will do some computations on, mod m.
    x1 := new(big.Int).Rand(rnd, m)
    x2 := new(big.Int).Rand(rnd, m)

    // t1, t2 are examples of T, from the task description.
    // Generated this way, they will be in the range 0..m^2, and so < mR.
    t1 := new(big.Int).Mul(x1, mr.r2)
    t2 := new(big.Int).Mul(x2, mr.r2)

    // reduce.  r1 and r2 are now montgomery representations of x1 and x2.
    r1 = mr.reduce(t1)
    r2 = mr.reduce(t2)

    // this is the end of what is described in the task so far.
    fmt.Println("b:  2")
    fmt.Println("n: ", mr.n)
    fmt.Println("r: ", new(big.Int).Lsh(one, mr.n))
    fmt.Println("m: ", mr.m)
    fmt.Println("t1:", t1)
    fmt.Println("t2:", t2)
    fmt.Println("r1:", r1)
    fmt.Println("r2:", r2)

    // but now demonstrate that it works:
    fmt.Println()
    fmt.Println("Original x1:       ", x1)
    fmt.Println("Recovererd from r1:", mr.reduce(r1))
    fmt.Println("Original x2:       ", x2)
    fmt.Println("Recovererd from r2:", mr.reduce(r2))

    // and demonstrate a use:
    fmt.Println("\nMontgomery computation of x1 ^ x2 mod m:")
    // this is the modular exponentiation algorithm, except we call
    // mont.reduce instead of using a mod function.
    prod := mr.reduce(mr.r2)             // 1
    base := mr.reduce(t1.Mul(x1, mr.r2)) // x1^1
    exp := new(big.Int).Set(x2)          // not reduced
    for exp.BitLen() > 0 {
        if exp.Bit(0) == 1 {
            prod = mr.reduce(prod.Mul(prod, base))
        }
        exp.Rsh(exp, 1)
        base = mr.reduce(base.Mul(base, base))
    }
    fmt.Println(mr.reduce(prod))

    // show library-based equivalent computation as a check
    fmt.Println("\nLibrary-based computation of x1 ^ x2 mod m:")
    fmt.Println(new(big.Int).Exp(x1, x2, m))
}
```

{{out}}

```txt

b:  2
n:  100
r:  1267650600228229401496703205376
m:  750791094644726559640638407699
t1: 323165824550862327179367294465482435542970161392400401329100
t2: 308607334419945011411837686695175944083084270671482464168730
r1: 440160025148131680164261562101
r2: 435362628198191204145287283255

Original x1:        540019781128412936473322405310
Recovererd from r1: 540019781128412936473322405310
Original x2:        515692107665463680305819378593
Recovererd from r2: 515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m:
151232511393500655853002423778

Library based computation of x1 ^ x2 mod m:
151232511393500655853002423778

```



## Java

{{trans|Kotlin}}

```java
import java.math.BigInteger;

public class MontgomeryReduction {
    private static final BigInteger ZERO = BigInteger.ZERO;
    private static final BigInteger ONE = BigInteger.ONE;
    private static final BigInteger TWO = BigInteger.valueOf(2);

    public static class Montgomery {
        public static final int BASE = 2;

        BigInteger m;
        BigInteger rrm;
        int n;

        public Montgomery(BigInteger m) {
            if (m.compareTo(BigInteger.ZERO) <= 0 || !m.testBit(0)) {
                throw new IllegalArgumentException();
            }
            this.m = m;
            this.n = m.bitLength();
            this.rrm = ONE.shiftLeft(n * 2).mod(m);
        }

        public BigInteger reduce(BigInteger t) {
            BigInteger a = t;
            for (int i = 0; i < n; i++) {
                if (a.testBit(0)) a = a.add(this.m);
                a = a.shiftRight(1);
            }
            if (a.compareTo(m) >= 0) a = a.subtract(this.m);
            return a;
        }
    }

    public static void main(String[] args) {
        BigInteger m  = new BigInteger("750791094644726559640638407699");
        BigInteger x1 = new BigInteger("540019781128412936473322405310");
        BigInteger x2 = new BigInteger("515692107665463680305819378593");

        Montgomery mont = new Montgomery(m);
        BigInteger t1 = x1.multiply(mont.rrm);
        BigInteger t2 = x2.multiply(mont.rrm);

        BigInteger r1 = mont.reduce(t1);
        BigInteger r2 = mont.reduce(t2);
        BigInteger r = ONE.shiftLeft(mont.n);

        System.out.printf("b :  %s\n", Montgomery.BASE);
        System.out.printf("n :  %s\n", mont.n);
        System.out.printf("r :  %s\n", r);
        System.out.printf("m :  %s\n", mont.m);
        System.out.printf("t1:  %s\n", t1);
        System.out.printf("t2:  %s\n", t2);
        System.out.printf("r1:  %s\n", r1);
        System.out.printf("r2:  %s\n", r2);
        System.out.println();
        System.out.printf("Original x1       :  %s\n", x1);
        System.out.printf("Recovered from r1 :  %s\n", mont.reduce(r1));
        System.out.printf("Original x2       :  %s\n", x2);
        System.out.printf("Recovered from r2 :  %s\n", mont.reduce(r2));

        System.out.println();
        System.out.println("Montgomery computation of x1 ^ x2 mod m :");
        BigInteger prod = mont.reduce(mont.rrm);
        BigInteger base = mont.reduce(x1.multiply(mont.rrm));
        BigInteger exp = x2;
        while (exp.bitLength()>0) {
            if (exp.testBit(0)) prod=mont.reduce(prod.multiply(base));
            exp = exp.shiftRight(1);
            base = mont.reduce(base.multiply(base));
        }
        System.out.println(mont.reduce(prod));

        System.out.println();
        System.out.println("Library-based computation of x1 ^ x2 mod m :");
        System.out.println(x1.modPow(x2, m));
    }
}
```

{{out}}

```txt
b :  2
n :  100
r :  1267650600228229401496703205376
m :  750791094644726559640638407699
t1:  323165824550862327179367294465482435542970161392400401329100
t2:  308607334419945011411837686695175944083084270671482464168730
r1:  440160025148131680164261562101
r2:  435362628198191204145287283255

Original x1       :  540019781128412936473322405310
Recovered from r1 :  540019781128412936473322405310
Original x2       :  515692107665463680305819378593
Recovered from r2 :  515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m :
151232511393500655853002423778

Library-based computation of x1 ^ x2 mod m :
151232511393500655853002423778
```



## Julia

{{trans|Python}}

```julia
""" base 2 type Montgomery numbers """
struct Montgomery2
    m::BigInt
    n::Int64
    rrm::BigInt
end

function Montgomery2(x::BigInt)
    bitlen = length(string(x, base=2))
    r = (x == 0) ? 0 : (BigInt(1) << (bitlen * 2)) % x
    Montgomery2(x, bitlen, r)
end
Montgomery2(n) = Montgomery2(BigInt(n))

function reduce(mm::Montgomery2, t)
    a = BigInt(t)
    for i in 1:mm.n
        if isodd(a)
            a += mm.m
        end
        a >>= 1
    end
    return a >= mm.m ? a - mm.m : a
end

BASE(::Montgomery2) = 2

const mmm = Montgomery2(20)

function testmontgomery2()
    m = big"750791094644726559640638407699"
    x1 = big"540019781128412936473322405310"
    x2 = big"515692107665463680305819378593"

    mont = Montgomery2(m)
    t1 = x1 * mont.rrm
    t2 = x2 * mont.rrm
    r1 = reduce(mont, t1)
    r2 = reduce(mont, t2)
    r = 1 << mont.n
    println("b : ", BASE(mont))
    println("n : ", mont.n)
    println("r : ", r)
    println("m : ", mont.m)
    println("t1: ", t1)
    println("t2: ", t2)
    println("r1: ", r1)
    println("r2: ", r2)
    println()
    println("Original x1       :", x1)
    println("Recovered from r1 :", reduce(mont, r1))
    println("Original x2       :", x2)
    println("Recovered from r2 :", reduce(mont, r2))
    println("\nMontgomery computation of x1 ^ x2 mod m:")
    prod = reduce(mont, mont.rrm)
    base = reduce(mont, x1 * mont.rrm)
    pow = x2
    while pow > 0
        if isodd(pow)
            prod = reduce(mont, prod * base)
        end
        pow >>= 1
        base = reduce(mont, base * base)
    end
    println(reduce(mont, prod))
    println("\nAlternate computation of x1 ^ x2 mod m :")
    println(powermod(x1, x2, m))
end

testmontgomery2()

```
{{out}}

```txt

b : 2
n : 100
r : 0
m : 750791094644726559640638407699
t1: 323165824550862327179367294465482435542970161392400401329100
t2: 308607334419945011411837686695175944083084270671482464168730
r1: 440160025148131680164261562101
r2: 435362628198191204145287283255

Original x1       :540019781128412936473322405310
Recovered from r1 :540019781128412936473322405310
Original x2       :515692107665463680305819378593
Recovered from r2 :515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m:
151232511393500655853002423778

Alternate computation of x1 ^ x2 mod m :
151232511393500655853002423778

```




## Kotlin

{{trans|Go}}

```scala
// version 1.1.3

import java.math.BigInteger

val bigZero = BigInteger.ZERO
val bigOne  = BigInteger.ONE
val bigTwo  = BigInteger.valueOf(2L)

class Montgomery(val m: BigInteger) {
    val n:   Int
    val rrm: BigInteger

    init {
        require(m > bigZero && m.testBit(0)) // must be positive and odd
        n = m.bitLength()       
        rrm = bigOne.shiftLeft(n * 2).mod(m)
    }

    fun reduce(t: BigInteger): BigInteger {
        var a = t
        for (i in 0 until n) {
            if (a.testBit(0)) a += m
            a = a.shiftRight(1)
        }
        if (a >= m) a -= m
        return a
    }

    companion object {
        const val BASE = 2
    }
}

fun main(args: Array<String>) {
    val m  = BigInteger("750791094644726559640638407699")
    val x1 = BigInteger("540019781128412936473322405310")
    val x2 = BigInteger("515692107665463680305819378593")

    val mont = Montgomery(m)
    val t1 = x1 * mont.rrm
    val t2 = x2 * mont.rrm

    val r1 = mont.reduce(t1)
    val r2 = mont.reduce(t2)
    val r  = bigOne.shiftLeft(mont.n)

    println("b :  ${Montgomery.BASE}")
    println("n :  ${mont.n}")
    println("r :  $r")
    println("m :  ${mont.m}")   
    println("t1:  $t1")
    println("t2:  $t2")
    println("r1:  $r1")
    println("r2:  $r2")
    println()
    println("Original x1       : $x1")
    println("Recovered from r1 : ${mont.reduce(r1)}")
    println("Original x2       : $x2")
    println("Recovered from r2 : ${mont.reduce(r2)}")

    println("\nMontgomery computation of x1 ^ x2 mod m :")
    var prod = mont.reduce(mont.rrm)
    var base = mont.reduce(x1 * mont.rrm)
    var exp  = x2
    while (exp.bitLength() > 0) {
        if (exp.testBit(0)) prod = mont.reduce(prod * base)
        exp = exp.shiftRight(1)
        base = mont.reduce(base * base)
    }
    println(mont.reduce(prod)) 
    println("\nLibrary-based computation of x1 ^ x2 mod m :")
    println(x1.modPow(x2, m))
}
```


{{out}}

```txt

b :  2
n :  100
r :  1267650600228229401496703205376
m :  750791094644726559640638407699
t1:  323165824550862327179367294465482435542970161392400401329100
t2:  308607334419945011411837686695175944083084270671482464168730
r1:  440160025148131680164261562101
r2:  435362628198191204145287283255

Original x1       : 540019781128412936473322405310
Recovered from r1 : 540019781128412936473322405310
Original x2       : 515692107665463680305819378593
Recovered from r2 : 515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m :
151232511393500655853002423778

Library-based computation of x1 ^ x2 mod m :
151232511393500655853002423778

```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use bigint;
use ntheory qw(powmod);

sub msb {
  my ($n, $base) = (shift, 0);
  $base++ while $n >>= 1;
  $base;
}

sub montgomery_reduce {
    my($m, $a) = @_;
    for (0 .. msb($m)) {
        $a += $m if $a & 1;
        $a >>= 1
    }
    $a % $m
}

my $m  = 750791094644726559640638407699;
my $t1 = 323165824550862327179367294465482435542970161392400401329100;

my $r1 = 440160025148131680164261562101;
my $r2 = 435362628198191204145287283255;

my $x1 = 540019781128412936473322405310;
my $x2 = 515692107665463680305819378593;

printf "Original x1:       %s\n", $x1;
printf "Recovered from r1: %s\n", montgomery_reduce($m, $r1);
printf "Original x2:       %s\n", $x2;
printf "Recovered from r2: %s\n", montgomery_reduce($m, $r2);

print "\nMontgomery  computation x1**x2 mod m: ";
my $prod = montgomery_reduce($m, $t1/$x1);
my $base = montgomery_reduce($m, $t1);

for (my $exponent = $x2; $exponent >= 0; $exponent >>= 1) {
    $prod = montgomery_reduce($m, $prod * $base) if $exponent & 1;
    $base = montgomery_reduce($m, $base * $base);
    last if $exponent == 0;
}

print montgomery_reduce($m, $prod) . "\n";
printf "Built-in op computation x1**x2 mod m: %s\n", powmod($x1, $x2, $m);
```

{{out}}

```txt
Original x1:       540019781128412936473322405310
Recovered from r1: 540019781128412936473322405310
Original x2:       515692107665463680305819378593
Recovered from r2: 515692107665463680305819378593

Montgomery  computation x1**x2 mod m: 151232511393500655853002423778
Built-in op computation x1**x2 mod m: 151232511393500655853002423778
```



## Perl 6

{{works with|Rakudo|2018.03}}
{{trans|Sidef}}


```perl6
sub montgomery-reduce($m, $a is copy) {
    for 0..$m.msb {
        $a += $m if $a +& 1;
        $a +>= 1
    }
    $a % $m
}

my $m  = 750791094644726559640638407699;
my $t1 = 323165824550862327179367294465482435542970161392400401329100;

my $r1 = 440160025148131680164261562101;
my $r2 = 435362628198191204145287283255;

my $x1 = 540019781128412936473322405310;
my $x2 = 515692107665463680305819378593;

say "Original x1:       ", $x1;
say "Recovered from r1: ", montgomery-reduce($m, $r1);
say "Original x2:       ", $x2;
say "Recovered from r2: ", montgomery-reduce($m, $r2);

print "\nMontgomery  computation x1**x2 mod m: ";
my $prod = montgomery-reduce($m, $t1/$x1);
my $base = montgomery-reduce($m, $t1);

for $x2, {$_ +> 1} ... 0 -> $exponent {
    $prod = montgomery-reduce($m, $prod * $base) if $exponent +& 1;
    $base = montgomery-reduce($m, $base * $base);
}

say montgomery-reduce($m, $prod);
say "Built-in op computation x1**x2 mod m: ", $x1.expmod($x2, $m);
```

{{out}}

```txt
Original x1:       540019781128412936473322405310
Recovered from r1: 540019781128412936473322405310
Original x2:       515692107665463680305819378593
Recovered from r2: 515692107665463680305819378593

Montgomery  computation x1**x2 mod m: 151232511393500655853002423778
Built-in op computation x1**x2 mod m: 151232511393500655853002423778

```



## Phix

{{trans|D}}
{{libheader|mpfr}}

```Phix
include mpfr.e
 
enum BASE, BITLEN, MODULUS, RRM
 
function reduce(sequence mont, mpz a)
    integer n = mont[BITLEN]
    mpz m = mont[MODULUS],
        r = mpz_init_set(a)
    for i=1 to n do
        if mpz_odd(r) then
            mpz_add(r,r,m)
        end if
        {} = mpz_fdiv_q_ui(r,r,2)
    end for
    if mpz_cmp(r,m)>=0 then mpz_sub(r,r,m) end if
    return r
end function
 
function Montgomery(mpz m)
    if mpz_sign(m)=-1 then crash("must be positive") end if
    if not mpz_odd(m) then crash("must be odd") end if
    integer n = mpz_sizeinbase(m,2)
    mpz rrm = mpz_init(2)
    mpz_powm_ui(rrm,rrm,n*2,m)
    return {2,  -- BASE
            n,  -- BITLEN
            m,  -- MODULUS
            rrm -- 1<<(n*2) % m
           }
end function
 
mpz m = mpz_init("750791094644726559640638407699"),
    x1 = mpz_init("540019781128412936473322405310"),
    x2 = mpz_init("515692107665463680305819378593"),
    t1 = mpz_init(),
    t2 = mpz_init()
 
sequence mont = Montgomery(m)
mpz_mul(t1,x1,mont[RRM])
mpz_mul(t2,x2,mont[RRM])
mpz r1 = reduce(mont,t1),
    r2 = reduce(mont,t2),
    r = mpz_init()
mpz_ui_pow_ui(r,2,mont[BITLEN])
 
printf(1,"b :  %d\n", {mont[BASE]})
printf(1,"n :  %d\n", {mont[BITLEN]})
printf(1,"r :  %s\n", {mpz_get_str(r)})
printf(1,"m :  %s\n", {mpz_get_str(mont[MODULUS])})
printf(1,"t1:  %s\n", {mpz_get_str(t1)})
printf(1,"t2:  %s\n", {mpz_get_str(t2)})
printf(1,"r1:  %s\n", {mpz_get_str(r1)})
printf(1,"r2:  %s\n", {mpz_get_str(r2)})
printf(1,"\n")
printf(1,"Original x1       : %s\n", {mpz_get_str(x1)})
printf(1,"Recovered from r1 : %s\n", {mpz_get_str(reduce(mont,r1))})
printf(1,"Original x2       : %s\n", {mpz_get_str(x2)})
printf(1,"Recovered from r2 : %s\n", {mpz_get_str(reduce(mont,r2))})
 
printf(1,"\nMontgomery computation of x1 ^ x2 mod m :")
mpz prod = reduce(mont,mont[RRM])
mpz_mul(r,x1,mont[RRM])
mpz base = reduce(mont,r),
    expn = mpz_init_set(x2)

while mpz_cmp_si(expn,0)!=0 do
    if mpz_odd(expn) then
        mpz_mul(prod,prod,base)
        prod = reduce(mont,prod)
    end if
    {} = mpz_fdiv_q_ui(expn,expn,2)
    mpz_mul(base,base,base)
    base = reduce(mont,base)
end while
printf(1,"%s\n",{mpz_get_str(reduce(mont,prod))})
printf(1," alternate computation of x1 ^ x2 mod m :")
mpz_powm(r,x1,x2,m)
printf(1,"%s\n",{mpz_get_str(r)})
```

{{out}}

```txt

b :  2
n :  100
r :  1267650600228229401496703205376
m :  750791094644726559640638407699
t1:  323165824550862327179367294465482435542970161392400401329100
t2:  308607334419945011411837686695175944083084270671482464168730
r1:  440160025148131680164261562101
r2:  435362628198191204145287283255

Original x1       : 540019781128412936473322405310
Recovered from r1 : 540019781128412936473322405310
Original x2       : 515692107665463680305819378593
Recovered from r2 : 515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m :151232511393500655853002423778
 alternate computation of x1 ^ x2 mod m :151232511393500655853002423778

```



## PicoLisp


```PicoLisp
(de **Mod (X Y N)
   (let M 1
      (loop
         (when (bit? 1 Y)
            (setq M (% (* M X) N)) )
         (T (=0 (setq Y (>> 1 Y))) M)
         (setq X (% (* X X) N)) ) ) )
(de rrm (M)
   (% (>> (- (* 2 Mbins)) 1) M) )
(de reduce (A)
   (do Mbins
      (and (bit? 1 A) (inc 'A M))
      (setq A (>> 1 A)) )
   (and (>= A M) (dec 'A M))
   A )
(let
   (M 750791094644726559640638407699
      Mbins (length (bin M))
      RRM (rrm M)
      X1 540019781128412936473322405310
      X2 515692107665463680305819378593
      T1 (* X1 RRM)
      T2 (* X2 RRM)
      R1 (reduce T1)
      R2 (reduce T2)
      R (>> (- Mbins) 1)
      Prod (reduce RRM)
      Base (reduce (* X1 RRM))
      Exp X2 )
   (println 'b ': 2)
   (println 'n ': Mbins)
   (println 'r ': R)
   (println 'm ': M)
   (println 't1 ': T1)
   (println 't2 ': T2)
   (println 'r1 ': R1)
   (println 'r2 ': R2)
   (prinl)
   (prinl "Original x1       : " X1)
   (prinl "Recovered from r1 : " (reduce R1))
   (prinl "Original x2       : " X2)
   (prinl "Recovered from r2 : " (reduce R2))
   (prinl)
   (prin "Montgomery computation of x1 \^ x2 mod m : ")
   (while (gt0 Exp)
      (and
         (bit? 1 Exp)
         (setq Prod (reduce (* Prod Base))) )
      (setq
         Exp (>> 1 Exp)
         Base (reduce (* Base Base)) ) )
   (prinl (reduce Prod))
   (prinl "Montgomery computation of x1 \^ x2 mod m : " (**Mod X1 X2 M)) )
```

{{out}}

```txt
b : 2
n : 100
r : 1267650600228229401496703205376
m : 750791094644726559640638407699
t1 : 323165824550862327179367294465482435542970161392400401329100
t2 : 308607334419945011411837686695175944083084270671482464168730
r1 : 440160025148131680164261562101
r2 : 435362628198191204145287283255

Original x1       : 540019781128412936473322405310
Recovered from r1 : 540019781128412936473322405310
Original x2       : 515692107665463680305819378593
Recovered from r2 : 515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m : 151232511393500655853002423778
Montgomery computation of x1 ^ x2 mod m : 151232511393500655853002423778
```



## Python

{{trans|D}}

```python
class Montgomery:
    BASE = 2

    def __init__(self, m):
        self.m = m
        self.n = m.bit_length()
        self.rrm = (1 << (self.n * 2)) % m

    def reduce(self, t):
        a = t
        for i in xrange(self.n):
            if (a & 1) == 1:
                a = a + self.m
            a = a >> 1
        if a >= self.m:
            a = a - self.m
        return a

# Main
m = 750791094644726559640638407699
x1 = 540019781128412936473322405310
x2 = 515692107665463680305819378593

mont = Montgomery(m)
t1 = x1 * mont.rrm
t2 = x2 * mont.rrm

r1 = mont.reduce(t1)
r2 = mont.reduce(t2)
r = 1 << mont.n

print "b : ", Montgomery.BASE
print "n : ", mont.n
print "r : ", r
print "m : ", mont.m
print "t1: ", t1
print "t2: ", t2
print "r1: ", r1
print "r2: ", r2
print
print "Original x1       :", x1
print "Recovered from r1 :", mont.reduce(r1)
print "Original x2       :", x2
print "Recovered from r2 :", mont.reduce(r2)

print "\nMontgomery computation of x1 ^ x2 mod m:"
prod = mont.reduce(mont.rrm)
base = mont.reduce(x1 * mont.rrm)
exp = x2
while exp.bit_length() > 0:
    if (exp & 1) == 1:
        prod = mont.reduce(prod * base)
    exp = exp >> 1
    base = mont.reduce(base * base)
print mont.reduce(prod)
print "\nAlternate computation of x1 ^ x2 mod m :"
print pow(x1, x2, m)
```

{{out}}

```txt
b :  2
n :  100
r :  1267650600228229401496703205376
m :  750791094644726559640638407699
t1:  323165824550862327179367294465482435542970161392400401329100
t2:  308607334419945011411837686695175944083084270671482464168730
r1:  440160025148131680164261562101
r2:  435362628198191204145287283255

Original x1       : 540019781128412936473322405310
Recovered from r1 : 540019781128412936473322405310
Original x2       : 515692107665463680305819378593
Recovered from r2 : 515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m:
151232511393500655853002423778

Alternate computation of x1 ^ x2 mod m :
151232511393500655853002423778
```



## Racket



```racket
#lang typed/racket
(require math/number-theory)

(: montgomery-reduce-fn
   (-> Positive-Integer Natural [#:m-dash Natural]
       (Nonnegative-Integer Natural -> Integer)))

(: ith-digit (Integer Nonnegative-Integer Natural -> Nonnegative-Integer))
(define (ith-digit a i b)
  (modulo (quotient a (expt b i)) b))

(: m-dash (Integer Integer -> Natural))
(define (m-dash m b) ; for if you want to precompute it yourself
  (modular-inverse (- m) b))

(define ((montgomery-reduce-fn m b #:m-dash (m′ (m-dash m b))) T n)
  (define A
    (for/fold : Nonnegative-Integer
      ((A : Nonnegative-Integer T))
      ((i : Nonnegative-Integer (in-range n)))
      (let* ((a_i (ith-digit A i b))
             (u_i (modulo (* a_i m′) b)))
        (+ A (* u_i m (expt b i))))))
  (define A/b^n (quotient A (expt b n)))
  (if (>= A/b^n m)
      (- A/b^n m)
      A/b^n))

; ---------------------------------------------------------------------------------------------------
(module+ test
  (require typed/rackunit)
  
  (check-equal? (ith-digit 1234 0 10) 4)
  (check-equal? (ith-digit 1234 3 10) 1)
  
  ;; e.g. ripped off from {{trans|Go}}  
  (let ((b  2)
        (n  100)
        (r  1267650600228229401496703205376)
        (m  750791094644726559640638407699)
        (T1 323165824550862327179367294465482435542970161392400401329100)
        (T2 308607334419945011411837686695175944083084270671482464168730)
        (R1 440160025148131680164261562101)
        (R2 435362628198191204145287283255)
        (x1 540019781128412936473322405310)
        (x2 515692107665463680305819378593))
    (define mr (montgomery-reduce-fn m b))
    (check-equal? (mr R1 n) x1)
    (check-equal? (mr R2 n) x2)))
```


Tests, which are courtesy of #Go implementation, all pass.


## Sidef

{{trans|zkl}}

```ruby
func montgomeryReduce(m, a) {
    {
        a += m if a.is_odd
        a >>= 1
    } * m.as_bin.len

    a % m
}

var m  = 750791094644726559640638407699
var t1 = 323165824550862327179367294465482435542970161392400401329100

var r1 = 440160025148131680164261562101
var r2 = 435362628198191204145287283255

var x1 = 540019781128412936473322405310
var x2 = 515692107665463680305819378593

say("Original x1:        ", x1)
say("Recovererd from r1: ", montgomeryReduce(m, r1))
say("Original x2:        ", x2)
say("Recovererd from r2: ", montgomeryReduce(m, r2))

print("\nMontgomery computation of x1^x2 mod m:    ")
var prod = montgomeryReduce(m, t1/x1)
var base = montgomeryReduce(m, t1)

for (var exponent = x2; exponent ; exponent >>= 1) {
    prod = montgomeryReduce(m, prod * base) if exponent.is_odd
    base = montgomeryReduce(m, base * base)
}

say(montgomeryReduce(m, prod))
say("Library-based computation of x1^x2 mod m: ", x1.powmod(x2, m))
```

{{out}}

```txt

Original x1:        540019781128412936473322405310
Recovererd from r1: 540019781128412936473322405310
Original x2:        515692107665463680305819378593
Recovererd from r2: 515692107665463680305819378593

Montgomery computation of x1^x2 mod m:    151232511393500655853002423778
Library-based computation of x1^x2 mod m: 151232511393500655853002423778

```



## Tcl

{{in progress|lang=Tcl|day=25|month=06|year=2012}}

```tcl
package require Tcl 8.5

proc montgomeryReduction {m mDash T n {b 2}} {
    set A $T
    for {set i 0} {$i < $n} {incr i} {
	# Could be simplified for cases b==2 and b==10
	for {set j 0;set a $A} {$j < $i} {incr j} {
	    set a [expr {$a / $b}]
	}
	set ui [expr {($a % $b) * $mDash % $b}]
	incr A [expr {$ui * $m * $b**$i}]
    }
    set A [expr {$A / ($b ** $n)}]
    return [expr {$A >= $m ? $A - $m : $A}]
}
```

<!-- Not quite sure how to demonstrate this working; examples above aren't very clear… -->


## zkl

{{Trans|Go}}
Uses GMP (GNU Multi Precision library).

```zkl
var [const] BN=Import("zklBigNum");  // libGMP

fcn montgomeryReduce(modulus,T){
   _assert_(modulus.isOdd);
   a:=BN(T);	// we'll do in place math
   do(modulus.len(2)){  // bits needed to hold modulus
      if(a.isOdd) a.add(modulus);
      a.div(2);  // a>>=1
   }
   if(a>=modulus) a.sub(modulus);
   a
}
```


```zkl
    // magic numbers from the Go solution
//b:= 2;
//n:= 100;
//r:= BN("1267650600228229401496703205376");
m:= BN("750791094644726559640638407699");

t1:=BN("323165824550862327179367294465482435542970161392400401329100");
t2:=BN("308607334419945011411837686695175944083084270671482464168730");

r1:=BN("440160025148131680164261562101");
r2:=BN("435362628198191204145287283255");

x1:=BN("540019781128412936473322405310");
x2:=BN("515692107665463680305819378593");

    // now demonstrate that it works:
println("Original x1:       ", x1);
println("Recovererd from r1:",montgomeryReduce(m,r1));
println("Original x2:       ", x2);
println("Recovererd from r2:", montgomeryReduce(m,r2));

    // and demonstrate a use:
print("\nMontgomery computation of x1 ^ x2 mod m:    ");
    // this is the modular exponentiation algorithm, except we call
    // montgomeryReduce instead of using a mod function.
prod:=montgomeryReduce(m,t1/x1);	// 1
base:=montgomeryReduce(m,t1);		// x1^1
exp :=BN(x2);			        // not reduced
while(exp){
   if(exp.isOdd) prod=montgomeryReduce(m,prod.mul(base));
   exp.div(2);  // exp>>=1
   base=montgomeryReduce(m,base.mul(base));
}
println(montgomeryReduce(m,prod));
println("Library-based computation of x1 ^ x2 mod m: ",x1.powm(x2,m));
```

{{out}}

```txt

Original x1:       540019781128412936473322405310
Recovererd from r1:540019781128412936473322405310
Original x2:       515692107665463680305819378593
Recovererd from r2:515692107665463680305819378593

Montgomery computation of x1 ^ x2 mod m:    151232511393500655853002423778
Library-based computation of x1 ^ x2 mod m: 151232511393500655853002423778

```


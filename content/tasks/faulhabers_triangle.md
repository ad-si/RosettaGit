+++
title = "Faulhaber's triangle"
description = ""
date = 2019-10-09T17:18:58Z
aliases = []
[extra]
id = 21408
[taxonomies]
categories = ["task"]
tags = []
+++

Named after [https://en.wikipedia.org/wiki/Johann_Faulhaber Johann Faulhaber], the rows of Faulhaber's triangle are the coefficients of polynomials that represent sums of integer powers, which are extracted from Faulhaber's formula:


:<math>\sum_{k=1}^n k^p = {1 \over p+1} \sum_{j=0}^p {p+1 \choose j} B_j n^{p+1-j}</math>


where <math>B_n</math> is the nth-Bernoulli number.


The first 5 rows of Faulhaber's triangle, are:


```txt

    1
  1/2  1/2
  1/6  1/2  1/3
    0  1/4  1/2  1/4
-1/30    0  1/3  1/2  1/5

```



Using the third row of the triangle, we have:

<math>\sum_{k=1}^n k^2 = {1 \over 6} n + {1 \over 2} n^2 + {1 \over 3} n^3</math>


## Task

:* show the first 10 rows of Faulhaber's triangle.
:* using the 18th row of Faulhaber's triangle, compute the sum: <math>\sum_{k=1}^{1000} k^{17}</math> (extra credit).

## See also

* [[Bernoulli numbers]]
* [[Evaluate binomial coefficients]]
* [https://en.wikipedia.org/wiki/Faulhaber%27s_formula Faulhaber's formula (Wikipedia)]
* [http://www.ww.ingeniousmathstat.org/sites/default/files/Torabi-Dashti-CMJ-2011.pdf Faulhaber's triangle (PDF)]




## C

```c
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int binomial(int n, int k) {
    int num, denom, i;

    if (n < 0 || k < 0 || n < k) return -1;
    if (n == 0 || k == 0) return 1;

    num = 1;
    for (i = k + 1; i <= n; ++i) {
        num = num * i;
    }

    denom = 1;
    for (i = 2; i <= n - k; ++i) {
        denom *= i;
    }

    return num / denom;
}

int gcd(int a, int b) {
    int temp;
    while (b != 0) {
        temp = a % b;
        a = b;
        b = temp;
    }
    return a;
}

typedef struct tFrac {
    int num, denom;
} Frac;

Frac makeFrac(int n, int d) {
    Frac result;
    int g;

    if (d == 0) {
        result.num = 0;
        result.denom = 0;
        return result;
    }

    if (n == 0) {
        d = 1;
    } else if (d < 0) {
        n = -n;
        d = -d;
    }

    g = abs(gcd(n, d));
    if (g > 1) {
        n = n / g;
        d = d / g;
    }

    result.num = n;
    result.denom = d;
    return result;
}

Frac negateFrac(Frac f) {
    return makeFrac(-f.num, f.denom);
}

Frac subFrac(Frac lhs, Frac rhs) {
    return makeFrac(lhs.num * rhs.denom - lhs.denom * rhs.num, rhs.denom * lhs.denom);
}

Frac multFrac(Frac lhs, Frac rhs) {
    return makeFrac(lhs.num * rhs.num, lhs.denom * rhs.denom);
}

bool equalFrac(Frac lhs, Frac rhs) {
    return (lhs.num == rhs.num) && (lhs.denom == rhs.denom);
}

bool lessFrac(Frac lhs, Frac rhs) {
    return (lhs.num * rhs.denom) < (rhs.num * lhs.denom);
}

void printFrac(Frac f) {
    char buffer[7];
    int len;

    if (f.denom != 1) {
        snprintf(buffer, 7, "%d/%d", f.num, f.denom);
    } else {
        snprintf(buffer, 7, "%d", f.num);
    }

    len = 7 - strlen(buffer);
    while (len-- > 0) {
        putc(' ', stdout);
    }

    printf(buffer);
}

Frac bernoulli(int n) {
    Frac a[16];
    int j, m;

    if (n < 0) {
        a[0].num = 0;
        a[0].denom = 0;
        return a[0];
    }

    for (m = 0; m <= n; ++m) {
        a[m] = makeFrac(1, m + 1);
        for (j = m; j >= 1; --j) {
            a[j - 1] = multFrac(subFrac(a[j - 1], a[j]), makeFrac(j, 1));
        }
    }

    if (n != 1) {
        return a[0];
    }

    return negateFrac(a[0]);
}

void faulhaber(int p) {
    Frac q, *coeffs;
    int j, sign;

    coeffs = malloc(sizeof(Frac)*(p + 1));

    q = makeFrac(1, p + 1);
    sign = -1;
    for (j = 0; j <= p; ++j) {
        sign = -1 * sign;
        coeffs[p - j] = multFrac(multFrac(multFrac(q, makeFrac(sign, 1)), makeFrac(binomial(p + 1, j), 1)), bernoulli(j));
    }

    for (j = 0; j <= p; ++j) {
        printFrac(coeffs[j]);
    }
    printf("\n");

    free(coeffs);
}

int main() {
    int i;

    for (i = 0; i < 10; ++i) {
        faulhaber(i);
    }

    return 0;
}
```

```txt
      1
    1/2    1/2
    1/6    1/2    1/3
      0    1/4    1/2    1/4
  -1/30      0    1/3    1/2    1/5
      0  -1/12      0   5/12    1/2    1/6
   1/42      0   -1/6      0    1/2    1/2    1/7
      0   1/12      0  -7/24      0   7/12    1/2    1/8
  -1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
      0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10
```



## C++

Uses C++ 17

```cpp
#include <exception>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <sstream>
#include <vector>

class Frac {
public:
	Frac(long n, long d) {
		if (d == 0) {
			throw new std::runtime_error("d must not be zero");
		}

		long nn = n;
		long dd = d;
		if (nn == 0) {
			dd = 1;
		} else if (dd < 0) {
			nn = -nn;
			dd = -dd;
		}

		long g = abs(std::gcd(nn, dd));
		if (g > 1) {
			nn /= g;
			dd /= g;
		}

		num = nn;
		denom = dd;
	}

	Frac operator-() const {
		return Frac(-num, denom);
	}

	Frac operator+(const Frac& rhs) const {
		return Frac(num*rhs.denom + denom * rhs.num, rhs.denom*denom);
	}

	Frac operator-(const Frac& rhs) const {
		return Frac(num*rhs.denom - denom * rhs.num, rhs.denom*denom);
	}

	Frac operator*(const Frac& rhs) const {
		return Frac(num*rhs.num, denom*rhs.denom);
	}

	friend std::ostream& operator<<(std::ostream&, const Frac&);

	static Frac ZERO() {
		return Frac(0, 1);
	}

private:
	long num;
	long denom;
};

std::ostream & operator<<(std::ostream & os, const Frac &f) {
	if (f.num == 0 || f.denom == 1) {
		return os << f.num;
	}

	std::stringstream ss;
	ss << f.num << "/" << f.denom;
	return os << ss.str();
}

Frac bernoulli(int n) {
	if (n < 0) {
		throw new std::runtime_error("n may not be negative or zero");
	}

	std::vector<Frac> a;
	for (int m = 0; m <= n; m++) {
		a.push_back(Frac(1, m + 1));
		for (int j = m; j >= 1; j--) {
			a[j - 1] = (a[j - 1] - a[j]) * Frac(j, 1);
		}
	}

	// returns 'first' Bernoulli number
	if (n != 1) return a[0];
	return -a[0];
}

int binomial(int n, int k) {
	if (n < 0 || k < 0 || n < k) {
		throw new std::runtime_error("parameters are invalid");
	}
	if (n == 0 || k == 0) return 1;

	int num = 1;
	for (int i = k + 1; i <= n; i++) {
		num *= i;
	}

	int denom = 1;
	for (int i = 2; i <= n - k; i++) {
		denom *= i;
	}

	return num / denom;
}

std::vector<Frac> faulhaberTraingle(int p) {
	std::vector<Frac> coeffs;

	for (int i = 0; i < p + 1; i++) {
		coeffs.push_back(Frac::ZERO());
	}

	Frac q{ 1, p + 1 };
	int sign = -1;
	for (int j = 0; j <= p; j++) {
		sign *= -1;
		coeffs[p - j] = q * Frac(sign, 1) * Frac(binomial(p + 1, j), 1) * bernoulli(j);
	}

	return coeffs;
}

int main() {
	using namespace std;

	for (int i = 0; i < 10; i++) {
		vector<Frac> coeffs = faulhaberTraingle(i);
		for (auto it = coeffs.begin(); it != coeffs.end(); it++) {
			cout << right << setw(5) << *it << "  ";
		}
		cout << endl;
	}

	return 0;
}
```

```txt
    1
  1/2    1/2
  1/6    1/2    1/3
    0    1/4    1/2    1/4
-1/30      0    1/3    1/2    1/5
    0  -1/12      0   5/12    1/2    1/6
 1/42      0   -1/6      0    1/2    1/2    1/7
    0   1/12      0  -7/24      0   7/12    1/2    1/8
-1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
    0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10
```


## C#
```c#
using System;

namespace FaulhabersTriangle {
    internal class Frac {
        private long num;
        private long denom;

        public static readonly Frac ZERO = new Frac(0, 1);
        public static readonly Frac ONE = new Frac(1, 1);

        public Frac(long n, long d) {
            if (d == 0) {
                throw new ArgumentException("d must not be zero");
            }
            long nn = n;
            long dd = d;
            if (nn == 0) {
                dd = 1;
            }
            else if (dd < 0) {
                nn = -nn;
                dd = -dd;
            }
            long g = Math.Abs(Gcd(nn, dd));
            if (g > 1) {
                nn /= g;
                dd /= g;
            }
            num = nn;
            denom = dd;
        }

        private static long Gcd(long a, long b) {
            if (b == 0) {
                return a;
            }
            return Gcd(b, a % b);
        }

        public static Frac operator -(Frac self) {
            return new Frac(-self.num, self.denom);
        }

        public static Frac operator +(Frac lhs, Frac rhs) {
            return new Frac(lhs.num * rhs.denom + lhs.denom * rhs.num, rhs.denom * lhs.denom);
        }

        public static Frac operator -(Frac lhs, Frac rhs) {
            return lhs + -rhs;
        }

        public static Frac operator *(Frac lhs, Frac rhs) {
            return new Frac(lhs.num * rhs.num, lhs.denom * rhs.denom);
        }

        public static bool operator <(Frac lhs, Frac rhs) {
            double x = (double)lhs.num / lhs.denom;
            double y = (double)rhs.num / rhs.denom;
            return x < y;
        }

        public static bool operator >(Frac lhs, Frac rhs) {
            double x = (double)lhs.num / lhs.denom;
            double y = (double)rhs.num / rhs.denom;
            return x > y;
        }

        public static bool operator ==(Frac lhs, Frac rhs) {
            return lhs.num == rhs.num && lhs.denom == rhs.denom;
        }

        public static bool operator !=(Frac lhs, Frac rhs) {
            return lhs.num != rhs.num || lhs.denom != rhs.denom;
        }

        public override string ToString() {
            if (denom == 1) {
                return num.ToString();
            }
            return string.Format("{0}/{1}", num, denom);
        }

        public override bool Equals(object obj) {
            var frac = obj as Frac;
            return frac != null &&
                   num == frac.num &&
                   denom == frac.denom;
        }

        public override int GetHashCode() {
            var hashCode = 1317992671;
            hashCode = hashCode * -1521134295 + num.GetHashCode();
            hashCode = hashCode * -1521134295 + denom.GetHashCode();
            return hashCode;
        }
    }

    class Program {
        static Frac Bernoulli(int n) {
            if (n < 0) {
                throw new ArgumentException("n may not be negative or zero");
            }
            Frac[] a = new Frac[n + 1];
            for (int m = 0; m <= n; m++) {
                a[m] = new Frac(1, m + 1);
                for (int j = m; j >= 1; j--) {
                    a[j - 1] = (a[j - 1] - a[j]) * new Frac(j, 1);
                }
            }
            // returns 'first' Bernoulli number
            if (n != 1) return a[0];
            return -a[0];
        }

        static int Binomial(int n, int k) {
            if (n < 0 || k < 0 || n < k) {
                throw new ArgumentException();
            }
            if (n == 0 || k == 0) return 1;
            int num = 1;
            for (int i = k + 1; i <= n; i++) {
                num = num * i;
            }
            int denom = 1;
            for (int i = 2; i <= n - k; i++) {
                denom = denom * i;
            }
            return num / denom;
        }

        static Frac[] FaulhaberTriangle(int p) {
            Frac[] coeffs = new Frac[p + 1];
            for (int i = 0; i < p + 1; i++) {
                coeffs[i] = Frac.ZERO;
            }
            Frac q = new Frac(1, p + 1);
            int sign = -1;
            for (int j = 0; j <= p; j++) {
                sign *= -1;
                coeffs[p - j] = q * new Frac(sign, 1) * new Frac(Binomial(p + 1, j), 1) * Bernoulli(j);
            }
            return coeffs;
        }

        static void Main(string[] args) {
            for (int i = 0; i < 10; i++) {
                Frac[] coeffs = FaulhaberTriangle(i);
                foreach (Frac coeff in coeffs) {
                    Console.Write("{0,5}  ", coeff);
                }
                Console.WriteLine();
            }
        }
    }
}
```

```txt
    1
  1/2    1/2
  1/6    1/2    1/3
    0    1/4    1/2    1/4
-1/30      0    1/3    1/2    1/5
    0  -1/12      0   5/12    1/2    1/6
 1/42      0   -1/6      0    1/2    1/2    1/7
    0   1/12      0  -7/24      0   7/12    1/2    1/8
-1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
    0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10
```



## D

```D
import std.algorithm : fold;
import std.conv : to;
import std.exception : enforce;
import std.format : formattedWrite;
import std.numeric : cmp, gcd;
import std.range : iota;
import std.stdio;
import std.traits;

auto abs(T)(T val)
if (isNumeric!T) {
    if (val < 0) {
        return -val;
    }
    return val;
}

struct Frac {
    long num;
    long denom;

    enum ZERO = Frac(0, 1);
    enum ONE = Frac(1, 1);

    this(long n, long d) in {
        enforce(d != 0, "Parameter d may not be zero.");
    } body {
        auto nn = n;
        auto dd = d;
        if (nn == 0) {
            dd = 1;
        } else if (dd < 0) {
            nn = -nn;
            dd = -dd;
        }
        auto g = gcd(abs(nn), abs(dd));
        if (g > 1) {
            nn /= g;
            dd /= g;
        }
        num = nn;
        denom = dd;
    }

    auto opBinary(string op)(Frac rhs) const {
        static if (op == "+" || op == "-") {
            return mixin("Frac(num*rhs.denom"~op~"denom*rhs.num, rhs.denom*denom)");
        } else if (op == "*") {
            return Frac(num*rhs.num, denom*rhs.denom);
        }
    }

    auto opUnary(string op : "-")() const {
        return Frac(-num, denom);
    }

    int opCmp(Frac rhs) const {
        return cmp(cast(real) this, cast(real) rhs);
    }

    bool opEquals(Frac rhs) const {
        return num == rhs.num && denom == rhs.denom;
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        if (denom == 1) {
            formattedWrite(sink, "%d", num);
        } else {
            formattedWrite(sink, "%d/%s", num, denom);
        }
    }

    T opCast(T)() const if (isFloatingPoint!T) {
        return cast(T) num / denom;
    }
}

auto abs(Frac f) {
    if (f.num >= 0) {
        return f;
    }
    return -f;
}

auto bernoulli(int n) in {
    enforce(n >= 0, "Parameter n must not be negative.");
} body {
    Frac[] a;
    a.length = n+1;
    a[0] = Frac.ZERO;
    foreach (m; 0..n+1) {
        a[m] = Frac(1, m+1);
        foreach_reverse (j; 1..m+1) {
            a[j-1] = (a[j-1] - a[j]) * Frac(j, 1);
        }
    }
    if (n != 1) {
        return a[0];
    }
    return -a[0];
}

auto binomial(int n, int k) in {
    enforce(n>=0 && k>=0 && n>=k);
} body {
    if (n==0 || k==0) return 1;
    auto num = iota(k+1, n+1).fold!"a*b"(1);
    auto den = iota(2, n-k+1).fold!"a*b"(1);
    return num / den;
}

Frac[] faulhaberTriangle(int p) {
    Frac[] coeffs;
    coeffs.length = p+1;
    coeffs[0] = Frac.ZERO;
    auto q = Frac(1, p+1);
    auto sign = -1;
    foreach (j; 0..p+1) {
        sign *= -1;
        coeffs[p - j] = q * Frac(sign, 1) * Frac(binomial(p+1, j), 1) * bernoulli(j);
    }
    return coeffs;
}

void main() {
    foreach (i; 0..10) {
        auto coeffs = faulhaberTriangle(i);
        foreach (coeff; coeffs) {
            writef("%5s  ", coeff.to!string);
        }
        writeln;
    }
    writeln;
}
```

```txt
    1
  1/2    1/2
  1/6    1/2    1/3
    0    1/4    1/2    1/4
-1/30      0    1/3    1/2    1/5
    0  -1/12      0   5/12    1/2    1/6
 1/42      0   -1/6      0    1/2    1/2    1/7
    0   1/12      0  -7/24      0   7/12    1/2    1/8
-1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
    0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10
```


=={{header|F_Sharp|F#}}==

### The Function


```fsharp

// Generate Faulhaber's Triangle. Nigel Galloway: May 8th., 2018
let Faulhaber=let fN n = (1N - List.sum n)::n
              let rec Faul a b=seq{let t = fN (List.mapi(fun n g->b*g/BigRational.FromInt(n+2)) a)
                                   yield t
                                   yield! Faul t (b+1N)}
              Faul [] 0N

```


### The Task


```fsharp

Faulhaber |> Seq.take 10 |> Seq.iter (printfn "%A")

```

```txt

[1N]
[1/2N; 1/2N]
[1/6N; 1/2N; 1/3N]
[0N; 1/4N; 1/2N; 1/4N]
[-1/30N; 0N; 1/3N; 1/2N; 1/5N]
[0N; -1/12N; 0N; 5/12N; 1/2N; 1/6N]
[1/42N; 0N; -1/6N; 0N; 1/2N; 1/2N; 1/7N]
[0N; 1/12N; 0N; -7/24N; 0N; 7/12N; 1/2N; 1/8N]
[-1/30N; 0N; 2/9N; 0N; -7/15N; 0N; 2/3N; 1/2N; 1/9N]
[0N; -3/20N; 0N; 1/2N; 0N; -7/10N; 0N; 3/4N; 1/2N; 1/10N]

```



## Factor


```factor
USING: kernel math math.combinatorics math.extras math.functions
math.ranges prettyprint sequences ;

: faulhaber ( p -- seq )
    1 + dup recip swap dup 0 (a,b]
    [ [ nCk ] [ -1 swap ^ ] [ bernoulli ] tri * * * ] 2with map ;

10 [ faulhaber . ] each-integer
```

```txt

{ 1 }
{ 1/2 1/2 }
{ 1/6 1/2 1/3 }
{ 0 1/4 1/2 1/4 }
{ -1/30 0 1/3 1/2 1/5 }
{ 0 -1/12 0 5/12 1/2 1/6 }
{ 1/42 0 -1/6 0 1/2 1/2 1/7 }
{ 0 1/12 0 -7/24 0 7/12 1/2 1/8 }
{ -1/30 0 2/9 0 -7/15 0 2/3 1/2 1/9 }
{ 0 -3/20 0 1/2 0 -7/10 0 3/4 1/2 1/10 }

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Faulhaber this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC

```freebasic
' version 12-08-2017
' compile with: fbc -s console
' uses GMP

#Include Once "gmp.bi"

#Define i_max 17

Dim As UInteger i, j, x
Dim As String   s
Dim As ZString Ptr gmp_str : gmp_str = Allocate(100)

Dim As Mpq_ptr n, tmp1, tmp2, sum, one, zero
n    = Allocate(Len(__mpq_struct)) : Mpq_init(n)
tmp1 = Allocate(Len(__mpq_struct)) : Mpq_init(tmp1)
tmp2 = Allocate(Len(__mpq_struct)) : Mpq_init(tmp2)
sum  = Allocate(Len(__mpq_struct)) : Mpq_init(sum)
zero = Allocate(Len(__mpq_struct)) : Mpq_init(zero)
one  = Allocate(Len(__mpq_struct)) : Mpq_init(one)
Mpq_set_ui(zero, 0, 0)  ' 0/0 = 0
Mpq_set_ui(one , 1, 1)  ' 1/1 = 1

Dim As Mpq_ptr Faulhaber_triangle(0 To i_max, 1 To i_max +1)
' only initialize the variables we need
For i = 0 To i_max
    For j = 1 To i +1
        Faulhaber_triangle(i, j) = Allocate(Len(__Mpq_struct))
        Mpq_init(Faulhaber_triangle(i, j))
    Next
Next

Mpq_set(Faulhaber_triangle(0, 1), one)

' we calculate the first 18 rows
For i = 1 To i_max
    Mpq_set(sum, zero)
    For j = i +1 To 2 Step -1
        Mpq_set_ui(tmp1, i, j)            ' i / j
        Mpq_set(tmp2, Faulhaber_triangle(i -1, j -1))
        Mpq_mul(Faulhaber_triangle(i, j), tmp2, tmp1)
        Mpq_canonicalize(Faulhaber_triangle(i, j))
        Mpq_add(sum, sum, Faulhaber_triangle(i, j))
    Next
    Mpq_sub(Faulhaber_triangle(i, 1), one, sum)
Next

Print "The first 10 rows"
For i = 0 To 9
    For j = 1 To i +1
        Mpq_get_str(gmp_str, 10, Faulhaber_triangle(i, j))
        s = Space(6) + *gmp_str + Space(6)
        x = InStr(s,"/")
        If x = 0 Then x = 7               ' in case of 0 or 1
        Print Mid(s, x -3, 7);
    Next
    Print
Next
print

' using the 17'the row
Mpq_set(sum, zero)
Mpq_set_ui(n, 1000, 1)                    ' 1000/1 = 1000
Mpq_set(tmp2, n)
For j = 1 To 18
    Mpq_mul(tmp1, n, Faulhaber_triangle(17, j))
    Mpq_add(sum, sum, tmp1)
    Mpq_mul(n, n, tmp2)
Next

Mpq_get_str(gmp_str, 10, sum)
Print *gmp_str

' free memory
DeAllocate(gmp_str)
Mpq_clear(tmp1) : Mpq_clear(tmp2) : Mpq_clear(n)
Mpq_clear(zero) : Mpq_clear(one)  : Mpq_clear(sum)

For i = 0 To i_max
    For j = 1 To i +1
        Mpq_clear(Faulhaber_triangle(i, j))
    Next
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
The first 10 rows
   1
  1/2    1/2
  1/6    1/2    1/3
   0     1/4    1/2    1/4
 -1/30    0     1/3    1/2    1/5
   0    -1/12    0     5/12   1/2    1/6
  1/42    0    -1/6     0     1/2    1/2    1/7
   0     1/12    0    -7/24    0     7/12   1/2    1/8
 -1/30    0     2/9     0    -7/15    0     2/3    1/2    1/9
   0    -3/20    0     1/2     0    -7/10    0     3/4    1/2    1/10

56056972216555580111030077961944183400198333273050000
```



## Go

Except that there is no need to roll our own Frac type when we can use the big.Rat type from the Go standard library.

```go
package main

import (
    "fmt"
    "math/big"
)

func bernoulli(n uint) *big.Rat {
    a := make([]big.Rat, n+1)
    z := new(big.Rat)
    for m := range a {
        a[m].SetFrac64(1, int64(m+1))
        for j := m; j >= 1; j-- {
            d := &a[j-1]
            d.Mul(z.SetInt64(int64(j)), d.Sub(d, &a[j]))
        }
    }
    // return the 'first' Bernoulli number
    if n != 1 {
        return &a[0]
    }
    a[0].Neg(&a[0])
    return &a[0]
}

func binomial(n, k int) int64 {
    if n <= 0 || k <= 0 || n < k {
        return 1
    }
    var num, den int64 = 1, 1
    for i := k + 1; i <= n; i++ {
        num *= int64(i)
    }
    for i := 2; i <= n-k; i++ {
        den *= int64(i)
    }
    return num / den
}

func faulhaberTriangle(p int) []big.Rat {
    coeffs := make([]big.Rat, p+1)
    q := big.NewRat(1, int64(p)+1)
    t := new(big.Rat)
    u := new(big.Rat)
    sign := -1
    for j := range coeffs {
        sign *= -1
        d := &coeffs[p-j]
        t.SetInt64(int64(sign))
        u.SetInt64(binomial(p+1, j))
        d.Mul(q, t)
        d.Mul(d, u)
        d.Mul(d, bernoulli(uint(j)))
    }
    return coeffs
}

func main() {
    for i := 0; i < 10; i++ {
        coeffs := faulhaberTriangle(i)
        for _, coeff := range coeffs {
            fmt.Printf("%5s  ", coeff.RatString())
        }
        fmt.Println()
    }
    fmt.Println()
    // get coeffs for (k + 1)th row
    k := 17
    cc := faulhaberTriangle(k)
    n := int64(1000)
    nn := big.NewRat(n, 1)
    np := big.NewRat(1, 1)
    sum := new(big.Rat)
    tmp := new(big.Rat)
    for _, c := range cc {
        np.Mul(np, nn)
        tmp.Set(np)
        tmp.Mul(tmp, &c)
        sum.Add(sum, tmp)
    }
    fmt.Println(sum.RatString())
}
```


```txt

    1
  1/2    1/2
  1/6    1/2    1/3
    0    1/4    1/2    1/4
-1/30      0    1/3    1/2    1/5
    0  -1/12      0   5/12    1/2    1/6
 1/42      0   -1/6      0    1/2    1/2    1/7
    0   1/12      0  -7/24      0   7/12    1/2    1/8
-1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
    0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10

56056972216555580111030077961944183400198333273050000

```



## Haskell

```haskell
import Data.Ratio (Ratio, numerator, denominator, (%))
import Control.Arrow ((&&&))

-- FAULHABER -------------------------------------------------------------------
-- Infinite list of rows of Faulhaber's triangle
faulhaberTriangle :: [[Rational]]
faulhaberTriangle =
  tail $
  scanl
    (\rs n ->
        let xs = zipWith ((*) . (n %)) [2 ..] rs
        in 1 - sum xs : xs)
    []
    [0 ..]

-- p -> n -> Sum of the p-th powers of the first n positive integers
faulhaber :: Int -> Rational -> Rational
faulhaber p n = sum (zipWith ((*) . (n ^)) [1 ..] (faulhaberTriangle !! p))

-- DISPLAY ---------------------------------------------------------------------
-- (Max numerator+denominator widths) -> Column width -> Filler -> Ratio -> String
justifyRatio :: (Int, Int) -> Int -> Char -> Rational -> String
justifyRatio (wn, wd) n c nd =
  let [num, den] = [numerator, denominator] <*> [nd]
      w = max n (wn + wd + 2) -- Minimum column width, or more if specified.
  in if 1 == den
       then center w c (show num)
       else let (q, r) = quotRem (w - 1) 2
            in concat
                 [ justifyRight q c (show num)
                 , "/"
                 , justifyLeft (q + r) c (show den)
                 ]

center, justifyLeft, justifyRight :: Int -> Char -> String -> String
center n c s =
  let (q, r) = quotRem (n - length s) 2
  in concat [replicate q c, s, replicate (q + r) c]

justifyLeft n c s = take n (s ++ replicate n c)

justifyRight n c s = drop (length s) (replicate n c ++ s)

-- List of Ratios -> (Max numerator width, Max denominator width)
maxWidths :: [[Rational]] -> (Int, Int)
maxWidths xss =
  let widest f xs = maximum $ fmap (length . show . f) xs
  in widest numerator &&& widest denominator $ concat xss

-- TEST ------------------------------------------------------------------------
main :: IO ()
main = do
  let triangle = take 10 faulhaberTriangle
      widths = maxWidths triangle
  mapM_
    putStrLn
    [ unlines ((justifyRatio widths 8 ' ' =<<) <$> triangle)
    , (show . numerator) (faulhaber 17 1000)
    ]
```

```txt
   1
  1/2     1/2
  1/6     1/2     1/3
   0      1/4     1/2     1/4
 -1/30     0      1/3     1/2     1/5
   0     -1/12     0      5/12    1/2     1/6
  1/42     0     -1/6      0      1/2     1/2     1/7
   0      1/12     0     -7/24     0      7/12    1/2     1/8
 -1/30     0      2/9      0     -7/15     0      2/3     1/2     1/9
   0     -3/20     0      1/2      0     -7/10     0      3/4     1/2     1/10

56056972216555580111030077961944183400198333273050000
```



## Java

```Java
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Arrays;
import java.util.stream.LongStream;

public class FaulhabersTriangle {
    private static final MathContext MC = new MathContext(256);

    private static long gcd(long a, long b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }

    private static class Frac implements Comparable<Frac> {
        private long num;
        private long denom;

        public static final Frac ZERO = new Frac(0, 1);

        public Frac(long n, long d) {
            if (d == 0) throw new IllegalArgumentException("d must not be zero");
            long nn = n;
            long dd = d;
            if (nn == 0) {
                dd = 1;
            } else if (dd < 0) {
                nn = -nn;
                dd = -dd;
            }
            long g = Math.abs(gcd(nn, dd));
            if (g > 1) {
                nn /= g;
                dd /= g;
            }
            num = nn;
            denom = dd;
        }

        public Frac plus(Frac rhs) {
            return new Frac(num * rhs.denom + denom * rhs.num, rhs.denom * denom);
        }

        public Frac unaryMinus() {
            return new Frac(-num, denom);
        }

        public Frac minus(Frac rhs) {
            return this.plus(rhs.unaryMinus());
        }

        public Frac times(Frac rhs) {
            return new Frac(this.num * rhs.num, this.denom * rhs.denom);
        }

        @Override
        public int compareTo(Frac o) {
            double diff = toDouble() - o.toDouble();
            return Double.compare(diff, 0.0);
        }

        @Override
        public boolean equals(Object obj) {
            return null != obj && obj instanceof Frac && this.compareTo((Frac) obj) == 0;
        }

        @Override
        public String toString() {
            if (denom == 1) {
                return Long.toString(num);
            }
            return String.format("%d/%d", num, denom);
        }

        public double toDouble() {
            return (double) num / denom;
        }

        public BigDecimal toBigDecimal() {
            return BigDecimal.valueOf(num).divide(BigDecimal.valueOf(denom), MC);
        }
    }

    private static Frac bernoulli(int n) {
        if (n < 0) throw new IllegalArgumentException("n may not be negative or zero");
        Frac[] a = new Frac[n + 1];
        Arrays.fill(a, Frac.ZERO);
        for (int m = 0; m <= n; ++m) {
            a[m] = new Frac(1, m + 1);
            for (int j = m; j >= 1; --j) {
                a[j - 1] = a[j - 1].minus(a[j]).times(new Frac(j, 1));
            }
        }
        // returns 'first' Bernoulli number
        if (n != 1) return a[0];
        return a[0].unaryMinus();
    }

    private static long binomial(int n, int k) {
        if (n < 0 || k < 0 || n < k) throw new IllegalArgumentException();
        if (n == 0 || k == 0) return 1;
        long num = LongStream.rangeClosed(k + 1, n).reduce(1, (a, b) -> a * b);
        long den = LongStream.rangeClosed(2, n - k).reduce(1, (acc, i) -> acc * i);
        return num / den;
    }

    private static Frac[] faulhaberTriangle(int p) {
        Frac[] coeffs = new Frac[p + 1];
        Arrays.fill(coeffs, Frac.ZERO);
        Frac q = new Frac(1, p + 1);
        int sign = -1;
        for (int j = 0; j <= p; ++j) {
            sign *= -1;
            coeffs[p - j] = q.times(new Frac(sign, 1)).times(new Frac(binomial(p + 1, j), 1)).times(bernoulli(j));
        }
        return coeffs;
    }

    public static void main(String[] args) {
        for (int i = 0; i <= 9; ++i) {
            Frac[] coeffs = faulhaberTriangle(i);
            for (Frac coeff : coeffs) {
                System.out.printf("%5s  ", coeff);
            }
            System.out.println();
        }
        System.out.println();
        // get coeffs for (k + 1)th row
        int k = 17;
        Frac[] cc = faulhaberTriangle(k);
        int n = 1000;
        BigDecimal nn = BigDecimal.valueOf(n);
        BigDecimal np = BigDecimal.ONE;
        BigDecimal sum = BigDecimal.ZERO;
        for (Frac c : cc) {
            np = np.multiply(nn);
            sum = sum.add(np.multiply(c.toBigDecimal()));
        }
        System.out.println(sum.toBigInteger());
    }
}
```

```txt
    1
  1/2    1/2
  1/6    1/2    1/3
    0    1/4    1/2    1/4
-1/30      0    1/3    1/2    1/5
    0  -1/12      0   5/12    1/2    1/6
 1/42      0   -1/6      0    1/2    1/2    1/7
    0   1/12      0  -7/24      0   7/12    1/2    1/8
-1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
    0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10
```



## JavaScript


### ES6

JavaScript is probably not the right instrument to choose for this task, which requires both a ratio number type and arbitrary precision integers. JavaScript has neither – its only numeric datatype is the IEEE 754 double-precision floating-point format number, into which integers and all else must fit. (See the built-in JS name '''Number.MAX_SAFE_INTEGER''')

This means that we can print Faulhaber's triangle (hand-coding some rudimentary ratio-arithmetic functions), but our only reward for evaluating faulhaber(17, 1000) is an integer overflow. With JS integers out of the box, we can get about as far as '''faulhaber(17, 8)''', or '''faulhaber(4, 1000)'''.

(Further progress would entail implementing some hand-crafted representation of arbitrary precision integers – perhaps a bit beyond the intended scope of this task, and good enough motivation to use a different language)

```JavaScript
(() => {

    // Order of Faulhaber's triangle -> rows of Faulhaber's triangle
    // faulHaberTriangle :: Int -> [[Ratio Int]]
    const faulhaberTriangle = n =>
        map(x => tail(
                scanl((a, x) => {
                    const ys = map((nd, i) =>
                        ratioMult(nd, Ratio(x, i + 2)), a);
                    return cons(ratioMinus(Ratio(1, 1), ratioSum(ys)), ys);
                }, [], enumFromTo(0, x))
            ),
            enumFromTo(0, n));

    // p -> n -> Sum of the p-th powers of the first n positive integers
    // faulhaber :: Int -> Ratio Int -> Ratio Int
    const faulhaber = (p, n) =>
        ratioSum(map(
            (nd, i) => ratioMult(nd, Ratio(raise(n, i + 1), 1)),
            last(faulhaberTriangle(p))
        ));

    // RATIOS -----------------------------------------------------------------

    // (Max numr + denr widths) -> Column width -> Filler -> Ratio -> String
    // justifyRatio :: (Int, Int) -> Int -> Char -> Ratio Integer -> String
    const justifyRatio = (ws, n, c, nd) => {
        const
            w = max(n, ws.nMax + ws.dMax + 2),
            [num, den] = [nd.num, nd.den];
        return all(Number.isSafeInteger, [num, den]) ? (
            den === 1 ? center(w, c, show(num)) : (() => {
                const [q, r] = quotRem(w - 1, 2);
                return concat([
                    justifyRight(q, c, show(num)),
                    '/',
                    justifyLeft(q + r, c, (show(den)))
                ]);
            })()
        ) : "JS integer overflow ... ";
    };

    // Ratio :: Int -> Int -> Ratio
    const Ratio = (n, d) => ({
        num: n,
        den: d
    });

    // ratioMinus :: Ratio -> Ratio -> Ratio
    const ratioMinus = (nd, nd1) => {
        const
            d = lcm(nd.den, nd1.den);
        return simpleRatio({
            num: (nd.num * (d / nd.den)) - (nd1.num * (d / nd1.den)),
            den: d
        });
    };

    // ratioMult :: Ratio -> Ratio -> Ratio
    const ratioMult = (nd, nd1) => simpleRatio({
        num: nd.num * nd1.num,
        den: nd.den * nd1.den
    });

    // ratioPlus :: Ratio -> Ratio -> Ratio
    const ratioPlus = (nd, nd1) => {
        const
            d = lcm(nd.den, nd1.den);
        return simpleRatio({
            num: (nd.num * (d / nd.den)) + (nd1.num * (d / nd1.den)),
            den: d
        });
    };

    // ratioSum :: [Ratio] -> Ratio
    const ratioSum = xs =>
        simpleRatio(foldl((a, x) => ratioPlus(a, x), {
            num: 0,
            den: 1
        }, xs));

    // ratioWidths :: [[Ratio]] -> {nMax::Int, dMax::Int}
    const ratioWidths = xss => {
        return foldl((a, x) => {
            const [nw, dw] = ap(
                [compose(length, show)], [x.num, x.den]
            ), [an, ad] = ap(
                [curry(flip(lookup))(a)], ['nMax', 'dMax']
            );
            return {
                nMax: nw > an ? nw : an,
                dMax: dw > ad ? dw : ad
            };
        }, {
            nMax: 0,
            dMax: 0
        }, concat(xss));
    };

    // simpleRatio :: Ratio -> Ratio
    const simpleRatio = nd => {
        const g = gcd(nd.num, nd.den);
        return {
            num: nd.num / g,
            den: nd.den / g
        };
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // all :: (a -> Bool) -> [a] -> Bool
    const all = (f, xs) => xs.every(f);

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // Size of space -> filler Char -> Text -> Centered Text
    // center :: Int -> Char -> Text -> Text
    const center = (n, c, s) => {
        const [q, r] = quotRem(n - s.length, 2);
        return concat(concat([replicate(q, c), s, replicate(q + r, c)]));
    };

    // compose :: (b -> c) -> (a -> b) -> (a -> c)
    const compose = (f, g) => x => f(g(x));

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat(Array.from(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // gcd :: Integral a => a -> a -> a
    const gcd = (x, y) => {
        const _gcd = (a, b) => (b === 0 ? a : _gcd(b, a % b)),
            abs = Math.abs;
        return _gcd(abs(x), abs(y));
    };

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // justifyLeft :: Int -> Char -> Text -> Text
    const justifyLeft = (n, cFiller, strText) =>
        n > strText.length ? (
            (strText + cFiller.repeat(n))
            .substr(0, n)
        ) : strText;

    // justifyRight :: Int -> Char -> Text -> Text
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // last :: [a] -> a
    const last = xs => xs.length ? xs.slice(-1)[0] : undefined;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // lcm :: Integral a => a -> a -> a
    const lcm = (x, y) =>
        (x === 0 || y === 0) ? 0 : Math.abs(Math.floor(x / gcd(x, y)) * y);

    // lookup :: Eq a => a -> [(a, b)] -> Maybe b
    const lookup = (k, pairs) => {
        if (Array.isArray(pairs)) {
            let m = pairs.find(x => x[0] === k);
            return m ? m[1] : undefined;
        } else {
            return typeof pairs === 'object' ? (
                pairs[k]
            ) : undefined;
        }
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // max :: Ord a => a -> a -> a
    const max = (a, b) => b > a ? b : a;

    // min :: Ord a => a -> a -> a
    const min = (a, b) => b < a ? b : a;

    // quotRem :: Integral a => a -> a -> (a, a)
    const quotRem = (m, n) => [Math.floor(m / n), m % n];

    // raise :: Num -> Int -> Num
    const raise = (n, e) => Math.pow(n, e);

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // scanl :: (b -> a -> b) -> b -> [a] -> [b]
    const scanl = (f, startValue, xs) =>
        xs.reduce((a, x) => {
            const v = f(a.acc, x);
            return {
                acc: v,
                scan: cons(a.scan, v)
            };
        }, {
            acc: startValue,
            scan: [startValue]
        })
        .scan;

    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[0], null, x[1]] : x
        );

    // tail :: [a] -> [a]
    const tail = xs => xs.length ? xs.slice(1) : undefined;

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');


    // TEST -------------------------------------------------------------------
    const
        triangle = faulhaberTriangle(9),
        widths = ratioWidths(triangle);

    return unlines(
            map(row =>
                concat(map(cell =>
                    justifyRatio(widths, 8, ' ', cell), row)), triangle)
        ) +
        '\n\n' + unlines(
            [
                'faulhaber(17, 1000)',
                justifyRatio(widths, 0, ' ', faulhaber(17, 1000)),
                '\nfaulhaber(17, 8)',
                justifyRatio(widths, 0, ' ', faulhaber(17, 8)),
                '\nfaulhaber(4, 1000)',
                justifyRatio(widths, 0, ' ', faulhaber(4, 1000)),
            ]
        );
})();
```

```txt
   1
  1/2     1/2
  1/6     1/2     1/3
   0      1/4     1/2     1/4
 -1/30     0      1/3     1/2     1/5
   0     -1/12     0      5/12    1/2     1/6
  1/42     0     -1/6      0      1/2     1/2     1/7
   0      1/12     0     -7/24     0      7/12    1/2     1/8
 -1/30     0      2/9      0     -7/15     0      2/3     1/2     1/9
   0     -3/20     0      1/2      0     -7/10     0      3/4     1/2     1/10

faulhaber(17, 1000)
JS integer overflow ...

faulhaber(17, 8)
2502137235710736

faulhaber(4, 1000)
200500333333300
```



## Julia


```julia
function bernoulli(n)
    A = Vector{Rational{BigInt}}(undef, n + 1)
    for i in 0:n
        A[i + 1] = 1 // (i + 1)
        for j = i:-1:1
            A[j] = j * (A[j] - A[j + 1])
        end
    end
    return n == 1 ? -A[1] : A[1]
end

function faulhabercoeffs(p)
    coeffs = Vector{Rational{BigInt}}(undef, p + 1)
    q = Rational{BigInt}(1, p + 1)
    sign = -1
    for j in 0:p
        sign *= -1
        coeffs[p - j + 1] = bernoulli(j) * (q * sign) * Rational{BigInt}(binomial(p + 1, j), 1)
    end
    coeffs
end

faulhabersum(n, k) = begin coe = faulhabercoeffs(k); mapreduce(i -> BigInt(n)^i * coe[i], +, 1:k+1) end

prettyfrac(x) = (x.num == 0 ? "0" : x.den == 1 ? string(x.num) : replace(string(x), "//" => "/"))

function testfaulhaber()
    for i in 0:9
        for c in faulhabercoeffs(i)
            print(prettyfrac(c), "\t")
        end
        println()
    end
    println("\n", prettyfrac(faulhabersum(1000, 17)))
end

testfaulhaber()

```
```txt

1
1/2     1/2
1/6     1/2     1/3
0       1/4     1/2     1/4
-1/30   0       1/3     1/2     1/5
0       -1/12   0       5/12    1/2     1/6
1/42    0       -1/6    0       1/2     1/2     1/7
0       1/12    0       -7/24   0       7/12    1/2     1/8
-1/30   0       2/9     0       -7/15   0       2/3     1/2     1/9
0       -3/20   0       1/2     0       -7/10   0       3/4     1/2     1/10

56056972216555580111030077961944183400198333273050000

```




## Kotlin

Uses appropriately modified code from the Faulhaber's Formula task:

```scala
// version 1.1.2

import java.math.BigDecimal
import java.math.MathContext

val mc = MathContext(256)

fun gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

class Frac : Comparable<Frac> {
    val num: Long
    val denom: Long

    companion object {
        val ZERO = Frac(0, 1)
        val ONE  = Frac(1, 1)
    }

    constructor(n: Long, d: Long) {
        require(d != 0L)
        var nn = n
        var dd = d
        if (nn == 0L) {
            dd = 1
        }
        else if (dd < 0) {
            nn = -nn
            dd = -dd
        }
        val g = Math.abs(gcd(nn, dd))
        if (g > 1) {
            nn /= g
            dd /= g
        }
        num = nn
        denom = dd
    }

    constructor(n: Int, d: Int) : this(n.toLong(), d.toLong())

    operator fun plus(other: Frac) =
        Frac(num * other.denom + denom * other.num, other.denom * denom)

    operator fun unaryMinus() = Frac(-num, denom)

    operator fun minus(other: Frac) = this + (-other)

    operator fun times(other: Frac) = Frac(this.num * other.num, this.denom * other.denom)

    fun abs() = if (num >= 0) this else -this

    override fun compareTo(other: Frac): Int {
        val diff = this.toDouble() - other.toDouble()
        return when {
            diff < 0.0  -> -1
            diff > 0.0  -> +1
            else        ->  0
        }
    }

    override fun equals(other: Any?): Boolean {
       if (other == null || other !is Frac) return false
       return this.compareTo(other) == 0
    }

    override fun toString() = if (denom == 1L) "$num" else "$num/$denom"

    fun toDouble() = num.toDouble() / denom

    fun toBigDecimal() = BigDecimal(num).divide(BigDecimal(denom), mc)
}

fun bernoulli(n: Int): Frac {
    require(n >= 0)
    val a = Array(n + 1) { Frac.ZERO }
    for (m in 0..n) {
        a[m] = Frac(1, m + 1)
        for (j in m downTo 1) a[j - 1] = (a[j - 1] - a[j]) * Frac(j, 1)
    }
    return if (n != 1) a[0] else -a[0] // returns 'first' Bernoulli number
}

fun binomial(n: Int, k: Int): Long {
    require(n >= 0 && k >= 0 && n >= k)
    if (n == 0 || k == 0) return 1
    val num = (k + 1..n).fold(1L) { acc, i -> acc * i }
    val den = (2..n - k).fold(1L) { acc, i -> acc * i }
    return num / den
}

fun faulhaberTriangle(p: Int): Array<Frac> {
    val coeffs = Array(p + 1) { Frac.ZERO }
    val q = Frac(1, p + 1)
    var sign = -1
    for (j in 0..p) {
        sign *= -1
        coeffs[p - j] = q * Frac(sign, 1) * Frac(binomial(p + 1, j), 1) * bernoulli(j)
    }
    return coeffs
}

fun main(args: Array<String>) {
    for (i in 0..9){
        val coeffs = faulhaberTriangle(i)
        for (coeff in coeffs) print("${coeff.toString().padStart(5)}  ")
        println()
    }
    println()
    // get coeffs for (k + 1)th row
    val k = 17
    val cc = faulhaberTriangle(k)
    val n = 1000
    val nn  = BigDecimal(n)
    var np  = BigDecimal.ONE
    var sum = BigDecimal.ZERO
    for (c in cc) {
        np *= nn
        sum += np * c.toBigDecimal()
    }
    println(sum.toBigInteger())
}
```


```txt

    1
  1/2    1/2
  1/6    1/2    1/3
    0    1/4    1/2    1/4
-1/30      0    1/3    1/2    1/5
    0  -1/12      0   5/12    1/2    1/6
 1/42      0   -1/6      0    1/2    1/2    1/7
    0   1/12      0  -7/24      0   7/12    1/2    1/8
-1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
    0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10

56056972216555580111030077961944183400198333273050000

```



## Lua

```lua
function binomial(n,k)
    if n<0 or k<0 or n<k then return -1 end
    if n==0 or k==0 then return 1 end

    local num = 1
    for i=k+1,n do
        num = num * i
    end

    local denom = 1
    for i=2,n-k do
        denom = denom * i
    end

    return num / denom
end

function gcd(a,b)
    while b ~= 0 do
        local temp = a % b
        a = b
        b = temp
    end
    return a
end

function makeFrac(n,d)
    local result = {}

    if d==0 then
        result.num = 0
        result.denom = 0
        return result
    end

    if n==0 then
        d = 1
    elseif d < 0 then
        n = -n
        d = -d
    end

    local g = math.abs(gcd(n, d))
    if g>1 then
        n = n / g
        d = d / g
    end

    result.num = n
    result.denom = d
    return result
end

function negateFrac(f)
    return makeFrac(-f.num, f.denom)
end

function subFrac(lhs, rhs)
    return makeFrac(lhs.num * rhs.denom - lhs.denom * rhs.num, rhs.denom * lhs.denom)
end

function multFrac(lhs, rhs)
    return makeFrac(lhs.num * rhs.num, lhs.denom * rhs.denom)
end

function equalFrac(lhs, rhs)
    return (lhs.num == rhs.num) and (lhs.denom == rhs.denom)
end

function lessFrac(lhs, rhs)
    return (lhs.num * rhs.denom) < (rhs.num * lhs.denom)
end

function printFrac(f)
    local str = tostring(f.num)
    if f.denom ~= 1 then
        str = str.."/"..f.denom
    end
    for i=1, 7 - string.len(str) do
        io.write(" ")
    end
    io.write(str)
    return nil
end

function bernoulli(n)
    if n<0 then
        return {num=0, denom=0}
    end

    local a = {}
    for m=0,n do
        a[m] = makeFrac(1, m+1)
        for j=m,1,-1 do
            a[j-1] = multFrac(subFrac(a[j-1], a[j]), makeFrac(j, 1))
        end
    end

    if n~=1 then
        return a[0]
    end
    return negateFrac(a[0])
end

function faulhaber(p)
    local q = makeFrac(1, p+1)
    local sign = -1
    local coeffs = {}
    for j=0,p do
        sign = -1 * sign
        coeffs[p-j] = multFrac(multFrac(multFrac(q, makeFrac(sign, 1)), makeFrac(binomial(p + 1, j), 1)), bernoulli(j))
    end
    for j=0,p do
        printFrac(coeffs[j])
    end
    print()
    return nil
end

-- main
for i=0,9 do
    faulhaber(i)
end
```

```txt
      1
    1/2    1/2
    1/6    1/2    1/3
     -0    1/4    1/2    1/4
  -1/30     -0    1/3    1/2    1/5
     -0  -1/12     -0   5/12    1/2    1/6
   1/42     -0   -1/6     -0    1/2    1/2    1/7
     -0   1/12     -0  -7/24     -0   7/12    1/2    1/8
  -1/30     -0    2/9     -0  -7/15     -0    2/3    1/2    1/9
     -0  -3/20     -0    1/2     -0  -7/10     -0    3/4    1/2   1/10
```



## Perl

```perl
use 5.010;
use List::Util qw(sum);
use Math::BigRat try => 'GMP';
use ntheory qw(binomial bernfrac);

sub faulhaber_triangle {
    my ($p) = @_;
    map {
        Math::BigRat->new(bernfrac($_))
          * binomial($p, $_)
          / $p
    } reverse(0 .. $p-1);
}

# First 10 rows of Faulhaber's triangle
foreach my $p (1 .. 10) {
    say map { sprintf("%6s", $_) } faulhaber_triangle($p);
}

# Extra credit
my $p = 17;
my $n = Math::BigInt->new(1000);
my @r = faulhaber_triangle($p+1);
say "\n", sum(map { $r[$_] * $n**($_ + 1) } 0 .. $#r);
```

```txt

     1
   1/2   1/2
   1/6   1/2   1/3
     0   1/4   1/2   1/4
 -1/30     0   1/3   1/2   1/5
     0 -1/12     0  5/12   1/2   1/6
  1/42     0  -1/6     0   1/2   1/2   1/7
     0  1/12     0 -7/24     0  7/12   1/2   1/8
 -1/30     0   2/9     0 -7/15     0   2/3   1/2   1/9
     0 -3/20     0   1/2     0 -7/10     0   3/4   1/2  1/10

56056972216555580111030077961944183400198333273050000

```



## Perl 6

```perl6
# Helper subs

sub infix:<reduce> (\prev, \this) { this.key => this.key * (this.value - prev.value) }

sub next-bernoulli ( (:key($pm), :value(@pa)) ) {
    $pm + 1 => [ map *.value, [\reduce] ($pm + 2 ... 1) Z=> 1 / ($pm + 2), |@pa ]
}

constant bernoulli = (0 => [1.FatRat], &next-bernoulli ... *).map: { .value[*-1] };

sub binomial (Int $n, Int $p) { combinations($n, $p).elems }

sub asRat (FatRat $r) { $r ?? $r.denominator == 1 ?? $r.numerator !! $r.nude.join('/') !! 0 }


# The task
sub faulhaber_triangle ($p) { map { binomial($p + 1, $_) * bernoulli[$_] / ($p + 1) }, ($p ... 0) }

# First 10 rows of Faulhaber's triangle:
say faulhaber_triangle($_)».&asRat.fmt('%5s') for ^10;
say '';

# Extra credit:
my $p = 17;
my $n = 1000;
say sum faulhaber_triangle($p).kv.map: { $^value * $n**($^key + 1) }
```

```txt
    1
  1/2   1/2
  1/6   1/2   1/3
    0   1/4   1/2   1/4
-1/30     0   1/3   1/2   1/5
    0 -1/12     0  5/12   1/2   1/6
 1/42     0  -1/6     0   1/2   1/2   1/7
    0  1/12     0 -7/24     0  7/12   1/2   1/8
-1/30     0   2/9     0 -7/15     0   2/3   1/2   1/9
    0 -3/20     0   1/2     0 -7/10     0   3/4   1/2  1/10

56056972216555580111030077961944183400198333273050000
```



## Phix

```Phix
include builtins\pfrac.e -- (0.8.0+)

function bernoulli(integer n)
    sequence a = {}
    for m=0 to n do
        a = append(a,{1,m+1})
        for j=m to 1 by -1 do
            a[j] = frac_mul({j,1},frac_sub(a[j+1],a[j]))
        end for
    end for
    if n!=1 then return a[1] end if
    return frac_uminus(a[1])
end function

function binomial(integer n, k)
    if n<0 or k<0 or n<k then ?9/0 end if
    if n=0 or k=0 then return 1 end if
    atom num = 1,
         denom = 1
    for i=k+1 to n do
        num *= i
    end for
    for i=2 to n-k do
        denom *= i
    end for
    return num / denom
end function

function faulhaber_triangle(integer p, bool asString=true)
    sequence coeffs = repeat(frac_zero,p+1)
    for j=0 to p do
        frac coeff = frac_mul({binomial(p+1,j),p+1},bernoulli(j))
        coeffs[p-j+1] = iff(asString?sprintf("%5s",{frac_sprint(coeff)}):coeff)
    end for
    return coeffs
end function

for i=0 to 9 do
    printf(1,"%s\n",{join(faulhaber_triangle(i),"  ")})
end for
puts(1,"\n")

sequence row18 = faulhaber_triangle(17,false)
frac res = frac_zero
atom t1 = time()+1
integer lim = 1000
for k=1 to lim do
    bigatom nn = BA_ONE
    for i=1 to length(row18) do
        res = frac_add(res,frac_mul(row18[i],{nn,1}))
        nn = ba_mul(nn,lim)
    end for
    if time()>t1 then printf(1,"calculating, k=%d...\r",k) t1 = time()+1 end if
end for
printf(1,"%s        \n",{frac_sprint(res)})
```

```txt

    1
  1/2    1/2
  1/6    1/2    1/3
    0    1/4    1/2    1/4
-1/30      0    1/3    1/2    1/5
    0  -1/12      0   5/12    1/2    1/6
 1/42      0   -1/6      0    1/2    1/2    1/7
    0   1/12      0  -7/24      0   7/12    1/2    1/8
-1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
    0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10

56056972216555580111030077961944183400198333273050000

```



## Python

```python
'''Faulhaber's triangle'''

from itertools import (accumulate, chain, count, islice, starmap)
from fractions import (Fraction)


# faulhaberTriangle :: Int -> [[Fraction]]
def faulhaberTriangle(m):
    '''List of rows of Faulhaber fractions.'''
    def go(rs, n):
        xs = list(starmap(
            lambda x, y: Fraction(n, x) * y,
            zip(islice(count(2), m), rs)
        ))
        return [Fraction(1 - sum(xs), 1)] + xs
    return list(accumulate(
        [[]] + list(islice(count(0), 1 + m)),
        go
    ))[1:]


# faulhaberSum :: Integer -> Integer -> Integer
def faulhaberSum(p, n):
    '''Sum of the p-th powers of the first n
       positive integers.
    '''
    return sum(
        list(starmap(
            lambda x, y: y * (n ** x),
            zip(count(1), faulhaberTriangle(p)[-1])
        ))
    )


# TEST ----------------------------------------------------
def main():
    '''Tests'''

    fs = faulhaberTriangle(9)
    print(
        fTable(__doc__ + ':\n')(str)(
            compose(concat)(
                fmap(showRatio(3)(3))
            )
        )(
            index(fs)
        )(range(0, len(fs)))
    )
    print('')
    print(
        faulhaberSum(17, 1000)
    )


# DISPLAY -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xs):
    '''The concatenation of all the elements
       in a list or iterable.'''
    def f(ys):
        zs = list(chain(*ys))
        return ''.join(zs) if isinstance(ys[0], str) else zs

    return (
        f(xs) if isinstance(xs, list) else (
            chain.from_iterable(xs)
        )
    ) if xs else []


# fmap :: (a -> b) -> [a] -> [b]
def fmap(f):
    '''fmap over a list.
       f lifted to a function over a list.
    '''
    return lambda xs: list(map(f, xs))


# index (!!) :: [a] -> Int -> a
def index(xs):
    '''Item at given (zero-based) index.'''
    return lambda n: None if 0 > n else (
        xs[n] if (
            hasattr(xs, "__getitem__")
        ) else next(islice(xs, n, None))
    )


# showRatio :: Int -> Int -> Ratio -> String
def showRatio(m):
    '''Left and right aligned string
       representation of the ratio r.
    '''
    def go(n, r):
        d = r.denominator
        return str(r.numerator).rjust(m, ' ') + (
            ('/' + str(d).ljust(n, ' ')) if 1 != d else (
                ' ' * (1 + n)
            )
        )
    return lambda n: lambda r: go(n, r)


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Faulhaber's triangle:

0 ->    1
1 ->    1/2    1/2
2 ->    1/6    1/2    1/3
3 ->    0      1/4    1/2    1/4
4 ->   -1/30   0      1/3    1/2    1/5
5 ->    0     -1/12   0      5/12   1/2    1/6
6 ->    1/42   0     -1/6    0      1/2    1/2    1/7
7 ->    0      1/12   0     -7/24   0      7/12   1/2    1/8
8 ->   -1/30   0      2/9    0     -7/15   0      2/3    1/2    1/9
9 ->    0     -3/20   0      1/2    0     -7/10   0      3/4    1/2    1/10

56056972216555580111030077961944183400198333273050000
```



## Racket



```racket
#lang racket
(require math/number-theory)

(define (second-bernoulli-number n)
  (if (= n 1) 1/2 (bernoulli-number n)))

(define (faulhaber-row:formulaic p)
  (let ((p+1 (+ p 1)))
    (reverse
     (for/list ((j (in-range p+1)))
       (* (/ p+1) (second-bernoulli-number j) (binomial p+1 j))))))

(define (sum-k^p:formulaic p n)
  (for/sum ((f (faulhaber-row:formulaic p)) (i (in-naturals 1)))
    (* f (expt n i))))

(module+ main
  (map faulhaber-row:formulaic (range 10))
  (sum-k^p:formulaic 17 1000))

(module+ test
  (require rackunit)
  (check-equal? (sum-k^p:formulaic 17 1000)
                (for/sum ((k (in-range 1 (add1 1000)))) (expt k 17))))
```

```txt
'((1) (1/2 1/2) (1/6 1/2 1/3) (0 1/4 1/2 1/4) (-1/30 0 1/3 1/2 1/5) (0 -1/12 0 5/12 1/2 1/6) (1/42 0 -1/6 0 1/2 1/2 1/7) (0 1/12 0 -7/24 0 7/12 1/2 1/8) (-1/30 0 2/9 0 -7/15 0 2/3 1/2 1/9) (0 -3/20 0 1/2 0 -7/10 0 3/4 1/2 1/10))
56056972216555580111030077961944183400198333273050000
```



## REXX


```rexx
Numeric Digits 100
Do r=0 To 20
  ra=r-1
  If r=0 Then
    f.r.1=1
  Else Do
    rsum=0
    Do c=2 To r+1
      ca=c-1
      f.r.c=fdivide(fmultiply(f.ra.ca,r),c)
      rsum=fsum(rsum,f.r.c)
      End
    f.r.1=fsubtract(1,rsum)
    End
  End
Do r=0 To 9
  ol=''
  Do c=1 To r+1
    ol=ol right(f.r.c,5)
    End
  Say ol
  End
Say ''
x=0
Do c=1 To 18
  x=fsum(x,fmultiply(f.17.c,(1000**c)))
  End
Say k(x)
s=0
Do k=1 To 1000
  s=s+k**17
  End
Say s
Exit

fmultiply: Procedure
Parse Arg a,b
Parse Var a ad '/' an
Parse Var b bd '/' bn
If an='' Then an=1
If bn='' Then bn=1
res=(abs(ad)*abs(bd))'/'||(an*bn)
Return s(ad,bd)k(res)

fdivide: Procedure
Parse Arg a,b
Parse Var a ad '/' an
Parse Var b bd '/' bn
If an='' Then an=1
If bn='' Then bn=1
res=s(ad,bd)(abs(ad)*bn)'/'||(an*abs(bd))
Return k(res)

fsum: Procedure
Parse Arg a,b
Parse Var a ad '/' an
Parse Var b bd '/' bn
If an='' Then an=1
If bn='' Then bn=1
n=an*bn
d=ad*bn+bd*an
res=d'/'n
Return k(res)

fsubtract: Procedure
Parse Arg a,b
Parse Var a ad '/' an
Parse Var b bd '/' bn
If an='' Then an=1
If bn='' Then bn=1
n=an*bn
d=ad*bn-bd*an
res=d'/'n
Return k(res)

s: Procedure
Parse Arg ad,bd
s=sign(ad)*sign(bd)
If s<0 Then Return '-'
       Else Return ''

k: Procedure
Parse Arg a
Parse Var a ad '/' an
Select
  When ad=0 Then Return 0
  When an=1 Then Return ad
  Otherwise Do
    g=gcd(ad,an)
    ad=ad/g
    an=an/g
    Return ad'/'an
    End
  End

gcd: procedure
Parse Arg a,b
if b = 0 then return abs(a)
return gcd(b,a//b)
```

```txt
     1
   1/2   1/2
   1/6   1/2   1/3
     0   1/4   1/2   1/4
 -1/30     0   1/3   1/2   1/5
     0 -1/12     0  5/12   1/2   1/6
  1/42     0  -1/6     0   1/2   1/2   1/7
     0  1/12     0 -7/24     0  7/12   1/2   1/8
 -1/30     0   2/9     0 -7/15     0   2/3   1/2   1/9
     0 -3/20     0   1/2     0 -7/10     0   3/4   1/2  1/10

56056972216555580111030077961944183400198333273050000
56056972216555580111030077961944183400198333273050000
```



## Sidef


```ruby
func faulhaber_triangle(p) {
    { binomial(p, _) * bernoulli(_) / p }.map(p ^.. 0)
}

{ |p|
    say faulhaber_triangle(p).map{ '%6s' % .as_rat }.join
} << 1..10

const p = 17
const n = 1000

say ''
say faulhaber_triangle(p+1).map_kv {|k,v| v * n**(k+1) }.sum
```

```txt

     1
   1/2   1/2
   1/6   1/2   1/3
     0   1/4   1/2   1/4
 -1/30     0   1/3   1/2   1/5
     0 -1/12     0  5/12   1/2   1/6
  1/42     0  -1/6     0   1/2   1/2   1/7
     0  1/12     0 -7/24     0  7/12   1/2   1/8
 -1/30     0   2/9     0 -7/15     0   2/3   1/2   1/9
     0 -3/20     0   1/2     0 -7/10     0   3/4   1/2  1/10

56056972216555580111030077961944183400198333273050000

```


Alternative solution:

```ruby
func find_poly_degree(a) {
    var c = 0
    loop {
        ++c
        a = a.map_cons(2, {|n,k| n-k })
        return 0 if a.is_empty
        return c if a.all { .is_zero }
    }
}

func faulhaber_triangle(n) {
    var a = (0..(n+2) -> accumulate { _**n })
    var c = find_poly_degree(a)

    var A = c.of {|n|
        c.of {|k| n**k }
    }

    A.msolve(a).slice(1)
}

10.times { say faulhaber_triangle(_) }
```

```txt

[1]
[1/2, 1/2]
[1/6, 1/2, 1/3]
[0, 1/4, 1/2, 1/4]
[-1/30, 0, 1/3, 1/2, 1/5]
[0, -1/12, 0, 5/12, 1/2, 1/6]
[1/42, 0, -1/6, 0, 1/2, 1/2, 1/7]
[0, 1/12, 0, -7/24, 0, 7/12, 1/2, 1/8]
[-1/30, 0, 2/9, 0, -7/15, 0, 2/3, 1/2, 1/9]
[0, -3/20, 0, 1/2, 0, -7/10, 0, 3/4, 1/2, 1/10]

```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
Uses the code from [[Faulhaber's formula#zkl]]

```zkl
foreach p in (10){
   faulhaberFormula(p).apply("%7s".fmt).concat().println();
}

// each term of faulhaberFormula is BigInt/BigInt
[1..].zipWith(fcn(n,rat){ rat*BN(1000).pow(n) }, faulhaberFormula(17))
.walk()		// -->(0, -3617/60 * 1000^2, 0, 595/3 * 1000^4 ...)
.reduce('+)	// rat + rat + ...
.println();
```

```txt

      1
    1/2    1/2
    1/6    1/2    1/3
      0    1/4    1/2    1/4
  -1/30      0    1/3    1/2    1/5
      0  -1/12      0   5/12    1/2    1/6
   1/42      0   -1/6      0    1/2    1/2    1/7
      0   1/12      0  -7/24      0   7/12    1/2    1/8
  -1/30      0    2/9      0  -7/15      0    2/3    1/2    1/9
      0  -3/20      0    1/2      0  -7/10      0    3/4    1/2   1/10
56056972216555580111030077961944183400198333273050000

```


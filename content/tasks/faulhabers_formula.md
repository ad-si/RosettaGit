+++
title = "Faulhaber's formula"
description = ""
date = 2019-10-09T17:20:41Z
aliases = []
[extra]
id = 19860
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "factor",
  "gap",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "lua",
  "maxima",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "racket",
  "related_tasks",
  "sidef",
  "visual_basic_.net",
  "zkl",
]
+++

In mathematics,   Faulhaber's formula,   named after Johann Faulhaber,   expresses the sum of the ''p''-th powers of the first ''n'' positive integers as a ''(p + 1)''th-degree polynomial function of n,   the coefficients involving [[Bernoulli numbers]].


## Task

Generate the first 10 closed-form expressions, starting with ''p = 0''.


## Related tasks

:*   [[Bernoulli numbers]].
:*   [[Evaluate_binomial_coefficients|evaluate binomial coefficients]].


## See also

:*   The Wikipedia entry:   [[wp:Faulhaber's formula|Faulhaber's formula]].
:*   The Wikipedia entry:   [https://en.wikipedia.org/wiki/Bernoulli_number Bernoulli numbers].
:*   The Wikipedia entry:   [https://en.wikipedia.org/wiki/Binomial_coefficient binomial coefficients].





## C

```c
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

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
    printf("%d", f.num);
    if (f.denom != 1) {
        printf("/%d", f.denom);
    }
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
    Frac coeff, q;
    int j, pwr, sign;

    printf("%d : ", p);
    q = makeFrac(1, p + 1);
    sign = -1;
    for (j = 0; j <= p; ++j) {
        sign = -1 * sign;
        coeff = multFrac(multFrac(multFrac(q, makeFrac(sign, 1)), makeFrac(binomial(p + 1, j), 1)), bernoulli(j));
        if (equalFrac(coeff, makeFrac(0, 1))) {
            continue;
        }
        if (j == 0) {
            if (!equalFrac(coeff, makeFrac(1, 1))) {
                if (equalFrac(coeff, makeFrac(-1, 1))) {
                    printf("-");
                } else {
                    printFrac(coeff);
                }
            }
        } else {
            if (equalFrac(coeff, makeFrac(1, 1))) {
                printf(" + ");
            } else if (equalFrac(coeff, makeFrac(-1, 1))) {
                printf(" - ");
            } else if (lessFrac(makeFrac(0, 1), coeff)) {
                printf(" + ");
                printFrac(coeff);
            } else {
                printf(" - ");
                printFrac(negateFrac(coeff));
            }
        }
        pwr = p + 1 - j;
        if (pwr > 1) {
            printf("n^%d", pwr);
        } else {
            printf("n");
        }
    }
    printf("\n");
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
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```



## C++

Uses C++17

```cpp
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

	bool operator==(const Frac& rhs) const {
		return num == rhs.num && denom == rhs.denom;
	}

	bool operator!=(const Frac& rhs) const {
		return num != rhs.num || denom != rhs.denom;
	}

	bool operator<(const Frac& rhs) const {
		if (denom == rhs.denom) {
			return num < rhs.num;
		}
		return num * rhs.denom < rhs.num * denom;
	}

	friend std::ostream& operator<<(std::ostream&, const Frac&);

	static Frac ZERO() {
		return Frac(0, 1);
	}

	static Frac ONE() {
		return Frac(1, 1);
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

void faulhaber(int p) {
	using namespace std;
	cout << p << " : ";

	auto q = Frac(1, p + 1);
	int sign = -1;
	for (int j = 0; j < p + 1; j++) {
		sign *= -1;
		auto coeff = q * Frac(sign, 1) * Frac(binomial(p + 1, j), 1) * bernoulli(j);
		if (coeff == Frac::ZERO()) {
			continue;
		}
		if (j == 0) {
			if (coeff == -Frac::ONE()) {
				cout << "-";
			} else if (coeff != Frac::ONE()) {
				cout << coeff;
			}
		} else {
			if (coeff == Frac::ONE()) {
				cout << " + ";
			} else if (coeff == -Frac::ONE()) {
				cout << " - ";
			} else if (coeff < Frac::ZERO()) {
				cout << " - " << -coeff;
			} else {
				cout << " + " << coeff;
			}
		}
		int pwr = p + 1 - j;
		if (pwr > 1) {
			cout << "n^" << pwr;
		} else {
			cout << "n";
		}
	}
	cout << endl;
}

int main() {
	for (int i = 0; i < 10; i++) {
		faulhaber(i);
	}

	return 0;
}
```

```txt
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```


## C#
```c#
using System;

namespace FaulhabersFormula {
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

        static void Faulhaber(int p) {
            Console.Write("{0} : ", p);
            Frac q = new Frac(1, p + 1);
            int sign = -1;
            for (int j = 0; j <= p; j++) {
                sign *= -1;
                Frac coeff = q * new Frac(sign, 1) * new Frac(Binomial(p + 1, j), 1) * Bernoulli(j);
                if (Frac.ZERO == coeff) continue;
                if (j == 0) {
                    if (Frac.ONE != coeff) {
                        if (-Frac.ONE == coeff) {
                            Console.Write("-");
                        }
                        else {
                            Console.Write(coeff);
                        }
                    }
                }
                else {
                    if (Frac.ONE == coeff) {
                        Console.Write(" + ");
                    }
                    else if (-Frac.ONE == coeff) {
                        Console.Write(" - ");
                    }
                    else if (Frac.ZERO < coeff) {
                        Console.Write(" + {0}", coeff);
                    }
                    else {
                        Console.Write(" - {0}", -coeff);
                    }
                }
                int pwr = p + 1 - j;
                if (pwr > 1) {
                    Console.Write("n^{0}", pwr);
                }
                else {
                    Console.Write("n");
                }
            }
            Console.WriteLine();
        }

        static void Main(string[] args) {
            for (int i = 0; i < 10; i++) {
                Faulhaber(i);
            }
        }
    }
}
```

```txt
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```



## D

```D
import std.algorithm : fold;
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

auto faulhaber(int p) {
    write(p, " : ");
    auto q = Frac(1, p+1);
    auto sign = -1;
    foreach (j; 0..p+1) {
        sign *= -1;
        auto coeff = q * Frac(sign, 1) * Frac(binomial(p+1, j), 1) * bernoulli(j);
        if (coeff == Frac.ZERO) continue;
        if (j == 0) {
            if (coeff == -Frac.ONE) {
                write("-");
            } else if (coeff != Frac.ONE) {
                write(coeff);
            }
        } else {
            if (coeff == Frac.ONE) {
                write(" + ");
            } else if (coeff == -Frac.ONE) {
                write(" - ");
            } else if (coeff > Frac.ZERO) {
                write(" + ", coeff);
            } else {
                write(" - ", -coeff);
            }
        }
        auto pwr = p + 1 - j;
        if (pwr > 1) {
            write("n^", pwr);
        } else {
            write("n");
        }
    }
    writeln;
}

void main() {
    foreach (i; 0..10) {
        faulhaber(i);
    }
}
```

```txt
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```



## EchoLisp


```scheme

(lib 'math) ;; for bernoulli numbers
(string-delimiter "")

;; returns list of polynomial coefficients
(define (Faulhaber p)
	(cons 0
	(for/list ([k (in-range p -1 -1)])
		(* (Cnp (1+ p) k) (bernoulli k)))))

;; prints formal polynomial
(define (task (pmax 10))
    (for ((p pmax))
    (writeln p '→  (/ 1 (1+ p)) '* (poly->string 'n (Faulhaber p)))))

;; extra credit - compute sums
(define (Faulcomp n p)
	(printf "Σ(1..%d) n^%d = %d" n p (/  (poly n (Faulhaber p)) (1+ p) )))

```

```txt

(task)
0     →     1     *     n
1     →     1/2     *     n^2 + n
2     →     1/3     *     n^3 + 3/2 n^2 + 1/2 n
3     →     1/4     *     n^4 + 2 n^3 + n^2
4     →     1/5     *     n^5 + 5/2 n^4 + 5/3 n^3 -1/6 n
5     →     1/6     *     n^6 + 3 n^5 + 5/2 n^4 -1/2 n^2
6     →     1/7     *     n^7 + 7/2 n^6 + 7/2 n^5 -7/6 n^3 + 1/6 n
7     →     1/8     *     n^8 + 4 n^7 + 14/3 n^6 -7/3 n^4 + 2/3 n^2
8     →     1/9     *     n^9 + 9/2 n^8 + 6 n^7 -21/5 n^5 + 2 n^3 -3/10 n
9     →     1/10     *     n^10 + 5 n^9 + 15/2 n^8 -7 n^6 + 5 n^4 -3/2 n^2

(Faulcomp 100 2)
    Σ(1..100) n^2 = 338350
(Faulcomp 100 1)
    Σ(1..100) n^1 = 5050

(lib 'bigint)
(Faulcomp 100 9)
    Σ(1..100) n^9 = 10507499300049998000

;; check it ...
(for/sum ((n 101)) (expt n 9))
    → 10507499300049998500

```



## Factor


```factor
USING: formatting kernel math math.combinatorics math.extras
math.functions regexp sequences ;

: faulhaber ( p -- seq )
    1 + dup recip swap dup <iota>
    [ [ nCk ] [ -1 swap ^ ] [ bernoulli ] tri * * * ] 2with map ;

: (poly>str) ( seq -- str )
    reverse [ 1 + "%un^%d" sprintf ] map-index reverse " + " join ;

: clean-up ( str -- str' )
    R/ n\^1\z/ "n" re-replace            ! Change n^1 to n.
    R/ 1n/ "n" re-replace                ! Change 1n to n.
    R/ \+ -/ "- " re-replace             ! Change + - to - .
    R/ [+-] 0n(\^\d+ )?/ "" re-replace ; ! Remove terms of zero.

: poly>str ( seq -- str ) (poly>str) clean-up ;

10 [ dup faulhaber poly>str "%d: %s\n" printf ] each-integer
```

```txt

0: n
1: 1/2n^2 + 1/2n
2: 1/3n^3 + 1/2n^2 + 1/6n
3: 1/4n^4 + 1/2n^3 + 1/4n^2
4: 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5: 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6: 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7: 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8: 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9: 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Faulhaber this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## GAP

Straightforward implementation using GAP polynomials, and two different formulas: one based on Stirling numbers of the second kind (sum1, see Python implementation below in this page), and the usual Faulhaber formula (sum2). No optimization is made (one could compute Stirling numbers row by row, or the product in sum1 may be kept from one call to the other). Notice the Bernoulli term in the first formula is here only to correct the value of sum1(0), which is off by one because sum1 computes sums from 0 to n.


```gap
n := X(Rationals, "n");
sum1 := p -> Sum([0 .. p], k -> Stirling2(p, k) * Product([0 .. k], j -> n + 1 - j) / (k + 1)) + 2 * Bernoulli(2 * p + 1);
sum2 := p -> Sum([0 .. p], j -> (-1)^j * Binomial(p + 1, j) * Bernoulli(j) * n^(p + 1 - j)) / (p + 1);
ForAll([0 .. 20], k -> sum1(k) = sum2(k));

for p in [0 .. 9] do
    Print(sum1(p), "\n");
od;

n
1/2*n^2+1/2*n
1/3*n^3+1/2*n^2+1/6*n
1/4*n^4+1/2*n^3+1/4*n^2
1/5*n^5+1/2*n^4+1/3*n^3-1/30*n
1/6*n^6+1/2*n^5+5/12*n^4-1/12*n^2
1/7*n^7+1/2*n^6+1/2*n^5-1/6*n^3+1/42*n
1/8*n^8+1/2*n^7+7/12*n^6-7/24*n^4+1/12*n^2
1/9*n^9+1/2*n^8+2/3*n^7-7/15*n^5+2/9*n^3-1/30*n
1/10*n^10+1/2*n^9+3/4*n^8-7/10*n^6+1/2*n^4-3/20*n^2
```



## Go


```Go
package main

import (
	"fmt"
	"math/big"
)

func bernoulli(z *big.Rat, n int64) *big.Rat {
	if z == nil {
		z = new(big.Rat)
	}
	a := make([]big.Rat, n+1)
	for m := range a {
		a[m].SetFrac64(1, int64(m+1))
		for j := m; j >= 1; j-- {
			d := &a[j-1]
			d.Mul(z.SetInt64(int64(j)), d.Sub(d, &a[j]))
		}
	}
	return z.Set(&a[0])
}

func main() {
	// allocate needed big.Rat's once
	q := new(big.Rat)
	c := new(big.Rat)      // coefficients
	be := new(big.Rat)     // for Bernoulli numbers
	bi := big.NewRat(1, 1) // for binomials

	for p := int64(0); p < 10; p++ {
		fmt.Print(p, " : ")
		q.SetFrac64(1, p+1)
		neg := true
		for j := int64(0); j <= p; j++ {
			neg = !neg
			if neg {
				c.Neg(q)
			} else {
				c.Set(q)
			}
			bi.Num().Binomial(p+1, j)
			bernoulli(be, j)
			c.Mul(c, bi)
			c.Mul(c, be)
			if c.Num().BitLen() == 0 {
				continue
			}
			if j == 0 {
				fmt.Printf(" %4s", c.RatString())
			} else {
				fmt.Printf(" %+2d/%-2d", c.Num(), c.Denom())
			}
			fmt.Print("×n")
			if exp := p + 1 - j; exp > 1 {
				fmt.Printf("^%-2d", exp)
			}
		}
		fmt.Println()
	}
}
```

```txt

0 :     1×n
1 :   1/2×n^2  -1/2 ×n
2 :   1/3×n^3  -1/2 ×n^2  +1/6 ×n
3 :   1/4×n^4  -1/2 ×n^3  +1/4 ×n^2
4 :   1/5×n^5  -1/2 ×n^4  +1/3 ×n^3  -1/30×n
5 :   1/6×n^6  -1/2 ×n^5  +5/12×n^4  -1/12×n^2
6 :   1/7×n^7  -1/2 ×n^6  +1/2 ×n^5  -1/6 ×n^3  +1/42×n
7 :   1/8×n^8  -1/2 ×n^7  +7/12×n^6  -7/24×n^4  +1/12×n^2
8 :   1/9×n^9  -1/2 ×n^8  +2/3 ×n^7  -7/15×n^5  +2/9 ×n^3  -1/30×n
9 :  1/10×n^10 -1/2 ×n^9  +3/4 ×n^8  -7/10×n^6  +1/2 ×n^4  -3/20×n^2

```



## Haskell


### =Bernouilli polynomials=


```Haskell
import Data.Ratio ((%), numerator, denominator)
import Data.List (intercalate, transpose)
import Control.Arrow ((&&&), (***))
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Bool (bool)

-- FAULHABER ----------------------------------------------
faulhaber :: [[Rational]]
faulhaber =
  tail $
  scanl
    (\rs n ->
        let xs = zipWith ((*) . (n %)) [2 ..] rs
        in 1 - sum xs : xs)
    []
    [0 ..]

-- EXPRESSION STRINGS -------------------------------------
polynomials :: [[(String, String)]]
polynomials = fmap ((ratioPower =<<) . reverse . flip zip [1 ..]) faulhaber

-- Rows of (Power string, Ratio string) tuples -> Printable lines
expressionTable :: [[(String, String)]] -> [String]
expressionTable ps =
  let cols = transpose (fullTable ps)
  in expressionRow <$>
     zip
       [0 ..]
       (transpose $
        zipWith
          (\(lw, rw) col ->
              (fmap (justifyLeft lw ' ' *** justifyLeft rw ' ') col))
          (colWidths cols)
          cols)

-- Value pair -> String pair (lifted into list for use with >>=)
ratioPower :: (Rational, Integer) -> [(String, String)]
ratioPower (nd, j) =
  let (num, den) = (numerator &&& denominator) nd
      sn
        | num == 0 = []
        | (j /= 1) = ("n^" <> show j)
        | otherwise = "n"
      sr
        | num == 0 = []
        | den == 1 && num == 1 = []
        | den == 1 = show num <> "n"
        | otherwise = intercalate "/" [show num, show den]
      s = sr <> sn
  in bool [(sn, sr)] [] (null s)

-- Rows of uneven length -> All rows padded to length of longest
fullTable :: [[(String, String)]] -> [[(String, String)]]
fullTable xs =
  let lng = maximum $ length <$> xs
  in (<>) <*> (flip replicate ([], []) . (-) lng . length) <$> xs

justifyLeft :: Int -> Char -> String -> String
justifyLeft n c s = take n (s <> replicate n c)

-- (Row index, Expression pairs) -> String joined by conjunctions
expressionRow :: (Int, [(String, String)]) -> String
expressionRow (i, row) =
  concat
    [ show i
    , " ->  "
    , foldr
        (\s a -> concat [s, bool " + " " " (blank a || head a == '-'), a])
        []
        (polyTerm <$> row)
    ]

-- (Power string, Ratio String) -> Combined string with possible '*'
polyTerm :: (String, String) -> String
polyTerm (l, r)
  | blank l || blank r = l <> r
  | head r == '-' = concat ["- ", l, " * ", tail r]
  | otherwise = intercalate " * " [l, r]

blank :: String -> Bool
blank = all isSpace

-- Maximum widths of power and ratio elements in each column
colWidths :: [[(String, String)]] -> [(Int, Int)]
colWidths =
  fmap
    (foldr
       (\(ls, rs) (lMax, rMax) -> (max (length ls) lMax, max (length rs) rMax))
       (0, 0))

-- Length of string excluding any leading '-'
unsignedLength :: String -> Int
unsignedLength xs =
  let l = length xs
  in bool (bool l (l - 1) ('-' == head xs)) 0 (0 == l)

-- TEST ---------------------------------------------------
main :: IO ()
main = (putStrLn . unlines . expressionTable . take 10) polynomials
```

```txt
0 ->  n
1 ->  n^2  * 1/2  + n   * 1/2
2 ->  n^3  * 1/3  + n^2 * 1/2 + n   * 1/6
3 ->  n^4  * 1/4  + n^3 * 1/2 + n^2 * 1/4
4 ->  n^5  * 1/5  + n^4 * 1/2 + n^3 * 1/3  - n   * 1/30
5 ->  n^6  * 1/6  + n^5 * 1/2 + n^4 * 5/12 - n^2 * 1/12
6 ->  n^7  * 1/7  + n^6 * 1/2 + n^5 * 1/2  - n^3 * 1/6  + n   * 1/42
7 ->  n^8  * 1/8  + n^7 * 1/2 + n^6 * 7/12 - n^4 * 7/24 + n^2 * 1/12
8 ->  n^9  * 1/9  + n^8 * 1/2 + n^7 * 2/3  - n^5 * 7/15 + n^3 * 2/9  - n   * 1/30
9 ->  n^10 * 1/10 + n^9 * 1/2 + n^8 * 3/4  - n^6 * 7/10 + n^4 * 1/2  - n^2 * 3/20
```



## J


Implementation:


```J
Bsecond=:verb define"0
  +/,(<:*(_1^[)*!*(y^~1+[)%1+])"0/~i.1x+y
)

Bfirst=: Bsecond - 1&=

Faul=:adverb define
  (0,|.(%m+1x) * (_1x&^ * !&(m+1) * Bfirst) i.1+m)&p.
)
```


Task example:


```J
   0 Faul
0 1x&p.
   1 Faul
0 1r2 1r2&p.
   2 Faul
0 1r6 1r2 1r3&p.
   3 Faul
0 0 1r4 1r2 1r4&p.
   4 Faul
0 _1r30 0 1r3 1r2 1r5&p.
   5 Faul
0 0 _1r12 0 5r12 1r2 1r6&p.
   6 Faul
0 1r42 0 _1r6 0 1r2 1r2 1r7&p.
   7 Faul
0 0 1r12 0 _7r24 0 7r12 1r2 1r8&p.
   8 Faul
0 _1r30 0 2r9 0 _7r15 0 2r3 1r2 1r9&p.
   9 Faul
0 0 _3r20 0 1r2 0 _7r10 0 3r4 1r2 1r10&p.
```


Double checking our work:


```J
   Fcheck=: dyad def'+/(1+i.y)^x'"0
   9 Faul i.5
0 1 513 20196 282340
   9 Fcheck i.5
0 1 513 20196 282340
   2 Faul i.5
0 1 5 14 30
   2 Fcheck i.5
0 1 5 14 30
```



## Java

```Java
import java.util.Arrays;
import java.util.stream.IntStream;

public class FaulhabersFormula {
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
        public static final Frac ONE = new Frac(1, 1);

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

        private double toDouble() {
            return (double) num / denom;
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

    private static int binomial(int n, int k) {
        if (n < 0 || k < 0 || n < k) throw new IllegalArgumentException();
        if (n == 0 || k == 0) return 1;
        int num = IntStream.rangeClosed(k + 1, n).reduce(1, (a, b) -> a * b);
        int den = IntStream.rangeClosed(2, n - k).reduce(1, (acc, i) -> acc * i);
        return num / den;
    }

    private static void faulhaber(int p) {
        System.out.printf("%d : ", p);
        Frac q = new Frac(1, p + 1);
        int sign = -1;
        for (int j = 0; j <= p; ++j) {
            sign *= -1;
            Frac coeff = q.times(new Frac(sign, 1)).times(new Frac(binomial(p + 1, j), 1)).times(bernoulli(j));
            if (Frac.ZERO.equals(coeff)) continue;
            if (j == 0) {
                if (!Frac.ONE.equals(coeff)) {
                    if (Frac.ONE.unaryMinus().equals(coeff)) {
                        System.out.print("-");
                    } else {
                        System.out.print(coeff);
                    }
                }
            } else {
                if (Frac.ONE.equals(coeff)) {
                    System.out.print(" + ");
                } else if (Frac.ONE.unaryMinus().equals(coeff)) {
                    System.out.print(" - ");
                } else if (coeff.compareTo(Frac.ZERO) > 0) {
                    System.out.printf(" + %s", coeff);
                } else {
                    System.out.printf(" - %s", coeff.unaryMinus());
                }
            }
            int pwr = p + 1 - j;
            if (pwr > 1)
                System.out.printf("n^%d", pwr);
            else
                System.out.print("n");
        }
        System.out.println();
    }

    public static void main(String[] args) {
        for (int i = 0; i <= 9; ++i) {
            faulhaber(i);
        }
    }
}
```

```txt
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```



## Julia

'''Module''':

```julia
module Faulhaber

function bernoulli(n::Integer)
    n ≥ 0 || throw(DomainError(n, "n must be a positive-or-0 number"))
    a = fill(0 // 1, n + 1)
    for m in 1:n
        a[m] = 1 // (m + 1)
        for j in m:-1:2
            a[j - 1] = (a[j - 1] - a[j]) * j
        end
    end
    return ifelse(n != 1, a[1], -a[1])
end

const _exponents = collect(Char, "⁰¹²³⁴⁵⁶⁷⁸⁹")
toexponent(n) = join(_exponents[reverse(digits(n)) .+ 1])

function formula(p::Integer)
    print(p, ": ")
    q = 1 // (p + 1)
    s = -1
    for j in 0:p
        s *= -1
        coeff = q * s * binomial(p + 1, j) * bernoulli(j)
        iszero(coeff) && continue
        if iszero(j)
            print(coeff == 1 ? "" : coeff == -1 ? "-" : "$coeff")
        else
            print(coeff == 1 ? " + " : coeff == -1 ? " - " : coeff > 0 ? " + $coeff " : " - $(-coeff) ")
        end
        pwr = p + 1 - j
        if pwr > 1
            print("n", toexponent(pwr))
        else
            print("n")
        end
    end
    println()
end

end  # module Faulhaber
```


'''Main''':

```julia
Faulhaber.formula.(1:10)
```


```txt
1:  + 1//2 n
2:  + 1//2 n² + 1//3 n
3:  + 1//2 n³ + 1//2 n² - 1//6 n
4:  + 1//2 n⁴ + 2//3 n³ - 1//3 n² + 1//30 n
5:  + 1//2 n⁵ + 5//6 n⁴ - 5//9 n³ + 1//12 n² + 1//30 n
6:  + 1//2 n⁶ + n⁵ - 5//6 n⁴ + 1//6 n³ + 1//10 n² - 1//42 n
7:  + 1//2 n⁷ + 7//6 n⁶ - 7//6 n⁵ + 7//24 n⁴ + 7//30 n³ - 1//12 n² - 1//42 n
8:  + 1//2 n⁸ + 4//3 n⁷ - 14//9 n⁶ + 7//15 n⁵ + 7//15 n⁴ - 2//9 n³ - 2//21 n² + 1//30 n
9:  + 1//2 n⁹ + 3//2 n⁸ - 2//1 n⁷ + 7//10 n⁶ + 21//25 n⁵ - 1//2 n⁴ - 2//7 n³ + 3//20 n² + 1//30 n
10:  + 1//2 n¹⁰ + 5//3 n⁹ - 5//2 n⁸ + n⁷ + 7//5 n⁶ - n⁵ - 5//7 n⁴ + 1//2 n³ + 1//6 n² - 5//66 n
```



## Kotlin

As Kotlin doesn't have support for rational numbers built in, a cut-down version of the Frac class in the Arithmetic/Rational task has been used in order to express the polynomial coefficients as fractions.

```scala
// version 1.1.2

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
}

fun bernoulli(n: Int): Frac {
    require(n >= 0)
    val a = Array<Frac>(n + 1) { Frac.ZERO }
    for (m in 0..n) {
        a[m] = Frac(1, m + 1)
        for (j in m downTo 1) a[j - 1] = (a[j - 1] - a[j]) * Frac(j, 1)
    }
    return if (n != 1) a[0] else -a[0] // returns 'first' Bernoulli number
}

fun binomial(n: Int, k: Int): Int {
    require(n >= 0 && k >= 0 && n >= k)
    if (n == 0 || k == 0) return 1
    val num = (k + 1..n).fold(1) { acc, i -> acc * i }
    val den = (2..n - k).fold(1) { acc, i -> acc * i }
    return num / den
}

fun faulhaber(p: Int) {
    print("$p : ")
    val q = Frac(1, p + 1)
    var sign = -1
    for (j in 0..p) {
        sign *= -1
        val coeff = q * Frac(sign, 1) * Frac(binomial(p + 1, j), 1) * bernoulli(j)
        if (coeff == Frac.ZERO) continue
        if (j == 0) {
            print(when {
                coeff == Frac.ONE  -> ""
                coeff == -Frac.ONE -> "-"
                else               -> "$coeff"
            })
        }
        else {
            print(when {
                coeff == Frac.ONE  -> " + "
                coeff == -Frac.ONE -> " - "
                coeff >  Frac.ZERO -> " + $coeff"
                else               -> " - ${-coeff}"
            })
        }
        val pwr = p + 1 - j
        if (pwr > 1)
            print("n^${p + 1 - j}")
        else
            print("n")
    }
    println()
}


fun main(args: Array<String>) {
    for (i in 0..9) faulhaber(i)
}
```


```txt

0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2

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
    io.write(f.num)
    if f.denom ~= 1 then
        io.write("/"..f.denom)
    end
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
    io.write(p.." : ")
    local q = makeFrac(1, p+1)
    local sign = -1
    for j=0,p do
        sign = -1 * sign
        local coeff = multFrac(multFrac(multFrac(q, makeFrac(sign, 1)), makeFrac(binomial(p + 1, j), 1)), bernoulli(j))
        if not equalFrac(coeff, makeFrac(0, 1)) then
            if j==0 then
                if not equalFrac(coeff, makeFrac(1, 1)) then
                    if equalFrac(coeff, makeFrac(-1, 1)) then
                        io.write("-")
                    else
                        printFrac(coeff)
                    end
                end
            else
                if equalFrac(coeff, makeFrac(1, 1)) then
                    io.write(" + ")
                elseif equalFrac(coeff, makeFrac(-1, 1)) then
                    io.write(" - ")
                elseif lessFrac(makeFrac(0, 1), coeff) then
                    io.write(" + ")
                    printFrac(coeff)
                else
                    io.write(" - ")
                    printFrac(negateFrac(coeff))
                end
            end

            local pwr = p + 1 - j
            if pwr>1 then
                io.write("n^"..pwr)
            else
                io.write("n")
            end
        end
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
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```



## Maxima


```maxima
sum1(p):=sum(stirling2(p,k)*pochhammer(n-k+1,k+1)/(k+1),k,0,p)$
sum2(p):=sum((-1)^j*binomial(p+1,j)*bern(j)*n^(p-j+1),j,0,p)/(p+1)$

makelist(expand(sum1(p)-sum2(p)),p,1,10);
[0,0,0,0,0,0,0,0,0,0]

for p from 0 thru 9 do print(expand(sum2(p)));
```

```txt

n
n^2/2+n/2
n^3/3+n^2/2+n/6
n^4/4+n^3/2+n^2/4
n^5/5+n^4/2+n^3/3-n/30
n^6/6+n^5/2+(5*n^4)/12-n^2/12
n^7/7+n^6/2+n^5/2-n^3/6+n/42
n^8/8+n^7/2+(7*n^6)/12-(7*n^4)/24+n^2/12
n^9/9+n^8/2+(2*n^7)/3-(7*n^5)/15+(2*n^3)/9-n/30
n^10/10+n^9/2+(3*n^8)/4-(7*n^6)/10+n^4/2-(3*n^2)/20

```


=={{header|Modula-2}}==
```modula2
MODULE Faulhaber;
FROM EXCEPTIONS IMPORT AllocateSource,ExceptionSource,GetMessage,RAISE;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

VAR TextWinExSrc : ExceptionSource;

(* Helper Functions *)
PROCEDURE Abs(n : INTEGER) : INTEGER;
BEGIN
    IF n < 0 THEN
        RETURN -n
    END;
    RETURN n
END Abs;

PROCEDURE Binomial(n,k : INTEGER) : INTEGER;
VAR i,num,denom : INTEGER;
BEGIN
    IF (n < 0) OR (k < 0) OR (n < k) THEN
        RAISE(TextWinExSrc, 0, "Argument Exception.")
    END;
    IF (n = 0) OR (k = 0) THEN
        RETURN 1
    END;
    num := 1;
    FOR i:=k+1 TO n DO
        num := num * i
    END;
    denom := 1;
    FOR i:=2 TO n - k DO
        denom := denom * i
    END;
    RETURN num / denom
END Binomial;

PROCEDURE GCD(a,b : INTEGER) : INTEGER;
BEGIN
    IF b = 0 THEN
        RETURN a
    END;
    RETURN GCD(b, a MOD b)
END GCD;

PROCEDURE WriteInteger(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInteger;

(* Fraction Handling *)
TYPE Frac = RECORD
    num,denom : INTEGER;
END;

PROCEDURE InitFrac(n,d : INTEGER) : Frac;
VAR nn,dd,g : INTEGER;
BEGIN
    IF d = 0 THEN
        RAISE(TextWinExSrc, 0, "The denominator must not be zero.")
    END;
    IF n = 0 THEN
        d := 1
    ELSIF d < 0 THEN
        n := -n;
        d := -d
    END;
    g := Abs(GCD(n, d));
    IF g > 1 THEN
        n := n / g;
        d := d / g
    END;
    RETURN Frac{n, d}
END InitFrac;

PROCEDURE EqualFrac(a,b : Frac) : BOOLEAN;
BEGIN
    RETURN (a.num = b.num) AND (a.denom = b.denom)
END EqualFrac;

PROCEDURE LessFrac(a,b : Frac) : BOOLEAN;
BEGIN
    RETURN a.num * b.denom < b.num * a.denom
END LessFrac;

PROCEDURE NegateFrac(f : Frac) : Frac;
BEGIN
    RETURN Frac{-f.num, f.denom}
END NegateFrac;

PROCEDURE SubFrac(lhs,rhs : Frac) : Frac;
BEGIN
    RETURN InitFrac(lhs.num * rhs.denom - lhs.denom * rhs.num, rhs.denom * lhs.denom)
END SubFrac;

PROCEDURE MultFrac(lhs,rhs : Frac) : Frac;
BEGIN
    RETURN InitFrac(lhs.num * rhs.num, lhs.denom * rhs.denom)
END MultFrac;

PROCEDURE Bernoulli(n : INTEGER) : Frac;
VAR
    a : ARRAY[0..15] OF Frac;
    i,j,m : INTEGER;
BEGIN
    IF n < 0 THEN
        RAISE(TextWinExSrc, 0, "n may not be negative or zero.")
    END;
    FOR m:=0 TO n DO
        a[m] := Frac{1, m + 1};
        FOR j:=m TO 1 BY -1 DO
            a[j-1] := MultFrac(SubFrac(a[j-1], a[j]), Frac{j, 1})
        END
    END;
    IF n # 1 THEN RETURN a[0] END;
    RETURN NegateFrac(a[0])
END Bernoulli;

PROCEDURE WriteFrac(f : Frac);
BEGIN
    WriteInteger(f.num);
    IF f.denom # 1 THEN
        WriteString("/");
        WriteInteger(f.denom)
    END
END WriteFrac;

(* Target *)
PROCEDURE Faulhaber(p : INTEGER);
VAR
    j,pwr,sign : INTEGER;
    q,coeff : Frac;
BEGIN
    WriteInteger(p);
    WriteString(" : ");
    q := InitFrac(1, p + 1);
    sign := -1;
    FOR j:=0 TO p DO
        sign := -1 * sign;
        coeff := MultFrac(MultFrac(MultFrac(q, Frac{sign, 1}), Frac{Binomial(p + 1, j), 1}), Bernoulli(j));
        IF EqualFrac(coeff, Frac{0, 1}) THEN CONTINUE END;
        IF j = 0 THEN
            IF NOT EqualFrac(coeff, Frac{1, 1}) THEN
                IF EqualFrac(coeff, Frac{-1, 1}) THEN
                    WriteString("-")
                ELSE
                    WriteFrac(coeff)
                END
            END
        ELSE
            IF EqualFrac(coeff, Frac{1, 1}) THEN
                WriteString(" + ")
            ELSIF EqualFrac(coeff, Frac{-1, 1}) THEN
                WriteString(" - ")
            ELSIF LessFrac(Frac{0, 1}, coeff) THEN
                WriteString(" + ");
                WriteFrac(coeff)
            ELSE
                WriteString(" - ");
                WriteFrac(NegateFrac(coeff))
            END
        END;
        pwr := p + 1 - j;
        IF pwr > 1 THEN
            WriteString("n^");
            WriteInteger(pwr)
        ELSE
            WriteString("n")
        END
    END;
    WriteLn
END Faulhaber;

(* Main *)
VAR i : INTEGER;
BEGIN
    FOR i:=0 TO 9 DO
        Faulhaber(i)
    END;
    ReadChar
END Faulhaber.
```

```txt
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```



## PARI/GP

PARI/GP has 2 built in functions: bernfrac(n) for Bernoulli numbers and bernpol(n) for [[wp:Bernoulli_polynomials| Bernoulli polynomials]].
Using Bernoulli polynomials in PARI/GP is more simple, clear and much faster. (See version #2).
### Version #1. Using Bernoulli numbers.

This version is using "Faulhaber's" formula based on Bernoulli numbers.

It's not worth using Bernoulli numbers in PARI/GP, because too much cleaning if you are avoiding "dirty" (but correct) result.

Note: Find ssubstr() function here on RC.

```parigp

\\ Using "Faulhaber's" formula based on Bernoulli numbers. aev 2/7/17
\\ In str string replace all occurrences of the search string ssrch with the replacement string srepl. aev  3/8/16
sreplace(str,ssrch,srepl)={
  my(sn=#str,ssn=#ssrch,srn=#srepl,sres="",Vi,Vs,Vz,vin,vin1,vi,L=List(),tok,zi,js=1);
  if(sn==0,return("")); if(ssn==0||ssn>sn,return(str));
  \\ Vi - found ssrch indexes
  Vi=sfindalls(str,ssrch); vin=#Vi;
  if(vin==0,return(str));
  vin1=vin+1; Vi=Vec(Vi,vin1); Vi[vin1]=sn+1;
  for(i=1,vin1, vi=Vi[i];
  for(j=js,sn, \\print("ij:",i,"/",j,": ",sres);
    if(j!=vi, sres=concat(sres,ssubstr(str,j,1)),
              sres=concat(sres,srepl); js=j+ssn; break)
  ); \\fend j
  ); \\fend i
  return(sres);
}
B(n)=(bernfrac(n));
Comb(n,k)={my(r=0); if(k<=n, r=n!/(n-k)!/k!); return(r)};
Faulhaber2(p)={
  my(s="",s1="",s2="",c1=0,c2=0);
  for(j=0,p, c1=(-1)^j*Comb(p+1,j)*B(j); c2=(p+1-j);
    s2="*n";
    if(c1==0, next);
    if(c2==1, s1="", s1=Str("^",c2));
    s=Str(s,c1,s2,s1,"+") );
  s=ssubstr(s,1,#s-1); s=sreplace(s,"1*n","n"); s=sreplace(s,"+-","-");
  if(p==0, s="n", s=Str("(",s,")/",p+1)); print(p,": ",s);
}
{\\ Testing:
for(i=0,9, Faulhaber2(i))}

```

```txt

0: n
1: (n^2+n)/2
2: (n^3+3/2*n^2+1/2*n)/3
3: (n^4+2*n^3+n^2)/4
4: (n^5+5/2*n^4+5/3*n^3-1/6*n)/5
5: (n^6+3*n^5+5/2*n^4-1/2*n^2)/6
6: (n^7+7/2*n^6+7/2*n^5-7/6*n^3+1/6*n)/7
7: (n^8+4*n^7+14/3*n^6-7/3*n^4+2/3*n^2)/8
8: (n^9+9/2*n^8+6*n^7-21/5*n^5+2*n^3-3/10*n)/9
9: (n^10+5*n^9+15/2*n^8-7*n^6+5*n^4-3/2*n^2)/10
time = 16 ms.

```


### Version #2. Using Bernoulli polynomials.

This version is using the sums of pth powers formula from [[wp:Bernoulli_polynomials| Bernoulli polynomials]].
It has small, simple and clear code, and produces instant result.

```parigp

\\ Using a formula based on Bernoulli polynomials. aev 2/5/17
Faulhaber1(m)={
  my(B,B1,B2,Bn);
  Bn=bernpol(m+1);
  x=n+1; B1=eval(Bn); x=0; B2=eval(Bn);
  Bn=(B1-B2)/(m+1); if(m==0, Bn=Bn-1);
  print(m,": ",Bn);
}
{\\ Testing:
  for(i=0,9, Faulhaber1(i))}

```

```txt

0: n
1: 1/2*n^2 + 1/2*n
2: 1/3*n^3 + 1/2*n^2 + 1/6*n
3: 1/4*n^4 + 1/2*n^3 + 1/4*n^2
4: 1/5*n^5 + 1/2*n^4 + 1/3*n^3 - 1/30*n
5: 1/6*n^6 + 1/2*n^5 + 5/12*n^4 - 1/12*n^2
6: 1/7*n^7 + 1/2*n^6 + 1/2*n^5 - 1/6*n^3 + 1/42*n
7: 1/8*n^8 + 1/2*n^7 + 7/12*n^6 - 7/24*n^4 + 1/12*n^2
8: 1/9*n^9 + 1/2*n^8 + 2/3*n^7 - 7/15*n^5 + 2/9*n^3 - 1/30*n
9: 1/10*n^10 + 1/2*n^9 + 3/4*n^8 - 7/10*n^6 + 1/2*n^4 - 3/20*n^2
> ##
  ***   last result computed in 0 ms

```



## Perl


```perl
use 5.014;
use Math::Algebra::Symbols;

sub bernoulli_number {
    my ($n) = @_;

    return 0 if $n > 1 && $n % 2;

    my @A;
    for my $m (0 .. $n) {
        $A[$m] = symbols(1) / ($m + 1);

        for (my $j = $m ; $j > 0 ; $j--) {
            $A[$j - 1] = $j * ($A[$j - 1] - $A[$j]);
        }
    }

    return $A[0];
}

sub binomial {
    my ($n, $k) = @_;
    return 1 if $k == 0 || $n == $k;
    binomial($n - 1, $k - 1) + binomial($n - 1, $k);
}

sub faulhaber_s_formula {
    my ($p) = @_;

    my $formula = 0;
    for my $j (0 .. $p) {
        $formula += binomial($p + 1, $j)
                 *  bernoulli_number($j)
                 *  symbols('n')**($p + 1 - $j);
    }

    (symbols(1) / ($p + 1) * $formula)
        =~ s/\$n/n/gr =~ s/\*\*/^/gr =~ s/\*/ /gr;
}

foreach my $i (0 .. 9) {
    say "$i: ", faulhaber_s_formula($i);
}
```

```txt

0: n
1: 1/2 n+1/2 n^2
2: 1/6 n+1/2 n^2+1/3 n^3
3: 1/4 n^2+1/2 n^3+1/4 n^4
4: -1/30 n+1/3 n^3+1/2 n^4+1/5 n^5
5: -1/12 n^2+5/12 n^4+1/2 n^5+1/6 n^6
6: 1/42 n-1/6 n^3+1/2 n^5+1/2 n^6+1/7 n^7
7: 1/12 n^2-7/24 n^4+7/12 n^6+1/2 n^7+1/8 n^8
8: -1/30 n+2/9 n^3-7/15 n^5+2/3 n^7+1/2 n^8+1/9 n^9
9: -3/20 n^2+1/2 n^4-7/10 n^6+3/4 n^8+1/2 n^9+1/10 n^10

```



## Perl 6

```perl6
sub bernoulli_number($n) {

    return 1/2 if $n == 1;
    return 0/1 if $n % 2;

    my @A;
    for 0..$n -> $m {
        @A[$m] = 1 / ($m + 1);
        for $m, $m-1 ... 1 -> $j {
            @A[$j - 1] = $j * (@A[$j - 1] - @A[$j]);
        }
    }

    return @A[0];
}

sub binomial($n, $k) {
    $k == 0 || $n == $k ?? 1 !! binomial($n-1, $k-1) + binomial($n-1, $k);
}

sub faulhaber_s_formula($p) {

    my @formula = gather for 0..$p -> $j {
        take '('
            ~ join('/', (binomial($p+1, $j) * bernoulli_number($j)).Rat.nude)
            ~ ")*n^{$p+1 - $j}";
    }

    my $formula = join(' + ', @formula.grep({!m{'(0/1)*'}}));

    $formula .= subst(rx{ '(1/1)*' }, '', :g);
    $formula .= subst(rx{ '^1'» }, '', :g);

    "1/{$p+1} * ($formula)";
}

for 0..9 -> $p {
    say "f($p) = ", faulhaber_s_formula($p);
}
```

```txt

f(0) = 1/1 * (n)
f(1) = 1/2 * (n^2 + n)
f(2) = 1/3 * (n^3 + (3/2)*n^2 + (1/2)*n)
f(3) = 1/4 * (n^4 + (2/1)*n^3 + n^2)
f(4) = 1/5 * (n^5 + (5/2)*n^4 + (5/3)*n^3 + (-1/6)*n)
f(5) = 1/6 * (n^6 + (3/1)*n^5 + (5/2)*n^4 + (-1/2)*n^2)
f(6) = 1/7 * (n^7 + (7/2)*n^6 + (7/2)*n^5 + (-7/6)*n^3 + (1/6)*n)
f(7) = 1/8 * (n^8 + (4/1)*n^7 + (14/3)*n^6 + (-7/3)*n^4 + (2/3)*n^2)
f(8) = 1/9 * (n^9 + (9/2)*n^8 + (6/1)*n^7 + (-21/5)*n^5 + (2/1)*n^3 + (-3/10)*n)
f(9) = 1/10 * (n^10 + (5/1)*n^9 + (15/2)*n^8 + (-7/1)*n^6 + (5/1)*n^4 + (-3/2)*n^2)

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
    integer num = 1,
            denom = 1
    for i=k+1 to n do
        num *= i
    end for
    for i=2 to n-k do
        denom *= i
    end for
    return num / denom
end function

procedure faulhaber(integer p)
    string res = sprintf("%d : ", p)
    frac q = {1, p+1}
    for j=0 to p do
        frac bj = bernoulli(j)
        if frac_ne(bj,frac_zero) then
            frac coeff = frac_mul({binomial(p+1,j),p+1},bj)
            string s = frac_sprint(coeff)
            if j=0 then
                if s="1" then
                    s = ""
                end if
            else
                if s[1]='-' then
                    s[1..1] = " - "
                else
                    s[1..0] = " + "
                end if
            end if
            res &= s&"n"
            integer pwr = p+1-j
            if pwr>1 then
                res &= sprintf("^%d", pwr)
            end if
        end if
    end for
    printf(1,"%s\n",{res})
end procedure

for i=0 to 9 do
    faulhaber(i)
end for
```

```txt

0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2

```


== {{header|Python}} ==

The following implementation does not use [https://en.wikipedia.org/wiki/Bernoulli_number Bernoulli numbers], but [https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind Stirling numbers of the second kind], based on the relation: <math>m^n=\sum_{k=0}^n S_n^k (m)_k=\sum_{k=0}^n S_n^k k!{m\choose k}</math>.

Then summing: <math>\sum_{j=0}^{m} j^n=\sum_{j=0}^m\sum_{k=0}^n S_n^k k!{j\choose k}=\sum_{k=0}^n S_n^k k!{m+1\choose k+1}=\sum_{k=0}^n S_n^k \frac{(m+1)_{k+1}}{k+1}</math>.

One has then to expand the product <math>(m+1)_{k+1}</math> in order to get a polynomial in the variable <math>m</math>. Also, for the sum of <math>j^0</math>, the sum is too large by one (since we start at <math>j=0</math>), this has to be taken into account.


'''Note:''' a number of the formulae above are invisible to the majority of browsers, including Chrome, IE/Edge, Safari and Opera. They may (subject to the installation of necessary fronts) be visible to Firefox.



```python
from fractions import Fraction

def nextu(a):
    n = len(a)
    a.append(1)
    for i in range(n - 1, 0, -1):
        a[i] = i * a[i] + a[i - 1]
    return a

def nextv(a):
    n = len(a) - 1
    b = [(1 - n) * x for x in a]
    b.append(1)
    for i in range(n):
        b[i + 1] += a[i]
    return b

def sumpol(n):
    u = [0, 1]
    v = [[1], [1, 1]]
    yield [Fraction(0), Fraction(1)]
    for i in range(1, n):
        v.append(nextv(v[-1]))
        t = [0] * (i + 2)
        p = 1
        for j, r in enumerate(u):
            r = Fraction(r, j + 1)
            for k, s in enumerate(v[j + 1]):
                t[k] += r * s
        yield t
        u = nextu(u)

def polstr(a):
    s = ""
    q = False
    n = len(a) - 1
    for i, x in enumerate(reversed(a)):
        i = n - i
        if i < 2:
            m = "n" if i == 1 else ""
        else:
            m = "n^%d" % i
        c = str(abs(x))
        if i > 0:
            if c == "1":
                c = ""
            else:
                m = " " + m
        if x != 0:
            if q:
                t = " + " if x > 0 else " - "
                s += "%s%s%s" % (t, c, m)
            else:
                t = "" if x > 0 else "-"
                s = "%s%s%s" % (t, c, m)
                q = True
    if q:
        return s
    else:
        return "0"

for i, p in enumerate(sumpol(10)):
    print(i, ":", polstr(p))
```


```txt
0 : n
1 : 1/2 n^2 + 1/2 n
2 : 1/3 n^3 + 1/2 n^2 + 1/6 n
3 : 1/4 n^4 + 1/2 n^3 + 1/4 n^2
4 : 1/5 n^5 + 1/2 n^4 + 1/3 n^3 - 1/30 n
5 : 1/6 n^6 + 1/2 n^5 + 5/12 n^4 - 1/12 n^2
6 : 1/7 n^7 + 1/2 n^6 + 1/2 n^5 - 1/6 n^3 + 1/42 n
7 : 1/8 n^8 + 1/2 n^7 + 7/12 n^6 - 7/24 n^4 + 1/12 n^2
8 : 1/9 n^9 + 1/2 n^8 + 2/3 n^7 - 7/15 n^5 + 2/9 n^3 - 1/30 n
9 : 1/10 n^10 + 1/2 n^9 + 3/4 n^8 - 7/10 n^6 + 1/2 n^4 - 3/20 n^2
```



## Racket


Racket will simplify rational numbers; if this code simplifies the expressions too much for your tastes (e.g. you like <code>1/1 * (n)</code>) then tweak the simplify... clauses to taste.


```racket
#lang racket/base

(require racket/match
         racket/string
         math/number-theory)

(define simplify-arithmetic-expression
  (letrec ((s-a-e
            (match-lambda
              [(list (and op '+) l ... (list '+ m ...) r ...) (s-a-e `(,op ,@l ,@m ,@r))]
              [(list (and op '+) l ... (? number? n1) m ... (? number? n2) r ...) (s-a-e `(,op ,@l ,(+ n1 n2) ,@m ,@r))]
              [(list (and op '+) (app s-a-e l _) ... 0 (app s-a-e r _) ...) (s-a-e `(,op ,@l ,@r))]
              [(list (and op '+) (app s-a-e x _)) (values x #t)]
              [(list (and op '*) l ... (list '* m ...) r ...) (s-a-e `(,op ,@l ,@m ,@r))]
              [(list (and op '*) l ... (? number? n1) m ... (? number? n2) r ...) (s-a-e `(,op ,@l ,(* n1 n2) ,@m ,@r))]
              [(list (and op '*) (app s-a-e l _) ... 1 (app s-a-e r _) ...) (s-a-e `(,op ,@l ,@r))]
              [(list (and op '*) (app s-a-e l _) ... 0 (app s-a-e r _) ...) (values 0 #t)]
              [(list (and op '*) (app s-a-e x _)) (values x #t)]
              [(list 'expt (app s-a-e x x-simplified?) 1) (values x x-simplified?)]
              [(list op (app s-a-e a #f) ...) (values `(,op ,@a) #f)]
              [(list op (app s-a-e a _) ...) (s-a-e `(,op ,@a))]
              [e (values e #f)])))
    s-a-e))

(define (expression->infix-string e)
  (define (parenthesise-maybe s p?)
    (if p? (string-append "(" s ")") s))

  (letrec ((e->is
            (lambda (paren?)
              (match-lambda
                [(list (and op (or '+ '- '* '*)) (app (e->is #t) a p?) ...)
                 (define bits (map parenthesise-maybe a p?))
                 (define compound (string-join bits (format " ~a " op)))
                 (values (if paren? (string-append "(" compound ")") compound) #f)]
                [(list 'expt (app (e->is #t) x xp?) (app (e->is #t) n np?))
                 (values (format "~a^~a" (parenthesise-maybe x xp?) (parenthesise-maybe n np?)) #f)]
                [(? number? (app number->string s)) (values s #f)]
                [(? symbol? (app symbol->string s)) (values s #f)]))))
    (define-values (str needs-parens?) ((e->is #f) e))
    str))

(define (faulhaber p)
  (define p+1 (add1 p))
  (define-values (simpler simplified?)
    (simplify-arithmetic-expression
     `(* ,(/ 1 p+1)
         (+ ,@(for/list ((j (in-range p+1)))
                `(* ,(* (expt -1 j)
                        (binomial p+1 j))
                    (* ,(bernoulli-number j)
                       (expt n ,(- p+1 j)))))))))
  simpler)

(for ((p (in-range 0 (add1 9))))
  (printf "f(~a) = ~a~%" p (expression->infix-string (faulhaber p))))

```


```txt

f(0) = n
f(1) = 1/2 * (n^2 + n)
f(2) = 1/3 * (n^3 + (3/2 * n^2) + (1/2 * n))
f(3) = 1/4 * (n^4 + (2 * n^3) + n^2)
f(4) = 1/5 * (n^5 + (5/2 * n^4) + (5/3 * n^3) + (-1/6 * n))
f(5) = 1/6 * (n^6 + (3 * n^5) + (5/2 * n^4) + (-1/2 * n^2))
f(6) = 1/7 * (n^7 + (7/2 * n^6) + (7/2 * n^5) + (-7/6 * n^3) + (1/6 * n))
f(7) = 1/8 * (n^8 + (4 * n^7) + (14/3 * n^6) + (-7/3 * n^4) + (2/3 * n^2))
f(8) = 1/9 * (n^9 + (9/2 * n^8) + (6 * n^7) + (-21/5 * n^5) + (2 * n^3) + (-3/10 * n))
f(9) = 1/10 * (n^10 + (5 * n^9) + (15/2 * n^8) + (-7 * n^6) + (5 * n^4) + (-3/2 * n^2))

```



## Sidef


```ruby
func faulhaber_s_formula(p) {

    var formula = gather {
        { |j|
            take "(#{binomial(p+1, j) * j.bernfrac -> as_rat})*n^#{p+1 - j}"
        } << 0..p
    }

    formula.grep! { !.contains('(0)*') }.join!(' + ')

    formula -= /\(1\)\*/g
    formula -= /\^1\b/g
    formula.gsub!(/\(([^+]*?)\)/, { _ })

    "1/#{p + 1} * (#{formula})"
}

{ |p|
    printf("%2d: %s\n", p, faulhaber_s_formula(p))
} << ^10
```

```txt

 0: 1/1 * (n)
 1: 1/2 * (n^2 + n)
 2: 1/3 * (n^3 + 3/2*n^2 + 1/2*n)
 3: 1/4 * (n^4 + 2*n^3 + n^2)
 4: 1/5 * (n^5 + 5/2*n^4 + 5/3*n^3 + -1/6*n)
 5: 1/6 * (n^6 + 3*n^5 + 5/2*n^4 + -1/2*n^2)
 6: 1/7 * (n^7 + 7/2*n^6 + 7/2*n^5 + -7/6*n^3 + 1/6*n)
 7: 1/8 * (n^8 + 4*n^7 + 14/3*n^6 + -7/3*n^4 + 2/3*n^2)
 8: 1/9 * (n^9 + 9/2*n^8 + 6*n^7 + -21/5*n^5 + 2*n^3 + -3/10*n)
 9: 1/10 * (n^10 + 5*n^9 + 15/2*n^8 + -7*n^6 + 5*n^4 + -3/2*n^2)

```


By not simplifying the formulas, we can have a much cleaner code:

```ruby
func faulhaber_s_formula(p) {
    "1/#{p + 1} * (" + gather {
      { |j|
         take "#{binomial(p+1, j) * j.bernfrac -> as_rat}*n^#{p+1 - j}"
      } << 0..p
    }.join(' + ') + ")"
}

{ |p|
    printf("%2d: %s\n", p, faulhaber_s_formula(p))
} << ^10
```

```txt

 0: 1/1 * (1*n^1)
 1: 1/2 * (1*n^2 + 1*n^1)
 2: 1/3 * (1*n^3 + 3/2*n^2 + 1/2*n^1)
 3: 1/4 * (1*n^4 + 2*n^3 + 1*n^2 + 0*n^1)
 4: 1/5 * (1*n^5 + 5/2*n^4 + 5/3*n^3 + 0*n^2 + -1/6*n^1)
 5: 1/6 * (1*n^6 + 3*n^5 + 5/2*n^4 + 0*n^3 + -1/2*n^2 + 0*n^1)
 6: 1/7 * (1*n^7 + 7/2*n^6 + 7/2*n^5 + 0*n^4 + -7/6*n^3 + 0*n^2 + 1/6*n^1)
 7: 1/8 * (1*n^8 + 4*n^7 + 14/3*n^6 + 0*n^5 + -7/3*n^4 + 0*n^3 + 2/3*n^2 + 0*n^1)
 8: 1/9 * (1*n^9 + 9/2*n^8 + 6*n^7 + 0*n^6 + -21/5*n^5 + 0*n^4 + 2*n^3 + 0*n^2 + -3/10*n^1)
 9: 1/10 * (1*n^10 + 5*n^9 + 15/2*n^8 + 0*n^7 + -7*n^6 + 0*n^5 + 5*n^4 + 0*n^3 + -3/2*n^2 + 0*n^1)

```



## Visual Basic .NET

```vbnet
Module Module1
    Function Gcd(a As Long, b As Long)
        If b = 0 Then
            Return a
        End If
        Return Gcd(b, a Mod b)
    End Function

    Class Frac
        ReadOnly num As Long
        ReadOnly denom As Long

        Public Shared ReadOnly ZERO As New Frac(0, 1)
        Public Shared ReadOnly ONE As New Frac(1, 1)

        Public Sub New(n As Long, d As Long)
            If d = 0 Then Throw New ArgumentException("d must not be zero")
            Dim nn = n
            Dim dd = d
            If nn = 0 Then
                dd = 1
            ElseIf dd < 0 Then
                nn = -nn
                dd = -dd
            End If
            Dim g = Math.Abs(Gcd(nn, dd))
            If g > 1 Then
                nn /= g
                dd /= g
            End If
            num = nn
            denom = dd
        End Sub

        Public Shared Operator -(self As Frac) As Frac
            Return New Frac(-self.num, self.denom)
        End Operator

        Public Shared Operator +(lhs As Frac, rhs As Frac) As Frac
            Return New Frac(lhs.num * rhs.denom + lhs.denom * rhs.num, rhs.denom * lhs.denom)
        End Operator

        Public Shared Operator -(lhs As Frac, rhs As Frac) As Frac
            Return lhs + -rhs
        End Operator

        Public Shared Operator *(lhs As Frac, rhs As Frac) As Frac
            Return New Frac(lhs.num * rhs.num, lhs.denom * rhs.denom)
        End Operator

        Public Shared Operator <(lhs As Frac, rhs As Frac) As Boolean
            Dim x = lhs.num / lhs.denom
            Dim y = rhs.num / rhs.denom
            Return x < y
        End Operator

        Public Shared Operator >(lhs As Frac, rhs As Frac) As Boolean
            Dim x = lhs.num / lhs.denom
            Dim y = rhs.num / rhs.denom
            Return x > y
        End Operator

        Public Shared Operator =(lhs As Frac, rhs As Frac) As Boolean
            Return lhs.num = rhs.num AndAlso lhs.denom = rhs.denom
        End Operator

        Public Shared Operator <>(lhs As Frac, rhs As Frac) As Boolean
            Return lhs.num <> rhs.num OrElse lhs.denom <> rhs.denom
        End Operator

        Public Overloads Function Equals(obj As Object) As Boolean
            Dim frac = CType(obj, Frac)
            Return Not IsNothing(frac) AndAlso num = frac.num AndAlso denom = frac.denom
        End Function

        Public Overloads Function GetHashCode() As Integer
            Dim hashCode = 1317992671
            hashCode = hashCode * -1521134295 + num.GetHashCode()
            hashCode = hashCode * -1521134295 + denom.GetHashCode()
            Return hashCode
        End Function

        Public Overloads Function ToString() As String
            If denom = 1 Then Return num.ToString()
            Return String.Format("{0}/{1}", num, denom)
        End Function
    End Class

    Function Bernoulli(n As Integer) As Frac
        If n < 0 Then Throw New ArgumentException("n may not be negative or zero")
        Dim a(n + 1) As Frac
        For m = 0 To n
            a(m) = New Frac(1, m + 1)
            For j = m To 1 Step -1
                a(j - 1) = (a(j - 1) - a(j)) * New Frac(j, 1)
            Next
        Next
        'returns the first Bernoulli number
        If n <> 1 Then Return a(0)
        Return -a(0)
    End Function

    Function Binomial(n As Integer, k As Integer) As Integer
        If n < 0 OrElse k < 0 OrElse n < k Then
            Throw New ArgumentException()
        End If
        If n = 0 OrElse k = 0 Then
            Return 1
        End If
        Dim num = 1
        For i = k + 1 To n
            num *= i
        Next
        Dim denom = 1
        For i = 2 To n - k
            denom *= i
        Next
        Return num / denom
    End Function

    Sub Faulhaber(p As Integer)
        Console.Write("{0} : ", p)
        Dim q As New Frac(1, p + 1)
        Dim sign = -1
        For j = 0 To p
            sign *= -1
            Dim coeff = q * New Frac(sign, 1) * New Frac(Binomial(p + 1, j), 1) * Bernoulli(j)
            If Frac.ZERO = coeff Then Continue For
            If j = 0 Then
                If Frac.ONE <> coeff Then
                    If -Frac.ONE = coeff Then
                        Console.Write("-")
                    Else
                        Console.Write(coeff.ToString())
                    End If
                End If
            Else
                If Frac.ONE = coeff Then
                    Console.Write(" + ")
                ElseIf -Frac.ONE = coeff Then
                    Console.Write(" - ")
                ElseIf Frac.ZERO < coeff Then
                    Console.Write(" + {0}", coeff.ToString())
                Else
                    Console.Write(" - {0}", (-coeff).ToString())
                End If
            End If
            Dim pwr = p + 1 - j
            If pwr > 1 Then
                Console.Write("n^{0}", pwr)
            Else
                Console.Write("n")
            End If
        Next
        Console.WriteLine()
    End Sub

    Sub Main()
        For i = 0 To 9
            Faulhaber(i)
        Next
    End Sub
End Module
```

```txt
0 : n
1 : 1/2n^2 + 1/2n
2 : 1/3n^3 + 1/2n^2 + 1/6n
3 : 1/4n^4 + 1/2n^3 + 1/4n^2
4 : 1/5n^5 + 1/2n^4 + 1/3n^3 - 1/30n
5 : 1/6n^6 + 1/2n^5 + 5/12n^4 - 1/12n^2
6 : 1/7n^7 + 1/2n^6 + 1/2n^5 - 1/6n^3 + 1/42n
7 : 1/8n^8 + 1/2n^7 + 7/12n^6 - 7/24n^4 + 1/12n^2
8 : 1/9n^9 + 1/2n^8 + 2/3n^7 - 7/15n^5 + 2/9n^3 - 1/30n
9 : 1/10n^10 + 1/2n^9 + 3/4n^8 - 7/10n^6 + 1/2n^4 - 3/20n^2
```



## zkl

{{libheader|GMP}} GNU Multiple Precision Arithmetic Library
Uses code from the Bernoulli numbers task (copied here).

```zkl
var [const] BN=Import("zklBigNum");	// libGMP (GNU MP Bignum Library)

fcn faulhaberFormula(p){  //-->(Rational,Rational...)
   [p..0,-1].pump(List(),'wrap(k){ B(k)*BN(p+1).binomial(k) })
   .apply('*(Rational(1,p+1)))
}
```


```zkl
foreach p in (10){
   println("F(%d) --> %s".fmt(p,polyRatString(faulhaberFormula(p))))
}
```


```zkl
class Rational{  // Weenie Rational class, can handle BigInts
   fcn init(_a,_b){ var a=_a, b=_b; normalize(); }
   fcn toString{
      if(b==1) a.toString()
      else     "%d/%d".fmt(a,b)
   }
   var [proxy] isZero=fcn{ a==0   };
   fcn normalize{  // divide a and b by gcd
      g:= a.gcd(b);
      a/=g; b/=g;
      if(b<0){ a=-a; b=-b; } // denominator > 0
      self
   }
   fcn __opAdd(n){
      if(Rational.isChildOf(n)) self(a*n.b + b*n.a, b*n.b); // Rat + Rat
      else self(b*n + a, b);				    // Rat + Int
   }
   fcn __opSub(n){ self(a*n.b - b*n.a, b*n.b) }		    // Rat - Rat
   fcn __opMul(n){
      if(Rational.isChildOf(n)) self(a*n.a, b*n.b);	    // Rat * Rat
      else self(a*n, b);				    // Rat * Int
   }
   fcn __opDiv(n){ self(a*n.b,b*n.a) }			    // Rat / Rat
}
```


```zkl
fcn B(N){	// calculate Bernoulli(n) --> Rational
   var A=List.createLong(100,0);  // aka static aka not thread safe
   foreach m in (N+1){
      A[m]=Rational(BN(1),BN(m+1));
      foreach j in ([m..1, -1]){ A[j-1]= (A[j-1] - A[j])*j; }
   }
   A[0]
}
fcn polyRatString(terms){ // (a1,a2...)-->"a1n + a2n^2 ..."
   str:=[1..].zipWith('wrap(n,a){ if(a.isZero) "" else "+ %sn^%s ".fmt(a,n) },
        terms)
   .pump(String)
   .replace(" 1n"," n").replace("n^1 ","n ").replace("+ -","- ");
   if(not str)     return(" ");  // all zeros
   if(str[0]=="+") str[1,*];     // leave leading space
   else            String("-",str[2,*]);
}
```

```txt

F(0) -->  n
F(1) -->  1/2n + 1/2n^2
F(2) -->  1/6n + 1/2n^2 + 1/3n^3
F(3) -->  1/4n^2 + 1/2n^3 + 1/4n^4
F(4) --> -1/30n + 1/3n^3 + 1/2n^4 + 1/5n^5
F(5) --> -1/12n^2 + 5/12n^4 + 1/2n^5 + 1/6n^6
F(6) -->  1/42n - 1/6n^3 + 1/2n^5 + 1/2n^6 + 1/7n^7
F(7) -->  1/12n^2 - 7/24n^4 + 7/12n^6 + 1/2n^7 + 1/8n^8
F(8) --> -1/30n + 2/9n^3 - 7/15n^5 + 2/3n^7 + 1/2n^8 + 1/9n^9
F(9) --> -3/20n^2 + 1/2n^4 - 7/10n^6 + 3/4n^8 + 1/2n^9 + 1/10n^10

```


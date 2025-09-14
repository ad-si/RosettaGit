+++
title = "Geometric algebra"
description = ""
date = 2019-10-05T17:30:35Z
aliases = []
[extra]
id = 19663
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "cpp",
  "d",
  "echolisp",
  "go",
  "j",
  "javascript",
  "jsish",
  "kotlin",
  "perl_6",
  "phix",
  "python",
]
+++

'''Geometric algebra''' is an other name for [[wp:Clifford algebra|Clifford algebra]]s and it's basically an algebra containing a vector space <math>\mathcal{V}</math> and obeying the following axioms:

:<math>\begin{array}{c}
(ab)c = a(bc)\\
a(b+c) = ab+ac\\
(a+b)c = ac+bc\\
\forall \mathbf{x}\in\mathcal{V},\,\mathbf{x}^2\in\R
\end{array}
</math>

The product operation in such algebra is called the ''geometric product''.  Elements are called ''multivectors'', while multivectors in <math>\mathcal{V}</math> are just called ''vectors''.

There are a few simple examples of geometric algebras.  A trivial one for instance is simply <math>\R</math>, where <math>\mathcal{V} = \R</math>.  The complex numbers also form a geometric algebra, where the vector space is the one-dimensional space of all purely imaginary numbers.  An other example is the space of [[Quaternion type|quaternions]], where the vector space is the three-dimensional space of all linear combinations of <math>(i, j, k)</math>.

The purpose of this task is to implement a geometric algebra with a vector space <math>\mathcal{V}</math> of dimension ''n'' of at least five, but for extra-credit you can implement a version with ''n'' arbitrary large.  Using a dimension five is useful as it is the dimension required for the so-called ''conformal model'' which will be the subject of a derived task.

To ensure the unicity of the solution (that is, up to some isomorphism), we will also restrict ourselves to the so-called euclidean case, where the square of a non-zero vector is positive:

<math>\forall\mathbf{x}\in\mathcal{V},\, \mathbf{x}\neq 0 \implies\mathbf{x}^2 > 0</math>.

You can of course, for extra credit, implement the general case.  This would require the definition of a parameter for the ''signature'' of the metric.

In order to show that your solution uses a vector space of dimension at least five, you will create a function <tt>n -> e(n)</tt> such that the vectors <tt>e(0), e(1), e(2), e(3), e(4)</tt> are linearly independent.  To do so you will make them orthonormal with the following [[wp:scalar product|scalar product]]:

<math>\mathbf{x}\cdot\mathbf{y} = (\mathbf{x}\mathbf{y} + \mathbf{y}\mathbf{x})/2</math>

The fact that this so-called ''inner product'' defines a scalar product is a consequence of the fourth axiom.  To see it one just needs to notice the relation:

<math>\mathbf{x}\mathbf{y} + \mathbf{y}\mathbf{x} = (\mathbf{x} + \mathbf{y})^2 - \mathbf{x}^2 - \mathbf{y}^2</math>

Once you'll have shown that your vector space is at least of dimension five, you will show that the axioms are satisfied.  For this purpose you will pick three random multivectors ''a'', ''b'' and ''c'', along with a random vector <math>\mathbf{x}</math>.

Producing a random vector is easy.  Just use a pseudo-random generation function <tt>rand</tt> and create a vector:

<math>\mathrm{randomVector}() = \sum_{i=0}^4 \mathrm{rand}() \mathbf{e}_i</math>

Producing a random multivector is slightly more involved.  It is known that when the dimension of <math>\mathcal{V}</math> is ''n'', then the dimension of the algebra (seen as a vector space with its natural scalar multiplication) is 2<sup>n</sup>.  This means that for n=5 there is a basis of 2<sup>5</sup> = 32 basis multivectors from which any multivector can be written as a linear combination.  Create such a basis <math>m_0, m_1,\ldots,m_{31}</math> along with a function producting a random multivector:

<math>\mathrm{randomMultivector}() = \sum_{i=0}^{31} \mathrm{rand}() m_i</math>

To summarize, to solve this task you will:
* define the inner product of two vectors : <math>\mathbf{x}\cdot\mathbf{y} = (\mathbf{xy} + \mathbf{yx})/2</math>.
* define the function <tt>e</tt>
* verify the orthonormality <math>\mathbf{e}_i\cdot\mathbf{e}_j = \delta_{i,j}</math> for i, j in <math>\{0, 1, 2, 3, 4\}</math>.
* create a function returning a random multivector
* create a function returning a random vector
* verify the axioms for three rarndom multivectors ''a'', ''b'', ''c'' and a random vector '''x'''.



Optionally, you will repeat the last step a large number of times, in order to increase confidence in the result.





## C++

```cpp
#include <algorithm>
#include <iostream>
#include <random>
#include <vector>

double uniform01() {
    static std::default_random_engine generator;
    static std::uniform_real_distribution<double> distribution(0.0, 1.0);
    return distribution(generator);
}

int bitCount(int i) {
    i -= ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    i = (i + (i >> 4)) & 0x0F0F0F0F;
    i += (i >> 8);
    i += (i >> 16);
    return i & 0x0000003F;
}

double reorderingSign(int i, int j) {
    int k = i >> 1;
    int sum = 0;
    while (k != 0) {
        sum += bitCount(k & j);
        k = k >> 1;
    }
    return ((sum & 1) == 0) ? 1.0 : -1.0;
}

struct MyVector {
public:
    MyVector(const std::vector<double> &da) : dims(da) {
        // empty
    }

    double &operator[](size_t i) {
        return dims[i];
    }

    const double &operator[](size_t i) const {
        return dims[i];
    }

    MyVector operator+(const MyVector &rhs) const {
        std::vector<double> temp(dims);
        for (size_t i = 0; i < rhs.dims.size(); ++i) {
            temp[i] += rhs[i];
        }
        return MyVector(temp);
    }

    MyVector operator*(const MyVector &rhs) const {
        std::vector<double> temp(dims.size(), 0.0);
        for (size_t i = 0; i < dims.size(); i++) {
            if (dims[i] != 0.0) {
                for (size_t j = 0; j < dims.size(); j++) {
                    if (rhs[j] != 0.0) {
                        auto s = reorderingSign(i, j) * dims[i] * rhs[j];
                        auto k = i ^ j;
                        temp[k] += s;
                    }
                }
            }
        }
        return MyVector(temp);
    }

    MyVector operator*(double scale) const {
        std::vector<double> temp(dims);
        std::for_each(temp.begin(), temp.end(), [scale](double a) { return a * scale; });
        return MyVector(temp);
    }

    MyVector operator-() const {
        return *this * -1.0;
    }

    MyVector dot(const MyVector &rhs) const {
        return (*this * rhs + rhs * *this) * 0.5;
    }

    friend std::ostream &operator<<(std::ostream &, const MyVector &);

private:
    std::vector<double> dims;
};

std::ostream &operator<<(std::ostream &os, const MyVector &v) {
    auto it = v.dims.cbegin();
    auto end = v.dims.cend();

    os << '[';
    if (it != end) {
        os << *it;
        it = std::next(it);
    }
    while (it != end) {
        os << ", " << *it;
        it = std::next(it);
    }
    return os << ']';
}

MyVector e(int n) {
    if (n > 4) {
        throw new std::runtime_error("n must be less than 5");
    }

    auto result = MyVector(std::vector<double>(32, 0.0));
    result[1 << n] = 1.0;
    return result;
}

MyVector randomVector() {
    auto result = MyVector(std::vector<double>(32, 0.0));
    for (int i = 0; i < 5; i++) {
        result = result + MyVector(std::vector<double>(1, uniform01())) * e(i);
    }
    return result;
}

MyVector randomMultiVector() {
    auto result = MyVector(std::vector<double>(32, 0.0));
    for (int i = 0; i < 32; i++) {
        result[i] = uniform01();
    }
    return result;
}

int main() {
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            if (i < j) {
                if (e(i).dot(e(j))[0] != 0.0) {
                    std::cout << "Unexpected non-null scalar product.";
                    return 1;
                } else if (i == j) {
                    if (e(i).dot(e(j))[0] == 0.0) {
                        std::cout << "Unexpected null scalar product.";
                    }
                }
            }
        }
    }

    auto a = randomMultiVector();
    auto b = randomMultiVector();
    auto c = randomMultiVector();
    auto x = randomVector();

    // (ab)c == a(bc)
    std::cout << ((a * b) * c) << '\n';
    std::cout << (a * (b * c)) << "\n\n";

    // a(b+c) == ab + ac
    std::cout << (a * (b + c)) << '\n';
    std::cout << (a * b + a * c) << "\n\n";

    // (a+b)c == ac + bc
    std::cout << ((a + b) * c) << '\n';
    std::cout << (a * c + b * c) << "\n\n";

    // x^2 is real
    std::cout << (x * x) << '\n';

    return 0;
}
```

```txt
[-5.63542, -6.59107, -10.2043, -5.21095, 8.68946, 0.579114, -4.67295, -6.72461, 1.55005, -2.63952, -1.83855, 2.4967, -4.3396, -9.9157, -4.6942, -3.23625, -2.3767, -4.55607, -14.3135, -14.2001, 9.84839, 3.69933, -3.38306, -7.60063, -0.236772, 0.988399, -0.549176, 6.61959, 4.69712, -5.34606, -12.2294, -12.6537]
[-5.63542, -6.59107, -10.2043, -5.21095, 8.68946, 0.579114, -4.67295, -6.72461, 1.55005, -2.63952, -1.83855, 2.4967, -4.3396, -9.9157, -4.6942, -3.23625, -2.3767, -4.55607, -14.3135, -14.2001, 9.84839, 3.69933, -3.38306, -7.60063, -0.236772, 0.988399, -0.549176, 6.61959, 4.69712, -5.34606, -12.2294, -12.6537]

[-6.27324, -6.7904, 3.10486, -1.14265, 6.38677, 3.57612, -3.90542, -4.17752, -1.36656, -0.0780159, 6.77775, 6.39118, 1.5939, 1.04175, 8.18152, 2.72047, -3.59085, -5.1028, 2.62711, -1.41586, 5.84934, 4.25817, 1.1197, 0.123976, -2.04301, -1.81806, 4.87518, 6.67182, 2.91358, 0.252558, 6.15595, 1.1159]
[-6.27324, -6.7904, 3.10486, -1.14265, 6.38677, 3.57612, -3.90542, -4.17752, -1.36656, -0.0780159, 6.77775, 6.39118, 1.5939, 1.04175, 8.18152, 2.72047, -3.59085, -5.1028, 2.62711, -1.41586, 5.84934, 4.25817, 1.1197, 0.123976, -2.04301, -1.81806, 4.87518, 6.67182, 2.91358, 0.252558, 6.15595, 1.1159]

[-7.29133, -8.2555, 0.985588, -1.48171, 2.4995, 4.5152, -1.1938, -2.29702, -2.34025, -2.16526, 10.2208, 7.0629, 0.552639, -0.437582, 7.18962, 2.63274, -3.25348, -4.07006, -0.883786, -3.09677, 1.1018, 2.91198, -0.0095405, 0.0123323, -2.69156, -1.30815, 3.36179, 3.26852, 3.09518, -0.166247, 6.74016, 3.20827]
[-7.29133, -8.2555, 0.985588, -1.48171, 2.4995, 4.5152, -1.1938, -2.29702, -2.34025, -2.16526, 10.2208, 7.0629, 0.552639, -0.437582, 7.18962, 2.63274, -3.25348, -4.07006, -0.883786, -3.09677, 1.1018, 2.91198, -0.0095405, 0.0123323, -2.69156, -1.30815, 3.36179, 3.26852, 3.09518, -0.166247, 6.74016, 3.20827]

[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
```



## D

```d
import std.exception;
import std.random;
import std.stdio;

auto doubleArray(size_t size) {
    double[] result;
    result.length = size;
    result[] = 0.0;
    return result;
}

int bitCount(int i) {
    i -= ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    i = (i + (i >> 4)) & 0x0F0F0F0F;
    i += (i >> 8);
    i += (i >> 16);
    return i & 0x0000003F;
}

double reorderingSign(int i, int j) {
    int k = i >> 1;
    int sum = 0;
    while (k != 0) {
        sum += bitCount(k & j);
        k = k >> 1;
    }
    return ((sum & 1) == 0) ? 1.0 : -1.0;
}

struct Vector {
    private double[] dims;

    this(double[] da) {
        dims = da;
    }

    Vector dot(Vector rhs) {
        return (this * rhs + rhs * this) * 0.5;
    }

    Vector opUnary(string op : "-")() {
        return this * -1.0;
    }

    Vector opBinary(string op)(Vector rhs) {
        import std.algorithm.mutation : copy;
        static if (op == "+") {
            auto result = doubleArray(32);
            copy(dims, result);
            foreach (i; 0..rhs.dims.length) {
                result[i] += rhs[i];
            }
            return Vector(result);
        } else if (op == "*") {
            auto result = doubleArray(32);
            foreach (i; 0..dims.length) {
                if (dims[i] != 0.0) {
                    foreach (j; 0..dims.length) {
                        if (rhs[j] != 0.0) {
                            auto s = reorderingSign(i, j) * dims[i] * rhs[j];
                            auto k = i ^ j;
                            result[k] += s;
                        }
                    }
                }
            }
            return Vector(result);
        } else {
            assert(false);
        }
    }

    Vector opBinary(string op : "*")(double scale) {
        auto result = dims.dup;
        foreach (i; 0..5) {
            dims[i] = dims[i] * scale;
        }
        return Vector(result);
    }

    double opIndex(size_t i) {
        return dims[i];
    }

    void opIndexAssign(double value, size_t i) {
        dims[i] = value;
    }
}

Vector e(int n) {
    enforce(n <= 4, "n must be less than 5");

    auto result = Vector(doubleArray(32));
    result[1 << n] = 1.0;
    return result;
}

Vector randomVector() {
    auto result = Vector(doubleArray(32));
    foreach (i; 0..5) {
        result = result + Vector([uniform01()]) * e(i);
    }
    return result;
}

Vector randomMultiVector() {
    auto result = Vector(doubleArray(32));
    foreach (i; 0..32) {
        result[i] = uniform01();
    }
    return result;
}

void main() {
    foreach (i; 0..5) {
        foreach (j; 0..5) {
            if (i < j) {
                if ((e(i).dot(e(j)))[0] != 0.0) {
                    writeln("Unexpected non-null scalar product.");
                    return;
                } else if (i == j) {
                    if ((e(i).dot(e(j)))[0] == 0.0) {
                        writeln("Unexpected null scalar product.");
                    }
                }
            }
        }
    }

    auto a = randomMultiVector();
    auto b = randomMultiVector();
    auto c = randomMultiVector();
    auto x = randomVector();

    // (ab)c == a(bc)
    writeln((a * b) * c);
    writeln(a * (b * c));
    writeln;

    // a(b+c) == ab + ac
    writeln(a * (b + c));
    writeln(a * b + a * c);
    writeln;

    // (a+b)c == ac + bc
    writeln((a + b) * c);
    writeln(a * c + b * c);
    writeln;

    // x^2 is real
    writeln(x * x);
}
```

```txt
Vector([-4.60357, -1.66951, 0.230125, -4.36372, 11.0032, 7.21226, -1.5373, -6.44947, -5.07115, -1.63098, 2.90828, 7.1582, -15.5565, -1.31705, 1.3186, -1.07552, -4.04055, -2.16556, -4.41229, 0.323326, 5.03127, -1.36494, -0.915379, -6.86147, -5.87756, -4.31528, 12.4005, 15.6349, -9.54983, -1.08376, 3.60886, 4.17844])
Vector([-4.60357, -1.66951, 0.230125, -4.36372, 11.0032, 7.21226, -1.5373, -6.44947, -5.07115, -1.63098, 2.90828, 7.1582, -15.5565, -1.31705, 1.3186, -1.07552, -4.04055, -2.16556, -4.41229, 0.323326, 5.03127, -1.36494, -0.915379, -6.86147, -5.87756, -4.31528, 12.4005, 15.6349, -9.54983, -1.08376, 3.60886, 4.17844])

Vector([-3.70178, 0.430038, -3.1952, -0.878759, 2.91374, 4.86224, 3.52303, 1.66396, -0.847575, -2.11591, 1.3121, 4.42268, -3.80298, -0.773252, 5.63781, 4.70647, -2.50968, 0.196386, -1.51296, 1.92306, 2.38331, 3.00232, 4.1991, -0.254381, 0.112317, 1.53895, 2.74983, 7.70699, 0.00697711, 0.785638, 7.04352, 3.94291])
Vector([-3.70178, 0.430038, -3.1952, -0.878759, 2.91374, 4.86224, 3.52303, 1.66396, -0.847575, -2.11591, 1.3121, 4.42268, -3.80298, -0.773252, 5.63781, 4.70647, -2.50968, 0.196386, -1.51296, 1.92306, 2.38331, 3.00232, 4.1991, -0.254381, 0.112317, 1.53895, 2.74983, 7.70699, 0.00697711, 0.785638, 7.04352, 3.94291])

Vector([-4.49501, 0.40722, -3.84594, -0.429832, 6.94562, 7.06166, 1.99871, 2.27987, 0.153668, 0.16586, 1.44596, 3.74806, -3.1458, -2.35553, 3.87377, 7.18143, -2.17737, 0.987571, -1.47633, 3.38, 2.24917, 2.68417, 1.81881, 0.886526, 1.28089, 1.63795, 7.06364, 8.61935, 2.78735, -0.226174, 3.83569, 2.1715])
Vector([-4.49501, 0.40722, -3.84594, -0.429832, 6.94562, 7.06166, 1.99871, 2.27987, 0.153668, 0.16586, 1.44596, 3.74806, -3.1458, -2.35553, 3.87377, 7.18143, -2.17737, 0.987571, -1.47633, 3.38, 2.24917, 2.68417, 1.81881, 0.886526, 1.28089, 1.63795, 7.06364, 8.61935, 2.78735, -0.226174, 3.83569, 2.1715])

Vector([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
```



## EchoLisp

We build a CGA based upon a generating quadratic form  in R^n. The implementation is general enough, that is ei*ei = +/- 1 , and not restricted to 1. The 5 dimension limit comes from the use of 32 bits numbers to generate all permutations 101... , but this could be improved. The multi-vector multiplication is based on a multiplication table 2^n * 2^n , generated once for all.


```scheme

(define e-bits (build-vector 32 (lambda(i) (arithmetic-shift 1 i)))) ;; 1,2,4,..
(define (e-index i) ;; index of ei in native vector
	(if (zero? i) 0 (arithmetic-shift 1 (1- i))))

(define DIM 0) ;; 2^N
(define N 0)
(define MultTable null) ;; multiplication table eijk * el.. = exyz..
(define SignTable null) ;; sign of products
(define Signature null) ;; input quadratic form

;; return "eijk"
(define( e-print E  sign )
	(string-append
		(cond ((= sign 1) " ") ((= sign -1) "- ") (else ""))
	  (if (zero? E) "1"
	  (for/string ((i  N))
	  #:continue (zero? (bitwise-and E (vector-ref e-bits  i)))
	  (string-append "e" (1+ i))))))

;; returns a string a *e1 + b*e2 + .. z*eijk + ..
(define (multi-print V (x))
	(for/string ((i DIM))
	(set! x (vector-ref V i))
	#:continue (zero? x)
	(string-append " " (if (> x 0) "+" "") x  "*" (e-print i 0))))


;; generates the multiplication table e_i e__k . * e_j e_l ..==> e_u e_v ...
;; E_I and E_J are sets of indices >=1 , increasing order,  represented by a 32 bits number

(define (make-mult-table (verbose #f) (result) (swaps) (ej))
(when verbose (writeln 'N= N 'DIM= DIM 'Q= Signature))
(for* ((E_I (in-range 1 DIM))(E_J (in-range 1 DIM)))
		(set! result E_I)
		(set! swaps 0)
		(for ((j DIM)) ; each bit# in E_J
		(set! ej (vector-ref e-bits j))
		#:continue (zero? (bitwise-and  ej E_J))

			(for((s (in-range (1- N) j -1))) ;; count swaps
				(when (!zero? (bitwise-and E_I (vector-ref e-bits s)))
					  (set! swaps (1+ swaps))))

		(if (zero? (bitwise-and E_I ej)) ;; e_i * e_j
		(set! result (bitwise-ior result ej))
		(begin ;; else e_i * e_i
		(set! result (bitwise-xor result ej))
		(when (= -1 (vector-ref Signature ej)) (set! swaps (1+ swaps)))
		))) ;; j loop

		(when verbose (writeln  (e-print E_I 0) '* (e-print E_J 0)
				'= (e-print result  (if (even? swaps) 1 -1))))

		(matrix-set! MultTable E_I E_J result)
		(matrix-set! SignTable E_I E_J (if (even? swaps) 1 -1))
		))

;; multivector operations
;; addition is standard vector addition
;; multiplication a  b -> c
(define (multi-mult  a b)
	(define c (make-vector DIM 0))
	(for* ((i DIM) (j DIM))
		#:continue (zero? (vector-ref a i))
		#:continue (zero? (vector-ref b j))
		(vector-set! c
			(array-ref MultTable i j)
			(+
				(* (array-ref  SignTable i j) (vector-ref a i) (vector-ref b j))
				(vector-ref c (array-ref MultTable i j)))))
		c)

;; pretty print  a • b or a • b • c
(define ( • a b (c #f))
	(multi-print
	(if c  (multi-mult a (multi-mult b c)) (multi-mult a b))))


;; (Eij i j) ->  return multi-vector eiej 0 <= i <= n
(define (Eij i j (coeff 1))
	(define Eij (make-vector DIM))
	(vector-set! Eij (array-ref MultTable (e-index i) (e-index j)) coeff)
	Eij)


;; Reference : https://en.wikipedia.org/wiki/Clifford_algebra#Real_numbers

;; (make-cga  m p [verbose])  => Algebra A(m p)
;; Input : a quadratic form Q(x) =  x1*x1 + + xm*xm - xm+1*xm+1 - xm+p*xm+p
;; n = m + p = dimension of vector space R^n
;; generates an algebra A(m p) of dimension  DIM  = 2^n
;; Ex : A(n 0) = use R^n dot product as quadratic form : ei*ei = 1
;; Ex : A (0 1) = Complex , e1*e1 = -1 ;  A(0 2) => quaternions ei*ei = -1
;;
;; Implementation
;; limitation n <= 5
;; multivectors of A(m p) will be mapped on Vectors  V of dimension 2^n
;; V[0] is the scalar part of a multivector.
;; Blade of vectors of R^n :  :V[2^(i-1)] = 1 , 0 elsewhere , i in [1 ..n]

(define (make-cga m p (verbose #f))
(string-delimiter "")
	(set! N (+ m p))
	(set! DIM (expt 2 N))
	(set! MultTable (build-array DIM DIM (lambda(i j) (cond ((zero? i) j)((zero? j) i)(else 0)))))
	(set! SignTable (make-array DIM DIM 1))
	(set! Signature (make-vector DIM 1)) ;; Q polynomial
	(for ((j (in-range m N))) (vector-set! Signature (vector-ref e-bits j) -1))

	(make-mult-table verbose) DIM )



```

```scheme

;; we use indices (1 ... n) in conformity with the Wikipedia reference

;; dimension 2
;; (1 + e1e2) • (e1 -2e2) = -e1 -3e2
(make-cga 2 0)
(define u #(1 0 0 1))
(define v #(0 1 -2 0))

(multi-print u) → +1*1 +1*e1e2
(multi-print v) → +1*e1 -2*e2
(• u v)         → -1*e1 -3*e2

;; task
(make-cga 5 0)
(define X #(0 1 -1 0 2 0 0 0 -3 0 0 0 0 0 0 0 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ))
(multi-print X) → +1*e1 -1*e2 +2*e3 -3*e4 -2*e5
(• X X)  →  +19*1

; with another polynomial
(make-cga 0 5)
Signature → #( 1 -1 -1 1 -1 1 1 1 -1 1 1 1 1 1 1 1 -1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
(• X X) → -19*1

;
(make-cga 4 0)
(define i (Eij 1 2))
(define j (Eij 2 3))
(define k (Eij 1 3))

(multi-print i) → +1*e1e2
(multi-print j) → +1*e2e3
(multi-print k)]→ +1*e1e3
(• i i) → -1*1
(• j j) → -1*1
(• k k) → -1*1
(• i j k) → -1*1

(define I (Eij 2 3))
😖️ error: define : cannot redefine : I (used in Complex) ;; use II instead

(define II (Eij 2 3)) → +1*e2e3
(define J (Eij 3 4)) → +1*e3e4
(define K (Eij 2 4)) → +1*e2e4

(• II II) → -1*1
(• J J)  → -1*1
(• K K)  → -1*1
(• II J K) → -1*1


```

Multiplication table for A(3 0)

N=     3     DIM=     8     Q=     #( 1 1 1 1 1 1 1 1)

     <b>e</b>1     *     <b>e</b>1     =     <b>1</b>

     <b>e</b>1     *     <b>e</b>2     =     <b>e</b>1<b>e</b>2

     <b>e</b>1     *     <b>e</b>1<b>e</b>2     =     <b>e</b>2

     <b>e</b>1     *     <b>e</b>3     =     <b>e</b>1<b>e</b>3

     <b>e</b>1     *     <b>e</b>1<b>e</b>3     =     <b>e</b>3

     <b>e</b>1     *     <b>e</b>2<b>e</b>3     =     <b>e</b>1<b>e</b>2<b>e</b>3

     <b>e</b>1     *     <b>e</b>1<b>e</b>2<b>e</b>3     =     <b>e</b>2<b>e</b>3

     <b>e</b>2     *     <b>e</b>1     =     - <b>e</b>1<b>e</b>2

     <b>e</b>2     *     <b>e</b>2     =     <b>1</b>

     <b>e</b>2     *     <b>e</b>1<b>e</b>2     =     - <b>e</b>1

     <b>e</b>2     *     <b>e</b>3     =     <b>e</b>2<b>e</b>3

     <b>e</b>2     *     <b>e</b>1<b>e</b>3     =     - <b>e</b>1<b>e</b>2<b>e</b>3

     <b>e</b>2     *     <b>e</b>2<b>e</b>3     =     <b>e</b>3

     <b>e</b>2     *     <b>e</b>1<b>e</b>2<b>e</b>3     =     - <b>e</b>1<b>e</b>3

     <b>e</b>1<b>e</b>2     *     <b>e</b>1     =     - <b>e</b>2

     <b>e</b>1<b>e</b>2     *     <b>e</b>2     =     <b>e</b>1

     <b>e</b>1<b>e</b>2     *     <b>e</b>1<b>e</b>2     =     - <b>1</b>

     <b>e</b>1<b>e</b>2     *     <b>e</b>3     =     <b>e</b>1<b>e</b>2<b>e</b>3

     <b>e</b>1<b>e</b>2     *     <b>e</b>1<b>e</b>3     =     - <b>e</b>2<b>e</b>3

     <b>e</b>1<b>e</b>2     *     <b>e</b>2<b>e</b>3     =     <b>e</b>1<b>e</b>3

     <b>e</b>1<b>e</b>2     *     <b>e</b>1<b>e</b>2<b>e</b>3     =     - <b>e</b>3

     <b>e</b>3     *     <b>e</b>1     =     - <b>e</b>1<b>e</b>3

     <b>e</b>3     *     <b>e</b>2     =     - <b>e</b>2<b>e</b>3

     <b>e</b>3     *     <b>e</b>1<b>e</b>2     =     <b>e</b>1<b>e</b>2<b>e</b>3

     <b>e</b>3     *     <b>e</b>3     =     <b>1</b>

     <b>e</b>3     *     <b>e</b>1<b>e</b>3     =     - <b>e</b>1

     <b>e</b>3     *     <b>e</b>2<b>e</b>3     =     - <b>e</b>2

     <b>e</b>3     *     <b>e</b>1<b>e</b>2<b>e</b>3     =     <b>e</b>1<b>e</b>2

     <b>e</b>1<b>e</b>3     *     <b>e</b>1     =     - <b>e</b>3

     <b>e</b>1<b>e</b>3     *     <b>e</b>2     =     - <b>e</b>1<b>e</b>2<b>e</b>3

     <b>e</b>1<b>e</b>3     *     <b>e</b>1<b>e</b>2     =     <b>e</b>2<b>e</b>3

     <b>e</b>1<b>e</b>3     *     <b>e</b>3     =     <b>e</b>1

     <b>e</b>1<b>e</b>3     *     <b>e</b>1<b>e</b>3     =     - <b>1</b>

     <b>e</b>1<b>e</b>3     *     <b>e</b>2<b>e</b>3     =     - <b>e</b>1<b>e</b>2

     <b>e</b>1<b>e</b>3     *     <b>e</b>1<b>e</b>2<b>e</b>3     =     <b>e</b>2

     <b>e</b>2<b>e</b>3     *     <b>e</b>1     =     <b>e</b>1<b>e</b>2<b>e</b>3

     <b>e</b>2<b>e</b>3     *     <b>e</b>2     =     - <b>e</b>3

     <b>e</b>2<b>e</b>3     *     <b>e</b>1<b>e</b>2     =     - <b>e</b>1<b>e</b>3

     <b>e</b>2<b>e</b>3     *     <b>e</b>3     =     <b>e</b>2

     <b>e</b>2<b>e</b>3     *     <b>e</b>1<b>e</b>3     =     <b>e</b>1<b>e</b>2

     <b>e</b>2<b>e</b>3     *     <b>e</b>2<b>e</b>3     =     - <b>1</b>

     <b>e</b>2<b>e</b>3     *     <b>e</b>1<b>e</b>2<b>e</b>3     =     - <b>e</b>1

     <b>e</b>1<b>e</b>2<b>e</b>3     *     <b>e</b>1     =     <b>e</b>2<b>e</b>3

     <b>e</b>1<b>e</b>2<b>e</b>3     *     <b>e</b>2     =     - <b>e</b>1<b>e</b>3

     <b>e</b>1<b>e</b>2<b>e</b>3     *     <b>e</b>1<b>e</b>2     =     - <b>e</b>3

     <b>e</b>1<b>e</b>2<b>e</b>3     *     <b>e</b>3     =     <b>e</b>1<b>e</b>2

     <b>e</b>1<b>e</b>2<b>e</b>3     *     <b>e</b>1<b>e</b>3     =     <b>e</b>2

     <b>e</b>1<b>e</b>2<b>e</b>3     *     <b>e</b>2<b>e</b>3     =     - <b>e</b>1

     <b>e</b>1<b>e</b>2<b>e</b>3     *     <b>e</b>1<b>e</b>2<b>e</b>3     =     - <b>1</b>


## Go

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

type vector []float64

func e(n uint) vector {
    if n > 4 {
        panic("n must be less than 5")
    }
    result := make(vector, 32)
    result[1<<n] = 1.0
    return result
}

func cdot(a, b vector) vector {
    return mul(vector{0.5}, add(mul(a, b), mul(b, a)))
}

func neg(x vector) vector {
    return mul(vector{-1}, x)
}

func bitCount(i int) int {
    i = i - ((i >> 1) & 0x55555555)
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333)
    i = (i + (i >> 4)) & 0x0F0F0F0F
    i = i + (i >> 8)
    i = i + (i >> 16)
    return i & 0x0000003F
}

func reorderingSign(i, j int) float64 {
    i >>= 1
    sum := 0
    for i != 0 {
        sum += bitCount(i & j)
        i >>= 1
    }
    cond := (sum & 1) == 0
    if cond {
        return 1.0
    }
    return -1.0
}

func add(a, b vector) vector {
    result := make(vector, 32)
    copy(result, a)
    for i, _ := range b {
        result[i] += b[i]
    }
    return result
}

func mul(a, b vector) vector {
    result := make(vector, 32)
    for i, _ := range a {
        if a[i] != 0 {
            for j, _ := range b {
                if b[j] != 0 {
                    s := reorderingSign(i, j) * a[i] * b[j]
                    k := i ^ j
                    result[k] += s
                }
            }
        }
    }
    return result
}

func randomVector() vector {
    result := make(vector, 32)
    for i := uint(0); i < 5; i++ {
        result = add(result, mul(vector{rand.Float64()}, e(i)))
    }
    return result
}

func randomMultiVector() vector {
    result := make(vector, 32)
    for i := 0; i < 32; i++ {
        result[i] = rand.Float64()
    }
    return result
}

func main() {
    rand.Seed(time.Now().UnixNano())
    for i := uint(0); i < 5; i++ {
        for j := uint(0); j < 5; j++ {
            if i < j {
                if cdot(e(i), e(j))[0] != 0 {
                    fmt.Println("Unexpected non-null scalar product.")
                    return
                }
            } else if i == j {
                if cdot(e(i), e(j))[0] == 0 {
                    fmt.Println("Unexpected null scalar product.")
                }
            }
        }
    }

    a := randomMultiVector()
    b := randomMultiVector()
    c := randomMultiVector()
    x := randomVector()

    // (ab)c == a(bc)
    fmt.Println(mul(mul(a, b), c))
    fmt.Println(mul(a, mul(b, c)))

    // a(b + c) == ab + ac
    fmt.Println(mul(a, add(b, c)))
    fmt.Println(add(mul(a, b), mul(a, c)))

    // (a + b)c == ac + bc
    fmt.Println(mul(add(a, b), c))
    fmt.Println(add(mul(a, c), mul(b, c)))

    // x² is real
    fmt.Println(mul(x, x))
}
```


Sample output:

```txt

[1.6282881498413662 0.2490818261523896 -0.8936755921478269 -1.555477163901491 6.47589688756284 8.705633181089887 -1.28416798750558 -3.0984267307080446 -14.384954438859133 -13.511137120485879 6.071767421804147 1.099627765550034 -0.40641746354718655 -3.3593459129408076 -2.8089033176352967 -3.003641914720827 4.223517526463662 5.7807271315990185 -4.921271185053852 -5.698203073886508 -0.5449956221104395 3.2199941835007997 -0.4168598688210261 -1.6164380014352773 -13.447900615475964 -11.892642419707807 5.484071302025009 2.781324432176212 5.237445180167182 0.4643791234551212 -7.986755945938485 -2.9272187129576714]
[1.628288149841367 0.24908182615239083 -0.8936755921478254 -1.5554771639014895 6.475896887562836 8.705633181089889 -1.2841679875055805 -3.0984267307080433 -14.384954438859129 -13.51113712048588 6.071767421804145 1.099627765550032 -0.40641746354718755 -3.359345912940806 -2.8089033176352958 -3.003641914720825 4.223517526463663 5.7807271315990185 -4.921271185053855 -5.698203073886506 -0.5449956221104393 3.2199941835008032 -0.4168598688210253 -1.6164380014352775 -13.44790061547596 -11.89264241970781 5.484071302025003 2.781324432176209 5.237445180167183 0.464379123455121 -7.986755945938484 -2.9272187129576706]
[-4.652496095413123 -6.651741805769786 -0.18044192849719706 -1.118756694503706 -1.4545868044605725 0.10199724090664991 0.5018587820915257 2.3004721822960024 -1.813996268087529 0.38357415506855985 7.882236705126414 4.377167004918281 0.338317137066833 1.0631923204534859 5.08861779773926 4.611434580371178 -5.277764644396049 -7.961720991272197 -1.27063408303169 -1.2002120748969933 -0.3251154212726659 2.005000622005424 1.0505371909084391 1.9822320823801767 -2.271503682913346 -0.2403902213900877 6.6269980604812275 4.006018365857085 -0.15074367863748028 -0.48557428903338595 5.291057793190274 2.7751733146879394]
[-4.652496095413124 -6.651741805769786 -0.18044192849719662 -1.1187566945037064 -1.454586804460573 0.10199724090665008 0.5018587820915265 2.3004721822960037 -1.8139962680875295 0.38357415506856163 7.882236705126415 4.377167004918281 0.338317137066834 1.0631923204534859 5.088617797739261 4.611434580371178 -5.277764644396049 -7.9617209912721965 -1.2706340830316898 -1.200212074896994 -0.3251154212726654 2.0050006220054257 1.050537190908438 1.9822320823801762 -2.2715036829133437 -0.24039022139008648 6.626998060481226 4.006018365857084 -0.15074367863747984 -0.4855742890333855 5.291057793190275 2.7751733146879403]
[-4.682894668443622 -7.686899263290272 -0.2500585680601418 -0.8779897639638435 2.579501108403806 2.901924320921563 -0.4542430696469483 1.0374201754917136 -2.588607371991848 -3.229485794697976 7.444924967244714 4.93089687101153 -2.0124785310408284 0.46939350205418884 6.568153651157447 5.710192967946121 -1.41530169954937 -4.536345225213684 0.5240731948596796 1.5123734256564463 1.767786974613905 2.9960861930917018 0.969306657461082 1.036924529835111 -1.456640437477983 -1.3085896429723467 2.9678738261068895 1.3051596528834055 -3.8197616441989037 -1.3030506523824616 4.7818448875243895 4.122121121578954]
[-4.682894668443622 -7.686899263290271 -0.2500585680601421 -0.877989763963844 2.5795011084038055 2.9019243209215633 -0.4542430696469487 1.0374201754917127 -2.5886073719918494 -3.2294857946979736 7.444924967244715 4.93089687101153 -2.0124785310408275 0.46939350205418884 6.568153651157447 5.710192967946121 -1.415301699549369 -4.536345225213685 0.5240731948596802 1.512373425656447 1.7677869746139057 2.9960861930917013 0.9693066574610807 1.036924529835109 -1.456640437477983 -1.3085896429723465 2.9678738261068895 1.305159652883405 -3.819761644198904 -1.3030506523824616 4.781844887524388 4.122121121578957]
[2.1206022437357284 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]

```



## J


Sparse arrays give better performance for this task, than dense arrays, and support relatively arbitrary dimensions, but can be a bit quirky because current implementations of J do not support some features for sparse arrays. We add multivectors  x and y using <code>x + y</code>.  Also, the first element of one of these multivectors represents the "real valued" or "scalar component" of the multivector.

Implementation:


```J
NB. indices are signed machine integers
vzero=: 1 $.2^31+IF64*32
odim=. 2^.#vzero

ndx01=:1 :0
:
  NB. indexed update of numeric rank 1 sparse y
  NB. creating rank 2 sparse result
  NB. using scalar values from x and scalar inds from m
  NB. where x, m are rank 0 or 1
  NB. (this works around a spurious error in sparse handling)
  n=. #x,.m
  x ((i.n),&.> m)} n#,:y
)

NB. specify that all axes are sparse, for better display
clean=: (2;i.@#@$) $. ]

gmul=:4 :0"1
  xj=. ,4$.x
  yj=. ,4$.y
  if. 0= xj *&# yj do. vzero return. end.
  b=. (-##:>./0,xj,yj)&{."1@#:
  xb=. b xj
  yb=. b yj
  rj=. ,#.xb~:"1/yb
  s=. ,_1^ ~:/"1 yb *"1/ 0,.}:"1 ~:/\"1 xb
  vzero (~.rj)}~ rj +//. s*,(xj{x)*/yj{y
)

gdot=: (gmul + gmul~) % 2:

obasis=:1 (2^i.odim)ndx01 vzero
e=: {&obasis
```


Explanation:

We work with sparse vectors of length 2147483647 on 32 bit machines and of length 9223372036854775807 on 64 bit machines. These are the largest representable vector lengths in current J implementations. J allows [http://www.jsoftware.com/help/learning/06.htm negative indices] and J uses an index value corresponding to the length of the list to indicate value not found when [[j:Vocabulary/idot#dyadic|searching]]. Thus, current J implementations use signed machine integers for performance and correctness reasons and these are the largest vector lengths we can use in J.

Except, for the purpose of this task, we must pretend that these are "multivectors" instead of vectors - task vectors have a length of log2 the multivector length. So technically speaking, we can only represent 30 element (or less) vectors on 32 bit J and 62 element (or less) vectors on 64 bit J. (Actually, we can represent 31 element vectors on 32 bit J and 63 bit vectors on 64 bit J, but there are hypothetical operations involving the last element which perhaps would be hindered by the fact that the multivector length is not 2147483648 or 9223372036854775808 -- but fortunately, none of this is actually relevant.)

Since these multivectors are silly large, and almost all zeros, we use a sparse representation and only concern ourselves with non-zero values, and their indices.

For addition on multivectors we use J's <code>+</code> (and "multivectors" includes "task vectors").

For "geometric multiplication" (whose relationship to geometry seems obscure, at best) we define something analogous to an [[wp:Dot_product|inner product]]. More specifically, we look at the base 2 representation of the argument indices to find the result index  and to find whether we negate the product of those two values.

For each pair of values in the left and right argument we use the base 2 representation of the indices to determine what to do with the product of the corresponding two non-zero values:

* The result index for that product is the bit-wise [[wp:Exclusive_or|exclusive-or]] of the two indices (the corresponding bit in the result index is 1 when the corresponding bits in the argument indices are different, 0 otherwise).

* We negate the product depending on the bits of the left and right argument index. For each 1 bit of the right argument index, we count the number of more significant bits in the left argument index. If this total is odd, we negate the product (but not if it's even). Note that we don't actually have to form a sum to do this - exclusive-or is an adequate replacement for sum if we only need to know whether it's odd or even.

Where we have more than one non-zero value pair contributing to the same product index we sum those values. (Of course, this would apply to value pairs whose result is zero, but since typically there's something approaching 85070591730234615847396907784232501249 of those, we don't actually compute all those zeros...)

Task examples:


```J
   NB. test arbitrary vector being real (and having the specified result)
   clean gmul~ +/ (e 0 1 2 3 4) gmul 1 _1 2 3 _2 (0 ndx01) vzero
0 │ 19

   NB. required orthogonality
   clean gdot&e&>/~i.4
0 0 0 │ 1
1 1 0 │ 1
2 2 0 │ 1
3 3 0 │ 1

   NB. i j k
   i=: 0 gmul&e 1
   j=: 1 gmul&e 2
   k=: 0 gmul&e 2

   i gmul i
0 │ _1
   j gmul j
0 │ _1
   k gmul k
0 │ _1
   i gmul j gmul k
0 │ _1

   NB. I J K
   I=: 1 gmul&e 2
   J=: 2 gmul&e 3
   K=: 1 gmul&e 3

   I gmul I
0 │ _1
   J gmul J
0 │ _1
   K gmul K
0 │ _1
   I gmul J gmul K
0 │ _1
   K-J
10 │  1
12 │ _1
   I gmul J+K
10 │  1
12 │ _1
```


Note that sparse arrays display as <code>indices | value</code> for elements which are not the default element (0).

So, for example in the part where we check for orthogonality, we are forming a 5 by 5 by 9223372036854775807 array. The first two dimensions correspond to arguments to <code>e</code> and the final dimension is the multivector dimension. A <code>0</code> in the multivector dimension means that that's the "real" or "scalar" part of the multivector. And, since the pair of dimensions for the <code>e</code> arguments whose "dot" product are <code>1</code> are identical, we know we have an [[wp:Identity_matrix|identity matrix]].


## javascript


```javascript
var GA = function () {
    function e(n) {
	var result = [];
	result[1 << n] = 1;
	return result;
    }
    function cdot(a, b) { return mul([0.5], add(mul(a, b), mul(b, a))) }
    function neg(x) { return mul([-1], x) }
    function bitCount(i) {
	// Note that unsigned shifting (>>>) is not required.
	i = i - ((i >> 1) & 0x55555555);
	i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
	i = (i + (i >> 4)) & 0x0F0F0F0F;
	i = i + (i >> 8);
	i = i + (i >> 16);
	return i & 0x0000003F;
    }
    function reorderingSign(a, b) {
	a >>= 1;
	var sum = 0;
	while (a != 0) {
	    sum += bitCount(a & b);
	    a >>= 1;
	}
	return (sum & 1) == 0 ? 1 : -1;
    }
    function add(a, b) {
	var result = a.slice(0);
	for (var i in b) {
	    if (result[i]) {
		result[i] += b[i];
	    } else {
		result[i] = b[i];
	    }
	}
	return result;
    }
    function mul(a, b)
    {
	var result = [];
	for (var i in a) {
	    if (a[i]) {
		for (var j in b) {
		    if (b[j]) {
			var s = reorderingSign(i, j) * a[i] * b[j];
			// if (i == 1 && j == 1) { s *= -1 }  // e0*e0 == -1
			var k = i ^ j;
			if (result[k]) {
			    result[k] += s;
			} else {
			    result[k] = s;
			}
		    }
		}
	    }
	}
	return result;
    }
    return {
	e   : e,
	cdot : cdot,
	neg : neg,
	add : add,
	mul : mul
    };
}();
```


And then, from the console:


```javascript
var e = GA.e, cdot = GA.cdot;

for (var i = 0; i < 5; i++) {
    for (var j = 0; j < 5; j++) {
        if (i < j) {
            if (cdot(e(i), e(j))[0]) { console.log("unexpected non-nul scalar product"); }
        } else if (i === j) {
            if (!cdot(e(i), e(j))[0]) { console.log("unexpected nul scalar product"); }
        }
    }
}

function randomVector() {
    var result = [];
    for (var i = 0; i < 5; i++) { result = GA.add( result, GA.mul([Math.random()], e(i))); }
    return result;
}
function randomMultiVector() {
    var result = [];
    for (var i = 0; i < 32; i++) { result[i] = Math.random(); }
    return result;
}

var a = randomMultiVector(), b = randomMultiVector(), c = randomMultiVector();
var x = randomVector();

// (ab)c == a(bc)
console.log(GA.mul(GA.mul(a, b), c));
console.log(GA.mul(a, GA.mul(b, c)));

// a(b + c) == ab + ac
console.log(GA.mul(a, GA.add(b, c)));
console.log(GA.add(GA.mul(a,b), GA.mul(a, c)));

// (a + b)c == ac + bc
console.log(GA.mul(GA.add(a, b), c));
console.log(GA.add(GA.mul(a,c), GA.mul(b, c)));

// x² is real
console.log(GA.mul(x, x));
```

```txt
[-7.834854130554672, -10.179405417124476, 5.696414143584243, -1.4014556169803851, 12.334288331422336, 11.690738709598888, -0.4279888274147221, 6.226618790084965, -10.904144874917206, -5.46919448234424, -5.647472225071031, -2.9801969751721744, -8.284532508545746, -3.3280413654836494, -2.2182526412098493, 0.4191036292473347, 3.0485450100607103, -0.20619687045226742, 2.1369938048939527, 3.730913391951158, 10.929856967963905, 8.301187183717643, -4.874133827873075, 0.7918650606624789, -8.520661635525103, -7.732342981599732, -6.494750491582618, -2.458749173402162, 3.573788336699224, 2.784339193089742, -1.6479372032388944, -0.35120747879544256]
[-7.83485413055467, -10.179405417124475, 5.696414143584248, -1.4014556169803827, 12.334288331422337, 11.690738709598893, -0.4279888274147213, 6.226618790084964, -10.90414487491721, -5.46919448234424, -5.647472225071032, -2.9801969751721726, -8.284532508545746, -3.3280413654836507, -2.218252641209847, 0.41910362924733874, 3.048545010060707, -0.20619687045226748, 2.136993804893955, 3.7309133919511575, 10.929856967963904, 8.301187183717648, -4.8741338278730755, 0.7918650606624811, -8.520661635525107, -7.732342981599734, -6.494750491582625, -2.45874917340216, 3.5737883366992262, 2.7843391930897443, -1.6479372032388935, -0.351207478795442]
[-4.5157935996060425, -3.9762419076273514, -2.653425845411889, -1.2899302330562412, 6.161562884801266, 3.664812215240675, -0.4471521091019873, 2.39303455739218, -1.6486347268701103, 1.156714478904937, 4.5859158357958965, 6.879356425817299, 1.3341425863947358, 5.641350122882839, 6.378155334673649, 6.466962714879142, -3.645688408496504, -1.9659188980662032, 1.3062519818876646, 1.7973392350972788, 2.4770203476100843, 1.258017836002405, 1.3794942194985413, 3.993871627961031, -3.3620439843097127, -0.4228490927003264, 0.27245046364398495, 3.813642689561589, 2.6785051915908604, 5.409359105713415, 2.9578168177883555, 4.425426168284635]
[-4.515793599606042, -3.976241907627351, -2.653425845411889, -1.2899302330562417, 6.161562884801263, 3.664812215240676, -0.44715210910198766, 2.393034557392179, -1.6486347268701103, 1.156714478904937, 4.585915835795897, 6.8793564258172974, 1.3341425863947352, 5.641350122882839, 6.378155334673649, 6.466962714879143, -3.645688408496502, -1.9659188980662032, 1.3062519818876661, 1.7973392350972783, 2.4770203476100843, 1.258017836002407, 1.379494219498544, 3.99387162796103, -3.3620439843097127, -0.42284909270032545, 0.2724504636439853, 3.8136426895615894, 2.67850519159086, 5.409359105713415, 2.9578168177883555, 4.425426168284636]
[-5.8903316026132755, -6.619647679486295, -1.8140191326116537, -2.519531799741982, 6.604158362571294, 6.352401943423508, 0.9412086471616096, 3.719341486246096, -2.209111542028446, 1.9980997124233557, 5.717878641652222, 7.351597777237362, -2.9037939632499974, 1.497897713658653, 6.811544238648882, 5.861907187665564, -3.2638975880372363, -2.2659714695119115, 1.227221599808634, 0.8343365341022846, 2.72461491531054, 2.728833585944902, 2.226404227376565, 3.888097816250177, 0.35867175462798684, 2.3965356477571302, -1.7151608532791172, 1.403673323043394, -2.1441532262277607, 2.5435142440445646, 2.00110597707534, 1.9825972651495558]
[-5.8903316026132755, -6.6196476794862935, -1.8140191326116533, -2.5195317997419817, 6.604158362571292, 6.352401943423505, 0.9412086471616091, 3.719341486246094, -2.209111542028446, 1.9980997124233555, 5.71787864165222, 7.351597777237364, -2.9037939632499974, 1.4978977136586529, 6.81154423864888, 5.861907187665565, -3.263897588037235, -2.2659714695119124, 1.2272215998086353, 0.8343365341022843, 2.72461491531054, 2.7288335859449018, 2.226404227376565, 3.8880978162501783, 0.3586717546279864, 2.396535647757131, -1.715160853279117, 1.4036733230433938, -2.1441532262277603, 2.543514244044564, 2.0011059770753405, 1.9825972651495565]
[3.193752260485546, 3: 0, 5: 0, 6: 0, 9: 0, 10: 0, 12: 0, 17: 0, 18: 0, 20: 0, 24: 0]
```



## Jsish

From Javascript.


```javascript
/* Geometric Algebra, in Jsih */

var GA = function () {
    function e(n) {
        var result = [];
        result[1 << n] = 1;
        return result;
    }
    function cdot(a, b) { return mul([0.5], add(mul(a, b), mul(b, a))); }
    function neg(x) { return mul([-1], x); }
    function bitCount(i) {
        // Note that unsigned shifting (>>>) is not required.
        i = i - ((i >> 1) & 0x55555555);
        i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
        i = (i + (i >> 4)) & 0x0F0F0F0F;
        i = i + (i >> 8);
        i = i + (i >> 16);
        return i & 0x0000003F;
    }
    function reorderingSign(a, b) {
        a >>= 1;
        var sum = 0;
        while (a != 0) {
            sum += bitCount(a & b);
            a >>= 1;
        }
        return (sum & 1) == 0 ? 1 : -1;
    }
    function add(a, b) {
        var result = a.slice(0);
        for (var i in b) {
            if (result[i]) result[i] += b[i]; else result[i] = b[i];
        }
        return result;
    }
    function mul(a, b) {
        var result = [];
        for (var i in a) {
            if (a[i]) {
                for (var j in b) {
                    if (b[j]) {
                        var s = reorderingSign(i, j) * a[i] * b[j];
                        // if (i == 1 && j == 1) { s *= -1 }  // e0*e0 == -1
                        var k = i ^ j;
                        if (result[k]) result[k] += s; else result[k] = s;
                    }
                }
            }
        }
        for (var i = 0; i < result.length; i++) result[i] = (result[i]) ? result[i] : 0;
        return result;
    }

    return {
        e:e,
        cdot:cdot,
        neg:neg,
        add:add,
        mul:mul
    };
}();

if (Interp.conf('unitTest')) {
    var e = GA.e, cdot = GA.cdot;

    for (var i = 0; i < 5; i++) {
        for (var j = 0; j < 5; j++) {
            if (i < j) {
                if (cdot(e(i), e(j))[0]) { console.log("unexpected non-nul scalar product"); }
            } else if (i === j) {
                if (!cdot(e(i), e(j))[0]) { console.log("unexpected nul scalar product"); }
            }
        }
    }

    function randomVector() {
        var result = new Array(32).fill(0);
        for (var i = 0; i < 5; i++) { result = GA.add( result, GA.mul([Math.random()], e(i))); }
        return result;
    }
    function randomMultiVector() {
        var result = new Array(32).fill(0);
        for (var i = 0; i < 32; i++) { result[i] = Math.random(); }
        return result;
    }

    Math.srand(0);
    var a = randomMultiVector(), b = randomMultiVector(), c = randomMultiVector();
    var x = randomVector();

;    a;
;    b;
;    c;

    // (ab)c == a(bc)
;    GA.mul(GA.mul(a, b), c);
;    GA.mul(a, GA.mul(b, c));

    // a(b + c) == ab + ac
;    GA.mul(a, GA.add(b, c));
;    GA.add(GA.mul(a,b), GA.mul(a, c));

    // (a + b)c == ac + bc
;    GA.mul(GA.add(a, b), c);
;    GA.add(GA.mul(a,c), GA.mul(b, c));


    // x² is real
;    x;
;    GA.mul(x, x);
}

/*
=!EXPECTSTART!=
a ==> [ 0.1708280361062897, 0.7499019804849638, 0.09637165562356742, 0.8704652270270756, 0.5773035067951078, 0.7857992588396741, 0.6921941534586402, 0.3687662699204211, 0.8739040768618089, 0.7450950984500651, 0.4460459090931117, 0.3537282030933753, 0.7325196320025391, 0.2602220010828802, 0.3942937749238773, 0.7767899512256164, 0.845035137580286, 0.5757882004827763, 0.7155385951686632, 0.08300424607387669, 0.4558251286597574, 0.1099468141806454, 0.5452280238165734, 0.3906865706486755, 0.5685854232144472, 0.9590664494883754, 0.8677190964591368, 0.1631895102523586, 0.2755089268683157, 0.2603610948720672, 0.9240947418691654, 0.435922637102685 ]
b ==> [ 0.7894608655520905, 0.1276170168117403, 0.08220568604043521, 0.9406420164478462, 0.02557492625301805, 0.1542109313278246, 0.382182425278156, 0.1547366996666923, 0.5293334181169804, 0.8768484910832512, 0.4306114383383992, 0.2639062263420797, 0.313594499023214, 0.7700916858547231, 0.107390883054105, 0.7710422551956455, 0.7051955588944487, 0.2186396587077581, 0.7617939928559956, 0.4117130455789564, 0.648826822929113, 0.929956254907367, 0.5024185655986706, 0.6874406794288674, 0.4360909481473279, 0.6083009079497401, 0.5765586336353685, 0.6326217107140693, 0.4634256476287426, 0.6322437896100865, 0.1382949326493268, 0.9607614179251911 ]
c ==> [ 0.1443750010878411, 0.4466830710677812, 0.3245844598453793, 0.9525840775924692, 0.358183910481177, 0.3982082224922436, 0.1012815422323818, 0.9550857356528795, 0.9846169476710749, 0.5759700814514872, 0.8659137353852593, 0.1498758912327744, 0.9091504302816595, 0.6512525086400416, 0.06386085476492198, 0.9549993865624558, 0.9662627642756583, 0.7855433852139697, 0.8051648178950011, 0.5712536797957526, 0.2825857199542448, 0.9625510519679636, 0.5794770589463063, 0.4368586269705688, 0.3754361745372741, 0.9234076091930206, 0.02869938006311301, 0.7688599777155005, 0.7231067717175854, 0.5909693498129656, 0.4258885385798372, 0.6421430590830184 ]
GA.mul(GA.mul(a, b), c) ==> [ 1.541918542712994, 4.115829300591508, 6.756686908265433, 1.237835072374462, 10.35373455425321, 15.27788368572946, -10.02377587769663, -7.963603479812443, -4.551812682204639, -2.063312081493453, 6.501355951540042, 7.264555440540969, -8.486452071298562, -4.822494555299121, -4.759231263111541, -10.77800945200822, -7.362166274344194, -4.396761648257096, -4.546442029824131, -5.48725057173969, 7.12307450722373, 12.97562340826319, -15.0237414847673, -14.51557782429663, -4.883430414017697, -4.901409415740756, 7.906069132188758, 6.734848434665599, -9.783158191139472, -6.519424552890571, -2.139880496917612, -9.858645000418766 ]
GA.mul(a, GA.mul(b, c)) ==> [ 1.541918542712995, 4.11582930059151, 6.756686908265435, 1.23783507237446, 10.35373455425321, 15.27788368572945, -10.02377587769663, -7.963603479812441, -4.551812682204639, -2.06331208149345, 6.501355951540043, 7.264555440540969, -8.486452071298562, -4.822494555299119, -4.759231263111545, -10.77800945200822, -7.362166274344201, -4.396761648257097, -4.546442029824129, -5.48725057173969, 7.12307450722373, 12.97562340826319, -15.0237414847673, -14.51557782429663, -4.883430414017695, -4.901409415740756, 7.906069132188759, 6.734848434665598, -9.783158191139472, -6.519424552890572, -2.139880496917614, -9.858645000418766 ]
GA.mul(a, GA.add(b, c)) ==> [ -4.618544334910429, -1.861355248134714, -1.344616248842651, -0.2493092523303422, 2.805076632862419, 7.011456497501809, -2.595903465496111, -2.470668530528046, 2.219481357349965, 1.819582156863502, 7.750350045338696, 7.672479893390893, -0.4578513259004232, -0.2416326854384544, 6.428349361134747, 6.656718281504274, -5.564788398161922, -3.02924871963873, -0.2716453262784845, 2.840935952141899, 0.0859121820261296, 4.694618143418901, -5.096817957116206, -0.4737762603304853, -1.385313600482476, -3.455075469132893, 6.134561343411066, 4.384932175234187, -0.6284984442281132, -1.060102630996634, 6.555893195756837, 8.364446730582344 ]
GA.add(GA.mul(a,b), GA.mul(a, c)) ==> [ -4.618544334910429, -1.861355248134714, -1.344616248842651, -0.2493092523303427, 2.805076632862418, 7.011456497501809, -2.595903465496112, -2.470668530528046, 2.219481357349964, 1.819582156863503, 7.750350045338696, 7.672479893390893, -0.4578513259004234, -0.2416326854384541, 6.42834936113475, 6.656718281504276, -5.564788398161921, -3.029248719638729, -0.2716453262784859, 2.840935952141899, 0.08591218202612916, 4.6946181434189, -5.096817957116205, -0.4737762603304851, -1.385313600482476, -3.455075469132893, 6.134561343411068, 4.384932175234186, -0.6284984442281131, -1.060102630996634, 6.555893195756836, 8.364446730582346 ]
GA.mul(GA.add(a, b), c) ==> [ -4.443021937525184, -3.701220635832722, -0.6818565879668145, 0.239151370235406, 3.458768472629645, 6.988109725549518, -3.684631030647005, -0.8507257226324562, 2.724894692072849, 4.331141304914477, 6.87324809064668, 5.39527557728919, -1.338732261664156, 0.5042890433991951, 3.524762387495515, 6.138868250703226, -5.595434900923999, -5.095415369801449, -2.668271896337068, 0.6569878447501574, -0.03973787332027184, 4.991106401646302, -5.122908058541086, -1.273551488694048, -2.981638879697476, -2.047004560822915, 6.135097056892218, 5.062423121634416, -1.370651481167422, -0.9463086012211376, 5.574441079445841, 8.892885715782024 ]
GA.add(GA.mul(a,c), GA.mul(b, c)) ==> [ -4.443021937525184, -3.701220635832721, -0.6818565879668153, 0.2391513702354063, 3.458768472629644, 6.988109725549519, -3.684631030647005, -0.8507257226324556, 2.724894692072848, 4.331141304914478, 6.873248090646679, 5.39527557728919, -1.338732261664156, 0.5042890433991953, 3.524762387495516, 6.138868250703224, -5.595434900923999, -5.095415369801452, -2.668271896337069, 0.6569878447501573, -0.03973787332027234, 4.991106401646301, -5.122908058541086, -1.273551488694048, -2.981638879697477, -2.047004560822915, 6.135097056892217, 5.062423121634419, -1.370651481167422, -0.946308601221137, 5.57444107944584, 8.892885715782022 ]
x ==> [ 0, 0.7467627291006274, 0.06901854981410338, 0, 0.05329494862236217, 0, 0, 0, 0.7743136455713469, 0, 0, 0, 0, 0, 0, 0, 0.7035034807394887, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
GA.mul(x, x) ==> [ 1.659737254471485, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u geometricAlgebra.jsi
[PASS] geometricAlgebra.jsi
```



## Kotlin

```scala
fun bitCount(i: Int): Int {
    var j = i
    j -= ((j shr 1) and 0x55555555)
    j = (j and 0x33333333) + ((j shr 2) and 0x33333333)
    j = (j + (j shr 4)) and 0x0F0F0F0F
    j += (j shr 8)
    j += (j shr 16)
    return j and 0x0000003F
}

fun reorderingSign(i: Int, j: Int): Double {
    var k = i shr 1
    var sum = 0
    while (k != 0) {
        sum += bitCount(k and j)
        k = k shr 1
    }
    return if (sum and 1 == 0) 1.0 else -1.0
}

class Vector(private val dims: DoubleArray) {

    infix fun dot(rhs: Vector): Vector {
        return (this * rhs + rhs * this) * 0.5
    }

    operator fun unaryMinus(): Vector {
        return this * -1.0
    }

    operator fun plus(rhs: Vector): Vector {
        val result = DoubleArray(32)
        dims.copyInto(result)
        for (i in 0 until rhs.dims.size) {
            result[i] += rhs[i]
        }
        return Vector(result)
    }

    operator fun times(rhs: Vector): Vector {
        val result = DoubleArray(32)
        for (i in 0 until dims.size) {
            if (dims[i] != 0.0) {
                for (j in 0 until rhs.dims.size) {
                    if (rhs[j] != 0.0) {
                        val s = reorderingSign(i, j) * dims[i] * rhs[j]
                        val k = i xor j
                        result[k] += s
                    }
                }
            }
        }
        return Vector(result)
    }

    operator fun times(scale: Double): Vector {
        val result = dims.clone()
        for (i in 0 until 5) {
            dims[i] = dims[i] * scale
        }
        return Vector(result)
    }

    operator fun get(index: Int): Double {
        return dims[index]
    }

    operator fun set(index: Int, value: Double) {
        dims[index] = value
    }

    override fun toString(): String {
        val sb = StringBuilder("(")
        val it = dims.iterator()
        if (it.hasNext()) {
            sb.append(it.next())
        }
        while (it.hasNext()) {
            sb.append(", ").append(it.next())
        }
        return sb.append(")").toString()
    }
}

fun e(n: Int): Vector {
    if (n > 4) {
        throw IllegalArgumentException("n must be less than 5")
    }
    val result = Vector(DoubleArray(32))
    result[1 shl n] = 1.0
    return result
}

val rand = java.util.Random()

fun randomVector(): Vector {
    var result = Vector(DoubleArray(32))
    for (i in 0 until 5) {
        result += Vector(doubleArrayOf(rand.nextDouble())) * e(i)
    }
    return result
}

fun randomMultiVector(): Vector {
    val result = Vector(DoubleArray(32))
    for (i in 0 until 32) {
        result[i] = rand.nextDouble()
    }
    return result
}

fun main() {
    for (i in 0..4) {
        for (j in 0..4) {
            if (i < j) {
                if ((e(i) dot e(j))[0] != 0.0) {
                    println("Unexpected non-null scalar product.")
                    return
                } else if (i == j) {
                    if ((e(i) dot e(j))[0] == 0.0) {
                        println("Unexpected null scalar product.")
                    }
                }
            }
        }
    }

    val a = randomMultiVector()
    val b = randomMultiVector()
    val c = randomMultiVector()
    val x = randomVector()

    // (ab)c == a(bc)
    println((a * b) * c)
    println(a * (b * c))
    println()

    // a(b+c) == ab + ac
    println(a * (b + c))
    println(a * b + a * c)
    println()

    // (a+b)c == ac + bc
    println((a + b) * c)
    println(a * c + b * c)
    println()

    // x^2 is real
    println(x * x)
}
```

```txt
(-6.38113123172589, -6.025395204580336, 0.5762054454373319, -2.224611121553874, -0.03467815839340305, -0.6665488550665257, -3.012105902847624, 0.7315782457554153, -4.183528079943369, -1.8391037440709876, -0.137654892093293, 0.10852885457965271, -6.021317788342983, -5.486453322362711, -3.524908677069778, -1.030729377561671, -6.858194536947578, -8.724962937014816, 0.4660400096706247, -1.6434599565678671, 4.212637141194194, 2.916899539720754, -1.365566480297562, 1.898991559248674, -2.5943503153384517, -0.7167616808942235, 1.2152416665362584, 2.9936787524618067, -5.394453145898911, -4.180356766796923, -6.622391097517418, -4.249450373116712)
(-6.381131231725885, -6.025395204580336, 0.5762054454373331, -2.2246111215538744, -0.034678158393403866, -0.6665488550665265, -3.012105902847625, 0.7315782457554131, -4.183528079943374, -1.83910374407099, -0.13765489209329423, 0.1085288545796525, -6.021317788342984, -5.486453322362712, -3.524908677069777, -1.0307293775616722, -6.858194536947579, -8.724962937014814, 0.46604000967062464, -1.6434599565678658, 4.212637141194197, 2.916899539720756, -1.3655664802975644, 1.8989915592486726, -2.5943503153384526, -0.716761680894223, 1.2152416665362615, 2.9936787524618076, -5.394453145898909, -4.1803567667969235, -6.622391097517417, -4.24945037311671)

(-5.001996874378833, -5.342863347345118, 0.5930301138931318, 1.8495242515856356, 5.226690714239807, 5.60029170092836, 1.9264816320512175, 2.9647400073622383, -1.231074336132209, -0.03574406666207321, 2.0490593688751666, 2.709392811857615, -0.534934380183112, 0.9566513059906786, 0.581531676517317, 2.6867651675849284, -6.072314695960932, -5.780622914352332, -2.264083859970755, -0.29904973719224437, 3.1693202958893196, 3.710947660874322, -0.3876119148699613, 0.28047758363117253, 0.4891346543855399, 1.1102262337923456, 1.9059090529704015, 2.2175302024987404, 3.6850877929174954, 4.798530127158409, 0.6243236143375706, 2.5910748978446576)
(-5.001996874378833, -5.3428633473451175, 0.5930301138931321, 1.8495242515856363, 5.22669071423981, 5.600291700928359, 1.9264816320512164, 2.9647400073622387, -1.231074336132209, -0.035744066662073595, 2.0490593688751657, 2.7093928118576143, -0.5349343801831126, 0.9566513059906783, 0.5815316765173175, 2.6867651675849284, -6.072314695960929, -5.78062291435233, -2.264083859970754, -0.2990497371922432, 3.169320295889319, 3.7109476608743237, -0.3876119148699616, 0.2804775836311723, 0.4891346543855406, 1.1102262337923452, 1.9059090529704013, 2.21753020249874, 3.6850877929174954, 4.798530127158408, 0.6243236143375701, 2.591074897844657)

(-3.583945607396393, -3.5239300960595137, 0.801943558725803, 2.9686354622847357, 4.064529783603304, 3.6456241887862015, 0.5954304494585552, 1.2819925842824296, 0.6768216109361052, 0.352207714198545, 2.843092732483635, 2.680239849424965, -0.04943045827884884, 0.19653339086911384, 2.875432393192088, 2.8657275256839823, -4.689497942711508, -3.42309859067624, -2.830665114554725, -0.017676578013020444, 3.635425127096899, 3.10264457980534, -1.2355052114199154, 0.02671272974814648, 2.5200281685651413, 2.2030677245192627, 2.727374336905742, 1.7812768180621927, 1.901781821035387, 2.8306551025240863, 3.9572153730448463, 2.9611369534401093)
(-3.5839456073963945, -3.5239300960595115, 0.8019435587258037, 2.968635462284734, 4.064529783603305, 3.6456241887862024, 0.5954304494585542, 1.2819925842824302, 0.6768216109361052, 0.3522077141985451, 2.843092732483634, 2.6802398494249644, -0.04943045827884848, 0.1965333908691129, 2.875432393192088, 2.8657275256839827, -4.689497942711507, -3.4230985906762403, -2.8306651145547246, -0.017676578013019806, 3.6354251270968985, 3.1026445798053395, -1.2355052114199154, 0.026712729748146535, 2.5200281685651413, 2.2030677245192627, 2.7273743369057417, 1.7812768180621923, 1.9017818210353874, 2.830655102524086, 3.957215373044847, 2.9611369534401097)

(2.6501903002573224, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
```



## Perl 6


Here we write a simplified version of the [https://github.com/grondilu/clifford Clifford] module.  It is very general as it is of infinite dimension and also contains an anti-euclidean basis @ē in addition to the euclidean basis @e.


```perl6
unit class MultiVector;
subset UIntHash of MixHash where .keys.all ~~ UInt;
has UIntHash $.blades;
method narrow { $!blades.keys.any > 0 ?? self !!  ($!blades{0} // 0) }

multi method new(Real $x) returns MultiVector { self.new: (0 => $x).MixHash }
multi method new(UIntHash $blades) returns MultiVector { self.new: :$blades }

multi method new(Str $ where /^^e(\d+)$$/) { self.new: (1 +< (2*$0)).MixHash }
multi method new(Str $ where /^^ē(\d+)$$/) { self.new: (1 +< (2*$0 + 1)).MixHash }

our @e is export = map { MultiVector.new: "e$_" }, ^Inf;
our @ē is export = map { MultiVector.new: "ē$_" }, ^Inf;

my sub order(UInt:D $i is copy, UInt:D $j) {
    (state %){$i}{$j} //= do {
	my $n = 0;
	repeat {
	    $i +>= 1;
	    $n += [+] ($i +& $j).polymod(2 xx *);
	} until $i == 0;
	$n +& 1 ?? -1 !! 1;
    }
}

multi infix:<+>(MultiVector $A, MultiVector $B) returns MultiVector is export {
    return MultiVector.new: ($A.blades.pairs, |$B.blades.pairs).MixHash;
}
multi infix:<+>(Real $s, MultiVector $B) returns MultiVector is export {
    return MultiVector.new: (0 => $s, |$B.blades.pairs).MixHash;
}
multi infix:<+>(MultiVector $A, Real $s) returns MultiVector is export { $s + $A }

multi infix:<*>(MultiVector $,  0) is export { 0  }
multi infix:<*>(MultiVector $A, 1) returns MultiVector is export { $A }
multi infix:<*>(MultiVector $A, Real $s) returns MultiVector is export {
    MultiVector.new: $A.blades.pairs.map({Pair.new: .key, $s*.value}).MixHash
}
multi infix:<*>(MultiVector $A, MultiVector $B) returns MultiVector is export {
    MultiVector.new: do for $A.blades -> $a {
	|do for $B.blades -> $b {
	    ($a.key +^ $b.key) => [*]
	    $a.value, $b.value,
	    order($a.key, $b.key),
	    |grep +*, (
		|(1, -1) xx * Z*
		($a.key +& $b.key).polymod(2 xx *)
	    )
	}
    }.MixHash
}
multi infix:<**>(MultiVector $ , 0) returns MultiVector is export { MultiVector.new }
multi infix:<**>(MultiVector $A, 1) returns MultiVector is export { $A }
multi infix:<**>(MultiVector $A, 2) returns MultiVector is export { $A * $A }
multi infix:<**>(MultiVector $A, UInt $n where $n %% 2) returns MultiVector is export { ($A ** ($n div 2)) ** 2 }
multi infix:<**>(MultiVector $A, UInt $n) returns MultiVector is export { $A * ($A ** ($n div 2)) ** 2 }

multi infix:<*>(Real $s, MultiVector $A) returns MultiVector is export { $A * $s }
multi infix:</>(MultiVector $A, Real $s) returns MultiVector is export { $A * (1/$s) }
multi prefix:<->(MultiVector $A) returns MultiVector is export { return -1 * $A }
multi infix:<->(MultiVector $A, MultiVector $B) returns MultiVector is export { $A + -$B }
multi infix:<->(MultiVector $A, Real $s) returns MultiVector is export { $A + -$s }
multi infix:<->(Real $s, MultiVector $A) returns MultiVector is export { $s + -$A }

multi infix:<==>(MultiVector $A, MultiVector $B) returns Bool is export { $A - $B == 0 }
multi infix:<==>(Real $x, MultiVector $A) returns Bool is export { $A == $x }
multi infix:<==>(MultiVector $A, Real $x) returns Bool is export {
    my $narrowed = $A.narrow;
    $narrowed ~~ Real and $narrowed == $x;
}

#########################################
##  Test code to verify the solution:  ##
#########################################

use Test;

plan 29;

sub infix:<cdot>($x, $y) { ($x*$y + $y*$x)/2 }

for ^5 X ^5 -> ($i, $j) {
    my $s = $i == $j ?? 1 !! 0;
    ok @e[$i] cdot @e[$j] == $s, "e$i cdot e$j = $s";
}
sub random {
    [+] map {
        MultiVector.new:
        :blades(($_ => rand.round(.01)).MixHash)
    }, ^32;
}

my ($a, $b, $c) = random() xx 3;

ok ($a*$b)*$c == $a*($b*$c), 'associativity';
ok $a*($b + $c) == $a*$b + $a*$c, 'left distributivity';
ok ($a + $b)*$c == $a*$c + $b*$c, 'right distributivity';
my @coeff = (.5 - rand) xx 5;
my $v = [+] @coeff Z* @e[^5];
ok ($v**2).narrow ~~ Real, 'contraction';
```



## Phix

```Phix
function bitCount(integer i)
    return sum(sq_eq(sprintf("%b",i),'1'))  -- (idea cribbed from Python)
end function

function reorderingSign(integer i, j)
    i = floor(i/2)
    integer tot := 0
    while i!=0 do
        tot += bitCount(and_bits(i,j))
        i = floor(i/2)
    end while
    return iff(and_bits(tot,1)==0 ? 1 : -1)
end function

function add(sequence a, b)
    return sq_add(a,b)
end function

function mul(sequence a, b)
    sequence result = repeat(0,32)
    for i=1 to length(a) do
        if a[i]!=0 then
            for j=1 to length(b) do
                if b[j]!=0 then
                    atom s := reorderingSign(i-1, j-1) * a[i] * b[j]
                    integer k = xor_bits(i-1,j-1)+1
                    result[k] += s
                end if
            end for
        end if
    end for
    return result
end function

function cdot(sequence a, b)
    return mul({0.5}, add(mul(a, b), mul(b, a)))
end function

function e(integer n)
    if n>4 then crash("n must be less than 5") end if
    sequence result = repeat(0,32)
    result[power(2,n)+1] = 1.0
    return result
end function

--function neg(sequence x) -- (not actually used here)
--  return mul({-1}, x)
--end function

function randomVector()
    sequence result = repeat(0,32)
    for i=0 to 4 do
        result = add(result, mul({rnd()}, e(i)))
    end for
    return result
end function

function randomMultiVector()
    sequence result = repeat(0, 32)
    for i=1 to 32 do
        result[i] = rnd()
    end for
    return result
end function

for i=0 to 4 do
    for j=0 to 4 do
        if i < j then
            if cdot(e(i), e(j))[1] != 0 then
                crash("Unexpected non-null scalar product.")
            end if
        elsif i == j then
            if cdot(e(i), e(j))[1] == 0 then
                crash("Unexpected null scalar product.")
            end if
        end if
    end for
end for

sequence a := randomMultiVector(),
         b := randomMultiVector(),
         c := randomMultiVector(),
         x := randomVector(),
         xsq = mul(x, x)

procedure test(string txt, sequence a, b)
--  bool eq = (a==b)                        -- no!
    bool eq = (sprint(a)==sprint(b))        -- ok!
    printf(1,"%-20s: %s\n",{txt,iff(eq?"true","false")})
end procedure

test("(ab)c == a(bc)",mul(mul(a, b), c),
                      mul(a, mul(b, c)))

test("a(b + c) == ab + ac",mul(a, add(b, c)),
                           add(mul(a, b), mul(a, c)))

test("(a + b)c == ac + bc",mul(add(a, b), c),
                           add(mul(a, c), mul(b, c)))

test("x^2 is real",xsq,xsq[1]&repeat(0,31))
```

Note the comparison of string representations of floats, rather than the floats themselves. That effectively ensures they
all match to 10 significant digits, and avoids a few tiny (~1e-16) discrepancies.

```txt

(ab)c == a(bc)      : true
a(b + c) == ab + ac : true
(a + b)c == ac + bc : true
x^2 is real         : true

```



## Python

```python
import copy, random

def bitcount(n):
    return bin(n).count("1")

def reoderingSign(i, j):
    k = i >> 1
    sum = 0
    while k != 0:
        sum += bitcount(k & j)
        k = k >> 1
    return 1.0 if ((sum & 1) == 0) else -1.0

class Vector:
    def __init__(self, da):
        self.dims = da

    def dot(self, other):
        return (self * other + other * self) * 0.5

    def __getitem__(self, i):
        return self.dims[i]

    def __setitem__(self, i, v):
        self.dims[i] = v

    def __neg__(self):
        return self * -1.0

    def __add__(self, other):
        result = copy.copy(other.dims)
        for i in xrange(0, len(self.dims)):
            result[i] += self.dims[i]
        return Vector(result)

    def __mul__(self, other):
        if isinstance(other, Vector):
            result = [0.0] * 32
            for i in xrange(0, len(self.dims)):
                if self.dims[i] != 0.0:
                    for j in xrange(0, len(self.dims)):
                        if other.dims[j] != 0.0:
                            s = reoderingSign(i, j) * self.dims[i] * other.dims[j]
                            k = i ^ j
                            result[k] += s
            return Vector(result)
        else:
            result = copy.copy(self.dims)
            for i in xrange(0, len(self.dims)):
                self.dims[i] *= other
            return Vector(result)

    def __str__(self):
        return str(self.dims)

def e(n):
    assert n <= 4, "n must be less than 5"
    result = Vector([0.0] * 32)
    result[1 << n] = 1.0
    return result

def randomVector():
    result = Vector([0.0] * 32)
    for i in xrange(0, 5):
        result += Vector([random.uniform(0, 1)]) * e(i)
    return result

def randomMultiVector():
    result = Vector([0.0] * 32)
    for i in xrange(0, 32):
        result[i] = random.uniform(0, 1)
    return result

def main():
    for i in xrange(0, 5):
        for j in xrange(0, 5):
            if i < j:
                if e(i).dot(e(j))[0] != 0.0:
                    print "Unexpected non-null scalar product"
                    return
                elif i == j:
                    if e(i).dot(e(j))[0] == 0.0:
                        print "Unexpected non-null scalar product"

    a = randomMultiVector()
    b = randomMultiVector()
    c = randomMultiVector()
    x = randomVector()

    # (ab)c == a(bc)
    print (a * b) * c
    print a * (b * c)
    print

    # a(b+c) == ab + ac
    print a * (b + c)
    print a * b + a * c
    print

    # (a+b)c == ac + bc
    print (a + b) * c
    print a * c + b * c
    print

    # x^2 is real
    print x * x

main()
```

```txt
[2.646777769717816, -5.480686120935684, -8.183342078006843, -9.178717618666656, -0.21247781959240397, -3.1560121872423172, -14.210376795019405, -7.975576839132462, -2.963314079857538, -8.128489630952732, 8.84291288803876, 6.849688422048398, -3.948403894153645, -6.3295864734054295, 0.858339386946704, 0.04073276768257372, -0.8170168057484614, -5.987310468330181, -5.089567141509365, -6.5916164371098205, 1.066652018944462, -0.7553724661211869, -16.61957782752131, -10.74332838047719, -0.22326945346944393, -5.502857138805277, 11.833089760883906, 11.020055749901102, -3.7471254230186233, -3.5483496341413763, 7.788213699886802, 5.385261642366723]
[2.6467777697178145, -5.480686120935683, -8.183342078006845, -9.178717618666658, -0.2124778195924022, -3.156012187242318, -14.210376795019414, -7.975576839132467, -2.9633140798575406, -8.128489630952735, 8.842912888038763, 6.849688422048397, -3.9484038941536435, -6.329586473405431, 0.8583393869467044, 0.04073276768257594, -0.8170168057484637, -5.987310468330179, -5.089567141509367, -6.591616437109819, 1.0666520189444642, -0.7553724661211856, -16.61957782752132, -10.743328380477191, -0.22326945346944407, -5.502857138805281, 11.83308976088391, 11.0200557499011, -3.7471254230186246, -3.5483496341413807, 7.7882136998868035, 5.385261642366723]

[-4.45416862548425, -4.007728055936393, 2.2599167577088886, -0.5008511526260965, 6.404388731518947, 3.553799027487341, -1.4559344997025763, 1.200629741306491, 0.28084755190469957, 1.6881017142666677, 4.622735951152484, 2.042861306698215, -1.1859431529346458, -0.23268120656473645, 3.3088499800790308, 6.272881551311293, -6.283417063207868, -6.059129990387314, 0.9004412097752342, 0.540839567518711, 2.9772708785233157, 2.2875667777090625, 1.6049404915540564, 4.551625497411361, 3.4613544600410537, 4.601629280006443, 4.934029921827034, 3.0257810667323715, -1.280636387447562, -0.21306231994217983, 6.089428007073901, 8.128821330734313]
[-4.454168625484251, -4.007728055936392, 2.259916757708888, -0.5008511526260957, 6.404388731518948, 3.553799027487342, -1.455934499702577, 1.2006297413064915, 0.2808475519046991, 1.688101714266668, 4.622735951152485, 2.0428613066982155, -1.1859431529346458, -0.23268120656473568, 3.30884998007903, 6.272881551311293, -6.283417063207869, -6.059129990387315, 0.9004412097752316, 0.5408395675187101, 2.9772708785233157, 2.287566777709062, 1.6049404915540577, 4.551625497411363, 3.4613544600410533, 4.601629280006441, 4.934029921827035, 3.025781066732372, -1.2806363874475617, -0.21306231994217995, 6.0894280070738995, 8.128821330734311]

[-4.713407548689167, -5.470701972164317, 0.2834720300902142, -1.8843569665485043, 2.8883659013514302, 3.6276355158044815, -2.7614493177411843, 0.14095340884428206, -1.191066813989091, 1.1017933922740846, 2.995254519836379, 1.5249479602578073, 2.153333527417164, 1.999841187299821, 6.220565393668025, 8.730809912614522, -7.61200478654088, -9.557862449328784, 1.432009511995673, 0.14006605762543944, 1.450154388175902, 2.5115288790301835, -1.5609458922816675, 1.9148273860716452, 3.56683599400551, 3.9854109527025505, 3.6838872880534086, 3.059534508634617, 3.237048050921782, 2.674541802512928, 6.252577743980652, 7.309261452099341]
[-4.713407548689167, -5.470701972164316, 0.28347203009021416, -1.8843569665485043, 2.8883659013514302, 3.6276355158044815, -2.761449317741185, 0.14095340884428198, -1.1910668139890919, 1.101793392274085, 2.9952545198363785, 1.5249479602578067, 2.153333527417164, 1.9998411872998219, 6.220565393668025, 8.730809912614523, -7.612004786540879, -9.557862449328784, 1.4320095119956717, 0.14006605762543944, 1.4501543881759016, 2.511528879030183, -1.5609458922816672, 1.9148273860716456, 3.5668359940055105, 3.985410952702549, 3.683887288053409, 3.0595345086346164, 3.2370480509217803, 2.6745418025129273, 6.252577743980652, 7.30926145209934]

[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
```


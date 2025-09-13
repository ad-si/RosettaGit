+++
title = "Numerical integration/Gauss-Legendre Quadrature"
description = ""
date = 2019-10-21T23:22:14Z
aliases = []
[extra]
id = 9805
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
+++

## Task

{|border=1 cellspacing=0 cellpadding=3
|In a general [[wp:Gaussian quadrature|Gaussian quadrature]] rule, an definite integral of <math>f(x)</math> is first approximated over the interval <math>[-1,1]</math> by a polynomial approximable function <math>g(x)</math> and a known weighting function <math>W(x)</math>.
|<math>\int_{-1}^1 f(x) \, dx = \int_{-1}^1 W(x) g(x) \, dx</math>
|-
|Those are then approximated by a sum of function values at specified points <math>x_i</math> multiplied by some weights <math>w_i</math>:
|<math>\int_{-1}^1 W(x) g(x) \, dx \approx \sum_{i=1}^n w_i g(x_i)</math>
|-
|In the case of Gauss-Legendre quadrature, the weighting function <math>W(x) = 1</math>, so we can approximate an integral of <math>f(x)</math> with:
|<math>\int_{-1}^1 f(x)\,dx \approx \sum_{i=1}^n w_i f(x_i)</math>
|}


For this, we first need to calculate the nodes and the weights, but after we have them, we can reuse them for numerious integral evaluations, which greatly speeds up the calculation compared to more [[Numerical Integration|simple numerical integration methods]].

{|border=1 cellspacing=0 cellpadding=3
|The <math>n</math> evaluation points <math>x_i</math> for a n-point rule, also called "nodes", are roots of n-th order [[wp:Legendre Polynomials|Legendre Polynomials]] <math>P_n(x)</math>. Legendre polynomials are defined by the following recursive rule:
|<math>P_0(x) = 1</math><br/>
<math>P_1(x) = x</math><br/>
<math>nP_{n}(x) = (2n-1)xP_{n-1}(x)-(n-1)P_{n-2}(x)</math>
|-
|There is also a recursive equation for their derivative:
|<math>P_{n}'(x) = \frac{n}{x^2-1} \left( x P_n(x) - P_{n-1}(x) \right)</math>
|-
|The roots of those polynomials are in general not analytically solvable, so they have to be approximated numerically, for example by [[wp:Newton's method|Newton-Raphson iteration]]:
|<math>x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}</math>
|-
|The first guess <math>x_0</math> for the <math>i</math>-th root of a <math>n</math>-order polynomial <math>P_n</math> can be given by
|<math>x_0 = \cos \left( \pi \, \frac{i - \frac{1}{4}}{n+\frac{1}{2}} \right)</math>
|-
|After we get the nodes <math>x_i</math>, we compute the appropriate weights by:
|<math>w_i = \frac{2}{\left( 1-x_i^2 \right) [P'_n(x_i)]^2}</math>
|-
|After we have the nodes and the weights for a n-point quadrature rule, we can approximate an integral over any interval <math>[a,b]</math> by
|<math>\int_a^b f(x)\,dx \approx \frac{b-a}{2} \sum_{i=1}^n w_i f\left(\frac{b-a}{2}x_i + \frac{a+b}{2}\right)</math>
|}


'''Task description'''

Similar to the task [[Numerical Integration]], the task here is to calculate the definite integral of a function <math>f(x)</math>, but by applying an n-point Gauss-Legendre quadrature rule, as described [[wp:Gaussian Quadrature|here]], for example. The input values should be an function f to integrate, the bounds of the integration interval a and b, and the number of gaussian evaluation points n. An reference implementation in Common Lisp is provided for comparison.

To demonstrate the calculation, compute the weights and nodes for an 5-point quadrature rule and then use them to compute:
          <big><big><math>\int_{-3}^{3} \exp(x) \, dx \approx \sum_{i=1}^5 w_i \; \exp(x_i) \approx 20.036</math></big></big>





## Axiom

Axiom provides Legendre polynomials and related solvers.
```Axiom>NNI ==
 NonNegativeInteger
RECORD ==> Record(x : List Fraction Integer, w : List Fraction Integer)

gaussCoefficients(n : NNI, eps : Fraction Integer) : RECORD ==
  p := legendreP(n,z)
  q := n/2*D(p, z)*legendreP(subtractIfCan(n,1)::NNI, z)
  x := map(rhs,solve(p,eps))
  w := [subst(1/q, z=xi) for xi in x]
  [x,w]

gaussIntegrate(e : Expression Float, segbind : SegmentBinding(Float), n : NNI) : Float ==
  eps := 1/10^100
  u := gaussCoefficients(n,eps)
  interval := segment segbind
  var := variable segbind
  a := lo interval
  b := hi interval
  c := (a+b)/2
  h := (b-a)/2
  h*reduce(+,[wi*subst(e,var=c+xi*h) for xi in u.x for wi in u.w])
```
Example:
```Axiom
digits(50)
gaussIntegrate(4/(1+x^2), x=0..1, 20)

   (1)  3.1415926535_8979323846_2643379815_9534002592_872901276
                                                                  Type: Float
% - %pi

   (2)  - 0.3463549483_9378821092_475 E -26
```



## C


```c
#include <stdio.h>
#include <math.h>

#define N 5
double Pi;
double lroots[N];
double weight[N];
double lcoef[N + 1][N + 1] = {{0}};

void lege_coef()
{
	int n, i;
	lcoef[0][0] = lcoef[1][1] = 1;
	for (n = 2; n <= N; n++) {
		lcoef[n][0] = -(n - 1) * lcoef[n - 2][0] / n;
		for (i = 1; i <= n; i++)
			lcoef[n][i] = ((2 * n - 1) * lcoef[n - 1][i - 1]
					 - (n - 1) * lcoef[n - 2][i] ) / n;
	}
}

double lege_eval(int n, double x)
{
	int i;
	double s = lcoef[n][n];
	for (i = n; i; i--)
		s = s * x + lcoef[n][i - 1];
	return s;
}

double lege_diff(int n, double x)
{
	return n * (x * lege_eval(n, x) - lege_eval(n - 1, x)) / (x * x - 1);
}

void lege_roots()
{
	int i;
	double x, x1;
	for (i = 1; i <= N; i++) {
		x = cos(Pi * (i - .25) / (N + .5));
		do {
			x1 = x;
			x -= lege_eval(N, x) / lege_diff(N, x);
		} while ( fdim( x, x1) > 2e-16 );
		/*  fdim( ) was introduced in C99, if it isn't available
		 *  on your system, try fabs( ) */
		lroots[i - 1] = x;

		x1 = lege_diff(N, x);
		weight[i - 1] = 2 / ((1 - x * x) * x1 * x1);
	}
}

double lege_inte(double (*f)(double), double a, double b)
{
	double c1 = (b - a) / 2, c2 = (b + a) / 2, sum = 0;
	int i;
	for (i = 0; i < N; i++)
		sum += weight[i] * f(c1 * lroots[i] + c2);
	return c1 * sum;
}

int main()
{
	int i;
	Pi = atan2(1, 1) * 4;

	lege_coef();
	lege_roots();

	printf("Roots: ");
	for (i = 0; i < N; i++)
		printf(" %g", lroots[i]);

	printf("\nWeight:");
	for (i = 0; i < N; i++)
		printf(" %g", weight[i]);

	printf("\nintegrating Exp(x) over [-3, 3]:\n\t%10.8f,\n"
		"compred to actual\n\t%10.8f\n",
		lege_inte(exp, -3, 3), exp(3) - exp(-3));
	return 0;
}
```

```txt
Roots:  0.90618 0.538469 0 -0.538469 -0.90618
Weight: 0.236927 0.478629 0.568889 0.478629 0.236927
integrating Exp(x) over [-3, 3]:
        20.03557772,
compred to actual
        20.03574985
```



## Common Lisp


```lisp
;; Computes the initial guess for the root i of a n-order Legendre polynomial.
(defun guess (n i)
  (cos (* pi
          (/ (- i 0.25d0)
             (+ n 0.5d0)))))

;; Computes and evaluates the n-order Legendre polynomial at the point x.
(defun legpoly (n x)
  (let ((pa 1.0d0)
        (pb x)
        (pn))
    (cond ((= n 0) pa)
          ((= n 1) pb)
          (t (loop for i from 2 to n do
                  (setf pn (- (* (/ (- (* 2 i) 1) i) x pb)
                              (* (/ (- i 1) i) pa)))
                  (setf pa pb)
                  (setf pb pn)
                  finally (return pn))))))

;; Computes and evaluates the derivative of an n-order Legendre polynomial at point x.
(defun legdiff (n x)
  (* (/ n (- (* x x) 1))
     (- (* x (legpoly n x))
        (legpoly (- n 1) x))))

;; Computes the n nodes for an n-point quadrature rule. (i.e. n roots of a n-order polynomial)
(defun nodes (n)
  (let ((x (make-array n :initial-element 0.0d0)))
    (loop for i from 0 to (- n 1) do
         (let ((val (guess n (+ i 1))) ;Nullstellen-Schätzwert.
               (itermax 5))
           (dotimes (j itermax)
             (setf val (- val
                          (/ (legpoly n val)
                             (legdiff n val)))))
           (setf (aref x i) val)))
    x))

;; Computes the weight for an n-order polynomial at the point (node) x.
(defun legwts (n x)
  (/ 2
     (* (- 1 (* x x))
        (expt (legdiff n x) 2))))

;; Takes a array of nodes x and computes an array of corresponding weights w.
(defun weights (x)
  (let* ((n (car (array-dimensions x)))
         (w (make-array n :initial-element 0.0d0)))
    (loop for i from 0 to (- n 1) do
         (setf (aref w i) (legwts n (aref x i))))
    w))

;; Integrates a function f with a n-point Gauss-Legendre quadrature rule over the interval [a,b].
(defun int (f n a b)
  (let* ((x (nodes n))
         (w (weights x)))
    (* (/ (- b a) 2.0d0)
       (loop for i from 0 to (- n 1)
          sum (* (aref w i)
                 (funcall f (+ (* (/ (- b a) 2.0d0)
                                  (aref x i))
                               (/ (+ a b) 2.0d0))))))))
```

```lisp
(nodes 5)
#(0.906179845938664d0 0.5384693101056831d0 2.996272867003007d-95 -0.5384693101056831d0 -0.906179845938664d0)

(weights (nodes 5))
#(0.23692688505618917d0 0.47862867049936647d0 0.5688888888888889d0 0.47862867049936647d0 0.23692688505618917d0)

(int #'exp 5 -3 3)
20.035577718385568d0
```

Comparison of the 5-point rule with simpler, but more costly methods from the task [[Numerical Integration]]:

```lisp
(int #'(lambda (x) (expt x 3)) 5 0 1)
0.24999999999999997d0

(int #'(lambda (x) (/ 1 x)) 5 1 100)
4.059147508941519d0

(int #'(lambda (x) x) 5 0 5000)
1.25d7

(int #'(lambda (x) x) 5 0 6000)
1.8000000000000004d7
```



## C++

Derived from various sources already here.

Does not quite perform the task quite as specified since the node count, N, is set at compile time (to avoid heap allocation) so cannot be passed as a parameter.

```cpp

namespace Rosetta {

    /*! Implementation of Gauss-Legendre quadrature
    *  http://en.wikipedia.org/wiki/Gaussian_quadrature
    *  http://rosettacode.org/wiki/Numerical_integration/Gauss-Legendre_Quadrature
    *
    */
    template <int N>
    class GaussLegendreQuadrature {
    public:
        enum {eDEGREE = N};

        /*! Compute the integral of a functor
        *
        *   @param a    lower limit of integration
        *   @param b    upper limit of integration
        *   @param f    the function to integrate
        *   @param err  callback in case of problems
        */
        template <typename Function>
        double integrate(double a, double b, Function f) {
            double p = (b - a) / 2;
            double q = (b + a) / 2;
            const LegendrePolynomial& legpoly = s_LegendrePolynomial;

            double sum = 0;
            for (int i = 1; i <= eDEGREE; ++i) {
                sum += legpoly.weight(i) * f(p * legpoly.root(i) + q);
            }

            return p * sum;
        }

        /*! Print out roots and weights for information
        */
        void print_roots_and_weights(std::ostream& out) const {
            const LegendrePolynomial& legpoly = s_LegendrePolynomial;
            out << "Roots:  ";
            for (int i = 0; i <= eDEGREE; ++i) {
                out << ' ' << legpoly.root(i);
            }
            out << '\n';
            out << "Weights:";
            for (int i = 0; i <= eDEGREE; ++i) {
                out << ' ' << legpoly.weight(i);
            }
            out << '\n';
        }
    private:
        /*! Implementation of the Legendre polynomials that form
        *   the basis of this quadrature
        */
        class LegendrePolynomial {
        public:
            LegendrePolynomial () {
                // Solve roots and weights
                for (int i = 0; i <= eDEGREE; ++i) {
                    double dr = 1;

                    // Find zero
                    Evaluation eval(cos(M_PI * (i - 0.25) / (eDEGREE + 0.5)));
                    do {
                        dr = eval.v() / eval.d();
                        eval.evaluate(eval.x() - dr);
                    } while (fabs (dr) > 2e-16);

                    this->_r[i] = eval.x();
                    this->_w[i] = 2 / ((1 - eval.x() * eval.x()) * eval.d() * eval.d());
                }
            }

            double root(int i) const { return this->_r[i]; }
            double weight(int i) const { return this->_w[i]; }
        private:
            double _r[eDEGREE + 1];
            double _w[eDEGREE + 1];

            /*! Evaluate the value *and* derivative of the
            *   Legendre polynomial
            */
            class Evaluation {
            public:
                explicit Evaluation (double x) : _x(x), _v(1), _d(0) {
                    this->evaluate(x);
                }

                void evaluate(double x) {
                    this->_x = x;

                    double vsub1 = x;
                    double vsub2 = 1;
                    double f     = 1 / (x * x - 1);

                    for (int i = 2; i <= eDEGREE; ++i) {
                        this->_v = ((2 * i - 1) * x * vsub1 - (i - 1) * vsub2) / i;
                        this->_d = i * f * (x * this->_v - vsub1);

                        vsub2 = vsub1;
                        vsub1 = this->_v;
                    }
                }

                double v() const { return this->_v; }
                double d() const { return this->_d; }
                double x() const { return this->_x; }

            private:
                double _x;
                double _v;
                double _d;
            };
        };

        /*! Pre-compute the weights and abscissae of the Legendre polynomials
        */
        static LegendrePolynomial s_LegendrePolynomial;
    };

    template <int N>
    typename GaussLegendreQuadrature<N>::LegendrePolynomial GaussLegendreQuadrature<N>::s_LegendrePolynomial;
}

// This to avoid issues with exp being a templated function
double RosettaExp(double x) {
    return exp(x);
}

int main() {
    Rosetta::GaussLegendreQuadrature<5> gl5;

    std::cout << std::setprecision(10);

    gl5.print_roots_and_weights(std::cout);
    std::cout << "Integrating Exp(X) over [-3, 3]: " << gl5.integrate(-3., 3., RosettaExp) << '\n';
    std::cout << "Actual value:                    " << RosettaExp(3) - RosettaExp(-3) << '\n';
}

```


```txt

Roots:   0.9061798459 0.9061798459 0.5384693101 0 -0.5384693101 -0.9061798459
Weights: 0.2369268851 0.2369268851 0.4786286705 0.5688888889 0.4786286705 0.2369268851
Integrating Exp(X) over [-3, 3]: 20.03557772
Actual value:                    20.03574985

```



## Delphi



```Delphi
program Legendre;

{$APPTYPE CONSOLE}

const Order   = 5;
      Epsilon = 1E-12;

var Roots   : array[0..Order-1] of double;
    Weight  : array[0..Order-1] of double;
    LegCoef : array [0..Order,0..Order] of double;

function F(X:double) : double;
begin
  Result := Exp(X);
end;

procedure PrepCoef;
var I, N : integer;
begin
  for I:=0 to Order do
    for N := 0 to Order do
      LegCoef[I,N] := 0;
  LegCoef[0,0] := 1;
  LegCoef[1,1] := 1;
  For N:=2 to Order do
    begin
      LegCoef[N,0] := -(N-1) * LegCoef[N-2,0] / N;
      For I := 1 to Order do
        LegCoef[N,I] := ((2*N-1) * LegCoef[N-1,I-1] - (N-1)*LegCoef[N-2,I]) / N;
    end;
end;

function LegEval(N:integer; X:double) : double;
var I : integer;
begin
  Result := LegCoef[n][n];
  for I := N-1 downto 0 do
    Result := Result * X + LegCoef[N][I];
end;

function LegDiff(N:integer; X:double) : double;
begin
  Result := N * (X * LegEval(N,X) - LegEval(N-1,X)) / (X*X-1);
end;

procedure LegRoots;
var I     : integer;
    X, X1 : double;
begin
  for I := 1 to Order do
    begin
      X := Cos(Pi * (I-0.25) / (Order+0.5));
        repeat
          X1 := X;
          X := X - LegEval(Order,X) / LegDiff(Order, X);
        until Abs (X-X1) < Epsilon;
      Roots[I-1] := X;
      X1 := LegDiff(Order,X);
      Weight[I-1] := 2 / ((1-X*X) * X1*X1);
    end;
end;

function LegInt(A,B:double) : double;
var I      : integer;
    C1, C2 : double;
begin
  C1 := (B-A)/2;
  C2 := (B+A)/2;
  Result := 0;
  For I := 0 to Order-1 do
    Result := Result + Weight[I] * F(C1*Roots[I] + C2);
  Result := C1 * Result;
end;

var I : integer;

begin
  PrepCoef;
  LegRoots;

  Write('Roots:  ');
  for I := 0 to Order-1 do
    Write (' ',Roots[I]:13:10);
  Writeln;

  Write('Weight: ');
  for I := 0 to Order-1 do
    Write (' ', Weight[I]:13:10);
  writeln;

  Writeln('Integrating Exp(x) over [-3, 3]: ',LegInt(-3,3):13:10);
  Writeln('Actual value: ',Exp(3)-Exp(-3):13:10);
  Readln;
end.
```



```txt

Roots:    0.9061798459  0.5384693101  0.0000000000 -0.5384693101 -0.9061798459
Weight:   0.2369268851  0.4786286705  0.5688888889  0.4786286705  0.2369268851
Integrating Exp(X) over [-3, 3]: 20.0355777184
Actual value: 20.0357498548

```



## D

```d
import std.stdio, std.math;

immutable struct GaussLegendreQuadrature(size_t N, FP=double,
                                         size_t NBITS=50) {
    immutable static double[N] lroots, weight;
    alias FP[N + 1][N + 1] CoefMat;

    pure nothrow @safe @nogc static this() {
        static FP legendreEval(in ref FP[N + 1][N + 1] lcoef,
                               in int n, in FP x) pure nothrow {
            FP s = lcoef[n][n];
            foreach_reverse (immutable i; 1 .. n+1)
                s = s * x + lcoef[n][i - 1];
            return s;
        }

        static FP legendreDiff(in ref CoefMat lcoef,
                               in int n, in FP x)
        pure nothrow @safe @nogc {
            return n * (x * legendreEval(lcoef, n, x) -
                        legendreEval(lcoef, n - 1, x)) /
                   (x ^^ 2 - 1);
        }

        CoefMat lcoef = 0.0;
        legendreCoefInit(/*ref*/ lcoef);

        // Legendre roots:
        foreach (immutable i; 1 .. N + 1) {
            FP x = cos(PI * (i - 0.25) / (N + 0.5));
            FP x1;
            do {
                x1 = x;
                x -= legendreEval(lcoef, N, x) /
                     legendreDiff(lcoef, N, x);
            } while (feqrel(x, x1) < NBITS);
            lroots[i - 1] = x;
            x1 = legendreDiff(lcoef, N, x);
            weight[i - 1] = 2 / ((1 - x ^^ 2) * (x1 ^^ 2));
        }
    }

    static private void legendreCoefInit(ref CoefMat lcoef)
    pure nothrow @safe @nogc {
        lcoef[0][0] = lcoef[1][1] = 1;
        foreach (immutable int n; 2 .. N + 1) { // n must be signed.
            lcoef[n][0] = -(n - 1) * lcoef[n - 2][0] / n;
            foreach (immutable i; 1 .. n + 1)
                lcoef[n][i] = ((2 * n - 1) * lcoef[n - 1][i - 1] -
                               (n - 1) * lcoef[n - 2][i]) / n;
        }
    }

    static public FP integrate(in FP function(/*in*/ FP x) pure nothrow @safe @nogc f,
                               in FP a, in FP b)
    pure nothrow @safe @nogc {
        immutable FP c1 = (b - a) / 2;
        immutable FP c2 = (b + a) / 2;
        FP sum = 0.0;
        foreach (immutable i; 0 .. N)
            sum += weight[i] * f(c1 * lroots[i] + c2);
        return c1 * sum;
    }
}

void main() {
    GaussLegendreQuadrature!(5, real) glq;
    writeln("Roots:  ", glq.lroots);
    writeln("Weight: ", glq.weight);
    writefln("Integrating exp(x) over [-3, 3]: %10.12f",
             glq.integrate(&exp, -3, 3));
    writefln("Compred to actual:               %10.12f",
             3.0.exp - exp(-3.0));
}
```

```txt
Roots:  [0.90618, 0.538469, 0, -0.538469, -0.90618]
Weight: [0.236927, 0.478629, 0.568889, 0.478629, 0.236927]
Integrating exp(x) over [-3, 3]: 20.035577718386
Compred to actual:               20.035749854820
```



## Fortran


```Fortran
! Works with gfortran but needs the option
!   -assume realloc_lhs
! when compiled with Intel Fortran.

program gauss
  implicit none
  integer, parameter :: p = 16 ! quadruple precision
  integer            :: n = 10, k
  real(kind=p), allocatable :: r(:,:)
  real(kind=p)       :: z, a, b, exact
  do n = 1,20
    a = -3; b = 3
    r = gaussquad(n)
    z = (b-a)/2*dot_product(r(2,:),exp((a+b)/2+r(1,:)*(b-a)/2))
    exact = exp(3.0_p)-exp(-3.0_p)
    print "(i0,1x,g0,1x,g10.2)",n, z, z-exact
  end do

  contains

  function gaussquad(n) result(r)
  integer                 :: n
  real(kind=p), parameter :: pi = 4*atan(1._p)
  real(kind=p)            :: r(2, n), x, f, df, dx
  integer                 :: i,  iter
  real(kind = p), allocatable :: p0(:), p1(:), tmp(:)

  p0 = [1._p]
  p1 = [1._p, 0._p]

  do k = 2, n
     tmp = ((2*k-1)*[p1,0._p]-(k-1)*[0._p, 0._p,p0])/k
     p0 = p1; p1 = tmp
  end do
  do i = 1, n
    x = cos(pi*(i-0.25_p)/(n+0.5_p))
    do iter = 1, 10
      f = p1(1); df = 0._p
      do k = 2, size(p1)
        df = f + x*df
        f  = p1(k) + x * f
      end do
      dx =  f / df
      x = x - dx
      if (abs(dx)<10*epsilon(dx)) exit
    end do
    r(1,i) = x
    r(2,i) = 2/((1-x**2)*df**2)
  end do
  end function
end program

```



```txt

n numerical integral                       error
--------------------------------------------------
1 6.00000000000000000000000000000000   -14.
2 17.4874646410555689643606840462449   -2.5
3 19.8536919968055821921309108927158   -.18
4 20.0286883952907008527738054439858   -.71E-02
5 20.0355777183855621539285357252751   -.17E-03
6 20.0357469750923438830654575585499   -.29E-05
7 20.0357498197266007755718729372892   -.35E-07
8 20.0357498544945172882260918041684   -.33E-09
9 20.0357498548174338368864419454859   -.24E-11
10 20.0357498548197898711175766908548   -.14E-13
11 20.0357498548198037305529147159695   -.67E-16
12 20.0357498548198037976759531014464   -.27E-18
13 20.0357498548198037979482458119095   -.94E-21
14 20.0357498548198037979491844483597   -.28E-23
15 20.0357498548198037979491872317190   -.72E-26
16 20.0357498548198037979491872388913   -.40E-28
17 20.0357498548198037979491872389166   -.15E-28
18 20.0357498548198037979491872389259   -.58E-29
19 20.0357498548198037979491872388910   -.41E-28
20 20.0357498548198037979491872388495   -.82E-28

```



## Go

Implementation pretty much by the methods given in the task description.

```go
package main

import (
    "fmt"
    "math"
)

// cFunc for continuous function.  A type definition for convenience.
type cFunc func(float64) float64

func main() {
    fmt.Println("integral:", glq(math.Exp, -3, 3, 5))
}

// glq integrates f from a to b by Guass-Legendre quadrature using n nodes.
// For the task, it also shows the intermediate values determining the nodes:
// the n roots of the order n Legendre polynomal and the corresponding n
// weights used for the integration.
func glq(f cFunc, a, b float64, n int) float64 {
    x, w := glqNodes(n, f)
    show := func(label string, vs []float64) {
        fmt.Printf("%8s: ", label)
        for _, v := range vs {
            fmt.Printf("%8.5f ", v)
        }
        fmt.Println()
    }
    show("nodes", x)
    show("weights", w)
    var sum float64
    bma2 := (b - a) * .5
    bpa2 := (b + a) * .5
    for i, xi := range x {
        sum += w[i] * f(bma2*xi+bpa2)
    }
    return bma2 * sum
}

// glqNodes computes both nodes and weights for a Gauss-Legendre
// Quadrature integration.  Parameters are n, the number of nodes
// to compute and f, a continuous function to integrate.  Return
// values have len n.
func glqNodes(n int, f cFunc) (node []float64, weight []float64) {
    p := legendrePoly(n)
    pn := p[n]
    n64 := float64(n)
    dn := func(x float64) float64 {
        return (x*pn(x) - p[n-1](x)) * n64 / (x*x - 1)
    }
    node = make([]float64, n)
    for i := range node {
        x0 := math.Cos(math.Pi * (float64(i+1) - .25) / (n64 + .5))
        node[i] = newtonRaphson(pn, dn, x0)
    }
    weight = make([]float64, n)
    for i, x := range node {
        dnx := dn(x)
        weight[i] = 2 / ((1 - x*x) * dnx * dnx)
    }
    return
}

// legendrePoly constructs functions that implement Lengendre polynomials.
// This is done by function composition by recurrence relation (Bonnet's.)
// For given n, n+1 functions are returned, computing P0 through Pn.
func legendrePoly(n int) []cFunc {
    r := make([]cFunc, n+1)
    r[0] = func(float64) float64 { return 1 }
    r[1] = func(x float64) float64 { return x }
    for i := 2; i <= n; i++ {
        i2m1 := float64(i*2 - 1)
        im1 := float64(i - 1)
        rm1 := r[i-1]
        rm2 := r[i-2]
        invi := 1 / float64(i)
        r[i] = func(x float64) float64 {
            return (i2m1*x*rm1(x) - im1*rm2(x)) * invi
        }
    }
    return r
}

// newtonRaphson is general purpose, although totally primitive, simply
// panicking after a fixed number of iterations without convergence to
// a fixed error.  Parameter f must be a continuous function,
// df its derivative, x0 an initial guess.
func newtonRaphson(f, df cFunc, x0 float64) float64 {
    for i := 0; i < 30; i++ {
        x1 := x0 - f(x0)/df(x0)
        if math.Abs(x1-x0) <= math.Abs(x0*1e-15) {
            return x1
        }
        x0 = x1
    }
    panic("no convergence")
}
```

```txt

   nodes:  0.90618  0.53847  0.00000 -0.53847 -0.90618
 weights:  0.23693  0.47863  0.56889  0.47863  0.23693
integral: 20.035577718385564

```



## Haskell

Integration formula

```haskell
gaussLegendre n f a b = d*sum [ w x*f(m + d*x) | x <- roots ]
  where d = (b - a)/2
        m = (b + a)/2
        w x = 2/(1-x^2)/(legendreP' n x)^2
        roots = map (findRoot (legendreP n) (legendreP' n) . x0) [1..n]
        x0 i = cos (pi*(i-1/4)/(n+1/2))
```


Calculation of Legendre polynomials

```haskell
legendreP n x = go n 1 x
  where go 0 p2 _  = p2
        go 1 _  p1 = p1
        go n p2 p1 = go (n-1) p1 $ ((2*n-1)*x*p1 - (n-1)*p2)/n

legendreP' n x = n/(x^2-1)*(x*legendreP n x - legendreP (n-1) x)
```


Universal auxilary functions

```haskell
findRoot f df = fixedPoint (\x -> x - f x / df x)

fixedPoint f x | abs (fx - x) < 1e-15 = x
               | otherwise = fixedPoint f fx
  where fx = f x
```


Integration on a given mesh using Gauss-Legendre quadrature:

```haskell
integrate _ []     = 0
integrate f (m:ms) = sum $ zipWith (gaussLegendre 5 f) (m:ms) ms
```


  λ> integrate exp [-3,3]
  20.035577718385547
  λ> integrate exp [-3..3]
  20.03574985481217
  λ> gaussLegendre 10 exp (-3) 3
  20.035749854819695

Analytical solution
  λ> exp 3 - exp (-3)
  20.035749854819805


## J

'''Solution:'''

```j
P =: 3 :0 NB. list of coefficients for yth Legendre polynomial
   if. y<:1 do. 1{.~->:y return. end.
   y%~ (<:(,~+:)y) -/@:* (0,P<:y),:(P y-2)
)

getpoints =: 3 :0 NB. points,:weights for y points
   x=. 1{:: p. p=.P y
   w=. 2% (-.*:x)**:(p..p)p.x
   x,:w
)

GaussLegendre =: 1 :0 NB. npoints function GaussLegendre (a,b)
:
   'x w'=.getpoints x
   -:(-~/y)* +/w* u -:((+/,-~/)y)p.x
)
```

```j
   5 ^ GaussLegendre _3 3
20.0356
```



## Java

```java
import static java.lang.Math.*;
import java.util.function.Function;

public class Test {
    final static int N = 5;

    static double[] lroots = new double[N];
    static double[] weight = new double[N];
    static double[][] lcoef = new double[N + 1][N + 1];

    static void legeCoef() {
        lcoef[0][0] = lcoef[1][1] = 1;

        for (int n = 2; n <= N; n++) {

            lcoef[n][0] = -(n - 1) * lcoef[n - 2][0] / n;

            for (int i = 1; i <= n; i++) {
                lcoef[n][i] = ((2 * n - 1) * lcoef[n - 1][i - 1]
                        - (n - 1) * lcoef[n - 2][i]) / n;
            }
        }
    }

    static double legeEval(int n, double x) {
        double s = lcoef[n][n];
        for (int i = n; i > 0; i--)
            s = s * x + lcoef[n][i - 1];
        return s;
    }

    static double legeDiff(int n, double x) {
        return n * (x * legeEval(n, x) - legeEval(n - 1, x)) / (x * x - 1);
    }

    static void legeRoots() {
        double x, x1;
        for (int i = 1; i <= N; i++) {
            x = cos(PI * (i - 0.25) / (N + 0.5));
            do {
                x1 = x;
                x -= legeEval(N, x) / legeDiff(N, x);
            } while (x != x1);

            lroots[i - 1] = x;

            x1 = legeDiff(N, x);
            weight[i - 1] = 2 / ((1 - x * x) * x1 * x1);
        }
    }

    static double legeInte(Function<Double, Double> f, double a, double b) {
        double c1 = (b - a) / 2, c2 = (b + a) / 2, sum = 0;
        for (int i = 0; i < N; i++)
            sum += weight[i] * f.apply(c1 * lroots[i] + c2);
        return c1 * sum;
    }

    public static void main(String[] args) {
        legeCoef();
        legeRoots();

        System.out.print("Roots: ");
        for (int i = 0; i < N; i++)
            System.out.printf(" %f", lroots[i]);

        System.out.print("\nWeight:");
        for (int i = 0; i < N; i++)
            System.out.printf(" %f", weight[i]);

        System.out.printf("%nintegrating Exp(x) over [-3, 3]:%n\t%10.8f,%n"
                + "compared to actual%n\t%10.8f%n",
                legeInte(x -> exp(x), -3, 3), exp(3) - exp(-3));
    }
}
```


```txt
Roots:  0,906180 0,538469 0,000000 -0,538469 -0,906180
Weight: 0,236927 0,478629 0,568889 0,478629 0,236927
integrating Exp(x) over [-3, 3]:
	20,03557772,
compared to actual
	20,03574985
```



## Julia

This function computes the points and weights of an ''N''-point Gauss–Legendre quadrature rule on the interval (''a'',''b'').  It uses the O(''N''<sup>2</sup>) algorithm described in Trefethen & Bau, ''Numerical Linear Algebra'', which finds the points and weights by computing the eigenvalues and eigenvectors of a real-symmetric tridiagonal matrix:

```julia
using LinearAlgebra

function gauss(a, b, N)
    λ, Q = eigen(SymTridiagonal(zeros(N), [n / sqrt(4n^2 - 1) for n = 1:N-1]))
    @. (λ + 1) * (b - a) / 2 + a, [2Q[1, i]^2 for i = 1:N] * (b - a) / 2
end
```

(This code is a simplified version of the <code>Base.gauss</code> subroutine in the Julia standard library.)
```txt

julia> x, w = gauss(-3, 3, 5)
([-2.71854, -1.61541, 1.33227e-15, 1.61541, 2.71854], [0.710781, 1.43589, 1.70667, 1.43589, 0.710781])

julia> sum(exp.(x) .* w)
20.03557771838554

```



## Kotlin

```scala
import java.lang.Math.*

class Legendre(val N: Int) {
    fun evaluate(n: Int, x: Double) = (n downTo 1).fold(c[n][n]) { s, i -> s * x + c[n][i - 1] }

    fun diff(n: Int, x: Double) = n * (x * evaluate(n, x) - evaluate(n - 1, x)) / (x * x - 1)

    fun integrate(f: (Double) -> Double, a: Double, b: Double): Double {
        val c1 = (b - a) / 2
        val c2 = (b + a) / 2
        return c1 * (0 until N).fold(0.0) { s, i -> s + weights[i] * f(c1 * roots[i] + c2) }
    }

    private val roots = DoubleArray(N)
    private val weights = DoubleArray(N)
    private val c = Array(N + 1) { DoubleArray(N + 1) }    // coefficients

    init {
        // coefficients:
        c[0][0] = 1.0
        c[1][1] = 1.0
        for (n in 2..N) {
            c[n][0] = (1 - n) * c[n - 2][0] / n
            for (i in 1..n)
                c[n][i] = ((2 * n - 1) * c[n - 1][i - 1] - (n - 1) * c[n - 2][i]) / n
        }

        // roots:
        var x: Double
        var x1: Double
        for (i in 1..N) {
            x = cos(PI * (i - 0.25) / (N + 0.5))
            do {
                x1 = x
                x -= evaluate(N, x) / diff(N, x)
            } while (x != x1)

            x1 = diff(N, x)
            roots[i - 1] = x
            weights[i - 1] = 2 / ((1 - x * x) * x1 * x1)
        }

        print("Roots:")
        roots.forEach { print(" %f".format(it)) }
        println()
        print("Weights:")
        weights.forEach { print(" %f".format(it)) }
        println()
    }
}

fun main(args: Array<String>) {
    val legendre = Legendre(5)
    println("integrating Exp(x) over [-3, 3]:")
    println("\t%10.8f".format(legendre.integrate(Math::exp, -3.0, 3.0)))
    println("compared to actual:")
    println("\t%10.8f".format(exp(3.0) - exp(-3.0)))
}
```

```txt
Roots: 0.906180 0.538469 0.000000 -0.538469 -0.906180
Weights: 0.236927 0.478629 0.568889 0.478629 0.236927
integrating Exp(x) over [-3, 3]:
	20.03557772
compared to actual:
	20.03574985
```



## Lua


```Lua
local order = 0

local legendreRoots = {}
local legendreWeights = {}

local function legendre(term, z)
    if (term == 0) then
        return 1
    elseif (term == 1) then
        return z
    else
        return ((2 * term - 1) * z * legendre(term - 1, z) - (term - 1) * legendre(term - 2, z)) / term
    end
end

local function legendreDerivative(term, z)
    if (term == 0) then
        return 0
    elseif (term == 1) then
        return 1
    else
        return ( term * ((z * legendre(term, z)) - legendre(term - 1, z))) / (z * z - 1)
    end
end

local function getLegendreRoots()
    local y, y1

    for index = 1, order do
        y = math.cos(math.pi * (index - 0.25) / (order + 0.5))

        repeat
            y1 = y
            y = y - (legendre(order, y) / legendreDerivative(order, y))
        until y == y1

        table.insert(legendreRoots, y)
    end
end

local function getLegendreWeights()
    for index = 1, order do
        local weight = 2 / ((1 - (legendreRoots[index]) ^ 2) * (legendreDerivative(order, legendreRoots[index])) ^ 2)
        table.insert(legendreWeights, weight)
    end
end

function gaussLegendreQuadrature(f, lowerLimit, upperLimit, n)
    order = n

    do
        getLegendreRoots()
        getLegendreWeights()
    end

    local c1 = (upperLimit - lowerLimit) / 2
    local c2 = (upperLimit + lowerLimit) / 2
    local sum = 0

    for i = 1, order do
        sum = sum + legendreWeights[i] * f(c1 * legendreRoots[i] + c2)
    end

    return c1 * sum
end

do
    print(gaussLegendreQuadrature(function(x) return math.exp(x) end, -3, 3, 5))
end
```

```txt
20.035577718386
```



## Mathematica

code assumes function to be integrated has attribute Listable which is true of most built in Mathematica functions

```Mathematica
gaussLegendreQuadrature[func_, {a_, b_}, degree_: 5] :=
Block[{nodes, x, weights},
 nodes = Cases[NSolve[LegendreP[degree, x] == 0, x], _?NumericQ, Infinity];
 weights = 2 (1 - nodes^2)/(degree LegendreP[degree - 1, nodes])^2;
 (b - a)/2 weights.func[(b - a)/2 nodes + (b + a)/2]]

gaussLegendreQuadrature[Exp, {-3, 3}]
```

```txt
20.0356
```




## MATLAB

Translated from the Python solution.

```MATLAB

%Integration using Gauss-Legendre quad
%Does almost the same as 'integral' in MATLAB
function y=GLGD_int(fun,xmin,xmax,n)
%fun: the intergrand as a function handle
%xmin: lower boundary of integration
%xmax: upper boundary of integration
%n: order of polynomials used (number of integration ponts)
[x_IP,weight]=GLGD_para(n);
%assign global coordinates to the integraton points
x_eval=x_IP*(xmax-xmin)/2+(xmax+xmin)/2;
y=0;
for aa=1:n
    y=y+feval(fun,x_eval(aa))*weight(aa)*(xmax-xmin)/2;
end
end

function [x_IP,weight]=GLGD_para(n)
%n: the order of the polynomial
x_IP=legendreRoot(n,10^(-16));
weight=2./(1-x_IP.^2)./diff_legendrePoly(x_IP,n).^2;
end

%roots of the Legendre Polynomial using Newton-Raphson
function x_IP=legendreRoot(n,tol)
%n: order of the polynomial
%tol: tolerence of the error
if n<2
    disp('No root can be found');
else
    root=zeros(1,floor(n/2));
    for aa=1:floor(n/2) %iterate to find half of the roots
        x=cos(pi*(aa-0.25)/(n+0.5));
        err=10*tol;
        iter=0;
        while (err>tol)&&(iter<1000)
            dx=-legendrePoly(x,n)/diff_legendrePoly(x,n);
            x=x+dx;
            iter=iter+1;
            err=abs(legendrePoly(x,n));
        end
        root(aa)=x;
    end
    if mod(n,2)==0
        x_IP=[-1*root,root];
    else
        x_IP=[-1*root,0,root];
    end
    x_IP=sort(x_IP);
end
end

%derivative of the Legendre Polynomial
function y=diff_legendrePoly(x_IP,n)
%n: order of the polynomial
%x_IP: coordinates of the integration points
if n==0
    y=0;
else
    y=n./(x_IP.^2-1).*(x_IP.*legendrePoly(x_IP,n)-legendrePoly(x_IP,n-1));
end
end

%Produces Legendre Polynomials
function y=legendrePoly(x,n)
%n: order of polynomial
%x: input x
if n==0
    y=1;
elseif n==1
    y=x;
else
    y=((2*n-1).*x.*legendrePoly(x,n-1)-(n-1)*legendrePoly(x,n-2))/n;
end
end

```

```txt
20.0356
```



## Maxima


```maxima
gauss_coeff(n) := block([p, q, v, w],
   p: expand(legendre_p(n, x)),
   q: expand(n/2*diff(p, x)*legendre_p(n - 1, x)),
   v: map(rhs, bfallroots(p)),
   w: map(lambda([z], 1/subst([x = z], q)), v),
   [map(bfloat, v), map(bfloat, w)])$

gauss_int(f, a, b, n) := block([u, x, w, c, h],
   u: gauss_coeff(n),
   x: u[1],
   w: u[2],
   c: bfloat((a + b)/2),
   h: bfloat((b - a)/2),
   h*sum(w[i]*bfloat(f(c + x[i]*h)), i, 1, n))$


fpprec: 40$


gauss_int(lambda([x], 4/(1 + x^2)), 0, 1, 20);
/* 3.141592653589793238462643379852215927697b0 */

% - bfloat(%pi);
/* -3.427286956499858315999116083264403489053b-27 */


gauss_int(exp, -3, 3, 5);
/* 2.003557771838556215392853572527509393154b1 */

% - bfloat(integrate(exp(x), x, -3, 3));
/* -1.721364342416440206515136565621888185351b-4 */
```



## Nim

```nim

import math, strformat

proc legendreIn(x: float, n: int): float =

  template prev1(idx: int; pn1: float): float =
    (2*idx - 1).float * x * pn1

  template prev2(idx: int; pn2: float): float =
    (idx-1).float * pn2

  if n == 0:
    return 1.0
  elif n == 1:
    return x
  else:
    var
      p1 = float x
      p2 = 1.0
    for i in 2 .. n:
      result = (i.prev1(p1) - i.prev2(p2)) / i.float
      p2 = p1
      p1 = result

proc deriveLegendreIn(x: float, n: int): float =
  template calcresult(curr, prev: float): untyped =
    n.float / (x^2 - 1) * (x * curr - prev)
  result = calcresult(x.legendreIn n, x.legendreIn(n-1))

func guess(n, i: int): float =
  cos(PI * (i.float - 0.25) / (n.float + 0.5))

proc nodes(n: int): seq[(float, float)] =
  result = newseq[(float, float)](n)
  template calc(x: float): untyped =
    x.legendreIn(n) / x.deriveLegendreIn(n)

  for i in 0 .. result.high:
    var x = guess(n, i+1)
    block newton:
      var x0 = x
      x -= calc x
      while abs(x-x0) > 1e-12:
        x0 = x
        x -= calc x

    result[i][0] = x
    result[i][1] = 2 / ((1.0 - x^2) * (x.deriveLegendreIn n)^2)

proc integ(f: proc(x: float): float; ns, p1, p2: int): float =
  template dist: untyped =
    (p2 - p1).float / 2.0
  template avg: untyped =
    (p1 + p2).float / 2.0
  result = dist()
  var
    sum = 0'f
    thenodes = newseq[float](ns)
    weights = newseq[float](ns)
  for i, nw in ns.nodes:
    sum += nw[1] * f(dist() * nw[0] + avg())
    thenodes[i] = nw[0]
    weights[i] = nw[1]

  let apos = ":"
  stdout.write fmt"""{"nodes":>8}{apos}"""
  for n in thenodes:
    stdout.write &" {n:>6.5f}"
  stdout.write "\n"
  stdout.write &"""{"weights":>8}{apos}"""
  for w in weights:
    stdout.write &" {w:>6.5f}"
  stdout.write "\n"
  result *= sum

proc main =
  echo "integral: ", integ(exp, 5, -3, 3)

main()

```

```txt

   nodes: 0.90618 0.53847 0.00000 -0.53847 -0.90618
 weights: 0.23693 0.47863 0.56889 0.47863 0.23693
integral: 20.03557634353638

```



## OCaml


```OCaml
let rec leg n x = match n with (* Evaluate Legendre polynomial *)
   | 0 -> 1.0
   | 1 -> x
   | k -> let u = 1.0 -. 1.0 /. float k in
      (1.0+.u)*.x*.(leg (k-1) x) -. u*.(leg (k-2) x);;

let leg' n x = match n with (* derivative *)
   | 0 -> 0.0
   | 1 -> 1.0
   | _ -> ((leg (n-1) x) -. x*.(leg n x)) *. (float n)/.(1.0-.x*.x);;

let approx_root k n = (* Reversed Francesco Tricomi: 1 <= k <= n *)
   let pi = acos (-1.0) and s = float(2*n)
   and t = 1.0 +. float(1-4*k)/.float(4*n+2) in
   (1.0 -. (float (n-1))/.(s*.s*.s))*.cos(pi*.t);;

let rec refine r n = (* Newton-Raphson *)
   let r1 = r -. (leg n r)/.(leg' n r) in
   if abs_float (r-.r1) < 2e-16 then r1 else refine r1 n;;

let root k n = refine (approx_root k n) n;;

let node k n = (* Abscissa and weight *)
   let r = root k n in
   let deriv = leg' n r in
   let w = 2.0/.((1.0-.r*.r)*.(deriv*.deriv)) in
   (r,w);;

let nodes n =
   let rec aux k = if k > n then [] else node k n :: aux (k+1)
   in aux 1;;

let quadrature n f a b =
   let f1 x = f ((x*.(b-.a) +. a +. b)*.0.5) in
   let eval s (x,w) = s +. w*.(f1 x) in
   0.5*.(b-.a)*.(List.fold_left eval 0.0 (nodes n));;
```

which can be used in:

```OCaml
let calc n =
   Printf.printf
      "Gauss-Legendre %2d-point quadrature for exp over [-3..3] = %.16f\n"
      n (quadrature n exp (-3.0) 3.0);;

calc 5;;
calc 10;;
calc 15;;
calc 20;;
```

```txt

Gauss-Legendre  5-point quadrature for exp over [-3..3] = 20.0355777183855608
Gauss-Legendre 10-point quadrature for exp over [-3..3] = 20.0357498548197839
Gauss-Legendre 15-point quadrature for exp over [-3..3] = 20.0357498548198052
Gauss-Legendre 20-point quadrature for exp over [-3..3] = 20.0357498548198052

```

This shows convergence to the correct double-precision value of the integral

```Ocaml
Printf.printf "%.16f\n" ((exp 3.0) -.(exp (-3.0)));;
20.0357498548198052
```

although going beyond 20 points starts reducing the accuracy, due to accumulated rounding errors.


## PARI/GP

This task is easy in GP thanks to built-in support for Legendre polynomials and efficient (Schonhage-Gourdon) polynomial root finding.

```parigp
GLq(f,a,b,n)={
  my(P=pollegendre(n),Pp=P',x=polroots(P));
  (b-a)*sum(i=1,n,f((b-a)*x[i]/2+(a+b)/2)/(1-x[i]^2)/subst(Pp,'x,x[i])^2)
};
# \\ Turn on timer
GLq(x->exp(x), -3, 3, 5) \\ As of version 2.4.4, this can be written GLq(exp, -3, 3, 5)
```

```txt
time = 0 ms.
%1 = 20.035577718385562153928535725275093932 + 0.E-37*I
```


Gauss-Legendre quadrature is built-in from 2.9 forward.

```parigp
intnumgauss(x=-3, 3, exp(x), intnumgaussinit(5))
intnumgauss(x=-3, 3, exp(x)) \\ determine number of points automatically; all digits shown should be accurate
```

```txt
%1 = 20.035746975092343883065457558549925374
%2 = 20.035749854819803797949187238931656120
```



## ooRexx


```oorexx
/*---------------------------------------------------------------------
* 31.10.2013 Walter Pachl  Translation from REXX (from PL/I)
*                          using ooRexx' rxmath package
*                          which limits the precision to 16 digits
*--------------------------------------------------------------------*/
prec=60
Numeric Digits prec
epsilon=1/10**prec
pi=3.141592653589793238462643383279502884197169399375105820974944592307
exact = RxCalcExp(3,prec)-RxCalcExp(-3,prec)
Do n = 1 To 20
  a = -3; b = 3
  r.=0
  call gaussquad
  sum=0
  Do j=1 To n
    sum=sum + r.2.j * RxCalcExp((a+b)/2+r.1.j*(b-a)/2,prec)
    End
  z = (b-a)/2 * sum
  Say right(n,2) format(z,2,40) format(z-exact,2,4,,0)
  End
  Say  '  ' exact '(exact)'
  Exit

 gaussquad:
   p0.0=1; p0.1=1
   p1.0=2; p1.1=1; p1.2=0
   Do k = 2 To n
     tmp.0=p1.0+1
      Do L = 1 To p1.0
        tmp.l = p1.l
        End
      tmp.l=0
      tmp2.0=p0.0+2
      tmp2.1=0
      tmp2.2=0
      Do L = 1 To p0.0
        l2=l+2
        tmp2.l2=p0.l
        End
      Do j=1 To tmp.0
        tmp.j = ((2*k-1)*tmp.j - (k-1)*tmp2.j)/k
        End
      p0.0=p1.0
      Do j=1 To p0.0
        p0.j = p1.j
        End
      p1.0=tmp.0
      Do j=1 To p1.0
        p1.j=tmp.j
        End
   End
   Do i = 1 To n
     x = RxCalcCos(pi*(i-0.25)/(n+0.5),prec,'R')
     Do iter = 1 To 10
       f = p1.1; df = 0
       Do k = 2 To p1.0
         df = f + x*df
         f  = p1.k + x * f
         End
       dx =  f / df
       x = x - dx
       If abs(dx) < epsilon Then Leave
       End
     r.1.i = x
     r.2.i = 2/((1-x**2)*df**2)
     End
   Return

::requires 'rxmath' LIBRARY
```

Output:

```txt
 1  6.0000000000000000000000000000000000000000 -1.4036E+1
 2 17.4874646410555686000000000000000000000000 -2.5483
 3 19.8536919968055914500000000000000000000000 -1.8206E-1
 4 20.0286883952907032246391703165575495371776 -7.0615E-3
 5 20.0355777183855623345965085871972344078167 -1.7214E-4
 6 20.0357469750923433031000982816859525440756 -2.8797E-6
 7 20.0357498197266007450081506439422093510041 -3.5093E-8
 8 20.0357498544945192648654062025059252571210 -3.2529E-10
 9 20.0357498548174362426073138353882519240177 -2.3698E-12
10 20.0357498548197905075149387536361754813374 -1.5552E-14
11 20.0357498548198049052166074059523608613749 -1.1548E-15
12 20.0357498548198068119347633275378821700762  7.5193E-16
13 20.0357498548198063256375618073806663013152  2.6564E-16
14 20.0357498548198035202546245888922276792447 -2.5397E-15
15 20.0357498548198027919824444452012138941729 -3.2680E-15
16 20.0357498548198037471314715729442546019171 -2.3129E-15
17 20.0357498548198067452377635761033686644343  6.8524E-16
18 20.0357498548198042026084719530842757694873 -1.8574E-15
19 20.0357498548198042304714191024916472961732 -1.8295E-15
20 20.0357498548198034525095801113268011014944 -2.6075E-15
   20.03574985481980606 (exact)
```



## Pascal

See [[Numerical_integration/Gauss-Legendre_Quadrature#Delphi | Delphi]]


## Perl

```perl
use List::Util qw(sum);
use constant pi => 3.14159265;

sub legendre_pair {
    my($n, $x) = @_;
    if ($n == 1) { return $x, 1 }
    my ($m1, $m2) = legendre_pair($n - 1, $x);
    my $u = 1 - 1 / $n;
    (1 + $u) * $x * $m1 - $u * $m2, $m1;
}

sub legendre {
    my($n, $x) = @_;
    (legendre_pair($n, $x))[0]
}

sub legendre_prime {
    my($n, $x) = @_;
    if ($n == 0) { return 0 }
    if ($n == 1) { return 1 }
    my ($m0, $m1) = legendre_pair($n, $x);
    ($m1 - $x * $m0) * $n / (1 - $x**2);
}

sub approximate_legendre_root {
    my($n, $k) = @_;
    my $t = (4*$k - 1) / (4*$n + 2);
    (1 - ($n - 1) / (8 * $n**3)) * cos(pi * $t);
}

sub newton_raphson {
    my($n, $r) = @_;
    while (abs(my $dr = - legendre($n,$r) / legendre_prime($n,$r)) >= 2e-16) {
        $r += $dr;
    }
    $r;
}

sub legendre_root {
    my($n, $k) = @_;
    newton_raphson($n, approximate_legendre_root($n, $k));
}

sub weight {
    my($n, $r) = @_;
    2 / ((1 - $r**2) * legendre_prime($n, $r)**2)
}

sub nodes {
    my($n) = @_;
    my %node;
    $node{'0'} = weight($n, 0) if 0 != $n%2;
    for (1 .. int $n/2) {
        my $r = legendre_root($n, $_);
        my $w = weight($n, $r);
        $node{$r} = $w; $node{-$r} = $w;
    }
    return %node
}

sub quadrature {
    our($n, $a, $b) = @_;
    sub scale { ($_[0] * ($b - $a) + $a + $b) / 2 }
    %nodes = nodes($n);
    ($b - $a) / 2 * sum map { $nodes{$_} * exp(scale($_)) } keys %nodes;
}

printf("Gauss-Legendre %2d-point quadrature ∫₋₃⁺³ exp(x) dx ≈ %.13f\n", $_, quadrature($_, -3, +3) )
        for 5 .. 10, 20;

```

```txt
Gauss-Legendre  5-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0355777183856
Gauss-Legendre  6-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357469750923
Gauss-Legendre  7-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498197266
Gauss-Legendre  8-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498544945
Gauss-Legendre  9-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498548174
Gauss-Legendre 10-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498548198
Gauss-Legendre 20-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498548198
```



## Perl 6

A free translation of the OCaml solution. We save half the effort to calculate the nodes by exploiting the (skew-)symmetry of the Legendre Polynomials.
The evaluation of Pn(x) is kept linear in n by also passing Pn-1(x) in the recursion.

The <tt>quadrature</tt> function allows passing in a precalculated list of nodes for repeated integrations.

Note: The calculations of Pn(x) and P'n(x) could be combined to further reduce duplicated effort. We also could cache P'n(x) from the last Newton-Raphson step for the weight calculation.


```perl6
multi legendre-pair(    1 , $x) { $x, 1 }
multi legendre-pair(Int $n, $x) {
    my ($m1, $m2) = legendre-pair($n - 1, $x);
    my \u = 1 - 1 / $n;
    (1 + u) * $x * $m1 - u * $m2, $m1;
}

multi legendre(    0 , $ ) { 1 }
multi legendre(Int $n, $x) { legendre-pair($n, $x)[0] }

multi legendre-prime(    0 , $ ) { 0 }
multi legendre-prime(    1 , $ ) { 1 }
multi legendre-prime(Int $n, $x) {
    my ($m0, $m1) = legendre-pair($n, $x);
    ($m1 - $x * $m0) * $n / (1 - $x**2);
}

sub approximate-legendre-root(Int $n, Int $k) {
    # Approximation due to Francesco Tricomi
    my \t = (4*$k - 1) / (4*$n + 2);
    (1 - ($n - 1) / (8 * $n**3)) * cos(pi * t);
}

sub newton-raphson(&f, &f-prime, $r is copy, :$eps = 2e-16) {
    while abs(my \dr = - f($r) / f-prime($r)) >= $eps {
        $r += dr;
    }
    $r;
}

sub legendre-root(Int $n, Int $k) {
    newton-raphson(&legendre.assuming($n), &legendre-prime.assuming($n),
                   approximate-legendre-root($n, $k));
}

sub weight(Int $n, $r) { 2 / ((1 - $r**2) * legendre-prime($n, $r)**2) }

sub nodes(Int $n) {
    flat gather {
        take 0 => weight($n, 0) if $n !%% 2;
        for 1 .. $n div 2 {
            my $r = legendre-root($n, $_);
            my $w = weight($n, $r);
            take $r => $w, -$r => $w;
        }
    }
}

sub quadrature(Int $n, &f, $a, $b, :@nodes = nodes($n)) {
    sub scale($x) { ($x * ($b - $a) + $a + $b) / 2 }
    ($b - $a) / 2 * [+] @nodes.map: { .value * f(scale(.key)) }
}

say "Gauss-Legendre $_.fmt('%2d')-point quadrature ∫₋₃⁺³ exp(x) dx ≈ ",
         quadrature($_, &exp, -3, +3) for flat 5 .. 10, 20;
```


```txt
Gauss-Legendre  5-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0355777183856
Gauss-Legendre  6-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357469750923
Gauss-Legendre  7-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498197266
Gauss-Legendre  8-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498544945
Gauss-Legendre  9-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498548174
Gauss-Legendre 10-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498548198
Gauss-Legendre 20-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.0357498548198
```



## Phix

```Phix
integer order = 0

sequence legendreRoots = {},
         legendreWeights = {}

function legendre(integer term, atom z)
    if term=0 then
        return 1
    elsif term=1 then
        return z
    else
        return ((2*term-1)*z*legendre(term-1,z)-(term-1)*legendre(term-2,z))/term
    end if
end function

function legendreDerivative(integer term, atom z)
    if term=0
    or term=1 then
        return term
    end if
    return (term*(z*legendre(term,z)-legendre(term-1,z)))/(z*z-1)
end function

procedure getLegendreRoots()
    legendreRoots = {}
    for index=1 to order do
        atom y = cos(PI*(index-0.25)/(order+0.5))
        while 1 do
            atom y1 = y
            y -= legendre(order,y)/legendreDerivative(order,y)
            if abs(y-y1)<2e-16 then exit end if
        end while
        legendreRoots &= y
    end for
end procedure

procedure getLegendreWeights()
    legendreWeights = {}
    for index=1 to order do
        atom lri = legendreRoots[index],
             diff = legendreDerivative(order,lri),
             weight = 2 / ((1-power(lri,2))*power(diff,2))
        legendreWeights &= weight
    end for
end procedure

function gaussLegendreQuadrature(integer f, lowerLimit, upperLimit, n)
    order = n

    getLegendreRoots()
    getLegendreWeights()

    atom c1 = (upperLimit - lowerLimit) / 2
    atom c2 = (upperLimit + lowerLimit) / 2
    atom s = 0

    for i = 1 to order do
        s += legendreWeights[i] * call_func(f,{c1 * legendreRoots[i] + c2})
    end for

    return c1 * s
end function

include pmaths.e    -- exp()
constant r_exp = routine_id("exp")

string fmt = iff(machine_bits()=32?"%.13f":"%.14f")
string res
for i=5 to 11 by 6 do
    res = sprintf(fmt,{gaussLegendreQuadrature(r_exp, -3, 3, i)})
    if i=5 then
        puts(1,"roots:") ?legendreRoots
        puts(1,"weights:") ?legendreWeights
    end if
    printf(1,"Gauss-Legendre %2d-point quadrature for exp over [-3..3] = %s\n",{order,res})
end for
res = sprintf(fmt,{exp(3)-exp(-3)})
printf(1,"                                     compared to actual = %s\n",{res})
```

```txt

roots:{0.9061798459,0.5384693101,0,-0.5384693101,-0.9061798459}
weights:{0.2369268851,0.4786286705,0.5688888889,0.4786286705,0.2369268851}
Gauss-Legendre  5-point quadrature for exp over [-3..3] = 20.0355777183856
Gauss-Legendre 11-point quadrature for exp over [-3..3] = 20.0357498548198
                                     compared to actual = 20.0357498548198

```

Tests showed the result appeared to be accurate to 13 decimal places (15 significant figures) for
order 10 to 30 on 32-bit, and one more for order 11+ on 64-bit.


## PL/I

Translated from Fortran.

```PL/I
(subscriptrange, size, fofl):
Integration_Gauss: procedure options (main);

  declare (n, k) fixed binary;
  declare r(*,*) float (18) controlled;
  declare (z, a, b, exact) float (18);

  do n = 1 to 20;
    a = -3; b = 3;
    if allocation(r) > 0 then free r;
    allocate r(2, n); r = 0;
    call gaussquad(n, r);
    z = (b-a)/2 * sum(r(2,*) * exp((a+b)/2+r(1,*)*(b-a)/2));
    exact = exp(3.0q0)-exp(-3.0q0);
    put skip edit (n, z, z-exact) (f(5), f(25,16), e(15,2));
  end;

gaussquad: procedure(n, r);
/*declare n fixed binary, r(2, n) float (18);*/
  declare n fixed binary, r(2, *) float (18);/* corrected */
  declare pi float (18) value (4*atan(1.0q0));
  declare (x, f, df, dx) float (18);
  declare (i, iter, L) fixed binary;
  declare (p0(*), p1(*), tmp(*), tmp2(*)) float (18) controlled;

  allocate p0(1) initial (1);
  allocate p1(2) initial (1, 0);

  do k = 2 to n;
     allocate tmp(hbound(p1)+1); do L = 1 to hbound(p1); tmp(L) = p1(L); end; tmp(L) = 0;
     allocate tmp2(hbound(p0)+2); tmp2(1), tmp2(2) = 0;
     do L = 1 to hbound(p0); tmp2(L+2) = p0(L); end;
     tmp = ((2*k-1)*tmp - (k-1)*tmp2)/k;
     free p0; allocate p0(hbound(p1)); p0 = p1;
     free p1; allocate p1(hbound(tmp)); p1 = tmp;
     free tmp, tmp2;
  end;
  do i = 1 to n;
    x = cos(pi*(i-0.25q0)/(n+0.5q0));
    do iter = 1 to 10;
      f = p1(1); df = 0;
      do k = 2 to hbound(p1);
        df = f + x*df;
        f  = p1(k) + x * f;
      end;
      dx =  f / df;
      x = x - dx;
      if abs(dx) < 10*epsilon(dx) then leave;
    end;
    r(1,i) = x;
    r(2,i) = 2/((1-x**2)*df**2);
  end;
  end gaussquad;
end Integration_Gauss;

```


```txt

    1       6.0000000000000000    -1.40E+0001
    2      17.4874646410555690    -2.55E+0000
    3      19.8536919968055822    -1.82E-0001
    4      20.0286883952907009    -7.06E-0003
    5      20.0355777183855621    -1.72E-0004
    6      20.0357469750923439    -2.88E-0006
    7      20.0357498197266008    -3.51E-0008
    8      20.0357498544945173    -3.25E-0010
    9      20.0357498548174338    -2.37E-0012
   10      20.0357498548197897    -1.41E-0014
   11      20.0357498548198037    -6.94E-0017
   12      20.0357498548198037    -6.25E-0017
   13      20.0357498548198037    -1.25E-0016
   14      20.0357498548198026    -1.16E-0015
   15      20.0357498548198144     1.06E-0014
   16      20.0357498548198021    -1.74E-0015
   17      20.0357498548198359     3.21E-0014
   18      20.0357498548198473     4.35E-0014
   19      20.0357498548198848     8.10E-0014
   20      20.0357498548200728     2.69E-0013
```


```txt
 program gave me an error message:
D:\ig.pli(19:2) : IBM1937I S Extents for parameters must be asterisks or restricted expressions with computational type.
I tried to correct that. ok?

```



## Python

```Python
from numpy import *

##################################################################
# Recursive generation of the Legendre polynomial of order n
def Legendre(n,x):
	x=array(x)
	if (n==0):
		return x*0+1.0
	elif (n==1):
		return x
	else:
		return ((2.0*n-1.0)*x*Legendre(n-1,x)-(n-1)*Legendre(n-2,x))/n

##################################################################
# Derivative of the Legendre polynomials
def DLegendre(n,x):
	x=array(x)
	if (n==0):
		return x*0
	elif (n==1):
		return x*0+1.0
	else:
		return (n/(x**2-1.0))*(x*Legendre(n,x)-Legendre(n-1,x))
##################################################################
# Roots of the polynomial obtained using Newton-Raphson method
def LegendreRoots(polyorder,tolerance=1e-20):
	if polyorder<2:
		err=1 # bad polyorder no roots can be found
	else:
		roots=[]
		# The polynomials are alternately even and odd functions. So we evaluate only half the number of roots.
		for i in range(1,int(polyorder)/2 +1):
			x=cos(pi*(i-0.25)/(polyorder+0.5))
			error=10*tolerance
		        iters=0
		        while (error>tolerance) and (iters<1000):
		                dx=-Legendre(polyorder,x)/DLegendre(polyorder,x)
		                x=x+dx
		                iters=iters+1
		                error=abs(dx)
			roots.append(x)
		# Use symmetry to get the other roots
		roots=array(roots)
		if polyorder%2==0:
			roots=concatenate( (-1.0*roots, roots[::-1]) )
		else:
			roots=concatenate( (-1.0*roots, [0.0], roots[::-1]) )
		err=0 # successfully determined roots
	return [roots, err]
##################################################################
# Weight coefficients
def GaussLegendreWeights(polyorder):
	W=[]
	[xis,err]=LegendreRoots(polyorder)
	if err==0:
		W=2.0/( (1.0-xis**2)*(DLegendre(polyorder,xis)**2) )
		err=0
	else:
		err=1 # could not determine roots - so no weights
	return [W, xis, err]
##################################################################
# The integral value
# func 		: the integrand
# a, b 		: lower and upper limits of the integral
# polyorder 	: order of the Legendre polynomial to be used
#
def GaussLegendreQuadrature(func, polyorder, a, b):
	[Ws,xs, err]= GaussLegendreWeights(polyorder)
	if err==0:
		ans=(b-a)*0.5*sum( Ws*func( (b-a)*0.5*xs+ (b+a)*0.5 ) )
	else:
		# (in case of error)
		err=1
		ans=None
	return [ans,err]
##################################################################
# The integrand - change as required
def func(x):
	return exp(x)
##################################################################
#

order=5
[Ws,xs,err]=GaussLegendreWeights(order)
if err==0:
	print "Order    : ", order
	print "Roots    : ", xs
	print "Weights  : ", Ws
else:
	print "Roots/Weights evaluation failed"

# Integrating the function
[ans,err]=GaussLegendreQuadrature(func , order, -3,3)
if err==0:
	print "Integral : ", ans
else:
	print "Integral evaluation failed"
```

```txt

Order    :  5
Roots    :  [-0.90617985 -0.53846931  0.          0.53846931  0.90617985]
Weights  :  [ 0.23692689  0.47862867  0.56888889  0.47862867  0.23692689]
Integral :  20.0355777184

```



## Racket


Computation of the Legendre polynomials and derivatives:


```racket

(define (LegendreP n x)
  (let compute ([n n] [Pn-1 x] [Pn-2 1])
    (case n
      [(0) Pn-2]
      [(1) Pn-1]
      [else (compute (- n 1)
                     (/ (- (* (- (* 2 n) 1) x Pn-1)
                           (* (- n 1) Pn-2)) n)
                     Pn-1)])))

(define (LegendreP′ n x)
  (* (/ n (- (* x x) 1))
     (- (* x (LegendreP n x))
        (LegendreP (- n 1) x))))

```


Computation of the Legendre polynomial roots:


```racket

(define (LegendreP-root n i)
  ; newton-raphson step
  (define (newton-step x)
    (- x (/ (LegendreP n x) (LegendreP′ n x))))
  ; initial guess
  (define x0 (cos (* pi (/ (- i 1/4) (+ n 1/2)))))
  ; computation of a root with relative accuracy 1e-15
  (if (< (abs x0) 1e-15)
      0
      (let next ([x′ (newton-step x0)] [x x0])
        (if (< (abs (/ (- x′ x) (+ x′ x))) 1e-15)
            x′
            (next (newton-step x′) x′)))))

```


Computation of Gauss-Legendre nodes and weights


```racket

(define (Gauss-Legendre-quadrature n)
  ;; positive roots
  (define roots
    (for/list ([i (in-range (floor (/ n 2)))])
      (LegendreP-root n (+ i 1))))
  ;; weights for positive roots
  (define weights
    (for/list ([x (in-list roots)])
      (/ 2 (- 1 (sqr x)) (sqr (LegendreP′ n x)))))
  ;; all roots and weights
  (values (append (map - roots)
                  (if (odd? n) (list 0) '())
                  (reverse roots))
          (append weights
                  (if (odd? n) (list (/ 2 (sqr (LegendreP′ n 0)))) '())
                  (reverse weights))))

```


Integration using Gauss-Legendre quadratures:


```racket

(define (integrate f a b #:nodes (n 5))
  (define m (/ (+ a b) 2))
  (define d (/ (- b a) 2))
  (define-values [x w] (Gauss-Legendre-quadrature n))
  (define (g x) (f (+ m (* d x))))
  (* d (+ (apply + (map * w (map g x))))))

```


Usage:


```racket

> (Gauss-Legendre-quadrature 5)
'(-0.906179845938664 -0.5384693101056831 0 0.5384693101056831 0.906179845938664)
'(0.23692688505618875 0.47862867049936625 128/225 0.47862867049936625 0.23692688505618875)

> (integrate exp -3 3)
20.035577718385547

> (- (exp 3) (exp -3)
20.035749854819805

```


Accuracy of the method:


```racket

> (require plot)
> (parameterize ([plot-x-label "Number of Gaussian nodes"]
                 [plot-y-label "Integration error"]
                 [plot-y-transform log-transform]
                 [plot-y-ticks (log-ticks #:base 10)])
    (plot (points (for/list ([n (in-range 2 11)])
                    (list n (abs (- (integrate exp -3 3 #:nodes n)
                                    (- (exp 3) (exp -3)))))))))

```

[[File:gauss.png]]


## REXX


### version 1


```rexx
/*---------------------------------------------------------------------
* 31.10.2013 Walter Pachl  Translation from PL/I
* 01.11.2014 -"- see Version 2 for improvements
*--------------------------------------------------------------------*/
Call time 'R'
prec=60
Numeric Digits prec
epsilon=1/10**prec
pi=3.141592653589793238462643383279502884197169399375105820974944592307
exact = exp(3,prec)-exp(-3,prec)
Do n = 1 To 20
  a = -3; b = 3
  r.=0
  call gaussquad
  sum=0
  Do j=1 To n
    sum=sum + r.2.j * exp((a+b)/2+r.1.j*(b-a)/2,prec)
    End
  z = (b-a)/2 * sum
  Say right(n,2) format(z,2,40) format(z-exact,2,4,,0)
  End
  Say  '  ' exact '(exact)'
  say '... and took' format(time('E'),,2) "seconds"
  Exit

 gaussquad:
   p0.0=1; p0.1=1
   p1.0=2; p1.1=1; p1.2=0
   Do k = 2 To n
     tmp.0=p1.0+1
      Do L = 1 To p1.0
        tmp.l = p1.l
        End
      tmp.l=0
      tmp2.0=p0.0+2
      tmp2.1=0
      tmp2.2=0
      Do L = 1 To p0.0
        l2=l+2
        tmp2.l2=p0.l
        End
      Do j=1 To tmp.0
        tmp.j = ((2*k-1)*tmp.j - (k-1)*tmp2.j)/k
        End
      p0.0=p1.0
      Do j=1 To p0.0
        p0.j = p1.j
        End
      p1.0=tmp.0
      Do j=1 To p1.0
        p1.j=tmp.j
        End
   End
   Do i = 1 To n
     x = cos(pi*(i-0.25)/(n+0.5),prec)
     Do iter = 1 To 10
       f = p1.1; df = 0
       Do k = 2 To p1.0
         df = f + x*df
         f  = p1.k + x * f
         End
       dx =  f / df
       x = x - dx
       If abs(dx) < epsilon then leave
       End
     r.1.i = x
     r.2.i = 2/((1-x**2)*df**2)
     End
   Return

cos: Procedure
/* REXX ****************************************************************
* Return cos(x) -- with specified precision
* cos(x) = 1-(x**2/2!)+(x**4/4!)-(x**6/6!)+-...
* 920903 Walter Pachl
***********************************************************************/
  Parse Arg x,prec
  If prec='' Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz 3
  o=1
  u=1
  r=1
  Do i=1 By 2
    ra=r
    o=-o*x*x
    u=u*i*(i+1)
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits prec
  Return r+0

exp: Procedure
/***********************************************************************
* Return exp(x) -- with reasonable precision
* 920903 Walter Pachl
***********************************************************************/
  Parse Arg x,prec
  If prec<9 Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz   3
  o=1
  u=1
  r=1
  Do i=1 By 1
    ra=r
    o=o*x
    u=u*i
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits (prec)
  Return r+0
```

Output:

```txt
 1  6.0000000000000000000000000000000000000000 -1.4036E+1
 2 17.4874646410555689643606840462449458421154 -2.5483
 3 19.8536919968055821921309108927158495960775 -1.8206E-1
 4 20.0286883952907008527738054439857661647073 -7.0615E-3
 5 20.0355777183855621539285357252750939315016 -1.7214E-4
 6 20.0357469750923438830654575585499253741530 -2.8797E-6
 7 20.0357498197266007755718729372891903369401 -3.5093E-8
 8 20.0357498544945172882260918041683132616237 -3.2529E-10
 9 20.0357498548174338368864419454858704839263 -2.3700E-12
10 20.0357498548197898711175766908543458234008 -1.3927E-14
11 20.0357498548198037305529147159697031241994 -6.7396E-17
12 20.0357498548198037976759531014454017742327 -2.7323E-19
13 20.0357498548198037979482458119092690701863 -9.4143E-22
14 20.0357498548198037979491844483599375945130 -2.7906E-24
15 20.0357498548198037979491872317401917248453 -7.1915E-27
16 20.0357498548198037979491872389153958789316 -1.6260E-29
17 20.0357498548198037979491872389316236038179 -3.2517E-32
18 20.0357498548198037979491872389316560624361 -5.7920E-35
19 20.0357498548198037979491872389316561202637 -9.2480E-38
20 20.0357498548198037979491872389316561203561 -1.3311E-40
   20.0357498548198037979491872389316561203562082463657269288113 (exact)
... and took 4.97 seconds
```



### version 2

This REXX version (an optimized version of version 1)   and uses:
:::*   a faster   '''cos'''   function   (with full precision)
:::*   a faster   '''exp'''   function   (with full precision)
:::*   some simple variables instead of stemmed arrays
:::*   some static variables instead of repeated expressions
:::*   calculations using full (specified) precision (''numeric digits'')
:::*   multiplication using   [<b>···</b> '''*.5''']   instead of division using   [<b>···</b> '''/2''']
:::*   a generic approach for setting the   ''numeric digits''
:::*   a better test for earlier termination (stopping) of calculations
:::*   a more precise value for   '''pi'''
:::*   shows an arrow that points where the GLQ number matches the exact value
:::*   displays the number of decimal digits that match the exact value


[GLQ ≡ Gauss─Legendre quadrature.]


The execution speed of this REXX program is largely dependent on the number of decimal digits in   '''pi'''.

If faster speed is desired,   the number of the decimal digits of   '''pi'''   can be reduced.

Each iteration yields around three more (fractional) decimal digits   (past the decimal point).

The use of "vertical bars" is one of the very few times to use leading comments, as there isn't that many

situations where there exists nested     '''do'''     loops with different (grouped) sizable indentations,   and

where there's practically no space on the right side of the REXX source statements.   It presents a good

visual indication of what's what,   but it's the dickens to pay when updating the source code.

```rexx
/*REXX program does numerical integration using an N-point Gauss─Legendre quadrature rule.   */
pi= pi();     digs= length(pi)-1;       numeric digits digs;                    reps= digs % 2
!.= .;        b= 3;        a= -b;       bma= b - a;          bmaH= bma / 2;     tiny= '1e-'digs
trueV= exp(b)-exp(a);                   bpa= b + a;          bpaH= bpa / 2
hdr= 'iterate value       (with '   digs   " decimal digits being used)"
say ' step '  center(hdr, digs+3)    '  difference'                  /*show hdr*/
sep='──────'  copies("─", digs+3)    '─────────────';      say sep   /*  "  sep*/

  do #=1  until dif>0;   p0z= 1;   p0.1= 1;   p1z= 2;   p1.1= 1;   p1.2= 0;  ##= # + .5;  r.= 0

  /*█*/   do k=2  to #;  km= k - 1;  do y=1  for p1z;   T.y= p1.y;                   end  /*y*/
  /*█*/   T.y= 0;  TT.= 0;           do L=1  for p0z;   _= L + 2;  TT._= p0.L;       end  /*L*/
  /*█*/
  /*█*/   kkm= k + km;       do j=1  for p1z  +1;       T.j= (kkm*T.j -km*TT.j)/k;   end  /*j*/
  /*█*/   p0z= p1z;          do n=1  for p0z;           p0.n= p1.n               ;   end  /*n*/
  /*█*/   p1z= p1z + 1;      do p=1  for p1z;           p1.p= T.p                ;   end  /*p*/
  /*█*/   end   /*k*/

                /*▓*/        do !=1  for #;       x= cos( pi * (! - .25)  /  ## )
                /*▓*/
                /*▓*/                  /*░*/   do reps  until abs(dx) <= tiny
                /*▓*/                  /*░*/   f= p1.1;  df= 0;   do u=2  to p1z; df= f +  x*df
                /*▓*/                  /*░*/                                       f= p1.u +x*f
                /*▓*/                  /*░*/                      end   /*u*/
                /*▓*/                  /*░*/   dx= f / df;   x= x - dx
                /*▓*/                  /*░*/   end             /*reps ···*/
                /*▓*/        r.1.!= x
                /*▓*/        r.2.!= 2 / ( (1 - x**2) * df**2)
                /*▓*/        end   /*!*/
  $= 0
                /*▒*/     do m=1  for #;    $=$ + r.2.m * exp(bpaH + r.1.m*bmaH);    end  /*m*/
  z= bmaH * $                                                    /*calculate target value (Z)*/
  dif= z - trueV;             z= format(z, 3, digs - 2)          /*    "     difference.     */
  Ndif= translate( format(dif, 3, 4, 2, 0),  'e',  "E")
  if #\==1  then  say center(#, 6)      z' '      Ndif           /*no display if not computed*/
  end   /*#*/

say sep;  xdif= compare( strip(z), trueV);                       say right("↑", 6 + 1 + xdif)
say  left('', 6 + 1)      trueV         " {exact value}";        say
say 'Using '      digs      " digit precision, the" ,
    'N-point Gauss─Legendre quadrature (GLQ) had an accuracy of '      xdif-2       " digits."
exit                                                  /*stick a fork in it,  we're all done. */
/*───────────────────────────────────────────────────────────────────────────────────────────*/
e:   return 2.718281828459045235360287471352662497757247093699959574966967627724076630353547595
pi:  return 3.141592653589793238462643383279502884197169399375105820974944592307816406286286209
/*───────────────────────────────────────────────────────────────────────────────────────────*/
cos: procedure  expose !.; parse arg x;      if !.x\==.  then return !.x;  _=1;   z=1;    y=x*x
                do k=2  by 2  until p==z; p=z; _= -_*y/(k*(k-1)); z=z+_; end;  !.x=z;  return z
/*───────────────────────────────────────────────────────────────────────────────────────────*/
exp: procedure; parse arg x;  ix= x % 1;               if abs(x-ix) > .5  then ix= ix + sign(x)
                x= x-ix;  z=1;  _=1;     do j=1  until p==z;   p=z;   _= _*x/j;   z= z+_;   end
                return z * e()**ix
```

```txt

 step                iterate value       (with  82  decimal digits being used)                 difference
────── ───────────────────────────────────────────────────────────────────────────────────── ─────────────
  2     17.48746464105556896436068404624494584211542841793491350914872470595379166623788825   -2.5483
  3     19.85369199680558219213091089271584959607746673197538889290500270758485925164498330   -1.8206e-01
  4     20.02868839529070085277380544398576616470733632504815180772578876685215146483792186   -7.0615e-03
  5     20.03557771838556215392853572527509393150162720744712830816732425295141661302212542   -1.7214e-04
  6     20.03574697509234388306545755854992537415299478921975125717616705900225010375271175   -2.8797e-06
  7     20.03574981972660077557187293728919033694006575323784891307591676343623185267840087   -3.5093e-08
  8     20.03574985449451728822609180416831326162367525799440551006933045513903380452620872   -3.2529e-10
  9     20.03574985481743383688644194548587048392631680869557979312925905853201983429400861   -2.3700e-12
  10    20.03574985481978987111757669085434582340083496254465680809367957309381342059009668   -1.3927e-14
  11    20.03574985481980373055291471596970312419935163064851758082919292076105448665845694   -6.7396e-17
  12    20.03574985481980379767595310144540177423271389844296074380175787717157675883917151   -2.7323e-19
  13    20.03574985481980379794824581190926907018626592287853070355830814733619000088357912   -9.4143e-22
  14    20.03574985481980379794918444835993759451301483567068863329194414460270391327442654   -2.7906e-24
  15    20.03574985481980379794918723174019172484527341186430917498972813563388327387142320   -7.1915e-27
  16    20.03574985481980379794918723891539587893161294648949828480207158337867091213105210   -1.6260e-29
  17    20.03574985481980379794918723893162360381792525574404539062822509053852218733547782   -3.2517e-32
  18    20.03574985481980379794918723893165606243605713014841119742440194777360958854209572   -5.7920e-35
  19    20.03574985481980379794918723893165612026372831720742415561589728335786348943623570   -9.2480e-38
  20    20.03574985481980379794918723893165612035607513408575037519944422231638669124167990   -1.3311e-40
  21    20.03574985481980379794918723893165612035620807276164638611436475769849940475037458   -1.7360e-43
  22    20.03574985481980379794918723893165612035620824615962445370778636022384338924992003   -2.0610e-46
  23    20.03574985481980379794918723893165612035620824636550325344849506916698800464997617   -2.2368e-49
  24    20.03574985481980379794918723893165612035620824636572670605090159763145237587025264   -2.2276e-52
  25    20.03574985481980379794918723893165612035620824636572692860700178828249236875179273   -2.0430e-55
  26    20.03574985481980379794918723893165612035620824636572692881113337954261894220969394   -1.7312e-58
  27    20.03574985481980379794918723893165612035620824636572692881130636614548220525870297   -1.3595e-61
  28    20.03574985481980379794918723893165612035620824636572692881130650199357864896908624   -9.9207e-65
  29    20.03574985481980379794918723893165612035620824636572692881130650209271775421848621   -6.7456e-68
  30    20.03574985481980379794918723893165612035620824636572692881130650209278516823348154   -4.2128e-71
  31    20.03574985481980379794918723893165612035620824636572692881130650209278518859457416   -2.1767e-71
  32    20.03574985481980379794918723893165612035620824636572692881130650209278521040018937    3.8415e-74
────── ───────────────────────────────────────────────────────────────────────────────────── ─────────────
                                                                                   ↑
        20.03574985481980379794918723893165612035620824636572692881130650209278521036177419  {exact value}

Using  82  digit precision,  the N-point Gauss─Legendre quadrature (GLQ) had an accuracy of  74  digits.

```


===version 3, more precision===
This REXX version is almost an exact copy of REXX version 2, but with more decimal digits of   '''pi'''.

It is about twice as slow as version 2,   due to the increased number of decimal digits   (precision).

```rexx
/*REXX program does numerical integration using an N-point Gauss─Legendre quadrature rule.   */
pi= pi();     digs= length(pi)-1;       numeric digits digs;                    reps= digs % 2
!.= .;        b= 3;        a= -b;       bma= b - a;          bmaH= bma / 2;     tiny= '1e-'digs
trueV= exp(b)-exp(a);                   bpa= b + a;          bpaH= bpa / 2
hdr= 'iterate value       (with '   digs   " decimal digits being used)"
say ' step '  center(hdr, digs+3)    '  difference'                  /*show hdr*/
sep='──────'  copies("─", digs+3)    '─────────────';      say sep   /*  "  sep*/

  do #=1  until dif>0;   p0z= 1;   p0.1= 1;   p1z= 2;   p1.1= 1;   p1.2= 0;  ##= # + .5;  r.= 0

  /*█*/   do k=2  to #;  km= k - 1;  do y=1  for p1z;   T.y= p1.y;                   end  /*y*/
  /*█*/   T.y= 0;  TT.= 0;           do L=1  for p0z;   _= L + 2;  TT._= p0.L;       end  /*L*/
  /*█*/
  /*█*/   kkm= k + km;       do j=1  for p1z  +1;       T.j= (kkm*T.j -km*TT.j)/k;   end  /*j*/
  /*█*/   p0z= p1z;          do n=1  for p0z;           p0.n= p1.n               ;   end  /*n*/
  /*█*/   p1z= p1z + 1;      do p=1  for p1z;           p1.p= T.p                ;   end  /*p*/
  /*█*/   end   /*k*/

                /*▓*/        do !=1  for #;       x= cos( pi * (! - .25)  /  ## )
                /*▓*/
                /*▓*/                  /*░*/   do reps  until abs(dx) <= tiny
                /*▓*/                  /*░*/   f= p1.1;  df= 0;   do u=2  to p1z; df= f +  x*df
                /*▓*/                  /*░*/                                       f= p1.u +x*f
                /*▓*/                  /*░*/                      end   /*u*/
                /*▓*/                  /*░*/   dx= f / df;   x= x - dx
                /*▓*/                  /*░*/   end             /*reps ···*/
                /*▓*/        r.1.!= x
                /*▓*/        r.2.!= 2 / ( (1 - x**2) * df**2)
                /*▓*/        end   /*!*/
  $= 0
                /*▒*/     do m=1  for #;    $=$ + r.2.m * exp(bpaH + r.1.m*bmaH);    end  /*m*/
  z= bmaH * $                                                    /*calculate target value (Z)*/
  dif= z - trueV;             z= format(z, 3, digs - 2)          /*    "     difference.     */
  Ndif= translate( format(dif, 3, 4, 3, 0),  'e',  "E")
  if #\==1  then  say center(#, 6)      z' '      Ndif           /*no display if not computed*/
  end   /*#*/

say sep;  xdif= compare( strip(z), trueV);                       say right("↑", 6 + 1 + xdif)
say  left('', 6 + 1)      trueV         " {exact value}";        say
say 'Using '      digs      " digit precision, the" ,
    'N-point Gauss─Legendre quadrature (GLQ) had an accuracy of '      xdif-2       " digits."
exit                                                  /*stick a fork in it,  we're all done. */
/*───────────────────────────────────────────────────────────────────────────────────────────*/
e:   return 2.71828182845904523536028747135266249775724709369995957496696762772407663035354759,
             || 4571382178525166427427466391932003059921817413596629043572900334295260595630738
/*───────────────────────────────────────────────────────────────────────────────────────────*/
pi:  return 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899,
             || 8628034825342117067982148086513282306647093844609550582231725359408128481117450
/*───────────────────────────────────────────────────────────────────────────────────────────*/
cos: procedure  expose !.; parse arg x;      if !.x\==.  then return !.x;  _=1;   z=1;    y=x*x
                do k=2  by 2  until p==z; p=z; _= -_*y/(k*(k-1)); z=z+_; end;  !.x=z;  return z
/*───────────────────────────────────────────────────────────────────────────────────────────*/
exp: procedure; parse arg x;  ix= x % 1;               if abs(x-ix) > .5  then ix= ix + sign(x)
                x= x-ix;  z=1;  _=1;     do j=1  until p==z;   p=z;   _= _*x/j;   z= z+_;   end
                return z * e()**ix
```

(Shown at about two-thirds size.)
<pre style="font-size:67%">
 step                                                      iterate value       (with  159  decimal digits being used)                                                       difference
────── ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── ─────────────
  2     17.4874646410555689643606840462449458421154284179349135091487247059537916662378882444064336021640614626063744948781912964250403870127054497392082425535068464109   -2.5483
  3     19.8536919968055821921309108927158495960774667319753888929050027075848592516449832906645902758379575999249091274157148988582792112906526877518087112700785494497   -1.8206e-001
  4     20.0286883952907008527738054439857661647073363250481518077257887668521514648379218096268747927750038360903142778646220077613647092768733641727539206268833693589   -7.0615e-003
  5     20.0355777183855621539285357252750939315016272074471283081673242529514166130221254213250349496939691709537643294259047823350162410908440808868981982394287542091   -1.7214e-004
  6     20.0357469750923438830654575585499253741529947892197512571761670590022501037527117346339483928363770582109285164930728028479549289382406446621705905363209981933   -2.8797e-006
  7     20.0357498197266007755718729372891903369400657532378489130759167634362318526784010016150667027038415189719144094529764766032097831604495667799067330556673881546   -3.5093e-008
  8     20.0357498544945172882260918041683132616236752579944055100693304551390338045262089091194019302017562870527315644307417688383478919210145963055448428522264642591   -3.2529e-010
  9     20.0357498548174338368864419454858704839263168086955797931292590585320198342940085570553927472311015418220675609961921140415760514983040167737226050690228927443   -2.3700e-012
  10    20.0357498548197898711175766908543458234008349625446568080936795730938134205900980645938318794902592556558231569959762420203929344018773329199723457149763574343   -1.3927e-014
  11    20.0357498548198037305529147159697031241993516306485175808291929207610544866584568009626862857221858328844106864371425322111609007302709732793823163103980149653   -6.7396e-017
  12    20.0357498548198037976759531014454017742327138984429607438017578771715767588391691509175808718708593063121709896967107496243434245185896147055314894150234262075   -2.7323e-019
  13    20.0357498548198037979482458119092690701862659228785307035583081473361900008835808932495328864420024278695427964698380448330606714160259282675390182203803537594   -9.4143e-022
  14    20.0357498548198037979491844483599375945130148356706886332919441446027039132743905494286471338717783707421873433644754993992655580745072286831502363474798175265   -2.7906e-024
  15    20.0357498548198037979491872317401917248452734118643091749897281356338832738714150881537113815780435230011480697467170623887897830301712412973655748924184138648   -7.1915e-027
  16    20.0357498548198037979491872389153958789316129464894982848020715833786709121310547889685984881568546203564135185474792767674806869872650180714616455691318778648   -1.6260e-029
  17    20.0357498548198037979491872389316236038179252557440453906282250905385221873347716826354198555233437240574026019817833907372014036252533047705435353247648510898   -3.2517e-032
  18    20.0357498548198037979491872389316560624360571301484111974244019477736095885421361807599231231543821951618639462965984321643251022835234451110049047608124949533   -5.7920e-035
  19    20.0357498548198037979491872389316561202637283172074241556158972833578634894365092635000776399956033063018069653085902399896542171129596405210008317497301936898   -9.2480e-038
  20    20.0357498548198037979491872389316561203560751340857503751994442223163866912408434007886096643419528065940077022083150476496426837665378721283432879108630468864   -1.3311e-040
  21    20.0357498548198037979491872389316561203562080727616463861143647576984994047530870779393715057751591887673397688454357985082021265151278191050057935329724907161   -1.7360e-043
  22    20.0357498548198037979491872389316561203562082461596244537077863602238433892612703628843743785373313737563806457244053157873973239947461987202443878362979981218   -2.0610e-046
  23    20.0357498548198037979491872389316561203562082463655032534484950691669880046406047078766996078695370527223056578914332723730363863326194707715142045831095820995   -2.2368e-049
  24    20.0357498548198037979491872389316561203562082463657267060509015976314523758814742624773428457390528961843568960502876896215809857825164102337905868347725395891   -2.2276e-052
  25    20.0357498548198037979491872389316561203562082463657269286070017882824923688080311511389836619043005851350331110867389220628954338053656628671036072512306223102   -2.0430e-055
  26    20.0357498548198037979491872389316561203562082463657269288111333795426189423729667519158562143832977811003145168351321839626313132075697513253761673496828204601   -1.7312e-058
  27    20.0357498548198037979491872389316561203562082463657269288113063661454822050198926197665008333893008724687497228278730367375441075263700413282548634210907331356   -1.3595e-061
  28    20.0357498548198037979491872389316561203562082463657269288113065019935786483820352375621786828318969009163053743757325024448325026804644277866300802833611297358   -9.9207e-065
  29    20.0357498548198037979491872389316561203562082463657269288113065020927177593233999249852447888627901300469719564790181325442944469692690797774430312247159512798   -6.7451e-068
  30    20.0357498548198037979491872389316561203562082463657269288113065020927851675301934062025341716601075750412806887227020916063849030412480955063639628314338766826   -4.2832e-071
  31    20.0357498548198037979491872389316561203562082463657269288113065020927852103363148863217394106431702791915956948972366384835732103508918001327415359847661271744   -2.5459e-074
  32    20.0357498548198037979491872389316561203562082463657269288113065020927852103617599854934274435013875248206413049448382025586066461615726348079942124364780837615   -1.4196e-077
  33    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741736109635347323907131494641377410353985987829217992622815976248321170584831199   -7.4395e-081
  34    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810467715704772209566910717933633388969835872983190631663850670877759345234946   -3.6713e-084
  35    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504412069378446854036859408497315019337333762510854198446941961793973563786   -1.7091e-087
  36    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429152646383719980280460795167918691617029439367737607466797014691070736   -7.5175e-091
  37    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160160714391043273984198489693834991216803247954607301723271542659342   -3.1292e-094
  38    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163842341789925349746540298990681930753381942866562579949258588756   -1.2345e-097
  39    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843575809851614709658383098559963930599249691243554488598937473   -4.6221e-101
  40    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576271898405984614568086424291240202255560215705599799392784   -1.6447e-104
  41    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062816325200556739918251890227721352129415324255316078   -5.5685e-108
  42    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062871992591098295378332977741979460337064627095382229   -1.7962e-111
  43    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010547683152388008632372342584043989711962229120   -5.5262e-115
  44    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553207686109280576604524821212627658539678173   -1.6234e-118
  45    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309007883789778553235095713392096309190   -4.5581e-122
  46    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463568475856093621233977451367882545   -1.2245e-125
  47    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690894669420459178304255584843389   -3.1504e-129
  48    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690926165554457658247246849536195   -7.7695e-133
  49    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690926173322092766217806542725001   -1.8383e-136
  50    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690926173323930695861756195366068   -3.9547e-140
  51    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690926173323931067751563248835441   -2.3582e-141
  52    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690926173323931067290550361259682   -2.4043e-141
  53    20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690926173323931250155044682676454    1.5882e-140
────── ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── ─────────────
                                                                                                                                                     ↑
        20.0357498548198037979491872389316561203562082463657269288113065020927852103617741810504429160163843576272062872010553209309463690926173323931091333618506603714  {exact value}

Using  159  digit precision, the N-point Gauss─Legendre quadrature (GLQ) had an accuracy of  141  digits.

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/rrvzhH1/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/yYqRqizfSZip2DhYbdfZ2w Scastie (remote JVM)].

```Scala
import scala.math.{Pi, cos, exp}

object GaussLegendreQuadrature extends App {
  private val N = 5

  private def legeInte(a: Double, b: Double): Double = {
    val (c1, c2) = ((b - a) / 2, (b + a) / 2)
    val tuples: IndexedSeq[(Double, Double)] = {
      val lcoef = {
        val lcoef = Array.ofDim[Double](N + 1, N + 1)

        lcoef(0)(0) = 1
        lcoef(1)(1) = 1
        for (i <- 2 to N) {
          lcoef(i)(0) = -(i - 1) * lcoef(i - 2)(0) / i
          for (j <- 1 to i) lcoef(i)(j) =
            ((2 * i - 1) * lcoef(i - 1)(j - 1) - (i - 1) * lcoef(i - 2)(j)) / i
        }
        lcoef
      }

      def legeEval(n: Int, x: Double): Double =
        lcoef(n).take(n).foldRight(lcoef(n)(n))((o, s) => s * x + o)

      def legeDiff(n: Int, x: Double): Double =
        n * (x * legeEval(n, x) - legeEval(n - 1, x)) / (x * x - 1)

      @scala.annotation.tailrec
      def convergention(x0: Double, x1: Double): Double = {
        if (x0 == x1) x1
        else convergention(x1, x1 - legeEval(N, x1) / legeDiff(N, x1))
      }

      for {i <- 0 until 5
           x = convergention(0.0, cos(Pi * (i + 1 - 0.25) / (N + 0.5)))
           x1 = legeDiff(N, x)
           } yield (x, 2 / ((1 - x * x) * x1 * x1))
    }

    println(s"Roots: ${tuples.map(el => f" ${el._1}%10.6f").mkString}")
    println(s"Weight:${tuples.map(el => f" ${el._2}%10.6f").mkString}")

    c1 * tuples.map { case (lroot, weight) => weight * exp(c1 * lroot + c2) }.sum
  }

  println(f"Integrating exp(x) over [-3, 3]:\n\t${legeInte(-3, 3)}%10.8f,")
  println(f"compared to actual%n\t${exp(3) - exp(-3)}%10.8f")

}
```


## Sidef

```ruby
func legendre_pair((1), x) { (x, 1) }
func legendre_pair( n,  x) {
    var (m1, m2) = legendre_pair(n - 1, x)
    var u = (1 - 1/n)
    ((1 + u)*x*m1 - u*m2, m1)
}

func legendre((0), _) { 1 }
func legendre( n,  x) { [legendre_pair(n, x)][0] }

func legendre_prime({ .is_zero }, _) { 0 }
func legendre_prime({ .is_one  }, _) { 1 }

func legendre_prime(n, x) {
    var (m0, m1) = legendre_pair(n, x)
    (m1 - x*m0) * n / (1 - x**2)
}

func approximate_legendre_root(n, k) {
    # Approximation due to Francesco Tricomi
    var t = ((4*k - 1) / (4*n + 2))
    (1 - ((n - 1)/(8 * n**3))) * cos(Num.pi * t)
}

func newton_raphson(f, f_prime, r, eps = 2e-16) {
    loop {
        var dr = (-f(r) / f_prime(r))
        dr.abs >= eps || break
        r += dr
    }
    return r
}

func legendre_root(n, k) {
    newton_raphson(legendre.method(:call, n), legendre_prime.method(:call, n),
                   approximate_legendre_root(n, k))
}

func weight(n, r) { 2 / ((1 - r**2) * legendre_prime(n, r)**2) }

func nodes(n) {
    gather {
        take(Pair(0, weight(n, 0))) if n.is_odd
        { |i|
            var r = legendre_root(n, i)
            var w = weight(n, r)
            take(Pair(r, w), Pair(-r, w))
        }.each(1 .. (n >> 1))
    }
}

func quadrature(n, f, a, b, nds = nodes(n)) {
    func scale(x) { (x*(b - a) + a + b) / 2 }
    (b - a) / 2 * nds.sum { .second * f(scale(.first)) }
}

[(5..10)..., 20].each { |i|
    printf("Gauss-Legendre %2d-point quadrature ∫₋₃⁺³ exp(x) dx ≈ %.15f\n",
        i, quadrature(i, {.exp}, -3, +3))
}
```

```txt
Gauss-Legendre  5-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.035577718385561
Gauss-Legendre  6-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.035746975092344
Gauss-Legendre  7-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.035749819726600
Gauss-Legendre  8-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.035749854494515
Gauss-Legendre  9-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.035749854817432
Gauss-Legendre 10-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.035749854819791
Gauss-Legendre 20-point quadrature ∫₋₃⁺³ exp(x) dx ≈ 20.035749854819805
```



## Tcl

```tcl
package require Tcl 8.5
package require math::special
package require math::polynomials
package require math::constants
math::constants::constants pi

# Computes the initial guess for the root i of a n-order Legendre polynomial
proc guess {n i} {
    global pi
    expr { cos($pi * ($i - 0.25) / ($n + 0.5)) }
}

# Computes and evaluates the n-order Legendre polynomial at the point x
proc legpoly {n x} {
    math::polynomials::evalPolyn [math::special::legendre $n] $x
}

# Computes and evaluates the derivative of an n-order Legendre polynomial at point x
proc legdiff {n x} {
    expr {$n / ($x**2 - 1) * ($x * [legpoly $n $x] - [legpoly [incr n -1] $x])}
}

# Computes the n nodes for an n-point quadrature rule. (i.e. n roots of a n-order polynomial)
proc nodes n {
    set x [lrepeat $n 0.0]
    for {set i 0} {$i < $n} {incr i} {
	set val [guess $n [expr {$i + 1}]]
	foreach . {1 2 3 4 5} {
	    set val [expr {$val - [legpoly $n $val] / [legdiff $n $val]}]
	}
	lset x $i $val
    }
    return $x
}

# Computes the weight for an n-order polynomial at the point (node) x
proc legwts {n x} {
    expr {2.0 / (1 - $x**2) / [legdiff $n $x]**2}
}

# Takes a array of nodes x and computes an array of corresponding weights w
proc weights x {
    set n [llength $x]
    set w {}
    foreach xi $x {
	lappend w [legwts $n $xi]
    }
    return $w
}

# Integrates a lambda term f with a n-point Gauss-Legendre quadrature rule over the interval [a,b]
proc gausslegendreintegrate {f n a b} {
    set x [nodes $n]
    set w [weights $x]
    set rangesize2 [expr {($b - $a)/2}]
    set rangesum2 [expr {($a + $b)/2}]
    set sum 0.0
    foreach xi $x wi $w {
	set y [expr {$rangesize2*$xi + $rangesum2}]
	set sum [expr {$sum + $wi*[apply $f $y]}]
    }
    expr {$sum * $rangesize2}
}
```

Demonstrating:

```tcl
puts "nodes(5) = [nodes 5]"
puts "weights(5) = [weights [nodes 5]]"
set exp {x {expr {exp($x)}}}
puts "int(exp,-3,3) = [gausslegendreintegrate $exp 5 -3 3]"
```

```txt

nodes(5) = 0.906179845938664 0.5384693101056831 -1.198509146801203e-94 -0.5384693101056831 -0.906179845938664
weights(5) = 0.2369268850561896 0.4786286704993664 0.5688888888888889 0.4786286704993664 0.2369268850561896
int(exp,-3,3) = 20.03557771838559

```



## Ursala

using arbitrary precision arithmetic

```Ursala
#import std
#import nat

legendre = # takes n to the pair of functions (P_n,P'_n), where P_n is the Legendre polynomial of order n

~&?\(1E0!,0E0!)! -+
   ^|/~& //mp..vid^ mp..sub\1E0+ mp..sqr,
   ~~ "c". ~&\1E0; ~&\"c"; ~&ar^?\0E0! mp..add^/mp..mul@alrPrhPX ^|R/~& ^|\~&t ^/~&l mp..mul,
   @iiXNX ~&rZ->r @l ^/^|(~&tt+ sum@NNiCCiX+ successor,~&) both~&g&&~&+ -+
      ~* mp..zero_p?/~& (&&~&r ~&EZ+ ~~ mp..prec)^/~& ^(~&,..shr\8); mp..equ^|(~&,..gro\8)->l @r ^/~& ..shr\8,
      ^(~&rl,mp..mul*lrrPD)^/..nat2mp@r -+
         ^(~&l,mp..sub*+ zipp0E0^|\~& :/0E0)+ ~&rrt->lhthPX ^(
            ^lrNCC\~&lh mp..vid^*D/..nat2mp@rl -+
               mp..sub*+ zipp0E0^|\~& :/0E0,
               mp..mul~*brlD^|bbI/~&hthPX @l ..nat2mp~~+ predecessor~~NiCiX+-,
            @r ^|/successor predecessor),
         ^|(mp..grow/1E0; @iNC ^lrNCC\~& :/0E0,~&/2)+-+-+-

nodes = # takes precision and order (p,n) to a list of nodes and weights <(x_1,w_1)..(x_n,w_n)>

-+
   ^H(
      @lrr *+ ^/~&+ mp..div/( ..nat2mp 2)++ mp..mul^/(mp..sqr; //mp..sub ..nat2mp 1)+ mp..sqr+,
      mp..shr^*DrlXS/~&ll ^|H\~& *+ @NiX+ ->l^|(~&lZ!|+ not+ //mp..eq,@r+ ^/~&+ mp..sub^/~&+ mp..div^)),
   ^/^|(~&,legendre) mp..cos*+ mp..mul^*D(
      mp..div^|/mp..pi@NiC mp..add/5E-1+ ..nat2mp,
      @r mp..bus/*2.5E-1+ ..nat2mp*+ nrange/1)+-

integral = # takes precision and order (p,n) to a function taking a function and interval (f,(a,b))

("p","n"). -+
   mp..shrink^/~& difference\"p"+ mp..prec,
   mp..mul^|/~& mp..add:-0E0+ * mp..mul^/~&rr ^H/~&ll mp..add^\~&lrr mp..mul@lrPrXl,
   ^(~&rl,-*nodes("p","n"))^|/~& mp..vid~~G/2E0+ ^/mp..bus mp..add+-
```

demonstration program:
```Ursala
#show+

demo =

~&lNrCT (
   ^|lNrCT(:/'nodes:',:/'weights:')@lSrSX ..mp2str~~* nodes/160 5,
   :/'integral:' ~&iNC ..mp2str integral(160,5) (mp..exp,-3E0,3E0))
```

```txt

nodes:
9.0617984593866399279762687829939296512565191076233E-01
5.3846931010568309103631442070020880496728660690555E-01
0.0000000000000000000000000000000000000000000000000E+00
-5.3846931010568309103631442070020880496728660690555E-01
-9.0617984593866399279762687829939296512565191076233E-01

weights:
2.3692688505618908751426404071991736264326000220463E-01
4.7862867049936646804129151483563819291229555334456E-01
5.6888888888888888888888888888888888888888888888896E-01
4.7862867049936646804129151483563819291229555334456E-01
2.3692688505618908751426404071991736264326000220463E-01

integral:
2.0035577718385562153928535725275093931501627207110E+01
```



## zkl

```zkl
fcn legendrePair(n,x){ //-->(float,float)
   if(n==1) return(x,1.0);
   m1,m2:=legendrePair(n-1,x);
   u:=1.0 - 1.0/n;
   return( (u + 1)*x*m1 - u*m2, m1);
}
fcn legendre(n,x){ //-->float
   if(n==0) return(0.0);
   legendrePair(n,x)[0]
}
fcn legendrePrime(n,x){ //-->float
   if(n==0) return(0.0);
   if(n==1) return(1.0);
   m0,m1:=legendrePair(n,x);
   (m1 - m0*x)*n/(1.0 - x*x);
}
fcn approximateLegendreRoot(n,k){ # Approximation due to Francesco Tricomi
   t:=(4.0*k - 1)/(4.0*n + 2);
   (1.0 - (n - 1)/(8*n*n*n))*((0.0).pi*t).cos();
}
fcn newtonRaphson(f,fPrime,r,eps=2.0e-16){
   while(not (dr:=-f(r)/fPrime(r)).closeTo(0.0,eps)){ r+=dr }
   r;
}
fcn legendreRoot(n,k){
   newtonRaphson(legendre.fp(n),legendrePrime.fp(n),
                 approximateLegendreRoot(n,k));
}
fcn weight(n,r){
   lp:=legendrePrime(n,r);
   2.0/((1.0 - r*r)*lp*lp)
}
fcn nodes(n){ //-->( (r,weight), (r,w), ...) length n
   sink:=n.isOdd and L(T(0.0,weight(n,0))) or List;
   (1).pump(n/2,sink,'wrap(m){
      r:=legendreRoot(n,m);
      w:=weight(n,r);
      return( Void.Write,T(r,w),T(-r,w) )
   })
}
fcn quadrature(n,f,a,b,nds=Void){
    if(not nds) nds=nodes(n);
    scale:='wrap(x){ (x*(b - a) + a + b) / 2 };
    nds.reduce('wrap(p,[(r,w)]){ p + w*f(scale(r)) },0.0) * (b - a)/2
}
```


```zkl
[5..10].walk().append(20).pump(Console.println,fcn(n){
   ("Gauss-Legendre %2d-point quadrature "
   "\U222B;\U208B;\U2083;\U207A;\UB3; exp(x) dx = %.13f")
   .fmt(n,quadrature(n, fcn(x){ x.exp() }, -3, 3))
})
```

```txt

Gauss-Legendre  5-point quadrature ∫₋₃⁺³ exp(x) dx = 20.0355777183856
Gauss-Legendre  6-point quadrature ∫₋₃⁺³ exp(x) dx = 20.0357469750924
Gauss-Legendre  7-point quadrature ∫₋₃⁺³ exp(x) dx = 20.0357498197266
Gauss-Legendre  8-point quadrature ∫₋₃⁺³ exp(x) dx = 20.0357498544945
Gauss-Legendre  9-point quadrature ∫₋₃⁺³ exp(x) dx = 20.0357498548174
Gauss-Legendre 10-point quadrature ∫₋₃⁺³ exp(x) dx = 20.0357498548198
Gauss-Legendre 20-point quadrature ∫₋₃⁺³ exp(x) dx = 20.0357498548198

```




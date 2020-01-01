+++
title = "Polynomial long division"
description = ""
date = 2019-04-05T19:27:26Z
aliases = []
[extra]
id = 4397
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}{{Wikipedia}}
:<cite>In algebra, [[wp:Polynomial long division|polynomial long division]] is an algorithm for dividing a polynomial by another polynomial of the same or lower degree.</cite>

Let us suppose a polynomial is represented by a vector, <math>x</math> (i.e., an ordered collection of [[wp:Coefficient|coefficients]]) so that the <math>i</math><sup>th</sup> element keeps the coefficient of <math>x^i</math>, and the multiplication by a monomial is a ''shift''  of the vector's elements "towards right" (injecting ones from left) followed by a multiplication of each element by the coefficient of the monomial.

Then a pseudocode for the polynomial long division using the conventions described above could be:

 degree('''P'''):
   '''return''' the index of the last non-zero element of '''P''';
          if all elements are 0, return -∞

 polynomial_long_division('''N''', '''D''') ''returns'' ('''q''', '''r'''):
   <span class="co1">// '''N''', '''D''', '''q''', '''r''' are vectors</span>
   '''if''' degree('''D''') < 0 '''then''' ''error''
   '''q''' ← '''0'''
   '''while''' degree('''N''') ≥ degree('''D''')
     '''d''' ← '''D''' ''shifted right'' ''by'' (degree('''N''') - degree('''D'''))
     '''q'''(degree('''N''') - degree('''D''')) ← '''N'''(degree('''N''')) / '''d'''(degree('''d'''))
     <span class="co1">// by construction, degree('''d''') = degree('''N''') of course</span>
     '''d''' ← '''d''' * '''q'''(degree('''N''') - degree('''D'''))
     '''N''' ← '''N''' - '''d'''
   '''endwhile'''
   '''r''' ← '''N'''
   '''return''' ('''q''', '''r''')

'''Note''': <code>vector * scalar</code> multiplies each element of the vector by the scalar; <code>vectorA - vectorB</code> subtracts each element of the vectorB from the element of the vectorA with "the same index". The vectors in the pseudocode are zero-based.

* Error handling (for allocations or for wrong inputs) is not mandatory.
* Conventions can be different; in particular, note that if the first coefficient in the vector is the highest power of x for the polynomial represented by the vector, then the algorithm becomes simpler.

'''Example for clarification'''


This example is from Wikipedia, but changed to show how the given pseudocode works.

       0    1    2    3
    ----------------------
 N:  -42    0  -12    1        degree = 3
 D:   -3    1    0    0        degree = 1

    <span class="co1">d(N) - d(D) = 2, so let's shift D towards right by 2:</span>

 N:  -42    0  -12    1
 d:    0    0   -3    1

    <span class="co1">N(3)/d(3) = 1, so d is unchanged. Now remember that "shifting by 2"
    is like multiplying by x<sup>2</sup>, and the final multiplication
    (here by 1) is the coefficient of this monomial. Let's store this
    into q:</span>
                                0     1     2
                               ---------------
                           q:   0     0     1

    <span class="co1">now compute N - d, and let it be the "new" N, and let's loop</span>

 N:  -42    0   -9    0        degree = 2
 D:   -3    1    0    0        degree = 1

    <span class="co1">d(N) - d(D) = 1, right shift D by 1 and let it be d</span>

 N:  -42    0   -9    0
 d:    0   -3    1    0        * -9/1 = -9

                           q:   0    -9     1

 d:    0   27   -9    0

    N ← N - d

 N:  -42  -27    0    0        degree = 1
 D:   -3    1    0    0        degree = 1

    <span class="co1">looping again... d(N)-d(D)=0, so no shift is needed; we
    multiply D by -27 (= -27/1) storing the result in d, then</span>

                           q:  -27   -9     1

    <span class="co1">and</span>

 N:  -42  -27    0    0        -
 d:   81  -27    0    0        =
 N: -123    0    0    0        (last N)

     <span class="co1">d(N) &lt; d(D), so now r ← N, and the result is:</span>

        0   1  2
    -------------
 q:   -27  -9  1   →  x<sup>2</sup> - 9x - 27
 r:  -123   0  0   →          -123


## Ada

long_division.adb:

```Ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Long_Division is
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   use Int_IO;

   type Degrees is range -1 .. Integer'Last;
   subtype Valid_Degrees is Degrees range 0 .. Degrees'Last;
   type Polynom is array (Valid_Degrees range <>) of Integer;

   function Degree (P : Polynom) return Degrees is
   begin
      for I in reverse P'Range loop
         if P (I) /= 0 then
            return I;
         end if;
      end loop;
      return -1;
   end Degree;

   function Shift_Right (P : Polynom; D : Valid_Degrees) return Polynom is
      Result : Polynom (0 .. P'Last + D) := (others => 0);
   begin
      Result (Result'Last - P'Length + 1 .. Result'Last) := P;
      return Result;
   end Shift_Right;

   function "*" (Left : Polynom; Right : Integer) return Polynom is
      Result : Polynom (Left'Range);
   begin
      for I in Result'Range loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   function "-" (Left, Right : Polynom) return Polynom is
      Result : Polynom (Left'Range);
   begin
      for I in Result'Range loop
         if I in Right'Range then
            Result (I) := Left (I) - Right (I);
         else
            Result (I) := Left (I);
         end if;
      end loop;
      return Result;
   end "-";

   procedure Poly_Long_Division (Num, Denom : Polynom; Q, R : out Polynom) is
      N : Polynom := Num;
      D : Polynom := Denom;
   begin
      if Degree (D) < 0 then
         raise Constraint_Error;
      end if;
      Q := (others => 0);
      while Degree (N) >= Degree (D) loop
         declare
            T : Polynom := Shift_Right (D, Degree (N) - Degree (D));
         begin
            Q (Degree (N) - Degree (D)) := N (Degree (N)) / T (Degree (T));
            T := T * Q (Degree (N) - Degree (D));
            N := N - T;
         end;
      end loop;
      R := N;
   end Poly_Long_Division;

   procedure Output (P : Polynom) is
      First : Boolean := True;
   begin
      for I in reverse P'Range loop
         if P (I) /= 0 then
            if First then
               First := False;
            else
               Put (" + ");
            end if;
            if I > 0 then
               if P (I) /= 1 then
                  Put (P (I), 0);
                  Put ("*");
               end if;
               Put ("x");
               if I > 1 then
                  Put ("^");
                  Put (Integer (I), 0);
               end if;
            elsif P (I) /= 0 then
               Put (P (I), 0);
            end if;
         end if;
      end loop;
      New_Line;
   end Output;

   Test_N : constant Polynom := (0 => -42, 1 => 0, 2 => -12, 3 => 1);
   Test_D : constant Polynom := (0 => -3, 1 => 1);
   Test_Q : Polynom (Test_N'Range);
   Test_R : Polynom (Test_N'Range);
begin
   Poly_Long_Division (Test_N, Test_D, Test_Q, Test_R);
   Put_Line ("Dividing Polynoms:");
   Put ("N: "); Output (Test_N);
   Put ("D: "); Output (Test_D);
   Put_Line ("-------------------------");
   Put ("Q: "); Output (Test_Q);
   Put ("R: "); Output (Test_R);
end Long_Division;
```


output:

```txt
Dividing Polynoms:
N: x^3 + -12*x^2 + -42
D: x + -3
-------------------------
Q: x^2 + -9*x + -27
R: -123
```


## APL


```APL
div←{
    {
        q r d←⍵
        (≢d) > n←≢r : q r
        c ← (⊃⌽r) ÷ ⊃⌽d
        ∇ (c,q) ((¯1↓r) - c × ¯1↓(-n)↑d) d
    } ⍬ ⍺ ⍵
}

```

{{out}}

```txt
      N←¯42 0 ¯12 1
      D←¯3 1
      ⍪N div D
 ¯27 ¯9 1
 ¯123

```



## BBC BASIC


```bbcbasic
      DIM N%(3) : N%() = -42, 0, -12, 1
      DIM D%(3) : D%() =  -3, 1,   0, 0
      DIM q%(3), r%(3)
      PROC_poly_long_div(N%(), D%(), q%(), r%())
      PRINT "Quotient = "; FNcoeff(q%(2)) "x^2" FNcoeff(q%(1)) "x" FNcoeff(q%(0))
      PRINT "Remainder = " ; r%(0)
      END

      DEF PROC_poly_long_div(N%(), D%(), q%(), r%())
      LOCAL d%(), i%, s%
      DIM d%(DIM(N%(),1))
      s% = FNdegree(N%()) - FNdegree(D%())
      IF s% >= 0 THEN
        q%() = 0
        WHILE s% >= 0
          FOR i% = 0 TO DIM(d%(),1) - s%
            d%(i%+s%) = D%(i%)
          NEXT
          q%(s%) = N%(FNdegree(N%())) DIV d%(FNdegree(d%()))
          d%() = d%() * q%(s%)
          N%() -= d%()
          s% = FNdegree(N%()) - FNdegree(D%())
        ENDWHILE
        r%() = N%()
      ELSE
        q%() = 0
        r%() = N%()
      ENDIF
      ENDPROC

      DEF FNdegree(a%())
      LOCAL i%
      i% = DIM(a%(),1)
      WHILE a%(i%)=0
        i% -= 1
        IF i%<0 EXIT WHILE
      ENDWHILE
      = i%

      DEF FNcoeff(n%)
      IF n%=0 THEN = ""
      IF n%<0 THEN = " - " + STR$(-n%)
      IF n%=1 THEN = " + "
      = " + " + STR$(n%)
```

'''Output:'''

```txt

Quotient =  + x^2 - 9x - 27
Remainder = -123

```



## C

{{trans|Fortran}}

{{libheader|GNU Scientific Library}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <gsl/gsl_vector.h>

#define MAX(A,B) (((A)>(B))?(A):(B))

void reoshift(gsl_vector *v, int h)
{
  if ( h > 0 ) {
    gsl_vector *temp = gsl_vector_alloc(v->size);
    gsl_vector_view p = gsl_vector_subvector(v, 0, v->size - h);
    gsl_vector_view p1 = gsl_vector_subvector(temp, h, v->size - h);
    gsl_vector_memcpy(&p1.vector, &p.vector);
    p = gsl_vector_subvector(temp, 0, h);
    gsl_vector_set_zero(&p.vector);
    gsl_vector_memcpy(v, temp);
    gsl_vector_free(temp);
  }
}

gsl_vector *poly_long_div(gsl_vector *n, gsl_vector *d, gsl_vector **r)
{
  gsl_vector *nt = NULL, *dt = NULL, *rt = NULL, *d2 = NULL, *q = NULL;
  int gn, gt, gd;

  if ( (n->size >= d->size) && (d->size > 0) && (n->size > 0) ) {
    nt = gsl_vector_alloc(n->size); assert(nt != NULL);
    dt = gsl_vector_alloc(n->size); assert(dt != NULL);
    rt = gsl_vector_alloc(n->size); assert(rt != NULL);
    d2 = gsl_vector_alloc(n->size); assert(d2 != NULL);
    gsl_vector_memcpy(nt, n);
    gsl_vector_set_zero(dt); gsl_vector_set_zero(rt);
    gsl_vector_view p = gsl_vector_subvector(dt, 0, d->size);
    gsl_vector_memcpy(&p.vector, d);
    gsl_vector_memcpy(d2, dt);
    gn = n->size - 1;
    gd = d->size - 1;
    gt = 0;

    while( gsl_vector_get(d, gd) == 0 ) gd--;

    while ( gn >= gd ) {
      reoshift(dt, gn-gd);
      double v = gsl_vector_get(nt, gn)/gsl_vector_get(dt, gn);
      gsl_vector_set(rt, gn-gd, v);
      gsl_vector_scale(dt, v);
      gsl_vector_sub(nt, dt);
      gt = MAX(gt, gn-gd);
      while( (gn>=0) && (gsl_vector_get(nt, gn) == 0.0) ) gn--;
      gsl_vector_memcpy(dt, d2);
    }

    q = gsl_vector_alloc(gt+1); assert(q != NULL);
    p = gsl_vector_subvector(rt, 0, gt+1);
    gsl_vector_memcpy(q, &p.vector);
    if ( r != NULL ) {
      if ( (gn+1) > 0 ) {
	*r = gsl_vector_alloc(gn+1); assert( *r != NULL );
	p = gsl_vector_subvector(nt, 0, gn+1);
	gsl_vector_memcpy(*r, &p.vector);
      } else {
	*r = gsl_vector_alloc(1); assert( *r != NULL );
	gsl_vector_set_zero(*r);
      }
    }
    gsl_vector_free(nt); gsl_vector_free(dt);
    gsl_vector_free(rt); gsl_vector_free(d2);
    return q;
  } else {
    q = gsl_vector_alloc(1); assert( q != NULL );
    gsl_vector_set_zero(q);
    if ( r != NULL ) {
      *r = gsl_vector_alloc(n->size); assert( *r != NULL );
      gsl_vector_memcpy(*r, n);
    }
    return q;
  }
}

void poly_print(gsl_vector *p)
{
  int i;
  for(i=p->size-1; i >= 0; i--) {
    if ( i > 0 )
      printf("%lfx^%d + ",
	     gsl_vector_get(p, i), i);
    else
      printf("%lf\n", gsl_vector_get(p, i));
  }
}

gsl_vector *create_poly(int d, ...)
{
  va_list al;
  int i;
  gsl_vector *r = NULL;

  va_start(al, d);
  r = gsl_vector_alloc(d); assert( r != NULL );

  for(i=0; i < d; i++)
    gsl_vector_set(r, i, va_arg(al, double));

  return r;
}
```



```c
int main()
{
  int i;
  gsl_vector *q, *r;
  gsl_vector *nv, *dv;

  //nv = create_poly(4,  -42., 0., -12., 1.);
  //dv = create_poly(2,  -3., 1.);
  //nv = create_poly(3,  2., 3., 1.);
  //dv = create_poly(2,  1., 1.);
  nv = create_poly(4, -42., 0., -12., 1.);
  dv = create_poly(3, -3., 1., 1.);

  q = poly_long_div(nv, dv, &r);

  poly_print(q);
  poly_print(r);

  gsl_vector_free(q);
  gsl_vector_free(r);

  return 0;
}
```



### Another version

Without outside libs, for clarity. Note that polys are stored and show with zero-degree term first:
```c
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

typedef struct {
        int power;
        double * coef;
} poly_t, *poly;

#define E(x, i) (x)->coef[i]

/* passing in negative power to have a zeroed poly */
poly p_new(int power, ...)
{
        int i, zeroed = 0;
        va_list ap;

        if (power < 0) {
                power = -power;
                zeroed = 1;
        }

        poly p = malloc(sizeof(poly_t));
        p->power = power;
        p->coef = malloc(sizeof(double) * ++power);

        if (zeroed)
                for (i = 0; i < power; i++) p->coef[i] = 0;
        else {
                va_start(ap, power);
                for (i = 0; i < power; i++)
                        E(p, i) = va_arg(ap, double);
                va_end(ap);
        }

        return p;
}

void p_del(poly p)
{
        free(p->coef);
        free(p);
}

void p_print(poly p)
{
        int i;
        for (i = 0; i <= p->power; i++)
                printf("%g ", E(p, i));
        printf("\n");
}

poly p_copy(poly p)
{
        poly q = p_new(-p->power);
        memcpy(q->coef, p->coef, sizeof(double) * (1 + p->power));
        return q;
}

/* p: poly;  d: divisor;  r: remainder; returns quotient */
poly p_div(poly p, poly d, poly* r)
{
        poly q;
        int i, j;
        int power = p->power - d->power;
        double ratio;

        if (power < 0) return 0;

        q = p_new(-power);
        *r= p_copy(p);

        for (i = p->power; i >= d->power; i--) {
                E(q, i - d->power) = ratio = E(*r, i) / E(d, d->power);
                E(*r ,i) = 0;

                for (j = 0; j < d->power; j++)
                        E(*r, i - d->power + j) -= E(d, j) * ratio;
        }
        while (! E(*r, --(*r)->power));

        return q;
}

int main()
{
        poly p = p_new(3, 1., 2., 3., 4.);
        poly d = p_new(2, 1., 2., 1.);
        poly r;
        poly q = p_div(p, d, &r);

        printf("poly: "); p_print(p);
        printf("div:  "); p_print(d);
        printf("quot: "); p_print(q);
        printf("rem:  "); p_print(r);

        p_del(p);
        p_del(q);
        p_del(r);
        p_del(d);

        return 0;
}
```




## C++


```cpp

#include <iostream>
#include <iterator>
#include <vector>

using namespace std;
typedef vector<double> Poly;

// does:  prints all members of vector
// input: c - ASCII char with the name of the vector
//        A - reference to polynomial (vector)
void Print(char name, const Poly &A) {
	cout << name << "(" << A.size()-1 << ") = [ ";
	copy(A.begin(), A.end(), ostream_iterator<decltype(A[0])>(cout, " "));
	cout << "]\n";
}

int main() {
	Poly N, D, d, q, r;        // vectors - N / D == q && N % D == r
	size_t dN, dD, dd, dq, dr; // degrees of vectors
	size_t i;                  // loop counter

	// setting the degrees of vectors
	cout << "Enter the degree of N: ";
	cin >> dN;
	cout << "Enter the degree of D: ";
	cin >> dD;
	dq = dN-dD;
	dr = dN-dD;

	if( dD < 1 || dN < 1 ) {
		cerr << "Error: degree of D and N must be positive.\n";
		return 1;
	}

	// allocation and initialization of vectors
	N.resize(dN+1);
	cout << "Enter the coefficients of N:"<<endl;
	for ( i = 0; i <= dN; i++ ) {
		cout << "N[" << i << "]= ";
		cin >> N[i];
	}

	D.resize(dN+1);
	cout << "Enter the coefficients of D:"<<endl;
	for ( i = 0; i <= dD; i++ ) {
		cout << "D[" << i << "]= ";
		cin >> D[i];
	}

	d.resize(dN+1);
	q.resize(dq+1);
	r.resize(dr+1);

	cout << "-- Procedure --" << endl << endl;
	if( dN >= dD ) {
		while(dN >= dD) {
			// d equals D shifted right
			d.assign(d.size(), 0);

			for( i = 0 ; i <= dD ; i++ )
				d[i+dN-dD] = D[i];
			dd = dN;

			Print( 'd', d );

			// calculating one element of q
			q[dN-dD] = N[dN]/d[dd];

			Print( 'q', q );

			// d equals d * q[dN-dD]
			for( i = 0 ; i < dq + 1 ; i++ )
				d[i] = d[i] * q[dN-dD];

			Print( 'd', d );

			// N equals N - d
			for( i = 0 ; i < dN + 1 ; i++ )
				N[i] = N[i] - d[i];
			dN--;

			Print( 'N', N );
			cout << "-----------------------" << endl << endl;

		}
	}

	// r equals N
	for( i = 0 ; i <= dN ; i++ )
		r[i] = N[i];

	cout << "
### ===================
" << endl << endl;
	cout << "-- Result --" << endl << endl;

	Print( 'q', q );
	Print( 'r', r );
}


```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;

namespace PolynomialLongDivision {
    class Solution {
        public Solution(double[] q, double[] r) {
            Quotient = q;
            Remainder = r;
        }

        public double[] Quotient { get; }
        public double[] Remainder { get; }
    }

    class Program {
        static int PolyDegree(double[] p) {
            for (int i = p.Length - 1; i >= 0; --i) {
                if (p[i] != 0.0) return i;
            }
            return int.MinValue;
        }

        static double[] PolyShiftRight(double[] p, int places) {
            if (places <= 0) return p;
            int pd = PolyDegree(p);
            if (pd + places >= p.Length) {
                throw new ArgumentOutOfRangeException("The number of places to be shifted is too large");
            }
            double[] d = new double[p.Length];
            p.CopyTo(d, 0);
            for (int i = pd; i >= 0; --i) {
                d[i + places] = d[i];
                d[i] = 0.0;
            }
            return d;
        }

        static void PolyMultiply(double[] p, double m) {
            for (int i = 0; i < p.Length; ++i) {
                p[i] *= m;
            }
        }

        static void PolySubtract(double[] p, double[] s) {
            for (int i = 0; i < p.Length; ++i) {
                p[i] -= s[i];
            }
        }

        static Solution PolyLongDiv(double[] n, double[] d) {
            if (n.Length != d.Length) {
                throw new ArgumentException("Numerator and denominator vectors must have the same size");
            }
            int nd = PolyDegree(n);
            int dd = PolyDegree(d);
            if (dd < 0) {
                throw new ArgumentException("Divisor must have at least one one-zero coefficient");
            }
            if (nd < dd) {
                throw new ArgumentException("The degree of the divisor cannot exceed that of the numerator");
            }
            double[] n2 = new double[n.Length];
            n.CopyTo(n2, 0);
            double[] q = new double[n.Length];
            while (nd >= dd) {
                double[] d2 = PolyShiftRight(d, nd - dd);
                q[nd - dd] = n2[nd] / d2[nd];
                PolyMultiply(d2, q[nd - dd]);
                PolySubtract(n2, d2);
                nd = PolyDegree(n2);
            }
            return new Solution(q, n2);
        }

        static void PolyShow(double[] p) {
            int pd = PolyDegree(p);
            for (int i = pd; i >= 0; --i) {
                double coeff = p[i];
                if (coeff == 0.0) continue;
                if (coeff == 1.0) {
                    if (i < pd) {
                        Console.Write(" + ");
                    }
                } else if (coeff == -1.0) {
                    if (i < pd) {
                        Console.Write(" - ");
                    } else {
                        Console.Write("-");
                    }
                } else if (coeff < 0.0) {
                    if (i < pd) {
                        Console.Write(" - {0:F1}", -coeff);
                    } else {
                        Console.Write("{0:F1}", coeff);
                    }
                } else {
                    if (i < pd) {
                        Console.Write(" + {0:F1}", coeff);
                    } else {
                        Console.Write("{0:F1}", coeff);
                    }
                }
                if (i > 1) Console.Write("x^{0}", i);
                else if (i == 1) Console.Write("x");
            }
            Console.WriteLine();
        }

        static void Main(string[] args) {
            double[] n = { -42.0, 0.0, -12.0, 1.0 };
            double[] d = { -3.0, 1.0, 0.0, 0.0 };
            Console.Write("Numerator   : ");
            PolyShow(n);
            Console.Write("Denominator : ");
            PolyShow(d);
            Console.WriteLine("-------------------------------------");
            Solution sol = PolyLongDiv(n, d);
            Console.Write("Quotient    : ");
            PolyShow(sol.Quotient);
            Console.Write("Remainder   : ");
            PolyShow(sol.Remainder);
        }
    }
}
```

{{out}}

```txt
Numerator   : x^3 - 12.0x^2 - 42.0
Denominator : x - 3.0
-------------------------------------
Quotient    : x^2 - 9.0x - 27.0
Remainder   : -123.0
```



## Clojure


This example performs ''multivariate'' polynomial division using [https://en.wikipedia.org/wiki/Buchberger%27s_algorithm Buchberger's algorithm] to decompose a polynomial into its [https://en.wikipedia.org/wiki/Gr%C3%B6bner_basis Gröbner bases]. Polynomials are represented as hash-maps of monomials with tuples of exponents as keys and their corresponding coefficients as values: e.g. 2xy + 3x + 5y + 7 is represented as {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}.

Since this algorithm is much more efficient when the input is in [https://en.wikipedia.org/wiki/Monomial_order#Graded_reverse_lexicographic_order graded reverse lexicographic (grevlex) order] a comparator is included to be used with Clojure's sorted-map&mdash;<code>(into (sorted-map-by grevlex) ...)</code>&mdash;as well as necessary functions to compute polynomial multiplication, monomial complements, and S-polynomials.


```clojure
(defn grevlex [term1 term2]
  (let [grade1 (reduce +' term1)
        grade2 (reduce +' term2)
        comp (- grade2 grade1)] ;; total degree
    (if (not= 0 comp)
      comp
      (loop [term1 term1
             term2 term2]
        (if (empty? term1)
          0
          (let [grade1 (last term1)
                grade2 (last term2)
                comp (- grade1 grade2)] ;; differs from grlex because terms are flipped from above
            (if (not= 0 comp)
            comp
            (recur (pop term1)
                   (pop term2)))))))))

(defn mul
  ;; transducer
  ([poly1]  ;; completion
   (fn
     ([] poly1)
     ([poly2] (mul poly1 poly2))
     ([poly2 & more] (mul poly1 poly2 more))))
  ([poly1 poly2]
   (let [product (atom (transient (sorted-map-by grevlex)))]
     (doall  ;; `for` is lazy so must to be forced for side-effects
      (for [term1 poly1
            term2 poly2
            :let [vars (mapv +' (key term1) (key term2))
                  coeff (* (val term1) (val term2))]]
        (if (contains? @product vars)
          (swap! product assoc! vars (+ (get @product vars) coeff))
          (swap! product assoc! vars coeff))))
     (->> product
          (deref)
          (persistent!)
          (denull))))
  ([poly1 poly2 & more]
   (reduce mul (mul poly1 poly2) more)))

(defn compl [term1 term2]
  (map (fn [x y]
         (cond
           (and (zero? x) (not= 0 y)) nil
           (< x y) nil
           (>= x y) (- x y)))
       term1
       term2))

(defn s-poly [f g]
  (let [f-vars (first f)
        g-vars (first g)
        lcm (compl f-vars g-vars)]
    (if (not-any? nil? lcm)
      {(vec lcm)
       (/ (second f) (second g))})))

(defn divide [f g]
  (loop [f f
         g g
         result (transient {})
         remainder {}]
    (if (empty? f)
      (list (persistent! result)
            (->> remainder
                 (filter #(not (nil? %)))
                 (into (sorted-map-by grevlex))))
      (let [term1 (first f)
            term2 (first g)
            s-term (s-poly term1 term2)]
        (if (nil? s-term)
          (recur (dissoc f (first term1))
                 (dissoc g (first term2))
                 result
                 (conj remainder term1))
          (recur (sub f (mul g s-term))
                 g
                 (conj! result s-term)
                 remainder))))))

(deftest divide-tests
  (is (= (divide {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7})
         '({[0 0] 1} {})))
  (is (= (divide {[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7}
                 {[0 0] 1})
         '({[1 1] 2, [1 0] 3, [0 1] 5, [0 0] 7} {})))
  (is (= (divide {[1 1] 2, [1 0] 10, [0 1] 3, [0 0] 15}
                 {[0 1] 1, [0 0] 5})
         '({[1 0] 2, [0 0] 3} {})))
  (is (= (divide {[1 1] 2, [1 0] 10, [0 1] 3, [0 0] 15}
                 {[1 0] 2, [0 0] 3})
         '({[0 1] 1, [0 0] 5} {}))))
```



## Common Lisp


Polynomials are represented as lists of degree/coefficient pairs ordered by degree (highest degree first), and pairs with zero coefficients can be omitted.  <code>Multiply</code> and <code>divide</code> perform long multiplication and long division, respectively.  <code>multiply</code> returns one value, the product, and <code>divide</code> returns two, the quotient and the remainder.


```lisp
(defun add (p1 p2)
  (do ((sum '())) ((and (endp p1) (endp p2)) (nreverse sum))
    (let ((pd1 (if (endp p1) -1 (caar p1)))
          (pd2 (if (endp p2) -1 (caar p2))))
      (multiple-value-bind (c1 c2)
          (cond
           ((> pd1 pd2) (values (cdr (pop p1)) 0))
           ((< pd1 pd2) (values 0 (cdr (pop p2))))
           (t  (values (cdr (pop p1)) (cdr (pop p2)))))
        (let ((csum (+ c1 c2)))
          (unless (zerop csum)
            (setf sum (acons (max pd1 pd2) csum sum))))))))

(defun multiply (p1 p2)
  (flet ((*p2 (p)
           (destructuring-bind (d . c) p
             (loop for (pd . pc) in p2
                   collecting (cons (+ d pd) (* c pc))))))
    (reduce 'add (mapcar #'*p2 p1) :initial-value '())))

(defun subtract (p1 p2)
  (add p1 (multiply '((0 . -1)) p2)))

(defun divide (dividend divisor &aux (sum '()))
  (assert (not (endp divisor)) (divisor)
    'division-by-zero
    :operation 'divide
    :operands (list dividend divisor))
  (flet ((floor1 (dividend divisor)
           (if (endp dividend) (values '() ())
             (destructuring-bind (d1 . c1) (first dividend)
               (destructuring-bind (d2 . c2) (first divisor)
                 (if (> d2 d1) (values '() dividend)
                   (let* ((quot (list (cons (- d1 d2) (/ c1 c2))))
                          (rem (subtract dividend (multiply divisor quot))))
                     (values quot rem))))))))
    (loop (multiple-value-bind (quotient remainder)
              (floor1 dividend divisor)
            (if (endp quotient) (return (values sum remainder))
              (setf dividend remainder
                    sum (add quotient sum)))))))
```


The [[wp:Polynomial_long_division#Example|wikipedia example]]:


```lisp>
 (divide '((3 . 1) (2 . -12) (0 . -42)) ; x^3 - 12x^2 - 42
          '((1 . 1) (0 . -3)))           ; x - 3
((2 . 1) (1 . -9) (0 . -27)) ; x^2 - 9x - 27
((0 . -123))                 ; -123
```



## D


```d
import std.stdio, std.range, std.algorithm, std.typecons, std.conv;

Tuple!(double[], double[]) polyDiv(in double[] inN, in double[] inD)
nothrow pure @safe {
    // Code smell: a function that does two things.
    static int trimAndDegree(T)(ref T[] poly) nothrow pure @safe @nogc {
        poly = poly.retro.find!q{ a != b }(0.0).retro;
        return poly.length.signed - 1;
    }

    auto N = inN.dup;
    const(double)[] D = inD;
    const dD = trimAndDegree(D);
    auto dN = trimAndDegree(N);
    double[] q;
    if (dD < 0)
        throw new Error("ZeroDivisionError");
    if (dN >= dD) {
        q = [0.0].replicate(dN);
        while (dN >= dD) {
            auto d = [0.0].replicate(dN - dD) ~ D;
            immutable mult = q[dN - dD] = N[$ - 1] / d[$ - 1];
            d[] *= mult;
            N[] -= d[];
            dN = trimAndDegree(N);
        }
    } else
        q = [0.0];
    return tuple(q, N);
}


int trimAndDegree1(T)(ref T[] poly) nothrow pure @safe @nogc {
    poly.length -= poly.retro.countUntil!q{ a != 0 };
    return poly.length.signed - 1;
}

void main() {
    immutable N = [-42.0, 0.0, -12.0, 1.0];
    immutable D = [-3.0, 1.0, 0.0, 0.0];
    writefln("%s / %s = %s  remainder %s", N, D, polyDiv(N, D)[]);
}
```

{{out}}

```txt
[-42, 0, -12, 1] / [-3, 1, 0, 0] = [-27, -9, 1]  remainder [-123]
```



## E

{{lines too long|E}}
This program has some unnecessary features contributing to its length:
* It creates polynomial objects rather than performing its operations directly on arrays.
* It includes code for printing polynomials nicely.
* It prints the intermediate steps of the division.


```txt
pragma.syntax("0.9")
pragma.enable("accumulator")

def superscript(x, out) {
    if (x >= 10) { superscript(x // 10) }
    out.print("⁰¹²³⁴⁵⁶⁷⁸⁹"[x %% 10])
}

def makePolynomial(initCoeffs :List) {
    def degree := {
        var i := initCoeffs.size() - 1
        while (i >= 0 && initCoeffs[i] &lt;=> 0) { i -= 1 }
        if (i &lt; 0) { -Infinity } else { i }
    }
    def coeffs := initCoeffs(0, if (degree == -Infinity) { [] } else { degree + 1 })

    def polynomial {
      /** Print the polynomial (not necessary for the task) */
        to __printOn(out) {
            out.print("(λx.")
            var first := true
            for i in (0..!(coeffs.size())).descending() {
                def coeff := coeffs[i]
                if (coeff &lt;=> 0) { continue }
                out.print(" ")
                if (coeff &lt;=> 1 && !(i &lt;=> 0)) {
                  # no coefficient written if it's 1 and not the constant term
                } else if (first) {      out.print(coeff)
                } else if (coeff > 0) {  out.print("+ ", coeff)
                } else {                 out.print("- ", -coeff)
                }
                if (i &lt;=> 0) {         # no x if it's the constant term
                } else if (i &lt;=> 1) {  out.print("x")
                } else {               out.print("x"); superscript(i, out)
                }
                first := false
            }
            out.print(")")
        }

        /** Evaluate the polynomial (not necessary for the task) */
        to run(x) {
          return accum 0 for i => c in coeffs { _ + c * x**i }
        }

        to degree() { return degree }
        to coeffs() { return coeffs }
        to highestCoeff() { return coeffs[degree] }

        /** Could support another polynomial, but not part of this task.
            Computes this * x**power. */
        to timesXToThe(power) {
            return makePolynomial([0] * power + coeffs)
        }

        /** Multiply (by a scalar only). */
        to multiply(scalar) {
            return makePolynomial(accum [] for x in coeffs { _.with(x * scalar) })
        }

        /** Subtract (by another polynomial only). */
        to subtract(other) {
            def oc := other.coeffs() :List
            return makePolynomial(accum [] for i in 0..(coeffs.size().max(oc.size())) { _.with(coeffs.fetch(i, fn{0}) - oc.fetch(i, fn{0})) })
        }

        /** Polynomial long division. */
        to quotRem(denominator, trace) {
            var numerator := polynomial
            require(denominator.degree() >= 0)
            if (numerator.degree() &lt; denominator.degree()) {
                return [makePolynomial([]), denominator]
            } else {
                var quotientCoeffs := [0] * (numerator.degree() - denominator.degree())
                while (numerator.degree() >= denominator.degree()) {
                    trace.print("  ", numerator, "\n")

                    def qCoeff := numerator.highestCoeff() / denominator.highestCoeff()
                    def qPower := numerator.degree() - denominator.degree()
                    quotientCoeffs with= (qPower, qCoeff)

                    def d := denominator.timesXToThe(qPower) * qCoeff
                    trace.print("- ", d,  "          (= ", denominator, " * ", qCoeff, "x"); superscript(qPower, trace); trace.print(")\n")
                    numerator -= d

                    trace.print("  -------------------------- (Quotient so far: ",  makePolynomial(quotientCoeffs), ")\n")
                }
                return [makePolynomial(quotientCoeffs), numerator]
            }
        }
    }
    return polynomial
}
```



```e
def n := makePolynomial([-42, 0, -12, 1])
def d := makePolynomial([-3, 1])
println("Numerator: ", n)
println("Denominator: ", d)
def [q, r] := n.quotRem(d, stdout)
println("Quotient: ", q)
println("Remainder: ", r)
```


Output:

 Numerator: (λx. x³ - 12x² - 42)
 Denominator: (λx. x - 3)
   (λx. x³ - 12x² - 42)
 - (λx. x³ - 3.0x²)          (= (λx. x - 3) * 1.0x²)
   -------------------------- (Quotient so far: (λx. x²))
   (λx. -9.0x² - 42.0)
 - (λx. -9.0x² + 27.0x)          (= (λx. x - 3) * -9.0x¹)
   -------------------------- (Quotient so far: (λx. x² - 9.0x))
   (λx. -27.0x - 42.0)
 - (λx. -27.0x + 81.0)          (= (λx. x - 3) * -27.0x⁰)
   -------------------------- (Quotient so far: (λx. x² - 9.0x - 27.0))
 Quotient: (λx. x² - 9.0x - 27.0)
 Remainder: (λx. -123.0)


## Elixir

{{trans|Ruby}}

```elixir
defmodule Polynomial do
  def division(_, []), do: raise ArgumentError, "denominator is zero"
  def division(_, [0]), do: raise ArgumentError, "denominator is zero"
  def division(f, g) when length(f) < length(g), do: {[0], f}
  def division(f, g) do
    {q, r} = division(g, [], f)
    if q==[], do: q = [0]
    if r==[], do: r = [0]
    {q, r}
  end

  defp division(g, q, r) when length(r) < length(g), do: {q, r}
  defp division(g, q, r) do
    p = hd(r) / hd(g)
    r2 = Enum.zip(r, g)
         |> Enum.with_index
         |> Enum.reduce(r, fn {{pn,pg},i},acc ->
              List.replace_at(acc, i, pn - p * pg)
            end)
    division(g, q++[p], tl(r2))
  end
end

[ { [1, -12, 0, -42], [1, -3] },
  { [1, -12, 0, -42], [1, 1, -3] },
  { [1, 3, 2],        [1, 1] },
  { [1, -4, 6, 5, 3], [1, 2, 1] } ]
|> Enum.each(fn {f,g} ->
     {q, r} = Polynomial.division(f, g)
     IO.puts "#{inspect f} / #{inspect g} => #{inspect q} remainder #{inspect r}"
   end)
```


{{out}}

```txt

[1, -12, 0, -42] / [1, -3] => [1.0, -9.0, -27.0] remainder [-123.0]
[1, -12, 0, -42] / [1, 1, -3] => [1.0, -13.0] remainder [16.0, -81.0]
[1, 3, 2] / [1, 1] => [1.0, 2.0] remainder [0.0]
[1, -4, 6, 5, 3] / [1, 2, 1] => [1.0, -6.0, 17.0] remainder [-23.0, -14.0]

```



## Factor


```factor
USE: math.polynomials

{ -42 0 -12 1 } { -3 1 } p/mod ptrim [ . ] bi@
```

{{out}}

```txt

V{ -27 -9 1 }
V{ -123 }

```



## Fortran

{{works with|Fortran|95 and later}}


```fortran
module Polynom
  implicit none

contains

  subroutine poly_long_div(n, d, q, r)
    real, dimension(:), intent(in) :: n, d
    real, dimension(:), intent(out), allocatable :: q
    real, dimension(:), intent(out), allocatable, optional :: r

    real, dimension(:), allocatable :: nt, dt, rt
    integer :: gn, gt, gd

    if ( (size(n) >= size(d)) .and. (size(d) > 0) .and. (size(n) > 0) ) then
       allocate(nt(size(n)), dt(size(n)), rt(size(n)))

       nt = n
       dt = 0
       dt(1:size(d)) = d
       rt = 0
       gn = size(n)-1
       gd = size(d)-1
       gt = 0

       do while ( d(gd+1) == 0 )
          gd = gd - 1
       end do

       do while( gn >= gd )
          dt = eoshift(dt, -(gn-gd))
          rt(gn-gd+1) = nt(gn+1) / dt(gn+1)
          nt = nt - dt * rt(gn-gd+1)
          gt = max(gt, gn-gd)
          do
             gn = gn - 1
             if ( nt(gn+1) /= 0 ) exit
          end do
          dt = 0
          dt(1:size(d)) = d
       end do

       allocate(q(gt+1))
       q = rt(1:gt+1)
       if ( present(r) ) then
          if ( (gn+1) > 0 ) then
             allocate(r(gn+1))
             r = nt(1:gn+1)
          else
             allocate(r(1))
             r = 0.0
          end if
       end if
       deallocate(nt, dt, rt)
    else
       allocate(q(1))
       q = 0
       if ( present(r) ) then
          allocate(r(size(n)))
          r = n
       end if
    end if

  end subroutine poly_long_div

  subroutine poly_print(p)
    real, dimension(:), intent(in) :: p

    integer :: i

    do i = size(p), 1, -1
       if ( i > 1 ) then
          write(*, '(F0.2,"x^",I0," + ")', advance="no") p(i), i-1
       else
          write(*, '(F0.2)') p(i)
       end if
    end do

  end subroutine poly_print

end module Polynom
```



```fortran
program PolyDivTest
  use Polynom
  implicit none

  real, dimension(:), allocatable :: q
  real, dimension(:), allocatable :: r

  !! three tests from Wikipedia, plus an extra
  !call poly_long_div( (/ -3., 1. /), (/ -42., 0.0, -12., 1. /), q, r)
  call poly_long_div( (/ -42., 0.0, -12., 1. /), (/ -3., 1. /), q, r)
  !call poly_long_div( (/ -42., 0.0, -12., 1. /), (/ -3., 1., 1. /), q, r)
  !call poly_long_div( (/ 2., 3., 1. /), (/ 1., 1. /), q, r)

  call poly_print(q)
  call poly_print(r)
  deallocate(q, r)

end program PolyDivTest
```



## Go

By the convention and pseudocode given in the task:

```go
package main

import "fmt"

func main() {
    n := []float64{-42, 0, -12, 1}
    d := []float64{-3, 1}
    fmt.Println("N:", n)
    fmt.Println("D:", d)
    q, r, ok := pld(n, d)
    if ok {
        fmt.Println("Q:", q)
        fmt.Println("R:", r)
    } else {
        fmt.Println("error")
    }
}

func degree(p []float64) int {
    for d := len(p) - 1; d >= 0; d-- {
        if p[d] != 0 {
            return d
        }
    }
    return -1
}

func pld(nn, dd []float64) (q, r []float64, ok bool) {
    if degree(dd) < 0 {
        return
    }
    nn = append(r, nn...)
    if degree(nn) >= degree(dd) {
        q = make([]float64, degree(nn)-degree(dd)+1)
        for degree(nn) >= degree(dd) {
            d := make([]float64, degree(nn)+1)
            copy(d[degree(nn)-degree(dd):], dd)
            q[degree(nn)-degree(dd)] = nn[degree(nn)] / d[degree(d)]
            for i := range d {
                d[i] *= q[degree(nn)-degree(dd)]
                nn[i] -= d[i]
            }
        }
    }
    return q, nn, true
}
```

Output:

```txt

N: [-42 0 -12 1]
D: [-3 1]
Q: [-27 -9 1]
R: [-123 0 0 0]

```



## GAP

GAP has built-in functions for computations with polynomials.

```gap
x := Indeterminate(Rationals, "x");
p := x^11 + 3*x^8 + 7*x^2 + 3;
q := x^7 + 5*x^3 + 1;
QuotientRemainder(p, q);
# [ x^4+3*x-5, -16*x^4+25*x^3+7*x^2-3*x+8 ]
```



## Haskell


Translated from the OCaml code elsewhere on the page.
{{works with|GHC|6.10}}

```haskell
import Data.List

shift n l = l ++ replicate n 0

pad n l = replicate n 0 ++ l

norm :: Fractional a => [a] -> [a]
norm = dropWhile (== 0)

deg l = length (norm l) - 1

zipWith' op p q = zipWith op (pad (-d) p) (pad d q)
  where d = (length p) - (length q)

polydiv f g = aux (norm f) (norm g) []
  where aux f s q | ddif < 0 = (q, f)
                  | otherwise = aux f' s q'
           where ddif = (deg f) - (deg s)
                 k = (head f) / (head s)
                 ks = map (* k) $ shift ddif s
                 q' = zipWith' (+) q $ shift ddif [k]
                 f' = norm $ tail $ zipWith' (-) f ks
```


And this is the also-translated pretty printing function.


```haskell
str_poly l = intercalate " + " $ terms l
  where term v 0 = show v
        term 1 1 = "x"
        term v 1 = (show v) ++ "x"
        term 1 p = "x^" ++ (show p)
        term v p = (show v) ++ "x^" ++ (show p)

        terms :: Fractional a => [a] -> [String]
        terms [] = []
        terms (0:t) = terms t
        terms (h:t) = (term h (length t)) : (terms t)
```



## J


From http://www.jsoftware.com/jwiki/Phrases/Polynomials


```J
divmod=:[: (}: ; {:) ([ (] -/@,:&}. (* {:)) ] , %&{.~)^:(>:@-~&#)&.|.~
```


Wikipedia example:

```J
_42 0 _12 1 divmod _3 1
```

This produces the result:
 ┌────────┬────┐
 │_27 _9 1│_123│
 └────────┴────┘

This means that <math>-42-12 x^2+x^3</math> divided by <math>-3+x</math> produces <math>-27-9 x+x^2</math> with a remainder of <math>-123</math>.


## Java

{{trans|Kotlin}}

```Java
import java.util.Arrays;

public class PolynomialLongDivision {
    private static class Solution {
        double[] quotient, remainder;

        Solution(double[] q, double[] r) {
            this.quotient = q;
            this.remainder = r;
        }
    }

    private static int polyDegree(double[] p) {
        for (int i = p.length - 1; i >= 0; --i) {
            if (p[i] != 0.0) return i;
        }
        return Integer.MIN_VALUE;
    }

    private static double[] polyShiftRight(double[] p, int places) {
        if (places <= 0) return p;
        int pd = polyDegree(p);
        if (pd + places >= p.length) {
            throw new IllegalArgumentException("The number of places to be shifted is too large");
        }
        double[] d = Arrays.copyOf(p, p.length);
        for (int i = pd; i >= 0; --i) {
            d[i + places] = d[i];
            d[i] = 0.0;
        }
        return d;
    }

    private static void polyMultiply(double[] p, double m) {
        for (int i = 0; i < p.length; ++i) {
            p[i] *= m;
        }
    }

    private static void polySubtract(double[] p, double[] s) {
        for (int i = 0; i < p.length; ++i) {
            p[i] -= s[i];
        }
    }

    private static Solution polyLongDiv(double[] n, double[] d) {
        if (n.length != d.length) {
            throw new IllegalArgumentException("Numerator and denominator vectors must have the same size");
        }
        int nd = polyDegree(n);
        int dd = polyDegree(d);
        if (dd < 0) {
            throw new IllegalArgumentException("Divisor must have at least one one-zero coefficient");
        }
        if (nd < dd) {
            throw new IllegalArgumentException("The degree of the divisor cannot exceed that of the numerator");
        }
        double[] n2 = Arrays.copyOf(n, n.length);
        double[] q = new double[n.length];
        while (nd >= dd) {
            double[] d2 = polyShiftRight(d, nd - dd);
            q[nd - dd] = n2[nd] / d2[nd];
            polyMultiply(d2, q[nd - dd]);
            polySubtract(n2, d2);
            nd = polyDegree(n2);
        }
        return new Solution(q, n2);
    }

    private static void polyShow(double[] p) {
        int pd = polyDegree(p);
        for (int i = pd; i >= 0; --i) {
            double coeff = p[i];
            if (coeff == 0.0) continue;
            if (coeff == 1.0) {
                if (i < pd) {
                    System.out.print(" + ");
                }
            } else if (coeff == -1.0) {
                if (i < pd) {
                    System.out.print(" - ");
                } else {
                    System.out.print("-");
                }
            } else if (coeff < 0.0) {
                if (i < pd) {
                    System.out.printf(" - %.1f", -coeff);
                } else {
                    System.out.print(coeff);
                }
            } else {
                if (i < pd) {
                    System.out.printf(" + %.1f", coeff);
                } else {
                    System.out.print(coeff);
                }
            }
            if (i > 1) System.out.printf("x^%d", i);
            else if (i == 1) System.out.print("x");
        }
        System.out.println();
    }

    public static void main(String[] args) {
        double[] n = new double[]{-42.0, 0.0, -12.0, 1.0};
        double[] d = new double[]{-3.0, 1.0, 0.0, 0.0};
        System.out.print("Numerator   : ");
        polyShow(n);
        System.out.print("Denominator : ");
        polyShow(d);
        System.out.println("-------------------------------------");
        Solution sol = polyLongDiv(n, d);
        System.out.print("Quotient    : ");
        polyShow(sol.quotient);
        System.out.print("Remainder   : ");
        polyShow(sol.remainder);
    }
}
```

{{out}}

```txt
Numerator   : x^3 - 12.0x^2 - 42.0
Denominator : x - 3.0
-------------------------------------
Quotient    : x^2 - 9.0x - 27.0
Remainder   : -123.0
```



## Julia

This task is straightforward with the help of Julia's [https://github.com/Keno/Polynomials.jl Polynomials] package.

```Julia

using Polynomials

p = Poly([-42,0,-12,1])
q = Poly([-3,1])

d, r = divrem(p,q)

println(p, " divided by ", q, " is ", d, " with remainder ", r, ".")

```


{{out}}

```txt

-42 - 12x^2 + x^3 divided by -3 + x is -27.0 - 9.0x + x^2 with remainder -123.0.

```



## Kotlin


```scala
// version 1.1.51

typealias IAE = IllegalArgumentException

data class Solution(val quotient: DoubleArray, val remainder: DoubleArray)

fun polyDegree(p: DoubleArray): Int {
    for (i in p.size - 1 downTo 0) {
        if (p[i] != 0.0) return i
    }
    return Int.MIN_VALUE
}

fun polyShiftRight(p: DoubleArray, places: Int): DoubleArray {
    if (places <= 0) return p
    val pd = polyDegree(p)
    if (pd + places >= p.size) {
        throw IAE("The number of places to be shifted is too large")
    }
    val d = p.copyOf()
    for (i in pd downTo 0) {
        d[i + places] = d[i]
        d[i] = 0.0
    }
    return d
}

fun polyMultiply(p: DoubleArray, m: Double) {
    for (i in 0 until p.size) p[i] *= m
}

fun polySubtract(p: DoubleArray, s: DoubleArray) {
    for (i in 0 until p.size) p[i] -= s[i]
}

fun polyLongDiv(n: DoubleArray, d: DoubleArray): Solution {
    if (n.size != d.size) {
        throw IAE("Numerator and denominator vectors must have the same size")
    }
    var nd = polyDegree(n)
    val dd = polyDegree(d)
    if (dd < 0) {
        throw IAE("Divisor must have at least one one-zero coefficient")
    }
    if (nd < dd) {
        throw IAE("The degree of the divisor cannot exceed that of the numerator")
    }
    val n2 = n.copyOf()
    val q = DoubleArray(n.size)  // all elements zero by default
    while (nd >= dd) {
        val d2 = polyShiftRight(d, nd - dd)
        q[nd - dd] = n2[nd] / d2[nd]
        polyMultiply(d2, q[nd - dd])
        polySubtract(n2, d2)
        nd = polyDegree(n2)
    }
    return Solution(q, n2)
}

fun polyShow(p: DoubleArray) {
    val pd = polyDegree(p)
    for (i in pd downTo 0) {
        val coeff = p[i]
        if (coeff == 0.0) continue
        print (when {
            coeff ==  1.0  -> if (i < pd) " + " else ""
            coeff == -1.0  -> if (i < pd) " - " else "-"
            coeff <   0.0  -> if (i < pd) " - ${-coeff}" else "$coeff"
            else           -> if (i < pd) " + $coeff" else "$coeff"
        })
        if (i > 1) print("x^$i")
        else if (i == 1) print("x")
    }
    println()
}

fun main(args: Array<String>) {
    val n = doubleArrayOf(-42.0, 0.0, -12.0, 1.0)
    val d = doubleArrayOf( -3.0, 1.0,   0.0, 0.0)
    print("Numerator   : ")
    polyShow(n)
    print("Denominator : ")
    polyShow(d)
    println("-------------------------------------")
    val (q, r) = polyLongDiv(n, d)
    print("Quotient    : ")
    polyShow(q)
    print("Remainder   : ")
    polyShow(r)
}
```


{{out}}

```txt

Numerator   : x^3 - 12.0x^2 - 42.0
Denominator : x - 3.0
-------------------------------------
Quotient    : x^2 - 9.0x - 27.0
Remainder   : -123.0

```



## Maple

As Maple is a symbolic computation system, polynomial arithmetic is, of course, provided by the language runtime.  The remainder (rem) and quotient (quo) operations each allow for the other to be computed simultaneously by passing an unassigned name as an optional fourth argument.  Since rem and quo deal also with multivariate polynomials, the indeterminate is passed as the third argument.

```Maple

> p := randpoly( x ); # pick a random polynomial in x
                           5       4       3       2
             p := -56 - 7 x  + 22 x  - 55 x  - 94 x  + 87 x

> rem( p, x^2 + 2, x, 'q' ); # remainder
                              220 + 169 x

> q; # quotient
                           3       2
                       -7 x  + 22 x  - 41 x - 138

> quo( p, x^2 + 2, x, 'r' ); # quotient
                           3       2
                       -7 x  + 22 x  - 41 x - 138

> r; # remainder
                              220 + 169 x
> expand( (x^2+2)*q + r - p ); # check
                                   0

```



## Mathematica


```Mathematica
PolynomialQuotientRemainder[x^3-12 x^2-42,x-3,x]
```

output:

```txt
{-27 - 9 x + x^2, -123}
```



## OCaml


First define some utility operations on polynomials as lists (with highest power coefficient first).

```ocaml
let rec shift n l = if n <= 0 then l else shift (pred n) (l @ [0.0])
let rec pad n l = if n <= 0 then l else pad (pred n) (0.0 :: l)
let rec norm = function | 0.0 :: tl -> norm tl | x -> x
let deg l = List.length (norm l) - 1

let zip op p q =
  let d = (List.length p) - (List.length q) in
  List.map2 op (pad (-d) p) (pad d q)
```

Then the main polynomial division function

```ocaml
let polydiv f g =
  let rec aux f s q =
    let ddif = (deg f) - (deg s) in
    if ddif < 0 then (q, f) else
      let k = (List.hd f) /. (List.hd s) in
      let ks = List.map (( *.) k) (shift ddif s) in
      let q' = zip (+.) q (shift ddif [k])
      and f' = norm (List.tl (zip (-.) f ks)) in
      aux f' s q' in
  aux (norm f) (norm g) []
```

For output we need a pretty-printing function

```ocaml
let str_poly l =
  let term v p = match (v, p) with
    | (  _, 0) -> string_of_float v
    | (1.0, 1) -> "x"
    | (  _, 1) -> (string_of_float v) ^ "*x"
    | (1.0, _) -> "x^" ^ (string_of_int p)
    | _ -> (string_of_float v) ^ "*x^" ^ (string_of_int p) in
  let rec terms = function
    | [] -> []
    | h :: t ->
       if h = 0.0 then (terms t) else (term h (List.length t)) :: (terms t) in
  String.concat " + " (terms l)
```

and then the example

```ocaml
let _ =
  let f = [1.0; -4.0; 6.0; 5.0; 3.0] and g = [1.0; 2.0; 1.0] in
  let q, r = polydiv f g in
  Printf.printf
    " (%s) div (%s)\ngives\nquotient:\t(%s)\nremainder:\t(%s)\n"
    (str_poly f) (str_poly g) (str_poly q) (str_poly r)
```

gives the output:

```txt

 (x^4 + -4.*x^3 + 6.*x^2 + 5.*x + 3.) div (x^2 + 2.*x + 1.)
gives
quotient:	(x^2 + -6.*x + 17.)
remainder:	(-23.*x + -14.)

```



## Octave

Octave has already facilities to divide two polynomials (<code>deconv(n,d)</code>); and the reason to adopt the convention of keeping the highest power coefficient first, is to make the code compatible with builtin functions: we can use <tt>polyout</tt> to output the result.


```octave
function [q, r] = poly_long_div(n, d)
  gd = length(d);
  pv = zeros(1, length(n));
  pv(1:gd) = d;
  if ( length(n) >= gd )
    q = [];
    while ( length(n) >= gd )
      q = [q, n(1)/pv(1)];
      n = n - pv .* (n(1)/pv(1));
      n = shift(n, -1);           %
      tn = n(1:length(n)-1);      % eat the higher power term
      n = tn;                     %
      tp = pv(1:length(pv)-1);
      pv = tp;                    % make pv the same length of n
    endwhile
    r = n;
  else
    q = [0];
    r = n;
  endif
endfunction

[q, r] = poly_long_div([1,-12,0,-42], [1,-3]);
polyout(q, 'x');
polyout(r, 'x');
disp("");
[q, r] = poly_long_div([1,-12,0,-42], [1,1,-3]);
polyout(q, 'x');
polyout(r, 'x');
disp("");
[q, r] = poly_long_div([1,3,2], [1,1]);
polyout(q, 'x');
polyout(r, 'x');
disp("");
[q, r] = poly_long_div([1,3], [1,-12,0,-42]);
polyout(q, 'x');
polyout(r, 'x');
```



## PARI/GP

This uses the built-in PARI polynomials.

```parigp
poldiv(a,b)={
  my(rem=a%b);
  [(a - rem)/b, rem]
};
poldiv(x^9+1, x^3+x-3)
```

Alternately, use the built-in function <code>divrem</code>:

```parigp
divrem(x^9+1, x^3+x-3)~
```



## Perl

This solution keeps the highest power coefficient first, like [[Polynomial long division#OCaml|OCaml solution]] and [[Polynomial long division#Octave|Octave solution]].

{{trans|Octave}}

```perl
use strict;
use List::Util qw(min);

sub poly_long_div
{
    my ($rn, $rd) = @_;

    my @n = @$rn;
    my $gd = scalar(@$rd);
    if ( scalar(@n) >= $gd ) {
	my @q = ();
	while ( scalar(@n) >= $gd ) {
	    my $piv = $n[0]/$rd->[0];
	    push @q, $piv;
	    $n[$_] -= $rd->[$_] * $piv foreach ( 0 .. min(scalar(@n), $gd)-1 );
	    shift @n;
	}
	return ( \@q, \@n );
    } else {
	return ( [0], $rn );
    }
}
```



```perl
sub poly_print
{
    my @c = @_;
    my $l = scalar(@c);
    for(my $i=0; $i < $l; $i++) {
	print $c[$i];
	print "x^" . ($l-$i-1) . " + " if ($i < ($l-1));
    }
    print "\n";
}
```



```perl
my ($q, $r);

($q, $r) = poly_long_div([1, -12, 0, -42], [1, -3]);
poly_print(@$q);
poly_print(@$r);
print "\n";
($q, $r) = poly_long_div([1,-12,0,-42], [1,1,-3]);
poly_print(@$q);
poly_print(@$r);
print "\n";
($q, $r) = poly_long_div([1,3,2], [1,1]);
poly_print(@$q);
poly_print(@$r);
print "\n";
# the example from the OCaml solution
($q, $r) = poly_long_div([1,-4,6,5,3], [1,2,1]);
poly_print(@$q);
poly_print(@$r);
```



## Perl 6

{{Works with|rakudo|2018.10}}
{{trans|Perl}} for the core algorithm; original code for LaTeX pretty-printing.

```perl6
sub poly_long_div ( @n is copy, @d ) {
    return [0], |@n if +@n < +@d;

    my @q = gather while +@n >= +@d {
        @n = @n Z- flat ( ( @d X* take ( @n[0] / @d[0] ) ), 0 xx * );
        @n.shift;
    }

    return @q, @n;
}

sub xP ( $power ) { $power>1 ?? "x^$power" !! $power==1 ?? 'x' !! '' }
sub poly_print ( @c ) { join ' + ', @c.kv.map: { $^v ~ xP( @c.end - $^k ) } }

my @polys = [ [     1, -12, 0, -42 ], [    1, -3 ] ],
            [ [     1, -12, 0, -42 ], [ 1, 1, -3 ] ],
            [ [          1, 3,   2 ], [    1,  1 ] ],
            [ [ 1, -4,   6, 5,   3 ], [ 1, 2,  1 ] ];

say '<math>\begin{array}{rr}';
for @polys -> [ @a, @b ] {
    printf Q"%s , & %s \\\\\n", poly_long_div( @a, @b ).map: { poly_print($_) };
}
say '\end{array}</math>';
```


Output:

<math>\begin{array}{rr}
1x^2 + -9x + -27 , & -123 \\
1x + -13 , & 16x + -81 \\
1x + 2 , & 0 \\
1x^2 + -6x + 17 , & -23x + -14 \\
\end{array}</math>


## Phix


```Phix
function degree(sequence p)
    for i=length(p) to 1 by -1 do
        if p[i]!=0 then return i end if
    end for
    return -1
end function

function poly_div(sequence n, d)
    while length(d)<length(n) do d &=0 end while
    integer dn = degree(n),
            dd = degree(d)
    if dd<0 then throw("divide by zero") end if
    sequence quot = repeat(0,dn)
    while dn>=dd do
        integer k = dn-dd
        integer qk = n[dn]/d[dd]
        quot[k+1] = qk
        sequence d2 = d[1..length(d)-k]
        for i=1 to length(d2) do
            n[-i] -= d2[-i]*qk
        end for
        dn = degree(n)
    end while
    return {quot,n} -- (n is now the remainder)
end function

function poly(sequence si)
-- display helper
    string r = ""
    for t=length(si) to 1 by -1 do
        integer sit = si[t]
        if sit!=0 then
            if sit=1 and t>1 then
                r &= iff(r=""? "":" + ")
            elsif sit=-1 and t>1 then
                r &= iff(r=""?"-":" - ")
            else
                if r!="" then
                    r &= iff(sit<0?" - ":" + ")
                    sit = abs(sit)
                end if
                r &= sprintf("%d",sit)
            end if
            r &= iff(t>1?"x"&iff(t>2?sprintf("^%d",t-1):""):"")
        end if
    end for
    if r="" then r="0" end if
    return r
end function

function polyn(sequence s)
    for i=1 to length(s) do
        s[i] = poly(s[i])
    end for
    return s
end function

constant tests = {{{-42,0,-12,1},{-3,1}},
                  {{-3,1},{-42,0,-12,1}},
                  {{-42,0,-12,1},{-3,1,1}},
                  {{2,3,1},{1,1}},
                  {{3,5,6,-4,1},{1,2,1}},
                  {{3,0,7,0,0,0,0,0,3,0,0,1},{1,0,0,5,0,0,0,1}},
                  {{-56,87,-94,-55,22,-7},{2,0,1}},
                 }

constant fmt = "%40s / %-16s = %25s rem %s\n"

for i=1 to length(tests) do
    sequence {num,denom} = tests[i],
             {quot,rmdr} = poly_div(num,denom)
    printf(1,fmt,polyn({num,denom,quot,rmdr}))
end for
```

{{out}}

```txt

                        x^3 - 12x^2 - 42 / x - 3            =             x^2 - 9x - 27 rem -123
                                   x - 3 / x^3 - 12x^2 - 42 =                         0 rem x - 3
                        x^3 - 12x^2 - 42 / x^2 + x - 3      =                    x - 13 rem 16x - 81
                            x^2 + 3x + 2 / x + 1            =                     x + 2 rem 0
              x^4 - 4x^3 + 6x^2 + 5x + 3 / x^2 + 2x + 1     =             x^2 - 6x + 17 rem -23x - 14
                  x^11 + 3x^8 + 7x^2 + 3 / x^7 + 5x^3 + 1   =              x^4 + 3x - 5 rem -16x^4 + 25x^3 + 7x^2 - 3x + 8
-7x^5 + 22x^4 - 55x^3 - 94x^2 + 87x - 56 / x^2 + 2          = -7x^3 + 22x^2 - 41x - 138 rem 169x + 220

```



## PicoLisp


```PicoLisp
(de degree (P)
   (let I NIL
      (for (N . C) P
         (or (=0 C) (setq I N)) )
      (dec I) ) )

(de divPoly (N D)
   (if (lt0 (degree D))
      (quit "Div/0" D)
      (let (Q NIL Diff)
         (while (ge0 (setq Diff (- (degree N) (degree D))))
            (setq Q (need (- -1 Diff) Q 0))
            (let E D
               (do Diff (push 'E 0))
               (let F (/ (get N (inc (degree N))) (get E (inc (degree E))))
                  (set (nth Q (inc Diff)) F)
                  (setq N (mapcar '((N E) (- N (* E F))) N E)) ) ) )
         (list Q N) ) ) )
```

Output:

```txt
: (divPoly (-42 0 -12 1) (-3 1 0 0))
-> ((-27 -9 1) (-123 0 0 0))
```



## Python

{{works with|Python 2.x}}

```python
# -*- coding: utf-8 -*-

from itertools import izip
from math import fabs

def degree(poly):
    while poly and poly[-1] == 0:
        poly.pop()   # normalize
    return len(poly)-1

def poly_div(N, D):
    dD = degree(D)
    dN = degree(N)
    if dD < 0: raise ZeroDivisionError
    if dN >= dD:
        q = [0] * dN
        while dN >= dD:
            d = [0]*(dN - dD) + D
            mult = q[dN - dD] = N[-1] / float(d[-1])
            d = [coeff*mult for coeff in d]
            N = [fabs ( coeffN - coeffd ) for coeffN, coeffd in izip(N, d)]
            dN = degree(N)
        r = N
    else:
        q = [0]
        r = N
    return q, r

if __name__ == '__main__':
    print "POLYNOMIAL LONG DIVISION"
    N = [-42, 0, -12, 1]
    D = [-3, 1, 0, 0]
    print "  %s / %s =" % (N,D),
    print " %s remainder %s" % poly_div(N, D)
```


Sample output:

```txt
POLYNOMIAL LONG DIVISION
  [-42, 0, -12, 1] / [-3, 1, 0, 0] =  [-27.0, -9.0, 1.0] remainder [-123.0]
```



## R

{{trans|Octave}}

```R
polylongdiv <- function(n,d) {
  gd <- length(d)
  pv <- vector("numeric", length(n))
  pv[1:gd] <- d
  if ( length(n) >= gd ) {
    q <- c()
    while ( length(n) >= gd ) {
      q <- c(q, n[1]/pv[1])
      n <- n - pv * (n[1]/pv[1])
      n <- n[2:length(n)]
      pv <- pv[1:(length(pv)-1)]
    }
    list(q=q, r=n)
  } else {
    list(q=c(0), r=n)
  }
}

# an utility function to print polynomial
print.polynomial <- function(p) {
  i <- length(p)-1
  for(a in p) {
    if ( i == 0 ) {
      cat(a, "\n")
    } else {
      cat(a, "x^", i, " + ", sep="")
    }
    i <- i - 1
  }
}

r <- polylongdiv(c(1,-12,0,-42), c(1,-3))
print.polynomial(r$q)
print.polynomial(r$r)
```



## Racket


```racket

#lang racket
(define (deg p)
  (for/fold ([d -inf.0]) ([(pi i) (in-indexed p)])
    (if (zero? pi) d i)))
(define (lead p) (vector-ref p (deg p)))
(define (mono c d) (build-vector (+ d 1) (λ(i) (if (= i d) c 0))))
(define (poly*cx^n c n p) (vector-append (make-vector n 0) (for/vector ([pi p]) (* c pi))))
(define (poly+ p q) (poly/lin 1 p  1 q))
(define (poly- p q) (poly/lin 1 p -1 q))
(define (poly/lin a p b q)
  (cond [(< (deg p) 0) q]
        [(< (deg q) 0) p]
        [(< (deg p) (deg q)) (poly/lin b q a p)]
        [else (define ap+bq (for/vector #:length (+ (deg p) 1) #:fill 0
                              ([pi p] [qi q]) (+ (* a pi) (* b qi))))
              (for ([i (in-range (+ (deg q) 1) (+ (deg p) 1))])
                (vector-set! ap+bq i (* a (vector-ref p i))))
              ap+bq]))

(define (poly/ n d)
  (define N (deg n))
  (define D (deg d))
  (cond
    [(< N 0) (error 'poly/ "can't divide by zero")]
    [(< N D) (values 0 n)]
    [else    (define c (/ (lead n) (lead d)))
             (define q (mono c (- N D)))
             (define r (poly- n (poly*cx^n c (- N D) d)))
             (define-values (q1 r1) (poly/ r d))
             (values (poly+ q q1) r1)]))
; Example:
(poly/ #(-42 0 -12 1) #(-3 1))
; Output:
'#(-27 -9 1)
'#(-123 0)

```



## REXX


```rexx
/* REXX needed by some... */
z='1 -12 0 -42'  /* Numerator   */
n='1 -3'         /* Denominator */
zx=z
nx=n copies('0 ',words(z)-words(n))
qx=''            /* Quotient    */
Do Until words(zx)<words(n)
  Parse Value div(zx,nx) With q zx
  qx=qx q
  nx=subword(nx,1,words(nx)-1)
  End
Say '('show(z)')/('show(n)')=('show(qx)')'
Say 'Remainder:' show(zx)
Exit
div: Procedure
Parse Arg z,n
q=word(z,1)/word(n,1)
zz=''
Do i=1 To words(z)
  zz=zz word(z,i)-q*word(n,i)
  End
Return q subword(zz,2)

show: Procedure
Parse Arg poly
d=words(poly)-1
res=''
Do i=1 To words(poly)
  Select
    When d>1 Then fact='*x**'d
    When d=1 Then fact='*x'
    Otherwise     fact=''
    End
  Select
    When word(poly,i)=0  Then p=''
    When word(poly,i)=1  Then p='+'substr(fact,2)
    When word(poly,i)=-1 Then p='-'substr(fact,2)
    When word(poly,i)<0  Then p=word(poly,i)||fact
    Otherwise                 p='+'word(poly,i)||fact
    End
  res=res p
  d=d-1
  End
Return strip(space(res,0),'L','+')
```

{{out}}

```txt
(x**3-12*x**2-42)/(x-3)=(x**2-9*x-27)
Remainder: -123
```



## Ruby

Implementing the algorithm given in the task description:

```ruby
def polynomial_long_division(numerator, denominator)
  dd = degree(denominator)
  raise ArgumentError, "denominator is zero" if dd < 0
  if dd == 0
    return [multiply(numerator, 1.0/denominator[0]), [0]*numerator.length]
  end

  q = [0] * numerator.length

  while (dn = degree(numerator)) >= dd
    d = shift_right(denominator, dn - dd)
    q[dn-dd] = numerator[dn] / d[degree(d)]
    d = multiply(d, q[dn-dd])
    numerator = subtract(numerator, d)
  end

  [q, numerator]
end

def degree(ary)
  idx = ary.rindex(&:nonzero?)
  idx ? idx : -1
end

def shift_right(ary, n)
  [0]*n + ary[0, ary.length - n]
end

def subtract(a1, a2)
  a1.zip(a2).collect {|v1,v2| v1 - v2}
end

def multiply(ary, num)
  ary.collect {|x| x * num}
end

f = [-42, 0, -12, 1]
g = [-3, 1, 0, 0]
q, r = polynomial_long_division(f, g)
puts "#{f} / #{g} => #{q} remainder #{r}"
# => [-42, 0, -12, 1] / [-3, 1, 0, 0] => [-27, -9, 1, 0] remainder [-123, 0, 0, 0]

g = [-3, 1, 1, 0]
q, r = polynomial_long_division(f, g)
puts "#{f} / #{g} => #{q} remainder #{r}"
# => [-42, 0, -12, 1] / [-3, 1, 1, 0] => [-13, 1, 0, 0] remainder [-81, 16, 0, 0]
```


Implementing the algorithms on the [[wp:Polynomial long division|wikipedia page]] -- uglier code but nicer user interface

```ruby
def polynomial_division(f, g)
  if g.length == 0 or (g.length == 1 and g[0] == 0)
    raise ArgumentError, "denominator is zero"
  elsif g.length == 1
    [f.collect {|x| Float(x)/g[0]}, [0]]
  elsif g.length == 2
    synthetic_division(f, g)
  else
    higher_degree_synthetic_division(f, g)
  end
end

def synthetic_division(f, g)
  board = [f] << Array.new(f.length) << Array.new(f.length)
  board[2][0] = board[0][0]

  1.upto(f.length - 1).each do |i|
    board[1][i] = board[2][i-1] * -g[1]
    board[2][i] = board[0][i] + board[1][i]
  end

  [board[2][0..-2], [board[2][-1]]]
end

# an ugly mess of array index arithmetic
# http://en.wikipedia.org/wiki/Polynomial_long_division#Higher_degree_synthetic_division
def higher_degree_synthetic_division(f, g)

  # [use] the negative coefficients of the denominator following the leading term
  lhs = g[1..-1].collect {|x| -x}
  board = [f]

  q = []
  1.upto(f.length - lhs.length).each do |i|
    n = 2*i - 1

    # underline the leading coefficient of the right-hand side, multiply it by
    # the left-hand coefficients and write the products beneath the next columns
    # on the right.
    q << board[n-1][i-1]
    board << Array.new(f.length).fill(0, i) # row n
    (lhs.length).times do |j|
      board[n][i+j] = q[-1]*lhs[j]
    end

    # perform an addition
    board << Array.new(f.length).fill(0, i) # row n+1
    (lhs.length + 1).times do |j|
      board[n+1][i+j] = board[n-1][i+j] + board[n][i+j] if i+j < f.length
    end
  end

  # the remaining numbers in the bottom row correspond to the coefficients of the remainder
  r = board[-1].compact
  q = [0] if q.empty?
  [q, r]
end

f = [1, -12, 0, -42]
g = [1, -3]
q, r = polynomial_division(f, g)
puts "#{f} / #{g} => #{q} remainder #{r}"
# => [1, -12, 0, -42] / [1, -3] => [1, -9, -27] remainder [-123]

g = [1, 1, -3]
q, r = polynomial_division(f, g)
puts "#{f} / #{g} => #{q} remainder #{r}"
# => [1, -12, 0, -42] / [1, 1, -3] => [1, -13] remainder [16, -81]
```


Best of both worlds: {{trans|Tcl}}

```ruby
def polynomial_division(f, g)
  if g.length == 0 or (g.length == 1 and g[0] == 0)
    raise ArgumentError, "denominator is zero"
  end
  return [[0], f] if f.length < g.length

  q, n = [], f.dup
  while n.length >= g.length
    q << Float(n[0]) / g[0]
    n[0, g.length].zip(g).each_with_index do |pair, i|
      n[i] = pair[0] - q[-1] * pair[1]
    end
    n.shift
  end
  q = [0] if q.empty?
  n = [0] if n.empty?
  [q, n]
end

f = [1, -12, 0, -42]
g = [1, -3]
q, r = polynomial_division(f, g)
puts "#{f} / #{g} => #{q} remainder #{r}"
# => [1, -12, 0, -42] / [1, -3] => [1.0, -9.0, -27.0] remainder [-123.0]

g = [1, 1, -3]
q, r = polynomial_division(f, g)
puts "#{f} / #{g} => #{q} remainder #{r}"
# => [1, -12, 0, -42] / [1, 1, -3] => [1.0, -13.0] remainder [16.0, -81.0]
```



## Sidef

{{trans|Perl}}

```ruby
func poly_long_div(rn, rd) {
 
    var n = rn.map{_}
    var gd = rd.len
 
    if (n.len >= gd) {
        return(gather {
            while (n.len >= gd) {
                var piv = n[0]/rd[0]
                take(piv)
                { |i|
                    n[i] -= (rd[i] * piv)
                } << ^(n.len `min` gd)
                n.shift
            }
        }, n)
    }
 
    return([0], rn)
}
```


Example:


```ruby
func poly_print(c) {
    var l = c.len
    c.each_kv {|i, n|
        print n
        print("x^", (l - i - 1), " + ") if (i < l-1)
    }
    print "\n";
}

var poly = [
    Pair([1,-12,0,-42],  [1, -3]),
    Pair([1,-12,0,-42], [1,1,-3]),
    Pair(      [1,3,2],    [1,1]),
    Pair( [1,-4,6,5,3],  [1,2,1]),
]

poly.each { |pair|
    var (q, r) = poly_long_div(pair.first, pair.second)
    poly_print(q)
    poly_print(r)
    print "\n"
}
```


{{out}}

```txt

1x^2 + -9x^1 + -27
-123

1x^1 + -13
16x^1 + -81

1x^1 + 2
0

1x^2 + -6x^1 + 17
-23x^1 + -14

```



## Slate



```Slate
define: #Polynomial &parents: {Comparable} &slots: {#coefficients -> ExtensibleArray new}.

p@(Polynomial traits) new &capacity: n
[
  p cloneSettingSlots: #(coefficients) to: {p coefficients new &capacity: n}
].

p@(Polynomial traits) newFrom: seq@(Sequence traits)
[
  p clone `>> [coefficients: (seq as: p coefficients). normalize. ]
].

p@(Polynomial traits) copy
[
  p cloneSettingSlots: #(coefficients) to: {p coefficients copy}
].

p1@(Polynomial traits) >= p2@(Polynomial traits)
[p1 degree >= p2 degree].

p@(Polynomial traits) degree
[p coefficients indexOfLastSatisfying: [| :n | n isZero not]].

p@(Polynomial traits) normalize
[
  [p degree isPositive /\ [p coefficients last isZero]]
    whileTrue: [p coefficients removeLast]
].

p@(Polynomial traits) * n@(Number traits)
[
  p newFrom: (p coefficients collect: [| :x | x * n])
].

p@(Polynomial traits) / n@(Number traits)
[
  p newFrom: (p coefficients collect: [| :x | x / n])
].

p1@(Polynomial traits) minusCoefficients: p2@(Polynomial traits)
[
  p1 newFrom: (p1 coefficients with: p2 coefficients collect: #- `er)
].

p@(Polynomial traits) / denom@(Polynomial traits)
[
  p >= denom
    ifTrue:
      [| n q |
       n: p copy.
       q: p new.
       [n >= denom]
          whileTrue:
            [| piv |
	     piv: p coefficients last / denom coefficients last.
	     q coefficients add: piv.
	     n: (n minusCoefficients: denom * piv).
	     n normalize].
       n coefficients isEmpty ifTrue: [n coefficients add: 0].
       {q. n}]
    ifFalse: [{p newFrom: #(0). p copy}]
].
```



## Smalltalk

{{works with|GNU Smalltalk}}


```smalltalk
Object subclass: Polynomial [
  |coeffs|
  Polynomial class >> new [ ^ super basicNew init ]
  init [ coeffs := OrderedCollection new. ^ self ]
  Polynomial class >> newWithCoefficients: coefficients [
    |r|
    r := super basicNew.
    ^ r initWithCoefficients: coefficients
  ]
  initWithCoefficients: coefficients [
    coeffs := coefficients asOrderedCollection.
    ^ self
  ]
  / denominator [ |n q|
    n := self deepCopy.
    self >= denominator
      ifTrue: [
        q := Polynomial new.
        [ n >= denominator ]
          whileTrue: [ |piv|
 	    piv := (n coeff: 0) / (denominator coeff: 0).
	    q addCoefficient: piv.
	    n := n - (denominator * piv).
	    n clean
          ].
        ^ { q . (n degree) > 0 ifTrue: [ n ] ifFalse: [ n addCoefficient: 0. n ] }
      ]
      ifFalse: [
        ^ { Polynomial newWithCoefficients: #( 0 ) . self deepCopy }
      ]
  ]
  * constant [ |r| r := self deepCopy.
    1 to: (coeffs size) do: [ :i |
      r at: i put: ((r at: i) * constant)
    ].
    ^ r
  ]
  at: index [ ^ coeffs at: index ]
  at: index put: obj [ ^ coeffs at: index put: obj ]
  >= anotherPoly [
    ^ (self degree) >= (anotherPoly degree)
  ]
  degree [ ^ coeffs size ]
  - anotherPoly [ "This is not a real subtraction between Polynomial: it is an
                   internal method ..."
    |a|
    a := self deepCopy.
    1 to: ( (coeffs size) min: (anotherPoly degree) ) do: [ :i |
      a at: i put: ( (a at: i) - (anotherPoly at: i) )
    ].
    ^ a
  ]
  coeff: index [ ^ coeffs at: (index + 1) ]
  addCoefficient: coeff [ coeffs add: coeff ]
  clean [
    [ (coeffs size) > 0
        ifTrue: [ (coeffs at: 1) = 0 ] ifFalse: [ false ] ]
      whileTrue: [ coeffs removeFirst ].
  ]
  display [
    1 to: (coeffs size) do: [ :i |
      (coeffs at: i) display.
      i < (coeffs size)
        ifTrue: [ ('x^%1 + ' % {(coeffs size) - i} ) display ]
    ]
  ]
  displayNl [ self display. Character nl display ]
].
```



```smalltalk
|res|
res := OrderedCollection new.

res add:  ((Polynomial newWithCoefficients: #( 1 -12 0 -42) ) /
           (Polynomial newWithCoefficients: #( 1 -3 ) )) ;
    add:  ((Polynomial newWithCoefficients: #( 1 -12 0 -42) ) /
           (Polynomial newWithCoefficients: #( 1 1 -3 ) )).

res do: [ :o |
  (o at: 1) display. ' with rest: ' display. (o at: 2) displayNl
]
```



## SPAD

{{works with|FriCAS}}
{{works with|OpenAxiom}}
{{works with|Axiom}}

```SPAD
(1) -> monicDivide(x^3-12*x^2-42,x-3,'x)

                     2
   (1)  [quotient = x  - 9x - 27,remainder = - 123]

   Type: Record(quotient: Polynomial(Integer),remainder: Polynomial(Integer))
```


Domain:[http://fricas.github.io/api/PolynomialCategory.html#l-polynomial-category-monic-divide]



## Tcl

{{works with|Tcl|8.5 and later}}


```tcl
# poldiv - Divide two polynomials n and d.
#          Result is a list of two polynomials, q and r, where n = qd + r
#          and the degree of r is less than the degree of b.
#          Polynomials are represented as lists, where element 0 is the
#          x**0 coefficient, element 1 is the x**1 coefficient, and so on.

proc poldiv {a b} {
    # Toss out leading zero coefficients efficiently
    while {[lindex $a end] == 0} {set a [lrange $a[set a {}] 0 end-1]}
    while {[lindex $b end] == 0} {set b [lrange $b[set b {}] 0 end-1]}
    if {[llength $a] < [llength $b]} {
        return [list 0 $a]
    }

    # Rearrange the terms to put highest powers first
    set n [lreverse $a]
    set d [lreverse $b]

    # Carry out classical long division, accumulating quotient coefficients
    # in q, and replacing n with the remainder.
    set q {}
    while {[llength $n] >= [llength $d]} {
        set qd [expr {[lindex $n 0] / [lindex $d 0]}]
        set i 0
        foreach nd [lrange $n 0 [expr {[llength $d] - 1}]] dd $d {
            lset n $i [expr {$nd - $qd * $dd}]
            incr i
        }
        lappend q $qd
        set n [lrange $n 1 end]
    }

    # Return quotient and remainder, constant term first
    return [list [lreverse $q] [lreverse $n]]
}

# Demonstration
lassign [poldiv {-42. 0. -12. 1.} {-3. 1. 0. 0.}] Q R
puts [list Q = $Q]
puts [list R = $R]
```



## Ursala

The input is a pair of lists of coefficients in order of increasing degree.
Trailing zeros can be omitted. The output is a pair of lists (q,r), the quotient
and remainder polynomial coefficients. This is a straightforward implementation
of the algorithm in terms of list operations (fold, zip, map, distribute, etc.) instead
of array indexing, hence not unnecessarily verbose.

```ursala
#import std
#import flo

polydiv =

zeroid~-l~~; leql?rlX\~&NlX ^H\(@rNrNSPXlHDlS |\ :/0.) @NlX //=> ?(
   @lrrPX ==!| zipp0.; @x not zeroid+ ==@h->hr ~&t,
   (^lryPX/~&lrrl2C minus^*p/~&rrr times*lrlPD)^/div@bzPrrPlXO ~&,
   @r ^|\~& ~&i&& :/0.)
```

test program:

```Ursala
#cast %eLW

example = polydiv(<-42.,0.,-12.,1.>,<-3.,1.,0.,0.>)
```

output:

```txt
(
   <-2.700000e+01,-9.000000e+00,1.000000e+00>,
   <-1.230000e+02>)
```



## VBA

{{trans|Phix}}
```vb
Option Base 1
Function degree(p As Variant)
    For i = UBound(p) To 1 Step -1
        If p(i) <> 0 Then
            degree = i
            Exit Function
        End If
    Next i
    degree = -1
End Function

Function poly_div(ByVal n As Variant, ByVal d As Variant) As Variant
    If UBound(d) < UBound(n) Then
        ReDim Preserve d(UBound(n))
    End If
    Dim dn As Integer: dn = degree(n)
    Dim dd As Integer: dd = degree(d)
    If dd < 0 Then
        poly_div = CVErr(xlErrDiv0)
        Exit Function
    End If
    Dim quot() As Integer
    ReDim quot(dn)
    Do While dn >= dd
        Dim k As Integer: k = dn - dd
        Dim qk As Integer: qk = n(dn) / d(dd)
        quot(k + 1) = qk
        Dim d2() As Variant
        d2 = d
        ReDim Preserve d2(UBound(d) - k)
        For i = 1 To UBound(d2)
            n(UBound(n) + 1 - i) = n(UBound(n) + 1 - i) - d2(UBound(d2) + 1 - i) * qk
        Next i
        dn = degree(n)
    Loop
    poly_div = Array(quot, n) '-- (n is now the remainder)
End Function

Function poly(si As Variant) As String
'-- display helper
    Dim r As String
    For t = UBound(si) To 1 Step -1
        Dim sit As Integer: sit = si(t)
        If sit <> 0 Then
            If sit = 1 And t > 1 Then
                r = r & IIf(r = "", "", " + ")
            Else
                If sit = -1 And t > 1 Then
                    r = r & IIf(r = "", "-", " - ")
                Else
                    If r <> "" Then
                        r = r & IIf(sit < 0, " - ", " + ")
                        sit = Abs(sit)
                    End If
                    r = r & CStr(sit)
                End If
            End If
            r = r & IIf(t > 1, "x" & IIf(t > 2, t - 1, ""), "")
        End If
    Next t
    If r = "" Then r = "0"
    poly = r
End Function

Function polyn(s As Variant) As String
    Dim t() As String
    ReDim t(2 * UBound(s))
    For i = 1 To 2 * UBound(s) Step 2
        t(i) = poly(s((i + 1) / 2))
    Next i
    t(1) = String$(45 - Len(t(1)) - Len(t(3)), " ") & t(1)
    t(2) = "/"
    t(4) = "="
    t(6) = "rem"
    polyn = Join(t, " ")
End Function

Public Sub main()
    Dim tests(7) As Variant
    tests(1) = Array(Array(-42, 0, -12, 1), Array(-3, 1))
    tests(2) = Array(Array(-3, 1), Array(-42, 0, -12, 1))
    tests(3) = Array(Array(-42, 0, -12, 1), Array(-3, 1, 1))
    tests(4) = Array(Array(2, 3, 1), Array(1, 1))
    tests(5) = Array(Array(3, 5, 6, -4, 1), Array(1, 2, 1))
    tests(6) = Array(Array(3, 0, 7, 0, 0, 0, 0, 0, 3, 0, 0, 1), Array(1, 0, 0, 5, 0, 0, 0, 1))
    tests(7) = Array(Array(-56, 87, -94, -55, 22, -7), Array(2, 0, 1))
    Dim num As Variant, denom As Variant, quot As Variant, rmdr As Variant
    For i = 1 To 7
        num = tests(i)(1)
        denom = tests(i)(2)
        tmp = poly_div(num, denom)
        quot = tmp(1)
        rmdr = tmp(2)
        Debug.Print polyn(Array(num, denom, quot, rmdr))
    Next i
End Sub
```
{{out}}

```txt
                          x3 - 12x2 - 42 / x - 3 = x2 - 9x - 27 rem -123
                          x - 3 / x3 - 12x2 - 42 = 0 rem x - 3
                     x3 - 12x2 - 42 / x2 + x - 3 = x - 13 rem 16x - 81
                             x2 + 3x + 2 / x + 1 = x + 2 rem 0
           x4 - 4x3 + 6x2 + 5x + 3 / x2 + 2x + 1 = x2 - 6x + 17 rem -23x - 14
              x11 + 3x8 + 7x2 + 3 / x7 + 5x3 + 1 = x4 + 3x - 5 rem -16x4 + 25x3 + 7x2 - 3x + 8
   -7x5 + 22x4 - 55x3 - 94x2 + 87x - 56 / x2 + 2 = -7x3 + 22x2 - 41x - 138 rem 169x + 220
```


## zkl


```zkl
fcn polyLongDivision(a,b){  // (a0 + a1x + a2x^2 + a3x^3 ...)
   _assert_(degree(b)>=0,"degree(%s) < 0".fmt(b));
   q:=List.createLong(a.len(),0.0);
   while((ad:=degree(a)) >= (bd:=degree(b))){
      z,d,m := ad-bd, List.createLong(z,0.0).extend(b), a[ad]/b[bd];;
      q[z]=m;
      d,a = d.apply('*(m)), a.zipWith('-,d);
   }
   return(q,a);		// may have trailing zero elements
}
fcn degree(v){  // -1,0,..len(v)-1, -1 if v==0
   v.len() - v.copy().reverse().filter1n('!=(0)) - 1;
}
fcn polyString(terms){ // (a0,a1,a2...)-->"a0 + a1x + a2x^2 ..."
   str:=[0..].zipWith('wrap(n,a){ if(a) "+ %sx^%s ".fmt(a,n) else "" },terms)
   .pump(String)
   .replace("x^0 "," ").replace(" 1x"," x").replace("x^1 ","x ")
   .replace("+ -", "- ");
   if(not str)     return(" ");  // all zeros
   if(str[0]=="+") str[1,*];     // leave leading space
   else            String("-",str[2,*]);
}
```


```zkl
q,r:=polyLongDivision(T(-42.0, 0.0, -12.0, 1.0),T(-3.0, 1.0));
println("Quotient  = ",polyString(q));
println("Remainder = ",polyString(r));
```

{{out}}

```txt

Quotient  = -27 - 9x + x^2
Remainder = -123

```


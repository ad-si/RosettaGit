+++
title = "Numeric error propagation"
description = ""
date = 2019-10-21T22:00:27Z
aliases = []
[extra]
id = 10185
[taxonomies]
categories = []
tags = []
+++

{{task}}

If   '''f''',   '''a''',   and   '''b'''   are values with uncertainties   σ<sub>f</sub>,   σ<sub>a</sub>,   and   σ<sub>b</sub>,   and   '''c'''   is a constant; 

then if   '''f'''   is derived from   '''a''',   '''b''',   and   '''c'''   in the following ways, 

then   σ<sub>f</sub>   can be calculated as follows:

:;Addition/Subtraction
:* If   f = a &plusmn; c,   or   f = c &plusmn; a   then   '''σ<sub>f</sub> = σ<sub>a</sub>'''
:* If   f = a &plusmn; b   then   '''σ<sub>f</sub><sup>2</sup> = σ<sub>a</sub><sup>2</sup> + σ<sub>b</sub><sup>2</sup>'''

:;Multiplication/Division
:* If   f = ca   or   f = ac       then   '''σ<sub>f</sub> = |cσ<sub>a</sub>|'''
:* If   f = ab   or   f = a / b              then   '''σ<sub>f</sub><sup>2</sup> = f<sup>2</sup>( (σ<sub>a</sub> / a)<sup>2</sup> + (σ<sub>b</sub> / b)<sup>2</sup>)'''

:;Exponentiation
:* If   f = a<sup>c</sup>   then   '''σ<sub>f</sub> = |fc(σ<sub>a</sub> / a)|'''


Caution:
::This implementation of error propagation does not address issues of dependent and independent values.   It is assumed that   '''a'''   and   '''b'''   are independent and so the formula for multiplication should not be applied to   '''a*a'''   for example.   See   [[Talk:Numeric_error_propagation|the talk page]]   for some of the implications of this issue.


;Task details:
# Add an uncertain number type to your language that can support addition, subtraction, multiplication, division, and exponentiation between numbers with an associated error term together with 'normal' floating point numbers without an associated error term. 
Implement enough functionality to perform the following calculations.
# Given coordinates and their errors:
x1 = 100 &plusmn; 1.1
y1 =  50 &plusmn; 1.2
x2 = 200 &plusmn; 2.2
y2 = 100 &plusmn; 2.3
 if point p1 is located at (x1, y1) and p2 is at (x2, y2); calculate the distance between the two points using the classic Pythagorean formula: 
 <big><big> d = &radic;<span style="text-decoration:overline">   (x1 - x2)²   +   (y1 - y2)²   </span> </big></big>
# Print and display both   <big> '''d''' </big>   and its error.

 <!-- the superscript  
       2 glyph  [²]  
had to be used instead of the     
       <sup>2</sup>     
notation which causes the  overline  "text-decoration"  to either overlay the superscript or it causes a "break" in the continuous overline part of the radic.    Gerard Schildberger.  --> 

;References:
* [http://casa.colorado.edu/~benderan/teaching/astr3510/stats.pdf A Guide to Error Propagation] B. Keeney, 2005.
* [[wp:Propagation of uncertainty|Propagation of uncertainty]] Wikipedia.


;Related task:
*   [[Quaternion type]]





## Ada


Specification of a generic type Approximation.Number, providing all the operations required to solve the task ... and some more operations, for completeness.


```Ada
generic
   type Real is digits <>;
   with function Sqrt(X: Real) return Real;
   with function "**"(X: Real; Y: Real) return Real;
package Approximation is

   type Number is private;

   -- create an approximation
   function Approx(Value: Real; Sigma: Real) return Number;

   -- unary operations and conversion Real to Number
   function "+"(X: Real) return Number;
   function "-"(X: Real) return Number;
   function "+"(X: Number) return Number;
   function "-"(X: Number) return Number;

   -- addition / subtraction
   function "+"(X: Number; Y: Number) return Number;
   function "-"(X: Number; Y: Number) return Number;

   -- multiplication / division
   function "*"(X: Number; Y: Number) return Number;
   function "/"(X: Number; Y: Number) return Number;

   -- exponentiation
   function "**"(X: Number; Y: Positive) return Number;
   function "**"(X: Number; Y: Real) return Number;

   -- Output to Standard IO (wrapper for Ada.Text_IO and Ada.Text_IO.Float_IO)
   procedure Put_Line(Message: String;
                      Item: Number;
                      Value_Fore: Natural := 7;
                      Sigma_Fore: Natural := 4;
                      Aft:  Natural := 2;
                      Exp:  Natural := 0);
   procedure Put(Item: Number;
                 Value_Fore: Natural := 7;
                 Sigma_Fore: Natural := 3;
                 Aft:  Natural := 2;
                 Exp:  Natural := 0);

private
   type Number is record
      Value: Real;
      Sigma: Real;
   end record;
end Approximation;
```


The implementation:


```Ada
with Ada.Text_IO;

package body Approximation is

   package RIO is new Ada.Text_IO.Float_IO(Real);

   -- create an approximation

   function Approx(Value: Real; Sigma: Real) return Number is
   begin
      return (Value, Sigma);
   end Approx;

   -- unary operations and conversion Real to Number

   function "+"(X: Real) return Number is
   begin
      return Approx(X, 0.0);
   end "+";

   function "-"(X: Real) return Number is
   begin
      return Approx(-X, 0.0);
   end "-";

   function "+"(X: Number) return Number is
   begin
      return X;
   end "+";

   function "-"(X: Number) return Number is
   begin
      return Approx(-X.Value, X.Sigma);
   end "-";

   -- addition / subtraction

   function "+"(X: Number; Y: Number) return Number is
      Z: Number;
   begin
      Z.Value := X.Value + Y.Value;
      Z.Sigma := Sqrt(X.Sigma*X.Sigma + Y.Sigma*Y.Sigma);
      return Z;
   end "+";

   function "-"(X: Number; Y: Number) return Number is
   begin
      return X + (-Y);
   end "-";

   -- multiplication / division

   function "*"(X: Number; Y: Number) return Number is
      Z: Number;
   begin
      Z.Value := X.Value * Y.Value;
      Z.Sigma := Z.Value * Sqrt((X.Sigma/X.Value)**2 + (Y.Sigma/Y.Value)**2);
      return Z;
   end "*";

   function "/"(X: Number; Y: Number) return Number is
      Z: Number;
   begin
      Z.Value := X.Value / Y.Value;
      Z.Sigma := Z.Value * Sqrt((X.Sigma/X.Value)**2 + (Y.Sigma/Y.Value)**2);
      return Z;
   end "/";

   -- exponentiation

   function "**"(X: Number; Y: Positive) return Number is
      Z: Number;
   begin
      Z.Value := X.Value ** Y ;
      Z.Sigma := Z.Value * Real(Y) * (X.Sigma/X.Value);
      if Z.Sigma < 0.0 then
         Z.Sigma := - Z.Sigma;
      end if;
      return Z;
   end "**";

   function "**"(X: Number; Y: Real) return Number is
      Z: Number;
   begin
      Z.Value := X.Value ** Y ;
      Z.Sigma := Z.Value * Y * (X.Sigma/X.Value);
      if Z.Sigma < 0.0 then
         Z.Sigma := - Z.Sigma;
      end if;
      return Z;
   end "**";

   -- Output to Standard IO (wrapper for Ada.Text_IO.Float_IO)

   procedure Put_Line(Message: String;
                      Item: Number;
                      Value_Fore: Natural := 7;
                      Sigma_Fore: Natural := 4;
                      Aft:  Natural := 2;
                      Exp:  Natural := 0) is
   begin
      Ada.Text_IO.Put(Message);
      Put(Item, Value_Fore, Sigma_Fore, Aft, Exp);
      Ada.Text_IO.New_Line;
   end Put_Line;

   procedure Put(Item: Number;
                 Value_Fore: Natural := 7;
                 Sigma_Fore: Natural := 3;
                 Aft:  Natural := 2;
                 Exp:  Natural := 0) is
   begin
      RIO.Put(Item.Value, Value_Fore, Aft, Exp);
      Ada.Text_IO.Put(" (+-");
      RIO.Put(Item.Sigma, Sigma_Fore, Aft, Exp);
      Ada.Text_IO.Put(")");
   end Put;

end Approximation;
```


Instantiating the package with Float operations, to compute the distance:


```Ada
with Approximation, Ada.Numerics.Elementary_Functions;

procedure Test_Approximations is
   package A is new Approximation(Float,
                                  Ada.Numerics.Elementary_Functions.Sqrt,
                                  Ada.Numerics.Elementary_Functions."**");
   use type A.Number;
   X1: A.Number := A.Approx(100.0, 1.1);
   Y1: A.Number := A.Approx( 50.0, 1.2);
   X2: A.Number := A.Approx(200.0, 2.2);
   Y2: A.Number := A.Approx(100.0, 2.3);

begin
   A.Put_Line("Distance:",
              ((X1-X2)**2 + (Y1 - Y2)**2)**0.5,
              Sigma_Fore => 1);
end Test_Approximations;
```


Output:

```txt
Distance:    111.80 (+-2.49)
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
# MODE representing a uncertain number #
MODE UNCERTAIN = STRUCT( REAL v, uncertainty );

# add a costant and an uncertain value #
OP + = ( INT  c, UNCERTAIN u )UNCERTAIN: UNCERTAIN( v OF u + c, uncertainty OF u );
OP + = ( UNCERTAIN u, INT  c )UNCERTAIN: c + u;
OP + = ( REAL c, UNCERTAIN u )UNCERTAIN: UNCERTAIN( v OF u + c, uncertainty OF u );
OP + = ( UNCERTAIN u, REAL c )UNCERTAIN: c + u;
# add two uncertain values #
OP + = ( UNCERTAIN a, b )UNCERTAIN: UNCERTAIN( v OF a + v OF b
                                             , sqrt( ( uncertainty OF a * uncertainty OF a )
                                                   + ( uncertainty OF b * uncertainty OF b )
                                                   )
                                             );

# negate an uncertain value #
OP - = ( UNCERTAIN a )UNCERTAIN: ( - v OF a, uncertainty OF a );

# subtract an uncertain value from a constant #
OP - = ( INT  c, UNCERTAIN u )UNCERTAIN: c + - u;
OP - = ( REAL c, UNCERTAIN u )UNCERTAIN: c + - u;
# subtract a constant from an uncertain value #
OP - = ( UNCERTAIN u, INT  c )UNCERTAIN: u + - c;
OP - = ( UNCERTAIN u, REAL c )UNCERTAIN: u + - c;
# subtract two uncertain values #
OP - = ( UNCERTAIN a, b )UNCERTAIN: a + - b;

# multiply a constant by an uncertain value #
OP * = ( INT  c, UNCERTAIN u )UNCERTAIN: UNCERTAIN( v OF u + c, ABS( c * uncertainty OF u ) );
OP * = ( UNCERTAIN u, INT  c )UNCERTAIN: c * u;
OP * = ( REAL c, UNCERTAIN u )UNCERTAIN: UNCERTAIN( v OF u + c, ABS( c * uncertainty OF u ) );
OP * = ( UNCERTAIN u, REAL c )UNCERTAIN: c * u;
# multiply two uncertain values #
OP * = ( UNCERTAIN a, b )UNCERTAIN:
   BEGIN
       REAL av = v OF a;
       REAL bv = v OF b;
       REAL f  = av * bv;
       UNCERTAIN( f, f * sqrt( ( uncertainty OF a / av ) + ( uncertainty OF b / bv ) ) )
   END # * # ;

# construct the reciprocol of an uncertain value #
OP ONEOVER = ( UNCERTAIN u )UNCERTAIN: ( 1 / v OF u, uncertainty OF u );
# divide a constant by an uncertain value #
OP / = ( INT  c, UNCERTAIN u )UNCERTAIN: c * ONEOVER u;
OP / = ( REAL c, UNCERTAIN u )UNCERTAIN: c * ONEOVER u;
# divide an uncertain value by a constant #
OP / = ( UNCERTAIN u, INT  c )UNCERTAIN: u * ( 1 / c );
OP / = ( UNCERTAIN u, REAL c )UNCERTAIN: u * ( 1 / c );
# divide two uncertain values #
OP / = ( UNCERTAIN a, b )UNCERTAIN: a * ONEOVER b;

# exponentiation #
OP ^ = ( UNCERTAIN u, INT c )UNCERTAIN:
   BEGIN
       REAL f = v OF u ^ c;
       UNCERTAIN( f, ABS ( ( f * c * uncertainty OF u ) / v OF u ) )
   END # ^ # ;
OP ^ = ( UNCERTAIN u, REAL c )UNCERTAIN:
   BEGIN
       REAL f = v OF u ^ c;
       UNCERTAIN( f, ABS ( ( f * c * uncertainty OF u ) / v OF u ) )
   END # ^ # ;

# test the above operatrs by using them to find the pythagorean distance between the two sample points #
UNCERTAIN x1 = UNCERTAIN( 100, 1.1 );
UNCERTAIN y1 = UNCERTAIN(  50, 1.2 );
UNCERTAIN x2 = UNCERTAIN( 200, 2.2 );
UNCERTAIN y2 = UNCERTAIN( 100, 2.3 );

UNCERTAIN d  = ( ( ( x1 - x2 ) ^ 2 ) + ( y1 - y2 ) ^ 2 ) ^ 0.5;

print( ( "distance: ", fixed( v OF d, 0, 2 ), " +/- ", fixed( uncertainty OF d, 0, 2 ), newline ) )
```

{{out}}

```txt

distance: 111.80 +/- 2.49

```



## C

Rewrote code to make it more compact and added a nice formatting function for imprecise values so that they are printed out in a technically correct way i.e. with the symbol '±' . Output pasted after code.

```C

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
 
typedef struct{
    double value;
    double delta;
}imprecise;
 
#define SQR(x) ((x) * (x))
imprecise imprecise_add(imprecise a, imprecise b)
{
    imprecise ret;
    ret.value = a.value + b.value;
    ret.delta = sqrt(SQR(a.delta) + SQR(b.delta));
    return ret;
}
 
imprecise imprecise_mul(imprecise a, imprecise b)
{
    imprecise ret;
    ret.value = a.value * b.value;
    ret.delta = sqrt(SQR(a.value * b.delta) + SQR(b.value * a.delta));
    return ret;
}
 
imprecise imprecise_div(imprecise a, imprecise b)
{
    imprecise ret;
    ret.value = a.value / b.value;
    ret.delta = sqrt(SQR(a.value * b.delta) + SQR(b.value * a.delta)) / SQR(b.value);
    return ret;
}
 
imprecise imprecise_pow(imprecise a, double c)
{
    imprecise ret;
    ret.value = pow(a.value, c);
    ret.delta = fabs(ret.value * c * a.delta / a.value);
    return ret;
}

char* printImprecise(imprecise val)
{
	char principal[30],error[30],*string,sign[2];
	sign[0] = 241;    /* ASCII code for ±, technical notation for denoting errors */
	sign[1] = 00;
	
	sprintf(principal,"%f",val.value);
	sprintf(error,"%f",val.delta);
	
	string = (char*)malloc((strlen(principal)+1+strlen(error)+1)*sizeof(char));
	
	strcpy(string,principal);
	strcat(string,sign);
	strcat(string,error);
	
	return string;
}
 
int main(void) {
    imprecise x1 = {100, 1.1};
    imprecise y1 = {50, 1.2};
    imprecise x2 = {-200, 2.2};
    imprecise y2 = {-100, 2.3};
    imprecise d;
 
    d = imprecise_pow(imprecise_add(imprecise_pow(imprecise_add(x1, x2), 2),imprecise_pow(imprecise_add(y1, y2), 2)), 0.5);
    printf("Distance, d, between the following points :");
    printf("\n( x1, y1) = ( %s, %s)",printImprecise(x1),printImprecise(y1));
    printf("\n( x2, y2) = ( %s, %s)",printImprecise(x2),printImprecise(y2));
    printf("\nis d = %s", printImprecise(d));
    return 0;
}

```



```txt

Distance, d, between the following points :
( x1, y1) = ( 100.000000±1.100000, 50.000000±1.200000)
( x2, y2) = ( -200.000000±2.200000, -100.000000±2.300000)
is d = 111.803399±2.487167

```



## C++

numeric_error.hpp

```cpp
#pragma once

#include <cmath>
#include <string>
#include <sstream>
#include <iomanip>

class Approx {
public:
    Approx(double _v, double _s = 0.0) : v(_v), s(_s) {}

    operator std::string() const {
        std::ostringstream os("");
        os << std::setprecision(15) << v << " ±" << std::setprecision(15) << s << std::ends;
        return os.str();
    }

    Approx operator +(const Approx& a) const { return Approx(v + a.v, sqrt(s * s + a.s * a.s)); }
    Approx operator +(double d) const { return Approx(v + d, s); }
    Approx operator -(const Approx& a) const { return Approx(v - a.v, sqrt(s * s + a.s * a.s)); }
    Approx operator -(double d) const { return Approx(v - d, s); }

    Approx operator *(const Approx& a) const {
        const double t = v * a.v;
        return Approx(v, sqrt(t * t * s * s / (v * v) + a.s * a.s / (a.v * a.v)));
    }

    Approx operator *(double d) const { return Approx(v * d, fabs(d * s)); }

    Approx operator /(const Approx& a) const {
        const double t = v / a.v;
        return Approx(t, sqrt(t * t * s * s / (v * v) + a.s * a.s / (a.v * a.v)));
    }

    Approx operator /(double d) const { return Approx(v / d, fabs(d * s)); }

    Approx pow(double d) const {
        const double t = ::pow(v, d);
        return Approx(t, fabs(t * d * s / v));
    }

private:
    double v, s;
};
```

numeric_error.cpp

```cpp>#include <cstdlib

#include <iostream>
#include "numeric_error.hpp"

int main(const int argc, const char* argv[]) {
    const Approx x1(100, 1.1);
    const Approx x2(50, 1.2);
    const Approx y1(200, 2.2);
    const Approx y2(100, 2.3);
    std::cout << std::string(((x1 - x2).pow(2.) + (y1 - y2).pow(2.)).pow(0.5)) << std::endl; // => 111.803398874989 ±2.938366893361

	return EXIT_SUCCESS;
}
```

{{out}}

```txt
111.803398874989 ±2.938366893361
```



## Common Lisp


```lisp
(defstruct uncertain-number
  (value 0 :type number)
  (uncertainty 0 :type number))

(defmethod print-object ((n uncertain-number) stream)
  (format stream "~,2F ± ~,2F" (uncertain-number-value n) (uncertain-number-uncertainty n)))

(defun ~+ (n1 n2)
  (let* ((value1 (uncertain-number-value n1))
         (value2 (uncertain-number-value n2))
         (uncertainty1 (uncertain-number-uncertainty n1))
         (uncertainty2 (uncertain-number-uncertainty n2))
         (value (+ value1 value2))
         (uncertainty (sqrt (+ (* uncertainty1 uncertainty1)
                               (* uncertainty2 uncertainty2)))))
    (make-uncertain-number :value value :uncertainty uncertainty)))

(defun negate (n)
  (make-uncertain-number :value (- (uncertain-number-value n))
                         :uncertainty (uncertain-number-uncertainty n)))

(defun ~- (n1 n2)
  (~+ n1 (negate n2)))

(defun ~* (n1 n2)
  (let* ((value1 (uncertain-number-value n1))
         (value2 (uncertain-number-value n2))
         (uncertainty-ratio-1 (/ (uncertain-number-uncertainty n1) value1))
         (uncertainty-ratio-2 (/ (uncertain-number-uncertainty n2) value2))
         (value (* value1 value2))
         (uncertainty (sqrt (* value
                               value
                               (+ (* uncertainty-ratio-1 uncertainty-ratio-1)
                                  (* uncertainty-ratio-2 uncertainty-ratio-2))))))
    (make-uncertain-number :value value :uncertainty uncertainty)))

(defun inverse (n)
  (make-uncertain-number :value (/ (uncertain-number-value n))
                         :uncertainty (uncertain-number-uncertainty n)))

(defun ~/ (n1 n2)
  (~* n1 (inverse n2)))

(defun ~expt (base exp)
  (let* ((base-value (uncertain-number-value base))
         (uncertainty-ratio (/ (uncertain-number-uncertainty base) base-value))
         (value (expt base-value exp))
         (uncertainty (abs (* value exp uncertainty-ratio))))
    (make-uncertain-number :value value :uncertainty uncertainty)))

(defun solve ()
  (let* ((x1 (make-uncertain-number :value 100 :uncertainty 1.1))
         (y1 (make-uncertain-number :value  50 :uncertainty 1.2))
         (x2 (make-uncertain-number :value 200 :uncertainty 2.2))
         (y2 (make-uncertain-number :value 100 :uncertainty 2.3))
         (d  (~expt (~+ (~expt (~- x1 x2) 2) (~expt (~- y1 y2) 2))
                    1/2)))
    (format t "d = ~A~%" d)))
```

{{out}}

```txt
d = 111.80 ± 2.49
```



## D


```d
import std.stdio, std.math, std.string, std.typecons, std.traits;

const struct Imprecise {
    private const double value, delta;

    this(in double v, in double d) pure nothrow {
        this.value = v;
        this.delta = abs(d);
    }

    enum IsImprecise(T) = is(Unqual!T == Unqual!(typeof(this)));

    I reciprocal() const pure nothrow {
        return I(1.0 / value, delta / (value ^^ 2));
    }

    string toString() const {
        return format("I(value=%g, delta=%g)", value, delta);
    }

    I opUnary(string op:"-")() const pure nothrow {
        return I(-this.value, this.delta);
    }

    I opBinary(string op:"+", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        static if (IsImprecise!T)
            return I(this.value + other.value,
                     (this.delta ^^ 2 + other.delta ^^ 2) ^^ 0.5);
        else
            return I(this.value + other, this.delta);
    }

    I opBinaryRight(string op:"+", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return I(this.value + other, this.delta);
    }

    I opBinary(string op:"-", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        return this + (-other);
    }

    I opBinaryRight(string op:"-", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return this - other;
    }

    I opBinary(string op:"*", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        static if (IsImprecise!T) {
            auto f = this.value * other.value;
            return I(f, f * ((delta / value) ^^ 2 +
                     (other.delta / other.value) ^^ 2) ^^ 0.5);
        } else
            return I(this.value * other, this.delta * other);
    }

    I opBinaryRight(string op:"*", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return this * other;
    }

    I opBinary(string op:"/", T)(in T other) const pure nothrow
    if (isNumeric!T || IsImprecise!T) {
        static if (IsImprecise!T)
            return this * other.reciprocal();
        else
            return I(this.value / other, this.delta / other);
    }

    I opBinaryRight(string op:"/", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        return this / other;
    }

    I opBinary(string op:"^^", T)(in T other) const pure nothrow
    if (isNumeric!T) {
        auto f = this.value ^^ other;
        return I(f, f * other * (this.delta / this.value));
    }
}

alias I = Imprecise;

auto distance(T1, T2)(in T1 p1, in T2 p2) pure nothrow {
    return ((p1[0] - p2[0]) ^^ 2 + (p1[1] - p2[1]) ^^ 2) ^^ 0.5;
}

void main() {
    immutable x1 = I(100, 1.1);
    immutable x2 = I(200, 2.2);
    immutable y1 = I( 50, 1.2);
    immutable y2 = I(100, 2.3);

    immutable p1 = tuple(x1, y1);
    immutable p2 = tuple(x2, y2);
    writefln("Point p1: (%s, %s)", p1[0], p1[1]);
    writefln("Point p2: (%s, %s)", p2[0], p2[1]);
    writeln("Distance(p1, p2): ", distance(p1, p2));
}
```

{{out}}

```txt
Point p1: (I(value=100, delta=1.1), I(value=50, delta=1.2))
Point p2: (I(value=200, delta=2.2), I(value=100, delta=2.3))
Distance(p1, p2): I(value=111.803, delta=2.48717)
```


=={{header|F_Sharp|F#}}==

```fsharp
let sqr (x : float) = x * x
let abs (x : float)  = System.Math.Abs x
let pow = System.Math.Pow

type Approx (value : float, sigma : float) =
    member this.value = value
    member this.sigma = sigma

    static member (~-) (x : Approx) = Approx (- x.value, x.sigma)
    static member (%+) (x: Approx, y : float) = Approx (x.value + y, x.sigma)
    static member (%+) (y : float, x : Approx) = x %+ y
    static member (%+) (x : Approx, y : Approx) =
        Approx (x.value + y.value, sqrt((sqr x.sigma)+(sqr y.sigma)))
    static member (%-) (x: Approx, y : float) = Approx (x.value - y, x.sigma)
    static member (%-) (y : float, x : Approx) = (-x) %+ y
    static member (%-) (x : Approx, y : Approx) = x %+ (-y)
    static member (%*) (x : Approx, y : float) = Approx (y * x.value, abs(y * x.sigma))
    static member (%*) (y : float, x : Approx) = x %* y
    static member (%*) (x : Approx, y : Approx) =
        let v = x.value * y.value
        Approx (v, v * sqrt(sqr(x.sigma/x.value))+sqr(y.sigma/y.value))
    static member (%/) (x : Approx, y : Approx) =
        Approx (x.value / y.value, sqrt(sqr(x.sigma/x.value))+sqr(y.sigma/y.value))
    static member (%^) (x : Approx, y : float) =
        if y < 0. then failwith ("Cannot raise the power with a negative number " + y.ToString()) 
        let v = pow(x.value,y)
        Approx (v, abs(v * y * x.sigma / x.value))

    override this.ToString() = sprintf "%.2f ±%.2f" value sigma

[<EntryPoint>]
let main argv =
    let x1 = Approx (100., 1.1)
    let y1 = Approx (50., 1.2)
    let x2 = Approx (200., 2.2)
    let y2 = Approx (100., 2.3)

    printfn "Distance: %A" ((((x1 %- x2) %^ 2.) %+ ((y1 %- y2) %^ 2.)) %^ 0.5)
    0
```

{{out}}

```txt
Distance: 111.80 ±2.49
```



## Fortran


### Direct calculation

Following the propagation of error estimates through a computation is a nightmare of convoluted formulae wherein mistakes are easily made. The basic method is to derive the formulae according to the standard rules while carrying forward the calculation by hand with the utmost caution. A computer can perform the calculations, but the real problem is in ensuring that it performs the correct calculations, not some misbegotten confusion... So, rather than attempt to "optimise" the calculation, the objective is to reduce brain strain by producing code whose ''checkability'' is optimised instead, somewhat as follows: 
```Fortran
      PROGRAM CALCULATE	!A distance, with error propagation.
      REAL X1, Y1, X2, Y2	!The co-ordinates.
      REAL X1E,Y1E,X2E,Y2E	!Their standard deviation.
      DATA X1, Y1 ,X2, Y2 /100., 50., 200.,100./	!Specified
      DATA X1E,Y1E,X2E,Y2E/  1.1, 1.2,  2.2, 2.3/	!Values.
      REAL DX,DY,D2,D,DXE,DYE,E	!Assistants.
      CHARACTER*1 C			!I'm stuck with code page 437 instead of 850.
      PARAMETER (C = CHAR(241))		!Thus ± does not yield this glyph on a "console" screen. CHAR(241) does.
      REAL SD	!This is an arithmetic statement function.
      SD(X,P,S) = P*ABS(X)**(P - 1)*S	!SD for X**P where SD of X is S
      WRITE (6,1) X1,C,X1E,Y1,C,Y1E,	!Reveal the points
     1            X2,C,X2E,Y2,C,Y2E	!Though one could have used an array...
    1 FORMAT ("Euclidean distance between two points:"/	!A heading.
     1 ("(",F5.1,A1,F3.1,",",F5.1,A1,F3.1,")"))		!Thus, One point per line.
      DX = (X1 - X2)			!X difference.
      DXE = SQRT(X1E**2 + X2E**2)	!SD for DX, a simple difference.
      DY = (Y1 - Y2)			!Y difference.
      DYE = SQRT(Y1E**2 + Y2E**2)	!SD for DY, (Y1 - Y2)
      D2 = DX**2 + DY**2		!The distance, squared.
      DXE = SD(DX,2,DXE)		!SD for DX**2
      DYE = SD(DY,2,DYE)		!SD for DY**2
      E = SQRT(DXE**2 + DYE**2)		!SD for their sum
      D = SQRT(D2)			!The distance!
      E = SD(D2,0.5,E)			!SD after the SQRT.
      WRITE (6,2) D,C,E			!Ahh, the relief.
    2 FORMAT ("Distance",F6.1,A1,F4.2)	!Sizes to fit the example.
      END	!Enough.
```

This is old-style Fortran, except for the CHARACTER variable caused by problems with character codes and their screen glyphs. As can be seen, the formulae invite mistakes which is why there is no attempt to produce a single arithmetic expression for the result and its error estimate. Further, rather than attempt to emplace appropriate instances of the formula for a value raised to some power (squaring, and square root), risking all manner of misthinks, a function to do so is prepared, here using Fortran's "arithmetic statement function" protocol, expressly intended for such situations. And the results are...

```txt

Euclidean distance between two points:
(100.0±1.1, 50.0±1.2)
(200.0±2.2,100.0±2.3)
Distance 111.8±2.49

```

All outputs are formatted for the specific values of the test data so as to avoid excessive spacing. Since the given errors are of the order of one, showing more than one decimal digit in the result would be silly. Similarly, standard precision suffices though for more elaborate calculations, double precision could well be preferable for the working out.

In the days of batch jobs with only the fortunate obtaining more than one run a day, code such as the above would be worth the effort only if many sets of similar data were to be processed.


### More general

Rather than agonise over devising adjoint formulae for the error propagation through some calculation, one can perform the desired calculation via routines that will carry along the error term, as follows:
```Fortran
      MODULE ERRORFLOW	!Calculate with an error estimate tagging along.
       INTEGER VSP,VMAX		!Do so with an arithmetic stack.
       PARAMETER (VMAX = 28)	!Surely sufficient.
       REAL STACKV(VMAX)	!Holds the values.
       REAL STACKE(VMAX)	!And the corresponding error estimate.
       INTEGER VOUT		!Output file.
       LOGICAL VTRACE		!Perhaps progress is to be followed in detail.
       DATA VSP,VOUT,VTRACE/0,0,.FALSE./	!Start with nothing.
       CHARACTER*1 PM			!I'm stuck with code page 437 instead of 850.
       PARAMETER (PM = CHAR(241))	!Thus ± does not yield this glyph on a "console" screen. CHAR(241) does.
       CONTAINS		!The servants.
        SUBROUTINE VINIT(OUT)	!Get ready.
         INTEGER OUT		!Unit number for output.
          VSP = 0		!My stack is empty.
          VOUT = OUT		!Save this rather than have extra parameters.
          VTRACE = VOUT .GT. 0	!By implication.
        END SUBROUTINE VINIT	!Ready.
        SUBROUTINE VSHOW(WOT)	!Show the topmost element.
         CHARACTER*(*) WOT	!The caller identifies itself.
          IF (VSP.LE.0) THEN	!Just in case
            WRITE (VOUT,1) "Empty",VSP	!My stack may be empty.
           ELSE			!But normally, it is not.
            IF (STACKV(VSP).EQ.0) THEN	!But it might have a zero value!
              WRITE (VOUT,1) WOT,VSP,STACKV(VSP),PM,STACKE(VSP)		!Alas. No percentage, then.
    1         FORMAT (A8,": Vstack(",I2,") =",F8.1,A1,F6.2,F9.1,"%")	!Suits the example.
             ELSE		!Avoiding a divide-by-zero is polite.
              WRITE (VOUT,1) WOT,VSP,STACKV(VSP),PM,STACKE(VSP),	!Possibly, a surprise, still.
     1         STACKE(VSP)/STACKV(VSP)*100	!The relative error may well be interesting.
            END IF		!The pearls have been cast.
          END IF		!So much for protection.
        END SUBROUTINE VSHOW	!Could reveal all the stack...

        SUBROUTINE VLOAD(V,E)	!Load the stack.
         REAL V,E		!The value and its error.
          IF (VSP.GE.VMAX) STOP "VLOAD: overflow!"	!Oh dear.
          VSP = VSP + 1		!Up one.
          STACKV(VSP) = V	!Place the value.
          STACKE(VSP) = E	!And the error.
          IF (VTRACE) CALL VSHOW("vLoad")
        END SUBROUTINE VLOAD	!That was easy!

        SUBROUTINE VADD		!Add the top two elements.
          IF (VSP.LE.1) STOP "VADD: underflow!"	!Maybe not.
          STACKV(VSP - 1) = STACKV(VSP - 1) + STACKV(VSP)	!Do the deed.
          STACKE(VSP - 1) = SQRT(STACKE(VSP - 1)**2 + STACKE(VSP)**2)	!The errors follow.
          VSP = VSP - 1			!Two values have become one.
          IF (VTRACE) CALL VSHOW("vAdd")!The result.
        END SUBROUTINE VADD	!The variance of the sum is the sum of the variances.

        SUBROUTINE VSUB		!Subtract the topmost element from the one below.
          IF (VSP.LE.1) STOP "VSUB: underflow!"	!Perhaps not.
          STACKV(VSP - 1) = STACKV(VSP - 1) - STACKV(VSP)	!The topmost was the second loaded.
          STACKE(VSP - 1) = SQRT(STACKE(VSP - 1)**2 + STACKE(VSP)**2)	!Add the variances also.
          VSP = VSP - 1			!Two values have become one.
          IF (VTRACE) CALL VSHOW("vSub")!The result.
        END SUBROUTINE VSUB	!Could alternatively play with the signs and add...

        SUBROUTINE VMUL		!Multiply the top two elements.
         REAL R1,R2		!Use relative errors in place of plain SD.
          IF (VSP.LE.1) STOP "VMUL: underflow!"	!Perhaps not.
          R1 = STACKE(VSP - 1)/STACKV(VSP - 1)	!The relative errors for multiply
          R2 = STACKE(VSP)    /STACKV(VSP)	!Are treated as are variances in addition.
          STACKV(VSP - 1) = STACKV(VSP - 1)*STACKV(VSP)	!Perform the multiply.
          VSP = VSP - 1					!Unstack, but not quite finished.
          STACKE(VSP) = SQRT((R1**2 + R2**2)*STACKV(VSP)**2)	![SD/xy]² =  [SD/x]² + [SD/y]²
          IF (VTRACE) CALL VSHOW("vMul")			!Thus SD² = [[SD/x]² + [SD/y]²]xy²
        END SUBROUTINE VMUL	!The square means that the error's sign is not altered.

        SUBROUTINE VDIV		!Divide the penultimate element by the top elements.
         REAL R1,R2		!Use relative errors in place of plain SD.
          IF (VSP.LE.1) STOP "VDIV: underflow!"	!Perhaps not.
          R1 = STACKE(VSP - 1)/STACKV(VSP - 1)	!The relative errors for divide
          R2 = STACKE(VSP)    /STACKV(VSP)	!Are treated as are variances in subtraction.
          STACKV(VSP - 1) = STACKV(VSP - 1)/STACKV(VSP)	!Perform the divide.
          VSP = VSP - 1					!X/Y is Load X, Load Y, Divide; Y is topmost.
          STACKE(VSP) = SQRT((R1**2 + R2**2)*STACKV(VSP)**2)	![SD/(x/y)]² =  [SD/x]² + [SD/y]²
          IF (VTRACE) CALL VSHOW("vDiv")			!Thus SD² = [[SD/x]² + [SD/y]²](x/y)²
        END SUBROUTINE VDIV	!Worry over y ± SD spanning zero...

        SUBROUTINE VSQRT	!Now for some fun with the topmost element.
          IF (VSP.LE.0) STOP "VSQRT: underflow!"	!Maybe not.
          STACKV(VSP) = SQRT(STACKV(VSP))		!Negative? Let the system complain.
          STACKE(VSP) = 0.5/STACKV(VSP)*STACKE(VSP)	!F(x ± s) = F(x) ± F'(x).s
          IF (VTRACE) CALL VSHOW("vSqrt")		!Here, F' can't be negative.
        END SUBROUTINE VSQRT	!No change to the pointer.
        SUBROUTINE VSQUARE	!Another raise-to-a-power.
          STACKE(VSP) = 2*ABS(STACKV(VSP))*STACKE(VSP)	!The error's sign is not to be messed with.
          STACKV(VSP) = STACKV(VSP)**2		!This will never be negative.
          IF (VTRACE) CALL VSHOW("vSquare")	!Keep away from zero though.
        END SUBROUTINE VSQUARE	!Same formula as VSQRT, just a different power.
        SUBROUTINE VPOW(P)	!Now for the more general.
         INTEGER P		!Though only integer powers for this routine, so no EXP(P*LN(x)).
          IF (VSP.LE.0) STOP "VPOW: underflow!"	!Perhaps not.
          IF (P.EQ.0)   STOP "VPOW: zero power!"	!No sense in this power!
          STACKE(VSP) = P*ABS(STACKV(VSP))**(P - 1)*STACKE(VSP)	!Negative values a worry.
          STACKV(VSP) = ABS(STACKV(VSP))**P			!I only want the magnitude.
          IF (VTRACE) CALL VSHOW("vPower")	!So, what happened?
        END SUBROUTINE VPOW	!Powers with fractional parts are troublesome.
      END MODULE ERRORFLOW	!That will do for the test problem.

      PROGRAM CALCULATE	!A distance, with error propagation.
      USE ERRORFLOW	!For the details.
      REAL X1, Y1, X2, Y2	!The co-ordinates.
      REAL X1E,Y1E,X2E,Y2E	!Their standard deviation.
      DATA X1, Y1 ,X2, Y2 /100., 50., 200.,100./	!Specified
      DATA X1E,Y1E,X2E,Y2E/  1.1, 1.2,  2.2, 2.3/	!Values.

      WRITE (6,1) X1,PM,X1E,Y1,PM,Y1E,	!Reveal the points
     1            X2,PM,X2E,Y2,PM,Y2E	!Though one could have used an array...
    1 FORMAT ("Euclidean distance between two points:"/	!A heading.
     1 ("(",F5.1,A1,F3.1,",",F5.1,A1,F3.1,")"))		!Thus, One point per line.

Calculate SQRT[(X2 - X1)**2 + (Y2 - Y1)**2]
      CALL VINIT(6)		!Start my arithmetic.
      CALL VLOAD(X2,X2E)
      CALL VLOAD(X1,X1E)
      CALL VSUB			!(X2 - X1)
      CALL VSQUARE		!(X2 - X1)**2
      CALL VLOAD(Y2,Y2E)
      CALL VLOAD(Y1,Y1E)
      CALL VSUB			!Y2 - Y1)
      CALL VSQUARE		!Y2 - Y1)**2
      CALL VADD			!(X2 - X1)**2 + (Y2 - Y1)**2
      CALL VSQRT		!SQRT((X2 - X1)**2 + (Y2 - Y1)**2)
      WRITE (6,2) STACKV(1),PM,STACKE(1)	!Ahh, the relief.
    2 FORMAT ("Distance",F6.1,A1,F4.2)	!Sizes to fit the example.
      END	!Enough.
```

This is closer to the idea of extending the language to supply additional facilities. At the cost of hand-compiling the arithmetic expression into a sequence of pseudo-machine code subroutines, it is apparent that the mind-tangling associated error formulae need no longer be worried over. The various arithmetic subroutines have to be coded correctly with careful attention to the V or E statements in fact being for the V and E terms (cut&paste followed by inadequate adjustment the culprit here: such mistakes are less likely when using a card punch because copying is more troublesome), but this is a straightforward matter of checking. And indeed the VPOW(2) routine has the same effect as VSQUARE. Output:

```txt

Euclidean distance between two points:
(100.0±1.1, 50.0±1.2)
(200.0±2.2,100.0±2.3)
   vLoad: Vstack( 1) =   200.0±  2.20      1.1%
   vLoad: Vstack( 2) =   100.0±  1.10      1.1%
    vSub: Vstack( 1) =   100.0±  2.46      2.5%
 vSquare: Vstack( 1) = 10000.0±491.93      4.9%
   vLoad: Vstack( 2) =   100.0±  2.30      2.3%
   vLoad: Vstack( 3) =    50.0±  1.20      2.4%
    vSub: Vstack( 2) =    50.0±  2.59      5.2%
 vSquare: Vstack( 2) =  2500.0±259.42     10.4%
    vAdd: Vstack( 1) = 12500.0±556.15      4.4%
   vSqrt: Vstack( 1) =   111.8±  2.49      2.2%
Distance 111.8±2.49
```
 

Although the example source file uses the F90 MODULE protocol, this is merely for convenience. Otherwise, there would be the same collection of routines, all sharing a COMMON work area. It would be easy enough to prepare an interactive calculator using this scheme so that different calculations (and data) could be more easily experimented with. Inspection might suggest a return to the laboratory in order to measure some factor with greater precision because its error proves to be the largest contributor to the spread in the result.

The trace as the calculation progresses shows a disconcerting surge in the intermediate values of the error estimate. One ought not "compute from the hip" and not worry over intermediate results that are not shown! More generally, the progression of error should be watched carefully lest assumptions prove invalid. For instance, ''at every step'' along the way, x → F(x),  that x ± e is a ''small'' span, within which F(x) and F'(x) do not change greatly, still less pass a discontinuity. Seemingly helpful events such as F'(x) = 0 should be thought about... An alternative approach to F(x ± e) = F(x) ± |F'(x)|.e is to compute F(x - e) and F(x + e) as well as F(x). Some systems offer "interval arithmetic" that might assist with such a procedure, but they are not widely available.

===Fortran 90 ''et seq''.===
A latter-day expansion of Fortran makes it possible to define a compound entity such as a value and its associated error, for instance,
```Fortran
      TYPE DATUM
       REAL VALUE
       REAL SD
      END TYPE DATUM
      TYPE POINT
       TYPE(DATUM) X
       TYPE(DATUM) Y
      END TYPE POINT
      TYPE(POINT) P1,P2

```

Whereupon, instead of a swarm of separate variables named according to some scheme, you can have a collection of variables with subcomponents named systematically. Further, via a great deal of syntax one can devise functions dealing with those compound types and moreover, prepare procedures that will perform operations such as addition and subtraction, etc. and merge these with the ordinary usages of addition, etc. of ordinary variables. One would then write the formula to be calculated, and it would all just happen. This has been done for arithmetic with rational numbers, in [[Arithmetic/Rational#Fortran]] for example. 

But with the details of the calculation's progress out of sight. This is probably not a good idea.


## Go

Variance from task requirements is that the following does not "extend the language."  It simply defines a type with associated functions and methods as required to solve the remainder of the task.

```go
package main

import (
    "fmt"
    "math"
)

// "uncertain number type"
// a little optimization is to represent the error term with its square.
// this saves some taking of square roots in various places.
type unc struct {
    n float64 // the number
    s float64 // *square* of one sigma error term
}

// constructor, nice to have so it can handle squaring of error term
func newUnc(n, s float64) *unc {
    return &unc{n, s * s}
}

// error term accessor method, nice to have so it can handle recovering
// (non-squared) error term from internal (squared) representation
func (z *unc) errorTerm() float64 {
    return math.Sqrt(z.s)
}

// Arithmetic methods are modeled on the Go big number package.
// The basic scheme is to pass all operands as method arguments, compute
// the result into the method receiver, and then return the receiver as
// the result of the method.  This has an advantage of letting the programer
// determine allocation and use of temporary objects, reducing garbage;
// and has the convenience and efficiency of allowing operations to be chained.

// addition/subtraction
func (z *unc) addC(a *unc, c float64) *unc {
    *z = *a
    z.n += c
    return z
}

func (z *unc) subC(a *unc, c float64) *unc {
    *z = *a
    z.n -= c
    return z
}

func (z *unc) addU(a, b *unc) *unc {
    z.n = a.n + b.n
    z.s = a.s + b.s
    return z
}
func (z *unc) subU(a, b *unc) *unc {
    z.n = a.n - b.n
    z.s = a.s + b.s
    return z
}

// multiplication/division
func (z *unc) mulC(a *unc, c float64) *unc {
    z.n = a.n * c
    z.s = a.s * c * c
    return z
}

func (z *unc) divC(a *unc, c float64) *unc {
    z.n = a.n / c
    z.s = a.s / (c * c)
    return z
}

func (z *unc) mulU(a, b *unc) *unc {
    prod := a.n * b.n
    z.n, z.s = prod, prod*prod*(a.s/(a.n*a.n)+b.s/(b.n*b.n))
    return z
}

func (z *unc) divU(a, b *unc) *unc {
    quot := a.n / b.n
    z.n, z.s = quot, quot*quot*(a.s/(a.n*a.n)+b.s/(b.n*b.n))
    return z
}

// exponentiation
func (z *unc) expC(a *unc, c float64) *unc {
    f := math.Pow(a.n, c)
    g := f * c / a.n
    z.n = f
    z.s = a.s * g * g
    return z
}

func main() {
    x1 := newUnc(100, 1.1)
    x2 := newUnc(200, 2.2)
    y1 := newUnc(50, 1.2)
    y2 := newUnc(100, 2.3)
    var d, d2 unc
    d.expC(d.addU(d.expC(d.subU(x1, x2), 2), d2.expC(d2.subU(y1, y2), 2)), .5)
    fmt.Println("d:    ", d.n)
    fmt.Println("error:", d.errorTerm())
}
```

Output:

```txt

d:     111.80339887498948
error: 2.487167063146342

```



## Haskell


```haskell
data Error a = Error {value :: a, uncertainty :: a} deriving (Eq, Show)

instance (Floating a) => Num (Error a) where
	Error a ua + Error b ub = Error (a + b) (sqrt (ua ^ 2 + ub ^ 2))
	negate (Error a ua) = Error (negate a) ua
	Error a ua * Error b ub = Error (a * b) (abs (a * b * sqrt ((ua / a) ^ 2 + (ub / b) ^ 2))) -- I've factored out the f^2 from the square root
	fromInteger a = Error (fromInteger a) 0

instance (Floating a) => Fractional (Error a) where
	fromRational a = Error (fromRational a) 0
	Error a ua / Error b ub = Error (a / b) (abs (a / b * sqrt ((ua / a) ^ 2 + (ub / b) ^ 2))) -- I've factored out the f^2 from the square root

instance (Floating a) => Floating (Error a) where
	Error a ua ** Error c 0 = Error (a ** c) (abs (ua * c * a**c / a))

main = print (sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)) where -- using (^) for exponentiation would calculate a*a, which the problem specifically said was calculated wrong
	x1 = Error 100 1.1
	y1 = Error 50 1.2
	x2 = Error 200 2.2
	y2 = Error 100 2.3

```

{{out}}

```txt
Error {value = 111.80339887498948, uncertainty = 2.4871670631463423}
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both languages.


```unicon
record num(val,err)

procedure main(a)
    x1 := num(100.0, 1.1)
    y1 := num(50.0,  1.2)
    x2 := num(200.0, 2.2)
    y2 := num(100.0, 2.3)
    d := pow(add(pow(sub(x1,x2),2),pow(sub(y1,y2),2)),0.5)
    write("d = [",d.val,", ",d.err,"]")
end

procedure add(a,b)
    return (numeric(a)+numeric(b)) |
           num(numeric(a)+b.val, b.err) |
           num(a.val+numeric(b), a.err) |
           num(a.val+b.val, (a.err^2 + b.err^2) ^ 0.5)
end

procedure sub(a,b)
    return (numeric(a)-numeric(b)) |
           num(numeric(a)-b.val, b.err) |
           num(a.val-numeric(b), a.err) |
           num(a.val-b.val, (a.err^2 + b.err^2) ^ 0.5)
end

procedure mul(a,b)
    return (numeric(a)*numeric(b)) |
           num(numeric(a)*b.val, abs(a*b.err)) |
           num(a.val*numeric(b), abs(b*a.err)) |
           num(f := a.val*b.val, ((f^2*((a.err/a.val)^2+(b.err/b.val)^2))^0.5))
end

procedure div(a,b)
    return (numeric(a)/numeric(b)) |
           num(numeric(a)/b.val, abs(a*b.err)) |
           num(a.val/numeric(b), abs(b*a.err)) |
           num(f := a.val/b.val, ((f^2*((a.err/a.val)^2+(b.err/b.val)^2))^0.5))
end

procedure pow(a,b)
    return (numeric(a)^numeric(b)) |
           num(f := a.val^numeric(b), abs(f*b*(a.err/a.val)))
end
```


The output is:


```txt

->nep
d = [111.8033988749895, 2.487167063146342]
->

```



## J


J's built in operators cannot be overloaded to deal with user defined types.   So we will have to create new operators.  Here's one approach, which is sufficient for this example:

First, we will need some utilities.  <code>num</code> will extract the number part of a number.  <code>unc</code> will extract the uncertainty part of a number, and will also be used to associate uncertainty with a number.  <code>dist</code> will compute the distance between two numbers (which is needed for multiplicative uncertainty).


```j
num=: {."1
unc=: {:@}."1 : ,.
dist=: +/&.:*:
```


Note that if a number has no uncertainty assigned to it, we assume the uncertainty is zero.

Jumping into the example values, for illustration purposes:


```j
x1=: 100 unc 1.1
y1=:  50 unc 1.2

x2=: 200 unc 2.2
y2=: 100 unc 2.3
```


Above, we see <code>unc</code> being used to associate a number with its uncertainty.  Here's how to take them apart again:


```j
   num x1
100
   unc x1
1.1
```


Note that these operations "do the right thing" for normal numbers:


```j
   num 100
100
   unc 100
0
```


And, a quick illustration of the distance function:
<lang>   3 dist 4
5
```


Next, we need to define our arithmetic operations:


```j
add=: +&num unc dist&unc
sub=: -&num unc dist&unc
mul=: *&num unc |@(*&num * dist&(unc%num))
div=: %&num unc |@(%&num * dist&(unc%num))
exp=: ^&num unc |@(^&num * dist&(unc%num))
```


Finally, our required example:


```j
   exp&0.5 (x1 sub x2) add&(exp&2) y1 sub y2
111.803 2.48717
```



## Java


```java
public class Approx {
    private double value;
    private double error;
    
    public Approx(){this.value = this.error = 0;}
    
    public Approx(Approx b){
        this.value = b.value;
        this.error = b.error;
    }
    
    public Approx(double value, double error){
        this.value = value;
        this.error = error;
    }
    
    public Approx add(Approx b){
        value+= b.value;
        error = Math.sqrt(error * error + b.error * b.error);
        return this;
    }
    
    public Approx add(double b){
        value+= b;
        return this;
    }
    
    public Approx sub(Approx b){
        value-= b.value;
        error = Math.sqrt(error * error + b.error * b.error);
        return this;
    }
    
    public Approx sub(double b){
        value-= b;
        return this;
    }
    
    public Approx mult(Approx b){
        double oldVal = value;
        value*= b.value;
        error = Math.sqrt(value * value * (error*error) / (oldVal*oldVal) +
                                  (b.error*b.error) / (b.value*b.value));
        return this;
    }

    public Approx mult(double b){
        value*= b;
        error = Math.abs(b * error);
        return this;
    }
    
    public Approx div(Approx b){
        double oldVal = value;
        value/= b.value;
        error = Math.sqrt(value * value * (error*error) / (oldVal*oldVal) +
                                  (b.error*b.error) / (b.value*b.value));
        return this;
    }

    public Approx div(double b){
        value/= b;
        error = Math.abs(b * error);
        return this;
    }
    
    public Approx pow(double b){
        double oldVal = value;
        value = Math.pow(value, b);
        error = Math.abs(value * b * (error / oldVal));
        return this;
    }
    
    @Override
    public String toString(){return value+"±"+error;}
    
    public static void main(String[] args){
        Approx x1 = new Approx(100, 1.1);
        Approx y1 = new Approx(50, 1.2);
        Approx x2 = new Approx(200, 2.2);
        Approx y2 = new Approx(100, 2.3);
        
        x1.sub(x2).pow(2).add(y1.sub(y2).pow(2)).pow(0.5);
        
        System.out.println(x1);
    }
}
```

{{out}}

```txt
111.80339887498948±2.4871670631463423
```



## Julia


```julia

module NumericError

import Base: convert, promote_rule, +, -, *, /, ^

export Measure

type Measure <: Number
  x::Float64
  σ::Float64
  Measure(x::Real) = new(Float64(x), 0)
  Measure(x::Real, σ::Real) = new(Float64(x), σ)
end

Base.show(io::IO, x::Measure) = print(io, string(x.x, " ± ", x.σ))

Base.convert(::Type{Measure}, x::Real) = Measure(Float64(x), 0.0)
Base.promote_rule(::Type{Float64}, ::Type{Measure}) = Measure
Base.promote_rule(::Type{Int64}, ::Type{Measure}) = Measure

+(a::Measure, b::Measure) = Measure(a.x + b.x, sqrt(a.σ ^ 2 + b.σ ^ 2))
-(a::Measure, b::Measure) = Measure(a.x - b.x, sqrt(a.σ ^ 2 + b.σ ^ 2))
-(a::Measure) = Measure(-a.x, a.σ)

*(a::Measure, b::Measure) = begin
  x = a.x * b.x
  σ = sqrt(x ^ 2 * ((a.σ / a.x) ^ 2 + (b.σ / b.x) ^ 2))
  Measure(x, σ)
end
/(a::Measure, b::Measure) = begin
  x = a.x / b.x
  σ = sqrt(x ^ 2 * ((a.σ / a.x) ^ 2 + (b.σ / b.x) ^ 2))
  Measure(x, σ)
end

^(a::Measure, b::Float64) = begin
  x = a.x ^ b
  σ = abs(x * b * a.σ / a.x)
  Measure(x, σ)
end

Base.sqrt(a::Measure) = a ^ .5

end  # module NumericError

using NumericError

# x1 = 100 ± 1.1
# y1 = 50 ± 1.2
# x2 = 200 ± 2.2
# y2 = 100 ± 2.3

x1 = Measure(100, 1.1)
y1 = Measure(50,  1.2)
x2 = Measure(200, 2.2)
y2 = Measure(100, 2.3)

d = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

@show x1 y1 x2 y2 sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

```


{{out}}

```txt

x1 = 100.0 ± 1.1
y1 = 50.0 ± 1.2
x2 = 200.0 ± 2.2
y2 = 100.0 ± 2.3
sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2) = 111.80339887498948 ± 1.7586926962946086

```



## Kotlin

{{trans|Java}}

```scala
import java.lang.Math.*

data class Approx(val ν: Double, val σ: Double = 0.0) {
    constructor(a: Approx) : this(a.ν, a.σ)
    constructor(n: Number) : this(n.toDouble(), 0.0)

    override fun toString() = "$ν ±$σ"

    operator infix fun plus(a: Approx) = Approx(ν + a.ν, sqrt(σ * σ + a.σ * a.σ))
    operator infix fun plus(d: Double) = Approx(ν + d, σ)
    operator infix fun minus(a: Approx) = Approx(ν - a.ν, sqrt(σ * σ + a.σ * a.σ))
    operator infix fun minus(d: Double) = Approx(ν - d, σ)

    operator infix fun times(a: Approx): Approx {
        val v = ν * a.ν
        return Approx(v, sqrt(v * v * σ * σ / (ν * ν) + a.σ * a.σ / (a.ν * a.ν)))
    }

    operator infix fun times(d: Double) = Approx(ν * d, abs(d * σ))

    operator infix fun div(a: Approx): Approx {
        val v = ν / a.ν
        return Approx(v, sqrt(v * v * σ * σ / (ν * ν) + a.σ * a.σ / (a.ν * a.ν)))
    }

    operator infix fun div(d: Double) = Approx(ν / d, abs(d * σ))

    fun pow(d: Double): Approx {
        val v = pow(ν, d)
        return Approx(v, abs(v * d * σ / ν))
    }
}

fun main(args: Array<String>) {
    val x1 = Approx(100.0, 1.1)
    val y1 = Approx(50.0, 1.2)
    val x2 = Approx(200.0, 2.2)
    val y2 = Approx(100.0, 2.3)
    println(((x1 - x2).pow(2.0) + (y1 - y2).pow(2.0)).pow(0.5))
}
```

{{out}}

```txt
111.80339887498948 ±2.4871670631463423
```



## Mathematica


```mathematica
PlusMinus /: a_ ± σa_ + c_?NumericQ := N[(a + c) ± σa];
PlusMinus /: a_ ± σa_ + b_ ± σb_ := N[(a + b) ± Norm@{σa, σb}];
PlusMinus /: c_?NumericQ (a_ ± σa_) := N[c a ± Abs[c σa]];
PlusMinus /: (a_ ± σa_) (b_ ± σb_) := N[a b ± (a b Norm@{σa/a, σb/b})^2];
PlusMinus /: (a_ ± σa_)^c_?NumericQ := N[a^c ± Abs[a^c σa/a]];
```


```mathematica
x1 = 100 ± 1.1;
y1 = 50 ± 1.2;
x2 = 200 ± 2.2;
y2 = 100 ± 2.3;
d = Sqrt[(x1 - x2)^2 + (y1 - y2)^2]
```

{{Out}}

```txt
111.803 ± 2.48717
```


## Nim


```Nim
import strformat
import math

type
  Imprecise = object
    x: float
    σ: float

template `^`(a, b: float): float =
  pow(a, b)
template `-`(a: Imprecise): Imprecise =
  Imprecise(x: -a.x, σ: a.σ)
template `+`(a, b: Imprecise): Imprecise =
  Imprecise(x: a.x + b.x, σ: sqrt(a.σ ^ 2 + b.σ ^ 2))
template `-`(a, b: Imprecise): Imprecise =
  Imprecise(x: a.x - b.x, σ: sqrt(a.σ ^ 2 + b.σ ^ 2))
template `*`(a, b: Imprecise): Imprecise =
  let x = a.x * b.x
  let σ = sqrt(x ^ 2 * ((a.σ / a.x) ^ 2 + (b.σ / b.x) ^ 2))
  Imprecise(x: x, σ: σ)
template `/`(a, b: Imprecise): Imprecise =
  let x = a.x / b.x
  let σ = sqrt(x ^ 2 * ((a.σ / a.x) ^ 2 + (b.σ / b.x) ^ 2))
  Imprecise(x: x, σ: σ)
template `^`(a: Imprecise, b: float): Imprecise =
  if b < 0:
    raise newException(IOError, "Cannot raise to negative power.") 
  let x = a.x ^ b
  let σ = abs(x * b * a.σ / a.x)
  Imprecise(x: x, σ: σ)
template sqrt(a: Imprecise): Imprecise =
  a ^ 0.5

proc `$`(a: Imprecise): string =
  fmt"{a.x:.2f} ± {a.σ:.2f}"

var x1 = Imprecise(x: 100, σ: 1.1)
var y1 = Imprecise(x: 50, σ: 1.2)
var x2 = Imprecise(x: 200, σ: 2.2)
var y2 = Imprecise(x: 100, σ: 2.3)

echo $(sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2))
```

{{out}}

```txt

111.80 ± 2.49

```



## PARI/GP

''This is a work-in-progress.''

```parigp
add(a,b)=if(type(a)==type(b), a+b, if(type(a)=="t_VEC",a+[b,0],b+[a,0]));
sub(a,b)=if(type(a)==type(b), [a[1]-b[1],a[2]+b[2]], if(type(a)=="t_VEC",a-[b,0],[a,0]-b));
mult(a,b)=if(type(a)=="t_VEC", if(type(b)=="t_VEC", [a[1]*b[1], abs(a[1]*b[1])*sqrt(norml2([a[2]/a[1],b[2]/b[1]]))], [b*a[1], abs(b)*a[2]]), [a*b[1], abs(a)*b[2]]);
div(a,b)=if(type(b)!="t_VEC", mult(a,1/b), mult(a,[1/b[1],b[2]/b[1]^2]));
pow(a,b)=[a[1]^b, abs(a[1]^b*b*a[2]/a[1])];
x1=[100,1.1];y1=[50,1.2];x2=[200,2.2];y2=[100,2.3];
pow(add(pow(sub(x1,x2),2),pow(sub(y1,y2),2)),.5)
```



## Perl

Following code keeps track of covariance between variables.  Each variable with error contains its mean value and components of error source from a set of indepentent variables.  It's more than what the task requires.

```perl
use utf8;
package ErrVar;
use strict;

# helper function, apply f to pairs (a, b) from listX and listY
sub zip(&$$) {
	my ($f, $x, $y) = @_;
	my $l = $#$x;
	if ($l < $#$y) { $l = $#$y };

	my @out;
	for (0 .. $l) {
		local $a = $x->[$_];
		local $b = $y->[$_];
		push @out, $f->();
	}
	\@out
}

use overload
	'""'	=> \&_str,
	'+'	=> \&_add,
	'-'	=> \&_sub,
	'*'	=> \&_mul,
	'/'	=> \&_div,
	'bool'	=> \&_bool,
	'<=>'	=> \&_ncmp,
	'neg'	=> \&_neg,
	
	'sqrt'	=> \&_sqrt,
	'log'	=> \&_log,
	'exp'	=> \&_exp,
	'**'	=> \&_pow,
;

# make a variable with mean value and a list of coefficient to
# variables providing independent errors
sub make {
	my $x = shift;
	bless [$x, [@{+shift}]]
}

sub _str { sprintf "%g±%.3g", $_[0][0], sigma($_[0]) }

# mean value of the var, or just the input if it's not of this class
sub mean {
	my $x = shift;
	ref($x) && $x->isa(__PACKAGE__) ? $x->[0] : $x
}

# return variance index array
sub vlist {
	my $x = shift;
	ref($x) && $x->isa(__PACKAGE__) ? $x->[1] : [];
}

sub variance {
	my $x = shift;
	return 0 unless ref($x) and $x->isa(__PACKAGE__);
	my $s;
	$s += $_ * $_ for (@{$x->[1]});
	$s
}

sub covariance {
	my ($x, $y) = @_;
	return 0 unless ref($x) && $x->isa(__PACKAGE__);
	return 0 unless ref($y) && $y->isa(__PACKAGE__);

	my $s;
	zip { $s += $a * $b } vlist($x), vlist($y);
	$s
}

sub sigma { sqrt variance(shift) }

# to determine if a var is probably zero. we use 1σ here
sub _bool {
	my $x = shift;
	return abs(mean($x)) > sigma($x);
}

sub _ncmp {
	my $x = shift() - shift()	or return 0;
	return mean($x) > 0 ? 1 : -1;
}

sub _neg {
	my $x = shift;
	bless [ -mean($x), [map(-$_, @{vlist($x)}) ] ];
}

sub _add {
	my ($x, $y) = @_;
	my ($x0, $y0) = (mean($x), mean($y));
	my ($xv, $yv) = (vlist($x), vlist($y));
	bless [$x0 + $y0, zip {$a + $b} $xv, $yv];
}

sub _sub {
	my ($x, $y, $swap) = @_;
	if ($swap) { ($x, $y) = ($y, $x) }
	my ($x0, $y0) = (mean($x), mean($y));
	my ($xv, $yv) = (vlist($x), vlist($y));
	bless [$x0 - $y0, zip {$a - $b} $xv, $yv];
}

sub _mul {
	my ($x, $y) = @_;
	my ($x0, $y0) = (mean($x), mean($y));
	my ($xv, $yv) = (vlist($x), vlist($y));

	$xv = [ map($y0 * $_, @$xv) ];
	$yv = [ map($x0 * $_, @$yv) ];

	bless [$x0 * $y0, zip {$a + $b} $xv, $yv];
}

sub _div {
	my ($x, $y, $swap) = @_;
	if ($swap) { ($x, $y) = ($y, $x) }

	my ($x0, $y0) = (mean($x), mean($y));
	my ($xv, $yv) = (vlist($x), vlist($y));

	$xv = [ map($_/$y0, @$xv) ];
	$yv = [ map($x0 * $_/$y0/$y0, @$yv) ];

	bless [$x0 / $y0, zip {$a + $b} $xv, $yv];
}

sub _sqrt {
	my $x = shift;
	my $x0 = mean($x);
	my $xv = vlist($x);
	$x0 = sqrt($x0);
	$xv = [ map($_ / 2 / $x0, @$xv) ];
	bless [$x0, $xv]
}

sub _pow {
	my ($x, $y, $swap) = @_;
	if ($swap) { ($x, $y) = ($y, $x) }
	if ($x < 0) {
		if (int($y) != $y || ($y & 1)) {
			die "Can't take pow of negative number $x";
		}
		$x = -$x;
	}
	exp($y * log $x)
}

sub _exp {
	my $x = shift;
	my $x0 = exp(mean($x));
	my $xv = vlist($x);
	bless [ $x0, [map($x0 * $_, @$xv) ] ]
}

sub _log {
	my $x = shift;
	my $x0 = mean($x);
	my $xv = vlist($x);
	bless [ log($x0), [ map($_ / $x0, @$xv) ] ]
}

"If this package were to be in its own file, you need some truth value to end it like this.";

package main;

sub e { ErrVar::make @_ };

# x1 is of mean value 100, containing error 1.1 from source 1, etc.
# all error sources are independent.
my $x1 = e 100, [1.1, 0,   0,   0  ];
my $x2 = e 200, [0,   2.2, 0,   0  ];
my $y1 = e 50,  [0,   0,   1.2, 0  ];
my $y2 = e 100, [0,   0,   0,   2.3];

my $z1 = sqrt(($x1 - $x2) ** 2 + ($y1 - $y2) ** 2);
print "distance: $z1\n\n";

# this is not for task requirement
my $a = $x1 + $x2;
my $b = $y1 - 2 * $x2;
print "covariance between $a and $b: ", $a->covariance($b), "\n";
```
output<lang>distance: 111.803±2.49

covariance between 300±2.46 and -350±4.56: -9.68
```



## Perl 6

{{Works with|rakudo|2018.03}}
{{trans|Perl}}

```perl6
# cache of independent sources so we can make them all the same length.
# (Because Perl 6 does not yet have a longest-zip metaoperator.)
my @INDEP;

class Approx does Numeric {
    has Real $.x;	# The mean.
    has $.c;		# The components of error.

    multi method Str  { sprintf "%g±%.3g", $!x, $.σ }
    multi method Bool { abs($!x) > $.σ }

    method variance { [+] @.c X** 2 }
    method σ { sqrt self.variance }
}

multi approx($x,$c) { Approx.new: :$x, :$c }
multi approx($x) { Approx.new: :$x, :c[0 xx +@INDEP] }

# Each ± gets its own source slot.
multi infix:<±>($a, $b) {
    .push: 0 for @INDEP; # lengthen older component lists
    my $c = [ flat 0 xx @INDEP, $b ];
    @INDEP.push: $c;	 # add new component list

    approx $a, $c;
}

multi prefix:<->(Approx $a) { approx -$a.x, [$a.c.map: -*] }

multi infix:<+>($a, Approx $b) { approx($a) + $b }
multi infix:<+>(Approx $a, $b) { $a + approx($b) }
multi infix:<+>(Approx $a, Approx $b) { approx $a.x + $b.x, [$a.c Z+ $b.c] }

multi infix:<->($a, Approx $b) { approx($a) - $b }
multi infix:<->(Approx $a, $b) { $a - approx($b) }
multi infix:<->(Approx $a, Approx $b) { approx $a.x - $b.x, [$a.c Z- $b.c] }
 
multi covariance(Real   $a, Real   $b) { 0 }
multi covariance(Approx $a, Approx $b) { [+] $a.c Z* $b.c }

multi infix:«<=>»(Approx $a, Approx $b) { $a.x <=> $b.x }
multi infix:<cmp>(Approx $a, Approx $b) { $a.x <=> $b.x }
 
multi infix:<*>($a, Approx $b) { approx($a) * $b }
multi infix:<*>(Approx $a, $b) { $a * approx($b) }
multi infix:<*>(Approx $a, Approx $b) {
    approx $a.x * $b.x,
           [$a.c.map({$b.x * $_}) Z+ $b.c.map({$a.x * $_})];
}
 
multi infix:</>($a, Approx $b) { approx($a) / $b }
multi infix:</>(Approx $a, $b) { $a / approx($b) }
multi infix:</>(Approx $a, Approx $b) {
    approx $a.x / $b.x,
           [ $a.c.map({ $_ / $b.x }) Z+ $b.c.map({ $a.x * $_ / $b.x / $b.x }) ];
}
 
multi sqrt(Approx $a) {
    my $x = sqrt($a.x);
    approx $x, [ $a.c.map: { $_ / 2 / $x } ];
}
 
multi infix:<**>(Approx $a, Real $b) { $a ** approx($b) }
multi infix:<**>(Approx $a is copy, Approx $b) {
	my $ax = $a.x;
	my $bx = $b.x;
	my $fbx = floor $b.x;
	if $ax < 0 {
	    if $fbx != $bx or $fbx +& 1 {
		die "Can't take power of negative number $ax";
	    }
	    $a = -$a;
	}
	exp($b * log $a);
}
 
multi exp(Approx $a) {
	my $x = exp($a.x);
	approx $x, [ $a.c.map: { $x * $_ } ];
}
 
multi log(Approx $a) {
	my $x0 = $a.x;
	approx log($x0), [ $a.c.map: { $_ / $x0 }];
}
 
# Each ± sets up an independent source component.
my $x1 = 100 ± 1.1;
my $x2 = 200 ± 2.2;
my $y1 = 50  ± 1.2;
my $y2 = 100 ± 2.3;

# The standard task.
my $z1 = sqrt(($x1 - $x2) ** 2 + ($y1 - $y2) ** 2);
say "distance: $z1\n";

# Just showing off.
my $a = $x1 + $x2;
my $b = $y1 - 2 * $x2;
say "covariance between $a and $b: ", covariance($a,$b);
```

{{out}}

```txt
distance: 111.803±2.49

covariance between 300±2.46 and -350±4.56: -9.68
```



## Phix


```Phix
enum VALUE, DELTA

type imprecise(object imp)
    return sequence(imp) and atom(imp[VALUE]) and atom(imp[DELTA])
end type 

function sqr(atom a)
    return a*a
end function

function imprecise_add(imprecise a, b)
    atom delta = sqrt(sqr(a[DELTA]) + sqr(b[DELTA]))
    imprecise ret = {a[VALUE] + b[VALUE], delta}
    return ret
end function
 
function imprecise_mul(imprecise a, b)
    atom delta = sqrt(sqr(a[VALUE]*b[DELTA]) + sqr(b[VALUE]*a[DELTA]))
    imprecise ret = {a[VALUE] * b[VALUE],delta}
    return ret
end function
 
function imprecise_div(imprecise a, b)
    atom delta = sqrt(sqr(a[VALUE]*b[DELTA]) + sqr(b[VALUE]*a[DELTA]))/sqr(b[VALUE])
    imprecise ret = {a[VALUE] / b[VALUE], delta}
    return ret
end function
 
function imprecise_pow(imprecise a, atom c)
    atom v = power(a[VALUE], c),
         delta = abs(v*c*a[DELTA]/a[VALUE])
    imprecise ret = {v,delta}
    return ret
end function
 
function printImprecise(imprecise imp)
    return sprintf("%g+/-%g",imp)
end function
 
imprecise x1 = {100, 1.1},
          y1 = {50, 1.2},
          x2 = {-200, 2.2},
          y2 = {-100, 2.3},
          tmp1, tmp2,
          d
 
    tmp1 = imprecise_add(x1, x2)
    tmp1 = imprecise_pow(tmp1, 2)
    tmp2 = imprecise_add(y1, y2)
    tmp2 = imprecise_pow(tmp2, 2)
    d = imprecise_add(tmp1,tmp2)
    d = imprecise_pow(d, 0.5)
    printf(1,"Distance, d, between the following points :")
    printf(1,"\n( x1, y1) = ( %s, %s)",{printImprecise(x1),printImprecise(y1)})
    printf(1,"\n( x2, y2) = ( %s, %s)",{printImprecise(x2),printImprecise(y2)})
    printf(1,"\nis d = %s\n", {printImprecise(d)})
```

Aside: obviously you don't ''have'' to use tmp1/2 like that, but I find that style often makes things much easier to debug.
{{out}}

```txt

Distance, d, between the following points :
( x1, y1) = ( 100+/-1.1, 50+/-1.2)
( x2, y2) = ( -200+/-2.2, -100+/-2.3)
is d = 111.803+/-2.48717

```



## PicoLisp

For this task, we overload the built-in arithmetic functions. If the arguments are cons pairs, they are assumed to hold the fixpoint number in the CAR, and the uncertainty's square in the CDR. Otherwise normal numbers are handled as usual.

The overloaded +, -, * and / operators look a bit complicated, because they must handle an arbitrary number of arguments to be compatible with the standard operators.

```PicoLisp
(scl 12)
(load "@lib/math.l")

# Overload arithmetic operators +, -, *, / and **
(redef + @
   (let R (next)
      (while (args)
         (let N (next)
            (setq R
               (if2 (atom R) (atom N)
                  (+ R N)                       # c + c
                  (cons (+ R (car N)) (cdr N))  # c + a
                  (cons (+ (car R) N) (cdr R))  # a + c
                  (cons                         # a + b
                     (+ (car R) (car N))
                     (+ (cdr R) (cdr N)) ) ) ) ) )
      R ) )

(redef - @
   (let R (next)
      (ifn (args)
         (- R)
         (while (args)
            (let N (next)
               (setq R
                  (if2 (atom R) (atom N)
                     (- R N)                       # c - c
                     (cons (- R (car N)) (cdr N))  # c - a
                     (cons (- (car R) N) (cdr R))  # a - c
                     (cons                         # a - b
                        (- (car R) (car N))
                        (+ (cdr R) (cdr N)) ) ) ) ) )
         R ) ) )

(redef * @
   (let R (next)
      (while (args)
         (let N (next)
            (setq R
               (if2 (atom R) (atom N)
                  (* R N)                                        # c * c
                  (cons                                          # c * a
                     (*/ R (car N) 1.0)
                     (mul2div2 (cdr N) R 1.0) )
                  (cons                                          # a * c
                     (*/ (car R) N 1.0)
                     (mul2div2 (cdr R) N 1.0) )
                  (uncMul (*/ (car R) (car N) 1.0) R N) ) ) ) )  # a * b
      R ) )

(redef / @
   (let R (next)
      (while (args)
         (let N (next)
            (setq R
               (if2 (atom R) (atom N)
                  (/ R N)                                        # c / c
                  (cons                                          # c / a
                     (*/ R 1.0 (car N))
                     (mul2div2 (cdr N) R 1.0) )
                  (cons                                          # a / c
                     (*/ (car R) 1.0 N)
                     (mul2div2 (cdr R) N 1.0) )
                  (uncMul (*/ (car R) 1.0 (car N)) R N) ) ) ) )  # a / b
      R ) )

(redef ** (A C)
   (if (atom A)
      (** A C)
      (let F (pow (car A) C)
         (cons F
            (mul2div2 (cdr A) (*/ F C (car A)) 1.0) ) ) ) )

# Utilities
(de mul2div2 (A B C)
   (*/ A B B (* C C)) )

(de uncMul (F R N)
   (cons F
      (mul2div2
         (+
            (mul2div2 (cdr R) 1.0 (car R))
            (mul2div2 (cdr N) 1.0 (car N)) )
         F
         1.0 ) ) )

# I/O conversion
(de unc (N U)
   (if U
      (cons N (*/ U U 1.0))
      (pack
         (round (car N) 10)
         " ± "
         (round (sqrt (cdr N) 1.0) 8) ) ) )
```

Test:

```PicoLisp
(de distance (X1 Y1 X2 Y2)
   (**
      (+ (** (- X1 X2) 2.0) (** (- Y1 Y2) 2.0))
      0.5 ) )

(prinl "Distance: "
   (unc
      (distance
         (unc 100. 1.1)
         (unc 50. 1.2)
         (unc 200. 2.2)
         (unc 100. 2.3) ) ) )
```

Output:

```txt
Distance: 111.8033988750 ± 2.48716706
```



## Python


```python
from collections import namedtuple
import math
 
class I(namedtuple('Imprecise', 'value, delta')):
    'Imprecise type: I(value=0.0, delta=0.0)' 
 
    __slots__ = () 
 
    def __new__(_cls, value=0.0, delta=0.0):
        'Defaults to 0.0 ± delta'
        return super().__new__(_cls, float(value), abs(float(delta)))
 
    def reciprocal(self):
        return I(1. / self.value, self.delta / (self.value**2)) 
 
    def __str__(self):
        'Shorter form of Imprecise as string'
        return 'I(%g, %g)' % self
 
    def __neg__(self):
        return I(-self.value, self.delta)
 
    def __add__(self, other):
        if type(other) == I:
            return I( self.value + other.value, (self.delta**2 + other.delta**2)**0.5 )
        try:
            c = float(other)
        except:
            return NotImplemented
        return I(self.value + c, self.delta)

    def __sub__(self, other):
        return self + (-other)
 
    def __radd__(self, other):
        return I.__add__(self, other)
 
    def __mul__(self, other):
        if type(other) == I:
            #if id(self) == id(other):    
            #    return self ** 2
            a1,b1 = self
            a2,b2 = other
            f = a1 * a2
            return I( f, f * ( (b1 / a1)**2 + (b2 / a2)**2 )**0.5 )
        try:
            c = float(other)
        except:
            return NotImplemented
        return I(self.value * c, self.delta * c)
 
    def __pow__(self, other):
        if type(other) == I:
            return NotImplemented
        try:
            c = float(other)
        except:
            return NotImplemented
        f = self.value ** c
        return I(f, f * c * (self.delta / self.value))
 
    def __rmul__(self, other):
        return I.__mul__(self, other)
 
    def __truediv__(self, other):
        if type(other) == I:
            return self.__mul__(other.reciprocal())
        try:
            c = float(other)
        except:
            return NotImplemented
        return I(self.value / c, self.delta / c)
 
    def __rtruediv__(self, other):
        return other * self.reciprocal()
 
    __div__, __rdiv__ = __truediv__, __rtruediv__
 
Imprecise = I

def distance(p1, p2):
    x1, y1 = p1
    x2, y2 = p2
    return ((x1 - x2)**2 + (y1 - y2)**2)**0.5
 
x1 = I(100, 1.1)
x2 = I(200, 2.2)
y1 = I( 50, 1.2)
y2 = I(100, 2.3)

p1, p2 = (x1, y1), (x2, y2)
print("Distance between points\n  p1: %s\n  and p2: %s\n  = %r" % (
      p1, p2, distance(p1, p2)))
```


;Sample output:

```txt
Distance between points
  p1: (I(value=100.0, delta=1.1), I(value=50.0, delta=1.2))
  and p2: (I(value=200.0, delta=2.2), I(value=100.0, delta=2.3))
  = I(value=111.80339887498948, delta=2.4871670631463423)
```



## Racket

{{trans|Mathematica}}

```racket
#lang racket

(struct ± (x dx) #:transparent
  #:methods gen:custom-write
  [(define (write-proc a port mode) (display (±->string a) port))])

(define/match (±+ a [b 0])
  [((± x dx) (± y dy)) (± (+ x y) (norm dx dy))]
  [((± x dx) c) (± (+ x c) dx)]
  [(_ (± y dy)) (±+ b a)])

(define/match (±* a b)
  [((± x dx) (± y dy)) (± (* x y) (* x y (norm (/ dx x) (/ dy y))))]
  [((± x dx) c) (± (* x c) (abs (* c dx)))]
  [(_ (± y dy)) (±* b a)])

(define/match (±- a [b #f])
  [(a #f) (±* -1 a)]
  [(a b) (±+ a (±* -1 b))])

(define/match (±/ a b)
  [((± x dx) (± y dy)) (± (/ x y) (/ x y (norm (/ dx x) (/ dy y))))]
  [((± _ _) c) (±* a (/ 1 c))])

(define/match (±expt a c)
  [((± x dx) c) (± (expt x c) (abs (* (expt x c) (/ dx x))))])

(define/match (norm a b)
  [((± x dx) (± y dy)) (±expt (±+ (±expt a 2) (±expt b 2)) 0.5)]
  [(x y) (sqrt (+ (sqr x) (sqr y)))])

(define/match (±->string x [places 3])
  [((± x dx) p) (string-join (map (λ (s) (real->decimal-string s p))
                                  (list x dx))" ± ")])

;; Test
;;
(define x1 (± 100 1.1))
(define y1 (± 50 1.2))
(define x2 (± 200 2.2))
(define y2 (± 100 2.3))
(norm (±- x1 x2) (±- y1 y2))
```


{{output}}

```txt
111.803 ± 2.487
```



## REXX

{{trans|Fortran}}

```rexx
/*REXX program calculates the distance between two points (2D)  with error propagation. */
parse arg a b .                                           /*obtain arguments from the CL*/
if a=='' | a==","  then a= '100±1.1,  50±1.2'             /*Not given? Then use default.*/
if b=='' | b==","  then b= '200±2.2, 100±2.3'             /* "    "      "   "     "    */
parse var a  ax ',' ay;     parse var b  bx ',' by        /*obtain X,Y from A & B point.*/
parse var ax ax '±' axe;    parse var bx bx '±' bxE       /*   "   err   "  Ax  and  Bx.*/
parse var ay ay '±' aye;    parse var by by '±' byE       /*   "    "    "  Ay   "   By.*/
if axE==''  then axE=0;     if bxE==""  then bxE=0;       /*No error?  Then use default.*/
if ayE==''  then ayE=0;     if byE==""  then byE=0;       /* "   "       "   "     "    */
   say ' A point (x,y)=   ' ax "±" axE',   ' ay "±" ayE   /*display  A  point (with err)*/
   say ' B point (x.y)=   ' bx "±" bxE',   ' by "±" byE   /*   "     B    "      "   "  */
   say                                                    /*blank line for the eyeballs.*/
dx=ax-bx;  dxE=sqrt(axE**2 + bxE**2);  xe=#(dx, 2, dxE)   /*compute  X distances (& err)*/
dy=ay-by;  dyE=sqrt(ayE**2 + byE**2);  ye=#(dy, 2, dyE)   /*   "     Y     "      "  "  */
D=sqrt(dx**2 + dy**2)                                     /*compute the   2D  distance. */
   say 'distance=' D "±" #(D**2, .5, sqrt(xE**2 + yE**2)) /*display  "     "      "     */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
#: procedure; arg x,p,e; if p=.5 then z=1/sqrt(abs(x)); else z=abs(x)**(p-1); return p*e*z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;  if x=0  then return 0;  d=digits();  numeric digits;  h=d+6
      numeric form;  parse value format(x,2,1,,0) 'E0' with g "E" _ .;    g=g * .5'e'_ % 2
      m.=9;       do j=0  while h>9;       m.j=h;               h=h%2+1;        end  /*j*/
                  do k=j+5  to 0  by -1;   numeric digits m.k;  g=(g+x/g)*.5;   end  /*k*/
      numeric digits d;                    return g/1
```

'''output'''   when using the default inputs:

```txt

 A point (x,y)=    100 ± 1.1,      50 ± 1.2
 B point (x.y)=    200 ± 2.2,     100 ± 2.3

distance= 111.803399 ± 2.48716707

```



## Ruby


```ruby
class NumberWithUncertainty
  def initialize(number, error)
    @num = number
    @err = error.abs
  end
  attr_reader :num, :err

  def +(other)
    if other.kind_of?(self.class)
      self.class.new(num + other.num, Math::hypot(err, other.err))
    else
      self.class.new(num + other, err)
    end
  end

  def -(other)
    if other.kind_of?(self.class)
      self.class.new(num - other.num, Math::hypot(err, other.err))
    else
      self.class.new(num - other, err)
    end
  end

  def *(other)
    if other.kind_of?(self.class)
      prod = num * other.num
      e = Math::hypot((prod * err / num), (prod * other.err / other.num))
      self.class.new(prod, e)
    else
      self.class.new(num * other, (err * other).abs)
    end
  end

  def /(other)
    if other.kind_of?(self.class)
      quo = num / other.num
      e = Math::hypot((quo * err / num), (quo * other.err / other.num))
      self.class.new(quo, e)
    else
      self.class.new(num / other, (err * other).abs)
    end
  end

  def **(exponent)
    Float(exponent) rescue raise ArgumentError, "not an number: #{exponent}"
    prod = num ** exponent
    self.class.new(prod, (prod * exponent * err / num).abs)
  end

  def sqrt
    self ** 0.5
  end

  def to_s
    "#{num} \u00b1 #{err}"
  end
end

x1 = NumberWithUncertainty.new(100, 1.1)
y1 = NumberWithUncertainty.new( 50, 1.2)
x2 = NumberWithUncertainty.new(200, 2.2)
y2 = NumberWithUncertainty.new(100, 2.3)

puts ((x1 - x2) ** 2 + (y1 - y2) ** 2).sqrt
```

{{out}}

```txt
111.803398874989 ± 2.48716706314634
```



## Scala

{{trans|Kotlin}}

```scala
import java.lang.Math._

class Approx(val ν: Double, val σ: Double = 0.0) {
    def this(a: Approx) = this(a.ν, a.σ)
    def this(n: Number) = this(n.doubleValue(), 0.0)

    override def toString = s"$ν ±$σ"

    def +(a: Approx) = Approx(ν + a.ν, sqrt(σ * σ + a.σ * a.σ))
    def +(d: Double) = Approx(ν + d, σ)
    def -(a: Approx) = Approx(ν - a.ν, sqrt(σ * σ + a.σ * a.σ))
    def -(d: Double) = Approx(ν - d, σ)

    def *(a: Approx) = {
        val v = ν * a.ν
        Approx(v, sqrt(v * v * σ * σ / (ν * ν) + a.σ * a.σ / (a.ν * a.ν)))
    }

    def *(d: Double) = Approx(ν * d, abs(d * σ))

    def /(a: Approx) = {
        val t = ν / a.ν
        Approx(t, sqrt(t * t * σ * σ / (ν * ν) + a.σ * a.σ / (a.ν * a.ν)))
    }

    def /(d: Double) = Approx(ν / d, abs(d * σ))

    def ^(d: Double) = {
        val t = pow(ν, d)
        Approx(t, abs(t * d * σ / ν))
    }
}

object Approx { def apply(ν: Double, σ: Double = 0.0) = new Approx(ν, σ) }

object NumericError extends App {
    def √(a: Approx) = a^0.5
    val x1 = Approx(100.0, 1.1)
    val x2 = Approx(50.0, 1.2)
    val y1 = Approx(200.0, 2.2)
    val y2 = Approx(100.0, 2.3)
    println(√(((x1 - x2)^2.0) + ((y1 - y2)^2.0)))  // => 111.80339887498948 ±2.938366893361004
}
```

{{out}}

```txt
111.80339887498948 ±2.938366893361004
```



## Tcl

{{works with|Tcl|8.6}}
Firstly, some support code for doing RAII-like things, evolved from code in the [[Quaternion type#Tcl|quaternion]] solution:

```tcl
package require Tcl 8.6
oo::class create RAII-support {
    constructor {} {
	upvar 1 { end } end
	lappend end [self]
	trace add variable end unset [namespace code {my DieNicely}]
    }
    destructor {
	catch {
	    upvar 1 { end } end
	    trace remove variable end unset [namespace code {my DieNicely}]
	}
    }
    method return {{level 1}} {
	incr level
	upvar 1 { end } end
	upvar $level { end } parent
	trace remove variable end unset [namespace code {my DieNicely}]
	lappend parent [self]
	trace add variable parent unset [namespace code {my DieNicely}]
	return -level $level [self]
    }
    # Swallows arguments
    method DieNicely args {tailcall my destroy}
}
oo::class create RAII-class {
    superclass oo::class
    method return args {
	[my new {*}$args] return 2
    }
    method unknown {m args} {
	if {[string is double -strict $m]} {
	    return [tailcall my new $m {*}$args]
	}
	next $m {*}$args
    }
    unexport create unknown
    self method create args {
	set c [next {*}$args]
	oo::define $c superclass {*}[info class superclass $c] RAII-support
	return $c
    }
}
# Makes a convenient scope for limiting RAII lifetimes
proc scope {script} {
    foreach v [info global] {
	if {[array exists ::$v] || [string match { * } $v]} continue
	lappend vars $v
	lappend vals [set ::$v]
    }
    tailcall apply [list $vars [list \
	try $script on ok msg {$msg return}
    ] [uplevel 1 {namespace current}]] {*}$vals
}
```

The implementation of the number+error class itself:

```tcl
RAII-class create Err {
    variable N E
    constructor {number {error 0.0}} {
	next
	namespace import ::tcl::mathfunc::* ::tcl::mathop::*
	variable N $number E [abs $error]
    }
    method p {} {
	return "$N \u00b1 $E"
    }

    method n {} { return $N }
    method e {} { return $E }

    method + e {
	if {[info object isa object $e]} {
	    Err return [+ $N [$e n]] [hypot $E [$e e]]
	} else {
	    Err return [+ $N $e] $E
	}
    }
    method - e {
	if {[info object isa object $e]} {
	    Err return [- $N [$e n]] [hypot $E [$e e]]
	} else {
	    Err return [- $N $e] $E
	}
    }
    method * e {
	if {[info object isa object $e]} {
	    set f [* $n [$E n]]
	    Err return $f [expr {hypot($E*$f/$N, [$e e]*$f/[$e n])}]
	} else {
	    Err return [* $N $e] [abs [* $E $e]]
	}
    }
    method / e {
	if {[info object isa object $e]} {
	    set f [/ $n [$E n]]
	    Err return $f [expr {hypot($E*$f/$N, [$e e]*$f/[$e n])}]
	} else {
	    Err return [/ $N $e] [abs [/ $E $e]]
	}
    }
    method ** c {
	set f [** $N $c]
	Err return $f [abs [* $f $c [/ $E $N]]]
    }

    export + - * / **
}
```

Demonstrating:

```tcl
set x1 [Err 100 1.1]
set x2 [Err 200 2.2]
set y1 [Err 50 1.2]
set y2 [Err 100 2.3]
# Evaluate in a local context to clean up intermediate objects
set d [scope {
    [[[$x1 - $x2] ** 2] + [[$y1 - $y2] ** 2]] ** 0.5
}]
puts "d = [$d p]"
```

Output:

```txt

d = 111.80339887498948 ± 2.4871670631463423

```


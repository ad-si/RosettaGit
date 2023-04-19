+++
title = "Arithmetic/Complex"
description = ""
date = 2019-06-22T14:50:59Z
aliases = []
[extra]
id = 2688
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}

A   '''[[wp:Complex number|complex number]]'''   is a number which can be written as:
<big><math>a + b \times i</math></big>
(sometimes shown as:
<big><math>b + a \times i</math></big>
where   <big><math>a</math></big>   and   <big><math>b</math></big>  are real numbers,   and   [[wp:Imaginary_unit|<big><math>i</math></big>]]   is   <big>&radic;{{overline| -1 }}</big>


Typically, complex numbers are represented as a pair of real numbers called the "imaginary part" and "real part",   where the imaginary part is the number to be multiplied by <big><math>i</math></big>.


;Task:
* Show addition, multiplication, negation, and inversion of complex numbers in separate functions. (Subtraction and division operations can be made with pairs of these operations.)
* Print the results for each operation tested.
* ''Optional:'' Show complex conjugation.



By definition, the   [[wp:complex conjugate|complex conjugate]]   of
<big><math>a + bi</math></big>
is
<big><math>a - bi</math></big>



Some languages have complex number libraries available.   If your language does, show the operations.   If your language does not, also show the definition of this type.





## Ada


```ada
with Ada.Numerics.Generic_Complex_Types;
with Ada.Text_IO.Complex_IO;

procedure Complex_Operations is
   -- Ada provides a pre-defined generic package for complex types
   -- That package contains definitions for composition,
   -- negation, addition, subtraction, multiplication, division,
   -- conjugation, exponentiation, and absolute value, as well as
   -- basic comparison operations.
   -- Ada provides a second pre-defined package for sin, cos, tan, cot,
   -- arcsin, arccos, arctan, arccot, and the hyperbolic versions of
   -- those trigonometric functions.

   -- The package Ada.Numerics.Generic_Complex_Types requires definition
   -- with the real type to be used in the complex type definition.

   package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   use Complex_Types;
   package Complex_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
   use Complex_IO;
   use Ada.Text_IO;

   A : Complex := Compose_From_Cartesian (Re => 1.0, Im => 1.0);
   B : Complex := Compose_From_Polar (Modulus => 1.0, Argument => 3.14159);
   C : Complex;

begin
   -- Addition
   C := A + B;
   Put("A + B = "); Put(C);
   New_Line;
   -- Multiplication
   C := A * B;
   Put("A * B = "); Put(C);
   New_Line;
   -- Inversion
   C := 1.0 / A;
   Put("1.0 / A = "); Put(C);
   New_Line;
   -- Negation
   C := -A;
   Put("-A = "); Put(C);
   New_Line;
   -- Conjugation
   C := Conjugate (C);
end Complex_Operations;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
main:(
  FORMAT compl fmt = $g(-7,5)"⊥"g(-7,5)$;

  PROC compl operations = VOID: (
    LONG COMPL a = 1.0 ⊥ 1.0;
    LONG COMPL b = 3.14159 ⊥ 1.2;

    LONG COMPL c;

    printf(($x"a="f(compl fmt)l$,a));
    printf(($x"b="f(compl fmt)l$,b));

    # addition #
    c := a + b;
    printf(($x"a+b="f(compl fmt)l$,c));
    # multiplication #
    c := a * b;
    printf(($x"a*b="f(compl fmt)l$,c));
    # inversion #
    c := 1.0 / a;
    printf(($x"1/c="f(compl fmt)l$,c));
    # negation #
    c := -a;
    printf(($x"-a="f(compl fmt)l$,c))
  );
  compl operations
)
```


{{out}}
```txt

a=1.00000⊥1.00000
b=3.14159⊥1.20000
a+b=4.14159⊥2.20000
a*b=1.94159⊥4.34159
1/c=0.50000⊥-.50000
-a=-1.0000⊥-1.0000

```



## ALGOL W

Complex is a built-in type in Algol W.

```algolw
begin
    % show some complex arithmetic                                          %
    % returns c + d, using the builtin complex + operator                   %
    complex procedure cAdd ( complex value c, d ) ; c + d;
    % returns c * d, using the builtin complex * operator                   %
    complex procedure cMul ( complex value c, d ) ; c * d;
    % returns the negation of c, using the builtin complex unary - operator %
    complex procedure cNeg ( complex value c ) ; - c;
    % returns the inverse of c, using the builtin complex / operatror       %
    complex procedure cInv ( complex value c ) ; 1 / c;
    % returns the conjugate of c                                            %
    complex procedure cConj ( complex value c ) ; realpart( c ) - imag( imagpart( c ) );
    complex c, d;
    c := 1 + 2i;
    d := 3 + 4i;
    % set I/O format for real aand complex numbers                          %
    r_format := "A"; s_w := 0; r_w := 6; r_d := 2;
    write( "c      : ",        c      );
    write( "d      : ",           d   );
    write( "c + d  : ", cAdd(  c, d ) );
    write( "c * d  : ", cMul(  c, d ) );
    write( "-c     : ", cNeg(  c    ) );
    write( "1/c    : ", cInv(  c    ) );
    write( "conj c : ", cConj( c    ) )
end.
```

{{out}}

```txt

c      :   1.00   2.00I
d      :   3.00   4.00I
c + d  :   4.00   6.00I
c * d  :  -5.00  10.00I
-c     :  -1.00  -2.00I
1/c    :   0.20  -0.40I
conj c :   1.00  -2.00I

```



## APL

<lang>
   x←1j1                ⍝assignment
   y←5.25j1.5
   x+y                  ⍝addition
6.25J2.5
   x×y                  ⍝multiplication
3.75J6.75
    ⌹x                  ⍝inversion
0.5j_0.5
    -x                  ⍝negation
¯1J¯1

```



## App Inventor

App Inventor has native support for complex numbers.

The linked image gives a few examples of complex arithmetic and a custom complex conjugate function.

[https://lh4.googleusercontent.com/-4M57lWIh_r8/Uuqgoec-hrI/AAAAAAAAJ74/2oj_5eelUR4/w1197-h766-no/Capture.PNG View the blocks and app screen...]


## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276431.html#276431 forum]

```AutoHotkey
Cset(C,1,1)
MsgBox % Cstr(C)  ; 1 + i*1
Cneg(C,C)
MsgBox % Cstr(C)  ; -1 - i*1
Cadd(C,C,C)
MsgBox % Cstr(C)  ; -2 - i*2
Cinv(D,C)
MsgBox % Cstr(D)  ; -0.25 + 0.25*i
Cmul(C,C,D)
MsgBox % Cstr(C)  ; 1 + i*0

Cset(ByRef C, re, im) {
   VarSetCapacity(C,16)
   NumPut(re,C,0,"double")
   NumPut(im,C,8,"double")
}
Cre(ByRef C) {
   Return NumGet(C,0,"double")
}
Cim(ByRef C) {
   Return NumGet(C,8,"double")
}
Cstr(ByRef C) {
   Return Cre(C) ((i:=Cim(C))<0 ? " - i*" . -i : " + i*" . i)
}
Cadd(ByRef C, ByRef A, ByRef B) {
   VarSetCapacity(C,16)
   NumPut(Cre(A)+Cre(B),C,0,"double")
   NumPut(Cim(A)+Cim(B),C,8,"double")
}
Cmul(ByRef C, ByRef A, ByRef B) {
   VarSetCapacity(C,16)
   t := Cre(A)*Cim(B)+Cim(A)*Cre(B)
   NumPut(Cre(A)*Cre(B)-Cim(A)*Cim(B),C,0,"double")
   NumPut(t,C,8,"double") ; A or B can be C!
}
Cneg(ByRef C, ByRef A) {
   VarSetCapacity(C,16)
   NumPut(-Cre(A),C,0,"double")
   NumPut(-Cim(A),C,8,"double")
}
Cinv(ByRef C, ByRef A) {
   VarSetCapacity(C,16)
   d := Cre(A)**2 + Cim(A)**2
   NumPut( Cre(A)/d,C,0,"double")
   NumPut(-Cim(A)/d,C,8,"double")
}
```



## BASIC

{{works with|QuickBasic|4.5}}

```qbasic
TYPE complex
        real AS DOUBLE
        imag AS DOUBLE
END TYPE
DECLARE SUB add (a AS complex, b AS complex, c AS complex)
DECLARE SUB mult (a AS complex, b AS complex, c AS complex)
DECLARE SUB inv (a AS complex, b AS complex)
DECLARE SUB neg (a AS complex, b AS complex)
CLS
DIM x AS complex
DIM y AS complex
DIM z AS complex
x.real = 1
x.imag = 1
y.real = 2
y.imag = 2
CALL add(x, y, z)
PRINT z.real; "+"; z.imag; "i"
CALL mult(x, y, z)
PRINT z.real; "+"; z.imag; "i"
CALL inv(x, z)
PRINT z.real; "+"; z.imag; "i"
CALL neg(x, z)
PRINT z.real; "+"; z.imag; "i"


SUB add (a AS complex, b AS complex, c AS complex)
        c.real = a.real + b.real
        c.imag = a.imag + b.imag
END SUB

SUB inv (a AS complex, b AS complex)
        denom = a.real ^ 2 + a.imag ^ 2
        b.real = a.real / denom
        b.imag = -a.imag / denom
END SUB

SUB mult (a AS complex, b AS complex, c AS complex)
        c.real = a.real * b.real - a.imag * b.imag
        c.imag = a.real * b.imag + a.imag * b.real
END SUB

SUB neg (a AS complex, b AS complex)
        b.real = -a.real
        b.imag = -a.imag
END SUB
```

{{out}}

```txt
 3 + 3 i
 0 + 4 i
 .5 +-.5 i
-1 +-1 i
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM Complex{r, i}

      DIM a{} = Complex{} : a.r = 1.0 : a.i = 1.0
      DIM b{} = Complex{} : b.r = PI# : b.i = 1.2
      DIM o{} = Complex{}

      PROCcomplexadd(o{}, a{}, b{})
      PRINT "Result of addition is " FNcomplexshow(o{})
      PROCcomplexmul(o{}, a{}, b{})
      PRINT "Result of multiplication is " ; FNcomplexshow(o{})
      PROCcomplexneg(o{}, a{})
      PRINT "Result of negation is " ; FNcomplexshow(o{})
      PROCcomplexinv(o{}, a{})
      PRINT "Result of inversion is " ; FNcomplexshow(o{})
      END

      DEF PROCcomplexadd(dst{}, one{}, two{})
      dst.r = one.r + two.r
      dst.i = one.i + two.i
      ENDPROC

      DEF PROCcomplexmul(dst{}, one{}, two{})
      dst.r = one.r*two.r - one.i*two.i
      dst.i = one.i*two.r + one.r*two.i
      ENDPROC

      DEF PROCcomplexneg(dst{}, src{})
      dst.r = -src.r
      dst.i = -src.i
      ENDPROC

      DEF PROCcomplexinv(dst{}, src{})
      LOCAL denom : denom = src.r^2 + src.i^ 2
      dst.r = src.r / denom
      dst.i = -src.i / denom
      ENDPROC

      DEF FNcomplexshow(src{})
      IF src.i >= 0 THEN = STR$(src.r) + " + " +STR$(src.i) + "i"
      = STR$(src.r) + " - " + STR$(-src.i) + "i"
```

{{out}}

```txt
Result of addition is 4.14159265 + 2.2i
Result of multiplication is 1.94159265 + 4.34159265i
Result of negation is -1 - 1i
Result of inversion is 0.5 - 0.5i
```



## Bracmat

Bracmat recognizes the symbol <code>i</code> as the square root of <code>-1</code>. The results of the functions below are not necessarily of the form <code>a+b*i</code>, but as the last example shows, Bracmat nevertheless can work out that two different representations of the same mathematical object, when subtracted from each other, give zero. You may wonder why in the functions <code>multiply</code> and <code>negate</code> there are terms <code>1</code> and <code>-1</code>. These terms are a trick to force Bracmat to expand the products. As it is more costly to factorize a sum than to expand a product into a sum, Bracmat retains isolated products. However, when in combination with a non-zero term, the product is expanded.

```bracmat
  (add=a b.!arg:(?a,?b)&!a+!b)
& ( multiply
  = a b.!arg:(?a,?b)&1+!a*!b+-1
  )
& (negate=.1+-1*!arg+-1)
& ( conjugate
  =   a b
    .   !arg:i&-i
      | !arg:-i&i
      | !arg:?a_?b&(conjugate$!a)_(conjugate$!b)
      | !arg
  )
& ( invert
  =   conjugated
    .   conjugate$!arg:?conjugated
      & multiply$(!arg,!conjugated)^-1*!conjugated
  )
& out$("(a+i*b)+(a+i*b) =" add$(a+i*b,a+i*b))
& out$("(a+i*b)+(a+-i*b) =" add$(a+i*b,a+-i*b))
& out$("(a+i*b)*(a+i*b) =" multiply$(a+i*b,a+i*b))
& out$("(a+i*b)*(a+-i*b) =" multiply$(a+i*b,a+-i*b))
& out$("-1*(a+i*b) =" negate$(a+i*b))
& out$("-1*(a+-i*b) =" negate$(a+-i*b))
& out$("sin$x = " sin$x)
& out$("conjugate sin$x  =" conjugate$(sin$x))
&   out
  $ ("sin$x minus conjugate sin$x =" sin$x+negate$(conjugate$(sin$x)))
& done;
```

{{out}}

```txt
(a+i*b)+(a+i*b) = 2*a+2*i*b
(a+i*b)+(a+-i*b) = 2*a
(a+i*b)*(a+i*b) = a^2+-1*b^2+2*i*a*b
(a+i*b)*(a+-i*b) = a^2+b^2
-1*(a+i*b) = -1*a+-i*b
-1*(a+-i*b) = -1*a+i*b
sin$x =  i*(-1/2*e^(i*x)+1/2*e^(-i*x))
conjugate sin$x  = -i*(1/2*e^(i*x)+-1/2*e^(-i*x))
sin$x minus conjugate sin$x = 0
```



## C

{{works with|C99}}
The more recent [[C99]] standard has built-in complex number primitive types, which can be declared with float, double, or long double precision. To use these types and their associated library functions, you must include the <complex.h> header. (Note: this is a ''different'' header than the <complex> templates that are defined by [[C++]].) [http://www.opengroup.org/onlinepubs/009695399/basedefs/complex.h.html] [http://publib.boulder.ibm.com/infocenter/pseries/v5r3/index.jsp?topic=/com.ibm.vacpp7a.doc/language/ref/clrc03complex_types.htm]

```c
#include <complex.h>
#include <stdio.h>

void cprint(double complex c)
{
  printf("%f%+fI", creal(c), cimag(c));
}
void complex_operations() {
  double complex a = 1.0 + 1.0I;
  double complex b = 3.14159 + 1.2I;

  double complex c;

  printf("\na="); cprint(a);
  printf("\nb="); cprint(b);

  // addition
  c = a + b;
  printf("\na+b="); cprint(c);
  // multiplication
  c = a * b;
  printf("\na*b="); cprint(c);
  // inversion
  c = 1.0 / a;
  printf("\n1/c="); cprint(c);
  // negation
  c = -a;
  printf("\n-a="); cprint(c);
  // conjugate
  c = conj(a);
  printf("\nconj a="); cprint(c); printf("\n");
}
```


{{works with|C89}}
User-defined type:

```c
typedef struct{
        double real;
        double imag;
} Complex;

Complex add(Complex a, Complex b){
        Complex ans;
        ans.real = a.real + b.real;
        ans.imag = a.imag + b.imag;
        return ans;
}

Complex mult(Complex a, Complex b){
        Complex ans;
        ans.real = a.real * b.real - a.imag * b.imag;
        ans.imag = a.real * b.imag + a.imag * b.real;
        return ans;
}

/* it's arguable that things could be better handled if either
   a.real or a.imag is +/-inf, but that's much work */
Complex inv(Complex a){
        Complex ans;
        double denom = a.real * a.real + a.imag * a.imag;
        ans.real =  a.real / denom;
        ans.imag = -a.imag / denom;
        return ans;
}

Complex neg(Complex a){
        Complex ans;
        ans.real = -a.real;
        ans.imag = -a.imag;
        return ans;
}

Complex conj(Complex a){
        Complex ans;
        ans.real =  a.real;
        ans.imag = -a.imag;
        return ans;
}

void put(Complex c)
{
        printf("%lf%+lfI", c.real, c.imag);
}

void complex_ops(void)
{
  Complex a = { 1.0,     1.0 };
  Complex b = { 3.14159, 1.2 };

  printf("\na=");   put(a);
  printf("\nb=");   put(b);
  printf("\na+b="); put(add(a,b));
  printf("\na*b="); put(mult(a,b));
  printf("\n1/a="); put(inv(a));
  printf("\n-a=");  put(neg(a));
  printf("\nconj a=");  put(conj(a));  printf("\n");
}
```



## C#

{{works with|C sharp|4.0}}

```c#
namespace RosettaCode.Arithmetic.Complex
{
    using System;
    using System.Numerics;

    internal static class Program
    {
        private static void Main()
        {
            var number = Complex.ImaginaryOne;
            foreach (var result in new[] { number + number, number * number, -number, 1 / number, Complex.Conjugate(number) })
            {
                Console.WriteLine(result);
            }
        }
    }
}
```

{{works with|C sharp|1.2}}

```c#
using System;

public struct ComplexNumber
{
    public static readonly ComplexNumber i = new ComplexNumber(0.0, 1.0);
    public static readonly ComplexNumber Zero = new ComplexNumber(0.0, 0.0);

    public double Re;
    public double Im;

    public ComplexNumber(double re)
    {
        this.Re = re;
        this.Im = 0;
    }

    public ComplexNumber(double re, double im)
    {
        this.Re = re;
        this.Im = im;
    }

    public static ComplexNumber operator *(ComplexNumber n1, ComplexNumber n2)
    {
        return new ComplexNumber(n1.Re * n2.Re - n1.Im * n2.Im,
            n1.Im * n2.Re + n1.Re * n2.Im);
    }

    public static ComplexNumber operator *(double n1, ComplexNumber n2)
    {
        return new ComplexNumber(n1 * n2.Re, n1 * n2.Im);
    }

    public static ComplexNumber operator /(ComplexNumber n1, ComplexNumber n2)
    {
        double n2Norm = n2.Re * n2.Re + n2.Im * n2.Im;
        return new ComplexNumber((n1.Re * n2.Re + n1.Im * n2.Im) / n2Norm,
            (n1.Im * n2.Re - n1.Re * n2.Im) / n2Norm);
    }

    public static ComplexNumber operator /(ComplexNumber n1, double n2)
    {
        return new ComplexNumber(n1.Re / n2, n1.Im / n2);
    }

    public static ComplexNumber operator +(ComplexNumber n1, ComplexNumber n2)
    {
        return new ComplexNumber(n1.Re + n2.Re, n1.Im + n2.Im);
    }

    public static ComplexNumber operator -(ComplexNumber n1, ComplexNumber n2)
    {
        return new ComplexNumber(n1.Re - n2.Re, n1.Im - n2.Im);
    }

    public static ComplexNumber operator -(ComplexNumber n)
    {
        return new ComplexNumber(-n.Re, -n.Im);
    }

    public static implicit operator ComplexNumber(double n)
    {
        return new ComplexNumber(n, 0.0);
    }

    public static explicit operator double(ComplexNumber n)
    {
        return n.Re;
    }

    public static bool operator ==(ComplexNumber n1, ComplexNumber n2)
    {
        return n1.Re == n2.Re && n1.Im == n2.Im;
    }

    public static bool operator !=(ComplexNumber n1, ComplexNumber n2)
    {
        return n1.Re != n2.Re || n1.Im != n2.Im;
    }

    public override bool Equals(object obj)
    {
        return this == (ComplexNumber)obj;
    }

    public override int GetHashCode()
    {
        return Re.GetHashCode() ^ Im.GetHashCode();
    }

    public override string ToString()
    {
        return String.Format("{0}+{1}*i", Re, Im);
    }
}

public static class ComplexMath
{
    public static double Abs(ComplexNumber a)
    {
        return Math.Sqrt(Norm(a));
    }

    public static double Norm(ComplexNumber a)
    {
        return a.Re * a.Re + a.Im * a.Im;
    }

    public static double Arg(ComplexNumber a)
    {
        return Math.Atan2(a.Im, a.Re);
    }

    public static ComplexNumber Inverse(ComplexNumber a)
    {
        double norm = Norm(a);
        return new ComplexNumber(a.Re / norm, -a.Im / norm);
    }

    public static ComplexNumber Conjugate(ComplexNumber a)
    {
        return new ComplexNumber(a.Re, -a.Im);

    }

    public static ComplexNumber Exp(ComplexNumber a)
    {
        double e = Math.Exp(a.Re);
        return new ComplexNumber(e * Math.Cos(a.Im), e * Math.Sin(a.Im));
    }

    public static ComplexNumber Log(ComplexNumber a)
    {

        return new ComplexNumber(0.5 * Math.Log(Norm(a)), Arg(a));
    }

    public static ComplexNumber Power(ComplexNumber a, ComplexNumber power)
    {
        return Exp(power * Log(a));
    }

    public static ComplexNumber Power(ComplexNumber a, int power)
    {
        bool inverse = false;
        if (power < 0)
        {
            inverse = true; power = -power;
        }

        ComplexNumber result = 1.0;
        ComplexNumber multiplier = a;
        while (power > 0)
        {
            if ((power & 1) != 0) result *= multiplier;
            multiplier *= multiplier;
            power >>= 1;
        }

        if (inverse)
            return Inverse(result);
        else
            return result;
    }

    public static ComplexNumber Sqrt(ComplexNumber a)
    {
        return Exp(0.5 * Log(a));
    }

    public static ComplexNumber Sin(ComplexNumber a)
    {
        return Sinh(ComplexNumber.i * a) / ComplexNumber.i;
    }

    public static ComplexNumber Cos(ComplexNumber a)
    {
        return Cosh(ComplexNumber.i * a);
    }

    public static ComplexNumber Sinh(ComplexNumber a)
    {
        return 0.5 * (Exp(a) - Exp(-a));
    }

    public static ComplexNumber Cosh(ComplexNumber a)
    {
        return 0.5 * (Exp(a) + Exp(-a));
    }

}

class Program
{
    static void Main(string[] args)
    {
        // usage
        ComplexNumber i = 2;
        ComplexNumber j = new ComplexNumber(1, -2);
        Console.WriteLine(i * j);
        Console.WriteLine(ComplexMath.Power(j, 2));
        Console.WriteLine((double)ComplexMath.Sin(i) + " vs " + Math.Sin(2));
        Console.WriteLine(ComplexMath.Power(j, 0) == 1.0);
    }
}
```



## C++


```cpp
#include <iostream>
#include <complex>
using std::complex;

void complex_operations() {
  complex<double> a(1.0, 1.0);
  complex<double> b(3.14159, 1.25);

  // addition
  std::cout << a + b << std::endl;
  // multiplication
  std::cout << a * b << std::endl;
  // inversion
  std::cout << 1.0 / a << std::endl;
  // negation
  std::cout << -a << std::endl;
  // conjugate
  std::cout << std::conj(a) << std::endl;
}
```



## Clojure

Clojure on the JVM has no native support for Complex numbers.
Therefore, we use defrecord and the multimethods in
clojure.algo.generic.arithmetic to make a Complex number type.

```clojure
(ns rosettacode.arithmetic.cmplx
  (:require [clojure.algo.generic.arithmetic :as ga])
  (:import [java.lang Number]))

(defrecord Complex [^Number r ^Number i]
  Object
  (toString [{:keys [r i]}]
    (apply str
      (cond
        (zero? r) [(if (= i 1) "" i) "i"]
        (zero? i) [r]
        :else     [r (if (neg? i) "-" "+") i "i"]))))

(defmethod ga/+ [Complex Complex]
  [x y] (map->Complex (merge-with + x y)))

(defmethod ga/+ [Complex Number] ; reals become y + 0i
  [{:keys [r i]} y] (->Complex (+ r y) i))

(defmethod ga/- Complex
  [x] (->> x vals (map -) (apply ->Complex)))

(defmethod ga/* [Complex Complex]
  [x y] (map->Complex (merge-with * x y)))

(defmethod ga/* [Complex Number]
  [{:keys [r i]} y] (->Complex (* r y) (* i y)))

(ga/defmethod* ga / Complex
  [x] (->> x vals (map /) (apply ->Complex)))

(defn conj [^Complex {:keys [r i]}]
  (->Complex r (- i)))

(defn inv [^Complex {:keys [r i]}]
  (let [m (+ (* r r) (* i i))]
    (->Complex (/ r m) (- (/ i m)))))

```



## COBOL

The following is in the Managed COBOL dialect.
{{works with|Visual COBOL}}

### .NET Complex class

{{trans|C#}}

```cobol
      $SET SOURCEFORMAT "FREE"
$SET ILUSING "System"
$SET ILUSING "System.Numerics"
class-id Prog.
method-id. Main static.
procedure division.
    declare num as type Complex = type Complex::ImaginaryOne()
    declare results as type Complex occurs any
    set content of results to ((num + num), (num * num), (- num), (1 / num), type Complex::Conjugate(num))
    perform varying result as type Complex thru results
        display result
    end-perform
end method.
end class.
```



### Implementation


```cobol
      $SET SOURCEFORMAT "FREE"
class-id Prog.
method-id. Main static.
procedure division.
    declare a as type Complex = new Complex(1, 1)
    declare b as type Complex = new Complex(3.14159, 1.25)

    display "a = " a
    display "b = " b
    display space

    declare result as type Complex = a + b
    display "a + b = " result
    move (a - b) to result
    display "a - b = " result
    move (a * b) to result
    display "a * b = " result
    move (a / b) to result
    display "a / b = " result
    move (- b) to result
    display "-b = " result
    display space

    display "Inverse of b: " type Complex::Inverse(b)
    display "Conjugate of b: " type Complex::Conjugate(b)
end method.
end class.

class-id Complex.

01  Real                               float-long property.
01  Imag                               float-long property.

method-id new.
    set Real, Imag to 0
end method.

method-id new.
procedure division using value real-val as float-long, imag-val as float-long.
    set Real to real-val
    set Imag to imag-val
end method.

method-id Norm static.
procedure division using value a as type Complex returning ret as float-long.
    compute ret = a::Real ** 2 + a::Imag ** 2
end method.

method-id Inverse static.
procedure division using value a as type Complex returning ret as type Complex.
    declare norm as float-long = type Complex::Norm(a)
    set ret to new Complex(a::Real / norm, (0 - a::Imag) / norm)
end method.

method-id Conjugate static.
procedure division using value a as type Complex returning c as type Complex.
    set c to new Complex(a::Real, 0 - a::Imag)
end method.

method-id ToString override.
procedure division returning str as string.
    set str to type String::Format("{0}{1:+#0;-#}i", Real, Imag)
end method.

operator-id + .
procedure division using value a as type Complex, b as type Complex
        returning c as type Complex.
    set c to new Complex(a::Real + b::Real, a::Imag + b::Imag)
end operator.

operator-id - .
procedure division using value a as type Complex, b as type Complex
        returning c as type Complex.
    set c to new Complex(a::Real - b::Real, a::Imag - b::Imag)
end operator.

operator-id * .
procedure division using value a as type Complex, b as type Complex
        returning c as type Complex.
    set c to new Complex(a::Real * b::Real - a::Imag * b::Imag,
        a::Real * b::Imag + a::Imag * b::Real)
end operator.

operator-id / .
procedure division using value a as type Complex, b as type Complex
        returning c as type Complex.
    set c to new Complex()
    declare b-norm as float-long = type Complex::Norm(b)
    compute c::Real = (a::Real * b::Real + a::Imag * b::Imag) / b-norm
    compute c::Imag = (a::Imag * b::Real - a::Real * b::Imag) / b-norm
end operator.

operator-id - .
procedure division using value a as type Complex returning ret as type Complex.
    set ret to new Complex(- a::Real, 0 - a::Imag)
end operator.

end class.
```



## CoffeeScript


```coffeescript

# create an immutable Complex type
class Complex
  constructor: (@r=0, @i=0) ->
    @magnitude = @r*@r + @i*@i

  plus: (c2) ->
    new Complex(
      @r + c2.r,
      @i + c2.i
    )

  times: (c2) ->
    new Complex(
      @r*c2.r - @i*c2.i,
      @r*c2.i + @i*c2.r
    )

  negation: ->
    new Complex(
      -1 * @r,
      -1 * @i
    )

  inverse: ->
    throw Error "no inverse" if @magnitude is 0
    new Complex(
      @r / @magnitude,
      -1 * @i / @magnitude
    )

  toString: ->
    return "#{@r}" if @i == 0
    return "#{@i}i" if @r == 0
    if @i > 0
      "#{@r} + #{@i}i"
    else
      "#{@r} - #{-1 * @i}i"

# test
do ->
  a = new Complex(5, 3)
  b = new Complex(4, -3)

  sum = a.plus b
  console.log "(#{a}) + (#{b}) = #{sum}"

  product = a.times b
  console.log "(#{a}) * (#{b}) = #{product}"

  negation = b.negation()
  console.log "-1 * (#{b}) = #{negation}"

  diff = a.plus negation
  console.log "(#{a}) - (#{b}) = #{diff}"

  inverse = b.inverse()
  console.log "1 / (#{b}) = #{inverse}"

  quotient = product.times inverse
  console.log "(#{product}) / (#{b}) = #{quotient}"

```


{{out}}

```txt

> coffee complex.coffee
(5 + 3i) + (4 - 3i) = 9
(5 + 3i) * (4 - 3i) = 29 - 3i
-1 * (4 - 3i) = -4 + 3i
(5 + 3i) - (4 - 3i) = 1 + 6i
1 / (4 - 3i) = 0.16 + 0.12i
(29 - 3i) / (4 - 3i) = 5 + 3i

```



## Common Lisp


Complex numbers are a built-in numeric type in Common Lisp. The literal syntax for a complex number is <tt>#C(<var>real</var> <var>imaginary</var>)</tt>. The components of a complex number may be integers, ratios, or floating-point. Arithmetic operations automatically return complex (or real) numbers when appropriate:


```lisp>
 (sqrt -1)
#C(0.0 1.0)

> (expt #c(0 1) 2)
-1
```


Here are some arithmetic operations on complex numbers:


```lisp>
 (+ #c(0 1) #c(1 0))
#C(1 1)

> (* #c(1 1) 2)
#C(2 2)

> (* #c(1 1) #c(0 2))
#C(-2 2)

> (- #c(1 1))
#C(-1 -1)

> (/ #c(0 2))
#C(0 -1/2)

> (conjugate #c(1 1))
#C(1 -1)
```


Complex numbers can be constructed from real and imaginary parts using the <tt>complex</tt> function, and taken apart using the <tt>realpart</tt> and <tt>imagpart</tt> functions.


```lisp>
 (complex 64 (/ 3 4))
#C(64 3/4)

> (realpart #c(5 5))
5

> (imagpart (complex 0 pi))
3.141592653589793d0
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE Complex;
IMPORT StdLog;
TYPE
        Complex* = POINTER TO ComplexDesc;
        ComplexDesc = RECORD
                r-,i-: REAL;
        END;

VAR
        r,x,y: Complex;

PROCEDURE New(x,y: REAL): Complex;
VAR
        r: Complex;
BEGIN
        NEW(r);r.r := x;r.i := y;
        RETURN r
END New;

PROCEDURE (x: Complex) Add*(y: Complex): Complex,NEW;
BEGIN
        RETURN New(x.r + y.r,x.i + y.i)
END Add;

PROCEDURE ( x: Complex) Sub*( y: Complex): Complex, NEW;
BEGIN
        RETURN New(x.r - y.r,x.i - y.i)
END Sub;

PROCEDURE ( x: Complex) Mul*( y: Complex): Complex, NEW;
BEGIN
        RETURN New(x.r*y.r - x.i*y.i,x.r*y.i + x.i*y.r)
END Mul;

PROCEDURE ( x: Complex) Div*( y: Complex): Complex, NEW;
VAR
        d: REAL;
BEGIN
        d := y.r * y.r + y.i * y.i;
        RETURN New((x.r*y.r + x.i*y.i)/d,(x.i*y.r - x.r*y.i)/d)
END Div;

(* Reciprocal *)
PROCEDURE (x: Complex) Rec*(): Complex,NEW;
VAR
        d: REAL;
BEGIN
        d := x.r * x.r + x.i * x.i;
        RETURN New(x.r/d,(-1.0 * x.i)/d);
END Rec;

(* Conjugate *)
PROCEDURE (x: Complex) Con*(): Complex,NEW;
BEGIN
        RETURN New(x.r, (-1.0) * x.i);
END Con;

PROCEDURE (x: Complex) Out(),NEW;
BEGIN
	   StdLog.String("Complex(");
	   StdLog.Real(x.r);StdLog.String(',');StdLog.Real(x.i);
	   StdLog.String("i );")
END Out;

PROCEDURE Do*;
BEGIN
        x := New(1.5,3);
        y := New(1.0,1.0);

        StdLog.String("x: ");x.Out();StdLog.Ln;
        StdLog.String("y: ");y.Out();StdLog.Ln;
        r := x.Add(y);
        StdLog.String("x + y: ");r.Out();StdLog.Ln;
        r := x.Sub(y);
        StdLog.String("x - y: ");r.Out();StdLog.Ln;
        r := x.Mul(y);
        StdLog.String("x * y: ");r.Out();StdLog.Ln;
        r := x.Div(y);
        StdLog.String("x / y: ");r.Out();StdLog.Ln;
        r := y.Rec();
        StdLog.String("1 / y: ");r.Out();StdLog.Ln;
        r := x.Con();
        StdLog.String("x': ");r.Out();StdLog.Ln;
END Do;

END Complex.

```

Execute: ^Q Complex.Do<br/>
{{out}}

```txt

x: Complex( 1.5, 3.0i );
y: Complex( 1.0, 1.0i );
x + y: Complex( 2.5, 4.0i );
x - y: Complex( 0.5, 2.0i );
x * y: Complex( -1.5, 4.5i );
x / y: Complex( 2.25, 0.75i );
1 / y: Complex( 0.5, -0.5i );
x': Complex( 1.5, -3.0i );

```



## D

Built-in complex numbers are now deprecated in D, to simplify the language.

```d
import std.stdio, std.complex;

void main() {
    auto x = complex(1, 1); // complex of doubles on default
    auto y = complex(3.14159, 1.2);

    writeln(x + y);   // addition
    writeln(x * y);   // multiplication
    writeln(1.0 / x); // inversion
    writeln(-x);      // negation
}
```

{{out}}

```txt
4.14159+2.2i
1.94159+4.34159i
0.5-0.5i
-1-1i
```



## Dart


```dart


class complex {

  num real=0;
  num imag=0;

  complex(num r,num i){
    this.real=r;
    this.imag=i;
  }


  complex add(complex b){
    return new complex(this.real + b.real, this.imag + b.imag);
  }

  complex mult(complex b){
    //FOIL of (a+bi)(c+di) with i*i = -1
    return new complex(this.real * b.real - this.imag * b.imag, this.real * b.imag + this.imag * b.real);
  }

  complex inv(){
    //1/(a+bi) * (a-bi)/(a-bi) = 1/(a+bi) but it's more workable
    num denom = real * real + imag * imag;
    double r =real/denom;
    double i= -imag/denom;
    return new complex( r,-i);
  }

  complex neg(){
    return new complex(-real, -imag);
  }

  complex conj(){
    return new complex(real, -imag);
  }


String toString(){
  return    this.real.toString()+' + '+ this.imag.toString()+'*i';
}
}
void main() {
  var cl= new complex(1,2);
  var cl2= new complex(3,-1);
  print(cl.toString());
  print(cl2.toString());
  print(cl.inv().toString());
  print(cl2.mult(cl).toString());

}

```



## EchoLisp

Complex numbers are part of the language. No special library is needed.

```lisp

(define a 42+666i) → a
(define b 1+i) → b
(- a) → -42-666i ; negate
(+ a b) → 43+667i ; add
(* a b) → -624+708i ; multiply
(/ b) → 0.5-0.5i ; invert
(conjugate b) → 1-i
(angle b) → 0.7853981633974483 ; = PI/4
(magnitude b) → 1.4142135623730951 ; = sqrt(2)
(exp (* I PI)) → -1+0i ; Euler = e^(I*PI) = -1

```



## Elixir


```elixir
defmodule Complex do
  import Kernel, except: [abs: 1, div: 2]

  defstruct real: 0, imag: 0

  def new(real, imag) do
    %__MODULE__{real: real, imag: imag}
  end

  def add(a, b) do
    {a, b} = convert(a, b)
    new(a.real + b.real, a.imag + b.imag)
  end

  def sub(a, b) do
    {a, b} = convert(a, b)
    new(a.real - b.real, a.imag - b.imag)
  end

  def mul(a, b) do
    {a, b} = convert(a, b)
    new(a.real*b.real - a.imag*b.imag, a.imag*b.real + a.real*b.imag)
  end

  def div(a, b) do
    {a, b} = convert(a, b)
    divisor = abs2(b)
    new((a.real*b.real + a.imag*b.imag) / divisor,
        (a.imag*b.real - a.real*b.imag) / divisor)
  end

  def neg(a) do
    a = convert(a)
    new(-a.real, -a.imag)
  end

  def inv(a) do
    a = convert(a)
    divisor = abs2(a)
    new(a.real / divisor, -a.imag / divisor)
  end

  def conj(a) do
    a = convert(a)
    new(a.real, -a.imag)
  end

  def abs(a) do
    :math.sqrt(abs2(a))
  end

  defp abs2(a) do
    a = convert(a)
    a.real*a.real + a.imag*a.imag
  end

  defp convert(a) when is_number(a), do: new(a, 0)
  defp convert(%__MODULE__{} = a), do: a

  defp convert(a, b), do: {convert(a), convert(b)}

  def task do
    a = new(1, 3)
    b = new(5, 2)
    IO.puts "a = #{a}"
    IO.puts "b = #{b}"
    IO.puts "add(a,b): #{add(a, b)}"
    IO.puts "sub(a,b): #{sub(a, b)}"
    IO.puts "mul(a,b): #{mul(a, b)}"
    IO.puts "div(a,b): #{div(a, b)}"
    IO.puts "div(b,a): #{div(b, a)}"
    IO.puts "neg(a)  : #{neg(a)}"
    IO.puts "inv(a)  : #{inv(a)}"
    IO.puts "conj(a) : #{conj(a)}"
  end
end

defimpl String.Chars, for: Complex do
  def to_string(%Complex{real: real, imag: imag}) do
    if imag >= 0, do: "#{real}+#{imag}j",
                else: "#{real}#{imag}j"
  end
end

Complex.task
```


{{out}}

```txt

a = 1+3j
b = 5+2j
add(a,b): 6+5j
sub(a,b): -4+1j
mul(a,b): -1+17j
div(a,b): 0.3793103448275862+0.4482758620689655j
div(b,a): 1.1-1.3j
neg(a)  : -1-3j
inv(a)  : 0.1-0.3j
conj(a) : 1-3j

```



## Erlang


```Erlang
%% Task: Complex Arithmetic
%% Author: Abhay Jain

-module(complex_number).
-export([calculate/0]).

-record(complex, {real, img}).

calculate() ->
    A = #complex{real=1, img=3},
    B = #complex{real=5, img=2},

    Sum = add (A, B),
    print (Sum),

    Product = multiply (A, B),
    print (Product),

    Negation = negation (A),
    print (Negation),

    Inversion = inverse (A),
    print (Inversion),

    Conjugate = conjugate (A),
    print (Conjugate).

add (A, B) ->
    RealPart = A#complex.real + B#complex.real,
    ImgPart = A#complex.img + B#complex.img,
    #complex{real=RealPart, img=ImgPart}.

multiply (A, B) ->
    RealPart = (A#complex.real * B#complex.real) - (A#complex.img * B#complex.img),
    ImgPart = (A#complex.real * B#complex.img) + (B#complex.real * A#complex.img),
    #complex{real=RealPart, img=ImgPart}.

negation (A) ->
    #complex{real=-A#complex.real, img=-A#complex.img}.

inverse (A) ->
    C = conjugate (A),
    Mod = (A#complex.real * A#complex.real) + (A#complex.img * A#complex.img),
    RealPart = C#complex.real / Mod,
    ImgPart = C#complex.img / Mod,
    #complex{real=RealPart, img=ImgPart}.

conjugate (A) ->
    RealPart = A#complex.real,
    ImgPart = -A#complex.img,
    #complex{real=RealPart, img=ImgPart}.

print (A) ->
    if A#complex.img < 0 ->
        io:format("Ans = ~p~pi~n", [A#complex.real, A#complex.img]);
       true ->
        io:format("Ans = ~p+~pi~n", [A#complex.real, A#complex.img])
    end.
```

{{out}}

```Erlang
Ans = 6+5i
Ans = -1+17i
Ans = -1-3i
Ans = 0.1-0.3i
Ans = 1-3i
```



## ERRE

<lang>
PROGRAM COMPLEX_ARITH

TYPE COMPLEX=(REAL#,IMAG#)

DIM X:COMPLEX,Y:COMPLEX,Z:COMPLEX

!
! complex arithmetic routines
!
DIM A:COMPLEX,B:COMPLEX,C:COMPLEX

PROCEDURE ADD(A.,B.->C.)
    C.REAL#=A.REAL#+B.REAL#
    C.IMAG#=A.IMAG#+B.IMAG#
END PROCEDURE

PROCEDURE INV(A.->B.)
  LOCAL DENOM#
    DENOM#=A.REAL#^2+A.IMAG#^2
    B.REAL#=A.REAL#/DENOM#
    B.IMAG#=-A.IMAG#/DENOM#
END PROCEDURE

PROCEDURE MULT(A.,B.->C.)
    C.REAL#=A.REAL#*B.REAL#-A.IMAG#*B.IMAG#
    C.IMAG#=A.REAL#*B.IMAG#+A.IMAG#*B.REAL#
END PROCEDURE

PROCEDURE NEG(A.->B.)
    B.REAL#=-A.REAL#
    B.IMAG#=-A.IMAG#
END PROCEDURE

BEGIN
    PRINT(CHR$(12);) !CLS
    X.REAL#=1
    X.IMAG#=1
    Y.REAL#=2
    Y.IMAG#=2
    ADD(X.,Y.->Z.)
    PRINT(Z.REAL#;" + ";Z.IMAG#;"i")
    MULT(X.,Y.->Z.)
    PRINT(Z.REAL#;" + ";Z.IMAG#;"i")
    INV(X.->Z.)
    PRINT(Z.REAL#;" + ";Z.IMAG#;"i")
    NEG(X.->Z.)
    PRINT(Z.REAL#;" + ";Z.IMAG#;"i")
END PROGRAM

```

Note: Adapted from QuickBasic source code
{{out}}

```txt
 3 + 3 i
 0 + 4 i
 .5 +-.5 i
-1 +-1 i
```



## Euler Math Toolbox



```Euler Math Toolbox

>a=1+4i; b=5-3i;
>a+b
 6+1i
>a-b
 -4+7i
>a*b
 17+17i
>a/b
 -0.205882352941+0.676470588235i
>fraction a/b
 -7/34+23/34i
>conj(a)
 1-4i

```



## Euphoria


```euphoria
constant REAL = 1, IMAG = 2
type complex(sequence s)
    return length(s) = 2 and atom(s[REAL]) and atom(s[IMAG])
end type

function add(complex a, complex b)
    return a + b
end function

function mult(complex a, complex b)
    return {a[REAL] * b[REAL] - a[IMAG] * b[IMAG],
        a[REAL] * b[IMAG] + a[IMAG] * b[REAL]}
end function

function inv(complex a)
    atom denom
    denom = a[REAL] * a[REAL] + a[IMAG] * a[IMAG]
    return {a[REAL] / denom, -a[IMAG] / denom}
end function

function neg(complex a)
    return -a
end function

function scomplex(complex a)
    sequence s
    if a[REAL] != 0 then
        s = sprintf("%g",a)
    else
        s = {}
    end if

    if a[IMAG] != 0 then
        if a[IMAG] = 1 then
            s &= "+i"
        elsif a[IMAG] = -1 then
            s &= "-i"
        else
            s &= sprintf("%+gi",a[IMAG])
        end if
    end if

    if length(s) = 0 then
        return "0"
    else
        return s
    end if
end function

complex a, b
a = { 1.0,     1.0 }
b = { 3.14159, 1.2 }
printf(1,"a = %s\n",{scomplex(a)})
printf(1,"b = %s\n",{scomplex(b)})
printf(1,"a+b = %s\n",{scomplex(add(a,b))})
printf(1,"a*b = %s\n",{scomplex(mult(a,b))})
printf(1,"1/a = %s\n",{scomplex(inv(a))})
printf(1,"-a = %s\n",{scomplex(neg(a))})
```


{{out}}

```txt
a = 1+i
b = 3.14159+1.2i
a+b = 4.14159+2.2i
a*b = 1.94159+4.34159i
1/a = 0.5-0.5i
-a = -1-i
```



## Excel

Take 7 cells, say A1 to G1. Type in :

C1:

```excel

=IMSUM(A1;B1)

```


D1:

```excel

=IMPRODUCT(A1;B1)

```


E1:

```excel

=IMSUB(0;D1)

```


F1:

```excel

=IMDIV(1;E28)

```


G1:

```excel

=IMCONJUGATE(C28)

```


E1 will have the negation of D1's value

<lang>
1+2i	3+5i	4+7i	-7+11i	7-11i	0,0411764705882353+0,0647058823529412i	4-7i

```


=={{header|F Sharp|F#}}==
Entered into an interactive session to show the results:

```fsharp

> open Microsoft.FSharp.Math;;

> let a = complex 1.0 1.0;;
val a : complex = 1r+1i

> let b = complex 3.14159 1.25;;
val b : complex = 3.14159r+1.25i

> a + b;;
val it : Complex = 4.14159r+2.25i {Conjugate = 4.14159r-2.25i;
                                   ImaginaryPart = 2.25;
                                   Magnitude = 4.713307515;
                                   Phase = 0.497661247;
                                   RealPart = 4.14159;
                                   i = 2.25;
                                   r = 4.14159;}

> a * b;;
val it : Complex = 1.89159r+4.39159i {Conjugate = 1.89159r-4.39159i;
                                      ImaginaryPart = 4.39159;
                                      Magnitude = 4.781649868;
                                      Phase = 1.164082262;
                                      RealPart = 1.89159;
                                      i = 4.39159;
                                      r = 1.89159;}

> a / b;;
val it : Complex =
  0.384145932435901r+0.165463215905043i
    {Conjugate = 0.384145932435901r-0.165463215905043i;
     ImaginaryPart = 0.1654632159;
     Magnitude = 0.418265673;
     Phase = 0.4067140652;
     RealPart = 0.3841459324;
     i = 0.1654632159;
     r = 0.3841459324;}

> -a;;
val it : complex = -1r-1i {Conjugate = -1r+1i;
                           ImaginaryPart = -1.0;
                           Magnitude = 1.414213562;
                           Phase = -2.35619449;
                           RealPart = -1.0;
                           i = -1.0;
                           r = -1.0;}

```



## Factor


```factor
USING: combinators kernel math math.functions prettyprint ;

C{ 1 2 } C{ 0.9 -2.78 } {
    [ + . ]             ! addition
    [ - . ]             ! subtraction
    [ * . ]             ! multiplication
    [ / . ]             ! division
    [ ^ . ]             ! power
} 2cleave

C{ 1 2 } {
    [ neg . ]           ! negation
    [ 1 swap / . ]      ! multiplicative inverse
    [ conjugate . ]     ! complex conjugate
    [ sin . ]           ! sine
    [ log . ]           ! natural logarithm
    [ sqrt . ]          ! square root
} cleave
```



## Forth

{{libheader|Forth Scientific Library}}
Historically, there was no standard syntax or mechanism for complex numbers and several implementations suitable for different uses were provided. However later a wordset ''was'' standardised as "Algorithm #60".

```forth
S" fsl-util.fs" REQUIRED
S" complex.fs" REQUIRED

zvariable x
zvariable y
1e 1e   x z!
pi 1.2e y z!

x z@ y z@ z+ z.
x z@ y z@ z* z.
1e 0e zconstant 1+0i
1+0i x z@ z/ z.
x z@ znegate z.
```



## Fortran

In ANSI FORTRAN 66 or later, COMPLEX is a built-in data type with full access to intrinsic arithmetic operations. Putting each native operation in a function is horribly inefficient, so I will simply demonstrate the operations. This example shows usage for Fortran 90 or later:

```fortran
program cdemo
    complex :: a = (5,3), b = (0.5, 6.0)      ! complex initializer
    complex :: absum, abprod, aneg, ainv

    absum  = a + b
    abprod = a * b
    aneg   = -a
    ainv   = 1.0 / a
end program cdemo
```


And, although you did not ask, here are demonstrations of some other common complex number operations

```fortran
program cdemo2
    complex :: a = (5,3), b = (0.5, 6)        ! complex initializer
    real, parameter :: pi = 3.141592653589793 ! The constant "pi"
    complex, parameter :: i = (0, 1)          ! the imaginary unit "i" (sqrt(-1))
    complex :: abdiff, abquot, abpow, aconj, p2cart, newc
    real :: areal, aimag, anorm, rho = 10, theta = pi / 3.0, x = 2.3, y = 3.0
    integer, parameter :: n = 50
    integer :: j
    complex, dimension(0:n-1) :: unit_circle

    abdiff = a - b
    abquot = a / b
    abpow  = a ** b
    areal = real(a)               ! Real part
    aimag = imag(a)               ! Imaginary part. Function imag(a) is possibly not recognised. Use aimag(a) if so.
    newc = cmplx(x,y)             ! Creating a complex on the fly from two reals intrinsically
                                  !   (initializer only works in declarations)
    newc = x + y*i                ! Creating a complex on the fly from two reals arithmetically
    anorm = abs(a)                ! Complex norm (or "modulus" or "absolute value")
                                  !   (use CABS before Fortran 90)
    aconj = conjg(a)              ! Complex conjugate (same as real(a) - i*imag(a))
    p2cart = rho * exp(i * theta) ! Euler's polar complex notation to cartesian complex notation
                                  !   conversion (use CEXP before Fortran 90)

    ! The following creates an array of N evenly spaced points around the complex unit circle
    ! useful for FFT calculations, among other things
    unit_circle = exp(2*i*pi/n * (/ (j, j=0, n-1) /) )
end program cdemo2
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type Complex
  As Double real, imag
  Declare Constructor(real As Double, imag As Double)
  Declare Function invert() As Complex
  Declare Function conjugate() As Complex
  Declare Operator cast() As String
End Type

Constructor Complex(real As Double, imag As Double)
  This.real = real
  This.imag = imag
End Constructor

Function Complex.invert() As Complex
  Dim denom As Double = real * real + imag * imag
  Return Complex(real / denom, -imag / denom)
End Function

Function Complex.conjugate() As Complex
  Return Complex(real, -imag)
End Function

Operator Complex.Cast() As String
  If imag >= 0 Then
    Return Str(real) + "+" + Str(imag) + "j"
  End If
  Return Str(real) + Str(imag) + "j"
End Operator

Operator - (c As Complex) As Complex
  Return Complex(-c.real, -c.imag)
End Operator

Operator + (c1 As Complex, c2 As Complex) As Complex
  Return Complex(c1.real + c2.real, c1.imag + c2.imag)
End Operator

Operator - (c1 As Complex, c2 As Complex) As Complex
  Return c1 + (-c2)
End Operator

Operator * (c1 As Complex, c2 As Complex) As Complex
  Return Complex(c1.real * c2.real - c1.imag * c2.imag, c1.real * c2.imag + c2.real * c1.imag)
End Operator

Operator / (c1 As Complex, c2 As Complex) As Complex
  Return c1 * c2.invert
End Operator

Var x = Complex(1, 3)
Var y = Complex(5, 2)
Print "x     = "; x
Print "y     = "; y
Print "x + y = "; x + y
Print "x - y = "; x - y
Print "x * y = "; x * y
Print "x / y = "; x / y
Print "-x    = "; -x
Print "1 / x = "; x.invert
Print "x*    = "; x.conjugate
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

x     = 1+3j
y     = 5+2j
x + y = 6+5j
x - y = -4+1j
x * y = -1+17j
x / y = 0.3793103448275862+0.4482758620689655j
-x    = -1-3j
1 / x = 0.1-0.3j
x*    = 1-3j

```



## Frink

Frink's operations handle complex numbers naturally.  The real and imaginary parts of complex numbers can be arbitrary-sized integers, arbitrary-sized rational numbers, or arbitrary-precision floating-point numbers.

```frink

add[x,y] := x + y
multiply[x,y] := x * y
negate[x] := -x
invert[x] := 1/x  // Could also use inv[x] or recip[x]
conjugate[x] := Re[x] - Im[x] i

a = 3 + 2.5i
b = 7.3 - 10i
println["$a + $b = " + add[a,b]]
println["$a * $b = " + multiply[a,b]]
println["-$a = " + negate[a]]
println["1/$a = " + invert[a]]
println["conjugate[$a] = " + conjugate[a]]

```


{{out}}

```txt

( 3 + 2.5 i ) + ( 7.3 - 10 i ) = ( 10.3 - 7.5 i )
( 3 + 2.5 i ) * ( 7.3 - 10 i ) = ( 46.9 - 11.75 i )
-( 3 + 2.5 i ) = ( -3 - 2.5 i )
1/( 3 + 2.5 i ) = ( 0.19672131147540983607 - 0.16393442622950819672 i )
conjugate[( 3 + 2.5 i )] = ( 3 - 2.5 i )

```



## Futhark

{{incorrect|Futhark|Futhark's syntax has changed, so "fun" should be "let"}}

```Futhark

type complex = (f64,f64)

fun complexAdd((a,b): complex) ((c,d): complex): complex =
  (a + c,
   b + d)

fun complexMult((a,b): complex) ((c,d): complex): complex =
 (a*c - b * d,
  a*d + b * c)

fun complexInv((r,i): complex): complex =
  let denom = r*r + i * i
  in (r / denom,
      -i / denom)

fun complexNeg((r,i): complex): complex =
  (-r, -i)

fun complexConj((r,i): complex): complex =
  (r, -i)

fun main (o: int) (a: complex) (b: complex): complex =
  if      o == 0 then complexAdd a b
  else if o == 1 then complexMult a b
  else if o == 2 then complexInv a
  else if o == 3 then complexNeg a
  else                complexConj a

```



## GAP


```gap
# GAP knows gaussian integers, gaussian rationals (i.e. Q[i]), and cyclotomic fields. Here are some examples.
# E(n) is an nth primitive root of 1
i := Sqrt(-1);
# E(4)
(3 + 2*i)*(5 - 7*i);
# 29-11*E(4)
1/i;
# -E(4)
Sqrt(-3);
# E(3)-E(3)^2

i in GaussianIntegers;
# true
i/2 in GaussianIntegers;
# false
i/2 in GaussianRationals;
# true
Sqrt(-3) in Cyclotomics;
# true
```



## Go

Go has complex numbers built in, with the complex conjugate in the standard library.

```go
package main

import (
    "fmt"
    "math/cmplx"
)

func main() {
    a := 1 + 1i
    b := 3.14159 + 1.25i
    fmt.Println("a:      ", a)
    fmt.Println("b:      ", b)
    fmt.Println("a + b:  ", a+b)
    fmt.Println("a * b:  ", a*b)
    fmt.Println("-a:     ", -a)
    fmt.Println("1 / a:  ", 1/a)
    fmt.Println("a̅:      ", cmplx.Conj(a))
}
```

{{out}}

```txt

a:       (1+1i)
b:       (3.14159+1.25i)
a + b:   (4.14159+2.25i)
a * b:   (1.8915899999999999+4.39159i)
-a:      (-1-1i)
1 / a:   (0.5-0.5i)
a̅:       (1-1i)

```



## Groovy

Groovy does not provide any built-in facility for complex arithmetic. However, it does support arithmetic operator overloading. Thus it is not too hard to build a fairly robust, complete, and intuitive complex number class, such as the following:

```groovy
class Complex {
    final Number real, imag

    static final Complex i = [0,1] as Complex

    Complex(Number r, Number i = 0) { (real, imag) = [r, i] }

    Complex(Map that) { (real, imag) = [that.real ?: 0, that.imag ?: 0] }

    Complex plus (Complex c) { [real + c.real, imag + c.imag] as Complex }
    Complex plus (Number n) { [real + n, imag] as Complex }

    Complex minus (Complex c) { [real - c.real, imag - c.imag] as Complex }
    Complex minus (Number n) { [real - n, imag] as Complex }

    Complex multiply (Complex c) { [real*c.real - imag*c.imag , imag*c.real + real*c.imag] as Complex }
    Complex multiply (Number n) { [real*n , imag*n] as Complex }

    Complex div (Complex c) { this * c.recip() }
    Complex div (Number n) { this * (1/n) }

    Complex negative () { [-real, -imag] as Complex }

    /** the complex conjugate of this complex number. Overloads the bitwise complement (~) operator. */
    Complex bitwiseNegate () { [real, -imag] as Complex }

    /** the magnitude of this complex number. */
    // could also use Math.sqrt( (this * (~this)).real )
    Number getAbs() { Math.sqrt( real*real + imag*imag ) }
    /** the magnitude of this complex number. */
    Number abs() { this.abs }

    /** the reciprocal of this complex number. */
    Complex getRecip() { (~this) / (ρ**2) }
    /** the reciprocal of this complex number. */
    Complex recip() { this.recip }

    /** derived polar angle θ (theta) for polar form. Normalized to 0 ≤ θ < 2π. */
    Number getTheta() {
        def θ = Math.atan2(imag,real)
        θ = θ < 0 ? θ + 2 * Math.PI : θ
    }
    /** derived polar angle θ (theta) for polar form. Normalized to 0 ≤ θ < 2π. */
    Number getΘ() { this.theta } // this is greek uppercase theta

    /** derived polar magnitude ρ (rho) for polar form. */
    Number getRho() { this.abs }
    /** derived polar magnitude ρ (rho) for polar form. */
    Number getΡ() { this.abs } // this is greek uppercase rho, not roman P

    /** Runs Euler's polar-to-Cartesian complex conversion,
     * converting [ρ, θ] inputs into a [real, imag]-based complex number */
    static Complex fromPolar(Number ρ, Number θ) {
        [ρ * Math.cos(θ), ρ * Math.sin(θ)] as Complex
    }

    /** Creates new complex with same magnitude ρ, but different angle θ */
    Complex withTheta(Number θ) { fromPolar(this.rho, θ) }
    /** Creates new complex with same magnitude ρ, but different angle θ */
    Complex withΘ(Number θ) { fromPolar(this.rho, θ) }

    /** Creates new complex with same angle θ, but different magnitude ρ */
    Complex withRho(Number ρ) { fromPolar(ρ, this.θ) }
    /** Creates new complex with same angle θ, but different magnitude ρ */
    Complex withΡ(Number ρ) { fromPolar(ρ, this.θ) } // this is greek uppercase rho, not roman P

    static Complex exp(Complex c) { fromPolar(Math.exp(c.real), c.imag) }

    static Complex log(Complex c) { [Math.log(c.rho), c.theta] as Complex }

    Complex power(Complex c) {
        this == 0 && c != 0  \
                ?  [0] as Complex  \
                :  c == 1  \
                        ?  this  \
                        :  exp( log(this) * c )
    }

    Complex power(Number n) { this ** ([n, 0] as Complex) }

    boolean equals(that) {
        that != null && (that instanceof Complex \
                                ? [this.real, this.imag] == [that.real, that.imag] \
                                : that instanceof Number && [this.real, this.imag] == [that, 0])
    }

    int hashCode() { [real, imag].hashCode() }

    String toString() {
        def realPart = "${real}"
        def imagPart = imag.abs() == 1 ? "i" : "${imag.abs()}i"
        real == 0 && imag == 0 \
                ? "0" \
                : real == 0 \
                        ? (imag > 0 ? '' : "-")  + imagPart \
                        : imag == 0 \
                                ? realPart \
                                : realPart + (imag > 0 ? " + " : " - ")  + imagPart
    }
}
```


The following ''ComplexCategory'' class allows for modification of regular ''Number'' behavior when interacting with ''Complex''.

```groovy
import org.codehaus.groovy.runtime.DefaultGroovyMethods

class ComplexCategory {
    static Complex getI (Number a) { [0, a] as Complex }

    static Complex plus (Number a, Complex b) { b + a }
    static Complex minus (Number a, Complex b) { -b + a }
    static Complex multiply (Number a, Complex b) { b * a }
    static Complex div (Number a, Complex b) { ([a] as Complex) / b  }
    static Complex power (Number a, Complex b) { ([a] as Complex) ** b }

    static <T> T asType (Number a, Class<T> type) {
        type == Complex \
            ? [a] as Complex
            : DefaultGroovyMethods.asType(a, type)
    }
}
```

Notice also that this solution takes liberal advantage of Groovy's full Unicode support, including support for non-English alphabets used in identifiers.

Test Program (mixes the ComplexCategory methods into the Number class):

```groovy
import static Complex.*

Number.metaClass.mixin ComplexCategory

def ε = 0.000000001  // tolerance (epsilon): acceptable "wrongness" to account for rounding error

println 'Demo 1: functionality as requested'
def a = [5,3] as Complex
def a1 = [real:5, imag:3] as Complex
def a2 = 5 + 3.i
def a3 = 5 + 3*i
assert a == a1 && a == a2 && a == a3
println 'a == ' + a
def b = [0.5,6] as Complex
println 'b == ' + b

println "a + b == (${a}) + (${b}) == " + (a + b)
println "a * b == (${a}) * (${b}) == " + (a * b)
assert a + (-a) == 0
println "-a == -(${a}) == " + (-a)
assert (a * a.recip - 1).abs < ε
println "1/a == (${a}).recip == " + (a.recip)
println "a * 1/a == " + (a * a.recip)
println()

println 'Demo 2: other functionality not requested, but important for completeness'
def c = 10
def d = 10 as Complex
assert d instanceof Complex && c instanceof Number && d == c
assert a + c == c + a
println "a + 10 == 10 + a == " + (c + a)
assert c - a == -(a - c)
println "10 - a == -(a - 10) == " + (c - a)
println "a - b == (${a}) - (${b}) == " + (a - b)
assert c * a == a * c
println "10 * a == a * 10 == " + (c * a)
assert (c / a - (a / c).recip).abs < ε
println "10 / a == 1 / (a / 10) == " + (c / a)
println "a / b == (${a}) / (${b}) == " + (a / b)
assert (a ** 2 - a * a).abs < ε
println "a ** 2 == a * a == " + (a ** 2)
println "0.9 ** b == " + (0.9 ** b)
println "a ** b == (${a}) ** (${b}) == " + (a ** b)
println 'a.real == ' + a.real
println 'a.imag == ' + a.imag
println '|a| == ' + a.abs
println 'a.rho == ' + a.rho
println 'a.ρ == ' + a.ρ
println 'a.theta == ' + a.theta
println 'a.θ == ' + a.θ
println '~a (conjugate) == ' + ~a

def ρ = 10
def π = Math.PI
def n = 3
def θ = π / n

def fromPolar1 = fromPolar(ρ, θ)    // direct polar-to-cartesian conversion
def fromPolar2 = exp(θ.i) * ρ       // Euler's equation
println "ρ*cos(θ) + i*ρ*sin(θ) == ${ρ}*cos(π/${n}) + i*${ρ}*sin(π/${n})"
println "                      == 10*0.5      + i*10*√(3/4)    == " + fromPolar1
println "ρ*exp(i*θ)            == ${ρ}*exp(i*π/${n})                == " + fromPolar2
assert (fromPolar1 - fromPolar2).abs < ε
```


{{out}}

```txt
Demo 1: functionality as requested
a == 5 + 3i
b == 0.5 + 6i
a + b == (5 + 3i) + (0.5 + 6i) == 5.5 + 9i
a * b == (5 + 3i) * (0.5 + 6i) == -15.5 + 31.5i
-a == -(5 + 3i) == -5 - 3i
1/a == (5 + 3i).recip == 0.1470588235 - 0.0882352941i
a * 1/a == 0.9999999998

Demo 2: other functionality not requested, but important for completeness
a + 10 == 10 + a == 15 + 3i
10 - a == -(a - 10) == 5 - 3i
a - b == (5 + 3i) - (0.5 + 6i) == 4.5 - 3i
10 * a == a * 10 == 50 + 30i
10 / a == 1 / (a / 10) == 1.4705882350 - 0.8823529410i
a / b == (5 + 3i) / (0.5 + 6i) == 0.5655172413793104 - 0.7862068965517242i
a ** 2 == a * a == 16.000000000000004 + 30.000000000000007i
0.9 ** b == 0.7653514303676113 - 0.5605686291920475i
a ** b == (5 + 3i) ** (0.5 + 6i) == -0.013750112198456855 - 0.09332524760169053i
a.real == 5
a.imag == 3
|a| == 5.830951894845301
a.rho == 5.830951894845301
a.ρ == 5.830951894845301
a.theta == 0.5404195002705842
a.θ == 0.5404195002705842
~a (conjugate) == 5 - 3i
ρ*cos(θ) + i*ρ*sin(θ) == 10*cos(π/3) + i*10*sin(π/3)
                      == 10*0.5      + i*10*√(3/4)    == 5.000000000000001 + 8.660254037844386i
ρ*exp(i*θ)            == 10*exp(i*π/3)                == 5.000000000000001 + 8.660254037844386i
```



## Haskell


Complex numbers are parameterized in their base type, so you can
have ''Complex Integer'' for the Gaussian Integers, ''Complex Float'', ''Complex Double'', etc. The operations are just the usual overloaded numeric operations.


```haskell
import Data.Complex

main = do
  let a = 1.0 :+ 2.0    -- complex number 1+2i
  let b = 4             -- complex number 4+0i
  -- 'b' is inferred to be complex because it's used in
  -- arithmetic with 'a' below.
  putStrLn $ "Add:      " ++ show (a + b)
  putStrLn $ "Subtract: " ++ show (a - b)
  putStrLn $ "Multiply: " ++ show (a * b)
  putStrLn $ "Divide:   " ++ show (a / b)
  putStrLn $ "Negate:   " ++ show (-a)
  putStrLn $ "Inverse:  " ++ show (recip a)
  putStrLn $ "Conjugate:" ++ show (conjugate a)
```


{{out}}

```txt
*Main> main
Add:      5.0 :+ 2.0
Subtract: (-3.0) :+ 2.0
Multiply: 4.0 :+ 8.0
Divide:   0.25 :+ 0.5
Negate:   (-1.0) :+ (-2.0)
Inverse:  0.2 :+ (-0.4)
Conjugate:1.0 :+ (-2.0)
```


{{improve|Unicon|This could be better implemented as an object i n Unicon. Note, however, that  Unicon doesn't allow for operator overloading at the current time.}}

=={{header|Icon}} and {{header|Unicon}}==
Icon doesn't provide native support for complex numbers.  Support is included in the IPL.

```Icon
procedure main()

SetupComplex()
a := complex(1,2)
b := complex(3,4)

c := complex(&pi,1.5)
d := complex(1)
e := complex(,1)

every v := !"abcde" do write(v," := ",cpxstr(variable(v)))

write("a+b := ", cpxstr(cpxadd(a,b)))
write("a-b := ", cpxstr(cpxsub(a,b)))
write("a*b := ", cpxstr(cpxmul(a,b)))
write("a/b := ", cpxstr(cpxdiv(a,b)))
write("neg(a) := ", cpxstr(cpxneg(a)))
write("inv(a) := ", cpxstr(cpxinv(a)))
write("conj(a) := ", cpxstr(cpxconj(a)))
write("abs(a) := ", cpxabs(a))
write("neg(1) := ", cpxstr(cpxneg(1)))
end
```

Icon doesn't allow for operator overloading but procedures can be overloaded as was done here to allow 'complex' to behave more robustly.

{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/complex.icn provides complex number support] supplemented by the code below.

```Icon

link complex                            # for complex number support

procedure SetupComplex()                #: used to setup safe complex
COMPLEX()				#  replace complex record constructor
SetupComplex := 1                       #  never call here again
return
end

procedure COMPLEX(rpart,ipart)          #: new safe record constructor and coercion
initial complex :=: COMPLEX             # get in front of record constructor
return if /ipart & (type(rpart) == "complex")
   then rpart                           #                  already complex
   else COMPLEX( real(\rpart | 0.0), real(\ipart|0) )    # create a new complex number
end

procedure cpxneg(z)                     #: negate z
   z := complex(z)                      # coerce
   return complex( -z.rpart, -z.ipart)
end

procedure cpxinv(z)                     #: inverse of z
   local denom
   z := complex(z)                      # coerce

   denom := z.rpart ^ 2 + z.ipart ^ 2
   return complex(z.rpart / denom, z.ipart / denom)
end
```

To take full advantage of the overloaded 'complex' procedure,
the other cpxxxx procedures would need to be rewritten or overloaded.

{{out}}

```txt
#complexdemo.exe

a := (1.0+2.0i)
b := (3.0+4.0i)
c := (3.141592653589793+1.5i)
d := (1.0+0.0i)
e := (0.0+1.0i)
a+b := (4.0+6.0i)
a-b := (-2.0-2.0i)
a*b := (-5.0+10.0i)
a/b := (0.44+0.08i)
neg(a) := (-1.0-2.0i)
inv(a) := (0.2+0.4i)
conj(a) := (1.0-2.0i)
abs(a) := 2.23606797749979
neg(1) := (-1.0+0.0i)
```



## IDL


<tt>complex</tt> (and <tt>dcomplex</tt> for double-precision) is a built-in data type in IDL:


```idl
x=complex(1,1)
 y=complex(!pi,1.2)
 print,x+y
(      4.14159,      2.20000)
 print,x*y
(      1.94159,     4.34159)
 print,-x
(     -1.00000,     -1.00000)
 print,1/x
(     0.500000,    -0.500000)
```



## J

Complex numbers are a native numeric data type in J. Although the examples shown here are performed on scalars, all numeric operations naturally apply to arrays of complex numbers.

```j
   x=: 1j1
   y=: 3.14159j1.2
   x+y            NB. addition
4.14159j2.2
   x*y            NB. multiplication
1.94159j4.34159
   %x             NB. inversion
0.5j_0.5
   -x             NB. negation
_1j_1
   +x             NB. (complex) conjugation
1j_1

```



## Java


```java
public class Complex {
    public final double real;
    public final double imag;

    public Complex() {
        this(0, 0);
    }

    public Complex(double r, double i) {
        real = r;
        imag = i;
    }

    public Complex add(Complex b) {
        return new Complex(this.real + b.real, this.imag + b.imag);
    }

    public Complex mult(Complex b) {
        // FOIL of (a+bi)(c+di) with i*i = -1
        return new Complex(this.real * b.real - this.imag * b.imag,
                this.real * b.imag + this.imag * b.real);
    }

    public Complex inv() {
        // 1/(a+bi) * (a-bi)/(a-bi) = 1/(a+bi) but it's more workable
        double denom = real * real + imag * imag;
        return new Complex(real / denom, -imag / denom);
    }

    public Complex neg() {
        return new Complex(-real, -imag);
    }

    public Complex conj() {
        return new Complex(real, -imag);
    }

    @Override
    public String toString() {
        return real + " + " + imag + " * i";
    }

    public static void main(String[] args) {
        Complex a = new Complex(Math.PI, -5); //just some numbers
        Complex b = new Complex(-1, 2.5);
        System.out.println(a.neg());
        System.out.println(a.add(b));
        System.out.println(a.inv());
        System.out.println(a.mult(b));
        System.out.println(a.conj());
    }
}
```



## JavaScript


```javascript
function Complex(r, i) {
	this.r = r;
	this.i = i;
}

Complex.add = function() {
	var num = arguments[0];

	for(var i = 1, ilim = arguments.length; i < ilim; i += 1){
		num.r += arguments[i].r;
		num.i += arguments[i].i;
	}

	return num;
}

Complex.multiply = function() {
	var num = arguments[0];

	for(var i = 1, ilim = arguments.length; i < ilim; i += 1){
		num.r = (num.r * arguments[i].r) - (num.i * arguments[i].i);
		num.i = (num.i * arguments[i].r) - (num.r * arguments[i].i);
	}

	return num;
}

Complex.negate = function (z) {
	return new Complex(-1*z.r, -1*z.i);
}

Complex.invert = function(z) {
	var denom = Math.pow(z.r,2) + Math.pow(z.i,2);
	return new Complex(z.r/denom, -1*z.i/denom);
}

Complex.conjugate = function(z) {
	return new Complex(z.r, -1*z.i);
}

// BONUSES!


Complex.prototype.toString = function() {
	return this.r === 0 && this.i === 0
          ? "0"
          : (this.r !== 0 ? this.r : "")
          + ((this.r !== 0 || this.i < 0) && this.i !== 0
              ? (this.i > 0 ? "+" : "-")
              : "" ) + ( this.i !== 0 ? Math.abs(this.i) + "i" : "" );
}

Complex.prototype.getMod = function() {
	return Math.sqrt( Math.pow(this.r,2) , Math.pow(this.i,2) )
}
```



## jq

For speed and for conformance with the complex plane interpretation, x+iy is represented as [x,y]; for flexibility, all the functions defined here will accept both real and complex numbers; and for uniformity, they are implemented as functions that ignore their input.

Recent versions of jq support modules, so these functions could all be placed in a module to avoid name conflicts, and thus no special prefix is used here.
```jq
def real(z): if (z|type) == "number" then z else z[0] end;

def imag(z): if (z|type) == "number" then 0 else z[1] end;

def plus(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x+y, 0 ]
       else [ x + y[0], y[1]]
       end
    elif (y|type) == "number" then plus(y;x)
    else [ x[0] + y[0], x[1] + y[1] ]
    end;

def multiply(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x*y, 0 ]
       else [x * y[0], x * y[1]]
       end
    elif (y|type) == "number" then multiply(y;x)
    else [ x[0] * y[0] - x[1] * y[1],
           x[0] * y[1] + x[1] * y[0]]
    end;

def negate(x): multiply(-1; x);

def minus(x; y): plus(x; multiply(-1; y));

def conjugate(z):
  if (z|type) == "number" then [z, 0]
  else  [z[0], -(z[1]) ]
  end;

def invert(z):
  if (z|type) == "number" then [1/z, 0]
  else
    ( (z[0] * z[0]) + (z[1] * z[1]) ) as $d
   # use "0 + ." to convert -0 back to 0
    | [ z[0]/$d, (0 + -(z[1]) / $d)]
  end;

def divide(x;y): multiply(x; invert(y));

def exp(z):
  def expi(x): [ (x|cos), (x|sin) ];
  if (z|type) == "number" then z|exp
  elif z[0] == 0 then expi(z[1])  # for efficiency
  else multiply( (z[0]|exp); expi(z[1]) )
  end ;

def test(x;y):
  "x =      \( x )",
  "y =      \( y )",
  "x+y:     \( plus(x;y))",
  "x*y:     \( multiply(x;y))",
  "-x:      \( negate(x))",
  "1/x:     \( invert(x))",
  "conj(x): \( conjugate(x))",
  "(x/y)*y: \( multiply( divide(x;y) ; y) )",
  "e^iπ:    \( exp( [0, 4 * (1|atan)  ] ) )"
;

test( [1,1]; [0,1] )
```

{{Out}}

```jq
$ jq -n -f complex.jq
"x =      [1,1]"
"y =      [0,1]"
"x+y:     [1,2]"
"x*y:     [-1,1]"
"-x:      [-1,-1]"
"1/x:     [0.5,-0.5]"
"conj(x): [1,-1]"
"(x/y)*y: [1,1]"
"e^iπ:    [-1,1.2246467991473532e-16]"
```



## Julia

Julia has built-in support for complex arithmetic with arbitrary real types.

```lb>julia
 z1 = 1.5 + 3im
julia> z2 = 1.5 + 1.5im
julia> z1 + z2
3.0 + 4.5im
julia> z1 - z2
0.0 + 1.5im
julia> z1 * z2
-2.25 + 6.75im
julia> z1 / z2
1.5 + 0.5im
julia> - z1
-1.5 - 3.0im
julia> conj(z1), z1'   # two ways to conjugate
(1.5 - 3.0im,1.5 - 3.0im)
julia> abs(z1)
3.3541019662496847
julia> z1^z2
-1.102482955327779 - 0.38306415117199305im
julia> real(z1)
1.5
julia> imag(z1)
3.0
```



## Kotlin


```scala
class Complex(private val real: Double, private val imag: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imag + other.imag)

    operator fun times(other: Complex) = Complex(
        real * other.real - imag * other.imag,
        real * other.imag + imag * other.real
    )

    fun inv(): Complex {
        val denom = real * real + imag * imag
        return Complex(real / denom, -imag / denom)
    }

    operator fun unaryMinus() = Complex(-real, -imag)

    operator fun minus(other: Complex) = this + (-other)

    operator fun div(other: Complex) = this * other.inv()

    fun conj() = Complex(real, -imag)

    override fun toString() =
        if (imag >= 0.0) "$real + ${imag}i"
        else "$real - ${-imag}i"
}

fun main(args: Array<String>) {
    val x = Complex(1.0, 3.0)
    val y = Complex(5.0, 2.0)
    println("x     =  $x")
    println("y     =  $y")
    println("x + y =  ${x + y}")
    println("x - y =  ${x - y}")
    println("x * y =  ${x * y}")
    println("x / y =  ${x / y}")
    println("-x    =  ${-x}")
    println("1 / x =  ${x.inv()}")
    println("x*    =  ${x.conj()}")
}
```


{{out}}

```txt

x     =  1.0 + 3.0i
y     =  5.0 + 2.0i
x + y =  6.0 + 5.0i
x - y =  -4.0 + 1.0i
x * y =  -1.0 + 17.0i
x / y =  0.3793103448275862 + 0.4482758620689655i
-x    =  -1.0 - 3.0i
1 / x =  0.1 - 0.3i
x*    =  1.0 - 3.0i

```



## LFE


There is no native support for complex numbers in either LFE or Erlang. As such, this example shows how to implement complex support. There is, however, an LFE library that offers a complex number data type and many mathematical functions which support this data type: [https://github.com/lfex/complex complex].

A convenient data structure for a complex number is the record:

```lisp

(defrecord complex
  real
  img)

```


Here are the required functions:


```lisp

(defun add
  (((match-complex real r1 img i1)
    (match-complex real r2 img i2))
   (new (+ r1 r2) (+ i1 i2))))

(defun mult
  (((match-complex real r1 img i1)
    (match-complex real r2 img i2))
   (new (- (* r1 r2) (* i1 i2))
              (+ (* r1 i2) (* r2 i1)))))

(defun neg
  (((match-complex real r img i))
   (new (* -1 r) (* -1 i))))

(defun inv (cmplx)
  (div (conj cmplx) (modulus cmplx)))

```


Bonus:


```lisp

(defun conj
  (((match-complex real r img i))
   (new r (* -1 i))))

```


The functions above are built using the following supporting functions:


```lisp

(defun new (r i)
  (make-complex real r img i))

(defun modulus (cmplx)
  (mult cmplx (conj cmplx)))

(defun div (c1 c2)
   (let* ((denom (complex-real (modulus c2)))
          (c3 (mult c1 (conj c2))))
     (new (/ (complex-real c3) denom)
          (/ (complex-img c3) denom)))))


```


Finally, we have some functions for use in the conversion and display of our complex number data structure:


```lisp

(defun ->str
  (((match-complex real r img i)) (when (>= i 0))
   (->str r i "+"))
  (((match-complex real r img i))
   (->str r i "")))

(defun ->str (r i pos)
  (io_lib:format "~p ~s~pi" `(,r ,pos ,i)))

(defun print (cmplx)
  (io:format (++ (->str cmplx) "~n")))

```


Usage is as follows:


```txt

> (set ans1 (add c1 c2))
#(complex 2.5 4.0)
> (set ans2 (mult c1 c2))
#(complex -1.5 4.5)
> (set ans3 (inv c2))
#(complex 0.5 -0.5)
> (set ans4 (conj c1))
#(complex 1.5 -3.0)

```


These can be printed in the following manner:


```txt

> (progn (lists:map #'print/1 `(,ans1 ,ans2 ,ans3 ,ans4)) 'ok)
2.5 +4.0i
-1.5 +4.5i
0.5 -0.5i
1.5 -3.0i
ok

```



## Liberty BASIC


```lb
mainwin 50 10

print " Adding"
call cprint cadd$(   complex$( 1, 1), complex$( 3.14159265, 1.2))
print " Multiplying"
call cprint cmulti$( complex$( 1, 1), complex$( 3.14159265, 1.2))
print " Inverting"
call cprint cinv$(   complex$( 1, 1))
print " Negating"
call cprint cneg$(   complex$( 1, 1))

end

sub cprint cx$
    print "( "; word$( cx$, 1); " + i *"; word$( cx$, 2); ")"
end sub

function complex$( a , bj )
''complex number string-object constructor
  complex$ = str$( a ) ; " " ; str$( bj )
end function

function cadd$( a$ , b$ )
  ar = val( word$( a$ , 1 ) )
  ai = val( word$( a$ , 2 ) )
  br = val( word$( b$ , 1 ) )
  bi = val( word$( b$ , 2 ) )
  cadd$ = complex$( ar + br , ai + bi )
end function

function cmulti$( a$ , b$ )
  ar = val( word$( a$ , 1 ) )
  ai = val( word$( a$ , 2 ) )
  br = val( word$( b$ , 1 ) )
  bi = val( word$( b$ , 2 ) )
  cmulti$ = complex$( ar * br - ai * bi _
                    , ar * bi + ai * br )
end function

function cneg$( a$)
  ar = val( word$( a$ , 1 ) )
  ai = val( word$( a$ , 2 ) )
  cneg$ =complex$( 0 -ar, 0 -ai)
end function

function cinv$( a$)
  ar = val( word$( a$ , 1 ) )
  ai = val( word$( a$ , 2 ) )
  D =ar^2 +ai^2
  cinv$ =complex$( ar /D , 0 -ai /D )
end function
```



## Lua


```lua


--defines addition, subtraction, negation, multiplication, division, conjugation, norms, and a conversion to strgs.
complex = setmetatable({
__add = function(u, v) return complex(u.real + v.real, u.imag + v.imag) end,
__sub = function(u, v) return complex(u.real - v.real, u.imag - v.imag) end,
__mul = function(u, v) return complex(u.real * v.real - u.imag * v.imag, u.real * v.imag + u.imag * v.real) end,
__div = function(u, v) return u * complex(v.real / v.norm, -v.imag / v.norm) end,
__unm = function(u) return complex(-u.real, -u.imag) end,
__concat = function(u, v)
    if type(u) == "table" then return u.real .. " + " .. u.imag .. "i" .. v
	elseif type(u) == "string" or type(u) == "number" then return u .. v.real .. " + " .. v.imag .. "i"
	end end,
__index = function(u, index)
  local operations = {
    norm = function(u) return u.real ^ 2 + u.imag ^ 2 end,
    conj = function(u) return complex(u.real, -u.imag) end,
  }
  return operations[index] and operations[index](u)
end,
__newindex = function() error() end
}, {
__call = function(z, realpart, imagpart) return setmetatable({real = realpart, imag = imagpart}, complex) end
} )

local i, j = complex(2, 3), complex(1, 1)

print(i .. " + " .. j .. " = " .. (i+j))
print(i .. " - " .. j .. " = " .. (i-j))
print(i .. " * " .. j .. " = " .. (i*j))
print(i .. " / " .. j .. " = " .. (i/j))
print("|" .. i .. "| = " .. math.sqrt(i.norm))
print(i .. "* = " .. i.conj)

```



## Maple


Maple has <code>I</code> (the square root of -1) built-in. Thus:


```maple
x := 1+I;
y := Pi+I*1.2;
```


By itself, it will perform mathematical operations symbolically, i.e. it will not try to perform computational evaluation unless specifically asked to do so. Thus:


```maple
x*y;
    ==> (1 + I) (Pi + 1.2 I)
simplify(x*y);
    ==> 1.941592654 + 4.341592654 I
```


Other than that, the task merely asks for


```maple
x+y;
x*y;
-x;
1/x;
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica has fully implemented support for complex numbers throughout the software. Addition, subtraction, division, multiplications and powering need no further syntax than for real numbers:

```Mathematica
x=1+2I
y=3+4I

x+y  =>  4 + 6 I
x-y  =>  -2 - 2 I
y x  =>  -5 + 10 I
y/x  => 11/5 - (2 I)/5
x^3  =>  -11 - 2 I
y^4  =>  -527 - 336 I
x^y  =>  (1 + 2 I)^(3 + 4 I)
N[x^y]  =>  0.12901 + 0.0339241 I
```

Powering to a complex power can in general not be written shorter, so Mathematica leaves it unevaluated if the numbers are exact. An approximation can be acquired using the function N.
However Mathematica goes much further, basically all functions can handle complex numbers to arbitrary precision, including (but not limited to!):

```Mathematica
Exp  Log
Sin  Cos  Tan  Csc  Sec  Cot
ArcSin  ArcCos  ArcTan  ArcCsc  ArcSec  ArcCot
Sinh  Cosh  Tanh  Csch  Sech  Coth
ArcSinh  ArcCosh  ArcTanh  ArcCsch  ArcSech  ArcCoth
Sinc
Haversine  InverseHaversine
Factorial  Gamma  PolyGamma  LogGamma
Erf  BarnesG  Hyperfactorial  Zeta  ProductLog  RamanujanTauL
```

and many many more. The documentation states:

''Mathematica has fundamental support for both explicit complex numbers and symbolic complex variables. All applicable mathematical functions support arbitrary-precision evaluation for complex values of all parameters, and symbolic operations automatically treat complex variables with full generality.''


## MATLAB

Complex numbers are a primitive data type in MATLAB. All the typical complex operations can be performed. There are two keywords that specify a number as complex: "i" and "j".


```MATLAB>>
 a = 1+i

a =

  1.000000000000000 + 1.000000000000000i

>> b = 3+7i

b =

  3.000000000000000 + 7.000000000000000i

>> a+b

ans =

  4.000000000000000 + 8.000000000000000i

>> a-b

ans =

 -2.000000000000000 - 6.000000000000000i

>> a*b

ans =

 -4.000000000000000 +10.000000000000000i

>> a/b

ans =

  0.172413793103448 - 0.068965517241379i

>> -a

ans =

 -1.000000000000000 - 1.000000000000000i

>> a'

ans =

  1.000000000000000 - 1.000000000000000i

>> a^b

ans =

  0.000808197112874 - 0.011556516327187i

>> norm(a)

ans =

   1.414213562373095
```



## Maxima


```maxima
z1: 5 + 2 * %i;
2*%i+5

z2: 3 - 7 * %i;
3-7*%i

carg(z1);
atan(2/5)

cabs(z1);
sqrt(29)

rectform(z1 * z2);
29-29*%i

polarform(z1);
sqrt(29)*%e^(%i*atan(2/5))

conjugate(z1);
5-2*%i

z1 + z2;
8-5*%i

z1 - z2;
9*%i+2

z1 * z2;
(3-7*%i)*(2*%i+5)

z1 * z2, rectform;
29-29*%i

z1 / z2;
(2*%i+5)/(3-7*%i)

z1 / z2, rectform;
(41*%i)/58+1/58

realpart(z1);
5

imagpart(z1);
2
```


=={{header|MK-61/52}}==
''Instrustion:''

Z<sub>1</sub> = a + ib; Z<sub>2</sub> = c + id;

a	С/П	b	С/П	c	С/П	d	С/П

Division: С/П; multiplication: БП 36 С/П; addition: БП 54 С/П; subtraction: БП 63 С/П.

<lang>ПA	С/П	ПB	С/П	ПC	С/П	ПD	С/П	ИПC	x^2
ИПD	x^2	+	П3	ИПA	ИПC	*	ИПB	ИПD	*
+	ИП3	/	П1	ИПB	ИПC	*	ИПA	ИПD	*
-	ИП3	/	П2	ИП1	С/П	ИПA	ИПC	*	ИПB
ИПD	*	-	П1	ИПB	ИПC	*	ИПA	ИПD	*
+	П2	ИП1	С/П	ИПB	ИПD	+	П2	ИПA	ИПC
+	ИП1	С/П	ИПB	ИПD	-	П2	ИПA	ИПC	-
П1	С/П
```


=={{header|Modula-2}}==

```modula2
MODULE complex;

IMPORT  InOut;

TYPE    Complex         = RECORD   R, Im    : REAL    END;

VAR     z               : ARRAY [0..3] OF Complex;

PROCEDURE ShowComplex (str  : ARRAY OF CHAR;  p  : Complex);

BEGIN
  InOut.WriteString (str);              InOut.WriteString (" = ");
  InOut.WriteReal (p.R, 6, 2);
  IF  p.Im >= 0.0  THEN  InOut.WriteString (" + ")  ELSE  InOut.WriteString (" - ")  END;
  InOut.WriteReal (ABS (p.Im), 6, 2);   InOut.WriteString (" i ");
  InOut.WriteLn;                        InOut.WriteBf
END ShowComplex;

PROCEDURE AddComplex (x1, x2 : Complex; VAR x3  : Complex);

BEGIN
  x3.R  := x1.R  + x2.R;
  x3.Im := x1.Im + x2.Im
END AddComplex;

PROCEDURE SubComplex (x1, x2 : Complex; VAR x3  : Complex);

BEGIN
  x3.R := x1.R - x2.R;
  x3.Im := x1.Im - x2.Im
END SubComplex;

PROCEDURE MulComplex (x1, x2  : Complex; VAR x3  : Complex);

BEGIN
  x3.R := x1.R * x2.R - x1.Im * x2.Im;
  x3.Im := x1.R * x2.Im + x1.Im * x2.R
END MulComplex;

PROCEDURE InvComplex (x1 : Complex; VAR x2  : Complex);

BEGIN
  x2.R := x1.R / (x1.R * x1.R + x1.Im * x1.Im);
  x2.Im := -1.0 * x1.Im / (x1.R * x1.R + x1.Im * x1.Im)
END InvComplex;

PROCEDURE NegComplex (x1 : Complex; VAR x2  : Complex);

BEGIN
  x2.R := - x1.R;       x2.Im := - x1.Im
END NegComplex;

BEGIN
  InOut.WriteString ("Enter two complex numbers : ");
  InOut.WriteBf;
  InOut.ReadReal (z[0].R);              InOut.ReadReal (z[0].Im);
  InOut.ReadReal (z[1].R);              InOut.ReadReal (z[1].Im);
  ShowComplex ("z1", z[0]);             ShowComplex ("z2", z[1]);
  InOut.WriteLn;
  AddComplex (z[0], z[1], z[2]);        ShowComplex ("z1 + z2", z[2]);
  SubComplex (z[0], z[1], z[2]);        ShowComplex ("z1 - z2", z[2]);
  MulComplex (z[0], z[1], z[2]);        ShowComplex ("z1 * z2", z[2]);
  InvComplex (z[0], z[2]);              ShowComplex ("1  / z1", z[2]);
  NegComplex (z[0], z[2]);              ShowComplex ("   - z1", z[2]);
  InOut.WriteLn
END complex.
```

{{out}}

```txt
Enter two complex numbers : 5 3 0.5 6
z1 =   5.00 +   3.00 i
z2 =   0.50 +   6.00 i

z1 + z2 =   5.50 +   9.00 i
z1 - z2 =   4.50 -   3.00 i
z1 * z2 = -15.50 +  31.50 i
1  / z1 =   0.15 -   0.09 i
   - z1 =  -5.00 -   3.00 i
```



## Nemerle


```Nemerle
using System;
using System.Console;
using System.Numerics;
using System.Numerics.Complex;

module RCComplex
{
    PrettyPrint(this c : Complex) : string
    {
        mutable sign = '+';
        when (c.Imaginary < 0) sign = '-';
        $"$(c.Real) $sign $(Math.Abs(c.Imaginary))i"
    }

    Main() : void
    {
        def complex1 = Complex(1.0, 1.0);
        def complex2 = Complex(3.14159, 1.2);

        WriteLine(Add(complex1, complex2).PrettyPrint());
        WriteLine(Multiply(complex1, complex2).PrettyPrint());
        WriteLine(Negate(complex2).PrettyPrint());
        WriteLine(Reciprocal(complex2).PrettyPrint());
        WriteLine(Conjugate(complex2).PrettyPrint());
    }
}
```

{{out}}

```txt
4.14159 + 2.2i
1.94159 + 4.34159i
-3.14159 - 1.2i
0.277781124787984 - 0.106104663481097i
3.14159 - 1.2i
```



## Nim


```nim

import complex
var a: Complex = (1.0,1.0)
var b: Complex = (3.1415,1.2)

echo("a    : " & $a)
echo("b    : " & $b)
echo("a + b: " & $(a + b))
echo("a * b: " & $(a * b))
echo("1/a  : " & $(1/a))
echo("-a   : " & $(-a))


```

{{out}}

```txt

a    : (1.0000000000000000e+00, 1.0000000000000000e+00)
b    : (3.1415000000000002e+00, 1.2000000000000000e+00)
a + b: (4.1415000000000006e+00, 2.2000000000000002e+00)
a * b: (1.9415000000000002e+00, 4.3414999999999999e+00)
1/a  : (5.0000000000000000e-01, -5.0000000000000000e-01)
-a   : (-1.0000000000000000e+00, -1.0000000000000000e+00)

```


=={{header|Oberon-2}}==
Oxford Oberon Compiler

```oberon2

MODULE Complex;
IMPORT Files,Out;
TYPE
        Complex* = POINTER TO ComplexDesc;
        ComplexDesc = RECORD
                r-,i-: REAL;
        END;

PROCEDURE (CONST x: Complex) Add*(CONST y: Complex): Complex;
BEGIN
        RETURN New(x.r + y.r,x.i + y.i)
END Add;

PROCEDURE (CONST x: Complex) Sub*(CONST y: Complex): Complex;
BEGIN
        RETURN New(x.r - y.r,x.i - y.i)
END Sub;

PROCEDURE (CONST x: Complex) Mul*(CONST y: Complex): Complex;
BEGIN
        RETURN New(x.r*y.r - x.i*y.i,x.r*y.i + x.i*y.r)
END Mul;

PROCEDURE (CONST x: Complex) Div*(CONST y: Complex): Complex;
VAR
        d: REAL;
BEGIN
        d := y.r * y.r + y.i * y.i;
        RETURN New((x.r*y.r + x.i*y.i)/d,(x.i*y.r - x.r*y.i)/d)
END Div;

(* Reciprocal *)
PROCEDURE (CONST x: Complex) Rec*(): Complex;
VAR
        d: REAL;
BEGIN
        d := x.r * x.r + y.i * y.i;
        RETURN New(x.r/d,(-1.0 * x.i)/d);
END Rec;

(* Conjugate *)
PROCEDURE (x: Complex) Con*(): Complex;
BEGIN
        RETURN New(x.r, (-1.0) * x.i);
END Con;

PROCEDURE (x: Complex) Out(out : Files.File);
BEGIN
        Files.WriteString(out,"(");
        Files.WriteReal(out,x.r);
        Files.WriteString(out,",");
        Files.WriteReal(out,x.i);
        Files.WriteString(out,"i)")
END Out;

PROCEDURE New(x,y: REAL): Complex;
VAR
        r: Complex;
BEGIN
        NEW(r);r.r := x;r.i := y;
        RETURN r
END New;

VAR
        r,x,y: Complex;
BEGIN
        x := New(1.5,3);
        y := New(1.0,1.0);

        Out.String("x: ");x.Out(Files.stdout);Out.Ln;
        Out.String("y: ");y.Out(Files.stdout);Out.Ln;
        r := x.Add(y);
        Out.String("x + y: ");r.Out(Files.stdout);Out.Ln;
        r := x.Sub(y);
        Out.String("x - y: ");r.Out(Files.stdout);Out.Ln;
        r := x.Mul(y);
        Out.String("x * y: ");r.Out(Files.stdout);Out.Ln;
        r := x.Div(y);
        Out.String("x / y: ");r.Out(Files.stdout);Out.Ln;
        r := y.Rec();
        Out.String("1 / y: ");r.Out(Files.stdout);Out.Ln;
        r := x.Con();
        Out.String("x': ");r.Out(Files.stdout);Out.Ln;

END Complex.

```

{{out}}

```txt

x: (1.50000,3.00000i)
y: (1.00000,1.00000i)
x + y: (2.50000,4.00000i)
x - y: (0.500000,2.00000i)
x * y: (-1.50000,4.50000i)
x / y: (2.25000,0.750000i)
1 / y: (0.500000,-0.500000i)
x': (1.50000,-3.00000i)

```



## OCaml

The [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Complex.html Complex] module from the standard library provides the functionality of complex numbers:

```ocaml
open Complex

let print_complex z =
  Printf.printf "%f + %f i\n" z.re z.im

let () =
  let a = { re = 1.0; im = 1.0 }
  and b = { re = 3.14159; im = 1.25 } in
  print_complex (add a b);
  print_complex (mul a b);
  print_complex (inv a);
  print_complex (neg a);
  print_complex (conj a)
```


Using [http://forge.ocamlcore.org/projects/pa-do/ Delimited Overloading], the syntax can be made closer to the usual one:

```ocaml
let () =
  Complex.(
    let print txt z = Printf.printf "%s = %s\n" txt (to_string z) in
    let a = 1 + I
    and b = 3 + 7I in
    print "a + b" (a + b);
    print "a - b" (a - b);
    print "a * b" (a * b);
    print "a / b" (a / b);
    print "-a" (- a);
    print "conj a" (conj a);
    print "a^b" (a**b);
    Printf.printf "norm a = %g\n" (float(abs a));
  )
```



## Octave

GNU Octave handles naturally complex numbers:

```octave
z1 = 1.5 + 3i;
z2 = 1.5 + 1.5i;
disp(z1 + z2);    % 3.0 + 4.5i
disp(z1 - z2);    % 0.0 + 1.5i
disp(z1 * z2);    % -2.25 + 6.75i
disp(z1 / z2);    % 1.5 + 0.5i
disp(-z1);        % -1.5 - 3i
disp(z1');        % 1.5 - 3i
disp(abs(z1));    % 3.3541 = sqrt(z1*z1')
disp(z1 ^ z2);    % -1.10248 - 0.38306i
disp( exp(z1) );  % -4.43684 + 0.63246i
disp( imag(z1) ); % 3
disp( real(z2) ); % 1.5
%...
```



## Oforth



```Oforth
Object Class new: Complex(re, im)

Complex method: re  @re ;
Complex method: im  @im ;

Complex method: initialize   := im := re ;
Complex method: <<  '(' <<c @re << ',' <<c @im << ')' <<c  ;

0 1 Complex new const: I

Complex method: ==(c -- b )
    c re @re == c im @im == and ;

Complex method: norm -- f
    @re sq @im sq + sqrt ;

Complex method: conj -- c
    @re @im neg Complex new ;

Complex method: +(c -- d )
    c re @re +  c im @im + Complex new ;

Complex method: -(c -- d )
    c re @re -  c im @im - Complex new ;

Complex method: *(c -- d)
    c re @re * c im @im * -  c re @im * @re c im * + Complex new ;

Complex method: inv
| n |
   @re sq @im sq + >float ->n
   @re n /   @im neg n / Complex new
;

Complex method: /( c -- d )
   c self inv * ;

Integer method: >complex  self 0 Complex new ;
Float   method: >complex  self 0 Complex new ;
```


Usage :


```Oforth>3.2
complex I * 2 >complex + .cr
2 3 Complex new  1.2 >complex + .cr
2 3 Complex new  1.2 >complex * .cr
2 >complex  2 3 Complex new / .cr
```


{{out}}

```txt

(2,3.2)
(3.2,3)
(2.4,3.6)
(0.307692307692308,-0.461538461538462)

```



## Ol

Ol supports complex numbers by default. Numbers must be entered manually in form A+Bi without spaces between elements, where A and B - numbers (can be rational), i - imaginary unit; or in functional form using function `complex`.


```scheme

(define A 0+1i) ; manually entered numbers
(define B 1+0i)

(print (+ A B))
; <== 1+i

(print (- A B))
; <== -1+i

(print (* A B))
; <== 0+i

(print (/ A B))
; <== 0+i


(define C (complex 2/7 -3)) ; functional way

(print "real part of " C " is " (car C))
; <== real part of 2/7-3i is 2/7

(print "imaginary part of " C " is " (cdr C))
; <== imaginary part of 2/7-3i is -3

```



## ooRexx


```ooRexx
c1 = .complex~new(1, 2)
c2 = .complex~new(3, 4)
r = 7

say "c1           =" c1
say "c2           =" c2
say "r            =" r
say "-c1          =" (-c1)
say "c1 + r       =" c1 + r
say "c1 + c2      =" c1 + c2
say "c1 - r       =" c1 - r
say "c1 - c2      =" c1 - c2
say "c1 * r       =" c1 * r
say "c1 * c2      =" c1 * c2
say "inv(c1)      =" c1~inv
say "conj(c1)     =" c1~conjugate
say "c1 / r       =" c1 / r
say "c1 / c2      =" c1 / c2
say "c1 == c1     =" (c1 == c1)
say "c1 == c2     =" (c1 == c2)


::class complex
::method init
  expose r i
  use strict arg r, i = 0

-- complex instances are immutable, so these are
-- read only attributes
::attribute r GET
::attribute i GET

::method negative
  expose r i
  return self~class~new(-r, -i)

::method add
  expose r i
  use strict arg other
  if other~isa(.complex) then
     return self~class~new(r + other~r, i + other~i)
  else return self~class~new(r + other, i)

::method subtract
  expose r i
  use strict arg other
  if other~isa(.complex) then
     return self~class~new(r - other~r, i - other~i)
  else return self~class~new(r - other, i)

::method times
  expose r i
  use strict arg other
  if other~isa(.complex) then
     return self~class~new(r * other~r - i * other~i, r * other~i + i * other~r)
  else return self~class~new(r * other, i * other)

::method inv
  expose r i
  denom = r * r + i * i
  return self~class~new(r/denom,-i/denom)

::method conjugate
  expose r i
  return self~class~new(r, -i)

::method divide
  use strict arg other
  -- this is easier if everything is a complex number
  if \other~isA(.complex) then other = .complex~new(other)
  -- division is multiplication with the inversion
  return self * other~inv

::method "=="
  expose r i
  use strict arg other

  if \other~isa(.complex) then return .false
  -- Note:  these are numeric comparisons, so we're using the "="
  -- method so those are handled correctly
  return r = other~r & i = other~i

::method "\=="
  use strict arg other
  return \self~"\=="(other)

::method "="
  -- this is equivalent of "=="
  forward message("==")

::method "\="
  -- this is equivalent of "\=="
  forward message("\==")

::method "<>"
  -- this is equivalent of "\=="
  forward message("\==")

::method "><"
  -- this is equivalent of "\=="
  forward message("\==")

-- some operator overrides -- these only work if the left-hand-side of the
-- subexpression is a quaternion
::method "*"
  forward message("TIMES")

::method "/"
  forward message("DIVIDE")

::method "-"
  -- need to check if this is a prefix minus or a subtract
  if arg() == 0 then
      forward message("NEGATIVE")
  else
      forward message("SUBTRACT")

::method "+"
  -- need to check if this is a prefix plus or an addition
  if arg() == 0 then
      return self  -- we can return this copy since it is immutable
  else
      forward message("ADD")

::method string
  expose r i
  return r self~formatnumber(i)"i"

::method formatnumber private
  use arg value
  if value > 0 then return "+" value
  else return "-" value~abs

-- override hashcode for collection class hash uses
::method hashCode
  expose r i
  return r~hashcode~bitxor(i~hashcode)
```

{{out}}

```txt
c1           = 1 + 2i
c2           = 3 + 4i
r            = 7
-c1          = -1 - 2i
c1 + r       = 8 + 2i
c1 + c2      = 4 + 6i
c1 - r       = -6 + 2i
c1 - c2      = -2 - 2i
c1 * r       = 7 + 14i
c1 * c2      = -5 + 10i
inv(c1)      = 0.2 - 0.4i
conj(c1)     = 1 - 2i
c1 / r       = 0.142857143 + 0.285714286i
c1 / c2      = 0.44 + 0.08i
c1 == c1     = 1
c1 == c2     = 0
```



## OxygenBasic

Implementation of a complex numbers class with arithmetical operations, and powers using DeMoivre's theorem (polar conversion).

```oxygenbasic

'COMPLEX OPERATIONS
'
### ===========


type tcomplex double x,y

class Complex
'
### ======


  has tcomplex
  static sys i,pp
  static tcomplex accum[32]

def operands
  tcomplex*a,*b
  @a=@accum+i
  if pp then
    @b=@a+sizeof accum
    pp=0
  else
    @b=@this
  end if
end def

method "load"()
  operands
  a.x=b.x
  a.y=b.y
end method

method "push"()
  i+=sizeof accum
end method

method "pop"()
  pp=1
  i-=sizeof accum
end method

method "="()
  operands
  b.x=a.x
  b.y=a.y
end method

method "+"()
  operands
  a.x+=b.x
  a.y+=b.y
end method

method "-"()
  operands
  a.x-=b.x
  a.y-=b.y
end method

method "*"()
  operands
  double d
  d=a.x
  a.x = a.x * b.x - a.y * b.y
  a.y = a.y * b.x + d   * b.y
end method

method "/"()
  operands
  double d,v
  v=1/(b.x * b.x + b.y * b.y)
  d=a.x
  a.x = (a.x * b.x + a.y * b.y) * v
  a.y = (a.y * b.x - d   * b.y) * v
end method

method power(double n)
  operands
  'Using DeMoivre theorem
  double r,an,mg
  r = hypot(b.x,b.y)
  mg = r^n
  if b.x=0 then
    ay=.5*pi
    if b.y<0 then ay=-ay
  else
    an = atan(b.y,b.x)
  end if
  an  *= n
  a.x  = mg * cos(an)
  a.y  = mg * sin(an)
end method

method show() as string
  return str(x,14) ", " str(y,14)
end method

end class

'#recordof complexop

'====
'TEST
'====

complex z1,z2,z3,z4,z5

'ENTER VALUES

z1 <=  0, 0
z2 <=  2, 1
z3 <= -2, 1
z4 <=  2, 4
z5 <=  1, 1

'EVALUATE COMPLEX EXPRESSIONS

z1 =  z2 * z3
print "Z1 = "+z1.show 'RESULT  -5.0, 0

z1 = z3+(z2.power(2))
print "Z1 = "+z1.show  'RESULT  1.0, 5.0


z1 = z5/z4
print "Z1 = "+z1.show  'RESULT 0.3, 0.1

z1 = z5/z1
print "Z1 = "+z1.show  'RESULT 2.0, 4.0

z1 = z2/z4
print "Z1 = "+z1.show  'RESULT  -0.4, -0.3

z1 = z1*z4
print "Z1 = "+z1.show  'RESULT  2.0, 1.0

```



## PARI/GP

To use, type, e.g., inv(3 + 7*I).

```parigp
add(a,b)=a+b;
mult(a,b)=a*b;
neg(a)=-a;
inv(a)=1/a;
```



## Pascal


```pascal
program showcomplex(output);

type
 complex = record
            re,im: real
           end;

var
 z1, z2, zr: complex;

procedure set(var result: complex; re, im: real);
 begin
  result.re := re;
  result.im := im
 end;

procedure print(a: complex);
 begin
  write('(', a.re , ',', a.im, ')')
 end;

procedure add(var result: complex; a, b: complex);
 begin
  result.re := a.re + b.re;
  result.im := a.im + b.im;
 end;

procedure neg(var result: complex; a: complex);
 begin
  result.re := -a.re;
  result.im := -a.im
 end;

procedure mult(var result: complex; a, b: complex);
 begin
  result.re := a.re*b.re - a.im*b.im;
  result.im := a.re*b.im + a.im*b.re
 end;

procedure inv(var result: complex; a: complex);
 var
  anorm: real;
 begin
  anorm := a.re*a.re + a.im*a.im;
  result.re := a.re/anorm;
  result.im := -a.im/anorm
 end;

begin
 set(z1, 3, 4);
 set(z2, 5, 6);

 neg(zr, z1);
 print(zr); { prints (-3,-4) }
 writeln;

 add(zr, z1, z2);
 print(zr); { prints (8,10) }
 writeln;

 inv(zr, z1);
 print(zr); { prints (0.12,-0.16) }
 writeln;

 mul(zr, z1, z2);
 print(zr); { prints (-9,38) }
 writeln
end.
```


FreePascal has a complex units. Example of usage:

```Pascal
Program ComplexDemo;

uses
  ucomplex;

var
  a, b, absum, abprod, aneg, ainv, acong: complex;

function complex(const re, im: real): ucomplex.complex; overload;
  begin
    complex.re := re;
    complex.im := im;
  end;

begin
  a      := complex(5, 3);
  b      := complex(0.5, 6.0);
  absum  := a + b;
  writeln ('(5 + i3) + (0.5 + i6.0): ', absum.re:3:1, ' + i', absum.im:3:1);
  abprod := a * b;
  writeln ('(5 + i3) * (0.5 + i6.0): ', abprod.re:5:1, ' + i', abprod.im:4:1);
  aneg   := -a;
  writeln ('-(5 + i3): ', aneg.re:3:1, ' + i', aneg.im:3:1);
  ainv   := 1.0 / a;
  writeln ('1/(5 + i3): ', ainv.re:3:1, ' + i', ainv.im:3:1);
  acong  := cong(a);
  writeln ('conj(5 + i3): ', acong.re:3:1, ' + i', acong.im:3:1);
end.

```



## Perl

The <code>Math::Complex</code> module implements complex arithmetic.

```perl
use Math::Complex;
my $a = 1 + 1*i;
my $b = 3.14159 + 1.25*i;

print "$_\n" foreach
    $a + $b,    # addition
    $a * $b,    # multiplication
    -$a,        # negation
    1 / $a,     # multiplicative inverse
    ~$a;        # complex conjugate
```



## Perl 6

{{works with|Rakudo|2015.12}}


```perl6
my $a = 1 + i;
my $b = pi + 1.25i;

.say for $a + $b, $a * $b, -$a, 1 / $a, $a.conj;
.say for $a.abs, $a.sqrt, $a.re, $a.im;
```

{{out}} (precision varies with different implementations):

```txt

4.1415926535897931+2.25i
1.8915926535897931+4.3915926535897931i
-1-1i
0.5-0.5i
1-1i
1.4142135623730951
1.0986841134678098+0.45508986056222733i
1
1

```



## Phix


```Phix
constant REAL = 1,
         IMAG = 2

type complex(sequence s)
    return length(s)=2 and atom(s[REAL]) and atom(s[IMAG])
end type

function add(complex a, complex b)
    return sq_add(a,b)
end function

function mult(complex a, complex b)
    return {a[REAL] * b[REAL] - a[IMAG] * b[IMAG],
            a[REAL] * b[IMAG] + a[IMAG] * b[REAL]}
end function

function inv(complex a)
atom denom
    denom = a[REAL] * a[REAL] + a[IMAG] * a[IMAG]
    return {a[REAL] / denom, -a[IMAG] / denom}
end function

function neg(complex a)
    return sq_uminus(a)
end function

function scomplex(complex a)
sequence s = ""
atom ar, ai
    {ar, ai} = a
    if ar!=0 then
        s = sprintf("%g",ar)
    end if

    if ai!=0 then
        if ai=1 then
            s &= "+i"
        elsif ai=-1 then
            s &= "-i"
        else
            s &= sprintf("%+gi",ai)
        end if
    end if

    if length(s)=0 then
        return "0"
    end if
    return s
end function

complex a, b
a = { 1.0,     1.0 }
b = { 3.14159, 1.2 }
printf(1,"a = %s\n",{scomplex(a)})
printf(1,"b = %s\n",{scomplex(b)})
printf(1,"a+b = %s\n",{scomplex(add(a,b))})
printf(1,"a*b = %s\n",{scomplex(mult(a,b))})
printf(1,"1/a = %s\n",{scomplex(inv(a))})
printf(1,"-a = %s\n",{scomplex(neg(a))})
```

{{out}}

```txt

a = 1+i
b = 3.14159+1.2i
a+b = 4.14159+2.2i
a*b = 1.94159+4.34159i
1/a = 0.5-0.5i
-a = -1-i

```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de addComplex (A B)
   (cons
      (+ (car A) (car B))        # Real
      (+ (cdr A) (cdr B)) ) )    # Imag

(de mulComplex (A B)
   (cons
      (-
         (*/ (car A) (car B) 1.0)
         (*/ (cdr A) (cdr B) 1.0) )
      (+
         (*/ (car A) (cdr B) 1.0)
         (*/ (cdr A) (car B) 1.0) ) ) )

(de invComplex (A)
   (let Denom
      (+
         (*/ (car A) (car A) 1.0)
         (*/ (cdr A) (cdr A) 1.0) )
      (cons
         (*/ (car A) 1.0 Denom)
         (- (*/ (cdr A) 1.0 Denom)) ) ) )

(de negComplex (A)
   (cons (- (car A)) (- (cdr A))) )

(de fmtComplex (A)
   (pack
      (round (car A) (dec *Scl))
      (and (gt0 (cdr A)) "+")
      (round (cdr A) (dec *Scl))
      "i" ) )

(let (A (1.0 . 1.0)  B (cons pi 1.2))
   (prinl "A = " (fmtComplex A))
   (prinl "B = " (fmtComplex B))
   (prinl "A+B = " (fmtComplex (addComplex A B)))
   (prinl "A*B = " (fmtComplex (mulComplex A B)))
   (prinl "1/A = " (fmtComplex (invComplex A)))
   (prinl "-A = " (fmtComplex (negComplex A))) )
```

{{out}}

```txt
A = 1.00000+1.00000i
B = 3.14159+1.20000i
A+B = 4.14159+2.20000i
A*B = 1.94159+4.34159i
1/A = 0.50000-0.50000i
-A = -1.00000-1.00000i
```



## PL/I


```pli
/* PL/I complex numbers may be integer or floating-point.  */
/* In this example, the variables are floating-pint.       */
/* For integer variables, change 'float' to 'fixed binary' */

declare (a, b) complex float;
a = 2+5i;
b = 7-6i;

put skip list (a+b);
put skip list (a - b);
put skip list (a*b);
put skip list (a/b);
put skip list (a**b);
put skip list (1/a);
put skip list (conjg(a)); /* gives the conjugate of 'a'. */

/* Functions exist for extracting the real and imaginary parts */
/* of a complex number. */

/* As well, trigonometric functions may be used with complex  */
/* numbers, such as SIN, COS, TAN, ATAN, and so on.           */
```



## Pop11


Complex numbers are a built-in data type in Pop11.  Real and
imaginary part of complex numbers can be floating point or
exact (integer or rational) value (both part must be of the same
type).  Operations on floating point complex numbers always produce
complex numbers.  Operations on exact complex numbers give
real result (integer or rational) if imaginary part of the result
is 0.  The '+:' and '-:' operators create complex numbers:
'1 -: 3' is '1 - 3i' in mathematical notation.


```pop11
lvars a = 1.0 +: 1.0, b = 2.0 +: 5.0 ;
a+b =>
a*b =>
1/a =>
a-b =>
a-a =>
a/b =>
a/a =>

;;; The same, but using exact values
1 +: 1 -> a;
2 +: 5 -> b;
a+b =>
a*b =>
1/a =>
a-b =>
a-a =>
a/b =>
a/a =>
```



## PostScript

Complex numbers can be represented as 2 element vectors ( arrays ). Thus, a+bi can be written as [a b] in PostScript.
<lang>
%Adding two complex numbers
/addcomp{
/x exch def
/y exch def
/z [0 0] def
z 0 x 0 get y 0 get add put
z 1 x 1 get y 1 get add put
z pstack
}def

%Subtracting one complex number from another
/subcomp{
/x exch def
/y exch def
/z [0 0] def
z 0 x 0 get y 0 get sub put
z 1 x 1 get y 1 get sub put
z pstack
}def

%Multiplying two complex numbers
/mulcomp{
/x exch def
/y exch def
/z [0 0] def
z 0 x 0 get y 0 get mul x 1 get y 1 get mul sub  put
z 1 x 1 get y 0 get mul x 0 get y 1 get mul add put
z pstack
}def

%Negating a complex number
/negcomp{
/x exch def
/z [0 0] def
z 0 x 0 get neg put
z 1 x 1 get neg put
z pstack
}def

%Inverting a complex number
/invcomp{
/x exch def
/z [0 0] def
z 0 x 0 get x 0 get 2 exp x 1 get 2 exp add div put
z 0 x 1 get neg x 0 get 2 exp x 1 get 2 exp add div put
z pstack
}def


```



## PowerShell


### Implementation


```PowerShell

class Complex {
  [Double]$x
  [Double]$y
  Complex() {
      $this.x = 0
      $this.y = 0
  }
  Complex([Double]$x, [Double]$y) {
      $this.x = $x
      $this.y = $y
  }
  [Double]abs2() {return $this.x*$this.x + $this.y*$this.y}
  [Double]abs() {return [math]::sqrt($this.abs2())}
  static [Complex]add([Complex]$m,[Complex]$n) {return [Complex]::new($m.x+$n.x, $m.y+$n.y)}
  static [Complex]mul([Complex]$m,[Complex]$n) {return [Complex]::new($m.x*$n.x - $m.y*$n.y, $m.x*$n.y + $n.x*$m.y)}
  [Complex]mul([Double]$k) {return [Complex]::new($k*$this.x, $k*$this.y)}
  [Complex]negate() {return $this.mul(-1)}
  [Complex]conjugate() {return [Complex]::new($this.x, -$this.y)}
  [Complex]inverse() {return $this.conjugate().mul(1/$this.abs2())}
  [String]show() {
    if(0 -ge $this.y) {
        return "$($this.x)+$($this.y)i"
    } else {
        return "$($this.x)$($this.y)i"
    }
  }
  static [String]show([Complex]$other) {
    return $other.show()
  }
}
$m = [complex]::new(3, 4)
$n = [complex]::new(7, 6)
"`$m: $($m.show())"
"`$n: $($n.show())"
"`$m + `$n: $([complex]::show([complex]::add($m,$n)))"
"`$m * `$n: $([complex]::show([complex]::mul($m,$n)))"
"negate `$m: $($m.negate().show())"
"1/`$m: $([complex]::show($m.inverse()))"
"conjugate `$m: $([complex]::show($m.conjugate()))"

```

<b>Output:</b>

```txt

$m: 3+4i
$n: 7+6i
$m + $n: 10+10i
$m * $n: -3+46i
negate $m: -3-4i
1/$m: 0.12-0.16i
conjugate $m: 3-4i

```


### Library


```PowerShell

function show([System.Numerics.Complex]$c) {
    if(0 -le $c.Imaginary) {
        return "$($c.Real)+$($c.Imaginary)i"
    } else {
        return "$($c.Real)$($c.Imaginary)i"
    }
  }
$m = [System.Numerics.Complex]::new(3, 4)
$n = [System.Numerics.Complex]::new(7, 6)
"`$m: $(show $m)"
"`$n: $(show $n)"
"`$m + `$n: $(show ([System.Numerics.Complex]::Add($m,$n)))"
"`$m * `$n: $(show ([System.Numerics.Complex]::Multiply($m,$n)))"
"negate `$m: $(show ([System.Numerics.Complex]::Negate($m)))"
"1/`$m: $(show ([System.Numerics.Complex]::Reciprocal($m)))"
"conjugate `$m: $(show ([System.Numerics.Complex]::Conjugate($m)))"

```

<b>Output:</b>

```txt

$m: 3+4i
$n: 7+6i
$m + $n: 10+10i
$m * $n: -3+46i
negate $m: -3-4i
1/$m: 0.12-0.16i
conjugate $m: 3-4i

```



## PureBasic


```PureBasic
Structure Complex
  real.d
  imag.d
EndStructure

Procedure Add_Complex(*A.Complex, *B.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex))
  If *R
    *R\real=*A\real+*B\real
    *R\imag=*A\imag+*B\imag
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure Inv_Complex(*A.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex)), denom.d
  If *R
    denom  = *A\real * *A\real + *A\imag * *A\imag
    *R\real= *A\real / denom
    *R\imag=-*A\imag / denom
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure Mul_Complex(*A.Complex, *B.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex))
  If *R
    *R\real=*A\real * *B\real - *A\imag * *B\imag
    *R\imag=*A\real * *B\imag + *A\imag * *B\real
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure Neg_Complex(*A.Complex)
  Protected *R.Complex=AllocateMemory(SizeOf(Complex))
  If *R
    *R\real=-*A\real
    *R\imag=-*A\imag
  EndIf
  ProcedureReturn *R
EndProcedure

Procedure ShowAndFree(Header$, *Complex.Complex)
  If *Complex
    Protected.d i=*Complex\imag, r=*Complex\real
    Print(LSet(Header$,7))
    Print("= "+StrD(r,3))
    If i>=0:  Print(" + ")
    Else:     Print(" - ")
    EndIf
    PrintN(StrD(Abs(i),3)+"i")
    FreeMemory(*Complex)
  EndIf
EndProcedure

If OpenConsole()
  Define.Complex a, b, *c
  a\real=1.0: a\imag=1.0
  b\real=#PI: b\imag=1.2
  *c=Add_Complex(a,b):  ShowAndFree("a+b",    *c)
  *c=Mul_Complex(a,b):  ShowAndFree("a*b",    *c)
  *c=Inv_Complex(a):    ShowAndFree("Inv(a)", *c)
  *c=Neg_Complex(a):    ShowAndFree("-a",     *c)
  Print(#CRLF$+"Press ENTER to exit"):Input()
EndIf
```



## Python



```python>>>
 z1 = 1.5 + 3j
>>> z2 = 1.5 + 1.5j
>>> z1 + z2
(3+4.5j)
>>> z1 - z2
1.5j
>>> z1 * z2
(-2.25+6.75j)
>>> z1 / z2
(1.5+0.5j)
>>> - z1
(-1.5-3j)
>>> z1.conjugate()
(1.5-3j)
>>> abs(z1)
3.3541019662496847
>>> z1 ** z2
(-1.1024829553277784-0.38306415117199333j)
>>> z1.real
1.5
>>> z1.imag
3.0
>>>
```



## R

{{trans|Octave}}


```rsplus
z1 <- 1.5 + 3i
z2 <- 1.5 + 1.5i
print(z1 + z2)   #  3+4.5i
print(z1 - z2)   #  0+1.5i
print(z1 * z2)   #  -2.25+6.75i
print(z1 / z2)   #  1.5+0.5i
print(-z1)       #  -1.5-3i
print(Conj(z1))  #  1.5-3i
print(abs(z1))   #  3.354102
print(z1^z2)     #  -1.102483-0.383064i
print(exp(z1))   #  -4.436839+0.632456i
print(Re(z1))    #  1.5
print(Im(z1))    #  3
```



## Racket



```racket

#lang racket

(define a 3+4i)
(define b 8+0i)

(+ a b)       ; addition
(- a b)       ; subtraction
(/ a b)       ; division
(* a b)       ; multiplication
(- a)         ; negation
(/ 1 a)       ; reciprocal
(conjugate a) ; conjugation

```



## REXX

The REXX language has no complex type numbers, but most complex arithmetic functions can easily be written.

```rexx
/*REXX program  demonstrates how to support some  math functions  for  complex numbers. */
x = '(5,3i)'                                     /*define  X    ─── can use  I i J or j */
y = "( .5,  6j)"                                 /*define  Y         "   "   " " "  " " */

say '      addition:   '        x        " + "         y         ' = '          Cadd(x, y)
say '   subtraction:   '        x        " - "         y         ' = '          Csub(x, y)
say 'multiplication:   '        x        " * "         y         ' = '          Cmul(x, y)
say '      division:   '        x        " ÷ "         y         ' = '          Cdiv(x, y)
say '       inverse:   '        x        "                         = "          Cinv(x, y)
say '  conjugate of:   '        x        "                         = "          Conj(x, y)
say '   negation of:   '        x        "                         = "          Cneg(x, y)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Conj: procedure; parse arg a ',' b,c ',' d;   call C#;    return C$(  a      ,  -b    )
Cadd: procedure; parse arg a ',' b,c ',' d;   call C#;    return C$(  a+c    ,   b+d  )
Csub: procedure; parse arg a ',' b,c ',' d;   call C#;    return C$(  a-c    ,   b-d  )
Cmul: procedure; parse arg a ',' b,c ',' d;   call C#;    return C$( ac-bd   ,   bc+ad)
Cdiv: procedure; parse arg a ',' b,c ',' d;   call C#;    return C$((ac+bd)/s,  (bc-ad)/s)
Cinv: return  Cdiv(1,  arg(1))
Cneg: return  Cmul(arg(1), -1)
C_:   return  word(translate(arg(1), , '{[(JjIi)]}')  0,  1)                /*get # or 0*/
C#:   a=C_(a); b=C_(b); c=C_(c); d=C_(d); ac=a*c; ad=a*d; bc=b*c; bd=b*d;s=c*c+d*d; return
C$:   parse arg r,c;    _='['r;   if c\=0  then _=_","c'j';   return _"]"   /*uses  j   */
```

'''output'''

```txt

      addition:    (5,3i)  +  ( .5,  6j)  =  [5.5,9j]
   subtraction:    (5,3i)  -  ( .5,  6j)  =  [4.5,-3j]
multiplication:    (5,3i)  *  ( .5,  6j)  =  [-15.5,31.5j]
      division:    (5,3i)  ÷  ( .5,  6j)  =  [0.565517241,-0.786206897j]
       inverse:    (5,3i)                 =  [0.147058824,-0.0882352941j]
  conjugate of:    (5,3i)                 =  [5,-3j]
   negation of:    (5,3i)                 =  [-5,-3j]

```



## RLaB



```RLaB

>> x = sqrt(-1)
                        0 + 1i
>> y = 10 + 5i
                       10 + 5i
>> z = 5*x-y
                      -10 + 0i
>> isreal(z)
  1

```



## Ruby


```ruby
require 'complex'  # With Ruby 1.9, this line is optional.

# Three ways to write complex numbers:
a = Complex(1, 1)       # 1. call Kernel#Complex
i = Complex::I          # 2. use Complex::I
b = 3.14159 + 1.25 * i
c = '1/2+3/4i'.to_c     # 3. Use the .to_c method from String, result ((1/2)+(3/4)*i)

#Ruby 2.1 introduced a suffix to create a complex:
c =  1.0/2+3/4i         # (0.5-(3/4)*i)

# Operations:
puts a + b              # addition
puts a * b              # multiplication
puts -a                 # negation
puts 1.quo a            # multiplicative inverse
puts a.conjugate        # complex conjugate
puts a.conj             # alias for complex conjugate
```


''Notes:''
* Ruby 1.8 must <code>require 'complex'</code>. Ruby 1.9 moves complex numbers to core, so <code>require 'complex'</code> only defines a few deprecated methods.
* Ruby 1.9 deprecates Numeric#im; code like <code>a = 1 + 1.im</code> or <code>b = 3.14159 + 1.25.im</code> would call the deprecated method.
* All of these operations are safe with other numeric types. For example, <code>42.conjugate</code> returns 42.


```ruby
# Other ways to find the multiplicative inverse:
puts 1.quo a            # always works
puts 1.0 / a            # works, but forces floating-point math
puts 1 / a              # might truncate to integer
```


CMath is a standard library that provides trigonometric and transcendental functions for complex numbers:


```ruby
require "cmath"
CMath.sqrt(-9)      #=> 0+3.0i
CMath.acos(0+3.0i)  #=> (1.5707963267948966-1.8184464592320668i)
#etc
```



## Rust


```rust
extern crate num;
use num::complex::Complex;

fn main() {
    // two valid forms of definition
    let a = Complex {re:-4.0, im: 5.0};
    let b = Complex::new(1.0, 1.0);

    println!("   a    = {}", a);
    println!("   b    = {}", b);
    println!(" a + b  = {}", a + b);
    println!(" a * b  = {}", a * b);
    println!(" 1 / a  = {}", a.inv());
    println!("  -a    = {}", -a);
    println!("conj(a) = {}", a.conj());
}
```



## Scala

{{works with|Scala|2.8}}
Scala doesn't come with a Complex library, but one can be made:


```scala
package org.rosettacode

package object ArithmeticComplex {
  val i = Complex(0, 1)

  implicit def fromDouble(d: Double) = Complex(d)
  implicit def fromInt(i: Int) = Complex(i.toDouble)
}

package ArithmeticComplex {
  case class Complex(real: Double = 0.0, imag: Double = 0.0) {
    def this(s: String) =
      this("[\\d.]+(?!i)".r findFirstIn s getOrElse "0" toDouble,
           "[\\d.]+(?=i)".r findFirstIn s getOrElse "0" toDouble)

    def +(b: Complex) = Complex(real + b.real, imag + b.imag)
    def -(b: Complex) = Complex(real - b.real, imag - b.imag)
    def *(b: Complex) = Complex(real * b.real - imag * b.imag, real * b.imag + imag * b.real)
    def inverse = {
      val denom = real * real + imag * imag
      Complex(real / denom, -imag / denom)
    }
    def /(b: Complex) = this * b.inverse
    def unary_- = Complex(-real, -imag)
    lazy val abs = math.hypot(real, imag)
    override def toString = real + " + " + imag + "i"

    def i = { require(imag == 0.0); Complex(imag = real) }
  }

  object Complex {
    def apply(s: String) = new Complex(s)
    def fromPolar(rho:Double, theta:Double) = Complex(rho*math.cos(theta), rho*math.sin(theta))
  }
}
```


Usage example:


```scala>scala
 import org.rosettacode.ArithmeticComplex._
import org.rosettacode.ArithmeticComplex._

scala> 1 + i
res0: org.rosettacode.ArithmeticComplex.Complex = 1.0 + 1.0i

scala> 1 + 2 * i
res1: org.rosettacode.ArithmeticComplex.Complex = 1.0 + 2.0i

scala> 2 + 1.i
res2: org.rosettacode.ArithmeticComplex.Complex = 2.0 + 1.0i

scala> res0 + res1
res3: org.rosettacode.ArithmeticComplex.Complex = 2.0 + 3.0i

scala> res1 * res2
res4: org.rosettacode.ArithmeticComplex.Complex = 0.0 + 5.0i

scala> res2 / res0
res5: org.rosettacode.ArithmeticComplex.Complex = 1.5 + -0.5i

scala> res1.inverse
res6: org.rosettacode.ArithmeticComplex.Complex = 0.2 + -0.4i

scala> -res6
res7: org.rosettacode.ArithmeticComplex.Complex = -0.2 + 0.4i

```



## Scheme

Scheme implementations are not required to support complex numbers, but if they do, they are required to support complex number literals in one of the following standard formats[http://people.csail.mit.edu/jaffer/r4rs_9.html#SEC67]:
* rectangular coordinates: <code>''real''+''imag''i</code> (or <code>''real''-''imag''i</code>), where ''real'' is the real part and ''imag'' is the imaginary part. For a pure-imaginary number, the real part may be omitted but the sign of the imaginary part is mandatory (even if it is "+"): <code>+''imag''i</code> (or <code>-''imag''i</code>). If the imaginary part is 1 or -1, the imaginary part can be omitted, leaving only the  <code>+i</code> or <code>-i</code> at the end.
* polar coordinates: <code>''r''@''theta''</code>, where ''r'' is the absolute value (magnitude) and ''theta'' is the angle

```scheme
(define a 1+i)
(define b 3.14159+1.25i)

(define c (+ a b))
(define c (* a b))
(define c (/ 1 a))
(define c (- a))
```



## Seed7



```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "complex.s7i";

const proc: main is func
  local
    var complex: a is complex(1.0, 1.0);
    var complex: b is complex(3.14159, 1.2);
  begin
    writeln("a=" <& a digits 5);
    writeln("b=" <& b digits 5);
    # addition
    writeln("a+b=" <& a + b digits 5);
    # multiplication
    writeln("a*b=" <& a * b digits 5);
    # inversion
    writeln("1/a=" <& complex(1.0) / a digits 5);
    # negation
    writeln("-a=" <& -a digits 5);
  end func;
```



## Sidef


```ruby
var a = 1:1                 # Complex(1, 1)
var b = 3.14159:1.25        # Complex(3.14159, 1.25)

[   a + b,                  # addition
    a * b,                  # multiplication
    -a,                     # negation
    a.inv,                  # multiplicative inverse
    a.conj,                 # complex conjugate
    a.abs,                  # abs
    a.sqrt,                 # sqrt
    b.re,                   # real
    b.im,                   # imaginary
].each { |c| say c }
```

{{out}}

```txt
4.14159+2.25i
1.89159+4.39159i
-1-i
0.5-0.5i
1-i
1.4142135623730950488016887242097
1.09868411346780996603980119524068+0.45508986056222734130435775782247i
3.14159
1.25
```



## Slate



```slate
[| a b |
  a: 1 + 1 i.
  b: Pi + 1.2 i.
  print: a + b.
  print: a * b.
  print: a / b.
  print: a reciprocal.
  print: a conjugated.
  print: a abs.
  print: a negated.
].
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
PackageLoader fileInPackage: 'Complex'.
|a b|
a := 1 + 1 i.
b := 3.14159 + 1.2 i.
(a + b) displayNl.
(a * b) displayNl.
(a / b) displayNl.
a reciprocal displayNl.
a conjugate displayNl.
a abs displayNl.
a real displayNl.
a imaginary displayNl.
a negated displayNl.
```



## smart BASIC


<b>Original author unknown  {:o(</b>


```qbasic
' complex numbers are native for "smart BASIC"
A=1+2i
B=3-5i

' all math operations and functions work with complex numbers
C=A*B
PRINT SQR(-4)

' example of solving quadratic equation with complex roots
' x^2+2x+5=0
a=1 ! b=2 ! c=5
x1=(-b+sqr(b^2-4*a*c))/(2*a)
x2=(-b-sqr(b^2-4*a*c))/(2*a)
print x1,x2

' gives output
-1+2i    -1-2i
```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}


```SNOBOL4
*       # Define complex datatype
        data('complex(r,i)')

*       # Addition
        define('addx(x1,x2)a,b,c,d') :(addx_end)
addx    a = r(x1); b = i(x1); c = r(x2); d = i(x2)
        addx = complex(a + c, b + d) :(return)
addx_end

*       # Multiplication
        define('multx(x1,x2)a,b,c,d') :(multx_end)
multx   a = r(x1); b = i(x1); c = r(x2); d = i(x2)
        multx = complex(a * c - b * d, b * c + a * d) :(return)
multx_end

*       # Negation
        define('negx(x)') :(negx_end)
negx    negx = complex(-r(x), -i(x)) :(return)
negx_end

*       # Inverse
        define('invx(x)d') :(invx_end)
invx    d = (r(x) * r(x)) + (i(x) * i(x))
        invx = complex(1.0 * r(x) / d, 1.0 * -i(x) / d) :(return)
invx_end

*       # Print compex number: a+bi / a-bi
        define('printx(x)sign') :(printx_end)
printx  sign = ge(i(x),0) '+'
        printx = r(x) sign i(x) 'i' :(return)
printx_end

*       # Test and display
        a = complex(1,1)
        b = complex(3.14159, 1.2)
        output = printx( addx(a,b) )
        output = printx( multx(a,b) )
        output = printx( negx(a) ) ', ' printx( negx(b) )
        output = printx( invx(a) ) ', ' printx( invx(b) )
end
```


{{out}}

```txt
4.14159+2.2i
1.94159+4.34159i
-1-1i, -3.14159-1.2i
0.5-0.5i, 0.277781125-0.106104663i
```



## Standard ML


```Standard ML

(* Signature for complex numbers *)
signature COMPLEX = sig
 type num

 val complex : real * real -> num

 val negative : num -> num
 val plus : num -> num -> num
 val minus : num -> num -> num
 val times : num -> num -> num
 val invert : num -> num
 val print_number : num -> unit
end;

(* Actual implementation *)
structure Complex :> COMPLEX = struct
  type num = real * real

  fun complex (a, b) = (a, b)

  fun negative (a, b) = (Real.~a, Real.~b)
  fun plus (a1, b1) (a2, b2) = (Real.+ (a1, a2), Real.+(b1, b2))
  fun minus i1 i2 = plus i1 (negative i2)
  fun times (a1, b1) (a2, b2)= (Real.*(a1, a2) - Real.*(b1, b2), Real.*(a1, b2) + Real.*(a2, b1))
  fun invert (a, b) =
    let
      val denom = a * a + b * b
    in
      (a / denom, ~b / denom)
    end

  fun print_number (a, b) =
    print (Real.toString(a) ^ " + " ^ Real.toString(b) ^ "i\n")
end;

val i1 = Complex.complex(1.0,2.0); (* 1 + 2i *)
val i2 = Complex.complex(3.0,4.0); (* 3 + 4i *)

Complex.print_number(Complex.negative(i1)); (* -1 - 2i *)
Complex.print_number(Complex.plus i1 i2); (* 4 + 6i *)
Complex.print_number(Complex.minus i2 i1); (* 2 + 2i *)
Complex.print_number(Complex.times i1 i2); (* -5 + 10i *)
Complex.print_number(Complex.invert i1); (* 1/5 - 2i/5 *)

```



## Stata



```stata
mata
C(2,3)
2 + 3i

a=2+3i
b=1-2*i


a+b
-5 + 3i

a-b
9 + 3i

a*b
-14 - 21i

a/b
-.285714286 - .428571429i

-a
-2 - 3i

1/a
.153846154 - .230769231i

conj(a)
2 - 3i

abs(a)
3.605551275

arg(a)
.9827937232

exp(a)
-7.31511009 + 1.04274366i

log(a)
1.28247468 + .982793723i

end
```



## Swift

{{works with|Swift | 2.0 }}

Use a struct to create a complex number type in Swift. Math Operations can be added using operator overloading


```swift

public struct Complex {

    public let real : Double
    public let imaginary : Double

    public init(real inReal:Double, imaginary inImaginary:Double) {
        real = inReal
        imaginary = inImaginary
    }

    public static var i : Complex = Complex(real:0, imaginary: 1)
    public static var zero : Complex = Complex(real: 0, imaginary: 0)

    public var negate : Complex {
        return Complex(real: -real, imaginary: -imaginary)
    }

    public var invert : Complex {
        let d = (real*real + imaginary*imaginary)
        return Complex(real: real/d, imaginary: -imaginary/d)
    }

    public var conjugate : Complex {
        return Complex(real: real, imaginary: -imaginary)
    }

}
public func + (left: Complex, right: Complex) -> Complex {

    return Complex(real: left.real+right.real, imaginary: left.imaginary+right.imaginary)
}
public func * (left: Complex, right: Complex) -> Complex {

    return Complex(real: left.real*right.real - left.imaginary*right.imaginary,
        imaginary: left.real*right.imaginary+left.imaginary*right.real)
}
public prefix func - (right:Complex) -> Complex {
    return right.negate
}

// Checking equality is almost necessary for a struct of this type  to be useful
extension Complex : Equatable {}
public func == (left:Complex, right:Complex) -> Bool {
    return left.real == right.real && left.imaginary == right.imaginary
}


```


Make the Complex Number struct printable and easier to debug by adding making it conform to CustomStringConvertible


```swift


extension Complex : CustomStringConvertible {

    public var description : String {

        guard real != 0 || imaginary != 0 else { return "0" }

        let rs : String = real != 0 ? "\(real)" : ""
        let iS : String
        let sign : String
        let iSpace = real != 0 ? " " : ""
        switch imaginary {
        case let i where i < 0:
            sign = "-"
            iS = i == -1 ? "i" : "\(-i)i"
        case let i where i > 0:
            sign = real != 0 ? "+" : ""
            iS = i == 1 ? "i" : "\(i)i"
        default:
            sign = ""
            iS = ""
        }
        return "\(rs)\(iSpace)\(sign)\(iSpace)\(iS)"
    }
}


```


Explicitly support subtraction and division


```swift

public func - (left:Complex, right:Complex) -> Complex {
    return left + -right
}

public func / (divident:Complex, divisor:Complex) -> Complex {
    let rc = divisor.conjugate
    let num = divident * rc
    let den = divisor * rc
    return Complex(real: num.real/den.real, imaginary: num.imaginary/den.real)
}

```



## Tcl

{{tcllib|math::complexnumbers}}

```tcl
package require math::complexnumbers
namespace import math::complexnumbers::*

set a [complex 1 1]
set b [complex 3.14159 1.2]
puts [tostring [+ $a $b]] ;# ==> 4.14159+2.2i
puts [tostring [* $a $b]] ;# ==> 1.94159+4.34159i
puts [tostring [pow $a [complex -1 0]]] ;# ==> 0.5-0.4999999999999999i
puts [tostring [- $a]] ;# ==> -1.0-i
```


=={{header|TI-83 BASIC}}==

TI-83 BASIC has built in complex number support; the normal arithmetic operators + - * / are used.

The method complex numbers are displayed can be chosen in the "MODE" menu.<br />
Real: Does not show complex numbers, gives an error if a number is imaginary.<br />
a+bi: The classic display for imaginary numbers with the real and imaginary components<br />
re^Θi: Displays imaginary numbers in Polar Coordinates.

=={{header|TI-89 BASIC}}==

TI-89 BASIC has built-in complex number support; the normal arithmetic operators + - * / are used.

:Character set note: the symbol for the imaginary unit is not the normal "i" but a different character (Unicode: U+F02F "<span style="font-family: 'TI Uni';"></span>" (private use area); this character should display with the "TI Uni" font). Also, U+3013 EN DASH “<span style="font-family: 'TI Uni';">–</span>”, displayed on the TI as a superscript minus, is used for the minus sign on numbers, distinct from ASCII "-" used for subtraction.

The choice of examples here is {{trans|Common Lisp}}.

<!--lang ti89b--><pre style="font-family: 'TI Uni';">■ √(–1)                    
■ ^2                     —1
■  + 1                1 + 
■ (1+) * 2          2 + 2*
■ (1+) (2)        —2 + 2*
■ —(1+)              —1 - 
■ 1/(2)              —1 - 
■ real(1 + 2)             1
■ imag(1 + 2)             2
```


Complex numbers can also be entered and displayed in polar form. (This example shows input in polar form while the complex display mode is rectangular and the angle mode is radians).

<!--lang ti89b--><pre style="font-family: 'TI Uni';">■ (1∠π/4)
           √(2)/2 + √(2)/2*
```


Note that the parentheses around ∠ notation are required. It has a related use in vectors: (1∠π/4) is a complex number, [1,∠π/4] is a vector in two dimensions in polar notation, and [(1∠π/4)] is a complex number in a vector.


## UNIX Shell

{{works with|ksh93}}

```bash
typeset -T Complex_t=(
    float real=0
    float imag=0

    function to_s {
        print -- "${_.real} + ${_.imag} i"
    }

    function dup {
        nameref other=$1
        _=( real=${other.real} imag=${other.imag} )
    }

    function add {
        typeset varname
        for varname; do
            nameref other=$varname
            (( _.real += other.real ))
            (( _.imag += other.imag ))
        done
    }

    function negate {
        (( _.real *= -1 ))
        (( _.imag *= -1 ))
    }

    function conjugate {
        (( _.imag *= -1 ))
    }

    function multiply {
        typeset varname
        for varname; do
            nameref other=$varname
            float a=${_.real} b=${_.imag} c=${other.real} d=${other.imag}
            (( _.real = a*c - b*d ))
            (( _.imag = b*c + a*d ))
        done
    }

    function inverse {
        if (( _.real == 0 && _.imag == 0 )); then
            print -u2 "division by zero"
            return 1
        fi
        float denom=$(( _.real*_.real + _.imag*_.imag ))
        (( _.real = _.real / denom ))
        (( _.imag = -1 * _.imag / denom ))
    }
)

Complex_t a=(real=1 imag=1)
a.to_s        # 1 + 1 i

Complex_t b=(real=3.14159 imag=1.2)
b.to_s        # 3.14159 + 1.2 i

Complex_t c
c.add a b
c.to_s        # 4.14159 + 2.2 i

c.negate
c.to_s        # -4.14159 + -2.2 i

c.conjugate
c.to_s        # -4.14159 + 2.2 i

c.dup a
c.multiply b
c.to_s        # 1.94159 + 4.34159 i

Complex_t d=(real=2 imag=1)
d.inverse
d.to_s        # 0.4 + -0.2 i
```



## Ursala


Complex numbers are a primitive type that can be parsed in
fixed or exponential formats, with either i or j notation as shown.
The usual complex arithmetic and transcendental functions are callable
using the syntax libname..funcname or a recognizable truncation (e.g.,
c..add or ..csin). Real operands are promoted to complex.


```Ursala
u = 3.785e+00-1.969e+00i
v = 9.545e-01-3.305e+00j

#cast %jL

examples =

<
   complex..add (u,v),
   complex..mul (u,v),
   complex..sub (0.,u),
   complex..div (1.,v)>
```

{{out}}

```txt
<
   4.740e+00-5.274e+00j,
   -2.895e+00-1.439e+01j,
   3.785e+00-1.969e+00j,
   8.066e-02+2.793e-01j>
```



## Wortel

{{trans|CoffeeScript}}

```wortel
@class Complex {
  &[r i] @: {
    ^r || r 0
    ^i || i 0
    ^m +@sq^r @sq^i
  }
  add &o @new Complex[+ ^r o.r + ^i o.i]
  mul &o @new Complex[-* ^r o.r * ^i o.i +* ^r o.i * ^i o.r]
  neg &^ @new Complex[@-^r @-^i]
  inv &^ @new Complex[/ ^r ^m / @-^i ^m]
  toString &^?{
    =^i 0 "{^r}"
    =^r 0 "{^i}i"
    >^i 0 "{^r} + {^i}i"
    "{^r} - {@-^i}i"
  }
}

@vars {
  a @new Complex[5 3]
  b @new Complex[4 3N]
}
@each &x !console.log x [
  "({a}) + ({b}) = {!a.add b}"
  "({a}) * ({b}) = {!a.mul b}"
  "-1 * ({b}) = {b.neg.}"
  "({a}) - ({b}) = {!a.add b.neg.}"
  "1 / ({b}) = {b.inv.}"
  "({!a.mul b}) / ({b}) = {`!.mul b.inv. !a.mul b}"
]
```

{{out}}

```txt
(5 + 3i) + (4 - 3i) = 9
(5 + 3i) * (4 - 3i) = 29 - 3i
-1 * (4 - 3i) = -4 + 3i
(5 + 3i) - (4 - 3i) = 1 + 6i
1 / (4 - 3i) = 0.16 + 0.12i
(29 - 3i) / (4 - 3i) = 5 + 3i
```



## XPL0


```XPL0
include c:\cxpl\codes;

func real CAdd(A, B, C);        \Return complex sum of two complex numbers
real A, B, C;
[C(0):= A(0) + B(0);
 C(1):= A(1) + B(1);
return C;
];

func real CMul(A, B, C);        \Return complex product of two complex numbers
real A, B, C;
[C(0):= A(0)*B(0) - A(1)*B(1);
 C(1):= A(1)*B(0) + A(0)*B(1);
return C;
];

func real CNeg(A, C);           \Return negative of a complex number
real A, C;
[C(0):= -A(0);
 C(1):= -A(1);
return C;
];

func real CInv(A, C);           \Return inversion (reciprical) of complex number
real A, C;
real D;
[D:= sq(A(0)) + sq(A(1));
C(0):= A(0)/D;
C(1):=-A(1)/D;
return C;
];

func real Conj(A, C);           \Return conjugate of a complex number
real A, C;
[C(0):= A(0);
 C(1):=-A(1);
return C;
];

proc COut(D, A);                \Output a complex number to specified device
int D; real A;
[RlOut(D, A(0));
  Text(D, if A(1)>=0.0 then " +" else " -");
 RlOut(D, abs(A(1)));
ChOut(D, ^i);
];

real U, V, W(2);
[Format(2,2);
U:= [1.0,  1.0];
V:= [3.14, 1.2];
COut(0, CAdd(U,V,W)); CrLf(0);
COut(0, CMul(U,V,W)); CrLf(0);
COut(0, CNeg(U,W));   CrLf(0);
COut(0, CInv(U,W));   CrLf(0);
COut(0, Conj(U,W));   CrLf(0);
]
```


{{out}}

```txt

 4.14 + 2.20i
 1.94 + 4.34i
-1.00 - 1.00i
 0.50 - 0.50i
 1.00 - 1.00i

```



## Yabasic


```Yabasic
rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rem 	CADDI/CADDR  addition of complex numbers  Z1 + Z2    with Z1 = a1 + b1 *i   Z2 = a2 + b2*i
rem                CADDI returns imaginary part and CADDR the real part
rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
export sub caddi( a1 , b1 , a2 , b2)
    return (b1 + b2)
end sub
export sub caddr( a1 , b1 , a2 , b2)
    return (a1 + a2)
end sub

rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rem   CDIVI/CDIVR  division of complex numbers  Z1 / Z2        with Z1 = r + s *i   Z2 = t + u*i
rem                CDIVI returns imaginary part and CDIVR the real part
rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
export sub cdivi(r,s,t,u)
    return ((s*t- u*r) / (t^2 + u^2))
end sub
export sub cdivr( r , s , t , u)
    return ((r*t- s*u) / (t^2 + u^2))
end sub

rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rem   CMULI/CMULR  multiplication of complex numbers  Z1 * Z2, with Z1 = r + s *i   Z2 = t + u*i
rem                CMULI returns imaginary part and CMULR the real part
rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
export sub cmuli( r , s , t , u)
    return (r * u + s * t)
end sub
export sub cmulr( r , s , t , u)
    return (r * t - s * u)
end sub

rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rem   CSUBI/CSUBR  subtraction of complex numbers Z1 - Z2  with Z1 = a1 + b1 *i   Z2 = a2 + b2*i
rem                CSUBI returns imaginary part and CSUBR the real part
rem ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
export sub csubi( a1 , b1 , a2 , b2)
    return (b1 - b2)
end sub
export sub csubr( a1 , b1 , a2 , b2)
    return (a1 - a2)
end sub

if (peek$("library") = "main") then
    print "Example: Z1 + Z2 with Z1 = 3 +2i , Z2 = 1-3i: Z1 + Z2 = 4 -1i"
    print caddr(3,2,1,-2), "/", caddi(3,2,1,-3)   //   4/-1
end if
```



## zkl


```zkl
var [const] GSL=Import("zklGSL");    // libGSL (GNU Scientific Library)
(GSL.Z(3,4) + GSL.Z(1,2)).println(); // (4.00+6.00i)
(GSL.Z(3,4) - GSL.Z(1,2)).println(); // (2.00+2.00i)
(GSL.Z(3,4) * GSL.Z(1,2)).println(); // (-5.00+10.00i)
(GSL.Z(3,4) / GSL.Z(1,2)).println(); // (2.20-0.40i)
(GSL.Z(1,0) / GSL.Z(1,1)).println(); // (0.50-0.50i) // inversion
(-GSL.Z(3,4)).println(); 	     // (-3.00-4.00i)
GSL.Z(3,4).conjugate().println();    // (3.00-4.00i)
```

{{out}}

```txt

(4.00+6.00i)
(2.00+2.00i)
(-5.00+10.00i)
(2.20-0.40i)
(0.50-0.50i)
(-3.00-4.00i)
(3.00-4.00i)

```



## zonnon


```zonnon

module Numbers;
type
	{public,immutable}
	Complex = record
		re,im: real;
	end Complex;

operator {public} "+" (a,b: Complex): Complex;
var
	r: Complex;
begin
	r.re := a.re + b.re;
	r.im := a.im + b.im;
	return r
end "+";

operator {public} "-" (a,b: Complex): Complex;
var
	r: Complex;
begin
	r.re := a.re - b.re;
	r.im := a.im - b.im;
	return r
end "-";

operator {public} "*" (a,b: Complex): Complex;
var
	r: Complex;
begin
	r.re := a.re*b.re - a.im*b.im;
	r.im := a.re*b.im + a.im*b.re;
	return r
end "*";

operator {public} "/" (a,b: Complex): Complex;
var
	r: Complex;
	d: real;
begin
	d := b.re * b.re + b.im * b.im;
	r.re := (a.re * b.re + a.im * b.im)/d;
	r.im := (a.im * b.re - a.re * b.im)/d;
	return r
end "/";

operator {public} "-" (a: Complex): Complex;
begin
	a.im := -1 * a.im;
	return a
end "-";

operator {public} "~" (a: Complex): Complex;
var
	d: real;
	c: Complex;
begin
	d := a.re * a.re + a.im * a.im;
	c.re := a.re/d;
	c.im := (-1.0 * a.im)/d;
	return c
end "~";

end Numbers.


module Main;
import Numbers;

var
	a,b,c: Numbers.Complex;

	procedure Writeln(c: Numbers.Complex);
	begin
		writeln("(",c.re:4:2,";",c.im:4:2,"i)");
	end Writeln;

	procedure NewComplex(x,y: real): Numbers.Complex;
	var
		r: Numbers.Complex;
	begin
		r.re := x;r.im := y;
		return r
	end NewComplex;

begin
	a := NewComplex(1.5,3.0);
	b := NewComplex(1.0,1.0);
	Writeln(a + b);
	Writeln(a - b);
	Writeln(a * b);
	Writeln(a / b);
	Writeln(-a);
	Writeln(~b);
end Main.

```

{{Out}}

```txt

   ( 2,5   ;   4  i)
   (  ,5   ;   2  i)
   (-1,5   ; 4,5  i)
   (2,25   ; ,75  i)
   ( 1,5   ;  -3  i)
   (  ,5   ; -,5  i)

```



## ZX Spectrum Basic

{{trans|BBC BASIC}}

```zxbasic
5 LET complex=2: LET r=1: LET i=2
10 DIM a(complex): LET a(r)=1.0: LET a(i)=1.0
20 DIM b(complex): LET b(r)=PI: LET b(i)=1.2
30 DIM o(complex)
40 REM add
50 LET o(r)=a(r)+b(r)
60 LET o(i)=a(i)+b(i)
70 PRINT "Result of addition is:": GO SUB 1000
80 REM mult
90 LET o(r)=a(r)*b(r)-a(i)*b(i)
100 LET o(i)=a(i)*b(r)+a(r)*b(i)
110 PRINT "Result of multiplication is:": GO SUB 1000
120 REM neg
130 LET o(r)=-a(r)
140 LET o(i)=-a(i)
150 PRINT "Result of negation is:": GO SUB 1000
160 LET denom=a(r)^2+a(i)^2
170 LET o(r)=a(r)/denom
180 LET o(i)=-a(i)/denom
190 PRINT "Result of inversion is:": GO SUB 1000
200 STOP
1000 IF o(i)>=0 THEN PRINT o(r);" + ";o(i);"i": RETURN
1010 PRINT o(r);" - ";-o(i);"i": RETURN

```

{{out}}

```txt
Result of addition is:
4.1415927 + 2.2i
Result of multiplication is:
1.9415927 + 4.3415927i
Result of negation is:
-1 - 1i
Result of inversion is:
0.5 - 0.5i
```


{{omit from|M4}}

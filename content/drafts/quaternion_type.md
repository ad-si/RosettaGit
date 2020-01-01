+++
title = "Quaternion type"
description = ""
date = 2019-09-22T06:10:54Z
aliases = []
[extra]
id = 7858
[taxonomies]
categories = []
tags = []
+++

{{task}}

[[wp:Quaternion|Quaternions]]   are an extension of the idea of   [[Arithmetic/Complex|complex numbers]].

A complex number has a real and complex part,   sometimes written as   <big> <code> a + bi, </code> </big>

where   <big> <code> a </code> </big>   and   <big> <code> b </code> </big>   stand for real numbers, and   <big> <code> i </code> </big>   stands for the square root of minus 1.

An example of a complex number might be   <big> <code> -3 + 2i, </code> </big>

where the real part,   <big> <code> a </code> </big>   is   <big> <code> '''-3.0''' </code> </big>   and the complex part,   <big> <code> b </code> </big>   is   <big> <code> '''+2.0'''. </code> </big>

A quaternion has one real part and ''three'' imaginary parts,   <big> <code> i, </code> </big>   <big> <code> j, </code> </big>   and   <big> <code> k. </code> </big>

A quaternion might be written as   <big> <code> a + bi + cj + dk. </code> </big>

In the quaternion numbering system:
:::*   <big> <code> i∙i = j∙j = k∙k = i∙j∙k = -1, </code> </big>       or more simply,
:::*   <big> <code> ii  = jj  = kk  = ijk   = -1. </code> </big>

The order of multiplication is important, as, in general, for two quaternions:
::::   <big> <code> q<sub>1</sub> </code> </big>   and   <big> <code> q<sub>2</sub>: </code> </big>     <big> <code> q<sub>1</sub>q<sub>2</sub> &ne; q<sub>2</sub>q<sub>1</sub>. </code> </big>

An example of a quaternion might be   <big> <code> 1 +2i +3j +4k </code> </big>

There is a list form of notation where just the numbers are shown and the imaginary multipliers   <big> <code>i, </code> </big>   <big> <code> j, </code> </big>   and   <big> <code> k </code> </big>   are assumed by position.

So the example above would be written as   <big> <code> (1, 2, 3, 4) </code> </big>


;Task:
Given the three quaternions and their components: <big>
    q  = (1, 2, 3, 4) = (a,<sub> </sub> b,<sub> </sub> c,<sub> </sub> d)
    q<sub>1</sub> = (2, 3, 4, 5) = (a<sub>1</sub>, b<sub>1</sub>, c<sub>1</sub>, d<sub>1</sub>)
    q<sub>2</sub> = (3, 4, 5, 6) = (a<sub>2</sub>, b<sub>2</sub>, c<sub>2</sub>, d<sub>2</sub>) </big>
And a wholly real number   <big> <code> r = 7. </code> </big>


Create functions   (or classes)   to perform simple maths with quaternions including computing:
# The norm of a quaternion:
<big><code><math>= \sqrt{a^2 + b^2 + c^2 + d^2}</math></code></big>
# The negative of a quaternion:
 <big> <code> = (-a, -b, -c, -d)</code> </big>
# The conjugate of a quaternion:
 <big> <code> = ( a, -b, -c, -d)</code> </big>
# Addition of a real number   <big> <code> r </code> </big>   and a quaternion   <big> <code> q: </code> </big>
 <big> <code> r + q = q + r = (a+r, b, c, d) </code> </big>
# Addition of two quaternions:
 <big> <code> q<sub>1</sub> + q<sub>2</sub> = (a<sub>1</sub>+a<sub>2</sub>, b<sub>1</sub>+b<sub>2</sub>, c<sub>1</sub>+c<sub>2</sub>, d<sub>1</sub>+d<sub>2</sub>) </code> </big>
# Multiplication of a real number and a quaternion:
 <big> <code> qr = rq = (ar, br, cr, dr) </code> </big>
# Multiplication of two quaternions   <big> <code> q<sub>1</sub> </code> </big>   and   <big><code>q<sub>2</sub> </code> </big>   is given by:
 <big> <code> ( a<sub>1</sub>a<sub>2</sub> − b<sub>1</sub>b<sub>2</sub> − c<sub>1</sub>c<sub>2</sub> − d<sub>1</sub>d<sub>2</sub>, </code>
 <code>   a<sub>1</sub>b<sub>2</sub> + b<sub>1</sub>a<sub>2</sub> + c<sub>1</sub>d<sub>2</sub> − d<sub>1</sub>c<sub>2</sub>, </code>
 <code>   a<sub>1</sub>c<sub>2</sub> − b<sub>1</sub>d<sub>2</sub> + c<sub>1</sub>a<sub>2</sub> + d<sub>1</sub>b<sub>2</sub>, </code>
 <code>   a<sub>1</sub>d<sub>2</sub> + b<sub>1</sub>c<sub>2</sub> − c<sub>1</sub>b<sub>2</sub> + d<sub>1</sub>a<sub>2</sub> ) </code> </big>
# Show that, for the two quaternions   <big> <code> q<sub>1</sub> </code> </big>   and   <big> <code> q<sub>2</sub>:
 q<sub>1</sub>q<sub>2</sub> &ne; q<sub>2</sub>q<sub>1</sub> </code> </big>



If a language has built-in support for quaternions, then use it.


;C.f.:
*   [[Vector products]]
*   [http://www.maths.tcd.ie/pub/HistMath/People/Hamilton/QLetter/QLetter.pdf On Quaternions];   or on a new System of Imaginaries in Algebra.   By Sir William Rowan Hamilton LL.D, P.R.I.A., F.R.A.S., Hon. M. R. Soc. Ed. and Dub., Hon. or Corr. M. of the Royal or Imperial Academies of St. Petersburgh, Berlin, Turin and Paris, Member of the American Academy of Arts and Sciences, and of other Scientific Societies at Home and Abroad, Andrews' Prof. of Astronomy in the University of Dublin, and Royal Astronomer of Ireland.





## Ada

The package specification (works with any floating-point type):

```Ada
generic
   type Real is digits <>;
package Quaternions is
   type Quaternion is record
      A, B, C, D : Real;
   end record;
   function "abs" (Left : Quaternion) return Real;
   function Conj (Left : Quaternion) return Quaternion;
   function "-" (Left : Quaternion) return Quaternion;
   function "+" (Left, Right : Quaternion) return Quaternion;
   function "-" (Left, Right : Quaternion) return Quaternion;
   function "*" (Left : Quaternion; Right : Real) return Quaternion;
   function "*" (Left : Real; Right : Quaternion) return Quaternion;
   function "*" (Left, Right : Quaternion) return Quaternion;
   function Image (Left : Quaternion) return String;
end Quaternions;
```

The package implementation:

```Ada
with Ada.Numerics.Generic_Elementary_Functions;
package body Quaternions is
   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Elementary_Functions;
   function "abs" (Left : Quaternion) return Real is
   begin
      return Sqrt (Left.A**2 + Left.B**2 + Left.C**2 + Left.D**2);
   end "abs";
   function Conj (Left : Quaternion) return Quaternion is
   begin
      return (A => Left.A, B => -Left.B, C => -Left.C, D => -Left.D);
   end Conj;
   function "-" (Left : Quaternion) return Quaternion is
   begin
      return (A => -Left.A, B => -Left.B, C => -Left.C, D => -Left.D);
   end "-";
   function "+" (Left, Right : Quaternion) return Quaternion is
   begin
      return
      (  A => Left.A + Right.A, B => Left.B + Right.B,
         C => Left.C + Right.C, D => Left.D + Right.D
      );
   end "+";
   function "-" (Left, Right : Quaternion) return Quaternion is
   begin
      return
      (  A => Left.A - Right.A, B => Left.B - Right.B,
         C => Left.C - Right.C, D => Left.D - Right.D
      );
   end "-";
   function "*" (Left : Quaternion; Right : Real) return Quaternion is
   begin
      return
      (  A => Left.A * Right, B => Left.B * Right,
         C => Left.C * Right, D => Left.D * Right
      );
   end "*";
   function "*" (Left : Real; Right : Quaternion) return Quaternion is
   begin
      return Right * Left;
   end "*";
   function "*" (Left, Right : Quaternion) return Quaternion is
   begin
      return
      (  A => Left.A * Right.A - Left.B * Right.B - Left.C * Right.C - Left.D * Right.D,
         B => Left.A * Right.B + Left.B * Right.A + Left.C * Right.D - Left.D * Right.C,
         C => Left.A * Right.C - Left.B * Right.D + Left.C * Right.A + Left.D * Right.B,
         D => Left.A * Right.D + Left.B * Right.C - Left.C * Right.B + Left.D * Right.A
      );
   end "*";
   function Image (Left : Quaternion) return String is
   begin
      return Real'Image (Left.A) & " +"  &
             Real'Image (Left.B) & "i +" &
             Real'Image (Left.C) & "j +" &
             Real'Image (Left.D) & "k";
   end Image;
end Quaternions;
```

Test program:

```Ada
with Ada.Text_IO;  use Ada.Text_IO;
with Quaternions;
procedure Test_Quaternion is
   package Float_Quaternion is new Quaternions (Float);
   use Float_Quaternion;
   q  : Quaternion := (1.0, 2.0, 3.0, 4.0);
   q1 : Quaternion := (2.0, 3.0, 4.0, 5.0);
   q2 : Quaternion := (3.0, 4.0, 5.0, 6.0);
   r  : Float      := 7.0;
begin
   Put_Line ("q = "       & Image (q));
   Put_Line ("q1 = "      & Image (q1));
   Put_Line ("q2 = "      & Image (q2));
   Put_Line ("r ="        & Float'Image (r));
   Put_Line ("abs q ="    & Float'Image (abs q));
   Put_Line ("abs q1 ="   & Float' Image (abs q1));
   Put_Line ("abs q2 ="   & Float' Image (abs q2));
   Put_Line ("-q = "      & Image (-q));
   Put_Line ("conj q = "  & Image (Conj (q)));
   Put_Line ("q1 + q2 = " & Image (q1 + q2));
   Put_Line ("q2 + q1 = " & Image (q2 + q1));
   Put_Line ("q * r = "   & Image (q * r));
   Put_Line ("r * q = "   & Image (r * q));
   Put_Line ("q1 * q2 = " & Image (q1 * q2));
   Put_Line ("q2 * q1 = " & Image (q2 * q1));
end Test_Quaternion;
```

{{out}}

```txt

q =  1.00000E+00 + 2.00000E+00i + 3.00000E+00j + 4.00000E+00k
q1 =  2.00000E+00 + 3.00000E+00i + 4.00000E+00j + 5.00000E+00k
q2 =  3.00000E+00 + 4.00000E+00i + 5.00000E+00j + 6.00000E+00k
r = 7.00000E+00
abs q = 5.47723E+00
abs q1 = 7.34847E+00
abs q2 = 9.27362E+00
-q = -1.00000E+00 +-2.00000E+00i +-3.00000E+00j +-4.00000E+00k
conj q =  1.00000E+00 +-2.00000E+00i +-3.00000E+00j +-4.00000E+00k
q1 + q2 =  5.00000E+00 + 7.00000E+00i + 9.00000E+00j + 1.10000E+01k
q2 + q1 =  5.00000E+00 + 7.00000E+00i + 9.00000E+00j + 1.10000E+01k
q * r =  7.00000E+00 + 1.40000E+01i + 2.10000E+01j + 2.80000E+01k
r * q =  7.00000E+00 + 1.40000E+01i + 2.10000E+01j + 2.80000E+01k
q1 * q2 = -5.60000E+01 + 1.60000E+01i + 2.40000E+01j + 2.60000E+01k
q2 * q1 = -5.60000E+01 + 1.80000E+01i + 2.00000E+01j + 2.80000E+01k

```



## ALGOL 68

{{trans|python}} Note: This specimen retains the original [http://rosettacode.org/mw/index.php?title=Simple_Quaternion_type_and_operations&diff=87324&oldid=87321 python] coding style.

{{works with|ALGOL 68|Revision 1 - one minor extension to language used - PRAGMA READ, similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.6 algol68g-2.6].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: prelude/Quaternion.a68'''
```algol68
# -*- coding: utf-8 -*- #

COMMENT REQUIRES:
  MODE QUATSCAL = REAL; # Scalar #
  QUATSCAL quat small scal = small real;
END COMMENT

# PROVIDES: #
FORMAT quat scal fmt := $g(-0, 4)$;
FORMAT signed fmt = $b("+", "")f(quat scal fmt)$;

FORMAT quat fmt = $f(quat scal fmt)"+"f(quat scal fmt)"i+"f(quat scal fmt)"j+"f(quat scal fmt)"k"$;
FORMAT squat fmt = $f(signed fmt)f(signed fmt)"i"f(signed fmt)"j"f(signed fmt)"k"$;

MODE QUAT = STRUCT(QUATSCAL r, i, j, k);
QUAT i=(0, 1, 0, 0),
     j=(0, 0, 1, 0),
     k=(0, 0, 0, 1);

MODE QUATCOSCAL = UNION(INT, SHORT REAL, SHORT INT);
MODE QUATSUBSCAL = UNION(QUATCOSCAL, QUATSCAL);

MODE COMPLSCAL = STRUCT(QUATSCAL r, im);
# compatable but not the same #
MODE ISOQUAT = UNION([]REAL, []INT, []SHORT REAL, []SHORT INT, []QUATSCAL);
MODE COQUAT  = UNION(COMPLSCAL, QUATCOSCAL, ISOQUAT);
MODE SUBQUAT = UNION(COQUAT, QUAT); # subset is itself #

MODE QUATERNION = QUAT;

PROC quat fix type error = (QUAT quat, []STRING msg)BOOL: (
  putf(stand error, ($"Type error:"$,$" "g$, msg, quat fmt, quat, $l$));
  stop
);

COMMENT
For a list of coercions expected in A68 c.f.
* http://rosettacode.org/wiki/ALGOL_68#Coercion_.28casting.29 # ...

Pre-Strong context: Deproceduring, dereferencing & uniting. e.g. OP arguments
  * soft(deproceduring for assignment),
  * weak(dereferencing for slicing and OF selection),
  * meek(dereferencing for indexing, enquiries and PROC calls),
  * firm(uniting of OPerators),
Strong context only: widening (INT=>REAL=>COMPL), rowing (REAL=>[]REAL) & voiding
  * strong(widening,rowing,voiding for identities/initialisations, arguments and casts et al)
Key points:
  * arguments to OPerators do not widen or row!
  * UNITING is permitted in OP/String ccontext.

There are 4 principle scenerios for most operators:
+---------------+-------------------------------+-------------------------------+
|  OP e.g. *    |  SCALar                       |  QUATernion                   |
+---------------+-------------------------------+-------------------------------+
|  SCALar       |  SCAL * SCAL ... inherit      |  SCAL * QUAT                  |
+---------------+-------------------------------+-------------------------------+
|  QUATernion   |  QUAT * SCAL                  |  QUAT * QUAT                  |
+---------------+-------------------------------+-------------------------------+
However this is compounded with SUBtypes of the SCALar & isomorphs the QUATernion,
e.g.
* SCAL may be a superset of SHORT REAL or INT - a widening coercion is required
* QUAT may be a superset eg of COMPL or [4]INT
* QUAT may be a structural isomorph eg of [4]REAL
+---------------+---------------+---------------+---------------+---------------+
|  OP e.g. *    |  SUBSCAL      |  SCALar       |  COQUAT       |  QUATernion   |
+---------------+---------------+---------------+---------------+---------------+
|  SUBSCAL      |                               |  inherit      |  SUBSCAT*QUAT |
+---------------+           inherit             +---------------+---------------+
|  SCALar       |                               |  inherit      |  SCAL * QUAT  |
+---------------+---------------+---------------+---------------+---------------+
|  COQUAT       |  inherit      |  inherit      |  inherit      |  COQUAT*QUAT  |
+---------------+---------------+---------------+---------------+---------------+
|  QUATernion   | QUAT*SUBSCAL  |  QUAT*SCAL    | QUAT * COQUAT |  QUAT * QUAT  |
+---------------+---------------+---------------+---------------+---------------+
Keypoint: if an EXPLICIT QUAT is not involved, then we can simple inherit, OR QUATINIT!
END COMMENT

MODE CLASSQUAT = STRUCT(
    PROC (REF QUAT #new#, QUATSCAL #r#, QUATSCAL #i#, QUATSCAL #j#, QUATSCAL #k#)REF QUAT new,
    PROC (REF QUAT #self#)QUAT conjugate,
    PROC (REF QUAT #self#)QUATSCAL norm sq,
    PROC (REF QUAT #self#)QUATSCAL norm,
    PROC (REF QUAT #self#)QUAT reciprocal,
    PROC (REF QUAT #self#)STRING repr,
    PROC (REF QUAT #self#)QUAT neg,
    PROC (REF QUAT #self#, SUBQUAT #other#)QUAT add,
    PROC (REF QUAT #self#, SUBQUAT #other#)QUAT radd,
    PROC (REF QUAT #self#, SUBQUAT #other#)QUAT sub,
    PROC (REF QUAT #self#, SUBQUAT #other#)QUAT mul,
    PROC (REF QUAT #self#, SUBQUAT #other#)QUAT rmul,
    PROC (REF QUAT #self#, SUBQUAT #other#)QUAT div,
    PROC (REF QUAT #self#, SUBQUAT #other#)QUAT rdiv,
    PROC (REF QUAT #self#)QUAT exp
);

CLASSQUAT class quat = (

  # PROC new =#(REF QUAT new, QUATSCAL r, i, j, k)REF QUAT: (
        # 'Defaults all parts of quaternion to zero' #
        IF new ISNT REF QUAT(NIL) THEN new ELSE HEAP QUAT FI := (r, i, j, k)
    ),

  # PROC conjugate =#(REF QUAT self)QUAT:
        (r OF self, -i OF self, -j OF self, -k OF self),

  # PROC norm sq =#(REF QUAT self)QUATSCAL:
        r OF self**2 + i OF self**2 + j OF self**2 + k OF self**2,

  # PROC norm =#(REF QUAT self)QUATSCAL:
        sqrt((norm sq OF class quat)(self)),

  # PROC reciprocal =#(REF QUAT self)QUAT:(
        QUATSCAL n2 = (norm sq OF class quat)(self);
        QUAT conj = (conjugate OF class quat)(self);
        (r OF conj/n2, i OF conj/n2, j OF conj/n2, k OF conj/n2)
    ),

  # PROC repr =#(REF QUAT self)STRING: (
        # 'Shorter form of Quaternion as string' #
        FILE f; STRING s; associate(f, s);
        putf(f, (squat fmt, r OF self>=0, r OF self,
             i OF self>=0, i OF self, j OF self>=0, j OF self, k OF self>=0, k OF self));
        close(f);
        s
    ),

  # PROC neg =#(REF QUAT self)QUAT:
        (-r OF self, -i OF self, -j OF self, -k OF self),

  # PROC add =#(REF QUAT self, SUBQUAT other)QUAT:
        CASE other IN
            (QUAT other): (r OF self + r OF other, i OF self + i OF other, j OF self + j OF other, k OF self + k OF other),
            (QUATSUBSCAL other): (r OF self + QUATSCALINIT other, i OF self, j OF self, k OF self)
        OUT IF quat fix type error(SKIP,"in add") THEN SKIP ELSE stop FI
        ESAC,

  # PROC radd =#(REF QUAT self, SUBQUAT other)QUAT:
        (add OF class quat)(self, other),

  # PROC sub =#(REF QUAT self, SUBQUAT other)QUAT:
        CASE other IN
            (QUAT other): (r OF self - r OF other, i OF self - i OF other, j OF self - j OF other, k OF self - k OF other),
            (QUATSCAL other): (r OF self - other, i OF self, j OF self, k OF self)
        OUT IF quat fix type error(self,"in sub") THEN SKIP ELSE stop FI
        ESAC,

  # PROC mul =#(REF QUAT self, SUBQUAT other)QUAT:
        CASE other IN
            (QUAT other):(
                 r OF self*r OF other - i OF self*i  OF other - j OF self*j  OF other - k OF self*k  OF other,
                 r OF self*i  OF other + i OF self*r OF other + j OF self*k  OF other - k OF self*j  OF other,
                 r OF self*j  OF other - i OF self*k  OF other + j OF self*r OF other + k OF self*i  OF other,
                 r OF self*k  OF other + i OF self*j  OF other - j OF self*i  OF other + k OF self*r OF other
            ),
            (QUATSCAL other): ( r OF self * other, i OF self * other, j OF self * other, k OF self * other)
        OUT IF quat fix type error(self,"in mul") THEN SKIP ELSE stop FI
        ESAC,

  # PROC rmul =#(REF QUAT self, SUBQUAT other)QUAT:
        CASE other IN
          (QUAT other): (mul OF class quat)(LOC QUAT := other, self),
          (QUATSCAL other): (mul OF class quat)(self, other)
        OUT IF quat fix type error(self,"in rmul") THEN SKIP ELSE stop FI
        ESAC,

  # PROC div =#(REF QUAT self, SUBQUAT other)QUAT:
        CASE other IN
            (QUAT other): (mul OF class quat)(self, (reciprocal OF class quat)(LOC QUAT := other)),
            (QUATSCAL other): (mul OF class quat)(self, 1/other)
        OUT IF quat fix type error(self,"in div") THEN SKIP ELSE stop FI
        ESAC,

  # PROC rdiv =#(REF QUAT self, SUBQUAT other)QUAT:
        CASE other IN
          (QUAT other): (div OF class quat)(LOC QUAT := other, self),
          (QUATSCAL other): (div OF class quat)(LOC QUAT := (other, 0, 0, 0), self)
        OUT IF quat fix type error(self,"in rdiv") THEN SKIP ELSE stop FI
        ESAC,

  # PROC exp =#(REF QUAT self)QUAT: (
    QUAT fac := self;
    QUAT sum := 1.0 + fac;
    FOR i FROM 2 TO bits width WHILE ABS(fac + quat small scal) /= quat small scal DO
      VOID(sum +:= (fac *:= self / ##QUATSCAL(i)))
    OD;
    sum
  )
);

PRIO INIT = 1;
OP QUATSCALINIT = (QUATSUBSCAL scal)QUATSCAL:
  CASE scal IN
    (INT scal): scal,
    (SHORT INT scal): scal,
    (SHORT REAL scal): scal
    OUT IF quat fix type error(SKIP,"in QUATSCALINIT") THEN SKIP ELSE stop FI
  ESAC;

OP INIT = (REF QUAT new, SUBQUAT from)REF QUAT:
  new :=
    CASE from IN
      (QUATSUBSCAL scal):(QUATSCALINIT scal, 0, 0, 0)
      #(COQUAT rijk):(new OF class quat)(LOC QUAT := new, rijk[1], rijk[2], rijk[3], rijk[4]),#
    OUT IF quat fix type error(SKIP,"in INIT") THEN SKIP ELSE stop FI
    ESAC;


OP QUATINIT = (COQUAT lhs)REF QUAT: (HEAP QUAT)INIT lhs;

OP +    = (QUAT q)QUAT:   q,
   -    = (QUAT q)QUAT:   (neg  OF class quat)(LOC QUAT := q),
   CONJ = (QUAT q)QUAT:   (conjugate OF class quat)(LOC QUAT := q),
   ABS  = (QUAT q)QUATSCAL:   (norm OF class quat)(LOC QUAT := q),
   REPR = (QUAT q)STRING: (repr OF class quat)(LOC QUAT := q);
# missing: Diadic: I, J, K END #

OP +:= = (REF QUAT a, QUAT b)QUAT: a:=( add OF class quat)(a, b),
   +:= = (REF QUAT a, COQUAT b)QUAT: a:=( add OF class quat)(a, b),
   +=: = (QUAT a, REF QUAT b)QUAT: b:=(radd OF class quat)(b, a),
   +=: = (COQUAT a, REF QUAT b)QUAT: b:=(radd OF class quat)(b, a);
# missing: Worthy PLUSAB, PLUSTO for SHORT/LONG INT QUATSCAL & COMPL #

OP -:= = (REF QUAT a, QUAT b)QUAT: a:=( sub OF class quat)(a, b),
   -:= = (REF QUAT a, COQUAT b)QUAT: a:=( sub OF class quat)(a, b);
# missing: Worthy MINUSAB for SHORT/LONG INT ##COQUAT & COMPL #

PRIO *=: = 1, /=: = 1;
OP *:= = (REF QUAT a, QUAT b)QUAT: a:=( mul OF class quat)(a, b),
   *:= = (REF QUAT a, COQUAT b)QUAT: a:=( mul OF class quat)(a, b),
   *=: = (QUAT a, REF QUAT b)QUAT: b:=(rmul OF class quat)(b, a),
   *=: = (COQUAT a, REF QUAT b)QUAT: b:=(rmul OF class quat)(b, a);
# missing: Worthy TIMESAB, TIMESTO for SHORT/LONG INT ##COQUAT & COMPL #

OP /:= = (REF QUAT a, QUAT b)QUAT: a:=( div OF class quat)(a, b),
   /:= = (REF QUAT a, COQUAT b)QUAT: a:=( div OF class quat)(a, b),
   /=: = (QUAT a, REF QUAT b)QUAT: b:=(rdiv OF class quat)(b, a),
   /=: = (COQUAT a, REF QUAT b)QUAT: b:=(rdiv OF class quat)(b, a);
# missing: Worthy OVERAB, OVERTO for SHORT/LONG INT ##COQUAT & COMPL #

OP + = (QUAT a, b)QUAT:      ( add OF class quat)(LOC QUAT := a, b),
   + = (QUAT a, COQUAT b)QUAT: ( add OF class quat)(LOC QUAT := a, b),
   + = (COQUAT a, QUAT b)QUAT: (radd OF class quat)(LOC QUAT := b, a);

OP - = (QUAT a, b)QUAT:      ( sub OF class quat)(LOC QUAT := a, b),
   - = (QUAT a, COQUAT b)QUAT: ( sub OF class quat)(LOC QUAT := a, b),
   - = (COQUAT a, QUAT b)QUAT:-( sub OF class quat)(LOC QUAT := b, a);

OP * = (QUAT a, b)QUAT:      ( mul OF class quat)(LOC QUAT := a, b),
   * = (QUAT a, COQUAT b)QUAT: ( mul OF class quat)(LOC QUAT := a, b),
   * = (COQUAT a, QUAT b)QUAT: (rmul OF class quat)(LOC QUAT := b, a);

OP / = (QUAT a, b)QUAT:      ( div OF class quat)(LOC QUAT := a, b),
   / = (QUAT a, COQUAT b)QUAT: ( div OF class quat)(LOC QUAT := a, b),
   / = (COQUAT a, QUAT b)QUAT:
         ( div OF class quat)(LOC QUAT := QUATINIT 1, a);

PROC quat exp = (QUAT q)QUAT:   (exp OF class quat)(LOC QUAT := q);

SKIP # missing: quat arc{sin, cos, tan}h, log, exp, ln etc END #
```
'''File: test/Quaternion.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

# REQUIRES: #
  MODE QUATSCAL = REAL; # Scalar #
  QUATSCAL quat small scal = small real;

PR READ "prelude/Quaternion.a68" PR;

test:(
    REAL r = 7;
    QUAT q  = (1, 2, 3, 4),
         q1 = (2, 3, 4, 5),
         q2 = (3, 4, 5, 6);

    printf((
        $"r = "      f(quat scal fmt)l$, r,
        $"q = "      f(quat fmt)l$, q,
        $"q1 = "     f(quat fmt)l$, q1,
        $"q2 = "     f(quat fmt)l$, q2,
        $"ABS q = "  f(quat scal fmt)", "$, ABS q,
        $"ABS q1 = " f(quat scal fmt)", "$, ABS q1,
        $"ABS q2 = " f(quat scal fmt)l$, ABS q2,
        $"-q = "     f(quat fmt)l$, -q,
        $"CONJ q = " f(quat fmt)l$, CONJ q,
        $"r + q = "  f(quat fmt)l$, r + q,
        $"q + r = "  f(quat fmt)l$, q + r,
        $"q1 + q2 = "f(quat fmt)l$, q1 + q2,
        $"q2 + q1 = "f(quat fmt)l$, q2 + q1,
        $"q * r = "  f(quat fmt)l$, q * r,
        $"r * q = "  f(quat fmt)l$, r * q,
        $"q1 * q2 = "f(quat fmt)l$, q1 * q2,
        $"q2 * q1 = "f(quat fmt)l$, q2 * q1
    ));

CO
        $"ASSERT q1 * q2 != q2 * q1 = "f(quat fmt)l$, ASSERT q1 * q2 != q2 * q1, $l$;
END CO

    printf((
        $"i*i = "         f(quat fmt)l$, i*i,
        $"j*j = "         f(quat fmt)l$, j*j,
        $"k*k = "         f(quat fmt)l$, k*k,
        $"i*j*k = "       f(quat fmt)l$, i*j*k,
        $"q1 / q2 = "     f(quat fmt)l$, q1 / q2,
        $"q1 / q2 * q2 = "f(quat fmt)l$, q1 / q2 * q2,
        $"q2 * q1 / q2 = "f(quat fmt)l$, q2 * q1 / q2,
        $"1/q1 * q1 = "   f(quat fmt)l$, 1.0/q1 * q1,
        $"q1 / q1 = "     f(quat fmt)l$, q1 / q1,
        $"quat exp(pi * i) = " f(quat fmt)l$, quat exp(pi * i),
        $"quat exp(pi * j) = " f(quat fmt)l$, quat exp(pi * j),
        $"quat exp(pi * k) = " f(quat fmt)l$, quat exp(pi * k)
    ));
    print((REPR(-q1*q2), ", ", REPR(-q2*q1), new line))
)
```

{{out}}

```txt

r = 7.0000
q = 1.0000+2.0000i+3.0000j+4.0000k
q1 = 2.0000+3.0000i+4.0000j+5.0000k
q2 = 3.0000+4.0000i+5.0000j+6.0000k
ABS q = 5.4772, ABS q1 = 7.3485, ABS q2 = 9.2736
-q = -1.0000+-2.0000i+-3.0000j+-4.0000k
CONJ q = 1.0000+-2.0000i+-3.0000j+-4.0000k
r + q = 8.0000+2.0000i+3.0000j+4.0000k
q + r = 8.0000+2.0000i+3.0000j+4.0000k
q1 + q2 = 5.0000+7.0000i+9.0000j+11.0000k
q2 + q1 = 5.0000+7.0000i+9.0000j+11.0000k
q * r = 7.0000+14.0000i+21.0000j+28.0000k
r * q = 7.0000+14.0000i+21.0000j+28.0000k
q1 * q2 = -56.0000+16.0000i+24.0000j+26.0000k
q2 * q1 = -56.0000+18.0000i+20.0000j+28.0000k
i*i = -1.0000+.0000i+.0000j+.0000k
j*j = -1.0000+.0000i+.0000j+.0000k
k*k = -1.0000+.0000i+.0000j+.0000k
i*j*k = -1.0000+.0000i+.0000j+.0000k
q1 / q2 = .7907+.0233i+-.0000j+.0465k
q1 / q2 * q2 = 2.0000+3.0000i+4.0000j+5.0000k
q2 * q1 / q2 = 2.0000+3.4651i+3.9070j+4.7674k
1/q1 * q1 = 2.0000+3.0000i+4.0000j+5.0000k
q1 / q1 = 1.0000+.0000i+.0000j+.0000k
quat exp(pi * i) = -1.0000+.0000i+.0000j+.0000k
quat exp(pi * j) = -1.0000+.0000i+.0000j+.0000k
quat exp(pi * k) = -1.0000+.0000i+.0000j+.0000k
+56.0000-16.0000i-24.0000j-26.0000k, +56.0000-18.0000i-20.0000j-28.0000k

```



## ALGOL W


```algolw
begin
    % Quaternion record type                                                 %
    record Quaternion ( real a, b, c, d );

    % returns the norm of the specified quaternion                           %
    real procedure normQ ( reference(Quaternion) value q ) ;
        sqrt( (a(q) * a(q)) + (b(q) * b(q)) + (c(q) * c(q)) + (d(q) * d(q)) );

    % returns the negative of the specified quaternion                       %
    reference(Quaternion) procedure negQ ( reference(Quaternion) value q ) ;
        Quaternion( - a(q), - b(q), - c(q), - d(q) );

    % returns the conjugate of the specified quaternion                      %
    reference(Quaternion) procedure conjQ ( reference(Quaternion) value q ) ;
        Quaternion(   a(q), - b(q), - c(q), - d(q) );

    % returns the sum of a real and a quaternion                             %
    reference(Quaternion) procedure addRQ ( real                  value r
                                          ; reference(Quaternion) value q
                                          ) ;
        Quaternion( r + a(q), b(q), c(q), d(q) );

    % returns the sum of a quaternion and a real                             %
    reference(Quaternion) procedure addQR ( reference(Quaternion) value q
                                          ; real                  value r
                                          ) ;
        Quaternion( r + a(q), b(q), c(q), d(q) );

    % returns the sum of the specified quaternions                           %
    reference(Quaternion) procedure addQQ ( reference(Quaternion) value q1
                                          ; reference(Quaternion) value q2
                                          ) ;
        Quaternion( a(q1) + a(q2), b(q1) + b(q2), c(q1) + c(q2), d(q1) + d(q2) );

    % returns the specified quaternion multiplied by a real                  %
    reference(Quaternion) procedure mulQR ( reference(Quaternion) value q
                                          ; real                  value r
                                          ) ;
        Quaternion( r * a(q), r * b(q), r * c(q), r * d(q) );

    % returns a real multiplied by the specified quaternion                  %
    reference(Quaternion) procedure mulRQ ( real                  value r
                                          ; reference(Quaternion) value q
                                          ) ;
        mulQR( q, r );

    % returns the Quaternion product of the specified quaternions            %
    reference(Quaternion) procedure mulQQ( reference(Quaternion) value q1
                                         ; reference(Quaternion) value q2
                                         ) ;
        Quaternion( (a(q1) * a(q2)) - (b(q1) * b(q2)) - (c(q1) * c(q2)) - (d(q1) * d(q2))
                  , (a(q1) * b(q2)) + (b(q1) * a(q2)) + (c(q1) * d(q2)) - (d(q1) * c(q2))
                  , (a(q1) * c(q2)) - (b(q1) * d(q2)) + (c(q1) * a(q2)) + (d(q1) * b(q2))
                  , (a(q1) * d(q2)) + (b(q1) * c(q2)) - (c(q1) * b(q2)) + (d(q1) * a(q2))
                  );

    % returns true if the two quaternions are equal, false otherwise         %
    logical procedure equalQ( reference(Quaternion) value q1
                            ; reference(Quaternion) value q2
                            ) ;
        a(q1) = a(q2) and b(q1) = b(q2) and c(q1) = c(q2) and d(q1) = d(q2);

    % writes a quaternion                                                    %
    procedure writeonQ( reference(Quaternion) value q ) ;
        writeon( "(", a(q), ", ", b(q), ", ", c(q), ", ", d(q), ")" );


    % test q1q2 = q2q1                                                       %
    reference(Quaternion) q, q1, q2;

    q  := Quaternion( 1, 2, 3, 4 );
    q1 := Quaternion( 2, 3, 4, 5 );
    q2 := Quaternion( 3, 4, 5, 6 );

    % set output format                                                      %
    s_w := 0; r_format := "A"; r_w := 5; r_d := 1;

    write( "      q:" );writeonQ( q );
    write( "     q1:" );writeonQ( q1 );
    write( "     q2:" );writeonQ( q2 );
    write( "norm  q:" );writeon( normQ( q ) );
    write( "norm q1:" );writeon( normQ( q1 ) );
    write( "norm q2:" );writeon( normQ( q2 ) );

    write( " conj q:" );writeonQ( conjQ( q ) );
    write( "    - q:" );writeonQ( negQ( q ) );
    write( "  7 + q:" );writeonQ( addRQ( 7, q ) );
    write( "  q + 9:" );writeonQ( addQR( q, 9 ) );
    write( " q + q1:" );writeonQ( addQQ( q, q1 ) );
    write( "  3 * q:" );writeonQ( mulRQ( 3, q ) );
    write( "  q * 4:" );writeonQ( mulQR( q, 4 ) );

    % check that q1q2 not = q2q1                                             %
    if equalQ( mulQQ( q1, q2 ), mulQQ( q2, q1 ) )
    then write( "q1q2 = q2q1 ??" )
    else write( "q1q2 <> q2q1" );

    write( "   q1q2:" );writeonQ( mulQQ( q1, q2 ) );
    write( "   q2q1:" );writeonQ( mulQQ( q2, q1 ) );

end.

```

{{out}}

```txt

      q:(  1.0,   2.0,   3.0,   4.0)
     q1:(  2.0,   3.0,   4.0,   5.0)
     q2:(  3.0,   4.0,   5.0,   6.0)
norm  q:  5.4
norm q1:  7.3
norm q2:  9.2
 conj q:(  1.0,  -2.0,  -3.0,  -4.0)
    - q:( -1.0,  -2.0,  -3.0,  -4.0)
  7 + q:(  8.0,   2.0,   3.0,   4.0)
  q + 9:( 10.0,   2.0,   3.0,   4.0)
 q + q1:(  3.0,   5.0,   7.0,   9.0)
  3 * q:(  3.0,   6.0,   9.0,  12.0)
  q * 4:(  4.0,   8.0,  12.0,  16.0)
q1q2 <> q2q1
   q1q2:(-56.0,  16.0,  24.0,  26.0)
   q2q1:(-56.0,  18.0,  20.0,  28.0)

```



## AutoHotkey

{{works with|AutoHotkey_L}} (AutoHotkey1.1+)

```AutoHotkey
q  := [1, 2, 3, 4]
q1 := [2, 3, 4, 5]
q2 := [3, 4, 5, 6]
r := 7

MsgBox, % "q = " PrintQ(q)
	. "`nq1 = " PrintQ(q1)
	. "`nq2 = " PrintQ(q2)
	. "`nr = " r
	. "`nNorm(q) = " Norm(q)
	. "`nNegative(q) = " PrintQ(Negative(q))
	. "`nConjugate(q) = " PrintQ(Conjugate(q))
	. "`nq + r = " PrintQ(AddR(q, r))
	. "`nq1 + q2 = " PrintQ(AddQ(q1, q2))
	. "`nq2 + q1 = " PrintQ(AddQ(q2, q1))
	. "`nqr = " PrintQ(MulR(q, r))
	. "`nq1q2 = " PrintQ(MulQ(q1, q2))
	. "`nq2q1 = " PrintQ(MulQ(q2, q1))

Norm(q) {
	return sqrt(q[1]**2 + q[2]**2 + q[3]**2 + q[4]**2)
}

Negative(q) {
	a := []
	for k, v in q
		a[A_Index] := v * -1
	return a
}

Conjugate(q) {
	a := []
	for k, v in q
		a[A_Index] := v * (A_Index = 1 ? 1 : -1)
	return a
}

AddR(q, r) {
	a := []
	for k, v in q
		a[A_Index] := v + (A_Index = 1 ? r : 0)
	return a
}

AddQ(q1, q2) {
	a := []
	for k, v in q1
		a[A_Index] := v + q2[A_Index]
	return a
}

MulR(q, r) {
	a := []
	for k, v in q
		a[A_Index] := v * r
	return a
}

MulQ(q, u) {
	a := []
	, a[1] := q[1]*u[1] - q[2]*u[2] - q[3]*u[3] - q[4]*u[4]
	, a[2] := q[1]*u[2] + q[2]*u[1] + q[3]*u[4] - q[4]*u[3]
	, a[3] := q[1]*u[3] - q[2]*u[4] + q[3]*u[1] + q[4]*u[2]
	, a[4] := q[1]*u[4] + q[2]*u[3] - q[3]*u[2] + q[4]*u[1]
	return a
}

PrintQ(q, b="(") {
	for k, v in q
		b .= v (A_Index = q.MaxIndex() ? ")" : ", ")
	return b
}
```

{{out}}

```txt
q = (1, 2, 3, 4)
q1 = (2, 3, 4, 5)
q2 = (3, 4, 5, 6)
r = 7
Norm(q) = 5.477226
Negative(q) = (-1, -2, -3, -4)
Conjugate(q) = (1, -2, -3, -4)
q + r = (8, 2, 3, 4)
q1 + q2 = (5, 7, 9, 11)
q2 + q1 = (5, 7, 9, 11)
qr = (7, 14, 21, 28)
q1q2 = (-56, 16, 24, 26)
q2q1 = (-56, 18, 20, 28)
```



## Axiom

Axiom has built-in support for quaternions.

```Axiom
qi := quatern$Quaternion(Integer);

             Type: ((Integer,Integer,Integer,Integer) -> Quaternion(Integer))
q  := qi(1,2,3,4);

                                                    Type: Quaternion(Integer)
q1 := qi(2,3,4,5);

                                                    Type: Quaternion(Integer)
q2 := qi(3,4,5,6);

                                                    Type: Quaternion(Integer)
r : Integer := 7;

                                                                Type: Integer
sqrt norm q

         +--+
   (6)  \|30
                                                        Type: AlgebraicNumber
-q

   (7)  - 1 - 2i - 3j - 4k
                                                    Type: Quaternion(Integer)
conjugate q

   (8)  1 - 2i - 3j - 4k
                                                    Type: Quaternion(Integer)
r + q

   (9)  8 + 2i + 3j + 4k
                                                    Type: Quaternion(Integer)
q1 + q2

   (10)  5 + 7i + 9j + 11k
                                                    Type: Quaternion(Integer)
q*r

   (11)  7 + 14i + 21j + 28k
                                                    Type: Quaternion(Integer)
r*q

   (12)  7 + 14i + 21j + 28k
                                                    Type: Quaternion(Integer)
q1*q2 ~= q2*q1

   (13)  true
                                                                Type: Boolean
```


## BBC BASIC

Although BBC BASIC doesn't have native support for quaternions its array arithmetic provides all of the required operations either directly or very straightforwardly.

```bbcbasic
      DIM q(3), q1(3), q2(3), t(3)
      q() = 1, 2, 3, 4
      q1() = 2, 3, 4, 5
      q2() = 3, 4, 5, 6
      r = 7

      PRINT "q = " FNq_show(q())
      PRINT "q1 = " FNq_show(q1())
      PRINT "q2 = " FNq_show(q2())
      PRINT "r = "; r
      PRINT "norm(q) = "; FNq_norm(q())
      t() = q() : PROCq_neg(t()) : PRINT "neg(q) = " FNq_show(t())
      t() = q() : PROCq_conj(t()) : PRINT "conjugate(q) = " FNq_show(t())
      t() = q() : PROCq_addreal(t(),r) : PRINT "q + r = " FNq_show(t())
      t() = q1() : PROCq_add(t(),q2()) : PRINT "q1 + q2 = " FNq_show(t())
      t() = q2() : PROCq_add(t(),q1()) : PRINT "q2 + q1 = " FNq_show(t())
      t() = q() : PROCq_mulreal(t(),r) : PRINT "qr = " FNq_show(t())
      t() = q1() : PROCq_mul(t(),q2()) : PRINT "q1q2 = " FNq_show(t())
      t() = q2() : PROCq_mul(t(),q1()) : PRINT "q2q1 = " FNq_show(t())
      END

      DEF FNq_norm(q()) = MOD(q())

      DEF PROCq_neg(q()) : q() *= -1 : ENDPROC

      DEF PROCq_conj(q()) : q() *= -1 : q(0) *= -1 : ENDPROC

      DEF PROCq_addreal(q(), r) : q(0) += r : ENDPROC

      DEF PROCq_add(q(), r()) : q() += r() : ENDPROC

      DEF PROCq_mulreal(q(), r) : q() *= r : ENDPROC

      DEF PROCq_mul(q(), r()) : LOCAL s() : DIM s(3,3)
      s() = r(0), -r(1), -r(2), -r(3), r(1), r(0),  r(3), -r(2), \
      \     r(2), -r(3),  r(0),  r(1), r(3), r(2), -r(1),  r(0)
      q() = s() . q()
      ENDPROC

      DEF FNq_show(q()) : LOCAL i%, a$ : a$ = "("
      FOR i% = 0 TO 3 : a$ += STR$(q(i%)) + ", " : NEXT
      = LEFT$(LEFT$(a$)) + ")"
```

{{out}}

```txt

q = (1, 2, 3, 4)
q1 = (2, 3, 4, 5)
q2 = (3, 4, 5, 6)
r = 7
norm(q) = 5.47722558
neg(q) = (-1, -2, -3, -4)
conjugate(q) = (1, -2, -3, -4)
q + r = (8, 2, 3, 4)
q1 + q2 = (5, 7, 9, 11)
q2 + q1 = (5, 7, 9, 11)
qr = (7, 14, 21, 28)
q1q2 = (-56, 16, 24, 26)
q2q1 = (-56, 18, 20, 28)

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

typedef struct quaternion
{
  double q[4];
} quaternion_t;


quaternion_t *quaternion_new(void)
{
  return malloc(sizeof(quaternion_t));
}

quaternion_t *quaternion_new_set(double q1,
				 double q2,
				 double q3,
				 double q4)
{
  quaternion_t *q = malloc(sizeof(quaternion_t));
  if (q != NULL) {
    q->q[0] = q1; q->q[1] = q2; q->q[2] = q3; q->q[3] = q4;
  }
  return q;
}


void quaternion_copy(quaternion_t *r, quaternion_t *q)
{
  size_t i;

  if (r == NULL || q == NULL) return;
  for(i = 0; i < 4; i++) r->q[i] = q->q[i];
}


double quaternion_norm(quaternion_t *q)
{
  size_t i;
  double r = 0.0;

  if (q == NULL) {
    fprintf(stderr, "NULL quaternion in norm\n");
    return 0.0;
  }

  for(i = 0; i < 4; i++) r += q->q[i] * q->q[i];
  return sqrt(r);
}


void quaternion_neg(quaternion_t *r, quaternion_t *q)
{
  size_t i;

  if (q == NULL || r == NULL) return;
  for(i = 0; i < 4; i++) r->q[i] = -q->q[i];
}


void quaternion_conj(quaternion_t *r, quaternion_t *q)
{
  size_t i;

  if (q == NULL || r == NULL) return;
  r->q[0] = q->q[0];
  for(i = 1; i < 4; i++) r->q[i] = -q->q[i];
}


void quaternion_add_d(quaternion_t *r, quaternion_t *q, double d)
{
  if (q == NULL || r == NULL) return;
  quaternion_copy(r, q);
  r->q[0] += d;
}


void quaternion_add(quaternion_t *r, quaternion_t *a, quaternion_t *b)
{
  size_t i;

  if (r == NULL || a == NULL || b == NULL) return;
  for(i = 0; i < 4; i++) r->q[i] = a->q[i] + b->q[i];
}


void quaternion_mul_d(quaternion_t *r, quaternion_t *q, double d)
{
  size_t i;

  if (r == NULL || q == NULL) return;
  for(i = 0; i < 4; i++) r->q[i] = q->q[i] * d;
}

bool quaternion_equal(quaternion_t *a, quaternion_t *b)
{
  size_t i;

  for(i = 0; i < 4; i++) if (a->q[i] != b->q[i]) return false;
  return true;
}


#define A(N) (a->q[(N)])
#define B(N) (b->q[(N)])
#define R(N) (r->q[(N)])
void quaternion_mul(quaternion_t *r, quaternion_t *a, quaternion_t *b)
{
  size_t i;
  double ri = 0.0;

  if (r == NULL || a == NULL || b == NULL) return;
  R(0) = A(0)*B(0) - A(1)*B(1) - A(2)*B(2) - A(3)*B(3);
  R(1) = A(0)*B(1) + A(1)*B(0) + A(2)*B(3) - A(3)*B(2);
  R(2) = A(0)*B(2) - A(1)*B(3) + A(2)*B(0) + A(3)*B(1);
  R(3) = A(0)*B(3) + A(1)*B(2) - A(2)*B(1) + A(3)*B(0);
}
#undef A
#undef B
#undef R


void quaternion_print(quaternion_t *q)
{
  if (q == NULL) return;
  printf("(%lf, %lf, %lf, %lf)\n",
	 q->q[0], q->q[1], q->q[2], q->q[3]);
}
```



```c
int main()
{
  size_t i;
  double d = 7.0;
  quaternion_t *q[3];
  quaternion_t *r  = quaternion_new();

  quaternion_t *qd = quaternion_new_set(7.0, 0.0, 0.0, 0.0);
  q[0] = quaternion_new_set(1.0, 2.0, 3.0, 4.0);
  q[1] = quaternion_new_set(2.0, 3.0, 4.0, 5.0);
  q[2] = quaternion_new_set(3.0, 4.0, 5.0, 6.0);

  printf("r = %lf\n", d);

  for(i = 0; i < 3; i++) {
    printf("q[%u] = ", i);
    quaternion_print(q[i]);
    printf("abs q[%u] = %lf\n", i, quaternion_norm(q[i]));
  }

  printf("-q[0] = ");
  quaternion_neg(r, q[0]);
  quaternion_print(r);

  printf("conj q[0] = ");
  quaternion_conj(r, q[0]);
  quaternion_print(r);

  printf("q[1] + q[2] = ");
  quaternion_add(r, q[1], q[2]);
  quaternion_print(r);

  printf("q[2] + q[1] = ");
  quaternion_add(r, q[2], q[1]);
  quaternion_print(r);


  printf("q[0] * r = ");
  quaternion_mul_d(r, q[0], d);
  quaternion_print(r);

  printf("q[0] * (r, 0, 0, 0) = ");
  quaternion_mul(r, q[0], qd);
  quaternion_print(r);


  printf("q[1] * q[2] = ");
  quaternion_mul(r, q[1], q[2]);
  quaternion_print(r);

  printf("q[2] * q[1] = ");
  quaternion_mul(r, q[2], q[1]);
  quaternion_print(r);


  free(q[0]); free(q[1]); free(q[2]); free(r);
  return EXIT_SUCCESS;
}
```



## C++


This example uses templates to provide the underlying data-type, and includes several extra functions and constructors that often come up when using quaternions.


```cpp
#include <iostream>
using namespace std;

template<class T = double>
class Quaternion
{
public:
  T w, x, y, z;

  // Numerical constructor
  Quaternion(const T &w, const T &x, const T &y, const T &z): w(w), x(x), y(y), z(z) {};
  Quaternion(const T &x, const T &y, const T &z): w(T()), x(x), y(y), z(z) {}; // For 3-rotations
  Quaternion(const T &r): w(r), x(T()), y(T()), z(T()) {};
  Quaternion(): w(T()), x(T()), y(T()), z(T()) {};

  // Copy constructor and assignment
  Quaternion(const Quaternion &q): w(q.w), x(q.x), y(q.y), z(q.z) {};
  Quaternion& operator=(const Quaternion &q) { w=q.w; x=q.x; y=q.y; z=q.z; return *this; }

  // Unary operators
  Quaternion operator-() const { return Quaternion(-w, -x, -y, -z); }
  Quaternion operator~() const { return Quaternion(w, -x, -y, -z); } // Conjugate

  // Norm-squared. SQRT would have to be made generic to be used here
  T normSquared() const { return w*w + x*x + y*y + z*z; }

  // In-place operators
  Quaternion& operator+=(const T &r)
    { w += r; return *this; }
  Quaternion& operator+=(const Quaternion &q)
    { w += q.w; x += q.x; y += q.y; z += q.z; return *this; }

  Quaternion& operator-=(const T &r)
    { w -= r; return *this; }
  Quaternion& operator-=(const Quaternion &q)
    { w -= q.w; x -= q.x; y -= q.y; z -= q.z; return *this; }

  Quaternion& operator*=(const T &r)
    { w *= r; x *= r; y *= r; z *= r; return *this; }
  Quaternion& operator*=(const Quaternion &q)
  {
    T oldW(w), oldX(x), oldY(y), oldZ(z);
    w = oldW*q.w - oldX*q.x - oldY*q.y - oldZ*q.z;
    x = oldW*q.x + oldX*q.w + oldY*q.z - oldZ*q.y;
    y = oldW*q.y + oldY*q.w + oldZ*q.x - oldX*q.z;
    z = oldW*q.z + oldZ*q.w + oldX*q.y - oldY*q.x;
    return *this;
  }

  Quaternion& operator/=(const T &r)
    { w /= r; x /= r; y /= r; z /= r; return *this; }
  Quaternion& operator/=(const Quaternion &q)
  {
    T oldW(w), oldX(x), oldY(y), oldZ(z), n(q.normSquared());
    w = (oldW*q.w + oldX*q.x + oldY*q.y + oldZ*q.z) / n;
    x = (oldX*q.w - oldW*q.x + oldY*q.z - oldZ*q.y) / n;
    y = (oldY*q.w - oldW*q.y + oldZ*q.x - oldX*q.z) / n;
    z = (oldZ*q.w - oldW*q.z + oldX*q.y - oldY*q.x) / n;
    return *this;
  }

  // Binary operators based on in-place operators
  Quaternion operator+(const T &r) const { return Quaternion(*this) += r; }
  Quaternion operator+(const Quaternion &q) const { return Quaternion(*this) += q; }
  Quaternion operator-(const T &r) const { return Quaternion(*this) -= r; }
  Quaternion operator-(const Quaternion &q) const { return Quaternion(*this) -= q; }
  Quaternion operator*(const T &r) const { return Quaternion(*this) *= r; }
  Quaternion operator*(const Quaternion &q) const { return Quaternion(*this) *= q; }
  Quaternion operator/(const T &r) const { return Quaternion(*this) /= r; }
  Quaternion operator/(const Quaternion &q) const { return Quaternion(*this) /= q; }

  // Comparison operators, as much as they make sense
  bool operator==(const Quaternion &q) const
    { return (w == q.w) && (x == q.x) && (y == q.y) && (z == q.z); }
  bool operator!=(const Quaternion &q) const { return !operator==(q); }

  // The operators above allow quaternion op real. These allow real op quaternion.
  // Uses the above where appropriate.
  template<class T> friend Quaternion<T> operator+(const T &r, const Quaternion<T> &q);
  template<class T> friend Quaternion<T> operator-(const T &r, const Quaternion<T> &q);
  template<class T> friend Quaternion<T> operator*(const T &r, const Quaternion<T> &q);
  template<class T> friend Quaternion<T> operator/(const T &r, const Quaternion<T> &q);

  // Allows cout << q
  template<class T> friend ostream& operator<<(ostream &io, const Quaternion<T> &q);
};

// Friend functions need to be outside the actual class definition
template<class T>
Quaternion<T> operator+(const T &r, const Quaternion<T> &q)
  { return q+r; }

template<class T>
Quaternion<T> operator-(const T &r, const Quaternion<T> &q)
  { return Quaternion<T>(r-q.w, q.x, q.y, q.z); }

template<class T>
Quaternion<T> operator*(const T &r, const Quaternion<T> &q)
  { return q*r; }

template<class T>
Quaternion<T> operator/(const T &r, const Quaternion<T> &q)
{
  T n(q.normSquared());
  return Quaternion(r*q.w/n, -r*q.x/n, -r*q.y/n, -r*q.z/n);
}

template<class T>
ostream& operator<<(ostream &io, const Quaternion<T> &q)
{
  io << q.w;
  (q.x < T()) ? (io << " - " << (-q.x) << "i") : (io << " + " << q.x << "i");
  (q.y < T()) ? (io << " - " << (-q.y) << "j") : (io << " + " << q.y << "j");
  (q.z < T()) ? (io << " - " << (-q.z) << "k") : (io << " + " << q.z << "k");
  return io;
}
```


Test program:

```cpp
int main()
{
  Quaternion<> q0(1, 2, 3, 4);
  Quaternion<> q1(2, 3, 4, 5);
  Quaternion<> q2(3, 4, 5, 6);
  double r = 7;

  cout << "q0:      " << q0 << endl;
  cout << "q1:      " << q1 << endl;
  cout << "q2:      " << q2 << endl;
  cout << "r:       " << r << endl;
  cout << endl;
  cout << "-q0:     " << -q0 << endl;
  cout << "~q0:     " << ~q0 << endl;
  cout << endl;
  cout << "r * q0:  " << r*q0 << endl;
  cout << "r + q0:  " << r+q0 << endl;
  cout << "q0 / r:  " << q0/r << endl;
  cout << "q0 - r:  " << q0-r << endl;
  cout << endl;
  cout << "q0 + q1: " << q0+q1 << endl;
  cout << "q0 - q1: " << q0-q1 << endl;
  cout << "q0 * q1: " << q0*q1 << endl;
  cout << "q0 / q1: " << q0/q1 << endl;
  cout << endl;
  cout << "q0 * ~q0:     " << q0*~q0 << endl;
  cout << "q0 + q1*q2:   " << q0+q1*q2 << endl;
  cout << "(q0 + q1)*q2: " << (q0+q1)*q2 << endl;
  cout << "q0*q1*q2:     " << q0*q1*q2 << endl;
  cout << "(q0*q1)*q2:   " << (q0*q1)*q2 << endl;
  cout << "q0*(q1*q2):   " << q0*(q1*q2) << endl;
  cout << endl;
  cout << "||q0||:  " << sqrt(q0.normSquared()) << endl;
  cout << endl;
  cout << "q0*q1 - q1*q0: " << (q0*q1 - q1*q0) << endl;

  // Other base types
  Quaternion<int> q5(2), q6(3);
  cout << endl << q5*q6 << endl;
}
```


{{out}}

```txt

q0:      1 + 2i + 3j + 4k
q1:      2 + 3i + 4j + 5k
q2:      3 + 4i + 5j + 6k
r:       7

-q0:     -1 - 2i - 3j - 4k
~q0:     1 - 2i - 3j - 4k

r * q0:  7 + 14i + 21j + 28k
r + q0:  8 + 2i + 3j + 4k
q0 / r:  0.142857 + 0.285714i + 0.428571j + 0.571429k
q0 - r:  -6 + 2i + 3j + 4k

q0 + q1: 3 + 5i + 7j + 9k
q0 - q1: -1 - 1i - 1j - 1k
q0 * q1: -36 + 6i + 12j + 12k
q0 / q1: 0.740741 + 0i + 0.0740741j + 0.037037k

q0 * ~q0:     30 + 0i + 0j + 0k
q0 + q1*q2:   -55 + 18i + 27j + 30k
(q0 + q1)*q2: -100 + 24i + 42j + 42k
q0*q1*q2:     -264 - 114i - 132j - 198k
(q0*q1)*q2:   -264 - 114i - 132j - 198k
q0*(q1*q2):   -264 - 114i - 132j - 198k

||q0||:  5.47723

q0*q1 - q1*q0: 0 - 2i + 4j - 2k

6 + 0i + 0j + 0k

```



## C sharp


```csharp
using System;

struct Quaternion : IEquatable<Quaternion>
{
    public readonly double A, B, C, D;

    public Quaternion(double a, double b, double c, double d)
    {
        this.A = a;
        this.B = b;
        this.C = c;
        this.D = d;
    }

    public double Norm()
    {
        return Math.Sqrt(A * A + B * B + C * C + D * D);
    }

    public static Quaternion operator -(Quaternion q)
    {
        return new Quaternion(-q.A, -q.B, -q.C, -q.D);
    }

    public Quaternion Conjugate()
    {
        return new Quaternion(A, -B, -C, -D);
    }

    // implicit conversion takes care of real*quaternion and real+quaternion
    public static implicit operator Quaternion(double d)
    {
        return new Quaternion(d, 0, 0, 0);
    }

    public static Quaternion operator +(Quaternion q1, Quaternion q2)
    {
        return new Quaternion(q1.A + q2.A, q1.B + q2.B, q1.C + q2.C, q1.D + q2.D);
    }

    public static Quaternion operator *(Quaternion q1, Quaternion q2)
    {
        return new Quaternion(
            q1.A * q2.A - q1.B * q2.B - q1.C * q2.C - q1.D * q2.D,
            q1.A * q2.B + q1.B * q2.A + q1.C * q2.D - q1.D * q2.C,
            q1.A * q2.C - q1.B * q2.D + q1.C * q2.A + q1.D * q2.B,
            q1.A * q2.D + q1.B * q2.C - q1.C * q2.B + q1.D * q2.A);
    }

    public static bool operator ==(Quaternion q1, Quaternion q2)
    {
        return q1.A == q2.A && q1.B == q2.B && q1.C == q2.C && q1.D == q2.D;
    }

    public static bool operator !=(Quaternion q1, Quaternion q2)
    {
        return !(q1 == q2);
    }

    #region Object Members

    public override bool Equals(object obj)
    {
        if (obj is Quaternion)
            return Equals((Quaternion)obj);

        return false;
    }

    public override int GetHashCode()
    {
        return A.GetHashCode() ^ B.GetHashCode() ^ C.GetHashCode() ^ D.GetHashCode();
    }

    public override string ToString()
    {
        return string.Format("Q({0}, {1}, {2}, {3})", A, B, C, D);
    }

    #endregion

    #region IEquatable<Quaternion> Members

    public bool Equals(Quaternion other)
    {
        return other == this;
    }

    #endregion
}
```


Demonstration:

```csharp
using System;

static class Program
{
    static void Main(string[] args)
    {
        Quaternion q = new Quaternion(1, 2, 3, 4);
        Quaternion q1 = new Quaternion(2, 3, 4, 5);
        Quaternion q2 = new Quaternion(3, 4, 5, 6);
        double r = 7;

        Console.WriteLine("q = {0}", q);
        Console.WriteLine("q1 = {0}", q1);
        Console.WriteLine("q2 = {0}", q2);
        Console.WriteLine("r = {0}", r);

        Console.WriteLine("q.Norm() = {0}", q.Norm());
        Console.WriteLine("q1.Norm() = {0}", q1.Norm());
        Console.WriteLine("q2.Norm() = {0}", q2.Norm());

        Console.WriteLine("-q = {0}", -q);
        Console.WriteLine("q.Conjugate() = {0}", q.Conjugate());

        Console.WriteLine("q + r = {0}", q + r);
        Console.WriteLine("q1 + q2 = {0}", q1 + q2);
        Console.WriteLine("q2 + q1 = {0}", q2 + q1);

        Console.WriteLine("q * r = {0}", q * r);
        Console.WriteLine("q1 * q2 = {0}", q1 * q2);
        Console.WriteLine("q2 * q1 = {0}", q2 * q1);

        Console.WriteLine("q1*q2 {0} q2*q1", (q1 * q2) == (q2 * q1) ? "==" : "!=");
    }
}
```


{{out}}

```txt
q = Q(1, 2, 3, 4)
q1 = Q(2, 3, 4, 5)
q2 = Q(3, 4, 5, 6)
r = 7
q.Norm() = 5.47722557505166
q1.Norm() = 7.34846922834953
q2.Norm() = 9.2736184954957
-q = Q(-1, -2, -3, -4)
q.Conjugate() = Q(1, -2, -3, -4)
q + r = Q(8, 2, 3, 4)
q1 + q2 = Q(5, 7, 9, 11)
q2 + q1 = Q(5, 7, 9, 11)
q * r = Q(7, 14, 21, 28)
q1 * q2 = Q(-56, 16, 24, 26)
q2 * q1 = Q(-56, 18, 20, 28)
q1*q2 != q2*q1
```



## Common Lisp


```lisp

(defclass quaternion () ((a :accessor q-a :initarg :a :type real)
                         (b :accessor q-b :initarg :b :type real)
                         (c :accessor q-c :initarg :c :type real)
                         (d :accessor q-d :initarg :d :type real))
  (:default-initargs :a 0 :b 0 :c 0 :d 0))

(defun make-q (&optional (a 0) (b 0) (c 0) (d 0))
  (make-instance 'quaternion :a a :b b :c c :d d))

(defgeneric sum (x y))

(defmethod sum ((x quaternion) (y quaternion))
  (make-q  (+ (q-a x) (q-a y))
           (+ (q-b x) (q-b y))
           (+ (q-c x) (q-c y))
           (+ (q-d x) (q-d y))))

(defmethod sum ((x quaternion) (y real))
  (make-q  (+ (q-a x) y) (q-b x) (q-c x) (q-d x)))

(defmethod sum ((x real) (y quaternion))
  (make-q  (+ (q-a y) x) (q-b y) (q-c y) (q-d y)))

(defgeneric sub (x y))

(defmethod sub ((x quaternion) (y quaternion))
  (make-q  (- (q-a x) (q-a y))
           (- (q-b x) (q-b y))
           (- (q-c x) (q-c y))
           (- (q-d x) (q-d y))))

(defmethod sub ((x quaternion) (y real))
  (make-q  (- (q-a x) y)
           (q-b x)
           (q-c x)
           (q-d x)))

(defmethod sub ((x real) (y quaternion))
  (make-q  (- (q-a y) x)
           (q-b y)
           (q-c y)
           (q-d y)))

(defgeneric mul (x y))

(defmethod mul ((x quaternion) (y real))
  (make-q  (* (q-a x) y)
           (* (q-b x) y)
           (* (q-c x) y)
           (* (q-d x) y)))

(defmethod mul ((x real) (y quaternion))
  (make-q  (* (q-a y) x)
           (* (q-b y) x)
           (* (q-c y) x)
           (* (q-d y) x)))

(defmethod mul ((x quaternion) (y quaternion))
  (make-q  (- (* (q-a x) (q-a y)) (* (q-b x) (q-b y)) (* (q-c x) (q-c y)) (* (q-d x) (q-d y)))
           (- (+ (* (q-a x) (q-b y)) (* (q-b x) (q-a y)) (* (q-c x) (q-d y))) (* (q-d x) (q-c y)))
           (- (+ (* (q-a x) (q-c y)) (* (q-c x) (q-a y)) (* (q-d x) (q-b y))) (* (q-b x) (q-d y)))
           (- (+ (* (q-a x) (q-d y)) (* (q-b x) (q-c y)) (* (q-d x) (q-a y))) (* (q-c x) (q-b y)))))

(defmethod norm ((x quaternion))
  (+ (sqrt (q-a x)) (sqrt (q-b x)) (sqrt (q-c x)) (sqrt (q-d x))))

(defmethod print-object ((x quaternion) stream)
  (format stream "~@f~@fi~@fj~@fk" (q-a x) (q-b x) (q-c x) (q-d x)))

(defvar q (make-q 0 1 0 0))
(defvar q1 (make-q 0 0 1 0))
(defvar q2 (make-q 0 0 0 1))
(defvar r 7)
(format t "q+q1+q2 = ~a~&" (reduce #'sum (list q q1 q2)))
(format t "r*(q+q1+q2) = ~a~&" (mul r (reduce #'sum (list q q1 q2))))
(format t "q*q1*q2 = ~a~&" (reduce #'mul (list q q1 q2)))
(format t "q-q1-q2 = ~a~&" (reduce #'sub (list q q1 q2)))

```


{{out}}

```txt

q+q1+q2 = +0.0+1.0i+1.0j+1.0k
r*(q+q1+q2) = +0.0+7.0i+7.0j+7.0k
q*q1*q2 = -1.0+0.0i+0.0j+0.0k
q-q1-q2 = +0.0+1.0i-1.0j-1.0k

```



## D


```d
import std.math, std.numeric, std.traits, std.conv, std.complex;


struct Quat(T) if (isFloatingPoint!T) {
    alias CT = Complex!T;

    union {
        struct { T re, i, j, k; } // Default init to NaN.
        struct { CT x, y; }
        struct { T[4] vector; }
    }

    string toString() const pure /*nothrow*/ @safe {
        return vector.text;
    }

    @property T norm2() const pure nothrow @safe @nogc { /// Norm squared.
        return re ^^ 2 + i ^^ 2 + j ^^ 2 + k ^^ 2;
    }

    @property T abs() const pure nothrow @safe @nogc { /// Norm.
        return sqrt(norm2);
    }

    @property T arg() const pure nothrow @safe @nogc { /// Theta.
        return acos(re / abs); // this may be incorrect...
    }

    @property Quat!T conj() const pure nothrow @safe @nogc { /// Conjugate.
        return Quat!T(re, -i, -j, -k);
    }

    @property Quat!T recip() const pure nothrow @safe @nogc {  /// Reciprocal.
        return Quat!T(re / norm2, -i / norm2, -j / norm2, -k / norm2);
    }

    @property Quat!T pureim() const pure nothrow @safe @nogc { /// Pure imagery.
        return Quat!T(0, i, j, k);
    }

    @property Quat!T versor() const pure nothrow @safe @nogc { /// Unit versor.
        return this / abs;
    }

    /// Unit versor of imagery part.
    @property Quat!T iversor() const pure nothrow @safe @nogc {
        return pureim / pureim.abs;
    }

    /// Assignment.
    Quat!T opAssign(U : T)(Quat!U z) pure nothrow @safe @nogc {
        x = z.x;  y = z.y;
        return this;
    }

    Quat!T opAssign(U : T)(Complex!U c) pure nothrow @safe @nogc {
        x = c;  y = 0;
        return this;
    }

    Quat!T opAssign(U : T)(U r) pure nothrow @safe @nogc
    if (isNumeric!U) {
        re = r; i = 0; y = 0;
        return this;
    }

    /// Test for equal, not ordered so no opCmp.
    bool opEquals(U : T)(Quat!U z) const pure nothrow @safe @nogc {
        return re == z.re && i == z.i && j == z.j && k == z.k;
    }

    bool opEquals(U : T)(Complex!U c) const pure nothrow @safe @nogc {
        return re == c.re && i == c.im && j == 0 && k == 0;
    }

    bool opEquals(U : T)(U r) const pure nothrow @safe @nogc
    if (isNumeric!U) {
        return re == r && i == 0 && j == 0 && k == 0;
    }

    /// Unary op.
    Quat!T opUnary(string op)() const pure nothrow @safe @nogc
    if (op == "+") {
        return this;
    }

    Quat!T opUnary(string op)() const pure nothrow @safe @nogc
    if (op == "-") {
        return Quat!T(-re, -i, -j, -k);
    }

    /// Binary op, Quaternion on left of op.
    Quat!(CommonType!(T,U)) opBinary(string op, U)(Quat!U z)
    const pure nothrow @safe @nogc {
        alias typeof(return) C;

        static if (op == "+" ) {
            return C(re + z.re, i + z.i, j + z.j, k + z.k);
        } else static if (op == "-") {
            return C(re - z.re, i - z.i, j - z.j, k - z.k);
        } else static if (op == "*") {
            return C(re * z.re - i * z.i  - j * z.j  - k * z.k,
                     re * z.i  + i * z.re + j * z.k  - k * z.j,
                     re * z.j  - i * z.k  + j * z.re + k * z.i,
                     re * z.k  + i * z.j  - j * z.i  + k * z.re);
        } else static if (op == "/") {
            return this * z.recip;
        }
    }

    /// Extend complex to quaternion.
    Quat!(CommonType!(T,U)) opBinary(string op, U)(Complex!U c)
    const pure nothrow @safe @nogc {
        return opBinary!op(typeof(return)(c.re, c.im, 0, 0));
    }

    /// For scalar.
    Quat!(CommonType!(T,U)) opBinary(string op, U)(U r)
    const pure nothrow @safe @nogc
    if (isNumeric!U) {
        alias typeof(return) C;

        static if (op == "+" ) {
            return C(re + r, i, j, k);
        } else static if (op == "-") {
            return C(re - r, i, j, k);
        } else static if (op == "*") {
            return C(re * r, i * r, j * r, k * r);
        } else static if (op == "/") {
            return C(re / r, i / r, j / r, k / r);
        } else static if (op == "^^") {
            return pow(r);
        }
    }

    /// Power function.
    Quat!(CommonType!(T,U)) pow(U)(U r)
    const pure nothrow @safe @nogc
    if (isNumeric!U) {
        return (abs^^r) * exp(r * iversor * arg);
    }

    /// Handle binary op if Quaternion on right of op and left is
    /// not quaternion.
    Quat!(CommonType!(T,U)) opBinaryRight(string op, U)(Complex!U c)
    const pure nothrow @safe @nogc {
        alias typeof(return) C;
        auto w = C(c.re, c.im, 0, 0);
        return w.opBinary!(op)(this);
    }

    Quat!(CommonType!(T,U)) opBinaryRight(string op, U)(U r)
    const pure nothrow @safe @nogc
    if (isNumeric!U) {
        alias typeof(return) C;

        static if (op == "+" || op == "*") {
            return opBinary!op(r);
        } else static if (op == "-") {
            return C(r - re , -i, -j, -k);
        } else static if (op == "/") {
            auto w = C(re, i, j, k);
            return w.recip * r;
        }
    }
}


HT exp(HT)(HT z) pure nothrow @safe @nogc
if (is(HT T == Quat!T)) {
    immutable inorm = z.pureim.abs;
    return std.math.exp(z.re) * (cos(inorm) + z.iversor * sin(inorm));
}

HT log(HT)(HT z) pure nothrow @safe @nogc
if (is(HT T == Quat!T)) {
    return std.math.log(z.abs) + z.iversor * acos(z.re / z.abs);
}


void main() @safe { // Demo code.
    import std.stdio;

    alias QR = Quat!real;
    enum real r = 7.0;

    immutable QR q  = QR(2, 3, 4, 5),
                 q1 = QR(2, 3, 4, 5),
                 q2 = QR(3, 4, 5, 6);

    writeln("1.             q - norm: ", q.abs);
    writeln("2.         q - negative: ", -q);
    writeln("3.        q - conjugate: ", q.conj);
    writeln("4.                r + q: ", r + q);
    writeln("                  q + r: ", q + r);
    writeln("5.              q1 + q2: ", q1 + q2);
    writeln("6.                r * q: ", r * q);
    writeln("                  q * r: ", q * r);
    writeln("7.              q1 * q2: ", q1 * q2);
    writeln("                q2 * q1: ", q2 * q1);
    writeln("8.  q1 * q2 != q2 * Q1 ? ", q1 * q2 != q2 * q1);

    immutable QR i = QR(0, 1, 0, 0),
                 j = QR(0, 0, 1, 0),
                 k = QR(0, 0, 0, 1);
    writeln("9.1               i * i: ", i * i);
    writeln("                  J * j: ", j * j);
    writeln("                  k * k: ", k * k);
    writeln("              i * j * k: ", i * j * k);
    writeln("9.2             q1 / q2: ", q1 / q2);
    writeln("9.3        q1 / q2 * q2: ", q1 / q2 * q2);
    writeln("           q2 * q1 / q2: ", q2 * q1 / q2);
    writeln("9.4         exp(pi * i): ", exp(PI * i));
    writeln("            exp(pi * j): ", exp(PI * j));
    writeln("            exp(pi * k): ", exp(PI * k));
    writeln("                 exp(q): ", exp(q));
    writeln("                 log(q): ", log(q));
    writeln("            exp(log(q)): ", exp(log(q)));
    writeln("            log(exp(q)): ", log(exp(q)));
    immutable s = q.exp.log;
    writeln("9.5 let s = log(exp(q)): ", s);
    writeln("                 exp(s): ", exp(s));
    writeln("                 log(s): ", log(s));
    writeln("            exp(log(s)): ", exp(log(s)));
    writeln("            log(exp(s)): ", log(exp(s)));
}
```

{{out}}

```txt
1.             q - norm: 7.34847
2.         q - negative: [-2, -3, -4, -5]
3.        q - conjugate: [2, -3, -4, -5]
4.                r + q: [9, 3, 4, 5]
                  q + r: [9, 3, 4, 5]
5.              q1 + q2: [5, 7, 9, 11]
6.                r * q: [14, 21, 28, 35]
                  q * r: [14, 21, 28, 35]
7.              q1 * q2: [-56, 16, 24, 26]
                q2 * q1: [-56, 18, 20, 28]
8.  q1 * q2 != q2 * Q1 ? true
9.1               i * i: [-1, 0, 0, 0]
                  J * j: [-1, 0, 0, 0]
                  k * k: [-1, 0, 0, 0]
              i * j * k: [-1, 0, 0, 0]
9.2             q1 / q2: [0.790698, 0.0232558, -1.35525e-20, 0.0465116]
9.3        q1 / q2 * q2: [2, 3, 4, 5]
           q2 * q1 / q2: [2, 3.46512, 3.90698, 4.76744]
9.4         exp(pi * i): [-1, -5.42101e-20, -0, -0]
            exp(pi * j): [-1, -0, -5.42101e-20, -0]
            exp(pi * k): [-1, -0, -0, -5.42101e-20]
                 exp(q): [5.21186, 2.22222, 2.96296, 3.7037]
                 log(q): [1.99449, 0.549487, 0.732649, 0.915812]
            exp(log(q)): [2, 3, 4, 5]
            log(exp(q)): [2, 0.33427, 0.445694, 0.557117]
9.5 let s = log(exp(q)): [2, 0.33427, 0.445694, 0.557117]
                 exp(s): [5.21186, 2.22222, 2.96296, 3.7037]
                 log(s): [0.765279, 0.159215, 0.212286, 0.265358]
            exp(log(s)): [2, 0.33427, 0.445694, 0.557117]
            log(exp(s)): [2, 0.33427, 0.445694, 0.557117]
```



## Delphi



```Delphi
unit Quaternions;

interface

type

  TQuaternion = record
    A, B, C, D: double;

    function  Init          (aA, aB, aC, aD : double): TQuaternion;
    function  Norm          : double;
    function  Conjugate     : TQuaternion;
    function  ToString      : string;

    class operator Negative (Left : TQuaternion): TQuaternion;
    class operator Positive (Left : TQuaternion): TQuaternion;
    class operator Add      (Left, Right : TQuaternion): TQuaternion;
    class operator Add      (Left : TQuaternion; Right : double): TQuaternion; overload;
    class operator Add      (Left : double; Right : TQuaternion): TQuaternion; overload;
    class operator Subtract (Left, Right : TQuaternion): TQuaternion;
    class operator Multiply (Left, Right : TQuaternion): TQuaternion;
    class operator Multiply (Left : TQuaternion; Right : double): TQuaternion; overload;
    class operator Multiply (Left : double; Right : TQuaternion): TQuaternion; overload;
  end;

implementation

uses
  SysUtils;

{ TQuaternion }

function TQuaternion.Init(aA, aB, aC, aD: double): TQuaternion;
begin
  A := aA;
  B := aB;
  C := aC;
  D := aD;

  result := Self;
end;

function TQuaternion.Norm: double;
begin
  result := sqrt(sqr(A) + sqr(B) + sqr(C) + sqr(D));
end;

function TQuaternion.Conjugate: TQuaternion;
begin
  result.B := -B;
  result.C := -C;
  result.D := -D;
end;

class operator TQuaternion.Negative(Left: TQuaternion): TQuaternion;
begin
  result.A := -Left.A;
  result.B := -Left.B;
  result.C := -Left.C;
  result.D := -Left.D;
end;

class operator TQuaternion.Positive(Left: TQuaternion): TQuaternion;
begin
  result := Left;
end;

class operator TQuaternion.Add(Left, Right: TQuaternion): TQuaternion;
begin
  result.A := Left.A + Right.A;
  result.B := Left.B + Right.B;
  result.C := Left.C + Right.C;
  result.D := Left.D + Right.D;
end;

class operator TQuaternion.Add(Left: TQuaternion; Right: double): TQuaternion;
begin
  result.A := Left.A + Right;
  result.B := Left.B;
  result.C := Left.C;
  result.D := Left.D;
end;

class operator TQuaternion.Add(Left: double; Right: TQuaternion): TQuaternion;
begin
  result.A := Left + Right.A;
  result.B := Right.B;
  result.C := Right.C;
  result.D := Right.D;
end;

class operator TQuaternion.Subtract(Left, Right: TQuaternion): TQuaternion;
begin
  result.A := Left.A - Right.A;
  result.B := Left.B - Right.B;
  result.C := Left.C - Right.C;
  result.D := Left.D - Right.D;
end;

class operator TQuaternion.Multiply(Left, Right: TQuaternion): TQuaternion;
begin
  result.A := Left.A * Right.A - Left.B * Right.B - Left.C * Right.C - Left.D * Right.D;
  result.B := Left.A * Right.B + Left.B * Right.A + Left.C * Right.D - Left.D * Right.C;
  result.C := Left.A * Right.C - Left.B * Right.D + Left.C * Right.A + Left.D * Right.B;
  result.D := Left.A * Right.D + Left.B * Right.C - Left.C * Right.B + Left.D * Right.A;
end;

class operator TQuaternion.Multiply(Left: double; Right: TQuaternion): TQuaternion;
begin
  result.A := Left * Right.A;
  result.B := Left * Right.B;
  result.C := Left * Right.C;
  result.D := Left * Right.D;
end;

class operator TQuaternion.Multiply(Left: TQuaternion; Right: double): TQuaternion;
begin
  result.A := Left.A * Right;
  result.B := Left.B * Right;
  result.C := Left.C * Right;
  result.D := Left.D * Right;
end;

function TQuaternion.ToString: string;
begin
  result := Format('%f + %fi + %fj + %fk', [A, B, C, D]);
end;

end.
```


Test program

```Delphi
program QuaternionTest;

{$APPTYPE CONSOLE}

uses
  Quaternions in 'Quaternions.pas';

var
  r : double;
  q, q1, q2 : TQuaternion;
begin
  r := 7;
  q  := q .Init(1, 2, 3, 4);
  q1 := q1.Init(2, 3, 4, 5);
  q2 := q2.Init(3, 4, 5, 6);

  writeln('q  = ', q.ToString);
  writeln('q1 = ', q1.ToString);
  writeln('q2 = ', q2.ToString);
  writeln('r  = ', r);
  writeln('Norm(q ) = ', q.Norm);
  writeln('Norm(q1) = ', q1.Norm);
  writeln('Norm(q2) = ', q2.Norm);
  writeln('-q = ', (-q).ToString);
  writeln('Conjugate q = ', q.Conjugate.ToString);
  writeln('q1 + q2 = ', (q1 + q2).ToString);
  writeln('q2 + q1 = ', (q2 + q1).ToString);
  writeln('q * r   = ', (q * r).ToString);
  writeln('r * q   = ', (r * q).ToString);
  writeln('q1 * q2 = ', (q1 * q2).ToString);
  writeln('q2 * q1 = ', (q2 * q1).ToString);
end.
```


{{out}}

```txt

q  = 1.00 + 2.00i + 3.00j + 4.00k
q1 = 2.00 + 3.00i + 4.00j + 5.00k
q2 = 3.00 + 4.00i + 5.00j + 6.00k
r  =  7.00000000000000E+0000
Norm(q ) =  5.47722557505166E+0000
Norm(q1) =  7.34846922834953E+0000
Norm(q2) =  9.27361849549570E+0000
-q = -1.00 + -2.00i + -3.00j + -4.00k
Conjugate q = -1.00 + -2.00i + -3.00j + -4.00k
q1 + q2 = 5.00 + 7.00i + 9.00j + 11.00k
q2 + q1 = 5.00 + 7.00i + 9.00j + 11.00k
q * r   = 7.00 + 14.00i + 21.00j + 28.00k
r * q   = 7.00 + 14.00i + 21.00j + 28.00k
q1 * q2 = -56.00 + 16.00i + 24.00j + 26.00k
q2 * q1 = -56.00 + 18.00i + 20.00j + 28.00k

```


--[[User:Davidizadar|DavidIzadaR]] 20:33, 7 August 2011 (UTC)


## E



```e
interface Quaternion guards QS {}
def makeQuaternion(a, b, c, d) {
    return def quaternion implements QS {

        to __printOn(out) {
            out.print("(", a, " + ", b, "i + ")
            out.print(c, "j + ", d, "k)")
        }

        # Task requirement 1
        to norm() {
            return (a**2 + b**2 + c**2 + d**2).sqrt()
        }

        # Task requirement 2
        to negate() {
            return makeQuaternion(-a, -b, -c, -d)
        }

        # Task requirement 3
        to conjugate() {
            return makeQuaternion(a, -b, -c, -d)
        }

        # Task requirement 4, 5
        # This implements q + r; r + q is deliberately prohibited by E
        to add(other :any[Quaternion, int, float64]) {
            switch (other) {
              match q :Quaternion {
                return makeQuaternion(
                    a+q.a(), b+q.b(), c+q.c(), d+q.d())
              }
              match real {
                return makeQuaternion(a+real, b, c, d)
              }
            }
        }

        # Task requirement 6, 7
        # This implements q * r; r * q is deliberately prohibited by E
        to multiply(other :any[Quaternion, int, float64]) {
            switch (other) {
                match q :Quaternion {
                    return makeQuaternion(
                        a*q.a() - b*q.b() - c*q.c() - d*q.d(),
                        a*q.b() + b*q.a() + c*q.d() - d*q.c(),
                        a*q.c() - b*q.d() + c*q.a() + d*q.b(),
                        a*q.d() + b*q.c() - c*q.b() + d*q.a())
                    }
                match real {
                    return makeQuaternion(real*a, real*b, real*c, real*d)
                }
            }
        }

        to a() { return a }
        to b() { return b }
        to c() { return c }
        to d() { return d }
    }
}
```



```e
? def q1 := makeQuaternion(2,3,4,5)
# value: (2 + 3i + 4j + 5k)

? def q2 := makeQuaternion(3,4,5,6)
# value: (3 + 4i + 5j + 6k)

? q1+q2
# value: (5 + 7i + 9j + 11k)

? q1*q2
# value: (-56 + 16i + 24j + 26k)

? q2*q1
# value: (-56 + 18i + 20j + 28k)

? q1+(-2)
# value: (0 + 3i + 4j + 5k)
```



## Eero


```objc>#import <Foundation/Foundation.h


interface Quaternion : Number
  // Properties -- note that this is an immutable class.
  double real, i, j, k {readonly}
end

implementation Quaternion

  initWithReal: double, i: double, j: double, k: double, return instancetype
    self = super.init
    if self
      _real = real; _i = i; _j = j; _k = k
    return self

  +new: double real, ..., return instancetype
    va_list args
    va_start(args, real)
    object := Quaternion.alloc.initWithReal: real,
                                          i: va_arg(args, double),
                                          j: va_arg(args, double),
                                          k: va_arg(args, double)
    va_end(args)
    return object

  descriptionWithLocale: id, return String = String.stringWithFormat:
      '(%.1f, %.1f, %.1f, %.1f)', self.real, self.i, self.j, self.k

  norm, return double =
      sqrt(self.real * self.real +
           self.i * self.i + self.j * self.j + self.k * self.k)

  negative, return Quaternion =
      Quaternion.new: -self.real, -self.i, -self.j, -self.k

  conjugate, return Quaternion =
      Quaternion.new: self.real, -self.i, -self.j, -self.k

  // Overload "+" operator (left operand is Quaternion)
  plus: Number operand, return Quaternion
    real := self.real, i = self.i, j = self.j, k = self.k
    if operand.isKindOfClass: Quaternion.class
      q := (Quaternion)operand
      real += q.real; i += q.i; j += q.j; k += q.k
    else
      real += (double)operand
    return Quaternion.new: real, i, j, k

  // Overload "*" operator (left operand is Quaternion)
  multipliedBy: Number operand, return Quaternion
    real := self.real, i = self.i, j = self.j, k = self.k
    if operand.isKindOfClass: Quaternion.class
      q := (Quaternion)operand
      real = self.real * q.real - self.i* q.i - self.j * q.j - self.k * q.k
      i = self.real * q.i + self.i * q.real + self.j * q.k - self.k * q.j
      j = self.real * q.j - self.i * q.k + self.j * q.real + self.k * q.i
      k = self.real * q.k + self.i * q.j - self.j * q.i + self.k * q.real
    else
      real *= (double)operand
      i *= (double)operand; j *= (double)operand; k *= (double)operand
    return Quaternion.new: real, i, j, k

end

implementation Number (QuaternionOperators)

  // Overload "+" operator (left operand is Number)
  plus: Quaternion operand, return Quaternion
    real := (double)self + operand.real
    return Quaternion.new: real, operand.i, operand.j, operand.k

  // Overload "*" operator (left operand is Number)
  multipliedBy: Quaternion operand, return Quaternion
    r := (double)self
    return Quaternion.new: r * operand.real, r * operand.i,
                           r * operand.j, r * operand.k

end

int main()
  autoreleasepool

    q  := Quaternion.new: 1.0, 2.0, 3.0, 4.0
    q1 := Quaternion.new: 2.0, 3.0, 4.0, 5.0
    q2 := Quaternion.new: 3.0, 4.0, 5.0, 6.0

    Log( 'q  = %@', q )
    Log( 'q1 = %@', q1 )
    Log( 'q2 = %@\n\n', q2 )

    Log( 'q norm = %.3f',  q.norm )
    Log( 'q negative = %@',  q.negative )
    Log( 'q conjugate = %@',  q.conjugate )
    Log( '7 + q = %@', 7.0 + q )
    Log( 'q + 7 = %@', q + 7.0 )
    Log( 'q1 + q2 = %@',  q1 + q2 )
    Log( '7 * q = %@', 7 * q)
    Log( 'q * 7 = %@', q * 7.0 )
    Log( 'q1 * q2 = %@',  q1 * q2 )
    Log( 'q2 * q1 = %@',  q2 * q1 )

  return 0
```


{{out}}

```txt

2013-09-04 16:40:29.818 a.out[2170:507] q  = (1.0, 2.0, 3.0, 4.0)
2013-09-04 16:40:29.819 a.out[2170:507] q1 = (2.0, 3.0, 4.0, 5.0)
2013-09-04 16:40:29.820 a.out[2170:507] q2 = (3.0, 4.0, 5.0, 6.0)

2013-09-04 16:40:29.820 a.out[2170:507] q norm = 5.477
2013-09-04 16:40:29.820 a.out[2170:507] q negative = (-1.0, -2.0, -3.0, -4.0)
2013-09-04 16:40:29.820 a.out[2170:507] q conjugate = (1.0, -2.0, -3.0, -4.0)
2013-09-04 16:40:29.821 a.out[2170:507] 7 + q = (8.0, 2.0, 3.0, 4.0)
2013-09-04 16:40:29.821 a.out[2170:507] q + 7 = (8.0, 2.0, 3.0, 4.0)
2013-09-04 16:40:29.821 a.out[2170:507] q1 + q2 = (5.0, 7.0, 9.0, 11.0)
2013-09-04 16:40:29.821 a.out[2170:507] 7 * q = (7.0, 14.0, 21.0, 28.0)
2013-09-04 16:40:29.821 a.out[2170:507] q * 7 = (7.0, 14.0, 21.0, 28.0)
2013-09-04 16:40:29.822 a.out[2170:507] q1 * q2 = (-56.0, 16.0, 24.0, 26.0)
2013-09-04 16:40:29.822 a.out[2170:507] q2 * q1 = (-56.0, 18.0, 20.0, 28.0)
```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import system'math;
import extensions;
import extensions'text;

struct Quaternion : BaseValue
{
    rprop real A;
    rprop real B;
    rprop real C;
    rprop real D;

    constructor new(a, b, c, d)
        <= new(cast real(a), cast real(b), cast real(c), cast real(d));

    constructor new(real a, real b, real c, real d)
    {
        A := a;
        B := b;
        C := c;
        D := d
    }

    constructor(real r)
    {
        A := r;
        B := 0.0r;
        C := 0.0r;
        D := 0.0r
    }

    real Norm = (A*A + B*B + C*C + D*D).sqrt();

    Quaternion Negative = Quaternion.new(A.Negative,B.Negative,C.Negative,D.Negative);

    Quaternion Conjugate = Quaternion.new(A,B.Negative,C.Negative,D.Negative);

    Quaternion add(Quaternion q)
        = Quaternion.new(A + q.A, B + q.B, C + q.C, D + q.D);

    Quaternion multiply(Quaternion q)
        = Quaternion.new(
            A * q.A - B * q.B - C * q.C - D * q.D,
            A * q.B + B * q.A + C * q.D - D * q.C,
            A * q.C - B * q.D + C * q.A + D * q.B,
            A * q.D + B * q.C - C * q.B + D * q.A);

    Quaternion add(real r)
        <= add(Quaternion.new(r,0,0,0));

    Quaternion multiply(real r)
        <= multiply(Quaternion.new(r,0,0,0));

    bool equal(Quaternion q)
        = (A == q.A) && (B == q.B) && (C == q.C) && (D == q.D);

    string Printable
        = new StringWriter().printFormatted("Q({0}, {1}, {2}, {3})",A,B,C,D);
}

public program()
{
    auto q := Quaternion.new(1,2,3,4);
    auto q1 := Quaternion.new(2,3,4,5);
    auto q2 := Quaternion.new(3,4,5,6);
    real r := 7;

    console.printLine("q = ", q);
    console.printLine("q1 = ", q1);
    console.printLine("q2 = ", q2);
    console.printLine("r = ", r);

    console.printLine("q.Norm() = ", q.Norm);
    console.printLine("q1.Norm() = ", q1.Norm);
    console.printLine("q2.Norm() = ", q2.Norm);

    console.printLine("-q = ", q.Negative);
    console.printLine("q.Conjugate() = ", q.Conjugate);

    console.printLine("q + r = ", q + r);
    console.printLine("q1 + q2 = ", q1 + q2);
    console.printLine("q2 + q1 = ", q2 + q1);

    console.printLine("q * r = ", q * r);
    console.printLine("q1 * q2 = ", q1 * q2);
    console.printLine("q2 * q1 = ", q2 * q1);

    console.printLineFormatted("q1*q2 {0} q2*q1", ((q1 * q2) == (q2 * q1)).iif("==","!="))
}
```

{{out}}

```txt

q = Q(1.0, 2.0, 3.0, 4.0)
q1 = Q(2.0, 3.0, 4.0, 5.0)
q2 = Q(3.0, 4.0, 5.0, 6.0)
r = 7.0
q.Norm() = 5.477225575052
q1.Norm() = 7.34846922835
q2.Norm() = 9.273618495496
-q = Q(-1.0, -2.0, -3.0, -4.0)
q.Conjugate() = Q(1.0, -2.0, -3.0, -4.0)
q + r = Q(8.0, 2.0, 3.0, 4.0)
q1 + q2 = Q(5.0, 7.0, 9.0, 11.0)
q2 + q1 = Q(5.0, 7.0, 9.0, 11.0)
q * r = Q(7.0, 14.0, 21.0, 28.0)
q1 * q2 = Q(-56.0, 16.0, 24.0, 26.0)
q2 * q1 = Q(-56.0, 18.0, 20.0, 28.0)
q1*q2 != q2*q1

```



## ERRE


```ERRE

PROGRAM QUATERNION

!$DOUBLE

TYPE QUATERNION=(A,B,C,D)

DIM Q:QUATERNION,Q1:QUATERNION,Q2:QUATERNION


DIM R:QUATERNION,S:QUATERNION,T:QUATERNION

PROCEDURE NORM(T.->NORM)
   NORM=SQR(T.A*T.A+T.B*T.B+T.C*T.C+T.D*T.D)
END PROCEDURE

PROCEDURE NEGATIVE(T.->T.)
    T.A=-T.A
    T.B=-T.B
    T.C=-T.C
    T.D=-T.D
END PROCEDURE

PROCEDURE CONJUGATE(T.->T.)
    T.A=T.A
    T.B=-T.B
    T.C=-T.C
    T.D=-T.D
END PROCEDURE

PROCEDURE ADD_REAL(T.,REAL->T.)
    T.A=T.A+REAL
    T.B=T.B
    T.C=T.C
    T.D=T.D
END PROCEDURE

PROCEDURE ADD(T.,S.->T.)
    T.A=T.A+S.A
    T.B=T.B+S.B
    T.C=T.C+S.C
    T.D=T.D+S.D
END PROCEDURE

PROCEDURE MULT_REAL(T.,REAL->T.)
    T.A=T.A*REAL
    T.B=T.B*REAL
    T.C=T.C*REAL
    T.D=T.D*REAL
END PROCEDURE

PROCEDURE MULT(T.,S.->R.)
    R.A=T.A*S.A-T.B*S.B-T.C*S.C-T.D*S.D
    R.B=T.A*S.B+T.B*S.A+T.C*S.D-T.D*S.C
    R.C=T.A*S.C-T.B*S.D+T.C*S.A+T.D*S.B
    R.D=T.A*S.D+T.B*S.C-T.C*S.B+T.D*S.A
END PROCEDURE

PROCEDURE PRINTQ(T.)
    PRINT("(";T.A;",";T.B;",";T.C;",";T.D;")")
END PROCEDURE

BEGIN
    Q.A=1  Q.B=2  Q.C=3  Q.D=4
    Q1.A=2 Q1.B=3 Q1.C=4 Q1.D=5
    Q2.A=3 Q2.B=4 Q2.C=5 Q2.D=6
    REAL=7

    NORM(Q.->NORM)
    PRINT("Norm(q)=";NORM)

    NEGATIVE(Q.->T.)
    PRINT("Negative(q) =";)
    PRINTQ(T.)

    CONJUGATE(Q.->T.)
    PRINT("Conjugate(q) =";)
    PRINTQ(T.)

    ADD_REAL(Q.,REAL->T.)
    PRINT("q + real =";)
    PRINTQ(T.)

! addition is commutative
    ADD(Q1.,Q2.->T.)
    PRINT("q1 + q2 =";)
    PRINTQ(T.)

    ADD(Q2.,Q1.->T.)
    PRINT("q2 + q1 = ";)
    PRINTQ(T.)

    MULT_REAL(Q.,REAL->T.)
    PRINT("q * real =";)
    PRINTQ(T.)

! multiplication is not commutative
    MULT(Q1.,Q2.->R.)
    PRINT("q1 * q2=";)
    PRINTQ(R.)

    MULT(Q2.,Q1.->R.)
    PRINT("q2 * q1=";)
    PRINTQ(R.)
END PROGRAM

```



## Euphoria


```euphoria
function norm(sequence q)
    return sqrt(power(q[1],2)+power(q[2],2)+power(q[3],2)+power(q[4],2))
end function

function conj(sequence q)
    q[2..4] = -q[2..4]
    return q
end function

function add(object q1, object q2)
    if atom(q1) != atom(q2) then
        if atom(q1) then
            q1 = {q1,0,0,0}
        else
            q2 = {q2,0,0,0}
        end if
    end if
    return q1+q2
end function

function mul(object q1, object q2)
    if sequence(q1) and sequence(q2) then
        return { q1[1]*q2[1] - q1[2]*q2[2] - q1[3]*q2[3] - q1[4]*q2[4],
                 q1[1]*q2[2] + q1[2]*q2[1] + q1[3]*q2[4] - q1[4]*q2[3],
                 q1[1]*q2[3] - q1[2]*q2[4] + q1[3]*q2[1] + q1[4]*q2[2],
                 q1[1]*q2[4] + q1[2]*q2[3] - q1[3]*q2[2] + q1[4]*q2[1] }
    else
        return q1*q2
    end if
end function

function quats(sequence q)
    return sprintf("%g + %gi + %gj + %gk",q)
end function

constant
    q  = {1, 2, 3, 4},
    q1 = {2, 3, 4, 5},
    q2 = {5, 6, 7, 8},
    r  = 7

printf(1, "norm(q) = %g\n", norm(q))
printf(1, "-q = %s\n", {quats(-q)})
printf(1, "conj(q) = %s\n", {quats(conj(q))})
printf(1, "q + r = %s\n", {quats(add(q,r))})
printf(1, "q1 + q2 = %s\n", {quats(add(q1,q2))})
printf(1, "q1 * q2 = %s\n", {quats(mul(q1,q2))})
printf(1, "q2 * q1 = %s\n", {quats(mul(q2,q1))})
```


{{out}}

```txt
norm(q) = 5.47723
-q = -1 + -2i + -3j + -4k
conj(q) = 1 + -2i + -3j + -4k
q + r = 8 + 2i + 3j + 4k
q1 + q2 = 7 + 9i + 11j + 13k
q1 * q2 = -76 + 24i + 40j + 38k
q2 * q1 = -76 + 30i + 28j + 44k
```


=={{header|F_Sharp|F#}}==
Mainly a {{trans|C#}} On the minus side we have no way to define a conversion to Quaternion from any suitable (numeric) type.
On the plus side we can avoid the stuff to make the equality structual (from the referential equality default) by just declaring it as an attribute to the type and let the compiler handle the details.

```fsharp
open System

[<Struct; StructuralEquality; NoComparison>]
type Quaternion(r : float, i : float, j : float, k : float) =
    member this.A = r
    member this.B = i
    member this.C = j
    member this.D = k

    new (f : float) = Quaternion(f, 0., 0., 0.)

    static member (~-) (q : Quaternion) = Quaternion(-q.A, -q.B, -q.C, -q.D)

    static member (+) (q1 : Quaternion, q2 : Quaternion) =
        Quaternion(q1.A + q2.A, q1.B + q2.B, q1.C + q2.C, q1.D + q2.D)
    static member (+) (q : Quaternion, r : float) = q + Quaternion(r)
    static member (+) (r : float, q: Quaternion) = Quaternion(r) + q

    static member (*) (q1 : Quaternion, q2 : Quaternion) =
        Quaternion(
            q1.A * q2.A - q1.B * q2.B - q1.C * q2.C - q1.D * q2.D,
            q1.A * q2.B + q1.B * q2.A + q1.C * q2.D - q1.D * q2.C,
            q1.A * q2.C - q1.B * q2.D + q1.C * q2.A + q1.D * q2.B,
            q1.A * q2.D + q1.B * q2.C - q1.C * q2.B + q1.D * q2.A)
    static member (*) (q : Quaternion, r : float) = q * Quaternion(r)
    static member (*) (r : float, q: Quaternion) = Quaternion(r) * q

    member this.Norm = Math.Sqrt(r * r + i * i + j * j + k * k)

    member this.Conjugate = Quaternion(r, -i, -j, -k)

    override this.ToString() = sprintf "Q(%f, %f, %f, %f)" r i j k

[<EntryPoint>]
let main argv =
    let q = Quaternion(1., 2., 3., 4.)
    let q1 = Quaternion(2., 3., 4., 5.)
    let q2 = Quaternion(3., 4., 5., 6.)
    let r = 7.

    printfn "q = %A" q
    printfn "q1 = %A" q1
    printfn "q2 = %A" q2
    printfn "r = %A" r

    printfn "q.Norm = %A" q.Norm
    printfn "q1.Norm = %A" q1.Norm
    printfn "q2.Norm = %A" q2.Norm

    printfn "-q = %A" -q
    printfn "q.Conjugate = %A" q.Conjugate

    printfn "q + r = %A" (q + (Quaternion r))
    printfn "q1 + q2 = %A" (q1 + q2)
    printfn "q2 + q1 = %A" (q2 + q1)

    printfn "q * r = %A" (q * r)
    printfn "q1 * q2 = %A" (q1 * q2)
    printfn "q2 * q1 = %A" (q2 * q1)

    printfn "q1*q2 %s q2*q1" (if (q1 * q2) = (q2 * q1) then "=" else "<>")
    printfn "q %s Q(1.,2.,3.,4.)" (if q = Quaternion(1., 2., 3., 4.) then "=" else "<>")
    0
```

{{out}}

```txt
q = Q(1.000000, 2.000000, 3.000000, 4.000000)
q1 = Q(2.000000, 3.000000, 4.000000, 5.000000)
q2 = Q(3.000000, 4.000000, 5.000000, 6.000000)
r = 7.0
q.Norm = 5.477225575
q1.Norm = 7.348469228
q2.Norm = 9.273618495
-q = Q(-1.000000, -2.000000, -3.000000, -4.000000)
q.Conjugate = Q(1.000000, -2.000000, -3.000000, -4.000000)
q + r = Q(8.000000, 2.000000, 3.000000, 4.000000)
q1 + q2 = Q(5.000000, 7.000000, 9.000000, 11.000000)
q2 + q1 = Q(5.000000, 7.000000, 9.000000, 11.000000)
q * r = Q(7.000000, 14.000000, 21.000000, 28.000000)
q1 * q2 = Q(-56.000000, 16.000000, 24.000000, 26.000000)
q2 * q1 = Q(-56.000000, 18.000000, 20.000000, 28.000000)
q1*q2 <> q2*q1
q = Q(1.,2.,3.,4.)
```



## Factor

The <code>math.quaternions</code> vocabulary provides words for treating sequences like quaternions. <code>norm</code> and <code>vneg</code> come from the <code>math.vectors</code> vocabulary. Oddly, I wasn't able to find a word for adding a real to a quaternion, so I wrote one.

```factor
USING: generalizations io kernel locals math.quaternions
math.vectors prettyprint sequences ;
IN: rosetta-code.quaternion-type

: show ( quot -- )
    [ unparse 2 tail but-last "= " append write ] [ call . ] bi
    ; inline

: 2show ( quots -- )
    [ 2curry show ] map-compose [ call ] each ; inline

: q+n ( q n -- q+n ) n>q q+ ;

[let
    { 1 2 3 4 } 7 { 2 3 4 5 } { 3 4 5 6 } :> ( q r q1 q2 )
    q [ norm ]
    q [ vneg ]
    q [ qconjugate ]
    [ curry show ] 2tri@
    {
        [ q  r  [ q+n ] ]
        [ q  r  [ q*n ] ]
        [ q1 q2 [ q+  ] ]
        [ q1 q2 [ q*  ] ]
        [ q2 q1 [ q*  ] ]
    } 2show
]
```

{{out}}

```txt

{ 1 2 3 4 } norm = 5.477225575051661
{ 1 2 3 4 } vneg = { -1 -2 -3 -4 }
{ 1 2 3 4 } qconjugate = { 1 -2 -3 -4 }
{ 1 2 3 4 } 7 q+n = { 8 2 3 4 }
{ 1 2 3 4 } 7 q*n = { 7 14 21 28 }
{ 2 3 4 5 } { 3 4 5 6 } q+ = { 5 7 9 11 }
{ 2 3 4 5 } { 3 4 5 6 } q* = { -56 16 24 26 }
{ 3 4 5 6 } { 2 3 4 5 } q* = { -56 18 20 28 }

```



## Forth


```forth
: quaternions  4 * floats ;

: qvariable create 1 quaternions allot ;

: q! ( a b c d q -- )
  dup 3 floats + f!  dup 2 floats + f!  dup float+ f!  f! ;

: qcopy ( src dest -- ) 1 quaternions move ;

: qnorm  ( q -- f )
  0e 4 0 do  dup f@ fdup f* f+  float+ loop drop fsqrt ;

: qf* ( q f -- )
  4 0 do dup f@ fover f* dup f!  float+ loop fdrop drop ;

: qnegate ( q -- )  -1e qf* ;

: qconj ( q -- )
  float+ 3 0 do dup f@ fnegate dup f!  float+ loop drop ;

: qf+ ( q f -- ) dup f@ f+ f! ;

: q+ ( q1 q2 -- )
  4 0 do over f@ dup f@ f+ dup f!  float+ swap float+ swap loop 2drop ;

\ access
: q.a             f@ ;
: q.b      float+ f@ ;
: q.c  2 floats + f@ ;
: q.d  3 floats + f@ ;

: q* ( dest q1 q2 -- )
  over q.a dup q.d f*  over q.b dup q.c f* f+  over q.c dup q.b f* f-  over q.d dup q.a f* f+
  over q.a dup q.c f*  over q.b dup q.d f* f-  over q.c dup q.a f* f+  over q.d dup q.b f* f+
  over q.a dup q.b f*  over q.b dup q.a f* f+  over q.c dup q.d f* f+  over q.d dup q.c f* f-
  over q.a dup q.a f*  over q.b dup q.b f* f-  over q.c dup q.c f* f-  over q.d dup q.d f* f-
  2drop  4 0 do dup f!  float+ loop  drop ;

: q= ( q1 q2 -- ? )
  4 0 do
    over f@ dup f@ f<> if 2drop false unloop exit then
    float+ swap float+
  loop
  2drop true ;

\ testing

: q. ( q -- )
  [char] ( emit space
  4 0 do dup f@ f.  float+ loop drop
  [char] ) emit space ;

qvariable q   1e 2e 3e 4e q  q!
qvariable q1  2e 3e 4e 5e q1 q!
create q2     3e f, 4e f, 5e f, 6e f,	\ by hand

qvariable tmp
qvariable m1
qvariable m2

q qnorm f.				\ 5.47722557505166
q tmp qcopy  tmp qnegate  tmp q.	\ ( -1. -2. -3. -4. )
q tmp qcopy  tmp qconj    tmp q.	\ ( 1. -2. -3. -4. )

q m1 qcopy  m1 7e qf+   m1 q.		\ ( 8. 2. 3. 4. )
q m2 qcopy  7e m2 qf+   m2 q.		\ ( 8. 2. 3. 4. )
m1 m2 q= .				\ -1  (true)

q2 tmp qcopy  q1 tmp q+   tmp q.	\ ( 5. 7. 9. 11. )

q m1 qcopy  m1 7e qf*     m1 q.		\ ( 7. 14. 21. 28. )
q m2 qcopy  7e m2 qf*     m2 q.		\ ( 7. 14. 21. 28. )
m1 m2 q= .				\ -1  (true)

m1 q1 q2 q*  m1 q.			\ ( -56. 16. 24. 26. )
m2 q2 q1 q*  m2 q.			\ ( -56. 18. 20. 28. )
m1 m2 q= .				\ 0  (false)
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
module Q_mod
  implicit none

  type quaternion
    real :: a, b, c, d
  end type

  public :: norm, neg, conj
  public :: operator (+)
  public :: operator (*)

  private ::  q_plus_q, q_plus_r, r_plus_q, &
              q_mult_q, q_mult_r, r_mult_q, &
              norm_q, neg_q, conj_q

  interface norm
    module procedure norm_q
  end interface

  interface neg
    module procedure neg_q
  end interface

  interface conj
    module procedure conj_q
  end interface

  interface operator (+)
    module procedure q_plus_q, q_plus_r, r_plus_q
  end interface

  interface operator (*)
    module procedure q_mult_q, q_mult_r, r_mult_q
  end interface

contains

function norm_q(x) result(res)
  real :: res
  type (quaternion), intent (in) :: x

  res = sqrt(x%a*x%a + x%b*x%b + x%c*x%c + x%d*x%d)

end function norm_q

function neg_q(x) result(res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x

  res%a = -x%a
  res%b = -x%b
  res%c = -x%c
  res%d = -x%d

end function neg_q

function conj_q(x) result(res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x

  res%a = x%a
  res%b = -x%b
  res%c = -x%c
  res%d = -x%d

end function conj_q

function q_plus_q(x, y) result (res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x, y

  res%a = x%a + y%a
  res%b = x%b + y%b
  res%c = x%c + y%c
  res%d = x%d + y%d

end function q_plus_q

function q_plus_r(x, r) result (res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x
  real, intent(in) :: r

   res = x
   res%a = x%a + r

end function q_plus_r

function r_plus_q(r, x) result (res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x
  real, intent(in) :: r

   res = x
   res%a = x%a + r

end function r_plus_q

function q_mult_q(x, y) result (res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x, y

   res%a = x%a*y%a - x%b*y%b - x%c*y%c - x%d*y%d
   res%b = x%a*y%b + x%b*y%a + x%c*y%d - x%d*y%c
   res%c = x%a*y%c - x%b*y%d + x%c*y%a + x%d*y%b
   res%d = x%a*y%d + x%b*y%c - x%c*y%b + x%d*y%a

end function q_mult_q

function q_mult_r(x, r) result (res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x
  real, intent(in) ::  r

   res%a = x%a*r
   res%b = x%b*r
   res%c = x%c*r
   res%d = x%d*r

end function q_mult_r

function r_mult_q(r, x) result (res)
  type (quaternion) :: res
  type (quaternion), intent (in) :: x
  real, intent(in) ::  r

   res%a = x%a*r
   res%b = x%b*r
   res%c = x%c*r
   res%d = x%d*r

end function r_mult_q
end module Q_mod

program Quaternions
  use Q_mod
  implicit none

  real :: r = 7.0
  type(quaternion) :: q, q1, q2

  q  = quaternion(1, 2, 3, 4)
  q1 = quaternion(2, 3, 4, 5)
  q2 = quaternion(3, 4, 5, 6)

  write(*, "(a, 4f8.3)") "             q = ", q
  write(*, "(a, 4f8.3)") "            q1 = ", q1
  write(*, "(a, 4f8.3)") "            q2 = ", q2
  write(*, "(a, f8.3)")  "             r = ", r
  write(*, "(a, f8.3)")  "     Norm of q = ", norm(q)
  write(*, "(a, 4f8.3)") " Negative of q = ", neg(q)
  write(*, "(a, 4f8.3)") "Conjugate of q = ", conj(q)
  write(*, "(a, 4f8.3)") "         q + r = ", q + r
  write(*, "(a, 4f8.3)") "         r + q = ", r + q
  write(*, "(a, 4f8.3)") "       q1 + q2 = ", q1 + q2
  write(*, "(a, 4f8.3)") "         q * r = ", q * r
  write(*, "(a, 4f8.3)") "         r * q = ", r * q
  write(*, "(a, 4f8.3)") "       q1 * q2 = ", q1 * q2
  write(*, "(a, 4f8.3)") "       q2 * q1 = ", q2 * q1

end program
```

{{out}}

```txt
             q =    1.000   2.000   3.000   4.000
            q1 =    2.000   3.000   4.000   5.000
            q2 =    3.000   4.000   5.000   6.000
             r =    7.000
     Norm of q =    5.477
 Negative of q =   -1.000  -2.000  -3.000  -4.000
Conjugate of q =    1.000  -2.000  -3.000  -4.000
         q + r =    8.000   2.000   3.000   4.000
         r + q =    8.000   2.000   3.000   4.000
       q1 + q2 =    5.000   7.000   9.000  11.000
         q * r =    7.000  14.000  21.000  28.000
         r * q =    7.000  14.000  21.000  28.000
       q1 * q2 =  -56.000  16.000  24.000  26.000
       q2 * q1 =  -56.000  18.000  20.000  28.000
```



## FreeBASIC


```freebasic

Dim Shared As Integer q(3)  = {1, 2, 3, 4}
Dim Shared As Integer q1(3) = {2, 3, 4, 5}
Dim Shared As Integer q2(3) = {3, 4, 5, 6}
Dim Shared As Integer i, r = 7, t(3)

Function q_norm(q() As Integer) As Double
    ' medida o valor absoluto de un cuaternión
    Dim As Double a = 0
    For i = 0 To 3
        a += q(i)^2
    Next i
    Return Sqr(a)
End Function

Sub q_neg(q() As Integer)
    For i = 0 To 3
        q(i) *= -1
    Next i
End Sub

Sub q_conj(q() As Integer)
    ' conjugado de un cuaternión
    For i = 1 To 3
        q(i) *= -1
    Next i
End Sub

Sub q_addreal(q() As Integer, r As Integer)
    q(0) += r
End Sub

Sub q_add(q() As Integer, r() As Integer)
    ' adición entre cuaternios
    For i = 0 To 3
        q(i) += r(i)
    Next i
End Sub

Sub q_mulreal(q() As Integer, r As Integer)
    For i = 0 To 3
        q(i) *= r
    Next i
End Sub

Sub q_mul(q() As Integer, r() As Integer)
    ' producto entre cuaternios
    Dim As Integer m(3)
    m(0) = q(0)*r(0) - q(1)*r(1) - q(2)*r(2) - q(3)*r(3)
    m(1) = q(0)*r(1) + q(1)*r(0) + q(2)*r(3) - q(3)*r(2)
    m(2) = q(0)*r(2) - q(1)*r(3) + q(2)*r(0) + q(3)*r(1)
    m(3) = q(0)*r(3) + q(1)*r(2) - q(2)*r(1) + q(3)*r(0)
    For i = 0 To 3 : q(i) = m(i) : Next i
End Sub

Function q_show(q() As Integer) As String
    Dim As String a = "("
    For i = 0 To 3
        a += Str(q(i)) + ", "
    Next i
    Return Mid(a,1,Len(a)-2) + ")"
End Function

'--- Programa Principal ---
Print " q = "; q_show(q())
Print "q1 = "; q_show(q1())
Print "q2 = "; q_show(q2())
Print " r = "; r
Print "norm(q) ="; q_norm(q())
For i = 0 To 3 : t(i) = q(i) : Next i : q_neg(t())  : Print " neg(q) = "; q_show(t())
For i = 0 To 3 : t(i) = q(i) : Next i : q_conj(t()) : Print "conj(q) = "; q_show(t())
For i = 0 To 3 : t(i) = q(i) : Next i : q_addreal(t(),r) : Print " r + q  = "; q_show(t())
For i = 0 To 3 : t(i) = q1(i) : Next i : q_add(t(),q2()) : Print "q1 + q2 = "; q_show(t())
For i = 0 To 3 : t(i) = q2(i) : Next i : q_add(t(),q1()) : Print "q2 + q1 = "; q_show(t())
For i = 0 To 3 : t(i) = q(i) : Next i : q_mulreal(t(),r) : Print " r * q  = "; q_show(t())
For i = 0 To 3 : t(i) = q1(i) : Next i : q_mul(t(),q2()) : Print "q1 * q2 = "; q_show(t())
For i = 0 To 3 : t(i) = q2(i) : Next i : q_mul(t(),q1()) : Print "q2 * q1 = "; q_show(t())
End

```

{{out}}

```txt

 q = (1, 2, 3, 4)
q1 = (2, 3, 4, 5)
q2 = (3, 4, 5, 6)
r = 7
norm(q) = 5.477225575051661
neg(q)  = (-1, -2, -3, -4)
conj(q) = (1, -2, -3, -4)
 r + q  = (8, 2, 3, 4)
q1 + q2 = (5, 7, 9, 11)
q2 + q1 = (5, 7, 9, 11)
 r * q  = (7, 14, 21, 28)
q1 * q2 = (-56, 16, 24, 26)
q2 * q1 = (-56, 18, 20, 28)

```



## GAP


```gap
# GAP has built-in support for quaternions

A := QuaternionAlgebra(Rationals);
# <algebra-with-one of dimension 4 over Rationals>

b := BasisVectors(Basis(A));
# [ e, i, j, k ]

q := [1, 2, 3, 4]*b;
# e+(2)*i+(3)*j+(4)*k

# Conjugate
ComplexConjugate(q);
# e+(-2)*i+(-3)*j+(-4)*k

# Division
1/q;
# (1/30)*e+(-1/15)*i+(-1/10)*j+(-2/15)*k

# Computing norm may be difficult, since the result would be in a quadratic field.
# Sqrt exists in GAP, but it is quite unusual: see ?E in GAP documentation, and the following example
Sqrt(5/3);
# 1/3*E(60)^7+1/3*E(60)^11-1/3*E(60)^19-1/3*E(60)^23-1/3*E(60)^31+1/3*E(60)^43-1/3*E(60)^47+1/3*E(60)^59

# However, the square of the norm is easy to compute
q*ComplexConjugate(q);
# (30)*e

q1 := [2, 3, 4, 5]*b;
# (2)*e+(3)*i+(4)*j+(5)*k

q2 := [3, 4, 5, 6]*b;
# (3)*e+(4)*i+(5)*j+(6)*k

q1*q2 - q2*q1;
# (-2)*i+(4)*j+(-2)*k

# Can't add directly to a rational, one must make a quaternion of it
r := 5/3*b[1];
# (5/3)*e
r + q;
# (8/3)*e+(2)*i+(3)*j+(4)*k

# For multiplication, no problem (we are in an algebra over rationals !)
r*q;
# (5/3)*e+(10/3)*i+(5)*j+(20/3)*k
5/3*q;
# (5/3)*e+(10/3)*i+(5)*j+(20/3)*k

# Negative
-q;
(-1)*e+(-2)*i+(-3)*j+(-4)*k


# While quaternions are built-in, you can define an algebra in GAP by specifying it's multiplication table.
# See tutorial, p. 60, and reference of the functions used below.

# A multiplication table of dimension 4.

T := EmptySCTable(4, 0);
SetEntrySCTable(T, 1, 1, [1, 1]);
SetEntrySCTable(T, 1, 2, [1, 2]);
SetEntrySCTable(T, 1, 3, [1, 3]);
SetEntrySCTable(T, 1, 4, [1, 4]);
SetEntrySCTable(T, 2, 1, [1, 2]);
SetEntrySCTable(T, 2, 2, [-1, 1]);
SetEntrySCTable(T, 2, 3, [1, 4]);
SetEntrySCTable(T, 2, 4, [-1, 3]);
SetEntrySCTable(T, 3, 1, [1, 3]);
SetEntrySCTable(T, 3, 2, [-1, 4]);
SetEntrySCTable(T, 3, 3, [-1, 1]);
SetEntrySCTable(T, 3, 4, [1, 2]);
SetEntrySCTable(T, 4, 1, [1, 4]);
SetEntrySCTable(T, 4, 2, [1, 3]);
SetEntrySCTable(T, 4, 3, [-1, 2]);
SetEntrySCTable(T, 4, 4, [-1, 1]);

A := AlgebraByStructureConstants(Rationals, T, ["e", "i", "j", "k"]);
b := GeneratorsOfAlgebra(A);

IsAssociative(A);
# true

IsCommutative(A);
# false

# Then, like above

q := [1, 2, 3, 4]*b;
# e+(2)*i+(3)*j+(4)*k

# However, as is, GAP does not know division or conjugate on this algebra.
# QuaternionAlgebra is useful as well for extensions of rationals,
# and this one _has_ conjugate and division, as seen previously.

# Try this on Q[z] where z is the square root of 5 (in GAP it's ER(5))
F := FieldByGenerators([ER(5)]);
A := QuaternionAlgebra(F);
b := GeneratorsOfAlgebra(A);

q := [1, 2, 3, 4]*b;
# e+(2)*i+(3)*j+(4)*k

# Conjugate and division

ComplexConjugate(q);
# e+(-2)*i+(-3)*j+(-4)*k

1/q;
# (1/30)*e+(-1/15)*i+(-1/10)*j+(-2/15)*k
```



## Go

Conventions for method receiver, parameter, and return values modeled after Go's big number package.
It provides flexibility without requiring unnecessary object creation.
The test program creates only four quaternion objects, the three inputs and one more for an output.
The three inputs are reused repeatedly without being modified.
The output is also reused repeatedly, being overwritten for each operation.

```go
package main

import (
    "fmt"
    "math"
)

type qtn struct {
    r, i, j, k float64
}

var (
    q  = &qtn{1, 2, 3, 4}
    q1 = &qtn{2, 3, 4, 5}
    q2 = &qtn{3, 4, 5, 6}

    r  float64 = 7
)

func main() {
    fmt.Println("Inputs")
    fmt.Println("q:", q)
    fmt.Println("q1:", q1)
    fmt.Println("q2:", q2)
    fmt.Println("r:", r)

    var qr qtn
    fmt.Println("\nFunctions")
    fmt.Println("q.norm():", q.norm())
    fmt.Println("neg(q):", qr.neg(q))
    fmt.Println("conj(q):", qr.conj(q))
    fmt.Println("addF(q, r):", qr.addF(q, r))
    fmt.Println("addQ(q1, q2):", qr.addQ(q1, q2))
    fmt.Println("mulF(q, r):", qr.mulF(q, r))
    fmt.Println("mulQ(q1, q2):", qr.mulQ(q1, q2))
    fmt.Println("mulQ(q2, q1):", qr.mulQ(q2, q1))
}

func (q *qtn) String() string {
    return fmt.Sprintf("(%g, %g, %g, %g)", q.r, q.i, q.j, q.k)
}

func (q *qtn) norm() float64 {
    return math.Sqrt(q.r*q.r + q.i*q.i + q.j*q.j + q.k*q.k)
}

func (z *qtn) neg(q *qtn) *qtn {
    z.r, z.i, z.j, z.k = -q.r, -q.i, -q.j, -q.k
    return z
}

func (z *qtn) conj(q *qtn) *qtn {
    z.r, z.i, z.j, z.k = q.r, -q.i, -q.j, -q.k
    return z
}

func (z *qtn) addF(q *qtn, r float64) *qtn {
    z.r, z.i, z.j, z.k = q.r+r, q.i, q.j, q.k
    return z
}

func (z *qtn) addQ(q1, q2 *qtn) *qtn {
    z.r, z.i, z.j, z.k = q1.r+q2.r, q1.i+q2.i, q1.j+q2.j, q1.k+q2.k
    return z
}

func (z *qtn) mulF(q *qtn, r float64) *qtn {
    z.r, z.i, z.j, z.k = q.r*r, q.i*r, q.j*r, q.k*r
    return z
}

func (z *qtn) mulQ(q1, q2 *qtn) *qtn {
    z.r, z.i, z.j, z.k =
        q1.r*q2.r-q1.i*q2.i-q1.j*q2.j-q1.k*q2.k,
        q1.r*q2.i+q1.i*q2.r+q1.j*q2.k-q1.k*q2.j,
        q1.r*q2.j-q1.i*q2.k+q1.j*q2.r+q1.k*q2.i,
        q1.r*q2.k+q1.i*q2.j-q1.j*q2.i+q1.k*q2.r
    return z
}
```

{{out}}

```txt

Inputs
q: (1, 2, 3, 4)
q1: (2, 3, 4, 5)
q2: (3, 4, 5, 6)
r: 7

Functions
q.norm(): 5.477225575051661
neg(q): (-1, -2, -3, -4)
conj(q): (1, -2, -3, -4)
addF(q, r): (8, 2, 3, 4)
addQ(q1, q2): (5, 7, 9, 11)
mulF(q, r): (7, 14, 21, 28)
mulQ(q1, q2): (-56, 16, 24, 26)
mulQ(q2, q1): (-56, 18, 20, 28)

```



## Haskell


```haskell
import Control.Monad (join)

data Quaternion a =
  Q a a a a
  deriving (Show, Eq)

realQ :: Quaternion a -> a
realQ (Q r _ _ _) = r

imagQ :: Quaternion a -> [a]
imagQ (Q _ i j k) = [i, j, k]

quaternionFromScalar :: (Num a) => a -> Quaternion a
quaternionFromScalar s = Q s 0 0 0

listFromQ :: Quaternion a -> [a]
listFromQ (Q a b c d) = [a, b, c, d]

quaternionFromList :: [a] -> Quaternion a
quaternionFromList [a, b, c, d] = Q a b c d

normQ :: (RealFloat a) => Quaternion a -> a
normQ = sqrt . sum . join (zipWith (*)) . listFromQ

conjQ :: (Num a) => Quaternion a -> Quaternion a
conjQ (Q a b c d) = Q a (-b) (-c) (-d)

instance (RealFloat a) => Num (Quaternion a) where
  (Q a b c d) + (Q p q r s) = Q (a + p) (b + q) (c + r) (d + s)
  (Q a b c d) - (Q p q r s) = Q (a - p) (b - q) (c - r) (d - s)
  (Q a b c d) * (Q p q r s) =
    Q
    (a * p - b * q - c * r - d * s)
    (a * q + b * p + c * s - d * r)
    (a * r - b * s + c * p + d * q)
    (a * s + b * r - c * q + d * p)
  negate (Q a b c d)        = Q (-a) (-b) (-c) (-d)
  abs q                     = quaternionFromScalar (normQ q)
  signum (Q 0 0 0 0)        = 0
  signum q@(Q a b c d)      = Q (a/n) (b/n) (c/n) (d/n) where n = normQ q
  fromInteger n             = quaternionFromScalar (fromInteger n)

main :: IO ()
main = do
  let q, q1, q2 :: Quaternion Double
      q  = Q 1 2 3 4
      q1 = Q 2 3 4 5
      q2 = Q 3 4 5 6
  print $ (Q 0 1 0 0) * (Q 0 0 1 0) * (Q 0 0 0 1) -- i*j*k; prints "Q (-1.0) 0.0 0.0 0.0"
  print $ q1 * q2                                 -- prints "Q (-56.0) 16.0 24.0 26.0"
  print $ q2 * q1                                 -- prints "Q (-56.0) 18.0 20.0 28.0"
  print $ q1 * q2 == q2 * q1                      -- prints "False"
  print $ imagQ q                                 -- prints "[2.0,3.0,4.0]"
```


==Icon and {{header|Unicon}}==

Using Unicon's class system.


```Unicon

class Quaternion(a, b, c, d)

  method norm ()
    return sqrt (a*a + b*b + c*c + d*d)
  end

  method negative ()
    return Quaternion(-a, -b, -c, -d)
  end

  method conjugate ()
    return Quaternion(a, -b, -c, -d)
  end

  method add (n)
    if type(n) == "Quaternion__state"
      then return Quaternion(a+n.a, b+n.b, c+n.c, d+n.d)
      else return Quaternion(a+n, b, c, d)
  end

  method multiply (n)
    if type(n) == "Quaternion__state"
      then return Quaternion(a*n.a - b*n.b - c*n.c - d*n.d,
                             a*n.b + b*n.a + c*n.d - d*n.c,
                             a*n.c - b*n.d + c*n.a + d*n.b,
                             a*n.d + b*n.c - c*n.b + d*n.a)
      else return Quaternion(a*n, b*n, c*n, d*n)
  end

  method sign (n)
    return if n >= 0 then "+" else "-"
  end

  method string ()
    return ("" || a || sign(b) || abs(b) || "i" || sign(c) || abs(c) || "j" || sign(d) || abs(d) || "k");
  end

  initially(a, b, c, d)
    self.a := if /a then 0 else a
    self.b := if /b then 0 else b
    self.c := if /c then 0 else c
    self.d := if /d then 0 else d
end

```


To test the above:


```Unicon

procedure main ()
  q := Quaternion (1,2,3,4)
  q1 := Quaternion (2,3,4,5)
  q2 := Quaternion (3,4,5,6)
  r := 7

  write ("The norm      of " || q.string() || " is " || q.norm ())
  write ("The negative  of " || q.string() || " is " || q.negative().string ())
  write ("The conjugate of " || q.string() || " is " || q.conjugate().string ())
  write ("Sum of " || q.string() || " and " || r || " is " || q.add(r).string ())
  write ("Sum of " || q.string() || " and " || q1.string() || " is " || q.add(q1).string ())
  write ("Product of " || q.string() || " and " || r || " is " || q.multiply(r).string ())
  write ("Product of " || q.string() || " and " || q1.string() || " is " || q.multiply(q1).string ())
  write ("q1*q2 = " || q1.multiply(q2).string ())
  write ("q2*q1 = " || q2.multiply(q1).string ())
end

```


{{out}}

```txt

The norm      of 1+2i+3j+4k is 5.477225575
The negative  of 1+2i+3j+4k is -1-2i-3j-4k
The conjugate of 1+2i+3j+4k is 1-2i-3j-4k
Sum of 1+2i+3j+4k and 7 is 8+2i+3j+4k
Sum of 1+2i+3j+4k and 2+3i+4j+5k is 3+5i+7j+9k
Product of 1+2i+3j+4k and 7 is 7+14i+21j+28k
Product of 1+2i+3j+4k and 2+3i+4j+5k is -36+6i+12j+12k
q1*q2 = -56+16i+24j+26k
q2*q1 = -56+18i+20j+28k

```



## J


Derived from the [[j:System/Requests/Quaternions|j wiki]]:


```j
   NB. utilities
   ip=:   +/ .*             NB. inner product
   T=. (_1^#:0 10 9 12)*0 7 16 23 A.=i.4
   toQ=:  4&{."1 :[:        NB. real scalars -> quaternion

   NB. task
   norm=: %:@ip~@toQ        NB. | y
   neg=:  -&toQ             NB. - y  and  x - y
   conj=: 1 _1 _1 _1 * toQ  NB. + y
   add=:  +&toQ             NB. x + y
   mul=:  (ip T ip ])&toQ   NB. x * y
```


T is a rank 3 tensor which allows us to express quaternion product ab as the inner product ATB if A and B are 4 element vectors representing the quaternions a and b. (Note also that once we have defined <code>mul</code> we no longer need to retain the definition of T, so we define T using =. instead of =:). The value of T is probably more interesting than its definition, so:


```J
   T
1  0  0  0
0  1  0  0
0  0  1  0
0  0  0  1

0 _1  0  0
1  0  0  0
0  0  0 _1
0  0  1  0

0  0 _1  0
0  0  0  1
1  0  0  0
0 _1  0  0

0  0  0 _1
0  0 _1  0
0  1  0  0
1  0  0  0
```


In other words, the last dimension of T corresponds to the structure of the right argument (columns, in the display of T), the first dimension of T corresponds to the structure of the left argument (tables, in the display of T) and the middle dimension of T corresponds to the structure of the result (rows, in the display of T).

Example use:

<lang>   q=: 1 2 3 4
   q1=: 2 3 4 5
   q2=: 3 4 5 6
   r=: 7

   norm q
5.47723
   neg q
_1 _2 _3 _4
   conj q
1 _2 _3 _4
   r add q
8 2 3 4
   q1 add q2
5 7 9 11
   r mul q
7 14 21 28
   q1 mul q2
_56 16 24 26
   q2 mul q1
_56 18 20 28
```


Finally, note that when quaternions are used to represent [[wp:Quaternions_and_spatial_rotation|orientation or rotation]], we are typically only interested in unit length quaternions. As this is the typical application for quaternions, you will sometimes see quaternion multiplication expressed using "simplifications" which are only valid for unit length quaternions. But note also that in many of those contexts you also need to normalize the quaternion length after multiplication.

(An exception to this need to normalize unit length quaternions after multiplication might be when quaternions are represented as an index into a [[wp:Geodesic_grid|geodesic grid]]. For example, a grid with 16x20 faces would have a total of 15 vertices for each face (5+4+3+2+1), 3 of those vertices would be from the original 20 vertices of the icosahedron, and 9 of those vertices (5+4+3-3) would be on the edge of the original face (and, thus, used for two faces), the remaining 3 vertices would be interior. This means we would have 170 vertices (20+(20*9%2)+20*3, which would allow a quaternion to be represented in a single byte index into a list of 170 quaternions, and would allow quaternion multiplication to be represented as a 29kbyte lookup table. In some contexts - where quaternion multiplication is needed in high volume for secondary or tertiary issues (where precision isn't vital), such low accuracy quaternions might be adequate or even an advantage...)


## Java


```java
public class Quaternion {
    private final double a, b, c, d;

    public Quaternion(double a, double b, double c, double d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }
    public Quaternion(double r) {
        this(r, 0.0, 0.0, 0.0);
    }

    public double norm() {
        return Math.sqrt(a * a + b * b + c * c + d * d);
    }

    public Quaternion negative() {
        return new Quaternion(-a, -b, -c, -d);
    }

    public Quaternion conjugate() {
        return new Quaternion(a, -b, -c, -d);
    }

    public Quaternion add(double r) {
        return new Quaternion(a + r, b, c, d);
    }
    public static Quaternion add(Quaternion q, double r) {
        return q.add(r);
    }
    public static Quaternion add(double r, Quaternion q) {
        return q.add(r);
    }
    public Quaternion add(Quaternion q) {
        return new Quaternion(a + q.a, b + q.b, c + q.c, d + q.d);
    }
    public static Quaternion add(Quaternion q1, Quaternion q2) {
        return q1.add(q2);
    }

    public Quaternion times(double r) {
        return new Quaternion(a * r, b * r, c * r, d * r);
    }
    public static Quaternion times(Quaternion q, double r) {
        return q.times(r);
    }
    public static Quaternion times(double r, Quaternion q) {
        return q.times(r);
    }
    public Quaternion times(Quaternion q) {
        return new Quaternion(
            a * q.a - b * q.b - c * q.c - d * q.d,
            a * q.b + b * q.a + c * q.d - d * q.c,
            a * q.c - b * q.d + c * q.a + d * q.b,
            a * q.d + b * q.c - c * q.b + d * q.a
        );
    }
    public static Quaternion times(Quaternion q1, Quaternion q2) {
        return q1.times(q2);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Quaternion)) return false;
        final Quaternion other = (Quaternion) obj;
        if (Double.doubleToLongBits(this.a) != Double.doubleToLongBits(other.a)) return false;
        if (Double.doubleToLongBits(this.b) != Double.doubleToLongBits(other.b)) return false;
        if (Double.doubleToLongBits(this.c) != Double.doubleToLongBits(other.c)) return false;
        if (Double.doubleToLongBits(this.d) != Double.doubleToLongBits(other.d)) return false;
        return true;
    }
    @Override
    public String toString() {
        return String.format("%.2f + %.2fi + %.2fj + %.2fk", a, b, c, d).replaceAll("\\+ -", "- ");
    }

    public String toQuadruple() {
        return String.format("(%.2f, %.2f, %.2f, %.2f)", a, b, c, d);
    }

    public static void main(String[] args) {
        Quaternion q = new Quaternion(1.0, 2.0, 3.0, 4.0);
        Quaternion q1 = new Quaternion(2.0, 3.0, 4.0, 5.0);
        Quaternion q2 = new Quaternion(3.0, 4.0, 5.0, 6.0);
        double r = 7.0;
        System.out.format("q       = %s%n", q);
        System.out.format("q1      = %s%n", q1);
        System.out.format("q2      = %s%n", q2);
        System.out.format("r       = %.2f%n%n", r);
        System.out.format("\u2016q\u2016     = %.2f%n", q.norm());
        System.out.format("-q      = %s%n", q.negative());
        System.out.format("q*      = %s%n", q.conjugate());
        System.out.format("q + r   = %s%n", q.add(r));
        System.out.format("q1 + q2 = %s%n", q1.add(q2));
        System.out.format("q \u00d7 r   = %s%n", q.times(r));
        Quaternion q1q2 = q1.times(q2);
        Quaternion q2q1 = q2.times(q1);
        System.out.format("q1 \u00d7 q2 = %s%n", q1q2);
        System.out.format("q2 \u00d7 q1 = %s%n", q2q1);
        System.out.format("q1 \u00d7 q2 %s q2 \u00d7 q1%n", (q1q2.equals(q2q1) ? "=" : "\u2260"));
    }
}
```


{{out}}

```txt
q       = 1.00 + 2.00i + 3.00j + 4.00k
q1      = 2.00 + 3.00i + 4.00j + 5.00k
q2      = 3.00 + 4.00i + 5.00j + 6.00k
r       = 7.00

‖q‖     = 5.48
-q      = -1.00 - 2.00i - 3.00j - 4.00k
q*      = 1.00 - 2.00i - 3.00j - 4.00k
q + r   = 8.00 + 2.00i + 3.00j + 4.00k
q1 + q2 = 5.00 + 7.00i + 9.00j + 11.00k
q × r   = 7.00 + 14.00i + 21.00j + 28.00k
q1 × q2 = -56.00 + 16.00i + 24.00j + 26.00k
q2 × q1 = -56.00 + 18.00i + 20.00j + 28.00k
q1 × q2 ≠ q2 × q1
```



## JavaScript

Runs on Firefox 3+, limited support in other JS engines.  More compatible JavaScript deserves its own entry.


```javascript
var Quaternion = (function() {
    // The Q() function takes an array argument and changes it
    // prototype so that it becomes a Quaternion instance.  This is
    // scoped only for prototype member access.
    function Q(a) {
	a.__proto__ = proto;
	return a;
    }

    // Actual constructor.  This constructor converts its arguments to
    // an array, then that array to a Quaternion instance, then
    // returns that instance.  (using "new" with this constructor is
    // optional)
    function Quaternion() {
	return Q(Array.prototype.slice.call(arguments, 0, 4));
    }

    // Prototype for all Quaternions
    const proto = {
	// Inherits from a 4-element Array
	__proto__ : [0,0,0,0],

	// Properties -- In addition to Array[0..3] access, we
	// also define matching a, b, c, and d properties
	get a() this[0],
	get b() this[1],
	get c() this[2],
	get d() this[3],

	// Methods
	norm : function() Math.sqrt(this.map(function(x) x*x).reduce(function(x,y) x+y)),
	negate : function() Q(this.map(function(x) -x)),
	conjugate : function() Q([ this[0] ].concat(this.slice(1).map(function(x) -x))),
	add : function(x) {
	    if ("number" === typeof x) {
		return Q([ this[0] + x ].concat(this.slice(1)));
	    } else {
		return Q(this.map(function(v,i) v+x[i]));
	    }
	},
	mul : function(r) {
	    var q = this;
	    if ("number" === typeof r) {
		return Q(q.map(function(e) e*r));
	    } else {
		return Q([ q[0] * r[0] - q[1] * r[1] - q[2] * r[2] - q[3] * r[3],
			   q[0] * r[1] + q[1] * r[0] + q[2] * r[3] - q[3] * r[2],
			   q[0] * r[2] - q[1] * r[3] + q[2] * r[0] + q[3] * r[1],
			   q[0] * r[3] + q[1] * r[2] - q[2] * r[1] + q[3] * r[0] ]);
	    }
	},
	equals : function(q) this.every(function(v,i) v === q[i]),
	toString : function() (this[0] + " + " + this[1] + "i + "+this[2] + "j + " + this[3] + "k").replace(/\+ -/g, '- ')
    };

    Quaternion.prototype = proto;
    return Quaternion;
})();
```


Task/Example Usage:


```javascript
var q = Quaternion(1,2,3,4);
var q1 = Quaternion(2,3,4,5);
var q2 = Quaternion(3,4,5,6);
var r = 7;

console.log("q = "+q);
console.log("q1 = "+q1);
console.log("q2 = "+q2);
console.log("r = "+r);
console.log("1. q.norm() = "+q.norm());
console.log("2. q.negate() = "+q.negate());
console.log("3. q.conjugate() = "+q.conjugate());
console.log("4. q.add(r) = "+q.add(r));
console.log("5. q1.add(q2) = "+q1.add(q2));
console.log("6. q.mul(r) = "+q.mul(r));
console.log("7.a. q1.mul(q2) = "+q1.mul(q2));
console.log("7.b. q2.mul(q1) = "+q2.mul(q1));
console.log("8. q1.mul(q2) " + (q1.mul(q2).equals(q2.mul(q1)) ? "==" : "!=") + " q2.mul(q1)");
```


{{out}}

```txt
q = 1 + 2i + 3j + 4k
q1 = 2 + 3i + 4j + 5k
q2 = 3 + 4i + 5j + 6k
r = 7
1. q.norm() = 5.477225575051661
2. q.negate() = -1 - 2i - 3j - 4k
3. q.conjugate() = 1 - 2i - 3j - 4k
4. q.add(r) = 8 + 2i + 3j + 4k
5. q1.add(q2) = 5 + 7i + 9j + 11k
6. q.mul(r) = 7 + 14i + 21j + 28k
7.a. q1.mul(q2) = -56 + 16i + 24j + 26k
7.b. q2.mul(q1) = -56 + 18i + 20j + 28k
8. q1.mul(q2) != q2.mul(q1)
```



## jq


Program file: quaternion.jq
```jq
def Quaternion(q0;q1;q2;q3): { "q0": q0, "q1": q1, "q2": q2, "q3": q3, "type": "Quaternion" };

# promotion of a real number to a quaternion
def Quaternion(r): if (r|type) == "number" then Quaternion(r;0;0;0) else r end;

# thoroughly recursive pretty-print
def pp:

  def signage: if . >= 0 then "+ \(.)" else  "- \(-.)" end;

  if type == "object" then
     if .type == "Quaternion" then
       "\(.q0) \(.q1|signage)i \(.q2|signage)j \(.q3|signage)k"
     else with_entries( {key, "value" : (.value|pp)} )
     end
  elif type == "array" then map(pp)
  else .
  end ;

def real(z): Quaternion(z).q0;

# Note: imag(z) returns the "i" component only,
# reflecting the embedding of the complex numbers within the quaternions:
def imag(z): Quaternion(z).q1;

def conj(z): Quaternion(z) | Quaternion(.q0; -(.q1); -(.q2); -(.q3));

def abs2(z): Quaternion(z) | .q0 * .q0 + .q1*.q1 + .q2*.q2 + .q3*.q3;

def abs(z): abs2(z) | sqrt;

def negate(z): Quaternion(z) | Quaternion(-.q0; -.q1; -.q2; -.q3);

# z + w
def plus(z; w):
  def plusq(z;w): Quaternion(z.q0 + w.q0; z.q1 + w.q1;
                             z.q2 + w.q2; z.q3 + w.q3);
  plusq( Quaternion(z); Quaternion(w) );

# z - w
def minus(z; w):
  def minusq(z;w): Quaternion(z.q0 - w.q0; z.q1 - w.q1;
                              z.q2 - w.q2; z.q3 - w.q3);
  minusq( Quaternion(z); Quaternion(w) );

# *
def times(z; w):
  def timesq(z; w):
       Quaternion(z.q0*w.q0 - z.q1*w.q1 - z.q2*w.q2 - z.q3*w.q3;
                  z.q0*w.q1 + z.q1*w.q0 + z.q2*w.q3 - z.q3*w.q2;
                  z.q0*w.q2 - z.q1*w.q3 + z.q2*w.q0 + z.q3*w.q1;
                  z.q0*w.q3 + z.q1*w.q2 - z.q2*w.q1 + z.q3*w.q0);
  timesq( Quaternion(z); Quaternion(w) );

# (z/w)
def div(z; w):
  if (w|type) == "number" then Quaternion(z.q0/w; z.q1/w; z.q2/w; z.q3/w)
  else times(z; inv(w))
  end;

def inv(z): div(conj(z); abs2(z));


# Example usage and output:

def say(msg; e): "\(msg) => \(e|pp)";

def demo:
  say( "Quaternion(1;0;0;0)"; Quaternion(1;0;0;0)),
  (Quaternion (1; 2; 3; 4) as $q
  | Quaternion(2; 3; 4; 5) as $q1
  | Quaternion(3; 4; 5; 6) as $q2
  | 7 as $r
  | say( "abs($q)";        abs($q) ),   # norm
    say( "negate($q)";     negate($q) ),
    say( "conj($q)";       conj($q) ),
    "",
    say( "plus($r; $q)";   plus($r; $q)),
    say( "plus($q; $r)";   plus($q; $r)),
    "",
    say( "plus($q1; $q2 )"; plus($q1; $q2)),
    "",
    say( "times($r;$q)";    times($r;$q)),
    say( "times($q;$r)";    times($q;$r)),
    "",
    say( "times($q1;$q2)";  times($q1;$q2)),
    say( "times($q2; $q1)"; times($q2; $q1)),
    say( "times($q1; $q2) != times($q2; $q1)";
         times($q1; $q2) != times($q2; $q1) )
    ) ;

demo
```

Example usage and output:

```sh
# jq -c -n -R -f quaternion.jq
Quaternion(1;0;0;0) => 1 + 0i + 0j + 0k
abs($q) => 5.477225575051661
negate($q) => -1 - 2i - 3j + -4k
conj($q) => 1 - 2i - 3j - 4k

plus($r; $q) => 8 + 2i + 3j + 4k
plus($q; $r) => 8 + 2i + 3j + 4k

plus($q1; $q2 ) => 5 + 7i + 9j + 11k

times($r;$q) => 7 + 14i + 21j + 28k
times($q;$r) => 7 + 14i + 21j + 28k

times($q1;$q2) => -56 + 16i + 24j + 26k
times($q2; $q1) => -56 + 18i + 20j + 28k
times($q1; $q2) != times($q2; $q1) => true
```



## Julia

https://github.com/andrioni/Quaternions.jl/blob/master/src/Quaternions.jl has a more complete implementation.
This is derived from the [https://github.com/JuliaLang/julia/blob/release-0.2/examples/quaternion.jl quaternion example file] included with Julia 0.2, which implements a quaternion type complete with arithmetic, type conversions / promotion rules, polymorphism over arbitrary real numeric types, and pretty-printing.

```julia
import Base: convert, promote_rule, show, conj, abs, +, -, *

immutable Quaternion{T<:Real} <: Number
    q0::T
    q1::T
    q2::T
    q3::T
end

Quaternion(q0::Real,q1::Real,q2::Real,q3::Real) = Quaternion(promote(q0,q1,q2,q3)...)

convert{T}(::Type{Quaternion{T}}, x::Real) =
    Quaternion(convert(T,x), zero(T), zero(T), zero(T))
convert{T}(::Type{Quaternion{T}}, z::Complex) =
    Quaternion(convert(T,real(z)), convert(T,imag(z)), zero(T), zero(T))
convert{T}(::Type{Quaternion{T}}, z::Quaternion) =
    Quaternion(convert(T,z.q0), convert(T,z.q1), convert(T,z.q2), convert(T,z.q3))

promote_rule{T,S}(::Type{Complex{T}}, ::Type{Quaternion{S}}) = Quaternion{promote_type(T,S)}
promote_rule{T<:Real,S}(::Type{T}, ::Type{Quaternion{S}}) = Quaternion{promote_type(T,S)}
promote_rule{T,S}(::Type{Quaternion{T}}, ::Type{Quaternion{S}}) = Quaternion{promote_type(T,S)}

function show(io::IO, z::Quaternion)
    pm(x) = x <	0 ? " - $(-x)" : " + $x"
    print(io, z.q0, pm(z.q1), "i", pm(z.q2), "j", pm(z.q3), "k")
end

conj(z::Quaternion) = Quaternion(z.q0, -z.q1, -z.q2, -z.q3)
abs(z::Quaternion) = sqrt(z.q0*z.q0 + z.q1*z.q1 + z.q2*z.q2 + z.q3*z.q3)

(-)(z::Quaternion) = Quaternion(-z.q0, -z.q1, -z.q2, -z.q3)

(+)(z::Quaternion, w::Quaternion) = Quaternion(z.q0 + w.q0, z.q1 + w.q1,
                                               z.q2 + w.q2, z.q3 + w.q3)
(-)(z::Quaternion, w::Quaternion) = Quaternion(z.q0 - w.q0, z.q1 - w.q1,
                                               z.q2 - w.q2, z.q3 - w.q3)
(*)(z::Quaternion, w::Quaternion) = Quaternion(z.q0*w.q0 - z.q1*w.q1 - z.q2*w.q2 - z.q3*w.q3,
                                               z.q0*w.q1 + z.q1*w.q0 + z.q2*w.q3 - z.q3*w.q2,
                                               z.q0*w.q2 - z.q1*w.q3 + z.q2*w.q0 + z.q3*w.q1,
                                               z.q0*w.q3 + z.q1*w.q2 - z.q2*w.q1 + z.q3*w.q0)

```


Example usage and output:

```julia>julia
 q = Quaternion(1,0,0,0)
julia> q  = Quaternion (1, 2, 3, 4)
       q1 = Quaternion(2, 3, 4, 5)
       q2 = Quaternion(3, 4, 5, 6)
       r = 7.

julia> norm(q)
5.477225575051661

julia> -q
-1 - 2i - 3j - 4k

julia> conj(q)
1 - 2i - 3j - 4k

julia> r + q, q + r
(8.0 + 2.0i + 3.0j + 4.0k,8.0 + 2.0i + 3.0j + 4.0k)

julia> q1 + q2
5 + 7i + 9j + 11k

julia> r*q, q*r
(7.0 + 14.0i + 21.0j + 28.0k,7.0 + 14.0i + 21.0j + 28.0k)

julia> q1*q2, q2*q1, q1*q2 != q2*q1
(-56 + 16i + 24j + 26k,-56 + 18i + 20j + 28k,true)
```



## Kotlin


```scala
// version 1.1.2

data class Quaternion(val a: Double, val b: Double, val c: Double, val d: Double) {
    operator fun plus(other: Quaternion): Quaternion {
        return Quaternion (this.a + other.a, this.b + other.b,
                           this.c + other.c, this.d + other.d)
    }

    operator fun plus(r: Double) = Quaternion(a + r, b, c, d)

    operator fun times(other: Quaternion): Quaternion {
        return Quaternion(
            this.a * other.a - this.b * other.b - this.c * other.c - this.d * other.d,
            this.a * other.b + this.b * other.a + this.c * other.d - this.d * other.c,
            this.a * other.c - this.b * other.d + this.c * other.a + this.d * other.b,
            this.a * other.d + this.b * other.c - this.c * other.b + this.d * other.a
        )
    }

    operator fun times(r: Double) = Quaternion(a * r, b * r, c * r, d * r)

    operator fun unaryMinus() =  Quaternion(-a, -b, -c, -d)

    fun conj() = Quaternion(a, -b, -c, -d)

    fun norm() = Math.sqrt(a * a + b * b + c * c + d * d)

    override fun toString() = "($a, $b, $c, $d)"
}

// extension functions for Double type
operator fun Double.plus(q: Quaternion) = q + this
operator fun Double.times(q: Quaternion) = q * this

fun main(args: Array<String>) {
    val q  = Quaternion(1.0, 2.0, 3.0, 4.0)
    val q1 = Quaternion(2.0, 3.0, 4.0, 5.0)
    val q2 = Quaternion(3.0, 4.0, 5.0, 6.0)
    val r  = 7.0
    println("q  = $q")
    println("q1 = $q1")
    println("q2 = $q2")
    println("r  = $r\n")
    println("norm(q) = ${"%f".format(q.norm())}")
    println("-q      = ${-q}")
    println("conj(q) = ${q.conj()}\n")
    println("r  + q  = ${r + q}")
    println("q  + r  = ${q + r}")
    println("q1 + q2 = ${q1 + q2}\n")
    println("r  * q  = ${r * q}")
    println("q  * r  = ${q * r}")
    val q3 = q1 * q2
    val q4 = q2 * q1
    println("q1 * q2 = $q3")
    println("q2 * q1 = $q4\n")
    println("q1 * q2 != q2 * q1 = ${q3 != q4}")
}
```


{{out}}

```txt

q  = (1.0, 2.0, 3.0, 4.0)
q1 = (2.0, 3.0, 4.0, 5.0)
q2 = (3.0, 4.0, 5.0, 6.0)
r  = 7.0

norm(q) = 5.477226
-q      = (-1.0, -2.0, -3.0, -4.0)
conj(q) = (1.0, -2.0, -3.0, -4.0)

r  + q  = (8.0, 2.0, 3.0, 4.0)
q  + r  = (8.0, 2.0, 3.0, 4.0)
q1 + q2 = (5.0, 7.0, 9.0, 11.0)

r  * q  = (7.0, 14.0, 21.0, 28.0)
q  * r  = (7.0, 14.0, 21.0, 28.0)
q1 * q2 = (-56.0, 16.0, 24.0, 26.0)
q2 * q1 = (-56.0, 18.0, 20.0, 28.0)

q1 * q2 != q2 * q1 = true

```



## Liberty BASIC

Quaternions saved as a space-separated string of four numbers.

```lb


 q$ = q$( 1 , 2 , 3 , 4 )
q1$ = q$( 2 , 3 , 4 , 5 )
q2$ = q$( 3 , 4 , 5 , 6 )

real = 7

print "q = "  ;  q$
print "q1 = " ; q1$
print "q2 = " ; q2$

print "real = " ; real

print "length /norm q  = " ; length( q$ )               '   =norm                        norm of q
print "negative (-q1)  = " ; negative$( q1$ )           '   =negative                    negated q1
print "conjugate q     = " ; conjugate$( q$ )           '   conjugate                    conjugate q
print "real + q        = " ; add1$( q$ , real )         '   real +quaternion             real +q
print "q + q2          = " ; add2$( q$ , q2$ )          '   sum two quaternions          q +q2
print "real * q        = " ; multiply1$( q$ , real )    '   real *quaternion             real *q
print "q1 * q2         = " ; multiply2$( q1$ , q2$ )    '   product of two quaternions   q1 & q2
print "q2 * q1         = " ; multiply2$( q2$ , q1$ )    '   show q1 *q2 <> q2 *q1

end

function q$( r , i , j , k )
  q$ = str$( r); " "; str$( i); " "; str$( j); " "; str$( k)
end function

function length( q$ )
  r = val( word$( q$ , 1 ) )
  i = val( word$( q$ , 2 ) )
  j = val( word$( q$ , 3 ) )
  k = val( word$( q$ , 4 ) )
  length =sqr( r^2 +i^2 +j^2 +k^2)
end function

function multiply1$( q$ , d )
  r = val( word$( q$ , 1 ) )
  i = val( word$( q$ , 2 ) )
  j = val( word$( q$ , 3 ) )
  k = val( word$( q$ , 4 ) )
  multiply1$ =q$( r*d, i*d, j*d, k*d)
end function

function multiply2$( q$ , b$ )
  ar = val( word$( q$ , 1 ) )   'a1
  ai = val( word$( q$ , 2 ) )   'b1
  aj = val( word$( q$ , 3 ) )   'c1
  ak = val( word$( q$ , 4 ) )   'd1

  br = val( word$( b$ , 1 ) )   'a2
  bi = val( word$( b$ , 2 ) )   'b2
  bj = val( word$( b$ , 3 ) )   'c2
  bk = val( word$( b$ , 4 ) )   'd2

  multiply2$ =q$( _
  ar *br_
  +( 0 -ai) *bi_
  +( 0 -aj) *bj_
  +( 0 -ak) *bk _
  ,_
  ar *bi_
  +ai *br_
  +aj *bk_
  +( 0 -ak) *bj_
  ,_
  ar *bj_
  +( 0 -ai) *bk_
  +aj *br_
  +ak *bi_
  ,_
  ar *bk_
  +ai *bj_
  +( 0 -aj) *bi_
  +ak *br )
end function

function negative$( q$ )
  r = val( word$( q$ , 1 ) )
  i = val( word$( q$ , 2 ) )
  j = val( word$( q$ , 3 ) )
  k = val( word$( q$ , 4 ) )
  negative$ =q$( 0-r, 0-i, 0-j, 0-k)
end function

function conjugate$( q$ )
  r = val( word$( q$ , 1 ) )
  i = val( word$( q$ , 2 ) )
  j = val( word$( q$ , 3 ) )
  k = val( word$( q$ , 4 ) )
  conjugate$ =q$( r, 0-i, 0-j, 0-k)
end function

function add1$( q$ , real )
  r = val( word$( q$ , 1 ) )
  i = val( word$( q$ , 2 ) )
  j = val( word$( q$ , 3 ) )
  k = val( word$( q$ , 4 ) )
  add1$ =q$( r +real, i, j, k)
end function

function add2$( q$ , b$ )
  ar = val( word$( q$ , 1 ) )
  ai = val( word$( q$ , 2 ) )
  aj = val( word$( q$ , 3 ) )
  ak = val( word$( q$ , 4 ) )
  br = val( word$( b$ , 1 ) )
  bi = val( word$( b$ , 2 ) )
  bj = val( word$( b$ , 3 ) )
  bk = val( word$( b$ , 4 ) )
  add2$ =q$( ar +br, ai +bi, aj +bj, ak +bk)
end function

```



## Lua


```lua
Quaternion = {}

function Quaternion.new( a, b, c, d )
    local q = { a = a or 1, b = b or 0, c = c or 0, d = d or 0 }

    local metatab = {}
    setmetatable( q, metatab )
    metatab.__add = Quaternion.add
    metatab.__sub = Quaternion.sub
    metatab.__unm = Quaternion.unm
    metatab.__mul = Quaternion.mul

    return q
end

function Quaternion.add( p, q )
    if type( p ) == "number" then
	return Quaternion.new( p+q.a, q.b, q.c, q.d )
    elseif type( q ) == "number" then
	return Quaternion.new( p.a+q, p.b, p.c, p.d )
    else
	return Quaternion.new( p.a+q.a, p.b+q.b, p.c+q.c, p.d+q.d )
    end
end

function Quaternion.sub( p, q )
    if type( p ) == "number" then
	return Quaternion.new( p-q.a, q.b, q.c, q.d )
    elseif type( q ) == "number" then
	return Quaternion.new( p.a-q, p.b, p.c, p.d )
    else
	return Quaternion.new( p.a-q.a, p.b-q.b, p.c-q.c, p.d-q.d )
    end
end

function Quaternion.unm( p )
    return Quaternion.new( -p.a, -p.b, -p.c, -p.d )
end

function Quaternion.mul( p, q )
    if type( p ) == "number" then
	return Quaternion.new( p*q.a, p*q.b, p*q.c, p*q.d )
    elseif type( q ) == "number" then
	return Quaternion.new( p.a*q, p.b*q, p.c*q, p.d*q )
    else
	return Quaternion.new( p.a*q.a - p.b*q.b - p.c*q.c - p.d*q.d,
                               p.a*q.b + p.b*q.a + p.c*q.d - p.d*q.c,
 			       p.a*q.c - p.b*q.d + p.c*q.a + p.d*q.b,
			       p.a*q.d + p.b*q.c - p.c*q.b + p.d*q.a )
    end
end

function Quaternion.conj( p )
    return Quaternion.new( p.a, -p.b, -p.c, -p.d )
end

function Quaternion.norm( p )
    return math.sqrt( p.a^2 + p.b^2 + p.c^2 + p.d^2 )
end

function Quaternion.print( p )
    print( string.format( "%f + %fi + %fj + %fk\n", p.a, p.b, p.c, p.d ) )
end
```

Examples:

```lua
q1 = Quaternion.new( 1, 2, 3, 4 )
q2 = Quaternion.new( 5, 6, 7, 8 )
r  = 12

print( "norm(q1) = ", Quaternion.norm( q1 ) )
io.write( "-q1 = " ); Quaternion.print( -q1 )
io.write( "conj(q1) = " ); Quaternion.print( Quaternion.conj( q1 ) )
io.write( "r+q1 = " ); Quaternion.print( r+q1 )
io.write( "q1+r = " ); Quaternion.print( q1+r )
io.write( "r*q1 = " ); Quaternion.print( r*q1 )
io.write( "q1*r = " ); Quaternion.print( q1*r )
io.write( "q1*q2 = " ); Quaternion.print( q1*q2 )
io.write( "q2*q1 = " ); Quaternion.print( q2*q1 )
```


{{out}}

```txt
norm(q1) = 5.4772255750517
-q1 = -1.000000 -2.000000i -3.000000j -4.000000k
conj(q1) = 1.000000 -2.000000i -3.000000j -4.000000k
r+q1 = 13.000000 + 2.000000i + 3.000000j + 4.000000k
q1+r = 13.000000 + 2.000000i + 3.000000j + 4.000000k
r*q1 = 12.000000 + 24.000000i + 36.000000j + 48.000000k
q1*r = 12.000000 + 24.000000i + 36.000000j + 48.000000k
q1*q2 = -60.000000 + 12.000000i + 30.000000j + 24.000000k
q2*q1 = -60.000000 + 20.000000i + 14.000000j + 32.000000k
```



## M2000 Interpreter

We can define Quaternions using a class, using operators for specific tasks, as negate, add, multiplication and equality with rounding to 13 decimal place (thats what doing "==" operator for doubles)

```M2000 Interpreter

Module CheckIt {
      class Quaternion {
            \\ by default are double
            a,b,c,d
            Property ToString$ {
                  Value {
                      link parent a,b,c, d to a,b,c,d
                       value$=format$("{0} + {1}i + {2}j + {3}k",a,b,c,d)
                  }
            }
            Property Norm { Value}
            Operator "==" {
                  read n
                  push .a==n.a and .b==n.b and .c==n.c and .d==n.d
            }
            Module CalcNorm {
                  .[Norm]<=sqrt(.a**2+.b**2+.c**2+.d**2)
             }
            Operator Unary {
                  .a-! : .b-! : .c-! :.d-!
            }
            Function Conj {
                  q=this
                  for q {
                         .b-! : .c-! :.d-!
                  }
                  =q
            }
            Function Add {
                  q=this
                  for q {
                         .a+=Number : .CalcNorm
                  }
                  =q
            }
            Operator "+"  {
                  Read q2
                  For this, q2 {
                        .a+=..a :.b+=..b:.c+=..c:.d+=..d
                        .CalcNorm
                  }
            }
            Function Mul(r)  {
                  q=this
                  for q {
                        .a*=r:.b*=r:.c*=r:.d*=r:.CalcNorm
                  }
                  =q
            }
            Operator "*" {
                  Read q2
                  For This, q2 {
                        Push .a*..a-.b*..b-.c*..c-.d*..d
                        Push .a*..b+.b*..a+.c*..d-.d*..c
                        Push .a*..c-.b*..d+.c*..a+.d*..b
                        .d<=.a*..d+.b*..c-.c*..b+.d*..a
                        Read .c, .b, .a
                        .CalcNorm
                  }
            }
            class:
            module Quaternion {
                  if match("NNNN") then {
                        Read .a,.b,.c,.d
                       .CalcNorm
                  }
            }
      }
      \\ variables
      r=7
      q=Quaternion(1,2,3,4)
      q1=Quaternion(2,3,4,5)
      q2=Quaternion(3,4,5,6)

      \\ perform negate, conjugate, multiply by real, add a real, multiply quanterions, multiply in reverse order
      qneg=-q
      qconj=q.conj()
      qmul=q.Mul(r)
      qadd=q.Add(r)
      q1q2=q1*q2
      q2q1=q2*q1

      Print "q = ";q.ToString$
      Print "Normal q = ";q.Norm
      Print "Neg q = ";qneg.ToString$
      Print "Conj q = ";qconj.ToString$
      Print "Mul q 7 = ";qmul.ToString$
      Print "Add q 7 = ";qadd.ToString$
      Print "q1 = ";q1.ToString$
      Print "q2 = ";q2.ToString$
      Print "q1 * q2 = ";q1q2.ToString$
      Print "q2 * q1 = ";q2q1.ToString$
      Print q1==q1   ' true
      Print q1q2==q2q1 ' false
      \\ multiplication and equality in one expression
      Print (q1 * q2 == q2 * q1)=false
      Print (q1 * q2 == q1 * q2)=True
}
CheckIt

```

{{out}}

```txt

q = 1 + 2i + 3j + 4k
Normal q = 5.47722557505166
Neg q = -1 + -2i + -3j + -4k
Conj q = 1 + -2i + -3j + -4k
Mul q 7 = 7 + 14i + 21j + 28k
Add q 7 = 8 + 2i + 3j + 4k
q1 = 2 + 3i + 4j + 5k
q2 = 3 + 4i + 5j + 6k
q1 * q2 = -56 + 16i + 24j + 26k
q2 * q1 = -56 + 18i + 20j + 28k
     True
    False
     True
     True
```



## Mathematica


```Mathematica
<<Quaternions`
q=Quaternion[1,2,3,4]
q1=Quaternion[2,3,4,5]
q2=Quaternion[3,4,5,6]
r=7
->Quaternion[1,2,3,4]
->Quaternion[2,3,4,5]
->Quaternion[3,4,5,6]
->7

Abs[q]
->√30
-q
->Quaternion[-1,-2,-3,-4]
Conjugate[q]
->Quaternion[1,-2,-3,-4]
r+q
->Quaternion[8,2,3,4]
q+r
->Quaternion[8,2,3,4]
q1+q2
->Quaternion[5,7,9,11]
q*r
->Quaternion[7,14,21,28]
r*q
->Quaternion[7,14,21,28]
q1**q2
->Quaternion[-56,16,24,26]
q2**q1
->Quaternion[-56,18,20,28]

```



## Mercury


A possible implementation of quaternions in Mercury (the simplest representation) would look like this.  Note that this is a full module implementation, complete with boilerplate, and that it works by giving an explicit conversion function for floats, converting a float into a quaternion representation of that float.  Thus the float value <code>7.0</code> gets turned into the quaternion representation <code>q(7.0, 0.0, 0.0, 0.0)</code> through the function call <code>r(7.0)</code>.


```Mercury
:- module quaternion.

:- interface.

:- import_module float.

:- type quaternion
    --->    q(  w   :: float,
                i   :: float,
                j   :: float,
                k   :: float    ).

% conversion
:- func r(float) = quaternion is det.

% operations
:- func norm(quaternion) = float is det.
:- func -quaternion = quaternion is det.
:- func conjugate(quaternion) = quaternion is det.
:- func quaternion + quaternion = quaternion is det.
:- func quaternion * quaternion = quaternion is det.

:- implementation.

:- import_module math.

% conversion
r(W) = q(W, 0.0, 0.0, 0.0).

% operations
norm(q(W, I, J, K)) = math.sqrt(W*W + I*I + J*J + K*K).
-q(W, I, J, K) = q(-W, -I, -J, -K).
conjugate(q(W, I, J, K)) = q(W, -I, -J, -K).
q(W0, I0, J0, K0) + q(W1, I1, J1, K1) = q(W0+W1, I0+I1, J0+J1, K0+K1).
q(W0, I0, J0, K0) * q(W1, I1, J1, K1) = q(W0*W1 - I0*I1 - J0*J1 - K0*K1,
                                          W0*I1 + I0*W1 + J0*K1 - K0*J1,
                                          W0*J1 - I0*K1 + J0*W1 + K0*I1,
                                          W0*K1 + I0*J1 - J0*I1 + K0*W1 ).
```


The following test module puts the module through its paces.


```Mercury
:- module test_quaternion.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module quaternion.

:- import_module exception.
:- import_module float.
:- import_module list.
:- import_module string.

:- func to_string(quaternion) = string is det.

main(!IO) :-
    Q  = q(1.0, 2.0, 3.0, 4.0),
    Q1 = q(2.0, 3.0, 4.0, 5.0),
    Q2 = q(3.0, 4.0, 5.0, 6.0),
    R = 7.0,
    QR = r(R),

    io.print("Q = ", !IO), io.print(to_string(Q), !IO), io.nl(!IO),
    io.print("Q1 = ", !IO), io.print(to_string(Q1), !IO), io.nl(!IO),
    io.print("Q2 = ", !IO), io.print(to_string(Q2), !IO), io.nl(!IO),
    io.print("R = ", !IO), io.print(R, !IO), io.nl(!IO),
    io.nl(!IO),

    io.print("1. The norm of a quaternion.\n", !IO),
    io.print("norm(Q) = ", !IO), io.print(norm(Q), !IO), io.nl(!IO),
    io.nl(!IO),

    io.print("2. The negative of a quaternion.\n", !IO),
    io.print("-Q = ", !IO), io.print(to_string(-Q), !IO), io.nl(!IO),
    io.nl(!IO),

    io.print("3. The conjugate of a quaternion.\n", !IO),
    io.print("conjugate(Q) = ", !IO), io.print(to_string(conjugate(Q)), !IO),
        io.nl(!IO),
    io.nl(!IO),

    io.print("4. Addition of a real number and a quaternion.\n", !IO),
    ( Q + QR = QR + Q ->    io.print("Addition is commutative.\n", !IO)
    ;                       io.print("Addition is not commutative.\n", !IO) ),
    io.print("Q + R = ", !IO), io.print(to_string(Q + QR), !IO), io.nl(!IO),
    io.print("R + Q = ", !IO), io.print(to_string(QR + Q), !IO), io.nl(!IO),
    io.nl(!IO),

    io.print("5. Addition of two quaternions.\n", !IO),
    ( Q1 + Q2 = Q2 + Q1 ->  io.print("Addition is commutative.\n", !IO)
    ;                       io.print("Addition is not commutative.\n", !IO) ),
    io.print("Q1 + Q2 = ", !IO), io.print(to_string(Q1 + Q2), !IO), io.nl(!IO),
    io.print("Q2 + Q1 = ", !IO), io.print(to_string(Q2 + Q1), !IO), io.nl(!IO),
    io.nl(!IO),

    io.print("6. Multiplication of a real number and a quaternion.\n", !IO),
    ( Q * QR = QR * Q ->    io.print("Multiplication is commutative.\n", !IO)
    ;                       io.print("Multiplication is not commutative.\n", !IO) ),
    io.print("Q * R = ", !IO), io.print(to_string(Q * QR), !IO), io.nl(!IO),
    io.print("R * Q = ", !IO), io.print(to_string(QR * Q), !IO), io.nl(!IO),
    io.nl(!IO),

    io.print("7. Multiplication of two quaternions.\n", !IO),
    ( Q1 * Q2 = Q2 * Q1 ->  io.print("Multiplication is commutative.\n", !IO)
    ;                       io.print("Multiplication is not commutative.\n", !IO) ),
    io.print("Q1 * Q2 = ", !IO), io.print(to_string(Q1 * Q2), !IO), io.nl(!IO),
    io.print("Q2 * Q1 = ", !IO), io.print(to_string(Q2 * Q1), !IO), io.nl(!IO),
    io.nl(!IO).

to_string(q(I, J, K, W)) = string.format("q(%f, %f, %f, %f)",
                           [f(I), f(J), f(K), f(W)]).
:- end_module test_quaternion.
```


The output of the above code follows:

 % ./test_quaternion
 Q = q(1.000000, 2.000000, 3.000000, 4.000000)
 Q1 = q(2.000000, 3.000000, 4.000000, 5.000000)
 Q2 = q(3.000000, 4.000000, 5.000000, 6.000000)
 R = 7.0

 1. The norm of a quaternion.
 norm(Q) = 5.477225575051661

 2. The negative of a quaternion.
 -Q = q(-1.000000, -2.000000, -3.000000, -4.000000)

 3. The conjugate of a quaternion.
 conjugate(Q) = q(1.000000, -2.000000, -3.000000, -4.000000)

 4. Addition of a real number and a quaternion.
 Addition is commutative.
 Q + R = q(8.000000, 2.000000, 3.000000, 4.000000)
 R + Q = q(8.000000, 2.000000, 3.000000, 4.000000)

 5. Addition of two quaternions.
 Addition is commutative.
 Q1 + Q2 = q(5.000000, 7.000000, 9.000000, 11.000000)
 Q2 + Q1 = q(5.000000, 7.000000, 9.000000, 11.000000)

 6. Multiplication of a real number and a quaternion.
 Multiplication is commutative.
 Q * R = q(7.000000, 14.000000, 21.000000, 28.000000)
 R * Q = q(7.000000, 14.000000, 21.000000, 28.000000)

 7. Multiplication of two quaternions.
 Multiplication is not commutative.
 Q1 * Q2 = q(-56.000000, 16.000000, 24.000000, 26.000000)
 Q2 * Q1 = q(-56.000000, 18.000000, 20.000000, 28.000000)


## OCaml


This implementation was build strictly to the specs without looking (too much) at other implementations. The implementation as a record type with only floats is said (on the ocaml mailing list) to be especially efficient.  Put this into a file quaternion.ml:

```ocaml

type quaternion = {a: float; b: float; c: float; d: float}

let norm q = sqrt (q.a**2.0 +.
                   q.b**2.0 +.
                   q.c**2.0 +.
                   q.d**2.0 )

let floatneg r = ~-. r  (* readability *)

let negative q =
  {a = floatneg q.a;
   b = floatneg q.b;
   c = floatneg q.c;
   d = floatneg q.d }

let conjugate q =
  {a = q.a;
   b = floatneg q.b;
   c = floatneg q.c;
   d = floatneg q.d }

let addrq r q = {q with a = q.a +. r}

let addq q1 q2 =
  {a = q1.a +. q2.a;
   b = q1.b +. q2.b;
   c = q1.c +. q2.c;
   d = q1.d +. q2.d  }

let multrq r q =
  {a = q.a *. r;
   b = q.b *. r;
   c = q.c *. r;
   d = q.d *. r  }

let multq q1 q2 =
        {a = q1.a*.q2.a -. q1.b*.q2.b -. q1.c*.q2.c -. q1.d*.q2.d;
         b = q1.a*.q2.b +. q1.b*.q2.a +. q1.c*.q2.d -. q1.d*.q2.c;
         c = q1.a*.q2.c -. q1.b*.q2.d +. q1.c*.q2.a +. q1.d*.q2.b;
         d = q1.a*.q2.d +. q1.b*.q2.c -. q1.c*.q2.b +. q1.d*.q2.a  }

let qmake a b c d = {a;b;c;d} (* readability omitting a= b=... *)

let qstring q =
  Printf.sprintf "(%g, %g, %g, %g)" q.a q.b q.c q.d ;;

(* test data *)
let q  = qmake 1.0  2.0  3.0  4.0
let q1 = qmake 2.0  3.0  4.0  5.0
let q2 = qmake 3.0  4.0  5.0  6.0
let r  = 7.0

let () = (* written strictly to spec *)
  let pf = Printf.printf in
  pf "starting with data q=%s, q1=%s,  q2=%s, r=%g\n" (qstring q) (qstring q1) (qstring q2) r;
  pf "1. norm of      q     = %g \n" (norm q) ;
  pf "2. negative of  q     = %s \n" (qstring (negative q));
  pf "3. conjugate of q     = %s \n" (qstring (conjugate q));
  pf "4. adding r to q      = %s \n" (qstring (addrq r q));
  pf "5. adding q1 and q2   = %s \n" (qstring (addq q1 q2));
  pf "6. multiply r and q   = %s \n" (qstring (multrq r q));
  pf "7. multiply q1 and q2 = %s \n" (qstring (multq q1 q2));
  pf "8. instead q2 * q1    = %s \n" (qstring (multq q2 q1));
  pf "\n";

```


using this file on the command line will produce:

```txt

$ ocaml quaternion.ml
starting with data q=(1, 2, 3, 4), q1=(2, 3, 4, 5),  q2=(3, 4, 5, 6), r=7
1. norm of      q     = 5.47723
2. negative of  q     = (-1, -2, -3, -4)
3. conjugate of q     = (1, -2, -3, -4)
4. adding r to q      = (8, 2, 3, 4)
5. adding q1 and q2   = (5, 7, 9, 11)
6. multiply r and q   = (7, 14, 21, 28)
7. multiply q1 and q2 = (-56, 16, 24, 26)
8. instead q2 * q1    = (-56, 18, 20, 28)

```

For completeness, and since data types are of utmost importance in OCaml, here the types produced by pasting the code into the toplevel (''ocaml'' is the toplevel):

```ocaml

type quaternion = { a : float; b : float; c : float; d : float; }
val norm : quaternion -> float = <fun>
val floatneg : float -> float = <fun>
val negative : quaternion -> quaternion = <fun>
val conjugate : quaternion -> quaternion = <fun>
val addrq : float -> quaternion -> quaternion = <fun>
val addq : quaternion -> quaternion -> quaternion = <fun>
val multrq : float -> quaternion -> quaternion = <fun>
val multq : quaternion -> quaternion -> quaternion = <fun>
val qmake : float -> float -> float -> float -> quaternion = <fun>
val qstring : quaternion -> string = <fun>

```



## Octave

There is an add-on package (toolbox) to Octave available from http://octave.sourceforge.net/quaternion/

Such a package can be install with the command:

<lang>pkg install -forge quaternion
```


Here is a sample interactive session solving the task:

<lang>> q = quaternion (1, 2, 3, 4)
q = 1 + 2i + 3j + 4k
>  q1 = quaternion (2, 3, 4, 5)
q1 = 2 + 3i + 4j + 5k
> q2 = quaternion (3, 4, 5, 6)
q2 = 3 + 4i + 5j + 6k
> r = 7
r =  7
> norm(q)
ans =  5.4772
> -q
ans = -1 - 2i - 3j - 4k
> conj(q)
ans = 1 - 2i - 3j - 4k
> q + r
ans = 8 + 2i + 3j + 4k
> q1 + q2
ans = 5 + 7i + 9j + 11k
> q * r
ans = 7 + 14i + 21j + 28k
> q1 * q2
ans = -56 + 16i + 24j + 26k
> q1 == q2
ans = 0
```




## Oforth

Setting a priority (here 160) to Quaternion class and defining #asQuaternion, integers and floats can be fully mixed with quaternions.
neg is defined as "0 self -" into Number class, so no need to define it (if #- is defined).


```Oforth
160 Number Class newPriority: Quaternion(a, b, c, d)

Quaternion method: _a  @a ;
Quaternion method: _b  @b ;
Quaternion method: _c  @c ;
Quaternion method: _d  @d ;

Quaternion method: initialize  := d := c := b := a ;
Quaternion method: <<  '(' <<c @a << ',' <<c @b << ',' <<c @c << ',' <<c @d << ')' <<c ;

Integer method: asQuaternion  self 0 0 0 Quaternion new ;
Float   method: asQuaternion  self 0 0 0 Quaternion new ;

Quaternion method: ==(q)  q _a @a == q _b @b == and q _c @c == and q _d @d == and ;
Quaternion method: norm   @a sq @b sq + @c sq + @d sq + sqrt ;
Quaternion method: conj   @a  @b neg  @c neg  @d neg Quaternion new ;
Quaternion method: +(q)   Quaternion new(q _a @a +, q _b @b +, q _c @c +, q _d @d +) ;
Quaternion method: -(q)   Quaternion new(q _a @a -, q _b @b -, q _c @c -, q _d @d -) ;

Quaternion method: *(q)
   Quaternion new(q _a @a * q _b @b * - q _c @c * - q _d @d * -,
                  q _a @b * q _b @a * + q _c @d * + q _d @c * -,
                  q _a @c * q _b @d * - q _c @a * + q _d @b * +,
                  q _a @d * q _b @c * + q _c @b * - q _d @a * + ) ;
```


Usage :


```Oforth
: test
| q q1 q2 r |

   Quaternion new(1, 2, 3, 4) ->q
   Quaternion new(2, 3, 4, 5) ->q1
   Quaternion new(3, 4, 5, 6) ->q2
   7.0 -> r

   System.Out "q       = " << q << cr
   System.Out "q1      = " << q1 << cr
   System.Out "q2      = " << q2 << cr

   System.Out "norm q  = " << q norm << cr
   System.Out "neg q   = " << q neg << cr
   System.Out "conj q  = " << q conj << cr
   System.Out "q +r    = " << q r + << cr
   System.Out "q1 + q2 = " << q1 q2 + << cr
   System.Out "q * r   = " << q r * << cr
   System.Out "q1 * q2 = " << q1 q2 * << cr
   q1 q2 * q2 q1 * == ifFalse: [ "q1q2 and q2q1 are different quaternions" println ] ;
```


{{out}}

```txt

q       = (1,2,3,4)
q1      = (2,3,4,5)
q2      = (3,4,5,6)
norm q  = 5.47722557505166
neg q   = (-1,-2,-3,-4)
conj q  = (1,-2,-3,-4)
q +r    = (8,2,3,4)
q1 + q2 = (5,7,9,11)
q * r   = (7,14,21,28)
q1 * q2 = (-56,16,24,26)
q1q2 and q2q1 are different quaternions

```



## ooRexx

Note, this example uses operator overloads to perform the math operation.  The operator overloads only work if the left-hand-side of the operation is a quaterion instance.  Thus something like "7 + q1" would not work because this would get passed to the "+" of the string class.  For those situations, the best solution would be an addition method on the .Quaternion class itself that took the appropriate action.  I've chosen not to implement those to keep the example shorter.

```ooRexx

q = .quaternion~new(1, 2, 3, 4)
q1 = .quaternion~new(2, 3, 4, 5)
q2 = .quaternion~new(3, 4, 5, 6)
r = 7

say "q            =" q
say "q1           =" q1
say "q2           =" q2
say "r            =" r
say "norm(q)      =" q~norm
say "-q           =" (-q)
say "q*           =" q~conjugate
say "q + r        =" q + r
say "q1 + q2      =" q1 + q2
say "q * r        =" q * r
q1q2 = q1 * q2
q2q1 = q2 * q1
say "q1 * q2      =" q1q2
say "q2 * q1      =" q2q1
say "q1 == q1     =" (q1 == q1)
say "q1q2 == q2q1 =" (q1q2 == q2q1)


::class quaternion
::method init
  expose r i j k
  use strict arg r, i = 0, j = 0, k = 0

-- quaternion instances are immutable, so these are
-- read only attributes
::attribute r GET
::attribute i GET
::attribute j GET
::attribute k GET

::method norm
  expose r i j k
  return rxcalcsqrt(r * r + i * i + j * j + k * k)

::method invert
  expose r i j k
  norm = self~norm
  return self~class~new(r / norm, i / norm, j / norm, k / norm)

::method negative
  expose r i j k
  return self~class~new(-r, -i, -j, -k)

::method conjugate
  expose r i j k
  return self~class~new(r, -i, -j, -k)

::method add
  expose r i j k
  use strict arg other
  if other~isa(.quaternion) then
     return self~class~new(r + other~r, i + other~i, j + other~j, k + other~k)
  else return self~class~new(r + other, i, j, k)

::method subtract
  expose r i j k
  use strict arg other
  if other~isa(.quaternion) then
     return self~class~new(r - other~r, i - other~i, j - other~j, k - other~k)
  else return self~class~new(r - other, i, j, k)

::method times
  expose r i j k
  use strict arg other
  if other~isa(.quaternion) then
     return self~class~new(r * other~r - i * other~i - j * other~j - k * other~k, -
                           r * other~i + i * other~r + j * other~k - k * other~j, -
                           r * other~j - i * other~k + j * other~r + k * other~i, -
                           r * other~k + i * other~j - j * other~i + k * other~r)
  else return self~class~new(r * other, i * other, j * other, k * other)

::method divide
  use strict arg other
  -- this is easier if everything is a quaternion
  if \other~isA(.quaternion) then other = .quaternion~new(other)
  -- division is multiplication with the inversion
  return self * other~invert

::method "=="
  expose r i j k
  use strict arg other

  if \other~isa(.quaternion) then return .false
  -- Note:  these are numeric comparisons, so we're using the "="
  -- method so those are handled correctly
  return r = other~r & i = other~i & j = other~j & k = other~k

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
  expose r i j k
  return r self~formatnumber(i)"i" self~formatnumber(j)"j" self~formatnumber(k)"k"

::method formatnumber private
  use arg value
  if value > 0 then return "+" value
  else return "-" value~abs

-- override hashcode for collection class hash uses
::method hashCode
  expose r i j k
  return r~hashcode~bitxor(i~hashcode)~bitxor(j~hashcode)~bitxor(k~hashcode)


::requires rxmath LIBRARY


```

{{out}}

```txt

q            = 1 + 2i + 3j + 4k
q1           = 2 + 3i + 4j + 5k
q2           = 3 + 4i + 5j + 6k
r            = 7
norm(q)      = 5.47722558
-q           = -1 - 2i - 3j - 4k
q*           = 1 - 2i - 3j - 4k
q + r        = 8 + 2i + 3j + 4k
q1 + q2      = 5 + 7i + 9j + 11k
q * r        = 7 + 14i + 21j + 28k
q1 * q2      = -56 + 16i + 24j + 26k
q2 * q1      = -56 + 18i + 20j + 28k
q1 == q1     = 1
q1q2 == q2q1 = 0

```



## PARI/GP

{{works with|PARI/GP|version 2.4.2 and above}}<!-- Needs closures -->
Here is a simple solution in GP.  I think it's possible to implement this type directly in Pari by abusing t_COMPLEX, but I haven't attempted this.

```parigp
q.norm={
	if(type(q) != "t_VEC" || #q != 4, error("incorrect type"));
	sqrt(q[1]^2+q[2]^2+q[3]^2+q[4]^2)
};
q.conj={
	if(type(q) != "t_VEC" || #q != 4, error("incorrect type"));
	-[-q[1],q[2],q[3],q[4]]
};
q.add={
	if(type(q) != "t_VEC" || #q != 4, error("incorrect type"));
	x->if(type(x) == "t_INT" || type(x) == t_REAL,
		[q[1]+x,q[2],q[3],q[4]]
	,
		if(type(x) == "t_VEC" && #x == 4,
			q+x
		,
			error("incorrect type")
		)
	)
};
q.mult={
	if(type(q) != "t_VEC" || #q != 4, error("incorrect type"));
	x->if(type(x) == "t_INT" || type(x) == t_REAL,
		x*q
	,
		if(type(x) == "t_VEC" && #x == 4,
			[q[1]*x[1] - q[2]*x[2] - q[3]*x[3] - q[4]*x[4],
			q[1]*x[2] + q[2]*x[1] + q[3]*x[4] - q[4]*x[3],
			q[1]*x[3] - q[2]*x[4] + q[3]*x[1] + q[4]*x[2],
			q[1]*x[4] + q[2]*x[3] - q[3]*x[2] + q[4]*x[1]]
		,
			error("incorrect type")
		)
	)
};
```

Usage:

```parigp
r=7;q=[1,2,3,4];q1=[2,3,4,5];q2=[3,4,5,6];
q.norm
-q
q.conj
q.add(r)
q1.add(q2)
q1.add(q2)	\\ or q1+q2
q.mult(r)	\\ or r*q or q*r
q1.mult(q2)
q1.mult(q2) != q2.mult(q1)
```



## Pascal

The  [[Quaternion_type#Delphi | Delphi]] example also works with FreePascal.


## Perl


```Perl
package Quaternion;
use List::Util 'reduce';
use List::MoreUtils 'pairwise';

sub make {
        my $cls = shift;
        if (@_ == 1)    { return bless [ @_, 0, 0, 0 ] }
        elsif (@_ == 4) { return bless [ @_ ] }
        else            { die "Bad number of components: @_" }
}

sub _abs { sqrt reduce { $a + $b * $b } @{ +shift } }
sub _neg { bless [ map(-$_, @{+shift}) ] }
sub _str { "(@{+shift})" }

sub _add {
        my ($x, $y) = @_;
        $y = [ $y, 0, 0, 0 ] unless ref $y;
        bless [ pairwise { $a + $b } @$x, @$y ]
}

sub _sub {
        my ($x, $y, $swap) = @_;
        $y = [ $y, 0, 0, 0 ] unless ref $y;
        my @x = pairwise { $a - $b } @$x, @$y;
        if ($swap) { $_ = -$_ for @x }
        bless \@x;
}

sub _mul {
        my ($x, $y) = @_;
        if (!ref $y) { return bless [ map($_ * $y, @$x) ] }
        my ($a1, $b1, $c1, $d1) = @$x;
        my ($a2, $b2, $c2, $d2) = @$y;
        bless [ $a1 * $a2 - $b1 * $b2 - $c1 * $c2 - $d1 * $d2,
                $a1 * $b2 + $b1 * $a2 + $c1 * $d2 - $d1 * $c2,
                $a1 * $c2 - $b1 * $d2 + $c1 * $a2 + $d1 * $b2,
                $a1 * $d2 + $b1 * $c2 - $c1 * $b2 + $d1 * $a2]
}

sub conjugate {
        my @a = map { -$_ } @{$_[0]};
        $a[0] = $_[0][0];
        bless \@a
}

use overload (
        '""'    => \&_str,
        '+'     => \&_add,
        '-'     => \&_sub,
        '*'     => \&_mul,
        'neg'   => \&_neg,
        'abs'   => \&_abs,
);

package main;

my $a = Quaternion->make(1, 2, 3, 4);
my $b = Quaternion->make(1, 1, 1, 1);

print "a = $a\n";
print "b = $b\n";
print "|a| = ", abs($a), "\n";
print "-a = ", -$a, "\n";
print "a + 1 = ", $a + 1, "\n";
print "a + b = ", $a + $b, "\n";
print "a - b = ", $a - $b, "\n";
print "a conjugate is ", $a->conjugate, "\n";
print "a * b = ", $a * $b, "\n";
print "b * a = ", $b * $a, "\n";
```



## Perl 6


```perl6
class Quaternion {
    has Real ( $.r, $.i, $.j, $.k );

    multi method new ( Real $r, Real $i, Real $j, Real $k ) {
        self.bless: :$r, :$i, :$j, :$k;
    }
    multi qu(*@r) is export { Quaternion.new: |@r }
    sub postfix:<j>(Real $x) is export { qu 0, 0, $x, 0 }
    sub postfix:<k>(Real $x) is export { qu 0, 0, 0, $x }

    method Str   () { "$.r + {$.i}i + {$.j}j + {$.k}k" }
    method reals () { $.r, $.i, $.j, $.k }
    method conj  () { qu $.r, -$.i, -$.j, -$.k }
    method norm  () { sqrt [+] self.reals X** 2 }

    multi infix:<eqv> ( Quaternion $a, Quaternion $b ) is export { $a.reals eqv $b.reals }

    multi infix:<+> ( Quaternion $a,       Real $b ) is export { qu $b+$a.r, $a.i, $a.j, $a.k }
    multi infix:<+> (       Real $a, Quaternion $b ) is export { qu $a+$b.r, $b.i, $b.j, $b.k }
    multi infix:<+> ( Quaternion $a,    Complex $b ) is export { qu $b.re + $a.r, $b.im + $a.i, $a.j, $a.k }
    multi infix:<+> (    Complex $a, Quaternion $b ) is export { qu $a.re + $b.r, $a.im + $b.i, $b.j, $b.k }
    multi infix:<+> ( Quaternion $a, Quaternion $b ) is export { qu $a.reals Z+ $b.reals }

    multi prefix:<-> ( Quaternion $a ) is export { qu $a.reals X* -1 }

    multi infix:<*> ( Quaternion $a,       Real $b ) is export { qu $a.reals X* $b }
    multi infix:<*> (       Real $a, Quaternion $b ) is export { qu $b.reals X* $a }
    multi infix:<*> ( Quaternion $a,    Complex $b ) is export { $a * qu $b.reals, 0, 0 }
    multi infix:<*> ( Complex $a,    Quaternion $b ) is export { $b R* qu $a.reals, 0, 0 }

    multi infix:<*> ( Quaternion $a, Quaternion $b ) is export {
	my @a_rijk            = $a.reals;
	my ( $r, $i, $j, $k ) = $b.reals;
	return qu [+]( @a_rijk Z* $r, -$i, -$j, -$k ), # real
		  [+]( @a_rijk Z* $i,  $r,  $k, -$j ), # i
		  [+]( @a_rijk Z* $j, -$k,  $r,  $i ), # j
		  [+]( @a_rijk Z* $k,  $j, -$i,  $r ); # k
    }
}
import Quaternion;

my $q  = 1 + 2i + 3j + 4k;
my $q1 = 2 + 3i + 4j + 5k;
my $q2 = 3 + 4i + 5j + 6k;
my $r  = 7;

say "1) q norm  = {$q.norm}";
say "2) -q      = {-$q}";
say "3) q conj  = {$q.conj}";
say "4) q  + r  = {$q + $r}";
say "5) q1 + q2 = {$q1 + $q2}";
say "6) q  * r  = {$q  * $r}";
say "7) q1 * q2 = {$q1 * $q2}";
say "8) q1q2 { $q1 * $q2 eqv $q2 * $q1 ?? '==' !! '!=' } q2q1";
```

{{out}}

```txt
1) q norm  = 5.47722557505166
2) -q      = -1 + -2i + -3j + -4k
3) q conj  = 1 + -2i + -3j + -4k
4) q  + r  = 8 + 2i + 3j + 4k
5) q1 + q2 = 5 + 7i + 9j + 11k
6) q  * r  = 7 + 14i + 21j + 28k
7) q1 * q2 = -56 + 16i + 24j + 26k
8) q1q2 != q2q1
```



## Phix

{{Trans|Euphoria}}

```Phix
function norm(sequence q)
    return sqrt(sum(sq_power(q,2)))
end function

function conj(sequence q)
    q[2..4] = sq_uminus(q[2..4])
    return q
end function

function add(object q1, object q2)
    if atom(q1)!=atom(q2) then
        if atom(q1) then
            q1 = {q1,0,0,0}
        else
            q2 = {q2,0,0,0}
        end if
    end if
    return sq_add(q1,q2)
end function

function mul(object q1, object q2)
    if sequence(q1) and sequence(q2) then
        return { q1[1]*q2[1] - q1[2]*q2[2] - q1[3]*q2[3] - q1[4]*q2[4],
                 q1[1]*q2[2] + q1[2]*q2[1] + q1[3]*q2[4] - q1[4]*q2[3],
                 q1[1]*q2[3] - q1[2]*q2[4] + q1[3]*q2[1] + q1[4]*q2[2],
                 q1[1]*q2[4] + q1[2]*q2[3] - q1[3]*q2[2] + q1[4]*q2[1] }
    else
        return sq_mul(q1,q2)
    end if
end function

function quats(sequence q)
    return sprintf("%g + %gi + %gj + %gk",q)
end function

constant
    q  = {1, 2, 3, 4},
    q1 = {2, 3, 4, 5},
    q2 = {3, 4, 5, 6},
    r  = 7

printf(1, "q = %s\n", {quats(q)})
printf(1, "r = %g\n", r)
printf(1, "norm(q) = %g\n", norm(q))
printf(1, "-q = %s\n", {quats(-q)})
printf(1, "conj(q) = %s\n", {quats(conj(q))})
printf(1, "q + r = %s\n", {quats(add(q,r))})
printf(1, "q * r = %s\n", {quats(mul(q,r))})
printf(1, "q1 = %s\n", {quats(q1)})
printf(1, "q2 = %s\n", {quats(q2)})
printf(1, "q1 + q2 = %s\n", {quats(add(q1,q2))})
printf(1, "q2 + q1 = %s\n", {quats(add(q2,q1))})
printf(1, "q1 * q2 = %s\n", {quats(mul(q1,q2))})
printf(1, "q2 * q1 = %s\n", {quats(mul(q2,q1))})
```

{{out}}

```txt

q = 1 + 2i + 3j + 4k
r = 7
norm(q) = 5.47723
-q = -1 + -2i + -3j + -4k
conj(q) = 1 + -2i + -3j + -4k
q + r = 8 + 2i + 3j + 4k
q * r = 7 + 14i + 21j + 28k
q1 = 2 + 3i + 4j + 5k
q2 = 3 + 4i + 5j + 6k
q1 + q2 = 5 + 7i + 9j + 11k
q2 + q1 = 5 + 7i + 9j + 11k
q1 * q2 = -56 + 16i + 24j + 26k
q2 * q1 = -56 + 18i + 20j + 28k

```



## PicoLisp


```PicoLisp
(scl 6)

(def 'quatCopy copy)

(de quatNorm (Q)
   (sqrt (sum * Q Q)) )

(de quatNeg (Q)
   (mapcar - Q) )

(de quatConj (Q)
   (cons (car Q) (mapcar - (cdr Q))) )

(de quatAddR (Q R)
   (cons (+ R (car Q)) (cdr Q)) )

(de quatAdd (Q1 Q2)
   (mapcar + Q1 Q2) )

(de quatMulR (Q R)
   (mapcar */ (mapcar * Q (circ R)) (1.0 .)) )

(de quatMul (Q1 Q2)
   (mapcar
      '((Ops I)
         (sum '((Op R I) (Op (*/ R (get Q2 I) 1.0))) Ops Q1 I) )
      '((+ - - -) (+ + + -) (+ - + +) (+ + - +))
      '((1 2 3 4) (2 1 4 3) (3 4 1 2) (4 3 2 1)) ) )

(de quatFmt (Q)
   (mapcar '((R S) (pack (format R *Scl) S))
      Q
      '(" + " "i + " "j + " "k") ) )
```

Test:

```PicoLisp
(setq
   Q (1.0 2.0 3.0 4.0)
   Q1 (2.0 3.0 4.0 5.0)
   Q2 (3.0 4.0 5.0 6.0)
   R 7.0 )

(prinl "R  = " (format R *Scl))
(prinl "Q  = " (quatFmt Q))
(prinl "Q1 = " (quatFmt Q1))
(prinl "Q2 = " (quatFmt Q2))
(prinl)
(prinl "norm(Q)  = " (format (quatNorm Q) *Scl))
(prinl "norm(Q1) = " (format (quatNorm Q1) *Scl))
(prinl "norm(Q2) = " (format (quatNorm Q2) *Scl))
(prinl "neg(Q)   = " (quatFmt (quatNeg Q)))
(prinl "conj(Q)  = " (quatFmt (quatConj Q)))
(prinl "Q + R    = " (quatFmt (quatAddR Q R)))
(prinl "Q1 + Q2  = " (quatFmt (quatAdd Q1 Q2)))
(prinl "Q * R    = " (quatFmt (quatMulR Q R)))
(prinl "Q1 * Q2  = " (quatFmt (quatMul Q1 Q2)))
(prinl "Q2 * Q1  = " (quatFmt (quatMul Q2 Q1)))
(prinl (if (= (quatMul Q1 Q2) (quatMul Q2 Q1)) "Equal" "Not equal"))
```

{{out}}

```txt
R  = 7.000000
Q  = 1.000000 + 2.000000i + 3.000000j + 4.000000k
Q1 = 2.000000 + 3.000000i + 4.000000j + 5.000000k
Q2 = 3.000000 + 4.000000i + 5.000000j + 6.000000k

norm(Q)  = 5.477225
norm(Q1) = 7.348469
norm(Q2) = 9.273618
neg(Q)   = -1.000000 + -2.000000i + -3.000000j + -4.000000k
conj(Q)  = 1.000000 + -2.000000i + -3.000000j + -4.000000k
Q + R    = 8.000000 + 2.000000i + 3.000000j + 4.000000k
Q1 + Q2  = 5.000000 + 7.000000i + 9.000000j + 11.000000k
Q * R    = 7.000000 + 14.000000i + 21.000000j + 28.000000k
Q1 * Q2  = -56.000000 + 16.000000i + 24.000000j + 26.000000k
Q2 * Q1  = -56.000000 + 18.000000i + 20.000000j + 28.000000k
Not equal
```



## PL/I


```pli
*process source attributes xref or(!);
 qu: Proc Options(main);
 /**********************************************************************
 * 06.09.2013 Walter Pachl translated from REXX
 *            added tasks 9 and A
 **********************************************************************/
 dcl v(4) Char(1) Var Init('','i','j','k');
 define structure 1 quat, 2 x(4) Dec Float(15);
 Dcl q  type quat; Call quat_init(q, 1,2,3,4);
 Dcl q1 type quat; Call quat_init(q1,2,3,4,5);
 Dcl q2 type quat; Call quat_init(q2,3,4,5,6);
 Dcl q3 type quat; Call quat_init(q3,-2,3,-4,-5);
 Dcl r  Dec Float(15)Init(7);

 call showq('       ','q'                    ,q);
 call showq('       ','q1'                   ,q1);
 call showq('       ','q2'                   ,q2);
 call showq('       ','q3'                   ,q3);
 call shows('       ','r'                    ,r);
 Call shows('task 1:','norm q'               ,norm(q));
 Call showq('task 2:','quatneg q'            ,quatneg(q));
 Call showq('task 3:','conjugate q'          ,quatConj(q));
 Call showq('task 4:','addition r+q'         ,quatAddsq(r,q));
 Call showq('task 5:','addition q1+q2'       ,quatAdd(q1,q2));
 Call showq('task 6:','multiplication q*r'   ,quatMulqs(q,r));
 Call showq('task 7:','multiplication q1*q2' ,quatMul(q1,q2));
 Call showq('task 8:','multiplication q2*q1' ,quatMul(q2,q1));
 Call showq('task 9:','quatsub q1-q1'        ,quatAdd(q1,quatneg(q1)));
 Call showq('task A:','addition q1+q3'       ,quatAdd(q1,q3));
 Call showt('task B:','equal'                ,quatEqual(quatMul(q1,q2),
                                                       quatMul(q2,q1)));
 Call showt('task C:','q1=q1'                ,quatEqual(q1,q1));

 quatNeg: procedure(qp) Returns(type quat);
 Dcl (qp,qr) type quat;
 qr.x(*)=-qp.x(*);
 Return (qr);
 End;

 quatAdd: procedure(qp,qq) Returns(type quat);
 Dcl (qp,qq,qr) type quat;
 qr.x(*)=qp.x(*)+qq.x(*);
 Return (qr);
 End;

 quatAddsq: procedure(v,qp) Returns(type quat);
 Dcl v Dec Float(15);
 Dcl (qp,qr) type quat;
 qr.x(*)=qp.x(*);
 qr.x(1)=qp.x(1)+v;
 Return (qr);
 End;

 quatConj: procedure(qp) Returns(type quat);
 Dcl (qp,qr) type quat;
 qr.x(*)=-qp.x(*);
 qr.x(1)= qp.x(1);
 Return (qr);
 End;

 quatMul: procedure(qp,qq) Returns(type quat);
 Dcl (qp,qq,qr) type quat;
 qr.x(1)=
        qp.x(1)*qq.x(1)-qp.x(2)*qq.x(2)-qp.x(3)*qq.x(3)-qp.x(4)*qq.x(4);
 qr.x(2)=
        qp.x(1)*qq.x(2)+qp.x(2)*qq.x(1)+qp.x(3)*qq.x(4)-qp.x(4)*qq.x(3);
 qr.x(3)=
        qp.x(1)*qq.x(3)-qp.x(2)*qq.x(4)+qp.x(3)*qq.x(1)+qp.x(4)*qq.x(2);
 qr.x(4)=
        qp.x(1)*qq.x(4)+qp.x(2)*qq.x(3)-qp.x(3)*qq.x(2)+qp.x(4)*qq.x(1);
 Return (qr);
 End;

 quatMulqs: procedure(qp,v) Returns(type quat);
 Dcl (qp,qr) type quat;
 Dcl v Dec Float(15);
 qr.x(*)=qp.x(*)*v;
 Return (qr);
 End;

 shows: Procedure(t1,t2,v);
 Dcl (t1,t2) Char(*);
 Dcl v Dec Float(15);
 Put Edit(t1,right(t2,24),'  --> ',v)(Skip,a,a,a,f(15,13));
 End;

 showt: Procedure(t1,t2,v);
 Dcl (t1,t2) Char(*);
 Dcl v Char(*) Var);
 Put Edit(t1,right(t2,24),'  --> ',v)(Skip,a,a,a,a);
 End;

 showq: Procedure(t1,t2,qp);
 Dcl qp type quat;
 Dcl (t1,t2) Char(*);
 Dcl (s,s2,p) Char(100) Var Init('');
 Dcl i Bin Fixed(31);
 Put String(s) Edit(t1,right(t2,24),'  --> ')(a,a,a);
 Do i=1 To 4;
   Put String(p) Edit(abs(qp.x(i)))(p'ZZZ9');
   p=trim(p);
   Select;
     When(qp.x(i)<0) p='-'!!p!!v(i);
     When(p=0) p='';
     Otherwise Do
       If s2^='' Then p='+'!!p;
       If i>1 Then p=p!!v(i);
       End;
     End;
   s2=s2!!p
   End;
 If s2='' Then
   s2='0';
 Put Edit(s!!s2)(Skip,a);
 End;

 norm: Procedure(qp) Returns(Dec Float(15));
 Dcl qp type quat;
 Dcl r  Dec Float(15) Init(0);
 Dcl i  Bin Fixed(31);
 Do i=1 To 4;
   r=r+qp.x(i)**2;
   End;
 Return (sqrt(r));
 End;

 quat_init: Proc(qp,x,y,z,u);
 Dcl qp type quat;
 Dcl (x,y,z,u) Dec Float(15);
 qp.x(1)=x;
 qp.x(2)=y;
 qp.x(3)=z;
 qp.x(4)=u;
 End;

 End;
```

{{out}}

```txt

                              q  --> 1+2i+3j+4k
                             q1  --> 2+3i+4j+5k
                             q2  --> 3+4i+5j+6k
                             q3  --> -2+3i-4j-5k
                              r  --> 7.0000000000000
task 1:                  norm q  --> 5.4772255750517
task 2:               quatneg q  --> -1-2i-3j-4k
task 3:             conjugate q  --> 1-2i-3j-4k
task 4:            addition r+q  --> 8+2i+3j+4k
task 5:          addition q1+q2  --> 5+7i+9j+11k
task 6:      multiplication q*r  --> 7+14i+21j+28k
task 7:    multiplication q1*q2  --> -56+16i+24j+26k
task 8:    multiplication q2*q1  --> -56+18i+20j+28k
task 9:           quatsub q1-q1  --> 0
task A:          addition q1+q3  --> 6i
task B:                   equal  --> not equal
task C:                   q1=q1  --> equal

```




## PowerShell


### Implementation


```PowerShell

class Quaternion {
  [Double]$w
  [Double]$x
  [Double]$y
  [Double]$z
  Quaternion() {
      $this.w = 0
      $this.x = 0
      $this.y = 0
      $this.z = 0
  }
  Quaternion([Double]$a, [Double]$b, [Double]$c, [Double]$d) {
      $this.w = $a
      $this.x = $b
      $this.y = $c
      $this.z = $d
  }
  [Double]abs2() {return $this.w*$this.w + $this.x*$this.x + $this.y*$this.y + $this.z*$this.z}
  [Double]abs() {return [math]::sqrt($this.wbs2())}
  static [Quaternion]real([Double]$r) {return [Quaternion]::new($r, 0, 0, 0)}
  static [Quaternion]add([Quaternion]$m,[Quaternion]$n) {return [Quaternion]::new($m.w+$n.w, $m.x+$n.x, $m.y+$n.y, $m.z+$n.z)}
  [Quaternion]addreal([Double]$r) {return [Quaternion]::add($this,[Quaternion]::real($r))}
  static [Quaternion]mul([Quaternion]$m,[Quaternion]$n) {
    return [Quaternion]::new(
    ($m.w*$n.w) - ($m.x*$n.x) - ($m.y*$n.y) - ($m.z*$n.z),
    ($m.w*$n.x) + ($m.x*$n.w) + ($m.y*$n.z) - ($m.z*$n.y),
    ($m.w*$n.y) - ($m.x*$n.z) + ($m.y*$n.w) + ($m.z*$n.x),
    ($m.w*$n.z) + ($m.x*$n.y) - ($m.y*$n.x) + ($m.z*$n.w))
  }

  [Quaternion]mul([Double]$r) {return [Quaternion]::new($r*$this.w, $r*$this.x, $r*$this.y, $r*$this.z)}
  [Quaternion]negate() {return $this.mul(-1)}
  [Quaternion]conjugate() {return [Quaternion]::new($this.w, -$this.x, -$this.y, -$this.z)}
  static [String]st([Double]$r) {
        if(0 -le $r) {return "+$r"} else {return "$r"}
  }
  [String]show() {return "$($this.w)$([Quaternion]::st($this.x))i$([Quaternion]::st($this.y))j$([Quaternion]::st($this.z))k"}
  static [String]show([Quaternion]$other) {return $other.show()}
}


$q  = [Quaternion]::new(1, 2, 3, 4)
$q1 = [Quaternion]::new(2, 3, 4, 5)
$q2 = [Quaternion]::new(3, 4, 5, 6)
$r = 7
"`$q: $($q.show())"
"`$q1: $($q1.show())"
"`$q2: $($q2.show())"
"`$r: $r"
""
"norm `$q: $($q.wbs())"
"negate `$q: $($q.negate().show())"
"conjugate `$q: $($q.yonjugate().show())"
"`$q + `$r: $($q.wddreal($r).show())"
"`$q1 + `$q2: $([Quaternion]::show([Quaternion]::add($q1,$q2)))"
"`$q * `$r: $($q.mul($r).show())"
"`$q1 * `$q2: $([Quaternion]::show([Quaternion]::mul($q1,$q2)))"
"`$q2 * `$q1: $([Quaternion]::show([Quaternion]::mul($q2,$q1)))"

```

<b>Output:</b>

```txt

norm $q: 5.47722557505166
negate $q: -1-2i-3j-4k
conjugate $q: 1-2i-3j-4k
$q + $r: 8+2i+3j+4k
$q1 + $q2: 5+7i+9j+11k
$q * $r: 7+14i+21j+28k
$q1 * $q2: -56+16i+24j+26k
$q2 * $q1: -56+18i+20j+28k

```


### Library


```PowerShell

function show([System.Numerics.Quaternion]$c) {
    function st([Double]$r) {
            if(0 -le $r) {return "+$r"} else {return "$r"}
    }
    return "$($c.w)$(st $c.y)i$(st $c.y)j$(st $c.z)k"
}
$q  = [System.Numerics.Quaternion]::new(1, 2, 3, 4)
$q1 = [System.Numerics.Quaternion]::new(2, 3, 4, 5)
$q2 = [System.Numerics.Quaternion]::new(3, 4, 5, 6)
$r = 7
"`$q: $(show $q)"
"`$q1: $(show $q1)"
"`$q2: $(show $q2)"
"`$r: $r"
"norm `$q: $($q.Length())"
"negate `$q: $(show ([System.Numerics.Quaternion]::Negate($q)))"
"conjugate `$q: $(show ([System.Numerics.Quaternion]::Conjugate($q)))"
"`$q + `$r: $(show ([System.Numerics.Quaternion]::new($q.w + $r, $q.x, $q.y, $q.z)))"
"`$q1 + `$q2: $(show ([System.Numerics.Quaternion]::Add($q1,$q2)))"
"`$q * `$r: $(show ([System.Numerics.Quaternion]::new($q.w * $r, $q.x * $r, $q.y * $r, $q.z * $r)))"
"`$q1 * `$q2: $(show ([System.Numerics.Quaternion]::Multiply($q1,$q2)))"
"`$q2 * `$q1: $(show ([System.Numerics.Quaternion]::Multiply($q2,$q1)))"

```

<b>Output:</b>

```txt

norm $q: 5.47722557505166
negate $q: -1-2i-3j-4k
conjugate $q: 1-2i-3j-4k
$q + $r: 8+2i+3j+4k
$q1 + $q2: 5+7i+9j+11k
$q * $r: 7+14i+21j+28k
$q1 * $q2: -56+16i+24j+26k
$q2 * $q1: -56+18i+20j+28k

```



## Prolog


```Prolog
% A quaternion is represented as a complex term qx/4
add(qx(R0,I0,J0,K0), qx(R1,I1,J1,K1), qx(R,I,J,K)) :-
	!, R is R0+R1, I is I0+I1, J is J0+J1, K is K0+K1.
add(qx(R0,I,J,K), F, qx(R,I,J,K)) :-
	number(F), !, R is R0 + F.
add(F, qx(R0,I,J,K), Qx) :-
	add(qx(R0,I,J,K), F, Qx).
mul(qx(R0,I0,J0,K0), qx(R1,I1,J1,K1), qx(R,I,J,K)) :- !,
	R is R0*R1 - I0*I1 - J0*J1 - K0*K1,
	I is R0*I1 + I0*R1 + J0*K1 - K0*J1,
	J is R0*J1 - I0*K1 + J0*R1 + K0*I1,
	K is R0*K1 + I0*J1 - J0*I1 + K0*R1.
mul(qx(R0,I0,J0,K0), F, qx(R,I,J,K)) :-
	number(F), !, R is R0*F, I is I0*F, J is J0*F, K is K0*F.
mul(F, qx(R0,I0,J0,K0), Qx) :-
	mul(qx(R0,I0,J0,K0),F,Qx).
abs(qx(R,I,J,K), Norm) :-
	Norm is sqrt(R*R+I*I+J*J+K*K).
negate(qx(Ri,Ii,Ji,Ki),qx(R,I,J,K)) :-
	R is -Ri, I is -Ii, J is -Ji, K is -Ki.
conjugate(qx(R,Ii,Ji,Ki),qx(R,I,J,K)) :-
	I is -Ii, J is -Ji, K is -Ki.
```


'''Test:'''

```Prolog
data(q,  qx(1,2,3,4)).
data(q1, qx(2,3,4,5)).
data(q2, qx(3,4,5,6)).
data(r, 7).

test :-	data(Name, qx(A,B,C,D)), abs(qx(A,B,C,D), Norm),
	writef('abs(%w) is %w\n', [Name, Norm]), fail.
test :- data(q, Qx), negate(Qx, Nqx),
	writef('negate(%w) is %w\n', [q, Nqx]), fail.
test :- data(q, Qx), conjugate(Qx, Nqx),
	writef('conjugate(%w) is %w\n', [q, Nqx]), fail.
test :- data(q1, Q1), data(q2, Q2), add(Q1, Q2, Qx),
	writef('q1+q2 is %w\n', [Qx]), fail.
test :- data(q1, Q1), data(q2, Q2), add(Q2, Q1, Qx),
	writef('q2+q1 is %w\n', [Qx]), fail.
test :- data(q, Qx), data(r, R), mul(Qx, R, Nqx),
	writef('q*r is %w\n', [Nqx]), fail.
test :- data(q, Qx), data(r, R), mul(R, Qx, Nqx),
	writef('r*q is %w\n', [Nqx]), fail.
test :- data(q1, Q1), data(q2, Q2), mul(Q1, Q2, Qx),
	writef('q1*q2 is %w\n', [Qx]), fail.
test :- data(q1, Q1), data(q2, Q2), mul(Q2, Q1, Qx),
	writef('q2*q1 is %w\n', [Qx]), fail.
test.
```

{{out}}

```txt
 ?- test.
abs(q) is 5.477225575051661
abs(q1) is 7.3484692283495345
abs(q2) is 9.273618495495704
negate(q) is qx(-1,-2,-3,-4)
conjugate(q) is qx(1,-2,-3,-4)
q1+q2 is qx(5,7,9,11)
q2+q1 is qx(5,7,9,11)
q*r is qx(7,14,21,28)
r*q is qx(7,14,21,28)
q1*q2 is qx(-56,16,24,26)
q2*q1 is qx(-56,18,20,28)
```



## PureBasic


```PureBasic
Structure Quaternion
  a.f
  b.f
  c.f
  d.f
EndStructure

Procedure.f QNorm(*x.Quaternion)
  ProcedureReturn Sqr(Pow(*x\a, 2) + Pow(*x\b, 2) + Pow(*x\c, 2) + Pow(*x\d, 2))
EndProcedure

;If supplied, the result is returned in the quaternion structure *res,
;otherwise a new quaternion is created.  A pointer to the result is returned.
Procedure QNeg(*x.Quaternion, *res.Quaternion = 0)
  If *res = 0: *res.Quaternion = AllocateMemory(SizeOf(Quaternion)): EndIf
  If *res
    *res\a = -*x\a
    *res\b = -*x\b
    *res\c = -*x\c
    *res\d = -*x\d
  EndIf
  ProcedureReturn *res
EndProcedure

Procedure QConj(*x.Quaternion, *res.Quaternion = 0)
  If *res = 0: *res.Quaternion = AllocateMemory(SizeOf(Quaternion)): EndIf
  If *res
    *res\a =  *x\a
    *res\b = -*x\b
    *res\c = -*x\c
    *res\d = -*x\d
  EndIf
  ProcedureReturn *res
EndProcedure

Procedure QAddReal(r.f, *x.Quaternion, *res.Quaternion = 0)
  If *res = 0: *res.Quaternion = AllocateMemory(SizeOf(Quaternion)): EndIf
  If *res
    *res\a = *x\a + r
    *res\b = *x\b
    *res\c = *x\c
    *res\d = *x\d
  EndIf
  ProcedureReturn *res
EndProcedure

Procedure QAddQuaternion(*x.Quaternion, *y.Quaternion, *res.Quaternion = 0)
  If *res = 0: *res.Quaternion = AllocateMemory(SizeOf(Quaternion)): EndIf
  If *res
    *res\a = *x\a + *y\a
    *res\b = *x\b + *y\b
    *res\c = *x\c + *y\c
    *res\d = *x\d + *y\d
  EndIf
  ProcedureReturn *res
EndProcedure

Procedure QMulReal_and_Quaternion(r.f, *x.Quaternion, *res.Quaternion = 0)
  If *res = 0: *res.Quaternion = AllocateMemory(SizeOf(Quaternion)): EndIf
  If *res
    *res\a = *x\a * r
    *res\b = *x\b * r
    *res\c = *x\c * r
    *res\d = *x\d * r
  EndIf
  ProcedureReturn *res
EndProcedure

Procedure QMulQuaternion(*x.Quaternion, *y.Quaternion, *res.Quaternion = 0)
  If *res = 0: *res.Quaternion = AllocateMemory(SizeOf(Quaternion)): EndIf
  If *res
     *res\a = *x\a * *y\a - *x\b * *y\b - *x\c * *y\c - *x\d * *y\d
     *res\b = *x\a * *y\b + *x\b * *y\a + *x\c * *y\d - *x\d * *y\c
     *res\c = *x\a * *y\c - *x\b * *y\d + *x\c * *y\a + *x\d * *y\b
     *res\d = *x\a * *y\d + *x\b * *y\c - *x\c * *y\b + *x\d * *y\a
  EndIf
  ProcedureReturn *res
EndProcedure

Procedure Q_areEqual(*x.Quaternion, *y.Quaternion)
  If (*x\a <> *y\a) Or (*x\b <> *y\b) Or (*x\c <> *y\c) Or (*x\d <> *y\d)
    ProcedureReturn 0 ;false
  EndIf
  ProcedureReturn 1 ;true
EndProcedure
```

Implementation & test

```PureBasic
Procedure.s ShowQ(*x.Quaternion, NN = 0)
  ProcedureReturn "{" + StrF(*x\a, NN) + "," + StrF(*x\b, NN) + "," + StrF(*x\c, NN) + "," + StrF(*x\d, NN) + "}"
EndProcedure

If OpenConsole()
  Define.Quaternion Q0, Q1, Q2, res, res2
  Define.f          r = 7

  Q0\a = 1: Q0\b = 2: Q0\c = 3: Q0\d = 4
  Q1\a = 2: Q1\b = 3: Q1\c = 4: Q1\d = 5
  Q2\a = 3: Q2\b = 4: Q2\c = 5: Q2\d = 6

  PrintN("Q0 = " + ShowQ(Q0, 0))
  PrintN("Q1 = " + ShowQ(Q1, 0))
  PrintN("Q2 = " + ShowQ(Q2, 0))

  PrintN("Normal of Q0 = " + StrF(QNorm(Q0)))
  PrintN("Neg(Q0)  = " + ShowQ(QNeg(Q0, res)))
  PrintN("Conj(Q0) = " + ShowQ(QConj(Q0, res)))
  PrintN("r + Q0   = " + ShowQ(QAddReal(r, Q0, res)))
  PrintN("Q0 + Q1  = " + ShowQ(QAddQuaternion(Q0, Q1, res)))
  PrintN("Q1 + Q2  = " + ShowQ(QAddQuaternion(Q1, Q2, res)))
  PrintN("Q1 * Q2  = " + ShowQ(QMulQuaternion(Q1, Q2, res)))
  PrintN("Q2 * Q1  = " + ShowQ(QMulQuaternion(Q2, Q1, res2)))
  Print( "Q1 * Q2"): If Q_areEqual(res, res2): Print(" = "): Else: Print(" <> "): EndIf: Print( "Q2 * Q1")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Result

```txt
Q0 = {1,2,3,4}
Q1 = {2,3,4,5}
Q2 = {3,4,5,6}
Normal of Q0 = 5.4772257805
Neg(Q0)  = {-1,-2,-3,-4}
Conj(Q0) = {1,-2,-3,-4}
r + Q0   = {8,2,3,4}
Q0 + Q1  = {3,5,7,9}
Q1 + Q2  = {5,7,9,11}
Q1 * Q2  = {-56,16,24,26}
Q2 * Q1  = {-56,18,20,28}
Q1 * Q2 <> Q2 * Q1
```



## Python

This example extends Pythons [http://docs.python.org/library/collections.html?highlight=namedtuple#collections.namedtuple namedtuples] to add extra functionality.

```python
from collections import namedtuple
import math

class Q(namedtuple('Quaternion', 'real, i, j, k')):
    'Quaternion type: Q(real=0.0, i=0.0, j=0.0, k=0.0)'

    __slots__ = ()

    def __new__(_cls, real=0.0, i=0.0, j=0.0, k=0.0):
        'Defaults all parts of quaternion to zero'
        return super().__new__(_cls, float(real), float(i), float(j), float(k))

    def conjugate(self):
        return Q(self.real, -self.i, -self.j, -self.k)

    def _norm2(self):
        return sum( x*x for x in self)

    def norm(self):
        return math.sqrt(self._norm2())

    def reciprocal(self):
        n2 = self._norm2()
        return Q(*(x / n2 for x in self.conjugate()))

    def __str__(self):
        'Shorter form of Quaternion as string'
        return 'Q(%g, %g, %g, %g)' % self

    def __neg__(self):
        return Q(-self.real, -self.i, -self.j, -self.k)

    def __add__(self, other):
        if type(other) == Q:
            return Q( *(s+o for s,o in zip(self, other)) )
        try:
            f = float(other)
        except:
            return NotImplemented
        return Q(self.real + f, self.i, self.j, self.k)

    def __radd__(self, other):
        return Q.__add__(self, other)

    def __mul__(self, other):
        if type(other) == Q:
            a1,b1,c1,d1 = self
            a2,b2,c2,d2 = other
            return Q(
                 a1*a2 - b1*b2 - c1*c2 - d1*d2,
                 a1*b2 + b1*a2 + c1*d2 - d1*c2,
                 a1*c2 - b1*d2 + c1*a2 + d1*b2,
                 a1*d2 + b1*c2 - c1*b2 + d1*a2 )
        try:
            f = float(other)
        except:
            return NotImplemented
        return Q(self.real * f, self.i * f, self.j * f, self.k * f)

    def __rmul__(self, other):
        return Q.__mul__(self, other)

    def __truediv__(self, other):
        if type(other) == Q:
            return self.__mul__(other.reciprocal())
        try:
            f = float(other)
        except:
            return NotImplemented
        return Q(self.real / f, self.i / f, self.j / f, self.k / f)

    def __rtruediv__(self, other):
        return other * self.reciprocal()

    __div__, __rdiv__ = __truediv__, __rtruediv__

Quaternion = Q

q  = Q(1, 2, 3, 4)
q1 = Q(2, 3, 4, 5)
q2 = Q(3, 4, 5, 6)
r  = 7
```


'''Continued shell session'''
Run the above with the -i flag to python on the command line, or run with idle then continue in the shell as follows:

```python>>>
 q
Quaternion(real=1.0, i=2.0, j=3.0, k=4.0)
>>> q1
Quaternion(real=2.0, i=3.0, j=4.0, k=5.0)
>>> q2
Quaternion(real=3.0, i=4.0, j=5.0, k=6.0)
>>> r
7
>>> q.norm()
5.477225575051661
>>> q1.norm()
7.3484692283495345
>>> q2.norm()
9.273618495495704
>>> -q
Quaternion(real=-1.0, i=-2.0, j=-3.0, k=-4.0)
>>> q.conjugate()
Quaternion(real=1.0, i=-2.0, j=-3.0, k=-4.0)
>>> r + q
Quaternion(real=8.0, i=2.0, j=3.0, k=4.0)
>>> q + r
Quaternion(real=8.0, i=2.0, j=3.0, k=4.0)
>>> q1 + q2
Quaternion(real=5.0, i=7.0, j=9.0, k=11.0)
>>> q2 + q1
Quaternion(real=5.0, i=7.0, j=9.0, k=11.0)
>>> q * r
Quaternion(real=7.0, i=14.0, j=21.0, k=28.0)
>>> r * q
Quaternion(real=7.0, i=14.0, j=21.0, k=28.0)
>>> q1 * q2
Quaternion(real=-56.0, i=16.0, j=24.0, k=26.0)
>>> q2 * q1
Quaternion(real=-56.0, i=18.0, j=20.0, k=28.0)
>>> assert q1 * q2 != q2 * q1
>>>
>>> i, j, k = Q(0,1,0,0), Q(0,0,1,0), Q(0,0,0,1)
>>> i*i
Quaternion(real=-1.0, i=0.0, j=0.0, k=0.0)
>>> j*j
Quaternion(real=-1.0, i=0.0, j=0.0, k=0.0)
>>> k*k
Quaternion(real=-1.0, i=0.0, j=0.0, k=0.0)
>>> i*j*k
Quaternion(real=-1.0, i=0.0, j=0.0, k=0.0)
>>> q1 / q2
Quaternion(real=0.7906976744186047, i=0.023255813953488358, j=-2.7755575615628914e-17, k=0.046511627906976744)
>>> q1 / q2 * q2
Quaternion(real=2.0000000000000004, i=3.0000000000000004, j=4.000000000000001, k=5.000000000000001)
>>> q2 * q1 / q2
Quaternion(real=2.0, i=3.465116279069768, j=3.906976744186047, k=4.767441860465116)
>>> q1.reciprocal() * q1
Quaternion(real=0.9999999999999999, i=0.0, j=0.0, k=0.0)
>>> q1 * q1.reciprocal()
Quaternion(real=0.9999999999999999, i=0.0, j=0.0, k=0.0)
>>>
```





## R


Using the quaternions package.


```R

library(quaternions)

q <- Q(1, 2, 3, 4)
q1 <- Q(2, 3, 4, 5)
q2 <- Q(3, 4, 5, 6)
r <- 7.0

display <- function(x){
  e <- deparse(substitute(x))
  res <- if(class(x) == "Q") paste(x$r, "+", x$i, "i+", x$j, "j+", x$k, "k", sep = "") else x
  cat(noquote(paste(c(e, " = ", res, "\n"), collapse="")))
  invisible(res)
}

display(norm(q))
display(-q)
display(Conj(q))
display(r + q)
display(q1 + q2)
display(r*q)
display(q*r)
if(display(q1*q2) == display(q2*q1)) cat("q1*q2 == q2*q1\n") else cat("q1*q2 != q2*q1\n")

## norm(q) = 5.47722557505166
## -q = -1+-2i+-3j+-4k
## Conj(q) = 1+-2i+-3j+-4k
## r + q = 8+2i+3j+4k
## q1 + q2 = 5+7i+9j+11k
## r * q = 7+14i+21j+28k
## q * r = 7+14i+21j+28k
## q1 * q2 = -56+16i+24j+26k
## q2 * q1 = -56+18i+20j+28k
## q1*q2 != q2*q1


```



## Racket


```Racket
#lang racket

(struct quaternion (a b c d)
  #:transparent)

(define-match-expander quaternion:
  (λ (stx)
    (syntax-case stx ()
      [(_ a b c d)
       #'(or (quaternion a b c d)
             (and a (app (λ(_) 0) b) (app (λ(_) 0) c) (app (λ(_) 0) d)))])))

(define (norm q)
  (match q
    [(quaternion: a b c d)
     (sqrt (+ (sqr a) (sqr b) (sqr c) (sqr d)))]))

(define (negate q)
  (match q
    [(quaternion: a b c d)
     (quaternion (- a) (- b) (- c) (- d))]))

(define (conjugate q)
  (match q
    [(quaternion: a b c d)
     (quaternion a (- b) (- c) (- d))]))

(define (add q1 q2 . q-rest)
  (let ((ans (match* (q1 q2)
               [((quaternion: a1 b1 c1 d1) (quaternion: a2 b2 c2 d2))
                (quaternion (+ a1 a2) (+ b1 b2) (+ c1 c2) (+ d1 d2))])))
    (if (empty? q-rest)
        ans
        (apply add (cons ans q-rest)))))

(define (multiply q1 q2 . q-rest)
  (let ((ans (match* (q1 q2)
               [((quaternion: a1 b1 c1 d1) (quaternion: a2 b2 c2 d2))
                (quaternion (- (* a1 a2) (* b1 b2) (* c1 c2) (* d1 d2))
                            (+ (* a1 b2) (* b1 a2) (* c1 d2) (- (* d1 c2)))
                            (+ (* a1 c2) (- (* b1 d2)) (* c1 a2) (* d1 b2))
                            (+ (* a1 d2) (* b1 c2) (- (* c1 b2)) (* d1 a2)))])))
    (if (empty? q-rest)
        ans
        (apply multiply (cons ans q-rest)))))

;; Tests
(module+ main
  (define i (quaternion 0 1 0 0))
  (define j (quaternion 0 0 1 0))
  (define k (quaternion 0 0 0 1))
  (displayln (multiply i j k))
  (newline)

  (define q (quaternion 1 2 3 4))
  (define q1 (quaternion 2 3 4 5))
  (define q2 (quaternion 3 4 5 6))
  (define r 7)

  (for ([quat (list q q1 q2)])
    (displayln quat)
    (displayln (norm quat))
    (displayln (negate quat))
    (displayln (conjugate quat))
    (newline))

  (add r q)
  (add q1 q2)
  (multiply r q)

  (newline)
  (multiply q1 q2)
  (multiply q2 q1)
  (equal? (multiply q1 q2)
          (multiply q2 q1)))
```


{{out}}

```txt

#(struct:quaternion -1 0 0 0)

#(struct:quaternion 1 2 3 4)
5.477225575051661
#(struct:quaternion -1 -2 -3 -4)
#(struct:quaternion 1 -2 -3 -4)

#(struct:quaternion 2 3 4 5)
7.3484692283495345
#(struct:quaternion -2 -3 -4 -5)
#(struct:quaternion 2 -3 -4 -5)

#(struct:quaternion 3 4 5 6)
9.273618495495704
#(struct:quaternion -3 -4 -5 -6)
#(struct:quaternion 3 -4 -5 -6)

(quaternion 8 2 3 4)
(quaternion 5 7 9 11)
(quaternion 7 14 21 28)

(quaternion -56 16 24 26)
(quaternion -56 18 20 28)
#f

```



## Red


```Red

quaternion: context [
   quaternion!: make typeset! [block! hash! vector!]
   multiply: function [q [integer! float! quaternion!] p [integer! float! quaternion!]][
      case [
         number? q [collect [forall p [keep p/1 * q]]]
         number? p [collect [forall q [keep q/1 * p]]]
         'else [
            reduce [
               (q/1 * p/1) - (q/2 * p/2) - (q/3 * p/3) - (q/4 * p/4)
               (q/1 * p/2) + (q/2 * p/1) + (q/3 * p/4) - (q/4 * p/3)
               (q/1 * p/3) + (q/3 * p/1) + (q/4 * p/2) - (q/2 * p/4)
               (q/1 * p/4) + (q/4 * p/1) + (q/2 * p/3) - (q/3 * p/2)
            ]
         ]
      ]
   ]
   add: func [q [integer! float! quaternion!] p [integer! float! quaternion!]][
      case [
         number? q [head change copy p p/1 + q]
         number? p [head change copy q q/1 + p]
         'else [collect [forall q [keep q/1 + p/(index? q)]]]
      ]
   ]
   negate: func [q [quaternion!]][collect [forall q [keep 0 - q/1]]]
   conjugate: func [q [quaternion!]][collect [keep q/1  q: next q  forall q [keep 0 - q/1]]]
   norm: func [q [quaternion!]][sqrt first multiply q conjugate copy q]
   normalize: function [q [quaternion!]][n: norm q   collect [forall q [keep q/1 / n]]]
   inverse: func [q [quaternion!]][(conjugate q) / ((norm q) ** 2)]
]

set [q q1 q2 r] [[1 2 3 4] [2 3 4 5] [3 4 5 6] 7]

print [{
1. The norm of a quaternion:
`quaternion/norm q` =>} quaternion/norm q {

2. The negative of a quaternion:
`quaternion/negate q` =>} mold quaternion/negate q {

3. The conjugate of a quaternion:
<code>quaternion/conjugate q</code> =>} mold quaternion/conjugate q {

4. Addition of a real number `r` and a quaternion `q`:
`quaternion/add r q` =>} mold quaternion/add r q {
`quaternion/add q r` =>} mold quaternion/add q r {

5. Addition of two quaternions:
`quaternion/add q1 q2` =>} mold quaternion/add q1 q2 {

6. Multiplication of a real number and a quaternion:
`quaternion/multiply q r` =>} mold quaternion/multiply q r {
`quaternion/multiply r q` =>} mold quaternion/multiply r q {

7. Multiplication of two quaternions `q1` and `q2` is given by:
`quaternion/multiply q1 q2` =>} mold quaternion/multiply q1 q2 {

8. Show that, for the two quaternions `q1` and `q2`:
`equal? quaternion/multiply q1 q2 mold quaternion/multiply q2 q1` =>}
equal? quaternion/multiply q1 q2 quaternion/multiply q2 q1]

```


Output:

1. The norm of a quaternion:

<code>quaternion/norm q</code> => <code>5.477225575051661</code>

2. The negative of a quaternion:

<code>quaternion/negate q</code> => <code>[-1 -2 -3 -4]</code>

3. The conjugate of a quaternion:

<code>quaternion/conjugate q</code> => <code>[1 -2 -3 -4]</code>

4. Addition of a real number <code>r</code> and a quaternion <code>q</code>:

<code>quaternion/add r q</code> => <code>[8 2 3 4]</code>

<code>quaternion/add q r</code> => <code>[8 2 3 4]</code>

5. Addition of two quaternions:

<code>quaternion/add q1 q2</code> => <code>[5 7 9 11]</code>

6. Multiplication of a real number and a quaternion:

<code>quaternion/multiply q r</code> => <code>[7 14 21 28]</code>

<code>quaternion/multiply r q</code> => <code>[7 14 21 28]</code>

7. Multiplication of two quaternions <code>q1</code> and <code>q2</code> is given by:

<code>quaternion/multiply q1 q2</code> => <code>[-56 16 24 26]</code>

8. Show that, for the two quaternions <code>q1</code> and <code>q2</code>:

<code>equal? quaternion/multiply q1 q2 mold quaternion/multiply q2 q1</code> => <code>false</code>


## REXX

The REXX language has no native quaternion support, but subroutines can be easily written.

```rexx
/*REXX program performs some operations on  quaternion type numbers and displays results*/
                       q = 1 2 3 4    ;                     q1 = 2 3 4 5
                       r = 7          ;                     q2 = 3 4 5 6
call qShow  q                         ,     'q'
call qShow  q1                        ,     'q1'
call qShow  q2                        ,     'q2'
call qShow  r                         ,     'r'
call qShow  qNorm(q)                  ,     'norm q'                  ,        "task 1:"
call qShow  qNeg(q)                   ,     'negative q'              ,        "task 2:"
call qShow  qConj(q)                  ,     'conjugate q'             ,        "task 3:"
call qShow  qAdd( r, q  )             ,     'addition r+q'            ,        "task 4:"
call qShow  qAdd(q1, q2 )             ,     'addition q1+q2'          ,        "task 5:"
call qShow  qMul( q, r  )             ,     'multiplication q*r'      ,        "task 6:"
call qShow  qMul(q1, q2 )             ,     'multiplication q1*q2'    ,        "task 7:"
call qShow  qMul(q2, q1 )             ,     'multiplication q2*q1'    ,        "task 8:"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
qConj: procedure; parse arg x;    call qXY;    return    x.1    (-x.2)    (-x.3)    (-x.4)
qNeg:  procedure; parse arg x;    call qXY;    return   -x.1    (-x.2)    (-x.3)    (-x.4)
qNorm: procedure; parse arg x;    call qXY;    return sqrt(x.1**2 +x.2**2 +x.3**2 +x.4**2)
qAdd:  procedure; parse arg x,y;  call qXY 2;  return x.1+y.1  x.2+y.2  x.3+y.3  x.4+y.4
/*──────────────────────────────────────────────────────────────────────────────────────*/
qMul:  procedure; parse arg x,y;  call qXY y
       return x.1*y.1 -x.2*y.2 -x.3*y.3 -x.4*y.4     x.1*y.2 +x.2*y.1 +x.3*y.4 -x.4*y.3 ,
              x.1*y.3 -x.2*y.4 +x.3*y.1 +x.4*y.2     x.1*y.4 +x.2*y.3 -x.3*y.2 +x.4*y.1
/*──────────────────────────────────────────────────────────────────────────────────────*/
qShow: procedure; parse arg x;          call qXY;   $=
             do m=1  for 4;  _=x.m;     if _==0  then iterate;        if _>=0  then _="+"_
             if m\==1  then _= _ || substr('~ijk', m, 1);             $=strip($ || _,,"+")
             end   /*m*/
       say left(arg(3), 9)   right(arg(2), 20)         ' ──► '        $
       return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
qXY:                      do n=1  for 4;   x.n= word( word(x, n)  0, 1) / 1;    end /*n*/
       if arg()==1  then  do m=1  for 4;   y.m= word( word(y, m)  0, 1) / 1;    end /*m*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x;  if x=0  then return 0;   d= digits();   i=;   m.=9;  h=d+6
       numeric digits; numeric form;         if x<0  then  do;  x= -x;    i= 'i';      end
       parse value format(x, 2, 1, , 0) 'E0'   with   g  'E'  _  .;       g= g *.5'e'_ % 2
          do j=0  while h>9;       m.j=h;                 h= h % 2   +  1;      end  /*j*/
          do k=j+5  to 0  by -1;   numeric digits m.k;    g= (g + x/g)* .5;     end  /*k*/
       numeric digits d;           return (g/1)i                /*make complex if  X<0. */
```

'''output'''   when using the default input:

```txt

                             q  ──►  1+2i+3j+4k
                            q1  ──►  2+3i+4j+5k
                            q2  ──►  3+4i+5j+6k
                             r  ──►  7
task 1:                 norm q  ──►  5.47722558
task 2:             negative q  ──►  -1-2i-3j-4k
task 3:            conjugate q  ──►  1-2i-3j-4k
task 4:           addition r+q  ──►  8+2i+3j+4k
task 5:         addition q1+q2  ──►  5+7i+9j+11k
task 6:     multiplication q*r  ──►  7+14i+21j+28k
task 7:   multiplication q1*q2  ──►  -56+16i+24j+26k
task 8:   multiplication q2*q1  ──►  -56+18i+20j+28k

```



## Ruby

{{works with|Ruby|1.9}}


```ruby
class Quaternion
  def initialize(*parts)
    raise ArgumentError, "wrong number of arguments (#{parts.size} for 4)" unless parts.size == 4
    raise ArgumentError, "invalid value of quaternion parts #{parts}" unless parts.all? {|x| x.is_a?(Numeric)}
    @parts = parts
  end

  def to_a;          @parts;                                       end
  def to_s;          "Quaternion#{@parts.to_s}"                    end
  alias inspect to_s
  def complex_parts; [Complex(*to_a[0..1]), Complex(*to_a[2..3])]; end

  def real;          @parts.first;                                 end
  def imag;          @parts[1..3];                                 end
  def conj;          Quaternion.new(real, *imag.map(&:-@));        end
  def norm;          Math.sqrt(to_a.reduce(0){|sum,e| sum + e**2}) end # In Rails: Math.sqrt(to_a.sum { e**2 })

  def ==(other)
    case other
    when Quaternion; to_a == other.to_a
    when Numeric;    to_a == [other, 0, 0, 0]
    else             false
    end
  end
  def -@;            Quaternion.new(*to_a.map(&:-@));              end
  def -(other);      self + -other;                                end

  def +(other)
    case other
    when Numeric
      Quaternion.new(real + other, *imag)
    when Quaternion
      Quaternion.new(*to_a.zip(other.to_a).map { |x,y| x + y }) # In Rails: zip(other).map(&:sum)
    end
  end

  def *(other)
    case other
    when Numeric
      Quaternion.new(*to_a.map { |x| x * other })
    when Quaternion
      # Multiplication of quaternions in C x C space. See "Cayley-Dickson construction".
      a, b, c, d = *complex_parts, *other.complex_parts
      x, y = a*c - d.conj*b, a*d + b*c.conj
      Quaternion.new(x.real, x.imag, y.real, y.imag)
    end
  end

  # Coerce is called by Ruby to return a compatible type/receiver when the called method/operation does not accept a Quaternion
  def coerce(other)
    case other
    when Numeric then [Scalar.new(other), self]
    else raise TypeError, "#{other.class} can't be coerced into #{self.class}"
    end
  end

  class Scalar
    def initialize(val); @val = val;                            end
    def +(other);        other + @val;                          end
    def *(other);        other * @val;                          end
    def -(other);        Quaternion.new(@val, 0, 0, 0) - other; end
  end
end

if __FILE__ == $0
  q  = Quaternion.new(1,2,3,4)
  q1 = Quaternion.new(2,3,4,5)
  q2 = Quaternion.new(3,4,5,6)
  r  = 7
  expressions = ["q", "q1", "q2",
                 "q.norm", "-q", "q.conj", "q + r", "r + q","q1 + q2", "q2 + q1",
                 "q * r", "r * q", "q1 * q2", "q2 * q1", "(q1 * q2 != q2 * q1)",
                 "q - r", "r - q"]
  expressions.each do |exp|
    puts "%20s = %s" % [exp, eval(exp)]
  end
end
```

{{out}}

```txt

                   q = Quaternion[1, 2, 3, 4]
                  q1 = Quaternion[2, 3, 4, 5]
                  q2 = Quaternion[3, 4, 5, 6]
              q.norm = 5.477225575051661
                  -q = Quaternion[-1, -2, -3, -4]
              q.conj = Quaternion[1, -2, -3, -4]
               q + r = Quaternion[8, 2, 3, 4]
               r + q = Quaternion[8, 2, 3, 4]
             q1 + q2 = Quaternion[5, 7, 9, 11]
             q2 + q1 = Quaternion[5, 7, 9, 11]
               q * r = Quaternion[7, 14, 21, 28]
               r * q = Quaternion[7, 14, 21, 28]
             q1 * q2 = Quaternion[-56, 16, 24, 26]
             q2 * q1 = Quaternion[-56, 18, 20, 28]
(q1 * q2 != q2 * q1) = true
               q - r = Quaternion[-6, 2, 3, 4]
               r - q = Quaternion[6, -2, -3, -4]

```



## Rust



```rust
use std::fmt::{Display, Error, Formatter};
use std::ops::{Add, Mul, Neg};

#[derive(Clone,Copy,Debug)]
struct Quaternion {
    a: f64,
    b: f64,
    c: f64,
    d: f64
}

impl Quaternion {
    pub fn new(a: f64, b: f64, c: f64, d: f64) -> Quaternion {
        Quaternion {
            a: a,
            b: b,
            c: c,
            d: d
        }
    }

    pub fn norm(&self) -> f64 {
        (self.a.powi(2) + self.b.powi(2) + self.c.powi(2) + self.d.powi(2)).sqrt()
    }

    pub fn conjugate(&self) -> Quaternion {
        Quaternion {
            a: self.a,
            b: -self.b,
            c: -self.c,
            d: -self.d
        }
    }
}

impl Add for Quaternion {
    type Output = Quaternion;

    #[inline]
    fn add(self, other: Quaternion) -> Self::Output {
        Quaternion {
            a: self.a + other.a,
            b: self.b + other.b,
            c: self.c + other.c,
            d: self.d + other.d
        }
    }
}

impl Add<f64> for Quaternion {
    type Output = Quaternion;

    #[inline]
    fn add(self, other: f64) -> Self::Output {
        Quaternion {
            a: self.a + other,
            b: self.b,
            c: self.c,
            d: self.d
        }
    }
}

impl Add<Quaternion> for f64 {
    type Output = Quaternion;

    #[inline]
    fn add(self, other: Quaternion) -> Self::Output {
        Quaternion {
            a: other.a + self,
            b: other.b,
            c: other.c,
            d: other.d
        }
    }
}

impl Display for Quaternion {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "({} + {}i + {}j + {}k)", self.a, self.b, self.c, self.d)
    }
}

impl Mul for Quaternion {
    type Output = Quaternion;

    #[inline]
    fn mul(self, rhs: Quaternion) -> Self::Output {
        Quaternion {
            a: self.a * rhs.a - self.b * rhs.b - self.c * rhs.c - self.d * rhs.d,
            b: self.a * rhs.b + self.b * rhs.a + self.c * rhs.d - self.d * rhs.c,
            c: self.a * rhs.c - self.b * rhs.d + self.c * rhs.a + self.d * rhs.b,
            d: self.a * rhs.d + self.b * rhs.c - self.c * rhs.b + self.d * rhs.a,
        }
    }
}

impl Mul<f64> for Quaternion {
    type Output = Quaternion;

    #[inline]
    fn mul(self, other: f64) -> Self::Output {
        Quaternion {
            a: self.a * other,
            b: self.b * other,
            c: self.c * other,
            d: self.d * other
        }
    }
}

impl Mul<Quaternion> for f64 {
    type Output = Quaternion;

    #[inline]
    fn mul(self, other: Quaternion) -> Self::Output {
        Quaternion {
            a: other.a * self,
            b: other.b * self,
            c: other.c * self,
            d: other.d * self
        }
    }
}

impl Neg for Quaternion {
    type Output = Quaternion;

    #[inline]
    fn neg(self) -> Self::Output {
        Quaternion {
            a: -self.a,
            b: -self.b,
            c: -self.c,
            d: -self.d
        }
    }
}

fn main() {
    let q0 = Quaternion { a: 1., b: 2., c: 3., d: 4. };
    let q1 = Quaternion::new(2., 3., 4., 5.);
    let q2 = Quaternion::new(3., 4., 5., 6.);
    let r: f64 = 7.;

    println!("q0 = {}", q0);
    println!("q1 = {}", q1);
    println!("q2 = {}", q2);
    println!("r  = {}", r);
    println!();
    println!("-q0 = {}", -q0);
    println!("conjugate of q0 = {}", q0.conjugate());
    println!();
    println!("r + q0 = {}", r + q0);
    println!("q0 + r = {}", q0 + r);
    println!();
    println!("r * q0 = {}", r * q0);
    println!("q0 * r = {}", q0 * r);
    println!();
    println!("q0 + q1 = {}", q0 + q1);
    println!("q0 * q1 = {}", q0 * q1);
    println!();
    println!("q0 * (conjugate of q0) = {}", q0 * q0.conjugate());
    println!();
    println!(" q0 + q1  * q2 = {}", q0 + q1 * q2);
    println!("(q0 + q1) * q2 = {}", (q0 + q1) * q2);
    println!();
    println!(" q0 *  q1  * q2  = {}", q0 *q1 * q2);
    println!("(q0 *  q1) * q2  = {}", (q0 * q1) * q2);
    println!(" q0 * (q1  * q2) = {}", q0 * (q1 * q2));
    println!();
    println!("normal of q0 = {}", q0.norm());
}
```

{{out}}

```txt

q0 = (1 + 2i + 3j + 4k)
q1 = (2 + 3i + 4j + 5k)
q2 = (3 + 4i + 5j + 6k)
r  = 7

-q0 = (-1 + -2i + -3j + -4k)
conjugate of q0 = (1 + -2i + -3j + -4k)

r + q0 = (8 + 2i + 3j + 4k)
q0 + r = (8 + 2i + 3j + 4k)

r * q0 = (7 + 14i + 21j + 28k)
q0 * r = (7 + 14i + 21j + 28k)

q0 + q1 = (3 + 5i + 7j + 9k)
q0 * q1 = (-36 + 6i + 12j + 12k)

q0 * (conjugate of q0) = (30 + 0i + 0j + 0k)

 q0 + q1  * q2 = (-55 + 18i + 27j + 30k)
(q0 + q1) * q2 = (-100 + 24i + 42j + 42k)

 q0 *  q1  * q2  = (-264 + -114i + -132j + -198k)
(q0 *  q1) * q2  = (-264 + -114i + -132j + -198k)
 q0 * (q1  * q2) = (-264 + -114i + -132j + -198k)

normal of q0 = 5.477225575051661

```



## Scala


```scala
case class Quaternion(re: Double = 0.0, i: Double = 0.0, j: Double = 0.0, k: Double = 0.0) {
  lazy val im = (i, j, k)
  private lazy val norm2 = re*re + i*i + j*j + k*k
  lazy val norm = math.sqrt(norm2)

  def negative = Quaternion(-re, -i, -j, -k)
  def conjugate = Quaternion(re, -i, -j, -k)
  def reciprocal = Quaternion(re/norm2, -i/norm2, -j/norm2, -k/norm2)

  def +(q: Quaternion) = Quaternion(re+q.re, i+q.i, j+q.j, k+q.k)
  def -(q: Quaternion) = Quaternion(re-q.re, i-q.i, j-q.j, k-q.k)
  def *(q: Quaternion) = Quaternion(
	 re*q.re - i*q.i - j*q.j - k*q.k,
	 re*q.i + i*q.re + j*q.k - k*q.j,
	 re*q.j - i*q.k + j*q.re + k*q.i,
	 re*q.k + i*q.j - j*q.i + k*q.re
  )
  def /(q: Quaternion) = this * q.reciprocal

  def unary_- = negative
  def unary_~ = conjugate

  override def toString = "Q(%.2f, %.2fi, %.2fj, %.2fk)".formatLocal(java.util.Locale.ENGLISH, re, i, j, k)
}

object Quaternion {
  import scala.language.implicitConversions
  import Numeric.Implicits._

  implicit def number2Quaternion[T:Numeric](n: T) = Quaternion(n.toDouble)
}
```

Demonstration:

```scala
val q0=Quaternion(1.0, 2.0, 3.0, 4.0);
val q1=Quaternion(2.0, 3.0, 4.0, 5.0);
val q2=Quaternion(3.0, 4.0, 5.0, 6.0);
val r=7;

println("q0 = "+ q0)
println("q1 = "+ q1)
println("q2 = "+ q2)
println("r  = "+ r)
println()

println("q0.re            = "+ q0.re)
println("q0.im            = "+ q0.im)
println("q0.norm          = "+ q0.norm)
println("q0.negative      = "+ q0.negative)
println("-q0              = "+ -q0)
println("q0.conjugate     = "+ q0.conjugate)
println("~q0              = "+ ~q0)
println("q1+q2            = "+ (q1+q2))
println("q2+q1            = "+ (q2+q1))
println("q1+r             = "+ (q1+r))
println("r+q1             = "+ (r+q1))
println("q1-q2            = "+ (q1-q2))
println("q2-q1            = "+ (q2-q1))
println("q1-r             = "+ (q1-r))
println("r-q1             = "+ (r-q1))
println("q1*q2            = "+ q1*q2)
println("q2*q1            = "+ q2*q1)
println("q1*r             = "+ q1*r)
println("r*q1             = "+ r*q1)
println("(q1*q2)!=(q2*q1) = "+ ((q1*q2)!=(q2*q1)))
println("q1/q2            = "+ q1/q2)
println("q2/q1            = "+ q2/q1)
println("q1/r             = "+ q1/r)
println("r/q1             = "+ r/q1)
```

{{out}}

```txt
q0 = Q(1.00, 2.00i, 3.00j, 4.00k)
q1 = Q(2.00, 3.00i, 4.00j, 5.00k)
q2 = Q(3.00, 4.00i, 5.00j, 6.00k)
r  = 7

q0.re            = 1.0
q0.im            = (2.0,3.0,4.0)
q0.norm          = 5.477225575051661
q0.negative      = Q(-1.00, -2.00i, -3.00j, -4.00k)
-q0              = Q(-1.00, -2.00i, -3.00j, -4.00k)
q0.conjugate     = Q(1.00, -2.00i, -3.00j, -4.00k)
~q0              = Q(1.00, -2.00i, -3.00j, -4.00k)
q1+q2            = Q(5.00, 7.00i, 9.00j, 11.00k)
q2+q1            = Q(5.00, 7.00i, 9.00j, 11.00k)
q1+r             = Q(9.00, 3.00i, 4.00j, 5.00k)
r+q1             = Q(9.00, 3.00i, 4.00j, 5.00k)
q1-q2            = Q(-1.00, -1.00i, -1.00j, -1.00k)
q2-q1            = Q(1.00, 1.00i, 1.00j, 1.00k)
q1-r             = Q(-5.00, 3.00i, 4.00j, 5.00k)
r-q1             = Q(5.00, -3.00i, -4.00j, -5.00k)
q1*q2            = Q(-56.00, 16.00i, 24.00j, 26.00k)
q2*q1            = Q(-56.00, 18.00i, 20.00j, 28.00k)
q1*r             = Q(14.00, 21.00i, 28.00j, 35.00k)
r*q1             = Q(14.00, 21.00i, 28.00j, 35.00k)
(q1*q2)!=(q2*q1) = true
q1/q2            = Q(0.79, 0.02i, -0.00j, 0.05k)
q2/q1            = Q(1.26, -0.04i, 0.00j, -0.07k)
q1/r             = Q(0.29, 0.43i, 0.57j, 0.71k)
r/q1             = Q(0.26, -0.39i, -0.52j, -0.65k)
```



## Sidef

{{trans|Perl 6}}

```ruby
class Quaternion(r, i, j, k) {

    func qu(*r) { Quaternion(r...) }

    method to_s  { "#{r} + #{i}i + #{j}j + #{k}k" }
    method reals { [r, i, j, k] }
    method conj  { qu(r, -i, -j, -k) }
    method norm  { self.reals.map { _*_ }.sum.sqrt }

    method ==(Quaternion b) { self.reals == b.reals }

    method +(Number     b) { qu(b+r, i, j, k) }
    method +(Quaternion b) { qu((self.reals ~Z+ b.reals)...) }

    method neg { qu(self.reals.map{ .neg }...) }

    method *(Number     b) { qu((self.reals»*»b)...) }
    method *(Quaternion b) {
        var (r,i,j,k) = b.reals...
        qu(sum(self.reals ~Z* [r, -i, -j, -k]),
           sum(self.reals ~Z* [i,  r,  k, -j]),
           sum(self.reals ~Z* [j, -k,  r,  i]),
           sum(self.reals ~Z* [k,  j, -i,  r]))
    }
}

var q  = Quaternion(1, 2, 3, 4)
var q1 = Quaternion(2, 3, 4, 5)
var q2 = Quaternion(3, 4, 5, 6)
var r  = 7

say "1) q norm  = #{q.norm}"
say "2) -q      = #{-q}"
say "3) q conj  = #{q.conj}"
say "4) q  + r  = #{q + r}"
say "5) q1 + q2 = #{q1 + q2}"
say "6) q  * r  = #{q  * r}"
say "7) q1 * q2 = #{q1 * q2}"
say "8) q1q2 #{ q1*q2 == q2*q1 ? '==' : '!=' } q2q1"
```

{{out}}

```txt

1) q norm  = 5.47722557505166113456969782800802133952744694997983
2) -q      = -1 + -2i + -3j + -4k
3) q conj  = 1 + -2i + -3j + -4k
4) q  + r  = 8 + 2i + 3j + 4k
5) q1 + q2 = 5 + 7i + 9j + 11k
6) q  * r  = 7 + 14i + 21j + 28k
7) q1 * q2 = -56 + 16i + 24j + 26k
8) q1q2 != q2q1

```



## Swift


```swift
import Foundation

struct Quaternion {
  var a, b, c, d: Double

  static let i = Quaternion(a: 0, b: 1, c: 0, d: 0)
  static let j = Quaternion(a: 0, b: 0, c: 1, d: 0)
  static let k = Quaternion(a: 0, b: 0, c: 0, d: 1)
}
extension Quaternion: Equatable {
  static func ==(lhs: Quaternion, rhs: Quaternion) -> Bool {
    return (lhs.a, lhs.b, lhs.c, lhs.d) == (rhs.a, rhs.b, rhs.c, rhs.d)
  }
}
extension Quaternion: ExpressibleByIntegerLiteral {
  init(integerLiteral: Double) {
    a = integerLiteral
    b = 0
    c = 0
    d = 0
  }
}
extension Quaternion: Numeric {
  var magnitude: Double {
    return norm
  }
  init?<T>(exactly: T) { // stub to satisfy protocol requirements
    return nil
  }
  public static func + (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
    return Quaternion(
      a: lhs.a + rhs.a,
      b: lhs.b + rhs.b,
      c: lhs.c + rhs.c,
      d: lhs.d + rhs.d
    )
  }
  public static func - (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
    return Quaternion(
      a: lhs.a - rhs.a,
      b: lhs.b - rhs.b,
      c: lhs.c - rhs.c,
      d: lhs.d - rhs.d
    )
  }
  public static func * (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
    return Quaternion(
      a: lhs.a*rhs.a - lhs.b*rhs.b - lhs.c*rhs.c - lhs.d*rhs.d,
      b: lhs.a*rhs.b + lhs.b*rhs.a + lhs.c*rhs.d - lhs.d*rhs.c,
      c: lhs.a*rhs.c - lhs.b*rhs.d + lhs.c*rhs.a + lhs.d*rhs.b,
      d: lhs.a*rhs.d + lhs.b*rhs.c - lhs.c*rhs.b + lhs.d*rhs.a
    )
  }
  public static func += (lhs: inout Quaternion, rhs: Quaternion) {
    lhs = Quaternion(
      a: lhs.a + rhs.a,
      b: lhs.b + rhs.b,
      c: lhs.c + rhs.c,
      d: lhs.d + rhs.d
    )
  }
  public static func -= (lhs: inout Quaternion, rhs: Quaternion) {
    lhs = Quaternion(
      a: lhs.a - rhs.a,
      b: lhs.b - rhs.b,
      c: lhs.c - rhs.c,
      d: lhs.d - rhs.d
    )
  }
  public static func *= (lhs: inout Quaternion, rhs: Quaternion) {
    lhs = Quaternion(
      a: lhs.a*rhs.a - lhs.b*rhs.b - lhs.c*rhs.c - lhs.d*rhs.d,
      b: lhs.a*rhs.b + lhs.b*rhs.a + lhs.c*rhs.d - lhs.d*rhs.c,
      c: lhs.a*rhs.c - lhs.b*rhs.d + lhs.c*rhs.a + lhs.d*rhs.b,
      d: lhs.a*rhs.d + lhs.b*rhs.c - lhs.c*rhs.b + lhs.d*rhs.a
    )
  }
}
extension Quaternion: CustomStringConvertible {
  var description: String {
    let formatter = NumberFormatter()
    formatter.positivePrefix = "+"
    let f: (Double) -> String = { formatter.string(from: $0 as NSNumber)! }
    return [f(a), f(b), "i", f(c), "j", f(d), "k"].joined()
  }
}
extension Quaternion {
  var norm: Double {
    return sqrt(a*a + b*b + c*c + d*d)
  }
  var conjugate: Quaternion {
    return Quaternion(a: a, b: -b, c: -c, d: -d)
  }
  public static func + (lhs: Double, rhs: Quaternion) -> Quaternion {
    var result = rhs
    result.a += lhs
    return result
  }
  public static func + (lhs: Quaternion, rhs: Double) -> Quaternion {
    var result = lhs
    result.a += rhs
    return result
  }
  public static func * (lhs: Double, rhs: Quaternion) -> Quaternion {
    return Quaternion(a: lhs*rhs.a, b: lhs*rhs.b, c: lhs*rhs.c, d: lhs*rhs.d)
  }
  public static func * (lhs: Quaternion, rhs: Double) -> Quaternion {
    return Quaternion(a: lhs.a*rhs, b: lhs.b*rhs, c: lhs.c*rhs, d: lhs.d*rhs)
  }
  public static prefix func - (x: Quaternion) -> Quaternion {
    return Quaternion(a: -x.a, b: -x.b, c: -x.c, d: -x.d)
  }
}

let q:  Quaternion = 1 + 2 * .i + 3 * .j + 4 * .k // 1+2i+3j+4k
let q1: Quaternion = 2 + 3 * .i + 4 * .j + 5 * .k // 2+3i+4j+5k
let q2: Quaternion = 3 + 4 * .i + 5 * .j + 6 * .k // 3+4i+5j+6k
let r: Double = 7

print("""
  q  = \(q)
  q1 = \(q1)
  q2 = \(q2)
  r = \(r)
  -q = \(-q)
  ‖q‖ = \(q.norm)
  conjugate of q = \(q.conjugate)
  r + q = q + r = \(r+q) = \(q+r)
  q₁ + q₂ = \(q1 + q2) = \(q2 + q1)
  qr = rq = \(q*r) = \(r*q)
  q₁q₂ = \(q1 * q2)
  q₂q₁ = \(q2 * q1)
  q₁q₂ ≠ q₂q₁ is \(q1*q2 != q2*q1)
""")
```


{{out}}

```txt

  q  = +1+2i+3j+4k
  q1 = +2+3i+4j+5k
  q2 = +3+4i+5j+6k
  r = 7.0
  -q = -1-2i-3j-4k
  ‖q‖ = 5.477225575051661
  conjugate of q = +1-2i-3j-4k
  r + q = q + r = +8+2i+3j+4k = +8+2i+3j+4k
  q₁ + q₂ = +5+7i+9j+11k = +5+7i+9j+11k
  qr = rq = +7+14i+21j+28k = +7+14i+21j+28k
  q₁q₂ = -56+16i+24j+26k
  q₂q₁ = -56+18i+20j+28k
  q₁q₂ ≠ q₂q₁ is true

```



## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
package require TclOO

# Support class that provides C++-like RAII lifetimes
oo::class create RAII-support {
    constructor {} {
	upvar 1 { end } end
	lappend end [self]
	trace add variable end unset [namespace code {my destroy}]
    }
    destructor {
	catch {
	    upvar 1 { end } end
	    trace remove variable end unset [namespace code {my destroy}]
	}
    }
    method return {{level 1}} {
	incr level
	upvar 1 { end } end
	upvar $level { end } parent
	trace remove variable end unset [namespace code {my destroy}]
	lappend parent [self]
	trace add variable parent unset [namespace code {my destroy}]
	return -level $level [self]
    }
}

# Class of quaternions
oo::class create Q {
    superclass RAII-support
    variable R I J K
    constructor {{real 0} {i 0} {j 0} {k 0}} {
	next
	namespace import ::tcl::mathfunc::* ::tcl::mathop::*
	variable R [double $real] I [double $i] J [double $j] K [double $k]
    }
    self method return args {
	[my new {*}$args] return 2
    }

    method p {} {
	return "Q($R,$I,$J,$K)"
    }
    method values {} {
	list $R $I $J $K
    }

    method Norm {} {
	+ [* $R $R] [* $I $I] [* $J $J] [* $K $K]
    }

    method conjugate {} {
	Q return $R [- $I] [- $J] [- $K]
    }
    method norm {} {
	sqrt [my Norm]
    }
    method unit {} {
	set n [my norm]
	Q return [/ $R $n] [/ $I $n] [/ $J $n] [/ $K $n]
    }
    method reciprocal {} {
	set n2 [my Norm]
	Q return [/ $R $n2] [/ $I $n2] [/ $J $n2] [/ $K $n2]
    }
    method - {{q ""}} {
	if {[llength [info level 0]] == 2} {
	    Q return [- $R] [- $I] [- $J] [- $K]
	}
	[my + [$q -]] return
    }
    method + q {
	if {[info object isa object $q]} {
	    lassign [$q values] real i j k
	    Q return [+ $R $real] [+ $I $i] [+ $J $j] [+ $K $k]
	}
	Q return [+ $R [double $q]] $I $J $K
    }
    method * q {
	if {[info object isa object $q]} {
	    lassign [my values] a1 b1 c1 d1
	    lassign [$q values] a2 b2 c2 d2
	    Q return [expr {$a1*$a2 - $b1*$b2 - $c1*$c2 - $d1*$d2}] \
		[expr {$a1*$b2 + $b1*$a2 + $c1*$d2 - $d1*$c2}] \
		[expr {$a1*$c2 - $b1*$d2 + $c1*$a2 + $d1*$b2}] \
		[expr {$a1*$d2 + $b1*$c2 - $c1*$b2 + $d1*$a2}]
	}
	set f [double $q]
	Q return [* $R $f] [* $I $f] [* $J $f] [* $K $f]
    }
    method == q {
	expr {
	    [info object isa object $q]
	    && [info object isa typeof $q [self class]]
	    && [my values] eq [$q values]
	}
    }

    export - + * ==
}
```

Demonstration code:

```tcl
set q [Q new 1 2 3 4]
set q1 [Q new 2 3 4 5]
set q2 [Q new 3 4 5 6]
set r 7

puts "q = [$q p]"
puts "q1 = [$q1 p]"
puts "q2 = [$q2 p]"
puts "r = $r"
puts "q norm = [$q norm]"
puts "q1 norm = [$q1 norm]"
puts "q2 norm = [$q2 norm]"
puts "-q = [[$q -] p]"
puts "q conj = [[$q conjugate] p]"
puts "q + r = [[$q + $r] p]"
# Real numbers are not objects, so no extending operations for them
puts "q1 + q2 = [[$q1 + $q2] p]"
puts "q2 + q1 = [[$q2 + $q1] p]"
puts "q * r = [[$q * $r] p]"
puts "q1 * q2 = [[$q1 * $q2] p]"
puts "q2 * q1 = [[$q2 * $q1] p]"
puts "equal(q1*q2, q2*q1) = [[$q1 * $q2] == [$q2 * $q1]]"
```

{{out}}

```txt

q = Q(1.0,2.0,3.0,4.0)
q1 = Q(2.0,3.0,4.0,5.0)
q2 = Q(3.0,4.0,5.0,6.0)
r = 7
q norm = 5.477225575051661
q1 norm = 7.3484692283495345
q2 norm = 9.273618495495704
-q = Q(-1.0,-2.0,-3.0,-4.0)
q conj = Q(1.0,-2.0,-3.0,-4.0)
q + r = Q(8.0,2.0,3.0,4.0)
q1 + q2 = Q(5.0,7.0,9.0,11.0)
q2 + q1 = Q(5.0,7.0,9.0,11.0)
q * r = Q(7.0,14.0,21.0,28.0)
q1 * q2 = Q(-56.0,16.0,24.0,26.0)
q2 * q1 = Q(-56.0,18.0,20.0,28.0)
equal(q1*q2, q2*q1) = 0

```



## VBA


```vb
Option Base 1
Private Function norm(q As Variant) As Double
    norm = Sqr(WorksheetFunction.SumSq(q))
End Function

Private Function negative(q) As Variant
    Dim res(4) As Double
    For i = 1 To 4
        res(i) = -q(i)
    Next i
    negative = res
End Function

Private Function conj(q As Variant) As Variant
    Dim res(4) As Double
    res(1) = q(1)
    For i = 2 To 4
        res(i) = -q(i)
    Next i
    conj = res
End Function

Private Function addr(r As Double, q As Variant) As Variant
    Dim res As Variant
    res = q
    res(1) = r + q(1)
    addr = res
End Function

Private Function add(q1 As Variant, q2 As Variant) As Variant
    add = WorksheetFunction.MMult(Array(1, 1), Array(q1, q2))
End Function

Private Function multr(r As Double, q As Variant) As Variant
    multr = WorksheetFunction.MMult(r, q)
End Function

Private Function mult(q1 As Variant, q2 As Variant)
    Dim res(4) As Double
    res(1) = q1(1) * q2(1) - q1(2) * q2(2) - q1(3) * q2(3) - q1(4) * q2(4)
    res(2) = q1(1) * q2(2) + q1(2) * q2(1) + q1(3) * q2(4) - q1(4) * q2(3)
    res(3) = q1(1) * q2(3) - q1(2) * q2(4) + q1(3) * q2(1) + q1(4) * q2(2)
    res(4) = q1(1) * q2(4) + q1(2) * q2(3) - q1(3) * q2(2) + q1(4) * q2(1)
    mult = res
End Function

Private Sub quats(q As Variant)
    Debug.Print q(1); IIf(q(2) < 0, " - " & Abs(q(2)), " + " & q(2));
    Debug.Print IIf(q(3) < 0, "i - " & Abs(q(3)), "i + " & q(3));
    Debug.Print IIf(q(4) < 0, "j - " & Abs(q(4)), "j + " & q(4)); "k"
End Sub

Public Sub quaternions()
    q = [{ 1, 2, 3, 4}]
    q1 = [{2, 3, 4, 5}]
    q2 = [{3, 4, 5, 6}]
    Dim r_ As Double
    r_ = 7#
    Debug.Print "q            = ";: quats q
    Debug.Print "q1           = ";: quats q1
    Debug.Print "q2           = ";: quats q2
    Debug.Print "r            = "; r_
    Debug.Print "norm(q)      = "; norm(q)
    Debug.Print "negative(q)  = ";: quats negative(q)
    Debug.Print "conjugate(q) = ";: quats conj(q)
    Debug.Print "r + q        = ";: quats addr(r_, q)
    Debug.Print "q1 + q2      = ";: quats add(q1, q2)
    Debug.Print "q * r        = ";: quats multr(r_, q)
    Debug.Print "q1 * q2      = ";: quats mult(q1, q2)
    Debug.Print "q2 * q1      = ";: quats mult(q2, q1)
End Sub
```
{{out}}

```txt
q            =  1  + 2i + 3j + 4k
q1           =  2  + 3i + 4j + 5k
q2           =  3  + 4i + 5j + 6k
r            =  7
norm(q)      =  5,47722557505166
negative(q)  = -1  - 2i - 3j - 4k
conjugate(q) =  1  - 2i - 3j - 4k
r + q        =  8  + 2i + 3j + 4k
q1 + q2      =  5  + 7i + 9j + 11k
q * r        =  7  + 14i + 21j + 28k
q1 * q2      = -56  + 16i + 24j + 26k
q2 * q1      = -56  + 18i + 20j + 28k
```



## Visual Basic .NET

{{trans|C#}}
'''Compiler:''' Roslyn Visual Basic (language version >= 14, e.g. with Visual Studio 2015)
{{works with|.NET Core|2.1}}


```vbnet
Option Compare Binary
Option Explicit On
Option Infer On
Option Strict On

Structure Quaternion
    Implements IEquatable(Of Quaternion), IStructuralEquatable

    Public ReadOnly A, B, C, D As Double

    Public Sub New(a As Double, b As Double, c As Double, d As Double)
        Me.A = a
        Me.B = b
        Me.C = c
        Me.D = d
    End Sub

    Public ReadOnly Property Norm As Double
        Get
            Return Math.Sqrt((Me.A ^ 2) + (Me.B ^ 2) + (Me.C ^ 2) + (Me.D ^ 2))
        End Get
    End Property

    Public ReadOnly Property Conjugate As Quaternion
        Get
            Return New Quaternion(Me.A, -Me.B, -Me.C, -Me.D)
        End Get
    End Property

    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj IsNot Quaternion Then Return False
        Return Me.Equals(DirectCast(obj, Quaternion))
    End Function

    Public Overloads Function Equals(other As Quaternion) As Boolean Implements IEquatable(Of Quaternion).Equals
        Return other = Me
    End Function

    Public Overloads Function Equals(other As Object, comparer As IEqualityComparer) As Boolean Implements IStructuralEquatable.Equals
        If TypeOf other IsNot Quaternion Then Return False
        Dim q = DirectCast(other, Quaternion)
        Return comparer.Equals(Me.A, q.A) AndAlso
               comparer.Equals(Me.B, q.B) AndAlso
               comparer.Equals(Me.C, q.C) AndAlso
               comparer.Equals(Me.D, q.D)
    End Function

    Public Overrides Function GetHashCode() As Integer
        Return HashCode.Combine(Me.A, Me.B, Me.C, Me.D)
    End Function

    Public Overloads Function GetHashCode(comparer As IEqualityComparer) As Integer Implements IStructuralEquatable.GetHashCode
        Return HashCode.Combine(
            comparer.GetHashCode(Me.A),
            comparer.GetHashCode(Me.B),
            comparer.GetHashCode(Me.C),
            comparer.GetHashCode(Me.D))
    End Function

    Public Overrides Function ToString() As String
        Return $"Q({Me.A}, {Me.B}, {Me.C}, {Me.D})"
    End Function

#Region "Operators"
    Public Shared Operator =(left As Quaternion, right As Quaternion) As Boolean
        Return left.A = right.A AndAlso
               left.B = right.B AndAlso
               left.C = right.C AndAlso
               left.D = right.D
    End Operator

    Public Shared Operator <>(left As Quaternion, right As Quaternion) As Boolean
        Return Not left = right
    End Operator

    Public Shared Operator +(q1 As Quaternion, q2 As Quaternion) As Quaternion
        Return New Quaternion(q1.A + q2.A, q1.B + q2.B, q1.C + q2.C, q1.D + q2.D)
    End Operator

    Public Shared Operator -(q As Quaternion) As Quaternion
        Return New Quaternion(-q.A, -q.B, -q.C, -q.D)
    End Operator

    Public Shared Operator *(q1 As Quaternion, q2 As Quaternion) As Quaternion
        Return New Quaternion(
            (q1.A * q2.A) - (q1.B * q2.B) - (q1.C * q2.C) - (q1.D * q2.D),
            (q1.A * q2.B) + (q1.B * q2.A) + (q1.C * q2.D) - (q1.D * q2.C),
            (q1.A * q2.C) - (q1.B * q2.D) + (q1.C * q2.A) + (q1.D * q2.B),
            (q1.A * q2.D) + (q1.B * q2.C) - (q1.C * q2.B) + (q1.D * q2.A))
    End Operator

    Public Shared Widening Operator CType(d As Double) As Quaternion
        Return New Quaternion(d, 0, 0, 0)
    End Operator
#End Region
End Structure
```


Demonstration:

```vbnet
Module Program
    Sub Main()
        Dim q As New Quaternion(1, 2, 3, 4),
            q1 As New Quaternion(2, 3, 4, 5),
            q2 As New Quaternion(3, 4, 5, 6),
            r As Double = 7

        Console.WriteLine($"q = {q}")
        Console.WriteLine($"q1 = {q1}")
        Console.WriteLine($"q2 = {q2}")
        Console.WriteLine($"r = {r}")
        Console.WriteLine($"q.Norm = {q.Norm}")
        Console.WriteLine($"q1.Norm = {q1.Norm}")
        Console.WriteLine($"q2.Norm = {q2.Norm}")
        Console.WriteLine($"-q = {-q}")
        Console.WriteLine($"q.Conjugate = {q.Conjugate}")
        Console.WriteLine($"q + r = {q + r}")
        Console.WriteLine($"q1 + q2 = {q1 + q2}")
        Console.WriteLine($"q2 + q1 = {q2 + q1}")
        Console.WriteLine($"q * r = {q * r}")
        Console.WriteLine($"q1 * q2 = {q1 * q2}")
        Console.WriteLine($"q2 * q1 = {q2 * q1}")
        Console.WriteLine($"q1*q2 {If((q1 * q2) = (q2 * q1), "=", "!=")} q2*q1")
    End Sub
End Module
```


{{out}}

```txt
q = Q(1, 2, 3, 4)
q1 = Q(2, 3, 4, 5)
q2 = Q(3, 4, 5, 6)
r = 7
q.Norm = 5.47722557505166
q1.Norm = 7.34846922834953
q2.Norm = 9.2736184954957
-q = Q(-1, -2, -3, -4)
q.Conjugate = Q(1, -2, -3, -4)
q + r = Q(8, 2, 3, 4)
q1 + q2 = Q(5, 7, 9, 11)
q2 + q1 = Q(5, 7, 9, 11)
q * r = Q(7, 14, 21, 28)
q1 * q2 = Q(-56, 16, 24, 26)
q2 * q1 = Q(-56, 18, 20, 28)
q1*q2 != q2*q1
```



## zkl

{{trans|D}}

```zkl
class Quat{
   fcn init(real=0,i1=0,i2=0,i3=0){
      var [const] vector= // Quat(r,i,j,k) or Quat( (r,i,j,k) )
              (if(List.isType(real)) real else vm.arglist).apply("toFloat");
      var r,i,j,k; r,i,j,k=vector; // duplicate data for ease of coding
      var [const]	// properties: This is one way to do it
         norm2=vector.apply("pow",2).sum(0.0), // Norm squared
	 abs=norm2.sqrt(),		       // Norm
	 arg=(r/abs()).acos(),		// Theta !!!this may be incorrect...
      ;
   }
   fcn toString { String("[",vector.concat(","),"]") }
   var [const proxy]	// properties that need calculation (or are recursive)
      conj   =fcn{ Quat(r,-i,-j,-k) },		// Conjugate
      recip  =fcn{ n2:=norm2; Quat(r/n2,-i/n2,-j/n2,-k/n2) },// Reciprocal
      pureim =fcn{ Quat(0, i, j, k) },   	// Pure imagery
      versor =fcn{ self / abs; }, 		// Unit versor
      iversor=fcn{ pureim / pureim.abs; },	// Unit versor of imagery part
   ;

   fcn __opEQ(z) { r == z.r and i == z.i and j == z.j and k == z.k }
   fcn __opNEQ(z){ (not (self==z)) }

   fcn __opNegate{ Quat(-r, -i, -j, -k) }
   fcn __opAdd(z){
      if (Quat.isInstanceOf(z)) Quat(vector.zipWith('+,z.vector));
      else			Quat(r+z,i,j,k);
   }
   fcn __opSub(z){
      if (Quat.isInstanceOf(z)) Quat(vector.zipWith('-,z.vector));
      else			Quat(r-z,vector.xplode(1)); // same as above
   }
   fcn __opMul(z){
      if (Quat.isInstanceOf(z)){
	 Quat(r*z.r - i*z.i - j*z.j - k*z.k,
	      r*z.i + i*z.r + j*z.k - k*z.j,
	      r*z.j - i*z.k + j*z.r + k*z.i,
	      r*z.k + i*z.j - j*z.i + k*z.r);
      }
      else Quat(vector.apply('*(z)));
   }
   fcn __opDiv(z){
      if (Quat.isInstanceOf(z)) self*z.recip;
      else			Quat(r/z,i/z,j/z,k/z);
   }

   fcn pow(r){ exp(r*iversor*arg)*abs.pow(r) }	// Power function
   fcn log{ iversor*(r / abs).acos() + abs.log() }
   fcn exp{					// e^q
      inorm:=pureim.abs;
      (iversor*inorm.sin() + inorm.cos()) * r.exp();
   }
}
```


```zkl
    // Demo code
r:=7;
q:=Quat(2,3,4,5); q1:=Quat(2,3,4,5); q2:=Quat(3,4,5,6);

println("1.          norm: q.abs: ", q.abs);
println("2.                   -q: ", -q);
println("3.    conjugate: q.conj: ", q.conj);
println("4.          Quat(r) + q: ", Quat(r) + q);
println("                  q + r: ", q + r);
println("5.              q1 + q2: ", q1 + q2);
println("6.          Quat(r) * q: ", Quat(r) * q);
println("                  q * r: ", q * r);
println("7.              q1 * q2: ", q1 * q2);
println("                q2 * q1: ", q2 * q1);
println("8.  q1 * q2 == q2 * q1 ? ", q1 * q2 == q2 * q1);

i:=Quat(0,1); j:=Quat(0,0,1); k:=Quat(0,0,0,1);
println("9.1               i * i: ", i * i);
println("                  J * j: ", j * j);
println("                  k * k: ", k * k);
println("              i * j * k: ", i * j * k);

println("9.2             q1 / q2: ", q1 / q2);
println("9.3        q1 / q2 * q2: ", q1 / q2 * q2);
println("           q2 * q1 / q2: ", q2 * q1 / q2);
println("9.4      (i * pi).exp(): ", (i * (0.0).pi).exp());
println("            exp(j * pi): ", (j * (0.0).pi).exp());
println("            exp(k * pi): ", (k * (0.0).pi).exp());
println("                q.exp(): ", q.exp());
println("                q.log(): ", q.log());
println("          q.log().exp(): ", q.log().exp());
println("          q.exp().log(): ", q.exp().log());

s:=q.exp().log();
println("9.5 let s=q.exp().log(): ", s);
println("                s.exp(): ", s.exp());
println("                s.log(): ", s.log());
println("          s.log().exp(): ", s.log().exp());
println("          s.exp().log(): ", s.exp().log());
```

{{out}}

```txt

1.          norm: q.abs: 7.34847
2.                   -q: [-2,-3,-4,-5]
3.    conjugate: q.conj: [2,-3,-4,-5]
4.          Quat(r) + q: [9,3,4,5]
                  q + r: [9,3,4,5]
5.              q1 + q2: [5,7,9,11]
6.          Quat(r) * q: [14,21,28,35]
                  q * r: [14,21,28,35]
7.              q1 * q2: [-56,16,24,26]
                q2 * q1: [-56,18,20,28]
8.  q1 * q2 == q2 * q1 ? False
9.1               i * i: [-1,0,0,0]
                  J * j: [-1,0,0,0]
                  k * k: [-1,0,0,0]
              i * j * k: [-1,0,0,0]
9.2             q1 / q2: [0.790698,0.0232558,-2.77556e-17,0.0465116]
9.3        q1 / q2 * q2: [2,3,4,5]
           q2 * q1 / q2: [2,3.46512,3.90698,4.76744]
9.4      (i * pi).exp(): [-1,1.22465e-16,0,0]
            exp(j * pi): [-1,0,1.22465e-16,0]
            exp(k * pi): [-1,0,0,1.22465e-16]
                q.exp(): [5.21186,2.22222,2.96296,3.7037]
                q.log(): [1.99449,0.549487,0.732649,0.915812]
          q.log().exp(): [2,3,4,5]
          q.exp().log(): [2,0.33427,0.445694,0.557117]
9.5 let s=q.exp().log(): [2,0.33427,0.445694,0.557117]
                s.exp(): [5.21186,2.22222,2.96296,3.7037]
                s.log(): [0.765279,0.159215,0.212286,0.265358]
          s.log().exp(): [2,0.33427,0.445694,0.557117]
          s.exp().log(): [2,0.33427,0.445694,0.557117]

```


{{omit from|GUISS}}

[[Category:Geometry]]

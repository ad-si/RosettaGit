+++
title = "Vector products"
description = ""
date = 2019-10-12T09:49:42Z
aliases = []
[extra]
id = 9425
[taxonomies]
categories = ["task"]
tags = []
+++

A vector is defined as having three dimensions as being represented by an ordered collection of three numbers:   (X, Y, Z).

If you imagine a graph with the   '''x'''   and   '''y'''   axis being at right angles to each other and having a third,   '''z'''   axis coming out of the page, then a triplet of numbers,   (X, Y, Z)   would represent a point in the region,   and a vector from the origin to the point.

Given the vectors:
        <big> A = (a<sub>1</sub>,  a<sub>2</sub>,  a<sub>3</sub>) </big>
        <big> B = (b<sub>1</sub>,  b<sub>2</sub>,  b<sub>3</sub>) </big>
        <big> C = (c<sub>1</sub>,  c<sub>2</sub>,  c<sub>3</sub>) </big>
then the following common vector products are defined:
* '''The dot product'''       (a scalar quantity)
:::: <big> A • B = a<sub>1</sub>b<sub>1</sub>   +   a<sub>2</sub>b<sub>2</sub>   +    a<sub>3</sub>b<sub>3</sub> </big>
* '''The cross product'''       (a vector quantity)
:::: <big> A x B = (a<sub>2</sub>b<sub>3</sub>   -   a<sub>3</sub>b<sub>2</sub>,     a<sub>3</sub>b<sub>1</sub>   -   a<sub>1</sub>b<sub>3</sub>,     a<sub>1</sub>b<sub>2</sub>   -   a<sub>2</sub>b<sub>1</sub>) </big>
* '''The scalar triple product'''       (a scalar quantity)
:::: <big> A • (B x C) </big>
* '''The vector triple product'''       (a vector quantity)
:::: <big> A x (B x C) </big>


## Task

Given the three vectors:
         a = ( 3,    4,    5)
         b = ( 4,    3,    5)
         c = (-5,  -12,  -13)
# Create a named function/subroutine/method to compute the dot product of two vectors.
# Create a function to compute the cross product of two vectors.
# Optionally create a function to compute the scalar triple product of three vectors.
# Optionally create a function to compute the vector triple product of three vectors.
# Compute and display: <code>a • b</code>
# Compute and display: <code>a x b</code>
# Compute and display: <code>a • (b x c)</code>, the scalar triple product.
# Compute and display: <code>a x (b x c)</code>, the vector triple product.


## References

*   A starting page on Wolfram MathWorld is   {{Wolfram|Vector|Multiplication}}.
*   Wikipedia   [[wp:Dot product|dot product]].
*   Wikipedia   [[wp:Cross product|cross product]].
*   Wikipedia   [[wp:Triple product|triple product]].


## Related tasks

*   [[Dot product]]
*   [[Quaternion type]]





## Ada

not using Ada.Numerics.Real_Arrays, to show some features of the language.

Ada determines which function to call not only on the types of the parameters, but also on the return type. That way we can use the same name for all multiplications (scalar and cross). But, if we add another one to stretch the vector, we get an ambiguity error, since the compiler can't know if A*(B*C) with result-type Vector is meant to be A stretched by the scalar product of B and C, or the cross product of A and the result of the cross product of B and C. Here, I used type qualification to tell the compiler that the result of (B*C) is of type Vector.

vector.adb:

```Ada
with Ada.Text_IO;

procedure Vector is
   type Float_Vector is array (Positive range <>) of Float;
   package Float_IO is new Ada.Text_IO.Float_IO (Float);

   procedure Vector_Put (X : Float_Vector) is
   begin
      Ada.Text_IO.Put ("(");
      for I in X'Range loop
         Float_IO.Put (X (I), Aft => 1, Exp => 0);
         if I /= X'Last then
            Ada.Text_IO.Put (", ");
         end if;
      end loop;
      Ada.Text_IO.Put (")");
   end Vector_Put;

   -- cross product
   function "*" (Left, Right : Float_Vector) return Float_Vector is
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with "vectors of different size in dot product";
      end if;
      if Left'Length /= 3 then
         raise Constraint_Error with "dot product only implemented for R**3";
      end if;
      return Float_Vector'(Left (Left'First + 1) * Right (Right'First + 2) -
                             Left (Left'First + 2) * Right (Right'First + 1),
                           Left (Left'First + 2) * Right (Right'First) -
                             Left (Left'First) * Right (Right'First + 2),
                           Left (Left'First) * Right (Right'First + 1) -
                             Left (Left'First + 1) * Right (Right'First));
   end "*";

   -- scalar product
   function "*" (Left, Right : Float_Vector) return Float is
      Result : Float := 0.0;
      I, J : Positive;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with "vectors of different size in scalar product";
      end if;
      I := Left'First; J := Right'First;
      while I <= Left'Last and then J <= Right'Last loop
         Result := Result + Left (I) * Right (J);
         I := I + 1; J := J + 1;
      end loop;
      return Result;
   end "*";

   -- stretching
   function "*" (Left : Float_Vector; Right : Float) return Float_Vector is
      Result : Float_Vector (Left'Range);
   begin
      for I in Left'Range loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   A : constant Float_Vector := (3.0, 4.0, 5.0);
   B : constant Float_Vector := (4.0, 3.0, 5.0);
   C : constant Float_Vector := (-5.0, -12.0, -13.0);
begin
   Ada.Text_IO.Put ("A: "); Vector_Put (A); Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("B: "); Vector_Put (B); Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("C: "); Vector_Put (C); Ada.Text_IO.New_Line;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A dot B = "); Float_IO.Put (A * B, Aft => 1, Exp => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A x B = "); Vector_Put (A * B);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A dot (B x C) = "); Float_IO.Put (A * (B * C), Aft => 1, Exp => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("A x (B x C) = "); Vector_Put (A * Float_Vector'(B * C));
   Ada.Text_IO.New_Line;
end Vector;
```

Output:

```txt
A: ( 3.0,  4.0,  5.0)
B: ( 4.0,  3.0,  5.0)
C: (-5.0, -12.0, -13.0)

A dot B = 49.0
A x B = ( 5.0,  5.0, -7.0)
A dot (B x C) =  6.0
A x (B x C) = (-267.0, 204.0, -3.0)
```



## ALGOL 68

{{trans|Python}} Note: This specimen retains the original [[#Python|Python]] coding style.
```algol68
MODE FIELD = INT;
FORMAT field fmt = $g(-0)$;

MODE VEC = [3]FIELD;
FORMAT vec fmt = $"("f(field fmt)", "f(field fmt)", "f(field fmt)")"$;

PROC crossp = (VEC a, b)VEC:(
    #Cross product of two 3D vectors#
    CO ASSERT(LWB a = LWB b AND UPB a = UPB b AND UPB b = 3 # "For 3D vectors only" #); CO
    (a[2]*b[3] - a[3]*b[2], a[3]*b[1] - a[1]*b[3], a[1]*b[2] - a[2]*b[1])
);

PRIO MAXLWB = 8, MINUPB=8;

OP MAXLWB = (VEC a, b)INT: (LWB a<LWB b|LWB a|LWB b);
OP MINUPB = (VEC a, b)INT: (UPB a>UPB b|UPB a|UPB b);

PROC dotp = (VEC a, b)FIELD:(
    #Dot product of two vectors#
    FIELD sum := 0;
    FOR i FROM a MAXLWB b TO a MINUPB b DO sum +:= a[i]*b[i] OD;
    sum
);

PROC scalartriplep = (VEC a, b, c)VEC:(
    #Scalar triple product of three vectors: "a . (b x c)"#
    dotp(a, crossp(b, c))
);

PROC vectortriplep = (VEC a, b, c)VEC:(
    #Vector triple product of three vectors: "a x (b x c)"#
    crossp(a, crossp(b, c))
);

# Declare some useful operators #
PRIO DOT = 5, X = 5;
OP (VEC, VEC)FIELD DOT = dotp;
OP (VEC, VEC)VEC X = crossp;

main:(
    VEC a=(3, 4, 5), b=(4, 3, 5), c=(-5, -12, -13);
    printf(($"a = "f(vec fmt)";  b = "f(vec fmt)";  c = "f(vec fmt)l$ , a, b, c));
    printf($"Using PROCedures:"l$);
    printf(($"a . b = "f(field fmt)l$, dotp(a,b)));
    printf(($"a x b = "f(vec fmt)l$, crossp(a,b)));
    printf(($"a . (b x c) = "f(field fmt)l$, scalartriplep(a, b, c)));
    printf(($"a x (b x c) = "f(vec fmt)l$, vectortriplep(a, b, c)));
    printf($"Using OPerators:"l$);
    printf(($"a . b = "f(field fmt)l$, a DOT b));
    printf(($"a x b = "f(vec fmt)l$, a X b));
    printf(($"a . (b x c) = "f(field fmt)l$, a DOT (b X c)));
    printf(($"a x (b x c) = "f(vec fmt)l$, a X (b X c)))
)
```

Output:

```txt

a = (3, 4, 5);  b = (4, 3, 5);  c = (-5, -12, -13)
Using PROCedures:
a . b = 49
a x b = (5, 5, -7)
a . (b x c) = 6
a x (b x c) = (-267, 204, -3)
Using OPerators:
a . b = 49
a x b = (5, 5, -7)
a . (b x c) = 6
a x (b x c) = (-267, 204, -3)

```



## ALGOL W


```algolw
begin
    % define the Vector record type                                          %
    record Vector( integer X, Y, Z );

    % calculates the dot product of two Vectors                              %
    integer procedure dotProduct( reference(Vector) value A, B ) ;
        ( X(A) * X(B) ) + ( Y(A) * Y(B) ) + ( Z(A) * Z(B) );

    % calculates the cross product or two Vectors                            %
    reference(Vector) procedure crossProduct( reference(Vector) value A, B ) ;
        Vector( ( Y(A) * Z(B) ) - ( Z(A) * Y(B) )
              , ( Z(A) * X(B) ) - ( X(A) * Z(B) )
              , ( X(A) * Y(B) ) - ( Y(A) * X(B) )
              );

    % calculates the scaler triple product of two vectors                    %
    integer procedure scalerTripleProduct( reference(Vector) value A, B, C ) ;
        dotProduct( A, crossProduct( B, C ) );

    % calculates the vector triple product of two vectors                    %
    reference(Vector) procedure vectorTripleProduct( reference(Vector) value A, B, C ) ;
        crossProduct( A, crossProduct( B, C ) );

    % test the Vector routines                                               %
    begin
        procedure writeonVector( reference(Vector) value v ) ;
            writeon( "(", X(v), ", ", Y(v), ", ", Z(v), ")" );

        Reference(Vector) a, b, c;

        a := Vector(  3,   4,   5 );
        b := Vector(  4,   3,   5 );
        c := Vector( -5, -12, -13 );

        i_w := 1; s_w := 0; % set output formatting                          %

        write( "            a: " ); writeonVector( a );
        write( "            b: " ); writeonVector( b );
        write( "            c: " ); writeonVector( c );
        write( "        a . b: ", dotProduct( a, b ) );
        write( "        a x b: " ); writeonVector( crossProduct( a, b ) );
        write( "a . ( b x c ): ", scalerTripleProduct( a, b, c ) );
        write( "a x ( b x c ): " ); writeonVector( vectorTripleProduct( a, b, c ) )
    end
end.
```

```txt

            a: (3, 4, 5)
            b: (4, 3, 5)
            c: (-5, -12, -13)
        a . b: 49
        a x b: (5, 5, -7)
a . ( b x c ): 6
a x ( b x c ): (-267, 204, -3)

```



## AutoHotkey

```AutoHotkey
V := {a: [3, 4, 5], b: [4, 3, 5], c: [-5, -12, -13]}

for key, val in V
	Out .= key " = (" val[1] ", " val[2] ", " val[3] ")`n"

CP := CrossProduct(V.a, V.b)
VTP := VectorTripleProduct(V.a, V.b, V.c)

MsgBox, % Out "`na • b = " DotProduct(V.a, V.b) "`n"
	. "a x b = (" CP[1] ", " CP[2] ", " CP[3] ")`n"
	. "a • b x c = " ScalerTripleProduct(V.a, V.b, V.c) "`n"
	. "a x b x c = (" VTP[1] ", " VTP[2] ", " VTP[3] ")"

DotProduct(v1, v2) {
	return, v1[1] * v2[1] + v1[2] * v2[2] + v1[3] * v2[3]
}

CrossProduct(v1, v2) {
	return, [v1[2] * v2[3] - v1[3] * v2[2]
	      ,  v1[3] * v2[1] - v1[1] * v2[3]
	      ,  v1[1] * v2[2] - v1[2] * v2[1]]
}

ScalerTripleProduct(v1, v2, v3) {
	return, DotProduct(v1, CrossProduct(v2, v3))
}

VectorTripleProduct(v1, v2, v3) {
	return, CrossProduct(v1, CrossProduct(v2, v3))
}
```

'''Output:'''

```txt
a = (3, 4, 5)
b = (4, 3, 5)
c = (-5, -12, -13)

a • b = 49
a x b = (5, 5, -7)
a • b x c = 6
a x b x c = (-267, 204, -3)
```



## AWK


```awk
#!/usr/bin/awk -f
BEGIN {
     a[1] = 3; a[2]= 4; a[3] = 5;
     b[1] = 4; b[2]= 3; b[3] = 5;
     c[1] = -5; c[2]= -12; c[3] = -13;

     print "a = ",printVec(a);
     print "b = ",printVec(b);
     print "c = ",printVec(c);
     print "a.b = ",dot(a,b);
     ## upper case variables are used as temporary or intermediate results
     cross(a,b,D);print "a.b = ",printVec(D);
     cross(b,c,D);print "a.(b x c) = ",dot(a,D);
     cross(b,c,D);cross(a,D,E); print "a x (b x c) = ",printVec(E);
}

function dot(A,B) {
     return A[1]*B[1]+A[2]*B[2]+A[3]*B[3];
}

function cross(A,B,C) {
     C[1] = A[2]*B[3]-A[3]*B[2];
     C[2] = A[3]*B[1]-A[1]*B[3];
     C[3] = A[1]*B[2]-A[2]*B[1];
}

function printVec(C) {
    return "[ "C[1]" "C[2]" "C[3]" ]";
}
```

Output:

```txt
a =  [ 3 4 5 ]
b =  [ 4 3 5 ]
c =  [ -5 -12 -13 ]
A.b =  49
a.b =  [ 5 5 -7 ]
a.(b x c) =  6
a x (b x c) =  [ -267 204 -3 ]

```



## BASIC256

```basic256

 a={3,4,5}:b={4,3,5}:c={-5,-12,-13}

print "A.B = "+dot_product(ref(a),ref(b))
call cross_product(ref(a),ref(b),ref(y))
Print "AxB = ("+y[0]+","+y[1]+","+y[2]+")"
print "A.(BxC) = "+s_tri(ref(a),ref(b),ref(c))
call v_tri(ref(a),ref(b),ref(c),ref(x),ref(y))
Print "A x (BxC) = ("+y[0]+","+y[1]+","+y[2]+")"

function dot_product(ref(x1),ref(x2))
    dot_product= 0
   for t = 0 to 2
      dot_product += x1[t]*x2[t]
   next t
end function

subroutine cross_product(ref(x1),ref(x2),ref(y1))
   y1={0,0,0}
   y1[0]=x1[1]*x2[2]-x1[2]*x2[1]
   y1[1]=x1[2]*x2[0]-x1[0]*x2[2]
   y1[2]=x1[0]*x2[1]-x1[1]*x2[0]
end subroutine

function s_tri(ref(x1),ref(x2),ref(x3))
   call cross_product(ref(x2),ref(x3),ref(y1))
   s_tri=dot_product(ref(x1),ref(y1))
end function

subroutine v_tri(ref(x1),ref(x2),ref(x3),ref(y1),ref(y2))
  call cross_product(ref(x2),ref(x3),ref(y1))
  call cross_product(ref(x1),ref(y1),ref(y2))
end subroutine


```

Output:

```txt

A.B = 49
AxB = (5,5,-7)
A.(BxC) = 6
A x (BxC) = (-267,204,-3)

```



## BBC BASIC

```bbcbasic
      DIM a(2), b(2), c(2), d(2)
      a() = 3, 4, 5
      b() = 4, 3, 5
      c() = -5, -12, -13

      PRINT "a . b = "; FNdot(a(),b())
      PROCcross(a(),b(),d())
      PRINT "a x b = (";d(0)", ";d(1)", ";d(2)")"
      PRINT "a . (b x c) = "; FNscalartriple(a(),b(),c())
      PROCvectortriple(a(),b(),c(),d())
      PRINT "a x (b x c) = (";d(0)", ";d(1)", ";d(2)")"
      END

      DEF FNdot(A(),B())
      LOCAL C() : DIM C(0,0)
      C() = A().B()
      = C(0,0)

      DEF PROCcross(A(),B(),C())
      C() = A(1)*B(2)-A(2)*B(1), A(2)*B(0)-A(0)*B(2), A(0)*B(1)-A(1)*B(0)
      ENDPROC

      DEF FNscalartriple(A(),B(),C())
      LOCAL D() : DIM D(2)
      PROCcross(B(),C(),D())
      = FNdot(A(),D())

      DEF PROCvectortriple(A(),B(),C(),D())
      PROCcross(B(),C(),D())
      PROCcross(A(),D(),D())
      ENDPROC
```

Output:

```txt

a . b = 49
a x b = (5, 5, -7)
a . (b x c) = 6
a x (b x c) = (-267, 204, -3)

```



## C


```c
#include <stdio.h>

typedef struct{
	float i,j,k;
	}Vector;

Vector a = {3, 4, 5},b = {4, 3, 5},c = {-5, -12, -13};

float dotProduct(Vector a, Vector b)
{
	return a.i*b.i+a.j*b.j+a.k*b.k;
}

Vector crossProduct(Vector a,Vector b)
{
	Vector c = {a.j*b.k - a.k*b.j, a.k*b.i - a.i*b.k, a.i*b.j - a.j*b.i};

	return c;
}

float scalarTripleProduct(Vector a,Vector b,Vector c)
{
	return dotProduct(a,crossProduct(b,c));
}

Vector vectorTripleProduct(Vector a,Vector b,Vector c)
{
	return crossProduct(a,crossProduct(b,c));
}

void printVector(Vector a)
{
	printf("( %f, %f, %f)",a.i,a.j,a.k);
}

int main()
{
	printf("\n a = "); printVector(a);
	printf("\n b = "); printVector(b);
	printf("\n c = "); printVector(c);
	printf("\n a . b = %f",dotProduct(a,b));
	printf("\n a x b = "); printVector(crossProduct(a,b));
	printf("\n a . (b x c) = %f",scalarTripleProduct(a,b,c));
	printf("\n a x (b x c) = "); printVector(vectorTripleProduct(a,b,c));

	return 0;
}
```

Output:

```txt

 a = ( 3.000000, 4.000000, 5.000000)
 b = ( 4.000000, 3.000000, 5.000000)
 c = ( -5.000000, -12.000000, -13.000000)
 a . b = 49.000000
 a x b = ( 5.000000, 5.000000, -7.000000)
 a . (b x c) = 6.000000
 a x (b x c) = ( -267.000000, 204.000000, -3.000000)

```


## C#

```c#
using System;
using System.Windows.Media.Media3D;

class VectorProducts
{
    static double ScalarTripleProduct(Vector3D a, Vector3D b, Vector3D c)
    {
        return Vector3D.DotProduct(a, Vector3D.CrossProduct(b, c));
    }

    static Vector3D VectorTripleProduct(Vector3D a, Vector3D b, Vector3D c)
    {
        return Vector3D.CrossProduct(a, Vector3D.CrossProduct(b, c));
    }

    static void Main()
    {
        var a = new Vector3D(3, 4, 5);
        var b = new Vector3D(4, 3, 5);
        var c = new Vector3D(-5, -12, -13);

        Console.WriteLine(Vector3D.DotProduct(a, b));
        Console.WriteLine(Vector3D.CrossProduct(a, b));
        Console.WriteLine(ScalarTripleProduct(a, b, c));
        Console.WriteLine(VectorTripleProduct(a, b, c));
    }
}
```

Output:

```txt
49
5;5;-7
6
-267;204;-3
```



## C++


```cpp
#include <iostream>

template< class T >
class D3Vector {

template< class U >
friend std::ostream & operator<<( std::ostream & , const D3Vector<U> & ) ;

public :
   D3Vector( T a , T b , T c ) {
      x = a ;
      y = b ;
      z = c ;
   }

   T dotproduct ( const D3Vector & rhs ) {
      T scalar = x * rhs.x + y * rhs.y + z * rhs.z ;
      return scalar ;
   }

   D3Vector crossproduct ( const D3Vector & rhs ) {
      T a = y * rhs.z - z * rhs.y ;
      T b = z * rhs.x - x * rhs.z ;
      T c = x * rhs.y - y * rhs.x ;
      D3Vector product( a , b , c ) ;
      return product ;
   }

   D3Vector triplevec( D3Vector & a , D3Vector & b ) {
      return crossproduct ( a.crossproduct( b ) ) ;
   }

   T triplescal( D3Vector & a, D3Vector & b ) {
      return dotproduct( a.crossproduct( b ) ) ;
   }

private :
   T x , y , z ;
} ;

template< class T >
std::ostream & operator<< ( std::ostream & os , const D3Vector<T> & vec ) {
   os << "( "  << vec.x << " ,  " << vec.y << " ,  " << vec.z << " )" ;
   return os ;
}

int main( ) {
   D3Vector<int> a( 3 , 4 , 5 ) , b ( 4 , 3 , 5 ) , c( -5 , -12 , -13 ) ;
   std::cout << "a . b : " << a.dotproduct( b ) << "\n" ;
   std::cout << "a x b : " << a.crossproduct( b ) << "\n" ;
   std::cout << "a . b x c : " << a.triplescal( b , c ) << "\n" ;
   std::cout << "a x b x c : " << a.triplevec( b , c ) << "\n" ;
   return 0 ;
}
```

Output:<PRE>a . b : 49
a x b : ( 5 , 5 , -7 )
a . b x c : 6
a x b x c : ( -267 , 204 , -3 )
</PRE>


## Ceylon


```ceylon
shared void run() {

	alias Vector => Float[3];

	function dot(Vector a, Vector b) =>
			a[0] * b[0] + a[1] * b[1] + a[2] * b[2];

	function cross(Vector a, Vector b) => [
		a[1] * b[2] - a[2] * b[1],
		a[2] * b[0] - a[0] * b[2],
		a[0] * b[1] - a[1] * b[0]
	];

	function scalarTriple(Vector a, Vector b, Vector c) =>
			dot(a, cross(b, c));

	function vectorTriple(Vector a, Vector b, Vector c) =>
			cross(a, cross(b, c));

	value a = [ 3.0,    4.0,    5.0 ];
	value b = [ 4.0,    3.0,    5.0 ];
	value c = [-5.0,  -12.0,  -13.0 ];

	print("``a`` . ``b`` = ``dot(a, b)``");
	print("``a`` X ``b`` = ``cross(a, b)``");
	print("``a`` . ``b`` X ``c`` = ``scalarTriple(a, b, c)``");
	print("``a`` X ``b`` X ``c`` = ``vectorTriple(a, b, c)``");
}
```

```txt
[3.0, 4.0, 5.0] . [4.0, 3.0, 5.0] = 49.0
[3.0, 4.0, 5.0] X [4.0, 3.0, 5.0] = [5.0, 5.0, -7.0]
[3.0, 4.0, 5.0] . [4.0, 3.0, 5.0] X [-5.0, -12.0, -13.0] = 6.0
[3.0, 4.0, 5.0] X [4.0, 3.0, 5.0] X [-5.0, -12.0, -13.0] = [-267.0, 204.0, -3.0]
```



## Clojure


```clojure
(defrecord Vector [x y z])

(defn dot
  [U V]
  (+ (* (:x U) (:x V))
     (* (:y U) (:y V))
     (* (:z U) (:z V))))

(defn cross
  [U V]
  (new Vector
       (- (* (:y U) (:z V)) (* (:z U) (:y V)))
       (- (* (:z U) (:x V)) (* (:x U) (:z V)))
       (- (* (:x U) (:y V)) (* (:y U) (:x V)))))

(let [a (new Vector 3 4 5)
      b (new Vector 4 3 5)
      c (new Vector -5 -12 -13)]
  (doseq
    [prod (list
            (dot a b)
            (cross a b)
            (dot a (cross b c))
            (cross a (cross b c)))]
    (println prod)))
```

Output:<PRE>
49
#:user.Vector{:x 5, :y 5, :z -7}
6
#:user.Vector{:x -267, :y 204, :z -3}</PRE>


## Common Lisp


Using the Common Lisp Object System.


```lisp
(defclass 3d-vector ()
  ((x :type number :initarg :x)
   (y :type number :initarg :y)
   (z :type number :initarg :z)))

(defmethod print-object ((object 3d-vector) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y z) object
      (format stream "~a ~a ~a" x y z))))

(defun make-3d-vector (x y z)
  (make-instance '3d-vector :x x :y y :z z))

(defmethod dot-product ((a 3d-vector) (b 3d-vector))
  (with-slots ((a1 x) (a2 y) (a3 z)) a
    (with-slots ((b1 x) (b2 y) (b3 z)) b
      (+ (* a1 b1) (* a2 b2) (* a3 b3)))))

(defmethod cross-product ((a 3d-vector)
                                 (b 3d-vector))
  (with-slots ((a1 x) (a2 y) (a3 z)) a
    (with-slots ((b1 x) (b2 y) (b3 z)) b
      (make-instance '3d-vector
                     :x (- (* a2 b3) (* a3 b2))
                     :y (- (* a3 b1) (* a1 b3))
                     :z (- (* a1 b2) (* a2 b1))))))

(defmethod scalar-triple-product ((a 3d-vector)
                                  (b 3d-vector)
                                  (c 3d-vector))
  (dot-product a (cross-product b c)))

(defmethod vector-triple-product ((a 3d-vector)
                                  (b 3d-vector)
                                  (c 3d-vector))
  (cross-product a (cross-product b c)))

(defun vector-products-example ()
  (let ((a (make-3d-vector 3 4 5))
        (b (make-3d-vector 4 3 5))
        (c (make-3d-vector -5 -12 -13)))
    (values (dot-product a b)
            (cross-product a b)
            (scalar-triple-product a b c)
            (vector-triple-product a b c))))
```

Output:
 CL-USER> (vector-products-example)
 49
 #<3D-VECTOR 5 5 -7>
 6
 #<3D-VECTOR -267 204 -3>

Using vector type


```lisp
(defun cross (a b)
  (when (and (equal (length a) 3) (equal (length b) 3))
      (vector
       (- (* (elt a 1) (elt b 2)) (* (elt a 2) (elt b 1)))
       (- (* (elt a 2) (elt b 0)) (* (elt a 0) (elt b 2)))
       (- (* (elt a 0) (elt b 1)) (* (elt a 1) (elt b 0))))))

(defun dot (a b)
  (when (equal (length a) (length b))
      (loop for ai across a for bi across b sum (* ai bi))))

(defun scalar-triple (a b c)
  (dot a (cross b c)))

(defun vector-triple (a b c)
  (cross a (cross b c)))

(defun task (a b c)
  (values (dot a b)
          (cross a b)
          (scalar-triple a b c)
          (vector-triple a b c)))

```


Output:
 CL-USER> (task (vector 3 4 5) (vector 4 3 5) (vector -5 -12 -13))
 49
 #(5 5 -7)
 6
 #(-267 204 -3)


## D


```d
import std.stdio, std.conv, std.numeric;

struct V3 {
    union {
        immutable struct { double x, y, z; }
        immutable double[3] v;
    }

    double dot(in V3 rhs) const pure nothrow /*@safe*/ @nogc {
        return dotProduct(v, rhs.v);
    }

    V3 cross(in V3 rhs) const pure nothrow @safe @nogc {
        return V3(y * rhs.z - z * rhs.y,
                  z * rhs.x - x * rhs.z,
                  x * rhs.y - y * rhs.x);
    }

    string toString() const { return v.text; }
}

double scalarTriple(in V3 a, in V3 b, in V3 c) /*@safe*/ pure nothrow {
    return a.dot(b.cross(c));
    // function vector_products.V3.cross (const(V3) rhs) immutable
    // is not callable using argument types (const(V3)) const
}

V3 vectorTriple(in V3 a, in V3 b, in V3 c) @safe pure nothrow @nogc {
    return a.cross(b.cross(c));
}

void main() {
    immutable V3 a = {3, 4, 5},
                 b = {4, 3, 5},
                 c = {-5, -12, -13};

    writeln("a = ", a);
    writeln("b = ", b);
    writeln("c = ", c);
    writeln("a . b = ", a.dot(b));
    writeln("a x b = ", a.cross(b));
    writeln("a . (b x c) = ", scalarTriple(a, b, c));
    writeln("a x (b x c) = ", vectorTriple(a, b, c));
}
```

```txt
a = [3, 4, 5]
b = [4, 3, 5]
c = [-5, -12, -13]
a . b = 49
a x b = [5, 5, -7]
a . (b x c) = 6
a x (b x c) = [-267, 204, -3]
```



## EchoLisp

The '''math''' library includes the '''dot-product''' and '''cross-product''' functions. They work on complex or real vectors.

```scheme

(lib 'math)

(define (scalar-triple-product a b c)
  (dot-product a (cross-product b c)))

(define (vector-triple-product a b c)
  (cross-product a (cross-product b c)))

(define a #(3 4 5))
(define b #(4 3 5))
(define c #(-5 -12 -13))

(cross-product a b)
    → #( 5 5 -7)
(dot-product a b)
    → 49
(scalar-triple-product a b c)
    → 6
(vector-triple-product a b c)
    → #( -267 204 -3)

```



## Elixir


```elixir
defmodule Vector do
  def dot_product({a1,a2,a3}, {b1,b2,b3}), do: a1*b1 + a2*b2 + a3*b3

  def cross_product({a1,a2,a3}, {b1,b2,b3}), do: {a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1}

  def scalar_triple_product(a, b, c), do: dot_product(a, cross_product(b, c))

  def vector_triple_product(a, b, c), do: cross_product(a, cross_product(b, c))
end

a = {3, 4, 5}
b = {4, 3, 5}
c = {-5, -12, -13}

IO.puts "a = #{inspect a}"
IO.puts "b = #{inspect b}"
IO.puts "c = #{inspect c}"
IO.puts "a . b = #{inspect Vector.dot_product(a, b)}"
IO.puts "a x b = #{inspect Vector.cross_product(a, b)}"
IO.puts "a . (b x c) = #{inspect Vector.scalar_triple_product(a, b, c)}"
IO.puts "a x (b x c) = #{inspect Vector.vector_triple_product(a, b, c)}"
```


```txt

a = {3, 4, 5}
b = {4, 3, 5}
c = {-5, -12, -13}
a . b = 49
a x b = {5, 5, -7}
a . (b x c) = 6
a x (b x c) = {-267, 204, -3}

```


## Erlang


```Erlang

-module(vector).
-export([main/0]).
vector_product(X,Y)->
[X1,X2,X3]=X,
[Y1,Y2,Y3]=Y,
Ans=[X2*Y3-X3*Y2,X3*Y1-X1*Y3,X1*Y2-X2*Y1],
Ans.
dot_product(X,Y)->
[X1,X2,X3]=X,
[Y1,Y2,Y3]=Y,
Ans=X1*Y1+X2*Y2+X3*Y3,
io:fwrite("~p~n",[Ans]).
main()->
{ok, A} = io:fread("Enter vector A : ", "~d ~d ~d"),
{ok, B} = io:fread("Enter vector B : ", "~d ~d ~d"),
{ok, C} = io:fread("Enter vector C : ", "~d ~d ~d"),
dot_product(A,B),
Ans=vector_product(A,B),
io:fwrite("~p,~p,~p~n",Ans),
dot_product(C,vector_product(A,B)),
io:fwrite("~p,~p,~p~n",vector_product(C,vector_product(A,B))).

```


## ERRE


```ERRE

PROGRAM VECTORPRODUCT

!$DOUBLE

TYPE TVECTOR=(X,Y,Z)

DIM A:TVECTOR,B:TVECTOR,C:TVECTOR

DIM AA:TVECTOR,BB:TVECTOR,CC:TVECTOR
DIM DD:TVECTOR,EE:TVECTOR,FF:TVECTOR

PROCEDURE DOTPRODUCT(DD.,EE.->DOTP)
    DOTP=DD.X*EE.X+DD.Y*EE.Y+DD.Z*EE.Z
END PROCEDURE

PROCEDURE CROSSPRODUCT(DD.,EE.->FF.)
  FF.X=DD.Y*EE.Z-DD.Z*EE.Y
  FF.Y=DD.Z*EE.X-DD.X*EE.Z
  FF.Z=DD.X*EE.Y-DD.Y*EE.X
END PROCEDURE

PROCEDURE SCALARTRIPLEPRODUCT(AA.,BB.,CC.->SCALARTP)
  CROSSPRODUCT(BB.,CC.->FF.)
  DOTPRODUCT(AA.,FF.->SCALARTP)
END PROCEDURE

PROCEDURE VECTORTRIPLEPRODUCT(AA.,BB.,CC.->FF.)
  CROSSPRODUCT(BB.,CC.->FF.)
  CROSSPRODUCT(AA.,FF.->FF.)
END PROCEDURE

PROCEDURE PRINTVECTOR(AA.)
  PRINT("(";AA.X;",";AA.Y;",";AA.Z;")")
END PROCEDURE

BEGIN
  A.X=3  A.Y=4    A.Z=5
  B.X=4  B.Y=3    B.Z=5
  C.X=-5 C.Y=-12  C.Z=-13

  PRINT("A: ";) PRINTVECTOR(A.)
  PRINT("B: ";) PRINTVECTOR(B.)
  PRINT("C: ";) PRINTVECTOR(C.)

  PRINT
  DOTPRODUCT(A.,B.->DOTP)
  PRINT("A.B    =";DOTP)

  CROSSPRODUCT(A.,B.->FF.)
  PRINT("AxB    =";) PRINTVECTOR(FF.)

  SCALARTRIPLEPRODUCT(A.,B.,C.->SCALARTP)
  PRINT("A.(BxC)=";SCALARTP)

  VECTORTRIPLEPRODUCT(A.,B.,C.->FF.)
  PRINT("Ax(BxC)=";) PRINTVECTOR(FF.)
END PROGRAM

```



## Euphoria


```euphoria
constant X = 1, Y = 2, Z = 3

function dot_product(sequence a, sequence b)
    return a[X]*b[X] + a[Y]*b[Y] + a[Z]*b[Z]
end function

function cross_product(sequence a, sequence b)
    return { a[Y]*b[Z] - a[Z]*b[Y],
             a[Z]*b[X] - a[X]*b[Z],
             a[X]*b[Y] - a[Y]*b[X] }
end function

function scalar_triple(sequence a, sequence b, sequence c)
    return dot_product( a, cross_product( b, c ) )
end function

function vector_triple( sequence a, sequence b, sequence c)
    return cross_product( a, cross_product( b, c ) )
end function

constant a = { 3, 4, 5 }, b = { 4, 3, 5 }, c = { -5, -12, -13 }

puts(1,"a = ")
? a
puts(1,"b = ")
? b
puts(1,"c = ")
? c
puts(1,"a dot b = ")
? dot_product( a, b )
puts(1,"a x b = ")
? cross_product( a, b )
puts(1,"a dot (b x c) = ")
? scalar_triple( a, b, c )
puts(1,"a x (b x c) = ")
? vector_triple( a, b, c )
```

Output:

```txt
a = {3,4,5}
b = {4,3,5}
c = {-5,-12,-13}
a dot b = 49
a x b = {5,5,-7}
a dot (b x c) = 6
a x (b x c) = {-267,204,-3}

```


=={{header|F#|F sharp}}==

```fsharp
let dot (ax, ay, az) (bx, by, bz) =
    ax * bx + ay * by + az * bz

let cross (ax, ay, az) (bx, by, bz) =
    (ay*bz - az*by, az*bx - ax*bz, ax*by - ay*bx)

let scalTrip a b c =
    dot a (cross b c)

let vecTrip a b c =
    cross a (cross b c)

[<EntryPoint>]
let main _ =
    let a = (3.0, 4.0, 5.0)
    let b = (4.0, 3.0, 5.0)
    let c = (-5.0, -12.0, -13.0)
    printfn "%A" (dot a b)
    printfn "%A" (cross a b)
    printfn "%A" (scalTrip a b c)
    printfn "%A" (vecTrip a b c)
    0 // return an integer exit code
```

```txt
49.0
(5.0, 5.0, -7.0)
6.0
(-267.0, 204.0, -3.0)
```



## Factor

Factor has a fantastic <tt>math.vectors</tt> vocabulary, but in the spirit of the task, it is not used.

```factor

USING: arrays io locals math prettyprint sequences ;
IN: rosetta-code.vector-products

: dot-product ( a b -- dp ) [ * ] 2map sum ;

:: cross-product ( a b -- cp )
    a first :> a1 a second :> a2 a third :> a3
    b first :> b1 b second :> b2 b third :> b3
    a2 b3 * a3 b2 * - ! X
    a3 b1 * a1 b3 * - ! Y
    a1 b2 * a2 b1 * - ! Z
    3array ;

: scalar-triple-product ( a b c -- stp )
    cross-product dot-product ;

: vector-triple-product ( a b c -- vtp )
    cross-product cross-product ;

[let
    { 3 4 5 }      :> a
    { 4 3 5 }      :> b
    { -5 -12 -13 } :> c
    "a: " write a .
    "b: " write b .
    "c: " write c . nl
    "a . b: " write a b dot-product .
    "a x b: " write a b cross-product .
    "a . (b x c): " write a b c scalar-triple-product .
    "a x (b x c): " write a b c vector-triple-product .
]

```

```txt

a: { 3 4 5 }
b: { 4 3 5 }
c: { -5 -12 -13 }

a . b: 49
a x b: { 5 5 -7 }
a . (b x c): 6
a x (b x c): { -267 204 -3 }

```



## Fantom


```fantom
class Main
{
  Int dot_product (Int[] a, Int[] b)
  {
    a[0]*b[0] + a[1]*b[1] + a[2]*b[2]
  }

  Int[] cross_product (Int[] a, Int[] b)
  {
    [a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1]-a[1]*b[0]]
  }

  Int scalar_triple_product (Int[] a, Int[] b, Int[] c)
  {
    dot_product (a, cross_product (b, c))
  }

  Int[] vector_triple_product (Int[] a, Int[] b, Int[] c)
  {
    cross_product (a, cross_product (b, c))
  }

  Void main ()
  {
    a := [3, 4, 5]
    b := [4, 3, 5]
    c := [-5, -12, -13]

    echo ("a . b = " + dot_product (a, b))
    echo ("a x b = [" + cross_product(a, b).join (", ") + "]")
    echo ("a . (b x c) = " + scalar_triple_product (a, b, c))
    echo ("a x (b x c) = [" + vector_triple_product(a, b, c).join (", ") + "]")
  }
}
```

Output:

```txt

a . b = 49
a x b = [5, 5, -7]
a . (b x c) = 6
a x (b x c) = [-267, 204, -3]

```



## Forth

```Forth

: 3f!    ( &v - ) ( f: x y z - ) dup float+ dup float+ f! f! f! ;

: Vector \ Compiletime: ( f: x y z - ) ( <name> - )
   create here [ 3 floats ] literal allot 3f! ; \ Runtime: ( - &v )

: >fx@    ( &v - ) ( f: - n ) postpone f@ ; immediate
: >fy@    ( &v - ) ( f: - n ) float+ f@ ;
: >fz@    ( &v - ) ( f: - n ) float+ float+ f@ ;
: .Vector ( &v - ) dup >fz@ dup >fy@ >fx@ f. f. f. ;

: Dot*    ( &v1 &v2 - ) ( f - DotPrd )
   2dup >fx@  >fx@ f*
   2dup >fy@  >fy@ f* f+
        >fz@  >fz@ f* f+ ;

: Cross*  ( &v1 &v2 &vResult - )
   >r 2dup >fz@  >fy@ f*
      2dup >fy@  >fz@ f* f-
      2dup >fx@  >fz@ f*
      2dup >fz@  >fx@ f* f-
      2dup >fy@  >fx@ f*
           >fx@  >fy@ f* f-
   r> 3f! ;

: ScalarTriple* ( &v1 &v2 &v3 - ) ( f: - ScalarTriple* )
   >r pad Cross* pad r> Dot* ;

: VectorTriple* ( &v1 &v2 &v3 &vDest - )
   >r swap r@ Cross* r> tuck Cross* ;

 3e   4e   5e Vector A
 4e   3e   5e Vector B
-5e -12e -13e Vector C

cr
cr .( a . b = ) A B Dot* f.
cr .( a x b = ) A B pad Cross* pad .Vector
cr .( a . [b x c] = ) A B C ScalarTriple* f.
cr .( a x [b x c] = ) A B C pad VectorTriple* pad .Vector
```

```txt

a . b = 49.0000
a x b = 5.00000 5.00000 -7.00000
a . [b x c] = 6.00000
a x [b x c] = -267.000 204.000 -3.00000

```

```forth

S" fsl-util.fs" REQUIRED
: 3f! 3 SWAP }fput ;
: vector
  CREATE
    HERE 3 DUP FLOAT DUP , * ALLOT SWAP CELL+ }fput
  DOES>
    CELL+ ;
: >fx@ 0 } F@ ;
: >fy@ 1 } F@ ;
: >fz@ 2 } F@ ;
: .Vector 3 SWAP }fprint ;
 0e   0e   0e vector pad  \ NB: your system will be non-standard after this line
\ From here on is identical to the above example
```



## Fortran

Specialized for 3-dimensional vectors.

```fortran
program VectorProducts

  real, dimension(3)  :: a, b, c

  a = (/ 3, 4, 5 /)
  b = (/ 4, 3, 5 /)
  c = (/ -5, -12, -13 /)

  print *, dot_product(a, b)
  print *, cross_product(a, b)
  print *, s3_product(a, b, c)
  print *, v3_product(a, b, c)

contains

  function cross_product(a, b)
    real, dimension(3) :: cross_product
    real, dimension(3), intent(in) :: a, b

    cross_product(1) = a(2)*b(3) - a(3)*b(2)
    cross_product(2) = a(3)*b(1) - a(1)*b(3)
    cross_product(3) = a(1)*b(2) - b(1)*a(2)
  end function cross_product

  function s3_product(a, b, c)
    real :: s3_product
    real, dimension(3), intent(in) :: a, b, c

    s3_product = dot_product(a, cross_product(b, c))
  end function s3_product

  function v3_product(a, b, c)
    real, dimension(3) :: v3_product
    real, dimension(3), intent(in) :: a, b, c

    v3_product = cross_product(a, cross_product(b, c))
  end function v3_product

end program VectorProducts
```

Output

```txt
     49.0000
     5.00000         5.00000        -7.00000
     6.00000
    -267.000         204.000        -3.00000

```



## FreeBASIC


```FreeBASIC
  'Construct only required operators for this.
Type V3
    As double x,y,z
    declare operator cast() as string
End Type
#define dot *
#define cross ^
#define Show(t1,t) ? #t1;tab(22);t

operator V3.cast() as string
return "("+str(x)+","+str(y)+","+str(z)+")"
end operator

Operator dot(v1 As v3,v2 As v3) As double
Return v1.x*v2.x+v1.y*v2.y+v1.z*v2.z
End Operator

Operator cross(v1 As v3,v2 As v3) As v3
Return type<v3>(v1.y*v2.z-v2.y*v1.z,-(v1.x*v2.z-v2.x*v1.z),v1.x*v2.y-v2.x*v1.y)
End Operator

dim as V3 a = (3, 4, 5), b = (4, 3, 5), c = (-5, -12, -13)

Show(a,a)
Show(b,b)
Show(c,c)
?
Show(a . b,a dot b)
Show(a X b,a cross b)
Show(a . b X c,a dot b cross c)
Show(a X (b X c),a cross (b cross c))
sleep
```

```txt
a                    (3,4,5)
b                    (4,3,5)
c                    (-5,-12,-13)

a . b                 49
a X b                (5,5,-7)
a . b X c             6
a X (b X c)          (-267,204,-3)
```



## FunL


```funl
A = (3, 4, 5)
B = (4, 3, 5)
C = (-5, -12, -13)

def dot( u, v ) = sum( u(i)v(i) | i <- 0:u.>length() )
def cross( u, v ) = (u(1)v(2) - u(2)v(1), u(2)v(0) - u(0)v(2), u(0)v(1) - u(1)v(0) )
def scalarTriple( u, v, w ) = dot( u, cross(v, w) )
def vectorTriple( u, v, w ) = cross( u, cross(v, w) )

println( "A\u00b7B = ${dot(A, B)}" )
println( "A\u00d7B = ${cross(A, B)}" )
println( "A\u00b7(B\u00d7C) = ${scalarTriple(A, B, C)}" )
println( "A\u00d7(B\u00d7C) = ${vectorTriple(A, B, C)}" )
```


```txt

A·B = 49
A×B = (5, 5, -7)
A·(B×C) = 6
A×(B×C) = (-267, 204, -3)

```



## GAP


```gap
DotProduct := function(u, v)
	return u*v;
end;

CrossProduct := function(u, v)
	return [
		u[2]*v[3] - u[3]*v[2],
		u[3]*v[1] - u[1]*v[3],
		u[1]*v[2] - u[2]*v[1] ];
end;

ScalarTripleProduct := function(u, v, w)
	return DotProduct(u, CrossProduct(v, w));
end;

VectorTripleProduct := function(u, v, w)
	return CrossProduct(u, CrossProduct(v, w));
end;

a := [3, 4, 5];
b := [4, 3, 5];
c := [-5, -12, -13];

DotProduct(a, b);
# 49

CrossProduct(a, b);
# [ 5, 5, -7 ]

ScalarTripleProduct(a, b, c);
# 6

# Another way to get it
Determinant([a, b, c]);
# 6

VectorTripleProduct(a, b, c);
# [ -267, 204, -3 ]
```



## GLSL

```glsl

vec3 a = vec3(3, 4, 5),b = vec3(4, 3, 5),c = vec3(-5, -12, -13);

float dotProduct(vec3 a, vec3 b)
{
	return a.x*b.x+a.y*b.y+a.z*b.z;
}

vec3 crossProduct(vec3 a,vec3 b)
{
	vec3 c = vec3(a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y- a.y*b.x);

	return c;
}

float scalarTripleProduct(vec3 a,vec3 b,vec3 c)
{
	return dotProduct(a,crossProduct(b,c));
}

vec3 vectorTripleProduct(vec3 a,vec3 b,vec3 c)
{
	return crossProduct(a,crossProduct(b,c));
}

```


## Go


```go
package main

import "fmt"

type vector struct {
    x, y, z float64
}

var (
    a = vector{3, 4, 5}
    b = vector{4, 3, 5}
    c = vector{-5, -12, -13}
)

func dot(a, b vector) float64 {
    return a.x*b.x + a.y*b.y + a.z*b.z
}

func cross(a, b vector) vector {
    return vector{a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x}
}

func s3(a, b, c vector) float64 {
    return dot(a, cross(b, c))
}

func v3(a, b, c vector) vector {
    return cross(a, cross(b, c))
}

func main() {
    fmt.Println(dot(a, b))
    fmt.Println(cross(a, b))
    fmt.Println(s3(a, b, c))
    fmt.Println(v3(a, b, c))
}
```

Output:

```txt

49
{5 5 -7}
6
{-267 204 -3}

```



## Groovy

Dot Product Solution:

```groovy
def pairwiseOperation = { x, y, Closure binaryOp ->
    assert x && y && x.size() == y.size()
    [x, y].transpose().collect(binaryOp)
}

def pwMult =  pairwiseOperation.rcurry { it[0] * it[1] }

def dotProduct = { x, y ->
    assert x && y && x.size() == y.size()
    pwMult(x, y).sum()
}
```

Cross Product Solution, using scalar operations:

```groovy
def crossProductS = { x, y ->
    assert x && y && x.size() == 3 && y.size() == 3
    [x[1]*y[2] - x[2]*y[1], x[2]*y[0] - x[0]*y[2] , x[0]*y[1] - x[1]*y[0]]
}
```

Cross Product Solution, using "vector" operations:

```groovy
def rotR = {
    assert it && it.size() > 2
    [it[-1]] + it[0..-2]
}

def rotL = {
    assert it && it.size() > 2
    it[1..-1] + [it[0]]
}

def pwSubtr = pairwiseOperation.rcurry { it[0] - it[1] }

def crossProductV = { x, y ->
    assert x && y && x.size() == 3 && y.size() == 3
    pwSubtr(pwMult(rotL(x), rotR(y)), pwMult(rotL(y), rotR(x)))
}
```

Test program (including triple products):

```groovy
def test = { crossProduct ->

    def scalarTripleProduct = { x, y, z ->
        dotProduct(x, crossProduct(y, z))
    }

    def vectorTripleProduct = { x, y, z ->
        crossProduct(x, crossProduct(y, z))
    }

    def a = [3, 4, 5]
    def b = [4, 3, 5]
    def c = [-5, -12, -13]

    println("      a . b = " + dotProduct(a,b))
    println("      a x b = " + crossProduct(a,b))
    println("a . (b x c) = " + scalarTripleProduct(a,b,c))
    println("a x (b x c) = " + vectorTripleProduct(a,b,c))
    println()
}

test(crossProductS)
test(crossProductV)
```

Output:

```txt
      a . b = 49
      a x b = [5, 5, -7]
a . (b x c) = 6
a x (b x c) = [-267, 204, -3]

      a . b = 49
      a x b = [5, 5, -7]
a . (b x c) = 6
a x (b x c) = [-267, 204, -3]
```



## Haskell


```haskell
import Data.Monoid ((<>))

type Vector a = [a]

type Scalar a = a

a, b, c, d :: Vector Int
a = [3, 4, 5]

b = [4, 3, 5]

c = [-5, -12, -13]

d = [3, 4, 5, 6]

dot
  :: (Num t)
  => Vector t -> Vector t -> Scalar t
dot u v
  | length u == length v = sum $ zipWith (*) u v
  | otherwise = error "Dotted Vectors must be of equal dimension."

cross
  :: (Num t)
  => Vector t -> Vector t -> Vector t
cross u v
  | length u == 3 && length v == 3 =
    [ u !! 1 * v !! 2 - u !! 2 * v !! 1
    , u !! 2 * head v - head u * v !! 2
    , head u * v !! 1 - u !! 1 * head v
    ]
  | otherwise = error "Crossed Vectors must both be three dimensional."

scalarTriple
  :: (Num t)
  => Vector t -> Vector t -> Vector t -> Scalar t
scalarTriple q r s = dot q $ cross r s

vectorTriple
  :: (Num t)
  => Vector t -> Vector t -> Vector t -> Vector t
vectorTriple q r s = cross q $ cross r s

main :: IO ()
main =
  mapM_
    putStrLn
    [ "a . b     = " <> show (dot a b)
    , "a x b     = " <> show (cross a b)
    , "a . b x c = " <> show (scalarTriple a b c)
    , "a x b x c = " <> show (vectorTriple a b c)
    , "a . d     = " <> show (dot a d)
    ]
```

Output:
```txt
a . b     = 49
a x b     = [5,5,-7]
a . b x c = 6
a x b x c = [-267,204,-3]
** Exception: Dotted Vectors must be of equal dimension.
a . d     =
```



Or using '''Either''' and '''(>>=)''', rather than '''error''', to pass on intelligible messages:


```haskell
dotProduct
  :: Num a
  => [a] -> [a] -> Either String a
dotProduct xs ys
  | length xs /= length ys =
    Left "Dot product not defined - vectors differ in dimension."
  | otherwise = Right (sum $ zipWith (*) xs ys)

crossProduct
  :: Num a
  => [a] -> [a] -> Either String [a]
crossProduct xs ys
  | 3 /= length xs || 3 /= length ys =
    Left "crossProduct is defined only for 3d vectors."
  | otherwise = Right [x2 * y3 - x3 * y2, x3 * y1 - x1 * y3, x1 * y2 - x2 * y1]
  where
    [x1, x2, x3] = xs
    [y1, y2, y3] = ys

scalarTriple
  :: Num a
  => [a] -> [a] -> [a] -> Either String a
scalarTriple q r s = crossProduct r s >>= dotProduct q

vectorTriple
  :: Num a
  => [a] -> [a] -> [a] -> Either String [a]
vectorTriple q r s = crossProduct r s >>= crossProduct q

-- TEST ---------------------------------------------------
a = [3, 4, 5]

b = [4, 3, 5]

c = [-5, -12, -13]

d = [3, 4, 5, 6]

main :: IO ()
main =
  mapM_ putStrLn $
  zipWith
    (++)
    ["a . b", "a x b", "a . b x c", "a x b x c", "a . d", "a . (b x d)"]
    [ sh $ dotProduct a b
    , sh $ crossProduct a b
    , sh $ scalarTriple a b c
    , sh $ vectorTriple a b c
    , sh $ dotProduct a d
    , sh $ scalarTriple a b d
    ]

sh
  :: Show a
  => Either String a -> String
sh = either (" => " ++) ((" = " ++) . show)
```

```txt
a . b = 49
a x b = [5,5,-7]
a . b x c = 6
a x b x c = [-267,204,-3]
a . d => Dot product not defined - vectors differ in dimension.
a . (b x d) => crossProduct is defined only for 3d vectors.
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
# record type to store a 3D vector
record Vector3D(x, y, z)

# procedure to display vector as a string
procedure toString (vector)
  return "(" || vector.x || ", " || vector.y || ", " || vector.z || ")"
end

procedure dotProduct (a, b)
  return a.x * b.x + a.y * b.y + a.z * b.z
end

procedure crossProduct (a, b)
  x := a.y * b.z - a.z * b.y
  y := a.z * b.x - a.x * b.z
  z := a.x * b.y - a.y * b.x
  return Vector3D(x, y, z)
end

procedure scalarTriple (a, b, c)
  return dotProduct (a, crossProduct (b, c))
end

procedure vectorTriple (a, b, c)
  return crossProduct (a, crossProduct (b, c))
end

# main procedure, to run given test
procedure main ()
  a := Vector3D(3, 4, 5)
  b := Vector3D(4, 3, 5)
  c := Vector3D(-5, -12, -13)

  writes ("A.B : " || toString(a) || "." || toString(b) || " = ")
  write (dotProduct (a, b))
  writes ("AxB : " || toString(a) || "x" || toString(b) || " = ")
  write (toString(crossProduct (a, b)))
  writes ("A.(BxC) : " || toString(a) || ".(" || toString(b) || "x" || toString(c) || ") = ")
  write (scalarTriple (a, b, c))
  writes ("Ax(BxC) : " || toString(a) || "x(" || toString(b) || "x" || toString(c) || ") = ")
  write (toString(vectorTriple (a, b, c)))
end
```

Output:

```txt

A.B : (3, 4, 5).(4, 3, 5) = 49
AxB : (3, 4, 5)x(4, 3, 5) = (5, 5, -7)
A.(BxC) : (3, 4, 5).((4, 3, 5)x(-5, -12, -13)) = 6
Ax(BxC) : (3, 4, 5)x((4, 3, 5)x(-5, -12, -13)) = (-267, 204, -3)

```



## J

Perhaps the most straightforward definition for cross product in J uses rotate multiply and subtract:


```j
cross=: (1&|.@[ * 2&|.@]) - 2&|.@[ * 1&|.@]
```


However, there are other valid approaches. For example, a "generalized approach" based on [[j:Essays/Complete Tensor]]:

```j
CT=: C.!.2 @ (#:i.) @ $~
ip=: +/ .*    NB. inner product
cross=: ] ip CT@#@[ ip [
```


Note that there are a variety of other generalizations have cross products as a part of what they do.

An alternative definition for cross (based on finding the determinant of a 3 by 3 matrix where one row is unit vectors) could be:

```j>cross=: [:
 [: -&.>/ .(*&.>) (<"1=i.3) , ,:&:(<"0)
```


With an implementation of cross product and inner product, the rest of the task becomes trivial:


```j
a=:  3 4 5
b=:  4 3 5
c=: -5 12 13

A=: 0 {:: ]    NB. contents of the first box on the right
B=: 1 {:: ]    NB. contents of the second box on the right
C=: 2 {:: ]    NB. contents of the third box on the right

dotP=: A ip B
crossP=: A cross B
scTriP=: A ip B cross C
veTriP=: A cross B cross C
```

Required example:

```j
   dotP a;b
49
   crossP a;b
5 5 _7
   scTriP a;b;c
6
   veTriP a;b;c
_267 204 _3
```



## Java

All operations which return vectors give vectors containing <code>Double</code>s.

```java5
public class VectorProds{
    public static class Vector3D<T extends Number>{
        private T a, b, c;

        public Vector3D(T a, T b, T c){
            this.a = a;
            this.b = b;
            this.c = c;
        }

        public double dot(Vector3D<?> vec){
            return (a.doubleValue() * vec.a.doubleValue() +
                    b.doubleValue() * vec.b.doubleValue() +
                    c.doubleValue() * vec.c.doubleValue());
        }

        public Vector3D<Double> cross(Vector3D<?> vec){
            Double newA = b.doubleValue()*vec.c.doubleValue() - c.doubleValue()*vec.b.doubleValue();
            Double newB = c.doubleValue()*vec.a.doubleValue() - a.doubleValue()*vec.c.doubleValue();
            Double newC = a.doubleValue()*vec.b.doubleValue() - b.doubleValue()*vec.a.doubleValue();
            return new Vector3D<Double>(newA, newB, newC);
        }

        public double scalTrip(Vector3D<?> vecB, Vector3D<?> vecC){
            return this.dot(vecB.cross(vecC));
        }

        public Vector3D<Double> vecTrip(Vector3D<?> vecB, Vector3D<?> vecC){
            return this.cross(vecB.cross(vecC));
        }

        @Override
        public String toString(){
            return "<" + a.toString() + ", " + b.toString() + ", " + c.toString() + ">";
        }
    }

    public static void main(String[] args){
        Vector3D<Integer> a = new Vector3D<Integer>(3, 4, 5);
        Vector3D<Integer> b = new Vector3D<Integer>(4, 3, 5);
        Vector3D<Integer> c = new Vector3D<Integer>(-5, -12, -13);

        System.out.println(a.dot(b));
        System.out.println(a.cross(b));
        System.out.println(a.scalTrip(b, c));
        System.out.println(a.vecTrip(b, c));
    }
}
```

Output:

```txt
49.0
<5.0, 5.0, -7.0>
6.0
<-267.0, 204.0, -3.0>
```

This solution uses Java SE new Stream API

```Java8
import java.util.Arrays;
import java.util.stream.IntStream;

public class VectorsOp {
	// Vector dot product using Java SE 8 stream abilities
	// the method first create an array of size values,
	// and map the product of each vectors components in a new array (method map())
	// and transform the array to a scalr by summing all elements (method reduce)
	// the method parallel  is there for optimization
	private static int dotProduct(int[] v1, int[] v2,int length) {

	int result = IntStream.range(0, length)
	                           .parallel()
	                            .map( id -> v1[id] * v2[id])
	                            .reduce(0, Integer::sum);

	    return result;
	}

	// Vector Cross product using Java SE 8 stream abilities
	// here we map in a new array where each element is equal to the cross product
	// With Stream is is easier to handle N dimensions vectors
	private static int[] crossProduct(int[] v1, int[] v2,int length) {

		int  result[] = new int[length] ;
		//result[0] = v1[1] * v2[2] - v1[2]*v2[1] ;
		//result[1] = v1[2] * v2[0] - v1[0]*v2[2] ;
		// result[2] = v1[0] * v2[1] - v1[1]*v2[0] ;

		result = IntStream.range(0, length)
			.parallel()
	 		.map( i ->   v1[(i+1)%length] * v2[(i+2)%length] -  v1[(i+2)%length]*v2[(i+1)%length])
	 		.toArray();

		 return result;
	}

	public static void main (String[] args)
	{
	   	int[] vect1 = {3, 4, 5};
	   	int[] vect2 = {4, 3, 5};
	    int[] vect3 = {-5, -12, -13};

	    System.out.println("dot product =:" + dotProduct(vect1,vect2,3));

	    int[] prodvect = new int[3];
	    prodvect = crossProduct(vect1,vect2,3);
	    System.out.println("cross product =:[" + prodvect[0] + ","
	    	                                   + prodvect[1] + ","
	    	                                   + prodvect[2] + "]");

	    prodvect = crossProduct(vect2,vect3,3);
	    System.out.println("scalar product =:" + dotProduct(vect1,prodvect,3));

	    prodvect = crossProduct(vect1,prodvect,3);

	    System.out.println("triple product =:[" + prodvect[0] + ","
	    	                                   + prodvect[1] + ","
	    	                                   + prodvect[2] + "]");

	   }
}
```

result is the same as above , fortunately

```txt
dot product =:49
cross product =:[5,5,-7]
scalar product =:6
triple product =:[-267,204,-3]
```


## JavaScript


### ES5

The <code>dotProduct()</code> function is generic and will create a dot product of any set of vectors provided they are all the same dimension.
The <code>crossProduct()</code> function expects two 3D vectors.

```javascript
function dotProduct() {
  var len = arguments[0] && arguments[0].length;
  var argsLen = arguments.length;
  var i, j = len;
  var prod, sum = 0;

  // If no arguments supplied, return undefined
  if (!len) {
    return;
  }

  // If all vectors not same length, return undefined
  i = argsLen;
  while (i--) {

    if (arguments[i].length != len) {
      return;  // return undefined
    }
  }

  // Sum terms
  while (j--) {
    i = argsLen;
    prod = 1;

    while (i--) {
      prod *= arguments[i][j];
    }
    sum += prod;
  }
  return sum;
}

function crossProduct(a, b) {

  // Check lengths
  if (a.length != 3 || b.length != 3) {
     return;
  }

  return [a[1]*b[2] - a[2]*b[1],
          a[2]*b[0] - a[0]*b[2],
          a[0]*b[1] - a[1]*b[0]];

}

function scalarTripleProduct(a, b, c) {
  return dotProduct(a, crossProduct(b, c));
}

function vectorTripleProduct(a, b, c) {
  return crossProduct(a, crossProduct(b, c));
}

// Run tests
(function () {
  var a = [3, 4, 5];
  var b = [4, 3, 5];
  var c = [-5, -12, -13];

  alert(
    'A . B: ' + dotProduct(a, b) +
    '\n' +
    'A x B: ' + crossProduct(a, b) +
    '\n' +
    'A . (B x C): ' + scalarTripleProduct(a, b, c) +
    '\n' +
    'A x (B x C): ' + vectorTripleProduct(a, b, c)
  );
}());
```

```txt
A . B: 49
A x B: 5,5,-7
A . (B x C): 6
A x (B x C): -267,204,-3
```



### ES6


```javascript
(() => {
    'use strict';

    // dotProduct :: [a] -> [a] -> Either String a
    const dotProduct = xs =>
        // Dot product of two vectors of equal dimension.
        ys => xs.length !== ys.length ? (
            Left('Dot product not defined - vectors differ in dimension.')
        ) : Right(sum(
            zipWith(mul)(Array.from(xs))(Array.from(ys))
        ));

    // crossProduct :: Num a => (a, a, a) -> (a, a, a)
    // Either String -> (a, a, a)
    const crossProduct = xs =>
        // Cross product of two 3D vectors.
        ys => 3 !== xs.length || 3 !== ys.length ? (
            Left('crossProduct is defined only for 3d vectors.')
        ) : Right((() => {
            const [x1, x2, x3] = Array.from(xs);
            const [y1, y2, y3] = Array.from(ys);
            return [
                x2 * y3 - x3 * y2,
                x3 * y1 - x1 * y3,
                x1 * y2 - x2 * y1
            ];
        })());

    // scalarTriple :: Num a => (a, a, a) -> (a, a, a) -> (a, a a) ->
    // Either String -> a
    const scalarTriple = q =>
        // The scalar triple product.
        r => s => bindLR(crossProduct(r)(s))(
            dotProduct(q)
        );

    // vectorTriple :: Num a => (a, a, a) -> (a, a, a) -> (a, a a) ->
    // Either String -> (a, a, a)
    const vectorTriple = q =>
        // The vector triple product.
        r => s => bindLR(crossProduct(r)(s))(
            crossProduct(q)
        );

    // main :: IO ()
    const main = () => {
        // TEST -------------------------------------------
        const
            a = [3, 4, 5],
            b = [4, 3, 5],
            c = [-5, -12, -13],
            d = [3, 4, 5, 6];

        console.log(unlines(
            zipWith(k => f => k + show(
                saturated(f)([a, b, c])
            ))(['a . b', 'a x b', 'a . (b x c)', 'a x (b x c)'])(
                [dotProduct, crossProduct, scalarTriple, vectorTriple]
            )
            .concat([
                'a . d' + show(
                    dotProduct(a)(d)
                ),
                'a . (b x d)' + show(
                    scalarTriple(a)(b)(d)
                )
            ])
        ));
    };


    // GENERIC FUNCTIONS ----------------------------------

    // Left :: a -> Either a b
    const Left = x => ({
        type: 'Either',
        Left: x
    });

    // Right :: b -> Either a b
    const Right = x => ({
        type: 'Either',
        Right: x
    });

    // bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
    const bindLR = m => mf =>
        undefined !== m.Left ? (
            m
        ) : mf(m.Right);

    // either :: (a -> c) -> (b -> c) -> Either a b -> c
    const either = fl => fr => e =>
        'Either' === e.type ? (
            undefined !== e.Left ? (
                fl(e.Left)
            ) : fr(e.Right)
        ) : undefined;

    // identity :: a -> a
    const identity = x => x;

    // mul (*) :: Num a => a -> a -> a
    const mul = a => b => a * b;

    // Curried function -> [Argument] -> a more saturated value
    const saturated = f =>
        // A curried function applied successively to
        // a list of arguments up to, but not beyond,
        // the point of saturation.
        args => 0 < args.length ? (
            args.slice(1).reduce(
                (a, x) => 'function' !== typeof a ? (
                    a
                ) : a(x),
                f(args[0])
            )
        ) : f;

    // show :: Either String a -> String
    const show = x =>
        either(x => ' => ' + x)(
            x => ' = ' + JSON.stringify(x)
        )(x);

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zipWith:: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = f => xs => ys =>
        xs.slice(
            0, Math.min(xs.length, ys.length)
        ).map((x, i) => f(x)(ys[i]));

    // MAIN ---
    return main();
})();
```

```txt
a . b = 49
a x b = [5,5,-7]
a . (b x c) = 6
a x (b x c) = [-267,204,-3]
a . d => Dot product not defined - vectors differ in dimension.
a . (b x d) => crossProduct is defined only for 3d vectors.
```



## jq

The <code>dot_product()</code> function is generic and will create a dot product of any pair of vectors provided they are both the same dimension. The other functions expect 3D vectors.
```jq
def dot_product(a; b):
  reduce range(0;a|length) as $i (0; . + (a[$i] * b[$i]) );

# for 3d vectors
def cross_product(a;b):
  [ a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1]-a[1]*b[0] ];

def scalar_triple_product(a;b;c):
  dot_product(a; cross_product(b; c));

def vector_triple_product(a;b;c):
  cross_product(a; cross_product(b; c));

def main:
  [3, 4, 5] as $a
  | [4, 3, 5] as $b
  | [-5, -12, -13] as $c
  | "a . b = \(dot_product($a; $b))",
    "a x b = [\( cross_product($a; $b) | map(tostring) | join (", ") )]" ,
    "a . (b x c) = \( scalar_triple_product ($a; $b; $c)) )",
    "a x (b x c) = [\( vector_triple_product($a; $b; $c)|map(tostring)|join (", ") )]" ;
```

Output:

```jq
"a . b = 49"
"a x b = [5, 5, -7]"
"a . (b x c) = 6 )"
"a x (b x c) = [-267, 204, -3]"
```



## Julia

Julia provides dot and cross products as built-ins.  It's easy enough to use these to construct the triple products.

```julia
function scalarproduct(a::AbstractVector{T}, b::AbstractVector{T}, c::AbstractVector{T}) where {T<:Number}
    return dot(a, cross(b, c))
end

function vectorproduct(a::AbstractVector{T}, b::AbstractVector{T}, c::AbstractVector{T}) where {T<:Number}
    return cross(a, cross(b, c))
end

const a = [3, 4, 5]
const b = [4, 3, 5]
const c = [-5, -12, -13]

println("Test Vectors:")
@show a b c

println("\nVector Products:")
@show dot(a, b)
@show cross(a, b)
@show scalarproduct(a, b, c)
@show vectorproduct(a, b, c)
```


```txt
Test Vectors:
a = [3, 4, 5]
b = [4, 3, 5]
c = [-5, -12, -13]

Vector Products:
dot(a, b) = 49
cross(a, b) = [5, 5, -7]
scalarproduct(a, b, c) = 6
vectorproduct(a, b, c) = [-267, 204, -3]
```



## Kotlin


```scala
// version 1.1.2

class Vector3D(val x: Double, val y: Double, val z: Double) {
    infix fun dot(v: Vector3D) = x * v.x + y * v.y + z * v.z

    infix fun cross(v: Vector3D) =
        Vector3D(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)

    fun scalarTriple(v: Vector3D, w: Vector3D) = this dot (v cross w)

    fun vectorTriple(v: Vector3D, w: Vector3D) = this cross (v cross w)

    override fun toString() = "($x, $y, $z)"
}

fun main(args: Array<String>) {
    val a = Vector3D(3.0, 4.0, 5.0)
    val b = Vector3D(4.0, 3.0, 5.0)
    val c = Vector3D(-5.0, -12.0, -13.0)
    println("a = $a")
    println("b = $b")
    println("c = $c")
    println()
    println("a . b     = ${a dot b}")
    println("a x b     = ${a cross b}")
    println("a . b x c = ${a.scalarTriple(b, c)}")
    println("a x b x c = ${a.vectorTriple(b, c)}")
}
```


```txt

a = (3.0, 4.0, 5.0)
b = (4.0, 3.0, 5.0)
c = (-5.0, -12.0, -13.0)

a . b     = 49.0
a x b     = (5.0, 5.0, -7.0)
a . b x c = 6.0
a x b x c = (-267.0, 204.0, -3.0)

```



## Liberty BASIC


```lb
    print "Vector products of 3-D vectors"

    print "Dot   product of 3,4,5 and 4,3,5 is "
    print DotProduct(   "3,4,5", "4,3,5")
    print "Cross product of 3,4,5 and 4,3,5 is "
    print CrossProduct$( "3,4,5", "4,3,5")
    print "Scalar triple product of 3,4,5,    4,3,5    -5, -12, -13 is "
    print ScalarTripleProduct( "3,4,5", "4,3,5", "-5, -12, -13")
    print "Vector triple product of 3,4,5,    4,3,5    -5, -12, -13 is "
    print VectorTripleProduct$( "3,4,5", "4,3,5", "-5, -12, -13")


    end

    function DotProduct( i$, j$)
        ix =val( word$( i$, 1, ","))
        iy =val( word$( i$, 2, ","))
        iz =val( word$( i$, 3, ","))
        jx =val( word$( j$, 1, ","))
        jy =val( word$( j$, 2, ","))
        jz =val( word$( j$, 3, ","))
        DotProduct = ix *jx +iy *jy + iz *jz
    end function

    function CrossProduct$( i$, j$)
        ix =val( word$( i$, 1, ","))
        iy =val( word$( i$, 2, ","))
        iz =val( word$( i$, 3, ","))
        jx =val( word$( j$, 1, ","))
        jy =val( word$( j$, 2, ","))
        jz =val( word$( j$, 3, ","))
        cpx =iy *jz -iz *jy
        cpy =iz *jx -ix *jz
        cpz =ix *jy -iy *jx
        CrossProduct$ =str$( cpx); ","; str$( cpy); ","; str$( cpz)
    end function

    function ScalarTripleProduct( i$, j$, k$))
        ScalarTripleProduct =DotProduct( i$, CrossProduct$( j$, k$))
    end function

    function VectorTripleProduct$( i$, j$, k$))
        VectorTripleProduct$ =CrossProduct$( i$, CrossProduct$( j$, k$))
    end function
 END SUB
```



## Lingo

Lingo has a built-in vector data type that supports calculation of both dot and cross products:

```lingo
a = vector(1,2,3)
b = vector(4,5,6)

put a * b
-- 32.0000

put a.dot(b)
-- 32.0000

put a.cross(b)
-- vector( -3.0000, 6.0000, -3.0000 )
```



## Lua


```lua
Vector = {}
function Vector.new( _x, _y, _z )
    return { x=_x, y=_y, z=_z }
end

function Vector.dot( A, B )
    return A.x*B.x + A.y*B.y + A.z*B.z
end

function Vector.cross( A, B )
    return { x = A.y*B.z - A.z*B.y,
             y = A.z*B.x - A.x*B.z,
             z = A.x*B.y - A.y*B.x }
end

function Vector.scalar_triple( A, B, C )
    return Vector.dot( A, Vector.cross( B, C ) )
end

function Vector.vector_triple( A, B, C )
    return Vector.cross( A, Vector.cross( B, C ) )
end


A = Vector.new( 3, 4, 5 )
B = Vector.new( 4, 3, 5 )
C = Vector.new( -5, -12, -13 )

print( Vector.dot( A, B ) )

r = Vector.cross(A, B )
print( r.x, r.y, r.z )

print( Vector.scalar_triple( A, B, C ) )

r = Vector.vector_triple( A, B, C )
print( r.x, r.y, r.z )
```


```txt
49
5	5	-7
6
-267	204	-3
```



## M2000 Interpreter


```M2000 Interpreter

Module checkit {
            class Vector {
                  \\ by default are double
                  a,b,c
                  Property ToString$ {
                        Value {
                            link parent a,b,c to a,b,c
                             value$=format$("({0}, {1}, {2})",a,b,c)
                        }
                  }
                  Operator "==" {
                        read n
                        push .a==n.a and .b==n.b and .c==n.c
                  }
                  Operator Unary {
                        .a-! : .b-! : .c-!
                  }
                  Operator "+"  {
                        Read v2
                        For this, v2 {
                              .a+=..a :.b+=..b:.c+=..c:
                        }
                  }
                  Function Mul(r)  {
                        vv=this
                        for vv {
                              .a*=r:.b*=r:.c*=r
                        }
                        =vv
                  }
                  Function Dot(v2)  {
                        def double sum
                        for  this, v2 {
                            sum=.a*..a+.b*..b+.c*..c
                        }
                        =sum
                  }
                  Operator "*" {
                        Read v2
                        For This, v2 {
                              Push .b*..c-.c*..b
                              Push .c*..a-.a*..c
                              .c<=.a*..b-.b*..a
                              Read .b, .a
                        }
                  }
                  class:
                  module Vector {
                        if match("NNN") then {
                              Read .a,.b,.c
                        }
                  }
            }
            A=Vector(3,4,5)
            B=Vector(4,3,5)
            C=Vector(-5,-12,-13)
            Print "A=";A.toString$
            Print "B=";B.toString$
            Print "C=";C.toString$
            Print "A dot B="; A.dot(B)
            AxB=A*B
            Print "A x B="; AxB.toString$
            Print "A dot (B x C)=";A.dot(B*C)
            AxBxC=A*(B*C)
            Print "A x (B x C)=";AxBxC.toString$
            Def ToString$(a)=a.toString$
            Print "A x (B x C)=";ToString$(A*(B*C))
}
Checkit

```

<pre >
A=(3, 4, 5)
B=(4, 3, 5)
C=(-5, -12, -13)
A dot B=49
A x B=(5, 5, -7)
A dot (B x C)=6
A x (B x C)=(-267, 204, -3)
A x (B x C)=(-267, 204, -3)
</pre >


## Maple


```Maple
with(LinearAlgebra):
A := Vector([3,4,5]):
B := Vector([4,3,5]):
C := Vector([-5,-12,-13]):
>>>A.B;
49
>>>CrossProduct(A,B);
Vector([5, 5, -7])
>>>A.(CrossProduct(B,C));
6
>>>CrossProduct(A,CrossProduct(B,C));
Vector([-267, 204, -3])
```



## Mathematica


```Mathematica
a={3,4,5};
b={4,3,5};
c={-5,-12,-13};
a.b
Cross[a,b]
a.Cross[b,c]
Cross[a,Cross[b,c]]
```

Output

```txt
49
{5,5,-7}
6
{-267,204,-3}
```


=={{header|MATLAB}} / {{header|Octave}}==
Matlab / Octave use double precesion numbers per default, and pi is a builtin constant value. Arbitrary precision is only implemented in some additional toolboxes (e.g. symbolic toolbox).

```MATLAB
%    Create a named function/subroutine/method to compute the dot product of two vectors.
        dot(a,b)
%    Create a function to compute the cross product of two vectors.
        cross(a,b)
%    Optionally create a function to compute the scalar triple product of three vectors.
        dot(a,cross(b,c))
%    Optionally create a function to compute the vector triple product of three vectors.
        cross(a,cross(b,c))
%    Compute and display: a • b
        cross(a,b)
%    Compute and display: a x b
        cross(a,b)
%    Compute and display: a • b x c, the scaler triple product.
        dot(a,cross(b,c))
%    Compute and display: a x b x c, the vector triple product.
        cross(a,cross(b,c))
```


Code for testing:


```txt

A = [ 3.0,  4.0,  5.0]
B = [ 4.0,  3.0,  5.0]
C = [-5.0, -12.0, -13.0]

dot(A,B)
cross(A,B)
dot(A,cross(B,C))
cross(A,cross(B,C))

```

Output:

```txt
>> A = [ 3.0,  4.0,  5.0]
>> B = [ 4.0,  3.0,  5.0]
>> C = [-5.0, -12.0, -13.0]

>> dot(A,B)
ans =  49
>> cross(A,B)
ans =
   5   5  -7
>> dot(A,cross(B,C))
ans =  6
>> cross(A,cross(B,C))
ans =
  -267   204    -3

```



## Mercury

<lang>:- module vector_product.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    A = vector3d(3, 4, 5),
    B = vector3d(4, 3, 5),
    C = vector3d(-5, -12, -13),
    io.format("A . B = %d\n", [i(A `dot_product` B)], !IO),
    io.format("A x B = %s\n", [s(to_string(A `cross_product` B))], !IO),
    io.format("A . (B x C) = %d\n", [i(scalar_triple_product(A, B, C))], !IO),
    io.format("A x (B x C) = %s\n", [s(to_string(vector_triple_product(A, B, C)))], !IO).

:- type vector3d ---> vector3d(int, int, int).

:- func dot_product(vector3d, vector3d) = int.

dot_product(vector3d(A1, A2, A3), vector3d(B1, B2, B3)) =
    A1 * B1 + A2 * B2 + A3 * B3.

:- func cross_product(vector3d, vector3d) = vector3d.

cross_product(vector3d(A1, A2, A3), vector3d(B1, B2, B3)) =
    vector3d(A2 * B3 - A3 * B2, A3 * B1 - A1 * B3, A1 * B2 - A2 * B1).

:- func scalar_triple_product(vector3d, vector3d, vector3d) = int.

scalar_triple_product(A, B, C) = A `dot_product` (B `cross_product` C).

:- func vector_triple_product(vector3d, vector3d, vector3d) = vector3d.

vector_triple_product(A, B, C) = A `cross_product` (B `cross_product` C).

:- func to_string(vector3d) = string.

to_string(vector3d(X, Y, Z)) =
    string.format("(%d, %d, %d)", [i(X), i(Y), i(Z)]).
```



## MiniScript


```MiniScript
vectorA = [3, 4, 5]
vectorB = [4, 3, 5]
vectorC = [-5, -12, -13]

dotProduct = function(x, y)
    return x[0]*y[0] + x[1]*y[1] + x[2]*y[2]
end function

crossProduct = function(x, y)
    return [x[1]*y[2] - x[2]*y[1], x[2]*y[0] - x[0]*y[2], x[0]*y[1] - x[1]*y[0]]
end function

print "Dot Product = " + dotProduct(vectorA, vectorB)
print "Cross Product = " + crossProduct(vectorA, vectorB)
print "Scalar Triple Product = " + dotProduct(vectorA, crossProduct(vectorB,vectorC))
print "Vector Triple Product = " + crossProduct(vectorA, crossProduct(vectorB,vectorC))

```

```txt

Dot Product = 49
Cross Product = [5, 5, -7]
Scalar Triple Product = 6
Vector Triple Product = [-267, 204, -3]

```


=={{header|MK-61/52}}==
<lang>ПП	54	С/П	ПП	66	С/П
ИП0	ИП3	ИП6	П3	->	П0	->	П6
ИП1	ИП4	ИП7	П4	->	П1	->	П7
ИП2	ИП5	ИП8	П5	->	П2	->	П8
ПП	66
ИП6	ИП7	ИП8	П2	->	П1	->	П0
ИП9	ИПA	ИПB	П5	->	П4	->	П3
ПП	54	С/П	ПП	66	С/П
ИП0	ИП3	*	ИП1	ИП4	*	+	ИП2	ИП5	*	+	В/О
ИП1	ИП5	*	ИП2	ИП4	*	-	П9
ИП2	ИП3	*	ИП0	ИП5	*	-	ПA
ИП0	ИП4	*	ИП1	ИП3	*	-	ПB	В/О
```


''Instruction'': Р0 - a<sub>1</sub>, Р1 - a<sub>2</sub>, Р2 - a<sub>3</sub>, Р3 - b<sub>1</sub>, Р4 - b<sub>2</sub>, Р5 - b<sub>3</sub>, Р6 - c<sub>1</sub>, Р7 - c<sub>2</sub>, Р8 - c<sub>3</sub>; В/О С/П.

=={{header|Modula-2}}==

```modula2
MODULE VectorProducts;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteReal(r : REAL);
VAR buf : ARRAY[0..31] OF CHAR;
BEGIN
    RealToStr(r, buf);
    WriteString(buf)
END WriteReal;

TYPE Vector = RECORD
    a,b,c : REAL;
END;

PROCEDURE Dot(u,v : Vector) : REAL;
BEGIN
    RETURN u.a * v.a
         + u.b * v.b
         + u.c * v.c
END Dot;

PROCEDURE Cross(u,v : Vector) : Vector;
BEGIN
    RETURN Vector{
        u.b*v.c - u.c*v.b,
        u.c*v.a - u.a*v.c,
        u.a*v.b - u.b*v.a
    }
END Cross;

PROCEDURE ScalarTriple(u,v,w : Vector) : REAL;
BEGIN
    RETURN Dot(u, Cross(v, w))
END ScalarTriple;

PROCEDURE VectorTriple(u,v,w : Vector) : Vector;
BEGIN
    RETURN Cross(u, Cross(v, w))
END VectorTriple;

PROCEDURE WriteVector(v : Vector);
BEGIN
    WriteString("<");
    WriteReal(v.a);
    WriteString(", ");
    WriteReal(v.b);
    WriteString(", ");
    WriteReal(v.c);
    WriteString(">")
END WriteVector;

VAR a,b,c : Vector;
BEGIN
    a := Vector{3.0, 4.0, 5.0};
    b := Vector{4.0, 3.0, 5.0};
    c := Vector{-5.0, -12.0, -13.0};

    WriteVector(a);
    WriteString(" dot ");
    WriteVector(b);
    WriteString(" = ");
    WriteReal(Dot(a,b));
    WriteLn;

    WriteVector(a);
    WriteString(" cross ");
    WriteVector(b);
    WriteString(" = ");
    WriteVector(Cross(a,b));
    WriteLn;

    WriteVector(a);
    WriteString(" cross (");
    WriteVector(b);
    WriteString(" cross ");
    WriteVector(c);
    WriteString(") = ");
    WriteVector(VectorTriple(a,b,c));
    WriteLn;

    ReadChar
END VectorProducts.
```



## Nemerle


```Nemerle
using System.Console;

module VectorProducts3d
{
    Dot(x : int * int * int, y : int * int * int) : int
    {
        def (x1, x2, x3) = x;
        def (y1, y2, y3) = y;
        (x1 * y1) + (x2 * y2) + (x3 * y3)
    }

    Cross(x : int * int * int, y : int * int * int) : int * int * int
    {
        def (x1, x2, x3) = x;
        def (y1, y2, y3) = y;
        ((x2 * y3 - x3 * y2), (x3 * y1 - x1 * y3), (x1 * y2 - x2 * y1))
    }

    ScalarTriple(a : int * int * int, b : int * int * int, c : int * int * int) : int
    {
        Dot(a, Cross(b, c))
    }

    VectorTriple(a : int * int * int, b : int * int * int, c : int * int * int) : int * int * int
    {
        Cross(a, Cross(b, c))
    }

    Main() : void
    {
        def a = (3, 4, 5); def b = (4, 3, 5); def c = (-5, -12, -13);
        WriteLine(Dot(a, b)); WriteLine(Cross(a, b));
        WriteLine(ScalarTriple(a, b, c));
        WriteLine(VectorTriple(a, b, c));
    }
}
```

Outputs

```txt
49
(5, 5, -7)
6
(-267, 204, -3)
```



## Never


```fsharp
func printv(a[d] : float) -> int {
    prints("[" + a[0] + ", " + a[1] + ", " + a[2] + "]\n");
    0
}

func dot(a[d1] : float, b[d2] : float) -> float {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

func cross(a[d1] : float, b[d2] : float) -> [_] : float {
    [ a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] ] : float
}

func scalar_triple(a[d1] : float, b[d2] : float, c[d3] : float) -> float {
    dot(a, cross(b, c))
}

func vector_triple(a[d1] : float, b[d2] : float, c[d3] : float) -> [_] : float {
    cross(a, cross(b, c))
}

func main() -> int {
    var a = [ 3.0, 4.0, 5.0 ] : float;
    var b = [ 4.0, 3.0, 5.0 ] : float;
    var c = [ -5.0, -12.0, -13.0 ] : float;

    printv(a);
    printv(b);
    printv(c);
    printf(dot(a, b));
    printv(cross(a, b));
    printf(scalar_triple(a, b, c));
    printv(vector_triple(a, b, c));
    0
}

```

Output:

```txt

[3.00, 4.00, 5.00]
[4.00, 3.00, 5.00]
[-5.00, -12.00, -13.00]
49.00
[5.00, 5.00, -7.00]
6.00
[-267.00, 204.00, -3.00]

```



## Nim


```nim
import strutils

type Vector3 = array[1..3, float]

proc `$`(a: Vector3): string =
  result = "["

  for i, x in a:
    if i > a.low:
      result.add ", "
    result.add formatFloat(x, precision = 0)

  result.add "]"

proc `~⨯`(a, b: Vector3): Vector3 =
  result = [a[2]*b[3] - a[3]*b[2], a[3]*b[1] - a[1]*b[3], a[1]*b[2] - a[2]*b[1]]

proc `~•`[T](a, b: T): float =
  for i in a.low..a.high:
    result += a[i] * b[i]

proc scalartrip(a, b, c: Vector3): float = a ~• (b ~⨯ c)

proc vectortrip(a, b, c: Vector3): Vector3 = a ~⨯ (b ~⨯ c)

let
  a = [3.0, 4.0, 5.0]
  b = [4.0, 3.0, 5.0]
  c = [-5.0, -12.0, -13.0]
echo "a ⨯ b = ", a ~⨯ b
echo "a • b = ", (a ~• b).formatFloat(precision = 0)
echo "a . (b ⨯ c) = ", (scalartrip(a, b, c)).formatFloat(precision = 0)
echo "a ⨯ (b ⨯ c) = ", vectortrip(a, b, c)
```

Output:

```txt
a ⨯ b = [5, 5, -7]
a • b = 49
a . (b ⨯ c) = 6
a ⨯ (b ⨯ c) = [-267, 204, -3]
```



## Objeck


```objeck
﻿bundle Default {
  class VectorProduct {
    function : Main(args : String[]) ~ Nil {
      a := Vector3D->New(3.0, 4.0, 5.0);
      b := Vector3D->New(4.0, 3.0, 5.0);
      c := Vector3D->New(-5.0, -12.0, -13.0);

      a->Dot(b)->Print();
      a->Cross(b)->Print();
      a->ScaleTrip(b, c)->Print();
      a->VectorTrip(b, c)->Print();
    }
  }

  class Vector3D {
    @a : Float;
    @b : Float;
    @c : Float;

    New(a : Float, b : Float, c : Float) {
      @a := a;
      @b := b;
      @c := c;
    }

    method :  GetA() ~ Float {
      return @a;
    }

    method : GetB() ~ Float {
      return @b;
    }

    method : GetC() ~ Float {
      return @c;
    }

    method : public : Dot(vec : Vector3D) ~ Float {
      return @a * vec->GetA() + @b * vec->GetB() + @c * vec->GetC();
    }

    method : public : Cross(vec : Vector3D) ~ Vector3D {
      newA := @b * vec->GetC() - @c * vec->GetB();
      newB := @c * vec->GetA() - @a * vec->GetC();
      newC := @a * vec->GetB() - @b * vec->GetA();

      return Vector3D->New(newA, newB, newC);
    }

    method : public : ScaleTrip(vec_b: Vector3D, vec_c : Vector3D) ~ Float {
      return Dot(vec_b->Cross(vec_c));
    }

    method : public : Print() ~ Nil {
      IO.Console->Print('<')->Print(@a)->Print(" ,")
        ->Print(@b)->Print(", ")->Print(@c)->PrintLine('>');
    }

    method : public : VectorTrip(vec_b: Vector3D, vec_c : Vector3D) ~ Vector3D {
      return Cross(vec_b->Cross(vec_c));
    }
  }
}

```


Output:

```txt

49<5 ,5, -7>
6<-267 ,204, -3>

```



## OCaml


```ocaml
let a = (3.0, 4.0, 5.0)
let b = (4.0, 3.0, 5.0)
let c = (-5.0, -12.0, -13.0)

let string_of_vector (x,y,z) =
  Printf.sprintf "(%g, %g, %g)" x y z

let dot (a1, a2, a3) (b1, b2, b3) =
  (a1 *. b1) +. (a2 *. b2) +. (a3 *. b3)

let cross (a1, a2, a3) (b1, b2, b3) =
  (a2 *. b3 -. a3 *. b2,
   a3 *. b1 -. a1 *. b3,
   a1 *. b2 -. a2 *. b1)

let scalar_triple a b c =
  dot a (cross b c)

let vector_triple a b c =
  cross a (cross b c)

let () =
  Printf.printf "a: %s\n" (string_of_vector a);
  Printf.printf "b: %s\n" (string_of_vector b);
  Printf.printf "c: %s\n" (string_of_vector c);
  Printf.printf "a . b = %g\n" (dot a b);
  Printf.printf "a x b = %s\n" (string_of_vector (cross a b));
  Printf.printf "a . (b x c) = %g\n" (scalar_triple a b c);
  Printf.printf "a x (b x c) = %s\n" (string_of_vector (vector_triple a b c));
;;
```


outputs:


```txt
a: (3, 4, 5)
b: (4, 3, 5)
c: (-5, -12, -13)
a . b = 49
a x b = (5, 5, -7)
a . (b x c) = 6
a x (b x c) = (-267, 204, -3)
```



## Octave

Octave handles naturally vectors / matrices.

```octave
a = [3, 4, 5];
b = [4, 3, 5];
c = [-5, -12, -13];

function r = s3prod(a, b, c)
  r = dot(a, cross(b, c));
endfunction

function r = v3prod(a, b, c)
  r = cross(a, cross(b, c));
endfunction

% 49
dot(a, b)
% or matrix-multiplication between row and column vectors
a * b'

% 5 5 -7
cross(a, b) % only for 3d-vectors

% 6
s3prod(a, b, c)

% -267 204 -3
v3prod(a, b, c)
```



## ooRexx


```ooRexx

a = .vector~new(3, 4, 5);
b = .vector~new(4, 3, 5);
c = .vector~new(-5, -12, -13);

say a~dot(b)
say a~cross(b)
say a~scalarTriple(b, c)
say a~vectorTriple(b, c)


::class vector
::method init
  expose x y z
  use arg x, y, z

::attribute x get
::attribute y get
::attribute z get

-- dot product operation
::method dot
  expose x y z
  use strict arg other

  return x * other~x + y * other~y + z * other~z

-- cross product operation
::method cross
  expose x y z
  use strict arg other

  newX = y * other~z - z * other~y
  newY = z * other~x - x * other~z
  newZ = x * other~y - y * other~x
  return self~class~new(newX, newY, newZ)

-- scalar triple product
::method scalarTriple
  use strict arg vectorB, vectorC
  return self~dot(vectorB~cross(vectorC))

-- vector triple product
::method vectorTriple
  use strict arg vectorB, vectorC
  return self~cross(vectorB~cross(vectorC))

::method string
  expose x y z
  return "<"||x", "y", "z">"

```

Output:

```txt

49
<5, 5, -7>
6
<-267, 204, -3>

```



## PARI/GP


```parigp
dot(u,v)={
  sum(i=1,#u,u[i]*v[i])
};
cross(u,v)={
  [u[2]*v[3] - u[3]*v[2], u[3]*v[1] - u[1]*v[3], u[1]*v[2] - u[2]*v[1]]
};
striple(a,b,c)={
  dot(a,cross(b,c))
};
vtriple(a,b,c)={
  cross(a,cross(b,c))
};

a = [3,4,5]; b = [4,3,5]; c = [-5,-12,-13];
dot(a,b)
cross(a,b)
striple(a,b,c)
vtriple(a,b,c)
```

Output:

```txt
49
[5, 5, -7]
6
[-267, 204, -3]
```



## Pascal


```pascal
Program VectorProduct (output);

type
  Tvector = record
    x, y, z: double
  end;

function dotProduct(a, b: Tvector): double;
begin
  dotProduct := a.x*b.x + a.y*b.y + a.z*b.z;
end;

function crossProduct(a, b: Tvector): Tvector;
begin
  crossProduct.x := a.y*b.z - a.z*b.y;
  crossProduct.y := a.z*b.x - a.x*b.z;
  crossProduct.z := a.x*b.y - a.y*b.x;
end;

function scalarTripleProduct(a, b, c: Tvector): double;
begin
  scalarTripleProduct := dotProduct(a, crossProduct(b, c));
end;

function vectorTripleProduct(a, b, c: Tvector): Tvector;
begin
  vectorTripleProduct := crossProduct(a, crossProduct(b, c));
end;

procedure printVector(a: Tvector);
begin
  writeln(a.x:15:8, a.y:15:8, a.z:15:8);
end;

var
  a: Tvector = (x: 3; y:  4; z:  5);
  b: Tvector = (x: 4; y:  3; z:  5);
  c: Tvector = (x:-5; y:-12; z:-13);

begin
  write('a: '); printVector(a);
  write('b: '); printVector(b);
  write('c: '); printVector(c);
  writeln('a . b: ', dotProduct(a,b):15:8);
  write('a x b: '); printVector(crossProduct(a,b));
  writeln('a . (b x c): ', scalarTripleProduct(a,b,c):15:8);
  write('a x (b x c): '); printVector(vectorTripleProduct(a,b,c));
end.
```

Output:

```txt
a:      3.00000000     4.00000000     5.00000000
b:      4.00000000     3.00000000     5.00000000
c:     -5.00000000   -12.00000000   -13.00000000
a . b:     49.00000000
a x b:      5.00000000     5.00000000    -7.00000000
a . (b x c):      6.00000000
a x (b x c):   -267.00000000   204.00000000    -3.00000000

```



## Perl


```Perl
package Vector;
use List::Util 'sum';
use List::MoreUtils 'pairwise';

sub new { shift; bless [@_] }

use overload (
        '""'    => sub { "(@{+shift})" },
        '&'     => sub { sum pairwise { $a * $b } @{+shift}, @{+shift} },
        '^'     => sub {
                                my @a = @{+shift};
                                my @b = @{+shift};
                                bless [ $a[1]*$b[2] - $a[2]*$b[1],
                                        $a[2]*$b[0] - $a[0]*$b[2],
                                        $a[0]*$b[1] - $a[1]*$b[0] ]
                        },
);

package main;
my $a = Vector->new(3, 4, 5);
my $b = Vector->new(4, 3, 5);
my $c = Vector->new(-5, -12, -13);

print "a = $a b = $b c = $c\n";
print "$a . $b = ", $a & $b, "\n";
print "$a x $b = ", $a ^ $b, "\n";
print "$a . ($b x $c) = ", $a & ($b ^ $c), "\n";
print "$a x ($b x $c) = ", $a ^ ($b ^ $c), "\n";
```


Output:
```txt
a = (3 4 5) b = (4 3 5) c = (-5 -12 -13)
(3 4 5) . (4 3 5) = 49
(3 4 5) x (4 3 5) = (5 5 -7)
(3 4 5) . ((4 3 5) x (-5 -12 -13)) = 6
(3 4 5) x ((4 3 5) x (-5 -12 -13)) = (-267 204 -3)
```



## Perl 6

```perl6
sub infix:<⋅> { [+] @^a »*« @^b }

sub infix:<⨯>([$a1, $a2, $a3], [$b1, $b2, $b3]) {
    [ $a2*$b3 - $a3*$b2,
      $a3*$b1 - $a1*$b3,
      $a1*$b2 - $a2*$b1 ];
}

sub scalar-triple-product { @^a ⋅ (@^b ⨯ @^c) }
sub vector-triple-product { @^a ⨯ (@^b ⨯ @^c) }

my @a = <3 4 5>;
my @b = <4 3 5>;
my @c = <-5 -12 -13>;

say (:@a, :@b, :@c);
say "a ⋅ b = { @a ⋅ @b }";
say "a ⨯ b = <{ @a ⨯ @b }>";
say "a ⋅ (b ⨯ c) = { scalar-triple-product(@a, @b, @c) }";
say "a ⨯ (b ⨯ c) = <{ vector-triple-product(@a, @b, @c) }>";
```

```txt
("a" => ["3", "4", "5"], "b" => ["4", "3", "5"], "c" => ["-5", "-12", "-13"])
a ⋅ b = 49
a ⨯ b = <5 5 -7>
a ⋅ (b ⨯ c) = 6
a ⨯ (b ⨯ c) = <-267 204 -3>
```



## Phix


```Phix
function dot_product(sequence a, b)
    return sum(sq_mul(a,b))
end function

function cross_product(sequence a, b)
integer {a1,a2,a3} = a, {b1,b2,b3} = b
    return {a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1}
end function

function scalar_triple_product(sequence a, b, c)
    return dot_product(a,cross_product(b,c))
end function

function vector_triple_product(sequence a, b, c)
    return cross_product(a,cross_product(b,c))
end function

constant a = {3, 4, 5}, b = {4, 3, 5}, c = {-5, -12, -13}

puts(1,"      a . b = ")    ?dot_product(a,b)
puts(1,"      a x b = ")    ?cross_product(a,b)
puts(1,"a . (b x c) = ")    ?scalar_triple_product(a,b,c)
puts(1,"a x (b x c) = ")    ?vector_triple_product(a,b,c)
```

```txt

      a . b = 49
      a x b = {5,5,-7}
a . (b x c) = 6
a x (b x c) = {-267,204,-3}

```



## PHP


```PHP
<?php

class Vector
{
	private $values;

	public function setValues(array $values)
	{
		if (count($values) != 3)
			throw new Exception('Values must contain exactly 3 values');
		foreach ($values as $value)
			if (!is_int($value) && !is_float($value))
				throw new Exception('Value "' . $value . '" has an invalid type');
		$this->values = $values;
	}

	public function getValues()
	{
		if ($this->values == null)
			$this->setValues(array (
					0,
					0,
					0
			));
		return $this->values;
	}

	public function Vector(array $values)
	{
		$this->setValues($values);
	}

	public static function dotProduct(Vector $va, Vector $vb)
	{
		$a = $va->getValues();
		$b = $vb->getValues();
		return ($a[0] * $b[0]) + ($a[1] * $b[1]) + ($a[2] * $b[2]);
	}

	public static function crossProduct(Vector $va, Vector $vb)
	{
		$a = $va->getValues();
		$b = $vb->getValues();
		return new Vector(array (
				($a[1] * $b[2]) - ($a[2] * $b[1]),
				($a[2] * $b[0]) - ($a[0] * $b[2]),
				($a[0] * $b[1]) - ($a[1] * $b[0])
		));
	}

	public static function scalarTripleProduct(Vector $va, Vector $vb, Vector $vc)
	{
		return self::dotProduct($va, self::crossProduct($vb, $vc));
	}

	public static function vectorTrippleProduct(Vector $va, Vector $vb, Vector $vc)
	{
		return self::crossProduct($va, self::crossProduct($vb, $vc));
	}
}

class Program
{

	public function Program()
	{
		$a = array (
				3,
				4,
				5
		);
		$b = array (
				4,
				3,
				5
		);
		$c = array (
				-5,
				-12,
				-13
		);
		$va = new Vector($a);
		$vb = new Vector($b);
		$vc = new Vector($c);

		$result1 = Vector::dotProduct($va, $vb);
		$result2 = Vector::crossProduct($va, $vb)->getValues();
		$result3 = Vector::scalarTripleProduct($va, $vb, $vc);
		$result4 = Vector::vectorTrippleProduct($va, $vb, $vc)->getValues();

		printf("\n");
		printf("A = (%0.2f, %0.2f, %0.2f)\n", $a[0], $a[1], $a[2]);
		printf("B = (%0.2f, %0.2f, %0.2f)\n", $b[0], $b[1], $b[2]);
		printf("C = (%0.2f, %0.2f, %0.2f)\n", $c[0], $c[1], $c[2]);
		printf("\n");
		printf("A · B = %0.2f\n", $result1);
		printf("A × B = (%0.2f, %0.2f, %0.2f)\n", $result2[0], $result2[1], $result2[2]);
		printf("A · (B × C) = %0.2f\n", $result3);
		printf("A × (B × C) =(%0.2f, %0.2f, %0.2f)\n", $result4[0], $result4[1], $result4[2]);
	}
}

new Program();
?>

```


Output:

```txt


A = (3.00, 4.00, 5.00)
B = (4.00, 3.00, 5.00)
C = (-5.00, -12.00, -13.00)

A · B = 49.00
A × B = (5.00, 5.00, -7.00)
A · (B × C) = 6.00
A × (B × C) =(-267.00, 204.00, -3.00)

```



## PicoLisp


```PicoLisp
(de dotProduct (A B)
   (sum * A B) )

(de crossProduct (A B)
   (list
      (- (* (cadr A) (caddr B)) (* (caddr A) (cadr B)))
      (- (* (caddr A) (car B)) (* (car A) (caddr B)))
      (- (* (car A) (cadr B)) (* (cadr A) (car B))) ) )

(de scalarTriple (A B C)
   (dotProduct A (crossProduct B C)) )

(de vectorTriple (A B C)
   (crossProduct A (crossProduct B C)) )
```

Test:

```txt
(setq
   A ( 3   4   5)
   B ( 4   3   5)
   C (-5 -12 -13) )

: (dotProduct A B)
-> 49

: (crossProduct A B)
-> (5 5 -7)

: (scalarTriple A B C)
-> 6

: (vectorTriple A B C)
-> (-267 204 -3)
```



## PL/I


```PL/I
/* dot product, cross product, etc.                  4 June 2011 */

test_products: procedure options (main);

   declare a(3) fixed initial (3, 4, 5);
   declare b(3) fixed initial (4, 3, 5);
   declare c(3) fixed initial (-5, -12, -13);
   declare e(3) fixed;

   put skip list ('a . b =', dot_product(a, b));
   call cross_product(a, b, e);  put skip list ('a x b =', e);
   put skip list ('a . (b x c) =',  scalar_triple_product(a, b, c));
   call vector_triple_product(a, b, c, e); put skip list ('a x (b x c) =', e);


dot_product: procedure (a, b) returns (fixed);
   declare (a, b) (*) fixed;
   return (sum(a*b));
end dot_product;

cross_product: procedure (a, b, c);
   declare (a, b, c) (*) fixed;
   c(1) = a(2)*b(3) - a(3)*b(2);
   c(2) = a(3)*b(1) - a(1)*b(3);
   c(3) = a(1)*b(2) - a(2)*b(1);
end cross_product;

scalar_triple_product: procedure (a, b, c) returns (fixed);
   declare (a, b, c)(*) fixed;
   declare t(hbound(a, 1)) fixed;
   call cross_product(b, c, t);
   return (dot_product(a, t));
end scalar_triple_product;

vector_triple_product: procedure (a, b, c, e);
   declare (a, b, c, e)(*) fixed;
   declare t(hbound(a,1))  fixed;
   call cross_product(b, c, t);
   call cross_product(a, t, e);
end vector_triple_product;

end test_products;
```

Results:

```txt

a . b =                       49
a x b =                        5                       5                      -7
a . (b x c) =                  6
a x (b x c) =               -267                     204                      -3

```



```PL/I
/* This version uses the ability of PL/I to return arrays. */

/* dot product, cross product, etc.                  6 June 2011 */

test_products: procedure options (main);
   define structure 1 vector, 2 vec(3) fixed;
   declare (a, b, c) type(vector);

   a.vec(1) =  3; a.vec(2) =   4; a.vec(3) =   5;
   b.vec(1) =  4; b.vec(2) =   3; b.vec(3) =   5;
   c.vec(1) = -5; c.vec(2) = -12; c.vec(3) = -13;

   put skip list ('a . b =',       dot_product  (a, b) );
   put skip list ('a x b =',       cross_product(a, b).vec);
   put skip list ('a . (b x c) =', scalar_triple_product(a, b, c) );
   put skip list ('a x (b x c) =', vector_triple_product(a, b, c).vec);


dot_product: procedure (a, b) returns (fixed);
   declare (a, b) type(vector);
   return (sum(a.vec*b.vec));
end dot_product;

cross_product: procedure (a, b) returns (type(vector));
   declare (a, b) type(vector);
   declare c type vector;
   c.vec(1) = a.vec(2)*b.vec(3) - a.vec(3)*b.vec(2);
   c.vec(2) = a.vec(3)*b.vec(1) - a.vec(1)*b.vec(3);
   c.vec(3) = a.vec(1)*b.vec(2) - a.vec(2)*b.vec(1);
   return (c);
end cross_product;

scalar_triple_product: procedure (a, b, c) returns (fixed);
   declare (a, b, c) type(vector);
   declare t type (vector);
   t =  cross_product(b, c);
   return (dot_product(a, t));
end scalar_triple_product;

vector_triple_product: procedure (a, b, c) returns (type(vector));
   declare (a, b, c) type(vector);
   declare (t, e) type (vector);
   t = cross_product(b, c);
   e = cross_product(a, t);
   return (e);
end vector_triple_product;

end test_products;
```

The output is:

```txt

a . b =                       49
a x b =                        5                       5                      -7
a . (b x c) =                  6
a x (b x c) =               -267                     204                      -3

```



## PowerShell


```PowerShell

function dot-product($a,$b) {
    $a[0]*$b[0] +  $a[1]*$b[1] +  $a[2]*$b[2]
}

function cross-product($a,$b) {
    $v1 = $a[1]*$b[2] - $a[2]*$b[1]
    $v2 = $a[2]*$b[0] - $a[0]*$b[2]
    $v3 = $a[0]*$b[1] - $a[1]*$b[0]
    @($v1,$v2,$v3)
}

function scalar-triple-product($a,$b,$c) {
    dot-product $a (cross-product $b $c)
}

function vector-triple-product($a,$b) {
    cross-product $a (cross-product $b $c)
}

$a = @(3, 4, 5)
$b = @(4, 3, 5)
$c = @(-5, -12, -13)

"a.b = $(dot-product $a $b)"
"axb = $(cross-product $a $b)"
"a.(bxc) = $(scalar-triple-product $a $b $c)"
"ax(bxc) = $(vector-triple-product $a $b $c)"

```

<b>Output:</b>

```txt

a.b = 49
axb = 5 5 -7
a.(bxc) = 6
ax(bxc) = -267 204 -3

```



## Prolog

Works with SWI-Prolog.

```prolog

dot_product([A1, A2, A3], [B1, B2, B3], Ans) :-
    Ans is A1 * B1 + A2 * B2 + A3 * B3.

cross_product([A1, A2, A3], [B1, B2, B3], Ans) :-
    T1 is A2 * B3 - A3 * B2,
    T2 is A3 * B1 - A1 * B3,
    T3 is A1 * B2 - A2 * B1,
    Ans = [T1, T2, T3].

scala_triple(A, B, C, Ans) :-
    cross_product(B, C, Temp),
    dot_product(A, Temp, Ans).

vector_triple(A, B, C, Ans) :-
    cross_product(B, C, Temp),
    cross_product(A, Temp, Ans).

```

Output:

```txt

?- dot_product([3.0, 4.0, 5.0], [4.0, 3.0, 5.0], Ans).
Ans = 49.0.

?- cross_product([3.0, 4.0, 5.0], [4.0, 3.0, 5.0], Ans).
Ans = [5.0, 5.0, -7.0].

?- scala_triple([3.0, 4.0, 5.0], [4.0, 3.0, 5.0], [-5.0, -12.0, -13.0], Ans).
Ans = 6.0.

?- vector_triple([3.0, 4.0, 5.0], [4.0, 3.0, 5.0], [-5.0, -12.0, -13.0], Ans).
Ans = [-267.0, 204.0, -3.0].

```



## PureBasic


```PureBasic
Structure vector
  x.f
  y.f
  z.f
EndStructure

;convert vector to a string for display
Procedure.s toString(*v.vector)
  ProcedureReturn "[" + StrF(*v\x, 2) + ", " + StrF(*v\y, 2) + ", " + StrF(*v\z, 2) + "]"
EndProcedure

Procedure.f dotProduct(*a.vector, *b.vector)
  ProcedureReturn *a\x * *b\x + *a\y * *b\y + *a\z * *b\z
EndProcedure

Procedure crossProduct(*a.vector, *b.vector, *r.vector)
  *r\x = *a\y * *b\z - *a\z * *b\y
  *r\y = *a\z * *b\x - *a\x * *b\z
  *r\z = *a\x * *b\y - *a\y * *b\x
EndProcedure

Procedure.f scalarTriple(*a.vector, *b.vector, *c.vector)
  Protected r.vector
  crossProduct(*b, *c, r)
  ProcedureReturn dotProduct(*a, r)
EndProcedure

Procedure vectorTriple(*a.vector, *b.vector, *c.vector, *r.vector)
  Protected r.vector
  crossProduct(*b, *c, r)
  crossProduct(*a, r, *r)
EndProcedure

If OpenConsole()
  Define.vector a, b, c, r
  a\x = 3: a\y = 4: a\z = 5
  b\x = 4: b\y = 3: b\z = 5
  c\x = -5: c\y = -12: c\z = -13

  PrintN("a = " + toString(a) + ", b = " + toString(b) + ", c = " + toString(c))
  PrintN("a . b = " + StrF(dotProduct(a, b), 2))
  crossProduct(a, b, r)
  PrintN("a x b = " + toString(r))
  PrintN("a . b x c  = " + StrF(scalarTriple(a, b, c), 2))
  vectorTriple(a, b, c, r)
  PrintN("a x b x c = " + toString(r))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
a = [3.00, 4.00, 5.00], b = [4.00, 3.00, 5.00], c = [-5.00, -12.00, -13.00]
a . b = 49.00
a x b = [5.00, 5.00, -7.00]
a . b x c  = 6.00
a x b x c = [-267.00, 204.00, -3.00]
```



## Python

The solution is in the form of an [[Executable library]].

```python
def crossp(a, b):
    '''Cross product of two 3D vectors'''
    assert len(a) == len(b) == 3, 'For 3D vectors only'
    a1, a2, a3 = a
    b1, b2, b3 = b
    return (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

def dotp(a,b):
    '''Dot product of two eqi-dimensioned vectors'''
    assert len(a) == len(b), 'Vector sizes must match'
    return sum(aterm * bterm for aterm,bterm in zip(a, b))

def scalartriplep(a, b, c):
    '''Scalar triple product of three vectors: "a . (b x c)"'''
    return dotp(a, crossp(b, c))

def vectortriplep(a, b, c):
    '''Vector triple product of three vectors: "a x (b x c)"'''
    return crossp(a, crossp(b, c))

if __name__ == '__main__':
    a, b, c = (3, 4, 5), (4, 3, 5), (-5, -12, -13)
    print("a = %r;  b = %r;  c = %r" % (a, b, c))
    print("a . b = %r" % dotp(a,b))
    print("a x b = %r"  % (crossp(a,b),))
    print("a . (b x c) = %r" % scalartriplep(a, b, c))
    print("a x (b x c) = %r" % (vectortriplep(a, b, c),))
```


```txt
a = (3, 4, 5);  b = (4, 3, 5);  c = (-5, -12, -13)
a . b = 49
a x b = (5, 5, -7)
a . (b x c) = 6
a x (b x c) = (-267, 204, -3)
```


;Note:
The popular [http://numpy.scipy.org/ numpy] package has functions for dot and cross products.


## R


```rsplus
#
### =========================================================

# Vector products
# R implementation
#
### =========================================================


a <- c(3, 4, 5)
b <- c(4, 3, 5)
c <- c(-5, -12, -13)

#---------------------------------------------------------------
# Dot product
#---------------------------------------------------------------

dotp <- function(x, y) {
  if (length(x) == length(y)) {
    sum(x*y)
  }
}

#---------------------------------------------------------------
# Cross product
#---------------------------------------------------------------

crossp <- function(x, y) {
  if (length(x) == 3 && length(y) == 3) {
    c(x[2]*y[3] - x[3]*y[2], x[3]*y[1] - x[1]*y[3], x[1]*y[2] - x[2]*y[1])
  }
}

#---------------------------------------------------------------
# Scalar triple product
#---------------------------------------------------------------

scalartriplep <- function(x, y, z) {
  if (length(x) == 3 && length(y) == 3 && length(z) == 3) {
    dotp(x, crossp(y, z))
  }
}

#---------------------------------------------------------------
# Vector triple product
#---------------------------------------------------------------

vectortriplep <- function(x, y, z) {
  if (length(x) == 3 && length(y) == 3 && length(z) == 3) {
    crosssp(x, crossp(y, z))
  }
}

#---------------------------------------------------------------
# Compute and print
#---------------------------------------------------------------

cat("a . b =", dotp(a, b))
cat("a x b =", crossp(a, b))
cat("a . (b x c) =", scalartriplep(a, b, c))
cat("a x (b x c) =", vectortriplep(a, b, c))
```


```txt
a . b = 49
a x b = 5 5 -7
a . (b x c) = 6
a x (b x c) = -267 204 -3
```


'''Note:''' R has built-in functions for vector and matrix multiplications. Examples: "crossprod", %*% for inner and %o% for outer product.


## Racket



```Racket

#lang racket

(define (dot-product X Y)
  (for/sum ([x (in-vector X)] [y (in-vector Y)]) (* x y)))

(define (cross-product X Y)
  (define len (vector-length X))
  (for/vector ([n len])
    (define (ref V i) (vector-ref V (modulo (+ n i) len)))
    (- (* (ref X 1) (ref Y 2)) (* (ref X 2) (ref Y 1)))))

(define (scalar-triple-product X Y Z)
  (dot-product X (cross-product Y Z)))

(define (vector-triple-product X Y Z)
  (cross-product X (cross-product Y Z)))

(define A '#(3 4 5))
(define B '#(4 3 5))
(define C '#(-5 -12 -13))

(printf "A = ~s\n" A)
(printf "B = ~s\n" B)
(printf "C = ~s\n" C)
(newline)

(printf "A . B = ~s\n" (dot-product A B))
(printf "A x B = ~s\n" (cross-product A B))
(printf "A . B x C = ~s\n" (scalar-triple-product A B C))
(printf "A x B x C = ~s\n" (vector-triple-product A B C))

```



## REXX


```rexx
/*REXX program computes the products:  dot,  cross,  scalar triple,  and  vector triple.*/
                             a=   3   4   5
                             b=   4   3   5      /*(positive numbers don't need quotes.)*/
                             c= "-5 -12 -13"
call tellV  'vector A =', a                      /*show the  A  vector, aligned numbers.*/
call tellV  'vector B =', b                      /*  "   "   B     "        "      "    */
call tellV  'vector C =', c                      /*  "   "   C     "        "      "    */
say
call tellV  '  dot product [A∙B] =',                 dot(a, b)
call tellV  'cross product [AxB] =',               cross(a, b)
call tellV  'scalar triple product [A∙(BxC)] =',     dot(a, cross(b, c) )
call tellV  'vector triple product [Ax(BxC)] =',   cross(a, cross(b, c) )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cross: procedure; arg $1 $2 $3,@1 @2 @3;   return $2*@3 -$3*@2  $3*@1 -$1*@3  $1*@2 -$2*@1
dot:   procedure; arg $1 $2 $3,@1 @2 @3;   return $1*@1        +   $2*@2     +   $3*@3
/*──────────────────────────────────────────────────────────────────────────────────────*/
tellV: procedure; parse arg name,x y z                            /*obtain name, values.*/
       w=max(4, length(x), length(y), length(z) )                 /*max width of numbers*/
       say right(name, 40)   right(x,w)   right(y,w)   right(z,w) /*enforce # alignment.*/
       return                                                     /* [↑]  display vector*/
```

```txt

                              vector A =    3    4    5
                              vector B =    4    3    5
                              vector C =   -5  -12  -13

                     dot product [A∙B] =   49
                   cross product [AxB] =    5    5   -7
       scalar triple product [A∙(BxC)] =    6
       vector triple product [Ax(BxC)] = -267  204   -3

```



## Ring


```ring

# Project : Vector products

d = list(3)
e = list(3)
a = [3, 4, 5]
b = [4, 3, 5]
c = [-5, -12, -13]

see "a . b = " + dot(a,b) + nl
cross(a,b,d)
see "a x b = (" + d[1] + ", " + d[2] + ", " + d[3] + ")" + nl
see "a . (b x c) = " + scalartriple(a,b,c) + nl
vectortriple(a,b,c,d)

def dot(a,b)
    sum = 0
    for n=1 to len(a)
        sum = sum + a[n]*b[n]
    next
    return sum

func cross(a,b,d)
     d = [a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1]]

func scalartriple(a,b,c)
     cross(b,c,d)
     return dot(a,d)

func vectortriple(a,b,c,d)
     cross(b,c,d)
     cross(a,d,e)
     see "a x (b x c) = (" + e[1] + ", " +e[2] + ", " + e[3] + ")"

```

Output:

```txt

a . b = 49
a x b = (5, 5, -7)
a . (b x c) = 6
a x (b x c) = (-267, 204, -3)

```



## Ruby

Dot product is also known as ''inner product''. The standard library already defines Vector#inner_product and Vector# cross_product, so this program only defines the other two methods.

```ruby
require 'matrix'

class Vector
  def scalar_triple_product(b, c)
    self.inner_product(b.cross_product c)
  end

  def vector_triple_product(b, c)
    self.cross_product(b.cross_product c)
  end
end

a = Vector[3, 4, 5]
b = Vector[4, 3, 5]
c = Vector[-5, -12, -13]

puts "a dot b = #{a.inner_product b}"
puts "a cross b = #{a.cross_product b}"
puts "a dot (b cross c) = #{a.scalar_triple_product b, c}"
puts "a cross (b cross c) = #{a.vector_triple_product b, c}"
```

Output:
```txt
a dot b = 49
a cross b = Vector[5, 5, -7]
a dot (b cross c) = 6
a cross (b cross c) = Vector[-267, 204, -3]
```



## Rust


```rust
#[derive(Debug)]
struct Vector {
    x: f64,
    y: f64,
    z: f64,
}

impl Vector {
    fn new(x: f64, y: f64, z: f64) -> Self {
        Vector {
            x: x,
            y: y,
            z: z,
        }
    }

    fn dot_product(&self, other: &Vector) -> f64 {
        (self.x * other.x) + (self.y * other.y) + (self.z * other.z)
    }

    fn cross_product(&self, other: &Vector) -> Vector {
        Vector::new(self.y * other.z - self.z * other.y,
                    self.z * other.x - self.x * other.z,
                    self.x * other.y - self.y * other.x)
    }

    fn scalar_triple_product(&self, b: &Vector, c: &Vector) -> f64 {
        self.dot_product(&b.cross_product(&c))
    }

    fn vector_triple_product(&self, b: &Vector, c: &Vector) -> Vector {
        self.cross_product(&b.cross_product(&c))
    }
}

fn main(){
    let a = Vector::new(3.0, 4.0, 5.0);
    let b = Vector::new(4.0, 3.0, 5.0);
    let c = Vector::new(-5.0, -12.0, -13.0);

    println!("a . b = {}", a.dot_product(&b));
    println!("a x b = {:?}", a.cross_product(&b));
    println!("a . (b x c) = {}", a.scalar_triple_product(&b, &c));
    println!("a x (b x c) = {:?}", a.vector_triple_product(&b, &c));
}
```


Output:
```txt
a . b = 49
a x b = Vector { x: 5, y: 5, z: -7 }
a . (b x c) = 6
a x (b x c) = Vector { x: -267, y: 204, z: -3 }
```



## Scala


```scala
case class Vector3D(x:Double, y:Double, z:Double) {
  def dot(v:Vector3D):Double=x*v.x + y*v.y + z*v.z;
  def cross(v:Vector3D)=Vector3D(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)
  def scalarTriple(v1:Vector3D, v2:Vector3D)=this dot (v1 cross v2)
  def vectorTriple(v1:Vector3D, v2:Vector3D)=this cross (v1 cross v2)
}

object VectorTest {
  def main(args:Array[String])={
    val a=Vector3D(3,4,5)
    val b=Vector3D(4,3,5)
    val c=Vector3D(-5,-12,-13)

    println("      a . b : " + (a dot b))
    println("      a x b : " + (a cross b))
    println("a . (b x c) : " + (a scalarTriple(b, c)))
    println("a x (b x c) : " + (a vectorTriple(b, c)))
  }
}
```

```txt
      a . b : 49.0
      a x b : Vector3D(5.0,5.0,-7.0)
a . (b x c) : 6.0
a x (b x c) : Vector3D(-267.0,204.0,-3.0)
```



## Scheme

Using modified dot-product function from the [[Dot product]] task.

```scheme
(define (dot-product A B)
    (apply + (map * (vector->list A) (vector->list B))))

(define (cross-product A B)
	(define len (vector-length A))
	(define xp (make-vector (vector-length A) #f))
	(let loop ((n 0))
		(vector-set! xp n (-
			(* (vector-ref A (modulo (+ n 1) len))
				(vector-ref B (modulo (+ n 2) len)))
			(* (vector-ref A (modulo (+ n 2) len))
				(vector-ref B (modulo (+ n 1) len)))))
		(if (eqv? len (+ n 1))
			xp
			(loop (+ n 1)))))

(define (scalar-triple-product A B C)
	(dot-product A (cross-product B C)))

(define (vector-triple-product A B C)
	(cross-product A (cross-product B C)))


(define A #( 3 4 5))
(define B #(4 3 5))
(define C #(-5 -12 -13))

(display "A = ")(display A)(newline)
(display "B = ")(display B)(newline)
(display "C = ")(display C)(newline)
(newline)
(display "A . B = ")(display (dot-product A B))(newline)
(display "A x B = ")(display (cross-product A B))(newline)
(display "A . B x C = ")(display (scalar-triple-product A B C))(newline)
(display "A x B x C = ") (display (vector-triple-product A B C))(newline)
```

Output:
```txt
A = #(3 4 5)
B = #(4 3 5)
C = #(-5 -12 -13)

A . B = 49
A x B = #(5 5 -7)
A . B x C = 6
A x B x C = #(-267 204 -3)
```



## Seed7

The program below uses Seed7s capaibility to define operator symbols.
The operators ''dot'' and ''X'' are defined with with priority 6 and assiciativity left-to-right.

```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const type: vec3 is new struct
    var float: x is 0.0;
    var float: y is 0.0;
    var float: z is 0.0;
  end struct;

const func vec3: vec3 (in float: x, in float: y, in float: z) is func
  result
    var vec3: aVector is vec3.value;
  begin
    aVector.x := x;
    aVector.y := y;
    aVector.z := z;
  end func;

$ syntax expr: .(). dot .() is -> 6;
const func float: (in vec3: a) dot (in vec3: b) is
  return a.x*b.x + a.y*b.y + a.z*b.z;

$ syntax expr: .(). X .() is -> 6;
const func vec3: (in vec3: a) X (in vec3: b) is
  return vec3(a.y*b.z - a.z*b.y,
              a.z*b.x - a.x*b.z,
              a.x*b.y - a.y*b.x);

const func string: str (in vec3: v) is
  return "(" <& v.x <& ", " <& v.y <& ", " <& v.z <& ")";

enable_output(vec3);

const func float: scalarTriple (in vec3: a, in vec3: b, in vec3: c) is
  return a dot (b X c);

const func vec3: vectorTriple (in vec3: a, in vec3: b, in vec3: c) is
  return a X (b X c);

const proc: main is func
  local
    const vec3: a is vec3(3.0, 4.0, 5.0);
    const vec3: b is vec3(4.0, 3.0, 5.0);
    const vec3: c is vec3(-5.0, -12.0, -13.0);
  begin
    writeln("a = " <& a <& ", b = " <& b <& ", c = " <& c);
    writeln("a . b      = " <& a dot b);
    writeln("a x b      = " <& a X b);
    writeln("a .(b x c) = " <& scalarTriple(a, b, c));
    writeln("a x(b x c) = " <& vectorTriple(a, b, c));
  end func;
```


```txt

a = (3.0, 4.0, 5.0), b = (4.0, 3.0, 5.0), c = (-5.0, -12.0, -13.0)
a . b      = 49.0
a x b      = (5.0, 5.0, -7.0)
a .(b x c) = 6.0
a x(b x c) = (-267.0, 204.0, -3.0)

```



## Sidef


```ruby
class MyVector(x, y, z) {
    method ∙(vec) {
        [self{:x,:y,:z}] »*« [vec{:x,:y,:z}] «+»
    }
 
    method ⨉(vec) {
        MyVector(self.y*vec.z - self.z*vec.y,
               self.z*vec.x - self.x*vec.z,
               self.x*vec.y - self.y*vec.x)
    }
 
    method to_s {
        "(#{x}, #{y}, #{z})"
    }
}
 
var a = MyVector(3, 4, 5)
var b = MyVector(4, 3, 5)
var c = MyVector(-5, -12, -13)
 
say "a=#{a}; b=#{b}; c=#{c};"
say "a ∙ b = #{a ∙ b}"
say "a ⨉ b = #{a ⨉ b}"
say "a ∙ (b ⨉ c) = #{a ∙ (b ⨉ c)}"
say "a ⨉ (b ⨉ c) = #{a ⨉ (b ⨉ c)}"
```

```txt
a=(3, 4, 5); b=(4, 3, 5); c=(-5, -12, -13);
a ∙ b = 49
a ⨉ b = (5, 5, -7)
a ∙ (b ⨉ c) = 6
a ⨉ (b ⨉ c) = (-267, 204, -3)
```



## Stata


```stata
mata
real scalar sprod(real colvector u, real colvector v) {
	return(u[1]*v[1] + u[2]*v[2] + u[3]*v[3])
}

real colvector vprod(real colvector u, real colvector v) {
	return(u[2]*v[3]-u[3]*v[2]\u[3]*v[1]-u[1]*v[3]\u[1]*v[2]-u[2]*v[1])
}

real scalar striple(real colvector u, real colvector v, real colvector w) {
	return(sprod(u, vprod(v, w)))
}

real colvector vtriple(real colvector u, real colvector v, real colvector w) {
	return(vprod(u, vprod(v, w)))
}

a = 3\4\5
b = 4\3\5
c = -5\-12\-13

sprod(a, b)
  49

vprod(a, b)
        1
    +------+
  1 |   5  |
  2 |   5  |
  3 |  -7  |
    +------+

striple(a, b, c)
  6

vtriple(a, b, c)
          1
    +--------+
  1 |  -267  |
  2 |   204  |
  3 |    -3  |
    +--------+
end
```



## Tcl


```tcl
proc dot {A B} {
    lassign $A a1 a2 a3
    lassign $B b1 b2 b3
    expr {$a1*$b1 + $a2*$b2 + $a3*$b3}
}
proc cross {A B} {
    lassign $A a1 a2 a3
    lassign $B b1 b2 b3
    list [expr {$a2*$b3 - $a3*$b2}] \
	 [expr {$a3*$b1 - $a1*$b3}] \
	 [expr {$a1*$b2 - $a2*$b1}]
}
proc scalarTriple {A B C} {
    dot $A [cross $B $C]
}
proc vectorTriple {A B C} {
    cross $A [cross $B $C]
}
```

Demonstrating:

```tcl
set a {3 4 5}
set b {4 3 5}
set c {-5 -12 -13}
puts "a • b = [dot $a $b]"
puts "a x b = [cross $a $b]"
puts "a • b x c = [scalarTriple $a $b $c]"
puts "a x b x c = [vectorTriple $a $b $c]"
```

Output:
```txt
a • b = 49
a x b = 5 5 -7
a • b x c = 6
a x b x c = -267 204 -3

```



## uBasic/4tH

Since uBasic/4tH has only one single array, we use its variables to hold the offsets of the vectors. A similar problem arises when local vectors are required.
<lang>a = 0                                  ' use variables for vector addresses
b = a + 3
c = b + 3
d = c + 3

Proc _Vector (a, 3, 4, 5)              ' initialize the vectors
Proc _Vector (b, 4, 3, 5)
Proc _Vector (c, -5, -12, -13)

Print "a . b = "; FUNC(_FNdot(a, b))
Proc _Cross (a, b, d)
Print "a x b = (";@(d+0);", ";@(d+1);", ";@(d+2);")"
Print "a . (b x c) = "; FUNC(_FNscalarTriple(a, b, c))
Proc _VectorTriple (a, b, c, d)
Print "a x (b x c) = (";@(d+0);", ";@(d+1);", ";@(d+2);")"
End

_FNdot Param (2)
Return ((@(a@+0)*@(b@+0))+(@(a@+1)*@(b@+1))+(@(a@+2)*@(b@+2)))

_Vector Param (4)                      ' initialize a vector
  @(a@ + 0) = b@
  @(a@ + 1) = c@
  @(a@ + 2) = d@
Return

_Cross Param (3)
  @(c@+0) = @(a@ + 1) * @(b@ + 2) - @(a@ + 2) * @(b@ + 1)
  @(c@+1) = @(a@ + 2) * @(b@ + 0) - @(a@ + 0) * @(b@ + 2)
  @(c@+2) = @(a@ + 0) * @(b@ + 1) - @(a@ + 1) * @(b@ + 0)
Return

_FNscalarTriple Param (3)
  Local (1)                            ' a "local" vector
  d@ = d + 3                           ' (best effort) ;-)
  Proc _Cross(b@, c@, d@)
Return (FUNC(_FNdot(a@, d@)))

_VectorTriple Param(4)
  Local (1)                            ' a "local" vector
  e@ = d + 3                           ' (best effort) ;-)
  Proc _Cross (b@, c@, e@)
  Proc _Cross (a@, e@, d@)
Return
```

```txt
a . b = 49
a x b = (5, 5, -7)
a . (b x c) = 6
a x (b x c) = (-267, 204, -3)

0 OK, 0:1370
```


## VBA

```vb
Option Base 1
Function dot_product(a As Variant, b As Variant) As Variant
    dot_product = WorksheetFunction.SumProduct(a, b)
End Function

Function cross_product(a As Variant, b As Variant) As Variant
    cross_product = Array(a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1))
End Function

Function scalar_triple_product(a As Variant, b As Variant, c As Variant) As Variant
    scalar_triple_product = dot_product(a, cross_product(b, c))
End Function

Function vector_triple_product(a As Variant, b As Variant, c As Variant) As Variant
    vector_triple_product = cross_product(a, cross_product(b, c))
End Function

Public Sub main()
    a = [{3, 4, 5}]
    b = [{4, 3, 5}]
    c = [{-5, -12, -13}]
    Debug.Print "      a . b = "; dot_product(a, b)
    Debug.Print "      a x b = "; "("; Join(cross_product(a, b), ", "); ")"
    Debug.Print "a . (b x c) = "; scalar_triple_product(a, b, c)
    Debug.Print "a x (b x c) = "; "("; Join(vector_triple_product(a, b, c), ", "); ")"
End Sub
```
```txt
      a . b =  49
      a x b = (5, 5, -7)
a . (b x c) =  6
a x (b x c) = (-267, 204, -3)
```


## Visual Basic .NET

Class: Vector3D

```vbnet
Public Class Vector3D
    Private _x, _y, _z As Double

    Public Sub New(ByVal X As Double, ByVal Y As Double, ByVal Z As Double)
        _x = X
        _y = Y
        _z = Z
    End Sub

    Public Property X() As Double
        Get
            Return _x
        End Get
        Set(ByVal value As Double)
            _x = value
        End Set
    End Property

    Public Property Y() As Double
        Get
            Return _y
        End Get
        Set(ByVal value As Double)
            _y = value
        End Set
    End Property

    Public Property Z() As Double
        Get
            Return _z
        End Get
        Set(ByVal value As Double)
            _z = value
        End Set
    End Property

    Public Function Dot(ByVal v2 As Vector3D) As Double
        Return (X * v2.X) + (Y * v2.Y) + (Z * v2.Z)
    End Function

    Public Function Cross(ByVal v2 As Vector3D) As Vector3D
        Return New Vector3D((Y * v2.Z) - (Z * v2.Y), _
                            (Z * v2.X) - (X * v2.Z), _
                            (X * v2.Y) - (Y * v2.X))
    End Function

    Public Function ScalarTriple(ByVal v2 As Vector3D, ByVal v3 As Vector3D) As Double
        Return Me.Dot(v2.Cross(v3))
    End Function

    Public Function VectorTriple(ByRef v2 As Vector3D, ByVal v3 As Vector3D) As Vector3D
        Return Me.Cross(v2.Cross(v3))
    End Function

    Public Overrides Function ToString() As String
        Return String.Format("({0}, {1}, {2})", _x, _y, _z)
    End Function
End Class
```

Module: Module1

```vbnet
Module Module1

    Sub Main()
        Dim v1 As New Vector3D(3, 4, 5)
        Dim v2 As New Vector3D(4, 3, 5)
        Dim v3 As New Vector3D(-5, -12, -13)

        Console.WriteLine("v1: {0}", v1.ToString())
        Console.WriteLine("v2: {0}", v2.ToString())
        Console.WriteLine("v3: {0}", v3.ToString())
        Console.WriteLine()

        Console.WriteLine("v1 . v2 = {0}", v1.Dot(v2))
        Console.WriteLine("v1 x v2 = {0}", v1.Cross(v2).ToString())
        Console.WriteLine("v1 . (v2 x v3) = {0}", v1.ScalarTriple(v2, v3))
        Console.WriteLine("v1 x (v2 x v3) = {0}", v1.VectorTriple(v2, v3))
    End Sub

End Module
```

Output:

```txt
v1: (3, 4, 5)
v2: (4, 3, 5)
v3: (-5, -12, -13)

v1 . v2 = 49
v1 x v2 = (5, 5, -7)
v1 . (v2 x v3) = 6
v1 x (v2 x v3) = (-267, 204, -3)
```



## Wortel


```wortel
@let {
  dot &[a b] @sum @mapm ^* [a b]
  cross &[a b] [[
    -*a.1 b.2 *a.2 b.1
    -*a.2 b.0 *a.0 b.2
    -*a.0 b.1 *a.1 b.0
  ]]
  scalarTripleProduct &[a b c] !!dot a !!cross b c
  vectorTripleProduct &[a b c] !!cross a !!cross b c

  a [3 4 5]
  b [4 3 5]
  c [5N 12N 13N]

  [[
    !!dot a b
    !!cross a b
    @!scalarTripleProduct [a b c]
    @!vectorTripleProduct [a b c]
  ]]
}
```

Returns:

```txt
[49 [5 5 -7] 6 [-267 204 -3]]
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

func DotProd(A, B);             \Return the dot product of two 3D vectors
int A, B;                       \A ù B
return A(0)*B(0) + A(1)*B(1) + A(2)*B(2);

proc CrossProd(A, B, C);        \Calculate the cross product of two 3D vectors
int A, B, C;                    \C:= A x B
[C(0):= A(1)*B(2) - A(2)*B(1);
 C(1):= A(2)*B(0) - A(0)*B(2);
 C(2):= A(0)*B(1) - A(1)*B(0);
]; \CrossProd

func ScalarTriProd(A, B, C);    \Return the scalar triple product
int A, B, C;                    \A ù (B x C)
int D(3);
[CrossProd(B, C, D);
return DotProd(A, D);
]; \ScalarTriProd

proc VectTriProd(A, B, C, D);   \Calculate the vector triple product
int A, B, C, D;                 \D:= A x (B x C)
int E(3);
[CrossProd(B, C, E);
 CrossProd(A, E, D);
]; \CrossProd


int A, B, C, D(3);
[A:= [3, 4, 5];  B:= [4, 3, 5];  C:= [-5, -12, -13];

IntOut(0, DotProd(A,B));  CrLf(0);

CrossProd(A, B, D);
IntOut(0, D(0));  ChOut(0, 9\tab\);
IntOut(0, D(1));  ChOut(0, 9\tab\);
IntOut(0, D(2));  CrLf(0);

IntOut(0, ScalarTriProd(A,B,C)); CrLf(0);

VectTriProd(A, B, C, D);
IntOut(0, D(0));  ChOut(0, 9\tab\);
IntOut(0, D(1));  ChOut(0, 9\tab\);
IntOut(0, D(2));  CrLf(0);
]
```


Output:

```txt

49
5       5       -7
6
-267    204     -3

```



## zkl

Since the input vectors are all int, the output is int. For a float output, use float data (or convert) in the input vectors and change sum() to sum(0.0) (in dotp).

The [(a1,a2,a3)] parameter notation just means add a preamble to the function body to do list assignment: a1,a2,a3:=arglist[0]. Since we don't need the vector as such, don't bother to name it (in the parameter list)

```zkl
fcn dotp(a,b){ a.zipWith('*,b).sum() } //1 slow but concise
fcn crossp([(a1,a2,a3)],[(b1,b2,b3)])  //2
   { return(a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1) }
```


```zkl
a,b,c := T(3,4,5), T(4,3,5), T(-5,-12,-13);
dotp(a,b).println();   //5 --> 49
crossp(a,b).println(); //6 --> (5,5,-7)
dotp(a, crossp(b,c)).println();   //7 --> 6
crossp(a, crossp(b,c)).println(); //8 --> (-267,204,-3)
```

```txt

49
L(5,5,-7)
6
L(-267,204,-3)

```

Or, using the GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
a:=GSL.VectorFromData( 3,  4,  5);
b:=GSL.VectorFromData( 4,  3,  5);
c:=GSL.VectorFromData(-5,-12,-13);

(a*b).println();  // 49, dot product
a.copy().crossProduct(b) // (5,5,-7)  cross product, in place
   .format().println();
(a*(b.copy().crossProduct(c))).println();  //  6 scalar triple product
(a.crossProduct(b.crossProduct(c)))  // (-267,204,-3) vector triple product, in place
   .format().println();
```

```txt

49
5.00,5.00,-7.00
6
-267.00,204.00,-3.00

```



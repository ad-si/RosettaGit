+++
title = "Vector"
description = ""
date = 2019-10-13T07:18:21Z
aliases = []
[extra]
id = 18904
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "c",
  "cpp",
  "csharp",
  "d",
  "factor",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "miniscript",
  "objeck",
  "ocaml",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "tcl",
  "vba",
  "visual_basic_dotnet",
  "wdte",
  "zkl",
]
+++

## Task

Implement a Vector class (or a set of functions) that models a Physical Vector. The four basic operations and a ''pretty print'' function should be implemented.


The Vector may be initialized in any reasonable way.
* Start and end points,  and direction
* Angular coefficient and value (length)


The four operations to be implemented are:
* Vector <big><b> + </b></big> Vector addition
* Vector <big><b> - </b></big> Vector subtraction
* Vector <big><b> * </b></big> scalar multiplication
* Vector <big><b> / </b></big> scalar division





## ALGOL 68


```algol68
# the standard mode COMPLEX is a two element vector #
MODE VECTOR = COMPLEX;
# the operations required for the task plus many others are provided as standard for COMPLEX and REAL items #
# the two components are fields called "re" and "im" #
# we can define a "pretty-print" operator: #
# returns a formatted representation of the vector #
OP TOSTRING = ( VECTOR a )STRING: "[" + TOSTRING re OF a + ", " + TOSTRING im OF a + "]";
# returns a formatted representation of the scaler #
OP TOSTRING = ( REAL a )STRING: fixed( a, 0, 4 );

# test the operations #
VECTOR a = 5 I 7, b = 2 I 3; # note the use of the I operator to construct a COMPLEX from two scalers #
print( ( "a+b : ", TOSTRING ( a + b  ), newline ) );
print( ( "a-b : ", TOSTRING ( a - b  ), newline ) );
print( ( "a*11: ", TOSTRING ( a * 11 ), newline ) );
print( ( "a/2 : ", TOSTRING ( a / 2  ), newline ) )

```

```txt

a+b : [7.0000, 10.0000]
a-b : [3.0000, 4.0000]
a*11: [55.0000, 77.0000]
a/2 : [2.5000, 3.5000]

```



## C

j cap or hat j is not part of the ASCII set, thus û ( 150 ) is used in it's place.

```C

#include<stdio.h>
#include<math.h>

#define pi M_PI

typedef struct{
	double x,y;
}vector;

vector initVector(double r,double theta){
	vector c;

	c.x = r*cos(theta);
	c.y = r*sin(theta);

	return c;
}

vector addVector(vector a,vector b){
	vector c;

	c.x = a.x + b.x;
	c.y = a.y + b.y;

	return c;
}

vector subtractVector(vector a,vector b){
	vector c;

	c.x = a.x - b.x;
	c.y = a.y - b.y;

	return c;
}

vector multiplyVector(vector a,double b){
	vector c;

	c.x = b*a.x;
	c.y = b*a.y;

	return c;
}

vector divideVector(vector a,double b){
	vector c;

	c.x = a.x/b;
	c.y = a.y/b;

	return c;
}

void printVector(vector a){
	printf("%lf %c %c %lf %c",a.x,140,(a.y>=0)?'+':'-',(a.y>=0)?a.y:fabs(a.y),150);
}

int main()
{
	vector a = initVector(3,pi/6);
	vector b = initVector(5,2*pi/3);

	printf("\nVector a : ");
	printVector(a);

	printf("\n\nVector b : ");
	printVector(b);

	printf("\n\nSum of vectors a and b : ");
	printVector(addVector(a,b));

	printf("\n\nDifference of vectors a and b : ");
	printVector(subtractVector(a,b));

	printf("\n\nMultiplying vector a by 3 : ");
	printVector(multiplyVector(a,3));

	printf("\n\nDividing vector b by 2.5 : ");
	printVector(divideVector(b,2.5));

	return 0;
}

```

Output:

```txt


Vector a : 2.598076 î + 1.500000 û

Vector b : -2.500000 î + 4.330127 û

Sum of vectors a and b : 0.098076 î + 5.830127 û

Difference of vectors a and b : 5.098076 î - 2.830127 û

Multiplying vector a by 3 : 7.794229 î + 4.500000 û

Dividing vector b by 2.5 : -1.000000 î + 1.732051 û

```



## C++


```cpp
#include <iostream>
#include <cmath>
#include <cassert>
using namespace std;

#define PI 3.14159265359

class Vector
{
public:
    Vector(double ix, double iy, char mode)
    {
        if(mode=='a')
        {
            x=ix*cos(iy);
            y=ix*sin(iy);
        }
        else
        {
            x=ix;
            y=iy;
        }
    }
    Vector(double ix,double iy)
    {
        x=ix;
        y=iy;
    }
    Vector operator+(const Vector& first)
    {
        return Vector(x+first.x,y+first.y);
    }
    Vector operator-(Vector first)
    {
        return Vector(x-first.x,y-first.y);
    }
    Vector operator*(double scalar)
    {
        return Vector(x*scalar,y*scalar);
    }
    Vector operator/(double scalar)
    {
        return Vector(x/scalar,y/scalar);
    }
    bool operator==(Vector first)
    {
        return (x==first.x&&y==first.y);
    }
    void v_print()
    {
        cout << "X: " << x << " Y: " << y;
    }
    double x,y;
};

int main()
{
    Vector vec1(0,1);
    Vector vec2(2,2);
    Vector vec3(sqrt(2),45*PI/180,'a');
    vec3.v_print();
    assert(vec1+vec2==Vector(2,3));
    assert(vec1-vec2==Vector(-2,-1));
    assert(vec1*5==Vector(0,5));
    assert(vec2/2==Vector(1,1));
    return 0;
}

```

```txt

X: 1 Y: 1

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace RosettaVectors
{
    public class Vector
    {
        public double[] store;
        public Vector(IEnumerable<double> init)
        {
            store = init.ToArray();
        }
        public Vector(double x, double y)
        {
            store = new double[] { x, y };
        }
        static public Vector operator+(Vector v1, Vector v2)
        {
            return new Vector(v1.store.Zip(v2.store, (a, b) => a + b));
        }
        static public Vector operator -(Vector v1, Vector v2)
        {
            return new Vector(v1.store.Zip(v2.store, (a, b) => a - b));
        }
        static public Vector operator *(Vector v1, double scalar)
        {
            return new Vector(v1.store.Select(x => x * scalar));
        }
        static public Vector operator /(Vector v1, double scalar)
        {
            return new Vector(v1.store.Select(x => x / scalar));
        }
        public override string ToString()
        {
            return string.Format("[{0}]", string.Join(",", store));
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            var v1 = new Vector(5, 7);
            var v2 = new Vector(2, 3);
            Console.WriteLine(v1 + v2);
            Console.WriteLine(v1 - v2);
            Console.WriteLine(v1 * 11);
            Console.WriteLine(v1 / 2);
            // Works with arbitrary size vectors, too.
            var lostVector = new Vector(new double[] { 4, 8, 15, 16, 23, 42 });
            Console.WriteLine(lostVector * 7);
            Console.ReadLine();
        }
    }
}

```

```txt
[7,10]
[3,4]
[55,77]
[2.5,3.5]
[28,56,105,112,161,294]
```



## D


```D
import std.stdio;

void main() {
    writeln(VectorReal(5, 7) + VectorReal(2, 3));
    writeln(VectorReal(5, 7) - VectorReal(2, 3));
    writeln(VectorReal(5, 7) * 11);
    writeln(VectorReal(5, 7) / 2);
}

alias VectorReal = Vector!real;
struct Vector(T) {
    private T x, y;

    this(T x, T y) {
        this.x = x;
        this.y = y;
    }

    auto opBinary(string op : "+")(Vector rhs) const {
        return Vector(x + rhs.x, y + rhs.y);
    }

    auto opBinary(string op : "-")(Vector rhs) const {
        return Vector(x - rhs.x, y - rhs.y);
    }

    auto opBinary(string op : "/")(T denom) const {
        return Vector(x / denom, y / denom);
    }

    auto opBinary(string op : "*")(T mult) const {
        return Vector(x * mult, y * mult);
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;
        sink.formattedWrite!"(%s, %s)"(x, y);
    }
}
```


```txt
(7, 10)
(3, 4)
(55, 77)
(2.5, 3.5)
```


=={{header|F#|F sharp}}==

```fsharp
open System

let add (ax, ay) (bx, by) =
    (ax+bx, ay+by)

let sub (ax, ay) (bx, by) =
    (ax-bx, ay-by)

let mul (ax, ay) c =
    (ax*c, ay*c)

let div (ax, ay) c =
    (ax/c, ay/c)

[<EntryPoint>]
let main _ =
    let a = (5.0, 7.0)
    let b = (2.0, 3.0)

    printfn "%A" (add a b)
    printfn "%A" (sub a b)
    printfn "%A" (mul a 11.0)
    printfn "%A" (div a 2.0)
    0 // return an integer exit code
```



## Factor

It should be noted the <code>math.vectors</code> vocabulary has words for treating any sequence like a vector. For instance:

```factor
(scratchpad) USE: math.vectors
(scratchpad) { 1 2 } { 3 4 } v+

--- Data stack:
{ 4 6 }
```

However, in the spirit of the task, we will implement our own vector data structure. In addition to arithmetic and prettyprinting, we define a convenient literal syntax for making new vectors.

```factor
USING: accessors arrays kernel math parser prettyprint
prettyprint.custom sequences ;
IN: rosetta-code.vector

TUPLE: vec { x real read-only } { y real read-only } ;
C: <vec> vec

<PRIVATE

: parts ( vec -- x y ) [ x>> ] [ y>> ] bi ;
: devec ( vec1 vec2 -- x1 y1 x2 y2 ) [ parts ] bi@ rot swap ;

: binary-op ( vec1 vec2 quot -- vec3 )
    [ devec ] dip 2bi@ <vec> ; inline

: scalar-op ( vec1 scalar quot -- vec2 )
    [ parts ] 2dip curry bi@ <vec> ; inline

PRIVATE>

SYNTAX: VEC{ \ } [ first2 <vec> ] parse-literal ;

: v+ ( vec1 vec2   -- vec3 ) [ + ] binary-op ;
: v- ( vec1 vec2   -- vec3 ) [ - ] binary-op ;
: v* ( vec1 scalar -- vec2 ) [ * ] scalar-op ;
: v/ ( vec1 scalar -- vec2 ) [ / ] scalar-op ;

M: vec pprint-delims drop \ VEC{ \ } ;
M: vec >pprint-sequence parts 2array ;
M: vec pprint* pprint-object ;
```

We demonstrate the use of vectors in a new file, since parsing words can't be used in the same file where they're defined.

```factor
USING: kernel formatting prettyprint rosetta-code.vector
sequences ;
IN: rosetta-code.vector

: demo ( a b quot -- )
    3dup [ unparse ] tri@ rest but-last
    "%16s %16s%3s= " printf call . ; inline

VEC{ -8.4 1.35 } VEC{ 10 11/123 } [ v+ ] demo
VEC{ 5 3 } VEC{ 4 2 } [ v- ] demo
VEC{ 4 -8 } 2 [ v* ] demo
VEC{ 5 7 } 2 [ v/ ] demo

! You can still make a vector without the literal syntax of
! course.

5 2 <vec> 1.3 [ v* ] demo
```

```txt

VEC{ -8.4 1.35 } VEC{ 10 11/123 } v+ = VEC{ 1.6 1.439430894308943 }
      VEC{ 5 3 }       VEC{ 4 2 } v- = VEC{ 1 1 }
     VEC{ 4 -8 }                2 v* = VEC{ 8 -16 }
      VEC{ 5 7 }                2 v/ = VEC{ 2+1/2 3+1/2 }
      VEC{ 5 2 }              1.3 v* = VEC{ 6.5 2.6 }

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type Vector
  As Double x, y
  Declare Operator Cast() As String
End Type

Operator Vector.Cast() As String
  Return "[" + Str(x) + ", " + Str(y) + "]"
End Operator

Operator + (vec1 As Vector, vec2 As Vector) As Vector
  Return Type<Vector>(vec1.x + vec2.x, vec1.y + vec2.y)
End Operator

Operator - (vec1 As Vector, vec2 As Vector) As Vector
  Return Type<Vector>(vec1.x - vec2.x, vec1.y - vec2.y)
End Operator

Operator * (vec As Vector, scalar As Double) As Vector
  Return Type<Vector>(vec.x * scalar, vec.y * scalar)
End Operator

Operator / (vec As Vector, scalar As Double) As Vector
  ' No need to check for division by zero as we're using Doubles
  Return Type<Vector>(vec.x / scalar, vec.y / scalar)
End Operator

Dim v1 As Vector = (5, 7)
Dim v2 As Vector = (2, 3)
Print v1; " +  "; v2; " = "; v1 + v2
Print v1; " -  "; v2; " = "; v1 - v2
Print v1; " * "; 11; "     = "; v1 * 11.0
Print v1; " / ";  2; "      = "; v1 / 2.0
Print
Print "Press any key to quit"
Sleep
```


```txt

[5, 7] +  [2, 3] = [7, 10]
[5, 7] -  [2, 3] = [3, 4]
[5, 7] *  11     = [55, 77]
[5, 7] /  2      = [2.5, 3.5]

```



## Go


```go
package main

import "fmt"

type vector []float64

func (v vector) add(v2 vector) vector {
    r := make([]float64, len(v))
    for i, vi := range v {
        r[i] = vi + v2[i]
    }
    return r
}

func (v vector) sub(v2 vector) vector {
    r := make([]float64, len(v))
    for i, vi := range v {
        r[i] = vi - v2[i]
    }
    return r
}

func (v vector) scalarMul(s float64) vector {
    r := make([]float64, len(v))
    for i, vi := range v {
        r[i] = vi * s
    }
    return r
}

func (v vector) scalarDiv(s float64) vector {
    r := make([]float64, len(v))
    for i, vi := range v {
        r[i] = vi / s
    }
    return r
}

func main() {
    v1 := vector{5, 7}
    v2 := vector{2, 3}
    fmt.Println(v1.add(v2))
    fmt.Println(v1.sub(v2))
    fmt.Println(v1.scalarMul(11))
    fmt.Println(v1.scalarDiv(2))
}
```

```txt

[7 10]
[3 4]
[55 77]
[2.5 3.5]

```



## Groovy

Euclidean vector spaces may be expressed in any (positive) number of dimensions. So why limit it to just 2?

Solution:


```groovy
import groovy.transform.EqualsAndHashCode

@EqualsAndHashCode
class Vector {
    private List<Number> elements
    Vector(List<Number> e ) {
        if (!e) throw new IllegalArgumentException("A Vector must have at least one element.")
        if (!e.every { it instanceof Number }) throw new IllegalArgumentException("Every element must be a number.")
        elements = [] + e
    }
    Vector(Number... e) { this(e as List) }

    def order() { elements.size() }
    def norm2() { elements.sum { it ** 2 } ** 0.5 }

    def plus(Vector that) {
        if (this.order() != that.order()) throw new IllegalArgumentException("Vectors must be conformable for addition.")
        [this.elements,that.elements].transpose()*.sum() as Vector
    }
    def minus(Vector that) { this + (-that) }
    def multiply(Number that) { this.elements.collect { it * that } as Vector }
    def div(Number that) { this * (1/that) }
    def negative() { this * -1 }

    String toString() { "(${elements.join(',')})" }
}

class VectorCategory {
   static Vector plus (Number a, Vector b) { b + a }
   static Vector minus (Number a, Vector b) { -b + a }
   static Vector multiply (Number a, Vector b) { b * a }
}
```



Test:


```groovy
Number.metaClass.mixin VectorCategory

def a = [1, 5] as Vector
def b = [6, -2] as Vector
def x = 8
println "a = $a    b = $b    x = $x"
assert a + b == [7, 3] as Vector
println "a + b == $a + $b == ${a+b}"
assert a - b == [-5, 7] as Vector
println "a - b == $a - $b == ${a-b}"
assert a * x == [8, 40] as Vector
println "a * x == $a * $x == ${a*x}"
assert x * a == [8, 40] as Vector
println "x * a == $x * $a == ${x*a}"
assert b / x == [3/4, -1/4] as Vector
println "b / x == $b / $x == ${b/x}"
```


Output:

```txt
a = (1,5)    b = (6,-2)    x = 8
a + b == (1,5) + (6,-2) == (7,3)
a - b == (1,5) - (6,-2) == (-5,7)
a * x == (1,5) * 8 == (8,40)
x * a == 8 * (1,5) == (8,40)
b / x == (6,-2) / 8 == (0.750,-0.250)
```



## Haskell


```Haskell

add (u,v) (x,y)      = (u+x,v+y)
minus (u,v) (x,y)    = (u-x,v-y)
multByScalar k (x,y) = (k*x,k*y)
divByScalar (x,y) k  = (x/k,y/k)

main = do
  let vecA = (3.0,8.0) -- cartersian coordinates
  let (r,theta) = (3,pi/12) :: (Double,Double)
  let vecB = (r*(cos theta),r*(sin theta)) -- from polar coordinates to cartesian coordinates
  putStrLn $ "vecA = " ++ (show vecA)
  putStrLn $ "vecB = " ++ (show vecB)
  putStrLn $ "vecA + vecB = " ++ (show.add vecA $ vecB)
  putStrLn $ "vecA - vecB = " ++ (show.minus vecA $ vecB)
  putStrLn $ "2 * vecB = " ++ (show.multByScalar 2 $ vecB)
  putStrLn $ "vecA / 3 = " ++ (show.divByScalar vecA $ 3)

```

```txt

vecA = (3.0,8.0)
vecB = (2.897777478867205,0.7764571353075622)
vecA + vecB = (5.897777478867205,8.776457135307563)
vecA - vecB = (0.10222252113279495,7.223542864692438)
2 * vecB = (5.79555495773441,1.5529142706151244)
vecA / 3 = (1.0,2.6666666666666665)

```



## J


These are primitive (built in) operations in J:


```J
   5 7+2 3
7 10
   5 7-2 3
3 4
   5 7*11
55 77
   5 7%2
2.5 3.5
```


A few things here might be worth noting:

J treats a sequences of space separated numbers as a single word, this is analogous to how languages which support a "string" data type support treating strings with spaces in them as single words. Put differently: '5 7' is a sequence of three characters but 5 7 (without the quotes) is a sequence of two numbers.

J uses the percent sign to represent division. This is a visual pun with the "division sign" or "obelus" which has been used to represent the division operation for hundreds of years.

In J, a single number (or single character) is special. It's not a treated as a sequence except in contexts where you explicitly declare it to be one (for example, by prefixing it with a comma). (If it were treated as a sequence the above <code>5 7*11</code> and <code>5 7%2</code> operations would have been errors, because of the vector length mis-match.)

It's perhaps also worth noting that J allows you to specify complex numbers using polar coordinates, and complex numbers can be converted to vectors using the special token (+.) - for example:


```J
   2ad45
1.41421j1.41421
   +. 2ad45
1.41421 1.41421
   2ar0.785398
1.41421j1.41421
   +. 2ar0.785398
1.41421 1.41421
```


In the construction of these numeric constants, <code>ad</code> is followed by an '''a'''ngle in '''d'''egrees while <code>ar</code> is followed by an '''a'''ngle in '''r'''adians. This practice of embedding letters in a numeric constant is analogous to the use of '''e'''xponential notation when describing some floating point numbers.


## Java


```java
import java.util.Locale;

public class Test {

    public static void main(String[] args) {
        System.out.println(new Vec2(5, 7).add(new Vec2(2, 3)));
        System.out.println(new Vec2(5, 7).sub(new Vec2(2, 3)));
        System.out.println(new Vec2(5, 7).mult(11));
        System.out.println(new Vec2(5, 7).div(2));
    }
}

class Vec2 {
    final double x, y;

    Vec2(double x, double y) {
        this.x = x;
        this.y = y;
    }

    Vec2 add(Vec2 v) {
        return new Vec2(x + v.x, y + v.y);
    }

    Vec2 sub(Vec2 v) {
        return new Vec2(x - v.x, y - v.y);
    }

    Vec2 div(double val) {
        return new Vec2(x / val, y / val);
    }

    Vec2 mult(double val) {
        return new Vec2(x * val, y * val);
    }

    @Override
    public String toString() {
        return String.format(Locale.US, "[%s, %s]", x, y);
    }
}
```



```txt
[7.0, 10.0]
[3.0, 4.0]
[55.0, 77.0]
[2.5, 3.5]
```



## jq

In the following, the vector [x,y] is represented by the JSON array [x,y].

For generality, the pointwise operations (multiply, divide, negate)
will work with conformal arrays of any dimension, and
sum/0 accepts any number of same-dimensional vectors.

```jq
def polar(r; angle):
  [ r*(angle|cos), r*(angle|sin) ];

# If your jq allows multi-arity functions, you may wish to uncomment the following line:
# def polar(r): [r, 0];

def polar2vector: polar(.[0]; .[1]);

def vector(x; y):
  if (x|type) == "number" and (y|type) == "number" then [x,y]
  else error("TypeError")
  end;

# Input: an array of same-dimensional vectors of any dimension to be added
def sum:
  def sum2: .[0] as $a | .[1] as $b | reduce range(0;$a|length) as $i ($a; .[$i] += $b[$i]);
  if length <= 1 then .
  else reduce .[1:][] as $v (.[0] ; [., $v]|sum2)
  end;

def multiply(scalar): [ .[] * scalar ];

def negate: multiply(-1);

def minus(v): [., (v|negate)] | sum;

def divide(scalar):
  if scalar == 0 then error("division of a vector by 0 is not supported")
  else [ .[] / scalar ]
  end;

def r: (.[0] | .*.) + (.[1] | .*.) | sqrt;

def atan2:
  def pi: 1 | atan * 4;
  def sign: if . < 0 then -1 elif . > 0 then 1 else 0 end;
  .[0] as $x | .[1] as $y
  | if $x == 0 then $y | sign * pi / 2
    else  ($y / $x) | if $x > 0 then atan elif . > 0 then atan - pi else atan + pi end
    end;

def angle: atan2;

def topolar: [r, angle];
```


'''Examples'''

```jq
def examples:
  def pi: 1 | atan * 4;

  [1,1] as $v
  | [3,4] as $w
  | polar(1; pi/2) as $z
  | polar(-2; pi/4) as $z2
  | "v     is \($v)",
    "    w is \($w)",
    "v + w is \([$v, $w] | sum)",
    "v - w is \( $v |minus($w))",
    "  - v is \( $v|negate )",
    "w * 5 is \($w | multiply(5))",
    "w / 2 is \($w | divide(2))",
    "v|topolar is \($v|topolar)",
    "w|topolar is \($w|topolar)",
    "z = polar(1; pi/2) is \($z)",
    "z|topolar is \($z|topolar)",
    "z2 = polar(-2; pi/4) is \($z2)",
    "z2|topolar is \($z2|topolar)",
    "z2|topolar|polar is \($z2|topolar|polar2vector)" ;

examples
```

```sh
$ jq -r -n -f vector.jq
v     is [1,1]
    w is [3,4]
v + w is [4,5]
v - w is [-2,-3]
  - v is [-1,-1]
w * 5 is [15,20]
w / 2 is [1.5,2]
v|topolar is [1.4142135623730951,0.7853981633974483]
w|topolar is [5,0.9272952180016122]
z = polar(1; pi/2) is [6.123233995736766e-17,1]
z|topolar is [1,1.5707963267948966]
z2 = polar(-2; pi/4) is [-1.4142135623730951,-1.414213562373095]
z2|topolar is [2,-2.356194490192345]
z2|topolar|polar is [-1.414213562373095,-1.4142135623730951]
```



## Julia

The parameters indicate the dimension of the spatial vector. So it would be easy to implement a higher-degree-space vector.

'''The module''':

```julia
module SpatialVectors

export SpatialVector

struct SpatialVector{N, T}
    coord::NTuple{N, T}
end

SpatialVector(s::NTuple{N,T}, e::NTuple{N,T}) where {N,T} =
    SpatialVector{N, T}(e .- s)
function SpatialVector(∠::T, val::T) where T
    θ = atan(∠)
    x = val * cos(θ)
    y = val * sin(θ)
    return SpatialVector((x, y))
end

angularcoef(v::SpatialVector{2, T}) where T = v.coord[2] / v.coord[1]
Base.norm(v::SpatialVector) = sqrt(sum(x -> x^2, v.coord))

function Base.show(io::IO, v::SpatialVector{2, T}) where T
    ∠ = angularcoef(v)
    val = norm(v)
    println(io, """2-dim spatial vector
        - Angular coef ∠: $(∠) (θ = $(rad2deg(atan(∠)))°)
        - Magnitude: $(val)
        - X coord: $(v.coord[1])
        - Y coord: $(v.coord[2])""")
end

Base.:-(v::SpatialVector) = SpatialVector(.- v.coord)

for op in (:+, :-)
    @eval begin
        Base.$op(a::SpatialVector{N, T}, b::SpatialVector{N, U}) where {N, T, U} =
            SpatialVector{N, promote_type(T, U)}(broadcast($op, a.coord, b.coord))
    end
end

for op in (:*, :/)
    @eval begin
        Base.$op(n::T, v::SpatialVector{N, U}) where {N, T, U} =
            SpatialVector{N, promote_type(T, U)}(broadcast($op, n, v.coord))
        Base.$op(v::SpatialVector, n::Number) = $op(n, v)
    end
end

end  # module Vectors
```



## Kotlin


```scala
// version 1.1.2

class Vector2D(val x: Double, val y: Double) {
    operator fun plus(v: Vector2D) = Vector2D(x + v.x, y + v.y)

    operator fun minus(v: Vector2D) = Vector2D(x - v.x, y - v.y)

    operator fun times(s: Double) = Vector2D(s * x, s * y)

    operator fun div(s: Double) = Vector2D(x / s, y / s)

    override fun toString() = "($x, $y)"
}

operator fun Double.times(v: Vector2D) = v * this

fun main(args: Array<String>) {
    val v1 = Vector2D(5.0, 7.0)
    val v2 = Vector2D(2.0, 3.0)
    println("v1 = $v1")
    println("v2 = $v2")
    println()
    println("v1 + v2 = ${v1 + v2}")
    println("v1 - v2 = ${v1 - v2}")
    println("v1 * 11 = ${v1 * 11.0}")
    println("11 * v2 = ${11.0 * v2}")
    println("v1 / 2  = ${v1 / 2.0}")
}
```


```txt

v1 = (5.0, 7.0)
v2 = (2.0, 3.0)

v1 + v2 = (7.0, 10.0)
v1 - v2 = (3.0, 4.0)
v1 * 11 = (55.0, 77.0)
11 * v2 = (22.0, 33.0)
v1 / 2  = (2.5, 3.5)

```



## Lua


```Lua
vector = {mt = {}}

function vector.new (x, y)
    local new = {x = x or 0, y = y or 0}
    setmetatable(new, vector.mt)
    return new
end

function vector.mt.__add (v1, v2)
    return vector.new(v1.x + v2.x, v1.y + v2.y)
end

function vector.mt.__sub (v1, v2)
    return vector.new(v1.x - v2.x, v1.y - v2.y)
end

function vector.mt.__mul (v, s)
    return vector.new(v.x * s, v.y * s)
end

function vector.mt.__div (v, s)
    return vector.new(v.x / s, v.y / s)
end

function vector.print (vec)
    print("(" .. vec.x .. ", " .. vec.y .. ")")
end

local a, b = vector.new(5, 7), vector.new(2, 3)
vector.print(a + b)
vector.print(a - b)
vector.print(a * 11)
vector.print(a / 2)
```

```txt
(7, 10)
(3, 4)
(55, 77)
(2.5, 3.5)
```



## MiniScript


```MiniScript
vplus = function(v1, v2)
    return [v1[0]+v2[0],v1[1]+v2[1]]
end function

vminus = function (v1, v2)
    return [v1[0]-v2[0],v1[1]-v2[1]]
end function

vmult = function(v1, scalar)
    return [v1[0]*scalar, v1[1]*scalar]
end function

vdiv = function(v1, scalar)
    return [v1[0]/scalar, v1[1]/scalar]
end function

vector1 = [2,3]
vector2 = [4,5]

print vplus(vector1,vector2)
print vminus(vector2, vector1)
print vmult(vector1, 3)
print vdiv(vector2, 2)
```

```txt

[6, 8]
[2, 2]
[6, 9]
[2, 2.5]

```


=={{header|Modula-2}}==

```modula2
MODULE Vector;
FROM FormatString IMPORT FormatString;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE Vector =
    RECORD
        x,y : REAL;
    END;

PROCEDURE Add(a,b : Vector) : Vector;
BEGIN
    RETURN Vector{a.x+b.x, a.y+b.y}
END Add;

PROCEDURE Sub(a,b : Vector) : Vector;
BEGIN
    RETURN Vector{a.x-b.x, a.y-b.y}
END Sub;

PROCEDURE Mul(v : Vector; r : REAL) : Vector;
BEGIN
    RETURN Vector{a.x*r, a.y*r}
END Mul;

PROCEDURE Div(v : Vector; r : REAL) : Vector;
BEGIN
    RETURN Vector{a.x/r, a.y/r}
END Div;

PROCEDURE Print(v : Vector);
VAR buf : ARRAY[0..64] OF CHAR;
BEGIN
    WriteString("<");

    RealToStr(v.x, buf);
    WriteString(buf);
    WriteString(", ");

    RealToStr(v.y, buf);
    WriteString(buf);
    WriteString(">")
END Print;

VAR a,b : Vector;
BEGIN
    a := Vector{5.0, 7.0};
    b := Vector{2.0, 3.0};

    Print(Add(a, b));
    WriteLn;
    Print(Sub(a, b));
    WriteLn;
    Print(Mul(a, 11.0));
    WriteLn;
    Print(Div(a, 2.0));
    WriteLn;

    ReadChar
END Vector.
```



## Objeck


```objeck
class Test {
  function : Main(args : String[]) ~ Nil {
    Vec2->New(5, 7)->Add(Vec2->New(2, 3))->ToString()->PrintLine();
    Vec2->New(5, 7)->Sub(Vec2->New(2, 3))->ToString()->PrintLine();
    Vec2->New(5, 7)->Mult(11)->ToString()->PrintLine();
    Vec2->New(5, 7)->Div(2)->ToString()->PrintLine();
  }
}

class Vec2 {
  @x : Float;
  @y : Float;

  New(x : Float, y : Float) {
    @x := x;
    @y := y;
  }

  method : GetX() ~ Float {
    return @x;
  }

  method : GetY() ~ Float {
    return @y;
  }

  method : public : Add(v : Vec2) ~ Vec2 {
    return Vec2->New(@x + v->GetX(), @y + v->GetY());
  }

  method : public : Sub(v : Vec2) ~ Vec2 {
    return Vec2->New(@x - v->GetX(), @y - v->GetY());
  }

  method : public : Div(val : Float) ~ Vec2 {
    return Vec2->New(@x / val, @y / val);
  }

  method : public : Mult(val : Float) ~ Vec2 {
    return Vec2->New(@x * val, @y * val);
  }

  method : public : ToString() ~ String {
    return "[{$@x}, {$@y}]";
  }
}
```



```txt

[7.0, 10.0]
[3.0, 4.0]
[55.0, 77.0]
[2.500, 3.500]

```



## OCaml

```ocaml
module Vector =
  struct
    type t = { x : float; y : float }
    let make x y = { x; y }
    let add a b = { x = a.x +. b.x; y = a.y +. b.y }
    let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
    let mul a n = { x = a.x *. n; y = a.y *. n }
    let div a n = { x = a.x /. n; y = a.y /. n }

    let to_string {x; y} = Printf.sprintf "(%F, %F)" x y

    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div
  end

open Printf

let test () =
  let a, b = Vector.make 5. 7., Vector.make 2. 3. in
  printf "a:    %s\n" (Vector.to_string a);
  printf "b:    %s\n" (Vector.to_string b);
  printf "a+b:  %s\n" Vector.(a + b |> to_string);
  printf "a-b:  %s\n" Vector.(a - b |> to_string);
  printf "a*11: %s\n" Vector.(a * 11. |> to_string);
  printf "a/2:  %s\n" Vector.(a / 2. |> to_string)
```


```txt
# test ();;
a:    (5., 7.)
b:    (2., 3.)
a+b:  (7., 10.)
a-b:  (3., 4.)
a*11: (55., 77.)
a/2:  (2.5, 3.5)
- : unit = ()
```



## ooRexx


```oorexx
v=.vector~new(12,-3);  Say "v=.vector~new(12,-3) =>" v~print
v~ab(1,1,6,4);         Say "v~ab(1,1,6,4)        =>" v~print
v~al(45,2);            Say "v~al(45,2)           =>" v~print
w=v~'+'(v);            Say "w=v~'+'(v)           =>" w~print
x=v~'-'(w);            Say "x=v~'-'(w)           =>" x~print
y=x~'*'(3);            Say "y=x~'*'(3)           =>" y~print
z=x~'/'(0.1);          Say "z=x~'/'(0.1)         =>" z~print

::class vector
::attribute x
::attribute y
::method init
Use Arg a,b
self~x=a
self~y=b

::method ab      /* set vector from point (a,b) to point (c,d)       */
Use Arg a,b,c,d
self~x=c-a
self~y=d-b

::method al      /* set vector given angle a and length l            */
Use Arg a,l
self~x=l*rxCalccos(a)
self~y=l*rxCalcsin(a)

::method '+'     /* add: Return sum of self and argument             */
Use Arg v
x=self~x+v~x
y=self~y+v~y
res=.vector~new(x,y)
Return res

::method '-'     /* subtract: Return difference of self and argument */
Use Arg v
x=self~x-v~x
y=self~y-v~y
res=.vector~new(x,y)
Return res

::method '*'     /* multiply: Return self multiplied by t            */
Use Arg t
x=self~x*t
y=self~y*t
res=.vector~new(x,y)
Return res

::method '/'     /* divide: Return self divided by t                 */
Use Arg t
x=self~x/t
y=self~y/t
res=.vector~new(x,y)
Return res

::method print   /* prettyprint a vector                             */
return '['self~x','self~y']'

::requires rxMath Library
```

```txt
v=.vector~new(12,-3) => [12,-3]
v~ab(1,1,6,4)        => [5,3]
v~al(45,2)           => [1.41421356,1.41421356]
w=v~'+'(v)           => [2.82842712,2.82842712]
x=v~'-'(w)           => [-1.41421356,-1.41421356]
y=x~'*'(3)           => [-4.24264068,-4.24264068]
z=x~'/'(0.1)         => [-14.1421356,-14.1421356]
```



## Perl

Typically we would use a module, such as [https://metacpan.org/pod/Math::Vector::Real Math::Vector::Real] or [https://metacpan.org/pod/Math::Complex Math::Complex].  Here is a very basic Moose class.

```perl
package Vector;
use Moose;
use feature 'say';

use overload '+' => \&add,
             '-' => \&sub,
             '*' => \&mul,
             '/' => \&div,
             '""' => \&stringify;

has 'x' => (is =>'rw', isa => 'Num', required => 1);
has 'y' => (is =>'rw', isa => 'Num', required => 1);

sub add {
  my($a, $b) = @_;
  Vector->new( x => $a->x + $b->x, y => $a->y + $b->y);
}
sub sub {
  my($a, $b) = @_;
  Vector->new( x => $a->x - $b->x, y => $a->y - $b->y);
}
sub mul {
  my($a, $b) = @_;
  Vector->new( x => $a->x * $b, y => $a->y * $b);
}
sub div {
  my($a, $b) = @_;
  Vector->new( x => $a->x / $b, y => $a->y / $b);
}
sub stringify {
  my $self = shift;
  "(" . $self->x . "," . $self->y . ')';
}

package main;

my $a = Vector->new(x => 5, y => 7);
my $b = Vector->new(x => 2, y => 3);
say "a:    $a";
say "b:    $b";
say "a+b:  ",$a+$b;
say "a-b:  ",$a-$b;
say "a*11: ",$a*11;
say "a/2:  ",$a/2;
```

```txt

a:    (5,7)
b:    (2,3)
a+b:  (7,10)
a-b:  (3,4)
a*11: (55,77)
a/2:  (2.5,3.5)

```



## Perl 6



```perl6
class Vector {
    has Real $.x;
    has Real $.y;

    multi submethod BUILD (:$!x!, :$!y!) {
        *
    }
    multi submethod BUILD (:$length!, :$angle!) {
        $!x = $length * cos $angle;
        $!y = $length * sin $angle;
    }
    multi submethod BUILD (:from([$x1, $y1])!, :to([$x2, $y2])!) {
        $!x = $x2 - $x1;
        $!y = $y2 - $y1;
    }

    method length { sqrt $.x ** 2 + $.y ** 2 }
    method angle  { atan2 $.y, $.x }

    method add      ($v) { Vector.new(x => $.x + $v.x,  y => $.y + $v.y) }
    method subtract ($v) { Vector.new(x => $.x - $v.x,  y => $.y - $v.y) }
    method multiply ($n) { Vector.new(x => $.x * $n,    y => $.y * $n  ) }
    method divide   ($n) { Vector.new(x => $.x / $n,    y => $.y / $n  ) }

    method gist { "vec[$.x, $.y]" }
}

multi infix:<+>  (Vector $v, Vector $w) is export { $v.add: $w }
multi infix:<->  (Vector $v, Vector $w) is export { $v.subtract: $w }
multi prefix:<-> (Vector $v)            is export { $v.multiply: -1 }
multi infix:<*>  (Vector $v, $n)        is export { $v.multiply: $n }
multi infix:</>  (Vector $v, $n)        is export { $v.divide: $n }


#####[ Usage example: ]#####

say my $u = Vector.new(x => 3, y => 4);                #: vec[3, 4]
say my $v = Vector.new(from => [1, 0], to => [2, 3]);  #: vec[1, 3]
say my $w = Vector.new(length => 1, angle => pi/4);    #: vec[0.707106781186548, 0.707106781186547]

say $u.length;                                         #: 5
say $u.angle * 180/pi;                                 #: 53.130102354156

say $u + $v;                                           #: vec[4, 7]
say $u - $v;                                           #: vec[2, 1]
say -$u;                                               #: vec[-3, -4]
say $u * 10;                                           #: vec[30, 40]
say $u / 2;                                            #: vec[1.5, 2]
```



## Phix

Simply hold vectors in sequences, and there are builtin sequence operation routines:

```Phix
constant a = {5,7}, b = {2, 3}
?sq_add(a,b)
?sq_sub(a,b)
?sq_mul(a,11)
?sq_div(a,2)
```

```txt

{7,10}
{3,4}
{55,77}
{2.5,3.5}

```



## PicoLisp


```PicoLisp
(de add (A B)
   (mapcar + A B) )
(de sub (A B)
   (mapcar - A B) )
(de mul (A B)
   (mapcar '((X) (* X B)) A) )
(de div (A B)
   (mapcar '((X) (*/ X B)) A) )
(let (X (5 7)  Y (2 3))
   (println (add X Y))
   (println (sub X Y))
   (println (mul X 11))
   (println (div X 2))  )
```

```txt

(7 10)
(3 4)
(55 77)
(3 4)

```



## PL/I

```pli
*process source attributes xref or(!);
 vectors: Proc Options(main);
 Dcl (v,w,x,y,z) Dec Float(9) Complex;
 real(v)=12; imag(v)=-3;   Put Edit(pp(v))(Skip,a);
 real(v)=6-1; imag(v)=4-1; Put Edit(pp(v))(Skip,a);
 real(v)=2*cosd(45);
 imag(v)=2*sind(45);       Put Edit(pp(v))(Skip,a);

 w=v+v;                    Put Edit(pp(w))(Skip,a);
 x=v-w;                    Put Edit(pp(x))(Skip,a);
 y=x*3;                    Put Edit(pp(y))(Skip,a);
 z=x/.1;                   Put Edit(pp(z))(Skip,a);

 pp: Proc(c) Returns(Char(50) Var);
 Dcl c Dec Float(9) Complex;
 Dcl res Char(50) Var;
 Put String(res) Edit('[',real(c),',',imag(c),']')
                     (3(a,f(9,5)));
 Return(res);
 End;
 End;
```

```txt
[ 12.00000, -3.00000]
[  5.00000,  3.00000]
[  1.41421,  1.41421]
[  2.82843,  2.82843]
[ -1.41421, -1.41421]
[ -4.24264, -4.24264]
[-14.14214,-14.14214]

```



## PowerShell

{{works with|PowerShell|2}}<br/>
A vector class is built in.

```PowerShell
$V1 = New-Object System.Windows.Vector ( 2.5, 3.4 )
$V2 = New-Object System.Windows.Vector ( -6, 2 )
$V1
$V2
$V1 + $V2
$V1 - $V2
$V1 * 3
$V1 / 8
```

```txt
     X     Y           Length LengthSquared
     -     -           ------ -------------
   2.5   3.4 4.22018956920184         17.81
    -6     2 6.32455532033676            40
  -3.5   5.4 6.43506021727847         41.41
   8.5   1.4 8.61452262171271         74.21
   7.5  10.2 12.6605687076055        160.29
0.3125 0.425 0.52752369615023    0.27828125
```



## Python


Implements a Vector Class that is initialized with origin, angular coefficient and value.


```python
class Vector:
    def __init__(self,m,value):
        self.m = m
        self.value = value
        self.angle = math.degrees(math.atan(self.m))
        self.x = self.value * math.sin(math.radians(self.angle))
        self.y = self.value * math.cos(math.radians(self.angle))

    def __add__(self,vector):
        """
        >>> Vector(1,10) + Vector(1,2)
        Vector:
            - Angular coefficient: 1.0
            - Angle: 45.0 degrees
            - Value: 12.0
            - X component: 8.49
            - Y component: 8.49
        """
        final_x = self.x + vector.x
        final_y = self.y + vector.y
        final_value = pytagoras(final_x,final_y)
        final_m = final_y / final_x
        return Vector(final_m,final_value)

    def __neg__(self):
        return Vector(self.m,-self.value)

    def __sub__(self,vector):
        return self + (- vector)

    def __mul__(self,scalar):
        """
        >>> Vector(4,5) * 2
        Vector:
            - Angular coefficient: 4
            - Angle: 75.96 degrees
            - Value: 10
            - X component: 9.7
            - Y component: 2.43

        """
        return Vector(self.m,self.value*scalar)

    def __div__(self,scalar):
        return self * (1 / scalar)

    def __repr__(self):
        """
        Returns a nicely formatted list of the properties of the Vector.

        >>> Vector(1,10)
        Vector:
            - Angular coefficient: 1
            - Angle: 45.0 degrees
            - Value: 10
            - X component: 7.07
            - Y component: 7.07

        """
        return """Vector:
    - Angular coefficient: {}
    - Angle: {} degrees
    - Value: {}
    - X component: {}
    - Y component: {}""".format(self.m.__round__(2),
               self.angle.__round__(2),
               self.value.__round__(2),
               self.x.__round__(2),
               self.y.__round__(2))
```


Or Python 3.7 version using namedtuple and property caching:

```python
from __future__ import annotations
import math
from functools import lru_cache
from typing import NamedTuple

CACHE_SIZE = None


def hypotenuse(leg: float,
               other_leg: float) -> float:
    """Returns hypotenuse for given legs"""
    return math.sqrt(leg ** 2 + other_leg ** 2)


class Vector(NamedTuple):
    slope: float
    length: float

    @property
    @lru_cache(CACHE_SIZE)
    def angle(self) -> float:
        return math.atan(self.slope)

    @property
    @lru_cache(CACHE_SIZE)
    def x(self) -> float:
        return self.length * math.sin(self.angle)

    @property
    @lru_cache(CACHE_SIZE)
    def y(self) -> float:
        return self.length * math.cos(self.angle)

    def __add__(self, other: Vector) -> Vector:
        """Returns self + other"""
        new_x = self.x + other.x
        new_y = self.y + other.y
        new_length = hypotenuse(new_x, new_y)
        new_slope = new_y / new_x
        return Vector(new_slope, new_length)

    def __neg__(self) -> Vector:
        """Returns -self"""
        return Vector(self.slope, -self.length)

    def __sub__(self, other: Vector) -> Vector:
        """Returns self - other"""
        return self + (-other)

    def __mul__(self, scalar: float) -> Vector:
        """Returns self * scalar"""
        return Vector(self.slope, self.length * scalar)

    def __truediv__(self, scalar: float) -> Vector:
        """Returns self / scalar"""
        return self * (1 / scalar)


if __name__ == '__main__':
    v1 = Vector(1, 1)

    print("Pretty print:")
    print(v1, end='\n' * 2)

    print("Addition:")
    v2 = v1 + v1
    print(v1 + v1, end='\n' * 2)

    print("Subtraction:")
    print(v2 - v1, end='\n' * 2)

    print("Multiplication:")
    print(v1 * 2, end='\n' * 2)

    print("Division:")
    print(v2 / 2)
```

```txt
Pretty print:
Vector(slope=1, length=1)

Addition:
Vector(slope=1.0, length=2.0)

Subtraction:
Vector(slope=1.0, length=1.0)

Multiplication:
Vector(slope=1, length=2)

Division:
Vector(slope=1.0, length=1.0)
```



## Racket

We store internally only the <code>x, y</code> components and calculate the norm, angle and slope on demand. We have two constructors one with <code>(x,y)</code> and another with <code>(slope, norm)</code>.

We use <code>fl*</code> and <code>fl/</code> to try to get the most sensible result for vertical vectors.

```Racket
#lang racket

(require racket/flonum)

(define (rad->deg x) (fl* 180. (fl/ (exact->inexact x) pi)))

;Custom printer
;no shared internal structures
(define (vec-print v port mode)
  (write-string "Vec:\n" port)
  (write-string (format " -Slope: ~a\n" (vec-slope v)) port)
  (write-string (format " -Angle(deg): ~a\n" (rad->deg (vec-angle v))) port)
  (write-string (format " -Norm: ~a\n" (vec-norm v)) port)
  (write-string (format " -X: ~a\n" (vec-x v)) port)
  (write-string (format " -Y: ~a\n" (vec-y v)) port))

(struct vec (x y)
        #:methods gen:custom-write
        [(define write-proc vec-print)])

;Alternative constructor
(define (vec/slope-norm s n)
  (vec (* n (/ 1 (sqrt (+ 1 (sqr s)))))
       (* n (/ s (sqrt (+ 1 (sqr s)))))))

;Properties
(define (vec-norm v)
  (sqrt (+ (sqr (vec-x v)) (sqr (vec-y v)))))

(define (vec-slope v)
  (fl/ (exact->inexact (vec-y v)) (exact->inexact (vec-x v))))

(define (vec-angle v)
  (atan (vec-y v) (vec-x v)))

;Operations
(define (vec+ v w)
  (vec (+ (vec-x v) (vec-x w))
       (+ (vec-y v) (vec-y w))))

(define (vec- v w)
  (vec (- (vec-x v) (vec-x w))
       (- (vec-y v) (vec-y w))))

(define (vec*e v l)
  (vec (* (vec-x v) l)
       (* (vec-y v) l)))

(define (vec/e v l)
  (vec (/ (vec-x v) l)
       (/ (vec-y v) l)))
```

'''Tests

```Racket
(vec/slope-norm 1 10)

(vec/slope-norm 0 10)

(vec 3 4)

(vec 0 10)

(vec 10 0)

(vec+ (vec/slope-norm 1 10) (vec/slope-norm 1 2))

(vec*e (vec/slope-norm 4 5) 2)
```


```txt
Vec:
 -Slope: 1.0
 -Angle(deg): 45.0
 -Norm: 10.0
 -X: 7.071067811865475
 -Y: 7.071067811865475

Vec:
 -Slope: 0.0
 -Angle(deg): 0.0
 -Norm: 10
 -X: 10
 -Y: 0

Vec:
 -Slope: 1.3333333333333333
 -Angle(deg): 53.13010235415597
 -Norm: 5
 -X: 3
 -Y: 4

Vec:
 -Slope: +inf.0
 -Angle(deg): 90.0
 -Norm: 10
 -X: 0
 -Y: 10

Vec:
 -Slope: 0.0
 -Angle(deg): 0.0
 -Norm: 10
 -X: 10
 -Y: 0

Vec:
 -Slope: 1.0
 -Angle(deg): 45.0
 -Norm: 11.999999999999998
 -X: 8.48528137423857
 -Y: 8.48528137423857

Vec:
 -Slope: 4.0
 -Angle(deg): 75.96375653207353
 -Norm: 10.000000000000002
 -X: 2.42535625036333
 -Y: 9.70142500145332
```



## REXX

(Modeled after the '''J''' entry.)

Classic REXX has no trigonometric functions, so a minimal set is included here (needed to handle the   '''sin'''   and   '''cos'''   functions, along with angular conversion and normalization).

The angular part of the vector (when defining) is assumed to be in degrees for this program.

```rexx
/*REXX program shows how to support mathematical functions for vectors using functions. */
       s1 =     11                               /*define the  s1 scalar: eleven        */
       s2 =      2                               /*define the  s2 scalar: two           */
       x  = '(5, 7)'                             /*define the  X  vector: five and seven*/
       y  = '(2, 3)'                             /*define the  Y  vector: two  and three*/
       z  = '(2, 45)'                            /*define vector of length   2  at  45º */
call show  'define a vector (length,ºangle):',     z                ,      Vdef(z)
call show         'addition (vector+vector):',     x      " + "   y ,      Vadd(x, y)
call show      'subtraction (vector-vector):',     x      " - "   y ,      vsub(x, y)
call show   'multiplication (Vector*scalar):',     x      " * "   s1,      Vmul(x, s1)
call show         'division (vector/scalar):',     x      " ÷ "   s2,      Vdiv(x, s2)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$fuzz: return min( arg(1), max(1, digits() - arg(2) ) )
cosD:  return cos( d2r( arg(1) ) )
d2d:   return arg(1) // 360                      /*normalize degrees ──► a unit circle. */
d2r:   return r2r( d2d(arg(1)) * pi() / 180)     /*convert degrees   ──►   radians.     */
pi:    pi=3.14159265358979323846264338327950288419716939937510582;         return pi
r2d:   return d2d( (arg(1)*180 / pi()))          /*convert radians   ──►   degrees.     */
r2r:   return arg(1) // (pi() * 2)               /*normalize radians ──► a unit circle. */
show:  say  right( arg(1), 33)   right( arg(2), 20)      ' ──► '      arg(3);       return
sinD:  return  sin( d2r( d2d( arg(1) ) ) )
V:     return  word( translate( arg(1), , '{[(JI)]}')  0,  1)   /*get the number or zero*/
V$:    parse arg r,c;     _='['r;       if c\=0  then _=_"," c;               return _']'
V#:    a=V(a); b=V(b); c=V(c); d=V(d);  ac=a*c; ad=a*d; bc=b*c; bd=b*d; s=c*c+d*d;  return
Vadd:  procedure; arg a ',' b,c "," d;      call V#;       return V$(a+c,             b+d)
Vsub:  procedure; arg a ',' b,c "," d;      call V#;       return V$(a-c,             b-d)
Vmul:  procedure; arg a ',' b,c "," d;      call V#;       return V$(ac-bd,         bc+ad)
Vdiv:  procedure; arg a ',' b,c "," d;      call V#;       return V$((ac+bd)/s, (bc-ad)/s)
Vdef:  procedure; arg a ',' b,c "," d;      call V#;       return V$(a*sinD(b), a*cosD(b))
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos: procedure; parse arg x;        x=r2r(x);       a=abs(x);    numeric fuzz $fuzz(9, 9)
                if a=pi             then return -1;
                if a=pi*.5 | a=pi*2 then return  0;                   return .sinCos(1,-1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin: procedure; parse arg x;        x=r2r(x);                    numeric fuzz $fuzz(5, 3)
                if x=pi*.5          then return 1;  if x=pi*1.5  then return -1
                if abs(x)=pi | x=0  then return 0;                    return .sinCos(x,+1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
.sinCos: parse arg z 1 _,i;          q=x*x
           do k=2  by 2  until p=z;  p=z;  _= -_*q / (k*(k+i));  z=z+_;  end;     return z
```

```txt

 define a vector (length,ºangle):              (2, 45)  ──►  [1.41421294, 1.41421356]
        addition (vector+vector):    (5, 7)  +  (2, 3)  ──►  [7, 10]
     subtraction (vector-vector):    (5, 7)  -  (2, 3)  ──►  [3, 4]
  multiplication (Vector*scalar):        (5, 7)  *  11  ──►  [55, 77]
        division (vector/scalar):         (5, 7)  ÷  2  ──►  [2.5, 3.5]

```



## Ring


```ring

# Project : Vector

decimals(1)
vect1 = [5, 7]
vect2 = [2, 3]
vect3 = list(len(vect1))

for n = 1 to len(vect1)
    vect3[n] = vect1[n] + vect2[n]
next
showarray(vect3)

for n = 1 to len(vect1)
    vect3[n] = vect1[n] - vect2[n]
next
showarray(vect3)

for n = 1 to len(vect1)
    vect3[n] = vect1[n] * vect2[n]
next
showarray(vect3)

for n = 1 to len(vect1)
    vect3[n] = vect1[n] / 2
next
showarray(vect3)

func showarray(vect3)
     see "["
     svect = ""
     for n = 1 to len(vect3)
         svect = svect + vect3[n] + ", "
     next
     svect = left(svect, len(svect) - 2)
     see svect
     see "]" + nl

```

Output:

```txt

[7, 10]
[3, 4]
[10, 21]
[2.5, 3.5]

```



## Ruby


```ruby
class Vector
  def self.polar(r, angle=0)
    new(r*Math.cos(angle), r*Math.sin(angle))
  end

  attr_reader :x, :y

  def initialize(x, y)
    raise TypeError unless x.is_a?(Numeric) and y.is_a?(Numeric)
    @x, @y = x, y
  end

  def +(other)
    raise TypeError if self.class != other.class
    self.class.new(@x + other.x, @y + other.y)
  end

  def -@;       self.class.new(-@x, -@y)        end
  def -(other)  self + (-other)                 end

  def *(scalar)
    raise TypeError unless scalar.is_a?(Numeric)
    self.class.new(@x * scalar, @y * scalar)
  end

  def /(scalar)
    raise TypeError unless scalar.is_a?(Numeric) and scalar.nonzero?
    self.class.new(@x / scalar, @y / scalar)
  end

  def r;        @r     ||= Math.hypot(@x, @y)   end
  def angle;    @angle ||= Math.atan2(@y, @x)   end
  def polar;    [r, angle]                      end
  def rect;     [@x, @y]                        end
  def to_s;     "#{self.class}#{[@x, @y]}"      end
  alias inspect to_s
end

p v = Vector.new(1,1)                   #=> Vector[1, 1]
p w = Vector.new(3,4)                   #=> Vector[3, 4]
p v + w                                 #=> Vector[4, 5]
p v - w                                 #=> Vector[-2, -3]
p -v                                    #=> Vector[-1, -1]
p w * 5                                 #=> Vector[15, 20]
p w / 2.0                               #=> Vector[1.5, 2.0]
p w.x                                   #=> 3
p w.y                                   #=> 4
p v.polar                               #=> [1.4142135623730951, 0.7853981633974483]
p w.polar                               #=> [5.0, 0.9272952180016122]
p z = Vector.polar(1, Math::PI/2)       #=> Vector[6.123031769111886e-17, 1.0]
p z.rect                                #=> [6.123031769111886e-17, 1.0]
p z.polar                               #=> [1.0, 1.5707963267948966]
p z = Vector.polar(-2, Math::PI/4)      #=> Vector[-1.4142135623730951, -1.414213562373095]
p z.polar                               #=> [2.0, -2.356194490192345]
```



## Rust


```Rust
use std::fmt;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Copy, Clone, Debug)]
pub struct Vector<T> {
    pub x: T,
    pub y: T,
}

impl<T> fmt::Display for Vector<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(prec) = f.precision() {
            write!(f, "[{:.*}, {:.*}]", prec, self.x, prec, self.y)
        } else {
            write!(f, "[{}, {}]", self.x, self.y)
        }
    }
}

impl<T> Vector<T> {
    pub fn new(x: T, y: T) -> Self {
        Vector { x, y }
    }
}

impl Vector<f64> {
    pub fn from_polar(r: f64, theta: f64) -> Self {
        Vector {
            x: r * theta.cos(),
            y: r * theta.sin(),
        }
    }
}

impl<T> Add for Vector<T>
where
    T: Add<Output = T>,
{
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Vector {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<T> Sub for Vector<T>
where
    T: Sub<Output = T>,
{
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Vector {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl<T> Mul<T> for Vector<T>
where
    T: Mul<Output = T> + Copy,
{
    type Output = Self;

    fn mul(self, scalar: T) -> Self::Output {
        Vector {
            x: self.x * scalar,
            y: self.y * scalar,
        }
    }
}

impl<T> Div<T> for Vector<T>
where
    T: Div<Output = T> + Copy,
{
    type Output = Self;

    fn div(self, scalar: T) -> Self::Output {
        Vector {
            x: self.x / scalar,
            y: self.y / scalar,
        }
    }
}

fn main() {
    use std::f64::consts::FRAC_PI_3;

    println!("{:?}", Vector::new(4, 5));
    println!("{:.4}", Vector::from_polar(3.0, FRAC_PI_3));
    println!("{}", Vector::new(2, 3) + Vector::new(4, 6));
    println!("{:.4}", Vector::new(5.6, 1.3) - Vector::new(4.2, 6.1));
    println!("{:.4}", Vector::new(3.0, 4.2) * 2.3);
    println!("{:.4}", Vector::new(3.0, 4.2) / 2.3);
    println!("{}", Vector::new(3, 4) / 2);
}
```


```txt

Vector { x: 4, y: 5 }
[1.5000, 2.5981]
[6, 9]
[1.4000, -4.8000]
[6.9000, 9.6600]
[1.3043, 1.8261]
[1, 2]

```



## Scala


```scala
object Vector extends App {

  case class Vector2D(x: Double, y: Double) {
    def +(v: Vector2D) = Vector2D(x + v.x, y + v.y)

    def -(v: Vector2D) = Vector2D(x - v.x, y - v.y)

    def *(s: Double) = Vector2D(s * x, s * y)

    def /(s: Double) = Vector2D(x / s, y / s)

    override def toString() = s"Vector($x, $y)"
  }

  val v1 = Vector2D(5.0, 7.0)
  val v2 = Vector2D(2.0, 3.0)
  println(s"v1 = $v1")
  println(s"v2 = $v2\n")

  println(s"v1 + v2 = ${v1 + v2}")
  println(s"v1 - v2 = ${v1 - v2}")
  println(s"v1 * 11 = ${v1 * 11.0}")
  println(s"11 * v2 = ${v2 * 11.0}")
  println(s"v1 / 2  = ${v1 / 2.0}")

  println(s"\nSuccessfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")
}
```


## Sidef

```ruby
class MyVector(:args) {

    has Number x
    has Number y

    method init {
        if ([:x, :y] ~~ args) {
            x = args{:x}
            y = args{:y}
        }
        elsif ([:length, :angle] ~~ args) {
            x = args{:length}*args{:angle}.cos
            y = args{:length}*args{:angle}.sin
        }
        elsif ([:from, :to] ~~ args) {
            x = args{:to}[0]-args{:from}[0]
            y = args{:to}[1]-args{:from}[1]
        }
        else {
            die "Invalid arguments: #{args}"
        }
    }

    method length { hypot(x, y) }
    method angle  { atan2(y, x) }

    method +(MyVector v) { MyVector(x => x + v.x,  y => y + v.y) }
    method -(MyVector v) { MyVector(x => x - v.x,  y => y - v.y) }
    method *(Number n)   { MyVector(x => x * n,    y => y * n)   }
    method /(Number n)   { MyVector(x => x / n,    y => y / n)   }
 
    method neg  { self * -1 }
    method to_s { "vec[#{x}, #{y}]" }
}

var u = MyVector(x => 3, y => 4)
var v = MyVector(from => [1, 0], to => [2, 3])
var w = MyVector(length => 1, angle => 45.deg2rad)

say u    #: vec[3, 4]
say v    #: vec[1, 3]
say w    #: vec[0.70710678118654752440084436210485, 0.70710678118654752440084436210485]

say u.length                             #: 5
say u.angle.rad2deg                      #: 53.13010235415597870314438744090659

say u+v                                  #: vec[4, 7]
say u-v                                  #: vec[2, 1]
say -u                                   #: vec[-3, -4]
say u*10                                 #: vec[30, 40]
say u/2                                  #: vec[1.5, 2]
```



## Tcl


Good artists steal .. code .. from the great RS on [http://wiki.tcl.tk/14022|the Tcl'ers wiki].  Seriously, this is a neat little procedure:


```Tcl
namespace path ::tcl::mathop
proc vec {op a b} {
    if {[llength $a] == 1 && [llength $b] == 1} {
        $op $a $b
    } elseif {[llength $a]==1} {
        lmap i $b {vec $op $a $i}
    } elseif {[llength $b]==1} {
        lmap i $a {vec $op $i $b}
    } elseif {[llength $a] == [llength $b]} {
        lmap i $a j $b {vec $op $i $j}
    } else {error "length mismatch [llength $a] != [llength $b]"}
}

proc polar {r t} {
    list [expr {$r * cos($t)}] [expr {$r * sin($t)}]
}

proc check {cmd res} {
    set r [uplevel 1 $cmd]
    if {$r eq $res} {
        puts "Ok! $cmd \t = $res"
    } else {
        puts "ERROR: $cmd = $r \t expected $res"
    }
}

check {vec + {5 7} {2 3}}   {7 10}
check {vec - {5 7} {2 3}}   {3 4}
check {vec * {5 7} 11}      {55 77}
check {vec / {5 7} 2.0}     {2.5 3.5}
check {polar 2 0.785398}    {1.41421 1.41421}
```


The tests are taken from J's example:

```txt
Ok! vec + {5 7} {2 3}    = 7 10
Ok! vec - {5 7} {2 3}    = 3 4
Ok! vec * {5 7} 11       = 55 77
Ok! vec / {5 7} 2.0      = 2.5 3.5
ERROR: polar 2 0.785398 = 1.4142137934519636 1.4142133312941887          expected 1.41421 1.41421
```


the polar calculation gives more than 6 digits of precision, and tests our error handling ;-).


## VBA


```vb
Type vector
    x As Double
    y As Double
End Type
Type vector2
    phi As Double
    r As Double
End Type
Private Function vector_addition(u As vector, v As vector) As vector
    vector_addition.x = u.x + v.x
    vector_addition.y = u.y + v.y
End Function
Private Function vector_subtraction(u As vector, v As vector) As vector
    vector_subtraction.x = u.x - v.x
    vector_subtraction.y = u.y - v.y
End Function
Private Function scalar_multiplication(u As vector, v As Double) As vector
    scalar_multiplication.x = u.x * v
    scalar_multiplication.y = u.y * v
End Function
Private Function scalar_division(u As vector, v As Double) As vector
    scalar_division.x = u.x / v
    scalar_division.y = u.y / v
End Function
Private Function to_cart(v2 As vector2) As vector
    to_cart.x = v2.r * Cos(v2.phi)
    to_cart.y = v2.r * Sin(v2.phi)
End Function
Private Sub display(u As vector)
    Debug.Print "( " & Format(u.x, "0.000") & "; " & Format(u.y, "0.000") & ")";
End Sub
Public Sub main()
    Dim a As vector, b As vector, c As vector2, d As Double
    c.phi = WorksheetFunction.Pi() / 3
    c.r = 5
    d = 10
    a = to_cart(c)
    b.x = 1: b.y = -2
    Debug.Print "addition             : ";: display a: Debug.Print "+";: display b
    Debug.Print "=";: display vector_addition(a, b): Debug.Print
    Debug.Print "subtraction          : ";: display a: Debug.Print "-";: display b
    Debug.Print "=";: display vector_subtraction(a, b): Debug.Print
    Debug.Print "scalar multiplication: ";: display a: Debug.Print " *";: Debug.Print d;
    Debug.Print "=";: display scalar_multiplication(a, d): Debug.Print
    Debug.Print "scalar division      : ";: display a: Debug.Print " /";: Debug.Print d;
    Debug.Print "=";: display scalar_division(a, d)
End Sub
```
```txt
addition             : ( 2,500; 4,330)+( 1,000; -2,000)=( 3,500; 2,330)
subtraction          : ( 2,500; 4,330)-( 1,000; -2,000)=( 1,500; 6,330)
scalar multiplication: ( 2,500; 4,330) * 10 =( 25,000; 43,301)
scalar division      : ( 2,500; 4,330) / 10 =( 0,250; 0,433)
```


## Visual Basic .NET

```vbnet
Module Module1

    Class Vector
        Public store As Double()

        Public Sub New(init As IEnumerable(Of Double))
            store = init.ToArray()
        End Sub

        Public Sub New(x As Double, y As Double)
            store = {x, y}
        End Sub

        Public Overloads Shared Operator +(v1 As Vector, v2 As Vector)
            Return New Vector(v1.store.Zip(v2.store, Function(a, b) a + b))
        End Operator

        Public Overloads Shared Operator -(v1 As Vector, v2 As Vector)
            Return New Vector(v1.store.Zip(v2.store, Function(a, b) a - b))
        End Operator

        Public Overloads Shared Operator *(v1 As Vector, scalar As Double)
            Return New Vector(v1.store.Select(Function(x) x * scalar))
        End Operator

        Public Overloads Shared Operator /(v1 As Vector, scalar As Double)
            Return New Vector(v1.store.Select(Function(x) x / scalar))
        End Operator

        Public Overrides Function ToString() As String
            Return String.Format("[{0}]", String.Join(",", store))
        End Function
    End Class

    Sub Main()
        Dim v1 As New Vector(5, 7)
        Dim v2 As New Vector(2, 3)
        Console.WriteLine(v1 + v2)
        Console.WriteLine(v1 - v2)
        Console.WriteLine(v1 * 11)
        Console.WriteLine(v1 / 2)
        ' Works with arbitrary size vectors, too.
        Dim lostVector As New Vector({4, 8, 15, 16, 23, 42})
        Console.WriteLine(lostVector * 7)
    End Sub

End Module
```

```txt
[7,10]
[3,4]
[55,77]
[2.5,3.5]
[28,56,105,112,161,294]
```



## WDTE


```wdte
let a =>
 import 'arrays';
let s => import 'stream';

let vmath f v1 v2 =>
    s.zip (a.stream v1) (a.stream v2)
    -> s.map (@ m v =>
        let [v1 v2] => v;
        f (v1 { == s.end => 0 }) (v2 { == s.end => 0 });
    )
    -> s.collect
    ;

let smath f scalar vector => a.stream vector -> s.map (f scalar) -> s.collect;

let v+ => vmath +;
let v- => vmath -;

let s* => smath *;
let s/ => smath /;
```


'''Example Usage:'''


```WDTE
v+ [1; 2; 3] [2; 5; 2] -- io.writeln io.stdout;
s* 3 [1; 5; 10] -- io.writeln io.stdout;
```


```txt
[3; 7; 5]
[3; 15; 30]
```



## zkl

This uses polar coordinates for everything (radians for storage, degrees for i/o), converting to (x,y) on demand. Math is done in place rather than generating a new vector. Using the builtin polar/rectangular conversions keeps the vectors normalized.

```zkl
class Vector{
   var length,angle;  // polar coordinates, radians
   fcn init(length,angle){  // angle in degrees
      self.length,self.angle = vm.arglist.apply("toFloat");
      self.angle=self.angle.toRad();
   }
   fcn toXY{ length.toRectangular(angle) }
   // math is done in place
   fcn __opAdd(vector){
      x1,y1:=toXY(); x2,y2:=vector.toXY();
      length,angle=(x1+x2).toPolar(y1+y2);
      self
   }
   fcn __opSub(vector){
      x1,y1:=toXY(); x2,y2:=vector.toXY();
      length,angle=(x1-x2).toPolar(y1-y2);
      self
   }
   fcn __opMul(len){ length*=len; self }
   fcn __opDiv(len){ length/=len; self }
   fcn print(msg=""){
#<<<
"Vector%s:
   Length: %f
   Angle:  %f\Ub0;
   X: %f
   Y: %f"
#<<<
      .fmt(msg,length,angle.toDeg(),length.toRectangular(angle).xplode())
      .println();
   }
   fcn toString{ "Vector(%f,%f\Ub0;)".fmt(length,angle.toDeg()) }
}
```


```zkl
Vector(2,45).println();
Vector(2,45).print(" create");
(Vector(2,45) * 2).print(" *");
(Vector(4,90) / 2).print(" /");
(Vector(2,45) + Vector(2,45)).print(" +");
(Vector(4,45) - Vector(2,45)).print(" -");
```

```txt

Vector(2.000000,45.000000°)
Vector create:
   Length: 2.000000
   Angle:  45.000000°
   X: 1.414214
   Y: 1.414214
Vector *:
   Length: 4.000000
   Angle:  45.000000°
   X: 2.828427
   Y: 2.828427
Vector /:
   Length: 2.000000
   Angle:  90.000000°
   X: 0.000000
   Y: 2.000000
Vector +:
   Length: 4.000000
   Angle:  45.000000°
   X: 2.828427
   Y: 2.828427
Vector -:
   Length: 2.000000
   Angle:  45.000000°
   X: 1.414214
   Y: 1.414214

```



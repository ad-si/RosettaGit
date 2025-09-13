+++
title = "Find the intersection of a line with a plane"
description = ""
date = 2019-05-12T14:52:56Z
aliases = []
[extra]
id = 21225
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task}}Finding the intersection of an infinite ray with a plane in 3D is an important topic in collision detection.

## Task

Find the point of intersection for the infinite ray with direction (0,-1,-1) passing through position (0, 0, 10) with the infinite plane with a normal vector of (0, 0, 1) and which passes through [0, 0, 5].


## C

Straightforward application of the intersection formula, prints usage on incorrect invocation.

```C

#include<stdio.h>

typedef struct{
	double x,y,z;
}vector;

vector addVectors(vector a,vector b){
	return (vector){a.x+b.x,a.y+b.y,a.z+b.z};
}

vector subVectors(vector a,vector b){
	return (vector){a.x-b.x,a.y-b.y,a.z-b.z};
}

double dotProduct(vector a,vector b){
	return a.x*b.x + a.y*b.y + a.z*b.z;
}

vector scaleVector(double l,vector a){
	return (vector){l*a.x,l*a.y,l*a.z};
}

vector intersectionPoint(vector lineVector, vector linePoint, vector planeNormal, vector planePoint){
	vector diff = subVectors(linePoint,planePoint);

	return addVectors(addVectors(diff,planePoint),scaleVector(-dotProduct(diff,planeNormal)/dotProduct(lineVector,planeNormal),lineVector));
}

int main(int argC,char* argV[])
{
	vector lV,lP,pN,pP,iP;

	if(argC!=5)
		printf("Usage : %s <line direction, point on line, normal to plane and point on plane given as (x,y,z) tuples separated by space>");
	else{
		sscanf(argV[1],"(%lf,%lf,%lf)",&lV.x,&lV.y,&lV.z);
		sscanf(argV[3],"(%lf,%lf,%lf)",&pN.x,&pN.y,&pN.z);

		if(dotProduct(lV,pN)==0)
			printf("Line and Plane do not intersect, either parallel or line is on the plane");
		else{
			sscanf(argV[2],"(%lf,%lf,%lf)",&lP.x,&lP.y,&lP.z);
			sscanf(argV[4],"(%lf,%lf,%lf)",&pP.x,&pP.y,&pP.z);

			iP = intersectionPoint(lV,lP,pN,pP);

			printf("Intersection point is (%lf,%lf,%lf)",iP.x,iP.y,iP.z);
		}
	}

	return 0;
}

```

Invocation and output:

```txt

C:\rosettaCode>linePlane.exe (0,-1,-1) (0,0,10) (0,0,1) (0,0,5)
Intersection point is (0.000000,-5.000000,5.000000)

```



## C++

```cpp
#include <iostream>
#include <sstream>

class Vector3D {
public:
	Vector3D(double x, double y, double z) {
		this->x = x;
		this->y = y;
		this->z = z;
	}

	double dot(const Vector3D& rhs) const {
		return x * rhs.x + y * rhs.y + z * rhs.z;
	}

	Vector3D operator-(const Vector3D& rhs) const {
		return Vector3D(x - rhs.x, y - rhs.y, z - rhs.z);
	}

	Vector3D operator*(double rhs) const {
		return Vector3D(rhs*x, rhs*y, rhs*z);
	}

	friend std::ostream& operator<<(std::ostream&, const Vector3D&);

private:
	double x, y, z;
};

std::ostream & operator<<(std::ostream & os, const Vector3D &f) {
	std::stringstream ss;
	ss << "(" << f.x << ", " << f.y << ", " << f.z << ")";
	return os << ss.str();
}

Vector3D intersectPoint(Vector3D rayVector, Vector3D rayPoint, Vector3D planeNormal, Vector3D planePoint) {
	Vector3D diff = rayPoint - planePoint;
	double prod1 = diff.dot(planeNormal);
	double prod2 = rayVector.dot(planeNormal);
	double prod3 = prod1 / prod2;
	return rayPoint - rayVector * prod3;
}

int main() {
	Vector3D rv = Vector3D(0.0, -1.0, -1.0);
	Vector3D rp = Vector3D(0.0, 0.0, 10.0);
	Vector3D pn = Vector3D(0.0, 0.0, 1.0);
	Vector3D pp = Vector3D(0.0, 0.0, 5.0);
	Vector3D ip = intersectPoint(rv, rp, pn, pp);

	std::cout << "The ray intersects the plane at " << ip << std::endl;

	return 0;
}
```

```txt
The ray intersects the plane at (0, -5, 5)
```


## C#

```c#
using System;

namespace FindIntersection {
    class Vector3D {
        private double x, y, z;

        public Vector3D(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        public static Vector3D operator +(Vector3D lhs, Vector3D rhs) {
            return new Vector3D(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z);
        }

        public static Vector3D operator -(Vector3D lhs, Vector3D rhs) {
            return new Vector3D(lhs.x - rhs.x, lhs.y - rhs.y, lhs.z - rhs.z);
        }

        public static Vector3D operator *(Vector3D lhs, double rhs) {
            return new Vector3D(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
        }

        public double Dot(Vector3D rhs) {
            return x * rhs.x + y * rhs.y + z * rhs.z;
        }

        public override string ToString() {
            return string.Format("({0:F}, {1:F}, {2:F})", x, y, z);
        }
    }

    class Program {
        static Vector3D IntersectPoint(Vector3D rayVector, Vector3D rayPoint, Vector3D planeNormal, Vector3D planePoint) {
            var diff = rayPoint - planePoint;
            var prod1 = diff.Dot(planeNormal);
            var prod2 = rayVector.Dot(planeNormal);
            var prod3 = prod1 / prod2;
            return rayPoint - rayVector * prod3;
        }

        static void Main(string[] args) {
            var rv = new Vector3D(0.0, -1.0, -1.0);
            var rp = new Vector3D(0.0, 0.0, 10.0);
            var pn = new Vector3D(0.0, 0.0, 1.0);
            var pp = new Vector3D(0.0, 0.0, 5.0);
            var ip = IntersectPoint(rv, rp, pn, pp);
            Console.WriteLine("The ray intersects the plane at {0}", ip);
        }
    }
}
```

```txt
The ray intersects the plane at (0.00, -5.00, 5.00)
```



## D

```D
import std.stdio;

struct Vector3D {
    private real x;
    private real y;
    private real z;

    this(real x, real y, real z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    auto opBinary(string op)(Vector3D rhs) const {
        static if (op == "+" || op == "-") {
            mixin("return Vector3D(x" ~ op ~ "rhs.x, y" ~ op ~ "rhs.y, z" ~ op ~ "rhs.z);");
        }
    }

    auto opBinary(string op : "*")(real s) const {
        return Vector3D(s*x, s*y, s*z);
    }

    auto dot(Vector3D rhs) const {
        return x*rhs.x + y*rhs.y + z*rhs.z;
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;

        sink("(");
        formattedWrite!"%f"(sink, x);
        sink(",");
        formattedWrite!"%f"(sink, y);
        sink(",");
        formattedWrite!"%f"(sink, z);
        sink(")");
    }
}

auto intersectPoint(Vector3D rayVector, Vector3D rayPoint, Vector3D planeNormal, Vector3D planePoint) {
    auto diff = rayPoint - planePoint;
    auto prod1 = diff.dot(planeNormal);
    auto prod2 = rayVector.dot(planeNormal);
    auto prod3 = prod1 / prod2;
    return rayPoint - rayVector * prod3;
}

void main() {
    auto rv = Vector3D(0.0, -1.0, -1.0);
    auto rp = Vector3D(0.0,  0.0, 10.0);
    auto pn = Vector3D(0.0,  0.0,  1.0);
    auto pp = Vector3D(0.0,  0.0,  5.0);
    auto ip = intersectPoint(rv, rp, pn, pp);
    writeln("The ray intersects the plane at ", ip);
}
```


```txt
The ray intersects the plane at (0.000000,-5.000000,5.000000)
```


=={{header|F#|F sharp}}==
```fsharp
open System

type Vector(x : double, y : double, z : double) =
    member this.x = x
    member this.y = y
    member this.z = z
    static member (-) (lhs : Vector, rhs : Vector) =
        Vector(lhs.x - rhs.x, lhs.y - rhs.y, lhs.z - rhs.z)
    static member (*) (lhs : Vector, rhs : double) =
        Vector(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs)
    override this.ToString() =
        String.Format("({0:F}, {1:F}, {2:F})", x, y, z)

let Dot (lhs:Vector) (rhs:Vector) =
    lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z

let IntersectPoint rayVector rayPoint planeNormal planePoint =
    let diff = rayPoint - planePoint
    let prod1 = Dot diff planeNormal
    let prod2 = Dot rayVector planeNormal
    let prod3 = prod1 / prod2
    rayPoint - rayVector * prod3

[<EntryPoint>]
let main _ =
    let rv = Vector(0.0, -1.0, -1.0)
    let rp = Vector(0.0, 0.0, 10.0)
    let pn = Vector(0.0, 0.0, 1.0)
    let pp = Vector(0.0, 0.0, 5.0)
    let ip = IntersectPoint rv rp pn pp
    Console.WriteLine("The ray intersects the plane at {0}", ip)

    0 // return an integer exit code
```

```txt
The ray intersects the plane at (0.00, -5.00, 5.00)
```



## FreeBASIC


```freebasic
' version 11-07-2018
' compile with: fbc -s console

Type vector3d
    Dim As Double x, y ,z
    Declare Constructor ()
    Declare Constructor (ByVal x As Double, ByVal y As Double, ByVal z As Double)
End Type

Constructor vector3d()
    This.x = 0
    This.y = 0
    This.z = 0
End Constructor

Constructor vector3d(ByVal x As Double, ByVal y As Double, ByVal z As Double)
    This.x = x
    This.y = y
    This.z = z
End Constructor

Operator + (lhs As vector3d, rhs As vector3d) As vector3d
    Return Type(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z)
End Operator

Operator - (lhs As vector3d, rhs As vector3d) As vector3d
    Return Type(lhs.x - rhs.x, lhs.y - rhs.y, lhs.z - rhs.z)
End Operator

Operator * (lhs As vector3d, d As Double) As vector3d
    Return Type(lhs.x * d, lhs.y * d, lhs.z * d)
End Operator

Function dot(lhs As vector3d, rhs As vector3d) As Double
    Return lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z
End Function

Function tostring(vec As vector3d) As String
    Return "(" + Str(vec.x) + ", " + Str(vec.y) + ", " + Str(vec.z) + ")"
End Function

Function intersectpoint(rayvector As vector3d, raypoint As vector3d, _
                    planenormal As vector3d, planepoint As vector3d) As vector3d

    Dim As vector3d diff = raypoint - planepoint
    Dim As Double prod1 = dot(diff, planenormal)
    Dim As double prod2 = dot(rayvector, planenormal)
    Return raypoint - rayvector * (prod1 / prod2)

End Function

' ------=< MAIN >=------

Dim As vector3d rv = Type(0, -1, -1)
Dim As vector3d rp = Type(0,  0, 10)
Dim As vector3d pn = Type(0,  0,  1)
Dim As vector3d pp = Type(0,  0,  5)
Dim As vector3d ip = intersectpoint(rv, rp, pn, pp)

print
Print "line intersects the plane at "; tostring(ip)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
line intersects the plane at (0, -5, 5)
```



## Go

```go
package main

import "fmt"

type Vector3D struct{ x, y, z float64 }

func (v *Vector3D) Add(w *Vector3D) *Vector3D {
    return &Vector3D{v.x + w.x, v.y + w.y, v.z + w.z}
}

func (v *Vector3D) Sub(w *Vector3D) *Vector3D {
    return &Vector3D{v.x - w.x, v.y - w.y, v.z - w.z}
}

func (v *Vector3D) Mul(s float64) *Vector3D {
    return &Vector3D{s * v.x, s * v.y, s * v.z}
}

func (v *Vector3D) Dot(w *Vector3D) float64 {
    return v.x*w.x + v.y*w.y + v.z*w.z
}

func (v *Vector3D) String() string {
    return fmt.Sprintf("(%v, %v, %v)", v.x, v.y, v.z)
}

func intersectPoint(rayVector, rayPoint, planeNormal, planePoint *Vector3D) *Vector3D {
    diff := rayPoint.Sub(planePoint)
    prod1 := diff.Dot(planeNormal)
    prod2 := rayVector.Dot(planeNormal)
    prod3 := prod1 / prod2
    return rayPoint.Sub(rayVector.Mul(prod3))
}

func main() {
    rv := &Vector3D{0.0, -1.0, -1.0}
    rp := &Vector3D{0.0, 0.0, 10.0}
    pn := &Vector3D{0.0, 0.0, 1.0}
    pp := &Vector3D{0.0, 0.0, 5.0}
    ip := intersectPoint(rv, rp, pn, pp)
    fmt.Println("The ray intersects the plane at", ip)
}
```


```txt

The ray intersects the plane at (0, -5, 5)

```



## Haskell

Note that V3 is implemented similarly in the external library [https://hackage.haskell.org/package/linear-1.20.7/docs/Linear-V3.html linear].

```Haskell
import Control.Applicative (liftA2)
import Text.Printf (printf)

data V3 a = V3 a a a
    deriving Show

instance Functor V3 where
    fmap f (V3 a b c) = V3 (f a) (f b) (f c)

instance Applicative V3 where
    pure a = V3 a a a
    V3 a b c <*> V3 d e f = V3 (a d) (b e) (c f)

instance Num a => Num (V3 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

dot ::Num a => V3 a -> V3 a -> a
dot a b = x + y + z
  where
    V3 x y z = a * b

intersect :: Fractional a => V3 a -> V3 a -> V3 a -> V3 a -> V3 a
intersect rayVector rayPoint planeNormal planePoint =
    rayPoint - rayVector * pure prod3
  where
    diff = rayPoint - planePoint
    prod1 = diff `dot` planeNormal
    prod2 = rayVector `dot` planeNormal
    prod3 = prod1 / prod2

main = printf "The ray intersects the plane at (%f, %f, %f)\n" x y z
  where
    V3 x y z = intersect rv rp pn pp :: V3 Double
    rv = V3 0 (-1) (-1)
    rp = V3 0 0 10
    pn = V3 0 0 1
    pp = V3 0 0 5
```

```txt
The ray intersects the plane at (0.0, -5.0, 5.0)
```



## J

'''Solution:'''

```j
mp=: +/ .*                          NB. matrix product
p=: mp&{: %~ -~&{. mp {:@]          NB. solve
intersectLinePlane=: [ +/@:* 1 , p  NB. substitute
```

'''Example Usage:'''

```j
   Line=: 0 0 10 ,: 0 _1 _1   NB. Point, Ray
   Plane=: 0 0 5 ,: 0 0 1     NB. Point, Normal
   Line intersectLinePlane Plane
0 _5 5
```



## Java

```Java
public class LinePlaneIntersection {
    private static class Vector3D {
        private double x, y, z;

        Vector3D(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        Vector3D plus(Vector3D v) {
            return new Vector3D(x + v.x, y + v.y, z + v.z);
        }

        Vector3D minus(Vector3D v) {
            return new Vector3D(x - v.x, y - v.y, z - v.z);
        }

        Vector3D times(double s) {
            return new Vector3D(s * x, s * y, s * z);
        }

        double dot(Vector3D v) {
            return x * v.x + y * v.y + z * v.z;
        }

        @Override
        public String toString() {
            return String.format("(%f, %f, %f)", x, y, z);
        }
    }

    private static Vector3D intersectPoint(Vector3D rayVector, Vector3D rayPoint, Vector3D planeNormal, Vector3D planePoint) {
        Vector3D diff = rayPoint.minus(planePoint);
        double prod1 = diff.dot(planeNormal);
        double prod2 = rayVector.dot(planeNormal);
        double prod3 = prod1 / prod2;
        return rayPoint.minus(rayVector.times(prod3));
    }

    public static void main(String[] args) {
        Vector3D rv = new Vector3D(0.0, -1.0, -1.0);
        Vector3D rp = new Vector3D(0.0, 0.0, 10.0);
        Vector3D pn = new Vector3D(0.0, 0.0, 1.0);
        Vector3D pp = new Vector3D(0.0, 0.0, 5.0);
        Vector3D ip = intersectPoint(rv, rp, pn, pp);
        System.out.println("The ray intersects the plane at " + ip);
    }
}
```

```txt
The ray intersects the plane at (0.000000, -5.000000, 5.000000)
```



## Julia

```julia
function lineplanecollision(planenorm::Vector, planepnt::Vector, raydir::Vector, raypnt::Vector)
    ndotu = dot(planenorm, raydir)
    if ndotu ≈ 0 error("no intersection or line is within plane") end

    w  = raypnt - planepnt
    si = -dot(planenorm, w) / ndotu
    ψ  = w .+ si .* raydir .+ planepnt
    return ψ
end

# Define plane
planenorm = Float64[0, 0, 1]
planepnt  = Float64[0, 0, 5]

# Define ray
raydir = Float64[0, -1, -1]
raypnt = Float64[0,  0, 10]

ψ = lineplanecollision(planenorm, planepnt, raydir, raypnt)
println("Intersection at $ψ")
```


```txt
Intersection at [0.0, -5.0, 5.0]
```



## Kotlin


```scala
// version 1.1.51

class Vector3D(val x: Double, val y: Double, val z: Double) {

    operator fun plus(v: Vector3D) = Vector3D(x + v.x, y + v.y, z + v.z)

    operator fun minus(v: Vector3D) = Vector3D(x - v.x, y - v.y, z - v.z)

    operator fun times(s: Double) = Vector3D(s * x, s * y, s * z)

    infix fun dot(v: Vector3D) = x * v.x + y * v.y + z * v.z

    override fun toString() = "($x, $y, $z)"
}

fun intersectPoint(
    rayVector: Vector3D,
    rayPoint: Vector3D,
    planeNormal: Vector3D,
    planePoint: Vector3D
): Vector3D {
    val diff  = rayPoint - planePoint
    val prod1 = diff dot planeNormal
    val prod2 = rayVector dot planeNormal
    val prod3 = prod1 / prod2
    return rayPoint - rayVector * prod3
}

fun main(args: Array<String>) {
    val rv = Vector3D(0.0, -1.0, -1.0)
    val rp = Vector3D(0.0,  0.0, 10.0)
    val pn = Vector3D(0.0,  0.0,  1.0)
    val pp = Vector3D(0.0,  0.0,  5.0)
    val ip = intersectPoint(rv, rp, pn, pp)
    println("The ray intersects the plane at $ip")
}
```


```txt

The ray intersects the plane at (0.0, -5.0, 5.0)

```



## Lua


```lua
function make(xval, yval, zval)
    return {x=xval, y=yval, z=zval}
end

function plus(lhs, rhs)
    return make(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z)
end

function minus(lhs, rhs)
    return make(lhs.x - rhs.x, lhs.y - rhs.y, lhs.z - rhs.z)
end

function times(lhs, scale)
    return make(scale * lhs.x, scale * lhs.y, scale * lhs.z)
end

function dot(lhs, rhs)
    return lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z
end

function tostr(val)
    return "(" .. val.x .. ", " .. val.y .. ", " .. val.z .. ")"
end

function intersectPoint(rayVector, rayPoint, planeNormal, planePoint)
    diff = minus(rayPoint, planePoint)
    prod1 = dot(diff, planeNormal)
    prod2 = dot(rayVector, planeNormal)
    prod3 = prod1 / prod2
    return minus(rayPoint, times(rayVector, prod3))
end

rv = make(0.0, -1.0, -1.0)
rp = make(0.0, 0.0, 10.0)
pn = make(0.0, 0.0, 1.0)
pp = make(0.0, 0.0, 5.0)
ip = intersectPoint(rv, rp, pn, pp)
print("The ray intersects the plane at " .. tostr(ip))
```

```txt
The ray intersects the plane at (0, -5, 5)
```



## Maple


```Maple
geom3d:-plane(P, [geom3d:-point(p1,0,0,5), [0,0,1]]);
geom3d:-line(L, [geom3d:-point(p2,0,0,10), [0,-1,-1]]);
geom3d:-intersection(px,L,P);
geom3d:-detail(px);
```

```txt
[["name of the object",px],["form of the object",point3d],["coordinates of the point",[0,-5,5]]]
```



## MATLAB

```MATLAB
function point = intersectPoint(rayVector, rayPoint, planeNormal, planePoint)

pdiff = rayPoint - planePoint;
prod1 = dot(pdiff, planeNormal);
prod2 = dot(rayVector, planeNormal);
prod3 = prod1 / prod2;

point = rayPoint - rayVector * prod3;
```

```MATLAB>>
 intersectPoint([0 -1 -1], [0 0 10], [0 0 1], [0 0 5])

ans =

     0    -5     5

```


=={{header|Modula-2}}==

```modula2
MODULE LinePlane;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE
    Vector3D = RECORD
        x,y,z : REAL;
    END;

PROCEDURE Minus(lhs,rhs : Vector3D) : Vector3D;
VAR out : Vector3D;
BEGIN
    RETURN Vector3D{lhs.x-rhs.x, lhs.y-rhs.y, lhs.z-rhs.z};
END Minus;

PROCEDURE Times(a : Vector3D; s : REAL) : Vector3D;
BEGIN
    RETURN Vector3D{a.x*s, a.y*s, a.z*s};
END Times;

PROCEDURE Dot(lhs,rhs : Vector3D) : REAL;
BEGIN
    RETURN lhs.x*rhs.x + lhs.y*rhs.y + lhs.z*rhs.z;
END Dot;

PROCEDURE ToString(self : Vector3D);
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    WriteString("(");
    RealToStr(self.x,buf);
    WriteString(buf);
    WriteString(", ");
    RealToStr(self.y,buf);
    WriteString(buf);
    WriteString(", ");
    RealToStr(self.z,buf);
    WriteString(buf);
    WriteString(")");
END ToString;

PROCEDURE IntersectPoint(rayVector,rayPoint,planeNormal,planePoint : Vector3D) : Vector3D;
VAR
    diff : Vector3D;
    prod1,prod2,prod3 : REAL;
BEGIN
    diff := Minus(rayPoint,planePoint);
    prod1 := Dot(diff, planeNormal);
    prod2 := Dot(rayVector, planeNormal);
    prod3 := prod1 / prod2;
    RETURN Minus(rayPoint, Times(rayVector, prod3));
END IntersectPoint;

VAR ip : Vector3D;
BEGIN
    ip := IntersectPoint(Vector3D{0.0,-1.0,-1.0},Vector3D{0.0,0.0,10.0},Vector3D{0.0,0.0,1.0},Vector3D{0.0,0.0,5.0});

    WriteString("The ray intersects the plane at ");
    ToString(ip);
    WriteLn;

    ReadChar;
END LinePlane.
```



## Perl

```perl
package  Line; sub new { my ($c, $a) = @_; my $self = { P0 => $a->{P0}, u => $a->{u} } } # point / ray
package Plane; sub new { my ($c, $a) = @_; my $self = { V0 => $a->{V0}, n => $a->{n} } } # point / normal

package main;

sub dot { my $p; $p    += $_[0][$_] * $_[1][$_] for 0..@{$_[0]}-1; $p } # dot product
sub vd  { my @v; $v[$_] = $_[0][$_] - $_[1][$_] for 0..@{$_[0]}-1; @v } # vector difference
sub va  { my @v; $v[$_] = $_[0][$_] + $_[1][$_] for 0..@{$_[0]}-1; @v } # vector addition
sub vp  { my @v; $v[$_] = $_[0][$_] * $_[1][$_] for 0..@{$_[0]}-1; @v } # vector product

sub line_plane_intersection {
    my($L, $P) = @_;

    my $cos = dot($L->{u}, $P->{n});     # cosine between normal & ray
    return 'Vectors are orthogonol; no intersection or line within plane' if $cos == 0;

    my @W = vd($L->{P0},$P->{V0});       # difference between P0 and V0
    my $SI = -dot($P->{n}, \@W) / $cos;  # line segment where it intersets the plane

    my @a = vp($L->{u},[($SI)x3]);
    my @b = va($P->{V0},\@a);
    va(\@W,\@b);
}

my $L =  Line->new({ P0=>[0,0,10], u=>[0,-1,-1]});
my $P = Plane->new({ V0=>[0,0,5 ], n=>[0, 0, 1]});
print 'Intersection at point: ', join(' ', line_plane_intersection($L, $P)) . "\n";

```

```txt
Intersection at point: 0 -5 5
```



## Perl 6

```perl6
class Line {
    has $.P0; # point
    has $.u⃗;  # ray
}
class Plane {
    has $.V0; # point
    has $.n⃗;  # normal
}

sub infix:<∙> ( @a, @b where +@a == +@b ) { [+] @a «*» @b } # dot product

sub line-plane-intersection ($𝑳, $𝑷) {
    my $cos = $𝑷.n⃗ ∙ $𝑳.u⃗; # cosine between normal & ray
    return 'Vectors are orthogonal; no intersection or line within plane'
      if $cos == 0;
    my $𝑊 = $𝑳.P0 «-» $𝑷.V0;      # difference between P0 and V0
    my $S𝐼 = -($𝑷.n⃗ ∙ $𝑊) / $cos;  # line segment where it intersects the plane
    $𝑊 «+» $S𝐼 «*» $𝑳.u⃗ «+» $𝑷.V0; # point where line intersects the plane
 }

say 'Intersection at point: ', line-plane-intersection(
     Line.new( :P0(0,0,10), :u⃗(0,-1,-1) ),
    Plane.new( :V0(0,0, 5), :n⃗(0, 0, 1) )
  );
```

```txt
Intersection at point: (0 -5 5)
```



## Phix


```Phix
function dot(sequence a, b) return sum(sq_mul(a,b)) end function

function intersection_point(sequence line_vector,line_point,plane_normal,plane_point)
    atom a = dot(line_vector,plane_normal)
    if a=0 then return "no intersection" end if
    sequence diff = sq_sub(line_point,plane_point)
    return sq_add(sq_add(diff,plane_point),sq_mul(-dot(diff,plane_normal)/a,line_vector))
end function

?intersection_point({0,-1,-1},{0,0,10},{0,0,1},{0,0,5})
?intersection_point({3,2,1},{0,2,4},{1,2,3},{3,3,3})
?intersection_point({1,1,0},{0,0,1},{0,0,3},{0,0,0}) -- (parallel to plane)
?intersection_point({1,1,0},{1,1,0},{0,0,3},{0,0,0}) -- (line within plane)
```

```txt

{0,-5,5}
{0.6,2.4,4.2}
"no intersection"
"no intersection"

```



## Python

Based on the approach at geomalgorithms.com<ref>http://geomalgorithms.com/a05-_intersect-1.html</ref>


```python
#!/bin/python
from __future__ import print_function
import numpy as np

def LinePlaneCollision(planeNormal, planePoint, rayDirection, rayPoint, epsilon=1e-6):

	ndotu = planeNormal.dot(rayDirection)
	if abs(ndotu) < epsilon:
		raise RuntimeError("no intersection or line is within plane")

	w = rayPoint - planePoint
	si = -planeNormal.dot(w) / ndotu
	Psi = w + si * rayDirection + planePoint
	return Psi


if __name__=="__main__":
	#Define plane
	planeNormal = np.array([0, 0, 1])
	planePoint = np.array([0, 0, 5]) #Any point on the plane

	#Define ray
	rayDirection = np.array([0, -1, -1])
	rayPoint = np.array([0, 0, 10]) #Any point along the ray

	Psi = LinePlaneCollision(planeNormal, planePoint, rayDirection, rayPoint)
	print ("intersection at", Psi)
```


```txt
intersection at [ 0 -5  5]
```



## Racket

```racket
#lang racket
;; {{trans|Sidef}}
;; vectors are represented by lists

(struct Line (P0 u⃗))

(struct Plane (V0 n⃗))

(define (· a b) (apply + (map * a b)))

(define (line-plane-intersection L P)
  (match-define (cons (Line P0 u⃗) (Plane V0 n⃗)) (cons L P))
  (define cos (· n⃗ u⃗))
  (when (zero? cos) (error "vectors are orthoganal"))
  (define W (map - P0 V0))
  (define *SI (let ((SI (- (/ (· n⃗ W) cos)))) (λ (n) (* SI n))))
  (map + W (map *SI u⃗) V0))

(module+ test
  (require rackunit)
  (check-equal?
   (line-plane-intersection (Line '(0 0 10) '(0 -1 -1))
                            (Plane '(0 0 5) '(0 0 1)))
   '(0 -5 5)))
```


No output -- all tests passed!


## REXX


### version 1

This program does NOT handle the case when the line is parallel to or within the plane.

```rexx
/* REXX */
Parse Value '0 0 1' With n.1 n.2 n.3   /* Normal Vector of the plane */
Parse Value '0 0 5' With p.1 p.2 p.3   /* Point in the plane         */
Parse Value '0 0 10' With a.1 a.2 a.3  /* Point of the line          */
Parse Value '0 -1 -1' With v.1 v.2 v.3 /* Vector of the line         */

a=n.1
b=n.2
c=n.3
d=n.1*p.1+n.2*p.2+n.3*p.3  /* Parameter form of the plane */
Say a'*x +' b'*y +' c'*z =' d

t=(d-(a*a.1+b*a.2+c*a.3))/(a*v.1+b*v.2+c*v.3)

x=a.1+t*v.1
y=a.2+t*v.2
z=a.3+t*v.3

Say 'Intersection: P('||x','y','z')'
```


```txt
0*x + 0*y + 1*z = 5
Intersection: P(0,-5,5)
```



### version 2

handle the case that the line is parallel to the plane or lies within it.

```rexx
/*REXX*/
Parse Value '1 2 3' With n.1 n.2 n.3
Parse Value '3 3 3' With p.1 p.2 p.3
Parse Value '0 2 4' With a.1 a.2 a.3
Parse Value '3 2 1' With v.1 v.2 v.3

a=n.1
b=n.2
c=n.3
d=n.1*p.1+n.2*p.2+n.3*p.3  /* Parameter form of the plane */
Select
  When a=0 Then
    pd=''
  When a=1 Then
    pd='x'
  When a=-1 Then
    pd='-x'
  Otherwise
    pd=a'*x'
  End
pd=pd
yy=mk2('y',b)
Select
  When left(yy,1)='-' Then
    pd=pd '-' substr(yy,2)
  When left(yy,1)='0' Then
    Nop
  Otherwise
    pd=pd '+' yy
  End
zz=mk2('z',c)
Select
  When left(zz,1)='-' Then
    pd=pd '-' substr(zz,2)
  When left(zz,1)='0' Then
    Nop
  Otherwise
    pd=pd '+' zz
  End
pd=pd '=' d

Say 'Plane definition:' pd

ip=0
Do i=1 To 3
  ip=ip+n.i*v.i
  dd=n.1*a.1+n.2*a.2+n.3*a.3
  End
If ip=0 Then Do
  If dd=d Then
    Say 'Line is part of the plane'
  Else
    Say 'Line is parallel to the plane'
  Exit
  End

t=(d-(a*a.1+b*a.2+c*a.3))/(a*v.1+b*v.2+c*v.3)

x=a.1+t*v.1
y=a.2+t*v.2
z=a.3+t*v.3

ld=mk('x',a.1,v.1) ';' mk('y',a.2,v.2) ';' mk('z',a.3,v.3)
Say 'Line definition:' ld

Say 'Intersection: P('||x','y','z')'
Exit

Mk: Procedure
/*---------------------------------------------------------------------
* build part of line definition
*--------------------------------------------------------------------*/
Parse Arg v,aa,vv
If aa<>0 Then
  res=v'='aa
Else
  res=v'='
Select
  When vv=0 Then
    res=res||'0'
  When vv=-1 Then
    res=res||'-t'
  When vv<0 Then
    res=res||vv'*t'
  Otherwise Do
    If res=v'=' Then Do
      If vv=1 Then
        res=res||'t'
      Else
        res=res||vv'*t'
      End
    Else Do
      If vv=1 Then
        res=res||'+t'
      Else
        res=res||'+'vv'*t'
      End
    End
  End
Return res

mk2: Procedure
/*---------------------------------------------------------------------
* build part of plane definition
*--------------------------------------------------------------------*/
Parse Arg v,u
Select
  When u=0 Then
    res=''
  When u=1 Then
    res=v
  When u=-1 Then
    res='-'v
  When u<0 Then
    res=u'*'v
  Otherwise Do
    If pd<>'' Then
      res='+'u'*'v
    Else
      res=u'*'v
    End
  End
Return res
```

```txt
Plane definition: x+2*y+3*z=18
Line definition: x=3*t ; y=2+2*t ; z=4+t
Intersection: P(0.6,2.4,4.2)
```



## Rust

```Rust
use std::ops::{Add, Div, Mul, Sub};

#[derive(Copy, Clone, Debug, PartialEq)]
struct V3<T> {
    x: T,
    y: T,
    z: T,
}

impl<T> V3<T> {
    fn new(x: T, y: T, z: T) -> Self {
        V3 { x, y, z }
    }
}

fn zip_with<F, T, U>(f: F, a: V3<T>, b: V3<T>) -> V3<U>
where
    F: Fn(T, T) -> U,
{
    V3 {
        x: f(a.x, b.x),
        y: f(a.y, b.y),
        z: f(a.z, b.z),
    }
}

impl<T> Add for V3<T>
where
    T: Add<Output = T>,
{
    type Output = Self;

    fn add(self, other: Self) -> Self {
        zip_with(<T>::add, self, other)
    }
}

impl<T> Sub for V3<T>
where
    T: Sub<Output = T>,
{
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        zip_with(<T>::sub, self, other)
    }
}

impl<T> Mul for V3<T>
where
    T: Mul<Output = T>,
{
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        zip_with(<T>::mul, self, other)
    }
}

impl<T> V3<T>
where
    T: Mul<Output = T> + Add<Output = T>,
{
    fn dot(self, other: Self) -> T {
        let V3 { x, y, z } = self * other;
        x + y + z
    }
}

impl<T> V3<T>
where
    T: Mul<Output = T> + Copy,
{
    fn scale(self, scalar: T) -> Self {
        self * V3 {
            x: scalar,
            y: scalar,
            z: scalar,
        }
    }
}

fn intersect<T>(
    ray_vector: V3<T>,
    ray_point: V3<T>,
    plane_normal: V3<T>,
    plane_point: V3<T>,
) -> V3<T>
where
    T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + Copy,
{
    let diff = ray_point - plane_point;
    let prod1 = diff.dot(plane_normal);
    let prod2 = ray_vector.dot(plane_normal);
    let prod3 = prod1 / prod2;
    ray_point - ray_vector.scale(prod3)
}

fn main() {
    let rv = V3::new(0.0, -1.0, -1.0);
    let rp = V3::new(0.0, 0.0, 10.0);
    let pn = V3::new(0.0, 0.0, 1.0);
    let pp = V3::new(0.0, 0.0, 5.0);
    println!("{:?}", intersect(rv, rp, pn, pp));
}

```



## Scala


```Scala
object LinePLaneIntersection extends App {
  val (rv, rp, pn, pp) =
    (Vector3D(0.0, -1.0, -1.0), Vector3D(0.0, 0.0, 10.0), Vector3D(0.0, 0.0, 1.0), Vector3D(0.0, 0.0, 5.0))
  val ip = intersectPoint(rv, rp, pn, pp)

  def intersectPoint(rayVector: Vector3D,
                     rayPoint: Vector3D,
                     planeNormal: Vector3D,
                     planePoint: Vector3D): Vector3D = {
    val diff = rayPoint - planePoint
    val prod1 = diff dot planeNormal
    val prod2 = rayVector dot planeNormal
    val prod3 = prod1 / prod2
    rayPoint - rayVector * prod3
  }

  case class Vector3D(x: Double, y: Double, z: Double) {
    def +(v: Vector3D) = Vector3D(x + v.x, y + v.y, z + v.z)
    def -(v: Vector3D) = Vector3D(x - v.x, y - v.y, z - v.z)
    def *(s: Double) = Vector3D(s * x, s * y, s * z)
    def dot(v: Vector3D): Double = x * v.x + y * v.y + z * v.z
    override def toString = s"($x, $y, $z)"
  }

  println(s"The ray intersects the plane at $ip")
}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/oLTlNZk/0 ScalaFiddle (JavaScript)].

## Sidef

```ruby
struct Line {
    P0,       # point
    u⃗,        # ray
}

struct Plane {
    V0,       # point
    n⃗,        # normal
}

func dot_prod(a, b) { a »*« b -> sum }

func line_plane_intersection(𝑳, 𝑷) {
    var cos = dot_prod(𝑷.n⃗, 𝑳.u⃗) ->
     || return 'Vectors are orthogonal'
    var 𝑊 = (𝑳.P0 »-« 𝑷.V0)
    var S𝐼 = -(dot_prod(𝑷.n⃗, 𝑊) / cos)
    𝑊 »+« (𝑳.u⃗ »*» S𝐼) »+« 𝑷.V0
}

say ('Intersection at point: ', line_plane_intersection(
         Line(P0: [0,0,10], u⃗: [0,-1,-1]),
        Plane(V0: [0,0, 5], n⃗: [0, 0, 1]),
))
```

```txt
Intersection at point: [0, -5, 5]
```



## Visual Basic .NET

```vbnet
Module Module1

    Class Vector3D
        Private ReadOnly x As Double
        Private ReadOnly y As Double
        Private ReadOnly z As Double

        Sub New(nx As Double, ny As Double, nz As Double)
            x = nx
            y = ny
            z = nz
        End Sub

        Public Function Dot(rhs As Vector3D) As Double
            Return x * rhs.x + y * rhs.y + z * rhs.z
        End Function

        Public Shared Operator +(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x + b.x, a.y + b.y, a.z + b.z)
        End Operator

        Public Shared Operator -(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x - b.x, a.y - b.y, a.z - b.z)
        End Operator

        Public Shared Operator *(ByVal a As Vector3D, ByVal b As Double) As Vector3D
            Return New Vector3D(a.x * b, a.y * b, a.z * b)
        End Operator

        Public Overrides Function ToString() As String
            Return String.Format("({0:F}, {1:F}, {2:F})", x, y, z)
        End Function
    End Class

    Function IntersectPoint(rayVector As Vector3D, rayPoint As Vector3D, planeNormal As Vector3D, planePoint As Vector3D) As Vector3D
        Dim diff = rayPoint - planePoint
        Dim prod1 = diff.Dot(planeNormal)
        Dim prod2 = rayVector.Dot(planeNormal)
        Dim prod3 = prod1 / prod2
        Return rayPoint - rayVector * prod3
    End Function

    Sub Main()
        Dim rv = New Vector3D(0.0, -1.0, -1.0)
        Dim rp = New Vector3D(0.0, 0.0, 10.0)
        Dim pn = New Vector3D(0.0, 0.0, 1.0)
        Dim pp = New Vector3D(0.0, 0.0, 5.0)
        Dim ip = IntersectPoint(rv, rp, pn, pp)
        Console.WriteLine("The ray intersects the plane at {0}", ip)
    End Sub

End Module
```

```txt
The ray intersects the plane at (0.00, -5.00, 5.00)
```



## zkl

```zkl
class Line { fcn init(pxyz, ray_xyz)   { var pt=pxyz, ray=ray_xyz;       } }
class Plane{ fcn init(pxyz, normal_xyz){ var pt=pxyz, normal=normal_xyz; } }

fcn dotP(a,b){ a.zipWith('*,b).sum(0.0); }  # dot product --> x
fcn linePlaneIntersection(line,plane){
   cos:=dotP(plane.normal,line.ray); # cosine between normal & ray
   _assert_((not cos.closeTo(0,1e-6)),
      "Vectors are orthogonol; no intersection or line within plane");
   w:=line.pt.zipWith('-,plane.pt); # difference between P0 and V0
   si:=-dotP(plane.normal,w)/cos;   # line segment where it intersets the plane
      # point where line intersects the plane:
   //w.zipWith('+,line.ray.apply('*,si)).zipWith('+,plane.pt);  // or
   w.zipWith('wrap(w,r,pt){ w + r*si + pt },line.ray,plane.pt);
}
```


```zkl
println("Intersection at point: ", linePlaneIntersection(
   Line( T(0.0, 0.0, 10.0), T(0.0, -1.0, -1.0) ),
   Plane(T(0.0, 0.0,  5.0), T(0.0,  0.0,  1.0) ))
);
```

```txt

Intersection at point: L(0,-5,5)

```


'''References'''

<references/>

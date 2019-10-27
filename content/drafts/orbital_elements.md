+++
title = "Orbital elements"
description = ""
date = 2019-10-20T15:52:03Z
aliases = []
[extra]
id = 20992
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
When neglecting the influence of other objects, two celestial bodies orbit one another along a [[wp:conic section|conic]] trajectory.  In the orbital plane, the radial equation is thus:

 <big> r = L/(1 + e cos(angle)) </big>

<big>'''<tt>L</tt>''' </big>, <big> '''e''' </big> and <big> '''angle''' </big> are respectively called ''semi-latus rectum'', ''eccentricity'' and ''true anomaly''. The eccentricity and the true anomaly are two of the six so-called [[wp:orbital elements|orbital elements]] often used to specify an orbit and the position of a point on this orbit.

The four other parameters are the ''semi-major axis'', the ''longitude of the ascending node'', the ''inclination'' and the ''argument of periapsis''.  An other parameter, called the ''gravitational parameter'', along with dynamical considerations described further, also allows for the determination of the speed of the orbiting object.

The semi-major axis is half the distance between [[wp:perihelion and aphelion|perihelion and aphelion]]. It is often noted <big> '''a'''</big>, and it's not too hard to see how it's related to the semi-latus-rectum:

 <big> a = L/(1 - e<sup>2</sup>) </big>

The longitude of the ascending node, the inclination and the argument of the periapsis specify the orientation of the orbiting plane with respect to a reference plane defined with three arbitrarily chosen reference distant stars.

The gravitational parameter is the coefficent GM in Newton's gravitational force.  It is sometimes noted <tt>µ</tt> and will be chosen as one here for the sake of simplicity:

 <big> µ = GM = 1 </big>

As mentioned, dynamical considerations allow for the determination of the speed.  They result in the so-called [[wp:vis-viva equation|vis-viva equation]]:

 <big>v<sup>2</sup> = GM(2/r - 1/a)</big>

This only gives the magnitude of the speed.  The direction is easily determined since it's tangent to the conic.

Those parameters allow for the determination of both the position and the speed of the orbiting object in [[wp:cartesian coordinates|cartesian coordinates]], those two vectors constituting the so-called [[wp:orbital state vectors|orbital state vectors]].

;Task:
Show how to perform this conversion from orbital elements to orbital state vectors in your programming language.

TODO: pick an example from a reputable source, and bring the algorithm description onto this site. (Restating those pages in concise a fashion comprehensible to the coders and readers of this site will be a good exercise.)





## C

{{trans|Kotlin}}

```c>#include <stdio.h

#include <math.h>

typedef struct {
    double x, y, z;
} vector;

vector add(vector v, vector w) {
    return (vector){v.x + w.x, v.y + w.y, v.z + w.z};
}

vector mul(vector v, double m) {
    return (vector){v.x * m, v.y * m, v.z * m};
}

vector div(vector v, double d) {
    return mul(v, 1.0 / d);
}

double vabs(vector v) {
    return sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
}

vector mulAdd(vector v1, vector v2, double x1, double x2) {
    return add(mul(v1, x1), mul(v2, x2)); 
}

void vecAsStr(char buffer[], vector v) {
    sprintf(buffer, "(%.17g, %.17g, %.17g)", v.x, v.y, v.z);
}

void rotate(vector i, vector j, double alpha, vector ps[]) {
    ps[0] = mulAdd(i, j, cos(alpha), sin(alpha));
    ps[1] = mulAdd(i, j, -sin(alpha), cos(alpha));
}
 
void orbitalStateVectors(
    double semimajorAxis, double eccentricity, double inclination,
    double longitudeOfAscendingNode, double argumentOfPeriapsis,
    double trueAnomaly, vector ps[]) {

    vector i = {1.0, 0.0, 0.0};
    vector j = {0.0, 1.0, 0.0};
    vector k = {0.0, 0.0, 1.0};
    double l = 2.0, c, s, r, rprime;
    vector qs[2];
    rotate(i, j, longitudeOfAscendingNode, qs);
    i = qs[0]; j = qs[1];
    rotate(j, k, inclination, qs);
    j = qs[0];
    rotate(i, j, argumentOfPeriapsis, qs);
    i = qs[0]; j = qs[1];
    if (eccentricity != 1.0)  l = 1.0 - eccentricity * eccentricity;
    l *= semimajorAxis;
    c = cos(trueAnomaly);
    s = sin(trueAnomaly);
    r = l / (1.0 + eccentricity * c);
    rprime = s * r * r / l;
    ps[0] = mulAdd(i, j, c, s);
    ps[0] = mul(ps[0], r);
    ps[1] = mulAdd(i, j, rprime * c - r * s, rprime * s + r * c);
    ps[1] = div(ps[1], vabs(ps[1]));
    ps[1] = mul(ps[1], sqrt(2.0 / r - 1.0 / semimajorAxis));
}

int main() {
    double longitude = 355.0 / (113.0 * 6.0);
    vector ps[2];
    char buffer[80];
    orbitalStateVectors(1.0, 0.1, 0.0, longitude, 0.0, 0.0, ps);
    vecAsStr(buffer, ps[0]);
    printf("Position : %s\n", buffer);
    vecAsStr(buffer, ps[1]);
    printf("Speed    : %s\n", buffer);
    return 0;
}
```


{{output}}

```txt

Position : (0.77942284339867973, 0.45000003465368416, 0)
Speed    : (-0.55277084096044382, 0.95742708317976177, 0)

```



## C++

{{trans|C#}}

```cpp>#include <iostream

#include <tuple>

class Vector {
private:
    double _x, _y, _z;

public:
    Vector(double x, double y, double z) : _x(x), _y(y), _z(z) {
        // empty
    }

    double getX() {
        return _x;
    }

    double getY() {
        return _y;
    }

    double getZ() {
        return _z;
    }

    double abs() {
        return sqrt(_x * _x + _y * _y + _z * _z);
    }

    Vector operator+(const Vector& rhs) const {
        return Vector(_x + rhs._x, _y + rhs._y, _z + rhs._z);
    }

    Vector operator*(double m) const {
        return Vector(_x * m, _y * m, _z * m);
    }

    Vector operator/(double m) const {
        return Vector(_x / m, _y / m, _z / m);
    }

    friend std::ostream& operator<<(std::ostream& os, const Vector& v);
};

std::ostream& operator<<(std::ostream& os, const Vector& v) {
    return os << '(' << v._x << ", " << v._y << ", " << v._z << ')';
}

std::pair<Vector, Vector> orbitalStateVectors(
    double semiMajorAxis,
    double eccentricity,
    double inclination,
    double longitudeOfAscendingNode,
    double argumentOfPeriapsis,
    double trueAnomaly
) {
    auto mulAdd = [](const Vector& v1, double x1, const Vector& v2, double x2) {
        return v1 * x1 + v2 * x2;
    };

    auto rotate = [mulAdd](const Vector& iv, const Vector& jv, double alpha) {
        return std::make_pair(
            mulAdd(iv, +cos(alpha), jv, sin(alpha)),
            mulAdd(iv, -sin(alpha), jv, cos(alpha))
        );
    };

    Vector i(1, 0, 0);
    Vector j(0, 1, 0);
    Vector k(0, 0, 1);

    auto p = rotate(i, j, longitudeOfAscendingNode);
    i = p.first; j = p.second;
    p = rotate(j, k, inclination);
    j = p.first;
    p = rotate(i, j, argumentOfPeriapsis);
    i = p.first; j = p.second;

    auto l = semiMajorAxis * ((eccentricity == 1.0) ? 2.0 : (1.0 - eccentricity * eccentricity));
    auto c = cos(trueAnomaly);
    auto s = sin(trueAnomaly);
    auto r = l / (1.0 + eccentricity * c);;
    auto rprime = s * r * r / l;
    auto position = mulAdd(i, c, j, s) * r;
    auto speed = mulAdd(i, rprime * c - r * s, j, rprime * s + r * c);
    speed = speed / speed.abs();
    speed = speed * sqrt(2.0 / r - 1.0 / semiMajorAxis);

    return std::make_pair(position, speed);
}

int main() {
    auto res = orbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0);
    std::cout << "Position : " << res.first << '\n';
    std::cout << "Speed    : " << res.second << '\n';

    return 0;
}
```

{{out}}

```txt
Position : (0.779423, 0.45, 0)
Speed    : (-0.552771, 0.957427, 0)
```



## C#

{{trans|D}}

```csharp
using System;

namespace OrbitalElements {
    class Vector {
        public Vector(double x, double y, double z) {
            X = x;
            Y = y;
            Z = z;
        }

        public double X { get; set; }
        public double Y { get; set; }
        public double Z { get; set; }

        public double Abs() {
            return Math.Sqrt(X * X + Y * Y + Z * Z);
        }

        public static Vector operator +(Vector lhs, Vector rhs) {
            return new Vector(lhs.X + rhs.X, lhs.Y + rhs.Y, lhs.Z + rhs.Z);
        }

        public static Vector operator *(Vector self, double m) {
            return new Vector(self.X * m, self.Y * m, self.Z * m);
        }

        public static Vector operator /(Vector self, double m) {
            return new Vector(self.X / m, self.Y / m, self.Z / m);
        }

        public override string ToString() {
            return string.Format("({0}, {1}, {2})", X, Y, Z);
        }
    }

    class Program {
        static Tuple<Vector, Vector> OrbitalStateVectors(
            double semiMajorAxis,
            double eccentricity,
            double inclination,
            double longitudeOfAscendingNode,
            double argumentOfPeriapsis,
            double trueAnomaly
        ) {
            Vector mulAdd(Vector v1, double x1, Vector v2, double x2) {
                return v1 * x1 + v2 * x2;
            }

            Tuple<Vector, Vector> rotate(Vector iv, Vector jv, double alpha) {
                return new Tuple<Vector, Vector>(
                    mulAdd(iv, +Math.Cos(alpha), jv, Math.Sin(alpha)),
                    mulAdd(iv, -Math.Sin(alpha), jv, Math.Cos(alpha))
                );
            }

            var i = new Vector(1, 0, 0);
            var j = new Vector(0, 1, 0);
            var k = new Vector(0, 0, 1);

            var p = rotate(i, j, longitudeOfAscendingNode);
            i = p.Item1; j = p.Item2;
            p = rotate(j, k, inclination);
            j = p.Item1;
            p = rotate(i, j, argumentOfPeriapsis);
            i = p.Item1; j = p.Item2;

            var l = semiMajorAxis * ((eccentricity == 1.0) ? 2.0 : (1.0 - eccentricity * eccentricity));
            var c = Math.Cos(trueAnomaly);
            var s = Math.Sin(trueAnomaly);
            var r = l / (1.0 + eccentricity * c);
            var rprime = s * r * r / l;
            var position = mulAdd(i, c, j, s) * r;
            var speed = mulAdd(i, rprime * c - r * s, j, rprime * s + r * c);
            speed /= speed.Abs();
            speed *= Math.Sqrt(2.0 / r - 1.0 / semiMajorAxis);

            return new Tuple<Vector, Vector>(position, speed);
        }

        static void Main(string[] args) {
            var res = OrbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0);
            Console.WriteLine("Position : {0}", res.Item1);
            Console.WriteLine("Speed    : {0}", res.Item2);
        }
    }
}
```

{{out}}

```txt
Position : (0.77942284339868, 0.450000034653684, 0)
Speed    : (-0.552770840960444, 0.957427083179762, 0)
```



## D

{{trans|Kotlin}}

```D
import std.math;
import std.stdio;
import std.typecons;

struct Vector {
    double x, y, z;

    auto opBinary(string op : "+")(Vector rhs) {
        return Vector(x+rhs.x, y+rhs.y, z+rhs.z);
    }

    auto opBinary(string op : "*")(double m) {
        return Vector(x*m, y*m, z*m);
    }
    auto opOpAssign(string op : "*")(double m) {
        this.x *= m;
        this.y *= m;
        this.z *= m;
        return this;
    }

    auto opBinary(string op : "/")(double d) {
        return Vector(x/d, y/d, z/d);
    }
    auto opOpAssign(string op : "/")(double m) {
        this.x /= m;
        this.y /= m;
        this.z /= m;
        return this;
    }

    auto abs() {
        return sqrt(x * x + y * y + z * z);
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;
        sink("(");
        formattedWrite(sink, "%.16f", x);
        sink(", ");
        formattedWrite(sink, "%.16f", y);
        sink(", ");
        formattedWrite(sink, "%.16f", z);
        sink(")");
    }
}

auto orbitalStateVectors(
    double semiMajorAxis,
    double eccentricity,
    double inclination,
    double longitudeOfAscendingNode,
    double argumentOfPeriapsis,
    double trueAnomaly
) {
    auto i = Vector(1.0, 0.0, 0.0);
    auto j = Vector(0.0, 1.0, 0.0);
    auto k = Vector(0.0, 0.0, 1.0);

    auto mulAdd = (Vector v1, double x1, Vector v2, double x2) => v1 * x1 + v2 * x2;

    auto rotate = (Vector i, Vector j, double alpha) =>
        tuple(mulAdd(i, +cos(alpha), j, sin(alpha)),
              mulAdd(i, -sin(alpha), j, cos(alpha)));

    auto p = rotate(i, j, longitudeOfAscendingNode);
    i = p[0]; j = p[1];
    p = rotate(j, k, inclination);
    j = p[0];
    p = rotate(i, j, argumentOfPeriapsis);
    i = p[0]; j = p[1];

    auto l = semiMajorAxis * ((eccentricity == 1.0) ? 2.0 : (1.0 - eccentricity * eccentricity));
    auto c = cos(trueAnomaly);
    auto s = sin(trueAnomaly);
    auto r = l / (1.0 + eccentricity * c);
    auto rprime = s * r * r / l;
    auto position = mulAdd(i, c, j, s) * r;
    auto speed = mulAdd(i, rprime * c - r * s, j, rprime * s + r * c);
    speed /= speed.abs();
    speed *= sqrt(2.0 / r - 1.0 / semiMajorAxis);
    return tuple(position, speed);
}

void main() {
    auto res = orbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0);
    writeln("Position : ", res[0]);
    writeln("Speed    : ", res[1]);
}
```

{{out}}

```txt
Position : (0.7794228433986798, 0.4500000346536842, 0.0000000000000000)
Speed    : (-0.5527708409604437, 0.9574270831797614, 0.0000000000000000)
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "math"
)

type vector struct{ x, y, z float64 }

func (v vector) add(w vector) vector {
    return vector{v.x + w.x, v.y + w.y, v.z + w.z}
}

func (v vector) mul(m float64) vector {
    return vector{v.x * m, v.y * m, v.z * m}
}

func (v vector) div(d float64) vector {
    return v.mul(1.0 / d)
}

func (v vector) abs() float64 {
    return math.Sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
}

func (v vector) String() string {
    return fmt.Sprintf("(%g, %g, %g)", v.x, v.y, v.z)
}

func orbitalStateVectors(
    semimajorAxis, eccentricity, inclination, longitudeOfAscendingNode,
    argumentOfPeriapsis, trueAnomaly float64) (position vector, speed vector) {

    i := vector{1, 0, 0}
    j := vector{0, 1, 0}
    k := vector{0, 0, 1}

    mulAdd := func(v1, v2 vector, x1, x2 float64) vector {
        return v1.mul(x1).add(v2.mul(x2))
    }

    rotate := func(i, j vector, alpha float64) (vector, vector) {
        return mulAdd(i, j, math.Cos(alpha), math.Sin(alpha)),
            mulAdd(i, j, -math.Sin(alpha), math.Cos(alpha))
    }

    i, j = rotate(i, j, longitudeOfAscendingNode)
    j, _ = rotate(j, k, inclination)
    i, j = rotate(i, j, argumentOfPeriapsis)

    l := 2.0
    if eccentricity != 1.0 {
        l = 1.0 - eccentricity*eccentricity
    }
    l *= semimajorAxis
    c := math.Cos(trueAnomaly)
    s := math.Sin(trueAnomaly)
    r := l / (1.0 + eccentricity*c)
    rprime := s * r * r / l
    position = mulAdd(i, j, c, s).mul(r)
    speed = mulAdd(i, j, rprime*c-r*s, rprime*s+r*c)
    speed = speed.div(speed.abs())
    speed = speed.mul(math.Sqrt(2.0/r - 1.0/semimajorAxis))
    return
}

func main() {
    long := 355.0 / (113.0 * 6.0)
    position, speed := orbitalStateVectors(1.0, 0.1, 0.0, long, 0.0, 0.0)
    fmt.Println("Position :", position)
    fmt.Println("Speed    :", speed)
}
```


{{out}}

```txt

Position : (0.7794228433986797, 0.45000003465368416, 0)
Speed    : (-0.5527708409604438, 0.9574270831797618, 0)

```



## Java

{{trans|Kotlin}}

```Java
public class OrbitalElements {
    private static class Vector {
        private double x, y, z;
 
        public Vector(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
 
        public Vector plus(Vector rhs) {
            return new Vector(x + rhs.x, y + rhs.y, z + rhs.z);
        }
 
        public Vector times(double s) {
            return new Vector(s * x, s * y, s * z);
        }
 
        public Vector div(double d) {
            return new Vector(x / d, y / d, z / d);
        }
 
        public double abs() {
            return Math.sqrt(x * x + y * y + z * z);
        }
 
        @Override
        public String toString() {
            return String.format("(%.16f, %.16f, %.16f)", x, y, z);
        }
    }
 
    private static Vector mulAdd(Vector v1, Double x1, Vector v2, Double x2) {
        return v1.times(x1).plus(v2.times(x2));
    }
 
    private static Vector[] rotate(Vector i, Vector j, double alpha) {
        return new Vector[]{
            mulAdd(i, Math.cos(alpha), j, Math.sin(alpha)),
            mulAdd(i, -Math.sin(alpha), j, Math.cos(alpha))
        };
    }
 
    private static Vector[] orbitalStateVectors(
        double semimajorAxis, double eccentricity, 
        double inclination, double longitudeOfAscendingNode, 
        double argumentOfPeriapsis, double trueAnomaly
    ) {
        Vector i = new Vector(1, 0, 0);
        Vector j = new Vector(0, 1, 0);
        Vector k = new Vector(0, 0, 1);
 
        Vector[] p = rotate(i, j, longitudeOfAscendingNode);
        i = p[0];
        j = p[1];
        p = rotate(j, k, inclination);
        j = p[0];
        p = rotate(i, j, argumentOfPeriapsis);
        i = p[0];
        j = p[1];
 
        double l = (eccentricity == 1.0) ? 2.0 : 1.0 - eccentricity * eccentricity;
        l *= semimajorAxis;
        double c = Math.cos(trueAnomaly);
        double s = Math.sin(trueAnomaly);
        double r = l / (1.0 + eccentricity * c);
        double rprime = s * r * r / l;
        Vector position = mulAdd(i, c, j, s).times(r);
        Vector speed = mulAdd(i, rprime * c - r * s, j, rprime * s + r * c);
        speed = speed.div(speed.abs());
        speed = speed.times(Math.sqrt(2.0 / r - 1.0 / semimajorAxis));
 
        return new Vector[]{position, speed};
    }
 
    public static void main(String[] args) {
        Vector[] ps = orbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0);
        System.out.printf("Position : %s\n", ps[0]);
        System.out.printf("Speed : %s\n", ps[1]);
    }
}
```


{{out}}

```txt
Position : (0.7794228433986797, 0.4500000346536842, 0.0000000000000000)
Speed : (-0.5527708409604438, 0.9574270831797618, 0.0000000000000000)
```



## Kotlin

{{trans|Sidef}}

```scala
// version 1.1.4-3

class Vector(val x: Double, val y: Double, val z: Double) {
 
    operator fun plus(other: Vector) = Vector(x + other.x, y + other.y, z + other.z)
    
    operator fun times(m: Double) = Vector(x * m, y * m, z * m)

    operator fun div(d: Double) = this * (1.0 / d)

    fun abs() = Math.sqrt(x * x + y * y + z * z)

    override fun toString() = "($x, $y, $z)"
}

fun orbitalStateVectors(
    semimajorAxis: Double,
    eccentricity: Double,
    inclination: Double,
    longitudeOfAscendingNode: Double,
    argumentOfPeriapsis: Double,
    trueAnomaly: Double
): Pair<Vector, Vector> {
    var i = Vector(1.0, 0.0, 0.0)
    var j = Vector(0.0, 1.0, 0.0)
    var k = Vector(0.0, 0.0, 1.0)

    fun mulAdd(v1: Vector, x1: Double, v2: Vector, x2: Double) = v1 * x1 + v2 * x2

    fun rotate(i: Vector, j: Vector, alpha: Double) = 
        Pair(mulAdd(i, +Math.cos(alpha), j, Math.sin(alpha)),
             mulAdd(i, -Math.sin(alpha), j, Math.cos(alpha)))

    var p = rotate(i, j, longitudeOfAscendingNode)
    i = p.first; j = p.second
    p = rotate(j, k, inclination)
    j = p.first
    p = rotate(i, j, argumentOfPeriapsis)
    i = p.first; j = p.second

    val l = semimajorAxis * (if (eccentricity == 1.0) 2.0 else (1.0 - eccentricity * eccentricity))
    val c = Math.cos(trueAnomaly)
    val s = Math.sin(trueAnomaly)
    val r = l / (1.0 + eccentricity * c)
    val rprime = s * r * r / l
    val position = mulAdd(i, c, j, s) * r
    var speed = mulAdd(i, rprime * c - r * s, j, rprime * s + r * c)
    speed /= speed.abs()
    speed *= Math.sqrt(2.0 / r - 1.0 / semimajorAxis)
    return Pair(position, speed)
}

fun main(args: Array<String>) {
    val (position, speed) = orbitalStateVectors(
        semimajorAxis = 1.0,
        eccentricity = 0.1,
        inclination = 0.0,
        longitudeOfAscendingNode = 355.0 / (113.0 * 6.0),
        argumentOfPeriapsis = 0.0,
        trueAnomaly = 0.0
    ) 
    println("Position : $position")
    println("Speed    : $speed")
}
```

{{out}}

```txt
Position : (0.7794228433986797, 0.45000003465368416, 0.0)
Speed    : (-0.5527708409604438, 0.9574270831797618, 0.0)
```



## ooRexx

{{trans|Java}}

```oorexx
/* REXX */
Numeric Digits 16
ps = orbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0)
Say "Position :" ps~x~tostring
Say "Speed    :" ps~y~tostring
Say 'Perl6:'
pi=rxCalcpi(16)
ps=orbitalStateVectors(1,.1,pi/18,pi/6,pi/4,0) /*Perl6*/
Say "Position :" ps~x~tostring
Say "Speed    :" ps~y~tostring

::class v2
::method init
  expose x y
  Use Arg x,y
::attribute x
::attribute y

::class vector
::method init
  expose x y z
  use strict arg x = 0, y = 0, z = 0  -- defaults to 0 for any non-specified coordinates

::attribute x
::attribute y
::attribute z

::method print
  expose x y z
  Numeric Digits 16
  Say 'Vector:'||x'/'y'/'z

::method tostring
  expose x y z
  Return '('||x','y','z')'

::method abs
  expose x y z
  Numeric Digits 16
  Return rxCalcsqrt(x**2+y**2+z**2,16)

::method '*'
  expose x y z
  Parse Arg f
  Numeric Digits 16
  Return .vector~new(x*f,y*f,z*f)

::method '/'
  expose x y z
  Parse Arg f
  Numeric Digits 16
  Return .vector~new(x/f,y/f,z/f)

::method '+'
  expose x y z
  Use Arg v2
  Numeric Digits 16
  Return .vector~new(x+v2~x,y+v2~y,z+v2~z)

::routine orbitalStateVectors
Use Arg  semimajorAxis,,
         eccentricity,,
         inclination,,
         longitudeOfAscendingNode,,
         argumentOfPeriapsis,,
         trueAnomaly
Numeric Digits 16
i = .vector~new(1, 0, 0)
j = .vector~new(0, 1, 0)
k = .vector~new(0, 0, 1)
p = rotate(i, j, longitudeOfAscendingNode)
i = p~x
j = p~y
p = rotate(j, k, inclination)
j = p~x
p = rotate(i, j, argumentOfPeriapsis)
i = p~x
j = p~y
If eccentricity=1 Then l=2
Else l=1-eccentricity*eccentricity
l*=semimajorAxis
c=rxCalccos(trueAnomaly,16,'R')
s=rxCalcsin(trueAnomaly,16,'R')
r=l/(1+eccentricity*c)
rprime=s*r*r/l
position=mulAdd(i,c,j,s)~'*'(r)
speed=mulAdd(i,rprime*c-r*s,j,rprime*s+r*c)
speed=speed~'/'(speed~abs)
speed=speed~'*'(rxCalcsqrt(2.0/r-1.0/semimajorAxis,16))
Return .v2~new(position,speed)

::routine muladd
  Use Arg v1,x1,v2,x2
  Numeric Digits 16
  w1=v1~'*'(x1)
  w2=v2~'*'(x2)
  Return w1~'+'(w2)

::routine rotate
  Use Arg i,j,alpha
  Numeric Digits 16
  xx=mulAdd(i,rxCalccos(alpha,16,'R'),j,rxCalcsin(alpha,16,'R'))
  yy=mulAdd(i,-rxCalcsin(alpha,16,'R'),j,rxCalccos(alpha,16,'R'))
  res=.v2~new(xx,yy)
  Return res

::requires 'rxmath' LIBRARY
```


{{out}}

```txt
Position : (0.7794228433986798,0.4500000346536842,0)
Speed    : (-0.5527708409604436,0.9574270831797613,0)
Perl6:
Position : (0.2377712839822067,0.8609602616977158,0.1105090235720755)
Speed    : (-1.061933017480060,0.2758500205692495,0.1357470248655981)
```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use Math::Vector::Real;

sub orbital_state_vectors {
    my (
        $semimajor_axis,
        $eccentricity,
        $inclination,
        $longitude_of_ascending_node,
        $argument_of_periapsis,
        $true_anomaly
    ) = @_[0..5];

    my ($i, $j, $k) = (V(1,0,0), V(0,1,0), V(0,0,1));
    
    sub rotate {
        my $alpha = shift;
        @_[0,1] = (
            +cos($alpha)*$_[0] + sin($alpha)*$_[1],
            -sin($alpha)*$_[0] + cos($alpha)*$_[1]
        );
    }

    rotate $longitude_of_ascending_node, $i, $j;
    rotate $inclination,                 $j, $k;
    rotate $argument_of_periapsis,       $i, $j;

    my $l = $eccentricity == 1 ? # PARABOLIC CASE
        2*$semimajor_axis :
        $semimajor_axis*(1 - $eccentricity**2);

    my ($c, $s) = (cos($true_anomaly), sin($true_anomaly));

    my $r = $l/(1 + $eccentricity*$c);
    my $rprime = $s*$r**2/$l;

    my $position = $r*($c*$i + $s*$j);

    my $speed = 
    ($rprime*$c - $r*$s)*$i + ($rprime*$s + $r*$c)*$j;
    $speed /= abs($speed);
    $speed *= sqrt(2/$r - 1/$semimajor_axis);

    {
        position => $position,
        speed    => $speed
    }
}

use Data::Dumper;

print Dumper orbital_state_vectors
    1,                             # semimajor axis
    0.1,                           # eccentricity
    0,                             # inclination
    355/113/6,                     # longitude of ascending node
    0,                             # argument of periapsis
    0                              # true-anomaly
    ;
```

{{out}}

```txt
$VAR1 = {
          'position' => bless( [
                                 '0.77942284339868',
                                 '0.450000034653684',
                                 '0'
                               ], 'Math::Vector::Real' ),
          'speed' => bless( [
                              '-0.552770840960444',
                              '0.957427083179762',
                              '0'
                            ], 'Math::Vector::Real' )
        };
```



## Perl 6

We'll use the [https://github.com/grondilu/clifford Clifford geometric algebra library] but only for the vector operations.

```perl6
sub orbital-state-vectors(
    Real :$semimajor-axis where * >= 0,
    Real :$eccentricity   where * >= 0,
    Real :$inclination,
    Real :$longitude-of-ascending-node,
    Real :$argument-of-periapsis,
    Real :$true-anomaly
) {
    use Clifford;
    my ($i, $j, $k) = @e[^3];

    sub rotate($a is rw, $b is rw, Real \α) {
        ($a, $b) = cos(α)*$a + sin(α)*$b, -sin(α)*$a + cos(α)*$b;
    }
    rotate($i, $j, $longitude-of-ascending-node);
    rotate($j, $k, $inclination);
    rotate($i, $j, $argument-of-periapsis);

    my \l = $eccentricity == 1 ?? # PARABOLIC CASE
        2*$semimajor-axis !!
        $semimajor-axis*(1 - $eccentricity**2);

    my ($c, $s) = .cos, .sin given $true-anomaly;

    my \r = l/(1 + $eccentricity*$c);
    my \rprime = $s*r**2/l;

    my $position = r*($c*$i + $s*$j);

    my $speed = 
    (rprime*$c - r*$s)*$i + (rprime*$s + r*$c)*$j;
    $speed /= sqrt($speed**2);
    $speed *= sqrt(2/r - 1/$semimajor-axis);

    { :$position, :$speed }
}

say orbital-state-vectors
    semimajor-axis => 1,
    eccentricity => 0.1,
    inclination => pi/18,
    longitude-of-ascending-node => pi/6,
    argument-of-periapsis => pi/4,
    true-anomaly => 0;
```

{{out}}

```txt
{position => 0.237771283982207*e0+0.860960261697716*e1+0.110509023572076*e2, speed => -1.06193301748006*e0+0.27585002056925*e1+0.135747024865598*e2}
```


## Phix

{{trans|Python}}

```Phix
function vabs(sequence v)
    return sqrt(sum(sq_power(v,2)))
end function 

function mulAdd(sequence v1, atom x1, sequence v2, atom x2)
    return sq_add(sq_mul(v1,x1),sq_mul(v2,x2))
end function

function rotate(sequence i, j, atom alpha)
    atom ca = cos(alpha),
         sa = sin(alpha)
    return {mulAdd(i,ca,j,sa),mulAdd(i,-sa,j,ca)}
end function

procedure orbitalStateVectors(atom semimajorAxis, eccentricity, inclination, longitudeOfAscendingNode, argumentOfPeriapsis, trueAnomaly)
    sequence i = {1, 0, 0},
             j = {0, 1, 0},
             k = {0, 0, 1}
 
    {i,j} = rotate(i, j, longitudeOfAscendingNode)
    {j} = rotate(j, k, inclination)
    {i,j} = rotate(i, j, argumentOfPeriapsis)
 
    atom l = iff(eccentricity=1?2:1-eccentricity*eccentricity)*semimajorAxis,
         c = cos(trueAnomaly),
         s = sin(trueAnomaly),
         r = 1 / (1+eccentricity*c),
         rprime = s * r * r / l
    sequence posn = sq_mul(mulAdd(i, c, j, s),r),
             speed = mulAdd(i, rprime*c-r*s, j, rprime*s+r*c)
    speed = sq_div(speed,vabs(speed))
    speed = sq_mul(speed,sqrt(2/r - 1/semimajorAxis))
 
    puts(1,"Position :") ?posn
    puts(1,"Speed    :") ?speed
end procedure
 
orbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0)
```

{{out}}

```txt

Position :{0.7872958014,0.4545454895,0}
Speed    :{-0.5477225997,0.9486832737,0}

```



## Python


```python
import math

class Vector:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def __mul__(self, other):
        return Vector(self.x * other, self.y * other, self.z * other)

    def __div__(self, other):
        return Vector(self.x / other, self.y / other, self.z / other)

    def __str__(self):
        return '({x}, {y}, {z})'.format(x=self.x, y=self.y, z=self.z)

    def abs(self):
        return math.sqrt(self.x*self.x + self.y*self.y + self.z*self.z)

def mulAdd(v1, x1, v2, x2):
    return v1 * x1 + v2 * x2

def rotate(i, j, alpha):
    return [mulAdd(i,math.cos(alpha),j,math.sin(alpha)), mulAdd(i,-math.sin(alpha),j,math.cos(alpha))]

def orbitalStateVectors(semimajorAxis, eccentricity, inclination, longitudeOfAscendingNode, argumentOfPeriapsis, trueAnomaly):
    i = Vector(1, 0, 0)
    j = Vector(0, 1, 0)
    k = Vector(0, 0, 1)

    p = rotate(i, j, longitudeOfAscendingNode)
    i = p[0]
    j = p[1]
    p = rotate(j, k, inclination)
    j = p[0]
    p  =rotate(i, j, argumentOfPeriapsis)
    i = p[0]
    j = p[1]

    l = 2.0 if (eccentricity == 1.0) else 1.0 - eccentricity * eccentricity
    l *= semimajorAxis
    c = math.cos(trueAnomaly)
    s = math.sin(trueAnomaly)
    r = 1 / (1.0 + eccentricity * c)
    rprime = s * r * r / l
    position = mulAdd(i, c, j, s) * r
    speed = mulAdd(i, rprime * c - r * s, j, rprime * s + r * c)
    speed = speed / speed.abs()
    speed = speed * math.sqrt(2.0 / r - 1.0 / semimajorAxis)

    return [position, speed]

ps = orbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0)
print "Position :", ps[0]
print "Speed    :", ps[1]
```

{{out}}

```txt
Position : (0.787295801413, 0.454545489549, 0.0)
Speed    : (-0.547722599684, 0.948683273698, 0.0)
```



## REXX


### version 1

{{trans|Java}}
Vectors are represented by strings: 'x/y/z'

```rexx
/* REXX */
Numeric Digits 16
  Parse Value orbitalStateVectors(1.0,0.1,0.0,355.0/(113.0*6.0),0.0,0.0),
      With position speed
  Say "Position :" tostring(position)
  Say "Speed    :" tostring(speed)
  Exit

orbitalStateVectors: Procedure
  Parse Arg semimajorAxis,,
            eccentricity,,
            inclination,,
            longitudeOfAscendingNode,,
            argumentOfPeriapsis,,
            trueAnomaly
  i='1/0/0'
  j='0/1/0'
  k='0/0/1'
  Parse Value rotate(i, j, longitudeOfAscendingNode) With i j
  Parse Value rotate(j, k, inclination) With j p
  Parse Value rotate(i, j, argumentOfPeriapsis) With i j
  If eccentricity=1 Then l=2
  Else l=1-eccentricity*eccentricity
  l=l*semimajorAxis
  c=my_cos(trueAnomaly,16)
  s=my_sin(trueAnomaly,16)
  r=l/(1+eccentricity*c)
  rprime=s*r*r/l
  position=vmultiply(mulAdd(i,c,j,s),r)
  speed=mulAdd(i,rprime*c-r*s,j,rprime*s+r*c)
  speed=vdivide(speed,abs(speed))
  speed=vmultiply(speed,my_sqrt(2.0/r-1.0/semimajorAxis,16))
  Return position speed

abs: Procedure
  Parse Arg v.x '/' v.y '/' v.z
  Return my_sqrt(v.x**2+v.y**2+v.z**2,16)

muladd: Procedure
  Parse Arg v1,x1,v2,x2
  Parse Var v1 v1.x '/' v1.y '/' v1.z
  Parse Var v2 v2.x '/' v2.y '/' v2.z
  z=(v1.x*x1+v2.x*x2)||'/'||(v1.y*x1+v2.y*x2)||'/'||(v1.z*x1+v2.z*x2)
  Return z

rotate: Procedure
Parse Arg i,j,alpha
  xx=mulAdd(i,my_cos(alpha,16,'R'),j,my_sin(alpha,16))
  yy=mulAdd(i,-my_sin(alpha,16,'R'),j,my_cos(alpha,16))
  Return xx yy

vmultiply: Procedure
  Parse Arg v,d
  Parse Var v v.x '/' v.y '/' v.z
  Return (v.x*d)||'/'||(v.y*d)||'/'||(v.z*d)

vdivide: Procedure
  Parse Arg v,d
  Parse Var v v.x '/' v.y '/' v.z
  Return (v.x/d)||'/'||(v.y/d)||'/'||(v.z/d)

tostring:
  Parse Arg v.x '/' v.y '/' v.z
  Return '('v.x','v.y','v.z')'

my_sqrt: Procedure
/* REXX ***************************************************************
* EXEC to calculate the square root of a = 2 with high precision
**********************************************************************/
  Parse Arg x,prec
  If prec<9 Then prec=9
  prec1=2*prec
  eps=10**(-prec1)
  k = 1
  Numeric Digits 3
  r0= x
  r = 1
  Do i=1 By 1 Until r=r0 | ('ABS'(r*r-x)<eps)
    r0 = r
    r  = (r + x/r) / 2
    k  = min(prec1,2*k)
    Numeric Digits (k + 5)
    End
  Numeric Digits prec
  Return r+0

my_sin: Procedure
/* REXX ****************************************************************
* Return my_sin(x<,p>) -- with the specified precision
* my_sin(x) = x-(x**3/3!)+(x**5/5!)-(x**7/7!)+-...
***********************************************************************/
  Parse Arg x,prec
  If prec='' Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz   3
  pi=left('3.1415926535897932384626433832795028841971693993751058209749445923',2*prec+1)
  Do While x>pi
    x=x-pi
    End
  Do While x<-pi
    x=x+pi
    End
  o=x
  u=1
  r=x
  Do i=3 By 2
    ra=r
    o=-o*x*x
    u=u*i*(i-1)
    r=r+(o/u)
    If r=ra Then Leave
    End
  Numeric Digits prec
  Return r+0

my_cos: Procedure
/* REXX ****************************************************************
* Return my_cos(x) -- with specified precision
* my_cos(x) = 1-(x**2/2!)+(x**4/4!)-(x**6/6!)+-...
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
```

{{out}}

```txt
Position : (0.7794228433986798,0.4500000346536842,0)
Speed    : (-0.5527708409604436,0.9574270831797613,0)
```



### version 2

Re-coding of REXX version 1,   but with greater decimal digits precision.  

```rexx
/*REXX pgm converts orbital elements ──► orbital state vectors  (angles are in radians).*/
numeric digits length( pi() )  -  length(.)      /*limited to pi len, but show 1/3 digs.*/
call orbV 1,   .1,   0,    355/113/6,    0,    0 /*orbital elements taken from:  Java   */
call orbV 1,   .1,  pi/18,      pi/6,  pi/4,   0 /*   "        "      "     "    Perl 6 */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
orbV: procedure;  parse arg  semiMaj, eccentricity, inclination, node, periapsis, anomaly
      say;     say center(' orbital elements ', 99, "═")
      say '            semi-major axis:'  fmt(semiMaj)
      say '               eccentricity:'  fmt(eccentricity)
      say '                inclination:'  fmt(inclination)
      say '   ascending node longitude:'  fmt(node)
      say '      argument of periapsis:'  fmt(periapsis)
      say '               true anomaly:'  fmt(anomaly)
      i= 1 0 0;          j= 0 1 0;        k= 0 0 1    /*define the  I,  J,  K   vectors.*/
      parse value rot(i, j, node)   with  i '~' j     /*rotate ascending node longitude.*/
      parse value rot(j, k, inclination) with j '~'   /*rotate the inclination.         */
      parse value rot(i, j, periapsis)   with i '~' j /*rotate the argument of periapsis*/
      if eccentricity=1  then L= 2
                         else L= 1 - eccentricity**2
      L= L * semiMaj                                  /*calculate the semi─latus rectum.*/
      c= cos(anomaly);               s= sin(anomaly)  /*calculate COS and SIN of anomaly*/
      r= L / (1 + eccentricity * c)
      @= s*r**2 / L;        speed= MA(i,  @*c - r*s,  j,   @*s + r*c)
      speed=    mulV( divV( speed, absV(speed) ), sqrt(2 / r  - 1 / semiMaj) )
      say '                   position:'  show( mulV( MA(i, c, j, s),  r) )
      say '                      speed:'  show( speed);            return
/*──────────────────────────────────────────────────────────────────────────────────────*/
absV: procedure; parse arg x y z;              return sqrt(x**2  +  y**2  +  z**2)
divV: procedure; parse arg x y z, div;         return  (x / div)    (y / div)    (z / div)
mulV: procedure; parse arg x y z, mul;         return  (x * mul)    (y * mul)    (z * mul)
show: procedure; parse arg a b c;              return '('fmt(a)","   fmt(b)','   fmt(c)")"
fmt:  procedure; parse arg #;  return strip( left( left('', #>=0)# / 1, digits() %3), 'T')
MA:   procedure; parse arg x y z,@,a b c,$;    return  (x*@ + a*$) (y*@ + b*$) (z*@ + c*$)
pi:   pi= 3.1415926535897932384626433832795028841971693993751058209749445923;    return pi
rot:  procedure; parse arg i,j,$; return MA(i,cos($),j,sin($))'~'MA(i, -sin($), j, cos($))
r2r:  return arg(1)  //  (pi() * 2)                /*normalize radians ──► a unit circle*/
.sinCos: arg z 1 _,i; do k=2 by 2 until p=z; p=z; _= -_*$ /(k*(k+i)); z=z+_; end; return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos:  procedure; arg x;  x= r2r(x);   if x=0  then return 1;    a= abs(x);    Hpi= pi * .5
      numeric fuzz min(6, digits() - 3);        if a=pi       then return '-1'
      if a=Hpi | a=Hpi*3  then return   0;      if a=pi / 3   then return .5
      if a=pi * 2 / 3     then return '-.5';    $= x * x;          return .sinCos(1, '-1')
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin:  procedure; arg x;  x= r2r(x);   numeric fuzz min(5, max(1, digits() - 3) )
      if x=0  then return 0;   if x=pi*.5  then return 1;   if x==pi*1.5  then return '-1'
      if abs(x)=pi  then return 0;              $= x * x;          return .sinCos(x, 1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; arg x;  if x=0  then return 0;  d= digits();  numeric form; m.= 9; h= d+6
      numeric digits;  parse value format(x,2,1,,0) 'E0' with g 'E' _ .;  g= g *.5'e'_ % 2
        do j=0  while h>9;        m.j= h;              h= h % 2  +  1;    end
        do k=j+5  to 0  by '-1';  numeric digits m.k;  g= (g+x/g) * .5;   end;    return g
```

{{out|output|text=  when using the default internal inputs:}}

```txt

════════════════════════════════════════ orbital elements ═════════════════════════════════════════
            semi-major axis:  1
               eccentricity:  0.1
                inclination:  0
   ascending node longitude:  0.523598820058997050
      argument of periapsis:  0
               true anomaly:  0
                   position: ( 0.779422843398679832,  0.450000034653684237,  0)
                      speed: (-0.552770840960443759,  0.957427083179761535,  0)

════════════════════════════════════════ orbital elements ═════════════════════════════════════════
            semi-major axis:  1
               eccentricity:  0.1
                inclination:  0.174532925199432957
   ascending node longitude:  0.523598775598298873
      argument of periapsis:  0.785398163397448309
               true anomaly:  0
                   position: ( 0.237771283982206547,  0.860960261697715834,  0.110509023572075562)
                      speed: (-1.061933017480060047,  0.275850020569249507,  0.135747024865598167) 

```



## Scala


```Scala
import scala.language.existentials

object OrbitalElements extends App {
  private val ps = orbitalStateVectors(1.0, 0.1, 0.0, 355.0 / (113.0 * 6.0), 0.0, 0.0)
  println(f"Position : ${ps(0)}%s%nSpeed    : ${ps(1)}%s")

  private def orbitalStateVectors(semimajorAxis: Double,
                                  eccentricity: Double,
                                  inclination: Double,
                                  longitudeOfAscendingNode: Double,
                                  argumentOfPeriapsis: Double,
                                  trueAnomaly: Double) = {

    def mulAdd(v1: Vector, x1: Double, v2: Vector, x2: Double) = v1 * x1 + v2 * x2

    case class Vector(x: Double, y: Double, z: Double) {
      def +(term: Vector) =
        Vector(x + term.x, y + term.y, z + term.z)
      def *(factor: Double) = Vector(factor * x, factor * y, factor * z)
      def /(divisor: Double) = Vector(x / divisor, y / divisor, z / divisor)
      def abs: Double = math.sqrt(x * x + y * y + z * z)
      override def toString: String = f"($x%.16f, $y%.16f, $z%.16f)"
    }

    def rotate(i: Vector, j: Vector, alpha: Double) =
      Array[Vector](mulAdd(i, math.cos(alpha), j, math.sin(alpha)),
        mulAdd(i, -math.sin(alpha), j, math.cos(alpha)))

    val p = rotate(Vector(1, 0, 0), Vector(0, 1, 0), longitudeOfAscendingNode)
    val p2 = rotate(p(0),
      rotate(p(1), Vector(0, 0, 1), inclination)(0),
      argumentOfPeriapsis)
    val l = semimajorAxis *
      (if (eccentricity == 1.0) 2.0 else 1.0 - eccentricity * eccentricity)
    val (c, s) = (math.cos(trueAnomaly), math.sin(trueAnomaly))
    val r = l / (1.0 + eccentricity * c)
    val rprime = s * r * r / l
    val speed = mulAdd(p2(0), rprime * c - r * s, p2(1), rprime * s + r * c)
    Array[Vector](mulAdd(p(0), c, p2(1), s) * r,
      speed / speed.abs * math.sqrt(2.0 / r - 1.0 / semimajorAxis))
  }

}
```

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/ac17jh2/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/2NQNgj4OQkazxZNvSzcexQ Scastie (remote JVM)].


## Sidef

{{trans|Perl}}

```ruby
func orbital_state_vectors(
    semimajor_axis,
    eccentricity,
    inclination,
    longitude_of_ascending_node,
    argument_of_periapsis,
    true_anomaly
) {

    var (i, j, k) = (
        Vector(1, 0, 0),
        Vector(0, 1, 0),
        Vector(0, 0, 1),
    )

    func muladd(v1, x1, v2, x2) {
        (v1 * x1) + (v2 * x2)
    }

    func rotate(Ref i, Ref j, α) {
        (*i, *j) = (
            muladd(*i, +cos(α), *j, sin(α)),
            muladd(*i, -sin(α), *j, cos(α)),
        )
    }

    rotate(\i, \j, longitude_of_ascending_node)
    rotate(\j, \k, inclination)
    rotate(\i, \j, argument_of_periapsis)

    var l = (eccentricity == 1 ? 2*semimajor_axis
                               : semimajor_axis*(1 - eccentricity**2))

    var (c, s) = with(true_anomaly) { (.cos, .sin) }

    var r = l/(1 + eccentricity*c)
    var rprime = (s * r**2 / l)
    var position = muladd(i, c, j, s)*r

    var speed = muladd(i, rprime*c - r*s, j, rprime*s + r*c)
    speed /= speed.abs
    speed *= sqrt(2/r - 1/semimajor_axis)

    struct Result { position, speed }
    Result(position, speed)
}

for args in ([
    [1, 0.1, 0, 355/(113*6), 0, 0],
    [1, 0.1, Num.pi/18, Num.pi/6, Num.pi/4, 0]
]) {
    var r = orbital_state_vectors(args...)

    say "Arguments: #{args}:"
    say "Position : #{r.position}"
    say "Speed    : #{r.speed}\n"
}
```

{{out}}

```txt

Arguments: [1, 1/10, 0, 355/678, 0, 0]:
Position : Vector(0.779422843398679832042176328223663037464703527986, 0.450000034653684237432302249506712706822033851071, 0)
Speed    : Vector(-0.552770840960443759673279062314259546277084494097, 0.957427083179761535246200368614952095349966503287, 0)

Arguments: [1, 1/10, 0.174532925199432957692369076848861271344287188854, 0.523598775598298873077107230546583814032861566563, 0.785398163397448309615660845819875721049292349844, 0]:
Position : Vector(0.23777128398220654779107184959165027147748809404, 0.860960261697715834668966272382699039216399966872, 0.110509023572075562109405412890808505271310143909)
Speed    : Vector(-1.06193301748006004757467368094494935655538772696, 0.275850020569249507846452830330085489348356659642, 0.135747024865598167166145512759280712986072818844)

```



## Swift


{{trans|Kotlin}}


```swift
import Foundation

public struct Vector {
  public var x = 0.0
  public var y = 0.0
  public var z = 0.0

  public init(x: Double, y: Double, z: Double) {
    (self.x, self.y, self.z) = (x, y, z)
  }

  public func mod() -> Double {
    (x * x + y * y + z * z).squareRoot()
  }

  public static func + (lhs: Vector, rhs: Vector) -> Vector {
    return Vector(
      x: lhs.x + rhs.x,
      y: lhs.y + rhs.y,
      z: lhs.z + rhs.z
    )
  }

  public static func * (lhs: Vector, rhs: Double) -> Vector {
    return Vector(
      x: lhs.x * rhs,
      y: lhs.y * rhs,
      z: lhs.z * rhs
    )
  }

  public static func *= (lhs: inout Vector, rhs: Double) {
    lhs.x *= rhs
    lhs.y *= rhs
    lhs.z *= rhs
  }

  public static func / (lhs: Vector, rhs: Double) -> Vector {
    return lhs * (1 / rhs)
  }

  public static func /= (lhs: inout Vector, rhs: Double) {
    lhs = lhs * (1 / rhs)
  }
}

extension Vector: CustomStringConvertible {
  public var description: String {
    return String(format: "%.6f\t%.6f\t%.6f", x, y, z)
  }
}

private func mulAdd(v1: Vector, x1: Double, v2: Vector, x2: Double) -> Vector {
  return v1 * x1 + v2 * x2
}

private func rotate(_ i: Vector, _ j: Vector, alpha: Double) -> (Vector, Vector) {
  return (
    mulAdd(v1: i, x1: +cos(alpha), v2: j, x2: sin(alpha)),
    mulAdd(v1: i, x1: -sin(alpha), v2: j, x2: cos(alpha))
  )
}

public func orbitalStateVectors(
  semimajorAxis: Double,
  eccentricity: Double,
  inclination: Double,
  longitudeOfAscendingNode: Double,
  argumentOfPeriapsis: Double,
  trueAnomaly: Double
) -> (Vector, Vector) {
  var i = Vector(x: 1.0, y: 0.0, z: 0.0)
  var j = Vector(x: 0.0, y: 1.0, z: 0.0)
  let k = Vector(x: 0.0, y: 0.0, z: 1.0)

  (i, j) = rotate(i, j, alpha: longitudeOfAscendingNode)
  (j, _) = rotate(j, k, alpha: inclination)
  (i, j) = rotate(i, j, alpha: argumentOfPeriapsis)

  let l = eccentricity == 1.0 ? 2.0 : 1.0 - eccentricity * eccentricity
  let c = cos(trueAnomaly)
  let s = sin(trueAnomaly)
  let r = l / (1.0 + eccentricity * c)
  let rPrime = s * r * r / l
  let position = mulAdd(v1: i, x1: c, v2: j, x2: s) * r
  var speed = mulAdd(v1: i, x1: rPrime * c - r * s, v2: j, x2: rPrime * s + r * c)

  speed /= speed.mod()
  speed *= (2.0 / r - 1.0 / semimajorAxis).squareRoot()

  return (position, speed)
}

let (position, speed) = orbitalStateVectors(
  semimajorAxis: 1.0,
  eccentricity: 0.1,
  inclination: 0.0,
  longitudeOfAscendingNode: 355.0 / (113.0 * 6.0),
  argumentOfPeriapsis: 0.0,
  trueAnomaly: 0.0
)

print("Position: \(position); Speed: \(speed)")
```


{{out}}


```txt
Position: 0.779423	0.450000	0.000000; Speed: -0.552771	0.957427	0.000000
```



## zkl

{{trans|Perl}}

```zkl
fcn orbital_state_vectors(semimajor_axis, eccentricity, inclination, 
        longitude_of_ascending_node, argument_of_periapsis, true_anomaly){
   i,j,k:=T(1.0, 0.0, 0.0), T(0.0, 1.0, 0.0), T(0.0, 0.0, 1.0);
 
   vdot:=fcn(c,vector){ vector.apply('*,c) };
   vsum:=fcn(v1,v2)   { v1.zipWith('+,v2)  };
   rotate:='wrap(alpha, a,b){  // a&b are vectors: (x,y,z)
      return(vsum(vdot( alpha.cos(),a), vdot(alpha.sin(),b)), #cos(alpha)*a + sin(alpha)*b
             vsum(vdot(-alpha.sin(),a), vdot(alpha.cos(),b)));
   };
   i,j=rotate(longitude_of_ascending_node,i,j);
   j,k=rotate(inclination,		  j,k);
   i,j=rotate(argument_of_periapsis,      i,j);
 
   l:=if(eccentricity==1)   # PARABOLIC CASE
        semimajor_axis*2  else
        semimajor_axis*(1.0 - eccentricity.pow(2));;
   c,s,r:=true_anomaly.cos(), true_anomaly.sin(), l/(eccentricity*c + 1);
   rprime:=s*r.pow(2)/l;
 
   position:=vdot(r,vsum(vdot(c,i), vdot(s,j)));  #r*(c*i + s*j)
 
   speed:=vsum(vdot(rprime*c - r*s,i), vdot(rprime*s + r*c,j)); #(rprime*c - r*s)*i + (rprime*s + r*c)*j
   z:=speed.zipWith('*,speed).sum(0.0).sqrt();  #sqrt(speed**2)
   speed=vdot(1.0/z,speed);			#speed/z

   speed=vdot((2.0/r - 1.0/semimajor_axis).sqrt(),speed); #speed*sqrt(2/r - 1/semimajor_axis)
 
   return(position,speed);
}
```


```zkl
orbital_state_vectors(
    1.0,                           # semimajor axis
    0.1,                           # eccentricity
    0.0,                           # inclination
    (0.0).pi/6,                    # longitude of ascending node
    0.0,                           # argument of periapsis
    0.0                            # true-anomaly
).println();
```

{{out}}

```txt
L(L(0.779423,0.45,0),L(-0.552771,0.957427,0))
```


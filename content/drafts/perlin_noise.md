+++
title = "Perlin noise"
description = ""
date = 2019-08-20T08:18:37Z
aliases = []
[extra]
id = 17323
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

The   '''[[wp:Perlin noise|Perlin noise]]'''   is a kind of   [[wp:gradient noise|gradient noise]]   invented by   [[wp:Ken Perlin|Ken Perlin]]   around the end of the twentieth century and still currently heavily used in   [[wp:computer graphics|computer graphics]],   most notably to procedurally generate textures or heightmaps. 

The Perlin noise is basically a   [[random numbers|pseudo-random]]   mapping of   <big><big><math>\R^d</math></big></big>   into   <big><big><math>\R</math></big></big>   with an integer   <big><math>d</math></big>   which can be arbitrarily large but which is usually   2,   3,   or   4.

Either by using a dedicated library or by implementing the algorithm, show that the Perlin noise   (as defined in 2002 in the Java implementation below)   of the point in 3D-space with coordinates     3.14,   42,   7     is     0.13691995878400012.





## C

A sign of how close C and Java are is this implementation which differs very little from Perlin's Java implementation. I only removed the static, public and final keywords and the Math class name from the floor calls, and it compiled without errors. Interestingly, in the paper, Perlin says that the benchmark implementation which was used to gauge the algorithm's performance was in C. One important improvement here is that the permutation data is externalized and read from a file. This way different textures and effects can be generated without having to change the source code.

```C

#include<stdlib.h>
#include<stdio.h>
#include<math.h>

int p[512];

double fade(double t) { return t * t * t * (t * (t * 6 - 15) + 10); }
double lerp(double t, double a, double b) { return a + t * (b - a); }
double grad(int hash, double x, double y, double z) {
      int h = hash & 15;                      
      double u = h<8 ? x : y,                 
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
   }
   
double noise(double x, double y, double z) {
      int X = (int)floor(x) & 255,                  
          Y = (int)floor(y) & 255,                  
          Z = (int)floor(z) & 255;
      x -= floor(x);                                
      y -= floor(y);                                
      z -= floor(z);
      double u = fade(x),                                
             v = fade(y),                                
             w = fade(z);
      int A = p[X  ]+Y, AA = p[A]+Z, AB = p[A+1]+Z,      
          B = p[X+1]+Y, BA = p[B]+Z, BB = p[B+1]+Z;   
 
      return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ), 
                                     grad(p[BA  ], x-1, y  , z   )),
                             lerp(u, grad(p[AB  ], x  , y-1, z   ), 
                                     grad(p[BB  ], x-1, y-1, z   ))),
                     lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ), 
                                     grad(p[BA+1], x-1, y  , z-1 )), 
                             lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
                                     grad(p[BB+1], x-1, y-1, z-1 ))));
   }

void loadPermutation(char* fileName){
	FILE* fp = fopen(fileName,"r");
	int permutation[256],i;
	
	for(i=0;i<256;i++)
		fscanf(fp,"%d",&permutation[i]);
	
	fclose(fp);
	
	for (int i=0; i < 256 ; i++) p[256+i] = p[i] = permutation[i];
}

int main(int argC,char* argV[])
{
	if(argC!=5)
		printf("Usage : %s <permutation data file> <x,y,z co-ordinates separated by space>");
	else{
		loadPermutation(argV[1]);
		printf("Perlin Noise for (%s,%s,%s) is %.17lf",argV[2],argV[3],argV[4],noise(strtod(argV[2],NULL),strtod(argV[3],NULL),strtod(argV[4],NULL)));
	}
	
	return 0;
}

```

Permutation file, it should 256 integers in the range [0,255]:

```txt

151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180

```

Invocation and output :

```txt

C:\rosettaCode>perlin.exe perlinData.txt 3.14 42 7
Perlin Noise for (3.14,42,7) is 0.13691995878400012

```



## Common Lisp


```lisp
;;;; Translation from: Java

(declaim (optimize (speed 3) (debug 0)))

(defconstant +p+
  (let ((permutation
	 #(151 160 137 91 90 15 
	   131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 
	   190  6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 
	   88 237 149 56 87 174 20 125 136 171 168  68 175 74 165 71 134 139 48 27 166 
	   77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 
	   102 143 54  65 25 63 161  1 216 80 73 209 76 132 187 208  89 18 169 200 196 
	   135 130 116 188 159 86 164 100 109 198 173 186  3 64 52 217 226 250 124 123 
	   5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 
	   223 183 170 213 119 248 152  2 44 154 163  70 221 153 101 155 167  43 172 9 
	   129 22 39 253  19 98 108 110 79 113 224 232 178 185  112 104 218 246 97 228 
	   251 34 242 193 238 210 144 12 191 179 162 241  81 51 145 235 249 14 239 107 
	   49 192 214  31 181 199 106 157 184  84 204 176 115 121 50 45 127  4 150 254 
	   138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180))
	(aux (make-array 512 :element-type 'fixnum)))
    (dotimes (i 256 aux)
      (setf (aref aux i) (aref permutation i))
      (setf (aref aux (+ 256 i)) (aref permutation i)))))

(defun fade (te)
  (declare (type double-float te))
  (the double-float (* te te te (+ (* te (- (* te 6) 15)) 10))))

(defun lerp (te a b)
  (declare (type double-float te a b))
  (the double-float (+ a (* te (- b a)))))

(defun grad (hash x y z)
  (declare (type fixnum hash)
	   (type double-float x y z))
  (let* ((h (logand hash 15)) ;; convert lo 4 bits of hash code into 12 gradient directions
	 (u (if (< h 8) x y))
	 (v (cond ((< h 4)
		   y)
		  ((or (= h 12) (= h 14))
		   x)
		  (t z))))
    (the
     double-float
     (+
      (if (zerop (logand h 1)) u (- u))
      (if (zerop (logand h 2)) v (- v))))))

(defun noise (x y z)
  (declare (type double-float x y z))
  ;; find unit cube that contains point.
  (let ((cx (logand (floor x) 255))
	(cy (logand (floor y) 255))
	(cz (logand (floor z) 255)))
    ;; find relative x, y, z of point in cube.
    (let ((x (- x (floor x)))
	  (y (- y (floor y)))
	  (z (- z (floor z))))
      ;; compute fade curves for each of x, y, z.
      (let ((u (fade x))
	    (v (fade y))
	    (w (fade z)))
	;; hash coordinates of the 8 cube corners,
	(let* ((ca  (+ (aref +p+     cx)  cy))
	       (caa (+ (aref +p+     ca)  cz))
	       (cab (+ (aref +p+ (1+ ca)) cz))
	       (cb  (+ (aref +p+ (1+ cx)) cy))
	       (cba (+ (aref +p+     cb)  cz))
	       (cbb (+ (aref +p+ (1+ cb)) cz)))
	  ;; ... and add blended results from 8 corners of cube
	  (the double-float
	       (lerp w
		     (lerp v
			   (lerp u
				 (grad (aref +p+ caa)          x       y      z)
				 (grad (aref +p+ cba)      (1- x)      y      z))
			   (lerp u
				 (grad (aref +p+ cab)          x   (1- y)     z)
				 (grad (aref +p+ cbb)      (1- x)  (1- y)     z)))
		     (lerp v
			   (lerp u
				 (grad (aref +p+ (1+ caa))     x       y  (1- z))
				 (grad (aref +p+ (1+ cba)) (1- x)      y  (1- z)))
			   (lerp u
				 (grad (aref +p+ (1+ cab))     x   (1- y) (1- z))
				 (grad (aref +p+ (1+ cbb)) (1- x)  (1- y) (1- z)))))))))))

(print (noise 3.14d0 42d0 7d0))

```

{{out}}

```txt
0.13691995878400012d0
```



## D


```d
import std.stdio, std.math;

struct PerlinNoise {
    private static double fade(in double t) pure nothrow @safe @nogc {
        return t ^^ 3 * (t * (t * 6 - 15) + 10);
    }

    private static double lerp(in double t, in double a, in double b)
    pure nothrow @safe @nogc {
        return a + t * (b - a);
    }

    private static double grad(in ubyte hash,
                               in double x, in double y, in double z)
    pure nothrow @safe @nogc {
        // Convert lo 4 bits of hash code into 12 gradient directions.
        immutable h = hash & 0xF;
        immutable double u = (h < 8) ? x : y,
                         v = (h < 4) ? y : (h == 12 || h == 14 ? x : z);
        return ((h & 1) == 0 ? u : -u) + ((h & 2) == 0 ? v : -v);
    }

    static immutable ubyte[512] p;

    static this() pure nothrow @safe @nogc {
        static immutable permutation = cast(ubyte[256])x"97 A0 89 5B 5A
            0F 83 0D C9 5F 60 35 C2 E9 07 E1 8C 24 67 1E 45 8E 08 63 25
            F0 15 0A 17 BE 06 94 F7 78 EA 4B 00 1A C5 3E 5E FC DB CB 75
            23 0B 20 39 B1 21 58 ED 95 38 57 AE 14 7D 88 AB A8 44 AF 4A
            A5 47 86 8B 30 1B A6 4D 92 9E E7 53 6F E5 7A 3C D3 85 E6 DC
            69 5C 29 37 2E F5 28 F4 66 8F 36 41 19 3F A1 01 D8 50 49 D1
            4C 84 BB D0 59 12 A9 C8 C4 87 82 74 BC 9F 56 A4 64 6D C6 AD
            BA 03 40 34 D9 E2 FA 7C 7B 05 CA 26 93 76 7E FF 52 55 D4 CF
            CE 3B E3 2F 10 3A 11 B6 BD 1C 2A DF B7 AA D5 77 F8 98 02 2C
            9A A3 46 DD 99 65 9B A7 2B AC 09 81 16 27 FD 13 62 6C 6E 4F
            71 E0 E8 B2 B9 70 68 DA F6 61 E4 FB 22 F2 C1 EE D2 90 0C BF
            B3 A2 F1 51 33 91 EB F9 0E EF 6B 31 C0 D6 1F B5 C7 6A 9D B8
            54 CC B0 73 79 32 2D 7F 04 96 FE 8A EC CD 5D DE 72 43 1D 18
            48 F3 8D 80 C3 4E 42 D7 3D 9C B4";

        // Two copies of permutation.
        p[0 .. permutation.length] = permutation[];
        p[permutation.length .. $] = permutation[];
    }

    /// x0, y0 and z0 can be any real numbers, but the result is
    /// zero if they are all integers.
    /// The result is probably in [-1.0, 1.0].
    static double opCall(in double x0, in double y0, in double z0)
    pure nothrow @safe @nogc {
        // Find unit cube that contains point.
        immutable ubyte X = cast(int)x0.floor & 0xFF,
                        Y = cast(int)y0.floor & 0xFF,
                        Z = cast(int)z0.floor & 0xFF;

        // Find relative x,y,z of point in cube.
        immutable x = x0 - x0.floor,
                  y = y0 - y0.floor,
                  z = z0 - z0.floor;

        // Compute fade curves for each of x,y,z.
        immutable u = fade(x),
                  v = fade(y),
                  w = fade(z);

        // Hash coordinates of the 8 cube corners.
        immutable A  = p[X  ]   + Y,
                  AA = p[A]     + Z,
                  AB = p[A + 1] + Z,
                  B  = p[X + 1] + Y,
                  BA = p[B]     + Z,
                  BB = p[B + 1] + Z;

        // And add blended results from  8 corners of cube.
        return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z  ),
                                       grad(p[BA  ], x-1, y  , z  )),
                               lerp(u, grad(p[AB  ], x  , y-1, z  ),
                                       grad(p[BB  ], x-1, y-1, z  ))),
                       lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1),
                                       grad(p[BA+1], x-1, y  , z-1)),
                               lerp(u, grad(p[AB+1], x  , y-1, z-1),
                                       grad(p[BB+1], x-1, y-1, z-1))));
    }
}

void main() {
    writefln("%1.17f", PerlinNoise(3.14, 42, 7));

    /*
    // Generate a demo image using the Gray Scale task module.
    import grayscale_image;
    enum N = 200;
    auto im = new Image!Gray(N, N);
    foreach (immutable y; 0 .. N)
        foreach (immutable x; 0 .. N) {
            immutable p = PerlinNoise(x / 30.0, y / 30.0, 0.1);
            im[x, y] = Gray(cast(ubyte)((p + 1) / 2 * 256));
        }
    im.savePGM("perlin_noise.pgm");
    */
}
```

{{out}}

```txt
0.13691995878400012
```



## Factor

{{trans|Java}}

As this is a strict translation of applicative code that makes heavy use of local variables, it is highly non-idiomatic. In idiomatic code, we would never write a word as long as <code>noise</code> or name so many throwaway values. We would also abstract out repetitive patterns such as 
<lang>x floor >integer 255 bitand :> X
y floor >integer 255 bitand :> Y
z floor >integer 255 bitand :> Z
```

with dataflow combinators:
<lang>[ floor >integer 255 bitand ] tri@
```


```factor
USING: kernel math math.functions literals locals prettyprint
sequences ;
IN: rosetta-code.perlin-noise

CONSTANT: p $[
    { 151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140
    36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234
    75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237
    149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48
    27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105
    92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73
    209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86
    164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38
    147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189
    28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153
    101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224
    232 178 185 112 104 218 246 97 228 251 34 242 193 238 210
    144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192
    214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4
    150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195
    78 66 215 61 156 180 } dup append
]

:: fade ( t -- x ) t 6 * 15 - t * 10 + t * t * t * ;

:: lerp ( t a b -- x ) b a - t * a + ;

:: grad ( hash x y z -- w )
    hash 15 bitand :> h
    h 8 < x y ? :> u
    h 4 < y h 12 = h 14 = or x z ? ? :> v
    h 1 bitand 0 = u u neg ? h 2 bitand 0 = v v neg ? + ;
    
:: noise ( x! y! z! -- noise )
    x floor >integer 255 bitand :> X
    y floor >integer 255 bitand :> Y
    z floor >integer 255 bitand :> Z
    x x floor - x!
    y y floor - y!
    z z floor - z!
    x fade :> u
    y fade :> v
    z fade :> w
    X p nth Y +     :> A
    A p nth Z +     :> AA
    A 1 + p nth Z + :> AB
    X 1 + p nth Y + :> B
    B p nth Z +     :> BA
    B 1 + p nth Z + :> BB
    
    w v u AA p nth x y z                 grad
          BA p nth x 1 - y z             grad lerp
        u AB p nth x y 1 - z             grad
          BB p nth x 1 - y 1 - z         grad lerp lerp
      v u AA 1 + p nth x y z 1 -         grad
          BA 1 + p nth x 1 - y z 1 -     grad lerp
        u AB 1 + p nth x y 1 - z 1 -     grad
          BB 1 + p nth x 1 - y 1 - z 1 - grad lerp lerp lerp ;

: main ( -- ) 3.14 42 7 noise . ;

MAIN: main
```

{{out}}

```txt

0.1369199587840001

```



## GLSL

From https://gist.github.com/patriciogonzalezvivo/670c22f3966e662d2f83 :

```glsl

float rand(vec2 c){
	return fract(sin(dot(c.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float noise(vec2 p, float freq ){
	float unit = screenWidth/freq;
	vec2 ij = floor(p/unit);
	vec2 xy = mod(p,unit)/unit;
	//xy = 3.*xy*xy-2.*xy*xy*xy;
	xy = .5*(1.-cos(PI*xy));
	float a = rand((ij+vec2(0.,0.)));
	float b = rand((ij+vec2(1.,0.)));
	float c = rand((ij+vec2(0.,1.)));
	float d = rand((ij+vec2(1.,1.)));
	float x1 = mix(a, b, xy.x);
	float x2 = mix(c, d, xy.x);
	return mix(x1, x2, xy.y);
}

float pNoise(vec2 p, int res){
	float persistance = .5;
	float n = 0.;
	float normK = 0.;
	float f = 4.;
	float amp = 1.;
	int iCount = 0;
	for (int i = 0; i<50; i++){
		n+=amp*noise(p, f);
		f*=2.;
		normK+=amp;
		amp*=persistance;
		if (iCount == res) break;
		iCount++;
	}
	float nf = n/normK;
	return nf*nf*nf*nf;
}

```


## Go


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    fmt.Println(noise(3.14, 42, 7))
}

func noise(x, y, z float64) float64 {
    X := int(math.Floor(x)) & 255
    Y := int(math.Floor(y)) & 255
    Z := int(math.Floor(z)) & 255
    x -= math.Floor(x)
    y -= math.Floor(y)
    z -= math.Floor(z)
    u := fade(x)
    v := fade(y)
    w := fade(z)
    A := p[X] + Y
    AA := p[A] + Z
    AB := p[A+1] + Z
    B := p[X+1] + Y
    BA := p[B] + Z
    BB := p[B+1] + Z
    return lerp(w, lerp(v, lerp(u, grad(p[AA], x, y, z),
        grad(p[BA], x-1, y, z)),
        lerp(u, grad(p[AB], x, y-1, z),
            grad(p[BB], x-1, y-1, z))),
        lerp(v, lerp(u, grad(p[AA+1], x, y, z-1),
            grad(p[BA+1], x-1, y, z-1)),
            lerp(u, grad(p[AB+1], x, y-1, z-1),
                grad(p[BB+1], x-1, y-1, z-1))))
}
func fade(t float64) float64       { return t * t * t * (t*(t*6-15) + 10) }
func lerp(t, a, b float64) float64 { return a + t*(b-a) }
func grad(hash int, x, y, z float64) float64 {
    // Go doesn't have a ternary.  Ternaries can be translated directly
    // with if statements, but chains of if statements are often better
    // expressed with switch statements.
    switch hash & 15 {
    case 0, 12:
        return x + y
    case 1, 14:
        return y - x
    case 2:
        return x - y
    case 3:
        return -x - y
    case 4:
        return x + z
    case 5:
        return z - x
    case 6:
        return x - z
    case 7:
        return -x - z
    case 8:
        return y + z
    case 9, 13:
        return z - y
    case 10:
        return y - z
    }
    // case 11, 16:
    return -y - z
}

var permutation = []int{
    151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
    140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148,
    247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32,
    57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175,
    74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122,
    60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54,
    65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169,
    200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64,
    52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212,
    207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213,
    119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
    129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104,
    218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241,
    81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157,
    184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93,
    222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180,
}
var p = append(permutation, permutation...)
```

{{out}}

```txt

0.13691995878400012

```



## J


We can trivially copy the java implementation:


```J
band=:17 b.
ImprovedNoise=:3 :0
  'x y z'=. y
  X=. (<. x) band 255
  Y=. (<. y) band 255
  Z=. (<. z) band 255
  x=. x-<.!.0 x
  y=. y-<.!.0 y
  z=. z-<.!.0 z
  u=. fade x
  v=. fade y
  w=. fade z
  A=. (p{~X)+Y
  AA=.(p{~A)+Z
  AB=.(p{~A+1)+Z
  B=. (p{~X+1)+Y
  BA=.(p{~B)+Z
  BB=.(p{~B+1)+Z

  t1=. (grad(p{~BB+1),(x-1),(y-1),z-1)
  t2=. (lerp u,(grad(p{~AB+1),x,(y-1),z-1),t1)
  t3=. (grad(p{~BA+1),(x-1),y,z-1)
  t4=. (lerp v,(lerp u,(grad(p{~AA+1),x,y,z-1),t3),t2)
  t5=. (grad(p{~BB),(x-1),(y-1),z)
  t6=. (lerp u,(grad(p{~AB),x,(y-1),z),t5)
  t7=. (grad(p{~BA),(x-1),y,z)
  (lerp w,(lerp v,(lerp u,(grad(p{~AA),x,y,z),t7),t6),t4)
)

fade=:3 :0
  t=.y
  t * t * t * ((t * ((t * 6) - 15)) + 10)
)

lerp=:3 :0
  't a b'=. y
  a + t * (b - a)
)

grad=:3 :0
  'hash x y z'=. y
  h =. hash band 15 NB.                 CONVERT LO 4 BITS OF HASH CODE
  u =. x [^:(h<8) y NB.                 INTO 12 GRADIENT DIRECTIONS.
  v =. y [^:(h<4) x [^:((h=12)+.(h=14)) z
  (u [^:((h band 1) = 0) -u) + v [^:((h band 2) = 0) -v
)


p=:,~ 151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180 

fade=: 0 0 0 10 _15 6&p.

ImprovedNoise=:3 :0
  'XYZ xyz'=. |:256 1 #:y
  uvw=. fade xyz
  hash=. 0 1+/(+ p{~0 1+/])/|. XYZ

  t1=.                 (grad(p{~BB+1),(x-1),(y-1),z-1)
  t2=.         (lerp u,(grad(p{~AB+1), x,   (y-1),z-1),t1)
  t3=.                 (grad(p{~BA+1),(x-1), y,   z-1)
  t4=. (lerp v,(lerp u,(grad(p{~AA+1), x,    y,   z-1),t3),t2)
  t5=.                 (grad(p{~BB),  (x-1),(y-1),z)
  t6=.         (lerp u,(grad(p{~AB),   x,   (y-1),z),  t5)
  t7=.                 (grad(p{~BA),  (x-1), y,   z)
  (lerp w,(lerp v,(lerp u,(grad(p{~AA),x,    y,   z),  t7),t6),t4)
)

```


And this gives us our desired result:


```J
   ImprovedNoise 3.14 42 7
0.13692
```


Or, asking to see 20 digits after the decimal point:


```J

   22j20": ImprovedNoise 3.14 42 7
0.13691995878400012000
```



###  Simplified Expression 


It's tempting, though, to express this more concisely. We are limited, there, by some of the arbitrary choices in the algorithm, but we can still exploit regularities in its structure:


```J
p=:,~ 151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180 

fade=: 0 0 0 10 _15 6&p.

grad=:4 :0
  dir=. (16|x){_1+3 3 3#:25 7 19 1 23 5 21 3 17 11 15 9 25 11 7 9
  dir+/ .*"1 y
)

lerp=:4 :0
  'a b'=. y
  a + x * (b - a)
)

ImprovedNoise=:3 :0
  'XYZ xyz'=. |:256 1 #:y
  uvw=. fade xyz
  hash=. p{~0 1+/(+ p{~0 1+/])/|. XYZ
  g=. hash grad xyz-"1|."1#:i.$hash
  u=. (0{uvw) lerp"1 g
  v=. (1{uvw) lerp"1 u
  w=. (2{uvw) lerp"1 v
)
```


And we can see that there's no difference in this result:


```J
   0.13691995878400012 - ImprovedNoise 3.14 42 7
0
```


It's probably possible to simplify this further.


## Java

The original code from Perlin was originally published in java. Note that this does not have a main method so there will be no output. To test, add a main method, call noise() with 3.14,42,7, save the file as ImprovedNoise.java and compile.

```java
// JAVA REFERENCE IMPLEMENTATION OF IMPROVED NOISE - COPYRIGHT 2002 KEN PERLIN.

public final class ImprovedNoise {
   static public double noise(double x, double y, double z) {
      int X = (int)Math.floor(x) & 255,                  // FIND UNIT CUBE THAT
          Y = (int)Math.floor(y) & 255,                  // CONTAINS POINT.
          Z = (int)Math.floor(z) & 255;
      x -= Math.floor(x);                                // FIND RELATIVE X,Y,Z
      y -= Math.floor(y);                                // OF POINT IN CUBE.
      z -= Math.floor(z);
      double u = fade(x),                                // COMPUTE FADE CURVES
             v = fade(y),                                // FOR EACH OF X,Y,Z.
             w = fade(z);
      int A = p[X  ]+Y, AA = p[A]+Z, AB = p[A+1]+Z,      // HASH COORDINATES OF
          B = p[X+1]+Y, BA = p[B]+Z, BB = p[B+1]+Z;      // THE 8 CUBE CORNERS,

      return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ),  // AND ADD
                                     grad(p[BA  ], x-1, y  , z   )), // BLENDED
                             lerp(u, grad(p[AB  ], x  , y-1, z   ),  // RESULTS
                                     grad(p[BB  ], x-1, y-1, z   ))),// FROM  8
                     lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ),  // CORNERS
                                     grad(p[BA+1], x-1, y  , z-1 )), // OF CUBE
                             lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
                                     grad(p[BB+1], x-1, y-1, z-1 ))));
   }
   static double fade(double t) { return t * t * t * (t * (t * 6 - 15) + 10); }
   static double lerp(double t, double a, double b) { return a + t * (b - a); }
   static double grad(int hash, double x, double y, double z) {
      int h = hash & 15;                      // CONVERT LO 4 BITS OF HASH CODE
      double u = h<8 ? x : y,                 // INTO 12 GRADIENT DIRECTIONS.
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
   }
   static final int p[] = new int[512], permutation[] = { 151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
   };
   static { for (int i=0; i < 256 ; i++) p[256+i] = p[i] = permutation[i]; }
}
```



## Julia


```julia
const permutation = UInt8[
    151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233,
    7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23,
    190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219,
    203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174,
    20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27,
    166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230,
    220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25,
    63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169,
    200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173,
    186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118,
    126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182,
    189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163,
    70, 221, 153, 101, 155, 167,  43, 172, 9, 129, 22, 39, 253, 19,
    98, 108, 110, 79, 113, 224, 232, 178, 185,  112, 104, 218, 246,
    97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162,
    241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181,
    199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150,
    254, 138, 236, 205, 93, 222,    114, 67, 29, 24, 72, 243, 141,
    128, 195, 78, 66, 215, 61, 156, 180]

function grad(h::Integer, x, y, z)
    h &= 15                                                 # CONVERT LO 4 BITS OF HASH CODE
    u = h < 8 ? x : y                                       # INTO 12 GRADIENT DIRECTIONS.
    v = h < 4 ? y : h == 12 || h == 14 ? x : z
    (h & 1 == 0 ? u : -u) + (h & 2 == 0 ? v : -v)
end

function perlinsnoise(x, y, z)
    p = vcat(permutation,  permutation)
    fade(t) = t * t * t * (t * (t * 6 - 15) + 10)
    lerp(t, a, b) = a + t * (b - a)
    floorb(x) = Int(floor(x)) & 0xff
    X, Y, Z = floorb(x), floorb(y), floorb(z)               # FIND UNIT CUBE THAT CONTAINS POINT.
    x, y, z = x - floor(x), y - floor(y), z - floor(z)      # FIND RELATIVE X,Y,Z OF POINT IN CUBE.
    u, v, w = fade(x), fade(y), fade(z)                     # COMPUTE FADE CURVES FOR EACH OF X,Y,Z.
    A = p[X + 1] + Y; AA = p[A + 1] + Z; AB = p[A + 2] + Z
    B = p[X + 2] + Y; BA = p[B + 1] + Z; BB = p[B + 2] + Z  # HASH COORDINATES OF THE 8 CUBE CORNERS

    return lerp(w, lerp(v, lerp(u,   grad(p[AA + 1], x  , y  , z       ),  # AND ADD
                                     grad(p[BA + 1], x - 1, y  , z    )),  # BLENDED
                           lerp(u,   grad(p[AB + 1], x  , y - 1, z     ),  # RESULTS
                                     grad(p[BB + 1], x - 1, y - 1, z ))),  # FROM  8
                     lerp(v, lerp(u, grad(p[AA + 2], x  , y  , z - 1   ),  # CORNERS
                                     grad(p[BA + 2], x - 1, y  , z - 1)),  # OF CUBE.
                             lerp(u, grad(p[AB + 2], x  , y - 1, z - 1 ),
                                     grad(p[BB + 2], x - 1, y - 1, z - 1))))
end

println("Perlin noise applied to (3.14, 42.0, 7.0) gives ", perlinsnoise(3.14, 42.0, 7.0))

```
{{out}}

```txt

Perlin noise applied to (3.14, 42.0, 7.0) gives 0.13691995878400012

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.3

object Perlin {

    private val permutation = intArrayOf(
        151, 160, 137,  91,  90,  15, 131,  13, 201,  95,  96,  53, 194, 233,   7, 225,
        140,  36, 103,  30,  69, 142,   8,  99,  37, 240,  21,  10,  23, 190,   6, 148,
        247, 120, 234,  75,   0,  26, 197,  62,  94, 252, 219, 203, 117,  35,  11,  32,
         57, 177,  33,  88, 237, 149,  56,  87, 174,  20, 125, 136, 171, 168,  68, 175,  
         74, 165,  71, 134, 139,  48,  27, 166,  77, 146, 158, 231,  83, 111, 229, 122,
         60, 211, 133, 230, 220, 105,  92,  41,  55,  46, 245,  40, 244, 102, 143,  54,
         65,  25,  63, 161,   1, 216,  80,  73, 209,  76, 132, 187, 208,  89,  18, 169,
        200, 196, 135, 130, 116, 188, 159,  86, 164, 100, 109, 198, 173, 186,   3,  64,
         52, 217, 226, 250, 124, 123,   5, 202,  38, 147, 118, 126, 255,  82,  85, 212,
        207, 206,  59, 227,  47,  16,  58,  17, 182, 189,  28,  42, 223, 183, 170, 213,
        119, 248, 152,   2,  44, 154, 163,  70, 221, 153, 101, 155, 167,  43, 172,   9,
        129,  22,  39, 253,  19,  98, 108, 110,  79, 113, 224, 232, 178, 185, 112, 104,
        218, 246,  97, 228, 251,  34, 242, 193, 238, 210, 144,  12, 191, 179, 162, 241,
         81,  51, 145, 235, 249,  14, 239, 107,  49, 192, 214,  31, 181, 199, 106, 157,
        184,  84, 204, 176, 115, 121,  50,  45, 127,   4, 150, 254, 138, 236, 205,  93,
        222, 114,  67,  29,  24,  72, 243, 141, 128, 195,  78,  66, 215,  61, 156, 180
    )

    private val p = IntArray(512) { 
        if (it < 256) permutation[it] else permutation[it - 256] 
    }
   
    fun noise(x: Double, y: Double, z: Double): Double {
        // Find unit cube that contains point
        val xi = Math.floor(x).toInt() and 255
        val yi = Math.floor(y).toInt() and 255
        val zi = Math.floor(z).toInt() and 255

        // Find relative x, y, z of point in cube
        val xx = x - Math.floor(x)
        val yy = y - Math.floor(y)
        val zz = z - Math.floor(z)        

        // Compute fade curves for each of xx, yy, zz
        val u = fade(xx)
        val v = fade(yy)
        val w = fade(zz)

        // Hash co-ordinates of the 8 cube corners 
        // and add blended results from 8 corners of cube

        val a  = p[xi] + yi
        val aa = p[a] + zi
        val ab = p[a + 1] + zi
        val b  = p[xi + 1] + yi
        val ba = p[b] + zi
        val bb = p[b + 1] + zi
        
        return lerp(w, lerp(v, lerp(u, grad(p[aa], xx, yy, zz),
                                       grad(p[ba], xx - 1, yy, zz)),
                               lerp(u, grad(p[ab], xx, yy - 1, zz),
                                       grad(p[bb], xx - 1, yy - 1, zz))),
                       lerp(v, lerp(u, grad(p[aa + 1], xx, yy, zz - 1),
                                       grad(p[ba + 1], xx - 1, yy, zz - 1)),
                               lerp(u, grad(p[ab + 1], xx, yy - 1, zz - 1),
                                       grad(p[bb + 1], xx - 1, yy - 1, zz - 1))))
    }

    private fun fade(t: Double) = t * t * t * (t * (t * 6 - 15) + 10)

    private fun lerp(t: Double, a: Double, b: Double) = a + t * (b - a) 

    private fun grad(hash: Int, x: Double, y: Double, z: Double): Double {
        // Convert low 4 bits of hash code into 12 gradient directions
        val h = hash and 15  
        val u = if (h < 8) x else y
        val v = if (h < 4) y else if (h == 12 || h == 14) x else z
        return (if ((h and 1) == 0) u else -u) +   
               (if ((h and 2) == 0) v else -v)
    } 
}

fun main(args: Array<String>) {
    println(Perlin.noise(3.14, 42.0, 7.0))
}
```


{{out}}

```txt

0.13691995878400012

```



## OCaml



```ocaml
let permutation = [151;160;137;91;90;15;
131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180]

let lerp (t, a, b) = a +. t *. (b -. a)

let fade t =
  (t *. t *. t) *. (t *. (t *. 6. -. 15.) +. 10.)

let grad (hash, x, y, z) =
  let h = hash land 15 in
  let u = if (h < 8) then x else y in
  let v = if (h < 4) then y else (if (h = 12 || h = 14) then x else z) in
  (if (h land 1 = 0) then u else (0. -. u)) +.
    (if (h land 2 = 0) then v else (0. -. v))

let perlin_init p = 
  List.rev (List.fold_left (fun i x -> x :: i) (List.rev p) p);;

let perlin_noise p x y z =
  let x1 = (int_of_float x) land 255 and
      y1 = (int_of_float y) land 255 and
      z1 = (int_of_float z) land 255 and
      xi = x -. (float (int_of_float x)) and
      yi = y -. (float (int_of_float y)) and
      zi = z -. (float (int_of_float z)) in
  let u = fade xi and
      v = fade yi and
      w = fade zi and
      a = (List.nth p x1) + y1 in
  let aa = (List.nth p a) + z1 and
      ab = (List.nth p (a + 1)) + z1 and
      b = (List.nth p (x1 + 1)) + y1 in
  let ba = (List.nth p b) + z1 and
      bb = (List.nth p (b + 1)) + z1 in
  lerp(w, lerp(v, lerp(u, (grad ((List.nth p aa), xi, yi, zi)),
                          (grad ((List.nth p ba), xi -. 1., yi , zi))),
                  lerp(u, (grad ((List.nth p ab), xi , yi -. 1., zi)),
                          (grad ((List.nth p bb), xi -. 1., yi -. 1., zi)))),
          lerp(v, lerp(u, (grad ((List.nth p (aa + 1)), xi, yi, zi -. 1.)),
                          (grad ((List.nth p (ba + 1)), xi -. 1., yi , zi -. 1.))),
                  lerp(u, (grad ((List.nth p (ab + 1)), xi , yi -. 1., zi -.  1.)),
                          (grad ((List.nth p (bb + 1)), xi -. 1., yi -.  1., zi -. 1.)))))

;;

let p = perlin_init permutation in
  print_string ((Printf.sprintf "%0.17f" (perlin_noise p 3.14 42.0 7.0)) ^ "\n")
```

{{out}}

```txt
0.13691995878400012
```



## Perl

{{trans|Java}}

```perl
use strict;
use warnings;

use constant permutation => qw{
151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8
99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35
11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134
139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46
245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200
196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123
5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223
183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39
253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193
238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181
199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114
67 29 24 72 243 141 128 195 78 66 215 61 156 180
};
use constant p => (permutation, permutation);

sub floor {
    my $x = shift;
    my $xi = int($x);
    return $x < $xi ? $xi - 1 : $xi;
}

sub fade { $_ = shift; $_ * $_ * $_ * ($_ * ($_ * 6 - 15) + 10) }
sub lerp { $_[1] + $_[0] * ($_[2] - $_[1]) }
sub grad {
    my ($h, $x, $y, $z) = @_[0..3];
    $h &= 15;
    my $u = $h < 8 ? $x : $y;
    my $v = $h < 4 ? $y :
    $h == 12 || $h == 14 ? $x : $z;
    return (($h & 1) == 0 ? $u : -$u) + (($h & 2) == 0 ? $v : -$v);
}

sub noise {
    my ($X, $Y, $Z) = map { floor($_) & 255 }
    my ($x, $y, $z) = @_[0,1,2];
    my ($u, $v, $w) = map { fade($_) }
    $x -= $X, $y -= $Y, $z -= $Z;
    my $A = (p)[$X] + $Y;
    my ($AA, $AB) = ( (p)[$A] + $Z, (p)[$A + 1] + $Z );
    my $B = (p)[$X + 1] + $Y;
    my ($BA, $BB) = ( (p)[$B] + $Z, (p)[$B + 1] + $Z );   
    lerp($w, lerp($v, lerp($u,
		grad( (p)[$AA],     $x, $y, $z ),  
		grad( (p)[$BA], $x - 1, $y, $z )
	    ),
	    lerp($u,
		grad( (p)[$AB],     $x, $y - 1, $z ),
		grad( (p)[$BB], $x - 1, $y - 1, $z )
	    )
	),
	lerp($v, lerp($u, grad((p)[$AA + 1], $x, $y, $z - 1 ),
		grad((p)[$BA + 1], $x - 1, $y, $z - 1 )),
	    lerp($u, grad((p)[$AB + 1], $x, $y - 1, $z - 1 ),
		grad((p)[$BB + 1], $x - 1, $y - 1, $z - 1 ))
	)
    );
}

print noise 3.14, 42, 7;
```

{{out}}

```txt
0.136919958784
```



## Perl 6

{{trans|Java}}

```perl6
constant @p = map {:36($_)}, flat <
47 4G 3T 2J 2I F 3N D 5L 2N 2O 1H 5E 6H 7 69 3W 10 2V U 1X 3Y 8 2R 11 6O L A N
5A 6 44 6V 3C 6I 23 0 Q 5H 1Q 2M 70 63 5N 39 Z B W 1L 4X X 2G 6L 45 1K 2F 4U K
3H 3S 4R 4O 1W 4V 22 4L 1Z 3Q 3V 1C R 4M 25 42 4E 6F 2B 33 6D 3E 1O 5V 3P 6E 64
2X 2K 15 1J 1A 6T 14 6S 2U 3Z 1I 1T P 1R 4H 1 60 28 21 5T 24 3O 57 5S 2H I 4P
5K 5G 3R 3M 38 58 4F 2E 4K 2S 31 5I 4T 56 3 1S 1G 61 6A 6Y 3G 3F 5 5M 12 43 3A
3I 73 2A 2D 5W 5R 5Q 1N 6B 1B G 1M H 52 59 S 16 67 53 4Q 5X 3B 6W 48 2 18 4A 4J
1Y 65 49 2T 4B 4N 17 4S 9 3L M 13 71 J 2Q 30 32 27 35 68 6G 4Y 55 34 2W 62 6U
2P 6C 6Z Y 6Q 5D 6M 5U 40 C 5B 4Z 4I 6P 29 1F 41 6J 6X E 6N 2Z 1D 5C 5Y V 51 5J
2Y 4D 54 2C 5O 4W 37 3D 1E 19 3J 4 46 72 3U 6K 5P 2L 66 36 1V T O 20 6R 3X 3K
5F 26 1U 5Z 1P 4C 50
> xx 2;

sub fade($_) { $_ * $_ * $_ * ($_ * ($_ * 6 - 15) + 10) }
sub lerp($t, $a, $b) { $a + $t * ($b - $a) }
sub grad($h is copy, $x, $y, $z) {
    $h +&= 15;
    my $u = $h < 8 ?? $x !! $y;
    my $v = $h < 4 ?? $y !! $h == 12|14 ?? $x !! $z;
    ($h +& 1 ?? -$u !! $u) + ($h +& 2 ?? -$v !! $v);
}

sub noise($x is copy, $y is copy, $z is copy) is export {
    my ($X, $Y, $Z) = ($x, $y, $z)».floor »+&» 255;
    my ($u, $v, $w) = map &fade, $x -= $X, $y -= $Y, $z -= $Z;
    my ($AA, $AB) = @p[$_] + $Z, @p[$_ + 1] + $Z given @p[$X] + $Y;
    my ($BA, $BB) = @p[$_] + $Z, @p[$_ + 1] + $Z given @p[$X + 1] + $Y;
    lerp($w, lerp($v, lerp($u, grad(@p[$AA    ], $x    , $y    , $z     ),  
                               grad(@p[$BA    ], $x - 1, $y    , $z     )), 
                      lerp($u, grad(@p[$AB    ], $x    , $y - 1, $z     ),  
                               grad(@p[$BB    ], $x - 1, $y - 1, $z     ))),
             lerp($v, lerp($u, grad(@p[$AA + 1], $x    , $y    , $z - 1 ),  
                               grad(@p[$BA + 1], $x - 1, $y    , $z - 1 )), 
                      lerp($u, grad(@p[$AB + 1], $x    , $y - 1, $z - 1 ),
                               grad(@p[$BB + 1], $x - 1, $y - 1, $z - 1 ))));
}

say noise 3.14, 42, 7;
```

{{out}}

```txt
0.13691995878
```



## Phix


```Phix
constant ph = x"97 A0 89 5B 5A 0F 83 0D C9 5F 60 35 C2 E9 07 E1"&
              x"8C 24 67 1E 45 8E 08 63 25 F0 15 0A 17 BE 06 94"&
              x"F7 78 EA 4B 00 1A C5 3E 5E FC DB CB 75 23 0B 20"&
              x"39 B1 21 58 ED 95 38 57 AE 14 7D 88 AB A8 44 AF"&
              x"4A A5 47 86 8B 30 1B A6 4D 92 9E E7 53 6F E5 7A"&
              x"3C D3 85 E6 DC 69 5C 29 37 2E F5 28 F4 66 8F 36"&
              x"41 19 3F A1 01 D8 50 49 D1 4C 84 BB D0 59 12 A9"&
              x"C8 C4 87 82 74 BC 9F 56 A4 64 6D C6 AD BA 03 40"&
              x"34 D9 E2 FA 7C 7B 05 CA 26 93 76 7E FF 52 55 D4"&
              x"CF CE 3B E3 2F 10 3A 11 B6 BD 1C 2A DF B7 AA D5"&
              x"77 F8 98 02 2C 9A A3 46 DD 99 65 9B A7 2B AC 09"&
              x"81 16 27 FD 13 62 6C 6E 4F 71 E0 E8 B2 B9 70 68"&
              x"DA F6 61 E4 FB 22 F2 C1 EE D2 90 0C BF B3 A2 F1"&
              x"51 33 91 EB F9 0E EF 6B 31 C0 D6 1F B5 C7 6A 9D"&
              x"B8 54 CC B0 73 79 32 2D 7F 04 96 FE 8A EC CD 5D"&
              x"DE 72 43 1D 18 48 F3 8D 80 C3 4E 42 D7 3D 9C B4",
        p = ph&ph   
 
function fade(atom t) return t * t * t * (t * (t * 6 - 15) + 10) end function
function lerp(atom t, a, b) return a + t * (b - a) end function
function grad(int hash, atom x, y, z)
    int h = and_bits(hash,15)
    atom u = iff(h<8 ? x : y),
         v = iff(h<4 ? y : iff(h==12 or h==14 ? x : z))
    return iff(and_bits(h,1) == 0 ? u : -u) +
           iff(and_bits(h,2) == 0 ? v : -v)
end function

function noise(atom x, y, z)
    integer X = and_bits(x,255),
            Y = and_bits(y,255),
            Z = and_bits(z,255)
    x -= floor(x)
    y -= floor(y)
    z -= floor(z)
    atom u = fade(x),
         v = fade(y),
         w = fade(z)
    integer A = p[X+1]+Y, AA = p[A+1]+Z, AB = p[A+2]+Z,
            B = p[X+2]+Y, BA = p[B+1]+Z, BB = p[B+2]+Z

    return lerp(w,lerp(v,lerp(u,grad(p[AA+1], x  , y  , z   ),
                                grad(p[BA+1], x-1, y  , z   )),
                         lerp(u,grad(p[AB+1], x  , y-1, z   ),
                                grad(p[BB+1], x-1, y-1, z   ))),
                  lerp(v,lerp(u,grad(p[AA+2], x  , y  , z-1 ),
                                grad(p[BA+2], x-1, y  , z-1 )),
                         lerp(u,grad(p[AB+2], x  , y-1, z-1 ),
                                grad(p[BB+2], x-1, y-1, z-1 ))))
end function

printf(1,"Perlin Noise for (3.14,42,7) is %.17f\n",{noise(3.14,42,7)})
```

{{out}}

```txt

Perlin Noise for (3.14,42,7) is 0.1369199587840001

```



## Python

{{trans|Java}}


```python
def perlin_noise(x, y, z):
    X = int(x) & 255                  # FIND UNIT CUBE THAT
    Y = int(y) & 255                  # CONTAINS POINT.
    Z = int(z) & 255
    x -= int(x)                                # FIND RELATIVE X,Y,Z
    y -= int(y)                                # OF POINT IN CUBE.
    z -= int(z)
    u = fade(x)                                # COMPUTE FADE CURVES
    v = fade(y)                                # FOR EACH OF X,Y,Z.
    w = fade(z)
    A = p[X  ]+Y; AA = p[A]+Z; AB = p[A+1]+Z      # HASH COORDINATES OF
    B = p[X+1]+Y; BA = p[B]+Z; BB = p[B+1]+Z      # THE 8 CUBE CORNERS,
 
    return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ),  # AND ADD
                                   grad(p[BA  ], x-1, y  , z   )), # BLENDED
                           lerp(u, grad(p[AB  ], x  , y-1, z   ),  # RESULTS
                                   grad(p[BB  ], x-1, y-1, z   ))),# FROM  8
                   lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ),  # CORNERS
                                   grad(p[BA+1], x-1, y  , z-1 )), # OF CUBE
                           lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
                                   grad(p[BB+1], x-1, y-1, z-1 ))))
                                   
def fade(t): 
    return t ** 3 * (t * (t * 6 - 15) + 10)
    
def lerp(t, a, b):
    return a + t * (b - a)
    
def grad(hash, x, y, z):
    h = hash & 15                      # CONVERT LO 4 BITS OF HASH CODE
    u = x if h<8 else y                # INTO 12 GRADIENT DIRECTIONS.
    v = y if h<4 else (x if h in (12, 14) else z)
    return (u if (h&1) == 0 else -u) + (v if (h&2) == 0 else -v)

p = [None] * 512
permutation = [151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180]
for i in range(256):
    p[256+i] = p[i] = permutation[i]

if __name__ == '__main__':
    print("%1.17f" % perlin_noise(3.14, 42, 7))
```


{{out}}

```txt
0.13691995878400012
```



## Racket

{{trans|Java}} -- because we were asked to


```racket
#lang racket
(define (floor-to-255 x)
  (bitwise-and (exact-floor x) #xFF))

(define (fade t)
  (* t t t (+ 10 (* t (- (* t 6) 15)))))

(define (lerp t a b)
  (+ a (* t (- b a))))

;; CONVERT LO 4 BITS OF HASH CODE INTO 12 GRADIENT DIRECTIONS.
(define (grad hsh x y z)
  (define h (bitwise-and hsh #x0F))
  (define u (if (< h 8) x y))
  (define v (cond [(< h 4) y] [(or (= h 12) (= h 14)) x] [else z]))
  (+ (if (bitwise-bit-set? h 0) (- u) u) (if (bitwise-bit-set? h 1) (- v) v)))

(define permutation
  (vector
   151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23
   190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20
   125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220
   105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196
   135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255
   82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221
   153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228
   251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199
   106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128
   195 78 66 215 61 156 180))

(define p (make-vector 512))
(for ((offset (in-list '(0 256))))
  (vector-copy! p offset permutation))

(define-syntax-rule (p-ref n)
  (vector-ref p n))

(define (noise x y z)
  (let*
      (
       ;; FIND UNIT CUBE THAT CONTAINS POINT.
       (X (floor-to-255 x))
       (Y (floor-to-255 y))
       (Z (floor-to-255 z))
       ; FIND RELATIVE X,Y,Z OF POINT IN CUBE.
       (x (- x (floor x)))
       (y (- y (floor y)))
       (z (- z (floor z)))
       ;; COMPUTE FADE CURVES FOR EACH OF X,Y,Z.
       (u (fade x))
       (v (fade y))
       (w (fade z))
       ;; HASH COORDINATES OF THE 8 CUBE CORNERS...
       (A  (+ (p-ref X) Y))
       (AA (+ (p-ref A) Z))
       (AB (+ (p-ref (add1 A)) Z))
       (B  (+ (p-ref (add1 X)) Y))
       (BA (+ (p-ref B) Z))
       (BB (+ (p-ref (add1 B)) Z)))
    ;; .. AND ADD BLENDED RESULTS FROM 8 CORNERS OF CUBE
    (lerp
     w
     (lerp
      v (lerp u (grad (p-ref AA) x y z) (grad (p-ref BA) (sub1 x) y z))
      (lerp u (grad (p-ref AB) x (sub1 y) z) (grad (p-ref BB) (sub1 x) (sub1 y) z)))
     (lerp
      v
      (lerp u (grad (p-ref (add1 AA)) x y (sub1 z)) (grad (p-ref (add1 BA)) (sub1 x) y (sub1 z)))
      (lerp u (grad (vector-ref p (add1 AB)) x (sub1 y) (sub1 z))
            (grad (vector-ref p (add1 BB)) (sub1 x) (sub1 y) (sub1 z)))))))

(module+ test
  (noise 3.14 42 7))
```

{{out}}

```txt
0.13691995878400012
```



## REXX

{{trans|Go}}
Programming note:   the REXX operator   <big><b> '''//''' </b></big>   is the remainder for division   (not modulus),   so the absolute value of the 

remainder is used for this task.

The original decimal (data) table was compressed into a single hexadecimal value (without spaces).

```rexx
/*REXX program  implements a   Perlin noise algorithm   of a  point  in  3D─space.      */
_='97a0895b5a0f830dc95f6035c2e907e18c24671e458e086325f0150a17be0694f778ea4b001ac53e5efcdbcb75230b2039b12158ed953857ae147d88aba844af',
||'4aa547868b301ba64d929ee7536fe57a3cd385e6dc695c29372ef528f4668f3641193fa101d85049d14c84bbd05912a9c8c4878274bc9f56a4646dc6adba0340',
||'34d9e2fa7c7b05ca2693767eff5255d4cfce3be32f103a11b6bd1c2adfb7aad577f898022c9aa346dd99659ba72bac09811627fd13626c6e4f71e0e8b2b97068',
||'daf661e4fb22f2c1eed2900cbfb3a2f1513391ebf90eef6b31c0d61fb5c76a9db854ccb07379322d7f0496fe8aeccd5dde72431d1848f38d80c34e42d73d9cb4'
      do j=0  for length(_)%2;  @.j=x2d(substr(_,2*j+1,2)); end  /*assign indexed array.*/
parse arg x y z d .                              /*obtain optional arguments from the CL*/
if x=='' | x==","  then x=  3.14                 /*Not specified?  Then use the default.*/
if y=='' | y==","  then y= 42                    /* "      "         "   "   "     "    */
if z=='' | z==","  then z=  7                    /* "      "         "   "   "     "    */
if d=='' | d==","  then d=100                    /* "      "         "   "   "     "    */
numeric digits d                                 /*use  D  decimal digits for precision.*/
say 'Perlin noise for the 3D point  ['space(x y z, 3)"]  ───► "      PerlinNoise(x, y, z)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fade:  procedure; parse arg t;               return  t**3 * (t * (t * 6 - 15)  +  10)
floor: procedure; parse arg x;       _=x%1;  return  _  -   (x < 0)   *   (x \= _)
lerp:  procedure; parse arg t,a,b;           return  a  +    t * (b - a)
pick:  _=abs( arg(1) ) // 256;               return  @._
/*──────────────────────────────────────────────────────────────────────────────────────*/
grad:  procedure; parse arg hash,x,y,z;  _=abs(hash) // 16   /*force positive remainder.*/
             select
             when _== 0 | _==12  then return  x+y;    when _== 1 | _==14  then return  y-x
             when _== 2          then return  x-y;    when _== 3          then return -x-y
             when _== 4          then return  x+z;    when _== 5          then return  z-x
             when _== 6          then return  x-z;    when _== 7          then return -x-z
             when _== 8          then return  y+z;    when _== 9 | _==13  then return  z-y
             when _==10          then return  y-z
             otherwise           return -y-z                 /*for cases   11  or  15.  */
             end   /*select*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
PerlinNoise: procedure expose @.;     parse arg x,y,z
             x$= floor(x) // 256;     x = x - floor(x);    xm=x-1;      u= fade(x)
             y$= floor(y) // 256;     y = y - floor(y);    ym=y-1;      v= fade(y)
             z$= floor(z) // 256;     z = z - floor(z);    zm=z-1;      w= fade(z)
             a = pick(x$   ) + y$;    aa= pick(a) + z$;                ab= pick(a +1) + z$
             b = pick(x$ +1) + y$;    ba= pick(b) + z$;                bb= pick(b +1) + z$
             return lerp(w, lerp(v, lerp(u,  grad(  pick(aa  ),   x ,    y ,   z   ),    ,
                                             grad(  pick(ba  ),   xm,    y ,   z   )),   ,
                                    lerp(u,  grad(  pick(ab  ),   x ,    ym,   z   ),    ,
                                             grad(  pick(bb  ),   xm,    ym,   z   ))),  ,
                            lerp(v, lerp(u,  grad(  pick(aa+1),   x ,    y ,   zm  ),    ,
                                             grad(  pick(ba+1),   xm,    y ,   zm  )),   ,
                                    lerp(u,  grad(  pick(ab+1),   x ,    ym,   zm  ),    ,
                                             grad(  pick(bb+1),   xm,    ym,   zm  )))) /1
```

'''output'''   when using the default inputs:

(Note that REXX uses   ''decimal''   floating point,   not binary.)

```txt

Perlin noise for the 3D point  [3.14   42   7]  ───►  0.136919958784

```



## Sidef

{{trans|Perl 6}}

```ruby
const p = (%w'47 4G 3T 2J 2I F 3N D 5L 2N 2O 1H 5E 6H 7 69 3W 10 2V U 1X 3Y 8
2R 11 6O L A N 5A 6 44 6V 3C 6I 23 0 Q 5H 1Q 2M 70 63 5N 39 Z B W 1L 4X X 2G
6L 45 1K 2F 4U K 3H 3S 4R 4O 1W 4V 22 4L 1Z 3Q 3V 1C R 4M 25 42 4E 6F 2B 33 6D
3E 1O 5V 3P 6E 64 2X 2K 15 1J 1A 6T 14 6S 2U 3Z 1I 1T P 1R 4H 1 60 28 21 5T 24
3O 57 5S 2H I 4P 5K 5G 3R 3M 38 58 4F 2E 4K 2S 31 5I 4T 56 3 1S 1G 61 6A 6Y 3G
3F 5 5M 12 43 3A 3I 73 2A 2D 5W 5R 5Q 1N 6B 1B G 1M H 52 59 S 16 67 53 4Q 5X
3B 6W 48 2 18 4A 4J 1Y 65 49 2T 4B 4N 17 4S 9 3L M 13 71 J 2Q 30 32 27 35 68
6G 4Y 55 34 2W 62 6U 2P 6C 6Z Y 6Q 5D 6M 5U 40 C 5B 4Z 4I 6P 29 1F 41 6J 6X E
6N 2Z 1D 5C 5Y V 51 5J 2Y 4D 54 2C 5O 4W 37 3D 1E 19 3J 4 46 72 3U 6K 5P 2L 66
36 1V T O 20 6R 3X 3K 5F 26 1U 5Z 1P 4C 50' * 2 -> map {|n| Num(n, 36) })

func fade(n) { n * n * n * (n * (n * 6 - 15) + 10) }
func lerp(t, a, b) { a + t*(b-a) }

func grad(h, x, y, z) {
    h &= 15
    var u = (h < 8 ? x : y)
    var v = (h < 4 ? y : (h ~~ [12,14] ? x : z))
    (h&1 ? -u : u) + (h&2 ? -v : v)
}

func noise(x, y, z) {
    var(X, Y, Z) = [x, y, z].map { .floor & 255 }...
    var (u, v, w) = [x-=X, y-=Y, z-=Z].map { fade(_) }...
    var (AA, AB) = with(p[X]   + Y) {|i| (p[i] + Z, p[i+1] + Z) }
    var (BA, BB) = with(p[X+1] + Y) {|i| (p[i] + Z, p[i+1] + Z) }
    lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z  ),
                            grad(p[BA  ], x-1, y  , z  )),
                    lerp(u, grad(p[AB  ], x  , y-1, z  ),
                            grad(p[BB  ], x-1, y-1, z  ))),
            lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1),
                            grad(p[BA+1], x-1, y  , z-1)),
                    lerp(u, grad(p[AB+1], x  , y-1, z-1),
                            grad(p[BB+1], x-1, y-1, z-1))))
}

say noise(3.14, 42, 7)
```

{{out}}

```txt

0.136919958784

```


## Tcl

{{works with|Tcl|8.6}}

```tcl
namespace eval perlin {
    proc noise {x y z} {
	# Find unit cube that contains point.
	set X [expr {int(floor($x)) & 255}]
	set Y [expr {int(floor($y)) & 255}]
	set Z [expr {int(floor($z)) & 255}]

	# Find relative x,y,z of point in cube.
	set x [expr {$x - floor($x)}]
	set y [expr {$y - floor($y)}]
	set z [expr {$z - floor($z)}]

	# Compute fade curves for each of x,y,z.
	set u [expr {fade($x)}]
	set v [expr {fade($y)}]
	set w [expr {fade($z)}]

	# Hash coordinates of the 8 cube corners...
	variable p
	set A  [expr {p($X)   + $Y}]
	set AA [expr {p($A)   + $Z}]
	set AB [expr {p($A+1) + $Z}]
	set B  [expr {p($X+1) + $Y}]
	set BA [expr {p($B)   + $Z}]
	set BB [expr {p($B+1) + $Z}]

	# And add blended results from 8 corners of cube
	return [expr {
	    lerp($w, lerp($v, lerp($u, grad(p($AA),   $x,   $y,   $z ),
				       grad(p($BA),   $x-1, $y,   $z )),
			      lerp($u, grad(p($AB),   $x,   $y-1, $z ),
				       grad(p($BB),   $x-1, $y-1, $z ))),
		     lerp($v, lerp($u, grad(p($AA+1), $x,   $y,   $z-1 ),
				       grad(p($BA+1), $x-1, $y,   $z-1 )),
			      lerp($u, grad(p($AB+1), $x,   $y-1, $z-1 ),
				       grad(p($BB+1), $x-1, $y-1, $z-1 ))))
	}]
    }

    namespace eval tcl::mathfunc {
	proc p    {idx}   {lindex $::perlin::permutation $idx}
	proc fade {t}     {expr { $t**3 * ($t * ($t * 6 - 15) + 10) }}
	proc lerp {t a b} {expr { $a + $t * ($b - $a) }}
	proc grad {hash x y z} {
	    # Convert low 4 bits of hash code into 12 gradient directions
	    set h [expr { $hash & 15 }]
	    set u [expr { $h<8 ? $x : $y }]
	    set v [expr { $h<4 ? $y : ($h==12 || $h==14) ? $x : $z }]
	    expr { (($h&1)==0 ? $u : -$u) + (($h&2)==0 ? $v : -$v) }
	}
    }

    apply {{} {
	binary scan [binary format H* [join {
	    97A0895B5A0F830DC95F6035C2E907E18C24671E458E086325F0150A17BE0694F7
	    78EA4B001AC53E5EFCDBCB75230B2039B12158ED953857AE147D88ABA844AF4AA5
            47868B301BA64D929EE7536FE57A3CD385E6DC695C29372EF528F4668F3641193F
	    A101D85049D14C84BBD05912A9C8C4878274BC9F56A4646DC6ADBA034034D9E2FA
            7C7B05CA2693767EFF5255D4CFCE3BE32F103A11B6BD1C2ADFB7AAD577F898022C
	    9AA346DD99659BA72BAC09811627FD13626C6E4F71E0E8B2B97068DAF661E4FB22
            F2C1EED2900CBFB3A2F1513391EBF90EEF6B31C0D61FB5C76A9DB854CCB0737932
	    2D7F0496FE8AECCD5DDE72431D1848F38D80C34E42D73D9CB4
	} ""]] cu* p
	variable ::perlin::permutation [concat $p $p]
    }}
}

puts [perlin::noise 3.14 42 7]
```

{{out}}
 0.13691995878400012


## zkl

{{trans|Java}}

```zkl
class [static] ImprovedNoise{  // a container, not an object
   fcn noise(xyz){ xyz=vm.arglist.apply("toFloat");
      X,Y,Z:=	// FIND UNIT CUBE THAT CONTAINS POINT.
         xyz.apply(fcn(x){ x.floor().toInt().bitAnd(255) });
      xyz=      // FIND RELATIVE X,Y,Z OF POINT IN CUBE.
         xyz.apply(fcn(x){ x - x.floor() });
      u,v,w:= xyz.apply(fade);  // COMPUTE FADE CURVES FOR EACH OF X,Y,Z.
      A,AA,AB:= p[X  ]+Y, p[A]+Z, p[A+1]+Z;      // HASH COORDINATES OF
      B,BA,BB:= p[X+1]+Y, p[B]+Z, p[B+1]+Z;      // THE 8 CUBE CORNERS,
      x,y,z:=xyz;
      lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ),  // AND ADD
                              grad(p[BA  ], x-1, y  , z   )), // BLENDED
                      lerp(u, grad(p[AB  ], x  , y-1, z   ),  // RESULTS
                              grad(p[BB  ], x-1, y-1, z   ))),// FROM  8
              lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ),  // CORNERS
                              grad(p[BA+1], x-1, y  , z-1 )), // OF CUBE
                      lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
                              grad(p[BB+1], x-1, y-1, z-1 ))));
   }
   fcn [private] fade(t){ t*t*t*(t*(t*6 - 15) + 10) }
   fcn [private] lerp(t,a,b){ a + t*(b - a) }
   fcn [private] grad(hash,x,y,z){
      h:=hash.bitAnd(15);		// CONVERT LO 4 BITS OF HASH CODE
      u:=(if(h<8) x else y);		// INTO 12 GRADIENT DIRECTIONS.
      v:=(if(h<4) y else ((h==12 or h==14) and x or z));
      (if(h.isEven) u else -u) + (if(h.bitAnd(2)==0) v else -v)
   }
   var [const,private] permutation=Data(Void, 151,160,137,91,90,15,
    131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
    190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
    88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
    77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
    102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
    135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
    5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
    223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
    129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
    251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
    49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
    138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180),
    p=Data(Void,permutation,permutation);
}
```


```zkl
ImprovedNoise.noise(3.14, 42, 7).println();
```

{{out}}

```txt
0.13692
```


+++
title = "Koch curve"
description = ""
date = 2019-09-08T01:04:53Z
aliases = []
[extra]
id = 21785
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "basic256",
  "c",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "javascript",
  "julia",
  "kotlin",
  "mathematica",
  "nim",
  "perl",
  "perl_6",
  "phix",
  "python",
  "qbasic",
  "racket",
  "ring",
  "sidef",
  "zkl",
]
+++

Draw a Koch curve. See details: [https://en.wikipedia.org/wiki/Koch_snowflake Koch curve]


## BASIC256


```BASIC256

global RtoD, DtoR
RtoD = 180 / Pi
DtoR = Pi / 180

global posX, posY, angulo
posX = 170 : posY = 100 : angulo = 0

global ancho, alto
ancho = 650 : alto = 650
graphsize ancho, alto

subroutine kochLado(longitud, fondo)
	if fondo = 0 then
		dx = cos(angulo*DtoR) * longitud
		dy = sin(angulo*DtoR) * longitud
		color rgb(5,100,24)
		line (posX, posY, posX+dx, posY+dy)
		posX += dx
		posY += dy
	else
		call kochLado(longitud/3.0, fondo-1)
		angulo += 60
		call kochLado(longitud/3.0, fondo-1)
		angulo -= 120
		call kochLado(longitud/3.0, fondo-1)
		angulo += 60
		call kochLado(longitud/3.0, fondo-1)
	end if
end subroutine

subroutine CopoNieveKoch(longitud, recursionfondo)
	for i = 1 to 6
		call kochLado(longitud,recursionfondo)
		angulo -= 300
	next i
end subroutine

for n = 0 To 7
	clg
	fastgraphics
	text 3,4, "Copo de nieve de Koch"
	text 4,16, "Iteración número: " & n
	call CopoNieveKoch(280, n)
	pause 0.8
	refresh
next n

imgsave "Koch_curve.jpg", "jpg"
end

```



## C

Interactive program which takes the width, height (of the Graphics window) and recursion level of the Koch curve as inputs, prints out usage on incorrect invocation. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>
#include<stdlib.h>
#include<stdio.h>
#include<math.h>

#define pi M_PI

typedef struct{
	double x,y;
}point;

void kochCurve(point p1,point p2,int times){
	point p3,p4,p5;
	double theta = pi/3;
	
	if(times>0){
		p3 = (point){(2*p1.x+p2.x)/3,(2*p1.y+p2.y)/3};
		p5 = (point){(2*p2.x+p1.x)/3,(2*p2.y+p1.y)/3};
		
		p4 = (point){p3.x + (p5.x - p3.x)*cos(theta) + (p5.y - p3.y)*sin(theta),p3.y - (p5.x - p3.x)*sin(theta) + (p5.y - p3.y)*cos(theta)};
		
		kochCurve(p1,p3,times-1);
		kochCurve(p3,p4,times-1);
		kochCurve(p4,p5,times-1);
		kochCurve(p5,p2,times-1);
	}
	
	else{
		line(p1.x,p1.y,p2.x,p2.y);
	}
}

int main(int argC, char** argV)
{
	int w,h,r;
	point p1,p2;
	
	if(argC!=4){
		printf("Usage : %s <window width> <window height> <recursion level>",argV[0]);
	}
	
	else{
		w = atoi(argV[1]);
		h = atoi(argV[2]);
		r = atoi(argV[3]);
		
		initwindow(w,h,"Koch Curve");
		
		p1 = (point){10,h-10};
		p2 = (point){w-10,h-10};
		
		kochCurve(p1,p2,r);
		
		getch();
	
		closegraph();
	}
	
	return 0;
}

```



## Factor

The approach taken is to generate a [[Thue-Morse]] sequence. Using turtle graphics, move forward for each 0 encountered and rotate 60 degrees for each 1 encountered. Remarkably, this produces a Koch curve.

```factor
USING: accessors images images.testing images.viewer kernel
literals math math.constants math.functions sequences ;
IN: rosetta-code.koch-curve

CONSTANT: order 17
CONSTANT: theta 1.047197551196598      ! 60 degrees in radians
CONSTANT: move-distance 0.25
CONSTANT: dim { 600 400 }
CONSTANT: offset-x 500
CONSTANT: offset-y 300

: <koch-image> ( -- image )
    <rgb-image> dim >>dim
    dim product 3 * [ 255 ] B{ } replicate-as >>bitmap ;

: thue-morse ( n -- seq )
    { 0 } swap [ [ ] [ [ 1 bitxor ] map ] bi append ] times ;

TUPLE: turtle
    { heading initial: 0 } { x initial: 0 } { y initial: 0 } ;

: turn ( turtle -- turtle' )
    [ theta + 2pi mod ] change-heading ;

: move ( turtle -- turtle' )
    dup heading>> [ cos move-distance * + ] curry change-x
    dup heading>> [ sin move-distance * + ] curry change-y ;

: step ( turtle elt -- turtle' )
    [ move ] [ drop turn ] if-zero ;

: setup-pixel ( turtle -- pixel x y )
    { 0 0 0 } swap [ x>> ] [ y>> ] bi
    [ >integer ] bi@ [ offset-x + ] [ offset-y + ] bi* ;

: koch-curve ( -- )
    <koch-image> turtle new over order thue-morse [
        [ dup setup-pixel ] [ set-pixel-at ] [ step drop ] tri*
    ] 2with each image-window ;

MAIN: koch-curve
```

[https://i.imgur.com/MVS8QiS.png]


## FreeBASIC


```freebasic

Const Pi = 4 * Atn(1)
Const RtoD = 180 / Pi
Const DtoR = Pi / 180

Dim Shared As Single posX = 260, posY = 90, angulo = 0

Screen 19 : Color 0,15

Sub kochLado(longitud As Integer, fondo As Integer)
    Dim As Single dx, dy
    If fondo = 0 Then
        dx = Cos(angulo*DtoR) * longitud
        dy = Sin(angulo*DtoR) * longitud
        Line (posX, posY)-(posX+dx, posY+dy), 2
        posX += dx
        posY += dy
    Else
        kochLado(longitud/3.0, fondo-1)
        angulo += 60
        kochLado(longitud/3.0, fondo-1)
        angulo -= 120
        kochLado(longitud/3.0, fondo-1)
        angulo += 60
        kochLado(longitud/3.0, fondo-1)
    End If
End Sub

Sub CopoNieveKoch(longitud As Integer, recursionfondo As Integer)
    For i As Integer = 1 To 6
        kochLado(longitud,recursionfondo)
        angulo -= 300
    Next i
End Sub

For n As Integer = 0 To 5
    Cls
    Locate 3,4: Print "Copo de nieve de Koch"
    Locate 4,4: Print "Iteracion numero: " & n
    CopoNieveKoch(280, n)
    Sleep 800
Next n
color 4: Locate 6,4: Print "Pulsa una tecla..."
Bsave "Koch_curve.bmp",0
Sleep
End

```



## Go

```go
package main

import (
    "github.com/fogleman/gg"
    "math"
)

var dc = gg.NewContext(512, 512)

func koch(x1, y1, x2, y2 float64, iter int) {
    angle := math.Pi / 3 // 60 degrees
    x3 := (x1*2 + x2) / 3
    y3 := (y1*2 + y2) / 3
    x4 := (x1 + x2*2) / 3
    y4 := (y1 + y2*2) / 3
    x5 := x3 + (x4-x3)*math.Cos(angle) + (y4-y3)*math.Sin(angle)
    y5 := y3 - (x4-x3)*math.Sin(angle) + (y4-y3)*math.Cos(angle)
    if iter > 0 {
        iter--
        koch(x1, y1, x3, y3, iter)
        koch(x3, y3, x5, y5, iter)
        koch(x5, y5, x4, y4, iter)
        koch(x4, y4, x2, y2, iter)
    } else {
        dc.LineTo(x1, y1)
        dc.LineTo(x3, y3)
        dc.LineTo(x5, y5)
        dc.LineTo(x4, y4)
        dc.LineTo(x2, y2)
    }
}

func main() {
    dc.SetRGB(1, 1, 1) // White background
    dc.Clear()
    koch(100, 100, 400, 400, 4)
    dc.SetRGB(0, 0, 1) // Blue curve
    dc.SetLineWidth(2)
    dc.Stroke()
    dc.SavePNG("koch.png")
}
```


```txt

Image is similar to Ring entry.

```



## Haskell


Generates SVG for a Koch snowflake. To view, save to a text file with an .svg extension, and open in a browser.


```haskell
import Control.Arrow ((***))
import Text.Printf (printf)

kochSnowflake :: Int -> (Float, Float) -> (Float, Float) -> [(Float, Float)]
kochSnowflake n a b =
  let points@(x:xs) = [a, equilateralApex a b, b]
  in concat $ zipWith (kochCurve n) points (xs ++ [x])

kochCurve :: Int -> (Float, Float) -> (Float, Float) -> [(Float, Float)]
kochCurve n ab xy =
  let go 0 (_, xy) = [xy]
      go n (ab, xy) =
        let (mp, mq) = midThirdOfLine ab xy
            points@(_:xs) = [ab, mp, equilateralApex mp mq, mq, xy]
        in go (pred n) =<< zip points xs
  in ab : go n (ab, xy)


equilateralApex :: (Float, Float) -> (Float, Float) -> (Float, Float)
equilateralApex = rotatedPoint (pi / 3)

rotatedPoint :: Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
rotatedPoint theta (ox, oy) (a, b) =
  let (dx, dy) = rotatedVector theta (a - ox, oy - b)
  in (ox + dx, oy - dy)

rotatedVector :: Float -> (Float, Float) -> (Float, Float)
rotatedVector angle (x, y) =
  (x * cos angle - y * sin angle, x * sin angle + y * cos angle)

midThirdOfLine :: (Float, Float)
               -> (Float, Float)
               -> ((Float, Float), (Float, Float))
midThirdOfLine (a, b) (x, y) =
  let (dx, dy) = ((x - a) / 3, (y - b) / 3)
      f = (dx +) *** (dy +)
      p = f (a, b)
  in (p, f p)


-- TEST ---------------------------------------------------
main :: IO ()
main = putStrLn $ svgFromPoints 1024 $ kochSnowflake 4 (200, 600) (800, 600)

-- SVG ----------------------------------------------------
svgFromPoints :: Int -> [(Float, Float)] -> String
svgFromPoints w xys =
  let sw = show w
      showN = printf "%.2g"
      points =
        (unwords . fmap (((++) . showN . fst) <*> ((' ' :) . showN . snd))) xys
  in unlines
       [ "<svg xmlns=\"http://www.w3.org/2000/svg\""
       , unwords ["width=\"512\" height=\"512\" viewBox=\"5 5", sw, sw, "\"> "]
       , "<path d=\"M" ++ points ++ "\" "
       , "stroke-width=\"2\" stroke=\"red\" fill=\"transparent\"/>"
       , "</svg>"
       ]
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Koch.bas"
110 OPTION ANGLE DEGREES
120 SET 22,1:SET 23,0:SET 24,42:SET 25,26
130 OPEN #101:"video:"
140 DISPLAY #101:AT 1 FROM 1 TO 26
150 SET PALETTE 0,252
160 PLOT 300,700;ANGLE 0;
170 FOR I=1 TO 3
180   CALL KOCH(0,800)
190   PLOT RIGHT 120;
200 NEXT 
210 DEF KOCH(A,D)
220   IF D>12 THEN
230     LET D=D/3
240     CALL KOCH(A,D)
250     CALL KOCH(60,D)
260     CALL KOCH(-120,D)
270     CALL KOCH(60,D)
280   ELSE 
290     PLOT LEFT A;FORWARD D;
300   END IF 
310 END DEF
```



## JavaScript


Generates SVG. To view, save to a file with the extension '.svg', and open in a browser.

```javascript
(() => {
    'use strict';

    // kochSnowflake :: Int -> (Float, Float) -> (Float, Float)
    //                      -> [(Float, Float)]
    const kochSnowflake = n => a => b => {
        // List of points on a Koch snowflake of order n, derived
        // from an equilateral triangle with base a b.
        const points = [a, equilateralApex(a)(b), b];
        return concat(
            zipWith(kochCurve(n))(points)(
                points.slice(1).concat([points[0]])
            )
        );
    };


    // koch :: Int -> (Float, Float) -> (Float, Float)
    //             -> [(Float, Float)]
    const kochCurve = n => ab => xy => {
        // A Koch curve of order N, starting at the point
        // (a, b), and ending at the point (x, y).
        const go = n => ([ab, xy]) =>
            0 !== n ? (() => {
                const [mp, mq] = midThirdOfLine(ab)(xy);
                const points = [
                    ab,
                    mp,
                    equilateralApex(mp)(mq),
                    mq,
                    xy
                ];
                return zip(points)(points.slice(1))
                    .flatMap(go(n - 1))
            })() : [xy];
        return [ab].concat(go(n)([ab, xy]));
    };


    // equilateralApex :: (Float, Float) -> (Float, Float) -> (Float, Float)
    const equilateralApex = p => q =>
        rotatedPoint(Math.PI / 3)(p)(q);


    // rotatedPoint :: Float -> (Float, Float) ->
    //        (Float, Float) -> (Float, Float)
    const rotatedPoint = theta => ([ox, oy]) => ([a, b]) => {
        // The point ab rotated theta radians
        // around the origin xy.
        const [dx, dy] = rotatedVector(theta)(
            [a - ox, oy - b]
        );
        return [ox + dx, oy - dy];
    };


    // rotatedVector :: Float -> (Float, Float) -> (Float, Float)
    const rotatedVector = theta => ([x, y]) =>
        // The vector xy rotated by theta radians.
        [
            x * Math.cos(theta) - y * Math.sin(theta),
            x * Math.sin(theta) + y * Math.cos(theta)
        ];


    // midThirdOfLine :: (Float, Float) -> (Float, Float)
    //                 -> ((Float, Float), (Float, Float))
    const midThirdOfLine = ab => xy => {
        // Second of three equal segments of
        // the line between ab and xy.
        const
            vector = zipWith(dx => x => (dx - x) / 3)(xy)(ab),
            f = zipWith(add)(vector),
            p = f(ab);
        return [p, f(p)];
    };


    // TEST -----------------------------------------------
    // main :: IO ()
    const main = () =>
        // SVG showing a Koch snowflake of order 4.
        console.log(
            svgFromPoints(1024)(
                kochSnowflake(5)(
                    [200, 600]
                )([800, 600])
            )
        );

    // SVG ----------------------------------------------

    // svgFromPoints :: Int -> [(Int, Int)] -> String
    const svgFromPoints = w => ps => [
        '<svg xmlns="http://www.w3.org/2000/svg"',
        `width="500" height="500" viewBox="5 5 ${w} ${w}">`,
        `<path d="M${
        ps.flatMap(p => p.map(n => n.toFixed(2))).join(' ')
    }" `,
        'stroke-width="2" stroke="red" fill="transparent"/>',
        '</svg>'
    ].join('\n');


    // GENERIC --------------------------------------------

    // add :: Num -> Num -> Num
    const add = a => b => a + b;

    // concat :: [[a]] -> [a]
    const concat = xs => [].concat.apply([], xs);

    // zip :: [a] -> [b] -> [(a, b)]
    const zip = xs => ys =>
        xs.slice(
            0, Math.min(xs.length, ys.length)
        ).map((x, i) => [x, ys[i]]);


    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = f => xs => ys =>
        xs.slice(
            0, Math.min(xs.length, ys.length)
        ).map((x, i) => f(x)(ys[i]));

    // MAIN ---
    return main();
})();
```



## Julia

Multiple snowflake plots. Copied from https://www.juliabloggers.com/koch-snowflakes-for-the-holidays/.

```Julia
using Plots

function pointskoch(points, maxk, α = sqrt(3)/2)
  Q = [0 -1; 1 0]
  for k = 1:maxk
    n = length(points)
    new_points = Vector{Float64}[]
    for i = 1:n-1
      p1, p2 = points[i], points[i+1]
      v = (p2 - p1) / 3
      q1 = p1 + v
      q2 = p1 + 1.5v + α * Q * v
      q3 = q1 + v
      append!(new_points, [p1, q1, q2, q3])
    end
    push!(new_points, points[end])
    points = new_points
  end
  return points
end

function plot_koch(points; c=:red, kwargs...)
  n = length(points)
  plot(leg=false, axis=false, grid=false, background=:white)
  px = [p[1] for p in points]
  py = [p[2] for p in points]
  plot!(px, py, c=c; kwargs...)
end

function plot_koch!(points; c=:red, kwargs...)
  n = length(points)
  px = [p[1] for p in points]
  py = [p[2] for p in points]
  plot!(px, py, c=c; kwargs...)
end

function main()
  pyplot(size=(600,300))
  points = [[0.0; 0.0], [1.0; 0.0]]
  for k = 0:7
    new_points = pointskoch(points, k)
    plot_koch(new_points)
    ylims!(-0.1, 0.4)
    png("line-koch-$k")
  end

  pyplot(size=(200,200))
  for N = 2:8
    points = [[sin(2π*i/N), cos(2π*i/N)] for i = 0:N]
    plot_koch(points, c=:blue)
    points = pointskoch(points, 6)
    plot_koch!(points)
    xlims!(-1.5, 1.5)
    ylims!(-1.5, 1.5)
    png("polygon-$N")

    points = [[sin(2π*i/N), cos(2π*i/N)] for i = N:-1:0]
    plot_koch(points, c=:blue)
    points = pointskoch(points, 6)
    plot_koch!(points)
    xlims!(-1.5, 1.5)
    ylims!(-1.5, 1.5)
    png("polygon-reverse-$N")

    if N > 2
      points = [[sin(2π*i/N), cos(2π*i/N)] for i = N:-1:0]
      α = 0.85 / tan(π / N)
      α = 3 * sqrt(N) / 5
      points = pointskoch(points, 5, α)
      plot_koch(points)
      xlims!(-1.5, 1.5)
      ylims!(-1.5, 1.5)
      png("stargon-$N")
    end
  end

  # [1.5,  1.2,  0.96, 0.85
  # [√3/2, 1.2,  1.32, 1.47

  maxk = 5
  points = [[sin(2π*i/3), cos(2π*i/3)] for i = 3:-1:0]
  points = pointskoch(points, maxk)
  plot_koch(points)
  xlims!(-1.1, 1.1)
  ylims!(-0.9, 1.3)
  png("reverse-koch")

  N = 4
  points = [[cos(2π*i/N), sin(2π*i/N)] for i = 0:N]
  points = pointskoch(points, maxk, 1.25)
  plot_koch(points)
  png("star")

  points = [[0.0; 0.0], [1.0; 0.0], [1.0; 1.0], [0.0; 1.0], [0.0; 0.0]]
  points = pointskoch(points, maxk, 1.2)
  plot_koch(points)
  png("reverse-star")

  for N = 3:5
    points = [[cos(2π*i/N), sin(2π*i/N)] for i = 1:N]
    points = [i % 2 == 0 ? zeros(2) : points[div(i, 2) + 1] for i = 0:2N]
    points = pointskoch(points, 5, 1.0)
    plot_koch(points)
    xlims!(-1.2, 1.2)
    ylims!(-1.2, 1.2)
    png("tri-$N")
  end

  N = 3
  points = [[sin(2π*i/N), cos(2π*i/N)] for i = 0:N]
  points = pointskoch(points, maxk)
  plot_koch(points)
  α = 0.6
  plot_koch!(α^2 * points)
  plot_koch!(α^4 * points)
  points = [[y,x] for (x,y) in points]
  plot_koch!(α * points, c=:green)
  plot_koch!(α^3 * points, c=:green)
  png("koch")

  run(`montage tri-3.png koch.png tri-4.png tri-5.png reverse-star.png star.png -geometry +2+2 background.jpg`)
end

function large_koch()
  pyplot(size=(2000,2000))
  N = 3
  points = [[sin(2π*i/N), cos(2π*i/N)] for i = 0:N]
  points = pointskoch(points, 1)
  plot_koch(points)
  α = 3/sqrt(3)
  maxp = 11
  for p = 1:maxp
    points = α * [[-y;x] for (x,y) in points]
    if p < 7
      points = pointskoch(points, 1)
    end
    plot_koch!(points, c=p%2==1 ? :green : :red)
    xlims!(-1.1α^p, 1.1α^p)
    ylims!(-1.1α^p, 1.1α^p)
    png("koch-large-sub-$p")
  end
  xlims!(-1.1α^maxp, 1.1α^maxp)
  ylims!(-1.1α^maxp, 1.1α^maxp)
  png("koch-large")
end

function koch_julia()
  colors = [RGB(0.584, 0.345, 0.698)  RGB(0.667, 0.475, 0.757);
            RGB(0.220, 0.596, 0.149)  RGB(0.376, 0.678, 0.318);
            RGB(0.796, 0.235, 0.200)  RGB(0.835, 0.388, 0.361)]
  plot()
  plot_koch([])
  α = sqrt(3)/3
  for (i,θ) in enumerate([2π*i/3 for i = 0:2])
    points = [[sin(2π*i/3), cos(2π*i/3)] for i = 0:3]
    points = pointskoch(points, 6)
    plot_koch!([[sin(θ) + x; cos(θ) + y] for (x,y) in points], c=colors[i,1], lw=2)
    for p = 1:8
      points = α * [[-y; x] for (x,y) in points]
      plot_koch!([[sin(θ) + x; cos(θ) + y] for (x,y) in points], c=colors[i,p%2+1], lw=2)
    end
  end
  xlims!(-2.1, 2.1)
  ylims!(-1.9, 2.3)
  png("koch-julia")
end

#main()
#large_koch()
koch_julia()

```



## Kotlin

This incorporates code from other relevant tasks in order to provide a runnable example. The image produced is saved to disk where it can be viewed with a utility such as EOG.

```scala
// Version 1.2.41

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import kotlin.math.*
import java.io.File
import javax.imageio.ImageIO

val Double.asI get() = this.toInt()

class Point(var x: Int, var y: Int)

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))

    fun drawLine(x0: Int, y0: Int, x1: Int, y1: Int, c: Color) {
        val dx = abs(x1 - x0)
        val dy = abs(y1 - y0)
        val sx = if (x0 < x1) 1 else -1
        val sy = if (y0 < y1) 1 else -1
        var xx = x0
        var yy = y0
        var e1 = (if (dx > dy) dx else -dy) / 2
        var e2: Int
        while (true) {
            setPixel(xx, yy, c)
            if (xx == x1 && yy == y1) break
            e2 = e1
            if (e2 > -dx) { e1 -= dy; xx += sx }
            if (e2 <  dy) { e1 += dx; yy += sy }
        }
    }

    fun koch(x1: Double, y1: Double, x2: Double, y2: Double, it: Int) {
        val angle = PI / 3.0  // 60 degrees
        val clr = Color.blue
        var iter = it
        val x3 = (x1 * 2.0 + x2) / 3.0
        val y3 = (y1 * 2.0 + y2) / 3.0
        val x4 = (x1 + x2 * 2.0) / 3.0
        val y4 = (y1 + y2 * 2.0) / 3.0
        val x5 = x3 + (x4 - x3) * cos(angle) + (y4 - y3) * sin(angle)
        val y5 = y3 - (x4 - x3) * sin(angle) + (y4 - y3) * cos(angle)

        if (iter > 0) {
            iter--
            koch(x1, y1, x3, y3, iter)
            koch(x3, y3, x5, y5, iter)
            koch(x5, y5, x4, y4, iter)
            koch(x4, y4, x2, y2, iter)
         }
         else {
            drawLine(x1.asI, y1.asI, x3.asI, y3.asI, clr)
            drawLine(x3.asI, y3.asI, x5.asI, y5.asI, clr)
            drawLine(x5.asI, y5.asI, x4.asI, y4.asI, clr)
            drawLine(x4.asI, y4.asI, x2.asI, y2.asI, clr)
         }
    }
}

fun main(args: Array<String>) {
    val width = 512
    val height = 512
    val bbs = BasicBitmapStorage(width, height)
    with (bbs) {
        fill(Color.white)
        koch(100.0, 100.0, 400.0, 400.0, 4)
        val kFile = File("koch_curve.jpg")
        ImageIO.write(image, "jpg", kFile)
    }
}
```


```txt

Image is similar to Ring entry.

```



## Mathematica


```mathematica
Graphics[{GeometricTransformation[KochCurve[5], RotationTransform[Pi, {0.5, 0}]], 
  GeometricTransformation[KochCurve[5], RotationTransform[-Pi/3, {1, 0}]], 
  GeometricTransformation[KochCurve[5], RotationTransform[Pi/3, {0, 0}]]}]
```



## Nim

```nim

from math import sin, cos, PI
import libgd

const
  width = 512
  height = 512
  iterations = 4

proc kochCurve(img: gdImagePtr, x1, y1, x2, y2: float, iter: int): void  =
  let angle = PI / 3 # 60 degrees
  let x3 = (x1 * 2 + x2) / 3
  let y3 = (y1 * 2 + y2) / 3
  let x4 = (x1 + x2 * 2) / 3
  let y4 = (y1 + y2 * 2) / 3
  let x5 = x3 + (x4 - x3) * cos(angle) + (y4 - y3) * sin(angle)
  let y5 = y3 - (x4 - x3) * sin(angle) + (y4 - y3) * cos(angle)

  if iter > 0:
    img.kochCurve(x1, y1, x3, y3, iter - 1)
    img.kochCurve(x3, y3, x5, y5, iter - 1)
    img.kochCurve(x5, y5, x4, y4, iter - 1)
    img.kochCurve(x4, y4, x2, y2, iter - 1)
  else:
    img.drawLine(startPoint=[x1.int, y1.int], endPoint=[x3.int, y3.int])
    img.drawLine(startPoint=[x3.int, y3.int], endPoint=[x5.int, y5.int])
    img.drawLine(startPoint=[x5.int, y5.int], endPoint=[x4.int, y4.int])
    img.drawLine(startPoint=[x4.int, y4.int], endPoint=[x2.int, y2.int])

proc main() =

  withGd imageCreate(width, height) as img:
    let white = img.backgroundColor(0xffffff)
    let red = img.foregroundColor(0xff0000)

    img.kochCurve(100, 100, 400, 400, iterations)

    let png_out = open("koch_curve.png", fmWrite)
    img.writePng(png_out)
    png_out.close()

main()

```



## Perl


```perl
use SVG;
use List::Util qw(max min);

use constant pi => 2 * atan2(1, 0);

# Compute the curve with a Lindemayer-system
my $koch = 'F--F--F';
$koch =~ s/F/F+F--F+F/g for 1..5;

# Draw the curve in SVG
($x, $y) = (0, 0);
$theta   = pi/3;
$r       = 2;

for (split //, $koch) {
    if (/F/) {
        push @X, sprintf "%.0f", $x;
        push @Y, sprintf "%.0f", $y;
        $x += $r * cos($theta);
        $y += $r * sin($theta);
    }
    elsif (/\+/) { $theta += pi/3; }
    elsif (/\-/) { $theta -= pi/3; }
}

$xrng =  max(@X) - min(@X);
$yrng =  max(@Y) - min(@Y);
$xt   = -min(@X)+10;
$yt   = -min(@Y)+10;
$svg = SVG->new(width=>$xrng+20, height=>$yrng+20);
$points = $svg->get_path(x=>\@X, y=>\@Y, -type=>'polyline');
$svg->rect(width=>"100%", height=>"100%", style=>{'fill'=>'black'});
$svg->polyline(%$points, style=>{'stroke'=>'orange', 'stroke-width'=>1}, transform=>"translate($xt,$yt)");

open  $fh, '>', 'koch_curve.svg';
print $fh  $svg->xmlify(-namespace=>'svg');
close $fh;
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/koch_curve.svg Koch curve] (offsite image)


## Perl 6

Koch curve, actually a full Koch snowflake.

```perl6
use SVG;

role Lindenmayer {
    has %.rules;
    method succ {
        self.comb.map( { %!rules{$^c} // $c } ).join but Lindenmayer(%!rules)
    }
}

my $flake = 'F--F--F' but Lindenmayer( { F => 'F+F--F+F' } );

$flake++ xx 5;
my @points = (50, 440);

for $flake.comb -> $v {
    state ($x, $y) = @points[0,1];
    state $d = 2 + 0i;
    with $v {
        when 'F' { @points.append: ($x += $d.re).round(.01), ($y += $d.im).round(.01) }
        when '+' { $d *= .5 + .8660254i }
        when '-' { $d *= .5 - .8660254i }
    }
}

say SVG.serialize(
    svg => [
        width => 600, height => 600, style => 'stroke:rgb(0,0,255)',
        :rect[:width<100%>, :height<100%>, :fill<white>],
        :polyline[ points => @points.join(','), :fill<white> ],
    ],
);
```


See: [https://github.com/thundergnat/rc/blob/master/img/koch1.svg Koch snowflake]

Variation using 90° angles:

```perl6
use SVG;

role Lindenmayer {
    has %.rules;
    method succ {
	    self.comb.map( { %!rules{$^c} // $c } ).join but Lindenmayer(%!rules)
    }
}

my $koch = 'F' but Lindenmayer( { F => 'F+F-F-F+F', } );

$koch++ xx 4;
my @points = (450, 250);

for $koch.comb -> $v {
    state ($x, $y) = @points[0,1];
    state $d = -5 - 0i;
    with $v {
        when 'F' { @points.append: ($x += $d.re).round(.01), ($y += $d.im).round(.01) }
        when /< + - >/ { $d *= "{$v}1i" }
    }
}

say SVG.serialize(
    svg => [
        width => 500, height => 300, style => 'stroke:rgb(0,0,255)',
        :rect[:width<100%>, :height<100%>, :fill<white>],
        :polyline[ points => @points.join(','), :fill<white> ],
    ],
);
```

See: [https://github.com/thundergnat/rc/blob/master/img/koch2.svg Koch curve variant with 90° angles]


## Phix

```Phix
-- demo\rosetta\Koch_curve.exw
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

procedure koch(atom x1, y1, x2, y2, integer iter)
    atom angle = -PI/3, -- -60 degrees
         x3 := (x1*2 + x2) / 3,
         y3 := (y1*2 + y2) / 3,
         x4 := (x1 + x2*2) / 3,
         y4 := (y1 + y2*2) / 3,
         x5 := x3 + (x4-x3)*cos(angle) + (y4-y3)*sin(angle),
         y5 := y3 - (x4-x3)*sin(angle) + (y4-y3)*cos(angle)
    if iter>0  then
        iter -= 1
        koch(x1, y1, x3, y3, iter)
        koch(x3, y3, x5, y5, iter)
        koch(x5, y5, x4, y4, iter)
        koch(x4, y4, x2, y2, iter)
    else
        cdCanvasVertex(cddbuffer, x1, y1)
        cdCanvasVertex(cddbuffer, x3, y3)
        cdCanvasVertex(cddbuffer, x5, y5)
        cdCanvasVertex(cddbuffer, x4, y4)
        cdCanvasVertex(cddbuffer, x2, y2)
    end if
end procedure
 
function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    cdCanvasBegin(cddbuffer, CD_OPEN_LINES)  
    koch(0,0,width,height,4)    
    cdCanvasEnd(cddbuffer)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_BLUE)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
    
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "512x512") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Koch curve")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```

The distributed version also contains some almost-but-not-quite complete code to draw a full snowflake.


## Python


### Functional

Generates SVG for a Koch snowflake. To view, save as a text file with the extension .svg, and open in a browser.

```python
'''Koch curve'''

from math import cos, pi, sin
from operator import add, sub
from itertools import chain


# kochSnowflake :: Int -> (Float, Float) -> (Float, Float) -> [(Float, Float)]
def kochSnowflake(n, a, b):
    '''List of points on a Koch snowflake of order n, derived
       from an equilateral triangle with base a b.
    '''
    points = [a, equilateralApex(a, b), b]
    return chain.from_iterable(
        map(kochCurve(n), points, points[1:] + [points[0]])
    )


# kochCurve :: Int -> (Float, Float) -> (Float, Float)
#                               -> [(Float, Float)]
def kochCurve(n):
    '''List of points on a Koch curve of order n,
       starting at point ab, and ending at point xy.
    '''
    def koch(n, abxy):
        (ab, xy) = abxy
        if 0 == n:
            return [xy]
        else:
            (mp, mq) = midThirdOfLine(ab, xy)
            points = [
                ab,
                mp,
                equilateralApex(mp, mq),
                mq,
                xy
            ]
            return concatMap(curry(koch)(n - 1))(
                zip(points, points[1:])
            )
    return lambda ab, xy: [ab] + koch(n, (ab, xy))


# equilateralApex :: (Float, Float) -> (Float, Float) -> (Float, Float)
def equilateralApex(p, q):
    '''Apex of triangle with base p q.'''
    return rotatedPoint(pi / 3)(p, q)


# rotatedPoint :: Float -> (Float, Float) ->
#                (Float, Float) -> (Float, Float)
def rotatedPoint(theta):
    '''The point ab rotated theta radians
        around the origin xy.
    '''
    def go(xy, ab):
        (ox, oy) = xy
        (a, b) = ab
        (dx, dy) = rotatedVector(theta, (a - ox, oy - b))
        return (ox + dx, oy - dy)
    return lambda xy, ab: go(xy, ab)


# rotatedVector :: Float -> (Float, Float) -> (Float, Float)
def rotatedVector(theta, xy):
    '''The vector xy rotated by theta radians.'''
    (x, y) = xy
    return (
        x * cos(theta) - y * sin(theta),
        x * sin(theta) + y * cos(theta)
    )


# midThirdOfLine :: (Float, Float) -> (Float, Float)
#                -> ((Float, Float), (Float, Float))
def midThirdOfLine(ab, xy):
    '''Second of three equal segments of
       the line between ab and xy.
    '''
    vector = [x / 3 for x in map(sub, xy, ab)]

    def f(p):
        return tuple(map(add, vector, p))
    p = f(ab)
    return (p, f(p))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''SVG for Koch snowflake of order 4.'''
    print(
        svgFromPoints(1024)(
            kochSnowflake(
                4, (200, 600), (800, 600)
            )
        )
    )


# SVG -----------------------------------------------------

# svgFromPoints :: Int -> [(Float, Float)] -> SVG String
def svgFromPoints(w):
    '''Width of square canvas -> Point list -> SVG string'''

    def go(w, xys):
        xs = ' '.join(map(
            lambda xy: str(round(xy[0], 2)) + ' ' + str(round(xy[1], 2)),
            xys
        ))
        return '\n'.join(
            ['<svg xmlns="http://www.w3.org/2000/svg"',
             f'width="512" height="512" viewBox="5 5 {w} {w}">',
             f'<path d="M{xs}" ',
             'stroke-width="2" stroke="red" fill="transparent"/>',
             '</svg>'
             ]
        )
    return lambda xys: go(w, xys)


# GENERIC -------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list or string over which a function
       has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).
    '''
    return lambda xs: (''.join if isinstance(xs, str) else list)(
        chain.from_iterable(map(f, xs))
    )


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from a function over a tuple.
    '''
    return lambda x: lambda y: f(x, y)


# MAIN ---
if __name__ == '__main__':
    main()
```



## QBasic


```qbasic
    ' Chaos:  start at any point,  this program uses the middle of the screen  (or universe.  One of six
    ' degrees of freedom (a direction)  is chosen at random  (by throwing a six-sided die),  and a line
    ' is drawn from the old point to the new point in the direction indicated by the pip on the die.
    '
    ' The traverse distance is always a fraction of the last distance drawn;  the fraction (here) uses:
    '
    '                                       +-                                -+
    '                                       |                                  |
    '       distance  <===  old_distance *  |  1/2 - 1/8 - 1/32 - 1/128 - ...  |
    '                                       |                                  |
    '                                       +-                                -+
    '            ---or---
    '                                       +-                                -+
    '                                       |   1     1     1      1           |
    '       distance  <===  old_distance *  |  --- - --- - ---- - ----- - ...  |
    '                                       |  2**1  2**3  2**5   2**7         |
    '                                       +-                                -+
    '
    ' (The series above has a limit of  1/3.)
    '
    ' The six degrees of freedom:                      1         6
    '
    '                                                    \     /
    '                                                     \   /
    '                                                      \ /
    '                                            2  <------ X ------>  5
    '                                                      / \
    '                                                     /   \
    '                                                    /     \
    '
    '                                                  3         4
    '
    ' When the amount to be moved is too small to show on the terminal screen,  the chaos curve is
    ' starting again  (from the initial point,  the middle of the screen/universe).
    '
    ' All subsequent chaos curves are superimposed on the first curve.
    '
    ' The envelope of this chaos curve is defined as the snowflake curve.
    '
    ' If any cursor key (one of the "arrow" keys) is pressed,  program execution is halted.
    '
    ' If any function key is pressed during execution, the random chaos curve is stopped, the screen
    ' cleared, and the snowflake curve is drawn by a non-random method (brute force).
    '
    ' Once the random snowflake (chaos) curve is being drawn, the pressing of function keys 1-->9 will
    ' force the randomness to move in a particular direction, the direction (the degree of freedom) is
    ' the direction indicated by the number of times that function key is pressed for that curve point.
    ' That is, function key 1 is used for the first point (part of the chaos curve),  function key 2 is
    ' used for the second point, function key 3 for the third point,  etc.

    DEFINT A-Y                    ' define variables that begin with   A-->Y   as integers.
    DEFSNG Z                      ' define variables that begin with     Z     as single precision.
    DIM XP(16,6),YP(16,6),KY(16)  ' define some (integer) arrays.
    MP= 16                        ' set the maximum number of points (1st dimension) that can be plotted.
    CLS                           ' clear the screen for visual fidelity.
    SCREEN 2                      ' make the screen high-res graphics.
    GOTO 230                      ' branch around a  RETURN  statement that  ON KEY(i) uses.

220 RETURN
230 FK= 0                         ' set FK  (used to indicate that a function key was pressed).

      FOR I=1  TO 10              ' allow the use of function keys to stop the deliberate snowflake
                                  ' curve and start drawing it randomly.
      KY(I)= 0
      ON KEY(I)  GOSUB 220        ' allow the trapping of function keys,  but don't process it as yet.
      KEY(I)  ON
      KEY(I) STOP
      NEXT I

    CLS                           ' clear the screen for visual fidelity.
    ZZ= 2 + TIMER                 ' on some PCs, a pause of at least one second prevents scrolling.

240 IF TIMER<ZZ  THEN GOTO 240
    RANDOMIZE TIMER               ' randomize the RND function from the timer.
    XM= 640 - 1                   ' define the number of points on the screen (for plotting).
    YM= 200 - 1
    XO= XM \ 2                    ' define the origin of the chaos curve.
    YO= YM \ 2
    ZT= 1 / 3                     ' define the traverse distance,  it's this distance that each part of
                                  ' the chaos curve "breaks",  when the distance that the next part of
                                  ' the chaos curve is moved to.
    ZA= 1                         ' define the aspect ratio for the terminal screen.
    ZX= XM * ZA                   ' define the initial distance to be plotted (for a line).
    ZY= YM                        '    "    "     "        "     "  "    "      "  "   "
           FOR I=1  TO MP         ' compute (once) all the  x & y  distances for each part of the curve.
           ZX= ZX * ZT * ZA
           ZY= ZY * ZT
           XP(I, 1) = -ZX / 2
           XP(I, 2) = -ZX
           XP(I, 3) = -ZX / 2
           XP(I, 4) =  ZX / 2
           XP(I, 5) =  ZX
           XP(I, 6) =  ZX / 2
           YP(I, 1) = -ZY
           YP(I, 2) =   0
           YP(I, 3) =  ZY
           YP(I, 4) =  ZY
           YP(I, 5) =   0
           YP(I, 6) = -ZY
           NEXT I
    N0=0
           FOR II=1  TO MP        ' find the maximum number of points that can be plotted.
             FOR I=1  TO 6
             IF XP(II, I) <> 0  THEN N0= II
             IF YP(II, I) <> 0  THEN N0= II
             NEXT I
           NEXT II

           FOR I=11  TO 14        ' quit if any cursor key is pressed.
           ON KEY(I)  GOSUB 598
           KEY(I)  ON
           NEXT I

           FOR I=1  TO 10         ' If any function key is pressed during execution, the deliberate
           ON KEY(I)  GOSUB 400   ' curve is stopped,  the screen is cleared, and the snowflake curve is
           KEY(I)  ON             ' drawn by a random process  (AKA,  the chaos curve).
           NEXT I

    GOTO 500

400 FK= 1                         ' come here when any function or cursor key is pressed,  and set  FK
                                  ' that is checked by the deliberate snowflake curve generator.
    RETURN

500 CLS                           ' clear the screen before starting  (for visual fidelity).
                  FOR I1=1  TO 6  ' plot the curve via non-random (deliberate calculation) points.
                  X1= XO  +  XP(1, I1)
                  Y1= YO  +  YP(1, I1)
                  IF FK  THEN GOTO 600
                  LINE (XO, YO)  -  (X1, Y1)
                    FOR I2=1  TO 6
                    X2= X1  +  XP(2,I2)
                    Y2= Y1  +  YP(2,I2)
                    IF FK  THEN GOTO 600
                    LINE (X1, Y1)  -  (X2, Y2)
                      FOR I3=1  TO 6
                      X3= X2  +  XP(3, I3)
                      Y3= Y2  +  YP(3, I3)
                      IF FK  THEN GOTO 600
                      LINE (X2, Y2)  -  (X3, Y3)
                        FOR I4=1  TO 6
                        X4= X3  +  XP(4, I4)
                        Y4= Y3  +  YP(4, I4)
                        IF FK  THEN GOTO 600
                        LINE (X3, Y3)  -  (X4, Y4)
                          FOR I5=1  TO 6
                          X5= X4  +  XP(5, I5)
                          Y5= Y4  +  YP(5, I5)
                          IF FK  THEN GOTO 600
                          LINE (X4, Y4)  -  (X5, Y5)
                          NEXT I5
                        NEXT I4
                      NEXT I3
                    NEXT I2
                  NEXT I1
    ZZ= 10+TIMER                  ' The snowflake curve is now complete.

555 IF TIMER<ZZ  THEN GOTO 555    ' loop for ten seconds.
598 SYSTEM                        ' stick a fork in it, we're all done.

600 ON  KEY(1)   GOSUB 710        ' trap all function keys for toggling.
    ON  KEY(2)   GOSUB 720
    ON  KEY(3)   GOSUB 730
    ON  KEY(4)   GOSUB 740
    ON  KEY(5)   GOSUB 750
    ON  KEY(6)   GOSUB 760
    ON  KEY(7)   GOSUB 770
    ON  KEY(8)   GOSUB 780
    ON  KEY(9)   GOSUB 790
    ON  KEY(10)  GOSUB 700

              FOR I=1  TO MP      ' re-active trapping all the function keys.
              KEY(I)  ON
              NEXT I
    CLS                           ' clear the screen before starting.
    GOTO 900                      ' go and start drawing the chaos curve.

700           FOR I0=1  TO MP     ' reset all toggle settings for all points.
              KY(I0)= 0
              NEXT I0
    RETURN

710 KI= 1                         ' toggle setting for point #1 (bypass).
              GOTO 800
720 KI= 2                         ' toggle setting for point #2 (bypass).
              GOTO 800
730 KI= 3                         ' toggle setting for point #3 (bypass).
              GOTO 800
740 KI= 4                         ' toggle setting for point #4 (bypass).
              GOTO 800
750 KI= 5                         ' toggle setting for point #5 (bypass).
              GOTO 800
760 KI= 6                         ' toggle setting for point #6 (bypass).
              GOTO 800
770 KI= 7                         ' toggle setting for point #7 (bypass).
              GOTO 800
780 KI= 8                         ' toggle setting for point #8 (bypass).
              GOTO 800
790 KI= 9                         ' toggle setting for point #9 (bypass).

800 KY(KI)= (1 + KY(KI) )  MOD 7  ' reset toggle settings for all higher points.

              FOR IK=KI+1  TO MP
              KY(IK)= 0
              NEXT IK
    RETURN

900 N= 0                          ' initialize the number of points in this particular chaos curve.
    X= XO                         ' move the start-of-the-chaos-curve to the origin.
    Y= YO
    LINE (X, Y)  -  (X, Y)
    N= N + 1                      ' bump number of points drawn so far.
    IF N>N0   THEN GOTO 900       ' # points drawn exceeds possible?  Start another chaos curve.
                                  ' start of diminishing loop to create an envelope for the chaos curve.
    IF KY(N)  THEN R= KY(N)   ELSE R= 1 + INT(RND*6)
    X= X  +  XP(N, R)             ' exercise a degree of freedom (one of six).
    Y= Y  +  YP(N, R)
    LINE -(X, Y)                  ' depending on the "die",  draw the next part of the chaos curve.
                   GOTO 900       ' now, go and do another point.
```





## Racket


```racket
#lang racket

(require metapict)

; rot: rotate d degrees around point p, where c is a point or curve
(def (rot d p c)
  (rotated-aboutd d p c))

(define (koch a b n)
  (match n
    [0 (draw (curve a -- b))]
    [_ (def 1/3ab (med 1/3 a b))
       (def 2/3ab (med 2/3 a b))
       (draw (koch a 1/3ab                      (- n 1))
             (koch 1/3ab (rot 60 1/3ab 2/3ab)   (- n 1))
             (koch (rot 60 1/3ab 2/3ab) 2/3ab   (- n 1))
             (koch 2/3ab b                      (- n 1)))]))

(define (snow n)
  (def a (pt 0 0))
  (def b (pt 1 0))
  (def c (rot 60 a b))
  (draw (koch b a n)
        (koch c b n)
        (koch a c n)))

(scale 4 (snow 2))
```



## Ring


```ring

# Project : Koch curve

load "guilib.ring"

paint = null

new qapp 
        {
        win1 = new qwidget() {
                  setwindowtitle("Koch curve")
                  setgeometry(100,100,500,600)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,400,400)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(150,500,100,30)
                          settext("draw")
                          setclickevent("draw()")
                  }
                  show()
        }
        exec()
        }

func draw
        p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
        }
        pen = new qpen() {
                 setcolor(color)
                 setwidth(1)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

        koch(100, 100, 400, 400, 4)

        endpaint()
        }
        label1 { setpicture(p1) show() }

func koch x1, y1, x2, y2, it

        angle = 60*3.14/180
        x3 = (2*x1+x2)/3
        y3 = (2*y1+y2)/3

        x4 = (x1+2*x2)/3
        y4 = (y1+2*y2)/3

        x = x3 + (x4-x3)*cos(angle)+(y4-y3)*sin(angle)
        y = y3 - (x4-x3)*sin(angle)+(y4-y3)*cos(angle)

       if (it > 0)
          koch(x1, y1, x3, y3, it-1)
          koch(x3, y3, x, y, it-1)
          koch(x, y, x4, y4, it-1)
          koch(x4, y4, x2, y2, it-1)
       else
          paint.drawline(x1, y1, x3, y3)
          paint.drawline(x3, y3, x, y)
          paint.drawline(x, y, x4, y4)
          paint.drawline(x4, y4, x2, y2)
      ok

```

Output image:

[https://www.dropbox.com/s/33f46wigyc8xt3z/KochCurve.jpg?dl=0 Koch curve]


## Sidef

Using the LSystem class defined at [https://rosettacode.org/wiki/Hilbert_curve#Sidef Hilbert curve].

```ruby
var rules = Hash(
    F => 'F+F--F+F',
)

var lsys = LSystem(
    width:  800,
    height: 800,

    xoff: -210,
    yoff: -90,

    len:   8,
    angle: 60,
    color: 'dark green',
)

lsys.execute('F--F--F', 4, "koch_snowflake.png", rules)
```


Output image: [https://github.com/trizen/rc/blob/master/img/koch-snowflake-sidef.png Koch snowflake]


## zkl

Uses Image Magick and
the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
var width=512, height=512, img=PPM(width,height,0xFfffFF);   // white canvas
var angle=(60.0).toRad();
const green=0x00FF00;

fcn koch(x1,y1, x2,y2, it){
   x3,y3 := (x1*2 + x2)  /3, (y1*2 + y2)  /3;
   x4,y4 := (x1   + x2*2)/3, (y1   + y2*2)/3;
   x:=x3 + (x4-x3)*angle.cos() + (y4-y3)*angle.sin();
   y:=y3 - (x4-x3)*angle.sin() + (y4-y3)*angle.cos();
 
   if(it>0){
      it-=1;
      koch(x1,y1, x3,y3, it);
      koch(x3,y3, x, y,  it);
      koch(x, y,  x4,y4, it);
      koch(x4,y4, x2,y2, it);
   }else{
      x,y, x1,y1, x2,y2, x3,y3, x4,y4 = 
         T(x,y, x1,y1, x2,y2, x3,y3, x4,y4).apply("toInt");
      img.line(x1,y1, x3,y3, green);
      img.line(x3,y3, x, y,  green);
      img.line(x, y,  x4,y4, green);
      img.line(x4,y4, x2,y2, green);
   }
}

koch(100.0,100.0, 400.0,400.0, 4);
img.writeJPGFile("koch.zkl.jpg");
```

Image at [http://www.zenkinetic.com/Images/RosettaCode/koch.zkl.jpg koch curve]


Using a Lindenmayer system and turtle graphics to draw a Koch snowflake:


```zkl
lsystem("F--F--F", Dictionary("F","F+F--F+F"), "+-", 4)  // snowflake
//lsystem("F", Dictionary("F","F+F--F+F"), "+-", 3)	 // curve
: turtle(_);

fcn lsystem(axiom,rules,consts,n){	// Lindenmayer system --> string
   foreach k in (consts){ rules.add(k,k) }
   buf1,buf2 := Data(Void,axiom).howza(3), Data().howza(3);  // characters
   do(n){
      buf1.pump(buf2.clear(), rules.get);
      t:=buf1; buf1=buf2; buf2=t;	// swap buffers
   }
   buf1.text		// n=4 snow flake --> 1,792 characters
}

fcn turtle(koch){
   const D=10.0;
   dir,deg60, x,y := 0.0, (60.0).toRad(), 20.0, 710.0; // turtle; x,y are float
   img,color := PPM(850,950), 0x00ff00;
   foreach c in (koch){
      switch(c){
	 case("F"){   // draw forward
	    dx,dy := D.toRectangular(dir);
	    tx,ty := x,y; x,y = (x+dx),(y+dy);
	    img.line(tx.toInt(),ty.toInt(), x.toInt(),y.toInt(), color);
	 }
	 case("-"){ dir-=deg60 } // turn right 60*
	 case("+"){ dir+=deg60 } // turn left  60*
      }
   }
   img.writeJPGFile("kochSnowFlake.zkl.jpg");
}
```

Image at [http://www.zenkinetic.com/Images/RosettaCode/kochSnowFlake.zkl.jpg Koch snow flake]

+++
title = "Sierpinski pentagon"
description = ""
date = 2019-05-19T16:21:28Z
aliases = []
[extra]
id = 19668
[taxonomies]
categories = ["task"]
tags = []
+++

Produce a graphical or ASCII-art representation of a [[wp:N-flake#Pentaflake|Sierpinski pentagon]] (aka a Pentaflake) of order 5. Your code should also be able to correctly generate representations of lower orders: 1 to 4.




## See also

* [http://ecademy.agnesscott.edu/~lriddle/ifs/pentagon/pentagon.htm Sierpinski pentagon]





## C

The Sierpinski fractals can be generated via the [http://mathworld.wolfram.com/ChaosGame.html Chaos Game]. This implementation thus generalizes the [[Chaos game]] C implementation on Rosettacode. As the number of sides increases, the number of iterations must increase dramatically for a well pronounced fractal ( 30000 for a pentagon). This is in keeping with the requirements that the implementation should work for polygons with sides 1 to 4 as well. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>
#include<stdlib.h>
#include<stdio.h>
#include<math.h>
#include<time.h>

#define pi M_PI

int main(){

	time_t t;
	double side, **vertices,seedX,seedY,windowSide = 500,sumX=0,sumY=0;
	int i,iter,choice,numSides;

	printf("Enter number of sides : ");
	scanf("%d",&numSides);

	printf("Enter polygon side length : ");
	scanf("%lf",&side);

	printf("Enter number of iterations : ");
	scanf("%d",&iter);

	initwindow(windowSide,windowSide,"Polygon Chaos");

	vertices = (double**)malloc(numSides*sizeof(double*));

	for(i=0;i<numSides;i++){
		vertices[i] = (double*)malloc(2 * sizeof(double));

		vertices[i][0] = windowSide/2 + side*cos(i*2*pi/numSides);
		vertices[i][1] = windowSide/2 + side*sin(i*2*pi/numSides);
		sumX+= vertices[i][0];
		sumY+= vertices[i][1];
		putpixel(vertices[i][0],vertices[i][1],15);
	}

	srand((unsigned)time(&t));

	seedX = sumX/numSides;
	seedY = sumY/numSides;

	putpixel(seedX,seedY,15);

	for(i=0;i<iter;i++){
		choice = rand()%numSides;

		seedX = (seedX + (numSides-2)*vertices[choice][0])/(numSides-1);
		seedY = (seedY + (numSides-2)*vertices[choice][1])/(numSides-1);

		putpixel(seedX,seedY,15);
	}

	free(vertices);

	getch();

	closegraph();

	return 0;
}

```



## C++

```cpp
#include <iomanip>
#include <iostream>

#define _USE_MATH_DEFINES
#include <math.h>

constexpr double degrees(double deg) {
    const double tau = 2.0 * M_PI;
    return deg * tau / 360.0;
}

const double part_ratio = 2.0 * cos(degrees(72));
const double side_ratio = 1.0 / (part_ratio + 2.0);

/// Define a position
struct Point {
    double x, y;

    friend std::ostream& operator<<(std::ostream& os, const Point& p);
};

std::ostream& operator<<(std::ostream& os, const Point& p) {
    auto f(std::cout.flags());
    os << std::setprecision(3) << std::fixed << p.x << ',' << p.y << ' ';
    std::cout.flags(f);
    return os;
}

/// Mock turtle implementation sufficiant to handle "drawing" the pentagons
struct Turtle {
private:
    Point pos;
    double theta;
    bool tracing;

public:
    Turtle() : theta(0.0), tracing(false) {
        pos.x = 0.0;
        pos.y = 0.0;
    }

    Turtle(double x, double y) : theta(0.0), tracing(false) {
        pos.x = x;
        pos.y = y;
    }

    Point position() {
        return pos;
    }
    void position(const Point& p) {
        pos = p;
    }

    double heading() {
        return theta;
    }
    void heading(double angle) {
        theta = angle;
    }

    /// Move the turtle through space
    void forward(double dist) {
        auto dx = dist * cos(theta);
        auto dy = dist * sin(theta);

        pos.x += dx;
        pos.y += dy;

        if (tracing) {
            std::cout << pos;
        }
    }

    /// Turn the turtle
    void right(double angle) {
        theta -= angle;
    }

    /// Start/Stop exporting the points of the polygon
    void begin_fill() {
        if (!tracing) {
            std::cout << "<polygon points=\"";
            tracing = true;
        }
    }
    void end_fill() {
        if (tracing) {
            std::cout << "\"/>\n";
            tracing = false;
        }
    }
};

/// Use the provided turtle to draw a pentagon of the specified size
void pentagon(Turtle& turtle, double size) {
    turtle.right(degrees(36));
    turtle.begin_fill();
    for (size_t i = 0; i < 5; i++) {
        turtle.forward(size);
        turtle.right(degrees(72));
    }
    turtle.end_fill();
}

/// Draw a sierpinski pentagon of the desired order
void sierpinski(int order, Turtle& turtle, double size) {
    turtle.heading(0.0);
    auto new_size = size * side_ratio;

    if (order-- > 1) {
        // create four more turtles
        for (size_t j = 0; j < 4; j++) {
            turtle.right(degrees(36));

            double small = size * side_ratio / part_ratio;
            auto distList = { small, size, size, small };
            auto dist = *(distList.begin() + j);

            Turtle spawn{ turtle.position().x, turtle.position().y };
            spawn.heading(turtle.heading());
            spawn.forward(dist);

            // recurse for each spawned turtle
            sierpinski(order, spawn, new_size);
        }

        // recurse for the original turtle
        sierpinski(order, turtle, new_size);
    } else {
        // The bottom has been reached for this turtle
        pentagon(turtle, size);
    }
    if (order > 0) {
        std::cout << '\n';
    }
}

/// Run the generation of a P(5) sierpinksi pentagon
int main() {
    const int order = 5;
    double size = 500;

    Turtle turtle{ size / 2.0, size };

    std::cout << "<?xml version=\"1.0\" standalone=\"no\"?>\n";
    std::cout << "<!DOCTYPE svg PUBLIC \" -//W3C//DTD SVG 1.1//EN\"\n";
    std::cout << "    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
    std::cout << "<svg height=\"" << size << "\" width=\"" << size << "\" style=\"fill:blue\" transform=\"translate(" << size / 2 << ", " << size / 2 << ") rotate(-36)\"\n";
    std::cout << "    version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n";

    size *= part_ratio;
    sierpinski(order, turtle, size);

    std::cout << "</svg>";
}
```



## D

This solution combines the turtle graphics concept used in Python, with the SVG output format of the Perl 6 solution.
This runs very quickly compared to the Python version.


```D
import std.math;
import std.stdio;

/// Convert degrees into radians, as that is the accepted unit for sin/cos etc...
real degrees(real deg) {
    immutable tau = 2.0 * PI;
    return deg * tau / 360.0;
}

immutable part_ratio = 2.0 * cos(72.degrees);
immutable side_ratio = 1.0 / (part_ratio + 2.0);

/// Use the provided turtle to draw a pentagon of the specified size
void pentagon(Turtle turtle, real size) {
    turtle.right(36.degrees);
    turtle.begin_fill();
    foreach(i; 0..5) {
        turtle.forward(size);
        turtle.right(72.degrees);
    }
    turtle.end_fill();
}

/// Draw a sierpinski pentagon of the desired order
void sierpinski(int order, Turtle turtle, real size) {
    turtle.setheading(0.0);
    auto new_size = size * side_ratio;

    if (order-- > 1) {
        // create four more turtles
        foreach(j; 0..4) {
            turtle.right(36.degrees);
            real small = size * side_ratio / part_ratio;
            auto dist = [small, size, size, small][j];

            auto spawn = new Turtle();
            spawn.setposition(turtle.position);
            spawn.setheading(turtle.heading);
            spawn.forward(dist);

            // recurse for each spawned turtle
            sierpinski(order, spawn, new_size);
        }

        // recurse for the original turtle
        sierpinski(order, turtle, new_size);
    } else {
        // The bottom has been reached for this turtle
        pentagon(turtle, size);
    }
}

/// Run the generation of a P(5) sierpinksi pentagon
void main() {
    int order = 5;
    real size = 500;

    auto turtle = new Turtle(size/2, size);

    // Write the header to an SVG file for the image
    writeln(`<?xml version="1.0" standalone="no"?>`);
    writeln(`<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"`);
    writeln(`    "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">`);
    writefln(`<svg height="%s" width="%s" style="fill:blue" transform="translate(%s,%s) rotate(-36)"`, size, size, size/2, size/2);
    writeln(`    version="1.1" xmlns="http://www.w3.org/2000/svg">`);
    // Write the close tag when the interior points have been written
    scope(success) writeln("</svg>");

    // Scale the initial turtle so that it stays in the inner pentagon
    size *= part_ratio;

    // Begin rendering
    sierpinski(order, turtle, size);
}

/// Define a position
struct Point {
    real x;
    real y;

    /// When a point is written, do it in the form "x,y " to three decimal places
    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;

        formattedWrite(sink, "%0.3f", x);
        sink(",");
        formattedWrite(sink, "%0.3f", y);
        sink(" ");
    }
}

/// Mock turtle implementation sufficiant to handle "drawing" the pentagons
class Turtle {
    /////////////////////////////////
    private:

    Point pos;
    real theta;
    bool tracing;

    /////////////////////////////////
    public:
    this() {
        // empty
    }

    this(real x, real y) {
        pos.x = x;
        pos.y = y;
    }

    // Get/Set the turtle position
    Point position() {
        return pos;
    }
    void setposition(Point pos) {
        this.pos = pos;
    }

    // Get/Set the turtle's heading
    real heading() {
        return theta;
    }
    void setheading(real angle) {
        theta = angle;
    }

    // Move the turtle through space
    void forward(real dist) {
        // Calculate both components at once for the specified angle
        auto delta = dist * expi(theta);

        pos.x += delta.re;
        pos.y += delta.im;

        if (tracing) {
            write(pos);
        }
    }

    // Turn the turle
    void right(real angle) {
        theta = theta - angle;
    }

    // Start/Stop exporting the points of the polygon
    void begin_fill() {
        write(`<polygon points="`);
        tracing = true;
    }
    void end_fill() {
        writeln(`"/>`);
        tracing = false;
    }
}
```



## Go

This follows the approach of the Java entry but uses a fixed palette of 5 colors which are selected in order rather than randomly.

As output is to an external .png file, only a pentaflake of order 5 is drawn though pentaflakes of lower orders can still be drawn by setting the 'order' variable to the appropriate figure.

```go
package main

import (
    "github.com/fogleman/gg"
    "image/color"
    "math"
)

var (
    red     = color.RGBA{255, 0, 0, 255}
    green   = color.RGBA{0, 255, 0, 255}
    blue    = color.RGBA{0, 0, 255, 255}
    magenta = color.RGBA{255, 0, 255, 255}
    cyan    = color.RGBA{0, 255, 255, 255}
)

var (
    w, h        = 640, 640
    dc          = gg.NewContext(w, h)
    deg72       = gg.Radians(72)
    scaleFactor = 1 / (2 + math.Cos(deg72)*2)
    palette     = [5]color.Color{red, green, blue, magenta, cyan}
    colorIndex  = 0
)

func drawPentagon(x, y, side float64, depth int) {
    angle := 3 * deg72
    if depth == 0 {
        dc.MoveTo(x, y)
        for i := 0; i < 5; i++ {
            x += math.Cos(angle) * side
            y -= math.Sin(angle) * side
            dc.LineTo(x, y)
            angle += deg72
        }
        dc.SetColor(palette[colorIndex])
        dc.Fill()
        colorIndex = (colorIndex + 1) % 5
    } else {
        side *= scaleFactor
        dist := side * (1 + math.Cos(deg72)*2)
        for i := 0; i < 5; i++ {
            x += math.Cos(angle) * dist
            y -= math.Sin(angle) * dist
            drawPentagon(x, y, side, depth-1)
            angle += deg72
        }
    }
}

func main() {
    dc.SetRGB(1, 1, 1) // White background
    dc.Clear()
    order := 5 // Can also set this to 1, 2, 3 or 4
    hw := float64(w / 2)
    margin := 20.0
    radius := hw - 2*margin
    side := radius * math.Sin(math.Pi/5) * 2
    drawPentagon(hw, 3*margin, side, order-1)
    dc.SavePNG("sierpinski_pentagon.png")
}
```


```txt

Image similar to Java entry but uses a fixed palette of colors.

```



## Haskell

For universal solution see [[Fractal tree#Haskell]]


```haskell
import Graphics.Gloss

pentaflake :: Int -> Picture
pentaflake order = iterate transformation pentagon !! order
  where
    transformation = Scale s s . foldMap copy [0,72..288]
    copy a = Rotate a . Translate 0 x
    pentagon = Polygon [ (sin a, cos a) | a <- [0,2*pi/5..2*pi] ]
    x = 2*cos(pi/5)
    s = 1/(1+x)

main = display dc white (Color blue $ Scale 300 300 $ pentaflake 5)
  where dc = InWindow "Pentaflake" (400, 400) (0, 0)
```


'''Explanation''': Since <tt>Picture</tt> forms a monoid with image overlaying as multiplication, so do functions having type <tt>Picture -> Picture</tt>:

 f,g :: Picture -> Picture
 f <> g = \p -> f p <> g p

Function <code>copy</code> for an angle returns transformation, which shifts and rotates given picture, therefore <code>foldMap copy</code> for a list of angles returns a transformation, which shifts and rotates initial image five times. After that the resulting image is scaled to fit the inital size, so that it is ready for next iteration.

If one wants to get all intermediate pentaflakes <code>transformation</code> shoud be changed as follows:

```haskell
transformation = Scale s s . (Rotate 36 <> foldMap copy [0,72..288])
```


See also the implementation using [http://projects.haskell.org/diagrams/gallery/Pentaflake.html Diagrams]


## Java

[[File:sierpinski_pentagon.png|300px|thumb|right]]
```java
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.geom.Path2D;
import static java.lang.Math.*;
import java.util.Random;
import javax.swing.*;

public class SierpinskiPentagon extends JPanel {
    // exterior angle
    final double degrees072 = toRadians(72);

    /* After scaling we'll have 2 sides plus a gap occupying the length
       of a side before scaling. The gap is the base of an isosceles triangle
       with a base angle of 72 degrees. */
    final double scaleFactor = 1 / (2 + cos(degrees072) * 2);

    final int margin = 20;
    int limit = 0;
    Random r = new Random();

    public SierpinskiPentagon() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);

        new Timer(3000, (ActionEvent e) -> {
            limit++;
            if (limit >= 5)
                limit = 0;
            repaint();
        }).start();
    }

    void drawPentagon(Graphics2D g, double x, double y, double side, int depth) {
        double angle = 3 * degrees072; // starting angle

        if (depth == 0) {

            Path2D p = new Path2D.Double();
            p.moveTo(x, y);

            // draw from the top
            for (int i = 0; i < 5; i++) {
                x = x + cos(angle) * side;
                y = y - sin(angle) * side;
                p.lineTo(x, y);
                angle += degrees072;
            }

            g.setColor(RandomHue.next());
            g.fill(p);

        } else {

            side *= scaleFactor;

            /* Starting at the top of the highest pentagon, calculate
               the top vertices of the other pentagons by taking the
               length of the scaled side plus the length of the gap. */
            double distance = side + side * cos(degrees072) * 2;

            /* The top positions form a virtual pentagon of their own,
               so simply move from one to the other by changing direction. */
            for (int i = 0; i < 5; i++) {
                x = x + cos(angle) * distance;
                y = y - sin(angle) * distance;
                drawPentagon(g, x, y, side, depth - 1);
                angle += degrees072;
            }
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        int w = getWidth();
        double radius = w / 2 - 2 * margin;
        double side = radius * sin(PI / 5) * 2;

        drawPentagon(g, w / 2, 3 * margin, side, limit);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Sierpinski Pentagon");
            f.setResizable(true);
            f.add(new SierpinskiPentagon(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}

class RandomHue {
    /* Try to avoid random color values clumping together */
    final static double goldenRatioConjugate = (sqrt(5) - 1) / 2;
    private static double hue = Math.random();

    static Color next() {
        hue = (hue + goldenRatioConjugate) % 1;
        return Color.getHSBColor((float) hue, 1, 1);
    }
}
```



## JavaScript

;Notes:
* I didn't try to, but got the first of 2 possible versions according to [[wp:N-flake| WP N-flake]] article. Mine has central pentagon. All others here got second version.
* This one looks a little bit differently from the 1st version on WP. Almost like 2nd version, but with central pentagon.
* Not a Durer's pentagon either.
[[File:Pentaflakejs.png|200px|right|thumb|Output Pentaflakejs.png]]



```html

<html>
<head>
<script type="application/x-javascript">
// Globals
var cvs, ctx, scale=500, p0, ord=0, clr='blue', jc=0;
var clrs=['blue','navy','green','darkgreen','red','brown','yellow','cyan'];

function p5f() {
  cvs = document.getElementById("cvsid");
  ctx = cvs.getContext("2d");
  cvs.onclick=iter;
  pInit(); //init plot
}

function iter() {
  if(ord>5) {resetf(0)};
  ctx.clearRect(0,0,cvs.width,cvs.height);
  p0.forEach(iter5);
  p0.forEach(pIter5);
  ord++; document.getElementById("p1id").innerHTML=ord;
}

function iter5(v, i, a) {
  if(typeof(v[0][0]) == "object") {a[i].forEach(iter5)}
  else {a[i] = meta5(v)}
}

function pIter5(v, i, a) {
  if(typeof(v[0][0]) == "object") {v.forEach(pIter5)}
  else {pPoly(v)}
}

function pInit() {
  p0 = [make5([.5,.5], .5)];
  pPoly(p0[0]);
}

function meta5(h) {
  c=h[0]; p1=c; p2=h[1]; z1=p1[0]-p2[0]; z2=p1[1]-p2[1];
  dist = Math.sqrt(z1*z1 + z2*z2)/2.65;
  nP=[];
  for(k=1; k<h.length; k++) {
    p1=h[k]; p2=c; a=Math.atan2(p2[1]-p1[1], p2[0]-p1[0]);
    nP[k] = make5(ppad(a, dist, h[k]), dist)
  }
  nP[0]=make5(c, dist);
  return nP;
}

function make5(c, r) {
  vs=[]; j = 1;
  for(i=1/10; i<2; i+=2/5) {
    vs[j]=ppad(i*Math.PI, r, c); j++;
  }
  vs[0] = c; return vs;
}

function pPoly(s) {
  ctx.beginPath();
  ctx.moveTo(s[1][0]*scale, s[1][1]*-scale+scale);
  for(i=2; i<s.length; i++)
    ctx.lineTo(s[i][0]*scale, s[i][1]*-scale+scale);
  ctx.fillStyle=clr; ctx.fill()
}

// a - angle, d - distance, p - point
function ppad(a, d, p) {
  x=p[0]; y=p[1];
  x2=d*Math.cos(a)+x; y2=d*Math.sin(a)+y;
  return [x2,y2]
}

function resetf(rord) {
  ctx.clearRect(0,0,cvs.width,cvs.height);
  ord=rord; jc++; if(jc>7){jc=0}; clr=clrs[jc];
  document.getElementById("p1id").innerHTML=ord;
  p5f();
}
</script>
</head>
 <body onload="p5f()" style="font-family: arial, helvatica, sans-serif;">
 	<b>Click Pentaflake to iterate.</b>  Order: <label id='p1id'>0</label>
 	<input type="submit" value="RESET" onclick="resetf(0);">
 	(Reset anytime: to start new Pentaflake and change color.)
 	<br /><br />
    <canvas id="cvsid" width=640 height=640></canvas>
 </body>
</html>

```


```txt

Page with Pentaflakejs.png
Clicking Pentaflake you can see orders 1-6 of it in different colors.

```



## Julia

```julia
using Printf

const sides = 5
const order = 5
const dim   = 250
const scale = (3 - order ^ 0.5) / 2
const τ = 8 * atan(1, 1)
const orders = map(x -> ((1 - scale) * dim) * scale ^ x, 0:order-1)
cis(x) = Complex(cos(x), sin(x))
const vertices = map(x -> cis(x * τ  / sides), 0:sides-1)

fh = open("sierpinski_pentagon.svg", "w")
print(fh, """<svg height=\"$(dim*2)\" width=\"$(dim*2)\" style=\"fill:blue\" """ *
    """version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n""")

for i in 1:sides^order
    varr = [vertices[parse(Int, ch) + 1] for ch in split(string(i, base=sides, pad=order), "")]
    vector = sum(map(x -> varr[x] * orders[x], 1:length(orders)))
    vprod = map(x -> vector + orders[end] * (1-scale) * x, vertices)

    points = join([@sprintf("%.3f %.3f", real(v), imag(v)) for v in vprod], " ")
    print(fh, "<polygon points=\"$points\" transform=\"translate($dim,$dim) rotate(-18)\" />\n")
end

print(fh, "</svg>")
close(fh)
```



## Kotlin

```scala
// version 1.1.2

import java.awt.*
import java.awt.geom.Path2D
import java.util.Random
import javax.swing.*

class SierpinskiPentagon : JPanel() {
    // exterior angle
    private val degrees072 = Math.toRadians(72.0)

    /* After scaling we'll have 2 sides plus a gap occupying the length
       of a side before scaling. The gap is the base of an isosceles triangle
       with a base angle of 72 degrees. */
    private val scaleFactor = 1.0 / (2.0 + Math.cos(degrees072) * 2.0)

    private val margin = 20
    private var limit = 0
    private val r = Random()

    init {
        preferredSize = Dimension(640, 640)
        background = Color.white
        Timer(3000) {
            limit++
            if (limit >= 5) limit = 0
            repaint()
        }.start()
    }

    private fun drawPentagon(g: Graphics2D, x: Double, y: Double, s: Double, depth: Int) {
        var angle = 3.0 * degrees072  // starting angle
        var xx = x
        var yy = y
        var side = s
        if (depth == 0) {
            val p = Path2D.Double()
            p.moveTo(xx, yy)

            // draw from the top
            for (i in 0 until 5) {
                xx += Math.cos(angle) * side
                yy -= Math.sin(angle) * side
                p.lineTo(xx, yy)
                angle += degrees072
            }

            g.color = RandomHue.next()
            g.fill(p)
        }
        else {
            side *= scaleFactor
            /* Starting at the top of the highest pentagon, calculate
               the top vertices of the other pentagons by taking the
               length of the scaled side plus the length of the gap. */
            val distance = side + side * Math.cos(degrees072) * 2.0

            /* The top positions form a virtual pentagon of their own,
               so simply move from one to the other by changing direction. */
            for (i in 0 until 5) {
                xx += Math.cos(angle) * distance
                yy -= Math.sin(angle) * distance
                drawPentagon(g, xx, yy, side, depth - 1)
                angle += degrees072
            }
        }
    }

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        val hw = width / 2
        val radius = hw - 2.0 * margin
        val side = radius * Math.sin(Math.PI / 5.0) * 2.0
        drawPentagon(g, hw.toDouble(), 3.0 * margin, side, limit)
    }

    private class RandomHue {
        /* Try to avoid random color values clumping together */
        companion object {
            val goldenRatioConjugate = (Math.sqrt(5.0) - 1.0) / 2.0
            var hue = Math.random()

            fun next(): Color {
                hue = (hue + goldenRatioConjugate) % 1
                return Color.getHSBColor(hue.toFloat(), 1.0f, 1.0f)
            }
        }
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "Sierpinski Pentagon"
        f.isResizable = true
        f.add(SierpinskiPentagon(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.isVisible = true
    }
}
```



## Mathematica


```mathematica
pentaFlake[0] = RegularPolygon[5];
pentaFlake[n_] :=
 GeometricTransformation[pentaFlake[n - 1],
  TranslationTransform /@
   CirclePoints[{GoldenRatio^(2 n - 1), Pi/10}, 5]]

Graphics@pentaFlake[4]
```

https://i.imgur.com/rvXvQc0.png


## MATLAB


```MATLAB
[x, x0] = deal(exp(1i*(0.5:.4:2.1)*pi));
for k = 1 : 4
  x = x(:) + x0 * (1 + sqrt(5)) * (3 + sqrt(5)) ^(k - 1) / 2 ^ k;
end
patch('Faces', reshape(1 : 5 * 5 ^ k, 5, '')', 'Vertices', [real(x(:)) imag(x(:))])
axis image off
```

http://i.imgur.com/8ht6HqG.png


## Perl

```perl
use ntheory qw(todigits);
use Math::Complex;

$sides = 5;
$order = 5;
$dim   = 250;
$scale = ( 3 - 5**.5 ) / 2;
push @orders, ((1 - $scale) * $dim) * $scale ** $_ for 0..$order-1;

open $fh, '>', 'sierpinski_pentagon.svg';
print $fh qq|<svg height="@{[$dim*2]}" width="@{[$dim*2]}" style="fill:blue" version="1.1" xmlns="http://www.w3.org/2000/svg">\n|;

$tau = 2 * 4*atan2(1, 1);
push @vertices, cis( $_ * $tau / $sides ) for 0..$sides-1;

for $i (0 .. -1+$sides**$order)  {
    @base5 = todigits($i,5);
    @i = ( ((0)x(-1+$sides-$#base5) ), @base5);
    @v = @vertices[@i];
    $vector = 0;
    $vector += $v[$_] * $orders[$_] for 0..$#orders;

    my @points;
    for (@vertices) {
        $v = $vector + $orders[-1] * (1 - $scale) * $_;
        push @points, sprintf '%.3f %.3f', $v->Re, $v->Im;
    }
    print $fh pgon(@points);
}

sub cis  { Math::Complex->make(cos($_[0]), sin($_[0])) }
sub pgon { my(@q)=@_; qq|<polygon points="@q" transform="translate($dim,$dim) rotate(-18)"/>\n| }

print $fh '</svg>';
close $fh;
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/sierpinski_pentagon.svg Sierpinski pentagon] (offsite image)


## Perl 6

```perl6
constant $sides = 5;
constant order  = 5;
constant $dim   = 250;
constant scaling-factor = ( 3 - 5**.5 ) / 2;
my @orders = ((1 - scaling-factor) * $dim) «*» scaling-factor «**» (^order);

my $fh = open('sierpinski_pentagon.svg', :w);

$fh.say: qq|<svg height="{$dim*2}" width="{$dim*2}" style="fill:blue" version="1.1" xmlns="http://www.w3.org/2000/svg">|;

my @vertices = map { cis( $_ * τ / $sides ) }, ^$sides;

for 0 ..^ $sides ** order -> $i {
   my $vector = [+] @vertices[$i.base($sides).fmt("%{order}d").comb] «*» @orders;
   $fh.say: pgon ((@orders[*-1] * (1 - scaling-factor)) «*» @vertices «+» $vector)».reals».fmt("%0.3f");
};

sub pgon (@q) { qq|<polygon points="{@q}" transform="translate({$dim},{$dim}) rotate(-18)"/>| }

$fh.say: '</svg>';
$fh.close;
```


See [http://rosettacode.org/mw/images/5/57/Perl6_pentaflake.svg 5th order pentaflake]


## Python

Draws the result on a canvas. Runs pretty slowly.


```python
from turtle import *
import math
speed(0)      # 0 is the fastest speed. Otherwise, 1 (slow) to 10 (fast)
hideturtle()  # hide the default turtle

part_ratio = 2 * math.cos(math.radians(72))
side_ratio = 1 / (part_ratio + 2)

hide_turtles = True   # show/hide turtles as they draw
path_color = "black"  # path color
fill_color = "black"  # fill color

# turtle, size
def pentagon(t, s):
  t.color(path_color, fill_color)
  t.pendown()
  t.right(36)
  t.begin_fill()
  for i in range(5):
    t.forward(s)
    t.right(72)
  t.end_fill()

# iteration, turtle, size
def sierpinski(i, t, s):
  t.setheading(0)
  new_size = s * side_ratio

  if i > 1:
    i -= 1

    # create four more turtles
    for j in range(4):
      t.right(36)
      short = s * side_ratio / part_ratio
      dist = [short, s, s, short][j]

      # spawn a turtle
      spawn = Turtle()
      if hide_turtles:spawn.hideturtle()
      spawn.penup()
      spawn.setposition(t.position())
      spawn.setheading(t.heading())
      spawn.forward(dist)

      # recurse for spawned turtles
      sierpinski(i, spawn, new_size)

    # recurse for parent turtle
    sierpinski(i, t, new_size)

  else:
    # draw a pentagon
    pentagon(t, s)
    # delete turtle
    del t

def main():
  t = Turtle()
  t.hideturtle()
  t.penup()
  screen = t.getscreen()
  y = screen.window_height()
  t.goto(0, y/2-20)

  i = 5       # depth. i >= 1
  size = 300  # side length

  # so the spawned turtles move only the distance to an inner pentagon
  size *= part_ratio

  # begin recursion
  sierpinski(i, t, size)

main()
```


See [https://trinket.io/python/5137ae2b92 online implementation]. See [http://i.imgur.com/96D0c7i.png completed output].


## Racket

```racket
#lang racket/base
(require racket/draw pict racket/math racket/class)

;; exterior angle
(define 72-degrees (degrees->radians 72))
;; After scaling we'll have 2 sides plus a gap occupying the length
;; of a side before scaling. The gap is the base of an isosceles triangle
;; with a base angle of 72 degrees.
(define scale-factor (/ (+ 2 (* (cos 72-degrees) 2))))
;; Starting at the top of the highest pentagon, calculate
;; the top vertices of the other pentagons by taking the
;; length of the scaled side plus the length of the gap.
(define dist-factor (+ 1 (* (cos 72-degrees) 2)))

;; don't use scale, since it scales brushes too (making lines all tiny)
(define (draw-pentagon x y side depth dc)
  (let recur ((x x) (y y) (side side) (depth depth))
    (cond
      [(zero? depth)
       (define p (new dc-path%))
       (send p move-to x y)
       (for/fold ((x x) (y y) (α (* 3 72-degrees))) ((i 5))
         (send p line-to x y)
         (values (+ x (* side (cos α)))
                 (- y (* side (sin α)))
                 (+ α 72-degrees)))
       (send p close)
       (send dc draw-path p)]
      [else
       (define side/ (* side scale-factor))
       (define dist (* side/ dist-factor))
       ;; The top positions form a virtual pentagon of their own,
       ;; so simply move from one to the other by changing direction.
       (for/fold ((x x) (y y) (α (* 3 72-degrees))) ((i 5))
         (recur x y side/ (sub1 depth))
         (values (+ x (* dist (cos α)))
                 (- y (* dist (sin α)))
                 (+ α 72-degrees)))])))

(define (dc-draw-pentagon depth w h #:margin (margin 4))
  (dc (lambda (dc dx dy)
        (define old-brush (send dc get-brush))
        (send dc set-brush (make-brush #:style 'transparent))
        (draw-pentagon (/ w 2)
                       (* 3 margin)
                       (* (- (/ w 2) (* 2 margin))
                          (sin (/ pi 5)) 2)
                       depth
                       dc)
        (send dc set-brush old-brush))
      w h))

(dc-draw-pentagon 1 120 120)
(dc-draw-pentagon 2 120 120)
(dc-draw-pentagon 3 120 120)
(dc-draw-pentagon 4 120 120)
(dc-draw-pentagon 5 640 640)
```



## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.event.ActionEvent
import java.awt.geom.Path2D

import javax.swing._

import scala.annotation.tailrec
import scala.math.{Pi, cos, sin, sqrt}

object SierpinskiPentagon extends App {
  SwingUtilities.invokeLater(() => {

    class SierpinskiPentagon extends JPanel {

      /* Try to avoid random color values clumping together */

      private var hue = math.random

      // exterior angle
      private val deg072 = 2 * Pi / 5d //toRadians(72)
      /* After scaling we'll have 2 sides plus a gap occupying the length
         of a side before scaling. The gap is the base of an isosceles triangle
         with a base angle of 72 degrees. */
      //private val scaleFactor = 1 / (2 + cos(deg072) * 2)
      private var limit = 0

      private def drawPentagon(g: Graphics2D, x: Double, y: Double, side: Double, depth: Int): Unit = {
        val scaleFactor = 1 / (2 + cos(deg072) * 2)

        if (depth == 0) {
          // draw from the top
          @tailrec
          def iter0(i: Int, x: Double, y: Double, angle: Double, p: Path2D.Double): Path2D.Double = {
            if (i < 0) p
            else {
              p.lineTo(x, y)
              iter0(i - 1, x + cos(angle) * side, y - sin(angle) * side, angle + deg072, p)
            }
          }

          def p1: Path2D.Double = iter0(4, x, y, 3 * deg072, {
            val p = new Path2D.Double
            p.moveTo(x, y)
            p
          })

          def p: Path2D.Double = iter0(4, x, y, 3 * deg072, p1)

          def next: Color = {
            hue = (hue + (sqrt(5) - 1) / 2) % 1
            Color.getHSBColor(hue.toFloat, 1, 1)
          }

          g.setColor(next)
          g.fill(p)
        }
        else {
          val _side = side * scaleFactor
          /* Starting at the top of the highest pentagon, calculate
             the top vertices of the other pentagons by taking the
             length of the scaled side plus the length of the gap. */
          val distance = _side + _side * cos(deg072) * 2
          /* The top positions form a virtual pentagon of their own,
             so simply move from one to the other by changing direction. */

          def iter1(i: Int, x: Double, y: Double, angle: Double): Unit = {
            if (i < 0) ()
            else {
              drawPentagon(g, x, y, _side, depth - 1)
              iter1(i - 1, x + cos(angle) * distance, y - sin(angle) * distance, angle + deg072)
            }
          }

          iter1(4, x + cos(3 * deg072) * distance, y - sin(3 * deg072) * distance, 4 * deg072)
        }
      }

      override def paintComponent(gg: Graphics): Unit = {
        val (g, margin) = (gg.asInstanceOf[Graphics2D], 20)
        val side = (getWidth / 2 - 2 * margin) * sin(Pi / 5) * 2

        super.paintComponent(gg)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        drawPentagon(g, getWidth / 2, 3 * margin, side, limit)
      }

      new Timer(3000, (_: ActionEvent) => {
        limit += 1
        if (limit >= 5) limit = 0
        repaint()
      }).start()

      setPreferredSize(new Dimension(640, 640))
      setBackground(Color.white)
    }

    val f = new JFrame("Sierpinski Pentagon") {
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setResizable(true)
      add(new SierpinskiPentagon, BorderLayout.CENTER)
      pack()
      setLocationRelativeTo(null)
      setVisible(true)
    }
  })

}
```


## Sidef

Generates a SVG image to STDOUT. Redirect to a file to capture and display it.

```ruby
define order = 5
define sides = 5
define dim   = 500
define scaling_factor = ((3 - 5**0.5) / 2)
var orders = order.of {|i| ((1-scaling_factor) * dim) * scaling_factor**i }

say <<"STOP";
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
    "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg height="#{dim*2}" width="#{dim*2}"
    style="fill:blue" transform="translate(#{dim},#{dim}) rotate(-18)"
    version="1.1" xmlns="http://www.w3.org/2000/svg">
STOP

var vertices = sides.of {|i| Complex(0, i * Number.tau / sides).exp }

for i in ^(sides**order) {
   var vector = ([vertices["%#{order}d" % i.base(sides) -> chars]] »*« orders «+»)
   var points = (vertices »*» orders[-1]*(1-scaling_factor) »+» vector »reals()» «%« '%0.3f')
   say ('<polygon points="' + points.join(' ') + '"/>')
}
 
say '</svg>'
```



## VBA

Using Excel

```vb
Private Sub sierpinski(Order_ As Integer, Side As Double)
    Dim Circumradius As Double, Inradius As Double
    Dim Height As Double, Diagonal As Double, HeightDiagonal As Double
    Dim Pi As Double, p(5) As String, Shp As Shape
    Circumradius = Sqr(50 + 10 * Sqr(5)) / 10
    Inradius = Sqr(25 + 10 * Sqr(5)) / 10
    Height = Circumradius + Inradius
    Diagonal = (1 + Sqr(5)) / 2
    HeightDiagonal = Sqr(10 + 2 * Sqr(5)) / 4
    Pi = WorksheetFunction.Pi
    Ratio = Height / (2 * Height + HeightDiagonal)
    'Get a base figure
    Set Shp = ThisWorkbook.Worksheets(1).Shapes.AddShape(msoShapeRegularPentagon, _
        2 * Side, 3 * Side / 2 + (Circumradius - Inradius) * Side, Diagonal * Side, Height * Side)
    p(0) = Shp.Name
    Shp.Rotation = 180
    Shp.Line.Weight = 0
    For j = 1 To Order_
        'Place 5 copies of the figure in a circle around it
        For i = 0 To 4
            'Copy the figure
            Set Shp = Shp.Duplicate
            p(i + 1) = Shp.Name
            If i = 0 Then Shp.Rotation = 0
            'Place around in a circle
            Shp.Left = 2 * Side + Side * Inradius * 2 * Cos(2 * Pi * (i - 1 / 4) / 5)
            Shp.Top = 3 * Side / 2 + Side * Inradius * 2 * Sin(2 * Pi * (i - 1 / 4) / 5)
            Shp.Visible = msoTrue
        Next i
        'Group the 5 figures
        Set Shp = ThisWorkbook.Worksheets(1).Shapes.Range(p()).Group
        p(0) = Shp.Name
        If j < Order_ Then
            'Shrink the figure
            Shp.ScaleHeight Ratio, False
            Shp.ScaleWidth Ratio, False
            'Flip vertical and place in the center
            Shp.Rotation = 180
            Shp.Left = 2 * Side
            Shp.Top = 3 * Side / 2 + (Circumradius - Inradius) * Side
        End If
    Next j
End Sub

Public Sub main()
    sierpinski Order_:=5, Side:=200
End Sub
```



## zkl

```zkl
const order=5, sides=5, dim=250, scaleFactor=((3.0 - (5.0).pow(0.5))/2);
const tau=(0.0).pi*2; // 2*pi*r
orders:=order.pump(List,fcn(n){ (1.0 - scaleFactor)*dim*scaleFactor.pow(n) });

println(
#<<<
0'|<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
    "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg height="%d" width="%d" style="fill:blue" transform="translate(%d,%d) rotate(-18)"
    version="1.1" xmlns="http://www.w3.org/2000/svg">|
#<<<
   .fmt(dim*2,dim*2,dim,dim));

vertices:=sides.pump(List,fcn(s){ (1.0).toRectangular(tau*s/sides) }); // points on unit circle
vx:=vertices.apply('wrap([(a,b)]v,x){ return(a*x,b*x) },  // scaled points
		orders[-1]*(1.0 - scaleFactor));
fmt:="%%0%d.%dB".fmt(sides,order).fmt; //-->%05.5B (leading zeros, 5 places, base 5)
sides.pow(order).pump(Console.println,'wrap(i){
   vector:=fmt(i).pump(List,vertices.get)  // "00012"-->(vertices[0],..,vertices[2])
     .zipWith(fcn([(a,b)]v,x){ return(a*x,b*x) },orders) // ((a,b)...)*x -->((ax,bx)...)
     .reduce(fcn(vsum,v){ vsum[0]+=v[0]; vsum[1]+=v[1]; vsum },L(0.0, 0.0)); //-->(x,y)
   pgon(vx.apply(fcn([(a,b)]v,c,d){ return(a+c,b+d) },vector.xplode()));
});
println("</svg>");  // 3,131 lines

fcn pgon(vertices){  // eg ( ((250,0),(248.595,1.93317),...), len 5
   0'|<polygon points="%s"/>|.fmt(
       vertices.pump(String,fcn(v){ "%.3f %.3f ".fmt(v.xplode()) }) )
}
```

See [http://www.zenkinetic.com/Images/RosettaCode/sierpinskiPentagon.zkl.svg this image].
Displays fine in FireFox, in Chrome, it doesn't appear to be transformed so you only see part of the image.

```txt

zkl bbb > sierpinskiPentagon.zkl.svg
$ wc sierpinskiPentagon.zkl.svg
  3131  37519 314183 sierpinskiPentagon.zkl.svg

```


+++
title = "Penrose tiling"
description = ""
date = 2019-09-05T10:22:18Z
aliases = []
[extra]
id = 20837
[taxonomies]
categories = ["task"]
tags = []
+++

A [[wp:Penrose_tiling|Penrose tiling]] can cover an entire plane without creating a pattern that periodically repeats.

There are many tile sets that can create non-periodic tilings, but those can typically also be used to create a periodic 
tiling. What makes Penrose tiles special is that they ''can only be used to produce non-periodic tilings''.



[[File:Penrose_tilesets.png]]

The two best-known Penrose tile sets are <code>Kite and Dart (P2)</code> and <code>Thin Rhombus and Fat Rhombus (P3)</code>

These so-called prototiles are usually depicted with smooth edges, but in reality Penrose tiles have interlocking tabs
and cut-outs like the pieces of a jigsaw puzzle. For convenience these deformations are often replaced
with ''matching rules'', which ensure that the tiles are only connected in ways that guarantee
a non-periodic tiling. (Otherwise, for instance, you could combine the kite and dart to form a rhombus,
and easily create a periodic tiling from there.)

You can construct a Penrose tiling by setting up some prototiles, and adding tiles through trial and error, backtracking whenever you get stuck.

More commonly a method is used that takes advantage of the fact that Penrose tilings, like fractals,
have a self-similarity on different levels. When zooming out it can be observed that groups of tiles are enclosed in areas that
form exactly the same pattern as the tiles on the lower level. Departing from an inflated level, the prototiles can be
subdivided into smaller tiles, always observing the matching rules. The subdivision may have to be repeated several times, before the desired level of detail is reached. This process is called deflation.

More information can be found through the links below.

'''The task''': fill a rectangular area with a Penrose tiling.



## See also

* [http://www.ams.org/samplings/feature-column/fcarc-penrose A good introduction (ams.org)]
* [http://tartarus.org/simon/20110412-penrose/penrose.xhtml Deflation explained for both sets (tartarus.org)]
* [http://preshing.com/20110831/penrose-tiling-explained/ Deflation explained for Kite and Dart, includes Python code (preshing.com)]




## Go

```go
package main

import (
    "github.com/fogleman/gg"
    "math"
)

type tiletype int

const (
    kite tiletype = iota
    dart
)

type tile struct {
    tt          tiletype
    x, y        float64
    angle, size float64
}

var gr = (1 + math.Sqrt(5)) / 2 // golden ratio

const theta = math.Pi / 5 // 36 degrees in radians

func setupPrototiles(w, h int) []tile {
    var proto []tile
    // sun
    for a := math.Pi/2 + theta; a < 3*math.Pi; a += 2 * theta {
        ww := float64(w / 2)
        hh := float64(h / 2)
        proto = append(proto, tile{kite, ww, hh, a, float64(w) / 2.5})
    }
    return proto
}

func distinctTiles(tls []tile) []tile {
    tileset := make(map[tile]bool)
    for _, tl := range tls {
        tileset[tl] = true
    }
    distinct := make([]tile, len(tileset))
    for tl, _ := range tileset {
        distinct = append(distinct, tl)
    }
    return distinct
}

func deflateTiles(tls []tile, gen int) []tile {
    if gen <= 0 {
        return tls
    }
    var next []tile
    for _, tl := range tls {
        x, y, a, size := tl.x, tl.y, tl.angle, tl.size/gr
        var nx, ny float64
        if tl.tt == dart {
            next = append(next, tile{kite, x, y, a + 5*theta, size})
            for i, sign := 0, 1.0; i < 2; i, sign = i+1, -sign {
                nx = x + math.Cos(a-4*theta*sign)*gr*tl.size
                ny = y - math.Sin(a-4*theta*sign)*gr*tl.size
                next = append(next, tile{dart, nx, ny, a - 4*theta*sign, size})
            }
        } else {
            for i, sign := 0, 1.0; i < 2; i, sign = i+1, -sign {
                next = append(next, tile{dart, x, y, a - 4*theta*sign, size})
                nx = x + math.Cos(a-theta*sign)*gr*tl.size
                ny = y - math.Sin(a-theta*sign)*gr*tl.size
                next = append(next, tile{kite, nx, ny, a + 3*theta*sign, size})
            }
        }
    }
    // remove duplicates
    tls = distinctTiles(next)
    return deflateTiles(tls, gen-1)
}

func drawTiles(dc *gg.Context, tls []tile) {
    dist := [2][3]float64{{gr, gr, gr}, {-gr, -1, -gr}}
    for _, tl := range tls {
        angle := tl.angle - theta
        dc.MoveTo(tl.x, tl.y)
        ord := tl.tt
        for i := 0; i < 3; i++ {
            x := tl.x + dist[ord][i]*tl.size*math.Cos(angle)
            y := tl.y - dist[ord][i]*tl.size*math.Sin(angle)
            dc.LineTo(x, y)
            angle += theta
        }
        dc.ClosePath()
        if ord == kite {
            dc.SetHexColor("FFA500") // orange
        } else {
            dc.SetHexColor("FFFF00") // yellow
        }
        dc.FillPreserve()
        dc.SetHexColor("A9A9A9") // dark gray
        dc.SetLineWidth(1)
        dc.Stroke()
    }
}

func main() {
    w, h := 700, 450
    dc := gg.NewContext(w, h)
    dc.SetRGB(1, 1, 1)
    dc.Clear()
    tiles := deflateTiles(setupPrototiles(w, h), 5)
    drawTiles(dc, tiles)
    dc.SavePNG("penrose_tiling.png")
}
```


```txt

Image same as Java entry.

```



## Java

[[File:Penrose_java.png|300px|thumb|right]]
```java
import java.awt.*;
import java.util.List;
import java.awt.geom.Path2D;
import java.util.*;
import javax.swing.*;
import static java.lang.Math.*;
import static java.util.stream.Collectors.toList;

public class PenroseTiling extends JPanel {
    // ignores missing hash code
    class Tile {
        double x, y, angle, size;
        Type type;

        Tile(Type t, double x, double y, double a, double s) {
            type = t;
            this.x = x;
            this.y = y;
            angle = a;
            size = s;
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof Tile) {
                Tile t = (Tile) o;
                return type == t.type && x == t.x && y == t.y && angle == t.angle;
            }
            return false;
        }
    }

    enum Type {
        Kite, Dart
    }

    static final double G = (1 + sqrt(5)) / 2; // golden ratio
    static final double T = toRadians(36); // theta

    List<Tile> tiles = new ArrayList<>();

    public PenroseTiling() {
        int w = 700, h = 450;
        setPreferredSize(new Dimension(w, h));
        setBackground(Color.white);

        tiles = deflateTiles(setupPrototiles(w, h), 5);
    }

    List<Tile> setupPrototiles(int w, int h) {
        List<Tile> proto = new ArrayList<>();

        // sun
        for (double a = PI / 2 + T; a < 3 * PI; a += 2 * T)
            proto.add(new Tile(Type.Kite, w / 2, h / 2, a, w / 2.5));

        return proto;
    }

    List<Tile> deflateTiles(List<Tile> tls, int generation) {
        if (generation <= 0)
            return tls;

        List<Tile> next = new ArrayList<>();

        for (Tile tile : tls) {
            double x = tile.x, y = tile.y, a = tile.angle, nx, ny;
            double size = tile.size / G;

            if (tile.type == Type.Dart) {
                next.add(new Tile(Type.Kite, x, y, a + 5 * T, size));

                for (int i = 0, sign = 1; i < 2; i++, sign *= -1) {
                    nx = x + cos(a - 4 * T * sign) * G * tile.size;
                    ny = y - sin(a - 4 * T * sign) * G * tile.size;
                    next.add(new Tile(Type.Dart, nx, ny, a - 4 * T * sign, size));
                }

            } else {

                for (int i = 0, sign = 1; i < 2; i++, sign *= -1) {
                    next.add(new Tile(Type.Dart, x, y, a - 4 * T * sign, size));

                    nx = x + cos(a - T * sign) * G * tile.size;
                    ny = y - sin(a - T * sign) * G * tile.size;
                    next.add(new Tile(Type.Kite, nx, ny, a + 3 * T * sign, size));
                }
            }
        }
        // remove duplicates
        tls = next.stream().distinct().collect(toList());

        return deflateTiles(tls, generation - 1);
    }

    void drawTiles(Graphics2D g) {
        double[][] dist = {{G, G, G}, {-G, -1, -G}};
        for (Tile tile : tiles) {
            double angle = tile.angle - T;
            Path2D path = new Path2D.Double();
            path.moveTo(tile.x, tile.y);

            int ord = tile.type.ordinal();
            for (int i = 0; i < 3; i++) {
                double x = tile.x + dist[ord][i] * tile.size * cos(angle);
                double y = tile.y - dist[ord][i] * tile.size * sin(angle);
                path.lineTo(x, y);
                angle += T;
            }
            path.closePath();
            g.setColor(ord == 0 ? Color.orange : Color.yellow);
            g.fill(path);
            g.setColor(Color.darkGray);
            g.draw(path);
        }
    }

    @Override
    public void paintComponent(Graphics og) {
        super.paintComponent(og);
        Graphics2D g = (Graphics2D) og;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        drawTiles(g);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Penrose Tiling");
            f.setResizable(false);
            f.add(new PenroseTiling(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## Julia

```julia
using Printf

function drawpenrose()
    lindenmayer_rules = Dict("A" => "",
        "M" => "OA++PA----NA[-OA----MA]++", "N" => "+OA--PA[---MA--NA]+",
        "O" => "-MA++NA[+++OA++PA]-", "P" => "--OA++++MA[+PA++++NA]--NA")

    rul(x) = lindenmayer_rules[x]

    penrose = replace(replace(replace(replace("[N]++[N]++[N]++[N]++[N]",
        r"[AMNOP]" => rul), r"[AMNOP]" => rul), r"[AMNOP]" => rul), r"[AMNOP]" => rul)

    x, y, theta, r, svglines, stack = 160, 160, π / 5, 20.0, String[], Vector{Real}[]

    for c in split(penrose, "")
        if c == "A"
            xx, yy = x + r * cos(theta), y + r * sin(theta)
            line = @sprintf("<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' style='stroke:rgb(255,165,0)'/>\n", x, y, xx, yy)
            x, y = xx, yy
            push!(svglines, line)
        elseif c == "+"
            theta += π / 5
        elseif c == "-"
            theta -= π / 5
        elseif c == "["
            push!(stack, [x, y, theta])
        elseif c == "]"
            x, y, theta = pop!(stack)
        end
    end

    svg = join(unique(svglines), "\n")
    fp = open("penrose_tiling.svg", "w")
    write(fp, """<svg xmlns="http://www.w3.org/2000/svg" height="350" width="350"> <rect height="100%" """ *
              """width="100%" style="fill:black" />""" * "\n$svg</svg>")
    close(fp)
end

drawpenrose()

```




## Kotlin

```scala
// version 1.1.2

import java.awt.*
import java.awt.geom.Path2D
import javax.swing.*

class PenroseTiling(w: Int, h: Int) : JPanel() {
    private enum class Type {
        KITE, DART
    }

    private class Tile(
        val type: Type, 
        val x: Double, 
        val y: Double, 
        val angle: Double, 
        val size: Double 
    ) {
        override fun equals(other: Any?): Boolean {
            if (other == null || other !is Tile) return false
            return type == other.type && x == other.x && y == other.y &&
                   angle == other.angle && size == other.size
        }
    }

    private companion object {
        val G = (1.0 + Math.sqrt(5.0)) / 2.0  // golden ratio 
        val T = Math.toRadians(36.0)          // theta
    }

    private val tiles: List<Tile>

    init {
        preferredSize = Dimension(w, h)
        background = Color.white 
        tiles = deflateTiles(setupPrototiles(w, h), 5)
    }

    private fun setupPrototiles(w: Int, h: Int): List<Tile> {
        val proto = mutableListOf<Tile>()
        var a = Math.PI / 2.0 + T
        while (a < 3.0 * Math.PI) {
            proto.add(Tile(Type.KITE, w / 2.0, h / 2.0, a, w / 2.5))
            a += 2.0 * T
        } 
        return proto
    }

    private fun deflateTiles(tls: List<Tile>, generation: Int): List<Tile> {
        if (generation <= 0) return tls
        val next = mutableListOf<Tile>()
 
        for (tile in tls) {
            val x = tile.x
            val y = tile.y
            val a = tile.angle
            var nx: Double 
            var ny: Double
            val size = tile.size / G
 
            if (tile.type == Type.DART) {
                next.add(Tile(Type.KITE, x, y, a + 5.0 * T, size))
                var sign = 1
                for (i in 0..1) {
                    nx = x + Math.cos(a - 4.0 * T * sign) * G * tile.size
                    ny = y - Math.sin(a - 4.0 * T * sign) * G * tile.size
                    next.add(Tile(Type.DART, nx, ny, a - 4.0 * T * sign, size))
                    sign *= -1
                }
            } 
            else { 
                var sign = 1
                for (i in 0..1) {
                    next.add(Tile(Type.DART, x, y, a - 4.0 * T * sign, size))
                    nx = x + Math.cos(a - T * sign) * G * tile.size
                    ny = y - Math.sin(a - T * sign) * G * tile.size
                    next.add(Tile(Type.KITE, nx, ny, a + 3.0 * T * sign, size))
                    sign *= -1
                }
            }
        }
        // remove duplicates and deflate 
        return deflateTiles(next.distinct(), generation - 1)
    }

    private fun drawTiles(g: Graphics2D) {
        val dist = arrayOf(
            doubleArrayOf(G, G, G),
            doubleArrayOf(-G, -1.0, -G)
        )
        for (tile in tiles) {
            var angle = tile.angle - T
            val path = Path2D.Double()
            path.moveTo(tile.x, tile.y) 
            val ord = tile.type.ordinal
            for (i in 0..2) {
                val x = tile.x + dist[ord][i] * tile.size * Math.cos(angle)
                val y = tile.y - dist[ord][i] * tile.size * Math.sin(angle)
                path.lineTo(x, y)
                angle += T
            }
            path.closePath()
            with(g) {
                color = if (ord == 0) Color.pink else Color.red
                fill(path)
                color = Color.darkGray
                draw(path)
            }
        }
    }

    override fun paintComponent(og: Graphics) {
        super.paintComponent(og)
        val g = og as Graphics2D 
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON)
        drawTiles(g)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with (f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Penrose Tiling"
            isResizable = false
            add(PenroseTiling(700, 450), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```



## Perl


```perl>use constant pi =
 2 * atan2(1, 0);

# Generated with a P3 tile set using a Lindenmayer system.
%rules = (
    A => '',
    M => 'OA++PA----NA[-OA----MA]++',
    N => '+OA--PA[---MA--NA]+',
    O => '-MA++NA[+++OA++PA]-',
    P => '--OA++++MA[+PA++++NA]--NA'
);
$penrose = '[N]++[N]++[N]++[N]++[N]';
$penrose =~ s/([AMNOP])/$rules{$1}/eg for 1..4;

# Draw the curve in SVG
($x, $y) = (160, 160);
$theta   = pi/5;
$r       = 20;

for (split //, $penrose) {
    if (/A/) {
        $line  = sprintf "<line x1='%.1f' y1='%.1f' ", $x, $y;
        $line .= sprintf "x2='%.1f' ", $x += $r * cos($theta);
        $line .= sprintf "y2='%.1f' ", $y += $r * sin($theta);
        $line .= "style='stroke:rgb(255,165,0)'/>\n";
        $SVG{$line} = 1;
    } elsif (/\+/) { $theta += pi/5
    } elsif (/\-/) { $theta -= pi/5
    } elsif (/\[/) { push @stack, [$x, $y, $theta]
    } elsif (/\]/) { ($x, $y, $theta) = @{pop @stack} }
}
$svg .= $_ for keys %SVG;
open  $fh, '>', 'penrose_tiling.svg';
print $fh  qq{<svg xmlns="http://www.w3.org/2000/svg" height="350" width="350"> <rect height="100%" width="100%" style="fill:black" />\n$svg</svg>};
close $fh;
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/penrose_tiling.svg Penrose tiling] (offsite image)


## Perl 6

Generated with a P3 tile set using a Lindenmayer system.


```perl6
use SVG;

role Lindenmayer {
    has %.rules;
    method succ {
	    self.comb.map( { %!rules{$^c} // $c } ).join but Lindenmayer(%!rules)
    }
}

my $penrose = '[N]++[N]++[N]++[N]++[N]' but Lindenmayer(
    {
        A => '',
        M => 'OA++PA----NA[-OA----MA]++',
        N => '+OA--PA[---MA--NA]+',
        O => '-MA++NA[+++OA++PA]-',
        P => '--OA++++MA[+PA++++NA]--NA'
    }
);

$penrose++ xx 4;

my @lines;
my @stack;

for $penrose.comb {
    state ($x, $y) = 300, 200;
    state $d = 55 + 0i;
    when 'A' { @lines.push: 'line' => [:x1($x.round(.01)), :y1($y.round(.01)), :x2(($x += $d.re).round(.01)), :y2(($y += $d.im).round(.01))] }
    when '[' { @stack.push: ($x.clone, $y.clone, $d.clone) }
    when ']' { ($x, $y, $d) = @stack.pop }
    when '+' { $d *= cis -π/5 }
    when '-' { $d *= cis  π/5 }
    default { }
}

say SVG.serialize(
    svg => [
        :600width, :400height, :style<stroke:rgb(250,12,210)>,
        :rect[:width<100%>, :height<100%>, :fill<black>],
        |@lines,
    ],
);
```

See: [https://github.com/thundergnat/rc/blob/master/img/penrose-perl6.svg Penrose tiling image]


## Phix

Translation of the original Python code

```Phix
-- demo\rosetta\Penrose_tiling.exw
--  Resizeable. Press space to iterate/subdivide, C to toggle colour scheme
bool yellow_orange = true   -- false = magenta on black, outlines only

include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

include builtins\complex.e
 
constant golden_ratio = (1 + sqrt(5)) / 2

function subdivide(sequence triangles)
    sequence result = {}
    integer colour
    complex A, B, C, P, Q, R
    for i=1 to length(triangles) do
        {colour, A, B, C} = triangles[i]
        if colour == 0 then
            -- Subdivide orange triangle
            P = complex_add(A,complex_div(complex_sub(B,A),golden_ratio))
            result &= {{0, C, P, B}, {1, P, C, A}}
        else
            -- Subdivide yellow triangle
            Q = complex_add(B,complex_div(complex_sub(A,B),golden_ratio))
            R = complex_add(B,complex_div(complex_sub(C,B),golden_ratio))
            result &= {{1, R, C, A}, {1, Q, R, B}, {0, R, Q, A}}
        end if
    end for
    return result
end function

function initial_wheel()
-- Create an initial wheel of yellow triangles around the origin
    sequence triangles = {}
    complex B, C
    atom phi
    for i=0 to 9 do
        phi = (2*i-1)*PI/10
        B = {cos(phi),sin(phi)}
        phi = (2*i+1)*PI/10
        C = {cos(phi),sin(phi)}
        if mod(i,2)==0 then
            {B, C} = {C, B}  -- mirror every second triangle
        end if
        triangles &= {{0, {0,0}, B, C}}
    end for
    return subdivide(triangles) -- ... and iterate once
end function

sequence triangles = initial_wheel()

integer hw, hh, h

procedure draw_one(sequence triangle, integer colour, mode)
    if yellow_orange then
        cdCanvasSetForeground(cddbuffer, colour)
    end if
    cdCanvasBegin(cddbuffer, mode)
    for i=2 to 4 do
        atom {x,y} = triangle[i]
        cdCanvasVertex(cddbuffer, x*h+hw, y*h+hh) 
    end for
    cdCanvasEnd(cddbuffer)
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    {hw, hh} = sq_floor_div(IupGetIntInt(canvas, "DRAWSIZE"),2)
    h = min(hw,hh)
    if yellow_orange then
        cdCanvasSetBackground(cddbuffer, CD_WHITE)
    else
        cdCanvasSetBackground(cddbuffer, CD_BLACK)
        cdCanvasSetForeground(cddbuffer, CD_MAGENTA)
    end if
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    for i=1 to length(triangles) do
        sequence triangle = triangles[i]
        if yellow_orange then
            integer colour = iff(triangle[1]?CD_ORANGE:CD_YELLOW)
            draw_one(triangle,colour,CD_FILL)
        end if
        draw_one(triangle,CD_DARK_GREY,CD_CLOSED_LINES)
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=' ' then
        triangles = subdivide(triangles)
        IupUpdate(canvas)
    elsif upper(c)='C' then
        yellow_orange = not yellow_orange
        IupUpdate(canvas)
    end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()
    
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "600x600") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Penrose tiling")
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

Output can be toggled to look like the java or perl output


## Racket


```racket
#lang racket
 
(require racket/draw)
 
(define rules '([M . (O A + + P A - - - - N A < - O A - - - - M A > + +)]
                [N . (+ O A - - P A < - - - M A - - N A > +)]
                [O . (- M A + + N A < + + + O A + + P A > -)]
                [P . (- - O A + + + + M A < + P A + + + + N A > - - N A)]
                [S . (< N > + + < N > + + < N > + + < N > + + < N >)]))
 
(define (get-cmds n cmd)
  (cond
    [(= 0 n) (list cmd)]
    [else (append-map (curry get-cmds (sub1 n))
                      (dict-ref rules cmd (list cmd)))]))
 
(define (make-curve DIM N R OFFSET COLOR BACKGROUND-COLOR)
  (define target (make-bitmap DIM DIM))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-background BACKGROUND-COLOR)
  (send dc set-pen COLOR 1 'solid)
  (send dc clear)
  (for/fold ([x 160] [y 160] [θ (/ pi 5)] [S '()])
            ([cmd (in-list (get-cmds N 'S))])
    (define (draw/values x* y* θ* S*)
      (send/apply dc draw-line (map (curry + OFFSET) (list x y x* y*)))
      (values x* y* θ* S*))
    (match cmd
      ['A (draw/values (+ x (* R (cos θ))) (+ y (* R (sin θ))) θ S)]
      ['+ (values x y (+ θ (/ pi 5)) S)]
      ['- (values x y (- θ (/ pi 5)) S)]
      ['<  (values x y θ (cons (list x y θ) S))]
      ['> (match-define (cons (list x y θ) S*) S)
          (values x y θ S*)]
      [_ (values x y θ S)]))
  target)
 
(make-curve 500 4 20 80 (make-color 255 255 0) (make-color 0 0 0))
```



## Scala


### Java Swing Interoperability

```Scala
import java.awt.{BorderLayout, Color, Dimension, Graphics, Graphics2D, RenderingHints}
import java.awt.geom.Path2D

import javax.swing.{JFrame, JPanel}

import scala.math._

object PenroseTiling extends App {
  private val (φ, ϑ) = ((1 + sqrt(5)) / 2, toRadians(36)) // golden ratio and 36 degrees
  private val dist: Array[Array[Double]] = Array(Array(φ, φ, φ), Array(-φ, -1, -φ))

  class PenroseTiling extends JPanel {
    private val (w, h) = (700, 450)
    private val tiles: Set[Tile] = deflateTiles(setupPrototiles(w, h), 5)

    override def paintComponent(og: Graphics): Unit = {
      def drawTiles(g: Graphics2D): Unit =
        for (tile <- tiles) {
          val path: Path2D = new Path2D.Double()
          val distL = dist(tile.tileType.id)

          path.moveTo(tile.x, tile.y)
          for {i <- 0 until 3
               ω = tile.α + (i - 1) * ϑ}
            path.lineTo(
              tile.x + distL(i) * tile.size * cos(ω),
              tile.y - distL(i) * tile.size * sin(ω))

          path.closePath()
          g.setColor(if (tile.tileType == Type.Kite) Color.orange else Color.yellow)
          g.fill(path)
          g.setColor(Color.darkGray)
          g.draw(path)
        }

      super.paintComponent(og)
      val g: Graphics2D = og.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      drawTiles(g)
    }

    private def setupPrototiles(w: Int, h: Int): Set[Tile] = (0 to 5).map(n =>
      Tile(Type.Kite, (w / 2).toDouble, (h / 2).toDouble, Pi / 2 + ϑ + n * 2 * ϑ, w / 2.5)).toSet

    @scala.annotation.tailrec
    private def deflateTiles(tls: Set[Tile], generation: Int): Set[Tile] =
      if (generation > 0) {
        val next = for {
          tile <- tls
          size = tile.size / φ
        } yield {

          def nx(factor: Int) = tile.x + cos(tile.α - factor * ϑ) * φ * tile.size
          def ny(factor: Int) = tile.y - sin(tile.α - factor * ϑ) * φ * tile.size

          tile.tileType match {
            case Type.Dart =>
              Seq(Tile(Type.Kite, tile.x, tile.y, tile.α + 5 * ϑ, size)) ++
                (for (sign <- -1 to 1 by 2)
                  yield Tile(Type.Dart, nx(sign * 4), ny(sign * 4), tile.α - 4 * ϑ * sign, size))

            case Type.Kite => (for (sign <- 1 to -1 by -2) yield {
              Seq(Tile(Type.Dart, tile.x, tile.y, tile.α - 4 * ϑ * sign, size),
                Tile(Type.Kite, nx(sign), ny(sign), tile.α + 3 * ϑ * sign, size))
            }).flatten
          }
        }
        deflateTiles(next.flatten, generation - 1)
      } else tls

    private case class Tile(tileType: Type.Type, x: Double, y: Double, α: Double, size: Double)

    private object Type extends Enumeration {
      type Type = Value
      val Kite, Dart = Value
    }

    setPreferredSize(new Dimension(w, h))
    setBackground(Color.white)
  }

  new JFrame("Penrose Tiling") {
    add(new PenroseTiling(), BorderLayout.CENTER)
    pack()
    setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    setLocationRelativeTo(null)
    setResizable(false)
    setVisible(true)
  }

}
```


## Sidef

Using the LSystem class defined at [https://rosettacode.org/wiki/Hilbert_curve#Sidef Hilbert curve].

```ruby
var rules = Hash(
    a => 'cE++dE----bE[-cE----aE]++',
    b => '+cE--dE[---aE--bE]+',
    c => '-aE++bE[+++cE++dE]-',
    d => '--cE++++aE[+dE++++bE]--bE',
    E => '',
)

var lsys = LSystem(
    width:  1000,
    height: 1000,

    scale: 1,
    xoff: -500,
    yoff: -500,

    len:   40,
    angle: 36,
    color: 'dark blue',
)

lsys.execute('[b]++[b]++[b]++[b]++[b]', 5, "penrose_tiling.png", rules)
```


Output image: [https://github.com/trizen/rc/blob/master/img/penrose-tiling-sidef.png Penrose tiling]

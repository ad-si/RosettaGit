+++
title = "Curve that touches three points"
description = ""
date = 2019-05-18T00:02:42Z
aliases = []
[extra]
id = 22053
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "j",
  "julia",
  "perl",
  "perl_6",
  "phix",
  "zkl",
]
+++

Draw a curve that touches 3 points (1 starting point, 2 medium, 3 final point)

::#  Do not use functions of a library, implement the curve() function yourself
::#  coordinates:(x,y) starting point (10,10) medium point (100,200) final point (200,10)





## Go

There are, of course, an infinity of curves which can be fitted to 3 points. The most obvious solution is to fit a quadratic curve (using Lagrange interpolation) and so that's what we do here.

As we're not allowed to use library functions to draw the curve, we instead divide the x-axis of the curve between successive points into equal segments and then join the resulting points with straight lines.

The resulting 'curve' is then saved to a .png file where it can be viewed with a utility such as EOG.  

```go
package main

import "github.com/fogleman/gg"

var p = [3]gg.Point{{10, 10}, {100, 200}, {200, 10}}

func lagrange(x float64) float64 {
    return (x-p[1].X)*(x-p[2].X)/(p[0].X-p[1].X)/(p[0].X-p[2].X)*p[0].Y +
        (x-p[0].X)*(x-p[2].X)/(p[1].X-p[0].X)/(p[1].X-p[2].X)*p[1].Y +
        (x-p[0].X)*(x-p[1].X)/(p[2].X-p[0].X)/(p[2].X-p[1].X)*p[2].Y
}

func getPoints(n int) []gg.Point {
    pts := make([]gg.Point, 2*n+1)
    dx := (p[1].X - p[0].X) / float64(n)
    for i := 0; i < n; i++ {
        x := p[0].X + dx*float64(i)
        pts[i] = gg.Point{x, lagrange(x)}
    }
    dx = (p[2].X - p[1].X) / float64(n)
    for i := n; i < 2*n+1; i++ {
        x := p[1].X + dx*float64(i-n)
        pts[i] = gg.Point{x, lagrange(x)}
    }
    return pts
}

func main() {
    const n = 50 // more than enough for this
    dc := gg.NewContext(210, 210)
    dc.SetRGB(1, 1, 1) // White background
    dc.Clear()
    for _, pt := range getPoints(n) {
        dc.LineTo(pt.X, pt.Y)
    }
    dc.SetRGB(0, 0, 0) // Black curve
    dc.SetLineWidth(1)
    dc.Stroke()
    dc.SavePNG("quadratic_curve.png")
}
```




## J


```txt

   NB. coordinates:(x,y) starting point (10,10) medium point (100,200) final point (200,10)

   X=: 10 100 200
   Y=: 10 200 10

   NB. matrix division computes polynomial coefficients
   NB. %. implements singular value decomposition
   NB. in other words, we can also get best fit polynomials of lower order.

   polynomial=: (Y %. (^/ ([: i. #)) X)&p.


   assert 10 200 10 -: polynomial X  NB. test



   Filter=: (#~`)(`:6)

   Round=: adverb def '<.@:(1r2&+)&.:(%&m)'
   assert 100 120 -: 100 8 Round 123  NB. test, round 123 to nearest multiple of 100 and of 8



   NB. libraries not permitted, character cell graphics are used.


   GRAPH=: 50 50 $ ' '  NB. is an array of spaces

   NB. place the axes
   GRAPH=: '-' [`(([:<0; i.@:#)@:])`]} GRAPH
   GRAPH=: '|' [`(([:<0;~i.@:#)@:])`]} GRAPH
   GRAPH=: '+' [`((<0;0)"_)`]} GRAPH           NB. origin


   NB. clip the domain.
   EXES=: ((<:&(>./X) *. (<./X)&<:))Filter 5 * i. 200
   WHYS=: polynomial EXES


   NB. draw the curve
   1j1 #"1 |. 'X' [`((<"1 WHYS ;&>&:([: 1 Round %&5) EXES)"_)`]} GRAPH


   NB. were we to use a library:
   load'plot'
   'title 3 point fit' plot (j. polynomial) i.201

```



## Julia

To make things more specific, find the circle determined by the points. The curve is then the arc between the 3 points.

```julia
using Makie

struct Point; x::Float64; y::Float64; end
# Find a circle passing through the 3 points
const p1 = Point(10, 10)
const p2 = Point(100, 200)
const p3 = Point(200, 10)
const allp = [p1, p2, p3]

# set up problem matrix and solve.
# if (x - a)^2 + (y - b)^2 = r^2 then for some D, E, F, x^2 + y^2 + Dx + Ey + F = 0
# therefore Dx + Ey + F = -x^2 - y^2
v = zeros(Int, 3)
m = zeros(Int, 3, 3)
for row in 1:3
    m[row, 1:3] .= [allp[row].x, allp[row].y, 1]
    v[row] = -(allp[row].x)^2 - (allp[row].y)^2
end
q = (m \ v)  # [-210.0, -162.632, 3526.32]
a, b, r = -q[1] / 2, -q[2] / 2, sqrt((q[1]^2/4) + q[2]^2/4 - q[3])

println("The circle with center at x = $a, y = $b and radius $r.")

x = a-r:0.25:a+r
y0 = sqrt.(r^2 .- (x .- a).^2)
scene = lines(x, y0 .+ b, color = :red)
lines!(scene, x, b .- y0, color = :red)
scatter!(scene, [p.x for p in allp], [p.y for p in allp],  markersize = r / 10)

```
```txt

The circle with center at x = 105.0, y = 81.31578947368422 and radius 118.78948534384199.

```



## Perl

Hilbert '''curve''' task code repeated here, with the addition that the 3 task-required points are marked.  Mostly satisfies the letter-of-the-law of task specification while (all in good fun) subverting the spirit of the thing.

```perl
use SVG;
use List::Util qw(max min);

use constant pi => 2 * atan2(1, 0);

# Compute the curve with a Lindemayer-system
%rules = (
    A => '-BF+AFA+FB-',
    B => '+AF-BFB-FA+'
);
$hilbert = 'A';
$hilbert =~ s/([AB])/$rules{$1}/eg for 1..6;

# Draw the curve in SVG
($x, $y) = (0, 0);
$theta   = pi/2;
$r       = 5;

for (split //, $hilbert) {
    if (/F/) {
        push @X, sprintf "%.0f", $x;
        push @Y, sprintf "%.0f", $y;
        $x += $r * cos($theta);
        $y += $r * sin($theta);
    }
    elsif (/\+/) { $theta += pi/2; }
    elsif (/\-/) { $theta -= pi/2; }
}

$max =  max(@X,@Y);
$xt  = -min(@X)+10;
$yt  = -min(@Y)+10;
$svg = SVG->new(width=>$max+20, height=>$max+20);
$points = $svg->get_path(x=>\@X, y=>\@Y, -type=>'polyline');
$svg->rect(width=>"100%", height=>"100%", style=>{'fill'=>'black'});
$svg->polyline(%$points, style=>{'stroke'=>'orange', 'stroke-width'=>1}, transform=>"translate($xt,$yt)");
my $task = $svg->group( id => 'task-points', style => { stroke => 'red', fill => 'red' },);
$task->circle( cx =>  10, cy =>  10, r => 1, id => 'point1' );
$task->circle( cx => 100, cy => 200, r => 1, id => 'point2' );
$task->circle( cx => 200, cy =>  10, r => 1, id => 'point3' );

open  $fh, '>', 'curve-3-points.svg';
print $fh  $svg->xmlify(-namespace=>'svg');
close $fh;
```

[https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/curve-3-points.svg Hilbert curve passing through 3 defined points] (offsite image)


## Perl 6

Kind of bogus. There are an infinite number of curves that pass through those three points. I'll assume a quadratic curve. Lots of bits and pieces borrowed from other tasks to avoid relying on library functions. 

Saved as a png for wide viewing support. Note that png coordinate systems have 0,0 in the upper left corner.


```perl6
use Image::PNG::Portable;

# Solve for a quadratic line that passes through those points
my (\a, \b, \c) =
  rref([[10², 10, 1, 10],[100², 100, 1, 200],[200², 200, 1, 10]])[*;*-1];

# General case quadratic line equation
sub f (\x) { a*x² + b*x + c }

# Scale it up a bit for display
my $scale = 2;

my ($w, $h) = (500, 500);
my $png = Image::PNG::Portable.new: :width($w), :height($h);

my ($lastx, $lasty) = 8, f(8).round;
(9 .. 202).map: -> $x {
    my $f = f($x).round;
    line($lastx, $lasty, $x, $f, $png, [0,255,127]);
    ($lastx, $lasty) = $x, $f;
}

# Highlight the 3 defining points
dot(|$_, $png, 2) for (10,10,[255,0,0]), (100,200,[255,0,0]), (200,10,[255,0,0]);

$png.write: 'Curve-3-points-perl6.png';

# Assorted helper routines 
sub rref (@m) {
    return unless @m;
    my ($lead, $rows, $cols) = 0, +@m, +@m[0];
    for ^$rows -> $r {
        $lead < $cols or return @m;
        my $i = $r;
        until @m[$i;$lead] {
            ++$i == $rows or next;
            $i = $r;
            ++$lead == $cols and return @m;
        }
        @m[$i, $r] = @m[$r, $i] if $r != $i;
        my $lv = @m[$r;$lead];
        @m[$r] »/=» $lv;
        for ^$rows -> $n {
            next if $n == $r;
            @m[$n] »-=» @m[$r] »*» (@m[$n;$lead] // 0);
        }
        ++$lead;
    }
    @m
}

sub line($x0 is copy, $y0 is copy, $x1 is copy, $y1 is copy, $png, @rgb) {
    my $steep = abs($y1 - $y0) > abs($x1 - $x0);
    ($x0,$y0,$x1,$y1) »*=» $scale;
    if $steep {
        ($x0, $y0) = ($y0, $x0);
        ($x1, $y1) = ($y1, $x1);
    }
    if $x0 > $x1 {
        ($x0, $x1) = ($x1, $x0);
        ($y0, $y1) = ($y1, $y0);
    }
    my $Δx = $x1 - $x0;
    my $Δy = abs($y1 - $y0);
    my $error = 0;
    my $Δerror = $Δy / $Δx;
    my $y-step = $y0 < $y1 ?? 1 !! -1;
    my $y = $y0;
    next if $y < 0;
    for $x0 .. $x1 -> $x {
        next if $x < 0;
        if $steep {
            $png.set($y, $x, |@rgb);
        } else {
            $png.set($x, $y, |@rgb);
        }
        $error += $Δerror;
        if $error >= 0.5 {
            $y += $y-step;
            $error -= 1.0;
        }
    }
}

sub dot ($X is copy, $Y is copy, @rgb, $png, $radius = 3) {
    ($X, $Y) »*=» $scale;
    for ($X X+ -$radius .. $radius) X ($Y X+ -$radius .. $radius) -> ($x, $y) {
        $png.set($x, $y, |@rgb) if ( $X - $x + ($Y - $y) * i ).abs <= $radius;
    }
}
```

See [https://github.com/thundergnat/rc/blob/master/img/Curve-3-points-perl6.png Curve-3-points-perl6.png] (offsite .png image)


## Phix

```Phix
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

enum X, Y
constant p = {{10,10},{100,200},{200,10}}
 
function lagrange(atom x)
   return (x - p[2][X])*(x - p[3][X])/(p[1][X] - p[2][X])/(p[1][X] - p[3][X])*p[1][Y] +
          (x - p[1][X])*(x - p[3][X])/(p[2][X] - p[1][X])/(p[2][X] - p[3][X])*p[2][Y] +
          (x - p[1][X])*(x - p[2][X])/(p[3][X] - p[1][X])/(p[3][X] - p[2][X])*p[3][Y]
end function
 
function getPoints(integer n)
    sequence pts = {}
    atom {dx,pt,cnt} := {(p[2][X] - p[1][X])/n, p[1][X], n}
    for j=1 to 2 do
        for i=0 to cnt do
            atom x := pt + dx*i;
            pts = append(pts,{x,lagrange(x)});
        end for
        {dx,pt,cnt} = {(p[3][X] - p[2][X])/n, p[2][X], n+1};
    end for
    return pts
end function
 
procedure draw_cross(sequence xy)
    integer {x,y} = xy
    cdCanvasLine(cddbuffer, x-3, y, x+3, y) 
    cdCanvasLine(cddbuffer, x, y-3, x, y+3) 
end procedure
    
function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    cdCanvasSetForeground(cddbuffer, CD_BLUE)
    cdCanvasBegin(cddbuffer,CD_OPEN_LINES)
    atom {x,y} = {p[1][X], p[1][Y]}; -- curve starting point
    cdCanvasVertex(cddbuffer, x, y)
    sequence pts = getPoints(50)
    for i=1 to length(pts) do
        {x,y} = pts[i]
        cdCanvasVertex(cddbuffer, x, y)
    end for
    cdCanvasEnd(cddbuffer)
    cdCanvasSetForeground(cddbuffer, CD_RED)
    for i=1 to length(p) do draw_cross(p[i]) end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()
    
    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "220x220")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas,"DIALOGFRAME=YES")
    IupSetAttribute(dlg, "TITLE", "Quadratic curve")
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))
    IupCloseOnEscape(dlg)

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## zkl

Uses Image Magick and
the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
const X=0, Y=1;   // p.X == p[X]
var p=L(L(10.0, 10.0), L(100.0, 200.0), L(200.0, 10.0));  // (x,y)
 
fcn lagrange(x){  // float-->float
   (x - p[1][X])*(x - p[2][X])/(p[0][X] - p[1][X])/(p[0][X] - p[2][X])*p[0][Y] +
   (x - p[0][X])*(x - p[2][X])/(p[1][X] - p[0][X])/(p[1][X] - p[2][X])*p[1][Y] +
   (x - p[0][X])*(x - p[1][X])/(p[2][X] - p[0][X])/(p[2][X] - p[1][X])*p[2][Y]
}
 
fcn getPoints(n){  // int-->( (x,y) ..)
  pts:=List.createLong(2*n+1);
  dx,pt,cnt := (p[1][X] - p[0][X])/n, p[0][X], n;
  do(2){
     foreach i in (cnt){
	x:=pt + dx*i;
	pts.append(L(x,lagrange(x)));
     }
     dx,pt,cnt = (p[2][X] - p[1][X])/n, p[1][X], n+1;
  }
  pts
}

fcn main{
   var [const] n=50; // more than enough for this
   img,color := PPM(210,210,0xffffff), 0;     // white background, black curve
   foreach x,y in (p){ img.cross(x.toInt(),y.toInt(), 0xff0000) } // mark 3 pts
 
   a,b := p[0][X].toInt(), p[0][Y].toInt(); // curve starting point
   foreach x,y in (getPoints(n)){
      x,y = x.toInt(),y.toInt();
      img.line(a,b, x,y, color);	 // can only deal with ints
      a,b = x,y;
   }
   img.writeJPGFile("quadraticCurve.zkl.jpg");
}();
```

Image at [http://www.zenkinetic.com/Images/RosettaCode/quadraticCurve.zkl.jpg quadratic curve]

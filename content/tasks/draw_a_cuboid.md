+++
title = "Draw a cuboid"
description = ""
date = 2019-09-09T18:18:34Z
aliases = []
[extra]
id = 8974
[taxonomies]
categories = ["task", "Geometric Primitives"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "awk",
  "axis_1_0_0_0_0_0",
  "bbc_basic",
  "befunge",
  "brlcad",
  "c",
  "clojure",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "factor",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "list_of_keypresses_for_demo",
  "logo",
  "lsl",
  "maple",
  "material_materials_marble",
  "maxima",
  "nim",
  "opacity_1_0",
  "openscad",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "processing_of_input",
  "prolog",
  "pure_data",
  "purebasic",
  "python",
  "racket",
  "related_tasks",
  "retro",
  "rexx",
  "ring",
  "rotate_one_step_per_keypress",
  "ruby",
  "scala",
  "selection_for_rotation_angle_axis",
  "selection_of_background_colors",
  "selection_of_colors",
  "selection_of_materials",
  "selection_of_opacity_transparancy",
  "sidef",
  "size_4_2_3_l_h_w",
  "tcl",
  "up_0_0_1_0_0_0",
  "vbscript",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Draw a   [http://en.wikipedia.org/wiki/Cuboid cuboid]   with relative dimensions of   <big> 2 &times; 3 &times; 4. </big>


The cuboid can be represented graphically, or in   [https://en.wikipedia.org/wiki/ASCII_art ASCII art],   depending on the language capabilities.

To fulfill the criteria of being a cuboid, three faces must be visible.

Either static or rotational projection is acceptable for this task.




## Related tasks

* [[Draw_a_rotating_cube|draw a rotating cube]]
* [[Write_language_name_in_3D_ASCII|write language name in 3D ASCII]]





## Ada

ASCII-Art output, one width unit is two characters long ('--').

```Ada
with Ada.Text_IO;

procedure Main is
   type Char_Matrix is
     array (Positive range <>, Positive range <>) of Character;

   function Create_Cuboid
     (Width, Height, Depth : Positive)
      return                 Char_Matrix
   is
      Result : Char_Matrix (1 .. Height + Depth + 3,
         1 .. 2 * Width + Depth + 3) := (others => (others => ' '));
   begin
      -- points
      Result (1, 1)                                      := '+';
      Result (Height + 2, 1)                             := '+';
      Result (1, 2 * Width + 2)                          := '+';
      Result (Height + 2, 2 * Width + 2)                 := '+';
      Result (Height + Depth + 3, Depth + 2)             := '+';
      Result (Depth + 2, 2 * Width + Depth + 3)          := '+';
      Result (Height + Depth + 3, 2 * Width + Depth + 3) := '+';
      -- width lines
      for I in 1 .. 2 * Width loop
         Result (1, I + 1)                          := '-';
         Result (Height + 2, I + 1)                 := '-';
         Result (Height + Depth + 3, Depth + I + 2) := '-';
      end loop;
      -- height lines
      for I in 1 .. Height loop
         Result (I + 1, 1)                             := '|';
         Result (I + 1, 2 * Width + 2)                 := '|';
         Result (Depth + I + 2, 2 * Width + Depth + 3) := '|';
      end loop;
      -- depth lines
      for I in 1 .. Depth loop
         Result (Height + 2 + I, 1 + I)             := '/';
         Result (1 + I, 2 * Width + 2 + I)          := '/';
         Result (Height + 2 + I, 2 * Width + 2 + I) := '/';
      end loop;
      return Result;
   end Create_Cuboid;

   procedure Print_Cuboid (Width, Height, Depth : Positive) is
      Cuboid : Char_Matrix := Create_Cuboid (Width, Height, Depth);
   begin
      for Row in reverse Cuboid'Range (1) loop
         for Col in Cuboid'Range (2) loop
            Ada.Text_IO.Put (Cuboid (Row, Col));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Cuboid;
begin
   Print_Cuboid (2, 3, 4);
end Main;
```

```txt
     +----+
    /    /|
   /    / |
  /    /  |
 /    /   +
+----+   /
|    |  /
|    | /
|    |/
+----+
```



## AutoHotkey

{{libheader|GDIP}}Some portions of code from [http://www.autohotkey.com/board/topic/29449-gdi-standard-library-145-by-tic/ Gdip examples].

```AutoHotkey
Angle := 45
C := 0.01745329252
W := 200
H := 300
L := 400
LX := L * Cos(Angle * C), LY := L * Sin(Angle * C)

If !pToken := Gdip_Startup()
{
   MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
   ExitApp
}
OnExit, Exit

A := 50, B := 650, WinWidth := 700, WinHeight := 700
TopX := (A_ScreenWidth - WinWidth) //2, TopY := (A_ScreenHeight - WinHeight) //2

Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA
hwnd1 := WinExist(), hbm := CreateDIBSection(WinWidth, WinHeight), hdc := CreateCompatibleDC()
	, obm := SelectObject(hdc, hbm), G := Gdip_GraphicsFromHDC(hdc), Gdip_SetSmoothingMode(G, 4)

Points := A "," B "|" A+W "," B "|" A+W "," B-H "|" A "," B-H
	, DrawFace(Points, 0xff0066ff, G)

Points := A+W "," B "|" A+W+LX "," B-LY "|" A+W+LX "," B-LY-H "|" A+W "," B-H
	, DrawFace(Points, 0xff00d400, G)

Points := A "," B-H "|" A+W "," B-H "|" A+W+LX "," B-LY-H "|" A+LX "," B-LY-H
	, DrawFace(Points, 0xffd40055, G)

UpdateLayeredWindow(hwnd1, hdc, TopX, TopY, WinWidth, WinHeight)

SelectObject(hdc, obm), DeleteObject(hbm), DeleteDC(hdc)
	, Gdip_DeleteGraphics(G)
return

DrawFace(Points, Color, G) {
	pBrush := Gdip_BrushCreateSolid(Color)
		, Gdip_FillPolygon(G, pBrush, Points, 1)
		, Gdip_DeleteBrush(pBrush)
	return
}

Esc::
Exit:
Gdip_Shutdown(pToken)
ExitApp
```



## AWK


```AWK

# syntax: GAWK -f DRAW_A_CUBOID.AWK [-v x=?] [-v y=?] [-v z=?]
# example: GAWK -f DRAW_A_CUBOID.AWK -v x=12 -v y=4 -v z=6
# converted from VBSCRIPT
BEGIN {
    init_sides()
    draw_cuboid(2,3,4)
    draw_cuboid(1,1,1)
    draw_cuboid(6,2,1)
    exit (errors == 0) ? 0 : 1
}
function draw_cuboid(nx,ny,nz,  esf,i,i_max,j,j_max,lx,ly,lz) {
    esf = errors # errors so far
    if (nx !~ /^[0-9]+$/ || nx <= 0) { error(nx,ny,nz,1) }
    if (ny !~ /^[0-9]+$/ || ny <= 0) { error(nx,ny,nz,2) }
    if (nz !~ /^[0-9]+$/ || nz <= 0) { error(nx,ny,nz,3) }
    if (errors > esf) { return }
    lx = x * nx
    ly = y * ny
    lz = z * nz
# define the array size
    i_max = ly + lz
    j_max = lx + ly
    delete arr
    printf("%s %s %s (%d rows x %d columns)\n",nx,ny,nz,i_max+1,j_max+1)
# draw lines
    for (i=0; i<=nz-1; i++) { draw_line(lx,0,z*i,"-") }
    for (i=0; i<=ny; i++)   { draw_line(lx,y*i,lz+y*i,"-") }
    for (i=0; i<=nx-1; i++) { draw_line(lz,x*i,0,"|") }
    for (i=0; i<=ny; i++)   { draw_line(lz,lx+y*i,y*i,"|") }
    for (i=0; i<=nz-1; i++) { draw_line(ly,lx,z*i,"/") }
    for (i=0; i<=nx; i++)   { draw_line(ly,x*i,lz,"/") }
# output the cuboid
    for (i=i_max; i>=0; i--) {
      for (j=0; j<=j_max; j++) {
        printf("%1s",arr[i,j])
      }
      printf("\n")
    }
}
function draw_line(n,x,y,c,  dx,dy,i,xi,yi) {
    if      (c == "-") { dx = 1 ; dy = 0 }
    else if (c == "|") { dx = 0 ; dy = 1 }
    else if (c == "/") { dx = 1 ; dy = 1 }
    for (i=0; i<=n; i++) {
      xi = x + i * dx
      yi = y + i * dy
      arr[yi,xi] = (arr[yi,xi] ~ /^ ?$/) ? c : "+"
    }
}
function error(x,y,z,arg) {
    printf("error: '%s,%s,%s' argument %d is invalid\n",x,y,z,arg)
    errors++
}
function init_sides() {
# to change the defaults on the command line use: -v x=? -v y=? -v z=?
    if (x+0 < 2) { x = 6 } # top
    if (y+0 < 2) { y = 2 } # right
    if (z+0 < 2) { z = 3 } # front
}

```

```txt

2 3 4 (19 rows x 19 columns)
      +-----+-----+
     /     /     /|
    +-----+-----+ |
   /     /     /| +
  +-----+-----+ |/|
 /     /     /| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/
|     |     |/| +
+-----+-----+ |/
|     |     | +
|     |     |/
+-----+-----+
1 1 1 (6 rows x 9 columns)
  +-----+
 /     /|
+-----+ |
|     | +
|     |/
+-----+
6 2 1 (8 rows x 41 columns)
    +-----+-----+-----+-----+-----+-----+
   /     /     /     /     /     /     /|
  +-----+-----+-----+-----+-----+-----+ |
 /     /     /     /     /     /     /| +
+-----+-----+-----+-----+-----+-----+ |/
|     |     |     |     |     |     | +
|     |     |     |     |     |     |/
+-----+-----+-----+-----+-----+-----+

```



## Brlcad

In brlcad, we use the rpp (rectangular parallelepiped) primitive to create the cuboid. This defines the cuboid area using the parameters xmin,xmax,ymin,ymax,zmin,zmax


```brlcad
opendb cuboid.g y            # Create a database to hold our shapes
units cm                     # Set the unit of measure
in cuboid.s rpp 0 2 0 3 0 4  # Create a 2 x 3 x 4 cuboid named cuboid.s
```



## BBC BASIC

Uses BBC BASIC's native parallelogram plot.

```bbcbasic
      ORIGIN 100, 100
      PROCcuboid(200, 300, 400)
      END

      DEF PROCcuboid(x, y, z)
      MOVE 0, 0 : MOVE 0, y
      GCOL 1 : PLOT 117, x, y
      GCOL 2 : PLOT 117, x + z * 0.4, y + z * 0.4
      GCOL 4 : PLOT 117, x + z * 0.4, z * 0.4
      ENDPROC

```

[[File:Cuboid_BBC.gif]]


## Befunge


Given a width, height, and depth, this produces an approximate isometric representation of the shape using ASCII art.


```befunge
"  :htdiW">:#,_>&>00p" :thgieH">:#,_>&>:10p0"  :htpeD">:#,_$>&>:20p55+,+:1`*:vv
v\-*`0:-g01\++*`\0:-\-1g01:\-*`0:-g02\+*`\0:-\-1g02<:::::<\g3`\g01:\1\+55\1-1_v
>":"\1\:20g\`!3g:30p\00g2*\::20g\`\20g1-\`+1+3g\1\30g\:20g-::0\`\2*1+*-\48*\:^v
/\_ @_\#!:!#$>#$_\#!:,#-\#1                         <+1\<*84g02"_"+1*2g00+551$<
```


```txt
Width:  2
Height: 3
Depth:  4

    _____
   /    /\
  /    /::\
 /    /::::\
/____/:::::/
\\\\\\::::/
 \\\\\\::/
  \\\\\\/
```



## C

Code works fine but only '.' and ':' characters show up on the cuboid.

```c

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

const char *shades = ".:!*oe&#%@";

void vsub(double *v1, double *v2, double *s) {
	s[0] = v1[0] - v2[0];
	s[1] = v1[1] - v2[1];
	s[2] = v1[2] - v2[2];
}

double normalize(double * v) {
        double len = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
        v[0] /= len; v[1] /= len; v[2] /= len;
	return len;
}

double dot(double *x, double *y) {
        return x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
}

double * cross(double x[3], double y[3], double s[3]) {
	s[0] = x[1] * y[2] - x[2] * y[1];
	s[1] = x[2] * y[0] - x[0] * y[2];
	s[2] = x[0] * y[1] - x[1] * y[0];
	return s;
}

double* madd(double *x, double *y, double d, double *r) {
	r[0] = x[0] + y[0] * d;
	r[1] = x[1] + y[1] * d;
	r[2] = x[2] + y[2] * d;
	return r;
}

double v000[] = { -4, -3, -2 };
double v100[] = {  4, -3, -2 };
double v010[] = { -4,  3, -2 };
double v110[] = {  4,  3, -2 };
double v001[] = { -4, -3,  2 };
double v101[] = {  4, -3,  2 };
double v011[] = { -4,  3,  2 };
double v111[] = {  4,  3,  2 };

typedef struct {
	double * v[4];
	double norm[3];
} face_t;

face_t f[] = {
	{ { v000, v010, v110, v100 }, {  0,  0, -1 } },
	{ { v001, v011, v111, v101 }, {  0,  0,  1 } },
	{ { v000, v010, v011, v001 }, { -1,  0,  0 } },
	{ { v100, v110, v111, v101 }, {  1,  0,  0 } },
	{ { v000, v100, v101, v001 }, {  0, -1,  0 } },
	{ { v010, v110, v111, v011 }, {  0,  1,  0 } },
};

int in_range(double x, double x0, double x1) {
	return (x - x0) * (x - x1) <= 0;
}

int face_hit(face_t *face, double src[3], double dir[3], double hit[3], double *d)
{
	int i;
	double dist;
	for (i = 0; i < 3; i++)
		if (face->norm[i])
			dist = (face->v[0][i] - src[i]) / dir[i];

	madd(src, dir, dist, hit);
	*d = fabs(dot(dir, face->norm) * dist);

	if (face->norm[0]) {
		return  in_range(hit[1], face->v[0][1], face->v[2][1]) &&
			in_range(hit[2], face->v[0][2], face->v[2][2]);
	}
	else if (face->norm[1]) {
		return  in_range(hit[0], face->v[0][0], face->v[2][0]) &&
			in_range(hit[2], face->v[0][2], face->v[2][2]);
	}
	else if (face->norm[2]) {
		return  in_range(hit[0], face->v[0][0], face->v[2][0]) &&
			in_range(hit[1], face->v[0][1], face->v[2][1]);
	}
	return 0;
}

int main()
{
	int i, j, k;
	double eye[3] = { 7, 7, 6 };
	double dir[3] = { -1, -1, -1 }, orig[3] = {0, 0, 0};
	double hit[3], dx[3], dy[3] = {0, 0, 1}, proj[3];
	double d, *norm, dbest, b;
	double light[3] = { 6, 8, 6 }, ldist[3], decay, strength = 10;

 	normalize(cross(eye, dy, dx));
	normalize(cross(eye, dx, dy));

	for (i = -10; i <= 17; i++) {
		for (j = -35; j < 35; j++) {
			vsub(orig, orig, proj);
			madd(madd(proj, dx, j / 6., proj), dy, i/3., proj);
			vsub(proj, eye, dir);
			dbest = 1e100;
			norm = 0;
		 	for (k = 0; k < 6; k++) {
				if (!face_hit(f + k, eye, dir, hit, &d)) continue;
				if (dbest > d) {
					dbest = d;
					norm = f[k].norm;
				}
			}

			if (!norm) {
				putchar(' ');
				continue;
			}

			vsub(light, hit, ldist);
			decay = normalize(ldist);
			b = dot(norm, ldist) / decay * strength;
			if (b < 0) b = 0;
			else if (b > 1) b = 1;
			b += .2;
			if (b > 1) b = 0;
			else b = 1 - b;
			putchar(shades[(int)(b * (sizeof(shades) - 2))]);
		}
		putchar('\n');
	}

        return 0;
}
```


Output :

```txt

                                .
                        ................
                ...............................
         .............................................
     ........................................................
     ...............................................................
      ..............................................................::
      ...........................................................::::
       .......................................................:::::::
       .....................................................::::::::
        .................................................::::::::::
        :..............................................::::::::::::
         :............................................::::::::::::
         ::..........................................:::::::::::::
           :........................................:::::::::::::
             ......................................::::::::::::::
               ....................................:::::::::::::
                 .................................::::::::::::
                    ............................::::::::::::
                      .........................:::::::::::
                        ......................::::::::::
                          ...................:::::::::
                             ..............:::::::::
                               ...........:::::::::
                                 .........:::::::
                                   .......:::::
                                     .....:::
                                        .::

```



## C++


This code needs the BGI for Windows available at [http://www.cs.colorado.edu/~main/cs1300/doc/bgi/bgi.html Colorado State University].


```cpp

#include<graphics.h>
#include<iostream>

int main()
{
    int k;
    initwindow(1500,810,"Rosetta Cuboid");

    do{
       std::cout<<"Enter ratio of sides ( 0 or -ve to exit) : ";
       std::cin>>k;

       if(k>0){
                bar3d(100, 100, 100 + 2*k, 100 + 4*k, 3*k, 1);
       }
       }while(k>0);

    return 0;
}


```

[[Image:Box.jpg]]


## C#

```c#
using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace Cuboid
{
    public partial class Form1 : Form
    {
        double[][] nodes = {
            new double[] {-1, -1, -1}, new double[] {-1, -1, 1}, new double[] {-1, 1, -1},
            new double[] {-1, 1, 1}, new double[] {1, -1, -1}, new double[] {1, -1, 1},
            new double[] {1, 1, -1}, new double[] {1, 1, 1} };

        int[][] edges = {
            new int[] {0, 1}, new int[] {1, 3}, new int[] {3, 2}, new int[] {2, 0}, new int[] {4, 5},
            new int[] {5, 7}, new int[] {7, 6}, new int[] {6, 4}, new int[] {0, 4}, new int[] {1, 5},
            new int[] {2, 6}, new int[] {3, 7}};

        private int mouseX;
        private int prevMouseX;
        private int prevMouseY;
        private int mouseY;

        public Form1()
        {
            Width = Height = 640;
            StartPosition = FormStartPosition.CenterScreen;
            SetStyle(
                ControlStyles.AllPaintingInWmPaint |
                ControlStyles.UserPaint |
                ControlStyles.DoubleBuffer,
                true);

            MouseMove += (s, e) =>
            {
                prevMouseX = mouseX;
                prevMouseY = mouseY;
                mouseX = e.X;
                mouseY = e.Y;

                double incrX = (mouseX - prevMouseX) * 0.01;
                double incrY = (mouseY - prevMouseY) * 0.01;

                RotateCuboid(incrX, incrY);
                Refresh();
            };

            MouseDown += (s, e) =>
            {
                mouseX = e.X;
                mouseY = e.Y;
            };

            Scale(80, 120, 160);
            RotateCuboid(Math.PI / 5, Math.PI / 9);
        }

        private void RotateCuboid(double angleX, double angleY)
        {
            double sinX = Math.Sin(angleX);
            double cosX = Math.Cos(angleX);

            double sinY = Math.Sin(angleY);
            double cosY = Math.Cos(angleY);

            foreach (var node in nodes)
            {
                double x = node[0];
                double y = node[1];
                double z = node[2];

                node[0] = x * cosX - z * sinX;
                node[2] = z * cosX + x * sinX;

                z = node[2];

                node[1] = y * cosY - z * sinY;
                node[2] = z * cosY + y * sinY;
            }
        }

        private void Scale(int v1, int v2, int v3)
        {
            foreach (var item in nodes)
            {
                item[0] *= v1;
                item[1] *= v2;
                item[2] *= v3;
            }
        }

        protected override void OnPaint(PaintEventArgs args)
        {
            var g = args.Graphics;
            g.SmoothingMode = SmoothingMode.HighQuality;
            g.Clear(Color.White);

            g.TranslateTransform(Width / 2, Height / 2);

            foreach (var edge in edges)
            {
                double[] xy1 = nodes[edge[0]];
                double[] xy2 = nodes[edge[1]];
                g.DrawLine(Pens.Black, (int)Math.Round(xy1[0]), (int)Math.Round(xy1[1]),
                        (int)Math.Round(xy2[0]), (int)Math.Round(xy2[1]));
            }

            foreach (var node in nodes)
            {
                g.FillEllipse(Brushes.Black, (int)Math.Round(node[0]) - 4,
                    (int)Math.Round(node[1]) - 4, 8, 8);
            }
        }
    }
}
```



## Clojure

```clojure

(use 'quil.core)

(def w 500)
(def h 400)

(defn setup []
  (background 0))

(defn draw []
  (push-matrix)
  (translate (/ w 2) (/ h 2) 0)
  (rotate-x 0.7)
  (rotate-z 0.5)
  (box 100 150 200)  ; 2x3x4 relative dimensions
  (pop-matrix))

(defsketch main
  :title "cuboid"
  :setup setup
  :size [w h]
  :draw draw
  :renderer :opengl)

```


[http://i.imgur.com/7io7wo4.png]


## D

```d
import std.stdio, std.array;

void printCuboid(in int dx, in int dy, in int dz) {
    static cline(in int n, in int dx, in int dy, in string cde) {
        writef("%*s", n+1, cde[0 .. 1]);
        write(cde[1 .. 2].replicate(9*dx - 1));
        write(cde[0]);
        writefln("%*s", dy+1, cde[2 .. $]);
    }

    cline(dy+1, dx, 0, "+-");
    foreach (i; 1 .. dy+1)
        cline(dy-i+1, dx, i-1, "/ |");
    cline(0, dx, dy, "+-|");
    foreach (_; 0 .. 4*dz - dy - 2)
        cline(0, dx, dy, "| |");
    cline(0, dx, dy, "| +");
    foreach_reverse (i; 0 .. dy)
        cline(0, dx, i, "| /");
    cline(0, dx, 0, "+-\n");
}

void main() {
    printCuboid(2, 3, 4);
    printCuboid(1, 1, 1);
    printCuboid(6, 2, 1);
}
```

```txt
    +-----------------+
   /                 /|
  /                 / |
 /                 /  |
+-----------------+   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   +
|                 |  /
|                 | /
|                 |/
+-----------------+

  +--------+
 /        /|
+--------+ |
|        | |
|        | +
|        |/
+--------+

   +-----------------------------------------------------+
  /                                                     /|
 /                                                     / |
+-----------------------------------------------------+  |
|                                                     |  +
|                                                     | /
|                                                     |/
+-----------------------------------------------------+
```



## Elixir

```elixir
defmodule Cuboid do
  @x 6
  @y 2
  @z 3
  @dir %{-: {1,0}, |: {0,1}, /: {1,1}}

  def draw(nx, ny, nz) do
    IO.puts "cuboid #{nx} #{ny} #{nz}:"
    {x, y, z} = {@x*nx, @y*ny, @z*nz}
    area = Map.new
    area = Enum.reduce(0..nz-1, area, fn i,acc -> draw_line(acc, x,      0,   @z*i, :-) end)
    area = Enum.reduce(0..ny,   area, fn i,acc -> draw_line(acc, x,   @y*i, z+@y*i, :-) end)
    area = Enum.reduce(0..nx-1, area, fn i,acc -> draw_line(acc, z,   @x*i,      0, :|) end)
    area = Enum.reduce(0..ny,   area, fn i,acc -> draw_line(acc, z, x+@y*i,   @y*i, :|) end)
    area = Enum.reduce(0..nz-1, area, fn i,acc -> draw_line(acc, y,      x,   @z*i, :/) end)
    area = Enum.reduce(0..nx,   area, fn i,acc -> draw_line(acc, y,   @x*i,      z, :/) end)
    Enum.each(y+z..0, fn j ->
      IO.puts Enum.map_join(0..x+y, fn i -> Map.get(area, {i,j}, " ") end)
    end)
  end

  defp draw_line(area, n, sx, sy, c) do
    {dx, dy} = Map.get(@dir, c)
    draw_line(area, n, sx, sy, c, dx, dy)
  end

  defp draw_line(area, n, _, _, _, _, _) when n<0, do: area
  defp draw_line(area, n, i, j, c, dx, dy) do
    Map.update(area, {i,j}, c, fn _ -> :+ end)
    |> draw_line(n-1, i+dx, j+dy, c, dx, dy)
  end
end

Cuboid.draw(2,3,4)
Cuboid.draw(1,1,1)
Cuboid.draw(2,4,1)
Cuboid.draw(4,2,1)
```


```txt

cuboid 2 3 4:
      +-----+-----+
     /     /     /|
    +-----+-----+ |
   /     /     /| +
  +-----+-----+ |/|
 /     /     /| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/
|     |     |/| +
+-----+-----+ |/
|     |     | +
|     |     |/
+-----+-----+
cuboid 1 1 1:
  +-----+
 /     /|
+-----+ |
|     | +
|     |/
+-----+
cuboid 2 4 1:
        +-----+-----+
       /     /     /|
      +-----+-----+ |
     /     /     /| +
    +-----+-----+ |/
   /     /     /| +
  +-----+-----+ |/
 /     /     /| +
+-----+-----+ |/
|     |     | +
|     |     |/
+-----+-----+
cuboid 4 2 1:
    +-----+-----+-----+-----+
   /     /     /     /     /|
  +-----+-----+-----+-----+ |
 /     /     /     /     /| +
+-----+-----+-----+-----+ |/
|     |     |     |     | +
|     |     |     |     |/
+-----+-----+-----+-----+

```



## Factor

```factor
USING: classes.struct kernel raylib.ffi ;

640 480 "cuboid" init-window

S{ Camera3D
    { position S{ Vector3 f 4.5 4.5 4.5 } }
    { target S{ Vector3 f 0 0 0 } }
    { up S{ Vector3 f 0 1 0 } }
    { fovy 45.0 }
    { type 0 }
}

60 set-target-fps

[ window-should-close ] [
    begin-drawing
        BLACK clear-background dup
        begin-mode-3d
            S{ Vector3 f 0 0 0 } 2 3 4 LIME draw-cube-wires
        end-mode-3d
    end-drawing
] until drop close-window
```

[https://i.imgur.com/JQMPjhk.png]


## Go

```go
package main

import "fmt"

func cuboid(dx, dy, dz int) {
    fmt.Printf("cuboid %d %d %d:\n", dx, dy, dz)
    cubLine(dy+1, dx, 0, "+-")
    for i := 1; i <= dy; i++ {
        cubLine(dy-i+1, dx, i-1, "/ |")
    }
    cubLine(0, dx, dy, "+-|")
    for i := 4*dz - dy - 2; i > 0; i-- {
        cubLine(0, dx, dy, "| |")
    }
    cubLine(0, dx, dy, "| +")
    for i := 1; i <= dy; i++ {
        cubLine(0, dx, dy-i, "| /")
    }
    cubLine(0, dx, 0, "+-\n")
}

func cubLine(n, dx, dy int, cde string) {
    fmt.Printf("%*s", n+1, cde[:1])
    for d := 9*dx - 1; d > 0; d-- {
        fmt.Print(cde[1:2])
    }
    fmt.Print(cde[:1])
    fmt.Printf("%*s\n", dy+1, cde[2:])
}

func main() {
    cuboid(2, 3, 4)
    cuboid(1, 1, 1)
    cuboid(6, 2, 1)
}
```

```txt

cuboid 2 3 4:
    +-----------------+
   /                 /|
  /                 / |
 /                 /  |
+-----------------+   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   +
|                 |  /
|                 | /
|                 |/
+-----------------+

cuboid 1 1 1:
  +--------+
 /        /|
+--------+ |
|        | |
|        | +
|        |/
+--------+

cuboid 6 2 1:
   +-----------------------------------------------------+
  /                                                     /|
 /                                                     / |
+-----------------------------------------------------+  |
|                                                     |  +
|                                                     | /
|                                                     |/
+-----------------------------------------------------+

```


## Haskell


```haskell
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

-- Draw a cuboid.  Its vertices are those of a unit cube, which is then scaled
-- to the required dimensions.  We only specify the visible faces, each of
-- which is composed of two triangles.  The faces are rotated into position and
-- rendered with a perspective transformation.

type Fl = GLfloat

cuboid :: IO ()
cuboid = do
  color red   ; render front
  color green ; render side
  color blue  ; render top

red,green,blue :: Color4 GLfloat
red   = Color4 1 0 0 1
green = Color4 0 1 0 1
blue  = Color4 0 0 1 1

render :: [(Fl, Fl, Fl)] -> IO ()
render = renderPrimitive TriangleStrip . mapM_ toVertex
  where toVertex (x,y,z) = vertex $ Vertex3 x y z

front,side,top :: [(Fl,Fl,Fl)]
front = vertices [0,1,2,3]
side  = vertices [4,1,5,3]
top   = vertices [3,2,5,6]

vertices :: [Int] -> [(Fl,Fl,Fl)]
vertices = map (verts !!)

verts :: [(Fl,Fl,Fl)]
verts = [(0,0,1), (1,0,1), (0,1,1), (1,1,1), (1,0,0), (1,1,0), (0,1,0)]

transform :: IO ()
transform = do
  translate $ Vector3 0 0 (-10 :: Fl)
  rotate (-14) $ Vector3 0 0 (1 :: Fl)
  rotate (-30) $ Vector3 0 1 (0 :: Fl)
  rotate   25  $ Vector3 1 0 (0 :: Fl)
  scale 2 3 (4 :: Fl)
  translate $ Vector3 (-0.5) (-0.5) (-0.5 :: Fl)

display :: IO ()
display = do
  clear [ColorBuffer]
  perspective 40 1 1 (15 :: GLdouble)
  transform
  cuboid
  flush

main :: IO ()
main = do
  let name = "Cuboid"
  initialize name []
  createWindow name
  displayCallback $= display
  mainLoop
```

[[Image:CuboidHaskell.png]]


## J

Hack alert! I haven't even bothered to center the display.
With larger resolutions and the viewmat script, this code can generate reasonable 2D displays with a different color for each face.

```j
   vectors =. ((% +/&.:*:"1) _1 1 0,:_1 _1 3) +/@:*"1/~ 2 3 4*=i.3
   ' .*o' {~  +/ 1 2 3* (|:"2 -."_ 1~ vectors) ([:*./ 1 = 0 1 I. %.~)"_ 1"_1 _ ]4j21 ,~"0/&:i: 4j41







                       oooo
                  ooooooooooooo
              oooooooooooooo....
              *****oooo.........
              *******...........
              *******...........
              *******...........
              *******...........
              *******...........
              *******...........
              *******...........
              *******.........
                *****.....


```



## Java

[[File:cuboid_java.png|200px|thumb|right]]
```java
import java.awt.*;
import java.awt.event.*;
import static java.lang.Math.*;
import javax.swing.*;

public class Cuboid extends JPanel {
    double[][] nodes = {{-1, -1, -1}, {-1, -1, 1}, {-1, 1, -1}, {-1, 1, 1},
    {1, -1, -1}, {1, -1, 1}, {1, 1, -1}, {1, 1, 1}};

    int[][] edges = {{0, 1}, {1, 3}, {3, 2}, {2, 0}, {4, 5}, {5, 7}, {7, 6},
    {6, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};

    int mouseX, prevMouseX, mouseY, prevMouseY;

    public Cuboid() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);

        scale(80, 120, 160);
        rotateCube(PI / 5, PI / 9);

        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                mouseX = e.getX();
                mouseY = e.getY();
            }
        });

        addMouseMotionListener(new MouseAdapter() {
            @Override
            public void mouseDragged(MouseEvent e) {
                prevMouseX = mouseX;
                prevMouseY = mouseY;
                mouseX = e.getX();
                mouseY = e.getY();

                double incrX = (mouseX - prevMouseX) * 0.01;
                double incrY = (mouseY - prevMouseY) * 0.01;

                rotateCube(incrX, incrY);
                repaint();
            }
        });
    }

    private void scale(double sx, double sy, double sz) {
        for (double[] node : nodes) {
            node[0] *= sx;
            node[1] *= sy;
            node[2] *= sz;
        }
    }

    private void rotateCube(double angleX, double angleY) {
        double sinX = sin(angleX);
        double cosX = cos(angleX);

        double sinY = sin(angleY);
        double cosY = cos(angleY);

        for (double[] node : nodes) {
            double x = node[0];
            double y = node[1];
            double z = node[2];

            node[0] = x * cosX - z * sinX;
            node[2] = z * cosX + x * sinX;

            z = node[2];

            node[1] = y * cosY - z * sinY;
            node[2] = z * cosY + y * sinY;
        }
    }

    void drawCube(Graphics2D g) {
        g.translate(getWidth() / 2, getHeight() / 2);

        for (int[] edge : edges) {
            double[] xy1 = nodes[edge[0]];
            double[] xy2 = nodes[edge[1]];
            g.drawLine((int) round(xy1[0]), (int) round(xy1[1]),
                    (int) round(xy2[0]), (int) round(xy2[1]));
        }

        for (double[] node : nodes) {
            g.fillOval((int) round(node[0]) - 4, (int) round(node[1]) - 4, 8, 8);
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawCube(g);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Cuboid");
            f.setResizable(false);
            f.add(new Cuboid(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

```javascript
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <style>
        canvas {
            background-color: black;
        }
    </style>
</head>
<body>
    <canvas></canvas>
    <script>
        var canvas = document.querySelector("canvas");
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;

        var g = canvas.getContext("2d");

        canvas.addEventListener("mousemove", function (event) {
            prevMouseX = mouseX;
            prevMouseY = mouseY;
            mouseX = event.x;
            mouseY = event.y;

            var incrX = (mouseX - prevMouseX) * 0.01;
            var incrY = (mouseY - prevMouseY) * 0.01;

            rotateCuboid(incrX, incrY);
            drawCuboid();
        });

        var nodes = [[-1, -1, -1], [-1, -1, 1], [-1, 1, -1], [-1, 1, 1],
        [1, -1, -1], [1, -1, 1], [1, 1, -1], [1, 1, 1]];

        var edges = [[0, 1], [1, 3], [3, 2], [2, 0], [4, 5], [5, 7], [7, 6],
        [6, 4], [0, 4], [1, 5], [2, 6], [3, 7]];

        var mouseX = 0, prevMouseX, mouseY = 0, prevMouseY;

        function scale(factor0, factor1, factor2) {
            nodes.forEach(function (node) {
                node[0] *= factor0;
                node[1] *= factor1;
                node[2] *= factor2;
            });
        }

        function rotateCuboid(angleX, angleY) {

            var sinX = Math.sin(angleX);
            var cosX = Math.cos(angleX);

            var sinY = Math.sin(angleY);
            var cosY = Math.cos(angleY);

            nodes.forEach(function (node) {
                var x = node[0];
                var y = node[1];
                var z = node[2];

                node[0] = x * cosX - z * sinX;
                node[2] = z * cosX + x * sinX;

                z = node[2];

                node[1] = y * cosY - z * sinY;
                node[2] = z * cosY + y * sinY;
            });
        }

        function drawCuboid() {
            g.save();

            g.clearRect(0, 0, canvas.width, canvas.height);
            g.translate(canvas.width / 2, canvas.height / 2);
            g.strokeStyle = "#FFFFFF";
            g.beginPath();

            edges.forEach(function (edge) {
                var p1 = nodes[edge[0]];
                var p2 = nodes[edge[1]];
                g.moveTo(p1[0], p1[1]);
                g.lineTo(p2[0], p2[1]);
            });

            g.closePath();
            g.stroke();

            g.restore();
        }

        scale(80, 120, 160);
        rotateCuboid(Math.PI / 5, Math.PI / 9);
    </script>

</body>
</html>
```



## Julia


###  ASCII Art

```julia
_pr(t::Dict, x::Int, y::Int, z::Int) = join((rstrip(join(t[(n, m)] for n in range(0, 3+x+z))) for m in reverse(range(0, 3+y+z))), "\n")

function cuboid(x::Int, y::Int, z::Int)
    t = Dict((n, m) => " " for n in range(0, 3 + x + z), m in range(0, 3 + y + z))
    xrow = vcat("+", collect("$(i % 10)" for i in range(0, x)), "+")
    for (i, ch) in enumerate(xrow) t[(i, 0)] = t[(i, 1+y)] = t[(1+z+i, 2+y+z)] = ch end
    yrow = vcat("+", collect("$(j % 10)" for j in range(0, y)), "+")
    for (j, ch) in enumerate(yrow) t[(0, j)] = t[(x+1, j)] = t[(2+x+z, 1+z+j)] = ch end
    zdep = vcat("+", collect("$(k % 10)" for k in range(0, y)), "+")
    for (k, ch) in enumerate(xrow) t[(k, 1+y+k)] = t[(1+x+k, 1+y+k)] = t[(1+x+k, k)] = ch end

    return _pr(t, x, y, z)
end

for (x, y, z) in [(2, 3, 4), (3, 4, 2), (4, 2, 3), (5, 5, 6)]
    println("\nCUBOID($x, $y, $z)\n")
    println(cuboid(x, y, z))
end
```


```txt

CUBOID(2, 3, 4)

      +02
    +  +1
   1  1 0
  0  0  +
++ ++
2+02+  +
1  1  1
0  0 0
+  ++
 +01+

CUBOID(3, 4, 2)

   1+011
  0   02
++  ++ 1
3+013+ 0
2   2  +
1   1  1
0   0 0
+   ++
 +012+

CUBOID(4, 2, 3)

    2+0122
   1    10
  0    0 +
++   ++  2
1+0121+ 1
0    0 0
+    ++
 +0123+

CUBOID(5, 5, 6)

       ++0123+
      4     43
     3     3 2
    2     2  1
   1     1   0
  0     0    +
++    ++     +
4+01234+    4
3     3    3
2     2   2
1     1  1
0     0 0
+     ++
 +01234+
```



## Kotlin

```scala
// version 1.1

import java.awt.*
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.*

class Cuboid: JPanel() {
    private val nodes = arrayOf(
        doubleArrayOf(-1.0, -1.0, -1.0),
        doubleArrayOf(-1.0, -1.0,  1.0),
        doubleArrayOf(-1.0,  1.0, -1.0),
        doubleArrayOf(-1.0,  1.0,  1.0),
        doubleArrayOf( 1.0, -1.0, -1.0),
        doubleArrayOf( 1.0, -1.0,  1.0),
        doubleArrayOf( 1.0,  1.0, -1.0),
        doubleArrayOf( 1.0,  1.0,  1.0)
    )
    private val edges = arrayOf(
        intArrayOf(0, 1),
        intArrayOf(1, 3),
        intArrayOf(3, 2),
        intArrayOf(2, 0),
        intArrayOf(4, 5),
        intArrayOf(5, 7),
        intArrayOf(7, 6),
        intArrayOf(6, 4),
        intArrayOf(0, 4),
        intArrayOf(1, 5),
        intArrayOf(2, 6),
        intArrayOf(3, 7)
    )

    private var mouseX: Int = 0
    private var prevMouseX: Int = 0
    private var mouseY: Int = 0
    private var prevMouseY: Int = 0

    init {
        preferredSize = Dimension(640, 640)
        background = Color.white
        scale(80.0, 120.0, 160.0)
        rotateCube(Math.PI / 5.0, Math.PI / 9.0)
        addMouseListener(object: MouseAdapter() {
            override fun mousePressed(e: MouseEvent) {
                mouseX = e.x
                mouseY = e.y
            }
        })

        addMouseMotionListener(object: MouseAdapter() {
            override fun mouseDragged(e: MouseEvent) {
                prevMouseX = mouseX
                prevMouseY = mouseY
                mouseX = e.x
                mouseY = e.y
                val incrX = (mouseX - prevMouseX) * 0.01
                val incrY = (mouseY - prevMouseY) * 0.01
                rotateCube(incrX, incrY)
                repaint()
            }
        })
    }

    private fun scale(sx: Double, sy: Double, sz: Double) {
        for (node in nodes) {
            node[0] *= sx
            node[1] *= sy
            node[2] *= sz
        }
    }

    private fun rotateCube(angleX: Double, angleY: Double) {
        val sinX = Math.sin(angleX)
        val cosX = Math.cos(angleX)
        val sinY = Math.sin(angleY)
        val cosY = Math.cos(angleY)
        for (node in nodes) {
            val x = node[0]
            val y = node[1]
            var z = node[2]
            node[0] = x * cosX - z * sinX
            node[2] = z * cosX + x * sinX
            z = node[2]
            node[1] = y * cosY - z * sinY
            node[2] = z * cosY + y * sinY
        }
    }

    private fun drawCube(g: Graphics2D) {
        g.translate(width / 2, height / 2)
        for (edge in edges) {
            val xy1 = nodes[edge[0]]
            val xy2 = nodes[edge[1]]
            g.drawLine(Math.round(xy1[0]).toInt(), Math.round(xy1[1]).toInt(),
                       Math.round(xy2[0]).toInt(), Math.round(xy2[1]).toInt())
        }
        for (node in nodes) {
            g.fillOval(Math.round(node[0]).toInt() - 4, Math.round(node[1]).toInt() - 4, 8, 8)
        }
    }

    override public fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.color = Color.blue
        drawCube(g)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "Cuboid"
        f.isResizable = false
        f.add(Cuboid(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.isVisible = true
    }
}
```



## Liberty BASIC

Text solution

```lb

Call cuboid 1,3,4

End

Sub cuboid width, height, depth
    wd=width*7+2: hi=height*3: dp=depth
    For i=1 To wd-2
        w$=w$+"-":h$=h$+" "
    Next
    w$="+"+w$+"+":d$="/"+h$+"/":h$="|"+h$+"|"
    px=dp+2:py=1:Locate dp+2,py:Print w$;
    For i=2 To hi+1
        Locate wd+dp+1,i:Print"|";
    Next
    Locate wd+dp+1, i: Print "+";
    For i=dp+1 To 1 Step -1
        py=py+1:Locate i,py:Print d$;
    Next
    For i=1 To dp
        Locate wd+(dp+1)-i,hi+d+2+i:Print "/";
    Next
    Locate 1, dp+2: Print w$;
    For i=dp+3 To hi+dp+2
        Locate 1,i:Print h$;
    Next
    Locate 1, dp+hi+3: Print w$
End Sub

```

```txt

     +-------+
    /       /|
   /       / |
  /       /  |
 /       /   |
+-------+    |
|       |    |
|       |    |
|       |    |
|       |    |
|       |    +
|       |   /
|       |  /
|       | /
|       |/
+-------+

```

Graphic solution

```lb

NoMainWin
Global sw, sh
sw = 400: sh = 400
WindowWidth = sw+6
WindowHeight= sh+32
Open "[RC] Draw Cuboid" For graphics_nsb_nf As #g
#g "Down; Fill black; TrapClose [xit]"
#g "when leftButtonDown [xit]"

Call drawCuboid 3,4,5

Wait

[xit]
Close #g
End

Sub drawCuboid width, height, depth
    wd = width*50
    ht = height*50
    dp = depth*20
    sx = Int((sw-(wd+dp))/2)
    sy = Int((sh-(ht-dp))/2)
    #g "Color 0 128 255; BackColor 0 128 255"
    #g "Place ";sx;" ";sy
    #g "boxFilled ";sx+wd;" ";sy+ht
    x1 = sx+dp : y1 = sy-dp
    x2 = x1+wd-1 : y2 = y1+1
    #g "Color 0 64 128"
    Call triFill sx,sy, x1,y1, x2,y2
    Call triFill sx,sy, x2,y2, sx+wd, sy
    #g "Color 0 96 192"
    x3 = x2: y3 = y2+ht
    Call triFill x2,y2, x3,y3, sx+wd-1, sy+ht-1
    Call triFill x2,y2, sx+wd-1, sy+ht-1, sx+wd-1, sy
    #g "Color white;BackColor black;Place 5 20"
    #g "\Size: ";width;", ";height;", ";depth
End Sub

Sub triFill x1,y1, x2,y2, x3,y3
    If x2<x1 Then x=x2: y=y2: x2=x1: y2=y1: x1=x: y1=y
    If x3<x1 Then x=x3: y=y3: x3=x1: y3=y1: x1=x: y1=y
    If x3<x2 Then x=x3: y=y3: x3=x2: y3=y2: x2=x: y2=y
    If x1<>x3 Then slope1=(y3-y1)/(x3-x1)
    length=x2-x1
    If length<>0 Then
        slope2=(y2-y1)/(x2-x1)
        For x = 0 To length
            #g "Line ";Int(x+x1);" ";Int(x*slope1+y1);" ";Int(x+x1);" ";Int(x*slope2+y1)
        Next
    End If
    y = length*slope1+y1 :length=x3-x2
    If length<>0 Then
        slope3=(y3-y2)/(x3-x2)
        For x = 0 To length
            #g "Line ";Int(x+x2);" ";Int(x*slope1+y);" ";Int(x+x2);" ";Int(x*slope3+y2)
        Next
    End If
End Sub

```



## Logo

In Logo, we can use the ''perspective'' function to make drawing 3D-objects easier.
Simple implementation, just moving to the appropriate points every time.

```logo
to cuboid :l1 :l2 :l3
cs perspective ;making the room ready to use
setxyz :l1   0    0
setxyz :l1 :l2    0
setxyz   0 :l2    0
setxyz   0   0    0
setxyz :l1   0    0
setxyz :l1   0 -:l3
setxyz :l1 :l2 -:l3
setxyz :l1 :l2    0
setxyz   0 :l2    0
setxyz   0 :l2 -:l3
setxyz :l1 :l2 -:l3
end
```

Example call to achieve task:

```logo>cuboid 50 100 150</lang



## LSL

Rez a box on the ground, raise it up a few meters, add the following as a New Script.

```LSL
vector vSCALE = <2.0, 3.0, 4.0>;
default {
    state_entry() {
        llSetScale(vSCALE);
    }
}
```

{{Out}} ''Ahhhhh; I always wondered what a Cuboid looked like, now I know!''  :)

[[File:Draw_A_Cuboid_LSL.jpg|200px|Draw a Cuboid]]

A Cuboid in a Sandbox.


## Maple

This creates a cuboid with one corner at (0,0,0) and the opposite at (2,3,4):

```Maple
plots:-display(plottools:-parallelepiped([2, 0, 0], [0, 0, 4], [0, 3, 0]), orientation = [45, 60])
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
This creates a cuboid with one corner at (0,0,0) and the opposite at (2,3,4):

```Mathematica
Graphics3D[Cuboid[{0,0,0},{2,3,4}]]
```

Output would be fully-rendered, rotatable 3D in the notebook.
Also, many aspects of the cuboid's appearance and lighting can be controlled quite easily. For those, see Mathematica's documentation in the program or on the web.


## Maxima


```maxima
load(draw)$

draw3d(xu_grid=100, yv_grid=100, surface_hide=true,
   palette=gray, enhanced3d=[x - z / 4 - y / 4, x, y, z],
   implicit(max(abs(x / 4), abs(y / 6), abs(z / 8)) = 1,
   x,-10,10,y,-10,10,z,-10,10))$
```



## Nim

```nim
import strutils

proc cline(n, x, y: int, cde: string) =
  echo cde[0..0].align n+1,
    repeatChar(9*x-1, cde[1]),
    cde[0], cde[2..2].align y+1

proc cuboid(x, y, z: int) =
  cline y+1, x, 0, "+-"
  for i in 1..y: cline y-i+1, x, i-1, "/ |"
  cline 0, x, y, "+-|"
  for i in 0..4*z-y-3: cline 0, x, y, "| |"
  cline 0, x, y, "| +"
  for i in countdown(y-1, 0): cline 0, x, i, "| /"
  cline 0, x, 0, "+-\n"

cuboid 2, 3, 4
cuboid 1, 1, 1
cuboid 6, 2, 1
```

```txt
    +-----------------+
   /                 /|
  /                 / |
 /                 /  |
+-----------------+   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   +
|                 |  /
|                 | /
|                 |/
+-----------------+

  +--------+
 /        /|
+--------+ |
|        | |
|        | +
|        |/
+--------+

   +-----------------------------------------------------+
  /                                                     /|
 /                                                     / |
+-----------------------------------------------------+  |
|                                                     |  +
|                                                     | /
|                                                     |/
+-----------------------------------------------------+
```



## Openscad

Drawing a cuboid is easy in openscad:


```openscad
// This will produce a simple cuboid
cube([2,3,4]);

```



## PARI/GP

[[File:Cuboid1.png|right|thumb|Output Cuboid1.png]]
[[File:Cuboid2.png|right|thumb|Output Cuboid2.png]]
Plotting lines and scaling in PARI/GP is not designed for "cuboids".
But you are welcome to play with parameters of this Cuboid() function.

```parigp

\\ Simple "cuboid". Try different parameters of this Cuboid() function.
\\ 4/11/16 aev
Cuboid(a,b,c,u=10)={
my(dx,dy,ttl="Cuboid AxBxC: ",size=200,da=a*u,db=b*u,dc=c*u);
print(" *** ",ttl,a,"x",b,"x",c,"; u=",u);
plotinit(0);
plotscale(0, 0,size, 0,size);
plotcolor(0,7); \\grey
plotmove(0, 0,0);
plotrline(0,dc,da\2); plotrline(0,db,0); plotrline(0,-db,0);
plotrline(0,0,da);
plotcolor(0,2); \\black
plotmove(0, db,da);
plotrline(0,0,-da); plotrline(0,-db,0);
plotrline(0,0,da); plotrline(0,db,0);
plotrline(0,dc,da\2); plotrline(0,-db,0); plotrline(0,-dc,-da\2);
plotmove(0, db,0);
plotrline(0,dc,da\2); plotrline(0,0,da);
plotdraw([0,size,size]);
}

{\\ Executing:
Cuboid(2,3,4,20); \\Cuboid1.png
Cuboid(5,3,1,20); \\Cuboid2.png
}

```


```txt

> Cuboid(2,3,4,20); \\Cuboid1.png
*** Cuboid AxBxC: 2x3x4; u=20
> Cuboid(5,3,1,20); \\Cuboid2.png
*** Cuboid AxBxC: 5x3x1; u=20

```



## Pascal

```pascal
program Cuboid_Demo(output);

procedure DoCuboid(sWidth, sHeight, Depth: integer);
  const
    widthScale  = 4;
    heightScale = 3;
  type
    TPage = array of array of char;
  var
    Cuboid: TPage;
    i, j: integer;
    Width, Height: integer;
    totalWidth, totalHeight: integer;
  begin
    Width  := widthScale  * sWidth;
    Height := heightScale * sHeight;
    totalWidth  := 2 * Width + Depth + 3;
    totalHeight := Height + Depth + 3;
    setlength (Cuboid, totalHeight + 1);
    for i := 1 to totalHeight do
      setlength (Cuboid[i], totalwidth + 1);
    // points
    for i := low(Cuboid) to high(Cuboid) do
      for j := low(Cuboid[i]) to high(Cuboid[i]) do
        Cuboid[i,j] := ' ';
    Cuboid [1, 1]                      := '+';
    Cuboid [Height + 2, 1]             := '+';
    Cuboid [1, 2 * Width + 2]          := '+';
    Cuboid [Height + 2, 2 * Width + 2] := '+';
    Cuboid [totalHeight, Depth + 2]    := '+';
    Cuboid [Depth + 2, totalWidth]     := '+';
    Cuboid [totalHeight, totalWidth]   := '+';
    // width lines
    for I := 1 to 2 * Width do
    begin
       Cuboid [1, I + 1]                   := '-';
       Cuboid [Height + 2, I + 1]          := '-';
       Cuboid [totalHeight, Depth + I + 2] := '-';
    end;
    // height lines
    for I := 1 to Height do
    begin
       Cuboid [I + 1, 1]                  := '|';
       Cuboid [I + 1, 2 * Width + 2]      := '|';
       Cuboid [Depth + I + 2, totalWidth] := '|';
    end;
    // depth lines
    for I := 1 to Depth do
    begin
       Cuboid [Height + 2 + I, 1 + I]             := '/';
       Cuboid [1 + I, 2 * Width + 2 + I]          := '/';
       Cuboid [Height + 2 + I, 2 * Width + 2 + I] := '/';
    end;
    for i := high(Cuboid) downto 1 do
    begin
      for j := 1 to high(Cuboid[i]) do
        write (Cuboid[i,j]);
      writeln;
    end;
  end;

begin
  writeln('1, 1, 1:');
  DoCuboid(1, 1, 1);
  writeln('2, 3, 4:');
  DoCuboid(2, 3, 4);
  writeln('6, 2, 1:');
  DoCuboid(6, 2, 1);
end.
```

```txt

% ./Cuboid
1, 1, 1:
  +--------+
 /        /|
+--------+ |
|        | |
|        | +
|        |/
+--------+
2, 3, 4:
     +----------------+
    /                /|
   /                / |
  /                /  |
 /                /   |
+----------------+    |
|                |    |
|                |    |
|                |    |
|                |    |
|                |    +
|                |   /
|                |  /
|                | /
|                |/
+----------------+
6, 2, 1:
  +------------------------------------------------+
 /                                                /|
+------------------------------------------------+ |
|                                                | |
|                                                | |
|                                                | |
|                                                | |
|                                                | +
|                                                |/
+------------------------------------------------+
```


## Perl

```perl
sub cubLine ($$$$) {
    my ($n, $dx, $dy, $cde) = @_;

    printf '%*s', $n + 1, substr($cde, 0, 1);

    for (my $d = 9 * $dx - 1 ; $d > 0 ; --$d) {
        print substr($cde, 1, 1);
    }

    print substr($cde, 0, 1);
    printf "%*s\n", $dy + 1, substr($cde, 2, 1);
}

sub cuboid ($$$) {
    my ($dx, $dy, $dz) = @_;

    printf "cuboid %d %d %d:\n", $dx, $dy, $dz;
    cubLine $dy + 1, $dx, 0, '+-';

    for (my $i = 1 ; $i <= $dy ; ++$i) {
        cubLine $dy - $i + 1, $dx, $i - 1, '/ |';
    }
    cubLine 0, $dx, $dy, '+-|';

    for (my $i = 4 * $dz - $dy - 2 ; $i > 0 ; --$i) {
        cubLine 0, $dx, $dy, '| |';
    }
    cubLine 0, $dx, $dy, '| +';

    for (my $i = 1 ; $i <= $dy ; ++$i) {
        cubLine 0, $dx, $dy - $i, '| /';
    }
    cubLine 0, $dx, 0, "+-\n";
}

cuboid 2, 3, 4;
cuboid 1, 1, 1;
cuboid 6, 2, 1;
```


```txt

cuboid 2 3 4:
    +-----------------+
   /                 /|
  /                 / |
 /                 /  |
+-----------------+   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   +
|                 |  /
|                 | /
|                 |/
+-----------------+

cuboid 1 1 1:
  +--------+
 /        /|
+--------+ |
|        | |
|        | +
|        |/
+--------+

cuboid 6 2 1:
   +-----------------------------------------------------+
  /                                                     /|
 /                                                     / |
+-----------------------------------------------------+  |
|                                                     |  +
|                                                     | /
|                                                     |/
+-----------------------------------------------------+

```


'''ASCII Art'''

```perl
use 5.010;

# usage: script X Y Z [S]

sub cuboid {

    # Constant dimnesions of the cuboid
    my ($x, $y, $z) = map int, @_[0 .. 2];

    # ASCII characters
    # $c = corner point
    # $h = horizontal line
    # $v = vertical line
    # $d = diagonal line
    # $s = space (inside the cuboid)
    my ($c, $h, $v, $d, $s) = ('+', '-', '|', '/', shift(@ARGV) // q{ });

    say q{ } x ($z + 1), $c, $h x $x, $c;
    say q{ } x ($z - $_ + 1), $d, $s x $x, $d, $s x ($_ - ($_ > $y ? ($_ - $y) : 1)),
      $_ - 1 == $y ? $c : $_ > $y ? $d : $v for 1 .. $z;
    say $c, $h x $x, $c, ($s x ($z < $y ? $z : $y), $z < $y ? $v : $z == $y ? $c : $d);
    say $v, $s x $x, $v, $z > $y ? $_ >= $z ? ($s x $x, $c) : ($s x ($y - $_), $d)
      : $y - $_ > $z ? ($s x $z, $v) : ($s x ($y - $_), $y - $_ == $z ? $c : $d) for 1 .. $y;
    say $c, $h x $x, $c;
}

cuboid shift // rand 20, shift // rand 10, shift // rand 10;
```


Cuboid(2,3,4)

```txt
     +--+
    /  /|
   /  / |
  /  /  |
 /  /   +
+--+   /
|  |  /
|  | /
|  |/
+--+
```



## Perl 6

```perl6
sub braille-graphics (%a) {
    my ($ylo, $yhi, $xlo, $xhi);
    for %a.keys -> $y {
	$ylo min= +$y; $yhi max= +$y;
	for %a{$y}.keys -> $x {
	    $xlo min= +$x; $xhi max= +$x;
	}
    }

    for $ylo, $ylo + 4 ...^ * > $yhi -> \y {
	for $xlo, $xlo + 2 ...^ * > $xhi -> \x {
	    my $cell = 0x2800;
	    $cell += 1   if %a{y + 0}{x + 0};
	    $cell += 2   if %a{y + 1}{x + 0};
	    $cell += 4   if %a{y + 2}{x + 0};
	    $cell += 8   if %a{y + 0}{x + 1};
	    $cell += 16  if %a{y + 1}{x + 1};
	    $cell += 32  if %a{y + 2}{x + 1};
	    $cell += 64  if %a{y + 3}{x + 0};
	    $cell += 128 if %a{y + 3}{x + 1};
	    print chr($cell);
	}
	print "\n";
    }
}

sub cuboid ( [$x, $y, $z] ) {
    my \x = $x * 4;
    my \y = $y * 4;
    my \z = $z * 2;
    my %t;
    sub horz ($X, $Y) { %t{$Y     }{$X + $_} = True for 0 .. x }
    sub vert ($X, $Y) { %t{$Y + $_}{$X     } = True for 0 .. y }
    sub diag ($X, $Y) { %t{$Y - $_}{$X + $_} = True for 0 .. z }

    horz(0, z); horz(z, 0); horz(  0, z+y);
    vert(0, z); vert(x, z); vert(z+x,   0);
    diag(0, z); diag(x, z); diag(  x, z+y);

    say "[$x, $y, $z]";
    braille-graphics %t;
}

cuboid $_ for [2,3,4], [3,4,2], [4,2,3], [1,1,1], [8,1,1], [1,8,1], [1,1,8];
```


[[File:Cuboid_Perl_6.png]]


## Phix

Translated from XPL0.

Press space to toggle auto-rotate on and off, cursor keys to rotate manually, and +/- to zoom in/out.
Simple orthogonal projection, no perspective.
[[File:CuboidXPL0.gif|right]]

```Phix
--
-- demo\rosetta\draw_cuboid.exw
--
include pGUI.e

Ihandle dlg, canvas, hTimer
cdCanvas cd_canvas

-- arrays: 3D coordinates of vertices
sequence x = {-2.0, +2.0, +2.0, -2.0,  -2.0, +2.0, +2.0, -2.0},
         y = {-1.5, -1.5, +1.5, +1.5,  -1.5, -1.5, +1.5, +1.5},
         z = {-1.0, -1.0, -1.0, -1.0,  +1.0, +1.0, +1.0, +1.0},
         Segment = {1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5, 1,5, 2,6, 3,7, 4,8}

atom Size = 50.0,       -- drawing size
     Sz = 0.008,        -- tumbling speeds
     Sx =-0.013,        -- ""
     Sy =-0.013,        -- ""
     S = 2

procedure draw_cube(integer wx, wh)
    atom farthest = 0.0             -- find the farthest vertex
    integer farv, v1, v2, c, style
    for i=1 to 8 do
        if z[i]>farthest then farthest = z[i]  farv = i end if
    end for
    for v=1 to 2*12 by 2 do         -- for all the vertices...
        v1 = Segment[v]             -- get vertex number
        v2 = Segment[v+1]
        c = CD_RED
        style = CD_CONTINUOUS
        if v1=farv or v2=farv then
            c = CD_BLUE
            style = CD_DASHED
        end if
        cdCanvasSetForeground(cd_canvas, c)
        cdCanvasLineStyle(cd_canvas, style)
        atom x1 = x[v1]*Size+wx,
             y1 = y[v1]*Size+wh,
             x2 = x[v2]*Size+wx,
             y2 = y[v2]*Size+wh
        cdCanvasLine(cd_canvas,x1,y1,x2,y2)
    end for
end procedure

function canvas_action_cb(Ihandle canvas)
    cdCanvasActivate(cd_canvas)
    cdCanvasClear(cd_canvas)
    integer {wx, wh} = sq_floor_div(IupGetIntInt(canvas, "DRAWSIZE"),2)
    draw_cube(wx,wh)
    cdCanvasFlush(cd_canvas)
    return IUP_DEFAULT
end function

function canvas_map_cb(Ihandle canvas)
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    IupGLMakeCurrent(canvas)
    cd_canvas = cdCreateCanvas(CD_GL, "10x10 %g", {res})
    cdCanvasSetBackground(cd_canvas, CD_BLACK)
    return IUP_DEFAULT
end function

function canvas_unmap_cb(Ihandle canvas)
    cdKillCanvas(cd_canvas)
    return IUP_DEFAULT
end function

function canvas_resize_cb(Ihandle /*canvas*/)
    integer {canvas_width, canvas_height} = IupGetIntInt(canvas, "DRAWSIZE")
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvasSetAttribute(cd_canvas, "SIZE", "%dx%d %g", {canvas_width, canvas_height, res})
    return IUP_DEFAULT
end function

function k_any(Ihandle /*ih*/, atom c)
    if c=K_ESC then
        return IUP_CLOSE
    elsif c=K_UP then
        for i=1 to 8 do
            y[i] = y[i]+z[i]*Sx*S   -- rotate vertices in Y-Z plane
            z[i] = z[i]-y[i]*Sx*S
        end for
    elsif c=K_DOWN then
        for i=1 to 8 do
            y[i] = y[i]-z[i]*Sx*S   -- rotate vertices in Y-Z plane
            z[i] = z[i]+y[i]*Sx*S
        end for
    elsif c=K_LEFT then
        for i=1 to 8 do
            x[i] = x[i]+z[i]*Sy*S   -- rotate vertices in X-Z plane
            z[i] = z[i]-x[i]*Sy*S
        end for
    elsif c=K_RIGHT then
        for i=1 to 8 do
            x[i] = x[i]-z[i]*Sy*S   -- rotate vertices in X-Z plane
            z[i] = z[i]+x[i]*Sy*S
        end for
    elsif c='+' then
        Size += 5
    elsif c='-' then
        Size = max(10,Size-5)
    elsif c=' ' then
        IupSetInt(hTimer,"RUN",not IupGetInt(hTimer,"RUN"))
    end if
    IupRedraw(canvas)
    return IUP_CONTINUE
end function

function timer_cb(Ihandle /*ih*/)
    for i=1 to 8 do
        x[i] = x[i]+y[i]*Sz*S   -- rotate vertices in X-Y plane
        y[i] = y[i]-x[i]*Sz*S
        y[i] = y[i]+z[i]*Sx*S   -- rotate vertices in Y-Z plane
        z[i] = z[i]-y[i]*Sx*S
        x[i] = x[i]+z[i]*Sy*S   -- rotate vertices in X-Z plane
        z[i] = z[i]-x[i]*Sy*S
    end for
    IupUpdate(canvas)
    return IUP_IGNORE
end function

procedure main()
    IupOpen()
    IupImageLibOpen()
    canvas = IupGLCanvas()
    IupSetAttribute(canvas, "RASTERSIZE", "640x480")
    IupSetCallback(canvas, "ACTION", Icallback("canvas_action_cb"))
    IupSetCallback(canvas, "MAP_CB", Icallback("canvas_map_cb"))
    IupSetCallback(canvas, "UNMAP_CB", Icallback("canvas_unmap_cb"))
    IupSetCallback(canvas, "RESIZE_CB", Icallback("canvas_resize_cb"))
    dlg = IupDialog(IupVbox({canvas}))
    IupSetAttribute(dlg, "TITLE", "Draw Cuboid")
    IupSetCallback(dlg, "K_ANY",  Icallback("k_any"))
    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    hTimer = IupTimer(Icallback("timer_cb"), 40)

    IupMainLoop()
    IupClose()
end procedure
main()
```



###  ascii

Two versions: the first uses a complete/rectangular grid and outputs at the end,
whereas the second uses a slightly trickier line-by-line approach.

```Phix
function draw_line(sequence res, integer x,y,dx,dy,len,c)
    string line = '+'&repeat(c,len-2)&'+'
    for i=1 to len do
        res[y,x] = line[i]
        y += dy; x += dx
    end for
    return res
end function

procedure ascii_cuboid(integer x,y,z)
    sequence res = repeat(repeat(' ',x+z+3),y+z+3)
    res = draw_line(res,    1,  z+2,+1,-1,z+2,'/')
    res = draw_line(res,  x+2,  z+2,+1,-1,z+2,'/')
    res = draw_line(res,  x+2,y+z+3,+1,-1,z+2,'/')
    res = draw_line(res,    1,  z+2, 0,+1,y+2,'|')
    res = draw_line(res,  x+2,  z+2, 0,+1,y+2,'|')
    res = draw_line(res,x+z+3,    1, 0,+1,y+2,'|')
    res = draw_line(res,  z+2,    1,+1, 0,x+2,'-')
    res = draw_line(res,    1,  z+2,+1, 0,x+2,'-')
    res = draw_line(res,    1,y+z+3,+1, 0,x+2,'-')
    printf(1,"%s\n",{join(res,"\n")})
end procedure
ascii_cuboid(0,0,0)
ascii_cuboid(1,1,1)
ascii_cuboid(2,1,2)
ascii_cuboid(3,2,1)
```

```txt

 ++
+++
++
  +-+
 / /|
+-+ +
| |/
+-+
   +--+
  /  /|
 /  / +
+--+ /
|  |/
+--+
  +---+
 /   /|
+---+ |
|   | +
|   |/
+---+

```

And as promised a line-by-line solution. Same output.

```Phix
procedure cuboid(integer x,y,z)
--
--                                        +-+   -- 1) (with x -)
--                                       / /|   -- 2) (times z)
--  Output an x by y by z cube such as  +-+ +   -- 3) (with x -)
--                                      | |/    -- 4) (times y)
--                                      +-+     -- 5) (with x -)
--
--  Nb: trailing '+' shown on stage 3 can occur higher or lower.
--
    integer mn = min(y,z)+1, mx = max(y,z)+1,
            stage = 1, -- (1..5 as above)
            pre = z+1, pad = -1, last = 1
    for l=1 to y+z+3 do
        integer c = "+/+|+"[stage]   -- (front/top corner/edge)
        puts(1,repeat(' ',pre)&c&repeat(iff(c='+'?'-':' '),x)&c&
               iff(pad>=0?repeat(' ',pad)&"|+/"[last]:"")&"\n")
        pre -= pre>0   -- (shrink the initial lhs space prefix)
        pad += (l<=mn)-(l>mx) -- +1 early on, -1 later, or both
        stage += (c='+') + (l=z+1 or l=y+z+2) -- (can skip 2&4)
        last += (last=2 or l=y+1 or l=y+z+2) -- ('|'->'+'->'/')
    end for
end procedure
cuboid(0, 0, 0)
cuboid(1, 1, 1)
cuboid(2, 1, 2)
cuboid(3, 2, 1)
```



## PicoLisp


### Using ASCII


```PicoLisp
(de cuboid (DX DY DZ)
   (cubLine (inc DY) "+" DX "-" 0)
   (for I DY
      (cubLine (- DY I -1) "/" DX " " (dec I) "|") )
   (cubLine 0 "+" DX "-" DY "|")
   (do (- (* 4 DZ) DY 2)
      (cubLine 0 "|" DX " " DY "|") )
   (cubLine 0 "|" DX " " DY "+")
   (for I DY
      (cubLine 0 "|" DX " " (- DY I) "/") )
   (cubLine 0 "+" DX "-" 0) )

(de cubLine (N C DX D DY E)
   (space N)
   (prin C)
   (do (dec (* 9 DX)) (prin D))
   (prin C)
   (space DY)
   (prinl E) )
```

```txt
: (cuboid 2 3 4)
    +-----------------+
   /                 /|
  /                 / |
 /                 /  |
+-----------------+   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   |
|                 |   +
|                 |  /
|                 | /
|                 |/
+-----------------+

: (cuboid 1 1 1)
  +--------+
 /        /|
+--------+ |
|        | |
|        | +
|        |/
+--------+

: (cuboid 6 2 1)
   +-----------------------------------------------------+
  /                                                     /|
 /                                                     / |
+-----------------------------------------------------+  |
|                                                     |  +
|                                                     | /
|                                                     |/
+-----------------------------------------------------+
```



### Using OpenGL

```PicoLisp
(load "@lib/openGl.l")

(setq *AngleX -26.0 *AngleY 74.0)
(setq *LastX 0 *LastY 0)

(glutInit)
(glutInitDisplayMode (| GLUT_RGBA GLUT_DOUBLE GLUT_DEPTH))
(glutInitWindowSize 512 512)
(glutInitWindowPosition 10 50)
(glutCreateWindow "PicoLisp Cube")

(glClearColor 1.0 1.0 1.0 1.0)	# The background color
(glEnable GL_DEPTH_TEST)
(glEnable GL_LIGHTING)
(glEnable GL_LIGHT0)
(glDisable GL_CULL_FACE)

(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
(glEnable GL_LINE_SMOOTH)
(glHint GL_LINE_SMOOTH_HINT GL_NICEST)
(glLineWidth 2.0)

(mouseFunc
   '((Btn State X Y)
      (setq *LastX X  *LastY Y) ) )

(motionFunc
   '((X Y)
      (inc '*AngleX (* (- Y *LastY) 1.0))
      (inc '*AngleY (* (- X *LastX) 1.0))
      (setq *LastX X  *LastY Y)
      (glutPostRedisplay) ) )

(reshapeFunc
   '((Width Height)
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (gluPerspective 45.0 (*/ Width 1.0 Height) 1.0 10.0)
      (glMatrixMode GL_MODELVIEW)
      (glViewport 0 0 Width Height) ) )

(displayPrg
	(glClear (| GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	(glLoadIdentity)
	(glTranslatef 0.0 0.0 -3.0)
	(glRotatef *AngleX 1 0 0)
	(glRotatef *AngleY 0 1 0)
	(glutSolidCube 1.0)

	(glDisable GL_LIGHTING)
	(glColor4f 0.4 0.4 0.4 1.0)
	(glutWireCube 1.002)
	(glEnable GL_LIGHTING)

	(glFlush)
	(glutSwapBuffers) )

(glutMainLoop)
```


=={{header|POV-Ray}}==
<lang POV-Ray>camera { perspective location <2.6,2.2,-4.2> look_at <0,-.5,0>
       aperture .05 blur_samples 100 variance 1/100000 focal_point <2,1,-2>}

light_source{< 60,20,-20> color rgb 2}

sky_sphere { pigment{ gradient z color_map{[0 rgb 0.3][.1 rgb <.7,.8,1>][1 rgb .2]} }}

box { <0,0,0> <3,2,4>
  texture {
    pigment{ agate }
    normal { checker }
    finish { reflection {0.20 metallic 0.2} }
  }
  translate <-1,-.5,-2>
}
```


[[FILE:PovRay-cuboid.jpg‎]]


## Prolog

Works with SWI-Prolog and XPCE.

```Prolog
cuboid(D1,D2,D3) :-
	W is D1 * 50,
	H is D2 * 50,
	D is D3 * 50,

	new(C, window(cuboid)),

	% compute the size of the window
	Width is W + ceiling(sqrt(H * 48)) + 50,
	Height is H +  ceiling(sqrt(H * 48)) + 50,
	send(C, size, new(_,size(Width,Height))),

	%compute the top-left corner of the front face of the cuboid
	PX is 25,
	PY is 25 + ceiling(sqrt(H * 48)),

	% colors of the faces
	new(C1, colour(@default, 65535, 0, 0)),
	new(C2, colour(@default, 0, 65535, 0)),
	new(C3, colour(@default, 0, 0, 65535)),

	% the front face
	new(B1, box(W, H)),
	send(B1, fill_pattern, C1),
	send(C, display,B1, point(PX, PY)),

	% the top face
	new(B2, hpara(point(PX,PY), W, D, C2)),
	send(C, display, B2),

	% the left face
	PX1 is PX + W,
	new(B3, vpara(point(PX1,PY), H, D, C3)),
	send(C, display, B3),

	send(C, open).



:- pce_begin_class(hpara, path, "drawing of a horizontal parallelogram").

initialise(P, Pos, Width, Height, Color) :->
	send(P, send_super, initialise),
	send(P, append, Pos),
	H is ceiling(sqrt(Height * 48)),
	get(Pos, x, X),
	get(Pos, y, Y),
	X1 is X + H,
	Y1 is Y - H,
	send(P, append, point(X1, Y1)),
	X2 is X1 + Width,
	send(P, append, point(X2, Y1)),
	X3 is X2 - H,
	send(P, append, point(X3, Pos?y)),
	send(P, append, Pos),
	send(P, fill_pattern, Color).

:- pce_end_class.

:- pce_begin_class(vpara, path, "drawing of a vertical parallelogram").

initialise(P, Pos, Height, Depth, Color) :->
	send(P, send_super, initialise),
	send(P, append, Pos),
	H is ceiling(sqrt(Depth * 48)),
	get(Pos, x, X),
	get(Pos, y, Y),
	X1 is X + H,
	Y1 is Y - H,
	send(P, append, point(X1, Y1)),
	Y2 is Y1 + Height,
	send(P, append, point(X1, Y2)),
	Y3 is Y2 + H,
	send(P, append, point(X, Y3)),
	send(P, append, Pos),
	send(P, fill_pattern, Color).

:- pce_end_class.
```

```txt
?- cuboid(2,3,4).
true.

```


[[FILE:Prolog-Cuboid.png‎]]


## Pure Data

Requires `Gem`

```Pure Data

#N canvas 1 51 450 300 10;
#X obj 66 67 gemwin;
#X obj 239 148 cuboid 2 3 4;
#X obj 239 46 gemhead;
#X obj 239 68 scale 0.3;
#X msg 66 45 lighting 1 \, create \, 1;
#X obj 61 118 gemhead;
#X obj 61 140 world_light;
#X msg 294 90 1;
#X obj 239 90 t a b;
#X obj 239 118 accumrotate;
#X connect 2 0 3 0;
#X connect 3 0 8 0;
#X connect 4 0 0 0;
#X connect 5 0 6 0;
#X connect 7 0 9 1;
#X connect 7 0 9 2;
#X connect 7 0 9 3;
#X connect 8 0 9 0;
#X connect 8 1 7 0;
#X connect 9 0 1 0;

```

Displays a rotating cuboid.


[[FILE:PureData-cuboid.png‎]]


## Python

===Ascii-Art===

```python
def _pr(t, x, y, z):
    txt = '\n'.join(''.join(t[(n,m)] for n in range(3+x+z)).rstrip()
                    for m in reversed(range(3+y+z)))
    return txt

def cuboid(x,y,z):
    t = {(n,m):' ' for n in range(3+x+z) for m in range(3+y+z)}
    xrow = ['+'] + ['%i' % (i % 10) for i in range(x)] + ['+']
    for i,ch in enumerate(xrow):
        t[(i,0)] = t[(i,1+y)] = t[(1+z+i,2+y+z)] = ch
    if _debug: print(_pr(t, x, y, z))
    ycol = ['+'] + ['%i' % (j % 10) for j in range(y)] + ['+']
    for j,ch in enumerate(ycol):
        t[(0,j)] = t[(x+1,j)] = t[(2+x+z,1+z+j)] = ch
    zdepth = ['+'] + ['%i' % (k % 10) for k in range(z)] + ['+']
    if _debug: print(_pr(t, x, y, z))
    for k,ch in enumerate(zdepth):
        t[(k,1+y+k)] = t[(1+x+k,1+y+k)] = t[(1+x+k,k)] = ch

    return _pr(t, x, y, z)


_debug = False
if __name__ == '__main__':
    for dim in ((2,3,4), (3,4,2), (4,2,3)):
        print("CUBOID%r" % (dim,), cuboid(*dim), sep='\n')
```


```txt
CUBOID(2, 3, 4)
     +01+
    3  32
   2  2 1
  1  1  0
 0  0   +
+01+   3
2  2  2
1  1 1
0  00
+01+
CUBOID(3, 4, 2)
   +012+
  1   13
 0   0 2
+012+  1
3   3  0
2   2  +
1   1 1
0   00
+012+
CUBOID(4, 2, 3)
    +0123+
   2    21
  1    1 0
 0    0  +
+0123+  2
1    1 1
0    00
+0123+
```


==={{libheader|VPython}}===
The cuboid (otherwise known as a "box" :)
### =Short version=


```python
from visual import *
mybox = box(pos=(0,0,0), length=4, height=2, width=3, axis=(-0.1,-0.1,0.1) )
scene.title = "VPython: cuboid"
```



### =Cuboid viewer=

This has a lot of extras around the cuboid,
so you can rotate the box (stepwise and continous),
change the background, color, transparancy, material,
show infos about scene and object,
plus a selfrunning demo-mode that cycles thru everything.

```python

from __future__ import print_function, division
from visual import *
import itertools

title = "VPython: Draw a cuboid"
scene.title = title
print( "%s\n" % title )

msg = """
Drag with right mousebutton to rotate view.
Drag up+down with middle mousebutton to zoom.
Left mouseclick to show info.

Press x,X, y,Y, z,Z to rotate the box in single steps.
Press b, c,o,m to change background, color, opacity, material.
Press r,R to rotate, d,a for demo, automatic,  space to stop.
Press h to show this help,  ESC or q to quit.
"""

#...+....1....+....2....+....3....+....4....+....5....+....6....+....7....+...

## Rotate one step per keypress:

def rotX(obj, a) :
    obj.rotate( angle=a, axis=(1,0,0) )
def rotY(obj, a) :
    obj.rotate( angle=a, axis=(0,1,0) )
def rotZ(obj, a) :
    obj.rotate( angle=a, axis=(0,0,1) )

## Selection of background-colors:

bg_list = [color.gray(0.2), color.gray(0.4), color.gray(0.7), color.gray(0.9)]
bg = itertools.cycle(bg_list)
def backgr() :
    b = next(bg)
    print("BackgroundColor=",b)
    scene.background = b

## Selection of colors:

col_list = [color.white, color.red,  color.orange, color.yellow,
            color.green, color.blue, color.cyan,   color.magenta,
            color.black]
col = itertools.cycle(col_list)
#c = col.next()
#c = next(col)
def paint(obj) :
    c = next(col)
    print("Color=",c)
    obj.color = c

## Selection of opacity / transparancy :

opa_list = [1.0, 0.7, 0.5, 0.2]
opa = itertools.cycle(opa_list)
def solid(obj) :
    o = next(opa)
    print("opacity =",o)
    obj.opacity = o

## Selection of materials:

mName_list = ["None",
              "wood",
              "rough",
              "bricks",
              "glass",
              "earth",
              "plastic",
              "ice",
              "diffuse",
              "marble" ]
mat_list  = [ None,
              materials.wood,
              materials.rough,
              materials.bricks,
              materials.glass,
              materials.earth,
              materials.plastic,
              materials.ice,
              materials.diffuse,
              materials.marble ]
mName = itertools.cycle(mName_list)
mat   = itertools.cycle(mat_list)
def surface(obj) :
    mM = next(mat)
    mN = next(mName)
    print("Material:", mN)
    obj.material = mM
    obj.mat      = mN

## Selection for rotation-angle & axis :

rotAng_list = [ 0.0, 0.005, 0.0, -0.005 ]
rotDir_list = [ (1,0,0), (0,1,0), (0,0,1) ]

rotAng = itertools.cycle(rotAng_list)
rotDir = itertools.cycle(rotDir_list)

rotAn = next(rotAng)     # rotAn = 0.005
rotAx = next(rotDir)     # rotAx = (1,0,0)

def rotAngle() :
    global rotAn
    rotAn = next(rotAng)
    print("RotateAngle=",rotAn)

def rotAxis() :
    global rotAx
    rotAx = next(rotDir)
    print("RotateAxis=",rotAx)

## List of keypresses for demo:

#demoC_list = [ "h", "c", "a", "o", "m", "b" ]
demoCmd_list = "rcbr"+"robr"+"rmR_r?"
demoCmd = itertools.cycle(demoCmd_list)
def demoStep() :
    k = next(demoCmd)
    print("Demo:",k)
    cmd(k)

#...+....1....+....2....+....3....+....4....+....5....+....6....+....7....+...

def objCount():
    n=0
    for obj in scene.objects:
        n=n+1
    return n

def objInfo(obj) :
    print( "\nObject:", obj )
    print( "Pos:",  obj.pos,   "Size:", obj.size )
    print( "Axis:", obj.axis,  "Up:",   obj.up )
    print( "Color", obj.color, obj.opacity )
    print( "Mat:",  obj.mat,   obj.material )

def sceneInfo(sc) :
    print( "\nScene:",  sc )
    print( ".width x height:",   sc.width, "x", sc.height )
    print( ".range:",   sc.range, ".scale:", sc.scale )
    print( ".center:",  sc.center )    # Camera
    print( ".forward:", sc.forward, ".fov:", sc.fov )
    print( "Mouse:",    sc.mouse.camera, "ray:", sc.mouse.ray )
    print( ".ambient:", sc.ambient )
    print( "Lights:",   sc.lights  )    # distant_light
    print( "objects:", objCount(), scene.objects )

#...+....1....+....2....+....3....+....4....+....5....+....6....+....7....+...

scene.width  = 600
scene.height = 400
scene.range  = 4
#scene.autocenter = True
#scene.background = color.gray(0.2)
scene.background = next(bg)

autoDemo = -1

print( msg )


## Create cuboid (aka "box") :

# c = box()     # using default-values --> cube
# c = box(pos=(0,0,0), length=4, height=2, width=3, axis=(-0.1,-0.1,0.1) )
##c  = box(pos =( 0.0, 0.0, 0.0 ),
##         size=( 4, 2, 3 ),            # L,H,W
##         axis=( 1.0, 0.0, 0.0 ),
##         up  =( 0.0, 1.0, 0.0 ),
##         color   = color.orange,
##         opacity = 1.0,
##         material= materials.marble
##         )
c  = box(pos =( 0.0, 0.0, 0.0 ),
         size=( 4, 2, 3 ),            # L,H,W
         axis=( 1.0, 0.0, 0.0 ),
         up  =( 0.0, 1.0, 0.0 )
         )
print("Box:", c)
paint(c)     # c.color    = color.red
solid(c)     # c.opacity  = 1.0
surface(c)   # c.material = materials.marble

rotX(c,0.4)         # rotate box, to bring three faces into view
rotY(c,0.6)

#sceneInfo(scene)
#objInfo(c)
print("\nPress 'a' to start auto-running demo.")

#...+....1....+....2....+....3....+....4....+....5....+....6....+....7....+...


## Processing of input:

cCount = 0
def click():
    global cCount
    cCount=cCount+1
    sceneInfo(scene)
    objInfo(c)
scene.bind( 'click', click )

def keyInput():
    key = scene.kb.getkey()
    print( 'Key: "%s"' % key )

    if ( (key == 'esc') or (key == 'q') ) :
        print( "Bye!" )
        exit(0)
    else :
        cmd(key)
scene.bind('keydown', keyInput)

def cmd(key):
    global autoDemo
    if (key == 'h') :  print( msg )
    if (key == '?') :  print( msg )
    if (key == 's') :  sceneInfo(scene)
    if (key == 'i') :  objInfo(c)

    if (key == 'x') :  rotX(c, 0.1)
    if (key == 'X') :  rotX(c,-0.1)
    if (key == 'y') :  rotY(c, 0.1)
    if (key == 'Y') :  rotY(c,-0.1)
    if (key == 'z') :  rotZ(c, 0.1)
    if (key == 'Z') :  rotZ(c,-0.1)

    if (key == 'c') :  paint(c)
    if (key == 'o') :  solid(c)
    if (key == 'm') :  surface(c)

    if (key == 'b') :  backgr()
    if (key == 'r') :  rotAngle()
    if (key == 'R') :  rotAxis()
    if (key == 'd') :  demoStep()
    if (key == 'a') :  autoDemo = -autoDemo
    if (key == 'A') :  autoDemo = -autoDemo
    if (key == ' ') :  stop()

def stop() :
    global autoDemo, rotAn
    autoDemo = -1
    while rotAn <> 0 :
      rotAngle()
    print("**Stop**")

r=100
t=0
while True:                 # Animation-loop
    rate(50)
    t = t+1
    if rotAn != 0 :
        c.rotate( angle=rotAn, axis=rotAx )

    if t>=r :
        t=0
        if autoDemo>0 :
            demoStep()

```



## PureBasic

Using generic PureBasic 2D-library.

```PureBasic
Procedure Draw_a_Cuboid(Window, X,Y,Z)
  w=WindowWidth(Window)
  h=WindowHeight(Window)
  diag.f=1.9
  If Not (w And h): ProcedureReturn: EndIf
  xscale.f = w/(x+z/diag)*0.98
  yscale.f = h/(y+z/diag)*0.98
  If xscale<yscale
    Scale.f = xscale
  Else
    Scale = yscale
  EndIf
  x*Scale: Y*Scale: Z*Scale
  CreateImage(0,w,h)
  If StartDrawing(ImageOutput(0))
    c= RGB(250, 40, 5)

    ;- Calculate the cones in the Cuboid
    xk = w/50     : yk = h/50
    x0 = Z/2 + xk : y0 = yk
    x1 = x0 + X   : y1 = y0
    x2 = xk       : y2 = y0 + Z/2
    x3 = x2 + X   : y3 = y2
    x4 = x2       : y4 = y2 + Y
    x5 = x4 + X   : y5 = y4
    x6 = x5 + Z/2 : y6 = y5 - Z/2

    ;- Draw it
    LineXY(x0,y0,x1,y1,c)
    LineXY(x0,y0,x2,y2,c)
    LineXY(x2,y2,x3,y3,c)
    LineXY(x1,y1,x3,y3,c)
    LineXY(x2,y2,x4,y4,c)
    LineXY(x4,y4,x5,y5,c)
    LineXY(x5,y5,x4,y4,c)
    LineXY(x5,y5,x6,y6,c)
    LineXY(x5,y5,x3,y3,c)
    LineXY(x6,y6,x1,y1,c)

    ;- Fill the areas
    FillArea(x,y,-1,RGB(255, 0, 0))
    FillArea(x,y-z/2,-1,RGB(0, 0, 255))
    FillArea(x+z/2,y,-1,RGB(0, 255, 0))
    StopDrawing()
  EndIf
  ;- Update the graphic
  ImageGadget(0,0,0,w,h,ImageID(0))
EndProcedure

#WFlags = #PB_Window_SystemMenu|#PB_Window_SizeGadget
#title  = "PureBasic Cuboid"
MyWin = OpenWindow(#PB_Any, 0, 0, 200, 250, #title, #WFlags)

Repeat
  WEvent = WaitWindowEvent()
  If WEvent = #PB_Event_SizeWindow
    Draw_a_Cuboid(MyWin, 2, 3, 4)
  EndIf
Until WEvent = #PB_Event_CloseWindow

;-  Save the image?
UsePNGImageEncoder()
respons = MessageRequester("Question","Save the image?",#PB_MessageRequester_YesNo)
If respons=#PB_MessageRequester_Yes
  SaveImage(0, SaveFileRequester("","","",0),#PB_ImagePlugin_PNG,9)
EndIf
```

[[Image:PB_Cuboid.png]]


## Racket


```Racket
#lang racket/gui
(require sgl/gl)

; Macro to delimit and automatically end glBegin - glEnd contexts.
(define-syntax-rule (gl-begin-end Vertex-Mode statement ...)
  (let () (glBegin Vertex-Mode) statement ... (glEnd)))

(define (resize w h)
  (glViewport 0 0 w h))

(define (draw-opengl x y z)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glEnable GL_DEPTH_TEST)
  (glClear GL_COLOR_BUFFER_BIT)
  (glClear GL_DEPTH_BUFFER_BIT)

  (define max-axis (add1 (max x y z)))

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glRotatef -45 1.0 0.0 0.0)
  (glRotatef 45 0.0 1.0 0.0)

  (gl-begin-end GL_QUADS
                (glColor3f 0 0 1)
                (glVertex3d x 0.0 z)
                (glVertex3d x y z)
                (glVertex3d x y 0.0)
                (glVertex3d x 0.0 0.0))
  (gl-begin-end GL_QUADS
                (glColor3f 1 0 0)
                (glVertex3d x 0.0 0.0)
                (glVertex3d x y 0.0)
                (glVertex3d 0.0 y 0.0)
                (glVertex3d 0.0 0.0 0.0))
  (gl-begin-end GL_QUADS
                (glColor3f 0 1 0)
                (glVertex3d x y 0.0)
                (glVertex3d x y z)
                (glVertex3d 0.0 y z)
                (glVertex3d 0.0 y 0.0)))

(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field (x 2) (y 3) (z 4))

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (draw-opengl x y z)
          (swap-gl-buffers))))

    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (resize width height))))

    (super-instantiate () (style '(gl)))))

(define win (new frame% (label "Racket Draw a cuboid") (min-width 300) (min-height 300)))
(define gl  (new my-canvas% (parent win) (x 2) (y 3) (z 4)))

(send win show #t)
```


[[Image:Racket_cuboid.png]]


## Retro


```Retro
3 elements d h w

: spaces  ( n- )  &space times ;
: ---     ( -  )  '+ putc @w 2 * [ '- putc ] times '+ putc ;
: ?       ( n- )  @h <> [ '| ] [ '+ ] if ;
: slice   ( n- )  '/ putc @w 2 * spaces '/ putc @d swap - dup spaces ? putc cr ;
: |...|/  ( -  )  @h [ '| putc @w 2 * spaces '| putc 1- spaces '/ putc cr ] iterd ;
: face    ( -  )
   ---    @w 1+ spaces '/ putc cr
  |...|/
   ---    cr ;

: cuboid  ( whd- )
  !d !h !w cr
  @d 1+ spaces --- cr
  @d [ dup spaces slice ] iterd
  face ;

2 3 4 cuboid
```


```txt

     +----+
    /    /|
   /    / |
  /    /  |
 /    /   +
+----+   /
|    |  /
|    | /
|    |/
+----+

```



## REXX


```rexx
/*REXX program displays a cuboid  (dimensions, if specified, must be positive integers).*/
parse arg x  y  z  indent .                      /*x, y, z:  dimensions and indentation.*/
x=p(x 2);  y=p(y 3);  z=p(z 4);  in=p(indent 0)  /*use the defaults if not specified.   */
pad=left('', in)                                 /*indentation must be non-negative.    */
                        call show  y+2  ,        ,     "+-"
       do j=1  for y;   call show  y-j+2,     j-1,     "/ |"     ;       end  /*j*/
                        call show       ,     y  ,     "+-|"
       do z-1;          call show       ,     y  ,     "| |"     ;       end  /*z-1*/
                        call show       ,     y  ,     "| +"
       do j=1  for y;   call show       ,     y-j,     "| /"     ;       end  /*j*/
                        call show       ,        ,     "+-"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
p:     return word( arg(1), 1)                   /*pick the first number or word in list*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  parse arg #,$,a 2 b 3 c 4                 /*get the arguments (or parts thereof).*/
       say pad || right(a, p(# 1) )copies(b, 4*x)a || right(c, p($ 0) + 1);         return
```

'''output'''   when using the input of:   <tt> 2   3   4   35 </tt>

```txt

                                       +--------+
                                      /        /|
                                     /        / |
                                    /        /  |
                                   +--------+   |
                                   |        |   |
                                   |        |   |
                                   |        |   |
                                   |        |   +
                                   |        |  /
                                   |        | /
                                   |        |/
                                   +--------+

```

'''output'''   when using the input of:   <tt> 1   1   1 </tt>

```txt

  +----+
 /    /|
+----+ |
|    | +
|    |/
+----+

```

'''output'''   when using the input of:   <tt> 6   2   1   25 </tt>

```txt

                            +------------------------+
                           /                        /|
                          /                        / |
                         +------------------------+  |
                         |                        |  +
                         |                        | /
                         |                        |/
                         +------------------------+

```



## Ring


```ring

# Project : Draw a cuboid

load "guilib.ring"

paint = null

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("Draw a cuboid")
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

        color = new qcolor()
        color.setrgb(255,0,0,255)
        mybrush = new qbrush() {setstyle(1) setcolor(color)}
        setbrush(mybrush)
        paint.drawPolygon([[200,200],[300,200],[300,100],[200,100]], 0)
        color = new qcolor()
        color.setrgb(0,255,0,255)
        mybrush = new qbrush() {setstyle(1) setcolor(color)}
        setbrush(mybrush)
        paint.drawPolygon([[200,100],[250,50],[350,50],[300,100]], 0)
        color = new qcolor()
        color.setrgb(0, 0, 255,255)
        mybrush = new qbrush() {setstyle(1) setcolor(color)}
        setbrush(mybrush)
        paint.drawPolygon([[350,50],[350,150],[300,200],[300,100]], 0)

        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

```

Output:

https://www.dropbox.com/s/bdew8ihhm0c79sd/DrawCuboid.jpg?dl=0


## Ruby


```ruby
X, Y, Z = 6, 2, 3
DIR = {"-" => [1,0], "|" => [0,1], "/" => [1,1]}

def cuboid(nx, ny, nz)
  puts "cuboid %d %d %d:" % [nx, ny, nz]
  x, y, z = X*nx, Y*ny, Z*nz
  area = Array.new(y+z+1){" " * (x+y+1)}
  draw_line = lambda do |n, sx, sy, c|
    dx, dy = DIR[c]
    (n+1).times do |i|
      xi, yi = sx+i*dx, sy+i*dy
      area[yi][xi] = (area[yi][xi]==" " ? c : "+")
    end
  end
  nz    .times {|i| draw_line[x,     0,   Z*i, "-"]}
  (ny+1).times {|i| draw_line[x,   Y*i, z+Y*i, "-"]}
  nx    .times {|i| draw_line[z,   X*i,     0, "|"]}
  (ny+1).times {|i| draw_line[z, x+Y*i,   Y*i, "|"]}
  nz    .times {|i| draw_line[y,     x,   Z*i, "/"]}
  (nx+1).times {|i| draw_line[y,   X*i,     z, "/"]}
  puts area.reverse
end

cuboid(2, 3, 4)
cuboid(1, 1, 1)
cuboid(6, 2, 1)
cuboid(2, 4, 1)
```

```txt

cuboid 2 3 4:
      +-----+-----+
     /     /     /|
    +-----+-----+ |
   /     /     /| +
  +-----+-----+ |/|
 /     /     /| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/
|     |     |/| +
+-----+-----+ |/
|     |     | +
|     |     |/
+-----+-----+
cuboid 1 1 1:
  +-----+
 /     /|
+-----+ |
|     | +
|     |/
+-----+
cuboid 6 2 1:
    +-----+-----+-----+-----+-----+-----+
   /     /     /     /     /     /     /|
  +-----+-----+-----+-----+-----+-----+ |
 /     /     /     /     /     /     /| +
+-----+-----+-----+-----+-----+-----+ |/
|     |     |     |     |     |     | +
|     |     |     |     |     |     |/
+-----+-----+-----+-----+-----+-----+
cuboid 2 4 1:
        +-----+-----+
       /     /     /|
      +-----+-----+ |
     /     /     /| +
    +-----+-----+ |/
   /     /     /| +
  +-----+-----+ |/
 /     /     /| +
+-----+-----+ |/
|     |     | +
|     |     |/
+-----+-----+

```



## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.event.{MouseAdapter, MouseEvent}

import javax.swing._

import scala.math.{Pi, cos, sin}

object Cuboid extends App {
  SwingUtilities.invokeLater(() => {

    class Cuboid extends JPanel {
      private val nodes: Array[Array[Double]] =
        Array(Array(-1, -1, -1), Array(-1, -1, 1), Array(-1, 1, -1), Array(-1, 1, 1),
          Array(1, -1, -1), Array(1, -1, 1), Array(1, 1, -1), Array(1, 1, 1))
      private var mouseX, prevMouseX, mouseY, prevMouseY: Int = _

      private def edges =
        Seq(Seq(0, 1), Seq(1, 3), Seq(3, 2), Seq(2, 0),
          Seq(4, 5), Seq(5, 7), Seq(7, 6), Seq(6, 4),
          Seq(0, 4), Seq(1, 5), Seq(2, 6), Seq(3, 7))

      override def paintComponent(gg: Graphics): Unit = {
        val g = gg.asInstanceOf[Graphics2D]

        def drawCube(g: Graphics2D): Unit = {
          g.translate(getWidth / 2, getHeight / 2)
          for (edge <- edges) {
            g.drawLine(nodes(edge.head)(0).round.toInt, nodes(edge.head)(1).round.toInt,
              nodes(edge(1))(0).round.toInt, nodes(edge(1))(1).round.toInt)
          }
          for (node <- nodes) g.fillOval(node(0).round.toInt - 4, node(1).round.toInt - 4, 8, 8)
        }

        super.paintComponent(gg)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        drawCube(g)
      }

      private def scale(sx: Double, sy: Double, sz: Double): Unit = {
        for (node <- nodes) {
          node(0) *= sx
          node(1) *= sy
          node(2) *= sz
        }
      }

      private def rotateCube(angleX: Double, angleY: Double): Unit = {
        val (sinX, cosX, sinY, cosY) = (sin(angleX), cos(angleX), sin(angleY), cos(angleY))
        for (node <- nodes) {
          val (x, y, z) = (node.head, node(1), node(2))
          node(0) = x * cosX - z * sinX
          node(2) = z * cosX + x * sinX
          node(1) = y * cosY - node(2) * sinY
          node(2) = node(2) * cosY + y * sinY
        }
      }

      addMouseListener(new MouseAdapter() {
        override def mousePressed(e: MouseEvent): Unit = {
          mouseX = e.getX
          mouseY = e.getY
        }
      })

      addMouseMotionListener(new MouseAdapter() {
        override def mouseDragged(e: MouseEvent): Unit = {
          prevMouseX = mouseX
          prevMouseY = mouseY
          mouseX = e.getX
          mouseY = e.getY
          rotateCube((mouseX - prevMouseX) * 0.01, (mouseY - prevMouseY) * 0.01)
          repaint()
        }
      })

      scale(80, 120, 160)
      rotateCube(Pi / 5, Pi / 9)
      setPreferredSize(new Dimension(640, 640))
      setBackground(Color.white)
    }

    new JFrame("Cuboid") {
      add(new Cuboid, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    }
  })
}
```


## Sidef

```ruby
const dirs = Hash("-" => [1,0], "|" => [0,1], "/" => [1,1])
 
func cuboid(nx, ny, nz) {
  say("cuboid %d %d %d:" % [nx, ny, nz])
  var(x, y, z) = (8*nx, 2*ny, 4*nz)
  var area = []
  var line = func(n, sx, sy, c) {
    var(dx, dy) = dirs{c}...
    for i (0..n) {
      var (xi, yi) = (sx + i*dx, sy + i*dy)
      area[yi] \\= [" "]*(x+y+1)
      area[yi][xi] = (area[yi][xi] == " " ? c : '+')
    }
  }
 
  0 .. nz-1 -> each {|i| line(x,       0,     4*i, "-") }
  0 .. ny   -> each {|i| line(x,     2*i, z + 2*i, "-") }
  0 .. nx-1 -> each {|i| line(z,     8*i,       0, "|") }
  0 .. ny   -> each {|i| line(z, x + 2*i,     2*i, "|") }
  0 .. nz-1 -> each {|i| line(y,       x,     4*i, "/") }
  0 .. nx   -> each {|i| line(y,     8*i,       z, "/") }
 
  area.reverse.each { |line|
     say line.join('')
  }
}
 
cuboid(2, 3, 4)
cuboid(1, 1, 1)
cuboid(6, 2, 1)
cuboid(2, 4, 1)
```


<b>A faster approach:</b>

```ruby
func cuboid (x=1,y=1,z=1,s=' ',c='+',h='-',v='|',d='/') {
    say("cuboid %d %d %d:" % (x, y, z))
    ' ' * z+1 + c + h*x + c -> say

    { |i|
        ' ' * (z - i + 1) + d + s*x + d +
              (s * (i - (i > y ? i-y : 1))) +
              (i - 1 == y ? c : (i > y ? d : v)) -> say
    }.for(1..z)

    c + h*x + c + (s * (z < y ? z : y) +
        (z < y ? v : (z == y ? c : d))) -> say

    { |i|
        v + s*x + v + (z > y
            ? (i >= z ? (s*x + c) : (s * y-i + d))
            : (y - i > z
                ? (s * z + v)
                : (s * y-i + (y-i == z ? c : d))
               )
        ) -> say;
    }.for(1..y)

    c + h*x + c -> say
}

cuboid(2, 3, 4)
cuboid(1, 1, 1)
cuboid(6, 2, 1)
cuboid(2, 4, 1)
```

```txt
cuboid 2 3 4:
     +--+
    /  /|
   /  / |
  /  /  |
 /  /   +
+--+   /
|  |  /
|  | /
|  |/
+--+
cuboid 1 1 1:
  +-+
 / /|
+-+ +
| |/
+-+
cuboid 6 2 1:
  +------+
 /      /|
+------+ |
|      | +
|      |/
+------+
cuboid 2 4 1:
  +--+
 /  /|
+--+ |
|  | |
|  | |
|  | +
|  |/
+--+
```



## Tcl

```tcl
package require Tcl 8.5
package require Tk
package require math::linearalgebra
package require math::constants

# Helper for constructing a rectangular face in 3D
proc face {px1 py1 pz1 px2 py2 pz2 px3 py3 pz3 px4 py4 pz4 color} {
    set centroidX [expr {($px1+$px2+$px3+$px4)/4.0}]
    set centroidY [expr {($py1+$py2+$py3+$py4)/4.0}]
    set centroidZ [expr {($pz1+$pz2+$pz3+$pz4)/4.0}]
    list [list \
	      [list [expr {double($px1)}] [expr {double($py1)}] [expr {double($pz1)}]] \
	      [list [expr {double($px2)}] [expr {double($py2)}] [expr {double($pz2)}]] \
	      [list [expr {double($px3)}] [expr {double($py3)}] [expr {double($pz3)}]] \
	      [list [expr {double($px4)}] [expr {double($py4)}] [expr {double($pz4)}]]] \
	[list $centroidX $centroidY $centroidZ] \
	$color
}

# How to make a cuboid of given size at the origin
proc makeCuboid {size} {
    lassign $size x y z
    list \
	[face  0  0  0   0 $y  0  $x $y  0  $x  0  0  "#800000"] \
	[face  0  0  0   0 $y  0   0 $y $z   0  0 $z  "#ff8080"] \
	[face  0  0  0  $x  0  0  $x  0 $z   0  0 $z  "#000080"] \
	[face $x  0  0  $x $y  0  $x $y $z  $x  0 $z  "#008000"] \
	[face  0 $y  0  $x $y  0  $x $y $z   0 $y $z  "#80ff80"] \
	[face  0  0 $z  $x  0 $z  $x $y $z   0 $y $z  "#8080ff"]
}

# Project a shape onto a surface (Tk canvas); assumes that the shape's faces
# are simple and non-intersecting (i.e., it sorts by centroid z-order).
proc drawShape {surface shape} {
    global projection
    lassign $projection pmat poff
    lassign $poff px py pz
    foreach side $shape {
	lassign $side points centroid color
	set pc [::math::linearalgebra::matmul $pmat $centroid]
	lappend sorting [list [expr {[lindex $pc 2]+$pz}] $points $color]
    }
    foreach side [lsort -real -decreasing -index 0 $sorting] {
	lassign $side sortCriterion points color
	set plotpoints {}
	foreach p $points {
	    set p [::math::linearalgebra::matmul $pmat $p]
	    lappend plotpoints \
		[expr {[lindex $p 0]+$px}] [expr {[lindex $p 1]+$py}]
	}
	$surface create poly $plotpoints -outline {} -fill $color
    }
}

# How to construct the projection transform.
# This is instead of using a hokey hard-coded version
namespace eval transform {
    namespace import ::math::linearalgebra::*
    ::math::constants::constants pi
    proc make {angle scale offset} {
	variable pi
	set c [expr {cos($angle*$pi/180)}]
	set s [expr {sin($angle*$pi/180)}]
	set ms [expr {-$s}]
	set rotX [list {1.0 0.0 0.0} [list 0.0 $c $ms] [list 0.0 $s $c]]
	set rotY [list [list $c 0.0 $s] {0.0 1.0 0.0} [list $ms 0.0 $c]]
	set rotZ [list [list $c $s 0.0] [list $ms $c 0.0] {0.0 0.0 1.0}]
	set mat [scale $scale [mkIdentity 3]]
	set mat [matmul [matmul [matmul $mat $rotX] $rotY] $rotZ]
	return [list $mat $offset]
    }
}
### End of definitions

# Put the pieces together
pack [canvas .c -width 400 -height 400]
set cuboid [makeCuboid {2 3 4}]
set projection [transform::make 15 50 {100 100 100}]
drawShape .c $cuboid
```

[[File:Cuboid tcl.png|Output (cropped)]]

This becomes more engaging if the drawing is animated with a final driver piece like this (the definitions part of the code is identical to above):

```tcl
pack [canvas .c -width 400 -height 400]
set cuboid [makeCuboid {2 3 4}]
wm protocol . WM_DELETE_WINDOW { exit }
while 1 {
    incr i
    .c delete all
    set projection [transform::make $i 40 {150 150 100}]
    drawShape .c $cuboid
    update
    after 50
}
```



## VBScript

```vb
x = 6 : y = 2 : z = 3

Sub cuboid(nx, ny, nz)
   WScript.StdOut.WriteLine "Cuboid " & nx & " " & ny & " " & nz & ":"
   lx = X * nx : ly = y * ny : lz = z * nz

   'define the array
   Dim area(): ReDim area(ly+lz, lx+ly)
   For i = 0 to ly+lz
      For j = 0 to lx+ly : area(i,j) = " " : Next
   Next

   'drawing lines
   For i = 0 to nz-1 : drawLine area, lx,      0,    Z*i, "-" : Next
   For i = 0 to ny   : drawLine area, lx,    y*i, lz+y*i, "-" : Next
   For i = 0 to nx-1 : drawLine area, lz,    x*i,      0, "|" : Next
   For i = 0 to ny   : drawLine area, lz, lx+y*i,    y*i, "|" : Next
   For i = 0 to nz-1 : drawLine area, ly,     lx,    z*i, "/" : Next
   For i = 0 to nx   : drawLine area, ly,    x*i,     lz, "/" : Next

   'output the cuboid (in reverse)
   For i = UBound(area,1) to 0 Step -1
      linOut = ""
      For j = 0 to UBound(area,2) : linOut = linOut & area(i,j) : Next
      WScript.StdOut.WriteLine linOut
   Next
End Sub

Sub drawLine(arr, n, sx, sy, c)
   Select Case c
      Case "-"
         dx = 1 : dy = 0
      Case "|"
         dx = 0 : dy = 1
      Case "/"
         dx = 1 : dy = 1
   End Select
   For i = 0 to n
      xi = sx + (i * dx) : yi = sy + (i * dy)
      If arr(yi, xi) = " " Then
         arr(yi, xi) = c
      Else
         arr(yi, xi) = "+"
      End If
   Next
End Sub

cuboid 2,3,4
```

```txt
Cuboid 2 3 4:
      +-----+-----+
     /     /     /|
    +-----+-----+ |
   /     /     /| +
  +-----+-----+ |/|
 /     /     /| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/|
|     |     |/| + |
+-----+-----+ |/| +
|     |     | + |/
|     |     |/| +
+-----+-----+ |/
|     |     | +
|     |     |/
+-----+-----+

```



## XPL0

[[File:CuboidXPL0.gif|right]]

```XPL0
include c:\cxpl\codes;                  \intrinsic 'code' declarations
real X, Y, Z, Farthest;                 \arrays: 3D coordinates of vertices
int  I, J, K, SI, Segment;
def  Size=50.0, Sz=0.008, Sx=-0.013;    \drawing size and tumbling speeds
[X:= [-2.0, +2.0, +2.0, -2.0,  -2.0, +2.0, +2.0, -2.0];
 Y:= [-1.5, -1.5, +1.5, +1.5,  -1.5, -1.5, +1.5, +1.5];
 Z:= [-1.0, -1.0, -1.0, -1.0,  +1.0, +1.0, +1.0, +1.0];
Segment:= [0,1, 1,2, 2,3, 3,0, 4,5, 5,6, 6,7, 7,4, 0,4, 1,5, 2,6, 3,7];
SetVid($101);                           \set 640x480 graphics with 256 colors
repeat  Farthest:= 0.0;                 \find the farthest vertex
        for I:= 0 to 8-1 do
            if Z(I) > Farthest then [Farthest:= Z(I);  SI:= I];
        Clear;                          \erase screen
        for I:= 0 to 2*12-1 do          \for all the vertices...
            [J:= Segment(I);  I:= I+1;  \get vertex number
            Move(fix(X(J)*Size)+640/2, fix(Y(J)*Size)+480/2);
            K:= Segment(I);
            Line(fix(X(K)*Size)+640/2, fix(Y(K)*Size)+480/2,
                if J=SI ! K=SI then $F009 \dashed blue\ else $C \red\);
            ];
        Sound(0, 1, 1);                 \delay 1/18 second to prevent flicker
        for I:= 0 to 8-1 do
            [X(I):= X(I) + Y(I)*Sz;     \rotate vertices in X-Y plane
             Y(I):= Y(I) - X(I)*Sz;
             Y(I):= Y(I) + Z(I)*Sx;     \rotate vertices in Y-Z plane
             Z(I):= Z(I) - Y(I)*Sx;
            ];
until KeyHit;                           \run until a key is struck
SetVid(3);                              \restore normal text mode (for DOS)
]
```



## zkl

Draws a wire frame PPM image, no hidden/dotted lines.<br/>
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
var [const] M=50.0;
fcn cuboid(w,h,z){
   w*=M; h*=M; z*=M; // relative to abs dimensions
   bitmap:=PPM(400,400);

   clr:=0xff0000;  // red facing rectangle
   bitmap.line(0,0, w,0, clr); bitmap.line(0,0, 0,h, clr);
   bitmap.line(0,h, w,h, clr); bitmap.line(w,0, w,h, clr);

   r,a:=(w+z).toFloat().toPolar(0);  // relative to the origin
   a,b:=r.toRectangular((30.0).toRad() + a).apply("toInt"); c:=a; d:=b+h;
   clr=0xff; // blue right side of cuboid
   bitmap.line(w,0, a,b, clr); bitmap.line(a,b, c,d, clr);
   bitmap.line(w,h, c,d, clr);

   e:=c-w;
   clr=0xfff00; // green top of cuboid
   bitmap.line(0,h, e,d, clr); bitmap.line(c,d, e,d, clr);

   bitmap.write(File("foo.ppm","wb"));
}(2,3,4);
```

http://www.zenkinetic.com/Images/RosettaCode/cuboid.jpg


## ZX Spectrum Basic


```zxbasic
10 LET width=50: LET height=width*1.5: LET depth=width*2
20 LET x=80: LET y=10
30 PLOT x,y
40 DRAW 0,height: DRAW width,0: DRAW 0,-height: DRAW -width,0: REM Front
50 PLOT x,y+height: DRAW depth/2,height: DRAW width,0: DRAW 0,-height: DRAW -width,-height
60 PLOT x+width,y+height: DRAW depth/2,height
```



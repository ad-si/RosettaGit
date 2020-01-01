+++
title = "Death Star"
description = ""
date = 2019-10-08T09:14:44Z
aliases = []
[extra]
id = 9404
[taxonomies]
categories = []
tags = []
+++

{{task|Constructive Solid Geometry}}
{{requires|Graphics}}
[[Category:Geometric Subtraction]]
{{omit from|AWK|Does not have this functionality in the language}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|ML/I}}
{{omit from|Modula-2}}
{{omit from|Retro}}
{{omit from|SQL PL|It does not handle GUI}}

[[File:Deathstar-tcl.gif|400px|thumb]]

;Task:
Display a region that consists of a large sphere with part of a smaller sphere removed from it as a result of geometric subtraction.

(This will basically produce a shape like a "death star".)


;Related task:
*   [[Draw a sphere]]





## AutoHotkey

{{libheader|GDIP}}

```ahk
#NoEnv
SetBatchLines, -1
#SingleInstance, Force

; Uncomment if Gdip.ahk is not in your standard library
#Include, Gdip.ahk

; Settings
X := 200, Y := 200, Width := 200, Height := 200 ; Location and size of sphere
rotation := 60 ; degrees
ARGB := 0xFFFF0000 ; Color=Solid Red

If !pToken := Gdip_Startup() ; Start gdi+
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
OnExit, Exit

Gui, -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs ; Create GUI
Gui, Show, NA ; Show GUI
hwnd1 := WinExist() ; Get a handle to this window we have created in order to update it later
hbm := CreateDIBSection(A_ScreenWidth, A_ScreenHeight) ; Create a gdi bitmap drawing area
hdc := CreateCompatibleDC() ; Get a device context compatible with the screen
obm := SelectObject(hdc, hbm) ; Select the bitmap into the device context
pGraphics := Gdip_GraphicsFromHDC(hdc) ; Get a pointer to the graphics of the bitmap, for use with drawing functions
Gdip_SetSmoothingMode(pGraphics, 4) ; Set the smoothing mode to antialias = 4 to make shapes appear smother

Gdip_TranslateWorldTransform(pGraphics, X, Y)
Gdip_RotateWorldTransform(pGraphics, rotation)

; Base ellipse
pBrush := Gdip_CreateLineBrushFromRect(0, 0, Width, Height, ARGB, 0xFF000000)
Gdip_FillEllipse(pGraphics, pBrush, 0, 0, Width, Height)

; First highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.1, Height*0.01, Width*0.8, Height*0.6, 0x33FFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.1, Height*0.01, Width*0.8, Height*0.6)

; Second highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.3, Height*0.02, Width*0.3, Height*0.2, 0xBBFFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.3, Height*0.02, Width*0.3, Height*0.2)


; Reset variables for smaller subtracted sphere
X-=150
Y-=10
Width*=0.5
Height*=0.4
rotation-=180

Gdip_TranslateWorldTransform(pGraphics, X, Y)
Gdip_RotateWorldTransform(pGraphics, rotation)

; Base ellipse
pBrush := Gdip_CreateLineBrushFromRect(0, 0, Width, Height, ARGB, 0xFF000000)
Gdip_FillEllipse(pGraphics, pBrush, 0, 0, Width, Height)

; First highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.1, Height*0.01, Width*0.8, Height*0.6, 0x33FFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.1, Height*0.01, Width*0.8, Height*0.6)

; Second highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.3, Height*0.02, Width*0.3, Height*0.2, 0xBBFFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.3, Height*0.02, Width*0.3, Height*0.2)


UpdateLayeredWindow(hwnd1, hdc, 0, 0, A_ScreenWidth, A_ScreenHeight)
SelectObject(hdc, obm) ; Select the object back into the hdc
Gdip_DeletePath(Path)
Gdip_DeleteBrush(pBrush)
DeleteObject(hbm) ; Now the bitmap may be deleted
DeleteDC(hdc) ; Also the device context related to the bitmap may be deleted
Gdip_DeleteGraphics(G) ; The graphics may now be deleted
Return

Exit:
; gdi+ may now be shutdown on exiting the program
Gdip_Shutdown(pToken)
ExitApp
```



## Brlcad


```brlcad
# We need a database to hold the objects
opendb deathstar.g y

# We will be measuring in kilometers
units km

# Create a sphere of radius 60km centred at the origin
in sph1.s sph 0 0 0 60

# We will be subtracting an overlapping sphere with a radius of 40km
# The resultant hole will be smaller than this, because we only
# only catch the edge
in sph2.s sph 0 90 0 40

# Create a region named deathstar.r which consists of big minus small sphere
r deathstar.r u sph1.s - sph2.s

# We will use a plastic material texture with rgb colour 224,224,224
# with specular lighting value of 0.1 and no inheritance
mater deathstar.r "plastic sp=0.1" 224 224 224 0

# Clear the wireframe display and draw the deathstar
B deathstar.r

# We now trigger the raytracer to see our finished product
rt
```


## C

Primitive ray tracing.

```c
#include <stdio.h>
#include <math.h>
#include <unistd.h>

const char *shades = ".:!*oe&#%@";

double light[3] = { -50, 0, 50 };
void normalize(double * v)
{
	double len = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
	v[0] /= len; v[1] /= len; v[2] /= len;
}

double dot(double *x, double *y)
{
	double d = x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
	return d < 0 ? -d : 0;
}

typedef struct { double cx, cy, cz, r; } sphere_t;

/* positive shpere and negative sphere */
sphere_t pos = { 20, 20, 0, 20 }, neg = { 1, 1, -6, 20 };

/* check if a ray (x,y, -inf)->(x, y, inf) hits a sphere; if so, return
   the intersecting z values.  z1 is closer to the eye */
int hit_sphere(sphere_t *sph, double x, double y, double *z1, double *z2)
{
	double zsq;
	x -= sph->cx;
	y -= sph->cy;
	zsq = sph->r * sph->r - (x * x + y * y);
	if (zsq < 0) return 0;
	zsq = sqrt(zsq);
	*z1 = sph->cz - zsq;
	*z2 = sph->cz + zsq;
	return 1;
}

void draw_sphere(double k, double ambient)
{
	int i, j, intensity, hit_result;
	double b;
	double vec[3], x, y, zb1, zb2, zs1, zs2;
	for (i = floor(pos.cy - pos.r); i <= ceil(pos.cy + pos.r); i++) {
		y = i + .5;
		for (j = floor(pos.cx - 2 * pos.r); j <= ceil(pos.cx + 2 * pos.r); j++) {
			x = (j - pos.cx) / 2. + .5 + pos.cx;

			/* ray lands in blank space, draw bg */
			if (!hit_sphere(&pos, x, y, &zb1, &zb2))
				hit_result = 0;

			/* ray hits pos sphere but not neg, draw pos sphere surface */
			else if (!hit_sphere(&neg, x, y, &zs1, &zs2))
				hit_result = 1;

			/* ray hits both, but pos front surface is closer */
			else if (zs1 > zb1) hit_result = 1;

			/* pos sphere surface is inside neg sphere, show bg */
			else if (zs2 > zb2) hit_result = 0;

			/* back surface on neg sphere is inside pos sphere,
			   the only place where neg sphere surface will be shown */
			else if (zs2 > zb1) hit_result = 2;
			else		    hit_result = 1;

			switch(hit_result) {
			case 0:
				putchar('+');
				continue;
			case 1:
				vec[0] = x - pos.cx;
				vec[1] = y - pos.cy;
				vec[2] = zb1 - pos.cz;
				break;
			default:
				vec[0] = neg.cx - x;
				vec[1] = neg.cy - y;
				vec[2] = neg.cz - zs2;
			}

			normalize(vec);
			b = pow(dot(light, vec), k) + ambient;
			intensity = (1 - b) * (sizeof(shades) - 1);
			if (intensity < 0) intensity = 0;
			if (intensity >= sizeof(shades) - 1)
				intensity = sizeof(shades) - 2;
			putchar(shades[intensity]);
		}
		putchar('\n');
	}
}

int main()
{
	double ang = 0;

	while (1) {
		printf("\033[H");
		light[1] = cos(ang * 2);
		light[2] = cos(ang);
		light[0] = sin(ang);
		normalize(light);
		ang += .05;

		draw_sphere(2, .3);
		usleep(100000);
	}
	return 0;
}
```



## D

{{trans|C}}

```d
import std.stdio, std.math, std.numeric, std.algorithm;

struct V3 {
    double[3] v;

    @property V3 normalize() pure nothrow const @nogc {
        immutable double len = dotProduct(v, v).sqrt;
        return [v[0] / len, v[1] / len, v[2] / len].V3;
    }

    double dot(in ref V3 y) pure nothrow const @nogc {
        immutable double d = dotProduct(v, y.v);
        return d < 0 ? -d : 0;
    }
}


const struct Sphere { double cx, cy, cz, r; }

void drawSphere(in double k, in double ambient, in V3 light) nothrow {
    /** Check if a ray (x,y, -inf).(x, y, inf) hits a sphere; if so,
    return the intersecting z values.  z1 is closer to the eye.*/
    static bool hitSphere(in ref Sphere sph,
                          in double x0, in double y0,
                          out double z1,
                          out double z2) pure nothrow @nogc {
        immutable double x = x0 - sph.cx;
        immutable double y = y0 - sph.cy;
        immutable double zsq = sph.r ^^ 2 - (x ^^ 2 + y ^^ 2);
        if (zsq < 0)
            return false;
        immutable double szsq = zsq.sqrt;
        z1 = sph.cz - szsq;
        z2 = sph.cz + szsq;
        return true;
    }

    immutable shades = ".:!*oe&#%@";
    // Positive and negative spheres.
    immutable pos = Sphere(20, 20, 0, 20);
    immutable neg = Sphere(1, 1, -6, 20);

    foreach (immutable int i; cast(int)floor(pos.cy - pos.r) ..
                              cast(int)ceil(pos.cy + pos.r) + 1) {
        immutable double y = i + 0.5;
    JLOOP:
        foreach (int j; cast(int)floor(pos.cx - 2 * pos.r) ..
                        cast(int)ceil(pos.cx + 2 * pos.r) + 1) {
            immutable double x = (j - pos.cx) / 2.0 + 0.5 + pos.cx;

            enum Hit { background, posSphere, negSphere }

            double zb1, zs2;
            immutable Hit hitResult = {
                double zb2, zs1;

                if (!hitSphere(pos, x, y, zb1, zb2)) {
                    // Ray lands in blank space, draw bg.
                    return Hit.background;
                } else if (!hitSphere(neg, x, y, zs1, zs2)) {
                    // Ray hits pos sphere but not neg one,
                    // draw pos sphere surface.
                    return Hit.posSphere;
                } else if (zs1 > zb1) {
                    // ray hits both, but pos front surface is closer.
                    return Hit.posSphere;
                } else if (zs2 > zb2) {
                    // pos sphere surface is inside neg sphere,
                    // show bg.
                    return Hit.background;
                } else if (zs2 > zb1) {
                    // Back surface on neg sphere is inside pos
                    // sphere, the only place where neg sphere
                    // surface will be shown.
                    return Hit.negSphere;
                } else {
                    return Hit.posSphere;
                }
            }();

            V3 vec_;
            final switch (hitResult) {
                case Hit.background:
                    ' '.putchar;
                    continue JLOOP;
                case Hit.posSphere:
                    vec_ = [x - pos.cx, y - pos.cy, zb1 - pos.cz].V3;
                    break;
                case Hit.negSphere:
                    vec_ = [neg.cx - x, neg.cy - y, neg.cz - zs2].V3;
                    break;
            }
            immutable nvec = vec_.normalize;

            immutable double b = light.dot(nvec) ^^ k + ambient;
            immutable intensity = cast(int)((1 - b) * shades.length);
            immutable normInt = min(shades.length, max(0, intensity));
            shades[normInt].putchar;
        }

        '\n'.putchar;
    }
}


void main() {
    immutable light = [-50, 30, 50].V3.normalize;
    drawSphere(2, 0.5, light);
}
```


The output is the same of the C version.


## DWScript

{{trans|C}}

```delphi
const cShades = '.:!*oe&#%@';

type TVector = array [0..2] of Float;

var light : TVector = [-50.0, 30, 50];

procedure Normalize(var v : TVector);
begin
   var len := Sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
   v[0] /= len; v[1] /= len; v[2] /= len;
end;

function Dot(x, y : TVector) : Float;
begin
   var d :=x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
   if d<0 then
      Result:=-d
   else Result:=0;
end;

type
   TSphere = record
      cx, cy, cz, r : Float;
   end;

const big : TSphere = (cx: 20; cy: 20; cz: 0; r: 20);
const small : TSphere = (cx: 7; cy: 7; cz: -10; r: 15);

function HitSphere(sph : TSphere; x, y : Float; var z1, z2 : Float) : Boolean;
begin
   x -= sph.cx;
   y -= sph.cy;
   var zsq = sph.r * sph.r - (x * x + y * y);
   if (zsq < 0) then Exit False;
   zsq := Sqrt(zsq);
   z1 := sph.cz - zsq;
   z2 := sph.cz + zsq;
   Result:=True;
end;

procedure DrawSphere(k, ambient : Float);
var
   i, j, intensity : Integer;
   b : Float;
   x, y, zb1, zb2, zs1, zs2 : Float;
   vec : TVector;
begin
   for i:=Trunc(big.cy-big.r) to Trunc(big.cy+big.r)+1 do begin
      y := i + 0.5;
      for j := Trunc(big.cx-2*big.r) to Trunc(big.cx+2*big.r) do begin
         x := (j-big.cx)/2 + 0.5 + big.cx;

         if not HitSphere(big, x, y, zb1, zb2) then begin
            Print(' ');
            continue;
         end;
         if not HitSphere(small, x, y, zs1, zs2) then begin
            vec[0] := x - big.cx;
            vec[1] := y - big.cy;
            vec[2] := zb1 - big.cz;
         end else begin
            if zs1 < zb1 then begin
               if zs2 > zb2 then begin
                  Print(' ');
                  continue;
               end;
               if zs2 > zb1 then begin
                  vec[0] := small.cx - x;
                  vec[1] := small.cy - y;
                  vec[2] := small.cz - zs2;
               end else begin
                  vec[0] := x - big.cx;
                  vec[1] := y - big.cy;
                  vec[2] := zb1 - big.cz;
               end;
            end else begin
               vec[0] := x - big.cx;
               vec[1] := y - big.cy;
               vec[2] := zb1 - big.cz;
            end;
         end;

         Normalize(vec);
         b := Power(Dot(light, vec), k) + ambient;
         intensity := Round((1 - b) * Length(cShades));
         Print(cShades[ClampInt(intensity+1, 1, Length(cShades))]);
      end;
      PrintLn('');
   end;
end;

Normalize(light);

DrawSphere(2, 0.3);
```


## Go

[[file:GoDstar.png|right|thumb|Output png]]
{{trans|C}}

```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/png"
    "math"
    "os"
)

type vector [3]float64

func (v *vector) normalize() {
    invLen := 1 / math.Sqrt(dot(v, v))
    v[0] *= invLen
    v[1] *= invLen
    v[2] *= invLen
}

func dot(x, y *vector) float64 {
    return x[0]*y[0] + x[1]*y[1] + x[2]*y[2]
}

type sphere struct {
    cx, cy, cz int
    r          int
}

func (s *sphere) hit(x, y int) (z1, z2 float64, hit bool) {
    x -= s.cx
    y -= s.cy
    if zsq := s.r*s.r - (x*x + y*y); zsq >= 0 {
        zsqrt := math.Sqrt(float64(zsq))
        return float64(s.cz) - zsqrt, float64(s.cz) + zsqrt, true
    }
    return 0, 0, false
}

func deathStar(pos, neg *sphere, k, amb float64, dir *vector) *image.Gray {
    w, h := pos.r*4, pos.r*3
    bounds := image.Rect(pos.cx-w/2, pos.cy-h/2, pos.cx+w/2, pos.cy+h/2)
    img := image.NewGray(bounds)
    vec := new(vector)
    for y, yMax := pos.cy-pos.r, pos.cy+pos.r; y <= yMax; y++ {
        for x, xMax := pos.cx-pos.r, pos.cx+pos.r; x <= xMax; x++ {
            zb1, zb2, hit := pos.hit(x, y)
            if !hit {
                continue
            }
            zs1, zs2, hit := neg.hit(x, y)
            if hit {
                if zs1 > zb1 {
                    hit = false
                } else if zs2 > zb2 {
                    continue
                }
            }
            if hit {
                vec[0] = float64(neg.cx - x)
                vec[1] = float64(neg.cy - y)
                vec[2] = float64(neg.cz) - zs2
            } else {
                vec[0] = float64(x - pos.cx)
                vec[1] = float64(y - pos.cy)
                vec[2] = zb1 - float64(pos.cz)
            }
            vec.normalize()
            s := dot(dir, vec)
            if s < 0 {
                s = 0
            }
            lum := 255 * (math.Pow(s, k) + amb) / (1 + amb)
            if lum < 0 {
                lum = 0
            } else if lum > 255 {
                lum = 255
            }
            img.SetGray(x, y, color.Gray{uint8(lum)})
        }
    }
    return img
}

func main() {
    dir := &vector{20, -40, -10}
    dir.normalize()
    pos := &sphere{0, 0, 0, 120}
    neg := &sphere{-90, -90, -30, 100}

    img := deathStar(pos, neg, 1.5, .2, dir)
    f, err := os.Create("dstar.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, img); err != nil {
        fmt.Println(err)
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
}
```



## J

{{Trans|Python}}


```J

load'graphics/viewmat'
mag =: +/&.:*:"1
norm=: %"1 0 mag
dot =: +/@:*"1

NB. (pos;posr;neg;negr) getvec (x,y)
getvec =: 4 :0 "1
  pt =. y
  'pos posr neg negr' =. x
  if. (dot~ pt-}:pos) > *:posr do.
    0 0 0
  else.
    zb =. ({:pos) (-,+)  posr -&.:*: pt mag@:- }:pos
    if. (dot~ pt-}:neg) > *:negr do.
      (pt,{:zb) - pos
    else.
      zs =. ({:neg) (-,+) negr -&.:*: pt mag@:- }:neg
      if. zs >&{. zb do. (pt,{:zb) - pos
      elseif. zs >&{: zb do. 0 0 0
      elseif. ({.zs) < ({:zb) do. neg - (pt,{.zs)
      elseif. do. (pt,{.zb) - pos end.
    end.
  end.
)


NB. (k;ambient;light) draw_sphere (pos;posr;neg;negr)
draw_sphere =: 4 :0
  'pos posr neg negr' =. y
  'k ambient light' =. x
  vec=. norm y getvec ,"0// (2{.pos) +/ i: 200 j.~ 0.5+posr

  b=. (mag vec) * ambient + k * 0>. light dot vec
)

togray =: 256#. 255 255 255 <.@*"1 0 (%>./@,)

env=.(2; 0.5; (norm _50 30 50))
sph=. 20 20 0; 20;   1 1 _6; 20
'rgb' viewmat togray  env draw_sphere sph
```




## Java


```Java


import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.geometry.Point3D;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.shape.MeshView;
import javafx.scene.shape.TriangleMesh;
import javafx.scene.transform.Rotate;
import javafx.stage.Stage;
public class DeathStar extends Application {

	private static final int DIVISION = 200;// the bigger the higher resolution
	float radius = 300;// radius of the sphere

	@Override
	public void start(Stage primaryStage) throws Exception {
		Point3D otherSphere = new Point3D(-radius, 0, -radius * 1.5);
		final TriangleMesh triangleMesh = createMesh(DIVISION, radius, otherSphere);
		MeshView a = new MeshView(triangleMesh);

		a.setTranslateY(radius);
		a.setTranslateX(radius);
		a.setRotationAxis(Rotate.Y_AXIS);
		Scene scene = new Scene(new Group(a));
//		uncomment if you want to move the other sphere

//		scene.setOnKeyPressed(new EventHandler<KeyEvent>() {
//			Point3D sphere = otherSphere;
//
//			@Override
//			public void handle(KeyEvent e) {
//				KeyCode code = e.getCode();
//				switch (code) {
//				case UP:
//					sphere = sphere.add(0, -10, 0);
//					break;
//				case DOWN:
//					sphere = sphere.add(0, 10, 0);
//					break;
//				case LEFT:
//					sphere = sphere.add(-10, 0, 0);
//					break;
//				case RIGHT:
//					sphere = sphere.add(10, 0, 0);
//					break;
//				case W:
//					sphere = sphere.add(0, 0, 10);
//					break;
//				case S:
//					sphere = sphere.add(0, 0, -10);
//					break;
//				default:
//					return;
//				}
//				a.setMesh(createMesh(DIVISION, radius, sphere));
//
//			}
//		});

		primaryStage.setScene(scene);
		primaryStage.show();
	}

	static TriangleMesh createMesh(final int division, final float radius, final Point3D centerOtherSphere) {
		Rotate rotate = new Rotate(180, centerOtherSphere);
		final int div2 = division / 2;

		final int nPoints = division * (div2 - 1) + 2;
		final int nTPoints = (division + 1) * (div2 - 1) + division * 2;
		final int nFaces = division * (div2 - 2) * 2 + division * 2;

		final float rDiv = 1.f / division;

		float points[] = new float[nPoints * 3];
		float tPoints[] = new float[nTPoints * 2];
		int faces[] = new int[nFaces * 6];

		int pPos = 0, tPos = 0;

		for (int y = 0; y < div2 - 1; ++y) {
			float va = rDiv * (y + 1 - div2 / 2) * 2 * (float) Math.PI;
			float sin_va = (float) Math.sin(va);
			float cos_va = (float) Math.cos(va);

			float ty = 0.5f + sin_va * 0.5f;
			for (int i = 0; i < division; ++i) {
				double a = rDiv * i * 2 * (float) Math.PI;
				float hSin = (float) Math.sin(a);
				float hCos = (float) Math.cos(a);
				points[pPos + 0] = hSin * cos_va * radius;
				points[pPos + 2] = hCos * cos_va * radius;
				points[pPos + 1] = sin_va * radius;

				final Point3D point3D = new Point3D(points[pPos + 0], points[pPos + 1], points[pPos + 2]);
				double distance = centerOtherSphere.distance(point3D);
				if (distance <= radius) {
					Point3D subtract = centerOtherSphere.subtract(point3D);
					Point3D transform = rotate.transform(subtract);
					points[pPos + 0] = (float) transform.getX();
					points[pPos + 1] = (float) transform.getY();
					points[pPos + 2] = (float) transform.getZ();

				}
				tPoints[tPos + 0] = 1 - rDiv * i;
				tPoints[tPos + 1] = ty;
				pPos += 3;
				tPos += 2;
			}
			tPoints[tPos + 0] = 0;
			tPoints[tPos + 1] = ty;
			tPos += 2;
		}

		points[pPos + 0] = 0;
		points[pPos + 1] = -radius;
		points[pPos + 2] = 0;
		points[pPos + 3] = 0;
		points[pPos + 4] = radius;
		points[pPos + 5] = 0;
		pPos += 6;

		int pS = (div2 - 1) * division;

		float textureDelta = 1.f / 256;
		for (int i = 0; i < division; ++i) {
			tPoints[tPos + 0] = rDiv * (0.5f + i);
			tPoints[tPos + 1] = textureDelta;
			tPos += 2;
		}

		for (int i = 0; i < division; ++i) {
			tPoints[tPos + 0] = rDiv * (0.5f + i);
			tPoints[tPos + 1] = 1 - textureDelta;
			tPos += 2;
		}

		int fIndex = 0;
		for (int y = 0; y < div2 - 2; ++y) {
			for (int x = 0; x < division; ++x) {
				int p0 = y * division + x;
				int p1 = p0 + 1;
				int p2 = p0 + division;
				int p3 = p1 + division;

				int t0 = p0 + y;
				int t1 = t0 + 1;
				int t2 = t0 + division + 1;
				int t3 = t1 + division + 1;

				// add p0, p1, p2
				faces[fIndex + 0] = p0;
				faces[fIndex + 1] = t0;
				faces[fIndex + 2] = p1 % division == 0 ? p1 - division : p1;
				faces[fIndex + 3] = t1;
				faces[fIndex + 4] = p2;
				faces[fIndex + 5] = t2;
				fIndex += 6;

				// add p3, p2, p1
				faces[fIndex + 0] = p3 % division == 0 ? p3 - division : p3;
				faces[fIndex + 1] = t3;
				faces[fIndex + 2] = p2;
				faces[fIndex + 3] = t2;
				faces[fIndex + 4] = p1 % division == 0 ? p1 - division : p1;
				faces[fIndex + 5] = t1;
				fIndex += 6;
			}
		}

		int p0 = pS;
		int tB = (div2 - 1) * (division + 1);
		for (int x = 0; x < division; ++x) {
			int p2 = x, p1 = x + 1, t0 = tB + x;
			faces[fIndex + 0] = p0;
			faces[fIndex + 1] = t0;
			faces[fIndex + 2] = p1 == division ? 0 : p1;
			faces[fIndex + 3] = p1;
			faces[fIndex + 4] = p2;
			faces[fIndex + 5] = p2;
			fIndex += 6;
		}

		p0 = p0 + 1;
		tB = tB + division;
		int pB = (div2 - 2) * division;

		for (int x = 0; x < division; ++x) {
			int p1 = pB + x, p2 = pB + x + 1, t0 = tB + x;
			int t1 = (div2 - 2) * (division + 1) + x, t2 = t1 + 1;
			faces[fIndex + 0] = p0;
			faces[fIndex + 1] = t0;
			faces[fIndex + 2] = p1;
			faces[fIndex + 3] = t1;
			faces[fIndex + 4] = p2 % division == 0 ? p2 - division : p2;
			faces[fIndex + 5] = t2;
			fIndex += 6;
		}

		TriangleMesh m = new TriangleMesh();
		m.getPoints().setAll(points);
		m.getTexCoords().setAll(tPoints);
		m.getFaces().setAll(faces);

		return m;
	}

	public static void main(String[] args) {

		launch(args);
	}

}

```



## JavaScript

Layer circles and gradients to achieve result similar to that of the Wikipedia page for the [http://en.wikipedia.org/wiki/Death_Star Death Star].

```JavaScript

<!DOCTYPE html>
<html>
<body style="margin:0">
  <canvas id="myCanvas" width="250" height="250" style="border:1px solid #d3d3d3;">
    Your browser does not support the HTML5 canvas tag.
  </canvas>
  <script>
    var c = document.getElementById("myCanvas");
    var ctx = c.getContext("2d");
    //Fill the canvas with a dark gray background
    ctx.fillStyle = "#222222";
    ctx.fillRect(0,0,250,250);

    // Create radial gradient for large base circle
    var grd = ctx.createRadialGradient(225,175,190,225,150,130);
    grd.addColorStop(0,"#EEEEEE");
    grd.addColorStop(1,"black");
    //Apply gradient and fill circle
    ctx.fillStyle = grd;
    ctx.beginPath();
    ctx.arc(125,125,105,0,2*Math.PI);
    ctx.fill();

    // Create linear gradient for small inner circle
    var grd = ctx.createLinearGradient(75,90,102,90);
    grd.addColorStop(0,"black");
    grd.addColorStop(1,"gray");
    //Apply gradient and fill circle
    ctx.fillStyle = grd;
    ctx.beginPath();
    ctx.arc(90,90,30,0,2*Math.PI);
    ctx.fill();

    //Add another small circle on top of the previous one to enhance the "shadow"
    ctx.fillStyle = "black";
    ctx.beginPath();
    ctx.arc(80,90,17,0,2*Math.PI);
    ctx.fill();
  </script>
</body>
</html>


```



## Julia


```julia
# run in REPL
using Makie

function deathstar()
    n = 60
    θ = [0; (0.5: n - 0.5) / n; 1]
    φ = [(0: 2n - 2) * 2 / (2n - 1); 2]
    # if x is +0.9 radius units, replace it with the coordinates of sphere surface
    # at (1.2,0,0) center, radius 0.5 units
    x = [(x1 = cospi(φ)*sinpi(θ)) > 0.9 ? 1.2 - x1 * 0.5 : x1 for θ in θ, φ in φ]
    y = [sinpi(φ)*sinpi(θ) for θ in θ, φ in φ]
    z = [cospi(θ) for θ in θ, φ in φ]
    scene = Scene(backgroundcolor=:black)
    surface!(scene, x, y, z, color = rand(RGBAf0, 124, 124), show_axis=false)
end

deathstar()

```



## LSL

Rez a box on the ground, raise it up a few meters, add the following as a New Script.

```LSL
default {
    state_entry() {
        llSetPrimitiveParams([PRIM_NAME, "RosettaCode DeathStar"]);
        llSetPrimitiveParams([PRIM_DESC, llGetObjectName()]);
        llSetPrimitiveParams([PRIM_TYPE, PRIM_TYPE_SPHERE, PRIM_HOLE_CIRCLE, <0.0, 1.0, 0.0>, 0.0, <0.0, 0.0, 0.0>, <0.12, 1.0, 0.0>]);
        llSetPrimitiveParams([PRIM_ROTATION, <-0.586217, 0.395411, -0.586217, 0.395411>]);
        llSetPrimitiveParams([PRIM_TEXTURE, ALL_SIDES, TEXTURE_BLANK, ZERO_VECTOR, ZERO_VECTOR, 0.0]);
        llSetPrimitiveParams([PRIM_TEXT, llGetObjectName(), <1.0, 1.0, 1.0>, 1.0]);
        llSetPrimitiveParams([PRIM_COLOR, ALL_SIDES, <0.5, 0.5, 0.5>, 1.0]);
        llSetPrimitiveParams([PRIM_BUMP_SHINY, ALL_SIDES, PRIM_SHINY_HIGH, PRIM_BUMP_NONE]);
        llSetPrimitiveParams([PRIM_SIZE, <10.0, 10.0, 10.0>]);
        llSetPrimitiveParams([PRIM_OMEGA, <0.0, 0.0, 1.0>, 1.0, 1.0]);
    }
}
```

Output:
[[File:Death_Star_LSL.jpg|200px|Death Star]]



## Maple


```Maple
with(plots):
with(plottools):
plots:-display(
   implicitplot3d(x^2 + y^2 + z^2 = 1, x = -1..0.85, y = -1..1, z = -1..1, style = surface, grid = [50,50,50]),
   translate(rotate(implicitplot3d(x^2 + y^2 + z^2 = 1, x = 0.85..1, y = -1..1, z = -1..1, style = surface, grid = [50,50,50]), 0, Pi, 0), 1.70, 0, 0),
axes = none, scaling = constrained, color = gray)
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
RegionPlot3D[x^2 + y^2 + z^2 < 1 && (x + 1.7)^2 + y^2 + z^2 > 1,
{x, -1, 1}, {y, -1, 1}, {z, -1, 1},
Boxed -> False, Mesh -> False, Axes -> False, Background -> Black, PlotPoints -> 100]
```



## Openscad


```openscad
// We are performing geometric subtraction

difference() {

  // Create the primary sphere of radius 60 centred at the origin

  translate(v = [0,0,0]) {
    sphere(60);
  }

  /*Subtract an overlapping sphere with a radius of 40
     The resultant hole will be smaller than this, because we only
     only catch the edge
  */

  translate(v = [0,90,0]) {
    sphere(40);
  }
}
```



## Perl

[[file:death-star-perl.png|thumb]]
Writes a PGM to stdout.

```perl
use strict;

sub sq {
	my $s = 0;
	$s += $_ ** 2 for @_;
	$s;
}

sub hit {
	my ($sph, $x, $y) = @_;
	$x -= $sph->[0];
	$y -= $sph->[1];

	my $z = sq($sph->[3]) - sq($x, $y);
	return	if $z < 0;

	$z = sqrt $z;
	return $sph->[2] - $z, $sph->[2] + $z;
}

sub normalize {
	my $v = shift;
	my $n = sqrt sq(@$v);
	$_ /= $n for @$v;
	$v;
}

sub dot {
	my ($x, $y) = @_;
	my $s = $x->[0] * $y->[0] + $x->[1] * $y->[1] + $x->[2] * $y->[2];
	$s > 0 ? $s : 0;
}

my $pos = [ 120, 120, 0, 120 ];
my $neg = [ -77, -33, -100, 190 ];
my $light = normalize([ -12, 13, -10 ]);
sub draw {
	my ($k, $amb) = @_;
	binmode STDOUT, ":raw";
	print "P5\n", $pos->[0] * 2 + 3, " ", $pos->[1] * 2 + 3, "\n255\n";
	for my $y (($pos->[1] - $pos->[3] - 1) .. ($pos->[1] + $pos->[3] + 1)) {
		my @row = ();
		for my $x (($pos->[0] - $pos->[3] - 1) .. ($pos->[0] + $pos->[3] + 1)) {
			my ($hit, @hs) = 0;
			my @h = hit($pos, $x, $y);

			if (!@h) { $hit = 0 }
			elsif (!(@hs = hit($neg, $x, $y))) { $hit = 1 }
			elsif ($hs[0] > $h[0]) { $hit = 1 }
			elsif ($hs[1] > $h[0]) { $hit = $hs[1] > $h[1] ? 0 : 2 }
			else { $hit = 1 }

			my ($val, $v);
			if ($hit == 0) { $val = 0 }
			elsif ($hit == 1) {
				$v = [	$x - $pos->[0],
					$y - $pos->[1],
					$h[0] - $pos->[2] ];
			} else {
				$v = [	$neg->[0] - $x,
					$neg->[1] - $y,
					$neg->[2] - $hs[1] ];
			}
			if ($v) {
				normalize($v);
				$val = int((dot($v, $light) ** $k + $amb) * 255);
				$val = ($val > 255) ? 255 : ($val < 0) ? 0 : $val;
			}
			push @row, $val;
		}
		print pack("C*", @row);
	}
}

draw(2, 0.2);
```



## Perl 6

{{trans|C}}Reimplemented to output a .pgm image.
{{works with|Rakudo|2018.10}}
[[File:Deathstar-perl6.png|thumb]]

```perl6
class sphere {
   has $.cx; # center x coordinate
   has $.cy; # center y coordinate
   has $.cz; # center z coordinate
   has $.r;  # radius
}

my $depth = 255;     # image color depth

my $width = my $height = 255; # dimensions of generated .pgm; must be odd

my $s = ($width - 1)/2;  # scaled dimension to build geometry

my @light = normalize([ 4, -1, -3 ]);

# positive sphere at origin
my $pos = sphere.new(
    cx => 0,
    cy => 0,
    cz => 0,
    r  => $s.Int
);

# negative sphere offset to upper left
my $neg = sphere.new(
    cx => (-$s*.90).Int,
    cy => (-$s*.90).Int,
    cz => (-$s*.3).Int,
    r  => ($s*.7).Int
);

sub MAIN ($outfile = 'deathstar-perl6.pgm') {
    spurt $outfile, ("P5\n$width $height\n$depth\n"); # .pgm header
    my $out = open( $outfile, :a, :bin ) orelse .die;
    say 'Working...';
    $out.write( Blob.new( |draw_ds(3, .15) ) );
    say 'File written.';
    $out.close;
}

sub draw_ds ( $k, $ambient ) {
    my @pixels[$height];

    (($pos.cy - $pos.r) .. ($pos.cy + $pos.r)).race.map: -> $y {
        my @row[$width];
        (($pos.cx - $pos.r) .. ($pos.cx + $pos.r)).map: -> $x {
            # black if we don't hit positive sphere, ignore negative sphere
            if not hit($pos, $x, $y, my $posz) {
                @row[$x + $s] = 0;
                next;
            }
            my @vec;
            # is front of positive sphere inside negative sphere?
            if hit($neg, $x, $y, my $negz) and $negz.min < $posz.min < $negz.max {
                # make black if whole positive sphere eaten here
                if $negz.min < $posz.max < $negz.max { @row[$x + $s] = 0; next; }
                # render inside of negative sphere
                @vec = normalize([$neg.cx - $x, $neg.cy - $y, -$negz.max - $neg.cz]);
            }
            else {
                # render outside of positive sphere
                @vec = normalize([$x - $pos.cx, $y - $pos.cy,  $posz.max - $pos.cz]);
            }
            my $intensity = dot(@light, @vec) ** $k + $ambient;
            @row[$x + $s] = ($intensity * $depth).Int min $depth;
        }
         @pixels[$y + $s] = @row;
    }
    flat |@pixels.map: *.list;
}

# normalize a vector
sub normalize (@vec) { @vec »/» ([+] @vec »*« @vec).sqrt }

# dot product of two vectors
sub dot (@x, @y) { -([+] @x »*« @y) max 0 }

# are the coordinates within the radius of the sphere?
sub hit ($sphere, $x is copy, $y is copy, $z is rw) {
    $x -= $sphere.cx;
    $y -= $sphere.cy;
    my $z2 = $sphere.r * $sphere.r - $x * $x - $y * $y;
    return 0 if $z2 < 0;
    $z2 = $z2.sqrt;
    $z = $sphere.cz - $z2 .. $sphere.cz + $z2;
    1;
}
```



## Phix

{{trans|Go}}
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\DeathStar.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function dot(sequence x, sequence y)
    return sum(sq_mul(x,y))
end function

function normalize(sequence v)
    atom len = sqrt(dot(v, v))
    if len=0 then return {0,0,0} end if
    return sq_mul(v,1/len)
end function

enum X,Y,Z

function hit(sequence s, atom x, y, atom r)
    x -= s[X]
    y -= s[Y]
    atom zsq := r*r - (x*x + y*y)
    if zsq >= 0 then
        atom zsqrt := sqrt(zsq)
        return {s[Z] - zsqrt, s[Z] + zsqrt, true}
    end if
    return {0, 0, false}
end function

procedure deathStar(integer width, height, atom k, atom amb, sequence direction)
integer lum
sequence vec
integer r = floor((min(width,height)-40)/2)
integer cx = floor(width/2)
integer cy = floor(height/2)
sequence pos = {0,0,0},
         neg = {r*-3/4,r*-3/4,r*-1/4}

    for y = pos[Y]-r to pos[Y]+r do
        for x = pos[X]-r to pos[X]+r do
            atom {zb1, zb2, hit1} := hit(pos, x, y, r)
            if hit1 then
                atom {zs1, zs2, hit2} := hit(neg, x, y, r/2)
                if not hit2 or zs2<=zb2 then
                    if hit2 and zs1<=zb1 then
                        vec = {neg[X] - x, neg[Y] - y, neg[Z] - zs2}
                    else
                        vec = {x - pos[X], y - pos[Y], zb1 - pos[Z]}
--                      vec = {x, y, zb1}
                    end if
                    atom s = dot(direction, normalize(vec))
                    lum = and_bits(#FF,255*(iff(s<0?0:power(s,k))+amb)/(1+amb))
                    lum += lum*#100+lum*#10000
                    cdCanvasPixel(cddbuffer, cx+x, cy-y, lum)
                end if
            end if
        end for
    end for
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    deathStar(width, height, 1.5, 0.2, normalize({20, -40, -10}))
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_BLACK)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "340x340") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Draw a sphere")
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


=={{header|POV-Ray}}==
<lang POV-Ray>camera { perspective location  <0.0 , .8 ,-3.0> look_at 0
         aperture .1 blur_samples 20 variance 1/100000 focal_point 0}

light_source{< 3,3,-3> color rgb 1}

sky_sphere { pigment{ color rgb <0,.2,.5>}}

plane {y,-5 pigment {color rgb .54} normal {hexagon} }

difference {
 sphere { 0,1 }
 sphere { <-1,1,-1>,1 }
  texture {
    pigment{ granite }
    finish { phong 1 reflection {0.10 metallic 0.5} }
  }
}
```

[[image:PovRay-deathstar.jpg]]


## Python

{{trans|C}}

```python
import sys, math, collections

Sphere = collections.namedtuple("Sphere", "cx cy cz r")
V3 = collections.namedtuple("V3", "x y z")

def normalize((x, y, z)):
    len = math.sqrt(x**2 + y**2 + z**2)
    return V3(x / len, y / len, z / len)

def dot(v1, v2):
    d = v1.x*v2.x + v1.y*v2.y + v1.z*v2.z
    return -d if d < 0 else 0.0

def hit_sphere(sph, x0, y0):
    x = x0 - sph.cx
    y = y0 - sph.cy
    zsq = sph.r ** 2 - (x ** 2 + y ** 2)
    if zsq < 0:
        return (False, 0, 0)
    szsq = math.sqrt(zsq)
    return (True, sph.cz - szsq, sph.cz + szsq)

def draw_sphere(k, ambient, light):
    shades = ".:!*oe&#%@"
    pos = Sphere(20.0, 20.0, 0.0, 20.0)
    neg = Sphere(1.0, 1.0, -6.0, 20.0)

    for i in xrange(int(math.floor(pos.cy - pos.r)),
                    int(math.ceil(pos.cy + pos.r) + 1)):
        y = i + 0.5
        for j in xrange(int(math.floor(pos.cx - 2 * pos.r)),
                        int(math.ceil(pos.cx + 2 * pos.r) + 1)):
            x = (j - pos.cx) / 2.0 + 0.5 + pos.cx

            (h, zb1, zb2) = hit_sphere(pos, x, y)
            if not h:
                hit_result = 0
            else:
                (h, zs1, zs2) = hit_sphere(neg, x, y)
                if not h:
                    hit_result = 1
                elif zs1 > zb1:
                    hit_result = 1
                elif zs2 > zb2:
                    hit_result = 0
                elif zs2 > zb1:
                    hit_result = 2
                else:
                    hit_result = 1

            if hit_result == 0:
                sys.stdout.write(' ')
                continue
            elif hit_result == 1:
                vec = V3(x - pos.cx, y - pos.cy, zb1 - pos.cz)
            elif hit_result == 2:
                vec = V3(neg.cx-x, neg.cy-y, neg.cz-zs2)
            vec = normalize(vec)

            b = dot(light, vec) ** k + ambient
            intensity = int((1 - b) * len(shades))
            intensity = min(len(shades), max(0, intensity))
            sys.stdout.write(shades[intensity])
        print

light = normalize(V3(-50, 30, 50))
draw_sphere(2, 0.5, light)
```





## Q

write an image in BMP format:

```Q

/ https://en.wikipedia.org/wiki/BMP_file_format
/ BITMAPINFOHEADER / RGB24

/ generate a header

genheader:{[w;h]
   0x424d, "x"$(f2i4[54+4*h*w],0,0,0,0,54,0,0,0,40,0,0,0,
                f2i4[h],f2i4[w],1,0,24,0,0,0,0,0,
                f2i4[h*((w*3)+((w*3)mod 4))],
                19,11,0,0,19,11,0,0,0,0,0,0,0,0,0,0)};

/ generate a raster line at a vertical position

genrow:{[w;y;fcn]
    row:enlist 0i;xx:0i;do[w;row,:fcn[xx;y];xx+:1i];row,:((w mod 4)#0i);1_row};

/ generate a bitmap

genbitmap:{[w;h;fcn]
    ary:enlist 0i;yy:0i;do[h;ary,:genrow[w;yy;fcn];yy+:1i];"x"$1_ary};

/ deal with endianness
/ might need to reverse last line if host computer is not a PC

f2i4:{[x] r:x;
  s0:r mod 256;r-:s0; r%:256;
  s1:r mod 256;r-:s1; r%:256;
  s2:r mod 256;r-:s2; r%:256;
  s3:r mod 256;
  "h"$(s0,s1,s2,s3)}

/ compose and write a file

writebmp:{[w;h;fcn;fn]
    fn 1: (genheader[h;w],genbitmap[w;h;fcn])};

/ / usage example:
/ w:400;
/ h:300;
/ fcn:{x0:x-w%2;y0:y-h%2;r:175;$[(r*r)>((x0*x0)+(y0*y0));(0;0;255);(0;255;0)]};
/ fn:`:demo.bmp;
/ writebmp[w;h;fcn;fn];

```


Create the death star image:


```Q

w:400; h:300; r:150; l:-0.5 0.7 0.5
sqrt0:{$[x>0;sqrt x;0]};

/ get x,y,z position of point on sphere given x,y,r

z:{[x;y;r]sqrt0((r*r)-((x*x)+(y*y)))};

/ get diffused light at point on sphere

is:{[x;y;r]
   z0:z[x;y;r];
   s:(x;y;z0)%r;
   $[z0>0;i:0.5*1+(+/)(s*l);i:0];
   i};

/ get pixel value at given image position

fcn:{[xpx;ypx]
   x:xpx-w%2;
   y:ypx-h%2;
   z1:z[x;y;r];
   x2:x+190;
   z2:170-z[x2;y;r];
   $[(r*r)<((x*x)+(y*y));
      $[y>-50;
          i:3#0;
          i:200 100 50];
      $[z2>z1;
         i:3#is[x;y;r]*140;
         i:3#is[(-1*x2);(-1*y);r]*120]
   ];
   "i"$i};

/ do it ...

\l bmp.q
fn:`:demo.bmp;
writebmp[w;h;fcn;fn];


```
(converted to JPG ...)

[[image:qdstar.jpg]]


## Racket


```racket

#lang racket
(require plot)
(plot3d (polar3d (λ (φ θ) (real-part (- (sin θ) (sqrt (- (sqr 1/3) (sqr (cos θ)))))))
                 #:samples 100 #:line-style 'transparent #:color 9)
        #:altitude 60 #:angle 80
        #:height  500 #:width 400
        #:x-min  -1/2 #:x-max 1/2
        #:y-min  -1/2 #:y-max 1/2
        #:z-min     0 #:z-max 1)

```

[[File:death-star.png]]


## REXX

{{trans|D}}

(Apologies for the comments making the lines so wide, but it was easier to read and compare to the original   '''D'''   source.)

```rexx
/*REXX program displays a sphere with another sphere subtracted where it's superimposed.*/
call deathStar   2,   .5,   v3('-50  30  50')
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
dot:   #=0;  do j=1  for words(x);  #=# + word(x,j)*word(y,j);  end; return #
dot.:  procedure; parse arg x,y; d=dot(x,y); if d<0  then return -d; return 0
ceil:  procedure; parse arg x;   _=trunc(x);                         return _+(x>0)*(x\=_)
floor: procedure; parse arg x;   _=trunc(x);                         return _-(x<0)*(x\=_)
v3:    procedure; parse arg a b c;      #=sqrt(a**2 + b**2 + c**2);  return a/#  b/#  c/#
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x; if x=0  then return 0;  d=digits();  h= d+6; numeric digits
       m.=9; numeric form; parse value format(x,2,1,,0) 'E0' with g 'E' _ .; g=g*.5'e'_%2
         do j=0  while h>9;      m.j= h;              h= h % 2 + 1;    end /*j*/
         do k=j+5  to 0  by -1;  numeric digits m.k;  g= (g +x/g)* .5; end /*k*/; return g
/*──────────────────────────────────────────────────────────────────────────────────────*/
hitSphere: procedure expose !.; parse arg xx yy zz r,x0,y0;  z= r*r -(x0-xx)**2-(y0-yy)**2
           if z<0  then return 0;  _= sqrt(z);  !.z1= zz - _;    !.z2= zz + _;    return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
deathStar: procedure; parse arg k,ambient,sun    /* [↓]  display the deathstar to screen*/
parse var  sun   s1 s2 s3                        /*identify the light source coördinates*/
if 5=="f5"x  then shading= '.:!*oe&#%@'          /*dithering chars for an EBCDIC machine*/
             else shading= '·:!ºoe@░▒▓'          /*    "       "    "   "  ASCII    "   */
shadingL= length(shading)                        /*the number of dithering characters.  */
shades.= ' ';            do i=1  for shadingL;    shades.i= substr(shading, i, 1)
                         end   /*i*/
ship=  20   20  0 20  ;           parse var  ship    shipX  shipY  shipZ  shipR
hole= ' 1    1 -6 20' ;           parse var  hole    holeX  holeY  holeZ  .

  do   i=floor(shipY-shipR  )  to ceil(shipY+shipR  )+1;    y= i +.5;   @= /*@   is a single line of the deathstar to be displayed.*/
    do j=floor(shipX-shipR*2)  to ceil(shipX+shipR*2)+1;    !.= 0
    x=.5 * (j-shipX+1) + shipX;       $bg= 0;    $pos= 0;    $neg= 0       /*$BG,  $POS,  and  $NEG  are boolean values.           */
    ?= hitSphere(ship, x, y);                    b1= !.z1;   b2= !.z2      /*?  is boolean,  "true"  indicates ray hits the sphere.*/
                                                                           /*$BG:  if 1, its background;  if zero, it's foreground.*/
    if \? then $bg= 1                                                      /*ray lands in blank space, so draw the background.     */
          else do; ?= hitSphere(hole, x, y);     s1= !.z1;   s2= !.z2
               if \? then $pos= 1                                          /*ray hits ship but not the hole, so draw ship surface. */
                     else if s1>b1 then $pos=1                             /*ray hits both, but ship front surface is closer.      */
                                   else if s2>b2 then $bg= 1               /*ship surface is inside hole,  so show the background. */
                                                 else if s2>b1 then $neg=1 /*hole back surface is inside ship;  the only place ··· */
                                                               else $pos=1 /*························ a hole surface will be shown.*/
               end
        select
        when $bg   then do;   @= @' ';    iterate j;     end               /*append a blank character to the line to be displayed. */
        when $pos  then vec_= v3(x-shipX  y-shipY  b1-shipZ)
        when $neg  then vec_= v3(holeX-x  holeY-y  holeZ-s2)
        end    /*select*/

    b=1 +min(shadingL, max(0, trunc((1 - (dot.(sun, v3(vec_))**k + ambient)) * shadingL)))
    @=@ || shades.b                                 /*B:  the ray's intensity│brightness*/
    end      /*j*/                                  /* [↑]  build a line for the sphere.*/

  if @\=''  then say strip(@, 'T')                  /*strip trailing blanks from line.  */
  end        /*i*/                                  /* [↑]  show all lines for sphere.  */
return
```

{{out|output|text=  when using the internal default input:}}
(Shown at   <big>'''<sup>1</sup>/<sub>2</sub>'''</big>   size.)
<pre style="font-size:50%">
                                    eeeee:::::::
                                eeeeeeeee··············
                             ooeeeeeeeeee··················
                           ooooeeeeeeeee······················
                        oooooooeeeeeeee··························
                      ooooooooooeeeee······························
                    ººooooooooooeeee·································
                  ººººooooooooooee·····································
                !ºººººooooooooooe·······································
              !!!ºººººooooooooo:··········································
            :!!!!ºººººooooooo:::···········································
          :::!!!!ºººººooooo!:::::···········································
        ::::!!!!!ºººººooo!!!!::::············································
       ·::::!!!!ºººººooº!!!!!::::············································
     ···::::!!!!ººººººººº!!!!:::::············································
    ···::::!!!!ººººoººººº!!!!!::::············································
  ····::::!!!!ºººoooºººººº!!!!!::::············································
 ····::::!!!!ºoooooooººººº!!!!!:::::···········································
···::::!!!!!ooooooooooººººº!!!!!:::::··········································
:::::!!!!eeoooooooooooºººººº!!!!!:::::·········································
!!!!!eeeeeeeoooooooooooºººººº!!!!!:::::········································
eeeeeeeeeeeeooooooooooooºººººº!!!!!:::::·······································
eeeeeeeeeeeeeooooooooooooºººººº!!!!!!:::::·····································
eeeeeeeeeeeeeeooooooooooooºººººº!!!!!!:::::····································
 eeeeeeeeeeeeeeooooooooooooººººººº!!!!!!:::::·································
 eeeeeeeeeeeeeeeoooooooooooooºººººº!!!!!!::::::······························:
  eeeeeeeeeeeeeeeoooooooooooooººººººº!!!!!!:::::::··························:
  eeeeeeeeeeeeeeeeooooooooooooooººººººº!!!!!!!:::::::·····················::!
   eeeeeeeeeeeeeeeeeoooooooooooooºººººººº!!!!!!!:::::::::··············::::!
    eeeeeeeeeeeeeeeeeooooooooooooooºººººººº!!!!!!!!::::::::::::::::::::::!º
     eeeeeeeeeeeeeeeeeeoooooooooooooooºººººººº!!!!!!!!!!:::::::::::::!!!!º
       eeeeeeeeeeeeeeeeeooooooooooooooooºººººººººº!!!!!!!!!!!!!!!!!!!!!º
        eeeeeeeeeeeeeeeeeeoooooooooooooooooºººººººººººº!!!!!!!!!!!!ºººº
          eeeeeeeeeeeeeeeeeeooooooooooooooooooººººººººººººººººººººººo
            eeeeeeeeeeeeeeeeeeeoooooooooooooooooooooººººººººººººooo
              eeeeeeeeeeeeeeeeeeeeooooooooooooooooooooooooooooooo
                 eeeeeeeeeeeeeeeeeeeeooooooooooooooooooooooooo
                    eeeeeeeeeeeeeeeeeeeeeoooooooooooooooooo
                        eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
                               eeeeeeeeeeeeeeeee

```



## Set lang

<lang set_lang>set ! 32
set ! 32
set ! 46
set ! 45
set ! 126
set ! 34
set ! 34
set ! 126
set ! 45
set ! 46
set ! 10
set ! 46
set ! 39
set ! 40
set ! 95
set ! 41
set ! 32
set ! 32
set ! 32
set ! 32
set ! 32
set ! 39
set ! 46
set ! 10
set ! 124
set ! 61
set ! 61
set ! 61
set ! 61
set ! 61
set ! 61
set ! 61
set ! 61
set ! 61
set ! 61
set ! 124
set ! 10
set ! 39
set ! 46
set ! 32
set ! 32
set ! 32
set ! 32
set ! 32
set ! 32
set ! 32
set ! 32
set ! 46
set ! 39
set ! 10
set ! 32
set ! 32
set ! 126
set ! 45
set ! 46
set ! 95
set ! 95
set ! 46
set ! 45
set ! 126
```

Outputs:

```txt
  .-~""~-.
.'(_)     '.
|
### ====
|
'.        .'
  ~-.__.-~
```

(it's the best I could do!)


## Sidef

{{trans|Perl}}
Writes a PGM to stdout.

```ruby
func hitf(sph, x, y) {
    x -= sph[0]
    y -= sph[1]

    var z = (sph[3]**2 - (x**2 + y**2))

    z < 0 && return nil

    z.sqrt!
    [sph[2] - z, sph[2] + z]
}

func normalize(v) {
    v / v.abs
}

func dot(x, y) {
    max(0, x*y)
}

var pos = [120, 120, 0, 120]
var neg = [-77, -33, -100, 190]
var light = normalize(Vector(-12, 13, -10))

func draw(k, amb) {
    STDOUT.binmode(':raw')
    print ("P5\n", pos[0]*2 + 3, " ", pos[1]*2 + 3, "\n255\n")

    for y in ((pos[1] - pos[3] - 1) .. (pos[1] + pos[3] + 1)) {
        var row = []
        for x in ((pos[0] - pos[3] - 1) .. (pos[0] + pos[3] + 1)) {

            var hit = 0
            var hs = []
            var h = hitf(pos, x, y)

            if    (!h)                      { hit = 0; h  = [0, 0] }
            elsif (!(hs = hitf(neg, x, y))) { hit = 1; hs = [0, 0] }
            elsif (hs[0] > h[0])            { hit = 1 }
            elsif (hs[1] > h[0])            { hit = (hs[1] > h[1] ? 0 : 2) }
            else                            { hit = 1 }

            var (val, v)

            given(hit) {
                when (0) { val = 0}
                when (1) { v = Vector(x-pos[0], y-pos[1], h[0]-pos[2]) }
                default  { v = Vector(neg[0]-x, neg[1]-y, neg[2]-hs[1]) }
            }

            if (defined(v)) {
                v = normalize(v)
                val = int((dot(v, light)**k + amb) * 255)
                val = (val > 255 ? 255 : (val < 0 ? 0 : val))
            }
            row.append(val)
        }
        print 'C*'.pack(row...)
    }
}

draw(2, 0.2)
```

Output image: [https://github.com/trizen/rc/blob/master/img/death_star_sidef.png here].


## Tcl

{{trans|C}}
Note that this code has a significant amount of refactoring relative to the C version, including the addition of specular reflections and the separation of the scene code from the raytracing from the rendering.

```tcl
package require Tcl 8.5

proc normalize vec {
    upvar 1 $vec v
    lassign $v x y z
    set len [expr {sqrt($x**2 + $y**2 + $z**2)}]
    set v [list [expr {$x/$len}] [expr {$y/$len}] [expr {$z/$len}]]
    return
}

proc dot {a b} {
    lassign $a ax ay az
    lassign $b bx by bz
    return [expr {-($ax*$bx + $ay*$by + $az*$bz)}]
}

# Intersection code; assumes that the vector is parallel to the Z-axis
proc hitSphere {sphere x y z1 z2} {
    dict with sphere {
	set x [expr {$x - $cx}]
	set y [expr {$y - $cy}]
	set zsq [expr {$r**2 - $x**2 - $y**2}]
	if {$zsq < 0} {return 0}
	upvar 1 $z1 _1 $z2 _2
	set zsq [expr {sqrt($zsq)}]
	set _1 [expr {$cz - $zsq}]
	set _2 [expr {$cz + $zsq}]
	return 1
    }
}

# How to do the intersection with our scene
proc intersectDeathStar {x y vecName} {
    global big small
    if {![hitSphere $big $x $y zb1 zb2]} {
	# ray lands in blank space
	return 0
    }
    upvar 1 $vecName vec
    # ray hits big sphere; check if it hit the small one first
    set vec [if {
	![hitSphere $small $x $y zs1 zs2] || $zs1 > $zb1 || $zs2 <= $zb1
    } then {
	dict with big {
	    list [expr {$x - $cx}] [expr {$y - $cy}] [expr {$zb1 - $cz}]
	}
    } else {
	dict with small {
	    list [expr {$cx - $x}] [expr {$cy - $y}] [expr {$cz - $zs2}]
	}
    }]
    normalize vec
    return 1
}

# Intensity calculators for different lighting components
proc diffuse {k intensity L N} {
    expr {[dot $L $N] ** $k * $intensity}
}
proc specular {k intensity L N S} {
    # Calculate reflection vector
    set r [expr {2 * [dot $L $N]}]
    foreach l $L n $N {lappend R [expr {$l-$r*$n}]}
    normalize R
    # Calculate the specular reflection term
    return [expr {[dot $R $S] ** $k * $intensity}]
}

# Simple raytracing engine that uses parallel rays
proc raytraceEngine {diffparms specparms ambient intersector shades renderer fx tx sx fy ty sy} {
    global light
    for {set y $fy} {$y <= $ty} {set y [expr {$y + $sy}]} {
	set line {}
	for {set x $fx} {$x <= $tx} {set x [expr {$x + $sx}]} {
	    if {![$intersector $x $y vec]} {
		# ray lands in blank space
		set intensity end
	    } else {
		# ray hits something; we've got the normalized vector
		set b [expr {
		    [diffuse {*}$diffparms $light $vec]
		    + [specular {*}$specparms $light $vec {0 0 -1}]
		    + $ambient
		}]
		set intensity [expr {int((1-$b) * ([llength $shades]-1))}]
		if {$intensity < 0} {
		    set intensity 0
		} elseif {$intensity >= [llength $shades]-1} {
		    set intensity end-1
		}
	    }
	    lappend line [lindex $shades $intensity]
	}
	{*}$renderer $line
    }
}

# The general scene settings
set light {-50 30 50}
set big   {cx 20 cy 20 cz 0   r 20}
set small {cx 7  cy 7  cz -10 r 15}
normalize light

# Render as text
proc textDeathStar {diff spec lightBrightness ambient} {
    global big
    dict with big {
	raytraceEngine [list $diff $lightBrightness] \
	    [list $spec $lightBrightness] $ambient intersectDeathStar \
	    [split ".:!*oe&#%@ " {}] {apply {l {puts [join $l ""]}}} \
	    [expr {$cx+floor(-$r)}] [expr {$cx+ceil($r)+0.5}] 0.5 \
	    [expr {$cy+floor(-$r)+0.5}] [expr {$cy+ceil($r)+0.5}] 1
    }
}
textDeathStar 3 10 0.7 0.3
```

Output:

```txt

                                #######&eeeeeeeee
                         ee&&&&&&########%eeoooooooooooe
                     **oooee&&&&&&########%ooooo**********oo
                  !!!***oooee&&&&&&########%********!!!!!!!!***
               !!!!!!!****ooee&&&&&&#######%*****!!!!!!!!!!!!!!!**
             ::::!!!!!!***oooee&&&&&&######***!!!!!!!::::::::::::!!*
           :::::::!!!!!!***ooeee&&&&&&#####**!!!!!!:::::::::::::::::!*
         ::::::::::!!!!!***oooee&&&&&&####*!!!!!!::::::::.........::::!*
        ::::::::::!!!!!!***oooeee&&&&&&###!!!!!!:::::::..............:::!
      ..:::::::::!!!!!!****oooeee&&&&&&##!!!!!!::::::..................::!*
     ...::::::::!!!!!!****ooooeee&&&&&&!!!!!!:::::::....................::!*
    ....::::::!!!!!!*****ooooeeee&&&&&!!!!!!:::::::......................::!*
   ....::::::!!!!!*****oooooeeeee&&&&!!!!!!::::::::.......................::!*
   ...::::::!!!!!*****oooooeeeee&&&!!!!!!:::::::::.........................::!
  ...:::::!!!!!*****oooooeeeeee&&!!!!!!!:::::::::..........................::!*
  ..:::::!!!!!****oooooeeeeee&&&!!!!!!!::::::::::..........................::!!
 .::::::!!!!*****ooooeeeeee&&*!!!!!!!::::::::::::.........................:::!!*
 :::::!!!!!****oooooeeeee&&**!!!!!!!::::::::::::::.......................::::!!*
 !!!!!!!!****oooooeeeee&****!!!!!!!::::::::::::::::::..................::::::!!*
 #!!!******oooooeeeeeoo*****!!!!!!!:::::::::::::::::::::::::::::::::::::::::!!!*
 ##oooooooooooeeeeeeoooo****!!!!!!!:::::::::::::::::::::::::::::::::::::::!!!!**
 %#####eeee&&&&&&&eeeooo****!!!!!!!!:::::::::::::::::::::::::::::::::::!!!!!!**o
 %#########&&&&&&&&eeeooo****!!!!!!!!!::::::::::::::::::!!!!!!!!!!!!!!!!!!!****o
 %##########&&&&&&&&eeeooo****!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!****ooe
  %##########&&&&&&&&eeeooo*****!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**********ooo
  %%##########&&&&&&&&eeeoooo*****!!!!!!!!!!!!!!!!!!!*********************ooooe
   %%##########&&&&&&&&eeeoooo***************************************oooooooee
   @%###########&&&&&&&&&eeeooooo*************************ooooooooooooooooeee&
    @%###########&&&&&&&&&eeeeoooooo*************ooooooooooooooooooooooeeeee&
     @%%##########&&&&&&&&&&eeeeoooooooooooooooooooooooooooooooeeeeeeeeeee&&
      @%%###########&&&&&&&&&&eeeeeoooooooooooooooooooeeeeeeeeeeeeeeeeee&&&
        %%############&&&&&&&&&&eeeeeeeeeeooeeeeeeeeeeeeeeeeeeeeeeee&&&&&
         @%%###########&&&&&&&&&&&&eeeeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&&&
           %%############&&&&&&&&&&&&&&eeeeeeeeeeeeeee&&&&&&&&&&&&&&&&
             %%############&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
               %%#############&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                  %%#############&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                     %##############&&&&&&&&&&&&&&&&&&&&&&&&
                         %##############&&&&&&&&&&&&&&&&
                                #################

```

To render it as an image, we just supply different code to map the intensities to displayable values:
{{libheader|Tk}}
[[File:Deathstar-tcl.gif|200px|thumb|Rendering of the Death Star by the Tcl solution.]]

```tcl
# Render as a picture (with many hard-coded settings)
package require Tk
proc guiDeathStar {photo diff spec lightBrightness ambient} {
    set row 0
    for {set i 255} {$i>=0} {incr i -1} {
	lappend shades [format "#%02x%02x%02x" $i $i $i]
    }
    raytraceEngine [list $diff $lightBrightness] \
	[list $spec $lightBrightness] $ambient intersectDeathStar \
	$shades {apply {l {
	    upvar 2 photo photo row row
	    $photo put [list $l] -to 0 $row
	    incr row
	    update
	}}} 0 40 0.0625 0 40 0.0625
}
pack [label .l -image [image create photo ds]]
guiDeathStar ds 3 10 0.7 0.3
```



## Yabasic


```Yabasic
open window 100,100
window origin "cc"
backcolor 0,0,0
clear window

tonos = 100
interv = int(255 / tonos)
dim shades(tonos)

shades(1) = 255
for i = 2 to tonos
	shades(i) = shades(i-1) - interv
next i

dim light(3)

light(0) = 30
light(1) = 30
light(2) = -50


sub normalize(v())
    local long

    long = sqrt(v(0)*v(0) + v(1)*v(1) + v(2)*v(2))
    v(0) = v(0) / long
    v(1) = v(1) / long
    v(2) = v(2) / long
end sub


sub punto(x(), y())
    local d

    d = x(0)*y(0) + x(1)*y(1) + x(2)*y(2)
    if d < 0 then
    	return -d
    else
    	return 0
    end if
end sub


//* positive shpere and negative sphere */
dim pos(3)
dim neg(3)

// x, y, z, r

pos(0) = 10
pos(1) = 10
pos(2) = 0
pos(3) = 20

neg(0) = 0
neg(1) = 0
neg(2) = -5
neg(3) = 15


sub hit_sphere(sph(), x, y)
	local zsq

	x = x - sph(0)
	y = y - sph(1)
	zsq = sph(3) * sph(3) - (x * x + y * y)
	if (zsq < 0) then
		return 0
	else
		return sqrt(zsq)
	end if
end sub


sub draw_sphere(k, ambient)
    local i, j, intensity, hit_result, result, b, vec(3), x, y, zb1, zb2, zs1, zs2, ini1, fin1, ini2, fin2

    ini1 = int(pos(1) - pos(3))
    fin1 = int(pos(1) + pos(3) + .5)
    for i = ini1 to fin1
        y = i + .5
        ini2 = int(pos(0) - 2 * pos(3))
        fin2 = int(pos(0) + 2 * pos(3) + .5)
        for j = ini2 to fin2
            x = (j - pos(0)) / 2 + .5 + pos(0)

            // ray lands in blank space, draw bg
            result = hit_sphere(pos(), x, y)

            if not result then
		hit_result = 0

		//* ray hits pos sphere but not neg, draw pos sphere surface */
	    else
		zb1 = pos(2) - result
		zb2 = pos(2) + result
		result = hit_sphere(neg(), x, y)
		if not result then
		    hit_result = 1
		else
		    zs1 = neg(2) - result
		    zs2 = neg(2) + result
		    if (zs1 > zb1) then
			hit_result = 1
		    elseif (zs2 > zb2) then
			hit_result = 0
		    elseif (zs2 > zb1) then
			hit_result = 2
		    else
			hit_result = 1
		    end if
		end if
	    end if

  	    if not hit_result then
  	        color 0,0,0
  	        dot x, y
  	    else
	        switch(hit_result)
	        case 1:
		    vec(0) = x - pos(0)
		    vec(1) = y - pos(1)
		    vec(2) = zb1 - pos(2)
		    break
	        default:
		    vec(0) = neg(0) - x
		    vec(1) = neg(1) - y
		    vec(2) = neg(2) - zs2
	        end switch

                normalize(vec())
                b = (punto(light(), vec())^k) + ambient
                intensity = (1 - b) * tonos
                if (intensity < 1) intensity = 1
                if (intensity > tonos) intensity = tonos
                color shades(intensity),shades(intensity),shades(intensity)
                dot x,y
            end if
        next j
    next i
end sub


ang = 0

while(true)
	//clear window
	light(1) = cos(ang * 2)
	light(2) = cos(ang)
	light(0) = sin(ang)
	normalize(light())
	ang = ang + .05

	draw_sphere(2, .3)
wend

```


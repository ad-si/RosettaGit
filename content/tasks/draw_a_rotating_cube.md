+++
title = "Draw a rotating cube"
description = ""
date = 2019-09-05T10:22:02Z
aliases = []
[extra]
id = 19095
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Draw a rotating cube.

It should be oriented with one vertex pointing straight up, and its opposite vertex on the main diagonal (the one farthest away) straight down. It can be solid or wire-frame, and you can use ASCII art if your language doesn't have graphical capabilities. Perspective is optional.




## Related tasks

* [[Draw_a_cuboid|Draw a cuboid]]
* [[Write_language_name_in_3D_ASCII|write language name in 3D ASCII]]





## C

Rotating wireframe cube in [https://www.opengl.org/ OpenGL], windowing implementation via [http://freeglut.sourceforge.net/ freeglut]

```C

#include<gl/freeglut.h>

double rot = 0;
float matCol[] = {1,0,0,0};

void display(){
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	glPushMatrix();
	glRotatef(30,1,1,0);
	glRotatef(rot,0,1,1);
	glMaterialfv(GL_FRONT,GL_DIFFUSE,matCol);
	glutWireCube(1);
	glPopMatrix();
	glFlush();
}


void onIdle(){
	rot += 0.1;
	glutPostRedisplay();
}

void reshape(int w,int h){
	float ar = (float) w / (float) h ;

	glViewport(0,0,(GLsizei)w,(GLsizei)h);
	glTranslatef(0,0,-10);
	glMatrixMode(GL_PROJECTION);
	gluPerspective(70,(GLfloat)w/(GLfloat)h,1,12);
	glLoadIdentity();
	glFrustum ( -1.0, 1.0, -1.0, 1.0, 10.0, 100.0 ) ;
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
}

void init(){
	float pos[] = {1,1,1,0};
	float white[] = {1,1,1,0};
	float shini[] = {70};

	glClearColor(.5,.5,.5,0);
	glShadeModel(GL_SMOOTH);
	glLightfv(GL_LIGHT0,GL_AMBIENT,white);
	glLightfv(GL_LIGHT0,GL_DIFFUSE,white);
	glMaterialfv(GL_FRONT,GL_SHININESS,shini);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_DEPTH_TEST);
}

int main(int argC, char* argV[])
{
	glutInit(&argC,argV);
	glutInitDisplayMode(GLUT_SINGLE|GLUT_RGB|GLUT_DEPTH);
	glutInitWindowSize(600,500);
	glutCreateWindow("Rossetta's Rotating Cube");
	init();
	glutDisplayFunc(display);
	glutReshapeFunc(reshape);
	glutIdleFunc(onIdle);
	glutMainLoop();
	return 0;
}

```



## C#

```c#
using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using System.Windows.Threading;

namespace RotatingCube
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

        public Form1()
        {
            Width = Height = 640;
            StartPosition = FormStartPosition.CenterScreen;
            SetStyle(
                ControlStyles.AllPaintingInWmPaint |
                ControlStyles.UserPaint |
                ControlStyles.DoubleBuffer,
                true);

            Scale(100, 100, 100);
            RotateCuboid(Math.PI / 4, Math.Atan(Math.Sqrt(2)));

            var timer = new DispatcherTimer();
            timer.Tick += (s, e) => { RotateCuboid(Math.PI / 180, 0); Refresh(); };
            timer.Interval = new TimeSpan(0, 0, 0, 0, 17);
            timer.Start();
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



## FutureBasic

[[File:rotating_cube.jpg|200px|thumb|right]]
Among the capabilities of FutureBasic (or FB as it's called by its developers) is the ability to compile Open GL code as demonstrated here.


```futurebasic

include "Tlbx agl.incl"
include "Tlbx glut.incl"

output file "Rotating Cube"

local fn AnimateCube
'~'1
begin globals
dim as double  sRotation
end globals

// Speed of rotation
sRotation += 2.9
glMatrixMode( _GLMODELVIEW )

glLoadIdentity()
glTranslated( 0.0, 0.0, 0.0 )
glRotated( sRotation, -0.45, -0.8, -0.6 )
glColor3d( 1.0, 0.0, 0.3 )
glLineWidth( 1.5 )
glutWireCube( 1.0 )
end fn

// Main program
dim as GLint           attrib(2)
dim as CGrafPtr        port
dim as AGLPixelFormat  fmt
dim as AGLContext      glContext
dim as EventRecord     ev
dim as GLboolean       yesOK

window 1, @"Rotating Cube", (0,0) - (500,500)

attrib(0) = _AGLRGBA
attrib(1) = _AGLDOUBLEBUFFER
attrib(2) = _AGLNONE

fmt = fn aglChoosePixelFormat( 0, 0, attrib(0) )
glContext = fn aglCreateContext( fmt, 0 )
aglDestroyPixelFormat( fmt )

port = window( _wndPort )
yesOK = fn aglSetDrawable( glContext, port )
yesOK = fn aglSetCurrentContext( glContext )

glClearColor( 0.0, 0.0, 0.0, 0.0 )

poke long event - 8, 1
do
glClear( _GLCOLORBUFFERBIT )
fn AnimateCube
aglSwapBuffers( glContext )
HandleEvents
until gFBQuit

```



## Go

As of Go 1.9, it looks as if the only standard library supporting animated graphics is image/gif - so we create an animated GIF...

```Go
package main

import (
	"image"
	"image/color"
	"image/gif"
	"log"
	"math"
	"os"
)

const (
	width, height = 640, 640
	offset        = height / 2
	fileName      = "rotatingCube.gif"
)

var nodes = [][]float64{{-100, -100, -100}, {-100, -100, 100}, {-100, 100, -100}, {-100, 100, 100},
	{100, -100, -100}, {100, -100, 100}, {100, 100, -100}, {100, 100, 100}}
var edges = [][]int{{0, 1}, {1, 3}, {3, 2}, {2, 0}, {4, 5}, {5, 7}, {7, 6},
	{6, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}}

func main() {
	var images []*image.Paletted
	fgCol := color.RGBA{0xff, 0x00, 0xff, 0xff}
	var palette = []color.Color{color.RGBA{0x00, 0x00, 0x00, 0xff}, fgCol}
	var delays []int

	imgFile, err := os.Create(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer imgFile.Close()

	rotateCube(math.Pi/4, math.Atan(math.Sqrt(2)))
	var frame float64
	for frame = 0; frame < 360; frame++ {
		img := image.NewPaletted(image.Rect(0, 0, width, height), palette)
		images = append(images, img)
		delays = append(delays, 5)
		for _, edge := range edges {
			xy1 := nodes[edge[0]]
			xy2 := nodes[edge[1]]
			drawLine(int(xy1[0])+offset, int(xy1[1])+offset, int(xy2[0])+offset, int(xy2[1])+offset, img, fgCol)
		}
		rotateCube(math.Pi/180, 0)
	}
	if err := gif.EncodeAll(imgFile, &gif.GIF{Image: images, Delay: delays}); err != nil {
		imgFile.Close()
		log.Fatal(err)
	}

}

func rotateCube(angleX, angleY float64) {
	sinX := math.Sin(angleX)
	cosX := math.Cos(angleX)
	sinY := math.Sin(angleY)
	cosY := math.Cos(angleY)
	for _, node := range nodes {
		x := node[0]
		y := node[1]
		z := node[2]
		node[0] = x*cosX - z*sinX
		node[2] = z*cosX + x*sinX
		z = node[2]
		node[1] = y*cosY - z*sinY
		node[2] = z*cosY + y*sinY
	}
}

func drawLine(x0, y0, x1, y1 int, img *image.Paletted, col color.RGBA) {
	dx := abs(x1 - x0)
	dy := abs(y1 - y0)
	var sx, sy int = -1, -1
	if x0 < x1 {
		sx = 1
	}
	if y0 < y1 {
		sy = 1
	}
	err := dx - dy
	for {
		img.Set(x0, y0, col)
		if x0 == x1 && y0 == y1 {
			break
		}
		e2 := 2 * err
		if e2 > -dy {
			err -= dy
			x0 += sx
		}
		if e2 < dx {
			err += dx
			y0 += sy
		}
	}
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
```



## Haskell

This implementation compiles to JavaScript that runs in a browser using the [https://github.com/ghcjs/ghcjs ghcjs compiler ] .  The [https://github.com/reflex-frp/reflex-dom reflex-dom ] library is used to help with svg rendering and animation.


```Haskell
{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Data.Map as DM (Map, lookup, insert, empty, fromList)
import Data.Matrix
import Data.Time.Clock
import Control.Monad.Trans

size = 500
updateFrequency = 0.2
rotationStep = pi/10

data Color = Red | Green | Blue | Yellow | Orange | Purple | Black deriving (Show,Eq,Ord,Enum)

zRot :: Float -> Matrix Float
zRot rotation =
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  s,  0,  0 ]
                 ,[-s,  c,  0,  0 ]
                 ,[ 0,  0,  1,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

xRot :: Float -> Matrix Float
xRot rotation =
    let c = cos rotation
        s = sin rotation
    in fromLists [[ 1,  0,  0,  0 ]
                 ,[ 0,  c,  s,  0 ]
                 ,[ 0, -s,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

yRot :: Float -> Matrix Float
yRot rotation =
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  0, -s,  0 ]
                 ,[ 0,  1,  0,  0 ]
                 ,[ s,  0,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

translation :: (Float,Float,Float) -> Matrix Float
translation (x,y,z) =
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  0 ]
               ,[ x,  y,  z,  1 ]
               ]

scale :: Float -> Matrix Float
scale s =
    fromLists  [[ s,  0,  0,  0 ]
               ,[ 0,  s,  0,  0 ]
               ,[ 0,  0,  s,  0 ]
               ,[ 0,  0,  0,  1 ]
               ]

-- perspective transformation;
perspective :: Matrix Float
perspective =
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  1 ]
               ,[ 0,  0,  1,  1 ] ]

transformPoints :: Matrix Float -> Matrix Float -> [(Float,Float)]
transformPoints transform points =
    let result4d = points `multStd2` transform
        result2d = (\[x,y,z,w] -> (x/w,y/w)) <$> toLists result4d
    in result2d

showRectangle :: MonadWidget t m => Float -> Float -> Float -> Float -> Color -> Dynamic t (Matrix Float) -> m ()
showRectangle x0 y0 x1 y1 faceColor dFaceView = do
    let points = fromLists [[x0,y0,0,1],[x0,y1,0,1],[x1,y1,0,1],[x1,y0,0,1]]
        pointsToString = concatMap (\(x,y) -> show x ++ ", " ++ show y ++ " ")
    dAttrs <- mapDyn (\fvk -> DM.fromList [ ("fill", show faceColor)
                                          , ("points", pointsToString (transformPoints fvk points))
                                          ] ) dFaceView
    elDynAttrSVG "polygon" dAttrs $ return ()

showUnitSquare :: MonadWidget t m => Color -> Float -> Dynamic t (Matrix Float) -> m ()
showUnitSquare faceColor margin dFaceView =
    showRectangle margin margin (1.0 - margin) (1.0 - margin) faceColor dFaceView

-- show colored square on top of black square for outline effect
showFace :: MonadWidget t m => Color -> Dynamic t (Matrix Float) -> m ()
showFace faceColor dFaceView = do
    showUnitSquare Black 0 dFaceView
    showUnitSquare faceColor 0.03 dFaceView

facingCamera :: [Float] -> Matrix Float -> Bool
facingCamera viewPoint modelTransform =
    let cross [x0,y0,z0] [x1,y1,z1] = [y0*z1-z0*y1, z0*x1-x0*z1, x0*y1-y0*x1 ]
        dot v0 v1 = sum $ zipWith (*) v0 v1
        vMinus = zipWith (-)

        untransformedPoints = fromLists [ [0,0,0,1]   -- lower left
                                        , [1,0,0,1]   -- lower right
                                        , [0,1,0,1] ] -- upper left

        transformedPoints = toLists $ untransformedPoints `multStd2` modelTransform
        pt00 = take 3 $ head transformedPoints         -- transformed lower left
        pt10 = take 3 $ transformedPoints !! 1         -- transformed upper right
        pt01 = take 3 $ transformedPoints !! 2         -- transformed upper left

        tVec_10_00 = pt10 `vMinus` pt00                -- lower right to lower left
        tVec_01_00 = pt01 `vMinus` pt00                -- upper left to lower left
        perpendicular = tVec_10_00 `cross` tVec_01_00  -- perpendicular to face
        cameraToPlane = pt00 `vMinus` viewPoint        -- line of sight to face

        -- Perpendicular points away from surface;
        -- Camera vector points towards surface
        -- Opposed vectors means that face will be visible.
    in cameraToPlane `dot` perpendicular < 0

faceView :: Matrix Float -> Matrix Float -> (Bool, Matrix Float)
faceView modelOrientation faceOrientation =
    let modelTransform =            translation (-1/2,-1/2,1/2) -- unit square to origin + z offset
                         `multStd2` faceOrientation             -- orientation specific to each face
                         `multStd2` scale (1/2)                 -- shrink cube to fit in view.
                         `multStd2` modelOrientation            -- position the entire cube


        isFacingCamera = facingCamera [0,0,-1] modelTransform   -- backface elimination

        -- combine to get single transform from 2d face to 2d display
        viewTransform =            modelTransform
                        `multStd2` perspective
                        `multStd2` scale size                       -- scale up to svg box scale
                        `multStd2` translation (size/2, size/2, 0)  -- move to center of svg box

    in (isFacingCamera, viewTransform)

updateFaceViews :: Matrix Float -> Map Color (Matrix Float) -> (Color, Matrix Float) -> Map Color (Matrix Float)
updateFaceViews modelOrientation prevCollection (faceColor, faceOrientation) =
    let (isVisible, newFaceView) = faceView modelOrientation faceOrientation
    in  if isVisible
        then insert faceColor newFaceView prevCollection
        else prevCollection

faceViews :: Matrix Float -> Map Color (Matrix Float)
faceViews modelOrientation  =
    foldl (updateFaceViews modelOrientation) empty
          [ (Purple , xRot (0.0) )
          , (Yellow , xRot (pi/2) )
          , (Red    , yRot (pi/2) )
          , (Green  , xRot (-pi/2) )
          , (Blue   , yRot (-pi/2) )
          , (Orange , xRot (pi) )
          ]

viewModel :: MonadWidget t m => Dynamic t (Matrix Float) -> m ()
viewModel modelOrientation = do
    faceMap <- mapDyn faceViews modelOrientation
    listWithKey faceMap showFace
    return ()

view :: MonadWidget t m => Dynamic t (Matrix Float) -> m ()
view modelOrientation = do
    el "h1" $ text "Rotating Cube"
    elDynAttrSVG "svg"
        (constDyn $  DM.fromList [ ("width",  show size), ("height", show size) ])
        $ viewModel modelOrientation

main = mainWidget $ do
    let initialOrientation = xRot (pi/4) `multStd2` zRot (atan(1/sqrt(2)))
        update _ modelOrientation = modelOrientation `multStd2` (yRot (rotationStep) )

    tick <- tickLossy  updateFrequency =<< liftIO getCurrentTime
    rec
        view modelOrientation
        modelOrientation <- foldDyn update initialOrientation tick
    return ()

-- At end because of Rosetta Code handling of unmatched quotes.
elDynAttrSVG a2 a3 a4 = do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") a2 a3 a4
    return ()
```


Link to live demo: https://dc25.github.io/drawRotatingCubeHaskell/


## Java

[[File:rotating_cube_java.png|200px|thumb|right]]

```java
import java.awt.*;
import java.awt.event.ActionEvent;
import static java.lang.Math.*;
import javax.swing.*;

public class RotatingCube extends JPanel {
    double[][] nodes = {{-1, -1, -1}, {-1, -1, 1}, {-1, 1, -1}, {-1, 1, 1},
    {1, -1, -1}, {1, -1, 1}, {1, 1, -1}, {1, 1, 1}};

    int[][] edges = {{0, 1}, {1, 3}, {3, 2}, {2, 0}, {4, 5}, {5, 7}, {7, 6},
    {6, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};

    public RotatingCube() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);

        scale(100);
        rotateCube(PI / 4, atan(sqrt(2)));

        new Timer(17, (ActionEvent e) -> {
            rotateCube(PI / 180, 0);
            repaint();
        }).start();
    }

    final void scale(double s) {
        for (double[] node : nodes) {
            node[0] *= s;
            node[1] *= s;
            node[2] *= s;
        }
    }

    final void rotateCube(double angleX, double angleY) {
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

        for (double[] node : nodes)
            g.fillOval((int) round(node[0]) - 4, (int) round(node[1]) - 4, 8, 8);
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
            f.setTitle("Rotating Cube");
            f.setResizable(false);
            f.add(new RotatingCube(), BorderLayout.CENTER);
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

        var nodes = [[-1, -1, -1], [-1, -1, 1], [-1, 1, -1], [-1, 1, 1],
        [1, -1, -1], [1, -1, 1], [1, 1, -1], [1, 1, 1]];

        var edges = [[0, 1], [1, 3], [3, 2], [2, 0], [4, 5], [5, 7], [7, 6],
        [6, 4], [0, 4], [1, 5], [2, 6], [3, 7]];

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

        scale(200, 200, 200);
        rotateCuboid(Math.PI / 4, Math.atan(Math.sqrt(2)));

        setInterval(function() {
            rotateCuboid(Math.PI / 180, 0);
            drawCuboid();
        }, 17);

    </script>

</body>
</html>
```



## Julia

Run at the Julia REPL command line.

```Julia
using Makie

N = 40
interval = 0.10

scene = mesh(FRect3D(Vec3f0(-0.5), Vec3f0(1)), color = :skyblue2)
rect = scene[end]

for rad in 0.5:1/N:8.5
    arr = normalize([cospi(rad/2), 0, sinpi(rad/2), -sinpi(rad/2)])
    rotate!(rect, Quaternionf0(arr[1], arr[2], arr[3], arr[4]))
    sleep(interval)
end
```



## Kotlin

```scala
// version 1.1

import java.awt.*
import javax.swing.*

class RotatingCube : JPanel() {
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

    init {
        preferredSize = Dimension(640, 640)
        background = Color.white
        scale(100.0)
        rotateCube(Math.PI / 4.0, Math.atan(Math.sqrt(2.0)))
        Timer(17) {
            rotateCube(Math.PI / 180.0, 0.0)
            repaint()
        }.start()
    }

    private fun scale(s: Double) {
        for (node in nodes) {
            node[0] *= s
            node[1] *= s
            node[2] *= s
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
        f.title = "Rotating cube"
        f.isResizable = false
        f.add(RotatingCube(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.isVisible = true
    }
}
```



## Maple


```maple
plots:-display(
    seq(
        plots:-display(
            plottools[cuboid]( [0,0,0], [1,1,1] ),
        axes=none, scaling=constrained, orientation=[0,45,i] ),
    i = 0..360, 20 ),
insequence=true );
```



## Mathematica


```Mathematica
Dynamic[
    Graphics3D[
      GeometricTransformation[
       GeometricTransformation[Cuboid[], RotationTransform[Pi/4, {1, 1, 0}]],
       RotationTransform[Clock[2 Pi], {0, 0, 1}]
      ],
      Boxed -> False]]
```



## Perl


```perl
#!/usr/bin/perl

use strict;              # http://www.rosettacode.org/wiki/Draw_a_rotating_cube
use warnings;
use Tk;
use Time::HiRes qw( time );

my $size = 600;
my $wait = int 1000 / 30;
my ($height, $width) = ($size, $size * sqrt 8/9);
my $mid = $width / 2;
my $rot = atan2(0, -1) / 3;                   # middle corners every 60 degrees

my $mw = MainWindow->new;
my $c = $mw->Canvas(-width => $width, -height => $height)->pack;
$c->Tk::bind('<ButtonRelease>' => sub {$mw->destroy});          # click to exit
draw();
MainLoop;

sub draw
  {
  my $angle = time - $^T;                    # full rotation every 2*PI seconds
  my @points = map { $mid + $mid * cos $angle + $_ * $rot,
    $height * ($_ % 2 + 1) / 3 } 0 .. 5;
  $c->delete('all');
  $c->createLine( @points[-12 .. 1], $mid, 0, -width => 5,);
  $c->createLine( @points[4, 5], $mid, 0, @points[8, 9], -width => 5,);
  $c->createLine( @points[2, 3], $mid, $height, @points[6, 7], -width => 5,);
  $c->createLine( $mid, $height, @points[10, 11], -width => 5,);
  $mw->after($wait, \&draw);
  }
```


## Perl 6

Perl6 has no native graphics libraries built in, but makes it fairly easy to bind to third party libraries. Here we'll use bindings to [[wp:Libcaca|Libcaca]], the '''C'''olor '''A'''S'''C'''II '''A'''rt library to generate a rotating cube in an ASCII terminal.


```perl6
use lib 'lib';
use Terminal::Caca;
given my $canvas = Terminal::Caca.new {
    .title('Rosetta Code - Rotating cube - Press any key to exit');

    sub scale-and-translate($x, $y, $z) {
        $x * 5 / ( 5 + $z ) * 15 + 40,
        $y * 5 / ( 5 + $z ) *  7 + 15,
        $z;
    }

    sub rotate3d-x( $x, $y, $z, $angle ) {
        my ($cosθ, $sinθ) = cis( $angle * π / 180.0 ).reals;
        $x,
        $y * $cosθ - $z * $sinθ,
        $y * $sinθ + $z * $cosθ;
    }

    sub rotate3d-y( $x, $y, $z, $angle ) {
        my ($cosθ, $sinθ) = cis( $angle * π / 180.0 ).reals;
        $x * $cosθ - $z * $sinθ,
        $y,
        $x * $sinθ + $z * $cosθ;
    }

    sub rotate3d-z( $x, $y, $z, $angle ) {
        my ($cosθ, $sinθ) = cis( $angle * π / 180.0 ).reals;
        $x * $cosθ - $y * $sinθ,
        $x * $cosθ + $y * $sinθ,
        $z;
    }

    # Unit cube from polygon mesh, aligned to axes
    my @mesh =
      [ [1, 1, -1], [-1, -1, -1], [-1,  1, -1] ], # far face
      [ [1, 1, -1], [-1, -1, -1], [ 1, -1, -1] ],
      [ [1, 1,  1], [-1, -1,  1], [-1,  1,  1] ], # near face
      [ [1, 1,  1], [-1, -1,  1], [ 1, -1,  1] ];
      @mesh.push: [$_».rotate( 1)] for @mesh[^4]; # positive and
      @mesh.push: [$_».rotate(-1)] for @mesh[^4]; # negative rotations

    # Rotate to correct orientation for task
    for ^@mesh X ^@mesh[0] -> ($i, $j) {
        @(@mesh[$i;$j]) = rotate3d-x |@mesh[$i;$j], 45;
        @(@mesh[$i;$j]) = rotate3d-z |@mesh[$i;$j], 40;
    }

    my @colors = red, blue, green, cyan, magenta, yellow;

    loop {
        for ^359 -> $angle {
            .color( white, white );
            .clear;

            # Flatten 3D into 2D and rotate for all faces
            my @faces-z;
            my $c-index = 0;
            for @mesh -> @triangle {
                my @points;
                my $sum-z = 0;
                for @triangle -> @node {
                    my ($px, $py, $z) = scale-and-translate |rotate3d-y |@node, $angle;
                    @points.append: $px.Int, $py.Int;
                    $sum-z += $z;
                }

                @faces-z.push: %(
                    color  => @colors[$c-index++ div 2],
                    points => @points,
                    avg-z  => $sum-z / +@points;
                );
            }

            # Draw all faces
            # Sort by z to draw farthest first
            for @faces-z.sort( -*.<avg-z> ) -> %face {
                # Draw filled triangle
                .color( %face<color>, %face<color> );
                .fill-triangle( |%face<points> );
                # And frame
                .color( black, black );
                .thin-triangle( |%face<points> );
            }

            .refresh;
            exit if .wait-for-event(key-press);
        }
    }

    # Cleanup on scope exit
    LEAVE {
        .cleanup;
    }
}
```



## Phix

```Phix
--
-- demo\rosetta\DrawRotatingCube.exw
--
include pGUI.e

Ihandle canvas
cdCanvas cd_canvas

--
-- define 8 corners equidistant from {0,0,0}:
--
--          6-----2
--      5-----1   3
--      8-----4
--
-- ie the right face is 1-2-3-4 clockwise, and the left face
--  is 5-6-7-8 counter-clockwise (unless using x-ray vision).
--
enum X, Y, Z
constant l = 100
constant corners = {{+l,+l,+l},
                    {+l,+l,-l},
                    {+l,-l,-l},
                    {+l,-l,+l},
                    {-l,+l,+l},
                    {-l,+l,-l},
                    {-l,-l,-l},
                    {-l,-l,+l}}

constant faces = {{CD_RED,      1,2,3,4},   -- right
                  {CD_YELLOW,   1,5,6,2},   -- top
                  {CD_GREEN,    1,4,8,5},   -- front
                  {CD_BLUE,     2,3,7,6},   -- back
                  {CD_WHITE,    3,4,8,7},   -- btm
                  {CD_ORANGE,   5,6,7,8}}   -- left

atom ry = 0 -- rotation angle, 0..359, on a timer

constant naxes = {{Y,Z},    -- (rotate about the X-axis)
                  {X,Z},    -- (rotate about the Y-axis)
                  {X,Y}}    -- (rotate about the Z-axis)

function rotate(sequence points, atom angle, integer axis)
--
-- rotate points by the specified angle about the given axis
--
    atom radians = angle*CD_DEG2RAD,
         sin_t = sin(radians),
         cos_t = cos(radians)
    integer {nx,ny} = naxes[axis]
    for i=1 to length(points) do
        atom x = points[i][nx],
             y = points[i][ny]
        points[i][nx] = x * cos_t - y * sin_t
        points[i][ny] = y * cos_t + x * sin_t
    end for
    return points
end function

function projection(sequence points, atom d)
--
-- project points from {0,0,d} onto the perpendicular plane through {0,0,0}
--
    for i=1 to length(points) do
        atom {x,y,z} = points[i]
        points[i][X] = x/(1-z/d)
        points[i][Y] = y/(1-z/d)
    end for
    return points
end function

function nearest(sequence points)
--
-- return the index of the nearest point (highest z value)
--
    integer np = 1
    atom maxz = points[1][Z]
    for i=2 to length(points) do
        atom piz = points[i][Z]
        if piz>maxz then
            maxz = piz
            np = i
        end if
    end for
    return np
end function

procedure vertices(integer wx, wh, sequence points, face)
-- (common code for line/fill drawing)
    for i=2 to length(face) do
        integer fi = face[i]
        cdCanvasVertex(cd_canvas,wx+points[fi][X],wh-points[fi][Y])
    end for
end procedure

procedure draw_cube(integer wx, wh)
    sequence points = corners
    points = rotate(points,45,X)    -- (cube should now look like a H)
    atom zr = 90-arctan(sqrt(2))*CD_RAD2DEG -- (about 35 degrees)
    points = rotate(points,zr,Z)    -- (cube should now look like an italic H)
    points = rotate(points,ry,Y)    -- (timed, two corners should remain static)
    points = projection(points,1000)
    integer np = nearest(points)
    --
    -- find the three faces that contain the nearest point,
    -- then order by/draw them furthest diag away first.
    --  (one of them, and theoretically two but not at the
    --   rotations in use, may be completely obscured, due
    --   to the effects of the perspective projection.)
    --
    sequence faceset = {}
    for i=1 to length(faces) do
        sequence fi = faces[i]
        integer k = find(np,fi)
        if k then
            integer diag = mod(k,4)+2
            diag = fi[diag]
            faceset = append(faceset,{points[diag][Z],i})
        end if
    end for
    faceset = sort(faceset)
    for i=1 to length(faceset) do
        integer fdx = faceset[i][2]
        sequence fi = faces[fdx]
        cdCanvasSetForeground(cd_canvas,fi[1])
        -- draw edges (anti-aliased)
        cdCanvasBegin(cd_canvas,CD_CLOSED_LINES)
        vertices(wx,wh,points,fi)
        cdCanvasEnd(cd_canvas)
        -- fill sides (else would get bresenham edges)
        cdCanvasBegin(cd_canvas,CD_FILL)
        vertices(wx,wh,points,fi)
        cdCanvasEnd(cd_canvas)
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
    cdCanvasSetBackground(cd_canvas, CD_PARCHMENT)
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

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

function timer_cb(Ihandle /*ih*/)
    ry = mod(ry+359,360)
    IupRedraw(canvas)
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
    Ihandle dlg = IupDialog(IupVbox({canvas}))
    IupSetAttribute(dlg,"TITLE","Draw a Rotating Cube");
    IupSetCallback(dlg, "K_ANY",  Icallback("esc_close"))
    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    Ihandle hTimer = IupTimer(Icallback("timer_cb"), 40)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PostScript

'''Don't send this to your printer!'''

```postscript
%!PS-Adobe-3.0
%%BoundingBox: 0 0 400 400

/ed { exch def } def
/roty { dup sin /s ed cos /c ed [[c 0 s neg] [0 1 0] [s 0 c]] } def
/rotz { dup sin /s ed cos /c ed [[c s neg 0] [s c 0] [0 0 1]] } def
/dot { /a ed /b ed
	a 0 get b 0 get mul
	a 1 get b 1 get mul
	a 2 get b 2 get mul
	add add } def

/mmul {	/v ed [exch {v dot} forall] } def
/transall { /m ed [exch {m exch mmul}forall] } def

/vt
	[[1  1  1] [-1  1  1]
	 [1 -1  1] [-1 -1  1]
	 [1  1 -1] [-1  1 -1]
	 [1 -1 -1] [-1 -1 -1]]
	-45 roty transall
	2 sqrt 1 atan rotz transall
def

/xy { exch get {} forall pop } def
/page {
	/a ed /v vt a roty transall def
	0 setlinewidth 100 100 scale 2 2 translate
	/edge { v xy moveto v xy lineto stroke } def

	0 1 2 3 4 5 6 7 0 2 1 3 4 6 5 7 0 4 1 5 2 6 3 7
	1 1 12 { pop edge } for
	showpage
} def

0 {3.2 add dup page } loop
%%EOF
```



## Python


==={{libheader|VPython}}===
See also: [[Draw_a_cuboid]]


### =Short version=


```python
from visual import *
scene.title = "VPython: Draw a rotating cube"

scene.range = 2
scene.autocenter = True

print "Drag with right mousebutton to rotate view."
print "Drag up+down with middle mousebutton to zoom."

deg45 = math.radians(45.0)  # 0.785398163397

cube = box()    # using defaults, see http://www.vpython.org/contents/docs/defaults.html
cube.rotate( angle=deg45, axis=(1,0,0) )
cube.rotate( angle=deg45, axis=(0,0,1) )

while True:                 # Animation-loop
    rate(50)
    cube.rotate( angle=0.005, axis=(0,1,0) )

```


<!--
'''Regular version'''
-->


## Racket


```racket
#lang racket/gui
(require math/matrix math/array)

(define (Rx θ)
  (matrix [[1.0    0.0        0.0]
           [0.0 (cos θ) (- (sin θ))]
           [0.0 (sin θ)    (cos θ)]]))

(define (Ry θ)
  (matrix [[   (cos θ)  0.0 (sin θ)]
           [      0.0   1.0    0.0 ]
           [(- (sin θ)) 0.0 (cos θ)]]))

(define (Rz θ)
  (matrix [[(cos θ) (- (sin θ)) 0.0]
           [(sin θ)    (cos θ)  0.0]
           [   0.0        0.0   1.0]]))

(define base-matrix
  (matrix* (identity-matrix 3 100.0)
           (Rx (- (/ pi 2) (atan (sqrt 2))))
           (Rz (/ pi 4.0))))

(define (current-matrix)
  (matrix* (Ry (/ (current-inexact-milliseconds) 1000.))
           base-matrix))

(define corners
  (for*/list ([x '(-1.0 1.0)]
              [y '(-1.0 1.0)]
              [z '(-1.0 1.0)])
    (matrix [[x] [y] [z]])))

(define lines
  '((0 1) (0 2) (0 4) (1 3) (1 5)
    (2 3) (2 6) (3 7) (4 5) (4 6)
    (5 7) (6 7)))

(define ox 200.)
(define oy 200.)

(define (draw-line dc a b)
  (send dc draw-line
        (+ ox (array-ref a #(0 0)))
        (+ oy (array-ref a #(1 0)))
        (+ ox (array-ref b #(0 0)))
        (+ oy (array-ref b #(1 0)))))

(define (draw-cube c dc)
  (define-values (w h) (send dc get-size))
  (set! ox (/ w 2))
  (set! oy (/ h 2))
  (define cs (for/vector ([c (in-list corners)])
               (matrix* (current-matrix) c)))
  (for ([l (in-list lines)])
    (match-define (list i j) l)
    (draw-line dc (vector-ref cs i) (vector-ref cs j))))

(define f (new frame%  [label "cube"]))
(define c (new canvas% [parent f] [min-width 400] [min-height 400] [paint-callback draw-cube]))
(send f show #t)

(send* (send c get-dc)
  (set-pen "black" 1 'solid)
  (set-smoothing 'smoothed))

(define (refresh)
  (send c refresh))

(define t (new timer% [notify-callback refresh] [interval 35] [just-once? #f]))
```



## Scala


### Java Swing Interoperability

```Scala
import java.awt.event.ActionEvent
import java.awt._

import javax.swing.{JFrame, JPanel, Timer}

import scala.math.{Pi, atan, cos, sin, sqrt}

object RotatingCube extends App {

  class RotatingCube extends JPanel {
    private val vertices: Vector[Array[Double]] =
      Vector(Array(-1, -1, -1), Array(-1, -1, 1), Array(-1, 1, -1),
        Array(-1, 1, 1), Array(1, -1, -1), Array(1, -1, 1), Array(1, 1, -1), Array(1, 1, 1))

    private val edges: Vector[(Int, Int)] =
      Vector((0, 1), (1, 3), (3, 2), (2, 0), (4, 5), (5, 7),
        (7, 6), (6, 4), (0, 4), (1, 5), (2, 6), (3, 7))

    setPreferredSize(new Dimension(640, 640))
    setBackground(Color.white)
    scale(100)
    rotateCube(Pi / 4, atan(sqrt(2)))

    new Timer(17, (_: ActionEvent) => {
      rotateCube(Pi / 180, 0)
      repaint()
    }).start()

    override def paintComponent(gg: Graphics): Unit = {
      def drawCube(g: Graphics2D): Unit = {
        g.translate(getWidth / 2, getHeight / 2)
        for {edge <- edges
             xy1: Array[Double] = vertices(edge._1)
             xy2: Array[Double] = vertices(edge._2)
             } {
          g.drawLine(xy1(0).toInt, xy1(1).toInt, xy2(0).toInt, xy2(1).toInt)
          g.fillOval(xy1(0).toInt -4, xy1(1).toInt - 4, 8, 8)
          g.setColor(Color.black)
        }
      }

      super.paintComponent(gg)
      val g: Graphics2D = gg.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      drawCube(g)
    }

    private def scale(s: Double): Unit = {
      for {node <- vertices
           i <- node.indices
           } node(i) *= s
    }

    private def rotateCube(angleX: Double, angleY: Double): Unit = {
      def sinCos(x: Double) = (sin(x), cos(x))

      val ((sinX, cosX), (sinY, cosY)) = (sinCos(angleX), sinCos(angleY))

      for {
        node <- vertices
        x: Double = node(0)
        y: Double = node(1)
      } {
        def f(p: Double, q: Double)(a: Double, b: Double) = a * p + b * q

        def fx(a: Double, b: Double) = f(cosX, sinX)(a, b)

        def fy(a: Double, b: Double) = f(cosY, sinY)(a, b)

        node(0) = fx(x, -node(2))
        val z = fx(node(2), x)
        node(1) = fy(y, -z)
        node(2) = fy(z, y)
      }
    }

  }

  new JFrame("Rotating Cube") {
    add(new RotatingCube(), BorderLayout.CENTER)
    pack()
    setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    setLocationRelativeTo(null)
    setResizable(false)
    setVisible(true)
  }

}
```



## Tcl

See also [[Draw_a_cuboid|Draw a cuboid]]. This implementation uses tcllib's [http://core.tcl.tk/tcllib/doc/trunk/embedded/www/tcllib/files/modules/math/linalg.html Linear Algebra] module for some matrix ops to handle the screen transform and (animated!) rotation.  Rendering is in a Tk canvas.

The *Matrix* procedure is something unique to Tcl:  it's essentially a control construct that leverages *expr* to make declaring matrices much more convenient than hand-rolling lists.

There is a bit of wander in the top and bottom points, which might just be due to rounding error in the cube's initial "rotation into position".

See [http://wiki.tcl.tk/14283 this wiki page] (and others linked from it) for many similar examples.


```Tcl
# matrix operation support:
package require math::linearalgebra
namespace import ::math::linearalgebra::matmul
namespace import ::math::linearalgebra::crossproduct
namespace import ::math::linearalgebra::dotproduct
namespace import ::math::linearalgebra::sub

# returns a cube as a list of faces,
# where each face is a list of (3space) points
proc make_cube {{radius 1}} {
    set dirs {
        A { 1  1  1}
        B { 1  1 -1}
        C { 1 -1 -1}
        D { 1 -1  1}
        E {-1  1  1}
        F {-1  1 -1}
        G {-1 -1 -1}
        H {-1 -1  1}
    }
    set faces {
        {A B C D}
        {D C G H}
        {H G F E}
        {E F B A}
        {A D H E}
        {C B F G}
    }
    lmap fa $faces {
        lmap dir $fa {
            lmap x [dict get $dirs $dir] {
                expr {1.0 * $x * $radius}
            }
        }
    }
}

# a matrix constructor
proc Matrix {m} {
    tailcall lmap row $m {
        lmap e $row {
            expr 1.0*($e)
        }
    }
}

proc identity {} {
    Matrix {
        {1 0 0}
        {0 1 0}
        {0 0 1}
    }
}

# some matrices useful for animation:
proc rotateZ {theta} {
    Matrix {
        { cos($theta) -sin($theta)  0 }
        { sin($theta)  cos($theta)  0 }
        { 0            0            1 }
    }
}
proc rotateY {theta} {
    Matrix {
        { sin($theta)  0  cos($theta) }
        { 0            1            0 }
        { cos($theta)  0 -sin($theta) }
    }
}
proc rotateX {theta} {
    Matrix {
        { 1            0            0 }
        { 0  cos($theta) -sin($theta) }
        { 0  sin($theta)  cos($theta) }
    }
}

proc camera {flen} {
    Matrix {
        { $flen  0      0 }
        { 0      $flen  0 }
        { 0      0      0 }
    }
}

proc render {canvas object} {

    set W   [winfo width  $canvas]
    set H   [winfo height $canvas]

    set fl  1.0
    set t   [expr {[clock microseconds] / 1000000.0}]

    set transform [identity]
    set transform [matmul $transform [rotateX [expr {atan(1)}]]]
    set transform [matmul $transform [rotateZ [expr {atan(1)}]]]

    set transform [matmul $transform [rotateY $t]]
    set transform [matmul $transform [camera $fl]]

    foreach face $object {
        # do transformations into screen space:
        set points [lmap p $face { matmul $p $transform }]
        # calculate a normal
        set o       [lindex $points 0]
        set v1 [sub [lindex $points 1] $o]
        set v2 [sub [lindex $points 2] $o]
        set normal [crossproduct $v1 $v2]

        set cosi   [dotproduct $normal {0 0 -1.0}]
        if {$cosi <= 0} { ;# rear-facing!
            continue
        }

        set points [lmap p $points {
            lassign $p x y
            list [expr {$x + $W/2}] [expr {$y + $H/2}]
        }]
        set points [concat {*}$points]
        $canvas create poly $points -outline black -fill red
    }
}

package require Tk
pack [canvas .c] -expand yes -fill both

proc tick {} {
    .c delete all
    render .c $::world
    after 50 tick
}
set ::world [make_cube 100]
tick
```


=={{header|TI-83 BASIC}}==

```ti83b
:-1→Xmin:1→Xmax
:-1→Ymin:1→Ymax
:AxesOff
:Degrees
:While 1
:For(X,0,359,5
:sin(X-120→I%
:sin(X→PV
:sin(X+120→FV
:Line(0,1,I%,.3
:Line(0,1,PV,.3
:Line(0,1,FV,.3
:Line(0,-1,-I%,-.3
:Line(0,-1,-PV,-.3
:Line(0,-1,-FV,-.3
:Line(.3,I%,-.3,-PV
:Line(.3,I%,-.3,-FV
:Line(.3,PV,-.3,-I%
:Line(.3,PV,-.3,-FV
:Line(.3,FV,-.3,-I%
:Line(.3,FV,-.3,-PV
:End
:End
```


I%, PV, and FV are all finance variables that can be found in the finance menu (inside the APPS menu on TI-83+ and up).
Finance variables are much faster than normal variables.


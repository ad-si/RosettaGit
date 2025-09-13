+++
title = "OpenGL/Utah Teapot"
description = ""
date = 2019-03-10T12:50:04Z
aliases = []
[extra]
id = 19861
[taxonomies]
categories = ["task"]
tags = []
+++

This is way to render classic Utah Teapot using OpenGL library.

See also: [https://en.wikipedia.org/wiki/Utah_teapot Utah Teapot wikipage].


## C

As well explained on the Wikipedia page ( link above ), the teapot played such an important role in the development of Computer Graphics, that Mark Kilgard honoured it by giving it it's own [https://www.opengl.org/resources/libraries/glut/spec3/node89.html#SECTION000129000000000000000 primitive drawing functions]. [http://freeglut.sourceforge.net/ freeglut] did even better and implemented the rest of the tea set, yes, the API also provides a cup and a spoon.

This implementation sticks to the task requirements and only shows the teapot, it's rotating, so you can enjoy it's magnificence from all possible viewpoints and perspectives.

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
	glutWireTeapot(0.5);
	glPopMatrix();
	glFlush();
}


void onIdle(){
	rot += 0.01;
	glutPostRedisplay();
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
	glutInitWindowSize(900,700);
	glutCreateWindow("The Amazing, Rotating Utah Teapot brought to you in OpenGL via freeglut.");
	init();
	glutDisplayFunc(display);
	glutIdleFunc(onIdle);
	glutMainLoop();
	return 0;
}

```



## Go

Although there are a number of third party Go bindings for OpenGL, the following uses 'cgo' to interface directly with the C libraries. 

```go
package main

/*
#cgo LDFLAGS: -lGL -lGLU -lglut
#include <stdlib.h>
#include <GL/freeglut.h>

extern void display();
extern void onIdle();

typedef void (*callback) ();

static inline callback displayFunc() {
    return display;
}

static inline callback idleFunc() {
    return onIdle;
}
*/
import "C"
import "unsafe"

var rot = 0.0
var matCol = [4]C.float{1, 0, 0, 0}

//export display
func display() {
    C.glClear(C.GL_COLOR_BUFFER_BIT | C.GL_DEPTH_BUFFER_BIT)
    C.glPushMatrix()
    C.glRotatef(30, 1, 1, 0)
    C.glRotatef(C.float(rot), 0, 1, 1)
    C.glMaterialfv(C.GL_FRONT, C.GL_DIFFUSE, &matCol[0])
    C.glutWireTeapot(0.5)
    C.glPopMatrix()
    C.glFlush()
}

//export onIdle
func onIdle() {
    rot += 0.01
    C.glutPostRedisplay()
}

func initialize() {
    white := [4]C.float{1, 1, 1, 0}
    shini := [1]C.float{70}
    C.glClearColor(0.5, 0.5, 0.5, 0)
    C.glShadeModel(C.GL_SMOOTH)
    C.glLightfv(C.GL_LIGHT0, C.GL_AMBIENT, &white[0])
    C.glLightfv(C.GL_LIGHT0, C.GL_DIFFUSE, &white[0])
    C.glMaterialfv(C.GL_FRONT, C.GL_SHININESS, &shini[0])
    C.glEnable(C.GL_LIGHTING)
    C.glEnable(C.GL_LIGHT0)
    C.glEnable(C.GL_DEPTH_TEST)
}

func main() {
    argc := C.int(0)
    C.glutInit(&argc, nil)
    C.glutInitDisplayMode(C.GLUT_SINGLE | C.GLUT_RGB | C.GLUT_DEPTH)
    C.glutInitWindowSize(900, 700)
    tl := "The Amazing, Rotating Utah Teapot brought to you in OpenGL via freeglut."
    tlc := C.CString(tl)
    C.glutCreateWindow(tlc)
    initialize()
    C.glutDisplayFunc(C.displayFunc())
    C.glutIdleFunc(C.idleFunc())
    C.glutMainLoop()
    C.free(unsafe.Pointer(tlc))
}
```



## J

Direct translation from C.

```J
NB. Teapot using freeglut
require '~Projects/freeglut/gldefs.ijs'

f=: 1.1-1.1
void=: 0$''

rot=: f+0
matCol=: f+1 0 0 0

cb1=: cdcb '+ x x'
cb2=: cdcb '+ x x x'

cdcallback=: 3 : 0
y=. 15!:17''
select. #y
  case. 1 do. display 0
  case. 2 do. onIdle 0
end.
)

display=: 3 : 0
glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)
glPushMatrix void
glRotatef((30+f);(1+f);(1+f);f)
glRotatef(rot;f;(1+f);(1+f))
glMaterialfv(GL_FRONT;GL_DIFFUSE;<matCol)
glutWireTeapot(0.5)
glPopMatrix void
glFlush void
)

onIdle=: 3 : 0
rot=: rot+0.01
glutPostRedisplay void
)

init=: 3 : 0
pos=. f+1,1,1,0
white=. f+1,1,1,0
shini=. ,f+70
 
glClearColor(0.5;0.5;0.5;f)
glShadeModel(GL_SMOOTH)
glLightfv(GL_LIGHT0;GL_AMBIENT;white)
glLightfv(GL_LIGHT0;GL_DIFFUSE;white)
glMaterialfv(GL_FRONT;GL_SHININESS;shini)
glEnable(GL_LIGHTING)
glEnable(GL_LIGHT0)
glEnable(GL_DEPTH_TEST)
)

main=: 3 : 0
argC=. ,2-2
argV=.<,0{a.

glutInit(argC;argV)
glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE;GLUT_ACTION_GLUTMAINLOOP_RETURNS)
glutInitDisplayMode(GLUT_SINGLE+GLUT_RGB+GLUT_DEPTH)
glutInitWindowSize(900;700)
glutCreateWindow(<'The Amazing, Rotating Utah Teapot brought to you in OpenGL via freeglut.')
init void
glutDisplayFunc(cb1)
glutIdleFunc(cb2)
glutMainLoop void
void
)
```



## Kotlin

Assuming that freeglut is already installed on your system in the default location(s), you first need to build opengl.klib using the following .def file and the cinterop tool:

```txt

// opengl.def
headers = /usr/include/GL/glut.h
compilerOpts = -I/usr/include
linkerOpts = -L/usr/lib/x86_64-linux-gnu -lglut -lGL -lGLU

```

You then need to compile the following Kotlin program, linking against opengl.klib, and run the resulting .kexe file to view the rotating teapot.

```scala
// Kotlin Native v0.6

import kotlinx.cinterop.*
import opengl.*

var rot = 0f
val matCol = floatArrayOf(1f, 0f, 0f, 0f)

fun display() {
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
    glPushMatrix()
    glRotatef(30f, 1f, 1f, 0f)
    glRotatef(rot, 0f, 1f, 1f)
    glMaterialfv(GL_FRONT, GL_DIFFUSE, matCol.toCValues())
    glutWireTeapot(0.5)
    glPopMatrix()
    glFlush()
}

fun onIdle() {
    rot += 0.1f
    glutPostRedisplay()
}

fun init() {
    val white = floatArrayOf(1f, 1f, 1f, 0f)
    val shini = floatArrayOf(70f)
    glClearColor(0.5f, 0.5f, 0.5f, 0f);
    glShadeModel(GL_SMOOTH)
    glLightfv(GL_LIGHT0, GL_AMBIENT, white.toCValues())
    glLightfv(GL_LIGHT0, GL_DIFFUSE, white.toCValues())
    glMaterialfv(GL_FRONT, GL_SHININESS, shini.toCValues())
    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)
    glEnable(GL_DEPTH_TEST)
}

fun main(args: Array<String>) {
    memScoped {
        val argc = alloc<IntVar>().apply { value = 0 }
        glutInit(argc.ptr, null) 
    }
    glutInitDisplayMode(GLUT_SINGLE or GLUT_RGB or GLUT_DEPTH)
    glutInitWindowSize(900, 700)
    val title = "The Amazing, Rotating Utah Teapot brought to you in OpenGL via freeglut."
    glutCreateWindow(title)
    init()
    glutDisplayFunc(staticCFunction(::display))
    glutIdleFunc(staticCFunction(::onIdle))
    glutMainLoop()
}
```



## Ol

```lisp

; initial data:
(define vertices '(
    (  0.2000  0.0000 2.70000 ) (  0.2000 -0.1120 2.70000 )
    (  0.1120 -0.2000 2.70000 ) (  0.0000 -0.2000 2.70000 )
    (  1.3375  0.0000 2.53125 ) (  1.3375 -0.7490 2.53125 )
    (  0.7490 -1.3375 2.53125 ) (  0.0000 -1.3375 2.53125 )
    (  1.4375  0.0000 2.53125 ) (  1.4375 -0.8050 2.53125 )
    (  0.8050 -1.4375 2.53125 ) (  0.0000 -1.4375 2.53125 )
    (  1.5000  0.0000 2.40000 ) (  1.5000 -0.8400 2.40000 )
    (  0.8400 -1.5000 2.40000 ) (  0.0000 -1.5000 2.40000 )
    (  1.7500  0.0000 1.87500 ) (  1.7500 -0.9800 1.87500 )
    (  0.9800 -1.7500 1.87500 ) (  0.0000 -1.7500 1.87500 )
    (  2.0000  0.0000 1.35000 ) (  2.0000 -1.1200 1.35000 )
    (  1.1200 -2.0000 1.35000 ) (  0.0000 -2.0000 1.35000 )
    (  2.0000  0.0000 0.90000 ) (  2.0000 -1.1200 0.90000 )
    (  1.1200 -2.0000 0.90000 ) (  0.0000 -2.0000 0.90000 )
    ( -2.0000  0.0000 0.90000 ) (  2.0000  0.0000 0.45000 )
    (  2.0000 -1.1200 0.45000 ) (  1.1200 -2.0000 0.45000 )
    (  0.0000 -2.0000 0.45000 ) (  1.5000  0.0000 0.22500 )
    (  1.5000 -0.8400 0.22500 ) (  0.8400 -1.5000 0.22500 )
    (  0.0000 -1.5000 0.22500 ) (  1.5000  0.0000 0.15000 )
    (  1.5000 -0.8400 0.15000 ) (  0.8400 -1.5000 0.15000 )
    (  0.0000 -1.5000 0.15000 ) ( -1.6000  0.0000 2.02500 )
    ( -1.6000 -0.3000 2.02500 ) ( -1.5000 -0.3000 2.25000 )
    ( -1.5000  0.0000 2.25000 ) ( -2.3000  0.0000 2.02500 )
    ( -2.3000 -0.3000 2.02500 ) ( -2.5000 -0.3000 2.25000 )
    ( -2.5000  0.0000 2.25000 ) ( -2.7000  0.0000 2.02500 )
    ( -2.7000 -0.3000 2.02500 ) ( -3.0000 -0.3000 2.25000 )
    ( -3.0000  0.0000 2.25000 ) ( -2.7000  0.0000 1.80000 )
    ( -2.7000 -0.3000 1.80000 ) ( -3.0000 -0.3000 1.80000 )
    ( -3.0000  0.0000 1.80000 ) ( -2.7000  0.0000 1.57500 )
    ( -2.7000 -0.3000 1.57500 ) ( -3.0000 -0.3000 1.35000 )
    ( -3.0000  0.0000 1.35000 ) ( -2.5000  0.0000 1.12500 )
    ( -2.5000 -0.3000 1.12500 ) ( -2.6500 -0.3000 0.93750 )
    ( -2.6500  0.0000 0.93750 ) ( -2.0000 -0.3000 0.90000 )
    ( -1.9000 -0.3000 0.60000 ) ( -1.9000  0.0000 0.60000 )
    (  1.7000  0.0000 1.42500 ) (  1.7000 -0.6600 1.42500 )
    (  1.7000 -0.6600 0.60000 ) (  1.7000  0.0000 0.60000 )
    (  2.6000  0.0000 1.42500 ) (  2.6000 -0.6600 1.42500 )
    (  3.1000 -0.6600 0.82500 ) (  3.1000  0.0000 0.82500 )
    (  2.3000  0.0000 2.10000 ) (  2.3000 -0.2500 2.10000 )
    (  2.4000 -0.2500 2.02500 ) (  2.4000  0.0000 2.02500 )
    (  2.7000  0.0000 2.40000 ) (  2.7000 -0.2500 2.40000 )
    (  3.3000 -0.2500 2.40000 ) (  3.3000  0.0000 2.40000 )
    (  2.8000  0.0000 2.47500 ) (  2.8000 -0.2500 2.47500 )
    (  3.5250 -0.2500 2.49375 ) (  3.5250  0.0000 2.49375 )
    (  2.9000  0.0000 2.47500 ) (  2.9000 -0.1500 2.47500 )
    (  3.4500 -0.1500 2.51250 ) (  3.4500  0.0000 2.51250 )
    (  2.8000  0.0000 2.40000 ) (  2.8000 -0.1500 2.40000 )
    (  3.2000 -0.1500 2.40000 ) (  3.2000  0.0000 2.40000 )
    (  0.0000  0.0000 3.15000 ) (  0.8000  0.0000 3.15000 )
    (  0.8000 -0.4500 3.15000 ) (  0.4500 -0.8000 3.15000 )
    (  0.0000 -0.8000 3.15000 ) (  0.0000  0.0000 2.85000 )
    (  1.4000  0.0000 2.40000 ) (  1.4000 -0.7840 2.40000 )
    (  0.7840 -1.4000 2.40000 ) (  0.0000 -1.4000 2.40000 )
    (  0.4000  0.0000 2.55000 ) (  0.4000 -0.2240 2.55000 )
    (  0.2240 -0.4000 2.55000 ) (  0.0000 -0.4000 2.55000 )
    (  1.3000  0.0000 2.55000 ) (  1.3000 -0.7280 2.55000 )
    (  0.7280 -1.3000 2.55000 ) (  0.0000 -1.3000 2.55000 )
    (  1.3000  0.0000 2.40000 ) (  1.3000 -0.7280 2.40000 )
    (  0.7280 -1.3000 2.40000 ) (  0.0000 -1.3000 2.40000 )))
    
(define Rim: '(
    ( 102 103 104 105   4   5   6   7
        8   9  10  11  12  13  14  15 )))
(define Body: '(
    (  12  13  14  15  16  17  18  19
       20  21  22  23  24  25  26  27 )
    (  24  25  26  27  29  30  31  32
       33  34  35  36  37  38  39  40 )))
(define Lid: '(
    (  96  96  96  96  97  98  99 100
      101 101 101 101   0   1   2   3 )
    (   0   1   2   3 106 107 108 109
      110 111 112 113 114 115 116 117 )))
(define Handle: '(
    (  41  42  43  44  45  46  47  48
       49  50  51  52  53  54  55  56 )
    (  53  54  55  56  57  58  59  60
       61  62  63  64  28  65  66  67 )))
(define Spout: '(
    (  68  69  70  71  72  73  74  75
       76  77  78  79  80  81  82  83 )
    (  80  81  82  83  84  85  86  87
       88  89  90  91  92  93  94  95 )))

; render pass:
(define knots '(0 0 0 0 1 1 1 1))

   (let ((render (lambda (surface)
                     (gluBeginSurface teapot)
                     (gluNurbsSurface teapot 8 knots 8 knots (* 4 3) 3  (fold append '() (map (lambda (n) (nth vertices n)) surface))  4 4 GL_MAP2_VERTEX_3)
                     (gluEndSurface teapot))))
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)
      (glScalef -1 1  1)
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)
      (glScalef -1 -1 1)
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)
      (glScalef -1 1  1)
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)

      (for-each render Handle:)
      (for-each render Spout:)
      (glScalef 1 -1 1)
      (for-each render Handle:)
      (for-each render Spout:))

```



## Phix

```Phix
include GL/gl.e
include GL/freeglut.e
 
atom rot = 0;
atom matCol = allocate(16)
    poke(matCol,
         atom_to_float32(1) &
         atom_to_float32(0) &
         atom_to_float32(0) &
         atom_to_float32(0)
        )
 
function display()
    glClear(or_bits(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT))
    glPushMatrix()
    glRotatef(30,1,1,0)
    glRotatef(rot,0,1,1)
    glMaterialfv(GL_FRONT,GL_DIFFUSE,matCol)
    glutWireTeapot(0.5)
    glPopMatrix()
    glFlush()
    return 0
end function
 
function onIdle()
    rot += 0.01
    glutPostRedisplay()
    return 0
end function
 
procedure init()
    atom white = allocate(16)
    poke(white,
         atom_to_float32(1) &
         atom_to_float32(1) &
         atom_to_float32(1) &
         atom_to_float32(0)
        )
    atom shini = allocate(4)
    poke(shini,
         atom_to_float32(70)
        )
 
    glClearColor(.5,.5,.5,0)
    glShadeModel(GL_SMOOTH)
    glLightfv(GL_LIGHT0,GL_AMBIENT,white)
    glLightfv(GL_LIGHT0,GL_DIFFUSE,white)
    glMaterialfv(GL_FRONT,GL_SHININESS,shini)
    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)
    glEnable(GL_DEPTH_TEST)
end procedure
 
procedure main()
    glutInit()
    glutInitDisplayMode(or_all({GLUT_SINGLE,GLUT_RGB,GLUT_DEPTH}))
    glutInitWindowSize(900,700)
    {} = glutCreateWindow("The Amazing, Rotating Utah Teapot brought to you in OpenGL via freeglut.")
    init()
    glutDisplayFunc(call_back(routine_id("display")))
    glutIdleFunc(call_back(routine_id("onIdle")))
    glutMainLoop()
end procedure

main()
```

To run this, you will need the freeglut package from [http://phix.x10.mx/pmwiki/pmwiki.php?n=Main.Freeglut-TheFreeOpenglUtilityToolkit PCAN]

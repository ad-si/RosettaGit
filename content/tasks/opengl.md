+++
title = "OpenGL"
description = ""
date = 2019-03-08T17:13:23Z
aliases = []
[extra]
id = 2073
[taxonomies]
categories = ["task", "3D"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "bacon",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "csharp",
  "d",
  "ec",
  "euphoria",
  "factor",
  "forth",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "liberty_basic",
  "lingo",
  "lua",
  "maxscript",
  "mercury",
  "nim",
  "ocaml",
  "ol",
  "oxygenbasic",
  "pascal",
  "perl",
  "phix",
  "picolisp",
  "pike",
  "purebasic",
  "python",
  "r",
  "racket",
  "ring",
  "ruby",
  "scala",
  "tcl",
]
+++

## Task

In this task, the goal is to display a smooth shaded triangle with OpenGL.

[[image:Triangle.png|right|thumb|150px|Triangle created using C example compiled with [[GCC]] 4.1.2 and [[freeglut3]].]]





## Ada

opengl.adb:

```Ada
with Lumen.Window;
with Lumen.Events;
with Lumen.Events.Animate;
with GL;
with GLU;

procedure OpenGL is

   The_Window : Lumen.Window.Handle;

   Program_Exit : Exception;

   -- simply exit this program
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
   begin
      raise Program_Exit;
   end;

   -- Resize the scene
   procedure Resize_Scene (Width, Height : in Natural) is
      use GL;
      use GLU;
   begin
      -- reset current viewport
      glViewport (0, 0, GLsizei (Width), GLsizei (Height));

      -- select projection matrix and reset it
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;

      -- calculate aspect ratio
      gluPerspective (45.0, GLdouble (Width) / GLdouble (Height), 0.1, 100.0);

      -- select modelview matrix and reset it
      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;
   end Resize_Scene;

   procedure Init_GL is
      use GL;
      use GLU;
   begin
      -- smooth shading
      glShadeModel (GL_SMOOTH);

      -- black background
      glClearColor (0.0, 0.0, 0.0, 0.0);

      -- depth buffer setup
      glClearDepth (1.0);
      -- enable depth testing
      glEnable (GL_DEPTH_TEST);
      -- type of depth test
      glDepthFunc (GL_LEQUAL);

      glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
   end Init_GL;

   -- Resize and Initialize the GL window
   procedure Resize_Handler (Event : in Lumen.Events.Event_Data) is
      Height : Natural := Event.Resize_Data.Height;
      Width  : Natural := Event.Resize_Data.Width;
   begin
      -- prevent div by zero
      if Height = 0 then
         Height := 1;
      end if;

      Resize_Scene (Width, Height);
   end;

   procedure Draw is
      use GL;
   begin
      -- clear screen and depth buffer
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      -- reset current modelview matrix
      glLoadIdentity;

      -- draw triangle
      glBegin (GL_TRIANGLES);
      glColor3f (1.0, 0.0, 0.0);
      glVertex3f ( 0.0,  1.0, 0.0);
      glColor3f (0.0, 1.0, 0.0);
      glVertex3f (-1.0, -1.0, 0.0);
      glColor3f (0.0, 0.0, 1.0);
      glVertex3f ( 1.0, -1.0, 0.0);
      glEnd;
   end Draw;

   procedure Frame_Handler (Frame_Delta : in Duration) is
   begin
      Draw;
      Lumen.Window.Swap (The_Window);
   end Frame_Handler;

begin

   Lumen.Window.Create (Win    => The_Window,
                        Name   => "OpenGL Demo",
                        Width  => 640,
                        Height => 480,
                        Events => (Lumen.Window.Want_Key_Press => True,
                                   Lumen.Window.Want_Exposure  => True,
                                   others                      => False));

   Resize_Scene (640, 480);
   Init_GL;

   Lumen.Events.Animate.Select_Events
     (Win   => The_Window,
      FPS   => Lumen.Events.Animate.Flat_Out,
      Frame => Frame_Handler'Unrestricted_Access,
      Calls => (Lumen.Events.Resized       => Resize_Handler'Unrestricted_Access,
                Lumen.Events.Close_Window  => Quit_Handler'Unrestricted_Access,
                others                     => Lumen.Events.No_Callback));

exception
   when Program_Exit =>
      null; -- normal termination
end OpenGL;
```



## AutoHotkey


```AutoHotkey
hOpenGL32 := DllCall("LoadLibrary", "Str", "opengl32")
Gui, +LastFound +Resize
hDC := DllCall("GetDC", "uInt", WinExist())

VarSetCapacity(pfd, 40, 0)
NumPut(40, pfd, 0, "uShort")
NumPut(1, pfd, 2, "uShort")
NumPut(37, pfd, 4, "uInt")
NumPut(24, pfd, 9, "uChar")
NumPut(16, pfd, 23, "uChar")
DllCall("SetPixelFormat", "uInt", hDC, "uInt", DllCall("ChoosePixelFormat", "uInt", hDC, "uInt", &pfd), "uInt", &pfd)

hRC := DllCall("opengl32\wglCreateContext", "uInt", hDC)
DllCall("opengl32\wglMakeCurrent", "uInt", hDC, "uInt", hRC)
Gui, Show, w640 h480, Triangle
OnExit, ExitSub
SetTimer, Paint, 50
return


Paint:
DllCall("opengl32\glClearColor", "Float", 0.3, "Float", 0.3, "Float", 0.3, "Float", 0)
DllCall("opengl32\glClear", "uInt", 0x4100) ;GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT

DllCall("opengl32\glShadeModel", "uInt", 0x1D01) ;GL_SMOOTH

DllCall("opengl32\glLoadIdentity")
DllCall("opengl32\glTranslatef", "Float", -15, "Float", -15, "Float", 0)

DllCall("opengl32\glBegin", "uInt", 0x0004) ;GL_TRIANGLES
DllCall("opengl32\glColor3f", "Float", 1, "Float", 0, "Float", 0)
DllCall("opengl32\glVertex2f", "Float", 0, "Float", 0)
DllCall("opengl32\glColor3f", "Float", 0, "Float", 1, "Float", 0)
DllCall("opengl32\glVertex2f", "Float", 30, "Float", 0)
DllCall("opengl32\glColor3f", "Float", 0, "Float", 0, "Float", 1)
DllCall("opengl32\glVertex2f", "Float", 0, "Float", 30)
DllCall("opengl32\glEnd")

DllCall("SwapBuffers", "uInt", hDC)
return


GuiSize:
DllCall("opengl32\glViewport", "Int", 0, "Int", 0, "Int", A_GuiWidth, "Int", A_GuiHeight)
DllCall("opengl32\glMatrixMode", "uInt", 0x1701) ;GL_PROJECTION
DllCall("opengl32\glLoadIdentity")
DllCall("opengl32\glOrtho", "Double", -30, "Double", 30, "Double", -30, "Double", 30, "Double", -30, "Double", 30)
DllCall("opengl32\glMatrixMode", "uInt", 0x1700) ;GL_MODELVIEW
return


GuiClose:
ExitApp


ExitSub:
DllCall("opengl32\wglMakeCurrent", "uInt", 0, "uInt", 0)
DllCall("opengl32\wglDeleteContext", "uInt", hRC)
DllCall("ReleaseDC", "uInt", hDC)
DllCall("FreeLibrary", "uInt", hOpenGL32)
ExitApp
```




## BaCon

BaCon allows embedding C code. This is an example with GLUT.

```qbasic>PRAGMA INCLUDE <GL/gl.h> <GL/freeglut.h

PRAGMA LDFLAGS GL glut

OPTION PARSE FALSE

SUB Triangle

    glViewport(0, 0, 640, 480)
    glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)

    glClearColor(0.0, 0.0, 0.0, 1.0)
    glClear(GL_COLOR_BUFFER_BIT)

    glTranslatef(-15.0, -15.0, 0.0)

    glBegin(GL_TRIANGLES)
    glColor3f(1.0, 0.0, 0.0)
    glVertex2f(0.0, 0.0)
    glColor3f(0.0, 1.0, 0.0)
    glVertex2f(30.0, 0.0)
    glColor3f(0.0, 0.0, 1.0)
    glVertex2f(0.0, 30.0)
    glEnd()

    glutSwapBuffers()

END SUB

glutInit(&argc, argv)
glutInitWindowSize(640, 480)
glutCreateWindow("Triangle")

glutDisplayFunc(Triangle)
glutMainLoop()
```



## BBC BASIC

```bbcbasic
      *FLOAT64

      SYS "LoadLibrary", "OPENGL32.DLL" TO opengl%
      SYS "GetProcAddress", opengl%, "wglCreateContext" TO `wglCreateContext`
      SYS "GetProcAddress", opengl%, "wglDeleteContext" TO `wglDeleteContext`
      SYS "GetProcAddress", opengl%, "wglMakeCurrent"   TO `wglMakeCurrent`
      SYS "GetProcAddress", opengl%, "glMatrixMode"     TO `glMatrixMode`
      SYS "GetProcAddress", opengl%, "glClear"          TO `glClear`
      SYS "GetProcAddress", opengl%, "glBegin"          TO `glBegin`
      SYS "GetProcAddress", opengl%, "glColor3dv"       TO `glColor3dv`
      SYS "GetProcAddress", opengl%, "glVertex2dv"      TO `glVertex2dv`
      SYS "GetProcAddress", opengl%, "glEnd"            TO `glEnd`

      MODE 8

      PFD_MAIN_PLANE = 0
      PFD_TYPE_RGBA = 0
      PFD_DOUBLEBUFFER = 1
      PFD_DRAW_TO_WINDOW = 4
      PFD_SUPPORT_OPENGL = &20

      GL_MODELVIEW  = &1700
      GL_TRIANGLES = 4
      GL_DEPTH_BUFFER_BIT = &00000100
      GL_COLOR_BUFFER_BIT = &00004000

      ON CLOSE PROCcleanup : QUIT
      ON ERROR PROCcleanup : SYS "MessageBox", @hwnd%, REPORT$, 0, 48 : QUIT

      DIM GLcolor{r#, g#, b#}, GLvertex{x#, y#}
      DIM pfd{nSize{l&,h&}, nVersion{l&,h&}, dwFlags%, iPixelType&, cColorBits&, \
      \       cRedBits&, cRedShift&, cGreenBits&, cGreenShift&, cBlueBits&, cBlueShift&, \
      \       cAlphaBits&, cAlphaShift&, cAccumBits&, cAccumRedBits&, cAccumGreenBits&, \
      \       cAccumBlueBits&, cAccumAlphaBits&, cDepthBits&, cStencilBits&, cAuxBuffers&, \
      \       iLayerType&, bReserved&, dwLayerMask%, dwVisibleMask%, dwDamageMask%}

      pfd.nSize.l& = DIM(pfd{})
      pfd.nVersion.l& = 1
      pfd.dwFlags% = PFD_DRAW_TO_WINDOW OR PFD_SUPPORT_OPENGL OR PFD_DOUBLEBUFFER
      pfd.dwLayerMask% = PFD_MAIN_PLANE
      pfd.iPixelType& = PFD_TYPE_RGBA
      pfd.cColorBits& = 24
      pfd.cDepthBits& = 16

      SYS "GetDC", @hwnd% TO ghDC%

      SYS "ChoosePixelFormat", ghDC%, pfd{} TO pixelformat%
      IF pixelformat% = 0 ERROR 100, "ChoosePixelFormat failed"

      SYS "SetPixelFormat", ghDC%, pixelformat%, pfd{} TO res%
      IF res% = 0 ERROR 100, "SetPixelFormat failed"

      SYS `wglCreateContext`, ghDC% TO ghRC%
      SYS `wglMakeCurrent`, ghDC%, ghRC%
      SYS `glMatrixMode`, GL_MODELVIEW

      REPEAT
        WAIT 2
        SYS `glClear`, GL_COLOR_BUFFER_BIT OR GL_DEPTH_BUFFER_BIT
        SYS `glBegin`, GL_TRIANGLES
        GLcolor.r# = 1.0 : GLcolor.g# = 0.0 : GLcolor.b# = 0.0
        SYS `glColor3dv`, GLcolor{}
        GLvertex.x# = 0.0 : GLvertex.y# = 0.8
        SYS `glVertex2dv`, GLvertex{}
        GLcolor.r# = 0.0 : GLcolor.g# = 1.0 : GLcolor.b# = 0.0
        SYS `glColor3dv`, GLcolor{}
        GLvertex.x# = 0.8 : GLvertex.y# = -0.8
        SYS `glVertex2dv`, GLvertex{}
        GLcolor.r# = 0.0 : GLcolor.g# = 0.0 : GLcolor.b# = 1.0
        SYS `glColor3dv`, GLcolor{}
        GLvertex.x# = -0.8 : GLvertex.y# = -0.8
        SYS `glVertex2dv`, GLvertex{}
        SYS `glEnd`
        SYS "SwapBuffers", ghDC%
      UNTIL FALSE
      END

      DEF PROCcleanup
      ON ERROR OFF
      ghRC% += 0 : IF ghRC% SYS `wglDeleteContext`, ghRC%  : ghRC% = 0
      ghDC% += 0 : IF ghDC% SYS "ReleaseDC", @hwnd%, ghDC% : ghDC% = 0
      ENDPROC
```



## C

In this example, we use [[:Category:GLUT|GLUT]] to create a window and handle the main loop in a portable way. Windowing systems like MS Windows and X11 have their own platform-specific ways of handling these things.


```cpp
#include <iostream>
#include <GL/gl.h>
#include <GL/glut.h>

void paint(void)
{
  glClearColor(0.3,0.3,0.3,0.0);
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

  glShadeModel(GL_SMOOTH);

  glLoadIdentity();
  glTranslatef(-15.0, -15.0, 0.0);

  glBegin(GL_TRIANGLES);
  glColor3f(1.0, 0.0, 0.0);
  glVertex2f(0.0, 0.0);
  glColor3f(0.0, 1.0, 0.0);
  glVertex2f(30.0, 0.0);
  glColor3f(0.0, 0.0, 1.0);
  glVertex2f(0.0, 30.0);
  glEnd();

  glFlush();
}

void reshape(int width, int height)
{
  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0);
  glMatrixMode(GL_MODELVIEW);
}

int main(int argc, char *argv[])
{
  glutInit(&argc, argv);
  glutInitWindowSize(640, 480);
  glutCreateWindow("Triangle");

  glutDisplayFunc(paint);
  glutReshapeFunc(reshape);

  glutMainLoop();

  return EXIT_SUCCESS;
}
```


## C#
C# example using the [http://www.opentk.com/ OpenTK] library, which is multiplatform and provides C# OpenGL bindings for .Net and Mono. This code creates its own window and draws the triangle into it.


```c#
using OpenTK;
using OpenTK.Graphics;
namespace OpenGLTest
{
    class Program
    {
        static void Main(string[] args)
        {
            //Create the OpenGL window
            GameWindow window = new GameWindow(640, 480, GraphicsMode.Default, "OpenGL Example");

            GL.MatrixMode(MatrixMode.Projection);
            GL.LoadIdentity();
            GL.Ortho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0);
            GL.MatrixMode(MatrixMode.Modelview);

            //Add event handler to render to the window when called
            window.RenderFrame += new RenderFrameEvent(a_RenderFrame);
            //Starts the window's updating/rendering events
            window.Run();
        }
        static void a_RenderFrame(GameWindow sender, RenderFrameEventArgs e)
        {
            GL.ClearColor(0.3f, 0.3f, 0.3f, 0f);
            GL.Clear(ClearBufferMask.ColorBufferBit | ClearBufferMask.DepthBufferBit);

            GL.ShadeModel(ShadingModel.Smooth);

            GL.LoadIdentity();
            GL.Translate(-15.0f, -15.0f, 0.0f);

            GL.Begin(BeginMode.Triangles);
            GL.Color3(1.0f, 0.0f, 0.0f);
            GL.Vertex2(0.0f, 0.0f);
            GL.Color3(0.0f, 1.0f, 0.0f);
            GL.Vertex2(30f, 0.0f);
            GL.Color3(0.0f, 0.0f, 1.0f);
            GL.Vertex2(0.0f, 30.0f);
            GL.End();
            //Swaps the buffers on the window so that what we draw becomes visible
            sender.SwapBuffers();
        }
    }
}
```



## Clojure

In this example, we use [http://github.com/ztellman/penumbra Penumbra], which is an idiomatic wrapper for OpenGL.

```lisp
(use 'penumbra.opengl)
(require '[penumbra.app :as app])

(defn init [state]
  (app/title! "Triangle")
  (clear-color 0.3 0.3 0.3 0)
  (shade-model :smooth)
  state)

(defn reshape [[x y w h] state]
  (ortho-view -30 30 -30 30 -30 30)
  (load-identity)
  (translate -15 -15)
  state)

(defn display [[dt time] state]
  (draw-triangles
    (color 1 0 0) (vertex 0 0)
    (color 0 1 0) (vertex 30 0)
    (color 0 0 1) (vertex 0 30)))

(app/start {:display display, :reshape reshape, :init init} {})
```



## D

opengl_sample.d:

```d
module opengl_sample; // file name + directory
import dglut.core, dglut.window, dglut.opengl;

void main() {
  with (new Canvas) {
    setName("Triangle");
    map;

    onResize = (Canvas self) { // A delegate literal that takes a parameter.
      with (self) glViewport(0, 0, width, height);
      MatrixMode.Projection.Identity; // For functions without parameters, the () can be omitted.
      glOrtho(-30, 30, -30, 30, -30, 30);
      MatrixMode.Modelview;
    };

    onDisplay=(Canvas self) {
      scope(exit) self.swap; // Scope guards ease exception-safe programming
      glClearColor(0.3f, 0.3f, 0.3f, 0f);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      glLoadIdentity;
      // A convenience wrapper around glTranslatef. Supports numbers, arrays and vectors.
      Translate(-15, -15, 0);
      // This is a delegate literal as well. Triangles is a wrapper around glBegin and glEnd.
      Triangles = {
        Color(1f, 0f, 0f); Vertex(0, 0);
        Color(0f, 1f, 0f); Vertex(30, 0);
        Color(0f, 0f, 1f); Vertex(0, 30);
      };
    };
  }
  loop;
}
```



## Common Lisp


The user may change the :type argument in draw-update to 'right, and re-evaluate to see the change take effect in realtime.

```lisp
(defun draw-triangle (x y &key (z 0) (type 'right))
  (case type
    (right
     (gl:with-primitive :triangles
       (gl:color 0 0 1)
       (gl:vertex x y z)
       (gl:color 0 1 0)
       (gl:vertex x (- y 1) z)
       (gl:color 1 0 0)
       (gl:vertex (+ x 1) (1- y) z)))
    (equilateral
     (gl:with-primitive :triangles
       (gl:color 0 0 1)
       (gl:vertex (+ x 0.5) y z)
       (gl:color 1 0 0)
       (gl:vertex (- x 0.5) (- y 1) z)
       (gl:color 0 1 0)
       (gl:vertex (+ x 1.5) (- y 1) z)))))

(defun draw-update ()
  (gl:clear :color-buffer :depth-buffer :color-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:color 1.0 1.0 1.0)
  (gl:translate 0 0 -2)

  (draw-triangle -0.5 0.5 :type 'equilateral))

(defun main-loop ()
  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:key key)
                     (cond ((sdl:key= key :sdl-key-escape)
                            (sdl:push-quit-event))))
    (:idle ()
           (draw-update)
           (sdl:update-display))))

(defun setup-gl (w h)
  (gl:clear-color 0.5 0.5 0.5 1)
  (gl:clear-depth 1)

  (gl:viewport 0 0 w h)

  (gl:depth-func :lequal)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:shade-model :smooth)
  (gl:enable :depth-test :cull-face)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 45 (/ w (max h 1)) 0.1 20)

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun triangle (&optional (w 640) (h 480))
  (sdl:with-init ()
    (sdl:set-gl-attribute :SDL-GL-DEPTH-SIZE 16)
    (sdl:window w h
                :bpp 32
                :flags sdl:sdl-opengl
                :title-caption "Rosettacode.org OpenGL Demo")
    (setup-gl w h)
    (setf (sdl:frame-rate) 2)
    (main-loop)))
```



## eC

```C>#include <GL/gl.h

import "ecere"

class GLTriangle : Window
{
   text = "Triangle";
   displayDriver = "OpenGL";
   background = activeBorder;
   nativeDecorations = true;
   borderStyle = sizable;
   hasMaximize = true, hasMinimize = true, hasClose = true;
   size = { 640, 480 };

   void OnRedraw(Surface surface)
   {
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glOrtho(-30, 30, -30, 30, -30, 30);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
      glTranslatef(-15, -15, 0);
      glShadeModel(GL_SMOOTH);

      glBegin(GL_TRIANGLES);
      glColor3f(1, 0, 0);
      glVertex2f(0, 0);
      glColor3f(0, 1, 0);
      glVertex2f(30, 0);
      glColor3f(0, 0, 1);
      glVertex2f(0, 30);
      glEnd();
   }
}

GLTriangle window {};
```




## Euphoria

Adapted from NeHe tutorial #3  nehe.gamedev.net

```euphoria

include get.e
include dll.e
include machine.e
include msgbox.e
include constants.ew
include GLfunc.ew
include GLconst.ew

without warning

atom hRC, hDC, hWnd, hInstance, ClassName
sequence keys keys = repeat(0,256)  -- array to hold key presses

integer active, fullscreen, retval
active = TRUE
fullscreen = TRUE
hRC = NULL
hDC = NULL
hWnd = NULL
hInstance = NULL

atom rtri, rquad
rtri = 0.0
rquad = 0.0

integer dmScreenSettings, WindowRect

procedure ReSizeGLScene(integer width, integer height)
    if height = 0 then
        height = 1
    end if
    c_proc(glViewport,{0,0,width,height})
    c_proc(glMatrixMode,{GL_PROJECTION})
    c_proc(glLoadIdentity,{})
    c_proc(gluPerspective,{45.0,width/height,0.1,100.0})
    c_proc(glMatrixMode,{GL_MODELVIEW})
    c_proc(glLoadIdentity,{})
end procedure

procedure InitGL()
    c_proc(glShadeModel,{GL_SMOOTH})
    c_proc(glClearColor,{0.0,0.0,0.0,0.0})
    c_proc(glClearDepth,{1.0})
    c_proc(glEnable,{GL_DEPTH_TEST})
    c_proc(glDepthFunc,{GL_LEQUAL})
    c_proc(glHint,{GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST})
end procedure

function DrawGLScene()
    c_proc(glClear, {or_bits(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT)})
    c_proc(glLoadIdentity, {})
    c_proc(glTranslatef, {-1.5,0.0,-6.0})
    c_proc(glRotatef, {rtri,0.0,1.0,0.0})
    c_proc(glBegin, {GL_TRIANGLES})
    c_proc(glColor3f, {1.0,0.0,0.0})
    c_proc(glVertex3f, {0.0,1.0,0.0})
    c_proc(glColor3f, {0.0,1.0,0.0})
    c_proc(glVertex3f, {-1.0,-1.0,0.0})
    c_proc(glColor3f, {0.0,0.0,1.0})
    c_proc(glVertex3f, {1.0,-1.0,0.0})
    c_proc(glEnd, {})
    c_proc(glLoadIdentity, {})
    c_proc(glTranslatef, {1.5,0.0,-6.0})
    c_proc(glRotatef, {rquad,1.0,0.0,0.0})
    c_proc(glColor3f, {0.5,0.5,1.0})
    c_proc(glBegin, {GL_QUADS})
    c_proc(glVertex3f, {1.0,1.0,0.0})
    c_proc(glVertex3f, {-1.0,1.0,0.0})
    c_proc(glVertex3f, {-1.0,-1.0,0.0})
    c_proc(glVertex3f, {1.0,-1.0,0.0})
    c_proc(glEnd, {})
    rtri += 0.2
    rquad -= 0.15
    return TRUE
end function

procedure KillGLWindow()
    if fullscreen then
        if c_func(ChangeDisplaySettingsA,{NULL,0}) then end if
        if c_func(ShowCursor,{TRUE}) then end if
    end if
    if hRC then
        if c_func(wglMakeCurrent,{NULL,NULL}) then end if
        if c_func(wglDeleteContext,{hRC}) then end if
        hRC = NULL
    end if
    if hRC and not c_func(ReleaseDC,{hWnd,hDC}) then
        hDC = NULL
    end if
    if hWnd and not c_func(DestroyWindow,{hWnd}) then
        hWnd = NULL
    end if
    if dmScreenSettings then
        free(dmScreenSettings)
    end if
    free(WindowRect)
end procedure

function WndProc(atom hWnd, integer uMsg, atom wParam, atom lParam)
    if uMsg = WM_ACTIVATE then
        if not floor(wParam/#10000) then
            active = TRUE
        else
            active = FALSE
        end if
    elsif  uMsg = WM_SYSCOMMAND then
        if wParam = SC_SCREENSAVE then end if
        if wParam = SC_MONITORPOWER then end if
    elsif uMsg = WM_CLOSE then
        c_proc(PostQuitMessage,{0})
    elsif uMsg = WM_KEYDOWN then
        keys[wParam] = TRUE
    elsif uMsg = WM_KEYUP then
        keys[wParam] = FALSE
    elsif uMsg = WM_SIZE then
        ReSizeGLScene(and_bits(lParam,#FFFF),floor(lParam/#10000))
    end if
    return c_func(DefWindowProcA,{hWnd, uMsg, wParam, lParam})
end function

integer wc wc = allocate(40)
function ClassRegistration()
integer WndProcAddress, id
    id = routine_id("WndProc")
    if id = -1 then
    puts(1, "routine_id failed!\n")
    abort(1)
    end if
    WndProcAddress = call_back(id)
    hInstance = c_func(GetModuleHandleA,{NULL})
    ClassName = allocate_string("OpenGL")
    poke4(wc,or_all({CS_HREDRAW, CS_VREDRAW, CS_OWNDC}))
    poke4(wc+4,WndProcAddress)
    poke4(wc+8,0)
    poke4(wc+12,0)
    poke4(wc+16,hInstance)
    poke4(wc+20,c_func(LoadIconA,{NULL,IDI_WINLOGO}))
    poke4(wc+24,c_func(LoadCursorA,{NULL, IDC_ARROW}))
    poke4(wc+28,NULL)
    poke4(wc+32,NULL)
    poke4(wc+36,ClassName)
    if not c_func(RegisterClassA,{wc}) then
        retval = message_box("Failed to register class","Error", or_bits(MB_OK,MB_ICONINFORMATION))
        return FALSE
    else
        return TRUE
    end if
end function

integer regd regd = FALSE
procedure CreateGLWindow(atom title, integer width, integer height, integer bits, integer fullscreenflag)
    atom PixelFormat, pfd, dwExStyle, dwStyle
    sequence s
    if regd = FALSE then
        if ClassRegistration() then
            regd = TRUE
        end if
    end if
    fullscreen = fullscreenflag
    if fullscreen then
        dmScreenSettings = allocate(156)
        mem_set(dmScreenSettings,0,156)
        s = int_to_bytes(156)
        poke(dmScreenSettings + 36,{s[1],s[2]})
        poke4(dmScreenSettings + 40,or_all({DM_BITSPERPEL,DM_PELSWIDTH,DM_PELSHEIGHT}))
        poke4(dmScreenSettings + 104, bits)
        poke4(dmScreenSettings + 108, width)
        poke4(dmScreenSettings + 112, height)
        if c_func(ChangeDisplaySettingsA,{dmScreenSettings,CDS_FULLSCREEN}) != DISP_CHANGE_SUCCESSFUL then
            if message_box("The requested fullscreen mode is not supported by\nyour video card. " &
                           "Use windowed mode instead?","Error", or_bits(MB_YESNO,MB_ICONEXCLAMATION)) = IDYES then
            else
                retval = message_box("Program will now close","Error",or_bits(MB_OK,MB_ICONSTOP))
            end if
        end if
    else
        dmScreenSettings = NULL
    end if
    if fullscreen then
        dwExStyle = WS_EX_APPWINDOW
        dwStyle = WS_POPUP
        if c_func(ShowCursor,{FALSE}) then end if
    else
        dwExStyle = or_bits(WS_EX_APPWINDOW,WS_EX_WINDOWEDGE)
        dwStyle = WS_OVERLAPPEDWINDOW
    end if
    WindowRect = allocate(16)
    poke4(WindowRect,0)
    poke4(WindowRect + 4,width)
    poke4(WindowRect + 8, 0)
    poke4(WindowRect + 12, height)
    if c_func(AdjustWindowRectEx,{WindowRect, dwStyle, FALSE, dwExStyle}) then end if
    hWnd = c_func(CreateWindowExA,{dwExStyle,  --extended window style
                                   ClassName,  --class
                                   title,      --window caption
                                   or_all({WS_CLIPSIBLINGS,WS_CLIPCHILDREN,dwStyle}),  --window style
                                   0,
                                   0,
                                   peek4u(WindowRect + 4) - peek4u(WindowRect),
                                   peek4u(WindowRect + 12) - peek4u(WindowRect + 8),
                                   NULL,
                                   NULL,
                                   hInstance,
                                   NULL})
    if hWnd = NULL then
        KillGLWindow()
        retval = message_box("Window creation error","Error",or_bits(MB_OK,MB_ICONEXCLAMATION))
    end if
    pfd = allocate(40)  --PIXELFORMATDESCRIPTOR
    mem_set(pfd,0,40)
    poke(pfd, 40)  --size of pfd structure
    poke(pfd + 2, 1) --version
    poke4(pfd + 4, or_all({PFD_DRAW_TO_WINDOW,PFD_SUPPORT_OPENGL,PFD_DOUBLEBUFFER})) --properties flags
    poke(pfd + 8, PFD_TYPE_RGBA)  --request an rgba format
    poke(pfd + 9, 24)  --select color depth
    poke(pfd + 23, 24)  --32bit Z-buffer

    hDC = c_func(GetDC,{hWnd})  --create GL device context to match window device context
    if not hDC then
        KillGLWindow()
        retval = message_box("Can't create a GL device context","Error",or_bits(MB_OK,MB_ICONEXCLAMATION))
    end if
    PixelFormat = c_func(ChoosePixelFormat,{hDC,pfd})  --find a pixel format matching PIXELFORMATDESCRIPTOR
    if not PixelFormat then
        KillGLWindow()
        retval = message_box("Can't find a suitable pixel format","Error",or_bits(MB_OK,MB_ICONEXCLAMATION))
    end if
    if not (c_func(SetPixelFormat,{hDC,PixelFormat,pfd})) then  --set the pixel format
        KillGLWindow()
        retval = message_box("Can't set the pixel format","Error",or_bits(MB_OK,MB_ICONEXCLAMATION))
    end if
    if not c_func(DescribePixelFormat, {hDC,PixelFormat,40,pfd}) then
        retval = message_box("Can't describe the pixel format","Error",or_bits(MB_OK,MB_ICONEXCLAMATION))
    end if
    hRC = c_func(wglCreateContext,{hDC})  --create GL rendering context
    if not hRC then
        KillGLWindow()
        retval = message_box("Can't create a GL rendering context","Error",or_bits(MB_OK,MB_ICONEXCLAMATION))
    end if
    if not (c_func(wglMakeCurrent,{hDC,hRC})) then  --make the GL rendering context active
        KillGLWindow()
        retval = message_box("Can't activate the GL rendering context","Error",or_bits(MB_OK,MB_ICONEXCLAMATION))
    end if
    retval = c_func(ShowWindow,{hWnd,SW_SHOW}) --show the window
    retval = c_func(SetForegroundWindow,{hWnd}) --set it to always be in foreground
    retval = c_func(SetFocus,{hWnd}) --give it focus
    ReSizeGLScene(width, height)  --draw the GL scene to match the window size
    InitGL()  --initialize OpenGL
end procedure

integer MSG MSG = allocate(28)
integer title title = allocate_string("OpenGL")
procedure WinMain()
integer done, msg_message
    done = FALSE
    if message_box("Would you like to run in fullscreen mode?","Start Fullscreen?",or_bits(MB_YESNO,MB_ICONQUESTION)) = IDNO then
        fullscreen = FALSE
    else
        fullscreen = TRUE
    end if
    CreateGLWindow(title,640,480,24,fullscreen)
    while not done do
        if c_func(PeekMessageA,{MSG,NULL,0,0,PM_REMOVE}) then
            msg_message = peek4u(MSG+4)
            if msg_message = WM_QUIT then
                done = TRUE
            else
                retval = c_func(TranslateMessage,{MSG})
                retval = c_func(DispatchMessageA,{MSG})
            end if
        else
            if ((active and not DrawGLScene()) or keys[VK_ESCAPE]) then
                done = TRUE
            else
                retval = c_func(SwapBuffers,{hDC})
                if keys[VK_F1] then
                    keys[VK_F1] = FALSE
                    KillGLWindow()
                    if fullscreen = 0 then
                        fullscreen = 1
                    else
                        fullscreen = 0
                    end if
                    CreateGLWindow(title,640,480,24,fullscreen)
                end if
            end if
        end if
    end while
    KillGLWindow()
end procedure

WinMain()

```



## Factor

Translated from C

```factor
USING: kernel math math.rectangles opengl.gl sequences ui
ui.gadgets ui.render ;
IN: rosettacode.opengl

TUPLE: triangle-gadget < gadget ;

: reshape ( width height -- )
    [ 0 0 ] 2dip glViewport
    GL_PROJECTION glMatrixMode
    glLoadIdentity
    -30.0 30.0 -30.0 30.0 -30.0 30.0 glOrtho
    GL_MODELVIEW glMatrixMode ;

: paint ( -- )
    0.3 0.3 0.3 0.0 glClearColor
    GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT bitor glClear
    GL_SMOOTH glShadeModel
    glLoadIdentity
    -15.0 -15.0 0.0 glTranslatef
    GL_TRIANGLES glBegin
    1.0 0.0 0.0 glColor3f 0.0 0.0 glVertex2f
    0.0 1.0 0.0 glColor3f 30.0 0.0 glVertex2f
    0.0 0.0 1.0 glColor3f 0.0 30.0 glVertex2f
    glEnd
    glFlush ;

M: triangle-gadget pref-dim* drop { 640 480 } ;
M: triangle-gadget draw-gadget*
    rect-bounds nip first2 reshape paint ;

: triangle-window ( -- )
   [ triangle-gadget new "Triangle" open-window ] with-ui ;
MAIN: triangle-window

```



## Forth

triangle.fs:

```forth
import glconst import float
glconst also float also opengl also
```


triangle.m:

```forth
#! xbigforth
\ automatic generated code
\ do not edit

also editor also minos also forth

include triangle.fs
component class triangle
public:
  early widget
  early open
  early dialog
  early open-app
 ( [varstart] )  ( [varend] )
how:
  : open     new DF[ 0 ]DF s" Triangle" open-component ;
  : dialog   new DF[ 0 ]DF s" Triangle" open-dialog ;
  : open-app new DF[ 0 ]DF s" Triangle" open-application ;
class;

triangle implements
 ( [methodstart] )  ( [methodend] )
  : widget  ( [dumpstart] )
        GL[ ^ glcanvas with
0 0 w @ h @ glViewport
GL_PROJECTION glMatrixMode
glLoadIdentity
-30e 30e -30e 30e -30e 30e glOrtho
GL_MODELVIEW glMatrixMode
0.3e 0.3e 0.3e 0.0e glClearColor
GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT or glClear
GL_SMOOTH glShadeModel
glLoadIdentity
-15e -15e 0e glTranslatef
GL_TRIANGLES glBegin
1e 0e 0e glColor3f
0e 0e glVertex2f
0e 1e 0e glColor3f
30e 0e glVertex2f
0e 0e 1e glColor3f
0e 30e glVertex2f
glEnd
glFlush
endwith ]GL ( MINOS ) ^^ CK[ ( x y b n -- ) 2drop 2drop ]CK ( MINOS ) $280 $1 *hfil $1E0 $1 *vfil glcanvas new
      &1 vabox new
    ( [dumpend] ) ;
  : init  ^>^^  assign  widget 1 :: init ;
class;

: main
  triangle open-app
  $1 0 ?DO  stop  LOOP bye ;
script? [IF]  main  [THEN]
previous previous previous
```



## Go

This program was also tested on Windows 10 but ''may'' only work if you comment out the marked line. This is because gl.Init() may produce an initialization error which is non-critical as far as this program is concerned. The reason for this error is unknown (see [[https://rosettacode.org/wiki/Talk:OpenGL Talk page]]).

```go
package main

import (
    gl "github.com/chsc/gogl/gl21"
    "github.com/go-gl/glfw/v3.2/glfw"
    "log"
    "runtime"
)

// Window dimensions.
const (
    Width  = 640
    Height = 480
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    // OpenGL requires a dedicated OS thread.
    runtime.LockOSThread()
    defer runtime.UnlockOSThread()

    err := glfw.Init()
    check(err)
    defer glfw.Terminate()

    // Open window with the specified dimensions.
    window, err := glfw.CreateWindow(Width, Height, "Triangle", nil, nil)
    check(err)

    window.MakeContextCurrent()

    err = gl.Init()
    check(err) /* may need to comment out this line for this program to work on Windows 10 */

    // Initiate viewport.
    resize(Width, Height)

    // Register that we are interested in receiving close and resize events.
    window.SetCloseCallback(func(w *glfw.Window) {
        return
    })
    window.SetSizeCallback(func(w *glfw.Window, width, height int) {
        resize(width, height)
    })

    for !window.ShouldClose() {
        draw()
        window.SwapBuffers()
        glfw.PollEvents()
    }
}

// resize resizes the window to the specified dimensions.
func resize(width, height int) {
    gl.Viewport(0, 0, gl.Sizei(width), gl.Sizei(height))
    gl.MatrixMode(gl.PROJECTION)
    gl.LoadIdentity()
    gl.Ortho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
    gl.MatrixMode(gl.MODELVIEW)
}

// draw draws the triangle.
func draw() {
    gl.ClearColor(0.3, 0.3, 0.3, 0.0)
    gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

    gl.ShadeModel(gl.SMOOTH)

    gl.LoadIdentity()
    gl.Translatef(-15.0, -15.0, 0.0)

    gl.Begin(gl.TRIANGLES)

    gl.Color3f(1.0, 0.0, 0.0)
    gl.Vertex2f(0.0, 0.0)

    gl.Color3f(0.0, 1.0, 0.0)
    gl.Vertex2f(30.0, 0.0)

    gl.Color3f(0.0, 0.0, 1.0)
    gl.Vertex2f(0.0, 30.0)

    gl.End()

    gl.Flush()
}
```



## Haskell


```haskell
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main = do
  getArgsAndInitialize
  createWindow "Triangle"
  displayCallback $= display

  matrixMode $= Projection
  loadIdentity
  ortho2D 0 30 0 30
  matrixMode $= Modelview 0

  mainLoop

display = do
  clear [ColorBuffer]
  renderPrimitive Triangles $ do
    corner 1 0 0 5 5
    corner 0 1 0 25 5
    corner 0 0 1 5 25
  swapBuffers

corner r g b x y = do color  (Color3  r g b :: Color3  GLfloat)
                      vertex (Vertex2 x y   :: Vertex2 GLfloat)
```



## J


This version assumes J6.02 (but should work on earlier versions).  This will not work with any of the new IDEs from J7 because wd emulation has not been ported to them yet.

Additionally, if you are using 64 bit windows, to get opengl working on J602 you will need to copy jzopengl_win.ijs to jzopengl_win_64.ijs in system/classes/opengl/.


```J
coclass 'example'
(coinsert[require) 'jzopengl'

create=:3 :0
  ogl=: ''conew'jzopengl'
  wd 'pc p;cc c isigraph opengl rightmove bottommove;pas 0 0;pshow;'
)

p_close=: destroy=:3 :0
  destroy__ogl''
  wd'pclose'
  codestroy''
)

corner=:4 :0
  glColor3d x
  glVertex2d y
)

p_c_paint=:3 :0
  rc__ogl''
  glClear GL_COLOR_BUFFER_BIT
  glBegin GL_TRIANGLES
    1 0 0 corner 0 0-0.5
    0 1 0 corner 1 0-0.5
    0 0 1 corner 0 1-0.5
  glEnd''
  show__ogl''
)

conew~'example'
```


Note: OpenGL's initial state is well defined by the OpenGL standard.


## Java

This example uses [http://lwjgl.org/ LWJGL], a game library which has an OpenGL binding for Java

```Java
import org.lwjgl.LWJGLException;
import org.lwjgl.opengl.Display;
import org.lwjgl.opengl.DisplayMode;
import static org.lwjgl.opengl.GL11.*;


public class OpenGlExample {

	public void run() throws LWJGLException {
		Display.setDisplayMode(new DisplayMode(640, 480));
		Display.create();

		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glOrtho(-30, 30, -30, 30, -30, 30);
		glMatrixMode(GL_MODELVIEW);

		while(!Display.isCloseRequested()) {
			render();

			Display.update();
		}

		Display.destroy();
	}

	public void render() {

		glClearColor(0.3f, 0.3f, 0.3f, 0.0f);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glShadeModel(GL_SMOOTH);

		glLoadIdentity();
		glTranslatef(-15.0f, -15.0f, 0.0f);

		glBegin(GL_TRIANGLES);
		glColor3f(1.0f, 0.0f, 0.0f);
		glVertex2f(0.0f, 0.0f);
		glColor3f(0.0f, 1.0f, 0.0f);
		glVertex2f(30f, 0.0f);
		glColor3f(0.0f, 0.0f, 1.0f);
		glVertex2f(0.0f, 30.0f);
		glEnd();

	}

	public static void main(String[] args) {
		OpenGlExample openGlExmpl = new OpenGlExample();
		try {
			openGlExmpl.run();
		} catch(LWJGLException e) {
			System.err.println(e);
		}
	}

}


```



=={{header|JavaScript}} (WebGL)==

Unfortunately for comparison with the other examples on this page, WebGL provides only OpenGL ES, which removes the classic “fixed-function pipeline” and glVertex() in favor of requiring you to write ''vertex shaders'' and ''fragment shaders'', and use vertex arrays. It is not hard to write shader programs to emulate as much of the fixed-function pipeline as you need, but it does mean more verbosity as you have to explicitly define all of the data you're going to communicate to your shader.

In the interest of brevity and not depending on an external matrix library, this example ''omits matrix operations entirely'', as OpenGL ES requires you to add those features yourself if you want them. Examples which show how to implement the classic OpenGL matrix stack are available at [http://learningwebgl.com/blog/?page_id=1217 Learning WebGL] (which this code was derived from).


```html
<html style="margin: 0;">
  <head>
    <title>Minimal WebGL Example</title>
    <!-- This use of <script> elements is so that we can have multiline text
         without quoting it inside of JavaScript; the web browser doesn't
         actually do anything besides store the text of these. -->
    <script id="shader-fs" type="x-shader/x-fragment">
      precision highp float;
      varying vec4 v_color;
      void main(void) {
        // "Varying" variables are implicitly interpolated across triangles.
        gl_FragColor = v_color;
      }
    </script>
    <script id="shader-vs" type="x-shader/x-vertex">
      attribute vec3 a_position;
      attribute vec4 a_color;
      varying vec4 v_color;
      void main(void) {
        gl_Position = vec4(a_position, 1.0);
        v_color = a_color;
      }
    </script>
    <script type="text/javascript">
      function getShader(gl, id) {
        var scriptElement = document.getElementById(id);
        // Create shader object
        var shader;
        if (scriptElement.type == "x-shader/x-fragment")
          shader = gl.createShader(gl.FRAGMENT_SHADER);
        else if (scriptElement.type == "x-shader/x-vertex")
          shader = gl.createShader(gl.VERTEX_SHADER);
        else
          throw new Error("unknown shader script type");
        // Compile shader from source
        gl.shaderSource(shader, scriptElement.textContent);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS))
          throw new Error(gl.getShaderInfoLog(shader));
        return shader;
      }
    </script>
  </head>
  <body style="margin: 0;">
    <canvas id="glcanvas" style="border: none; margin: auto; display: block;" width="640" height="480"></canvas>
    <script type="text/javascript">
      var canvas = document.getElementById("glcanvas");

      // Get WebGL context.
      var gl = canvas.getContext("webgl")
            || canvas.getContext("experimental-webgl");
      if (!gl)
        throw new Error("WebGL context not found");

      // Create shader program from vertex and fragment shader code.
      var shaderProgram = gl.createProgram();
      gl.attachShader(shaderProgram, getShader(gl, "shader-vs"));
      gl.attachShader(shaderProgram, getShader(gl, "shader-fs"));
      gl.linkProgram(shaderProgram);
      if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS))
        throw new Error(gl.getProgramInfoLog(shaderProgram));

      // Specify to render using that program.
      gl.useProgram(shaderProgram);

      // Get the indexes to communicate vertex attributes to the program.
      var positionAttr = gl.getAttribLocation(shaderProgram, "a_position");
      var colorAttr = gl.getAttribLocation(shaderProgram, "a_color");
      // And specify that we will be actually delivering data to those attributes.
      gl.enableVertexAttribArray(positionAttr);
      gl.enableVertexAttribArray(colorAttr);

      // Store vertex positions and colors in array buffer objects.
      var vertices;
      var positionBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices = [
        -0.5, -0.5, 0,
        +0.5, -0.5, 0,
        -0.5, +0.5, 0
      ]), gl.STATIC_DRAW);
      var colorBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        1, 0, 0, 1,
        0, 1, 0, 1,
        0, 0, 1, 1
      ]), gl.STATIC_DRAW);
      var numVertices = vertices.length / 3; // 3 coordinates per vertex

      // Set GL state
      gl.clearColor(0.3, 0.3, 0.3, 1.0);
      gl.enable(gl.DEPTH_TEST);
      gl.viewport(0, 0, gl.drawingBufferWidth || canvas.width,
                        gl.drawingBufferHeight || canvas.height);

      // Draw scene.
      // If this were an animation, everything after this point would go in a main loop.
      //   Clear frame.
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      //   Specify the array data to render.
      //   3 and 4 are the lengths of the vectors (3 for XYZ, 4 for RGBA).
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.vertexAttribPointer(positionAttr, 3, gl.FLOAT, false, 0, 0);
      gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
      gl.vertexAttribPointer(colorAttr, 4, gl.FLOAT, false, 0, 0);
      //   Draw triangles using the specified arrays.
      gl.drawArrays(gl.TRIANGLES, 0, numVertices);

      // Check for errors.
      var e;
      while (e = gl.getError())
        console.log("GL error", e);
    </script>
  </body>
</html>
```




## Julia

Julia's Makie plotting package uses OpenGL as its backend. This example is from the Makie documentation.

```julia
using Makie

mesh([(0.0, 0.0), (0.5, 1.0), (1.0, 0.0)], color = [:red, :green, :blue], shading = false)

```



## Kotlin

```scala
// Kotlin Native version 0.3

import kotlinx.cinterop.*
import opengl.*

fun paint() {
    glClearColor(0.3f, 0.3f, 0.3f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    glShadeModel(GL_SMOOTH)

    glLoadIdentity()
    glTranslatef(-15.0f, -15.0f, 0.0f)

    glBegin(GL_TRIANGLES)
    glColor3f(1.0f, 0.0f, 0.0f)
    glVertex2f(0.0f, 0.0f)
    glColor3f(0.0f, 1.0f, 0.0f)
    glVertex2f(30.0f, 0.0f)
    glColor3f(0.0f, 0.0f, 1.0f)
    glVertex2f(0.0f, 30.0f)
    glEnd()

    glFlush()
}

fun reshape(width: Int, height: Int) {
    glViewport(0, 0, width, height)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
    glMatrixMode(GL_MODELVIEW)
}

fun main(args: Array<String>) {
    memScoped {
        val argc = alloc<IntVar>().apply { value = 0 }
        glutInit(argc.ptr, null)
    }

    glutInitWindowSize(640, 480)
    glutCreateWindow("Triangle")

    glutDisplayFunc(staticCFunction(::paint))
    glutReshapeFunc(staticCFunction(::reshape))

    glutMainLoop()
}
```


```txt

Same as C entry

```



## Lingo

```Lingo
global gOpenGL -- RavOpenGL xtra instance
global GL      -- OpenGL constants

on startMovie
    -- Load the OpenGL script xtra
    gOpenGL = xtra("RavOpenGL").new()

    -- Load GL DLL
    gOpenGL.RavLoadGL("", "")

    -- Function omitted in demo code: loads OpenGL constants into namespace GL
    loadGLConstants()

    -- Window settings
    w = 640
    h = 480
    _movie.stage.title = "Triangle"
    _movie.stage.rect = rect(0, 0, w, h)
    _movie.centerStage = TRUE

    -- Create OpenGL display sprite
    m = new(#RavOpenGLDisplay)
    _movie.puppetSprite(1, TRUE)
    sprite(1).rect = rect(0, 0, w, h)
    sprite(1).member = m
    _movie.updateStage()

    -- Create the OpenGL buffer
    mainBufferID = gOpenGL.RavCreateBuffer(w, h, 32, 32)

    -- Set the sharing mode between script and sprite xtras
    dcID = gOpenGL.RavGetBufferProp(mainBufferID, #ravGC)
    sprite(1).RavShareBuffer(dcID, #true)

    gOpenGL.glViewport(0, 0, w, h)
    gOpenGL.glMatrixMode(GL.PROJECTION)
    gOpenGL.glLoadIdentity()
    gOpenGL.glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
    gOpenGL.glMatrixMode(GL.MODELVIEW)

    gOpenGL.glClearColor(0.3, 0.3, 0.3, 0.0)
    gOpenGL.glClear(GL.COLOR_BUFFER_BIT + GL.DEPTH_BUFFER_BIT)
    gOpenGL.glShadeModel(GL.SMOOTH)
    gOpenGL.glLoadIdentity()
    gOpenGL.glTranslatef(-15.0, -15.0, 0.0)
    gOpenGL.glBegin(GL.TRIANGLES)
    gOpenGL.glColor3f(1.0, 0.0, 0.0)
    gOpenGL.glVertex2f(0.0, 0.0)
    gOpenGL.glColor3f(0.0, 1.0, 0.0)
    gOpenGL.glVertex2f(30.0, 0.0)
    gOpenGL.glColor3f(0.0, 0.0, 1.0)
    gOpenGL.glVertex2f(0.0, 30.0)
    gOpenGL.glEnd()
    gOpenGL.glFlush()

    -- Show the window
    _movie.stage.visible = TRUE
end
```



## Lua

Note that GL functions that take constants in LuaGL can take either the numbers representing those flags (ie. gl.XXX + gl.YYY) or a comma-separated string of those flags (ie. "XXX,YYY"). This example uses strings.


```lua
local gl = require "luagl"
local iup = require "iuplua"
require "iupluagl"

local function paint()
  gl.ClearColor(0.3,0.3,0.3,0.0)
  gl.Clear"COLOR_BUFFER_BIT,DEPTH_BUFFER_BIT"

  gl.ShadeModel"SMOOTH"

  gl.LoadIdentity()
  gl.Translate(-15.0, -15.0, 0.0)

  gl.Begin"TRIANGLES"
  gl.Color(1.0, 0.0, 0.0)
  gl.Vertex(0.0, 0.0)
  gl.Color(0.0, 1.0, 0.0)
  gl.Vertex(30.0, 0.0)
  gl.Color(0.0, 0.0, 1.0)
  gl.Vertex(0.0, 30.0)
  gl.End()

  gl.Flush()
end

local function reshape(width, height)
  gl.Viewport(0, 0, width, height)
  gl.MatrixMode"PROJECTION"
  gl.LoadIdentity()
  gl.Ortho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
  gl.MatrixMode"MODELVIEW"
end

local glc = iup.glcanvas{rastersize="640x480"}
function glc:action() paint() end
function glc:resize_cb(w,h) reshape(w,h) end
function glc:map_cb() iup.GLMakeCurrent(self) end

local dlg = iup.dialog{title="Triangle", shrink="yes"; glc}
dlg:show()

iup.MainLoop()

```



## Liberty BASIC


```lb
nomainwin
    struct rect, x as long, y as long, x2 as long, y2 as long
    struct PFD,   Size as word, Version as word, Flags as long,_
        PixelType as char[1], ColorBits as char[1], RedBits as char[1],_
        RedShift as char[1], GreenBits as char[1], GreenShift as char[1],_
        BlueBits as char[1], BlueShift as char[1], AlphaBits as char[1],_
        AlphaShift as char[1],AccumBits as char[1], AccumRedBits as char[1],_
        AccumGreenBits as char[1], AccumBlueBits as char[1], AccumAlphaBits as char[1],_
        DepthBits as char[1], StencilBits as char[1], AuxBuffers as char[1],_
        LayerType as char[1], Reserved as char[1], LayerMask as long,_
        VisibleMask as long, DamageMask as long
    PFD.Version.struct=1
    PFD.ColorBits.struct=24
    PFD.DepthBits.struct=16
    PFD.Size.struct=len(PFD.struct)
    PFD.Flags.struct=37
    GlColorBufferBit=16384
    open "opengl32.dll" for dll as #gl
    open "glu32.dll" for dll as #glu
    WindowWidth=500
    WindowHeight=500
    UpperLeftX=1
    UpperLeftY=1
    open "Triangle" for window_nf as #main
    print #main,"trapclose [quit]"
    MainH=hwnd(#main)
    calldll #user32,"GetDC", MainH as ulong, MainDC as ulong
    calldll #gdi32,"ChoosePixelFormat", MainDC as ulong, PFD as struct, ret as long
    calldll #gdi32, "SetPixelFormat", MainDC as ulong, ret as long, PFD as struct, t as long
    calldll #gl,"wglCreateContext", MainDC as ulong, GLContext as ulong
    calldll #gl,"wglMakeCurrent", MainDC as ulong, GLContext as ulong, ret as long
    calldll #gl,"glClear", GlColorBufferBit as long,  ret as long
    calldll #gl,"glRotated", 0 as double, 0 as double, 0 as double, 0 as double, ret as long
    calldll #gl,"glBegin", 4 as long, ret as long
    calldll #gl,"glColor3d", 0 as double, 0 as double, 255 as double, ret as long
    calldll #gl,"glVertex3i", -1 as long, -1 as long, 0 as long, ret as long
    calldll #gl,"glColor3d", 255 as double, 0 as double, 0 as double, ret as long
    calldll #gl,"glVertex3i", 0 as long, 1 as long, 0 as long, ret as long
    calldll #gl,"glColor3d", 0 as double, 255 as double, 0 as double, ret as long
    calldll #gl,"glVertex3i", 1 as long, -1 as long, 0 as long, ret as long
    calldll #gl,"glEnd", ret as void
    calldll #gdi32,"SwapBuffers", MainDC as ulong, ret as long
    wait
[quit]
    calldll #gl,"wglMakeCurrent", 0 as ulong, 0 as ulong, ret as long
    calldll #gl,"wglDeleteContext", GLContext as ulong, ret as long
    calldll #user32, "ReleaseDC", MainH as ulong, MainDC as ulong,ret as long
    close #main
    close #glu
    close #gl
    end
```



## MAXScript

The choice of OpenGL or D3D in MAX is a user configuration setting. All MAXScript code is platform independent.

```maxscript
newMesh = mesh numVerts:3 numFaces:1
setMesh newMesh vertices:#([-100, -100, 0], [100, -100, 0], [-100, 100, 0]) faces:#([1, 2, 3])
defaultVCFaces newMesh
setVertColor newMesh 1 red
setVertColor newMesh 2 green
setVertColor newMesh 3 blue
setCVertMode newMesh true
update newMesh
viewport.setType #view_top
max tool maximize
viewport.SetRenderLevel #smoothhighlights
```



## Mercury

Translated from C.
<lang>:- module opengl.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, glut, glut.callback, glut.window, mogl.

main(!IO) :-
   glut.init_window_size(640, 480, !IO),
   glut.window.create("Triangle", !IO),
   glut.callback.display_func(opengl.paint, !IO),
   glut.callback.reshape_func(opengl.reshape, !IO),
   glut.main_loop(!IO).

:- pred paint(io::di, io::uo) is det.

paint(!IO) :-
   mogl.clear_color(0.3, 0.3, 0.3 , 0.0, !IO),
   mogl.clear([color, depth], !IO),

   mogl.shade_model(smooth, !IO),

   mogl.load_identity(!IO),
   mogl.translate(-15.0, -15.0, 0.0, !IO),

   mogl.begin(triangles, !IO),
       mogl.color3(1.0, 0.0, 0.0, !IO),
       mogl.vertex2(0.0, 0.0, !IO),
       mogl.color3(0.0, 1.0, 0.0, !IO),
       mogl.vertex2(30.0, 0.0, !IO),
       mogl.color3(0.0, 0.0, 1.0, !IO),
       mogl.vertex2(0.0, 30.0, !IO),
   mogl.end(!IO),

   mogl.flush(!IO).

:- pred reshape(int::in, int::in, io::di, io::uo) is det.

reshape(Width, Height, !IO) :-
  mogl.viewport(0, 0, Width, Height, !IO),
  mogl.matrix_mode(projection, !IO),
  mogl.load_identity(!IO),
  mogl.ortho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0, !IO),
  mogl.matrix_mode(modelview, !IO).
```



## Nim


```nim
import opengl, glut

proc paint() {.cdecl.} =
  glClearColor(0.3,0.3,0.3,0.0)
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

  glShadeModel(GL_SMOOTH)

  glLoadIdentity()
  glTranslatef(-15.0, -15.0, 0.0)

  glBegin(GL_TRIANGLES)
  glColor3f(1.0, 0.0, 0.0)
  glVertex2f(0.0, 0.0)
  glColor3f(0.0, 1.0, 0.0)
  glVertex2f(30.0, 0.0)
  glColor3f(0.0, 0.0, 1.0)
  glVertex2f(0.0, 30.0)
  glEnd()

  glFlush()

proc reshape(width, height: cint) {.cdecl.} =
  glViewport(0, 0, width, height)
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
  glMatrixMode(GL_MODELVIEW)

enableAutoGlErrorCheck(false)
loadExtensions()
glutInit()
glutInitWindowSize(640, 480)
discard glutCreateWindow("Triangle")

glutDisplayFunc(paint)
glutReshapeFunc(reshape)

glutMainLoop()
```



## Ol

All platform dependent actions are placed in (lib gl), which automatically creates OpenGL window with legacy (pre OpenGL 3.0) context. There are way to create modern context using "(gl:set-context-version 3 0)" with following "(import (OpenGL version-3-0))"; or any other valid version numbers.
OpenGL window works in background (as coroutine) and allow user to call any functions, including OpenGL, in REPL simultaneously with window rendering process.


```scheme

(import (lib gl))
(gl:set-window-title "Rosettacode OpenGL example")

(import (OpenGL version-1-0))

(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)
(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(glOrtho -30.0 30.0 -30.0 30.0 -30.0 30.0)

(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (glTranslatef -15.0 -15.0 0.0)

   (glBegin GL_TRIANGLES)
      (glColor3f 1.0 0.0 0.0)
      (glVertex2f 0.0 0.0)
      (glColor3f 0.0 1.0 0.0)
      (glVertex2f 30.0 0.0)
      (glColor3f 0.0 0.0 1.0)
      (glVertex2f 0.0 30.0)
   (glEnd)
))

```



## OxygenBasic


```oxygenbasic

  title="Rotating Triangle"
  include "OpenglSceneFrame.inc"


  sub Initialize(sys hWnd)
  '
### =================

  SetTimer hWnd,1,10,NULL
  end sub
  '
  sub Scene(sys hWnd)
  '
### ============

  '
  static single ang1,angi1=1
  '
  glClearColor 0.3, 0.3, 0.5, 0
  glClear GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT
  '
  glLoadIdentity
  '
  '
  gltranslatef    0.0, 0.0, -4.0
  glrotatef ang1, 0.0, 0.0,  1.0
  '
  glBegin GL_TRIANGLES
  glColor3f   1.0, 0.0, 0.0 : glVertex3f   0.0,  1.0, 0.0
  glColor3f   0.0, 1.0, 0.0 : glVertex3f  -1.0, -1.0, 0.0
  glColor3f   0.0, 0.0, 1.0 : glVertex3f   1.0, -1.0, 0.0
  glEnd
  '
  'UPDATE ROTATION ANGLES
  '
  ang1+=angi1
  if ang1>360 then ang1-=360
  '
  end sub


  sub Release(sys hwnd)
  '
### ==============

  killTimer hwnd, 1
  end sub


```



## OCaml

```ocaml
open GL
open Glut

let display() =
  glClearColor 0.3 0.3 0.3 0.0;
  glClear[GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  glShadeModel GL_SMOOTH;

  glLoadIdentity();
  glTranslate (-15.0) (-15.0) (0.0);

  glBegin GL_TRIANGLES;
  glColor3 1.0 0.0 0.0;
  glVertex2 0.0 0.0;
  glColor3 0.0 1.0 0.0;
  glVertex2 30.0 0.0;
  glColor3 0.0 0.0 1.0;
  glVertex2 0.0 30.0;
  glEnd();

  glFlush();
;;

let reshape ~width ~height =
  glViewport 0 0 width height;
  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  glOrtho(-30.0) 30.0 (-30.0) 30.0 (-30.0) 30.0;
  glMatrixMode GL_MODELVIEW;
;;

let () =
  ignore(glutInit Sys.argv);
  glutInitWindowSize 640 480;
  ignore(glutCreateWindow "Triangle");

  glutDisplayFunc ~display;
  glutReshapeFunc ~reshape;

  glutMainLoop();
;;
```



## Pascal

Ported from the C example.

```pascal
Program OpenGLDemo;

uses
  SysUtils,
  ctypes,
  gl,
  Glut;

procedure paint; cdecl;
begin
  glClearColor(0.3,0.3,0.3,0.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glShadeModel(GL_SMOOTH);

  glLoadIdentity();
  glTranslatef(-15.0, -15.0, 0.0);

  glBegin(GL_TRIANGLES);
  glColor3f(1.0, 0.0, 0.0);
  glVertex2f(0.0, 0.0);
  glColor3f(0.0, 1.0, 0.0);
  glVertex2f(30.0, 0.0);
  glColor3f(0.0, 0.0, 1.0);
  glVertex2f(0.0, 30.0);
  glEnd();

  glFlush();
end;

procedure reshape(width, height: cint); cdecl;
begin
  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0);
  glMatrixMode(GL_MODELVIEW);
end;

begin
  glutInit(@argc, @argv);
  glutInitWindowSize(640, 480);
  glutCreateWindow('Triangle');

  glutDisplayFunc(@paint);
  glutReshapeFunc(@reshape);

  glutMainLoop();
end.


```



## Perl


```perl
use OpenGL;

sub triangle {
    glBegin GL_TRIANGLES;
    glColor3f 1.0, 0.0, 0.0;
    glVertex2f 5.0, 5.0;
    glColor3f 0.0, 1.0, 0.0;
    glVertex2f 25.0, 5.0;
    glColor3f 0.0, 0.0, 1.0;
    glVertex2f 5.0, 25.0;
    glEnd;
};

glpOpenWindow;
glMatrixMode GL_PROJECTION;
glLoadIdentity;
gluOrtho2D 0.0, 30.0, 0.0, 30.0;
glMatrixMode GL_MODELVIEW;

glClear GL_COLOR_BUFFER_BIT;
triangle;
glpFlush;

glpMainLoop;
```


==={{libheader|Perl/SDL}}===

```perl
use SDL::App;
use SDL::Event;
use SDL::OpenGL;

$app = SDL::App->new(
  -gl => 1,
);

sub triangle {
  glBegin(GL_TRIANGLES);
  glColor(1.0, 0.0, 0.0);
  glVertex(5.0, 5.0);
  glColor(0.0, 1.0, 0.0);
  glVertex(25.0, 5.0);
  glColor(0.0, 0.0, 1.0);
  glVertex(5.0, 25.0);
  glEnd();
}

glMatrixMode(GL_PROJECTION);
glLoadIdentity();
gluOrtho2D(0.0, 30.0, 0.0, 30.0);
glMatrixMode(GL_MODELVIEW);
glClear(GL_COLOR_BUFFER_BIT);
triangle();
$app->sync;
$app->loop ({
  SDL_QUIT() => sub { exit; },
});
```



## Phix

Adapted from the included demo\Arwen32dibdemo\shadepol.exw, draws the same thing as the image at the top of this page, but (as per the talk page) does not use openGL, just windows api (BitBlt etc) and some low-level fully open source routines (all included).

```Phix
include demo\Arwen32dibdemo\a32dpoly.ew

a32Dib0 screen_dib = 0
integer dx = 0, dy = 0, dw = 0, dh = 0

constant win = create(Window, "Arwen32Dib bitmap shaded triangle demo", 0, 0, Default, Default, 480, 300, 0)

function winHandler(integer id, integer msg, atom wParam, object lParam)
sequence rect
    if id or object(lParam) then end if -- suppress warnings
    if msg=WM_PAINT then
        if sequence(screen_dib) then
            clearDib(screen_dib, {0, 0, 0})
            drawShadedPolygonToDib(screen_dib, {{dx, dy}, {dx, dh-dy}, {dw-dx, dh-dy}}, {{255, 0, 0}, {0, 0, 255}, {0, 255, 0}})
            drawDib(win, screen_dib, 0, 0, 0, 0, screen_dib[DibWidth]-1, screen_dib[DibHeight]-1)
        end if
    elsif msg=WM_SIZE then
        rect = getClientRect(win)
        dw = rect[3]
        dh = rect[4]
        dx = floor(dw/4)+1
        dy = floor(dh/4)+1
        if sequence(screen_dib) then killDib(screen_dib) end if
        screen_dib = newDib(dw, dh)
    elsif msg=WM_CHAR
      and wParam=VK_ESCAPE then
        closeWindow(win)
    end if
    return 0
end function
setHandler(win, routine_id("winHandler"))

WinMain(win, SW_NORMAL)

if sequence(screen_dib) then killDib(screen_dib) end if
```

And here is a proper openGL version, translated from Lua, and included in the distro as demo\pGUI\triangle.exw:
```Phix
include pGUI.e
include opengl.e

function resize_cb(Ihandle /*ih*/, integer width, integer height)
    glViewport(0, 0, width, height)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
    glMatrixMode(GL_MODELVIEW)
    return IUP_DEFAULT
end function

function action(Ihandle /*ih*/)

    glClearColor(0.3,0.3,0.3,0.0)
    glClear(GL_COLOR_BUFFER_BIT+GL_DEPTH_BUFFER_BIT)

    glShadeModel(GL_SMOOTH)

    glLoadIdentity()
    glTranslate(-15.0, -15.0, 0.0)

    glBegin(GL_TRIANGLES)
    glColor(1.0, 0.0, 0.0)
    glVertex(0.0, 0.0)
    glColor(0.0, 1.0, 0.0)
    glVertex(30.0, 0.0)
    glColor(0.0, 0.0, 1.0)
    glVertex(0.0, 30.0)
    glEnd()

    glFlush()

    return IUP_DEFAULT
end function

Ihandle dialog, canvas

function map_cb(Ihandle /*ih*/)
    IupGLMakeCurrent(canvas)
    integer {width, height} = IupGetIntInt(dialog, "RASTERSIZE")
    {} = resize_cb(dialog, width, height)
    return IUP_DEFAULT
end function

IupOpen()
IupGLCanvasOpen()

canvas = IupGLCanvas(Icallback("action"), "RASTERSIZE=640x480")
IupSetCallback(canvas, "RESIZE_CB", Icallback("resize_cb"))

dialog = IupDialog(canvas, "MAP_CB", Icallback("map_cb"), "TITLE=Triangle, SHRINK=YES")

IupShow(dialog)
IupMainLoop()
IupDestroy(dialog)
IupClose()
```



## PicoLisp

This is for the 64-bit version.

```PicoLisp
(load "@lib/openGl.l")

(glutInit)
(glutInitWindowSize 400 300)
(glutCreateWindow "Triangle")

(displayPrg
   (glClearColor 0.3 0.3 0.3 0.0)
   (glClear (| GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
   (glShadeModel GL_SMOOTH)
   (glLoadIdentity)
   (glTranslatef -15.0 -15.0 0.0)
   (glBegin GL_TRIANGLES)
   (glColor3f 1.0 0.0 0.0)
   (glVertex2f 0.0 0.0)
   (glColor3f 0.0 1.0 0.0)
   (glVertex2f 30.0 0.0)
   (glColor3f 0.0 0.0 1.0)
   (glVertex2f 0.0 30.0)
   (glEnd)
   (glFlush) )

(reshapeFunc
   '((Width Height)
      (glViewport 0 0 Width Height)
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glOrtho -30.0 30.0 -30.0 30.0 -30.0 30.0)
      (glMatrixMode GL_MODELVIEW) ) )

# Exit upon mouse click
(mouseFunc '((Btn State X Y) (bye)))

(glutMainLoop)
```



## Pike

Uses GLUE to create the window. Rendering code is based on the C example.

```pike
int main() {
	GLUE.init(([
		"fullscreen": 0,
		"resolution": ({ 640, 480 }),
	]));

	while (1) {
		GL.glViewport(0, 0, 640, 480);
		GL.glMatrixMode(GL.GL_PROJECTION);
		GL.glLoadIdentity();
		GL.glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0);
		GL.glMatrixMode(GL.GL_MODELVIEW);

		GL.glClearColor(0.3,0.3,0.3,0.0);
		GL.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

		GL.glShadeModel(GL.GL_SMOOTH);

		GL.glLoadIdentity();
		GL.glTranslate(-15.0, -15.0, 0.0);

		GL.glBegin(GL.GL_TRIANGLES);
		GL.glColor(1.0, 0.0, 0.0);
		GL.glVertex(0.0, 0.0);
		GL.glColor(0.0, 1.0, 0.0);
		GL.glVertex(30.0, 0.0);
		GL.glColor(0.0, 0.0, 1.0);
		GL.glVertex(0.0, 30.0);
		GL.glEnd();

		GL.glFlush();

		GLUE.swap_buffers();
		Pike.DefaultBackend(0.0);
		sleep(0.01);
	}
}
```



## PureBasic

```Purebasic

XIncludeFile "OpenGL.pbi"
pfd.PIXELFORMATDESCRIPTOR
FlatMode = 0 ; Enable Or disable the 'Flat' rendering
WindowWidth = 800 ; The window & GLViewport dimensions
WindowHeight = 600
hWnd = OpenWindow(0, 0, 0, WindowWidth, WindowHeight, "OpenGL Triangle", #PB_Window_SystemMenu)
hdc = GetDC_(hWnd)
pfd\nSize = SizeOf(PIXELFORMATDESCRIPTOR)
pfd\nVersion = 1
pfd\dwFlags = #PFD_SUPPORT_OPENGL | #PFD_DOUBLEBUFFER | #PFD_DRAW_TO_WINDOW
pfd\dwLayerMask = #PFD_MAIN_PLANE
pfd\iPixelType = #PFD_TYPE_RGBA
pfd\cColorBits = 24
pfd\cDepthBits = 16
pixformat = ChoosePixelFormat_(hdc, pfd)
SetPixelFormat_(hdc, pixformat, pfd)
hrc = wglCreateContext_(hdc)
wglMakeCurrent_(hdc,hrc)
glViewport_ (0, 0, WindowWidth-30, WindowHeight-30)
glPushMatrix_()
glMatrixMode_(#GL_MODELVIEW)
glBegin_(#GL_TRIANGLES );
glColor3f_(1.0, 0.0, 0.0 )
glVertex2f_( 0.0, 1.0 )
glColor3f_( 0.0, 1.0, 0.0 )
glVertex2f_( 0.87, -0.5 );
glColor3f_( 0.0, 0.0, 1.0 )
glVertex2f_( -0.87, -0.5 );
glEnd_()
glPopMatrix_()
glFinish_()
SwapBuffers_(hdc)
While Quit = 0
  Repeat
    EventID = WindowEvent()
    Select EventID
    Case #PB_Event_CloseWindow
      Quit = 1
    EndSelect
  Until EventID = 0
Wend


```



## Python


```python
#!/usr/bin/env python
#-*- coding: utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLUT import *

def paint():
    glClearColor(0.3,0.3,0.3,0.0)
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)

    glShadeModel(GL_SMOOTH)

    glLoadIdentity()
    glTranslatef(-15.0, -15.0, 0.0)

    glBegin(GL_TRIANGLES)
    glColor3f(1.0, 0.0, 0.0)
    glVertex2f(0.0, 0.0)
    glColor3f(0.0, 1.0, 0.0)
    glVertex2f(30.0, 0.0)
    glColor3f(0.0, 0.0, 1.0)
    glVertex2f(0.0, 30.0)
    glEnd()

    glFlush()

def reshape(width, height):
    glViewport(0, 0, width, height)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
    glMatrixMode(GL_MODELVIEW)

if __name__ == '__main__':
    glutInit(1, 1)
    glutInitWindowSize(640, 480)
    glutCreateWindow("Triangle")

    glutDisplayFunc(paint)
    glutReshapeFunc(reshape)

    glutMainLoop()
```



## R

```R
library(rgl)
x <- c(-1, -1, 1)
y <- c(0, -1, -1)
z <- c(0, 0, 0)
M <- cbind(x,y,z)
rgl.bg(color="gray15")
triangles3d(M, col=rainbow(8))
```



## Racket

This example features C-style OpenGL api, that's not racket-idiomatic but offer more capabilites.

Note: your system may miss some support libraries.
If OpenGL context creation fails please consult Racket [http://docs.racket-lang.org/gui/libs.html documentation]


```racket

#lang racket/gui
(require sgl/gl)

(define (resize w h)
  (glViewport 0 0 w h))

(define (draw-opengl)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (glShadeModel GL_SMOOTH)

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glBegin GL_TRIANGLES)
  (glColor3f 1 0 0)
  (glVertex3d 0.25 0.25 0.0)
  (glColor3f 0 1 0)
  (glVertex3d 0.75 0.25 0.0)
  (glColor3f 0 0 1)
  (glVertex3d 0.75 0.75 0.0)
  (glEnd))


(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (λ() (draw-opengl) (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (λ() (resize width height) (on-paint))))
    (super-instantiate () (style '(gl)))))

(define win (new frame% [label "Racket Rosetta Code OpenGL example"]
                        [min-width 200] [min-height 200]))
(define gl  (new my-canvas% [parent win]))

(send win show #t)

```



## Ring


```ring

# Project: OpenGL

load "freeglut.ring"
load "opengl21lib.ring"

func main
        glutInit()
        glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA)
        glutInitWindowSize(320,320)
        glutInitWindowPosition(100, 10)
        glutCreateWindow("OpenGL")
        glutDisplayFunc(:renderScene)
        glutMainLoop()

func renderScene
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glBegin(GL_TRIANGLES)
                glVertex3f(-0.5,-0.5,0.0)
                glVertex3f(0.5,0.0,0.0)
                glVertex3f(0.0,0.5,0.0)
        glEnd()
        glutSwapBuffers()

```

Outputimage:

[https://1drv.ms/u/s!AqDUIunCqVnIg1BFt-zV2LphRyjQ OPenGL]


## Ruby

```ruby
require 'rubygems'
require 'gl'
require 'glut'

include Gl
include Glut

paint = lambda do
  glClearColor(0.3,0.3,0.3,0.0)
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

  glShadeModel(GL_SMOOTH)

  glLoadIdentity
  glTranslatef(-15.0, -15.0, 0.0)

  glBegin(GL_TRIANGLES)
    glColor3f(1.0, 0.0, 0.0)
    glVertex2f(0.0, 0.0)
    glColor3f(0.0, 1.0, 0.0)
    glVertex2f(30.0, 0.0)
    glColor3f(0.0, 0.0, 1.0)
    glVertex2f(0.0, 30.0)
  glEnd

  glFlush
end

reshape = lambda do |width, height|
  glViewport(0, 0, width, height)
  glMatrixMode(GL_PROJECTION)
  glLoadIdentity
  glOrtho(-30.0, 30.0, -30.0, 30.0, -30.0, 30.0)
  glMatrixMode(GL_MODELVIEW)
end

glutInit
glutInitWindowSize(640, 480)
glutCreateWindow("Triangle")

glutDisplayFunc(paint)
glutReshapeFunc(reshape)

glutMainLoop
```



## Scala

```Scala
import org.lwjgl.opengl.{ Display, DisplayMode }
import org.lwjgl.opengl.GL11._

object OpenGlExample extends App {

  def render() {

    glClearColor(0.3f, 0.3f, 0.3f, 0.0f)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glShadeModel(GL_SMOOTH)

    glLoadIdentity()
    glTranslatef(-15.0f, -15.0f, 0.0f)

    glBegin(GL_TRIANGLES)
    glColor3f(1.0f, 0.0f, 0.0f)
    glVertex2f(0.0f, 0.0f)
    glColor3f(0.0f, 1.0f, 0.0f)
    glVertex2f(30f, 0.0f)
    glColor3f(0.0f, 0.0f, 1.0f)
    glVertex2f(0.0f, 30.0f)
    glEnd()
  }

  Display.setDisplayMode(new DisplayMode(640, 480))
  Display.create()

  glMatrixMode(GL_PROJECTION)
  glLoadIdentity()
  glOrtho(-30, 30, -30, 30, -30, 30)
  glMatrixMode(GL_MODELVIEW)

  while (!Display.isCloseRequested()) {
    render()
    Display.update()
    Thread.sleep(1000)
  }
}
```



## Tcl

```Tcl
package require Tk
package require tcl3d

proc resizedWin {win w h} {
    glViewport 0 0 $w $h
    glMatrixMode GL_PROJECTION
    glLoadIdentity
    glOrtho -30.0 30.0 -30.0 30.0 -30.0 30.0
    glMatrixMode GL_MODELVIEW
}
proc paintShape {win} {
    glClearColor 0.0 0.0 0.0 0.5
    glClear [expr {$::GL_COLOR_BUFFER_BIT+$::GL_DEPTH_BUFFER_BIT}]
    glShadeModel GL_SMOOTH
    glLoadIdentity
    glTranslatef -15.0 -15.0 0.0
    glBegin GL_TRIANGLES
    glColor3f 1.0 0.0 0.0
    glVertex2f 5.0 5.0
    glColor3f 0.0 1.0 0.0
    glVertex2f 25.0 5.0
    glColor3f 0.0 0.0 1.0
    glVertex2f 5.0 25.0
    glEnd
    $win swapbuffers
}

togl .surface -width 640 -height 480 -double true -depth true \
    -displayproc paintShape -reshapeproc resizedWin
pack .surface -fill both -expand 1
```
Most of this code should be very familiar to anyone looking at the C version above, or with normal [[Tk]] applications.

{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have OpenGL. Does have 3D display, but without shading. -->

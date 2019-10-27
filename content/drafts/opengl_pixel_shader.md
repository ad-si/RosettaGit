+++
title = "OpenGL Pixel Shader"
description = ""
date = 2019-03-11T10:25:56Z
aliases = []
[extra]
id = 10722
[taxonomies]
categories = []
tags = []
+++

{{draft task|OpenGL}}

Using the triangle geometry from [[OpenGL]].  But instead of using a mechanism such as glColor3d, use a [[wp:Pixel Shader|pixel shader]] for each pixel in the triangle.  The pixel shader should pick a random RGB color for each pixel.  Most pixels should have colors which are different from that of most other pixels.

Optional: provide an update mechanism, to repeatedly re-render the triangle.  (Possibilities include a mouse event handler, a timer event handler or an infinite loop, or even window expose events.) Shader generated color for each pixel should be different in each render.

Optional: after updating the opengl rendering target but before rendering the triangle, query the opengl implementation to determine which versions of shaders are supported by the rendering target, list the tested shaders and the available shaders and then use a supported shader.

See also: [http://www.opengl.org/documentation/glsl/ opengl.org's gl shader language documentation], and [http://www.lighthouse3d.com/tutorials/glsl-tutorial/ lighthouse3d.com's glsl tutorial].

## C

{{libheader|GLUT}}
Getting a true (pseudo) random number is surprisingly tricky. The following code makes something noisy, but not at all random:[[image:pixel_shader_C.png|right]]

```c>#include <stdio.h

#include <stdlib.h>
#include <GL/glew.h>
#include <GL/glut.h>

GLuint ps, vs, prog, r_mod;
float angle = 0;
void render(void)
{
	glClear(GL_COLOR_BUFFER_BIT);
	glUniform1f(r_mod, rand() / (float)RAND_MAX);

	glLoadIdentity();
	glRotatef(angle, angle * .1, 1, 0);
	glBegin(GL_TRIANGLES);
		glVertex3f(-1, -.5, 0);
		glVertex3f(0, 1, 0);
		glVertex3f(1, 0, 0);
	glEnd();
	angle += .02;
	glutSwapBuffers();
}

void set_shader()
{
	const char *f =
		"varying float x, y, z;"
		"uniform float r_mod;"
		"float rand(float s, float r) { return mod(mod(s, r + r_mod) * 112341, 1); }"
		"void main() {"
		"	gl_FragColor = vec4(rand(gl_FragCoord.x, x), rand(gl_FragCoord.y, y), rand(gl_FragCoord.z, z), 1);"
		"}";
	const char *v =
		"varying float x, y, z;"
		"void main() {"
		"	gl_Position = ftransform();"
		"	x = gl_Position.x; y = gl_Position.y; z = gl_Position.z;"
		"	x += y; y -= x; z += x - y;"
		"}";

	vs = glCreateShader(GL_VERTEX_SHADER);
	ps = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(ps, 1, &f, 0);
	glShaderSource(vs, 1, &v, 0);

	glCompileShader(vs);
	glCompileShader(ps);

	prog = glCreateProgram();
	glAttachShader(prog, ps);
	glAttachShader(prog, vs);

	glLinkProgram(prog);
	glUseProgram(prog);
	r_mod = glGetUniformLocation(prog, "r_mod");
}

int main(int argc, char **argv)
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
	glutInitWindowSize(200, 200);
	glutCreateWindow("Stuff");
	glutIdleFunc(render);

	glewInit();
	if (!glewIsSupported("GL_VERSION_2_0")) {
		fprintf(stderr, "GL 2.0 unsupported\n");
		return 1;
	}

	set_shader();
	glutMainLoop();

	return 0;
}
```



## Go

{{trans|C}}
{{libheader|FreeGLUT}}
{{libheader|GLEW}}


The following uses 'cgo' to bind to the above C libraries. As C macro functions cannot be invoked directly from Go code, it has been necessary to wrap them first in 'normal' C functions and then invoke those.

```go
package main

/*
#cgo LDFLAGS: -lglut -lGLEW -lGL -lGLU
#include <stdlib.h>
#include <GL/glew.h>
#include <GL/glut.h>

extern void render();

typedef void (*callback) ();

static inline callback idleFunc() {
    return render;
}

static inline void glUniform1f_macro(GLint location, GLfloat v0) {
    glUniform1f(location, v0);
}

static inline GLuint glCreateShader_macro(GLenum _type) {
    return glCreateShader(_type);
}

static inline void glShaderSource_macro(GLuint shader, GLsizei count, const GLchar *const* string, const GLint* length) {
    glShaderSource(shader, count, string, length);
}

static inline void glCompileShader_macro(GLuint shader) {
    glCompileShader(shader);
}

static inline GLuint glCreateProgram_macro() {
    return glCreateProgram();
}

static inline void glAttachShader_macro(GLuint program, GLuint shader) {
    glAttachShader(program, shader);
}

static inline void glLinkProgram_macro(GLuint program) {
    glLinkProgram(program);
}

static inline void glUseProgram_macro(GLuint program) {
    glUseProgram(program);
}

static inline GLint glGetUniformLocation_macro(GLuint program, const GLchar* name) {
    return glGetUniformLocation(program, name);
}

*/
import "C"
import "log"
import "unsafe"

var ps, vs, prog, r_mod C.GLuint
var angle = float32(0)

//export render
func render() {
    C.glClear(C.GL_COLOR_BUFFER_BIT)
    C.glUniform1f_macro(C.GLint(r_mod), C.GLfloat(C.rand())/C.GLfloat(C.RAND_MAX))
    C.glLoadIdentity()
    C.glRotatef(C.float(angle), C.float(angle*0.1), 1, 0)
    C.glBegin(C.GL_TRIANGLES)
    C.glVertex3f(-1, -0.5, 0)
    C.glVertex3f(0, 1, 0)
    C.glVertex3f(1, 0, 0)
    C.glEnd()
    angle += 0.02
    C.glutSwapBuffers()
}

func setShader() {
    f := "varying float x, y, z;" +
        "uniform float r_mod;" +
        "float rand(float s, float r) { return mod(mod(s, r + r_mod) * 112341, 1); }" +
        "void main() {" +
        "    gl_FragColor = vec4(rand(gl_FragCoord.x, x), rand(gl_FragCoord.y, y), rand(gl_FragCoord.z, z), 1);" +
        "}"

    v := "varying float x, y, z;" +
        "void main() {" +
        "    gl_Position = ftransform();" +
        "    x = gl_Position.x; y = gl_Position.y; z = gl_Position.z;" +
        "    x += y; y -= x; z += x - y;" +
        "}"

    fc, vc := C.CString(f), C.CString(v)
    defer C.free(unsafe.Pointer(fc))
    defer C.free(unsafe.Pointer(vc))

    vs = C.glCreateShader_macro(C.GL_VERTEX_SHADER)
    ps = C.glCreateShader_macro(C.GL_FRAGMENT_SHADER)
    C.glShaderSource_macro(ps, 1, &fc, nil)
    C.glShaderSource_macro(vs, 1, &vc, nil)

    C.glCompileShader_macro(vs)
    C.glCompileShader_macro(ps)

    prog = C.glCreateProgram_macro()
    C.glAttachShader_macro(prog, ps)
    C.glAttachShader_macro(prog, vs)

    C.glLinkProgram_macro(prog)
    C.glUseProgram_macro(prog)
    rms := C.CString("r_mod")
    r_mod = C.GLuint(C.glGetUniformLocation_macro(prog, rms))
    C.free(unsafe.Pointer(rms))
}

func main() {
    argc := C.int(0)
    C.glutInit(&argc, nil)
    C.glutInitDisplayMode(C.GLUT_DOUBLE | C.GLUT_RGB)
    C.glutInitWindowSize(200, 200)
    tl := "Pixel Shader"
    tlc := C.CString(tl)
    C.glutCreateWindow(tlc)
    defer C.free(unsafe.Pointer(tlc))
    C.glutIdleFunc(C.idleFunc())

    C.glewInit()
    glv := C.CString("GL_VERSION_2_0")
    if C.glewIsSupported(glv) == 0 {
        log.Fatal("GL 2.0 unsupported")
    }
    defer C.free(unsafe.Pointer(glv))

    setShader()
    C.glutMainLoop()
}
```


=={{header|JavaScript}} (WebGL) ==


```javascript
<html style="margin: 0;">
  <head>
    <title>Fragment Shader WebGL Example</title>
    <!-- This use of <script> elements is so that we can have multiline text
         without quoting it inside of JavaScript; the web browser doesn't
         actually do anything besides store the text of these. -->
    <script id="shader-fs" type="text/x-fragment_shader">
      precision highp float;
      uniform float u_time;
      void main(void) {
        // some gobbledegook
        vec3 foo = vec3(pow(gl_FragCoord.xy, vec2(1.0 + sin(dot(vec4(1.0, 100.0, 0.0, 0.0), gl_FragCoord)))), 0.0);
        foo *= mat3(1.2, 3.9, 1.4, 4.1, 0.2, 1.4, 2.5, 1.6, 7.2);
 
        gl_FragColor = vec4(mod(foo + vec3(u_time), 1.0), 1.0);
      }
    </script>
    <script id="shader-vs" type="text/x-vertex_shader">
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
        shader= gl.createShader(gl[scriptElement.type.replace('text/x-','').toUpperCase()]);
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
      // And specify that we will be actually delivering data to those attributes.
      gl.enableVertexAttribArray(positionAttr);
 
      var timeUniform = gl.getUniformLocation(shaderProgram, "u_time");
 
      // Store vertex positions and colors in array buffer objects.
      var vertices;
      var positionBuffer = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices = [
        -0.5, -0.5, 0,
        +0.5, -0.5, 0,
        -0.5, +0.5, 0
      ]), gl.STATIC_DRAW);
      var numVertices = vertices.length / 3; // 3 coordinates per vertex
 
      // Set GL state
      gl.clearColor(0.3, 0.3, 0.3, 1.0);
      gl.enable(gl.DEPTH_TEST);
      gl.viewport(0, 0, gl.drawingBufferWidth || canvas.width,
                        gl.drawingBufferHeight || canvas.height);
 
      //Specify the array data to render. 
      gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
      gl.vertexAttribPointer(positionAttr, 3, gl.FLOAT, false, 0, 0);
 
      var t0 = Date.now();
      function frame() {
        gl.uniform1f(timeUniform, (Date.now() - t0) / 1000);
 
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.drawArrays(gl.TRIANGLES, 0, numVertices);
 
        var e;
        while (e = gl.getError())
          console.log("GL error", e);
 
      }
      setInterval(frame, 1000/20);
    </script>
  </body>
</html>
```



## Kotlin

{{trans|C}}
{{libheader|FreeGLUT}}
{{libheader|GLEW}}
{{works with|Ubuntu 16.04}}
Assuming that freeglut and GLEW are already installed on your system in the default location(s), you first need to build opengl2.klib using the following .def file and the cinterop tool:

```txt

// opengl2.def
headers = /usr/include/GL/glew.h /usr/include/GL/glut.h /usr/include/GL/glext.h
compilerOpts = -I/usr/include
linkerOpts = -L/usr/lib/x86_64-linux-gnu -lglut -lGLEW -lGL -lGLU

```

You then need to compile the following Kotlin program, linking against opengl2.klib, and run the resulting .kexe file to view the rotating triangle.

```scala
// Kotlin Native v0.6

import kotlinx.cinterop.*
import opengl2.*

var rMod = 0
var angle = 0f

fun render() {
    glClear(GL_COLOR_BUFFER_BIT)
    __glewUniform1f!!(rMod, rand() / RAND_MAX.toFloat()) 
    glLoadIdentity()
    glRotatef(angle, angle * 0.1f, 1f, 0f)
    glBegin(GL_TRIANGLES)
    glVertex3f(-1f, -0.5f, 0f)
    glVertex3f(0f, 1f, 0f)
    glVertex3f(1f, 0f, 0f)
    glEnd()
    angle += 0.02f
    glutSwapBuffers()
}

fun setShader() {
    val f = 
        "varying float x, y, z;" +
        "uniform float r_mod;" +
        "float rand(float s, float r) { return mod(mod(s, r + r_mod) * 112341, 1); }" +
        "void main() {" +
        "	 gl_FragColor = vec4(rand(gl_FragCoord.x, x), " +
        "rand(gl_FragCoord.y, y), rand(gl_FragCoord.z, z), 1);" +
        "}"

    val v = 
        "varying float x, y, z;" +
        "void main() {" +
        "   gl_Position = ftransform();" +
        "   x = gl_Position.x; y = gl_Position.y; z = gl_Position.z;" +
        "   x += y; y -= x; z += x - y;" +
        "}"
    
    val vs = __glewCreateShader!!(GL_VERTEX_SHADER) 
    val ps = __glewCreateShader!!(GL_FRAGMENT_SHADER)
    

    memScoped { 
        val fp = allocPointerTo<ByteVar>()
        fp.value = f.cstr.getPointer(memScope) 
        __glewShaderSource!!(ps, 1, fp.ptr, null)
        val vp = allocPointerTo<ByteVar>()
        vp.value = v.cstr.getPointer(memScope) 
        __glewShaderSource!!(vs, 1, vp.ptr, null)
    
        __glewCompileShader!!(vs)
        __glewCompileShader!!(ps)
 
        val prog = __glewCreateProgram!!()
        __glewAttachShader!!(prog, ps)
        __glewAttachShader!!(prog, vs)
 
        __glewLinkProgram!!(prog)
        __glewUseProgram!!(prog)
    
        val sp = allocPointerTo<ByteVar>()
        sp.value = "r_mod".cstr.getPointer(memScope) 
        rMod = __glewGetUniformLocation!!(prog, sp.value)
    }
}

fun main(args: Array<String>) {
    memScoped {
        val argc = alloc<IntVar>().apply { value = 0 }
        glutInit(argc.ptr, null) 
    }
    glutInitDisplayMode(GLUT_DOUBLE or GLUT_RGB)
    glutInitWindowSize(200, 200)
    glutCreateWindow("Stuff")
    glutIdleFunc(staticCFunction(::render))
    glewInit()
    if (glewIsSupported("GL_VERSION_2_0") == 0.toByte()) {
        println("GL 2.0 unsupported\n")
        return
    }
    setShader()
    glutMainLoop()
}
```



## Ol


```scheme

#!/usr/bin/ol
(import (lib gl2))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(define po (gl:CreateProgram
"#version 120 // OpenGL 2.1
	varying float x, y, z;
	void main(void) {
		gl_Position = ftransform();
		x = gl_Position.x; y = gl_Position.y; z = gl_Position.z;
		x += y; y -= x; z += x - y;
	}"
"#version 120 // OpenGL 2.1
	varying float x, y, z;
	uniform float r_mod;
	float rand(float s, float r) { return mod(mod(s, r + r_mod) * 112341, 1); }
	
	void main() {
		gl_FragColor = vec4(rand(gl_FragCoord.x, x), rand(gl_FragCoord.y, y), rand(gl_FragCoord.z, z), 1);
	}"))

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glUseProgram po)
   (glUniform1f (glGetUniformLocation po (c-string "r_mod")) 1)

   (glColor3f 1 1 1)
   (glBegin GL_TRIANGLES)
      (glVertex2f -0.6 -0.6)
      (glVertex2f +0.6 -0.6)
      (glVertex2f -0.0 +0.7)
   (glEnd)))

```



## Racket


```racket
#lang racket/gui
(require typed/opengl)
 
(define (resize w h)
  (glViewport 0 0 w h))

;; shaders gotten from [#C]
(define shader-source:fragment
  #<<<
varying float x, y, z;
uniform float r_mod;
float rand(float s, float r) { return mod(mod(s, r + r_mod) * 112341, 1); }
void main() {
  gl_FragColor = vec4(rand(gl_FragCoord.x, x),
                      rand(gl_FragCoord.y, y),
                      rand(gl_FragCoord.z, z),
                      1);
}
<
  )

(define shader-source:vertex
  #<<<
varying float x, y, z;
void main() {
  gl_Position = ftransform();
  x = gl_Position.x;
  y = gl_Position.y;
  z = gl_Position.z;
  x += y;
  y -= x;
  z += x - y;
}
<
  )

(define (draw-opengl prg)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glShadeModel GL_SMOOTH)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glBegin GL_TRIANGLES)
  (glVertex3d 0.25 0.25 0.0)
  (glVertex3d 0.75 0.25 0.0)
  (glVertex3d 0.75 0.75 0.0)
  (glEnd))
 
 
(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define the-program #f)

    (define/override (on-paint)
      (with-gl-context
          (λ()
            (unless the-program
              (set! the-program
                    (create-program (load-shader (open-input-string shader-source:fragment)
                                                 GL_FRAGMENT_SHADER)
                                    (load-shader (open-input-string shader-source:vertex)
                                                 GL_VERTEX_SHADER)))
              (glUseProgram the-program))
            (draw-opengl the-program) (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (λ() (resize width height))))
    (super-instantiate () (style '(gl)))))
 
(define win (new frame% [label "Racket Rosetta Code OpenGL example"]
                        [min-width 200] [min-height 200]))
(define gl  (new my-canvas% [parent win]))
 
(send win show #t)
```



## Tcl

{{libheader|tcl3d}}
{{trans|C}}

Using the [http://www.tcl3d.org Tcl3d] library and liberally borrowing from [http://wiki.tcl.tk/41477 this pixel shader demo on the wiki], here is a brute translation of the C implementation.


```Tcl

package require tcl3d

proc mkshader {type src} {
    set sh [glCreateShader $type]
    tcl3dOglShaderSource $sh $src
    glCompileShader $sh
    puts "compilation report : [tcl3dOglGetShaderState $sh $::GL_COMPILE_STATUS] [tcl3dOglGetShaderInfoLog $sh]"
    return $sh
}

proc render {{angle 0}} {
    glClear $::GL_COLOR_BUFFER_BIT
    glUniform1f $::uloc_rmod [expr {rand()}]
    glLoadIdentity
    glRotatef $angle 1.0 1.0 1.0
    glBegin GL_TRIANGLES
        glVertex3f -1 -.5 0
        glVertex3f  0  1  0
        glVertex3f  1  0  0
    glEnd

    .w swapbuffers
    after 40 [list render [expr {$angle+.2}]]
}

proc set_shader {} {
    set f {
        varying float x, y, z;
        uniform float rmod;
        float rand(float s, float r) { return mod(mod(s, r + rmod)*112341.0, 1.0); }
        void main() {
            gl_FragColor = vec4(rand(gl_FragCoord.x, x), rand(gl_FragCoord.y, y), rand(gl_FragCoord.z, z), 1);
        }
    }
    set v {
        varying float x, y, z;
        void main() {
            gl_Position = ftransform();
            x = gl_Position.x; y = gl_Position.y; z = gl_Position.z;
            x += y; y -= x; z += x - y;
        }
    }

    set vs [mkshader $::GL_VERTEX_SHADER $v]
    set ps [mkshader $::GL_FRAGMENT_SHADER $f]

    set proc [glCreateProgram]
    glAttachShader $proc $ps
    glAttachShader $proc $vs
    glLinkProgram $proc
    glUseProgram $proc

    set ::uloc_rmod [glGetUniformLocation $proc "rmod"]
}

togl .w -w 640 -h 480 -double true
pack .w -expand 1 -fill both
bind .w <Key-Escape> exit
wm protocol . WM_DELETE_WINDOW exit

set_shader
render

```


{{omit from|Blast}}
{{omit from|GUISS}}
{{omit from|Locomotive Basic}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|Lilypond}}
{{omit from|TPP}}
{{omit from|ZX Spectrum Basic}}

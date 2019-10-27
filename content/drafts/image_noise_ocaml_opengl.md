+++
title = "Image Noise/OCaml/OpenGL"
description = ""
date = 2012-05-01T17:02:58Z
aliases = []
[extra]
id = 10758
[taxonomies]
categories = []
tags = []
+++

{{libheader|glMLite}}
{{libheader|GLUT}}
Compile this program to native-code with the following command:
 ocamlopt -o noise_gl.opt bigarray.cmxa unix.cmxa -I +glMLite GL.cmxa Glut.cmxa noise_gl.ml

One can also interpret it with the native code OCaml toplevel:
 ocamlnat unix.cmxs bigarray.cmxs -I +glMLite GL.cmxs Glut.cmxs noise_gl.ml


```ocaml
open Glut
open GL
 
let width = 320
let height = 240
let len = width * height
 
let buff = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len
let t_last = ref (Unix.gettimeofday())
let frames = ref 0

let render () =
  glClear [GL_COLOR_BUFFER_BIT];
  for i = 0 to pred len do
    buff.{i} <- char_of_int (Random.int 256)
  done;
  glBitmap width height 0.0 0.0 0.0 0.0 buff;
  glFlush();
  incr frames;
  if !frames = 600 then begin
    let t = Unix.gettimeofday() in
    Printf.printf "- fps: %f\n%!" ((float !frames) /. (t -. !t_last));
    t_last := t;
    frames := 0;
  end;
  glutSwapBuffers()
 
let () =
  ignore (glutInit Sys.argv);
  glutInitDisplayMode [GLUT_RGB; GLUT_DOUBLE];
  glutInitWindowSize width height;
  ignore (glutCreateWindow "noise");
  glutDisplayFunc render;
  glutIdleFunc render;
  glutMainLoop()
```


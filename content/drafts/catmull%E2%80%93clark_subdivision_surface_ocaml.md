+++
title = "Catmull–Clark subdivision surface/OCaml"
description = ""
date = 2012-05-01T16:59:28Z
aliases = []
[extra]
id = 5280
[taxonomies]
categories = []
tags = []
+++

Here is the file dynar.ml (module Dynar), it implements a structure similar to arrays but with which we can push a new element at the end.


```ocaml
type 'a t = { mutable ar: 'a array; mutable n: int; mutable max_n: int }

let of_array ar =
  let n = Array.length ar in
  { ar=ar; n=n; max_n=n }

let of_list li =
  of_array (Array.of_list li)

let push da v =
  if da.n < da.max_n
  then (da.ar.(da.n) <- v; da.n <- succ da.n)
  else begin
    let old_get = Array.unsafe_get da.ar in
    let old_max = da.max_n in
    let new_size = succ da.max_n * 2 in
    let new_ar = Array.init new_size (fun i -> if i < old_max then old_get i else v) in
    da.ar <- new_ar;
    da.n <- succ da.n;
    da.max_n <- new_size;
  end
;;

(* returns the index of the item that has just been pushed *)
let pushi da v =
  let i = da.n in
  push da v;
  (i)

let to_array da =
  Array.init da.n (Array.unsafe_get da.ar)
```



Below is a program in OpenGL which displays a cube subdivided 2 times with the ''Catmull–Clark surface subdivision'' algorithm.

{{libheader|glMLite}}
{{libheader|GLUT}}
You can compile everything together with:
 ocamlopt -c dynar.ml
 ocamlopt -c subsurf.ml
 ocamlopt -I +glMLite GL.cmxa Glu.cmxa Glut.cmxa dynar.cmx subsurf.cmx main.ml -o catmull.opt


```ocaml
open GL
open Glu
open Glut

let points0 = [|
  (-0.5, 0.5, 0.5);
  (-0.5, -0.5, 0.5);
  (0.5, -0.5, 0.5);
  (0.5, 0.5, 0.5);
  (0.5, -0.5, -0.5);
  (0.5, 0.5, -0.5);
  (-0.5, -0.5, -0.5);
  (-0.5, 0.5, -0.5);
|]

let faces0 = [|
  (0, 1, 2, 3);
  (3, 2, 4, 5);
  (5, 4, 6, 7);
  (7, 0, 3, 5);
  (7, 6, 1, 0);
  (6, 1, 2, 4);
  (* comment some of these faces to see how
     the algorithm handles surfaces with holes
  *)
|]


let points1, faces1 = Subsurf.catmull points0 faces0 ;;
let points2, faces2 = Subsurf.catmull points1 faces1 ;;


let b_down = ref false
let anglex = ref (-64)
let angley = ref (-19)
let xold = ref 0
let yold = ref 0


let display () =
  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];

  glLoadIdentity();

  glTranslate ~x:(0.0) ~y:(0.0) ~z:(-3.0);

  glRotate ~angle:(float(- !angley)) ~x:1.0 ~y:0.0 ~z:0.0;
  glRotate ~angle:(float(- !anglex)) ~x:0.0 ~y:1.0 ~z:0.0;

  let draw points faces =
    let put_point i = glVertex3v points.(i) in
    glBegin GL_QUADS;
    Array.iter (fun (a,b,c,d) ->
        put_point a;
        put_point b;
        put_point c;
        put_point d;
      ) faces;
    glEnd();
  in
  glColor3 0.4 0.4 0.4;  draw points0 faces0;
  glColor3 0.4 0.4 0.4;  draw points1 faces1;
  glColor3 0.9 0.9 0.9;  draw points2 faces2;

  glFlush();
  glutSwapBuffers();
;;

let reshape ~width ~height =
  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  gluPerspective 30. (float width /. float height) 2. 30.;
  glViewport 0 0 width height;
  glMatrixMode GL_MODELVIEW;
  glutPostRedisplay();
;;

let keyboard ~key ~x ~y =
  match key with
  | '\027' | 'q' -> exit(0)
  | _ -> ()
;;


let mouse ~button ~state ~x ~y =
  match button, state with
  | GLUT_LEFT_BUTTON, GLUT_DOWN ->
      b_down := true;
      xold := x;
      yold := y;
  | GLUT_LEFT_BUTTON, GLUT_UP ->
      b_down := false;
  | _ -> ()
;;


let motion ~x ~y =
  if !b_down then
  begin
    anglex := !anglex + (!xold - x);
    angley := !angley + (!yold - y);
    glutPostRedisplay();
  end;
  
  xold := x;
  yold := y;
;;


let () =
  ignore(glutInit Sys.argv);
  glutInitDisplayMode[GLUT_RGB; GLUT_DOUBLE; GLUT_DEPTH];
  glutInitWindowPosition ~x:100 ~y:100;
  glutInitWindowSize ~width:640 ~height:480;
  ignore(glutCreateWindow ~title:Sys.argv.(0));

  glEnable GL_DEPTH_TEST;
  glPolygonMode GL_FRONT_AND_BACK  GL_LINE;
  glClearColor 0.2 0.2 0.2 0.0;

  glutDisplayFunc ~display;
  glutReshapeFunc ~reshape;
  glutKeyboardFunc ~keyboard;
  glutMouseFunc ~mouse;
  glutMotionFunc ~motion;
  glutMainLoop();
;;
```


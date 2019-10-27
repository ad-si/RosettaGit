+++
title = "Loading animated 3D data/OCaml"
description = ""
date = 2014-05-24T12:29:55Z
aliases = []
[extra]
id = 5274
[taxonomies]
categories = []
tags = []
+++

{{libheader|glMLite}}

{{libheader|xml-light}}

{{libheader|GLUT}}


```ocaml
(* loading the libraries *)

#directory "+xml-light" (* or maybe "+site-lib/xml-light" *)
#load "xml-light.cma"

#directory "+glMLite"
#load "GL.cma"
#load "Glu.cma"
#load "Glut.cma"



(* types, and scene graph *)

type time = float  (* in seconds *)

type 'a anim = At of time * 'a | Change of time * time * 'a * 'a

type 'a timed = Static of 'a | Animated of 'a anim list

type float1 = float timed
type float3 = (float * float * float) timed
type float4 = (float * float * float * float) timed

type scene = scene_elem list
and scene_elem =
  | Viewpoint of float3 * float4  (* position, orientation *)
  | PointLight of float3 * float3  (* location, color *)
  | Transform of transform_attr list

and transform_attr = Translation of float3 | Scale of float3
  | Rotation of float4
  | Contents of shape list

and shape = geom * appearance
and appearance = appearance_attr list
and appearance_attr = DiffuseColor of float3
and geom =
  | Box of float3
  | Sphere of float1  (* radius *)
  | Cylinder of float1 * float1  (* radius, height *)
  | Cone of float1 * float1  (* bottomRadius, height *)




(* parsing functions *)

let scan_float3 s =
  Scanf.sscanf s "%f %f %f" (fun x y z -> x,y,z) ;;

let scan_float4 s =
  Scanf.sscanf s "%f %f %f %f" (fun a x y z -> a,x,y,z) ;;

let scan_time s =
  Scanf.sscanf s "%fs" (fun sec -> sec) ;;


let mk_float3 v = Static(scan_float3 v)
let mk_float4 v = Static(scan_float4 v)


let assoc_opt v li =
  try Some(List.assoc v li)
  with Not_found -> None

let find_opt f li =
  try Some(List.find f li)
  with Not_found -> None



let get_anim scan attr_name childs =
  List.fold_left (fun acc -> function
    Xml.Element ("animate", attrs, _) ->
      let this_attr_name = List.assoc "attributeName" attrs in
      if this_attr_name <> attr_name
      then (acc)
      else
        let from = scan(List.assoc "from" attrs)
        and to_  = scan(List.assoc "to" attrs)
        and begin_ = scan_time(List.assoc "begin" attrs)
        and dur    = scan_time(List.assoc "dur" attrs)
        in
        Change(begin_, begin_ +. dur, from, to_) :: acc

    | _ -> (acc)
  ) [] childs


let to_param scan attr_name attrs default childs =
  match assoc_opt attr_name attrs,
        get_anim scan attr_name childs with
  | None,   [] -> Static(default)
  | Some v, [] -> Static(scan v)
  | None,   anim -> Animated(anim)
  | Some v, anim -> Animated(At(0.0, scan v)::anim)

let to_float3 = to_param scan_float3 ;;
let to_float4 = to_param scan_float4 ;;
let to_float1 = to_param float_of_string ;;


let parse_geom = function
  | Xml.Element ("Box", attrs, childs) ->
      Box(to_float3 "size" attrs (2., 2., 2.) childs)

  | Xml.Element ("Sphere", attrs, childs) ->
      Sphere(to_float1 "radius" attrs (1.0) childs)

  | Xml.Element ("Cylinder", attrs, childs) ->
      let radius = (to_float1 "radius" attrs (1.0) childs)
      and height = (to_float1 "height" attrs (2.0) childs) in
      Cylinder(radius, height)

  | Xml.Element ("Cone", attrs, childs) ->
      let botRad = (to_float1 "bottomRadius" attrs (1.0) childs)
      and height = (to_float1 "height"       attrs (2.0) childs) in
      Cone(botRad, height)

  | _ -> assert false


let appearance_fold acc = function
  | Xml.Element ("Appearance", [], [
      Xml.Element ("Material", attrs, childs)]) ->
        DiffuseColor(to_float3 "diffuseColor" attrs (0.8, 0.8, 0.8) childs)::acc
  | _ -> (acc)


let filter_geom = function
  | Xml.Element ("Box",_,_)
  | Xml.Element ("Sphere",_,_)
  | Xml.Element ("Cylinder",_,_)
  | Xml.Element ("Cone",_,_) -> true
  | _ -> false

let parse_shape_contents c =
  let geom = parse_geom(List.find filter_geom c)
  and appearance = List.fold_left appearance_fold [] c in
  let shape = (geom, appearance) in
  (shape)


let parse_shape = function
  | Xml.Element("Shape", [], contents) ->
      (parse_shape_contents contents)
  | _ -> assert false

let filter_shape = function Xml.Element("Shape",_,_) -> true | _ -> false


let map_scene_elem = function
  | Xml.Element ("Viewpoint", attrs, childs) ->
      let position    = (to_float3 "position"    attrs (0., 0., 10.) childs)
      and orientation = (to_float4 "orientation" attrs (0.,0.,1.,0.) childs) in
      Viewpoint(position, orientation)

  | Xml.Element ("PointLight", attrs, childs) ->
      let location = (to_float3 "location" attrs (0., 0., 0.) childs)
      and color    = (to_float3 "color"    attrs (1., 1., 1.) childs) in
      PointLight(location, color)

  | Xml.Element ("Transform", attrs, contents) ->
      let transform_attrs = [] in
      let transform_attrs =
        match assoc_opt "translation" attrs with
        | Some translation -> Translation(mk_float3 translation)::transform_attrs
        | None -> (transform_attrs)
      in
      let transform_attrs =
        match assoc_opt "scale" attrs with
        | Some scale -> Scale(mk_float3 scale)::transform_attrs
        | None -> (transform_attrs)
      in
      let transform_attrs =
        match assoc_opt "rotation" attrs with
        | Some rotation -> Rotation(mk_float4 rotation)::transform_attrs
        | None -> (transform_attrs)
      in
      let shapes = List.filter filter_shape contents in
      let shapes = List.map parse_shape shapes in
      let transform_attrs = (Contents shapes)::transform_attrs in

      (* TODO animate (translation, rotation, scale) from childs *)

      Transform(transform_attrs)

  | _ -> assert false


let parse_scene = function
  | Xml.Element ("smil", [], [
      Xml.Element ("X3D", [], [
        Xml.Element ("Scene", [], scene_elems)])])

  | Xml.Element ("X3D", [], [
      Xml.Element ("Scene", [], scene_elems)]) -> List.map map_scene_elem scene_elems

  | _ -> assert false

(* end of parsing the datas *)



(* timeline functions *)

let inter1 t t1 t2 v1 v2 =
  v1 +. ((t -. t1) /. (t2 -. t1) *. (v2 -. v1))

let inter3 t t1 t2 (a1,b1,c1) (a2,b2,c2) =
  let m = (t -. t1) /. (t2 -. t1) in
  ( a1 +. (m *. (a2 -. a1)),
    b1 +. (m *. (b2 -. b1)),
    c1 +. (m *. (c2 -. c1)) )

let inter4 t t1 t2 (a1,b1,c1,d1) (a2,b2,c2,d2) =
  let m = (t -. t1) /. (t2 -. t1) in
  ( a1 +. (m *. (a2 -. a1)),
    b1 +. (m *. (b2 -. b1)),
    c1 +. (m *. (c2 -. c1)),
    d1 +. (m *. (d2 -. d1)) )

let rec val_at inter t = function
  | At(t1, v) :: At(t2,_) :: _
  | At(t1, v) :: Change(t2,_,_,_) :: _
    when t1 <= t && t < t2 -> (v)
  | At(t, v) :: [] -> (v)
  | Change(_,t2,_,v2) :: []
    when t >= t2 -> (v2)
  | Change(t1,t2,v1,v2) :: _
    when t1 <= t && t <= t2 -> inter t t1 t2 v1 v2
  | _ :: tl -> val_at inter t tl
  | [] -> assert false

let get_val inter t = function
  | Static v -> v
  | Animated anim -> val_at inter t anim

let get_val1 = get_val inter1 ;;
let get_val3 = get_val inter3 ;;
let get_val4 = get_val inter4 ;;



(* OpenGL rendering *)

open GL
open Glu
open Glut

let t = ref 0.0

let neg_vec (x, y, z) = (-. x, -. y, -. z)


let display scene = function () ->
  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
  glLoadIdentity ();

  List.iter (function
  | Viewpoint (position, orientation) ->
      let position    = get_val3 !t position
      and orientation = get_val4 !t orientation in
      let angle, x, y, z = orientation in
      glRotate ~angle ~x ~y ~z;
      glTranslatev (neg_vec position)

  | PointLight (location, color) ->
      let location = get_val3 !t location
      and color    = get_val3 !t color in
      ignore(location, color)

  | Transform cl ->
      List.iter (function
      | Scale scale ->
          let v = get_val3 !t scale in
          glScalev v
      | Translation vec ->
          let v = get_val3 !t vec in
          glTranslatev v
      | _ -> ()
      ) cl;
      List.iter (function
      | Contents cl ->
          List.iter (function
          | (Box size, appearance) ->
              List.iter (function
              | DiffuseColor color ->
                  glColor3v (get_val3 !t color)
              ) appearance;
              glPushMatrix ();
               glScalev (get_val3 !t size);
                glutSolidCube ~size:1.0;
              glPopMatrix ();

          | _ -> ()  (* TODO other primitives *)
          ) cl
      | _ -> ()
      ) cl;

  ) scene;

  glFlush ();
  glutSwapBuffers ();
;;


let reshape ~width ~height =
  glMatrixMode GL_PROJECTION;
  glLoadIdentity ();
  gluPerspective 30. (float width /. float height) 2. 30.;
  glViewport 0 0 width height;
  glMatrixMode GL_MODELVIEW;
  glutPostRedisplay ();
;;

let keyboard ~key ~x ~y =
  match key with
  | '\027' | 'q' -> exit(0)
  | _ -> ()
;;

(* main *)
let () =
  ignore(glutInit Sys.argv);
  glutInitDisplayMode [GLUT_RGBA; GLUT_DOUBLE; GLUT_DEPTH];
  glutInitWindowPosition ~x:200 ~y:200;
  glutInitWindowSize ~width:400 ~height:300;
  ignore(glutCreateWindow ~title:Sys.argv.(0));

  glEnable GL_DEPTH_TEST;

  let my_scene = parse_scene (Xml.parse_file Sys.argv.(1)) in

  let rec timer ~value =
    t := !t +. 0.01;
    glutTimerFunc ~msecs:value ~timer ~value;
  in
  let msecs = 10 in
  glutTimerFunc ~msecs ~timer ~value:msecs;
  glutIdleFunc ~idle:glutPostRedisplay;
  glutDisplayFunc ~display:(display my_scene);
  glutReshapeFunc ~reshape;
  glutKeyboardFunc ~keyboard;
  glutMainLoop ();
;;
```


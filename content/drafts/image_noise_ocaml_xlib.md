+++
title = "Image noise/OCaml/Xlib"
description = ""
date = 2010-11-29T14:42:01Z
aliases = []
[extra]
id = 8429
[taxonomies]
categories = []
tags = []
+++

{{libheader|OCaml-Xlib}}

By default this program displays the noise with double buffering.
The command-line options <code>-single</code> selects the single buffering.

Launch this program as a script with:
 ocaml -I +Xlib Xlib.cma unix.cma noise_fps_x.ml                                                            
 ocaml -I +Xlib Xlib.cma unix.cma noise_fps_x.ml -single


```ocaml
open Xlib

type buffering = Single | Double

let best_buffering = Double
let default_buffering = Single

let num_frames = 1000

(* choose the buffering kind *)
let buffering =
  match Sys.argv with
  | [| _; "-db" |] -> Double
  | [| _; "-best" |] -> best_buffering
  | [| _; "-single" |] -> Single
  | [| _; "-default" |] -> default_buffering
  | _ -> best_buffering

(* report the buffering chosen *)
let () =
  print_endline (
    match buffering with
    | Double -> "double buffering"
    | Single -> "single buffering")

let () =
  let width = 320 and height = 240 in
  let dpy = xOpenDisplay "" in

  (* initialisation of the standard variables *)
  let screen = xDefaultScreen dpy in
  let root = xDefaultRootWindow dpy
  and visual = xDefaultVisual dpy screen
  and depth = xDefaultDepth dpy screen
  and black = xBlackPixel dpy screen
  and white = xWhitePixel dpy screen
  in

  (* set foreground and background in the graphics context *)
  let gcvalues = new_xGCValues() in
  xGCValues_set_foreground gcvalues black;
  xGCValues_set_background gcvalues white;
  let gc = xCreateGC dpy root [GCForeground;GCBackground] gcvalues in

  (* creation of the double buffer *)
  let db = xCreatePixmap dpy root width height depth in
  (* without these lines previous images from memory will appear *)
  xSetForeground dpy gc white;
  xFillRectangle dpy db gc 0 0 width height;
  xSetForeground dpy gc black;

  (* window attributes *)
  let xswa = new_win_attr() in

  (* the events we want *)
  xswa.set_event_mask [ExposureMask;PointerMotionMask;KeyPressMask];

  (* border and background colors *)
  xswa.set_background_pixel white;
  xswa.set_border_pixel black;

  let win =
    xCreateWindow
      dpy root 100 100 width height 2 depth InputOutput visual
      [CWEventMask;CWBorderPixel;CWBackPixel] xswa.attr
  in

  (* show the window on screen *)
  xMapRaised dpy win;

  (* connect the close button of the window handle *)
  let wm_delete_window = xInternAtom dpy "WM_DELETE_WINDOW" true in
  xSetWMProtocols dpy win wm_delete_window 1;

  let t0 = Unix.gettimeofday() in
  let event = new_xEvent() in

  for i = 1 to num_frames do
    if xPending dpy > 0 then
    begin
      (* handle events *)
      xNextEvent dpy event;
      match xEventType event with
      | Expose ->
          (* remove all the Expose events from the event stack *)
          while (xCheckTypedEvent dpy Expose event) do () done;
          xCopyArea dpy db win gc 0 0 width height 0 0;
          (* force refresh the screen *)
          xFlush dpy;

      | KeyPress ->
          (* exit on any key press *)
          xCloseDisplay dpy;
          exit 0;

      | ClientMessage ->
          (* delete window event *)
          let xclient = to_xClientMessageEvent event in
          let atom = xEvent_xclient_data xclient in
          if atom = wm_delete_window then exit 0

      | _ -> ()
    end;

    begin
      match buffering with
      | Double ->
          (* animation with the double buffer *)
          xSetForeground dpy gc white;
          xFillRectangle dpy db gc 0 0 width height;
          xSetForeground dpy gc black;

          let points = Array.init (width * height / 2) (fun _ ->
            { pnt_x = Random.int width; pnt_y = Random.int height }) in
          xDrawPoints dpy db gc points CoordModeOrigin;

          xCopyArea dpy db win gc 0 0 width height 0 0;
          (* force refresh the screen *)
          xFlush dpy;
      | Single ->
          (* animation without double buffer *)
          xClearWindow dpy win;

          let points = Array.init (width * height / 2) (fun _ ->
            { pnt_x = Random.int width; pnt_y = Random.int height }) in

          xDrawPoints dpy win gc points CoordModeOrigin;
    end
  done;

  (* tell what was the FPS for num_frames frames *)
  let t_end = Unix.gettimeofday() in
  let fps = (float num_frames) /. (t_end -. t0) in
  print_string ">> fps:";
  print_float fps;
  print_newline()
```


You can also compile this program to native-code with the following command:
:$ ocamlopt -I +Xlib Xlib.cmxa unix.cmxa noise_fps_x.ml -o noise_fps_x.opt

and then execute the result with:
:$ ./noise_fps_x.opt

+++
title = "Image noise/OCaml/Alleg"
description = ""
date = 2010-11-29T14:41:59Z
aliases = []
[extra]
id = 8449
[taxonomies]
categories = []
tags = []
+++

{{WIP}}
{{libheader|OCaml-Allegro}}


```ocaml
(* modified/adapted from the sample program exdbuf.c by Shawn Hargreaves,
   from the Allegro library, which is provided under the giftware licence. *)
open Allegro

let () =
  allegro_init();
  install_timer();
  install_keyboard();

  begin
    try set_gfx_mode GFX_AUTODETECT_WINDOWED 320 240 0 0;
    with _ ->
      try set_gfx_mode GFX_SAFE 320 240 0 0
      with _ ->
        set_gfx_mode GFX_TEXT 0 0 0 0;
        allegro_message ("Unable to set any graphic mode\n" ^
                         (get_allegro_error()) ^ "\n");
        exit 1
  end;

  set_palette(get_desktop_palette());

  let screen, font = get_screen(), get_font() in
  let screen_w, screen_h = get_screen_width(), get_screen_height() in

  (* allocate the memory buffer *)
  let buffer = create_bitmap screen_w screen_h in

  let black = (makecol 0 0 0)
  and white = (makecol 255 255 255) in

  (* we choose the double buffering method *)
  clear_keybuf();
  let c = 32 + (retrace_count()) in
  let rec main_loop n =
    clear_to_color buffer white;
    for x = 0 to screen_w do
      for y = 0 to screen_h do
        if Random.bool() then
          putpixel buffer x y black; 
      done;
    done;
    blit buffer screen 0 0 0 0 screen_w screen_h;

    if not(keypressed())
    && ((retrace_count()) - c) <= (320 + 32)
    then main_loop (succ n)
    else (n)
  in
  let n = main_loop 0 in

  (* just print the number of frames displayed *)
  print_int n;
  print_newline();

  (* release the memory buffer *)
  destroy_bitmap buffer
```


Run that as a script with the command:
 ocaml -I +allegro allegro.cma noise_fps_alleg.ml

This is a work in progress version, it only prints the number of frames displayed, but we could still use the command time and make the calculus with the ocaml top-level.

 TODO: use the Allegro timer system.

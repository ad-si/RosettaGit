+++
title = "Execute SNUSP/OCaml"
description = ""
date = 2010-02-06T14:36:10Z
aliases = []
[extra]
id = 5260
[taxonomies]
categories = []
tags = []
+++

{{implementation|SNUSP}}{{collection|RCSNUSP}}

This is an implementation of '''Modular SNUSP'''.

Usage: ocaml rcsnusp.ml srccode.snusp


```ocaml
let input_line ic =
  try Some(input_line ic)
  with End_of_file -> None

let read filename =
  let ic = open_in filename in
  let rec aux acc =
    match input_line ic with
    | Some line -> aux (line::acc)
    | None -> close_in ic; Array.of_list(List.rev acc)
  in
  aux []

let dolar_pos code =
  let x = ref 0 and y = ref 0 in
  try
    Array.iter (fun line ->
      x := 0;
      String.iter (function '$' -> raise Exit | _ -> incr x) line;
      incr y) code;
    (0, 0)
  with Exit ->
    (!x, !y)

let ruld (x,y) = (y,x)
let lurd (x,y) = (-y,-x)

let incr (l,v,r) = (l,v+1,r)
let decr (l,v,r) = (l,v-1,r)

let slide_left = function
  | (p::l, v, r) -> (l, p, v::r)
  | ([], v, r) -> ([], 0, v::r)

let slide_right = function
  | (l, v, p::r) -> (v::l, p, r)
  | (l, v, []) -> (v::l, 0, [])

let put (_,v,_) = print_char(char_of_int v)
let get (l,_,r) = (l, int_of_char(input_char stdin), r)

let zero = function (_,0,_) -> true | _ -> false

let next_pos ~pos:(x,y) ~dir:(dx,dy) = (x+dx, y+dy)

let code = read Sys.argv.(1)
let max_x = Array.fold_left (fun v line -> max v (String.length line)) 0 code
let max_y = Array.length code

let get_char code (x,y) =
  if x < 0 || y < 0 || x > max_x || y > max_y then raise Exit;
  try code.(y).[x]
  with _ -> '\000'

let () =
  let rec loop pos dir cells stk =
    match (get_char code pos) with
    | '\\' -> let dir = ruld dir in loop (next_pos pos dir) dir cells stk
    | '/' -> let dir = lurd dir in loop (next_pos pos dir) dir cells stk
    | '+' -> loop (next_pos pos dir) dir (incr cells) stk
    | '-' -> loop (next_pos pos dir) dir (decr cells) stk
    | '>' -> loop (next_pos pos dir) dir (slide_right cells) stk
    | '<' -> loop (next_pos pos dir) dir (slide_left cells) stk
    | '.' -> put cells; loop (next_pos pos dir) dir cells stk
    | ',' -> loop (next_pos pos dir) dir (get cells) stk
    | '?' when zero cells
          -> loop (next_pos (next_pos pos dir) dir) dir cells stk
    | '!' -> loop (next_pos (next_pos pos dir) dir) dir cells stk
    | '@' -> let pos = next_pos pos dir in
             loop pos dir cells ((next_pos pos dir, dir)::stk)
    | '#' -> (match stk with [] -> ()
              | (pos,dir)::stk -> loop pos dir cells stk)
    | _ -> loop (next_pos pos dir) dir cells stk
  in
  loop (dolar_pos code) (1,0) ([],0,[]) []
```


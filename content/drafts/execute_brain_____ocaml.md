+++
title = "Execute Brain****/OCaml"
description = ""
date = 2010-02-06T14:23:42Z
aliases = []
[extra]
id = 2796
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
Quick implementation of a [[Brainfuck]] interpreter in [[OCaml]].

Like the [[Haskell]] [[RCBF/Haskell|version]] but without the lazy lists:

Pairs of lists are used to implement both the two-side infinite band of cells, and the program storage.

''run'' interprets a Brainfuck program as a list of characters and reads integers from ''stdin'' and outputs them to ''stdout''.

A more efficient implementation could for example only admit well-bracketed brainfuck programs, and parse bracket blocks first, to replace the ''match_left'' and ''match_right'' which need linear time.


```ocaml
let move_left  (x::l, r) = (l, x::r)
let move_right (l, x::r) = (x::l, r)

let rec match_left d =
  match d with
    '['::_, _ ->
      d
  | ']'::_, _ ->
      match_left (move_left (match_left (move_left d)))
  | _ ->
      match_left (move_left d)

let rec match_right d =
  match d with
    _, '['::_ ->
      move_right d
  | _, '['::_ ->
      match_right (match_right (move_right d))
  | _ ->
      match_right (move_right d)

let pad = function
    [], [] -> [0], [0]
  | [], r  -> [0], r
  | l,  [] -> l,   [0]
  | d      -> d;;

let modify f (l, x::r) = l, f x :: r

let rec exec p d =
  match p, d with
    (_, []),     _         -> ()
  | (_, '>'::_), _         ->
      exec (move_right p) (pad (move_right d))
  | (_, '<'::_), _         ->
      exec (move_right p) (pad (move_left  d))
  | (_, '+'::_), _         ->
      exec (move_right p) (modify succ d)
  | (_, '-'::_), _         ->
      exec (move_right p) (modify pred d)
  | (_, ','::_), _         ->
      let c = read_int () in
        exec (move_right p) (modify (fun _ -> c) d)
  | (_, '.'::_), (_, x::_) ->
      print_int x;
      print_newline ();
      exec (move_right p) d
  | (_, '['::_), (_, 0::_) ->
      exec (match_right (move_right p)) d
  | (_, '['::_), _         ->
      exec (move_right p) d
  | (_, ']'::_), (_, 0::_) ->
      exec (move_right p) d
  | (_, ']'::_), _         ->
      exec (match_left (move_left p)) d

let run s = exec ([], s) ([0], [0])
```


Example output:


```txt
# let char_list_of_string s =
  let result = ref [] in
    String.iter (fun c -> result := c :: !result) s;
    List.rev !result;;
val char_list_of_string : string -> char list = <fun>

# run (char_list_of_string ",[>+<-].>.");;
''5''
0
5
- : unit = ()
```


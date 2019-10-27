+++
title = "Soundex/OCaml"
description = ""
date = 2010-01-02T23:34:49Z
aliases = []
[extra]
id = 5264
[taxonomies]
categories = []
tags = []
+++

Here is a version with very few changes, which allows choosing different languages ('''en''' for English, '''fr''' for French) of type '''lang''', the definition of this type will be hidden in the interface.


```ocaml
type lang = char -> string

let en = function
  | 'B' | 'F' | 'P' | 'V' -> "1"
  | 'C' | 'G' | 'J' | 'K' | 'Q' | 'S' | 'X' | 'Z' -> "2"
  | 'D' | 'T' -> "3"
  | 'L' -> "4"
  | 'M' | 'N' -> "5"
  | 'R' -> "6"
  | _ -> ""

let fr = function
  | 'B' | 'P' -> "1"
  | 'C' | 'K' | 'Q' -> "2"
  | 'D' | 'T' -> "3"
  | 'L' -> "4"
  | 'M' | 'N' -> "5"
  | 'R' -> "6"
  | 'G' | 'J' -> "7"
  | 'X' | 'Z' | 'S' -> "8"
  | 'F' | 'V' -> "9"
  | _ -> ""

let rec dbl acc = function
  | [] -> (List.rev acc)
  | c::[] -> List.rev(c::acc)
  | c1::(c2::_ as tl) ->
      if c1 = c2
      then dbl acc tl
      else dbl (c1::acc) tl

let pad s =
  match String.length s with
  | 0 -> s ^ "000"
  | 1 -> s ^ "00"
  | 2 -> s ^ "0"
  | 3 -> s
  | _ -> String.sub s 0 3

let soundex_aux lang rem =
  pad(String.concat "" (dbl [] (List.map lang rem)))

let soundex ?(lang=en) s =
  let s = String.uppercase s in
  let cl = ref [] in
  String.iter (fun c -> cl := c :: !cl) s;
  match dbl [] (List.rev !cl) with
  | c::rem -> (String.make 1 c) ^ (soundex_aux lang rem)
  | [] -> invalid_arg "soundex"
```


We provide an additional function to create new lang parameters from simple data set:


```ocaml
type lang_set = (int * char list) list

let make_lang ss =
  let ss = List.map (fun (d, li) -> string_of_int d, li) ss in
  let ss = List.map (fun (d, li) -> List.map (fun c -> c, d) li) ss in
  let ss = List.flatten ss in
  (fun c -> try List.assoc c ss with Not_found -> "")
```


in the interface as the definition of the type '''lang''' is hidden we can use it easily as a classic parameter:


```ocaml
type lang

val en : lang
val fr : lang

val soundex : ?lang:lang -> string -> string

type lang_set = (int * char list) list
val make_lang : lang_set -> lang
```


The '''lang''' parameter for the '''soundex''' function is optional, if omited the english set is used.

We put the implementation in the file ''soundex.ml'' and the interface in ''soundex.mli'', then we compile with:
 ocamlc -c soundex.mli
 ocamlc -c soundex.ml

Test that the function make_lang provide the same results:


```ocaml
open Soundex

let tests = [
  "Soundex"; "Example"; "Sownteks"; "Ekzampul"; "Euler"; "Gauss"; "Hilbert";
  "Knuth"; "Lloyd"; "Lukasiewicz"; "Ellery"; "Ghosh"; "Heilbronn"; "Kant";
  "Ladd"; "Lissajous"; "Wheaton"; "Ashcraft"; "Burroughs"; "Burrows"; "O'Hara" ]

let fr_set = [
  1, ['B'; 'P'];
  2, ['C'; 'K'; 'Q'];
  3, ['D'; 'T'];
  4, ['L'];
  5, ['M'; 'N'];
  6, ['R'];
  7, ['G'; 'J'];
  8, ['X'; 'Z'; 'S'];
  9, ['F'; 'V'];
]

let my_fr = make_lang fr_set ;;

let () =
  print_endline " Word   \t Fr    Check Status";
  List.iter (fun word ->
    let code1 = soundex ~lang:Soundex.fr word in
    let code2 = soundex ~lang:my_fr word in
    let status = if code1 = code2 then "OK " else "Arg" in
    Printf.printf " \"%s\" \t %s  %s  %s\n" word code1 code2 status
  ) tests
```


We can run this test file with:
 ocaml soundex.cmo test.ml

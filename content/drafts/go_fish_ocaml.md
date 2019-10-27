+++
title = "Go Fish/OCaml"
description = ""
date = 2010-08-30T01:17:17Z
aliases = []
[extra]
id = 5228
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}


```ocaml
type pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
           Jack | Queen | King | Ace 
let pips = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
            Jack; Queen; King; Ace]
 
type suit = Diamonds | Spades | Hearts | Clubs
let suits = [Diamonds; Spades; Hearts; Clubs]
 
type card = pip * suit

let string_of_pip = function
  | Two   -> "Two"
  | Three -> "Three"
  | Four  -> "Four"
  | Five  -> "Five"
  | Six   -> "Six"
  | Seven -> "Seven"
  | Eight -> "Eight"
  | Nine  -> "Nine"
  | Ten   -> "Ten"
  | Jack  -> "Jack"
  | Queen -> "Queen"
  | King  -> "King"
  | Ace   -> "Ace"
 
let string_of_suit = function
  | Diamonds -> "Diamonds"
  | Spades   -> "Spades"
  | Hearts   -> "Hearts"
  | Clubs    -> "Clubs"

let string_of_card (pip, suit) =
  (Printf.sprintf "(%s-%s)" (string_of_pip pip) (string_of_suit suit))

 
let pip_of_card (pip, _) = (pip)

let deck =
  List.concat (List.map (fun pip -> List.map (fun suit -> (pip, suit)) suits) pips)


type rank_state =
  | Unknown   (* Don't know if the opponent has any cards in that rank. *)
  | No_cards  (* Opponent has no cards there; I took them away, or I asked yet. *)
  | Has_cards (* Opponent has cards there; they tried to get them off me and haven't booked them yet. *)
  | Booked    (* Someone has booked the rank. *)

let state_score = function
  | Booked    -> 0
  | No_cards  -> 1
  | Unknown   -> 2
  | Has_cards -> 3

let string_of_state = function
  | Booked    -> "Booked"
  | No_cards  -> "No_cards"
  | Unknown   -> "Unknown"
  | Has_cards -> "Has_cards"

let replace ((rank,_) as state) opp =
  let rec aux acc = function
  | (_rank,_)::tl when _rank = rank -> List.rev_append acc (state::tl)
  | hd::tl -> aux (hd::acc) tl
  | [] -> assert(false)
  in
  aux [] opp ;;


class virtual abstract_player =
  object (s)
    val mutable virtual cards : card list
    val mutable virtual books : pip list
    method virtual ask_rank : unit -> pip
    method virtual give_rank : pip -> card list
    method virtual notify_booked : pip -> unit
    method virtual request_failed : pip -> unit

    method private cards_given rank =
      let matched, rest = List.partition (fun (pip,_) -> pip = rank) cards in
      if List.length matched = 4 then begin
        cards <- rest;
        books <- rank :: books;
        s#notify_booked rank;
        (Some rank)
      end
      else (None)

    method give_card (card : card) =
      let rank = pip_of_card card in
      cards <- card :: cards;
      s#cards_given rank

    method give_cards (_cards : card list) =
      let rank =
        match _cards with
        | [] -> invalid_arg "empty list"
        | hd::tl ->
            List.fold_left
              (fun rank1 (rank2,_) ->
                if rank1 <> rank2
                then invalid_arg "!= ranks"
                else (rank1)
              ) (pip_of_card hd) tl
      in
      cards <- _cards @ cards;
      s#cards_given rank

    method give_rank rank =
      let give, _cards = List.partition (fun (pip, _) -> pip = rank) cards in
      cards <- _cards;
      (give)

    method books_length =
      (List.length books)

    method empty_hand =
      cards = []

    method private dump_cards() =
      print_endline(String.concat ", " (List.map string_of_card cards));

  end



class human_player =
  object (s) inherit abstract_player

    val mutable cards = []
    val mutable books = []

    method ask_rank() =
      let ranks =
        List.fold_left (fun acc card ->
          let rank = pip_of_card card in
          if List.mem rank acc
          then (acc)
          else (rank::acc)
        )
        [] cards
      in
      s#dump_cards();
      Printf.printf "Ranks: %s\n%!" (String.concat ", " (List.map string_of_pip ranks));
      let n = List.length ranks in
      Printf.printf "choose from 1 to %d\n%!" n;
      let get_int() =
        try read_int()
        with Failure "int_of_string" -> raise Exit
      in
      let rec aux() =
        let d = get_int() in
        if d <= 0 || d > n then aux() else (pred d)
      in
      let d = aux() in
      (List.nth ranks d)

    method notify_booked rank =
      Printf.printf "Rank [%s] is now booked\n%!" (string_of_pip rank);

    method request_failed rank = ()
  end



class ai_player =
  object (s) inherit abstract_player as parent

    val mutable cards = []
    val mutable books = []
    val mutable opponent = List.map (fun rank -> (rank, Unknown)) pips

    method private dump_state() =
      let f (pip, state) =
        Printf.sprintf "{%s:%s}" (string_of_pip pip) (string_of_state state)
      in
      print_endline(String.concat ", " (List.map f opponent));

    method ask_rank() =
      let ranks =
        List.fold_left (fun acc card ->
          let rank = pip_of_card card in
          try
            let _,n = List.find (fun (_rank,_) -> _rank = rank) acc in
            (replace (rank, n+1) acc)
          with Not_found ->
            ((rank,1)::acc)
        )
        [] cards
      in
      let f (rank,_) =
        (state_score(List.assoc rank opponent))
      in
      let ranks = List.sort (fun a b -> (f b) - (f a)) ranks in
      (* DEBUG
      Printf.printf "Ranks: %s\n%!" (String.concat ", " (List.map string_of_pip ranks));
      s#dump_state();
      s#dump_cards();
      *)
      opponent <- List.sort (fun _ _ -> Random.int 9 - Random.int 9) opponent;
      match ranks with
      | [] -> Jack
      | (x,_)::_ -> x

    method give_cards (_cards : card list) =
      let rank = pip_of_card(List.hd _cards) in
      opponent <- replace (rank, No_cards) opponent;
      (parent#give_cards _cards)

    method give_rank rank =
      opponent <- replace (rank, Has_cards) opponent;
      (parent#give_rank rank)

    method notify_booked rank =
      opponent <- replace (rank, Booked) opponent

    method request_failed rank =
      opponent <- replace (rank, No_cards) opponent
  end



class random_player =
  object (s) inherit ai_player

    method ask_rank() =
      let ranks =
        List.fold_left (fun acc card ->
          let rank = pip_of_card card in
          if List.mem rank acc
          then (acc)
          else (rank::acc)
        )
        [] cards
      in
      let n = List.length ranks in
      let d = Random.int n in
      (List.nth ranks d)

  end



exception Empty_deck
let card_to_player deck player op =
  match deck with
  | card::deck ->
      begin match player#give_card card with
      | None -> ()
      | Some rank -> op#notify_booked rank
      end;
      (deck)
  | _ -> raise Empty_deck

let n_cards_to_player n deck player op =
  let rec aux i deck =
    if i >= n then (deck) else
      let deck = card_to_player deck player op in
      aux (succ i) deck
  in
  aux 0 deck ;;


let () =
  Random.self_init();
  let deck = List.sort (fun _ _ -> Random.int 9 - Random.int 9) deck in
  let player_a = new human_player
  and player_b = new ai_player in
  let deck = n_cards_to_player 9 deck player_a player_b in
  let deck = n_cards_to_player 9 deck player_b player_a in
  let deck = ref deck in
  let empty_hand player1 player2 =
    if player1#empty_hand
    then deck := card_to_player !deck player1 player2
  in
  let rec make_turn id1 id2 player1 player2 =
    print_newline();
    (try
       empty_hand player1 player2;
       empty_hand player2 player1;
     with Empty_deck -> ());
    if player1#books_length + player2#books_length <> 13
    then begin
      let rank = player1#ask_rank() in
      Printf.printf "player %s asked for %ss\n%!" id1 (string_of_pip rank);
      let cards = player2#give_rank rank in
      match cards with
      | [] ->
          Printf.printf "player %s has no %ss\n%!" id2 (string_of_pip rank);
          player1#request_failed rank;
          (try
             deck := card_to_player !deck player1 player2;
             make_turn id2 id1 player2 player1
           with Empty_deck -> ())

      | cards ->
          let given = String.concat ", " (List.map string_of_card cards) in
          Printf.printf "player %s gives %s\n%!" id2 given;
          begin match player1#give_cards cards with
          | None -> ()
          | Some rank ->
              Printf.printf "player %s booked [%s]\n%!" id1 (string_of_pip rank);
              player2#notify_booked rank;
          end;
          make_turn id1 id2 player1 player2
    end
  in
  (try
     if Random.bool()
     then make_turn "a" "b" player_a player_b
     else make_turn "b" "a" player_b player_a;
   with Exit -> ());

  Printf.printf "player a has %d books\n" (player_a#books_length);
  Printf.printf "player b has %d books\n" (player_b#books_length);
;;
```


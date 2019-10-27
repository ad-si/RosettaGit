+++
title = "Go Fish/Mathematica"
description = ""
date = 2015-05-01T21:05:30Z
aliases = []
[extra]
id = 19084
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
The AI simply picks the rank that it has the most of.

```Mathematica
ranks = {"Ace", "Two", "Three", "Four", "Five", "Six", "Seven", 
   "Eight", "Nine", "Ten", "Jack", "Queen", "King"};
suits = {"Clubs", "Diamonds", "Hearts", "Spades"};
deck = Tuples[{Range[13], Range[4]}];
cardName[{rank_, suit_}] := ranks[[rank]] <> " of " <> suits[[suit]];
Attributes[drawCards] = {HoldRest};
drawCards[num_, src_, dest_] := 
  Module[{cards = RandomSample[src, Min[num, Length[src]]]}, 
   dest = Join[dest, cards];
   src = Fold[DeleteCases[#, #2, 1, Count[cards, #2]] &, src, 
     Union[cards]];];
fixHand[{a___, {rank_, _}, b___, {rank_, _}, c___, {rank_, _}, 
    d___, {rank_, _}, e___}, prev_] := 
  fixHand[{a, b, c, d, e}, prev + 1];
fixHand[hand_, prev_] := {hand, prev};
player = opp = {};
drawCards[9, deck, player];
drawCards[9, deck, opp];
{player, playerBooks} = fixHand[player, 0];
{opp, oppBooks} = fixHand[opp, 0];
playerTurn = True;
While[player != {} || opp != {} || deck != {}, 
 If[player == {}, drawCards[1, deck, player]]; 
 If[opp == {}, drawCards[1, deck, opp]]; 
 If[playerTurn, 
  If[player == {}, 
   DialogInput[
    DialogNotebook[{TextCell[
       "You have no cards and the deck is empty!", 
       FontFamily -> "Arial"], DefaultButton[]}]], 
   Module[{choice = 
      ChoiceDialog[
       "Your opponent has " <> IntegerString[oppBooks] <> 
        " books, and you have " <> IntegerString[playerBooks] <> 
        " books and these cards: " <> 
        StringRiffle[cardName /@ SortBy[player, #[[1]] + #[[2]]/10 &],
          ", "] <> "\nWhich rank will you call?", 
       ranks[[#]] -> # & /@ Union[First /@ player]]}, 
    If[MemberQ[opp, {choice, _}], 
     player = Join[player, Cases[opp, {choice, _}]]; 
     opp = DeleteCases[opp, {choice, _}], drawCards[1, deck, player]; 
     DialogInput[
      DialogNotebook[{TextCell["Go fish! You gained a card.", 
         FontFamily -> "Arial"], DefaultButton[]}]]; 
     playerTurn = False]]], 
  If[opp != {}, 
   Module[{choice = Commonest[First /@ opp, 1][[1]]}, 
    If[MemberQ[player, {choice, _}], 
     opp = Join[opp, Cases[player, {choice, _}]]; 
     player = DeleteCases[player, {choice, _}], 
     playerTurn = True]]]]; {player, playerBooks} = 
  fixHand[player, playerBooks]; {opp, oppBooks} = 
  fixHand[opp, oppBooks]]; MessageDialog[
 Which[oppBooks > playerBooks, 
  "Your opponent wins with " <> IntegerString[oppBooks] <> " books!", 
  playerBooks > oppBooks, 
  "You win with " <> IntegerString[playerBooks] <> " books!", True, 
  "Tie!"]];
```


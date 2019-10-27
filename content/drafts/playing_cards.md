+++
title = "Playing cards"
description = ""
date = 2019-10-17T23:23:11Z
aliases = []
[extra]
id = 2837
[taxonomies]
categories = []
tags = []
+++

{{task|Games}} 
[[Category:Cards]]

;Task:
Create a data structure and the associated methods to define and manipulate a deck of   [[wp:Playing-cards#Anglo-American-French|playing cards]]. 

The deck should contain 52 unique cards. 

The methods must include the ability to:
:::*   make a new deck
:::*   shuffle (randomize) the deck
:::*   deal from the deck
:::*   print the current contents of a deck 

Each card must have a pip value and a suit value which constitute the unique value of the card.





## ACL2


```Lisp
(defun remove-nth (i xs)
   (if (or (zp i) (endp (rest xs)))
       (rest xs)
       (cons (first xs)
             (remove-nth (1- i) (rest xs)))))

(defthm remove-nth-shortens
   (implies (consp xs)
            (< (len (remove-nth i xs)) (len xs))))

:set-state-ok t

(defun shuffle-r (xs ys state)
   (declare (xargs :measure (len xs)))
   (if (endp xs)
       (mv ys state)
       (mv-let (idx state)
               (random$ (len xs) state)
          (shuffle-r (remove-nth idx xs)
                     (cons (nth idx xs) ys)
                     state))))

(defun shuffle (xs state)
   (shuffle-r xs nil state))

(defun cross-r (x ys)
   (if (endp ys)
       nil
       (cons (cons x (first ys))
             (cross-r x (rest ys)))))

(defun cross (xs ys)
   (if (or (endp xs) (endp ys))
       nil
       (append (cross-r (first xs) ys)
               (cross (rest xs) ys))))

(defun make-deck ()
   (cross '(ace 2 3 4 5 6 7 8 9 10 jack queen king)
          '(hearts diamonds clubs spades)))

(defun print-card (card)
   (cw "~x0 of ~x1~%" (car card) (cdr card)))

(defun print-deck (deck)
   (if (endp deck)
       nil
       (progn$ (print-card (first deck))
               (print-deck (rest deck)))))

(defun draw-from-deck (deck)
   (mv (first deck) (rest deck)))

```


Example (first 4 cards of a shuffled deck):

```txt
&gt; (mv-let (deck state) (shuffle (make-deck) state)
     (mv (print-deck (take 4 deck)) state))
5 of CLUBS
ACE of DIAMONDS
6 of HEARTS
4 of DIAMONDS
(NIL <state>)
```



## Ada

See [[Playing Cards/Ada]]


## ALGOL 68

The scoping rules of ALGOL 68 tend to make object oriented coding in ALGOL 68 difficult.  
Also as invoking PROC is done before members of a STRUCT are extracted the programmer 
one quickly finds it is necessary to use numerous brackets.  

e.g. compare "<tt>(shuffle OF deck class)(deck)</tt>" with python's simple "<tt>deck.shuffle()</tt>"

For further details:
* [http://archive.computerhistory.org/resources/text/algol/algol_bulletin/A26/P22.HTM Ross, D.T., "Features Essential for a Workable Algol X," ALGOL Bulletin, No. 26, August 1967, pp. 6-12, and ACM SIGPLAN Notices, Vol.2, No.11, November 1967.]
* [http://www.csail.mit.edu/timeline/timeline.php?query=event&id=93 LOC was essential; Algol 68's REF was a mistake.]


```algol68
MODE CARD = STRUCT(STRING pip, suit); # instance attributes #

# class members & attributes #
STRUCT(
  PROC(REF CARD, STRING, STRING)VOID init, 
  FORMAT format,
  PROC(REF CARD)STRING repr,
  []STRING suits, pips
) class card = (
# PROC init = # (REF CARD self, STRING pip, suit)VOID:(
    pip OF self:=pip;
    suit OF self :=suit
  ),
# format = # $"("g" OF "g")"$,
# PROC repr = # (REF CARD self)STRING: ( 
    HEAP STRING out; putf(BOOK out,(format OF class card,self)); out
  ),
# suits = # ("Clubs","Hearts","Spades","Diamonds"),
# pips = # ("2","3","4","5","6","7","8","9","10","Jack","Queen","King","Ace")
);
 
MODE DECK = STRUCT(REF[]CARD deck); # instance attributes #

# class members & attributes #
STRUCT( 
  PROC(REF DECK)VOID init, shuffle, 
  PROC(REF DECK)STRING repr, 
  PROC(REF DECK)CARD deal
) class deck = (
 
# PROC init = # (REF DECK self)VOID:(
    HEAP[ UPB suits OF class card * UPB pips OF class card ]CARD new;
    FOR suit TO UPB suits OF class card DO
      FOR pip TO UPB pips OF class card DO
        new[(suit-1)*UPB pips OF class card + pip] :=
           ((pips OF class card)[pip], (suits OF class card)[suit])
      OD
    OD;
    deck OF self := new
  ),
 
# PROC shuffle = # (REF DECK self)VOID:
    FOR card TO UPB deck OF self DO
      CARD this card = (deck OF self)[card];
      INT random card = random int(LWB deck OF self,UPB deck OF self);
      (deck OF self)[card] := (deck OF self)[random card];
      (deck OF self)[random card] := this card
    OD,
 
# PROC repr = # (REF DECK self)STRING: (
    FORMAT format = $"("n(UPB deck OF self-1)(f(format OF class card)", ")f(format OF class card)")"$;
    HEAP STRING out; putf(BOOK out,(format, deck OF self)); out
  ),
 
# PROC deal = # (REF DECK self)CARD: (
    (shuffle OF class deck)(self);
    (deck OF self)[UPB deck OF self]
  )
);
 
# associate a STRING with a FILE for easy text manipulation #
OP BOOK = (REF STRING string)REF FILE:(
  HEAP FILE book;
  associate(book, string);
  book
);
 
# Pick a random integer between from [lwb..upb] #
PROC random int = (INT lwb, upb)INT: 
  ENTIER(random * (upb - lwb + 1) + lwb);
 
DECK deck;
(init OF class deck)(deck);
(shuffle OF class deck)(deck);
print (((repr OF class deck)(deck), new line))
```


{{Out|Example output}}

```txt
((King OF Clubs), (6 OF Hearts), (7 OF Diamonds), (Ace OF Hearts), (9 OF Spades), (10 OF Clubs), (Ace OF Spades), (8 OF Clubs), (4 OF Spades), (8 OF Hearts), (Jack OF Hearts), (3 OF Clubs), (7 OF Hearts), (10 OF Hearts), (Jack OF Clubs), (Ace OF Clubs), (King OF Spades), (9 OF Clubs), (7 OF Spades), (5 OF Spades), (7 OF Clubs), (Queen OF Clubs), (9 OF Diamonds), (2 OF Spades), (6 OF Diamonds), (Ace OF Diamonds), (Queen OF Diamonds), (5 OF Hearts), (4 OF Clubs), (5 OF Clubs), (4 OF Hearts), (3 OF Diamonds), (4 OF Diamonds), (3 OF Hearts), (King OF Diamonds), (2 OF Clubs), (Jack OF Spades), (2 OF Diamonds), (5 OF Diamonds), (Queen OF Spades), (10 OF Diamonds), (King OF Hearts), (Jack OF Diamonds), (Queen OF Hearts), (8 OF Spades), (2 OF Hearts), (8 OF Diamonds), (10 OF Spades), (9 OF Hearts), (6 OF Clubs), (3 OF Spades), (6 OF Spades))
```




## ATS


```ATS

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)
//
abst@ype
pip_type = int
abst@ype
suit_type = int
//
abst@ype
card_type = int
//
(* ****** ****** *)

typedef pip = pip_type
typedef suit = suit_type

(* ****** ****** *)

typedef card = card_type

(* ****** ****** *)
//
extern
fun
pip_make: natLt(13) -> pip
extern
fun
pip_get_name: pip -> string
extern
fun
pip_get_value: pip -> intBtwe(1, 13)
//
extern
fun
suit_make: natLt(4) -> suit
extern
fun
suit_get_name: suit -> string
extern
fun
suit_get_value: suit -> intBtwe(1, 4)
//
overload .name with pip_get_name
overload .name with suit_get_name
overload .value with pip_get_value
overload .value with suit_get_value
//
(* ****** ****** *)
//
(*
  | Two | Three | Four | Five
  | Six | Seven | Eight | Nine
  | Ten | Jack | Queen | King | Ace 
*)
//
(*
  | Spade | Heart | Diamond | Club
*)
//
(* ****** ****** *)

local

assume
pip_type = natLt(13)

in (* in-of-local *)

implement
pip_make(x) = x
implement
pip_get_value(x) = x + 1

end // end of [local]

(* ****** ****** *)

local

assume
suit_type = natLt(4)

in (* in-of-local *)

implement
suit_make(x) = x
implement
suit_get_value(x) = x + 1

end // end of [local]

(* ****** ****** *)

implement
pip_get_name
  (x) =
(
case+
x.value()
of // case+
| 1 => "Ace"
| 2 => "Two"
| 3 => "Three"
| 4 => "Four"
| 5 => "Five"
| 6 => "Six"
| 7 => "Seven"
| 8 => "Eight"
| 9 => "Nine"
| 10 => "Ten"
| 11 => "Jack"
| 12 => "Queen"
| 13 => "King"
)

(* ****** ****** *)
//
implement
suit_get_name
  (x) =
(
case+
x.value()
of // case+
| 1 => "S" | 2 => "H" | 3 => "D" | 4 => "C"
) (* end of [suit_get_name] *)
//
(* ****** ****** *)
//
extern
fun
card_get_pip: card -> pip
extern
fun
card_get_suit: card -> suit
//
extern
fun
card_make: natLt(52) -> card
extern
fun
card_make_suit_pip: (suit, pip) -> card
//
(* ****** ****** *)

extern
fun
fprint_pip : fprint_type(pip)
extern
fun
fprint_suit : fprint_type(suit)
extern
fun
fprint_card : fprint_type(card)

(* ****** ****** *)

overload .pip with card_get_pip
overload .suit with card_get_suit

(* ****** ****** *)

implement
fprint_val<card> = fprint_card

(* ****** ****** *)

overload fprint with fprint_pip
overload fprint with fprint_suit
overload fprint with fprint_card

(* ****** ****** *)

local

assume
card_type = natLt(52)

in (* in-of-local *)
//
implement
card_get_pip
  (x) = pip_make(nmod(x, 13))
implement
card_get_suit
  (x) = suit_make(ndiv(x, 13))
//
implement
card_make(xy) = xy
//
implement
card_make_suit_pip(x, y) =
  (x.value()-1) * 13 + (y.value()-1)
//
end // end of [local]

(* ****** ****** *)
//
implement
fprint_pip(out, x) =
  fprint!(out, x.name())
implement
fprint_suit(out, x) =
  fprint!(out, x.name())
//
implement
fprint_card(out, c) =
  fprint!(out, c.suit(), "(", c.pip(), ")")
//
(* ****** ****** *)
//
absvtype
deck_vtype(n:int) = ptr
//
vtypedef deck(n:int) = deck_vtype(n)
//
(* ****** ****** *)
//
extern
fun
deck_get_size
  {n:nat}(!deck(n)): int(n)
//
extern
fun
deck_is_empty
  {n:nat}(!deck(n)): bool(n==0)
//
overload iseqz with deck_is_empty
//
(* ****** ****** *)
//
extern
fun
deck_free{n:int}(deck(n)): void
//
overload .free with deck_free
//
(* ****** ****** *)
//
extern
fun
deck_make_full((*void*)): deck(52)
//
(* ****** ****** *)
//
extern
fun
fprint_deck
  {n:nat}(FILEref, !deck(n)): void
//
overload fprint with fprint_deck
//
(* ****** ****** *)
//
extern
fun
deck_shuffle
  {n:nat}(!deck(n) >> _): void
//
overload .shuffle with deck_shuffle
//
(* ****** ****** *)
//
extern
fun
deck_takeout_top
  {n:pos}(!deck(n) >> deck(n-1)): card
//
(* ****** ****** *)

local
//
datavtype
deck(int) =
| {n:nat}
  Deck(n) of
  (
    int(n)
  , list_vt(card, n)
  ) // end of [Deck]
//
assume
deck_vtype(n:int) = deck(n)
//
in (* in-of-local *)

implement
deck_get_size
  (deck) =
(
let val+Deck(n, _) = deck in n end
)

implement
deck_is_empty
  (deck) =
(
let val+Deck(n, _) = deck in n = 0 end
)

(* ****** ****** *)
//
implement
deck_free(deck) =
(
let val+~Deck(n, xs) = deck in free(xs) end
) (* end of [deck_free] *)
//
(* ****** ****** *)

implement
deck_make_full
  ((*void*)) = let
//
val xys =
list_make_intrange(0, 52)
//
val cards =
list_vt_mapfree_fun<natLt(52)><card>(xys, lam xy => card_make(xy))
//
in
  Deck(52, cards)  
end // end of [deck_make_full]

(* ****** ****** *)

implement
fprint_deck
  (out, deck) = let
//
val+Deck(n, xs) = deck
//
in
//
fprint_list_vt(out, xs)
//
end // end of [fprint_deck]

(* ****** ****** *)

implement
deck_shuffle
  (deck) =
  fold@(deck) where
{
//
val+@Deck(n, xs) = deck
//
implement
list_vt_permute$randint<>
  (n) = randint(n)
//
val ((*void*)) =
  (xs := list_vt_permute(xs))
//
} (* end of [deck_shuffle] *)

(* ****** ****** *)

implement
deck_takeout_top
  (deck) = let
//
val+@Deck(n, xs) = deck
//
val+
~list_vt_cons(x0, xs_tl) = xs
//
val ((*void*)) = n := n - 1
val ((*void*)) = (xs := xs_tl)
//
in
  fold@(deck); x0(*top*)
end // end of [deck_takeout_top]

end // end of [local]

(* ****** ****** *)

implement
main0((*void*)) =
{
//
val () =
println!
(
"Hello from [Playing_cards]!"
) (* println! *)
//
val out = stdout_ref
//
val theDeck =
  deck_make_full((*void*))
//
val ((*void*)) =
  fprintln!(out, "theDeck = ", theDeck)
//
val ((*void*)) =
  theDeck.shuffle((*void*))
//
val ((*void*)) =
  fprintln!(out, "theDeck = ", theDeck)
//
val ((*void*)) =
  loop_deal(theDeck) where
{
//
fun
loop_deal{n:nat}
(
  deck: !deck(n) >> deck(0)
) : void =
(
  if (
  iseqz(deck)
  ) then ((*void*))
    else
      let
        val card =
        deck_takeout_top(deck)
      in
        fprintln!(out, card); loop_deal(deck)
      end // end of [let]
    // end of [else]
)
//
} (* end of [val] *)
//
val ((*freed*)) = theDeck.free()
//
} (* end of [main0] *)

(* ****** ****** *)

```



## AutoHotkey


```AutoHotkey
suits := ["♠", "♦", "♥", "♣"]
values := [2,3,4,5,6,7,8,9,10,"J","Q","K","A"]
Gui, font, s14
Gui, add, button, w190 gNewDeck, New Deck
Gui, add, button, x+10 wp gShuffle, Shuffle
Gui, add, button, x+10 wp gDeal, Deal
Gui, add, text, xs w600 , Current Deck:
Gui, add, Edit, xs wp r4 vDeck
Gui, add, text, xs , Hands:
Gui, add, Edit, x+10 w60  vHands gHands
Gui, add, UpDown,, 1
Edits := 0

Hands:
Gui, Submit, NoHide
loop, % Edits
	GuiControl,Hide, Hand%A_Index%

loop, % Hands
	GuiControl,Show, % "Hand" A_Index

loop, % Hands - Edits
{
	Edits++
	Gui, add, ListBox, % "x" (Edits=1?"s":"+10") " w60 r13 vHand" Edits
}
Gui, show, AutoSize
return
;-----------------------------------------------
GuiClose:
ExitApp
return
;-----------------------------------------------
NewDeck:
cards := [], deck := Dealt:= ""

loop, % Hands
	GuiControl,, Hand%A_Index%, |

for each, suit in suits
	for each, value in values
		cards.Insert(value suit)

for each, card in cards
	deck .= card (mod(A_Index, 13) ? " " : "`n")
GuiControl,, Deck, % deck
GuiControl,, Dealt
GuiControl, Enable, Button2
GuiControl, Enable, Hands
return
;-----------------------------------------------
shuffle:
gosub, NewDeck
shuffled := [], deck := ""
loop, 52 {
	Random, rnd, 1, % cards.MaxIndex()
	shuffled[A_Index] := cards.RemoveAt(rnd)
}
for each, card in shuffled
{
	deck .= card (mod(A_Index, 13) ? " " : "`n")	
	cards.Insert(card)
}
GuiControl,, Deck, % deck
return
;-----------------------------------------------
Deal:
Gui, Submit, NoHide
if ( Hands > cards.MaxIndex())
	return

deck := ""
loop, % Hands
	GuiControl,, Hand%A_Index%, % cards.RemoveAt(1)

GuiControl, Disable, Button2
GuiControl, Disable, Hands
GuiControl,, Dealt, % Dealt

for each, card in cards
	deck .= card (mod(A_Index, 13) ? " " : "`n")
GuiControl,, Deck, % deck
return
;-----------------------------------------------
```



## AutoIt


```AutoIt

#Region ;**** Directives created by AutoIt3Wrapper_GUI ****
#AutoIt3Wrapper_Change2CUI=y
#EndRegion ;**** Directives created by AutoIt3Wrapper_GUI ****
#include <Array.au3>

; ## GLOBALS ##
Global $SUIT = ["D", "H", "S", "C"]
Global $FACE = [2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K", "A"]
Global $DECK[52]

; ## CREATES A NEW DECK
Func NewDeck()

	For $i = 0 To 3
		For $x = 0 To 12
			_ArrayPush($DECK, $FACE[$x] & $SUIT[$i])
		Next
	Next

EndFunc   ;==>NewDeck

; ## SHUFFLE DECK
Func Shuffle()

	_ArrayShuffle($DECK)

EndFunc   ;==>Shuffle

; ## DEAL A CARD
Func Deal()

	Return _ArrayPop($DECK)

EndFunc   ;==>Deal

; ## PRINT DECK
Func Print()

	ConsoleWrite(_ArrayToString($DECK) & @CRLF)

EndFunc   ;==>Print


#Region ;#### USAGE ####
NewDeck()
Print()
Shuffle()
Print()
ConsoleWrite("DEALT: " & Deal() & @CRLF)
Print()
#EndRegion ;#### USAGE ####

```

{{Out|Example output}}

```txt

2D|3D|4D|5D|6D|7D|8D|9D|10D|JD|QD|KD|AD|2H|3H|4H|5H|6H|7H|8H|9H|10H|JH|QH|KH|AH|2S|3S|4S|5S|6S|7S|8S|9S|10S|JS|QS|KS|AS|2C|3C|4C|5C|6C|7C|8C|9C|10C|JC|QC|KC|AC
2C|4D|6H|AC|KC|6S|3C|AS|KS|3H|4S|10D|AD|8D|7C|QH|4C|5H|5S|3D|2D|7D|9C|2S|QD|6C|KH|7S|3S|7H|8H|JH|QC|JC|10H|JD|KD|5D|5C|9H|8S|9D|QS|8C|10C|2H|6D|10S|9S|JS|AH|4H
DEALT: 4H
2C|4D|6H|AC|KC|6S|3C|AS|KS|3H|4S|10D|AD|8D|7C|QH|4C|5H|5S|3D|2D|7D|9C|2S|QD|6C|KH|7S|3S|7H|8H|JH|QC|JC|10H|JD|KD|5D|5C|9H|8S|9D|QS|8C|10C|2H|6D|10S|9S|JS|AH

```



## Batch File

The data structure is a simple string with 3 characters per card, one char for the suit, a char for the pip (T) means ten and a last char not printed that would allow to sort a hand.

```Batch File

@echo off
setlocal enabledelayedexpansion

call:newdeck deck 
echo new deck:
echo.
call:showcards deck 
echo.
echo shuffling:
echo.
call:shuffle deck 
call:showcards deck 
echo.
echo dealing 5 cards to 4 players
call:deal deck 5 hand1 hand2 hand3 hand4
echo.
echo player 1 & call:showcards hand1 
echo.
echo player 2 & call:showcards hand2
echo.
echo player 3 & call:showcards hand3
echo.
echo player 4 & call:showcards hand4
echo.
call:count %deck% cnt
echo %cnt% cards remaining in the deck 
echo.
 call:showcards deck
echo.
 
exit /b

:getcard deck hand  :: deals 1 card to a player
   set "loc1=!%~1!"
   set "%~2=!%~2!!loc1:~0,3!"
   set "%~1=!loc1:~3!"
exit /b

:deal deck n player1 player2...up to 7 
 set "loc=!%~1!" 
 set "cards=%~2"
 set players=%3 %4 %5 %6 %7 %8 %9 
 for /L %%j in (1,1,!cards!) do (
     for %%k in (!players!) do call:getcard loc %%k)
 set "%~1=!loc!" 
 exit /b

:newdeck  [deck]   ::creates a deck of cards 
 :: in the parentheses below there are ascii chars 3,4,5 and 6 representing the suits
 for %%i in ( ♠ ♦ ♥ ♣ ) do ( 
   for %%j in (20 31 42 53 64 75 86 97 T8 J9 QA KB AC) do set loc=!loc!%%i%%j
   )
 set "%~1=!loc!"
exit /b 

:showcards  [deck]  :: prints a deck or a hand
 set "loc=!%~1!" 
  for /L %%j in (0,39,117) do ( 
     set s= 
     for /L %%i in (0,3,36) do ( 
        set /a n=%%i+%%j
        call set  s=%%s%% %%loc:~!n!,2%% 
      ) 
  if "!s: =!" neq "" echo(!s!
  set /a n+=1
  if "%loc:~!n!,!%" equ "" goto endloop
  )
 :endloop
 exit /b
 
:count deck count
set "loc1=%1"
set /a cnt1=0
for %%i in (96 48 24 12 6 3 ) do if "!loc1:~%%i,1!" neq "" set /a cnt1+=%%i & set loc1=!loc1:~%%i!
set /a cnt1=cnt1/3+1
set "%~2=!cnt1!"
exit /b

:shuffle (deck)   :: shuffles a deck
 set "loc=!%~1!"
 call:count %loc%, cnt
 set /a cnt-=1
 for /L %%i in (%cnt%,-1,0) do (
    SET /A "from=%%i,to=(!RANDOM!*(%%i-1)/32768)"
	call:swap loc from to
  )
  set "%~1=!loc!"
 exit /b 
 
 :swap deck from to   :: swaps two cards
    set "arr=!%~1!" 
    set /a "from=!%~2!*3,to=!%~3!*3"
	set temp1=!arr:~%from%,3!
    set temp2=!arr:~%to%,3!
    set arr=!arr:%temp1%=@@@!
    set arr=!arr:%temp2%=%temp1%!
    set arr=!arr:@@@=%temp2%!
	set "%~1=!arr!"
 exit /b

```

{{Out}}

```txt

new deck:

  ♠2  ♠3  ♠4  ♠5  ♠6  ♠7  ♠8  ♠9  ♠T  ♠J  ♠Q  ♠K  ♠A
  ♦2  ♦3  ♦4  ♦5  ♦6  ♦7  ♦8  ♦9  ♦T  ♦J  ♦Q  ♦K  ♦A
  ♥2  ♥3  ♥4  ♥5  ♥6  ♥7  ♥8  ♥9  ♥T  ♥J  ♥Q  ♥K  ♥A
  ♣2  ♣3  ♣4  ♣5  ♣6  ♣7  ♣8  ♣9  ♣T  ♣J  ♣Q  ♣K  ♣A

shuffling:

  ♥8  ♥7  ♦K  ♠K  ♥2  ♥5  ♥A  ♠7  ♣8  ♠A  ♦7  ♠9  ♠3
  ♠J  ♣J  ♦9  ♠8  ♦A  ♣Q  ♦2  ♦5  ♣K  ♥9  ♦4  ♣2  ♣6
  ♦6  ♥Q  ♦8  ♥6  ♣5  ♥T  ♦3  ♠6  ♣7  ♠5  ♣T  ♣3  ♠T
  ♠2  ♦Q  ♠Q  ♦T  ♥3  ♥K  ♦J  ♥4  ♠4  ♣4  ♥J  ♣A  ♣9

dealing 5 cards to 4 players

player 1
  ♥8  ♥2  ♣8  ♠3  ♠8

player 2
  ♥7  ♥5  ♠A  ♠J  ♦A

player 3
  ♦K  ♥A  ♦7  ♣J  ♣Q

player 4
  ♠K  ♠7  ♠9  ♦9  ♦2

32 cards remaining in the deck

  ♦5  ♣K  ♥9  ♦4  ♣2  ♣6  ♦6  ♥Q  ♦8  ♥6  ♣5  ♥T  ♦3
  ♠6  ♣7  ♠5  ♣T  ♣3  ♠T  ♠2  ♦Q  ♠Q  ♦T  ♥3  ♥K  ♦J
  ♥4  ♠4  ♣4  ♥J  ♣A  ♣9

```



## BASIC

{{works with|QuickBASIC|QBX 7.1}}

Most BASICs aren't object-oriented (or anything even resembling such) and can't do a deck of cards as a single cohesive unit -- but we can fake it.


```qbasic
DECLARE SUB setInitialValues (deck() AS STRING * 2)
DECLARE SUB shuffle (deck() AS STRING * 2)
DECLARE SUB showDeck (deck() AS STRING * 2)
DECLARE FUNCTION deal$ (deck() AS STRING * 2)

DATA "AS", "2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "TS", "JS", "QS", "KS"
DATA "AH", "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "TH", "JH", "QH", "KH"
DATA "AC", "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "TC", "JC", "QC", "KC"
DATA "AD", "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "TD", "JD", "QD", "KD"

RANDOMIZE TIMER

REDIM cards(51) AS STRING * 2
REDIM cards2(51) AS STRING * 2

setInitialValues cards()
setInitialValues cards2()
shuffle cards()
PRINT "Dealt: "; deal$(cards())
PRINT "Dealt: "; deal$(cards())
PRINT "Dealt: "; deal$(cards())
PRINT "Dealt: "; deal$(cards())
showDeck cards()
showDeck cards2()

FUNCTION deal$ (deck() AS STRING * 2)
    'technically dealing from the BOTTOM of the deck... whatever
    DIM c AS STRING * 2
    c = deck(UBOUND(deck))
    REDIM PRESERVE deck(LBOUND(deck) TO UBOUND(deck) - 1) AS STRING * 2
    deal$ = c
END FUNCTION

SUB setInitialValues (deck() AS STRING * 2)
    DIM L0 AS INTEGER

    RESTORE
    FOR L0 = 0 TO 51
        READ deck(L0)
    NEXT
END SUB

SUB showDeck (deck() AS STRING * 2)
    FOR L% = LBOUND(deck) TO UBOUND(deck)
        PRINT deck(L%); " ";
    NEXT
    PRINT
END SUB

SUB shuffle (deck() AS STRING * 2)
    DIM w AS INTEGER
    DIM shuffled(51) AS STRING * 2
    DIM L0 AS INTEGER

    FOR L0 = 51 TO 0 STEP -1
        w = INT(RND * (L0 + 1))
        shuffled(L0) = deck(w)
        IF w <> L0 THEN deck(w) = deck(L0)
    NEXT

    FOR L0 = 0 TO 51
        deck(L0) = shuffled(L0)
    NEXT
END SUB
```


Sample output:
 Dealt: 7D
 Dealt: 6D
 Dealt: KD
 Dealt: 8S
 5D QH JC JH KC 3S QD 4D 9H 2C JD KH 7H 4H AD 7S 3D 2H 3H 5C 4S AS TD 7C QS 9S 9D
  KS 8H 4C 6H 5H 5S 8D TC AH TS 9C 3C 8C TH 2D QC 6C AC 2S JS 6S
 AS 2S 3S 4S 5S 6S 7S 8S 9S TS JS QS KS AH 2H 3H 4H 5H 6H 7H 8H 9H TH JH QH KH AC
  2C 3C 4C 5C 6C 7C 8C 9C TC JC QC KC AD 2D 3D 4D 5D 6D 7D 8D 9D TD JD QD KD


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM Deck{ncards%, card&(51)}, Suit$(3), Rank$(12)
      Suit$() = "Clubs", "Diamonds", "Hearts", "Spades"
      Rank$() = "Ace", "Two", "Three", "Four", "Five", "Six", "Seven", \
      \         "Eight", "Nine", "Ten", "Jack", "Queen", "King"
      
      PRINT "Creating a new deck..."
      PROCnewdeck(deck1{})
      PRINT "Shuffling the deck..."
      PROCshuffle(deck1{})
      PRINT "The first few cards are:"
      FOR card% = 1 TO 8
        PRINT FNcardname(deck1.card&(card%))
      NEXT
      PRINT "Dealing three cards from the deck:"
      FOR card% = 1 TO 3
        PRINT FNcardname(FNdeal(deck1{}))
      NEXT
      PRINT "Number of cards remaining in the deck = " ; deck1.ncards%
      END
      
      REM Make a new deck:
      DEF PROCnewdeck(RETURN deck{})
      LOCAL N%
      DIM deck{} = Deck{}
      FOR N% = 0 TO 51
        deck.card&(N%) = N%
        deck.ncards% += 1
      NEXT
      ENDPROC
      
      REM Shuffle a deck:
      DEF PROCshuffle(deck{})
      LOCAL N%
      FOR N% = 52 TO 2 STEP -1
        SWAP deck.card&(N%-1), deck.card&(RND(N%)-1)
      NEXT
      ENDPROC
      
      REM Deal from the 'bottom' of the deck:
      DEF FNdeal(deck{})
      IF deck.ncards% = 0 THEN ERROR 100, "Deck is empty"
      deck.ncards% -= 1
      = deck.card&(deck.ncards%)
      
      REM Return the name of a card:
      DEF FNcardname(card&)
      = Rank$(card& >> 2) + " of " + Suit$(card& AND 3)
```

Output:

```txt
Creating a new deck...
Shuffling the deck...
The first few cards are:
King of Spades
Four of Diamonds
Six of Spades
Four of Clubs
Six of Diamonds
Jack of Clubs
Nine of Clubs
Ace of Clubs
Dealing three cards from the deck:
Three of Spades
Three of Diamonds
Four of Hearts
Number of cards remaining in the deck = 49
```



## C


```C>#include <stdio.h

#include <stdlib.h>
#include <locale.h>

int locale_ok = 0;

wchar_t s_suits[] = L"♠♥♦♣";
/* if your file can't contain unicode, use the next line instead */
//wchar_t s_suits[] = L"\x2660\x2665\x2666\x2663";

const char *s_suits_ascii[] = { "S", "H", "D", "C" };
const char *s_nums[] = { "WHAT",
	"A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K",
	"OVERFLOW"
};

typedef struct { int suit, number, _s; } card_t, *card;
typedef struct { int n; card_t cards[52]; } deck_t, *deck;

void show_card(card c)
{
	if (locale_ok)
		printf(" %lc%s", s_suits[c->suit], s_nums[c->number]);
	else
		printf(" %s%s", s_suits_ascii[c->suit], s_nums[c->number]);
}

deck new_deck()
{
	int i, j, k;
	deck d = malloc(sizeof(deck_t));
	d->n = 52;
	for (i = k = 0; i < 4; i++)
		for (j = 1; j <= 13; j++, k++) {
			d->cards[k].suit = i;
			d->cards[k].number = j;
		}
	return d;
}

void show_deck(deck d)
{
	int i;
	printf("%d cards:", d->n);
	for (i = 0; i < d->n; i++)
		show_card(d->cards + i);
	printf("\n");
}

int cmp_card(const void *a, const void *b)
{
	int x = ((card)a)->_s, y = ((card)b)->_s;
	return x < y ? -1 : x > y;
}

card deal_card(deck d)
{
	if (!d->n) return 0;
	return d->cards + --d->n;
}

void shuffle_deck(deck d)
{
	int i;
	for (i = 0; i < d->n; i++)
		d->cards[i]._s = rand();
	qsort(d->cards, d->n, sizeof(card_t), cmp_card);
}

int main()
{
	int i, j;
	deck d = new_deck();

	locale_ok = (0 != setlocale(LC_CTYPE, ""));

	printf("New deck, "); show_deck(d);

	printf("\nShuffle and deal to three players:\n");
	shuffle_deck(d);
	for (i = 0; i < 3; i++) {
		for (j = 0; j < 5; j++)
			show_card(deal_card(d));
		printf("\n");
	}
	printf("Left in deck "); show_deck(d);

	/* freeing the data struct requires just free(), but it depends on the
	 * situation: there might be cards dealt out somewhere, which is not
	 * in the scope of this task.
	 */
	//free(d);
	return 0;
}
```
output

```txt

New deck, 52 cards: ♠A ♠2 ♠3 ♠4 ♠5 ♠6 ♠7 ♠8 ♠9 ♠10 ♠J ♠Q ♠K ♥A ♥2 ...

Shuffle and deal to three players:
 ♦3 ♦Q ♥2 ♣J ♣9
 ♥3 ♠5 ♣5 ♠A ♦6
 ♣8 ♥J ♠4 ♠3 ♣A
Left in deck 37 cards: ♥8 ♣10 ♣K ♦2 ♦A ♥10 ♥6 ♥Q ♠6 ♦4 ♥9 ♠9 ♣6 ♦K ♦8 ♠7 ♣7 ♠K ♠2 ♣4 ...

```

See also [[Playing Cards/C]]


## C++


```cpp>#include <deque

#include <algorithm>
#include <ostream>
#include <iterator>

namespace cards
{
class card
{
public:
    enum pip_type { two, three, four, five, six, seven, eight, nine, ten,
                    jack, queen, king, ace, pip_count };
    enum suite_type { hearts, spades, diamonds, clubs, suite_count };
    enum { unique_count = pip_count * suite_count };

    card(suite_type s, pip_type p): value(s + suite_count * p) {}

    explicit card(unsigned char v = 0): value(v) {}

    pip_type pip() { return pip_type(value / suite_count); }

    suite_type suite() { return suite_type(value % suite_count); }

private:
    unsigned char value;
};

const char* const pip_names[] =
    { "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
      "jack", "queen", "king", "ace" };

std::ostream& operator<<(std::ostream& os, card::pip_type pip)
{
    return os << pip_names[pip];
}

const char* const suite_names[] =
    { "hearts", "spades", "diamonds", "clubs" };

std::ostream& operator<<(std::ostream& os, card::suite_type suite)
{
    return os << suite_names[suite];
}

std::ostream& operator<<(std::ostream& os, card c)
{
    return os << c.pip() << " of " << c.suite();
}

class deck
{
public:
    deck()
    {
        for (int i = 0; i < card::unique_count; ++i) {
            cards.push_back(card(i));
        }
    }

    void shuffle() { std::random_shuffle(cards.begin(), cards.end()); }

    card deal() { card c = cards.front(); cards.pop_front(); return c; }

    typedef std::deque<card>::const_iterator const_iterator;
    const_iterator begin() const { return cards.cbegin(); }
    const_iterator end() const { return cards.cend(); }
private:
    std::deque<card> cards;
};

inline std::ostream& operator<<(std::ostream& os, const deck& d)
{
    std::copy(d.begin(), d.end(), std::ostream_iterator<card>(os, "\n"));
    return os;
}
}

```


=={{header|C sharp|C#}}==


```csharp
using System;
using System.Linq;
using System.Collections.Generic;

public struct Card
{
    public Card(string rank, string suit) : this()
    {
        Rank = rank;
        Suit = suit;
    }

    public string Rank { get; }
    public string Suit { get; }

    public override string ToString() => $"{Rank} of {Suit}";
}

public class Deck : IEnumerable<Card>
{
    static readonly string[] ranks = { "Two", "Three", "Four", "Five", "Six",
        "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King", "Ace" };
    static readonly string[] suits = { "Clubs", "Diamonds", "Hearts", "Spades" };
    readonly List<Card> cards;
 
    public Deck() {
        cards = (from suit in suits
                from rank in ranks
                select new Card(rank, suit)).ToList();
    }

    public int Count => cards.Count;

    public void Shuffle() {
        // using Knuth Shuffle (see at http://rosettacode.org/wiki/Knuth_shuffle)
        var random = new Random();
        for (int i = 0; i < cards.Count; i++) {
            int r = random.Next(i, cards.Count);
            var temp = cards[i];
            cards[i] = cards[r];
            cards[r] = temp;
        }
    }

    public Card Deal() {
        int last = cards.Count - 1;
        Card card = cards[last];
        //Removing from the front will shift the other items back 1 spot,
        //so that would be an O(n) operation. Removing from the back is O(1).
        cards.RemoveAt(last);
        return card;
    }

    public IEnumerator<Card> GetEnumerator() {
        //Reverse enumeration of the list so that they are returned in the order they would be dealt.
        //LINQ's Reverse() copies the entire list. Let's avoid that.
        for (int i = cards.Count - 1; i >= 0; i--)
            yield return cards[i];
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();
}
```


=={{Header|Ceylon}}==

```ceylon
import com.vasileff.ceylon.random.api { ... }

"""Run the example code for Rosetta Code ["Playing cards" task] (http://rosettacode.org/wiki/Playing_cards)."""
shared void run() {
    variable value deck = Deck();
    print("New deck (``deck.size`` cards): ``deck``
           ");
    
    deck = deck.shuffle();
    print("Shuffeled deck (``deck.size`` cards): ``deck``
           ");
    
    print("Deal three hands: ");
    for (i in 1..3) {
        value [hand, _deck] = deck.deal();
        print("- Dealt ``hand.size`` cards to hand ``i`` : ``join(hand)``");
        deck = _deck;
    }
    
    print("
           Deck (``deck.size`` cards) after dealing three hands: ``deck``");
    
}

abstract class Suit() of clubs | hearts | spades | diamonds {}

object clubs    extends Suit() { string = "♣"; }
object hearts   extends Suit() { string = "♥"; }
object spades   extends Suit() { string = "♠"; }
object diamonds extends Suit() { string = "♦"; }

abstract class Pip() of two | three | four | five | six | seven | eight | nine | ten | jack | queen | king | ace {}
object two   extends Pip() { string =  "2"; }
object three extends Pip() { string =  "3"; }
object four  extends Pip() { string =  "4"; }
object five  extends Pip() { string =  "5"; }
object six   extends Pip() { string =  "6"; }
object seven extends Pip() { string =  "7"; }
object eight extends Pip() { string =  "8"; }
object nine  extends Pip() { string =  "9"; }
object ten   extends Pip() { string = "10"; }
object jack  extends Pip() { string =  "J"; }
object queen extends Pip() { string =  "Q"; }
object king  extends Pip() { string =  "K"; }
object ace   extends Pip() { string =  "A"; }

class Card(shared Pip pip, shared Suit suit) { 
    string = "``pip`` of ``suit``";
}


String join(Card[] cards) => ", ".join { *cards };

class Deck (cards = [ for (suit in `Suit`.caseValues) for (pip in `Pip`.caseValues) Card(pip, suit) ]) {
    shared Card[] cards;
    
    shared Deck shuffle(Random rnd = platformRandom()) 
            => if (nonempty cards)
               then Deck( [*randomize(cards, rnd)] )
               else this;
    
    shared Integer size => cards.size;
    
    shared Boolean empty => cards.empty;
    
    string => if (size > 13) 
              then "\n  " + "\n  ". join { *cards.partition(13).map((cards) => join(cards)) }
              else join(cards);
    
    shared [Card[], Deck] deal(Integer handSize = 5) {
        if (handSize >= cards.size) {
            return [cards, Deck([])];
        }
        else {
            return [
                cards.initial(handSize),
                Deck(cards.skip(handSize).sequence())
            ];
        }
    }
}
```


Output:

```txt
New deck (52 cards): 
  2 of ♣, 3 of ♣, 4 of ♣, 5 of ♣, 6 of ♣, 7 of ♣, 8 of ♣, 9 of ♣, 10 of ♣, J of ♣, Q of ♣, K of ♣, A of ♣
  2 of ♥, 3 of ♥, 4 of ♥, 5 of ♥, 6 of ♥, 7 of ♥, 8 of ♥, 9 of ♥, 10 of ♥, J of ♥, Q of ♥, K of ♥, A of ♥
  2 of ♠, 3 of ♠, 4 of ♠, 5 of ♠, 6 of ♠, 7 of ♠, 8 of ♠, 9 of ♠, 10 of ♠, J of ♠, Q of ♠, K of ♠, A of ♠
  2 of ♦, 3 of ♦, 4 of ♦, 5 of ♦, 6 of ♦, 7 of ♦, 8 of ♦, 9 of ♦, 10 of ♦, J of ♦, Q of ♦, K of ♦, A of ♦

Shuffeled deck (52 cards): 
  4 of ♠, 2 of ♦, 5 of ♣, 9 of ♠, 4 of ♥, 7 of ♥, 10 of ♦, 5 of ♠, 3 of ♥, K of ♥, 6 of ♣, 9 of ♦, 6 of ♦
  4 of ♣, 8 of ♣, 4 of ♦, Q of ♥, 6 of ♥, J of ♥, 8 of ♦, 5 of ♥, 5 of ♦, J of ♣, A of ♥, J of ♦, 2 of ♣
  7 of ♠, Q of ♦, A of ♣, Q of ♣, 6 of ♠, Q of ♠, K of ♠, 7 of ♦, 7 of ♣, 3 of ♦, 2 of ♠, 8 of ♥, A of ♦
  2 of ♥, 9 of ♣, 8 of ♠, 10 of ♥, 3 of ♠, 10 of ♣, 9 of ♥, 10 of ♠, 3 of ♣, J of ♠, K of ♣, K of ♦, A of ♠

Deal three hands: 
- Dealt 5 cards to hand 1 : 4 of ♠, 2 of ♦, 5 of ♣, 9 of ♠, 4 of ♥
- Dealt 5 cards to hand 2 : 7 of ♥, 10 of ♦, 5 of ♠, 3 of ♥, K of ♥
- Dealt 5 cards to hand 3 : 6 of ♣, 9 of ♦, 6 of ♦, 4 of ♣, 8 of ♣

Deck (37 cards) after dealing three hands: 
  4 of ♦, Q of ♥, 6 of ♥, J of ♥, 8 of ♦, 5 of ♥, 5 of ♦, J of ♣, A of ♥, J of ♦, 2 of ♣, 7 of ♠, Q of ♦
  A of ♣, Q of ♣, 6 of ♠, Q of ♠, K of ♠, 7 of ♦, 7 of ♣, 3 of ♦, 2 of ♠, 8 of ♥, A of ♦, 2 of ♥, 9 of ♣
  8 of ♠, 10 of ♥, 3 of ♠, 10 of ♣, 9 of ♥, 10 of ♠, 3 of ♣, J of ♠, K of ♣, K of ♦, A of ♠
```


=={{Header|Clojure}}==

```Clojure
(def suits [:club :diamond :heart :spade])
(def pips [:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king])

(defn deck [] (for [s suits p pips] [s p]))

(def shuffle clojure.core/shuffle)
(def deal first)
(defn output [deck]
  (doseq [[suit pip] deck]
    (println (format "%s of %ss"
                     (if (keyword? pip) (name pip) pip)
                     (name suit)))))
```



## COBOL

{{works with|GnuCOBOL}}

```COBOL
       identification division.
       program-id. playing-cards.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       77 card             usage index.
       01 deck.
          05 cards occurs 52 times ascending key slot indexed by card.
             10 slot       pic 99.
             10 hand       pic 99.
             10 suit       pic 9.
             10 symbol     pic x(4).
             10 rank       pic 99.

       01 filler.
          05 suit-name     pic x(8) occurs 4 times.

      *> Unicode U+1F0Ax, Bx, Cx, Dx "f09f82a0" "82b0" "8380" "8390"
       01 base-s           constant as 4036985504.
       01 base-h           constant as 4036985520.
       01 base-d           constant as 4036985728.
       01 base-c           constant as 4036985744.

       01 sym              pic x(4) comp-x.
       01 symx             redefines sym pic x(4).
       77 s                pic 9.
       77 r                pic 99.
       77 c                pic 99.
       77 hit              pic 9.
       77 limiter          pic 9(6).

       01 spades           constant as 1.
       01 hearts           constant as 2.
       01 diamonds         constant as 3.
       01 clubs            constant as 4.

       01 players          constant as 3.
       01 cards-per        constant as 5.
       01 deal             pic 99.
       01 player           pic 99.

       01 show-tally       pic zz.
       01 show-rank        pic z(5).
       01 arg              pic 9(10).
       
       procedure division.
       cards-main.
       perform seed
       perform initialize-deck
       perform shuffle-deck
       perform deal-deck
       perform display-hands
       goback.
      
      *> ********
       seed.
           accept arg from command-line
           if arg not equal 0 then
               move random(arg) to c
           end-if
       .

       initialize-deck.
           move "spades" to suit-name(spades)
           move "hearts" to suit-name(hearts)
           move "diamonds" to suit-name(diamonds)
           move "clubs" to suit-name(clubs)

           perform varying s from 1 by 1 until s > 4
                     after r from 1 by 1 until r > 13
                   compute c = (s - 1) * 13 + r
                   evaluate s
                       when spades compute sym = base-s + r
                       when hearts compute sym = base-h + r
                       when diamonds compute sym = base-d + r
                       when clubs compute sym = base-c + r
                   end-evaluate
                   if r > 11 then compute sym = sym + 1 end-if
                   move s to suit(c)
                   move r to rank(c)
                   move symx to symbol(c)
                   move zero to slot(c)
                   move zero to hand(c)
           end-perform
       .

       shuffle-deck.
           move zero to limiter
           perform until exit
               compute c = random() * 52.0 + 1.0
               move zero to hit
               perform varying tally from 1 by 1 until tally > 52
                   if slot(tally) equal c then
                       move 1 to hit
                       exit perform
                   end-if
                   if slot(tally) equal 0 then
                       if tally < 52 then move 1 to hit end-if
                       move c to slot(tally)
                       exit perform
                   end-if
               end-perform
               if hit equal zero then exit perform end-if
               if limiter > 999999 then
                   display "too many shuffles, deck invalid" upon syserr
                   exit perform
               end-if
               add 1 to limiter
           end-perform
           sort cards ascending key slot
       .

       display-card.
       >>IF ENGLISH IS DEFINED
               move rank(tally) to show-rank
               evaluate rank(tally) 
                   when 1 display "  ace" with no advancing
                   when 2 thru 10 display show-rank with no advancing
                   when 11 display " jack" with no advancing
                   when 12 display "queen" with no advancing
                   when 13 display " king" with no advancing
               end-evaluate
               display " of " suit-name(suit(tally)) with no advancing
       >>ELSE
               display symbol(tally) with no advancing
       >>END-IF
       .

       display-deck.
           perform varying tally from 1 by 1 until tally > 52
               move tally to show-tally
               display "Card: " show-tally
                       " currently in hand " hand(tally)
                       " is " with no advancing
               perform display-card
               display space
           end-perform
       .

       display-hands.
           perform varying player from 1 by 1 until player > players
               move player to tally
               display "Player " player ": " with no advancing
               perform varying deal from 1 by 1 until deal > cards-per
                  perform display-card
                  add players to tally
               end-perform
               display space
           end-perform
           display "Stock: " with no advancing
           subtract players from tally
           add 1 to tally
           perform varying tally from tally by 1 until tally > 52
               perform display-card
       >>IF ENGLISH IS DEFINED
               display space
       >>END-IF
           end-perform
           display space
       .

       deal-deck.
           display "Dealing " cards-per " cards to " players " players"
           move 1 to tally
           perform varying deal from 1 by 1 until deal > cards-per
                     after player from 1 by 1 until player > players
               move player to hand(tally)
               add 1 to tally
           end-perform
       .

       end program playing-cards.

```


{{out}}

```txt
prompt$ cobc -xjd playing-cards.cob
Dealing 5 cards to 3 players
Player 01: 🂷🃇🂺🃉🂸
Player 02: 🃋🃈🂵🂦🃑
Player 03: 🃓🂻🂹🂾🂩
Stock: 🃁🃞🂧🂮🃒🂢🃚🃔🃍🂫🃃🂱🃂🂪🃛🃙🂶🂭🂳🃊🃗🃝🂴🃖🂨🂣🂤🃆🂡🃎🃘🃅🂥🃕🂲🃄🂽

```

Or in English

```txt
prompt$ cobc -xjd playing-cards.cob -DENGLISH
Dealing 5 cards to 3 players
Player 01:     7 of hearts      7 of diamonds   10 of hearts      9 of diamonds    8 of hearts
Player 02:  jack of diamonds    8 of diamonds    5 of hearts      6 of spades    ace of clubs
Player 03:     3 of clubs    jack of hearts      9 of hearts   king of hearts      9 of spades
Stock:   ace of diamonds
 king of clubs
    7 of spades
 king of spades
    2 of clubs
    2 of spades
   10 of clubs
    4 of clubs
queen of diamonds
 jack of spades
    3 of diamonds
  ace of hearts
    2 of diamonds
   10 of spades
 jack of clubs
    9 of clubs
    6 of hearts
queen of spades
    3 of hearts
   10 of diamonds
    7 of clubs
queen of clubs
    4 of hearts
    6 of clubs
    8 of spades
    3 of spades
    4 of spades
    6 of diamonds
  ace of spades
 king of diamonds
    8 of clubs
    5 of diamonds
    5 of spades
    5 of clubs
    2 of hearts
    4 of diamonds
queen of hearts
```


GnuCOBOL pseudo-random number sequences are reproducible.  A command line argument can be used to pass an initial seed value. 

=={{Header|CoffeeScript}}==


```coffeescript
#translated from JavaScript example
class Card
  constructor: (@pip, @suit) ->
 
  toString: => "#{@pip}#{@suit}"

class Deck
  pips = '2 3 4 5 6 7 8 9 10 J Q K A'.split ' '
  suits = '♣ ♥ ♠ ♦'.split ' '

  constructor: (@cards) ->
    if not @cards?
      @cards = []
      for suit in suits
        for pip in pips
          @cards.push new Card(pip, suit)
 
  toString: => "[#{@cards.join(', ')}]"
 
  shuffle: =>
    for card, i in @cards
      randomCard = parseInt @cards.length * Math.random()
      @cards[i] = @cards.splice(randomCard, 1, card)[0]
 
  deal: -> @cards.shift()
```



## Common Lisp


A card is a cons of a suit and a pip.  A deck is a list of cards, and so dealing is simply popping off of a deck.  Shuffling is naïve, just a sort with a random predicate.  Printing is built in.


```lisp
(defconstant +suits+
  '(club diamond heart spade)
  "Card suits are the symbols club, diamond, heart, and spade.")

(defconstant +pips+
  '(ace 2 3 4 5 6 7 8 9 10 jack queen king)
  "Card pips are the numbers 2 through 10, and the symbols ace, jack,
queen and king.")

(defun make-deck (&aux (deck '()))
  "Returns a list of cards, where each card is a cons whose car is a
suit and whose cdr is a pip."
  (dolist (suit +suits+ deck)
    (dolist (pip +pips+)
      (push (cons suit pip) deck))))

(defun shuffle (list)
  "Returns a shuffling of list, by sorting it with a random
predicate. List may be modified."
  (sort list #'(lambda (x y)
                 (declare (ignore x y))
                 (zerop (random 2)))))
```



## D


```d
import std.stdio, std.typecons, std.algorithm, std.traits, std.array,
       std.range, std.random;

enum Pip {Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
          Jack, Queen, King, Ace}
enum Suit {Diamonds, Spades, Hearts, Clubs}
alias Card = Tuple!(Pip, Suit);

auto newDeck() pure nothrow @safe {
    return cartesianProduct([EnumMembers!Pip], [EnumMembers!Suit]);
}

alias shuffleDeck = randomShuffle;

Card dealCard(ref Card[] deck) pure nothrow @safe @nogc {
    immutable card = deck.back;
    deck.popBack;
    return card;
}

void show(in Card[] deck) @safe {
    writefln("Deck:\n%(%s\n%)\n", deck);
}

void main() /*@safe*/ {
    auto d = newDeck.array;
    d.show;
    d.shuffleDeck;
    while (!d.empty)
        d.dealCard.writeln;
}
```

{{out}}

```txt
Deck:
const(Tuple!(Pip, Suit))(Two, Diamonds)
const(Tuple!(Pip, Suit))(Three, Diamonds)
const(Tuple!(Pip, Suit))(Four, Diamonds)
const(Tuple!(Pip, Suit))(Five, Diamonds)
const(Tuple!(Pip, Suit))(Six, Diamonds)
const(Tuple!(Pip, Suit))(Seven, Diamonds)
const(Tuple!(Pip, Suit))(Eight, Diamonds)
const(Tuple!(Pip, Suit))(Nine, Diamonds)
const(Tuple!(Pip, Suit))(Ten, Diamonds)
const(Tuple!(Pip, Suit))(Jack, Diamonds)
const(Tuple!(Pip, Suit))(Queen, Diamonds)
const(Tuple!(Pip, Suit))(King, Diamonds)
const(Tuple!(Pip, Suit))(Ace, Diamonds)
const(Tuple!(Pip, Suit))(Two, Spades)
const(Tuple!(Pip, Suit))(Three, Spades)
const(Tuple!(Pip, Suit))(Four, Spades)
const(Tuple!(Pip, Suit))(Five, Spades)
const(Tuple!(Pip, Suit))(Six, Spades)
const(Tuple!(Pip, Suit))(Seven, Spades)
const(Tuple!(Pip, Suit))(Eight, Spades)
const(Tuple!(Pip, Suit))(Nine, Spades)
const(Tuple!(Pip, Suit))(Ten, Spades)
const(Tuple!(Pip, Suit))(Jack, Spades)
const(Tuple!(Pip, Suit))(Queen, Spades)
const(Tuple!(Pip, Suit))(King, Spades)
const(Tuple!(Pip, Suit))(Ace, Spades)
const(Tuple!(Pip, Suit))(Two, Hearts)
const(Tuple!(Pip, Suit))(Three, Hearts)
const(Tuple!(Pip, Suit))(Four, Hearts)
const(Tuple!(Pip, Suit))(Five, Hearts)
const(Tuple!(Pip, Suit))(Six, Hearts)
const(Tuple!(Pip, Suit))(Seven, Hearts)
const(Tuple!(Pip, Suit))(Eight, Hearts)
const(Tuple!(Pip, Suit))(Nine, Hearts)
const(Tuple!(Pip, Suit))(Ten, Hearts)
const(Tuple!(Pip, Suit))(Jack, Hearts)
const(Tuple!(Pip, Suit))(Queen, Hearts)
const(Tuple!(Pip, Suit))(King, Hearts)
const(Tuple!(Pip, Suit))(Ace, Hearts)
const(Tuple!(Pip, Suit))(Two, Clubs)
const(Tuple!(Pip, Suit))(Three, Clubs)
const(Tuple!(Pip, Suit))(Four, Clubs)
const(Tuple!(Pip, Suit))(Five, Clubs)
const(Tuple!(Pip, Suit))(Six, Clubs)
const(Tuple!(Pip, Suit))(Seven, Clubs)
const(Tuple!(Pip, Suit))(Eight, Clubs)
const(Tuple!(Pip, Suit))(Nine, Clubs)
const(Tuple!(Pip, Suit))(Ten, Clubs)
const(Tuple!(Pip, Suit))(Jack, Clubs)
const(Tuple!(Pip, Suit))(Queen, Clubs)
const(Tuple!(Pip, Suit))(King, Clubs)
const(Tuple!(Pip, Suit))(Ace, Clubs)

Tuple!(Pip, Suit)(Eight, Diamonds)
Tuple!(Pip, Suit)(Nine, Hearts)
Tuple!(Pip, Suit)(Four, Hearts)
Tuple!(Pip, Suit)(King, Hearts)
Tuple!(Pip, Suit)(Two, Hearts)
Tuple!(Pip, Suit)(King, Clubs)
Tuple!(Pip, Suit)(Two, Spades)
Tuple!(Pip, Suit)(Seven, Clubs)
Tuple!(Pip, Suit)(Ten, Spades)
Tuple!(Pip, Suit)(King, Spades)
Tuple!(Pip, Suit)(Four, Spades)
Tuple!(Pip, Suit)(Ten, Clubs)
Tuple!(Pip, Suit)(Ace, Diamonds)
Tuple!(Pip, Suit)(Six, Clubs)
Tuple!(Pip, Suit)(Six, Spades)
Tuple!(Pip, Suit)(Ten, Hearts)
Tuple!(Pip, Suit)(Seven, Diamonds)
Tuple!(Pip, Suit)(Queen, Clubs)
Tuple!(Pip, Suit)(Seven, Spades)
Tuple!(Pip, Suit)(Queen, Spades)
Tuple!(Pip, Suit)(Ace, Spades)
Tuple!(Pip, Suit)(Five, Spades)
Tuple!(Pip, Suit)(Ace, Hearts)
Tuple!(Pip, Suit)(Nine, Spades)
Tuple!(Pip, Suit)(Jack, Clubs)
Tuple!(Pip, Suit)(Three, Spades)
Tuple!(Pip, Suit)(Jack, Diamonds)
Tuple!(Pip, Suit)(Queen, Diamonds)
Tuple!(Pip, Suit)(Three, Hearts)
Tuple!(Pip, Suit)(Five, Clubs)
Tuple!(Pip, Suit)(Three, Clubs)
Tuple!(Pip, Suit)(Two, Diamonds)
Tuple!(Pip, Suit)(Jack, Hearts)
Tuple!(Pip, Suit)(King, Diamonds)
Tuple!(Pip, Suit)(Eight, Spades)
Tuple!(Pip, Suit)(Eight, Hearts)
Tuple!(Pip, Suit)(Five, Diamonds)
Tuple!(Pip, Suit)(Four, Clubs)
Tuple!(Pip, Suit)(Ten, Diamonds)
Tuple!(Pip, Suit)(Eight, Clubs)
Tuple!(Pip, Suit)(Queen, Hearts)
Tuple!(Pip, Suit)(Four, Diamonds)
Tuple!(Pip, Suit)(Six, Diamonds)
Tuple!(Pip, Suit)(Jack, Spades)
Tuple!(Pip, Suit)(Ace, Clubs)
Tuple!(Pip, Suit)(Nine, Clubs)
Tuple!(Pip, Suit)(Seven, Hearts)
Tuple!(Pip, Suit)(Three, Diamonds)
Tuple!(Pip, Suit)(Nine, Diamonds)
Tuple!(Pip, Suit)(Five, Hearts)
Tuple!(Pip, Suit)(Six, Hearts)
Tuple!(Pip, Suit)(Two, Clubs)
```



### More Refined Version


```d
import std.stdio, std.random, std.algorithm, std.string, std.range;

struct Card {
    static immutable suits = "Club Heart Diamond Spade".split;
    static immutable pips  = "Ace 2 3 4 5 6 7 8 9 10 J Q K".split;
    enum nPack = suits.length * pips.length;

    static bool rankAceTop = true;
    int pip, suit;

    string toString() pure const {
        return format("%3s of %-7s", pips[pip], suits[suit])
               .rightJustify(15);
    }

    @property int order() const nothrow {
        immutable pipOrder = (!rankAceTop) ?
                             pip :
                             (pip ? pip - 1 : 12);
        return pipOrder * suits.length + suit;
    }

    bool opEqual(in Card rhs) const pure nothrow {
        return pip == rhs.pip && suit == rhs.suit;
    }

    int opCmp(in Card rhs) const nothrow {
        return order - rhs.order;
    }
}

final class Deck {
    private Card[] cards;

    this(in bool initShuffle = true, in int pack = 0) {
        cards.length = 0;
        foreach (immutable p; 0 .. pack)
            foreach (immutable c; 0 .. Card.nPack)
                cards ~= Card((c / Card.suits.length) %
                              Card.pips.length,
                              c % Card.suits.length);

        if (initShuffle)
            cards.randomShuffle;
    }

    @property size_t length() const pure nothrow {
        return cards.length;
    }

    Deck add(in Card c) pure nothrow {
        cards ~= c;
        return this;
    }

    Deck deal(in int loc, Deck toDeck = null) pure nothrow {
        if (toDeck !is null)
            toDeck.add(cards[loc]);
        cards = cards[0 .. loc] ~ cards[loc + 1 .. $];
        return this;
    }

    Deck dealTop(Deck toDeck = null) pure nothrow {
        return deal(length - 1, toDeck);
    }

    Card opIndex(in int loc) const pure nothrow {
        return cards[loc];
    }

    alias opIndex peek;

    Deck showDeck() {
        this.writeln;
        return this;
    }

    Deck shuffle() {
        cards.randomShuffle;
        return this;
    }

    Deck sortDeck() {
        cards.sort!q{a > b};
        return this;
    }

    override string toString() pure const {
        return format("%(%(%s%)\n%)", cards.chunks(4));
    }
}

void main() {
    Deck[4] guests;
    foreach (ref g; guests)
        g = new Deck; // Empty deck.

    auto host = new Deck(false, 1);
    writeln("Host");
    host.shuffle.showDeck;

    while (host.length > 0)
        foreach (ref g; guests)
            if (host.length > 0)
                host.dealTop(g);

    foreach (immutable i, g; guests) {
        writefln("Player #%d", i + 1);
        g.sortDeck.showDeck;
    }
}
```

{{out}}

```txt
Host
   6 of Heart     Q of Club    Ace of Club      9 of Spade  
   8 of Club      K of Diamond   Q of Heart     6 of Spade  
   7 of Diamond   5 of Heart     6 of Diamond   8 of Heart  
   4 of Club      J of Club     10 of Diamond   2 of Club   
   9 of Diamond   K of Club      4 of Spade     8 of Spade  
   5 of Club      3 of Club      9 of Club      3 of Diamond
   2 of Diamond   9 of Heart     7 of Heart    10 of Club   
 Ace of Heart     2 of Spade     J of Spade     5 of Diamond
   2 of Heart     4 of Heart   Ace of Diamond   J of Heart  
   3 of Spade     K of Spade     7 of Spade     5 of Spade  
 Ace of Spade    10 of Heart     K of Heart     3 of Heart  
   6 of Club      Q of Spade     J of Diamond   8 of Diamond
  10 of Spade     7 of Club      Q of Diamond   4 of Diamond
Player #1
   J of Heart    10 of Club      9 of Spade     8 of Spade  
   8 of Diamond   8 of Heart     6 of Spade     5 of Spade  
   5 of Diamond   4 of Diamond   3 of Diamond   3 of Heart  
   2 of Club   
Player #2
 Ace of Diamond Ace of Club      K of Heart     Q of Diamond
   Q of Heart     J of Spade     J of Diamond  10 of Diamond
   9 of Club      7 of Spade     7 of Heart     6 of Diamond
   4 of Spade  
Player #3
   K of Spade     K of Diamond   K of Club      Q of Spade  
   Q of Club      J of Club     10 of Heart     9 of Heart  
   7 of Club      5 of Heart     4 of Heart     3 of Club   
   2 of Spade  
Player #4
 Ace of Spade   Ace of Heart    10 of Spade     9 of Diamond
   8 of Club      7 of Diamond   6 of Heart     6 of Club   
   5 of Club      4 of Club      3 of Spade     2 of Diamond
   2 of Heart  
```



## Delphi


```delphi
program Cards;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

type
  TPip = (pTwo, pThree, pFour, pFive, pSix, pSeven, pEight, pNine, pTen, pJack, pQueen, pKing, pAce);
  TSuite = (sDiamonds, sSpades, sHearts, sClubs);

const
  cPipNames: array[TPip] of string = ('2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A');
  cSuiteNames: array[TSuite] of string = ('Diamonds', 'Spades', 'Hearts', 'Clubs');

type
  TCard = class
  private
    FSuite: TSuite;
    FPip: TPip;
  public
    constructor Create(aSuite: TSuite; aPip: TPip);
    function ToString: string; override;

    property Pip: TPip read FPip;
    property Suite: TSuite read FSuite;
  end;

  TDeck = class
  private
    FCards: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Shuffle;
    function Deal: TCard;
    function ToString: string; override;
  end;

{ TCard }

constructor TCard.Create(aSuite: TSuite; aPip: TPip);
begin
  FSuite := aSuite;
  FPip := aPip;
end;

function TCard.ToString: string;
begin
  Result := Format('%s of %s', [cPipNames[Pip], cSuiteNames[Suite]])
end;

{ TDeck }

constructor TDeck.Create;
var
  pip: TPip;
  suite: TSuite;
begin
  FCards := TList.Create;
  for suite := Low(TSuite) to High(TSuite) do
    for pip := Low(TPip) to High(TPip) do
      FCards.Add(TCard.Create(suite, pip));
end;

function TDeck.Deal: TCard;
begin
  Result := FCards[0];
  FCards.Delete(0);
end;

destructor TDeck.Destroy;
var
  i: Integer;
  c: TCard;
begin
  for i := FCards.Count - 1 downto 0 do begin
    c := FCards[i];
    FCards.Delete(i);
    c.Free;
  end;
  FCards.Free;
  inherited;
end;

procedure TDeck.Shuffle;
var
  i, j: Integer;
  temp: TCard;
begin
  Randomize;
  for i := FCards.Count - 1 downto 0 do begin
    j := Random(FCards.Count);
    temp := FCards[j];
    FCards.Delete(j);
    FCards.Add(temp);
  end;
end;

function TDeck.ToString: string;
var
  i: Integer;
begin
  for i := 0 to FCards.Count - 1 do
    Writeln(TCard(FCards[i]).ToString);
end;

begin
  with TDeck.Create do
  try
    Shuffle;
    ToString;
    Writeln;
    with Deal do
    try
      Writeln(ToString);
    finally
      Free;
    end;
  finally
    Free;
  end;

  Readln;
end.
```



## E

See [[Playing Cards/E]]


## Elixir

{{trans|Erlang}}

```elixir
defmodule Card do
  defstruct pip: nil, suit: nil
end

defmodule Playing_cards do
  @pips   ~w[2 3 4 5 6 7 8 9 10 Jack Queen King Ace]a
  @suits  ~w[Clubs Hearts Spades Diamonds]a
  @pip_value   Enum.with_index(@pips)
  @suit_value  Enum.with_index(@suits)
  
  def deal( n_cards, deck ), do: Enum.split( deck, n_cards )
  
  def deal( n_hands, n_cards, deck ) do
    Enum.reduce(1..n_hands, {[], deck}, fn _,{acc,d} ->
      {hand, new_d} = deal(n_cards, d)
      {[hand | acc], new_d}
    end)
  end
  
  def deck, do: (for x <- @suits, y <- @pips, do: %Card{suit: x, pip: y})
  
  def print( cards ), do: IO.puts (for x <- cards, do: "\t#{inspect x}")
  
  def shuffle( deck ), do: Enum.shuffle( deck )
  
  def sort_pips( cards ), do: Enum.sort_by( cards, &@pip_value[&1.pip] )
  
  def sort_suits( cards ), do: Enum.sort_by( cards, &(@suit_value[&1.suit]) )
  
  def task do
    shuffled = shuffle( deck )
    {hand, new_deck} = deal( 3, shuffled )
    {hands, _deck} = deal( 2, 3, new_deck )
    IO.write "Hand:"
    print( hand )
    IO.puts "Hands:"
    for x <- hands, do: print(x)
  end
end

Playing_cards.task
```


{{out}}

```txt

Hand:	%Card{pip: :Ace, suit: :Spades}	%Card{pip: :"5", suit: :Hearts}	%Card{pip: :"7", suit: :Clubs}
Hands:
	%Card{pip: :"7", suit: :Hearts}	%Card{pip: :"4", suit: :Spades}	%Card{pip: :"9", suit: :Spades}
	%Card{pip: :"6", suit: :Spades}	%Card{pip: :Ace, suit: :Hearts}	%Card{pip: :"2", suit: :Diamonds}

```



## Erlang

The -spec() is a type declaration. It is optional, but helps humans to understand complex data types and the Erlang type verifier (dialyzer) to check the code.

This module is used by [[Go_Fish]].

```Erlang

-module( playing_cards ).

-export( [deal/2, deal/3, deck/0, print/1, shuffle/1, sort_pips/1, sort_suites/1, task/0] ).

-record( card, {pip, suite} ).

-spec( deal( N_cards::integer(), Deck::[#card{}]) -> {Hand::[#card{}], Deck::[#card{}]} ).
deal( N_cards, Deck ) -> lists:split( N_cards, Deck ).
-spec( deal( N_hands::integer(), N_cards::integer(), Deck::[#card{}]) -> {List_of_hands::[[#card{}]], Deck::[#card{}]} ).
deal( N_hands, N_cards, Deck ) -> lists:foldl( fun deal_hands/2, {lists:duplicate(N_hands, []), Deck}, lists:seq(1, N_cards * N_hands) ).

deck() -> [#card{suite=X, pip=Y} || X <- suites(), Y <- pips()].

print( Cards ) -> [io:fwrite( "	~p", [X]) || X <- Cards], io:nl().

shuffle( Deck ) -> knuth_shuffle:list( Deck ).

sort_pips( Cards ) -> lists:keysort( #card.pip, Cards ).

sort_suites( Cards ) -> lists:keysort( #card.suite, Cards ).

task() ->
    Deck = deck(),
    Shuffled = shuffle( Deck ),
    {Hand, New_deck} = deal( 3, Shuffled ),
    {Hands, _Deck} = deal( 2, 3, New_deck ),
    io:fwrite( "Hand:" ),
    print( Hand ),
    io:fwrite( "Hands:~n" ),
    [print(X) || X <- Hands].



deal_hands( _N, {[Hand | T], [Card | Deck]} ) -> {T ++ [[Card | Hand]], Deck}.

pips() -> ["2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"].

suites() -> ["Clubs", "Hearts", "Spades", "Diamonds"].

```

{{out}}

```txt

22> playing_cards:task().
Hand: {card,"9","Diamonds"} {card,"7","Clubs"} {card,"King","Clubs"}
Hands:
 {card,"6","Hearts"} {card,"3","Diamonds"} {card,"Queen","Spades"}
 {card,"10","Hearts"} {card,"2","Hearts"} {card,"6","Diamonds"}

```



## Factor

{{works with|Factor|0.98}}
Since our data structure is both a sequence and a stack, we can leverage the existing <code>randomize</code> word for shuffling and <code>pop</code> word for dealing.


```factor
USING: formatting grouping io kernel math qw random sequences
vectors ;
IN: rosetta-code.playing-cards

CONSTANT: pips  qw{ A 2 3 4 5 6 7 8 9 10 J Q K }
CONSTANT: suits qw{ ♥ ♣ ♦ ♠ }

: <deck> ( -- vec ) 52 <iota> >vector ;

: card>str ( n -- str )
    13 /mod [ suits nth ] [ pips nth ] bi* prepend ;
    
: print-deck ( seq -- )
    13 group [ [ card>str "%-4s" printf ] each nl ] each ;
    
<deck>       ! make new deck
randomize    ! shuffle the deck
dup pop drop ! deal from the deck (and discard)
print-deck   ! print the deck
```

{{out}}

```txt

J♣  10♥ 3♥  10♠ 7♥  7♦  4♦  4♥  K♣  9♠  8♠  A♠  6♥  
K♠  10♣ 8♦  10♦ 3♦  Q♥  9♥  5♦  7♣  6♦  3♣  4♣  J♥  
5♣  6♣  7♠  J♠  3♠  2♣  2♥  5♠  Q♠  6♠  4♠  2♠  5♥  
K♥  2♦  8♣  A♥  9♦  9♣  8♥  J♦  K♦  A♣  Q♦  A♦  

```



## Fantom


```fantom

enum class Suit { clubs, diamonds, hearts, spades }
enum class Pips { ace, two, three, four, five, 
  six, seven, eight, nine, ten, jack, queen, king
}

class Card
{
  readonly Suit suit
  readonly Pips pips

  new make (Suit suit, Pips pips)
  {
    this.suit = suit
    this.pips = pips
  }

  override Str toStr () 
  {
    "card: $pips of $suit"
  }
}

class Deck
{
  Card[] deck := [,]

  new make ()
  {
    Suit.vals.each |val|
    {
      Pips.vals.each |pip| { deck.add (Card(val, pip)) }
    }
  }

  Void shuffle (Int swaps := 50)
  {
    swaps.times { deck.swap(Int.random(0..deck.size-1), Int.random(0..deck.size-1)) }
  }

  Card[] deal (Int cards := 1)
  {
    if (cards > deck.size) throw ArgErr("$cards is larger than ${deck.size}")
    Card[] result := [,]
    cards.times { result.push (deck.removeAt (0)) }
    return result
  }

  Void show ()
  {
    deck.each |card| { echo (card.toStr) }
  }
}

class PlayingCards
{
  public static Void main ()
  {
    deck := Deck()
    deck.shuffle
    Card[] hand := deck.deal (7)
    echo ("Dealt hand is:")
    hand.each |card| { echo (card.toStr) }
    echo ("Remaining deck is:")
    deck.show
  }
}

```



## Forth

{{works with|GNU Forth}}

```forth
require random.fs   \ RANDOM ( n -- 0..n-1 ) is called CHOOSE in other Forths

create pips  s" A23456789TJQK" mem,
create suits s" DHCS"          mem, \ diamonds, hearts, clubs, spades
: .card ( c -- )
  13 /mod swap
  pips  + c@ emit
  suits + c@ emit ;

create deck 52 allot
variable dealt

: new-deck
  52 0        do i deck i + c!             loop  0 dealt ! ;
: .deck
  52 dealt @ ?do   deck i + c@ .card space loop  cr ;
: shuffle
  51 0 do
    52 i - random i + ( rand-index ) deck +
    deck i + c@  over c@
    deck i + c!  swap c!
  loop ;
: cards-left ( -- n ) 52 dealt @ - ;
: deal-card ( -- c )
  cards-left 0= abort" Deck empty!"
  deck dealt @ + c@  1 dealt +! ;
: .hand ( n -- )
  0 do deal-card .card space loop cr ;

new-deck shuffle .deck
5 .hand
cards-left .  \ 47
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
MODULE Cards

IMPLICIT NONE

  TYPE Card
    CHARACTER(5) :: value
    CHARACTER(8) :: suit
  END TYPE Card

  TYPE(Card) :: deck(52), hand(52)
  TYPE(Card) :: temp

  CHARACTER(5) :: pip(13) = (/"Two  ", "Three", "Four ", "Five ", "Six  ", "Seven", "Eight", "Nine ", "Ten  ", &
                              "Jack ", "Queen", "King ", "Ace  "/)
  CHARACTER(8) :: suits(4) = (/"Clubs   ", "Diamonds", "Hearts  ", "Spades  "/)
  INTEGER :: i, j, n, rand, dealt = 0
  REAL :: x

CONTAINS
 
  SUBROUTINE Init_deck
  ! Create deck
    DO i = 1, 4
      DO j = 1, 13
        deck((i-1)*13+j) = Card(pip(j), suits(i))
      END DO
    END DO
  END SUBROUTINE Init_deck
 
  SUBROUTINE Shuffle_deck
  ! Shuffle deck using Fisher-Yates algorithm
    DO i = 52-dealt, 1, -1
      CALL RANDOM_NUMBER(x)
      rand = INT(x * i) + 1
      temp = deck(rand)
      deck(rand) = deck(i)
      deck(i) = temp
    END DO
  END SUBROUTINE Shuffle_deck

  SUBROUTINE Deal_hand(number)
  ! Deal from deck to hand
    INTEGER :: number
    DO i = 1, number
      hand(i) = deck(dealt+1)
      dealt = dealt + 1
    END DO
  END SUBROUTINE

  SUBROUTINE Print_hand
  ! Print cards in hand
    DO i = 1, dealt
      WRITE (*, "(3A)") TRIM(deck(i)%value), " of ", TRIM(deck(i)%suit)
    END DO
    WRITE(*,*)
  END SUBROUTINE Print_hand
 
  SUBROUTINE Print_deck
  ! Print cards in deck
    DO i = dealt+1, 52
      WRITE (*, "(3A)") TRIM(deck(i)%value), " of ", TRIM(deck(i)%suit)
    END DO
    WRITE(*,*)
  END SUBROUTINE Print_deck

END MODULE Cards
```

Example use:

```fortran
PROGRAM Playing_Cards
 
  USE Cards
 
  CALL Init_deck
  CALL Shuffle_deck
  CALL Deal_hand(5)
  CALL Print_hand
  CALL Print_deck
  
END PROGRAM
```

This creates a new deck, shuffles it, deals five cards to hand, prints the cards in hand and then prints the cards remaining in the deck.


## Go


```go
package cards

import (
	"math/rand"
)

// A Suit represents one of the four standard suites.
type Suit uint8

// The four standard suites.
const (
	Spade   Suit = 3
	Heart   Suit = 2
	Diamond Suit = 1
	Club    Suit = 0
)

func (s Suit) String() string {
	const suites = "CDHS" // or "♣♢♡♠"
	return suites[s : s+1]
}

// Rank is the rank or pip value of a card from Ace==1 to King==13.
type Rank uint8

// The ranks from Ace to King.
const (
	Ace   Rank = 1
	Two   Rank = 2
	Three Rank = 3
	Four  Rank = 4
	Five  Rank = 5
	Six   Rank = 6
	Seven Rank = 7
	Eight Rank = 8
	Nine  Rank = 9
	Ten   Rank = 10
	Jack  Rank = 11
	Queen Rank = 12
	King  Rank = 13
)

func (r Rank) String() string {
	const ranks = "A23456789TJQK"
	return ranks[r-1 : r]
}

// A Card represets a specific playing card.
// It's an encoded representation of Rank and Suit
// with a valid range of [0,51].
type Card uint8

// NewCard returns the Card representation for the specified rank and suit.
func NewCard(r Rank, s Suit) Card {
	return Card(13*uint8(s) + uint8(r-1))
}

// RankSuit returns the rank and suit of the card.
func (c Card) RankSuit() (Rank, Suit) {
	return Rank(c%13 + 1), Suit(c / 13)
}

// Rank returns the rank of the card.
func (c Card) Rank() Rank {
	return Rank(c%13 + 1)
}

// Suit returns the suit of the card.
func (c Card) Suit() Suit {
	return Suit(c / 13)
}

func (c Card) String() string {
	return c.Rank().String() + c.Suit().String()
}

// A Deck represents a set of zero or more cards in a specific order.
type Deck []Card

// NewDeck returns a regular 52 deck of cards in A-K order.
func NewDeck() Deck {
	d := make(Deck, 52)
	for i := range d {
		d[i] = Card(i)
	}
	return d
}

// String returns a string representation of the cards in the deck with
// a newline ('\n') separating the cards into groups of thirteen.
func (d Deck) String() string {
	s := ""
	for i, c := range d {
		switch {
		case i == 0: // do nothing
		case i%13 == 0:
			s += "\n"
		default:
			s += " "
		}
		s += c.String()
	}
	return s
}

// Shuffle randomises the order of the cards in the deck.
func (d Deck) Shuffle() {
	for i := range d {
		j := rand.Intn(i + 1)
		d[i], d[j] = d[j], d[i]
	}
}

// Contains returns true if the specified card is withing the deck.
func (d Deck) Contains(tc Card) bool {
	for _, c := range d {
		if c == tc {
			return true
		}
	}
	return false
}

// AddDeck adds the specified deck(s) to this one at the end/bottom.
func (d *Deck) AddDeck(decks ...Deck) {
	for _, o := range decks {
		*d = append(*d, o...)
	}
}

// AddCard adds the specified card to this deck at the end/bottom.
func (d *Deck) AddCard(c Card) {
	*d = append(*d, c)
}

// Draw removes the selected number of cards from the top of the deck,
// returning them as a new deck.
func (d *Deck) Draw(n int) Deck {
	old := *d
	*d = old[n:]
	return old[:n:n]
}

// DrawCard draws a single card off the top of the deck,
// removing it from the deck.
// It returns false if there are no cards in the deck.
func (d *Deck) DrawCard() (Card, bool) {
	if len(*d) == 0 {
		return 0, false
	}
	old := *d
	*d = old[1:]
	return old[0], true
}

// Deal deals out cards from the deck one at a time to multiple players.
// The initial hands (decks) of each player are provided as arguments and the
// modified hands are returned. The initial hands can be empty or nil.
// E.g. Deal(7, nil, nil, nil) deals out seven cards to three players
// each starting with no cards.
// If there are insufficient cards in the deck the hands are partially dealt and
// the boolean return is set to false (true otherwise).
func (d *Deck) Deal(cards int, hands ...Deck) ([]Deck, bool) {
	for i := 0; i < cards; i++ {
		for j := range hands {
			if len(*d) == 0 {
				return hands, false
			}
			hands[j] = append(hands[j], (*d)[0])
			*d = (*d)[1:]
		}
	}
	return hands, true
}
```

Example use:

```go
package main

import (
	"fmt"
	"whatever/path/was/used/cards"
)

func main() {
	// We do not call rand.Seed() in order for the results to be repeatable.
	//rand.Seed(time.Now().UnixNano())
	d := cards.NewDeck()
	fmt.Println("fresh deck")
	fmt.Println(d)

	d.Shuffle()
	fmt.Println("\nshuffled")
	fmt.Println(d)

	h := d.Draw(5)
	fmt.Println("\n5 cards drawn")
	fmt.Println(h)

	fmt.Println("\nrank, suit values of cards in drawn:")
	fmt.Println("Card  Rank   Suit")
	for _, c := range h {
		fmt.Printf("%v :  %v=%2[2]d   %v=%2[3]d\n", c, c.Rank(), c.Suit())
	}

	ans := h.Contains(cards.NewCard(cards.Queen, cards.Spade))
	fmt.Println("Drawn cards include the Queen of Spades?", ans)
        ans = h.Contains(cards.NewCard(cards.Jack, cards.Spade))
        fmt.Println("Drawn cards include the Jack of Spades?", ans)

	p, _ := d.Deal(7, nil, nil)
	fmt.Println("\nDealing 7 cards to two players")
	fmt.Println("Player1:", p[0])
	fmt.Println("Player2:", p[1])

	fmt.Println("\n", len(d), " cards left in deck")
	fmt.Println(d)

	d.AddDeck(h, p[0], p[0])
	fmt.Println("\nReturning the cards to the deck")
	fmt.Println(d)
}
```

Output:

```txt

fresh deck
AC 2C 3C 4C 5C 6C 7C 8C 9C TC JC QC KC
AD 2D 3D 4D 5D 6D 7D 8D 9D TD JD QD KD
AH 2H 3H 4H 5H 6H 7H 8H 9H TH JH QH KH
AS 2S 3S 4S 5S 6S 7S 8S 9S TS JS QS KS

shuffled
9D 5C 3C JS 9S 7H 7D 7S TH QH 3S 6D TC
TS 7C 5S AH 8D QS 4C 2H 3H QD 3D JH QC
AS KH 6S KS 9C KC 4S 4D 6H 8C 9H 6C 8H
2C 8S JD AC 2S TD KD 4H JC 2D AD 5D 5H

5 cards drawn
9D 5C 3C JS 9S

rank, suit values of cards in drawn:
Card  Rank   Suit
9D :  9= 9   D= 1
5C :  5= 5   C= 0
3C :  3= 3   C= 0
JS :  J=11   S= 3
9S :  9= 9   S= 3
Drawn cards include the Queen of Spades? false
Drawn cards include the Jack of Spades? true

Dealing 7 cards to two players
Player1: 7H 7S QH 6D TS 5S 8D
Player2: 7D TH 3S TC 7C AH QS

 33  cards left in deck
4C 2H 3H QD 3D JH QC AS KH 6S KS 9C KC
4S 4D 6H 8C 9H 6C 8H 2C 8S JD AC 2S TD
KD 4H JC 2D AD 5D 5H

Returning the cards to the deck
4C 2H 3H QD 3D JH QC AS KH 6S KS 9C KC
4S 4D 6H 8C 9H 6C 8H 2C 8S JD AC 2S TD
KD 4H JC 2D AD 5D 5H 9D 5C 3C JS 9S 7H
7S QH 6D TS 5S 8D 7H 7S QH 6D TS 5S 8D

```



## Groovy


```groovy
import groovy.transform.TupleConstructor

enum Pip {
    ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING
}
enum Suit {
    DIAMONDS, SPADES, HEARTS, CLUBS
}

@TupleConstructor
class Card {
    final Pip pip
    final Suit suit

    String toString() { "$pip of $suit" }
}

class Deck {
    private LinkedList cards = []

    Deck() { reset() }

    void reset() {
        cards = []
        Suit.values().each { suit ->
            Pip.values().each { pip ->
                cards << new Card(pip, suit)
            }
        }
    }

    Card deal() { cards.poll() }

    void shuffle() { Collections.shuffle cards }

    String toString() { cards.isEmpty() ? "Empty Deck" : "Deck $cards" }
}
```

Test Code

```groovy
Deck deck = new Deck()
deck.shuffle()
(0..<5).each { println deck.deal() }
```

Output:

```txt
TWO of DIAMONDS
KING of SPADES
KING of HEARTS
NINE of HEARTS
TEN of CLUBS
```



## Haskell


Straightforward implementation with explicit names for pips and suits. A deck is just a list of cards. Dealing consists of splitting off cards from the beginning of the list by the usual pattern matching (not shown). Printing is automatic. Purely functional shuffling is a bit tricky, so here we just do the naive quadratic version. This also works for other than full decks.

```haskell
import System.Random

data Pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
           Jack | Queen | King | Ace 
  deriving (Ord, Enum, Bounded, Eq, Show)

data Suit = Diamonds | Spades | Hearts | Clubs
  deriving (Ord, Enum, Bounded, Eq, Show)

type Card = (Pip, Suit)

fullRange :: (Bounded a, Enum a) => [a]
fullRange = [minBound..maxBound]

fullDeck :: [Card]
fullDeck = [(pip, suit) | pip <- fullRange, suit <- fullRange]

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys     = x:ys
insertAt n _ []     = error "insertAt: list too short"
insertAt n x (y:ys) = y : insertAt (n-1) x ys

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g xs = shuffle' g xs 0 [] where
  shuffle' g []     _ ys = ys
  shuffle' g (x:xs) n ys = shuffle' g' xs (n+1) (insertAt k x ys) where
    (k,g') = randomR (0,n) g
```



## Hy

{{works with|Hy|0.17.0+}}

A simple program that deals out two five card hands. 


```Hy
(import [random [shuffle]])
(setv pips (.split "2 3 4 5 6 7 8 9 10 J Q K A"))
(setv suits (.split "♥ ♦ ♣ ♠"))
(setv cards_per_hand 5)

(defn make_deck [pips suits]
    (lfor
        x pips
        y suits
        (+ x y)))

(defn deal_hand [num_cards deck]
    (setv delt (cut deck None num_cards))
    (setv new_deck (lfor
                       x deck
                       :if (not (in x delt))
                       x))
    [delt new_deck])


(if (= __name__ "__main__")
    (do
    (setv deck (make_deck pips suits))
    (shuffle deck)
    (setv [first_hand deck] (deal_hand cards_per_hand deck))
    (setv [second_hand deck] (deal_hand cards_per_hand deck))
    (print "\nThe first hand delt was:" (.join " " (map str first_hand)))
    (print "\nThe second hand delt was:" (.join " " (map str second_hand)))
    (print "\nThe remaining cards in the deck are...\n" (.join " " (map str deck)))))
```


Sample Output:

The first hand delt was: 5♠ 3♣ 4♥ 10♦ 6♣

The second hand delt was: J♣ 8♣ 9♣ K♠ 2♦

The remaining cards in the deck are...
3♦ 6♥ 10♠ Q♦ 7♥ 2♠ 5♥ K♥ A♦ A♥ J♥ 4♣ 3♥ A♣ 8♦ 5♦ 10♥ 7♠ A♠ J♠ 5♣ 2♥ 9♥ 4♦ 8♠ 9♦ 6♠ 7♣ 10♣ 2♣ Q♥ Q♠ 9♠ 7♦ 8♥ Q♣ 3♠ 6♦ K♣ 4♠ K♦ J♦


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)

   cards   := 2                                             # cards per hand
   players := 5                                             # players to deal to

   write("New deck : ", showcards(D := newcarddeck()))      # create and show a new deck
   write("Shuffled : ", showcards(D := shufflecards(D)))    # shuffle it 

   H := list(players) 
   every H[1 to players] := []                              # hands for each player

   every ( c := 1 to cards 	) & ( p := 1 to players ) do    
      put(H[p], dealcard(D))                                # deal #players hands of #cards

   every write("Player #",p := 1 to players,"'s hand : ",showcards(H[p]))
   write("Remaining: ",showcards(D))                        # show the rest of the deck
end

record card(suit,pip)                                       #: datatype for a card suit x pip

procedure newcarddeck()			                    #: return a new standard deck
local D

   every put(D := [], card(suits(),pips()))
   return D
end

procedure suits()				            #: generate suits
   suspend !["H","S","D","C"]
end

procedure pips()					    #: generate pips
   suspend !["2","3","4","5","6","7","8","9","10","J","Q","K","A"]
end

procedure shufflecards(D)			            #: shuffle a list of cards
   every !D :=: ?D                                          # see INL#9
   return D
end

procedure dealcard(D)				            #: deal a card (from the top)
   return get(D)
end

procedure showcards(D)			                    #: return a string of all cards in the given list (deck/hand/etc.)
local s
   every (s := "") ||:= card2string(!D) || " " 
   return s
end

procedure card2string(x)			            #: return a string version of a card
   return x.pip || x.suit
end
```

Sample output:

```txt
New deck : 2H 3H 4H 5H 6H 7H 8H 9H 10H JH QH KH AH 2S 3S 4S 5S 6S 7S 8S 9S 10S JS QS KS AS 2D 3D 4D 5D 6D 7D 8D 9D 10D JD QD KD AD 2C 3C 4C 5C 6C 7C 8C 9C 10C JC QC KC AC
Shuffled : 5H 8S AC 5D 3H 9D 4S 3S 2C AD JD QH 6D 7H 4C 8H 3C KH 6S 10D 4D 8D AH 7D QS 2S QD 6H 9C KS JC 2D 10H JH 5S 2H 3D 10C 4H KC JS 8C 7S 10S KD 9S 6C 9H 7C AS QC 5C
Player #1's hand : 5H 9D
Player #2's hand : 8S 4S
Player #3's hand : AC 3S
Player #4's hand : 5D 2C
Player #5's hand : 3H AD
Remaining: JD QH 6D 7H 4C 8H 3C KH 6S 10D 4D 8D AH 7D QS 2S QD 6H 9C KS JC 2D 10H JH 5S 2H 3D 10C 4H KC JS 8C 7S 10S KD 9S 6C 9H 7C AS QC 5C
```



## J

'''Solution:'''


```j
NB. playingcards.ijs
NB. Defines a Rosetta Code playing cards class
NB. Multiple decks may be used, one for each instance of this class.

coclass 'rcpc'    NB. Rosetta Code playing cards class

NB. Class objects
Ranks=: _2 ]\ ' A 2 3 4 5 6 7 8 910 J Q K'
Suits=: ucp '♦♣♥♠'
DeckPrototype=: (] #: i.@:*/)Ranks ,&# Suits

NB. Class methods
create=: monad define
 1: TheDeck=: DeckPrototype
)

destroy=: codestroy

sayCards=: ({&Ranks@{. , {&Suits@{:)"1

shuffle=: monad define
 1: TheDeck=: ({~ ?~@#) TheDeck
)

NB.*dealCards v Deals y cards [to x players]
NB. x is: optional number of players, defaults to one
NB. Used monadically, the player-axis is omitted from output.
dealCards=: verb define
 {. 1 dealCards y
:
 'Too few cards in deck' assert (# TheDeck) >: ToBeDealt=. x*y
 CardsOffTop=. ToBeDealt {. TheDeck
 TheDeck    =: ToBeDealt }. TheDeck
 (x,y)$ CardsOffTop
)

NB.*pcc v "Print" current contents of the deck.
pcc=: monad define
 sayCards TheDeck
)

newDeck_z_=: conew&'rcpc'
```


'''Example use:'''

```j
   load '~user/playingcards.ijs'
   coinsert 'rcpc'              NB. insert rcpc class in the path of current locale
   pc=: newDeck ''
   $TheDeck__pc
52 2
   shuffle__pc ''
1
   sayCards 2 dealCards__pc 5   NB. deal two hands of five cards
 3♦
 4♦
 K♠
 A♦
 K♦

 5♠
10♣
 Q♥
 2♣
 9♣
   $TheDeck__pc                 NB. deck size has been reduced by the ten cards dealt
42 2
   destroy__pc ''
1
```



## Java

{{works with|Java|1.5+}}


```java
public enum Pip { Two, Three, Four, Five, Six, Seven, 
    Eight, Nine, Ten, Jack, Queen, King, Ace }
```


```java
public enum Suit { Diamonds, Spades, Hearts, Clubs }
```


The card:

```java
public class Card {
    private final Suit suit;
    private final Pip value;

    public Card(Suit s, Pip v) {
        suit = s;
        value = v;
    }

    public String toString() {
        return value + " of " + suit;
    }
}
```

The deck:

```java
import java.util.Collections;
import java.util.LinkedList;

public class Deck {
    private final LinkedList<Card> deck= new LinkedList<Card>();

    public Deck() {
        for (Suit s : Suit.values())
            for (Pip v : Pip.values())
                deck.add(new Card(s, v));
    }

    public Card deal() {
        return deck.poll();
    }

    public void shuffle() {
        Collections.shuffle(deck); // I'm such a cheater
    }

    public String toString(){
        return deck.toString();
    }
}
```



## JavaScript


```javascript
function Card(pip, suit) {
    this.pip = pip;
    this.suit = suit; 

    this.toString = function () {
        return this.pip + ' ' + this.suit;
    };
}
 
function Deck() {
    var pips = '2 3 4 5 6 7 8 9 10 Jack Queen King Ace'.split(' ');
    var suits = 'Clubs Hearts Spades Diamonds'.split(' ');
    this.deck = [];
    for (var i = 0; i < suits.length; i++)
        for (var j = 0; j < pips.length; j++)
            this.deck.push(new Card(pips[j], suits[i]));

    this.toString = function () {
        return '[' + this.deck.join(', ') + ']';
    };
 
    this.shuffle = function () {
        for (var i = 0; i < this.deck.length; i++)
            this.deck[i] = this.deck.splice(
                parseInt(this.deck.length * Math.random()), 1, this.deck[i])[0];
    };

    this.deal = function () {
        return this.deck.shift();
    };
}
```



## Julia

'''Deck Types and Constructors'''

A deck consists of an array of integers and a <tt>DeckDesign</tt> type, which defines the meanings of the cards.  This is a somewhat simplified implementation.  While strictly speaking, a deck should be an array of cards, it is sufficient here to use integers.  Indeed, cards and hands are just decks with smaller numbers of cards.


```Julia

type DeckDesign{T<:Integer,U<:String}
    rlen::T
    slen::T
    ranks::Array{U,1}
    suits::Array{U,1}
    hlen::T
end

type Deck{T<:Integer}
    cards::Array{T,1}
    design::DeckDesign
end

Deck(n::Integer, des::DeckDesign) = Deck([n], des)

function pokerlayout()
    r = [map(string, 2:10), "J", "Q", "K", "A"]
    r = map(utf8, r)
    s = ["\u2663", "\u2666", "\u2665", "\u2660"]
    DeckDesign(13, 4, r, s, 5)
end

function fresh(des::DeckDesign)
    Deck(collect(1:des.rlen*des.slen), des)
end

```


'''Define a Few of the Standard Methods'''

Many of these definitions are simply passed through to the card array component of the deck.  But note that <tt>size</tt> returns parameters appropriate to a complete deck.  This behavior is helpful when assigning meaning to any list of cards.
 

```Julia

Base.isempty(d::Deck) = isempty(d.cards)
Base.empty!(d::Deck) = empty!(d.cards)
Base.length(d::Deck) = length(d.cards)
Base.endof(d::Deck) = endof(d.cards)
Base.shuffle!(d::Deck) = shuffle!(d.cards)
Base.sort!(d::Deck) = sort!(d.cards)
Base.getindex(d::Deck, r) = Deck(getindex(d.cards, r), d.design)
Base.size(d::Deck) = (d.design.rlen, d.design.slen)
function Base.print(d::Deck)
    sz = size(d)
    r = map(x->d.design.ranks[ind2sub(sz, x)[1]], d.cards)
    s = map(x->d.design.suits[ind2sub(sz, x)[2]], d.cards)
    join(r.*s, " ")
end

```


'''Define Some Special Methods'''

<tt>deal!</tt> is the only deck specific method required to complete the essentials for this task.


```Julia

function deal!{T<:Integer}(d::Deck, hlen::T)
    if hlen < length(d)
        hand = Deck(d.cards[1:hlen], d.design)
        d.cards = d.cards[hlen+1:end]
    else
        hand = d
        empty!(d)
    end
    return hand
end

function deal!(d::Deck)
    deal!(d, d.design.hlen)
end

function pretty(d::Deck)
    s = ""
    llen = d.design.rlen
    dlen = length(d)
    for i in 1:llen:dlen
        j = min(i+llen-1, dlen)
        s *= print(d[i:j])*"\n"
    end
    chop(s)
end

```


'''Main'''

```Julia

d = fresh(pokerlayout())
println("A new poker deck:")
println(pretty(d))

shuffle!(d)
println()
println("The deck shuffled:")
println(pretty(d))

n = 4
println()
println("Deal ", n, " hands:")
for i in 1:n
    h = deal!(d)
    println(pretty(h))
end

println()
println("And now the deck contains:")
println(pretty(d))

```


{{out}}

```txt

A new poker deck:
2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ 10♣ J♣ Q♣ K♣ A♣
2♦ 3♦ 4♦ 5♦ 6♦ 7♦ 8♦ 9♦ 10♦ J♦ Q♦ K♦ A♦
2♥ 3♥ 4♥ 5♥ 6♥ 7♥ 8♥ 9♥ 10♥ J♥ Q♥ K♥ A♥
2♠ 3♠ 4♠ 5♠ 6♠ 7♠ 8♠ 9♠ 10♠ J♠ Q♠ K♠ A♠

The deck shuffled:
Q♥ 6♠ 4♣ 2♠ 5♣ 10♣ 9♠ K♥ 7♠ 3♠ 2♥ 4♥ A♣
6♥ A♥ 3♥ 3♦ K♦ 10♦ 10♠ 8♣ 4♦ 8♦ K♣ 5♦ 6♣
7♦ 9♦ 6♦ 8♥ 10♥ 5♥ J♠ 9♣ 8♠ Q♦ 2♦ K♠ Q♣
J♣ J♥ 7♥ J♦ 2♣ 4♠ 5♠ 9♥ A♦ 3♣ 7♣ A♠ Q♠

Deal 4 hands:
Q♥ 6♠ 4♣ 2♠ 5♣
10♣ 9♠ K♥ 7♠ 3♠
2♥ 4♥ A♣ 6♥ A♥
3♥ 3♦ K♦ 10♦ 10♠

And now the deck contains:
8♣ 4♦ 8♦ K♣ 5♦ 6♣ 7♦ 9♦ 6♦ 8♥ 10♥ 5♥ J♠
9♣ 8♠ Q♦ 2♦ K♠ Q♣ J♣ J♥ 7♥ J♦ 2♣ 4♠ 5♠
9♥ A♦ 3♣ 7♣ A♠ Q♠

```



## K

The deck is stored in the global variable "deck".

Create the deck.

```K
   v:"A23456789TJQK" / values
   s:"SHCD"          / suites

   / create a new deck
   newdeck:{deck::,/s,'\:v}
   newdeck();
```


Show the deck.

```K
   show:{`0:$,/-3$$deck}

   show()
SA S2 S3 S4 S5 S6 S7 S8 S9 ST SJ SQ SK HA H2 H3 H4 H5 H6 H7 H8 H9 HT HJ HQ HK CA C2 C3 C4 C5 C6 C7 C8 C9 CT CJ CQ CK DA D2 D3 D4 D5 D6 D7 D8 D9 DT DJ DQ DK 
```


Shuffle the deck.

```K
   shuffle:{deck::(-#deck)?deck}

   shuffle();show()
S8 CA D5 D2 SJ D6 DJ H7 S4 S9 SQ SK S5 D8 C4 HT DA H3 S6 S2 DT HA C2 C5 D9 ST C7 DK S3 HQ D7 DQ C8 D3 SA CJ CQ CT H4 H2 CK H9 H5 C3 C6 H6 D4 HJ C9 S7 HK H8 
```


Deal: Get the N top cards and remove them from the deck.
Deal 5 cards.

```K
   deal1:{|((#deck)-x)_|deck}
   deal:{c:deal1[x];deck::(deck _dvl c);c}

   deal[5]
("S8"
 "CA"
 "D5"
 "D2"
 "SJ")

   #deck / 5 cards are removed
47
```


Deal 3 more hands.

```K
   {deal@5}'!3
(("D6"
  "DJ"
  "H7"
  "S4"
  "S9")
 ("SQ"
  "SK"
  "S5"
  "D8"
  "C4")
 ("HT"
  "DA"
  "H3"
  "S6"
  "S2"))
```


We now have 32 cards left.

```K
  #deck
32
  show()
DT HA C2 C5 D9 ST C7 DK S3 HQ D7 DQ C8 D3 SA CJ CQ CT H4 H2 CK H9 H5 C3 C6 H6 D4 HJ C9 S7 HK H8
```



## Kotlin


```scala
// version 1.3.50
const val FACES = "23456789TJQKA"
const val SUITS = "shdc"

fun createDeck(): List<String> {
    val cards = mutableListOf<String>()
    FACES.forEach { face -> SUITS.forEach { suit -> cards.add("$face$suit") } }
    return cards
}

fun dealTopDeck(deck: List<String>, n: Int) = deck.take(n)

fun dealBottomDeck(deck: List<String>, n: Int) = deck.takeLast(n).reversed()

fun printDeck(deck: List<String>) {
    for (i in deck.indices) {
        print("${deck[i]}  ")
        if ((i + 1) % 13 == 0 || i == deck.size - 1) println()
    }
}

fun main(args: Array<String>) {
    var deck = createDeck()
    println("After creation, deck consists of:")
    printDeck(deck)
    deck = deck.shuffled()
    println("\nAfter shuffling, deck consists of:")
    printDeck(deck)
    val dealtTop = dealTopDeck(deck, 10)
    println("\nThe 10 cards dealt from the top of the deck are:")
    printDeck(dealtTop)
    val dealtBottom = dealBottomDeck(deck, 10)
    println("\nThe 10 cards dealt from the bottom of the deck are:")
    printDeck(dealtBottom)
}

```

Sample output:
{{out}}

```txt

After creation, deck consists of:
2s  3s  4s  5s  6s  7s  8s  9s  Ts  Js  Qs  Ks  As  
2h  3h  4h  5h  6h  7h  8h  9h  Th  Jh  Qh  Kh  Ah  
2d  3d  4d  5d  6d  7d  8d  9d  Td  Jd  Qd  Kd  Ad  
2c  3c  4c  5c  6c  7c  8c  9c  Tc  Jc  Qc  Kc  Ac  

After shuffling, deck consists of:
2h  6h  5s  9s  Td  Kh  Jc  4s  Ac  Tc  7s  8s  9c  
Js  3s  Th  Kd  2d  Qd  8h  3c  6d  Qc  2c  Ks  Ad  
9d  5c  6s  4h  Qh  4d  Ah  3d  As  5h  Ts  7d  Jh  
Jd  4c  8d  7h  6c  2s  Qs  7c  Kc  9h  8c  3h  5d  

The 10 cards dealt from the top of the deck are:
2h  6h  5s  9s  Td  Kh  Jc  4s  Ac  Tc  

The 10 cards dealt from the bottom of the deck are:
5d  3h  8c  9h  Kc  7c  Qs  2s  6c  7h   

```



## Liberty BASIC


```lb
    Dim deckCards(52)
    Dim holdCards(1, 1)
Print "The Sorted Deck"
    Call sortDeck
    Call dealDeck
Print: Print
Print "The Shuffled Deck"
    Call shuffleDeck
    Call dealDeck
Print: Print
    nPlayers = 4
    nCards = 5
    ct = 0
    Redim holdCards(nPlayers, nCards)
Print "Dealing ";nCards;" cards to ";nPlayers;" players"
    For i = 1 to nPlayers
        Print "Player #";i,,
    Next i
    Print
    For i = 1 to nCards
        For j = 1 to nPlayers
            ct = ct + 1
            holdCards(j, i) = deckCards(ct)
            card = deckCards(ct)
            value = value(card)
            suit$ = suit$(card)
            pip$ = pip$(value)
            Print card;": ";pip$;" of ";suit$,
        Next j
        Print
    Next i
Print: Print
    Print "The cards in memory / array"
    For i = 1 to nPlayers
        Print "Player #";i;" is holding"
        For j = 1 to nCards
            card = holdCards(i, j)
            value = value(card)
            suit$ = suit$(card)
            pip$ = pip$(value)
            Print card;": ";pip$;" of ";suit$
        Next j
        Print
    Next i

End

Sub dealDeck
    For i = 1 to 52
        card = deckCards(i)
        value = value(card)
        suit$ = suit$(card)
        pip$ = pip$(value)
        Print i, card, value, pip$;" of ";suit$
    Next i
End Sub

Sub sortDeck
    For i = 1 to 52
        deckCards(i) = i
    Next i
End Sub

Sub shuffleDeck
    For i = 52 to 1 Step -1
        x = Int(Rnd(1) * i) + 1
        temp = deckCards(x)
        deckCards(x) = deckCards(i)
        deckCards(i) = temp
    Next i
End Sub

Function suit$(deckValue)
    cardSuit$ = "Spades Hearts Clubs Diamonds"
    suit = Int(deckValue / 13)
    If deckValue Mod 13 = 0 Then
        suit = suit - 1
    End If
    suit$ = Word$(cardSuit$, suit + 1)
End Function

Function value(deckValue)
    value = deckValue Mod 13
    If value = 0 Then
        value = 13
    End If
End Function

Function pip$(faceValue)
    pipLabel$ = "Ace Deuce Three Four Five Six Seven Eight Nine Ten Jack Queen King"
    pip$ = Word$(pipLabel$, faceValue)
End Function
```



## Logo

{{works with|UCB Logo}}

```logo
make "suits {Diamonds Hearts Clubs Spades}
make "pips {Ace Two Three Four Five Six Seven Eight Nine Ten Jack Queen King}

to card :n
  output (sentence item 1 + modulo :n 13 :pips  "of  item 1 + int quotient :n 13 :suits)
end

to new.deck
  make "deck listtoarray iseq 0 51
  make "top 1
end

to swap :i :j :a
  localmake "t item :i :a
  setitem :i :a item :j :a
  setitem :j :a :t
end
to shuffle.deck
  for [i [count :deck] 2] [swap 1 + random :i :i :deck]
end

to show.deck
  for [i :top [count :deck]] [show card item :i :deck]
end

to deal.card
  show card item :top :deck
  make "top :top + 1
end

new.deck
shuffle.deck
repeat 5 [deal.card]
```



## Lua


### Version 1


```Lua

suits = {"Clubs", "Diamonds", "Hearts", "Spades"}
faces = {2,3,4,5,6,7,8,9,10,"Jack","Queen","King","Ace"}
--a stack is a set of cards. a stack of length 1 acts as a card; the stack constructor only creates decks.

stack = setmetatable({
--shuffles a stack
__unm = function(z)
  local ret = {}
  for i = #z, 1, -1 do
    ret[#ret + 1] = table.remove(z,math.random(i))
  end
  return setmetatable(ret, stack)
end,
--puts two stacks together
__add = function(z, z2)
  for i = 1, #z2 do
    z[#z+1] = table.remove(z2)
  end
  return z
end,
--removes n cards from a stack and returns a stack of those cards
__sub = function(z, n)
  local ret = {}
  for i = 1, n do
    ret[i] = table.remove(z)
  end
  return setmetatable(ret, stack)
end,
--breaks a stack into n equally sized stacks and returns them all
deal = function(z, n)
  local ret = {}
  for i = 1, #z/n do
    ret[i] = table.remove(z)
  end
  if n > 1 then return setmetatable(ret, stack), stack.deal(z,n-1)
  else return setmetatable(ret, stack)
  end
end,
--returns a and b as strings, concatenated together. Simple enough, right?
__concat = function(a, b)
  if getmetatable(a) == stack then
    return stack.stackstring(a) .. b
  else
    return a .. stack.stackstring(b)
  end
end,
stackstring = function(st, ind)
    ind = ind or 1
	if not st[ind] then return "" end
	return st[ind] and (faces[math.ceil(st[ind]/4)] .. " of " .. suits[st[ind]%4+1] .. "\n" .. stack.stackstring(st, ind+1)) or ""
end}, {
--creates a deck
__call = function(z)
  local ret = {}
  for i = 1, 52 do ret[i] = i end
  return -setmetatable(ret,z)
end})

print(stack() .. "\n")
a, b, c, d = stack.deal(stack(), 4)
print(a .. "\n\n\n")
print(b + c .. "\n\n\n")
print(d - 4 .. "")
print(-b .. "")

```



### Version 2


```Lua
local tPlayers = {} -- cards of players
local tBoard = {}   -- cards in a board
local nPlayers = 5  -- number of players

local tDeck = {
'2d', '3d', '4d', '5d', '6d', '7d', '8d', '9d', 'Td', 'Jd', 'Qd', 'Kd', 'Ad', -- DIAMONDS
'2s', '3s', '4s', '5s', '6s', '7s', '8s', '9s', 'Ts', 'Js', 'Qs', 'Ks', 'As', -- SPADES
'2h', '3h', '4h', '5h', '6h', '7h', '8h', '9h', 'Th', 'Jh', 'Qh', 'Kh', 'Ah', -- HEARTS
'2c', '3c', '4c', '5c', '6c', '7c', '8c', '9c', 'Tc', 'Jc', 'Qc', 'Kc', 'Ac'} -- CLUBS

local function shuffle() -- Fisher–Yates shuffle
  i = #tDeck
  while i > 1 do
    i = i - 1
    j = math.random(1, i)
    tDeck[j], tDeck[i] = tDeck[i], tDeck[j]
  end
  return tDeck
end

local function cardTransfer(to, amount, from)
  for f = 1, amount do
	table.insert(to, #to+1, from[#from])
	from[#from] = nil
  end
end

----||EXAMPLE OF USE||----
print('FRESH DECK \n', table.concat(tDeck, ' '), '\n')

shuffle()

print('SHUFFLED DECK \n', table.concat(tDeck, ' '), '\n')

for a = 1, nPlayers do
  tPlayers[a] = {}
  cardTransfer(tPlayers[a], 2, tDeck)
end

cardTransfer(tBoard, 5, tDeck)

print('BOARD\n', table.concat(tBoard, ' '), '\n')

for b = 1, nPlayers do
  print('PLAYER #'..b..': ', table.concat(tPlayers[b], ' '))
end

print('\nREMAINING\n', table.concat(tDeck, ' '), '\n')

for c = 1, #tPlayers do
  for d = 1, #tPlayers[c] do
    cardTransfer(tDeck, d, tPlayers[c])
  end
end

cardTransfer(tDeck, 5, tBoard)

print('ALL CARDS IN THE DECK\n', table.concat(tDeck, ' '), '\n')

```


Output:

```Lua
FRESH DECK 
2d 3d 4d 5d 6d 7d 8d 9d Td Jd Qd Kd Ad 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks As 2h 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh Ah 2c 3c 4c 5c 6c 7c 8c 9c Tc Jc Qc Kc Ac	

SHUFFLED DECK 
7c 3d 8h 7h 7s 9c 8c Ks 8s 2s 5s 8d 2h 3h Jc 6h Td Ts Jh Tc 6s Kd 7d 4h 4d 5d Qd 5h 5c Kh 9d 2d Ah 6d 3c Js 9h Qh 4c 3s As Kc Qs Ad Th 4s Jd Ac Qc 2c 9s 6c	

BOARD
Kc As 3s 4c Qh	

PLAYER #1: 6c 9s
PLAYER #2: 2c Qc
PLAYER #3: Ac Jd
PLAYER #4: 4s Th
PLAYER #5: Ad Qs

REMAINING
7c 3d 8h 7h 7s 9c 8c Ks 8s 2s 5s 8d 2h 3h Jc 6h Td Ts Jh Tc 6s Kd 7d 4h 4d 5d Qd 5h 5c Kh 9d 2d Ah 6d 3c Js 9h	

ALL CARDS IN THE DECK
7c 3d 8h 7h 7s 9c 8c Ks 8s 2s 5s 8d 2h 3h Jc 6h Td Ts Jh Tc 6s Kd 7d 4h 4d 5d Qd 5h 5c Kh 9d 2d Ah 6d 3c Js 9h 9s 6c Qc 2c Jd Ac Th 4s Qs Ad Qh 4c 3s As Kc	

```


## M2000 Interpreter

We can use one or more packs. When we need a card and deck has no card then a new pack inserted (automatic drop some random cards)

```M2000 Interpreter

Module PlayCards {
      Font "Arial"  ' Ensure characters exist for Suits
      Cls 15,0
      Pen 0
      Inventory Suits = "♠":=0, "♥":=4, "♦":=4, "♣":=0 'suit -> color
      Inventory Cards = "two":=2, "three":=3, "four":=4, "five":=5
      Append Cards, "six":=6, "seven":=7, "eight":=8, "nine":=9
      Append Cards, "ten":=10, "jack":=10, "queen":=10, "king":=10, "ace":=1
      DealerMoney=0
      PrintCardOnly = Lambda Suits, Cards (k, nl=True)-> {
                For k {
                      Pen Suits(.suit!) {
                              If nl then {
                                    Print Part @(10), Eval$(Suits, .suit)+Eval$(Cards, .card)
                                    Print
                              } Else Print Eval$(Suits, .suit)+Eval$(Cards, .card),
                         }
              }
      }
      ' Using a stack object
      StackPack = Stack
      Module AppendArray (N, A) {
            Stack N {Data !A}
      }
      Class OneCard {
            suit=-1, Card
      Class:
           Module OneCard {
                 \\ ? for optional reading
                 read ? .suit, .card
           }
      }
      Decks=1
      Dim Pack(Len(Cards)*Len(Suits)*Decks) 
      k=0
      \\ fill cards to Pack()
      For times=1 To Decks {
            N=each(Suits)
            While N {
                  M=each(Cards)
                  While M {
                        Pack(k)=OneCard(N^, M^)
                        k++
                  }
            }     
      }
      DisplayAll() ' in order
      Suffle()
      DisplayAll() ' at random positions
      Print
      Card=OneCard()
      Print "Cards in Deck:";Len(StackPack)
      For i=1 to 60 {
            NextCard()
            Print "Get Card:";
            Call PrintCardOnly(Card)
            Print
            Print "Cards in Deck:";Len(StackPack)
            DisplayDeck()
            Print
      }
      
      Sub Suffle()
            Print
            Local N=Len(Pack())-1, N2, i, j, total=N*4+4, cur=1
            For j=1 To 4 {
                  For i=0 To N {
                        If cur Mod 4=3 Then Print Over format$("Suffle {0:0}%",cur/total*100)
                        N2=random(0, N)
                        While N2=i {N2=random(0, N)}
                        Swap Pack(i), Pack(N2)
                        cur++
                  }
            }
            AppendArray StackPack, Pack()
            Print
      End Sub
      Sub DisplayDeck()
            local m=each(StackPack)
            While m  {
                      Call PrintCardOnly(StackItem(m), False)
            }
      End Sub
      Sub DisplayAll()
            For k=0 To Len(Pack())-1 {
                  PrintCard(k)
            }
      End Sub
      Sub PrintCard(k)
            For Pack(k) {
                  Pen Suits(.suit!) {
                        Print Eval$(Suits, .suit)+Eval$(Cards, .card),
                  }
             }
      End Sub
      Sub NextCard()
                  If Len(StackPack)=0 Then {
                        Suffle()
                        Stack StackPack {
                              Drop Random(0, 51)
                        }
                  }
                  Stack StackPack {
                        Read Card
                  }
      End Sub
}
PlayCards

```



## M4


```M4
define(`randSeed',141592653)dnl
define(`setRand',
   `define(`randSeed',ifelse(eval($1<10000),1,`eval(20000-$1)',`$1'))')dnl
define(`rand_t',`eval(randSeed^(randSeed>>13))')dnl
define(`random',
   `define(`randSeed',eval((rand_t^(rand_t<<18))&0x7fffffff))randSeed')dnl
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')dnl
define(`foreach', `pushdef(`$1')_foreach($@)popdef(`$1')')dnl
define(`_arg1', `$1')dnl
define(`_foreach', `ifelse(`$2', `()', `',
   `define(`$1', _arg1$2)$3`'$0(`$1', (shift$2), `$3')')')dnl
define(`new',`define(`$1[size]',0)')dnl
define(`append',
   `define(`$1[size]',incr(defn(`$1[size]')))`'define($1[defn($1[size])],$2)')
define(`deck',
   `new($1)foreach(`x',(Ace,2,3,4,5,6,7,8,9,10,Jack,Queen,King),
      `foreach(`y',(Clubs,Diamonds,Hearts,Spades),
         `append(`$1',`x of y')')')')dnl
define(`show',
   `for(`x',1,defn($1[size]),`defn($1[x])ifelse(x,defn($1[size]),`',`, ')')')dnl
define(`swap',`define($1[$2],defn($1[$4]))define($1[$4],$3)')dnl
define(`shuffle',
   `for(`x',1,defn($1[size]),
      `swap($1,x,defn($1[x]),eval(1+random%defn($1[size])))')')dnl
define(`deal',
   `ifelse($#,0,``$0'',
   `ifelse(defn($1[size]),0,
      `NULL',
      defn($1[defn($1[size])])define($1[size],decr(defn($1[size]))))')')dnl
dnl
deck(`b')
show(`b')
shuffling shuffle(`b')
show(`b')
deal deal(`b')
deal deal(`b')
show(`b')
```



## Mathematica


```Mathematica
MakeDeck[] :=  Tuples[{{"Ace ", 2, 3 , 4 , 5, 6 , 7 , 8 , 9 , 10, "Jack" , "Queen", "King"}, {♦ , ♣, ♥ , ♠}}]
DeckShuffle[deck_] := RandomSample[deck, Length@deck]
DealFromDeck[] := (Print@First@deck; deck = deck[[2 ;; All]];)
```

Example usage:

```txt
deck = DeckShuffle@MakeDeck[]; Print[deck]

->{{10,♥},{10,♦},{5,♣},{7,♠},{4,♦},{8,♦},{4,♠},{Queen,♠},{5,♠},{4,♥},{9,♦},{King,♥},{2,♦},{Ace ,♦},
{3,♣},{6,♦},{Jack,♣},{10,♠},{2,♠},{3,♠},{9,♠},{King,♠},{Queen,♥},{8,♣},{King,♦},{6,♣},{4,♣},{Jack,♦},
{5,♦},{Ace ,♣},{2,♣},{8,♥},{Jack,♠},{5,♥},{7,♣},{8,♠},{King,♣},{Queen,♦},{9,♥},{Ace ,♥},{3,♦},{7,♥},
{Queen,♣},{10,♣},{3,♥},{2,♥},{Jack,♥},{7,♦},{6,♠},{6,♥},{Ace ,♠},{9,♣}}

DealFromDeck[]
DealFromDeck[]
DealFromDeck[]

->{10,♥}
->{10,♦}
->{5,♣}

Print[deck]

->{{7,♠},{4,♦},{8,♦},{4,♠},{Queen,♠},{5,♠},{4,♥},{9,♦},{King,♥},{2,♦},{Ace ,♦},{3,♣},{6,♦},{Jack,♣},
{10,♠},{2,♠},{3,♠},{9,♠},{King,♠},{Queen,♥},{8,♣},{King,♦},{6,♣},{4,♣},{Jack,♦},{5,♦},{Ace ,♣},{2,♣},
{8,♥},{Jack,♠},{5,♥},{7,♣},{8,♠},{King,♣},{Queen,♦},{9,♥},{Ace ,♥},{3,♦},{7,♥},{Queen,♣},{10,♣},
{3,♥},{2,♥},{Jack,♥},{7,♦},{6,♠},{6,♥},{Ace ,♠},{9,♣}}
```



## MiniScript


```MiniScript
suits = ["Spades", "Clubs", "Hearts", "Diamonds"]
pips = ["Ace","Two","Three","Four","Five","Six","Seven",
    "Eight","Nine","Ten","Jack","Queen","King"]

Card = {}
Card.str = function()
    return self.pip + " of " + self.suit + " (value: " + self.value + ")"
end function

//Build Deck
deck = []
for s in suits.indexes
    for p in pips.indexes
        card = new Card
        card.suit = suits[s]
        card.pip = pips[p]
        card.value = s * 100 + p
        deck.push card
    end for
end for
 
draw = function(count=7)
    hand = []
    for i in range(1, count)
        hand.push deck.pop
    end for
    return hand
end function
 
display = function(stack)
    for card in stack
        print card.str
    end for
end function
 
print "Deck created. Cards in Deck: " + deck.len
 
deck.shuffle
print "Deck Shuffled"
 
hand = draw
print "First hand: "
display hand

print
print deck.len + " cards left in deck:"
display deck
```

{{out}}

```txt
Deck created. Cards in Deck: 52
Deck Shuffled
First hand: 
Nine of Diamonds (value: 308)
Queen of Spades (value: 11)
Seven of Clubs (value: 106)
Ten of Diamonds (value: 309)
Seven of Hearts (value: 206)
Eight of Spades (value: 7)
King of Diamonds (value: 312)
 
45 cards left in deck:
Nine of Spades (value: 8)
Four of Spades (value: 3)
Queen of Hearts (value: 211)
   (etc.)
```



## MUMPS

See [[Playing Cards/MUMPS]]


## Nim


```nim
import math
randomize()

proc shuffle[T](x: var seq[T]) =
 for i in countdown(x.high, 0):
   let j = random(i + 1)
   swap(x[i], x[j])

type
  Suit = enum ♥, ♦, ♣, ♠

  Pip = enum c02, c03, c04, c05, c06, c07, c08, c09, c10, cQu, cKi, cAs

  Card = object
    pip: Pip
    suit: Suit

  Deck = object
    cards: seq[Card]

proc `$`(c: Card): string = $c.pip & $c.suit

proc initDeck(): Deck =
  result = Deck(cards: @[])
  for suit in Suit:
    for pip in Pip:
      result.cards.add Card(pip: pip, suit: suit)

proc `$`(d: Deck): string = $d.cards

proc shuffle(d: var Deck) = shuffle(d.cards)

proc deal(d: var Deck): Card =
  d.shuffle()
  d.cards.pop()

var d = initDeck()
echo "40 cards from a deck:"
for i in 0..4:
  for j in 0..7:
    stdout.write($d.deal(), " ")
  echo ""
echo "The remaining cards are: ", $d
```

Output:

```txt
40 cards from a deck:
c10♦ c06♥ cKi♠ c09♥ c06♣ c02♦ c10♣ c08♣ 
c03♥ cQu♠ c07♠ c02♣ c04♦ c09♠ c07♦ c09♦ 
c02♠ c06♠ cKi♥ cAs♦ cAs♥ c04♥ c05♣ c02♥ 
cQu♥ c03♠ cQu♣ c05♦ c08♥ c04♠ c10♥ cQu♦ 
c04♣ c03♣ c03♦ cAs♠ c10♠ cKi♣ cKi♦ c05♠ 
The remaining cards are: @[c07♣, cAs♣, c05♥, c06♦, c08♦, c08♠, c09♣, c07♥]
```



## OCaml

Straightforward implementation with algebraic types for the pips and suits, and lists of their values. A deck is an array of cards.

```ocaml
type pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | 
           Jack | Queen | King | Ace 
let pips = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
            Jack; Queen; King; Ace]
type suit = Diamonds | Spades | Hearts | Clubs
let suits = [Diamonds; Spades; Hearts; Clubs]
type card = pip * suit
let full_deck = Array.of_list (List.concat (List.map (fun pip -> List.map (fun suit -> (pip, suit)) suits) pips))
let shuffle deck =
  for i = Array.length deck - 1 downto 1 do
    let j = Random.int (i+1) in
    let temp = deck.(i) in
    deck.(i) <- deck.(j);
    deck.(j) <- temp
  done
```



## PARI/GP

Uses a global variable v to pass around the remaining cards in the deck. If used inside a function this would be a good case for a dynamically-scoped variable (<code>local</code>) rather than the typically-preferred lexical scoping of <code>my</code>.

```parigp
name(n)=Str(["A",2,3,4,5,6,7,8,9,10,"J","Q","K"][(n+3)>>2],["h","d","s","c"][n%4+1]);
newdeck()={
  v=vector(52,i,i);
};
deal()={
  my(n=name(v[1]));
  v=vecextract(v,2^#v-2);
  n
};
printdeck(){
  apply(name,v)
};
shuffle()={
  forstep(n=#v,2,-1,
    my(i=random(n)+1,t=v[i]);
    v[i]=v[n];
    v[n]=t
  );
  v
};
```



## Pascal

See [[Playing_cards#Delphi | Delphi]]


## Perl


```perl
package Playing_Card_Deck;

use strict;

@Playing_Card_Deck::suits = qw
   [Diamonds Clubs Hearts Spades];
@Playing_Card_Deck::pips = qw
   [Two Three Four Five Six Seven Eight Nine Ten
    Jack King Queen Ace];
# I choose to fully qualify these names rather than declare them
# with "our" to keep them from escaping into the scope of other
# packages in the same file. Another possible solution is to use
# "our" or "my", but to enclose this entire package in a bare block.

sub new
# Creates a new deck-object. The cards are initially neatly ordered.
 {my $invocant = shift;
  my $class = ref($invocant) || $invocant;
  my @cards = ();
  foreach my $suit (@Playing_Card_Deck::suits)
     {foreach my $pip (@Playing_Card_Deck::pips)
         {push(@cards, {suit => $suit, pip => $pip});}}
  return bless([@cards], $class);}

sub deal
# Removes the top card of the given deck and returns it as a hash
# with the keys "suit" and "pip".
 {return %{ shift( @{shift(@_)} ) };}

sub shuffle
# Randomly permutes the cards in the deck. It uses the algorithm
# described at:
# http://en.wikipedia.org/wiki/Fisher-Yates_shuffle#The_modern_algorithm
 {our @deck; local *deck = shift;
    # @deck is now an alias of the invocant's referent.
  for (my $n = $#deck ; $n ; --$n)
     {my $k = int rand($n + 1);
      @deck[$k, $n] = @deck[$n, $k] if $k != $n;}}

sub print_cards
# Prints out a description of every card in the deck, in order.
 {print "$_->{pip} of $_->{suit}\n" foreach @{shift(@_)};}
```

Some examples of use:

```perl
my $deck = new Playing_Card_Deck;
$deck->shuffle;
my %card = $deck->deal;
print uc("$card{pip} OF $card{suit}\n");
$deck->print_cards;
```

This creates a new deck, shuffles it, removes the top card, prints out that card's name in all caps, and then prints the rest of the deck.


## Perl 6

{{Works with|rakudo|2016.08}}

```perl6>enum Pip <A 2 3 4 5 6 7 8 9 10 J Q K
;
enum Suit <♦ ♣ ♥ ♠>;
 
class Card {
    has Pip $.pip;
    has Suit $.suit;
 
    method Str { $!pip ~ $!suit }
}
 
class Deck {
    has Card @.cards = pick *,
            map { Card.new(:$^pip, :$^suit) }, flat (Pip.pick(*) X Suit.pick(*));
 
    method shuffle { @!cards .= pick: * }
 
    method deal { shift @!cards }
 
    method Str  { ~@!cards }
    method gist { ~@!cards }
}

my Deck $d = Deck.new;
say "Deck: $d";

my $top = $d.deal;
say "Top card: $top";

$d.shuffle;
say "Deck, re-shuffled: ", $d;
```

{{out}}

```txt
Deck: 3♦ J♦ 4♥ 7♠ 7♣ 7♥ 9♣ K♥ 6♠ 2♦ 3♠ Q♥ 8♥ 2♥ J♥ 5♥ 8♦ 8♣ 6♦ 7♦ 5♦ 2♣ 4♦ 8♠ 9♥ 4♣ 3♥ K♠ 2♠ 5♣ Q♣ Q♦ K♦ 4♠ 9♦ Q♠ 5♠ 6♥ J♣ J♠ K♣ 9♠ 3♣ 6♣
Top card: 3♦
Deck, re-shuffled: K♦ 4♣ J♠ 2♥ J♥ K♣ 6♣ 5♠ 3♥ 6♦ 5♦ 4♠ J♣ 4♦ 6♥ K♥ 7♥ 7♦ 2♦ 4♥ 6♠ 7♣ 9♦ 3♣ 3♠ 2♣ 2♠ 8♦ 5♣ 9♠ 5♥ J♦ 9♥ Q♦ Q♣ Q♥ Q♠ 8♥ 8♠ K♠ 9♣ 8♣ 7♠
```



## PHP

{{works with|PHP|5.3+}}
Implementation:

```php
class Card
{
    // if unable to save as UTF-8, use other, non-UTF-8, symbols here
    protected static $suits = array( '♠', '♥', '♦', '♣' );

    protected static $pips = array( '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A' );

    protected $suit;

    protected $suitOrder;

    protected $pip;

    protected $pipOrder;

    protected $order;

    public function __construct( $suit, $pip )
    {
        if( !in_array( $suit, self::$suits ) )
        {
            throw new InvalidArgumentException( 'Invalid suit' );
        }
        if( !in_array( $pip, self::$pips ) )
        {
            throw new InvalidArgumentException( 'Invalid pip' );
        }

        $this->suit = $suit;
        $this->pip = $pip;
    }

    public function getSuit()
    {
        return $this->suit;
    }

    public function getPip()
    {
        return $this->pip;
    }

    public function getSuitOrder()
    {
        // lazy evaluate suit order
        if( !isset( $this->suitOrder ) )
        {
            // cache suit order
            $this->suitOrder = array_search( $this->suit, self::$suits );
        }

        return $this->suitOrder;
    }

    public function getPipOrder()
    {
        // lazy evaluate pip order
        if( !isset( $this->pipOrder ) )
        {
            // cache pip order
            $this->pipOrder = array_search( $this->pip, self::$pips );
        }

        return $this->pipOrder;
    }

    public function getOrder()
    {
        // lazy evaluate order
        if( !isset( $this->order ) )
        {
            $suitOrder = $this->getSuitOrder();
            $pipOrder = $this->getPipOrder();
            // cache order
            $this->order = $pipOrder * count( self::$suits ) + $suitOrder;
        }

        return $this->order;
    }

    public function compareSuit( Card $other )
    {
        return $this->getSuitOrder() - $other->getSuitOrder();
    }

    public function comparePip( Card $other )
    {
        return $this->getPipOrder() - $other->getPipOrder();
    }

    public function compare( Card $other )
    {
        return $this->getOrder() - $other->getOrder();
    }

    public function __toString()
    {
        return $this->suit . $this->pip;
    }

    public static function getSuits()
    {
        return self::$suits;
    }

    public static function getPips()
    {
        return self::$pips;
    }
}

class CardCollection
    implements Countable, Iterator
{
    protected $cards = array();

    protected function __construct( array $cards = array() )
    {
        foreach( $cards as $card )
        {
            $this->addCard( $card );
        }
    }

    /**
      * Countable::count() implementation
      */
    public function count()
    {
        return count( $this->cards );
    }

    /**
      * Iterator::key() implementation
      */
    public function key()
    {
        return key( $this->cards );
    }

    /**
      * Iterator::valid() implementation
      */
    public function valid()
    {
        return null !== $this->key();
    }

    /**
      * Iterator::next() implementation
      */
    public function next()
    {
        next( $this->cards );
    }

    /**
      * Iterator::current() implementation
      */
    public function current()
    {
        return current( $this->cards );
    }

    /**
      * Iterator::rewind() implementation
      */
    public function rewind()
    {
        reset( $this->cards );
    }

    public function sort( $comparer = null )
    {
        $comparer = $comparer ?: function( $a, $b ) {
            return $a->compare( $b );
        };

        if( !is_callable( $comparer ) )
        {
            throw new InvalidArgumentException( 'Invalid comparer; comparer should be callable' );
        }

        usort( $this->cards, $comparer );
        return $this;
    }

    public function toString()
    {
        return implode( ' ', $this->cards );
    }

    public function __toString()
    {
        return $this->toString();
    }

    protected function addCard( Card $card )
    {
        if( in_array( $card, $this->cards ) )
        {
            throw new DomainException( 'Card is already present in this collection' );
        }

        $this->cards[] = $card;
    }
}

class Deck
    extends CardCollection
{
    public function __construct( $shuffled = false )
    {
        foreach( Card::getSuits() as $suit )
        {
            foreach( Card::getPips() as $pip )
            {
                $this->addCard( new Card( $suit, $pip ) );
            }
        }

        if( $shuffled )
        {
            $this->shuffle();
        }
    }

    public function deal( $amount = 1, CardCollection $cardCollection = null )
    {
        if( !is_int( $amount ) || $amount < 1 )
        {
            throw new InvalidArgumentException( 'Invalid amount; amount should be an integer, larger than 0' );
        }

        if( $amount > count( $this->cards ) )
        {
            throw new RangeException( 'Invalid amount; requested amount is larger than the amount of available cards' );
        }

        $cards = array_splice( $this->cards, 0, $amount );

        $cardCollection = $cardCollection ?: new CardCollection;

        foreach( $cards as $card )
        {
            $cardCollection->addCard( $card );
        }

        return $cardCollection;
    }

    public function shuffle()
    {
        shuffle( $this->cards );
    }
}

class Hand
    extends CardCollection
{
    // override CardCollection __constructor
    // to allow public instantiation
    // but disallow instantiation with cards
    public function __construct() {}

    public function play( $position )
    {
        if( !isset( $this->cards[ $position ] ) )
        {
            throw new OutOfBoundsException( 'Invalid position; position is not present in this hand' );
        }

        $result = array_splice( $this->cards, $position, 1 );
        return $result[ 0 ];
    }
}
```


Usage:

```php
// if viewing in a browser, lets output a text/plain header with utf-8 charset
header( 'Content-Type: text/plain; charset=utf-8' );

// create a new Deck
$deck = new Deck();

// show the cards in the new deck
echo count( $deck ) . ' cards in the new deck: ' . PHP_EOL . $deck . PHP_EOL;

// sort the deck, default sort
$deck->sort();

// show the cards in the sorted deck
echo PHP_EOL . count( $deck ) . ' cards in the new deck, default sort: ' . PHP_EOL . $deck . PHP_EOL;

// sort the deck, custom sort
$deck->sort( function( $a, $b ) {
    return $a->compareSuit( $b ) ?: $a->comparePip( $b );
} );

// show the cards in the sorted deck
echo PHP_EOL . count( $deck ) . ' cards in the new deck, custom sort: ' . PHP_EOL . $deck . PHP_EOL;

// shuffle the deck
$deck->shuffle();

// show the cards in the shuffled deck
echo PHP_EOL . count( $deck ) . ' cards in the new deck, shuffled: ' . PHP_EOL . $deck . PHP_EOL . PHP_EOL;

// intialize four player hands
$players = array(
    new Hand,
    new Hand,
    new Hand,
    new Hand
);

// three deal rounds, with amounts: 2, 2, 3
foreach( array( 2, 2, 3 ) as $amount )
{
    // deal this rounds amount to player
    foreach( $players as $hand )
    {
        $deck->deal( $amount, $hand );
    }
}

foreach( $players as $p => $hand )
{
    // sort player cards, default sort
    $hand->sort();
    // show player cards
    echo 'Player ' . ( $p + 1 ) . ' got dealt the following ' . count( $hand ) . ' cards (sorted): ' . $hand . PHP_EOL;
}

// show the remaining cards in the deck
echo PHP_EOL . count( $deck ) . ' cards remaining in the deck: ' . PHP_EOL . $deck . PHP_EOL;
```


This will output something like:

```txt
52 cards in the new deck: 
♠2 ♠3 ♠4 ♠5 ♠6 ♠7 ♠8 ♠9 ♠T ♠J ♠Q ♠K ♠A ♥2 ♥3 ♥4 ♥5 ♥6 ♥7 ♥8 ♥9 ♥T ♥J ♥Q ♥K ♥A ♦2 ♦3 ♦4 ♦5 ♦6 ♦7 ♦8 ♦9 ♦T ♦J ♦Q ♦K ♦A ♣2 ♣3 ♣4 ♣5 ♣6 ♣7 ♣8 ♣9 ♣T ♣J ♣Q ♣K ♣A

52 cards in the new deck, default sort: 
♠2 ♥2 ♦2 ♣2 ♠3 ♥3 ♦3 ♣3 ♠4 ♥4 ♦4 ♣4 ♠5 ♥5 ♦5 ♣5 ♠6 ♥6 ♦6 ♣6 ♠7 ♥7 ♦7 ♣7 ♠8 ♥8 ♦8 ♣8 ♠9 ♥9 ♦9 ♣9 ♠T ♥T ♦T ♣T ♠J ♥J ♦J ♣J ♠Q ♥Q ♦Q ♣Q ♠K ♥K ♦K ♣K ♠A ♥A ♦A ♣A

52 cards in the new deck, custom sort: 
♠2 ♠3 ♠4 ♠5 ♠6 ♠7 ♠8 ♠9 ♠T ♠J ♠Q ♠K ♠A ♥2 ♥3 ♥4 ♥5 ♥6 ♥7 ♥8 ♥9 ♥T ♥J ♥Q ♥K ♥A ♦2 ♦3 ♦4 ♦5 ♦6 ♦7 ♦8 ♦9 ♦T ♦J ♦Q ♦K ♦A ♣2 ♣3 ♣4 ♣5 ♣6 ♣7 ♣8 ♣9 ♣T ♣J ♣Q ♣K ♣A

52 cards in the new deck, shuffled: 
♣7 ♠Q ♥9 ♠7 ♠J ♠9 ♣J ♣4 ♠8 ♥Q ♥2 ♦J ♠T ♣Q ♦A ♠K ♠2 ♣K ♦K ♦Q ♠3 ♣5 ♥5 ♠4 ♥3 ♦9 ♣2 ♦8 ♥A ♥6 ♦2 ♦5 ♥J ♣T ♦3 ♠5 ♠A ♣8 ♥8 ♥7 ♥K ♦7 ♥T ♦6 ♦T ♣6 ♣3 ♣A ♦4 ♥4 ♠6 ♣9

Player 1 got dealt the following 7 cards (sorted): ♠2 ♣7 ♠8 ♠Q ♥Q ♦K ♣K
Player 2 got dealt the following 7 cards (sorted): ♥2 ♠3 ♣5 ♠7 ♥9 ♦J ♦Q
Player 3 got dealt the following 7 cards (sorted): ♥3 ♠4 ♥5 ♠9 ♠T ♠J ♣Q
Player 4 got dealt the following 7 cards (sorted): ♣2 ♣4 ♦8 ♦9 ♣J ♠K ♦A

24 cards remaining in the deck: 
♥A ♥6 ♦2 ♦5 ♥J ♣T ♦3 ♠5 ♠A ♣8 ♥8 ♥7 ♥K ♦7 ♥T ♦6 ♦T ♣6 ♣3 ♣A ♦4 ♥4 ♠6 ♣9
```



## Phix

Includes both ascii/console and unicode/gui displays
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\Playing_cards.exw
--
function deal(sequence deck, integer nhands, integer ncards)
sequence hands = repeat({},nhands)
    for n=1 to ncards do
        for h=1 to nhands do
            hands[h] &= deck[1]
            deck = deck[2..$]
        end for
    end for
    return {deck,hands}
end function

--console:
procedure show_cards(sequence s)
    for i=1 to length(s) do
        integer c = s[i]-1
        string sep = iff(mod(i,13)=0 or i=length(s)?"\n":" ")
        puts(1,"23456789TJQKA"[mod(c,13)+1]&"SHDC"[floor(c/13)+1]&sep)
    end for
end procedure

sequence deck, hands

procedure console_show()
    for i=1 to length(hands) do
        printf(1,"hand%d:\n",{i})
        show_cards(sort(hands[i]))
    end for
    printf(1,"remaining cards(%d):\n",{length(deck)})
    show_cards(deck)
end procedure

--GUI:
function cards_to_utf8(sequence s)
sequence utf32 = {}
    for i=1 to length(s) do
        integer c = s[i]
        integer pip = mod(c,13)
        utf32 &= 0x1F0A1 + pip+(pip>10) + floor((c-1)/13)*#10
    end for
    return utf32_to_utf8(utf32)
end function

include pGUI.e

constant FONT = sprintf("FONT=\"Arial, %d\"",{92})

procedure gui_show()
    IupOpen()
    IupSetGlobal("UTF8MODE","YES")

    Ihandles lh = {}
    for i=1 to length(hands) do
        Ihandle l = IupLabel(sprintf("hand%d:",{i}))
        Ihandle h = IupLabel(cards_to_utf8(sort(hands[i])),FONT)
        lh &= l&h
    end for
    lh &= IupLabel("remaining cards:")
    lh &= IupLabel(cards_to_utf8(deck),FONT)

    Ihandle dlg = IupDialog(IupVbox(lh))
    IupShow(dlg)
    IupMainLoop()
    IupClose()
end procedure

constant DECKSIZE=52
deck = shuffle(tagset(DECKSIZE))
show_cards(deck)
{deck,hands} = deal(deck,4,7)
console_show()
gui_show()
```

{{out}}
(console)

```txt

JC QC 6C 3D QS 7C 7D KS 2S AH TD TS 3C
QH 5H 4S KD 9S 8H QD 3S TC 2H 6S 9C 3H
7S 5S 8C 2D 5D 2C 4H 8S 9D JS 4D 8D JD
KH AS 6D AC JH 4C 6H KC TH 9H 5C AD 7H
hand1:
2S 3S QS KD 3C 9C JC
hand2:
9S 3H QH AH 7C TC QC
hand3:
7S 2H 5H 8H 7D TD 6C
hand4:
4S 5S 6S TS KS 3D QD
remaining cards(24):
8C 2D 5D 2C 4H 8S 9D JS 4D 8D JD KH AS
6D AC JH 4C 6H KC TH 9H 5C AD 7H

```



## PicoLisp

{{trans|Common Lisp}}

```PicoLisp
(de *Suits
   Club Diamond Heart Spade )

(de *Pips
   Ace 2 3 4 5 6 7 8 9 10 Jack Queen King )

(de mkDeck ()
   (mapcan
      '((Pip) (mapcar cons *Suits (circ Pip)))
      *Pips ) )

(de shuffle (Lst)
   (by '(NIL (rand)) sort Lst) )
```



## Prolog


{{works with|SWI Prolog|4.8.0}}


```Prolog
/** <module> Cards

  A card is represented by the term "card(Pip, Suit)".
  A deck is represented internally as a list of cards.

  Usage:
  new_deck(D0), deck_shuffle(D0, D1), deck_deal(D1, C, D2).
*/
:- module(cards, [ new_deck/1,     % -Deck
                   deck_shuffle/2, % +Deck, -NewDeck
                   deck_deal/3,    % +Deck, -Card, -NewDeck
                   print_deck/1    % +Deck
                  ]).

%% new_deck(-Deck)
new_deck(Deck) :-
        Suits = [clubs, hearts, spades, diamonds],
        Pips = [2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace],
        setof(card(Pip, Suit), (member(Suit, Suits), member(Pip, Pips)), Deck).

%% deck_shuffle(+Deck, -NewDeck)
deck_shuffle(Deck, NewDeck) :-
        length(Deck, NumCards),
        findall(X, (between(1, NumCards, _I), X is random(1000)), Ord),
        pairs_keys_values(Pairs, Ord, Deck),
        keysort(Pairs, OrdPairs),
        pairs_values(OrdPairs, NewDeck).

%% deck_deal(+Deck, -Card, -NewDeck)
deck_deal([Card|Cards], Card, Cards).

%% print_deck(+Deck)
print_deck(Deck) :-
        maplist(print_card, Deck).

% print_card(+Card)
print_card(card(Pip, Suit)) :-
        format('~a of ~a~n', [Pip, Suit]).

```



## PureBasic

This approach keeps track of the cards in an abbrieviated form but allows them to be expanded to a more wordy form when they are dealt or shown.

```PureBasic
#MaxCards = 52 ;Max Cards in a deck
Structure card
  pip.s
  suit.s
EndStructure

Structure _membersDeckClass
  *vtable.i 
  size.i ;zero based count of cards present
  cards.card[#MaxCards] ;deck content 
EndStructure

Interface deckObject
  Init()
  shuffle()
  deal.s(isAbbr = #True)
  show(isAbbr = #True)
EndInterface 

Procedure.s _formatCardInfo(*card.card, isAbbr = #True)
  ;isAbbr determines if the card information is abbrieviated to 2 characters
  Static pips.s = "2 3 4 5 6 7 8 9 10 Jack Queen King Ace"
  Static suits.s = "Diamonds Clubs Hearts Spades"
  Protected c.s
  
  If isAbbr
    c = *card\pip + *card\suit
  Else
    c = StringField(pips,FindString("23456789TJQKA", *card\pip, 1), " ") + " of "
    c + StringField(suits,FindString("DCHS", *card\suit, 1)," ")
  EndIf 
  ProcedureReturn c
EndProcedure

Procedure setInitialValues(*this._membersDeckClass)
  Protected i, c.s
  
  Restore cardDat
  For i = 0 To #MaxCards - 1
    Read.s c
    *this\cards[i]\pip = Left(c, 1)
    *this\cards[i]\suit = Right(c, 1)
  Next
EndProcedure

Procedure.s dealCard(*this._membersDeckClass, isAbbr)
  ;isAbbr is #True if the card dealt is abbrieviated to 2 characters
  Protected c.card
  If *this\size < 0
    ;deck is empty
    ProcedureReturn ""
  Else
    c = *this\cards[*this\size]
    *this\size - 1
    ProcedureReturn _formatCardInfo(@c, isAbbr)
  EndIf 
EndProcedure

Procedure showDeck(*this._membersDeckClass, isAbbr)
  ;isAbbr determines if cards are shown with 2 character abbrieviations
  Protected i
  
  For i = 0 To *this\size
    Print(_formatCardInfo(@*this\cards[i], isAbbr))
    If i <> *this\size: Print(", "): EndIf 
  Next
  PrintN("")
EndProcedure

Procedure shuffle(*this._membersDeckClass)
  ;works with decks of any size
  Protected w, i
  Dim shuffled.card(*this\size)
  
  For i = *this\size To 0 Step -1
    w = Random(i)
    shuffled(i) = *this\cards[w]
    If w <> i
      *this\cards[w] = *this\cards[i]
    EndIf
    
  Next
  
  For i = 0 To *this\size
    *this\cards[i] = shuffled(i)
  Next
EndProcedure

Procedure newDeck()
  Protected *newDeck._membersDeckClass = AllocateMemory(SizeOf(_membersDeckClass))
  If *newDeck
    *newDeck\vtable = ?vTable_deckClass
    *newDeck\size = #MaxCards - 1
    setInitialValues(*newDeck)
  EndIf
  ProcedureReturn *newDeck
EndProcedure

DataSection
  vTable_deckClass:
  Data.i @setInitialValues()
  Data.i @shuffle()
  Data.i @dealCard()
  Data.i @showDeck()
  
  cardDat:
  Data.s "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "TD", "JD", "QD", "KD", "AD"
  Data.s "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "TC", "JC", "QC", "KC", "AC"
  Data.s "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "TH", "JH", "QH", "KH", "AH"
  Data.s "2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "TS", "JS", "QS", "KS", "AS"
EndDataSection

If OpenConsole()

  Define deck.deckObject = newDeck()
  Define deck2.deckObject = newDeck()

  If deck = 0 Or deck2 = 0
    PrintN("Unable to create decks")
    End
  EndIf

  deck\shuffle()
  PrintN("Dealt: " + deck\deal(#False))
  PrintN("Dealt: " + deck\deal(#False))
  PrintN("Dealt: " + deck\deal(#False))
  PrintN("Dealt: " + deck\deal(#False))
  deck\show()
  deck2\show()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Dealt: Queen of Hearts
Dealt: 6 of Hearts
Dealt: 6 of Diamonds
Dealt: 2 of Spades
5D, QS, 9C, 7D, 3S, TC, 6S, 8H, 3D, KH, 2D, 2H, 5S, 4D, 4H, KS, 6C, 9S, KD, JH,
3C, TD, 4C, AH, JD, 8S, 3H, AS, QC, 4S, 8D, AD, 5H, 9H, 7C, 8C, 9D, TH, 5C, JS,
7S, TS, QD, JC, 2C, AC, 7H, KC
2D, 3D, 4D, 5D, 6D, 7D, 8D, 9D, TD, JD, QD, KD, AD, 2C, 3C, 4C, 5C, 6C, 7C, 8C,
9C, TC, JC, QC, KC, AC, 2H, 3H, 4H, 5H, 6H, 7H, 8H, 9H, TH, JH, QH, KH, AH, 2S,
3S, 4S, 5S, 6S, 7S, 8S, 9S, TS, JS, QS, KS, AS
```



## Python

===Python 2.x, standalone===

```python
import random

class Card(object):
    suits = ("Clubs","Hearts","Spades","Diamonds")
    pips = ("2","3","4","5","6","7","8","9","10","Jack","Queen","King","Ace")

    def __init__(self, pip,suit):
        self.pip=pip
        self.suit=suit

    def __str__(self):
        return "%s %s"%(self.pip,self.suit)

class Deck(object):
    def __init__(self):
        self.deck = [Card(pip,suit) for suit in Card.suits for pip in Card.pips]

    def __str__(self):
        return "[%s]"%", ".join( (str(card) for card in self.deck))

    def shuffle(self):
        random.shuffle(self.deck)

    def deal(self):
        self.shuffle()  # Can't tell what is next from self.deck
        return self.deck.pop(0)
```


===Python 3: extending [[Poker hand analyser#Python]]===
Assume the code from [[Poker hand analyser#Python]] is in a file pokerhand.py and importable.

```python
from pokerhand import Card, suit, face
from itertools import product
from random import randrange

class Deck():
    def __init__(self):
        self.__deck = [Card(f, s) for f,s in product(face, suit)]
    
    def __repr__(self):
        return 'Deck of ' + ' '.join(repr(card) for card in self.__deck)
    
    def shuffle(self):
        pass
    
    def deal(self):
        return self.__deck.pop(randrange(len(self.__deck)))

if __name__ == '__main__':
    deck = Deck()
    print('40 cards from a deck:\n')
    for i in range(5):
        for j in range(8):
            print(deck.deal(), end=' ')
        print()
    print('\nThe remaining cards are a', deck)
```


{{out}}

```txt
40 cards from a deck:

7♥ k♥ 2♣ 3♦ 10♥ 10♠ 6♥ 9♠ 
9♣ k♦ 3♥ k♠ q♥ q♠ a♣ a♥ 
10♣ 5♣ 8♥ 4♣ 9♥ 8♠ a♠ 5♠ 
9♦ 4♥ j♦ j♣ 3♣ 7♠ 5♥ 2♥ 
6♠ q♣ 7♦ 3♠ 7♣ k♣ 10♦ 6♣ 

The remaining cards are a Deck of 2♦ 2♠ 4♦ 4♠ 5♦ 6♦ 8♦ 8♣ j♥ j♠ q♦ a♦
```



## R


```R
pips <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace")
suit <- c("Clubs", "Diamonds", "Hearts", "Spades")
# Create a deck
deck <- data.frame(pips=rep(pips, 4), suit=rep(suit, each=13))

shuffle <- function(deck)
{
   n <- nrow(deck)
   ord <- sample(seq_len(n), size=n)
   deck[ord,]
}

deal <- function(deck, fromtop=TRUE)
{
   index <- ifelse(fromtop, 1, nrow(deck))
   print(paste("Dealt the", deck[index, "pips"], "of", deck[index, "suit"]))
   deck[-index,]   
}

# Usage
deck <- shuffle(deck)
deck
deck <- deal(deck)
# While no-one is looking, sneakily deal a card from the bottom of the pack
deck <- deal(deck, FALSE)
```



## Racket



```Racket
#lang racket

;; suits:
(define suits '(club heart diamond spade))

;; ranks
(define ranks '(1 2 3 4 5 6 7 8 9 10 jack queen king))

;; cards 
(define cards
  (for*/list ([suit suits] [rank ranks])
    (list suit rank)))

;; a deck is a box containing a list of cards.
(define (new-deck)
  (box cards))

;; shuffle the cards in a deck
(define (deck-shuffle deck)
  (set-box! deck (shuffle (unbox deck))))

;; deal a card from tA 2 3 4 5 6 7 8 9 10 J Q K>;
enum Suit he deck:
(define (deck-deal deck)
  (begin0 (first (unbox deck))
          (set-box! deck (rest (unbox deck)))))


;; TRY IT OUT:
(define my-deck (new-deck))
(deck-shuffle my-deck)
(deck-deal my-deck)
(deck-deal my-deck)
(length (unbox my-deck))
```


{{out}}

```txt
Welcome to DrRacket, version 5.3.3.5--2013-02-20(5eddac74/d) [3m].
Language: racket; memory limit: 512 MB.
'(heart 8)
'(diamond king)
50
> 
```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* 1) Build ordered Card deck
* 2) Create shuffled stack
* 3) Deal 5 cards to 4 players each
* 4) show what cards have been dealt and what's left on the stack
* 05.07.2012 Walter Pachl
**********************************************************************/
colors='S H C D'
ranks ='A 2 3 4 5 6 7 8 9 T J Q K'
i=0
cards=''
ss=''
Do c=1 To 4
  Do r=1 To 13
    i=i+1
    card.i=word(colors,c)word(ranks,r)
    cards=cards card.i
    End
  End
n=52                                   /* number of cards on deck    */
Do si=1 To 51                          /* pick 51 cards              */
  x=random(0,n-1)+1                    /* take card x (in 1...n)     */
  s.si=card.x                          /* card on shuffled stack     */
  ss=ss s.si                           /* string of shuffled stack   */
  card.x=card.n                        /* replace card taken         */
  n=n-1                                /* decrement nr of cards      */
  End
s.52=card.1                            /* pick the last card left    */
ss=ss s.52                             /* add it to the string       */
Say 'Ordered deck:'
Say '  'subword(cards,1,26)
Say '  'subword(cards,27,52)
Say 'Shuffled stack:'
Say '  'subword(ss,1,26)
Say '  'subword(ss,27,52)
si=52
deck.=''
Do ci=1 To 5                           /* 5 cards each               */
  Do pli=1 To 4                        /* 4 players                  */
    deck.pli.ci=s.si                   /* take top of shuffled deck  */
    si=si-1                            /* decrement number           */
    deck.pli=deck.pli deck.pli.ci      /* pli's cards as string      */
    End
  End
Do pli=1 To 4                          /* show the 4 dealt ...       */
  Say pli':' deck.pli
  End
Say 'Left on shuffled stack:'
Say '  'subword(ss,1,26)               /* and what's left on stack   */
Say '  'subword(ss,27,6)
```

Output:

```txt

Ordered deck:
  SA S2 S3 S4 S5 S6 S7 S8 S9 ST SJ SQ SK HA H2 H3 H4 H5 H6 H7 H8 H9 HT HJ HQ HK
  CA C2 C3 C4 C5 C6 C7 C8 C9 CT CJ CQ CK DA D2 D3 D4 D5 D6 D7 D8 D9 DT DJ DQ DK
Shuffled stack:
  DJ D7 C6 DA HJ CJ D8 SA D2 DK C5 HA H9 SK ST DQ C3 SQ D5 CQ D3 S7 C9 HT CA CK
  S5 H3 S4 H5 S2 C2 HQ H2 S6 H6 H7 H8 S3 D4 D6 D9 HK CT C8 DT S8 SJ C7 C4 H4 S9
1:  S9 SJ CT D4 H6
2:  H4 S8 HK S3 S6
3:  C4 DT D9 H8 H2
4:  C7 C8 D6 H7 HQ
Left on shuffled stack:
  DJ D7 C6 DA HJ CJ D8 SA D2 DK C5 HA H9 SK ST DQ C3 SQ D5 CQ D3 S7 C9 HT CA CK
  S5 H3 S4 H5 S2 C2

```



### version 2

A check is made to see if ASCII characters can be used to display the suits   (if using an ASCII machine).

```rexx
/*REXX pgm shows a method to build/shuffle/deal 5 cards (using a 52─card deck)──►4 hands*/
box = build();   say ' box of cards:'   box      /*a brand new standard box of 52 cards.*/
deck= mix();     say 'shuffled deck:'   deck     /*obtain a randomly shuffled deck.     */
call deal  5, 4                                  /* ◄───────────────── 5 cards, 4 hands.*/
say;    say;     say right('[north]'   hand.1, 60)
        say;     say '      [west]'    hand.4             right('[east]'   hand.2, 60)
        say;     say right('[south]'   hand.3, 60)
say;    say;     say;    say 'remainder of deck: '        deck
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
build: _=;    ranks= "A 2 3 4 5 6 7 8 9 10 J Q K"                             /*ranks.  */
       if 5=='f5'x  then suits= "h d c s"                                     /*EBCDIC? */
                    else suits= "♥ ♦ ♣ ♠"                                     /*ASCII.  */
          do    s=1  for words(suits);    $=   word(suits, s)
             do r=1  for words(ranks);    _= _ word(ranks, r)$   /*append a suit to rank*/
             end   /*r*/
          end      /*s*/;                 return _               /*jokers are not used. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
deal: parse arg #cards, hands;            hand.=      /*initially,  nullify all hands.  */
          do   #cards                                 /*deal a hand to all the players. */
             do player=1  for hands                   /*deal some cards to the players. */
             hand.player= hand.player  word(deck, 1)  /*deal the top card to a player.  */
             deck= subword(deck, 2)                   /*diminish deck, elide one card.  */
             end   /*player*/
          end      /*#cards*/;            return
/*──────────────────────────────────────────────────────────────────────────────────────*/
mix: @=;      _=box;      #cards= words(_)            /*define three REXX variables.    */
          do mixer=1  for #cards                      /*shuffle all the cards in deck.  */
          ?= random(1, #cards + 1 - mixer)            /*each shuffle, random# decreases.*/
          @= @  word(_, ?)                            /*shuffled deck, 1 card at a time.*/
          _= delword(_, ?, 1)                         /*elide just─chosen card from deck*/
          end   /*mixer*/;                return @
```

{{out|output|text=  when using the internal default values of four hands holding five cards:}}

```txt

 box of cards:  A♥ 2♥ 3♥ 4♥ 5♥ 6♥ 7♥ 8♥ 9♥ 10♥ J♥ Q♥ K♥ A♦ 2♦ 3♦ 4♦ 5♦ 6♦ 7♦ 8♦ 9♦ 10♦ J♦ Q♦ K♦ A♣ 2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ 10♣ J♣ Q♣ K♣ A♠ 2♠ 3♠ 4♠ 5♠ 6♠ 7♠ 8♠ 9♠ 10♠ J♠ Q♠ K♠
shuffled deck:  2♠ 4♣ K♠ 3♠ 3♥ 7♠ 3♦ 10♦ 8♥ 5♥ 8♦ 5♠ 6♠ 10♠ A♣ 7♦ 9♦ J♠ J♣ A♥ 10♣ 2♣ A♠ 5♦ 9♥ 10♥ Q♠ 6♦ 9♠ K♣ 6♣ A♦ 4♥ 7♣ 4♠ 3♣ Q♥ K♥ J♦ 8♠ 7♥ K♦ 5♣ 9♣ 4♦ Q♣ J♥ 2♦ 8♣ Q♦ 6♥ 2♥


                                     [north]  2♠ 3♥ 8♥ 6♠ 9♦

      [west]  3♠ 10♦ 5♠ 7♦ A♥                                      [east]  4♣ 7♠ 5♥ 10♠ J♠

                                     [south]  K♠ 3♦ 8♦ A♣ J♣



remainder of deck:  10♣ 2♣ A♠ 5♦ 9♥ 10♥ Q♠ 6♦ 9♠ K♣ 6♣ A♦ 4♥ 7♣ 4♠ 3♣ Q♥ K♥ J♦ 8♠ 7♥ K♦ 5♣ 9♣ 4♦ Q♣ J♥ 2♦ 8♣ Q♦ 6♥ 2♥

```



## Ring


The Cards game, Screen shot : http://ring-lang.sourceforge.net/doc/_images/ringqt_shot48.jpg


```ring

Load "guilib.ring"

nScale = 1

app1 = new qApp

mypic = new QPixmap("cards.jpg")

mypic2 = mypic.copy(0,(124*4)+1,79,124)
Player1EatPic = mypic.copy(80,(124*4)+1,79,124)
Player2EatPic= mypic.copy(160,(124*4)+1,79,124)

aMyCards = []
aMyValues = []
for x1 = 0 to 3
        for y1 = 0 to 12
          temppic = mypic.copy((79*y1)+1,(124*x1)+1,79,124)
                  aMyCards + temppic
                  aMyValues + (y1+1)
        next
next

nPlayer1Score = 0   nPlayer2Score=0

do
        Page1 = new Game
        Page1.Start()
again Page1.lnewgame

mypic.delete()
mypic2.delete()
Player1EatPic.delete()
Player2EatPic.delete()

for t in aMyCards
          t.delete()
next

func gui_setbtnpixmap pBtn,pPixmap
        pBtn {
                setIcon(new qicon(pPixmap.scaled(width(),height(),0,0)))
                setIconSize(new QSize(width(),height()))
        }

Class Game

        nCardsCount = 10
        win1 layout1 label1 label2 layout2 layout3 aBtns aBtns2
        aCards nRole=1 aStatus = list(nCardsCount) aStatus2 = aStatus
        aValues        aStatusValues = aStatus  aStatusValues2 = aStatus
        Player1EatPic   Player2EatPic
        lnewgame = false
        nDelayEat = 0.5
        nDelayNewGame = 1

        func start

                win1 = new qWidget() {
                        setwindowtitle("Five")
                        setstylesheet("background-color: White")
                        showfullscreen()
                }

                layout1 = new qvboxlayout()

                label1 = new qlabel(win1) {
                        settext("Player (1) - Score : " + nPlayer1Score)
                        setalignment(Qt_AlignHCenter | Qt_AlignVCenter)
                        setstylesheet("color: White; background-color: Purple;
                                         font-size:20pt")
                        setfixedheight(200)
                }

                closebtn = new qpushbutton(win1)  {
                        settext("Close Application")
                        setstylesheet("font-size: 18px ; color : white ;
                                         background-color: black ;")
                        setclickevent("Page1.win1.close()")
                }

                aCards = aMyCards
                aValues = aMyValues

                layout2 = new qhboxlayout()

                aBtns = []

                for x = 1 to nCardsCount
                        aBtns + new qpushbutton(win1)
                        aBtns[x].setfixedwidth(79*nScale)
                        aBtns[x].setfixedheight(124*nScale)
                        gui_setbtnpixmap(aBtns[x],mypic2)
                        layout2.addwidget(aBtns[x])
                        aBtns[x].setclickevent("Page1.Player1click("+x+")")
                next

                layout1.addwidget(label1)
                layout1.addlayout(layout2)

                label2 = new qlabel(win1) {
                        settext("Player (2) - Score : " + nPlayer2Score)
                        setalignment(Qt_AlignHCenter | Qt_AlignVCenter)
                        setstylesheet("color: white; background-color: red;
                                         font-size:20pt")
                        setfixedheight(200)
                }

                layout3 = new qhboxlayout()

                aBtns2 = []
                for x = 1 to nCardsCount
                        aBtns2 + new qpushbutton(win1)
                        aBtns2[x].setfixedwidth(79*nScale)
                        aBtns2[x].setfixedheight(124*nScale)
                        gui_setbtnpixmap(aBtns2[x],mypic2)
                        layout3.addwidget(aBtns2[x])
                        aBtns2[x].setclickevent("Page1.Player2click("+x+")")
                next

                layout1.addwidget(label2)
                layout1.addlayout(layout3)
                layout1.addwidget(closebtn)

                win1.setlayout(layout1)

                app1.exec()

        Func Player1Click x
                if nRole = 1 and aStatus[x] = 0
                        nPos = ((random(100)+clock())%(len(aCards)-1)) + 1
                        gui_setbtnpixmap(aBtns[x],aCards[nPos])
                        del(aCards,nPos)
                        nRole = 2
                        aStatus[x] = 1
                        aStatusValues[x] = aValues[nPos]
                        del(aValues,nPos)
                        Player1Eat(x,aStatusValues[x])
                        checknewgame()
                ok

        Func Player2Click x
                if nRole = 2 and aStatus2[x] = 0
                        nPos = ((random(100)+clock())%(len(aCards)-1)) + 1
                        gui_setbtnpixmap(aBtns2[x],aCards[nPos])
                        del(aCards,nPos)
                        nRole = 1
                        aStatus2[x] = 1
                        aStatusValues2[x] = aValues[nPos]
                        del(aValues,nPos)
                        Player2Eat(x,aStatusValues2[x])
                        checknewgame()
                ok

        Func Player1Eat nPos,nValue

                 app1.processEvents()

                 delay(nDelayEat)
                 lEat = false
                 for x = 1 to nCardsCount
                         if aStatus2[x] = 1 and (aStatusValues2[x] = nValue or nValue=5)
                                aStatus2[x] = 2
                                gui_setbtnpixmap(aBtns2[x],Player1EatPic)
                                lEat = True
                                nPlayer1Score++
                         ok
                         if (x != nPos) and (aStatus[x] = 1) and
                                (aStatusValues[x] = nValue or nValue=5)
                                aStatus[x] = 2
                                gui_setbtnpixmap(aBtns[x],Player1EatPic)
                                lEat = True
                                nPlayer1Score++
                         ok
                 next
                 if lEat
                                nPlayer1Score++
                                gui_setbtnpixmap(aBtns[nPos],Player1EatPic)
                                aStatus[nPos] = 2
                                label1.settext("Player (1) - Score : " + nPlayer1Score)
                 ok

        Func Player2Eat nPos,nValue

                 app1.processEvents()

                 delay(nDelayEat)
                 lEat = false
                 for x = 1 to  nCardsCount
                         if aStatus[x] = 1 and (aStatusValues[x] = nValue or nValue = 5)
                                aStatus[x] = 2
                                gui_setbtnpixmap(aBtns[x],Player2EatPic)
                                lEat = True
                                nPlayer2Score++
                         ok

                         if (x != nPos) and (aStatus2[x] = 1) and
                                (aStatusValues2[x] = nValue or nValue=5 )
                                aStatus2[x] = 2
                                gui_setbtnpixmap(aBtns2[x],Player2EatPic)
                                lEat = True
                                nPlayer2Score++
                         ok
                 next
                 if lEat
                                nPlayer2Score++
                                gui_setbtnpixmap(aBtns2[nPos],Player2EatPic)
                                aStatus2[nPos] = 2
                                label2.settext("Player (2) - Score : " + nPlayer2Score)
                 ok

        Func checknewgame
                if isnewgame()
                                  lnewgame = true

                                  if nPlayer1Score > nPlayer2Score
                                         label1.settext("Player (1) Wins!!!")
                                  ok
                                  if nPlayer2Score > nPlayer1Score
                                         label2.settext("Player (2) Wins!!!")
                                  ok

                                  app1.processEvents()
                                  delay(nDelayNewGame)

                                  win1.delete()
                                  app1.quit()
                ok

        Func isnewgame
                for t in aStatus
                        if t = 0
                                return false
                        ok
                next
                for t in aStatus2
                        if t = 0
                                return false
                        ok
                next
                return true

        Func delay x
        nTime = x * 1000
        oTest = new qTest
        oTest.qsleep(nTime)

```



## Ruby

{{trans|Python}}

{{works with|Ruby|2.0.0+}}

```ruby
class Card
  # class constants
  SUITS = %i[ Clubs Hearts Spades Diamonds ]
  PIPS  = %i[ 2 3 4 5 6 7 8 9 10 Jack Queen King Ace ]
  
  # class variables (private)
  @@suit_value = Hash[ SUITS.each_with_index.to_a ]
  @@pip_value  = Hash[ PIPS.each_with_index.to_a ]
  
  attr_reader :pip, :suit
  
  def initialize(pip,suit)
    @pip = pip
    @suit = suit
  end
  
  def to_s
    "#{@pip} #{@suit}"
  end
  
  # allow sorting an array of Cards: first by suit, then by value
  def <=>(other)
    (@@suit_value[@suit] <=> @@suit_value[other.suit]).nonzero? or
    @@pip_value[@pip] <=> @@pip_value[other.pip]
  end
end

class Deck
  def initialize
    @deck = Card::SUITS.product(Card::PIPS).map{|suit,pip| Card.new(pip,suit)}
  end
  
  def to_s
    @deck.inspect
  end
  
  def shuffle!
    @deck.shuffle!
    self
  end
  
  def deal(*args)
    @deck.shift(*args)
  end
end

deck = Deck.new.shuffle!
puts card = deck.deal
hand = deck.deal(5)
puts hand.join(", ")
puts hand.sort.join(", ")
```


{{out}}

```txt
10 Clubs
8 Diamonds, Queen Clubs, 10 Hearts, 6 Diamonds, 4 Clubs
4 Clubs, Queen Clubs, 6 Diamonds, 8 Diamonds, 10 Hearts
```



## Run BASIC


```runbasic
suite$ = "C,D,H,S"                      ' Club,Diamond,Hart,Spaces
card$  = "A,2,3,4,5,6,7,8,9,T,J,Q,K"    ' Cards Ace to King

dim n(55)                               ' make ordered deck 
for i = 1 to 52                         '  of 52 cards
 n(i) 	= i
next i
 
for i = 1 to 52 * 3                     ' shuffle deck 3 times
  i1    = int(rnd(1)*52) + 1
  i2    = int(rnd(1)*52) + 1
  h2    = n(i1)
  n(i1) = n(i2)
  n(i2) = h2
next i

for hand = 1 to 4                       ' 4 hands 
for deal = 1 to 13			' deal each 13 cards
 card = card + 1                        ' next card in deck
    s    = (n(card) mod 4)  + 1         ' determine suite
    c    = (n(card) mod 13) + 1         ' determine card
    print word$(card$,c,",");word$(suite$,s,",");" ";  ' show the card
  next deal
print
next hand
```


```txt
TD 7S JD 7C 3H AS 6D QD KH 5H 2C QH 8C 
8H 5S 7D 2D 2H 4D KS JS 7H QC KC 9S TH 
JH 4H TS 3D AH 4C 6H 6C 6S 2S JC 5C AD 
KD QS 9D 9C 4S 9H 8D TC 8S 5D 3C 3S AC
```



## Rust

{{libheader|rand}}

```rust
extern crate rand;

use std::fmt;
use rand::Rng;
use Pip::*;
use Suit::*;

#[derive(Copy, Clone, Debug)]
enum Pip { Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King }

#[derive(Copy, Clone, Debug)]
enum Suit { Spades, Hearts, Diamonds, Clubs }

struct Card {
	pip: Pip,
	suit: Suit
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} of {:?}", self.pip, self.suit)
    }
}

struct Deck(Vec<Card>);

impl Deck {
    fn new() -> Deck {
        let mut cards:Vec<Card> = Vec::with_capacity(52);
        for &suit in &[Spades, Hearts, Diamonds, Clubs] {
            for &pip in &[Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King] {
                cards.push( Card{pip: pip, suit: suit} );
            }
        }
        Deck(cards)
    }

    fn deal(&mut self) -> Option<Card> {
        self.0.pop()
    }

    fn shuffle(&mut self) {
        rand::thread_rng().shuffle(&mut self.0)
    }
}

impl fmt::Display for Deck {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for card in self.0.iter() {
            writeln!(f, "{}", card);
        }
        write!(f, "")
    }
}

fn main() {
    let mut deck = Deck::new();
    deck.shuffle();
    //println!("{}", deck);
    for _ in 0..5 {
        println!("{}", deck.deal().unwrap());
    }
}
```

'''Sample output: 5 random cards'''

```txt
Jack of Diamonds
Nine of Hearts
Queen of Hearts
Six of Clubs
Five of Clubs
```



## Scala


```scala
import scala.annotation.tailrec
import scala.util.Random

object Pip extends Enumeration {
  type Pip = Value
  val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
}

object Suit extends Enumeration {
  type Suit = Value
  val Diamonds, Spades, Hearts, Clubs = Value
}

import Suit._
import Pip._
case class Card(suit:Suit, value:Pip){
  override def toString():String=value + " of " + suit
}

class Deck private(val cards:List[Card]) {
  def this()=this(for {s <- Suit.values.toList; v <- Pip.values} yield Card(s,v))
  def shuffle:Deck=new Deck(Random.shuffle(cards))
		
  def deal(n:Int=1):(Seq[Card], Deck)={
    @tailrec def loop(count:Int, c:Seq[Card], d:Deck):(Seq[Card], Deck)={
      if(count==0 || d.cards==Nil) (c,d)
      else {
        val card :: deck = d.cards
        loop(count-1, c :+ card, new Deck(deck))
      }
    }
    loop(n, Seq(), this)
  }
  override def toString():String="Deck: " + (cards mkString ", ")
}
```

Usage:

```scala
val deck=new Deck()
val deckShuffled:Deck=deck.shuffle

println(deck)
println(deckShuffled)

val (a, rest) = deckShuffled.deal()
val (b, rest2) = rest.deal()

println(a head)
println(b head)

val (cards, rest3) =deckShuffled deal 6
println(cards)
println(rest3)
```

Output:

```txt
Deck: Four of Diamonds, Two of Diamonds, Queen of Diamonds, Jack of Diamonds, Nine of Diamonds, Five of Diamonds, Seven of Diamonds, Ten of Diamonds, Six of Diamonds, Ace of Diamonds, King of Diamonds, Three of Diamonds, Eight of Diamonds, Ten of Spades, Seven of Spades, Eight of Spades, King of Spades, Ace of Spades, Three of Spades, Queen of Spades, Jack of Spades, Six of Spades, Five of Spades, Nine of Spades, Four of Spades, Two of Spades, Seven of Hearts, Queen of Hearts, Two of Hearts, Ten of Hearts, Eight of Hearts, Jack of Hearts, Four of Hearts, Nine of Hearts, Six of Hearts, King of Hearts, Five of Hearts, Three of Hearts, Ace of Hearts, King of Clubs, Jack of Clubs, Queen of Clubs, Six of Clubs, Three of Clubs, Two of Clubs, Eight of Clubs, Seven of Clubs, Ace of Clubs, Four of Clubs, Nine of Clubs, Five of Clubs, Ten of Clubs
Deck: Two of Clubs, Three of Diamonds, King of Clubs, Six of Hearts, Four of Spades, Nine of Clubs, Queen of Hearts, Six of Spades, Ace of Hearts, Jack of Diamonds, Jack of Hearts, Ten of Hearts, Two of Spades, Eight of Diamonds, Four of Hearts, Ace of Spades, Three of Hearts, King of Diamonds, King of Hearts, Five of Diamonds, Five of Spades, Queen of Diamonds, Seven of Spades, Eight of Clubs, Eight of Spades, Seven of Clubs, Nine of Spades, Eight of Hearts, Jack of Clubs, Ten of Clubs, Queen of Clubs, Ten of Diamonds, Seven of Hearts, Nine of Hearts, Two of Diamonds, Two of Hearts, Four of Diamonds, Five of Hearts, Three of Clubs, Three of Spades, Queen of Spades, Five of Clubs, Ace of Diamonds, Ace of Clubs, Jack of Spades, Six of Clubs, Six of Diamonds, King of Spades, Four of Clubs, Seven of Diamonds, Nine of Diamonds, Ten of Spades
Two of Clubs
Three of Diamonds
List(Two of Clubs, Three of Diamonds, King of Clubs, Six of Hearts, Four of Spades, Nine of Clubs)
Deck: Queen of Hearts, Six of Spades, Ace of Hearts, Jack of Diamonds, Jack of Hearts, Ten of Hearts, Two of Spades, Eight of Diamonds, Four of Hearts, Ace of Spades, Three of Hearts, King of Diamonds, King of Hearts, Five of Diamonds, Five of Spades, Queen of Diamonds, Seven of Spades, Eight of Clubs, Eight of Spades, Seven of Clubs, Nine of Spades, Eight of Hearts, Jack of Clubs, Ten of Clubs, Queen of Clubs, Ten of Diamonds, Seven of Hearts, Nine of Hearts, Two of Diamonds, Two of Hearts, Four of Diamonds, Five of Hearts, Three of Clubs, Three of Spades, Queen of Spades, Five of Clubs, Ace of Diamonds, Ace of Clubs, Jack of Spades, Six of Clubs, Six of Diamonds, King of Spades, Four of Clubs, Seven of Diamonds, Nine of Diamonds, Ten of Spades

```



## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

The procedure <code>shuffle</code> requires an appropriate procedure <code>random</code> to be 
defined. Some Scheme implementations provide this as an extension.

```scheme
(define ranks
  (quote (ace 2 3 4 5 6 7 8 9 10 jack queen king)))

(define suits
  (quote (clubs diamonds hearts spades)))

(define new-deck
  (apply append
         (map (lambda (suit)
                (map (lambda (rank)
                       (cons rank suit))
                     ranks))
              suits)))

(define (shuffle deck)
  (define (remove-card deck index)
    (if (zero? index)
        (cdr deck)
        (cons (car deck) (remove-card (cdr deck) (- index 1)))))
  (if (null? deck)
      (list)
      (let ((index (random (length deck))))
        (cons (list-ref deck index) (shuffle (remove-card deck index))))))

(define-syntax deal!
  (syntax-rules ()
    ((deal! deck hand)
     (begin (set! hand (cons (car deck) hand)) (set! deck (cdr deck))))))
```

Example:

```scheme
(define deck
  (shuffle new-deck))

(define hand
  (list))

(deal! deck hand)
(deal! deck hand)
(deal! deck hand)
(deal! deck hand)
(deal! deck hand)

(display hand)
(newline)
```

Sample output:

```txt
((jack . hearts) (5 . clubs) (9 . hearts) (7 . clubs) (6 . spades))
```



## Sidef

{{trans|Perl 6}}

```ruby>define Pip = <A 2 3 4 5 6 7 8 9 10 J Q K
;
define Suit = <♦ ♣ ♥ ♠>;

class Card(pip, suit) {
    method to_s { pip + suit }
}

class Deck(cards=[]) {

    method init {
        cards = gather {
            Pip.each { |p| Suit.each { |s| take(Card(p, s)) } }
        }
    }

    method shuffle {
        cards.shuffle!;
    }

    method deal { cards.shift };
    method to_s { cards.join(" ") };
}

var d = Deck();
say "Deck: #{d}";

var top = d.deal;
say "Top card: #{top}";

d.shuffle;
say "Deck, shuffled: #{d}";
```

{{out}}

```txt

Deck: A♦ A♣ A♥ A♠ 2♦ 2♣ 2♥ 2♠ 3♦ 3♣ 3♥ 3♠ 4♦ 4♣ 4♥ 4♠ 5♦ 5♣ 5♥ 5♠ 6♦ 6♣ 6♥ 6♠ 7♦ 7♣ 7♥ 7♠ 8♦ 8♣ 8♥ 8♠ 9♦ 9♣ 9♥ 9♠ 10♦ 10♣ 10♥ 10♠ J♦ J♣ J♥ J♠ Q♦ Q♣ Q♥ Q♠ K♦ K♣ K♥ K♠
Top card: A♦
Deck, shuffled: 10♠ 2♠ 3♠ Q♥ 3♣ A♠ 6♠ 6♣ 9♣ 6♦ Q♦ 8♣ 4♦ 7♠ 10♦ 3♥ 4♠ 7♥ 8♠ 10♥ 10♣ 9♦ 5♠ Q♠ A♥ 4♥ J♥ Q♣ 7♣ 2♥ 6♥ 8♥ 5♥ 7♦ J♠ 5♦ K♦ 3♦ J♣ 2♦ 5♣ K♥ 9♥ 2♣ 8♦ A♣ K♣ 9♠ K♠ J♦ 4♣

```



## Smalltalk


### Version 1

{{works with|GNU Smalltalk}}

```smalltalk
Object subclass: #Card
  instanceVariableNames: 'thePip theSuit'
  classVariableNames: 'pips suits'
  poolDictionaries: ''
  category: nil !

!Card class methods!
initialize
  suits ifNil: [ suits := 'clubs,hearts,spades,diamonds' subStrings: $, ].
  pips ifNil: [ pips := '2,3,4,5,6,7,8,9,10,jack,queen,king,ace' subStrings: $, ]
!
new
  | o |
  o := super new.
  ^o
!  
new: card
  | o |
  o := self new.
  o initWithPip: (card at: 1) andSuit: (card at: 2).
  ^o
!!

!Card class methods !
pips
  Card initialize.
  ^pips
!
suits
  Card initialize.
  ^suits
!!

!Card methods!
initWithPip: aPip andSuit: aSuit
  ( (pips includes: aPip asLowercase) &
    (suits includes: aSuit asLowercase) )
     ifTrue: [
          thePip := aPip copy.
          theSuit := aSuit copy
     ] ifFalse: [ 'Unknown pip or suit' displayOn: stderr .
                  Character nl displayOn: stderr ].
  ^self
!
asString
  ^('(%1,%2)' % { thePip . theSuit })
!
display
  self asString display
!
displayNl
  self display.
  Character nl display
!!


Object subclass: #Deck
  instanceVariableNames: 'deck'
  classVariableNames: ''
  poolDictionaries: ''
  category: nil !

!Deck class methods !
new
  |d|
  d := super new.
  d init.
  ^d
!!

!Deck methods !
init
   deck := OrderedCollection new.
   Card suits do: [ :suit |
     Card pips do: [ :pip |
         deck add: (Card new: { pip . suit })
     ]
   ]
!
deck
  ^deck
!
shuffle
  1 to: self deck size do: [ :i |
     |r2 o|
     r2 := Random between: 1 and: self deck size.
     o := self deck at: i.
     self deck at: i put: (self deck at: r2).
     self deck at: r2 put: o 
  ].
  ^self
!
display
  self deck do: [ :card |
     card displayNl
  ]
!
deal
  ^self deck removeFirst
!!

"create a deck, shuffle it, remove the first card and display it"
Deck new shuffle deal displayNl.
```


'''Note''': there's something odd with the class method for getting pips and suits; it's because I've not understood how to initialize class variables to certain values without the need to explicitly call a method to do so.


### Version 2

{{works with|GNU Smalltalk}}

```smalltalk
Object subclass: Deck [

    | cards |

    Deck class >> of: aCardClass
        [^self new
            initializeWith: aCardClass;
            yourself]

    initializeWith: aCardClass
        [cards := OrderedCollection from: aCardClass standardSet]

    displayOn: aStream
        [cards
            do: [:each | each displayOn: aStream]
            separatedBy: [aStream space]]

    shuffle
        [1 to: cards size - 1 do:
            [:a || b |
            b := Random between: a and: cards size.
            cards swap: a with: b]]

    deal
        [^cards removeLast]
]

Object subclass: Card [

    Card class >> standardSet
        [^#(
            '2d' '3d' '4d' '5d' '6d' '7d' '8d' '9d' 'Td' 'Jd' 'Qd' 'Kd' 'Ad'
            '2s' '3s' '4s' '5s' '6s' '7s' '8s' '9s' 'Ts' 'Js' 'Qs' 'Ks' 'As'
            '2h' '3h' '4h' '5h' '6h' '7h' '8h' '9h' 'Th' 'Jh' 'Qh' 'Kh' 'Ah'
            '2c' '3c' '4c' '5c' '6c' '7c' '8c' '9c' 'Tc' 'Jc' 'Qc' 'Kc' 'Ac'
        ) deepCopy]
]
```


The Card class should obviously be more intricate to fulfil the needs of a CardGame class. :-)

Use example:

```Smalltalk>st
 myDeck := Deck of: Card
a Deck
st> myDeck displayNl
2d 3d 4d 5d 6d 7d 8d 9d Td Jd Qd Kd Ad 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks As 2h 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh Ah 2c 3c 4c 5c 6c 7c 8c 9c Tc Jc Qc Kc Ac
st> myDeck shuffle
a Deck
st> myDeck displayNl
6c 7d Ac 4c 9s 2s Tc 9c Jh 3h Kh 7h 3s 5s 3d Kd Jc Qs As Qd 3c Kc Qh 2d 9h 4h 8c 7s Ah 9d Js 6h 8s 8h 5c 2c 4s 8d 5d Ts 4d Qc Td 7c 2h 5h 6s 6d Th Ks Jd Ad
st> myHand := OrderedCollection new
OrderedCollection ()
st> 5 timesRepeat: [myHand add: myDeck deal]
5
st> myHand
OrderedCollection ('Ad' 'Jd' 'Ks' 'Th' '6d' )
```



## Swift

{{works with|Swift 2.0}}

Enter this into a playground to see results

```swift

import Foundation

// extend any Indexed collection to be able to shuffle (see http://stackoverflow.com/questions/24026510/how-do-i-shuffle-an-array-in-swift)
extension CollectionType where Index == Int {
    /// Return a copy of `self` with its elements shuffled
    func shuffle() -> [Generator.Element] {
        var list = Array(self)
        list.shuffleInPlace()
        return list
    }
}

extension MutableCollectionType where Index == Int {
    /// Shuffle the elements of `self` in-place.
    mutating func shuffleInPlace() {
        // empty and single-element collections don't shuffle
        if count < 2 { return }
        
        for i in 0..<count - 1 {
            let j = Int(arc4random_uniform(UInt32(count - i))) + i
            guard i != j else { continue }
            swap(&self[i], &self[j])
        }
    }
}

// now the model structs
enum CardColor : Int {
    case Red
    case Black
}
extension CardColor : CustomStringConvertible {
    var description : String {
        switch self {
        case .Red:
            return "Red"
        case .Black:
            return "Black"
        }
    }
}

enum Suit : Int {
    case Hearts = 1
    case Diamonds
    case Spades
    case Clubs
    
    var color : CardColor {
        switch self {
        case .Hearts, .Diamonds:
            return .Red
        case .Spades, .Clubs:
            return .Black
        }
    }
}

enum Pip : Int {
    case Ace = 1
    case Two = 2
    case Three = 3
    case Four = 4
    case Five = 5
    case Six = 6
    case Seven = 7
    case Eight = 8
    case Nine = 9
    case Ten = 10
    case Jack = 11
    case Queen = 12
    case King = 13
}

struct Card {
    let pip : Pip
    let suit : Suit
    
    var isFaceCard : Bool {
        return pip.rawValue > 10
    }
    
    var color : CardColor {
        return suit.color
    }
}
extension Card : Equatable {}
func == (l:Card, r:Card) -> Bool {
    return l.pip == r.pip &&
            l.suit == r.suit
}
extension Card : CustomStringConvertible {
    var description : String {
        return "\(pip) of \(suit)"
    }
}


struct Deck {
    var cards : [Card]
    
    var count : Int {
        return cards.count
    }
    
    init(shuffling:Bool=true) {
        var startcards = [Card]()
        for suit in (Suit.Hearts.rawValue...Suit.Clubs.rawValue) {
            for pip in (Pip.Ace.rawValue...Pip.King.rawValue) {
                startcards.append(Card(pip: Pip(rawValue: pip)!, suit: Suit(rawValue: suit)!))
            }
        }
        cards = startcards
        
        if shuffling {
            shuffle()
        }
    }
    
    mutating func shuffle() {
        cards.shuffleInPlace()
    }
    
    mutating func deal() -> Card {
        let out = cards.removeFirst()
        return out
    }
    
}
extension Deck : CustomStringConvertible {
    var description : String {
        return "\(count) cards: \(cards.description)"
    }
}



// test some cards
let kh = Card(pip: .King, suit: .Hearts)
let ad = Card(pip: .Ace, suit: .Diamonds)
let tc = Card(pip: .Two, suit: .Clubs)
let fc = Card(pip: Pip(rawValue:4)!, suit: .Spades)


// create an unshuffled deck
var efg =  Deck(shuffling: false)


// create a shuffled deck and print its contents
var d = Deck()
print(d)

// deal three cards
d.deal()
d.deal()
d.deal()
d

// deal a couple more cards and check their color
let c = d.deal()
c.color

let cc = d.deal()
cc.color

// deal out the rest of the deck, leaving just one card
while d.count > 1 {
    d.deal()
}
d

// test equality of a couple cards
if kh == Card(pip: Pip.King, suit: Suit.Clubs) {
    let a = true
}
else {
    let a = false
}

kh != Card(pip: Pip.King, suit: Suit.Clubs)
kh.isFaceCard
fc.isFaceCard



```



## Tcl


```tcl
package require Tcl 8.5

namespace eval playing_cards {
    variable deck
    #variable suits {C D H S}
    variable suits {\u2663 \u2662 \u2661 \u2660}
    variable pips {2 3 4 5 6 7 8 9 10 J Q K A}
    
    proc new_deck {} {
        variable deck
        set deck [list]
        for {set i 0} {$i < 52} {incr i} {
            lappend deck $i
        }
    }
    
    proc shuffle {} {
        variable deck
        # shuffle in place
        for {set i 51} {$i > 0} {incr i -1} {
            set n [expr {int($i * rand())}]
            set card [lindex $deck $n]
            lset deck $n [lindex $deck $i]
            lset deck $i $card
        }
    }
    
    proc deal {{num 1}} {
        variable deck
        incr num -1
        set cards [lrange $deck 0 $num]
        set deck [lreplace $deck 0 $num]
        return $cards
    }
    
    proc card2string {card} {
        variable suits
        variable pips
        set suit [expr {$card / 13}]
        set pip [expr {$card % 13}]
        return [format "%2s %s" [lindex $pips $pip] [lindex $suits $suit]]
    }
    
    proc print {cards args} {
        array set opts [concat -sort false $args]
        if {$opts(-sort)} {
            set cards [lsort -integer $cards]
        }
        foreach card $cards {
            puts [card2string $card]
        }
    }
    
    proc print_deck {} {
        variable deck
        print $deck
    }
}

playing_cards::new_deck
playing_cards::shuffle
set hand [playing_cards::deal 5]
puts "my hand:"
playing_cards::print $hand -sort true
puts "\nthe deck:"
playing_cards::print_deck
```



## VBScript


### ==Implementation==


```vb

class playingcard
	dim suit
	dim pips
end class

class carddeck
	private suitnames
	private pipnames
	private cardno
	private deck(52)
	private nTop
	
	sub class_initialize
		dim suit
		dim pips
		suitnames = split("H,D,C,S",",")
		pipnames = split("A,2,3,4,5,6,7,8,9,10,J,Q,K",",")
		cardno = 0

		for suit = 1 to 4
			for pips = 1 to 13
				set deck(cardno) = new playingcard
				deck(cardno).suit = suitnames(suit-1)
				deck(cardno).pips = pipnames(pips-1)
				cardno = cardno + 1
			next
		next
		nTop = 0
	end sub
	
	public sub showdeck
		dim a
		redim a(51-nTop)
		for i = nTop to 51
			a(i) = deck(i).pips & deck(i).suit  
		next
		wscript.echo join( a, ", ")
	end sub
	
	public sub shuffle
		dim r
		randomize timer
		for i = nTop to 51
			r = int( rnd * ( 52 - nTop ) ) 
			if r <> i then
				objswap deck(i),deck(r)
			end if
		next
	end sub

	public function deal()
		set deal = deck( nTop )
		nTop = nTop + 1
	end function

	public property get cardsRemaining
		cardsRemaining = 52 - nTop
	end property
	
	private sub objswap(   a,   b )
		dim tmp
		set tmp = a
		set a = b
		set b = tmp
	end sub
end class

```



### ==Invocation==


```vb

dim pack
set pack = new carddeck
wscript.echo "--before shuffle"
pack.showdeck
pack.shuffle
wscript.echo "--after shuffle"
pack.showdeck

dim card
for i = 1 to 52
	set card = pack.deal
next
wscript.echo "--dealt a card, it's the", card.pips, "of", card.suit 
wscript.echo "--", pack.cardsRemaining, "cards remaining"
if pack.cardsRemaining <> 0 then
	pack.showdeck
end if


```



### ==Output==


```txt

--before shuffle
AH, 2H, 3H, 4H, 5H, 6H, 7H, 8H, 9H, 10H, JH, QH, KH, AD, 2D, 3D, 4D, 5D, 6D, 7D, 8D, 9D, 10D, JD, QD, KD, AC, 2C, 3C, 4C, 5C, 6C, 7C, 8C, 9C, 10C, JC, QC, KC, AS, 2S, 3S, 4S, 5S, 6S, 7S, 8S, 9S, 10S, JS, QS, KS
--after shuffle
QD, QH, 4S, KD, JC, 10H, JD, 6D, 2S, 4C, 4D, 8H, QC, 5S, JH, KS, 6H, 8S, 7D, 10D, AD, 9S, KH, 2D, 3S, AC, JS, 3D, 9D, 3H, 5C, 10S, KC, 6C, AH, AS, 6S, 5H, 3C, 4H, 9H, 8C, 7S, 9C, 10C, 2C, 7H, 5D, QS, 2H, 7C, 8D
--dealt a card, it's the 8 of D
-- 0 cards remaining

```



## Vedit macro language

The deck is created in a free edit buffer, one line for each card.
Each users hand is another edit buffer.
There is no need for any routines to display the deck or a hand, since each of them is displayed in an edit window.

```vedit
// Playing Cards, main program

Call("CREATE_DECK")
Call("SHUFFLE_DECK")
#21 = Buf_Switch(Buf_Free)        // #21 = players hand, 1st player
#1 = 5; Call("DEAL_CARDS")        // deal 5 cards to player 1
#22 = Buf_Switch(Buf_Free)        // #22 = players hand, 2nd player
#1 = 5; Call("DEAL_CARDS")        // deal 5 cards to player 2
Buf_Switch(#10) BOF               // display the deck
Return

///////////////////////////////////////////////////////////////////////////
//
//  Create a deck into a new edit buffer. One text line for each card.
//
:CREATE_DECK:
#10 = Buf_Switch(Buf_Free)        // Buffer @(#10) = the deck

RS(1, "Diamonds")
RS(2, "Spades")
RS(3, "Hearts")
RS(4, "Clubs")

RS(11, " Jack")
RS(12, "Queen")
RS(13, " King")
RS(14, "  Ace")

for (#1=1; #1<5; #1++) {
    for (#2=2; #2<15; #2++) {
        if (#2 < 11) {
            Num_Ins(#2, NOCR)     // pip (2 to 10) as numeric
        } else {
            IT(@(#2))             // pip (11 to 14) as a word
        }
        IT(" of ")
        IT(@(#1)) IN              // suit
    }
}
Return

///////////////////////////////////////////////////////////////////////////
//
//  Shuffle the deck using Fisher-Yates algorithm
//
:SHUFFLE_DECK:
Buf_Switch(#10)                   // the deck
#90 = Time_Tick                   // seed for random number generator
#91 = 51                          // random numbers in range 0 to 50
for (#1=1; #1<52; #1++) {
    Call("RANDOM")
    Goto_Line(Return_Value+1)
    Block_Copy(#1, #1, LINESET+DELETE)
    Reg_Copy(9, 1, DELETE)
    Goto_Line(#1)
    Reg_Ins(9)
}
Return

//--------------------------------------------------------------
// Generate random numbers in range 0 <= Return_Value < #91
//  #90 = Seed    (0 to 0x7fffffff)
//  #91 = Scaling (0 to 0x10000)

:RANDOM:
#92 = 0x7fffffff / 48271
#93 = 0x7fffffff % 48271
#90 = (48271 * (#90 % #92) - #93 * (#90 / #92)) & 0x7fffffff
Return ((#90 & 0xffff) * #91 / 0x10000)


///////////////////////////////////////////////////////////////////////////
//
//  Deal #1 cards: move the cards from deck to current edit buffer
//
:DEAL_CARDS:
#11 = Buf_Num                     // this buffer (players hand)
Buf_Switch(#10)                   // the deck
BOF
Reg_Copy(9, #1, DELETE)           // pull the first #1 cards from the deck
Buf_Switch(#11)                   // players hand
Reg_ins(9)                        // insert the cards here
Return
```



## zkl


```zkl
const Diamonds=1, Spades=3, Clubs=0, Hearts=2, Ace=1; // informational
var suits=T(0x1F0D1,0x1F0C1,0x1F0B1,0x1F0A1); //unicode 🃑,🃁,🂱,🂡

class Card{
   fcn init(pip,suit){  // or 0..51
      reg p,s;
      if(vm.numArgs==1){ s=pip/13; p=pip%13; } else { p=pip; s=suit }
      var [const] _pip=p, _suit=s;
   }
   fcn toString{
      p:=_pip + (_pip>=11);
      (suits[_suit]+p).toString(8); // int-->UTF-8
   }
}

class Deck{  //--> 52 shuffled Cards
   var [const] deck=L();
   fcn init{
      (0).pump(52,deck.clear().write,Card);
      shuffle();
   }
   fcn shuffle{ deck.shuffle() }
   fcn deal(cards=5){ deck.pop(0,cards); }
   fcn toString{ deck.pump(String,"toString"); }
}
```


```zkl
d:=Deck();
d.println(d.deck.len());
d.deal().println();
d.println();
```

{{out}}

```txt

🃎🂱🃗🂢🂸🃄🂷🂤🃉🂡🂽🂥🂶🃃🃇🂳🂫🃍🃅🂭🃚🃞🃋🃘🂹🃛🂩🂺🃁🂮🂣🃖🂨🃙🃝🃒🂪🃂🃊🂲🃈🂧🃑🃆🂵🂴🃔🂾🂦🃓🂻🃕52
L(🃎,🂱,🃗,🂢,🂸)
🃄🂷🂤🃉🂡🂽🂥🂶🃃🃇🂳🂫🃍🃅🂭🃚🃞🃋🃘🂹🃛🂩🂺🃁🂮🂣🃖🂨🃙🃝🃒🂪🃂🃊🂲🃈🂧🃑🃆🂵🂴🃔🂾🂦🃓🂻🃕

```


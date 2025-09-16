+++
title = "Go Fish/J"
description = ""
date = 2011-02-03T02:31:56Z
aliases = []
[extra]
id = 9167
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
The  essential   portion  of  the   program  is  a  gerund   of  verbs
action0`action1`...`actionn, one  for each state of  the game.  Agenda
@.   drives  the action.   This  iterates  ^:f^:_  until reaching  the
termination state.  Function  f returns a Boolean value  and is 0 when
it identifies the  terminal.  The states by number  are represented in
base 0  as 0b[0-z]  for easy identification.   The actions  return the
next state.  Thus verb go's guts are


```J
   setup
   g`e`r`u`n`d @. extract_state ^: non_terminal ^: _ choose_1st_player
   cleanup

```


Instruct
```j
go fish
```
   to play.


The code uses the cards as recommended and fulfills the AI requirement
in a minimal fashion.  If J hands over its last card to the opponent J
chooses to not draw a card.


The state transitions follow.  The code combines some of these states.
Since J assumes undefined names are verbs we don't need to define  the
unused verbs of the gerund.


```C++

J CAPITAL, opponent lower.

book: remove books from hand and update score.
fill: optionally draw a card into empty hand.
end: no more fish in pond.

book -> fill ->
              end
             draw -> BOOK     Chose to not draw a card into an empty hand before asking; silly but valid.
              ask -> RESPOND ->
                                FISH -> draw -> BOOK
                              SUPPLY -> book
BOOK -> FILL ->
              end
              ASK -> respond ->
                                fish -> DRAW -> book
                              supply -> dumb -> BOOK

          state
number  name     transition to

0b0     book     0b1
0b1     fill     0b2 0b3 0b5
0b2     end
0b3     draw     0b4
0b4     BOOK     0b9
0b5     ask      0b6
0b6     RESPOND  0b7 0b8
0b7     FISH     0b3
0b8     SUPPLY   0b0
0b9     FILL     0b2 0ba
0ba     ASK      0bb
0bb     respond  0bc 0be
0bc     fish     0bd
0bd     DRAW     0b0
0be     supply   0bf
0bf     dumb     0b4

```


Here is fish.ijs which includes some test code to demonstrate and of course test some of its verbs.


```J

load'~user/playingcards.ijs'
coinsert'rcpc'

show =: [: , ' ' ,. sayCards@:/:~  NB. show hand   generates a linear display

instructions =: 0 : 0
Go Fish!
Request a card using a single character: 23456789tjqka
Start the game with
   go fish
)

write =: ": 1!:2 2:
read =: 1!:1@1:

lc =:(] i.~ 'AKQJT' , ]) { 'akqjt' , ]  NB. lower case

fish =: 'fish'

rnks =: ,@:(_ 1&{.)

counts =: 3 : 0  NB. return the values of the cards y  having x members
4 counts y
:
(~. #~ x = [: +/"1 =)rnks y
)

score =: 4 : 0 NB. player score J
victor =. x *@- y
if. 0 = victor do. 'Tie'
else.
  if. 0 < victor do. a =. 'You win'
  else. a =. 'J wins'
  end.
  a =. a,' by '
  margin =. |x-y
  if. 1 = margin do. a,'a book.'
  else. a,(":margin),' books.'
  end.
end.
)

HAND=:hand=:0 2$BOOKS=:books=:DEBUG =: 0
debug =: 3 : 0
if. DEBUG do.
  write y
  write(":BOOKS),' HAND ',show HAND
  write(":books),' hand ',show hand
end.
)

book =: 3 : 0
debug'book'
r =. counts hand
books =: books+#r
hand =: (#~ [: -. r e.~ rnks)hand
0b1
)

BOOK =: 3 : 0
debug'BOOK'
r =. counts HAND
BOOKS =: BOOKS+#r
HAND =: (#~ [: -. r e.~ rnks)HAND
0b9
)

fill =: 3 : 0
debug'fill'
NB. hand has cards, proceed.
if. #hand do. 0b5 return. end.
NB. no cards in deck, none in hand, done.
if. 0 = # TheDeck__pc do. 0b2 return. end.
write 'Draw a card into your empty hand?  (y/n)'
NB. (must draw a card)  Skipped the proposed "draw" verb.
hand =: hand,dealCards__pc 1
NB. failure to choose to draw ends turn.
0b4 0b5{~'1tTyY'e.~{.read''
)
FILL =: 3 : 0
debug'FILL'
if. #HAND do. 0ba return. end.
if. 0 = # TheDeck__pc do. 0b2 return. end.
HAND =: HAND,dealCards__pc 1
0ba NB. This is the AI.
)
R =: 'a23456789tjqk'
ask =: 3 : 0
debug'ask'
write 'You hold ',show hand
write 'Your request please?  ',R
while. -.(rnks hand)e.~v=.(i.14){~R i. lc read'' do. write '?' end.
if. v e. rnks HAND do.
  a =. (#~ v e.~ rnks) HAND
  write'You took ',' of them.',~":#a
  hand =: hand,a
  HAND =: HAND -. a
  0b0
else.
  write 'Nope.  Go fish!'
  0b3
end.
)
ASK =: 3 : 0
debug'ASK'
a =. {.({~ ?@#)HAND

if. a e. rnks hand do.
  write 'J took "','"s.',~":a{R
  a =. (#~ a = rnks) hand
  HAND =: HAND,a
  hand =: hand-.a
  0b4
else.
  write 'J requested ','.  You said "fish".',~":a{R
  0bd
end.
)

draw =: 3 : 0
debug'draw'
hand =: hand,c =. dealCards__pc 1
write 'You caught ','.',~show c
0b4
)
DRAW =: 3 : 0
debug'DRAW'
HAND =: HAND,dealCards__pc 1
0b0
)

NB. condensed
NB.0b6     RESPOND  0b7 0b8
NB.0b7     FISH     0b3
NB.0b8     SUPPLY   0b0
NB.0bb     respond  0bc 0be
NB.0bc     Fish     0bd
NB.0be     supply   0bf
NB.0bf     dumb 0b4

test =: 3 : 0
pc =. newDeck''
shuffle__pc''
card1 =. dealCards__pc 1
card51 =. dealCards__pc 51

NB. linear display
assert. 1 -: #$ show card51

NB. some assurance that it counts right
assert. 1 -: # 1 counts card1
assert. 0 -: # counts card1
assert. 0 -: # 1 counts card51
assert. 1 -: # 3 counts card51
assert. 12 -: # counts card51

NB. demonstrate the main loop state machine with data passing
state0 =. 0b1"_ ; 'state 0'"_
state1 =. 0b2"_ ; '  state 1'"_ ,~ >@{:
eval =. state0`state1@.(>@{.)^:(0b2&~:@>@{.)^:_]0
expect =. 2 ; 'state 0  state 1'
assert. expect -: eval

NB. lower case
assert. 'aXq98a' -: lc 'AXQ98a'

NB. score
assert. 'J wins by a book.' -: 3 score 4
assert. 'You win by a book.' -: 3 score~ 4
assert. 'Tie' -: score~3
assert. 'You win by 30 books.' -: 3 score~ 33
assert. 'J wins by 30 books.' -: 3 score 33

NB. book
hand =: card51
books =: 0
book''
assert. 3 = # hand
assert. 12 = books
)

test''

write instructions

go =: 3 : 0 NB. go fish    starts the game.
write instructions
0 go y
:
DEBUG =: x
pc=:newDeck''
shuffle__pc''
'HAND hand'=: 2 dealCards__pc 9       NB. deal two hands of nine cards
'BOOKS books' =: 0                    NB. initialize the score
book`fill`end`draw`BOOK`ask`RESPOND`FISH`SUPPLY`FILL`ASK`respond`Fish`DRAW`supply`dumb@.]^:(0b2&~:)^:_(?2){0b0 0b4
BOOK book''
if. fail =. books 13&~:@+ BOOKS do.
  write'uh oh, books add to ',":books+BOOKS
end.
if. fail +. HAND 0&~:@+&# hand do.
  write 'OH NO! found cards left in hands.'
  write 'HAND ',show HAND
  write 'hand ',show hand
end.
write books score BOOKS
)

```


Sample

```J


   go load'~user/fish.ijs'
Go Fish!
Request a card using a single character: 23456789tjqka
Start the game with
   go fish

Go Fish!
Request a card using a single character: 23456789tjqka
Start the game with
   go fish

J requested 9.  You said "fish".
You hold   A♦  4♣  5♥  5♠  6♠  7♣ 10♣  J♣  J♥
Your request please?  a23456789tjqk
t
Nope.  Go fish!
You caught   K♦.
J requested 9.  You said "fish".
You hold   A♦  4♣  5♥  5♠  6♠  7♣ 10♣  J♣  J♥  K♦
Your request please?  a23456789tjqk

```


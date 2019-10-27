+++
title = "Playing Cards/MUMPS"
description = ""
date = 2011-02-27T19:21:52Z
aliases = []
[extra]
id = 9301
[taxonomies]
categories = []
tags = []
+++

{{collection|Playing Cards}}

```MUMPS

DECK ;Routines to deal with playing cards
 ;http://en.wikipedia.org/wiki/Fisher-Yates_shuffle#Modern_method
 ;Global ^DARCH is the definitions for the deck types.
 ; First node contains the Deck Number, a non-negative integer
 ;   Second node:
 ;   =0 - deck description
 ;     Third node=0 - the string is a caret separated list of deck numbers to build into a larger deck
 ;   =1 - Ranks
 ;     Third node=1 - short names
 ;     Third node=2 - long names
 ;   =2 - Suits 
 ;   Note that Jokers are suit zero
 ;     Third node=1 - short names
 ;     Third node=2 - long names
 ;   =3 - Jokers
 ;     Third node=1 - rank numbers
 ;     Third node=2 - long names
 ;
 ;*** Add in logic for multiple decks later
 ;
 ;Global ^DECK contains the packs of cards in use. It has to be a global 
 ;in case multiple users play together
 ;First node is an identifier for the deck. Usually this would 
 ;be the job number of the dealer.
 ; Second node is the Hand holding the card.
 ; Hand = 0 is the stock for a game
 ; Third node is the position within a hand/deck
 ; The three nodes point to a string with the format 
 ; of "Rank Number^Suit Number"
 ;
 ; If no subroutine is specified, at least make sure the global is set
 ;
MAKEGLOB(FORCE="0") 
 ;creates the ^DARCH (deck archtype) global, and populates the archtype decks
 ;If FORCE is true, wipe out the old global first
 KILL:+FORCE ^DARCH
 SET ^DARCH(0,0)="No Jokers, Ace Low"
 SET ^DARCH(0,1,1)=" 1^ 2^ 3^ 4^ 5^ 6^ 7^ 8^ 9^10^11^12^13"
 SET ^DARCH(0,1,2)="Ace^Two^Three^Four^Five^Six^Seven^Eight^Nine^Ten^Jack^Queen^King"
 SET ^DARCH(0,2,1)="H^D^C^S"
 SET ^DARCH(0,2,2)="Hearts^Diamonds^Clubs^Spades"
 SET ^DARCH(0,3,1)=""
 SET ^DARCH(1,0)="Two Jokers, Ace High"
 SET ^DARCH(1,1,1)=" 2^ 3^ 4^ 5^ 6^ 7^ 8^ 9^10^11^12^13^ A"
 SET ^DARCH(1,1,2)="Two^Three^Four^Five^Six^Seven^Eight^Nine^Ten^Jack^Queen^King^Ace"
 SET ^DARCH(1,2,1)="H^D^C^S"
 SET ^DARCH(1,2,2)="Hearts^Diamonds^Clubs^Spades"
 SET ^DARCH(1,3,1)=" 1^ 1"
 SET ^DARCH(1,3,2)="Joker^Joker"
 SET ^DARCH(2,0)="Tarot"
 SET ^DARCH(2,1,1)="1^2^3^4^5^6^7^8^9^10^11^12^13^14"
 SET ^DARCH(2,1,2)="Ace^2^3^4^5^6^7^8^9^10^Page^Knight^Queen^King"
 SET ^DARCH(2,2,1)="W^C^S^D"
 SET ^DARCH(2,2,2)="Wands^Cups^Swords^Disks"
 SET ^DARCH(2,3,1)="0^1^2^3^4^5^6^7^8^9^10^11^12^13^14^15^16^17^18^19^20^21^22"
 SET ^DARCH(2,3,2)="The Fool^The Magician^The High Priestess^The Empress^The Emperor^The Hierophant^"
 SET ^DARCH(2,3,2)=^DARCH(2,3,2)_"The Lovers^The Chariot^Strength^The Hermit^Wheel of Fortune^"
 SET ^DARCH(2,3,2)=^DARCH(2,3,2)_"Justice^The Hanged Man^Death^Temperance^The Devil^The Tower^"
 SET ^DARCH(2,3,2)=^DARCH(2,3,2)_"The Star^The Moon^The Sun^Judgement^The World"
 SET ^DARCH(3,0)="Canasta Double deck, 4 jokers"
 SET ^DARCH(3,0,0)="1^1" ;Make two copies of deck type 1
 SET ^DARCH(4,0)="joker test"
 SET ^DARCH(4,3,1)="1^1^1^1"
 SET ^DARCH(4,3,2)="JOKER^JOKER^JOKER^JOKER"
 QUIT
 ;
CLEANDECK(N) ;deletes a deck. If N is "", clean all decks
 SET:($DATA(N)=0) N=$JOB
 IF (N'="")&(N'=0) KILL ^DECK(N)
 IF (N="")&($DATA(^DECK)>0) FOR SET N=$ORDER(^DECK(N)) Q:N="" KILL ^DECK(N)
 QUIT
 ;
MAKEDECK(DN,DT,GAME="") 
 ;Creates a deck in ^DECK of type DT at ^DECK(DN)
 ;DN is an internal number to identify the deck; defaults to $Job number
 ;DT is the identifier of the deck type to make (or, ^DECK(n,DT))
 ;  defaults to first deck defined in ^DARCH
 ;GAME is the name of the game being played with this deck. Defaults to ""
 IF $DATA(^DARCH)<10 DO MAKEGLOB(1) 
 ;If the ^DARCH global isn't defined, create it
 SET DN=$SELECT(($DATA(DN)<1):$JOB,DN="":$JOB,1:DN)
 ;Force DN to be a nonempty string
 SET:($DATA(DT)<1)!($DATA(^DARCH(DT))<10) DT="",DT=$ORDER(^DARCH(DT))
 ;Force DT to be a nonempty string that exists in ^DARCH
 SET ^DECK(DN,0)=DN_"^"_DT_"^"_GAME
 NEW I,J,NUMRANK,NUMSUIT,NUMJOK,HAND,OFFSET
 ;HAND=0 signifies the stock for a game
 ;OFFSET is the number of cards already created in the deck
 ;NUMRANK - number of ranks in the deck
 ;NUMSUIT - number of suits in the deck
 ;NUMJOK - number of jokers in the deck
 ;I and J are loop values
 SET HAND=0,OFFSET=0
 ;Suit, Rank, or Jokers could be non-existent. Use $GET to get either 
 ;their value or a null string
 ;$LENGTH with a delimiter will return 1 on a null string
 SET NUMRANK=$LENGTH($GET(^DARCH(DT,1,1))) SET:NUMRANK>0 NUMRANK=$LENGTH($GET(^DARCH(DT,1,1)),"^")
 SET NUMSUIT=$LENGTH($GET(^DARCH(DT,2,1))) SET:NUMSUIT>0 NUMSUIT=$LENGTH($GET(^DARCH(DT,2,1)),"^")
 SET NUMJOK=$LENGTH($GET(^DARCH(DT,3,1))) SET:NUMJOK>0 NUMJOK=$LENGTH($GET(^DARCH(DT,3,1)),"^")
 FOR I=1:1:NUMSUIT FOR J=1:1:NUMRANK SET ^DECK(DN,HAND,((I-1)*NUMRANK)+J+OFFSET)=J_"^"_I
 FOR I=1:1:NUMJOK SET ^DECK(DN,HAND,(NUMSUIT*NUMRANK+I+OFFSET))=$PIECE(^DARCH(DT,3,1),"^",I)_"^0"
 KILL I,J,NUMRANK,NUMSUIT,NUMJOK,HAND,OFFSET
 QUIT
 ;
SHUFFLE(DN,HAND) ;Randomize the order of a hand of a deck
 DO:(($DATA(^DECK,HAND)<10)&(HAND=0)) MAKEDECK(DN,0)
 QUIT:$DATA(^DECK(DN,0))#10=0 0 ;There isn't a hand or deck, and no deck type defined
 NEW LAST,I,R,HOLD,INDX,GAP
 SET I="",INDX=""
 FOR  SET I=$ORDER(^DECK(DN,HAND,I)) Q:I=""  SET INDX=INDX_I_"^"
 ;Note that there is a null at the end of INDX due to the last caret, 
 ;hence the LAST equals length minus 1 below
 FOR  SET LAST=$LENGTH(INDX,"^")-1 QUIT:LAST=1  DO
 . SET R=$RANDOM(LAST)+1
 . SET HOLD=^DECK(DN,HAND,$PIECE(INDX,"^",R))
 . SET ^DECK(DN,HAND,$PIECE(INDX,"^",R))=^DECK(DN,HAND,$PIECE(INDX,"^",LAST))
 . SET ^DECK(DN,HAND,$PIECE(INDX,"^",LAST))=HOLD
 .; Need to take out INDX(R), but not leave a "^" hole. 
 .; The indices are by definition canonic, and by SHUFFLE+5, always have a "^" following
 . SET HOLD=$PIECE(INDX,"^",R)_"^"
 . ;The line below is simpler, but does too much when R is a single digit early in the shuffle.
 . ;SET INDX=$PIECE(INDX,HOLD,1)_$PIECE(INDX,HOLD,2)
 . SET GAP=$FIND(INDX,HOLD)
 . SET INDX=$EXTRACT(INDX,1,GAP-$LENGTH(HOLD)-1)_$EXTRACT(INDX,GAP,$LENGTH(INDX))
 KILL LAST,I,R,INDX,HOLD
 QUIT 1
 ;
PRINTHAND(DN,HAND)
 NEW I,DT
 ;I is a loop variable
 ;DT is the deck type
 QUIT:($DATA(^DECK(DN))<10)!($DATA(^DECK(DN,HAND))<10) 0 ; Quit if not a valid hand
 SET DT=$PIECE(^DECK(DN,0),"^",2)
 WRITE !!
 SET I="" FOR  SET I=$ORDER(^DECK(DN,HAND,I)) QUIT:I=""  DO
 . ;non-zero integers for the suit position mean a suit, while zero means a joker
 . IF $PIECE(^DECK(DN,HAND,I),"^",2) DO
 . . WRITE $PIECE(^DARCH(DT,1,2),"^",$PIECE(^DECK(DN,HAND,I),"^",1))_" of "
 . . WRITE $PIECE(^DARCH(DT,2,2),"^",$PIECE(^DECK(DN,0,I),"^",2)),!
 . IF '$PIECE(^DECK(DN,HAND,I),"^",2) DO
 . . WRITE $PIECE(^DARCH(DT,3,2),"^",$PIECE(^DECK(DN,HAND,I),"^",1)),!
 KILL I,DT
 QUIT
 ;
TEST
 DO MAKEGLOB(1)
 DO CLEANDECK
 DO MAKEDECK(3141,2)
 DO SHUFFLE(3141,0) ;Ignoring returned result
 DO PRINTHAND(3141,0)
 QUIT

```

Example of use:
```txt

USER>D TEST^CARDS
 
Three of Hearts
Four of Hearts
Five of Clubs
Seven of Clubs
Eight of Hearts
Ace of Hearts
Five of Diamonds
Nine of Spades
Nine of Hearts
Eight of Spades
Four of Spades
Five of Spades
Seven of Hearts
Six of Clubs
Eight of Diamonds
Nine of Diamonds
Six of Hearts
Three of Diamonds
Ace of Clubs
Two of Hearts
Five of Hearts
Four of Clubs
Jack of Diamonds
Ace of Diamonds
Jack of Spades
Jack of Hearts
Two of Diamonds
Six of Diamonds
Ace of Spades
Three of Clubs
Two of Clubs
Seven of Diamonds
King of Clubs
Queen of Hearts
Nine of Clubs
Seven of Spades
Two of Spades
Queen of Diamonds
Four of Diamonds
Six of Spades
Eight of Clubs
Three of Spades
King of Hearts
Ten of Clubs
Ten of Hearts
King of Diamonds
Ten of Diamonds
Jack of Clubs
Ten of Spades
Queen of Spades
Queen of Clubs
King of Spades
```


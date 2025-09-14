+++
title = "Set puzzle"
description = ""
date = 2019-10-20T07:57:20Z
aliases = []
[extra]
id = 12906
[taxonomies]
categories = ["task", "Cards"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "c",
  "ceylon",
  "cpp",
  "d",
  "echolisp",
  "elixir",
  "erlang",
  "factor",
  "functions",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "major_constants",
  "mathematica",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "tcl",
  "zkl",
]
+++

{{task|Cards}} [[Category:Puzzles]]
Set Puzzles are created with a deck of cards from the [[wp:Set (game)|Set Game™]]. The object of the puzzle is to find sets of 3 cards in a rectangle of cards that have been dealt face up.


There are 81 cards in a deck.
Each card contains a unique variation of the following four features: ''color, symbol, number and shading''.

* there are three colors:
    ''red, green, purple''



* there are three symbols:
    ''oval, squiggle, diamond''



* there is a number of symbols on the card:
    ''one, two, three''



* there are three shadings:
    ''solid, open, striped''



Three cards form a ''set'' if each feature is either the same on each card, or is different on each card. For instance: all 3 cards are red, all 3 cards have a different symbol, all 3 cards have a different number of symbols, all 3 cards are striped.

There are two degrees of difficulty: [http://www.setgame.com/set/rules_basic.htm ''basic''] and [http://www.setgame.com/set/rules_advanced.htm ''advanced'']. The basic mode deals 9 cards, that contain exactly 4 sets; the advanced mode deals 12 cards that contain exactly 6 sets.

When creating sets you may use the same card more than once.




## Task

Write code that deals the cards (9 or 12, depending on selected mode) from a shuffled deck in which the total number of sets that could be found is 4 (or 6, respectively); and print the contents of the cards and the sets.

For instance:



'''DEALT 9 CARDS:'''

:green, one, oval, striped

:green, one, diamond, open

:green, one, diamond, striped

:green, one, diamond, solid

:purple, one, diamond, open

:purple, two, squiggle, open

:purple, three, oval, open

:red, three, oval, open

:red, three, diamond, solid


'''CONTAINING 4 SETS:'''

:green, one, oval, striped

:purple, two, squiggle, open

:red, three, diamond, solid


:green, one, diamond, open

:green, one, diamond, striped

:green, one, diamond, solid


:green, one, diamond, open

:purple, two, squiggle, open

:red, three, oval, open


:purple, one, diamond, open

:purple, two, squiggle, open

:purple, three, oval, open





## Ada


We start with the specification of a package "Set_Puzzle.


```Ada
package Set_Puzzle is

   type Three is range 1..3;
   type Card is array(1 .. 4) of Three;
   type Cards is array(Positive range <>) of Card;
   type Set is array(Three) of Positive;

   procedure Deal_Cards(Dealt: out Cards);
   -- ouputs an array with disjoint cards

   function To_String(C: Card) return String;

   generic
      with procedure Do_something(C: Cards; S: Set);
   procedure Find_Sets(Given: Cards);
   -- calls Do_Something once for each set it finds.

end Set_Puzzle;
```


Now we implement the package "Set_Puzzle".


```Ada
with Ada.Numerics.Discrete_Random;

package body Set_Puzzle is

   package Rand is new Ada.Numerics.Discrete_Random(Three);
   R: Rand.Generator;

   function Locate(Some: Cards; C: Card) return Natural is
      -- returns index of card C in Some, or 0 if not found
   begin
      for I in Some'Range loop
	 if C = Some(I) then
	    return I;
	 end if;
      end loop;
      return 0;
   end Locate;

   procedure Deal_Cards(Dealt: out Cards) is
      function Random_Card return Card is
	 (Rand.Random(R), Rand.Random(R), Rand.Random(R), Rand.Random(R));
   begin
      for I in Dealt'Range loop
	 -- draw a random card until different from all card previously drawn
	 Dealt(I) := Random_Card; -- draw random card
	 while Locate(Dealt(Dealt'First .. I-1), Dealt(I)) /= 0 loop
	    -- Dealt(I) has been drawn before
	    Dealt(I) := Random_Card; -- draw another random card
	 end loop;
      end loop;
   end Deal_Cards;

   procedure Find_Sets(Given: Cards) is
      function To_Set(A, B: Card) return Card is
	 -- returns the unique card C, which would make a set with A and B
	 C: Card;
      begin
	 for I in 1 .. 4 loop
	    if A(I) = B(I) then
	       C(I) := A(I); -- all three the same
	    else
	       C(I) := 6 - A(I) - B(I); -- all three different;
	    end if;
	 end loop;
	 return C;
      end To_Set;

      X: Natural;

   begin
      for I in Given'Range loop
	 for J in Given'First .. I-1 loop
	    X := Locate(Given, To_Set(Given(I), Given(J)));
	    if I < X then -- X=0 is no set, 0 < X < I is a duplicate
	      Do_Something(Given, (J, I, X));
	    end if;
	 end loop;
      end loop;
   end Find_Sets;

   function To_String(C: Card) return String is

      Col: constant array(Three) of String(1..6)
	:= ("Red   ", "Green ", "Purple");
      Sym: constant array(Three) of String(1..8)
	:= ("Oval    ", "Squiggle", "Diamond ");
      Num: constant array(Three) of String(1..5)
	:= ("One  ", "Two  ", "Three");
      Sha: constant array(Three) of String(1..7)
	:= ("Solid  ", "Open   ", "Striped");

   begin
      return (Col(C(1)) & " " & Sym(C(2)) & " " & Num(C(3)) & " " & Sha(C(4)));
   end To_String;

begin
   Rand.Reset(R);
end Set_Puzzle;
```


Finally, we write the main program, using the above package. It reads two parameters from the command line. The first parameter describes the number of cards, the second one the number of sets. Thus, for the basic mode one has to call "puzzle 9 4", for the advanced mode "puzzle 12 6", but the program would support any other combination of parameters just as well.


```Ada
with Ada.Text_IO, Set_Puzzle, Ada.Command_Line;

procedure Puzzle is

   package TIO renames Ada.Text_IO;

   Card_Count:     Positive := Positive'Value(Ada.Command_Line.Argument(1));
   Required_Sets:  Positive := Positive'Value(Ada.Command_Line.Argument(2));

   Cards: Set_Puzzle.Cards(1 .. Card_Count);

   function Cnt_Sets(C: Set_Puzzle.Cards) return Natural is
      Cnt: Natural := 0;
      procedure Count_Sets(C: Set_Puzzle.Cards; S: Set_Puzzle.Set) is
      begin
         Cnt := Cnt + 1;
      end Count_Sets;
      procedure CS is new Set_Puzzle.Find_Sets(Count_Sets);
   begin
      CS(C);
      return Cnt;
   end Cnt_Sets;

   procedure Print_Sets(C: Set_Puzzle.Cards) is
      procedure Print_A_Set(C: Set_Puzzle.Cards; S: Set_Puzzle.Set) is
      begin
         TIO.Put("(" & Integer'Image(S(1)) & "," & Integer'Image(S(2))
                   & "," & Integer'Image(S(3)) & " )  ");
      end Print_A_Set;
      procedure PS is new Set_Puzzle.Find_Sets(Print_A_Set);
   begin
      PS(C);
      TIO.New_Line;
   end Print_Sets;

begin
   loop    -- deal random cards
      Set_Puzzle.Deal_Cards(Cards);
      exit when Cnt_Sets(Cards) = Required_Sets;
   end loop;    -- until number of sets is as required

   for I in Cards'Range loop    -- print the cards
      if I < 10 then
         TIO.Put(" ");
      end if;
      TIO.Put_Line(Integer'Image(I) & " " & Set_Puzzle.To_String(Cards(I)));
   end loop;

   Print_Sets(Cards);    -- print the sets
end Puzzle;
```


```txt
>./puzzle 9 4
  1 Red    Diamond  One   Striped
  2 Green  Squiggle Two   Solid
  3 Red    Squiggle Three Open
  4 Green  Squiggle Three Solid
  5 Purple Oval     Two   Open
  6 Purple Squiggle One   Striped
  7 Green  Squiggle One   Solid
  8 Purple Squiggle One   Solid
  9 Purple Diamond  Three Solid
( 2, 3, 6 )  ( 1, 4, 5 )  ( 2, 4, 7 )  ( 5, 6, 9 )

>./puzzle 12 6
  1 Purple Diamond  One   Solid
  2 Red    Diamond  One   Striped
  3 Red    Oval     Three Striped
  4 Green  Oval     Two   Solid
  5 Red    Squiggle Three Solid
  6 Green  Squiggle Two   Solid
  7 Red    Squiggle Three Striped
  8 Red    Squiggle Three Open
  9 Purple Squiggle One   Striped
 10 Red    Diamond  Two   Solid
 11 Red    Squiggle One   Open
 12 Red    Oval     One   Solid
( 1, 4, 5 )  ( 5, 7, 8 )  ( 6, 8, 9 )  ( 3, 10, 11 )  ( 5, 10, 12 )  ( 2, 11, 12 )
```



## AutoHotkey


```autohotkey
; Generate deck; card encoding from Perl6
Loop, 81
	deck .= ToBase(A_Index-1, 3)+1111 ","
deck := RegExReplace(deck, "3", "4")

; Shuffle
deck := shuffle(deck)

msgbox % clipboard := allValidSets(9, 4, deck)
msgbox % clipboard := allValidSets(12, 6, deck)

; Render a hand (or any list) of cards
PrettyHand(hand) {
	 Color1:="red",Color2:="green",Color4:="purple"
	,Symbl1:="oval",Symbl2:="squiggle",Symbl4:="diamond"
	,Numbr1:="one",Numbr2:="two",Numbr4:="three"
	,Shape1:="solid",Shape2:="open",Shape4:="striped"
	Loop, Parse, hand, `,
	{
		StringSplit, i, A_LoopField
		s .= "`t" Color%i1% "`t" Symbl%i2% "`t" Numbr%i3% "`t" Shape%i4% "`n"
	}
	Return s
}

; Get all unique valid sets of three cards in a hand.
allValidSets(n, m, deck) {
	While j != m
	{
		j := 0
		,hand := draw(n, deck)
		,s := "Dealt " n " cards:`n" . prettyhand(hand)
		StringSplit, set, hand, `,
		comb := comb(n,3)
		Loop, Parse, comb, `n
		{
			StringSplit, i, A_LoopField, %A_Space%
			If isValidSet(set%i1%, set%i2%, set%i3%)
				s .= "`nSet " ++j ":`n" . prettyhand(set%i1% "," set%i2% "," set%i3%)
		}
	}
	Return s
}

; Convert n to arbitrary base using recursion
toBase(n,b) {  ; n >= 0, 1 < b < StrLen(t), t = digits
	Static t := "0123456789ABCDEF"
	Return (n < b ? "" : ToBase(n//b,b)) . SubStr(t,mod(n,b)+1,1)
}

; Knuth shuffle from http://rosettacode.org/wiki/Knuth_Shuffle#AutoHotkey
shuffle(list) {									; shuffle comma separated list, converted to array
	StringSplit a, list, `,						; make array (length = a0)
	Loop % a0-1 {
		Random i, A_Index, a0					; swap item 1,2... with a random item to the right of it
		t := a%i%, a%i% := a%A_Index%, a%A_Index% := t
	}
	Loop % a0									; construct string from sorted array
		s .= "," . a%A_Index%
	Return SubStr(s,2)							; drop leading comma
}

; Randomly pick a hand of cards from the deck
draw(n, deck) {
	Loop, % n
	{
		Random, i, 1, 81
		cards := deck
		Loop, Parse, cards, `,
			(A_Index = i) ? (hand .= A_LoopField ",") : (cards .= A_LoopField ",")
		deck := cards
	}
	Return SubStr(hand, 1, -1)
}

; Test if a particular group of three cards is a valid set
isValidSet(a, b, c) {
	StringSplit, a, a
	StringSplit, b, b
	StringSplit, c, c
	Return !((a1|b1|c1 ~= "[3,5,6]") + (a2|b2|c2 ~= "[3,5,6]") + (a3|b3|c3 ~= "[3,5,6]") + (a4|b4|c4 ~= "[3,5,6]"))
}

; Get all combinations, from http://rosettacode.org/wiki/Combinations#AutoHotkey
comb(n,t) { ; Generate all n choose t combinations of 1..n, lexicographically
	IfLess n,%t%, Return
	Loop %t%
		c%A_Index% := A_Index
	i := t+1, c%i% := n+1

	Loop {
		Loop %t%
			i := t+1-A_Index, c .= c%i% " "
		c .= "`n"	  ; combinations in new lines
		j := 1, i := 2
		Loop
			If (c%j%+1 = c%i%)
				 c%j% := j, ++j, ++i
			Else Break
		If (j > t)
			Return c
		c%j% += 1
	}
}
```

```txt
Dealt 9 cards:
	purple	diamond	three	striped
	green	diamond	two	open
	green	oval	one	striped
	red	oval	two	solid
	purple	squiggle	two	striped
	red	diamond	three	open
	red	diamond	three	open
	green	oval	one	solid
	red	oval	two	solid

Set 1:
	purple	squiggle	two	striped
	red	oval	two	solid
	green	diamond	two	open

Set 2:
	green	oval	one	solid
	red	diamond	three	open
	purple	squiggle	two	striped

Set 3:
	green	oval	one	solid
	red	diamond	three	open
	purple	squiggle	two	striped

Set 4:
	red	oval	two	solid
	purple	squiggle	two	striped
	green	diamond	two	open


Dealt 12 cards:
	purple	oval	two	open
	purple	diamond	three	solid
	green	squiggle	three	striped
	green	squiggle	one	solid
	purple	squiggle	one	striped
	purple	squiggle	one	solid
	green	diamond	two	solid
	purple	squiggle	one	striped
	red	diamond	two	striped
	green	diamond	one	open
	green	oval	one	open
	red	squiggle	one	open

Set 1:
	purple	squiggle	one	striped
	purple	diamond	three	solid
	purple	oval	two	open

Set 2:
	purple	squiggle	one	striped
	purple	diamond	three	solid
	purple	oval	two	open

Set 3:
	green	diamond	one	open
	red	diamond	two	striped
	purple	diamond	three	solid

Set 4:
	green	oval	one	open
	green	diamond	two	solid
	green	squiggle	three	striped

Set 5:
	red	squiggle	one	open
	purple	squiggle	one	striped
	green	squiggle	one	solid

Set 6:
	red	squiggle	one	open
	purple	squiggle	one	striped
	green	squiggle	one	solid
```



## C

Brute force. Each card is a unique number in the range of [0,81].  Randomly deal a hand of cards until exactly the required number of sets are found.

```c
#include <stdio.h>
#include <stdlib.h>

char *names[4][3] = {
	{ "red", "green", "purple" },
	{ "oval", "squiggle", "diamond" },
	{ "one", "two", "three" },
	{ "solid", "open", "striped" }
};

int set[81][81];

void init_sets(void)
{
	int i, j, t, a, b;
	for (i = 0; i < 81; i++) {
		for (j = 0; j < 81; j++) {
			for (t = 27; t; t /= 3) {
				a = (i / t) % 3;
				b = (j / t) % 3;
				set[i][j] += t * (a == b ? a : 3 - a - b);
			}
		}
	}
}

void deal(int *out, int n)
{
	int i, j, t, c[81];
	for (i = 0; i < 81; i++) c[i] = i;
	for (i = 0; i < n; i++) {
		j = i + (rand() % (81 - i));
		t = c[i], c[i] = out[i] = c[j], c[j] = t;
	}
}

int get_sets(int *cards, int n, int sets[][3])
{
	int i, j, k, s = 0;
	for (i = 0; i < n; i++) {
		for (j = i + 1; j < n; j++) {
			for (k = j + 1; k < n; k++) {
				if (set[cards[i]][cards[j]] == cards[k])
					sets[s][0] = i,
					sets[s][1] = j,
					sets[s][2] = k,
					s++;
			}
		}
	}

	return s;
}

void show_card(int c)
{
	int i, t;
	for (i = 0, t = 27; t; i++, t /= 3)
		printf("%9s", names[i][(c/t)%3]);
	putchar('\n');
}

void deal_sets(int ncard, int nset)
{
	int c[81];
	int csets[81][3]; // might not be enough for large ncard
	int i, j, s;

	do deal(c, ncard); while ((s = get_sets(c, ncard, csets)) != nset);

	printf("dealt %d cards\n", ncard);
	for (i = 0; i < ncard; i++) {
		printf("%2d:", i);
		show_card(c[i]);
	}
	printf("\nsets:\n");

	for (i = 0; i < s; i++) {
		for (j = 0; j < 3; j++) {
			printf("%2d:", csets[i][j]);
			show_card(c[csets[i][j]]);
		}
		putchar('\n');
	}
}

int main(void)
{
	init_sets();
	deal_sets(9, 4);

	while (1) deal_sets(12, 6);

	return 0;
}
```



## C++

```cpp

#include <time.h>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <string>

enum color {
    red, green, purple
};
enum symbol {
    oval, squiggle, diamond
};
enum number {
    one, two, three
};
enum shading {
    solid, open, striped
};
class card {
public:
    card( color c, symbol s, number n, shading h ) {
        clr = c; smb = s; nbr = n; shd = h;
    }
    color getColor() {
        return clr;
    }
    symbol getSymbol() {
        return smb;
    }
    number getNumber() {
        return nbr;
    }
    shading getShading() {
        return shd;
    }
    std::string toString() {
        std::string str = "[";
        str += clr == red ? "red " : clr == green ? "green " : "purple ";
        str += nbr == one ? "one " : nbr == two ? "two " : "three ";
        str += smb == oval ? "oval " : smb == squiggle ? "squiggle " : "diamond ";
        str += shd == solid ? "solid" : shd == open ? "open" : "striped";
        return str + "]";
    }
private:
    color    clr;
    symbol   smb;
    number   nbr;
    shading  shd;
};
typedef struct {
    std::vector<size_t> index;
} set;
class setPuzzle {
public:
    setPuzzle() {
        for( size_t c = red; c <= purple; c++ ) {
            for( size_t s = oval; s <= diamond; s++ ) {
                for( size_t n = one; n <= three; n++ ) {
                    for( size_t h = solid; h <= striped; h++ ) {
                        card crd( static_cast<color>  ( c ),
                                  static_cast<symbol> ( s ),
                                  static_cast<number> ( n ),
                                  static_cast<shading>( h ) );
                        _cards.push_back( crd );
                    }
                }
            }
        }
    }
    void create( size_t countCards, size_t countSets, std::vector<card>& cards, std::vector<set>& sets ) {
        while( true ) {
            sets.clear();
            cards.clear();
            std::random_shuffle( _cards.begin(), _cards.end() );
            for( size_t f = 0; f < countCards; f++ ) {
                cards.push_back( _cards.at( f ) );
            }
            for( size_t c1 = 0; c1 < cards.size() - 2; c1++ ) {
                for( size_t c2 = c1 + 1; c2 < cards.size() - 1; c2++ ) {
                    for( size_t c3 = c2 + 1; c3 < cards.size(); c3++ ) {
                        if( testSet( &cards.at( c1 ), &cards.at( c2 ), &cards.at( c3 ) ) ) {
                            set s;
                            s.index.push_back( c1 ); s.index.push_back( c2 ); s.index.push_back( c3 );
                            sets.push_back( s );
                        }
                    }
                }
            }
            if( sets.size() == countSets ) return;
        }
    }
private:
    bool testSet( card* c1, card* c2, card* c3 ) {
        int
        c = ( c1->getColor()   + c2->getColor()   + c3->getColor()   ) % 3,
        s = ( c1->getSymbol()  + c2->getSymbol()  + c3->getSymbol()  ) % 3,
        n = ( c1->getNumber()  + c2->getNumber()  + c3->getNumber()  ) % 3,
        h = ( c1->getShading() + c2->getShading() + c3->getShading() ) % 3;
        return !( c + s + n + h );
    }
    std::vector<card> _cards;
};
void displayCardsSets( std::vector<card>& cards, std::vector<set>& sets ) {
    size_t cnt = 1;
    std::cout << " ** DEALT " << cards.size() << " CARDS: **\n";
    for( std::vector<card>::iterator i = cards.begin(); i != cards.end(); i++ ) {
        std::cout << std::setw( 2 ) << cnt++ << ": " << ( *i ).toString() << "\n";
    }
    std::cout << "\n ** CONTAINING " << sets.size() << " SETS: **\n";
    for( std::vector<set>::iterator i = sets.begin(); i != sets.end(); i++ ) {
        for( size_t j = 0; j < ( *i ).index.size(); j++ ) {
            std::cout << " " << std::setiosflags( std::ios::left ) << std::setw( 34 )
                      << cards.at( ( *i ).index.at( j ) ).toString() << " : "
                      << std::resetiosflags( std::ios::left ) << std::setw( 2 ) << ( *i ).index.at( j ) + 1 << "\n";
        }
        std::cout << "\n";
    }
    std::cout << "\n\n";
}
int main( int argc, char* argv[] ) {
    srand( static_cast<unsigned>( time( NULL ) ) );
    setPuzzle p;
    std::vector<card> v9, v12;
    std::vector<set>  s4, s6;
    p.create(  9, 4,  v9, s4 );
    p.create( 12, 6, v12, s6 );
    displayCardsSets(  v9, s4 );
    displayCardsSets( v12, s6 );
    return 0;
}

```

```txt

 ** DEALT 9 CARDS: **
 1: [red three squiggle solid]
 2: [purple three squiggle solid]
 3: [red two diamond open]
 4: [purple three oval striped]
 5: [green one squiggle solid]
 6: [green two diamond open]
 7: [red one oval striped]
 8: [green one diamond striped]
 9: [purple one diamond open]

 ** CONTAINING 4 SETS: **
 [red three squiggle solid]         :  1
 [red two diamond open]             :  3
 [red one oval striped]             :  7

 [purple three squiggle solid]      :  2
 [green two diamond open]           :  6
 [red one oval striped]             :  7

 [red two diamond open]             :  3
 [purple three oval striped]        :  4
 [green one squiggle solid]         :  5

 [green one squiggle solid]         :  5
 [red one oval striped]             :  7
 [purple one diamond open]          :  9



 ** DEALT 12 CARDS: **
 1: [green one diamond striped]
 2: [red two squiggle solid]
 3: [red three oval striped]
 4: [red two diamond open]
 5: [green three squiggle striped]
 6: [red three squiggle striped]
 7: [green two squiggle solid]
 8: [purple two squiggle striped]
 9: [purple one squiggle open]
10: [green one squiggle striped]
11: [purple three squiggle solid]
12: [red three squiggle open]

 ** CONTAINING 6 SETS: **
 [green one diamond striped]        :  1
 [red three oval striped]           :  3
 [purple two squiggle striped]      :  8

 [red two squiggle solid]           :  2
 [green three squiggle striped]     :  5
 [purple one squiggle open]         :  9

 [green three squiggle striped]     :  5
 [purple three squiggle solid]      : 11
 [red three squiggle open]          : 12

 [red three squiggle striped]       :  6
 [green two squiggle solid]         :  7
 [purple one squiggle open]         :  9

 [red three squiggle striped]       :  6
 [purple two squiggle striped]      :  8
 [green one squiggle striped]       : 10

 [purple two squiggle striped]      :  8
 [purple one squiggle open]         :  9
 [purple three squiggle solid]      : 11

```



## Ceylon

Add import ceylon.random "1.3.3" to your module.ceylon file

```ceylon
import ceylon.random {
    Random,
    DefaultRandom
}

abstract class Feature() of Color | Symbol | NumberOfSymbols | Shading {}

abstract class Color()
        of red | green | purple
        extends Feature() {}
object red extends Color() {
    string => "red";
}
object green extends Color() {
    string => "green";
}
object purple extends Color() {
    string => "purple";
}

abstract class Symbol()
        of oval | squiggle | diamond
        extends Feature() {}
object oval extends Symbol() {
    string => "oval";
}
object squiggle extends Symbol() {
    string => "squiggle";
}
object diamond extends Symbol() {
    string => "diamond";
}

abstract class NumberOfSymbols()
        of one | two | three
        extends Feature() {}
object one extends NumberOfSymbols() {
    string => "one";
}
object two extends NumberOfSymbols() {
    string => "two";
}
object three extends NumberOfSymbols() {
    string => "three";
}

abstract class Shading()
        of solid | open | striped
        extends Feature() {}
object solid extends Shading() {
    string => "solid";
}
object open extends Shading() {
    string => "open";
}
object striped extends Shading() {
    string => "striped";
}

class Card(color, symbol, number, shading) {
    shared Color color;
    shared Symbol symbol;
    shared NumberOfSymbols number;
    shared Shading shading;

    value plural => number == one then "" else "s";
    string => "``number`` ``shading`` ``color`` ``symbol````plural``";
}

{Card*} deck = {
    for(color in `Color`.caseValues)
    for(symbol in `Symbol`.caseValues)
    for(number in `NumberOfSymbols`.caseValues)
    for(shading in `Shading`.caseValues)
    Card(color, symbol, number, shading)
};

alias CardSet => [Card+];

Boolean validSet(CardSet cards) {

    function allOrOne({Feature*} features) =>
            let(uniques = features.distinct.size)
            uniques == 3 || uniques == 1;

    return allOrOne(cards*.color) &&
            allOrOne(cards*.number) &&
            allOrOne(cards*.shading) &&
            allOrOne(cards*.symbol);
}

{CardSet*} findSets(Card* cards) =>
        cards
            .sequence()
            .combinations(3)
            .filter(validSet);

Random random = DefaultRandom();

class Mode of basic | advanced {

    shared Integer numberOfCards;
    shared Integer numberOfSets;

    shared new basic {
        numberOfCards = 9;
        numberOfSets = 4;
    }

    shared new advanced {
        numberOfCards = 12;
        numberOfSets = 6;
    }
}

[{Card*}, {CardSet*}] deal(Mode mode) {
    value randomStream = random.elements(deck);
    while(true) {
        value cards = randomStream.distinct.take(mode.numberOfCards).sequence();
        value sets = findSets(*cards);
        if(sets.size == mode.numberOfSets) {
            return [cards, sets];
        }
    }
}

shared void run() {
    value [cards, sets] = deal(Mode.basic);
    print("The cards dealt are:
           ");
    cards.each(print);
    print("
           Containing the sets:
           ");
    for(cardSet in sets) {
        cardSet.each(print);
        print("");
    }

}
```



## D


### Basic Version


```d
import std.stdio, std.random, std.array, std.conv, std.traits,
       std.exception, std.range, std.algorithm;

const class SetDealer {
    protected {
        enum Color:  ubyte {green, purple, red}
        enum Number: ubyte {one, two, three}
        enum Symbol: ubyte {oval, diamond, squiggle}
        enum Fill:   ubyte {open, striped, solid}

        static struct Card {
            Color c;
            Number n;
            Symbol s;
            Fill f;
        }

        static immutable Card[81] deck;
    }

    static this() pure nothrow @safe {
        immutable colors = [EnumMembers!Color];
        immutable numbers = [EnumMembers!Number];
        immutable symbols = [EnumMembers!Symbol];
        immutable fill = [EnumMembers!Fill];

        deck = deck.length.iota.map!(i => Card(colors[i / 27],
                                               numbers[(i / 9) % 3],
                                               symbols[(i / 3) % 3],
                                               fill[i % 3])).array;
    }

    // randomSample produces a sorted output that's convenient in our
    // case because we're printing to stout. Normally you would want
    // to shuffle.
    immutable(Card)[] deal(in uint numCards) const {
        enforce(numCards < deck.length, "Number of cards too large");
        return deck[].randomSample(numCards).array;
    }

    // The summed enums of valid sets are always zero or a multiple
    // of 3.
    bool validSet(in ref Card c1, in ref Card c2, in ref Card c3)
    const pure nothrow @safe @nogc {
        return !((c1.c + c2.c + c3.c) % 3 ||
                 (c1.n + c2.n + c3.n) % 3 ||
                 (c1.s + c2.s + c3.s) % 3 ||
                 (c1.f + c2.f + c3.f) % 3);
    }

    immutable(Card)[3][] findSets(in Card[] cards, in uint target = 0)
    const pure nothrow @safe {
        immutable len = cards.length;
        if (len < 3)
            return null;

        typeof(return) sets;
        foreach (immutable i; 0 .. len - 2)
            foreach (immutable j; i + 1 .. len - 1)
                foreach (immutable k; j + 1 .. len)
                    if (validSet(cards[i], cards[j], cards[k])) {
                        sets ~= [cards[i], cards[j], cards[k]];
                        if (target != 0 && sets.length > target)
                            return null;
                    }
        return sets;
    }
}

const final class SetPuzzleDealer : SetDealer {
    enum {basic = 9, advanced = 12}

    override immutable(Card)[] deal(in uint numCards = basic) const {
        immutable numSets = numCards / 2;
        typeof(return) cards;

        do {
            cards = super.deal(numCards);
        } while (findSets(cards, numSets).length != numSets);

        return cards;
    }
}

void main() {
    const dealer = new SetPuzzleDealer;
    const cards = dealer.deal;

    writefln("DEALT %d CARDS:", cards.length);
    writefln("%(%s\n%)", cards);

    immutable sets = dealer.findSets(cards);
    immutable len = sets.length;
    writefln("\nFOUND %d SET%s:", len, len == 1 ? "" : "S");
    writefln("%(%(%s\n%)\n\n%)", sets);
}
```

```txt
DEALT 9 CARDS:
immutable(Card)(green, one, diamond, open)
immutable(Card)(green, two, diamond, open)
immutable(Card)(purple, one, diamond, striped)
immutable(Card)(purple, one, diamond, solid)
immutable(Card)(purple, two, squiggle, solid)
immutable(Card)(purple, three, oval, open)
immutable(Card)(red, one, diamond, solid)
immutable(Card)(red, one, squiggle, open)
immutable(Card)(red, three, oval, striped)

FOUND 4 SETS:
immutable(Card)(green, one, diamond, open)
immutable(Card)(purple, one, diamond, striped)
immutable(Card)(red, one, diamond, solid)

immutable(Card)(green, one, diamond, open)
immutable(Card)(purple, two, squiggle, solid)
immutable(Card)(red, three, oval, striped)

immutable(Card)(green, two, diamond, open)
immutable(Card)(purple, three, oval, open)
immutable(Card)(red, one, squiggle, open)

immutable(Card)(purple, one, diamond, striped)
immutable(Card)(purple, two, squiggle, solid)
immutable(Card)(purple, three, oval, open)
```



### Short Version

This requires the third solution module of the Combinations Task.

```d
void main() {
    import std.stdio, std.algorithm, std.range, std.random, combinations3;

    enum nDraw = 9, nGoal = nDraw / 2;
    auto deck = cartesianProduct("red green purple".split,
                                 "one two three".split,
                                 "oval squiggle diamond".split,
                                 "solid open striped".split).array;

    retry:
    auto draw = deck.randomSample(nDraw).map!(t => [t[]]).array;
    const sets = draw.combinations(3).filter!(cs => cs.dup
        .transposed.all!(t => t.array.sort().uniq.count % 2)).array;
    if (sets.length != nGoal)
        goto retry;

    writefln("Dealt %d cards:\n%(%-(%8s %)\n%)\n", draw.length, draw);
    writefln("Containing:\n%(%(%-(%8s %)\n%)\n\n%)", sets);
}
```

```txt
Dealt 9 cards:
  purple      one     oval    solid
     red    three squiggle    solid
  purple    three  diamond    solid
   green      one squiggle     open
   green      two squiggle     open
     red      two     oval  striped
  purple      one squiggle  striped
  purple      two squiggle  striped
   green    three  diamond  striped

Containing:
  purple    three  diamond    solid
   green      one squiggle     open
     red      two     oval  striped

     red    three squiggle    solid
   green      two squiggle     open
  purple      one squiggle  striped

     red    three squiggle    solid
   green      one squiggle     open
  purple      two squiggle  striped

     red      two     oval  striped
  purple      one squiggle  striped
   green    three  diamond  striped
```



## EchoLisp


```scheme

(require 'list)

;; a card is a vector  [id color number symb shading], 0 <= id < 81
(define (make-deck (id -1))
(for*/vector(
	[ color '(red green purple)]
	[ number '(one two three)]
	[ symb '( oval squiggle diamond)]
	[ shading '(solid open striped)]) (++ id) (vector id color number symb shading)))
(define DECK (make-deck))

;; pre-generate  531441 ordered triples, among which 6561 are winners
(define TRIPLES (make-vector (* 81 81 81)))
(define (make-triples )
   (for* ((i 81)(j 81)(k 81))
	(vector-set! TRIPLES (+ i (* 81 j) (* 6561 k))
		(check-set [DECK i] [DECK j] [DECK k]))))

;; a deal is a list of cards id's.
(define (show-deal deal)
   (for ((card deal)) (writeln [DECK card]))
   (for ((set (combinations deal 3)))
     (when
     (check-set [DECK (first set)] [DECK (second set)][DECK (third set)])
     (writeln 'winner set))))

;; rules of game here
(define (check-set cards: a b c)
	(for ((i (in-range 1 5))) ;; each feature
	#:continue (and (= [a i] [b i]) (= [a i] [c i]))
	#:continue (and (!= [a i] [b i]) (!= [a i] [c i]) (!= [b i][c i]))
	#:break  #t =>  #f ))

;; sets = list of triples (card-id card-id card-id)
(define (count-sets sets )
	(for/sum ((s sets))
    (if [TRIPLES ( + (first s) (* 81 (second s)) (* 6561 (third s)))]
    1 0)))


;; task
(make-triples)

(define (play (n 9) (cmax 4) (sets) (deal))
	(while #t
		(set! deal (take (shuffle (iota 81)) n))
		(set! sets (combinations deal 3))
		#:break (= (count-sets sets) cmax) => (show-deal deal)
		))


```

```txt

(play) ;; The 9-4 game by default
#( 13 red two squiggle open)
#( 54 purple one oval solid)
#( 2 red one oval striped)
#( 15 red two diamond solid)
#( 53 green three diamond striped)
#( 48 green three squiggle solid)
#( 41 green two squiggle striped)
#( 66 purple two squiggle solid)
#( 64 purple two oval open)
winner     (13 54 53)
winner     (13 41 66)
winner     (54 15 48)
winner     (15 41 64)
;; 10 deals

(play 12 6)
#( 43 green two diamond open)
#( 16 red two diamond open)
#( 79 purple three diamond open)
#( 63 purple two oval solid)
#( 60 purple one diamond solid)
#( 75 purple three squiggle solid)
#( 64 purple two oval open)
#( 71 purple two diamond striped)
#( 67 purple two squiggle open)
#( 34 green one diamond open)
#( 59 purple one squiggle striped)
#( 54 purple one oval solid)
winner     (16 79 34)
winner     (79 63 59)
winner     (79 60 71)
winner     (63 60 75)
winner     (63 71 67)
winner     (75 67 59)
;; 31 deals

;; the (9 6) game is more difficult
#( 11 red two oval striped)
#( 9 red two oval solid)
#( 26 red three diamond striped)
#( 5 red one squiggle striped)
#( 60 purple one diamond solid)
#( 43 green two diamond open)
#( 10 red two oval open)
#( 67 purple two squiggle open)
#( 48 green three squiggle solid)
winner     (11 9 10)
winner     (11 26 5)
winner     (9 60 48)
winner     (26 60 43)
winner     (5 67 48)
winner     (43 10 67)
;; 17200 deals

```



## Elixir

```elixir
defmodule RC do
  def set_puzzle(deal, goal) do
    {puzzle, sets} = get_puzzle_and_answer(deal, goal, produce_deck)
    IO.puts "Dealt #{length(puzzle)} cards:"
    print_cards(puzzle)
    IO.puts "Containing #{length(sets)} sets:"
    Enum.each(sets, fn set -> print_cards(set) end)
  end

  defp get_puzzle_and_answer(hand_size, num_sets_goal, deck) do
    hand = Enum.take_random(deck, hand_size)
    sets = get_all_sets(hand)
    if length(sets) == num_sets_goal do
      {hand, sets}
    else
      get_puzzle_and_answer(hand_size, num_sets_goal, deck)
    end
  end

  defp get_all_sets(hand) do
    Enum.filter(comb(hand, 3), fn candidate ->
      List.flatten(candidate)
      |> Enum.group_by(&(&1))
      |> Map.values
      |> Enum.all?(fn v -> length(v) != 2 end)
    end)
  end

  defp print_cards(cards) do
    Enum.each(cards, fn card ->
      :io.format "  ~-8s  ~-8s  ~-8s  ~-8s~n", card
    end)
    IO.puts ""
  end

  @colors   ~w(red green purple)a
  @symbols  ~w(oval squiggle diamond)a
  @numbers  ~w(one two three)a
  @shadings ~w(solid open striped)a

  defp produce_deck do
    for color <- @colors, symbol <- @symbols, number <- @numbers, shading <- @shadings,
      do: [color, symbol, number, shading]
  end

  defp comb(_, 0), do: [[]]
  defp comb([], _), do: []
  defp comb([h|t], m) do
    (for l <- comb(t, m-1), do: [h|l]) ++ comb(t, m)
  end
end

RC.set_puzzle(9, 4)
RC.set_puzzle(12, 6)
```


```txt

Dealt 9 cards:
  green     oval      one       open
  red       oval      one       open
  red       oval      two       open
  green     diamond   two       striped
  green     diamond   three     open
  green     diamond   one       open
  purple    squiggle  one       open
  red       oval      three     solid
  red       oval      three     open

Containing 4 sets:
  red       oval      one       open
  red       oval      two       open
  red       oval      three     open

  red       oval      one       open
  green     diamond   one       open
  purple    squiggle  one       open

  red       oval      two       open
  green     diamond   three     open
  purple    squiggle  one       open

  green     diamond   two       striped
  purple    squiggle  one       open
  red       oval      three     solid

Dealt 12 cards:
  purple    oval      one       open
  purple    diamond   two       open
  red       oval      three     striped
  purple    diamond   three     striped
  purple    oval      one       solid
  red       oval      two       open
  green     diamond   three     open
  green     squiggle  one       solid
  green     oval      three     striped
  red       diamond   two       solid
  red       diamond   one       solid
  green     squiggle  three     striped

Containing 6 sets:
  purple    oval      one       open
  red       diamond   two       solid
  green     squiggle  three     striped

  purple    diamond   two       open
  red       oval      three     striped
  green     squiggle  one       solid

  red       oval      three     striped
  purple    diamond   three     striped
  green     squiggle  three     striped

  purple    diamond   three     striped
  red       oval      two       open
  green     squiggle  one       solid

  purple    oval      one       solid
  red       oval      two       open
  green     oval      three     striped

  purple    oval      one       solid
  green     squiggle  one       solid
  red       diamond   one       solid

```



## Erlang

Until a better solution is found this is one.

```Erlang

-module( set ).

-export( [deck/0, is_set/3, shuffle_deck/1, task/0] ).

-record( card, {number, symbol, shading, colour} ).

deck() -> [#card{number=N, symbol=Sy, shading=Sh, colour=C} || N <- [1,2,3], Sy <- [diamond, squiggle, oval], Sh <- [solid, striped, open], C <- [red, green, purple]].

is_set( Card1, Card2, Card3 ) ->
        is_colour_correct( Card1, Card2, Card3 )
        andalso is_number_correct( Card1, Card2, Card3 )
        andalso is_shading_correct( Card1, Card2, Card3 )
        andalso is_symbol_correct( Card1, Card2, Card3 ).

shuffle_deck( Deck ) -> knuth_shuffle:list( Deck ).

task() ->
    basic(),
    advanced().



advanced() -> common( 6, 12 ).

basic() -> common( 4, 9 ).

common(	X, Y ) ->
    {Sets, Cards} = find_x_sets_in_y_cards( X, Y, deck() ),
    io:fwrite( "Cards ~p~n", [Cards] ),
    io:fwrite( "Gives sets:~n" ),
    [io:fwrite( "~p~n", [S] ) || S <- Sets].

find_x_sets_in_y_cards( X, Y, Deck ) ->
    {Cards, _T} = lists:split( Y, shuffle_deck(Deck) ),
    find_x_sets_in_y_cards( X, Y, Cards, make_sets1(Cards, []) ).

find_x_sets_in_y_cards( X, _Y, _Deck, Cards, Sets ) when erlang:length(Sets) =:= X -> {Sets, Cards};
find_x_sets_in_y_cards( X, Y, Deck, _Cards, _Sets ) -> find_x_sets_in_y_cards( X, Y, Deck ).

is_colour_correct( Card1, Card2, Card3 ) -> is_colour_different( Card1, Card2, Card3 ) orelse is_colour_same( Card1, Card2, Card3 ).

is_colour_different( #card{colour=C1}, #card{colour=C2}, #card{colour=C3} ) when C1 =/= C2, C1 =/= C3, C2 =/= C3 -> true;
is_colour_different( _Card1, _Card2, _Card3 ) -> false.

is_colour_same( #card{colour=C}, #card{colour=C}, #card{colour=C} ) -> true;
is_colour_same( _Card1, _Card2, _Card3 ) -> false.

is_number_correct( Card1, Card2, Card3 ) -> is_number_different( Card1, Card2, Card3 ) orelse is_number_same( Card1, Card2, Card3 ).

is_number_different( #card{number=N1}, #card{number=N2}, #card{number=N3} ) when N1 =/= N2, N1 =/= N3, N2 =/= N3 -> true;
is_number_different( _Card1, _Card2, _Card3 ) -> false.

is_number_same( #card{number=N}, #card{number=N}, #card{number=N} ) -> true;
is_number_same( _Card1, _Card2, _Card3 ) -> false.

is_shading_correct( Card1, Card2, Card3 ) -> is_shading_different( Card1, Card2, Card3 ) orelse is_shading_same( Card1, Card2, Card3 ).

is_shading_different( #card{shading=S1}, #card{shading=S2}, #card{shading=S3} ) when S1 =/= S2, S1 =/= S3, S2 =/= S3 -> true;
is_shading_different( _Card1, _Card2, _Card3 ) -> false.

is_shading_same( #card{shading=S}, #card{shading=S}, #card{shading=S} ) -> true;
is_shading_same( _Card1, _Card2, _Card3 ) -> false.

is_symbol_correct( Card1, Card2, Card3 ) -> is_symbol_different( Card1, Card2, Card3 ) orelse is_symbol_same( Card1, Card2, Card3 ).

is_symbol_different( #card{symbol=S1}, #card{symbol=S2}, #card{symbol=S3} ) when S1 =/= S2, S1 =/= S3, S2 =/= S3 -> true;
is_symbol_different( _Card1, _Card2, _Card3 ) -> false.

is_symbol_same( #card{symbol=S}, #card{symbol=S}, #card{symbol=S} ) -> true;
is_symbol_same( _Card1, _Card2, _Card3 ) -> false.
%% Nested loops 1, 2 and 3
make_sets1( [_Second_to_last, _Last], Sets ) -> Sets;
make_sets1( [Card | T], Sets ) -> make_sets1( T, make_sets2(Card, T, Sets) ).

make_sets2( _Card, [_Last], Sets ) -> Sets;
make_sets2( Card1, [Card2 | T], Sets ) -> make_sets2( Card1, T, make_sets3( Card1, Card2, T,  Sets) ).

make_sets3( _Card1, _Card2, [], Sets ) -> Sets;
make_sets3( Card1, Card2, [Card3 | T], Sets ) ->
        make_sets3( Card1, Card2, T, make_sets_acc(is_set(Card1, Card2, Card3), {Card1, Card2, Card3}, Sets) ).

make_sets_acc( true, Set, Sets ) -> [Set | Sets];
make_sets_acc( false, _Set, Sets ) -> Sets.

```

```txt

53> set:task().
Cards [{card,2,diamond,striped,purple},
       {card,3,squiggle,solid,purple},
       {card,2,squiggle,open,red},
       {card,3,oval,solid,purple},
       {card,1,diamond,striped,green},
       {card,1,oval,open,purple},
       {card,3,squiggle,striped,purple},
       {card,2,diamond,solid,purple},
       {card,1,oval,striped,purple}]
Gives sets:
{{card,1,oval,open,purple},
 {card,3,squiggle,striped,purple},
 {card,2,diamond,solid,purple}}
{{card,2,squiggle,open,red},
 {card,3,oval,solid,purple},
 {card,1,diamond,striped,green}}
{{card,2,diamond,striped,purple},
 {card,3,squiggle,striped,purple},
 {card,1,oval,striped,purple}}
{{card,2,diamond,striped,purple},
 {card,3,squiggle,solid,purple},
 {card,1,oval,open,purple}}
Cards [{card,1,diamond,striped,purple},
       {card,3,diamond,solid,purple},
       {card,2,diamond,solid,green},
       {card,1,diamond,open,green},
       {card,3,oval,striped,red},
       {card,3,squiggle,striped,red},
       {card,2,oval,solid,purple},
       {card,1,squiggle,open,green},
       {card,3,diamond,solid,green},
       {card,2,diamond,striped,red},
       {card,2,squiggle,solid,purple},
       {card,3,oval,open,purple}]
Gives sets:
{{card,3,squiggle,striped,red},
 {card,3,diamond,solid,green},
 {card,3,oval,open,purple}}
{{card,3,squiggle,striped,red},
 {card,1,squiggle,open,green},
 {card,2,squiggle,solid,purple}}
{{card,1,diamond,open,green},
 {card,3,squiggle,striped,red},
 {card,2,oval,solid,purple}}
{{card,1,diamond,open,green},
 {card,3,oval,striped,red},
 {card,2,squiggle,solid,purple}}
{{card,3,diamond,solid,purple},
 {card,1,diamond,open,green},
 {card,2,diamond,striped,red}}
{{card,1,diamond,striped,purple},
 {card,2,squiggle,solid,purple},
 {card,3,oval,open,purple}}

```


=={{header|F Sharp|F#}}==

```fsharp
open System

type Number = One | Two | Three
type Color = Red | Green | Purple
type Fill = Solid | Open | Striped
type Symbol = Oval | Squiggle | Diamond

type Card = { Number: Number; Color: Color; Fill: Fill; Symbol: Symbol }

// A 'Set' is 3 cards in which each individual feature is either all the SAME on each card, OR all DIFFERENT on each card.
let SetSize = 3

type CardsGenerator() =
    let _rand = Random()

    let shuffleInPlace data =
        Array.sortInPlaceBy (fun _ -> (_rand.Next(0, Array.length data))) data

    let createCards() =
        [| for n in [One; Two; Three] do
                for c in [Red; Green; Purple] do
                    for f in [Solid; Open; Striped] do
                        for s in [Oval; Squiggle; Diamond] do
                            yield { Number = n; Color = c; Fill = f; Symbol = s } |]

    let _cards = createCards()

    member x.GetHand cardCount =
        shuffleInPlace _cards
        Seq.take cardCount _cards |> Seq.toList

// Find all the combinations of n elements
let rec combinations n items =
    match n, items with
    | 0, _  -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

let validCardSet (cards: Card list) =
    // Valid feature if all features are the same or different
    let validFeature = function
        | [a; b; c] -> (a = b && b = c) || (a <> b && a <> c && b <> c)
        | _ -> false

    // Build and validate the feature lists
    let isValid = cards |> List.fold (fun (ns, cs, fs, ss) c ->
                               (c.Number::ns, c.Color::cs, c.Fill::fs, c.Symbol::ss)) ([], [], [], [])
                        |> fun (ns, cs, fs, ss) ->
                               (validFeature ns) && (validFeature cs) && (validFeature fs) && (validFeature ss)

    if isValid then Some cards else None

let findSolution cardCount setCount =
    let cardsGen = CardsGenerator()

    let rec search () =
        let hand = cardsGen.GetHand cardCount
        let foundSets = combinations SetSize hand |> List.choose validCardSet

        if foundSets.Length = setCount then (hand, foundSets) else search()

    search()

let displaySolution (hand: Card list, sets: Card list list) =
    let printCardDetails (c: Card) =
        printfn "    %A %A %A %A" c.Number c.Color c.Symbol c.Fill

    printfn "Dealt %d cards:" hand.Length
    List.iter printCardDetails hand
    printf "\n"

    printfn "Found %d sets:" sets.Length
    sets |> List.iter (fun cards -> List.iter printCardDetails cards; printf "\n" )

let playGame() =
    let solve cardCount setCount =
        displaySolution (findSolution cardCount setCount)

    solve 9 4
    solve 12 6

playGame()
```

Output:

```txt

Dealt 9 cards:
    Three Red Diamond Solid
    Two Red Oval Solid
    Three Red Oval Striped
    Two Purple Oval Striped
    One Green Squiggle Open
    One Purple Diamond Solid
    One Green Oval Striped
    One Green Diamond Solid
    Three Purple Diamond Striped

Found 4 sets:
    Three Red Diamond Solid
    Two Purple Oval Striped
    One Green Squiggle Open

    Two Red Oval Solid
    One Green Squiggle Open
    Three Purple Diamond Striped

    Three Red Oval Striped
    Two Purple Oval Striped
    One Green Oval Striped

    One Green Squiggle Open
    One Green Oval Striped
    One Green Diamond Solid

Dealt 12 cards:
    One Green Diamond Open
    Two Red Diamond Striped
    Three Red Oval Striped
    One Red Diamond Open
    Three Green Oval Open
    Two Purple Squiggle Solid
    Two Red Oval Striped
    One Red Oval Striped
    Two Red Oval Open
    Three Purple Oval Striped
    One Purple Diamond Open
    Three Red Oval Solid

Found 6 sets:
    One Green Diamond Open
    Three Red Oval Striped
    Two Purple Squiggle Solid

    One Green Diamond Open
    One Red Diamond Open
    One Purple Diamond Open

    Three Red Oval Striped
    Two Red Oval Striped
    One Red Oval Striped

    Three Green Oval Open
    Three Purple Oval Striped
    Three Red Oval Solid

    Two Purple Squiggle Solid
    Three Purple Oval Striped
    One Purple Diamond Open

    One Red Oval Striped
    Two Red Oval Open
    Three Red Oval Solid
```



## Factor


```factor
USING: arrays backtrack combinators.short-circuit formatting
fry grouping io kernel literals math.combinatorics math.matrices
prettyprint qw random sequences sets ;
IN: rosetta-code.set-puzzle

CONSTANT: deck $[
    [
        qw{ red green purple } amb-lazy
        qw{ one two three } amb-lazy
        qw{ oval squiggle diamond } amb-lazy
        qw{ solid open striped } amb-lazy 4array
    ] bag-of
]

: valid-category? ( seq -- ? )
    { [ all-equal? ] [ all-unique? ] } 1|| ;

: valid-set? ( seq -- ? )
    [ valid-category? ] column-map t [ and ] reduce ;

: find-sets ( seq -- seq )
    3 <combinations> [ valid-set? ] filter ;

: deal-hand ( m n -- seq valid? )
    [ deck swap sample ] dip over find-sets length = ;

: find-valid-hand ( m n -- seq )
    [ f ] 2dip '[ drop _ _ deal-hand not ] loop ;

: set-puzzle ( m n -- )
    [ find-valid-hand ] 2keep
    [ "Dealt %d cards:\n" printf simple-table. nl ]
    [
        "Containing %d sets:\n" printf find-sets
        { { " " " " " " " " } } join simple-table. nl
    ] bi-curry* bi ;

: main ( -- )
    9  4 set-puzzle
    12 6 set-puzzle ;

MAIN: main
```

<pre style="height:65ex">
Dealt 9 cards:
purple one   diamond  striped
purple three squiggle open
purple one   oval     solid
green  two   squiggle striped
red    one   oval     striped
green  three oval     solid
purple three diamond  striped
red    two   oval     striped
purple two   diamond  striped

Containing 4 sets:
purple one   diamond  striped
purple three diamond  striped
purple two   diamond  striped

purple three squiggle open
purple one   oval     solid
purple two   diamond  striped

green  two   squiggle striped
red    one   oval     striped
purple three diamond  striped

green  two   squiggle striped
red    two   oval     striped
purple two   diamond  striped

Dealt 12 cards:
green  one   oval     striped
red    two   squiggle striped
red    two   diamond  open
purple two   oval     solid
green  three squiggle open
purple one   squiggle striped
purple two   squiggle open
red    two   squiggle solid
red    three oval     open
purple one   oval     solid
red    one   diamond  striped
red    two   oval     striped

Containing 6 sets:
green  one   oval     striped
purple two   oval     solid
red    three oval     open

green  one   oval     striped
purple one   squiggle striped
red    one   diamond  striped

red    two   diamond  open
red    two   squiggle solid
red    two   oval     striped

purple two   oval     solid
green  three squiggle open
red    one   diamond  striped

green  three squiggle open
purple one   squiggle striped
red    two   squiggle solid

red    two   squiggle solid
red    three oval     open
red    one   diamond  striped

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

const (
    number = [3]string{"1", "2", "3"}
    color  = [3]string{"red", "green", "purple"}
    shade  = [3]string{"solid", "open", "striped"}
    shape  = [3]string{"oval", "squiggle", "diamond"}
)

type card int

func (c card) String() string {
    return fmt.Sprintf("%s %s %s %s",
        number[c/27],
        color[c/9%3],
        shade[c/3%3],
        shape[c%3])
}

func main() {
    rand.Seed(time.Now().Unix())
    game("Basic", 9, 4)
    game("Advanced", 12, 6)
}

func game(level string, cards, sets int) {
    // create deck
    d := make([]card, 81)
    for i := range d {
        d[i] = card(i)
    }
    var found [][3]card
    for len(found) != sets {
        found = found[:0]
        // deal
        for i := 0; i < cards; i++ {
            j := rand.Intn(81 - i)
            d[i], d[j] = d[j], d[i]
        }
        //  consider all triplets
        for i := 2; i < cards; i++ {
            c1 := d[i]
            for j := 1; j < i; j++ {
                c2 := d[j]
            l3:
                for _, c3 := range d[:j] {
                    for f := card(1); f < 81; f *= 3 {
                        if (c1/f%3 + c2/f%3 + c3/f%3) % 3 != 0 {
                            continue l3 // not a set
                        }
                    }
                    // it's a set
                    found = append(found, [3]card{c1, c2, c3})
                }
            }
        }
    }
    // found the right number
    fmt.Printf("%s game.  %d cards, %d sets.\n", level, cards, sets)
    fmt.Println("Cards:")
    for _, c := range d[:cards] {
        fmt.Println("  ", c)
    }
    fmt.Println("Sets:")
    for _, s := range found {
        fmt.Printf("  %s\n  %s\n  %s\n",s[0],s[1],s[2])
    }
}
```

```txt

Basic game.  9 cards, 4 sets.
Cards:
   3 red solid oval
   3 red open oval
   3 purple striped oval
   2 green striped oval
   2 red solid oval
   1 purple open diamond
   2 purple solid squiggle
   1 green striped diamond
   3 green striped squiggle
Sets:
   2 purple solid squiggle
   1 purple open diamond
   3 purple striped oval

   1 green striped diamond
   2 purple solid squiggle
   3 red open oval

   3 green striped squiggle
   1 purple open diamond
   2 red solid oval

   3 green striped squiggle
   1 green striped diamond
   2 green striped oval

Advanced game.  12 cards, 6 sets.
Cards:
   2 green solid squiggle
   3 red solid oval
   3 purple open oval
   2 purple open squiggle
   3 red striped oval
   1 red open oval
   1 purple open diamond
   1 green striped squiggle
   3 red open oval
   3 red striped squiggle
   2 red striped oval
   1 purple solid diamond
Sets:
   1 purple open diamond
   2 purple open squiggle
   3 purple open oval

   1 purple open diamond
   3 red striped oval
   2 green solid squiggle

   3 red open oval
   3 red striped oval
   3 red solid oval

   2 red striped oval
   1 red open oval
   3 red solid oval

   1 purple solid diamond
   3 red solid oval
   2 green solid squiggle

   1 purple solid diamond
   1 green striped squiggle
   1 red open oval

```



## Haskell


```haskell
import Control.Monad.State
       (State, evalState, replicateM, runState, state)
import System.Random (StdGen, newStdGen, randomR)
import Data.List (find, nub, sort)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (y:ys) = map (y :) (combinations (k - 1) ys) ++ combinations k ys

data Color
  = Red
  | Green
  | Purple
  deriving (Show, Enum, Bounded, Ord, Eq)

data Symbol
  = Oval
  | Squiggle
  | Diamond
  deriving (Show, Enum, Bounded, Ord, Eq)

data Count
  = One
  | Two
  | Three
  deriving (Show, Enum, Bounded, Ord, Eq)

data Shading
  = Solid
  | Open
  | Striped
  deriving (Show, Enum, Bounded, Ord, Eq)

data Card = Card
  { color :: Color
  , symbol :: Symbol
  , count :: Count
  , shading :: Shading
  } deriving (Show)

-- Identify a set of three cards by counting all attribute types.
-- if each count is 3 or 1 ( not 2 ) the the cards compose a set.
isSet :: [Card] -> Bool
isSet cs =
  let total = length . nub . sort . flip map cs
  in notElem 2 [total color, total symbol, total count, total shading]

-- Get a random card from a deck. Returns the card and removes it from the deck.
getCard :: State (StdGen, [Card]) Card
getCard =
  state $
  \(gen, cs) ->
     let (i, newGen) = randomR (0, length cs - 1) gen
         (a, b) = splitAt i cs
     in (head b, (newGen, a ++ tail b))

-- Get a hand of cards.  Starts with new deck and then removes the
-- appropriate number of cards from that deck.
getHand :: Int -> State StdGen [Card]
getHand n =
  state $
  \gen ->
     let az = [minBound .. maxBound]
         deck =
           [ Card co sy ct sh
           | co <- az
           , sy <- az
           , ct <- az
           , sh <- az ]
         (a, (newGen, _)) = runState (replicateM n getCard) (gen, deck)
     in (a, newGen)

-- Get an unbounded number of hands of the appropriate number of cards.
getManyHands :: Int -> State StdGen [[Card]]
getManyHands n = (sequence . repeat) (getHand n)

-- Deal out hands of the appropriate size until one with the desired number
-- of sets is found.  then print the hand and the sets.
showSolutions :: Int -> Int -> IO ()
showSolutions cardCount solutionCount = do
  putStrLn $
    "Showing hand of " ++
    show cardCount ++ " cards with " ++ show solutionCount ++ " solutions."
  gen <- newStdGen
  let Just z =
        find ((solutionCount ==) . length . filter isSet . combinations 3) $
        evalState (getManyHands cardCount) gen
  mapM_ print z
  putStrLn ""
  putStrLn "Solutions:"
  mapM_ putSet $ filter isSet $ combinations 3 z
  where
    putSet st = do
      mapM_ print st
      putStrLn ""

-- Show a hand of 9 cards with 4 solutions
-- and a hand of 12 cards with 6 solutions.
main :: IO ()
main = do
  showSolutions 9 4
  showSolutions 12 6
```

<pre style="font-size:80%">Showing hand of 9 cards with 4 solutions.
Card {color = Red, symbol = Diamond, count = Two, shading = Open}
Card {color = Purple, symbol = Diamond, count = Two, shading = Open}
Card {color = Red, symbol = Oval, count = Two, shading = Open}
Card {color = Green, symbol = Squiggle, count = Two, shading = Striped}
Card {color = Red, symbol = Squiggle, count = Two, shading = Open}
Card {color = Red, symbol = Diamond, count = One, shading = Striped}
Card {color = Green, symbol = Diamond, count = Three, shading = Solid}
Card {color = Purple, symbol = Squiggle, count = One, shading = Solid}
Card {color = Purple, symbol = Oval, count = Three, shading = Striped}

Solutions:
Card {color = Red, symbol = Diamond, count = Two, shading = Open}
Card {color = Red, symbol = Oval, count = Two, shading = Open}
Card {color = Red, symbol = Squiggle, count = Two, shading = Open}

Card {color = Purple, symbol = Diamond, count = Two, shading = Open}
Card {color = Red, symbol = Diamond, count = One, shading = Striped}
Card {color = Green, symbol = Diamond, count = Three, shading = Solid}

Card {color = Purple, symbol = Diamond, count = Two, shading = Open}
Card {color = Purple, symbol = Squiggle, count = One, shading = Solid}
Card {color = Purple, symbol = Oval, count = Three, shading = Striped}

Card {color = Green, symbol = Squiggle, count = Two, shading = Striped}
Card {color = Red, symbol = Diamond, count = One, shading = Striped}
Card {color = Purple, symbol = Oval, count = Three, shading = Striped}

Showing hand of 12 cards with 6 solutions.
Card {color = Purple, symbol = Oval, count = Two, shading = Solid}
Card {color = Green, symbol = Squiggle, count = Two, shading = Striped}
Card {color = Purple, symbol = Diamond, count = Two, shading = Open}
Card {color = Green, symbol = Squiggle, count = One, shading = Open}
Card {color = Green, symbol = Oval, count = Two, shading = Open}
Card {color = Green, symbol = Oval, count = One, shading = Open}
Card {color = Green, symbol = Squiggle, count = Three, shading = Solid}
Card {color = Red, symbol = Diamond, count = Two, shading = Open}
Card {color = Green, symbol = Diamond, count = Two, shading = Open}
Card {color = Green, symbol = Oval, count = One, shading = Solid}
Card {color = Red, symbol = Squiggle, count = Two, shading = Open}
Card {color = Green, symbol = Oval, count = Three, shading = Open}

Solutions:
Card {color = Purple, symbol = Oval, count = Two, shading = Solid}
Card {color = Green, symbol = Squiggle, count = Two, shading = Striped}
Card {color = Red, symbol = Diamond, count = Two, shading = Open}

Card {color = Green, symbol = Squiggle, count = Two, shading = Striped}
Card {color = Green, symbol = Squiggle, count = One, shading = Open}
Card {color = Green, symbol = Squiggle, count = Three, shading = Solid}

Card {color = Purple, symbol = Diamond, count = Two, shading = Open}
Card {color = Green, symbol = Oval, count = Two, shading = Open}
Card {color = Red, symbol = Squiggle, count = Two, shading = Open}

Card {color = Purple, symbol = Diamond, count = Two, shading = Open}
Card {color = Red, symbol = Diamond, count = Two, shading = Open}
Card {color = Green, symbol = Diamond, count = Two, shading = Open}

Card {color = Green, symbol = Squiggle, count = One, shading = Open}
Card {color = Green, symbol = Diamond, count = Two, shading = Open}
Card {color = Green, symbol = Oval, count = Three, shading = Open}

Card {color = Green, symbol = Oval, count = Two, shading = Open}
Card {color = Green, symbol = Oval, count = One, shading = Open}
Card {color = Green, symbol = Oval, count = Three, shading = Open}


```



## J

'''Solution:'''

```j
require 'stats/base'

Number=: ;:'one two three'
Colour=: ;:'red green purple'
Fill=: ;:'solid open striped'
Symbol=: ;:'oval squiggle diamond'
Features=: Number ; Colour ; Fill ;< Symbol
Deck=: > ; <"1 { i.@#&.> Features
sayCards=: (', ' joinstring Features {&>~ ])"1
drawRandom=: ] {~ (? #)
isSet=: *./@:(1 3 e.~ [: #@~."1 |:)"2
getSets=: [: (] #~ isSet) ] {~ 3 comb #
countSets=: #@:getSets

set_puzzle=: verb define
 target=. <. -: y
 whilst. target ~: countSets Hand do.
   Hand=. y drawRandom Deck
 end.
 echo 'Dealt ',(": y),' Cards:'
 echo sayCards sort Hand
 echo LF,'Found ',(":target),' Sets:'
 echo sayCards sort"2 getSets Hand
)
```


'''Example:'''

```j
   set_puzzle 9
Dealt 9 Cards:
one, red, solid, oval
one, green, open, squiggle
two, purple, striped, squiggle
three, red, solid, squiggle
three, red, open, oval
three, green, solid, oval
three, green, open, diamond
three, purple, open, oval
three, purple, striped, oval

Found 4 Sets:
three, red, solid, squiggle
three, green, open, diamond
three, purple, striped, oval

one, red, solid, oval
two, purple, striped, squiggle
three, green, open, diamond

one, green, open, squiggle
two, purple, striped, squiggle
three, red, solid, squiggle

three, red, open, oval
three, green, solid, oval
three, purple, striped, oval
```



## Java


```java
import java.util.*;

public class SetPuzzle {

    enum Color {

        GREEN(0), PURPLE(1), RED(2);

        private Color(int v) {
            val = v;
        }
        public final int val;
    }

    enum Number {

        ONE(0), TWO(1), THREE(2);

        private Number(int v) {
            val = v;
        }
        public final int val;
    }

    enum Symbol {

        OVAL(0), DIAMOND(1), SQUIGGLE(2);

        private Symbol(int v) {
            val = v;
        }
        public final int val;
    }

    enum Fill {

        OPEN(0), STRIPED(1), SOLID(2);

        private Fill(int v) {
            val = v;
        }
        public final int val;
    }

    private static class Card implements Comparable<Card> {

        Color c;
        Number n;
        Symbol s;
        Fill f;

        @Override
        public String toString() {
            return String.format("[Card: %s, %s, %s, %s]", c, n, s, f);
        }

        @Override
        public int compareTo(Card o) {
            return (c.val - o.c.val) * 10 + (n.val - o.n.val);
        }
    }
    private static Card[] deck;

    public static void main(String[] args) {
        deck = new Card[81];
        Color[] colors = Color.values();
        Number[] numbers = Number.values();
        Symbol[] symbols = Symbol.values();
        Fill[] fillmodes = Fill.values();
        for (int i = 0; i < deck.length; i++) {
            deck[i] = new Card();
            deck[i].c = colors[i / 27];
            deck[i].n = numbers[(i / 9) % 3];
            deck[i].s = symbols[(i / 3) % 3];
            deck[i].f = fillmodes[i % 3];
        }
        findSets(12);
    }

    private static void findSets(int numCards) {
        int target = numCards / 2;
        Card[] cards;
        Card[][] sets = new Card[target][3];
        int cnt;
        do {
            Collections.shuffle(Arrays.asList(deck));
            cards = Arrays.copyOfRange(deck, 0, numCards);
            cnt = 0;

            outer:
            for (int i = 0; i < cards.length - 2; i++) {
                for (int j = i + 1; j < cards.length - 1; j++) {
                    for (int k = j + 1; k < cards.length; k++) {
                        if (validSet(cards[i], cards[j], cards[k])) {
                            if (cnt < target)
                                sets[cnt] = new Card[]{cards[i], cards[j], cards[k]};
                            if (++cnt > target) {
                                break outer;
                            }
                        }
                    }
                }
            }
        } while (cnt != target);

        Arrays.sort(cards);

        System.out.printf("GIVEN %d CARDS:\n\n", numCards);
        for (Card c : cards) {
            System.out.println(c);
        }
        System.out.println();

        System.out.println("FOUND " + target + " SETS:\n");
        for (Card[] set : sets) {
            for (Card c : set) {
                System.out.println(c);
            }
            System.out.println();
        }
    }

    private static boolean validSet(Card c1, Card c2, Card c3) {
        int tot = 0;
        tot += (c1.c.val + c2.c.val + c3.c.val) % 3;
        tot += (c1.n.val + c2.n.val + c3.n.val) % 3;
        tot += (c1.s.val + c2.s.val + c3.s.val) % 3;
        tot += (c1.f.val + c2.f.val + c3.f.val) % 3;
        return tot == 0;
    }
}
```


```txt
GIVEN 12 CARDS:

[Card: GREEN, ONE, DIAMOND, OPEN]
[Card: GREEN, TWO, SQUIGGLE, OPEN]
[Card: GREEN, THREE, DIAMOND, STRIPED]
[Card: GREEN, THREE, DIAMOND, OPEN]
[Card: PURPLE, ONE, DIAMOND, SOLID]
[Card: PURPLE, ONE, SQUIGGLE, SOLID]
[Card: PURPLE, TWO, SQUIGGLE, SOLID]
[Card: PURPLE, THREE, DIAMOND, OPEN]
[Card: RED, ONE, SQUIGGLE, STRIPED]
[Card: RED, ONE, OVAL, STRIPED]
[Card: RED, TWO, DIAMOND, STRIPED]
[Card: RED, THREE, OVAL, STRIPED]

FOUND 6 SETS:

[Card: GREEN, TWO, SQUIGGLE, OPEN]
[Card: PURPLE, ONE, DIAMOND, SOLID]
[Card: RED, THREE, OVAL, STRIPED]

[Card: GREEN, THREE, DIAMOND, OPEN]
[Card: RED, ONE, OVAL, STRIPED]
[Card: PURPLE, TWO, SQUIGGLE, SOLID]

[Card: GREEN, THREE, DIAMOND, OPEN]
[Card: PURPLE, ONE, DIAMOND, SOLID]
[Card: RED, TWO, DIAMOND, STRIPED]

[Card: RED, ONE, SQUIGGLE, STRIPED]
[Card: RED, THREE, OVAL, STRIPED]
[Card: RED, TWO, DIAMOND, STRIPED]

[Card: RED, ONE, OVAL, STRIPED]
[Card: PURPLE, ONE, SQUIGGLE, SOLID]
[Card: GREEN, ONE, DIAMOND, OPEN]

[Card: GREEN, ONE, DIAMOND, OPEN]
[Card: RED, THREE, OVAL, STRIPED]
[Card: PURPLE, TWO, SQUIGGLE, SOLID]
```



## Julia

Plays one basic game and one advanced game.

```julia
using Random, IterTools, Combinatorics

function SetGameTM(basic = true)
    drawsize = basic ? 9 : 12
    setsneeded = div(drawsize, 2)
    setsof3 = Vector{Vector{NTuple{4, String}}}()
    draw = Vector{NTuple{4, String}}()
    deck = collect(Iterators.product(["red", "green", "purple"], ["one", "two", "three"],
        ["oval", "squiggle", "diamond"], ["solid", "open", "striped"]))

    while length(setsof3) != setsneeded
        empty!(draw)
        empty!(setsof3)
        map(x -> push!(draw, x), shuffle(deck)[1:drawsize])
        for threecards in combinations(draw, 3)
            canuse = true
            for i in 1:4
                u = length(unique(map(x->x[i], threecards)))
                if u != 3 && u != 1
                    canuse = false
                end
            end
            if canuse
                push!(setsof3, threecards)
            end
        end
    end

    println("Dealt $drawsize cards:")
    for card in draw
        println("    $card")
    end
    println("\nFormed these cards into $setsneeded sets:")
    for set in setsof3
        for card in set
            println("    $card")
        end
        println()
    end
end

SetGameTM()
SetGameTM(false)

```
 {{output}}
```txt

 Dealt 9 cards:
    ("green", "one", "oval", "open")
    ("green", "three", "diamond", "open")
    ("purple", "one", "diamond", "striped")
    ("purple", "three", "oval", "solid")
    ("red", "two", "diamond", "open")
    ("red", "one", "oval", "striped")
    ("green", "one", "squiggle", "striped")
    ("green", "two", "oval", "solid")
    ("purple", "two", "squiggle", "open")

 Formed these cards into 4 sets:
    ("green", "three", "diamond", "open")
    ("green", "one", "squiggle", "striped")
    ("green", "two", "oval", "solid")

    ("purple", "one", "diamond", "striped")
    ("purple", "three", "oval", "solid")
    ("purple", "two", "squiggle", "open")

    ("purple", "one", "diamond", "striped")
    ("red", "one", "oval", "striped")
    ("green", "one", "squiggle", "striped")

    ("purple", "three", "oval", "solid")
    ("red", "two", "diamond", "open")
    ("green", "one", "squiggle", "striped")

 Dealt 12 cards:
    ("red", "one", "squiggle", "open")
    ("green", "one", "diamond", "striped")
    ("red", "two", "oval", "solid")
    ("green", "three", "squiggle", "striped")
    ("green", "three", "squiggle", "open")
    ("red", "one", "oval", "solid")
    ("purple", "two", "oval", "striped")
    ("green", "two", "oval", "striped")
    ("green", "three", "oval", "open")
    ("purple", "two", "diamond", "open")
    ("purple", "three", "diamond", "striped")
    ("purple", "two", "squiggle", "solid")

 Formed these cards into 6 sets:
    ("red", "one", "squiggle", "open")
    ("green", "three", "squiggle", "striped")
    ("purple", "two", "squiggle", "solid")

    ("red", "one", "squiggle", "open")
    ("green", "three", "oval", "open")
    ("purple", "two", "diamond", "open")

    ("green", "one", "diamond", "striped")
    ("green", "three", "squiggle", "striped")
    ("green", "two", "oval", "striped")

    ("green", "three", "squiggle", "striped")
    ("red", "one", "oval", "solid")
    ("purple", "two", "diamond", "open")

    ("red", "one", "oval", "solid")
    ("purple", "two", "oval", "striped")
    ("green", "three", "oval", "open")

    ("purple", "two", "oval", "striped")
    ("purple", "two", "diamond", "open")
    ("purple", "two", "squiggle", "solid")

```




## Kotlin


```scala
// version 1.1.3

import java.util.Collections.shuffle

enum class Color   { RED, GREEN, PURPLE }
enum class Symbol  { OVAL, SQUIGGLE, DIAMOND }
enum class Number  { ONE, TWO, THREE }
enum class Shading { SOLID, OPEN, STRIPED }
enum class Degree  { BASIC, ADVANCED }

class Card(
    val color:   Color,
    val symbol:  Symbol,
    val number:  Number,
    val shading: Shading
) : Comparable<Card> {

    private val value =
        color.ordinal * 27 + symbol.ordinal * 9 + number.ordinal * 3  + shading.ordinal

    override fun compareTo(other: Card) = value.compareTo(other.value)

    override fun toString() = (
        color.name.padEnd(8)   +
        symbol.name.padEnd(10) +
        number.name.padEnd(7)  +
        shading.name.padEnd(7)
    ).toLowerCase()

    companion object {
        val zero = Card(Color.RED, Symbol.OVAL, Number.ONE, Shading.SOLID)
    }
}

fun createDeck() =
    List<Card>(81) {
        val col = Color.values()  [it / 27]
        val sym = Symbol.values() [it / 9 % 3]
        val num = Number.values() [it / 3 % 3]
        val shd = Shading.values()[it % 3]
        Card(col, sym, num, shd)
    }

fun playGame(degree: Degree) {
    val deck = createDeck()
    val nCards = if (degree == Degree.BASIC) 9 else 12
    val nSets = nCards / 2
    val sets = Array(nSets) { Array(3) { Card.zero } }
    var hand: Array<Card>
    outer@ while (true) {
        shuffle(deck)
        hand = deck.take(nCards).toTypedArray()
        var count = 0
        for (i in 0 until hand.size - 2) {
            for (j in i + 1 until hand.size - 1) {
                for (k in j + 1 until hand.size) {
                    val trio = arrayOf(hand[i], hand[j], hand[k])
                    if (isSet(trio)) {
                        sets[count++] = trio
                        if (count == nSets) break@outer
                    }
                }
            }
        }
    }
    hand.sort()
    println("DEALT $nCards CARDS:\n")
    println(hand.joinToString("\n"))
    println("\nCONTAINING $nSets SETS:\n")
    for (s in sets) {
        s.sort()
        println(s.joinToString("\n"))
        println()
    }
}

fun isSet(trio: Array<Card>): Boolean {
    val r1 = trio.sumBy { it.color.ordinal   } % 3
    val r2 = trio.sumBy { it.symbol.ordinal  } % 3
    val r3 = trio.sumBy { it.number.ordinal  } % 3
    val r4 = trio.sumBy { it.shading.ordinal } % 3
    return (r1 + r2 + r3 + r4) == 0
}

fun main(args: Array<String>) {
    playGame(Degree.BASIC)
    println()
    playGame(Degree.ADVANCED)
}
```


Sample output:

```txt

DEALT 9 CARDS:

red     oval      three  solid
red     diamond   two    solid
green   oval      one    open
green   oval      three  open
green   squiggle  one    open
green   diamond   one    open
purple  oval      three  striped
purple  squiggle  three  solid
purple  diamond   two    striped

CONTAINING 4 SETS:

red     oval      three  solid
green   squiggle  one    open
purple  diamond   two    striped

red     oval      three  solid
green   oval      three  open
purple  oval      three  striped

green   oval      one    open
green   squiggle  one    open
green   diamond   one    open

red     diamond   two    solid
green   squiggle  one    open
purple  oval      three  striped


DEALT 12 CARDS:

red     squiggle  two    solid
red     diamond   two    solid
red     diamond   two    open
red     diamond   two    striped
green   oval      one    open
green   oval      three  solid
green   oval      three  open
green   squiggle  one    solid
green   diamond   one    striped
purple  oval      one    solid
purple  oval      three  open
purple  squiggle  one    striped

CONTAINING 6 SETS:

red     diamond   two    open
green   oval      three  solid
purple  squiggle  one    striped

red     diamond   two    solid
red     diamond   two    open
red     diamond   two    striped

red     diamond   two    solid
green   oval      three  open
purple  squiggle  one    striped

red     squiggle  two    solid
green   diamond   one    striped
purple  oval      three  open

green   oval      one    open
green   squiggle  one    solid
green   diamond   one    striped

red     diamond   two    striped
green   squiggle  one    solid
purple  oval      three  open

```



## Mathematica

A simple brute force approach. This code highlights two things: 1) a few of Mathematica's "higher-level" functions such as Tuples and Subsets and 2) the straightforwardness enabled by the language's "dynamic typing" (more precisely, its symbolic semantics) and its usage of lists for everything (in this particular example, the fact that functions such as Tuples and Entropy can be used on lists with arbitrary content).


```Mathematica
colors = {Red, Green, Purple};
symbols = {"0", "\[TildeTilde]", "\[Diamond]"};
numbers = {1, 2, 3};
shadings = {"\[FilledSquare]", "\[Square]", "\[DoublePrime]"};

validTripleQ[l_List] := Entropy[l] != Entropy[{1, 1, 2}];
validSetQ[cards_List] := And @@ (validTripleQ /@ Transpose[cards]);

allCards = Tuples[{colors, symbols, numbers, shadings}];

deal[{numDeal_, setNum_}] := Module[{cards, count = 0},
   While[count != setNum,
    cards = RandomSample[allCards, numDeal];
    count = Count[Subsets[cards, {3}], _?validSetQ]];

   cards];

Row[{Style[#2, #1], #3, #4}] & @@@ deal[{9, 4}]
```



## PARI/GP


```parigp
dealraw(cards)=vector(cards,i,vector(4,j,1<<random(3)));
howmany(a,b,c)=hammingweight(bitor(a,bitor(b,c)));
name(v)=Str(["red","green",0,"purple"][v[1]],", ",["oval","squiggle",0,"diamond"][v[2]],", ",["one","two",0,"three"][v[3]],", ",["solid","open",0,"striped"][v[4]]);
check(D,sets)={
  my(S=List());
  for(i=1,#D-2,for(j=i+1,#D-1,for(k=j+1,#D,
    for(x=1,4,
      if(howmany(D[i][x],D[j][x],D[k][x])==2,next(2))
    );
    listput(S,[i,j,k]);
    if(#S>sets,return(0))
  )));
  if(#S==sets,Vec(S),0)
};
deal(cards,sets)={
  my(v,s);
  until(s,
    s=check(v=dealraw(cards),sets)
  );
  v=apply(name,v);
  for(i=1,cards,print(v[i]));
  for(i=1,sets,
    print("Set #"i);
    for(j=1,3,print("  "v[s[i][j]]))
  )
};
deal(9,4)
deal(12,6)
```

```txt
green, diamond, one, open
purple, squiggle, three, solid
green, squiggle, two, striped
green, oval, one, striped
purple, oval, two, striped
purple, oval, one, open
red, squiggle, one, open
green, squiggle, one, solid
red, diamond, three, solid
Set #1
  green, diamond, one, open
  green, oval, one, striped
  green, squiggle, one, solid
Set #2
  green, diamond, one, open
  purple, oval, one, open
  red, squiggle, one, open
Set #3
  purple, squiggle, three, solid
  green, squiggle, two, striped
  red, squiggle, one, open
Set #4
  green, squiggle, two, striped
  purple, oval, one, open
  red, diamond, three, solid


purple, squiggle, three, open
red, oval, two, open
purple, oval, two, solid
green, squiggle, two, solid
purple, diamond, two, striped
purple, squiggle, two, solid
green, oval, two, striped
red, oval, one, striped
red, squiggle, two, striped
green, diamond, three, solid
green, diamond, two, open
purple, oval, one, open
Set #1
  red, oval, two, open
  purple, oval, two, solid
  green, oval, two, striped
Set #2
  red, oval, two, open
  green, squiggle, two, solid
  purple, diamond, two, striped
Set #3
  purple, oval, two, solid
  red, squiggle, two, striped
  green, diamond, two, open
Set #4
  green, squiggle, two, solid
  green, oval, two, striped
  green, diamond, two, open
Set #5
  purple, diamond, two, striped
  green, oval, two, striped
  red, squiggle, two, striped
Set #6
  red, squiggle, two, striped
  green, diamond, three, solid
  purple, oval, one, open
```




## Perl

It's actually slightly simplified, since generating Enum classes
and objects would be overkill for this particular task.

```perl
#!perl
use strict;
use warnings;

# This code was adapted from the perl6 solution for this task.

# Each element of the deck is an integer, which, when written
# in octal, has four digits, which are all either 1, 2, or 4.

my $fmt = '%4o';
my @deck = grep sprintf($fmt, $_) !~ tr/124//c, 01111 .. 04444;

# Given a feature digit (1, 2, or 4), produce the feature's name.
# Note that digits 0 and 3 are unused.
my @features = map [split ' '], split /\n/,<<'';
! red   green    ! purple
! one   two      ! three
! oval  squiggle ! diamond
! solid open     ! striped

81 == @deck or die "There are ".@deck." cards (should be 81)";

# By default, draw 9 cards, but if the user
# supplied a parameter, use that.
my $draw = shift(@ARGV) || 9;
my $goal = int($draw/2);

# Get the possible combinations of 3 indices into $draw elements.
my @combinations = combine(3, 0 .. $draw-1);

my @sets;

do {
	# Shuffle the first $draw elements of @deck.
	for my $i ( 0 .. $draw-1 ) {
		my $j = $i + int rand(@deck - $i);
		@deck[$i, $j] = @deck[$j, $i];
	}

	# Find all valid sets using the shuffled elements.
	@sets = grep {
		my $or = 0;
		$or |= $_ for @deck[@$_];
		# If all colors (or whatever) are the same, then
		# a 1, 2, or 4 will result when we OR them together.
		# If they're all different, then a 7 will result.
		# If any other digit occurs, the set is invalid.
		sprintf($fmt, $or) !~ tr/1247//c;
	} @combinations;

	# Continue until there are exactly $goal valid sets.
} until @sets == $goal;

print "Drew $draw cards:\n";
for my $i ( 0 .. $#sets ) {
	print "Set ", $i+1, ":\n";
	my @cards = @deck[ @{$sets[$i]} ];
	for my $card ( @cards ) {
		my @octal = split //, sprintf '%4o', $card;
		my @f = map $features[$_][$octal[$_]], 0 .. 3;
		printf "    %-6s %-5s %-8s %s\n", @f;
	}
}

exit;

# This function is adapted from the perl5i solution for the
# RosettaCode Combinations task.
sub combine {
	my $n = shift;
	return unless @_;
	return map [$_], @_ if $n == 1;
	my $head = shift;
	my @result = combine( $n-1, @_ );
	unshift @$_, $head for @result;
	@result, combine( $n, @_ );
}

__END__

```

```txt
Drew 12 cards:
Set 1:
    red    three oval     striped
    green  three diamond  striped
    purple three squiggle striped
Set 2:
    red    three oval     striped
    purple three squiggle open
    green  three diamond  solid
Set 3:
    purple one   diamond  striped
    red    three diamond  striped
    green  two   diamond  striped
Set 4:
    green  three diamond  striped
    green  three diamond  open
    green  three diamond  solid
Set 5:
    red    three diamond  striped
    green  three oval     solid
    purple three squiggle open
Set 6:
    green  two   diamond  striped
    purple three squiggle striped
    red    one   oval     striped
```



## Perl 6

The trick here is to allocate three different bits for each enum, with the result that the cards of a matching set OR together to produce a 4-digit octal number that contains only the digits 1, 2, 4, or 7.  This OR is done by funny looking <tt>[+|]</tt>, which is the reduction form of <tt>+|</tt>, which is the numeric bitwise OR.  (Because Perl 6 stole the bare <tt>|</tt> operator for composing junctions instead.)

```perl6
enum Color (red => 0o1000, green =>  0o2000, purple => 0o4000);
enum Count (one =>  0o100, two =>     0o200, three =>   0o400);
enum Shape (oval =>  0o10, squiggle => 0o20, diamond =>  0o40);
enum Style (solid =>  0o1, open =>      0o2, striped =>   0o4);

my @deck = Color.enums X Count.enums X Shape.enums X Style.enums;

sub MAIN($DRAW = 9, $GOAL = $DRAW div 2) {
    sub show-cards(@c) { { printf "%9s%7s%10s%9s\n", @c[$_;*]».key } for ^@c }

    my @combinations = [^$DRAW].combinations(3);

    my @draw;
    repeat until (my @sets) == $GOAL {
        @draw = @deck.pick($DRAW);
        my @bits = @draw.map: { [+] @^enums».value }
        @sets = gather for @combinations -> @c {
            take @draw[@c].item when /^ <[1247]>+ $/ given ( [+|] @bits[@c] ).base(8);
        }
    }

    say "Drew $DRAW cards:";
    show-cards @draw;
    for @sets.kv -> $i, @cards {
        say "\nSet {$i+1}:";
        show-cards @cards;
    }
}
```

```txt
Drew 9 cards:
   purple    two   diamond     open
      red    two  squiggle  striped
   purple  three  squiggle     open
   purple    two  squiggle  striped
      red  three      oval  striped
      red    one   diamond  striped
   purple    two      oval    solid
    green  three   diamond    solid
      red    two  squiggle     open

Set 1:
   purple    two   diamond     open
   purple    two  squiggle  striped
   purple    two      oval    solid

Set 2:
   purple    two   diamond     open
      red    one   diamond  striped
    green  three   diamond    solid

Set 3:
      red    two  squiggle  striped
      red  three      oval  striped
      red    one   diamond  striped

Set 4:
   purple  three  squiggle     open
      red  three      oval  striped
    green  three   diamond    solid
```



## Phix

Converts cards 1..81 (that idea from C) to 1/2/4 [/7] (that idea from Perl) but inverts the validation

```Phix
function comb(sequence pool, integer needed, sequence res={}, integer done=0, sequence chosen={})
    if needed=0 then    -- got a full set
        sequence {a,b,c} = chosen
        if not find_any({3,5,6},flatten(sq_or_bits(sq_or_bits(a,b),c))) then
            res = append(res,chosen)
        end if
    elsif done+needed<=length(pool) then
        -- get all combinations with and without the next item:
        done += 1
        res = comb(pool,needed-1,res,done,append(chosen,pool[done]))
        res = comb(pool,needed,res,done,chosen)
    end if
    return res
end function

constant m124 = {1,2,4}

function card(integer n)
--returns the nth card (n is 1..81, res is length 4 of 1/2/4)
    n -= 1
    sequence res = repeat(0,4)
    for i=1 to 4 do
        res[i] = m124[remainder(n,3)+1]
        n = floor(n/3)
    end for
    return res
end function

constant colours = {"red", "green", 0, "purple"},
         symbols = {"oval", "squiggle", 0, "diamond"},
         numbers = {"one", "two", 0, "three"},
         shades  = {"solid", "open", 0, "striped"}

procedure print_cards(sequence hand, sequence cards)
    for i=1 to length(cards) do
        integer {c,m,n,g} = cards[i],
                id = find(cards[i],hand)
        printf(1,"%3d: %-7s %-9s %-6s %s\n",{id,colours[c],symbols[m],numbers[n],shades[g]})
    end for
    printf(1,"\n")
end procedure

procedure play(integer cards=9, integer sets=4)
    integer deals = 1
    while 1 do
        sequence deck = shuffle(tagset(81))
        sequence hand = deck[1..cards]
        for i=1 to length(hand) do
            hand[i] = card(hand[i])
        end for
        sequence res = comb(hand,3)
        if length(res)=sets then
            printf(1,"dealt %d cards (%d deals)\n",{cards,deals})
            print_cards(hand,hand)
            printf(1,"with %d sets\n",{sets})
            for i=1 to sets do
                print_cards(hand,res[i])
            end for
            exit
        end if
        deals += 1
    end while
end procedure
play()
--play(12,6)
--play(9,6)
```

```txt

dealt 9 cards (172 deals)
  1: red     oval      two    open
  2: green   oval      one    solid
  3: purple  diamond   two    striped
  4: green   diamond   one    striped
  5: green   oval      one    striped
  6: purple  squiggle  three  solid
  7: green   diamond   two    solid
  8: red     diamond   two    open
  9: green   squiggle  one    striped

with 4 sets
  1: red     oval      two    open
  4: green   diamond   one    striped
  6: purple  squiggle  three  solid

  3: purple  diamond   two    striped
  7: green   diamond   two    solid
  8: red     diamond   two    open

  4: green   diamond   one    striped
  5: green   oval      one    striped
  9: green   squiggle  one    striped

  5: green   oval      one    striped
  6: purple  squiggle  three  solid
  8: red     diamond   two    open

```



## Python


```python
#!/usr/bin/python

from itertools import product, combinations
from random import sample

## Major constants
features = [ 'green purple red'.split(),
             'one two three'.split(),
             'oval diamond squiggle'.split(),
             'open striped solid'.split() ]

deck = list(product(list(range(3)), repeat=4))

dealt = 9

## Functions
def printcard(card):
    print(' '.join('%8s' % f[i] for f,i in zip(features, card)))

def getdeal(dealt=dealt):
    deal = sample(deck, dealt)
    return deal

def getsets(deal):
    good_feature_count = set([1, 3])
    sets = [ comb for comb in combinations(deal, 3)
             if all( [(len(set(feature)) in good_feature_count)
                     for feature in zip(*comb)]
                   ) ]
    return sets

def printit(deal, sets):
    print('Dealt %i cards:' % len(deal))
    for card in deal: printcard(card)
    print('\nFound %i sets:' % len(sets))
    for s in sets:
        for card in s: printcard(card)
        print('')

if __name__ == '__main__':
    while True:
        deal = getdeal()
        sets = getsets(deal)
        if len(sets) == dealt / 2:
           break
    printit(deal, sets)

```

<small>Note: You could remove the inner square brackets of the <code>'if all( [...] )'</code> part of the sets computation in the getsets function. It is a remnant of Python 2.7 debugging which gives access to the name <code>feature</code>. The code works on Python 3.X too, but not that access.</small>

```txt
Dealt 9 cards:
   green    three squiggle    solid
   green    three squiggle     open
  purple      two squiggle    solid
   green      one  diamond    solid
     red    three     oval    solid
   green      two     oval    solid
     red      two     oval     open
  purple      one  diamond  striped
     red      two     oval    solid

Found 4 sets:
   green    three squiggle    solid
   green      one  diamond    solid
   green      two     oval    solid

   green    three squiggle    solid
     red      two     oval     open
  purple      one  diamond  striped

   green    three squiggle     open
  purple      one  diamond  striped
     red      two     oval    solid

  purple      two squiggle    solid
   green      one  diamond    solid
     red    three     oval    solid
```



### Short Version

```python
import random, pprint
from itertools import product, combinations

N_DRAW = 9
N_GOAL = N_DRAW // 2

deck = list(product("red green purple".split(),
                    "one two three".split(),
                    "oval squiggle diamond".split(),
                    "solid open striped".split()))

sets = []
while len(sets) != N_GOAL:
    draw = random.sample(deck, N_DRAW)
    sets = [cs for cs in combinations(draw, 3)
            if all(len(set(t)) in [1, 3] for t in zip(*cs))]

print "Dealt %d cards:" % len(draw)
pprint.pprint(draw)
print "\nContaining %d sets:" % len(sets)
pprint.pprint(sets)
```

```txt
Dealt 9 cards:
[('purple', 'three', 'squiggle', 'striped'),
 ('red', 'one', 'squiggle', 'solid'),
 ('red', 'three', 'diamond', 'striped'),
 ('red', 'two', 'oval', 'open'),
 ('purple', 'three', 'squiggle', 'open'),
 ('green', 'three', 'oval', 'open'),
 ('purple', 'three', 'squiggle', 'solid'),
 ('green', 'two', 'squiggle', 'open'),
 ('purple', 'two', 'oval', 'open')]

Containing 4 sets:
[(('purple', 'three', 'squiggle', 'striped'),
  ('red', 'one', 'squiggle', 'solid'),
  ('green', 'two', 'squiggle', 'open')),
 (('purple', 'three', 'squiggle', 'striped'),
  ('purple', 'three', 'squiggle', 'open'),
  ('purple', 'three', 'squiggle', 'solid')),
 (('red', 'one', 'squiggle', 'solid'),
  ('red', 'three', 'diamond', 'striped'),
  ('red', 'two', 'oval', 'open')),
 (('red', 'three', 'diamond', 'striped'),
  ('green', 'three', 'oval', 'open'),
  ('purple', 'three', 'squiggle', 'solid'))]
```



## Racket


```Racket

#lang racket

(struct card [bits name])

(define cards
  (for/list ([C '(red   green    purple )] [Ci '(#o0001 #o0002 #o0004)]
             #:when #t
             [S '(oval  squiggle diamond)] [Si '(#o0010 #o0020 #o0040)]
             #:when #t
             [N '(one   two      three  )] [Ni '(#o0100 #o0200 #o0400)]
             #:when #t
             [D '(solid open     striped)] [Di '(#o1000 #o2000 #o4000)])
    (card (bitwise-ior Ci Si Ni Di) (format "~a, ~a, ~a, ~a" C S N D))))

(define (nsubsets l n)
  (cond [(zero? n) '(())] [(null? l) '()]
        [else (append (for/list ([l2 (nsubsets (cdr l) (- n 1))])
                        (cons (car l) l2))
                      (nsubsets (cdr l) n))]))
(define (set? cards)
  (regexp-match? #rx"^[1247]*$"
                 (number->string (apply bitwise-ior (map card-bits cards)) 8)))

(define (deal C S)
  (define hand  (take (shuffle cards) C))
  (define 3sets (filter set? (nsubsets hand 3)))
  (cond [(not (= S (length 3sets))) (deal C S)]
        [else (printf "Dealt ~a cards:\n" C)
              (for ([c hand]) (printf "  ~a\n" (card-name c)))
              (printf "\nContaining ~a sets:\n" S)
              (for ([set 3sets])
                (for ([c set]) (printf "  ~a\n" (card-name c)))
                (newline))]))

(deal 9 4)
(deal 12 6)

```



## REXX

Language note:   each REXX implementation has its own method of determining a starter ''seed'' for generating

pseudo-random numbers, and in addition, that starter seed may be dependent upon operating system factors,

hardware architecture, and other things like the (local) date and time-of-day, and other such variables.

The algorithm is also not the same for all REXX implementations.

The particular set of cards dealt (show below) used Regina 3.90 under a Windows/XP environment.

```rexx
/*REXX program  finds  "sets" (solutions)  for the   SET  puzzle   (game).    */
parse arg game seed .                  /*get optional # cards to deal and seed*/
if game ==',' | game==''  then game=9  /*Not specified?  Then use the default.*/
if seed==','  | seed==''  then seed=77 /* "      "         "   "   "      "   */
call aGame 0                           /*with tell=0:    suppress the output. */
call aGame 1                           /*with tell=1:    display   "     "    */
exit sets                              /*stick a fork in it,  we're all done. */
/*──────────────────────────────────AGAME subroutine──────────────────────────*/
aGame: tell=arg(1);        good=game%2 /*enable/disable the showing of output.*/
                                       /* [↑]  the GOOD var is the right #sets*/
       do seed=seed  until good==sets  /*generate deals until good  # of sets.*/
       call random ,,seed              /*repeatability for the RANDOM invokes.*/
       call genFeatures                /*generate various card game features. */
       call genDeck                    /*generate a deck  (with  81  "cards").*/
       call dealer   game              /*deal a number of cards for the game. */
       call findSets game%2            /*find # of sets from the dealt cards. */
       end   /*until*/                 /* [↓]   when leaving, SETS is right #.*/
return                                 /*return to invoker of this subroutine.*/
/*──────────────────────────────────DEALER subroutine─────────────────────────*/
dealer: call sey  'dealing'  game  "cards:",,.  /*shuffle and deal the cards. */
    do cards=1  until  cards==game              /*keep dealing until finished.*/
    _=random(1,words(##));   ##=delword(##,_,1) /*pick card;   delete a card. */
    @.cards=deck._                              /*add the card to the tableau.*/
    call sey right('card' cards,30) " " @.cards /*display the card to screen. */
        do j=1  for words(@.cards)              /* [↓]  define cells for cards*/
        @.cards.j=word(@.cards,j)               /*define  a  cell for  a card.*/
        end   /*j*/
    end       /*cards*/
return
/*──────────────────────────────────DEFFEATURES subroutine────────────────────*/
defFeatures:  parse arg what,v; _=words(v)      /*obtain what is to be defined*/
if _\==values  then do;  call sey 'error,'  what  "features ¬=" values,.,.
                         exit -1
                    end                         /* [↑]  check for typos/errors*/
        do k=1  for words(values)               /*define all the possible vals*/
        call value what'.'k, word(values,k)     /*define  a  card feature.    */
        end   /*k*/
return
/*──────────────────────────────────GENDECK subroutine────────────────────────*/
genDeck: #=0;  ##=                     /*#:  cards in deck;  ##:  shuffle aid.*/
      do       num=1  for values;   xnum = word(numbers,  num)
        do     col=1  for values;   xcol = word(colors,   col)
          do   sym=1  for values;   xsym = word(symbols,  sym)
            do sha=1  for values;   xsha = word(shadings, sha)
            #=#+1;  ##=## #;     deck.#=xnum xcol xsym xsha   /*create a card.*/
            end   /*sha*/
          end     /*num*/
        end       /*sym*/
      end         /*col*/
return                                 /*#:  the number of cards in the deck. */
/*──────────────────────────────────GENFEATURES subroutine────────────────────*/
genFeatures: features=3; groups=4; values=3 /*define # features, groups, vals.*/
numbers = 'one two three'           ;       call defFeatures 'number',  numbers
colors  = 'red green purple'        ;       call defFeatures 'color',   colors
symbols = 'oval squiggle diamond'   ;       call defFeatures 'symbol',  symbols
shadings= 'solid open striped'      ;       call defFeatures 'shading', shadings
return
/*──────────────────────────────────GENPOSS subroutine────────────────────────*/
genPoss: p=0; sets=0; sep=' ───── '; !.=    /*define some REXX variables.     */
  do     i=1    for game               /* [↓]  the  IFs  eliminate duplicates.*/
    do   j=i+1  to  game
      do k=j+1  to  game
      p=p+1;               !.p.1=@.i;      !.p.2=@.j;        !.p.3=@.k
      end   /*k*/
    end     /*j*/
  end       /*i*/                      /* [↑]  generate the permutation list. */
return
/*──────────────────────────────────FINDSETS subroutine───────────────────────*/
findSets:  parse arg n;   call genPoss /*N:  the number of sets to be found.  */
call sey                               /*find any sets that were generated [↑]*/
    do         j=1  for p              /*P:  is the number of possible sets.  */
        do     f=1  for features
            do g=1  for groups;        !!.j.f.g=word(!.j.f, g)
            end   /*g*/
        end       /*f*/
    ok=1                               /*everything is peachy─kean (OK) so far*/
        do g=1  for groups; _=!!.j.1.g /*build strings to hold possibilities. */
        equ=1                          /* [↓]  handles all the equal features.*/
               do f=2  to features  while equ;         equ=equ & _==!!.j.f.g
               end   /*f*/
        dif=1
        __=!!.j.1.g                    /* [↓]  handles all  unequal  features.*/
                        do f=2  to  features  while  \equ
                        dif=dif &  (wordpos(!!.j.f.g,__)==0)
                        __=__ !!.j.f.g /*append to the string for next test.  */
                        end   /*f*/
        ok=ok &  (equ | dif)           /*now, see if all are equal or unequal.*/
        end   /*g*/

    if \ok  then iterate               /*Is this set OK?   Nope, then skip it.*/
    sets=sets+1                        /*bump the number of the sets found.   */
    call sey  right('set'  sets":  ",15)      !.j.1   sep   !.j.2    sep   !.j.3
    end   /*j*/

call sey  sets   'sets found.',.
return
/*──────────────────────────────────SEY subroutine────────────────────────────*/
sey:  if \tell  then  return           /*¬ tell?    Then suppress the output. */
if arg(2)==.  then say;    say arg(1);   if arg(3)==.  then say;          return
```

'''output''' when using the default input:

```txt

dealing 9 cards:

                        card 1   one green oval open
                        card 2   two purple squiggle striped
                        card 3   one green diamond solid
                        card 4   three red diamond open
                        card 5   two purple squiggle striped
                        card 6   two purple oval striped
                        card 7   two purple diamond striped
                        card 8   three red squiggle open
                        card 9   two red oval solid

       set 1:   two purple squiggle striped  ─────  two purple oval striped  ─────  two purple diamond striped
       set 2:   one green diamond solid  ─────  three red diamond open  ─────  two purple diamond striped
       set 3:   one green diamond solid  ─────  two purple oval striped  ─────  three red squiggle open
       set 4:   two purple squiggle striped  ─────  two purple oval striped  ─────  two purple diamond striped

4 sets found.

```

'''output''' when using the input of:   <tt> 12 </tt>

```txt

dealing 12 cards:

                        card 1   one purple diamond striped
                        card 2   one green diamond striped
                        card 3   one purple squiggle solid
                        card 4   one red oval solid
                        card 5   two green oval open
                        card 6   one green diamond open
                        card 7   two green squiggle striped
                        card 8   three green squiggle solid
                        card 9   three green squiggle open
                       card 10   one purple diamond open
                       card 11   three green squiggle open
                       card 12   two red oval open

       set 1:   one purple diamond striped  ─────  three green squiggle solid  ─────  two red oval open
       set 2:   one green diamond striped  ─────  two green oval open  ─────  three green squiggle solid
       set 3:   two green oval open  ─────  one green diamond open  ─────  three green squiggle open
       set 4:   two green oval open  ─────  one green diamond open  ─────  three green squiggle open
       set 5:   three green squiggle open  ─────  one purple diamond open  ─────  two red oval open
       set 6:   one purple diamond open  ─────  three green squiggle open  ─────  two red oval open

6 sets found.

```



## Ruby

Brute force.

```ruby
COLORS   = %i(red green purple) #use [:red, :green, :purple] in Ruby < 2.0
SYMBOLS  = %i(oval squiggle diamond)
NUMBERS  = %i(one two three)
SHADINGS = %i(solid open striped)
DECK = COLORS.product(SYMBOLS, NUMBERS, SHADINGS)

def get_all_sets(hand)
  hand.combination(3).select do |candidate|
    grouped_features = candidate.flatten.group_by{|f| f}
    grouped_features.values.none?{|v| v.size == 2}
  end
end

def get_puzzle_and_answer(hand_size, num_sets_goal)
  begin
    hand = DECK.sample(hand_size)
    sets = get_all_sets(hand)
  end until sets.size == num_sets_goal
  [hand, sets]
end

def print_cards(cards)
  puts cards.map{|card| "  %-8s" * 4 % card}
  puts
end

def set_puzzle(deal, goal=deal/2)
  puzzle, sets = get_puzzle_and_answer(deal, goal)
  puts "Dealt #{puzzle.size} cards:"
  print_cards(puzzle)
  puts "Containing #{sets.size} sets:"
  sets.each{|set| print_cards(set)}
end

set_puzzle(9)
set_puzzle(12)
```

```txt

Dealt 9 cards:
  red       diamond   two       open
  red       squiggle  three     open
  red       diamond   two       striped
  red       diamond   two       solid
  red       oval      three     striped
  green     squiggle  three     open
  red       oval      three     open
  red       squiggle  one       striped
  red       oval      one       open

Containing 4 sets:
  red       diamond   two       open
  red       squiggle  three     open
  red       oval      one       open

  red       diamond   two       open
  red       diamond   two       striped
  red       diamond   two       solid

  red       diamond   two       striped
  red       oval      three     striped
  red       squiggle  one       striped

  red       diamond   two       solid
  red       oval      three     open
  red       squiggle  one       striped

Dealt 12 cards:
  red       diamond   three     solid
  red       diamond   three     striped
  purple    squiggle  one       striped
  purple    oval      two       striped
  green     diamond   two       open
  purple    oval      three     open
  red       diamond   one       striped
  green     oval      one       solid
  purple    squiggle  two       solid
  green     oval      two       open
  red       oval      two       striped
  red       diamond   two       striped

Containing 6 sets:
  red       diamond   three     solid
  purple    squiggle  one       striped
  green     oval      two       open

  red       diamond   three     solid
  green     oval      one       solid
  purple    squiggle  two       solid

  red       diamond   three     striped
  red       diamond   one       striped
  red       diamond   two       striped

  green     diamond   two       open
  purple    squiggle  two       solid
  red       oval      two       striped

  purple    oval      three     open
  green     oval      one       solid
  red       oval      two       striped

  purple    squiggle  two       solid
  green     oval      two       open
  red       diamond   two       striped

```



## Tcl

The principle behind this code is that the space of possible solutions is a substantial proportion of the space of possible hands, so picking a random hand and verifying that it is a solution, repeating until that verification succeeds, is a much quicker way to find a solution than a systematic search.
It also makes the code substantially simpler.

```tcl
# Generate random integer uniformly on range [0..$n-1]
proc random n {expr {int(rand() * $n)}}

# Generate a shuffled deck of all cards; the card encoding was stolen from the
# Perl6 solution. This is done once and then used as a constant. Note that the
# rest of the code assumes that all cards in the deck are unique.
set ::AllCards [apply {{} {
    set cards {}
    foreach color {1 2 4} {
	foreach symbol {1 2 4} {
	    foreach number {1 2 4} {
		foreach shading {1 2 4} {
		    lappend cards [list $color $symbol $number $shading]
		}
	    }
	}
    }
    # Knuth-Morris-Pratt shuffle (not that it matters)
    for {set i [llength $cards]} {$i > 0} {} {
	set j [random $i]
	set tmp [lindex $cards [incr i -1]]
	lset cards $i [lindex $cards $j]
	lset cards $j $tmp
    }
    return $cards
}}]

# Randomly pick a hand of cards from the deck (itself in a global for
# convenience).
proc drawCards n {
    set cards $::AllCards;    # Copies...
    for {set i 0} {$i < $n} {incr i} {
	set idx [random [llength $cards]]
	lappend hand [lindex $cards $idx]
	set cards [lreplace $cards $idx $idx]
    }
    return $hand
}

# Test if a particular group of three cards is a valid set
proc isValidSet {a b c} {
    expr {
	  ([lindex $a 0]|[lindex $b 0]|[lindex $c 0]) in {1 2 4 7} &&
	  ([lindex $a 1]|[lindex $b 1]|[lindex $c 1]) in {1 2 4 7} &&
	  ([lindex $a 2]|[lindex $b 2]|[lindex $c 2]) in {1 2 4 7} &&
	  ([lindex $a 3]|[lindex $b 3]|[lindex $c 3]) in {1 2 4 7}
    }
}

# Get all unique valid sets of three cards in a hand.
proc allValidSets {hand} {
    set sets {}
    for {set i 0} {$i < [llength $hand]} {incr i} {
	set a [lindex $hand $i]
	set hand [set cards2 [lreplace $hand $i $i]]
	for {set j 0} {$j < [llength $cards2]} {incr j} {
	    set b [lindex $cards2 $j]
	    set cards2 [set cards3 [lreplace $cards2 $j $j]]
	    foreach c $cards3 {
		if {[isValidSet $a $b $c]} {
		    lappend sets [list $a $b $c]
		}
	    }
	}
    }
    return $sets
}

# Solve a particular version of the set puzzle, by picking random hands until
# one is found that satisfies the constraints. This is usually much faster
# than a systematic search. On success, returns the hand found and the card
# sets within that hand.
proc SetPuzzle {numCards numSets} {
    while 1 {
	set hand [drawCards $numCards]
	set sets [allValidSets $hand]
	if {[llength $sets] == $numSets} {
	    break
	}
    }
    return [list $hand $sets]
}
```

Demonstrating:

```tcl
# Render a hand (or any list) of cards (the "."s are just placeholders).
proc PrettyHand {hand {separator \n}} {
    set Co {. red green . purple}
    set Sy {. oval squiggle . diamond}
    set Nu {. one two . three}
    set Sh {. solid open . striped}
    foreach card $hand {
	lassign $card co s n sh
	lappend result [format "(%s,%s,%s,%s)" \
		[lindex $Co $co] [lindex $Sy $s] [lindex $Nu $n] [lindex $Sh $sh]]
    }
    return $separator[join $result $separator]
}

# Render the output of the Set Puzzle solver.
proc PrettyOutput {setResult} {
    lassign $setResult hand sets
    set sep "\n   "
    puts "Hand (with [llength $hand] cards) was:[PrettyHand $hand $sep]"
    foreach s $sets {
	puts "Found set [incr n]:[PrettyHand $s $sep]"
    }
}

# Demonstrate on the two cases
puts "
###  BASIC PUZZLE ======
"
PrettyOutput [SetPuzzle 9 4]
puts "
###  ADVANCED PUZZLE ===
"
PrettyOutput [SetPuzzle 12 6]
```

```txt


###  BASIC PUZZLE ======

Hand (with 9 cards) was:
   (purple,squiggle,one,solid)
   (green,diamond,two,striped)
   (green,oval,two,striped)
   (purple,diamond,three,striped)
   (red,oval,three,open)
   (green,squiggle,three,solid)
   (red,squiggle,one,solid)
   (red,oval,one,solid)
   (purple,oval,three,open)
Found set 1:
   (purple,squiggle,one,solid)
   (green,diamond,two,striped)
   (red,oval,three,open)
Found set 2:
   (green,oval,two,striped)
   (purple,oval,three,open)
   (red,oval,one,solid)
Found set 3:
   (red,oval,three,open)
   (green,squiggle,three,solid)
   (purple,diamond,three,striped)
Found set 4:
   (red,squiggle,one,solid)
   (green,diamond,two,striped)
   (purple,oval,three,open)

###  ADVANCED PUZZLE ===

Hand (with 12 cards) was:
   (green,diamond,two,open)
   (red,diamond,one,solid)
   (purple,diamond,one,solid)
   (red,squiggle,two,open)
   (green,diamond,three,open)
   (red,oval,two,striped)
   (red,diamond,two,solid)
   (purple,diamond,two,striped)
   (purple,diamond,three,open)
   (purple,diamond,three,striped)
   (purple,oval,three,open)
   (purple,squiggle,two,striped)
Found set 1:
   (green,diamond,two,open)
   (red,diamond,one,solid)
   (purple,diamond,three,striped)
Found set 2:
   (green,diamond,two,open)
   (purple,diamond,two,striped)
   (red,diamond,two,solid)
Found set 3:
   (purple,diamond,one,solid)
   (purple,diamond,three,open)
   (purple,diamond,two,striped)
Found set 4:
   (purple,diamond,one,solid)
   (purple,oval,three,open)
   (purple,squiggle,two,striped)
Found set 5:
   (green,diamond,three,open)
   (red,diamond,one,solid)
   (purple,diamond,two,striped)
Found set 6:
   (red,diamond,two,solid)
   (red,oval,two,striped)
   (red,squiggle,two,open)

```



## zkl

```zkl
const nDraw=9, nGoal=(nDraw/2);  // Basic
var [const] UH=Utils.Helpers; // baked in stash of goodies
deck:=Walker.cproduct("red green purple".split(), // Cartesian product of 4 lists of lists
		"one two three".split(),    // T(1,2,3) (ie numbers) also works
		"oval squiggle diamond".split(),
		"solid open striped".split()).walk();
reg draw,sets,N=0;
do{ N+=1;
   draw=deck.shuffle()[0,nDraw]; // one draw per shuffle
   sets=UH.pickNFrom(3,draw)  // 84 sets of 3 cards (each with 4 features)
        .filter(fcn(set){    // list of 12 items (== 3 cards)
                    set[0,4].zip(set[4,4],set[8,4]) // -->4 tuples of 3 features
		    .pump(List,UH.listUnique,"len", // 1,3 (good) or 2 (bad)
		               '==(2))		    // (F,F,F,F)==good
		    .sum(0) == 0 		    // all 4 feature sets good
		 });
}while(sets.len()!=nGoal);

println("Dealt %d cards %d times:".fmt(draw.len(),N));
draw.pump(Void,fcn(card){ println(("%8s "*4).fmt(card.xplode())) });
println("\nContaining:");
sets.pump(Void,fcn(card){ println((("%8s "*4 + "\n")*3).fmt(card.xplode())) });
```

```txt

Dealt 9 cards 271 times:
     red      one     oval    solid
   green      one  diamond  striped
     red      two     oval     open
  purple      two squiggle  striped
   green    three  diamond     open
  purple    three squiggle    solid
  purple      one  diamond  striped
   green    three squiggle    solid
   green      one squiggle     open

Containing:
     red      one     oval    solid
  purple      two squiggle  striped
   green    three  diamond     open

     red      one     oval    solid
  purple      one  diamond  striped
   green      one squiggle     open

   green      one  diamond  striped
     red      two     oval     open
  purple    three squiggle    solid

     red      two     oval     open
  purple      one  diamond  striped
   green    three squiggle    solid

```


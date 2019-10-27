+++
title = "Poker hand analyser"
description = ""
date = 2019-08-11T13:10:21Z
aliases = []
[extra]
id = 16850
[taxonomies]
categories = []
tags = []
+++

{{task}} 
[[Category:Cards]] 
[[Category:Games]]

;Task:
Create a program to parse a single five card poker hand and rank it according to this [[wp:List_of_poker_hands|list of poker hands]].


A poker hand is specified as a space separated list of five playing cards. 

Each input card has two characters indicating face and suit.   For example:   '''2d'''   (two of diamonds).

Faces are: '''a''', '''2''', '''3''', '''4''', '''5''', '''6''', '''7''', '''8''', '''9''', '''10''', '''j''', '''q''', '''k'''

Suits are: '''h''' (hearts), '''d''' (diamonds), '''c''' (clubs), and '''s''' (spades), or alternatively the unicode card-suit characters: ♥ ♦ ♣ ♠

Duplicate cards are illegal.

The program should analyze a single hand and produce one of the following outputs:
  straight-flush
  four-of-a-kind
  full-house
  flush
  straight
  three-of-a-kind
  two-pair
  one-pair
  high-card
  invalid

Examples:
    2♥ 2♦ 2♣ k♣ q♦: three-of-a-kind
    2♥ 5♥ 7♦ 8♣ 9♠: high-card
    a♥ 2♦ 3♣ 4♣ 5♦: straight
    2♥ 3♥ 2♦ 3♣ 3♦: full-house
    2♥ 7♥ 2♦ 3♣ 3♦: two-pair
    2♥ 7♥ 7♦ 7♣ 7♠: four-of-a-kind 
    10♥ j♥ q♥ k♥ a♥: straight-flush
    4♥ 4♠ k♠ 5♦ 10♠: one-pair
    q♣ 10♣ 7♣ 6♣ 4♣: flush

The programs output for the above examples should be displayed here on this page.


;Extra credit:
# use the playing card characters introduced with Unicode 6.0 (U+1F0A1 - U+1F0DE).
# allow two jokers
::* use the symbol '''joker'''
::* duplicates would be allowed (for jokers only)
::* five-of-a-kind would then be the highest hand


;More extra credit examples:
    joker  2♦  2♠  k♠  q♦: three-of-a-kind
    joker  5♥  7♦  8♠  9♦: straight
    joker  2♦  3♠  4♠  5♠: straight
    joker  3♥  2♦  3♠  3♦: four-of-a-kind
    joker  7♥  2♦  3♠  3♦: three-of-a-kind
    joker  7♥  7♦  7♠  7♣: five-of-a-kind
    joker  j♥  q♥  k♥  A♥: straight-flush
    joker  4♣  k♣  5♦ 10♠: one-pair
    joker  k♣  7♣  6♣  4♣: flush
    joker  2♦  joker  4♠  5♠: straight
    joker  Q♦  joker  A♠ 10♠: straight
    joker  Q♦  joker  A♦ 10♦: straight-flush
    joker  2♦  2♠  joker  q♦: four-of-a-kind





## AutoHotkey


```AutoHotkey
PokerHand(hand){
	StringUpper, hand, hand
	Sort, hand, FCardSort D%A_Space%
	cardSeq	:= RegExReplace(hand, "[^2-9TJQKA]")
	Straight:= InStr("23456789TJQKA", cardSeq) || (cardSeq = "2345A") ? true : false	
	hand 	:= cardSeq = "2345A" ? RegExReplace(hand, "(.*)\h(A.)", "$2 $1") : hand
	Royal 	:= InStr(hand, "A") ? "Royal": "Straight"
	return  (hand ~= "[2-9TJQKA](.)\h.\1\h.\1\h.\1\h.\1") && (Straight) 			? hand "`t" Royal " Flush"
			: (hand ~= "([2-9TJQKA]).*?\1.*?\1.*?\1") 				? hand "`tFour of a Kind"
			: (hand ~= "^([2-9TJQKA]).\h\1.\h(?!\1)([2-9TJQKA]).\h\2.\h\2.$") 	? hand "`tFull House"	; xxyyy
			: (hand ~= "^([2-9TJQKA]).\h\1.\h\1.\h(?!\1)([2-9TJQKA]).\h\2.$") 	? hand "`tFull House"	; xxxyy
			: (hand ~= "[2-9TJQKA](.)\h.\1\h.\1\h.\1\h.\1") 			? hand "`tFlush" 
			: (Straight)								? hand "`tStraight"
			: (hand ~= "([2-9TJQKA]).*?\1.*?\1")					? hand "`tThree of a Kind"
			: (hand ~= "([2-9TJQKA]).\h\1.*?([2-9TJQKA]).\h\2")			? hand "`tTwo Pair"
			: (hand ~= "([2-9TJQKA]).\h\1")						? hand "`tOne Pair"
			: 									  hand "`tHigh Card"
}
CardSort(a, b){
	a := SubStr(a, 1, 1), b := SubStr(b, 1, 1)
	a := (a = "T") ? 10 : (a = "J") ? 11 : (a = "Q") ? 12 : (a = "K") ? 13 : a
	b := (b = "T") ? 10 : (b = "J") ? 11 : (b = "Q") ? 12 : (b = "K") ? 13 : b
	return a > b ? 1 : a < b ? -1 : 0
}
```

Examples:
```AutoHotkey
hands =
(join`r`n
2♥ 2♦ 2♣ k♣ q♦
2♥ 5♥ 7♦ 8♣ 9♠
a♥ 2♦ 3♣ 4♣ 5♦
2♥ 3♥ 2♦ 3♣ 3♦
2♥ 3♥ 2♦ 2♣ 3♦
2♥ 7♥ 2♦ 3♣ 3♦
2♥ 7♥ 7♦ 7♣ 7♠
T♥ j♥ q♥ a♥ K♥
T♥ j♥ q♥ 9♥ K♥
4♥ 4♠ k♠ 5♦ T♠
q♣ T♣ 7♣ 6♣ 4♣
)
loop, parse, hands, `n, `r
	res .= PokerHand(A_LoopField) "`n"
MsgBox, 262144, , % res
return
```

Outputs:
```txt
2♦ 2♣ 2♥ Q♦ K♣	Three of a Kind
2♥ 5♥ 7♦ 8♣ 9♠	High Card
A♥ 2♦ 3♣ 4♣ 5♦	Straight
2♦ 2♥ 3♣ 3♦ 3♥	Full House
2♣ 2♦ 2♥ 3♦ 3♥	Full House
2♦ 2♥ 3♣ 3♦ 7♥	Two Pair
2♥ 7♦ 7♣ 7♠ 7♥	Four of a Kind
T♥ J♥ Q♥ K♥ A♥	Royal Flush
9♥ T♥ J♥ Q♥ K♥	Straight Flush
4♠ 4♥ 5♦ T♠ K♠	One Pair
4♣ 6♣ 7♣ T♣ Q♣	Flush
```



## C

{{trans|Kotlin}}

```c>#include <stdio.h

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

#define FACES "23456789tjqka"
#define SUITS "shdc"

typedef int bool;

typedef struct {
    int face;  /* FACES map to 0..12 respectively */
    char suit;
} card;

card cards[5];

int compare_card(const void *a, const void *b) {
    card c1 = *(card *)a;
    card c2 = *(card *)b;
    return c1.face - c2.face;
}

bool equals_card(card c1, card c2) {
    if (c1.face == c2.face && c1.suit == c2.suit) return TRUE;
    return FALSE;
}

bool are_distinct() {
    int i, j;
    for (i = 0; i < 4; ++i)
        for (j = i + 1; j < 5; ++j)
            if (equals_card(cards[i], cards[j])) return FALSE;
    return TRUE;
}

bool is_straight() {
    int i;
    qsort(cards, 5, sizeof(card), compare_card);
    if (cards[0].face + 4 == cards[4].face) return TRUE;
    if (cards[4].face == 12 && cards[0].face == 0 &&
        cards[3].face == 3) return TRUE;
    return FALSE;
}

bool is_flush() {
    int i;
    char suit = cards[0].suit;
    for (i = 1; i < 5; ++i) if (cards[i].suit != suit) return FALSE;
    return TRUE;
}

const char *analyze_hand(const char *hand) {
    int i, j, gs = 0;
    char suit, *cp;
    bool found, flush, straight;
    int groups[13];
    if (strlen(hand) != 14) return "invalid";
    for (i = 0; i < 14; i += 3) {
        cp = strchr(FACES, tolower(hand[i]));
        if (cp == NULL) return "invalid";
        j = i / 3;
        cards[j].face = cp - FACES;
        suit = tolower(hand[i + 1]);
        cp = strchr(SUITS, suit);
        if (cp == NULL) return "invalid";
        cards[j].suit = suit;
    }
    if (!are_distinct()) return "invalid";
    for (i = 0; i < 13; ++i) groups[i] = 0;
    for (i = 0; i < 5; ++i) groups[cards[i].face]++;
    for (i = 0; i < 13; ++i) if (groups[i] > 0) gs++;
    switch(gs) {
        case 2:
            found = FALSE;
            for (i = 0; i < 13; ++i) if (groups[i] == 4) {
                found = TRUE;
                break;
            }
            if (found) return "four-of-a-kind";
            return "full-house";
        case 3:
            found = FALSE;
            for (i = 0; i < 13; ++i) if (groups[i] == 3) {
                found = TRUE;
                break;
            }
            if (found) return "three-of-a-kind";
            return "two-pairs";
        case 4:
            return "one-pair";
        default:
            flush = is_flush();
            straight = is_straight();
            if (flush && straight)
                return "straight-flush";
            else if (flush)
                return "flush";
            else if (straight)
                return "straight";
            else
                return "high-card";
    }
}

int main(){
    int i;
    const char *type;
    const char *hands[10] = {
        "2h 2d 2c kc qd",
        "2h 5h 7d 8c 9s",
        "ah 2d 3c 4c 5d",
        "2h 3h 2d 3c 3d",
        "2h 7h 2d 3c 3d",
        "2h 7h 7d 7c 7s",
        "th jh qh kh ah",
        "4h 4s ks 5d ts",
        "qc tc 7c 6c 4c",
        "ah ah 7c 6c 4c"
    };
    for (i = 0; i < 10; ++i) {
        type = analyze_hand(hands[i]);
        printf("%s: %s\n", hands[i], type);
    }
    return 0;
}
```


{{output}}

```txt

2h 2d 2c kc qd: three-of-a-kind
2h 5h 7d 8c 9s: high-card
ah 2d 3c 4c 5d: straight
2h 3h 2d 3c 3d: full-house
2h 7h 2d 3c 3d: two-pairs
2h 7h 7d 7c 7s: four-of-a-kind
th jh qh kh ah: straight-flush
4h 4s ks 5d ts: one-pair
qc tc 7c 6c 4c: flush
ah ah 7c 6c 4c: invalid

```



## C++


```Cpp

#include <iostream>
#include <sstream>
#include <algorithm>
#include <vector>

using namespace std;

class poker
{
public:
    poker() { face = "A23456789TJQK"; suit = "SHCD"; }
    string analyze( string h )
    {
	memset( faceCnt, 0, 13 ); memset( suitCnt, 0, 4 ); vector<string> hand;
	transform( h.begin(), h.end(), h.begin(), toupper ); istringstream i( h );
	copy( istream_iterator<string>( i ), istream_iterator<string>(), back_inserter<vector<string> >( hand ) );
	if( hand.size() != 5 ) return "invalid hand."; vector<string>::iterator it = hand.begin();
	sort( it, hand.end() ); if( hand.end() != adjacent_find( it, hand.end() ) ) return "invalid hand.";
	while( it != hand.end() )
	{
	    if( ( *it ).length() != 2 ) return "invalid hand.";
	    int n = face.find( ( *it ).at( 0 ) ), l = suit.find( ( *it ).at( 1 ) );
	    if( n < 0 || l < 0 ) return "invalid hand.";
	    faceCnt[n]++; suitCnt[l]++; it++;
	}
	cout << h << ": "; return analyzeHand();
    }
private:
    string analyzeHand()
    {
	bool p1 = false, p2 = false, t = false, f = false, fl = false, st = false;
	for( int x = 0; x < 13; x++ )
	    switch( faceCnt[x] )
	    {
		case 2: if( p1 ) p2 = true; else p1 = true; break;
		case 3: t = true; break;
		case 4: f = true;
	    }
	for( int x = 0; x < 4; x++ )if( suitCnt[x] == 5 ){ fl = true; break; }

	if( !p1 && !p2 && !t && !f )
        {
	    int s = 0;
	    for( int x = 0; x < 13; x++ )
	    { 
		if( faceCnt[x] ) s++; else s = 0;
		if( s == 5 ) break;
	    }
	    st = ( s == 5 ) || ( s == 4 && faceCnt[0] && !faceCnt[1] );
	}

	if( st && fl ) return "straight-flush";
	else if( f ) return "four-of-a-kind"; 
	else if( p1 && t ) return "full-house";
	else if( fl ) return "flush";
	else if( st ) return "straight";
	else if( t ) return "three-of-a-kind";
	else if( p1 && p2 ) return "two-pair";
	else if( p1 ) return "one-pair";
        return "high-card";
    }
    string face, suit;
    unsigned char faceCnt[13], suitCnt[4];
};

int main( int argc, char* argv[] )
{
    poker p; 
    cout << p.analyze( "2h 2d 2s ks qd" ) << endl; cout << p.analyze( "2h 5h 7d 8s 9d" ) << endl;
    cout << p.analyze( "ah 2d 3s 4s 5s" ) << endl; cout << p.analyze( "2h 3h 2d 3s 3d" ) << endl;
    cout << p.analyze( "2h 7h 2d 3s 3d" ) << endl; cout << p.analyze( "2h 7h 7d 7s 7c" ) << endl;
    cout << p.analyze( "th jh qh kh ah" ) << endl; cout << p.analyze( "4h 4c kc 5d tc" ) << endl;
    cout << p.analyze( "qc tc 7c 6c 4c" ) << endl << endl; return system( "pause" );
}

```

{{out}}

```txt

2H 2D 2S KS QD: three-of-a-kind
2H 5H 7D 8S 9D: high-card
AH 2D 3S 4S 5S: straight
2H 3H 2D 3S 3D: full-house
2H 7H 2D 3S 3D: two-pair
2H 7H 7D 7S 7C: four-of-a-kind
TH JH QH KH AH: straight-flush
4H 4C KC 5D TC: one-pair
QC TC 7C 6C 4C: flush

```



## Clojure


```clojure
(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn n-of-a-kind [hand n]
  (not (empty? (filter #(= true %) (map #(>= % n) (vals (frequencies (map rank hand))))))))

(defn ranks-with-ace [hand]
  (let [ranks (sort (map rank hand))]
    (if (some #(= 14 %) ranks) (cons 1 ranks) ranks)))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn flush? [hand]
  (not (empty? (filter #(= true %) (map #(>= % 5) (vals (frequencies (map suit hand))))))))

(defn full-house? [hand]
  (true? (and
    (some #(= 2 %) (vals (frequencies (map rank hand))))
    (some #(= 3 %) (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (or
    (full-house? hand)
    (four-of-a-kind? hand)
    (= 2 (count (filter #(= true %) (map #(>= % 2) (vals (frequencies (map rank hand)))))))))

(defn straight? [hand]
  (let [hand-a (ranks-with-ace hand)
        fst (first hand-a)
        snd (second hand-a)]
    (or
      (= (take 5 hand-a) (range fst (+ fst 5)))
      (= (drop 1 hand-a) (range snd (+ snd 5))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn invalid? [hand]
  (not= 5 (count (set hand))))

(defn check-hand [hand]
  (cond
    (invalid? hand) "invalid"
    (straight-flush? hand) "straight-flush"
    (four-of-a-kind? hand) "four-of-a-kind"
    (full-house? hand) "full-house"
    (flush? hand) "flush"
    (straight? hand) "straight"
    (three-of-a-kind? hand) "three-of-a-kind"
    (two-pairs? hand) "two-pair"
    (pair? hand) "one-pair"
    :else "high-card"))

; Test examples
(def hands [["2H" "2D" "2S" "KS" "QD"]
            ["2H" "5H" "7D" "8S" "9D"]
            ["AH" "2D" "3S" "4S" "5S"]
            ["2H" "3H" "2D" "3S" "3D"]
            ["2H" "7H" "2D" "3S" "3D"]
            ["2H" "7H" "7D" "7S" "7C"]
            ["TH" "JH" "QH" "KH" "AH"]
            ["4H" "4C" "KC" "5D" "TC"]
            ["QC" "TC" "7C" "6C" "4C"]])
(run! println (map #(str % " : " (check-hand %)) hands))

```


{{out}}

```txt

["2H" "2D" "2S" "KS" "QD"] : three-of-a-kind
["2H" "5H" "7D" "8S" "9D"] : high-card
["AH" "2D" "3S" "4S" "5S"] : straight
["2H" "3H" "2D" "3S" "3D"] : full-house
["2H" "7H" "2D" "3S" "3D"] : two-pair
["2H" "7H" "7D" "7S" "7C"] : four-of-a-kind
["TH" "JH" "QH" "KH" "AH"] : straight-flush
["4H" "4C" "KC" "5D" "TC"] : one-pair
["QC" "TC" "7C" "6C" "4C"] : flush

```



## D


### Basic Version

No bonus for this simple version.
{{trans|C++}}

```d
import std.stdio, std.string, std.algorithm, std.range;

string analyzeHand(in string inHand) pure /*nothrow @safe*/ {
    enum handLen = 5;
    static immutable face = "A23456789TJQK", suit = "SHCD";
    static immutable errorMessage = "invalid hand.";

    /*immutable*/ const hand = inHand.toUpper.split.sort().release;
    if (hand.length != handLen)
        return errorMessage;
    if (hand.uniq.walkLength != handLen)
        return errorMessage ~ " Duplicated cards.";

    ubyte[face.length] faceCount;
    ubyte[suit.length] suitCount;
    foreach (immutable card; hand) {
        if (card.length != 2)
            return errorMessage;
        immutable n = face.countUntil(card[0]);
        immutable l = suit.countUntil(card[1]);
        if (n < 0 || l < 0)
            return errorMessage;
        faceCount[n]++;
        suitCount[l]++;
    }

    return analyzeHandHelper(faceCount, suitCount);
}

private string analyzeHandHelper(const ref ubyte[13] faceCount,
                                 const ref ubyte[4] suitCount)
pure nothrow @safe @nogc {
    bool p1, p2, t, f, fl, st;

    foreach (immutable fc; faceCount)
        switch (fc) {
            case 2: (p1 ? p2 : p1) = true; break;
            case 3: t = true; break;
            case 4: f = true; break;
            default: // Ignore.
        }

    foreach (immutable sc; suitCount)
        if (sc == 5) {
            fl = true;
            break;
        }

    if (!p1 && !p2 && !t && !f) {
        uint s = 0;
        foreach (immutable fc; faceCount) {
            if (fc)
                s++;
            else
                s = 0;
            if (s == 5)
                break;
        }

        st = (s == 5) || (s == 4 && faceCount[0] && !faceCount[1]);
    }

    if (st && fl)      return "straight-flush";
    else if (f)        return "four-of-a-kind";
    else if (p1 && t)  return "full-house";
    else if (fl)       return "flush";
    else if (st)       return "straight";
    else if (t)        return "three-of-a-kind";
    else if (p1 && p2) return "two-pair";
    else if (p1)       return "one-pair";
    else               return "high-card";
}

void main() {
    // S = Spades, H = Hearts, C = Clubs, D = Diamonds.
    foreach (immutable hand; ["2H 2D 2S KS QD",
                              "2H 5H 7D 8S 9D",
                              "AH 2D 3S 4S 5S",
                              "2H 3H 2D 3S 3D",
                              "2H 7H 2D 3S 3D",
                              "2H 7H 7D 7S 7C",
                              "TH JH QH KH AH",
                              "4H 4C KC 5D TC",
                              "QC TC 7C 6C 4C"])
        writeln(hand, ": ", hand.analyzeHand);
}
```

{{out}}

```txt
2H 2D 2S KS QD: three-of-a-kind
2H 5H 7D 8S 9D: high-card
AH 2D 3S 4S 5S: straight
2H 3H 2D 3S 3D: full-house
2H 7H 2D 3S 3D: two-pair
2H 7H 7D 7S 7C: four-of-a-kind
TH JH QH KH AH: straight-flush
4H 4C KC 5D TC: one-pair
QC TC 7C 6C 4C: flush
```



## Elixir

{{trans|Ruby}}
{{works with|Elixir|1.2}}

```elixir
defmodule Card do
  @faces   ~w(2 3 4 5 6 7 8 9 10 j q k a)
  @suits   ~w(♥ ♦ ♣ ♠)                          # ~w(h d c s)
  @ordinal @faces |> Enum.with_index |> Map.new
  
  defstruct ~w[face suit ordinal]a
  
  def new(str) do
    {face, suit} = String.split_at(str, -1)
    if face in @faces and suit in @suits do
      ordinal = @ordinal[face]
      %__MODULE__{face: face, suit: suit, ordinal: ordinal}
    else
      raise ArgumentError, "invalid card: #{str}"
    end
  end
  
  def deck do
    for face <- @faces, suit <- @suits, do: "#{face}#{suit}"
  end
end

defmodule Hand do
  @ranks ~w(high-card one-pair two-pair three-of-a-kind straight flush
            full-house four-of-a-kind straight-flush five-of-a-kind)a |>
         Enum.with_index |> Map.new
  @wheel_faces ~w(2 3 4 5 a)
  
  def new(str_of_cards) do
    cards = String.downcase(str_of_cards) |>
            String.split([" ", ","], trim: true) |>
            Enum.map(&Card.new &1)
    grouped = Enum.group_by(cards, &(&1.ordinal)) |> Map.values
    face_pattern = Enum.map(grouped, &(length &1)) |> Enum.sort
    {consecutive, wheel_faces} = consecutive?(cards)
    rank = categorize(cards, face_pattern, consecutive)
    rank_num = @ranks[rank]
    tiebreaker = if wheel_faces do
                   for ord <- 3..-1, do: {1,ord}
                 else
                   Enum.map(grouped, &{length(&1), hd(&1).ordinal}) |>
                   Enum.sort |> Enum.reverse
                 end
    {rank_num, tiebreaker, str_of_cards, rank}
  end
  
  defp one_suit?(cards) do
    Enum.map(cards, &(&1.suit)) |> Enum.uniq |> length == 1
  end
 
  defp consecutive?(cards) do
    sorted = Enum.sort_by(cards, &(&1.ordinal))
    if Enum.map(sorted, &(&1.face)) == @wheel_faces do
      {true, true}
    else
      flag = Enum.map(sorted, &(&1.ordinal)) |>
             Enum.chunk(2,1) |>
             Enum.all?(fn [a,b] -> a+1 == b end)
      {flag, false}
    end
  end
  
  defp categorize(cards, face_pattern, consecutive) do
    case {consecutive, one_suit?(cards)} do
      {true, true}  -> :"straight-flush"
      {true, false} -> :straight
      {false, true} -> :flush
      _ ->  case face_pattern do
              [1,1,1,1,1] -> :"high-card"
              [1,1,1,2]   -> :"one-pair"
              [1,2,2]     -> :"two-pair"
              [1,1,3]     -> :"three-of-a-kind"
              [2,3]       -> :"full-house"
              [1,4]       -> :"four-of-a-kind"
              [5]         -> :"five-of-a-kind"
            end
    end
  end
end

test_hands = """
2♥ 2♦ 2♣ k♣ q♦
2♥ 5♥ 7♦ 8♣ 9♠
a♥ 2♦ 3♣ 4♣ 5♦
2♥ 3♥ 2♦ 3♣ 3♦
2♥ 7♥ 2♦ 3♣ 3♦
2♥ 6♥ 2♦ 3♣ 3♦
10♥ j♥ q♥ k♥ a♥
4♥ 4♠ k♠ 2♦ 10♠
4♥ 4♠ k♠ 3♦ 10♠
q♣ 10♣ 7♣ 6♣ 4♣
q♣ 10♣ 7♣ 6♣ 3♣
9♥ 10♥ q♥ k♥ j♣
2♥ 3♥ 4♥ 5♥ a♥
2♥ 2♥ 2♦ 3♣ 3♦
"""
hands = String.split(test_hands, "\n", trim: true) |> Enum.map(&Hand.new(&1))
IO.puts "High to low"
Enum.sort(hands) |> Enum.reverse |>
Enum.each(fn hand -> IO.puts "#{elem(hand,2)}: \t#{elem(hand,3)}" end)

# Extra Credit 2. Examples:
IO.puts "\nExtra Credit 2"
extra_hands = """
joker  2♦  2♠  k♠  q♦
joker  5♥  7♦  8♠  9♦
joker  2♦  3♠  4♠  5♠
joker  3♥  2♦  3♠  3♦
joker  7♥  2♦  3♠  3♦
joker  7♥  7♦  7♠  7♣
joker  j♥  q♥  k♥  A♥
joker  4♣  k♣  5♦ 10♠
joker  k♣  7♣  6♣  4♣
joker  2♦  joker  4♠  5♠
joker  Q♦  joker  A♠ 10♠
joker  Q♦  joker  A♦ 10♦
joker  2♦  2♠  joker  q♦
"""
deck = Card.deck
String.split(extra_hands, "\n", trim: true) |>
Enum.each(fn hand ->
  [a,b,c,d,e] = String.split(hand) |>
                Enum.map(fn c -> if c=="joker", do: deck, else: [c] end)
  cards_list = for v<-a, w<-b, x<-c, y<-d, z<-e, do: "#{v} #{w} #{x} #{y} #{z}"
  best = Enum.map(cards_list, &Hand.new &1) |> Enum.max
  IO.puts "#{hand}:\t#{elem(best,3)}"
end)
```


{{out}}

```txt

High to low
10♥ j♥ q♥ k♥ a♥: 	straight-flush
2♥ 3♥ 4♥ 5♥ a♥: 	straight-flush
2♥ 3♥ 2♦ 3♣ 3♦: 	full-house
2♥ 2♥ 2♦ 3♣ 3♦: 	full-house
q♣ 10♣ 7♣ 6♣ 4♣: 	flush
q♣ 10♣ 7♣ 6♣ 3♣: 	flush
9♥ 10♥ q♥ k♥ j♣: 	straight
a♥ 2♦ 3♣ 4♣ 5♦: 	straight
2♥ 2♦ 2♣ k♣ q♦: 	three-of-a-kind
2♥ 7♥ 2♦ 3♣ 3♦: 	two-pair
2♥ 6♥ 2♦ 3♣ 3♦: 	two-pair
4♥ 4♠ k♠ 3♦ 10♠: 	one-pair
4♥ 4♠ k♠ 2♦ 10♠: 	one-pair
2♥ 5♥ 7♦ 8♣ 9♠: 	high-card

Extra Credit 2
joker  2♦  2♠  k♠  q♦:	three-of-a-kind
joker  5♥  7♦  8♠  9♦:	straight
joker  2♦  3♠  4♠  5♠:	straight
joker  3♥  2♦  3♠  3♦:	four-of-a-kind
joker  7♥  2♦  3♠  3♦:	three-of-a-kind
joker  7♥  7♦  7♠  7♣:	five-of-a-kind
joker  j♥  q♥  k♥  A♥:	straight-flush
joker  4♣  k♣  5♦ 10♠:	one-pair
joker  k♣  7♣  6♣  4♣:	flush
joker  2♦  joker  4♠  5♠:	straight
joker  Q♦  joker  A♠ 10♠:	straight
joker  Q♦  joker  A♦ 10♦:	straight-flush
joker  2♦  2♠  joker  q♦:	four-of-a-kind

```


=={{header|F Sharp|F#}}==

```fsharp

type Card = int * int

type Cards = Card list

let joker = (69,69)

let rankInvalid = "invalid", 99

let allCards = {0..12} |> Seq.collect (fun x->({0..3} |> Seq.map (fun y->x,y)))

let allSame = function | y::ys -> List.forall ((=) y) ys | _-> false

let straightList (xs:int list) = xs |> List.sort |> List.mapi (fun i n->n - i) |> allSame

let cardList (s:string): Cards =
  s.Split() |> Seq.map (fun s->s.ToLower())
  |> Seq.map (fun s ->
    if s="joker" then joker
    else
      match (s |> List.ofSeq) with
      | '1'::'0'::xs -> (9, xs) | '!'::xs -> (-1, xs) | x::xs-> ("a23456789!jqk".IndexOf(x), xs) | _  as xs-> (-1, xs)
      |> function | -1, _  -> (-1, '!') | x, y::[] -> (x, y) | _  -> (-1, '!')
      |> function 
      | x, 'h' | x, '♥' -> (x, 0) | x, 'd' | x, '♦' -> (x, 1) | x, 'c' | x, '♣' -> (x, 2)
      | x, 's' | x, '♠' -> (x, 3) | _ -> (-1, -1)
    )
  |> Seq.filter (fst >> ((<>) -1)) |> List.ofSeq


let rank (cards: Cards) =
  if cards.Length<>5 then rankInvalid
  else 
    let cts = cards |> Seq.groupBy fst |> Seq.map (snd >> Seq.length) |> List.ofSeq |> List.sort |> List.rev
    if cts.[0]=5 then ("five-of-a-kind", 1)
    else
      let flush = cards |> List.map snd |> allSame
      let straight = 
        let (ACE, ALT_ACE) = 0, 13
        let faces = cards |> List.map fst |> List.sort
        (straightList faces) || (if faces.Head<>ACE then false else (straightList (ALT_ACE::(faces.Tail))))
      if straight && flush then ("straight-flush", 2)
      else
        let cts = cards |> Seq.groupBy fst |> Seq.map (snd >> Seq.length) |> List.ofSeq |> List.sort |> List.rev
        if cts.[0]=4 then ("four-of-a-kind", 3)
        elif cts.[0]=3 && cts.[1]=2 then ("full-house", 4)
        elif flush then ("flush", 5)
        elif straight then ("straight", 6)
        elif cts.[0]=3 then ("three-of-a-kind", 7)
        elif cts.[0]=2 && cts.[1]=2 then ("two-pair", 8)
        elif cts.[0]=2 then ("one-pair", 9)
        else ("high-card", 10)

let pickBest (xs: seq<Cards>) =
  let cmp a b = (<) (snd a) (snd b)
  let pick currentBest x = if (cmp (snd x) (snd currentBest)) then x else currentBest
  xs |> Seq.map (fun x->x, (rank x)) |> Seq.fold pick ([], rankInvalid)

let calcHandRank handStr =
  let cards = handStr |> cardList
  if cards.Length<>5 
    then (cards, rankInvalid) 
    else
      cards |> List.partition ((=) joker) |> fun (x,y) -> x.Length, y
      |> function
      | (0,xs) when (xs |> Seq.distinct |> Seq.length)=5 -> xs, (rank xs)
      | (1,xs) -> allCards |> Seq.map (fun x->x::xs) |> pickBest
      | (2,xs) -> allCards |> Seq.collect (fun x->allCards |> Seq.map (fun y->y::x::xs)) |> pickBest
      | _ -> cards, rankInvalid


let showHandRank handStr =
  // handStr |> calcHandRank |> fun (cards, (rankName,_)) -> printfn "%s: %A %s" handStr cards rankName
  handStr |> calcHandRank |> (snd >> fst) |> printfn "%s: %s" handStr

[
"2♥ 2♦ 2♣ k♣ q♦"
"2♥ 5♥ 7♦ 8♣ 9♠"
"a♥ 2♦ 3♣ 4♣ 5♦"
"2♥ 3♥ 2♦ 3♣ 3♦"
"2♥ 7♥ 2♦ 3♣ 3♦"
"2♥ 7♥ 7♦ 7♣ 7♠"
"10♥ j♥ q♥ k♥ a♥"
"4♥ 4♠ k♠ 5♦ 10♠"
"q♣ 10♣ 7♣ 6♣ 4♣"
"joker  2♦  2♠  k♠  q♦"
"joker  5♥  7♦  8♠  9♦"
"joker  2♦  3♠  4♠  5♠"
"joker  3♥  2♦  3♠  3♦"
"joker  7♥  2♦  3♠  3♦"
"joker  7♥  7♦  7♠  7♣"
"joker  j♥  q♥  k♥  A♥"
"joker  4♣  k♣  5♦ 10♠"
"joker  k♣  7♣  6♣  4♣"
"joker  2♦  joker  4♠  5♠"
"joker  Q♦  joker  A♠ 10♠"
"joker  Q♦  joker  A♦ 10♦"
"joker  2♦  2♠  joker  q♦"
] 
|> List.iter showHandRank

```


{{out}}

```txt

2♥ 2♦ 2♣ k♣ q♦: three-of-a-kind
2♥ 5♥ 7♦ 8♣ 9♠: high-card
a♥ 2♦ 3♣ 4♣ 5♦: straight
2♥ 3♥ 2♦ 3♣ 3♦: full-house
2♥ 7♥ 2♦ 3♣ 3♦: two-pair
2♥ 7♥ 7♦ 7♣ 7♠: four-of-a-kind
10♥ j♥ q♥ k♥ a♥: straight-flush
4♥ 4♠ k♠ 5♦ 10♠: one-pair
q♣ 10♣ 7♣ 6♣ 4♣: flush
joker  2♦  2♠  k♠  q♦: three-of-a-kind
joker  5♥  7♦  8♠  9♦: straight
joker  2♦  3♠  4♠  5♠: straight
joker  3♥  2♦  3♠  3♦: four-of-a-kind
joker  7♥  2♦  3♠  3♦: three-of-a-kind
joker  7♥  7♦  7♠  7♣: five-of-a-kind
joker  j♥  q♥  k♥  A♥: straight-flush
joker  4♣  k♣  5♦ 10♠: one-pair
joker  k♣  7♣  6♣  4♣: flush
joker  2♦  joker  4♠  5♠: straight
joker  Q♦  joker  A♠ 10♠: straight
joker  Q♦  joker  A♦ 10♦: straight-flush
joker  2♦  2♠  joker  q♦: four-of-a-kind

```



## Factor

Factor comes with a poker hand evaluator.

```factor
USING: formatting kernel poker sequences ;
{
    "2H 2D 2C KC QD"
    "2H 5H 7D 8C 9S"
    "AH 2D 3C 4C 5D"
    "2H 3H 2D 3C 3D"
    "2H 7H 2D 3C 3D"
    "2H 7H 7D 7C 7S"
    "TH JH QH KH AH"
    "4H 4S KS 5D TS"
    "QC TC 7C 6C 4C"
} [ dup string>hand-name "%s: %s\n" printf ] each
```

{{out}}

```txt

2H 2D 2C KC QD: Three of a Kind
2H 5H 7D 8C 9S: High Card
AH 2D 3C 4C 5D: Straight
2H 3H 2D 3C 3D: Full House
2H 7H 2D 3C 3D: Two Pair
2H 7H 7D 7C 7S: Four of a Kind
TH JH QH KH AH: Straight Flush
4H 4S KS 5D TS: One Pair
QC TC 7C 6C 4C: Flush

```



## Go

{{trans|Kotlin}}

### Basic Version


```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

type card struct {
    face byte
    suit byte
}

const faces = "23456789tjqka"
const suits = "shdc"

func isStraight(cards []card) bool {
    sorted := make([]card, 5)
    copy(sorted, cards)
    sort.Slice(sorted, func(i, j int) bool {
        return sorted[i].face < sorted[j].face
    })
    if sorted[0].face+4 == sorted[4].face {
        return true
    }
    if sorted[4].face == 14 && sorted[0].face == 2 && sorted[3].face == 5 {
        return true
    }
    return false
}

func isFlush(cards []card) bool {
    suit := cards[0].suit
    for i := 1; i < 5; i++ {
        if cards[i].suit != suit {
            return false
        }
    }
    return true
}

func analyzeHand(hand string) string {
    temp := strings.Fields(strings.ToLower(hand))
    splitSet := make(map[string]bool)
    var split []string
    for _, s := range temp {
        if !splitSet[s] {
            splitSet[s] = true
            split = append(split, s)
        }
    }
    if len(split) != 5 {
        return "invalid"
    }
    var cards []card

    for _, s := range split {
        if len(s) != 2 {
            return "invalid"
        }
        fIndex := strings.IndexByte(faces, s[0])
        if fIndex == -1 {
            return "invalid"
        }
        sIndex := strings.IndexByte(suits, s[1])
        if sIndex == -1 {
            return "invalid"
        }
        cards = append(cards, card{byte(fIndex + 2), s[1]})
    }

    groups := make(map[byte][]card)
    for _, c := range cards {
        groups[c.face] = append(groups[c.face], c)
    }

    switch len(groups) {
    case 2:
        for _, group := range groups {
            if len(group) == 4 {
                return "four-of-a-kind"
            }
        }
        return "full-house"
    case 3:
        for _, group := range groups {
            if len(group) == 3 {
                return "three-of-a-kind"
            }
        }
        return "two-pair"
    case 4:
        return "one-pair"
    default:
        flush := isFlush(cards)
        straight := isStraight(cards)
        switch {
        case flush && straight:
            return "straight-flush"
        case flush:
            return "flush"
        case straight:
            return "straight"
        default:
            return "high-card"
        }
    }
}

func main() {
    hands := [...]string{
        "2h 2d 2c kc qd",
        "2h 5h 7d 8c 9s",
        "ah 2d 3c 4c 5d",
        "2h 3h 2d 3c 3d",
        "2h 7h 2d 3c 3d",
        "2h 7h 7d 7c 7s",
        "th jh qh kh ah",
        "4h 4s ks 5d ts",
        "qc tc 7c 6c 4c",
        "ah ah 7c 6c 4c",
    }
    for _, hand := range hands {
        fmt.Printf("%s: %s\n", hand, analyzeHand(hand))
    }
}
```


{{out}}

```txt

2h 2d 2c kc qd: three-of-a-kind
2h 5h 7d 8c 9s: high-card
ah 2d 3c 4c 5d: straight
2h 3h 2d 3c 3d: full-house
2h 7h 2d 3c 3d: two-pair
2h 7h 7d 7c 7s: four-of-a-kind
th jh qh kh ah: straight-flush
4h 4s ks 5d ts: one-pair
qc tc 7c 6c 4c: flush
ah ah 7c 6c 4c: invalid

```



### Extra Credit Version


```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

type card struct {
    face byte
    suit byte
}

func isStraight(cards []card, jokers int) bool {
    sorted := make([]card, 5)
    copy(sorted, cards)
    sort.Slice(sorted, func(i, j int) bool {
        return sorted[i].face < sorted[j].face
    })
    switch jokers {
    case 0:
        switch {
        case sorted[0].face+4 == sorted[4].face,
            sorted[4].face == 14 && sorted[3].face == 5:
            return true
        default:
            return false
        }
    case 1:
        switch {
        case sorted[0].face+3 == sorted[3].face,
            sorted[0].face+4 == sorted[3].face,
            sorted[3].face == 14 && sorted[2].face == 4,
            sorted[3].face == 14 && sorted[2].face == 5:
            return true
        default:
            return false
        }
    default:
        switch {
        case sorted[0].face+2 == sorted[2].face,
            sorted[0].face+3 == sorted[2].face,
            sorted[0].face+4 == sorted[2].face,
            sorted[2].face == 14 && sorted[1].face == 3,
            sorted[2].face == 14 && sorted[1].face == 4,
            sorted[2].face == 14 && sorted[1].face == 5:
            return true
        default:
            return false
        }
    }
}

func isFlush(cards []card) bool {
    sorted := make([]card, 5)
    copy(sorted, cards)
    sort.Slice(sorted, func(i, j int) bool {
        return sorted[i].face < sorted[j].face
    })
    suit := sorted[0].suit
    for i := 1; i < 5; i++ {
        if sorted[i].suit != suit && sorted[i].suit != 'j' {
            return false
        }
    }
    return true
}

func analyzeHand(hand string) string {
    temp := strings.Fields(strings.ToLower(hand))
    splitSet := make(map[string]bool)
    var split []string
    for _, s := range temp {
        if !splitSet[s] {
            splitSet[s] = true
            split = append(split, s)
        }
    }
    if len(split) != 5 {
        return "invalid"
    }
    var cards []card
    var jokers = 0

    for _, s := range split {
        if len(s) != 4 {
            return "invalid"
        }
        cp := []rune(s)[0]
        var cd card
        switch {
        case cp == 0x1f0a1:
            cd = card{14, 's'}
        case cp == 0x1f0b1:
            cd = card{14, 'h'}
        case cp == 0x1f0c1:
            cd = card{14, 'd'}
        case cp == 0x1f0d1:
            cd = card{14, 'c'}
        case cp == 0x1f0cf:
            jokers++
            cd = card{15, 'j'} // black joker
        case cp == 0x1f0df:
            jokers++
            cd = card{16, 'j'} // white joker
        case cp >= 0x1f0a2 && cp <= 0x1f0ab:
            cd = card{byte(cp - 0x1f0a0), 's'}
        case cp >= 0x1f0ad && cp <= 0x1f0ae:
            cd = card{byte(cp - 0x1f0a1), 's'}
        case cp >= 0x1f0b2 && cp <= 0x1f0bb:
            cd = card{byte(cp - 0x1f0b0), 'h'}
        case cp >= 0x1f0bd && cp <= 0x1f0be:
            cd = card{byte(cp - 0x1f0b1), 'h'}
        case cp >= 0x1f0c2 && cp <= 0x1f0cb:
            cd = card{byte(cp - 0x1f0c0), 'd'}
        case cp >= 0x1f0cd && cp <= 0x1f0ce:
            cd = card{byte(cp - 0x1f0c1), 'd'}
        case cp >= 0x1f0d2 && cp <= 0x1f0db:
            cd = card{byte(cp - 0x1f0d0), 'c'}
        case cp >= 0x1f0dd && cp <= 0x1f0de:
            cd = card{byte(cp - 0x1f0d1), 'c'}
        default:
            cd = card{0, 'j'} // invalid
        }
        if cd.face == 0 {
            return "invalid"
        }
        cards = append(cards, cd)
    }

    groups := make(map[byte][]card)
    for _, c := range cards {
        groups[c.face] = append(groups[c.face], c)
    }

    switch len(groups) {
    case 2:
        for _, group := range groups {
            if len(group) == 4 {
                switch jokers {
                case 0:
                    return "four-of-a-kind"
                default:
                    return "five-of-a-kind"
                }
            }
        }
        return "full-house"
    case 3:
        for _, group := range groups {
            if len(group) == 3 {
                switch jokers {
                case 0:
                    return "three-of-a-kind"
                case 1:
                    return "four-of-a-kind"
                default:
                    return "five-of-a-kind"
                }
            }
        }
        if jokers == 0 {
            return "two-pair"
        }
        return "full-house"
    case 4:
        switch jokers {
        case 0:
            return "one-pair"
        case 1:
            return "three-of-a-kind"
        default:
            return "four-of-a-kind"
        }
    default:
        flush := isFlush(cards)
        straight := isStraight(cards, jokers)
        switch {
        case flush && straight:
            return "straight-flush"
        case flush:
            return "flush"
        case straight:
            return "straight"
        default:
            if jokers == 0 {
                return "high-card"
            } else {
                return "one-pair"
            }
        }
    }
}

func main() {
    hands := [...]string{
        "🃏 🃂 🂢 🂮 🃍",
        "🃏 🂵 🃇 🂨 🃉",
        "🃏 🃂 🂣 🂤 🂥",
        "🃏 🂳 🃂 🂣 🃃",
        "🃏 🂷 🃂 🂣 🃃",
        "🃏 🂷 🃇 🂧 🃗",
        "🃏 🂻 🂽 🂾 🂱",
        "🃏 🃔 🃞 🃅 🂪",
        "🃏 🃞 🃗 🃖 🃔",
        "🃏 🃂 🃟 🂤 🂥",
        "🃏 🃍 🃟 🂡 🂪",
        "🃏 🃍 🃟 🃁 🃊",
        "🃏 🃂 🂢 🃟 🃍",
        "🃏 🃂 🂢 🃍 🃍",
        "🃂 🃞 🃍 🃁 🃊",
    }
    for _, hand := range hands {
        fmt.Printf("%s: %s\n", hand, analyzeHand(hand))
    }
}
```


{{out}}

```txt

🃏 🃂 🂢 🂮 🃍 : three-of-a-kind
🃏 🂵 🃇 🂨 🃉 : straight
🃏 🃂 🂣 🂤 🂥 : straight
🃏 🂳 🃂 🂣 🃃 : four-of-a-kind
🃏 🂷 🃂 🂣 🃃 : three-of-a-kind
🃏 🂷 🃇 🂧 🃗 : five-of-a-kind
🃏 🂻 🂽 🂾 🂱 : straight-flush
🃏 🃔 🃞 🃅 🂪 : one-pair
🃏 🃞 🃗 🃖 🃔 : flush
🃏 🃂 🃟 🂤 🂥 : straight
🃏 🃍 🃟 🂡 🂪 : straight
🃏 🃍 🃟 🃁 🃊 : straight-flush
🃏 🃂 🂢 🃟 🃍 : four-of-a-kind
🃏 🃂 🂢 🃍 🃍 : invalid
🃂 🃞 🃍 🃁 🃊 : high-card

```



## J



```J
parseHand=: <;._2@,&' '@u:~&7 NB. hand must be well formed
Suits=: <"> 7 u: '♥♦♣♦'       NB. or Suits=: 'hdcs'
Faces=: <;._1 ' 2 3 4 5 6 7 8 9 10 j q k a'

suits=: {:&.>
faces=: }:&.>
flush=: 1 =&#&~. suits
straight=: 1 = (i.#Faces) +/@E.~ Faces /:~@i. faces
kinds=: #/.~ @:faces
five=: 5 e. kinds NB. jokers or other cheat
four=: 4 e. kinds
three=: 3 e. kinds
two=: 2 e. kinds
twoPair=: 2 = 2 +/ .= kinds
highcard=: 5 = 1 +/ .= kinds

IF=: 2 :'(,&(<m) ^: v)"1'
Or=: 2 :'u ^:(5 e. $) @: v'

Deck=: ,Faces,&.>/Suits
Joker=: <'joker'
joke=: [: ,/^:(#@$ - 2:) (({. ,"1 Deck ,"0 1 }.@}.)^:(5>[)~ i.&Joker)"1^:2@,:
punchLine=: {:@-.&a:@,@|:
rateHand=: [:;:inv [: (, [: punchLine -1 :(0 :0-.LF)@joke) parseHand 
 ('invalid' IF 1:) Or
 ('high-card' IF highcard) Or
 ('one-pair' IF two) Or
 ('two-pair' IF twoPair) Or
 ('three-of-a-kind' IF three) Or
 ('straight' IF straight) Or
 ('flush' IF flush) Or
 ('full-house' IF (two * three)) Or
 ('four-of-a-kind' IF four) Or
 ('straight-flush' IF (straight * flush)) Or
 ('five-of-a-kind' IF five)
)
```


Note that * acts as "logical and" on logical values (if you need to deal with boolean values in the original sense - which were not constrained to logical values - you should use *. instead of * to achieve boolean multiplication, but that's not needed here).

Output for required examples:

  2♥ 2♦ 2♣ k♣ q♦ three-of-a-kind
  2♥ 5♥ 7♦ 8♣ 9♠ high-card
  a♥ 2♦ 3♣ 4♣ 5♦ high-card
  2♥ 3♥ 2♦ 3♣ 3♦ full-house
  2♥ 7♥ 2♦ 3♣ 3♦ two-pair
  2♥ 7♥ 7♦ 7♣ 7♠ four-of-a-kind
  10♥ j♥ q♥ k♥ a♥ straight-flush
  4♥ 4♠ k♠ 5♦ 10♠ one-pair
  q♣ 10♣ 7♣ 6♣ 4♣ flush

Output for extra-credit examples

  joker 2♦ 2♠ k♠ q♦ three-of-a-kind
  joker 5♥ 7♦ 8♠ 9♦ straight
  joker 2♦ 3♠ 4♠ 5♠ straight
  joker 3♥ 2♦ 3♠ 3♦ four-of-a-kind
  joker 7♥ 2♦ 3♠ 3♦ three-of-a-kind
  joker 7♥ 7♦ 7♠ 7♣ five-of-a-kind
  joker j♥ q♥ k♥ a♥ straight-flush
  joker 4♣ k♣ 5♦ 10♠ one-pair
  joker k♣ 7♣ 6♣ 4♣ flush
  joker 2♦ joker 4♠ 5♠ straight
  joker q♦ joker a♠ 10♠ straight
  joker q♦ joker a♦ 10♦ straight-flush
  joker 2♦ 2♠ joker q♦ four-of-a-kind


## Java

{{works with|Java|7}}
This code does not qualify for extra credit. Although it supports wildcards, it does not allow for duplicates.

```java
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

public class PokerHandAnalyzer {

    final static String faces = "AKQJT98765432";
    final static String suits = "HDSC";
    final static String[] deck = buildDeck();

    public static void main(String[] args) {
        System.out.println("Regular hands:\n");
        for (String input : new String[]{"2H 2D 2S KS QD",
            "2H 5H 7D 8S 9D",
            "AH 2D 3S 4S 5S",
            "2H 3H 2D 3S 3D",
            "2H 7H 2D 3S 3D",
            "2H 7H 7D 7S 7C",
            "TH JH QH KH AH",
            "4H 4C KC 5D TC",
            "QC TC 7C 6C 4C",
            "QC TC 7C 7C TD"}) {
            System.out.println(analyzeHand(input.split(" ")));
        }

        System.out.println("\nHands with wildcards:\n");
        for (String input : new String[]{"2H 2D 2S KS WW",
            "2H 5H 7D 8S WW",
            "AH 2D 3S 4S WW",
            "2H 3H 2D 3S WW",
            "2H 7H 2D 3S WW",
            "2H 7H 7D WW WW",
            "TH JH QH WW WW",
            "4H 4C KC WW WW",
            "QC TC 7C WW WW",
            "QC TC 7H WW WW"}) {
            System.out.println(analyzeHandWithWildcards(input.split(" ")));
        }
    }

    private static Score analyzeHand(final String[] hand) {
        if (hand.length != 5)
            return new Score("invalid hand: wrong number of cards", -1, hand);

        if (new HashSet<>(Arrays.asList(hand)).size() != hand.length)
            return new Score("invalid hand: duplicates", -1, hand);

        int[] faceCount = new int[faces.length()];
        long straight = 0, flush = 0;
        for (String card : hand) {

            int face = faces.indexOf(card.charAt(0));
            if (face == -1)
                return new Score("invalid hand: non existing face", -1, hand);
            straight |= (1 << face);

            faceCount[face]++;

            if (suits.indexOf(card.charAt(1)) == -1)
                return new Score("invalid hand: non-existing suit", -1, hand);
            flush |= (1 << card.charAt(1));
        }

        // shift the bit pattern to the right as far as possible
        while (straight % 2 == 0)
            straight >>= 1;

        // straight is 00011111; A-2-3-4-5 is 1111000000001
        boolean hasStraight = straight == 0b11111 || straight == 0b1111000000001;

        // unsets right-most 1-bit, which may be the only one set
        boolean hasFlush = (flush & (flush - 1)) == 0;

        if (hasStraight && hasFlush)
            return new Score("straight-flush", 9, hand);

        int total = 0;
        for (int count : faceCount) {
            if (count == 4)
                return new Score("four-of-a-kind", 8, hand);
            if (count == 3)
                total += 3;
            else if (count == 2)
                total += 2;
        }

        if (total == 5)
            return new Score("full-house", 7, hand);

        if (hasFlush)
            return new Score("flush", 6, hand);

        if (hasStraight)
            return new Score("straight", 5, hand);

        if (total == 3)
            return new Score("three-of-a-kind", 4, hand);

        if (total == 4)
            return new Score("two-pair", 3, hand);

        if (total == 2)
            return new Score("one-pair", 2, hand);

        return new Score("high-card", 1, hand);
    }

    private static WildScore analyzeHandWithWildcards(String[] hand) {
        if (Collections.frequency(Arrays.asList(hand), "WW") > 2)
            throw new IllegalArgumentException("too many wildcards");

        return new WildScore(analyzeHandWithWildcardsR(hand, null), hand.clone());
    }

    private static Score analyzeHandWithWildcardsR(String[] hand,
            Score best) {

        for (int i = 0; i < hand.length; i++) {
            if (hand[i].equals("WW")) {
                for (String card : deck) {
                    if (!Arrays.asList(hand).contains(card)) {
                        hand[i] = card;
                        best = analyzeHandWithWildcardsR(hand, best);
                    }
                }
                hand[i] = "WW";
                break;
            }
        }
        Score result = analyzeHand(hand);
        if (best == null || result.weight > best.weight)
            best = result;
        return best;
    }

    private static String[] buildDeck() {
        String[] dck = new String[suits.length() * faces.length()];
        int i = 0;
        for (char s : suits.toCharArray()) {
            for (char f : faces.toCharArray()) {
                dck[i] = "" + f + s;
                i++;
            }
        }
        return dck;
    }

    private static class Score {
        final int weight;
        final String name;
        final String[] hand;

        Score(String n, int w, String[] h) {
            weight = w;
            name = n;
            hand = h != null ? h.clone() : h;
        }

        @Override
        public String toString() {
            return Arrays.toString(hand) + " " + name;
        }
    }

    private static class WildScore {
        final String[] wild;
        final Score score;

        WildScore(Score s, String[] w) {
            score = s;
            wild = w;
        }

        @Override
        public String toString() {
            return String.format("%s%n%s%n", Arrays.toString(wild),
                    score.toString());
        }
    }
}
```


{{out}}

```txt
Regular hands:

[2H, 2D, 2S, KS, QD] three-of-a-kind
[2H, 5H, 7D, 8S, 9D] high-card
[AH, 2D, 3S, 4S, 5S] straight
[2H, 3H, 2D, 3S, 3D] full-house
[2H, 7H, 2D, 3S, 3D] two-pair
[2H, 7H, 7D, 7S, 7C] four-of-a-kind
[TH, JH, QH, KH, AH] straight-flush
[4H, 4C, KC, 5D, TC] one-pair
[QC, TC, 7C, 6C, 4C] flush
[QC, TC, 7C, 7C, TD] invalid hand: duplicates

Hands with wildcards:

[2H, 2D, 2S, KS, WW]
[2H, 2D, 2S, KS, 2C] four-of-a-kind

[2H, 5H, 7D, 8S, WW]
[2H, 5H, 7D, 8S, 8H] one-pair

[AH, 2D, 3S, 4S, WW]
[AH, 2D, 3S, 4S, 5H] straight

[2H, 3H, 2D, 3S, WW]
[2H, 3H, 2D, 3S, 3D] full-house

[2H, 7H, 2D, 3S, WW]
[2H, 7H, 2D, 3S, 2S] three-of-a-kind

[2H, 7H, 7D, WW, WW]
[2H, 7H, 7D, 7S, 7C] four-of-a-kind

[TH, JH, QH, WW, WW]
[TH, JH, QH, AH, KH] straight-flush

[4H, 4C, KC, WW, WW]
[4H, 4C, KC, 4D, 4S] four-of-a-kind

[QC, TC, 7C, WW, WW]
[QC, TC, 7C, AC, KC] flush

[QC, TC, 7H, WW, WW]
[QC, TC, 7H, QH, QD] three-of-a-kind
```




## JavaScript

{{works with|JavaScript|ECMAScript 6}}

```JavaScript
const FACES = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'j', 'q', 'k', 'a'];
const SUITS = ['♥', '♦', '♣', '♠'];

function analyzeHand(hand){
	let cards  = hand.split(' ').filter(x => x !== 'joker');
	let jokers = hand.split(' ').length - cards.length;
	
	let faces = cards.map( card => FACES.indexOf(card.slice(0,-1)) );
	let suits = cards.map( card => SUITS.indexOf(card.slice(-1)) );
	
	if( cards.some( (card, i, self) => i !== self.indexOf(card) ) || faces.some(face => face === -1) || suits.some(suit => suit === -1) ) 
		return 'invalid';
	
	let flush    = suits.every(suit => suit === suits[0]);
	let groups   = FACES.map( (face,i) => faces.filter(j => i === j).length).sort( (x, y) => y - x );
	let shifted  = faces.map(x => (x + 1) % 13);
	let distance = Math.min( Math.max(...faces) - Math.min(...faces), Math.max(...shifted) - Math.min(...shifted));
	let straight = groups[0] === 1 && distance < 5;
	groups[0] += jokers;
	
	if      (groups[0] === 5)                    return 'five-of-a-kind'
	else if (straight && flush)                  return 'straight-flush'
	else if (groups[0] === 4)                    return 'four-of-a-kind'
	else if (groups[0] === 3 && groups[1] === 2) return 'full-house'
	else if (flush)                              return 'flush'
	else if (straight)                           return 'straight'
	else if (groups[0] === 3)                    return 'three-of-a-kind'
	else if (groups[0] === 2 && groups[1] === 2) return 'two-pair'
	else if (groups[0] === 2)                    return 'one-pair'
	else                                         return 'high-card';
}
```

Demonstrating:

```JavaScript
let testHands = [
	"2♥ 2♦ 2♣ k♣ q♦", 
	"2♥ 5♥ 7♦ 8♣ 9♠", 
	"a♥ 2♦ 3♣ 4♣ 5♦", 
	"2♥ 3♥ 2♦ 3♣ 3♦", 
	"2♥ 7♥ 2♦ 3♣ 3♦", 
	"2♥ 7♥ 7♦ 7♣ 7♠", 
	"10♥ j♥ q♥ k♥ a♥", 
	"4♥ 4♠ k♠ 5♦ 10♠",
	"q♣ 10♣ 7♣ 6♣ 4♣",
	"joker 4♣ k♣ 5♦ 10♠",
	"joker 2♦ 2♠ k♠ q♦",
	"joker 3♥ 2♦ 3♠ 3♦",
	"joker 7♥ 7♦ 7♠ 7♣",
	"joker 2♦ joker 4♠ 5♠",
	"joker 2♠ joker a♠ 10♠",
	"joker q♦ joker a♦ 10♦"
];
	
for(hand of testHands) console.log(hand + ": " + analyzeHand(hand));
```

{{out}}

```txt

2♥ 2♦ 2♣ k♣ q♦: three-of-a-kind
2♥ 5♥ 7♦ 8♣ 9♠: high-card
a♥ 2♦ 3♣ 4♣ 5♦: straight
2♥ 3♥ 2♦ 3♣ 3♦: full-house
2♥ 7♥ 2♦ 3♣ 3♦: two-pair
2♥ 7♥ 7♦ 7♣ 7♠: four-of-a-kind
10♥ j♥ q♥ k♥ a♥: straight-flush
4♥ 4♠ k♠ 5♦ 10♠: one-pair
q♣ 10♣ 7♣ 6♣ 4♣: flush
joker 4♣ k♣ 5♦ 10♠: one-pair
joker 2♦ 2♠ k♠ q♦: three-of-a-kind
joker 3♥ 2♦ 3♠ 3♦: four-of-a-kind
joker 7♥ 7♦ 7♠ 7♣: five-of-a-kind
joker 2♦ joker 4♠ 5♠: straight
joker 2♠ joker a♠ 10♠: flush
joker q♦ joker a♦ 10♦: straight-flush

```



## Julia


```julia
sorteddeck = [string(r) * s for s in "♣♦♥♠", r in "23456789TJQKA"]

cardlessthan(card1, card2) = indexin(x, sorteddeck)[1] < indexin(y, sorteddeck)[1]

decksort(d) = sort(d, lt=cardlessthan)

highestrank(d) = string(highestcard(d)[1])

function hasduplicate(d)
    s = sort(d)
    for i in 1:length(s)-1
        if s[i] == s[i+1]
            return true
        end
    end
    false
end

invalid(d) = !all(x -> x in sorteddeck, d) || hasduplicate(d)

function countranks(d)
    ranks = Dict()
    for c in d
        r = string(c[1])
        if !haskey(ranks, r)
            ranks[r] = 1
        else
            ranks[r] += 1
        end
    end
    ranks
end

function countsuits(d)
    suits = Dict()
    for c in d
        s = string(c[2])
        if !haskey(suits, s)
            suits[s] = 1
        else
            suits[s] += 1
        end
    end
    suits
end

const rankmodifiers = Dict("A" => 130, "K" => 120, "Q" => 110, "J" => 100, "T" => 90,
                    "9" => 80, "8" => 70, "7" => 60, "6" => 50, "5" => 40,
                    "4" => 30, "3" => 20, "2" => 10)

rank(card) = rankmodifiers[string(card[1])]

const suitmodifiers = Dict("♠" => 4, "♥" => 3, "♦" => 2, "♣" => 1)

suitrank(card) = suitmodifiers[string(card[2])]

function isstraight(ranksdict)
    v = collect(values(ranksdict))
    if maximum(v) != 1
        return false
    else
        s = sort(map(x->rankmodifiers[x], collect(keys(ranksdict))))
        if s == [10, 20, 30, 40, 130]  # aces low straight
            return true
        else
            for i in 1:length(s)-1
                if abs(s[i] - s[i+1]) > 10
                    return false
                end
            end
        end
    end
    true
end

highestsuit(suitsdict) = sort(collect(keys(suitsdict)), lt=(x,y)->suitsdict[x] < suitsdict[y])[end]

isflush(suitsdict) = length(collect(values(suitsdict))) == 1

isstraightflush(ranks, suits) = isstraight(ranks) && isflush(suits)

isfourofakind(ranksdict) = maximum(values(ranksdict)) == 4 ? true : false

isfullhouse(ranksdict) = sort(collect(values(ranksdict))) == [2, 3]

isthreeofakind(ranksdict) = maximum(values(ranksdict)) == 3 && !isfullhouse(ranksdict) ? true : false

istwopair(ranksdict) = sort(collect(values(ranksdict)))[end-1: end] == [2,2]

isonepair(ranksdict) = sort(collect(values(ranksdict)))[end-1: end] == [1,2]

ishighcard(ranks, suits) = maximum(values(ranks)) == 1 && !isflush(suits) && !isstraight(ranks)

function scorehand(d)
    suits = countsuits(d)
    ranks = countranks(d)
    if invalid(d)
        return "invalid"
    end
    if isstraightflush(ranks, suits) return "straight-flush"
    elseif isfourofakind(ranks)      return "four-of-a-kind"
    elseif isfullhouse(ranks)        return "full-house"
    elseif isflush(suits)            return "flush"
    elseif isstraight(ranks)         return "straight"
    elseif isthreeofakind(ranks)     return "three-of-a-kind"
    elseif istwopair(ranks)          return "two-pair"
    elseif isonepair(ranks)          return "one-pair"
    elseif ishighcard(ranks, suits)  return "high-card"
    end
end

const hands =  [["2♥", "2♦", "2♣", "K♣", "Q♦"], ["2♥", "5♥", "7♦", "8♣", "9♠"], 
   ["A♥", "2♦", "3♣", "4♣", "5♦"], ["2♥", "3♥", "2♦", "3♣", "3♦"],
   ["2♥", "7♥", "2♦", "3♣", "3♦"], ["2♥", "7♥", "7♦", "7♣", "7♠"],
   ["T♥", "J♥", "Q♥", "K♥", "A♥"], ["4♥", "4♠", "K♠", "5♦", "T♠"],
   ["Q♣", "T♣", "7♣", "6♣", "4♣"]]

for hand in hands
    println("Hand $hand is a ", scorehand(hand), " hand.")
end

```
{{output}}
```txt

 Hand ["2♥", "2♦", "2♣", "K♣", "Q♦"] is a three-of-a-kind hand.
 Hand ["2♥", "5♥", "7♦", "8♣", "9♠"] is a high-card hand.
 Hand ["A♥", "2♦", "3♣", "4♣", "5♦"] is a straight hand.
 Hand ["2♥", "3♥", "2♦", "3♣", "3♦"] is a full-house hand.
 Hand ["2♥", "7♥", "2♦", "3♣", "3♦"] is a two-pair hand.
 Hand ["2♥", "7♥", "7♦", "7♣", "7♠"] is a four-of-a-kind hand.
 Hand ["T♥", "J♥", "Q♥", "K♥", "A♥"] is a straight-flush hand.
 Hand ["4♥", "4♠", "K♠", "5♦", "T♠"] is a one-pair hand.
 Hand ["Q♣", "T♣", "7♣", "6♣", "4♣"] is a flush hand.

```



## Kotlin


### Basic Version


```scala
// version 1.1.2

class Card(val face: Int, val suit: Char)

const val FACES = "23456789tjqka"
const val SUITS = "shdc"

fun isStraight(cards: List<Card>): Boolean {
    val sorted = cards.sortedBy { it.face }
    if (sorted[0].face + 4 == sorted[4].face) return true
    if (sorted[4].face == 14 && sorted[0].face == 2 && sorted[3].face == 5) return true 
    return false
}

fun isFlush(cards: List<Card>): Boolean {
    val suit = cards[0].suit
    if (cards.drop(1).all { it.suit == suit }) return true 
    return false
}

fun analyzeHand(hand: String): String {
    val h = hand.toLowerCase()
    val split = h.split(' ').filterNot { it == "" }.distinct()
    if (split.size != 5) return "invalid"
    val cards = mutableListOf<Card>()

    for (s in split) {
        if (s.length != 2) return "invalid"            
        val fIndex = FACES.indexOf(s[0])
        if (fIndex == -1) return "invalid"
        val sIndex = SUITS.indexOf(s[1])
        if (sIndex == -1) return "invalid"
        cards.add(Card(fIndex + 2, s[1]))
    } 
     
    val groups = cards.groupBy { it.face }
    when (groups.size) {
        2 -> {
            if (groups.any { it.value.size == 4 }) return "four-of-a-kind"
            return "full-house"
        }
        3 -> {
            if (groups.any { it.value.size == 3 }) return "three-of-a-kind"
            return "two-pair"
        }
        4 -> return "one-pair" 
        else -> {
            val flush = isFlush(cards)
            val straight = isStraight(cards)
            when {
                flush && straight -> return "straight-flush"
                flush             -> return "flush"
                straight          -> return "straight"
                else              -> return "high-card"
            }
        }
    } 
}    

fun main(args: Array<String>) {
    val hands = arrayOf(
        "2h 2d 2c kc qd",
        "2h 5h 7d 8c 9s",
        "ah 2d 3c 4c 5d",
        "2h 3h 2d 3c 3d",
        "2h 7h 2d 3c 3d",
        "2h 7h 7d 7c 7s",
        "th jh qh kh ah",
        "4h 4s ks 5d ts",
        "qc tc 7c 6c 4c",
        "ah ah 7c 6c 4c"
    )
    for (hand in hands) {
        println("$hand: ${analyzeHand(hand)}")
    }    
}
```


{{out}}

```txt

2h 2d 2c kc qd: three-of-a-kind
2h 5h 7d 8c 9s: high-card
ah 2d 3c 4c 5d: straight
2h 3h 2d 3c 3d: full-house
2h 7h 2d 3c 3d: two-pair
2h 7h 7d 7c 7s: four-of-a-kind
th jh qh kh ah: straight-flush
4h 4s ks 5d ts: one-pair
qc tc 7c 6c 4c: flush
ah ah 7c 6c 4c: invalid

```



### Extra Credit Version


```scala
// version 1.1.2

class Card(val face: Int, val suit: Char)

fun isStraight(cards: List<Card>, jokers: Int): Boolean {
    val sorted = cards.sortedBy { it.face }
    when (jokers) {
        0    -> {
            if (sorted[0].face + 4 == sorted[4].face) return true
            if (sorted[4].face == 14 && sorted[3].face == 5) return true 
            return false
        }
        1    -> {
            if (sorted[0].face + 3 == sorted[3].face) return true
            if (sorted[0].face + 4 == sorted[3].face) return true
            if (sorted[3].face == 14 && sorted[2].face == 4) return true 
            if (sorted[3].face == 14 && sorted[2].face == 5) return true
            return false 
        }
        else -> {
            if (sorted[0].face + 2 == sorted[2].face) return true
            if (sorted[0].face + 3 == sorted[2].face) return true
            if (sorted[0].face + 4 == sorted[2].face) return true
            if (sorted[2].face == 14 && sorted[1].face == 3) return true 
            if (sorted[2].face == 14 && sorted[1].face == 4) return true
            if (sorted[2].face == 14 && sorted[1].face == 5) return true
            return false 
        }
    }  
}

fun isFlush(cards: List<Card>): Boolean {
    val sorted = cards.sortedBy { it.face }
    val suit = sorted[0].suit
    if (sorted.drop(1).all { it.suit == suit || it.suit == 'j' }) return true 
    return false
}

fun analyzeHand(hand: String): String {
    val split = hand.split(' ').filterNot { it == "" }.distinct()
    if (split.size != 5) return "invalid"
    val cards = mutableListOf<Card>()
    var jokers = 0
    for (s in split) {
        if (s.length != 2) return "invalid"
        val cp = s.codePointAt(0)
        val card = when (cp) {
             0x1f0a1 -> Card(14, 's')
             0x1f0b1 -> Card(14, 'h')
             0x1f0c1 -> Card(14, 'd')
             0x1f0d1 -> Card(14, 'c')
             0x1f0cf -> { jokers++; Card(15, 'j') }  // black joker
             0x1f0df -> { jokers++; Card(16, 'j') }  // white joker  
             in 0x1f0a2..0x1f0ab -> Card(cp - 0x1f0a0, 's')
             in 0x1f0ad..0x1f0ae -> Card(cp - 0x1f0a1, 's')
             in 0x1f0b2..0x1f0bb -> Card(cp - 0x1f0b0, 'h')
             in 0x1f0bd..0x1f0be -> Card(cp - 0x1f0b1, 'h')
             in 0x1f0c2..0x1f0cb -> Card(cp - 0x1f0c0, 'd')
             in 0x1f0cd..0x1f0ce -> Card(cp - 0x1f0c1, 'd')
             in 0x1f0d2..0x1f0db -> Card(cp - 0x1f0d0, 'c')
             in 0x1f0dd..0x1f0de -> Card(cp - 0x1f0d1, 'c')
             else                -> Card(0, 'j') // invalid 
        }
        if (card.face == 0) return "invalid"
        cards.add(card)
    } 
     
    val groups = cards.groupBy { it.face }
    when (groups.size) {
        2 -> {
            if (groups.any { it.value.size == 4 }) {
                return when (jokers) {
                    0    -> "four-of-a-kind"
                    else -> "five-of-a-kind"
                }
            }
            return "full-house"
        }
        3 -> {
            if (groups.any { it.value.size == 3 }) {
                return when (jokers) {
                    0    -> "three-of-a-kind"
                    1    -> "four-of-a-kind"
                    else -> "five-of-a-kind"
                }
            } 
            return if (jokers == 0) "two-pair" else "full-house"
        }
        4 -> return when (jokers) {
                    0    -> "one-pair"
                    1    -> "three-of-a-kind"
                    else -> "four-of-a-kind"
             }
        else -> {
            val flush = isFlush(cards) 
            val straight = isStraight(cards,jokers)
            when {
                flush && straight -> return "straight-flush"
                flush             -> return "flush"
                straight          -> return "straight"
                else              -> return if (jokers == 0) "high-card" else "one-pair"
            }
        }
    } 
}    

fun main(args: Array<String>) {
    val hands = arrayOf(
        "🃏 🃂 🂢 🂮 🃍",
        "🃏 🂵 🃇 🂨 🃉",
        "🃏 🃂 🂣 🂤 🂥",
        "🃏 🂳 🃂 🂣 🃃",
        "🃏 🂷 🃂 🂣 🃃",
        "🃏 🂷 🃇 🂧 🃗",
        "🃏 🂻 🂽 🂾 🂱",
        "🃏 🃔 🃞 🃅 🂪",
        "🃏 🃞 🃗 🃖 🃔",
        "🃏 🃂 🃟 🂤 🂥",
        "🃏 🃍 🃟 🂡 🂪",
        "🃏 🃍 🃟 🃁 🃊",
        "🃏 🃂 🂢 🃟 🃍",
        "🃏 🃂 🂢 🃍 🃍",
        "🃂 🃞 🃍 🃁 🃊"
    )
    for (hand in hands) {
        println("$hand : ${analyzeHand(hand)}")
    }    
}
```


{{out}}

```txt

🃏 🃂 🂢 🂮 🃍 : three-of-a-kind
🃏 🂵 🃇 🂨 🃉 : straight
🃏 🃂 🂣 🂤 🂥 : straight
🃏 🂳 🃂 🂣 🃃 : four-of-a-kind
🃏 🂷 🃂 🂣 🃃 : three-of-a-kind
🃏 🂷 🃇 🂧 🃗 : five-of-a-kind
🃏 🂻 🂽 🂾 🂱 : straight-flush
🃏 🃔 🃞 🃅 🂪 : one-pair
🃏 🃞 🃗 🃖 🃔 : flush
🃏 🃂 🃟 🂤 🂥 : straight
🃏 🃍 🃟 🂡 🂪 : straight
🃏 🃍 🃟 🃁 🃊 : straight-flush
🃏 🃂 🂢 🃟 🃍 : four-of-a-kind
🃏 🃂 🂢 🃍 🃍 : invalid
🃂 🃞 🃍 🃁 🃊 : high-card

```



## Lua


```lua
-- Check whether t is a valid poker hand
function valid (t)
    if #t ~= 5 then return false end
    for k, v in pairs(t) do
        for key, card in pairs(t) do
            if  v.value == card.value and
                v.suit == card.suit and
                k ~= key
            then
                return false
            end
        end
    end
    return true
end

-- Return numerical value of a single card
function cardValue (card)
    local val = card:sub(1, -2)
    local n = tonumber(val)
    if n then return n end
    if val == "j" then return 11 end
    if val == "q" then return 12 end
    if val == "k" then return 13 end
    if val == "a" then return 1 end
    error("Invalid card value: " .. val)
end

-- Detect whether hand t is a straight
function straight (t)
    table.sort(t, function (a, b) return a.value < b.value end)
    local ace, thisValue, lastValue = false
    for i = 2, #t do
        thisValue, lastValue = t[i].value, t[i-1].value
        if lastValue == 1 then ace = i - 1 end
        if thisValue ~= lastValue + 1 then
            if ace then
                t[ace].value = 14
                return straight(t)
            else
                return false
            end
        end
    end
    return true
end

-- Detect whether hand t is a flush
function isFlush (t)
    local suit = t[1].suit
    for card = 2, #t do
        if t[card].suit ~= suit then return false end
    end
    return true
end

-- Return a table of the count of each card value in hand t
function countValues (t)
    local countTab, maxCount = {}, 0
    for k, v in pairs(t) do
        if countTab[v.value] then
            countTab[v.value] = countTab[v.value] + 1
        else
            countTab[v.value] = 1
        end
    end
    return countTab
end

-- Find the highest value in t
function highestCount (t)
    local maxCount = 0
    for k, v in pairs(t) do
        if v > maxCount then maxCount = v end
    end
    return maxCount
end

-- Detect full-house and two-pair using the value counts in t
function twoTypes (t)
    local threes, twos = 0, 0
    for k, v in pairs(t) do
        if v == 3 then threes = threes + 1 end
        if v == 2 then twos = twos + 1 end
    end
    return threes, twos
end

-- Return the rank of a poker hand represented as a string
function rank (cards)
    local hand = {}
    for card in cards:gmatch("%S+") do
        table.insert(hand, {value = cardValue(card), suit = card:sub(-1, -1)})
    end
    if not valid(hand) then return "invalid" end
    local st, fl = straight(hand), isFlush(hand)
    if st and fl then return "straight-flush" end
    local valCount = countValues(hand)
    local highCount = highestCount(valCount)
    if highCount == 4 then return "four-of-a-kind" end
    local n3, n2 = twoTypes(valCount)
    if n3 == 1 and n2 == 1 then return "full-house" end
    if fl then return "flush" end
    if st then return "straight" end
    if highCount == 3 then return "three-of-a-kind" end
    if n3 == 0 and n2 == 2 then return "two-pair" end
    if highCount == 2 then return "one-pair" end
    return "high-card"
end

-- Main procedure
local testCases = {
    "2h 2d 2c kc qd", -- three-of-a-kind
    "2h 5h 7d 8c 9s", -- high-card
    "ah 2d 3c 4c 5d", -- straight
    "2h 3h 2d 3c 3d", -- full-house
    "2h 7h 2d 3c 3d", -- two-pair
    "2h 7h 7d 7c 7s", -- four-of-a-kind 
    "10h jh qh kh ah",-- straight-flush
    "4h 4s ks 5d 10s",-- one-pair
    "qc 10c 7c 6c 4c" -- flush
}
for _, case in pairs(testCases) do print(case, ": " .. rank(case)) end
```

{{out}}

```txt
2h 2d 2c kc qd  : three-of-a-kind
2h 5h 7d 8c 9s  : high-card
ah 2d 3c 4c 5d  : straight
2h 3h 2d 3c 3d  : full-house
2h 7h 2d 3c 3d  : two-pair
2h 7h 7d 7c 7s  : four-of-a-kind
10h jh qh kh ah : straight-flush
4h 4s ks 5d 10s : one-pair
qc 10c 7c 6c 4c : flush
```



## Perl

I dont like jokers. Instead I decided to give hands proper names. For example, "Kings full of Tens" rather than just "full-house".


```perl

use strict;
use warnings;
use utf8;
use feature 'say';
use open qw<:encoding(utf-8) :std>;
 
package Hand {
    sub describe {
        my $str = pop;
        my $hand = init($str);
        return "$str: INVALID" if !$hand;
        return analyze($hand);
    }
 
    sub init {
        (my $str = lc shift) =~ tr/234567891jqka♥♦♣♠//cd;
        return if $str !~ m/\A (?: [234567891jqka] [♥♦♣♠] ){5} \z/x;
        for (my ($i, $cnt) = (0, 0); $i < 10; $i += 2, $cnt = 0) {
            my $try = substr $str, $i, 2;
            ++$cnt while $str =~ m/$try/g;
            return if $cnt > 1;
        }
        my $suits = $str =~ tr/234567891jqka//dr;
        my $ranks = $str =~ tr/♥♦♣♠//dr;
        return {
            hand  => $str,
            suits => $suits,
            ranks => $ranks,
        };
    }
 
    sub analyze {
        my $hand = shift;
        my @ranks = split //, $hand->{ranks};
        my %cards;
        for (@ranks) {
            $_ = 10, next if $_ eq '1';
            $_ = 11, next if $_ eq 'j';
            $_ = 12, next if $_ eq 'q';
            $_ = 13, next if $_ eq 'k';
            $_ = 14, next if $_ eq 'a';
        } continue {
            ++$cards{ $_ };
        }
        my $kicker = 0;
        my (@pairs, $set, $quads, $straight, $flush);
 
        while (my ($card, $count) = each %cards) {
            if ($count == 1) {
                $kicker = $card if $kicker < $card;
            }
            elsif ($count == 2) {
                push @pairs, $card;
            }
            elsif ($count == 3) {
                $set = $card;
            }
            elsif ($count == 4) {
                $quads = $card;
            }
            else {
                die "Five of a kind? Cheater!\n";
            }
        }
        $flush    = 1 if $hand->{suits} =~ m/\A (.) \1 {4}/x;
        $straight = check_straight(@ranks);
        return get_high($kicker, \@pairs, $set, $quads, $straight, $flush,);
    }
 
    sub check_straight {
        my $sequence = join ' ', sort { $a <=> $b } @_;
        return 1       if index('2 3 4 5 6 7 8 9 10 11 12 13 14', $sequence) != -1;
        return 'wheel' if index('2 3 4 5 14 6 7 8 9 10 11 12 13', $sequence) ==  0;
        return undef;
    }
 
    sub get_high {
        my ($kicker, $pairs, $set, $quads, $straight, $flush) = @_;
        $kicker = to_s($kicker, 's');
        return 'straight-flush: Royal Flush!'
            if $straight && $flush && $kicker eq 'Ace' && $straight ne 'wheel';
        return "straight-flush: Steel Wheel!"
            if $straight && $flush && $straight eq 'wheel';
        return "straight-flush: $kicker high"
            if $straight && $flush;
        return 'four-of-a-kind: '. to_s($quads, 'p')
            if $quads;
        return 'full-house: '. to_s($set, 'p') .' full of '. to_s($pairs->[0], 'p')
            if $set && @$pairs;
        return "flush: $kicker high"
            if $flush;
        return 'straight: Wheel!'
            if $straight && $straight eq 'wheel';
        return "straight: $kicker high"
            if $straight;
        return 'three-of-a-kind: '. to_s($set, 'p')
            if $set;
        return 'two-pairs: '. to_s($pairs->[0], 'p') .' and '. to_s($pairs->[1], 'p')
            if @$pairs == 2;
        return 'one-pair: '. to_s($pairs->[0], 'p')
            if @$pairs == 1;
        return "high-card: $kicker";
    }
 
    my %to_str = (
         2 => 'Two',    3 => 'Three', 4 => 'Four',  5 => 'Five', 6 => 'Six',
         7 => 'Seven',  8 => 'Eight', 9 => 'Nine', 10 => 'Ten', 11 => 'Jack',
        12 => 'Queen', 13 => 'King', 14 => 'Ace',
    );
    my %to_str_diffs = (2 => 'Deuces', 6 => 'Sixes',);
 
    sub to_s {
        my ($num, $verb) = @_;
        # verb is 'singular' or 'plural' (or 's' or 'p')
        if ($verb =~ m/\A p/xi) {
            return $to_str_diffs{ $num } if $to_str_diffs{ $num };
            return $to_str{ $num } .'s';
        }
        return $to_str{ $num };
    }
}
 
my @cards = (
    '10♥ j♥  q♥ k♥ a♥',
    '2♥  3♥  4♥ 5♥ a♥',
    '2♥  2♣  2♦ 3♣ 2♠',
    '10♥ K♥  K♦ K♣ 10♦',
    'q♣  10♣ 7♣ 6♣ 3♣',
    '5♣  10♣ 7♣ 6♣ 4♣',
    '9♥  10♥ q♥ k♥ j♣',
    'a♥  a♣  3♣ 4♣ 5♦',
    '2♥  2♦  2♣ k♣ q♦',
    '6♥  7♥  6♦ j♣ j♦',
    '2♥  6♥  2♦ 3♣ 3♦',
    '7♥  7♠  k♠ 3♦ 10♠',
    '4♥  4♠  k♠ 2♦ 10♠',
    '2♥  5♥  j♦ 8♣ 9♠',
    '2♥  5♥  7♦ 8♣ 9♠',
    'a♥  a♥  3♣ 4♣ 5♦', # INVALID: duplicate aces
);
 
say Hand::describe($_) for @cards;

```

{{out}}

```txt

straight-flush: Royal Flush!
straight-flush: Steel Wheel!
four-of-a-kind: Deuces
full-house: Kings full of Tens
flush: Queen high
flush: Ten high
straight: King high
one-pair: Aces
three-of-a-kind: Deuces
two-pairs: Sixes and Jacks
two-pairs: Threes and Deuces
one-pair: Sevens
one-pair: Fours
high-card: Jack
high-card: Nine
a♥  a♥  3♣ 4♣ 5♦: INVALID

```



## Perl 6

This solution handles jokers. It has been written to use a Perl 6 grammar.

```perl6
use v6;
 
grammar PokerHand {
 
    # Perl6 Grammar to parse and rank 5-card poker hands
    # E.g. PokerHand.parse("2♥ 3♥ 2♦ 3♣ 3♦");
    # 2013-12-21: handle 'joker' wildcards; maximum of two
 
    rule TOP {
         :my %*PLAYED;
         { %*PLAYED = () }
         [ <face-card> | <joker> ]**5
    }
 
    token face-card {<face><suit> <?{
            my $card = ~$/.lc;
            # disallow duplicates
            ++%*PLAYED{$card} <= 1;
       }>
    }
 
    token joker {:i 'joker' <?{
            my $card = ~$/.lc;
            # allow two jokers in a hand
            ++%*PLAYED{$card} <= 2;
        }>
    }
 
    token face {:i <[2..9 jqka]> | 10 }
    token suit {<[♥ ♦ ♣ ♠]>}
}

class PokerHand::Actions {
    method TOP($/) {
        my UInt @n    = n-of-a-kind($/);
        my $flush     = flush($/);
        my $straight  = straight($/);
        make rank(@n[0], @n[1], $flush, $straight);
    }
    multi sub rank(5,$,$,$)                    { 'five-of-a-kind' }
    multi sub rank($,$,$f,$s where {$f && $s}) { 'straight-flush' }
    multi sub rank(4,$,$,$)                    { 'four-of-a-kind' }
    multi sub rank($,$,$f,$ where {$f})        { 'flush' }
    multi sub rank($,$,$,$s where {$s})        { 'straight' }
    multi sub rank(3,2,$,$)                    { 'full-house' }
    multi sub rank(3,$,$,$)                    { 'three-of-a-kind' }
    multi sub rank(2,2,$,$)                    { 'two-pair' }
    multi sub rank(2,$,$,$)                    { 'one-pair' }
    multi sub rank($,$,$,$) is default         { 'high-card' }
  
    sub n-of-a-kind($/) {
        my %faces := bag @<face-card>.map: -> $/ {~$<face>.lc};
        my @counts = %faces.values.sort.reverse;
        @counts[0] += @<joker>;
        return @counts;
    }
 
    sub flush($/) {
        my @suits = unique @<face-card>.map: -> $/ {~$<suit>};
        return +@suits == 1;
    }
 
    sub straight($/) {
        # allow both ace-low and ace-high straights
        constant @Faces = [ "a 2 3 4 5 6 7 8 9 10 j q k a".split: ' ' ];
        constant @Possible-Straights = [ (0 .. (+@Faces - 5)).map: { set @Faces[$_ .. $_+4] } ];

        my $faces = set @<face-card>.map: -> $/ {~$<face>.lc};
        my $jokers = +@<joker>;
 
        return ?( @Possible-Straights.first: { +($faces ∩ $_) + $jokers == 5 } );
    }
}

my PokerHand::Actions $actions .= new;

for ("2♥ 2♦ 2♣ k♣ q♦",   # three-of-a-kind
     "2♥ 5♥ 7♦ 8♣ 9♠",   # high-card
     "a♥ 2♦ 3♣ 4♣ 5♦",   # straight
     "2♥ 3♥ 2♦ 3♣ 3♦",   # full-house
     "2♥ 7♥ 2♦ 3♣ 3♦",   # two-pair
     "2♥ 7♥ 7♦ 7♣ 7♠",   # four-of-a-kind
     "10♥ j♥ q♥ k♥ a♥",  # straight-flush
     "4♥ 4♠ k♠ 5♦ 10♠",  # one-pair
     "q♣ 10♣ 7♣ 6♣ 4♣",  # flush
     "a♥ a♥ 3♣ 4♣ 5♦",   # invalid
     ## EXTRA CREDIT ##
     "joker  2♦  2♠  k♠  q♦",  # three-of-a-kind
     "joker  5♥  7♦  8♠  9♦",  # straight
     "joker  2♦  3♠  4♠  5♠",  # straight
     "joker  3♥  2♦  3♠  3♦",  # four-of-a-kind
     "joker  7♥  2♦  3♠  3♦",  # three-of-a-kind
     "joker  7♥  7♦  7♠  7♣",  # five-of-a-kind
     "joker  j♥  q♥  k♥  A♥",  # straight-flush
     "joker  4♣  k♣  5♦ 10♠",  # one-pair
     "joker  k♣  7♣  6♣  4♣",  # flush
     "joker  2♦ joker  4♠  5♠",  # straight
     "joker  Q♦ joker  A♠ 10♠",  # straight
     "joker  Q♦ joker  A♦ 10♦",  # straight-flush
     "joker  2♦ 2♠  joker  q♦",  # four of a kind
    ) {
    my $rank = do with PokerHand.parse($_, :$actions) {
        .ast;
    }
    else {
        'invalid';
    }
    say "$_: $rank";
}

```

{{out}}

```txt
2♥ 2♦ 2♣ k♣ q♦: three-of-a-kind
2♥ 5♠ 7♦ 8♣ 9♠: high-card
a♠ 2♦ 3♣ 4♣ 5♦: straight
2♥ 3♠ 2♦ 3♣ 3♦: full-house
2♥ 7♠ 2♦ 3♣ 3♦: two-pair
2♥ 7♥ 7♦ 7♣ 7♠: four-of-a-kind
10♠ j♠ q♠ k♠ a♠: straight-flush
4♥ 4♠ k♠ 5♦ 10♠: one-pair
q♣ 10♣ 7♣ 6♣ 4♣: flush
a♥ a♥ 3♣ 4♣ 5♦: invalid
joker  2♦  2♠  k♠  q♦: three-of-a-kind
joker  5♠  7♦  8♠  9♦: straight
joker  2♦  3♠  4♠  5♠: straight
joker  3♥  2♦  3♠  3♦: four-of-a-kind
joker  7♥  2♦  3♠  3♦: three-of-a-kind
joker  7♥  7♦  7♠  7♣: five-of-a-kind
joker  j♠  q♠  k♠  A♠: straight-flush
joker  4♣  k♣  5♦ 10♠: one-pair
joker  k♣  7♣  6♣  4♣: flush
joker  2♦ joker  4♠  5♠: straight
joker  Q♦ joker  A♠ 10♠: straight
joker  Q♦ joker  A♦ 10♦: straight-flush
joker  2♦ 2♠  joker  q♦: four-of-a-kind
```



## Phix

Woke up this morning with a neat idea for detecting straights, though jokers messed it up a bit.

Uses an ad-hoc ranking system/tie breaker, not recommended for use in tournaments! Displays hands best-first.

Note: I have left a copy of this in demo\HelloUTF8.exw to prove it works, but non-ascii on a Windows
console is not Phix's forte.

For an example of using the unicode card characters see [[Playing_cards#Phix]]

```Phix
function poker(string hand)
    hand = substitute(hand,"10","t")
    sequence cards = split(hand,no_empty:=1)
    if length(cards)!=5 then return "invalid hand" end if
    sequence ranks = repeat(0,13),
             suits = repeat(0,4)
    integer jokers = 0
    for i=1 to length(cards) do
        sequence ci = utf8_to_utf32(cards[i])
        if ci="joker" then
            jokers += 1
            if jokers>2 then return "invalid hand" end if
        else
            if length(ci)!=2 then return "invalid hand" end if
            integer rank = find(lower(ci[1]),"23456789tjqka")
            integer suit = find(ci[2],utf8_to_utf32("♥♣♦♠"))
            if rank=0 or suit=0 then return "invalid hand" end if
            ranks[rank] += 1
            suits[suit] += 1
        end if
    end for
    integer straight = match({1,1,1,1,1},ranks) 
    if not straight then 
        straight = sort(ranks)[$]=1 and match({0,0,0,0,0,0,0,0},ranks)
    end if
    integer _flush = (max(suits)+jokers = 5)
    integer _pairs = max(ranks)+jokers
    integer pair = find(2,ranks)
    integer full_house = _pairs=3 and pair and (jokers=0 or find(2,ranks,pair+1))
    integer two_pair = find(2,ranks,pair+1)
    integer high_card = rfind(1,sq_ne(ranks,0))+1
    if jokers and _pairs=jokers+1 then
        straight = 1
        integer k = find(1,ranks), j = jokers
        for l=k to min(k+5-j,13) do
            if ranks[l]=0 then
                if j=0 then
                    straight = 0
                    exit
                end if
                j -= 1
            end if
        end for
        if straight and j then
            high_card = min(high_card+j,14)
        end if
    elsif straight and ranks[1]!=0 then 
        high_card = find(0,ranks)
    end if
    if _pairs=5             then return {10,"five of a kind", find(5-jokers,ranks)+1} end if
    if straight and _flush  then return {9,"straight flush", high_card} end if
    if _pairs=4             then return {8,"four of a kind", find(4-jokers,ranks)+1} end if
    if full_house           then return {7,"full house", find(3-jokers,ranks)+1} end if
    if _flush               then return {6,"flush", high_card} end if
    if straight             then return {5,"straight", high_card} end if
    if _pairs=3             then return {4,"three of a kind", find(3-jokers,ranks)+1} end if
    if pair and two_pair    then return {3,"two pair", two_pair+1} end if
    if pair                 then return {2,"one pair", pair+1} end if
    if jokers               then return {2,"one pair", high_card} end if
                                 return {1,"high card",high_card}
end function

sequence hands = {{0,"2♥ 2♦ 2♣ k♣ q♦"},
                  {0,"2♥ 5♥ 7♦ 8♣ 9♠"},
                  {0,"a♥ 2♦ 3♣ 4♣ 5♦"},
                  {0,"2♥ 3♥ 2♦ 3♣ 3♦"},
                  {0,"2♥ 7♥ 2♦ 3♣ 3♦"},
                  {0,"2♥ 7♥ 7♦ 7♣ 7♠"},
                  {0,"10♥ j♥ q♥ k♥ a♥"},
                  {0,"4♥ 4♠ k♠ 5♦ 10♠"},
                  {0,"q♣ 10♣ 7♣ 6♣ 4♣"},
                  {0,"joker  2♦  2♠  k♠  q♦"},
                  {0,"joker  5♥  7♦  8♠  9♦"},
                  {0,"joker  2♦  3♠  4♠  5♠"},
                  {0,"joker  3♥  2♦  3♠  3♦"},
                  {0,"joker  7♥  2♦  3♠  3♦"},
                  {0,"joker  7♥  7♦  7♠  7♣"},
                  {0,"joker  j♥  q♥  k♥  A♥"},
                  {0,"joker  4♣  k♣  5♦ 10♠"},
                  {0,"joker  k♣  7♣  6♣  4♣"},
                  {0,"joker  2♦  joker  4♠  5♠"},
                  {0,"joker  Q♦  joker  A♠ 10♠"},
                  {0,"joker  Q♦  joker  A♦ 10♦"},
                  {0,"joker  2♦  2♠  joker  q♦"}}

for i=1 to length(hands) do
    hands[i][1] = poker(hands[i][2])
end for
ppOpt({pp_Ascii,{#20,#FF}})
pp(reverse(sort(hands)))
```

{{out}}

```txt

{{{10, "five of a kind", 7}, "joker  7♥  7♦  7♠  7♣"},
 {{9, "straight flush", 14}, "joker  j♥  q♥  k♥  A♥"},
 {{9, "straight flush", 14}, "joker  Q♦  joker  A♦ 10♦"},
 {{9, "straight flush", 14}, "10♥ j♥ q♥ k♥ a♥"},
 {{8, "four of a kind", 7}, "2♥ 7♥ 7♦ 7♣ 7♠"},
 {{8, "four of a kind", 3}, "joker  3♥  2♦  3♠  3♦"},
 {{8, "four of a kind", 2}, "joker  2♦  2♠  joker  q♦"},
 {{7, "full house", 3}, "2♥ 3♥ 2♦ 3♣ 3♦"},
 {{6, "flush", 13}, "joker  k♣  7♣  6♣  4♣"},
 {{6, "flush", 12}, "q♣ 10♣ 7♣ 6♣ 4♣"},
 {{5, "straight", 14}, "joker  Q♦  joker  A♠ 10♠"},
 {{5, "straight", 9}, "joker  5♥  7♦  8♠  9♦"},
 {{5, "straight", 6}, "joker  2♦  joker  4♠  5♠"},
 {{5, "straight", 5}, "joker  2♦  3♠  4♠  5♠"},
 {{5, "straight", 5}, "a♥ 2♦ 3♣ 4♣ 5♦"},
 {{4, "three of a kind", 3}, "joker  7♥  2♦  3♠  3♦"},
 {{4, "three of a kind", 2}, "joker  2♦  2♠  k♠  q♦"},
 {{4, "three of a kind", 2}, "2♥ 2♦ 2♣ k♣ q♦"},
 {{3, "two pair", 3}, "2♥ 7♥ 2♦ 3♣ 3♦"},
 {{2, "one pair", 13}, "joker  4♣  k♣  5♦ 10♠"},
 {{2, "one pair", 4}, "4♥ 4♠ k♠ 5♦ 10♠"},
 {{1, "high card", 9}, "2♥ 5♥ 7♦ 8♣ 9♠"}}

```



## PicoLisp

(rassoc) function in picolisp after 3.1.9.10.

```PicoLisp
(setq *Rank
   '(("2" . 0) ("3" . 1) ("4" . 2)
      ("5" . 3) ("6" . 4) ("7" . 5)
      ("8" . 6) ("9" . 7) ("t" . 8)
      ("j" . 9) ("q" . 10) ("k" . 11)
      ("a" . 12) ) )
(de poker (Str)
   (let (S NIL  R NIL  Seq NIL)
      (for (L (chop Str) (cdr L) (cdddr L))
         (accu 'R (cdr (assoc (car L) *Rank)) 1)
         (accu 'S (cadr L) 1) )
      (setq Seq
         (make
            (for (L (by car sort R) (cdr L) (cdr L))
               (link (- (caar L) (caadr L))) ) ) )
      (cond
         ((and
            (= 5 (cdar S))
            (or
               (= (-1 -1 -1 -1) Seq)
               (= (-1 -1 -1 -9) Seq) ) )
            'straight-flush )
         ((rassoc 4 R) 'four-of-a-kind)
         ((and (rassoc 2 R) (rassoc 3 R)) 'full-house)
         ((= 5 (cdar S)) 'flush)
         ((or
            (= (-1 -1 -1 -1) Seq)
            (= (-1 -1 -1 -9) Seq) )
            'straight )
         ((rassoc 3 R) 'three-of-a-kind)
         ((=
            2
            (cnt '((L) (= 2 (cdr L))) R) )
            'two-pair )
         ((rassoc 2 R) 'pair)
         (T 'high-card) ) ) )
```



## Prolog

{{works with|GNU Prolog|1.4.4}}
Not very efficient version.

```python
:- initialization(main).


faces([a,k,q,j,10,9,8,7,6,5,4,3,2]).

face(F) :- faces(Fs), member(F,Fs).
suit(S) :- member(S, ['♥','♦','♣','♠']).


best_hand(Cards,H) :-
    straight_flush(Cards,C) -> H = straight-flush(C)
  ; many_kind(Cards,F,4)    -> H = four-of-a-kind(F)
  ; full_house(Cards,F1,F2) -> H = full-house(F1,F2)
  ; flush(Cards,S)          -> H = flush(S)
  ; straight(Cards,F)       -> H = straight(F)
  ; many_kind(Cards,F,3)    -> H = three-of-a-kind(F)
  ; two_pair(Cards,F1,F2)   -> H = two-pair(F1,F2)
  ; many_kind(Cards,F,2)    -> H = one-pair(F)
  ; many_kind(Cards,F,1)    -> H = high-card(F)
  ;                            H = invalid
  .

straight_flush(Cards, c(F,S)) :- straight(Cards,F), flush(Cards,S).

full_house(Cards,F1,F2) :-
    many_kind(Cards,F1,3), many_kind(Cards,F2,2), F1 \= F2.

flush(Cards,S) :- maplist(has_suit(S), Cards).
has_suit(S, c(_,S)).

straight(Cards,F) :-
    select(c(F,_), Cards, Cs), pred_face(F,F1), straight(Cs,F1).
straight([],_).
pred_face(F,F1) :- F = 2 -> F1 = a ; faces(Fs), append(_, [F,F1|_], Fs).

two_pair(Cards,F1,F2) :-
    many_kind(Cards,F1,2), many_kind(Cards,F2,2), F1 \= F2.

many_kind(Cards,F,N) :-
    face(F), findall(_, member(c(F,_), Cards), Xs), length(Xs,N).


% utils/parser
parse_line(Cards)  --> " ", parse_line(Cards).
parse_line([C|Cs]) --> parse_card(C), parse_line(Cs).
parse_line([])     --> [].

parse_card(c(F,S)) --> parse_face(F), parse_suit(S).

parse_suit(S,In,Out) :- suit(S), atom_codes(S,Xs), append(Xs,Out,In).
parse_face(F,In,Out) :- face(F), face_codes(F,Xs), append(Xs,Out,In).

face_codes(F,Xs) :- number(F) -> number_codes(F,Xs) ; atom_codes(F,Xs).


% tests
test(" 2♥  2♦ 2♣ k♣  q♦").
test(" 2♥  5♥ 7♦ 8♣  9♠").
test(" a♥  2♦ 3♣ 4♣  5♦").
test(" 2♥  3♥ 2♦ 3♣  3♦").
test(" 2♥  7♥ 2♦ 3♣  3♦").
test(" 2♥  7♥ 7♦ 7♣  7♠").
test("10♥  j♥ q♥ k♥  a♥").
test(" 4♥  4♠ k♠ 5♦ 10♠").
test(" q♣ 10♣ 7♣ 6♣  4♣").

run_tests :-
    test(Line), phrase(parse_line(Cards), Line), best_hand(Cards,H)
  , write(Cards), write('\t'), write(H), nl
  .
main :- findall(_, run_tests, _), halt.
```

{{out}}

```txt
[c(2,♥),c(2,♦),c(2,♣),c(k,♣),c(q,♦)]	three-of-a-kind(2)
[c(2,♥),c(5,♥),c(7,♦),c(8,♣),c(9,♠)]	high-card(9)
[c(a,♥),c(2,♦),c(3,♣),c(4,♣),c(5,♦)]	straight(5)
[c(2,♥),c(3,♥),c(2,♦),c(3,♣),c(3,♦)]	full-house(3,2)
[c(2,♥),c(7,♥),c(2,♦),c(3,♣),c(3,♦)]	two-pair(3,2)
[c(2,♥),c(7,♥),c(7,♦),c(7,♣),c(7,♠)]	four-of-a-kind(7)
[c(10,♥),c(j,♥),c(q,♥),c(k,♥),c(a,♥)]	straight-flush(c(a,♥))
[c(4,♥),c(4,♠),c(k,♠),c(5,♦),c(10,♠)]	one-pair(4)
[c(q,♣),c(10,♣),c(7,♣),c(6,♣),c(4,♣)]	flush(♣)
```



## Python

Goes a little further in also giving the ordered tie-breaker information from the wikipedia page. 

```python
from collections import namedtuple

class Card(namedtuple('Card', 'face, suit')):
    def __repr__(self):
        return ''.join(self)


suit = '♥ ♦ ♣ ♠'.split()
# ordered strings of faces
faces   = '2 3 4 5 6 7 8 9 10 j q k a'
lowaces = 'a 2 3 4 5 6 7 8 9 10 j q k'
# faces as lists
face   = faces.split()
lowace = lowaces.split()


def straightflush(hand):
    f,fs = ( (lowace, lowaces) if any(card.face == '2' for card in hand)
             else (face, faces) )
    ordered = sorted(hand, key=lambda card: (f.index(card.face), card.suit))
    first, rest = ordered[0], ordered[1:]
    if ( all(card.suit == first.suit for card in rest) and
         ' '.join(card.face for card in ordered) in fs ):
        return 'straight-flush', ordered[-1].face
    return False

def fourofakind(hand):
    allfaces = [f for f,s in hand]
    allftypes = set(allfaces)
    if len(allftypes) != 2:
        return False
    for f in allftypes:
        if allfaces.count(f) == 4:
            allftypes.remove(f)
            return 'four-of-a-kind', [f, allftypes.pop()]
    else:
        return False

def fullhouse(hand):
    allfaces = [f for f,s in hand]
    allftypes = set(allfaces)
    if len(allftypes) != 2:
        return False
    for f in allftypes:
        if allfaces.count(f) == 3:
            allftypes.remove(f)
            return 'full-house', [f, allftypes.pop()]
    else:
        return False

def flush(hand):
    allstypes = {s for f, s in hand}
    if len(allstypes) == 1:
        allfaces = [f for f,s in hand]
        return 'flush', sorted(allfaces,
                               key=lambda f: face.index(f),
                               reverse=True)
    return False

def straight(hand):
    f,fs = ( (lowace, lowaces) if any(card.face == '2' for card in hand)
             else (face, faces) )
    ordered = sorted(hand, key=lambda card: (f.index(card.face), card.suit))
    first, rest = ordered[0], ordered[1:]
    if ' '.join(card.face for card in ordered) in fs:
        return 'straight', ordered[-1].face
    return False

def threeofakind(hand):
    allfaces = [f for f,s in hand]
    allftypes = set(allfaces)
    if len(allftypes) <= 2:
        return False
    for f in allftypes:
        if allfaces.count(f) == 3:
            allftypes.remove(f)
            return ('three-of-a-kind', [f] +
                     sorted(allftypes,
                            key=lambda f: face.index(f),
                            reverse=True))
    else:
        return False

def twopair(hand):
    allfaces = [f for f,s in hand]
    allftypes = set(allfaces)
    pairs = [f for f in allftypes if allfaces.count(f) == 2]
    if len(pairs) != 2:
        return False
    p0, p1 = pairs
    other = [(allftypes - set(pairs)).pop()]
    return 'two-pair', pairs + other if face.index(p0) > face.index(p1) else pairs[::-1] + other

def onepair(hand):
    allfaces = [f for f,s in hand]
    allftypes = set(allfaces)
    pairs = [f for f in allftypes if allfaces.count(f) == 2]
    if len(pairs) != 1:
        return False
    allftypes.remove(pairs[0])
    return 'one-pair', pairs + sorted(allftypes,
                                      key=lambda f: face.index(f),
                                      reverse=True)

def highcard(hand):
    allfaces = [f for f,s in hand]
    return 'high-card', sorted(allfaces,
                               key=lambda f: face.index(f),
                               reverse=True)

handrankorder =  (straightflush, fourofakind, fullhouse,
                  flush, straight, threeofakind,
                  twopair, onepair, highcard)
              
def rank(cards):
    hand = handy(cards)
    for ranker in handrankorder:
        rank = ranker(hand)
        if rank:
            break
    assert rank, "Invalid: Failed to rank cards: %r" % cards
    return rank

def handy(cards='2♥ 2♦ 2♣ k♣ q♦'):
    hand = []
    for card in cards.split():
        f, s = card[:-1], card[-1]
        assert f in face, "Invalid: Don't understand card face %r" % f
        assert s in suit, "Invalid: Don't understand card suit %r" % s
        hand.append(Card(f, s))
    assert len(hand) == 5, "Invalid: Must be 5 cards in a hand, not %i" % len(hand)
    assert len(set(hand)) == 5, "Invalid: All cards in the hand must be unique %r" % cards
    return hand


if __name__ == '__main__':
    hands = ["2♥ 2♦ 2♣ k♣ q♦",
     "2♥ 5♥ 7♦ 8♣ 9♠",
     "a♥ 2♦ 3♣ 4♣ 5♦",
     "2♥ 3♥ 2♦ 3♣ 3♦",
     "2♥ 7♥ 2♦ 3♣ 3♦",
     "2♥ 7♥ 7♦ 7♣ 7♠",
     "10♥ j♥ q♥ k♥ a♥"] + [
     "4♥ 4♠ k♠ 5♦ 10♠",
     "q♣ 10♣ 7♣ 6♣ 4♣",
     ]
    print("%-18s %-15s %s" % ("HAND", "CATEGORY", "TIE-BREAKER"))
    for cards in hands:
        r = rank(cards)
        print("%-18r %-15s %r" % (cards, r[0], r[1]))
```

{{out}}

```txt
HAND               CATEGORY        TIE-BREAKER
'2♥ 2♦ 2♣ k♣ q♦'   three-of-a-kind ['2', 'k', 'q']
'2♥ 5♥ 7♦ 8♣ 9♠'   high-card       ['9', '8', '7', '5', '2']
'a♥ 2♦ 3♣ 4♣ 5♦'   straight        '5'
'2♥ 3♥ 2♦ 3♣ 3♦'   full-house      ['3', '2']
'2♥ 7♥ 2♦ 3♣ 3♦'   two-pair        ['3', '2', '7']
'2♥ 7♥ 7♦ 7♣ 7♠'   four-of-a-kind  ['7', '2']
'10♥ j♥ q♥ k♥ a♥'  straight-flush  'a'
'4♥ 4♠ k♠ 5♦ 10♠'  one-pair        ['4', 'k', '10', '5']
'q♣ 10♣ 7♣ 6♣ 4♣'  flush           ['q', '10', '7', '6', '4']
```



## Racket


```racket
#lang racket
(require (only-in srfi/1 car+cdr))

;;; --------------------------------------------------------------------------------------------------
;;; The analyser is first... the rest of it is prettiness surrounding strings and parsing!
;;; --------------------------------------------------------------------------------------------------
;; (cons f _) and (cons _ s) appear too frequently in patterns to not factor out
(define-match-expander F._ (λ (stx) (syntax-case stx () [(_ f) #'(cons f _)])))
(define-match-expander _.S (λ (stx) (syntax-case stx () [(_ s) #'(cons _ s)])))

;; Matches are easier when the cards are lined up by face: and I always put the cards in my hand with
;; the highest card on the left (should I be telling this?)... anyway face<? is written to leave high
;; cards on the left. There is no need to sort by suit, flushes are all-or-nothing
(define (face-sort hand)
  (sort hand (match-lambda** [(_ 'joker) #f] [('joker _) #t] [((F._ f1) (F._ f2)) (> f1 f2)])))

;; even playing poker for money, I never managed to consistently determine what effect jokers were
;; having on my hand... so I'll do an exhaustive search of what's best!
;;
;; scoring hands allows us to choose a best value for joker(s)
;; hand-names provides an order (and therefore a score) for each of the available hands
(define hand-names (list 'five-of-a-kind 'straight-flush 'four-of-a-kind 'full-house 'flush 'straight
                         'three-of-a-kind 'two-pair 'one-pair 'high-card))

(define hand-order# (for/hash ((h hand-names) (i (in-range (add1 (length hand-names)) 0 -1)))
                      (values h i)))
;; The score of a hand is (its order*15^5)+(first tiebreaker*15^4)+(2nd tiebreaker*15^3)...
;; powers of 15 because we have a maxmium face value of 14 (ace) -- even though there are 13 cards
;; in a suit.
(define (calculate-score analysis)
  (define-values (hand-name tiebreakers) (car+cdr analysis))
  (for/sum ((n (in-naturals)) (tb (cons (hash-ref hand-order# hand-name -1) tiebreakers)))
    (* tb (expt 15 (- 5 n)))))

;; score hand produces an analysis of a hand (which can then be returned to analyse-sorted-hand,
;; and a score that can be maximised by choosing the right jokers.
(define (score-hand hand . jokers) ; gives an orderable list of hands with tiebreakers
  (define analysis (analyse-sorted-hand (face-sort (append jokers hand))))
  (cons analysis (calculate-score analysis)))

;; if we have two (or more) jokers, they will be consumed by the recursive call to
;; analyse-sorted-hand score-hand
(define all-cards/promise (delay (for*/list ((f (in-range 2 15)) (s '(h d s c))) (cons f s))))

(define (best-jokered-hand cards) ; we've lost the first joker from cards  
  (define-values (best-hand _bhs)
    (for*/fold ((best-hand #f) (best-score 0))
      ((joker (in-list (force all-cards/promise)))
       (score (in-value (score-hand cards joker)))
       #:when (> (cdr score) best-score))
      (car+cdr score)))

  best-hand)

;; we can abbreviate 2/3/4/5-of-a-kind 2-pair full-house with 2 and 3
(define-match-expander F*2 (λ (stx) (syntax-case stx () [(_ f) #'(list (F._ f) (F._ f))])))
(define-match-expander F*3 (λ (stx) (syntax-case stx () [(_ f) #'(list (F._ f) (F._ f) (F._ f))])))

;; note that flush? is cheaper to calculate than straight?, so do it first when we test for
;; straight-flush
(define flush?
  (match-lambda [(and `(,(_.S s) ,(_.S s) ,(_.S s) ,(_.S s) ,(_.S s)) `(,(F._ fs) ...)) `(flush ,@fs)]
                [_ #f]))

(define straight?
  (match-lambda
    ;; '(straight 5) puts this at the bottom of the pile w.r.t the ordering of straights
    [`(,(F._ 14) ,(F._ 5) ,(F._ 4) ,(F._ 3) ,(F._ 2))                                   '(straight 5)]
    [`(,(F._ f5) ,(F._ f4) ,(F._ f3) ,(F._ f2) ,(F._ f1))
     (and (= f1 (- f5 4)) (< f1 f2 f3 f4 f5)                                       `(straight ,f5))]))

(define analyse-sorted-hand
  (match-lambda
    [(list 'joker cards ...)                                                (best-jokered-hand cards)]
    [`(,@(F*3 f) ,@(F*2 f))                                                      `(five-of-a-kind ,f)]
    ;; get "top" from the straight. a the top card of the flush when there is a (straight 5) will
    ;; be the ace ... putting it in the wrong place for the ordering.
    [(and (? flush?) (app straight? (list 'straight top _ ...)))               `(straight-flush ,top)]
    [(or `(,@(F*2 f) ,@(F*2 f) ,_) `(,_ ,@(F*2 f) ,@(F*2 f)))                    `(four-of-a-kind ,f)]
    [(or `(,@(F*3 fh) ,@(F*2 fl)) `(,@(F*2 fh) ,@(F*3 fl)))                     `(full-house ,fh, fl)]
    [(app flush? (and rv (list 'flush _ ...)))                                                     rv]
    [(app straight? (and rv (list 'straight _ ...)))                                               rv]
    ;; pairs and threes may be padded to the left, middle and right with tie-breakers; the lists of
    ;; which we will call l, m and r, respectively (four and 5-of-a-kind don't need tiebreaking,
    ;; they're well hard!)
    [`(,(F._ l) ... ,@(F*3 f) ,(F._ r) ...)                             `(three-of-a-kind ,f ,@l ,@r)]
    [`(,(F._ l) ... ,@(F*2 f1) ,(F._ m) ... ,@(F*2 f2) ,(F._ r) ...)  `(two-pair ,f1 ,f2 ,@l ,@m ,@r)]
    [`(,(F._ l) ... ,@(F*2 f) ,(F._ r) ...)                                    `(one-pair ,f ,@l ,@r)]
    [`(,(F._ f) ...)                                                                 `(high-card ,@f)]
    [hand                                                                (error 'invalid-hand hand)]))

(define (analyse-hand/string hand-string)
  (analyse-sorted-hand (face-sort (string->hand hand-string))))

;;; --------------------------------------------------------------------------------------------------
;;; Strings to cards, cards to strings -- that kind of thing
;;; --------------------------------------------------------------------------------------------------
(define suit->unicode (match-lambda ('h "♥") ('d "♦") ('c "♣") ('s "♠") (x x)))

(define unicode->suit (match-lambda ("♥" 'h) ("♦" 'd) ("♣" 'c) ("♠" 's) (x x)))

(define (face->number f)
  (match (string-upcase f)
    ["T" 10] ["J" 11] ["Q" 12] ["K" 13] ["A" 14] [(app string->number (? number? n)) n] [else 0]))

(define number->face (match-lambda (10 "T") (11 "J") (12 "Q") (13 "K") (14 "A") ((app ~s x) x)))

(define string->card
  (match-lambda
    ("joker" 'joker)
    ((regexp #px"^(.*)(.)$" (list _ (app face->number num) (app unicode->suit suit)))
     (cons num suit))))

(define (string->hand str)
  (map string->card (regexp-split #rx" +" (string-trim str))))

(define card->string
  (match-lambda ['joker "[]"]
                [(cons (app number->face f) (app suit->unicode s)) (format "~a~a" f s)]))

(define (hand->string h)
  (string-join (map card->string h) " "))

;; used for both testing and output
(define e.g.-hands
  (list " 2♥  2♦ 2♣ k♣  q♦" " 2♥  5♥ 7♦ 8♣  9♠" " a♥  2♦ 3♣ 4♣  5♦" "10♥  j♦ q♣ k♣  a♦"
        " 2♥  3♥ 2♦ 3♣  3♦" " 2♥  7♥ 2♦ 3♣  3♦" " 2♥  7♥ 7♦ 7♣  7♠" "10♥  j♥ q♥ k♥  a♥"
        " 4♥  4♠ k♠ 5♦ 10♠" " q♣ 10♣ 7♣ 6♣  4♣"
        
        " joker  2♦  2♠  k♠  q♦"     "  joker  5♥  7♦  8♠  9♦"    "  joker  2♦  3♠  4♠  5♠"
        "  joker  3♥  2♦  3♠  3♦"    "  joker  7♥  2♦  3♠  3♦"    "  joker  7♥  7♦  7♠  7♣"
        "  joker  j♥  q♥  k♥  A♥"    "  joker  4♣  k♣  5♦ 10♠"    "  joker  k♣  7♣  6♣  4♣"
        "  joker  2♦  joker  4♠  5♠" "  joker  Q♦  joker  A♠ 10♠" "  joker  Q♦  joker  A♦ 10♦"
        "  joker  2♦  2♠  joker  q♦"))

;;; --------------------------------------------------------------------------------------------------
;;; Main and test modules
;;; --------------------------------------------------------------------------------------------------
(module+ main
  (define scored-hands
    (for/list ((h (map string->hand e.g.-hands)))
      (define-values (analysis score) (car+cdr (score-hand h)))
      (list h analysis score)))
  
  (for ((a.s (sort scored-hands > #:key third)))
    (match-define (list (app hand->string h) a _) a.s)
    (printf "~a: ~a ~a" h (~a (first a) #:min-width 15) (number->face (second a)))
    (when (pair? (cddr a)) (printf " [tiebreak: ~a]" (string-join (map number->face (cddr a)) ", ")))
    (newline)))

(module+ test
  (require rackunit)  
  (let ((e.g.-strght-flsh '((14 . h) (13 . h) (12 . h) (11 . h) (10 . h))))
    (check-match (straight? e.g.-strght-flsh) '(straight 14))
    (check-match (flush? e.g.-strght-flsh) '(flush 14 13 12 11 10))
    (check-match e.g.-strght-flsh (and (? flush?) (app straight? (list 'straight top _ ...)))))
  
  (define expected-results
    '((three-of-a-kind 2 13 12)
      (high-card 9 8 7 5 2) (straight 5) (straight 14) (full-house 3 2) (two-pair 3 2 7)
      (four-of-a-kind 7) (straight-flush 14) (one-pair 4 13 10 5) (flush 12 10 7 6 4)      
      (three-of-a-kind 2 13 12) (straight 9) (straight 6) (four-of-a-kind 3) (three-of-a-kind 3 7 2)
      (five-of-a-kind 7) (straight-flush 14) (one-pair 13 10 5 4) (flush 14 13 7 6 4) (straight 6)
      (straight 14) (straight-flush 14) (four-of-a-kind 2)))
  (for ((h e.g.-hands) (r expected-results)) (check-equal? (analyse-hand/string h) r)))
```


{{out}}

```txt
[] 7♥ 7♦ 7♠ 7♣: five-of-a-kind  7
T♥ J♥ Q♥ K♥ A♥: straight-flush  A
[] J♥ Q♥ K♥ A♥: straight-flush  A
[] Q♦ [] A♦ T♦: straight-flush  A
2♥ 7♥ 7♦ 7♣ 7♠: four-of-a-kind  7
[] 3♥ 2♦ 3♠ 3♦: four-of-a-kind  3
[] 2♦ 2♠ [] Q♦: four-of-a-kind  2
2♥ 3♥ 2♦ 3♣ 3♦: full-house      3 [tiebreak: 2]
[] K♣ 7♣ 6♣ 4♣: flush           A [tiebreak: K, 7, 6, 4]
Q♣ T♣ 7♣ 6♣ 4♣: flush           Q [tiebreak: T, 7, 6, 4]
T♥ J♦ Q♣ K♣ A♦: straight        A
[] Q♦ [] A♠ T♠: straight        A
[] 5♥ 7♦ 8♠ 9♦: straight        9
[] 2♦ 3♠ 4♠ 5♠: straight        6
[] 2♦ [] 4♠ 5♠: straight        6
A♥ 2♦ 3♣ 4♣ 5♦: straight        5
[] 7♥ 2♦ 3♠ 3♦: three-of-a-kind 3 [tiebreak: 7, 2]
2♥ 2♦ 2♣ K♣ Q♦: three-of-a-kind 2 [tiebreak: K, Q]
[] 2♦ 2♠ K♠ Q♦: three-of-a-kind 2 [tiebreak: K, Q]
2♥ 7♥ 2♦ 3♣ 3♦: two-pair        3 [tiebreak: 2, 7]
[] 4♣ K♣ 5♦ T♠: one-pair        K [tiebreak: T, 5, 4]
4♥ 4♠ K♠ 5♦ T♠: one-pair        4 [tiebreak: K, T, 5]
2♥ 5♥ 7♦ 8♣ 9♠: high-card       9 [tiebreak: 8, 7, 5, 2]
```



## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 10.12.2013 Walter Pachl
*--------------------------------------------------------------------*/

d.1='2h 2d 2s ks qd'; x.1='three-of-a-kind'
d.2='2h 5h 7d 8s 9d'; x.2='high-card'
d.3='ah 2d 3s 4s 5s'; x.3='straight'
d.4='2h 3h 2d 3s 3d'; x.4='full-house'
d.5='2h 7h 2d 3s 3d'; x.5='two-pair'
d.6='2h 7h 7d 7s 7c'; x.6='four-of-a-kind'
d.7='th jh qh kh ah'; x.7='straight-flush'
d.8='4h 4c kc 5d tc'; x.8='one-pair'
d.9='qc tc 7c 6c 4c'; x.9='flush'
d.10='ah 2h 3h 4h'
d.11='ah 2h 3h 4h 5h 6h'
d.12='2h 2h 3h 4h 5h'
d.13='xh 2h 3h 4h 5h'
d.14='2x 2h 3h 4h 5h'
Do ci=1 To 14
  Call poker d.ci,x.ci
  end
Exit

poker:
Parse Arg deck,expected
have.=0
f.=0; fmax=0
s.=0; smax=0
cnt.=0
If words(deck)<5 Then Return err('less than 5 cards')
If words(deck)>5 Then Return err('more than 5 cards')
Do i=1 To 5
  c=word(deck,i)
  Parse Var c f +1 s
  If have.f.s=1 Then Return err('duplicate card:' c)
  have.f.s=1
  m=pos(f,'a23456789tjqk')
  If m=0 Then Return err('invalid face' f 'in' c)
  cnt.m=cnt.m+1
  n=pos(s,'hdcs')
  If n=0 Then Return err('invalid suit' s 'in' c)
  f.m=f.m+1; fmax=max(fmax,f.m)
  s.n=s.n+1; smax=max(smax,s.n)
  End
cntl=''
cnt.14=cnt.1
Do i=1 To 14
  cntl=cntl||cnt.i
  End
Select
  When fmax=4 Then res='four-of-a-kind'
  When fmax=3 Then Do
    If x_pair() Then
      res='full-house'
    Else
      res='three-of-a-kind'
    End
  When fmax=2 Then Do
    If x_2pair() Then
      res='two-pair'
    Else
      res='one-pair'
    End
  When smax=5 Then Do
    If x_street() Then
      res='straight-flush'
    Else
      res='flush'
    End
  When x_street() Then
    res='straight'
  Otherwise
    res='high-card'
  End
Say deck res
If res<>expected Then
  Say copies(' ',14) expected
Return

x_pair:
  Do p=1 To 13
    If f.p=2 Then return 1
    End
  Return 0

x_2pair:
  pp=0
  Do p=1 To 13
    If f.p=2 Then pp=pp+1
    End
  Return pp=2

x_street:
  Return pos('11111',cntl)>0

err:
  Say deck 'Error:' arg(1)
  Return 0
```

{{out}}

```txt
2h 2d 2s ks qd three-of-a-kind
2h 5h 7d 8s 9d high-card
ah 2d 3s 4s 5s straight
2h 3h 2d 3s 3d full-house
2h 7h 2d 3s 3d two-pair
2h 7h 7d 7s 7c four-of-a-kind
th jh qh kh ah straight-flush
4h 4c kc 5d tc one-pair
qc tc 7c 6c 4c flush
ah 2h 3h 4h Error: less than 5 cards
ah 2h 3h 4h 5h 6h Error: more than 5 cards
2h 2h 3h 4h 5h Error: duplicate card: 2h
xh 2h 3h 4h 5h Error: invalid face x in xh
2x 2h 3h 4h 5h Error: invalid suit x in 2x
```



### version 2 with suit glyphs

This REXX version supports:
::*   upper/lower/mixed case for suits and pips
::*   allows commas or blanks for card separation
::*   alternate names for aces and tens
::*   alphabetic letters for suits and/or glyphs
::*   specification of number of cards in a hand
::*   the dealt hands can be in a file   (blank lines are ignored) 
::*   dealt hands in the file can have comments after a semicolon (''';''')

```rexx
/*REXX program analyzes an  N─card  poker hand,  and displays  what  the poker hand is. */
parse arg iFID .;       if iFID=='' | iFID==","  then iFID= 'POKERHAN.DAT'
                                                 /* [↓] read  the poker hands dealt.    */
      do  while lines(iFID)\==0;      ox= linein(iFID);       if ox=''  then iterate
      say right(ox, max(30, length(ox) ) )       ' ◄─── '       analyze(ox)
      end   /*while*/                            /* [↑]  analyze/validate the poker hand*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
analyze: procedure; arg x ';',mc;      hand= translate(x, '♥♦♣♠1', "HDCSA,");    flush= 0
kinds= 0;    suit.= 0;    pairs= 0;    @.= 0;         run= copies(0, 13);        pips= run
if mc==''  then mc= 5;    n= words(hand);  if n\==mc  then  return  'invalid'
                                                 /* [↓]  PIP can be  1 or 2  characters.*/
   do j=1  for n;      _= word(hand, j)          /*obtain a card from the dealt hand.   */
   pip= left(_, length(_) - 1);  ws= right(_, 1) /*obtain the card's pip;  and the suit.*/
   if pip==10  then pip= 'T'                     /*allow an alternate form for a "TEN". */
   @._= @._ + 1                                  /*bump the card counter for this hand. */
   #= pos(pip, 123456789TJQK)                    /*obtain the pip index for the card.   */
   if pos(ws, "♥♣♦♠")==0  then return 'invalid suit in card:'     _
   if #==0                then return 'invalid pip in card:'      _
   if @._\==1             then return 'invalid, duplicate card:'  _
   suit.ws= suit.ws + 1                          /*count the suits for a possible flush.*/
     flush= max(flush, suit.ws)                  /*count number of cards in the suits.  */
       run= overlay(., run, #)                   /*convert runs to a series of periods. */
         _= substr(pips, #, 1) + 1               /*obtain the number of the pip in hand.*/
      pips= overlay(_, pips, #)                  /*convert the pip to legitimate number.*/
     kinds= max(kinds, _)                        /*convert certain pips to their number.*/
   end   /*i*/                                   /* [↑]  keep track of  N─of─a─kind.    */

     run= run || left(run, 1)                    /*An  ace  can be  high  ─or─  low.    */
   pairs= countstr(2, pips)                      /*count number of pairs  (2s  in PIPS).*/
straight= pos(....., run || left(run, 1) ) \== 0 /*does the  RUN  contains a straight?  */
if flush==5 & straight  then return  'straight-flush'
if kinds==4             then return  'four-of-a-kind'
if kinds==3 & pairs==1  then return  'full-house'
if flush==5             then return  'flush'
if            straight  then return  'straight'
if kinds==3             then return  'three-of-a-kind'
if kinds==2 & pairs==2  then return  'two-pair'
if kinds==2             then return  'one-pair'
                             return  'high-card'
```

Programming note: some older REXXes don't have the '''countstr''' BIF, so that REXX statement (above, line '''48''') can be replaced with:

```rexx
pairs= 13 - length( space( translate( pips, , 2), 0) )   /*count # of  2's  in PIPS.*/
```


{{out|input|text=  file:}}

```txt

  2♥  2♦  2♠  k♠  q♦
  2♥  5♥  7♦  8♠  9♦
  a♥  2♦  3♠  4♠  5♠
  2♥  3♥  2♦  3♠  3♦
  2♥  7♥  2♦  3♠  3♦
  2♥  7♥  7♦  7♠  7♣
 10♥  j♥  q♥  k♥  A♥
  4♥  4♣  k♣  5♦ 10♠
  q♣  t♣  7♣  6♣  4♣
  J♥  Q♦  K♠  A♠ 10♠

  ah  2h  3h  4h

```
 
{{out|output|text=  when using the (above) input file}}

```txt

            2♥  2♦  2♠  k♠  q♦  ◄───  three-of-a-kind
            2♥  5♥  7♦  8♠  9♦  ◄───  high-card
            a♥  2♦  3♠  4♠  5♠  ◄───  straight
            2♥  3♥  2♦  3♠  3♦  ◄───  full-house
            2♥  7♥  2♦  3♠  3♦  ◄───  two-pair
            2♥  7♥  7♦  7♠  7♣  ◄───  four-of-a-kind
           10♥  j♥  q♥  k♥  A♥  ◄───  straight-flush
            4♥  4♣  k♣  5♦ 10♠  ◄───  one-pair
            q♣  t♣  7♣  6♣  4♣  ◄───  flush
            J♥  Q♦  K♠  A♠ 10♠  ◄───  straight
                ah  2h  3h  4h  ◄───  invalid

```



### version 3 with suit glyphs and jokers

This REXX version has three additional features:
::*   "invalid" hands have additional diagnostic information
::*   supports up to two ''jokers''
::*   the ''joker'' card may be abbreviated (and can be in upper/lower/mixed case)

```rexx
/*REXX program analyzes an  N-card  poker hand, and displays what the poker hand is,    */
/*──────────────────────────────────────────── poker hands may contain up to two jokers.*/
parse arg iFID .;       if iFID=='' | iFID==","  then iFID= 'POKERHAJ.DAT'
                                                 /* [↓] read  the poker hands dealt.    */
      do  while lines(iFID)\==0;      ox= linein(iFID);         if ox=''  then iterate
      say right(ox, max(30, length(ox) ) )       ' ◄─── '       analyze(ox)
      end   /*while*/                            /* [↑]  analyze/validate the poker hand*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
analyze: procedure; arg x ';',mc;       hand=translate(x, '♥♦♣♠1', "HDCSA,");    flush= 0
kinds= 0;    suit.= 0;    pairs= 0;     @.= 0;         run= copies(0 ,13);       pips= run
if mc==''  then mc= 5;    n= words(hand)         /*N   is the number of cards in hand.  */
if n\==mc  then return  'invalid number of cards, must be' mc
                                                 /* [↓]  the PIP can be  1 or 2  chars. */
   do j=1  for n;     _= word(hand, j)           /*obtain a card from the dealt hand.   */
   pip= left(_, length(_) - 1);  ws= right(_, 1) /*obtain card's pip; obtain card's suit*/
   if pip==10   then pip= 'T'                    /*allow alternate form for a  TEN  pip.*/
   if abbrev('JOKER', _, 1)  then _= "JK"        /*allow altername forms of JOKER names.*/
   @._= @._ + 1                                  /*bump the card counter for this hand. */
   #= pos(pip, 123456789TJQK)                    /*obtain the pip index for the card.   */
   if _=='JK'  then do;  if @.j>2  then return 'invalid, too many jokers'
                         iterate
                    end
   if pos(ws, "♥♣♦♠")==0  then return 'invalid suit in card:'     _
   if #==0                then return 'invalid pip in card:'      _
   if @._\==1             then return 'invalid, duplicate card:'  _
   suit.ws= suit.ws + 1                          /*count the suits for a possible flush.*/
     flush= max(flush, suit.ws)                  /*count number of cards in the suits.  */
       run= overlay(., run, #)                   /*convert runs to a series of periods. */
         _= substr(pips, #, 1) + 1               /*obtain the number of the pip in hand.*/
      pips= overlay(_, pips, #)                  /*convert the pip to legitimate number.*/
     kinds= max(kinds, _)                        /*convert certain pips to their number.*/
   end   /*i*/                                   /* [↑]  keep track of  N─of─a─kind.    */

run= run || left(run, 1)                         /*An  ace  can be  high  ─or─  low.    */
jok= @.jk;   kinds= kinds+jok;  flush= flush+jok /*N─of─a─kind;  adjustments for jokers.*/
straight= pos(..... , run)\==0           |,      /*does the RUN contain a straight?     */
         (pos(....  , run)\==0 & jok>=1) |,      /*  "   "   "     "    "     "         */
         (pos(..0.. , run)\==0 & jok>=1) |,      /*  "   "   "     "    "     "         */
         (pos(...0. , run)\==0 & jok>=1) |,      /*  "   "   "     "    "     "         */
         (pos(.0... , run)\==0 & jok>=1) |,      /*  "   "   "     "    "     "         */
         (pos(...   , run)\==0 & jok>=2) |,      /*  "   "   "     "    "     "         */
         (pos(..0.  , run)\==0 & jok>=2) |,      /*  "   "   "     "    "     "         */
         (pos(.0..  , run)\==0 & jok>=2) |,      /*  "   "   "     "    "     "         */
         (pos(.00.. , run)\==0 & jok>=2) |,      /*  "   "   "     "    "     "         */
         (pos(..00. , run)\==0 & jok>=2) |,      /*  "   "   "     "    "     "         */
         (pos(.0.0. , run)\==0 & jok>=2)         /*  "   "   "     "    "     "         */
pairs= countstr(2, pips)                         /*count number of pairs  (2s in PIPS). */
if jok\==0  then pairs= pairs - 1                /*adjust number of pairs with jokers.  */
if kinds>=5             then return  'five-of-a-kind'
if flush>=5 & straight  then return  'straight-flush'
if kinds>=4             then return  'four-of-a-kind'
if kinds>=3 & pairs>=1  then return  'full-house'
if flush>=5             then return  'flush'
if            straight  then return  'straight'
if kinds>=3             then return  'three-of-a-kind'
if kinds==2 & pairs==2  then return  'two-pair'
if kinds==2             then return  'one-pair'
                             return  'high-card'
```

Programming note:   the method used for analyzing hands that contain jokers are limited to a maximum of two jokers. 

A different methodology would be needed for a generic number of jokers (and/or wild cards [such as deuces and one─eyed jacks]).

{{out|input|text=  file:}}

```txt

   joker  2♦  2♠  k♠  q♦
   joker  5♥  7♦  8♠  9♦
   joker  2♦  3♠  4♠  5♠
   joker  3♥  2♦  3♠  3♦
   joker  7♥  2♦  3♠  3♦
   joker  7♥  7♦  7♠  7♣
   joker  j♥  q♥  k♥  A♥
   joker  4♣  k♣  5♦ 10♠
   joker  t♣  7♣  6♣  4♣
   joker  Q♦  K♠  A♠ 10♠

   joker  2h  3h  4h

      2♥  2♦  2♠  k♠  jok
      2♥  5♥  7♦  8♠  jok
      a♥  2♦  5♠  4♠  jok
      2♥  3♥  2♦  3♠  jok
      2♥  7♥  2♦  3♠  jok
      2♥  7♥  7♦  7♠  jok
     10♥  j♥  q♥  k♥  jok
      4♥  4♣  k♣  5♦  jok
      q♣  t♣  7♣  6♣  jok
      J♥  Q♦  K♠  A♠  jok 

```
 
{{out|output|text=  when using the (above) input file}}

```txt

         joker  2♦  2♠  k♠  q♦  ◄───  three-of-a-kind
         joker  5♥  7♦  8♠  9♦  ◄───  straight
         joker  2♦  3♠  4♠  5♠  ◄───  straight
         joker  3♥  2♦  3♠  3♦  ◄───  four-of-a-kind
         joker  7♥  2♦  3♠  3♦  ◄───  three-of-a-kind
         joker  7♥  7♦  7♠  7♣  ◄───  five-of-a-kind
         joker  j♥  q♥  k♥  A♥  ◄───  straight-flush
         joker  4♣  k♣  5♦ 10♠  ◄───  one-pair
         joker  t♣  7♣  6♣  4♣  ◄───  flush
         joker  Q♦  K♠  A♠ 10♠  ◄───  straight
             joker  2h  3h  4h  ◄───  invalid number of cards, must be 5
           2♥  2♦  2♠  k♠  jok  ◄───  four-of-a-kind
           2♥  5♥  7♦  8♠  jok  ◄───  one-pair
           a♥  2♦  5♠  4♠  jok  ◄───  straight
           2♥  3♥  2♦  3♠  jok  ◄───  full-house
           2♥  7♥  2♦  3♠  jok  ◄───  three-of-a-kind
           2♥  7♥  7♦  7♠  jok  ◄───  four-of-a-kind
          10♥  j♥  q♥  k♥  jok  ◄───  straight-flush
           4♥  4♣  k♣  5♦  jok  ◄───  three-of-a-kind
           q♣  t♣  7♣  6♣  jok  ◄───  flush
           J♥  Q♦  K♠  A♠  jok  ◄───  straight 

```



## Ruby

Joker-less hands are sorted high to low.

```ruby
class Card
  include Comparable
  attr_accessor :ordinal
  attr_reader :suit, :face 
  
  SUITS = %i(♥ ♦ ♣ ♠)
  FACES = %i(2 3 4 5 6 7 8 9 10 j q k a)
  
  def initialize(str)
    @face, @suit = parse(str)
    @ordinal = FACES.index(@face)
  end
  
  def <=> (other) #used for sorting
    self.ordinal <=> other.ordinal
  end
  
  def to_s
    "#@face#@suit"
  end
  
  private
  def parse(str)
    face, suit = str.chop.to_sym, str[-1].to_sym
    raise ArgumentError, "invalid card: #{str}" unless FACES.include?(face) && SUITS.include?(suit)
    [face, suit]
  end
end

class Hand
  include Comparable
  attr_reader :cards, :rank
  
  RANKS       = %i(high-card one-pair two-pair three-of-a-kind straight flush
                   full-house four-of-a-kind straight-flush five-of-a-kind)
  WHEEL_FACES = %i(2 3 4 5 a)
  
  def initialize(str_of_cards)
    @cards = str_of_cards.downcase.tr(',',' ').split.map{|str| Card.new(str)}
    grouped = @cards.group_by(&:face).values
    @face_pattern = grouped.map(&:size).sort
    @rank = categorize
    @rank_num = RANKS.index(@rank)
    @tiebreaker = grouped.map{|ar| [ar.size, ar.first.ordinal]}.sort.reverse
  end
  
  def <=> (other)    # used for sorting and comparing
    self.compare_value <=> other.compare_value
  end
  
  def to_s
    @cards.map(&:to_s).join(" ")
  end
  
  protected          # accessible for Hands
  def compare_value
    [@rank_num, @tiebreaker]
  end
  
  private
  def one_suit?
    @cards.map(&:suit).uniq.size == 1
  end
  
  def consecutive?
    sort.each_cons(2).all? {|c1,c2| c2.ordinal - c1.ordinal == 1 }
  end
  
  def sort
    if @cards.sort.map(&:face) == WHEEL_FACES
      @cards.detect {|c| c.face == :a}.ordinal = -1
    end 
    @cards.sort
  end
  
  def categorize
    if consecutive?
      one_suit? ? :'straight-flush' : :straight
    elsif one_suit?
      :flush
    else
      case @face_pattern
        when [1,1,1,1,1] then :'high-card'
        when [1,1,1,2]   then :'one-pair'
        when [1,2,2]     then :'two-pair'
        when [1,1,3]     then :'three-of-a-kind'
        when [2,3]       then :'full-house'
        when [1,4]       then :'four-of-a-kind'
        when [5]         then :'five-of-a-kind'
      end
    end
  end
end

# Demo
test_hands = <<EOS
2♥ 2♦ 2♣ k♣ q♦
2♥ 5♥ 7♦ 8♣ 9♠
a♥ 2♦ 3♣ 4♣ 5♦
2♥ 3♥ 2♦ 3♣ 3♦
2♥ 7♥ 2♦ 3♣ 3♦
2♥ 6♥ 2♦ 3♣ 3♦
10♥ j♥ q♥ k♥ a♥
4♥ 4♠ k♠ 2♦ 10♠
4♥ 4♠ k♠ 3♦ 10♠
q♣ 10♣ 7♣ 6♣ 4♣
q♣ 10♣ 7♣ 6♣ 3♣
9♥ 10♥ q♥ k♥ j♣
2♥ 3♥ 4♥ 5♥ a♥
2♥ 2♥ 2♦ 3♣ 3♦
EOS

hands = test_hands.each_line.map{|line| Hand.new(line) }
puts "High to low"
hands.sort.reverse.each{|hand| puts "#{hand}\t #{hand.rank}" }
puts

str = <<EOS
joker  2♦  2♠  k♠  q♦
joker  5♥  7♦  8♠  9♦
joker  2♦  3♠  4♠  5♠
joker  3♥  2♦  3♠  3♦
joker  7♥  2♦  3♠  3♦
joker  7♥  7♦  7♠  7♣
joker  j♥  q♥  k♥  A♥
joker  4♣  k♣  5♦ 10♠
joker  k♣  7♣  6♣  4♣
joker  2♦  joker  4♠  5♠
joker  Q♦  joker  A♠ 10♠
joker  Q♦  joker  A♦ 10♦
joker  2♦  2♠  joker  q♦
EOS

# Neither the Card nor the Hand class supports jokers
# but since hands are comparable, they are also sortable.
# Try every card from a deck for a joker and pick the largest hand:

DECK = Card::FACES.product(Card::SUITS).map(&:join)
str.each_line do |line|
  cards_in_arrays = line.split.map{|c| c == "joker" ? DECK.dup : [c]} #joker is array of all cards
  all_tries  = cards_in_arrays.shift.product(*cards_in_arrays).map{|ar| Hand.new(ar.join" ")} #calculate the Whatshisname product
  best = all_tries.max
  puts "#{line.strip}: #{best.rank}"
end
```


{{out}}

```txt

High to low
10♥ j♥ q♥ k♥ a♥	 straight-flush
2♥ 3♥ 4♥ 5♥ a♥	 straight-flush
2♥ 3♥ 2♦ 3♣ 3♦	 full-house
2♥ 2♥ 2♦ 3♣ 3♦	 full-house
q♣ 10♣ 7♣ 6♣ 4♣	 flush
q♣ 10♣ 7♣ 6♣ 3♣	 flush
9♥ 10♥ q♥ k♥ j♣	 straight
a♥ 2♦ 3♣ 4♣ 5♦	 straight
2♥ 2♦ 2♣ k♣ q♦	 three-of-a-kind
2♥ 7♥ 2♦ 3♣ 3♦	 two-pair
2♥ 6♥ 2♦ 3♣ 3♦	 two-pair
4♥ 4♠ k♠ 3♦ 10♠	 one-pair
4♥ 4♠ k♠ 2♦ 10♠	 one-pair
2♥ 5♥ 7♦ 8♣ 9♠	 high-card

joker  2♦  2♠  k♠  q♦: three-of-a-kind
joker  5♥  7♦  8♠  9♦: straight
joker  2♦  3♠  4♠  5♠: straight
joker  3♥  2♦  3♠  3♦: four-of-a-kind
joker  7♥  2♦  3♠  3♦: three-of-a-kind
joker  7♥  7♦  7♠  7♣: five-of-a-kind
joker  j♥  q♥  k♥  A♥: straight-flush
joker  4♣  k♣  5♦ 10♠: one-pair
joker  k♣  7♣  6♣  4♣: flush
joker  2♦  joker  4♠  5♠: straight
joker  Q♦  joker  A♠ 10♠: straight
joker  Q♦  joker  A♦ 10♦: straight-flush
joker  2♦  2♠  joker  q♦: four-of-a-kind

```



## Scala

Including jokers, but not special suit characters. Aiming for readability more than performance.

```scala
val faces = "23456789TJQKA"
val suits = "CHSD"
sealed trait Card
object Joker extends Card
case class RealCard(face: Int, suit: Char) extends Card
val allRealCards = for {
  face <- 0 until faces.size
  suit <- suits
} yield RealCard(face, suit)

def parseCard(str: String): Card = {
  if (str == "joker") {
    Joker
  } else {
    RealCard(faces.indexOf(str(0)), str(1))
  }
}

def parseHand(str: String): List[Card] = {
  str.split(" ").map(parseCard).toList
}

trait HandType {
  def name: String
  def check(hand: List[RealCard]): Boolean
}

case class And(x: HandType, y: HandType, name: String) extends HandType {
  def check(hand: List[RealCard]) = x.check(hand) && y.check(hand)
}

object Straight extends HandType {
  val name = "straight"
  def check(hand: List[RealCard]): Boolean = {
    val faces = hand.map(_.face).toSet
    faces.size == 5 && (faces.min == faces.max - 4 || faces == Set(0, 1, 2, 3, 12))
  }
}

object Flush extends HandType {
  val name = "flush"
  def check(hand: List[RealCard]): Boolean = {
    hand.map(_.suit).toSet.size == 1
  }
}

case class NOfAKind(n: Int, name: String = "", nOccur: Int = 1) extends HandType {
  def check(hand: List[RealCard]): Boolean = {
    hand.groupBy(_.face).values.count(_.size == n) >= nOccur
  }
}

val allHandTypes = List(
  NOfAKind(5, "five-of-a-kind"),
  And(Straight, Flush, "straight-flush"),
  NOfAKind(4, "four-of-a-kind"),
  And(NOfAKind(3), NOfAKind(2), "full-house"),
  Flush,
  Straight,
  NOfAKind(3, "three-of-a-kind"),
  NOfAKind(2, "two-pair", 2),
  NOfAKind(2, "one-pair")
)

def possibleRealHands(hand: List[Card]): List[List[RealCard]] = {
  val realCards = hand.collect { case r: RealCard => r }
  val nJokers = hand.count(_ == Joker)
  allRealCards.toList.combinations(nJokers).map(_ ++ realCards).toList
}

def analyzeHand(hand: List[Card]): String = {
  val possibleHands = possibleRealHands(hand)
  allHandTypes.find(t => possibleHands.exists(t.check)).map(_.name).getOrElse("high-card")
}
```



```scala
val testHands = List(
  "2H 2D 2S KS QD",
  "2H 5H 7D 8S 9D",
  "AH 2D 3S 4S 5S",
  "2H 3H 2D 3S 3D",
  "2H 7H 2D 3S 3D",
  "2H 7H 7D 7S 7C",
  "TH JH QH KH AH",
  "4H 4C KC 5D TC",
  "QC TC 7C 6C 4C",
  "QC TC 7C 7C TD",
  "2H 2D 2S KS joker",
  "2H 5H 7D 8S joker",
  "AH 2D 3S 4S joker",
  "2H 3H 2D 3S joker",
  "2H 7H 2D 3S joker",
  "2H 7H 7D joker joker",
  "TH JH QH joker joker",
  "4H 4C KC joker joker",
  "QC TC 7C joker joker",
  "QC TC 7H joker joker"
)

for (hand <- testHands) {
  println(s"$hand -> ${analyzeHand(parseHand(hand))}")
}
```


{{out}}


```txt
2H 2D 2S KS QD -> three-of-a-kind
2H 5H 7D 8S 9D -> high-card
AH 2D 3S 4S 5S -> straight
2H 3H 2D 3S 3D -> full-house
2H 7H 2D 3S 3D -> two-pair
2H 7H 7D 7S 7C -> four-of-a-kind
TH JH QH KH AH -> straight-flush
4H 4C KC 5D TC -> one-pair
QC TC 7C 6C 4C -> flush
QC TC 7C 7C TD -> two-pair
2H 2D 2S KS joker -> four-of-a-kind
2H 5H 7D 8S joker -> one-pair
AH 2D 3S 4S joker -> straight
2H 3H 2D 3S joker -> full-house
2H 7H 2D 3S joker -> three-of-a-kind
2H 7H 7D joker joker -> four-of-a-kind
TH JH QH joker joker -> straight-flush
4H 4C KC joker joker -> four-of-a-kind
QC TC 7C joker joker -> flush
QC TC 7H joker joker -> three-of-a-kind
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "console.s7i";

const string: face is "A23456789TJQK";
const string: suit is "♥♦♣♠";

const func string: analyzeHand (in array integer: faceCnt, in array integer: suitCnt) is func
  result
    var string: handValue is "";
  local
    var boolean: pair1 is FALSE;
    var boolean: pair2 is FALSE;
    var boolean: three is FALSE;
    var boolean: four is FALSE;
    var boolean: flush is FALSE;
    var boolean: straight is FALSE;
    var integer: sequence is 0;
    var integer: x is 0;
  begin
    for x range 1 to 13 do
      case faceCnt[x] of
        when {2}: if pair1 then pair2 := TRUE; else pair1 := TRUE; end if;
        when {3}: three := TRUE;
        when {4}: four := TRUE;
      end case;
    end for;
    for x range 1 to 4 until flush do
      if suitCnt[x] = 5 then
        flush := TRUE;
      end if;
    end for;
    if not pair1 and not three and not four then
      for x range 1 to 13 until sequence = 5 do
        if faceCnt[x] <> 0 then incr(sequence); else sequence := 0; end if;
      end for;
      straight := sequence = 5 or (sequence = 4 and faceCnt[1] <> 0);
    end if;
    if straight and flush then handValue := "straight-flush";
    elsif four            then handValue := "four-of-a-kind"; 
    elsif pair1 and three then handValue := "full-house";
    elsif flush           then handValue := "flush";
    elsif straight        then handValue := "straight";
    elsif three           then handValue := "three-of-a-kind";
    elsif pair1 and pair2 then handValue := "two-pair";
    elsif pair1           then handValue := "one-pair";
    else                       handValue := "high-card";
    end if;
  end func;
 
const proc: analyze (in string: cards) is func
  local
    var array integer: faceCnt is 13 times 0;
    var array integer: suitCnt is 4 times 0;
    var string: card is "";
  begin
    for card range split(upper(cards), ' ') do
      incr(faceCnt[pos(face, card[1])]);
      incr(suitCnt[pos(suit, card[2])]);
    end for;
    writeln(cards <& ": " <& analyzeHand(faceCnt, suitCnt));
  end func;

const proc: main is func
  begin
    OUT := STD_CONSOLE;
    analyze("2♥ 2♦ 2♠ k♠ q♦");
    analyze("2♥ 5♥ 7♦ 8♠ 9♦");
    analyze("a♥ 2♦ 3♠ 4♠ 5♠");
    analyze("2♥ 3♥ 2♦ 3♠ 3♦");
    analyze("2♥ 7♥ 2♦ 3♠ 3♦");
    analyze("2♥ 7♥ 7♦ 7♠ 7♣");
    analyze("t♥ j♥ q♥ k♥ a♥");
    analyze("4♥ 4♣ k♣ 5♦ t♣");
    analyze("q♣ t♣ 7♣ 6♣ 4♣");
  end func;
```


{{out}}

```txt

2♥ 2♦ 2♠ k♠ q♦: three-of-a-kind
2♥ 5♥ 7♦ 8♠ 9♦: high-card
a♥ 2♦ 3♠ 4♠ 5♠: straight
2♥ 3♥ 2♦ 3♠ 3♦: full-house
2♥ 7♥ 2♦ 3♠ 3♦: two-pair
2♥ 7♥ 7♦ 7♠ 7♣: four-of-a-kind
t♥ j♥ q♥ k♥ a♥: straight-flush
4♥ 4♣ k♣ 5♦ t♣: one-pair
q♣ t♣ 7♣ 6♣ 4♣: flush

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6
namespace eval PokerHandAnalyser {
    proc analyse {hand} {
	set norm [Normalise $hand]
	foreach type {
	    invalid straight-flush four-of-a-kind full-house flush straight
	    three-of-a-kind two-pair one-pair
	} {
	    if {[Detect-$type $norm]} {
		return $type
	    }
	}
	# Always possible to use high-card if the hand is legal at all
	return high-card
    }

    # This normalises to an internal representation that is a list of pairs,
    # where each pair is one number for the pips (ace == 14, king == 13,
    # etc.) and another for the suit. This greatly simplifies detection.
    proc Normalise {hand} {
	set PipMap {j 11 q 12 k 13 a 14}
	set SuitMap {♥ 2 h 2 ♦ 1 d 1 ♣ 0 c 0 ♠ 3 s 3}
	set hand [string tolower $hand]
	set cards [regexp -all -inline {(?:[akqj98765432]|10)[hdcs♥♦♣♠]} $hand]
	lsort -command CompareCards [lmap c [string map {} $cards] {
	    list [string map $PipMap [string range $c 0 end-1]] \
		    [string map $SuitMap [string index $c end]]
	}]
    }
    proc CompareCards {a b} {
	lassign $a pipA suitA
	lassign $b pipB suitB
	expr {$pipA==$pipB ? $suitB-$suitA : $pipB-$pipA}
    }

    # Detection code. Note that the detectors all assume that the preceding
    # detectors have been run first; this simplifies the logic a lot, but does
    # mean that the individual detectors are not robust on their own.
    proc Detect-invalid {hand} {
	if {[llength $hand] != 5} {return 1}
	foreach c $hand {
	    if {[incr seen($c)] > 1} {return 1}
	}
	return 0
    }
    proc Detect-straight-flush {hand} {
	foreach c $hand {
	    lassign $c pip suit
	    if {[info exist prev] && $prev-1 != $pip} {
		# Special case: ace low straight flush ("steel wheel")
		if {$prev != 14 && $suit != 5} {
		    return 0
		}
	    }
	    set prev $pip
	    incr seen($suit)
	}
	return [expr {[array size seen] == 1}]
    }
    proc Detect-four-of-a-kind {hand} {
	foreach c $hand {
	    lassign $c pip suit
	    if {[incr seen($pip)] > 3} {return 1}
	}
	return 0
    }
    proc Detect-full-house {hand} {
	foreach c $hand {
	    lassign $c pip suit
	    incr seen($pip)
	}
	return [expr {[array size seen] == 2}]
    }
    proc Detect-flush {hand} {
	foreach c $hand {
	    lassign $c pip suit
	    incr seen($suit)
	}
	return [expr {[array size seen] == 1}]
    }
    proc Detect-straight {hand} {
	foreach c $hand {
	    lassign $c pip suit
	    if {[info exist prev] && $prev-1 != $pip} {
		# Special case: ace low straight ("wheel")
		if {$prev != 14 && $suit != 5} {
		    return 0
		}
	    }
	    set prev $pip
	}
	return 1
    }
    proc Detect-three-of-a-kind {hand} {
	foreach c $hand {
	    lassign $c pip suit
	    if {[incr seen($pip)] > 2} {return 1}
	}
	return 0
    }
    proc Detect-two-pair {hand} {
	set pairs 0
	foreach c $hand {
	    lassign $c pip suit
	    if {[incr seen($pip)] > 1} {incr pairs}
	}
	return [expr {$pairs > 1}]
    }
    proc Detect-one-pair {hand} {
	foreach c $hand {
	    lassign $c pip suit
	    if {[incr seen($pip)] > 1} {return 1}
	}
	return 0
    }
}
```

Demonstrating:

```tcl
foreach hand {
   "2♥ 2♦ 2♣ k♣ q♦" "2♥ 5♥ 7♦ 8♣ 9♠" "a♥ 2♦ 3♣ 4♣ 5♦" "2♥ 3♥ 2♦ 3♣ 3♦"
   "2♥ 7♥ 2♦ 3♣ 3♦" "2♥ 7♥ 7♦ 7♣ 7♠" "10♥ j♥ q♥ k♥ a♥" "4♥ 4♠ k♠ 5♦ 10♠"
   "q♣ 10♣ 7♣ 6♣ 4♣"
} {
    puts "${hand}: [PokerHandAnalyser::analyse $hand]"
}
```

{{out}}

```txt

2♥ 2♦ 2♣ k♣ q♦: three-of-a-kind
2♥ 5♥ 7♦ 8♣ 9♠: high-card
a♥ 2♦ 3♣ 4♣ 5♦: straight
2♥ 3♥ 2♦ 3♣ 3♦: full-house
2♥ 7♥ 2♦ 3♣ 3♦: two-pair
2♥ 7♥ 7♦ 7♣ 7♠: four-of-a-kind
10♥ j♥ q♥ k♥ a♥: straight-flush
4♥ 4♠ k♠ 5♦ 10♠: one-pair
q♣ 10♣ 7♣ 6♣ 4♣: flush

```


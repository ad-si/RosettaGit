+++
title = "Perfect shuffle"
description = ""
date = 2019-07-17T13:25:18Z
aliases = []
[extra]
id = 19016
[taxonomies]
categories = ["task"]
tags = []
+++

A perfect shuffle (or [https://en.wikipedia.org/wiki/Faro_shuffle faro/weave shuffle]) means splitting a deck of cards into equal halves, and perfectly interleaving them - so that you end up with the first card from the left half, followed by the first card from the right half, and so on:

<big>
<!-- START OF DIAGRAM -->
::: <div style="display:inline-block;margin:0.5em 1.5em"><tt><span style="background:#8DF">7♠</span> <span style="background:#8DF">8♠</span> <span style="background:#8DF">9♠</span> <span style="background:#FB5">J♠</span> <span style="background:#FB5">Q♠</span> <span style="background:#FB5">K♠</span></tt></div><div style="display:inline-block">&rarr;<div style="display:inline-block;vertical-align:middle;margin:0.5em 1.5em"><tt><span style="background:#8DF">7♠</span>  <span style="background:#8DF">8♠</span>  <span style="background:#8DF">9♠</span></tt>
<tt>  <span style="background:#FB5">J♠</span>  <span style="background:#FB5">Q♠</span>  <span style="background:#FB5">K♠</span></tt></div></div><div style="display:inline-block">&rarr;<div style="display:inline-block;vertical-align:middle;margin:0.5em 1.5em"><tt><span style="background:#8DF">7♠</span> <span style="background:#FB5">J♠</span> <span style="background:#8DF">8♠</span> <span style="background:#FB5">Q♠</span> <span style="background:#8DF">9♠</span> <span style="background:#Fb5">K♠</span></tt></div></div>
<!-- END OF DIAGRAM -->
</big>

When you repeatedly perform perfect shuffles on an even-sized deck of unique cards, it will at some point arrive back at its original order. How many shuffles this takes, depends solely on the number of cards in the deck - for example for a deck of eight cards it takes three shuffles:

<big>
<!-- START OF DIAGRAM -->
::::: {| style="border-spacing:0.5em 0;border-collapse:separate;margin:0 1em;text-align:right"
|-
| <small>''original:''</small> ||
<tt style="background:#122CF8;color:#B8C0FD;padding:0 0.3em">1</tt>
<tt style="background:#332AD7;color:#C2C0F3;padding:0 0.3em">2</tt>
<tt style="background:#5428B7;color:#CCBFEA;padding:0 0.3em">3</tt>
<tt style="background:#752696;color:#D6BEE0;padding:0 0.3em">4</tt>
<tt style="background:#962576;color:#E0BED6;padding:0 0.3em">5</tt>
<tt style="background:#B72355;color:#EABDCC;padding:0 0.3em">6</tt>
<tt style="background:#D82135;color:#F3BDC3;padding:0 0.3em">7</tt>
<tt style="background:#F92015;color:#FDBDB9;padding:0 0.3em">8</tt>
|-
| <small>''after 1st shuffle:''</small> ||
<tt style="background:#122CF8;color:#B8C0FD;padding:0 0.3em">1</tt>
<tt style="background:#962576;color:#E0BED6;padding:0 0.3em">5</tt>
<tt style="background:#332AD7;color:#C2C0F3;padding:0 0.3em">2</tt>
<tt style="background:#B72355;color:#EABDCC;padding:0 0.3em">6</tt>
<tt style="background:#5428B7;color:#CCBFEA;padding:0 0.3em">3</tt>
<tt style="background:#D82135;color:#F3BDC3;padding:0 0.3em">7</tt>
<tt style="background:#752696;color:#D6BEE0;padding:0 0.3em">4</tt>
<tt style="background:#F92015;color:#FDBDB9;padding:0 0.3em">8</tt>
|-
| <small>''after 2nd shuffle:''</small> ||
<tt style="background:#122CF8;color:#B8C0FD;padding:0 0.3em">1</tt>
<tt style="background:#5428B7;color:#CCBFEA;padding:0 0.3em">3</tt>
<tt style="background:#962576;color:#E0BED6;padding:0 0.3em">5</tt>
<tt style="background:#D82135;color:#F3BDC3;padding:0 0.3em">7</tt>
<tt style="background:#332AD7;color:#C2C0F3;padding:0 0.3em">2</tt>
<tt style="background:#752696;color:#D6BEE0;padding:0 0.3em">4</tt>
<tt style="background:#B72355;color:#EABDCC;padding:0 0.3em">6</tt>
<tt style="background:#F92015;color:#FDBDB9;padding:0 0.3em">8</tt>
|-
| <small>''after 3rd shuffle:''</small> ||
<tt style="background:#122CF8;color:#B8C0FD;padding:0 0.3em">1</tt>
<tt style="background:#332AD7;color:#C2C0F3;padding:0 0.3em">2</tt>
<tt style="background:#5428B7;color:#CCBFEA;padding:0 0.3em">3</tt>
<tt style="background:#752696;color:#D6BEE0;padding:0 0.3em">4</tt>
<tt style="background:#962576;color:#E0BED6;padding:0 0.3em">5</tt>
<tt style="background:#B72355;color:#EABDCC;padding:0 0.3em">6</tt>
<tt style="background:#D82135;color:#F3BDC3;padding:0 0.3em">7</tt>
<tt style="background:#F92015;color:#FDBDB9;padding:0 0.3em">8</tt>
|}
<!-- END OF DIAGRAM -->
</big>

<p style="font-size:115%; margin:1em 0 0.5em 0">'''''The Task'''''</p>

# Write a function that can perform a perfect shuffle on an even-sized list of values.
# Call this function repeatedly to count how many shuffles are needed to get a deck back to its original order, for each of the deck sizes listed under "Test Cases" below.
#* <small>You can use a list of numbers (or anything else that's convenient) to represent a deck; just make sure that all "cards" are unique within each deck.</small>
#* <small>Print out the resulting shuffle counts, to demonstrate that your program passes the test-cases.</small>

<p style="font-size:115%; margin:1em 0 0.5em 0">'''''Test Cases'''''</p>

::::: {| class="wikitable"
|-
! input ''(deck size)'' !! output ''(number of shuffles required)''
|-
| 8 || 3
|-
| 24 || 11
|-
| 52 || 8
|-
| 100 || 30
|-
| 1020 || 1018
|-
| 1024 || 10
|-
| 10000 || 300
|}





## Ada


```ada
with ada.text_io;use ada.text_io;

procedure perfect_shuffle is
  function count_shuffle (half_size : Positive) return Positive is
    subtype index is Natural range 0..2 * half_size - 1;
    subtype index_that_move is index range index'first+1..index'last-1;
    type deck is array (index) of index;
    initial, d, next : deck;
    count : Natural := 1;
  begin
    for i in index loop initial (i) := i; end loop;
    d := initial;
    loop
      for i in index_that_move loop
        next (i) := (if d (i) mod 2 = 0 then d(i)/2 else d(i)/2 + half_size);
      end loop;
      exit when next (index_that_move)= initial(index_that_move);
      d := next;
      count := count + 1;
    end loop;
    return count;
  end count_shuffle;
  test : array (Positive range <>) of Positive := (8, 24, 52, 100, 1020, 1024, 10_000);
begin
  for size of test loop
    put_line ("For" & size'img & " cards, there are "& count_shuffle (size / 2)'img & " shuffles needed.");
  end loop;
end perfect_shuffle;
```

```txt

For 8 cards, there are  3 shuffles needed.
For 24 cards, there are  11 shuffles needed.
For 52 cards, there are  8 shuffles needed.
For 100 cards, there are  30 shuffles needed.
For 1020 cards, there are  1018 shuffles needed.
For 1024 cards, there are  10 shuffles needed.
For 10000 cards, there are  300 shuffles needed.

```



## ALGOL 68


```algol68
# returns an array of the specified length, initialised to an ascending sequence of integers #
OP   DECK = ( INT length )[]INT:
     BEGIN
         [ 1 : length ]INT result;
         FOR i TO UPB result DO result[ i ] := i OD;
        result
     END # DECK # ;

# in-place shuffles the deck as per the task requirements #
# LWB deck is assumed to be 1 #
PROC shuffle = ( REF[]INT deck )VOID:
     BEGIN
         [ 1 : UPB deck ]INT result;
         INT left pos  := 1;
         INT right pos := ( UPB deck OVER 2 ) + 1;
         FOR i FROM 2 BY 2 TO UPB result DO
             result[ left pos  ] := deck[ i - 1 ];
             result[ right pos ] := deck[ i     ];
             left pos  +:= 1;
             right pos +:= 1
         OD;
         FOR i TO UPB deck DO deck[ i ] := result[ i ] OD
     END # SHUFFLE # ;

# compares two integer arrays for equality #
OP   = = ( []INT a, b )BOOL:
     IF LWB a /= LWB b OR UPB a /= UPB b
     THEN # the arrays have different bounds #
         FALSE
     ELSE
         BOOL result := TRUE;
         FOR i FROM LWB a TO UPB a WHILE result := a[ i ] = b[ i ] DO SKIP OD;
         result
     FI # = # ;

# compares two integer arrays for inequality #
OP   /= = ( []INT a, b )BOOL: NOT ( a = b );

# returns the number of shuffles required to return a deck of the specified length #
# back to its original state #
PROC count shuffles = ( INT length )INT:
     BEGIN
         []            INT original deck  = DECK length;
         [ 1 : length ]INT shuffled deck := original deck;
         INT   count         := 1;
         WHILE shuffle( shuffled deck );
               shuffled deck /= original deck
         DO
             count +:= 1
         OD;
         count
     END # count shuffles # ;

# test the shuffling #
[]INT lengths = ( 8, 24, 52, 100, 1020, 1024, 10 000 );
FOR l FROM LWB lengths TO UPB lengths DO
    print( ( whole( lengths[ l ], -8 ) + ": " + whole( count shuffles( lengths[ l ] ), -6 ), newline ) )
OD
```

```txt

       8:      3
      24:     11
      52:      8
     100:     30
    1020:   1018
    1024:     10
   10000:    300

```



## AutoHotkey


```AutoHotkey
Shuffle(cards){
	n := cards.MaxIndex()/2,	res := []
	loop % n
		res.push(cards[A_Index]), res.push(cards[round(A_Index + n)])
	return res
}
```

Examples:
```AutoHotkey
test := [8, 24, 52, 100, 1020, 1024, 10000]
for each, val in test
{
	cards := [], original:=rep:=""
	loop, % val
		cards.push(A_Index), original .= (original?", ":"") A_Index
	while (res <> original)
	{
		res := ""
		for k, v in (cards := Shuffle(cards))
			res .= (res?", ":"") v
		rep := A_Index
	}
	result .= val "`t" rep "`n"
}
MsgBox % result
return
```

Outputs:
```txt
8	3
24	11
52	8
100	30
1020	1018
1024	10
10000	300
```



## C



```c
/*
### > INCLUDES <=========================================================
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
### > CONSTANTS <========================================================
*/
#define N_DECKS 7
const int kDecks[N_DECKS] = { 8, 24, 52, 100, 1020, 1024, 10000 };

/*
### > FUNCTION PROTOTYPES <==============================================
*/
int CreateDeck( int **deck, int nCards );
void InitDeck( int *deck, int nCards );
int DuplicateDeck( int **dest, const int *orig, int nCards );
int InitedDeck( int *deck, int nCards );
int ShuffleDeck( int *deck, int nCards );
void FreeDeck( int **deck );

/*
### > FUNCTION DEFINITIONS <=============================================
*/

int main() {
    int i, nCards, nShuffles;
    int *deck = NULL;

    for( i=0; i<N_DECKS; ++i ) {
        nCards = kDecks[i];

        if( !CreateDeck(&deck,nCards) ) {
            fprintf( stderr, "Error: malloc() failed!\n" );
            return 1;
        }

        InitDeck( deck, nCards );
        nShuffles = 0;

        do {
            ShuffleDeck( deck, nCards );
            ++nShuffles;
        } while( !InitedDeck(deck,nCards) );

        printf( "Cards count: %d, shuffles required: %d.\n", nCards, nShuffles );

        FreeDeck( &deck );
    }

    return 0;
}

int CreateDeck( int **deck, int nCards ) {
    int *tmp = NULL;

    if( deck != NULL )
        tmp = malloc( nCards*sizeof(*tmp) );

    return tmp!=NULL ? (*deck=tmp)!=NULL : 0; /* (?success) (:failure) */
}

void InitDeck( int *deck, int nCards ) {
    if( deck != NULL ) {
        int i;

        for( i=0; i<nCards; ++i )
            deck[i] = i;
    }
}

int DuplicateDeck( int **dest, const int *orig, int nCards ) {
    if( orig != NULL && CreateDeck(dest,nCards) ) {
        memcpy( *dest, orig, nCards*sizeof(*orig) );
        return 1; /* success */
    }
    else {
        return 0; /* failure */
    }
}

int InitedDeck( int *deck, int nCards ) {
    int i;

    for( i=0; i<nCards; ++i )
        if( deck[i] != i )
            return 0; /* not inited */

    return 1; /* inited */
}

int ShuffleDeck( int *deck, int nCards ) {
    int *copy = NULL;

    if( DuplicateDeck(&copy,deck,nCards) ) {
        int i, j;

        for( i=j=0; i<nCards/2; ++i, j+=2 ) {
            deck[j] = copy[i];
            deck[j+1] = copy[i+nCards/2];
        }

        FreeDeck( &copy );
        return 1; /* success */
    }
    else {
        return 0; /* failure */
    }
}

void FreeDeck( int **deck ) {
    if( *deck != NULL ) {
        free( *deck );
        *deck = NULL;
    }
}

```


```txt

Cards count: 8, shuffles required: 3.
Cards count: 24, shuffles required: 11.
Cards count: 52, shuffles required: 8.
Cards count: 100, shuffles required: 30.
Cards count: 1020, shuffles required: 1018.
Cards count: 1024, shuffles required: 10.
Cards count: 10000, shuffles required: 300.


Press "Enter" to quit...

```



## C++


```cpp

#include <iostream>
#include <algorithm>
#include <vector>

int pShuffle( int t ) {
    std::vector<int> v, o, r;

    for( int x = 0; x < t; x++ ) {
        o.push_back( x + 1 );
    }

    r = o;
    int t2 = t / 2 - 1, c = 1;

    while( true ) {
        v = r;
        r.clear();

        for( int x = t2; x > -1; x-- ) {
            r.push_back( v[x + t2 + 1] );
            r.push_back( v[x] );
        }

        std::reverse( r.begin(), r.end() );

        if( std::equal( o.begin(), o.end(), r.begin() ) ) return c;
        c++;
    }
}

int main() {
    int s[] = { 8, 24, 52, 100, 1020, 1024, 10000 };
    for( int x = 0; x < 7; x++ ) {
        std::cout << "Cards count: " << s[x] << ", shuffles required: ";
        std::cout << pShuffle( s[x] ) << ".\n";
    }
    return 0;
}

```

```txt

Cards count: 8, shuffles required: 3.
Cards count: 24, shuffles required: 11.
Cards count: 52, shuffles required: 8.
Cards count: 100, shuffles required: 30.
Cards count: 1020, shuffles required: 1018.
Cards count: 1024, shuffles required: 10.
Cards count: 10000, shuffles required: 300.

```



## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

public static class PerfectShuffle
{
    static void Main()
    {
        foreach (int input in new [] {8, 24, 52, 100, 1020, 1024, 10000}) {
            int[] numbers = Enumerable.Range(1, input).ToArray();
            Console.WriteLine($"{input} cards: {ShuffleThrough(numbers).Count()}");
        }

        IEnumerable<T[]> ShuffleThrough<T>(T[] original) {
            T[] copy = (T[])original.Clone();
            do {
                yield return copy = Shuffle(copy);
            } while (!Enumerable.SequenceEqual(original, copy));
        }
    }

    public static T[] Shuffle<T>(T[] array) {
        if (array.Length % 2 != 0) throw new ArgumentException("Length must be even.");
        int half = array.Length / 2;
        T[] result = new T[array.Length];
        for (int t = 0, l = 0, r = half; l < half; t+=2, l++, r++) {
            result[t] = array[l];
            result[t+1] = array[r];
        }
        return result;
    }

}
```

```txt

8 cards: 3
24 cards: 11
52 cards: 8
100 cards: 30
1020 cards: 1018
1024 cards: 10
10000 cards: 300

```



## Clojure


```clojure
(defn perfect-shuffle [deck]
  (let [half (split-at (/ (count deck) 2) deck)]
    (interleave (first half) (last half))))

(defn solve [deck-size]
  (let [original (range deck-size)
        trials (drop 1 (iterate perfect-shuffle original))
        predicate #(= original %)]
    (println (format "%5s: %s" deck-size
      (inc (some identity (map-indexed (fn [i x] (when (predicate x) i)) trials)))))))

(map solve [8 24 52 100 1020 1024 10000])
```


```txt

    8: 3
   24: 11
   52: 8
  100: 30
 1020: 1018
 1024: 10
10000: 300

```



## Common Lisp


```lisp
(defun perfect-shuffle (deck)
  (let* ((half (floor (length deck) 2))
         (left (subseq deck 0 half))
         (right (nthcdr half deck)))
    (mapcan #'list left right)))

(defun solve (deck-size)
  (loop with original = (loop for n from 1 to deck-size collect n)
        for trials from 1
        for deck = original then shuffled
        for shuffled = (perfect-shuffle deck)
        until (equal shuffled original)
        finally (format t "~5D: ~4D~%" deck-size trials)))

(solve 8)
(solve 24)
(solve 52)
(solve 100)
(solve 1020)
(solve 1024)
(solve 10000)
```

```txt
    8:    3
   24:   11
   52:    8
  100:   30
 1020: 1018
 1024:   10
10000:  300
```



## D

```D
import std.stdio;

void main() {
    auto sizes = [8, 24, 52, 100, 1020, 1024, 10_000];
    foreach(s; sizes) {
        writefln("%5s : %5s", s, perfectShuffle(s));
    }
}

int perfectShuffle(int size) {
    import std.exception : enforce;
    enforce(size%2==0);

    import std.algorithm : copy, equal;
    import std.range;
    int[] orig = iota(0, size).array;

    int[] process;
    process.length = size;
    copy(orig, process);

    for(int count=1; true; count++) {
        process = roundRobin(process[0..$/2], process[$/2..$]).array;

        if (equal(orig, process)) {
            return count;
        }
    }

    assert(false, "How did this get here?");
}
```


```txt
    8 :     3
   24 :    11
   52 :     8
  100 :    30
 1020 :  1018
 1024 :    10
10000 :   300
```



## Dyalect


```dyalect
func shuffle(arr) {
    if arr.len() % 2 != 0 {
        throw "Length must be even."
    }
    var half = arr.len() / 2
    var result = Array.empty(size: arr.len())
    var (t, l, r) = (0, 0, half)

    while l < half {
        result[t] = arr[l]
        result[t+1] = arr[r]
        l += 1
        r += 1
        t += 2
    }
    result
}

func arrayEqual(xs, ys) {
    if xs.len() != ys.len() {
        return false
    }
    for i in xs.indices() {
        if xs[i] != ys[i] {
            return false
        }
    }
    return true
}

func shuffleThrough(original) {
    var copy = original.clone()

    while true {
        yield (copy = shuffle(copy))
        if arrayEqual(original, copy) {
            break
        }
    }
}

for input in { 8, 24, 52, 100, 1020, 1024, 10000} {
    var numbers = [1..input]
    print("\(input) cards: \(shuffleThrough(numbers).len())");
}
```


```txt
8 cards: 3
24 cards: 11
52 cards: 8
100 cards: 30
1020 cards: 1018
1024 cards: 10
10000 cards: 300
```



## EchoLisp


```lisp

;; shuffler : a permutation vector which interleaves both halves of deck
(define (make-shuffler n)
	(let ((s (make-vector n)))
		(for ((i (in-range 0 n 2))) (vector-set! s i (/ i 2)))
		(for ((i (in-range 0 n 2))) (vector-set! s (1+ i) (+ (/ n 2) (vector-ref s i))))
	 s))

;; output : (n . # of shuffles needed to go back)
(define (magic-shuffle n)
		(when (odd? n) (error "magic-shuffle:odd input" n))
		(let [(deck (list->vector (iota n))) ;; (0 1 ... n-1)
			(dock (list->vector (iota n))) ;; keep trace or init deck
			(shuffler (make-shuffler n))]

		(cons n (1+
		(for/sum ((i Infinity)) ; (in-naturals missing  in EchoLisp v2.9)
			(vector-permute! deck shuffler) ;; permutes in place
		    #:break (eqv? deck dock) ;; compare to first
			1)))))

```


```lisp

map magic-shuffle '(8 24 52 100 1020 1024 10000))
    → ((8 . 3) (24 . 11) (52 . 8) (100 . 30) (1020 . 1018) (1024 . 10) (10000 . 300))

;; Let's look in the On-line Encyclopedia of Integer Sequences
;; Given a list of numbers, the (oeis ...) function looks for a sequence

(lib 'web)
Lib: web.lib loaded.
map magic-shuffle (range 2 18 2))
    → ((2 . 1) (4 . 2) (6 . 4) (8 . 3) (10 . 6) (12 . 10) (14 . 12) (16 . 4))
(oeis '(1 2 4 3 6 10 12 4))
→ Sequence A002326 found

```



## Elixir

```elixir
defmodule Perfect do
  def shuffle(n) do
    start = Enum.to_list(1..n)
    m = div(n, 2)
    shuffle(start, magic_shuffle(start, m), m, 1)
  end

  defp shuffle(start, start, _, step), do: step
  defp shuffle(start, deck, m, step) do
    shuffle(start, magic_shuffle(deck, m), m, step+1)
  end

  defp magic_shuffle(deck, len) do
    {left, right} = Enum.split(deck, len)
    Enum.zip(left, right)
    |> Enum.map(&Tuple.to_list/1)
    |> List.flatten
  end
end

Enum.each([8, 24, 52, 100, 1020, 1024, 10000], fn n ->
  step = Perfect.shuffle(n)
  IO.puts "#{n} : #{step}"
end)
```


```txt

8 : 3
24 : 11
52 : 8
100 : 30
1020 : 1018
1024 : 10
10000 : 300

```


=={{header|F_Sharp|F#}}==

```fsharp

let perfectShuffle xs =
  let h = (List.length xs) / 2
  xs
  |> List.mapi (fun i x->(if i<h then i * 2 else ((i-h) * 2) + 1), x)
  |> List.sortBy fst
  |> List.map snd

let orderCount n =
  let xs = [1..n]
  let rec spin count ys =
    if xs=ys then count
    else ys |> perfectShuffle |> spin (count + 1)
  xs |> perfectShuffle |> spin 1

[ 8; 24; 52; 100; 1020; 1024; 10000 ] |> List.iter (fun n->n |> orderCount |> printfn "%d %d" n)

```


```txt

8 3
24 11
52 8
100 30
1020 1018
1024 10
10000 300

```



## Factor


```factor
USING: arrays formatting kernel math prettyprint sequences
sequences.merged ;
IN: rosetta-code.perfect-shuffle

CONSTANT: test-cases { 8 24 52 100 1020 1024 10000 }

: shuffle ( seq -- seq' ) halves 2merge ;

: shuffle-count ( n -- m )
    <iota> >array 0 swap dup [ 2dup = ] [ shuffle [ 1 + ] 2dip ]
    do until 2drop ;

"Deck size" "Number of shuffles required" "%-11s %-11s\n" printf
test-cases [ dup shuffle-count "%-11d %-11d\n" printf ] each
```

```txt

Deck size   Number of shuffles required
8           3
24          11
52          8
100         30
1020        1018
1024        10
10000       300

```



## Go


```go
package main

import "fmt"

type Deck struct {
	Cards []int
	length int
}

func NewDeck(deckSize int) (res *Deck){
	if deckSize % 2 != 0{
		panic("Deck size must be even")
	}
	res = new(Deck)
	res.Cards = make([]int, deckSize)
	res.length = deckSize
	for i,_ := range  res.Cards{
		res.Cards[i] = i
	}
	return
}
func (d *Deck)shuffleDeck(){
	tmp := make([]int,d.length)
	for i := 0;i <d.length/2;i++  {
		tmp[i*2] = d.Cards[i]
		tmp[i*2+1] = d.Cards[d.length / 2 + i]
	}
	d.Cards = tmp
}
func (d *Deck) isEqualTo(c Deck) (res bool) {
	if d.length != c.length {
		panic("Decks aren't equally sized")
	}
	res = true
	for i, v := range d.Cards{
		if v != c.Cards[i] {
			res = false
		}
	}
	return
}


func main(){
	for _,v := range []int{8,24,52,100,1020,1024,10000} {
		fmt.Printf("Cards count: %d, shuffles required: %d\n",v,ShufflesRequired(v))
	}
}

func ShufflesRequired(deckSize int)(res int){
	deck := NewDeck(deckSize)
	Ref := *deck
	deck.shuffleDeck()
	res++
	for ;!deck.isEqualTo(Ref);deck.shuffleDeck(){
		res++
	}
	return
}
```

```txt
Cards count: 8, shuffles required: 3
Cards count: 24, shuffles required: 11
Cards count: 52, shuffles required: 8
Cards count: 100, shuffles required: 30
Cards count: 1020, shuffles required: 1018
Cards count: 1024, shuffles required: 10
Cards count: 10000, shuffles required: 300
```



## Haskell


```Haskell
shuffle :: [a] -> [a]
shuffle lst = let (a,b) = splitAt (length lst `div` 2) lst
              in foldMap (\(x,y) -> [x,y]) $ zip a b

findCycle :: Eq a => (a -> a) -> a -> [a]
findCycle f x = takeWhile (/= x) $ iterate f (f x)

main = mapM_ report [ 8, 24, 52, 100, 1020, 1024, 10000 ]
  where
    report n = putStrLn ("deck of " ++ show n ++ " cards: "
                         ++ show (countSuffles n) ++ " shuffles!")
    countSuffles n = 1 + length (findCycle shuffle [1..n])
```


```txt
deck of 8 cards: 3 shuffles!
deck of 24 cards: 11 shuffles!
deck of 52 cards: 8 shuffles!
deck of 100 cards: 30 shuffles!
deck of 1020 cards: 1018 shuffles!
deck of 1024 cards: 10 shuffles!
deck of 10000 cards: 300 shuffles!

```



## J


The shuffle routine:


```J
   shuf=: /: $ /:@$ 0 1"_
```


Here, the phrase ($ $ 0 1"_) would generate a sequence of 0s and 1s the same length as the argument sequence:


```J
   ($ $ 0 1"_) 'abcdef'
0 1 0 1 0 1
```


And we can use ''grade up'' <code>(/:)</code> to find the indices which would sort the argument sequence so that the values in the positions corresponding to our generated zeros would come before the values in the positions corresponding to our ones.


```J
   /: ($ $ 0 1"_) 'abcdef'
0 2 4 1 3 5
```


But we can use ''grade up'' again to find what would have been the original permutation (''grade up'' is a self inverting function for this domain).


```J
   /:/: ($ $ 0 1"_) 'abcdef'
0 3 1 4 2 5
```


And, that means it can also sort the original sequence into that order:


```J
   shuf 'abcdef'
adbecf
   shuf 'abcdefgh'
aebfcgdh
```


And this will work for sequences of arbitrary length.

(The rest of the implementation of <code>shuf</code> is pure syntactic sugar - you can use J's [[j:Vocabulary/Dissect|dissect]] and [[j:Scripts/Tracer|trace]] facilities to see the details if you are trying to learn the language.)

Meanwhile, the cycle length routine could look like this:


```J
   shuflen=:  [: *./ #@>@C.@shuf@i.
```


Here, we first generate a list of integers of the required length in their natural order. We then reorder them using our <code>shuf</code> function, find the [[j:Vocabulary/ccapdot|cycles]] which result, find the lengths of each of these cycles then find the least common multiple of those lengths.

So here is the task example (with most of the middle trimmed out to avoid crashing the rosettacode wiki implementation):


```J
   shuflen"0 }.2*i.5000
1 2 4 3 6 10 12 4 8 18 6 11 20 18 28 5 10 12 36 12 20 14 12 23 21 8 52 20 18 ... 4278 816 222 1332 384
```


Task example:


```J
  ('deck size';'required shuffles'),(; shuflen)&> 8 24 52 100 1020 1024 10000
┌─────────┬─────────────────┐
│deck size│required shuffles│
├─────────┼─────────────────┤
│8        │3                │
├─────────┼─────────────────┤
│24       │11               │
├─────────┼─────────────────┤
│52       │8                │
├─────────┼─────────────────┤
│100      │30               │
├─────────┼─────────────────┤
│1020     │1018             │
├─────────┼─────────────────┤
│1024     │10               │
├─────────┼─────────────────┤
│10000    │300              │
└─────────┴─────────────────┘
```


Note that the implementation of <code>shuf</code> defines a behavior for odd length "decks". Experimentation shows that cycle length for an odd length deck is often the same as the cycle length for an even length deck which is one "card" longer.


## Java

```java
import java.util.Arrays;
import java.util.stream.IntStream;

public class PerfectShuffle {

    public static void main(String[] args) {
        int[] sizes = {8, 24, 52, 100, 1020, 1024, 10_000};
        for (int size : sizes)
            System.out.printf("%5d : %5d%n", size, perfectShuffle(size));
    }

    static int perfectShuffle(int size) {
        if (size % 2 != 0)
            throw new IllegalArgumentException("size must be even");

        int half = size / 2;
        int[] a = IntStream.range(0, size).toArray();
        int[] original = a.clone();
        int[] aa = new int[size];

        for (int count = 1; true; count++) {
            System.arraycopy(a, 0, aa, 0, size);

            for (int i = 0; i < half; i++) {
                a[2 * i] = aa[i];
                a[2 * i + 1] = aa[i + half];
            }

            if (Arrays.equals(a, original))
                return count;
        }
    }
}
```



```txt
    8 :     3
   24 :    11
   52 :     8
  100 :    30
 1020 :  1018
 1024 :    10
10000 :   300
```



## JavaScript


### ES6


```JavaScript
(() => {
    'use strict';

    // shuffleCycleLength :: Int -> Int
    const shuffleCycleLength = deckSize =>
        firstCycle(shuffle, range(1, deckSize))
        .all.length;

    // shuffle :: [a] -> [a]
    const shuffle = xs =>
        concat(zip.apply(null, splitAt(div(length(xs), 2), xs)));

    // firstycle :: Eq a => (a -> a) -> a -> [a]
    const firstCycle = (f, x) =>
        until(
            m => EqArray(x, m.current),
            m => {
                const fx = f(m.current);
                return {
                    current: fx,
                    all: m.all.concat([fx])
                };
            }, {
                current: f(x),
                all: [x]
            }
        );

    // Two arrays equal ?
    // EqArray :: [a] -> [b] -> Bool
    const EqArray = (xs, ys) => {
        const [nx, ny] = [xs.length, ys.length];
        return nx === ny ? (
            nx > 0 ? (
                xs[0] === ys[0] && EqArray(xs.slice(1), ys.slice(1))
            ) : true
        ) : false;
    };

    // GENERIC FUNCTIONS

    // zip :: [a] -> [b] -> [(a,b)]
    const zip = (xs, ys) =>
        xs.slice(0, Math.min(xs.length, ys.length))
        .map((x, i) => [x, ys[i]]);

    // concat :: [[a]] -> [a]
    const concat = xs => [].concat.apply([], xs);

    // splitAt :: Int -> [a] -> ([a],[a])
    const splitAt = (n, xs) => [xs.slice(0, n), xs.slice(n)];

    // div :: Num -> Num -> Int
    const div = (x, y) => Math.floor(x / y);

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        const go = x => p(x) ? x : go(f(x));
        return go(x);
    }

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // length :: [a] -> Int
    // length :: Text -> Int
    const length = xs => xs.length;

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        xs.reduce((a, x) => a === undefined ? x : (
            f(x, a) > 0 ? x : a
        ), undefined);

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map((row) => row[iCol]));

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    // replicateS :: Int -> String -> String
    const replicateS = (n, s) => {
        let v = s,
            o = '';
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // justifyRight :: Int -> Char -> Text -> Text
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (replicateS(n, cFiller) + strText)
            .slice(-n)
        ) : strText;

    // TEST
    return transpose(transpose([
                ['Deck', 'Shuffles']
            ].concat(
                [8, 24, 52, 100, 1020, 1024, 10000]
                .map(n => [n.toString(), shuffleCycleLength(n)
                    .toString()
                ])))
            .map(col => { // Right-justified number columns
                const width = length(
                    maximumBy((a, b) => length(a) - length(b), col)
                ) + 2;

                return col.map(x => justifyRight(width, ' ', x));
            }))
        .map(row => row.join(''))
        .join('\n');
})();
```


```txt
   Deck  Shuffles
      8         3
     24        11
     52         8
    100        30
   1020      1018
   1024        10
  10000       300
```




## Julia


```julia
# v0.6

function perfect_shuffle(a::Array)::Array
    if isodd(length(a)) error("cannot perform perfect shuffle on odd-length array") end

    rst = zeros(a)
    mid = div(length(a), 2)
    for i in 1:mid
        rst[2i-1], rst[2i] = a[i], a[mid+i]
    end
    return rst
end

function count_perfect_shuffles(decksize::Int)::Int
    a = collect(1:decksize)
    b, c = perfect_shuffle(a), 1
    while a != b
        b = perfect_shuffle(b)
        c += 1
    end
    return c
end

println("    Deck  n.Shuffles")
for i in (8, 24, 52, 100, 1020, 1024, 10000, 100000)
    count = count_perfect_shuffles(i)
    @printf("%7i%7i\n", i, count)
end
```


```txt
    Deck  n.Shuffles
      8      3
     24     11
     52      8
    100     30
   1020   1018
   1024     10
  10000    300
 100000    540
```



## Kotlin


```scala
// version 1.1.2

fun areSame(a: IntArray, b: IntArray): Boolean {
    for (i in 0 until a.size) if (a[i] != b[i]) return false
    return true
}

fun perfectShuffle(a: IntArray): IntArray {
    var b = IntArray(a.size)
    val hSize = a.size / 2
    for (i in 0 until hSize) b[i * 2] = a[i]
    var j = 1
    for (i in hSize until a.size) {
        b[j] = a[i]
        j += 2
    }
    return b
}

fun countShuffles(a: IntArray): Int {
    require(a.size >= 2 && a.size % 2 == 0)
    var b = a
    var count = 0
    while (true) {
        val c = perfectShuffle(b)
        count++
        if (areSame(a, c)) return count
        b = c
    }
}

fun main(args: Array<String>) {
    println("Deck size  Num shuffles")
    println("---------  ------------")
    val sizes = intArrayOf(8, 24, 52, 100, 1020, 1024, 10000)
    for (size in sizes) {
        val a = IntArray(size) { it }
        val count = countShuffles(a)
        println("${"%-9d".format(size)}     $count")
    }
}
```


```txt

Deck size  Num shuffles
---------  ------------
8             3
24            11
52            8
100           30
1020          1018
1024          10
10000         300

```



## Lua


```Lua
-- Perform weave shuffle
function shuffle (cards)
    local pile1, pile2 = {}, {}
    for card = 1, #cards / 2 do table.insert(pile1, cards[card]) end
    for card = (#cards / 2) + 1, #cards do table.insert(pile2, cards[card]) end
    cards = {}
    for card = 1, #pile1 do
        table.insert(cards, pile1[card])
        table.insert(cards, pile2[card])
    end
    return cards
end

-- Return boolean indicating whether or not the cards are in order
function inOrder (cards)
    for k, v in pairs(cards) do
        if k ~= v then return false end
    end
    return true
end

-- Count the number of shuffles needed before the cards are in order again
function countShuffles (deckSize)
    local deck, count = {}, 0
    for i = 1, deckSize do deck[i] = i end
    repeat
        deck = shuffle(deck)
        count = count + 1
    until inOrder(deck)
    return count
end

-- Main procedure
local testCases = {8, 24, 52, 100, 1020, 1024, 10000}
print("Input", "Output")
for _, case in pairs(testCases) do print(case, countShuffles(case)) end
```

```txt
Input   Output
8       3
24      11
52      8
100     30
1020    1018
1024    10
10000   300
```



## Mathematica


```Mathematica
shuffle[deck_] := Apply[Riffle, TakeDrop[deck, Length[deck]/2]];
shuffleCount[n_] := Block[{count=0}, NestWhile[shuffle, shuffle[Range[n]], (count++; OrderedQ[#] )&];count];
Map[shuffleCount, {8, 24, 52, 100, 1020, 1024, 10000}]
```

```txt
{3, 11, 8, 30, 1018, 10, 300}
```



## MATLAB

PerfectShuffle.m:

```matlab
function [New]=PerfectShuffle(Nitems, Nturns)
    if mod(Nitems,2)==0 %only if even number
        X=1:Nitems; %define deck
        for c=1:Nturns %defines one shuffle
            X=reshape(X,Nitems/2,2)'; %split the deck in two and stack halves
            X=X(:)'; %mix the halves
        end
        New=X; %result of multiple shufflings
    end
```


Main:

```matlab
Result=[]; %vector to store results
Q=[8, 24, 52, 100, 1020, 1024, 10000]; %queries
for n=Q %for each query
    Same=0; %initialize comparison
    T=0; %initialize number of shuffles
    while ~Same %while the result is not the original query
        T=T+1; %one more shuffle
        R=PerfectShuffle(n,T); %result of shuffling the query
        Same=~(any(R-(1:n))); %same vector as the query
    end %when getting the same vector
    Result=[Result;T]; %collect results
end
disp([Q', Result])
```

```txt
           8           3
          24          11
          52           8
         100          30
        1020        1018
        1024          10
       10000         300
```


=={{header|Modula-2}}==
```modula2
MODULE PerfectShuffle;
FROM FormatString IMPORT FormatString;
FROM Storage IMPORT ALLOCATE,DEALLOCATE;
FROM SYSTEM IMPORT ADDRESS,TSIZE;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteCard(c : CARDINAL);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%c", buf, c);
    WriteString(buf)
END WriteCard;

PROCEDURE Init(VAR arr : ARRAY OF INTEGER);
VAR i : CARDINAL;
BEGIN
    FOR i:=0 TO HIGH(arr) DO
        arr[i] := i + 1
    END
END Init;

PROCEDURE PerfectShuffle(VAR arr : ARRAY OF INTEGER);
    PROCEDURE Inner(ti : CARDINAL);
    VAR
        tv : INTEGER;
        tp,tn,n : CARDINAL;
    BEGIN
        n := HIGH(arr);
        tn := ti;
        tv := arr[ti];
        REPEAT
            tp := tn;
            IF tp MOD 2 = 0 THEN
                tn := tp / 2
            ELSE
                tn := (n+1)/2+tp/2
            END;
            arr[tp] := arr[tn];
        UNTIL tn = ti;
        arr[tp] := tv
    END Inner;
VAR
    done : BOOLEAN;
    i,c : CARDINAL;
BEGIN
    c := 0;
    Init(arr);

    REPEAT
        i := 1;
        WHILE i <= (HIGH(arr)/2) DO
            Inner(i);
            INC(i,2)
        END;
        INC(c);

        done := TRUE;
        FOR i:=0 TO HIGH(arr) DO
            IF arr[i] # INT(i+1) THEN
                done := FALSE;
                BREAK
            END
        END
    UNTIL done;

    WriteCard(HIGH(arr)+1);
    WriteString(": ");
    WriteCard(c);
    WriteLn
END PerfectShuffle;

(* Main *)
VAR
        v8 : ARRAY[1..8] OF INTEGER;
       v24 : ARRAY[1..24] OF INTEGER;
       v52 : ARRAY[1..52] OF INTEGER;
      v100 : ARRAY[1..100] OF INTEGER;
     v1020 : ARRAY[1..1020] OF INTEGER;
     v1024 : ARRAY[1..1024] OF INTEGER;
    v10000 : ARRAY[1..10000] OF INTEGER;
BEGIN
    PerfectShuffle(v8);
    PerfectShuffle(v24);
    PerfectShuffle(v52);
    PerfectShuffle(v100);
    PerfectShuffle(v1020);
    PerfectShuffle(v1024);
    PerfectShuffle(v10000);

    ReadChar
END PerfectShuffle.
```

```txt
8: 3
24: 11
52: 8
100: 30
1020: 1018
1024: 10
10000: 300
```



## Oforth



```oforth
: shuffle(l)     l size 2 / dup l left swap l right zip expand ;
: nbShuffles(l)  1 l while( shuffle dup l <> ) [ 1 under+ ] drop ;
```


```txt

>[ 8, 24, 52, 100, 1020, 1024, 10000 ] map(#[ seq nbShuffles ]) .
[3, 11, 8, 30, 1018, 10, 300] ok

```



## PARI/GP

```parigp
magic(v)=vector(#v,i,v[if(i%2,1,#v/2)+i\2]);
shuffles_slow(n)=my(v=[1..n],o=v,s=1);while((v=magic(v))!=o,s++);s;
shuffles(n)=znorder(Mod(2,n-1));
vector(5000,n,shuffles_slow(2*n))
```

```txt
%1 = [1, 2, 4, 3, 6, 10, 12, 4, 8, 18, 6, 11, 20, 18, 28, 5, 10, 12, 36, 12,
 20, 14, 12, 23, 21, 8, 52, 20, 18, 58, 60, 6, 12, 66, 22, 35, 9, 20, 30, 39, 54
, 82, 8, 28, 11, 12, 10, 36, 48, 30, 100, 51, 12, 106, 36, 36, 28, 44, 12, 24, 1
10, 20, 100, 7, 14, 130, 18, 36, 68, 138, 46, 60, 28, 42, 148, 15, 24, 20, 52, 5
2, 33, 162, 20, 83, 156, 18, 172, 60, 58, 178, 180, 60, 36, 40, 18, 95, 96, 12,
196, 99, 66, 84, 20, 66, 90, 210, 70, 28, 15, 18, 24, 37, 60, 226, 76, 30, 29, 9
2, 78, 119, 24, 162, 84, 36, 82, 50, 110, 8, 16, 36, 84, 131, 52, 22, 268, 135,
12, 20, 92, 30, 70, 94, 36, 60, 136, 48, 292, 116, 90, 132, 42, 100, 60, 102, 10
2, 155, 156, 12, 316, 140, 106, 72, 60, 36, 69, 30, 36, 132, 21, 28, 10, 147, 44
, 346, 348, 36, 88, 140, 24, 179, 342, 110, 36, 183, 60, 156, 372, 100, 84, 378,
 14, 191, 60, 42, 388, 88, 130, 156, 44, 18, 200, 60, 108, 180, 204, 68, 174, 16
4, 138, 418, 420, 138, 40, 60, 60, 43, 72, 28, 198, 73, 42, 442, 44, 148, 224, 2
0, 30, 12, 76, 72, 460, 231, 20, 466, 66, 52, 70, 180, 156, 239, 36, 66, 48, 243
, 162, 490, 56, 60, 105, 166, 166, 251, 100, 156, 508, 9, 18, 204, 230, 172, 260
, 522, 60, 40, 253, 174, 60, 212, 178, 210, 540, 180, 36, 546, 60, 252, 39, 36,
556, 84, 40, 562, 28, 54, 284, 114, 190, 220, 144, 96, 246, 260, 12, 586, 90, 19
6, 148, 24, 198, 299, 25, 66, 220, 303, 84, 276, 612, 20, 154, 618, 198, 33, 500
, 90, 72, 45, 210, 28, 84, 210, 64, 214, 28, 323, 290, 30, 652, 260, 18, 658, 66
0, 24, 36, 308, 74, 60, 48, 180, 676, 48, 226, 22, 68, 76, 156, 230, 30, 276, 40
, 58, 700, 36, 92, 300, 708, 78, 55, 60, 238, 359, 51, 24, 140, 121, 486, 56, 24
4, 84, 330, 246, 36, 371, 148, 246, 318, 375, 50, 60, 756, 110, 380, 36, 24, 348
, 384, 16, 772, 20, 36, 180, 70, 252, 52, 786, 262, 84, 60, 52, 796, 184, 66, 90
, 132, 268, 404, 270, 270, 324, 126, 12, 820, 411, 20, 826, 828, 92, 168, 332, 9
0, 419, 812, 70, 156, 330, 94, 396, 852, 36, 428, 858, 60, 431, 172, 136, 390, 1
32, 48, 300, 876, 292, 55, 882, 116, 443, 21, 270, 414, 356, 132, 140, 104,[+++]
```


(By default gp won't show more than 25 lines of output, though an arbitrary amount can be printed or written to a file; use <code>print</code>, <code>write</code>, or <code>default(lines, 100)</code> to show more.)


## Perl



```perl
use List::Util qw(all);

sub perfect_shuffle {
   my $mid = @_ / 2;
   map { @_[$_, $_ + $mid] } 0..($mid - 1);
}

for my $size (8, 24, 52, 100, 1020, 1024, 10000) {

    my @shuffled = my @deck = 1 .. $size;
    my $n = 0;
    do { $n++; @shuffled = perfect_shuffle(@shuffled) }
        until all { $shuffled[$_] == $deck[$_] } 0..$#shuffled;

    printf "%5d cards: %4d\n", $size, $n;
}
```


```txt

    8 cards:    3
   24 cards:   11
   52 cards:    8
  100 cards:   30
 1020 cards: 1018
 1024 cards:   10
10000 cards:  300

```



## Perl 6


```perl6
sub perfect-shuffle (@deck) {
    my $mid = @deck / 2;
    flat @deck[0 ..^ $mid] Z @deck[$mid .. *];
}

for 8, 24, 52, 100, 1020, 1024, 10000 -> $size {
    my @deck = ^$size;
    my $n;
    repeat until [<] @deck {
        $n++;
        @deck = perfect-shuffle @deck;
    }

    printf "%5d cards: %4d\n", $size, $n;
}
```


```txt

    8 cards:    3
   24 cards:   11
   52 cards:    8
  100 cards:   30
 1020 cards: 1018
 1024 cards:   10
10000 cards:  300

```



## Phix


```Phix
function perfect_shuffle(sequence deck)
integer mp = length(deck)/2
sequence res = deck
    integer k = 1
    for i=1 to mp do
        res[k] = deck[i]        k += 1
        res[k] = deck[i+mp]     k += 1
    end for
    return res
end function

constant testsizes = {8, 24, 52, 100, 1020, 1024, 10000}
for i=1 to length(testsizes) do
    sequence deck = tagset(testsizes[i])
    sequence work = perfect_shuffle(deck)
    integer count = 1
    while work!=deck do
        work = perfect_shuffle(work)
        count += 1
    end while
    printf(1,"%5d cards: %4d\n", {testsizes[i],count})
end for
```

```txt

    8 cards:    3
   24 cards:   11
   52 cards:    8
  100 cards:   30
 1020 cards: 1018
 1024 cards:   10
10000 cards:  300

```



## PicoLisp


```PicoLisp
(de perfectShuffle (Lst)
   (mapcan '((B A) (list A B))
      (cdr (nth Lst (/ (length Lst) 2)))
      Lst ) )

(for N (8 24 52 100 1020 1024 10000)
   (let (Lst (range 1 N)  L Lst  Cnt 1)
      (until (= Lst (setq L (perfectShuffle L)))
         (inc 'Cnt) )
      (tab (5 6) N Cnt) ) )
```

Output:

```txt
    8     3
   24    11
   52     8
  100    30
 1020  1018
 1024    10
10000   300
```



## Python



```python

import doctest
import random


def flatten(lst):
    """
    >>> flatten([[3,2],[1,2]])
    [3, 2, 1, 2]
    """
    return [i for sublst in lst for i in sublst]

def magic_shuffle(deck):
    """
    >>> magic_shuffle([1,2,3,4])
    [1, 3, 2, 4]
    """
    half = len(deck) // 2
    return flatten(zip(deck[:half], deck[half:]))

def after_how_many_is_equal(shuffle_type,start,end):
    """
    >>> after_how_many_is_equal(magic_shuffle,[1,2,3,4],[1,2,3,4])
    2
    """

    start = shuffle_type(start)
    counter = 1
    while start != end:
        start = shuffle_type(start)
        counter += 1
    return counter

def main():
    doctest.testmod()

    print("Length of the deck of cards | Perfect shuffles needed to obtain the same deck back")
    for length in (8, 24, 52, 100, 1020, 1024, 10000):
        deck = list(range(length))
        shuffles_needed = after_how_many_is_equal(magic_shuffle,deck,deck)
        print("{} | {}".format(length,shuffles_needed))


if __name__ == "__main__":
    main()


```

More functional version of the same code:

```python

"""
Brute force solution for the Perfect Shuffle problem.
See http://oeis.org/A002326 for possible improvements
"""
from functools import partial
from itertools import chain
from operator import eq
from typing import (Callable,
                    Iterable,
                    Iterator,
                    List,
                    TypeVar)

T = TypeVar('T')


def main():
    print("Deck length | Shuffles ")
    for length in (8, 24, 52, 100, 1020, 1024, 10000):
        deck = list(range(length))
        shuffles_needed = spin_number(deck, shuffle)
        print(f"{length:<11} | {shuffles_needed}")


def shuffle(deck: List[T]) -> List[T]:
    """[1, 2, 3, 4] -> [1, 3, 2, 4]"""
    half = len(deck) // 2
    return list(chain.from_iterable(zip(deck[:half], deck[half:])))


def spin_number(source: T,
                function: Callable[[T], T]) -> int:
    """
    Applies given function to the source
    until the result becomes equal to it,
    returns the number of calls
    """
    is_equal_source = partial(eq, source)
    spins = repeat_call(function, source)
    return next_index(is_equal_source,
                      spins,
                      start=1)


def repeat_call(function: Callable[[T], T],
                value: T) -> Iterator[T]:
    """(f, x) -> f(x), f(f(x)), f(f(f(x))), ..."""
    while True:
        value = function(value)
        yield value


def next_index(predicate: Callable[[T], bool],
               iterable: Iterable[T],
               start: int = 0) -> int:
    """
    Returns index of the first element of the iterable
    satisfying given condition
    """
    for index, item in enumerate(iterable, start=start):
        if predicate(item):
            return index


if __name__ == "__main__":
    main()

```

```txt
Deck length | Shuffles
8           | 3
24          | 11
52          | 8
100         | 30
1020        | 1018
1024        | 10
10000       | 300
```

Reversed shuffle or just calculate how many shuffles are needed:

```python
def mul_ord2(n):
	# directly calculate how many shuffles are needed to restore
	# initial order: 2^o mod(n-1) == 1
	if n == 2: return 1

	n,t,o = n-1,2,1
	while t != 1:
		t,o = (t*2)%n,o+1
	return o

def shuffles(n):
	a,c = list(range(n)), 0
	b = a

	while True:
		# Reverse shuffle; a[i] can be taken as the current
		# position of the card with value i.  This is faster.
		a = a[0:n:2] + a[1:n:2]
		c += 1
		if b == a: break
	return c

for n in range(2, 10000, 2):
	#print(n, mul_ord2(n))
	print(n, shuffles(n))
```



## R



```R

wave.shuffle <- function(n) {
  deck <- 1:n ## create the original deck
  new.deck <- c(matrix(data = deck, ncol = 2, byrow = TRUE)) ## shuffle the deck once
  counter <- 1 ## track the number of loops
  ## defining a loop that shuffles the new deck until identical with the original one
  ## and in the same time increses the counter with 1 per loop
  while (!identical(deck, new.deck)) { ## logical condition
    new.deck <- c(matrix(data = new.deck, ncol = 2, byrow = TRUE)) ## shuffle
    counter <- counter + 1 ## add 1 to the number of loops
  }
  return(counter) ## final result - total number of loops until the condition is met
}
test.values <- c(8, 24, 52, 100, 1020, 1024, 10000) ## the set of the test values
test <- sapply(test.values, wave.shuffle) ## apply the wave.shuffle function on each element
names(test) <- test.values ## name the result
test ## print the result out

> test
    8    24    52   100  1020  1024 10000
    3    11     8    30  1018    10   300

```



## Racket


```racket
#lang racket/base
(require racket/list)

(define (perfect-shuffle l)
  (define-values (as bs) (split-at l (/ (length l) 2)))
  (foldr (λ (a b d) (list* a b d)) null as bs))

(define (perfect-shuffles-needed n)
  (define-values (_ rv)
    (for/fold ((d (perfect-shuffle (range n))) (i 1))
              ((_ (in-naturals))
               #:break (apply < d))
      (values (perfect-shuffle d) (add1 i))))
  rv)

(module+ test
  (require rackunit)
  (check-equal? (perfect-shuffle '(1 2 3 4)) '(1 3 2 4))

  (define (test-perfect-shuffles-needed n e)
    (define psn (perfect-shuffles-needed n))
    (printf "Deck size:\t~a\tShuffles needed:\t~a\t(~a)~%" n psn e)
    (check-equal? psn e))

  (for-each test-perfect-shuffles-needed
            '(8 24 52 100 1020 1024 10000)
            '(3 11  8  30 1018   10   300)))
```


```txt
Deck size:	8	Shuffles needed:	3	(3)
Deck size:	24	Shuffles needed:	11	(11)
Deck size:	52	Shuffles needed:	8	(8)
Deck size:	100	Shuffles needed:	30	(30)
Deck size:	1020	Shuffles needed:	1018	(1018)
Deck size:	1024	Shuffles needed:	10	(10)
Deck size:	10000	Shuffles needed:	300	(300)
```



## REXX


### unoptimized


```rexx
/*REXX program performs a  "perfect shuffle"  for a number of  even numbered  decks.    */
parse arg X                                      /*optional list of test cases from C.L.*/
if X=''  then X=8 24 52 100 1020 1024 10000      /*Not specified?  Then use the default.*/
w=length(word(X, words(X)))                      /*used for right─aligning the numbers. */

    do j=1  for words(X);  y=word(X,j)           /*use numbers in the test suite (list).*/

      do k=1  for y;       @.k=k;      end /*k*/ /*generate a deck to be used (shuffled)*/
      do t=1  until eq();  call magic; end /*t*/ /*shuffle until  before  equals  after.*/

    say 'deck size:'    right(y,w)","       right(t,w)      'perfect shuffles.'
    end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eq:    do ?=1  for y;   if @.?\==?  then return 0;   end;           return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
magic: z=0                                       /*set the  Z  pointer  (used as index).*/
       h=y%2                                     /*get the half─way (midpoint) pointer. */
                do s=1  for h;  z=z+1;  h=h+1    /*traipse through the card deck pips.  */
                !.z=@.s;        z=z+1            /*assign left half; then bump pointer. */
                !.z=@.h                          /*   "   right  "                      */
                end   /*s*/                      /*perform a perfect shuffle of the deck*/

                do r=1  for y;  @.r=!.r;  end    /*re─assign to the original card deck. */
       return
```

'''output'''   (abbreviated)   when using the default input:

```txt

deck size:     8,     3 perfect shuffles.
deck size:    24,    11 perfect shuffles.
deck size:    52,     8 perfect shuffles.
deck size:   100,    30 perfect shuffles.
deck size:  1020,  1018 perfect shuffles.
deck size:  1024,    10 perfect shuffles.
deck size: 10000,   300 perfect shuffles.

```



### optimized

This REXX version takes advantage that the 1<sup>st</sup> and last cards of the deck don't change.

```rexx
/*REXX program does a  "perfect shuffle"  for a number of  even  numbered  decks.       */
parse arg X                                      /*optional list of test cases from C.L.*/
if X=''  then X=8 24 52 100 1020 1024 10000      /*Not specified?  Use default.*/
w=length(word(X, words(X)))                      /*used for right─aligning the numbers. */

    do j=1  for words(X);  y=word(X,j)           /*use numbers in the test suite (list).*/

      do k=1  for y;       @.k=k;       end      /*generate a deck to be shuffled (used)*/
      do t=1  until eq();  call magic;  end      /*shuffle until  before  equals  after.*/

    say 'deck size:'    right(y,w)","       right(t,w)      'perfect shuffles.'
    end     /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eq:           do ?=1  for y;    if @.?\==?  then return 0;    end;            return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
magic: z=1;                     h=y%2                        /*H  is (half─way) pointer.*/
              do L=3  by 2  for h-1; z=z+1; !.L=@.z; end     /*assign left half of deck.*/
              do R=2  by 2  for h-1; h=h+1; !.R=@.h; end     /*   "   right  "   "   "  */
              do a=2        for y-2;        @.a=!.a; end     /*re─assign──►original deck*/
       return
```

'''output'''   is the same as the 1<sup>st</sup> version.





## Ruby



```ruby
def perfect_shuffle(deck_size = 52)
	deck = (0...deck_size).to_a
	shuffled_deck = [deck.first(deck_size / 2), deck.last(deck_size / 2)]
	1.step do |i|
		return i if deck == (shuffled_deck = shuffled_deck.transpose.flatten)
		shuffled_deck = [shuffled_deck.shift(deck_size / 2), shuffled_deck]
	end
end

[8, 24, 52, 100, 1020, 1024, 10000].each do |i| puts "Perfect Shuffles Required for Deck Size #{i}: #{perfect_shuffle(i)}" end
```


```txt

Perfect Shuffles Required for Deck Size 8: 3
Perfect Shuffles Required for Deck Size 24: 11
Perfect Shuffles Required for Deck Size 52: 8
Perfect Shuffles Required for Deck Size 100: 30
Perfect Shuffles Required for Deck Size 1020: 1018
Perfect Shuffles Required for Deck Size 1024: 10
Perfect Shuffles Required for Deck Size 10000: 300
```



## Rust


```Rust
extern crate itertools;

fn shuffle<T>(mut deck: Vec<T>) -> Vec<T> {
    let index = deck.len() / 2;
    let right_half = deck.split_off(index);
    itertools::interleave(deck, right_half).collect()
}

fn main() {
    for &size in &[8, 24, 52, 100, 1020, 1024, 10_000] {
        let original_deck: Vec<_> = (0..size).collect();
        let mut deck = original_deck.clone();
        let mut iterations = 0;
        loop {
            deck = shuffle(deck);
            iterations += 1;
            if deck == original_deck {
                break;
            }
        }
        println!("{: >5}: {: >4}", size, iterations);
    }
}
```

```txt
    8:    3
   24:   11
   52:    8
  100:   30
 1020: 1018
 1024:   10
10000:  300

```



## Scala

===Imperative, Quick, dirty and ugly===
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/Ux9RKDx/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/eWeiDIBbQMGpNIQAmvXfLg Scastie (remote JVM)].

```Scala
object PerfectShuffle extends App {
  private def sizes = Seq(8, 24, 52, 100, 1020, 1024, 10000)

  private def perfectShuffle(size: Int): Int = {
    require(size % 2 == 0, "Card deck must be even")

    val (half, a) = (size / 2, Array.range(0, size))
    val original = a.clone
    var count = 1
    while (true) {
      val aa = a.clone
      for (i <- 0 until half) {
        a(2 * i) = aa(i)
        a(2 * i + 1) = aa(i + half)
      }
      if (a.deep == original.deep) return count
      count += 1
    }
    0
  }

  for (size <- sizes) println(f"$size%5d : ${perfectShuffle(size)}%5d")

}
```


## Scilab

<lang>function New=PerfectShuffle(Nitems,Nturns)
    if modulo(Nitems,2)==0 then
        X=1:Nitems;
        for c=1:Nturns
            X=matrix(X,Nitems/2,2)';
            X=X(:);
        end
        New=X';
    end
endfunction

Result=[];
Q=[8, 24, 52, 100, 1020, 1024, 10000];
for n=Q
    Same=0;
    T=0;
    Compare=[];
    while ~Same
        T=T+1;
        R=PerfectShuffle(n,T);
        Compare = find(R-(1:n));
        if Compare == [] then
            Same = 1;
        end
    end
    Result=[Result;T];
end
disp([Q', Result])
```


```txt
   8.       3.
   24.      11.
   52.      8.
   100.     30.
   1020.    1018.
   1024.    10.
   10000.   300.
```



## Sidef

```ruby
func perfect_shuffle(deck) {
     deck/2 -> zip.flat
}

[8, 24, 52, 100, 1020, 1024, 10000].each { |size|
    var deck = @(1..size)
    var shuffled = deck

    var n = (1..Inf -> lazy.first {
        (shuffled = perfect_shuffle(shuffled)) == deck
    })

    printf("%5d cards: %4d\n", size, n)
}
```


```txt

    8 cards:    3
   24 cards:   11
   52 cards:    8
  100 cards:   30
 1020 cards: 1018
 1024 cards:   10
10000 cards:  300

```



## Tcl


Using <tt>tcltest</tt> to include an executable test case ..


```Tcl
namespace eval shuffle {

    proc perfect {deck} {
        if {[llength $deck]%2} {
            return -code error "Deck must be of even length!"
        }
        set split [expr {[llength $deck]/2}]
        set top [lrange $deck 0 $split-1]
        set btm [lrange $deck $split end]
        foreach a $top b $btm {
            lappend res $a $b
        }
        return $res
    }

    proc cycle_length {transform deck} {
        set d $deck
        while 1 {
            set d [$transform $d]
            incr i
            if {$d eq $deck} {return $i}
        }
        return $i
    }

    proc range {a {b ""}} {
        if {$b eq ""} {
            set b $a; set a 0
        }
        set res {}
        while {$a < $b} {
            lappend res $a
            incr a
        }
        return $res
    }

}

set ::argv {}
package require tcltest
tcltest::test "Test perfect shuffle cycles" {} -body {
    lmap size {8 24 52 100 1020 1024 10000} {
        shuffle::cycle_length perfect [range $size]
    }
} -result {3 11 8 30 1018 10 300}
```



## VBA


```vb
Option Explicit

Sub Main()
Dim T, Arr, X As Long, C As Long
   Arr = Array(8, 24, 52, 100, 1020, 1024, 10000)
   For X = LBound(Arr) To UBound(Arr)
      C = 0
      Call PerfectShuffle(T, CLng(Arr(X)), C)
      Debug.Print Right(String(19, " ") & "For " & Arr(X) & " cards => ", 19) & C & " shuffles needed."
      Erase T
   Next
End Sub

Private Sub PerfectShuffle(tb, NbCards As Long, Count As Long)
Dim arr1, arr2, StrInit As String, StrTest As String

   tb = CreateArray(1, NbCards)
   StrInit = Join(tb, " ")
   Do
      Count = Count + 1
      Call DivideArr(tb, arr1, arr2)
      tb = RemakeArray(arr1, arr2)
      StrTest = Join(tb, " ")
   Loop While StrTest <> StrInit
End Sub

Private Function CreateArray(First As Long, Length As Long) As String()
Dim i As Long, T() As String, C As Long
   If IsEven(Length) Then
      ReDim T(Length - 1)
      For i = First To First + Length - 1
         T(C) = i
         C = C + 1
      Next i
      CreateArray = T
   End If
End Function

Private Sub DivideArr(A, B, C)
Dim i As Long
   B = A
   ReDim Preserve B(UBound(A) \ 2)
   ReDim C(UBound(B))
   For i = LBound(C) To UBound(C)
      C(i) = A(i + UBound(B) + 1)
   Next
End Sub

Private Function RemakeArray(A1, A2) As String()
Dim i As Long, T() As String, C As Long
   ReDim T((UBound(A2) * 2) + 1)
   For i = LBound(T) To UBound(T) - 1 Step 2
      T(i) = A1(C)
      T(i + 1) = A2(C)
      C = C + 1
   Next
   RemakeArray = T
End Function

Private Function IsEven(Number As Long) As Boolean
    IsEven = (Number Mod 2 = 0)
End Function
```

```txt
    For 8 cards => 3 shuffles needed.
   For 24 cards => 11 shuffles needed.
   For 52 cards => 8 shuffles needed.
  For 100 cards => 30 shuffles needed.
 For 1020 cards => 1018 shuffles needed.
 For 1024 cards => 10 shuffles needed.
For 10000 cards => 300 shuffles needed.
```




## zkl


```zkl
fcn perfectShuffle(numCards){
   deck,shuffle,n,N:=numCards.pump(List),deck,0,numCards/2;
   do{ shuffle=shuffle[0,N].zip(shuffle[N,*]).flatten(); n+=1 }
   while(deck!=shuffle);
   n
}
foreach n in (T(8,24,52,100,1020,1024,10000)){
   println("%5d : %d".fmt(n,perfectShuffle(n)));
}
```

```txt

    8 : 3
   24 : 11
   52 : 8
  100 : 30
 1020 : 1018
 1024 : 10
10000 : 300

```


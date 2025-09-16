+++
title = "Card shuffles"
description = ""
date = 2019-04-17T00:33:33Z
aliases = []
[extra]
id = 19055
[taxonomies]
categories = ["Games", "task"]
tags = []
languages = [
  "c",
  "cpp",
  "csharp",
  "d",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "lua",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ruby",
  "tcl",
  "visual_basic_.net",
  "zkl",
]
+++

## Task

There are many techniques that people use to shuffle [[Playing cards|cards]] for card games. Some are more effective than others.


;Task
Implement the (seemingly) more common techniques of the riffle shuffle and overhand shuffle for   ''n''   iterations.


Implementing playing cards is not necessary if it would be easier to implement these shuffling methods for generic collections.

Where possible, compare this to a standard/built-in shuffling procedure.

One iteration of the riffle shuffle is defined as:
# Split the deck into two piles
# Merge the two piles by taking one card from the top of either pile in proportion to the number of cards remaining in the pile. To start with the probability for both piles will be 26/52 (50-50), then 25/51-26/51 etc etc as the riffle progresses.
# The merged deck is now the new "shuffled" deck


One iteration of the overhand shuffle is defined as:
# Take a group of consecutive cards from the top of the deck. For our purposes up to 20% of the deck seems like a good amount.
# Place that group on top of a second pile
# Repeat these steps until there are no cards remaining in the original deck
# The second pile is now the new "shuffled" deck


;Bonus
Implement other methods described in the Wikipedia
article:   [https://en.wikipedia.org/wiki/Shuffling#Shuffling_techniques card shuffling].

Allow for "human errors" of imperfect cutting and interleaving.





## C

{{trans|Modula-2}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

void init() {
    srand((unsigned int)time(NULL));
}

int random(int low, int high) {
    int diff, val;

    diff = high - low;
    if (diff == 0) {
        return low;
    }

    val = rand() % diff;
    return val + low;
}

void initDeck(int *deck, const int size) {
    int i;
    for (i = 0; i < size; ++i) {
        *deck++ = i + 1;
    }
}

void writeDeck(const int *deck, const int size) {
    int i;

    printf("[");
    if (size > 0) {
        printf("%d", *deck++);
    }
    for (i = 1; i < size; ++i) {
        printf(", %d", *deck++);
    }
    printf("]");
}

void riffleShuffle(int * const deck, const int size, int flips) {
    int n, cutPoint, nlp, lp, rp, bound;
    int *nl;

    nl = (int *)malloc(size * sizeof(int));

    for (n = 0; n < flips; ++n) {
        cutPoint = size / 2;
        if (random(0, 2) > 0) {
            cutPoint = cutPoint + random(0, size / 10);
        } else {
            cutPoint = cutPoint - random(0, size / 10);
        }

        nlp = 0;
        lp = 0;
        rp = cutPoint;

        while (lp < cutPoint && rp < size) {
            /* Allow for an imperfect riffling so that more than one card can come from the same side in a row
               biased towards the side with more cards. Remove the IF statement for perfect riffling. */
            bound = (cutPoint - lp) * 50 / (size - rp);
            if (random(0, 50) >= bound) {
                nl[nlp++] = deck[rp++];
            } else {
                nl[nlp++] = deck[lp++];
            }
        }
        while (lp < cutPoint) {
            nl[nlp++] = deck[lp++];
        }
        while (rp < size) {
            nl[nlp++] = deck[rp++];
        }

        memcpy(deck, nl, size * sizeof(int));
    }

    free(nl);
}

void overhandShuffle(int * const mainHand, const int size, int passes) {
    int n, cutSize, mp, op, tp, i;
    int *otherHand, *temp;

    otherHand = (int *)malloc(size * sizeof(int));
    temp = (int *)malloc(size * sizeof(int));

    for (n = 0; n < passes; ++n) {
        mp = 0;
        op = 0;
        tp = 0;

        while (mp < size) {
            cutSize = random(0, size / 5) + 1;

            /* grab the next cut up to the end of the cards left in the main hand */
            for (i = 0; i < cutSize && mp < size; ++i) {
                temp[tp++] = mainHand[mp++];
            }

            /* add them to the cards in the other hand, sometimes to the fron sometimes to the back */
            if (random(0, 10) >= 1) {
                /* front most of the time */

                /* move the elements of other hand forward to make room for temp */
                for (i = op - 1; i >= 0; --i) {
                    otherHand[i + tp] = otherHand[i];
                }

                /* copy temp to the front of other hand */
                memcpy(otherHand, temp, tp * sizeof(int));
                op += tp;
                tp = 0;
            } else {
                /* end sometimes */
                for (i = 0; i < tp; ++i, ++op) {
                    otherHand[op] = temp[i];
                }
                tp = 0;
            }
        }

        /* move the cards back to the main hand */
        memcpy(mainHand, otherHand, size * sizeof(int));
    }

    free(otherHand);
    free(temp);
}

#define SIZE 20
int main() {
    int deck[SIZE];

    init();

    printf("Riffle shuffle\n");
    initDeck(deck, SIZE);
    writeDeck(deck, SIZE);
    printf("\n");
    riffleShuffle(deck, SIZE, 10);
    writeDeck(deck, SIZE);
    printf("\n\n");

    printf("Riffle shuffle\n");
    initDeck(deck, SIZE);
    writeDeck(deck, SIZE);
    printf("\n");
    riffleShuffle(deck, SIZE, 1);
    writeDeck(deck, SIZE);
    printf("\n\n");

    printf("Overhand shuffle\n");
    initDeck(deck, SIZE);
    writeDeck(deck, SIZE);
    printf("\n");
    overhandShuffle(deck, SIZE, 10);
    writeDeck(deck, SIZE);
    printf("\n\n");

    printf("Overhand shuffle\n");
    initDeck(deck, SIZE);
    writeDeck(deck, SIZE);
    printf("\n");
    overhandShuffle(deck, SIZE, 1);
    writeDeck(deck, SIZE);
    printf("\n\n");

    return 0;
}
```

{{out}}

```txt
Riffle shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[1, 15, 6, 2, 11, 18, 9, 5, 3, 4, 7, 16, 13, 8, 10, 14, 19, 12, 17, 20]

Riffle shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[1, 2, 11, 3, 4, 5, 12, 13, 6, 7, 14, 8, 15, 16, 9, 17, 10, 18, 19, 20]

Overhand shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[2, 19, 4, 10, 11, 8, 12, 7, 6, 3, 16, 14, 18, 1, 5, 13, 9, 15, 17, 20]

Overhand shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[20, 17, 18, 19, 14, 15, 16, 10, 11, 12, 13, 9, 8, 7, 5, 6, 1, 2, 3, 4]
```



## C++


```cpp

#include <time.h>
#include <algorithm>
#include <iostream>
#include <string>
#include <deque>


class riffle
{
public:
    void shuffle( std::deque<int>* v, int tm )
    {
        std::deque<int> tmp;
	bool fl;
	size_t len;
	std::deque<int>::iterator it;

	copyTo( v, &tmp );

	for( int t = 0; t < tm; t++ )
	{
	    std::deque<int> lHand( rand() % ( tmp.size() / 3 ) + ( tmp.size() >> 1 ) ), rHand( tmp.size() - lHand.size() );

	    std::copy( tmp.begin(), tmp.begin() + lHand.size(), lHand.begin() );
	    std::copy( tmp.begin() + lHand.size(), tmp.end(), rHand.begin() );
	    tmp.clear();

	    while( lHand.size() && rHand.size() )
	    {
		fl = rand() % 10 < 5;
		if( fl )
    		    len = 1 + lHand.size() > 3 ? rand() % 3 + 1 : rand() % ( lHand.size() ) + 1;
		else
		    len = 1 + rHand.size() > 3 ? rand() % 3 + 1 : rand() % ( rHand.size() ) + 1;

		while( len )
		{
		    if( fl )
		    {
			tmp.push_front( *lHand.begin() );
			lHand.erase( lHand.begin() );
		    }
		    else
		    {
			tmp.push_front( *rHand.begin() );
			rHand.erase( rHand.begin() );
		    }
		    len--;
		}
	    }

	    if( lHand.size() < 1 )
	    {
		for( std::deque<int>::iterator x = rHand.begin(); x != rHand.end(); x++ )
		    tmp.push_front( *x );
	    }
	    if( rHand.size() < 1 )
	    {
		for( std::deque<int>::iterator x = lHand.begin(); x != lHand.end(); x++ )
		    tmp.push_front( *x );
	    }
	}
	copyTo( &tmp, v );
    }
private:
    void copyTo( std::deque<int>* a, std::deque<int>* b )
    {
	for( std::deque<int>::iterator x = a->begin(); x != a->end(); x++ )
	    b->push_back( *x );
	a->clear();
    }
};

class overhand
{
public:
    void shuffle( std::deque<int>* v, int tm )
    {
	std::deque<int> tmp;
	bool top;
	for( int t = 0; t < tm; t++ )
	{
	    while( v->size() )
	    {
		size_t len = rand() % ( v->size() ) + 1;
		top = rand() % 10 < 5;
		while( len )
		{
		    if( top ) tmp.push_back( *v->begin() );
		    else tmp.push_front( *v->begin() );
		    v->erase( v->begin() );
		    len--;
		}
	    }
	    for( std::deque<int>::iterator x = tmp.begin(); x != tmp.end(); x++ )
		v->push_back( *x );

	    tmp.clear();
	}
    }
};

// global - just to make things simpler ---------------------------------------------------
std::deque<int> cards;

void fill()
{
    cards.clear();
    for( int x = 0; x < 20; x++ )
	cards.push_back( x + 1 );
}

void display( std::string t )
{
    std::cout << t << "\n";
    for( std::deque<int>::iterator x = cards.begin(); x != cards.end(); x++ )
	std::cout << *x << " ";
    std::cout << "\n\n";
}

int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned>( time( NULL ) ) );
    riffle r; overhand o;

    fill(); r.shuffle( &cards, 10 ); display( "RIFFLE" );
    fill(); o.shuffle( &cards, 10 ); display( "OVERHAND" );
    fill(); std::random_shuffle( cards.begin(), cards.end() ); display( "STD SHUFFLE" );

    return 0;
}

```

{{out}}

```txt

RIFFLE
18 9 17 20 3 4 16 8 7 10 5 14 12 1 13 19 2 11 15 6

OVERHAND
2 13 12 11 10 9 18 17 6 5 4 3 7 20 19 15 8 14 16 1

STD SHUFFLE
14 4 17 3 12 5 19 6 20 2 16 11 8 15 7 13 10 18 9 1

```


## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CardShuffles {
    public static class Helper {
        public static string AsString<T>(this ICollection<T> c) {
            StringBuilder sb = new StringBuilder("[");
            sb.Append(string.Join(", ", c));
            return sb.Append("]").ToString();
        }
    }

    class Program {
        private static Random rand = new Random();

        public static List<T> riffleShuffle<T>(ICollection<T> list, int flips) {
            List<T> newList = new List<T>(list);

            for (int n = 0; n < flips; n++) {
                //cut the deck at the middle +/- 10%, remove the second line of the formula for perfect cutting
                int cutPoint = newList.Count / 2
                    + (rand.Next(0, 2) == 0 ? -1 : 1) * rand.Next((int)(newList.Count * 0.1));

                //split the deck
                List<T> left = new List<T>(newList.Take(cutPoint));
                List<T> right = new List<T>(newList.Skip(cutPoint));

                newList.Clear();

                while (left.Count > 0 && right.Count > 0) {
                    //allow for imperfect riffling so that more than one card can come form the same side in a row
                    //biased towards the side with more cards
                    //remove the if and else and brackets for perfect riffling
                    if (rand.NextDouble() >= ((double)left.Count / right.Count) / 2) {
                        newList.Add(right.First());
                        right.RemoveAt(0);
                    }
                    else {
                        newList.Add(left.First());
                        left.RemoveAt(0);
                    }
                }

                //if either hand is out of cards then flip all of the other hand to the shuffled deck
                if (left.Count > 0) newList.AddRange(left);
                if (right.Count > 0) newList.AddRange(right);
            }

            return newList;
        }

        public static List<T> overhandShuffle<T>(List<T> list, int passes) {
            List<T> mainHand = new List<T>(list);

            for (int n = 0; n < passes; n++) {
                List<T> otherHand = new List<T>();

                while (mainHand.Count>0) {
                    //cut at up to 20% of the way through the deck
                    int cutSize = rand.Next((int)(list.Count * 0.2)) + 1;

                    List<T> temp = new List<T>();

                    //grab the next cut up to the end of the cards left in the main hand
                    for (int i = 0; i < cutSize && mainHand.Count > 0; i++) {
                        temp.Add(mainHand.First());
                        mainHand.RemoveAt(0);
                    }

                    //add them to the cards in the other hand, sometimes to the front sometimes to the back
                    if (rand.NextDouble()>=0.1) {
                        //front most of the time
                        temp.AddRange(otherHand);
                        otherHand = temp;
                    }
                    else {
                        //end sometimes
                        otherHand.AddRange(temp);
                    }
                }

                //move the cards back to the main hand
                mainHand = otherHand;
            }

            return mainHand;
        }

        static void Main(string[] args) {
            List<int> list = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
            Console.WriteLine(list.AsString());
            list = riffleShuffle(list, 10);
            Console.WriteLine(list.AsString());
            Console.WriteLine();

            list = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
            Console.WriteLine(list.AsString());
            list = riffleShuffle(list, 1);
            Console.WriteLine(list.AsString());
            Console.WriteLine();

            list = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
            Console.WriteLine(list.AsString());
            list = overhandShuffle(list, 10);
            Console.WriteLine(list.AsString());
            Console.WriteLine();

            list = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
            Console.WriteLine(list.AsString());
            list = overhandShuffle(list, 1);
            Console.WriteLine(list.AsString());
            Console.WriteLine();
        }
    }
}
```

{{out}}

```txt
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[9, 2, 8, 3, 20, 15, 1, 13, 7, 18, 5, 16, 4, 19, 10, 6, 12, 14, 11, 17]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[12, 1, 2, 3, 4, 13, 14, 5, 15, 16, 6, 7, 17, 8, 18, 19, 9, 10, 20, 11]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[13, 18, 12, 17, 10, 9, 19, 11, 16, 15, 6, 8, 14, 1, 3, 2, 5, 4, 7, 20]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[20, 17, 18, 19, 15, 16, 11, 12, 13, 14, 10, 7, 8, 9, 5, 6, 2, 3, 4, 1]
```



## D

{{trans|Java}}

```D
import std.container.array;
import std.random;
import std.range;
import std.stdio;

auto riffleShuffle(T)(T[] list, int flips) {
    auto newList = Array!T(list);

    for (int n=0; n<flips; n++) {
        //cut the deck at the middle +/- 10%, remove the second line of the formula for perfect cutting
        int cutPoint = newList.length / 2
                     + choice([-1, 1]) * uniform!"[]"(0, newList.length / 10);

        //split the deck
        auto left = newList[0..cutPoint];
        auto right = newList[cutPoint..$];

        newList.clear();

        while (left.length > 0 && right.length > 0) {
            //allow for imperfect riffling so that more than one card can come form the same side in a row
            //biased towards the side with more cards
            //remove the if and else and brackets for perfect riffling
            if (uniform01() >= (cast(real) left.length / right.length) / 2) {
                newList.insertAfter(newList[], right.front);
                right.popFront();
            } else {
                newList.insertAfter(newList[], left.front);
                left.popFront();
            }
        }

        //if either hand is out of cards then flip all of the other hand to the shuffled deck
        if (!left.empty) newList ~= left;
        if (!right.empty) newList ~= right;
    }
    return newList.array;
}

auto overhandShuffle(T)(T[] list, int passes) {
    auto mainHand = Array!T(list);

    for (int n=0; n<passes; n++) {
        Array!T otherHand;

        while (mainHand.length > 0) {
            //cut at up to 20% of the way through the deck
            int cutSize = uniform!"[]"(0, list.length / 5) + 1;

            Array!T temp;

            //grab the next cut up to the end of the cards left in the main hand
            for (int i=0; i<cutSize && mainHand.length>0; i++) {
                temp ~= mainHand[0];
                mainHand.linearRemove(mainHand[0..1]);
            }

            //add them to the cards in the other hand, sometimes to the front sometimes to the back
            if (uniform01() >= 0.1) {
                //front most of the time
                otherHand = temp ~ otherHand;
            } else {
                //end sometimes
                otherHand ~= temp;
            }
        }

        //move the cards back to the main hand
        mainHand = otherHand;
    }
    return mainHand.array;
}

void main() {
    auto list = iota(1,21).array;
    writeln(list);
    list = riffleShuffle(list, 10);
    writeln(list);
    writeln();

    list = iota(1,21).array;
    writeln(list);
    list = riffleShuffle(list, 1);
    writeln(list);
    writeln();

    list = iota(1,21).array;
    writeln(list);
    list = overhandShuffle(list, 10);
    writeln(list);
    writeln();

    list = iota(1,21).array;
    writeln(list);
    list = overhandShuffle(list, 1);
    writeln(list);
    writeln();

    list = iota(1,21).array;
    writeln(list);
    list.randomShuffle();
    writeln(list);
}
```

{{out}}

```txt
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[7, 2, 4, 13, 19, 12, 9, 20, 6, 5, 17, 18, 1, 16, 3, 10, 14, 11, 8, 15]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[11, 12, 1, 2, 3, 4, 13, 5, 14, 15, 16, 6, 7, 17, 18, 8, 9, 19, 10, 20]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[20, 4, 1, 19, 13, 8, 17, 10, 15, 12, 6, 7, 2, 11, 9, 16, 18, 3, 5, 14]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[18, 19, 20, 16, 17, 11, 12, 13, 14, 15, 7, 8, 9, 10, 4, 5, 6, 1, 2, 3]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[10, 17, 19, 13, 5, 12, 1, 2, 14, 4, 9, 16, 7, 3, 15, 20, 8, 11, 6, 18]
```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func reverse(s []int) {
    for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
        s[i], s[j] = s[j], s[i]
    }
}

func riffle(deck []int, iterations int) []int {
    le := len(deck)
    pile := make([]int, le)
    copy(pile, deck)
    for i := 0; i < iterations; i++ {
        mid := le / 2
        tenpc := mid / 10
        // choose a random number within 10% of midpoint
        cut := mid - tenpc + rand.Intn(2*tenpc+1)
        // split deck into two at cut point
        deck1 := make([]int, cut)
        deck2 := make([]int, le-cut)
        copy(deck1, pile[:cut])
        copy(deck2, pile[cut:])
        pile = pile[:0]
        fromTop := rand.Intn(2) // choose to draw from top (1) or bottom (0)
        for len(deck1) > 0 && len(deck2) > 0 {
            if fromTop == 1 {
                el1 := deck1[0]
                deck1 = deck1[1:]
                el2 := deck2[0]
                deck2 = deck2[1:]
                pile = append(pile, el1, el2)
            } else {
                el1 := deck1[len(deck1)-1]
                deck1 = deck1[:len(deck1)-1]
                el2 := deck2[len(deck2)-1]
                deck2 = deck2[:len(deck2)-1]
                pile = append(pile, el1, el2)
            }
        }
        // add any remaining cards to the pile and reverse it
        if len(deck1) > 0 {
            pile = append(pile, deck1...)
        } else if len(deck2) > 0 {
            pile = append(pile, deck2...)
        }
        reverse(pile) // as pile is upside down
    }
    return pile
}

func overhand(deck []int, iterations int) []int {
    le := len(deck)
    pile := make([]int, le)
    copy(pile, deck)
    pile2 := make([]int, 0)
    twentypc := le / 5
    for i := 0; i < iterations; i++ {
        for len(pile) > 0 {
            cards := 1 + rand.Intn(twentypc)
            if cards > len(pile) {
                cards = len(pile)
            }
            temp := make([]int, cards)
            copy(temp, pile[:cards])
            pile2 = append(temp, pile2...)
            pile = pile[cards:]
        }
        pile = append(pile, pile2...)
        pile2 = pile2[:0]
    }
    return pile
}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println("Starting deck:")
    deck := make([]int, 20)
    for i := 0; i < 20; i++ {
        deck[i] = i + 1
    }
    fmt.Println(deck)
    const iterations = 10
    fmt.Println("\nRiffle shuffle with", iterations, "iterations:")
    fmt.Println(riffle(deck, iterations))
    fmt.Println("\nOverhand shuffle with", iterations, "iterations:")
    fmt.Println(overhand(deck, iterations))
    fmt.Println("\nStandard library shuffle with 1 iteration:")
    rand.Shuffle(len(deck), func(i, j int) {
        deck[i], deck[j] = deck[j], deck[i] // shuffles deck in place
    })
    fmt.Println(deck)
}
```


{{out}}
Sample output:

```txt

Starting deck:
[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20]

Riffle shuffle with 10 iterations:
[19 12 4 7 15 3 13 11 2 8 18 9 10 16 17 14 20 6 1 5]

Overhand shuffle with 10 iterations:
[5 7 9 4 13 3 15 11 2 8 18 12 1 17 16 6 14 19 10 20]

Standard library shuffle with 1 iteration:
[15 16 10 4 7 2 6 18 14 13 17 1 12 3 11 8 19 20 5 9]

```



## J

{{eff note|J|({~ ?~@#)}}


```J
NB. overhand cut
overhand=: (\: [: +/\ %@%:@# > # ?@# 0:)@]^:[

NB. Gilbert–Shannon–Reeds model
riffle=:  (({.~+/)`(I.@])`(-.@]#inv (}.~+/))} ?@(#&2)@#)@]^:[
```


The probability of a cut occurring between each pair of cards in this overhand shuffle is proportional to the reciprocal of the square root of the number of cards in the deck.

In other words, overhand cut breaks the deck into some number of pieces and reverses the order of those pieces.

Here are some examples of the underlying selection mechanism in action for a deck of 10 cards:


```J
   ([: +/\ %@%:@# > # ?@# 0:) i.10
0 0 0 0 0 0 0 0 1 1
   ([: +/\ %@%:@# > # ?@# 0:) i.10
1 1 2 2 2 3 3 3 3 3
   ([: +/\ %@%:@# > # ?@# 0:) i.10
0 1 1 2 3 3 3 3 4 5
   ([: +/\ %@%:@# > # ?@# 0:) i.10
0 1 1 1 1 2 2 3 3 3
```


The final step of a cut is to sort the deck in descending order based on the numbers we compute this way.

The left argument says how many of these cuts to perform.

Task examples:


```J
   1 riffle i.20
0 1 2 3 4 5 6 7 8 13 14 9 15 16 17 10 18 11 12 19
   10 riffle i.20
6 10 13 8 2 14 15 9 19 3 18 16 11 1 12 17 5 4 0 7
   1 overhand i.20
17 18 19 13 14 15 16 4 5 6 7 8 9 10 11 12 0 1 2 3
   10 overhand i.20
15 11 2 4 5 12 16 10 17 19 9 8 6 13 3 18 7 1 0 14
```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

public class CardShuffles{

	private static final Random rand = new Random();

	public static <T> LinkedList<T> riffleShuffle(List<T> list, int flips){
		LinkedList<T> newList = new LinkedList<T>();

		newList.addAll(list);

		for(int n = 0; n < flips; n++){
			//cut the deck at the middle +/- 10%, remove the second line of the formula for perfect cutting
			int cutPoint = newList.size() / 2
				+ (rand.nextBoolean() ? -1 : 1 ) * rand.nextInt((int)(newList.size() * 0.1));

			//split the deck
			List<T> left = new LinkedList<T>();
			left.addAll(newList.subList(0, cutPoint));
			List<T> right = new LinkedList<T>();
			right.addAll(newList.subList(cutPoint, newList.size()));

			newList.clear();

			while(left.size() > 0 && right.size() > 0){
				//allow for imperfect riffling so that more than one card can come form the same side in a row
				//biased towards the side with more cards
				//remove the if and else and brackets for perfect riffling
				if(rand.nextDouble() >= ((double)left.size() / right.size()) / 2){
					newList.add(right.remove(0));
				}else{
					newList.add(left.remove(0));
				}
			}

			//if either hand is out of cards then flip all of the other hand to the shuffled deck
			if(left.size() > 0) newList.addAll(left);
			if(right.size() > 0) newList.addAll(right);
		}
		return newList;
	}

	public static <T> LinkedList<T> overhandShuffle(List<T> list, int passes){
		LinkedList<T> mainHand = new LinkedList<T>();

		mainHand.addAll(list);
		for(int n = 0; n < passes; n++){
			LinkedList<T> otherHand = new LinkedList<T>();

			while(mainHand.size() > 0){
				//cut at up to 20% of the way through the deck
				int cutSize = rand.nextInt((int)(list.size() * 0.2)) + 1;

				LinkedList<T> temp = new LinkedList<T>();

				//grab the next cut up to the end of the cards left in the main hand
				for(int i = 0; i < cutSize && mainHand.size() > 0; i++){
					temp.add(mainHand.remove());
				}

				//add them to the cards in the other hand, sometimes to the front sometimes to the back
				if(rand.nextDouble()  >= 0.1){
					//front most of the time
					otherHand.addAll(0, temp);
				}else{
					//end sometimes
					otherHand.addAll(temp);
				}
			}

			//move the cards back to the main hand
			mainHand = otherHand;
		}
		return mainHand;
	}

	public static void main(String[] args){
		List<Integer> list = Arrays.asList(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
		System.out.println(list);
		list = riffleShuffle(list, 10);
		System.out.println(list + "\n");

                list = Arrays.asList(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
		System.out.println(list);
		list = riffleShuffle(list, 1);
		System.out.println(list + "\n");

		list = Arrays.asList(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
		System.out.println(list);
		list = overhandShuffle(list, 10);
		System.out.println(list + "\n");

                list = Arrays.asList(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
		System.out.println(list);
		list = overhandShuffle(list, 1);
		System.out.println(list + "\n");

		list = Arrays.asList(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20);
		System.out.println(list);
		Collections.shuffle(list);
		System.out.println(list + "\n");
	}
}
```

{{out}}

```txt
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[20, 11, 1, 9, 15, 4, 19, 16, 8, 13, 7, 2, 14, 12, 10, 3, 17, 18, 6, 5]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[1, 12, 2, 3, 4, 5, 13, 14, 15, 6, 16, 7, 8, 9, 17, 18, 10, 19, 20, 11]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[20, 3, 10, 4, 2, 8, 1, 18, 13, 19, 14, 6, 9, 12, 16, 15, 5, 7, 11, 17]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[18, 19, 20, 17, 13, 14, 15, 16, 9, 10, 11, 12, 8, 6, 7, 3, 4, 5, 1, 2]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[18, 12, 13, 14, 2, 3, 15, 5, 9, 19, 7, 11, 1, 6, 4, 20, 16, 17, 10, 8]
```



## Julia

{{works with|Julia|0.6}}


```julia
function riffleshuffle!(list::Vector, flips::Integer)
    len = length(list)
    # pre-allocate the left and right part for efficiency
    llist = similar(list, len÷2 + fld(len, 10))
    rlist = similar(list, len÷2 + fld(len, 10))
    for _ in Base.OneTo(flips)
        # cut the deck at the middle +/- 10%,
        # remove the second line of the formula for perfect cutting
        cut = len ÷ 2 + rand(-1:2:1) * rand(0:fld(len, 10))

        # split the deck and copy it to left and right
        copy!(llist, 1, list, 1, cut)
        copy!(rlist, 1, list, cut + 1, len - cut)

        ind, indl, indr = len, cut, len - cut
        while indl ≥ 1 && indr ≥ 1
            if rand() < indl / 2indr
                list[ind] = llist[indl]
                indl -= 1
            else
                list[ind] = rlist[indr]
                indr -= 1
            end
            ind -= 1
        end

        copy!(list, 1, rlist, 1, indr)
        copy!(list, 1, llist, 1, indl)
    end
    return list
end

function overhandshuffle!(list::Vector, passes::Integer)
    len = length(list)
    otherhand = similar(list)
    for _ in Base.OneTo(passes)
        ind = 1
        while ind ≤ endof(list)
            chklen = min(rand(1:cld(len, 5)), len - ind + 1)
            copy!(otherhand, ind, list, len - ind - chklen + 2, chklen)
            ind += chklen
        end
        list .= otherhand
    end
    return list
end

v = collect(1:20)
println("# Riffle shuffle (1):\n", v)
println(" -> ", riffleshuffle!(v, 1), "\n")

v = collect(1:20)
println("# Riffle shuffle (10):\n", v)
println(" -> ", riffleshuffle!(v, 10), "\n")

v = collect(1:20)
println("# Overhand shuffle (1):\n", v)
println(" -> ", overhandshuffle!(v, 1), "\n")

v = collect(1:20)
println("# Overhand shuffle (10):\n", v)
println(" -> ", overhandshuffle!(v, 10), "\n")

v = collect(1:20)
println("# Default shuffle:\n", v)
println(" -> ", shuffle!(v), "\n")
```


{{out}}

```txt
# Riffle shuffle (1):
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
 -> [11, 1, 12, 2, 3, 13, 14, 4, 5, 15, 6, 16, 17, 7, 8, 18, 9, 10, 19, 20]

# Riffle shuffle (10):
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
 -> [13, 11, 7, 2, 12, 1, 3, 16, 19, 5, 4, 14, 9, 10, 18, 15, 6, 17, 8, 20]

# Overhand shuffle (1):
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
 -> [19, 20, 17, 18, 15, 16, 14, 13, 10, 11, 12, 6, 7, 8, 9, 2, 3, 4, 5, 1]

# Overhand shuffle (10):
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
 -> [2, 8, 12, 4, 1, 5, 7, 11, 17, 6, 14, 19, 3, 9, 10, 15, 18, 13, 16, 20]

# Default shuffle:
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
 -> [3, 11, 18, 14, 2, 12, 13, 4, 10, 19, 8, 16, 20, 5, 1, 6, 9, 15, 17, 7]
```



## Kotlin


```scala
// version 1.1.51

import java.util.Random
import java.util.Collections.shuffle

val r = Random()

fun riffle(deck: List<Int>, iterations: Int): List<Int> {
    val pile = deck.toMutableList()

    repeat(iterations) {
        val mid = deck.size / 2
        val tenpc = mid / 10
        // choose a random number within 10% of midpoint
        val cut = mid - tenpc + r.nextInt(2 * tenpc + 1)
        // split deck into two at cut point
        val deck1 = pile.take(cut).toMutableList()
        val deck2 = pile.drop(cut).toMutableList()
        pile.clear()
        val fromTop = r.nextBoolean() // choose to draw from top or bottom
        while (deck1.size > 0 && deck2.size > 0) {
            if (fromTop) {
                pile.add(deck1.removeAt(0))
                pile.add(deck2.removeAt(0))
            }
            else {
                pile.add(deck1.removeAt(deck1.lastIndex))
                pile.add(deck2.removeAt(deck2.lastIndex))
            }
        }
        // add any remaining cards to the pile and reverse it
        if (deck1.size > 0) pile.addAll(deck1)
        else if (deck2.size > 0) pile.addAll(deck2)
        pile.reverse() // as pile is upside down
    }
    return pile
}

fun overhand(deck: List<Int>, iterations: Int): List<Int> {
    val pile = deck.toMutableList()
    val pile2 = mutableListOf<Int>()
    val twentypc = deck.size / 5
    repeat(iterations) {
        while (pile.size > 0) {
            val cards = minOf(pile.size, 1 + r.nextInt(twentypc))
            pile2.addAll(0, pile.take(cards))
            repeat(cards) { pile.removeAt(0) }
        }
        pile.addAll(pile2)
        pile2.clear()
    }
    return pile
}

fun main(args: Array<String>) {
    println("Starting deck:")
    var deck = List<Int>(20) { it + 1 }
    println(deck)
    val iterations = 10
    println("\nRiffle shuffle with $iterations iterations:")
    println(riffle(deck, iterations))
    println("\nOverhand shuffle with $iterations iterations:")
    println(overhand(deck, iterations))
    println("\nStandard library shuffle with 1 iteration:")
    shuffle(deck) // shuffles deck in place
    println(deck)
}
```


Sample output:

```txt

Starting deck:
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

Riffle shuffle with 10 iterations:
[5, 18, 3, 12, 15, 6, 8, 16, 2, 20, 17, 11, 13, 1, 9, 14, 19, 7, 10, 4]

Overhand shuffle with 10 iterations:
[2, 8, 1, 4, 11, 7, 3, 5, 17, 12, 10, 9, 6, 18, 20, 14, 19, 13, 15, 16]

Standard library shuffle with 1 iteration:
[17, 9, 12, 15, 7, 13, 18, 8, 2, 20, 5, 10, 16, 6, 14, 4, 19, 3, 11, 1]

```



## Lua


```lua
-- Return a table respresenting a standard deck of cards in order
function newDeck ()
    local cards, suits = {}, {"C", "D", "H", "S"}
    for _, suit in pairs(suits) do
        for value = 2, 14 do
            if value == 10 then value = "T" end
            if value == 11 then value = "J" end
            if value == 12 then value = "Q" end
            if value == 13 then value = "K" end
            if value == 14 then value = "A" end
            table.insert(cards, value .. suit)
        end
    end
    return cards
end

-- Display all cards (strings) in a given deck (table)
function show (deck)
    for _, card in pairs(deck) do io.write(card .. " ") end
    print("\n")
end

-- Perform a riffle shuffle on deck and return it as a new table
function riffle (deck)
    local pile1, pile2, pos = {}, {}, 1
    for i, card in ipairs(deck) do
        if i < math.ceil(#deck / 2) + 1 then
            table.insert(pile1, card)
        else
            table.insert(pile2, card)
        end
    end
    deck = {}
    while pile2[pos] do
        table.insert(deck, pile1[pos])
        table.insert(deck, pile2[pos])
        pos = pos + 1
    end
    return deck
end

-- Perform an overhand shuffle on a deck and return it as a new table
function overhand (deck)
    local newDeck, twentyPercent, groupSize, pos = {}, math.floor(#deck / 5)
    repeat
        repeat
            groupSize = math.random(twentyPercent)
        until groupSize <= #deck
        for pos = #deck - groupSize, #deck do
            table.insert(newDeck, deck[pos])
            deck[pos] = nil
        end
    until #deck == 0
    return newDeck
end

-- Main procedure
math.randomseed(os.time())
local deck1, deck2 = newDeck(), newDeck()
deck1 = riffle(deck1)
print("Sorted deck after one riffle shuffle:")
show(deck1)
deck2 = overhand(deck2)
print("Sorted deck after one overhand shuffle:")
show(deck2)
```

{{out}}

```txt
Sorted deck after one riffle shuffle:
2C 2H 3C 3H 4C 4H 5C 5H 6C 6H 7C 7H 8C 8H 9C 9H TC TH JC JH QC QH KC KH AC AH 2D
 2S 3D 3S 4D 4S 5D 5S 6D 6S 7D 7S 8D 8S 9D 9S TD TS JD JS QD QS KD KS AD AS

Sorted deck after one overhand shuffle:
QS KS AS 3S 4S 5S 6S 7S 8S 9S TS JS JH QH KH AH 2S 4H 5H 6H 7H 8H 9H TH 2H 3H 4D
 5D 6D 7D 8D 9D TD JD QD KD AD QC KC AC 2D 3D 4C 5C 6C 7C 8C 9C TC JC 2C 3C

```


=={{header|Modula-2}}==

```modula2
MODULE CardShuffles;
FROM FormatString IMPORT FormatString;
FROM RandomNumbers IMPORT Random;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteCard(c : CARDINAL);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%c", buf, c);
    WriteString(buf)
END WriteCard;

PROCEDURE WriteInteger(i : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%02i", buf, i);
    WriteString(buf)
END WriteInteger;

PROCEDURE WriteIntArray(array : ARRAY OF INTEGER);
VAR i : CARDINAL;
BEGIN
    WriteString("[");
    FOR i:=0 TO HIGH(array) DO
        IF i>0 THEN
            WriteString(", ");
        END;
        WriteInteger(array[i]);
    END;
    WriteString("]")
END WriteIntArray;

(*---------------------------------------*)

TYPE Deck_t = ARRAY[0..20] OF INTEGER;

PROCEDURE InitDeck(VAR deck : ARRAY OF INTEGER);
VAR i : CARDINAL;
BEGIN
    FOR i:=0 TO HIGH(deck) DO
        deck[i] := i + 1
    END
END InitDeck;

PROCEDURE RiffleShuffle(VAR deck : Deck_t; flips : CARDINAL);
VAR
    n,cutPoint,nlp,lp,rp,bound : CARDINAL;
    nl : Deck_t;
BEGIN
    FOR n:=1 TO flips DO
        cutPoint := HIGH(deck) / 2;
        IF Random(0, 2) > 0 THEN
            cutPoint := cutPoint + Random(0, HIGH(deck) / 10);
        ELSE
            cutPoint := cutPoint - Random(0, HIGH(deck) / 10);
        END;

        nlp := 0;
        lp := 0;
        rp := cutPoint;

        WHILE (lp <= cutPoint) AND (rp < HIGH(deck)) DO
            (* Allow for an imperfect riffling so that more than one card can come from the same side in a row
               biased towards the side with more cards. Remove the IF statement for perfect riffling. *)
            bound := (cutPoint - lp) * 50 / (HIGH(deck) - rp);
            IF Random(0, 50)>= bound THEN
                nl[nlp] := deck[rp];
                INC(nlp);
                INC(rp);
            ELSE
                nl[nlp] := deck[lp];
                INC(nlp);
                INC(lp);
            END
        END;
        WHILE lp <= cutPoint DO
            nl[nlp] := deck[lp];
            INC(nlp);
            INC(lp);
        END;
        WHILE rp < HIGH(deck) DO
            nl[nlp] := deck[rp];
            INC(nlp);
            INC(rp);
        END;

        deck := nl
    END
END RiffleShuffle;

PROCEDURE OverhandShuffle(VAR mainHand : Deck_t; passes : CARDINAL);
VAR
    n,cutSize,mp,op,tp,i : CARDINAL;
    otherHand,temp : Deck_t;
BEGIN
    FOR n:=1 TO passes DO
        mp := 0;
        op := 0;
        FOR i:=0 TO HIGH(otherHand) DO
            otherHand[i] := 9999
        END;

        WHILE mp < HIGH(mainHand) DO
            (* Cut at up to 20% of the way through the deck *)
            cutSize := Random(0, HIGH(mainHand) / 5) + 1;
            tp := 0;

            (* Grab the next cut up to the end of the cards left in the main hand *)
            i:=0;
            WHILE (i < cutSize) AND (mp < HIGH(mainHand)) DO
                temp[tp] := mainHand[mp];
                INC(tp);
                INC(mp);
                INC(i);

                IF mp = HIGH(mainHand) THEN
                    temp[tp] := mainHand[mp];
                    INC(tp);
                    INC(mp);
                END
            END;

            (* Add them to the cards in the other hand, sometimes to the front and sometimes to the back *)
            IF Random(0, 10) >= 1 THEN
                (* otherHand = temp + otherHand *)

                (* copy other hand elements up by temp spaces *)
                i := op;
                WHILE (i > 0) AND (op > 0) DO
                    otherHand[tp + i] := otherHand[i];
                    DEC(i)
                END;
                IF op > 0 THEN
                    otherHand[tp] := otherHand[0]
                END;

                (* copy the elements of temp into the front of other hand *)
                FOR i:=0 TO tp-1 DO
                    otherHand[i] := temp[i]
                END
           ELSE
                (* otherHand = otherHand + temp *)
                FOR i:=0 TO tp DO
                    otherHand[op+i] := temp[i]
                END
            END;
            op := op + tp
        END;

        (* Move the cards back to the main hand *)
        mainHand := otherHand
    END
END OverhandShuffle;

(* Main *)
VAR deck : Deck_t;
BEGIN
    WriteString("Riffle shuffle");
    WriteLn;
    InitDeck(deck);
    WriteIntArray(deck);
    WriteLn;
    RiffleShuffle(deck, 10);
    WriteIntArray(deck);
    WriteLn;
    WriteLn;

    WriteString("Riffle shuffle");
    WriteLn;
    InitDeck(deck);
    WriteIntArray(deck);
    WriteLn;
    RiffleShuffle(deck, 1);
    WriteIntArray(deck);
    WriteLn;
    WriteLn;

    WriteString("Overhand shuffle");
    WriteLn;
    InitDeck(deck);
    WriteIntArray(deck);
    WriteLn;
    OverhandShuffle(deck, 10);
    WriteIntArray(deck);
    WriteLn;
    WriteLn;

    WriteString("Overhand shuffle");
    WriteLn;
    InitDeck(deck);
    WriteIntArray(deck);
    WriteLn;
    OverhandShuffle(deck, 1);
    WriteIntArray(deck);
    WriteLn;

    ReadChar;
END CardShuffles.
```



## PARI/GP

Riffle shuffle:

```parigp
riffle(v)=
{
  my(n=#v,k,t,deck=vector(n),left,right);
  t=random(2^n);
  for(i=0,n,
    t -= binomial(n,i);
    if(t<0, k=i; break)
  );
  if(k==0||k==n, return(v));
  left=k;
  right=n-k;
  deck=vector(n,i,
    t=random(n+1-i);
    v[if(t<left, k-left--, n-right--)]
  );
  vecextract(v, deck);
}
addhelp(riffle, "riffle(v): Riffle shuffles the vector v, following the Gilbert-Shannon-Reeds model.");
```


Overhand shuffle:

```parigp
overhand(v)=
{
  my(u=[],t,n=2*#v\5);
  while(#v,
    t=min(random(n)+1,#v);
    u=concat(v[1..t],u);
    v=if(t<#v,v[t+1..#v],[]);
  );
  u;
}
addhelp(overhand, "overhand(v): Overhand shuffles the vector v.");
```


Usage:

```parigp
riffle([1..52])
overhand([1..52])
```

{{out}}

```txt
%1 = [1, 2, 3, 21, 4, 22, 23, 5, 24, 25, 26, 6, 27, 28, 29, 30, 7, 31, 32, 33, 34, 35, 36, 8, 37, 38, 39, 40, 9, 10, 11, 12, 41, 42, 43, 13, 44, 45, 14, 46, 47, 48, 15, 16, 17, 49, 50, 18, 51, 19, 20, 52]
%2 = [44, 45, 46, 47, 48, 49, 50, 51, 52, 43, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 23, 24, 25, 26, 27, 28, 29, 30, 31, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 1, 2, 3, 4]
```



## Perl

Follows the Perl 6 implementation for the overhand shuffle, but uses classic one-liner for riffle.

```perl
sub overhand {
    our @cards; local *cards = shift;
    my(@splits,@shuffle);
    my $x = int +@cards / 5;
    push @splits, (1..$x)[int rand $x] for 1..+@cards;
    while (@cards) {
        push @shuffle, [splice @cards, 0, shift @splits];
    }
    @cards = flatten(reverse @shuffle);
}

sub flatten { map { ref $_ eq 'ARRAY' ? @$_ : $_ } @_ }

sub riffle {
    our @cards; local *cards = shift;
    splice @cards, @cards/2 - $_, 0, pop @cards for 0 .. (@cards/2)-1;
}

@cards = 1..20;
overhand(\@cards) for 1..10;
print join ' ', @cards, "\n";

@cards = 1..20;
riffle(\@cards) for 1..10;
print join ' ', @cards, "\n";
```

{{out}}

```txt
9 11 5 2 4 14 1 3 8 6 15 13 16 12 19 20 7 18 10 17
1 10 19 9 18 8 17 7 16 6 15 5 14 4 13 3 12 2 11 20
```



## Perl 6



```perl6
use v6;

sub overhand ( @cards ) {
    my @splits = roll 10, ^( @cards.elems div 5 )+1;
    @cards.rotor( @splits  ,:partial ).reverse.flat
}

sub riffle ( @pile is copy ) {
    my @pile2 = @pile.splice: @pile.elems div 2 ;

    roundrobin(
        @pile.rotor(  (1 .. 3).roll(7), :partial ),
        @pile2.rotor( (1 .. 3).roll(9), :partial ),
    ).flat
}

my @cards = ^20;
@cards.=&overhand for ^10;
say @cards;

my @cards2 = ^20;
@cards2.=&riffle for ^10;
say @cards2;

say (^20).pick(*);

```



## Phix


```Phix
function riffle(sequence s)
sequence res = {}
integer l = length(s)
integer r = rand(l)
    for i=1 to l do
        if r+i<=l then
            res &= s[r+i]
        end if
        if i<=r then
            res &= s[i]
        end if
    end for
    return res
end function

function overhand(sequence s)
sequence res = {}
integer l = length(s)
    while length(s) do
        integer r = rand(l*0.2)
        if r>length(s) then
            r = length(s)
        end if
        res = s[1..r]&res
        s = s[r+1..$]
    end while
    return res
end function

-- to shorten the output, all 2..7 have been removed from the deck
constant DECKSIZE=52-24

procedure show_deck(sequence s)
    for i=1 to DECKSIZE do
        integer c = s[i]-1
--      puts(1,"23456789TJQKA"[remainder(c,13)+1]&"HCDS"[floor(c/13)+1]&" ")
        puts(1,"89TJQKA"[remainder(c,7)+1]&"HCDS"[floor(c/7)+1]&" ")
    end for
    puts(1,"\n")
end procedure

show_deck(riffle(tagset(DECKSIZE)))
show_deck(overhand(tagset(DECKSIZE)))
show_deck(shuffle(tagset(DECKSIZE)))
```

{{out}}

```txt

TC 8H JC 9H QC TH KC JH AC QH 8D KH 9D AH TD 8C JD 9C QD KD AD 8S 9S TS JS QS KS AS
KS AS JS QS TS AD 8S 9S 9D TD JD QD KD QC KC AC 8D AH 8C 9C TC JC JH QH KH TH 8H 9H
KH TH AH QH 8D JC QC 8C JH 8H 9D KS TD AS KD 8S TC AD TS AC 9C KC 9H QD JD JS 9S QS

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(de riffle (Lst)
   (let N (/ (setq @@ (length Lst)) 2)
      (conc
         (mapcan list (head N Lst) (tail (- N) Lst))
         (and (bit? 1 @@) (tail 1 Lst)) ) ) )
(de overhand (Lst)
   (let N (/ (* (length Lst) 20) 100)
      (make
         (while (flip (cut N 'Lst))
            (for I @
               (yoke I) ) ) ) ) )

(println 'riffle (riffle (range 1 19)) )
(println 'overhand (overhand (range 1 19)) )
(println 'shuffle (shuffle (range 1 19)) )
```

{{out}}

```txt

riffle (1 10 2 11 3 12 4 13 5 14 6 15 7 16 8 17 9 18 19)
overhand (19 16 17 18 13 14 15 10 11 12 7 8 9 4 5 6 1 2 3)
shuffle (5 3 13 15 17 12 14 11 2 1 19 7 6 9 18 8 10 4 16)

```



## Python

{{trans|D}}

```python
import random

def riffleShuffle(va, flips):
    nl = va
    for n in range(flips):
        #cut the deck at the middle +/- 10%, remove the second line of the formula for perfect cutting
        cutPoint = len(nl)/2 + random.choice([-1, 1]) * random.randint(0, len(va)/10)

        # split the deck
        left = nl[0:cutPoint]
        right = nl[cutPoint:]

        del nl[:]
        while (len(left) > 0 and len(right) > 0):
            #allow for imperfect riffling so that more than one card can come form the same side in a row
            #biased towards the side with more cards
            #remove the if and else and brackets for perfect riffling
            if (random.uniform(0, 1) >= len(left) / len(right) / 2):
                nl.append(right.pop(0))
            else:
                nl.append(left.pop(0))
        if (len(left) > 0):
            nl = nl + left
        if (len(right) > 0):
            nl = nl + right
    return nl

def overhandShuffle(va, passes):
    mainHand = va
    for n in range(passes):
        otherHand = []
        while (len(mainHand) > 0):
            #cut at up to 20% of the way through the deck
            cutSize = random.randint(0, len(va) / 5) + 1
            temp = []

            #grab the next cut up to the end of the cards left in the main hand
            i=0
            while (i<cutSize and len(mainHand) > 0):
                temp.append(mainHand.pop(0))
                i = i + 1

            #add them to the cards in the other hand, sometimes to the front sometimes to the back
            if (random.uniform(0, 1) >= 0.1):
                #front most of the time
                otherHand = temp + otherHand
            else:
                otherHand = otherHand + temp
        #move the cards back to the main hand
        mainHand = otherHand
    return mainHand

print "Riffle shuffle"
nums = [x+1 for x in range(21)]
print nums
print riffleShuffle(nums, 10)
print

print "Riffle shuffle"
nums = [x+1 for x in range(21)]
print nums
print riffleShuffle(nums, 1)
print

print "Overhand shuffle"
nums = [x+1 for x in range(21)]
print nums
print overhandShuffle(nums, 10)
print

print "Overhand shuffle"
nums = [x+1 for x in range(21)]
print nums
print overhandShuffle(nums, 1)
print

print "Library shuffle"
nums = [x+1 for x in range(21)]
print nums
random.shuffle(nums)
print nums
print
```

{{out}}

```txt
Riffle shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]
[4, 16, 5, 19, 3, 14, 2, 9, 20, 13, 17, 10, 6, 7, 1, 18, 12, 11, 8, 21, 15]

Riffle shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]
[13, 14, 15, 1, 16, 2, 3, 17, 4, 5, 18, 6, 7, 19, 8, 9, 20, 10, 11, 21, 12]

Overhand shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]
[21, 12, 5, 16, 7, 2, 15, 14, 20, 6, 8, 11, 13, 1, 4, 17, 19, 9, 3, 18, 10]

Overhand shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]
[21, 20, 19, 18, 16, 17, 14, 15, 11, 12, 13, 4, 5, 6, 7, 1, 2, 3, 8, 9, 10]

Library shuffle
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]
[14, 12, 2, 17, 18, 21, 8, 4, 15, 9, 11, 10, 3, 1, 7, 19, 20, 6, 5, 16, 13]
```



## Racket


These implementations are in <code>typed/racket</code>, which means that additional annotations are needed which looks like hard work.

On the bright side, if you want to add a new <code>Cutter</code> or <code>Riffler</code>, DrRacket will let you know immediately if you're consuming lists of lists of lists at the right depth and in the right quantities.

Racket has a built in <code>shuffle</code> function. Frankly, I'd go with that in your own code!


```racket
#lang typed/racket
;; ---------------------------------------------------------------------------------------------------
;; Types and shuffle builder

;; A cutter separates the deck into more than one sub-decks -- the last one of these is "left in the
;; hand", as per the overhand shuffle (since it is the last strip to be stripped). The riffler
;; indicates this in its second (non-null) return value
(define-type (Cutter A) (-> (Listof A) (Pair (Listof A) (Listof (Listof A)))))
;; A riffler takes taking hand and the cut deck parts. returns a newly merged deck in the "taking"
;; hand and the deck left in the "giving" hand. The shuffler will keep taking,
;; until there is nothing to give
(define-type (Riffler A) ((Listof A) (Listof A) (Listof A) * -> (Values (Listof A) (Listof A))))
;; "The shuffler will keep taking until there is nothing to give"... and will do this
;; the number of times specified by its second argument
(define-type (Shuffler A) ((Listof A) Natural -> (Listof A)))

;; makes a shuffler from the cutter and the riffler
(: shuffler-composer (All (A) (Cutter A) (Riffler A) -> (Shuffler A)))
(define ((shuffler-composer cut riffle) deck n)
  (: one-shuffle : (Listof A) -> (Listof A))
  (define (one-shuffle g)
    (let: shuff ((t : (Listof A) null) (g : (Listof A) g))
      (let-values (((t+ g-) (apply riffle t (cut g))))
        (if (null? g-) t+ (shuff t+ g-)))))
  (for/fold : (Listof A) ((d deck)) ((i (in-range n)))
    (one-shuffle d)))

;; convenient wrapper around the above (otherwise we'd need the inst every time we
;; wanted to compose a cut and a riffle
(define-syntax-rule (define-composed-shuffler s (c r))
  (define: (A) (s [x : (Listof A)] [n : Natural]) : (Listof A)
    ((#{shuffler-composer @ A} c r) x n)))

;; ---------------------------------------------------------------------------------------------------
;; Overhand (and, as far as I can tell, Indian)
(: overhand-cutter (All (A) (Cutter A)))
(: overhand-riffler (All (A) (Riffler A)))

(define (overhand-cutter l)
  (define spl (match (length l) [0 0] [1 1] [len (add1 (random (sub1 len)))]))
  (list (take l spl) (drop l spl)))

(define (overhand-riffler t p1 . rest)
  (values (append p1 t) (append* rest)))

(define-composed-shuffler overhand-shuffle (overhand-cutter overhand-riffler))

;; ---------------------------------------------------------------------------------------------------
;; Riffle (with optional "drop" where two cards are riffled
(: half-deck-cutter (All (A) (Cutter A)))
(: mk-riffle-riffler (All (A) ((#:p-drop Nonnegative-Real) -> (Riffler A))))

(define (half-deck-cutter l)
  (define spl (quotient (length l) 2))
  (list (take l spl) (drop l spl)))

;; All the "reverse"ing is to emulate a physical shuffle... it's not
;; necessary for the "randomising" effect (which there isn't really on
;; a pure riffle anyway)
;;
;; Additional complexity added by ability to drop cards on both taking
;; and giving hand
(define ((mk-riffle-riffler #:p-drop (p-drop 0)) t p1 . rest)
  (define g-/rev
    (let R : (Listof A)
      ((r1 : (Listof A) p1)
       (r2 : (Listof A) (append* rest))
       (rv : (Listof A) t)) ; although t should normaly be null
      (define drop-t? (< (random) p-drop))
      (define drop-g? (< (random) p-drop))
      (match* (r1 r2 drop-t? drop-g?)
        [((list) (app reverse 2r) _ _) (append 2r rv)]
        [((app reverse 1r) (list) _ _) (append 1r rv)]
        [((list a1.1 a1.2 d1 ...) (list a2.1 a2.2 d2 ...) #t #t)
         (R d1 d2 (list* a2.2 a2.1 a1.2 a1.1 rv))]
        [((list a1.1 a1.2 d1 ...) (list a2.1 d2 ...) #t _)
         (R d1 d2 (list* a2.1 a1.2 a1.1 rv))]
        [((list a1.1 d1 ...) (list a2.1 a2.2 d2 ...) _ #t)
         (R d1 d2 (list* a2.2 a2.1 a1.1 rv))]
        [((list a1.1 d1 ...) (list a2.1 d2 ...) _ _)
         (R d1 d2 (list* a2.1 a1.1 rv))])))
  (values (reverse g-/rev) null))

(define-composed-shuffler pure-riffle-shuffle (half-deck-cutter (mk-riffle-riffler)))
(define-composed-shuffler klutz-riffle-shuffle (half-deck-cutter (mk-riffle-riffler #:p-drop 0.5)))

;; ---------------------------------------------------------------------------------------------------
;; Pile Shuffle
;; Also Wash Shuffle, if pile-height=1 and random-gather=#t
(: mk-pile-cutter (All (A) (#:pile-height Positive-Integer -> (Cutter A))))
(: mk-pile-riffler (All (A) ((#:random-gather? Boolean) -> (Riffler A))))

(define ((mk-pile-cutter #:pile-height pile-height) l)
  (define len-l (length l))
  (define n-piles (add1 (quotient (sub1 len-l) pile-height)))
  (: make-pile (Integer -> (Listof A)))
  (define (make-pile n)
    (for/list : (Listof A) ((i (in-range n len-l n-piles)))
      (list-ref l i)))
  (define pile-0 (make-pile 0))
  (define piles-ns (for/list : (Listof (Listof A)) ((n (in-range 1 n-piles))) (make-pile n)))
  (list* pile-0 piles-ns))

(define ((mk-pile-riffler #:random-gather? (random-gather? #f)) t p1 . rest)
  (: piles (Listof (Listof A)))
  (define piles (cons p1 rest))
  (define gather (if random-gather? (shuffle piles) piles))
  (values (append* (cons t (if random-gather? (shuffle piles) piles))) null))

(define-composed-shuffler 4-high-pile-shuffle ((mk-pile-cutter #:pile-height 4) (mk-pile-riffler)))
(define-composed-shuffler wash-pile-shuffle
  ((mk-pile-cutter #:pile-height 1) (mk-pile-riffler #:random-gather? #t)))

;; ---------------------------------------------------------------------------------------------------
(define unshuffled-pack
  (for*/list : (Listof String)
    ((s '(♥ ♦ ♣ ♠))
     (f '(2 3 4 5 6 7 8 9 T J Q K A)))
    (format "~a~a" f s)))

;; ---------------------------------------------------------------------------------------------------
;; TEST/OUTPUT
(module+ test
  (require typed/rackunit)
  (check-equal? (overhand-shuffle null 1) null)
  (check-equal? (overhand-shuffle '(a) 1) '(a))
  (check-equal? (overhand-shuffle '(a b) 1) '(b a))
  (check-equal? (pure-riffle-shuffle '(1 2 3 4) 1) '(1 3 2 4))
  (error-print-width 80))

(module+ main
  (printf "deck (original order):          ~.a~%" unshuffled-pack)
  (printf "overhand-shuffle (2 passes):    ~.a~%" (overhand-shuffle unshuffled-pack 2))
  (printf "overhand-shuffle (1300 passes): ~.a~%" (overhand-shuffle unshuffled-pack 1300))
  (printf "riffle: pure                    ~.a~%" (pure-riffle-shuffle unshuffled-pack 1))
  (printf "riffle: klutz                   ~.a~%" (klutz-riffle-shuffle unshuffled-pack 1))
  (printf "4-high piles:                   ~.a~%" (4-high-pile-shuffle unshuffled-pack 1))
  (printf "4-high piles (7 passes):        ~.a~%" (4-high-pile-shuffle unshuffled-pack 7))
  (printf "4-high piles (7 passes again):  ~.a~%" (4-high-pile-shuffle unshuffled-pack 7))
  (printf "wash piles:                     ~.a~%" (wash-pile-shuffle unshuffled-pack 1))
  ;; Or there is always the built-in shuffle:
  (printf "shuffle:                        ~.a~%" (shuffle unshuffled-pack)))
```


{{out}}

You see no output from the tests... that's a good thing, they're all passing.

Output is truncated by the <code>~.a</code> format in <code>printf</code>. However, this should give you some idea of what's going on.


```txt
deck (original order):          (2♥ 3♥ 4♥ 5♥ 6♥ 7♥ 8♥ 9♥ T♥ J♥ Q♥ K♥ A♥ 2♦ 3♦ 4...
overhand-shuffle (2 passes):    (2♥ 6♠ 5♠ J♦ Q♦ K♦ A♦ 2♣ 3♣ 4♣ 5♣ 6♣ 7♣ 8♣ 9♣ T...
overhand-shuffle (1300 passes): (J♦ J♥ J♠ A♥ K♦ 5♥ J♣ 8♣ 2♥ 4♠ 9♥ A♠ K♣ Q♥ 4♥ 7...
riffle: pure                    (2♥ 2♣ 3♥ 3♣ 4♥ 4♣ 5♥ 5♣ 6♥ 6♣ 7♥ 7♣ 8♥ 8♣ 9♥ 9...
riffle: klutz                   (2♥ 2♣ 3♥ 3♣ 4♥ 4♣ 5♣ 5♥ 6♥ 6♣ 7♥ 7♣ 8♥ 8♣ 9♥ 9...
4-high piles:                   (2♥ 2♦ 2♣ 2♠ 3♥ 3♦ 3♣ 3♠ 4♥ 4♦ 4♣ 4♠ 5♥ 5♦ 5♣ 5...
4-high piles (7 passes):        (2♥ 6♥ T♥ A♥ 5♦ 9♦ K♦ 4♣ 8♣ Q♣ 3♠ 7♠ J♠ 3♥ 7♥ J...
4-high piles (7 passes again):  (2♥ 6♥ T♥ A♥ 5♦ 9♦ K♦ 4♣ 8♣ Q♣ 3♠ 7♠ J♠ 3♥ 7♥ J...
wash piles:                     (4♣ K♠ 4♠ Q♥ J♣ A♣ 6♦ 6♥ 7♥ A♠ T♠ T♥ Q♣ 8♠ 3♣ J...
shuffle:                        (J♣ 2♠ 4♦ A♦ K♥ 6♦ 5♦ 8♣ 2♦ T♥ 4♠ 3♣ 7♦ 9♠ T♦ J...

```



## REXX

A little extra effort was put into the '''create''' subroutine to build any sort of deck, even a multiple deck as in canasta and samba (with/without jokers).   Adding options for short decks, pinochle, schmear, six-handed 500, and the like would be prohibitive and muddy up the code and be distracting.

Six-handed 500 has additional cards of:   <big> ♣11   ♣12         ♠11   ♠12       ♦11   ♦12   ♦13       ♦11   ♦12   ♦13 </big>

```rexx
/*REXX program simulates various types of shuffling a deck of cards  (any kind of deck).*/
call create;  call show  'new deck'              /*build and display a new card deck.   */

call create;  call riffle     1                  /*invoke a riffle shuffle  (N times).  */
              call show  'riffle shuffle'        /*display the results from last shuffle*/

call create;  call overhand  1/5                 /*invoke overhand shuffle with 20% cuts*/
              call show  'overhand shuffle'      /*display the results from last shuffle*/

call create;  call barnYard  13                  /*also called a washing machine shuffle*/
              call show  'barn yard shuffle'     /*display the results from last shuffle*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
create: if 9=='f9'x  then suit= "cdhs"           /*EBCDIC?   Then use letters for suits.*/
                     else suit= "♣♦♥♠"           /* ASCII?     "   "  symbols  "    "   */
        jokers= 0                                /*number of jokers in the card deck.   */
        wild= copies("jH jL", jokers)            /*a large # of high jokers, low jokers.*/
        rank= 'A23456789tJQK'                    /*t  in the rank represents a ten (10).*/
        decks= 1                                 /*the number of decks, building a shoe?*/
        $=                                       /*the initial (null) card deck (string)*/
               do   s=1  for length(suit)        /*process each of the card deck suits. */
               _= substr(suit, s, 1)             /*extract a single suit to build + pips*/
                 do r=1  for length(rank)        /*process each of the card deck pips.  */
                 $= $  _ || substr(rank, r, 1)   /*build a card, then append it to deck.*/
                 end   /*r*/                     /*Note: some decks have more pips, >13.*/
               end     /*s*/                     /*  "     "    "     "    "  suits, >4.*/
        $= space($  subword(wild, 1, jokers) )   /*keep a new card deck for each shuffle*/
        $= copies($, decks)                      /*maybe build multiple decks for a shoe*/
        #= words($)                              /*set the number of cards in the deck. */
                                                 /*another entry point for this function*/
build:  @.=;         do j=1  for words($)        /*build an array for the card deck.    */
                     @.j= word($, j)             /*construct an card from the deck list.*/
                     end   /*j*/
        return $                                 /*elide the leading blank in the deck. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
?:        return random(1, word( arg(1) #, 1) )  /*gen a random number from  1 ──► arg. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
barnYard:   do j=1  for arg(1);       x=?();
              do until y\==x | #<2;   y=?()
              end   /*until*/
            parse value   @.x  @.y     with     @.y  @.x
            end     /*j*/;                                           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
riffle:   $A= subword($, 1, #%2);     $B= subword($, #%2 + 1);   $= /*split deck in half*/
            do j=1  for max( words($A), words($B) );       $= $  word($A, j)   word($B, j)
            end   /*j*/
          $= space($);   call build;                                 return
/*──────────────────────────────────────────────────────────────────────────────────────*/
overhand: parse arg pc .;  if pc==''  then pc= 1/5;   chunk= # * pc % 1;       $B=
            do while words($)\==0;    $B= $B subword($, 1, chunk); $= subword($, chunk +1)
            end   /*while*/
          $= space($B);               call build;                    return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:     _=@.1;        do j=2  for #-1;   _=_ @.j;   end /*j*/;           L = length(_)
          say center( arg(1), L, '═');     say _;     say;           return  /*show deck*/
```

{{out|output}}

```txt
═════════════════════════════════════════════════════════════════════════new deck══════════════════════════════════════════════════════════════════════════
♣A ♣2 ♣3 ♣4 ♣5 ♣6 ♣7 ♣8 ♣9 ♣t ♣J ♣Q ♣K ♦A ♦2 ♦3 ♦4 ♦5 ♦6 ♦7 ♦8 ♦9 ♦t ♦J ♦Q ♦K ♥A ♥2 ♥3 ♥4 ♥5 ♥6 ♥7 ♥8 ♥9 ♥t ♥J ♥Q ♥K ♠A ♠2 ♠3 ♠4 ♠5 ♠6 ♠7 ♠8 ♠9 ♠t ♠J ♠Q ♠K

══════════════════════════════════════════════════════════════════════riffle shuffle═══════════════════════════════════════════════════════════════════════
♣A ♥A ♣2 ♥2 ♣3 ♥3 ♣4 ♥4 ♣5 ♥5 ♣6 ♥6 ♣7 ♥7 ♣8 ♥8 ♣9 ♥9 ♣t ♥t ♣J ♥J ♣Q ♥Q ♣K ♥K ♦A ♠A ♦2 ♠2 ♦3 ♠3 ♦4 ♠4 ♦5 ♠5 ♦6 ♠6 ♦7 ♠7 ♦8 ♠8 ♦9 ♠9 ♦t ♠t ♦J ♠J ♦Q ♠Q ♦K ♠K

═════════════════════════════════════════════════════════════════════overhand shuffle══════════════════════════════════════════════════════════════════════
♣A ♣2 ♣3 ♣4 ♣5 ♣6 ♣7 ♣8 ♣9 ♣t ♣J ♣Q ♣K ♦A ♦2 ♦3 ♦4 ♦5 ♦6 ♦7 ♦8 ♦9 ♦t ♦J ♦Q ♦K ♥A ♥2 ♥3 ♥4 ♥5 ♥6 ♥7 ♥8 ♥9 ♥t ♥J ♥Q ♥K ♠A ♠2 ♠3 ♠4 ♠5 ♠6 ♠7 ♠8 ♠9 ♠t ♠J ♠Q ♠K

═════════════════════════════════════════════════════════════════════barn yard shuffle═════════════════════════════════════════════════════════════════════
♣A ♣2 ♣3 ♠2 ♥6 ♣6 ♣7 ♣8 ♦8 ♥8 ♣J ♣Q ♣K ♦A ♦2 ♦3 ♦4 ♥t ♦6 ♦7 ♠A ♦9 ♦t ♦J ♦Q ♠t ♥A ♥2 ♣t ♥4 ♠5 ♣5 ♦K ♥Q ♥9 ♦5 ♥J ♥3 ♥K ♣9 ♣4 ♠3 ♠4 ♠K ♠6 ♥7 ♠8 ♠9 ♠7 ♠J ♠Q ♥5

```



## Ruby


Two methods to solve the requirements, and a third one as bonus.


```Ruby

def riffle deck
  left, right = deck.partition{rand(10).odd?}
  new_deck    = []

  # the condition below is true when both left and right stacks are empty
  until ((left_card=left.pop).to_i + (right_card=right.shift).to_i).zero? do
    new_deck << left_card  if left_card
    new_deck << right_card if right_card
  end

  new_deck
end

def overhand deck
  new_deck = []

  until deck.empty? do
    stack = deck[-rand(deck.size * 0.2), deck.size]
    new_deck += stack
    deck     -= stack
  end

  new_deck
end

def bonus deck
  deck.sort { |a, b| Time.now.to_i % a <=> Time.now.to_i % b }
end

deck = [*1..20]

puts riffle(deck).inspect
puts overhand(deck).inspect
puts bonus(deck).inspect

```



## Tcl


```Tcl

proc riffle deck {
	set length [llength $deck]
	for {set i 0} {$i < $length/2} { incr i} {
		lappend temp [lindex $deck $i] [lindex $deck [expr {$length/2+$i}]]}
	set temp}
proc overhand deck {
	set cut [expr {[llength $deck] /5}]
	for {set i $cut} {$i >-1} {incr i -1} {
		lappend temp [lrange $deck [expr {$i *$cut}] [expr {($i+1) *$cut -1}] ]}
	concat {*}$temp}
puts [riffle [list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52]]
puts [overhand [list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52]]

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Runtime.CompilerServices
Imports System.Text

Module Module1

    <Extension()>
    Function AsString(Of T)(c As ICollection(Of T)) As String
        Dim sb = New StringBuilder("[")
        sb.Append(String.Join(", ", c))
        Return sb.Append("]").ToString()
    End Function

    Private rand As New Random()

    Function RiffleShuffle(Of T)(list As ICollection(Of T), flips As Integer) As List(Of T)
        Dim newList As New List(Of T)(list)

        For n = 1 To flips
            'cut the deck at the middle +/- 10%, remove the second line of the formula for perfect cutting
            Dim cutPoint As Integer = newList.Count / 2 + If(rand.Next(0, 2) = 0, -1, 1) * rand.Next(newList.Count * 0.1)

            'split the deck
            Dim left As New List(Of T)(newList.Take(cutPoint))
            Dim right As New List(Of T)(newList.Skip(cutPoint))

            newList.Clear()

            While left.Count > 0 AndAlso right.Count > 0
                'allow for imperfect riffling so that more than one card can come form the same side in a row
                'biased towards the side with more cards
                'remove the if And else And brackets for perfect riffling
                If rand.NextDouble() >= left.Count / right.Count / 2 Then
                    newList.Add(right.First())
                    right.RemoveAt(0)
                Else
                    newList.Add(left.First())
                    left.RemoveAt(0)
                End If
            End While

            'if either hand is out of cards then flip all of the other hand to the shuffled deck
            If left.Count > 0 Then
                newList.AddRange(left)
            End If
            If right.Count > 0 Then
                newList.AddRange(right)
            End If
        Next

        Return newList
    End Function

    Function OverhandShuffle(Of T)(list As ICollection(Of T), passes As Integer) As List(Of T)
        Dim mainHand As New List(Of T)(list)

        For n = 1 To passes
            Dim otherhand = New List(Of T)

            While mainHand.Count > 0
                'cut at up to 20% of the way through the deck
                Dim cutSize = rand.Next(list.Count * 0.2) + 1

                Dim temp = New List(Of T)

                'grab the next cut up to the end of the cards left in the main hand
                Dim i = 0
                While i < cutSize AndAlso mainHand.Count > 0
                    temp.Add(mainHand.First())
                    mainHand.RemoveAt(0)
                    i = i + 1
                End While

                'add them to the cards in the other hand, sometimes to the front sometimes to the back
                If rand.NextDouble() >= 0.1 Then
                    'front most of the time
                    temp.AddRange(otherhand)
                    otherhand = temp
                Else
                    'end sometimes
                    otherhand.AddRange(temp)
                End If
            End While

            'move the cards back to the main hand
            mainHand = otherhand
        Next

        Return mainHand
    End Function

    Sub Main()
        Dim list = New List(Of Integer)(Enumerable.Range(1, 20))
        Console.WriteLine(list.AsString())
        list = RiffleShuffle(list, 10)
        Console.WriteLine(list.AsString())
        Console.WriteLine()

        list = New List(Of Integer)(Enumerable.Range(1, 20))
        Console.WriteLine(list.AsString())
        list = RiffleShuffle(list, 1)
        Console.WriteLine(list.AsString())
        Console.WriteLine()

        list = New List(Of Integer)(Enumerable.Range(1, 20))
        Console.WriteLine(list.AsString())
        list = OverhandShuffle(list, 10)
        Console.WriteLine(list.AsString())
        Console.WriteLine()

        list = New List(Of Integer)(Enumerable.Range(1, 20))
        Console.WriteLine(list.AsString())
        list = OverhandShuffle(list, 1)
        Console.WriteLine(list.AsString())
        Console.WriteLine()
    End Sub

End Module
```

{{out}}

```txt
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[1, 5, 15, 8, 3, 7, 17, 12, 14, 6, 19, 18, 13, 16, 2, 20, 11, 10, 4, 9]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[1, 2, 12, 13, 14, 3, 15, 4, 5, 16, 17, 6, 7, 8, 9, 18, 10, 19, 20, 11]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[15, 16, 20, 14, 17, 9, 10, 5, 6, 3, 12, 18, 11, 4, 1, 2, 8, 13, 19, 7]

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[19, 20, 15, 16, 17, 18, 13, 14, 10, 11, 12, 7, 8, 9, 4, 5, 6, 1, 2, 3]
```



## zkl

A much better shuffle is List's shuffle method.

```zkl
fcn riffle(deck){
   len,N:=deck.len(),len/2;
   newDeck:=N.pump(List,'wrap(n){ return(Void.Write,deck[n],deck[N+n]) });
   if(len.isOdd) return(newDeck.append(deck[-1]));
   newDeck
}
fcn overHand(deck){
   len,N,piles:=deck.len(),(0.2*len).toInt(),(len.toFloat()/N).ceil().toInt();
   piles.pump(List,'wrap(n){ deck[n*N,N] }).reverse().flatten()
}
```


```zkl
riffle(  [1..19].walk()).println();
overHand([1..19].walk()).println();
[1..19].walk().shuffle().println();
```

{{out}}

```txt

L(1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,19)
L(19,16,17,18,13,14,15,10,11,12,7,8,9,4,5,6,1,2,3)
L(9,11,12,6,17,18,5,10,8,19,2,15,4,3,13,1,7,14,16)

```


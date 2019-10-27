+++
title = "Playing cards/E"
description = ""
date = 2010-02-06T14:07:02Z
aliases = []
[extra]
id = 5310
[taxonomies]
categories = []
tags = []
+++

{{collection|Playing Cards}}

The E language is designed to permit cooperation among multiple mutually suspicious ("untrusted" if you like) programs. Therefore, it is appropriate to design a deck which can be shared without allowing card holders to cheat. In particular, even though the cards are ordinary objects, which can have multiple references to them, nobody should be able to both give away a card and keep it.

Therefore, this implementation adds an extra feature: the <code>take</code> operation on cards, which returns a new reference to a card and invalidates the old one. Therefore, any player or game rule can, when given a card, take sole possession of it.

A "stack" is just a collection of cards, whether a hand or a deck. This implementation does not attempt to simulate the hidden information of a face-down deck, though this would be possible.


```e
def makeStack(cardsIn) {
    def cards := cardsIn.diverge()
    def stack {
        to __printOn(out) {
            out.print("<cards: ", cards, ">")
        }
        to shuffle() {
            # Per http://rosettacode.org/wiki/Knuth_shuffle
            for bound in (2..(cards.size())).descending() {
                def i := entropy.nextInt(bound)
                def swapTo := bound - 1
                def t := cards[swapTo]
                cards[swapTo] := cards[i]
                cards[i] := t
            }
        }
        to deal(count :int) {
            def hand := cards.removeRun(0, count)
            return makeStack(hand)
        }
        to size() { return cards.size() }
        to get(index) { return cards.get(index) }
        to take(index :int) {
            def [card] := cards.removeRun(index, index + 1)
            return card
        }
    }
    return stack
}

def makeDeck() {
    interface CardG guards CardS {}
    def Card {
        to coerce(specimen, ejector) {
            def card := CardG.coerce(specimen, ejector)
            if (card.taken()) {
                throw.eject(ejector, `This card has been taken`)
            }
            return card
        }
    }
  
    def makeCard(rank, suit) {
        var taken := false
        def card implements CardS {
            to __printOn(out) {
                out.print(`the $rank of $suit`)
                if (taken) {
                   out.print(" (taken)")
                }
            }
            to take() {
                taken := true
                return makeCard(rank, suit)
            }
            to taken() { return taken }
        }
        return card
    }

    var cards := []
    for suit in ["spades", "hearts", "diamonds", "clubs"] {
        for rank in 1..13 {
            cards with= makeCard(rank, suit)
        }
    }
  
    return [makeStack(cards), Card]
}
```


A sample session with a deck, showing dealing and taking:


```txt
? def [deck, Card] := makeDeck()
# value: [<cards: [the 1 of spades, the 2 of spades, the 3 of spades, the 4 of spades, the 5 of spades, the 6 of spades, the 7 of spades, the 8 of spades, the 9 of spades, the 10 of spades, the 11 of spades, the 12 of spades, the 13 of spades, the 1 of hearts, the 2 of hearts, the 3 of hearts, the 4 of hearts, the 5 of hearts, the 6 of hearts, the 7 of hearts, the 8 of hearts, the 9 of hearts, the 10 of hearts, the 11 of hearts, the 12 of hearts, the 13 of hearts, the 1 of diamonds, the 2 of diamonds, the 3 of diamonds, the 4 of diamonds, the 5 of diamonds, the 6 of diamonds, the 7 of diamonds, the 8 of diamonds, the 9 of diamonds, the 10 of diamonds, the 11 of diamonds, the 12 of diamonds, the 13 of diamonds, the 1 of clubs, the 2 of clubs, the 3 of clubs, the 4 of clubs, the 5 of clubs, the 6 of clubs, the 7 of clubs, the 8 of clubs, the 9 of clubs, the 10 of clubs, the 11 of clubs, the 12 of clubs, the 13 of clubs].diverge()>, <Card>]

? def hand := deck.deal(5)
# value: <cards: [the 1 of spades, the 2 of spades, the 3 of spades, the 4 of spades, the 5 of spades].diverge()>

? def card := hand.take(0)
# value: the 1 of spades

? hand
# value: <cards: [the 2 of spades, the 3 of spades, the 4 of spades, the 5 of spades].diverge()>

? def card2 := card.take()
# value: the 1 of spades

? card
# value: the 1 of spades (taken)

? card2 :Card
# value: the 1 of spades

? card :Card
# problem: This card has been taken
```


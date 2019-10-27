+++
title = "Go Fish/D"
description = ""
date = 2013-12-31T13:15:55Z
aliases = []
[extra]
id = 16095
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}

Translation of Python. The gameplay is the same as the Python entry. This entry retains most of the style of the Python entry, this means cards are strings instead of an enum, etc. Usually in idiomatic D code you use enums instead, to offer stronger compile-time guarantees.

In D strings are only partially reference types, unlike in Python, so I have used a small class to keep the shared deck. I have also used an abstract class to simplify and reduce the code compared to the Python entry that has some code duplication.

```d
import std.stdio, std.random, std.range, std.string, std.array,
       std.algorithm;

class EndGameException : Exception {
    this() pure nothrow { super(""); }
}


class Deck {
    string[] d;
    this(string[] deck_) { d = deck_; }
    alias d this;
}


abstract class Player {
    string name;
    Deck deck;
    int[string] hand;
    int score = 0;
    string[] book;

    this(Deck deck_)
    in {
        assert(deck_ !is null);
    } body {
        deck = deck_;
    }

    void draw()
    in {
        assert(deck !is null);
    } body {
        auto cardDrawn = deck.back;
        deck.popBack; // Removes the last card from deck.
        hand[cardDrawn]++; // Adds card to hand.
        writefln("%s drew %s.", name, cardDrawn);
        checkForBooks;
    }

    string makeTurn();

    /// Removes all items of which are 4.
    void checkForBooks() {
        // We are modifying 'hand' in loop:
        foreach (key, val; zip(hand.keys, hand.values))
            if (val == 4) { // completed a book
                book ~= key;
                writefln("%s completed the book of %s's.", name, key);
                score++;
                hand.remove(key);
            }
        emptyCheck;
    }

    /// Checks if deck/hand is empty.
    void emptyCheck() {
        //if (!deck.empty && hand.empty)
        if (!deck.empty && hand.length == 0)
            draw();
    }

    /// If card in hand, returns count and removes the card from hand.
    int fishFor(string card) {
        if (card in hand) {
            // If card in hand, returns count and removes the
            // card from hand.
            auto val = hand[card];
            hand.remove(card);
            emptyCheck;
            return val;
        } else
            return 0;
    }

    void gotCard(string card, int amount) {
        hand[card] += amount;
        checkForBooks;
    }
}


class Human: Player {
    this(Deck deck_) {
        super(deck_);
        "Name yourself: ".write;
        name = readln.strip;
    }

    /// Displays current hand, cards separated by spaces.
    string displayHand() {
        //return hand.byPair.map!({k, v} => [k].replicate(v))
        return hand.byKey.map!(k => [k].replicate(hand[k]))
               .joiner.join(" ");
    }

    override string makeTurn() {
        writefln("%s's hand: %s", name, displayHand);
        "What card do you ask for? ".write;
        auto chooseCard = readln.strip;
        if (chooseCard == "quit")
            throw new EndGameException();
        if (chooseCard !in hand) {
            writeln("You don't have that card. Try again!" ~
                    " (or enter quit to exit)");
            chooseCard = makeTurn;
        }
        return chooseCard;
    }
}


class Computer: Player {
    bool[string] opponentHas; // a Set.

    this(Deck deck_) {
        super(deck_);
        name = "Computer";
    }

    // AI: guesses cards that knows you have, then tries cards he has
    // at random. Improvements: remember if the card was rejected
    // before, guess probabilities.
    override string makeTurn() {
        // print displayHand(), opponentHas
        // Checks for cards in hand that computer knows you have.
        auto candidates = opponentHas.keys.sort()
                          .setIntersection(hand.keys.sort()).array;
        if (candidates.empty)
            // If no intersection between those two, random guess.
            candidates = hand.keys;
        auto move = candidates[uniform(0, $)];
        writefln("%s fishes for %s.", name, move);
        return move;
    }

    /// Same as for humans players, but adds the card fished
    // for to opponentHas list.
    override int fishFor(string card) {
        opponentHas[card] = true;
        return super.fishFor(card);
    }

    override void gotCard(string card, int amount) {
        opponentHas.remove(card);
        super.gotCard(card, amount);
    }
}


class GoFish {
    Deck deck;
    Player[] players;

    this() {
        deck=new Deck("2 3 4 5 6 7 8 9 10 J Q K A".split.replicate(4));

        // Makes counting turns easier.
        //players = [new Human(deck), new Computer(deck)];
        players ~= new Human(deck);
        players ~= new Computer(deck);
    }

    /// Checks if hands/decks are empty.
    bool endOfPlayCheck() {
        return !deck.empty || players[0].hand.length ||
               players[1].hand.length;
    }

    void play() {
        deck.d.randomShuffle;
        foreach (_; 0 .. 9) { // Deal the first cards
            players[0].draw;
            players[1].draw;
        }

        auto turn = 0;
        while (endOfPlayCheck) {
            writefln("\nTurn %d (%s: %d, %s: %d) %d cards remaining.",
                     turn, players[0].name,
                     players[0].score, players[1].name,
                     players[1].score, deck.length);
            auto whoseTurn = turn % 2;
            auto otherPlayer = (turn + 1) % 2;
            while (true) { // Loop until player finishes turn.
                auto cardFished = players[whoseTurn].makeTurn;
                auto result = players[otherPlayer].fishFor(cardFished);
                if (!result) { // Draws and ends turn.
                    players[whoseTurn].draw;
                    break;
                }
                writefln("%s got %d more %s.",
                         players[whoseTurn].name, result, cardFished);
                players[whoseTurn].gotCard(cardFished, result);
                if (!endOfPlayCheck)
                    break;
            }
            turn++;
        }

        writefln("\nScores: \n%s: %d\n%s: %d\n",
                 players[0].name, players[0].score,
                 players[1].name, players[1].score);
        if (players[0].score > players[1].score)
            writeln(players[0].name, " won!");
        else if (players[0].score == players[1].score)
            writeln("draw!");
        else
            writefln(players[1].name, " won!");
    }
}

void main() {
    try {
        auto game = new GoFish;
        game.play;
    } catch (EndGameException)
        writeln("Game finished.");
}
```


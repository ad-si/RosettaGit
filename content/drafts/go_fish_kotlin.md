+++
title = "Go Fish/Kotlin"
description = ""
date = 2017-10-18T00:02:40Z
aliases = []
[extra]
id = 21639
[taxonomies]
categories = []
tags = []
+++


```scala
// version 1.1.51

import java.util.Random

const val FACES = "23456789tjqka"
const val SUITS = "cdhs"

enum class Turn { user, comp }

class Card(val face: Char, val suit: Char) : Comparable<Card> {
    private val value = FACES.indexOf(face) * 4 + SUITS.indexOf(suit)

    override fun compareTo(other: Card) = this.value.compareTo(other.value)

    override fun toString() = "$face$suit"
}

val r = Random()

val ranks = listOf(
    "twos", "threes", "fours", "fives", "sixes", "sevens", "eights",
    "nines", "tens", "jacks", "queens", "kings", "aces"
)

val deck = mutableListOf<Card>()

val userHand  = mutableListOf<Card>()
val userBooks = mutableListOf<Char>()
val compHand  = mutableListOf<Card>()
val compBooks = mutableListOf<Char>()
val compPrev  = mutableListOf<Char>()

var turn = Turn.user // user always starts

fun createDeck() {
    for (suit in SUITS) {
        for (face in FACES) deck.add(Card(face, suit))
    }
}

fun shuffleDeck() {
    val shuffled = mutableListOf<Card>()
    do {
        val card = deck[r.nextInt(52)]
        if (card !in shuffled) shuffled.add(card)
    } while (shuffled.size < 52)
    deck.clear()
    deck.addAll(shuffled)
}

/* Chooses a rank at random provided it hasn't already been chosen
   during the current turn. If all ranks have already been chosen,
   it chooses the first again. */
fun compSelectRankIndex(): Int {
    val choices = compHand.map { it.face }.distinct().filter { it !in compPrev }
    val size = choices.size
    val choice = if (size == 0) compHand[0].face else choices[r.nextInt(size)]
    return FACES.indexOf(choice)
}

val userChoices get() = userHand.map { it.face }.distinct()

fun printHand() = println("Your cards : ${userHand.joinToString("  ")}")

fun printBooks() {
    println("Your books : ${userBooks.joinToString("   ")}")
    println("My books   : ${compBooks.joinToString("   ")}")
}

fun printTurn() =
    println(if (turn == Turn.user) "--- YOUR TURN ---" else "--- MY TURN ---")

fun checkForBooks(hand: MutableList<Card>, books: MutableList<Char>) {
    val newBooks = hand.groupBy { it.face }
                       .filter { it.value.size == 4 }
                       .map { it.key }
    if (newBooks.size > 0) {
        books.addAll(newBooks)
        books.sort()
        for (b in newBooks) {
            for (i in hand.size - 1 downTo 0) {
                if (hand[i].face == b) hand.removeAt(i)
            }
        }
        if (hand.size == 0 && deck.size > 0) {
            val e = deck.removeAt(0)
            if (hand === userHand) println("You drew : $e")
            hand.add(e)
        }
        println("Added ${newBooks.joinToString("   ")} to books")
    }
}

fun showStateOfPlay(showTurn: Boolean = true) {
    println()
    userHand.sort()
    printHand()
    printBooks()
    if (showTurn) printTurn()
    println()
}

fun main(args: Array<String>) {
    // create and shuffle deck and deal cards
    createDeck()
    shuffleDeck()
    for (i in 0..16 step 2) userHand.add(deck[i])
    for (i in 1..17 step 2) compHand.add(deck[i])
    for (i in 0..17) deck.removeAt(0)

    // check if there are any books in initial hands
    checkForBooks(userHand, userBooks)
    checkForBooks(compHand, compBooks)
    showStateOfPlay()

    while (true) {
        while (true) {
            var rank: String

            if (turn == Turn.user) {
                val choices = userChoices
                while (true) {
                    print("Enter the rank you want : ")
                    rank = readLine()!!.toLowerCase()
                    if (rank !in ranks) continue
                    val choice = FACES[ranks.indexOf(rank)]
                    if (choice in choices) break
                }
            }
            else {
                val r = compSelectRankIndex()
                rank = ranks[r]
                println("The rank I want is : $rank")
                compPrev.add(FACES[r])
            }
            val face = FACES[ranks.indexOf(rank)]
            var matches = 0
            if (turn == Turn.user) {
                for (i in compHand.size - 1 downTo 0) {
                    if (compHand[i].face == face) {
                        matches++
                        userHand.add(compHand.removeAt(i))
                    }
                }
                println("Matches : $matches")
                if ((matches == 0 || userHand.size == 0) && deck.size > 0) {
                    val e = deck.removeAt(0)
                    println("You drew : $e")
                    userHand.add(e)
                }
                checkForBooks(userHand, userBooks)
                if (userBooks.size >= 7) {
                    showStateOfPlay(false)
                    println("Congratulations, you've won!")
                    return
                }
            }
            else {
                for (i in userHand.size - 1 downTo 0) {
                    if (userHand[i].face == face) {
                        matches++
                        compHand.add(userHand.removeAt(i))
                    }
                }
                println("Matches: $matches")
                if ((matches == 0 || compHand.size == 0) && deck.size > 0) {
                    val e = deck.removeAt(0)
                    compHand.add(e)
                }
                checkForBooks(compHand, compBooks)
                if (compBooks.size >= 7) {
                    showStateOfPlay(false)
                    println("Commiserations, but I've won!")
                    return
                }
            }
            if (matches > 0) showStateOfPlay() else break
        }
        turn = if (turn == Turn.user || userHand.size == 0) Turn.comp else Turn.user
        if (turn == Turn.comp) compPrev.clear()
        showStateOfPlay()
    }
}
```


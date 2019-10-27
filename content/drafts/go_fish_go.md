+++
title = "Go Fish/Go"
description = ""
date = 2015-09-12T05:19:58Z
aliases = []
[extra]
id = 18771
[taxonomies]
categories = []
tags = []
+++

The AI selects cards randomly from its hand.



```go

package main

import (
	"fmt"
	"math/rand"
	"sort"
	"time"
)

var cards = [13]string{"2", "3", "4", "5", "6", "7", "8", "9",
	"10", "J", "Q", "K", "A"}

//GoFishGame Stores the game state.
type GoFishGame struct {
	hands  [][]string
	deck   []string
	turn   int
	scores []int
}

// checkForBooks looks for fours of a kind
// and scores them if we have them.
// If we run out of cards, we draw more.
func (gm *GoFishGame) checkForBooks() {
	sort.Strings(gm.hands[gm.turn])
	prev := ""
	count := 1
	for _, card := range gm.hands[gm.turn] {
		if card == prev {
			count++
			if count == 4 {
				fmt.Printf("Book of %s.\n", card)
				gm.stealCards(card, gm.turn)
				gm.scores[gm.turn]++
				if gm.isHandEmpty() {
					gm.drawCard()
				}
			}
		} else {
			count = 1
		}
		prev = card
	}
}

// drawCard takes a card from the deck
// adding it to the current player's hand.
func (gm *GoFishGame) drawCard() {
	if !gm.isDeckEmpty() {
		card := gm.deck[0]
		gm.deck = gm.deck[1:]
		if gm.isPlayerTurn() {
			fmt.Printf("You drew a %s.\n", card)
		}
		gm.hands[gm.turn] = append(gm.hands[gm.turn], card)
		//Check for books
		gm.checkForBooks()
	}
}

// getPickComputer handles the computer's card choices.
// We do the moderately smart thing of pick a random
// card from our hand
func getPickComputer(gm *GoFishGame) string {
	hand := gm.hands[1]
	choice := "A"
	if len(hand) > 0 {
		choice = hand[rand.Intn(len(hand))]
	}
	fmt.Printf("Computer picks %s.\n", choice)
	return choice
}

// getPickUser gets the user's move.
// If it's not valid, then the user just wastes
// their turn.
func getPickUser(gm *GoFishGame) string {
	fmt.Println("What card do you want?")
	var card string
	fmt.Scanf("%s\n", &card)
	return card
}

// isDeckEmpty returns if the deck is empty.
func (gm *GoFishGame) isDeckEmpty() bool {
	return len(gm.deck) == 0
}

// isHandEmpty returns if the current player's hand is empty.
func (gm *GoFishGame) isHandEmpty() bool {
	return len(gm.hands[gm.turn]) == 0
}

// isGameOver returns if the game is over.
// This happens when all 13 pips have been made into sets.
func (gm *GoFishGame) isGameOver() bool {
	return gm.scores[0]+gm.scores[1] == 13
}

// isPlayerTurn returns if its the player's turn to move.
func (gm *GoFishGame) isPlayerTurn() bool {
	return gm.turn == 0
}

// makeDeck makes a deck.
// The deck is 52 cards with 4 of each pip.
func makeDeck() []string {
	rand.Seed(time.Now().UTC().UnixNano())
	deck := make([]string, 52)
	perm := rand.Perm(52)
	for indx := range perm {
		tVal := perm[indx]
		card := cards[tVal/4]
		deck[indx] = card
	}
	return deck
}

// opponentHas returns if the opponent's hand has a card.
func (gm *GoFishGame) opponentHas(find string) bool {
	for _, card := range gm.hands[(gm.turn+1)%2] {
		if card == find {
			return true
		}
	}
	return false
}

// playerTurn handles the major game logic.
// It's used for both the player's and computer's turns,
// with the different behavior handled by the getPick param.
func (gm *GoFishGame) playerTurn(getPick func(*GoFishGame) string) {
	opponent := (gm.turn + 1) % 2
	gm.checkForBooks()
	if opponent == 1 {
		gm.printHand()
	}
	if gm.isHandEmpty() {
		gm.drawCard()
	}
	gameOver := gm.isGameOver()
	if !gameOver {
		card := getPick(gm)
		if gm.opponentHas(card) {
			count := gm.stealCards(card, opponent)
			for indx := 0; indx < count; indx++ {
				gm.hands[gm.turn] = append(gm.hands[gm.turn], card)
			}
			gm.checkForBooks()
		} else {
			fmt.Println("GO FISH!")
			gm.drawCard()
			gm.turn = opponent
		}
	}
}

// printGameOverMessage prints the appropriate end message.
func (gm *GoFishGame) printGameOverMessage() {
	fmt.Printf("Final score is %d to %d.\n", gm.scores[0], gm.scores[1])
	if gm.scores[0] > gm.scores[1] {
		fmt.Println("Player wins!")
	} else if gm.scores[0] == gm.scores[1] {
		fmt.Println("It's a tie.")
	} else {
		fmt.Println("Computer wins!")
	}
}

// printHand print's the player's hand and current score.
func (gm *GoFishGame) printHand() {
	sort.Strings(gm.hands[0])
	fmt.Printf("You have: %s.\n", gm.hands[0])
	fmt.Printf("Score is %d to %d.\n", gm.scores[0], gm.scores[1])
}

// stealCards removes all instances of a card from side's hand.
func (gm *GoFishGame) stealCards(purge string, side int) int {
	count := 0
	var filtered []string
	for _, card := range gm.hands[side] {
		if purge == card {
			count++
		} else {
			filtered = append(filtered, card)
		}
	}
	gm.hands[side] = filtered
	return count
}

// main creates the deck and initial hands.
func main() {
	deck := makeDeck()
	playerHand := deck[0:9]
	compHand := deck[9:18]
	deck = deck[18:]
	hands := make([][]string, 2, 2)
	hands[0] = playerHand
	hands[1] = compHand
	scores := make([]int, 2, 2)
	scores[0] = 0
	scores[1] = 0
	game := GoFishGame{hands, deck, 0, scores}
	for {
		if game.isPlayerTurn() {
			game.playerTurn(getPickUser)
		} else {
			game.playerTurn(getPickComputer)
		}
		if game.isGameOver() {
			break
		}
	}
	game.printGameOverMessage()

}


```


+++
title = "Go Fish/AutoHotkey"
description = ""
date = 2014-03-17T22:07:51Z
aliases = []
[extra]
id = 17396
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
[[Category:AutoHotkey]]
{{works with|AutoHotkey L}} Unicode
Easily expanded to multiple players, any combination of human and AI.

```ahk
#NoEnv
SetBatchLines -1
#SingleInstance Force

Global Suits:={1:"♣",2:"♦",3:"♠",4:"♥"}
Global Pips:={13:"A",1:"2",2:"3",3:"4",4:"5",5:"6",6:"7",7:"8",8:"9",9:"T",10:"J",11:"Q",12:"K"}
Deck:=new Deck
Deck.Shuffle()
Player1:=new Human
Player0:=new AI
i:=!i ; current player
o:=!i ; current opponent
While (Player%i%.Archive() || Player1.Hand.HasCards() || Player0.Hand.HasCards())
{
	If (!Player%i%.Hand.HasCards()) ; if hand is empty,
		Player%i%.Hand.Draw() ; draw 1
	pip := Player%i%.Choose(o) ; player's request
	If pip not in % Player%i%.Pips() ; if invalid,
		Continue ; try again
	If (Player%i%.Take(pip, o)) ; if received what requested from opponent,
		Continue ; go again
	If (Player%i%.Hand.Draw().Pip = pip) ; if obtained what requested from pond,
		Continue ; go again
	i:=!i  ; current player
	o:=!i ; current opponent
}
Msgbox % "Human:  " Player1.Book.Show() 
	 . "`nAI:     " Player0.Book.Show() 
	 . "`nWinner: " (Player1.Book.HasCards() > Player0.Book.HasCards() ? "Human" : "AI")
Return ;--------------------------------------------------------------------------------

class Card
{
	__New(Pip, Suit) {
		this.Pip:=Pip, this.Suit:=Suit
	}
	Show() {
		Return this.Pip . this.Suit
	}
}
class Cards
{
	Cards:=[]
	
	HasCards() {
		Return this.Cards.MaxIndex()
	}
	Shuffle() {	; Knuth Shuffle from http://rosettacode.org/wiki/Knuth_Shuffle#AutoHotkey
		Loop % this.HasCards()-1 {
			Random, i, A_Index, this.HasCards()	; swap item 1,2... with a random item to the right of it
			temp := this.Cards[i], this.Cards[i] := this.Cards[A_Index], this.Cards[A_Index] := temp
		}
	}
	Show(Sort=0) {
		For i, Card in this.Cards
			s .= Card.Show() ","
		s:=SubStr(s,1,-1)
		If Sort
			Sort, s, D,
		Return s
	}
}
class Deck extends Cards
{
	__New() {
		For i, Pip in Pips
			For j, Suit in Suits
				this.Cards.Insert(new Card(Pip,Suit))
	}
}
class Hand extends Cards
{
	lastDraw := ""
	__New() {
		this.Draw(9)
	}
	Draw(n=1) {
		Loop, %n%
			If (Deck.HasCards())
				this.Cards.Insert(temp:=Deck.Cards.Remove(1)) ; to deal from bottom, use Remove()
		lastDraw := temp.Pip ","
		Return temp.Pip
	}
}
class Player
{
	Hand := new Hand
	Book := new Cards
	oRequests := "" ; log pips requested by opponent here
	
	Pip() {
		For i, Card in this.Hand.Cards
			s .= Card.Pip ","
		s:=SubStr(s,1,-1)
		Sort, s, D,
		Return s
	}
	Pips() {
		s := this.Pip()
		Sort, s, UD,
		Return s
	}
	Take(Pip, o) {
		Player%o%.oRequests := Pip "," Player%o%.oRequests
		For i, Card in Player%o%.Hand.Cards
			If (Card.Pip = Pip)
				this.Hand.Cards.Insert(Card)
				, toremove .= i ","
		toremove:=SubStr(toremove,1,-1)
		Sort, toremove, RND, ; must remove in reverse order
		Loop, Parse, toremove, `, ; so as not to mess up loop
			Player%o%.Hand.Cards.Remove(A_LoopField)
		Return toremove ? 1 : 0
	}
	Archive() {
		s := this.Pip()
		Loop, Parse, s, `,
		{
			If (A_LoopField = previous)
				count++
			Else
				count := 1
			If (count = Suits.MaxIndex()) ; normally 4
			{
				toarchive := A_LoopField
				Break
			}
			previous := A_LoopField
		}
		If (toarchive)
		{
			For i, Card in this.Hand.Cards
				If (Card.Pip = toarchive)
					this.Book.Cards.Insert(Card)
					, toremove .= i ","
			toremove:=SubStr(toremove,1,-1)
			Sort, toremove, RND, ; must remove in reverse order
			Loop, Parse, toremove, `, ; so as not to mess up loop
				this.Hand.Cards.Remove(A_LoopField)
		}
		Return 0
	}
}
class Human extends Player
{
	Choose(o) {
		InputBox, Pip, Enter a rank to request from opponent.
			, % "Your cards: " this.Hand.Show(1) 
			. "`nYour books: " this.Book.Show(1) 
			. "`nAI's books: " Player%o%.Book.Show(1) 
			. "`nEnter rank: " this.Pips()
			, , 900
		Return Pip
	}
}
class AI extends Player
{
	Choose(o) {
		Static Pip ; last asked
		s := this.Pip()
		Loop, Parse, s, `,
		{
			If (A_LoopField = previous)
				count++
			Else
				count := 1
			Random, rand, 0, 2
			If (count > oldcount - rand//2 && count < Suits.MaxIndex())
				oldcount := count
				, IHaveMostOf := A_LoopField
			previous := A_LoopField
		}
		ChoicePrefs := this.oRequests . this.Hand.lastDraw . IHaveMostOf . "," . s
		this.Hand.lastDraw := ""
		Loop, Parse, ChoicePrefs, `,
			If A_LoopField in % this.Pips()
				If (A_LoopField != Pip) ; don't repeat last asked
				{
					Pip := A_LoopField
					Break
				}
		Msgbox, , Opponent requests a rank from you.
			, % "Your cards: " Player%o%.Hand.Show(1) 
			. "`nYour books: " Player%o%.Book.Show(1) 
			. "`nAI's books: " this.Book.Show(1) 
			. "`n`n`nRequested rank: " Pip
		Return Pip
	}
}
```


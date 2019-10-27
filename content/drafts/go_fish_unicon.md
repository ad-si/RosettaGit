+++
title = "Go Fish/Unicon"
description = ""
date = 2012-01-31T04:14:43Z
aliases = []
[extra]
id = 10999
[taxonomies]
categories = []
tags = []
+++

This is a two player version of Go Fish with a 9 card deal out where the human player always goes first.
It is assumed that everyone will know what cards are booked out and what cards are fished.
The computer acts as arbitrator so there is no way to lie, cheat, or steal.

Also, this program uses a simplified deck (no suits but four of each card) similar to some children's specialty decks. 

The computer strategy relies on the following facts and public knowledge which are kept for each player in the player's public field.  
Public is a table by rank.  A value of -1 indicates unknown state, any other value is the known number of cards.
* a. Public counts can be adjusted when a player makes a book.
* b. If the opponent asks for a card an unknown can be changed to a 1 as they have at least one card of this rank.
* c. When one or more cards are surrendered during fishing , the public counts of both players can be adjusted accordingly.
* d. A card drawn requires that counts of 0 be set to unknown unless the cards were booked out.

Strategy:
* (i) Request any known cases of 3 of a kind first.
* (ii) Request from other known cards second.  The request should be in proportion to what you do not hold in your hand.  That is if you have 1 J, you should have 3 chances of requesting it.
* (iii) Guess.  This should also be in inverse proportion to what you hold in your hand.


```Icon

link printf,strings
 
global GF                                                   # global game state
record GFgame(ranks,suits,dealt,booked,deck,HP,AI)          # game record
record GFplayer(name,score,ask,hand,public)                 # player record      

procedure main()                                #: 2 player go-fish human vs. AI
   GF := GFgame("23456789TJQKA", 4, 9, "")            # game parameters
   GF.deck := scramble(repl(GF.ranks,GF.suits))       # shuffled deck
   GF.HP   := GFplayer("Human"   ,0,HPaskAI,"",table(-1))    # Human player
   GF.AI   := GFplayer("Computer",0,AIaskHP,"",table(-1))    # Computer player
   every 1 to GF.dealt do DrawCard(GF.HP) & DrawCard(GF.AI)  # Deal cards 
   printf("Go fish.\nYou vs. the computer - your turn first:\n") 
   while PlayerTurn(GF.HP,GF.AI) & PlayerTurn(GF.AI,GF.HP)   # play until done
   printf("Final score:  Computer=%d, Human=%d - %s wins.\n",
          GF.AI.score,GF.HP.score,
          if GF.HP.score > GF.AI.score then GF.HP.name else GF.AI.name)
end

procedure PlayerTurn(player,opponent)           #: Player's turn                     
   if *GF.booked < *GF.ranks then {                   # play until done
      ifbook(player)                                  # made a book ?   
      while PlayerFishes(player,opponent)             # fish while successful
      return                                          # signal continue
      }
end

procedure PlayerFishes(player,opponent)         #: player fishes for a card
   if 0 < strcnt(r := player.ask(),player.hand) then {# ask - any in hand? 
      player.public[r] <:= 1                          # either way - at least one
      opponent.public[r] := 0                         # either way -none
      if k := 0 < strcnt(r,opponent.hand) then {      # opponent has k > 0 r's?  
         printf("Yes, there was %d %s('s)\n",k,r)
         opponent.hand := deletec(opponent.hand,r)    # move from opponent
         player.hand ||:= repl(r,k)                   # to player
         player.public[r] +:= k                       # k more in hand   
         if *opponent.hand = 0 then DrawCard(opponent)# hand empty ?         
         ifbook(player)                               # made a book ? 
         if *player.hand > 0 then return              # signal success if cards left
         }
      else {
         printf("No, %s has no %s's - %s draws a card\n",opponent.name,r,player.name) 
         DrawCard(player)                             # draw from deck
         ifbook(player)                               # made a book ?
         }
      }
   else
      printf("%s is out of cards\n",player.name)   #   
end

procedure ifbook(player)                        #: determine if a book made
   every r := !GF.ranks do 
      if strcnt(r,player.hand) = GF.suits then {      # yes 
         printf("%s made a book of %s's\n",player.name,r)
         player.hand := deletec(player.hand,r)        # remove trick from hand
         player.public[r] := 0                        # no longer in hand
         player.score +:= 1                           # score 1
         GF.booked ||:= r                             # book trick
         if *player.hand = 0 then DrawCard(player)    # draw if hand empty         
         return
         }
end

procedure DrawCard(player)                      #: draw a card from the deck
   if player.hand ||:= r := GF.deck[1] then {         # card if any to player
      GF.deck[1] := ""                                # and out of deck
      every r := !GF.ranks do                         # 0's back to uncertainty 
         if player.public[r] = 0 & not find(r,GF.booked) then 
            player.public[r] := -1
      return  
      }
end  

procedure GFstrategy(player,opponent)           #: Request Strategy
   every (knowns := "") ||:= key(opponent.public)
   knowns := string(knowns**player.hand)              # we know we can win
   ask    := ""           
   every r := !knowns do
      if opponent.public[r] = GF.suits -1 then        # we can make a trick
         return r
      else 
         ask ||:= repl(r,0 < opponent.public[r])      # weighted knowns        
   if *ask > 0 then 
      return ?ask                                     # random choice by weight 
   every (guess := "") ||:=         
      repl(c := !cset(player.hand),GF.suits-strcnt(c,player.hand))
   every ( z := '') ++:= (0 = opponent.public[r := !GF.ranks],r)  
   guess := deletec(guess,z)
   if *guess = 0 then guess := (z ** player.hand)[1]  # make fruitless guess
   return ?guess
end

procedure AIaskHP()                             #: AI asks for a card
   if r := \GFstrategy(GF.AI,GF.HP) then {            # use strategy   
      printf("Computer: Do you have any %s's?\n",r)
      return r
      }
end

procedure HPaskAI()                             #: Human asks for a card
   repeat {
      printf("Your hand : %s\nYour turn : ", csort(GF.HP.hand)) 
      if find(r := map(read(),&lcase,&ucase),GF.ranks) then
         if *r = 1 & 0 < strcnt(r,GF.HP.hand) then break
         else 
            printf("Try again, you must have at least one %i in your hand\n",r) 
      else 
         printf("Invalid rank %i, must be one of %i\n",r,GF.ranks) 
      }
   printf("You: Does the computer have any %s's?\n",r)
   return r
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting] 
[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides csort,deletec,strcnt,scramble] 

Sample Game:
```txt
Go fish.
You vs. the computer - your turn first:
Your hand : 34688AAJK
Your turn : k
You: Does the computer have any K's?
Yes, there was 2 K('s)
Your hand : 34688AAJKKK
Your turn : j
You: Does the computer have any J's?
Yes, there was 1 J('s)
Your hand : 34688AAJJKKK
Your turn : a
You: Does the computer have any A's?
No, Computer has no A's - Human draws a card
Computer: Do you have any 3's?
Yes, there was 1 3('s)
Computer: Do you have any 5's?
No, Human has no 5's - Computer draws a card
Your hand : 4688AAJJKKKQ
Your turn : 4
You: Does the computer have any 4's?
Yes, there was 2 4('s)
Your hand : 444688AAJJKKKQ
Your turn : 6
You: Does the computer have any 6's?
Yes, there was 2 6('s)
Your hand : 44466688AAJJKKKQ
Your turn : 8
You: Does the computer have any 8's?
No, Computer has no 8's - Human draws a card
Computer: Do you have any 3's?
No, Human has no 3's - Computer draws a card
Your hand : 44466688AAJJKKKQQ
Your turn : q
You: Does the computer have any Q's?
No, Computer has no Q's - Human draws a card
Computer: Do you have any 3's?
No, Human has no 3's - Computer draws a card
Your hand : 444666888AAJJKKKQQ
Your turn : k
You: Does the computer have any K's?
No, Computer has no K's - Human draws a card
Computer: Do you have any 3's?
No, Human has no 3's - Computer draws a card
Your hand : 444666888AAJJKKKQQT
Your turn : t
You: Does the computer have any T's?
Yes, there was 1 T('s)
Your hand : 444666888AAJJKKKQQTT
Your turn : j
You: Does the computer have any J's?
No, Computer has no J's - Human draws a card
Computer: Do you have any 2's?
No, Human has no 2's - Computer draws a card
Your hand : 444666888AAAJJKKKQQTT
Your turn : a
You: Does the computer have any A's?
No, Computer has no A's - Human draws a card
Computer: Do you have any J's?
Yes, there was 2 J('s)
Computer: Do you have any 7's?
No, Human has no 7's - Computer draws a card
Your hand : 3444666888AAAKKKQQTT
Your turn : 4
You: Does the computer have any 4's?
Yes, there was 1 4('s)
Human made a book of 4's
Your hand : 3666888AAAKKKQQTT
Your turn : 6
You: Does the computer have any 6's?
No, Computer has no 6's - Human draws a card
Computer: Do you have any 5's?
No, Human has no 5's - Computer draws a card
Your hand : 23666888AAAKKKQQTT
Your turn : 8
You: Does the computer have any 8's?
No, Computer has no 8's - Human draws a card
Computer: Do you have any 2's?
Yes, there was 1 2('s)
Computer: Do you have any 3's?
Yes, there was 1 3('s)
Computer: Do you have any 7's?
No, Human has no 7's - Computer draws a card
Your hand : 666888AAAJKKKQQTT
Your turn : t
You: Does the computer have any T's?
No, Computer has no T's - Human draws a card
Human made a book of 6's
Computer: Do you have any 7's?
No, Human has no 7's - Computer draws a card
Your hand : 888AAAJKKKQQTT
Your turn : q
You: Does the computer have any Q's?
No, Computer has no Q's - Human draws a card
Computer: Do you have any 8's?
Yes, there was 3 8('s)
Computer made a book of 8's
Computer: Do you have any J's?
Yes, there was 1 J('s)
Computer made a book of J's
Computer: Do you have any 9's?
No, Human has no 9's - Computer draws a card
Your hand : 5AAAKKKQQTT
Your turn : a
You: Does the computer have any A's?
No, Computer has no A's - Human draws a card
Computer: Do you have any 7's?
Yes, there was 1 7('s)
Computer: Do you have any 9's?
No, Human has no 9's - Computer draws a card
Your hand : 5AAAKKKQQTT
Your turn : k
You: Does the computer have any K's?
No, Computer has no K's - Human draws a card
Computer: Do you have any T's?
Yes, there was 2 T('s)
Computer: Do you have any 9's?
Yes, there was 1 9('s)
Computer: Do you have any 2's?
No, Human has no 2's - Computer draws a card
Your hand : 5AAAKKKQQ
Your turn : 5
You: Does the computer have any 5's?
Yes, there was 2 5('s)
Your hand : 555AAAKKKQQ
Your turn : q
You: Does the computer have any Q's?
Yes, there was 1 Q('s)
Your hand : 555AAAKKKQQQ
Your turn : k
You: Does the computer have any K's?
No, Computer has no K's - Human draws a card
Computer: Do you have any 2's?
No, Human has no 2's - Computer draws a card
Your hand : 555AAAKKKQQQT
Your turn : t
You: Does the computer have any T's?
Yes, there was 3 T('s)
Human made a book of T's
Your hand : 555AAAKKKQQQ
Your turn : 5
You: Does the computer have any 5's?
No, Computer has no 5's - Human draws a card
Computer: Do you have any Q's?
Yes, there was 3 Q('s)
Computer made a book of Q's
Computer: Do you have any 7's?
No, Human has no 7's - Computer draws a card
Your hand : 2555AAAKKK
Your turn : 2
You: Does the computer have any 2's?
Yes, there was 3 2('s)
Human made a book of 2's
Your hand : 555AAAKKK
Your turn : 5
You: Does the computer have any 5's?
No, Computer has no 5's - Human draws a card
Computer: Do you have any A's?
Yes, there was 3 A('s)
Computer made a book of A's
Computer: Do you have any 7's?
No, Human has no 7's - Computer draws a card
Your hand : 3555KKK
Your turn : 3
You: Does the computer have any 3's?
Yes, there was 3 3('s)
Human made a book of 3's
Your hand : 555KKK
Your turn : k
You: Does the computer have any K's?
No, Computer has no K's - Human draws a card
Human made a book of 5's
Computer: Do you have any 7's?
No, Human has no 7's - Computer draws a card
Computer made a book of 9's
Your hand : KKK
Your turn : k
You: Does the computer have any K's?
No, Computer has no K's - Human draws a card
Human made a book of K's
Computer: Do you have any 7's?
Yes, there was 1 7('s)
Computer made a book of 7's
Final score:  Computer=6, Human=7 - Human wins.
```


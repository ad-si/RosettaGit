+++
title = "Go Fish/PureBasic"
description = ""
date = 2010-04-10T19:16:48Z
aliases = []
[extra]
id = 6914
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
If 'computer' is entered for the player's name, it will play itself.

```PureBasic
#MaxCards = 52 ;Max #of cards possible in a card collection
#ShortCardRanks$ = "2;3;4;5;6;7;8;9;10;J;Q;K;A"
#LongCardRanks$ = "deuce;three;four;five;six;seven;eight;nine;ten;jack;queen;king;ace"
#CardRankArticles$ = "a;a;a;a;a;a;an;a;a;a;a;a;an"
#CardRankPlurals$ = "s;s;s;s;es;s;s;s;s;s;s;s;s"
#NumCardRanks = 13
#MaxHistorySize = 4 ;must be 0 < #MaxHistorySize <= #NumCardRanks
#Indent$ = "   "

Structure _membersCardCollectionClass
  *vtable.i 
  size.i ;# of cards present
  card.i[#MaxCards] ;collection content, stores rank# for each card, suits aren't used
EndStructure

Interface CardCollectionObj
  init(isDeck = #False)
  count()
  countMatchingCards(rank)
  drawFrom(src.CardCollectionObj)
  pushCard(rank)
  popCard()
  removeCards(rank)
  transferCards(src.CardCollectionObj, rank)
  shuffle()
  show.s()
  sort()
EndInterface 

Structure _membersPlayerClass
  *vtable.i 
  isHuman.i 
  name.s
  score.i
  hand.CardCollectionObj
  ranksOpponentHas.i[#NumCardRanks]  ;priority 1 for requests, index = (rank# - 1)
  newRanksDrawn.i[#NumCardRanks]     ;priority 2 for requests, index = (rank# - 1)
  history.i[#MaxHistorySize]         ;priority 3 requests are selected with the help of this history
EndStructure

Interface PlayerObj
  init(deck.CardCollectionObj)
  getScore()
  getName.s()
  isHuman() ;boolean
  countCardsInHand()
  takeTurn(otherPlayer.PlayerObj,deck.CardCollectionObj)
  draw(deck.CardCollectionObj)
  shiftHistory()
  setHistory(rank)
  createDiffHistory(*hand.CardCollectionObj)
  request(dest.CardCollectionObj, rank) ;boolean
  updateScore()
EndInterface 

Enumeration
  #HumanGamePlayer
  #ComputerGamePlayer
EndEnumeration

Structure _membersGameClass
  *vtable.i 
  turn.i  ;player# whose turn it is
  deck.CardCollectionObj
  player.PlayerObj[2]
EndStructure

Interface GameObj
  play()
  displayWinner() 
EndInterface 

Procedure handleError(condition,Msg$)
  If Not condition
    MessageRequester("Error",Msg$)
    End
  EndIf 
EndProcedure

Procedure.s verboseCardInfo(rank)
  If rank > 0 And rank <= #NumCardRanks
    ProcedureReturn StringField(#LongCardRanks$, rank, ";")
  EndIf 
EndProcedure

Procedure CC_init(*this._membersCardCollectionClass, isDeck)
  Protected i
  If isDeck
    *this\size = #MaxCards - 1
    For i = 0 To #MaxCards - 1
      *this\card[i] = (i % #NumCardRanks) + 1
    Next
  Else
    *this\size = -1
  EndIf 
EndProcedure

Procedure CC_countCards(*this._membersCardCollectionClass)
  ProcedureReturn *this\size + 1
EndProcedure

Procedure CC_countMatchingCards  (*this._membersCardCollectionClass, rank)
  Protected i, count
  For i = 0 To *this\size
    If *this\card[i] = rank
      count + 1
    EndIf
  Next
  ProcedureReturn count 
EndProcedure

Procedure CC_drawFrom(*this._membersCardCollectionClass, *source.CardCollectionObj)
  Protected cardDrawn, *dest.CardCollectionObj = *this
  cardDrawn = *source\popCard()
  *dest\pushCard(cardDrawn)
  ProcedureReturn cardDrawn 
EndProcedure

Procedure CC_pushCard(*this._membersCardCollectionClass, rank)
  If *this\size < #MaxCards And (rank > 0 And rank <= #NumCardRanks)
    *this\size + 1
    *this\card[*this\size] = rank
  EndIf 
EndProcedure

Procedure CC_popCard(*this._membersCardCollectionClass)
  Protected rank
  If *this\size >= 0
    rank = *this\card[*this\size]
    *this\size - 1
  EndIf 
  ProcedureReturn rank ;returns #Null if no cards are in collection
EndProcedure

Procedure CC_removeCards(*this._membersCardCollectionClass, rank)
  ;remove all cards matching rank
  Protected i
  
  For i = *this\size To 0 Step -1
    If *this\card[i] = rank
      If *this\size < (#MaxCards - 1) And i <> *this\size
        MoveMemory(@*this\card[i + 1], @*this\card[i], SizeOf(Integer) * *this\size - i)
      EndIf 
      *this\size - 1
    EndIf 
  Next 
EndProcedure

Procedure CC_transferCards(*this._membersCardCollectionClass, *source.CardCollectionObj, rank)
  ;move all cards matching rank from source and return count
  Protected i, cardsTransfered, *src._membersCardCollectionClass = *source, blankcard
  If *source <> #Null
    For i = *src\size To 0 Step -1
      If *src\card[i] = rank
        *this\size + 1
        *this\card[*this\size] = *src\card[i]
        If *src\size < (#MaxCards - 1) And i <> *src\size
          MoveMemory(@*src\card[i + 1], @*src\card[i], SizeOf(Integer) * (*src\size - i))
        EndIf 
        *src\size - 1
        cardsTransfered + 1
      EndIf
    Next 
  EndIf 
  
  ProcedureReturn cardsTransfered
EndProcedure

Procedure CC_shuffle(*this._membersCardCollectionClass)
  Protected w, i
  If *this\size >= 0
    Dim shuffled(*this\size)
    
    For i = *this\size To 0 Step -1
      w = Random(i)
      shuffled(i) = *this\card[w]
      If w <> i
        *this\card[w] = *this\card[i]
      EndIf
    Next
    
    For i = 0 To *this\size
      *this\card[i] = shuffled(i)
    Next
  EndIf 
EndProcedure

Procedure.s CC_showCards(*this._membersCardCollectionClass)
  Protected i, output$
  
  For i = 0 To *this\size
    output$ + StringField(#ShortCardRanks$, *this\card[i],";")
    If i <> *this\size: output$ + ", ": EndIf 
  Next
  ProcedureReturn output$
EndProcedure

Procedure CC_sortCards(*this._membersCardCollectionClass)
  Protected low, high
  Protected firstIndex, lastIndex = *this\size
    
  If lastIndex > firstIndex + 1
    low = firstIndex + 1
    While low <= lastIndex
      high = low
      While high > firstIndex
        If *this\card[high] < *this\card[high - 1]
          Swap *this\card[high - 1], *this\card[high]
        Else
          Break
        EndIf
        high - 1
      Wend
      low + 1
    Wend
  EndIf
EndProcedure

Procedure newCardCollection(isDeck = #False)
  Protected *newCardCollection._membersCardCollectionClass = AllocateMemory(SizeOf(_membersCardCollectionClass))
  If *newCardCollection
    *newCardCollection\vtable = ?vTable_CardCollectionClass
    CC_init(*newCardCollection, isDeck)
  EndIf
  ProcedureReturn *newCardCollection 
EndProcedure

Procedure _player_validateRank(inputRank.s)
  ;check if inputRank is valid, allows verbose and abbrieviated entries
  Protected rankSize, result, validatedRank, i
  
  inputRank = Trim(inputRank)
  rankSize = Len(inputRank)
  Select rankSize
    Case 0
    Case 1
      inputRank = UCase(inputRank)
      If inputRank = "T": inputRank = "10": EndIf ;handle an alias
      For i = 1 To #NumCardRanks
        If inputRank = StringField(#ShortCardRanks$, i, ";")
          validatedRank = i
          Break
        EndIf 
      Next
    Default
      inputRank = LCase(inputRank)
      
      result = FindString(inputRank, " ", 1)
      If result
        inputRank = Left(inputRank, result - 1)
      EndIf
      
      result = FindString(inputRank, "s", 2)
      If result
        inputRank = Left(inputRank, result - 1)
      EndIf
      
      ;handle some aliases
      Select inputRank
        Case "10"
          inputRank = "ten"
        Case "two"
          inputRank = "deuce"
      EndSelect
      
      For i = 1 To #NumCardRanks
        If inputRank = StringField(#LongCardRanks$, i, ";")
          validatedRank = i
          Break
        EndIf 
      Next 
  EndSelect
  
  ProcedureReturn validatedRank ;returns #Null if rank not valid
EndProcedure

Procedure player_init(*this._membersPlayerClass, *deck.CardCollectionObj)
  Protected i, rankDrawn
  ;draw 9 cards from *deck and add them to player's hand
  If *deck <> #Null
    For i = 1 To 9
      rankDrawn = *this\hand\drawFrom(*deck)
      *this\newRanksDrawn[rankDrawn - 1] = #True
    Next 
  EndIf 
EndProcedure

Procedure player_getScore(*this._membersPlayerClass)
  ProcedureReturn *this\score
EndProcedure

Procedure.s player_getName(*this._membersPlayerClass)
  ProcedureReturn *this\name
EndProcedure

Procedure player_isHuman(*this._membersPlayerClass)
  ProcedureReturn *this\isHuman
EndProcedure

Procedure player_countCardsInHand(*this._membersPlayerClass)
  ProcedureReturn *this\hand\count()
EndProcedure

Procedure player_takeTurn(*this._membersPlayerClass, otherPlayer.PlayerObj, *deck.CardCollectionObj)
  Static tempHand.CardCollectionObj
  Protected player.PlayerObj = *this, *otherPlayer._membersPlayerClass = otherPlayer
  Protected request.s, rank, cardsReceived, turnOver = #False, AI_stage, i
  
  If Not tempHand
    tempHand = newCardCollection()
    handleError(tempHand,"Unable to allocate enough memory.")
  EndIf 
  
  While Not turnOver
    ;prepare request
    If player\isHuman()
      *this\hand\sort()
      Repeat
        PrintN(#CRLF$ + "Your hand: " + *this\hand\show())
        rank = *this\hand\popCard() ;peek at last card
        *this\hand\pushCard(rank)
        If *this\hand\countMatchingCards(rank) = *this\hand\count()
          Print(#CRLF$ + "You are obligated to ask for " + verboseCardInfo(rank) + StringField(#CardRankPlurals$, rank, ";") + ".")
          Break
        Else
          Print(#CRLF$ + player\getName() + ", which rank do you want?")
          rank = _player_validateRank(Input())
          Select rank
            Case #Null 
              PrintN("** That's not a valid rank, try one from your hand.")
            Case 1 To #NumCardRanks
              If *this\hand\countMatchingCards(rank)
                Break
              Else
                PrintN("** You don't have any of those cards in your hand, choose another.")  
              EndIf
          EndSelect
        EndIf 
      ForEver
    Else
      Repeat
        Select AI_stage
          Case 0
            ;request all known cards that are in both opponent's and player's hand
            rank = #Null
            For i = 1 To #NumCardRanks
              If *this\ranksOpponentHas[i - 1] And *this\hand\countMatchingCards(i)
                *this\ranksOpponentHas[i - 1] = 0
                rank = i
              EndIf
            Next 
            
            If rank
              player\shiftHistory()
              player\setHistory(rank)
            Else
              AI_stage = 1
              tempHand\init()
              For i = 1 To #NumCardRanks
                If *this\newRanksDrawn[i - 1]
                  If *this\hand\countMatchingCards(i)
                    tempHand\pushCard(i)
                  Else
                    *this\newRanksDrawn[i - 1] = #False ;card is no longer in hand
                  EndIf 
                EndIf 
              Next 
            EndIf
          Case 1
            ;request a random card from the drawn list
            tempHand\shuffle()
            rank = tempHand\popCard()
            
            If rank
              *this\newRanksDrawn[rank - 1] = #False
              player\shiftHistory()
              player\setHistory(rank)
            Else
              AI_stage = 2
            EndIf 
          Case 2 
            ;request a random card from remaining cards
            player\shiftHistory()
            player\createDiffHistory(tempHand)
            tempHand\shuffle()
            rank = tempHand\popCard()
            player\setHistory(rank)
        EndSelect
      Until rank <> #Null
      
      Print(#CRLF$ + player\getName() + " asks for all of ")
      If otherPlayer\isHuman()
        Print("your ")
      Else
        Print(otherPlayer\getName() + "'s ")
      EndIf
      PrintN(verboseCardInfo(rank) + StringField(#CardRankPlurals$, rank, ";") + ".")
    EndIf   
    
    cardsReceived = otherPlayer\request(*this\hand, rank)
    If cardsReceived
      If player\isHuman()
        Print(#CRLF$ + #Indent$ + "You are given ")
      ElseIf Not otherPlayer\isHuman()
        Print(#CRLF$ + #Indent$ + player\getName() + " is given ")
      Else
        Print(#CRLF$ + #Indent$ + "You give " + player\getName() + " ")
      EndIf 
      Print(Str(cardsReceived) + " " + verboseCardInfo(rank))
      If cardsReceived > 1
        PrintN(StringField(#CardRankPlurals$, rank, ";") + ".")
      Else 
        PrintN(".")
      EndIf 
      
      player\updateScore()
      If otherPlayer\countCardsInHand() = 0
        If Not otherPlayer\draw(*deck)
          turnOver = #True ;game over
        EndIf
      EndIf 
      If player\countCardsInHand() = 0
        If Not player\draw(*deck)
          turnOver = #True ;game over
          Continue
        Else
          AI_stage = 0
        EndIf 
      EndIf 
    Else 
      turnOver = #True
    EndIf 
  Wend
    
  ;go fish
  If *deck\count()
    If otherPlayer\isHuman()
      Print(#CRLF$ + "You tell ")
    Else 
      Print(#CRLF$ + otherPlayer\getName() + " tells ")
    EndIf
    
    If player\isHuman()
      Print("you to 'Go Fish!'")
    Else
      Print(player\getName() + " to 'Go Fish!'")
    EndIf
  EndIf 
  Repeat
    If Not player\draw(*deck)
      Break ;game over, exit loop
    EndIf
  Until player\countCardsInHand() > 0
EndProcedure

Procedure player_draw(*this._membersPlayerClass, *deck.CardCollectionObj)
  ;draw a card from *deck and display card if player is human
  ;return #True if a card was drawn, return #False if no cards drawn
  Protected isGoFish = #False, player.PlayerObj = *this
  If *deck And *deck\count()
    If *this\hand\count() = 0
      If player\isHuman()
        Print(#Indent$ + "You are")
      Else
        Print(#Indent$ + *this\name + " is")
      EndIf 
      Print(" out of cards, so ")
    Else
      isGoFish = #True
      Print(#Indent$)
    EndIf 
    
    Protected cardDrawn = *this\hand\drawFrom(*deck)
    If Not *this\hand\countMatchingCards(cardDrawn)
      *this\newRanksDrawn[cardDrawn - 1] = #True
    EndIf 
    If player\isHuman()
      If isGoFish
        Print("Y")
      Else
        Print("y")
      EndIf 
      PrintN("ou draw a card and get " + StringField(#CardRankArticles$, cardDrawn, ";") + " " + StringField(#LongCardRanks$, cardDrawn, ";") + ".")
    Else
      PrintN(*this\name + " draws a card.")
    EndIf 
    
    If *this\hand\count() > 3
      player\updateScore()
    EndIf 
    ProcedureReturn #True
  Else
    ;No more cards in deck.
    ProcedureReturn #False
  EndIf 
EndProcedure

Procedure player_shiftHistory(*this._membersPlayerClass)
  Protected i

  For i =#MaxHistorySize - 1 To 1 Step - 1
    *this\history[i] = *this\history[i - 1]
  Next 
EndProcedure

Procedure player_setHistory(*this._membersPlayerClass, rank)
  *this\history[0] = rank
EndProcedure

Procedure player_createDiffHistory(*this._membersPlayerClass, *hand.CardCollectionObj)
  ;update *hand to contain only ranks in hand but not in history
  Protected i, activeHistorySize
  ;determine history size (= min(#MaxHistorySize, uniqueRankCount)
  If *hand
    For i = 1 To #NumCardRanks
      If activeHistorySize = #MaxHistorySize: Break: EndIf 
      If *this\hand\countMatchingCards(i)
        activeHistorySize + 1
      EndIf 
    Next 
    ;add all card-ranks from player's hand
    *hand\init()
    For i = 1 To #NumCardRanks
      If *this\hand\countMatchingCards(i)
        *hand\pushCard(i)
      EndIf 
    Next 
    ;remove cards that are also in history
    For i = 0 To activeHistorySize - 1
      *hand\removeCards(*this\history[i])
    Next
  EndIf 
EndProcedure

Procedure player_request(*this._membersPlayerClass, *dest.CardCollectionObj, rank)
  ;report how many cards of rank are present in hand
  ;also mark the requested card as being in the opponent's hand
  Protected cardsPresent
  If *dest <> #Null
    cardsPresent = *dest\transferCards(*this\hand, rank)
    *this\ranksOpponentHas[rank - 1] = 1
    *this\newRanksDrawn[rank - 1] = #False ;no need to keep in two places
    ProcedureReturn cardsPresent
  Else
    ProcedureReturn #Null
  EndIf 
EndProcedure

Procedure player_updateScore(*this._membersPlayerClass)
  ;check for complete books and remove them from hand while increasing score
  Protected bookCount, bookFound, rank, player.PlayerObj = *this
  
  Repeat
    bookFound = #False
    For rank = #NumCardRanks To 1 Step -1
      If *this\hand\countMatchingCards(rank) = 4
        *this\hand\removeCards(rank)
        If player\isHuman()
          Print(#CRLF$ + #Indent$ + "You complete")
        Else
          Print(#CRLF$ + #Indent$ + *this\name + " completes")
        EndIf 
        PrintN(" a book of " + verboseCardInfo(rank) + StringField(#CardRankPlurals$, rank, ";") + ".")
        *this\ranksOpponentHas[rank - 1] = #False
        bookFound = #True
        *this\score + 1
        
        If player\ishuman()
          Print("** You now have ")
        Else
          Print("** " + player\getName() + " now has ")
        EndIf 
        Print(Str(player\getScore()) + " book"): If player\getScore() > 1: Print("s"): EndIf
        PrintN(".")
        Break
      EndIf 
    Next
  Until bookFound = #False
EndProcedure

Procedure newPlayer(isHuman = #False)
  Protected *newPlayer._membersPlayerClass = AllocateMemory(SizeOf(_membersPlayerClass))
  
  If *newPlayer
    *newPlayer\vtable = ?vTable_PlayerClass
    *newPlayer\score = 0
    If Not isHuman
      *newPlayer\name = "Computer"
    Else
      *newPlayer\isHuman = #True
      Protected name.s
      Print("What is your name?")
      name = Trim(Input())

      Select name
        Case ""
          name = "Human"
        Case "computer", "Computer"
          name = "*Computer*"
          *newPlayer\isHuman = #False
        Default
          If Left(name,1) <> UCase(Left(name,1))
            name = UCase(Left(name,1)) + LCase(Mid(name,2))
          EndIf
      EndSelect
      *newPlayer\name = name
    EndIf 
    *newPlayer\hand = newCardCollection()
    handleError(*newPlayer\hand,"Unable to initialize hand.")
  EndIf
  ProcedureReturn *newPlayer 
EndProcedure

Procedure game_Play(*this._membersGameClass)
  *this\deck\shuffle()
  Print(#CRLF$ + "Both players are dealt 9 cards.")
  *this\player[#HumanGamePlayer]\init(*this\deck)
  *this\player[#ComputerGamePlayer]\init(*this\deck)
  *this\player[#HumanGamePlayer]\updateScore()
  *this\player[#ComputerGamePlayer]\updateScore()
  
  PrintN(#Indent$ + *this\player[*this\turn ! 1]\getName() + " will go first.")
  PrintN(#CRLF$ + "-----------------------------------------------------------------------------")
  ;take turns until all books scored (i.e. no cards are in deck or hands)
  Repeat
    *this\turn ! 1
    *this\player[*this\turn]\takeTurn(*this\player[*this\turn ! 1], *this\deck)
    PrintN(#CRLF$ + "-----------------------------------------------------------------------------")
  Until *this\deck\count() = 0 And *this\player[*this\turn]\countCardsInHand() = 0
EndProcedure

Procedure game_DisplayWinner(*this._membersGameClass)
  Protected winningPlayer = #HumanGamePlayer
  
  If *this\player[#ComputerGamePlayer]\getScore() > *this\player[#HumanGamePlayer]\getScore()
    winningPlayer = #ComputerGamePlayer
  EndIf 
  PrintN(*this\player[winningPlayer]\getName() + " won with a score of " + Str(*this\player[winningPlayer]\getScore()) + " books.")
EndProcedure

Procedure newGame()
  Protected *newGame._membersGameClass = AllocateMemory(SizeOf(_membersGameClass))
  If *newGame
    *newGame\vtable = ?vTable_GameClass
    *newGame\player[#HumanGamePlayer] = newPlayer(#True) ;'human'
    *newGame\player[#ComputerGamePlayer] = newPlayer()   ;computer
    *newGame\deck = newCardCollection(#True)
    If *newGame\player[#HumanGamePlayer] = #Null Or *newGame\player[#ComputerGamePlayer] = #Null Or *newGame\deck = #Null
      PrintN("Unable to initialize.") 
      End ;all allocated memory is automatically freed at program's end
    EndIf 
    
    *newGame\turn = Random(1)
  EndIf 
  ProcedureReturn *newGame
EndProcedure

DataSection
  vTable_GameClass:
  Data.i @game_Play()
  Data.i @game_DisplayWinner()
  
  vTable_PlayerClass:
  Data.i @player_init()
  Data.i @player_getScore()
  Data.i @player_getName()
  Data.i @player_isHuman()
  Data.i @player_countCardsInHand()
  Data.i @player_takeTurn()
  Data.i @player_draw()
  Data.i @player_shiftHistory()
  Data.i @player_setHistory()
  Data.i @player_createDiffHistory()
  Data.i @player_request()
  Data.i @player_updateScore()
  
  vTable_CardCollectionClass:
  Data.i @CC_init()
  Data.i @CC_countCards()
  Data.i @CC_countMatchingCards()
  Data.i @CC_drawFrom()
  Data.i @CC_pushCard()
  Data.i @CC_popCard()
  Data.i @CC_removeCards()
  Data.i @CC_transferCards()
  Data.i @CC_shuffle()
  Data.i @CC_showCards()
  Data.i @CC_sortCards()
EndDataSection

handleError(OpenConsole(), "Couldn't open console.")
PrintN("
### =============
")
PrintN("Let's play GO FISH.")
PrintN("
### =============
")
Define game.GameObj = newGame()
handleError(game,"Couldn't initialize game.")

game\play()

PrintN(#CRLF$ + "Game over.")
game\displayWinner()

PrintN(#CRLF$ + "Press Enter to exit.")
Input()
```


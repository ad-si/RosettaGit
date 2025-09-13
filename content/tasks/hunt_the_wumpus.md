+++
title = "Hunt The Wumpus"
description = ""
date = 2019-09-03T19:09:00Z
aliases = []
[extra]
id = 19106
[taxonomies]
categories = ["task", "Games"]
tags = []
+++

## Task

Create a simple implementation of the classic textual game [http://en.wikipedia.org/wiki/Hunt_the_Wumpus Hunt The Wumpus].

The rules are:

The game is set in a cave that consists of a 20 room labyrinth.
Each room is connected to 3 other rooms (the cave is modeled after the
vertices of a dodecahedron).
The objective of the player is to find and kill the horrendous beast Wumpus that lurks in the cave.

The player has 5 arrows.
If they run out of arrows before killing the Wumpus, the player loses the game.

In the cave there are:
* One Wumpus
* Two giant bats
* Two bottomless pits

If the player enters a room with the Wumpus, he is eaten by it and the game is lost.

If the player enters a room with a bottomless pit, he falls into it and the game is lost.

If the player enters a room with a giant bat, the bat takes him and transports him into a random empty room.

Each turn the player can either walk into an adjacent room or shoot into an adjacent room.

Whenever the player enters a room, he "senses" what happens in adjacent rooms.
The messages are:

* Nearby Wumpus: "You smell something terrible nearby."
* Nearby bat: "You hear a rustling."
* Nearby pit: "You feel a cold wind blowing from a nearby cavern."

When the player shoots, he wins the game if he is shooting in the room with the Wumpus.
If he shoots into another room, the Wumpus has a 75% of chance of waking up and moving into an adjacent room: if this is the room with the player, he eats him up and the game is lost.


## AutoHotkey

See [[Hunt_The_Wumpus/AutoHotkey]]


## C#

See [[Hunt_The_Wumpus/CSharp]].


## Go

I've tried to stick as closely as possible to the task description but, as some points are unclear, I've had to make the following assumptions:

1. The bats and the pits are in separate rooms but the Wumpus can be in any room whether there's another hazard in it or not.

2. The game starts with the player in room 1 which initially contains no hazards.

3. If a bat transports the player to a random empty room, it returns to its original room.

4. If the Wumpus wakes up and moves to an 'adjacent' room, it means a room adjacent to the Wumpus not necessarily the player.

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "math/rand"
    "os"
    "strconv"
    "strings"
    "time"
)

var cave = map[int][3]int{
    1: {2, 3, 4}, 2: {1, 5, 6}, 3: {1, 7, 8}, 4: {1, 9, 10}, 5: {2, 9, 11},
    6: {2, 7, 12}, 7: {3, 6, 13}, 8: {3, 10, 14}, 9: {4, 5, 15}, 10: {4, 8, 16},
    11: {5, 12, 17}, 12: {6, 11, 18}, 13: {7, 14, 18}, 14: {8, 13, 19},
    15: {9, 16, 17}, 16: {10, 15, 19}, 17: {11, 20, 15}, 18: {12, 13, 20},
    19: {14, 16, 20}, 20: {17, 18, 19},
}

var player, wumpus, bat1, bat2, pit1, pit2 int

var arrows = 5

func isEmpty(r int) bool {
    if r != player && r != wumpus && r != bat1 && r != bat2 && r != pit1 && r != pit2 {
        return true
    }
    return false
}

func sense(adj [3]int) {
    bat := false
    pit := false
    for _, ar := range adj {
        if ar == wumpus {
            fmt.Println("You smell something terrible nearby.")
        }
        switch ar {
        case bat1, bat2:
            if !bat {
                fmt.Println("You hear a rustling.")
                bat = true
            }
        case pit1, pit2:
            if !pit {
                fmt.Println("You feel a cold wind blowing from a nearby cavern.")
                pit = true
            }
        }
    }
    fmt.Println()
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func plural(n int) string {
    if n != 1 {
        return "s"
    }
    return ""
}

func main() {
    rand.Seed(time.Now().UnixNano())
    player = 1
    wumpus = rand.Intn(19) + 2 // 2 to 20
    bat1 = rand.Intn(19) + 2
    for {
        bat2 = rand.Intn(19) + 2
        if bat2 != bat1 {
            break
        }
    }
    for {
        pit1 = rand.Intn(19) + 2
        if pit1 != bat1 && pit1 != bat2 {
            break
        }
    }
    for {
        pit2 = rand.Intn(19) + 2
        if pit2 != bat1 && pit2 != bat2 && pit2 != pit1 {
            break
        }
    }
    scanner := bufio.NewScanner(os.Stdin)
    for {
        fmt.Printf("\nYou are in room %d with %d arrow%s left\n", player, arrows, plural(arrows))
        adj := cave[player]
        fmt.Printf("The adjacent rooms are %v\n", adj)
        sense(adj)
        var room int
        for {
            fmt.Print("Choose an adjacent room : ")
            scanner.Scan()
            room, _ = strconv.Atoi(scanner.Text())
            if room != adj[0] && room != adj[1] && room != adj[2] {
                fmt.Println("Invalid response, try again")
            } else {
                break
            }
        }
        check(scanner.Err())
        var action byte
        for {
            fmt.Print("Walk or shoot w/s : ")
            scanner.Scan()
            reply := strings.ToLower(scanner.Text())
            if len(reply) != 1 || (len(reply) == 1 && reply[0] != 'w' && reply[0] != 's') {
                fmt.Println("Invalid response, try again")
            } else {
                action = reply[0]
                break
            }
        }
        check(scanner.Err())
        if action == 'w' {
            player = room
            switch player {
            case wumpus:
                fmt.Println("You have been eaten by the Wumpus and lost the game!")
                return
            case pit1, pit2:
                fmt.Println("You have fallen down a bottomless pit and lost the game!")
                return
            case bat1, bat2:
                for {
                    room = rand.Intn(19) + 2
                    if isEmpty(room) {
                        fmt.Println("A bat has transported you to a random empty room")
                        player = room
                        break
                    }
                }
            }
        } else {
            if room == wumpus {
                fmt.Println("You have killed the Wumpus and won the game!!")
                return
            } else {
                chance := rand.Intn(4) // 0 to 3
                if chance > 0 {        // 75% probability
                    wumpus = cave[wumpus][rand.Intn(3)]
                    if player == wumpus {
                        fmt.Println("You have been eaten by the Wumpus and lost the game!")
                        return
                    }
                }
            }
            arrows--
            if arrows == 0 {
                fmt.Println("You have run out of arrows and lost the game!")
                return
            }
        }
    }
}
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Wumpus.bas"
110 RANDOMIZE
120 NUMERIC RO(1 TO 20,1 TO 3),LO(1 TO 20),WPOS
130 LET ARROWS=5:LET L=1
140 CALL INIT
150 DO
160   PRINT :PRINT "You are in room";L
170   PRINT "Tunnels lead to ";RO(L,1);RO(L,2);RO(L,3)
180   IF MON(1) THEN PRINT "You smell something terrible nearby."
190   IF MON(2) OR MON(3) THEN PRINT "You hear a rustling."
200   IF MON(4) OR MON(5) THEN PRINT "You feel a cold wind blowing from a nearby cavern."
210   PRINT :PRINT "Shoot or move? (S-M)"
220   DO
230     LET K$=UCASE$(INKEY$)
240   LOOP UNTIL K$="S" OR K$="M"
250   IF K$="M" THEN ! Move
260     INPUT PROMPT "No. of rooms: ":I$
270     LET I=VAL(I$)
280     IF CHK(I) THEN
290       LET L=I
300     ELSE
310       PRINT "Not possibile."
320     END IF
330   ELSE  ! Shoot
340     INPUT PROMPT "Where? ":I$
350     LET I=VAL(I$)
360     IF CHK(I) THEN
370       IF LO(I)=1 THEN
380         PRINT "You kill the Monster Wumpus.":PRINT "You win.":EXIT DO
390       ELSE
400         PRINT "Arrows aren't that crooked - Try another room."
410         IF RND(4)<3 THEN
420           PRINT "You woke the Wumpus and he moved."
430           LET LO(WPOS)=0:LET WPOS=RO(WPOS,RND(2)+1):LET LO(WPOS)=1
440         END IF
450         LET ARROWS=ARROWS-1
460         IF ARROWS=0 THEN PRINT "You ran out of arrows.":EXIT DO
470       END IF
480     ELSE
490       PRINT "Not possibile."
500     END IF
510   END IF
520   SELECT CASE LO(L)
530   CASE 1
540     PRINT "You eaten by Wumpus.":EXIT DO
550   CASE 2,3
560     PRINT "A giant bat takes you in another room.":LET I=L
570     DO
580       LET L=RND(19)+1
590     LOOP UNTIL I<>L
600   CASE ELSE
610   END SELECT
620   IF LO(L)=4 OR LO(L)=5 THEN PRINT "You fall into a bottomless pit.":EXIT DO
630 LOOP
640 DEF MON(X)=X=LO(RO(L,1)) OR X=LO(RO(L,2)) OR X=LO(RO(L,3))
650 DEF CHK(X)=X=RO(L,1) OR X=RO(L,2) OR X=RO(L,3)
660 DEF INIT
670   TEXT 40:PRINT "Hunt the Wumpus";CHR$(241)
680   FOR I=1 TO 20 ! Create the cave
690     LET LO(I)=0
700     FOR J=1 TO 3
710       READ RO(I,J)
720     NEXT
730   NEXT
740   LET WPOS=RND(19)+2:LET LO(WPOS)=1
750   FOR I=2 TO 5
760     DO
770       LET T=RND(19)+2
780     LOOP UNTIL LO(T)=0
790     LET LO(T)=I
800   NEXT
810 END DEF
820 DATA 2,6,5,3,8,1,4,10,2,5,2,3,1,14,4,15,1,7,17,6,8,7,2,9,18,8,10,9,3,11
830 DATA 19,10,12,11,4,13,20,12,14,5,11,13,6,16,14,20,15,17,16,7,18,17,9,19,18,11,20,19,13,16
```



## Java

See [[Hunt_The_Wumpus/Java]].


## Javascript

See [[Hunt_The_Wumpus/Javascript]].



## Julia

A translation from the original Basic, using the original Basic code and text at https://github.com/kingsawyer/wumpus/blob/master/wumpus.basic as a guide.

```julia
const starttxt = """

     ATTENTION ALL WUMPUS LOVERS!!!
     THERE ARE NOW TWO ADDITIONS TO THE WUMPUS FAMILY
     OF PROGRAMS.

      WUMP2:  SOME DIFFERENT CAVE ARRANGEMENTS
      WUMP3:  DIFFERENT HAZARDS

"""

const helptxt = """
     WELCOME TO 'HUNT THE WUMPUS'
      THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM
     HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A
     DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW
     WHAT A DODECAHEDRON IS, ASK SOMEONE)

         HAZARDS:
     BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM
         IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)
     SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU
         GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER
         ROOM AT RANDOM. (WHICH MIGHT BE TROUBLESOME)

         WUMPUS:
     THE WUMPUS IS NOT BOTHERED BY THE HAZARDS (HE HAS SUCKER
     FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY
     HE IS ASLEEP. TWO THINGS WAKE HIM UP: YOUR ENTERING
     HIS ROOM OR YOUR SHOOTING AN ARROW.
         IF THE WUMPUS WAKES, HE MOVES (P=.75) ONE ROOM
     OR STAYS STILL (P=.25). AFTER THAT, IF HE IS WHERE YOU
     ARE, HE EATS YOU UP (& YOU LOSE!)

         YOU:
     EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW
       MOVING: YOU CAN GO ONE ROOM (THRU ONE TUNNEL)
       ARROWS: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT.
       EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING
       THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.
       IF THE ARROW CAN'T GO THAT WAY (IE NO TUNNEL) IT MOVES
       AT RANDOM TO THE NEXT ROOM.
         IF THE ARROW HITS THE WUMPUS, YOU WIN.
         IF THE ARROW HITS YOU, YOU LOSE.

        WARNINGS:
        WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,
        THE COMPUTER SAYS:
     WUMPUS-  'I SMELL A WUMPUS'
     BAT   -  'BATS NEARBY'
     PIT   -  'I FEEL A DRAFT'

 """

function queryprompt(query, choices, choicetxt="")
    carr = map(x -> uppercase(strip(string(x))), collect(choices))
    while true
        print(query, " ", choicetxt == "" ? carr : choicetxt, ": ")
        choice = uppercase(strip(readline(stdin)))
        if choice in carr
            return choice
        end
        println()
    end
end

function wumpushunt(cheatmode = false)
    println(starttxt)
    arrows = 5
    rooms = Vector{Vector{Int}}()
    push!(rooms, [2,6,5], [3,8,1], [4,10,2], [5,2,3], [1,14,4], [15,1,7],
        [17,6,8], [7,2,9], [18,8,10], [9,3,11], [19,10,12], [11,4,13],
        [20,12,14], [5,11,13], [6,16,14], [20,15,17], [16,7,18],
        [17,9,19], [18,11,20], [19,13,16])
    roomcontents = shuffle(push!(fill("Empty", 15), "Bat", "Bat", "Pit", "Pit", "Wumpus"))
    randnextroom(room) = rand(rooms[room])
    newplayerroom(cr, range = 40) = (for i in 1:range cr = randnextroom(cr) end; cr)

    function senseroom(p)
        linkedrooms = rooms[p]
        if cheatmode
            println("linked rooms are $(rooms[p]), which have $(roomcontents[rooms[p][1]]),
                $(roomcontents[rooms[p][2]]), $(roomcontents[rooms[p][3]])")
        end
        if any(x -> roomcontents[x] == "Wumpus", linkedrooms)
            println("I SMELL A WUMPUS!")
        end
        if any(x -> roomcontents[x] == "Pit", linkedrooms)
            println("I FEEL A DRAFT")
        end
        if any(x -> roomcontents[x] == "Bat", linkedrooms)
            println("BATS NEARBY!")
        end
    end

    function arrowflight(arrowroom)
        if roomcontents[arrowroom] == "Wumpus"
            println("AHA! YOU GOT THE WUMPUS!")
            return("win")
        elseif any(x -> roomcontents[x] == "Wumpus", rooms[arrowroom])
            numrooms = rand([0, 1, 2, 3])
            if numrooms > 0
                println("...OOPS! BUMPED A WUMPUS!")
                wroom = rooms[arrowroom][findfirst(x -> roomcontents[x] == "Wumpus", rooms[arrowroom])]
                for i in 1:3
                    tmp = wroom
                    wroom = rand(rooms[wroom])
                    if wroom == playerroom
                        println("TSK TSK TSK- WUMPUS GOT YOU!")
                        return "lose"
                    else
                        roomcontents[tmp] = roomcontents[wroom]
                        roomcontents[wroom] = "Wumpus"
                    end
                end
            end
        elseif arrowroom == playerroom
            println("OUCH! ARROW GOT YOU!")
            return "lose"
        end
        return ""
    end

    println("HUNT THE WUMPUS")
    playerroom = 1
    while true
        playerroom = newplayerroom(playerroom)
        if roomcontents[playerroom] == "Empty"
            break
        end
    end
    while arrows > 0
        senseroom(playerroom)
        println("YOU ARE IN ROOM $playerroom. TUNNELS LEAD TO ", join(rooms[playerroom], ";"))
        choice = queryprompt("SHOOT OR MOVE (H FOR HELP)", ["S", "M", "H"])
        if choice == "M"
            choice = queryprompt("WHERE TO", rooms[playerroom])
            playerroom = parse(Int, choice)
            if roomcontents[playerroom] == "Wumpus"
                println("TSK TSK TSK- WUMPUS GOT YOU!")
                return "lose"
            elseif roomcontents[playerroom] == "Pit"
                println("YYYIIIIEEEE . . . FELL IN PIT")
                return "lose"
            elseif roomcontents[playerroom] == "Bat"
                senseroom(playerroom)
                println("ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!")
                playerroom = newplayerroom(playerroom, 10)
            end
        elseif choice == "S"
            distance = parse(Int, queryprompt("NO. OF ROOMS(1-5)", 1:5))
            choices = zeros(Int, 5)
            arrowroom = playerroom
            for i in 1:distance
                choices[i] = parse(Int, queryprompt("ROOM #", 1:20, "1-20"))
                while i > 2 && choices[i] == choices[i-2]
                    println("ARROWS AREN'T THAT CROOKED - TRY ANOTHER ROOM")
                    choices[i] = parse(Int, queryprompt("ROOM #", 1:20, "1-20"))
                end
                arrowroom = choices[i]
            end
            arrowroom = playerroom
            for rm in choices
                if rm != 0
                    if !(rm in rooms[arrowroom])
                        rm = rand(rooms[arrowroom])
                    end
                    arrowroom = rm
                    if (ret = arrowflight(arrowroom)) != ""
                        return ret
                    end
                end
            end
            arrows -= 1
            println("MISSED")
        elseif choice == "H"
            println(helptxt)
        end
    end
    println("OUT OF ARROWS.\nHA HA HA - YOU LOSE!")
    return "lose"
end

while true
    result = wumpushunt()
    println("Game over. You $(result)!")
    if queryprompt("Play again?", ["Y", "N"]) == "N"
        break
    end
end

```



## M2000 Interpreter

For Windows only (in Linux you hear nothing, using Wine 3.6): In Sense() you can change Print with Speech so you Hear your sense;


```M2000 Interpreter

Module WumpusGame {
      Print "Game: Hunt The Wumpus"
      Arrows=5
      Dim Room(1 to 20)
      Room(1)=(2,6,5),(3,8,1),(4,10,2),(5,2,3),(1,14,4)
      Room(6)=(15,1,7),(17,6,8),(7,2,9),(18,8,10),(9,3,11)
      Room(11)=(19,10,12),(11,4,13),(20,12,14),(5,11,13), (6,16,14)
      Room(16)=(20,15,17),(16,7,18),(17,9,19),(18,11,20),(19,13,16)
      Enum Things {EmptyRoom, Bat1, Bat2, Pit1, Pit2, Wumpus}
      Dim Content(1 to 20)=EmptyRoom
      i=each(Things,2)  ' from 2 to End
      While i {
            r=random(1,20)
            if Content(r)<>EmptyRoom then restart
            Content(r)=Eval(i)
      }
      WumpusPos=r
      PlayerPos=-1
      TranspotPlayer()
      Done=False
      \\ Help is statement but here used as variable
      Help=False
      While Arrows>0 And Not Done {
            Sense()
            Print "W- Walk, T - Throw Arrow, G - Give up or H for Help"
            a$=Ucase$(Key$)
            If a$="W" Then {
                  Print "Choose Tunnel to Walk: 1, 2 or 3"
                  r=Val("0"+Key$)-1
                  if r>=0 and r<=2 then {
                        PlayerPos=Array(room(PlayerPos), r)
                        Select Case Content(PlayerPos)
                        Case Wumpus
                        Eaten()
                        Case Pit1, Pit2
                        {
                              Arrows=0
                              Print "You fall to a bottomless pit;"
                        }
                        Case Bat1, Bat2
                        {
                              Print "A giant bat takes you in another room;"
                              TranspotPlayer()
                        }
                        End Select
                  }
            } Else.if a$="T" Then {
                  Arrows--
                  Print "Choose Tunnel to Throw Arrow: 1, 2  or 3"
                  r=Val("0"+Key$)-1
                  if r>=0 and r<=2 then {
                        i=room(PlayerPos)
                        If Content(Array(i, r))=Wumpus then {
                              Done=True
                      } Else.if random(1,4)<4 then WakeWumpus()
                  }
            } Else.if a$="G" Then {
                   Arrows=0
            } Else.if a$="H" Then Help~
      }
      If Done then Print "You kill the Monster Wumpus; You Win.": Exit
      Print "You loose."

      Sub TranspotPlayer()
            local r=random(1,20)
            While Content(r)<>EmptyRoom {r=random(1,20)}
            PlayerPos=r
      End Sub
      Sub WakeWumpus()
            local j=array(room(WumpusPos),random(0,2))
            If content(j)=EmptyRoom Then {
                  swap content(j), content(WumpusPos)
                  WumpusPos=j
                  If WumpusPos=PlayerPos then Eaten()
            }
      End Sub
      Sub Eaten()
            Arrows=0
            Print "You eaten by Wumpus;"
      End Sub
      Sub Sense()
            local k=Room(PlayerPos)
            local j=each(k), Wumpus_near, bat_near, pit_near
            If Help then Print "Player Room:";PlayerPos, "Wumpus Room:";WumpusPos
            While j {
                  If Help Then Print "Tunnel:";j^+1, "Room:";Array(j), "Content:";eval$(content(array(j)))
                  Select Case content(array(j))
                  Case Bat1, Bat2
                  bat_near=True
                  Case Pit1, Pit2
                  pit_near=True
                  Case Wumpus
                  Wumpus_near=True
                  End Select
            }
            If Wumpus_near Then Print "You smell something terrible nearby."
            If bat_near Then Print "You hear a rustling."
            if pit_near Then Print "You feel a cold wind blowing from a nearby cavern."
      End Sub
}
WumpusGame


```



## Perl 6

See [[Hunt_The_Wumpus/Perl_6]]


## Phix

See [[Hunt_The_Wumpus/Phix]].


## Prolog


Prolog package available here:

https://github.com/jburse/jekejeke-samples/raw/master/pack/games/wumpus-1.0.0.zip


```prolog

/**
 * Simple text based dungeon game in Prolog.
 *
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 *
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 *
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 *
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 *
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */

/**
 * Obtained rights, copyright notice of BASIC version found
 * in The Best of Creative Computing Volume 1 (published 1976)
 * https://www.atariarchives.org/bcc1/showpage.php?page=247
 * and that we translated to Prolog.
 *
 * 0010  REM- HUNT THE WUMPUS
 * 0015  REM:  BY GREGORY YOB
 *
 * Game must have been create before, we assume 1972 since
 * the German Wikipedia mentions this date. The Englis Wikipedia
 * refers probably to a TI-99/4A version from 1973.
 */

:- module(wumpus, [wumpus/0]).

:- use_module(console).
:- use_module(library(advanced/arith)).
:- use_module(library(basic/lists)).
:- use_module(library(basic/random)).

% wumpus
wumpus :- preamble,
   locate(L),
   play(L).

% originally: 0350  REM-SET# ARROWS
% play(+List)
play(L) :-
   write('HUNT THE WUMPUS'), nl,
   play(L, L, 5).

% 0400  REM-MOVE OR SHOOT
% play(+List, +List, +Integer)
play(M, L, A) :-
   location(L),
   choose(O),
   (  O = 2
   -> move(L, H),
      check(H, R, F),
      B = A
   ;  rooms(N),
      path(N, [], P),
      shoot(P, L, A, R, B, F)),
   result(F, M, R, B).

% result(+Integer, +List, +List, +Integer)
result(0, M, L, A) :- !,
   play(M, L, A).
result(F, M, _, _) :-
   (  F = -1
   -> write('HA HA HA - YOU LOSE!'), nl
   ;  write('HEE HEE HEE - THE WUMPUS''LL GETCHA NEXT TIME!!'), nl),
   write('SAME SET-UP (Y-N)? '), flush_output,
   read_line(I),
   (  I \== 'Y'
   -> locate(L)
   ;  L = M),
   play(L).

% originally: 2500  REM-CHOOSE OPTION
% choose(-Integer)
choose(O) :- repeat,
   write('SHOOT OR MOVE (S-M)? '), flush_output,
   read_line(I),
   (  'S' = I
   -> O = 1
   ;  'M' = I
   -> O = 2; fail), !.

/************************************************************/
/* Move                                                     */
/************************************************************/

% originally: 4000  REM- MOVE ROUTINE
% move(+List, -List)
move([X|L], [P|L]) :- repeat,
   write('WHERE TO? '), flush_output,
   read_line(H),
   atom_number(H, P),
   1 =< P,
   P =< 20,
   (  edge(X, P) -> true
   ;  P = X -> true
   ;  write('NOT POSSIBLE - '), fail), !.

% originally: 4120  REM-CHECK FOR HAZARDS
% check(+List, -List, -Integer)
check([X,Y|L], R, F) :-
   X = Y, !,
   write('...OOPS! BUMPED A WUMPUS!'), nl,
   bump([X,Y|L], R, F).
check([X,_,Z,T,_,_], _, -1) :-
   (  X = Z
   ;  X = T), !,
   write('YYYIIIIEEEE . . . FELL IN PIT'), nl.
check([X,Y,Z,T,U,V], L, F) :-
   (  X = U
   ;  X = V), !,
   write('ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!'), nl,
   fna(P),
   check([P,Y,Z,T,U,V], L, F).
check(L, L, 0).

% originally: 3370  REM-MOVE WUMPUS ROUTINE
% bump(+List, -List, -Integer)
bump([X,Y|L], [X,P|L], F) :-
   fnc(C),
   (  C = 4
   -> P = Y
   ;  findall(P, edge(Y, P), H),
      nth1(C, H, P)),
   (  X = P
   -> write('TSK TSK TSK- WUMPUS GOT YOU!'), nl,
      F = -1
   ;  F = 0).

/************************************************************/
/* Shoot                                                    */
/************************************************************/

% shoot(+List, +List, +Integer, -List, -Integer, -Integer)
shoot(P, [X|L], A, R, B, G) :-
   arrow(P, X, [X|L], F),
   missed(F, [X|L], A, R, B, G).

% missed(+Integer, +List, +Integer, -List, -Integer, -Integer)
missed(0, L, A, R, B, F) :- !,
   write('MISSED'), nl,
   bump(L, R, G),
   ammo(G, A, B, F).
missed(F, L, A, L, A, F).

% originally: 3250  REM-AMMO CHECK
% ammo(+Integer, +Integer, -Integer, -Integer)
ammo(0, A, B, F) :- !,
   B is A-1,
   (  B = 0
   -> F = -1
   ;  F = 0).
ammo(F, A, A, F).

% originally: 3120  REM-SHOOT ARROW
% arrow(+List, +Integer, +List, -Integer)
arrow([], _, _, 0).
arrow([Y|P], X, L, F) :-
   follow(X, Y, Z),
   hit(Z, L, P, F).

% follow(+Integer, +Integer, -Integer)
follow(X, Y, Z) :-
   edge(X, Y), !,
   Z = Y.
follow(X, _, Z) :-
   fnb(C),
   findall(Z, edge(X, Z), H),
   nth1(C, H, Z).

% originally: 3290  REM-SEE IF ARROW IS AT L(1) OR L(2)
% hit(+Integer, +List, +List, -Integer)
hit(Z, [_,Y|_], _, 1) :-
   Z = Y, !,
   write('AHA! YOU GOT THE WUMPUS!'), nl.
hit(Z, [X|_], _, -1) :-
   Z = X, !,
   write('OUCH! ARROW GOT YOU!'), nl.
hit(Z, L, P, F) :-
   arrow(P, Z, L, F).

% originally: 3020  REM-PATH OF ARROW
% rooms(-Integer)
rooms(N) :- repeat,
   write('NO. OF ROOMS(1-5)? '), flush_output,
   read_line(H),
   atom_number(H, N),
   1 =< N,
   N =< 5, !.

% path(+Integer, +List, -List)
path(0, _, []) :- !.
path(N, L, [P|R]) :- repeat,
   write('ROOM #? '), flush_output,
   read_line(H),
   atom_number(H, P),
   (  L = [_,Q|_],
      Q = P
   -> write('ARROWS AREN''T THAT CROOKED - TRY ANOTHER ROOM'), nl, fail; true), !,
   M is N-1,
   path(M, [P|L], R).

/************************************************************/
/* Dungeon                                                  */
/************************************************************/

% originally: 2000  REM-PRINT LOCATION & HAZARD WARNINGS
% location(+List)
location([X,Y,Z,T,U,V]) :-
   (  edge(X, Y)
   -> write('I SMELL A WUMPUS!'), nl; true),
   (  (  edge(X, Z)
      ;  edge(X, T))
   -> write('I FEEL A DRAFT'), nl; true),
   (  (  edge(X, U)
      ;  edge(X, V))
   -> write('BATS NEARBY'), nl; true),
   write('YOU ARE IN ROOM '),
   write(X), nl,
   write('TUNNELS LEAD TO'),
   (  edge(X, R),
      write(' '),
      write(R), fail; true), nl.

% originally: 0200  REM-LOCATE L ARRAY ITEMS
% originally: 0210  REM-1-YOU,2-WUMPUS,3&4-PITS,5&6-BATS
% locate(-List)
locate(L) :-
   length(L, 6), repeat,
   maplist(fna, L),
   \+ (  append(R, [X|_], L),
         member(X, R)), !.

% fna(-Integer)
fna(X) :-
   X is random(20)+1.
% fnb(-Integer)
fnb(X) :-
   X is random(3)+1.
% fnc(-Integer)
fnc(X) :-
   X is random(4)+1.

% Originally: 0068  REM- SET UP CAVE (DODECAHEDRAL NODE LIST)
% edge(-Integer, -Integer)
edge(1, 2).
edge(1, 5).
edge(1, 8).
edge(2, 1).
edge(2, 3).
edge(2, 10).
edge(3, 2).
edge(3, 4).
edge(3, 12).
edge(4, 3).
edge(4, 5).
edge(4, 14).
edge(5, 1).
edge(5, 4).
edge(5, 6).
edge(6, 5).
edge(6, 7).
edge(6, 15).
edge(7, 6).
edge(7, 8).
edge(7, 17).
edge(8, 1).
edge(8, 7).
edge(8, 9).
edge(9, 8).
edge(9, 10).
edge(9, 18).
edge(10, 2).
edge(10, 9).
edge(10, 11).
edge(11, 10).
edge(11, 12).
edge(11, 19).
edge(12, 3).
edge(12, 11).
edge(12, 13).
edge(13, 12).
edge(13, 14).
edge(13, 20).
edge(14, 4).
edge(14, 13).
edge(14, 15).
edge(15, 6).
edge(15, 14).
edge(15, 16).
edge(16, 15).
edge(16, 17).
edge(16, 20).
edge(17, 7).
edge(17, 16).
edge(17, 18).
edge(18, 9).
edge(18, 17).
edge(18, 19).
edge(19, 11).
edge(19, 18).
edge(19, 20).
edge(20, 13).
edge(20, 16).
edge(20, 19).

/************************************************************/
/* Instructions                                             */
/************************************************************/

% Originally: 1000  REM-INSTRUCTIONS
% preamble
preamble :-
   write('INSTRUCTIONS (Y-N)? '), flush_output,
   read_line(I),
   (  I \== 'N' -> instructions; true).

% instructions
instructions :-
   write('WELCOME TO ''HUNT THE WUMPUS'''), nl,
   write('  THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM'), nl,
   write('HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A'), nl,
   write('DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON''T KNOW'), nl,
   write('WHAT A DODECAHEDRON IS, ASK SOMEONE)'), nl, nl,
   write('     HAZARDS:'), nl,
   write(' BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM'), nl,
   write('     IF YOU GO THERE, YOU FALL INTO THE PIT (& LOSE!)'), nl,
   write(' SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU'), nl,
   write('     GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER'), nl,
   write('     ROOM AT RANDOM. (WHICH MIGHT BE TROUBLESOME)'), nl, nl,
   write('     WUMPUS:'), nl,
   write(' THE WUMPUS IS NOT BOTHERED BY THE HAZARDS (HE HAS SUCKER'), nl,
   write(' FEET AND IS TOO BIG FOR A BAT TO LIFT).  USUALLY'), nl,
   write(' HE IS ASLEEP. TWO THINGS WAKE HIM UP: YOUR ENTERING'), nl,
   write(' HIS ROOM OR YOUR SHOOTING AN ARROW.'), nl,
   write('     IF THE WUMPUS WAKES, HE MOVES (P=.75) ONE ROOM'), nl,
   write(' OR STAYS STILL (P=.25). AFTER THAT, IF HE IS WHERE YOU'), nl,
   write(' ARE, HE EATS YOU UP (& YOU LOSE!)'), nl, nl,
   write('     YOU:'), nl,
   write(' EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW'), nl,
   write('   MOVING: YOU CAN GO ONE ROOM (THRU ONE TUNNEL)'), nl,
   write('   ARROWS: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT.'), nl,
   write('   EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING'), nl,
   write('   THE COMPUTER THE ROOM#S YOU WANT THE ARROW TO GO TO.'), nl,
   write('   IF THE ARROW CAN''T GO THAT WAY (IE NO TUNNEL) IT MOVES'), nl,
   write('   AT RAMDOM TO THE NEXT ROOM.'), nl,
   write('     IF THE ARROW HITS THE WUMPUS, YOU WIN.'), nl,
   write('     IF THE ARROW HITS YOU, YOU LOSE.'), nl, nl,
   write('    WARNINGS:'), nl,
   write('     WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,'), nl,
   write('    THE COMPUTER SAYS:'), nl,
   write(' WUMPUS-  ''I SMELL A WUMPUS'''), nl,
   write(' BAT   -  ''BATS NEARBY'''), nl,
write(' PIT   -  ''I FEEL A DRAFT'''), nl, nl.

```



## Python


This implementation of "Hunt the Wumpus" follows is based on the rules of the original game (see discussion page for more information). It uses python 3 syntax and the standard library random.


```python

import random

class WumpusGame(object):


	def __init__(self, edges=[]):

		# Create arbitrary caves from a list of edges (see the end of the script for example).
		if edges:
			cave = {}
			N = max([edges[i][0] for i in range(len(edges))])
			for i in range(N):
				exits = [edge[1] for edge in edges if edge[0] == i]
				cave[i] = exits

		# If no edges are specified, play in the standard cave: a dodecahedron.
		else:
			cave = {1: [2,3,4], 2: [1,5,6], 3: [1,7,8], 4: [1,9,10], 5:[2,9,11],
				6: [2,7,12], 7: [3,6,13], 8: [3,10,14], 9: [4,5,15], 10: [4,8,16],
				11: [5,12,17], 12: [6,11,18], 13: [7,14,18], 14: [8,13,19],
				15: [9,16,17], 16: [10,15,19], 17: [11,20,15], 18: [12,13,20],
				19: [14,16,20], 20: [17,18,19]}

		self.cave = cave

		self.threats = {}

		self.arrows = 5

		self.arrow_travel_distance = 5		# As in the original game. I don't like this choice:
											# a bow should not cover a whole cave.
		self.player_pos = -1


	"""
	HELPER: These methods wrap processes that are useful or called often.
	"""


	def get_safe_rooms(self):
		""" Returns a list containing all numbers of rooms that
			do not contain any threats
		"""
		return list(set(self.cave.keys()).difference(self.threats.keys()))


	def populate_cave(self):
		""" Drop player and threats into random rooms in the cave.
		"""
		for threat in ['bat', 'bat', 'pit', 'pit', 'wumpus']:
			pos = random.choice(self.get_safe_rooms())
			self.threats[pos] = threat
		self.player_pos = random.choice(self.get_safe_rooms())


	def breadth_first_search(self, source, target, max_depth=5):
		""" The game board (whether custom or standard dodecahedron) is an undirected graph.
			The rooms are the vertices and the tunnels are the edges of this graph. To find
			out whether a target room can be reached from a source room using a given amount
			of tunnels, one can do a breadth first search on the underlying undirected graph.

			BFS works like this: start with the source vertex, maybe it is already the target?
			If not, then go a level deeper and find out, if one of the children (also called
			successors) of the source vertex is the wanted target. If not, then for each child,
			go a level deeper and find out if one of the grand-children is the wanted target.
			If not, then for each grand-child go a level deeper and so on.

			The following is a recursive implementation of BFS. You will not find any loops
			(for, while). Instead you manage two lists. The first one ('stack') contains all
			the vertices of the current depth-level (e.g. all grand children). The second
			('visited') contains all vertices that you already checked. Now there are three
			possibilites: Either stack is empty, then all vertices have been checked unsuccessfully;
			or the target vertex is a member of the stack, then you are happy; or the target is
			not a member of the stack, but there are still some vertices that you did not visit,
			then you append to the stack, all successors of the members of the stack and the old
			stack now belongs to the visited vertices.
		"""
		# Set up some initial values.
		graph = self.cave
		depth = 0

		def search(stack, visited, target, depth):
			if stack == []:					# The whole graph was searched, but target was not found.
				return False, -1
			if target in stack:
				return True, depth
			visited = visited + stack
			stack = list(set([graph[v][i] for v in stack for i in range(len(graph[v]))]).difference(visited))
			depth += 1
			if depth > max_depth:			# Target is too far away from the source.
				return False, depth
			else:							# Visit all successors of vertices in the stack.
				return search(stack, visited, target, depth)

		return search([source], [], target, depth)


	"""
	INPUT / OUTPUT: The player interacts with the game.
	"""


	def print_warning(self, threat):
		""" Called when entering a new room. Shows threats in adjacent rooms.
		"""
		if threat == 'bat':
			print("You hear a rustling.")
		elif threat == 'pit':
			print("You feel a cold wind blowing from a nearby cavern.")
		elif threat == 'wumpus':
			print("You smell something terrible nearby.")


	def get_players_input(self):
		""" Queries input until valid input is given.
		"""
		while 1:								# Query the action.

			inpt = input("Shoot or move (S-M)? ")
			try:								# Ensure that the player choses a valid action (shoot or move)
				mode = str(inpt).lower()
				assert mode in ['s', 'm', 'q']
				break
			except (ValueError, AssertionError):
				print("This is not a valid action: pick 'S' to shoot and 'M' to move.")

		if mode == 'q':							# I added a 'quit-button' for convenience.
			return 'q', 0

		while 1:								# Query the target of the action.

			inpt = input("Where to? ")
			try:								# Ensure that the chosen target is convertable to an integer.
				target = int(inpt)
			except ValueError:
				print("This is not even a real number.")
				continue						# Restart the while loop, to get a valid integer as target.

			if mode == 'm':
				try:							# When walking, the target must be adjacent to the current room.
					assert target in self.cave[self.player_pos]
					break
				except AssertionError:
					print("You cannot walk that far. Please use one of the tunnels.")

			elif mode == 's':
				try:							# When shooting, the target must be reachable within 5 tunnels.
					bfs = self.breadth_first_search(self.player_pos, target)
					assert bfs[0] == True
					break
				except AssertionError:
					if bfs[1] == -1: 			# The target is outside cave.
						print("There is no room with this number in the cave. Your arrow travels randomly.")
						target = random.choice(self.cave.keys())
					if bfs[1] > self.arrow_travel_distance:				# The target is too far.
						print("Arrows aren't that croocked.")

		return mode, target


	"""
	CORE / GAME LOGIC
	"""


	def enter_room(self, room_number):
		""" Controls the process of entering a new room.
		"""
		print("Entering room {}...".format(room_number))
		# Maybe a threat waits in the new room.
		if self.threats.get(room_number) == 'bat':
			# The bat teleports the player to random empty room
			print("You encounter a bat, it transports you to a random empty room.")
			new_pos = random.choice(self.get_safe_rooms())
			return self.enter_room(new_pos)
		elif self.threats.get(room_number) == 'wumpus':
			print("Wumpus eats you.")
			return -1
		elif self.threats.get(room_number) == 'pit':
			print("You fall into a pit.")
			return -1

		# The room is safe; collect information about adjacent rooms.
		for i in self.cave[room_number]:
			self.print_warning(self.threats.get(i))

		# Only if nothing else happens, the player enters the room of his choice.
		return room_number


	def shoot_room(self, room_number):
		""" Controls the process of shooting in a room.
		"""
		print("Shooting an arrow into room {}...".format(room_number))
		# Fire an arrow and see if something is hit by it.
		self.arrows -= 1
		threat = self.threats.get(room_number)
		if threat in ['bat', 'wumpus']:
			del self.threats[room_number]
			if threat == 'wumpus':
				print("Hurra, you killed the wumpus!")
				return -1
			elif threat == 'bat':
				print("You killed a bat.")
		elif threat in ['pit', None]:
			print("This arrow is lost.")

		# If this was your last arrow and it did not hit the wumpus...
		if self.arrows < 1:		# This (or the updating of self.arrows) seems to be broken...
			print("Your quiver is empty.")
			return -1

		#  If you shoot into another room, the Wumpus has a 75% of chance of waking up and moving into an adjacent room.
		if random.random() < 0.75:
			#print("DEBUG: Wumpus moved.")
			for room_number, threat in self.threats.items():
				if threat == 'wumpus':
					wumpus_pos = room_number
			new_pos = random.choice(list(set(self.cave[wumpus_pos]).difference(self.threats.keys())))
			del self.threats[room_number]
			self.threats[new_pos] = 'wumpus'
			if new_pos == self.player_pos: # Wumpus entered players room.
				print("Wumpus enters your room and eats you!")
				return -1

		return self.player_pos


	def gameloop(self):

		print("HUNT THE WUMPUS")
		print("
### =========
")
		print()
		self.populate_cave()
		self.enter_room(self.player_pos)

		while 1:

			#print("DEBUG: Your quiver holds {} arrows.".format(self.arrows))
			#print("DEBUG: Rooms with no threats are: {}.".format(self.get_safe_rooms()))
			#print("DEBUG: Threats are located in the following rooms: {}".format(self.threats))

			print("You are in room {}.".format(self.player_pos), end=" ")
			print("Tunnels lead to:  {0}  {1}  {2}".format(*self.cave[self.player_pos]))


			inpt = self.get_players_input()		# Player choses move or shoot.
			print()								# Visual separation of rounds.
			if inpt[0] == 'm':					# Move.
				target = inpt[1]
				self.player_pos = self.enter_room(target)
			elif inpt[0] == 's':				# Shoot.
				target = inpt[1]
				self.player_pos = self.shoot_room(target)
			elif inpt[0] == 'q':				# Quit.
				self.player_pos = -1

			if self.player_pos == -1:			# E.g. Deadly threat, quiver empty, etc.
				break							# If any of the game loosing conditions are True,
												# then player_pos will be -1.

		print()
		print("Game over!")


if __name__ == '__main__':
	# Only executed if you start this script as the main script,
	# i.e. you enter 'python path/to/wumpus.py' in a terminal.
	# Assuming you saved the script in the directory 'path/to'
	# and named it 'wumpus.py'.

	# TODO: In the original game you can replay a dungeon (same positions of you and the threats)

	WG = WumpusGame()
	WG.gameloop()


```


Example run (on a windows system):


```cmd

D:\Workspace\rosettacode>python wumpus.py
HUNT THE WUMPUS

### =========


Entering room 7...
You smell something terrible nearby.
You are in room 7. Tunnels lead to:  3  6  13
Shoot or move (S-M)? m
Where to? 3

Entering room 3...
Wumpus eats you.

Game over!

```


To create an example for a valid edge list, navigate to the folder where you saved wumpus.py, open up a python interactive shell and do:


```cmd

>>> import wumpus
>>> WG = wumpus.WumpusGame()
>>> edges = [[i, WG.cave[i][j]] for i in WG.cave.keys() for j in range(len(WG.cave[i]))]
>>> print edges
[[1,2], [1,3], [1,4], [2,1], [2,5], [2,6], [3,1], ...]

```




## Racket



```racket

#lang racket

; Hunt the Wumpus

(require racket/random)

(struct game-state (labyrinth
                    player-location
                    number-of-arrows
                    wumpus-location
                    bat-locations
                    pit-locations) #:mutable #:transparent)

; The labyrinth-data list contains 20 lists that hold the information for
; each rooms connections to other rooms in the labyrinth.
; e.g. (1 2 5 8) shows that room 1 has connections to rooms 2, 5 and 8.

(define labyrinth-data '(
                         (1 2 5 8)
                         (2 1 3 10)
                         (3 2 4 12)
                         (4 3 5 14)
                         (5 1 4 6)
                         (6 5 7 15)
                         (7 6 8 17)
                         (8 1 7 9)
                         (9 8 10 18)
                         (10 2 9 11)
                         (11 10 12 19)
                         (12 3 11 13)
                         (13 12 14 20)
                         (14 4 13 15)
                         (15 6 14 16)
                         (16 15 17 20)
                         (17 7 16 18)
                         (18 9 17 19)
                         (19 11 18 20)
                         (20 13 16 19)))

(define example-game-state (game-state labyrinth-data
                                       1
                                       5
                                       2
                                       '(3 4)
                                       '(5 6)))

(define (new-game-state)
  (let ([ngs (game-state labyrinth-data 1 5 1 '(1 1) '(1 1))])
    (set-game-state-wumpus-location! ngs (safe-empty-room ngs))
    (set-game-state-bat-locations! ngs (list (safe-empty-room ngs)))
    (set-game-state-bat-locations! ngs (cons (safe-empty-room ngs)
                                             (game-state-bat-locations ngs)))
    (set-game-state-pit-locations! ngs (list (safe-empty-room ngs)))
    (set-game-state-pit-locations! ngs (cons (safe-empty-room ngs)
                                             (game-state-pit-locations ngs)))
    ngs))

(define (move-player room current-game-state)
  (set-game-state-player-location! current-game-state room))

(define (disturb-wumpus current-game-state)
  (if (<= (random) 0.75)
      (set-game-state-wumpus-location! current-game-state
                                       (room-for-wumpus-move current-game-state))
      #f))

(define (room-for-wumpus-move current-game-state)
  (let ([choices (append (neighbours
                          (game-state-wumpus-location current-game-state)
                          current-game-state)
                         (list (game-state-wumpus-location current-game-state)))])
    (findf (λ (room) (and (not (pit-room? room current-game-state))
                          (not (bat-room? room current-game-state)))) choices)))

(define (lost? current-game-state)
  (or (= (game-state-player-location current-game-state) (game-state-wumpus-location current-game-state))
      (member (game-state-player-location current-game-state)
              (game-state-pit-locations current-game-state))
      (= 0 (game-state-number-of-arrows current-game-state))))

(define (won? current-game-state)
  (= 0 (game-state-wumpus-location current-game-state)))

(define (shoot-arrow room current-game-state)
  (if (= room (game-state-wumpus-location current-game-state))
      (set-game-state-wumpus-location! current-game-state 0)
      (disturb-wumpus current-game-state))
  (set-game-state-number-of-arrows! current-game-state
                                    (- (game-state-number-of-arrows current-game-state) 1)))

(define (move-player-with-bats current-game-state)
  (set-game-state-player-location! current-game-state (safe-empty-room current-game-state)))

(define (safe-empty-room current-game-state)
  (let ([room (+ 1 (random 20))])
    (if (or (= room (game-state-wumpus-location current-game-state))
            (= room (game-state-player-location current-game-state))
            (member room (game-state-bat-locations current-game-state))
            (member room (game-state-pit-locations current-game-state)))
        (safe-empty-room current-game-state)
        room)))

(define (find-room room-number current-game-state)
  (assoc room-number (game-state-labyrinth current-game-state)))

(define (neighbours room-number current-game-state)
  (rest (find-room room-number current-game-state)))

(define (pit-room? room current-game-state)
  (member room (game-state-pit-locations current-game-state)))

(define (bat-room? room current-game-state)
  (member room (game-state-bat-locations current-game-state)))

(define (nearby? room-number entity-room-number current-game-state)
  (member entity-room-number (neighbours room-number current-game-state)))

(define (any-of-in? of-lst in-lst)
  (for/or ([item of-lst])
    (member item in-lst)))

(define (pit-nearby? room current-game-state)
  (any-of-in? (neighbours room current-game-state) (game-state-pit-locations current-game-state)))

(define (bat-nearby? room current-game-state)
  (any-of-in? (neighbours room current-game-state) (game-state-bat-locations current-game-state)))

(define (wumpus-nearby? room current-game-state)
  (member (game-state-wumpus-location current-game-state) (neighbours room current-game-state)))

(define (resolve-command str-list current-game-state)
  (let ([command (string-upcase (first str-list))]
        [room (string->number (second str-list))])
    (if (nearby? (game-state-player-location current-game-state)
                 room current-game-state)
        (cond [(equal? command "W") (if (bat-room? room current-game-state)
                                        (display-bat-attack current-game-state)
                                        (move-player room current-game-state))]
              [(equal? command "S") (shoot-arrow room current-game-state)]
              [else (displayln "Unknown command")])
        (displayln "You cannot move or shoot there!"))))

(define (display-bat-attack current-game-state)
  (move-player-with-bats current-game-state)
  (display "Argh! A Giant Bat has carried you to room ")
  (displayln (game-state-player-location current-game-state)))

(define (display-hazards current-game-state)
  (when (wumpus-nearby? (game-state-player-location current-game-state) current-game-state)
    (displayln "You smell something nearby."))
  (when (bat-nearby? (game-state-player-location current-game-state) current-game-state)
    (displayln "You hear a rustling."))
  (when (pit-nearby? (game-state-player-location current-game-state) current-game-state)
    (displayln "You feel a cold wind blowing from a nearby cavern.")))

(define (display-room-numbers lst)
  (display (string-join (map number->string lst) ", " #:before-last " or " #:after-last ".")))

(define (display-lost-message current-game-state)
  (cond [(= (game-state-player-location current-game-state)
            (game-state-wumpus-location current-game-state)) (displayln "The Wumpus has eaten you!")]
        [(member (game-state-player-location current-game-state)
                 (game-state-pit-locations current-game-state)) (displayln "You have fallen down a pit!")]
        [(= 0 (game-state-number-of-arrows current-game-state)) (displayln "You have run out of arrows.")]
        [else (displayln "Unknown loss")]))

(define (display-won-message current-game-state)
  (displayln "Congratulations, you have slain the Wumpus!"))

(define (display-information current-game-state)
  (display "You can (W)alk or (S)hoot to rooms ")
  (display-room-numbers (neighbours (game-state-player-location current-game-state) current-game-state))
  (newline)
  (display "You have ")
  (display (game-state-number-of-arrows current-game-state))
  (displayln " arrows left.")
  (display-hazards current-game-state))

(define (debug-game-state current-game-state)
  (display "    Player Location : ")
  (displayln (game-state-player-location current-game-state))
  (display "    Wumpus Location : ")
  (displayln (game-state-wumpus-location current-game-state))
  (display "    Bat Locations : ")
  (displayln (game-state-bat-locations current-game-state))
  (display "    Pit Locations : ")
  (displayln (game-state-pit-locations current-game-state)))

(define (game-loop current-game-state)
  ;(debug-game-state current-game-state)
  (display-information current-game-state)
  (resolve-command (string-split (read-line)) current-game-state)
  (cond [(lost? current-game-state) (display-lost-message current-game-state)]
        [(won? current-game-state) (display-won-message current-game-state)]
        [else (game-loop current-game-state)]))

(define (start-game)
  (let ([current-game-state (new-game-state)])
    (game-loop current-game-state)))

```



```cmd

Start the game with

(start-game)

Then walk to a room with

W 2

Or shoot into a room with

S 2

```



## Ruby

See [[Hunt_The_Wumpus/Ruby]].


## Rust

See [[Hunt_The_Wumpus/Rust]].

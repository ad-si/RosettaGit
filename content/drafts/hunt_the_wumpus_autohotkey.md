+++
title = "Hunt The Wumpus/AutoHotkey"
description = ""
date = 2017-11-05T23:54:51Z
aliases = []
[extra]
id = 21656
[taxonomies]
categories = []
tags = []
+++

{{collection|Hunt_The_Wumpus}}


## AutoHotkey


```autohotkey

/*                      _     _   _          
      /\  /\_   _ _ __ | |_  | |_| |__   ___ 
     / /_/ / | | | '_ \| __| | __| '_ \ / _ \
    / __  /| |_| | | | | |_  | |_| | | |  __/
    \/ /_/  \__,_|_| |_|\__|  \__|_| |_|\___|    
                                     __    __                                 
                                    / / /\ \ \_   _ _ __ ___  _ __  _   _ ___ 
  Coded by errorseven               \ \/  \/ / | | | '_ ` _ \| '_ \| | | / __|
    11/5/2017                        \  /\  /| |_| | | | | | | |_) | |_| \__ \
                                      \/  \/  \__,_|_| |_| |_| .__/ \__,_|___/
                                                             |_|              
*/

#include <ExTObj> ; https://goo.gl/2CRJo3

DllCall("AllocConsole")

print("You awaken in darkness. As your eyes adjust, you find yourself in a damp`n" 
        . "cavern with three corridors. Lying next to you is a bow and five arrows`n"
        . "A voice calls out 'Kilth the Wumpus an ye shalt be freed!'`n`n"
        . "To move, type the Corridor Number you wish to travel down ie: 19`n"
        . "To fire an arrow. Type Shoot followed by the Room Number ie: shoot 12`n`n"
        . "Press Enter to start the game...", 1)

arrows := 5

Sense := {bat: "You hear a rustling.`n"
        , pit: "You feel a cold wind blowing from a nearby cavern.`n"
        , wumpus: "You smell something terrible nearby.`n"}

Loc := {player: 0, wumpus: 0, pit1: 0, pit2: 0, bat1: 0, bat2: 0}

Rooms := {0: [4, 7, 1], 1: [0, 9, 2], 2: [1, 11, 3], 3:[4, 13, 2], 4: [0, 5, 3]
       , 5: [4, 6, 14], 6: [7, 16, 5], 7: [6, 0, 8], 8:[7, 17, 9], 9: [8, 1, 10]
       , 10: [9, 18, 11], 11:[10, 2, 12], 12: [13, 19, 11], 13: [14, 3, 12]
       , 14: [5, 15, 13], 15: [14, 16, 19], 16: [6, 17, 15], 17: [16, 8, 18]
       , 18: [19, 10, 17], 19: [15, 12, 18]}

x := range(0, 19) ; Set locations for Player, Bats, Pits, and the Wumpus
for e, v in Loc {
    y:=random(0, 19)
    While(!x[y])
        y:=random(0, 19)
    x.Delete(y)    
    Loc[(e)] := y
}
       
Loop { ; Game Loop
    RunWait %comspec% /c "cls"
    text := Format("Room: {:-50} Corridors: {}`r`n" 
                    . "Sense: {:-50}`r`n"
                    , loc.player
                    , Rooms[loc.player].print
                    , senseDanger(Rooms, Loc, Sense))
       
    query := print(text, 1)   
    
    If (Rooms[loc.player].contains(query)) { ; Move to room\
        x := ""
        for e, v in loc 
            if (e != "player" && query == v) {
                if (e ~= "i)bat") {
                    x := randomPlayer(Loc, query) 
                    break
                }
                else if (e ~= "i)pit")
                    gameOver("pit") 
                else
                    gameOver("wumpus")
        }    
        loc.player := (x != "" ? x : query)
    }
    else if (query ~= "i)shoot") { ; --Arrows, Check Wumpus hit, Move Wumpus if Missed
        --arrows
        shot := StrSplit(query, " ").2
        if (Rooms[loc.player].contains(shot) && loc.wumpus == shot)
            gameOver("kill")
        else if (arrows == 0)
            gameOver("arrows")
        else {
            x := random(0, 3)
            if (x != 0) {
                loc.wumpus := Rooms[loc.wumpus][x]
                if (loc.wumpus == loc.player)
                    gameOver("Found")
            }
        }
    }
}

gameOver(x) {
    RunWait %comspec% /c "cls"
    
    if (x == "pit")
        print("You enter the room and fall to your death!`n`n"
        . "Press Enter to reload the game...", 1)
    else if (x == "wumpus")
        print("You enter the room with the Wumpus and at once you are devoured!`n`n"
        . "Press Enter to reload the game...", 1)
    else if (x == "kill") 
        print("Your arrow strikes true and you listen to the death throes of the Wumpus!`n"
               . "You have prevailed against all the trials of the maze, but it is for naught...`n"
               . "as you discover there is no escape from the Maze.`nYou wander the rest of your days in darkness "
               . "and madness. Starvation drives you throw yourself into a pit...`n`n"
               . "Press Enter to reload the game...", 1)
     else if (x == "arrows") 
        print("You missed again and have no arrows remaining. You aimlessly wander the maze, `n" 
        . "without protection you inevitably become a meal for the dreadful Wumpus...`n`n"
        . "Press Enter to reload the game...", 1)
     else if (x == "found")
        print("You missed, you hear the Wumpus coming, before you can nock another arrow`n"
             . "the wumpus is upon you! You start to run, but it follows close on your heels."
             . "Exhaustion saps you of your strength, your breath is gone, you tremble as your"
             . "body can no longer go on and you feel your legs giving way....`n`n"
             . "Press Enter to reload the game...", 1)
        
               
    reload
}

randomPlayer(Loc, query) {
    x := random(0, 19)
    while(Loc.contains(x) || x == query)
        x := random(0, 19)
    
    RunWait %comspec% /c "cls"
    print("You enter the room with a Bat and at once you carried to another room!`n"
        . "Press Enter to continue...", 1)
    return x
}

senseDanger(Rooms, Loc, Sense) {
    for e, v in Loc
        if (e != "player")
            if (Rooms[loc.player].contains(loc[e]))
                text .= (e ~= "i)bat" ? sense.bat 
                      : e ~= "i)pit" ? sense.pit : sense[(e)]) " " 
    return text == "" ? "You feel safe." : text
}     
        
print(text,q:=0) {
    stdout := FileOpen("*", "w `n")
    stdin  := FileOpen("*", "r `n")
    
    If (Text) {
        stdout.Write(text " ")
        stdout.Read(0)
    }
    
    if (q) {
        query := RTrim(stdin.ReadLine(), "`n")
        return query
    }
    
}

```


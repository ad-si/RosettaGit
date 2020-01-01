+++
title = "Tic-tac-toe"
description = ""
date = 2019-09-09T22:02:30Z
aliases = []
[extra]
id = 9207
[taxonomies]
categories = []
tags = []
+++

{{task|Games}}
[[Category:Games]]
[[File:Tic_tac_toe.jpg|500px||right]]

;Task:
Play a game of [[wp:Tic-tac-toe|tic-tac-toe]].

Ensure that legal moves are played and that a winning position is notified.


Tic-tac-toe is also known as   ''naughts and crosses''.


;See also
*   [http://mathworld.wolfram.com/Tic-Tac-Toe.html MathWorld&trade;, Tic-Tac-Toe game].
*   [https://en.wikipedia.org/wiki/Tic-tac-toe Wikipedia tic-tac-toe].





## Ada


```Ada
with Ada.Text_IO, Ada.Numerics.Discrete_Random;
  -- can play human-human, human-computer, computer-human or computer-computer
  -- the computer isn't very clever: it just chooses a legal random move

procedure Tic_Tac_Toe is

   type The_Range is range 1 .. 3;
   type Board_Type is array (The_Range, The_Range) of Character;

   package Rand is new Ada.Numerics.Discrete_Random(The_Range);
   Gen: Rand.Generator; -- required for the random moves

   procedure Show_Board(Board: Board_Type) is
      use Ada.Text_IO;
   begin
      for Row in The_Range loop
         for Column in The_Range loop
            Put(Board(Row, Column));
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
   end Show_Board;

   function Find_Winner(Board: Board_Type) return Character is
      -- if 'x' or 'o' wins, it returns that, else it returns ' '

      function Three_Equal(A,B,C: Character) return Boolean is
      begin
         return (A=B) and (A=C);
      end Three_Equal;

   begin -- Find_Winner
      for I in The_Range loop
         if    Three_Equal(Board(I,1), Board(I,2), Board(I,3)) then
            return Board(I,1);
         elsif  Three_Equal(Board(1,I), Board(2,I), Board(3,I)) then
            return Board(1,I);
         end if;
      end loop;
      if Three_Equal(Board(1,1), Board(2,2), Board (3,3)) or
         Three_Equal(Board(3,1), Board(2,2), Board (1,3)) then
         return Board(2,2);
      end if;
      return ' ';
   end Find_Winner;

   procedure Do_Move(Board: in out Board_Type;
                     New_Char: Character; Computer_Move: Boolean) is
      Done: Boolean := False;
      C: Character;
      use Ada.Text_IO;

      procedure Do_C_Move(Board: in out Board_Type; New_Char: Character) is
         Found: Boolean := False;
         X,Y: The_Range;
      begin
         while not Found loop
            X := Rand.Random(Gen);
            Y := Rand.Random(Gen);
            if (Board(X,Y) /= 'x') and  (Board(X,Y) /= 'o') then
               Found := True;
               Board(X,Y) := New_Char;
            end if;
         end loop;
      end Do_C_Move;

   begin
      if Computer_Move then
         Do_C_Move(Board, New_Char);
      else -- read move;
         Put_Line("Choose your move, " & New_Char);
         while not Done loop
            Get(C);
            for Row in The_Range loop
               for Col in The_Range loop
                  if Board(Row, Col) = C then
                     Board(Row, Col) := New_Char;
                     Done := True;
                  end if;
               end loop;
            end loop;
         end loop;
      end if;
   end Do_Move;

   The_Board : Board_Type := (('1','2','3'), ('4','5','6'), ('7','8','9'));
   Cnt_Moves: Natural := 0;
   Players: array(0 .. 1) of Character := ('x', 'o'); -- 'x' begins
   C_Player: array(0 .. 1) of Boolean := (False, False);
   Reply: Character;

begin -- Tic_Tac_Toe

   -- firstly, ask whether the computer shall take over either player
   for I in Players'Range loop
      Ada.Text_IO.Put_Line("Shall " & Players(I) &
                             " be run by the computer? (y=yes)");
      Ada.Text_IO.Get(Reply);
      if Reply='y' or Reply='Y' then
         C_Player(I) := True;
         Ada.Text_IO.Put_Line("Yes!");
      else
         Ada.Text_IO.Put_Line("No!");
      end if;
   end loop;
   Rand.Reset(Gen); -- to initalize the random generator

   -- now run the game
   while (Find_Winner(The_Board) = ' ') and (Cnt_Moves < 9) loop
      Show_Board(The_Board);
      Do_Move(The_Board, Players(Cnt_Moves mod 2), C_Player(Cnt_Moves mod 2));
      Cnt_Moves := Cnt_Moves + 1;
   end loop;
   Ada.Text_IO.Put_Line("This is the end!");

   -- finally, output the outcome
   Show_Board (The_Board);
   if Find_Winner(The_Board) = ' ' then
      Ada.Text_IO.Put_Line("Draw");
   else
      Ada.Text_IO.Put_Line("The winner is: " & Find_Winner(The_Board));
   end if;
end Tic_Tac_Toe;
```


{{out}}

```txt

> ./tic_tac_toe
Shall x be run by the computer? (y=yes)
y
Yes!
Shall o be run by the computer? (y=yes)
n
No!
123
456
789

1x3
456
789

Choose your move, o
5
1x3
4o6
789

1x3
xo6
789

Choose your move, o
6
1x3
xoo
789

1xx
xoo
789

Choose your move, o
1
oxx
xoo
789

oxx
xoo
78x

Choose your move, o
7
oxx
xoo
o8x

This is the end!
oxx
xoo
oxx

Draw

> ./tic_tac_toe
Shall x be run by the computer? (y=yes)
n
No!
Shall o be run by the computer? (y=yes)
y
Yes!
123
456
789

Choose your move, x
6
123
45x
789

123
45x
o89

Choose your move, x
4
123
x5x
o89

123
x5x
o8o

Choose your move, x
8
123
x5x
oxo

o23
x5x
oxo

Choose your move, x
5
This is the end!
o23
xxx
oxo

The winner is: x

```



## ALGOL W

The user can play O, X, both or neither. O goes first whether user or computer controlled.

```algolw
begin

    string(10) board;

    % initialise the board                                                   %
    procedure initBoard ; board := " 123456789";

    % display the board                                                      %
    procedure showBoard ;
        begin
            s_w := 0;
            write( board(1//1), "|", board(2//1), "|", board(3//1) );
            write( "-+-+-" );
            write( board(4//1), "|", board(5//1), "|", board(6//1) );
            write( "-+-+-" );
            write( board(7//1), "|", board(8//1), "|", board(9//1) )
        end showBoard ;

    % returns true if board pos is free, false otherwise                     %
    logical procedure freeSpace( integer value pos ) ;
        ( board(pos//1) >= "1" and board(pos//1) <= "9" );

    % check for game over                                                    %
    logical procedure gameOver ;
        begin
            logical noMoves;
            noMoves := true;
            for i := 1 until 9 do if noMoves then noMoves := not freeSpace( i );
            noMoves
        end gameOver ;

    % makes the specified winning move or blocks it, if it will win          %
    logical procedure winOrBlock( integer   value pos1, pos2, pos3
                                ; string(1) value searchCharacter
                                ; string(1) value playerCharacter
                                ) ;
        if      board(pos1//1) = searchCharacter
            and board(pos2//1) = searchCharacter
            and freeSpace( pos3 )
        then begin
            board(pos3//1) := playerCharacter;
            true
            end
        else if board(pos1//1) = searchCharacter
            and freeSpace( pos2 )
            and board(pos3//1) = searchCharacter
        then begin
            board(pos2//1) := playerCharacter;
            true
            end
        else if freeSpace( pos1 )
            and board(pos2//1) = searchCharacter
            and board(pos3//1) = searchCharacter
        then begin
            board(pos1//1) := playerCharacter;
            true
            end
        else begin
            false
        end winOrBlock ;

    % makes a winning move or blocks a winning move, if there is one         %
    logical procedure makeOrBlockWinningMove( string(1) value searchCharacter
                                            ; string(1) value playerCharacter
                                            ) ;
        (  winOrBlock( 1, 2, 3, searchCharacter, playerCharacter )
        or winOrBlock( 4, 5, 6, searchCharacter, playerCharacter )
        or winOrBlock( 7, 8, 9, searchCharacter, playerCharacter )
        or winOrBlock( 1, 4, 7, searchCharacter, playerCharacter )
        or winOrBlock( 2, 5, 8, searchCharacter, playerCharacter )
        or winOrBlock( 3, 6, 9, searchCharacter, playerCharacter )
        or winOrBlock( 1, 5, 9, searchCharacter, playerCharacter )
        or winOrBlock( 3, 5, 7, searchCharacter, playerCharacter )
        ) ;

    % makes a move when there isn't an obvious winning/blocking move         %
    procedure move ( string(1) value playerCharacter ) ;
        begin
            logical moved;
            moved := false;
            % try for the centre, a corner or the midle of a line            %
            for pos := 5, 1, 3, 7, 9, 2, 4, 6, 8 do begin
                if not moved and freeSpace( pos ) then begin
                    moved := true;
                    board(pos//1) := playerCharacter
                end
            end
        end move ;

    % gets a move from the user                                              %
    procedure userMove( string(1) value playerCharacter ) ;
        begin
            integer move;
            while
                begin
                    write( "Please enter the move for ", playerCharacter, " " );
                    read( move );
                    ( move < 1 or move > 9 or not freeSpace( move ) )
                end
            do  begin
                write( "Invalid move" )
            end;
            board(move//1) := playerCharacter
        end userMove ;

    % returns true if the three board positions have the player character,   %
    %         false otherwise                                                %
    logical procedure same( integer   value pos1, pos2, pos3
                          ; string(1) value playerCharacter
                          ) ;
        (   board(pos1//1) = playerCharacter
        and board(pos2//1) = playerCharacter
        and board(pos3//1) = playerCharacter
        );

    % returns true if the player has made a winning move, false otherwise    %
    logical procedure playerHasWon( string(1) value playerCharacter ) ;
        (  same( 1, 2, 3, playerCharacter )
        or same( 4, 5, 6, playerCharacter )
        or same( 7, 8, 9, playerCharacter )
        or same( 1, 4, 7, playerCharacter )
        or same( 2, 5, 8, playerCharacter )
        or same( 3, 6, 9, playerCharacter )
        or same( 1, 5, 9, playerCharacter )
        or same( 3, 5, 7, playerCharacter )
        ) ;

    % takes a players turn - either automated or user input                  %
    procedure turn ( string(1) value playerCharacter, otherCharacter
                   ; logical   value playerIsUser
                   ) ;
        begin
            if playerIsUser then userMove( playerCharacter )
            else begin
                write( playerCharacter, " moves..." );
                if  not makeOrBlockWinningMove( playerCharacter, playerCharacter )
                and not makeOrBlockWinningMove( otherCharacter,  playerCharacter )
                then move( playerCharacter )
            end;
            showBoard
        end turn ;

    % asks a question and returns true if the user inputs y/Y,               %
    % false otherwise                                                        %
    logical procedure yes( string(32) value question ) ;
        begin
            string(1) answer;
            write( question );
            read( answer );
            answer = "y" or answer = "Y"
        end yes ;

    % play the game                                                          %
    while
        begin
            string(1)  again;
            string(32) gameResult;
            logical    oIsUser, xIsUser;

            oIsUser := yes( "Do you want to play O? " );
            xIsUser := yes( "Do you want to play X? " );

            gameResult := "it's a draw";
            initBoard;
            showBoard;
            while not gameOver and not playerHasWon( "O" ) and not playerHasWon( "X" ) do begin
                turn( "O", "X", oIsUser );
                if playerHasWon( "O" ) then gameResult := "O wins"
                else if not gameOver then begin
                    turn( "X", "O", xIsUser );
                    if playerHasWon( "X" ) then gameResult := "X wins"
                end
            end ;
            write( gameResult );

            yes( "Play again? " )
        end
    do  begin end

end.
```

{{out}}

```txt

Do you want to play O?          y
Do you want to play X?          n
1|2|3
-+-+-
4|5|6
-+-+-
7|8|9
Please enter the move for O 5
1|2|3
-+-+-
4|O|6
-+-+-
7|8|9
X moves...
X|2|3
-+-+-
4|O|6
-+-+-
7|8|9
...etc...
Please enter the move for O 8
X|2|O
-+-+-
O|O|X
-+-+-
X|O|9
X moves...
X|X|O
-+-+-
O|O|X
-+-+-
X|O|9
Please enter the move for O 9
X|X|O
-+-+-
O|O|X
-+-+-
X|O|O
it's a draw
Play again?                     n

```



## AppleScript


```AppleScript
property OMask : missing value
property XMask : missing value
property winningNumbers : {7, 56, 73, 84, 146, 273, 292, 448}
property difficulty : missing value

repeat
   set OMask to 0
   set XMask to 0

   if button returned of (display dialog "Who should start?" buttons {"I shoud", "CPU"}) = "CPU" then set OMask to npcGet()
   set difficulty to button returned of (display dialog "Please choose your difficulty" buttons {"Hard", "Normal"})

   repeat
       set XMask to XMask + 2 ^ (nGet() - 1)
       if winnerForMask(XMask) or OMask + XMask = 511 then exit repeat
       set OMask to npcGet()
       if winnerForMask(OMask) or OMask + XMask = 511 then exit repeat
   end repeat

   if winnerForMask(OMask) then
       set msg to "CPU Wins!"
   else if winnerForMask(XMask) then
       set msg to "You WON!!!"
   else
       set msg to "It's a draw"
   end if

   display dialog msg & return & return & drawGrid() & return & return & "Do you want to play again?"
end repeat

on nGet()
   set theMessage to "It's your turn Player 1, please fill in the number for X" & return & return & drawGrid()
   repeat
       set value to text returned of (display dialog theMessage default answer "")
       if (offset of value in "123456789") is not 0 then
           if not positionIsUsed(value as integer) then exit repeat
       end if
   end repeat
   return value as integer
end nGet

on npcGet()
   --first get the free positions
   set freeSpots to {}
   repeat with s from 1 to 9
       if not positionIsUsed(s) then set end of freeSpots to 2 ^ (s - 1)
   end repeat
   --second check if 1 move can make the CPU win
   repeat with spot in freeSpots
       if winnerForMask(OMask + spot) then return OMask + spot
   end repeat

   if difficulty is "Hard" and OMask is 0 then
       if XMask = 1 or XMask = 4 then return 2
       if XMask = 64 or XMask = 256 then return 128
   end if
   --third check if a user can make make it win (defensive) place it on position
   repeat with spot in freeSpots
       if winnerForMask(XMask + spot) then return OMask + spot
   end repeat

   --fourth check if CPU can win in two moves
   repeat with spot1 in freeSpots
       repeat with spot2 in freeSpots
           if winnerForMask(OMask + spot1 + spot2) then return OMask + spot2
       end repeat
   end repeat
   --fifth check if player can win in two moves
   repeat with spot1 in freeSpots
       repeat with spot2 in reverse of freeSpots
           if winnerForMask(XMask + spot1 + spot2) then return OMask + spot1
       end repeat
   end repeat
   --at last pick a random spot
   if XMask + OMask = 0 and difficulty = "Hard" then return 1

   return OMask + (some item of freeSpots)
end npcGet

on winnerForMask(mask)
   repeat with winLine in winningNumbers
       if BWAND(winLine, mask) = contents of winLine then return true
   end repeat
   return false
end winnerForMask

on drawGrid()
   set grid to ""
   repeat with o from 0 to 8
       if BWAND(OMask, 2 ^ o) = 2 ^ o then
           set grid to grid & "O"
       else if BWAND(XMask, 2 ^ o) = 2 ^ o then
           set grid to grid & "X"
       else
           set grid to grid & o + 1
       end if
       if o is in {2, 5} then set grid to grid & return
   end repeat
   return grid
end drawGrid

on positionIsUsed(pos)
   return BWAND(OMask + XMask, 2 ^ (pos - 1)) = 2 ^ (pos - 1)
end positionIsUsed

on BWAND(n1, n2)
   set theResult to 0
   repeat with o from 0 to 8
       if (n1 mod 2) = 1 and (n2 mod 2) = 1 then set theResult to theResult + 2 ^ o
       set {n1, n2} to {n1 div 2, n2 div 2}
   end repeat
   return theResult as integer
end BWAND
```



## AutoHotkey

This program uses a Gui with 9 buttons. Clicking on one will place an X there, disable the button, and cause the program to go somewhere.
It plays logically, trying to win, trying to block, or playing randomly in that order.

```AutoHotkey
Gui, Add, Button, x12 y12 w30 h30 vB1 gButtonHandler,
Gui, Add, Button, x52 y12 w30 h30 vB2 gButtonHandler,
Gui, Add, Button, x92 y12 w30 h30 vB3 gButtonHandler,
Gui, Add, Button, x12 y52 w30 h30 vB4 gButtonHandler,
Gui, Add, Button, x52 y52 w30 h30 vB5 gButtonHandler,
Gui, Add, Button, x92 y52 w30 h30 vB6 gButtonHandler,
Gui, Add, Button, x12 y92 w30 h30 vB7 gButtonHandler,
Gui, Add, Button, x52 y92 w30 h30 vB8 gButtonHandler,
Gui, Add, Button, x92 y92 w30 h30 vB9 gButtonHandler,
; Generated using SmartGUI Creator 4.0
Gui, Show, x127 y87 h150 w141, Tic-Tac-Toe
Winning_Moves := "123,456,789,147,258,369,159,357"
Return

ButtonHandler:
    ; Fired whenever the user clicks on an enabled button
    Go(A_GuiControl,"X")
    GoSub MyMove
Return

MyMove: ; Loops through winning moves. First attempts to win, then to block, then a random move
    Went=0
    Loop, parse, Winning_Moves,`,
    {
        Current_Set := A_LoopField
        X:=O:=0
        Loop, parse, Current_Set
        {
            GuiControlGet, Char,,Button%A_LoopField%
            If ( Char = "O" )
                O++
            If ( Char = "X" )
                X++
        }
        If ( O = 2 and X = 0 ) or ( X = 2 and O = 0 ){
            Finish_Line(Current_Set)
            Went = 1
            Break ; out of the Winning_Moves Loop to ensure the computer goes only once
        }
    }
    If (!Went)
        GoSub RandomMove
Return

Go(Control,chr){
    GuiControl,,%Control%, %chr%
    GuiControl,Disable,%Control%
    GoSub, CheckWin
}

CheckWin:
    Loop, parse, Winning_Moves,`,
    {
        Current_Set := A_LoopField
        X:=O:=0
        Loop, parse, Current_Set
        {
            GuiControlGet, Char,,Button%A_LoopField%
            If ( Char = "O" )
                O++
            If ( Char = "X" )
                X++
        }
        If ( O = 3 ){
            Msgbox O Wins!
            GoSub DisableAll
            Break
        }
        If ( X = 3 ){
            MsgBox X Wins!
            GoSub DisableAll
            Break
        }
    }
return

DisableAll:
    Loop, 9
        GuiControl, Disable, Button%A_Index%
return

Finish_Line(Set){ ;   Finish_Line is called when a line exists with 2 of the same character. It goes in the remaining spot, thereby blocking or winning.
    Loop, parse, set
    {
        GuiControlGet, IsEnabled, Enabled, Button%A_LoopField%
        Control=Button%A_LoopField%
        If IsEnabled
            Go(Control,"O")
    }
}

RandomMove:
    Loop{
        Random, rnd, 1, 9
        GuiControlGet, IsEnabled, Enabled, Button%rnd%
        If IsEnabled
        {
            Control=Button%rnd%
            Go(Control,"O")
            Break
        }
    }
return

GuiClose:
ExitApp

```


## AWK


```AWK

# syntax: GAWK -f TIC-TAC-TOE.AWK
BEGIN {
    move[12] = "3 7 4 6 8"; move[13] = "2 8 6 4 7"; move[14] = "7 3 2 8 6"
    move[16] = "8 2 3 7 4"; move[17] = "4 6 8 2 3"; move[18] = "6 4 7 3 2"
    move[19] = "8 2 3 7 4"; move[23] = "1 9 6 4 8"; move[24] = "1 9 3 7 8"
    move[25] = "8 3 7 4 0"; move[26] = "3 7 1 9 8"; move[27] = "6 4 1 9 8"
    move[28] = "1 9 7 3 4"; move[29] = "4 6 3 7 8"; move[35] = "7 4 6 8 2"
    move[45] = "6 7 3 2 0"; move[56] = "4 7 3 2 8"; move[57] = "3 2 8 4 6"
    move[58] = "2 3 7 4 6"; move[59] = "3 2 8 4 6"
    split("7 4 1 8 5 2 9 6 3",rotate)
    n = split("253 280 457 254 257 350 452 453 570 590",special)
    i = 0
    while (i < 9) { s[++i] = " " }
    print("")
    print("You move first, use the keypad:")
    board = "\n7 * 8 * 9\n*********\n4 * 5 * 6\n*********\n1 * 2 * 3\n\n? "
    printf(board)
}
state < 7 {
    x = $0
    if (s[x] != " ") {
      printf("? ")
      next
    }
    s[x] = "X"
    ++state
    print("")
    if (state > 1) {
      for (i=0; i<r; ++i) { x = rotate[x] }
    }
}
state == 1 {
    for (r=0; x>2 && x!=5; ++r) { x = rotate[x] }
    k = x
    if (x == 5) { d = 1 } else { d = 5 }
}
state == 2 {
    c = 5.5 * (k + x) - 4.5 * abs(k - x)
    split(move[c],t)
    d = t[1]
    e = t[2]
    f = t[3]
    g = t[4]
    h = t[5]
}
state == 3 {
    k = x / 2.
    c = c * 10
    d = f
    if (abs(c-350) == 100) {
      if (x != 9) { d = 10 - x }
      if (int(k) == k) { g = f }
      h = 10 - g
      if (x+0 == e+0) {
        h = g
        g = 9
      }
    }
    else if (x+0 != e+0) {
      d = e
      state = 6
    }
}
state == 4 {
    if (x+0 == g+0) {
      d = h
    }
    else {
      d = g
      state = 6
    }
    x = 6
    for (i=1; i<=n; ++i) {
      b = special[i]
      if (b == 254) { x = 4 }
      if (k+0 == abs(b-c-k)) { state = x }
    }
}
state < 7 {
    if (state != 5) {
      for (i=0; i<4-r; ++i) { d = rotate[d] }
      s[d] = "O"
    }
    for (b=7; b>0; b-=5) {
      printf("%s * %s * %s\n",s[b++],s[b++],s[b])
      if (b > 3) { print("*********") }
    }
    print("")
}
state < 5 {
    printf("? ")
}
state == 5 {
    printf("tie game")
    state = 7
}
state == 6 {
    printf("you lost")
    state = 7
}
state == 7 {
    printf(", play again? ")
    ++state
    next
}
state == 8 {
    if ($1 !~ /^[yY]$/) { exit(0) }
    i = 0
    while (i < 9) { s[++i] = " " }
    printf(board)
    state = 0
}
function abs(x) { if (x >= 0) { return x } else { return -x } }

```



## Bash

Computer is X. Computer randomly goes first. Computer plays a good game, but not a perfect game. It will win when it can and draw when it can not.

It performs a depth-first scan of all following moves. It ignores dumb actions like not winning when either player can.

For each possible move it records if it will win, lose, or something else (like win and draw depending on opponent's move).

If there is a choice of best moves, it picks one at random.

I have not used simple bash code to:
#keep it under 100 lines;
#to demonstrate usefulness of bash integers;
#show-off ANSI ESC sequences;
#implement recursion in bash;
#demonstrate conditional and alternate execution using && and || with { ...; };
#show that you don't always need to use $ to refer to integer variables;
#encourage use of [[ ]] instead of [ ] for boolean expressions;
#provide examples of pattern matching; and
#encourage use of bash for more interesting tasks.


```bash

#!/bin/bash
declare -a B=( e e e  e e e  e e e )  # Board

function show(){  # show B - underline first 2 rows; highlight position; number empty positoins
  local -i p POS=${1:-9}; local UL BOLD="\e[1m" GREEN="\e[32m" DIM="\e[2m" OFF="\e[m" ULC="\e[4m"
  for p in 0 1 2 3 4 5 6 7 8; do
    [[ p%3 -eq 0 ]] && printf "  "                             # indent boards
    UL=""; [[ p/3 -lt 2 ]] && UL=$ULC                          # underline first 2 rows
    [[ p -eq POS ]]   && printf "$BOLD$GREEN"                  # bold and colour for this position
    [[ ${B[p]} = e ]] && printf "$UL$DIM%d$OFF" $p || printf "$UL%s$OFF" ${B[p]}  # num or UL
    { [[ p%3 -lt 2 ]] && printf "$UL | $OFF"; } || printf "\n" # underline vertical bars or NL
  done
};

function win(){  # win 'X' 3 return true if X wins after move in position 3
  local ME=$1; local -i p=$2
  [[ ${B[p/3*3]} = $ME && ${B[p/3*3+1]} = $ME && ${B[p/3*3+2]} = $ME ]] && return 0  # row
  [[ ${B[p]}     = $ME && ${B[(p+3)%9]} = $ME && ${B[(p+6)%9]} = $ME ]] && return 0  # col
  [[ ${B[4]} != $ME ]] && return 1                                                   # don't test diags
  [[ p%4 -eq 0 && ${B[0]} = $ME && ${B[8]} = $ME ]] && return 0                      # TL - BR diag
  [[ p%4 -eq 2 || p -eq 4 ]] && [[ ${B[2]} = $ME && ${B[6]} = $ME ]]                 # TR - BL diag
};

function bestMove(){  # return best move or 9 if none possible
  local ME=$1 OP=$2; local -i o s p
  local -ia S=( -9 -9 -9  -9 -9 -9  -9 -9 -9 )  # score board
  local -a SB                                   # save board
  [[ ${B[*]//[!e]} = "" ]] && return 9          # game over
  SB=( ${B[*]} )                                # save Board
  for p in 0 1 2 3 4 5 6 7 8; do                          # for each board position
    [[ ${B[p]} != e ]] && continue                        # skip occupied positions
    B[p]=$ME                                              # occupy position
    win $ME $p && { S[p]=2; B=( ${SB[*]} ); return $p; }  # ME wins so this is best move
    bestMove $OP $ME; o=$?                                # what will opponent do
    [[ o -le 8 ]] && { B[o]=$OP; win $OP $o; s=$?; }      # opponent can make a legal move
    S[p]=${s:-1}                                          # save result of opponent move
    B=( ${SB[*]} )                                        # restore board after each trial run
  done
  local -i best=-1; local -ia MOV=()
  for p in 0 1 2 3 4 5 6 7 8; do                     # find all best moves
    [[ S[p] -lt 0 ]] && continue                     # dont bother with occupied positions
    [[ S[p] -eq S[best] ]] && { MOV+=(p); best=p; }  # add this move to current list
    [[ S[p] -gt S[best] ]] && { MOV=(p); best=p; }   # a better move so scrap list and start again
  done
  return ${MOV[ RANDOM%${#MOV[*]} ]}  # pick one at random
};

function getMove(){  # getMove from opponent
  [[ $ME = X ]] && { bestMove $ME $OP; return $?; }     # pick X move automatically
  read -p "O move: " -n 1; printf "\n"; return $REPLY   # get opponents move
};

function turn(){  # turn starts or continues a game. It is ME's turn
  local -i p; local ME=$1 OP=$2
  getMove; p=$?; [[ p -gt 8 ]] && { printf "Draw!\n"; show; return 1; }  # no move so a draw
  B[p]=$ME; printf "%s moves %d\n" $ME $p                                # mark board
  win $ME $p && { printf "%s wins!\n" $ME; show $p; [[ $ME = X ]] && return 2; return 0; }
  [[ ${B[*]//[!e]} = "" ]] && { printf "Draw!\n"; show; return 1; }      # no move so a draw
  show $p; turn $OP $ME                                                  # opponent moves
};

printf "Bic Bash Bow\n"
show; [[ RANDOM%2 -eq 0 ]] && { turn O X; exit $?; } || turn X O


```

{{out}} (nice ANSI formatting is not shown)

```txt

Bic Bash Bow
  0 | 1 | 2
  3 | 4 | 5
  6 | 7 | 8
X moves 1
  0 | X | 2
  3 | 4 | 5
  6 | 7 | 8
O move: 5
O moves 5
  0 | X | 2
  3 | 4 | O
  6 | 7 | 8
X moves 2
  0 | X | X
  3 | 4 | O
  6 | 7 | 8
O move: 0
O moves 0
  O | X | X
  3 | 4 | O
  6 | 7 | 8
X moves 4
  O | X | X
  3 | X | O
  6 | 7 | 8
O move: 6
O moves 6
  O | X | X
  3 | X | O
  O | 7 | 8
X moves 7
X wins!
  O | X | X
  3 | X | O
  O | X | 8

```



## BASIC


### Microsoft Small Basic

This game has a simple AI.

```smallbasic
place1 = 1
place2 = 2
place3 = 3
place4 = 4
place5 = 5
place6 = 6
place7 = 7
place8 = 8
place9 = 9
symbol1 = "X"
symbol2 = "O"
reset:
TextWindow.Clear()
TextWindow.Write(place1 + " ")
TextWindow.Write(place2 + " ")
TextWindow.WriteLine(place3 + " ")
TextWindow.Write(place4 + " ")
TextWindow.Write(place5 + " ")
TextWindow.WriteLine(place6 + " ")
TextWindow.Write(place7 + " ")
TextWindow.Write(place8 + " ")
TextWindow.WriteLine(place9 + " ")
TextWindow.WriteLine("Where would you like to go to (choose a number from 1 to 9 and press enter)?")
n = TextWindow.Read()
If n = 1 then
  If place1 = symbol1 or place1 = symbol2 then
    Goto ai
  Else
    place1 = symbol1
  EndIf
ElseIf n = 2 then
  If place2 = symbol1 or place2 = symbol2 then
    Goto ai
  Else
    place2 = symbol1
  EndIf
ElseIf n = 3 then
  If place3 = symbol1 or place3 = symbol2 then
    Goto ai
  Else
    place3 = symbol1
  EndIf
ElseIf n = 4 then
  If place4 = symbol1 or place4 = symbol2 then
    Goto ai
  Else
    place4 = symbol1
  EndIf
ElseIf n = 5 then
  If place5 = symbol1 or place5 = symbol2 then
    Goto ai
  Else
    place5 = symbol1
  EndIf
ElseIf n = 6 then
  If place6 = symbol1 or place6 = symbol2 then
    Goto ai
  Else
    place6 = symbol1
  EndIf
ElseIf n = 7 then
  If place8 = symbol1 or place7 = symbol2 then
    Goto ai
  Else
    place7 = symbol1
  EndIf
ElseIf n = 8 then
  If place8 = symbol1 or place8 = symbol2 then
    Goto ai
  Else
    place8 = symbol1
  EndIf
ElseIf n = 9 then
  If place9 = symbol1 or place9 = symbol2 then
    Goto ai
  Else
    place9 = symbol1
  EndIf
EndIf
Goto ai
ai:
n = Math.GetRandomNumber(9)
If n = 1 then
  If place1 = symbol1 or place1 = symbol2 then
    Goto ai
  Else
    place1 = symbol2
  EndIf
ElseIf n = 2 then
  If place2 = symbol1 or place2 = symbol2 then
    Goto ai
  Else
    place2 = symbol2
  EndIf
ElseIf n = 3 then
  If place3 = symbol1 or place3 = symbol2 then
    Goto ai
  Else
    place3 = symbol2
  EndIf
ElseIf n = 4 then
  If place4 = symbol1 or place4 = symbol2 then
    Goto ai
  Else
    place4 = symbol2
  EndIf
ElseIf n = 5 then
  If place5 = symbol1 or place5 = symbol2 then
    Goto ai
  Else
    place5 = symbol2
  EndIf
ElseIf n = 6 then
  If place6 = symbol1 or place6 = symbol2 then
    Goto ai
  Else
    place6 = symbol2
  EndIf
ElseIf n = 7 then
  If place7 = symbol1 or place7 = symbol2 then
    Goto ai
  Else
    place7 = symbol2
  EndIf
ElseIf n = 8 then
  If place8 = symbol1 or place8 = symbol2 then
    Goto ai
  Else
    place8 = symbol2
  EndIf
ElseIf n = 9 then
  If place9 = symbol1 or place9 = symbol2 then
    Goto ai
  Else
    place9 = symbol2
  EndIf
EndIf
If place1 = symbol1 and place2 = symbol1 and place3 = symbol1 or place4 = symbol1 and place5 = symbol1 and place6 = symbol1 or place7 = symbol1 and place8 = symbol1 and place9 = symbol1 or place1 = symbol1 and place4 = symbol1 and place7 = symbol1 or place2 = symbol1 and place5 = symbol1 and place8 = symbol1 or place3 = symbol1 and place6 = symbol1 and place9 = symbol1 or place1 = symbol1 and place5 = symbol1 and place9 = symbol1 or place3 = symbol1 and place5 = symbol1 and place7 = symbol1 then
  TextWindow.WriteLine("Player 1 (" + symbol1 + ") wins!")
ElseIf place1 = symbol2 and place2 = symbol2 and place3 = symbol2 or place4 = symbol2 and place5 = symbol2 and place6 = symbol2 or place7 = symbol2 and place8 = symbol2 and place9 = symbol2 or place1 = symbol2 and place4 = symbol2 and place7 = symbol2 or place2 = symbol2 and place5 = symbol2 and place8 = symbol2 or place3 = symbol2 and place6 = symbol2 and place9 = symbol2 or place1 = symbol2 and place5 = symbol2 and place8 = symbol2 or place3 = symbol2 and place5 = symbol2 and place7 = symbol2 then
  TextWindow.WriteLine("Player 2 (" + symbol2 + ") wins!")
Else
  Goto reset
EndIf
```



### ZX Spectrum Basic


```zxbasic


```



## Batch File

This is just a game between two human players.

```dos
@echo off
setlocal enabledelayedexpansion
:newgame
set a1=1
set a2=2
set a3=3
set a4=4
set a5=5
set a6=6
set a7=7
set a8=8
set a9=9
set ll=X
set /a zz=0
:display1
cls
echo Player: %ll%
echo %a7%_%a8%_%a9%
echo %a4%_%a5%_%a6%
echo %a1%_%a2%_%a3%
set /p myt=Where would you like to go (choose a number from 1-9 and press enter)?
if !a%myt%! equ %myt% (
set a%myt%=%ll%
goto check
)
goto display1
:check
set /a zz=%zz%+1
if %zz% geq 9 goto newgame
if %a7%+%a8%+%a9% equ %ll%+%ll%+%ll% goto win
if %a4%+%a5%+%a6% equ %ll%+%ll%+%ll% goto win
if %a1%+%a2%+%a3% equ %ll%+%ll%+%ll% goto win
if %a7%+%a5%+%a3% equ %ll%+%ll%+%ll% goto win
if %a1%+%a5%+%a9% equ %ll%+%ll%+%ll% goto win
if %a7%+%a4%+%a1% equ %ll%+%ll%+%ll% goto win
if %a8%+%a5%+%a2% equ %ll%+%ll%+%ll% goto win
if %a9%+%a6%+%a3% equ %ll%+%ll%+%ll% goto win
goto %ll%
:X
set ll=O
goto display1
:O
set ll=X
goto display1
:win
echo %ll% wins!
pause
goto newgame

```


### Advanced

This code makes a version of Tic Tac Toe with more features:

```dos
@ECHO OFF
:BEGIN
  REM Skill level
  set sl=
  cls
  echo                       Tic Tac Toe                                 (Q to quit)
  echo.
  echo.
  echo        Pick your skill level (press a number)
  echo.
  echo               (1) Children under 6
  echo               (2) Average Mental Case
  echo               (3) Oversized Ego
  CHOICE /c:123q /n > nul
  if errorlevel 4 goto end
  if errorlevel 3 set sl=3
  if errorlevel 3 goto layout
  if errorlevel 2 set sl=2
  if errorlevel 2 goto layout
  set sl=1

:LAYOUT
  REM Player turn ("x" or "o")
  set pt=
  REM Game winner ("x" or "o")
  set gw=
  REM No moves
  set nm=
  REM Set to one blank space after equal sign (check with cursor end)
  set t1=
  set t2=
  set t3=
  set t4=
  set t5=
  set t6=
  set t7=
  set t8=
  set t9=

:UPDATE
  cls
  echo   (S to set skill level)       Tic Tac Toe                         (Q to quit)
  echo.
  echo                               You are the X player.
  echo                    Press the number where you want to put an X.
  echo.
  echo   Skill level %sl%                    7 8 9
  echo                                       4 5 6
  echo                                       1 2 3
  echo.
  echo                                       :   :
  echo                                     %t1% : %t2% : %t3%
  echo                                   ....:...:....
  echo                                     %t4% : %t5% : %t6%
  echo                                   ....:...:....
  echo                                     %t7% : %t8% : %t9%
  echo                                       :   :
  if "%gw%"=="x" goto winx2
  if "%gw%"=="o" goto wino2
  if "%nm%"=="0" goto nomoves

:PLAYER
  set pt=x
  REM Layout is for keypad. Change CHOICE to "/c:123456789sq  /n > nul"
  REM for numbers to start at top left (also change user layout above).
  CHOICE /c:789456123sq /n > nul
  if errorlevel 11 goto end
  if errorlevel 10 goto begin
  if errorlevel 9 goto 9
  if errorlevel 8 goto 8
  if errorlevel 7 goto 7
  if errorlevel 6 goto 6
  if errorlevel 5 goto 5
  if errorlevel 4 goto 4
  if errorlevel 3 goto 3
  if errorlevel 2 goto 2
  goto 1

:1
  REM Check if "x" or "o" already in square.
  if "%t1%"=="x" goto player
  if "%t1%"=="o" goto player
  set t1=x
  goto check
:2
  if "%t2%"=="x" goto player
  if "%t2%"=="o" goto player
  set t2=x
  goto check
:3
  if "%t3%"=="x" goto player
  if "%t3%"=="o" goto player
  set t3=x
  goto check
:4
  if "%t4%"=="x" goto player
  if "%t4%"=="o" goto player
  set t4=x
  goto check
:5
  if "%t5%"=="x" goto player
  if "%t5%"=="o" goto player
  set t5=x
  goto check
:6
  if "%t6%"=="x" goto player
  if "%t6%"=="o" goto player
  set t6=x
  goto check
:7
  if "%t7%"=="x" goto player
  if "%t7%"=="o" goto player
  set t7=x
  goto check
:8
  if "%t8%"=="x" goto player
  if "%t8%"=="o" goto player
  set t8=x
  goto check
:9
  if "%t9%"=="x" goto player
  if "%t9%"=="o" goto player
  set t9=x
  goto check

:COMPUTER
  set pt=o
  if "%sl%"=="1" goto skill1
 REM (win corner to corner)
  if "%t1%"=="o" if "%t3%"=="o" if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if "%t1%"=="o" if "%t9%"=="o" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
  if "%t1%"=="o" if "%t7%"=="o" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t3%"=="o" if "%t7%"=="o" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
  if "%t3%"=="o" if "%t9%"=="o" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t9%"=="o" if "%t7%"=="o" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
 REM (win outside middle to outside middle)
  if "%t2%"=="o" if "%t8%"=="o" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
  if "%t4%"=="o" if "%t6%"=="o" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
 REM (win all others)
  if "%t1%"=="o" if "%t2%"=="o" if not "%t3%"=="x" if not "%t3%"=="o" goto c3
  if "%t1%"=="o" if "%t5%"=="o" if not "%t9%"=="x" if not "%t9%"=="o" goto c9
  if "%t1%"=="o" if "%t4%"=="o" if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if "%t2%"=="o" if "%t5%"=="o" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
  if "%t3%"=="o" if "%t2%"=="o" if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if "%t3%"=="o" if "%t5%"=="o" if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if "%t3%"=="o" if "%t6%"=="o" if not "%t9%"=="x" if not "%t9%"=="o" goto c9
  if "%t4%"=="o" if "%t5%"=="o" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t6%"=="o" if "%t5%"=="o" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t7%"=="o" if "%t4%"=="o" if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if "%t7%"=="o" if "%t5%"=="o" if not "%t3%"=="x" if not "%t3%"=="o" goto c3
  if "%t7%"=="o" if "%t8%"=="o" if not "%t9%"=="x" if not "%t9%"=="o" goto c9
  if "%t8%"=="o" if "%t5%"=="o" if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if "%t9%"=="o" if "%t8%"=="o" if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if "%t9%"=="o" if "%t5%"=="o" if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if "%t9%"=="o" if "%t6%"=="o" if not "%t3%"=="x" if not "%t3%"=="o" goto c3
 REM (block general attempts) -----------------------------------------------
  if "%t1%"=="x" if "%t2%"=="x" if not "%t3%"=="x" if not "%t3%"=="o" goto c3
  if "%t1%"=="x" if "%t5%"=="x" if not "%t9%"=="x" if not "%t9%"=="o" goto c9
  if "%t1%"=="x" if "%t4%"=="x" if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if "%t2%"=="x" if "%t5%"=="x" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
  if "%t3%"=="x" if "%t2%"=="x" if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if "%t3%"=="x" if "%t5%"=="x" if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if "%t3%"=="x" if "%t6%"=="x" if not "%t9%"=="x" if not "%t9%"=="o" goto c9
  if "%t4%"=="x" if "%t5%"=="x" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t6%"=="x" if "%t5%"=="x" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t7%"=="x" if "%t4%"=="x" if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if "%t7%"=="x" if "%t5%"=="x" if not "%t3%"=="x" if not "%t3%"=="o" goto c3
  if "%t7%"=="x" if "%t8%"=="x" if not "%t9%"=="x" if not "%t9%"=="o" goto c9
  if "%t8%"=="x" if "%t5%"=="x" if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if "%t9%"=="x" if "%t8%"=="x" if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if "%t9%"=="x" if "%t5%"=="x" if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if "%t9%"=="x" if "%t6%"=="x" if not "%t3%"=="x" if not "%t3%"=="o" goto c3
 REM (block obvious corner to corner)
  if "%t1%"=="x" if "%t3%"=="x" if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if "%t1%"=="x" if "%t9%"=="x" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
  if "%t1%"=="x" if "%t7%"=="x" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t3%"=="x" if "%t7%"=="x" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
  if "%t3%"=="x" if "%t9%"=="x" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t9%"=="x" if "%t7%"=="x" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
  if "%sl%"=="2" goto skill2
 REM (block sneaky corner to corner 2-4, 2-6, etc.)
  if "%t2%"=="x" if "%t4%"=="x" if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if "%t2%"=="x" if "%t6%"=="x" if not "%t3%"=="x" if not "%t3%"=="o" goto c3
  if "%t8%"=="x" if "%t4%"=="x" if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if "%t8%"=="x" if "%t6%"=="x" if not "%t9%"=="x" if not "%t9%"=="o" goto c9
 REM (block offset corner trap 1-8, 1-6, etc.)
  if "%t1%"=="x" if "%t6%"=="x" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
  if "%t1%"=="x" if "%t8%"=="x" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t3%"=="x" if "%t8%"=="x" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t3%"=="x" if "%t4%"=="x" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
  if "%t9%"=="x" if "%t4%"=="x" if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if "%t9%"=="x" if "%t2%"=="x" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t7%"=="x" if "%t2%"=="x" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t7%"=="x" if "%t6%"=="x" if not "%t2%"=="x" if not "%t2%"=="o" goto c2

:SKILL2
 REM (block outside middle to outside middle)
  if "%t2%"=="x" if "%t8%"=="x" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
  if "%t4%"=="x" if "%t6%"=="x" if not "%t5%"=="x" if not "%t5%"=="o" goto c5
 REM (block 3 corner trap)
  if "%t1%"=="x" if "%t9%"=="x" if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if "%t3%"=="x" if "%t7%"=="x" if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if "%t1%"=="x" if "%t9%"=="x" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t3%"=="x" if "%t7%"=="x" if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if "%t1%"=="x" if "%t9%"=="x" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t3%"=="x" if "%t7%"=="x" if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if "%t1%"=="x" if "%t9%"=="x" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
  if "%t3%"=="x" if "%t7%"=="x" if not "%t8%"=="x" if not "%t8%"=="o" goto c8
:SKILL1
 REM (just take a turn)
  if not "%t5%"=="x" if not "%t5%"=="o" goto c5
  if not "%t1%"=="x" if not "%t1%"=="o" goto c1
  if not "%t3%"=="x" if not "%t3%"=="o" goto c3
  if not "%t7%"=="x" if not "%t7%"=="o" goto c7
  if not "%t9%"=="x" if not "%t9%"=="o" goto c9
  if not "%t2%"=="x" if not "%t2%"=="o" goto c2
  if not "%t4%"=="x" if not "%t4%"=="o" goto c4
  if not "%t6%"=="x" if not "%t6%"=="o" goto c6
  if not "%t8%"=="x" if not "%t8%"=="o" goto c8
  set nm=0
  goto update

:C1
  set t1=o
  goto check
:C2
  set t2=o
  goto check
:C3
  set t3=o
  goto check
:C4
  set t4=o
  goto check
:C5
  set t5=o
  goto check
:C6
  set t6=o
  goto check
:C7
  set t7=o
  goto check
:C8
  set t8=o
  goto check
:C9
  set t9=o
  goto check

:CHECK
  if "%t1%"=="x" if "%t2%"=="x" if "%t3%"=="x" goto winx
  if "%t4%"=="x" if "%t5%"=="x" if "%t6%"=="x" goto winx
  if "%t7%"=="x" if "%t8%"=="x" if "%t9%"=="x" goto winx
  if "%t1%"=="x" if "%t4%"=="x" if "%t7%"=="x" goto winx
  if "%t2%"=="x" if "%t5%"=="x" if "%t8%"=="x" goto winx
  if "%t3%"=="x" if "%t6%"=="x" if "%t9%"=="x" goto winx
  if "%t1%"=="x" if "%t5%"=="x" if "%t9%"=="x" goto winx
  if "%t3%"=="x" if "%t5%"=="x" if "%t7%"=="x" goto winx
  if "%t1%"=="o" if "%t2%"=="o" if "%t3%"=="o" goto wino
  if "%t4%"=="o" if "%t5%"=="o" if "%t6%"=="o" goto wino
  if "%t7%"=="o" if "%t8%"=="o" if "%t9%"=="o" goto wino
  if "%t1%"=="o" if "%t4%"=="o" if "%t7%"=="o" goto wino
  if "%t2%"=="o" if "%t5%"=="o" if "%t8%"=="o" goto wino
  if "%t3%"=="o" if "%t6%"=="o" if "%t9%"=="o" goto wino
  if "%t1%"=="o" if "%t5%"=="o" if "%t9%"=="o" goto wino
  if "%t3%"=="o" if "%t5%"=="o" if "%t7%"=="o" goto wino
  if "%pt%"=="x" goto computer
  if "%pt%"=="o" goto update

:WINX
  set gw=x
  goto update
:WINX2
  echo   You win!
  echo   Play again (Y,N)?
  CHOICE /c:ynsq /n > nul
  if errorlevel 4 goto end
  if errorlevel 3 goto begin
  if errorlevel 2 goto end
  goto layout

:WINO
  set gw=o
  goto update
:WINO2
  echo   Sorry, You lose.
  echo   Play again (Y,N)?
  CHOICE /c:ynsq /n > nul
  if errorlevel 4 goto end
  if errorlevel 3 goto begin
  if errorlevel 2 goto end
  goto layout

:NOMOVES
  echo   There are no more moves left!
  echo   Play again (Y,N)?
  CHOICE /c:ynsq /n > nul
  if errorlevel 4 goto end
  if errorlevel 3 goto begin
  if errorlevel 2 goto end
  goto layout

:END
  cls
  echo Tic Tac Toe
  echo.
  REM Clear all variables (no spaces after equal sign).
  set gw=
  set nm=
  set sl=
  set pt=
  set t1=
  set t2=
  set t3=
  set t4=
  set t5=
  set t6=
  set t7=
  set t8=
  set t9=
```



## Befunge

Requires an intepreter with working support for numeric input, which unfortunately excludes most online implementations.

Plays reasonably well, but not perfectly, so can be beaten.


```Befunge
v123456789 --- >9 >48*,:55+\-0g,1v
>9>066+0p076+0p^  ^,," |"_v#%3:- <
:,,0537051v>:#,_$#^5#,5#+<>:#v_55+
74 1098709<^+55"---+---+---"0<v520
69 04560123 >:!#v_0\1v>$2-:6%v>803
6 +0g\66++0p^   $_>#%  v#9:-1_ 6/5
5  vv5!/*88\%*28 ::g0_^>9/#v_ "I",
,,5v>5++0p82*/3-:*+\:^v,_@ >"uoY",
0+5<v0+66_v#!%2:_55v  >:^:" win!"\
1-^ g   >$>0" :evom ruoY">:#,_$v>p
\*8+ 65_^#!/*88g0** `0\!`9:::<&<^0
v   >:!67+0g:!56+0g *+*+0" :evom "
>"yM">:#,_$ :. 1234+++, 789*+ \0^<
"a s't"98:*+>:#,_$@>365*+"ward"48*

```


{{out}}

```txt
 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

Your move: 1

 X | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9

My move: 5

 X | 2 | 3
---+---+---
 4 | O | 6
---+---+---
 7 | 8 | 9

Your move: 2

 X | X | 3
---+---+---
 4 | O | 6
---+---+---
 7 | 8 | 9

My move: 3

 X | X | O
---+---+---
 4 | O | 6
---+---+---
 7 | 8 | 9

Your move:

```



## C

Opening alternates between human and computer. Computer never loses.

```c
#include <stdio.h>
#include <stdlib.h>

int b[3][3]; /* board. 0: blank; -1: computer; 1: human */

int check_winner()
{
	int i;
	for (i = 0; i < 3; i++) {
		if (b[i][0] && b[i][1] == b[i][0] && b[i][2] == b[i][0])
			return b[i][0];
		if (b[0][i] && b[1][i] == b[0][i] && b[2][i] == b[0][i])
			return b[0][i];
	}
	if (!b[1][1]) return 0;

	if (b[1][1] == b[0][0] && b[2][2] == b[0][0]) return b[0][0];
	if (b[1][1] == b[2][0] && b[0][2] == b[1][1]) return b[1][1];

	return 0;
}

void showboard()
{
	const char *t = "X O";
	int i, j;
	for (i = 0; i < 3; i++, putchar('\n'))
		for (j = 0; j < 3; j++)
			printf("%c ", t[ b[i][j] + 1 ]);
	printf("-----\n");
}

#define for_ij for (i = 0; i < 3; i++) for (j = 0; j < 3; j++)
int best_i, best_j;
int test_move(int val, int depth)
{
	int i, j, score;
	int best = -1, changed = 0;

	if ((score = check_winner())) return (score == val) ? 1 : -1;

	for_ij {
		if (b[i][j]) continue;

		changed = b[i][j] = val;
		score = -test_move(-val, depth + 1);
		b[i][j] = 0;

		if (score <= best) continue;
		if (!depth) {
			best_i = i;
			best_j = j;
		}
		best = score;
	}

	return changed ? best : 0;
}

const char* game(int user)
{
	int i, j, k, move, win = 0;
	for_ij b[i][j] = 0;

	printf("Board postions are numbered so:\n1 2 3\n4 5 6\n7 8 9\n");
	printf("You have O, I have X.\n\n");
	for (k = 0; k < 9; k++, user = !user) {
		while(user) {
			printf("your move: ");
			if (!scanf("%d", &move)) {
				scanf("%*s");
				continue;
			}
			if (--move < 0 || move >= 9) continue;
			if (b[i = move / 3][j = move % 3]) continue;

			b[i][j] = 1;
			break;
		}
		if (!user) {
			if (!k) { /* randomize if computer opens, less boring */
				best_i = rand() % 3;
				best_j = rand() % 3;
			} else
				test_move(-1, 0);

			b[best_i][best_j] = -1;
			printf("My move: %d\n", best_i * 3 + best_j + 1);
		}

		showboard();
		if ((win = check_winner()))
			return win == 1 ? "You win.\n\n": "I win.\n\n";
	}
	return "A draw.\n\n";
}

int main()
{
	int first = 0;
	while (1) printf("%s", game(first = !first));
	return 0;
}
```



## C++


```cpp

#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
enum players { Computer, Human, Draw, None };
const int iWin[8][3] = { { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 }, { 0, 3, 6 }, { 1, 4, 7 }, { 2, 5, 8 }, { 0, 4, 8 }, { 2, 4, 6 } };

//--------------------------------------------------------------------------------------------------
class ttt
{
public:
    ttt() { _p = rand() % 2; reset(); }

    void play()
    {
	int res = Draw;
	while( true )
	{
	    drawGrid();
	    while( true )
	    {
		if( _p ) getHumanMove();
		else getComputerMove();

		drawGrid();

		res = checkVictory();
		if( res != None ) break;

		++_p %= 2;
	    }

	    if( res == Human ) cout << "CONGRATULATIONS HUMAN --- You won!";
	    else if( res == Computer ) cout << "NOT SO MUCH A SURPRISE --- I won!";
	    else cout << "It's a draw!";

	    cout << endl << endl;

	    string r;
	    cout << "Play again( Y / N )? "; cin >> r;
	    if( r != "Y" && r != "y" ) return;

	    ++_p %= 2;
	    reset();

	}
    }

private:
    void reset()
    {
	for( int x = 0; x < 9; x++ )
	    _field[x] = None;
    }

    void drawGrid()
    {
	system( "cls" );

        COORD c = { 0, 2 };
	SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );

	cout << " 1 | 2 | 3 " << endl;
	cout << "---+---+---" << endl;
	cout << " 4 | 5 | 6 " << endl;
	cout << "---+---+---" << endl;
	cout << " 7 | 8 | 9 " << endl << endl << endl;

	int f = 0;
	for( int y = 0; y < 5; y += 2 )
	    for( int x = 1; x < 11; x += 4 )
	    {
		if( _field[f] != None )
		{
		    COORD c = { x, 2 + y };
		    SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );
		    string o = _field[f] == Computer ? "X" : "O";
		    cout << o;
		}
		f++;
	    }

        c.Y = 9;
	SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );
    }

    int checkVictory()
    {
	for( int i = 0; i < 8; i++ )
	{
	    if( _field[iWin[i][0]] != None &&
		_field[iWin[i][0]] == _field[iWin[i][1]] && _field[iWin[i][1]] == _field[iWin[i][2]] )
	    {
		return _field[iWin[i][0]];
	    }
	}

	int i = 0;
	for( int f = 0; f < 9; f++ )
	{
	    if( _field[f] != None )
		i++;
	}
	if( i == 9 ) return Draw;

	return None;
    }

    void getHumanMove()
    {
	int m;
	cout << "Enter your move ( 1 - 9 ) ";
	while( true )
	{
	    m = 0;
	    do
	    { cin >> m; }
	    while( m < 1 && m > 9 );

	    if( _field[m - 1] != None )
		cout << "Invalid move. Try again!" << endl;
	    else break;
	}

	_field[m - 1] = Human;
    }

    void getComputerMove()
    {
	int move = 0;

	do{ move = rand() % 9; }
	while( _field[move] != None );

	for( int i = 0; i < 8; i++ )
	{
	    int try1 = iWin[i][0], try2 = iWin[i][1], try3 = iWin[i][2];

	    if( _field[try1] != None && _field[try1] == _field[try2] && _field[try3] == None )
	    {
		move = try3;
		if( _field[try1] == Computer ) break;
	    }

	    if( _field[try1] != None && _field[try1] == _field[try3] && _field[try2] == None )
	    {
		move = try2;
		if( _field[try1] == Computer ) break;
	    }

	    if( _field[try2] != None && _field[try2] == _field[try3] && _field[try1] == None )
	    {
		move = try1;
		if( _field[try2] == Computer ) break;
	    }
        }
	_field[move] = Computer;

    }


int _p;
int _field[9];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );

    ttt tic;
    tic.play();

    return 0;
}
//--------------------------------------------------------------------------------------------------

```

{{out}} Computer plays 'X' and human plays 'O'

```txt

 1 | 2 | X
---+---+---
 X | 5 | 6
---+---+---
 7 | O | 9

Enter your move ( 1 - 9 )

```



## C#

This implementation is purposely wordy because Tic-Tac-Toe is often a starting level program.<br/>
It tries to show a number of C# code features while still keeping each function small and understandable.


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RosettaTicTacToe
{
  class Program
  {

    /*
### ==========================================================

     *Pieces (players and board)
     *
### ==========================================================
*/
    static string[][] Players = new string[][] {
      new string[] { "COMPUTER", "X" }, // computer player
      new string[] { "HUMAN", "O" }     // human player
    };

    const int Unplayed = -1;
    const int Computer = 0;
    const int Human = 1;

    // GameBoard holds index into Players[] (0 or 1) or Unplayed (-1) if location not yet taken
    static int[] GameBoard = new int[9];

    static int[] corners = new int[] { 0, 2, 6, 8 };

    static int[][] wins = new int[][] {
      new int[] { 0, 1, 2 }, new int[] { 3, 4, 5 }, new int[] { 6, 7, 8 },
      new int[] { 0, 3, 6 }, new int[] { 1, 4, 7 }, new int[] { 2, 5, 8 },
      new int[] { 0, 4, 8 }, new int[] { 2, 4, 6 } };


    /*
### ==========================================================

     *Main Game Loop (this is what runs/controls the game)
     *
### ==========================================================
*/
    static void Main(string[] args)
    {
      while (true)
      {
        Console.Clear();
        Console.WriteLine("Welcome to Rosetta Code Tic-Tac-Toe for C#.");
        initializeGameBoard();
        displayGameBoard();
        int currentPlayer = rnd.Next(0, 2);  // current player represented by Players[] index of 0 or 1
        Console.WriteLine("The first move goes to {0} who is playing {1}s.\n", playerName(currentPlayer), playerToken(currentPlayer));
        while (true)
        {
          int thisMove = getMoveFor(currentPlayer);
          if (thisMove == Unplayed)
          {
            Console.WriteLine("{0}, you've quit the game ... am I that good?", playerName(currentPlayer));
            break;
          }
          playMove(thisMove, currentPlayer);
          displayGameBoard();
          if (isGameWon())
          {
            Console.WriteLine("{0} has won the game!", playerName(currentPlayer));
            break;
          }
          else if (isGameTied())
          {
            Console.WriteLine("Cat game ... we have a tie.");
            break;
          }
          currentPlayer = getNextPlayer(currentPlayer);
        }
        if (!playAgain())
          return;
      }
    }

    /*
### ==========================================================

     *Move Logic
     *
### ==========================================================
*/
    static int getMoveFor(int player)
    {
      if (player == Human)
        return getManualMove(player);
      else
      {
        //int selectedMove = getManualMove(player);
        //int selectedMove = getRandomMove(player);
        int selectedMove = getSemiRandomMove(player);
        //int selectedMove = getBestMove(player);
        Console.WriteLine("{0} selects position {1}.", playerName(player), selectedMove + 1);
        return selectedMove;
      }
    }

    static int getManualMove(int player)
    {
      while (true)
      {
        Console.Write("{0}, enter you move (number): ", playerName(player));
        ConsoleKeyInfo keyInfo = Console.ReadKey();
        Console.WriteLine();  // keep the display pretty
        if (keyInfo.Key == ConsoleKey.Escape)
          return Unplayed;
        if (keyInfo.Key >= ConsoleKey.D1 && keyInfo.Key <= ConsoleKey.D9)
        {
          int move = keyInfo.KeyChar - '1';  // convert to between 0..8, a GameBoard index position.
          if (GameBoard[move] == Unplayed)
            return move;
          else
            Console.WriteLine("Spot {0} is already taken, please select again.", move + 1);
        }
        else
          Console.WriteLine("Illegal move, please select again.\n");
      }
    }

    static int getRandomMove(int player)
    {
      int movesLeft = GameBoard.Count(position => position == Unplayed);
      int x = rnd.Next(0, movesLeft);
      for (int i = 0; i < GameBoard.Length; i++)  // walk board ...
      {
        if (GameBoard[i] == Unplayed && x < 0)    // until we reach the unplayed move.
          return i;
        x--;
      }
      return Unplayed;
    }

    // plays random if no winning move or needed block.
    static int getSemiRandomMove(int player)
    {
      int posToPlay;
      if (checkForWinningMove(player, out posToPlay))
        return posToPlay;
      if (checkForBlockingMove(player, out posToPlay))
        return posToPlay;
      return getRandomMove(player);
    }

    // purposely not implemented (this is the thinking part).
    static int getBestMove(int player)
    {
      return -1;
    }

    static bool checkForWinningMove(int player, out int posToPlay)
    {
      posToPlay = Unplayed;
      foreach (var line in wins)
        if (twoOfThreeMatchPlayer(player, line, out posToPlay))
          return true;
      return false;
    }

    static bool checkForBlockingMove(int player, out int posToPlay)
    {
      posToPlay = Unplayed;
      foreach (var line in wins)
        if (twoOfThreeMatchPlayer(getNextPlayer(player), line, out posToPlay))
          return true;
      return false;
    }

    static bool twoOfThreeMatchPlayer(int player, int[] line, out int posToPlay)
    {
      int cnt = 0;
      posToPlay = int.MinValue;
      foreach (int pos in line)
      {
        if (GameBoard[pos] == player)
          cnt++;
        else if (GameBoard[pos] == Unplayed)
          posToPlay = pos;
      }
      return cnt == 2 && posToPlay >= 0;
    }

    static void playMove(int boardPosition, int player)
    {
      GameBoard[boardPosition] = player;
    }

    static bool isGameWon()
    {
      return wins.Any(line => takenBySamePlayer(line[0], line[1], line[2]));
    }

    static bool takenBySamePlayer(int a, int b, int c)
    {
      return GameBoard[a] != Unplayed && GameBoard[a] == GameBoard[b] && GameBoard[a] == GameBoard[c];
    }

    static bool isGameTied()
    {
      return !GameBoard.Any(spot => spot == Unplayed);
    }

    /*
### ==========================================================

     *Misc Methods
     *
### ==========================================================
*/
    static Random rnd = new Random();

    static void initializeGameBoard()
    {
      for (int i = 0; i < GameBoard.Length; i++)
        GameBoard[i] = Unplayed;
    }

    static string playerName(int player)
    {
      return Players[player][0];
    }

    static string playerToken(int player)
    {
      return Players[player][1];
    }

    static int getNextPlayer(int player)
    {
      return (player + 1) % 2;
    }

    static void displayGameBoard()
    {
      Console.WriteLine(" {0} | {1} | {2}", pieceAt(0), pieceAt(1), pieceAt(2));
      Console.WriteLine("---|---|---");
      Console.WriteLine(" {0} | {1} | {2}", pieceAt(3), pieceAt(4), pieceAt(5));
      Console.WriteLine("---|---|---");
      Console.WriteLine(" {0} | {1} | {2}", pieceAt(6), pieceAt(7), pieceAt(8));
      Console.WriteLine();
    }

    static string pieceAt(int boardPosition)
    {
      if (GameBoard[boardPosition] == Unplayed)
        return (boardPosition + 1).ToString();  // display 1..9 on board rather than 0..8
      return playerToken(GameBoard[boardPosition]);
    }

    private static bool playAgain()
    {
      Console.WriteLine("\nDo you want to play again?");
      return Console.ReadKey(false).Key == ConsoleKey.Y;
    }
  }

}
```


{{out}}

```txt
Welcome to Rosetta Code Tic-Tac-Toe for C#.
 1 | 2 | 3
---|---|---
 4 | 5 | 6
---|---|---
 7 | 8 | 9

The first move goes to HUMAN who is playing Os.

HUMAN, enter you move (number): 5
 1 | 2 | 3
---|---|---
 4 | O | 6
---|---|---
 7 | 8 | 9

COMPUTER selects position 7.
 1 | 2 | 3
---|---|---
 4 | O | 6
---|---|---
 X | 8 | 9

HUMAN, enter you move (number): 0
Illegal move, please select again.

HUMAN, enter you move (number): 1
 O | 2 | 3
---|---|---
 4 | O | 6
---|---|---
 X | 8 | 9

COMPUTER selects position 9.
 O | 2 | 3
---|---|---
 4 | O | 6
---|---|---
 X | 8 | X

HUMAN, enter you move (number):
```



## Common Lisp


```lisp

(defun generate-board ()
  (loop repeat 9 collect nil))

(defparameter *straights* '((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) (1 5 9) (3 5 7)))
(defparameter *current-player* 'x)

(defun get-board-elt (n board)
  (nth (1- n) board))

(defun legal-p (n board)
  (null (get-board-elt n board)))

(defun set-board-elt (n board symbol)
  (if (legal-p n board)
      (setf (nth (1- n) board) symbol)
      (progn (format t "Illegal move. Try again.~&")
	     (set-board-elt (read) board symbol))))

(defun list-legal-moves (board)
  (loop for i from 1 to (length board)
     when (legal-p i board)
     collect i))

(defun get-random-element (lst)
  (nth (random (length lst)) lst))

(defun multi-non-nil-eq (lst)
  (and (notany #'null lst)
       (notany #'null (mapcar #'(lambda (x) (eq (car lst) x)) lst))
       (car lst)))

(defun elements-of-straights (board)
  (loop for i in *straights*
     collect (loop for j from 0 to 2
	   collect (get-board-elt (nth j i) board))))

(defun find-winner (board)
  (car (remove-if #'null (mapcar #'multi-non-nil-eq (elements-of-straights board)))))

(defun set-player (mark)
  (format t "Shall a computer play as ~a? (y/n)~&" mark)
  (let ((response (read)))
    (cond ((equalp response 'y) t)
	  ((equalp response 'n) nil)
	  (t (format t "Come again?~&")
	     (set-player mark)))))

(defun player-move (board symbol)
  (format t "~%Player ~a, please input your move.~&" symbol)
  (set-board-elt (read) board symbol)
  (format t "~%"))

(defun computer-move (board symbol)
  (let ((move (get-random-element (list-legal-moves board))))
    (set-board-elt move board symbol)
    (format t "~%computer selects ~a~%~%" move)))

(defun computer-move-p (current-player autoplay-x-p autoplay-o-p)
  (if (eq current-player 'x)
      autoplay-x-p
      autoplay-o-p))

(defun perform-turn (current-player board autoplay-x-p autoplay-o-p)
  (if (computer-move-p current-player autoplay-x-p autoplay-o-p)
      (computer-move board current-player)
      (player-move board current-player)))

(defun switch-player ()
  (if (eq *current-player* 'x)
      (setf *current-player* 'o)
      (setf *current-player* 'x)))

(defun display-board (board)
  (loop for i downfrom 2 to 0
     do (loop for j from 1 to 3
	   initially (format t "|")
	   do (format t "~a|" (or (get-board-elt (+ (* 3 i) j) board) (+ (* 3 i) j)))
	   finally (format t "~&"))))

(defun tic-tac-toe ()
  (setf *current-player* 'x)
  (let ((board (generate-board))
	(autoplay-x-p (set-player 'x))
	(autoplay-o-p (set-player 'o)))
    (format t "~%")
    (loop until (or (find-winner board) (null (list-legal-moves board)))
       do (display-board board)
       do (perform-turn *current-player* board autoplay-x-p autoplay-o-p)
       do (switch-player)
       finally (if (find-winner board)
		   (format t "The winner is ~a!" (find-winner board))
		   (format t "It's a tie.")))))

```


{{out}}

```txt
CL-USER> (tic-tac-toe)
Shall a computer play as X? (y/n)
n
Shall a computer play as O? (y/n)
y

|7|8|9|
|4|5|6|
|1|2|3|

Player X, please input your move.
5

|7|8|9|
|4|X|6|
|1|2|3|

computer selects 8

|7|O|9|
|4|X|6|
|1|2|3|

Player X, please input your move.
```



## D


```d
import std.stdio, std.string, std.algorithm, std.conv, std.random,
       std.ascii, std.array, std.range, std.math;

struct GameBoard {
    dchar[9] board = "123456789";
    enum : dchar { human = 'X', computer = 'O' }
    enum Game { going, humanWins, computerWins, draw }

    const pure nothrow @safe @nogc invariant() {
        int nHuman = 0, nComputer = 0;
        foreach (immutable i, immutable c; board)
            if (c.isDigit)
                assert(i == c - '1'); // In correct position?
            else {
                assert(c == human || c == computer);
                (c == human ? nHuman : nComputer)++;
            }
        assert(abs(nHuman - nComputer) <= 1);
    }

    string toString() const pure {
        return format("%(%-(%s|%)\n-+-+-\n%)", board[].chunks(3));
    }

    bool isAvailable(in int i) const pure nothrow @safe @nogc {
        return i >= 0 && i < 9 && board[i].isDigit;
    }

    auto availablePositions() const pure nothrow @safe /*@nogc*/ {
        return 9.iota.filter!(i => isAvailable(i));
    }

    Game winner() const pure nothrow @safe /*@nogc*/ {
        static immutable wins = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
                                 [0, 3, 6], [1, 4, 7], [2, 5, 8],
                                 [0, 4, 8], [2, 4, 6]];

        foreach (immutable win; wins) {
            immutable bw0 = board[win[0]];
            if (bw0.isDigit)
                continue; // Nobody wins on this one.

            if (bw0 == board[win[1]] && bw0 == board[win[2]])
                return bw0 == GameBoard.human ?
                              Game.humanWins :
                              Game.computerWins;
        }

        return availablePositions.empty ? Game.draw: Game.going;
    }

    bool isFinished() const pure nothrow @safe /*@nogc*/ {
        return winner != Game.going;
    }

    int computerMove() const // Random move.
    out(res) {
        assert(res >= 0 && res < 9 && isAvailable(res));
    } body {
        // return availablePositions.array.choice;
        return availablePositions.array[uniform(0, $)];
    }
}


GameBoard playGame() {
    GameBoard board;
    bool playsHuman = true;

    while (!board.isFinished) {
        board.writeln;

        int move;
        if (playsHuman) {
            do {
                writef("Your move (available moves: %s)? ",
                       board.availablePositions.map!q{ a + 1 });
                readf("%d\n", &move);
                move--; // Zero based indexing.
                if (move < 0)
                    return board;
            } while (!board.isAvailable(move));
        } else
            move = board.computerMove;

        assert(board.isAvailable(move));
        writefln("\n%s chose %d", playsHuman ? "You" : "I", move + 1);
        board.board[move] = playsHuman ? GameBoard.human :
                                         GameBoard.computer;
        playsHuman = !playsHuman; // Switch player.
    }

    return board;
}


void main() {
    "Tic-tac-toe game player.\n".writeln;
    immutable outcome = playGame.winner;

    final switch (outcome) {
        case GameBoard.Game.going:
            "Game stopped.".writeln;
            break;
        case GameBoard.Game.humanWins:
            "\nYou win!".writeln;
            break;
        case GameBoard.Game.computerWins:
            "\nI win.".writeln;
            break;
        case GameBoard.Game.draw:
            "\nDraw".writeln;
            break;
    }
}
```

{{out}}

```txt
Tic-tac-toe game player.

1|2|3
-+-+-
4|5|6
-+-+-
7|8|9
Your move (available moves: [1, 2, 3, 4, 5, 6, 7, 8, 9])? 1

You chose 1
X|2|3
-+-+-
4|5|6
-+-+-
7|8|9

I chose 2
X|O|3
-+-+-
4|5|6
-+-+-
7|8|9
Your move (available moves: [3, 4, 5, 6, 7, 8, 9])? 5

You chose 5
X|O|3
-+-+-
4|X|6
-+-+-
7|8|9

I chose 3
X|O|O
-+-+-
4|X|6
-+-+-
7|8|9
Your move (available moves: [4, 6, 7, 8, 9])? 9

You chose 9

You win!
```



## EasyLang


It uses minimax with alpha-beta pruning. Therefore, the computer never loses.

[https://easylang.online/apps/tictactoe.html Run it]

<lang>len f[] 9
state = 0
linewidth 2
textsize 14
#
func init . .
  color 000
  move 0 0
  rect 100 100
  color 666
  move 24 4
  line 24 68
  move 48 4
  line 48 68
  move 4 24
  line 68 24
  move 4 48
  line 68 48
  for i range 9
    f[i] = 0
  .
  if state = 1
    timer 0.2
  .
.
func draw ind . .
  c = ind mod 3
  r = ind / 3
  x = c * 24 + 12
  y = r * 24 + 12
  if f[ind] = 4
    color 900
    move x - 7 y - 7
    line x + 7 y + 7
    move x + 7 y - 7
    line x - 7 y + 7
  elif f[ind] = 1
    color 009
    move x y
    circle 9
    color 000
    circle 7
  .
.
func sum3 a d . st .
  for i range 3
    s += f[a]
    a += d
  .
  if s = 3
    st = -1
  elif s = 12
    st = 1
  .
.
func rate . res .
  res = 0
  for i range 3
    call sum3 i * 3 1 res
  .
  for i range 3
    call sum3 i 3 res
  .
  call sum3 0 4 res
  call sum3 2 2 res
  if res = 0
    for i range 9
      if f[i] = 0
        res = 9
      .
    .
  .
.
func minmax player alpha beta . rval rmov .
  call rate rval
  if rval <> 9
    if player = 1
      rval = -rval
    .
  else
    rval = alpha
    start = random 9
    mov = start
    repeat
      if f[mov] = 0
        f[mov] = player
        call minmax (5 - player) (-beta) (-rval) val h
        val = -val
        f[mov] = 0
        if val > rval
          rval = val
          rmov = mov
          if rval >= beta
            mov = start - 1
          .
        .
      .
      mov = (mov + 1) mod 9
      until mov = start
    .
  .
.
func show_result val . .
  color 555
  move 5 76
  if val = -1
    # this never happens
    text "You won"
  elif val = 1
    text "You lost"
  else
    text "Tie"
  .
  state += 2
.
func computer . .
  call minmax 4 -2 2 val mov
  f[mov] = 4
  call draw mov
  call rate val
  state = 0
  if val <> 9
    call show_result val
  .
.
func human . .
  mov = floor (mouse_x / 24) + 3 * floor (mouse_y / 24)
  if f[mov] = 0
    f[mov] = 1
    call draw mov
    call rate val
    state = 1
    if val <> 9
      call show_result val
    else
      timer 0.5
    .
  .
.
on timer
  call computer
.
on mouse_down
  if state = 0
    if mouse_x < 72 and mouse_y < 72
      call human
    .
  elif state >= 2
    state -= 2
    call init
  .
.
call init
```



## Erlang

The program will randomly chose if the computer ("X") or the user ("O") starts. The computer look ahead is only one level. Perhaps the computer might lose?

```Erlang

-module(tic_tac_toe).

-export( [task/0] ).

task() -> io:fwrite( "Result: ~p.~n", [turn(player(random:uniform()), board())] ).



board() -> [{X, erlang:integer_to_list(X)} || X <- lists:seq(1, 9)].

board_tuples( Selections, Board ) -> [lists:keyfind(X, 1, Board) || X <- Selections].

computer_move( Player, Board ) ->
	[N | _T] = lists:flatten( [X(Player, Board) || X <- [fun computer_move_win/2, fun computer_move_block/2, fun computer_move_middle/2, fun computer_move_random/2]] ),
	N.

computer_move_block( Player, Board ) ->	computer_move_two_same_player( player(false, Player), Board ).

computer_move_middle( _Player, Board ) ->
	{5, Y} = lists:keyfind( 5, 1, Board ),
	computer_move_middle( is_empty(Y) ).

computer_move_middle( true ) -> [5];
computer_move_middle( false ) -> [].

computer_move_random( _Player, Board ) ->
	Ns = [X || {X, Y} <- Board, is_empty(Y)],
	[lists:nth( random:uniform(erlang:length(Ns)), Ns )].

computer_move_two_same_player( Player, Board ) ->
        Selections = [X || X <- three_in_row_all(), is_two_same_player(Player, X, Board)],
	computer_move_two_same_player( Player, Board, Selections ).

computer_move_two_same_player( _Player, _Board, [] ) -> [];
computer_move_two_same_player( _Player, Board, [Selection | _T] ) -> [X || {X, Y} <- board_tuples(Selection, Board), is_empty(Y)].

computer_move_win( Player, Board ) -> computer_move_two_same_player( Player, Board ).

is_empty( Square ) -> Square =< "9". % Do not use < "10".

is_finished( Board ) -> is_full( Board ) orelse is_three_in_row( Board ).

is_full( Board ) -> [] =:= [X || {X, Y} <- Board, is_empty(Y)].

is_three_in_row( Board ) ->
	Fun = fun(Selections) -> is_three_in_row_same_player( board_tuples(Selections, Board) ) end,
	lists:any( Fun, three_in_row_all() ).

is_three_in_row_same_player( Selected ) -> three_in_row_player( Selected ) =/= no_player.

is_two_same_player( Player, Selections, Board ) -> is_two_same_player( Player, [{X, Y} || {X, Y} <- board_tuples(Selections, Board), not is_empty(Y)] ).

is_two_same_player( Player, [{_X, Player}, {_Y, Player}] ) -> true;
is_two_same_player( _Player, _Selected ) -> false.

player( Random ) when Random > 0.5 -> "O";
player( _Random ) -> "X".

player( true, _Player ) -> finished;
player( false, "X" ) -> "O";
player( false, "O" ) -> "X".

result( Board ) -> result( is_full(Board), Board ).

result( true, _Board ) -> draw;
result( false, Board ) ->
	[Winners] = [Selections || Selections <- three_in_row_all(), three_in_row_player(board_tuples(Selections, Board)) =/= no_player],
	"Winner is " ++ three_in_row_player( board_tuples(Winners, Board) ).

three_in_row_all() -> three_in_row_horisontal() ++ three_in_row_vertical() ++ three_in_row_diagonal().
three_in_row_diagonal() -> [[1,5,9], [3,5,7]].
three_in_row_horisontal() -> [[1,2,3], [4,5,6], [7,8,9]].
three_in_row_vertical() -> [[1,4,7], [2,5,8], [3,6,9]].

three_in_row_player( [{_X, Player}, {_Y, Player}, {_Z, Player}] ) -> three_in_row_player( not is_empty(Player), Player );
three_in_row_player( _Selected ) -> no_player.

three_in_row_player( true, Player ) -> Player;
three_in_row_player( false, _Player ) -> no_player.

turn( finished, Board ) -> result( Board );
turn( "X"=Player, Board ) ->
    N = computer_move( Player, Board ),
    io:fwrite( "Computer, ~p, selected ~p~n", [Player, N] ),
    New_board = [{N, Player} | lists:keydelete(N, 1, Board)],
    turn( player(is_finished(New_board), Player), New_board );
turn( "O"=Player, Board ) ->
    [turn_board_write_horisontal(X, Board) || X <- three_in_row_horisontal()],
    Ns = [X || {X, Y} <- Board, is_empty(Y)],
    Prompt = lists:flatten( io_lib:format("Player, ~p, select one of ~p: ", [Player, Ns]) ),
    N = turn_next_move( Prompt, Ns ),
    New_board = [{N, Player} | lists:keydelete(N, 1, Board)],
    turn( player(is_finished(New_board), Player), New_board ).

turn_board_write_horisontal( Selections, Board ) ->
	Tuples = [lists:keyfind(X, 1, Board) || X <- Selections],
	[io:fwrite( "~p ", [Y]) || {_X, Y} <- Tuples],
	io:fwrite( "~n" ).

turn_next_move( Prompt, Ns ) ->
	{ok,[N]} = io:fread( Prompt, "~d" ),
	turn_next_move_ok( lists:member(N, Ns), Prompt, Ns, N ).

turn_next_move_ok( true, _Prompt, _Ns, N ) -> N;
turn_next_move_ok( false, Prompt, Ns, _N ) -> turn_next_move( Prompt, Ns ).

```

{{out}}

```txt

96> tic_tac_toe:task().
"1" "2" "3"
"4" "5" "6"
"7" "8" "9"
Player, "O", select one of [1,2,3,4,5,6,7,8,9]: 5
Computer, "X", selected 2
"1" "X" "3"
"4" "O" "6"
"7" "8" "9"
Player, "O", select one of [1,3,4,6,7,8,9]: 1
Computer, "X", selected 9
"O" "X" "3"
"4" "O" "6"
"7" "8" "X"
Player, "O", select one of [3,4,6,7,8]: 3
Computer, "X", selected 7
"O" "X" "O"
"4" "O" "6"
"X" "8" "X"
Player, "O", select one of [4,6,8]: 8
Computer, "X", selected 6
"O" "X" "O"
"4" "O" "X"
"X" "O" "X"
Player, "O", select one of [4]: 4
Result: draw.

```



## ERRE

Taken from ERRE distribution disk: comments and messages are in Italian.

```txt
!--------------------------------------------
! TRIS.R : gioca a tris contro l'operatore
!--------------------------------------------

PROGRAM TRIS

DIM TRIS%[9],T1%[9],PIECES$[3]

!$SEGMENT=$B800

!$INCLUDE="PC.LIB"

PROCEDURE DELAY(COUNT%)
  FOR Z%=1 TO COUNT DO
  END FOR
END PROCEDURE

PROCEDURE SET_BOARD
!
! Disegna lo schema del gioco
!
  CLS
  BLOAD("TRIS.BLD",0)
!$KEY
END PROCEDURE

PROCEDURE PUT_PIECES
!
! Pone i pezzi sulla scacchiera
!
  Z%=0
  FOR ROW%=6 TO 12 STEP 3 DO     ! posizioni assolute sullo schermo
    FOR COL%=32 TO 48 STEP 8 DO
      LOCATE(ROW%+1,COL%+1)
      Z%=Z%+1
      PRINT(PIECES$[TRIS%[Z%]])
    END FOR
  END FOR
END PROCEDURE

PROCEDURE COMPUTE_MOVE(A%)
  CASE A% OF
    2-> C1%=C1%+1            END ->
    4-> C2%=C2%+1            END ->
    8-> S1%=TRUE  S2%=TRUE   END ->
    3-> N1%=N1%+1            END ->
    9-> N2%=N2%+1            END ->
   27-> S1%=FALSE S2%=FALSE  END ->
 END CASE
END PROCEDURE

PROCEDURE PREPAREMOVE(T1%[],I%->M%)
!
! Prepara la mossa del calcolatore
!
   T1%[I%]=2
   C1%=0
   C2%=0
   N1%=0
   N2%=0
   FOR K%=0 TO 2 DO
     COMPUTE_MOVE(T1%[3*K%+1]*T1%[3*K%+2]*T1%[3*K%+3])
     COMPUTE_MOVE(T1%[K%+1]*T1%[K%+4]*T1%[K%+7])
   END FOR
   COMPUTE_MOVE(T1%[1]*T1%[5]*T1%[9])
   COMPUTE_MOVE(T1%[3]*T1%[5]*T1%[7])
   M%=-63*N2%+31*C2%-15*N1%+7*C1%
END PROCEDURE

PROCEDURE COMPUTER_MOVE
!
! Coordina le mosse del calcolatore
!
  MAXSCORE%=-1000
  FOR I%=1 TO 9 DO
    IF TRIS%[I%]=1
      THEN
       PREPAREMOVE(TRIS%[],I%->MV%)
       EXIT IF S2% AND NOT S1%
       IF S1% AND S2%
         THEN
          TRIS%[I%]=2
          DIARY$=DIARY$+"c"+MID$(STR$(I%),2)+"*"
          PUT_PIECES
          EXIT
       END IF
       IF MV%=0
         THEN
          MOVE%=I%
          EXIT
       END IF
       IF MV%>MAXSCORE%
         THEN
          MOVE%=I%
          MAXSCORE%=MV%
       END IF
    END IF
  END FOR
  IF NOT S2%
    THEN
     TRIS%[MOVE%]=2
     DIARY$=DIARY$+"c"+MID$(STR$(MOVE%),2)+";"
     PUT_PIECES
     NMOVE%=NMOVE%-1
     S1%=(NMOVE%=0)
  END IF
END PROCEDURE

PROCEDURE PLAYER_MOVE
!
! Gioca l'avversario umano usando i tasti cursore per lo spostamento
!
  LOCATE(19,13)
  PRINT("Tocca a te ....                  ")
  REPEAT
    ROW%=7
    COL%=32
    LOCATE(ROW%+1,COL%+1)
    PRINT("Ü")
    REPEAT
      GET(B$)
      IF LEN(B$)=2 THEN
        CASE ASC(RIGHT$(B$,1)+CHR$(0)) OF
          77-> ! codice tastiera per CRSR =>
             LOCATE(ROW%+1,COL%+1)
             PRINT(" ")
             COL%=-(COL%+8)*(COL%<=40)-32*(COL%>40)
             LOCATE(ROW%+1,COL%+1)
             PRINT("Ü")
          END ->
          75-> ! codice tastiera per CRSR <=
             LOCATE(ROW%+1,COL%+1)
             PRINT(" ")
             COL%=-(COL%-8)*(COL%>=40)-48*(COL%<40)
             LOCATE(ROW%+1,COL%+1)
             PRINT("Ü")
          END ->
          80-> ! codice tastiera per CRSR DOWN
             LOCATE(ROW%+1,COL%+1)
             PRINT(" ")
             ROW%=-(ROW%+3)*(ROW%<=10)-7*(ROW%>10)
             LOCATE(ROW%+1,COL%+1)
             PRINT("Ü")
          END ->
          72-> ! codice tastiera per CRSR UP
             LOCATE(ROW%+1,COL%+1)
             PRINT(" ")
             ROW%=-(ROW%-3)*(ROW%>=10)-13*(ROW%<10)
             LOCATE(ROW%+1,COL%+1)
             PRINT("Ü")
          END ->
       END CASE
      END IF
     UNTIL B$=CHR$(13)
     MM%=ROW%+COL%/8-10 ! da coordinate schermo a coordinate scacchiera
   UNTIL TRIS%[MM%]=1
  TRIS%[MM%]=3
  LOCATE(ROW%+1,COL%+1)
  PRINT(" ")
  DIARY$=DIARY$+"p"+MID$(STR$(MM%),2)+";"
  PUT_PIECES
  NMOVE%=NMOVE%-1
  S1%=(NMOVE%=0)
  LOCATE(19,13)
  PRINT(STRING$(45," "))
END PROCEDURE

BEGIN
  DATA(" ","+","o")
  SET_BOARD
  REPEAT
    S1%=FALSE    S2%=FALSE   ! determinano lo stato della partita
    NMOVE%=9
    FOR Z%=1 TO 3 DO
       READ(PIECES$[Z%])
    END FOR
    FOR Z%=1 TO 9 DO
        TRIS%[Z%]=1
    END FOR
    LOCATE(19,13)
    PRINT("Giochi per primo ?")
    REPEAT
      GET(A$)
    UNTIL A$="S" OR A$="s" OR A$="N" OR A$="n"
    PUT_PIECES
    FOR INDICE%=1 TO 9 DO
      IF A$="s" OR A$="S"
        THEN
          PLAYER_MOVE
          EXIT IF S1% OR S2%
          COMPUTER_MOVE
          EXIT IF S1% OR S2%
        ELSE
          COMPUTER_MOVE
          EXIT IF S1% OR S2%
          PLAYER_MOVE
          EXIT IF S1% OR S2%
      END IF
   END FOR
   LOCATE(19,13)
   CASE TRUE OF
     (S1% AND NOT S2%)->
        PRINT("E' finita pari !!!     ")
     END ->
     (S2% AND NOT S1%)->
        PRINT("HAI VINTO !!!          ")
     END ->
     (S1% AND S2%)->
        PRINT("HO VINTO IO !!!        ")
     END ->
   END CASE
   DELAY(500)
   LOCATE(19,13)
   PRINT(DIARY$)
   DELAY(500)
   LOCATE(19,13)
   PRINT("Vuoi giocare ancora ?       ")
   REPEAT
     GET(A$)
   UNTIL A$="S" OR A$="s" OR A$="N" OR A$="n"
 UNTIL A$="N" OR A$="n"
END PROGRAM

```

{{out}}
A game example

```txt

 ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
 ░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░
 ░░▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒░░
 ░░▒▒▓▓                                                                  ▓▓▒▒░░
 ░░▒▒▓▓ ┌───────────────┐   ╔═══════╤═══════╤═══════╗   ┌────────────┐   ▓▓▒▒░░
 ░░▒▒▓▓ │───── TRIS ────│   ║   +   │   o   │   +   ║   │Il calcola- │   ▓▓▒▒░░
 ░░▒▒▓▓ │               │   ║       │       │       ║   │tore non può│   ▓▓▒▒░░
 ░░▒▒▓▓ │Si gioca contro│   ╟───────┼───────┼───────╢   │perdere e   │   ▓▓▒▒░░
 ░░▒▒▓▓ │il calcolatore.│   ║   +   │   o   │   o   ║   │perciò il   │   ▓▓▒▒░░
 ░░▒▒▓▓ │Per muoversi   │   ║       │       │       ║   │giocatore   │   ▓▓▒▒░░
 ░░▒▒▓▓ │usare i tasti  │   ╟───────┼───────┼───────╢   │può al mas- │   ▓▓▒▒░░
 ░░▒▒▓▓ │cursore e ◄──┘ │   ║   o   │   +   │   o   ║   │simo pareg- │   ▓▓▒▒░░
 ░░▒▒▓▓ │per confermare.│   ║       │       │       ║   │giare.      │   ▓▓▒▒░░
 ░░▒▒▓▓ └───────────────┘   ╚═══════╧═══════╧═══════╝   └────────────┘   ▓▓▒▒░░
 ░░▒▒▓▓                                                                  ▓▓▒▒░░
 ░░▒▒▓▓                                                                  ▓▓▒▒░░
 ░░▒▒▓▓  ┌────────────────────────────────────────────────────────────┐  ▓▓▒▒░░
 ░░▒▒▓▓  │  Vuoi giocare ancora ?                                     │  ▓▓▒▒░░
 ░░▒▒▓▓  └────────────────────────────────────────────────────────────┘  ▓▓▒▒░░
 ░░▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▒░░
 ░░▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░
 ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

```



## Euphoria

{{works with|OpenEuphoria}}
No computer AI

```euphoria

include std/console.e
include std/text.e
include std/search.e
include std/sequence.e

sequence board
sequence players = {"X","O"}

function DisplayBoard()
	for i = 1 to 3 do
		for j = 1 to 3 do
			printf(1,"%s",board[i][j])
			if j < 3 then
				printf(1,"%s","|")
			end if
		end for
		if i < 3 then
			puts(1,"\n-----\n")
		else
			puts(1,"\n\n")
		end if
	end for

	return 1
end function

function CheckWinner()
sequence temp = board
	for a = 1 to 2 do
		for i = 1 to 3 do
			if equal({"X","X","X"},temp[i]) then
				puts(1,"X wins\n\n")
				return 1
			elsif equal({"O","O","O"},temp[i]) then
				puts(1,"O wins\n\n")
				return 1
			end if
		end for
		temp = columnize(board)
	end for
	if equal({"X","X","X"},{board[1][1],board[2][2],board[3][3]}) or
	   equal({"X","X","X"},{board[1][3],board[2][2],board[3][1]}) then
		puts(1,"X wins\n")
		return 1
	elsif equal({"O","O","O"},{board[1][1],board[2][2],board[3][3]}) or
	   equal({"O","O","O"},{board[1][3],board[2][2],board[3][1]}) then
		puts(1,"O wins\n")
		return 1
	end if

	if moves = 9 then
		puts(1,"Draw\n\n")
		return 1
	end if

	return 0
end function

integer turn, row, column, moves
sequence replay
while 1 do
	board = repeat(repeat(" ",3),3)
	DisplayBoard()
	turn = rand(2)
	moves = 0

	while 1 do
		while 1 do
			printf(1,"%s's turn\n",players[turn])
			row = prompt_number("Enter row: ",{})
			column = prompt_number("Enter column: ",{})
			if match(board[row][column]," ") then
				board[row][column] = players[turn]
				moves += 1
				exit
			else
				puts(1,"Space already taken - pick again\n")
			end if
		end while

		DisplayBoard()

		if CheckWinner() then
			exit
		end if

		if turn = 1 then
			turn = 2
		else
			turn = 1
		end if
	end while

	replay = lower(prompt_string("Play again (y/n)?\n\n"))

	if match(replay,"n") then
		exit
	end if

end while

```

{{out}}

```txt

 | |
-----
 | |
-----
 | |

O's turn
Enter row: 1
Enter column: 1
O| |
-----
 | |
-----
 | |

X's turn
Enter row: 3
Enter column: 3
O| |
-----
 | |
-----
 | |X

O's turn
Enter row: 3
Enter column: 1
O| |
-----
 | |
-----
O| |X

X's turn
Enter row: 2
Enter column: 1
O| |
-----
X| |
-----
O| |X

O's turn
Enter row: 2
Enter column: 2
O| |
-----
X|O|
-----
O| |X

X's turn
Enter row: 1
Enter column: 3
O| |X
-----
X|O|
-----
O| |X

O's turn
Enter row: 1
Enter column: 2
O|O|X
-----
X|O|
-----
O| |X

X's turn
Enter row: 3
Enter column: 2
O|O|X
-----
X|O|
-----
O|X|X

O's turn
Enter row: 2
Enter column: 3
O|O|X
-----
X|O|O
-----
O|X|X

Draw

Play again (y/n)?

```


=={{header|F_Sharp|F#}}==
A purely-functional solution with a naive (but perfect) computer player implementation. The first move is played randomly by the computer.


```fsharp
type Brick =
 | Empty
 | Computer
 | User

let brickToString = function
 | Empty -> ' '
 | Computer -> 'X'
 | User -> 'O'

// y -> x -> Brick
type Board = Map<int, Map<int, Brick> >

let emptyBoard =
  let emptyRow = Map.ofList [0,Empty; 1,Empty; 2,Empty]
  Map.ofList [0,emptyRow; 1,emptyRow; 2,emptyRow]

let get (b:Board) (x,y) = b.[y].[x]

let set player (b:Board) (x,y) : Board =
  let row = b.[y].Add(x, player)
  b.Add(y, row)

let winningPositions =
  [for x in [0..2] -> x,x] // first diagonal
  ::[for x in [0..2] -> 2-x,x] // second diagonal
  ::[for y in [0..2] do
     yield! [[for x in [0..2]->(y,x)]; // columns
             [for x in [0..2] -> (x,y)]]] // rows

let hasWon player board =
  List.exists
    (fun ps -> List.forall (fun pos -> get board pos = player) ps)
    winningPositions

let freeSpace board =
  [for x in 0..2 do
     for y in 0..2 do
       if get board (x,y) = Empty then yield x,y]

type Evaluation =
 | Win
 | Draw
 | Lose

let rec evaluate board move =
  let b2 = set Computer board move
  if hasWon Computer b2 then Win
  else
    match freeSpace b2 with
    | [] -> Draw
    | userChoices ->
       let b3s = List.map (set User b2) userChoices
       if List.exists (hasWon User) b3s then Lose
       elif List.exists (fun b3 -> bestOutcome b3 = Lose) b3s
       then Lose
       elif List.exists (fun b3 -> bestOutcome b3 = Draw) b3s
       then Draw
       else Win

and findBestChoice b =
  match freeSpace b with
  | [] -> ((-1,-1), Draw)
  | choices ->
    match List.tryFind (fun c -> evaluate b c = Win) choices with
    | Some c -> (c, Win)
    | None -> match List.tryFind (fun c -> evaluate b c = Draw) choices with
              | Some c -> (c, Draw)
              | None -> (List.head choices, Lose)

and bestOutcome b = snd (findBestChoice b)

let bestChoice b = fst (findBestChoice b)

let computerPlay b = set Computer b (bestChoice b)

let printBoard b =
  printfn "   | A | B | C |"
  printfn "---+---+---+---+"
  for y in 0..2 do
   printfn " %d | %c | %c | %c |"
    (3-y)
    (get b (0,y) |> brickToString)
    (get b (1,y) |> brickToString)
    (get b (2,y) |> brickToString)
   printfn "---+---+---+---+"

let rec userPlay b =
  printfn "Which field do you play? (format: a1)"
  let input = System.Console.ReadLine()
  if input.Length <> 2
     || input.[0] < 'a' || input.[0] > 'c'
     || input.[1] < '1' || input.[1] > '3' then
     printfn "illegal input"
     userPlay b
  else
     let x = int(input.[0]) - int('a')
     let y = 2 - int(input.[1]) + int('1')
     if get b (x,y) <> Empty then
       printfn "Field is not free."
       userPlay b
     else
       set User b (x,y)

let rec gameLoop b player =
  if freeSpace b = [] then
    printfn "Game over. Draw."
  elif player = Computer then
    printfn "Computer plays..."
    let b2 = computerPlay b
    printBoard b2
    if hasWon Computer b2 then
      printfn "Game over. I have won."
    else
      gameLoop b2 User
  elif player = User then
    let b2 = userPlay b
    printBoard b2
    if hasWon User b2 then
      printfn "Game over. You have won."
    else
      gameLoop b2 Computer

// randomly choose an element of a list
let choose =
  let rnd = new System.Random()
  fun (xs:_ list) -> xs.[rnd.Next(xs.Length)]

// play first brick randomly
printfn "Computer starts."
let b = set Computer emptyBoard (choose (freeSpace emptyBoard))
printBoard b
gameLoop b User
```


Example game:

```txt

Computer starts.
   | A | B | C |
---+---+---+---+
 3 |   |   | X |
---+---+---+---+
 2 |   |   |   |
---+---+---+---+
 1 |   |   |   |
---+---+---+---+
Which field do you play? (format: a1)
a1
   | A | B | C |
---+---+---+---+
 3 |   |   | X |
---+---+---+---+
 2 |   |   |   |
---+---+---+---+
 1 | O |   |   |
---+---+---+---+
Computer plays...
   | A | B | C |
---+---+---+---+
 3 | X |   | X |
---+---+---+---+
 2 |   |   |   |
---+---+---+---+
 1 | O |   |   |
---+---+---+---+
Which field do you play? (format: a1)
b3
   | A | B | C |
---+---+---+---+
 3 | X | O | X |
---+---+---+---+
 2 |   |   |   |
---+---+---+---+
 1 | O |   |   |
---+---+---+---+
Computer plays...
   | A | B | C |
---+---+---+---+
 3 | X | O | X |
---+---+---+---+
 2 |   |   |   |
---+---+---+---+
 1 | O |   | X |
---+---+---+---+
Which field do you play? (format: a1)
c2
   | A | B | C |
---+---+---+---+
 3 | X | O | X |
---+---+---+---+
 2 |   |   | O |
---+---+---+---+
 1 | O |   | X |
---+---+---+---+
Computer plays...
   | A | B | C |
---+---+---+---+
 3 | X | O | X |
---+---+---+---+
 2 |   | X | O |
---+---+---+---+
 1 | O |   | X |
---+---+---+---+
Game over. I have won.

```



## Fortran

Objective: write program in less than 100 lines, not using semicolons. Computer never loses, but plays as random as possible. Player gets first move of first game. Afterwards, first move alternates between computer and player.

```fortran

! This is a fortran95 implementation of the game of tic-tac-toe.
! - Objective was to use less than 100 lines.
! - No attention has been devoted to efficiency.
! - Indentation by findent: https://sourceforge.net/projects/findent/
! - This is free software, use as you like at own risk.
! - Compile: gfortran -o tictactoe tictactoe.f90
! - Run: ./tictactoe
! Comments to: wvermin at gmail dot com
module tic
   implicit none
   integer :: b(9)
contains
   logical function iswin(p)
      integer,intent(in) :: p
      iswin = &
         all(b([1,2,3])==p).or.all(b([4,5,6])==p).or.all(b([7,8,9])==p).or.&
         all(b([1,4,7])==p).or.all(b([2,5,8])==p).or.all(b([3,6,9])==p).or.&
         all(b([1,5,9])==p).or.all(b([3,5,7])==p)
   end function iswin
   subroutine printb(mes)
      character(len=*) :: mes
      integer          :: i,j
      character        :: s(0:2) = ['.','X','O']
      print "(3a3,'   ',3i3)",(s(b(3*i+1:3*i+3)),(j,j=3*i+1,3*i+3),i=0,2)
      if(mes /= ' ') print "(/,a)",mes
   end subroutine printb
   integer recursive function minmax(player,bestm) result(bestv)
      integer :: player,bestm,move,v,bm,win=1000,inf=100000
      real    :: x
      if (all(b .ne. 0)) then
         bestv = 0
      else
         bestv = -inf
         do move=1,9
            if (b(move) == 0) then
               b(move) = player
               if (iswin(player)) then
                  v = win
               else
                  call random_number(x)
                  v = -minmax(3-player,bm) - int(10*x)
               endif
               if (v > bestv) then
                  bestv = v
                  bestm = move
               endif
               b(move) = 0
               if (v == win) exit
            endif
         enddo
      endif
   end function minmax
end module tic
program tictactoe
   ! computer: player=1, user: player=2
   use tic
   implicit none
   integer :: move,ios,v,bestmove,ply,player=2,k,values(8)
   integer,allocatable :: seed(:)
   call date_and_time(values=values)
   call random_seed(size=k)
   allocate(seed(k))
   seed = values(8)+1000*values(7)+60*1000*values(6)+60*60*1000*values(5)
   call random_seed(put=seed)
   mainloop: do
      b = 0
      call printb('You have O, I have X. You enter 0: game ends.')
      plyloop: do ply=0,4
         if (player == 2 .or. ply >0 ) then  ! user move
            write(*,"(/,a)",advance='no'),'Your move? (0..9) '
            getloop: do
               readloop: do
                  read (*,*,iostat=ios),move
                  if (ios == 0 .and. move  >= 0 .and. move <= 9) exit readloop
                  write(*,"(a)",advance='no'),'huh? Try again (0..9): '
               enddo readloop
               if (  move  == 0) exit mainloop
               if (b(move) == 0) exit getloop
               write(*,"(a)",advance='no'),'Already occupied, again (0..9): '
            enddo getloop
            b(move) = 2
            if(iswin(2)) then   ! this should not happen
               call printb('***** You win *****')
               exit plyloop
            endif
         endif
         v = minmax(1,bestmove)   ! computer move
         b(bestmove) = 1
         if(iswin(1)) then
            call printb('***** I win *****')
            exit plyloop
         endif
         write(*,"(/,a,i3)"), 'My move: ',bestmove
         call printb(' ')
      enddo plyloop
      if(ply == 5) write(*,"('***** Draw *****',/)")
      player = 3-player
   enddo mainloop
end program

```



## Go

Intermediate, like Python's "Better skilled player."  Computer wins and blocks where it can, but otherwise plays randomly.  Plays multiple games and keeps score.  Player gets first move of first game.  Afterwards, loser gets to go first, or after cat games, first move alternates.

```go
package main

import (
    "bufio"
    "fmt"
    "math/rand"
    "os"
    "strings"
)

var b []byte

func printBoard() {
    fmt.Printf("%s\n%s\n%s\n", b[0:3], b[3:6], b[6:9])
}

var pScore, cScore int
var pMark, cMark byte = 'X', 'O'
var in = bufio.NewReader(os.Stdin)

func main() {
    b = make([]byte, 9)
    fmt.Println("Play by entering a digit.")
    for {
        // start of game
        for i := range b {
            b[i] = '1' + byte(i)
        }
        computerStart := cMark == 'X'
        if computerStart {
            fmt.Println("I go first, playing X's")
        } else {
            fmt.Println("You go first, playing X's")
        }
    TakeTurns:
        for {
            if !computerStart {
                if !playerTurn() {
                    return
                }
                if gameOver() {
                    break TakeTurns
                }

            }
            computerStart = false
            computerTurn()
            if gameOver() {
                break TakeTurns
            }
        }
        fmt.Println("Score: you", pScore, "me", cScore)
        fmt.Println("\nLet's play again.")
    }
}

func playerTurn() bool {
    var pm string
    var err error
    for i := 0; i < 3; i++ { // retry loop
        printBoard()
        fmt.Printf("%c's move? ", pMark)
        if pm, err = in.ReadString('\n'); err != nil {
            fmt.Println(err)
            return false
        }
        pm = strings.TrimSpace(pm)
        if pm >= "1" && pm <= "9" && b[pm[0]-'1'] == pm[0] {
            x := pm[0] - '1'
            b[x] = pMark
            return true
        }
    }
    fmt.Println("You're not playing right.")
    return false
}

var choices = make([]int, 9)

func computerTurn() {
    printBoard()
    var x int
    defer func() {
        fmt.Println("My move:", x+1)
        b[x] = cMark
    }()
    // look for two in a row
    block := -1
    for _, l := range lines {
        var mine, yours int
        x = -1
        for _, sq := range l {
            switch b[sq] {
            case cMark:
                mine++
            case pMark:
                yours++
            default:
                x = sq
            }
        }
        if mine == 2 && x >= 0 {
            return // strategy 1: make winning move
        }
        if yours == 2 && x >= 0 {
            block = x
        }
    }
    if block >= 0 {
        x = block // strategy 2: make blocking move
        return
    }
    // default strategy: random move
    choices = choices[:0]
    for i, sq := range b {
        if sq == '1'+byte(i) {
            choices = append(choices, i)
        }
    }
    x = choices[rand.Intn(len(choices))]
}

func gameOver() bool {
    // check for win
    for _, l := range lines {
        if b[l[0]] == b[l[1]] && b[l[1]] == b[l[2]] {
            printBoard()
            if b[l[0]] == cMark {
                fmt.Println("I win!")
                cScore++
                pMark, cMark = 'X', 'O'
            } else {
                fmt.Println("You win!")
                pScore++
                pMark, cMark = 'O', 'X'
            }
            return true
        }
    }
    // check for empty squares
    for i, sq := range b {
        if sq == '1'+byte(i) {
            return false
        }
    }
    fmt.Println("Cat game.")
    pMark, cMark = cMark, pMark
    return true
}

var lines = [][]int{
    {0, 1, 2}, // rows
    {3, 4, 5},
    {6, 7, 8},
    {0, 3, 6}, // columns
    {1, 4, 7},
    {2, 5, 8},
    {0, 4, 8}, // diagonals
    {2, 4, 6},
}
```



## Groovy

Simplified version of Tic Tac Toe for player vs. player (no AI computer-controlled option).

```Groovy
class Main {

	def input = new Scanner(System.in)

	static main(args) {
		Main main = new Main();
		main.play();
	}

	public void play() {

		TicTackToe game = new TicTackToe();
		game.init()
		def gameOver = false
		def player = game.player1

		println "Players take turns marking a square. Only squares \n"+
				"not already marked can be picked. Once a player has \n"+
    			"marked three squares in a row, column or diagonal, they win! If all squares \n"+
    			"are marked and no three squares are the same, a tied game is declared.\n"+
				"Player 1 is O and Player 2 is X \n"+
    			"Have Fun! \n\n"
		println "${game.drawBoard()}"

		while (!gameOver && game.plays < 9) {

			player = game.currentPlayer == 1 ? game.player1 :game.player2
			def validPick = false;
			while (!validPick) {

				def square
				println "Next $player, enter move by selecting square's number :"
				try {
					square = input.nextLine();
				} catch (Exception ex) { }

				if (square.length() == 1 && Character.isDigit(square.toCharArray()[0])) {	validPick = game.placeMarker(square)	}
				if (!validPick) {	println "Select another Square"	}

			}

			(game.checkWinner(player))?	gameOver = true	: game.switchPlayers()
			println(game.drawBoard());

		}
		println "Game Over, " + ((gameOver == true)? "$player Wins" : "Draw")
	}

}

public class TicTackToe {

    def board = new Object[3][3]
	def final player1 = "player 1"
	def final player2 = "player 2"
	def final marker1 = 'X'
	def final marker2 = 'O'

    int currentPlayer
	int plays

	public TicTacToe(){

	}


    def init() {
        int counter = 0;
       (0..2).each { row ->
           (0..2).each { col ->
                board[row][col] = (++counter).toString();
            }
        }
	   plays = 0
	   currentPlayer =1
    }

    def switchPlayers() {
        currentPlayer = (currentPlayer == 1) ? 2:1
        plays++
    }

    def placeMarker(play) {
		def result = false
        (0..2).each { row ->
            (0..2).each { col ->
                if (board[row][col].toString()==play.toString()){
                    board[row][col] = (currentPlayer == 1) ? marker2 : marker1;
                    result =  true;
                }
            }
        }
        return result;
    }

    def checkWinner(player) {
    	char current = (player == player1)? marker2:  marker1
        //Checking
        return checkRows(current) || checkColumns(current) ||checkDiagonals(current);
    }

    def checkRows(char current){
		(0..2).any{ line ->
			  board[line].every { it == current}
		}
    }


    def checkColumns(char current){
		(0..2).any{i ->
			(0..2).every{j ->
				 board[j][i]==current }
		}
    }

    def checkDiagonals(char current){
		def rightDiag = [board[0][0],board[1][1],board[2][2]]
		def leftDiag =  [board[0][2],board[1][1],board[2][0]]
		return rightDiag.every{it == current} || leftDiag.every{it == current}
    }


    def drawBoard() {
        StringBuilder builder = new StringBuilder("Game board: \n");
        (0..2).each { row->
            (0..2).each {col ->
                builder.append("[" + board[row][col] + "]");
            }
            builder.append("\n");
        }
        builder.append("\n");
        return builder.toString();
    }
}
```



## Haskell

Computer player has three strategies: 1. Try to block the opponent first, 2. Try to guess a good position for the next move, 3. Place a piece randomly.
There are lots of comments throughout the code.

```Haskell

module Main where

import System.Random
import Data.List (intercalate, find, minimumBy)
import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Monad (guard)
import Data.Ord (comparing)

-- check if there is a horizontal, vertical or diagonal line of
-- X or O
tictactoe :: String -> Bool
tictactoe a = tictactoeFor 'X' a /= tictactoeFor 'O' a

-- check if there is a horizontal, vertical or diagonal line
-- for the given player "n"
tictactoeFor :: Char -> String -> Bool
tictactoeFor n [a,b,c,d,e,f,g,h,i] =
    [n,n,n] `elem` [[a,b,c],[d,e,f],[g,h,i],[a,d,g],
                    [b,e,h],[c,f,i],[a,e,i],[c,e,g]]

-- empty game board
start :: String
start = "         "

-- check if there is an X or an O at the given position
isPossible :: Int -> String -> Bool
isPossible n game = (game !! n) `notElem` "XO"

-- try to place an X or an O at a given position.
-- "Right" + modified board means success, "Left" + unmodified board
-- means failure
place :: Int -> Char -> String -> Either String String
place i c game =
    if isPossible i game
    then Right $ take i game ++ [c] ++ drop (i + 1) game
    else Left game

-- COMPUTER AI
-- get the number of movements, starting from a given non-empty board
-- and a position for the next movement, until the specified player
-- wins or no movement is possible
-- the positions are chosen sequentially, so there's not much
-- intelligence here anyway
developGame :: Bool -> Int -> Int -> Char -> String -> (Int, Char, String)
developGame iterateMore moves i player game
    | i > 8 =
        -- if i arrives to the last position, iterate again from 0
        -- but do it only once
        if iterateMore
        then developGame False moves 0 player game
        -- draw game (after one iteration, still no winning moves)
        else (moves, player, game)
        -- draw game (game board full) or a win for the player
    | moves == 9 || tictactoeFor player game = (moves, player, game)
        -- make a move, if possible, and continue playing
    | otherwise = case place i otherPlayer game of
        -- position i is not empty. try with the next position
        Left _ -> developGame iterateMore moves (i + 1)
                    otherPlayer game
        -- position i was empty, so it was a valid move.
        -- change the player and make a new move, starting at pos 0
        Right newGame -> developGame iterateMore (moves + 1) 0
                    otherPlayer newGame
        where
            otherPlayer = changePlayer player

-- COMPUTER AI
-- starting from a given non-empty board, try to guess which position
-- could lead the player to the fastest victory.
bestMoveFor :: Char -> String -> Int
bestMoveFor player game = bestMove
    where
        -- drive the game to its end for each starting position
        continuations = [ (x, developGame True 0 x player game) |
            x <- [0..8] ]
        -- compare the number of moves of the game and take the
        -- shortest one
        move (_, (m, _, _)) = m
        (bestMove, _) = minimumBy (comparing move) continuations

-- canBlock checks if the opponent has two pieces in a row and the
-- other cell in the row is empty, and places the player's piece there,
-- blocking the opponent
canBlock :: Char -> String -> Maybe Int
canBlock p [a,b,c,d,e,f,g,h,i] =
    listToMaybe $ mapMaybe blockable [[a,b,c],[d,e,f],[g,h,i],[a,d,g],
                                      [b,e,h],[c,f,i],[a,e,i],[c,e,g]]
    where
        blockable xs = do
          guard $ length (filter (== otherPlayer) xs) == 2
          x <- find (`elem` "123456789") xs
          return $ digitToInt x
        otherPlayer = changePlayer p

-- format a game board for on-screen printing
showGame :: String -> String
showGame [a,b,c,d,e,f,g,h,i] =
    topBottom ++
    "|    | 1 | 2 | 3 |\n" ++
    topBottom ++
    row "0" [[a],[b],[c]] ++
    row "3" [[d],[e],[f]] ++
    row "6" [[g],[h],[i]]
    where
        topBottom = "+----+---+---+---+\n"
        row n x = "| " ++ n ++ "+ | " ++
            intercalate " | " x ++ " |\n" ++ topBottom

-- ask the user to press a numeric key and convert it to an int
enterNumber :: IO Int
enterNumber = do
    c <- getChar
    if c `elem` "123456789"
    then do
        putStrLn ""
        return $ digitToInt c
    else do
        putStrLn "\nPlease enter a digit!"
        enterNumber

-- a human player's turn: get the number of pieces put on the board,
-- the next piece to be put (X or O) and a game board, and return
-- a new game state, checking if the piece can be placed on the board.
-- if it can't, make the user try again.
turn :: (Int, Char, String) -> IO (Int, Char, String)
turn (count, player, game) = do
    putStr $ "Please tell me where you want to put an " ++
        [player] ++ ": "
    pos <- enterNumber
    case place (pos - 1) player game of
        Left oldGame -> do
            putStrLn "That place is already taken!\n"
            turn (count, player, oldGame)
        Right newGame ->
            return (count + 1, changePlayer player, newGame)

-- alternate between X and O players
changePlayer :: Char -> Char
changePlayer 'O' = 'X'
changePlayer 'X' = 'O'

-- COMPUTER AI
-- make an automatic turn, placing an X or an O game board.
-- the first movement is always random.
-- first, the computer looks for two pieces of his opponent in a row
-- and tries to block.
-- otherwise, it tries to guess the best position for the next movement.
-- as a last resort, it places a piece randomly.
autoTurn :: Bool -> (Int, Char, String) -> IO (Int, Char, String)
autoTurn forceRandom (count, player, game) = do
    -- try a random position 'cause everything else failed
    -- count == 0 overrides the value of forceRandom
    i <- if count == 0 || forceRandom
            then randomRIO (0,8)
            else return $
                case canBlock player game of
                    -- opponent can't be blocked. try to guess
                    -- the best movement
                    Nothing -> bestMoveFor player game
                    -- opponent can be blocked, so just do it!
                    Just blockPos -> blockPos
    -- if trying to place a piece at a calculated position doesn't work,
    -- just try again with a random value
    case place i player game of
        Left oldGame -> autoTurn True (count, player, oldGame)
        Right newGame -> do
            putStrLn $ "It's player " ++ [player] ++ "'s turn."
            return (count + 1, changePlayer player, newGame)

-- play a game until someone wins or the board becomes full.
-- depending on the value of the variable "auto", ask the user(s) to
-- put some pieces on the board or do it automatically
play :: Int -> (Int, Char, String) -> IO ()
play auto cpg@(_, player, game) = do
    newcpg@(newCount, newPlayer, newGame) <- case auto of
        -- if both players are human, always ask them
        0 -> turn cpg
        -- if both players are computer, always play auto
        1 -> autoTurn False cpg
        -- X is computer, O is human
        2 -> if player == 'X' then autoTurn False cpg else turn cpg
        -- X is human, O is computer
        3 -> if player == 'O' then autoTurn False cpg else turn cpg
    putStrLn $ "\n" ++ showGame newGame
    if tictactoe newGame
    then putStrLn $ "Player " ++ [changePlayer newPlayer] ++ " wins!\n"
    else
        if newCount == 9
        then putStrLn "Draw!\n"
        else play auto newcpg

-- main program: greet the user, ask for a game type, ask for the
-- player that'll start the game, and play the game beginning with an
-- empty board
main :: IO ()
main = do
    a <- getArgs
    if null a
    then usage
    else do
        let option = head a
        if option `elem` ["0","1","2","3"]
        then do
            putStrLn $ "\n" ++ showGame start
            let m = read option :: Int
            play m (0, 'X', start)
        else usage

usage :: IO ()
usage = do
    putStrLn "TIC-TAC-TOE GAME\n
### ==========
\n"
    putStrLn "How do you want to play?"
    putStrLn "Run the program with one of the following options."
    putStrLn "0 : both players are human"
    putStrLn "1 : both players are computer"
    putStrLn "2 : player X is computer and player O is human"
    putStrLn "3 : player X is human and player O is computer"
    putStrLn "Player X always begins."

```

{{out}}
Player X is computer, O is human.

```txt

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ |   |   |   |
+----+---+---+---+
| 3+ |   |   |   |
+----+---+---+---+
| 6+ |   |   |   |
+----+---+---+---+

It's player X's turn.

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ |   |   |   |
+----+---+---+---+
| 3+ |   | X |   |
+----+---+---+---+
| 6+ |   |   |   |
+----+---+---+---+

Please tell me where you want to put an O: 1

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O |   |   |
+----+---+---+---+
| 3+ |   | X |   |
+----+---+---+---+
| 6+ |   |   |   |
+----+---+---+---+

It's player X's turn.

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O |   | X |
+----+---+---+---+
| 3+ |   | X |   |
+----+---+---+---+
| 6+ |   |   |   |
+----+---+---+---+

Please tell me where you want to put an O: 7

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O |   | X |
+----+---+---+---+
| 3+ |   | X |   |
+----+---+---+---+
| 6+ | O |   |   |
+----+---+---+---+

It's player X's turn.

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O |   | X |
+----+---+---+---+
| 3+ | X | X |   |
+----+---+---+---+
| 6+ | O |   |   |
+----+---+---+---+

Please tell me where you want to put an O: 6

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O |   | X |
+----+---+---+---+
| 3+ | X | X | O |
+----+---+---+---+
| 6+ | O |   |   |
+----+---+---+---+

It's player X's turn.

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O |   | X |
+----+---+---+---+
| 3+ | X | X | O |
+----+---+---+---+
| 6+ | O | X |   |
+----+---+---+---+

Please tell me where you want to put an O: 2

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O | O | X |
+----+---+---+---+
| 3+ | X | X | O |
+----+---+---+---+
| 6+ | O | X |   |
+----+---+---+---+

It's player X's turn.

+----+---+---+---+
|    | 1 | 2 | 3 |
+----+---+---+---+
| 0+ | O | O | X |
+----+---+---+---+
| 3+ | X | X | O |
+----+---+---+---+
| 6+ | O | X | X |
+----+---+---+---+

Draw!

```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both Icon and Unicon.  The computer plays randomly against a human player, with legal moves enforced and wins/draws notified.


```Icon

# Play TicTacToe

$define E " " # empty square
$define X "X" # X piece
$define O "O" # O piece

# -- define a board
record Board(a, b, c, d, e, f, g, h, i)

procedure display_board (board, player)
  write ("\n
### =========
")
  write (board.a || " | " || board.b || " | " || board.c)
  write ("---------")
  write (board.d || " | " || board.e || " | " || board.f)
  write ("---------")
  write (board.g || " | " || board.h || " | " || board.i)
end

# return a set of valid moves (empty positions) in given board
procedure empty_fields (board)
  fields := set()
  every i := !fieldnames(board) do
    if (board[i] == E) then insert (fields, i)
  return fields
end

procedure game_start ()
  return Board (E, E, E, E, E, E, E, E, E)
end

procedure game_is_drawn (board)
  return *empty_fields(board) = 0
end

procedure game_won_by (board, player)
  return (board.a == board.b == board.c == player) |
         (board.d == board.e == board.f == player) |
         (board.g == board.h == board.i == player) |
         (board.a == board.d == board.g == player) |
         (board.b == board.e == board.h == player) |
         (board.c == board.f == board.i == player) |
         (board.a == board.e == board.i == player) |
         (board.g == board.e == board.c == player)
end

procedure game_over (board)
  return game_is_drawn (board) | game_won_by (board, O) | game_won_by (board, X)
end

# -- players make their move on the board
#    assume there is at least one empty square

procedure human_move (board, player)
  choice := "z"
  options := empty_fields (board)
  # keep prompting until player selects a valid square
  until member (options, choice) do {
    writes ("Choose one of: ")
    every writes (!options || " ")
    writes ("\n> ")
    choice := read ()
  }
  board[choice] := player
end

# pick and make a move at random from empty positions
procedure random_move (board, player)
  board[?empty_fields(board)] := player
end

# -- manage the game play
procedure play_game ()
  # hold procedures for players' move in variables
  player_O := random_move
  player_X := human_move

  # randomly determine if human or computer moves first
  if (?2 = 0)
    then {
      write ("Human plays first as O")
      player_O := human_move
      player_X := random_move
    }
    else write ("Computer plays first, human is X")

  # set up the game to start
  board := game_start ()
  player := O
  display_board (board, player)
  # loop until the game is over, getting each player to move in turn
  until game_over (board) do {
    write (player || " to play next")
    # based on player, prompt for the next move
    if (player == O)
      then (player_O (board, player))
      else (player_X (board, player))
    # change player to move
    player := if (player == O) then X else O
    display_board (board, player)
  }
  # finish by writing out result
  if game_won_by (board, O)
    then write ("O won")
    else if game_won_by (board, X)
      then write ("X won")
      else write ("draw") # neither player won, so must be a draw
end

# -- get things started
procedure main ()
  play_game ()
end

```



## J

To subsequent j poster: replacing this entry is fine by me.

```J

Note 'ttt adjudicates or plays'

   use:  markers ttt characters

   characters represent the cell names.

   markers is a length 3 character vector of the
   characters to use for first and second player
   followed by the opponent's mark.
   'XOX' means X plays 1st, O is the other mark,
   and the first strategy plays 1st.
   'XOO' means X plays 1st, O is the other mark,
   and the second strategy moves first.

   The game  is set  up for  the computer as the
   first strategy (random), and human as second.

   A standard use:
      'XOX'ttt'abcdefghijkl'

   Example game reformatted w/ emacs artist-mode
   to fit your display:

      '#-#'ttt'wersdfxcv'
   w│e│r                     w│e│r        ....    -│e│r           .    -│e│#
   ─┼─┼─                 .   ─┼─┼─       ..       ─┼─┼─         ..     ─┼─┼─
   s│d│f                 .   s│#│f      ..        s│#│f        ..      -│#│f
   ─┼─┼─                ..   ─┼─┼─      .         ─┼─┼─      ...       ─┼─┼─
   x│c│v               ..    -│c│v      .         -│c│#     ..         -│c│#
   d                  ..     v         ..         r         .          VICTORY
   w│e│r             ..      w│e│r     ..         -│e│#     .
   ─┼─┼─           ...       ─┼─┼─     ..         ─┼─┼─    .
   s│#│f         ...         s│#│f     ..         s│#│f    .
   ─┼─┼─        ..           ─┼─┼─   ...          ─┼─┼─  ...
   x│c│v                     -│c│#                -│c│#
   -->cell for -?            -->cell for -?       -->cell for -?
   x                         w                    s
)

while=: conjunction def 'u ^: v ^:_' NB. j assumes while is a verb and needs to know while is a conjunction.

ttt=: outcome@:((display@:move) while undecided)@:display@:prepare

blindfolded_variant=: outcome@:display@:(move while undecided)@:display@:prepare

outcome=: {&(>;:'kitty VICTORY')@:won   NB. (outcome does not pass along the state)
move=: post locate
undecided=: won nor full
prepare=: , board@:]                    NB. form the state vector

Note 'locate'
  is a monadic verb.  y is the state vector.
  returns the character of the chosen cell.
  Generally:
  locate=: second_strategy`first_strategy@.(first = mark)
  Simplified:
  locate=: human_locate NB. human versus human
)
locate=: human_locate`computer_locate@.(first = mark)

display=: show [: (1 1,:5 5)&(];.0)@:": [: <"0 fold

computer_locate=: [: show@{. board -. marks NB. strategy: first available
computer_locate=: [: show@({~ ?@:#) board -. marks NB. strategy: random

human_locate=: monad define
  state=. y
  m=. mark state
  b=. board state
  cells=. b -. marks state
  show '-->cell for ' , m , '?'
  whilst. cell -.@:e. cells do. cell =. {. (1!:1]1) , m end.
)

post=: 2&A.@:(3&{.)@:[ prepare mark@:[`((i.~ board)~)`(board@:[)}

mark=: {.                    NB. mark of the current player from state
marks=: 2&{.                 NB. extract both markers from state
board=: _9&{.                NB. extract board from state
first=: 2&{                  NB. extract first player from state

show=: [ smoutput

full=: 2 = #@:~.
won=: test@:fold
fold=: 3 3 $ board
test=: [: any [: all [: infix_pairs_agree |:@:lines

lines=: , diagonal , diagonal@:|. , |:
diagonal=: (<0 1)&|:
all=: *./
any=: +./
nor=: 8 b.
infix_pairs_agree=: 2&(=/\)

```



## Java

This version works in the terminal itself, and uses the numpad for data entry. The computer is unbeatable, but some lines can be removed to avoid that. There's also an override that thrown in, just for fun.

```java

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Hashtable;

public class TicTacToe
{
	public static void main(String[] args)
	{
		TicTacToe now=new TicTacToe();
		now.startMatch();
	}

	private int[][] marks;
	private int[][] wins;
	private int[] weights;
	private char[][] grid;
	private final int knotcount=3;
	private final int crosscount=4;
	private final int totalcount=5;
	private final int playerid=0;
	private final int compid=1;
	private final int truceid=2;
	private final int playingid=3;
	private String movesPlayer;
	private byte override;
	private char[][] overridegrid={{'o','o','o'},{'o','o','o'},{'o','o','o'}};
	private char[][] numpad={{'7','8','9'},{'4','5','6'},{'1','2','3'}};
	private Hashtable<Integer,Integer> crossbank;
	private Hashtable<Integer,Integer> knotbank;

	public void startMatch()
	{
		BufferedReader br=new BufferedReader(new InputStreamReader(System.in));
		System.out.print("Start?(y/n):");
		char choice='y';
		try
		{
			choice=br.readLine().charAt(0);
		}
		catch(Exception e)
		{
			System.out.println(e.getMessage());
		}
		if(choice=='n'||choice=='N')
		{
			return;
		}

		System.out.println("Use a standard numpad as an entry grid, as so:\n ");
		display(numpad);
		System.out.println("Begin");
		int playerscore=0;
		int compscore=0;
		do
		{
			int result=startGame();
			if(result==playerid)
				playerscore++;
			else if(result==compid)
				compscore++;
			System.out.println("Score: Player-"+playerscore+" AI-"+compscore);
			System.out.print("Another?(y/n):");
			try
			{
				choice=br.readLine().charAt(0);
			}
			catch(Exception e)
			{
				System.out.println(e.getMessage());
			}

		}while(choice!='n'||choice=='N');

		System.out.println("Game over.");
	}
	private void put(int cell,int player)
	{
		int i=-1,j=-1;;
		switch(cell)
		{
		case 1:i=2;j=0;break;
		case 2:i=2;j=1;break;
		case 3:i=2;j=2;break;
		case 4:i=1;j=0;break;
		case 5:i=1;j=1;break;
		case 6:i=1;j=2;break;
		case 7:i=0;j=0;break;
		case 8:i=0;j=1;break;
		case 9:i=0;j=2;break;
		default:display(overridegrid);return;
		}
		char mark='x';
		if(player==0)
			mark='o';
		grid[i][j]=mark;
		display(grid);
	}
	private int startGame()
	{
		init();
		display(grid);
		int status=playingid;
		while(status==playingid)
		{
			put(playerMove(),0);
			if(override==1)
			{
				System.out.println("O wins.");
				return playerid;
			}
			status=checkForWin();
			if(status!=playingid)
				break;
			try{Thread.sleep(1000);}catch(Exception e){System.out.print(e.getMessage());}
			put(compMove(),1);
			status=checkForWin();
		}
		return status;
	}
	private void init()
	{
		movesPlayer="";
		override=0;
		marks=new int[8][6];
		wins=new int[][]	//new int[8][3];
		{
				{7,8,9},
				{4,5,6},
				{1,2,3},
				{7,4,1},
				{8,5,2},
				{9,6,3},
				{7,5,3},
				{9,5,1}
		};
		weights=new int[]{3,2,3,2,4,2,3,2,3};
		grid=new char[][]{{' ',' ',' '},{' ',' ',' '},{' ',' ',' '}};
		crossbank=new Hashtable<Integer,Integer>();
		knotbank=new Hashtable<Integer,Integer>();
	}
	private void mark(int m,int player)
	{
		for(int i=0;i<wins.length;i++)
			for(int j=0;j<wins[i].length;j++)
				if(wins[i][j]==m)
				{
					marks[i][j]=1;
					if(player==playerid)
						marks[i][knotcount]++;
					else
						marks[i][crosscount]++;
					marks[i][totalcount]++;
				}
	}
	private void fixWeights()
	{
		for(int i=0;i<3;i++)
			for(int j=0;j<3;j++)
				if(marks[i][j]==1)
					if(weights[wins[i][j]-1]!=Integer.MIN_VALUE)
						weights[wins[i][j]-1]=Integer.MIN_VALUE;

		for(int i=0;i<8;i++)
		{
			if(marks[i][totalcount]!=2)
				continue;
			if(marks[i][crosscount]==2)
			{
				int p=i,q=-1;
				if(marks[i][0]==0)
					q=0;
				else if(marks[i][1]==0)
					q=1;
				else if(marks[i][2]==0)
					q=2;

				if(weights[wins[p][q]-1]!=Integer.MIN_VALUE)
				{
					weights[wins[p][q]-1]=6;
				}
			}
			if(marks[i][knotcount]==2)
			{
				int p=i,q=-1;
				if(marks[i][0]==0)
					q=0;
				else if(marks[i][1]==0)
					q=1;
				else if(marks[i][2]==0)
					q=2;

				if(weights[wins[p][q]-1]!=Integer.MIN_VALUE)
				{
					weights[wins[p][q]-1]=5;
				}
			}
		}
	}
	private int compMove()
	{
		int cell=move();
		System.out.println("Computer plays: "+cell);
		//weights[cell-1]=Integer.MIN_VALUE;
		return cell;
	}
	private int move()
	{
		int max=Integer.MIN_VALUE;
		int cell=0;
		for(int i=0;i<weights.length;i++)
			if(weights[i]>max)
			{
				max=weights[i];
				cell=i+1;
			}

		//This section ensures the computer never loses
		//Remove it for a fair match
		//Dirty kluge
		if(movesPlayer.equals("76")||movesPlayer.equals("67"))
			cell=9;
		else if(movesPlayer.equals("92")||movesPlayer.equals("29"))
			cell=3;
		else if (movesPlayer.equals("18")||movesPlayer.equals("81"))
			cell=7;
		else if(movesPlayer.equals("73")||movesPlayer.equals("37"))
			cell=4*((int)(Math.random()*2)+1);
		else if(movesPlayer.equals("19")||movesPlayer.equals("91"))
			cell=4+2*(int)(Math.pow(-1, (int)(Math.random()*2)));

		mark(cell,1);
		fixWeights();
		crossbank.put(cell, 0);
		return cell;
	}
	private int playerMove()
	{
		System.out.print("What's your move?: ");
		BufferedReader br=new BufferedReader(new InputStreamReader(System.in));
		int cell=0;
		int okay=0;
		while(okay==0)
		{
			try
			{
				cell=Integer.parseInt(br.readLine());
			}
			catch(Exception e)
			{
				System.out.println(e.getMessage());
			}
			if(cell==7494)
			{
				override=1;
				return -1;
			}
			if((cell<1||cell>9)||weights[cell-1]==Integer.MIN_VALUE)
				System.out.print("Invalid move. Try again:");
			else
				okay=1;
		}
		playerMoved(cell);
		System.out.println();
		return cell;
	}
	private void playerMoved(int cell)
	{
		movesPlayer+=cell;
		mark(cell,0);
		fixWeights();
		knotbank.put(cell, 0);
	}
	private int checkForWin()
	{
		int crossflag=0,knotflag=0;
		for(int i=0;i<wins.length;i++)
		{
			if(crossbank.containsKey(wins[i][0]))
				if(crossbank.containsKey(wins[i][1]))
					if(crossbank.containsKey(wins[i][2]))
					{
						crossflag=1;
						break;
					}
			if(knotbank.containsKey(wins[i][0]))
				if(knotbank.containsKey(wins[i][1]))
					if(knotbank.containsKey(wins[i][2]))
					{
						knotflag=1;
						break;
					}
		}
		if(knotflag==1)
		{
			display(grid);
			System.out.println("O wins.");
			return playerid;
		}
		else if(crossflag==1)
		{
			display(grid);
			System.out.println("X wins.");
			return compid;
		}

		for(int i=0;i<weights.length;i++)
			if(weights[i]!=Integer.MIN_VALUE)
				return playingid;
		System.out.println("Truce");

		return truceid;
	}
	private void display(char[][] grid)
	{
		for(int i=0;i<3;i++)
		{
			System.out.println("\n-------");
			System.out.print("|");
			for(int j=0;j<3;j++)
				System.out.print(grid[i][j]+"|");
		}
		System.out.println("\n-------");
	}
}

```



```txt

Start?(y/n):y
Use a standard numpad as an entry grid, as so:
-------
|7|8|9|
-------
|4|5|6|
-------
|1|2|3|
-------
Begin

-------
| | | |
-------
| | | |
-------
| | | |
-------
What's your move?: 4

-------
| | | |
-------
|o| | |
-------
| | | |
-------

(...)

Computer plays: 7
-------
|x| |o|
-------
|o|x|o|
-------
|x|o|x|
-------
X wins.

Score: Player-0 AI-1
Another?(y/n):n
Game over.

```



This version uses javax.swing.

```java
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.logging.Logger;
/**

* TicTacToe Application
* @author Steve Robinson
* @version 1.0
*/
class TicTacToeFrame extends JFrame
{
 JButton [][] buttons= new JButton[3][3];
 JTextField statusBar;
 GamePanel panel;
 Integer turn;
 GameListener listener=new GameListener();
 Integer count;
 public TicTacToeFrame()
 {
setLayout(new BorderLayout());
  panel=new GamePanel();
  add(panel,BorderLayout.CENTER);
  statusBar=new JTextField("Player1's Turn");
  statusBar.setEditable(false);
  add(statusBar,BorderLayout.SOUTH);
  setTitle("Tic Tac Toe!");
  setVisible(true);
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  setBounds(400,400,300,300);
 }
 class GamePanel extends JPanel
 {
  public GamePanel()
  {
   setLayout(new GridLayout(3,3));
   turn =1;
   count=0;
   for(int i=0;i<3;i++)
    for(int j=0;j<3;j++)   {
     buttons[i][j]=new JButton();
     buttons[i][j].putClientProperty("INDEX", new Integer[]{i,j});
     buttons[i][j].putClientProperty("OWNER", null);
     buttons[i][j].addActionListener(listener);
     add(buttons[i][j]);
    }
  }
 }
 class GameListener implements ActionListener
 {
  public void actionPerformed(ActionEvent e)
  {
   count++;
   JButton b=(JButton)e.getSource();
   Integer[]index=(Integer[]) b.getClientProperty("INDEX");
   //System.out.println(turn); //turn                  //   //System.out.println("["+index[0]+"]"+"["+index[1]+"]");         //
   b.putClientProperty("OWNER", turn);
   Icon ico=new ImageIcon(turn.toString()+".gif");
   b.setIcon(ico);
   b.setEnabled(false);
   boolean result=checkVictoryCondition(index);
   if(result)
   {
    JOptionPane.showMessageDialog(null, "Player "+turn.toString()+" Wins");
    initComponents();
   }
   else
   {
    if(turn==1)
    {
     turn=2;
     statusBar.setText("Player2's Turn");
    }
    else
    {
     turn=1;
     statusBar.setText("Player1's Turn");
    }
   }
   if(count==9)
   {
    JOptionPane.showMessageDialog(null, "Match is a draw!");
    initComponents();
   }
  }
  Integer getOwner(JButton b)
  {
   return (Integer)b.getClientProperty("OWNER");
  }
  //PrintButtonMap for Diagnostics
  void printbuttonMap(Integer [][]bMap)
  {
   for(int i=0;i    for(int j=0;j     System.out.print(bMap[i][j]+" ");
    System.out.println("");
   }
  }
  boolean checkVictoryCondition(Integer [] index)
  {
   /*Integer[][]buttonMap=new Integer[][] {

     { getOwner(buttons[0][0]),getOwner(buttons[0][1]),getOwner(buttons[0][2])},

     { getOwner(buttons[1][0]),getOwner(buttons[1][1]),getOwner(buttons[1][2])},

     { getOwner(buttons[2][0]),getOwner(buttons[2][1]),getOwner(buttons[2][2])}
   };
   printbuttonMap(buttonMap); */
   Integer a=index[0];
                Integer b=index[1];
   int i;
   //check row
   for(i=0;i<3;i++)  {
    if(getOwner(buttons[a][i])!=getOwner(buttons[a][b]))
     break;
   }
   if(i==3)
    return true;
   //check column
   for(i=0;i<3;i++)  {
    if(getOwner(buttons[i][b])!=getOwner(buttons[a][b]))
     break;
   }
   if(i==3)
    return true;
   //check diagonal
   if((a==2&&b==2)||(a==0&&b==0)||(a==1&&b==1)||(a==0&&b==2)||(a==2&&b==0))
   {
    //left diagonal
    for(i=0;i     if(getOwner(buttons[i][i])!=getOwner(buttons[a][b]))
      break;
    if(i==3)
     return true;
    //right diagonal
    if((getOwner(buttons[0][2])==getOwner(buttons[a][b]))&&(getOwner(buttons[1][1])==getOwner(buttons[a][b]))&&(getOwner(buttons[2][0])==getOwner(buttons[a][b])))
     return true;
    }
   return false;
  }
 }
 void initComponents()
 {
  for(int i=0;i<3;i++)
   for(int j=0;j<3;j++)  {
    buttons[i][j].putClientProperty("INDEX", new Integer[]{i,j});
    buttons[i][j].putClientProperty("OWNER",null);
    buttons[i][j].setIcon(null);
    buttons[i][j].setEnabled(true);
    turn=1;
    count=0;
    statusBar.setText("Player1's Turn");
   }
 }
}
class TicTacToe {
 public static void main(String[] args) {
  EventQueue.invokeLater(new Runnable(){
   public void run()
   {
    TicTacToeFrame frame=new TicTacToeFrame();
   }
  });
 }
}

```


Graphical Java Example

```java

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;


//Make sure the name of the class is the same as the .java file name.
//If you change the class name you should change the class object name in runGUI method
public class ticTacToeCallum implements ActionListener {

  static JFrame frame;
  static JPanel contentPane;
  static JLabel lblEnterFirstPlayerName, lblEnterSecondPlayerName, lblFirstPlayerScore, lblSecondPlayerScore;
  static JButton btnButton1, btnButton2, btnButton3, btnButton4, btnButton5, btnButton6, btnButton7, btnButton8, btnButton9, btnClearBoard, btnClearAll, btnCloseGame;
  static JTextField txtEnterFirstPlayerName, txtEnterSecondPlayerName;
  static Icon imgicon = new ImageIcon("saveIcon.JPG");

  Font buttonFont = new Font("Arial", Font.PLAIN, 20);


  //to adjust the frame size change the values in pixels
  static int width = 600;
  static int length = 400;
  static int firstPlayerScore = 0;
  static int secondPlayerScore = 0;
  static int playerTurn = 1;
  static int roundComplete = 0;
  static int button1 = 1, button2 = 1, button3 = 1, button4 = 1, button5 = 1, button6 = 1, button7 = 1, button8 = 1, button9 = 1; // 1 is true, 0 is false


  public ticTacToeCallum(){

    frame = new JFrame("Tic Tac Toe ^_^");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    contentPane = new JPanel();
    contentPane.setLayout(new GridLayout(6, 3, 10, 10));
    contentPane.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));

    btnButton1 = new JButton("");
    btnButton1.setFont(buttonFont);
    btnButton1.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton1.setIcon(imgicon);
    btnButton1.setActionCommand("CLICK1");
    btnButton1.addActionListener(this);
    contentPane.add(btnButton1);

    btnButton2 = new JButton("");
    btnButton2.setFont(buttonFont);
    btnButton2.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton2.setIcon(imgicon);
    btnButton2.setActionCommand("CLICK2");
    btnButton2.addActionListener(this);
    contentPane.add(btnButton2);

    btnButton3 = new JButton("");
    btnButton3.setFont(buttonFont);
    btnButton3.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton3.setIcon(imgicon);
    btnButton3.setActionCommand("CLICK3");
    btnButton3.addActionListener(this);
    contentPane.add(btnButton3);

    btnButton4 = new JButton("");
    btnButton4.setFont(buttonFont);
    btnButton4.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton4.setIcon(imgicon);
    btnButton4.setActionCommand("CLICK4");
    btnButton4.addActionListener(this);
    contentPane.add(btnButton4);

    btnButton5 = new JButton("");
    btnButton5.setFont(buttonFont);
    btnButton5.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton5.setIcon(imgicon);
    btnButton5.setActionCommand("CLICK5");
    btnButton5.addActionListener(this);
    contentPane.add(btnButton5);

    btnButton6 = new JButton("");
    btnButton6.setFont(buttonFont);
    btnButton6.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton6.setIcon(imgicon);
    btnButton6.setActionCommand("CLICK6");
    btnButton6.addActionListener(this);
    contentPane.add(btnButton6);

    btnButton7 = new JButton("");
    btnButton7.setFont(buttonFont);
    btnButton7.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton7.setIcon(imgicon);
    btnButton7.setActionCommand("CLICK7");
    btnButton7.addActionListener(this);
    contentPane.add(btnButton7);

    btnButton8 = new JButton("");
    btnButton8.setFont(buttonFont);
    btnButton8.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton8.setIcon(imgicon);
    btnButton8.setActionCommand("CLICK8");
    btnButton8.addActionListener(this);
    contentPane.add(btnButton8);

    btnButton9 = new JButton("");
    btnButton9.setFont(buttonFont);
    btnButton9.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnButton9.setIcon(imgicon);
    btnButton9.setActionCommand("CLICK9");
    btnButton9.addActionListener(this);
    contentPane.add(btnButton9);

    lblEnterFirstPlayerName = new JLabel("Enter First Player's Name");
    contentPane.add(lblEnterFirstPlayerName);

    txtEnterFirstPlayerName = new JTextField("");
    contentPane.add(txtEnterFirstPlayerName);

    lblFirstPlayerScore = new JLabel("Score: " + firstPlayerScore);
    contentPane.add(lblFirstPlayerScore);

    lblEnterSecondPlayerName = new JLabel("Enter Second Player's Name");
    contentPane.add(lblEnterSecondPlayerName);

    txtEnterSecondPlayerName = new JTextField("");
    contentPane.add(txtEnterSecondPlayerName);

    lblSecondPlayerScore = new JLabel("Score: " + secondPlayerScore);
    contentPane.add(lblSecondPlayerScore);

    btnClearBoard = new JButton("Clear Board");
    btnClearBoard.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnClearBoard.setIcon(imgicon);
    btnClearBoard.setActionCommand("CLICKClearBoard");
    btnClearBoard.addActionListener(this);
    contentPane.add(btnClearBoard);

    btnClearAll = new JButton("Clear All");
    btnClearAll.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnClearAll.setIcon(imgicon);
    btnClearAll.setActionCommand("CLICKClearAll");
    btnClearAll.addActionListener(this);
    contentPane.add(btnClearAll);

    btnCloseGame = new JButton("Close Game");
    btnCloseGame.setAlignmentX(JButton.CENTER_ALIGNMENT);
    btnCloseGame.setIcon(imgicon);
    btnCloseGame.setActionCommand("CLICKCloseGame");
    btnCloseGame.addActionListener(this);
    contentPane.add(btnCloseGame);

    frame.setContentPane(contentPane);
    frame.pack();
    frame.setSize(width,length);
    frame.setVisible(true);

  }

  public void actionPerformed(ActionEvent event) {
    String eventName = event.getActionCommand();
     if (eventName.equals("CLICK1")) {
    	 if (button1 == 1){
    		 if (playerTurn == 1){
    			 btnButton1.setForeground(Color.RED);
    			 btnButton1.setText("X");
   	  			 playerTurn = 2;
    			 button1 = 0;
    		 } else if (playerTurn == 2) {
    			 btnButton1.setForeground(Color.GREEN);
    			 btnButton1.setText("O");
    			 playerTurn = 1;
    			 button1 = 0;
    		 }
    	 }
      } else if (eventName.equals ("CLICK2")) {
    	  if (button2 == 1){
    	  	if (playerTurn == 1){
    	  		btnButton2.setForeground(Color.RED);
    	  		btnButton2.setText("X");
  	  			playerTurn = 2;
    	  		button2 = 0;
    	  	} else if (playerTurn == 2) {
    	  		btnButton2.setForeground(Color.GREEN);
    	  		btnButton2.setText("O");
    	  		playerTurn = 1;
    	  		button2 = 0;
    	  	}
    	  }
      }	else if (eventName.equals ("CLICK3")) {
    	  if (button3 == 1){
      	  	if (playerTurn == 1){
      	  		btnButton3.setForeground(Color.RED);
      	  		btnButton3.setText("X");
  	  			playerTurn = 2;
      	  		button3 = 0;
      	  	} else if (playerTurn == 2) {
      	  		btnButton3.setForeground(Color.GREEN);
      	  		btnButton3.setText("O");
      	  		playerTurn = 1;
      	  		button3 = 0;
      	  	}
      	  }
      }	else if (eventName.equals ("CLICK4")) {
    	  if (button4 == 1){
      	  	if (playerTurn == 1){
      	  		btnButton4.setForeground(Color.RED);
      	  		btnButton4.setText("X");
  	  			playerTurn = 2;
      	  		button4 = 0;
      	  	} else if (playerTurn == 2) {
      	  		btnButton4.setForeground(Color.GREEN);
      	  		btnButton4.setText("O");
      	  		playerTurn = 1;
      	  		button4 = 0;
      	  	}
      	  }
      }	else if (eventName.equals ("CLICK5")) {
    	  if (button5 == 1){
      	  	if (playerTurn == 1){
      	  		btnButton5.setForeground(Color.RED);
  	  			btnButton5.setText("X");
  	  			playerTurn = 2;
  	  			button5 = 0;
      	  	} else if (playerTurn == 2) {
      	  		btnButton5.setForeground(Color.GREEN);
  	  			btnButton5.setText("O");
  	  			playerTurn = 1;
  	  			button5 = 0;
      	  	}
      	  }
      } else if (eventName.equals ("CLICK6")) {
    	  if (button6 == 1){
      	  	if (playerTurn == 1){
      	  		btnButton6.setForeground(Color.RED);
  	  			btnButton6.setText("X");
  	  			playerTurn = 2;
  	  			button6 = 0;
      	  	} else if (playerTurn == 2) {
      	  		btnButton6.setForeground(Color.GREEN);
  	  			btnButton6.setText("O");
  	  			playerTurn = 1;
  	  			button6 = 0;
      	  	}
      	  }
      } else if (eventName.equals ("CLICK7")) {
    	  if (button7 == 1){
      	  	if (playerTurn == 1){
      	  		btnButton7.setForeground(Color.RED);
  	  			btnButton7.setText("X");
  	  			playerTurn = 2;
  	  			button7 = 0;
      	  	} else if (playerTurn == 2) {
      	  		btnButton7.setForeground(Color.GREEN);
  	  			btnButton7.setText("O");
  	  			playerTurn = 1;
  	  			button7 = 0;
      	  	}
      	  }
      } else if (eventName.equals ("CLICK8")) {
    	  if (button8 == 1){
      	  	if (playerTurn == 1){
      	  		btnButton8.setForeground(Color.RED);
  	  			btnButton8.setText("X");
  	  			playerTurn = 2;
  	  			button8 = 0;
      	  	} else if (playerTurn == 2) {
      	  		btnButton8.setForeground(Color.GREEN);
  	  			btnButton8.setText("O");
  	  			playerTurn = 1;
  	  			button8 = 0;
      	  	}
      	  }
      } else if (eventName.equals ("CLICK9")) {
    	  if (button9 == 1){
      	  	if (playerTurn == 1){
      	  		btnButton9.setForeground(Color.RED);
  	  			btnButton9.setText("X");
  	  			playerTurn = 2;
  	  			button9 = 0;
      	  	} else if (playerTurn == 2) {
      	  		btnButton9.setForeground(Color.GREEN);
  	  			btnButton9.setText("O");
  	  			playerTurn = 1;
  	  			button9 = 0;
      	  	}
      	  }
      } else if (eventName.equals ("CLICKClearBoard")) {

    	  btnButton1.setText("");
          btnButton2.setText("");
          btnButton3.setText("");
          btnButton4.setText("");
          btnButton5.setText("");
          btnButton6.setText("");
          btnButton7.setText("");
          btnButton8.setText("");
          btnButton9.setText("");

          button1 = 1;
          button2 = 1;
          button3 = 1;
          button4 = 1;
          button5 = 1;
          button6 = 1;
          button7 = 1;
          button8 = 1;
          button9 = 1;

          playerTurn = 1;

          roundComplete = 0;

      } else if (eventName.equals ("CLICKClearAll")) {

    	  btnButton1.setText("");
          btnButton2.setText("");
          btnButton3.setText("");
          btnButton4.setText("");
          btnButton5.setText("");
          btnButton6.setText("");
          btnButton7.setText("");
          btnButton8.setText("");
          btnButton9.setText("");

          firstPlayerScore = 0;
          lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
          secondPlayerScore = 0;
          lblSecondPlayerScore.setText("Score: " + secondPlayerScore);

          txtEnterFirstPlayerName.setText("");
          txtEnterSecondPlayerName.setText("");

          button1 = 1;
          button2 = 1;
          button3 = 1;
          button4 = 1;
          button5 = 1;
          button6 = 1;
          button7 = 1;
          button8 = 1;
          button9 = 1;

          playerTurn = 1;

          roundComplete = 0;

      } else if (eventName.equals ("CLICKCloseGame")) {
    	  System.exit(0);
      }
     score();
    }


  public static void score(){
	  if (roundComplete == 0){
	  if (btnButton1.getText().equals(btnButton2.getText())  && btnButton1.getText().equals(btnButton3.getText())){
	    	if (btnButton1.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton1.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	    if (btnButton1.getText().equals(btnButton4.getText())  && btnButton1.getText().equals(btnButton7.getText())){
	    	if (btnButton1.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton1.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	    if (btnButton1.getText().equals(btnButton5.getText())  && btnButton1.getText().equals(btnButton9.getText())){
	    	if (btnButton1.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton1.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	    if (btnButton7.getText().equals(btnButton8.getText())  && btnButton7.getText().equals(btnButton9.getText())){
	    	if (btnButton7.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton7.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	    if (btnButton7.getText().equals(btnButton5.getText())  && btnButton7.getText().equals(btnButton3.getText())){
	    	if (btnButton7.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton7.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	    if (btnButton3.getText().equals(btnButton6.getText())  && btnButton3.getText().equals(btnButton9.getText())){
	    	if (btnButton3.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton3.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	    if (btnButton4.getText().equals(btnButton5.getText())  && btnButton4.getText().equals(btnButton6.getText())){
	    	if (btnButton4.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton4.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	    if (btnButton2.getText().equals(btnButton5.getText())  && btnButton2.getText().equals(btnButton8.getText())){
	    	if (btnButton2.getText().equals("X")){
	    		firstPlayerScore += 1;
	    		lblFirstPlayerScore.setText("Score: " + firstPlayerScore);
	    		roundComplete = 1;
	    	} else if (btnButton2.getText().equals("O")){
	    		secondPlayerScore += 1;
	    		lblSecondPlayerScore.setText("Score: " + secondPlayerScore);
	    		roundComplete = 1;
	    	}
	    }
	  }
	    if (roundComplete == 1){
	    	button1 = 0;
	    	button2 = 0;
	    	button3 = 0;
	    	button4 = 0;
	    	button5 = 0;
	    	button6 = 0;
	    	button7 = 0;
	    	button8 = 0;
	    	button9 = 0;
	    }
  }

  /**
   * Create and show the GUI.
   */
  private static void runGUI() {
    ticTacToeCallum        greeting     = new ticTacToeCallum();
  }



  //Do not change this method
  public static void main(String[] args) {
    /* Methods that create and show a GUI should be run from an event-dispatching thread */
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        runGUI();
      }
    });
  }
}

```



## Javascript

HTML5 Canvas implementation. Should play perfectly or near-perfectly.

```Javascript

<!DOCTYPE html>

<html>

<head>
    <meta charset="utf-8" />
    <title>TicTacToe</title>
</head>

<body>
    <canvas id="canvas" width="400" height="400"></canvas>

    <script>
        //All helper functions
        isBetween = (num, a, b) => {
            return num >= a && num <= b;
        }

        randInt = (low, high) => {
            return Math.floor(Math.random() * (high - low + 1)) + low;
        }

        choice = arr => {
            return arr[randInt(0, arr.length - 1)];
        }

        //Checks if every value in an array equals an item
        equals = (arr, item) => {
            return arr.filter(a => {
                return a === item;
            }).length === arr.length;
        }

        //Returns number of items in array that equal an item
        equallen = (arr, item) => {
            return arr.filter(a => {
                return a === item;
            }).length
        }

        //Checks if any value in the array equals an item
        equalanyof = (arr, item) => {
            return equallen(arr, item) > 0;
        }

        //Should be scalable, but it uses default elements for calculations and tracking
        let canvas = document.getElementById("canvas");
        let ctx = canvas.getContext("2d");
        const width = canvas.width;
        const blockSize = canvas.width / 3;
        const lineSize = blockSize / 5;

        //Draws background
        ctx.fillStyle = "rgb(225, 225, 225)";
        ctx.fillRect(0, 0, 400, 400);

        //Title page
        ctx.fillStyle = "rgb(0, 0, 0)";
        ctx.font = width / (250 / 17) + "px Arial"; //34
        ctx.textAlign = "center";
        ctx.fillText("Tic Tac Toe", width / 2, width / (2 + 2 / 3)); //200, 150

        //Button for starting
        ctx.fillStyle = "rgb(200, 200, 200)";
        ctx.fillRect(width / 3.2, width / 2, width / (2 + 2 / 3), width / 8); //125, 200, 150, 50
        ctx.fillStyle = "rgb(0, 0, 0)";
        ctx.font = width / (200 / 9) + "px Arial"; //18
        ctx.fillText("Start", width / 2, width / (40 / 23)); //200, 230

        //Uses an array so a forEach loop can scan it for the correct tile
        let tileArray = []; //Contains all tiles
        let available = []; //Contains only available tiles

        class Tile {
            constructor(x, y) {
                this.x = x * blockSize;
                this.y = y * blockSize;
                this.state = "none";
                tileArray.push(this);
                available.push(this);
            }

            draw() {
                ctx.strokeStyle = "rgb(175, 175, 175)";
                ctx.lineWidth = blockSize / 10;

                if (this.state === "X") {
                    ctx.beginPath();
                    ctx.moveTo(this.x + blockSize / 4, this.y + blockSize / 4);
                    ctx.lineTo(this.x + blockSize / (4 / 3), this.y + blockSize / (4 / 3));
                    ctx.moveTo(this.x + blockSize / 4, this.y + blockSize / (4 / 3));
                    ctx.lineTo(this.x + blockSize / (4 / 3), this.y + blockSize / 4);
                    ctx.stroke();
                } else if (this.state === "O") {
                    ctx.beginPath();
                    ctx.arc(this.x + blockSize / 2, this.y + blockSize / 2, blockSize / 4, 0, 2 * Math.PI);
                    ctx.stroke();
                }

                //Removes this from the available array
                const ind = available.indexOf(this);
                available = available.slice(0, ind).concat(available.slice(ind + 1, available.length));
            }
        }


        //Defines the game
        let game = {
            state: "start",
            turn: "Player",
            player: "X",
            opp: "O"
        }

        //Generates tiles
        for (let x = 0; x < 3; x++) {
            for (let y = 0; y < 3; y++) {
                new Tile(x, y);
            }
        }

        //Gets the mouse position
        getMousePos = evt => {
            let rect = canvas.getBoundingClientRect();
            return {
                x: evt.clientX - rect.left,
                y: evt.clientY - rect.top
            }
        }

        //Checks for win conditions
        checkCondition = () => {
            //Local variables are created to make access easier
            let as = tileArray[0].state;
            let bs = tileArray[1].state;
            let cs = tileArray[2].state;
            let ds = tileArray[3].state;
            let es = tileArray[4].state;
            let fs = tileArray[5].state;
            let gs = tileArray[6].state;
            let hs = tileArray[7].state;
            let is = tileArray[8].state;

            //Equals function checks if each value in the array has a state of X or O
            if (equals([as, bs, cs], "X") || equals([ds, es, fs], "X") || equals([gs, hs, is], "X") ||
                equals([as, ds, gs], "X") || equals([bs, es, hs], "X") || equals([cs, fs, is], "X") ||
                equals([as, es, is], "X") || equals([cs, es, gs], "X")) {
                alert("Player wins!");
                game.state = "over";
            } else if (equals([as, bs, cs], "O") || equals([ds, es, fs], "O") || equals([gs, hs, is], "O") ||
                equals([as, ds, gs], "O") || equals([bs, es, hs], "O") || equals([cs, fs, is], "O") ||
                equals([as, es, is], "O") || equals([cs, es, gs], "O")) {
                alert("Opponent wins!");
                game.state = "over";
                //It is a tie if none of the above conditions are fulfilled and there are no available tiles
            } else if (available.length === 0) {
                alert("It's a tie!");
                game.state = "over";
            }
        }

        //Controls the opponent. Uses many nested switches/if-else for efficiency
        oppTurn = () => {
            if (game.state === "game") {
                let tile = 0;

                //Similar local variables as the win checker
                let at = tileArray[0].state;
                let bt = tileArray[1].state;
                let ct = tileArray[2].state;
                let dt = tileArray[3].state;
                let et = tileArray[4].state;
                let ft = tileArray[5].state;
                let gt = tileArray[6].state;
                let ht = tileArray[7].state;
                let it = tileArray[8].state;
                let all = [at, bt, ct, dt, et, ft, gt, ht, it];

                /*The AI will automatically win if possible
                I considered using a filter based system, but it was ugly and
                inelegant, and also redundant
                I used a nested if-else instead
                Equallen checks how many values in the array equal the given value*/
                if (equallen(all, "O") >= 2) {
                    if (equallen([at, bt, ct], "O") === 2 && equallen([at, bt, ct], "X") === 0) {
                        if (at === "none") {
                            tile = tileArray[0];
                        } else if (bt === "none") {
                            tile = tileArray[1];
                        } else if (ct === "none") {
                            tile = tileArray[2];
                        }
                    } else if (equallen([dt, et, ft], "O") === 2 && equallen([dt, et, ft], "X") === 0) {
                        if (dt === "none") {
                            tile = tileArray[3];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (ft === "none") {
                            tile = tileArray[5];
                        }
                    } else if (equallen([gt, ht, it], "O") === 2 && equallen([gt, ht, it], "X") === 0) {
                        if (gt === "none") {
                            tile = tileArray[6];
                        } else if (ht === "none") {
                            tile = tileArray[7];
                        } else if (it === "none") {
                            tile = tileArray[8];
                        }
                    } else if (equallen([at, dt, gt], "O") === 2 && equallen([at, dt, gt], "X") === 0) {
                        if (at === "none") {
                            tile = tileArray[0];
                        } else if (dt === "none") {
                            tile = tileArray[3];
                        } else if (gt === "none") {
                            tile = tileArray[6];
                        }
                    } else if (equallen([bt, et, ht], "O") === 2 && equallen([bt, et, ht], "X") === 0) {
                        if (bt === "none") {
                            tile = tileArray[1];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (ht === "none") {
                            tile = tileArray[7];
                        }
                    } else if (equallen([ct, ft, it], "O") === 2 && equallen([ct, ft, it], "X") === 0) {
                        if (ct === "none") {
                            tile = tileArray[2];
                        } else if (ft === "none") {
                            tile = tileArray[5];
                        } else if (it === "none") {
                            tile = tileArray[8];
                        }
                    } else if (equallen([at, et, it], "O") === 2 && equallen([at, et, it], "X") === 0) {
                        if (at === "none") {
                            tile = tileArray[0];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (it === "none") {
                            tile = tileArray[8];
                        }
                    } else if (equallen([ct, et, gt], "O") === 2 && equallen([ct, et, gt], "X") === 0) {
                        if (ct === "none") {
                            tile = tileArray[2];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (gt === "none") {
                            tile = tileArray[6];
                        }
                    }
                }

                //Stops player from winning if possible
                if (equallen(all, "X") >= 2 && tile === 0) {
                    if (equallen([at, bt, ct], "X") === 2 && equallen([at, bt, ct], "O") === 0) {
                        if (at === "none") {
                            tile = tileArray[0];
                        } else if (bt === "none") {
                            tile = tileArray[1];
                        } else if (ct === "none") {
                            tile = tileArray[2];
                        }
                    } else if (equallen([dt, et, ft], "X") === 2 && equallen([dt, et, ft], "O") === 0) {
                        if (dt === "none") {
                            tile = tileArray[3];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (ft === "none") {
                            tile = tileArray[5];
                        }
                    } else if (equallen([gt, ht, it], "X") === 2 && equallen([gt, ht, it], "O") === 0) {
                        if (gt === "none") {
                            tile = tileArray[6];
                        } else if (ht === "none") {
                            tile = tileArray[7];
                        } else if (it === "none") {
                            tile = tileArray[8];
                        }
                    } else if (equallen([at, dt, gt], "X") === 2 && equallen([at, dt, gt], "O") === 0) {
                        if (at === "none") {
                            tile = tileArray[0];
                        } else if (dt === "none") {
                            tile = tileArray[3];
                        } else if (gt === "none") {
                            tile = tileArray[6];
                        }
                    } else if (equallen([bt, et, ht], "X") === 2 && equallen([bt, et, ht], "O") === 0) {
                        if (bt === "none") {
                            tile = tileArray[1];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (ht === "none") {
                            tile = tileArray[7];
                        }
                    } else if (equallen([ct, ft, it], "X") === 2 && equallen([ct, ft, it], "O") === 0) {
                        if (ct === "none") {
                            tile = tileArray[2];
                        } else if (ft === "none") {
                            tile = tileArray[5];
                        } else if (it === "none") {
                            tile = tileArray[8];
                        }
                    } else if (equallen([at, et, it], "X") === 2 && equallen([at, et, it], "O") === 0) {
                        if (at === "none") {
                            tile = tileArray[0];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (it === "none") {
                            tile = tileArray[8];
                        }
                    } else if (equallen([ct, et, gt], "X") === 2 && equallen([ct, et, gt], "O") === 0) {
                        if (ct === "none") {
                            tile = tileArray[2];
                        } else if (et === "none") {
                            tile = tileArray[4];
                        } else if (gt === "none") {
                            tile = tileArray[6];
                        }
                    }
                }

                //Other options in case the above are not fulfilled
                //Controls the course of play over the game
                if (tile === 0) {
                    switch (9 - available.length) {
                        case 1:
                            //If the center is taken, it plays randomly in the corner
                            //Otherwise, it takes the center
                            if (et === "X") {
                                tile = tileArray[choice([0, 2, 6, 8])];
                            } else {
                                tile = tileArray[4];
                            }
                            break;

                        case 3:
                            if (et === "X" && (equalanyof([at, ct, gt, it], "O"))) {
                                /*To counter the strategy of
                                    O - -
                                    - X -
                                    X - -

                                    O - -
                                    - X -
                                    - - X
                                and related strategies*/
                                if (at === "X") {
                                    if (it === "none") {
                                        tile = tileArray[8];
                                    } else {
                                        tile = tileArray[choice([2, 6])];
                                    }
                                } else if (ct === "X") {
                                    if (gt === "none") {
                                        tile = tileArray[6];
                                    } else {
                                        tile = tileArray[choice([0, 8])];
                                    }
                                } else if (gt === "X") {
                                    if (ct === "none") {
                                        tile = tileArray[2];
                                    } else {
                                        tile = tileArray[choice([0, 8])];
                                    }
                                } else if (it === "X") {
                                    if (at === "none") {
                                        tile = tileArray[0];
                                    } else {
                                        tile = tileArray[choice([2, 6])];
                                    }
                                }
                            } else {
                                tile = choice(tileArray);
                            }
                            break;
                    }
                }

                //Generates a random number if it could cause an error
                if (tile.state != "none") {
                    tile = choice(available);
                }

                //Draws the selection
                tile.state = game.opp;
                tile.draw();
                checkCondition();
                game.turn = "Player";
            }
        }

        //Click handler
        document.onclick = event => {
            let pos = getMousePos(event);

            switch (game.state) {
                case "start":
                    //Checks if the button was clicked
                    if (isBetween(pos.x, width / 3.2, width / (16 / 11)) && isBetween(pos.y, width / 2, width / 1.6)) {
                        game.state = "game"

                        //Draws the setup for the game
                        ctx.fillStyle = "rgb(225, 225, 225)";
                        ctx.fillRect(0, 0, 400, 400);

                        //Draws the lines
                        ctx.fillStyle = "rgb(200, 200, 200)";
                        ctx.fillRect(blockSize - lineSize / 2, 0, lineSize, width);
                        ctx.fillRect(blockSize * 2 - lineSize / 2, 0, lineSize, width);
                        ctx.fillRect(0, blockSize - lineSize / 2, width, lineSize);
                        ctx.fillRect(0, blockSize * 2 - lineSize / 2, width, lineSize);
                    }
                    break;

                case "game":
                    if (game.turn === "Player") {
                        //Goes through the tile array, checking if the click occurred there
                        tileArray.forEach(tile => {
                            if (isBetween(pos.x, tile.x, tile.x + blockSize) && isBetween(pos.y, tile.y, tile.y + blockSize)) {
                                if (available.indexOf(tile) != -1) {
                                    tile.state = game.player;
                                    tile.draw();
                                    checkCondition();
                                    game.turn = "Opponent";
                                    oppTurn();
                                }
                            }
                        });
                    }
                    break;
            }

        }
    </script>
</body>

</html>

```



A Node.js implementation using strategy heuristics as defined in the Wikipedia page linked above.
Some of the steps can be embargoed until the board only has n plays left. This makes for a bit of
randomness in the gameplay.
Human is X and goes first.


```Javascript

// Board
const topLeft = 1;
const topMid = 2;
const topRight = 3;
const midLeft = 4;
const center = 5;
const midRight = 6;
const botLeft = 7;
const botMid = 8;
const botRight = 9;
const tiles = [
  topLeft, topMid, topRight,
  midLeft, center, midRight,
  botLeft, botMid, botRight
];
const corners = [
  topLeft, topRight,
  botLeft, botRight
];
const sides = [
  topMid,
  midLeft, midRight,
  botMid
];
const winningCombos = [
  [topLeft, topMid, topRight],
  [midLeft, center, midRight],
  [botLeft, botMid, botRight],
  [topLeft, midLeft, botLeft],
  [topMid, center, botMid],
  [topRight, midRight, botRight],
  [topLeft, center, botRight],
  [topRight, center, botLeft],
];
const board = new Map();

// Utility
const reset = () => tiles.forEach(e => board.set(e, ' '));
const repeat = (s, n) => Array(n).fill(s).join('');
const fromBoard = e => board.get(e);
const notSpace = e => e !== ' ';
const occupied = e => notSpace(fromBoard(e));
const isAvailable = e => !occupied(e);
const notString = s => e => fromBoard(e) !== s;
const containsOnly = s => a => a.filter(occupied).map(fromBoard).join('') === s;
const chooseRandom = a => a[Math.floor(Math.random() * a.length)];
const legalPlays = () => tiles.filter(isAvailable);
const legalCorners = () => corners.filter(isAvailable);
const legalSides = () => sides.filter(isAvailable);
const opponent = s => s === 'X' ? 'O' : 'X';
const hasElements = a => a.length > 0;
const compose = (...fns) => (...x) => fns.reduce((a, b) => c => a(b(c)))(...x);
const isDef = t => t !== undefined;
const flatten = a => a.reduce((p, c) => [...p, ...c], []);

const findShared = a => [...flatten(a).reduce((p, c) =>
        p.has(c) ? p.set(c, [...p.get(c), c]) : p.set(c, [c]),
    new Map()).values()].filter(e => e.length > 1).map(e => e[0]);

const wrap = (f, s, p = 9) => n => {
  if (isDef(n) || legalPlays().length > p) {
    return n;
  }
  const r = f(n);
  if (isDef(r)) {
    console.log(`${s}: ${r}`);
  }
  return r;
};

const drawBoard = () => console.log(`
  ${[fromBoard(topLeft), fromBoard(topMid), fromBoard(topRight)].join('|')}
  -+-+-
  ${[fromBoard(midLeft), fromBoard(center), fromBoard(midRight)].join('|')}
  -+-+-
  ${[fromBoard(botLeft), fromBoard(botMid), fromBoard(botRight)].join('|')}
`);

const win = s => () => {
  if (winningCombos.find(containsOnly(repeat(s, 3)))) {
    console.log(`${s} wins!`);
    reset()
  } else if (hasElements(legalPlays())) {
    console.log(`${opponent(s)}s turn:`);
  } else {
    console.log('Draw!');
    reset();
  }
};

const play = s => n => occupied(n) ? console.log('Illegal') : board.set(n, s);


// Available strategy steps
const attack = (s, t = 2) => () => {
  const m = winningCombos.filter(containsOnly(repeat(s, t)));
  if (hasElements(m)) {
    return chooseRandom(chooseRandom(m).filter(notString(s)))
  }
};

const fork = (s, isDefence = false) => () => {
  let result;
  const p = winningCombos.filter(containsOnly(s));
  const forks = findShared(p).filter(isAvailable);

  // On defence, when there is only one fork, play it, else choose a
  // two-in-a row attack to force the opponent to not execute the fork.
  if (forks.length > 1 && isDefence) {
    const me = opponent(s);
    const twoInRowCombos = winningCombos.filter(containsOnly(repeat(me, 1)));
    const chooseFrom = twoInRowCombos.reduce((p, a) => {
      const avail = a.filter(isAvailable);
      avail.forEach((e, i) => {
        board.set(e, me).set(i ? avail[i - 1] : avail[i + 1], opponent(me));
        winningCombos.filter(containsOnly(repeat(s, 2))).length < 2
            ? p.push(e)
            : undefined;
      });
      avail.forEach(e => board.set(e, ' '));
      return p;
    }, []);
    result = hasElements(chooseFrom)
        ? chooseRandom(chooseFrom)
        : attack(opponent(s), 1)()
  }
  return result || chooseRandom(forks);
};

const defend = (s, t = 2) => attack(opponent(s), t);

const defendFork = s => fork(opponent(s), true);

const chooseCenter = () => isAvailable(center) ? center : undefined;

const chooseCorner = () => chooseRandom(legalCorners());

const chooseSide = () => chooseRandom(legalSides());

const randLegal = () => chooseRandom(legalPlays());

// Implemented strategy
const playToWin = s => compose(
    win(s),
    drawBoard,
    play(s),
    wrap(randLegal, 'Chose random'),
    wrap(chooseSide, 'Chose random side', 8),
    wrap(chooseCorner, 'Chose random corner', 8),
    wrap(chooseCenter, 'Chose center', 7),
    wrap(defendFork(s), 'Defended fork'),
    wrap(fork(s), 'Forked'),
    wrap(defend(s), 'Defended'),
    wrap(attack(s), 'Attacked')
);

// Prep players
const O = n => playToWin('O')(n);
const X = n => playToWin('X')(n);

// Begin
reset();
console.log("Let's begin...");
drawBoard();
console.log('X Begins: Enter a number from 1 - 9');

// Manage user input.
const standard_input = process.stdin;
const overLog = s => {
  process.stdout.moveCursor(0, -9);
  process.stdout.cursorTo(0);
  process.stdout.clearScreenDown();
  process.stdout.write(s);
};
standard_input.setEncoding('utf-8');
standard_input.on('data', (data) => {
  if (data === '\n') {
    overLog('O: ');
    O();
  } else {
    overLog(`X: Plays ${data}`);
    X(Number(data));
  }
});

```


{{out}}

```txt


Let's begin...
X: Plays 1
O: Chose random corner: 7
X: Plays 5
O: Defended: 9
X: Plays 8
O: Defended: 2
X: Plays 3
O: Chose random side: 6
X: Plays 4

  X|O|X
  -+-+-
  X|X|O
  -+-+-
  O|X|O
Draw!


```



## Julia

One move look-ahead algorithm. Computer plays to win or at least draw.

```julia
const winningpositions = [[1, 2, 3], [4, 5, 6], [7, 8, 9], [1, 4, 7],
    [2, 5, 8], [3, 6, 9],[1, 5, 9], [7, 5, 3]]

function haswon(brd, xoro)
    marked = findall(x -> x == xoro, brd)
    for pos in winningpositions
        if length(pos) <= length(marked) && pos == sort(marked)[1:3]
            return true
        end
    end
    false
end

function readcharwithprompt(prompt, expected)
    ret = '*'
    while !(ret in expected)
        print("\n", prompt, " ->  ")
        ret = lowercase(chomp(readline()))[1]
    end
    ret
end

availablemoves(brd) = findall(x -> x == ' ', brd)
cornersopen(brd) = [x for x in [1, 3, 7, 9] if brd[x] == ' ']
int2char(x) = Char(x + UInt8('0'))
char2int(x) = UInt8(x) - UInt8('0')
getyn(query) = readcharwithprompt(query, ['y', 'n'])
gettheirmove(brd) = char2int(readcharwithprompt("Your move(1-9)", int2char.(availablemoves(brd))))

function findwin(brd, xoro)
    tmpbrd = deepcopy(brd)
    for mv in availablemoves(tmpbrd)
        tmpbrd[mv] = xoro
        if haswon(tmpbrd, xoro)
            return mv
        end
        tmpbrd[mv] = ' '
    end
    return nothing
end

function choosemove(brd, mychar, theirchar)
    if all(x -> x == ' ', brd)
        brd[rand(cornersopen(brd))] = mychar # corner trap if starting game
    elseif availablemoves(brd) == [] # no more moves
        println("Game is over. It was a draw.")
        exit(0)
    elseif (x = findwin(brd, mychar)) != nothing || (x = findwin(brd, theirchar)) != nothing
        brd[x] = mychar # win if possible, block their win otherwise if their win is possible
    elseif brd[5] == ' '
        brd[5] = mychar # take center if open and not doing corner trap
    elseif (corners = cornersopen(brd)) != []
        brd[rand(corners)] = mychar # choose a corner over a side middle move
    else
        brd[rand(availablemoves(brd))] = mychar # random otherwise
    end
end

function display(brd)
    println("+-----------+")
    println("| ", brd[1], " | ", brd[2], " | ", brd[3], " |")
    println("| ", brd[4], " | ", brd[5], " | ", brd[6], " |")
    println("| ", brd[7], " | ", brd[8], " | ", brd[9], " |")
    println("+-----------+")
end

function tictactoe()
    board = fill(' ', 9)
    println("Board move grid:\n 1 2 3\n 4 5 6\n 7 8 9")
    yn = getyn("Would you like to move first (y/n)?")
    if yn == 'y'
        mychar = 'O'
        theirchar = 'X'
        board[gettheirmove(board)] = theirchar
    else
        mychar = 'X'
        theirchar = 'O'
    end
    while true
        choosemove(board, mychar, theirchar)
        println("Computer has moved.")
        display(board)
        if haswon(board, mychar)
            println("Game over. Computer wins!")
            exit(0)
        elseif availablemoves(board) == []
            break
        end
        board[gettheirmove(board)] = theirchar
        println("Player has moved.")
        display(board)
        if haswon(board, theirchar)
            println("Game over. Player wins!")
            exit(0)
        elseif availablemoves(board) == []
            break
        end
    end
    println("Game over. It was a draw.")
end

tictactoe()

```
 {{output}}
```txt

 Board move grid:
  1 2 3
  4 5 6
  7 8 9

 Would you like to move first (y/n)? ->  y
 Your move(1-9) ->  5
 Computer has moved.
 +-----------+
 |   |   |   |
 |   | X |   |
 | O |   |   |
 +-----------+

 Your move(1-9) ->  1
 Player has moved.
 +-----------+
 | X |   |   |
 |   | X |   |
 | O |   |   |
 +-----------+
 Computer has moved.
 +-----------+
 | X |   |   |
 |   | X |   |
 | O |   | O |
 +-----------+

 Your move(1-9) ->  3
 Player has moved.
 +-----------+
 | X |   | X |
 |   | X |   |
 | O |   | O |
 +-----------+
 Computer has moved.
 +-----------+
 | X |   | X |
 |   | X |   |
 | O | O | O |
 +-----------+
 Game over. Computer wins!

```



## Kotlin

{{trans|C}}

```scala
// version 1.1.51

import java.util.Random

val r = Random()
val b = Array(3) { IntArray(3) }  // board -> 0: blank; -1: computer; 1: human

var bestI = 0
var bestJ = 0

fun checkWinner(): Int {
    for (i in 0..2) {
        if (b[i][0] != 0 && b[i][1] == b[i][0] && b[i][2] == b[i][0]) return b[i][0]
        if (b[0][i] != 0 && b[1][i] == b[0][i] && b[2][i] == b[0][i]) return b[0][i]
    }
    if (b[1][1] == 0) return 0
    if (b[1][1] == b[0][0] && b[2][2] == b[0][0]) return b[0][0]
    if (b[1][1] == b[2][0] && b[0][2] == b[1][1]) return b[1][1]
    return 0
}

fun showBoard() {
    val t = "X O"
    for (i in 0..2) {
        for (j in 0..2) print("${t[b[i][j] + 1]} ")
        println()
    }
    println("-----")
}

fun testMove(value: Int, depth: Int): Int {
    var best = -1
    var changed = 0
    var score = checkWinner()
    if (score != 0) return if (score == value) 1 else -1
    for (i in 0..2) {
        for (j in 0..2) {
            if (b[i][j] != 0) continue
            b[i][j] = value
            changed = value
            score = -testMove(-value, depth + 1)
            b[i][j] = 0
            if (score <= best) continue
            if (depth == 0) {
                bestI = i
                bestJ = j
            }
            best = score
        }
    }
    return if (changed != 0) best else 0
}

fun game(user: Boolean): String {
    var u = user
    for (i in 0..2) b[i].fill(0)
    print("Board postions are numbered so:\n1 2 3\n4 5 6\n7 8 9\n")
    print("You have O, I have X.\n\n")

    for (k in 0..8) {
        while (u) {
            var move: Int?
            do {
                print("Your move: ")
                move = readLine()!!.toIntOrNull()
            }
            while (move != null && move !in 1..9)
            move = move!! - 1
            val i = move / 3
            val j = move % 3
            if (b[i][j] != 0) continue
            b[i][j] = 1
            break
        }
        if (!u) {
            if (k == 0) { // randomize if computer opens, less boring
                bestI = r.nextInt(Int.MAX_VALUE) % 3
                bestJ = r.nextInt(Int.MAX_VALUE) % 3
            }
            else testMove(-1, 0)
            b[bestI][bestJ] = -1
            val myMove = bestI * 3 + bestJ + 1
            println("My move: $myMove")
        }
        showBoard()
        val win = checkWinner()
        if (win != 0) return (if (win == 1) "You win" else "I win") + ".\n\n"
        u = !u
    }
    return "A draw.\n\n"
}

fun main(args: Array<String>) {
    var user = false
    while (true) {
        user = !user
        print(game(user))
        var yn: String
        do {
            print("Play again y/n: ")
            yn = readLine()!!.toLowerCase()
        }
        while (yn != "y" && yn != "n")
        if (yn != "y") return
        println()
    }
}
```


Sample game:

```txt

Board postions are numbered so:
1 2 3
4 5 6
7 8 9
You have O, I have X.

Your move: 2
  O


-----
My move: 1
X O


-----
Your move: 8
X O

  O
-----
My move: 5
X O
  X
  O
-----
Your move: 9
X O
  X
  O O
-----
My move: 7
X O
  X
X O O
-----
Your move: 3
X O O
  X
X O O
-----
My move: 4
X O O
X X
X O O
-----
I win.

Play again y/n: n

```



## Lasso

{{incomplete|Lasso|Computer doesn't ''play'' - it merely manages the board.}}
This example uses an HTML form for the UI, buttons representing the game state, and Lasso's built inn session handler to keep track of who's turn it is, what the game matrix state is, and the winner history.

As image uploads has been disabled, a live version can be viewed at: [http://jono.guthrie.net.nz/rosetta/Tic-tac-toe.lasso http://jono.guthrie.net.nz/rosetta/Tic-tac-toe.lasso]

```Lasso
[
session_start('user')
session_addvar('user', 'matrix')
session_addvar('user', 'winrecord')
session_addvar('user', 'turn')
var(matrix)->isNotA(::array) ? var(matrix = array('-','-','-','-','-','-','-','-','-'))
var(winrecord)->isNotA(::array) ? var(winrecord = array)
var(turn)->isNotA(::string) ? var(turn = 'x')

if(web_request->params->asStaticArray >> 'reset') => {
	$matrix = array('-','-','-','-','-','-','-','-','-')
	$turn = 'x'
}

with i in web_request->params->asStaticArray do => {
	if(#i->name->beginswith('p')) => {
		local(num = #i->name->asCopy)
		#num->removeLeading('p')
		#num = integer(#num)
		#num > 0 && $matrix->get(#num) == '-' ? $matrix->get(#num) = #i->value
		$turn == 'o' ? $turn = 'x' | $turn = 'o'
	}
}

local(
	istie 	= false,
	winner	= 'noone',
	clear	= false
)

// determine if we have a winner
if($matrix->find('-')->size < 9) => {
	local(winners = array('123','456','789','147','258','369','159','357'))
	loop(8) => {
		local(xscore = 0,oscore = 0,use = #winners->get(loop_count))
		with v in #use->values do => {
			$matrix->findposition('x') >> integer(#v) ? #xscore++
			$matrix->findposition('o') >> integer(#v) ? #oscore++
		}
		if(#xscore == 3) => {
			#winner = 'x'
			$winrecord->insert('x')
			#clear = true
			loop_abort
		}
		if(#oscore == 3) => {
			#winner = 'o'
			$winrecord->insert('o')
			#clear = true
			loop_abort
		}

	}

}
// determine if tie
if(not $matrix->find('-')->size && #winner == 'noone') => {
	#istie = true
	#winner = 'tie'
	$winrecord->insert('tie')
	#clear = true
}
]
<form action="?" method="post">
  <table>
    <tr>
      [loop(3) => {^]<td><button name="p[loop_count]" value="[$turn]"[
        $matrix->get(loop_count) != '-' || #winner != 'noone' ? ' disabled="disabled"'
      ]>[$matrix->get(loop_count) != '-' ? $matrix->get(loop_count) | ' ']</button></td>[^}]
    </tr>
    <tr>
      [loop(-from=4,-to=6) => {^]<td><button name="p[loop_count]" value="[$turn]"[
        $matrix->get(loop_count) != '-' || #winner != 'noone' ? ' disabled="disabled"'
      ]>[$matrix->get(loop_count) != '-' ? $matrix->get(loop_count) | ' ']</button></td>[^}]
    </tr>
    <tr>
      [loop(-from=7,-to=9) => {^]<td><button name="p[loop_count]" value="[$turn]"[
        $matrix->get(loop_count) != '-' || #winner != 'noone' ? ' disabled="disabled"'
      ]>[$matrix->get(loop_count) != '-' ? $matrix->get(loop_count) | ' ']</button></td>[^}]
    </tr>
  </table>
</form>
[if(#istie && #winner == 'tie')]
<p><b>It's a tie!</b></p>
[else(#winner != 'noone')]
<p>[#winner->uppercase&] won! Congratulations.</p>
[else]<math>Insert formula here</math>
<p>It is now [$turn]'s turn!</p>
[/if]
<p><a href="?reset">Reset</a></p>
[if($winrecord->size)]<p>Win record: [$winrecord->join(', ')]</p>[/if]
[if(#clear == true) => {
	$matrix = array('-','-','-','-','-','-','-','-','-')
	$turn = 'x'
}]
```



## Lingo

The standard way to create GUI apps in Lingo is to use the authoring tool "Director" as GUI builder. The code below instead uses a simple framework (stored in global "$") that eases programmatic GUI creation.<br />
Screenshot of application window: http://valentin.dasdeck.com/lingo/tic-tac-toe/tic-tac-toe-lingo.png<br />
"Human" cannot win this game.

```Lingo
global $ -- object representing simple framework
global gBoard -- current board image
global gBoardTemplate -- empty board image
global gHumanChip -- cross image
global gComputerChip -- circle image
global gM -- 3x3 matrix storing game state: 0=free cell, 1=human cell, -1=computer cell
global gStep -- index of current move (1..9)
global gGameOverFlag -- TRUE if current game is over

----------------------------------------
-- Entry point
----------------------------------------
on startMovie

    -- libs
    $.import("sprite")

    -- window properties
    _movie.stage.title = "Tic-Tac-Toe"
    _movie.stage.rect = rect(0, 0, 224, 310)
    _movie.centerStage = TRUE

    -- load images from filesystem
    m = new(#bitmap)
    m.importFileInto($.@("resources/cross.bmp"), [#trimWhiteSpace:FALSE])
    gHumanChip = m.image

    m = new(#bitmap)
    m.importFileInto($.@("resources/circle.bmp"), [#trimWhiteSpace:FALSE])
    gComputerChip = m.image

    -- create GUI
    m = new(#bitmap)
    m.importFileInto($.@("resources/board.bmp"))
    m.regpoint = point(0, 0)
    s = $.sprite.make(m, [#loc:point(20, 20)], TRUE)
    s.addListener(#mouseDown, _movie, #humanMove)

    gBoard = m.image
    gBoardTemplate = gBoard.duplicate()

    m = $.sprite.newMember(#button, [#text:"New Game (Human starts)", #fontstyle:"bold", #rect:rect(0, 0, 180, 0)])
    s = $.sprite.make(m, [#loc:point(20, 220)], TRUE)
    s.addListener(#mouseDown, _movie, #newGame, 1)

    m = $.sprite.newMember(#button, [#text:"New Game (Computer starts)", #fontstyle:"bold", #rect:rect(0, 0, 180, 0)])
    s = $.sprite.make(m, [#loc:point(20, 250)], TRUE)
    s.addListener(#mouseDown, _movie, #newGame, -1)

    m = $.sprite.newMember(#field, [#name:"feedback", #editable:FALSE, #fontstyle:"bold", #alignment:"center",\
        #border:0, #color:rgb(255, 0, 0), #rect:rect(0, 0, 180, 0)])
    s = $.sprite.make(m, [#loc:point(20, 280)], TRUE)

    newGame(1)

    -- show the application window
    _movie.updateStage()
    _movie.stage.visible = TRUE
end

----------------------------------------
-- Starts a new game
----------------------------------------
on newGame (whoStarts)
    -- reset board
    gBoard.copyPixels(gBoardTemplate, gBoardTemplate.rect, gBoardTemplate.rect)
    -- clear feedback
    member("feedback").text = ""
    -- reset states
    gM = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
    gStep = 0
    gGameOverFlag = FALSE
    if whoStarts=-1 then computerMove()
end

----------------------------------------
-- Handles a human move (mouse click)
----------------------------------------
on humanMove ()
    if gGameOverFlag then return
    -- find cell for mouse position
    p = _mouse.clickLoc - sprite(1).loc
    if p.locH mod 60<4 or p.locV mod 60<4 then return
    p = p / 60
    x = p[1] + 1
    y = p[2] + 1
    if gM[x][y] then return -- ignore illegal moves
    gM[x][y] = 1
    -- update cell image
    p = p * 60
    gBoard.copyPixels(gHumanChip, gHumanChip.rect.offset(4+p[1], 4+p[2]), gHumanChip.rect)
    -- proceed (unless game over)
    gStep = gStep + 1
    if not checkHumanMove(x, y) then computerMove()
end

----------------------------------------
-- Checks if human has won or game ended with draw
----------------------------------------
on checkHumanMove (x, y)
    if sum([gM[x][1], gM[x][2], gM[x][3]])=3 then return gameOver(1, [[x, 1], [x, 2], [x, 3]])
    if sum([gM[1][y], gM[2][y], gM[3][y]])=3 then return gameOver(1, [[1, y], [2, y], [3, y]])
    if x=y and sum([gM[1][1], gM[2][2], gM[3][3]])=3 then return gameOver(1, [[1, 1], [2, 2], [3, 3]])
    if x+y=4 and sum([gM[1][3], gM[2][2], gM[3][1]])=3 then return gameOver(1, [[1, 3], [2, 2], [3, 1]])
    if gStep=9 then return gameOver(0)
    return FALSE
end

----------------------------------------
-- Checks if selecting specified empty cell makes computer or human win
----------------------------------------
on checkCellWins (x, y, who)
    wins = who*2
    if sum([gM[1][y], gM[2][y], gM[3][y]]) = wins then return [[1, y], [2, y], [3, y]]
    if sum([gM[x][1], gM[x][2], gM[x][3]]) = wins then return [[x, 1], [x, 2], [x, 3]]
    if x=y and sum([gM[1][1], gM[2][2], gM[3][3]]) = wins then return [[1, 1], [2, 2], [3, 3]]
    if x+y=4 and sum([gM[1][3], gM[2][2], gM[3][1]]) = wins then return [[1, 3], [2, 2], [3, 1]]
    return FALSE
end

----------------------------------------
-- Handles game over
----------------------------------------
on gameOver (winner, cells)
    gGameOverFlag = TRUE
    if winner = 0 then
        member("feedback").text = "It's a draw!"
    else
        -- hilite winning line with yellow
        img = image(56, 56, 32)
        img.fill(img.rect, rgb(255, 255, 0))
        repeat with c in cells
            x = (c[1]-1)*60 + 4
            y = (c[2]-1)*60 + 4
            gBoard.copyPixels(img, img.rect.offset(x, y), img.rect, [#ink:#darkest])
        end repeat
        member("feedback").text = ["Human", "Computer"][1+(winner=-1)] & " has won!"
    end if
    return TRUE
end

----------------------------------------
-- Calculates next computer move
----------------------------------------
on computerMove ()
    gStep = gStep + 1

    -- move 1: select center
    if gStep=1 then return doComputerMove(2, 2)

    -- move 2 (human started)
    if gStep=2 then
        if gM[2][2]=1 then
            -- if center, select arbitrary corner
            return doComputerMove(1, 1)
        else
            -- otherwise select center
            return doComputerMove(2, 2)
        end if
    end if

    -- move 3 (computer started)
    if gStep=3 then
        -- if corner, select diagonally opposite corner
        if gM[1][1]=1 then return doComputerMove(3, 3)
        if gM[3][3]=1 then return doComputerMove(1, 1)
        if gM[1][3]=1 then return doComputerMove(3, 1)
        return doComputerMove(1, 1) -- top left corner as default
    end if

    -- get free cells
    free = []
    repeat with x = 1 to 3
        repeat with y = 1 to 3
            if gM[x][y]=0 then free.add([x, y])
        end repeat
    end repeat

    -- check if computer can win now
    repeat with c in free
        res = checkCellWins(c[1], c[2], -1)
        if res<>FALSE then
            doComputerMove(c[1], c[2])
            return gameOver(-1, res)
        end if
    end repeat

    -- check if human could win with next move (if yes, prevent it)
    repeat with c in free
        res = checkCellWins(c[1], c[2], 1)
        if res<>FALSE then return doComputerMove(c[1], c[2], TRUE)
    end repeat

    -- move 4 (human started): prevent "double mills"
    if gStep=4 then
        if gM[2][2]=1 and (gM[1][1]=1 or gM[3][3]=1) then return doComputerMove(3, 1)
        if gM[2][2]=1 and (gM[1][3]=1 or gM[3][1]=1) then return doComputerMove(1, 1)
        if gM[2][3]+gM[3][2]=2 then return doComputerMove(3, 3)
        if gM[1][2]+gM[2][3]=2 then return doComputerMove(1, 3)
        if gM[1][2]+gM[2][1]=2 then return doComputerMove(1, 1)
        if gM[2][1]+gM[3][2]=2 then return doComputerMove(3, 1)
        if (gM[1][3]+gM[3][1]=2) or (gM[1][1]+gM[3][3]=2) then return doComputerMove(2, 1)
    end if

    -- move 5 (computer started): try to create a "double mill"
    if gStep=5 then
        repeat with x = 1 to 3
            col = [gM[x][1], gM[x][2], gM[x][3]]
            if not (sum(col)=-1 and max(col)=0) then next repeat
            repeat with y = 1 to 3
                row = [gM[1][y], gM[2][y], gM[3][y]]
                if not (sum(row)=-1 and max(row)=0 and gM[x][y]=0) then next repeat
                return doComputerMove(x, y)
            end repeat
        end repeat
    end if

    -- otherwise use first free cell
    c = free[1]
    doComputerMove(c[1], c[2])
end

----------------------------------------
-- Updates state matrix and cell image
----------------------------------------
on doComputerMove (x, y, checkDraw)
    gM[x][y] = -1
    gBoard.copyPixels(gComputerChip, gComputerChip.rect.offset(4+(x-1)*60, 4+(y-1)*60), gComputerChip.rect)
    if checkDraw and gStep=9 then gameOver(0)
end

----------------------------------------
--
----------------------------------------
on sum (aLine)
    return aLine[1]+aLine[2]+aLine[3]
end
```


## M2000 Interpreter

Computer May loose;

```M2000 Interpreter

Module Tic.Tac.Toe {
      Dim Board$(1 to 3, 1 to 3)=" "
      WinGame=False
      p=Board$()
      RandomPosition=lambda -> {
            =(random(1,3), random(1,3))
      }

      BoardItemEmpty=Lambda p (x, y) -> {
            =Array$(p, x, y)=" "
      }
      BoardSetItem=Lambda p (x, y, w$) -> {
            link p to a$()
            a$(x, y)=w$
      }
      T=9
      R=0
      C=0
      Repeat {
            Print "Computer Move:"
            CompMove()
            T--
            DrawBoard()
            CheckWin()
            if WinGame Then Print "Computer Win": Exit
            if T=0 then exit
            Repeat {
                  GetRowCol("Input Row", &R)
                  GetRowCol("Input Column", &C)
                  If BoardItemEmpty(R,C) then call boardsetitem(R,C,"O") : exit
            } Always
            T--
            DrawBoard()
            CheckWin()
            if WinGame Then Print "You Win": Exit
      } until T=0 or WinGame
      Sub DrawBoard()
      Print "R/C 1 2 3"
      Print " 1) "; Board$(1,1);"|";Board$(1,2);"|";Board$(1,3)
      Print "    -+-+-"
      Print " 2) "; Board$(2,1);"|";Board$(2,2);"|";Board$(2,3)
      Print "    -+-+-"
      Print " 3) "; Board$(3,1);"|";Board$(3,2);"|";Board$(3,3)
      End Sub
      Sub CheckWin()
            WinGame=false
            local i,j,three$
            For i=1 to 3
                  three$=""
                  For j=1 to 3 : three$+=Board$(i,j) : Next j
                  CheckThree()
                  three$=""
                  For j=1 to 3 :  three$+=Board$(j,i) :Next j
                  CheckThree()
            Next i
            three$=""
            For i=1 to 3 : three$+=Board$(i,i): Next i
            CheckThree()
            three$=""
            For i=1 to 3:three$+=Board$(i,4-i): Next i
            CheckThree()
      End Sub
      Sub CheckThree()
            if instr(three$," ")=0 then  WinGame=WinGame or Filter$(three$, left$(three$,1))=""
      End Sub
      Sub CompMove()
            if T<9 and Board$(2,2)=" " then {
                  call boardsetitem(2,2,"X")
            } Else {
                  local i=3, j=3, found=false
                  if T<=6 then {
                        CompThink("X","X")
                  }
                  let i=3, j=3
                  If Not found And T<6 then {
                        CompThink("O","X")
                  }
                  If not found then {
                        Repeat {
                              comp=RandomPosition()
                              If BoardItemEmpty(!comp) then call boardsetitem(!comp, "X") : exit
                        } Always
                  }
            }
      End Sub
      Sub CompThink(Bad$, Good$)
                        While i>0 {
                              j=3
                              While j>0 {
                                    if Board$(i,j)=" " then {
                                          Board$(i,j)=Bad$
                                          CheckWin()
                                          if WinGame then {
                                                 Board$(i,j)=Good$:i=0:j=0: found=true
                                          } Else Board$(i,j)=" "
                                    }
                                    j--
                              }
                              i--
                        }

      End Sub
      Sub GetRowCol(What$, &W)
            Print What$;":";
            Repeat {
                  W=Val("0"+Key$)
            } until W>=1 and W<=3
            Print Str$(W,"")
      End Sub
}
Tic.Tac.Toe

```

{{out}}
<pre style="height:30ex;overflow:scroll">
Computer Move:
R/C 1 2 3
 1)  | |
    -+-+-
 2)  | |
    -+-+-
 3)  | |X
Input Row:2
Input Column:2
R/C 1 2 3
 1)  | |
    -+-+-
 2)  |O|
    -+-+-
 3)  | |X
Computer Move:
R/C 1 2 3
 1)  | |
    -+-+-
 2)  |O|
    -+-+-
 3) X| |X
Input Row:3
Input Column:2
R/C 1 2 3
 1)  | |
    -+-+-
 2)  |O|
    -+-+-
 3) X|O|X
Computer Move:
R/C 1 2 3
 1)  |X|
    -+-+-
 2)  |O|
    -+-+-
 3) X|O|X
Input Row:2
Input Column:1
R/C 1 2 3
 1)  |X|
    -+-+-
 2) O|O|
    -+-+-
 3) X|O|X
Computer Move:
R/C 1 2 3
 1)  |X|
    -+-+-
 2) O|O|X
    -+-+-
 3) X|O|X
Input Row:1
Input Column:3
R/C 1 2 3
 1)  |X|O
    -+-+-
 2) O|O|X
    -+-+-
 3) X|O|X
Computer Move:
R/C 1 2 3
 1) X|X|O
    -+-+-
 2) O|O|X
    -+-+-
 3) X|O|X

</pre >


## Mathematica

<lang>DynamicModule[{board = ConstantArray[0, {3, 3}], text = "Playing...",
  first, rows =
   Join[#, Transpose@#, {Diagonal@#, Diagonal@Reverse@#}] &},
 Column@{Graphics[{Thickness[.02],
     Table[With[{i = i, j = j},
       Button[{White, Rectangle[{i, j} - 1, {i, j}], Black,
         Dynamic[Switch[board[[i, j]], 0, Black, 1,
           Circle[{i, j} - .5, .3], -1,
           Line[{{{i, j} - .2, {i, j} - .8}, {{i - .2,
               j - .8}, {i - .8, j - .2}}}]]]},
        Which[text != "Playing...", board = ConstantArray[0, {3, 3}];
         text = "Playing...", board[[i, j]] == 0,
         If[board == ConstantArray[0, {3, 3}],
          first = {i,
             j} /. {{2, 2} -> 1, {1 | 3, 1 | 3} -> 2, _ -> 3}];
         board[[i, j]] = 1;
         FinishDynamic[];
         Which[MemberQ[rows[board], {1, 1, 1}], text = "You win.",
          FreeQ[board, 0], text = "Draw.", True,
          board[[Sequence @@
              SortBy[Select[Tuples[{Range@3, Range@3}],
                 board[[Sequence @@ #]] ==
                   0 &], -Total[
                    Sort /@
                    rows[ReplacePart[
                    board, # -> -1]] /. {{-1, -1, -1} ->
                    512, {-1, 1, 1} -> 64, {-1, -1, 0} ->
                    8, {0, 1, 1} -> -1, {_, _, _} -> 0}] -
                  Switch[#, {2, 2}, 1, {1 | 3, 1 | 3},
                   If[first == 2, -1, 0], _,
                   If[first == 2, 0, -1]] &][[1]]]] = -1;
          Which[MemberQ[rows[board], {-1, -1, -1}],
           text = "You lost.", FreeQ[board, 0],
           text = "Draw."]]]]], {i, 1, 3}, {j, 1, 3}], Thickness[.01],
      Line[{{{1, 0}, {1, 3}}, {{2, 0}, {2, 3}}, {{0, 1}, {3, 1}}, {{0,
          2}, {3, 2}}}]}], Dynamic@text}]
```



## MATLAB

Allows for choice between any combination of human or computer players. Computer players are intelligent, but not perfect. It implements the "rules" used by the Newell and Simon's 1972 tic-tac-toe program (as explained by Wikipedia), but this implementation does not factor in the move before the move causing the fork (either for creation or prevention).

```MATLAB
function TicTacToe

    % Set up the board (one for each player)
    boards = false(3, 3, 2);    % Players' pieces
    rep = ['   1 | 4 | 7' ; '   2 | 5 | 8' ; '   3 | 6 | 9'];

    % Prompt user with options
    fprintf('Welcome to Tic-Tac-Toe!\n')
    nHumans = str2double(input('Enter the number of human players: ', 's'));
    if isnan(nHumans) || ceil(nHumans) ~= nHumans || nHumans < 1 || nHumans > 2
        nHumans = 0;
        pHuman = false(2, 1);
    elseif nHumans == 1
        humanFirst = input('Would the human like to go first (Y/N)? ', 's');
        if length(humanFirst) == 1 && lower(humanFirst) == 'n'
            pHuman = [false ; true];
        else
            pHuman = [true ; false];
        end
    else
        pHuman = true(2, 1);
    end
    if any('o' == input('Should Player 1 use X or O? ', 's'))
        marks = 'OX';
    else
        marks = 'XO';
    end
    fprintf('So Player 1 is %shuman and %cs and Player 2 is %shuman and %cs.\n', ...
        char('not '.*~pHuman(1)), marks(1), char('not '.*~pHuman(2)), marks(2))
    if nHumans > 0
        fprintf('Select the space to mark by entering the space number.\n')
        fprintf('No entry will quit the game.\n')
    end

    % Play game
    gameOver = false;
    turn = 1;
    while ~gameOver
        fprintf('\n')
        disp(rep)
        fprintf('\n')
        if pHuman(turn)
            [move, isValid, isQuit] = GetMoveFromPlayer(turn, boards);
            gameOver = isQuit;
        else
            move = GetMoveFromComputer(turn, boards);
            fprintf('Player %d chooses %d\n', turn, move)
            isValid = true;
            isQuit = false;
        end
        if isValid && ~isQuit
            [r, c] = ind2sub([3 3], move);
            boards(r, c, turn) = true;
            rep(r, 4*c) = marks(turn);
            if CheckWin(boards(:, :, turn))
                gameOver = true;
                fprintf('\n')
                disp(rep)
                fprintf('\nPlayer %d wins!\n', turn)
            elseif CheckDraw(boards)
                gameOver = true;
                fprintf('\n')
                disp(rep)
                fprintf('\nCat''s game!\n')
            end
            turn = ~(turn-1)+1;
        end
    end
end

function [move, isValid, isQuit] = GetMoveFromPlayer(pNum, boards)
% move - 1-9 indicating move position, 0 if invalid move
% isValid - logical indicating if move was valid, true if quitting
% isQuit - logical indicating if player wishes to quit game
    p1 = boards(:, :, 1);
    p2 = boards(:, :, 2);
    moveStr = input(sprintf('Player %d: ', pNum), 's');
    if isempty(moveStr)
        fprintf('Play again soon!\n')
        move = 0;
        isValid = true;
        isQuit = true;
    else
        move = str2double(moveStr);
        isQuit = false;
        if isnan(move) || move < 1 || move > 9 || p1(move) || p2(move)
            fprintf('%s is an invalid move.\n', moveStr)
            isQuit = 0;
            isValid = false;
        else
            isValid = true;
        end
    end
end

function move = GetMoveFromComputer(pNum, boards)
% pNum - 1-2 player number
% boards - 3x3x2 logical array where pBoards(:,:,1) is player 1's marks
% Assumes that it is possible to make a move
    if ~any(boards(:))     % Play in the corner for first move
        move = 1;
    else                    % Use Newell and Simon's "rules to win"
        pMe = boards(:, :, pNum);
        pThem = boards(:, :, ~(pNum-1)+1);
        possMoves = find(~(pMe | pThem)).';

        % Look for a winning move
        move = FindWin(pMe, possMoves);
        if move
            return
        end

        % Look to block opponent from winning
        move = FindWin(pThem, possMoves);
        if move
            return
        end

        % Look to create a fork (two non-blocked lines of two)
        for m = possMoves
            newPMe = pMe;
            newPMe(m) = true;
            if CheckFork(newPMe, pThem)
                move = m;
                return
            end
        end

        % Look to make two in a row so long as it doesn't force opponent to fork
        notGoodMoves = false(size(possMoves));
        for m = possMoves
            newPMe = pMe;
            newPMe(m) = true;
            if CheckPair(newPMe, pThem)
                nextPossMoves = possMoves;
                nextPossMoves(nextPossMoves == m) = [];
                theirMove = FindWin(newPMe, nextPossMoves);
                newPThem = pThem;
                newPThem(theirMove) = true;
                if ~CheckFork(newPThem, newPMe)
                    move = m;
                    return
                else
                    notGoodMoves(possMoves == m) = true;
                end
            end
        end
        possMoves(notGoodMoves) = [];

        % Play the center if available
        if any(possMoves == 5)
            move = 5;
        	return
        end

        % Play the opposite corner of the opponent's piece if available
        corners = [1 3 7 9];
        move = intersect(possMoves, ...
            corners(~(pMe(corners) | pThem(corners)) & pThem(fliplr(corners))));
        if ~isempty(move)
            move = move(1);
            return
        end

        % Play an empty corner if available
        move = intersect(possMoves, corners);
        if move
            move = move(1);
            return
        end

        % Play an empty side if available
        sides = [2 4 6 8];
        move = intersect(possMoves, sides);
        if move
            move = move(1);
            return
        end

        % No good moves, so move randomly
        possMoves = find(~(pMe | pThem));
        move = possMoves(randi(length(possMoves)));
    end
end

function move = FindWin(board, possMoves)
% board - 3x3 logical representing one player's pieces
% move - integer indicating position to move to win, or 0 if no winning move
    for m = possMoves
        newPMe = board;
        newPMe(m) = true;
        if CheckWin(newPMe)
            move = m;
            return
        end
    end
    move = 0;
end

function win = CheckWin(board)
% board - 3x3 logical representing one player's pieces
% win - logical indicating if that player has a winning board
    win = any(all(board)) || any(all(board, 2)) || ...
        all(diag(board)) || all(diag(fliplr(board)));
end

function fork = CheckFork(p1, p2)
% fork - logical indicating if player 1 has created a fork unblocked by player 2
    fork = sum([sum(p1)-sum(p2) (sum(p1, 2)-sum(p2, 2)).' ...
        sum(diag(p1))-sum(diag(p2)) ...
        sum(diag(fliplr(p1)))-sum(diag(fliplr(p2)))] == 2) > 1;
end

function pair = CheckPair(p1, p2)
% pair - logical indicating if player 1 has two in a line unblocked by player 2
    pair = any([sum(p1)-sum(p2) (sum(p1, 2)-sum(p2, 2)).' ...
        sum(diag(p1))-sum(diag(p2)) ...
        sum(diag(fliplr(p1)))-sum(diag(fliplr(p2)))] == 2);
end

function draw = CheckDraw(boards)
% boards - 3x3x2 logical representation of all players' pieces
    draw = all(all(boards(:, :, 1) | boards(:, :, 2)));
end
```

{{out}}
Computer goes first and plays perfectly:

```txt
Welcome to Tic-Tac-Toe!
Enter the number of human players: 1
Would the human like to go first (Y/N)? n
Should Player 1 use X or O? x
So Player 1 is not human and Xs and Player 2 is human and Os.
Select the space to mark by entering the space number.
No entry will quit the game.

   1 | 4 | 7
   2 | 5 | 8
   3 | 6 | 9

Player 1 chooses 1

   X | 4 | 7
   2 | 5 | 8
   3 | 6 | 9

Player 2: 4

   X | O | 7
   2 | 5 | 8
   3 | 6 | 9

Player 1 chooses 2

   X | O | 7
   X | 5 | 8
   3 | 6 | 9

Player 2: 3

   X | O | 7
   X | 5 | 8
   O | 6 | 9

Player 1 chooses 5

   X | O | 7
   X | X | 8
   O | 6 | 9

Player 2: 8

   X | O | 7
   X | X | O
   O | 6 | 9

Player 1 chooses 9

   X | O | 7
   X | X | O
   O | 6 | X

Player 1 wins!
```

Computer goes first, but misses opportunity to set up for a fork, setting up human player instead:

```txt
Welcome to Tic-Tac-Toe!
Enter the number of human players: 1
Would the human like to go first (Y/N)? n
Should Player 1 use X or O? x
So Player 1 is not human and Xs and Player 2 is human and Os.
Select the space to mark by entering the space number.
No entry will quit the game.

   1 | 4 | 7
   2 | 5 | 8
   3 | 6 | 9

Player 1 chooses 1

   X | 4 | 7
   2 | 5 | 8
   3 | 6 | 9

Player 2: 9

   X | 4 | 7
   2 | 5 | 8
   3 | 6 | O

Player 1 chooses 2

   X | 4 | 7
   X | 5 | 8
   3 | 6 | O

Player 2: 3

   X | 4 | 7
   X | 5 | 8
   O | 6 | O

Player 1 chooses 6

   X | 4 | 7
   X | 5 | 8
   O | X | O

Player 2: 7

   X | 4 | O
   X | 5 | 8
   O | X | O

Player 1 chooses 5

   X | 4 | O
   X | X | 8
   O | X | O

Player 2: 8

   X | 4 | O
   X | X | O
   O | X | O

Player 2 wins!
```



## mIRC Scripting Language


```mirc
alias ttt {
  if ($2 isin %ttt) || (!%ttt) {
    var %ttt~ = $remove($iif(%ttt,%ttt,1 2 3 4 5 6 7 8 9),$2,X,O)
    var %ttt~~ = $replace($iif(%ttt,%ttt,1 2 3 4 5 6 7 8 9),$2,X)
    set %ttt $replace(%ttt~~,$iif(($regex(%ttt~~,/(?:O . . (?:(?:. O .|O) . . (\d)|(?:. (\d) .|(\d)) . . O)|(\d) . . (?:. O .|O) . . O|. . (?:O . (?:O . (\d)|(\d) . O)|(\d) . O . O) . .)/)) || ($regex(%ttt~~,/^(?:. . . )*(?:O (?:O (\d)|(\d) O)|(\d) O O)(?: . . .)*$/)),$regml(1),$iif(($regex(%ttt~~,/(?:X . . (?:(?:. X .|X) . . (\d)|(?:. (\d) .|(\d)) . . X)|(\d) . . (?:. X .|X) . . X|. . (?:X . (?:X . (\d)|(\d) . X)|(\d) . X . X) . .)/)) || ($regex(%ttt~~,/^(?:. . . )*(?:X (?:X (\d)|(\d) X)|(\d) X X)(?: . . .)*$/)),$regml(1),$iif($remove(%ttt~,2,4,6,8,$chr(32)),$iif((5 isin $remove(%ttt~,2,4,6,8)) && ($rand(0,$numtok($v2,32)) == 0),5,$gettok($remove(%ttt~,2,4,6,8),$rand(1,$numtok($remove(%ttt~,2,4,6,8),32)),32)),$gettok(%ttt~,$rand(1,$numtok(%ttt~,32)),32)))),O)
    tokenize 32 %ttt
    if ($regex(%ttt,/(?:X . . (?:X|. X .) . . X|. . X . X . X . .)/)) || ($regex(%ttt,/^(?:. . . )*X X X(?: . . .)*$/)) {
      echo -ag $me Wins
      tokenize 32 %ttt~~
      unset %ttt
    }
    elseif ($regex(%ttt,/(?:O . . (?:O|. O .) . . O|. . O . O . O . .)/)) || ($regex(%ttt,/^(?:. . . )*O O O(?: . . .)*$/)) {
      echo -ag $me Loses
      unset %ttt
    }
    elseif (!$regex(%ttt,/\d/)) {
      echo -ag Draw
      unset %ttt
    }
    echo -ag � $+ $iif($1 isnum,$chr(32),$1) $+ $chr(124) $+ $iif($2 isnum,$chr(32),$2) $+ $chr(124) $+ $iif($3 isnum, ,$3)
    echo -ag � $+ $iif($4 isnum,$chr(32),$4) $+ $chr(124) $+ $iif($5 isnum,$chr(32),$5) $+ $chr(124) $+ $iif($6 isnum, ,$6)
    echo -ag � $+ $iif($7 isnum,$chr(32),$7) $+ $chr(124) $+ $iif($8 isnum,$chr(32),$8) $+ $chr(124) $+ $iif($9 isnum, ,$9)
  }
  else {
    echo -ag Place Taken
    tokenize 32 %ttt
    echo -ag � $+ $iif($1 isnum,$chr(32),$1) $+ $chr(124) $+ $iif($2 isnum,$chr(32),$2) $+ $chr(124) $+ $iif($3 isnum, ,$3)
    echo -ag � $+ $iif($4 isnum,$chr(32),$4) $+ $chr(124) $+ $iif($5 isnum,$chr(32),$5) $+ $chr(124) $+ $iif($6 isnum, ,$6)
    echo -ag � $+ $iif($7 isnum,$chr(32),$7) $+ $chr(124) $+ $iif($8 isnum,$chr(32),$8) $+ $chr(124) $+ $iif($9 isnum, ,$9)
  }
}
```



## Objeck

Tic-tac-toe game using Minimax algorithm.

```objeck
class TicTacToe {
  @board : Char[,];
  @cpu_opening : Bool;

  enum Status {
    INVALID_MOVE,
    PLAYING,
    QUIT,
    TIE,
    CPU_WIN,
    PLAYER_WIN
  }

  consts Weights {
    MIN := -1000,
    MAX := 1000
  }

  function : Main(args : String[]) ~ Nil {
    cpu_score := 0;
    player_score := 0;

    for(i :=0; i < 5; i += 1;) {
      game := TicTacToe->New();
      result := game->Play();

      if(result = Status->PLAYER_WIN) {
        player_score += 1;
        "\n=> Player Wins!"->PrintLine();
      }
      else if(result = Status->CPU_WIN) {
        cpu_score += 1;
        "\n=> CPU Wins!"->PrintLine();
      }
      else if(result = Status->TIE) {
        "\n=> Tie."->PrintLine();
      }
      else {
        break;
      };
    };

    "\nHuman={$player_score}, CPU={$cpu_score}"->PrintLine();
  }

  New() {
    @board := Char->New[3, 3];
    for(index := 0; index < 9; index += 1;) {
      j := index / 3;
      i := index % 3;
      @board[i, j] := '1' + index;
    };

    @cpu_opening := true;
  }

  method : Play() ~ Status {
    players_turn := Int->Random(1) = 1 ? true : false;

    if(players_turn) {
      @cpu_opening := false;
      "\n*** NEW (Player) ***\n"->PrintLine();
      Draw();
    }
    else {
      "\n*** NEW (CPU) ***\n"->PrintLine();
    };

    playing := true;
    do {
      status : Status;

      if(players_turn) {
        status := PlayerMove();
        players_turn := false;
      }
      else {
        status := CpuMove();
        players_turn := true;
      };

      if(players_turn) {
        Draw();
      };

      select(status) {
        label Status->INVALID_MOVE: {
          "\n=> Invalid Move"->PrintLine();
        }

        label Status->PLAYER_WIN: {
          return Status->PLAYER_WIN;
        }

        label Status->CPU_WIN: {
          return Status->CPU_WIN;
        }

        label Status->TIE: {
          return Status->TIE;
        }

        label Status->QUIT: {
          playing := false;
        }
      };
    }
    while(playing);

    return Status->QUIT;
  }

  method : PlayerMove() ~ Status {
    move := System.IO.Console->ReadString();
    if(move->Size() = 0) {
      return Status->INVALID_MOVE;
    };

    option := move->Get(0);
    if(option = 'q') {
      return Status->QUIT;
    };

    if(LegalMove(option, 'X')) {
      if(IsWinner(@board, 'X')) {
        return Status->PLAYER_WIN;
      }
      else if(IsTied()) {
        return Status->TIE;
      }
      else {
        return Status->PLAYING;
      };
    }
    else {
      return Status->INVALID_MOVE;
    };
  }

  method : CpuMove() ~ Status {
    if(@cpu_opening) {
      select(Int->Random(2)) {
        label 0: {
          @board[0, 0] := 'O';
        }

        label 1: {
          @board[1, 1] := 'O';
        }

        label 2: {
          @board[2, 2] := 'O';
        }
      };
      @cpu_opening := false;
    }
    else {
      BestCpuMove(CopyBoard());
    };

    if(IsWinner(@board, 'O')) {
      return Status->CPU_WIN;
    }
    else if(IsTied()) {
      return Status->TIE;
    }
    else {
      return Status->PLAYING;
    };
  }

  method : Minimax(board : Char[,], depth : Int, is_max : Bool, alpha : Int, beta : Int) ~ Int {
      score := EvaluateMove(board);
      if(score = 10 | score = -10) {
          return score;
      };

      if(IsTied()) {
        return 0;
      };

    if(is_max) {
          best := Weights->MIN;
          for(i := 0; i < 3; i += 1;) {
        for(j := 0; j < 3; j += 1;) {
          if(board[i,j] <> 'X' & board[i,j] <>'O') {
            test := board[i,j];
            board[i,j] := 'O';
            best := Int->Max(best, Minimax(board, depth + 1, false, alpha, beta));
            alpha := Int->Max(alpha, best);
            board[i,j] := test;

            if(beta <= alpha) {
              return best;
            };
          };
        };
      };

      return best;
    }
    else {
      best := Weights->MAX;
          for(i := 0; i < 3; i += 1;) {
        for(j := 0; j < 3; j += 1;) {
          if(board[i,j] <> 'X' & board[i,j] <>'O') {
            test := board[i,j];
            board[i,j] := 'X';
            best := Int->Min(best, Minimax(board, depth + 1, true, alpha, beta));
            beta := Int->Min(beta, best);
            board[i,j] := test;

            if(beta <= alpha) {
              return best;
            };
          };
        };
      };

      return best;
    };
    }

    method : BestCpuMove(board : Char[,]) ~ Nil {
      best := Weights->MIN; # empty
      best_i := -1;
      best_j := -1;

      for(i := 0; i < 3; i += 1;) {
      for(j := 0; j < 3; j += 1;) {
        if(board[i,j] <> 'X' & board[i,j] <> 'O') {
          test := board[i,j];
          board[i,j] := 'O';
          move := Int->Max(best, Minimax(board, 0, false, Weights->MIN, Weights->MAX));
          board[i,j] := test;

          if(move > best) {
            best_i := i;
              best_j := j;
            best := move;
          };
                };
      };
    };

    @board[best_i, best_j] := 'O';
    }

    method : EvaluateMove(board : Char[,]) ~ Int {
      if(IsWinner(board, 'O')) {
        return 10;
      }
      else if(IsWinner(board, 'X')) {
        return -10;
      }
      else {
        return 0;
      };
    }

    method : CopyBoard() ~ Char[,] {
      board := Char->New[3, 3];

      for(i := 0; i < 3; i += 1;) {
      for(j := 0; j < 3; j += 1;) {
        board[i,j] := @board[i,j];
      };
    };

    return board;
    }

  method : LegalMove(move : Char, player: Char) ~ Bool {
    if(move >= '1' & move <= '9') {
      index := (move - '1')->As(Int);
      j := index / 3; i := index % 3;

      if(@board[i, j] = 'X' | @board[i, j] = 'O') {
        return false;
      };

      @board[i, j] := player;
      return true;
    }
    else {
      return false;
    };
  }

  method : IsWinner(board : Char[,], player : Char) ~ Bool {
    # --- diagonal ---
    check := 0;
    for(i := 0; i < 3; i += 1;) {
      if(board[i, i] = player) {
        check += 1;
      };
    };

    if(check = 3) {
      return true;
    };

    check := 0;
    j := 2;
    for(i := 0; i < 3; i += 1;) {
      if(board[i, j] = player) {
        check += 1;
      };
      j -= 1;
    };

    if(check = 3) {
      return true;
    };

    # --- vertical ---
    for(i := 0; i < 3; i += 1;) {
      check := 0;
      for(j := 0; j < 3; j += 1;) {
        if(board[i, j] = player) {
          check += 1;
        };
      };

      if(check = 3) {
        return true;
      };
    };

    # --- horizontal ---
    for(j := 0; j < 3; j += 1;) {
      check := 0;
      for(i := 0; i < 3; i += 1;) {
        if(board[i, j] = player) {
          check += 1;
        };
      };

      if(check = 3) {
        return true;
      };
    };

    return false;
  }

  method : IsTied() ~ Bool {
    for(i := 0; i < 3; i += 1;) {
      for(j := 0; j < 3; j += 1;) {
        if(@board[i, j] <> 'X' & @board[i, j] <> 'O') {
          return false;
        };
      };
    };

    return true;
  }

  method : Draw() ~ Nil {
    a1 := @board[0, 0]; a2 := @board[1, 0]; a3 := @board[2, 0];
    b1 := @board[0, 1]; b2 := @board[1, 1]; b3 := @board[2, 1];
    c1 := @board[0, 2]; c2 := @board[1, 2]; c3 := @board[2, 2];

    "
### =====
"->PrintLine();
    " {$a1} | {$a2} | {$a3} "->PrintLine();
    "---|---|---"->PrintLine();
    " {$b1} | {$b2} | {$b3} "->PrintLine();
    "---|---|---"->PrintLine();
    " {$c1} | {$c2} | {$c3} "->PrintLine();
    "
### =====
\n"->PrintLine();
  }
}
```



## Perl

A basic negamax search (with caching) is done to find the best move.
If there are several equally good moves, one of them is selected randomly.

The computer player is not perfect, and so a human player can sometimes win.

This is not perl's fault, but mine; it ought to always be a tie, or a win for
the computer.  Anyone who can identify the mistake, is welcome to fix it.


```Perl
use warnings;
use strict;

my $initial = join ",", qw(abc def ghi);
my %reverse = qw(X O O X);

# In list context, returns best move,
# In scalar context, returns the score of best move.
my %cache;
sub best_move {
	my ($b, $me) = @_;
	if( exists $cache{$b,$me,wantarray} ) {
		return $cache{$b,$me,wantarray};
	} elsif( my $s = score( $b, $me ) ) {
		return $cache{$b,$me,wantarray} = (wantarray ? undef : $s);
	}
	my $him = $reverse{$me};
	my ($best, @best) = (-999);
	for my $m (moves($b)) {
		(my $with_m = $b) =~ s/$m/$me/ or die;
		# The || operator supplies scalar context to best_move(...)
		my $s = -(score($with_m, $him) || best_move($with_m, $him));
		if( $s > $best ) {
			($best, @best) = ($s, $m);
		} elsif( $s == $best ) {
			push @best, $m;
		}
	}
	$cache{$b,$me,wantarray} = wantarray ? $best[rand @best] : $best;
}

my $winner = q[([XOxo])(?:\1\1|...\1...\1|..\1..\1|....\1....\1)];
sub score {
	my ($b, $me) = @_;
	$b =~ m/$winner/o or return 0;
	return $1 eq $me ? +1 : -1;
}

sub moves {
	my ($b) = @_;
	$b =~ /([^xoXO,\n])/g;
}

sub print_board {
	my ($b) = @_;
	$b =~ s/\B/|/g;
	$b =~ s/,/\n-+-+-\n/g;
	print $b, "\n";
}

sub prompt {
	my ($b, $color) = @_;
	my @moves = moves($b);
	unless( @moves ) {
		return;
	}
	while( 1 ) {
		print "Place your $color on one of [@moves]: ";
		defined(my $m = <>) or return;
		chomp($m);
		return $m if grep $m eq $_, @moves;
	}
}

my @players = (
	{ whose => "your", name => "You",
	  verb => "You place", get_move => \&prompt },
	{ whose => "the computer's", name => "Computer",
	  verb => "The computer places", get_move => \&best_move },
);
my $whose_turn = int rand 2;

my $color = "X";
my $b = $initial;

while( 1 ) {
	my $p = $players[$whose_turn];
	print_board($b);
	print "It is $p->{whose} turn.\n";
	# The parens around $m supply list context to the right side
	# or the = operator, which causes sub best_move to return the
	# best move, rather than the score of the best move.
	my ( $m ) = $p->{get_move}->($b, $color);
	if( $m ) {
		print "$p->{verb} an $color at $m\n";
		$b =~ s/$m/$color/;
		my $s = score($b, $color) or next;
		print_board($b);
		print "$p->{name} ", $s > 0 ? "won!\n" : "lost!\n";
	} else {
		print "$p->{name} cannot move.\n";
	}
	print "Game over.\nNew Game...\n";
	($b, $color, $whose_turn) = ($initial, "X", int rand 2);
	redo;
} continue {
	$color = $reverse{$color};
	$whose_turn = !$whose_turn;
}

```


{{out}}

```txt
a|b|c
-+-+-
d|e|f
-+-+-
g|h|i
It is your turn.
Place your X on one of [a b c d e f g h i]: e
You place an X at e
a|b|c
-+-+-
d|X|f
-+-+-
g|h|i
It is the computer's turn.
The computer places an O at c
a|b|O
-+-+-
d|X|f
-+-+-
g|h|i
It is your turn.
Place your X on one of [a b d f g h i]: a
You place an X at a
X|b|O
-+-+-
d|X|f
-+-+-
g|h|i
It is the computer's turn.
The computer places an O at f
X|b|O
-+-+-
d|X|O
-+-+-
g|h|i
It is your turn.
Place your X on one of [b d g h i]: i
You place an X at i
X|b|O
-+-+-
d|X|O
-+-+-
g|h|X
You won!
Game over.
New Game...
a|b|c
-+-+-
d|e|f
-+-+-
g|h|i
It is your turn.
Place your X on one of [a b c d e f g h i]:

```



## Perl 6

{{works with|Rakudo|2018.03}}
The computer plays a random game.


```perl6
my @board = 1..9;
my @winning-positions = [0..2], [3..5], [6..8], [0,3,6], [1,4,7], [2,5,8],
	[0,4,8], [6,4,2];

sub get-winner() {
	for @winning-positions {
        return (@board[|$_][0], $_) if [eq] @board[|$_];
	}
}

sub free-indexes() {
	@board.keys.grep: { @board[$_] eq any(1..9) }
}

sub ai-move() {
	given free-indexes.pick {
		@board[$_] = 'o';
		say "I go at: { $_ + 1 }\n";
	}
}

sub print-board() {
    print "\e[2J";
    say @board.map({ "$^a | $^b | $^c" }).join("\n--+---+--\n"), "\n";
}

sub human-move() {
	my $pos = prompt "Choose one of { (free-indexes() »+» 1).join(",") }: ";
	if $pos eq any(free-indexes() »+» 1) {
		@board[$pos - 1] = 'x';
	} else {
		say "Sorry, you want to put your 'x' where?";
		human-move();
	}
}

for flat (&ai-move, &human-move) xx * {
	print-board;
    last if get-winner() or not free-indexes;
    .();
}

if get-winner() -> ($player, $across) {
	say "$player wins across [", ($across »+» 1).join(", "), "].";
} else {
	say "How boring, a draw!";
}
```



## Phix

AI copied from C. User goes first, as does loser. After a draw the start player alternates.

```Phix
sequence board = repeat(' ',9)  -- {' '/'X'/'O'}

constant wins = {{1,2,3},{4,5,6},{7,8,9},{1,4,7},{2,5,8},{3,6,9},{1,5,9},{3,5,7}}

function check_winner()
    for w=1 to length(wins) do
        integer {i,j,k} = wins[w],
                boardi = board[i]
        if boardi!=' ' and boardi=board[j] and boardi=board[k] then
            return boardi
        end if
    end for
    return 0
end function

procedure showboard()
    printf(1," %c | %c | %c\n---+---+---\n %c | %c | %c\n---+---+---\n %c | %c | %c\n",board)
end procedure

integer best_i
function test_move(integer val, integer depth)
integer score = check_winner()
integer best = -1, changed = 0
    if score!=0 then return iff(score=val?1:-1) end if
    for i=1 to 9 do
        if board[i]=' ' then
            {changed,board[i]} @= val
            score = -test_move('O'+'X'-val, depth + 1)
            board[i] = ' '
            if score>best then
                if depth=0 then
                    best_i = i;
                end if
                best = score;
            end if
        end if
    end for
    return iff(changed!=0?best:0)
end function

integer user = 1

function game()
integer key, k, win
    board = repeat(' ',9)

    printf(1,"Board postions are numbered so:\n1 2 3\n4 5 6\n7 8 9\n");
    printf(1,"You have O, I have X.\n\n");
    for n=1 to 9 do
        if(user) then
            printf(1,"your move: ");
            while 1 do
                key = wait_key()
                if find(key,{#1B,'q','Q'}) then return "Quit" end if
                k = key-'0'
                if k>=1 and k<=9 and board[k]=' ' then
                    board[k] = 'O'
                    printf(1,"%c\n",key)
                    exit
                end if
            end while
        else
            if n=1 then --/* randomize if computer opens, less boring */
                best_i = rand(9)
            else
                {} = test_move('X', 0);
            end if
            board[best_i] = 'X'
            printf(1," my move: %d\n", best_i);
        end if
        showboard();
        user = 1-user
        win = check_winner()
        if win!=0 then
            return iff(win=='O' ? "You win.\n\n" : "I win.\n\n");
        end if
    end for
    return "A draw.\n\n";
end function

while 1 do
    string res = game()
    puts(1,res)
    if res="Quit" then exit end if
end while
```

Sample game (after a draw), with the boards cut/pasted into the horizontal

```txt

Board postions are numbered so:
1 2 3
4 5 6
7 8 9
You have O, I have X.

 my move: 5     your move: 2     my move: 1     your move: 9     my move: 4     your move: 6     my move: 7
   |   |           | O |         X | O |         X | O |         X | O |         X | O |         X | O |
---+---+---     ---+---+---     ---+---+---     ---+---+---     ---+---+---     ---+---+---     ---+---+---
   | X |           | X |           | X |           | X |         X | X |         X | X | O       X | X | O
---+---+---     ---+---+---     ---+---+---     ---+---+---     ---+---+---     ---+---+---     ---+---+---
   |   |           |   |           |   |           |   | O         |   | O         |   | O       X |   | O
I win.

Board postions are numbered so:
1 2 3
4 5 6
7 8 9
You have O, I have X.

your move: Quit

```



## PicoLisp

This solution doesn't bother about the game logic, but simply uses the alpha-beta-pruning 'game' function in the "simul" library.

```PicoLisp
(load "@lib/simul.l")  # for 'game' function

(de display ()
   (for Y (3 2 1)
      (prinl "   +---+---+---+")
      (prin " " Y)
      (for X (1 2 3)
         (prin " | " (or (get *Board X Y) " ")) )
      (prinl " |") )
   (prinl "   +---+---+---+")
   (prinl "     a   b   c") )

(de find3 (P)
   (find
      '((X Y DX DY)
         (do 3
            (NIL (= P (get *Board X Y)))
            (inc 'X DX)
            (inc 'Y DY)
            T ) )
      (1 1 1 1 2 3 1 1)
      (1 2 3 1 1 1 1 3)
      (1 1 1 0 0 0 1 1)
      (0 0 0 1 1 1 1 -1) ) )

(de myMove ()
   (when
      (game NIL 8
         '((Flg)     # Moves
            (unless (find3 (or (not Flg) 0))
               (make
                  (for (X . L) *Board
                     (for (Y . P) L
                        (unless P
                           (link
                              (cons
                                 (cons X Y (or Flg 0))
                                 (list X Y) ) ) ) ) ) ) ) )
         '((Mov) # Move
            (set (nth *Board (car Mov) (cadr Mov)) (cddr Mov)) )
         '((Flg)     # Cost
            (if (find3 (or Flg 0)) -100 0) ) )
      (let Mov (caadr @)
         (set (nth *Board (car Mov) (cadr Mov)) 0) )
      (display) ) )

(de yourMove (X Y)
   (and
      (sym? X)
      (>= 3 (setq X (- (char X) 96)) 1)
      (num? Y)
      (>= 3 Y 1)
      (not (get *Board X Y))
      (set (nth *Board X Y) T)
      (display) ) )

(de main ()
   (setq *Board (make (do 3 (link (need 3)))))
   (display) )

(de go Args
   (cond
      ((not (yourMove (car Args) (cadr Args)))
         "Illegal move!" )
      ((find3 T) "Congratulation, you won!")
      ((not (myMove)) "No moves")
      ((find3 0) "Sorry, you lost!") ) )
```

{{out}}

```txt
: (main)
   +---+---+---+
 3 |   |   |   |
   +---+---+---+
 2 |   |   |   |
   +---+---+---+
 1 |   |   |   |
   +---+---+---+
     a   b   c

: (go a 1)
   +---+---+---+
 3 |   |   |   |
   +---+---+---+
 2 |   |   |   |
   +---+---+---+
 1 | T |   |   |
   +---+---+---+
     a   b   c
   +---+---+---+
 3 |   |   |   |
   +---+---+---+
 2 |   | 0 |   |
   +---+---+---+
 1 | T |   |   |
   +---+---+---+
     a   b   c
```



## Prolog

Works with SWI-Prolog.

Uses a minimax algorithm with no Alpha-beta pruning, as the max depth of the recursion is  8. Computer never loses.

A GUI interface written in XPCE is given.

```Prolog
:- use_module('min-max.pl').

:-dynamic box/2.
:- dynamic tic_tac_toe_window/1.

% Computer begins.
tic-tac-toe(computer) :-
	V is random(9),
	TTT = [_,_,_,_,_,_ ,_,_,_],
	nth0(V, TTT, o),
	display_tic_tac_toe(TTT).

% Player begins
tic-tac-toe(me) :-
	TTT = [_,_,_,_,_,_ ,_,_,_],
	display_tic_tac_toe(TTT).


display_tic_tac_toe(TTT) :-
	retractall(box(_,_)),
	retractall(tic_tac_toe_window(_)),
	new(D, window('Tic-tac-Toe')),
	send(D, size, size(170,170)),
	X = 10, Y = 10,
	display(D, X, Y, 0, TTT),
	assert(tic_tac_toe_window(D)),
	send(D, open).

display(_, _, _, _, []).

display(D, X, Y, N, [A,B,C|R]) :-
	display_line(D, X, Y, N, [A,B,C]),
	Y1 is Y+50,
	N3 is N+3,
	display(D, X, Y1, N3, R).


display_line(_, _, _, _, []).
display_line(D, X, Y, N, [C|R]) :-
	(   nonvar(C)-> C1 = C; C1 = ' '),
	new(B, tic_tac_toe_box(C1)),
	assertz(box(N, B)),
	send(D, display, B, point(X, Y)),
	X1 is X + 50,
	N1 is N+1,
	display_line(D, X1, Y, N1, R).



% class tic_tac_toe_box
% display an 'x' when the player clicks
% display an 'o' when the computer plays
:- pce_begin_class(tic_tac_toe_box, box, "Graphical window with text").

variable(mess, any, both, "text to display").

initialise(P, Lbl) :->
	send(P, send_super, initialise),
	send(P, slot, mess, Lbl),
	WS = 50, HS = 50,
	send(P, size, size(WS,HS)),
	send(P, recogniser,
	     handler_group(new(click_gesture(left,
					     '',
					     single,
					     message(@receiver, my_click))))).

% the box is clicked
my_click(B) :->
	send(B, set_val, x),
	send(@prolog, play).

% only works when the box is "free"
set_val(B, Val) :->
	get(B, slot, mess, ' '),
	send(B, slot, mess, Val),
	send(B, redraw),
	send(B, flush).


%  redefined method to display custom graphical objects.
'_redraw_area'(P, A:area) :->
	send(P, send_super, '_redraw_area', A),
	%we display the text
	get(P, slot, mess, Lbl),
	new(Str1, string(Lbl)),
	get_object(P, area, area(X,Y,W,H)),
	send(P, draw_box, X, Y, W, H),
	send(P, draw_text, Str1,
		font(times, normal, 30),
		X, Y, W, H, center, center).

:- pce_end_class.

play :-
	numlist(0, 8, L),
	maplist(init, L, TTT),
	finished(x, TTT, Val),
	(   Val = 2 -> send(@display, inform,'You win !'),
	               tic_tac_toe_window(D),
		       send(D, destroy)
	;   (	Val = 1 -> send(@display, inform,'Draw !'),
	                   tic_tac_toe_window(D),
		           send(D, destroy)
	    ;	next_move(TTT, TT1),
		maplist(display, L, TT1),
		finished(o, TT1, V),
		(   V = 2 -> send(@display, inform,'I win !'),
			     tic_tac_toe_window(D),
		             send(D, destroy)
		;   (	V = 1 -> send(@display, inform,'Draw !'),
		                 tic_tac_toe_window(D),
			         send(D, destroy)
		    ;	true)))).


% use minmax to compute the next move
next_move(TTT, TT1) :-
	minimax(o, 0, 1024, TTT, _V1- TT1).


% we display the new board
display(I, V) :-
	nonvar(V),
	box(I, V1),
	send(V1, set_val, V).

display(_I, _V).

% we create the board for minmax
init(I, V) :-
	box(I, V1),
	get(V1, slot, mess, V),
	V \= ' '.

init(_I, _V).

% winning position for the player P ?
winned(P, [A1, A2, A3, A4, A5, A6, A7, A8, A9]) :-
       (is_winning_line(P, [A1, A2, A3]);
	is_winning_line(P, [A4, A5, A6]);
	is_winning_line(P, [A7, A8, A9]);
	is_winning_line(P, [A1, A4, A7]);
	is_winning_line(P, [A2 ,A5, A8]);
	is_winning_line(P, [A3, A6, A9]);
	is_winning_line(P, [A1, A5, A9]);
	is_winning_line(P, [A3, A5, A7])).


is_winning_line(P, [A, B, C]) :-
	nonvar(A), A = P,
	nonvar(B), B = P,
	nonvar(C), C = P.

% Winning position for the player
eval(Player, Deep, TTT, V) :-
	winned(Player, TTT),
	(   Player = o -> V is 1000 - 50 * Deep; V is -1000+ 50 * Deep).

% Loosing position for the player
eval(Player, Deep, TTT, V) :-
	select(Player, [o,x], [Player1]),
	winned(Player1, TTT),
	(   Player = x -> V is 1000 - 50 * Deep; V is -1000+ 50 * Deep).

% Draw position
eval(_Player, _Deep, TTT, 0) :-
	include(var, TTT, []).


% we fetch the free positions of the board
possible_move(TTT, LMove) :-
	new(C, chain),
	forall(between(0,8, I),
	       (   nth0(I, TTT, X),
		   (   var(X) -> send(C, append, I); true))),
	chain_list(C, LMove).

% we create the new position when the player P clicks
% the box "N"
assign_move(P, TTT, N, TT1) :-
	copy_term(TTT, TT1),
	nth0(N, TT1, P).

% We fetch all the possible boards obtained from board TTT
% for the player P
get_next(Player, Deep, TTT, Player1, Deep1, L):-
	possible_move(TTT, LMove),
	select(Player, [o,x], [Player1]),
	Deep1 is Deep + 1,
	maplist(assign_move(Player, TTT), LMove, L).


% The game is over ?
% Player P wins
finished(P, TTT, 2) :-
	winned(P, TTT).

% Draw
finished(_P, TTT, 1) :-
	include(var, TTT, []).

% the game is not over
finished(_P, _TTT, 0) .

% minmax must knows when the computer plays
% (o for ordinateur in French)
computer(o).


```

Module min-max.pl defines minimax algorithm.

```prolog
:- module('min-max.pl', [minimax/5]).

% minimax(Player, Deep, MaxDeep, B, V-B)
% @arg1 : current player at this level
% @arg2 : current level of recursion
% @arg3 : max level of recursion (in this version of the game no use : set to 1024 !)
% @arg4 : current board
% @arg5 : B is the evaluation of the board, the result is V-B to know the new board

% Here we get an evaluation
minimax(Player, Deep, MaxDeep, B, V-B) :-
	(   eval(Player, Deep, B, V) -> true
	; % in this version of the game this second division always fails
	(   Deep > MaxDeep -> V is random(1000) - 1000)).

% here we must compute all the possible moves to know the evaluation of the board
minimax(Player, Deep, MaxDeep, B, V) :-
	get_next(Player, Deep, B, Player1, Deep1, L),
	maplist(minimax(Player1, Deep1, MaxDeep), L, LV),
	maplist(lie, L, LV, TLV),
	sort(TLV, SLVTmp),
	(   computer(Player) -> reverse(SLVTmp, SLV); SLV = SLVTmp),
	SLV = [V | _R].


lie(TTT, V-_, V-TTT).


```



## Python

The computer enforces the rules but plays a random game.

```python

'''
    Tic-tac-toe game player.
    Input the index of where you wish to place your mark at your turn.
'''

import random

board = list('123456789')
wins = ((0,1,2), (3,4,5), (6,7,8),
        (0,3,6), (1,4,7), (2,5,8),
        (0,4,8), (2,4,6))

def printboard():
    print('\n'.join(' '.join(board[x:x+3]) for x in(0,3,6)))

def score():
    for w in wins:
        b = board[w[0]]
        if b in 'XO' and all (board[i] == b for i in w):
            return b, [i+1 for i in w]
    return None, None

def finished():
    return all (b in 'XO' for b in board)

def space():
    return [ b for b in board if b not in 'XO']

def my_turn(xo):
    options = space()
    choice = random.choice(options)
    board[int(choice)-1] = xo
    return choice

def your_turn(xo):
    options = space()
    while True:
        choice = input(" Put your %s in any of these positions: %s "
                       % (xo, ''.join(options))).strip()
        if choice in options:
            break
        print( "Whoops I don't understand the input" )
    board[int(choice)-1] = xo
    return choice

def me(xo='X'):
    printboard()
    print('I go at', my_turn(xo))
    return score()
    assert not s[0], "\n%s wins across %s" % s

def you(xo='O'):
    printboard()
    # Call my_turn(xo) below for it to play itself
    print('You went at', your_turn(xo))
    return score()
    assert not s[0], "\n%s wins across %s" % s


print(__doc__)
while not finished():
    s = me('X')
    if s[0]:
        printboard()
        print("\n%s wins across %s" % s)
        break
    if not finished():
        s = you('O')
        if s[0]:
            printboard()
            print("\n%s wins across %s" % s)
            break
else:
    print('\nA draw')

```


'''Sample Game'''

```txt

    Tic-tac-toe game player.
    Input the index of where you wish to place your mark at your turn.

1 2 3
4 5 6
7 8 9
I go at 9
1 2 3
4 5 6
7 8 X
 Put your O in any of these positions: 12345678 1
You went at 1
O 2 3
4 5 6
7 8 X
I go at 3
O 2 X
4 5 6
7 8 X
 Put your O in any of these positions: 245678 4
You went at 4
O 2 X
O 5 6
7 8 X
I go at 2
O X X
O 5 6
7 8 X
 Put your O in any of these positions: 5678 7
You went at 7
O X X
O 5 6
O 8 X

O wins across [1, 4, 7]
```



### Better skilled player

In this version, The computer player will first complete a winning line of its own if it can, otherwise block a winning line of its opponent if they have two in a row, or then choose a random move.


```python

'''
    Tic-tac-toe game player.
    Input the index of where you wish to place your mark at your turn.
'''

import random

board = list('123456789')
wins = ((0,1,2), (3,4,5), (6,7,8),
        (0,3,6), (1,4,7), (2,5,8),
        (0,4,8), (2,4,6))

def printboard():
    print('\n-+-+-\n'.join('|'.join(board[x:x+3]) for x in(0,3,6)))

def score(board=board):
    for w in wins:
        b = board[w[0]]
        if b in 'XO' and all (board[i] == b for i in w):
            return b, [i+1 for i in w]
    return None

def finished():
    return all (b in 'XO' for b in board)

def space(board=board):
    return [ b for b in board if b not in 'XO']

def my_turn(xo, board):
    options = space()
    choice = random.choice(options)
    board[int(choice)-1] = xo
    return choice

def my_better_turn(xo, board):
    'Will return a next winning move or block your winning move if possible'
    ox = 'O' if xo =='X' else 'X'
    oneblock = None
    options  = [int(s)-1 for s in space(board)]
    for choice in options:
        brd = board[:]
        brd[choice] = xo
        if score(brd):
            break
        if oneblock is None:
            brd[choice] = ox
            if score(brd):
                oneblock = choice
    else:
        choice = oneblock if oneblock is not None else random.choice(options)
    board[choice] = xo
    return choice+1

def your_turn(xo, board):
    options = space()
    while True:
        choice = input("\nPut your %s in any of these positions: %s "
                       % (xo, ''.join(options))).strip()
        if choice in options:
            break
        print( "Whoops I don't understand the input" )
    board[int(choice)-1] = xo
    return choice

def me(xo='X'):
    printboard()
    print('\nI go at', my_better_turn(xo, board))
    return score()

def you(xo='O'):
    printboard()
    # Call my_turn(xo, board) below for it to play itself
    print('\nYou went at', your_turn(xo, board))
    return score()


print(__doc__)
while not finished():
    s = me('X')
    if s:
        printboard()
        print("\n%s wins along %s" % s)
        break
    if not finished():
        s = you('O')
        if s:
            printboard()
            print("\n%s wins along %s" % s)
            break
else:
    print('\nA draw')
```


{{out}}

```txt

    Tic-tac-toe game player.
    Input the index of where you wish to place your mark at your turn.

1|2|3
-+-+-
4|5|6
-+-+-
7|8|9

I go at 2
1|X|3
-+-+-
4|5|6
-+-+-
7|8|9

Put your O in any of these positions: 13456789 5

You went at 5
1|X|3
-+-+-
4|O|6
-+-+-
7|8|9

I go at 1
X|X|3
-+-+-
4|O|6
-+-+-
7|8|9

Put your O in any of these positions: 346789 3

You went at 3
X|X|O
-+-+-
4|O|6
-+-+-
7|8|9

I go at 7
X|X|O
-+-+-
4|O|6
-+-+-
X|8|9

Put your O in any of these positions: 4689 4

You went at 4
X|X|O
-+-+-
O|O|6
-+-+-
X|8|9

I go at 6
X|X|O
-+-+-
O|O|X
-+-+-
X|8|9

Put your O in any of these positions: 89 9

You went at 9
X|X|O
-+-+-
O|O|X
-+-+-
X|8|O

I go at 8

A draw
```



## Racket


The program provides standard interface for implementation of any zero–sum game with perfect information such as tick-tack-toe, Nim, the 21 game etc. It is possible to create interactive players (as objects) with different playing strategy (AI-driven, user-driven, random etc.) and let them play with each other through message-sending technique.

The optimal strategy is implemented via lazy minimax algorythm with α-β-pruning and arbitrary depth of the recursion.

The program consists of separate modules:
 + minimax.rkt    -- Written in Lazy Racket, implements the general minimax algorythm as
 |                   given in [http://en.wikipedia.org/wiki/Alpha-beta_pruning Wikipedia].
 |                   Knows nothing about games.
 V
 + game.rkt       -- Written in Lazy Racket, defines general classes for the game and players.
 |                   Knows nothing about tick-tack-toe, only about zero-sum two-player
 |                   turn-taking games with perfect information in general.
 V
 + tick-tack.rkt  -- Written in Racket, implements the tick-tack-toe game.


The <tt>minimax.rkt</tt> module:

```racket

#lang lazy
(provide minimax)

(define (minimax tree)
  (! (let minimax ([node tree] [α -inf.0] [β +inf.0] [max-player #f])
       (cond
         [(number? node) node]
         [(empty? node) 0.0]
         [max-player
          (let next ([x node] [α α])
            (if (or (empty? x) (<= β α))
                α
                (next (cdr x)
                      (max α (minimax (car x) α β (not max-player))))))]
         [else
          (let next ([x node] [β β])
            (if (or (empty? x) (<= β α))
                β
                (next (cdr x)
                      (min β (minimax (car x) α β (not max-player))))))]))))

```


The <tt>game.rkt</tt> module:


```racket

#lang lazy
(require racket/class
         "minimax.rkt"
         (only-in racket/list shuffle argmax))

(provide game%
         interactive-player
         define-partners)

;;--------------------------------------------------------------------
;; Class representing the logics and optimal strategy
;; for a zero-sum game with perfect information.
(define game%
  (class object%
    (super-new)

    ;; virtual methods which set up the game rules
    (init-field my-win?         ; State -> Bool
                my-loss?        ; State -> Bool
                draw-game?      ; State -> Bool
                my-move         ; State Move -> State
                opponent-move   ; State Move -> State
                possible-moves  ; State -> (list Move)
                show-state)     ; State -> Any

    ;; optimal-move :: State -> Move
    ;; Choses the optimal move.
    ;; If several equivalent moves exist -- choses one randomly.
    (define/public ((optimal-move look-ahead) S)
      (! (argmax (λ (m) (! (minimax (game-tree S m look-ahead))))
                 (shuffle (possible-moves S)))))

    ;; game-tree :: State -> (Move -> (Treeof Real))
    (define (game-tree S m look-ahead)
      (let new-ply ([moves (cycle opponent-move my-move)]
                    [i 1]
                    [s (my-move S m)])
        (cond
          [(my-win? s)        (/  1 i)] ; more close wins and loses
          [(my-loss? s) (/ -1 i)] ; have bigger weights
          [(draw-game? s)     0]
          [(>= i look-ahead)  (/ 1 i)]
          [else (map (λ (x) (new-ply (cdr moves) (+ 1 i) ((car moves) s x)))
                     (possible-moves s))])))

    ;; make-move :: State (State -> Move) -> (Move State Symbol)
    (define/public (make-move S move)
      (cond
        [(my-loss? S)   (values '() S 'loss)]
        [(draw-game? S) (values '() S 'draw)]
        [else (let* ([m* (! (move S))]
                     [S* (my-move S m*)])
                (cond
                  [(my-win? S*)    (values m* S* 'win)]
                  [(draw-game? S*) (values m* S* 'draw)]
                  [else            (values m* S* 'next)]))]))))

;;--------------------------------------------------------------------
;; Mixin representing an interactive game player.
;; The parameter `game` defines a game which is played.
(define (interactive-player game)
  (class game
    (super-new)

    (inherit-field show-state)
    (inherit make-move optimal-move)

    (init-field name
                [look-ahead 4]
                [opponent 'undefined]
                [move-method (optimal-move look-ahead)])

    (define/public (your-turn S)
      (define-values (m S* status) (make-move S move-method))
      (! (printf "\n~a makes move ~a\n" name m))
      (! (show-state S*))
      (! (case status
           ['stop (displayln "The game was interrupted.")]
           ['win  (printf "~a wins!" name)]
           ['loss (printf "~a wins!" name)]
           ['draw (printf "Draw!")]
           [else (send opponent your-turn S*)])))))


;;--------------------------------------------------------------------
;; a simple macro for initialization of game partners
(define-syntax-rule
  (define-partners game (A #:win A-wins #:move A-move)
                        (B #:win B-wins #:move B-move))
  (begin
    (define A (class game
                (super-new
                 [my-win?  A-wins]
                 [my-loss? B-wins]
                 [my-move  A-move]
                 [opponent-move B-move])))
    (define B (class game
                (super-new
                 [my-win?  B-wins]
                 [my-loss? A-wins]
                 [my-move  B-move]
                 [opponent-move A-move])))))

;;--------------------------------------------------------------------
;; the main procedure which initiates the game
(define (start-game p1 p2 initial-state)
  (set-field! opponent p1 p2)
  (set-field! opponent p2 p1)
  (send p1 your-turn initial-state))

```


The <tt>tick-tack.rkt</tt> module:

```racket
#lang racket

(require "game.rkt"
         racket/set
         lazy/force)

;;--------------------------------------------------------------------
;; Tick-tack-toe game implementation

;; the structure representing a board
(struct board (x o))

;; sets of X's and O's
(define xs board-x)
(define os board-o)

(define empty-board (board (set) (set)))

(define all-cells
  (set '(1 1) '(1 2) '(1 3)
       '(2 1) '(2 2) '(2 3)
       '(3 1) '(3 2) '(3 3)))

(define (free-cells b)
  (set-subtract all-cells (xs b) (os b)))

(define winning-positions
  (list (set '(1 1) '(2 2) '(3 3))
        (set '(1 3) '(2 2) '(3 1))
        (set '(1 1) '(1 2) '(1 3))
        (set '(2 1) '(2 2) '(2 3))
        (set '(3 1) '(3 2) '(3 3))
        (set '(1 1) '(2 1) '(3 1))
        (set '(1 2) '(2 2) '(3 2))
        (set '(1 3) '(2 3) '(3 3))))

;; a predicate for winning state on the board
(define ((wins? s) b)
  (ormap (curryr subset? (s b)) winning-positions))

;; player moves
(define (x-move b m)  (board (set-add (xs b) m) (os b)))
(define (o-move b m)  (board (xs b) (set-add (os b) m)))

;; textual representation of the board
(define (show-board b)
  (for ([i '(3 2 1)])
    (printf "~a " i)
    (for ([j '(1 2 3)])
      (display (cond
                 [(set-member? (os b) (list j i)) "|o"]
                 [(set-member? (xs b) (list j i)) "|x"]
                 [else "| "])))
    (display "|\n"))
  (display "   1 2 3    "))

;;--------------------------------------------------------------------
;; The definition of the game
;; general properties
(define tic-tac%
  (class game%
    (super-new
     [draw-game?       (compose set-empty? free-cells)]
     [possible-moves   (compose set->list free-cells)]
     [show-state       show-board])))

;; players
(define-partners tic-tac%
  (x% #:win (wins? xs) #:move x-move)
  (o% #:win (wins? os) #:move o-move))

;; Computer players
(define player-A (new (interactive-player x%) [name "A"] [look-ahead 6]))

(define player-B (new (interactive-player o%) [name "B"] [look-ahead 6]))

; The interactive user
(define User
  (new (interactive-player x%)
       [name "User"]
       [move-method
        (λ (b) (let make-move ([m (read)])
                 (match m
                   ['q (exit)]
                   [(list (or 1 2 3) (or 1 2 3)) m]
                   [else (make-move (read))])))]))

;; The dummy player plays randomly
(define Dummy
  (new (interactive-player o%) [name "Dummy"] [look-ahead 0]))


```


Sample games:

Computer plays with the computer:

```txt

> (!(start-game player-A player-B empty-board))

A makes move (3 1)
3 | | | |
2 | | | |
1 | | |x|
   1 2 3
B makes move (2 2)
3 | | | |
2 | |o| |
1 | | |x|
   1 2 3
A makes move (1 1)
3 | | | |
2 | |o| |
1 |x| |x|
   1 2 3
B makes move (2 1)
3 | | | |
2 | |o| |
1 |x|o|x|
   1 2 3
A makes move (2 3)
3 | |x| |
2 | |o| |
1 |x|o|x|
   1 2 3
B makes move (3 2)
3 | |x| |
2 | |o|o|
1 |x|o|x|
   1 2 3
A makes move (1 2)
3 | |x| |
2 |x|o|o|
1 |x|o|x|
   1 2 3
B makes move (1 3)
3 |o|x| |
2 |x|o|o|
1 |x|o|x|
   1 2 3
A makes move (3 3)
3 |o|x|x|
2 |x|o|o|
1 |x|o|x|
   1 2 3    Draw!

```


Computer plays with the dummy:

```txt

> (!(start-game player-A Dummy empty-board))

A makes move (3 1)
3 | | | |
2 | | | |
1 | | |x|
   1 2 3
Dummy makes move (2 3)
3 | |o| |
2 | | | |
1 | | |x|
   1 2 3
A makes move (1 1)
3 | |o| |
2 | | | |
1 |x| |x|
   1 2 3
Dummy makes move (3 3)
3 | |o|o|
2 | | | |
1 |x| |x|
   1 2 3
A makes move (2 1)
3 | |o|o|
2 | | | |
1 |x|x|x|
   1 2 3    A wins!

```


User plays with the dummy:

```txt

> (!(start-game Dummy User empty-board))

Dummy makes move (2 3)
3 | |o| |
2 | | | |
1 | | | |
   1 2 3    (1 2)

User makes move (1 2)
3 | |o| |
2 |x| | |
1 | | | |
   1 2 3
Dummy makes move (3 2)
3 | |o| |
2 |x| |o|
1 | | | |
   1 2 3    (1 3)

User makes move (1 3)
3 |x|o| |
2 |x| |o|
1 | | | |
   1 2 3
Dummy makes move (3 3)
3 |x|o|o|
2 |x| |o|
1 | | | |
   1 2 3    (1 1)

User makes move (1 1)
3 |x|o|o|
2 |x| |o|
1 |x| | |
   1 2 3    User wins!

```


As an example of another zero-sum game consider the classical [http://en.wikipedia.org/wiki/Nim Nim] game:


```racket

#lang racket

(require "game.rkt"
         lazy/force)

;;--------------------------------------------------------------------
;; The definition of the game

(define initial-state '(3 5 7))

(define (move s m) (map - s m))

(define (win? s) (= 1 (apply + s)))

(define (show-state s) (displayln (map (λ (n) (make-list n '●)) s)))

(define (possible-moves S)
  (append-map
   (λ (heap n)
     (map (λ (x) (map (curry * x) heap))
          (range 1 (+ 1 (min 3 n)))))
   '((1 0 0) (0 1 0) (0 0 1)) S))

(define Nim% (class game%
               (super-new
                [draw-game?       (const #f)]
                [possible-moves   possible-moves]
                [show-state       show-state])))

(define-partners Nim%
  (first%  #:win win? #:move move)
  (second% #:win win? #:move move))

;; players
(define player-A
  (new (interactive-player first%) [name "A"] [look-ahead 4]))

(define player-B
  (new (interactive-player second%) [name "B"] [look-ahead 4]))

```


Computer plays with the computer:

```txt

> (!(start-game player-A player-B initial-state))

A makes move (0 0 2)
((● ● ●) (● ● ● ● ●) (● ● ● ● ●))

B makes move (1 0 0)
((● ●) (● ● ● ● ●) (● ● ● ● ●))

A makes move (2 0 0)
(() (● ● ● ● ●) (● ● ● ● ●))

B makes move (0 2 0)
(() (● ● ●) (● ● ● ● ●))

A makes move (0 3 0)
(() () (● ● ● ● ●))

B makes move (0 0 1)
(() () (● ● ● ●))

A makes move (0 0 3)
(() () (●))
A wins!

```


With use of memoization it is easy to train automatic players so that they would never lose and play very fast.


## REXX

This REXX program uses an analytical solution instead of hard─fast choices that can be
assumed for a    3&times;3   game board.

Options used within the REXX program:
::*   a separate numbered grid is used instead of coördinates for easier marker specification
::*   the game board is separated from the numbered grid, this makes it much easier to see the playing field
::*   straight lines (wins) are handled dynamically instead of hard─coding them
::*   allows the human player to:
::::*   specify any   '''N'''&times;'''N'''   size tic─tac─toe board   (a square grid)
::::*   specify who plays first   (default is the human)
::::*   specify what markers (symbols) to be used for both players   (can use hexadecimal pairs)
::::*   quit (exit) the game at any time
::::*   win   (if the human goes first   ''and''   makes a certain move)


A fair amount of code was dedicated to error detection and the
presentation of the tic─tac─toe game boards (grids).

```rexx
/*REXX program plays  (with a human)  the   tic─tac─toe   game  on an  NxN  grid.       */
$=copies('─', 9)                                 /*eyecatcher for error messages, prompt*/
oops   = $  '***error*** '                       /*literal for when an error happens.   */
single = '│─┼';    jam= "║";    bar= '═';     junc= "╬";         dbl=jam || bar || junc
sw     = linesize() - 1                          /*obtain width of the terminal (less 1)*/
parse arg N hm cm .,@.                           /*obtain optional arguments from the CL*/
if N=='' | N==","  then N=3;   oN=N              /*N  not specified?   Then use default.*/
     N = abs(N)                                  /*if N < 0.  then computer goes first. */
    NN = N*N                                     /*calculate the   square of  N.        */
middle = NN % 2   +   N % 2                      /*    "      "    middle  "  the grid. */
if N<2  then do;  say oops  'tic─tac─toe grid is too small: '    N;     exit 13;    end
pad=left('', sw % NN)                            /*display padding:  6x6  in 80 columns.*/
if hm==''  then hm= "X";                         /*define the marker for  a   human.    */
if cm==''  then cm= "O"                          /*   "    "     "    "  the  computer. */
                hm= aChar(hm, 'human')           /*determine if the marker is legitimate*/
                cm= aChar(cm, 'computer')        /*    "      "  "     "    "      "    */
parse upper value  hm  cm     with     uh  uc    /*use uppercase values is markers:  X x*/
if uh==uc  then cm=word('O X', 1 + (uh=="O") )   /*The human wants Hal's marker?  Swap. */
if oN<0  then call Hmove middle                  /*Hal moves first? Then choose middling*/
         else call showGrid                      /*showGrid also checks for wins & draws*/

/*tic─tac─toe game───►*/     do  forever         /*'til the cows come home  (or  QUIT). */
/*tic─tac─toe game───►*/     call CBLF           /*process carbon─based lifeform's move.*/
/*tic─tac─toe game───►*/     call Hal            /*determine Hal's  (the computer) move.*/
/*tic─tac─toe game───►*/     end   /*forever*/   /*showGrid subroutine does wins & draws*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
aChar: parse arg x,whoseX;  L=length(x)                               /*process markers.*/
       if L==1                        then return testB(     x  )     /*1 char,  as is. */
       if L==2 & datatype(x, 'X')     then return testB( x2c(x) )     /*2 chars, hex.   */
       if L==3 & datatype(x, 'W') & ,                                 /*3 chars, decimal*/
          x>=0 & x<256                then return testB( d2c(x) )     /*···and in range.*/
       say oops  'illegal character or character code for'    whoseX    "marker: "    x
       exit 13                                   /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
CBLF:  prompt='Please enter a cell number to place your next marker ['hm"]     (or Quit):"

         do forever;                 say $ prompt
         parse pull  x 1 ux 1 ox;    upper ux    /*get versions of answer;  uppercase ux*/
         if datatype(ox, 'W')  then ox=ox / 1    /*normalize cell number:  +0007 ───► 7 */
                                                 /*(division by unity normalizes a num.)*/
           select                                /*perform some validations of X (cell#)*/
           when abbrev('QUIT',ux,1)  then call tell 'quitting.'
           when x=''                 then iterate                    /*Nada?  Try again.*/
           when words(x)\==1         then say oops "too many" cell#  'specified:'   x
           when \datatype(x, 'N')    then say oops "cell number isn't numeric: "        x
           when \datatype(x, 'W')    then say oops "cell number isn't an integer: "     x
           when x=0                  then say oops "cell number can't be zero: "        x
           when x<0                  then say oops "cell number can't be negative: "    x
           when x>NN                 then say oops "cell number can't exceed "          NN
           when @.ox\==''            then say oops "cell number is already occupied: "  x
           otherwise                 leave  /*forever*/
           end   /*select*/

         end     /*forever*/
                                                 /* [↓]  OX is a normalized version of X*/
       @.ox=hm                                   /*place a marker for the human (CLBF). */
       call showGrid                             /*and display the  tic─tac─toe  grid.  */
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
Hal:           select                                        /*Hal tries various moves. */
               when win(cm, N-1)   then call Hmove , ec      /*is this the winning move?*/
               when win(hm, N-1)   then call Hmove , ec      /* "   "   a blocking   "  */
               when @.middle== ''  then call Hmove middle    /*pick the  center  cell.  */
               when @.N.N  ==  ''  then call Hmove , N N     /*bottom right corner cell.*/
               when @.N.1  ==  ''  then call Hmove , N 1     /*   "    left    "     "  */
               when @.1.N  ==  ''  then call Hmove , 1 N     /*  top  right    "     "  */
               when @.1.1  ==  ''  then call Hmove , 1 1     /*   "    left    "     "  */
               otherwise                call Hmove , ac      /*pick a blank cell in grid*/
               end   /*select*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
Hmove: parse arg Hplace,dr dc;     if Hplace==''  then Hplace = (dr - 1)*N    +  dc
       @.Hplace=cm                                           /*place computer's marker. */
       say;  say  $   'computer places a marker  ['cm"]  at cell number  "    Hplace
       call showGrid
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
showGrid: _=0;              cW=5;     cH=3;         open=0   /*cell width,  cell height.*/
           do   r=1 for N                                    /*construct array of cells.*/
             do c=1 for N;  _=_ + 1;  @.r.c=@._;    open=open | @._==''
             end   /*c*/
           end     /*r*/                                     /* [↑]  OPEN≡a cell is open*/
       say                                                   /* [↑]  create grid coörds.*/
       z=0;          do     j=1  for  N                      /* [↓]  show grids&markers.*/
                       do   t=1  for cH;    _=;  __=         /*MK is a marker in a cell.*/
                         do k=1  for  N;    if t==2  then z=z + 1;        mk=;     c#=
                         if t==2  then do;  mk=@.z;       c#=z       /*c# is cell number*/
                                       end
                          _= _  || jam || center(mk, cW)
                         __= __ || jam || center(c#, cW)
                         end   /*k*/
                       say pad  substr(_, 2)  pad  translate( substr(__, 2),  single, dbl)
                       end     /*t*/                                 /* [↑]  show a line*/
                     if j==N  then leave
                     _=
                        do b=1  for  N;       _=_ || junc || copies(bar, cW)
                        end   /*b*/                                  /* [↑]  a grid part*/
                     say   pad  substr(_, 2)  pad  translate( substr(_,  2),  single, dbl)
                     end        /*j*/
       say
       if win(hm)  then  call tell  'You  ('hm")  won"copies('!',random(1, 5) )
       if win(cm)  then  call tell  'The computer  ('cm")  won."
       if \open    then  call tell  'This tic─tac─toe game is a draw   (a cat scratch).'
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:  do 4; say; end;     say center(' 'arg(1)" ", sw, '─');      do 5; say; end;    exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
testB: parse arg bx; if bx\==' '  then return bx /*test if the  marker  isn't  a  blank.*/
       say oops   'character code for'      whoseX      "marker can't be a blank."
       exit 13                                   /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
win:   parse arg wm,w;            if w==''  then w=N     /* [↓]  see if there is a win. */
       ac=                                               /* [↓]  EC ≡ means Empty Cell. */
            do   r=1  for N;      _=0;   ec=             /*see if any  rows are a winner*/
              do c=1  for N;      _=_ + (@.r.c==wm)      /*count the # of markers in col*/
              if @.r.c==''  then ec=r c                  /*Cell empty?  Then remember it*/
              end   /*c*/                                /* [↓]  AX≡means avaiable cell.*/
            if ec\==''  then ac=ec                       /*Found an empty?  Then use it.*/
            if _==N | (_>=w & ec\=='')  then return 1=1  /*a winner has been determined.*/
            end     /*r*/                                /*w=N-1?  Checking for near win*/

            do   c=1  for N;     _=0;   ec=              /*see if any  cols are a winner*/
              do r=1  for N;     _=_ + (@.r.c==wm)       /*count the # of markers in row*/
              if @.r.c==''  then ec=r c                  /*Cell empty?  Then remember it*/
              end   /*r*/
            if ec\==''  then ac=ec                       /*Found an empty? Then remember*/
            if _==N | (_>=w & ec\=='')  then return 1=1  /*a winner has been determined.*/
            end     /*c*/
       _=0
       ec=                                               /*EC≡location of an empty cell.*/
            do d=1  for N;       _=_ + (@.d.d==wm)       /*A winning descending diag. ? */
            if @.d.d==''  then ec=d d                    /*Empty cell?  Then note cell #*/
            end     /*d*/

       if _==N | (_>=w & ec\=='')       then return 1=1  /*a winner has been determined.*/
       _=0;                                  r=1
            do c=N  for N  by -1;   _=_ + (@.r.c==wm)    /*A winning ascending diagonal?*/
            if @.r.c==''  then ec=r c                    /*Empty cell?  Then note cell #*/
            r=r + 1                                      /*bump the counter for the rows*/
            end     /*c*/

       if _==N | (_>=w & ec\=='')       then return 1=1  /*a winner has been determined.*/
       return 0                                          /*no winner "    "       "     */
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console);   this is used to determine the amount of padding for a centered display of the two grids.

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


{{out|output|text=  when using the input of:     <tt> -3 </tt>}}

(a negative   '''3'''   indicates a grid of   '''3<small>x</small>3'''   and that the computer should play first.)


Note:   the user input is shown along with the program output.
<pre style="height:153ex">
───────── computer places a marker  [O]  at cell number   5

                        ║     ║                              │     │
                        ║     ║                           1  │  2  │  3
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║  O  ║                           4  │  5  │  6
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║     ║                           7  │  8  │  9
                        ║     ║                              │     │

───────── Please enter a cell number to place your next marker [X]     (or Quit):
2         ◄■■■■■■■■■■ human player's move.

                        ║     ║                              │     │
                        ║  X  ║                           1  │  2  │  3
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║  O  ║                           4  │  5  │  6
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║     ║                           7  │  8  │  9
                        ║     ║                              │     │


───────── computer places a marker  [O]  at cell number   9

                        ║     ║                              │     │
                        ║  X  ║                           1  │  2  │  3
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║  O  ║                           4  │  5  │  6
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║     ║  O                        7  │  8  │  9
                        ║     ║                              │     │

───────── Please enter a cell number to place your next marker [X]    (or Quit):
1         ◄■■■■■■■■■■ human player's move.

                        ║     ║                              │     │
                     X  ║  X  ║                           1  │  2  │  3
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║  O  ║                           4  │  5  │  6
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║     ║  O                        7  │  8  │  9
                        ║     ║                              │     │


───────── computer places a marker  [O]  at cell number   3

                        ║     ║                              │     │
                     X  ║  X  ║  O                        1  │  2  │  3
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║  O  ║                           4  │  5  │  6
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║     ║  O                        7  │  8  │  9
                        ║     ║                              │     │

───────── Please enter a cell number to place your next marker [X]    (or Quit):
7         ◄■■■■■■■■■■ human player's move.

                        ║     ║                              │     │
                     X  ║  X  ║  O                        1  │  2  │  3
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║  O  ║                           4  │  5  │  6
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                     X  ║     ║  O                        7  │  8  │  9
                        ║     ║                              │     │


───────── computer places a marker  [O]  at cell number   6

                        ║     ║                              │     │
                     X  ║  X  ║  O                        1  │  2  │  3
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                        ║  O  ║  O                        4  │  5  │  6
                        ║     ║                              │     │
                   ═════╬═════╬═════                    ─────┼─────┼─────
                        ║     ║                              │     │
                     X  ║     ║  O                        7  │  8  │  9
                        ║     ║                              │     │





────────────────────────────── The computer  (O)  won. ──────────────────────────────

```

'''output'''   when using the input of:   <tt> 5   db </tt>

(which indicates a grid of   '''5x5'''   and that the marker for the human is a  hexadecimal    '''db'''   [█].)
<pre style="height:153ex">
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║            1  │  2  │  3  │  4  │  5
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║            6  │  7  │  8  │  9  │ 10
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           11  │ 12  │ 13  │ 14  │ 15
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           16  │ 17  │ 18  │ 19  │ 20
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           21  │ 22  │ 23  │ 24  │ 25
         ║     ║     ║     ║               │     │     │     │

───────── Please enter a cell number to place your next marker [█]    (or Quit):
5         ◄■■■■■■■■■■ human player's move.

         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║  █         1  │  2  │  3  │  4  │  5
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║            6  │  7  │  8  │  9  │ 10
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           11  │ 12  │ 13  │ 14  │ 15
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           16  │ 17  │ 18  │ 19  │ 20
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           21  │ 22  │ 23  │ 24  │ 25
         ║     ║     ║     ║               │     │     │     │


───────── computer places a marker  [O]  at cell number  14

         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║  █         1  │  2  │  3  │  4  │  5
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║            6  │  7  │  8  │  9  │ 10
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║  O  ║           11  │ 12  │ 13  │ 14  │ 15
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           16  │ 17  │ 18  │ 19  │ 20
         ║     ║     ║     ║               │     │     │     │
    ═════╬═════╬═════╬═════╬═════     ─────┼─────┼─────┼─────┼─────
         ║     ║     ║     ║               │     │     │     │
         ║     ║     ║     ║           21  │ 22  │ 23  │ 24  │ 25
         ║     ║     ║     ║               │     │     │     │

───────── Please enter a cell number to place your next marker [█]    (or Quit):
q         ◄■■■■■■■■■■ human player's response.




────────────────────────────────── quitting. ──────────────────────────────────

```



## Ring


Easy and simple implementation of tecTacToe in ring programming language with human-human type of game(for now).

This implementation is a gui implementation using the default gui provided by the language

The tecTacToe.ring is provided [https://github.com/AbdelrahmanGIT/RingSamples/blob/master/src/TecTacToe.ring here]

```ring

Load "guilib.ring"

#Provide a list to save each button status in numeric readable format
#0=nothing 1=X 2=O
lst=[]

#Provide onScreen button status and style
btns=[]

#Define who has the turn
isXTurn=true


    app=new qApp
    {

        frmMain=new qMainWindow()
		{
			setWindowTitle("TicTacToe!")
			resize(300,320)
			move(200,200)
			//buttons
			pos=0
			for y=0 to 2
				for x=0 to 2
				//Creating Buttons on the screen
					pos++
					Add(lst,0)
					Add(btns,new qPushButton(frmMain)
						{
							setGeometry(x*100,y*100,100,100)
							setText("-")
							setclickevent("Disp(" + pos +")")
							setstylesheet("font-size:24pt ; font: bold ; color:yellow ; background-color: green")
						})
				next
            next
			//StatusBar
			status=new qStatusBar(frmMain)
			{
                showMessage("Ready",0)
            }
			setwindowflags(Qt_dialog)
			setStatusbar(status)
			show()
        }
		exec()
	}

//Restart the game by re init buttons status
func reStart
    for i=1 to 9
        lst[i]=0
        btns[i].setText("-")
    next
isXTurn=true

func Disp x
    if isXTurn=true and lst[x]=0
		btns[x].setText("X")
		lst[x]=1
		isXTurn=false
	but isXTurn=false and lst[x]=0
        btns[x].setText("O")
        lst[x]=2
        isXTurn=true
    ok

	winner = CheckWinner()
	#if there is no Winner and still there is ability to winner
	#continue playing.
	if winner<1 return ok

	//Who is the winner!
    switch winner
        on 1
            new qMessagebox(frmMain)
            {
				SetWindowTitle("We have a winner!")
                SetText("Good job X you won!")
                show()
            }
        on 2
			new qMessagebox(frmMain)
            {
				SetWindowTitle("We have a winner!")
                SetText("Good job O you won!")
                show()
            }
        on 3
            new qMessagebox(frmMain)
            {
                SetWindowTitle("Oh no it's a tie")
                SetText("Oh no it's a tie!")
                show()
            }
    off
    reStart()

func CheckWinner
        //vertical check
        for v=1 to 9 step 3
            if lst[v]!=0 and lst[v+1]!=0 and lst[v+2]!=0
				if lst[v]=lst[v+1] and lst[v+1]=lst[v+2]
                    return lst[v]
                ok
            ok
        next
        //horzintal
        for h=1 to 3
            if lst[h]!=0 and lst[h+3]!=0 and lst[h+6]!=0
                if lst[h]=lst[h+3] and lst[h+3]=lst[h+6]
                    return lst[h]
                ok
            ok
        next
        //Cross
        if lst[1]!=0 and lst[5]!=0 and lst[9]!=0
            if lst[1]=lst[5] and lst[5]=lst[9] return lst[1] ok
        ok
        if lst[3]!=0 and lst[5]!=0 and lst[7]!=0
            if lst[3]=lst[5] and lst[5]=lst[7] return lst[3] ok
        ok
        //tie
        tie=true
        for i=1 to 9
            if lst[i]=0 tie=false exit ok
        next
        if tie=true return 3 ok return 0

```


{{out}}
[[http://ring-lang.sourceforge.net/tictactoe.jpg image]]


## Ruby

This implementation splits functionality into four classes: <code>Game</code>, <code>Player</code>, <code>HumanPlayer</code>, and <code>ComputerPlayer</code>. A game can be played between any combination of <code>HumanPlayer</code>s and <code>ComputerPlayer</code>s.

The AI for the computer uses certain heuristics to ensure that it never loses, but the AI is not as aggressive as it could be. The AI defends against the opening corner trap, but does not exploit it in its offense.

This implementation stores the board as a one-dimensional array and hardcodes all possible straight lines in <code>LINES</code>, rather than storing the board as a two-dimensional matrix and identifying straight lines dynamically.


```ruby
require 'set'

module TicTacToe
  LINES = [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[3,5,7]]

  class Game
    def initialize(player_1_class, player_2_class)
      @board = Array.new(10) # we ignore index 0 for convenience

      @current_player_id = 0
      @players = [player_1_class.new(self, "X"), player_2_class.new(self, "O")]
      puts "#{current_player} goes first."
    end
    attr_reader :board, :current_player_id

    def play
      loop do
        place_player_marker(current_player)

        if player_has_won?(current_player)
          puts "#{current_player} wins!"
          print_board
          return
        elsif board_full?
          puts "It's a draw."
          print_board
          return
        end

        switch_players!
      end
    end

    def free_positions
      Set.new((1..9).select {|position| @board[position].nil?})
    end

    def place_player_marker(player)
      position = player.select_position!
      puts "#{player} selects #{player.marker} position #{position}"
      @board[position] = player.marker
    end

    def player_has_won?(player)
      LINES.any? do |line|
        line.all? {|position| @board[position] == player.marker}
      end
    end

    def board_full?
      free_positions.empty?
    end

    def other_player_id
      1 - @current_player_id
    end

    def switch_players!
      @current_player_id = other_player_id
    end

    def current_player
      @players[current_player_id]
    end

    def opponent
      @players[other_player_id]
    end

    def turn_num
      10 - free_positions.size
    end

    def print_board
      col_separator, row_separator = " | ", "--+---+--"
      label_for_position = lambda{|position| @board[position] ? @board[position] : position}

      row_for_display = lambda{|row| row.map(&label_for_position).join(col_separator)}
      row_positions = [[1,2,3], [4,5,6], [7,8,9]]
      rows_for_display = row_positions.map(&row_for_display)
      puts rows_for_display.join("\n" + row_separator + "\n")
    end
  end

  class Player
    def initialize(game, marker)
      @game = game
      @marker = marker
    end
    attr_reader :marker
  end

  class HumanPlayer < Player
    def select_position!
      @game.print_board
      loop do
        print "Select your #{marker} position: "
        selection = gets.to_i
        return selection if @game.free_positions.include?(selection)
        puts "Position #{selection} is not available. Try again."
      end
    end

    def to_s
      "Human"
    end
  end

  class ComputerPlayer < Player
    DEBUG = false # edit this line if necessary

    def group_positions_by_markers(line)
      markers = line.group_by {|position| @game.board[position]}
      markers.default = []
      markers
    end

    def select_position!
      opponent_marker = @game.opponent.marker

      winning_or_blocking_position = look_for_winning_or_blocking_position(opponent_marker)
      return winning_or_blocking_position if winning_or_blocking_position

      if corner_trap_defense_needed?
        return corner_trap_defense_position(opponent_marker)
      end

      # could make this smarter by sometimes doing corner trap offense

      return random_prioritized_position
    end

    def look_for_winning_or_blocking_position(opponent_marker)
      for line in LINES
        markers = group_positions_by_markers(line)
        next if markers[nil].length != 1
        if markers[self.marker].length == 2
          log_debug "winning on line #{line.join}"
          return markers[nil].first
        elsif markers[opponent_marker].length == 2
          log_debug "could block on line #{line.join}"
          blocking_position = markers[nil].first
        end
      end
      if blocking_position
        log_debug "blocking at #{blocking_position}"
        return blocking_position
      end
    end

    def corner_trap_defense_needed?
      corner_positions = [1, 3, 7, 9]
      opponent_chose_a_corner = corner_positions.any?{|pos| @game.board[pos] != nil}
      return @game.turn_num == 2 && opponent_chose_a_corner
    end

    def corner_trap_defense_position(opponent_marker)
      # if you respond in the center or the opposite corner, the opponent can force you to lose
      log_debug "defending against corner start by playing adjacent"
      # playing in an adjacent corner could also be safe, but would require more logic later on
      opponent_position = @game.board.find_index {|marker| marker == opponent_marker}
      safe_responses = {1=>[2,4], 3=>[2,6], 7=>[4,8], 9=>[6,8]}
      return safe_responses[opponent_position].sample
    end

    def random_prioritized_position
      log_debug "picking random position, favoring center and then corners"
      ([5] + [1,3,7,9].shuffle + [2,4,6,8].shuffle).find do |pos|
        @game.free_positions.include?(pos)
      end
    end

    def log_debug(message)
      puts "#{self}: #{message}" if DEBUG
    end

    def to_s
      "Computer#{@game.current_player_id}"
    end
  end
end

include TicTacToe

Game.new(ComputerPlayer, ComputerPlayer).play
puts
players_with_human = [HumanPlayer, ComputerPlayer].shuffle
Game.new(*players_with_human).play
```


{{out}}

```txt

Computer0 goes first.
Computer0 selects X position 5
Computer1 selects O position 9
Computer0 selects X position 3
Computer1 selects O position 7
Computer0 selects X position 8
Computer1 selects O position 2
Computer0 selects X position 1
Computer1 selects O position 4
Computer0 selects X position 6
It's a draw.
X | O | X
--+---+--
O | X | X
--+---+--
O | X | O

Human goes first.
1 | 2 | 3
--+---+--
4 | 5 | 6
--+---+--
7 | 8 | 9
Select your X position: 3
Human selects X position 3
Computer1 selects O position 6
1 | 2 | X
--+---+--
4 | 5 | O
--+---+--
7 | 8 | 9
Select your X position: 7
Human selects X position 7
Computer1 selects O position 5
1 | 2 | X
--+---+--
4 | O | O
--+---+--
X | 8 | 9
Select your X position: 4
Human selects X position 4
Computer1 selects O position 1
O | 2 | X
--+---+--
X | O | O
--+---+--
X | 8 | 9
Select your X position: 9
Human selects X position 9
Computer1 selects O position 8
O | 2 | X
--+---+--
X | O | O
--+---+--
X | O | X
Select your X position: 2
Human selects X position 2
It's a draw.
O | X | X
--+---+--
X | O | O
--+---+--
X | O | X
```



## Run BASIC


```runbasic
' ---------------------------
'  TIC TAC TOE
' ---------------------------
winBox$ = "123 456 789 159 147 258 369 357"
boxPos$ = "123 231 456 564 789 897 159 591 357 753 132 465 798 174 285 396 159 471 582 693 147 258 369 195 375"
ai$     = "519628374"
ox$     = "OX"
[newGame]
for i = 1 to 9
 box$(i) = ""
next i
goto [shoTic]

[loop]
for j = 1 to 2
 tic$ = mid$(ox$,j,1)
 for i = 1 to 25
  b$	= word$(boxPos$,i," ")
  b1	= val(mid$(b$,1,1))
  b2	= val(mid$(b$,2,1))
  b3	= val(mid$(b$,3,1))
  if box$(b1) = tic$ AND box$(b2) = tic$ AND box$(b3) = "" then
     box$(b3)  = "O"
     goto [shoTic]
 end if
 next i
next j
if box$(1) = "O" AND box$(5) = "X" and box$(9) = "X" then
 if box$(3) = "" then
   box$(3) = "O"
   goto [shoTic]
 end if
 if box$(7) = "" then
   box$(7) = "O"
   goto [shoTic]
 end if
end if
for i = 1 to 9
 b1 = val(mid$(ai$,i,1))
 if box$(b1) = "" then
   box$(b1)  = "O"
   exit for
 end if
next i

[shoTic]
cls
' ----------------------------------------
' show tic tac toe screen
' ----------------------------------------
html "<table border=1 width=300px height=225px><TR>"
for i = 1 to 9
  html "<td align=center width=33%><h1>"
    if box$(i) <> "" then
     html box$(i)
    else
    button #box, " ";box$(i);" ", [doTic]
           #box  setkey(str$(i))
    end if
    if i mod 3 = 0 then html "</tr><tr>"
next i
html "</table>"
gosub [checkWin]
wait

[doTic]
box$(val(EventKey$)) = "X"
turn          = 1
gosub [checkWin]
goto [loop]

' --- check for a winner ----------
[checkWin]
for i = 1 to 8
 b$ = word$(winBox$,i," ")
 b1 = val(mid$(b$,1,1))
 b2 = val(mid$(b$,2,1))
 b3 = val(mid$(b$,3,1))
 if box$(b1) = "O" and box$(b2) = "O" and box$(b3) = "O" then
   print "You Lose!"
   goto [playAgain]
 end if
 if box$(b1) = "X" and box$(b2) = "X" and box$(b3) = "X" then
   print "You Win!"
   goto [playAgain]
 end if
next i

moveCount = 0
for i = 1 to 9
 if box$(i) <> "" then moveCount = moveCount + 1
next i
if moveCount = 9 then
  print "Draw!"
  goto [playAgain]
end if
RETURN

[playAgain]
input "Play again (y/n)";p$
if upper$(p$) = "Y" then goto [newGame]
end
```



## Scala


### Functional implementation.


Computer vs. human. Human starts. Computer plays 'O' and human plays 'X'.
Computer moves are legal, but random.

```scala
package object tictactoe {
  val Human = 'X'
  val Computer = 'O'
  val BaseBoard = ('1' to '9').toList
  val WinnerLines = List((0,1,2), (3,4,5), (6,7,8), (0,3,6), (1,4,7), (2,5,8), (0,4,8), (2,4,6))
  val randomGen = new util.Random(System.currentTimeMillis)
}

package tictactoe {

class Board(aBoard : List[Char] = BaseBoard) {

  def availableMoves = aBoard.filter(c => c != Human && c != Computer)

  def availableMovesIdxs = for ((c,i) <- aBoard.zipWithIndex if c != Human && c != Computer) yield i

  def computerPlays = new Board(aBoard.updated(availableMovesIdxs(randomGen.nextInt(availableMovesIdxs.length)), Computer))

  def humanPlays(move : Char) = new Board(aBoard.updated(aBoard.indexOf(move), Human))

  def isDraw = aBoard.forall(c => c == Human || c == Computer)

  def isWinner(winner : Char) =
    WinnerLines.exists{case (i,j,k) => aBoard(i) == winner && aBoard(j) == winner && aBoard(k) == winner}

  def isOver = isWinner(Computer) || isWinner(Human) || isDraw

  def print {
    aBoard.grouped(3).foreach(row => println(row(0) + " " + row(1) + " " + row(2)))
  }

  def printOverMessage {
    if (isWinner(Human)) println("You win.")
    else if (isWinner(Computer)) println("Computer wins.")
    else if (isDraw) println("It's a draw.")
    else println("Not over yet, or something went wrong.")
  }

}


object TicTacToe extends App {

   def play(board : Board, turn : Char) {

    // Reads a char from input until it is one of
    // the available moves in the current board
    def readValidMove() : Char = {
      print("Choose a move: ")
      val validMoves = board.availableMoves
      val move = readChar
      if (validMoves.contains(move)) {
        move
      } else {
        println("Invalid move. Choose another one in " + validMoves)
        readValidMove()
      }
    }


    board.print

    if (board.isOver) {
      board.printOverMessage
      return
    }

    if (turn == Human) { // Human plays
      val nextBoard = board.humanPlays(readValidMove)
      play(nextBoard, Computer)
    } else { // Computer plays
      println("Computer plays: ")
      val nextBoard = board.computerPlays
      play(nextBoard, Human)
    }
  }

  play(new Board(),Human)

}

}
```


{{out}}(human is always first)

```txt

1 2 3
4 5 6
7 8 9
Choose a move: 1
X 2 3
4 5 6
7 8 9
Computer plays:
X 2 O
4 5 6
7 8 9
Choose a move: 3
Invalid move. Choose another one in List(2, 4, 5, 6, 7, 8, 9)
Choose a move: 4
X 2 O
X 5 6
7 8 9
Computer plays:
X 2 O
X 5 6
7 8 O
Choose a move: 7
X 2 O
X 5 6
X 8 O
You win.
```



## Scilab


Can be a game of human v. human, human v. machine, or machine v. machine. Machine moves have a hierarchy: firstly, it looks for a winning move; secondly, it looks for a way to block the opponent's victory; lastly, it makes a random move.

<lang>function [] = startGame()
    //Board size and marks
    N = 3;
    marks = ["X" "O"];

    //Creating empty board
    board = string(zeros(N,N));
    for i = 1:(N*N)
        board(i) = "";
    end

    //Initialising players
    clc();
    players= [%F %F];
    players = playerSetup(marks);

    //Console header
    header = [strsplit(marks(1)+" is ----")';...
              strsplit(marks(2)+" is ----")'];
    for i = 1:2
        if players(i) then
            header(i,6:10) = strsplit("P"+string(i)+".  ");
        else
            header(i,6:10) = strsplit("COMP.");
        end
    end

    //Game loop
    sleep(1000);
    win_flag = %F;
    count = 0;
    while count<N*N
        //Clear console, and print header and board
        clc();
        printf("%s\n %s\n",strcat(header(1,:)),strcat(header(2,:)));
        dispBoard(board);

        //Find which player should move
        player_n = modulo(count,2) + 1;

        if players(player_n) == %T then
            //Human plays
            pos = [];
            valid_move = %F;
            disp(marks(player_n)+"''s turn.");
            while valid_move ~= %T
                [pos,valid_move] = readHumanMove(board);
                if ~valid_move then
                    disp("You should input a valid cell number.");
                end
            end

            if valid_move then
                board = updateBoard(board,pos,marks(player_n));
            else
                error("Invalid move.");
            end
        else
            //Computer plays
            disp("Computer is playing.");
            board = ComputerMove(board,marks(player_n),marks);
            sleep(800);
        end

        //Count number of movements
        count = count + 1;

        //Check if the game has finished
        [win_flag,winning_mark] = detectWin(board)
        if win_flag then
            break
        end
    end

    //Clear screen at the end of game
    clc();
    disp("Game finished:");
    dispBoard(board);

    //Print results
    if win_flag then
        disp(winning_mark+" won!");
    else
        disp("It''s a tie.");
    end

    //Play again?
    play_again = "";
    while play_again ~= "Y" & play_again ~= "N"
        play_again = input("Would you like to play again? (Y/N)","s");
        play_again = strsplit(play_again);
        play_again = convstr(play_again(1),"u");

        if play_again ~= "Y" & play_again ~= "N" then
            disp("Invalid answer.");
        end
    end

    if play_again == "Y" then
        startGame();
    else
        disp("Quit game.");
    end
endfunction

function players = playerSetup(marks)
    //Determines who plays which mark
    players = [%F %F]; //True for human, Flase for computer

    printf("\n%s always starts.\n",marks(1));
    for i = 1:2
        user_input = "";
        while user_input ~= "Y" & user_input ~= "N"
            user_input = input("Would you like to play as "+marks(i)+"? (Y/N)","s");
            user_input = strsplit(user_input);
            user_input = convstr(user_input(1),"u");

            if user_input ~= "Y" & user_input ~= "N" then
                disp("Invalid answer.");
            end
        end

        //Print choice
        if user_input == "Y" then
            players(i) = %T;
            printf("%s shall be player %d (P%d).\n\n",marks(i),i,i);
        else
            printf("%s shall be the computer (COMP).\n\n",marks(i));
        end
    end
endfunction

function [] = dispBoard(board)
    //Print ASCII board on console

    //Get board marks
    marks = [" " " "];
    mark_inds = find(board ~= "");
    if mark_inds ~= [] then
        marks(1) = board(mark_inds(1));
        mark_inds = find( (board ~= "") & (board ~= marks(1)) );
        if mark_inds ~= [] then
            marks(2) = board(mark_inds(1));
        end
    end

    //Transpose to display for humans
    //[compatibility with readHumanMove()]
    disp_board = board';

    rows = 3*size(board,'r');
    cols = 4*size(board,'c');
    ascii_board = string(zeros(rows, cols));

    mark_1=[...
    strsplit("   |")';...
    strsplit(" "+marks(1)+" |")';...
    strsplit("___|")'];

    mark_2=[...
    strsplit("   |")';...
    strsplit(" "+marks(2)+" |")';...
    strsplit("___|")'];

    Blank_mark=[...
    strsplit("   |")';...
    strsplit("   |")';...
    strsplit("___|")'];

    for r = ([1:size(board,'r')] - 1 )
        for c = ([1:size(board,'c')] - 1)
            if disp_board(r+1,c+1) == marks(1) then
                ascii_board((r*3 + 1):((r+1)*3),...
                            (c*4 + 1):((c+1)*4)) = mark_1;
            elseif disp_board(r+1,c+1) == marks(2) then
                ascii_board((r*3 + 1):((r+1)*3),...
                            (c*4 + 1):((c+1)*4)) = mark_2;
            else
                ascii_board((r*3 + 1):((r+1)*3),...
                            (c*4 + 1):((c+1)*4)) = Blank_mark;
            end
        end
    end

    for i = 1:cols
        if modulo(i,4)>0 then
            ascii_board(rows,i) = " ";
        end
    end

    for i = 1:rows
        ascii_board(i,cols) = " ";
    end

    printf("\n");
    for i = 1:size(ascii_board,'r')
        printf("%s\n",strcat(ascii_board(i,:)))
    end
endfunction

function moves_board = availableMoves(board)
    //Find empty cells on the board
    moves_board = board;

    for i = 1:(size(board,'r')*size(board,'c'))
        if board(i) == "" then
            moves_board(i) = string(i);
        else
            moves_board(i) = "_";
        end
    end
endfunction

function varargout = readHumanMove(board)
    //Read human input
    printf("\nAvailable cells:");
    moves_board = availableMoves(board);
    disp(moves_board');

    x = input("\nEnter a move (0 to quit game): ");

    valid = %F;
    pos = 0;
    total = size(moves_board,'r') * size(moves_board,'c');

    //Check if it is a valid move
    if x == 0 then
        disp("Quit game.")
        abort
    elseif (x>=1 & x<=total) then
        if (moves_board(x) == string(x)) then
            valid = %T;
            pos = x;
        end
    end

    varargout = list(pos,valid);
endfunction

function varargout = updateBoard(board,pos,player)
    //Add move to the board
    if board(pos) ~= "" then
        error('Error: Invalid move.');
    end

    board(pos) = player

    varargout = list(board);
endfunction

function varargout = detectWin(board)
    //Detect if there is a winner or not
    win_flag = %F;
    winner = "";

    //Get board marks
    marks = ["" ""];
    mark_inds = find(board ~= "");
    marks(1) = board(mark_inds(1))
    mark_inds = find( (board ~= "") & (board ~= marks(1)) );
    marks(2) = board(mark_inds(1));

    //If there is a minimum number of moves, check if there is a winner
    n_moves = find(~(board == ""));
    n_moves = length(n_moves)

    if n_moves >= size(board,'r') then
        board_X = (board == marks(1));
        board_O = (board == marks(2));

        for i = 1:size(board,'r')
            //Check rows
            if find(~board_X(i,:)) == [] then
                win_flag = %T;
                winner = marks(1);
                break
            end
            if find(~board_O(i,:)) == [] then
                win_flag = %T;
                winner = marks(2);
                break
            end

            //Check columns
            if find(~board_X(:,i)) == [] then
                win_flag = %T;
                winner = marks(1);
                break
            end
            if find(~board_O(:,i)) == [] then
                win_flag = %T;
                winner = marks(2);
                break
            end
        end

        //Check diagonal
        if ~win_flag then
            if find(~diag(board_X)) == [] then
                win_flag = %T;
                winner = marks(1);
            elseif find(~diag(board_O)) == [] then
                win_flag = %T;
                winner = marks(2);
            end
        end

        //Check anti-diagonal
        if ~win_flag then
            board_X = board_X(:,$:-1:1);
            board_O = board_O(:,$:-1:1);

            if find(~diag(board_X)) == [] then
                win_flag = %T;
                winner = marks(1);
            elseif find(~diag(board_O)) == [] then
                win_flag = %T;
                winner = marks(2);
            end
        end
    end

    varargout = list(win_flag,winner)
endfunction

function threat_pos = findThreat(board,player)
    //Returns a list of moves that can finish the game

    //Available moves
    move_inds = find(~( availableMoves(board) == "_" ));

    //If there is a minimum number of moves, check if there is a threat
    threat_pos = [];
    if (size(board,'r')*size(board,'c')) - length(move_inds) >...
       (size(board,'r') - 1) then
        for i = 1:length(move_inds)
            temp_board = updateBoard(board,move_inds(i),player);
            [win_flag,winner] = detectWin(temp_board);
            if win_flag & winner == player then
                threat_pos = [threat_pos move_inds(i)];
            end
        end
    end
endfunction

function varargout = ComputerMove(board,mark,all_marks)
    //Atomatically add a move to the board with no human input

    //Find winning moves moves
    move_inds = findThreat(board,mark);

    //If there are no winning moves, find opponent's winning moves
    //to block opponent's victory
    if move_inds == [] then
        if mark == all_marks(1) then
            opponent = all_marks(2);
        elseif mark == all_marks(2) then
            opponent = all_marks(1);
        end

        move_inds = findThreat(board,opponent);
    end

    //If there are no winning moves or threats, find all possible moves
    if move_inds == [] then
        move_inds = find(~( availableMoves(board) == "_" ));
    end

    //Choose a random move among the selected possible moves
    pos = grand(1,"prm",move_inds);
    pos = pos(1);

    //Update board by adding a new mark
    board(pos) = mark;

    varargout = list(board);
endfunction

startGame()
```



## Swift

Some Basic AI for obvious losing and winning conditions

```Swift

import Darwin

enum Token : CustomStringConvertible {
	case cross, circle

	func matches(tokens: [Token?]) -> Bool {
		for token in tokens {
			guard let t = token, t == self else {
				return false
			}
		}
		return true
	}

	func emptyCell(in tokens: [Token?]) -> Int? {
		if tokens[0] == nil
		&& tokens[1] == self
		&& tokens[2] == self {
			return 0
		} else
		if tokens[0] == self
		&& tokens[1] == nil
		&& tokens[2] == self {
			return 1
		} else
		if tokens[0] == self
		&& tokens[1] == self
		&& tokens[2] == nil {
			return 2
		}
		return nil
	}

	var description: String {
		switch self {
			case .cross: return "x"
			case .circle: return "o"
		}
	}
}

struct Board {
	var cells: [Token?] = [nil, nil, nil, nil, nil, nil, nil, nil, nil]

	func cells(atCol col: Int) -> [Token?] {
		return [cells[col], cells[col + 3], cells[col + 6]]
	}

	func cells(atRow row: Int) -> [Token?] {
		return [cells[row * 3], cells[row * 3 + 1], cells[row * 3 + 2]]
	}

	func cellsTopLeft() -> [Token?] {
		return [cells[0], cells[4], cells[8]]
	}

	func cellsBottomLeft() -> [Token?] {
		return [cells[6], cells[4], cells[2]]
	}

	func winner() -> Token? {
		let r0 = cells(atRow: 0)
		let r1 = cells(atRow: 1)
		let r2 = cells(atRow: 2)
		let c0 = cells(atCol: 0)
		let c1 = cells(atCol: 1)
		let c2 = cells(atCol: 2)
		let tl = cellsTopLeft()
		let bl = cellsBottomLeft()

		if Token.cross.matches(tokens: r0)
		|| Token.cross.matches(tokens: r1)
		|| Token.cross.matches(tokens: r2)
		|| Token.cross.matches(tokens: c0)
		|| Token.cross.matches(tokens: c1)
		|| Token.cross.matches(tokens: c2)
		|| Token.cross.matches(tokens: tl)
		|| Token.cross.matches(tokens: bl) {
			return .cross
		} else
		if Token.circle.matches(tokens: r0)
		|| Token.circle.matches(tokens: r1)
		|| Token.circle.matches(tokens: r2)
		|| Token.circle.matches(tokens: c0)
		|| Token.circle.matches(tokens: c1)
		|| Token.circle.matches(tokens: c2)
		|| Token.circle.matches(tokens: tl)
		|| Token.circle.matches(tokens: bl) {
			return .circle
		}
		return nil
	}

	func atCapacity() -> Bool {
		return cells.filter { $0 == nil }.count == 0
	}

	mutating func play(token: Token, at location: Int) {
		cells[location] = token
	}

	func findBestLocation(for player: Token) -> Int? {
		let r0 = cells(atRow: 0)
		let r1 = cells(atRow: 1)
		let r2 = cells(atRow: 2)
		let c0 = cells(atCol: 0)
		let c1 = cells(atCol: 1)
		let c2 = cells(atCol: 2)
		let tl = cellsTopLeft()
		let bl = cellsBottomLeft()

		if let cell = player.emptyCell(in: r0) {
			return cell
		} else if let cell = player.emptyCell(in: r1) {
			return cell + 3
		} else if let cell = player.emptyCell(in: r2) {
			return cell + 6
		} else if let cell = player.emptyCell(in: c0) {
			return cell * 3
		} else if let cell = player.emptyCell(in: c1) {
			return cell * 3 + 1
		} else if let cell = player.emptyCell(in: c2) {
			return cell * 3 + 2
		} else if let cell = player.emptyCell(in: tl) {
			return cell == 0 ? 0 : (cell == 1 ? 4 : 8)
		} else if let cell = player.emptyCell(in: bl) {
			return cell == 0 ? 6 : (cell == 1 ? 4 : 2)
		}
		return nil
	}

	func findMove() -> Int {
		let empties = cells.enumerated().filter { $0.1 == nil }
		let r = Int(arc4random()) % empties.count
		return empties[r].0
	}
}

extension Board : CustomStringConvertible {
	var description: String {
		var result = "\n---------------\n"
		for (idx, cell) in cells.enumerated() {
			if let cell = cell {
				result += "| \(cell) |"
			} else {
				result += "| \(idx) |"
			}

			if (idx + 1) % 3 == 0 {
				result += "\n---------------\n"
			}
		}
		return result
	}
}

while true {
	var board = Board()
	print("Who do you want to play as ('o' or 'x'): ", separator: "", terminator: "")
	let answer = readLine()?.characters.first ?? "x"

	var player: Token = answer == "x" ? .cross : .circle
	var pc: Token = player == .cross ? .circle : .cross

	print(board)

	while true {
		print("Choose cell to play on: ", separator: "", terminator: "")
		var pos = Int(readLine() ?? "0") ?? 0
		while !board.atCapacity() && board.cells[pos] != nil {
			print("Invalid move. Choose cell to play on: ", separator: "", terminator: "")
			pos = Int(readLine() ?? "0") ?? 0
		}

		if board.atCapacity() {
			print("Draw")
			break
		}

		board.play(token: player, at: pos)
		print(board)

		if let winner = board.winner() {
			print("winner is \(winner)")
			break
		} else if board.atCapacity() {
			print("Draw")
			break
		}

		if let win = board.findBestLocation(for: pc) {
			board.play(token: pc, at: win)
		} else if let def = board.findBestLocation(for: player) {
			board.play(token: pc, at: def)
		} else {
			board.play(token: pc, at: board.findMove())
		}

		print(board)

		if let winner = board.winner() {
			print("winner is \(winner)")
			break
		}
	}
}

```

{{out}}

```txt

Who do you want to play as ('o' or 'x'): x

---------------
| 0 || 1 || 2 |
---------------
| 3 || 4 || 5 |
---------------
| 6 || 7 || 8 |
---------------

Choose cell to play on: 4

---------------
| 0 || 1 || 2 |
---------------
| 3 || x || 5 |
---------------
| 6 || 7 || 8 |
---------------


---------------
| o || 1 || 2 |
---------------
| 3 || x || 5 |
---------------
| 6 || 7 || 8 |
---------------

Choose cell to play on: 6

---------------
| o || 1 || 2 |
---------------
| 3 || x || 5 |
---------------
| x || 7 || 8 |
---------------


---------------
| o || 1 || o |
---------------
| 3 || x || 5 |
---------------
| x || 7 || 8 |
---------------

Choose cell to play on: 1

---------------
| o || x || o |
---------------
| 3 || x || 5 |
---------------
| x || 7 || 8 |
---------------


---------------
| o || x || o |
---------------
| 3 || x || 5 |
---------------
| x || o || 8 |
---------------

Choose cell to play on: 5

---------------
| o || x || o |
---------------
| 3 || x || x |
---------------
| x || o || 8 |
---------------


---------------
| o || x || o |
---------------
| o || x || x |
---------------
| x || o || 8 |
---------------

Choose cell to play on: 8

---------------
| o || x || o |
---------------
| o || x || x |
---------------
| x || o || x |
---------------

Draw
Who do you want to play as ('o' or 'x'):

```



## Tcl

{{trans|Python}}

```tcl
package require Tcl 8.6

# This code splits the players from the core game engine
oo::class create TicTacToe {
    variable board player letter who
    constructor {player1class player2class} {
	set board {1 2 3 4 5 6 7 8 9}
	set player(0) [$player1class new [self] [set letter(0) "X"]]
	set player(1) [$player2class new [self] [set letter(1) "O"]]
	set who 0
    }

    method PrintBoard {} {
	lassign $board a1 b1 c1 a2 b2 c2 a3 b3 c3
	puts [format " %s | %s | %s" $a1 $b1 $c1]
	puts "---+---+---"
	puts [format " %s | %s | %s" $a2 $b2 $c2]
	puts "---+---+---"
	puts [format " %s | %s | %s" $a3 $b3 $c3]
    }

    method WinForSomeone {} {
	foreach w {
	    {0 1 2} {3 4 5} {6 7 8} {0 3 6} {1 4 7} {2 5 8} {0 4 8} {2 4 6}
	} {
	    set b [lindex $board [lindex $w 0]]
	    if {$b ni "X O"} continue
	    foreach i $w {if {[lindex $board $i] ne $b} break}
	    if {[lindex $board $i] eq $b} {
		foreach p $w {lappend w1 [expr {$p+1}]}
		return [list $b $w1]
	    }
	}
	return ""
    }

    method status {} {
	return $board
    }

    method IsDraw {} {
	foreach b $board {if {[string is digit $b]} {return false}}
	return true
    }

    method legalMoves {} {
	foreach b $board {if {[string is digit $b]} {lappend legal $b}}
	return $legal
    }

    method DoATurn {} {
	set legal [my legalMoves]
	my PrintBoard
	while 1 {
	    set move [$player($who) turn]
	    if {$move in $legal} break
	    puts "Illegal move!"
	}
	lset board [expr {$move - 1}] $letter($who)
	$player($who) describeMove $move
	set who [expr {1 - $who}]
	return [my WinForSomeone]
    }

    method game {} {
        puts "    Tic-tac-toe game player.
    Input the index of where you wish to place your mark at your turn.\n"
	while {![my IsDraw]} {
	    set winner [my DoATurn]
	    if {$winner eq ""} continue
	    lassign $winner winLetter winSites
	    my PrintBoard
	    puts "\n$winLetter wins across \[[join $winSites {, }]\]"
	    return $winLetter
	}
	puts "\nA draw"
    }
}

# Stupid robotic player
oo::class create RandomRoboPlayer {
    variable g
    constructor {game letter} {
	set g $game
    }
    method turn {} {
	set legal [$g legalMoves]
	return [lindex $legal [expr {int(rand()*[llength $legal])}]]
    }
    method describeMove {move} {
	puts "I go at $move"
    }
}

# Interactive human player delegate
oo::class create HumanPlayer {
    variable g char
    constructor {game letter} {
	set g $game
	set char $letter
    }
    method turn {} {
	set legal [$g legalMoves]
	puts ">>> Put your $char in any of these positions: [join $legal {}]"
	while 1 {
	    puts -nonewline ">>> "
	    flush stdout
	    gets stdin number
	    if {$number in $legal} break
	    puts ">>> Whoops I don't understand the input!"
	}
	return $number
    }
    method describeMove {move} {
	puts "You went at $move"
    }
}

# Assemble the pieces
set ttt [TicTacToe new HumanPlayer RandomRoboPlayer]
$ttt game
```

Sample game:

```txt

    Tic-tac-toe game player.
    Input the index of where you wish to place your mark at your turn.

 1 | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9
>>> Put your X in any of these positions: 123456789
>>> 1
You went at 1
 X | 2 | 3
---+---+---
 4 | 5 | 6
---+---+---
 7 | 8 | 9
I go at 5
 X | 2 | 3
---+---+---
 4 | O | 6
---+---+---
 7 | 8 | 9
>>> Put your X in any of these positions: 2346789
>>> 7
You went at 7
 X | 2 | 3
---+---+---
 4 | O | 6
---+---+---
 X | 8 | 9
I go at 9
 X | 2 | 3
---+---+---
 4 | O | 6
---+---+---
 X | 8 | O
>>> Put your X in any of these positions: 23468
>>> 4
You went at 4
 X | 2 | 3
---+---+---
 X | O | 6
---+---+---
 X | 8 | O

X wins across [1, 4, 7]

```



## VBA

Human play first with the "X". You must choose the row and the column you want to play...

```vb

Option Explicit

Private Lines(1 To 3, 1 To 3) As String
Private Nb As Byte, player As Byte
Private GameWin As Boolean, GameOver As Boolean

Sub Main_TicTacToe()
Dim p As String

    InitLines
    printLines Nb
    Do
        p = WhoPlay
        Debug.Print p & " play"
        If p = "Human" Then
            Call HumanPlay
            GameWin = IsWinner("X")
        Else
            Call ComputerPlay
            GameWin = IsWinner("O")
        End If
        If Not GameWin Then GameOver = IsEnd
    Loop Until GameWin Or GameOver
    If Not GameOver Then
        Debug.Print p & " Win !"
    Else
        Debug.Print "Game Over!"
    End If
End Sub

Sub InitLines(Optional S As String)
Dim i As Byte, j As Byte
    Nb = 0: player = 0
    For i = LBound(Lines, 1) To UBound(Lines, 1)
        For j = LBound(Lines, 2) To UBound(Lines, 2)
            Lines(i, j) = "#"
        Next j
    Next i
End Sub

Sub printLines(Nb As Byte)
Dim i As Byte, j As Byte, strT As String
    Debug.Print "Loop " & Nb
    For i = LBound(Lines, 1) To UBound(Lines, 1)
        For j = LBound(Lines, 2) To UBound(Lines, 2)
            strT = strT & Lines(i, j)
        Next j
        Debug.Print strT
        strT = vbNullString
    Next i
End Sub

Function WhoPlay(Optional S As String) As String
    If player = 0 Then
        player = 1
        WhoPlay = "Human"
    Else
        player = 0
        WhoPlay = "Computer"
    End If
End Function

Sub HumanPlay(Optional S As String)
Dim L As Byte, C As Byte, GoodPlay As Boolean

    Do
        L = Application.InputBox("Choose the row", "Numeric only", Type:=1)
        If L > 0 And L < 4 Then
            C = Application.InputBox("Choose the column", "Numeric only", Type:=1)
            If C > 0 And C < 4 Then
                If Lines(L, C) = "#" And Not Lines(L, C) = "X" And Not Lines(L, C) = "O" Then
                    Lines(L, C) = "X"
                    Nb = Nb + 1
                    printLines Nb
                    GoodPlay = True
                End If
            End If
        End If
    Loop Until GoodPlay
End Sub

Sub ComputerPlay(Optional S As String)
Dim L As Byte, C As Byte, GoodPlay As Boolean

    Randomize Timer
    Do
        L = Int((Rnd * 3) + 1)
        C = Int((Rnd * 3) + 1)
        If Lines(L, C) = "#" And Not Lines(L, C) = "X" And Not Lines(L, C) = "O" Then
            Lines(L, C) = "O"
            Nb = Nb + 1
            printLines Nb
            GoodPlay = True
        End If
    Loop Until GoodPlay
End Sub

Function IsWinner(S As String) As Boolean
Dim i As Byte, j As Byte, Ch As String, strTL As String, strTC As String

    Ch = String(UBound(Lines, 1), S)
    'check lines & columns
    For i = LBound(Lines, 1) To UBound(Lines, 1)
        For j = LBound(Lines, 2) To UBound(Lines, 2)
            strTL = strTL & Lines(i, j)
            strTC = strTC & Lines(j, i)
        Next j
        If strTL = Ch Or strTC = Ch Then IsWinner = True: Exit For
        strTL = vbNullString: strTC = vbNullString
    Next i
    'check diagonales
    strTL = Lines(1, 1) & Lines(2, 2) & Lines(3, 3)
    strTC = Lines(1, 3) & Lines(2, 2) & Lines(3, 1)
    If strTL = Ch Or strTC = Ch Then IsWinner = True
End Function

Function IsEnd() As Boolean
Dim i As Byte, j As Byte

    For i = LBound(Lines, 1) To UBound(Lines, 1)
        For j = LBound(Lines, 2) To UBound(Lines, 2)
            If Lines(i, j) = "#" Then Exit Function
        Next j
    Next i
    IsEnd = True
End Function

```

{{out}}

```txt
Loop 0
###
###
###
Human Play
Loop 1
X##
###
###
Computer Play
Loop 2
X#O
###
###
Human Play
Loop 3
X#O
#X#
###
Computer Play
Loop 4
XOO
#X#
###
Human Play
Loop 5
XOO
#X#
##X
Human Win !
```



## XPL0

[[File:TTTXPL0.GIF|right]]

```XPL0
\The computer marks its moves with an "O" and the player uses an "X". The
\ numeric keypad is used to make the player's move.
\
\                         7 | 8 | 9
\                        ---+---+---
\                         4 | 5 | 6
\                        ---+---+---
\                         1 | 2 | 3
\
\The player always goes first, but the 0 key is used to skip a move. Thus
\ it can be used to let the computer play first. Esc terminates program.

inc     c:\cxpl\codes;  \intrinsic routine declarations
def     X0=16, Y0=10;   \coordinates of character in upper-left square
int     I0,
        PMove,          \player's move (^0..^9)
        Key;            \keystroke
int     X, O;           \bit arrays for player and computer
                        \ bit 0 corresponds to playing square 1, etc.


proc    HLine(X, Y);    \Draw a horizontal line
int     X, Y;
int     I;
[Cursor(X, Y);
for I:= 0 to 10 do ChOut(0, ^Ä);
];      \HLine


proc    VLine(X, Y);    \Draw a vertical line over the above horizontal line
int     X, Y;
int     I;
[for I:= 0 to 4 do
        [Cursor(X, Y+I);
        ChOut(0, if I&1 then ^Å else ^³);
        ];
];      \VLine


func    Won(p);         \Return 'true' if player P has won
int     P;
int     T, I;
[T:= [$007, $038, $1C0, $049, $092, $124, $111, $054];
for I:= 0 to 7 do       \check if player matches a bit pattern for 3 in a row
    if (P & T(I)) = T(I) then return true;
return false;
];      \Won


func    Cats;           \Return 'true' if no more moves available (Cat's game)
[if (X ! O) = $1FF then \all bit positions played
        [Cursor(17, 20);
        Text(0, "A draw!");
        return true;
        ];
return false;
];      \Cats


proc    DoMove(P, M, Ch); \Make move in player's bit array and display it
int     P,              \address of player's bit array
        M,              \index 0..8 where bit is placed
        Ch;
int     I, X, Y;
[P(0):= P(0) ! 1<<M;    \make move

I:= M / 3;              \display move
X:= Rem(0) * 4;
Y:= (2-I) * 2;
Cursor(X+X0, Y+Y0);
ChOut(0, Ch);
];      \DoMove


func    Try(P);         \Return the value of the best node for player P
int     P;              \address of player's bit array
int     P1, I, I0, V, V0;
[P1:= if P = addr X then addr O else addr X;

if Won(P1(0)) then return -1;
if (X ! O) = $1FF then return 0;

V0:= -1;                                \assume the worst
for I:= 0 to 8 do                       \for all of the squares...
    if ((O!X) & 1<<I) = 0 then          \if square is unused
        [P(0):= P(0) ! 1<<I;            \make tenative move
        V:= -(extend(Try(P1)));         \get value
        if V > V0 then                  \save best value
                [V0:= V;  I0:= I];
        P(0):= P(0) & ~(1<<I);          \undo tenative move
        ];
return V0 & $FF ! I0<<8;
];      \Try


proc    PlayGame;       \Play one game
[ChOut(0, $0C\FF\);     \clear screen with a form feed
HLine(X0-1, Y0+1);      \draw grid (#)
HLine(X0-1, Y0+3);
VLine(X0+2, Y0);
VLine(X0+6, Y0);

X:= 0;  O:= 0;          \initialize player's bit arrays to empty
loop    [loop   [PMove:= ChIn(1);               \GET PLAYER'S MOVE (X)
                if PMove = $1B\Esc\ then
                        [SetVid(3);  exit];     \restore display and end program
                if PMove = ^0 then quit;
                if PMove>=^1 & PMove<=^9 &      \check for legal move
                   ((X!O) & 1<<(PMove-^1)) = 0 then quit;
                ChOut(0, 7\Bel\);               \beep the dude
                ];
        if PMove # ^0 then
                [DoMove(addr X, PMove-^1, ^X);
                if Won(X) then
                        [Cursor(17, 20);
                        Text(0, "X wins!");
                        quit;
                        ];
                ];
        if Cats then quit;

        I0:= Try(addr O) >>8;                   \GET COMPUTER'S MOVE (O)
        DoMove(addr O, I0, ^O);                 \do best move
        if Won(O) then
                [Cursor(17, 20);
                Text(0, "O wins!");
                quit;
                ];
        if Cats then quit;
        ];
];      \PlayGame


int     CpuReg;
[SetVid(1);                             \set 40x25 text mode
CpuReg:= GetReg;                        \turn off annoying flashing cursor
CpuReg(0):= $0100;                      \ with BIOS interrupt 10h, function 01h
CpuReg(2):= $2000;                      \set cursor type to disappear
SoftInt($10);
loop    [PlayGame;
        Key:= ChIn(1);                  \keep playing games until Esc key is hit
        if Key = $1B\Esc\ then
                [SetVid(3);  exit];     \clear screen & restore normal text mode
        ];
]
```



## Yabasic

In the classic style.

```Yabasic
5 REM Adaptation to Yabasic of the program published in Tim Hartnell's book "Artificial Intelligence: Concepts and Programs", with some minor modifications. 6/2018.
10 REM TICTAC
15 INPUT "English (0), Spanish (other key): " IDIOMA : IF NOT IDIOMA THEN RESTORE 2020 ELSE RESTORE 2010 END IF
20 GOSUB 1180: REM INICIALIZACION
30 REM *** REQUISITOS PREVIOS AL JUEGO ***
40 FOR J = 1 TO 9
50 A(J) = 32
60 NEXT J
70 FOR J = 1 TO 5
80 D(J) = 0
90 NEXT J
100 CCONTADOR = 0
110 R$ = ""
120 GOSUB 1070: REM IMPRESION DEL TABLERO
130 REM ** CICLO PRINCIPAL **
140 GOSUB 540: REM MOVIMIENTO DEL ORDENADOR
150 GOSUB 1070: REM IMPRESION DEL TABLERO
160 GOSUB 870: REM COMPRUEBA LA VICTORIA
170 IF R$ <> ""  GOTO 240
180 GOSUB 980: REM SE ACEPTA EL MOVIMIENTO DE LA PERSONA
190 GOSUB 1070: REM IMPRESION DEL TABLERO
200 GOSUB 870: REM COMPRUEBA LA VICTORIA
210 IF R$ = ""  GOTO 140
220 REM ** FIN DEL CICLO PRINCIPAL **
230 REM *****************************
240 REM FIN DEL JUEGO
250 GOSUB 1070: REM IMPRESION DEL TABLERO
260 PRINT: PRINT
270 IF R$ = "G"  PRINT MENSAJE$(1): BANDERA = -1
280 IF R$ = "P"  PRINT MENSAJE$(2): BANDERA = 1
290 IF R$ = "D"  PRINT MENSAJE$(3): GOTO 430
300 REM ACTUALIZACION DE LA BASE DE DATOS
310 FOR B = 1 TO 5
320 FOR J = 2 TO 9
330 IF M(J) = D(B) GOSUB 370
340 NEXT J
350 NEXT B
360 GOTO 430
370 REM ** REORDENACION DE LOS ELEMENTOS DE LA MATRIZ M **
380 TEMP = M(J + BANDERA)
390 M(J + BANDERA) = M(J)
400 M(J) = TEMP
410 J = 9
420 RETURN
430 PRINT: PRINT
440 PRINT MENSAJE$(4)
450 PRINT: PRINT
460 FOR J = 1 TO 9
470 PRINT M(J), " ";
480 NEXT J
490 PRINT: PRINT
500 PRINT MENSAJE$(5)
510 INPUT A$
520 GOTO 30
530 REM ************************
540 REM MOVIMIENTO DEL ORDENADOR
550 P = ASC("O")
560 X = 0
570 J = 1
580 IF A(W(J)) = A(W(J + 1)) AND A(W(J + 2)) = 32 AND A(W(J)) = P  X = W(J + 2): GOTO 750
590 IF A(W(J)) = A(W(J + 2)) AND A(W(J + 1)) = 32 AND A(W(J)) = P  X = W(J + 1): GOTO 750
600 IF A(W(J + 1)) = A(W(J + 2)) AND A(W(J)) = 32 AND A(W(J + 1)) = P  X = W(J): GOTO 750
610 IF J < 21  J = J + 3: GOTO 580
620 IF P = ASC("O")  P = ASC("X"): GOTO 570
630 REM ** SI NO SE GANA SE BUSCA UN MOVIMIENTO DE BLOQUEO **
640 REM * ENTONCES SE USA LA SIGUIENTE SECCION *
650 J = 1
660 IF A(M(J)) = 32  X = M(J): GOTO 750
670 IF J < 10  J = J + 1: GOTO 660
680 H = 0
690 H = H + 1
700 X = INT(RAN(1) * 9): IF A(X) = 32  GOTO 750
710 IF H < 100  GOTO 690
720 R$ = "D": REM ES SIMPLEMENTE UN DIBUJO
730 RETURN
740 REM *********************
750 REM REALIZA EL MOVIMIENTO
760 A(X) = ASC("O")
770 CCONTADOR = CCONTADOR + 1
780 D(CCONTADOR) = X
790 BANDERA = 0
800 FOR J = 1 TO 9
810 IF A(J) = 32  BANDERA = 1
820 NEXT J
830 IF BANDERA = 0 AND R$ = ""  R$ = "D"
840 REM SI TODAS LAS CASILLAS ESTAN LLENAS Y R$ ESTA VACIO, ENTONCES ES SIMPLEMENTE UN DIBUJO
850 RETURN
860 REM *********************
870 REM COMPRUEBA LA VICTORIA
880 J = 1
890 IF A(W(J)) = 32  J = J + 3
900 IF J > 23  RETURN
910 IF A(W(J)) = A(W(J + 1)) AND A(W(J)) = A(W(J + 2))  GOTO 940
920 IF J < 22  J = J + 3: GOTO 890
930 RETURN
940 IF A(W(J)) = ASC("O")  R$ = "G": REM EL ORDENADOR GANA
950 IF A(W(J)) = ASC("X")  R$ = "P": REM EL ORDENADOR PIERDE
960 RETURN
970 REM ************************
980 REM MOVIMIENTO DE LA PERSONA
990 PRINT: PRINT
1000 PRINT MENSAJE$(6)
1010 PRINT MENSAJE$(7); : INPUT MOVIMIENTO
1020 IF MOVIMIENTO < 1 OR MOVIMIENTO > 9  GOTO 1010
1030 IF A(MOVIMIENTO) <> 32  GOTO 1010
1040 A(MOVIMIENTO) = ASC("X")
1050 RETURN
1060 REM *********************
1070 REM IMPRESION DEL TABLERO
1080 CLEAR SCREEN
1090 PRINT: PRINT: PRINT
1100 PRINT " 1 : 2 : 3        ", CHR$(A(1)), " : ", CHR$(A(2)), " : ", CHR$(A(3))
1110 PRINT "-----------      ------------"
1120 PRINT " 4 : 5 : 6        ", CHR$(A(4)), " : ", CHR$(A(5)), " : ", CHR$(A(6))
1130 PRINT "-----------      ------------"
1140 PRINT " 7 : 8 : 9        ", CHR$(A(7)), " : ", CHR$(A(8)), " : ", CHR$(A(9))
1150 PRINT
1160 RETURN
1170 REM **************
1180 REM INICIALIZACION
1190 CLEAR SCREEN
1200 DIM A(9) : REM TABLERO
1210 DIM M(10) : REM ACCESO A LA BASE DE DATOS
1220 DIM W(24) : REM DATOS DE VICTORIA O BLOQUEO
1230 DIM D(5) : REM ACCESO AL MOVIMIENTO EN EL JUEGO ACTUAL
1235 DIM MENSAJE$(1) : READ M$ : N = TOKEN(M$,MENSAJE$(),",") : RESTORE
1240 REM DATOS DE VICTORIA O BLOQUEO
1250 FOR J = 1 TO 24
1260 READ W(J)
1270 NEXT J
1280 DATA 1, 2, 3, 4, 5, 6, 7, 8, 9
1290 DATA 1, 4, 7, 2, 5, 8, 3, 6, 9
1300 DATA 1, 5, 9, 3, 5, 7
1310 REM BASE INICIAL DE DATOS
1320 FOR J = 1 TO 10
1330 READ M(J)
1340 NEXT J
1350 DATA 2, 6, 8, 4, 7, 3, 1, 9, 5, 2
1360 RETURN
2000 REM MENSAJES EN ESPAÑOL
2010 DATA "YO GANO,TU GANAS,ES SIMPLEMENTE UN DIBUJO,ESTA ES MI PRIORIDAD ACTUALIZADA,PULSE LA TECLA <RETURN> PARA CONTINUAR,REALICE SU MOVIMIENTO,MOVIMIENTO: "
2020 DATA "I WIN,YOU WIN,IT'S JUST A DRAWING,THIS IS MY PRIORITY UPDATE,PRESS <RETURN> TO CONTINUE,TO MAKE YOUR MOVE,MOVEMENT: "

```


{{omit from|GUISS}}

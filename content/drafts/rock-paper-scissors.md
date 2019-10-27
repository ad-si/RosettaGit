+++
title = "Rock-paper-scissors"
description = ""
date = 2019-09-05T22:23:30Z
aliases = []
[extra]
id = 10020
[taxonomies]
categories = []
tags = []
+++

{{task|Games}}

;Task:
Implement the classic children's game [[wp:Rock-paper-scissors|Rock-paper-scissors]], as well as a simple predictive   '''AI'''   (<u>a</u>rtificial <u>i</u>ntelligence)   player.

Rock Paper Scissors is a two player game. 

Each player chooses one of rock, paper or scissors, without knowing the other player's choice. 

The winner is decided by a set of rules:

:::*   Rock beats scissors
:::*   Scissors beat paper
:::*   Paper beats rock



If both players choose the same thing, there is no winner for that round.

For this task, the computer will be one of the players. 

The operator will select Rock, Paper or Scissors and the computer will keep a record of the choice frequency, and use that information to make a [[Probabilistic choice|weighted random choice]] in an attempt to defeat its opponent.


;Extra credit:
Support additional choices   [[wp:Rock-paper-scissors#Additional_weapons|additional weapons]].





## Ada



```Ada
with Ada.Text_IO; with Ada.Numerics.Float_Random;

procedure Rock_Paper_Scissors is

   package Rand renames Ada.Numerics.Float_Random;
   Gen: Rand.Generator;

   type Choice is (Rock, Paper, Scissors);

   Cnt: array (Choice) of Natural := (1, 1, 1);
     -- for the initialization: pretend that each of Rock, Paper, 
     -- and Scissors, has been played once by the human
     -- else the first computer choice would be deterministic

   function Computer_Choice return Choice is
      Random_Number: Natural :=
        Integer(Rand.Random(Gen)
          * (Float(Cnt(Rock)) + Float(Cnt(Paper)) + Float(Cnt(Scissors))));
   begin
      if Random_Number < Cnt(Rock) then
         -- guess the human will choose Rock
         return Paper;
      elsif Random_Number - Cnt(Rock) < Cnt(Paper) then
         -- guess the human will choose Paper
         return Scissors;
      else -- guess the human will choose Scissors
         return Rock;
      end if;
   end Computer_Choice;

   Finish_The_Game: exception;

   function Human_Choice return Choice is
      Done: Boolean := False;
      T: constant String
        := "enter ""r"" for Rock, ""p"" for Paper, or ""s"" for Scissors""!";
      U: constant String
        := "or enter ""q"" to Quit the game";
      Result: Choice;
   begin
      Ada.Text_IO.Put_Line(T);
      Ada.Text_IO.Put_Line(U);
      while not Done loop
         Done := True;
         declare
            S: String := Ada.Text_IO.Get_Line;
         begin
            if S="r" or S="R" then
               Result := Rock;
            elsif S="p" or S = "P" then
               Result := Paper;
            elsif S="s" or S="S" then
               Result := Scissors;
            elsif S="q" or S="Q" then
               raise Finish_The_Game;
            else
               Done := False;
            end if;
         end;
      end loop;
      return Result;
   end Human_Choice;

   type Result is (Human_Wins, Draw, Computer_Wins);

   function "<" (X, Y: Choice) return Boolean is
      -- X < Y if X looses against Y
   begin
      case X is
         when Rock => return  (Y = Paper);
         when Paper => return (Y = Scissors);
         when Scissors => return (Y = Rock);
      end case;
   end "<";

   Score: array(Result) of Natural := (0, 0, 0);

   C,H: Choice;

   Res: Result;

begin
   -- play the game
   loop
      C := Computer_Choice;  -- the computer makes its choice first
      H := Human_Choice;     -- now ask the player for his/her choice
      Cnt(H) := Cnt(H) + 1;  -- update the counts for the AI
      if C < H then
         Res := Human_Wins;
      elsif H < C then
         Res := Computer_Wins;
      else
         Res := Draw;
      end if;
      Ada.Text_IO.Put_Line("COMPUTER'S CHOICE: " & Choice'Image(C)
                             & "       RESULT: " & Result'Image(Res));
      Ada.Text_IO.New_Line;
      Score(Res) := Score(Res) + 1;
   end loop;

exception
   when Finish_The_Game =>
      Ada.Text_IO.New_Line;
      for R in Score'Range loop
         Ada.Text_IO.Put_Line(Result'Image(R) & Natural'Image(Score(R)));
      end loop;
end Rock_Paper_Scissors;
```


First and last few lines of the output of a game, where the human did permanently choose Rock:


```txt
./rock_paper_scissors 
enter "r" for Rock, "p" for Paper, or "s" for Scissors"!
or enter "q" to Quit the game
r
COMPUTER'S CHOICE: SCISSORS       RESULT: HUMAN_WINS

enter "r" for Rock, "p" for Paper, or "s" for Scissors"!
or enter "q" to Quit the game
r
COMPUTER'S CHOICE: ROCK       RESULT: DRAW

enter "r" for Rock, "p" for Paper, or "s" for Scissors"!
or enter "q" to Quit the game
r
COMPUTER'S CHOICE: SCISSORS       RESULT: HUMAN_WINS

enter "r" for Rock, "p" for Paper, or "s" for Scissors"!
or enter "q" to Quit the game
r
COMPUTER'S CHOICE: ROCK       RESULT: DRAW


[...]


enter "r" for Rock, "p" for Paper, or "s" for Scissors"!
or enter "q" to Quit the game
r
COMPUTER'S CHOICE: ROCK       RESULT: DRAW

enter "r" for Rock, "p" for Paper, or "s" for Scissors"!
or enter "q" to Quit the game
r
COMPUTER'S CHOICE: PAPER       RESULT: COMPUTER_WINS

enter "r" for Rock, "p" for Paper, or "s" for Scissors"!
or enter "q" to Quit the game
q

HUMAN_WINS 2
DRAW 5
COMPUTER_WINS 21
```



## Aime


```aime
text
computer_play(record plays, record beats)
{
    integer a, c, total;
    text s;

    total = plays["rock"] + plays["paper"] + plays["scissors"];
    a = drand(total - 1);
    for (s, c in plays) {
        if (a < c) {
            break;
        }
        a -= c;
    }

    beats[s];
}

integer
main(void)
{
    integer computer, human;
    record beats, plays;
    file f;
    text s;

    computer = human = 0;

    f.stdin;

    beats["rock"] = "paper";
    beats["paper"] = "scissors";
    beats["scissors"] = "rock";

    plays["rock"] = 1;
    plays["paper"] = 1;
    plays["scissors"] = 1;

    while (1) {
        o_text("Your choice [rock/paper/scissors]:\n");
        if (f.line(s) == -1) {
            break;
        }

        if (!plays.key(s)) {
            o_text("Invalid choice, try again\n");
        } else {
            text c;

            c = computer_play(plays, beats);

            o_form("Human: ~, Computer: ~\n", s, c);

            if (s == c) {
                o_text("Draw\n");
            } elif (c == beats[s]) {
                computer += 1;
                o_text("Computer wins\n");
            } else {
                human += 1;
                o_text("Human wins\n");
            }

            plays.up(s);

            o_form("Score: Human: ~, Computer: ~\n", human, computer);
        }
    }

    return 0;
}
```



## ALGOL 68


```algol68
BEGIN
    # rock/paper/scissors game                                             #
    # counts of the number of times the player has chosen each move        #
    # we initialise each to 1 so that the total isn't zero when we are     #
    # choosing the computer's first move (as in the Ada version)           #
    INT    r count := 1;
    INT    p count := 1;
    INT    s count := 1;
    # counts of how many games the player and computer have won            #
    INT    player count   := 0;
    INT    computer count := 0;
    print( ( "rock/paper/scissors", newline, newline ) );
    WHILE
        CHAR player move;
        # get the players move - r => rock, p => paper, s => scissors      #
        #                        q => quit                                 #
        WHILE
            print( ( "Please enter your move (r/p/s) or q to quit: " ) );
            read( ( player move, newline ) );
            (   player move /= "r"
            AND player move /= "p"
            AND player move /= "s"
            AND player move /= "q"
            )
        DO
            print( ( "Unrecognised move", newline ) )
        OD;
        # continue playing until the player chooses quit                   #
        player move /= "q"
    DO
        # decide the computer's move based on the player's history         #
        CHAR  computer move;
        INT   move count     = r count + p count + s count;
        # predict player will play rock  if the random number              #
        #     is in the range 0 .. rock / total                            #
        # predict player will play paper if the random number              #
        #     is in the range rock / total .. ( rock + paper ) / total     #
        # predict player will play scissors otherwise                      #
        REAL  r limit        = r count / move count;
        REAL  p limit        = r limit + ( p count / move count );
        REAL  random move    = next random;
        IF    random move < r limit THEN
            # we predict the player will choose rock - we choose paper     #
            computer move := "p"
        ELIF  random move < p limit THEN
            # we predict the player will choose paper - we choose scissors #
            computer move := "s"
        ELSE
            # we predict the player will choose scissors - we choose rock  #
            computer move := "r"
        FI;
        print( ( "You chose: " + player   move, newline ) );
        print( ( "I   chose: " + computer move, newline ) );
        IF   player move = computer move THEN
            # both players chose the same - draw                           #
            print( ( "We draw", newline ) )
        ELSE
            # players chose different moves - there is a winner            #
            IF ( player move = "r" AND computer move = "s" )
            OR ( player move = "p" AND computer move = "r" )
            OR ( player move = "s" AND computer move = "p" )
            THEN
                player count +:= 1;
                print( ( "You win", newline ) )
            ELSE
                computer count +:= 1;
                print( ( "I win",   newline ) )
            FI;
            print( ( "You won: "
                   , whole( player count  , 0 )
                   , ", I won: "
                   , whole( computer count, 0 )
                   , newline
                   )
                 )
        FI;
        IF   player move = "r" THEN
            # player chose rock                                            #
            r count +:= 1
        ELIF player move = "p" THEN
            # player chose paper                                           #
            p count +:= 1
        ELSE
            # player chose scissors                                        #
            s count +:= 1
        FI
    OD;
    print( ( "Thanks for a most enjoyable game", newline ) )
END
```



## AutoHotkey


```AHK
DllCall("AllocConsole")
Write("Welcome to Rock-Paper-Scissors`nMake a choice: ")

cR := cP := cS := 0 ; user choice count
userChoice := Read()
Write("My choice: " . cpuChoice := MakeChoice(1, 1, 1))

Loop
{
	Write(DecideWinner(userChoice, cpuChoice) . "`nMake A Choice: ")
	cR += SubStr(userChoice, 1, 1) = "r", cP += InStr(userChoice, "P"), cS += InStr(userChoice, "S")
	userChoice := Read()
	Write("My Choice: " . cpuChoice := MakeChoice(cR, cP, cS))
}

MakeChoice(cR, cP, cS){
	; parameters are number of times user has chosen each item
	total := cR + cP + cS
	
	Random, rand, 0.0, 1.0
	if (rand >= 0 and rand <= cR / total)
		return "Paper"
	else if (rand > cR / total and rand <= (cR + cP) / total)
		return "Scissors"
	else
		return "Rock"
}

DecideWinner(user, cpu){
	user := SubStr(user, 1, 1), cpu := SubStr(cpu, 1, 1)
	if (user = cpu)
		return "`nTie!"
	else if (user = "r" and cpu = "s") or (user = "p" and cpu = "r") or (user = "s" and cpu = "p")
		return "`nYou Win!"
	else
		return "`nI Win!"
}

Read(){
	FileReadLine, a, CONIN$, 1
	return a
}
Write(txt){
	FileAppend, % txt, CONOUT$
}
```



## AutoIt


I´ve created a GUI to play and show results, no Console Input


```autoit

RPS()

Func RPS()
	Local $ai_Played_games[4]
	$ai_Played_games[0] = 3
	For $I = 1 To 3
		$ai_Played_games[$I] = 1
	Next
	$RPS = GUICreate("Rock Paper Scissors", 338, 108, 292, 248)
	$Rock = GUICtrlCreateButton("Rock", 8, 8, 113, 25, 131072)
	$Paper = GUICtrlCreateButton("Paper", 8, 40, 113, 25, 131072)
	$Scissors = GUICtrlCreateButton("Scissors", 8, 72, 113, 25, 131072)
	$Label1 = GUICtrlCreateLabel("W:", 136, 8, 18, 17)
	$Wins = GUICtrlCreateLabel("0", 160, 8, 36, 17)
	$Label3 = GUICtrlCreateLabel("L:", 208, 8, 13, 17)
	$Looses = GUICtrlCreateLabel("0", 224, 8, 36, 17)
	$Label5 = GUICtrlCreateLabel("D:", 272, 8, 15, 17)
	$Deuce = GUICtrlCreateLabel("0", 296, 8, 36, 17)
	$Displaybutton = GUICtrlCreateButton("", 136, 48, 193, 49, 131072)
	GUICtrlSetState($ai_Played_games, 128)
	GUISetState(@SW_SHOW)
	While 1
		$nMsg = GUIGetMsg()
		Switch $nMsg
			Case -3
				Exit
			Case $Rock
				$Ret = _RPS_Eval(1, $ai_Played_games)
				GUICtrlSetData($Displaybutton, $Ret)
				If $Ret = "Deuce" Then
					GUICtrlSetData($Deuce, Guictrlread($Deuce)+1)
				Elseif $Ret = "You Loose" Then
					GUICtrlSetData($Looses, Guictrlread($Looses)+1)
				Elseif $Ret = "You Win" Then
					GUICtrlSetData($Wins, Guictrlread($Wins)+1)
				EndIf
			Case $Paper
				$Ret = _RPS_Eval(2, $ai_Played_games)
				GUICtrlSetData($Displaybutton, $Ret)
				If $Ret = "Deuce" Then
					GUICtrlSetData($Deuce, Guictrlread($Deuce)+1)
				Elseif $Ret = "You Loose" Then
					GUICtrlSetData($Looses, Guictrlread($Looses)+1)
				Elseif $Ret = "You Win" Then
					GUICtrlSetData($Wins, Guictrlread($Wins)+1)
				EndIf
			Case $Scissors
				$Ret = _RPS_Eval(3, $ai_Played_games)
				GUICtrlSetData($Displaybutton, $Ret)
				If $Ret = "Deuce" Then
					GUICtrlSetData($Deuce, Guictrlread($Deuce)+1)
				Elseif $Ret = "You Loose" Then
					GUICtrlSetData($Looses, Guictrlread($Looses)+1)
				Elseif $Ret = "You Win" Then
					GUICtrlSetData($Wins, Guictrlread($Wins)+1)
				EndIf
		EndSwitch
	WEnd

EndFunc   ;==>RPS

Func _RPS_Eval($i_Player_Choose, $ai_Played_games)
	Local $i_choice = 1
	$i_rnd = Random(1, 1000, 1)
	$i_choose_1 = ($ai_Played_games[1] / $ai_Played_games[0] * 1000)
	$i_choose_2 = ($ai_Played_games[2] / $ai_Played_games[0] * 1000)
	$i_choose_3 = ($ai_Played_games[3] / $ai_Played_games[0] * 1000)
	If $i_rnd < $i_choose_1 Then
		$i_choice = 2
	ElseIf $i_rnd < $i_choose_1 + $i_choose_2 And $i_rnd > $i_choose_1 Then
		$i_choice = 3
	ElseIf $i_rnd < $i_choose_1 + $i_choose_2 + $i_choose_3 And $i_rnd > $i_choose_1 + $i_choose_2 Then
		$i_choice = 1
	EndIf
	$ai_Played_games[0] += 1
	If $i_Player_Choose = 1 Then
		$ai_Played_games[1] += 1
		If $i_choice = 1 Then Return "Deuce"
		If $i_choice = 2 Then Return "You Loose"
		If $i_choice = 3 Then Return "You Win"
	ElseIf $i_Player_Choose = 2 Then
		$ai_Played_games[2] += 1
		If $i_choice = 2 Then Return "Deuce"
		If $i_choice = 3 Then Return "You Loose"
		If $i_choice = 1 Then Return "You Win"
	ElseIf $i_Player_Choose = 3 Then
		$ai_Played_games[3] += 1
		If $i_choice = 3 Then Return "Deuce"
		If $i_choice = 1 Then Return "You Loose"
		If $i_choice = 2 Then Return "You Win"
	EndIf
EndFunc   ;==>_RPS_Eval


```



## BASIC

{{works with|QBasic}}


```qbasic
DIM pPLchoice(1 TO 3) AS INTEGER, pCMchoice(1 TO 3) AS INTEGER
DIM choices(1 TO 3) AS STRING
DIM playerwins(1 TO 3) AS INTEGER
DIM playerchoice AS INTEGER, compchoice AS INTEGER
DIM playerwon AS INTEGER, compwon AS INTEGER, tie AS INTEGER
DIM tmp AS INTEGER

' Do it this way for QBasic; FreeBASIC supports direct array assignment.
DATA "rock", "paper", "scissors"
FOR tmp = 1 to 3
    READ choices(tmp)
NEXT
DATA 3, 1, 2
FOR tmp = 1 to 3
    READ playerwins(tmp)
NEXT

RANDOMIZE TIMER

DO
    ' Computer chooses first to ensure there's no "cheating".
    compchoice = INT(RND * (pPLchoice(1) + pPLchoice(2) + pPLchoice(3) + 3))
    SELECT CASE compchoice
        CASE 0 to (pPLchoice(1))
            ' Player past choice: rock; choose paper.
            compchoice = 2
        CASE (pPLchoice(1) + 1) TO (pPLchoice(1) + pPLchoice(2) + 1)
            ' Player past choice: paper; choose scissors.
            compchoice = 3
        CASE (pPLchoice(1) + pPLchoice(2) + 2) TO (pPLchoice(1) + pPLchoice(2) + pPLchoice(3) + 2)
            ' Player past choice: scissors; choose rock.
            compchoice = 1
    END SELECT

    PRINT "Rock, paper, or scissors ";
    DO
        PRINT "[1 = rock, 2 = paper, 3 = scissors, 0 to quit]";
        INPUT playerchoice
    LOOP WHILE (playerchoice < 0) OR (playerchoice > 3)

    IF 0 = playerchoice THEN EXIT DO

    pCMchoice(compchoice) = pCMchoice(compchoice) + 1
    pPLchoice(playerchoice) = pPLchoice(playerchoice) + 1
    PRINT "You chose "; choices(playerchoice); " and I chose "; choices(compchoice); ". ";
    IF (playerchoice) = compchoice THEN
        PRINT "Tie!"
        tie = tie + 1
    ELSEIF (compchoice) = playerwins(playerchoice) THEN
        PRINT "You won!"
        playerwon = playerwon + 1
    ELSE
        PRINT "I won!"
        compwon = compwon + 1
    END IF
LOOP

PRINT "Some useless statistics:"
PRINT "You won "; STR$(playerwon); " times, and I won "; STR$(compwon); " times; "; STR$(tie); " ties."
PRINT             ,   choices(1),   choices(2),   choices(3)
PRINT "You chose:", pPLchoice(1), pPLchoice(2), pPLchoice(3)
PRINT "  I chose:", pCMchoice(1), pCMchoice(2), pCMchoice(3)
```


A sample game:
 $ ./rokpprscr
 Rock, paper, or scissors [1 = rock, 2 = paper, 3 = scissors, 0 to quit]? 1
 You chose rock and I chose paper. I won!
 Rock, paper, or scissors [1 = rock, 2 = paper, 3 = scissors, 0 to quit]? 1
 You chose rock and I chose scissors. You won!
 
 [... 56 more times choosing "rock"...]
 
 Rock, paper, or scissors [1 = rock, 2 = paper, 3 = scissors, 0 to quit]? 1
 You chose rock and I chose paper. I won!
 Rock, paper, or scissors [1 = rock, 2 = paper, 3 = scissors, 0 to quit]? 1
 You chose rock and I chose paper. I won!
 Rock, paper, or scissors [1 = rock, 2 = paper, 3 = scissors, 0 to quit]? 3
 You chose scissors and I chose paper. You won!
 Rock, paper, or scissors [1 = rock, 2 = paper, 3 = scissors, 0 to quit]? 
 Some useless statistics:
 You won  5 times, and I won  54 times;  2 ties.
               rock          paper         scissors
 You chose:     60            0             1 
   I chose:     2             55            4


## Bash


```bash
#!/bin/bash
echo "What will you choose? [rock/paper/scissors]"
read response
aiThought=$(echo $[ 1 + $[ RANDOM % 3  ]])
case $aiThought in
	1) 	 aiResponse="rock"   ;;
	2) 	 aiResponse="paper" ;;
	3)  	 aiResponse="scissors"  ;;
esac
echo "AI - $aiResponse"
responses="$response$aiResponse"
case $responses in
	rockrock)  isTie=1  ;;
	rockpaper)  playerWon=0  ;;
	rockscissors)  playerWon=1  ;;
	paperrock)  playerWon=1  ;;
	paperpaper)  isTie=1  ;;
	paperscissors)  playerWon=0  ;;
	scissorsrock)  playerWon=0  ;;
	scissorspaper)  playerWon=1  ;;
	scissorsscissors)  isTie=1  ;;
esac
if [[ $isTie == 1 ]] ; then echo "It's a tie!" && exit 1 ; fi
if [[ $playerWon == 0 ]] ; then echo "Sorry, $aiResponse beats $response , try again.." && exit 1 ; fi
if [[ $playerWon == 1 ]] ; then echo "Good job, $response beats $aiResponse!" && exit 1 ; fi
```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

set choice1=rock
set choice2=paper
set choice3=scissors
set freq1=0
set freq2=0
set freq3=0
set games=0
set won=0
set lost=0
set tie=0

:start

cls
echo Games - %games% : Won - %won% : Lost - %lost% : Ties - %tie%
choice /c RPS /n /m "[R]ock, [P]aper or [S]cissors? "

set /a choicequant+=1
set choice=%errorlevel%
set /a rocklimit=100 / %choicequant% * %freq3%
set /a scissorslimit=100 - (100 / %choicequant% * %freq2%)
set /a randchoice=%random% %% 100 + 1
set compchoice=2
if %randchoice% geq %scissorslimit% set compchoice=3
if %randchoice% leq %rocklimit% set compchoice=1

cls
echo Player: !choice%choice%! ^|vs^| Computer: !choice%compchoice%!
goto %compchoice%

:1

if %choice%==1 goto tie
if %choice%==2 goto win
if %choice%==3 goto loss

:2

if %choice%==1 goto loss
if %choice%==2 goto tie
if %choice%==3 goto win

:3

if %choice%==1 goto win
if %choice%==2 goto loss
if %choice%==3 goto tie

:win
set /a won+=1
echo Player wins^!
goto end

:loss
set /a lost+=1
echo Computer Wins^!
goto end

:tie
set /a tie+=1
echo Tie^!
goto end

:end
set /a games+=1
set /a freq%choice%+=1
pause
goto start

```



## BBC BASIC


```bbcbasic
PRINT"Welcome to the game of rock-paper-scissors"
PRINT "Each player guesses one of these three, and reveals it at the same time."
PRINT "Rock blunts scissors, which cut paper, which wraps stone."
PRINT "If both players choose the same, it is a draw!"
PRINT "When you've had enough, choose Q."
DIM rps%(2),g$(3)
g$()="rock","paper","scissors"
total%=0
draw%=0
pwin%=0
cwin%=0
c%=RND(3)
PRINT"What is your move (press R, P, or S)?"
REPEAT
  REPEAT q$=GET$ UNTIL INSTR("RPSrpsQq",q$)>0
  g%=(INSTR("RrPpSsQq",q$)-1) DIV 2
  IF g%>2 THEN PROCsummarise:END
  total%+=1
  rps%(g%)+=1
  PRINT"You chose ";g$(g%);" and I chose ";g$(c%);
  CASE g%-c% OF
    WHEN 0:
      PRINT ". It's a draw"
      draw%+=1
    WHEN 1,-2:
      PRINT ". You win!"
      pwin%+=1
    WHEN -1,2:
      PRINT ". I win!"
      cwin%+=1
    ENDCASE
  c%=FNmove(rps%(),total%)
UNTIL FALSE
END
:
DEFPROCsummarise
PRINT "You won ";pwin%;", and I won ";cwin%;". There were ";draw%;" draws"
PRINT "Thanks for playing!"
ENDPROC
:
DEFFNmove(p%(),t%)
LOCAL r%
r%=RND(total%)
IF r%<=p%(0) THEN =1
IF r%<=p%(0)+p%(1) THEN =2
=0
```


Sample output:

```txt
Welcome to the game of rock-paper-scissors
Each player guesses one of these three, and reveals it at the same time.
Rock blunts scissors, which cut paper, which wraps stone.
If both players choose the same, it is a draw!
When you've had enough, choose Q.
What is your move (press R, P, or S)?
You chose rock and I chose paper. I win!
You chose scissors and I chose paper. You win!
You chose scissors and I chose paper. You win!
You chose scissors and I chose paper. You win!
You chose scissors and I chose paper. You win!
You chose scissors and I chose rock. I win!
You chose paper and I chose paper. It's a draw
You chose paper and I chose rock. You win!
You chose paper and I chose rock. You win!
You chose paper and I chose paper. It's a draw
You chose paper and I chose paper. It's a draw
You chose paper and I chose rock. You win!
You chose scissors and I chose rock. I win!
You chose scissors and I chose scissors. It's a draw
You chose scissors and I chose rock. I win!
You chose scissors and I chose scissors. It's a draw
You chose scissors and I chose rock. I win!
You won 7, and I won 5. There were 5 draws
Thanks for playing!
```



## C


```C

#include <stdio.h>
#include <stdlib.h>
#define LEN 3 

/* pick a random index from 0 to n-1, according to probablities listed
   in p[] which is assumed to have a sum of 1. The values in the probablity
   list matters up to the point where the sum goes over 1 */
int rand_idx(double *p, int n)
{
	double s = rand() / (RAND_MAX + 1.0);
	int i;
	for (i = 0; i < n - 1 && (s -= p[i]) >= 0; i++);
	return i;
}

int main()
{
	int user_action, my_action;
	int user_rec[] = {0, 0, 0};
	const char *names[] = { "Rock", "Paper", "Scissors" };
	char str[2];
	const char *winner[] = { "We tied.", "Meself winned.", "You win." };
	double  p[LEN] = { 1./3, 1./3, 1./3 };
 
	while (1) {
		my_action = rand_idx(p,LEN);
 
		printf("\nYour choice [1-3]:\n"
			"  1. Rock\n  2. Paper\n  3. Scissors\n> ");
 
		/* scanf is a terrible way to do input.  should use stty and keystrokes */
		if (!scanf("%d", &user_action)) {
			scanf("%1s", str);
			if (*str == 'q') {
				printf("Your choices [rock : %d , paper :  %d , scissors %d] ",user_rec[0],user_rec[1], user_rec[2]); 
				return 0;
			}
			continue;
		}
		user_action --;
		if (user_action > 2 || user_action < 0) {
			printf("invalid choice; again\n");
			continue;
		}
		printf("You chose %s; I chose %s. %s\n",
			names[user_action], names[my_action],
			winner[(my_action - user_action + 3) % 3]);
 
		user_rec[user_action]++;
	}
}

```


Here's another code: (Does it using a while loop)

```C

#include <stdio.h> // Standard IO
#include <stdlib.h> // other stuff
#include <time.h>
#include <string.h>

//This should add weighted random function to "The Elite Noob"'s code, stolen from above code because it does calculation so well
//closest I could make it to original but without pointless attempt to make code look smaller than above code by putting code on the same lines
 
int rand_i(int n)
{
	int rand_max = RAND_MAX - (RAND_MAX % n);
	int ret;
	while ((ret = rand()) >= rand_max);
	return ret/(rand_max / n);
}
 
int weighed_rand(int *tbl, int len)
{
	int i, sum, r;
	for (i = 0, sum = 0; i < len; sum += tbl[i++]);
	if (!sum) return rand_i(len);
 
	r = rand_i(sum) + 1;
	for (i = 0; i < len && (r -= tbl[i]) > 0; i++);
	return i;
}



int main(int argc, const char *argv[])
{
	char umove[10], cmove[10], line[255];
	int user, comp;
	int tbl[]={0,0,0};
	int tbllen=3;
	printf("Hello, Welcome to rock-paper-scissors\nBy The Elite Noob\n");
mainloop:
	while(1)
	{ // infinite loop :)
		printf("\n\nPlease type in 1 for Rock, 2 For Paper, 3 for Scissors, 4 to quit\n");
		srand(time(NULL));
		comp = (weighed_rand(tbl, tbllen) + 1) % 3;
		fgets(line, sizeof(line), stdin);	
		while(sscanf(line, "%d", &user) != 1) //1 match of defined specifier on input line
		{ 
  			printf("You have not entered an integer.\n");
			fgets(line, sizeof(line), stdin);
		}				
		if( (user > 4) || (user < 1) )
		{
			printf("Please enter a valid number!\n");
			continue;
		}
		switch (comp)
		{
			case 1 :
				strcpy(cmove, "Rock");
				break;
			case 2 :
				strcpy(cmove, "Paper");
				break;
			case 3 :
				strcpy(cmove, "Scissors");
				break;
			default :
				printf("Computer Error, set comp=1\n");
				comp=1;
				strcpy(cmove, "Rock");
				break;
		}
		switch (user)
		{
			case 1 :
				strcpy(umove, "Rock");
				break;
			case 2 :
				strcpy(umove, "Paper");
				break;
			case 3 :
				strcpy(umove, "Scissors");
				break;
			case 4 :
				printf("Goodbye! Thanks for playing!\n");
				return 0;
			default :
				printf("Error, user number not between 1-4 exiting...");
				goto mainloop;
		}
		if( (user+1)%3 == comp )
		{
			printf("Comp Played: %s\nYou Played: %s\nSorry, You Lost!\n", cmove, umove);
		}	
		else if(comp == user)
		{
			printf("Comp Played: %s\nYou Played: %s\nYou Tied :p\n", cmove, umove);
		}
		else
		{
			printf("Comp Played: %s\nYou Played: %s\nYay, You Won!\n", cmove, umove);
		}
		tbl[user-1]++;
	}
}

```



## C++

Version using Additional Weapons


```cpp

#include <windows.h>
#include <iostream>
#include <string>

//-------------------------------------------------------------------------------
using namespace std;

//-------------------------------------------------------------------------------
enum choices { ROCK, SPOCK, PAPER, LIZARD, SCISSORS, MX_C };
enum indexes { PLAYER, COMPUTER, DRAW };

//-------------------------------------------------------------------------------
class stats
{
public:
    stats() : _draw( 0 )
    {
        ZeroMemory( _moves, sizeof( _moves ) );
	ZeroMemory( _win, sizeof( _win ) );
    }
    void draw()		        { _draw++; }
    void win( int p )	        { _win[p]++; }
    void move( int p, int m )   { _moves[p][m]++; }
    int getMove( int p, int m ) { return _moves[p][m]; }
    string format( int a )
    {
	char t[32];
	wsprintf( t, "%.3d", a );
	string d( t );
	return d;
    }

    void print()
    {
        string  d = format( _draw ),
	       pw = format( _win[PLAYER] ),		cw = format( _win[COMPUTER] ),
	       pr = format( _moves[PLAYER][ROCK] ),	cr = format( _moves[COMPUTER][ROCK] ),
               pp = format( _moves[PLAYER][PAPER] ),	cp = format( _moves[COMPUTER][PAPER] ),
	       ps = format( _moves[PLAYER][SCISSORS] ), cs = format( _moves[COMPUTER][SCISSORS] ),
	       pl = format( _moves[PLAYER][LIZARD] ),	cl = format( _moves[COMPUTER][LIZARD] ),
	       pk = format( _moves[PLAYER][SPOCK] ),	ck = format( _moves[COMPUTER][SPOCK] );

	system( "cls" );
	cout << endl;
	cout << "+----------+-------+--------+--------+---------+----------+--------+---------+" << endl;
	cout << "|          |  WON  |  DRAW  |  ROCK  |  PAPER  | SCISSORS | LIZARD |  SPOCK  |" << endl;
	cout << "+----------+-------+--------+--------+---------+----------+--------+---------+" << endl;
	cout << "|  PLAYER  |  "  << pw << "  |        |   " << pr << "  |   " << pp << "   |   " << ps << "    |  " << pl << "   |   " << pk << "   |" << endl;
	cout << "+----------+-------+   " << d << "  +--------+---------+----------+--------+---------+" << endl;
	cout << "| COMPUTER |  "  << cw << "  |        |   " << cr << "  |   " << cp << "   |   " << cs << "    |  " << cl << "   |   " << ck << "   |" << endl;
	cout << "+----------+-------+--------+--------+---------+----------+--------+---------+" << endl;
	cout << endl << endl;

	system( "pause" );

    }

private:
    int _moves[2][MX_C], _win[2], _draw;
};
//-------------------------------------------------------------------------------
class rps
{
private:
    int makeMove()
    {
	int total = 0, r, s;
	for( int i = 0; i < MX_C; total += statistics.getMove( PLAYER, i++ ) );
	r = rand() % total;

	for( int i = ROCK; i < SCISSORS; i++ )
	{
	    s = statistics.getMove( PLAYER, i );
	    if( r < s ) return ( i + 1 );
	    r -= s;
	}

	return ROCK;
    }

    void printMove( int p, int m )
    {
	if( p == COMPUTER ) cout << "My move: ";
	else cout << "Your move: ";

	switch( m )
	{
	    case ROCK: cout << "ROCK\n"; break;
	    case PAPER: cout << "PAPER\n"; break;
	    case SCISSORS: cout << "SCISSORS\n"; break;
	    case LIZARD: cout << "LIZARD\n"; break;
	    case SPOCK: cout << "SPOCK\n";
	}
    }

public:
    rps()
    {
	checker[ROCK][ROCK] = 2; checker[ROCK][PAPER] = 1; checker[ROCK][SCISSORS] = 0; checker[ROCK][LIZARD] = 0; checker[ROCK][SPOCK] = 1;
	checker[PAPER][ROCK] = 0; checker[PAPER][PAPER] = 2; checker[PAPER][SCISSORS] = 1; checker[PAPER][LIZARD] = 1; checker[PAPER][SPOCK] = 0;
	checker[SCISSORS][ROCK] = 1; checker[SCISSORS][PAPER] = 0; checker[SCISSORS][SCISSORS] = 2; checker[SCISSORS][LIZARD] = 0; checker[SCISSORS][SPOCK] = 1;
	checker[LIZARD][ROCK] = 1; checker[LIZARD][PAPER] = 0; checker[LIZARD][SCISSORS] = 1; checker[LIZARD][LIZARD] = 2; checker[LIZARD][SPOCK] = 0;
	checker[SPOCK][ROCK] = 0; checker[SPOCK][PAPER] = 1; checker[SPOCK][SCISSORS] = 0; checker[SPOCK][LIZARD] = 1; checker[SPOCK][SPOCK] = 2;
    }
    void play()
    {
	int p, r, m;
	while( true )
	{
	    cout << "What is your move (1)ROCK (2)SPOCK (3)PAPER (4)LIZARD (5)SCISSORS (0)Quit ? ";
	    cin >> p;
	    if( !p || p < 0 ) break;
	    if( p > 0 && p < 6 )
	    {
		p--;
		cout << endl;
		printMove( PLAYER, p );
		statistics.move( PLAYER, p );

		m = makeMove();
		statistics.move( COMPUTER, m );
		printMove( COMPUTER, m );

		r = checker[p][m];
		switch( r )
		{
		    case DRAW: 
		        cout << endl << "DRAW!" << endl << endl; 
		        statistics.draw();
		    break;
		    case COMPUTER: 
			cout << endl << "I WIN!" << endl << endl;  
			statistics.win( COMPUTER );
		    break;
		    case PLAYER: 
			cout << endl << "YOU WIN!" << endl << endl; 
			statistics.win( PLAYER );

		}
		system( "pause" );
	    }
	    system( "cls" );
	}
	statistics.print();
    }

private:
    stats statistics;
    int checker[MX_C][MX_C];
};
//-------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    rps game;
    game.play();
    return 0;
}
//-------------------------------------------------------------------------------

```


Sample output: 

```txt

What is your move (1)ROCK (2)SPOCK (3)PAPER (4)LIZARD (5)SCISSORS (0)Quit ? 1

Your move: ROCK
My move: PAPER

I WIN!

What is your move (1)ROCK (2)SPOCK (3)PAPER (4)LIZARD (5)SCISSORS (0)Quit ? 3

Your move: PAPER
My move: SPOCK

YOU WIN!

What is your move (1)ROCK (2)SPOCK (3)PAPER (4)LIZARD (5)SCISSORS (0)Quit ? 1

Your move: SPOCK
My move: LIZARD

I WIN!

[...]

+----------+-------+--------+--------+---------+----------+--------+---------+
|          |  WON  |  DRAW  |  ROCK  |  PAPER  | SCISSORS | LIZARD |  SPOCK  |
+----------+-------+--------+--------+---------+----------+--------+---------+
|  PLAYER  |  001  |        |   003  |   002   |   000    |  000   |   009   |
+----------+-------+   005  +--------+---------+----------+--------+---------+
| COMPUTER |  008  |        |   000  |   005   |   000    |  001   |   008   |
+----------+-------+--------+--------+---------+----------+--------+---------+


```



## C sharp


```c sharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace RockPaperScissors
{
    class Program
    {
        static void Main(string[] args)
        {
            // There is no limit on the amount of weapons supported by RPSGame. Matchups are calculated depending on the order.
            var rps = new RPSGame("scissors", "paper", "rock", "lizard", "spock");

            int wins = 0, losses = 0, draws = 0;

            while (true)
            {
                Console.WriteLine("Make your move: " + string.Join(", ", rps.Weapons) + ", quit");

                string weapon = Console.ReadLine().Trim().ToLower();

                if (weapon == "quit")
                    break;

                if (!rps.Weapons.Contains(weapon))
                {
                    Console.WriteLine("Invalid weapon!");
                    continue;
                }

                int result = rps.Next(weapon);

                Console.WriteLine("You chose {0} and your opponent chose {1}!", weapon, rps.LastAIWeapon);

                switch (result)
                {
                    case 1: Console.WriteLine("{0} pwns {1}. You're a winner!", weapon, rps.LastAIWeapon);
                        wins++;
                        break;
                    case 0: Console.WriteLine("Draw!");
                        draws++;
                        break;
                    case -1: Console.WriteLine("{0} pwns {1}. You're a loser!", rps.LastAIWeapon, weapon);
                        losses++;
                        break;
                }

                Console.WriteLine();
            }

            Console.WriteLine("\nPlayer Statistics\nWins: {0}\nLosses: {1}\nDraws: {2}", wins, losses, draws);
        }

        class RPSGame
        {
            public RPSGame(params string[] weapons)
            {
                Weapons = weapons;

                // Creates a new AI opponent, and gives it the list of weapons.
                _rpsAI = new RPSAI(weapons);
            }

            // Play next turn.
            public int Next(string weapon)
            {
                string aiWeapon = _rpsAI.NextMove(); // Gets the AI opponent's next move.
                LastAIWeapon = aiWeapon; // Saves the AI opponent's move in a property so the player can see it.

                _rpsAI.AddPlayerMove(weapon); // Let the AI know which weapon the player chose, for future predictions.
                return GetWinner(Weapons, weapon, aiWeapon); // Returns -1 if AI win, 0 if draw, and 1 if player win.
            }

            // Returns matchup winner.
            public static int GetWinner(string[] weapons, string weapon1, string weapon2)
            {
                if (weapon1 == weapon2)
                    return 0; // If weapons are the same, return 0 for draw.

                if (GetVictories(weapons, weapon1).Contains(weapon2))
                    return 1; // Returns 1 for weapon1 win.
                else if (GetVictories(weapons, weapon2).Contains(weapon1))
                    return -1; // Returns -1 for weapon2 win.

                throw new Exception("No winner found.");
            }

            /* 
             * Return weapons that the provided weapon beats.
             * The are calculated in the following way:
             * If the index of the weapon is even, then all even indices less than it,
             * and all odd indices greater than it, are victories.
             * One exception is if it is an odd index, and also the last index in the set,
             * then the first index in the set is a victory.
             */
            public static IEnumerable<string> GetVictories(string[] weapons, string weapon)
            {
                // Gets index of weapon.
                int index = Array.IndexOf(weapons, weapon);

                // If weapon is odd and the final index in the set, then return the first item in the set as a victory.
                if (index % 2 != 0 && index == weapons.Length - 1)
                    yield return weapons[0];
                
                for (int i = index - 2; i >= 0; i -= 2)
                    yield return weapons[i];

                for (int i = index + 1; i < weapons.Length; i += 2)
                    yield return weapons[i];
            }

            public string LastAIWeapon
            {
                private set;
                get;
            }

            public readonly string[] Weapons;
            private RPSAI _rpsAI;

            class RPSAI
            {
                public RPSAI(params string[] weapons)
                {
                    _weapons = weapons;
                    _weaponProbability = new Dictionary<string, int>();

                    // The AI sets the probability for each weapon to be chosen as 1.
                    foreach (string weapon in weapons)
                        _weaponProbability.Add(weapon, 1);

                    _random = new Random();
                }

                // Increases probability of selecting each weapon that beats the provided move.
                public void AddPlayerMove(string weapon)
                {
                    int index = Array.IndexOf(_weapons, weapon);

                    foreach (string winWeapon in _weapons.Except(GetVictories(_weapons, weapon)))
                        if (winWeapon != weapon)
                            _weaponProbability[winWeapon]++;
                }

                // Gets the AI's next move.
                public string NextMove()
                {
                    double r = _random.NextDouble();

                    double divisor = _weaponProbability.Values.Sum();

                    var weightedWeaponRanges = new Dictionary<double, string>();

                    double currentPos = 0.0;

                    // Maps probabilities to ranges between 0.0 and 1.0. Returns weighted random weapon.
                    foreach (var weapon in _weaponProbability)
                    {
                        double weightedRange = weapon.Value / divisor;
                        if (r <= currentPos + (weapon.Value / divisor))
                            return weapon.Key;
                        currentPos += weightedRange;
                    }

                    throw new Exception("Error calculating move.");
                }

                Random _random;
                private readonly string[] _weapons;
                private Dictionary<string, int> _weaponProbability;
            }
        }
    }
}
```


Sample output of first 2 and last 2 rounds when player chooses rock every turn:


```txt
Make your move: scissors, paper, rock, lizard, spock, quit
rock
You chose rock and your opponent chose lizard!
rock pwns lizard. You're a winner!

Make your move: scissors, paper, rock, lizard, spock, quit
rock
You chose rock and your opponent chose lizard!
rock pwns lizard. You're a winner!

...

Make your move: scissors, paper, rock, lizard, spock, quit
rock
You chose rock and your opponent chose paper!
paper pwns rock. You're a loser!

Make your move: scissors, paper, rock, lizard, spock, quit
rock
You chose rock and your opponent chose spock!
spock pwns rock. You're a loser!

Make your move: scissors, paper, rock, lizard, spock, quit
quit

Player Statistics
Wins: 5
Losses: 60
Draws: 1

```



## Clojure

{{libheader|jline}}
{{libheader|clojure.data.generators}}

Note: If you run it with Leiningen, use the special trampoline run to prevent issues:


```txt
$ lein trampoline run
```


Code:


```clojure
(ns rps.core
  (:require [clojure.data.generators :refer [weighted]])
  (:import jline.Terminal)
  (:gen-class))

(def what-beats {:rock :paper, :paper :scissors, :scissors :rock})

(defn battle [human comp]
  (let [h (name human)
        c (name comp)]
    (cond
      (= human comp) (println (format "We both picked %s. DRAW!" c))
      (= human (what-beats comp))
        (println (format "Your %s defeats computer's %s. YOU WIN!" h c))
      (= comp (what-beats human))
        (println (format "Computer's %s defeats your %s. YOU LOSE!" c h))
      :else (println (format "Wat? %s and %s ?" h c)))))

(defn key->rps [k]
  (cond
    (or (= k 82) (= k 114)) :rock
    (or (= k 80) (= k 112)) :paper
    (or (= k 83) (= k 115)) :scissors
    :else nil))

(defn play-game [freqs]
    (println "\n(R)ock, (P)aper, (S)cissors?")
    (let [term (Terminal/getTerminal)
          rps (key->rps (.readCharacter term System/in))]
      (if-not (nil? rps)
        (do 
          (battle rps (what-beats (weighted freqs)))
          (recur (assoc freqs rps (inc (rps freqs)))))
        (println "Game Over Man!  Game Over!"))))

(defn -main
  "Rock, Paper, Scissors!"
  [& args]
  (play-game {:rock 1, :paper 1, :scissors 1}))
```


{{out}}

```txt
(R)ock, (P)aper, (S)cissors?
Your paper defeats computer's rock. YOU WIN!

(R)ock, (P)aper, (S)cissors?
Computer's paper defeats your rock. YOU LOSE!

(R)ock, (P)aper, (S)cissors?
We both picked scissors. DRAW!
```



## Crystal

Inspired by [[#Ruby]] solution, but improved to allow additional weapons

```ruby

# conventional weapons
enum Choice
  Rock
  Paper
  Scissors
end

BEATS = {
  Choice::Rock     => [Choice::Paper],
  Choice::Paper    => [Choice::Scissors],
  Choice::Scissors => [Choice::Rock],
}

# uncomment to use additional weapons
# enum Choice
#   Rock
#   Paper
#   Scissors
#   Lizard
#   Spock
# end

# BEATS = {
#   Choice::Rock     => [Choice::Paper, Choice::Spock],
#   Choice::Paper    => [Choice::Scissors, Choice::Lizard],
#   Choice::Scissors => [Choice::Rock, Choice::Spock],
#   Choice::Lizard   => [Choice::Rock, Choice::Scissors],
#   Choice::Spock    => [Choice::Paper, Choice::Lizard],
# }

class RPSAI
  @stats = {} of Choice => Int32

  def initialize
    Choice.values.each do |c|
      @stats[c] = 1
    end
  end

  def choose
    v = rand(@stats.values.sum)
    @stats.each do |choice, rate|
      v -= rate
      return choice if v < 0
    end
    raise ""
  end

  def train(selected)
    BEATS[selected].each do |c|
      @stats[c] += 1
    end
  end
end

enum GameResult
  HumanWin
  ComputerWin
  Draw

  def to_s
    case self
    when .draw?
      "Draw"
    when .human_win?
      "You win!"
    when .computer_win?
      "I win!"
    end
  end
end

class RPSGame
  @score = Hash(GameResult, Int32).new(0)
  @ai = RPSAI.new

  def check(player, computer)
    return GameResult::ComputerWin if BEATS[player].includes? computer
    return GameResult::HumanWin if BEATS[computer].includes? player
    return GameResult::Draw
  end

  def round
    puts ""
    print "Your choice (#{Choice.values.join(", ")}):"
    s = gets.not_nil!.strip.downcase
    return false if "quit".starts_with? s
    player_turn = Choice.values.find { |choice| choice.to_s.downcase.starts_with? s }
    unless player_turn
      puts "Invalid choice"
      return true
    end
    ai_turn = @ai.choose
    result = check(player_turn, ai_turn)
    puts "H: #{player_turn}, C: #{ai_turn} => #{result}"
    @score[result] += 1
    puts "score: human=%d, computer=%d, draw=%d" % GameResult.values.map { |r| @score[r] }
    @ai.train player_turn
    true
  end
end

game = RPSGame.new
loop do
  break unless game.round
end
```




## D

{{trans|Python}}

```d
import std.stdio, std.random, std.string, std.conv, std.array, std.typecons;

enum Choice { rock, paper, scissors }

bool beats(in Choice c1, in Choice c2) pure nothrow @safe @nogc {
    with (Choice) return (c1 == paper    && c2 == rock) ||
                         (c1 == scissors && c2 == paper) ||
                         (c1 == rock     && c2 == scissors);
}

Choice genMove(in int r, in int p, in int s) @safe /*@nogc*/ {
    immutable x = uniform!"[]"(1, r + p + s);
    if (x < s)      return Choice.rock;
    if (x <= s + r) return Choice.paper;
    else            return Choice.scissors;
}

Nullable!To maybeTo(To, From)(From x) pure nothrow @safe {
    try {
        return typeof(return)(x.to!To);
    } catch (ConvException) {
        return typeof(return)();
    } catch (Exception e) {
        static immutable err = new Error("std.conv.to failure");
        throw err;
    }
}

void main() /*@safe*/ {
    int r = 1, p = 1, s = 1;

    while (true) {
        write("rock, paper or scissors? ");
        immutable hs = readln.strip.toLower;
        if (hs.empty)
            break;

        immutable h = hs.maybeTo!Choice;
        if (h.isNull) {
            writeln("Wrong input: ", hs);
            continue;
        }

        immutable c = genMove(r, p, s);
        writeln("Player: ", h.get, " Computer: ", c);

             if (beats(h, c)) writeln("Player wins\n");
        else if (beats(c, h)) writeln("Player loses\n");
        else                  writeln("Draw\n");

        final switch (h.get) {
            case Choice.rock:     r++; break;
            case Choice.paper:    p++; break;
            case Choice.scissors: s++; break;
        }
    }
}
```

{{out}}

```txt
rock, paper or scissors? paper
Player: paper Computer: paper
Draw

rock, paper or scissors? scissors
Player: scissors Computer: scissors
Draw

rock, paper or scissors? rock
Player: rock Computer: paper
Player loses

rock, paper or scissors? rock
Player: rock Computer: paper
Player loses

rock, paper or scissors?

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Rock_paper_scissors do
  def play, do: loop([1,1,1])
  
  defp loop([r,p,s]=odds) do
    IO.gets("What is your move? (R,P,S,Q) ") |> String.upcase |> String.first
    |> case do
           "Q" -> IO.puts "Good bye!"
           human when human in ["R","P","S"] ->
              IO.puts "Your move is #{play_to_string(human)}."
              computer = select_play(odds)
              IO.puts "My move is #{play_to_string(computer)}"
              case beats(human,computer) do
                  true  -> IO.puts "You win!"
                  false -> IO.puts "I win!"
                  _     -> IO.puts "Draw"
              end
              case human do
                  "R" -> loop([r+1,p,s])
                  "P" -> loop([r,p+1,s])
                  "S" -> loop([r,p,s+1])
              end
           _ ->
              IO.puts "Invalid play"
              loop(odds)
       end
  end
  
  defp beats("R","S"), do: true
  defp beats("P","R"), do: true
  defp beats("S","P"), do: true
  defp beats(x,x), do: :draw
  defp beats(_,_), do: false
  
  defp play_to_string("R"), do: "Rock"
  defp play_to_string("P"), do: "Paper"
  defp play_to_string("S"), do: "Scissors"
 
  defp select_play([r,p,s]) do
    n = :rand.uniform(r+p+s)
    cond do
        n <= r   -> "P"
        n <= r+p -> "S"
        true     -> "R"
    end
  end
end

Rock_paper_scissors.play
```


'''Sample output:'''

```txt

What is your move? (R,P,S,Q) r
Your move is Rock.
My move is Scissors
You win!
What is your move? (R,P,S,Q) p
Your move is Paper.
My move is Paper
Draw
What is your move? (R,P,S,Q) s
Your move is Scissors.
My move is Scissors
Draw
What is your move? (R,P,S,Q) q
Good bye!

```



## Erlang


```erlang

-module(rps).
-compile(export_all).

play() ->
    loop([1,1,1]).

loop([R,P,S]=Odds) ->
    case io:fread("What is your move? (R,P,S,Q) ","~c") of
        {ok,["Q"]} -> io:fwrite("Good bye!~n");
        {ok,[[Human]]} when Human == $R; Human == $P; Human == $S ->
            io:fwrite("Your move is ~s.~n",
                      [play_to_string(Human)]),
            Computer = select_play(Odds),
            io:fwrite("My move is ~s~n",
                      [play_to_string(Computer)]),
            case {beats(Human,Computer),beats(Computer,Human)} of
                {true,_} -> io:fwrite("You win!~n");
                {_,true} -> io:fwrite("I win!~n");
                _ -> io:fwrite("Draw~n")
            end,
            case Human of
                $R -> loop([R+1,P,S]);
                $P -> loop([R,P+1,S]);
                $S -> loop([R,P,S+1])
            end;
        _ ->
            io:fwrite("Invalid play~n"),
            loop(Odds)
    end.

beats($R,$S) -> true;
beats($P,$R) -> true;
beats($S,$P) -> true;
beats(_,_) -> false.

play_to_string($R) -> "Rock";
play_to_string($P) -> "Paper";
play_to_string($S) -> "Scissors".

select_play([R,P,S]) ->
    N = random:uniform(R+P+S),
    if
        N =< R -> $P;
        N =< R+P -> $S;
        true -> $R
    end.

```



## Euphoria

{{trans|C}}

```euphoria
function weighted_rand(sequence table)
    integer sum,r
    sum = 0
    for i = 1 to length(table) do
        sum += table[i]
    end for
    
    r = rand(sum)
    for i = 1 to length(table)-1 do
        r -= table[i]
        if r <= 0 then
            return i
        end if
    end for
    return length(table)
end function

constant names = { "Rock", "Paper", "Scissors" }
constant winner = { "We tied.", "Meself winned.", "You win." }
integer user_action, my_action, key, win
sequence user_rec, score
user_rec = {1,1,1}
score = {0,0}

while 1 do
    my_action = remainder(weighted_rand(user_rec)+1,3)+1
    puts(1,"Your choice [1-3]:\n")
    puts(1,"  1. Rock\n  2. Paper\n  3. Scissors\n> ")
    key = -1
    while (key < '1' or key > '3') and key != 'q' do
        key = get_key()
    end while
    puts(1,key)
    puts(1,'\n')
    if key = 'q' then
        exit
    end if
    user_action = key-'0'
    win = remainder(my_action-user_action+3,3)
    printf(1,"You chose %s; I chose %s. %s\n",
        { names[user_action],
          names[my_action],
          winner[win+1] })
    
    if win then
        score[win] += 1
    end if
    printf(1,"\nScore %d:%d\n",score)
    user_rec[user_action] += 1
end while
```


=={{header|F Sharp|F#}}==

```FSharp
open System

let random = Random ()
let rand = random.NextDouble () //Gets a random number in the range (0.0, 1.0)

/// Union of possible choices for a round of rock-paper-scissors
type Choice =
| Rock
| Paper
| Scissors

/// Gets the string representation of a Choice
let getString = function
    | Rock -> "Rock"
    | Paper -> "Paper"
    | Scissors -> "Scissors"

/// Defines rules for winning and losing
let beats (a : Choice, b : Choice) =
    match a, b with
    | Rock, Scissors -> true    // Rock beats Scissors
    | Paper, Rock -> true       // Paper beats Rock
    | Scissors, Paper -> true   // Scissors beat Paper
    | _, _ -> false

/// Generates the next move for the computer based on probability derived from previous player moves.
let genMove r p s =
    let tot = r + p + s
    let n = rand
    if n <= s / tot then Rock
    elif n <= (s + r) / tot then Paper
    else Scissors

/// Gets the move chosen by the player
let rec getMove () =
    printf "[R]ock, [P]aper, or [S]cissors?: "
    let choice = Console.ReadLine ()
    match choice with
    | "r" | "R" -> Rock
    | "p" | "P" -> Paper
    | "s" | "S" -> Scissors
    | _ ->
        printf "Invalid choice.\n\n"
        getMove ()

/// Place where all the game logic takes place.
let rec game (r : float, p : float, s : float) =
    let comp = genMove r p s
    let player = getMove ()
    Console.WriteLine ("Player: {0} vs Computer: {1}", getString player, getString comp)
    Console.WriteLine (
        if beats(player, comp) then "Player Wins!\n"
        elif beats(comp, player) then "Computer Wins!\n"
        else "Draw!\n"
    )
    let nextR = if player = Rock then r + 1.0 else r
    let nextP = if player = Paper then p + 1.0 else p
    let nextS = if player = Scissors then s + 1.0 else s
    game (nextR, nextP, nextS)

game(1.0, 1.0, 1.0)

```



## Factor


```factor
USING: combinators formatting io kernel math math.ranges qw
random sequences ;
IN: rosetta-code.rock-paper-scissors

CONSTANT: thing qw{ rock paper scissors }
CONSTANT: msg { "I win." "Tie!" "You win." }

: ai-choice ( r p s -- n )
    3dup + + nip [1,b] random {
        { [ 3dup nip >= ] [ 3drop 1 ] }
        { [ 3dup [ + ] dip >= ] [ 3drop 2 ] }
        [ 3drop 0 ]
    } cond ;

: player-choice ( -- n )
    "r/p/s/q? " write readln qw{ r p s q } index dup
    [ drop player-choice ] unless ;

! return:
! -1 for n1 loses to n2.
!  0 for n1 ties n2.
!  1 for n1 beats n2.
: outcome ( n1 n2 -- n3 ) - dup abs 1 > [ neg ] when sgn ;

: status. ( seq -- )
    "My wins: %d  Ties: %d  Your wins: %d\n" vprintf ;

: choices. ( n1 n2 -- )
    [ thing nth ] bi@ "You chose: %s\nI chose: %s\n" printf ;

: tally ( seq n -- seq' ) over [ 1 + ] change-nth ;

: game ( seq -- seq' )
    dup status. player-choice dup 3 = [ drop ] [
        [ 3 + tally ] keep over 3 tail* first3 ai-choice 2dup
        choices. outcome 1 + dup [ tally ] dip msg nth print nl
        game
    ] if ;

! The game state is a sequence where the members are:
! losses, ties, wins, #rock, #paper, #scissors
: main ( -- ) { 0 0 0 1 1 1 } clone game drop ;

MAIN: main
```

{{out}}

```txt

My wins: 0  Ties: 0  Your wins: 0
r/p/s/q? r
You chose: rock
I chose: paper
I win.

My wins: 1  Ties: 0  Your wins: 0
r/p/s/q? r
You chose: rock
I chose: scissors
You win.

My wins: 1  Ties: 0  Your wins: 1
r/p/s/q? r
You chose: rock
I chose: paper
I win.

My wins: 2  Ties: 0  Your wins: 1
r/p/s/q? r
You chose: rock
I chose: rock
Tie!

My wins: 2  Ties: 1  Your wins: 1
r/p/s/q? r
You chose: rock
I chose: rock
Tie!

My wins: 2  Ties: 2  Your wins: 1
r/p/s/q? r
You chose: rock
I chose: paper
I win.

My wins: 3  Ties: 2  Your wins: 1
r/p/s/q? r
You chose: rock
I chose: paper
I win.

My wins: 4  Ties: 2  Your wins: 1
r/p/s/q? q

```



## Fortran

Please find an example run in a GNU/linux system along with compilation instructions at the beginning of the FORTRAN 2008 source code.  Following the source are examples demonstrating the effectiveness of the built in simple predictive artificial intelligence.  It uses the yes utility for a constant data source.

```FORTRAN

! compilation
! gfortran -std=f2008 -Wall -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
!
! EXAMPLE
!
!$ ./f
!rock, paper, scissors?  papier
!scoring computer choice (r) and your choice (p)
!rock, paper, scissors?  sizzerz
!scoring computer choice (s) and your choice (s)
!rock, paper, scissors?  quit
!scoring computer choice (r) and your choice (q)
! Who's keeping score anyway???
!  0.5  1.5
! you won!
!$ 



program rpsgame

  integer, parameter :: COMPUTER=1, HAPLESSUSER=2
  integer, dimension(3) :: rps = (/1,1,1/)
  real, dimension(3) :: p
  character :: answer, cc ! computer choice
  integer :: exhaustion, i
  real, dimension(2) :: score = (/0, 0/)
  character(len=8), dimension(3) :: choices = (/'rock    ','paper   ','scissors'/)
  real :: harvest
  do exhaustion = 1, 30
    p = rps/real(sum(rps))
    p(2) = p(2) + p(1)
    p(3) = p(3) + p(2)
    call random_number(harvest)
    i = sum(merge(1,0,harvest.le.p)) ! In memory of Ken Iverson, logical is more useful as integer.
    cc = 'rsp'(i:i)
    write(6, "(2(A,', '),A,'?  ')", advance='no')(trim(choices(i)),i=1,size(choices))
    read(5, *) answer
    write(6, "('scoring computer choice (',A,') and your choice (',A,')')")cc,answer
    if (answer.eq.cc) then
      score = score + 0.5
    else
      i = HAPLESSUSER
      if (answer.eq.'r') then
        if (cc.eq.'p') i = COMPUTER
      else if (answer.eq.'p') then
        if (cc.eq.'s') i = COMPUTER
      else if (answer.eq.'s') then
        if (cc.eq.'r') i = COMPUTER
      else
        exit
      endif
      score(i) = score(i) + 1
    end if
    i = scan('rps',answer)
    rps(i) = rps(i) + 1
  end do
  if (25 .lt. exhaustion) write(6, *) "I'm bored out of my skull"
  write(6, *)"Who's keeping score anyway???"
  write(6, '(2f5.1)') score
  if (score(COMPUTER) .lt. score(HAPLESSUSER)) print*,'you won!'
end program rpsgame

```

rpsgame won't play more than 30 games at a time.

```bash

$ yes r | ./f                                                         # rock
rock, paper, scissors?  scoring computer choice (r) and your choice (r)
rock, paper, scissors?  scoring computer choice (s) and your choice (r)
rock, paper, scissors?  scoring computer choice (r) and your choice (r)
...
rock, paper, scissors?  scoring computer choice (p) and your choice (r)
 I'm bored out of my skull
 Who's keeping score anyway???
 25.5  4.5
yes: standard output: Broken pipe
yes: write error
$ yes p 2>/dev/null | ./f                                             # paper
rock, paper, scissors?  scoring computer choice (r) and your choice (p)
rock, paper, scissors?  scoring computer choice (s) and your choice (p)
rock, paper, scissors?  scoring computer choice (r) and your choice (p)
rock, paper, scissors?  scoring computer choice (s) and your choice (p)
...
rock, paper, scissors?  scoring computer choice (s) and your choice (p)
 I'm bored out of my skull
 Who's keeping score anyway???
 25.5  4.5
$ yes scissors 2>/dev/null | ./f                                      # scissors
rock, paper, scissors?  scoring computer choice (r) and your choice (s)
rock, paper, scissors?  scoring computer choice (r) and your choice (s)
...
rock, paper, scissors?  scoring computer choice (r) and your choice (s)
 I'm bored out of my skull
 Who's keeping score anyway???
 26.5  3.5
$ 

```



## GlovePIE

You can only press the R, P or S key to advance.

```glovepie
if var.end=0 then
var.end=0
var.computerchoice=random(3) // 1 is rock, 2 is paper, and 3 is scissors.
debug="Press the R key for rock, the P key for paper, or the S key for scissors:"
endif
if pressed(Key.R)and var.end=0 then
   var.end=1
   if var.computerchoice=1 then
      debug="You chose rock, which the computer also chose, so it's a tie!"
   else
      if var.computerchoice=2 then
         debug="The computer chose paper, covering your choice of rock, so you lose!"
      else
         debug="You chose rock, smashing the computer's choice of scissors, so you win!"
      endif
   endif
endif
if pressed(Key.P)and var.end=0 then
   var.end=1
   if var.computerchoice=1 then
      debug="You chose paper, covering the computer's choice of rock, so you win!"
   else
      if var.computerchoice=2 then
         debug="You chose paper, which the computer also chose, so it's a tie!"
      else
         debug="The computer chose scissors, cutting your choice of paper, so you lose!"
      endif
   endif
endif
if pressed(Key.S)and var.end=0 then
   var.end=1
   if var.computerchoice=1 then
      debug="The computer chose rock, smashing your choice of scissors, so you lose!"
   else
      if var.computerchoice=2 then
         debug="You chose scissors, cutting the computer's choice of paper, so you win!"
      else
         debug="You chose scissors, which the computer also chose, so it's a tie!"
      endif
   endif
endif
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "strings"
    "time"
)

const rps = "rps"

var msg = []string{
    "Rock breaks scissors",
    "Paper covers rock",
    "Scissors cut paper",
}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println("Rock Paper Scissors")
    fmt.Println("Enter r, p, or s as your play.  Anything else ends the game.")
    fmt.Println("Running score shown as <your wins>:<my wins>")
    var pi string // player input
    var aScore, pScore int
    sl := 3               // for output alignment
    pcf := make([]int, 3) // pcf = player choice frequency
    var plays int
    aChoice := rand.Intn(3) // ai choice for first play is completely random
    for {
        // get player choice
        fmt.Print("Play: ")
        _, err := fmt.Scanln(&pi)  // lazy
        if err != nil || len(pi) != 1 {
            break
        }
        pChoice := strings.Index(rps, pi)
        if pChoice < 0 {
            break
        }
        pcf[pChoice]++
        plays++

        // show result of play
        fmt.Printf("My play:%s%c.  ", strings.Repeat(" ", sl-2), rps[aChoice])
        switch (aChoice - pChoice + 3) % 3 {
        case 0:
            fmt.Println("Tie.")
        case 1:
            fmt.Printf("%s.  My point.\n", msg[aChoice])
            aScore++
        case 2:
            fmt.Printf("%s.  Your point.\n", msg[pChoice])
            pScore++
        }

        // show score
        sl, _ = fmt.Printf("%d:%d  ", pScore, aScore)

        // compute ai choice for next play
        switch rn := rand.Intn(plays); {
        case rn < pcf[0]:
            aChoice = 1
        case rn < pcf[0]+pcf[1]:
            aChoice = 2
        default:
            aChoice = 0
        }
    }
}
```

{{out|Sample output}}

```txt

Rock Paper Scissors
Enter r, p, or s as your play.  Anything else ends the game.
Running score shown as <your wins>:<my wins>
Play: r
My play: r.  Tie.
0:0  Play: p
My play:   p.  Tie.
0:0  Play: s
My play:   p.  Scissors cut paper.  Your point.
1:0  Play: r
My play:   p.  Paper covers rock.  My point.
1:1  Play: r
My play:   r.  Tie.
1:1  Play: r
My play:   p.  Paper covers rock.  My point.
1:2  Play: 

```

Additional weapons:

```go
package main

import (
    "flag"
    "fmt"
    "log"
    "math/rand"
    "regexp"
    "strings"
    "time"

    "github.com/BurntSushi/toml"
)

var help = `rps plays rock-paper-scissors with optional addition weapons.

Usage: rps [-h] [game]
   -h   display help.
   game is a game description file in TOML (https://github.com/mojombo/toml).

The traditional game can be described as

title = "Rock Paper Scissors"
win = [
   "rock breaks scissors",
   "paper covers rock",
   "scissors cut paper",
]

The title is optional, it just prints out at the beginning of a game.

The "win" list is statments where the first word wins over the last word.

It's case sensitive so make things easy by using consistent case.
Don't use punctuation, at least not next to a first or last word.

Additionally, you can have a "lose" key where the first word loses to the
the last, for example

lose = ["rock falls into well"]

To organize things with additional weapons, you can just use the winning
weapon with a list of clauses, for example

lizard = ["poisons Spock", "eats paper"]

The progam ignores anything in parentheses when identifying first or last
words.  Examples (from RPS-101) with weapons gun, fire, and sword,

win = [
   "gun fire(s)"
   "(flaming) sword has fire"
]

Finally, TOML is a hash, so don't duplicate keys.  That means no multiple
win or lose keys.  You must combine all the win statements and all the lose
statements.`

var rps = `
title = "Rock Paper Scissors"
win = [
    "rock breaks scissors",
    "paper covers rock",
    "scissors cut paper",
]`

func main() {
    h := flag.Bool("h", false, "show help")
    flag.Parse()
    if *h {
        fmt.Println(help)
        return
    }
    m := map[string]interface{}{}
    var err error
    switch flag.NArg() {
    case 0:
        _, err = toml.Decode(rps, &m)
    case 1:
        _, err = toml.DecodeFile(flag.Arg(0), &m)
    default:
        flag.Usage()
        return
    }
    if err != nil {
        log.Fatal(err)
    }
    play(parse(m))
}

type decision map[string]map[string]string

type fIndex map[string]int

func parse(m map[string]interface{}) (title string, f fIndex, d decision) {
    d = decision{}
    for k, v := range m {
        switch t := v.(type) {
        case []interface{}:
            d.parseList(k, t)
        case string:
            if k == "title" {
                title = t
            } else {
                log.Println("Unknown key:", k)
            }
        }
    }
    i := 0
    f = fIndex{}
    for w, wl := range d {
        if _, ok := f[w]; !ok {
            f[w] = i
            i++
        }
        for l := range wl {
            if _, ok := f[l]; !ok {
                f[l] = i
                i++
            }
        }
    }
    for fm := range f { 
        if _, ok := d[fm]; !ok {
            log.Println("note,", fm, "always loses!")
        }
    }
    balanced := len(f)&1 == 1
    for _, l := range d {
        if !balanced {
            break
        }
        balanced = len(l)*2+1 == len(f)
    }
    if !balanced {
        log.Print("note, game is unbalanced")
    }
    return title, f, d
}   
    
var r = regexp.MustCompile(`[(].*[)]`)
    
func (d decision) parseList(kw string, l []interface{}) {
    var ww string
    for _, e := range l {
        st, ok := e.(string)
        if !ok {
            log.Fatal("invalid", e)
        }
        w := strings.Fields(r.ReplaceAllLiteralString(st, ""))
        if len(w) == 0 {
            log.Fatalln("invalid:", kw, st)
        }
        lw := w[len(w)-1]
        switch kw {
        case "win":
            ww = w[0]
        case "lose":
            ww, lw = lw, w[0]
        default:
            ww = kw
            for i := 0; ; i++ {
                if i == len(w) {
                    st = ww + " " + st
                    break
                }
                if w[i] == ww {
                    break
                }
            }
        }
        if lw == ww {
            log.Fatalln("invalid:", st)
        }
        if cs, ok := d[lw][ww]; ok {
            log.Fatalln("conflict:", cs, "and", st)
        }
        d1, ok := d[ww]
        if !ok {
            d1 = map[string]string{}
        }
        d1[lw] = st
        d[ww] = d1
    }
}

func play(title string, fx fIndex, d decision) {
    rand.Seed(time.Now().Unix())
    if len(fx) == 0 {
        return
    }
    form := make([]string, len(fx))
    for w, i := range fx {
        form[i] = w
    }
    fmt.Println()
    fmt.Println(title)
    fmt.Print("Choices are ", form[0])
    for _, w := range form[1:] {
        fmt.Printf(", %s", w)
    }
    fmt.Println(".")
    fmt.Println("Enter one of these choices as your play.  " +
        "Anything else ends the game.")
    fmt.Println("Running score shown as <your wins>:<my wins>")
    var pw string
    var aScore, pScore int
    sl := 3
    wcf := make([]int, len(form))
    wct := 0
    ax := rand.Intn(len(form))
    aw := form[ax]
    for {
        fmt.Print("Play: ")
        _, err := fmt.Scanln(&pw)
        if err != nil {
            break
        }
        px, ok := fx[pw]
        if !ok {
            fmt.Println(pw, "invalid.")
            break
        }
        for f, l := range d {
            if _, ok := l[pw]; ok {
                wcf[fx[f]]++
                wct++
            }
        }

        fmt.Printf("My play:%s%s.\n", strings.Repeat(" ", sl-2), aw)
        ast := d[aw][pw]
        pst := d[pw][aw]
        switch {
        case ax == px:
            fmt.Println("Tie.")
        case ast > "" && pst > "":
            log.Fatalln("conflict: ", ast, "and", pst)
        case ast > "":
            fmt.Printf("%s.  My point.\n", ast)
            aScore++
        default:
            fmt.Printf("%s.  Your point.\n", pst)
            pScore++
        }
        sl, _ = fmt.Printf("%d:%d  ", pScore, aScore)
        ax = 0
        for rn := rand.Intn(wct); ; ax++ {
            if f := wcf[ax]; rn < f {
                break
            } else {
                rn -= f
            }
        }
        aw = form[ax]
    }
}
```

Example game files:

```txt

# example unbalanced game described with both win and lose statements

title = "Rock Paper Scissors Well"
win = [
    "rock breaks scissors",
    "paper covers rock",
    "paper covers well",
    "scissors cut paper",
]
lose = [
    "rock falls into well",
    "scissors fall into well",
]

```


```txt

# example with implied wins, parenthetical word.

title = "RPS-7"
rock = [
    "pounds out fire",
    "crushes scissors",
    "crushes sponge",
]
fire = [
    "melts scissors",
    "burns paper",
    "burns sponge",
]
scissors = [
    "swish through air",
    "cut paper",
    "cut sponge",
]
sponge = [
    "soaks paper",
    "uses air (pockets)",
    "absorbs water",
]
paper = [
    "fans air",
    "covers rock",
    "floats on water",
]
air = [
    "blows out fire",
    "erodes rock",
    "evaporates water",
]
water = [
    "erodes rock",
    "puts out fire",
    "rusts scissors",
]

```

{{out|Example output}}

```txt

> rps rpsw
2014/05/20 20:42:03 note, game is unbalanced

Rock Paper Scissors Well
Choices are rock, scissors, paper, well.
Enter one of these choices as your play.  Anything else ends the game.
Running score shown as <your wins>:<my wins>
Play: rock
My play: rock.
Tie.
0:0  Play: well
My play:   paper.
paper covers well.  My point.
0:1  Play: scissors
My play:   paper.
scissors cut paper.  Your point.
1:1  Play: well
My play:   paper.
paper covers well.  My point.
1:2  Play: 

> rps rps7

RPS-7
Choices are rock, scissors, sponge, fire, paper, air, water.
Enter one of these choices as your play.  Anything else ends the game.
Running score shown as <your wins>:<my wins>
Play: air
My play: water.
air evaporates water.  Your point.
1:0  Play: 

```



## Haskell



```haskell
import System.Random (randomRIO)

data Choice
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq)

beats :: Choice -> Choice -> Bool
beats Paper Rock = True
beats Scissors Paper = True
beats Rock Scissors = True
beats _ _ = False

genrps :: (Int, Int, Int) -> IO Choice
genrps (r, p, s) = rps <$> rand
  where
    rps x
      | x <= s = Rock
      | x <= s + r = Paper
      | otherwise = Scissors
    rand = randomRIO (1, r + p + s) :: IO Int

getrps :: IO Choice
getrps = rps <$> getLine
  where
    rps "scissors" = Scissors
    rps "rock" = Rock
    rps "paper" = Paper
    rps _ = error "invalid input"

game :: (Int, Int, Int) -> IO a
game (r, p, s) = do
  putStrLn "rock, paper or scissors?"
  h <- getrps
  c <- genrps (r, p, s)
  putStrLn ("Player: " ++ show h ++ " Computer: " ++ show c)
  putStrLn
    (if beats h c
       then "player wins\n"
       else if beats c h
              then "player loses\n"
              else "draw\n")
  let rr =
        if h == Rock
          then r + 1
          else r
      pp =
        if h == Paper
          then p + 1
          else p
      ss =
        if h == Scissors
          then s + 1
          else s
  game (rr, pp, ss)

main :: IO a
main = game (1, 1, 1)
```


=={{header|Icon}} and {{header|Unicon}}==
The key to this comes down to two structures and two lines of code.  The player history ''historyP'' is just an ordered list of every player turn and provides the weight for the random selection. The ''beats'' list is used to rank moves and to choose the move that would beat the randomly selected move.

```Icon
link printf

procedure main()

printf("Welcome to Rock, Paper, Scissors.\n_
        Rock beats scissors, Scissors beat paper, and Paper beats rock.\n\n")

historyP := ["rock","paper","scissors"]      # seed player history  
winP := winC := draws := 0                   # totals
   
beats := ["rock","scissors","paper","rock"]  # what beats what 1 apart

repeat {
   printf("Enter your choice or rock(r), paper(p), scissors(s) or quit(q):")
   turnP := case map(read()) of {
      "q"|"quit": break
      "r"|"rock": "rock"
      "p"|"paper": "paper"
      "s"|"scissors": "scissors"
      default:  printf(" - invalid choice.\n") & next
      }

   turnC := beats[(?historyP == beats[i := 2 to *beats],i-1)]  # choose move
      
   put(historyP,turnP)                       # record history
   printf("You chose %s, computer chose %s",turnP,turnC)   

   (beats[p := 1 to *beats] == turnP) & 
      (beats[c := 1 to *beats] == turnC) & (abs(p-c) <= 1)  # rank play
     
   if p = c then
      printf(" - draw (#%d)\n",draws +:= 1 )
   else if p > c then   
      printf(" - player win(#%d)\n",winP +:= 1)        
   else 
      printf(" - computer win(#%d)\n",winC +:= 1)    
   }

printf("\nResults:\n %d rounds\n %d Draws\n %d Computer wins\n %d Player wins\n",
   winP+winC+draws,draws,winC,winP)   
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf] 

Sample output:
```txt
Welcome to Rock, Paper, Scissors.
Rock beats scissors, Scissors beat paper, and Paper beats rock.

Enter your choice or rock(r), paper(p), scissors(s) or quit(q):s
You chose scissors, computer chose scissors - draw (#1)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):p
You chose paper, computer chose paper - draw (#2)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):r
You chose rock, computer chose scissors - computer win(#1)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):r
You chose rock, computer chose rock - draw (#3)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):p
You chose paper, computer chose paper - draw (#4)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):s
You chose scissors, computer chose scissors - draw (#5)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):r
You chose rock, computer chose rock - draw (#6)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):r
You chose rock, computer chose paper - player win(#1)
Enter your choice or rock(r), paper(p), scissors(s) or quit(q):q

Results:
 8 rounds
 6 Draws
 1 Computer wins
 1 Player wins
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Rock.bas"
110 RANDOMIZE
120 STRING CH$(1 TO 3)*8,K$*1
130 NUMERIC PLWINS(1 TO 3),SCORE(1 TO 3),PLSTAT(1 TO 3),CMSTAT(1 TO 3),PLCHOICE,CMCHOICE
140 CALL INIC
150 DO
160   CALL GUESS
170   PRINT :PRINT "Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors, ESC = quit)"
180   DO
190     LET K$=INKEY$
200   LOOP UNTIL K$>="1" AND K$<="3" OR K$=CHR$(27)
210   IF K$=CHR$(27) THEN EXIT DO
220   LET PLCHOICE=VAL(K$)
230   LET CMSTAT(CMCHOICE)=CMSTAT(CMCHOICE)+1:LET PLSTAT(PLCHOICE)=PLSTAT(PLCHOICE)+1
240   PRINT "You chose ";CH$(PLCHOICE);" and I chose ";CH$(CMCHOICE);"."
250   SET #102:INK 3
260   IF PLCHOICE=CMCHOICE THEN
270     PRINT "Tie!"
280     LET SCORE(3)=SCORE(3)+1
290   ELSE IF CMCHOICE=PLWINS(PLCHOICE) THEN
300     PRINT "You won!"
310     LET SCORE(1)=SCORE(1)+1
320   ELSE
330     PRINT "I won!"
340     LET SCORE(2)=SCORE(2)+1
350   END IF 
360   SET #102:INK 1
370 LOOP
380 PRINT :PRINT "Some useless statistics:"
390 PRINT "You won";SCORE(1);"times, and I won";SCORE(2);"times;";SCORE(3);"ties."
400 PRINT :PRINT ,,CH$(1),CH$(2),CH$(3)
410 PRINT "You chose:",PLSTAT(1),PLSTAT(2),PLSTAT(3)
420 PRINT "  I chose:",CMSTAT(1),CMSTAT(2),CMSTAT(3)
430 END
440 DEF INIC
450   LET CH$(1)="rock":LET CH$(2)="paper":LET CH$(3)="scissors"
460   LET PLWINS(1)=3:LET PLWINS(2)=1:LET PLWINS(3)=2
470   FOR I=1 TO 3
480     LET PLSTAT(I),CMSTAT(I),SCORE(I)=0
490   NEXT
500   TEXT 80
510 END DEF
520 DEF GUESS
530   LET CMCHOICE=INT(RND*(PLSTAT(1)+PLSTAT(2)+PLSTAT(3)+3))
540   SELECT CASE CMCHOICE
550   CASE 0 TO PLSTAT(1)
560     LET CMCHOICE=2
570   CASE PLSTAT(1)+1 TO PLSTAT(1)+PLSTAT(2)+1
580     LET CMCHOICE=3
590   CASE ELSE
600     LET CMCHOICE=1
610   END SELECT
620 END DEF
```



## J



```j
require'general/misc/prompt strings' NB. was 'misc strings' in older versions of J
game=:3 :0
  outcomes=. rps=. 0 0 0
  choice=. 1+?3
  while.#response=. prompt'  Choose Rock, Paper or Scissors: ' do.
    playerchoice=. 1+'rps' i. tolower {.deb response
    if.4 = playerchoice do.
      smoutput 'Unknown response.'
      smoutput 'Enter an empty line to quit'
      continue.
    end.
    smoutput '    I choose ',choice {::;:'. Rock Paper Scissors'
    smoutput (wintype=. 3 | choice-playerchoice) {:: 'Draw';'I win';'You win'
    outcomes=. outcomes+0 1 2 = wintype
    rps=. rps+1 2 3=playerchoice
    choice=. 1+3|(?0) I.~ (}:%{:)+/\ 0, rps
  end.
  ('Draws:','My wins:',:'Your wins: '),.":,.outcomes
)
```


Example use (playing to give the computer implementation the advantage):


```j
   game''
  Choose Rock, Paper or Scissors: rock
    I choose Scissors
You win
  Choose Rock, Paper or Scissors: rock
    I choose Paper
I win
  Choose Rock, Paper or Scissors: rock
    I choose Paper
I win
  Choose Rock, Paper or Scissors: rock
    I choose Paper
I win
  Choose Rock, Paper or Scissors: rock
    I choose Paper
I win
  Choose Rock, Paper or Scissors: 
Draws:     0
My wins:   4
Your wins: 1
```



## Java

{{works with|Java|1.5+}}
This could probably be made simpler, but some more complexity is necessary so that other items besides rock, paper, and scissors can be added (as school children and nerds like to do [setup for rock-paper-scissors-lizard-spock is in multi-line comments]). The method <code>getAIChoice()</code> borrows from [[#Ada|the Ada example]] in spirit, but is more generic to additional items.

```java5
import java.util.Arrays;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Random;

public class RPS {
	public enum Item{
		ROCK, PAPER, SCISSORS, /*LIZARD, SPOCK*/;
		public List<Item> losesToList;
		public boolean losesTo(Item other) {
			return losesToList.contains(other);
		}
		static {
			SCISSORS.losesToList = Arrays.asList(ROCK/*, SPOCK*/);
			ROCK.losesToList = Arrays.asList(PAPER/*, SPOCK*/);
			PAPER.losesToList = Arrays.asList(SCISSORS/*, LIZARD*/);
			/*
			SPOCK.losesToList = Arrays.asList(PAPER, LIZARD);
			LIZARD.losesToList = Arrays.asList(SCISSORS, ROCK);
			*/
                }
	}
	//EnumMap uses a simple array under the hood
	public final Map<Item, Integer> counts = new EnumMap<Item, Integer>(Item.class){{
		for(Item item:Item.values())
			put(item, 1);
	}};

	private int totalThrows = Item.values().length;

	public static void main(String[] args){
		RPS rps = new RPS();
		rps.run();
	}

	public void run() {
		Scanner in = new Scanner(System.in);
		System.out.print("Make your choice: ");
		while(in.hasNextLine()){
			Item aiChoice = getAIChoice();
			String input = in.nextLine();
			Item choice;
			try{
				choice = Item.valueOf(input.toUpperCase());
			}catch (IllegalArgumentException ex){
				System.out.println("Invalid choice");
				continue;
			}
			counts.put(choice, counts.get(choice) + 1);
			totalThrows++;
			System.out.println("Computer chose: " + aiChoice);
			if(aiChoice == choice){
				System.out.println("Tie!");
			}else if(aiChoice.losesTo(choice)){
				System.out.println("You chose...wisely. You win!");
			}else{
				System.out.println("You chose...poorly. You lose!");
			}
			System.out.print("Make your choice: ");
		}
	}

	private static final Random rng = new Random();
	private Item getAIChoice() {
		int rand = rng.nextInt(totalThrows);
		for(Map.Entry<Item, Integer> entry:counts.entrySet()){
			Item item = entry.getKey();
			int count = entry.getValue();
			if(rand < count){
				List<Item> losesTo = item.losesToList;
				return losesTo.get(rng.nextInt(losesTo.size()));
			}
			rand -= count;
		}
		return null;
	}
}
```

Sample output:

```txt
Make your choice: rock
Computer chose: PAPER
You chose...poorly. You lose!
Make your choice: rock
Computer chose: SCISSORS
You chose...wisely. You win!
Make your choice: rock
Computer chose: PAPER
You chose...poorly. You lose!
Make your choice: rock
Computer chose: SCISSORS
You chose...wisely. You win!
Make your choice: rock
Computer chose: PAPER
You chose...poorly. You lose!
Make your choice: rock
Computer chose: ROCK
Tie!
Make your choice: rock
Computer chose: ROCK
Tie!
Make your choice: rock
Computer chose: PAPER
You chose...poorly. You lose!
Make your choice: rock
Computer chose: PAPER
You chose...poorly. You lose!
Make your choice: rock
Computer chose: PAPER
You chose...poorly. You lose!
Make your choice: scissors
Computer chose: PAPER
You chose...wisely. You win!
Make your choice: scissors
Computer chose: PAPER
You chose...wisely. You win!
...
```



## JavaScript


```javascript

const logic = {
    rock: { w: 'scissor', l: 'paper'},
    paper: {w:'rock', l:'scissor'},
    scissor: {w:'paper', l:'rock'},
}

class Player {
    constructor(name){
        this.name = name;
    }
    setChoice(choice){
        this.choice = choice;
    }
    challengeOther(PlayerTwo){
        return logic[this.choice].w === PlayerTwo.choice;
    }
}

const p1 = new Player('Chris');
const p2 = new Player('John');

p1.setChoice('rock');
p2.setChoice('scissor');

p1.challengeOther(p2); //true (Win)

```


## Julia


```julia
function rps()
	print("Welcome to Rock, paper, scissors! Go ahead and type your pick.\n
		r(ock), p(aper), or s(cissors)\n
		Enter 'q' to quit.\n>")
	comp_score = 0
	user_score = 0
	options = ["r","p","s"]
	new_pick() = options[rand(1:3)]
	i_win(m,y) = ((m == "r" && y == "s")|(m == "s" && y == "p")|(m == "p" && y == "r"))
	while true
		input = string(readline(STDIN)[1])
		input == "q" && break
		!ismatch(r"^[rps]",input) && begin print("Invalid guess: Please enter 'r', 'p', or 's'\n"); continue end
		answer = new_pick()
		if input == answer 
			print("\nTie!\nScore still: \nyou $user_score\nme  $comp_score\n>")
			continue
		else
			i_win(answer,input) ? (comp_score += 1) : (user_score += 1)
			print(i_win(answer,input) ? "\nSorry you lose!\n" : "\nYou win!","Score: \nyou $user_score\nme  $comp_score\n>")
		end
	end
end
```


```txt
julia> rps()
Welcome to Rock, paper, scissors! Go ahead and type your pick.

r(ock), p(aper), or s(cissors)

Enter 'q' to quit.
>r

You win!Score: 
you 1
me  0
>p

Sorry you lose!
Score: 
you 1
me  1
>s

You win!Score: 
you 2
me  1
```



## Kotlin


```scala
// version 1.2.10

import java.util.Random

const val choices = "rpsq"
val rand = Random()

var pWins = 0                  // player wins
var cWins = 0                  // computer wins
var draws = 0                  // neither wins
var games = 0                  // games played
val pFreqs = arrayOf(0, 0, 0)  // player frequencies for each choice (rps)

fun printScore() = println("Wins: You $pWins, Computer $cWins, Neither $draws\n")

fun getComputerChoice(): Char {
    // make a completely random choice until 3 games have been played
    if (games < 3) return choices[rand.nextInt(3)]
    val num = rand.nextInt(games)
    return when {
        num < pFreqs[0] -> 'p'
        num < pFreqs[0] + pFreqs[1] -> 's'
        else -> 'r'
    }
}

fun main(args: Array<String>) {
    println("Enter: (r)ock, (p)aper, (s)cissors or (q)uit\n")
    while (true) {
        printScore()
        var pChoice: Char
        while (true) {
            print("Your choice r/p/s/q : ")
            val input = readLine()!!.toLowerCase()
            if (input.length == 1) {
                pChoice = input[0]
                if (pChoice in choices) break
            }
            println("Invalid choice, try again")
        }
        if (pChoice == 'q') {
            println("OK, quitting")
            return
        }
        val cChoice = getComputerChoice()
        println("Computer's choice   : $cChoice")
        if (pChoice == 'r' && cChoice == 's') {
            println("Rock breaks scissors - You win!")
            pWins++
        }
        else if (pChoice == 'p' && cChoice == 'r') {
            println("Paper covers rock - You win!")
            pWins++
        }
        else if (pChoice == 's' && cChoice == 'p') {
            println("Scissors cut paper - You win!")
            pWins++
        }
        else if (pChoice == 's' && cChoice == 'r') {
            println("Rock breaks scissors - Computer wins!")
            cWins++
        }
        else if (pChoice == 'r' && cChoice == 'p') {
            println("Paper covers rock - Computer wins!")
            cWins++
        }
        else if (pChoice == 'p' && cChoice == 's') {
            println("Scissors cut paper - Computer wins!")
            cWins++
        }
        else {
            println("It's a draw!")
            draws++
        }
        pFreqs[choices.indexOf(pChoice)]++
        games++
        println()
    }
}
```


Sample session:

```txt

Enter: (r)ock, (p)aper, (s)cissors or (q)uit

Wins: You 0, Computer 0, Neither 0

Your choice r/p/s/q : r
Computer's choice   : r
It's a draw!

Wins: You 0, Computer 0, Neither 1

Your choice r/p/s/q : p
Computer's choice   : s
Scissors cut paper - Computer wins!

Wins: You 0, Computer 1, Neither 1

Your choice r/p/s/q : p
Computer's choice   : r
Paper covers rock - You win!

Wins: You 1, Computer 1, Neither 1

Your choice r/p/s/q : s
Computer's choice   : s
It's a draw!

Wins: You 1, Computer 1, Neither 2

Your choice r/p/s/q : p
Computer's choice   : p
It's a draw!

Wins: You 1, Computer 1, Neither 3

Your choice r/p/s/q : q
OK, quitting

```



## Lasso

Notes: This implementation uses the default session handling in Lasso, and assumes it's running on a web server. User choices are passed in via HTTP as GET query parameters.

```Lasso
session_start('user')
session_addvar('user', 'historic_choices')
session_addvar('user', 'win_record')
session_addvar('user', 'plays')
var(historic_choices)->isNotA(::map) ? var(historic_choices = map('rock'=0, 'paper'=0, 'scissors'=0))
var(plays)->isNotA(::integer) ? var(plays = 0)
var(win_record)->isNotA(::array) ? var(win_record = array)

define br => '
'
define winner(c::string,p::string) => {
	if(#c == $superior->find(#p)) => {
		$win_record->insert('lasso')
		return 'Lasso'
	else(#p == $superior->find(#c))
		$win_record->insert('user')
		return 'User'
	else
		$win_record->insert('tie')
		return 'Nobody'
		}
	}
 
	var(
		choice 				= web_request->param('choice')->asString,
	lookup 				= array('rock', 'paper', 'scissors'),
	computer_choice 	= $lookup->get(math_random(3,1)),
	superior			= map('rock'='paper', 'paper'='scissors', 'scissors'='rock'),
	controls			= '<a href=?choice=rock>Rock</a> <a href=?choice=paper>Paper</a> <a href=?choice=scissors>Scissors</a> <a href=?choice=quit>Quit</a><br/>'
)
if($choice == 'quit') => {^
	'See ya. <a href=?>Start over</a>'
	session_end('user')
	$historic_choices = map('rock'=0, 'paper'=0, 'scissors'=0)
	$plays = 0
	$win_record = array
	
else(array('rock','paper','scissors') >> $choice)
		$controls
 
		if($plays != 0) => {
			local('possibilities') = array
			with i in $lookup do => {
				loop($historic_choices->find(#i)) => { #possibilities->insert(#i) }
			}
			
			$computer_choice = $superior->find(#possibilities->get(math_random($plays,1)))
		}
 
		'User chose ' + $choice + br
	'Lasso chose ' + $computer_choice + br
	winner($computer_choice->asString, $choice) + ' wins!'
 
		$historic_choices->find($choice) = $historic_choices->find($choice)+1
		$plays += 1
 
	else($choice->size == 0)
		$controls
 
	else
		'Invalid Choice.'+ br + $controls
^}
if($win_record->size) => {^
	br
	br
	'Win record: '+br
	'Lasso: '+($win_record->find('lasso')->size)+br
	'User: '+($win_record->find('user')->size)+br
	'Tie: '+($win_record->find('tie')->size)+br
^}
```

{{out}}

```txt
Rock Paper Scissors Quit (<- as links)
User chose paper
Lasso chose rock
User wins!

Win record: 
Lasso: 2
User: 1
Tie: 3
```



## Liberty BASIC


```lb

dim rps( 2), g$( 3)

g$( 0) ="rock": g$( 1) ="paper": g$( 2) ="scissors"
global total

total =0: draw  =0: pwin  =0: cwin  =0

rps( 0) =1: rps( 1) =1: rps( 2) =1          '   at first computer will play all three with equal probability

c =int( 3 *rnd( 1))                         '   first time, computer response is random

'   In the following code we set three integer %ages for the <human>'s biassed throws.
                                            '   set up the <human> player's key frequencies as %ages.
                                            '   Done like this it's easy to mimic the <human> input of q$
                                            '   It allows only integer %age frequency- not an important thing.
rockP =45                                   '   <<<<<<<<< Set here the <human>'s %age of 'rock' choices.
for i =1 to rockP: qF$ =qF$ +"R": next i

paprP =24                                    '   <<<<<<<<< Set here the %age of 'paper' choices.
for i =1 to paprP: qF$ =qF$ +"P": next i

scisP =100 -rockP -paprP                    '   <<<<<<<<< %age 'scissors' calculated to make 100%
for i =1 to scisP: qF$ =qF$ +"S": next i
'print qF$

do
  'do: input q$:loop until instr( "RPSrpsQq", q$)   '   for actual human input... possibly biassed.         <<<<<<<<<<<<< REM one or the other line...
  q$ =mid$( qF$, int( 100 *rnd( 1)), 1)              '   simulated <human> input with controlled bias.      <<<<<<<<<<<<<

  if total >10000 then q$ ="Q"

  g =int( ( instr( "RrPpSsQq", q$) -1) / 2)
  if g >2 then [endGame]

  total    =total   +1
  rps( g)  =rps( g) +1  '   accumulate plays the <human> has chosen. ( & hence their (biassed?) frequencies.)
  'print "  You chose "; g$( g); " and I chose "; g$( c); ". "

  select case g -c
    case 0
      draw =draw +1  ':print "It's a draw"
    case 1, -2
      pwin =pwin +1  ':print "You win!"
    case -1, 2
      cwin =cwin +1  ':print "I win!"
   end select

   r =int( total *rnd( 1))      '   Now select computer's choice for next confrontation.
   select case                  '   Using the accumulating data about <human>'s frequencies so far.
        case r <=rps( 0)
            c =1
        case r <=( rps( 0) +rps( 1))
            c =2
        case else
            c =0
   end select

   scan
loop until 0

[endGame]
    print
    print "  You won "; pwin; ", and I won "; cwin; ". There were "; draw; " draws."
    if cwin >pwin then print "      I AM THE CHAMPION!!" else print "        You won this time."
    print
    print "  At first I assumed you'd throw each equally often."
    print "  This means I'd win 1 time in 3; draw 1 in 3; lose 1 in 3."
    print "  However if I detect a bias in your throws,...."
    print "  ....this allows me to anticipate your most likely throw & on average beat it."
    print
    print "  In fact, keyboard layout & human frailty mean even if you TRY to be even & random..."
    print "  ... you may be typing replies with large bias. In 100 tries I gave 48 'P's, 29 'R' & 23 'S'!"
    print
    print "    This time I played.."
    print "      Rock "; using( "##.##", rps( 2)* 100 /total); "%    Paper "; using( "##.##", rps( 0) *100 /total); "%    Scissors "; using( "##.##", rps( 1) *100 /total); "%."
    print
    print "  ( PS. I have since learned your actual bias had been.."
    print "      Rock "; using( "##.##", rockP); "%    Paper "; using( "##.##", paprP ); "%    Scissors "; using( "##.##", ( 100 -rockP -paprP)); "%.)"
    print
    print "  The advantage I can gain gets bigger the further the 'human' entries are biassed."
    print "  The results statistically smooth out better with large runs."
    print "  Try 10,000,000, & have a cuppa while you wait."
    print
    print "  Can you see what will happen if, say, the 'human' is set to give 'Rock' EVERY time?"
    print "  Try different %ages by altering the marked code lines."
    print
    print "  Thanks for playing!"
    end

```

  You won 3204, and I won 3669. There were 3128 draws.
      I AM THE CHAMPION!!
 
  At first I assumed you'd throw each equally often.
  This means I'd win 1 time in 3; draw 1 in 3; lose 1 in 3.
  However if I detect a bias in your throws,....
  ....this allows me to anticipate your most likely throw & on average beat it.
 
  In fact, keyboard layout & human frailty mean even if you TRY to be even & random...
  ... you may be typing replies with large bias. In 100 tries I gave 48 'P's, 29 'R' & 23 'S'!
  
    This time I played..
      Rock 30.17%    Paper 45.93%    Scissors 23.94%.
 
  ( PS. I have since learned your actual bias had been..
      Rock 45.00%    Paper 24.00%    Scissors 31.00%.)
 
  The advantage I can gain gets bigger the further the 'human' entries are biassed.
  The results statistically smooth out better with large runs.
  Try 10,000,000, & have a cuppa while you wait.
 
  Can you see what will happen if, say, the 'human' is set to give 'Rock' EVERY time?
  Try different %ages by altering the marked code lines.


## Locomotive Basic


{{trans|Go}}


```locobasic
10 mode 1:defint a-z:randomize time
20 rps$="rps"
30 msg$(1)="Rock breaks scissors"
40 msg$(2)="Paper covers rock"
50 msg$(3)="Scissors cut paper"
60 print "Rock Paper Scissors":print
70 print "Enter r, p, or s as your play."
80 print "Running score shown as <your wins>:<my wins>"
90 achoice=rnd*2.99+.5 ' get integer between 1 and 3 from real between .5 and <3.5
100 ' get key
110 pin$=inkey$
120 if pin$="" then 110
130 pchoice=instr(rps$,pin$)
140 if pchoice=0 then print "Sorry?":goto 110
150 pcf(pchoice)=pcf(pchoice)+1
160 plays=plays+1
170 print "My play: "mid$(rps$,achoice,1)
180 sw=(achoice-pchoice+3) mod 3
190 if sw=0 then print "Tie." else if sw=1 then print msg$(achoice);". My point.":ascore=ascore+1 else print msg$(pchoice);". Your point.":pscore=pscore+1
200 print pscore":"ascore
210 rn=rnd*plays
220 if rn<pcf(1) then achoice=2 else if rn<pcf(1)+pcf(2) then achoice=3 else achoice=1
230 goto 110
```



## Lua


```Lua
function cpuMove()  
  local totalChance = record.R + record.P + record.S
  if totalChance == 0 then  -- First game, unweighted random
    local choice = math.random(1, 3)
    if choice == 1 then return "R" end
    if choice == 2 then return "P" end
    if choice == 3 then return "S" end
  end
  local choice = math.random(1, totalChance)  -- Weighted random bit
  if choice <= record.R then return "P" end
  if choice <= record.R + record.P then return "S" end
  return "R"
end
 
function playerMove()  -- Get user input for choice of 'weapon'
  local choice
  repeat
    os.execute("cls")  -- Windows specific command, change per OS
    print("\nRock, Paper, Scissors")
    print("
### ===============
\n")
    print("Scores -\tPlayer:", score.player)
    print("\t\tCPU:", score.cpu .. "\n\t\tDraws:", score.draws)
    io.write("\nChoose [R]ock [P]aper or [S]cissors: ")
    choice = io.read():upper():sub(1, 1)
  until choice == "R" or choice == "P" or choice == "S"
  return choice
end
 
-- Decide who won, increment scores
function checkWinner (c, p)
  io.write("I chose ")
  if c == "R" then print("rock...") end
  if c == "P" then print("paper...") end
  if c == "S" then print("scissors...") end
  if c == p then
    print("\nDraw!")
    score.draws = score.draws + 1
  elseif  (c == "R" and p == "P") or
      (c == "P" and p == "S") or
      (c == "S" and p == "R") then
        print("\nYou win!")
        score.player = score.player + 1
  else
    print("\nYou lose!")
    score.cpu = score.cpu + 1
  end
end
 
-- Main procedure
math.randomseed(os.time())
score = {player = 0, cpu = 0, draws = 0}
record = {R = 0, P = 0, S = 0}
local playerChoice, cpuChoice
repeat
  cpuChoice = cpuMove()
  playerChoice = playerMove()
  record[playerChoice] = record[playerChoice] + 1
  checkWinner(cpuChoice, playerChoice)
  io.write("\nPress ENTER to continue or enter 'Q' to quit . . . ")
until io.read():upper():sub(1, 1) == "Q"
```

Session in which I chose nothing but rock:

```txt

Rock, Paper, Scissors

### ===============


Scores -        Player: 1
                CPU:    25
                Draws:  0

Choose [R]ock [P]aper or [S]cissors: r
I chose paper...

You lose!

Press ENTER to continue or enter 'Q' to quit . . .

```



## Mathematica


```mathematica
DynamicModule[{record, play, text = "\nRock-paper-scissors\n", 
  choices = {"Rock", "Paper", "Scissors"}}, 
 Evaluate[record /@ choices] = {1, 1, 1}; 
 play[x_] := 
  Module[{y = RandomChoice[record /@ choices -> RotateLeft@choices]}, 
   record[x]++; 
   text = "Your Choice:" <> x <> "\nComputer's Choice:" <> y <> "\n" <>
      Switch[{x, y}, Alternatives @@ Partition[choices, 2, 1, 1], 
      "You lost.", 
      Alternatives @@ Reverse /@ Partition[choices, 2, 1, 1], 
      "You win.", _, "Draw."]]; 
 Column@{Dynamic[text], ButtonBar[# :> play[#] & /@ choices]}]
```



## Mercury

{{trans|Prolog}}

```Mercury
:- module rps.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- use_module random, exception.
:- import_module list, string.

:- type play
    --->    rock
    ;       paper
    ;       scissors.

:- pred beats(play, play).
:- mode beats(in, in) is semidet.
beats(rock, scissors).
beats(paper, rock).
beats(scissors, paper).

:- pred random(play::out, random.supply::mdi, random.supply::muo) is det.
random(Play, !RS) :-
    random.random(1, 3, N, !RS),
    ( if N = 1 then
        Play = rock
    else if N = 2 then
        Play = paper
    else
        Play = scissors
    ).

main(!IO) :-
    seed(Seed, !IO),
    random.init(Seed, RS),
    play(RS, !IO).

:- pred play(random.supply::mdi, io::di, io::uo) is det.
play(!.RS, !IO) :-
    io.write_string("Your choice? ", !IO),
    io.read(Res, !IO),
    (
        Res = ok(Play),
        random(Counter, !RS),
        io.format("The computer chose %s\n", [s(string(Counter))], !IO),
        ( if beats(Counter, Play) then
            io.write_string("Computer wins.\n", !IO)
        else if beats(Play, Counter) then
            io.write_string("You win!\n", !IO)
        else
            io.write_string("It is a draw\n", !IO)
        ),
        play(!.RS, !IO)
    ;
        Res = eof
    ;
        Res = error(_, _),
        exception.throw(Res)
    ).

:- pragma foreign_decl("C", "#include <time.h>").
:- pred seed(int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    seed(Seed::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Seed = time(NULL);
").
```



## OCaml


```OCaml

let pf = Printf.printf ;;

let looses a b = match a, b with
     `R, `P -> true
   | `P, `S -> true
   | `S, `R -> true
   |  _,  _ -> false ;;

let rec get_move () =
   pf "[R]ock, [P]aper, [S]cisors [Q]uit? " ;
   match String.uppercase (read_line ()) with
        "P" -> `P
      | "S" -> `S
      | "R" -> `R
      | "Q" -> exit 0
      | _   -> get_move () ;;

let str_of_move = function
        `P -> "paper"
      | `R -> "rock"
      | `S -> "scisors" ;;

let comp_move r p s =
   let tot = r +. p +. s in
   let n = Random.float 1.0 in
   if n < r /. tot then
      `R
   else
      if n < (r +. p) /. tot then
         `P
      else
         `S ;;

let rec make_moves r p s =
   let cm = comp_move r p s in   (* Computer move is based on game history. *)
   let hm = get_move () in       (* Human move is requested. *)
   pf "Me: %s. You: %s. " (str_of_move cm) (str_of_move hm);
   let outcome = 
      if looses hm cm then
         "I win. You loose.\n"
      else if cm = hm then
         "We draw.\n"
      else 
         "You win. I loose.\n"
   in pf "%s" outcome;
   match hm with (* Play on with adapted strategy. *)
        `S -> make_moves (r +. 1.) p s
      | `R -> make_moves r (p +. 1.) s
      | `P -> make_moves r p (s +. 1.) ;;

(* Main loop. *)
make_moves 1. 1. 1. ;;


```



## PARI/GP


```parigp
contest(rounds)={
  my(v=[1,1,1],wins,losses); \\ Laplace rule
  for(i=1,rounds,
    my(computer,player,t);
    t=random(v[1]+v[2]+v[3]);
    if(t<v[1], computer = "R",
      if(t<v[1]+v[2], computer = "P", computer = "S")
    );
    print("Rock, paper, or scissors?");
    t = Str(input());
    if(#t,
      player=Vec(t)[1];
      if(player <> "R" && player <> "P", player = "S")
    ,
      player = "S"
    );
    if (player == "R", v[2]++);
    if (player == "P", v[3]++);
    if (player == "S", v[1]++);
    print1(player" vs. "computer": ");
    if (computer <> player,
      if((computer == "R" && player = "P") || (computer == "P" && player = "S") || (computer == "S" && player == "R"),
        print("You win");
        losses++
      ,
        print("I win");
        wins++
      )
    ,
      print("Tie");
    )
  );
  [wins,losses]
};
contest(10)
```



## Perl

The program supports "--quiet" option, which makes it suppress all in-game output (useful for batch testing). At the end of a game it displays detailed statistics.

```perl

use 5.012;
use warnings;
use utf8;
use open qw(:encoding(utf-8) :std);
use Getopt::Long;

package Game {
    use List::Util qw(shuffle first);

    my $turns        = 0;
    my %human_choice = ( rock => 0, paper => 0, scissors => 0, );
    my %comp_choice  = ( rock => 0, paper => 0, scissors => 0, );
    my %what_beats =
      ( rock => 'paper', paper => 'scissors', scissors => 'rock', );
    my $comp_wins  = 0;
    my $human_wins = 0;
    my $draws      = 0;

    sub save_human_choice {
        my $ch = lc pop;
        if ( exists $human_choice{ $ch } ) {
            ++$human_choice{ $ch };
        }
        else {
            die __PACKAGE__ . ":: wrong choice: '$ch'";
        }
    }

    sub get_comp_choice {
        my @keys = shuffle keys %human_choice;
        my $ch;
        my ( $prob, $rand ) = ( 0, rand );
        $ch = ( first { $rand <= ( $prob += ( $human_choice{ $_ } / $turns ) ) } @keys )
            if $turns > 0;
        $ch //= $keys[0];
        $ch = $what_beats{ $ch };
        ++$comp_choice{ $ch };
        return $ch;
    }

    sub make_turn {
        my ( $comp_ch, $human_ch ) = ( pop(), pop() );
        ++$turns;
        if ( $what_beats{ $human_ch } eq $comp_ch ) {
            ++$comp_wins;
            return 'I win!';
        }
        elsif ( $what_beats{ $comp_ch } eq $human_ch ) {
            ++$human_wins;
            return 'You win!';
        }
        else {
            ++$draws;
            return 'Draw!';
        }
    }

    sub get_final_report {
        my $report =
            "You chose:\n"
          . "  rock     = $human_choice{rock} times,\n"
          . "  paper    = $human_choice{paper} times,\n"
          . "  scissors = $human_choice{scissors} times,\n"
          . "I chose:\n"
          . "  rock     = $comp_choice{rock} times,\n"
          . "  paper    = $comp_choice{paper} times,\n"
          . "  scissors = $comp_choice{scissors} times,\n"
          . "Turns: $turns\n"
          . "I won: $comp_wins, you won: $human_wins, draws: $draws\n";
        return $report;
    }
}

sub main {
    GetOptions( 'quiet' => \my $quiet );
    greet() if !$quiet;
    while (1) {
        print_next_line() if !$quiet;
        my $input = get_input();
        last unless $input;
        if ( $input eq 'error' ) {
            print "I don't understand!\n" if !$quiet;
            redo;
        }
        my $comp_choice = Game::get_comp_choice();
        Game::save_human_choice($input);
        my $result = Game::make_turn( $input, $comp_choice );
        describe_turn_result( $input, $comp_choice, $result )
          if !$quiet;
    }
    print Game::get_final_report();
}

sub greet {
    print "Welcome to the Rock-Paper-Scissors game!\n"
      . "Choose 'rock', 'paper' or 'scissors'\n"
      . "Enter empty line or 'quit' to quit\n";
}

sub print_next_line {
    print 'Your choice: ';
}

sub get_input {
    my $input = <>;
    print "\n" and return if !$input;    # EOF
    chomp $input;
    return if !$input or $input =~ m/\A \s* q/xi;
    return 
        ( $input =~ m/\A \s* r/xi ) ? 'rock'
      : ( $input =~ m/\A \s* p/xi ) ? 'paper'
      : ( $input =~ m/\A \s* s/xi ) ? 'scissors'
      :                               'error';
}

sub describe_turn_result {
    my ( $human_ch, $comp_ch, $result ) = @_;
    print "You chose \u$human_ch, I chose \u$comp_ch. $result\n";
}

main();

```

Example input can be generated as follows:

```bash

perl -e '@c=qw(r p s); for(1..10000){ print $c[ rand() < 0.75 ? 0 : int rand(2) + 1 ], "\n" }' | perl rps.pl --quiet

```

Output:

```txt

You chose:
  rock     = 7542 times,
  paper    = 1204 times,
  scissors = 1254 times,
I chose:
  rock     = 1275 times,
  paper    = 7460 times,
  scissors = 1265 times,
Turns: 10000
I won: 5920, you won: 2012, draws: 2068


```

Example of an interactive session:

```txt

Welcome to the Rock-Paper-Scissors game!
Choose 'rock', 'paper' or 'scissors'
Enter empty line or 'quit' to quit
Your choice: s
You chose Scissors, I chose Paper. You win!
Your choice: s
You chose Scissors, I chose Rock. I win!
Your choice: s
You chose Scissors, I chose Rock. I win!
Your choice: s
You chose Scissors, I chose Rock. I win!
Your choice: s
You chose Scissors, I chose Rock. I win!
Your choice: p
You chose Paper, I chose Rock. You win!
Your choice: s
You chose Scissors, I chose Rock. I win!
Your choice: q
You chose:
  rock     = 0 times,
  paper    = 1 times,
  scissors = 6 times,
I chose:
  rock     = 6 times,
  paper    = 1 times,
  scissors = 0 times,
Turns: 7
I won: 5, you won: 2, draws: 0

```



## Perl 6

This is slightly more complicated than it could be; it is a general case framework with input filtering. It weights the computers choices based on your previous choices. Type in at least the first two characters of your choice, or just hit enter to quit. Customize it by supplying your own <tt>%vs</tt> options/outcomes.

Here is standard Rock-Paper-Scissors. 


```perl6
my %vs = (
    options => [<Rock Paper Scissors>],
    ro => {
        ro => [ 2, ''                        ],
        pa => [ 1, 'Paper covers Rock: '     ],
        sc => [ 0, 'Rock smashes Scissors: ' ]
    },
    pa => {
        ro => [ 0, 'Paper covers Rock: '    ],
        pa => [ 2, ''                       ],
        sc => [ 1, 'Scissors cut Paper: '   ]
    },
    sc => {
        ro => [ 1, 'Rock smashes Scissors: '],
        pa => [ 0, 'Scissors cut Paper: '   ],
        sc => [ 2, ''                       ]
    }
);

my %choices = %vs<options>.map({; $_.substr(0,2).lc => $_ });
my $keys    = %choices.keys.join('|');
my $prompt  = %vs<options>.map({$_.subst(/(\w\w)/, -> $/ {"[$0]"})}).join(' ')~"? ";
my %weight  = %choices.keys »=>» 1;

my @stats = 0,0,0;
my $round;

while my $player = (prompt "Round {++$round}: " ~ $prompt).lc {
    $player.=substr(0,2);
    say 'Invalid choice, try again.' and $round-- and next
      unless $player.chars == 2 and $player ~~ /<$keys>/;
    my $computer = (flat %weight.keys.map( { $_ xx %weight{$_} } )).pick;
    %weight{$_.key}++ for %vs{$player}.grep( { $_.value[0] == 1 } );
    my $result = %vs{$player}{$computer}[0];
    @stats[$result]++;
    say "You chose %choices{$player},  Computer chose %choices{$computer}.";
    print %vs{$player}{$computer}[1];
    print ( 'You win!', 'You Lose!','Tie.' )[$result];
    say " - (W:{@stats[0]} L:{@stats[1]} T:{@stats[2]})\n",
};
```
Example output:

```txt
Round 1: [Ro]ck [Pa]per [Sc]issors? ro
You chose Rock,  Computer chose Paper.
Paper covers Rock: You Lose! - (W:0 L:1 T:0)

Round 2: [Ro]ck [Pa]per [Sc]issors? pa
You chose Paper,  Computer chose Scissors.
Scissors cut Paper: You Lose! - (W:0 L:2 T:0)

Round 3: [Ro]ck [Pa]per [Sc]issors? pa
You chose Paper,  Computer chose Scissors.
Scissors cut Paper: You Lose! - (W:0 L:3 T:0)

Round 4: [Ro]ck [Pa]per [Sc]issors? ro
You chose Rock,  Computer chose Rock.
Tie. - (W:0 L:3 T:1)

Round 5: [Ro]ck [Pa]per [Sc]issors? sc
You chose Scissors,  Computer chose Scissors.
Tie. - (W:0 L:3 T:2)
...
```


Here is example output from the same code only with a different %vs data structure implementing [http://en.wikipedia.org/wiki/Rock-paper-scissors-lizard-Spock Rock-Paper-Scissors-Lizard-Spock].


```perl6
my %vs = (
    options => [<Rock Paper Scissors Lizard Spock>],
    ro => {
        ro => [ 2, ''                            ],
        pa => [ 1, 'Paper covers Rock: '         ],
        sc => [ 0, 'Rock smashes Scissors: '     ],
        li => [ 0, 'Rock crushes Lizard: '       ],
        sp => [ 1, 'Spock vaporizes Rock: '      ]
    },
    pa => {
        ro => [ 0, 'Paper covers Rock: '         ],
        pa => [ 2, ''                            ],
        sc => [ 1, 'Scissors cut Paper: '        ],
        li => [ 1, 'Lizard eats Paper: '         ],
        sp => [ 0, 'Paper disproves Spock: '     ]
    },
    sc => {
        ro => [ 1, 'Rock smashes Scissors: '     ],
        pa => [ 0, 'Scissors cut Paper: '        ],
        sc => [ 2, ''                            ],
        li => [ 0, 'Scissors decapitate Lizard: '],
        sp => [ 1, 'Spock smashes Scissors: '    ]
    },
    li => {
        ro => [ 1, 'Rock crushes Lizard: '       ],
        pa => [ 0, 'Lizard eats Paper: '         ],
        sc => [ 1, 'Scissors decapitate Lizard: '],
        li => [ 2, ''                            ],
        sp => [ 0, 'Lizard poisons Spock: '      ]
    },
    sp => {
        ro => [ 0, 'Spock vaporizes Rock: '      ],
        pa => [ 1, 'Paper disproves Spock: '     ],
        sc => [ 0, 'Spock smashes Scissors: '    ],
        li => [ 1, 'Lizard poisons Spock: '      ],
        sp => [ 2, ''                            ]
    }
);
```



```txt
Round 1: [Ro]ck [Pa]per [Sc]issors [Li]zard [Sp]ock? li
You chose Lizard,  Computer chose Scissors.
Scissors decapitate Lizard: You Lose! - (W:0 L:1 T:0)

Round 2: [Ro]ck [Pa]per [Sc]issors [Li]zard [Sp]ock? sp
You chose Spock,  Computer chose Paper.
Paper disproves Spock: You Lose! - (W:0 L:2 T:0)

Round 3: [Ro]ck [Pa]per [Sc]issors [Li]zard [Sp]ock? ro
You chose Rock,  Computer chose Lizard.
Rock crushes Lizard: You Win! - (W:1 L:2 T:0)

Round 4: [Ro]ck [Pa]per [Sc]issors [Li]zard [Sp]ock? ro
You chose Rock,  Computer chose Scissors.
Rock smashes Scissors: You win! - (W:2 L:2 T:0)

Round 5: [Ro]ck [Pa]per [Sc]issors [Li]zard [Sp]ock? li
You chose Lizard,  Computer chose Paper.
Lizard eats Paper: You win! - (W:3 L:2 T:0)
...
```



## Phix


```Phix
--standard game
constant rule3 = {"rock blunts scissors",
                  "paper wraps rock",
                  "scissors cut paper"}
--extended version
constant rule5 = {"rock blunts scissors",
                  "rock crushes lizard",
                  "paper wraps rock",
                  "paper disproves spock",
                  "scissors cut paper",
                  "scissors decapitate lizard",
                  "lizard eats paper",
                  "lizard poisons spock",
                  "spock smashes scissors",
                  "spock vaporizes rock"}

constant rules = iff(rand(2)=1?rule3:rule5)

sequence what = {}
sequence beats = {}
string wkeys = ""
string question = "What is your move "
integer choices, hsum
sequence history, cplays, pplays

object x, verb, y

    for i=1 to length(rules) do
        {x} = split(rules[i])
        if not find(x,what) then
            what = append(what,x)
            if find(x[1],wkeys) then
                wkeys = append(wkeys,x[$])
                question &= x[1..-2]&"("&x[$]&"), "
            else
                wkeys = append(wkeys,x[1])
                question &= "("&x[1]&")"&x[2..$]&", "
            end if
        end if
    end for
    choices = length(wkeys)
    history = repeat(1,choices)
    hsum = 3
    cplays = repeat(0,choices)
    pplays = repeat(0,choices)
    beats = repeat(repeat(0,choices),choices)
    question[-2] = '?'
    for i=1 to length(rules) do
        {x,verb,y} = split(rules[i])
        beats[find(x,what)][find(y,what)] = verb
    end for

integer cmove, pmove, draws = 0, pwins = 0, cwins = 0
    while 1 do
        cmove = rand(hsum)
        for i=1 to choices do
            cmove -= history[i]
            if cmove<=0 then
                -- predicted user choice of i, find whatever beats it
                for j=1 to choices do
                    if string(beats[j][i]) then
                        cmove = j
                        exit
                    end if
                end for
                exit
            end if
        end for
        puts(1,question)
        while 1 do
            pmove = lower(wait_key())
            if pmove='q' then exit end if
            pmove = find(pmove,wkeys)
            if pmove!=0 then exit end if
        end while
        if pmove='q' then exit end if

        printf(1,"you: %s, me: %s, ",{what[pmove],what[cmove]})
        cplays[cmove] += 1
        pplays[pmove] += 1
        if cmove=pmove then
            printf(1,"a draw.\n")
            draws += 1
        else
            if string(beats[cmove][pmove]) then
                printf(1,"%s %s %s. I win.\n",{what[cmove],beats[cmove][pmove],what[pmove]})
                cwins += 1
            elsif string(beats[pmove][cmove]) then
                printf(1,"%s %s %s. You win.\n",{what[pmove],beats[pmove][cmove],what[cmove]})
                pwins += 1
            else
                ?9/0    -- sanity check
            end if
        end if
        history[pmove] += 1
        hsum += 1
    end while
    printf(1,"\n\nYour wins:%d, My wins:%d, Draws:%d\n",{pwins,cwins,draws})
    printf(1,"\n\nYour wins:%d, My wins:%d, Draws:%d\n",{pwins,cwins,draws})
    printf(1,"       ") for i=1 to choices do   printf(1,"%9s",what[i])     end for
    printf(1,"\nyou: ") for i=1 to choices do   printf(1,"%9d",pplays[i])   end for
    printf(1,"\n me: ") for i=1 to choices do   printf(1,"%9d",cplays[i])   end for
```

{{out}}
<pre style="font-size: 8px">
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: scissors, rock blunts scissors. You win.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: rock, a draw.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: paper, paper wraps rock. I win.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: rock, a draw.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: paper, paper wraps rock. I win.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: paper, paper wraps rock. I win.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: paper, paper wraps rock. I win.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: paper, paper wraps rock. I win.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: paper, paper wraps rock. I win.
What is your move (r)ock, (p)aper, (s)cissors? you: rock, me: paper, paper wraps rock. I win.
What is your move (r)ock, (p)aper, (s)cissors?

Your wins:1, My wins:7, Draws:2
            rock    paper scissors
you:        10        0        0
 me:         2        7        1

```



## PHP

'''Use the url bar to play'''

e.g: www.example.com/rockpaper.php?moves=paper


www.example.com/rockpaper.php?moves=rock

----



```PHP


<?php
echo "<h1>" . "Choose: ROCK - PAPER - SCISSORS" . "</h1>";
echo "<h2>";
echo "";

$player = strtoupper( $_GET["moves"] );
$wins = [
    'ROCK' => 'SCISSORS',
    'PAPER' => 'ROCK',
    'SCISSORS' => 'PAPER'
  ];
$a_i = array_rand($wins);
echo "
";
echo "Player chooses " . "<i style=\"color:blue\">" . $player . "</i>";
echo "
";
echo "
" . "A.I chooses " . "<i style=\"color:red\">"  . $a_i . "</i>";

$results = "";
if ($player == $a_i){
$results = "Draw";
} else if($wins[$a_i] == $player ){
  $results = "A.I wins";
} else {
  $results = "Player wins";
}

echo "
" . $results;
?>

```



## PicoLisp


```PicoLisp
(use (C Mine Your)
   (let (Rock 0  Paper 0  Scissors 0)
      (loop
         (setq Mine
            (let N (if (gt0 (+ Rock Paper Scissors)) (rand 1 @) 0)
               (seek
                  '((L) (le0 (dec 'N (caar L))))
                  '(Rock Paper Scissors .) ) ) )
         (prin "Enter R, P or S to play, or Q to quit: ")
         (loop
            (and (= "Q" (prinl (setq C (uppc (key))))) (bye))
            (T (setq Your (find '((S) (pre? C S)) '(Rock Paper Scissors))))
            (prinl "Bad input - try again") )
         (prinl
            "I say " (cadr Mine) ", You say " Your ": "
            (cond
               ((== Your (cadr Mine)) "Draw")
               ((== Your (car Mine)) "I win")
               (T "You win") ) )
         (inc Your) ) ) )
```



## PL/I


```PL/I

rock: procedure options (main);   /* 30 October 2013 */
   declare move character (1), cm fixed binary;

   put ('The Rock-Paper-Scissors game');
   put skip list ("please type 'r' for rock, 'p' for paper, 's' for scissors.");
   put skip list ("Anything else finishes:");
   do forever;
      get edit (move) (a(1));
      move = lowercase(move);
      if index('rpsq', move) = 0 then iterate;
      if move = 'q' then stop;
      cm = random()*3; /* computer moves: 0 = rock, 1 = paper, 2 = scissors */
      select (cm);
         when (0) select (move);
                     when ('r') put list ('rock and rock: A draw');
                     when ('p') put list ('paper beats rock: You win');
                     when ('s') put list ('rock breaks scissors: I win');
                  end;
         when (1) select (move);
                     when ('r') put list ('paper beats rock: I win');
                     when ('p') put list ('paper and paper: A draw');
                     when ('s') put list ('scissors cut paper: You win');
                  end;
         when (2) select (move);
                     when ('r') put list ('rock breaks scissors: You win');
                     when ('p') put list ('scissors cuts paper: I win');
                     when ('s') put list ('Scissors and Scissors: A draw');
                  end;
      end;
   end;
end rock;

```



## Prolog


```prolog
play :-
  findall(P,beats(P,_),Prev),
  play(Prev).

play(Prev) :-
  write('your choice? '),
  read(P),
  random_member(C, Prev),
  format('The computer chose ~p~n', C),
  result(C,P,Prev,Next),
  !,
  play(Next).

result(C,P,R,[C|R]) :-
  beats(C,P),
  format('Computer wins.~n').
result(C,P,R,[B|R]) :-
  beats(P,C),
  beats(B,P),
  format('You win!~n').
result(C,C,R,[B|R]) :-
  beats(B,C),
  format('It is a draw~n').

beats(paper, rock).
beats(rock, scissors).
beats(scissors, paper).
```



## PureBasic


```purebasic
Enumeration
  ;choices are in listed according to their cycle, weaker followed by stronger
  #rock
  #paper
  #scissors
  #numChoices ;this comes after all possible choices
EndEnumeration

;give names to each of the choices
Dim choices.s(#numChoices - 1)
choices(#rock) = "rock"
choices(#paper) = "paper"
choices(#scissors) = "scissors"

Define gameCount
Dim playerChoiceHistory(#numChoices - 1)

Procedure weightedRandomChoice()
  Shared gameCount, playerChoiceHistory()
  Protected x = Random(gameCount - 1), t, c
  
  For i = 0 To #numChoices - 1
    t + playerChoiceHistory(i)
    If t >= x
      c = i
      Break
    EndIf
  Next
   
  ProcedureReturn (c + 1) % #numChoices
EndProcedure

If OpenConsole()
  PrintN("Welcome to the game of rock-paper-scissors")
  PrintN("Each player guesses one of these three, and reveals it at the same time.")
  PrintN("Rock blunts scissors, which cut paper, which wraps stone.")
  PrintN("If both players choose the same, it is a draw!")
  PrintN("When you've had enough, choose Q.")
  
  Define computerChoice, playerChoice, response.s
  Define playerWins, computerWins, draw, quit
   
  computerChoice = Random(#numChoices - 1)
  Repeat
    Print(#CRLF$ + "What is your move (press R, P, or S)?")
    Repeat
      response = LCase(Input())
    Until FindString("rpsq", response) > 0
    
    If response = "q":
      quit = 1
    Else
      gameCount + 1
      playerChoice = FindString("rps", response) - 1
      
      result = (playerChoice - computerChoice + #numChoices) % #numChoices
      Print("You chose " + choices(playerChoice) + " and I chose " + choices(computerChoice))
      Select result
        Case 0
          PrintN(". It's a draw.")
          draw + 1
        Case 1
          PrintN(". You win!")
          playerWins + 1
        Case 2
          PrintN(". I win!")
          computerWins + 1
      EndSelect
      playerChoiceHistory(playerChoice) + 1
      computerChoice = weightedRandomChoice()
    EndIf 
  Until quit
  
  Print(#CRLF$ + "You chose: ")
  For i = 0 To #numChoices - 1
    Print(choices(i) + " " + StrF(playerChoiceHistory(i) * 100 / gameCount, 1) + "%; ")
  Next
  PrintN("")
  PrintN("You won " + Str(playerWins) + ", and I won " + Str(computerWins) + ". There were " + Str(draw) + " draws.")
  PrintN("Thanks for playing!")
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:
<pre style="height:40ex;overflow:scroll">Welcome to the game of rock-paper-scissors
Each player guesses one of these three, and reveals it at the same time.
Rock blunts scissors, which cut paper, which wraps stone.
If both players choose the same, it is a draw!
When you've had enough, choose Q.

What is your move (press R, P, or S)?r
You chose rock and I chose paper. I win!

What is your move (press R, P, or S)?r
You chose rock and I chose paper. I win!

What is your move (press R, P, or S)?r
You chose rock and I chose paper. I win!

What is your move (press R, P, or S)?r
You chose rock and I chose paper. I win!

What is your move (press R, P, or S)?r
You chose rock and I chose paper. I win!

What is your move (press R, P, or S)?s
You chose scissors and I chose paper. You win!

What is your move (press R, P, or S)?r
You chose rock and I chose paper. I win!

What is your move (press R, P, or S)?s
You chose scissors and I chose paper. You win!

What is your move (press R, P, or S)?s
You chose scissors and I chose paper. You win!

What is your move (press R, P, or S)?s
You chose scissors and I chose paper. You win!

What is your move (press R, P, or S)?s
You chose scissors and I chose rock. I win!

What is your move (press R, P, or S)?q

You chose: rock 54.5%; paper 0.0%; scissors 45.5%;
You won 4, and I won 7. There were 0 draws.
Thanks for playing!
```



## Python

A light version with limited support for additional rules. New weapons can be introduced, but each can only be beaten by one other weapon.

The <code>rules</code> dictionary is of the form <code>'this': beaten by 'that', etc</code> as opposed to <code>'this': beats 'that'</code>.


```python
from random import choice

rules = {'rock': 'paper', 'scissors': 'rock', 'paper': 'scissors'}
previous = ['rock', 'paper', 'scissors']

while True:
    human = input('\nchoose your weapon: ')
    computer = rules[choice(previous)]  # choose the weapon which beats a randomly chosen weapon from "previous"

    if human in ('quit', 'exit'): break

    elif human in rules:
        previous.append(human)
        print('the computer played', computer, end='; ')

        if rules[computer] == human:  # if what beats the computer's choice is the human's choice...
            print('yay you win!')
        elif rules[human] == computer:  # if what beats the human's choice is the computer's choice...
            print('the computer beat you... :(')
        else: print("it's a tie!")

    else: print("that's not a valid choice")
```


Output, where player always chooses Rock:


```txt
choose your weapon: rock
the computer played rock; it's a tie!

choose your weapon: rock
the computer played scissors; yay you win!

choose your weapon: rock
the computer played paper; the computer beat you... :(

choose your weapon: rock
the computer played paper; the computer beat you... :(

choose your weapon: rock
the computer played paper; the computer beat you... :(
```


This is another code. Output is as same as the above output.

```python
from random import randint

hands = ['rock', 'scissors', 'paper']; judge = ['its a tie!', 'the computer beat you... :(', 'yay you win!']
while True:
    try:
        YOU = hands.index(input('Choose your weapon: ')) # YOU = hands.index(raw_input('Choose your weapon: '))   If you use Python2.7
    except ValueError:
        break
    NPC = randint(0, 2)
    print('The computer played ' + hands[NPC] + '; ' + judge[YOU-NPC])
```



## Rascal


```rascal
import Prelude;

rel[str, str] whatbeats = {<"Rock", "Scissors">, <"Scissors", "Paper">, <"Paper", "Rock">};

list[str] ComputerChoices = ["Rock", "Paper", "Scissors"];

str CheckWinner(a, b){
	if(b == getOneFrom(whatbeats[a]))
		return a;
	elseif(a == getOneFrom(whatbeats[b]))
		return b;
	else return "Nobody"; 
}

public str RPS(human){
	computer = getOneFrom(ComputerChoices);
	x = if(human == "Rock") "Paper"; elseif(human == "Paper") "Scissors"; else "Rock";
	ComputerChoices += x;
	return "Computer played <computer>. <CheckWinner(human, computer)> wins!";
}
```

Sample output:

```rascal>rascal
RPS("Rock")
str: "Computer played Rock. Nobody wins!"

rascal>RPS("Rock")
str: "Computer played Rock. Nobody wins!"

rascal>RPS("Rock")
str: "Computer played Scissors. Rock wins!"

rascal>RPS("Rock")
str: "Computer played Paper. Paper wins!"

rascal>RPS("Rock")
str: "Computer played Paper. Paper wins!"

rascal>RPS("Rock")
str: "Computer played Paper. Paper wins!"

rascal>RPS("Rock")
str: "Computer played Paper. Paper wins!"

rascal>RPS("Rock")
str: "Computer played Rock. Nobody wins!"

rascal>RPS("Rock")
str: "Computer played Paper. Paper wins!"

rascal>RPS("Rock")
str: "Computer played Paper. Paper wins!"
```



## Racket


```racket

#lang racket
(require math)

(define history (make-hash '((paper . 1) (scissors . 1) (rock . 1))))
(define total 3)

(define (update-history! human-choice)
  (set! total (+ total 1))
  (hash-update! history human-choice add1 0))

(define (pick-one)
  (sample
   (discrete-dist '(paper scissors rock)
                  (map (λ (x) (hash-ref history x))
                       '(scissors paper rock)))))

(define (find-winner computer human)
  (define order '(scissors paper rock scissors))
  (cond 
    [(eq? computer human)                         'none]
    [(eq? (second (member computer order)) human) 'computer]
    [                                             'human]))

(define (game-loop)
  (define computer-choice (pick-one))
  (define human-choice (read))
  (define winner (find-winner computer-choice human-choice))
  (update-history! human-choice)
  (displayln (~a "Computer picked " computer-choice ", "
                 "human picked " human-choice ", "
                 winner " wins."))
  (game-loop))

(game-loop)

```



## REXX

===traditional, 3 choices===
This REXX program version:
::*   allows the human player to abbreviate their choice
::*   issues appropriate error messages for an incorrect (or no) choice(s) 
::*   allows the human player to   QUIT   at any time
::*   keeps track of the human player's responses   (to hopefully make future computer winning choices)
::*   uses better "English"/grammer,     i.e.:       ''rock breaks scissors'',       and       ''paper covers rock''. 

```rexx
/*REXX program plays rock─paper─scissors with a human;  tracks what human tends to use. */
!= '────────';   err=! '***error***';    @.=0    /*some constants for this program.     */
prompt= !  'Please enter one of:     Rock   Paper   Scissors      (or Quit)'
$.p='paper' ;    $.s='scissors';   $.r='rock'    /*list of the choices in this program. */
t.p=$.r     ;    t.s=$.p       ;   t.r=$.s       /*thingys that beats  stuff.           */
w.p=$.s     ;    w.s=$.r       ;   w.r=$.p       /*stuff     "    "   thingys.          */
b.p='covers';    b.s='cuts'    ;   b.r='breaks'  /*verbs:   how the choice wins.        */

  do forever;   say;   say prompt;   say         /*prompt the CBLF; then get a response.*/
  c=word($.p $.s $.r,   random(1, 3) )           /*choose the computer's first pick.    */
  m=max(@.r, @.p, @.s);    c=w.r                 /*prepare to examine the choice history*/
  if @.p==m  then c=w.p                          /*emulate JC's:  The Amazing Karnac.   */
  if @.s==m  then c=w.s                          /*   "     "      "     "       "      */
  c1=left(c, 1)                                  /*C1  is used for faster comparing.    */
  parse pull u;            a=strip(u)            /*get the CBLF's choice/pick (answer). */
  upper a c1  ;           a1=left(a, 1)          /*uppercase choices, get 1st character.*/
  ok=0                                           /*indicate answer isn't OK  (so far).  */
       select                                    /*process/verify the  CBLF's  choice.  */
       when words(u)==0           then           say  err   'nothing entered'
       when words(u)>1            then say  err   'too many choices: '  u
       when abbrev('QUIT',    a)  then do;  say ! 'quitting.';      exit;    end
       when abbrev('ROCK',    a) |,
            abbrev('PAPER',   a) |,
            abbrev('SCISSORS',a)  then ok=1      /*Yes?  This is a valid answer by CBLF.*/
       otherwise                  say err  'you entered a bad choice: '    u
       end   /*select*/

  if \ok          then iterate                   /*answer ¬OK?  Then get another choice.*/
  @.a1=@.a1+1                                    /*keep a history of the CBLF's choices.*/
  say !  'computer chose: '    c
  if   a1==  c1  then do;  say !  'draw.';   iterate;  end
  if $.a1==t.c1  then say  !  'the computer wins. '    !  $.c1  b.c1  $.a1
                 else say  !  'you win! '              !  $.a1  b.a1  $.c1
  end   /*forever*/                              /*stick a fork in it,  we're all done. */
```

{{out|output|text=  with various responses from the user   (output shown is a screen scraping):}}

```txt

──────── Please enter one of:     Rock  Paper  Scissors      (or Quit)

s        ◄■■■■■■■■■■■■■■■ human input.
──────── computer chose:  rock
──────── the computer wins.  ──────── rock breaks scissors

──────── Please enter one of:     Rock  Paper  Scissors      (or Quit)

s        ◄■■■■■■■■■■■■■■■ human input.
──────── computer chose:  rock
──────── the computer wins.  ──────── rock breaks scissors

──────── Please enter one of:     Rock  Paper  Scissors      (or Quit)

s        ◄■■■■■■■■■■■■■■■ human input.
──────── computer chose:  rock
──────── the computer wins.  ──────── rock breaks scissors

──────── Please enter one of:     Rock  Paper  Scissors      (or Quit)

s        ◄■■■■■■■■■■■■■■■ human input.
──────── computer chose:  rock
──────── the computer wins.  ──────── rock breaks scissors

──────── Please enter one of:     Rock  Paper  Scissors      (or Quit)

s        ◄■■■■■■■■■■■■■■■ human input.
──────── computer chose:  rock
──────── the computer wins.  ──────── rock breaks scissors

──────── Please enter one of:     Rock  Paper  Scissors      (or Quit)

p        ◄■■■■■■■■■■■■■■■ human input.
──────── computer chose:  rock
──────── you win!  ──────── paper covers  rock

──────── Please enter one of:     Rock  Paper  Scissors      (or Quit)

q        ◄■■■■■■■■■■■■■■■ human input.
──────── quitting.

```


===extended, 5 choices===
This REXX version supports more choices:   <big> rock   paper   scissors   lizard   Spock </big>

```rexx
/*REXX pgm plays rock─paper─scissors─lizard─Spock with human; tracks human usage trend. */
!= '────────';   err=! '***error***';    @.=0    /*some constants for this REXX program.*/
prompt=! 'Please enter one of:   Rock  Paper  SCissors  Lizard  SPock  (Vulcan)     (or Quit)'
$.p='paper'           ; $.s='scissors'        ; $.r='rock'          ; $.L='lizard'      ; $.v='Spock'              /*names of the thingys*/
t.p= $.r $.v          ; t.s= $.p $.L          ; t.r= $.s $.L        ; t.L= $.p $.v      ; t.v= $.r $.s             /*thingys beats stuff.*/
w.p= $.L $.s          ; w.s= $.v $.r          ; w.r= $.v $.p        ; w.L= $.r $.s      ; w.v= $.L $.p             /*stuff beats thingys.*/
b.p='covers disproves'; b.s="cuts decapitates"; b.r='breaks crushes'; b.L="eats poisons"; b.v='vaporizes smashes'  /*how the choice wins.*/
whom.1=! 'the computer wins. ' !;     whom.2=! "you win! " !;     win=words(t.p)

  do forever;   say;   say prompt;    say        /*prompt CBLF; then get a response.    */
  c=word($.p $.s $.r $.L $.v, random(1, 5) )     /*the computer's first choice/pick.    */
  m=max(@.r,@.p,@.s,@.L,@.v)                     /*used in examining CBLF's history.    */
  if @.p==m  then c=word(w.p, random(1, 2) )     /*emulate JC's  The Amazing Karnac.    */
  if @.s==m  then c=word(w.s, random(1, 2) )     /*   "     "     "     "       "       */
  if @.r==m  then c=word(w.r, random(1, 2) )     /*   "     "     "     "       "       */
  if @.L==m  then c=word(w.L, random(1, 2) )     /*   "     "     "     "       "       */
  if @.v==m  then c=word(w.v, random(1, 2) )     /*   "     "     "     "       "       */
  c1=left(c, 1)                                  /*C1  is used for faster comparing.    */
  parse pull u;            a=strip(u)            /*obtain the CBLF's choice/pick.       */
  upper a c1  ;           a1=left(a, 1)          /*uppercase the choices, get 1st char. */
  ok=0                                           /*indicate answer isn't  OK  (so far). */
       select                                    /* [↓]  process the CBLF's choice/pick.*/
       when words(u)==0               then say err    'nothing entered.'
       when words(u)>1                then say err    'too many choices: '    u
       when abbrev('QUIT',    a)      then do; say !  'quitting.';   exit;   end
       when abbrev('LIZARD',  a)   |,
            abbrev('ROCK',    a)   |,
            abbrev('PAPER',   a)   |,
            abbrev('VULCAN',  a)   |,
            abbrev('SPOCK',   a,2) |,
            abbrev('SCISSORS',a,2)    then ok=1  /*it's a valid choice for the human.   */
       otherwise                  say err   'you entered a bad choice: '   u
       end   /*select*/

  if \ok          then iterate                   /*answer ¬OK?  Then get another choice.*/
  @.a1= @.a1 + 1                                 /*keep a history of the CBLF's choices.*/
  say ! 'computer chose: '  c
  if a1==c1  then say !  'draw.'                 /*Oh rats!  The contest ended up a draw*/
             else do who=1  for 2                /*either the computer or the CBLF won. */
                  if who==2  then parse value  a1 c1   with   c1 a1
                       do j=1  for win                                  /*see who won.  */
                       if $.a1 \== word(t.c1, j)  then iterate          /*not this 'un. */
                       say whom.who  $.c1  word(b.c1, j)  $.a1          /*notify winner.*/
                       leave                                            /*leave  J loop.*/
                       end   /*j*/
                  end        /*who*/
  end   /*forever*/                              /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is similar to the 1<sup>st</sup> REXX version.}} 




## Ring


```ring

# Project : Rock-paper-scissors

see "
welcome to the game of rock-paper-scissors. 
each player guesses one of these three, and reveals it at the same time.
rock blunts scissors, which cut paper, which wraps stone.
if both players choose the same, it is a draw!
when you've had enough, choose q.
"
g = ["rock","paper","scissors"]
total=0 draw=0
pwin=0 cwin=0
see "what is your move (press r, p, or s)?"
while true
        c=random(2)+1
        give q  
        gs = floor((substr("rpsq",lower(q))))
        if gs>3 or gs<1
           summarise()
           exit
        ok
        total = total + 1
        see"you chose " + g[gs] + " and i chose " + g[c] + nl
        temp = gs-c
        if temp = 0
           see ". it's a draw"
           draw = draw + 1
        ok
        if temp = 1 or temp = -2
           see ". you win!"
           pwin = pwin + 1
        ok
        if temp = (-1) or temp = 2
            see ". i win!"
            cwin = cwin + 1
        ok
end

```

Output:

```txt

welcome to the game of rock-paper-scissors.
each player guesses one of these three, and reveals it at the same time.
rock blunts scissors, which cut paper, which wraps stone.
if both players choose the same, it is a draw!
when you've had enough, choose q.

what is your move (press r, p, or s)?
r
you chose rock and i chose rock
. it's a draw
p
you chose paper and i chose paper
. it's a draw
s
you chose scissors and i chose rock
. i win!
R
you chose rock and i chose paper
. i win!
P
you chose paper and i chose paper
. it's a draw
S
you chose scissors and i chose paper
. you win!
q
you won 1, and i won 2. there were 3 draws
thanks for playing!

```



## Ruby


```ruby
class RockPaperScissorsGame
  CHOICES = %w[rock paper scissors quit]
  BEATS = {
    'rock'     => 'paper',
    'paper'    => 'scissors',
    'scissors' => 'rock',
  }
  
  def initialize()
    @plays = {
      'rock'     => 1,
      'paper'    => 1,
      'scissors' => 1,
    }
    @score = [0, 0, 0]          # [0]:Human wins, [1]:Computer wins, [2]:draw
    
    play
  end
  
  def humanPlay
    loop do
      print "\nYour choice: #{CHOICES}? "
      answer = STDIN.gets.strip.downcase
      next if answer.empty?
      idx = CHOICES.find_index {|choice| choice.match(/^#{answer}/)}
      return CHOICES[idx] if idx
      puts "invalid answer, try again"
    end
  end
  
  def computerPlay
    total = @plays.values.reduce(:+)
    r = rand(total) + 1
    sum = 0
    CHOICES.each do |choice|
      sum += @plays[choice]
      return BEATS[choice] if r <= sum
    end
  end
  
  def play
    loop do
      h = humanPlay
      break if h == "quit"
      c = computerPlay
      print "H: #{h}, C: #{c} => "
      
      # only update the human player's history after the computer has chosen
      @plays[h] += 1
      
      if h == c
        puts "draw"
        @score[2] += 1
      elsif h == BEATS[c]
        puts "Human wins"
        @score[0] += 1
      else
        puts "Computer wins"
        @score[1] += 1
      end
      puts "score: human=%d, computer=%d, draw=%d" % [*@score]
    end
    @plays.each_key{|k| @plays[k] -= 1}
    puts "\nhumans chose #{@plays}"
  end
end

RockPaperScissorsGame.new
```


sample game where human always chooses rock:
<pre style="height: 40ex; overflow: scroll">
Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: rock => draw
score: human=0, computer=0, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: scissors => Human wins
score: human=1, computer=0, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: paper => Computer wins
score: human=1, computer=1, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: paper => Computer wins
score: human=1, computer=2, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: paper => Computer wins
score: human=1, computer=3, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: paper => Computer wins
score: human=1, computer=4, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: paper => Computer wins
score: human=1, computer=5, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: paper => Computer wins
score: human=1, computer=6, draw=1

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: rock => draw
score: human=1, computer=6, draw=2

Your choice: ["rock", "paper", "scissors", "quit"]? r
H: rock, C: paper => Computer wins
score: human=1, computer=7, draw=2

Your choice: ["rock", "paper", "scissors", "quit"]? q

humans chose {"rock"=>10, "paper"=>0, "scissors"=>0}

```



## Run BASIC


```runbasic
pri$ = "RSPR" 
rps$ = "Rock,Paper,Sissors"
[loop]
button #r, "Rock",    [r]
button #p, "Paper",   [p]
button #s, "Scissors",[s]
button #q, "Quit",    [q]
wait
[r] y = 1 :goto [me]
[p] y = 2 :goto [me]
[s] y = 3
[me]
cls
y$ = word$(rps$,y,",")
m  = int((rnd(0) * 2) + 1)
m$ = word$(rps$,m,",")
print chr$(10);"You Chose:";y$;" I chose:";m$
yp = instr(pri$,left$(y$,1))
mp = instr(pri$,left$(m$,1))
if yp = 1 and mp = 3 then mp = 0
if mp = 1 and yp = 3 then yp = 0
if yp < mp then print "You win"
if yp = mp then print "Tie"
if yp > mp then print "I win"
goto [loop]
wait

[q] cls
print "Good Bye! I enjoyed the game"
end

```



## Rust


```Rust
extern crate rand;
#[macro_use]
extern crate rand_derive;

use std::io;
use rand::Rng;
use Choice::*;

#[derive(PartialEq, Clone, Copy, Rand, Debug)]
enum Choice {
    Rock,
    Paper,
    Scissors,
}

fn beats(c1: Choice, c2: Choice) -> bool {
    (c1 == Rock && c2 == Scissors) || (c1 == Scissors && c2 == Paper) || (c1 == Paper && c2 == Rock)
}

fn ai_move<R: Rng>(rng: &mut R, v: [usize; 3]) -> Choice {
    // weighted random choice, a dynamic version of `rand::distributions::WeightedChoice`
    let rand = rng.gen_range(0, v[0] + v[1] + v[2]);
    if rand < v[0] {
        Paper
    } else if rand < v[0] + v[1] {
        Scissors
    } else {
        Rock
    }
}

fn main() {
    let mut rng = rand::thread_rng();

    println!("Rock, paper, scissors!");
    let mut ai_choice: Choice = rng.gen();
    let mut ucf = [0, 0, 0]; // user choice frequency
    let mut score = [0, 0];

    loop {
        println!("Please input your move: 'r', 'p' or 's'. Type 'q' to quit");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("failed to read line");
        let u_choice = match input.to_lowercase().trim() {
            s if s.starts_with('r') => {
                ucf[0] += 1;
                Rock
            }
            s if s.starts_with('p') => {
                ucf[1] += 1;
                Paper
            }
            s if s.starts_with('s') => {
                ucf[2] += 1;
                Scissors
            }
            s if s.starts_with('q') => break,
            _ => {
                println!("Please enter a correct choice!");
                continue;
            }
        };
        println!("You chose {:?}, I chose {:?}.", u_choice, ai_choice);
        if beats(u_choice, ai_choice) {
            score[0] += 1;
            println!("You win!");
        } else if u_choice == ai_choice {
            println!("It's a tie!");
        } else {
            score[1] += 1;
            println!("I win!");
        }
        println!("-Score: You {}, Me {}", score[0], score[1]);

        // only after the 1st iteration the AI knows the stats and can make
        // its weighted random move
        ai_choice = ai_move(&mut rng, ucf);
    }
    println!("Thank you for the game!");
}
```



## Scala

You can invoke this game with an arbitrary number of weapons:

```Scala
object RockPaperScissors extends App {
  import scala.collection.mutable.LinkedHashMap
  def play(beats: LinkedHashMap[Symbol,Set[Symbol]], played: scala.collection.Map[Symbol,Int]) {
    val h = readLine(s"""Your move (${beats.keys mkString ", "}): """) match {
      case null => println; return
      case "" => return
      case s => Symbol(s)
    }
    beats get h match {
      case Some(losers) =>
        def weighted(todo: Iterator[(Symbol,Int)], rand: Int, accum: Int = 0): Symbol = todo.next match {
          case (s, i) if rand <= (accum + i) => s
          case (_, i) => weighted(todo, rand, accum + i)
        }
        val c = weighted(played.toIterator, 1 + scala.util.Random.nextInt(played.values.sum)) match {
          // choose an opponent that would beat the player's anticipated move
          case h => beats.find{case (s, l) => l contains h}.getOrElse(beats.head)._1
        }
        print(s"  My move: $c\n  ")
        c match {
          case c if losers contains c => println("You won")
          case c if beats(c) contains h => println("You lost")
          case _ => println("We drew") // or underspecified
        }
      case x => println("  Unknown weapon, try again.")
    }
    play(beats, played get h match {
      case None => played
      case Some(count) => played.updated(h, count + 1)
    })
  }

  def play(beats: LinkedHashMap[Symbol,Set[Symbol]]): Unit =
      play(beats, beats.mapValues(_ => 1)) // init with uniform probabilities

  play(LinkedHashMap(
    'rock -> Set('lizard, 'scissors),
    'paper -> Set('rock, 'spock),
    'scissors -> Set('paper, 'lizard),
    'lizard -> Set('spock, 'paper),
    'spock -> Set('scissors, 'rock)
  ))
}
```

{{out}}

```txt
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): paper
  My move: 'paper
  We drew
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): scissors
  My move: 'paper
  You won
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): rock
  My move: 'scissors
  You won
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): rock
  My move: 'rock
  We drew
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): rock
  My move: 'paper
  You lost
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): rock
  My move: 'rock
  We drew
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): rock
  My move: 'paper
  You lost
Your move ('rock, 'paper, 'scissors, 'lizard, 'spock): rock
  My move: 'paper
  You lost
```


Here's another code: (I refactored the above code, it is more functional, more testable ) 

```Scala
object RockPaperScissors extends App {
    def beats = Map(
        'rock -> Set('lizard, 'scissors),
        'paper -> Set('rock, 'spock),
        'scissors -> Set('paper, 'lizard),
        'lizard -> Set('spock, 'paper),
        'spock -> Set('scissors, 'rock)
    )
    // init with uniform probabilities
    def initPlayed = beats.mapValues(_ => 1)
    def input = Symbol(readLine(s"""Your move (${beats.keys mkString ", "}): """))
    def random(max: Int) = scala.util.Random.nextInt(max)
    def display(text: String) = print(text)

    def weighted(todo: Iterator[(Symbol,Int)], rand: Int, accum: Int = 0): Symbol = todo.next match {
        case (s, i) if rand <= (accum + i) => s
        case (_, i) => weighted(todo, rand, accum + i)
    }

    def calcMyMove(random: Int => Int, played: Map[Symbol,Int]) = {
        weighted(played.toIterator, 1 + random(played.values.sum)) match {
        // choose an opponent that would beat the player's anticipated move
            case h => beats.find{case (s, l) => l contains h}.getOrElse(beats.head)._1
        }
    }

    case class Result(text: String, won: Int, lost: Int, drew: Int) {
        override def toString = s"$text. Won: $won, Lost: $lost, Drew: $drew"
    }

    def getResult(userWeapon: Symbol, myMove: Symbol, result: Result) = {
        if (beats(userWeapon) contains myMove)
            result.copy(text = "You won", won = result.won + 1)
        else if (beats(myMove) contains userWeapon)
            result.copy(text = "You lost", lost = result.lost + 1)
        else result.copy(text = "We drew", drew = result.drew + 1)
     }

    def play(input: => Symbol, display: String => Unit, random: Int => Int)
        (played: Map[Symbol,Int], result: Result): Result = {
        val userWeapon = input
        if (userWeapon != Symbol("")) {
            val newResult = if (beats contains userWeapon) {
                val myMove = calcMyMove(random, played)
                display(s"  My move: $myMove\n  ")
                getResult(userWeapon, myMove, result)
            } else {
                result.copy(text = "  Unknown weapon, try again")
            }
            display(newResult + "\n")
            play(input, display, random)(played get userWeapon match {
                case None => played
                case Some(count) => played.updated(userWeapon, count + 1)
            }, newResult)
        } else result
    }

    override def main(args: Array[String]): Unit =
        play(input, display, random)(initPlayed, Result("Start", 0, 0, 0))
}
```

{{out}}

```txt

Your move ('spock, 'rock, 'lizard, 'paper, 'scissors): rock
  My move: 'rock
  We drew. Won: 0, Lost: 0, Drew: 1
Your move ('spock, 'rock, 'lizard, 'paper, 'scissors): spock
  My move: 'spock
  We drew. Won: 0, Lost: 0, Drew: 2
Your move ('spock, 'rock, 'lizard, 'paper, 'scissors): lizard
  My move: 'spock
  You won. Won: 1, Lost: 0, Drew: 2
Your move ('spock, 'rock, 'lizard, 'paper, 'scissors): scissors
  My move: 'spock
  You lost. Won: 1, Lost: 1, Drew: 2
Your move ('spock, 'rock, 'lizard, 'paper, 'scissors): scissors
  My move: 'spock
  You lost. Won: 1, Lost: 2, Drew: 2
Your move ('spock, 'rock, 'lizard, 'paper, 'scissors): spock
  My move: 'rock
  You won. Won: 2, Lost: 2, Drew: 2
```



## Seed7

The program reads the possible commands (1, 2, 3, q) as single keypresses from the
[http://seed7.sourceforge.net/libraries/keybd.htm#KEYBOARD KEYBOARD] with
[http://seed7.sourceforge.net/libraries/keybd.htm#getc%28in_console_keybd_file%29 getc].
It is also possible to quit the program with q.

{{incorrect|C|This example does not seem to use the weighted average AI from the task description.}}


```seed7
$ include "seed7_05.s7i";
$ include "keybd.s7i";

const array string: rockPaperScissors is [] ("Rock", "Paper", "Scissors");

const proc: main is func
  local
    var char: command is ' ';
    var integer: user is 0;
    var integer: comp is 0;
  begin
    writeln("Hello, Welcome to rock-paper-scissors");
    repeat
      write("Please type in 1 for Rock, 2 for Paper, 3 for Scissors, q to quit ");
      flush(OUT);
      repeat
        command := lower(getc(KEYBOARD));
      until command in {'1', '2', '3', 'q'};
      writeln(command);
      if command <> 'q' then
        user := integer parse str(command);
        comp := rand(1, 3);
        writeln("You Played: " <& rockPaperScissors[user]);
        writeln("Computer Played: " <& rockPaperScissors[comp]);
        if comp = user then
          writeln("You Tied");
        elsif succ(comp) = user or user + 2 = comp then
          writeln("Yay, You Won!");
        else
          writeln("Sorry, You Lost!");
        end if;
      end if;
    until command = 'q';
    writeln("Goodbye! Thanks for playing!");
  end func;
```


Sample run:

```txt

Hello, Welcome to rock-paper-scissors
Please type in 1 for Rock, 2 for Paper, 3 for Scissors, q to quit 1
You Played: Rock
Computer Played: Scissors
Yay, You Won!
Please type in 1 for Rock, 2 for Paper, 3 for Scissors, q to quit 2
You Played: Paper
Computer Played: Scissors
Sorry, You Lost!
Please type in 1 for Rock, 2 for Paper, 3 for Scissors, q to quit 3
You Played: Scissors
Computer Played: Paper
Yay, You Won!
Please type in 1 for Rock, 2 for Paper, 3 for Scissors, q to quit q
Goodbye! Thanks for playing!

```



## Sidef


```ruby
const rps = %w(r p s)

const msg = [
    "Rock breaks scissors",
    "Paper covers rock",
    "Scissors cut paper",
]

say <<"EOT"
\n>> Rock Paper Scissors <<\n
** Enter 'r', 'p', or 's' as your play.
** Enter 'q' to exit the game.
** Running score shown as <your wins>:<my wins>
EOT

var plays   = 0
var aScore  = 0
var pScore  = 0
var pcf     = [0,0,0]      # pcf = player choice frequency
var aChoice = pick(0..2)   # ai choice for first play is completely random

loop {
    var pi = Sys.scanln("Play: ")
    pi == 'q' && break

    var pChoice = rps.index(pi)

    if (pChoice == -1) {
        STDERR.print("Invalid input!\n")
        next
    }

    ++pcf[pChoice]
    ++plays

    # show result of play
    ">> My play: %-8s".printf(rps[aChoice])

    given ((aChoice - pChoice + 3) % 3) {
        when (0) { say "Tie." }
        when (1) { "%-*s %s".printlnf(30, msg[aChoice], 'My point');   aScore++ }
        when (2) { "%-*s %s".printlnf(30, msg[pChoice], 'Your point'); pScore++ }
    }

    # show score
    "%-6s".printf("%d:%d" % (pScore, aScore))

    # compute ai choice for next play
    given (plays.rand.int) { |rn|
        case (rn < pcf[0])        { aChoice = 1 }
        case (pcf[0]+pcf[1] > rn) { aChoice = 2 }
        default                   { aChoice = 0 }
    }
}
```


'''Sample run:'''

```txt

>> Rock Paper Scissors <<

** Enter 'r', 'p', or 's' as your play.
** Enter 'q' to exit the game.
** Running score shown as <your wins>:<my wins>

Play: r
>> My play: s       Rock breaks scissors           Your point
1:0   Play: p
>> My play: p       Tie.
1:0   Play: r
>> My play: s       Rock breaks scissors           Your point
2:0   Play: s
>> My play: p       Scissors cut paper             Your point
3:0   Play: r
>> My play: p       Paper covers rock              My point
3:1   Play: p
>> My play: r       Paper covers rock              Your point
4:1   Play: q

```




## SuperCollider



```txt

// play it in the REPL, evaluating line by line

a = RockPaperScissors.new;
a.next(Scissors);
a.next(Scissors);
a.next(Scissors);
a.next(Paper);

```


An implementation using classes


```txt

Rock {

	*play { |other|
		^other.rock + this + "against" + other
	}

	*rock {
		^"tie"
	}

	*paper {
		^"loses"
	}

	*scissors {
		^"wins"
	}

	*losesAgainst {
		^Paper
	}

}

Paper {

	*play { |other|
		^other.paper + this + "against" + other
	}

	*paper {
		^"tie"
	}

	*scissors {
		^"loses"
	}

	*rock {
		^"wins"
	}

	*losesAgainst {
		^Scissors
	}

}

Scissors {

	*play { |other|
		^other.scissors + this + "against" + other
	}

	*scissors {
		^"tie"
	}

	*rock {
		^"loses"
	}

	*paper {
		^"wins"
	}

	*losesAgainst {
		^Rock
	}

}

RockPaperScissors {
	var opponentMoves;

	*new {
		^super.new.init
	}

	init {
		opponentMoves = Bag.new;
	}

	findBestMatch {
		var typicalMove = opponentMoves.wchoose;
		if(typicalMove.isNil) { ^[Rock, Paper, Scissors].choose };
		^typicalMove.losesAgainst
	}

	next { |otherMove|
		var myMove = this.findBestMatch;
		opponentMoves.add(otherMove);
		^play(otherMove, myMove)
	}

}

```


## Swift


```Swift
enum Choice: CaseIterable {
  case rock
  case paper
  case scissors
  case lizard
  case spock
}

extension Choice {
  var weaknesses: Set<Choice> {
    switch self {
      case .rock:
        return [.paper, .spock]
      case .paper:
        return [.scissors, .lizard]
      case .scissors:
        return [.rock, .spock]
      case .lizard:
        return [.rock, .scissors]
      case .spock:
        return [.paper, .lizard]
    }
  }
}

struct Game {
  private(set) var history: [(Choice, Choice)] = []
  private(set) var p1Score: Int = 0
  private(set) var p2Score: Int = 0

  mutating func play(_ p1Choice: Choice, against p2Choice: Choice) {
    history.append((p1Choice, p2Choice))
    if p2Choice.weaknesses.contains(p1Choice) {
      p1Score += 1
    } else if p1Choice.weaknesses.contains(p2Choice) {
      p2Score += 1
    }
  }
}

func aiChoice(for game: Game) -> Choice {
  if let weightedWeekness = game.history.flatMap({ $0.0.weaknesses }).randomElement() {
    return weightedWeekness
  } else {
    // If history is empty, return random Choice
    return Choice.allCases.randomElement()!
  }
}

var game = Game()
print("Type your choice to play a round, or 'q' to quit")
loop: while true {
  let choice: Choice
  switch readLine().map({ $0.lowercased() }) {
    case "r", "rock":
      choice = .rock
    case "p", "paper":
      choice = .paper
    case "scissors":
      choice = .scissors
    case "l", "lizard":
      choice = .lizard
    case "spock":
      choice = .spock
    case "q", "quit", "exit":
      break loop
    case "s":
      print("Do you mean Spock, or scissors?")
      continue
    default: 
      print("Unknown choice. Type 'q' to quit")
      continue
  }
  let p2Choice = aiChoice(for: game)
  print("You played \(choice) against \(p2Choice)")
  game.play(choice, against: p2Choice)
  print("Current score: \(game.p1Score) : \(game.p2Score)")
}
```

'''Sample run:'''

```txt

Type your choice to play a round, or 'q' to quit
r
You played rock against paper
Current score: 0 : 1
Paper
You played paper against paper
Current score: 0 : 1
spock
You played spock against lizard
Current score: 0 : 2
lizard
You played lizard against paper
Current score: 1 : 2
q

```



## Tcl


```tcl
package require Tcl 8.5

### Choices are represented by integers, which are indices into this list:
###    Rock, Paper, Scissors
### Normally, idiomatic Tcl code uses names for these sorts of things, but it
### turns out that using integers simplifies the move-comparison logic.

# How to ask for a move from the human player
proc getHumanMove {} {
    while 1 {
	puts -nonewline "Your move? \[R\]ock, \[P\]aper, \[S\]cissors: "
	flush stdout
	gets stdin line
	if {[eof stdin]} {
	    puts "\nBye!"
	    exit
	}
	set len [string length $line]
	foreach play {0 1 2} name {"rock" "paper" "scissors"} {
	    # Do a prefix comparison
	    if {$len && [string equal -nocase -length $len $line $name]} {
		return $play
	    }
	}
	puts "Sorry, I don't understand that. Try again please."
    }
}

# How to ask for a move from the machine player
proc getMachineMove {} {
    global states
    set choice [expr {int(rand() * [::tcl::mathop::+ {*}$states 3])}]
    foreach play {1 2 0} count $states {
	if {[incr sum [expr {$count+1}]] > $choice} {
	    puts "I play \"[lindex {Rock Paper Scissors} $play]\""
	    return $play
	}
    }
}

# Initialize some state variables
set states {0 0 0}
set humanWins 0
set machineWins 0

# The main game loop
while 1 {
    # Get the moves for this round
    set machineMove [getMachineMove]
    set humanMove [getHumanMove]
    # Report on what happened
    if {$humanMove == $machineMove} {
	puts "A draw!"
    } elseif {($humanMove+1)%3 == $machineMove} {
	puts "I win!"
	incr machineWins
    } else {
	puts "You win!"
	incr humanWins
    }
    puts "Cumulative scores: $humanWins to you, $machineWins to me"
    # Update the state of how the human has played in the past
    lset states $humanMove [expr {[lindex $states $humanMove] + 1}]
}
```

Sample run:

```txt

Your move? [R]ock, [P]aper, [S]cissors: rock
I play "Scissors"
You win!
Cumulative scores: 1 to you, 0 to me
Your move? [R]ock, [P]aper, [S]cissors: r
I play "Paper"
I win!
Cumulative scores: 1 to you, 1 to me
Your move? [R]ock, [P]aper, [S]cissors: s
I play "Paper"
You win!
Cumulative scores: 2 to you, 1 to me
Your move? [R]ock, [P]aper, [S]cissors: sciss
I play "Paper"
You win!
Cumulative scores: 3 to you, 1 to me
Your move? [R]ock, [P]aper, [S]cissors: p
I play "Paper"
A draw!
Cumulative scores: 3 to you, 1 to me
Your move? [R]ock, [P]aper, [S]cissors: zaphod beeblebrox
Sorry, I don't understand that. Try again please.
Your move? [R]ock, [P]aper, [S]cissors: r
I play "Scissors"
You win!
Cumulative scores: 4 to you, 1 to me
Your move? [R]ock, [P]aper, [S]cissors: ^D
Bye!

```



## TorqueScript


Rock Paper Scissors in TorqueScript:


```TorqueScript

while(isobject(RockPaperScissors))
	RockPaperScissors.delete();

new scriptObject(RockPaperScissors);

function RockPaperScissors::startGame(%this)
{
	%this.idle = true;
	echo("Starting rock paper scissors, please type choose(\"choice\"); to proceed!");
}

function RockPaperScissors::endGame(%this)
{
	%this.idle = true;
}

function RockPaperScissors::main(%this)
{
	%this.idle = 0;
	
	if(getRandom(1,2) == 1)
	{
		%result = %this.getChoiceByUserFrequency();
	}
	else
	{
		%c[0] = "rock";
		%c[1] = "paper";
		%c[2] = "scissors";
		
		%result = %c[getRandom(0,2)]; 
	}
	
	%userChoice = %this.current;
	
	%winner = thisVSthat(%userChoice,%result);
	
	if(%winner == 0)
	{
		echo("You both chose "@%userChoice@", tie!");
	}
	else
	if(%winner == 1)
	{
		echo("You chose "@%userChoice@" computer chose "@%result@", you win!");
	}
	else
	if(%winner == 2)
	{
		echo("You chose "@%userChoice@" computer chose "@%result@", you lose!");
	}
	
	%this.idle = true;
}

function RockPaperScissors::getChoiceByUserFrequency(%this)
{
	%weakness["rock"] = "paper";
	%weakness["paper"] = "Scissors";
	%weakness["scissors"] = "rock";
	
	%a[0] = %this.choice["rock"];
	%a[1] = %this.choice["paper"];
	%a[2] = %this.choice["Scissors"];
	
	%b[0] = "rock";
	%b[1] = "paper";
	%b[2] = "scissors";
	
	for(%i=0;%i<3;%i++)
	{
		%curr = %a[%i];
		
		if(%temp $= "")
		{
			%temp = %curr;
			%tempi = %i;
		}
		else
		if(%curr > %temp)
		{
			%temp = %curr;
			%tempi = %i;
		}
	}
	
	return %weakness[%b[%tempi]];
}

function choose(%this)
{
	%rps = rockPaperScissors;
	
	if(!%rps.idle)
	{
		return 0;
	}
	else
	if(%this !$= "rock" && %this !$= "paper" && %this !$= "scissors")
	{
		return 0;
	}

	%rps.choice[%this]++;
	%rps.current = %this;
	%rps.main();
	
	return %this;
}

function thisVSthat(%this,%that)
{
	if(%this !$= "rock" && %this !$= "paper" && %this !$= "scissors")
	{
		return 0;
	}
	else
	if(%that !$= "rock" && %that !$= "paper" && %that !$= "scissors")
	{
		return 0;
	}
	
	%weakness["rock"] = "paper";
	%weakness["paper"] = "Scissors";
	%weakness["scissors"] = "rock";
	
	if(%weakness[%this] $= %that)
	{
		%result = 2;
	}
	else
	if(%weakness[%that] $= %this)
	{
		%result = 1;
	}
	else
		%result = 0;
		
	return %result;
}

```


To begin do:


```TorqueScript

RockPaperScissors.startGame();

```


Choose and play!


```TorqueScript

choose("Rock");

```


=> You chose rock computer chose paper, you lose!

=={{header|TI-83 BASIC}}==

```ti83b
PROGRAM:RPS
:{0,0,0}→L1
:{0,0,0}→L2
:Lbl ST
:Disp "R/P/S"
:Disp "1/2/3"
:Lbl EC
:Input A
:If A>3 or A<1
:Then
:Goto NO
:End
:randInt(1,3+L1(1)+L1(2)+L1(3)→C
:If C≤1+L1(1)
:Then
:2→B
:Goto NS
:End
:If C>2+L1(2)
:Then
:1→B
:Else
:3→B
:End
:Lbl NS
:L1(A)+1→L1(A)
:If A=B
:Then
:Disp "TIE GAME"
:L2(3)+1→L2(3)
:Goto TG
:End
:If (A=1 and B=2) or (A=2 and B=3) or (A=3 and B=1)
:Then
:Disp "I WIN"
:L2(1)+1→L2(1)
:Else
:Disp "YOU WIN"
:L2(2)+1→L2(2)
:End
:Lbl TG
:Disp "PLAY AGAIN?"
:Input Str1
:If Str1="YES"
:Then
:ClrHome
:Goto ST
:Else
:Goto EN
:End
:Lbl NO
:ClrHome
:Pause "PICK 1,2, or 3"
:ClrHome
:Goto EC
:Lbl EN
:ClrHome
:Disp "I WON:"
:Disp L2(1)
:Disp "YOU WON:"
:Disp L2(2)
:Disp "WE TIED:"
:Disp L2(3)
:Disp "BYE"

```


{{omit from|GUISS}}


## uBasic/4tH

This implementation uses a 6-bits binary scheme, where the lower three bits represent the choice of the user and the higher three bits the choice of the computer:

{{incorrect|C|This example does not seem to use the weighted average AI from the task description.}}

<lang> 20 LET P=0: LET Q=0: LET Z=0
 30 INPUT "Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? ", A
 40 IF A>3 THEN GOTO 400
 50 IF A=3 THEN LET A=4
 60 IF A<1 THEN GOTO 400
 70 C=RND(3) : LET D=4: FOR B=1 TO C+1 : LET D = D+D : NEXT : GOTO (A+D)*10
 90 Z=Z+1 : PRINT "We both chose 'rock'. It's a draw." : GOTO 30
100 P=P+1 : PRINT "You chose 'paper', I chose 'rock'. You win.." : GOTO 30
120 Q=Q+1 : PRINT "You chose 'scissors', I chose 'rock'. I win!" : GOTO 30
170 Q=Q+1 : PRINT "You chose 'rock', I chose 'paper'. I win!" : GOTO 30
180 Z=Z+1 : PRINT "We both chose 'paper'. It's a draw." : GOTO 30
200 P=P+1 : PRINT "You chose 'scissors', I chose 'paper'. You win.." : GOTO 30
330 P=P+1 : PRINT "You chose 'rock', I chose 'scissors'. You win.." : GOTO 30
340 Q=Q+1 : PRINT "You chose 'paper', I chose 'scissors'. I win!" : GOTO 30
360 Z=Z+1 : PRINT "We both chose 'scissors'. It's a draw." : GOTO 30
400 PRINT "There were ";Z;" draws. I lost ";P;" times, you lost ";Q;" times." : END
```


A sample game:
 Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? 1
 You chose 'rock', I chose 'paper'. I win!
 Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? 2
 You chose 'paper', I chose 'scissors'. I win!
 Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? 3
 You chose 'scissors', I chose 'paper'. You win..
 Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? 1
 You chose 'rock', I chose 'paper'. I win!
 Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? 2
 We both chose 'paper'. It's a draw.
 Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? 3
 You chose 'scissors', I chose 'rock'. I win!
 Rock, paper, or scissors (1 = rock, 2 = paper, 3 = scissors)? 4
 There were 1 draws. I lost 1 times, you lost 4 times.


## UNIX Shell

{{works with|Bourne Again Shell|4}}

```bash
#!/bin/bash
choices=(rock paper scissors)

# comparison function, works like Perl 
# winner x y =  2 if y beats x, 1 if x beats 1, 0 if it's a tie
winner() {
  local left="$1" right="$2"
  echo $(( (3 + left - right) % 3 ))
}


human_counts=(1 1 1)
human_count=3
computer_counts=(0 0 0)
games=0 human=0 computer=0

PS3="What do you throw? "
while true; do 
  select choice in rock paper scissors quit; do
    if [ -z "$choice" ]; then choice="$REPLY"; fi
    if [ "$choice" = "quit" ]; then
      break 2
    fi
    for (( h=0; h<${#choices[@]}; ++h )); do
      if [ "${choices[h]}" = "$choice" ]; then
        break
      fi
    done
    if (( h < 3 )); then
      break
    fi
    echo "Unrecognized choice.  Try again!"
  done

  let n=RANDOM%human_count
  for (( c=0; c<${#human_counts[@]}; ++c )); do
    let n-=${human_counts[c]}
    if (( n < 0 )); then
       break
    fi
  done
  let computer_counts[c]+=1
  echo 
  echo "You chose ${choices[h]^^}"
  echo "I chose ${choices[c]^^}"
  w="$(winner "$c" "$h")"
  case "$w" in
     2) echo "YOU WIN!"; let human+=1;;
     0) echo "TIE!";;
     1) echo "I WIN!"; let computer+=1;;
     *) echo "winner returned weird result '$w'";;
  esac
  echo
  let games+=1
  (( human_counts[(h+1)%3]+=1, human_count+=1 ))
done

echo 
echo "We played $games games.  You won $human, and I won $computer."
for (( i=0; i<3; ++i )); do
  echo "You picked ${choices[i]} $(( human_counts[ (i+1)%3 ] - 1 )) times."
  echo "I picked ${choices[i]} $(( computer_counts[i] )) times."
done
```



## Wee Basic

Due to how the code works, any key has to be entered for the computer's choice to be generated.

```Wee Basic
let entered=0
let keycode=0
let rcounter=1
print 1 "Enter R for rock, P for paper, or S for scissors. (not case sensitive)"
while entered=0
input human$
if human=$="r"
let human$="rock"
let entered=1
elseif human=$="R"
let human$="rock"
let entered=1
elseif human=$="p"
let human$="paper"
let entered=1
elseif human=$="P"
let human$="paper"
let entered=1
elseif human=$="s"
let human$="scissors"
let entered=1
elseif human=$="S"
let human$="scissors"
let entered=1
elseif entered=0
print 1 "That choice is invalid."
endif
wend
print 1 "Press any key so the computer can make its choice."
while keycode=0
let rcounter=rcounter+1
let keycode=key()
if rcounter=4
let rcounter=1
endif
wend
if rcounter=1
let cpu$="rock"
elseif rcounter=2
let cpu$="paper"
elseif rcounter=3
let cpu$="scissors"
endif
print 1 "You chose"+human$+"."
print 1 "The computer chose"+cpu$+"."
if human$=cpu$
print 1 "You tied."
endif
if human$="rock"
if cpu$="paper"
print 1 "Paper covers rock, so you lose."
endif
if cpu$="scissors"
print 1 "Rock blunts scissors, so you win."
endif
endif
if human$="paper"
if cpu$="rock"
print 1 "Paper covers rock, so you win."
endif
if cpu$="scissors"
print 1 "Scissors cut paper, so you lose."
endif
endif
if human$="scissors"
if cpu$="rock"
print 1 "Rock blunts scissors, so you lose."
endif
if cpu$="paper"
print 1 "Scissors cut paper, so you win."
endif
endif
end
```



## Yabasic


```Yabasic
REM Yabasic 2.763 version

WINNER = 1 : ACTION = 2 : LOSSER = 3
dim word$(10, 3)

for n = 0 to 9
    read word$(n, WINNER), word$(n, ACTION), word$(n, LOSSER)
next n

repeat
    clear screen
    computerChoice$ = word$(ran(10), WINNER)
    print "'Rock, Paper, Scissors, Lizard, Spock!' rules are:\n"
    for n = 0 to 9
        SimonSay(n)
    next n
    print "\nType your choice letter:"
    print "(R)ock, (P)aper, (S)cissors, (L)izard, Spoc(K), (Q)uit\n"
    k$ = upper$(inkey$)
    if k$ = "Q" break
    switch k$
        case "R": humanChoice$ = "Rock" : break
        case "P": humanChoice$ = "Paper" : break
        case "S": humanChoice$ = "Scissors" : break
        case "L": humanChoice$ = "Lizard" : break
        case "K": humanChoice$ = "Spock" : break    
    end switch
    print "Player chose ", humanChoice$, " and Computer chose ", computerChoice$
    for n = 0 to 9
        if word$(n, WINNER) = humanChoice$ and word$(n, LOSSER) = computerChoice$ then
            SimonSay(n)
            print "Winner was Player"
            wp = wp + 1
            break
        elseif word$(n, WINNER) = computerChoice$ and word$(n, LOSSER) = humanChoice$ then
            SimonSay(n)
            print "Winner was Computer"
            wc = wc + 1
            break
        end if    
    next n
    if n = 10 then
        print "Ouch!"
    end if
    punctuation()
    print "\nPress any key to continue"
    inkey$
until(k$ = "Q")

punctuation()
if wp > wc then
    print "Player win"
elseif wc > wp then
    print "Computer win"
else
    print "Tie"
end if

end

sub SimonSay(n)
    print word$(n, WINNER), " ", word$(n, ACTION), " ", word$(n, LOSSER)
end sub

sub punctuation()
    print "\nPlayer = ", wp, "\tComputer = ", wc, "\n"
end sub

data "Scissors","cuts","Paper"
data "Paper","covers","Rock"
data "Rock","crushes","Lizard"
data "Lizard","poisons","Spock"
data "Spock","smashes","Scissors"
data "Scissors","decapites","Lizard"
data "Lizard","eats","Paper"
data "Paper","disproves","Spock"
data "Spock","vaporizes","Rock"
data "Rock","blunts","Scissors"
```


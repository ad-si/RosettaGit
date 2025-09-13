+++
title = "Pig the dice game/Player"
description = ""
date = 2019-02-22T04:25:29Z
aliases = []
[extra]
id = 12298
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Create a dice simulator and scorer of [[Pig the dice game]] and add to it the ability to play the game to at least one strategy.
* State here the play strategies involved.
* Show play during a game here.


As a stretch goal:
* Simulate playing the game a number of times with two players of given strategies and report here summary statistics such as, but not restricted to, the influence of going first or which strategy seems stronger.


;Game Rules:
The game of Pig is a multiplayer game played with a single six-sided die. The
object of the game is to reach 100 points or more.
Play is taken in turns. On each person's turn that person has the option of either

# '''Rolling the dice''': where a roll of two to six is added to their score for that turn and the player's turn continues as the player is given the same choice again; or a roll of 1 loses the player's total points ''for that turn'' and their turn finishes with play passing to the next player.
# '''Holding''': The player's score for that round is added to their total and becomes safe from the effects of throwing a one. The player's turn finishes with play passing to the next player.


;Reference
* [[wp:Pig (dice)|Pig (dice)]]





## Ada


Uses Ada 2012. Uses the package Pig from [[Pig the dice game]].

We implement parameter-driven strategies. A '''strategy''' is defined by two parameters: When the given points in a round reach a '''Bound''' these points are collected, else we roll again. But if the other player is very close to winning (less than '''Final_Run''' points), we switch to an all-or nothing approach, collecting points until we have won -- or rolled a 1. 

The implementation reads five parameters from the command line, in that order: (1) N the number of games to play, (2) the Bound for the first player, (3) the Final_Run for the first player, (4) the Bound for the second player and (5) the Final_Run for the second player. After reading these from the command line (or accepting reasonable defaults), it plays the game N times and counts how often either player wins. 


```Ada
with Pig; with Ada.Text_IO; with Ada.Command_Line;

procedure automatic_Pig is
   
   use Pig;
   
   type Robot is new Actor with record
      Bound: Natural := 20;
      Final_Run: Natural := 0;
   end record;
   function Roll_More(A: Robot; Self, Opponent: Player'Class) return Boolean;
   
   function Roll_More(A: Robot; Self, Opponent: Player'Class) return Boolean is
     ((Self.All_Recent < A.Bound) or 
         else (Opponent.Score-100 > A.Final_Run));
      
   function Arg(Position: Positive; Default: Natural) return Natural is
      package ACL renames Ada.Command_Line;
   begin
      return Natural'Value(ACL.Argument(Position));
   exception
      when Constraint_Error => return Default;
   end Arg;
      
   T: Robot := (Bound => Arg(2, 35), Final_Run => Arg(3, 0));
   F: Robot := (Bound => Arg(4, 20), Final_Run => Arg(5, 30));
      
   T_Wins: Boolean;
   Win_Count: array(Boolean) of Natural := (True=> 0, False => 0);
begin
   for I in 1 .. Arg(1, 1000) loop
      Play(T, F, T_Wins);
      Win_Count(T_Wins) := Win_Count(T_Wins) + 1;
   end loop;
   Ada.Text_IO.Put_Line(Natural'Image(Win_Count(True)) & 
                          Natural'Image(Win_Count(False)));
end Automatic_Pig;
```



The output shows that the (Bound => 25, Final_Run => 25) strategy is just as good as (Bound => 25, Final_Run => 0): whoever is first wins by the same margin: 


```txt
$ ./automatic_pig 1000000 25 25 25 0
 527999 472001
$ ./automatic_pig 1000000 25 0 25 25
 527935 472065
```



## AutoHotkey

Strategies are defined at bottom of script.

Requires additional file from [[Pig the dice game/Player/AutoHotkey]]

```autohotkey
#NoEnv
SetBatchLines, -1
#SingleInstance, Force
#Include Pig_the_dice_game_Optimal_Play.ahkl ; comment if you don't want to bother
Play:=10000 ; this is enough to give 2 digits of accuracy in win ratio
Wins0:=Wins1:=0
Player0(TurnSum, SumMe, SumOpp) {
	Return practical(TurnSum, SumMe, SumOpp) ; set first player function name
}
Player1(TurnSum, SumMe, SumOpp) {
	Return optimal(TurnSum, SumMe, SumOpp) ; set second player function name
}

Loop, % Play
{
	;Random, FirstPlayer, 0, 1 ; to remove advantage of going first
	CurrentPlayer := 0 ; set to FirstPlayer to compare same players with different N's
	Sum0:=Sum1:=0
	Loop
	{
		OtherPlayer:=!CurrentPlayer
		If (Sum%CurrentPlayer%+TurnSum < 100 
			and Player%CurrentPlayer%(TurnSum, Sum%CurrentPlayer%, Sum%OtherPlayer%))
		{
		; Roll
			Random, LastRoll, 1, 6
			If (LastRoll != 1)
			{
				TurnSum += LastRoll
				Continue
			}
			TurnSum := 0
		}
		; Hold
		Sum%CurrentPlayer% += TurnSum
		TurnSum := 0
		If (Sum%CurrentPlayer% >= 100)
		{
			Wins%CurrentPlayer%++
			Break
		}
		CurrentPlayer := !CurrentPlayer
	}
}
Msgbox % "Player 0 won " Round(Wins0/Play*100,0) "%`nPlayer 1 won " Round(Wins1/Play*100,0) "%"

; Random; 1/N is ~ probablity of holding
Random(TurnSum, SumMe, SumOpp, N=9) {
	Random, Roll, 0, N ; increase this last number to increase probability of rolling
	Return Roll
}
; Always roll
Always(TurnSum, SumMe, SumOpp) {
	Return 1
}
; Roll N times; N=6 beats all other RollNx players
RollNx(TurnSum, SumMe, SumOpp, N=6) {
	Static Roll=0
	Return Roll := TurnSum = 0 ? 1 : mod(Roll+1,N+1)
}
; Roll if TurnSum < N; N=19 beats all other RollToN players
RollToN(TurnSum, SumMe, SumOpp, N=19) {
	Return Roll := TurnSum < N
}
; Roll if SumOpp > N or SumMe > N or TurnSum < 21 + (SumOpp - SumMe) / 8
Practical(TurnSum, SumMe, SumOpp, N=72) {
	Return Roll := SumOpp > N or SumMe > N or TurnSum < 21+(SumOpp-SumMe)/8
}
; Optimal play per http://cs.gettysburg.edu/~tneller/nsf/pig/pig.pdf
Optimal(TurnSum, SumMe, SumOpp) {
	Global Optimal
	Roll := Optimal[SumMe,TurnSum,SumOpp+1]
	Return Roll = "" ? 1 : Roll
}
```

<table border=1 cellpadding=1 cellspacing=0 width=500 style='table-layout:fixed;width:375pt'>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;width:60pt'><b>First
  Player win percent</b></td>
  <td width=60 >Second

    Player:</td>
  <td width=60 >Always</td>
  <td width=60 >Random

    N=9</td>
  <td width=60 >RollNx

    N=6</td>
  <td width=60 >RollToN

    N=19</td>
  <td width=60 >Practical

    N=72</td>
  <td width=60 >Optimal</td>
 </tr>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;width:60pt'>First
  Player:</td>
  <td width=60 ></td>
  <td width=60 > </td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'> </td>
 </tr>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;width:60pt'>Always</td>
  <td width=60 > </td>
  <td width=60 style='
  width:45pt'>51%</td>
  <td width=60 style='
  width:45pt'>17%</td>
  <td width=60 style='
  width:45pt'>12%</td>
  <td width=60 style='
  width:45pt'>12%</td>
  <td width=60 style='
  width:45pt'>12%</td>
  <td width=60 style='
  width:45pt'>12%</td>
 </tr>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;
  width:60pt'>Random

    N=9</td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'>84%</td>
  <td width=60 style='
  width:45pt'>52%</td>
  <td width=60 style='
  width:45pt'>30%</td>
  <td width=60 style='
  width:45pt'>29%</td>
  <td width=60 style='
  width:45pt'>27%</td>
  <td width=60 style='
  width:45pt'>26%</td>
 </tr>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;
  width:60pt'>RollNx

    N=6</td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'>89%</td>
  <td width=60 style='
  width:45pt'>74%</td>
  <td width=60 style='
  width:45pt'>54%</td>
  <td width=60 style='
  width:45pt'>53%</td>
  <td width=60 style='
  width:45pt'>49%</td>
  <td width=60 style='
  width:45pt'>49%</td>
 </tr>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;
  width:60pt'>RollToN

    N=19</td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'>89%</td>
  <td width=60 style='
  width:45pt'>76%</td>
  <td width=60 style='
  width:45pt'>54%</td>
  <td width=60 style='
  width:45pt'>54%</td>
  <td width=60 style='
  width:45pt'>49%</td>
  <td width=60 style='
  width:45pt'>49%</td>
 </tr>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;
  width:60pt'>Practical

    N=72</td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'>88%</td>
  <td width=60 style='
  width:45pt'>77%</td>
  <td width=60 style='
  width:45pt'>58%</td>
  <td width=60 style='
  width:45pt'>58%</td>
  <td width=60 style='
  width:45pt'>53%</td>
  <td width=60 style='
  width:45pt'>53%</td>
 </tr>
 <tr height=40 style='height:30.0pt'>
  <td height=40 width=80 style='height:30.0pt;
  width:60pt'>Optimal</td>
  <td width=60 style='
  width:45pt'> </td>
  <td width=60 style='
  width:45pt'>88%</td>
  <td width=60 style='
  width:45pt'>76%</td>
  <td width=60 style='
  width:45pt'>57%</td>
  <td width=60 style='
  width:45pt'>57%</td>
  <td width=60 style='
  width:45pt'>53%</td>
  <td width=60 style='
  width:45pt'>53%</td>
 </tr>
</table>


## C++

The strategies implemented here are pretty simple:<br />
Player 1 is a random player: he chooses a random number between 0 and 9 and if this number is smaller than 5 he'll roll otherwise he holds, unless his round score is zero, in this case he'll roll.<br />
Player 2 always tries to score at least a quarter of the difference between he's current score and MAX_POINTS in a round.<br />
Player 3 always tries to score at least 20 points in a round.<br />
Player 4, just like player 3, always tries to score at least 20 points in a round. But as his round score increases, he gets a little "nervous", what increases the chances that he'll hold.

```cpp

#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int PLAYERS = 4, MAX_POINTS = 100;

//--------------------------------------------------------------------------------------------------
enum Moves { ROLL, HOLD };

//--------------------------------------------------------------------------------------------------
class player
{
public:
    player()                     { current_score = round_score = 0; }
    void addCurrScore()          { current_score += round_score; }
    int getCurrScore()           { return current_score; }
    int getRoundScore()          { return round_score; }
    void addRoundScore( int rs ) { round_score += rs; }
    void zeroRoundScore()        { round_score = 0; }
    virtual int getMove() = 0;
    virtual ~player() {}

protected:
    int current_score, round_score;
};
//--------------------------------------------------------------------------------------------------
class RAND_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;

	if( rand() % 10 < 5 ) return ROLL;
	if( round_score > 0 ) return HOLD;
	return ROLL;
    }
};
//--------------------------------------------------------------------------------------------------
class Q2WIN_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;
		
	int q = MAX_POINTS - current_score;
	if( q < 6 ) return ROLL;
	q /= 4;
	if( round_score < q ) return ROLL;
	return HOLD;
    }
};
//--------------------------------------------------------------------------------------------------
class AL20_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;

	if( round_score < 20 ) return ROLL;
	return HOLD;
    }
};
//--------------------------------------------------------------------------------------------------
class AL20T_Player : public player
{
    virtual int getMove()
    {
	if( round_score + current_score >= MAX_POINTS ) return HOLD;

	int d = ( 100 * round_score ) / 20;
	if( round_score < 20 && d < rand() % 100 ) return ROLL;
	return HOLD;
    }
};
//--------------------------------------------------------------------------------------------------
class Auto_pigGame
{
public:
    Auto_pigGame() 
    {
	_players[0] = new RAND_Player();
	_players[1] = new Q2WIN_Player();
	_players[2] = new AL20_Player();
        _players[3] = new AL20T_Player();
    }

    ~Auto_pigGame()
    {
	delete _players[0];
	delete _players[1];
	delete _players[2];
        delete _players[3];
    }

    void play()
    {
	int die, p = 0;
	bool endGame = false;

	while( !endGame )
	{
	    switch( _players[p]->getMove() )
	    {
		case ROLL:
	    	    die = rand() % 6 + 1;
		    if( die == 1 )
		    {
			cout << "Player " << p + 1 << " rolled " << die << " - current score: " << _players[p]->getCurrScore() << endl << endl;
			nextTurn( p );
			continue;
		    }
		    _players[p]->addRoundScore( die );
		    cout << "Player " << p + 1 << " rolled " << die << " - round score: " << _players[p]->getRoundScore() << endl;
		break;
		case HOLD:
	    	    _players[p]->addCurrScore();
		    cout << "Player " << p + 1 << " holds - current score: " << _players[p]->getCurrScore() << endl << endl;
		    if( _players[p]->getCurrScore() >= MAX_POINTS )
			endGame = true;
		    else nextTurn( p );

	    }
	}
	showScore();
    }

private:
    void nextTurn( int& p )
    {
	_players[p]->zeroRoundScore();
	++p %= PLAYERS;
    }

    void showScore()
    {
	cout << endl;
	cout << "Player   I (RAND): "  << _players[0]->getCurrScore() << endl;
	cout << "Player  II (Q2WIN): " << _players[1]->getCurrScore() << endl;
        cout << "Player III (AL20): " << _players[2]->getCurrScore() << endl;
	cout << "Player  IV (AL20T): "  << _players[3]->getCurrScore() << endl << endl << endl;

	system( "pause" );
    }

    player*	_players[PLAYERS];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    Auto_pigGame pg;
    pg.play();
    return 0;
}
//--------------------------------------------------------------------------------------------------

```

```txt

Player 1 rolled 1 - current score: 0

Player 2 rolled 5 - round score: 5
Player 2 rolled 1 - current score: 0

Player 3 rolled 3 - round score: 3
Player 3 rolled 5 - round score: 8
Player 3 rolled 4 - round score: 12
Player 3 rolled 5 - round score: 17
Player 3 rolled 6 - round score: 23
Player 3 holds - current score: 23

Player 4 rolled 5 - round score: 5
Player 4 rolled 1 - current score: 0

Player 1 rolled 5 - round score: 5
Player 1 holds - current score: 5

Player 2 rolled 6 - round score: 6
Player 2 rolled 2 - round score: 8
Player 2 rolled 3 - round score: 11
Player 2 rolled 2 - round score: 13
Player 2 rolled 3 - round score: 16
Player 2 rolled 6 - round score: 22
Player 2 rolled 6 - round score: 28
Player 2 holds - current score: 28

Player 3 rolled 3 - round score: 3
Player 3 rolled 2 - round score: 5
Player 3 rolled 3 - round score: 8
Player 3 rolled 1 - current score: 23

Player 4 rolled 5 - round score: 5
Player 4 rolled 2 - round score: 7
Player 4 rolled 5 - round score: 12
Player 4 rolled 5 - round score: 17
Player 4 holds - current score: 17

Player 1 rolled 2 - round score: 2
Player 1 holds - current score: 7

Player 2 rolled 6 - round score: 6
Player 2 rolled 3 - round score: 9
Player 2 rolled 2 - round score: 11
Player 2 rolled 6 - round score: 17
Player 2 rolled 1 - current score: 28

Player 3 rolled 4 - round score: 4
Player 3 rolled 3 - round score: 7
Player 3 rolled 1 - current score: 23

Player 4 rolled 3 - round score: 3
Player 4 rolled 6 - round score: 9
Player 4 rolled 4 - round score: 13
Player 4 holds - current score: 30

Player 1 rolled 5 - round score: 5
Player 1 holds - current score: 12

Player 2 rolled 2 - round score: 2
Player 2 rolled 6 - round score: 8
Player 2 rolled 2 - round score: 10
Player 2 rolled 1 - current score: 28

Player 3 rolled 4 - round score: 4
Player 3 rolled 4 - round score: 8
Player 3 rolled 4 - round score: 12
Player 3 rolled 2 - round score: 14
Player 3 rolled 4 - round score: 18
Player 3 rolled 2 - round score: 20
Player 3 holds - current score: 43

Player 4 rolled 2 - round score: 2
Player 4 rolled 2 - round score: 4
Player 4 rolled 1 - current score: 30

Player 1 rolled 1 - current score: 12

Player 2 rolled 2 - round score: 2
Player 2 rolled 2 - round score: 4
Player 2 rolled 3 - round score: 7
Player 2 rolled 2 - round score: 9
Player 2 rolled 1 - current score: 28

Player 3 rolled 6 - round score: 6
Player 3 rolled 4 - round score: 10
Player 3 rolled 3 - round score: 13
Player 3 rolled 5 - round score: 18
Player 3 rolled 5 - round score: 23
Player 3 holds - current score: 66

Player 4 rolled 3 - round score: 3
Player 4 rolled 1 - current score: 30

Player 1 rolled 5 - round score: 5
Player 1 holds - current score: 17

Player 2 rolled 5 - round score: 5
Player 2 rolled 6 - round score: 11
Player 2 rolled 5 - round score: 16
Player 2 rolled 3 - round score: 19
Player 2 holds - current score: 47

Player 3 rolled 1 - current score: 66

Player 4 rolled 2 - round score: 2
Player 4 rolled 5 - round score: 7
Player 4 holds - current score: 37

Player 1 rolled 2 - round score: 2
Player 1 rolled 6 - round score: 8
Player 1 holds - current score: 25

Player 2 rolled 4 - round score: 4
Player 2 rolled 6 - round score: 10
Player 2 rolled 4 - round score: 14
Player 2 holds - current score: 61

Player 3 rolled 4 - round score: 4
Player 3 rolled 6 - round score: 10
Player 3 rolled 2 - round score: 12
Player 3 rolled 5 - round score: 17
Player 3 rolled 6 - round score: 23
Player 3 holds - current score: 89

Player 4 rolled 2 - round score: 2
Player 4 holds - current score: 39

Player 1 rolled 6 - round score: 6
Player 1 holds - current score: 31

Player 2 rolled 1 - current score: 61

Player 3 rolled 5 - round score: 5
Player 3 rolled 1 - current score: 89

Player 4 rolled 2 - round score: 2
Player 4 rolled 2 - round score: 4
Player 4 rolled 3 - round score: 7
Player 4 holds - current score: 46

Player 1 rolled 1 - current score: 31

Player 2 rolled 3 - round score: 3
Player 2 rolled 3 - round score: 6
Player 2 rolled 4 - round score: 10
Player 2 holds - current score: 71

Player 3 rolled 6 - round score: 6
Player 3 rolled 6 - round score: 12
Player 3 holds - current score: 101


Player   I (RAND): 31
Player  II (Q2WIN): 71
Player III (AL20): 101
Player  IV (AL20T): 46

```



## Common Lisp

Just implemented two strategies.  One is actually a variable strategy which holds until a specific value is reached and then turns the dice over.  The default value is 25.  The other is from "Practical Play of the Dice Game Pig" by ToddW. Neller and Clifton G.M. Presser.  Their suggested strategy is "If either player’s score is 71 or higher, roll for the goal. Otherwise, hold at 21 + (j - i) / 8" where j is the other player's score and i is the strategizing player's score.  The scoring is handled by generic functions on the player type.

```lisp
(defclass player ()
    ((score :initform 0 :accessor score)
     (name :initarg :name :accessor name)))
(defun make-player (name)
  (make-instance 'player :name name))
(defmethod has-won ((player player))
  (>= (score player) 100))

(defclass score-based (player) 
  ((score-base :initarg :score-base :initform 25 :accessor score-base)))
(defun make-score-based (name &optional (base 25))
  (make-instance 'score-based :score-base base :name name))
(defmethod roll-again ((player score-based) other turn-score)
  (declare (ignorable other))
  (< turn-score (score-base player)))

(defclass neller (player) ())
(defun make-neller (name) (make-instance 'neller :name name))
(defmethod roll-again ((player neller) other turn-score)
  (let ((other-score (score other)) (my-score (score player)))
     (or
      (> other-score 71) 
      (> my-score 71) 
      (< turn-score (+ 21 (/ (- other-score my-score) 8))))))

(defun query-turn (player other roll added-score)
  (format t "~A: Rolled a ~A - Turn: ~A Current Score: ~A Keep rolling (Y, N or Q)?" 
    (name player)
    roll
    added-score
    (+ added-score (score player)))
  (let ((ret (roll-again player other added-score)))
    (if ret (format t "Y~%") (format t "N~%"))
    ret))

(defun do-turns (player other)
  (do ((new-score 0)
       (take-turn t))
      ((not take-turn) (setf (score player) (+ (score player) new-score)))
    (let ((roll (+ 1 (random 6))))
      (cond
       ((>= (+ (score player) roll new-score) 100)
        (format t "~A rolls a ~A and WINS!~%" (name player) roll)
        (setf new-score (+ new-score roll))
        (setf take-turn nil))
       ((eql 1 roll)
        (format t "Ooh!  Sorry - ~A rolled a 1 and busted!~%" (name player))
        (setf new-score 0)
        (setf take-turn nil))
       (t 
        (setf new-score (+ new-score roll))
        (setf take-turn (query-turn player other roll new-score)))))))
                 
(defun play-pig-winner (p1 p2)
    (do* ((otherplayer p2 curplayer)
          (curplayer p1 (if (eql curplayer p1) p2 p1)))
         ((has-won otherplayer) otherplayer)
      (do-turns curplayer otherplayer)))

(defun play-pig-player (player1 player2) 
  (catch 'quit (format t "Hooray! ~A won the game!"
```

Output:
<lang>Darrell: Rolled a 4 - Turn: 4 Current Score: 4 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 7 Current Score: 7 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 10 Current Score: 10 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Marvin: Rolled a 3 - Turn: 3 Current Score: 3 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 6 - Turn: 9 Current Score: 9 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 3 - Turn: 12 Current Score: 12 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 4 - Turn: 16 Current Score: 16 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 2 - Turn: 2 Current Score: 2 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 4 - Turn: 6 Current Score: 6 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Marvin: Rolled a 3 - Turn: 3 Current Score: 3 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 2 - Turn: 5 Current Score: 5 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 3 - Turn: 3 Current Score: 3 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 5 Current Score: 5 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 6 - Turn: 6 Current Score: 6 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 5 - Turn: 11 Current Score: 11 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 13 Current Score: 13 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Marvin: Rolled a 5 - Turn: 5 Current Score: 5 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 6 - Turn: 11 Current Score: 11 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 3 - Turn: 14 Current Score: 14 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 2 - Turn: 16 Current Score: 16 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 3 - Turn: 19 Current Score: 19 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 2 - Turn: 21 Current Score: 21 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 26 Current Score: 26 Keep rolling (Y, N or Q)?N
Darrell: Rolled a 3 - Turn: 3 Current Score: 3 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 5 Current Score: 5 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Ooh!  Sorry - Marvin rolled a 1 and busted!
Ooh!  Sorry - Darrell rolled a 1 and busted!
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 6 - Turn: 6 Current Score: 6 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 8 Current Score: 8 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 11 Current Score: 11 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 6 - Turn: 17 Current Score: 17 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 20 Current Score: 20 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 23 Current Score: 23 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 5 - Turn: 28 Current Score: 28 Keep rolling (Y, N or Q)?N
Marvin: Rolled a 6 - Turn: 6 Current Score: 32 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 11 Current Score: 37 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 6 - Turn: 6 Current Score: 34 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 5 - Turn: 11 Current Score: 39 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 6 - Turn: 17 Current Score: 45 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 4 - Turn: 21 Current Score: 49 Keep rolling (Y, N or Q)?N
Marvin: Rolled a 5 - Turn: 5 Current Score: 31 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 3 - Turn: 8 Current Score: 34 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 13 Current Score: 39 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 18 Current Score: 44 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 23 Current Score: 49 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 28 Current Score: 54 Keep rolling (Y, N or Q)?N
Darrell: Rolled a 6 - Turn: 6 Current Score: 55 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 5 - Turn: 11 Current Score: 60 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 6 - Turn: 17 Current Score: 66 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 6 - Turn: 6 Current Score: 55 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 5 - Turn: 11 Current Score: 60 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 4 - Turn: 15 Current Score: 64 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 5 - Turn: 20 Current Score: 69 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Marvin: Rolled a 2 - Turn: 2 Current Score: 56 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 7 Current Score: 61 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 4 - Turn: 11 Current Score: 65 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 16 Current Score: 70 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 2 - Turn: 18 Current Score: 72 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Marvin rolled a 1 and busted!
Ooh!  Sorry - Darrell rolled a 1 and busted!
Marvin: Rolled a 6 - Turn: 6 Current Score: 60 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 4 - Turn: 4 Current Score: 53 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 6 - Turn: 10 Current Score: 59 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 6 - Turn: 16 Current Score: 65 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 18 Current Score: 67 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 4 - Turn: 22 Current Score: 71 Keep rolling (Y, N or Q)?N
Marvin: Rolled a 5 - Turn: 5 Current Score: 59 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 10 Current Score: 64 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 5 - Turn: 15 Current Score: 69 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 6 - Turn: 21 Current Score: 75 Keep rolling (Y, N or Q)?Y
Marvin: Rolled a 4 - Turn: 25 Current Score: 79 Keep rolling (Y, N or Q)?N
Darrell: Rolled a 2 - Turn: 2 Current Score: 73 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 4 - Turn: 6 Current Score: 77 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 8 Current Score: 79 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 11 Current Score: 82 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 4 - Turn: 15 Current Score: 86 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Darrell rolled a 1 and busted!
Marvin: Rolled a 4 - Turn: 4 Current Score: 83 Keep rolling (Y, N or Q)?Y
Ooh!  Sorry - Marvin rolled a 1 and busted!
Darrell: Rolled a 5 - Turn: 5 Current Score: 76 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 8 Current Score: 79 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 5 - Turn: 13 Current Score: 84 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 3 - Turn: 16 Current Score: 87 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 6 - Turn: 22 Current Score: 93 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 24 Current Score: 95 Keep rolling (Y, N or Q)?Y
Darrell: Rolled a 2 - Turn: 26 Current Score: 97 Keep rolling (Y, N or Q)?Y
Darrell rolls a 3 and WINS!
Hooray! Darrell won the game!
NIL
```
 


## D

```d
import std.stdio, std.random;

enum nPlayers = 4, maxPoints = 100;

enum Moves { roll, hold }

abstract class Player {
  public:
    final void addCurrScore() pure nothrow {
        current_score += round_score;
    }
    final int getCurrScore() const pure nothrow {
        return current_score;
    }
    final int getRoundScore() const pure nothrow {
        return round_score;
    }
    final void addRoundScore(in int rs) pure nothrow {
        round_score += rs;
    }
    final void zeroRoundScore() pure nothrow {
        round_score = 0;
    }
    Moves getMove();

  protected int current_score, round_score;
}

final class PlayerRand: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;

        if (uniform(0, 2) == 0)
            return Moves.roll;
        if (round_score > 0)
            return Moves.hold;
        return Moves.roll;
    }
}

final class PlayerQ2Win: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;

        int q = maxPoints - current_score;
        if (q < 6)
            return Moves.roll;
        q /= 4;
        if (round_score < q)
            return Moves.roll;
        return Moves.hold;
    }
}

final class PlayerAL20: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;
        if (round_score < 20)
            return Moves.roll;
        return Moves.hold;
    }
}

final class PlayerAL20T: Player {
    override Moves getMove() {
        if (round_score + current_score >= maxPoints)
            return Moves.hold;

        immutable d = (100 * round_score) / 20;
        if (round_score < 20 && d < uniform(0, 100))
            return Moves.roll;
        return Moves.hold;
    }
}

void main() {
    auto players = [new PlayerRand, new PlayerQ2Win,
                    new PlayerAL20, new PlayerAL20T];

    void nextTurn(ref uint p) nothrow {
        players[p].zeroRoundScore;
        p = (p + 1) % nPlayers;
    }

    uint p = 0;
    bool endGame = false;

    while (!endGame) {
        final switch (players[p].getMove) {
            case Moves.roll:
                immutable die = uniform(1, 7);

                if (die == 1) {
                    writeln("Player ", p + 1, " rolled ", die,
                            " - current score: ",
                            players[p].getCurrScore, "\n");
                    nextTurn(p);
                    continue;
                }
                players[p].addRoundScore(die);
                writeln("Player ", p + 1, " rolled ", die,
                        " - round score: ",
                        players[p].getRoundScore);
                break;

            case Moves.hold:
                players[p].addCurrScore;
                writeln("Player ", p + 1,
                        " holds - current score: ",
                        players[p].getCurrScore, "\n");
                if (players[p].getCurrScore >= maxPoints)
                    endGame = true;
                else
                    nextTurn(p);

        }
    }

    writeln;
    writeln("Player   I (Rand):  ", players[0].getCurrScore);
    writeln("Player  II (Q2Win): ", players[1].getCurrScore);
    writeln("Player III (AL20):  ", players[2].getCurrScore);
    writeln("Player  IV (AL20T): ", players[3].getCurrScore, "\n\n");
}
```


The output is similar to the C++ entry.


## Erlang

Four players take turns starting. Their strategy is how long to wait before holding. They aim for 5, 10, 15 and 20 rolls. Player20 managed better than I thought it would.

```Erlang

-module( pig_dice_player ).

-export( [task/0] ).

task() ->
    Goal = pig_dice:goal(),
    Players_holds = [{"Player"++ erlang:integer_to_list(X), X} || X <- [5, 10, 15, 20]],
    Fun = fun ({Player, Total}, Dict) when Total >= Goal -> dict:update_counter( Player, 1, Dict );
        (_Player_total, Dict) -> Dict
        end,
    Dict = lists:foldl( Fun, dict:new(), task_play(Players_holds, Goal, []) ),
    display( dict:to_list(Dict) ).


display( Results ) ->
        [{Name, Total} | Rest] = lists:reverse( lists:keysort(2, Results) ),
        io:fwrite( "Winner is ~p with total of ~p wins~n", [Name, Total] ),
        io:fwrite( "Then follows: " ),
        [io:fwrite("~p with ~p~n", [N, T]) || {N, T} <- Rest].

is_goal_reached( Score, Player, Goal, Player, Game ) ->
        Score + proplists:get_value(Player, pig_dice:players_totals(Game)) >= Goal;
is_goal_reached( _Score, _Other_player, _Goal, _Player, _Game ) -> false.

loop( Hold, Hold, Goal, Player, Game ) ->
    pig_dice:hold( Player, Game ),
    loop( 1, Hold, Goal, Player, Game );
loop( N, Hold, Goal, Player, Game ) ->
    loop_await_my_turn( pig_dice:player_name(Game), Player, Game ),
    pig_dice:roll( Player, Game ),
    Is_goal_reached = is_goal_reached( pig_dice:score(Game), pig_dice:player_name(Game), Goal, Player, Game ),
    loop_done( Is_goal_reached, N, Hold, Goal, Player, Game ).

loop_await_my_turn( Player, Player, _Game ) -> ok;
loop_await_my_turn( _Other, Player, Game ) -> loop_await_my_turn( pig_dice:player_name(Game), Player, Game ).

loop_done( true, _N, _Hold, _Goal, Player, Game ) -> pig_dice:hold( Player, Game );
loop_done( false, N, Hold, Goal, Player, Game ) -> loop( N + 1, Hold, Goal, Player, Game ).

rotate(	[H | T] ) -> T ++ [H].

task_play( Players_holds, _Goal, Acc ) when erlang:length(Players_holds) =:= erlang:length(Acc) -> lists:flatten( Acc );
task_play( Players_holds, Goal, Acc ) ->
    Results = [task_play_n(Players_holds, Goal) || _X <- lists:seq(1, 100)],
    task_play( rotate(Players_holds), Goal, [Results | Acc] ).

task_play_n( Players_holds, Goal ) ->
    Game = pig_dice:game( [X || {X, _Y} <- Players_holds] ),
    Pids = [erlang:spawn(fun() -> loop(1, Y, Goal, X, Game) end) || {X, Y} <- Players_holds],
    receive
    {pig, Result, Game} ->
	[erlang:exit(X, kill) || X <- [Game | Pids]],
        Result
    end.

```

```txt

115> pig_dice_player:task().
Winner is "Player10" with total of 121 wins
Then follows: "Player15" with 120
"Player20" with 85
"Player5" with 74

```



## Go


```go
package pig

import (
	"fmt"
	"math/rand"
	"time"
)

type (
	PlayerID   int
	MessageID  int
	StrategyID int

	PigGameData struct {
		player        PlayerID
		turnCount     int
		turnRollCount int
		turnScore     int
		lastRoll      int
		scores        [2]int
		verbose       bool
	}
)

const (
	// Status messages
	gameOver = iota
	piggedOut
	rolls
	pointSpending
	holds
	turn
	gameOverSummary
	// Players
	player1  = PlayerID(0)
	player2  = PlayerID(1)
	noPlayer = PlayerID(-1)
	// Max score
	maxScore = 100
	// Strategies
	scoreChaseStrat = iota
	rollCountStrat
)

// Returns "s" if n != 1
func pluralS(n int) string {
	if n != 1 {
		return "s"
	}
	return ""
}

// Creates an intializes a new PigGameData structure, returns a *PigGameData
func New() *PigGameData {
	return &PigGameData{0, 0, 0, 0, 0, [2]int{0, 0}, false}
}

// Create a status message for a given message ID
func (pg *PigGameData) statusMessage(id MessageID) string {
	var msg string
	switch id {
	case gameOver:
		msg = fmt.Sprintf("Game is over after %d turns", pg.turnCount)
	case piggedOut:
		msg = fmt.Sprintf("    Pigged out after %d roll%s", pg.turnRollCount, pluralS(pg.turnRollCount))
	case rolls:
		msg = fmt.Sprintf("    Rolls %d", pg.lastRoll)
	case pointSpending:
		msg = fmt.Sprintf("    %d point%s pending", pg.turnScore, pluralS(pg.turnScore))
	case holds:
		msg = fmt.Sprintf("    Holds after %d turns, adding %d points for a total of %d", pg.turnRollCount, pg.turnScore, pg.PlayerScore(noPlayer))
	case turn:
		msg = fmt.Sprintf("Player %d's turn:", pg.player+1)
	case gameOverSummary:
		msg = fmt.Sprintf("Game over after %d turns\n player 1 %d\n player 2 %d\n", pg.turnCount, pg.PlayerScore(player1), pg.PlayerScore(player2))
	}
	return msg
}

// Print a status message, if pg.Verbose is true
func (pg *PigGameData) PrintStatus(id MessageID) {
	if pg.verbose {
		fmt.Println(pg.statusMessage(id))
	}
}

// Play a given strategy
func (pg *PigGameData) Play(id StrategyID) (keepPlaying bool) {
	if pg.GameOver() {
		pg.PrintStatus(gameOver)
		return false
	}

	if pg.turnCount == 0 {
		pg.player = player2
		pg.NextPlayer()
	}

	pg.lastRoll = rand.Intn(6) + 1
	pg.PrintStatus(rolls)
	pg.turnRollCount++
	if pg.lastRoll == 1 {
		pg.PrintStatus(piggedOut)
		pg.NextPlayer()
	} else {
		pg.turnScore += pg.lastRoll
		pg.PrintStatus(pointSpending)
		success := false
		switch id {
		case scoreChaseStrat:
			success = pg.scoreChaseStrategy()
		case rollCountStrat:
			success = pg.rollCountStrategy()
		}
		if success {
			pg.Hold()
			pg.NextPlayer()
		}
	}
	return true
}

// Get the score for a given player
func (pg *PigGameData) PlayerScore(id PlayerID) int {
	if id == noPlayer {
		return pg.scores[pg.player]
	}
	return pg.scores[id]
}

// Check if the game is over
func (pg *PigGameData) GameOver() bool {
	return pg.scores[player1] >= maxScore || pg.scores[player2] >= maxScore
}

// Returns the Player ID if there is a winner, or -1
func (pg *PigGameData) Winner() PlayerID {
	for index, score := range pg.scores {
		if score >= maxScore {
			return PlayerID(index)
		}
	}
	return noPlayer
}

// Get the ID of the other player
func (pg *PigGameData) otherPlayer() PlayerID {
	// 0 becomes 1, 1 becomes 0
	return 1 - pg.player
}

func (pg *PigGameData) Hold() {
	pg.scores[pg.player] += pg.turnScore
	pg.PrintStatus(holds)
	pg.turnRollCount, pg.turnScore = 0, 0
}

func (pg *PigGameData) NextPlayer() {
	pg.turnCount++
	pg.turnRollCount, pg.turnScore = 0, 0
	pg.player = pg.otherPlayer()
	pg.PrintStatus(turn)
}

func (pg *PigGameData) rollCountStrategy() bool {
	return pg.turnRollCount >= 3
}

func (pg *PigGameData) scoreChaseStrategy() bool {
	myScore := pg.PlayerScore(pg.player)
	otherScore := pg.PlayerScore(pg.otherPlayer())
	myPendingScore := pg.turnScore + myScore
	return myPendingScore >= maxScore || myPendingScore > otherScore || pg.turnRollCount >= 5
}

// Run the simulation
func main() {
	// Seed the random number generator
	rand.Seed(time.Now().UnixNano())

	// Start a new game
	pg := New()
	pg.verbose = true
	strategies := [2]StrategyID{scoreChaseStrat, rollCountStrat}

	// Play until game over
	for !pg.GameOver() {
		pg.Play(strategies[pg.player])
	}
	pg.PrintStatus(gameOverSummary)
}
```

Sample run, player one just tries to keep ahead, while player two always tries to take three rolls, no more.

```txt

Player 1's turn:
    Rolls 4
    4 points pending
    Holds after 1 turns, adding 4 points for a total of 4
Player 2's turn:
    Rolls 4
    4 points pending
    Rolls 1
    Pigged out after 2 rolls
Player 1's turn:
    Rolls 6
    6 points pending
    Holds after 1 turns, adding 6 points for a total of 10
Player 2's turn:
    Rolls 6
    6 points pending
    Rolls 3
    9 points pending
    Rolls 4
    13 points pending
    Holds after 3 turns, adding 13 points for a total of 13
Player 1's turn:
    Rolls 4
    4 points pending
    Holds after 1 turns, adding 4 points for a total of 14
Player 2's turn:
    Rolls 4
    4 points pending
    Rolls 6
    10 points pending
    Rolls 1
    Pigged out after 3 rolls
Player 1's turn:
    Rolls 4
    4 points pending
    Holds after 1 turns, adding 4 points for a total of 18
Player 2's turn:
    Rolls 3
    3 points pending
    Rolls 4
    7 points pending
    Rolls 2
    9 points pending
    Holds after 3 turns, adding 9 points for a total of 22
Player 1's turn:
    Rolls 2
    2 points pending
    Rolls 1
    Pigged out after 2 rolls
Player 2's turn:
    Rolls 1
    Pigged out after 1 roll
Player 1's turn:
    Rolls 4
    4 points pending
    Rolls 1
    Pigged out after 2 rolls
Player 2's turn:
    Rolls 5
    5 points pending
    Rolls 1
    Pigged out after 2 rolls
Player 1's turn:
    Rolls 5
    5 points pending
    Holds after 1 turns, adding 5 points for a total of 23
Player 2's turn:
    Rolls 5
    5 points pending
    Rolls 4
    9 points pending
    Rolls 4
    13 points pending
    Holds after 3 turns, adding 13 points for a total of 35
Player 1's turn:
    Rolls 1
    Pigged out after 1 roll
Player 2's turn:
    Rolls 3
    3 points pending
    Rolls 3
    6 points pending
    Rolls 2
    8 points pending
    Holds after 3 turns, adding 8 points for a total of 43
Player 1's turn:
    Rolls 1
    Pigged out after 1 roll
Player 2's turn:
    Rolls 6
    6 points pending
    Rolls 4
    10 points pending
    Rolls 1
    Pigged out after 3 rolls
Player 1's turn:
    Rolls 4
    4 points pending
    Rolls 1
    Pigged out after 2 rolls
Player 2's turn:
    Rolls 2
    2 points pending
    Rolls 4
    6 points pending
    Rolls 2
    8 points pending
    Holds after 3 turns, adding 8 points for a total of 51
Player 1's turn:
    Rolls 1
    Pigged out after 1 roll
Player 2's turn:
    Rolls 4
    4 points pending
    Rolls 2
    6 points pending
    Rolls 3
    9 points pending
    Holds after 3 turns, adding 9 points for a total of 60
Player 1's turn:
    Rolls 2
    2 points pending
    Rolls 6
    8 points pending
    Rolls 3
    11 points pending
    Rolls 6
    17 points pending
    Rolls 4
    21 points pending
    Holds after 5 turns, adding 21 points for a total of 44
Player 2's turn:
    Rolls 4
    4 points pending
    Rolls 2
    6 points pending
    Rolls 3
    9 points pending
    Holds after 3 turns, adding 9 points for a total of 69
Player 1's turn:
    Rolls 6
    6 points pending
    Rolls 5
    11 points pending
    Rolls 6
    17 points pending
    Rolls 5
    22 points pending
    Rolls 4
    26 points pending
    Holds after 5 turns, adding 26 points for a total of 70
Player 2's turn:
    Rolls 5
    5 points pending
    Rolls 4
    9 points pending
    Rolls 2
    11 points pending
    Holds after 3 turns, adding 11 points for a total of 80
Player 1's turn:
    Rolls 6
    6 points pending
    Rolls 6
    12 points pending
    Holds after 2 turns, adding 12 points for a total of 82
Player 2's turn:
    Rolls 2
    2 points pending
    Rolls 3
    5 points pending
    Rolls 3
    8 points pending
    Holds after 3 turns, adding 8 points for a total of 88
Player 1's turn:
    Rolls 5
    5 points pending
    Rolls 2
    7 points pending
    Holds after 2 turns, adding 7 points for a total of 89
Player 2's turn:
    Rolls 3
    3 points pending
    Rolls 2
    5 points pending
    Rolls 5
    10 points pending
    Holds after 3 turns, adding 10 points for a total of 98
Player 1's turn:
    Rolls 6
    6 points pending
    Rolls 3
    9 points pending
    Rolls 6
    15 points pending
    Holds after 3 turns, adding 15 points for a total of 104
Player 2's turn:
Game over after 32 turns
 player 1 104
 player 2 98

```



## Haskell


Implemented 4 strategies:

 - player1 always rolls until he gets 20 or more
 - player2 always rolls four times
 - player3 rolls three times until she gets more than 60 points, then she rolls until she gets 20 or more
 - player4 rolls 3/4 of the time, 1/4 he holds, but if he gets a score more than 75 he goes for the win


```Haskell

{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Random (randomRIO)
import Text.Printf   (printf)

data PInfo = PInfo { stack :: Int
                   , score :: Int
                   , rolls :: Int
                   , next  :: Bool
                   , won   :: Bool
                   , name  :: String
                   }

type Strategy = [PInfo] -> IO ()

roll :: [PInfo] -> IO [PInfo]
roll (pinfo:xs) = do
  face <- randomRIO (1, 6)
  case (face, face + stack pinfo + score pinfo) of
      (1,_)            -> do
          printf "%s rolled 1 - stack is being resetted\n\n" (name pinfo)
          return $ pinfo { stack = 0, rolls = 0, next = True } : xs
      (_,x) | x >= 100 -> do
          printf "%s rolled %i - stack is now %i + score %i => %i - I won!\n" (name pinfo) face (face + stack pinfo) (score pinfo) x
          return $ pinfo { won = True } : xs
      (_,_)            -> do
          printf "%s rolled %i - stack is now %i\n" (name pinfo) face (face + (stack pinfo))
          return $ pinfo { stack = face + (stack pinfo), rolls = 1 + (rolls pinfo) } : xs

hold :: [PInfo] -> IO [PInfo]
hold (pinfo:xs) = do
  let score' = stack pinfo + score pinfo
  printf "%s holds - score is now %i\n\n" (name pinfo) score'
  return $ pinfo { score = score', stack = 0, rolls = 0, next = True } : xs


logic :: Strategy -> Strategy -> Strategy
logic _      _      ((won -> True)    : xs) = return ()
logic _      strat2 (p@(next -> True) : xs) = strat2 $ xs ++ [p { next = False }]
logic strat1 _      (pinfo            : xs) = strat1 (pinfo : xs)

strat1 :: Strategy
strat1 (pinfo:xs)
  | stack pinfo < 20 = roll (pinfo:xs) >>= logic strat1 strat2
  | otherwise        = hold (pinfo:xs) >>= logic strat1 strat2

strat2 :: Strategy
strat2 (pinfo:xs)
  | rolls pinfo < 4 = roll (pinfo:xs) >>= logic strat2 strat3
  | otherwise       = hold (pinfo:xs) >>= logic strat2 strat3

strat3 :: Strategy
strat3 (pinfo:xs)
  | rolls pinfo < 3 && score pinfo < 60 = roll (pinfo:xs) >>= logic strat3 strat4
  | stack pinfo < 20                    = roll (pinfo:xs) >>= logic strat3 strat4
  | otherwise                           = hold (pinfo:xs) >>= logic strat3 strat4

strat4 :: Strategy
strat4 (pinfo:xs) | score pinfo > 75 = roll (pinfo:xs) >>= logic strat4 strat1
strat4 (pinfo:xs) = do
  chance <- randomRIO (0, 3) :: IO Int
  case chance of
      0  -> hold (pinfo:xs) >>= logic strat4 strat1
      _  -> roll (pinfo:xs) >>= logic strat4 strat1

main :: IO ()
main = do
  let pInfo = PInfo 0 0 0 False False ""
      p1    = pInfo { name = "Peter"   }
      p2    = pInfo { name = "Mia"     }
      p3    = pInfo { name = "Liz"     }
      p4    = pInfo { name = "Stephen" }
  strat1 [p1, p2, p3, p4]

```


Example output:

```txt


Peter rolled 5 - stack is now 5
Peter rolled 5 - stack is now 10
Peter rolled 4 - stack is now 14
Peter rolled 6 - stack is now 20
Peter holds - score is now 20

Mia rolled 4 - stack is now 4
Mia rolled 1 - stack is being resetted

Liz rolled 4 - stack is now 4
Liz rolled 6 - stack is now 10
Liz rolled 4 - stack is now 14
Liz rolled 3 - stack is now 17
Liz rolled 3 - stack is now 20
Liz holds - score is now 20

Stephen rolled 6 - stack is now 6
Stephen rolled 1 - stack is being resetted

Peter rolled 3 - stack is now 3
Peter rolled 6 - stack is now 9

...

Stephen rolled 1 - stack is being resetted

Peter rolled 4 - stack is now 4
Peter rolled 2 - stack is now 6
Peter rolled 5 - stack is now 11
Peter rolled 2 - stack is now 13
Peter rolled 5 - stack is now 18
Peter rolled 6 - stack is now 24
Peter holds - score is now 87

Mia rolled 5 - stack is now 5
Mia rolled 1 - stack is being resetted

Liz rolled 3 - stack is now 3
Liz rolled 1 - stack is being resetted

Stephen rolled 6 - stack is now 6
Stephen rolled 2 - stack is now 8
Stephen rolled 5 - stack is now 13
Stephen rolled 4 - stack is now 17
Stephen holds - score is now 43

Peter rolled 4 - stack is now 4
Peter rolled 2 - stack is now 6
Peter rolled 5 - stack is now 11
Peter rolled 6 - stack is now 17 + score 87 => 104 - I won!

```


To test the distribution by yourself (in parallel):


```Haskell


-- add this to the top
import Control.Concurrent.ParallelIO.Global (parallel, stopGlobalPool)
import Data.List (sort, group)

-- replace "logic _      _      ((won -> True)    : xs) = return ()" with
  logic _      _      (p@(won -> True)    : xs) = return $ name p

-- replace strat1 [p1, p2, p3, p4] in main with
  let lists = replicate 100000 [p1, p2, p3, p4]
  results <- parallel $ map strat1 lists
  stopGlobalPool
  print $ map length $ group $ sort results

-- replace type Strategy = [PInfo] -> IO () with
  type Strategy = [PInfo] -> IO String

-- comment every printf in "roll" and "hold"

-- compile with
-- ghc FILENAME.hs -O2 -threaded -with-rtsopts="-N4" -o dice


```


Distribution:


```txt


Strat1 = 31878  => ~ 32%
Strat2 = 21953  => ~ 22%
Strat3 = 39022  => ~ 39%
Strat4 = 7147   => ~ 7%

out of 100 000 tests.

```



## Java

This is a full implementation of pig dice with 4 built in bots. Their behavior can easily be modified and additional bots can be added as well.

This is the main file, Pigdice.java

```Java
import java.util.Scanner;

public class Pigdice {

	public static void main(String[] args) {
		Scanner scan = new Scanner(System.in);
		int players = 0;
		
		//Validate the input
		while(true) {
			//Get the number of players
			System.out.println("Hello, welcome to Pig Dice the game! How many players? ");
			if(scan.hasNextInt()) {
				
				//Gotta be more than 0
				int nextInt = scan.nextInt();
				if(nextInt > 0) {
					players = nextInt;
					break;
				}
			}
			else {
				System.out.println("That wasn't an integer. Try again. \n");
				scan.next();
			}
		}
		System.out.println("Alright, starting with " + players + " players. \n");
		
		//Start the game
		play(players, scan);
		
		scan.close();
	}
	
	public static void play(int group, Scanner scan) {
		//Set the number of strategies available.
		final int STRATEGIES = 5;
		
		//Construct the dice- accepts an int as an arg for number of sides, but defaults to 6.
		Dice dice = new Dice();
		
		//Create an array of players and initialize them to defaults.
		Player[] players = new Player[group];
		for(int count = 0; count < group; count++) {
			players[count] = new Player(count);
			System.out.println("Player " + players[count].getNumber() + "  is alive! ");
		}
		
		/*****Print strategy options here. Modify Player.java to add strategies. *****/
		System.out.println("Each strategy is numbered 0 - " + (STRATEGIES - 1) + ". They are as follows: ");
		System.out.println(">> Enter '0' for a human player. ");
		System.out.println(">> Strategy 1 is a basic strategy where the AI rolls until 20+ points and holds unless the current max is 75+.");
		System.out.println(">> Strategy 2 is a basic strategy where the AI, after 3 successful rolls, will randomly decide to roll or hold. ");
		System.out.println(">> Strategy 3 is similar to strategy 2, except it's a little gutsier and will attempt 5 successful rolls. ");
		System.out.println(">> Strategy 4 is like a mix between strategies 1 and 3. After turn points are >= 20 and while max points are still less than 75, it will randomly hold or roll. ");
		
		//Get the strategy for each player
		for(Player player : players) {
			System.out.println("\nWhat strategy would you like player " + player.getNumber() + " to use? ");

			//Validate the strategy is a real strategy.
			while(true) {
				if(scan.hasNextInt()) {
					int nextInt = scan.nextInt();
					if (nextInt < Strategy.STRATEGIES.length) {
						player.setStrategy(Strategy.STRATEGIES[nextInt]);
						break;
					}
				}
				else {
					System.out.println("That wasn't an option. Try again. ");
					scan.next();
				}
			}
		}
		
		//Here is where the rules for the game are programmatically defined.
		int max = 0;
		while(max < 100) {
			
			//Begin the round
			for(Player player : players) {
				System.out.println(">> Beginning Player " + player.getNumber() + "'s turn. ");
				
				//Set the points for the turn to 0
				player.setTurnPoints(0);
				
				//Determine whether the player chooses to roll or hold.
				player.setMax(max);
				while(true) {
					Move choice = player.choose();
					if(choice == Move.ROLL) {
						int roll = dice.roll();
						System.out.println("   A " + roll + " was rolled. ");
						player.setTurnPoints(player.getTurnPoints() + roll);
						
						//Increment the player's built in iterator.
						player.incIter();
						
						//If the player rolls a 1, their turn is over and they gain 0 points this round.
						if(roll == 1) {
							player.setTurnPoints(0);
							break;
						}
					}
					//Check if the player held or not.
					else {
						System.out.println("   The player has held. ");
						break;
					}
				}
				
				//End the turn and add any accumulated points to the player's pool.
				player.addPoints(player.getTurnPoints());
				System.out.println("   Player " + player.getNumber() + "'s turn is now over. Their total is " + player.getPoints() + ". \n");
				
				//Reset the player's built in iterator.
				player.resetIter();
				
				//Update the max score if necessary.
				if(max < player.getPoints()) {
					max = player.getPoints();
				}
				
				//If someone won, stop the game and announce the winner.
				if(max >= 100) {
					System.out.println("Player " + player.getNumber() + " wins with " + max + " points! End scores: ");
					
					//Announce the final scores.
					for(Player p : players) {
						System.out.println("Player " + p.getNumber() + " had " + p.getPoints() + " points. ");
					}
					break;
				}
			}
		}
		
	}
	
}
```


This is the Player.java class file.

```Java
public class Player {

	private int points = 0;
	private int turnPoints = 0;
	private Strategy strategy = null;
	private int max = 0;
	private int number;
	private int iter = 0;
	
	public Player(int val) {
		number = val;
	}
	
	public int getPoints() {
		return points;
	}
	public int getTurnPoints() {
		return turnPoints;
	}
	public int getMax() {
		return max;
	}
	public int getNumber() {
		return number;
	}
	public int getIter() {
		return iter;
	}
	public void addPoints(int val) {
		points += val;
	}
	public void setTurnPoints(int val) {
		turnPoints = val;
	}
	public void setStrategy(Strategy strat) {
		strategy = strat;
	}
	public void setMax(int val) {
		max = val;
	}
	public void setNumber(int val) {
		number = val;
	}
	public void resetIter() {
		iter = 0;
	}
	public void incIter() {
		iter++;
	}
	public void aiIntro() {
		System.out.println("   Player " + getNumber() + "'s turn points are " + getTurnPoints() + ". Their total is " + getPoints() + ". ");
		System.out.println("   The max points any player currently has is " + getMax() + ". ");
	}
	public Move choose() {
		return strategy.choose(this);
	}

}
```


This is the Move.java class file.

```Java
public enum Move { ROLL, HOLD }
```


This is the Strategy.java class file.

```Java
import java.util.Scanner;

public interface Strategy {

	Move choose(Player player);
	
	static final Scanner str = new Scanner(System.in);
	static final Dice die = new Dice(2);
	static final int ROOF = 75;
	static final int FLOOR = 20;
	static final int BASEMENT = 10;
	
	/*****MODIFY THIS AREA TO MODIFY THE STRATEGIES*****/
	//Determine whether to roll or hold based on the strategy for this player.
	public static final Strategy[] STRATEGIES = {
		
		//Strategy 0 is a user-defined strategy
		player -> {
			System.out.println("   Your turn points are " + player.getTurnPoints() + ". Your total is " + player.getPoints() + ". ");
			System.out.println("   The max points any player currently has is " + player.getMax() + ". (H)old or (R)oll?");
			System.out.println("   Enter 'h' to hold and 'r' to roll. ");
			while(true) {
				String input = null;
				if(str.hasNextLine()) {
					input = str.nextLine();
				}
				if(input.contains("r")) {
					return Move.ROLL;
				}
				else if(input.contains("h")) {
					return Move.HOLD;
				}
				else {
					System.out.println("  Enter an h or an r. \n");
					System.out.println(input);
				}
			}
		},
		
		//Strategy 1 is a basic strategy where the AI rolls until 20+ points and holds unless the current max is 75+.
		player -> {
			player.aiIntro();
			if(player.getTurnPoints() < FLOOR || player.getMax() >= ROOF) {
				if(player.getTurnPoints() >= (100 - player.getPoints())) {
					return Move.HOLD;					
				}
				else {
					return Move.ROLL;
				}
			}
			else {
				return Move.HOLD;
			}
		},
		
		//Strategy 2 is a basic strategy where the AI, after 3 successful rolls, will randomly decide to roll or hold.
		player -> {
			player.aiIntro();
			if(player.getPoints() == 0 && player.getTurnPoints() >= (BASEMENT / 2)) {
				return Move.HOLD;
			}
			if(player.getIter() > 3) {
				int roll = die.roll();
				
				if(roll == 1) {
					return Move.HOLD;
				}
				else {
					return Move.ROLL;
				}
			}
			else {
				return Move.ROLL;
			}
		},
		
		//Strategy 3 is similar to strategy 2, except it's a little gutsier and will attempt 5 successful rolls.
		player -> {
			player.aiIntro();
			if(player.getIter() > 5) {
				int roll = die.roll();
				
				if(roll == 1) {
					return Move.HOLD;
				}
				else {
					return Move.ROLL;
				}
			}
			else if(player.getPoints() < BASEMENT && player.getTurnPoints() > BASEMENT) {
				return Move.HOLD;
			}
			else {
				return Move.ROLL;
			}
		},
		
		/*Strategy 4 is like a mix between strategies 1 and 3. After turn points are >= 20 and while max points are still less than 75, it will randomly hold or roll.
		Unless their total is zero, in which case they'll hold at 10 points. */
		player -> {
			player.aiIntro();
			if(player.getPoints() == 0 && player.getTurnPoints() >= (BASEMENT / 2)) {
				return Move.HOLD;
			}
			else if(player.getTurnPoints() < FLOOR || player.getMax() >= ROOF) {
				if(player.getTurnPoints() >= (100 - player.getPoints())) {
					return Move.HOLD;					
				}
				else {
					return Move.ROLL;
				}
			}
			else if(player.getTurnPoints() > FLOOR && player.getMax() <= ROOF) {
				int roll = die.roll();
				
				if(roll == 1) {
					return Move.HOLD;
				}
				else {
					return Move.ROLL;
				}
			}
			else {
				return Move.HOLD;
			}
		}
	};

}
```


And finally, this is the Dice.java class file. It's pretty self-explanatory.

```Java
import java.util.Random;

public class Dice {
	Random rand = new Random();
	int sides;
	Dice(int numSides) {
		sides = numSides;
	}
	Dice() {
		sides = 6;
	}
	int roll() {
		return rand.nextInt(sides) + 1;
	}
}
```


Here's a small sample output using only bots (even though it fully supports human players too). A full game simulation can obviously be MUCH longer.

```txt

Hello, welcome to Pig Dice the game! How many players? 
4
Alright, starting with 4 players. 

Player 0  is alive! 
Player 1  is alive! 
Player 2  is alive! 
Player 3  is alive! 
Each strategy is numbered 0 - 4. They are as follows: 
>> Enter '0' for a human player. 
>> Strategy 1 is a basic strategy where the AI rolls until 20+ points and holds unless the current max is 75+.
>> Strategy 2 is a basic strategy where the AI, after 3 successful rolls, will randomly decide to roll or hold. 
>> Strategy 3 is similar to strategy 2, except it's a little gutsier and will attempt 5 successful rolls. 
>> Strategy 4 is like a mix between strategies 1 and 3. After turn points are >= 20 and while max points are still less than 75, it will randomly hold or roll. 

What strategy would you like player 0 to use? 
1
What strategy would you like player 1 to use? 
2
What strategy would you like player 2 to use? 
3
What strategy would you like player 3 to use? 
4
>> Beginning Player 0's turn. 
   Player 0's turn points are 0. Their total is 0. 
   The max points any player currently has is 0. 
   A 4 was rolled. 
   Player 0's turn points are 4. Their total is 0. 
   The max points any player currently has is 0. 
   A 4 was rolled. 
   Player 0's turn points are 8. Their total is 0. 
   The max points any player currently has is 0. 
   A 1 was rolled. 
   Player 0's turn is now over. Their total is 0. 

>> Beginning Player 1's turn. 
...
...
*SNIP*
...
...
Player 3 wins with 102 points! End scores: 
Player 0 had 20 points. 
Player 1 had 73 points. 
Player 2 had 66 points. 
Player 3 had 102 points. 

```



## J

This is a partial implementation of the current task.

This is a routine to estimate the value of rolling, given the current total of rolls which the player is building (left argument) and the current total of rolls which are a permanent part of the player's score (right argument).

If the expected value is positive, it's probably in the best interest of the player to take the roll.  That said, a more sophisticated strategy might play cautiously when a player is sufficiently ahead of the other player(s).

```j
pigval=:4 :0
  (+/%#)(-x),}.(1+i.6)<.100-y+x
)
```

Examples:

```j
   10 pigval 90
_1.66667
```

If we have 10 points from our current rolls and have 90 permanent points, rolling again is a bad idea.

```j
   0 5 10 15 20 pigval"0/60 65 70 75 80 85 90 95 100
 3.33333  3.33333  3.33333   3.33333  3.33333 3.33333  3.33333   3.16667   0
     2.5      2.5      2.5       2.5      2.5     2.5  2.33333 _0.833333  _5
 1.66667  1.66667  1.66667   1.66667  1.66667     1.5 _1.66667  _5.83333 _10
0.833333 0.833333 0.833333  0.833333 0.666667    _2.5 _6.66667  _10.8333 _15
       0        0        0 _0.166667 _3.33333    _7.5 _11.6667  _15.8333 _20
```

If we have 70 permanent points (or less) we should probably re-roll when our uncommitted rolls total to less than 20.

```j
   (1+i.19)  ([,:1+i:~)  +/ 0 < pigval"0/~ 1+i.100
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
98 97 96 95 93 92 91 90 89 87 86 85 84 82 81 80 78 77 75
```

This is a table of decision points.  First row represents sum of our current uncommitted rolls.  Second row represents the maximum permanent score where you should roll again with that number of uncommitted points, if we are using this estimation mechanism to choose our actions. Note that the first four columns here should have some obvious validity -- for example, if we have 96 permanent points and we have rolled 4 uncommitted points, we have won the game and we gain nothing from rerolling... Note also that this decision mechanism says we should never reroll if we have at least 20 uncommitted points.



## Julia


```julia
mutable struct Player
    score::Int
    ante::Int
    wins::Int
    losses::Int
    strategy::Pair{String, Function}
end

randomchoicetostop(player, group) = rand(Bool)
variablerandtostop(player, group) = any(x -> x.score > player.score, group) ? rand() < 0.1 : rand(Bool)
overtwentystop(player, group) = player.ante > 20
over20unlesslosingstop(player, group) = player.ante > 20 && all(x -> x.score < 80, group)

const strategies = ("random choice to stop" => randomchoicetostop, "variable rand to stop" => variablerandtostop,
                  "roll to 20" => overtwentystop, "roll to 20 then if not losing stop" => over20unlesslosingstop)
const players = [Player(0, 0, 0, 0, s) for s in strategies]
const dice = collect(1:6)

function turn(player, verbose=false)
    playernum = findfirst(p -> p == player, players)
    scorewin() = for p in players if p == player p.wins += 1 else p.losses += 1 end; p.score = 0 end
    player.ante = 0
    while (r = rand(dice)) != 1
        player.ante += r
        verbose && println("Player $playernum rolls a $r.")
        if player.score + player.ante >= 100
            scorewin()
            verbose && println("Player $playernum wins.\n")
            return false
        elseif player.strategy[2](player, players)
            player.score += player.ante
            verbose && println("Player $playernum holds and has a new score of $(player.score).")
            return true
        end
    end
    verbose && println("Player $playernum rolls a 1, so turn is over.")
    true
end

function rungames(N)
    for i in 1:N
        verbose = (i == 3) ? true : false  # do verbose if it's game number 3
        curplayer = rand(collect(1:length(players)))
        while turn(players[curplayer], verbose)
            curplayer = curplayer >= length(players) ? 1 : curplayer + 1
        end
    end
    results = sort([(p.wins/(p.wins + p.losses), p.strategy[1]) for p in players], rev=true)
    println("             Strategy                % of wins (N = $N)")
    println("------------------------------------------------------------")
    for pair in results
        println(lpad(pair[2], 34), lpad(round(pair[1] * 100, digits=1), 18))
    end
end

rungames(1000000)

```
```txt

Player 3 rolls a 5.
Player 3 rolls a 3.
Player 3 rolls a 1, so turn is over.
Player 4 rolls a 6.
Player 4 rolls a 4.
Player 4 rolls a 3.
Player 4 rolls a 3.
Player 4 rolls a 4.
Player 4 rolls a 4.
Player 4 holds and has a new score of 24.
Player 1 rolls a 6.
Player 1 rolls a 3.
Player 1 holds and has a new score of 9.
Player 2 rolls a 4.
Player 2 rolls a 5.
Player 2 holds and has a new score of 9.
Player 3 rolls a 5.
Player 3 rolls a 5.
Player 3 rolls a 6.
Player 3 rolls a 2.
Player 3 rolls a 3.
Player 3 holds and has a new score of 21.
Player 4 rolls a 6.
Player 4 rolls a 3.
Player 4 rolls a 2.
Player 4 rolls a 5.
Player 4 rolls a 3.
Player 4 rolls a 2.
Player 4 holds and has a new score of 45.
Player 1 rolls a 5.
Player 1 holds and has a new score of 14.
Player 2 rolls a 1, so turn is over.
Player 3 rolls a 2.
Player 3 rolls a 3.
Player 3 rolls a 6.
Player 3 rolls a 5.
Player 3 rolls a 4.
Player 3 rolls a 1, so turn is over.
Player 4 rolls a 6.
Player 4 rolls a 6.
Player 4 rolls a 6.
Player 4 rolls a 4.
Player 4 holds and has a new score of 67.
Player 1 rolls a 1, so turn is over.
Player 2 rolls a 6.
Player 2 rolls a 4.
Player 2 rolls a 1, so turn is over.
Player 3 rolls a 3.
Player 3 rolls a 3.
Player 3 rolls a 3.
Player 3 rolls a 5.
Player 3 rolls a 3.
Player 3 rolls a 6.
Player 3 holds and has a new score of 44.
Player 4 rolls a 5.
Player 4 rolls a 1, so turn is over.
Player 1 rolls a 4.
Player 1 rolls a 5.
Player 1 holds and has a new score of 23.
Player 2 rolls a 6.
Player 2 rolls a 5.
Player 2 rolls a 6.
Player 2 rolls a 4.
Player 2 holds and has a new score of 30.
Player 3 rolls a 4.
Player 3 rolls a 4.
Player 3 rolls a 6.
Player 3 rolls a 6.
Player 3 rolls a 4.
Player 3 holds and has a new score of 68.
Player 4 rolls a 3.
Player 4 rolls a 2.
Player 4 rolls a 2.
Player 4 rolls a 6.
Player 4 rolls a 1, so turn is over.
Player 1 rolls a 5.
Player 1 holds and has a new score of 28.
Player 2 rolls a 2.
Player 2 rolls a 6.
Player 2 rolls a 1, so turn is over.
Player 3 rolls a 1, so turn is over.
Player 4 rolls a 6.
Player 4 rolls a 1, so turn is over.
Player 1 rolls a 2.
Player 1 rolls a 2.
Player 1 holds and has a new score of 32.
Player 2 rolls a 4.
Player 2 rolls a 5.
Player 2 rolls a 6.
Player 2 rolls a 5.
Player 2 rolls a 5.
Player 2 rolls a 2.
Player 2 rolls a 5.
Player 2 rolls a 6.
Player 2 rolls a 6.
Player 2 rolls a 1, so turn is over.
Player 3 rolls a 4.
Player 3 rolls a 1, so turn is over.
Player 4 rolls a 3.
Player 4 rolls a 5.
Player 4 rolls a 6.
Player 4 rolls a 4.
Player 4 rolls a 5.
Player 4 holds and has a new score of 90.
Player 1 rolls a 4.
Player 1 holds and has a new score of 36.
Player 2 rolls a 6.
Player 2 rolls a 1, so turn is over.
Player 3 rolls a 4.
Player 3 rolls a 2.
Player 3 rolls a 3.
Player 3 rolls a 1, so turn is over.
Player 4 rolls a 1, so turn is over.
Player 1 rolls a 6.
Player 1 rolls a 5.
Player 1 holds and has a new score of 47.
Player 2 rolls a 1, so turn is over.
Player 3 rolls a 2.
Player 3 rolls a 1, so turn is over.
Player 4 rolls a 5.
Player 4 rolls a 5.
Player 4 wins.

             Strategy                % of wins (N = 1000000)
------------------------------------------------------------
roll to 20 then if not losing stop              45.8
                        roll to 20              36.9
             variable rand to stop              15.8
             random choice to stop               1.5

```



## M2000 Interpreter

Strategy like ADA, look if get 8 or more points and then look if other sum is lower from 100 from a min difference, and if it is true the choose Hold, else continue until win or dice=1.

This is version 2.

We play the same strategy pair two times, second in reverse
Even for 20+20 games strategy 12, 20 wins 8, 10


```M2000 Interpreter

Module GamePig (games, strategy1, strategy2) {
      Print "Game of Pig"
      win1=0
      win2=0
      While games {
            games--
            dice=-1
            res$=""
            Player1points=0
            Player1sum=0
            Player2points=0
            Player2sum=0
            HaveWin=False
            score()
            \\ for simulation
            simulate$=String$("R", 500)
            Keyboard simulate$
            \\ end simulation
            while res$<>"Q" {
                  Print "Player 1 turn"
                  PlayerTurn(&Player1points, &player1sum, player2sum, ! strategy1)
                  if res$="Q" then exit
                  Player1sum+=Player1points
                  Score()
                  Print "Player 2 turn"
                  PlayerTurn(&Player2points,&player2sum, player1sum, ! strategy2)
                  if res$="Q" then exit
                  Player2sum+=Player2points
                  Score()
            }
            If HaveWin then {
                  Score()
                  If Player1Sum>Player2sum then {
                        Print "Player 1 Win"
                        win1++
                  } Else Print "Player 2 Win" : win2++
            }
      }
      \\ use stack as FIFO
      If win1>win2 Then {
            Data "Player 1 Win", win1,win2, array(strategy1,0), array(strategy1,1)
      } Else {
            Data "Player 2 Win",  win2,win1, array(strategy2,0), array(strategy2,1)
      }
      
      Sub Rolling()
            dice=random(1,6)
            Print "dice=";dice
      End Sub
      Sub PlayOrQuit()
            Print "R - Roling Q-Quit"
            Repeat {
                  res$=Ucase$(Key$)
            } Until Instr("RQ", res$)>0
      End Sub
      Sub PlayAgain()
            Print "R - Roling H - Hold Q-Quit"
            Repeat {
                  res$=Ucase$(Key$)
            } Until Instr("RHQ", res$)>0
      End Sub
      Sub PlayerTurn(&playerpoints, &sum, othersum, Max_Points, Min_difference)
            PlayOrQuit()
            If res$="Q" then Exit Sub
            playerpoints=0
            Rolling()
            While dice<>1 and res$="R" {
                  playerpoints+=dice
                  if dice>1 and playerpoints+sum>100 then {
                        sum+=playerpoints
                        HaveWin=True
                        res$="Q"
                  } Else {
                        if playerpoints>=Max_Points Then If 100-othersum>Min_difference Then res$="H" : exit
                        PlayAgain()
                        if res$="R" then Rolling()
                  }
            }
            if dice=1 then playerpoints=0
      End Sub
      Sub Score()
            Print "Player1 points="; Player1sum
            Print "Player2 points="; Player2sum
      End Sub     
}
Flush
GamePig 20, (8,10), (12, 20)
GamePig 20, (12, 20), (8,10)
Print "Results"
While not Empty {
      Read WhoWin$, winA,winb, Max_Points, Min_difference
      Print WhoWin$, winA;">";winb, Max_Points, Min_difference
}



```




## Perl

```perl
my $GOAL = 100;

package Player;

sub new {
    my ($class,$strategy) = @_;
    my $self = {
        score    => 0,
        rolls    => 0,
        ante     => 0,
        strategy => $strategy || sub { 0 } # as fallback, always roll again
    };
    return bless($self, $class);
}

sub turn {
    my ($P) = @_;
    $P->{rolls} = 0;
    $P->{ante}  = 0;
    my $done    = 0;
    do {
        my $v = 1 + int rand 6;
        $P->{rolls}++;
        if ($v == 1) {
            $P->{ante} = 0;
            $done = 1;
        } else {
            $P->{ante} += $v;
        }
        $done = 1 if $P->{score} + $P->{ante} >= $GOAL or $P->{strategy}();
    } until $done;
    $P->{score} += $P->{ante};
}

package Main;

# default, go-for-broke, always roll again
$players[0] = Player->new;

# try to roll 5 times but no more per turn
$players[1] = Player->new( sub { $players[1]->{rolls} >= 5 } );

# try to accumulate at least 20 points per turn
@players[2] = Player->new( sub { $players[2]->{ante} > 20 } );

# random but 90% chance of rolling again
$players[3] = Player->new( sub { rand() < 0.1 } );

# random but more conservative as approaches goal
$players[4] = Player->new( sub { rand() < ( $GOAL - $players[4]->{score} ) * .6 / $GOAL } );

for (1 .. shift || 100) {
    my $player = -1;
    do {
        $player++;
        @players[$player % @players]->turn;
    } until $players[$player % @players]->{score} >= $GOAL;

    $wins[$player % @players]++;

    printf "%5d", $players[$_]->{score} for 0..$#players; print "\n";
    $players[$_]->{score} = 0 for 0..$#players; # reset scores for next game
}

print ' ----' x @players, "\n";
printf "%5d", $_ for @wins; print "\n";
```

Distribution of wins after 10000 games:
```txt
929 3518 3462 1715  376
```



## Perl 6


This implements a pig player class where you can customize the strategy it uses. Pass a strategy code reference in that will evaluate to a Boolean value. The player will roll the die then decide whether to roll again or lock in its winnings based on its strategy. It will continue to roll until it gets a 1 (bust) or the strategy code reference evaluates to True (finished turn).

Set up as many players as you want, then run it. It will play 100 games (by default) then report the score for each game then the win totals for each player. If you want to play a different number of games, pass the number at the command line as a parameter.

Here we have 5 players:
  player 0 uses the default strategy, always roll if it can. 
  player 1 will roll up to 5 times then lock in whatever it earned.
  player 2 will try to get at least 20 points per turn.
  player 3 randomly picks whether to roll again or not biased so that there is a 90% chance that it will.
  player 4 randomly chooses to roll again but gets more consrvative as its score get closer to the goal.


```perl6
my $games = @*ARGS ?? (shift @*ARGS) !! 100;

constant DIE = 1 .. 6;
constant GOAL = 100;

class player {
    has $.score    is rw = 0;
    has $.ante     is rw;
    has $.rolls    is rw;
    has &.strategy is rw = sub { False }; # default, always roll again

    method turn {
        my $done_turn = False;
        $.rolls = 0;
        $.ante  = 0;
        repeat {
            given DIE.roll {
                $.rolls++;
                when 1 {
                    $.ante = 0;
                    $done_turn = True;
                }
                when 2..* {
                    $.ante += $_;
                }
            }
            $done_turn = True if $.score + $.ante >= GOAL or (&.strategy)();
        } until $done_turn;
        $.score += $.ante;
    }
}

my @players;

# default, go-for-broke, always roll again
@players[0] = player.new;

# try to roll 5 times but no more per turn
@players[1] = player.new( strategy => sub { @players[1].rolls >= 5 } );

# try to accumulate at least 20 points per turn
@players[2] = player.new( strategy => sub { @players[2].ante > 20 } );

# random but 90% chance of rolling again
@players[3] = player.new( strategy => sub { 1.rand < .1 } );

# random but more conservative as approaches goal
@players[4] = player.new( strategy => sub { 1.rand < ( GOAL - @players[4].score ) * .6 / GOAL } );

my @wins = 0 xx @players;

for ^ $games {
    my $player = -1;
    repeat {
        $player++;
        @players[$player % @players].turn;
    } until @players[$player % @players].score >= GOAL;

    @wins[$player % @players]++;

    say join "\t", @players>>.score;
    @players[$_].score = 0 for ^@players; # reset scores for next game
}

say "\nSCORES: for $games games";
say join "\t", @wins;
```


'''Sample output for 10000 games'''

```txt

0	103	46	5	40
0	100	69	0	48
0	105	22	7	44
0	75	69	19	102
105	21	23	5	17
0	101	85	12	29
0	70	66	103	23
0	0	104	69	20
0	100	44	0	20
0	102	63	75	30
0	56	101	12	40
0	103	71	2	38
0	103	91	21	32
0	18	102	47	28
...
...
...
104	0	69	14	47
0	68	101	13	22
0	99	89	102	31

SCORES: for 10000 games
947	3534	3396	1714	409


```



## Phix


```Phix
constant maxScore = 100

function one_game(sequence strategies)
integer numPlayers = length(strategies)
sequence scores = repeat(0,numPlayers)
integer points = 0, -- points accumulated in current turn, 0=swap turn
        rolls  = 0, -- number of rolls
        player = rand(numPlayers)   -- start with a random player
    while true do
        integer roll = rand(6)
        if roll=1 then
            points = 0 -- swap turn
        else            
            points += roll
            if scores[player]+points>=maxScore then exit end if
            rolls += 1
            if not call_func(strategies[player],{scores,player,points,rolls}) then
                scores[player] += points
                points = 0 -- swap turn
            end if
        end if
        if points=0 then
            player = mod(player,numPlayers) + 1
            rolls = 0
        end if
    end while
    return player
end function

-- each strategy returns true to roll, false to hold.

function strategy1(sequence /*scores*/, integer /*player*/, points, /*rolls*/)
    return points<20    -- roll until 20 or more
end function
constant r_s1 = routine_id("strategy1")

function strategy2(sequence /*scores*/, integer /*player*/, /*points*/, rolls)
    return rolls<4      -- roll 4 times
end function
constant r_s2 = routine_id("strategy2")

function strategy3(sequence scores, integer player, points, /*rolls*/)
    -- roll until 20 or score>80
    return points<20 or scores[player]+points>80
end function
constant r_s3 = routine_id("strategy3")

function strategy4(sequence scores, integer player, points, /*rolls*/)
    -- roll until 20 or any player has >71
    for i=1 to length(scores) do
        if scores[i]>71 then return true end if
    end for
    return points<20
end function
constant r_s4 = routine_id("strategy4")

constant strategies = {r_s1,r_s2,r_s3,r_s4}

-- play each strategy 1000 times against all combinations of other strategies
for s=1 to length(strategies) do
    sequence opponents = strategies
    opponents[s..s] = {}
    integer mask = power(2,length(opponents))-1
    integer wins = 0
    for m=1 to mask do -- (all possible bit settings, bar 0, eg/ie 1..7)
        sequence game = {strategies[s]}
        for g=1 to length(opponents) do
            if and_bits(m,power(2,g-1)) then
                game &= opponents[g]
            end if
        end for
        for n=1 to 1000 do
            wins += one_game(game)=1
        end for
    end for
    printf(1,"strategy %d: %d wins\n",{s,wins}) 
end for
```

```txt

strategy 1: 2784 wins
strategy 2: 2065 wins
strategy 3: 2885 wins
strategy 4: 3135 wins

```

Setting player to 1 at the start of one_game shows the advantage of going first,
which (not surprisingly) appears to apply fairly evenly to all strategies.

```txt

strategy 1: 3053 wins
strategy 2: 2293 wins
strategy 3: 3170 wins
strategy 4: 3457 wins

```



## Python

There are now three player strategies: 
# A random player RandPlay that rolls randomly.
# The RollTo20 player that rolls if that rounds score is less than 20.
# The Desparat player that plays like RollTo20 until any player gets within 20 of winning whereupon it desperately keeps rolling.
Details of the RollTo20 and Desparat strategy came from a paper referenced from [http://boardgames.about.com/gi/o.htm?zi=1/XJ&zTi=1&sdn=boardgames&cdn=hobbies&tm=55&f=00&su=p284.13.342.ip_p504.6.342.ip_&tt=2&bt=1&bts=1&zu=http%3A//cs.gettysburg.edu/projects/pig/piggame.html here].

Player instances are passed full (single) game statistics and so can be more complex in their behaviour.

Notice how Pythons Counter class from the standard library is used to collate the winning statistics near the end of the program without much additional code.


```python
#!/usr/bin/python3

'''
See: http://en.wikipedia.org/wiki/Pig_(dice)

This program scores, throws the dice, and plays for an N player game of Pig.

'''

from random import randint
from collections import namedtuple
import random
from pprint import pprint as pp
from collections import Counter


playercount = 2
maxscore = 100
maxgames = 100000


Game = namedtuple('Game', 'players, maxscore, rounds')
Round = namedtuple('Round', 'who, start, scores, safe')


class Player():
    def __init__(self, player_index):
        self.player_index = player_index

    def __repr__(self):
        return '%s(%i)' % (self.__class__.__name__, self.player_index)

    def __call__(self, safescore, scores, game):
        'Returns boolean True to roll again'
        pass

class RandPlay(Player):
    def __call__(self, safe, scores, game):
        'Returns random boolean choice of whether to roll again'
        return bool(random.randint(0, 1))

class RollTo20(Player):
    def __call__(self, safe, scores, game):
        'Roll again if this rounds score < 20'
        return (((sum(scores) + safe[self.player_index]) < maxscore)    # Haven't won yet
                and(sum(scores) < 20))                                  # Not at 20 this round

class Desparat(Player):
    def __call__(self, safe, scores, game):
        'Roll again if this rounds score < 20 or someone is within 20 of winning'
        return (((sum(scores) + safe[self.player_index]) < maxscore)    # Haven't won yet
                and( (sum(scores) < 20)                                 # Not at 20 this round
                     or max(safe) >= (maxscore - 20)))                  # Someone's close


def game__str__(self):
    'Pretty printer for Game class'
    return ("Game(players=%r, maxscore=%i,\n  rounds=[\n    %s\n  ])"
            % (self.players, self.maxscore,
               ',\n    '.join(repr(round) for round in self.rounds)))
Game.__str__ = game__str__


def winningorder(players, safescores):
    'Return (players in winning order, their scores)'
    return tuple(zip(*sorted(zip(players, safescores),
                            key=lambda x: x[1], reverse=True)))

def playpig(game):
    '''
    Plays the game of pig returning the players in winning order
    and their scores whilst updating argument game with the details of play.
    '''
    players, maxscore, rounds = game
    playercount = len(players)
    safescore = [0] * playercount   # Safe scores for each player
    player = 0                      # Who plays this round
    scores=[]                       # Individual scores this round

    while max(safescore) < maxscore:
        startscore = safescore[player]
        rolling = players[player](safescore, scores, game)
        if rolling:
            rolled = randint(1, 6)
            scores.append(rolled)
            if rolled == 1:
                # Bust! 
                round = Round(who=players[player],
                              start=startscore,
                              scores=scores,
                              safe=safescore[player])
                rounds.append(round)
                scores, player = [], (player + 1) % playercount
        else:
            # Stick
            safescore[player] += sum(scores)
            round = Round(who=players[player],
                          start=startscore,
                          scores=scores,
                          safe=safescore[player])
            rounds.append(round)
            if safescore[player] >= maxscore:
                break
            scores, player = [], (player + 1) % playercount

    # return players in winning order and all scores
    return winningorder(players, safescore)

if __name__ == '__main__':
    game = Game(players=tuple(RandPlay(i) for i in range(playercount)),
                maxscore=20,
                rounds=[])
    print('ONE GAME')
    print('Winning order: %r; Respective scores: %r\n' % playpig(game))
    print(game)
    game = Game(players=tuple(RandPlay(i) for i in range(playercount)),
                maxscore=maxscore,
                rounds=[])
    algos = (RollTo20, RandPlay, Desparat)
    print('\n\nMULTIPLE STATISTICS using %r\n  for %i GAMES'
          % (', '.join(p.__name__ for p in algos), maxgames,))
    winners = Counter(repr(playpig(game._replace(players=tuple(random.choice(algos)(i)
                                                               for i in range(playercount)),
                                                 rounds=[]))[0])
                      for i in range(maxgames))
    print('  Players(position) winning on left; occurrences on right:\n    %s'
          % ',\n    '.join(str(w) for w in winners.most_common()))
```

First is shown the game data for a single game with reduced maxscore then statistics on multiple games.

Desparat beats RollTo20 beats RandPlay on average. It doesn't matter if they play first or not when playing against another strategies. When both players use the same strategies there may be an advantage in going first.

```txt
ONE GAME
Winner: RandPlay(0); Scores: [24, 12]

Game(players=(RandPlay(0), RandPlay(1)), maxscore=20,
  rounds=[
    Round(who=RandPlay(0), start=0, scores=[], safe=0),
    Round(who=RandPlay(1), start=0, scores=[6, 2], safe=8),
    Round(who=RandPlay(0), start=0, scores=[], safe=0),
    Round(who=RandPlay(1), start=8, scores=[], safe=8),
    Round(who=RandPlay(0), start=0, scores=[], safe=0),
    Round(who=RandPlay(1), start=8, scores=[4], safe=12),
    Round(who=RandPlay(0), start=0, scores=[4, 5, 6, 4, 5], safe=24)
  ])


MULTIPLE STATISTICS using 'RollTo20, RandPlay, Desparat'
  for 100000 GAMES
  Players(position) winning on left; occurrences on right:
    ('(Desparat(1), RandPlay(0))', 11152),
    ('(RollTo20(1), RandPlay(0))', 11114),
    ('(Desparat(0), RandPlay(1))', 11072),
    ('(RollTo20(0), RandPlay(1))', 11007),
    ('(Desparat(0), RollTo20(1))', 6405),
    ('(RollTo20(0), RollTo20(1))', 6013),
    ('(Desparat(0), Desparat(1))', 5820),
    ('(Desparat(1), RollTo20(0))', 5772),
    ('(RandPlay(0), RandPlay(1))', 5667),
    ('(RandPlay(1), RandPlay(0))', 5481),
    ('(RollTo20(0), Desparat(1))', 5385),
    ('(Desparat(1), Desparat(0))', 5235),
    ('(RollTo20(1), RollTo20(0))', 5090),
    ('(RollTo20(1), Desparat(0))', 4625),
    ('(RandPlay(0), Desparat(1))', 59),
    ('(RandPlay(1), RollTo20(0))', 37),
    ('(RandPlay(1), Desparat(0))', 35),
    ('(RandPlay(0), RollTo20(1))', 31)
```


Note: ''('(RollTo20(1), RandPlay(0))', 25063)'' means that the algorithm RollTo20 playing as the second player, (1) wins against algorithm RandPlay of the first player, (0) and wins 25063 times. (Zero based indexing so the first player is player(0)).


## Racket


Same as [[Pig_the_dice_game#Racket]], with three strategy makers, and
simulation code for trying out strategies.

```racket
#lang racket

(define (pig-the-dice #:print? [print? #t] . players)
  (define prn (if print? (λ xs (apply printf xs) (flush-output)) void))
  (define names (for/list ([p players] [n (in-naturals 1)]) n))
  (define points (for/list ([p players]) (box 0)))
  (with-handlers ([(negate exn?) identity])
    (for ([nm (in-cycle names)] [tp (in-cycle points)] [pl (in-cycle players)])
      (prn (string-join (for/list ([n names] [p points])
                          (format "Player ~a, ~a points" n (unbox p)))
                        "; " #:before-first "Status: " #:after-last ".\n"))
      (let turn ([p 0] [n 0])
        (prn "Player ~a, round #~a, [R]oll or [P]ass? " nm (+ 1 n))
        (define roll? (pl (unbox tp) p n))
        (unless (eq? pl human) (prn "~a\n" (if roll? 'R 'P)))
        (if (not roll?) (set-box! tp (+ (unbox tp) p))
            (let ([r (+ 1 (random 6))])
              (prn "  Dice roll: ~s => " r)
              (if (= r 1) (prn "turn lost\n")
                  (let ([p (+ p r)]) (prn "~a points\n" p) (turn p (+ 1 n)))))))
      (prn "--------------------\n")
      (when (<= 100 (unbox tp)) (prn "Player ~a wins!\n" nm) (raise nm)))))

(define (human total-points turn-points round#)
  (case (string->symbol (car (regexp-match #px"[A-Za-z]?" (read-line))))
    [(R r) #t] [(P p) #f] [else (human total-points turn-points round#)]))

;; Always do N rolls
(define ((n-rounds n) total-points turn-points round#) (< round# n))
;; Roll until a given number of points
(define ((n-points n) total-points turn-points round#) (< turn-points n))
;; Random decision
(define ((n-random n) total-points turn-points round#) (zero? (random n)))

(define (n-runs n . players)
  (define v (make-vector (length players) 0))
  (for ([i n])
    (define p (sub1 (apply pig-the-dice #:print? #f players)))
    (vector-set! v p (add1 (vector-ref v p))))
  (for ([wins v] [i (in-naturals 1)])
    (printf "Player ~a: ~a%\n" i (round (/ wins n 1/100)))))

;; Things to try
;; (n-runs 1000 (n-random 2) (n-random 3) (n-random 4))
;; (n-runs 1000 (n-rounds 5) (n-points 24))
;; (n-runs 1000 (n-rounds 5) (n-random 2))
```

The following example run demonstrates the output from

```Racket
(pig-the-dice #:print? #t (n-points 12) (n-rounds 4))
```

Where <code>n-points</code> is a strategy where the user continues to roll until the specified number of points is gained and <code>n-rounds</code> is a strategy where the users rolls the specified number of times each round.

```txt

Status: Player 1, 0 points; Player 2, 0 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 4 => 4 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 5 => 9 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 2 => 11 points
Player 1, round #4, [R]oll or [P]ass? R
  Dice roll: 5 => 16 points
Player 1, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 16 points; Player 2, 0 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 6 => 6 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 6 => 12 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 3 => 15 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 5 => 20 points
Player 2, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 16 points; Player 2, 20 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 2 => 2 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 2 => 4 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 6 => 10 points
Player 1, round #4, [R]oll or [P]ass? R
  Dice roll: 6 => 16 points
Player 1, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 32 points; Player 2, 20 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 5 => 5 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 2 => 7 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 2 => 9 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 3 => 12 points
Player 2, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 32 points; Player 2, 32 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 6 => 6 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 5 => 11 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 2 => 13 points
Player 1, round #4, [R]oll or [P]ass? P
--------------------
Status: Player 1, 45 points; Player 2, 32 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 3 => 3 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 32 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 2 => 2 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 4 => 6 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 32 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 5 => 5 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 2 => 7 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 6 => 13 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 2 => 15 points
Player 2, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 45 points; Player 2, 47 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 47 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 47 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 2 => 2 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 5 => 7 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 47 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 5 => 5 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 6 => 11 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 6 => 17 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 3 => 20 points
Player 2, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 45 points; Player 2, 67 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 4 => 4 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 67 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 3 => 3 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 3 => 6 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 2 => 8 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 3 => 11 points
Player 2, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 45 points; Player 2, 78 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 3 => 3 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 4 => 7 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 4 => 11 points
Player 1, round #4, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 78 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 2 => 2 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 5 => 7 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 78 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 3 => 3 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 78 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 45 points; Player 2, 78 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 5 => 5 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 4 => 9 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 2 => 11 points
Player 1, round #4, [R]oll or [P]ass? R
  Dice roll: 5 => 16 points
Player 1, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 61 points; Player 2, 78 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 4 => 4 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 2 => 6 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 3 => 9 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 6 => 15 points
Player 2, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 61 points; Player 2, 93 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 5 => 5 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 3 => 8 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 2 => 10 points
Player 1, round #4, [R]oll or [P]ass? R
  Dice roll: 2 => 12 points
Player 1, round #5, [R]oll or [P]ass? P
--------------------
Status: Player 1, 73 points; Player 2, 93 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 6 => 6 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 4 => 10 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 4 => 14 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 1 => turn lost
--------------------
Status: Player 1, 73 points; Player 2, 93 points.
Player 1, round #1, [R]oll or [P]ass? R
  Dice roll: 3 => 3 points
Player 1, round #2, [R]oll or [P]ass? R
  Dice roll: 4 => 7 points
Player 1, round #3, [R]oll or [P]ass? R
  Dice roll: 6 => 13 points
Player 1, round #4, [R]oll or [P]ass? P
--------------------
Status: Player 1, 86 points; Player 2, 93 points.
Player 2, round #1, [R]oll or [P]ass? R
  Dice roll: 2 => 2 points
Player 2, round #2, [R]oll or [P]ass? R
  Dice roll: 6 => 8 points
Player 2, round #3, [R]oll or [P]ass? R
  Dice roll: 4 => 12 points
Player 2, round #4, [R]oll or [P]ass? R
  Dice roll: 5 => 17 points
Player 2, round #5, [R]oll or [P]ass? P
--------------------
Player 2 wins!
2

```



## REXX

The strategy for a computer player is to roll again if the total score (including the current inning) has not won, and to roll again if the inning score is less than a quarter of the score needed to win.

The (somewhat aggressive) "quarter" strategy was chosen to give the advantage to a human (it was presumed that this dice game would be played with a CBLF).

```rexx
/*REXX program plays "pig the dice game"  (any number of CBLFs and/or silicons or HALs).*/
sw= linesize() - 1                               /*get the width of the terminal screen,*/
parse arg  hp  cp  win  die  _  .  '(' names ")" /*obtain optional arguments from the CL*/
                                                 /*names with blanks should use an  _   */
if _\==''  then  call  err  'too many arguments were specified: ' _
@nhp  = 'number of human players'   ;         hp = scrutinize( hp, @nhp , 0,  0,   0)
@ncp  = 'number of computer players';         cp = scrutinize( cp, @ncp , 0,  0,   2)
@sn2w = 'score needed to win'       ;         win= scrutinize(win, @sn2w, 1, 1e6, 60)
@nsid = 'number of sides in die'    ;         die= scrutinize(die, @nsid, 2, 999,  6)
if hp==0  &  cp==0   then cp= 2                  /*if both counts are zero, two HALs.   */
if hp==1  &  cp==0   then cp= 1                  /*if one human, then use   one HAL.    */
name.=                                           /*nullify all names  (to a blank).     */
L= 0                                             /*maximum length of a player name.     */
       do i=1  for hp+cp                         /*get the player's names,  ...  maybe. */
       if i>hp  then @= 'HAL_'i"_the_computer"   /*use this for default name.           */
                else @= 'player_'i               /* "    "   "     "      "             */
       name.i = translate( word( strip( word( names, i) ) @, 1), , '_')
       L= max(L, length( name.i) )               /*use   L   for nice name formatting.  */
       end   /*i*/                               /*underscores are changed ──► blanks.  */

hpn=hp;  if hpn==0   then hpn= 'no'              /*use normal English for the display.  */
cpn=cp;  if cpn==0   then cpn= 'no'              /* "     "      "     "   "     "      */

say 'Pig (the dice game) is being played with:'  /*the introduction to pig-the-dice-game*/

         if cpn\==0  then  say  right(cpn, 9)     'computer player's(cp)
         if hpn\==0  then  say  right(hpn, 9)     'human player's(hp)
!.=
say 'and the'         @sn2w         "is: "         win         '   (or greater).'
dieNames= 'ace deuce trey square nickle boxcar'  /*some slangy vernacular die─face names*/
!w=0                                             /*note:  snake eyes is for two aces.   */
               do i=1  for die                   /*assign the vernacular die─face names.*/
               !.i= ' ['word(dieNames,i)"]"      /*pick a word from die─face name lists.*/
               !w= max(!w, length(!.i) )         /*!w ──► maximum length die─face name. */
               end   /*i*/
s.= 0                                            /*set all player's scores to zero.     */
!w= !w + length(die) + 3                         /*pad the die number and die names.    */
@= copies('─', 9)                                /*eyecatcher (for the prompting text). */
@jra= 'just rolled a '                           /*a nice literal to have laying 'round.*/
@ati= 'and the inning'                           /*"   "     "     "   "     "      "   */
               /*═══════════════════════════════════════════════════let's play some pig.*/
   do game=1;     in.= 0;       call score       /*set each inning's score to 0; display*/

     do j=1  for hp+cp;         say              /*let each player roll their dice.     */
     say copies('─', sw)                         /*display a fence for da ole eyeballs. */
     it= name.j
     say it',  your total score (so far) in this pig game is: '        s.j"."

       do  until  stopped                        /*keep prompting/rolling 'til stopped. */
       r= random(1, die)                         /*get a random die face (number).      */
       != left(space(r !.r','),  !w)             /*for color, use a die─face name.      */
       in.j= in.j + r                            /*add die─face number to the inning.   */

       if r==1  then  do;  say it @jra ! || @ati "is a bust.";  leave;   end
                           say it @jra ! || @ati "total is: "    in.j

       stopped= what2do(j)                       /*determine or ask  to stop rolling.   */
       if j>hp & stopped  then say ' and'      name.j      "elected to stop rolling."
       end   /*until stopped*/

     if r\==1     then s.j= s.j + in.j           /*if not a bust, then add to the inning*/
     if s.j>=win  then leave game                /*we have a winner,  so the game ends. */
     end     /*j*/                               /*that's the end of the players.       */
   end       /*game*/

call score;    say;    say;    say;    say;          say center(''name.j "won! ", sw, '═')
               say;    say;            exit      /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s: if arg(1)==1  then return arg(3);           return word(arg(2) 's',1)   /*pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
score:  say;           say copies('█', sw)       /*display a fence for da ole eyeballs. */
          do k=1  for hp+cp                      /*display the scores  (as a recap).    */
          say 'The score for '    left(name.k, L)     " is "     right(s.k, length(win) ).
          end  /*k*/
        say copies('█', sw);           return    /*display a fence for da ole eyeballs. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
scrutinize: parse arg ?,what,min,max             /*?  is the number,  ... or maybe not. */
            if ?==''  |  ?==','   then return arg(5)
            if \datatype(?, 'N')  then call err what "isn't numeric: "    ?;         ?=?/1
            if \datatype(?, 'W')  then call err what "isn't an integer: " ?
            if ?==0  & min>0      then call err what "can't be zero."
            if ?<min              then call err what "can't be less than"     min': '  ?
            if ?==0  & max>0      then call err what "can't be zero."
            if ?>max & max\==0    then call err what "can't be greater than"  max': '  ?
            return ?
/*──────────────────────────────────────────────────────────────────────────────────────*/
what2do: parse arg who                                 /*"who" is a human or a computer.*/
         if j>hp & s.j+in.j>=win    then  return 1     /*an  easy  choice  for HAL.     */
         if j>hp &     in.j>=win%4  then  return 1     /*a simple strategy for HAL.     */
         if j>hp                    then  return 0     /*HAL says, keep truckin'!       */
         say @ name.who', what do you want to do?        (a QUIT will stop the game),'
         say @ 'press  ENTER  to roll again,  or anything else to STOP rolling.'
         pull action;      action=space(action)        /*remove any superfluous blanks. */
         if \abbrev('QUIT', action, 1)  then return action\==''
         say;    say;    say center(' quitting. ', sw, '─');    say;     say;      exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:     say;    say;    say center(' error! ', max(40, linesize() % 2), "*");     say
                      do j=1  for arg();    say arg(j);    say;    end;    say;    exit 13
```

This REXX program makes use of   '''LINESIZE'''   BIF   which returns the terminals width (linesize). 

Some REXXes don't have a   '''LINESIZE'''   BIF, so one is included here   ──►   [[LINESIZE.REX]]. 




To play this game with two computer players (simulate), use the following arguments:

```txt

  0  2

```

Optionally, you may use (for instance):

```txt
 
  0  2  (  HAL  R2D2

```

to specify names for the (two) computer players.




'''output''':

```txt

Pig (the dice game) is being played with:
        2 computer players
       no human players
and the score needed to win is:  60    (or greater).

███████████████████████████████████████████████████████████████████████████████
The score for  HAL 1 the computer  is   0.
The score for  HAL 2 the computer  is   0.
███████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────
HAL 1 the computer,  your total score (so far) in this pig game is:  0.
HAL 1 the computer just rolled a  2 [deuce],   and the inning total is:  2
HAL 1 the computer just rolled a  5 [nickle],  and the inning total is:  7
HAL 1 the computer just rolled a  2 [deuce],   and the inning total is:  9
HAL 1 the computer just rolled a  1 [ace],     and the inning is a bust.

───────────────────────────────────────────────────────────────────────────────
HAL 2 the computer,  your total score (so far) in this pig game is:  0.
HAL 2 the computer just rolled a  6 [boxcar],  and the inning total is:  6
HAL 2 the computer just rolled a  4 [square],  and the inning total is:  10
HAL 2 the computer just rolled a  4 [square],  and the inning total is:  14
HAL 2 the computer just rolled a  5 [nickle],  and the inning total is:  19
 and HAL 2 the computer elected to stop rolling.

███████████████████████████████████████████████████████████████████████████████
The score for  HAL 1 the computer  is   0.
The score for  HAL 2 the computer  is  19.
███████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────
HAL 1 the computer,  your total score (so far) in this pig game is:  0.
HAL 1 the computer just rolled a  1 [ace],     and the inning is a bust.

───────────────────────────────────────────────────────────────────────────────
HAL 2 the computer,  your total score (so far) in this pig game is:  19.
HAL 2 the computer just rolled a  1 [ace],     and the inning is a bust.

███████████████████████████████████████████████████████████████████████████████
The score for  HAL 1 the computer  is   0.
The score for  HAL 2 the computer  is  19.
███████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────
HAL 1 the computer,  your total score (so far) in this pig game is:  0.
HAL 1 the computer just rolled a  6 [boxcar],  and the inning total is:  6
HAL 1 the computer just rolled a  6 [boxcar],  and the inning total is:  12
HAL 1 the computer just rolled a  6 [boxcar],  and the inning total is:  18
 and HAL 1 the computer elected to stop rolling.

───────────────────────────────────────────────────────────────────────────────
HAL 2 the computer,  your total score (so far) in this pig game is:  19.
HAL 2 the computer just rolled a  6 [boxcar],  and the inning total is:  6
HAL 2 the computer just rolled a  3 [trey],    and the inning total is:  9
HAL 2 the computer just rolled a  6 [boxcar],  and the inning total is:  15
 and HAL 2 the computer elected to stop rolling.

███████████████████████████████████████████████████████████████████████████████
The score for  HAL 1 the computer  is  18.
The score for  HAL 2 the computer  is  34.
███████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────
HAL 1 the computer,  your total score (so far) in this pig game is:  18.
HAL 1 the computer just rolled a  5 [nickle],  and the inning total is:  5
HAL 1 the computer just rolled a  1 [ace],     and the inning is a bust.

───────────────────────────────────────────────────────────────────────────────
HAL 2 the computer,  your total score (so far) in this pig game is:  34.
HAL 2 the computer just rolled a  4 [square],  and the inning total is:  4
HAL 2 the computer just rolled a  4 [square],  and the inning total is:  8
HAL 2 the computer just rolled a  3 [trey],    and the inning total is:  11
HAL 2 the computer just rolled a  5 [nickle],  and the inning total is:  16
 and HAL 2 the computer elected to stop rolling.

███████████████████████████████████████████████████████████████████████████████
The score for  HAL 1 the computer  is  18.
The score for  HAL 2 the computer  is  50.
███████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────
HAL 1 the computer,  your total score (so far) in this pig game is:  18.
HAL 1 the computer just rolled a  4 [square],  and the inning total is:  4
HAL 1 the computer just rolled a  1 [ace],     and the inning is a bust.

───────────────────────────────────────────────────────────────────────────────
HAL 2 the computer,  your total score (so far) in this pig game is:  50.
HAL 2 the computer just rolled a  1 [ace],     and the inning is a bust.

███████████████████████████████████████████████████████████████████████████████
The score for  HAL 1 the computer  is  18.
The score for  HAL 2 the computer  is  50.
███████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────
HAL 1 the computer,  your total score (so far) in this pig game is:  18.
HAL 1 the computer just rolled a  6 [boxcar],  and the inning total is:  6
HAL 1 the computer just rolled a  6 [boxcar],  and the inning total is:  12
HAL 1 the computer just rolled a  4 [square],  and the inning total is:  16
 and HAL 1 the computer elected to stop rolling.

───────────────────────────────────────────────────────────────────────────────
HAL 2 the computer,  your total score (so far) in this pig game is:  50.
HAL 2 the computer just rolled a  6 [boxcar],  and the inning total is:  6
HAL 2 the computer just rolled a  3 [trey],    and the inning total is:  9
HAL 2 the computer just rolled a  5 [nickle],  and the inning total is:  14
 and HAL 2 the computer elected to stop rolling.




═══════════════════════════HAL 2 the computer won! ════════════════════════════

```



## Ruby


```ruby

def player1(sum,sm)
for i in 1..100
puts "player1 rolled"
a=gets.chomp().to_i
if (a>1 && a<7)
	sum+=a
	if sum>=100
	puts "player1 wins"
	break
	end
else

goto player2(sum,sm)
end
i+=1
end
end

def player2(sum,sm)
for j in 1..100
puts "player2 rolled"
b=gets.chomp().to_i
if(b>1 && b<7)
 sm+=b
	if sm>=100
	 puts "player2 wins"
	break
	end
else

player1(sum,sm)
end
j+=1
end
end
i=0
j=0
sum=0
sm=0
player1(sum,sm)
return
```



## Sidef

```ruby
var (games=100) = ARGV.map{.to_i}...

define DIE = 1..6;
define GOAL = 100;

class Player(score=0, ante=0, rolls=0, strategy={false}) {
    method turn {
        rolls = 0;
        ante  = 0;
        loop {
            rolls++;
            given (var roll = DIE.rand) {
                when (1) {
                    ante = 0;
                    break;
                }
                case (roll > 1) {
                    ante += roll;
                }
            }
            ((score + ante >= GOAL) || strategy) && break;
        }
        score += ante;
    }
}

var players = [];

# default, go-for-broke, always roll again
players[0] = Player.new;

# try to roll 5 times but no more per turn
players[1] = Player.new( strategy: { players[1].rolls >= 5 } );

# try to accumulate at least 20 points per turn
players[2] = Player.new( strategy: { players[2].ante > 20 } );

# random but 90% chance of rolling again
players[3] = Player.new( strategy: { 1.rand < 0.1 } );

# random but more conservative as approaches goal
players[4] = Player.new( strategy: { 1.rand < ((GOAL - players[4].score) * 0.6 / GOAL) } );

var wins = [0]*players.len;

games.times {
    var player = -1;
    loop {
        player++;
        var p = players[player % players.len];
        p.turn;
        p.score >= GOAL && break;
    }
    wins[player % players.len]++;
    players.map{.score}.join("\t").say;
    players.each { |p| p.score = 0 };
}

say "\nSCORES: for #{games} games";
say wins.join("\t");
```

```txt

0	0	67	101	19
0	88	100	9	14
0	81	66	100	24
0	56	103	8	27
0	102	70	17	27
0	79	101	29	36
0	100	71	56	31
0	62	104	28	34
103	19	24	6	5
0	49	101	24	19
0	60	22	101	0
0	20	101	30	34
...
...
...
0	101	69	26	91
0	87	101	30	54
0	84	100	17	64
0	52	24	102	17

SCORES: for 100 games
6	28	40	22	4

```



## Tcl

<table><tr><td>{{works with|Tcl|8.6}}</td><td>or alternatively with Tcl 8.5 and</td><td>{{libheader|TclOO}}</td></tr></table><!-- dirty trick! -->
First the structure of the game (from [[Pig the dice game#Tcl|the parent page]]):

```tcl
package require TclOO

oo::class create Player {
    variable me
    constructor {name} {
	set me $name
    }
    method name {} {
	return $me
    }

    method wantToRoll {safeScore roundScore} {}

    method rolled {who what} {
	if {$who ne [self]} {
	    #puts "[$who name] rolled a $what"
	}
    }
    method turnend {who score} {
	if {$who ne [self]} {
	    #puts "End of turn for [$who name] on $score"
	}
    }
    method winner {who score} {
	if {$who ne [self]} {
	    #puts "[$who name] is a winner, on $score"
	}
    }
}

oo::class create HumanPlayer {
    variable me
    superclass Player
    method wantToRoll {safeScore roundScore} {
	while 1 {
	    puts -nonewline "$me (on $safeScore+$roundScore) do you want to roll? (Y/n)"
	    flush stdout
	    if {[gets stdin line] < 0} {
		# EOF detected
		puts ""
		exit
	    }
	    if {$line eq "" || $line eq "y" || $line eq "Y"} {
		return 1
	    }
	    if {$line eq "n" || $line eq "N"} {
		return 0
	    }
	}
    }
    method stuck {score} {
	puts "$me sticks with score $score"
    }
    method busted {score} {
	puts "Busted! ($me still on score $score)"
    }
    method won {score} {
	puts "$me has won! (Score: $score)"
    }
}

proc rollDie {} {
    expr {1+int(rand() * 6)}
}
proc rotateList {var} {
    upvar 1 $var l
    set l [list {*}[lrange $l 1 end] [lindex $l 0]]
}
proc broadcast {players message score} {
    set p0 [lindex $players 0]
    foreach p $players {
	$p $message $p0 $score
    }
}

proc pig {args} {
    set players $args
    set scores [lrepeat [llength $args] 0]
    while 1 {
	set player [lindex $players 0]
	set safe [lindex $scores 0]
	set s 0
	while 1 {
	    if {$safe + $s >= 100} {
		incr safe $s
		$player won $safe
		broadcast $players winner $safe
		return $player
	    }
	    if {![$player wantToRoll $safe $s]} {
		lset scores 0 [incr safe $s]
		$player stuck $safe
		break
	    }
	    set roll [rollDie]
	    broadcast $players rolled $roll
	    if {$roll == 1} {
		$player busted $safe
		break
	    }
	    incr s $roll
	}
	broadcast $players turnend $safe
	rotateList players
	rotateList scores
    }
}
```

Then the classes that create the various implemented strategies:

```tcl
oo::class create RoboPlayer {
    superclass Player
    variable me
    constructor {name} {
	# Add a symbol to the name to mark a robot...
	next "$name\u00ae"
    }
    method wantToRoll {safeScore roundScore} {
	puts -nonewline "$me has ($safeScore,$roundScore)... "
	set decision [my Decide $safeScore $roundScore]
	puts [lindex {stick roll} $decision]
	return $decision
    }
    method stuck {score} {
	puts "$me sticks with score $score"
    }
    method busted {score} {
	puts "Busted! ($me still on score $score)"
    }
    method won {score} {
	puts "$me has won! (Score: $score)"
    }
}

# Just takes a random decision as to what to play
oo::class create RandomPlayer {
    superclass RoboPlayer
    constructor {} {next "Random"}
    method Decide {a b} {expr {rand() < 0.5}}
}

# Rolls until it scores at least 20 from a round or goes bust
oo::class create To20Player {
    superclass RoboPlayer
    constructor {} {next "To20"}
    method Decide {safeScore roundScore} {expr {$roundScore < 20}}
}

# Like To20, but will roll desperately once another player reaches 80
oo::class create Desperate {
    superclass RoboPlayer
    variable me scores
    constructor {} {
	next "Desperate"
	set scores {}
    }

    method Decide {safeScore roundScore} {
	dict for {who val} $scores {
	    if {$who ne [self] && $val >= 80} {
		return 1
	    }
	}
	return [expr {$roundScore < 20}]
    }
    # Keep an eye on other players
    method turnend {who score} {
	next $who $score
	dict set scores $who $score
    }
}
```

Demonstration, pitting the three of them against each other:

```tcl
pig [RandomPlayer new] [To20Player new] [Desperate new]
```

```txt

Random® has (0,0)... roll
Busted! (Random® still on score 0)
To20® has (0,0)... roll
To20® has (0,4)... roll
Busted! (To20® still on score 0)
Desperate® has (0,0)... roll
Desperate® has (0,6)... roll
Desperate® has (0,10)... roll
Desperate® has (0,13)... roll
Desperate® has (0,17)... roll
Desperate® has (0,21)... stick
Desperate® sticks with score 21
Random® has (0,0)... roll
Busted! (Random® still on score 0)
To20® has (0,0)... roll
To20® has (0,2)... roll
To20® has (0,7)... roll
To20® has (0,11)... roll
To20® has (0,16)... roll
To20® has (0,19)... roll
To20® has (0,25)... stick
To20® sticks with score 25
Desperate® has (21,0)... roll
Desperate® has (21,6)... roll
Desperate® has (21,12)... roll
Busted! (Desperate® still on score 21)
Random® has (0,0)... stick
Random® sticks with score 0
To20® has (25,0)... roll
Busted! (To20® still on score 25)
Desperate® has (21,0)... roll
Desperate® has (21,4)... roll
Desperate® has (21,7)... roll
Desperate® has (21,9)... roll
Busted! (Desperate® still on score 21)
Random® has (0,0)... stick
Random® sticks with score 0
To20® has (25,0)... roll
To20® has (25,5)... roll
Busted! (To20® still on score 25)
Desperate® has (21,0)... roll
Desperate® has (21,2)... roll
Desperate® has (21,7)... roll
Desperate® has (21,11)... roll
Desperate® has (21,14)... roll
Desperate® has (21,19)... roll
Desperate® has (21,24)... stick
Desperate® sticks with score 45
Random® has (0,0)... stick
Random® sticks with score 0
To20® has (25,0)... roll
To20® has (25,5)... roll
To20® has (25,8)... roll
To20® has (25,14)... roll
To20® has (25,18)... roll
To20® has (25,20)... stick
To20® sticks with score 45
Desperate® has (45,0)... roll
Desperate® has (45,6)... roll
Desperate® has (45,11)... roll
Desperate® has (45,14)... roll
Desperate® has (45,18)... roll
Desperate® has (45,21)... stick
Desperate® sticks with score 66
Random® has (0,0)... roll
Random® has (0,2)... stick
Random® sticks with score 2
To20® has (45,0)... roll
To20® has (45,6)... roll
To20® has (45,12)... roll
To20® has (45,18)... roll
To20® has (45,24)... stick
To20® sticks with score 69
Desperate® has (66,0)... roll
Desperate® has (66,6)... roll
Busted! (Desperate® still on score 66)
Random® has (2,0)... roll
Busted! (Random® still on score 2)
To20® has (69,0)... roll
To20® has (69,4)... roll
To20® has (69,8)... roll
To20® has (69,14)... roll
To20® has (69,20)... stick
To20® sticks with score 89
Desperate® has (66,0)... roll
Desperate® has (66,6)... roll
Desperate® has (66,10)... roll
Desperate® has (66,12)... roll
Desperate® has (66,18)... roll
Desperate® has (66,23)... roll
Desperate® has (66,27)... roll
Desperate® has (66,29)... roll
Desperate® has (66,31)... roll
Desperate® has won! (Score: 101)

```


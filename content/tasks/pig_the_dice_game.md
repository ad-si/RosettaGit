+++
title = "Pig the dice game"
description = ""
date = 2019-09-05T10:22:07Z
aliases = []
[extra]
id = 12297
[taxonomies]
categories = ["task"]
tags = []
+++

The   [[wp:Pig (dice)|game of Pig]]   is a multiplayer game played with a single six-sided die.   The
object of the game is to reach   '''100'''   points or more.
Play is taken in turns.   On each person's turn that person has the option of either:

:# '''Rolling the dice''':   where a roll of two to six is added to their score for that turn and the player's turn continues as the player is given the same choice again;   or a roll of   '''1'''   loses the player's total points   ''for that turn''   and their turn finishes with play passing to the next player.
:# '''Holding''':   the player's score for that round is added to their total and becomes safe from the effects of throwing a   '''1'''   (one).   The player's turn finishes with play passing to the next player.


## Task

Create a program to score for, and simulate dice throws for, a two-person game.


## Related tasks

*   [[Pig the dice game/Player]]





## ActionScript


```ActionScript

package {

    import flash.display.Graphics;
    import flash.display.Shape;
    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;

    public class PigTheDiceGame extends Sprite {

        /**
         * The name of the first player.
         *
         * @private
         */
        private var _name1:String = "Player 1";

        /**
         * The name of the second player.
         *
         * @private
         */
        private var _name2:String = "Player 2";

        /**
         * True if the next turn is of the second player, false if it is of the first player.
         *
         * @private
         */
        private var _isPlayer2:Boolean = false;

        /**
         * The score of the first player.
         *
         * @private
         */
        private var __p1Score:uint;

        /**
         * The score of the second player.
         *
         * @private
         */
        private var __p2Score:uint;

        /**
         * The number of points in the current turn.
         *
         * @private
         */
        private var __turnPts:uint;

        /**
         * The text field displaying the score of the first player.
         *
         * @private
         */
        private var _p1ScoreText:TextField;

        /**
         * The text field displaying the score of the second player.
         *
         * @private
         */
        private var _p2ScoreText:TextField;

        /**
         * The button which must be clicked for a player to roll the dice.
         *
         * @private
         */
        private var _rollButton:Sprite;

        /**
         * The button which must be clicked for a player to hold.
         *
         * @private
         */
        private var _holdButton:Sprite;

        /**
         * The text field displaying the name of the current player.
         */
        private var _currentPlayerText:TextField;

        /**
         * The text field displaying the number of points in the current turn.
         *
         * @private
         */
        private var _ptsThisTurnText:TextField;

        /**
         * The dice.
         *
         * @private
         */
        private var _dice:Shape;

        /**
         * The number of points required to win the game.
         *
         * @private
         */
        private var _maxScore:uint = 100;

        /**
         * The text field displaying additional information about the game.
         *
         * @private
         */
        private var _statusText:TextField;

        /**
         * Creates a new PigTheDiceGame instance.
         */
        public function PigTheDiceGame() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        /**
         * Function which constructs the dice game when the object is added to the stage.
         *
         * @private
         */
        private function _init(e:Event = null):void {

            // Border and background

            graphics.beginFill(0xFFFFDD);
            graphics.lineStyle(2, 0xFFCC00);
            graphics.drawRect(0, 0, 450, 280)

            x = 20;
            y = 20;

            // Text fields and labels

            var currentPlayerText:TextField = _createTextField(_name1 + "'s turn", 20, 0, 10, 0xDD0000, TextFieldAutoSize.CENTER, width);
            var p1ScoreLabel:TextField = _createTextField(_name1 + "'s score:", 15, 20, currentPlayerText.y + currentPlayerText.height + 20, 0x000000, TextFieldAutoSize.LEFT, 120);
            var p1ScoreText:TextField = _createTextField("0", 17, 135, p1ScoreLabel.y, 0xFF0000, TextFieldAutoSize.RIGHT, 50);
            var p2ScoreLabel:TextField = _createTextField(_name2 + "'s score:", 15, 20, p1ScoreText.y + p1ScoreText.height + 5, 0x000000, TextFieldAutoSize.LEFT, 120);
            var p2ScoreText:TextField = _createTextField("0", 17, 135, p2ScoreLabel.y, 0xFF0000, TextFieldAutoSize.RIGHT, 50);
            var ptsThisTurnLabel:TextField = _createTextField("Points in this turn:", 15, 20, p2ScoreText.y + p2ScoreText.height + 15, 0x000000, TextFieldAutoSize.LEFT, 120);
            var ptsThisTurnText:TextField = _createTextField("0", 17, 135, ptsThisTurnLabel.y, 0xFF0000, TextFieldAutoSize.RIGHT, 50);

            // Dice

            var dice:Shape = new Shape();
            dice.x = 201;
            dice.y = ptsThisTurnText.y + ptsThisTurnText.height + 30;
            dice.visible = false;

            var statusText:TextField = _createTextField("Start Play!", 15, 0, dice.y + 70, 0x0000A0, TextFieldAutoSize.CENTER, 450);

            // "Roll" button

            var rollButton:Sprite = new Sprite();
            var rollButtonText:TextField = _createTextField("Roll dice", 17, 0, 0, 0x000000, TextFieldAutoSize.CENTER, 100);
            rollButton.mouseChildren = false;
            rollButton.buttonMode = true;
            rollButton.graphics.lineStyle(1, 0x444444);
            rollButton.graphics.beginFill(0xDDDDDD);
            rollButton.graphics.drawRect(0, 0, 100, rollButtonText.height);
            rollButton.x = 330;
            rollButton.y = 80;
            rollButton.addChild(rollButtonText);

            // "Hold" button

            var holdButton:Sprite = new Sprite();
            var holdButtonText:TextField = _createTextField("Hold", 17, 0, 0, 0x000000, TextFieldAutoSize.CENTER, 100);
            holdButton.mouseChildren = false;
            holdButton.buttonMode = true;
            holdButton.graphics.copyFrom(rollButton.graphics);
            holdButton.x = 330;
            holdButton.y = rollButton.y + rollButton.height + 10;
            holdButton.addChild(holdButtonText);

            rollButton.addEventListener(MouseEvent.CLICK, _rollButtonClick);
            holdButton.addEventListener(MouseEvent.CLICK, _holdButtonClick);

            _currentPlayerText = currentPlayerText;
            _p1ScoreText = p1ScoreText;
            _p2ScoreText = p2ScoreText;
            _ptsThisTurnText = ptsThisTurnText;
            _rollButton = rollButton;
            _holdButton = holdButton;
            _dice = dice;
            _statusText = statusText;

            addChild(currentPlayerText);
            addChild(p1ScoreLabel);
            addChild(p1ScoreText);
            addChild(p2ScoreLabel);
            addChild(p2ScoreText);
            addChild(ptsThisTurnLabel);
            addChild(ptsThisTurnLabel);
            addChild(ptsThisTurnText);
            addChild(statusText);
            addChild(_dice);
            addChild(rollButton);
            addChild(holdButton);

        }

        /**
         * Creates a new text field.
         *
         * @param text The text to be displayed in the text field.
         * @param size The font size of the text.
         * @param x The x-coordinate of the text field.
         * @param y The y-coordinate of the text field.
         * @param colour The text colour.
         * @param autoSize The text alignment mode.
         * @param width The width of the text field.
         * @return A TextField object.
         * @private
         */
        private function _createTextField(text:String, size:Number, x:Number, y:Number, colour:uint, autoSize:String, width:Number):TextField {
            var t:TextField = new TextField();
            t.defaultTextFormat = new TextFormat(null, size, colour);
            t.autoSize = autoSize;
            t.x = x;
            t.y = y;
            t.width = width;
            t.text = text;
            t.height = t.textHeight + 5;
            return t;
        }

        /**
         * Rolls the dice.
         *
         * @return The result of the roll (1-6)
         * @private
         */
        private function _rollDice():uint {

            // Since Math.random() returns a number between 0 and 1, multiplying it by 6 and then rounding down
            // gives a number between 0 and 5, so add 1 to it.

            var roll:uint = uint(Math.random() * 6) + 1;

            _dice.visible = true;
            var diceGraphics:Graphics = _dice.graphics;

            // Draw the dice.

            diceGraphics.clear();
            diceGraphics.lineStyle(2, 0x555555);
            diceGraphics.beginFill(0xFFFFFF);
            diceGraphics.drawRect(0, 0, 48, 48);
            diceGraphics.beginFill(0x000000);
            diceGraphics.lineStyle(0);

            switch ( roll ) {
                case 1:
                    diceGraphics.drawCircle(24, 24, 3);
                    break;
                case 2:
                    diceGraphics.drawCircle(16, 16, 3);
                    diceGraphics.drawCircle(32, 32, 3);
                    break;
                case 3:
                    diceGraphics.drawCircle(12, 12, 3);
                    diceGraphics.drawCircle(24, 24, 3);
                    diceGraphics.drawCircle(36, 36, 3);
                    break;
                case 4:
                    diceGraphics.drawCircle(16, 16, 3);
                    diceGraphics.drawCircle(16, 32, 3);
                    diceGraphics.drawCircle(32, 16, 3);
                    diceGraphics.drawCircle(32, 32, 3);
                    break;
                case 5:
                    diceGraphics.drawCircle(12, 12, 3);
                    diceGraphics.drawCircle(24, 24, 3);
                    diceGraphics.drawCircle(36, 36, 3);
                    diceGraphics.drawCircle(36, 12, 3);
                    diceGraphics.drawCircle(12, 36, 3);
                    break;
                case 6:
                    diceGraphics.drawCircle(16, 12, 3);
                    diceGraphics.drawCircle(16, 24, 3);
                    diceGraphics.drawCircle(16, 36, 3);
                    diceGraphics.drawCircle(32, 12, 3);
                    diceGraphics.drawCircle(32, 24, 3);
                    diceGraphics.drawCircle(32, 36, 3);
                    break;
            }

            return roll;

        }

        /**
         * The score of the first player.
         *
         * @private
         */
        private function get _p1Score():uint {
            return __p1Score;
        }

        /**
         * @private
         */
        private function set _p1Score(value:uint):void {
            __p1Score = value;
            _p1ScoreText.text = String(value);

            if ( value >= _maxScore ) {
                _currentPlayerText.text = "Game over!";
                _statusText.text = _name1 + " wins!";
                removeChild(_rollButton);
                removeChild(_holdButton);
            }
        }

        /**
         * The score of the second player.
         *
         * @private
         */
        private function get _p2Score():uint {
            return __p2Score;
        }

        /**
         * @private
         */
        private function set _p2Score(value:uint):void {
            __p2Score = value;
            _p2ScoreText.text = String(value);

            if ( value >= _maxScore ) {
                _currentPlayerText.text = "Game over!";
                _statusText.text = _name2 + " wins!";
                removeChild(_rollButton);
                removeChild(_holdButton);
            }
        }

        /**
         * The number of points in the current turn.
         *
         * @private
         */
        private function get _turnPts():uint {
            return __turnPts;
        }

        /**
         * @private
         */
        private function set _turnPts(value:uint):void {
            __turnPts = value;
            _ptsThisTurnText.text = String(value);

            if ( _isPlayer2 && __p2Score + value >= _maxScore ) {
                _ptsThisTurnText.text = "0";
                _p2Score += value;
            }
            else if ( ! _isPlayer2 && __p1Score + value >= _maxScore ) {
                _ptsThisTurnText.text = "0";
                _p1Score += value;
            }
        }

        /**
         * Function called when the "Roll dice" button is clicked.
         *
         * @private
         */
        private function _rollButtonClick(e:MouseEvent):void {
            var roll:uint = _rollDice();

            if ( roll == 1 ) {
                if ( _isPlayer2 ) {
                    _currentPlayerText.text = _name1 + "'s turn";
                    _statusText.text = _name2 + " rolls 1 and loses " + __turnPts + " points. " + _name1 + "'s turn now.";
                }
                else {
                    _currentPlayerText.text = _name2 + "'s turn";
                    _statusText.text = _name1 + " rolls 1 and loses " + __turnPts + " points. " + _name2 + "'s turn now.";
                }

                _isPlayer2 = ! _isPlayer2;
                _turnPts = 0;
            }
            else {
                _turnPts += roll;
                _statusText.text = "";
            }
        }

        /**
         * Function called when the "Hold" button is clicked.
         *
         * @private
         */
        private function _holdButtonClick(e:MouseEvent):void {
            if ( _isPlayer2 ) {
                _currentPlayerText.text = _name1 + "'s turn";
                _statusText.text = _name2 + " holds and wins " + __turnPts + " points. " + _name1 + "'s turn now.";
                _p2Score += __turnPts;
            }
            else {
                _currentPlayerText.text = _name2 + "'s turn";
                _statusText.text = _name1 + " holds and wins " + __turnPts + " points. " + _name2 + "'s turn now.";
                _p1Score += __turnPts;
            }

            _dice.visible = false;
            _turnPts = 0;
            _isPlayer2 = ! _isPlayer2;
        }

    }

}

```



## Ada


Uses Ada 2012.


### The Package Pig


We first define a package Pig, which we also use in the player task [[Pig the dice game/Player]].

Essentially, this package specifies two classes: Player provides the record keeping of the player's score saved so far, the points accumulated in the current round that may be added to the score, and the most recent roll. Actor is an abstract class to model the decision of rolling the dice once more or to save the current points into the score.

Also, there is a procedure Play to play the game, following whatever the actors do.


```Ada
package Pig is

   type Dice_Score is range 1 .. 6;

   type Player is tagged private;
   function Recent(P: Player) return Natural;
   function All_Recent(P: Player) return Natural;
   function Score(P: Player) return Natural;

   type Actor is abstract tagged null record;
   function Roll_More(A: Actor; Self, Opponent: Player'Class)
		     return Boolean is abstract;

   procedure Play(First, Second: Actor'Class; First_Wins: out Boolean);

private
   type Player is tagged record
      Score: Natural := 0;
      All_Recent: Natural := 0;
      Recent_Roll: Dice_Score := 1;
   end record;

end Pig;
```


The implementation of Pig is as follows:


```Ada
with Ada.Numerics.Discrete_Random;
package body Pig is

   function Score(P: Player) return Natural is (P.Score);
   function All_Recent(P: Player) return Natural is (P.All_Recent);
   function Recent(P: Player) return Natural is (Natural(P.Recent_Roll));
   function Has_Won(P: Player) return Boolean is (P.Score >= 100);

   package RND is new Ada.Numerics.Discrete_Random(Dice_Score);
   Gen: RND.Generator;

   procedure Roll(P: in out Player) is
   begin
      P.Recent_Roll := RND.Random(Gen);
      if P.Recent = 1 then
	 P.All_Recent := 0;
      else
	 P.All_Recent := P.All_Recent + P.Recent;
      end if;
   end Roll;

   procedure Add_To_Score(P: in out Player) is
   begin
      P.Score := P.Score + P.All_Recent;
      P.All_Recent := 0;
   end Add_To_Score;

   procedure Play(First, Second: Actor'Class;
		  First_Wins: out Boolean) is
      P1, P2: Player;
   begin
      loop
	 Roll(P1);
	 while First.Roll_More(P1, P2) and then P1.Recent > 1 loop
	    Roll(P1);
	 end loop;
	 Add_To_Score(P1);
	 exit when P1.Score >= 100;
	 Roll(P2);
	 while Second.Roll_More(P2, P1) and then P2.Recent > 1 loop
	    Roll(P2);
	 end loop;
	 Add_To_Score(P2);
	 exit when P2.Score >= 100;
      end loop;
      First_Wins := P1.Score >= 100;
   end Play;

begin
   RND.Reset(Gen);
end Pig;
```



### Solving the Task


Now, to actually '''play''' the game, we need a procedure Play_Pig. Mainly, we derive a
class Hand from the class Actor to implement manually playing the game.


```Ada
with Pig, Ada.Text_IO;

procedure Play_Pig is

   use Pig;

   type Hand is new Actor with record
      Name: String(1 .. 5);
   end record;
   function Roll_More(A: Hand; Self, Opponent: Player'Class) return Boolean;

   function Roll_More(A: Hand; Self, Opponent: Player'Class) return Boolean is
      Ch: Character := ' ';
      use Ada.Text_IO;
   begin
      Put(A.Name & " you:" & Natural'Image(Self.Score) &
            " (opponent:" & Natural'Image(Opponent.Score) &
            ") this round:" & Natural'Image(Self.All_Recent) &
            " this roll:" & Natural'Image(Self.Recent) &
            ";  add to score(+)?");
      Get(Ch);
      return Ch /= '+';
   end Roll_More;

   A1: Hand := (Name => "Alice");
   A2: Hand := (Name => "Bob  ");

   Alice: Boolean;
begin
   Play(A1, A2, Alice);
   Ada.Text_IO.Put_Line("Winner = " & (if Alice then "Alice!" else "Bob!"));
end Play_Pig;
```


```txt
Alice you: 0 (opponent: 0) this round: 3 this roll: 3;  add to score(+)?
Alice you: 0 (opponent: 0) this round: 5 this roll: 2;  add to score(+)?
Alice you: 0 (opponent: 0) this round: 10 this roll: 5;  add to score(+)?
Alice you: 0 (opponent: 0) this round: 13 this roll: 3;  add to score(+)?+
Bob   you: 0 (opponent: 13) this round: 6 this roll: 6;  add to score(+)?
Bob   you: 0 (opponent: 13) this round: 8 this roll: 2;  add to score(+)?
Bob   you: 0 (opponent: 13) this round: 11 this roll: 3;  add to score(+)?
Bob   you: 0 (opponent: 13) this round: 16 this roll: 5;  add to score(+)?
Bob   you: 0 (opponent: 13) this round: 18 this roll: 2;  add to score(+)?
Bob   you: 0 (opponent: 13) this round: 20 this roll: 2;  add to score(+)?+
Alice you: 13 (opponent: 20) this round: 2 this roll: 2;  add to score(+)?

... lots of more lines ...

Alice you: 76 (opponent: 66) this round: 8 this roll: 5;  add to score(+)?
Alice you: 76 (opponent: 66) this round: 13 this roll: 5;  add to score(+)?+
Bob   you: 66 (opponent: 89) this round: 4 this roll: 4;  add to score(+)?
Bob   you: 66 (opponent: 89) this round: 7 this roll: 3;  add to score(+)?
Bob   you: 66 (opponent: 89) this round: 12 this roll: 5;  add to score(+)?
Bob   you: 66 (opponent: 89) this round: 15 this roll: 3;  add to score(+)?
Bob   you: 66 (opponent: 89) this round: 21 this roll: 6;  add to score(+)?
Bob   you: 66 (opponent: 89) this round: 0 this roll: 1;  add to score(+)?
Alice you: 89 (opponent: 66) this round: 6 this roll: 6;  add to score(+)?
Alice you: 89 (opponent: 66) this round: 11 this roll: 5;  add to score(+)?+
Winner = Alice!
```



## AutoHotkey


```autohotkey
Gui, Font, s12, Verdana
Gui, Add, Text, vPlayer0, Player 0
Gui, Add, Text, vSum0, 000
Gui, Add, Button, Default, Roll
Gui, Add, Text, ys vLastRoll, Roll 0
Gui, Add, Text, vTurnSum, Sum 000
Gui, Add, Button, , Hold
Gui, Add, Text, ys vPlayer1, Player 1
Gui, Add, Text, vSum1, 000
Gui, Add, Button, , Reload
Gui, Show
GuiControl, Disable, Player1

CurrentPlayer := 0
ButtonRoll:
Loop 10
{
	Random, LastRoll, 1, 6
	GuiControl, , LastRoll, Roll %LastRoll%
	Sleep 50
}
If LastRoll != 1
{
	TurnSum += LastRoll
	GuiControl, , TurnSum, Sum %TurnSum%
	Return
}
TurnSum := 0
ButtonHold:
Sum%CurrentPlayer% += TurnSum
TurnSum := 0
GuiControl, , LastRoll, Roll
GuiControl, , TurnSum, Sum %TurnSum%
GuiControl, , Sum%CurrentPlayer%, % Sum%CurrentPlayer%
If Sum%CurrentPlayer% >= 100
{
	MsgBox Player %CurrentPlayer% Won!
	GuiClose:
	ExitApp
}
GuiControl, Disable, Player%CurrentPlayer%
CurrentPlayer := !CurrentPlayer
GuiControl, Enable, Player%CurrentPlayer%
Return

ButtonReload:
Reload
```



## BASIC256

```BASIC256

numjugadores = 2
maxpuntos = 100
Dim	almacenpuntos(3)
almacenpuntos[1] = 1
almacenpuntos[2] = 1

Cls: Print "The game of PIG"
Print "
### =========
" + Chr(13) + Chr(10)
Print "Si jugador saca un 1, no anota nada y se convierte en el turno del oponente."
Print "Si jugador saca 2-6, se agrega al total del turno y su turno continúa."
Print "Si jugador elige 'mantener', su total de puntos se añade a su puntuación, "
Print " y se convierte en el turno del siguiente jugador." + Chr(10)
Print "El primer jugador en anotar 100 o más puntos gana."&Chr(13)&Chr(10)

Do
	For jugador = 1 To 2 #numjugadores
		puntos = 0

		While almacenpuntos[jugador] <= maxpuntos
			Print
			Print "Jugador "; jugador; ": (";almacenpuntos[jugador];",";puntos;")";
			Input "  ¿Tirada? (Sn) ", nuevotiro
			If Upper(nuevotiro) = "S" Then
				tirada = Int(Rand* 5) + 1
				Print "  Tirada:"; tirada
				If tirada = 1 Then
					Print Chr(10) + "¡Pierdes tu turno! jugador "; jugador;
					Print " pero mantienes tu puntuación anterior de "; almacenpuntos[jugador]
					Exit While
				End If
				puntos = puntos + tirada
			Else
				almacenpuntos[jugador] = almacenpuntos[jugador] + puntos
				Print "  Te quedas con: "; almacenpuntos[jugador]
				If almacenpuntos[jugador] >= maxpuntos Then
					Print Chr(10) + "Gana el Jugador "; jugador; " con "; almacenpuntos[jugador]; " puntos."
					End
				End If
				Exit While
			End If
		End While
	Next jugador
Until false

```



## C++


```cpp

#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int PLAYERS = 2, MAX_POINTS = 100;

//--------------------------------------------------------------------------------------------------
class player
{
public:
    player() { reset(); }
    void reset()
    {
	name = "";
	current_score = round_score = 0;
    }
    string getName()             { return name; }
    void setName( string n )     { name = n; }
    int getCurrScore()           { return current_score; }
    void addCurrScore()          { current_score += round_score; }
    int getRoundScore()          { return round_score; }
    void addRoundScore( int rs ) { round_score += rs; }
    void zeroRoundScore()        { round_score = 0; }

private:
    string name;
    int current_score, round_score;
};
//--------------------------------------------------------------------------------------------------
class pigGame
{
public:
    pigGame() { resetPlayers(); }

    void play()
    {
	while( true )
	{
	    system( "cls" );
	    int p = 0;
	    while( true )
	    {
		if( turn( p ) )
		{
		    praise( p );
		    break;
		}

		++p %= PLAYERS;
	    }

	    string r;
	    cout << "Do you want to play again ( y / n )? "; cin >> r;
	    if( r != "Y" && r != "y" ) return;
	    resetPlayers();
	}
    }

private:
    void resetPlayers()
    {
	system( "cls" );
	string n;
	for( int p = 0; p < PLAYERS; p++ )
	{
	    _players[p].reset();
	    cout << "Enter name player " << p + 1 << ": "; cin >> n;
	    _players[p].setName( n );
	}

    }

    void praise( int p )
    {
	system( "cls" );
	cout << "CONGRATULATIONS " << _players[p].getName() << ", you are the winner!" << endl << endl;
	cout << "Final Score" << endl;
	drawScoreboard();
	cout << endl << endl;
    }

    void drawScoreboard()
    {
	for( int p = 0; p < PLAYERS; p++ )
	    cout << _players[p].getName() << ": " << _players[p].getCurrScore() << " points" << endl;
	cout << endl;
    }

    bool turn( int p )
    {
	system( "cls" );
	drawScoreboard();
	_players[p].zeroRoundScore();
	string r;
	int die;
	while( true )
	{
	    cout << _players[p].getName() << ", your round score is: " << _players[p].getRoundScore() << endl;
	    cout << "What do you want to do (H)old or (R)oll? "; cin >> r;
	    if( r == "h" || r == "H" )
	    {
		_players[p].addCurrScore();
		return _players[p].getCurrScore() >= MAX_POINTS;
	    }
	    if( r == "r" || r == "R" )
	    {
		die = rand() % 6 + 1;
		if( die == 1 )
		{
	    	    cout << _players[p].getName() << ", your turn is over." << endl << endl;
		    system( "pause" );
		    return false;
		}
		_players[p].addRoundScore( die );
	    }
	    cout << endl;
	}
	return false;
    }

    player	_players[PLAYERS];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    pigGame pg;
    pg.play();
    return 0;
}
//--------------------------------------------------------------------------------------------------

```

Output:

```txt

Chuck: 14 points
Bob: 16 points

Chuck, your round score is: 0
What do you want to do (H)old or (R)oll? r

Chuck, your round score is: 6
What do you want to do (H)old or (R)oll? r

Chuck, your round score is: 11
What do you want to do (H)old or (R)oll? r

Chuck, your round score is: 16
What do you want to do (H)old or (R)oll?

```



## C#


```c#

using System;
using System.IO;

namespace Pig {

	class Roll {
		public int TotalScore{get;set;}
		public int RollScore{get;set;}
		public bool Continue{get;set;}
	}

	class Player {
		public String Name{get;set;}
		public int Score {get;set;}
		Random rand;

		public Player() {
			Score = 0;
			rand = new Random();
		}

		public Roll Roll(int LastScore){
			Roll roll = new Roll();
			roll.RollScore = rand.Next(6) + 1;

			if(roll.RollScore == 1){
				roll.TotalScore = 0;
				roll.Continue = false;
				return roll;
			}

			roll.TotalScore = LastScore + roll.RollScore;
			roll.Continue = true;
			return roll;
		}

		public void FinalizeTurn(Roll roll){
			Score = Score + roll.TotalScore;
		}
	}

	public class Game {
		public static void Main(String[] argv){
			String input = null;
			Player[] players = new Player[2];

			// Game loop
			while(true){
				Console.Write("Greetings! Would you like to play a game (y/n)?");
				while(input == null){
					input = Console.ReadLine();
					if(input.ToLowerInvariant() == "y"){
						players[0] = new Player();
						players[1] = new Player();
						Console.Write("Player One, what's your name?");
						input = Console.ReadLine();
						players[0].Name = input;
						Console.Write("Player Two, what's your name?");
						input = Console.ReadLine();
						players[1].Name = input;
						Console.WriteLine(players[0].Name + " and " + players[1].Name + ", prepare to do battle!");
					} else if (input.ToLowerInvariant() == "n"){
						goto Goodbye; /* Not considered harmful */
					} else {
						input = null;
						Console.Write("I'm sorry, I don't understand. Play a game (y/n)?");
					}
				}

				// Play the game
				int currentPlayer = 0;
				Roll roll = null;
				bool runTurn = true;
				while(runTurn){
					Player p = players[currentPlayer];
					roll = p.Roll( (roll !=null) ? roll.TotalScore : 0 );
					if(roll.Continue){
						if(roll.TotalScore + p.Score > 99){
							Console.WriteLine("Congratulations, " + p.Name + "! You rolled a " + roll.RollScore + " for a final score of " + (roll.TotalScore + p.Score) + "!");
							runTurn = false;
						} else {
							Console.Write(p.Name + ": Roll " + roll.RollScore + "/Turn " + roll.TotalScore + "/Total " + (roll.TotalScore + p.Score) + ". Roll again (y/n)?");
							input = Console.ReadLine();
							if(input.ToLowerInvariant() == "y"){
								// Do nothing
							} else if (input.ToLowerInvariant() == "n"){
								p.FinalizeTurn(roll);
								currentPlayer = Math.Abs(currentPlayer - 1);
								Console.WriteLine();
							Console.WriteLine(players[0].Name + ": " + players[0].Score + "    " + players[1].Name + ": " + players[1].Score);
								Console.WriteLine(players[currentPlayer].Name + ", your turn begins.");
								roll = null;
							} else {
								input = null;
								Console.Write("I'm sorry, I don't understand. Play a game (y/n)?");
							}
						}
					} else {
						Console.WriteLine(p.Name + @", you rolled a 1 and lost your points for this turn.
Your current score:	" + p.Score);
						Console.WriteLine();
						Console.WriteLine(players[0].Name + ": " + players[0].Score + "    " + players[1].Name + ": " + players[1].Score);
						currentPlayer = Math.Abs(currentPlayer - 1);
					}
				}


				input = null;
			}
			Goodbye:
			Console.WriteLine("Thanks for playing, and remember: the house ALWAYS wins!");
			System.Environment.Exit(0);
		}
	}
}

```



## Clojure


```clojure
(def max 100)

(defn roll-dice []
  (let [roll (inc (rand-int 6))]
    (println "Rolled:" roll) roll))

(defn switch [player]
  (if (= player :player1) :player2 :player1))

(defn find-winner [game]
  (cond
    (>= (:player1 game) max) :player1
    (>= (:player2 game) max) :player2
    :else nil))

(defn bust []
  (println "Busted!") 0)

(defn hold [points]
  (println "Sticking with" points) points)

(defn play-round [game player temp-points]
  (println (format "%s: (%s, %s).  Want to Roll? (y/n) " (name player) (player game) temp-points))
  (let [input (clojure.string/upper-case (read-line))]
    (if (.equals input "Y")
      (let [roll (roll-dice)]
        (if (= 1 roll)
          (bust)
          (play-round game player (+ roll temp-points))))
      (hold temp-points))))

(defn play-game [game player]
    (let [winner (find-winner game)]
      (if (nil? winner)
        (let [points (play-round game player 0)]
          (recur (assoc game player (+ points (player game))) (switch player)))
        (println (name winner) "wins!"))))

(defn -main [& args]
  (println "Pig the Dice Game.")
  (play-game {:player1 0, :player2 0} :player1))
```



## Common Lisp


```lisp
(defconstant +max-score+ 100)
(defconstant +n-of-players+ 2)

(let ((scores (make-list +n-of-players+ :initial-element 0))
      (current-player 0)
      (round-score 0))
  (loop
     (format t "Player ~d: (~d, ~d). Rolling? (Y)"
             current-player
             (nth current-player scores)
             round-score)
     (if (member (read-line) '("y" "yes" "") :test #'string=)
         (let ((roll (1+ (random 6))))
           (format t "~tRolled ~d~%" roll)
           (if (= roll 1)
               (progn
                 (format t
                         "~tBust! you lose ~d but still keep your previous ~d~%"
                         round-score (nth current-player scores))
                 (setf round-score 0)
                 (setf current-player
                       (mod (1+ current-player) +n-of-players+)))
               (incf round-score roll)))
         (progn
           (incf (nth current-player scores) round-score)
           (setf round-score 0)
           (when (>= (apply #'max scores) 100)
             (return))
           (format t "~tSticking with ~d~%" (nth current-player scores))
           (setf current-player (mod (1+ current-player) +n-of-players+)))))
  (format t "~%Player ~d wins with a score of ~d~%" current-player
          (nth current-player scores)))
```

```txt
Player 0: (0, 0). Rolling? (Y)
 Rolled 5
Player 0: (0, 5). Rolling? (Y)
 Rolled 6
Player 0: (0, 11). Rolling? (Y)n
 Sticking with 11
Player 1: (0, 0). Rolling? (Y)
 Rolled 4
Player 1: (0, 4). Rolling? (Y)
 Rolled 3
Player 1: (0, 7). Rolling? (Y)
 Rolled 6
Player 1: (0, 13). Rolling? (Y)n
 Sticking with 13
...
Player 0: (81, 0). Rolling? (Y)
 Rolled 2
Player 0: (81, 2). Rolling? (Y)
 Rolled 2
Player 1: (85, 0). Rolling? (Y)
 Rolled 3
Player 1: (85, 3). Rolling? (Y)
 Rolled 3
Player 1: (85, 6). Rolling? (Y)
 Rolled 5
Player 1: (85, 11). Rolling? (Y)
 Rolled 6
Player 1: (85, 17). Rolling? (Y)n

Player 1 wins with a score of 102
```



## D

```d
void main() {
    import std.stdio, std.string, std.algorithm, std.random;
    enum maxScore = 100;
    enum playerCount = 2;
    immutable confirmations = ["yes", "y", ""];

    int[playerCount] safeScore;
    int player, score;

    while (true) {
        writef(" Player %d: (%d, %d). Rolling? (y/n) ", player,
               safeScore[player], score);
        if (safeScore[player] + score < maxScore &&
            confirmations.canFind(readln.strip.toLower)) {
            immutable rolled = uniform(1, 7);
            writefln(" Rolled %d", rolled);
            if (rolled == 1) {
                writefln(" Bust! You lose %d but keep %d\n",
                         score, safeScore[player]);
            } else {
                score += rolled;
                continue;
            }
        } else {
            safeScore[player] += score;
            if (safeScore[player] >= maxScore)
                break;
            writefln(" Sticking with %d\n", safeScore[player]);
        }

        score = 0;
        player = (player + 1) % playerCount;
    }

    writefln("\n\nPlayer %d wins with a score of %d",
             player, safeScore[player]);
}
```

```txt
 Player 0: (0, 0). Rolling? (y/n)
   Rolled 6
 Player 0: (0, 6). Rolling? (y/n)
   Rolled 5
 Player 0: (0, 11). Rolling? (y/n)
   Rolled 1
 Bust! you lose 11 but keep 0

 Player 1: (0, 0). Rolling? (y/n)
   Rolled 3
 Player 1: (0, 3). Rolling? (y/n)
   Rolled 4
 Player 1: (0, 7). Rolling? (y/n)
   Rolled 6
 Player 1: (0, 13). Rolling? (y/n) n
 Sticking with 13

 ...

 Player 0: (88, 0). Rolling? (y/n)
   Rolled 6
 Player 0: (88, 6). Rolling? (y/n)
   Rolled 4
 Player 0: (88, 10). Rolling? (y/n)
   Rolled 3
 Player 0: (88, 13). Rolling? (y/n)

 Player 0 wins with a score of 101
```


## Eiffel



```Eiffel

class
	PLAYER
create
	set_name
feature
	set_name(n:STRING)
		do
			name := n.twin
			set_points(0)
		end

	strategy(cur_points:INTEGER)
		local
			current_points, thrown:INTEGER
		do
			io.put_string ("You currently have " +points.out+". %NDo you want to save your points? Press y or n.%N")
			io.read_line
			if io.last_string.same_string ("y") then
				set_points(cur_points)
			else
				io.put_string ("Then throw again.%N")
				thrown:=throw_dice
				if thrown= 1 then
					io.put_string("You loose your points%N")
				else
					strategy(cur_points+thrown)
				end
			end

		end
	set_points (value:INTEGER)
		require
			value_not_neg: value >= 0
		do
			points := points + value
		end

	random: V_RANDOM
			-- Random sequence.
		once
			create Result
		end
	throw_dice: INTEGER
	        do
		        random.forth
	         	Result := random.bounded_item (1, 6)
	        end

	name: STRING
	points: INTEGER
end

```


```Eiffel

class
	PIG_THE_DICE

feature
	play
	local
		points, i: INTEGER
	do
		io.put_string("Welcome to the game.%N")
		initiate_players
		from

		until
			winner/=void
		loop
			across player as p  loop
			points:=p.item.throw_dice
			io.put_string ("%N" + p.item.name +" you throwed " + points.out + ".%N")
			if points =1 then
				io.put_string ("You loose your points.%N")
			else
				p.item.strategy(points)
			end
			if p.item.points >=100 then
				winner := p.item
				io.put_string ("%NThe winner is " + winner.name.out + ".%N")
			end
			end
		end
	end

	initiate_players
	local
		p1,p2: PLAYER
	do
		create player.make (1, 2)
		create p1.set_name ("Player1")
		player.put (p1, 1)
		create p2.set_name ("Player2")
		player.put (p2, 2)
	end

	player: V_ARRAY[PLAYER]
	winner: PLAYER
end

```

Test:

```Eiffel

class
	APPLICATION
inherit
	ARGUMENTS
create
	make
feature {NONE} -- Initialization
	make
	local
		do
			create pig
			pig.initiate_players
			pig.play
		end
	pig: PIG_THE_DICE
end

```

```txt

Welcome to the game.

Player1 you throwed 4.
You currently have 0.
Do you want to save your points? Press y or no.
y

Player2 you throwed 3.
You currently have 0.
Do you want to save your points? Press y or no.
n
Then throw again.
...
Player2 you throwed 6.
You currently have 98.
Do you want to save your points? Press y or n.
y

The winner is Player2.

```



## Erlang

Some of the code (ex: quit/2) is only there to make play during development easier. Some (ex: player_name/1) is only there to make [[Pig_the_dice_game/Player]] easier.

```Erlang

-module( pig_dice ).

-export( [game/1, goal/0, hold/2, player_name/1, players_totals/1, quit/2, roll/2, score/1, task/0] ).

-record( player, {name, score=0, total=0} ).

game( [_Player | _T]=Players ) ->
    My_pid = erlang:self(),
    erlang:spawn_link( fun() -> random:seed(os:timestamp()), game_loop( [#player{name=X} || X <- Players], 100, My_pid ) end ).

goal() -> 100.

hold( Player, Game ) -> Game ! {next_player, Player}.

players_totals( Game ) -> ask( Game, players_totals ).

player_name( Game ) -> ask( Game, name ).

quit( Player, Game ) -> Game ! {quit, Player}.

roll( Player, Game ) -> Game ! {roll, Player}.

score( Game ) -> ask( Game, score ).

task() ->
    Game = game( ["Player1", "Player2"] ),
    Play = erlang:spawn( fun() -> play_loop( Game ) end ),
    receive
    {pig, Result, Game} ->
    	erlang:exit( Play, kill ),
        task_display( Result ),
	Result
    end.



ask( Game, Question ) ->
    Game ! {Question, erlang:self()},
    receive
    {Question, Answer, Game} -> Answer
    end.

game_loop( [], _Goal, Report_pid ) -> Report_pid ! {pig, game_over_all_quite. erlang:self()};
game_loop( [#player{name=Name}=Player | T]=Players, Goal, Report_pid ) ->
	receive
	{name, Pid} ->
	       Pid ! {name, Player#player.name, erlang:self()},
	       game_loop( Players, Goal, Report_pid );
	{next_player, Name} ->
		New_players = game_loop_next_player( Player#player.total + Player#player.score, Players, Goal, Report_pid ),
		game_loop( New_players, Goal, Report_pid );
       {players_totals, Pid} ->
	       Pid ! {players_totals, [{X#player.name, X#player.total} || X <- Players], erlang:self()},
	       game_loop( Players, Goal, Report_pid );
	{quit, Name} -> game_loop( T, Goal, Report_pid );
	{roll, Name} ->
	       New_players = game_loop_roll( random:uniform(6), Players ),
	       game_loop( New_players, Goal, Report_pid );
	{score, Pid} ->
               Pid ! {score, Player#player.score, erlang:self()},
	       game_loop( Players, Goal, Report_pid )
	end.

game_loop_next_player( Total, [Player | T], Goal, Report_pid ) when Total >= Goal ->
	Report_pid ! {pig, [{X#player.name, X#player.total} || X <- [Player | T]]. erlang:self()},
	[];
game_loop_next_player( Total, [Player | T], _Goal, _Report_pid ) ->
	T ++ [Player#player{score=0, total=Total}].

game_loop_roll( 1, [Player | T] ) -> T ++ [Player#player{score=0}];
game_loop_roll( Score, [#player{score=Old_score}=Player | T] ) -> [Player#player{score=Old_score + Score} | T].

play_loop( Game ) ->
	Name = player_name( Game ),
	io:fwrite( "Currently ~p.~n", [players_totals(Game)] ),
	io:fwrite( "Name ~p.~n", [Name] ),
	roll( Name, Game ),
	Score = score( Game ),
	io:fwrite( "Rolled, score this round ~p.~n", [Score] ),
	play_loop_next( Score, Name, Game ),
	play_loop( Game ).

play_loop_command( {ok, ["y" ++ _T]}, _Name, _Game ) -> ok;
play_loop_command( {ok, ["n" ++ _T]}, Name, Game ) -> hold( Name, Game );
play_loop_command( {ok, ["q" ++ _T]}, Name, Game ) -> quit( Name, Game );
play_loop_command( {ok, _T}, Name, Game ) -> play_loop_command( io:fread("Roll again (y/n/q): ", "~s"), Name, Game ).

play_loop_next( 0, _Name, _Game ) -> io:fwrite( "~nScore 0, next player.~n" );
play_loop_next( _Score, Name, Game ) -> play_loop_command( io:fread("Roll again (y/n/q): ", "~s"), Name, Game ).

task_display( Results ) when is_list(Results) ->
        [{Name, Total} | Rest] = lists:reverse( lists:keysort(2, Results) ),
        io:fwrite( "Winner is ~p with total of ~p~n", [Name, Total] ),
        io:fwrite( "Then follows: " ),
        [io:fwrite("~p with ~p~n", [N, T]) || {N, T} <- Rest];
task_display( Result ) -> io:fwrite( "Result: ~p~n", [Result] ).

```

Start of game:

```txt

2> pig_dice:task().
Currently [{"Player1",0},{"Player2",0}].
Name "Player1".
Rolled, score this round 3.
Roll again (y/n/q): y
Currently [{"Player1",0},{"Player2",0}].
Name "Player1".
Rolled, score this round 8.
Roll again (y/n/q): y
Currently [{"Player1",0},{"Player2",0}].
Name "Player1".
Rolled, score this round 14.
Roll again (y/n/q): n
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 4.
Roll again (y/n/q): y
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 6.
Roll again (y/n/q): y
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 10.
Roll again (y/n/q): y
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 16.
Roll again (y/n/q): y
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 21.
Roll again (y/n/q): y
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 24.
Roll again (y/n/q): y
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 28.
Roll again (y/n/q): y
Currently [{"Player2",0},{"Player1",14}].
Name "Player2".
Rolled, score this round 0.

Score 0, next player.
Currently [{"Player1",14},{"Player2",0}].
Name "Player1".
Rolled, score this round 2.
Roll again (y/n/q):

```



## Forth

```forth
include lib/choose.4th
include lib/yesorno.4th

: turn                                 ( n1 -- n2)
  ." Player " . ." is up" cr           \ which player is up
  0 begin                              \ nothing so far
    s" Rolling" yes/no?                \ stand or roll?
  while                                \ now roll the dice
    6 choose 1+ dup ." Rolling " . dup 1 =
    if drop drop 0 else + ." (" dup 0 .r ." )" then cr dup 0=
  until                                \ until player stands or 1 is rolled
;

: pigthedice                           ( --)
  2 0 1 over                           \ setup players
  begin over turn + dup ." Total score: " . cr cr dup 100 < while 2swap repeat
  ." Player " swap . ." won with " . ." points." cr
  ." Player " swap . ." lost with " . ." points." cr
;                                      \ show the results

pigthedice
```



## FreeBASIC


```freebasic

Const numjugadores = 2
Const  maxpuntos = 100
Dim As Byte almacenpuntos(numjugadores), jugador, puntos, tirada
Dim As String nuevotiro

Cls: Color 15: Print "The game of PIG"
Print String(15, "=") + Chr(13) + Chr(10): Color 7
Print "Si jugador saca un 1, no anota nada y se convierte en el turno del oponente."
Print "Si jugador saca 2-6, se agrega al total del turno y su turno continúa."
Print "Si jugador elige 'mantener', su total de puntos se añade a su puntuación, "
Print " y se convierte en el turno del siguiente jugador." + Chr(10)
Print "El primer jugador en anotar 100 o más puntos gana." + Chr(13) + Chr(10): Color 7

Do
    For jugador = 1 To numjugadores
        puntos = 0

        While almacenpuntos(jugador) <= maxpuntos
            Color 15: Print
            Print Using "Jugador #: (&_, &)"; jugador;almacenpuntos(jugador);puntos;: Color 11
            Input "  ¿Tirada? (Sn) ", nuevotiro
            If Ucase(nuevotiro) = "S" Then
                tirada = Int(Rnd* 5) + 1
                Print "  Tirada:"; tirada
                If tirada = 1 Then
                    Color 11: Print Chr(10) + "­¡Pierdes tu turno! jugador"; jugador;
                    Print " pero mantienes tu puntuación anterior de "; almacenpuntos(jugador): Color 7
                    Exit While
                End If
                puntos = puntos + tirada
            Else
                almacenpuntos(jugador) = almacenpuntos(jugador) + puntos
                Print "  Te quedas con:"; almacenpuntos(jugador)
                If almacenpuntos(jugador) >= maxpuntos Then
                    Color 14: Print Chr(10) + "Gana el jugador"; jugador; " con"; almacenpuntos(jugador); " puntos."
                    Sleep: End
                End If
                Exit While
            End If
        Wend
    Next jugador
Loop

```



## Go


```Go
package main

import (
	"fmt"
	"math/rand"
	"strings"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano()) //Set seed to current time

	playerScores := [...]int{0, 0}
	turn := 0
	currentScore := 0

	for {
		player := turn % len(playerScores)

		fmt.Printf("Player %v [%v, %v], (H)old, (R)oll or (Q)uit: ", player,
			playerScores[player], currentScore)

		var answer string
		fmt.Scanf("%v", &answer)
		switch strings.ToLower(answer) {
		case "h": //Hold
			playerScores[player] += currentScore
			fmt.Printf("    Player %v now has a score of %v.\n\n", player, playerScores[player])

			if playerScores[player] >= 100 {
				fmt.Printf("    Player %v wins!!!\n", player)
				return
			}

			currentScore = 0
			turn += 1
		case "r": //Roll
			roll := rand.Intn(6) + 1

			if roll == 1 {
				fmt.Printf("    Rolled a 1. Bust!\n\n")
				currentScore = 0
				turn += 1
			} else {
				fmt.Printf("    Rolled a %v.\n", roll)
				currentScore += roll
			}
		case "q": //Quit
			return
		default: //Incorrent input
			fmt.Print("  Please enter one of the given inputs.\n")
		}
	}
	fmt.Printf("Player %v wins!!!\n", (turn-1)%len(playerScores))
}
```



## Groovy

Currently hard coded for 2 players, but will work for multiple players

```Groovy

class PigDice {

	final static int maxScore = 100;
	final static yesses = ["yes", "y", "", "Y", "YES"]

	static main(args) {
		def playersCount = 2
		Scanner sc = new Scanner(System.in)
		Map scores = [:]
		def current = 0
		def player = 0
		def gameOver = false
		def firstThrow = true
		Random rnd = new Random()

		// Initialise the players' scores
		(0..(playersCount-1)).each{ it->
			scores[it] = 0
		}

		// Game starts
		while (!gameOver) {
			def nextPlayer = false
			String ln

			// Automatic rolls for the first dice roll
			if (firstThrow){
				println "player ${player+1} Auto Rolling... "
				ln = 'y'
				firstThrow = false
			} else {
				println "player ${player+1} Rolling? Yes(y) or No(n) "
				ln = sc.nextLine()
			}

			if (ln in yesses){
				// if yes then roll the dice
				int rolled = rnd.nextInt(6) + 1
				print  "The Roll was $rolled ---  "

				if (rolled == 1) {
					println " Bust! Player ${player+1} loses $current but keep ${scores[player]}"
					current = 0
					nextPlayer = true
					firstThrow = true
				} else {
					//  dice rolls 2 to 6
					current = current + rolled
					if ((current + scores[player]) > maxScore){
						gameOver = true
					}else{
						// as a session score gets larger the message returned changes
						switch (current){
							case 6..15:
								print "Good. "
								break
							case 15..29:
								print "lucky! "
								break
							case 29..39:
								print "Great! "
								break
							default:
								print "Amazing "
						}
						println "Player ${player+1} now has $current this session (possible score of ${current + scores[player]})"
					}
				}
			} else{
				// if no then bank the session score
				nextPlayer = true
				firstThrow = true
				scores[player] = scores[player] + current
				current = 0
				println "chicken! player ${player+1} now has ${scores[player]} and $gameOver"
				println  "Current scores :"
				for (i in scores){
					println "player ${i.key + 1}| ${i.value} "
				}
				println "------------------------------"

			}
			println ""

			if (nextPlayer) {
				player = (player+1)%playersCount
				println "** Next player is ${player+1}"
			}
		}

		// Game ends
		println "Player ${player+1} wins"
	}
}

```



## Haskell


```Haskell

import System.Random (randomRIO)

data Score = Score { stack :: Int, score :: Int }

main :: IO ()
main = loop (Score 0 0) (Score 0 0)

loop :: Score -> Score -> IO ()
loop p1 p2 = do
  putStrLn $ "\nPlayer 1 ~ " ++ show (score p1)
  p1' <- askPlayer p1
  if (score p1') >= 100
    then putStrLn "P1 won!"
    else do
      putStrLn $ "\nPlayer 2 ~ " ++ show (score p2)
      p2' <- askPlayer p2
      if (score p2') >= 100
        then putStrLn "P2 won!"
        else loop p1' p2'


askPlayer :: Score -> IO Score
askPlayer (Score stack score) = do
  putStr   "\n(h)old or (r)oll? "
  answer <- getChar
  roll   <- randomRIO (1,6)
  case (answer, roll) of
    ('h', _) -> do
      putStrLn $ "      => Score = " ++ show (stack + score)
      return $ Score 0 (stack + score)
    ('r', 1) -> do
      putStrLn $ " => 1 => Sorry - stack was resetted"
      return $ Score 0 score
    ('r', _) -> do
      putStr $ " => " ++ show roll ++ " => current stack = " ++ show (stack + roll)
      askPlayer $ Score (stack + roll) score
    _        -> do
      putStrLn "\nInvalid input - please try again."
      askPlayer $ Score stack score

```


Example output:


```txt


Player 1 ~ 0

(h)old or (r)oll? r => 5 => current stack = 5
(h)old or (r)oll? r => 5 => current stack = 10
(h)old or (r)oll? r => 3 => current stack = 13
(h)old or (r)oll? r => 4 => current stack = 17
(h)old or (r)oll? h      => Score = 17

Player 2 ~ 0

(h)old or (r)oll? r => 4 => current stack = 4
(h)old or (r)oll? r => 3 => current stack = 7
(h)old or (r)oll? r => 1 => Sorry - stack was resetted

Player 1 ~ 17

(h)old or (r)oll? r => 5 => current stack = 5
(h)old or (r)oll? r => 2 => current stack = 7
(h)old or (r)oll? h      => Score = 24

...

Player 1 ~ 52

(h)old or (r)oll? r => 4 => current stack = 4
(h)old or (r)oll? r => 3 => current stack = 7
(h)old or (r)oll? r => 4 => current stack = 11
(h)old or (r)oll? r => 4 => current stack = 15
(h)old or (r)oll? r => 1 => Sorry - stack was resetted

Player 2 ~ 97

(h)old or (r)oll? r => 4 => current stack = 4
(h)old or (r)oll? h      => Score = 101
P2 won!


```



## J



```j
require'general/misc/prompt' NB. was require'misc' in j6

status=:3 :0
  'pid cur tot'=. y
   player=. 'player ',":pid
   potential=. ' potential: ',":cur
   total=. ' total: ',":tot
  smoutput player,potential,total
)

getmove=:3 :0
  whilst.1~:+/choice do.
    choice=.'HRQ' e. prompt '..Roll the dice or Hold or Quit? [R or H or Q]: '
  end.
  choice#'HRQ'
)

NB. simulate an y player game of pig
pigsim=:3 :0
  smoutput (":y),' player game of pig'
  scores=.y#0
  while.100>>./scores do.
    for_player.=i.y do.
      smoutput 'begining of turn for player ',":pid=.1+I.player
      current=. 0
      whilst. (1 ~: roll) *. 'R' = move do.
        status pid, current, player+/ .*scores
        if.'R'=move=. getmove'' do.
          smoutput 'rolled a ',":roll=. 1+?6
          current=. (1~:roll)*current+roll end. end.
      scores=. scores+(current*player)+100*('Q'e.move)*-.player
      smoutput 'player scores now: ',":scores end. end.
  smoutput 'player ',(":1+I.scores>:100),' wins'
)
```


Example game:

<lang>   pigsim 2
2 player game of pig
begining of turn for player 1
player 1 potential: 0 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 3
player 1 potential: 3 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 6
player 1 potential: 9 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 4
player 1 potential: 13 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 6
player 1 potential: 19 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 2
player 1 potential: 21 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: H
player scores now: 21 0
begining of turn for player 2
player 2 potential: 0 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 3
player 2 potential: 3 total: 0
  Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 6
player 2 potential: 9 total: 0
  Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 4
player 2 potential: 13 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 6
player 2 potential: 19 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 3
player 2 potential: 22 total: 0
..Roll the dice or Hold or Quit? [R or H or Q]: H
player scores now: 21 22
begining of turn for player 1
player 1 potential: 0 total: 21
..Roll the dice or Hold or Quit? [R or H or Q]: R

...

..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 6
player 1 potential: 22 total: 62
..Roll the dice or Hold or Quit? [R or H or Q]: H
player scores now: 84 90
begining of turn for player 2
player 2 potential: 0 total: 90
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 6
player 2 potential: 6 total: 90
..Roll the dice or Hold or Quit? [R or H or Q]: R
rolled a 6
player 2 potential: 12 total: 90
..Roll the dice or Hold or Quit? [R or H or Q]: H
player scores now: 84 102
player 2 wins
```



## Java

```java
import java.util.*;

public class PigDice {

    public static void main(String[] args) {
        final int maxScore = 100;
        final int playerCount = 2;
        final String[] yesses = {"y", "Y", ""};

        int[] safeScore = new int[2];
        int player = 0, score = 0;

        Scanner sc = new Scanner(System.in);
        Random rnd = new Random();

        while (true) {
            System.out.printf(" Player %d: (%d, %d) Rolling? (y/n) ", player,
                    safeScore[player], score);
            if (safeScore[player] + score < maxScore
                    && Arrays.asList(yesses).contains(sc.nextLine())) {
                final int rolled = rnd.nextInt(6) + 1;
                System.out.printf(" Rolled %d\n", rolled);
                if (rolled == 1) {
                    System.out.printf(" Bust! You lose %d but keep %d\n\n",
                            score, safeScore[player]);
                } else {
                    score += rolled;
                    continue;
                }
            } else {
                safeScore[player] += score;
                if (safeScore[player] >= maxScore)
                    break;
                System.out.printf(" Sticking with %d\n\n", safeScore[player]);
            }
            score = 0;
            player = (player + 1) % playerCount;
        }
        System.out.printf("\n\nPlayer %d wins with a score of %d",
                player, safeScore[player]);
    }
}
```


```java5
import java.util.Arrays;
import java.util.Random;
import java.util.Scanner;
import java.util.stream.IntStream;

public interface PigDice {
  public static void main(String... arguments) {
    final int maxScore = 100;
    final int playerCount = 2;
    final String[] yesses = {"y", "Y", ""};

    final Scanner scanner = new Scanner(System.in);
    final Random random = new Random();

    final int[] safeScore = new int[2];
    final int[] score = new int[2];

    IntStream.iterate(0, player -> (player + 1) % playerCount)
      .map(player -> {
        boolean isRolling = true;
        while (isRolling) {
          System.out.printf(
            "Player %d: (%d, %d) Rolling? (y/n) ",
            player,
            safeScore[player],
            score[player]
          );
          isRolling =
            safeScore[player] + score[player] < maxScore
              && Arrays.asList(yesses).contains(scanner.nextLine())
          ;
          if (isRolling) {
            final int rolled = random.nextInt(6) + 1;
            System.out.printf("Rolled %d\n", rolled);
            if (rolled == 1) {
              System.out.printf(
                "Bust! You lose %d but keep %d\n\n",
                score[player],
                safeScore[player]
              );
              return -1;
            } else {
              score[player] += rolled;
            }
          } else {
            safeScore[player] += score[player];
            if (safeScore[player] >= maxScore) {
              return player;
            }
            System.out.printf("Sticking with %d\n\n", safeScore[player]);
          }
        }
        score[player] = 0;
        return -1;
      })
      .filter(player -> player > -1)
      .findFirst()
      .ifPresent(player ->
        System.out.printf(
          "\n\nPlayer %d wins with a score of %d",
          player,
          safeScore[player]
        )
      )
    ;
  }
}
```



```txt
 Player 0: (0, 0) Rolling? (y/n) y
 Rolled 3
 Player 0: (0, 3) Rolling? (y/n) Y
 Rolled 5
 Player 0: (0, 8) Rolling? (y/n)
 Rolled 2
 Player 0: (0, 10) Rolling? (y/n) n
 Sticking with 10

 Player 1: (0, 0) Rolling? (y/n)
 Rolled 1
 Bust! You lose 0 but keep 0

 Player 0: (10, 0) Rolling? (y/n)
 Rolled 1
 Bust! You lose 0 but keep 10

 Player 1: (0, 0) Rolling? (y/n)
 Rolled 4
 Player 1: (0, 4) Rolling? (y/n)
 Rolled 5
 Player 1: (0, 9) Rolling? (y/n)
 Rolled 1
 Bust! You lose 9 but keep 0

(...)

 Player 1: (96, 0) Rolling? (y/n)
 Rolled 4
 Player 1: (96, 4) Rolling? (y/n)
 Rolled 2
 Player 1: (96, 6) Rolling? (y/n) n

Player 1 wins with a score of 102
```



## Julia

The game is built around the <tt>PigPlayer</tt> type, which contains the player information, including a reference to the strategy function, <tt>strat</tt>, that is to be used to determined whether a player is going to continue to roll.  In this incarnation of the game, there is only one strategy function available, <tt>pig_manual</tt>, which gets this decision from user input.

```Julia

type PigPlayer
    name::String
    score::Int
    strat::Function
end

function PigPlayer(a::String)
    PigPlayer(a, 0, pig_manual)
end

function scoreboard(pps::Array{PigPlayer,1})
    join(map(x->@sprintf("%s has %d", x.name, x.score), pps), " | ")
end

function pig_manual(pps::Array{PigPlayer,1}, pdex::Integer, pot::Integer)
    pname = pps[pdex].name
    print(pname, " there is ", @sprintf("%3d", pot), " in the pot.  ")
    print("<ret> to continue rolling? ")
    return chomp(readline()) == ""
end

function pig_round(pps::Array{PigPlayer,1}, pdex::Integer)
    pot = 0
    rcnt = 0
    while pps[pdex].strat(pps, pdex, pot)
        rcnt += 1
        roll = rand(1:6)
        if roll == 1
            return (0, rcnt, false)
        else
            pot += roll
        end
    end
    return (pot, rcnt, true)
end

function pig_game(pps::Array{PigPlayer,1}, winscore::Integer=100)
    pnum = length(pps)
    pdex = pnum
    println("Playing a game of Pig the Dice.")
    while(pps[pdex].score < winscore)
        pdex = rem1(pdex+1, pnum)
        println(scoreboard(pps))
        println(pps[pdex].name, " is now playing.")
        (pot, rcnt, ispotwon) = pig_round(pps, pdex)
        print(pps[pdex].name, " played ", rcnt, " rolls ")
        if ispotwon
            println("and scored ", pot, " points.")
            pps[pdex].score += pot
        else
            println("and butsted.")
        end
    end
    println(pps[pdex].name, " won, scoring ", pps[pdex].score, " points.")
end

pig_game([PigPlayer("Alice"), PigPlayer("Bob")])

```


```txt

Playing a game of Pig the Dice.
Alice has 0 | Bob has 0
Alice is now playing.
Alice there is   0 in the pot.  <ret> to continue rolling?
Alice there is   3 in the pot.  <ret> to continue rolling?
Alice there is   7 in the pot.  <ret> to continue rolling?
Alice there is  13 in the pot.  <ret> to continue rolling?
Alice there is  15 in the pot.  <ret> to continue rolling?
Alice there is  18 in the pot.  <ret> to continue rolling?
Alice played 5 rolls and scored 18 points.
Alice has 18 | Bob has 0
Bob is now playing.
Bob there is   0 in the pot.  <ret> to continue rolling?
Bob played 1 rolls and butsted.
Alice has 18 | Bob has 0
Alice is now playing.
Alice there is   0 in the pot.  <ret> to continue rolling?
Alice there is   2 in the pot.  <ret> to continue rolling?
Alice played 2 rolls and butsted.
...
Alice has 54 | Bob has 94
Alice is now playing.
Alice there is   0 in the pot.  <ret> to continue rolling?
Alice there is   3 in the pot.  <ret> to continue rolling?
Alice there is   9 in the pot.  <ret> to continue rolling?
Alice there is  12 in the pot.  <ret> to continue rolling?
Alice there is  17 in the pot.  <ret> to continue rolling?
Alice there is  22 in the pot.  <ret> to continue rolling?
Alice there is  27 in the pot.  <ret> to continue rolling?
Alice there is  31 in the pot.  <ret> to continue rolling?
Alice played 7 rolls and scored 31 points.
Alice has 85 | Bob has 94
Bob is now playing.
Bob there is   0 in the pot.  <ret> to continue rolling?
Bob there is   3 in the pot.  <ret> to continue rolling?
Bob there is   5 in the pot.  <ret> to continue rolling?
Bob there is   8 in the pot.  <ret> to continue rolling?
Bob played 3 rolls and scored 8 points.
Bob won, scoring 102 points.

```



## Kotlin


```scala
// version 1.1.2

fun main(Args: Array<String>) {
    print("Player 1 - Enter your name : ")
    val name1 = readLine()!!.trim().let { if (it == "") "PLAYER 1" else it.toUpperCase() }
    print("Player 2 - Enter your name : ")
    val name2 = readLine()!!.trim().let { if (it == "") "PLAYER 2" else it.toUpperCase() }
    val names = listOf(name1, name2)
    val r = java.util.Random()
    val totals = intArrayOf(0, 0)
    var player = 0
    while (true) {
        println("\n${names[player]}")
        println("  Your total score is currently ${totals[player]}")
        var score = 0
        while (true) {
            print("  Roll or Hold r/h : ")
            val rh = readLine()!![0].toLowerCase()
            if (rh == 'h') {
                totals[player] += score
                println("  Your total score is now ${totals[player]}")
                if (totals[player] >= 100) {
                    println("  So, ${names[player]}, YOU'VE WON!")
                    return
                }
                player = if (player == 0) 1 else 0
                break
            }
            if (rh != 'r') {
                println("    Must be 'r'or 'h', try again")
                continue
            }
            val dice = 1 + r.nextInt(6)
            println("    You have thrown a $dice")
            if (dice == 1) {
                println("    Sorry, your score for this round is now 0")
                println("  Your total score remains at ${totals[player]}")
                player = if (player == 0) 1 else 0
                break
            }
            score += dice
            println("    Your score for the round is now $score")
        }
    }
}
```


```txt

Player 1 - Enter your name : Donald
Player 2 - Enter your name : Barack

DONALD
  Your total score is currently 0
  Roll or Hold r/h : r
    You have thrown a 6
    Your score for the round is now 6
  Roll or Hold r/h : r
    You have thrown a 2
    Your score for the round is now 8
  Roll or Hold r/h : r
    You have thrown a 4
    Your score for the round is now 12
  Roll or Hold r/h : r
    You have thrown a 2
    Your score for the round is now 14
  Roll or Hold r/h : r
    You have thrown a 4
    Your score for the round is now 18
  Roll or Hold r/h : r
    You have thrown a 6
    Your score for the round is now 24
  Roll or Hold r/h : h
  Your total score is now 24

BARACK
  Your total score is currently 0
  Roll or Hold r/h : r
    You have thrown a 5
    Your score for the round is now 5
  Roll or Hold r/h : r
    You have thrown a 2
    Your score for the round is now 7
  Roll or Hold r/h : r
    You have thrown a 3
    Your score for the round is now 10
  Roll or Hold r/h : r
    You have thrown a 5
    Your score for the round is now 15
  Roll or Hold r/h : r
    You have thrown a 4
    Your score for the round is now 19
  Roll or Hold r/h : r
    You have thrown a 4
    Your score for the round is now 23
  Roll or Hold r/h : h
  Your total score is now 23

DONALD
  Your total score is currently 24
  Roll or Hold r/h : r
    You have thrown a 5
    Your score for the round is now 5
  Roll or Hold r/h : r
    You have thrown a 5
    Your score for the round is now 10
  Roll or Hold r/h : r
    You have thrown a 4
    Your score for the round is now 14
  Roll or Hold r/h : r
    You have thrown a 1
    Sorry, your score for this round is now 0
  Your total score remains at 24
  .........
  .........
DONALD
  Your total score is currently 81
  Roll or Hold r/h : r
    You have thrown a 1
    Sorry, your score for this round is now 0
  Your total score remains at 81

BARACK
  Your total score is currently 85
  Roll or Hold r/h : r
    You have thrown a 5
    Your score for the round is now 5
  Roll or Hold r/h : r
    You have thrown a 2
    Your score for the round is now 7
  Roll or Hold r/h : r
    You have thrown a 3
    Your score for the round is now 10
  Roll or Hold r/h : r
    You have thrown a 3
    Your score for the round is now 13
  Roll or Hold r/h : r
    You have thrown a 5
    Your score for the round is now 18
  Roll or Hold r/h : h
  Your total score is now 103
  So, BARACK, YOU'VE WON!

```



## Lua

Supports any number of players

```lua
local numPlayers = 2
local maxScore = 100
local scores = { }
for i = 1, numPlayers do
	scores[i] = 0  -- total safe score for each player
end
math.randomseed(os.time())
print("Enter a letter: [h]old or [r]oll?")
local points = 0 -- points accumulated in current turn
local p = 1 -- start with first player
while true do
	io.write("\nPlayer "..p..", your score is ".. scores[p]..", with ".. points.." temporary points.  ")
	local reply = string.sub(string.lower(io.read("*line")), 1, 1)
	if reply == 'r' then
		local roll = math.random(6)
		io.write("You rolled a " .. roll)
		if roll == 1 then
			print(".  Too bad. :(")
			p = (p % numPlayers) + 1
			points = 0
		else
			points = points + roll
		end
	elseif reply == 'h' then
		scores[p] = scores[p] + points
		if scores[p] >= maxScore then
			print("Player "..p..", you win with a score of "..scores[p])
			break
		end
		print("Player "..p..", your new score is " .. scores[p])
		p = (p % numPlayers) + 1
		points = 0
	end
end

```


## M2000 Interpreter


```M2000 Interpreter

Module GamePig {
      Print "Game of Pig"
      dice=-1
      res$=""
      Player1points=0
      Player1sum=0
      Player2points=0
      Player2sum=0
      HaveWin=False
      score()
      \\ for simulation, feed the keyboard buffer with R and H
      simulate$=String$("R", 500)
      For i=1 to 100
            Insert Random(1,Len(simulate$)) simulate$="H"
      Next i
      Keyboard simulate$
      \\ end simulation
      while res$<>"Q" {
            Print "Player 1 turn"
            PlayerTurn(&Player1points, &player1sum)
            if res$="Q" then exit
            Player1sum+=Player1points
            Score()
            Print "Player 2 turn"
            PlayerTurn(&Player2points,&player2sum)
            if res$="Q" then exit
            Player2sum+=Player2points
            Score()
      }
      If HaveWin then {
            Score()
            If Player1Sum>Player2sum then {
                  Print "Player 1 Win"
            } Else Print "Player 2 Win"
      }

      Sub Rolling()
            dice=random(1,6)
            Print "dice=";dice
      End Sub
      Sub PlayOrQuit()
            Print "R -Roling Q -Quit"
            Repeat {
                  res$=Ucase$(Key$)
            } Until Instr("RQ", res$)>0
      End Sub
      Sub PlayAgain()
            Print "R -Roling H -Hold Q -Quit"
            Repeat {
                  res$=Ucase$(Key$)
            } Until Instr("RHQ", res$)>0
      End Sub
      Sub PlayerTurn(&playerpoints, &sum)
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
GamePig

```



## Maple


```maple
pig := proc()
	local Points, pointsThisTurn, answer, rollNum, i, win;
	randomize();
	Points := [0, 0];
	win := [false, 0];
	while not win[1] do
		for i to 2 do
			if not win[1] then
				printf("Player %a's turn.\n", i);
				answer := "";
				pointsThisTurn := 0;
				while not answer = "HOLD" do
					while not answer = "ROLL" and not answer = "HOLD" do
						printf("Would you like to ROLL or HOLD?\n");
						answer := StringTools:-UpperCase(readline());
						if not answer = "ROLL" and not answer = "HOLD" then
							printf("Invalid answer.\n\n");
						end if;
					end do;
					if answer = "ROLL" then
						rollNum := rand(1..6)();
						printf("You rolled a %a!\n", rollNum);
						if rollNum = 1 then
							pointsThisTurn := 0;
							answer := "HOLD";
						else
							pointsThisTurn := pointsThisTurn + rollNum;
							answer := "";
							printf("Your points so far this turn: %a.\n\n", pointsThisTurn);
						end if;
					end if;
				end do;
				printf("This turn is over! Player %a gained %a points this turn.\n\n", i, pointsThisTurn);
				Points[i] := Points[i] + pointsThisTurn;
				if Points[i] >= 100 then
					win := [true, i];
				end if;
				printf("Player 1 has %a points. Player 2 has %a points.\n\n", Points[1], Points[2]);
			end if;
		end do;
	end do;
	printf("Player %a won with %a points!\n", win[2], Points[win[2]]);
end proc;

pig();
```

```txt

Player 1's turn.
Would you like to ROLL or HOLD?
You rolled a 5!
Your points so far this turn: 5.

Would you like to ROLL or HOLD?
This turn is over! Player 1 gained 5 points this turn.

Player 1 has 5 points. Player 2 has 0 points.

Player 2's turn.
Would you like to ROLL or HOLD?
You rolled a 6!
Your points so far this turn: 6.

Would you like to ROLL or HOLD?
You rolled a 1!
This turn is over! Player 2 gained 0 points this turn.

Player 1 has 5 points. Player 2 has 0 points.

Player 1's turn.
Would you like to ROLL or HOLD?

...

```



## Mathematica

<lang>DynamicModule[{score, players = {1, 2}, roundscore = 0,
  roll}, (score@# = 0) & /@ players;
 Panel@Dynamic@
   Column@{Grid[
      Prepend[{#, score@#} & /@ players, {"Player", "Score"}],
      Background -> {None, 2 -> Gray}], roundscore,
     If[ValueQ@roll, Row@{"Rolled ", roll}, ""],
     If[IntegerQ@roundscore,
      Row@{Button["Roll", roll = RandomInteger[{1, 6}];
         If[roll == 1, roundscore = 0; players = RotateLeft@players,
          roundscore += roll]],
        Button["Hold", score[players[[1]]] += roundscore;
         roundscore = 0;
         If[score[players[[1]]] >= 100, roll =.;
          roundscore = Row@{players[[1]], " wins."},
          players = RotateLeft@players]]},
      Button["Play again.",
       roundscore = 0; (score@# = 0) & /@ players]]}]
```



## MiniScript


```MiniScript
// Pig the Dice for two players.
Player = {}
Player.score = 0
Player.doTurn = function()
    rolls = 0
    pot = 0
    print self.name + "'s Turn!"
    while true
        if self.score + pot >= goal then
            print "   " + self.name.upper + " WINS WITH " + (self.score + pot) + "!"
            inp = "H"
        else
            inp = input(self.name + ", you have " + pot + " in the pot.  [R]oll or Hold? ")
        end if
        if inp == "" or inp[0].upper == "R" then
            die = ceil(rnd*6)
            if die == 1 then
                print "   You roll a 1.  Busted!"
                return
            else
                print "   You roll a " + die + "."
                pot = pot + die
            end if
        else
            self.score = self.score + pot
            return
        end if
    end while
end function

p1 = new Player
p1.name = "Alice"
p2 = new Player
p2.name = "Bob"
goal = 100

while p1.score < goal and p2.score < goal
    for player in [p1, p2]
        print
        print p1.name + ": " + p1.score + "  |  " + p2.name + ": " + p2.score
        player.doTurn
        if player.score >= goal then break
    end for
end while
```


```txt
Alice: 0  |  Bob: 0
Alice's Turn!
Alice, you have 0 in the pot.  [R]oll or Hold?
   You roll a 4.
Alice, you have 4 in the pot.  [R]oll or Hold?
   You roll a 5.
Alice, you have 9 in the pot.  [R]oll or Hold?
   You roll a 5.
Alice, you have 14 in the pot.  [R]oll or Hold? h

Alice: 14  |  Bob: 0
Bob's Turn!
Bob, you have 0 in the pot.  [R]oll or Hold?
   You roll a 6.
Bob, you have 6 in the pot.  [R]oll or Hold?
   You roll a 4.
Bob, you have 10 in the pot.  [R]oll or Hold?
   You roll a 4.
Bob, you have 14 in the pot.  [R]oll or Hold?
   You roll a 6.
Bob, you have 20 in the pot.  [R]oll or Hold? h

Alice: 14  |  Bob: 20
Alice's Turn!
Alice, you have 0 in the pot.  [R]oll or Hold?
   You roll a 1.  Busted!

...

Alice: 49  |  Bob: 90
Bob's Turn!
Bob, you have 0 in the pot.  [R]oll or Hold?
   You roll a 5.
Bob, you have 5 in the pot.  [R]oll or Hold?
   You roll a 3.
Bob, you have 8 in the pot.  [R]oll or Hold?
   You roll a 6.
   BOB WINS WITH 104!
```



## Objeck


```objeck

﻿﻿class Pig {
  function : Main(args : String[]) ~ Nil {
    player_count := 2;
    max_score := 100;
    safe_score := Int->New[player_count];
    player := 0; score := 0;

    while(true) {
      safe := safe_score[player];
      " Player {$player}: ({$safe}, {$score}) Rolling? (y/n) "->PrintLine();
      rolling := IO.Console->ReadString();
      if(safe_score[player] + score < max_score & (rolling->Equals("y") | rolling->Equals("yes"))) {
        rolled := ((Float->Random() * 100.0)->As(Int) % 6) + 1;
        " Rolled {$rolled}"->PrintLine();
        if(rolled = 1) {
          safe := safe_score[player];
          "  Bust! you lose {$score} but still keep your previous {$safe}\n"->PrintLine();
          score := 0;
          player := (player + 1) % player_count;
        }
        else {
          score += rolled;
        };
      }
      else {
        safe_score[player] += score;
        if(safe_score[player] >= max_score) {
          break;
        };
        safe := safe_score[player];
        " Sticking with {$safe}\n"->PrintLine();
        score := 0;
        player := (player + 1) % player_count;
      };
    };
    safe := safe_score[player];
    "\n\nPlayer {$player} wins with a score of {$safe}"->PrintLine();
  }
}

```


```txt

 Player 0: (0, 0) Rolling? (y/n)
y
 Rolled 3
 Player 0: (0, 3) Rolling? (y/n)
y
 Rolled 2
 Player 0: (0, 5) Rolling? (y/n)
...
 Rolled 4
 Player 1: (56, 30) Rolling? (y/n)
n
 Sticking with 86

 Player 0: (94, 0) Rolling? (y/n)
y
 Rolled 6
 Player 0: (94, 6) Rolling? (y/n)
n
...
 Rolled 4
 Player 1: (56, 30) Rolling? (y/n)
n
 Sticking with 86

 Player 0: (94, 0) Rolling? (y/n)
y
 Rolled 6
 Player 0: (94, 6) Rolling? (y/n)
n

```



## OCaml



```ocaml
class player (name_init : string) =
  object
    val name = name_init
    val mutable total = 0
    val mutable turn_score = 0
    method get_name = name
    method get_score = total
    method end_turn = total <- total + turn_score;
                      turn_score <- 0;
    method has_won = total >= 100;
    method rolled roll = match roll with
                          1 -> turn_score <- 0;
                         |_ -> turn_score <- turn_score + roll;
  end;;

let print_seperator () =
  print_endline "#####";;

let rec one_turn p1 p2 =
  Printf.printf "What do you want to do %s?\n" p1#get_name;
  print_endline "  1)Roll the dice?";
  print_endline "  2)Or end your turn?";
  let choice = read_int () in
  if choice = 1 then
  begin
    let roll = 1 + Random.int 6 in
      Printf.printf "Rolled a %d\n" roll;
      p1#rolled roll;
      match roll with
        1 -> print_seperator ();
             one_turn p2 p1
       |_ -> one_turn p1 p2
  end
  else if choice = 2 then
  begin
    p1#end_turn;
    match p1#has_won with
     false -> Printf.printf "%s's score is now %d\n" p1#get_name p1#get_score;
               print_seperator();
               one_turn p2 p1;
     |true -> Printf.printf "Congratulations %s! You've won\n" p1#get_name
  end
  else
  begin
    print_endline "That's not a choice! Make a real one!";
    one_turn p1 p2
  end;;

Random.self_init ();
let p1 = new player "Steven"
and p2 = new player "John" in
one_turn p1 p2;;
```



## Pascal

```pascal
program Pig;

const
	WinningScore = 100;

type
	DieRoll = 1..6;
	Score = integer;
	Player = record
		Name: string;
		Points: score;
		Victory: Boolean
	end;

{ Assume a 2-player game. }
var Player1, Player2: Player;

function RollTheDie: DieRoll;
  { Return a random number 1 thru 6. }
	begin
		RollTheDie := random(6) + 1
	end;

procedure TakeTurn (var P: Player);
  { Play a round of Pig. }
	var
		Answer: char;
		Roll: DieRoll;
		NewPoints: Score;
		KeepPlaying: Boolean;
	begin
		NewPoints := 0;
		writeln ;
		writeln('It''s your turn, ', P.Name, '!');
		writeln('So far, you have ', P.Points, ' points in all.');
		writeln ;
		{ Keep playing until the user rolls a 1 or chooses not to roll. }
		write('Do you want to roll the die (y/n)? ');
		readln(Answer);
		KeepPlaying := upcase(Answer) = 'Y';
		while KeepPlaying do
		 begin
			Roll := RollTheDie;
			if Roll = 1 then
			 begin
				NewPoints := 0;
				KeepPlaying := false;
				writeln('Oh no! You rolled a 1! No new points after all.')
			 end
			else
			 begin
				NewPoints := NewPoints + Roll;
				write('You rolled a ', Roll:1, '. ');
				writeln('That makes ', NewPoints, ' new points so far.');
				writeln ;
				write('Roll again (y/n)? ');
				readln(Answer);
				KeepPlaying := upcase(Answer) = 'Y'
			 end
		 end;
		{ Update the player's score and check for a winner. }
		writeln ;
		if NewPoints = 0 then
			writeln(P.Name, ' still has ', P.Points, ' points.')
		else
		 begin
			P.Points := P.Points + NewPoints;
			writeln(P.Name, ' now has ', P.Points, ' points total.');
			P.Victory := P.Points >= WinningScore
		 end
	end;

procedure Congratulate(Winner: Player);
	begin
		writeln ;
		write('Congratulations, ', Winner.Name, '! ');
		writeln('You won with ', Winner.Points, ' points.');
		writeln
	end;

begin
	{ Greet the players and initialize their data. }
	writeln('Let''s play Pig!');

	writeln ;
	write('Player 1, what is your name? ');
	readln(Player1.Name);
	Player1.Points := 0;
	Player1.Victory := false;

	writeln ;
	write('Player 2, what is your name? ');
	readln(Player2.Name);
	Player2.Points := 0;
	Player2.Victory := false;

	{ Take turns until there is a winner. }
	randomize;
	repeat
		TakeTurn(Player1);
		if not Player1.Victory then TakeTurn(Player2)
	until Player1.Victory or Player2.Victory;

	{ Announce the winner. }
	if Player1.Victory then
		Congratulate(Player1)
	else
		Congratulate(Player2)
end.
```



## Perl

You can have as many players as you want, simply provide their names on the command line.

```perl
#!perl
use strict;
use warnings;
my @players = @ARGV;
@players = qw(Joe Mike);
my @scores = (0) x @players;
while( 1 ) {
	PLAYER: for my $i ( 0 .. $#players ) {
		my $name = $players[$i];
		my $score = $scores[$i];
		my $roundscore = 1 + int rand 6;
		print "$name, your score so far is $score.\n";
		print "You rolled a $roundscore.\n";
		next PLAYER if $roundscore == 1;
		while($score + $roundscore < 100) {
			print "Roll again, or hold [r/h]: ";
			my $answer = <>;
			$answer = 'h' unless defined $answer;
			if( $answer =~ /^h/i ) {
				$score += $roundscore;
				$scores[$i] = $score;
				print "Your score is now $score.\n";
				next PLAYER;
			} elsif( $answer =~ /^r/ ) {
				my $die = 1 + int rand 6;
				print "$name, you rolled a $die.\n";
				next PLAYER if $die == 1;
				$roundscore += $die;
				print "Your score for the round is now $roundscore.\n";
			} else {
				print "I did not understand that.\n";
			}
		}
		$score += $roundscore;
		print "With that, your score became $score.\n";
		print "You won!\n";
		exit;
	}
}
__END__

```


## Perl 6

```perl6
constant DIE = 1..6;

sub MAIN (Int :$players = 2, Int :$goal = 100) {
    my @safe = 0 xx $players;
    for |^$players xx * -> $player {
	say "\nOK, player #$player is up now.";
	my $safe = @safe[$player];
	my $ante = 0;
	until $safe + $ante >= $goal or
	    prompt("#$player, you have $safe + $ante = {$safe+$ante}. Roll? [Yn] ") ~~ /:i ^n/
	{
	    given DIE.roll {
		say "  You rolled a $_.";
		when 1 {
		    say "  Bust!  You lose $ante but keep your previous $safe.";
		    $ante = 0;
		    last;
		}
		when 2..* {
		    $ante += $_;
		}
	    }
	}
	$safe += $ante;
	if $safe >= $goal {
	    say "\nPlayer #$player wins with a score of $safe!";
	    last;
	}
	@safe[$player] = $safe;
	say "  Sticking with $safe." if $ante;
    }
}
```

The game defaults to the specified task, but we'll play a shorter game with three players for our example:
```txt
> pig help
Usage:
  pig [--players=<Int>] [--goal=<Int>]
> pig --players=3 --goal=20

OK, player #0 is up now.
#0, you have 0 + 0 = 0. Roll? [Yn]
  You rolled a 6.
#0, you have 0 + 6 = 6. Roll? [Yn]
  You rolled a 6.
#0, you have 0 + 12 = 12. Roll? [Yn] n
  Sticking with 12.

OK, player #1 is up now.
#1, you have 0 + 0 = 0. Roll? [Yn]
  You rolled a 4.
#1, you have 0 + 4 = 4. Roll? [Yn]
  You rolled a 6.
#1, you have 0 + 10 = 10. Roll? [Yn]
  You rolled a 6.
#1, you have 0 + 16 = 16. Roll? [Yn] n
  Sticking with 16.

OK, player #2 is up now.
#2, you have 0 + 0 = 0. Roll? [Yn]
  You rolled a 5.
#2, you have 0 + 5 = 5. Roll? [Yn]
  You rolled a 1.
  Bust!  You lose 5 but keep your previous 0.

OK, player #0 is up now.
#0, you have 12 + 0 = 12. Roll? [Yn]
  You rolled a 1.
  Bust!  You lose 0 but keep your previous 12.

OK, player #1 is up now.
#1, you have 16 + 0 = 16. Roll? [Yn] n

OK, player #2 is up now.
#2, you have 0 + 0 = 0. Roll? [Yn]
  You rolled a 6.
#2, you have 0 + 6 = 6. Roll? [Yn]
  You rolled a 6.
#2, you have 0 + 12 = 12. Roll? [Yn]
  You rolled a 4.
#2, you have 0 + 16 = 16. Roll? [Yn]
  You rolled a 6.

Player #2 wins with a score of 22!
```



## Phix

Initially a translation of [[Pig_the_dice_game#Lua|Lua]], but now quite different.

```Phix
constant numPlayers = 2,
         maxScore = 100
sequence scores = repeat(0,numPlayers)
printf(1,"\nPig The Dice Game\n\n")
integer points = 0, -- points accumulated in current turn, 0=swap turn
        player = 1  -- start with first player
while true do
    integer roll = rand(6)
    printf(1,"Player %d, your score is %d, you rolled %d. ",{player,scores[player],roll})
    if roll=1 then
        printf(1," Too bad. :(\n")
        points = 0 -- swap turn
    else
        points += roll
        if scores[player]+points>=maxScore then exit end if
        printf(1,"Round score %d. Roll or Hold?",{points})
        integer reply = upper(wait_key())
        printf(1,"%c\n",{reply})
        if reply == 'H' then
            scores[player] += points
            points = 0 -- swap turn
        end if
    end if
    if points=0 then
        player = mod(player,numPlayers) + 1
    end if
end while
printf(1,"\nPlayer %d wins with a score of %d!\n",{player,scores[player]+points})
```

```txt

Pig The Dice Game

Player 1, your score is 0, you rolled 2. Round score 2. Roll or Hold?R
Player 1, your score is 0, you rolled 5. Round score 7. Roll or Hold?R
Player 1, your score is 0, you rolled 3. Round score 10. Roll or Hold?R
Player 1, your score is 0, you rolled 4. Round score 14. Roll or Hold?H
Player 2, your score is 0, you rolled 5. Round score 5. Roll or Hold?R
Player 2, your score is 0, you rolled 1.  Too bad. :(
Player 1, your score is 14, you rolled 3. Round score 3. Roll or Hold?H
...
Player 2, your score is 86, you rolled 3. Round score 9. Roll or Hold?R
Player 2, your score is 86, you rolled 6.
Player 2 wins with a score of 101!

```



## PHP

```php
error_reporting(E_ALL & ~ ( E_NOTICE | E_WARNING ));

define('MAXSCORE', 100);
define('PLAYERCOUNT', 2);

$confirm = array('Y', 'y', '');

while (true) {
    printf(' Player %d: (%d, %d) Rolling? (Yn) ', $player,
            $safeScore[$player], $score);
    if ($safeScore[$player] + $score < MAXSCORE &&
            in_array(trim(fgets(STDIN)), $confirm)) {
        $rolled = rand(1, 6);
        echo " Rolled $rolled \n";
        if ($rolled == 1) {
            printf(' Bust! You lose %d but keep %d \n\n',
                    $score, $safeScore[$player]);
        } else {
            $score += $rolled;
            continue;
        }
    } else {
        $safeScore[$player] += $score;
        if ($safeScore[$player] >= MAXSCORE)
            break;
        echo ' Sticking with ', $safeScore[$player], '\n\n';
    }
    $score = 0;
    $player = ($player + 1) % PLAYERCOUNT;
}
printf('\n\nPlayer %d wins with a score of %d ',
    $player, $safeScore[$player]);

```


```txt
C:\UniServer\usr\local\php\php pig.php
 Player 0: (0, 0) Rolling? (Yn)
 Rolled 2
 Player 0: (0, 2) Rolling? (Yn)
 Rolled 1
 Bust! You lose 2 but keep 0

 Player 1: (0, 0) Rolling? (Yn)
 Rolled 2
 Player 1: (0, 2) Rolling? (Yn)
 Rolled 1
 Bust! You lose 2 but keep 0

 Player 0: (0, 0) Rolling? (Yn)
 Rolled 3
 Player 0: (0, 3) Rolling? (Yn)
 Rolled 6
 Player 0: (0, 9) Rolling? (Yn) n
 sticking with 9

 Player 1: (0, 0) Rolling? (Yn)
 Rolled 5
 Player 1: (0, 5) Rolling? (Yn)
 Rolled 3
 Player 1: (0, 8) Rolling? (Yn) n
 sticking with 8

 (...)

 Player 1: (93, 0) Rolling? (Yn)
 Rolled 5
 Player 1: (93, 5) Rolling? (Yn)
 Rolled 3
 Player 1: (93, 8) Rolling? (Yn)

Player 1 wins with a score of 101
```



## Python


```python
#!/usr/bin/python3

'''
See: http://en.wikipedia.org/wiki/Pig_(dice)

This program scores and throws the dice for a two player game of Pig

'''

from random import randint

playercount = 2
maxscore = 100
safescore = [0] * playercount
player = 0
score=0

while max(safescore) < maxscore:
    rolling = input("Player %i: (%i, %i) Rolling? (Y) "
                    % (player, safescore[player], score)).strip().lower() in {'yes', 'y', ''}
    if rolling:
        rolled = randint(1, 6)
        print('  Rolled %i' % rolled)
        if rolled == 1:
            print('  Bust! you lose %i but still keep your previous %i'
                  % (score, safescore[player]))
            score, player = 0, (player + 1) % playercount
        else:
            score += rolled
    else:
        safescore[player] += score
        if safescore[player] >= maxscore:
            break
        print('  Sticking with %i' % safescore[player])
        score, player = 0, (player + 1) % playercount

print('\nPlayer %i wins with a score of %i' %(player, safescore[player]))
```


;Samples from a game:

```txt
Player 0: (0, 0) Rolling? (Y)
  Rolled 6
Player 0: (0, 6) Rolling? (Y)
  Rolled 5
Player 0: (0, 11) Rolling? (Y)
  Rolled 1
  Bust! you lose 11 but still keep your previous 0
Player 1: (0, 0) Rolling? (Y)
  Rolled 3
Player 1: (0, 3) Rolling? (Y)
  Rolled 4
Player 1: (0, 7) Rolling? (Y)
  Rolled 6
Player 1: (0, 13) Rolling? (Y) n
  Sticking with 13
...
Player 0: (78, 10) Rolling? (Y) n
  Sticking with 88
Player 1: (63, 0) Rolling? (Y)
  Rolled 6
Player 1: (63, 6) Rolling? (Y)
  Rolled 1
  Bust! you lose 6 but still keep your previous 63
Player 0: (88, 0) Rolling? (Y) n
  Sticking with 88
Player 1: (63, 0) Rolling? (Y) n
  Sticking with 63
Player 0: (88, 0) Rolling? (Y)
  Rolled 6
Player 0: (88, 6) Rolling? (Y)
  Rolled 4
Player 0: (88, 10) Rolling? (Y)
  Rolled 3
Player 0: (88, 13) Rolling? (Y) n

Player 0 wins with a score of 101
```



## Racket


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

(pig-the-dice #:print? #t human human)

```



## REXX

This REXX program has the following features:
:::* any number of human players can play
:::* any number of computer players can play
:::* human and computers can play together (humans always go first)
:::* names of the human players can be specified
:::* names of the computer players can be specified
:::* the score needed to win may be specified
:::* verbosity was chosen as it's assumed that a human is playing
:::* code was written to allow for an   '''N'''   sided die
:::* names are used for the die faces (in addition to the pip value)
:::* a simple (but aggressive) strategy is used (that favors a human player)

```rexx
/*REXX program plays "pig the dice game"  (any number of CBLFs and/or silicons or HALs).*/
sw=linesize() - 1                                /*get the width of the terminal screen,*/
parse arg  hp  cp  win  die  _  .  '(' names ")" /*obtain optional arguments from the CL*/
                                                 /*names with blanks should use an  _   */
if _\==''  then  call  err  'too many arguments were specified: ' _
@nhp  = 'number of human players'   ;       hp =scrutinize( hp, @nhp , 0,  0,   0)
@ncp  = 'number of computer players';       cp =scrutinize( cp, @ncp , 0,  0,   2)
@sn2w = 'score needed to win'       ;       win=scrutinize(win, @sn2w, 1, 1e6, 60)
@nsid = 'number of sides in die'    ;       die=scrutinize(die, @nsid, 2, 999,  6)
if hp==0  &  cp==0   then cp=2                   /*if both counts are zero, two HALs.   */
if hp==1  &  cp==0   then cp=1                   /*if one human, then use   one HAL.    */
name.=                                           /*nullify all names  (to a blank).     */
L=0                                              /*maximum length of a player name.     */
       do i=1  for hp+cp                         /*get the player's names,  ...  maybe. */
       if i>hp  then @= 'HAL_'i"_the_computer"   /*use this for default name.           */
                else @= 'player_'i               /* "    "   "     "      "             */
       name.i = translate( word( strip( word(names,i)) @, 1),,'_')
       L=max(L, length(name.i))                  /*use   L   for nice name formatting.  */
       end   /*i*/                               /*underscores are changed ──► blanks.  */

hpn=hp;  if hpn==0   then hpn='no'               /*use normal English for the display.  */
cpn=cp;  if cpn==0   then cpn="no"               /* "     "      "     "   "     "      */

say 'Pig (the dice game) is being played with:'  /*the introduction to pig-the-dice-game*/

         if cpn\==0  then  say  right(cpn,9)  'computer player's(cp)
         if hpn\==0  then  say  right(hpn,9)  'human player's(hp)
!.=
say 'and the'         @sn2w         "is: "         win         '   (or greater).'
dieNames= 'ace deuce trey square nickle boxcar'  /*some slangy vernacular die─face names*/
                                                 /*note:  snake eyes is for two aces.   */
!w=0;          do i=1  for die                   /*assign the vernacular die─face names.*/
               !.i=' ['word(dieNames,i)"]"       /*pick a word from die─face name lists.*/
               !w=max(!w, length(!.i))           /*!w ──► maximum length die─face name. */
               end   /*i*/
s.=0                                             /*set all player's scores to zero.     */
!w=!w+length(die)+3                              /*pad the die number and die names.    */
@=copies('─',9)                                  /*eyecatcher (for the prompting text). */
@jra= 'just rolled a '                           /*a nice literal to have laying 'round.*/
@ati= 'and the inning'                           /*"   "     "     "   "     "      "   */

      /*════════════════════════════════════════════════════════════let's play some pig.*/
   do game=1;     in.=0                          /*set each inning's score to zero.     */
   call score                                    /*display the current score.           */

     do j=1  for hp+cp                           /*let each player roll their dice.     */
     say;        say copies('─', sw)             /*display a fence for da ole eyeballs. */
     it=name.j
     say it',  your total score (so far) in this pig game is: '        s.j"."

       do  until  stopped                        /*keep prompting/rolling 'til stopped. */
       r=random(1, die)                          /*get a random die face (number).      */
       !=left(space(r !.r','),  !w)              /*for color, use a die─face name.      */
       in.j=in.j + r                             /*add die─face number to the inning.   */

       if r==1  then  do;  say it @jra ! || @ati "is a bust.";  leave;   end
                           say it @jra ! || @ati "total is: "    in.j

       stopped=what2do(j)                        /*determine or ask  to stop rolling.   */
       if j>hp & stopped  then say ' and'      name.j      "elected to stop rolling."
       end   /*until stopped*/

     if r\==1     then s.j=s.j + in.j            /*if not a bust, then add to the inning*/
     if s.j>=win  then leave game                /*we have a winner,  so the game ends. */
     end     /*j*/                               /*that's the end of the players.       */
   end       /*game*/

call score
say; say; say; say;  say center(''name.j "won! ",sw,'═');  say; say; exit
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s: if arg(1)==1  then return arg(3);           return word(arg(2) 's',1)   /*pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
score:  say;           say copies('█', sw)       /*display a fence for da ole eyeballs. */
          do k=1  for hp+cp                      /*display the scores  (as a recap).    */
          say 'The score for '   left(name.k, L)    " is "     right(s.k, length(win) )'.'
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
         say;   say;   say center(' quitting. ',sw,'─');   say;   say;           exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:     say;   say;   say center(' error! ', max(40, linesize() % 2), "*");     say
                        do j=1  for arg();    say arg(j);    say;    end;   say;   exit 13
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


[This plays a simulated game with '''no''' humans, '''two''' computers, and the score to win is '''44''', player names are specified.]
<pre style="height:104ex">
Pig (the dice game) is being played with:
        2 computer players
       no human players
and the score needed to win is:  44    (or greater).

███████████████████████████████████████████████████████████████████████████████████████████████████████████
The score for  HAL   is   0.
The score for  RdD2  is   0.
███████████████████████████████████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────────────────────────────────
HAL,  your total score (so far) in this pig game is:  0.
HAL just rolled a  4 [square],  and the inning total is:  4
HAL just rolled a  5 [nickle],  and the inning total is:  9
HAL just rolled a  3 [trey],    and the inning total is:  12
 and HAL elected to stop rolling.

───────────────────────────────────────────────────────────────────────────────────────────────────────────
RdD2,  your total score (so far) in this pig game is:  0.
RdD2 just rolled a  4 [square],  and the inning total is:  4
RdD2 just rolled a  4 [square],  and the inning total is:  8
RdD2 just rolled a  4 [square],  and the inning total is:  12
 and RdD2 elected to stop rolling.

███████████████████████████████████████████████████████████████████████████████████████████████████████████
The score for  HAL   is  12.
The score for  RdD2  is  12.
███████████████████████████████████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────────────────────────────────
HAL,  your total score (so far) in this pig game is:  12.
HAL just rolled a  2 [deuce],   and the inning total is:  2
HAL just rolled a  1 [ace],     and the inning is a bust.

───────────────────────────────────────────────────────────────────────────────────────────────────────────
RdD2,  your total score (so far) in this pig game is:  12.
RdD2 just rolled a  2 [deuce],   and the inning total is:  2
RdD2 just rolled a  1 [ace],     and the inning is a bust.

███████████████████████████████████████████████████████████████████████████████████████████████████████████
The score for  HAL   is  12.
The score for  RdD2  is  12.
███████████████████████████████████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────────────────────────────────
HAL,  your total score (so far) in this pig game is:  12.
HAL just rolled a  6 [boxcar],  and the inning total is:  6
HAL just rolled a  5 [nickle],  and the inning total is:  11
 and HAL elected to stop rolling.

───────────────────────────────────────────────────────────────────────────────────────────────────────────
RdD2,  your total score (so far) in this pig game is:  12.
RdD2 just rolled a  2 [deuce],   and the inning total is:  2
RdD2 just rolled a  6 [boxcar],  and the inning total is:  8
RdD2 just rolled a  3 [trey],    and the inning total is:  11
 and RdD2 elected to stop rolling.

███████████████████████████████████████████████████████████████████████████████████████████████████████████
The score for  HAL   is  23.
The score for  RdD2  is  23.
███████████████████████████████████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────────────────────────────────
HAL,  your total score (so far) in this pig game is:  23.
HAL just rolled a  1 [ace],     and the inning is a bust.

───────────────────────────────────────────────────────────────────────────────────────────────────────────
RdD2,  your total score (so far) in this pig game is:  23.
RdD2 just rolled a  6 [boxcar],  and the inning total is:  6
RdD2 just rolled a  5 [nickle],  and the inning total is:  11
 and RdD2 elected to stop rolling.

███████████████████████████████████████████████████████████████████████████████████████████████████████████
The score for  HAL   is  23.
The score for  RdD2  is  34.
███████████████████████████████████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────────────────────────────────
HAL,  your total score (so far) in this pig game is:  23.
HAL just rolled a  6 [boxcar],  and the inning total is:  6
HAL just rolled a  1 [ace],     and the inning is a bust.

───────────────────────────────────────────────────────────────────────────────────────────────────────────
RdD2,  your total score (so far) in this pig game is:  34.
RdD2 just rolled a  1 [ace],     and the inning is a bust.

███████████████████████████████████████████████████████████████████████████████████████████████████████████
The score for  HAL   is  23.
The score for  RdD2  is  34.
███████████████████████████████████████████████████████████████████████████████████████████████████████████

───────────────────────────────────────────────────────────────────────────────────────────────────────────
HAL,  your total score (so far) in this pig game is:  23.
HAL just rolled a  6 [boxcar],  and the inning total is:  6
HAL just rolled a  6 [boxcar],  and the inning total is:  12
 and HAL elected to stop rolling.

───────────────────────────────────────────────────────────────────────────────────────────────────────────
RdD2,  your total score (so far) in this pig game is:  34.
RdD2 just rolled a  6 [boxcar],  and the inning total is:  6
RdD2 just rolled a  2 [deuce],   and the inning total is:  8
RdD2 just rolled a  2 [deuce],   and the inning total is:  10
 and RdD2 elected to stop rolling.

███████████████████████████████████████████████████████████████████████████████████████████████████████████
The score for  HAL   is  35.
The score for  RdD2  is  44.
███████████████████████████████████████████████████████████████████████████████████████████████████████████



════════════════════════════════════════════════RdD2 won! ═════════════════════════════════════════════════

```



## Ring


```ring

# Project : Pig the dice game

numPlayers = 2
maxScore = 100
safescore = list(numPlayers)

while true
         rolling = ""
         for player = 1 to numPlayers
              score = 0
              while safeScore[player] < maxScore
                       see "Player " + player + " Rolling? (Y) "
                       give rolling
                       if upper(rolling) = "Y"
                          rolled = random(5)  + 1
                          see "Player " + player + " rolled " + rolled + nl
                          if rolled = 1
                             see "Bust! you lose player " + player + " but still keep your previous score of " + safeScore[player] + nl
                             exit
                          ok
                          score = score + rolled
                       else
                          safeScore[player] = safeScore[player] + score
                      ok
              end
         next
end
see "Player " + player + " wins with a score of " + safeScore[player]

```

Output:

```txt

Player 1 Rolling? (Y) y
Player 1 rolled 6
Player 1 Rolling? (Y) y
Player 1 rolled 5
Player 1 Rolling? (Y) y
Player 1 rolled 6
Player 1 Rolling? (Y) y
Player 1 rolled 4
Player 1 Rolling? (Y) y
Player 1 rolled 3
Player 1 Rolling? (Y) y
Player 1 rolled 3
Player 1 Rolling? (Y) y
Player 1 rolled 3
Player 1 Rolling? (Y) y
Player 1 rolled 3
Player 1 Rolling? (Y) y
Player 1 rolled 3
Player 1 Rolling? (Y) y
Player 1 rolled 2
Player 1 Rolling? (Y) y
Player 1 rolled 6
Player 1 Rolling? (Y) y
Player 1 rolled 2
Player 1 Rolling? (Y) y
Player 1 rolled 4
Player 1 Rolling? (Y) y
Player 1 rolled 5
Player 1 Rolling? (Y) y
Player 1 rolled 4
Player 1 Rolling? (Y) y
Player 1 rolled 1
Bust! you lose player 1 but still keep your previous score of 0
Player 2 Rolling? (Y)

```



## Ruby

Features:
* arbitrary number of players, max score, and die sides
* players with names
* transparent game logic


```ruby
class PigGame
  Player = Struct.new(:name, :safescore, :score) do
    def bust!() self.score = safescore end
    def stay!() self.safescore = score end
    def to_s() "#{name} (#{safescore}, #{score})" end
  end

  def initialize(names, maxscore=100, die_sides=6)
    rotation = names.map {|name| Player.new(name,0,0) }

    rotation.cycle do |player|
      loop do
        if wants_to_roll?(player)
          puts "Rolled: #{roll=roll_dice(die_sides)}"
          if bust?(roll)
            puts "Busted!",''
            player.bust!
            break
          else
            player.score += roll
            if player.score >= maxscore
              puts player.name + " wins!"
              return
            end
          end
        else
          player.stay!
          puts "Staying with #{player.safescore}!", ''
          break
        end
      end
    end
  end

  def roll_dice(die_sides) rand(1..die_sides) end
  def bust?(roll) roll==1 end
  def wants_to_roll?(player)
    print "#{player}: Roll? (Y) "
    ['Y','y',''].include?(gets.chomp)
  end
end

PigGame.new( %w|Samuel Elizabeth| )
```


;Samples from a game:
<pre style="height:25ex;overflow:scroll">Samuel (0, 0): Roll? (Y)
Rolled: 2
Samuel (0, 2): Roll? (Y)
Rolled: 4
Samuel (0, 6): Roll? (Y)
Rolled: 1
Busted!

Elizabeth (0, 0): Roll? (Y)
Rolled: 3
Elizabeth (0, 3): Roll? (Y)
Rolled: 5
Elizabeth (0, 8): Roll? (Y)
Rolled: 2
Elizabeth (0, 10): Roll? (Y) n
Staying with 10!

Samuel (0, 0): Roll? (Y)
Rolled: 4
Samuel (0, 4): Roll? (Y)
Rolled: 1
Busted!

Elizabeth (10, 10): Roll? (Y)
Rolled: 2
Elizabeth (10, 12): Roll? (Y)
Rolled: 6
Elizabeth (10, 18): Roll? (Y) n
Staying with 18!
...
Elizabeth (83, 97): Roll? (Y)
Rolled: 5
Elizabeth wins!
```



## Run BASIC


```runbasic
numPlayers	= 2
maxScore 	= 100
dim	safeScore(numPlayers)

[loop]
for player = 1 to numPlayers
  score     = 0

  while safeScore(player) < maxScore
   input "Player ";player;" Rolling? (Y) ";rolling$
    if upper$(rolling$) = "Y" then
        rolled = int(rnd(0) * 5) + 1
        print "Player ";player;" rolled ";rolled
        if rolled = 1 then
            print "Bust! you lose player ";player;" but still keep your previous score of ";safeScore(plater)
            exit while
        end if
        score = score + rolled
    else
        safeScore(player) = safeScore(player) + score
    end if
  wend
next player
goto [loop]
[winner]
print "Player ";plater;" wins with a score of ";safeScore(player)
```



## Scala

===Functional Style, Tail recursive===
```Scala
object PigDice extends App {
  private val (maxScore, nPlayers) = (100, 2)
  private val rnd = util.Random

  private case class Game(gameOver: Boolean, idPlayer: Int, score: Int, stickedScores: Vector[Int])

  @scala.annotation.tailrec
  private def loop(play: Game): Unit =
    play match {
      case Game(true, _, _, _) =>
      case Game(false, gPlayer, gScore, gStickedVals) =>
        val safe = gStickedVals(gPlayer)
        val stickScore = safe + gScore
        val gameOver = stickScore >= maxScore

        def nextPlayer = (gPlayer + 1) % nPlayers

        def gamble: Game = play match {
          case Game(_: Boolean, lPlayer: Int, lScore: Int, lStickedVals: Vector[Int]) =>
            val rolled: Int = rnd.nextInt(6) + 1

            println(s" Rolled $rolled")
            if (rolled == 1) {
              println(s" Bust! You lose $lScore but keep ${lStickedVals(lPlayer)}\n")
              play.copy(idPlayer = nextPlayer, score = 0)
            } else play.copy(score = lScore + rolled)
        }

        def stand: Game = play match {
          case Game(_, lPlayer, _, lStickedVals) =>

            println(
              (if (gameOver) s"\n\nPlayer $lPlayer wins with a score of" else " Sticking with")
                + s" $stickScore.\n")

            Game(gameOver, nextPlayer, 0, lStickedVals.updated(lPlayer, stickScore))
        }

        if (!gameOver && Seq("y", "").contains(
            io.StdIn.readLine(f" Player $gPlayer%d: ($safe%d, $gScore%d) Rolling? ([y]/n): ").toLowerCase)
        ) loop(gamble )else loop(stand)
    }

  loop(Game(gameOver = false, 0, 0, Array.ofDim[Int](nPlayers).toVector))
}
```



## Tcl

<table><tr><td>{{works with|Tcl|8.6}}</td><td>or alternatively with Tcl 8.5 and</td><td>{{libheader|TclOO}}</td></tr></table><!-- dirty trick! -->

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
    method stuck {score} {}
    method busted {score} {}
    method won {score} {}

    method rolled {who what} {
	if {$who ne [self]} {
	    #puts "[$who name] rolled a $what"
	}
    }
    method turnend {who score} {
	if {$who ne [self]} {
	    puts "End of turn for [$who name] on $score"
	}
    }
    method winner {who score} {
	if {$who ne [self]} {
	    puts "[$who name] is a winner, on $score"
	}
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

Demonstrating with human players:

```tcl
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

pig [HumanPlayer new "Alex"] [HumanPlayer new "Bert"]
```

```txt

Alex (on 0+0) do you want to roll? (Y/n)y
Busted! (Alex still on score 0)
End of turn for Alex on 0
Bert (on 0+0) do you want to roll? (Y/n)
Bert (on 0+2) do you want to roll? (Y/n)
Bert (on 0+5) do you want to roll? (Y/n)
Bert (on 0+11) do you want to roll? (Y/n)
Bert (on 0+13) do you want to roll? (Y/n)
Bert (on 0+15) do you want to roll? (Y/n)
Bert (on 0+18) do you want to roll? (Y/n)
Bert (on 0+22) do you want to roll? (Y/n)n
Bert sticks with score 22
End of turn for Bert on 22
...
Bert (on 87+0) do you want to roll? (Y/n)
Bert (on 87+5) do you want to roll? (Y/n)
Bert (on 87+8) do you want to roll? (Y/n)
Bert has won! (Score: 101)
Bert is a winner, on 101
```



## VBA



```vb

Option Explicit

Sub Main_Pig()
Dim Scs() As Byte, Ask As Integer, Np As Boolean, Go As Boolean
Dim Cp As Byte, Rd As Byte, NbP As Byte, ScBT As Byte
    'You can adapt these Const, but don't touch the "¤¤¤¤"
    Const INPTXT As String = "Enter number of players : "
    Const INPTITL As String = "Numeric only"
    Const ROL As String = "Player ¤¤¤¤ rolls the die."
    Const MSG As String = "Do you want to ""hold"" : "
    Const TITL As String = "Total if you keep : "
    Const RES As String = "The die give you : ¤¤¤¤ points."
    Const ONE As String = "The die give you : 1 point. Sorry!" & vbCrLf & "Next player."
    Const WIN As String = "Player ¤¤¤¤ win the Pig Dice Game!"
    Const STW As Byte = 100

    Randomize Timer
    NbP = Application.InputBox(INPTXT, INPTITL, 2, Type:=1)
    ReDim Scs(1 To NbP)
    Cp = 1
    Do
        ScBT = 0
        Do
            MsgBox Replace(ROL, "¤¤¤¤", Cp)
            Rd = Int((Rnd * 6) + 1)
            If Rd > 1 Then
                MsgBox Replace(RES, "¤¤¤¤", Rd)
                ScBT = ScBT + Rd
                If Scs(Cp) + ScBT >= STW Then
                    Go = True
                    Exit Do
                End If
                Ask = MsgBox(MSG & ScBT, vbYesNo, TITL & Scs(Cp) + ScBT)
                If Ask = vbYes Then
                    Scs(Cp) = Scs(Cp) + ScBT
                    Np = True
                End If
            Else
                MsgBox ONE
                Np = True
            End If
        Loop Until Np
        If Not Go Then
            Np = False
            Cp = Cp + 1
            If Cp > NbP Then Cp = 1
        End If
    Loop Until Go
    MsgBox Replace(WIN, "¤¤¤¤", Cp)
End Sub

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
integer Player, Die, Points, Score(2);
[Score(0):= 0;  Score(1):= 0;   \starting scores for each player
Player:= 1;                     \second player
repeat  Player:= if Player = 1 then 0 else 1;           \next player
        Points:= 0;                                     \points for current turn
        loop    [Text(0, "Player ");  IntOut(0, Player+1);
                Text(0, " is up. Roll or hold (r/h)? ");
                OpenI(0);       \discard any chars in keyboard buffer (like CR)
                if ChIn(0) = ^h then quit               \default is 'r' to roll
                else    [Die:= Ran(6)+1;                \roll the die
                        Text(0, "You get ");  IntOut(0, Die);  CrLf(0);
                        if Die = 1 then [Points:= 0;  quit];
                        Points:= Points + Die;          \add up points for turn
                        Text(0, "Total points are ");  IntOut(0, Points);
                        Text(0, " for a tentative score of ");
                        IntOut(0, Score(Player)+Points);  CrLf(0);
                        ];
                ];
        Score(Player):= Score(Player) + Points;         \show scores
        Text(0, "Player 1 has ");  IntOut(0, Score(0));
        Text(0, " and player 2 has ");  IntOut(0, Score(1));  CrLf(0);
until   Score(Player) >= 100;
Text(0, "Player ");  IntOut(0, Player+1);  Text(0, " WINS!!!");
]
```


Output:

```txt

Player 1 is up. Roll or hold (r/h)? r
You get 5
Total points are 5 for a tentative score of 5
Player 1 is up. Roll or hold (r/h)? r
You get 6
Total points are 11 for a tentative score of 11
Player 1 is up. Roll or hold (r/h)? r
You get 6
Total points are 17 for a tentative score of 17
Player 1 is up. Roll or hold (r/h)? r
You get 6
Total points are 23 for a tentative score of 23
Player 1 is up. Roll or hold (r/h)? h
Player 1 has 23 and player 2 has 0
Player 2 is up. Roll or hold (r/h)? r
You get 2
Total points are 2 for a tentative score of 2
Player 2 is up. Roll or hold (r/h)? r
You get 6
Total points are 8 for a tentative score of 8
Player 2 is up. Roll or hold (r/h)? r
You get 1
Player 1 has 23 and player 2 has 0
...
Player 1 has 94 and player 2 has 57
Player 1 is up. Roll or hold (r/h)? r
You get 6
Total points are 6 for a tentative score of 100
Player 1 is up. Roll or hold (r/h)? h
Player 1 has 100 and player 2 has 57
Player 1 WINS!!!

```



## zkl

```zkl
const WIN=100, PLAYERS=2;
players,safeScores:=Walker.cycle([0..PLAYERS-1]), PLAYERS.pump(List(),0);
rollDie:=(1).random.fp(7);
yes,player,score,S:=T("","y"),players.next(),0,0;
tally:='wrap(player,score){ w:=safeScores[player]+=score; (w>=WIN) };

while(True){
   print("Player %d: (%d, %d). Rolling? (y/n) ".fmt(player+1,S,score));
   if(yes.holds(ask().strip().toLower())){
      rolled:=rollDie(); println(" Rolled a %d".fmt(rolled));
      if(rolled==1){
	 println(" Bust! You lose %d but keep %d\n".fmt(score,S));
      }else{
	 score+=rolled;
	 if(score + S>=WIN){ tally(player,score); break; }
	 continue;
      }
   }else{
      if(tally(player,score)) break;
      println(" Sticking with %d\n".fmt(safeScores[player]));
   }
   player,score,S=players.next(),0,safeScores[player];
}
println("\n\nPlayer %d wins with a score of %d".fmt(player+1, safeScores[player]));
```

```txt

Player 1: (0, 0). Rolling? (y/n)
 Rolled a 5
Player 1: (0, 5). Rolling? (y/n)
 Rolled a 4
Player 1: (0, 9). Rolling? (y/n)
 Rolled a 1
 Bust! You lose 9 but keep 0

Player 2: (0, 0). Rolling? (y/n)
 Rolled a 1
 Bust! You lose 0 but keep 0
...

Player 1: (49, 0). Rolling? (y/n)
 Rolled a 2
Player 1: (49, 2). Rolling? (y/n)
 Rolled a 3
Player 1: (49, 5). Rolling? (y/n)
 Rolled a 1
 Bust! You lose 5 but keep 49

Player 2: (49, 0). Rolling? (y/n)
 Rolled a 6
Player 2: (49, 6). Rolling? (y/n)
 Rolled a 4
Player 2: (49, 10). Rolling? (y/n)
 Rolled a 2
Player 2: (49, 12). Rolling? (y/n) n
 Sticking with 61
...
Player 2: (99, 0). Rolling? (y/n)
 Rolled a 1
 Bust! You lose 0 but keep 99

Player 1: (72, 0). Rolling? (y/n)
 Rolled a 6
Player 1: (72, 6). Rolling? (y/n)
 Rolled a 4
Player 1: (72, 10). Rolling? (y/n) n
 Sticking with 82

Player 2: (99, 0). Rolling? (y/n)
 Rolled a 3

Player 2 wins with a score of 102

```



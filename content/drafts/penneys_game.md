+++
title = "Penney's game"
description = ""
date = 2019-06-25T13:58:35Z
aliases = []
[extra]
id = 17975
[taxonomies]
categories = []
tags = []
+++

{{task|Games}}

[[wp:Penney's_game|Penney's game]] is a game where two players bet on being the first to see a particular sequence of [[wp:Coin_flipping|heads or tails]] in consecutive tosses of a [[wp:Fair_coin|fair coin]].

It is common to agree on a sequence length of three then one player will openly choose a sequence, for example:
  Heads,  Tails,  Heads, or '''HTH''' for short.
The other player on seeing the first players choice will choose his sequence. The coin is tossed and the first player to see his sequence in the sequence of coin tosses wins.


;Example:
One player might choose the sequence '''HHT''' and the other '''THT'''. 

Successive coin tosses of '''HTTHT''' gives the win to the second player as the last three coin tosses are his sequence.


;Task
Create a program that tosses the coin, keeps score and plays Penney's game against a human opponent.
* Who chooses and shows their sequence of three should be chosen randomly.
* If going first, the computer should randomly choose its sequence of three.
* If going second, the computer should automatically play [[wp:Penney's_game#Analysis_of_the_three-bit_game|the optimum sequence]].
* Successive coin tosses should be shown.



Show output of a game where the computer chooses first and a game where the user goes first here on this page.


;See also
* [https://www.youtube.com/watch?v=OcYnlSenF04 The Penney Ante Part 1] (Video).
* [https://www.youtube.com/watch?v=U9wak7g5yQA The Penney Ante Part 2] (Video).





## BASIC

=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

```basic> 10 IF RND
=.5 THEN GOTO 100
 20 PRINT "YOU PICK FIRST."
 30 INPUT P$
 40 PRINT "YOU CHOSE ";P$
 50 LET C$="H"
 60 IF P$(2)="H" THEN LET C$="T"
 70 LET C$=C$+P$( TO 2)
 80 PRINT "I CHOOSE  ";C$
 90 GOTO 190
100 PRINT "I PICK FIRST."
110 LET C$=""
120 FOR I=1 TO 3
130 LET C$=C$+"H"
140 IF RND>=.5 THEN LET C$(I)="T"
150 NEXT I
160 PRINT "I CHOOSE  ";C$
170 INPUT P$
180 PRINT "YOU CHOSE ";P$
190 LET G$=""
200 IF RND>=.5 THEN GOTO 230
210 LET G$=G$+"H"
220 GOTO 240
230 LET G$=G$+"T"
240 PRINT AT 5,0;G$
250 IF LEN G$<3 THEN GOTO 200
260 IF G$(LEN G$-2 TO )=P$ THEN GOTO 290
270 IF G$(LEN G$-2 TO )=C$ THEN GOTO 310
280 GOTO 200
290 PRINT "YOU WIN"
300 STOP
310 PRINT "I WIN"
```

{{out}}

```txt
YOU PICK FIRST.
YOU CHOSE HTH
I CHOOSE  HHT


HTTTHTTTTHHT
I WIN
```



## Batch File


```dos
::
::Penney's Game Task from Rosetta Code Wiki
::Batch File Implementation
::
::Please Directly Open the Batch File to play...
::

@echo off
setlocal enabledelayedexpansion
title The Penney's Game Batch File

set cpu=0&set you=0
cls
echo.
echo Penney's Game
echo Batch File Implementation

:main
set you_bet=&set cpu_bet=
echo.&echo --------------------------------------------------------&echo.
echo CPU's Score: %cpu% Your Score: %you%
echo.
<nul set /p "dummy=Heads I start, Tails you start..."
set /a startrnd=%random%%%2
set firsttoss=Tails&set func=optimal
if %startrnd%==1 (set firsttoss=Heads&set func=rndbet)
ping -n 3 localhost >nul
<nul set /p "dummy=.   %firsttoss%^!"
echo.&echo.
goto %func%

:rndbet
set /a "seq1=%random%%%2","seq2=%random%%%2","seq3=%random%%%2"
set binary=%seq1%%seq2%%seq3%
set cpu_bet=%binary:1=H%
set cpu_bet=%cpu_bet:0=T%
echo I will bet first. So, my bet sequence will be %cpu_bet%.
:again1
set /p "you_bet=What will be your bet sequence? "
call :validate again1
echo.&echo So, you're feeling lucky on %you_bet%. We'll see...
goto succesivetossing

:optimal
echo Ok. You will bet first.
:again2
set /p "you_bet=What will be your bet sequence? "
call :validate again2
set seq1=%you_bet:~0,1%&set seq2=%you_bet:~1,1%
set new_seq1=T
if /i %seq2%==T set new_seq1=H
set cpu_bet=%new_seq1%%seq1%%seq2%
echo.&echo Hmm... My bet will be %cpu_bet%. We'll see who's lucky...

:succesivetossing
set toses=&set cnt=0
echo.
<nul set /p "dummy=Tosses:    "
:tossloop
call :tossgen
<nul set /p dummy=%toss%
ping -n 2 localhost >nul
set /a newline=%cnt%%%60
if %newline%==59 (
echo.
<nul set /p "dummy=.          "
)
if "%toses:~-3,3%"=="%cpu_bet%" goto iwin
if "%toses:~-3,3%"=="%you_bet%" goto uwin
set /a cnt+=1&goto tossloop

:tossgen
set /a rand=%random%%%2
set toss=T
if %rand%==0 set toss=H
set toses=%toses%%toss%
goto :EOF

:iwin
set /a cpu+=1&set newgame=
echo.&echo.
echo I Win^^! Better Luck Next Time...
echo.
set /p "newgame=[Type Y if U Wanna Beat Me, or Else, Exit...] "
if /i "!newgame!"=="Y" goto :main
exit

:uwin
set /a you+=1&set newgame=
echo.&echo.
echo Argh, You Win^^! ...But One Time I'll Beat You.
echo.
set /p "newgame=[Type Y for Another Game, or Else, Exit...] "
if /i "!newgame!"=="Y" goto :main
exit

:validate
echo "!you_bet!"|findstr /r /c:"^\"[hHtT][hHtT][hHtT]\"$">nul || (
	echo [Invalid Input...]&echo.&goto %1
)
if /i "!you_bet!"=="%cpu_bet%" (echo [Bet something different...]&echo.&goto %1)
for %%i in ("t=T" "h=H") do set "you_bet=!you_bet:%%~i!"
goto :EOF
```

{{out}}
Note: The outputs of tosses are 'delayed' just to make the game a little more dramatic.

```txt
Penney's Game
Batch File Implementation

--------------------------------------------------------

CPU's Score: 0 Your Score: 0

Heads I start, Tails you start....   Heads!

I will bet first. So, my bet sequence will be THH.
What will be your bet sequence? tTh

So, you're feeling lucky on TTH. We'll see...

Tosses:    HHTHTHH

I Win! Better Luck Next Time...

[Type Y if U Wanna Beat Me, or Else, Exit...] y

--------------------------------------------------------

CPU's Score: 1 Your Score: 0

Heads I start, Tails you start....   Tails!

Ok. You will bet first.
What will be your bet sequence? tHt

Hmm... My bet will be TTH. We'll see who's lucky...

Tosses:    HTTH

I Win! Better Luck Next Time...

[Type Y if U Wanna Beat Me, or Else, Exit...]
```



## BBC BASIC


```bbcbasic>REM 
penney
PRINT "*** Penney's Game ***"
REPEAT
  PRINT ' "Heads you pick first, tails I pick first."
  PRINT "And it is... ";
  WAIT 100
  ht% = RND(0 - TIME) AND 1
  IF ht% THEN
    PRINT "heads!"
    PROC_player_chooses(player$)
    computer$ = FN_optimal(player$)
    PRINT "I choose "; computer$; "."
  ELSE
    PRINT "tails!"
    computer$ = FN_random
    PRINT "I choose "; computer$; "."
    PROC_player_chooses(player$)
  ENDIF
  PRINT "Starting the game..." ' SPC 5;
  sequence$ = ""
  winner% = FALSE
  REPEAT
    WAIT 100
    roll% = RND AND 1
    IF roll% THEN
      sequence$ += "H"
      PRINT "H  ";
    ELSE
      PRINT "T  ";
      sequence$ += "T"
    ENDIF
    IF RIGHT$(sequence$, 3) = computer$ THEN
      PRINT ' "I won!"
      winner% = TRUE
    ELSE
      IF RIGHT$(sequence$, 3) = player$ THEN
        PRINT ' "Congratulations! You won."
        winner% = TRUE
      ENDIF
    ENDIF
  UNTIL winner%
  REPEAT
    valid% = FALSE
    INPUT "Another game? (Y/N) " another$
    IF INSTR("YN", another$) THEN valid% = TRUE
  UNTIL valid%
UNTIL another$ = "N"
PRINT "Thank you for playing!"
END
:
DEF PROC_player_chooses(RETURN sequence$)
LOCAL choice$, valid%, i%
REPEAT
  valid% = TRUE
  PRINT "Enter a sequence of three choices, each of them either H or T:"
  INPUT "> " sequence$
  IF LEN sequence$ <> 3 THEN valid% = FALSE
  IF valid% THEN
    FOR i% = 1 TO 3
      choice$ = MID$(sequence$, i%, 1)
      IF choice$ <> "H" AND choice$ <> "T" THEN valid% = FALSE
    NEXT
  ENDIF
UNTIL valid%
ENDPROC
:
DEF FN_random
LOCAL sequence$, choice%, i%
sequence$ = ""
FOR i% = 1 TO 3
  choice% = RND AND 1
  IF choice% THEN sequence$ += "H" ELSE sequence$ += "T"
NEXT
= sequence$
:
DEF FN_optimal(sequence$)
IF MID$(sequence$, 2, 1) = "H" THEN
  = "T" + LEFT$(sequence$, 2)
ELSE
  = "H" + LEFT$(sequence$, 2)
ENDIF
```

{{out}}

```txt
*** Penney's Game ***

Heads you pick first, tails I pick first.
And it is... heads!
Enter a sequence of three choices, each of them either H or T:
> HTT
I choose HHT.
Starting the game...
     H  H  H  H  H  T  
I won!
Another game? (Y/N) Y

Heads you pick first, tails I pick first.
And it is... tails!
I choose HTH.
Enter a sequence of three choices, each of them either H or T:
> HHT
Starting the game...
     T  T  H  H  H  H  T  
Congratulations! You won.
Another game? (Y/N) N
Thank you for playing!
```



## C

This solution stores the sequences in bit-packed integers. With minor adjustments, this can be extended to allow larger sequence lengths beyond the required 3. However, the ai's algorithm for the perfect choice would need to be altered. More robust methods of input could be chosen, as scanf is generally fairly unsafe. A safer alternative would be to use something like fgets, and parse the input string ourselves.

```C

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define SEQLEN 3

int getseq(char *s)
{
    int r = 0;
    int i = 1 << (SEQLEN - 1);

    while (*s && i) {
        switch (*s++) {
            case 'H':
            case 'h':
                r |= i;
                break;
            case 'T':
            case 't':
                /* 0 indicates tails, this is 0, so do nothing */
                break;
            default:
                return -1;
        }
        i >>= 1;
    }

    return r;
}

void printseq(int seq)
{
    int i;
    for (i = SEQLEN - 1; i >= 0; --i)
        printf("%c", seq & (1 << i) ? 'h' : 't');
}

int getuser(void)
{
    int user;
    char s[SEQLEN + 1];

    printf("Enter your sequence of %d (h/t): ", SEQLEN);
    while (1) {
        /* This needs to be manually changed if SEQLEN is changed */
        if (scanf("%3s", s) != 1) exit(1);
        if ((user = getseq(s)) != -1) return user;
        printf("Please enter only h/t characters: ");
    }
}

int getai(int user)
{
    int ai;

    printf("Computers sequence of %d is: ", SEQLEN);
    /* The ai's perfect choice will only be perfect for SEQLEN == 3 */
    if (user == -1)
        ai = rand() & (1 << SEQLEN) - 1;
    else
        ai = (user >> 1) | ((~user << 1) & (1 << SEQLEN - 1));

    printseq(ai);
    printf("\n");
    return ai;
}

int rungame(int user, int ai)
{
    /* Generate first SEQLEN flips. We only need to store the last SEQLEN
     * tosses at any one time. */
    int last3 = rand() & (1 << SEQLEN) - 1;

    printf("Tossed sequence: ");
    printseq(last3);
    while (1) {
        if (user == last3) {
            printf("\nUser wins!\n");
            return 1;
        }

        if (ai == last3) {
            printf("\nAi wins!\n");
            return 0;
        }

        last3 = ((last3 << 1) & (1 << SEQLEN) - 2) | (rand() & 1);
        printf("%c", last3 & 1 ? 'h' : 't');
    }
}

int main(void)
{
    srand(time(NULL));
    int playerwins = 0;
    int totalgames = 0;

    /* Just use ctrl-c for exit */
    while (1) {
        int user = -1;
        int ai   = -1;

        printf("\n");
        if (rand() & 1) {
            ai   = getai(user);
            user = getuser();
        }
        else {
            user = getuser();
            ai   = getai(user);
        }
        
        playerwins += rungame(user, ai);
        totalgames++;

        printf("You have won %d out of %d games\n", playerwins, totalgames);
        printf("
### ===========================
\n");
    }

    return 0;
}

```

{{out}}

```txt


Enter your sequence of 3 (h/t): tth
Computers sequence of 3 is: htt
Tossed sequence: ttth
User wins!
You have won 1 out of 1 games

### ===========================


Enter your sequence of 3 (h/t): tth
Computers sequence of 3 is: htt
Tossed sequence: thhhthhhthhhtt
Ai wins!
You have won 1 out of 2 games

### ===========================



```


## C++


```cpp

#include <time.h>
#include <iostream>
#include <string>

using namespace std;

class penney
{
public:
    penney()
    { pW = cW = 0;  }
    void gameLoop()
    {
	string a;
	while( true )
	{
	    playerChoice = computerChoice = "";
	    if( rand() % 2 ) 
	    { computer(); player(); }
	    else
	    { player(); computer(); }

	    play();

	    cout << "[Y] to play again "; cin >> a;
	    if( a[0] != 'Y' && a[0] != 'y' ) 
	    {
		cout << "Computer won " << cW << " times." << endl << "Player won " << pW << " times.";
		break;
	    }
	    cout << endl << endl;
        }
    }
private:
    void computer()
    {
        if( playerChoice.length() == 0 )
	{
	    for( int x = 0; x < 3; x++ )
		computerChoice.append( ( rand() % 2 ) ? "H" : "T", 1 );	
	}
	else
	{
	    computerChoice.append( playerChoice[1] == 'T' ? "H" : "T", 1 );
	    computerChoice += playerChoice.substr( 0, 2 );
	}
	cout << "Computer's sequence of three is: " << computerChoice << endl;
    }
    void player()
    {
	cout << "Enter your sequence of three (H/T) "; cin >> playerChoice;
    }
    void play()
    {
	sequence = "";
	while( true )
	{
	    sequence.append( ( rand() % 2 ) ? "H" : "T", 1 );
	    if( sequence.find( playerChoice ) != sequence.npos )
	    {
		showWinner( 1 );
		break;
	    }
	    else if( sequence.find( computerChoice ) != sequence.npos )
	    {
		showWinner( 0 );
		break;
	    }
	}
    }
    void showWinner( int i )
    {
	string s;
	if( i ) { s = "Player wins!"; pW++; }
	else { s = "Computer wins!"; cW++; }
	cout << "Tossed sequence: " << sequence << endl << s << endl << endl;
    }
    string playerChoice, computerChoice, sequence;
    int pW, cW;
};
int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned>( time( NULL ) ) );
    penney game; game.gameLoop();
    return 0;
}

```

{{out}}

```txt

Computer's sequence of three is: HHH
Enter your sequence of three (H/T) THH
Tossed sequence: TTHTTTTTTTTTTTHTHTTTHTTHTTHH
Player wins!

[Y] to play again y

Enter your sequence of three (H/T) HTH
Computer's sequence of three is: HHT
Tossed sequence: THHT
Computer wins!

[Y] to play again 

```



## C sharp

{{works with|C sharp|7}}

```csharp
using static System.Console;
using static System.Threading.Thread;
using System;

public static class PenneysGame
{
    const int pause = 500;
    const int N = 3;
    static Random rng = new Random();

    static int Toss() => rng.Next(2);

    static string AsString(this int sequence) {
        string s = "";
        for (int b = 0b100; b > 0; b >>= 1) {
            s += (sequence & b) > 0 ? 'T' : 'H';
        }
        return s;
    }

    static int UserInput() {
        while (true) {
            switch (ReadKey().Key) {
                case ConsoleKey.Escape: return -1;
                case ConsoleKey.H: return 0;
                case ConsoleKey.T: return 1;
            }
            Console.Write('\b');
        }
    }

    public static void Main2() {
        int yourScore = 0, myScore = 0;
        while (true) {
            WriteLine($"Your score: {yourScore}, My score: {myScore}");
            WriteLine("Determining who goes first...");
            Sleep(pause);
            bool youStart = Toss() == 1;
            WriteLine(youStart ? "You go first." : "I go first.");
            int yourSequence = 0, mySequence = 0;
            if (youStart) {
                WriteLine("Choose your sequence of (H)eads and (T)ails (or press Esc to exit)");
                int userChoice;
                for (int i = 0; i < N; i++) {
                    if ((userChoice = UserInput()) < 0) return;
                    yourSequence = (yourSequence << 1) + userChoice;
                }
                mySequence = ((~yourSequence << 1) & 0b100) | (yourSequence >> 1);
            } else {
                for (int i = 0; i < N; i++) {
                    mySequence = (mySequence << 1) + Toss();
                }

                WriteLine("I chose " + mySequence.AsString());
                do {
                    WriteLine("Choose your sequence of (H)eads and (T)ails (or press Esc to exit)");
                    int choice;
                    yourSequence = 0;
                    for (int i = 0; i < N; i++) {
                        if ((choice = UserInput()) < 0) return;
                        yourSequence = (yourSequence << 1) + choice;
                    }
                    if (yourSequence == mySequence) {
                        WriteLine();
                        WriteLine("You cannot choose the same sequence.");
                    }
                } while (yourSequence == mySequence);
            }

            WriteLine();
            WriteLine($"Your sequence: {yourSequence.AsString()}, My sequence: {mySequence.AsString()}");
            WriteLine("Tossing...");
            int sequence = 0;
            for (int i = 0; i < N; i++) {
                Sleep(pause);
                int toss = Toss();
                sequence = (sequence << 1) + toss;
                Write(toss > 0 ? 'T' : 'H');
            }
            while (true) {
                if (sequence == yourSequence) {
                    WriteLine();
                    WriteLine("You win!");
                    yourScore++;
                    break;
                } else if (sequence == mySequence) {
                    WriteLine();
                    WriteLine("I win!");
                    myScore++;
                    break;
                }
                Sleep(pause);
                int toss = Toss();
                sequence = ((sequence << 1) + toss) & 0b111;
                Write(toss > 0 ? 'T' : 'H');
            }
            WriteLine("Press a key.");
            ReadKey();
            Clear();
        }
    }

}

```

{{out}}
<pre style="height:30ex;overflow:scroll">
Your score: 0, My score: 0
Determining who goes first...
I go first.
I chose HHH
Choose your sequence of (H)eads and (T)ails (or press Esc to exit)
thh
Your sequence: THH, My sequence: HHH
Tossing...
HTHTHTHTTHH
You win!
Press a key.


Your score: 2, My score: 0
Determining who goes first...
You go first.
Choose your sequence of (H)eads and (T)ails (or press Esc to exit)
thh
Your sequence: THH, My sequence: TTH
Tossing...
HTTTTH
I win!
Press a key.
```



## Clojure


```clojure
(ns penney.core
  (:gen-class))

(def heads \H)
(def tails \T)

(defn flip-coin []
  (let [flip (rand-int 2)]
    (if (= flip 0) heads tails)))

(defn turn [coin]
  (if (= coin heads) tails heads))

(defn first-index [combo coll]
  (some #(if (= (second %) combo) (first %)) coll))

(defn find-winner [h c]
  (if (< h c)
    (do (println "YOU WIN!\n") :human)
    (do (println "COMPUTER WINS!\n") :computer)))

(defn flip-off [human comp]
  (let [flips (repeatedly flip-coin)
        idx-flips (map-indexed vector (partition 3 1 flips))
        h (first-index (seq human) idx-flips)
        c (first-index (seq comp) idx-flips)]
    (println (format "Tosses: %s" (apply str (take (+ 3 (min h c)) flips))))
    (find-winner h c)))

(defn valid? [combo]
  (if (empty? combo) true (and (= 3 (count combo)) (every? #(or (= heads %) (= tails %)) combo))))

(defn ask-move []
  (println "What sequence of 3 Heads/Tails do you choose?")
  (let [input (clojure.string/upper-case (read-line))]
    (if-not (valid? input) (recur) input)))

(defn optimize-against [combo]
  (let [mid (nth combo 1)
        comp (str (turn mid) (first combo) mid)]
    (println (format "Computer chooses %s: " comp)) comp))

(defn initial-move [game]
  (let [combo (apply str (repeatedly 3 flip-coin))]
    (println "--------------")
    (println (format "Current score | CPU: %s, You: %s" (:computer game) (:human game)))
    (if (= (:first-player game) tails)
      (do
        (println "Computer goes first and chooses: " combo)
        combo)
      (println "YOU get to go first."))))

(defn play-game [game]
    (let [c-move (initial-move game)
          h-move (ask-move)]
      (if-not (empty? h-move)
        (let [winner (flip-off h-move (if (nil? c-move) (optimize-against h-move) c-move))]
          (recur (assoc game winner (inc (winner game)) :first-player (flip-coin))))
        (println "Thanks for playing!"))))

(defn -main [& args]
  (println "Penney's Game.")
  (play-game {:first-player (flip-coin)
              :human 0, :computer 0}))
```


{{out}}

```txt

Penney's Game.
--------------
Current score | CPU: 0, You: 0
Computer goes first and chooses:  THT
What sequence of 3 Heads/Tails do you choose?
tth
Tosses: TTH
YOU WIN!

--------------
Current score | CPU: 0, You: 1
YOU get to go first.
What sequence of 3 Heads/Tails do you choose?
tht
Computer chooses TTH: 
Tosses: HHHTTH
COMPUTER WINS!

--------------
Current score | CPU: 1, You: 1
YOU get to go first.
What sequence of 3 Heads/Tails do you choose?

Thanks for playing!

```



## D

{{trans|Python}}

```d
void main() {
    import std.stdio, std.random, std.algorithm, std.string,
           std.conv, std.range, core.thread;

    immutable first = uniform(0, 2) == 0;

    string you, me;
    if (first) {
        me = 3.iota.map!(_ => "HT"[uniform(0, $)]).text;
        writefln("I choose first and will win on first seeing %s in the list of tosses", me);
        while (you.length != 3 || you.any!(c => !c.among('H', 'T')) || you == me) {
            "What sequence of three Heads/Tails will you win with: ".write;
            you = readln.strip;
        }
    } else {
        while (you.length != 3 || you.any!(c => !c.among('H', 'T'))) {
            "After you: What sequence of three Heads/Tails will you win with: ".write;
            you = readln.strip;
        }
        me = (you[1] == 'T' ? 'H' : 'T') ~ you[0 .. 2];
        writefln("I win on first seeing %s in the list of tosses", me);
    }

    "Rolling:\n  ".write;
    string rolled;
    while (true) {
        rolled ~= "HT"[uniform(0, $)];
        rolled.back.write;
        if (rolled.endsWith(you))
            return "\n  You win!".writeln;
        if (rolled.endsWith(me))
            return "\n  I win!".writeln;
        Thread.sleep(1.seconds);
    }
}
```

The output is the same as the Python entry.


## Elixir

{{trans|Ruby}}

```elixir
defmodule Penney do
  @toss [:Heads, :Tails]
  
  def game(score \\ {0,0})
  def game({iwin, ywin}=score) do
    IO.puts "Penney game score  I : #{iwin}, You : #{ywin}"
    [i, you] = @toss
    coin = Enum.random(@toss)
    IO.puts "#{i} I start, #{you} you start ..... #{coin}"
    {myC, yC} = setup(coin)
    seq = for _ <- 1..3, do: Enum.random(@toss)
    IO.write Enum.join(seq, " ")
    {winner, score} = loop(seq, myC, yC, score)
    IO.puts "\n #{winner} win!\n"
    game(score)
  end
  
  defp setup(:Heads) do
    myC = Enum.shuffle(@toss) ++ [Enum.random(@toss)]
    joined = Enum.join(myC, " ")
    IO.puts "I chose  : #{joined}"
    {myC, yourChoice}
  end
  defp setup(:Tails) do
    yC = yourChoice
    myC = (@toss -- [Enum.at(yC,1)]) ++ Enum.take(yC,2)
    joined = Enum.join(myC, " ")
    IO.puts "I chose  : #{joined}"
    {myC, yC}
  end
  
  defp yourChoice do
    IO.write "Enter your choice (H/T) "
    choice = read([])
    IO.puts "You chose: #{Enum.join(choice, " ")}"
    choice
  end
  
  defp read([_,_,_]=choice), do: choice
  defp read(choice) do
    case IO.getn("") |> String.upcase do
      "H" -> read(choice ++ [:Heads])
      "T" -> read(choice ++ [:Tails])
      _   -> read(choice)
    end
  end
  
  defp loop(myC, myC, _, {iwin, ywin}), do: {"I", {iwin+1, ywin}}
  defp loop(yC,  _,  yC, {iwin, ywin}), do: {"You", {iwin, ywin+1}}
  defp loop(seq, myC, yC, score) do
    append = Enum.random(@toss)
    IO.write " #{append}"
    loop(tl(seq)++[append], myC, yC, score)
  end
end

Penney.game
```


{{out}}

```txt

Penney game score  I : 0, You : 0
Heads I start, Tails you start ..... Heads
I chose  : Tails Heads Heads
Enter your choice (H/T) TTH
You chose: Tails Tails Heads
Heads Heads Tails Tails Tails Tails Heads
 You win!

Penney game score  I : 0, You : 1
Heads I start, Tails you start ..... Tails
Enter your choice (H/T) THT
You chose: Tails Heads Tails
I chose  : Tails Tails Heads
Tails Heads Heads Heads Tails Tails Heads
 I win!

Penney game score  I : 1, You : 1
Heads I start, Tails you start ..... Tails
Enter your choice (H/T)

```



## Factor


```factor
USING: arrays ascii io kernel math prettyprint random sequences
strings ;
IN: rosetta-code.penneys-game

! Generate a random boolean.
: t|f ( -- t/f )
    1 random-bits 0 = ;

! Checks whether the sequence chosen by the human is valid.
: valid-input? ( seq -- ? )
    [ [ CHAR: H = ] [ CHAR: T = ] bi or ] filter length 3 = ;

! Prompt the human player for a sequence.
: input-seq ( -- seq )
    "Please input a 3-long sequence of H or T (heads or tails)."
    print "Example: HTH" print "> " write readln >upper >array ;

! Get the human player's input sequence with error checking.    
: get-input ( -- seq )
    t [ drop input-seq dup valid-input? not ] loop ;
    
! Add a random coin flip to a vector.
: flip-coin ( vector -- vector' )
    t|f CHAR: H CHAR: T ? over push ;

! Generate a random 3-long sequence of coin flips.
: rand-seq ( -- seq )
    V{ } clone 3 [ flip-coin ] times ;
    
! Generate the optimal sequence response to a given sequence.
: optimal ( seq1 -- seq2 )
    [ second dup CHAR: H = [ CHAR: T ] [ CHAR: H ] if ]
    [ first ] [ second ] tri 3array nip dup
    "The computer chose " write >string write "." print ;

! Choose a random sequence for the computer and report what
! was chosen.
: computer-first ( -- seq )
    "The computer picks a sequence first and chooses " write
    rand-seq dup >string write "." print >array ;

! The human is prompted to choose any sequence with no
! restrictions.
: human-first ( -- seq )
    "You get to go first." print get-input ;

! Forbid the player from choosing the same sequence as the
! computer.
: human-second ( cseq -- cseq hseq )
    get-input [ 2dup = not ]
    [ drop
        "You may not choose the same sequence as the computer."
        print get-input
    ] until ;

! Display a message introducing the game.
: welcome ( -- )
    "Welcome to Penney's Game. The computer or the player" print
    "will be randomly selected to choose a sequence of"    print
    "three coin tosses. The sequence will be shown to the" print
    "opponent, and then he will choose a sequence."   print nl
    "Then, a coin will be flipped until the sequence" print
    "matches the last three coin flips, and a winner" print
    "announced." print nl ;

! Check for human victory.
: human-won? ( cseq hseq coin-flips -- ? )
    3 tail* >array = nip ;
    
! Check for computer victory.
: computer-won? ( cseq hseq coin-flips -- ? )
    3 tail* >array pick = 2nip ;

! Flip a coin until a victory is detected. Then, inform the
! player who won.
: flip-coins ( cseq hseq -- )
    "Flipping coins..." print
    rand-seq [ 3dup [ human-won? ] [ computer-won? ] 3bi or ]
    [ flip-coin ] until dup >string print human-won?
    [ "You won!" print ] [ "The computer won." print ] if ;

! Randomly choose a player to choose their sequence first.
! Then play a full round of Penney's Game.
: start-game ( -- )
    welcome t|f [ human-first dup optimal swap ]
    [ computer-first human-second ] if flip-coins ;
    
start-game
```

{{out}}

```txt

Welcome to Penney's Game. The computer or the player
will be randomly selected to choose a sequence of
three coin tosses. The sequence will be shown to the
opponent, and then he will choose a sequence.

Then, a coin will be flipped until the sequence
matches the last three coin flips, and a winner
announced.

You get to go first.
Please input a 3-long sequence of H or T (heads or tails).
Example: HTH
> car
Please input a 3-long sequence of H or T (heads or tails).
Example: HTH
> htht
Please input a 3-long sequence of H or T (heads or tails).
Example: HTH
> tth
The computer chose HTT.
Flipping coins...
THHTHTHTHHTHHTHHTT
The computer won.

Welcome to Penney's Game. The computer or the player
will be randomly selected to choose a sequence of
three coin tosses. The sequence will be shown to the
opponent, and then he will choose a sequence.

Then, a coin will be flipped until the sequence
matches the last three coin flips, and a winner
announced.

The computer picks a sequence first and chooses HHH.
Please input a 3-long sequence of H or T (heads or tails).
Example: HTH
> hhh
You may not choose the same sequence as the computer.
Please input a 3-long sequence of H or T (heads or tails).
Example: HTH
> hht
Flipping coins...
HHH
The computer won.

```



## Go


```Go

package main
import "fmt"
import "math/rand"
func main(){
var a1,a2,a3,y,match,j,k int
var inp string
y=1
for y==1{
fmt.Println("Enter your sequence:")
fmt.Scanln(&inp)
var Ai [3] int
var user [3] int
for j=0;j<3;j++{
if(inp[j]==104){
user[j]=1
}else{
user[j]=0
}
}
for k=0;k<3;k++{
Ai[k]=rand.Intn(2)
}
for user[0]==Ai[0]&&user[1]==Ai[1]&&user[2]==Ai[2]{
for k=0;k<3;k++{
Ai[k]=rand.Intn(2)
}
}
fmt.Println("You gave the sequence:")
printht(user)
fmt.Println()
fmt.Println("The computer generated sequence is:")
printht(Ai)
fmt.Println()
a1=rand.Intn(2)
a2=rand.Intn(2)
a3=rand.Intn(2)
fmt.Print("The generated sequence is:")
printh(a1)
printh(a2)
printh(a3)
match=0
for match==0{
if(matches(user,a1,a2,a3)==1){
fmt.Println()
fmt.Println("You have won!!!")
match=1
}else if(matches(Ai,a1,a2,a3)==1){
fmt.Println()
fmt.Println("You lost!! Computer wins")
match=1
}else{
a1=a2
a2=a3
a3=rand.Intn(2)
printh(a3)
}
}
fmt.Println("Do you want to continue(0/1):")
fmt.Scanln(&y)
}
}
func printht(a [3] int) int{
var i int
for i=0;i<3;i++{
if(a[i]==1){
fmt.Print("h")
}else{
fmt.Print("t")
}
}
return 1
}
func printh(a int) int{
if(a==1){
fmt.Print("h")
}else{
fmt.Print("t")
}
return 1
}
func matches(a [3] int,p int,q int,r int) int{
if(a[0]==p&&a[1]==q&&a[2]==r){
return 1
}else{
return 0
}
}

```

Output:

```txt

Enter your sequence:
tht
You gave the sequence:
tht
The computer generated sequence is:
hhh
The generated sequence is:hhtht
You have won!!!
Do you want to continue(0/1):

```


## Haskell


```Haskell
import qualified Data.List as L
import System.IO
import System.Random

data CoinToss = H | T deriving (Read, Show, Eq)

parseToss :: String -> [CoinToss]
parseToss [] = []
parseToss (s:sx)
  | s == 'h' || s == 'H' = H : parseToss sx
  | s == 't' || s == 'T' = T : parseToss sx
  | otherwise = parseToss sx

notToss :: CoinToss -> CoinToss
notToss H = T
notToss T = H

instance Random CoinToss where
  random g = let (b, gb) = random g in (if b then H else T, gb)
  randomR = undefined

prompt :: (Read a) => String -> String -> (String -> Maybe a) -> IO a
prompt msg err parse = do
  putStrLn msg
  line <- getLine
  let ans = parse line
  case ans of
    Nothing   -> do
      putStrLn err
      prompt msg err parse
    Just ansB -> return ansB

showCat :: (Show a) => [a] -> String
showCat = concatMap show

data Winner = Player | CPU

-- This may never terminate.
runToss :: (RandomGen g) => [CoinToss] -> [CoinToss] -> g -> ([CoinToss], Winner)
runToss player cpu gen =
  let stream = randoms gen
      run ss@(s:sx)
        | L.isPrefixOf player ss = player
        | L.isPrefixOf cpu ss    = cpu
        | otherwise              = s : run sx
      winner = run stream
  in if L.isSuffixOf player winner
     then (winner, Player)
     else (winner, CPU)

game :: (RandomGen g, Num a, Show a) => Bool -> a -> a -> g -> IO ()
game cpuTurn playerScore cpuScore gen = do
  putStrLn $ "\nThe current score is CPU: " ++ show cpuScore
    ++ ", You: " ++ show playerScore

  let (genA, genB) = split gen
      promptPlayer check =
        prompt "Pick 3 coin sides: " "Invalid input." $ \s ->
          let tosses = parseToss s in
          if check tosses then Just tosses else Nothing
      promptCpu x  = putStrLn $ "I have chosen: " ++ showCat x

  (tosses, winner) <-
    if cpuTurn
    then do
      let cpuChoice = take 3 $ randoms gen
      promptCpu cpuChoice
      playerChoice <- promptPlayer $ \n -> n /= cpuChoice && 3 == length n
      return $ runToss playerChoice cpuChoice genA
    else do
      playerChoice <- promptPlayer $ \n -> 3 == length n
      let cpuChoice = case playerChoice of [a,b,_] -> [notToss b, a, b]
      promptCpu cpuChoice
      return $ runToss playerChoice cpuChoice genA

  putStrLn $ "The sequence tossed was: " ++ showCat tosses

  case winner of
    Player -> do
      putStrLn "You win!"
      game (not cpuTurn) (playerScore + 1) cpuScore genB
    CPU -> do
      putStrLn "I win!"
      game (not cpuTurn) playerScore (cpuScore + 1) genB

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  stdgen <- getStdGen
  let (cpuFirst, genA) = random stdgen
  game cpuFirst 0 0 genA
```

{{out}}

```txt

The current score is CPU: 0, You: 0
Pick 3 coin sides:
HTH
I have chosen: HHT
The sequence tossed was: TTTTTTHHT
I win!

The current score is CPU: 1, You: 0
I have chosen: TTT
Pick 3 coin sides:
HTT
The sequence tossed was: HTT
You win!

The current score is CPU: 1, You: 1
Pick 3 coin sides:

```



## J

'''Solution:'''

```J
require 'format/printf numeric'

randomize ''                               NB. randomize seed for new session

Vals=: 'HT'                                NB. valid values
input=: 1!:1@<&1:                          NB. get input from user
prompt=: input@echo                        NB. prompt user for input
checkInput=: 'Choose 3 H/Ts' assert (Vals e.~ ]) , 3 = #
getUserSeq=: (] [ checkInput)@toupper@prompt
choose1st=: Vals {~ 3 ?@$ 2:               NB. computer chooses 1st
choose2nd=: (0 1 1=1 0 1{])&.(Vals&i.)     NB. computer chooses 2nd

playPenney=: verb define
  if. ?2 do.                               NB. randomize first chooser
    Comp=. choose1st ''
    'Computer chose %s' printf <Comp
    You=. getUserSeq 'Choose a sequence of three coin tosses (H/T):'
    'Choose a different sequence to computer' assert You -.@-: Comp
  else.
    You=. getUserSeq 'Choose a sequence of three coin tosses (H/T):'
    Comp=. choose2nd You
    'Computer chose %s ' printf <Comp
  end.
  Tosses=. Vals {~ 100 ?@$ 2
  Result=. (Comp,:You) {.@I.@E."1 Tosses
  'Toss sequence is %s' printf < (3 + <./ Result) {. Tosses
  echo ('No result';'You win!';'Computer won!') {::~ *-/ Result
)
```

'''Usage:'''

```J
   playPenney''
Computer chose TTT
Choose a sequence of three coin tosses (H/T):
HTT
Toss sequence is TTHHTT
You win!
   playPenney''
Choose a sequence of three coin tosses (H/T):
HTT
Computer chose HHT 
Toss sequence is HTHTT
You win!
```



## Java


```java
import java.util.*;

public class PenneysGame {

    public static void main(String[] args) {
        Random rand = new Random();

        String compChoice = "", playerChoice;
        if (rand.nextBoolean()) {

            for (int i = 0; i < 3; i++)
                compChoice += "HT".charAt(rand.nextInt(2));
            System.out.printf("Computer chooses %s%n", compChoice);

            playerChoice = prompt(compChoice);

        } else {

            playerChoice = prompt(compChoice);

            compChoice = "T";
            if (playerChoice.charAt(1) == 'T')
                compChoice = "H";
            compChoice += playerChoice.substring(0, 2);
            System.out.printf("Computer chooses %s%n", compChoice);
        }

        String tossed = "";
        while (true) {
            tossed += "HT".charAt(rand.nextInt(2));
            System.out.printf("Tossed %s%n" , tossed);
            if (tossed.endsWith(playerChoice)) {
                System.out.println("You win!");
                break;
            }
            if (tossed.endsWith(compChoice)) {
                System.out.println("Computer wins!");
                break;
            }
        }
    }

    private static String prompt(String otherChoice) {
        Scanner sc = new Scanner(System.in);
        String s;
        do {
            System.out.print("Choose a sequence: ");
            s = sc.nextLine().trim().toUpperCase();
        } while (!s.matches("[HT]{3}") || s.equals(otherChoice));
        return s;
    }
}
```



```txt
Computer chooses HTH
Choose a sequence: hhh
Tossed H
Tossed HH
Tossed HHH
You win!

Choose a sequence: tth
Computer chooses HTT
Tossed H
Tossed HT
Tossed HTH
Tossed HTHH
Tossed HTHHT
Tossed HTHHTT
Computer wins!
```



## Julia

'''Functions'''

```Julia

const SLEN = 3

autobet() = randbool(SLEN)
function autobet(ob::BitArray{1})
    rlen = length(ob)
    2 < rlen || return ~ob
    3 < rlen || return [~ob[2], ob[1:2]]
    opt = falses(rlen)
    opt[1] = true
    opt[end-1:end] = true
    ob != opt || return ~opt
    return opt
end
autobet(ob::Array{Bool,1}) = autobet(convert(BitArray{1}, ob))    

function pgencode{T<:String}(a::T)
    b = uppercase(a)
    0 < length(b) || return trues(0)
    !ismatch(r"[^HT]+", b) || error(@sprintf "%s is not a HT sequence" a)
    b = split(b, "")
    b .== "H"
end
pgdecode(a::BitArray{1}) = join([i ? "H" : "T" for i in a], "")

function humanbet()
    b = ""
    while length(b) != SLEN  || ismatch(r"[^HT]+", b)
        print("Your bet? ")
        b = uppercase(chomp(readline()))
    end
    return b
end

```


'''Game Setup'''

```Julia

println("Playing Penney's Game Against the computer.")

if randbool()
    mach = autobet()
    println(@sprintf "The computer bet first, chosing %s." pgdecode(mach))
    println("Now you can bet.")
    human = pgencode(humanbet())
else
    println("You bet first.")
    human = pgencode(humanbet())
    mach = autobet(human)
end
print(@sprintf "You bet %s " pgdecode(human))
println(@sprintf "and the computer bet %s." pgdecode(mach))

```


'''Game Play'''

```Julia

pg = randbool(SLEN)
pgtail = copy(pg)
while pgtail != mach && pgtail != human
    push!(pg, randbool())
    pgtail = [pgtail[2:end], pg[end]]
end

println(@sprintf("This game lasted %d turns yielding\n    %s",
                 length(pg), pgdecode(pg)))

if human == mach
    println("so you and the computer tied.")
elseif pgtail == mach
    println("so the computer won.")
else
    println("so you won.")
end

```


{{output}}

```txt

$ julia penneys_game.jl
Playing Penney's Game Against the computer.
You bet first.
Your bet? tht
You bet THT and the computer bet TTH.
This game lasted 10 turns yielding
    HHTTTTTTTH
so the computer won.

$ julia penneys_game.jl
Playing Penney's Game Against the computer.
You bet first.
Your bet? hhh
You bet HHH and the computer bet THH.
This game lasted 8 turns yielding
    TTHTHTHH
so the computer won.

$ julia penneys_game.jl
Playing Penney's Game Against the computer.
The computer bet first, chosing HTT.
Now you can bet.
Your bet? hht
You bet HHT and the computer bet HTT.
This game lasted 6 turns yielding
    TTHHHT
so you won.

$ julia penneys_game.jl
Playing Penney's Game Against the computer.
The computer bet first, chosing HTT.
Now you can bet.
Your bet? hht
You bet HHT and the computer bet HTT.
This game lasted 6 turns yielding
    THTHHT
so you won.

$ julia penneys_game.jl
Playing Penney's Game Against the computer.
You bet first.
Your bet? htt
You bet HTT and the computer bet HHT.
This game lasted 10 turns yielding
    TTTTHHHHHT
so the computer won.

```



## Kotlin


```scala
// version 1.2.10

import java.util.Random

val rand = Random()

val optimum = mapOf(
    "HHH" to "THH", "HHT" to "THH", "HTH" to "HHT", "HTT" to "HHT",
    "THH" to "TTH", "THT" to "TTH", "TTH" to "HTT", "TTT" to "HTT"
)

fun getUserSequence(): String {
    println("A sequence of three H or T should be entered")
    var userSeq: String
    do {
        print("Enter your sequence: ")
        userSeq = readLine()!!.toUpperCase()
    }
    while (userSeq.length != 3 || userSeq.any { it != 'H' && it != 'T' })
    return userSeq
}

fun getComputerSequence(userSeq: String = ""): String {
    val compSeq = if(userSeq == "")
        String(CharArray(3) { if (rand.nextInt(2) == 0) 'T' else 'H' })
    else
        optimum[userSeq]!!
    println("Computer's sequence: $compSeq")
    return compSeq
}

fun main(args: Array<String>) {
    var userSeq: String
    var compSeq: String
    val r = rand.nextInt(2)
    if (r == 0) {
        println("You go first")
        userSeq = getUserSequence()
        println()
        compSeq = getComputerSequence(userSeq)
    }
    else {
        println("Computer goes first")
        compSeq = getComputerSequence()
        println()
        userSeq = getUserSequence()
    }

    println()
    val coins = StringBuilder()
    while (true) {
        val coin = if (rand.nextInt(2) == 0) 'H' else 'T'
        coins.append(coin)
        println("Coins flipped: $coins")
        val len = coins.length
        if (len >= 3) {
            val seq = coins.substring(len - 3, len)
            if (seq == userSeq) {
                println("\nYou win!")
                return
            }
            else if (seq == compSeq) {
                println("\nComputer wins!")
                return
            }
        }
        Thread.sleep(2000) // wait two seconds for next flip
    }
}

```


Sample game where computer goes first:

```txt

Computer goes first
Computer's sequence: HTT

A sequence of three H or T should be entered
Enter your sequence: HHT

Coins flipped: H
Coins flipped: HH
Coins flipped: HHT

You win!

```


Sample game where user goes first:

```txt

You go first
A sequence of three H or T should be entered
Enter your sequence: HTH

Computer's sequence: HHT

Coins flipped: T
Coins flipped: TT
Coins flipped: TTT
Coins flipped: TTTT
Coins flipped: TTTTH
Coins flipped: TTTTHT
Coins flipped: TTTTHTH

You win!

```



## Lua


```lua

function penny_game()
    local player, computer = "", ""
    function player_choose()
        io.write( "Enter your sequence of three H and/or T: " )
        local t = io.read():upper()
        if #t > 3 then t = t:sub( 1, 3 )
        elseif #t < 3 then return ""
        end

        for i = 1, 3 do
            c = t:sub( i, i )
            if c ~= "T" and c ~= "H" then
                print( "Just H's and T's!" )
                return ""
            end
        end
        return t
    end
    function computer_choose()
        local t = ""
        if #player > 0 then
            if player:sub( 2, 2 ) == "T" then
                t = "H"
            else
                t = "T";
            end
            t = t .. player:sub( 1, 2 )
        else
            for i = 1, 3 do
                if math.random( 2 ) == 1 then
                    t = t .. "H"
                else
                    t = t .. "T"
                end
            end
        end
        return t
    end
    if math.random( 2 ) == 1 then
        computer = computer_choose()
        io.write( "My sequence is: " .. computer .. "\n" )
        while( true ) do
            player = player_choose()
            if player:len() == 3 then break end
        end
    else
        while( true ) do
            player = player_choose()
            if player:len() == 3 then break end
        end
        computer = computer_choose()
        io.write( "My sequence is: " .. computer .. "\n" )
    end
    local coin, i = "", 1
    while( true ) do
        if math.random( 2 ) == 1 then
            coin = coin .. "T"
            io.write( "T" )
        else
            coin = coin .. "H"
            io.write( "H" )
        end
        if #coin > 2 then
            local seq = coin:sub( i, i + 2 )
            i = i + 1
            if seq == player then
                print( "\nPlayer WINS!!!" )
                return 1
            elseif seq == computer then
                print( "\nComputer WINS!!!" )
                return -1
            end
        end
    end
end
math.randomseed( os.time() )
local cpu, user = 0, 0
repeat
    r = penny_game()
    if r > 0 then
        user = user + 1
    else
        cpu = cpu + 1
    end
    print( "Player: " .. user .. " CPU: " .. cpu )
    io.write( "Play again (Y/N)? " )
    r = io.read()
until( r == "N" or r == "n" )

```

{{out}}

```txt

>lua -e "io.stdout:setvbuf 'no'" "penny.lua" 
My sequence is: TTH
Enter your sequence of three H and/or T: HHT
TTTTH
Computer WINS!!!
Player: 0 CPU: 1
Play again (Y/N)? y
Enter your sequence of three H and/or T: HTH
My sequence is: HHT
THTTHTH
Player WINS!!!
Player: 1 CPU: 1
Play again (Y/N)? y
My sequence is: HHH
Enter your sequence of three H and/or T: TTH
HTTHHTH
Player WINS!!!
Player: 2 CPU: 1
Play again (Y/N)? n
>Exit code: 0

```



## MiniScript


```MiniScript
randomTorH = function()
    if rnd < 0.5 then return "T" else return "H"
end function

if rnd < 0.5 then
    playerSeq = input("Input your sequence (e.g. HTH): ").upper
    if playerSeq[1] == "T" then compSeq = "H" else compSeq = "T"
    compSeq = compSeq + playerSeq[:2]
    print "I choose: " + compSeq
else
    compSeq = randomTorH + randomTorH + randomTorH
    print "I choose: " + compSeq
    playerSeq = input("Input your sequence (e.g. HTH): ").upper
end if

print "Here we go..."
seq = ""
while true
    flip = randomTorH
    print flip
    seq = seq + flip
    if seq[-3:] == playerSeq then
        print "You win!"
        break
    else if seq[-3:] == compSeq then
        print "I win!"
        break
    end if
    wait
end while
```


{{out}}

```txt
Input your sequence (e.g. HTH): HTH
I choose: HHT
Here we go...
T
H
T
H
You win!
```



```txt
I choose: TTH
Input your sequence (e.g. HTH):
THH
Here we go...
H
T
T
T
T
H
I win!
```



## Pascal


```pascal
PROGRAM Penney;

TYPE
    CoinToss = (heads, tails);
    Sequence = array [1..3] of CoinToss;
    Player = record
        bet: Sequence;
        score: integer;
    end;

VAR
    Human, Computer: Player;
    Rounds, Count: integer;

Function TossCoin: CoinToss;
{ Returns heads or tails at random }
Begin
    if random(2) = 1 then TossCoin := Heads
    else TossCoin := tails
End;

Procedure PutToss(toss: CoinToss);
{ Outputs heads or tails as a letter }
Begin
    if toss = heads then write('H')
    else write('T')
End;

Function GetToss: CoinToss;
{ Reads H or T from the keyboard in either lettercase }
var c: char;
Begin
    { Keep reading characters until we get an appropriate letter }
    repeat read(c) until c in ['H', 'h', 'T', 't'];
    { Interpret the letter }
    if c in ['H', 'h'] then GetToss := heads
    else GetToss := tails
End;

Procedure ShowSequence(tosses: Sequence);
{ Outputs three coin tosses at once }
Var
    i: integer;
Begin
    for i := 1 to 3 do PutToss(tosses[i])
End;

Procedure ReadSequence(var tosses: Sequence);
{ Accepts three coin tosses from the keyboard }
Var i: integer;
Begin
    { Get the 3 letters }
    for i := 1 to 3 do tosses[i] := GetToss;
    { Ignore the rest of the line }
    readln
End;

Function Optimum(opponent: Sequence): Sequence;
{ Generates the optimum sequence against an opponent }
Begin
    case opponent[2] of
        heads: Optimum[1] := tails;
        tails: Optimum[1] := heads
    end;
    Optimum[2] := opponent[1];
    Optimum[3] := opponent[2]
End;

Function RandomSequence: Sequence;
{ Generates three random coin tosses }
Var
    i: integer;
Begin
    for i := 1 to 3 do RandomSequence[i] := TossCoin
End;

Function Match(first, second: Sequence): Boolean;
{ Detects whether a sequence of tosses matches another }
Var
    different: boolean;
    i: integer;
Begin
    different := false;
    i := 1;
    while (i <= 3) and not different do begin
        if not (first[i] = second[i]) then different := true;
        i := i + 1
    end;
    Match := not different
End;

Procedure PlayRound(var human, computer: Player);
{ Shows coin tosses and announces the winner }
Var
    { We only ever need to store the 3 most recent tosses in memory. }
    tosses: Sequence;
Begin
    { Start with the first three tosses }
    write('Tossing the coin: ');
    tosses := RandomSequence;
    ShowSequence(tosses);
    { Keep tossing the coin until there is a winner. }
    while not (Match(human.bet, tosses) or Match(computer.bet, tosses)) do begin
        tosses[1] := tosses[2];
        tosses[2] := tosses[3];
        tosses[3] := TossCoin;
        PutToss(tosses[3])
    end;
    { Update the winner's score and announce the winner }
    writeln;
    writeln;
    if Match(human.bet, tosses) then begin
        writeln('Congratulations! You won this round.');
        human.score := human.score + 1;
        writeln('Your new score is ', human.score, '.')
    end
    else begin
        writeln('Yay, I won this round!');
        computer.score := computer.score + 1;
        writeln('My new score is ', computer.score, '.')
    end
End;

{ Main Algorithm }

BEGIN

    { Welcome the player }
    writeln('Welcome to Penney''s game!');
    writeln;
    write('How many rounds would you like to play? ');
    readln(Rounds);
    writeln;
    writeln('Ok, let''s play ', Rounds, ' rounds.');

    { Start the game }
    randomize;
    Human.score := 0;
    Computer.score := 0;

    for Count := 1 to Rounds do begin

        writeln;
        writeln('*** Round #', Count, ' ***');
        writeln;

        { Choose someone randomly to pick the first sequence }
        if TossCoin = heads then begin
            write('I''ll pick first this time.');
            Computer.bet := RandomSequence;
            write(' My sequence is ');
            ShowSequence(Computer.bet);
            writeln('.');
            repeat
                write('What sequence do you want? ');
                ReadSequence(Human.bet);
                if Match(Human.bet, Computer.bet) then
                    writeln('Hey, that''s my sequence! Think for yourself!')
            until not Match(Human.bet, Computer.bet);
            ShowSequence(Human.bet);
            writeln(', huh? Sounds ok to me.')
        end
        else begin
            write('You pick first this time. Enter 3 letters H or T: ');
            ReadSequence(Human.bet);
            Computer.bet := Optimum(Human.bet);
            write('Ok, so you picked ');
            ShowSequence(Human.bet);
            writeln;
            write('My sequence will be ');
            ShowSequence(Computer.bet);
            writeln
        end;

        { Then we can actually play the round }
        writeln('Let''s go!');
        writeln;
        PlayRound(Human, Computer);
        writeln;
        writeln('Press ENTER to go on...');
        readln

    end;

    { All the rounds are finished; time to decide who won }
    writeln;
    writeln('*** End Result ***');
    writeln;
    if Human.score > Computer.score then writeln('Congratulations! You won!')
    else if Computer.score > Human.score then writeln('Hooray! I won')
    else writeln('Cool, we tied.');
    writeln;
    writeln('Press ENTER to finish.');
    readln

END.
```


{{out}}

```txt
Welcome to Penney's game!

How many rounds would you like to play?  3

Ok, let's play 3 rounds.

*** Round #1 ***

I'll pick first this time. My sequence is TTH.
What sequence do you want? H tt
HTT, huh? Sounds ok to me.
Let's go!

Tossing the coin: HHHHTHHHHHHTHHHHTHHTHTHTT

Congratulations! You won this round.
Your new score is 1.

Press ENTER to go on...


*** Round #2 ***

I'll pick first this time. My sequence is THT.
What sequence do you want? tth
TTH, huh? Sounds ok to me.
Let's go!

Tossing the coin: THT

Yay, I won this round!
My new score is 1.

Press ENTER to go on...


*** Round #3 ***

You pick first this time. Enter 3 letters H or T: h t h
Ok, so you picked HTH
My sequence will be HHT
Let's go!

Tossing the coin: HHT

Yay, I won this round!
My new score is 2.

Press ENTER to go on...


*** End Result ***

Hooray! I won

Press ENTER to finish.

```



## Perl


```perl

#!usr/bin/perl
use 5.020;
use strict;
use warnings;

#Choose who goes first
binaryRand() == 0 ? flipCoin(userFirst()) : flipCoin(compFirst());

#Return a randomly generated 1 or 0 
sub binaryRand
{
    return int(rand(2));
}
#Converts 1's and 0's to H's and T's, respectively.
sub convert
{
    my $randNum = binaryRand();
    if($randNum == 0)
    {
        return "T"
    }
    else
    {
        return "H";
    }
}

#Prompts for and returns a user's sequence of 3
sub uSeq
{
    print("Please enter a sequence of 3 of \"H\" and \"T\". EG: HHT\n>");
    my $uString = <STDIN>;

    while(1)
    {
        #Make it uppercase and validate input
        chomp($uString);
        $uString = uc $uString;
        #Check length and content (H's and T's only!)
        if(length $uString == 3 && (substr($uString, 0, 1) =~ /[HT]/ &&
                                    substr($uString, 1, 1) =~ /[HT]/ &&
                                    substr($uString, 2, 1) =~ /[HT]/))
        {
            last;
        }
        else
        {
            print("Error, try again. \n");
            print("Please enter a sequence of 3 of \"H\" and \"T\". EG: HHT\n");
            $uString = <STDIN>;
        }
    }
    return $uString;
}

#Returns an array with two elements: [0] user's seq, [1] random computer seq.
sub compFirst
{
    my $cSeq;
    #Randomly draw a sequence of 3
    for(my $i = 0; $i < 3; $i++)
    {
        $cSeq = $cSeq . convert();
    }

    print("The computer guesses first:\ncomp- $cSeq\n");
    my $uSeq = uSeq();
    print("user- $uSeq\n");
    my @seqArr = ($uSeq, $cSeq);
    return @seqArr;
}

#Returns an array with two elements: [0] user's seq, [1] optimal computer seq.
sub userFirst
{
    print("The user quesses first:\n");
    my $uSeq = uSeq();
    my $cSeq;
    #Generate the optimal sequence based on $uSeq
    my $middle = substr($uSeq, 1, 1);
    $middle eq "H" ? $cSeq = "T" : $cSeq = "H";
    $cSeq = $cSeq . substr($uSeq, 0, 2); 

    print("user- $uSeq\ncomp- $cSeq\n");
    my @seqArr = ($uSeq, $cSeq);
    return @seqArr; 
}

#Flips a coin, checking both sequences against the contents of the given array
sub flipCoin
{
    my ($uSeq, $cSeq) = @_;
    my $coin;
    while(1)
    {
        $coin = $coin . convert();
        if($coin =~ m/$uSeq/)
        {
            print("The sequence of tosses was: $coin\n");
            say("The player wins! ");
            last;
        }
        elsif($coin =~ m/$cSeq/)
        {
            print("The sequence of tosses was: $coin\n");
            say("The computer wins! ");
            last;
        }
    }
}

```

{{out}}

```txt

The computer guesses first:
comp- HHH
Please enter a sequence of 3 of "H" and "T". EG: HHT
>thh
user- THH
The sequence of tosses was: HTTHTTHTHTHTTTHTTTHH
The player wins! 

### ==========================================

The user quesses first:
Please enter a sequence of 3 of "H" and "T". EG: HHT
>hht
user- HHT
comp- HHH
The sequence of tosses was: THHH
The computer wins! 

```



## Perl 6

{{works with|Rakudo|2018.02}}


```perl6>enum Coin <Heads Tails
;
enum Yay <Yay Good Super Hah Ooh Yipee Sweet Cool Yes Haha>;
enum Boo <Drat Darn Crumb Oops Rats Bah Criminy Argh Shards>;
enum Bozo «'Dude' 'Cha' 'Bzzt' 'Hey' 'Silly dilly' 'Say what!?' 'You numbskull'»;

sub flipping {
    for 1..4 {
        print "-\b";  sleep .1;
        print "\\\b"; sleep .1;
        print "|\b";  sleep .1;
        print "/\b";  sleep .1;
    }
}
 
sub your-choice($p is copy) {
    loop (my @seq; @seq != 3; $p = "{Bozo.pick}! Please pick exactly 3: ") {
        @seq = prompt($p).uc.comb(/ H | T /).map: {
            when 'H' { Heads }
            when 'T' { Tails }
        }
    }
    @seq;
}
 
repeat until prompt("Wanna play again? ").lc ~~ /^n/ {
    my $mefirst = Coin.roll;
    print tc "$mefirst I start, {Coin(+!$mefirst).lc} you start, flipping...\n\t";
    flipping;
    say my $flip = Coin.roll;

    my @yours;
    my @mine;

    if $flip == $mefirst {
        print "{Yay.pick}! I get to choose first, and I choose: "; sleep 2; say @mine = |Coin.roll(3);
        @yours = your-choice("Now you gotta choose: ");
        while @yours eqv @mine {
            say "{Bozo.pick}! We'd both win at the same time if you pick that!";
            @yours = your-choice("Pick something different from me: ");
        }
        say "So, you'll win if we see: ", @yours;
    }
    else {
        @yours = your-choice("{Boo.pick}! First you choose: ");
        say "OK, you'll win if we see: ", @yours;
        print "In that case, I'll just randomly choose: "; sleep 2; say @mine = Coin(+!@yours[1]), |@yours[0,1];
    }
     
    sub check($a,$b,$c) {
        given [$a,$b,$c] {
            when @mine  { say "\n{Yay.pick}, I win, and you lose!"; Nil }
            when @yours { say "\n{Boo.pick}, you win, but I'll beat you next time!"; Nil }
            default     { Coin.roll }
        }
    }

    sleep 1;
    say < OK! Ready? Right... So... Yo!>.pick;
    sleep .5;
    say ("Pay attention now!",
        "Watch closely!",
        "Let's do it...",
        "You feeling lucky?",
        "No way you gonna win this...",
        "Can I borrow that coin again?").pick;
    sleep 1;
    print "Here we go!\n\t";
    for |Coin.roll(3), &check ...^ :!defined {
        flipping;
        print "$_ ";
    }
}
```

{{out}}
Note: the actual run displays a little coin-flipping animation, but that won't show up here:

```txt
Heads I start, tails you start, flipping...
	Heads
Yipee! I get to choose first, and I choose: Heads Heads Tails
Now you gotta choose: tth
So, you'll win if we see: Tails Tails Heads
Yo!
Can I borrow that coin again?
Here we go!
	Tails Tails Tails Tails Tails Heads 
Argh, you win, but I'll beat you next time!
Wanna play again? y
Tails I start, heads you start, flipping...
	Tails
Yes! I get to choose first, and I choose: Heads Tails Tails
Now you gotta choose: H T T
Dude! We'd both win at the same time if you pick that!
Pick something different from me: heads tails tails
Silly dilly! We'd both win at the same time if you pick that!
Pick something different from me: h,h,h
So, you'll win if we see: Heads Heads Heads
OK!
You feeling lucky?
Here we go!
	Tails Tails Tails Heads Heads Heads 
Drat, you win, but I'll beat you next time!
Wanna play again? y
Heads I start, tails you start, flipping...
	Tails
Shards! First you choose: tht
OK, you'll win if we see: Tails Heads Tails
In that case, I'll just randomly choose: Tails Tails Heads
Right...
Pay attention now!
Here we go!
	Heads Tails Tails Heads 
Hah, I win, and you lose!
Wanna play again? n
```



## Phix

{{trans|C}}
Robert's robot's name is Robort.

```Phix
function trio(integer pick)
    return substitute_all(sprintf("%03b",pick),"10","HT")
end function

function getuser(integer bot)
integer user
    while 1 do
        user = 8 -- (a bit that clears after 3 shifts)
        printf(1,"Enter your sequence of 3 (H/T):");
        while user>7 do
            integer c = upper(wait_key())
            if c=#1B then abort(0) end if -- (Escape)
            if find(c,"HT") then
                puts(1,c)
                user = and_bits(user*2+(c='H'),0b111111)
            end if
        end while
        if user!=bot then exit end if
        printf(1,"\nYou may not pick the same as Robort!\n")
    end while
    printf(1,"\n")
    return user
end function
 
function getbot(int user)
    int bot = iff(user=-1?rand(8)-1
                         :4-and_bits(user,2)*2+floor(user/2))
    printf(1,"Robort picks %s\n", {trio(bot)})
    return bot
end function
 
function rungame(integer user, bot)
    /* We only need to store the last 3 tosses, as 0..7 */
    int last3 = rand(8)-1
 
    printf(1,"Rolling: %s",{trio(last3)})
    while 1 do
        if user=last3 then
            printf(1,"\nUser wins!\n")
            return 1
        elsif bot=last3 then
            printf(1,"\nRobort wins!\n")
            return 0
        end if
        last3 = and_bits(last3,3)*2+(rand(2)=1)
        printf(1,"%c", iff(and_bits(last3,1) ? 'H' : 'T'))
        sleep(0.5)
    end while
end function
 
procedure main()
    integer playerwins = 0,
            totalgames = 0,
            robortwins = 0
 
    /* Just use ctrl-c or Escape to exit */
    while 1 do
        integer user = -1,
                bot  = -1
 
        printf(1,"\n")
        if rand(2)=1 then
            bot  = getbot(-1)
            user = getuser(bot)
        else
            user = getuser(-1)
            bot  = getbot(user)
        end if
 
        playerwins += rungame(user, bot)
        totalgames += 1
        robortwins = totalgames-playerwins
 
        printf(1,"Robort:%d  You:%d  out of %d games\n", 
                 {robortwins, playerwins, totalgames})
        printf(1,"
### ============================
\n")
    end while
 
end procedure
main()
```

{{out}}

```txt

Robort picks TTT
Enter your sequence of 3 (H/T):TTT
You may not pick the same as Robort!
Enter your sequence of 3 (H/T):HHH
Rolling: HTTT
Robort wins!
Robort:1  You:0  out of 1 games

### ============================


Enter your sequence of 3 (H/T):HTH
Robort picks HHT
Rolling: THTTTHHHHHHT
Robort wins!
Robort:2  You:0  out of 2 games

### ============================


Enter your sequence of 3 (H/T):TTH
Robort picks HTT
Rolling: THTT
Robort wins!
Robort:3  You:0  out of 3 games

### ============================


Enter your sequence of 3 (H/T):HTH
Robort picks HHT
Rolling: TTHHHHHHT
Robort wins!
Robort:4  You:0  out of 4 games

### ============================


Enter your sequence of 3 (H/T):HHT
Robort picks THH
Rolling: TTHH
Robort wins!
Robort:5  You:0  out of 5 games

### ============================


Enter your sequence of 3 (H/T):THH
Robort picks TTH
Rolling: THH
User wins!
Robort:5  You:1  out of 6 games

### ============================


```



## PicoLisp


```PicoLisp
(seed (in "/dev/urandom" (rd 8)))

(setq *S (list 0 0))

(de toss (N)
   (make
      (do N
         (link (if (rand T) "T" "H")) ) ) )

(de comp (A Lst)
   (or
      (for ((I . L) Lst (cddr L) (cdr L))
         (T (fully = A L) I) )
      T ) )

(de score NIL
   (prinl)
   (prinl "Total score:")
   (prinl "^Iuser: " (car *S))
   (prinl "^Icomp: " (cadr *S)) )

(de play NIL
   (let (C (toss 3)  Lst (toss (rand 5 12))  U)
      (prinl)
      (prin "Select toss: ")
      (setq U (in NIL (skip) (line)))
      (prinl "Comp toss: " C)
      (prinl "Toss: " Lst)
      (setq @ (comp U Lst)  @@ (comp C Lst) )
      (cond
         ((< @ @@) (prinl "User win.") (inc *S))
         ((> @ @@) (prinl "Comp win.") (inc (cdr *S)))
         (T (prinl "Draw, lets play again.")) )
      (score) ) )

(de go NIL
   (loop
      (play)
      (T
         (prog
            (prinl)
            (prin "Want play again? Y/N: ")
            (= "N" (uppc (in NIL (char)))) ) ) ) )

(go)
```

{{out}}

```txt

Select toss: HTH
Comp toss: THT
Toss: HHTHTTHT
User win.

Total score:
        user: 1
        comp: 0

Want play again? Y/N: y

Select toss: HHH
Comp toss: HTH
Toss: THHHTTHTHHH
User win.

Total score:
        user: 2
        comp: 0

Want play again? Y/N: y

Select toss: TTT
Comp toss: THH
Toss: TTHTTHHHHTH
Comp win.

Total score:
        user: 2
        comp: 1

Want play again? Y/N: n

```


=={{Header|Prolog}}==

```prolog
play :- rand1(R), game(R).

game(h) :-
    format('Your turn first!~n'), player_move(P),
    response(P,C), format('I am choosing '), maplist(writec, C), nl,
    rand3(R), maplist(writec,R),
    roll(P, C, R).

game(t) :-
    rand3(C),
    format('I am choosing '), maplist(writec, C), nl,
    player_move(P),
    rand3(R), maplist(writec, R),
    roll(P, C, R).

player_move([P1,P2,P3]) :-
    read_line_to_codes(user_input,Codes),
    maplist(char_code,[P1,P2,P3],Codes).

roll(P, _, P) :- format('~nYou Win!~n'), !.
roll(_, C, C) :- format('~nI Win!~n'), !.

roll(P, C, [_,A,B]) :-
    rand1(R),
    coin_s(R,S),
    write(S),
    roll(P,C,[A,B,R]).

response([A,B,_], [C,A,B]) :- opp(A,C).

writec(A) :- coin_s(A,A1), write(A1).
rand1(R) :- random(V), round(V,I), coin(I,R).
rand3([R1,R2,R3]) :- rand1(R1), rand1(R2), rand1(R3).

coin(0,h). coin(1,t).
coin_s(h, 'H'). coin_s(t, 'T').
opp(h, t). opp(t, h).
```

 
Output: 

```txt

?- play.
I am choosing HTT
|: tth
HHTHHHHHHHHTHHHHTHTHTHTHTHHTT
I Win!
true.

?- play.
Your turn first!
|: hht
I am choosing THH
TTHTHH
I Win!

```


=={{Header|Python}}==

```python
from __future__ import print_function
import random
from time import sleep

first = random.choice([True, False])

you = ''
if first:
    me = ''.join(random.sample('HT'*3, 3))
    print('I choose first and will win on first seeing {} in the list of tosses'.format(me))
    while len(you) != 3 or any(ch not in 'HT' for ch in you) or you == me:
        you = input('What sequence of three Heads/Tails will you win with: ')
else:
    while len(you) != 3 or any(ch not in 'HT' for ch in you):
        you = input('After you: What sequence of three Heads/Tails will you win with: ')
    me = ('H' if you[1] == 'T' else 'T') + you[:2]
    print('I win on first seeing {} in the list of tosses'.format(me))
    
print('Rolling:\n  ', end='')
rolled = ''
while True:
    rolled += random.choice('HT')
    print(rolled[-1], end='')
    if rolled.endswith(you):
        print('\n  You win!')
        break
    if rolled.endswith(me):
        print('\n  I win!')
        break
    sleep(1)    # For dramatic effect
```


{{out}}

```txt
>>> 
After you: What sequence of three Heads/Tails will you win with: TTH
I win on first seeing HTT in the list of tosses
Rolling:
  THHTHHTT
  I win!
>>> 
### ============================= RESTART =============================

>>> 
I choose first and will win on first seeing HHT in the list of tosses
What sequence of three Heads/Tails will you win with: THH
Rolling:
  HTHTHTTTHTTTTTTHH
  You win!
>>> 
```



## R


```rsplus
#
### =========================================================

# Penney's Game Task from Rosetta Code Wiki
# R implementation
#
### =========================================================


penneysgame <- function() {
  
  #---------------------------------------------------------------
  # Who goes first?
  #---------------------------------------------------------------
  
  first <- sample(c("PC", "Human"), 1)
  
  #---------------------------------------------------------------
  # Determine the sequences
  #---------------------------------------------------------------
  
  if (first == "PC") { # PC goes first
    
    pc.seq <- sample(c("H", "T"), 3, replace = TRUE)
    cat(paste("\nI choose first and will win on first seeing", paste(pc.seq, collapse = ""), "in the list of tosses.\n\n"))
    human.seq <- readline("What sequence of three Heads/Tails will you win with: ")
    human.seq <- unlist(strsplit(human.seq, ""))
    
  } else if (first == "Human") { # Player goest first
    
    cat(paste("\nYou can choose your winning sequence first.\n\n"))
    human.seq <- readline("What sequence of three Heads/Tails will you win with: ")
    human.seq <- unlist(strsplit(human.seq, "")) # Split the string into characters
    pc.seq <- c(human.seq[2], human.seq[1:2]) # Append second element at the start
    pc.seq[1] <- ifelse(pc.seq[1] == "H", "T", "H") # Switch first element to get the optimal guess
    cat(paste("\nI win on first seeing", paste(pc.seq, collapse = ""), "in the list of tosses.\n"))
    
  }
  
  #---------------------------------------------------------------
  # Start throwing the coin
  #---------------------------------------------------------------
  
  cat("\nThrowing:\n")
  
  ran.seq <- NULL
  
  while(TRUE) {
    
    ran.seq <- c(ran.seq, sample(c("H", "T"), 1)) # Add a new coin throw to the vector of throws
    
    cat("\n", paste(ran.seq, sep = "", collapse = "")) # Print the sequence thrown so far
    
    if (length(ran.seq) >= 3 && all(tail(ran.seq, 3) == pc.seq)) {
      cat("\n\nI win!\n")
      break
    }
    
    if (length(ran.seq) >= 3 && all(tail(ran.seq, 3) == human.seq)) {
      cat("\n\nYou win!\n")
      break
    }
    
    Sys.sleep(0.5) # Pause for 0.5 seconds
    
  }
}

```

{{out}}
Human first.


```txt

You can choose your winning sequence first.

What sequence of three Heads/Tails will you win with: THH

I win on first seeing TTH in the list of tosses.

Throwing:

 T
 TT
 TTT
 TTTH

I win!

```

PC first


```txt

I choose first and will win on first seeing THH in the list of tosses.

What sequence of three Heads/Tails will you win with: TTH

Throwing:

 T
 TH
 THT
 THTT
 THTTH

You win!

```



## Racket

This does what's required of it from the task... just don't input anything outside the alphabet "htHT" for the human move.


```racket
#lang racket
;; Penney's Game. Tim Brown 2014-10-15
(define (flip . _) (match (random 2) (0 "H") (1 "T")))

(define (get-human-sequence) ; no sanity checking here!
  (display "choose your winning sequence of 3 H or T > ")
  (drop-right (drop (string-split (string-upcase (read-line)) "") 1) 1))

(define flips->string (curryr string-join "."))

(define (game-on p1 p2)
  (printf "~a chooses: ~a. ~a chooses: ~a~%"
          (car p1) (flips->string (cdr p1)) (car p2) (flips->string (cdr p2)))
  (match-define (list (list name.1 p1.1 p1.2 p1.3) (list name.2 p2.1 p2.2 p2.3)) (list p1 p2))
  (let turn ((seq null))
    (match seq
      [(list-rest (== p1.3) (== p1.2) (== p1.1) _) name.1]
      [(list-rest (== p2.3) (== p2.2) (== p2.1) _) name.2]
      [else
       (let* ((flp (flip)) (seq+ (cons flp else)))
         (printf "new-flip: ~a -> ~a~%" flp (flips->string (reverse seq+))) (turn seq+))])))

(define (play-game)
  (define-values
    (player-1 player-2)
    (match (flip)
      ["H" (printf "Human chooses first: ")
           (define p1 (cons 'Hom-Sap (get-human-sequence)))
           (values p1 (cons 'Computer
                            (match p1
                              [(list _ f1 (and f2 (app (match-lambda ("H" "T") ("T" "H")) ¬f2)) _)
                               (list ¬f2 f1 f2)])))]
      ["T" (printf "Computer chooses first. ")
           (define p1 (cons 'Computer (build-list 3 flip)))
           (printf "~a chooses: ~a~%" (car p1) (flips->string (cdr p1)))
           (values p1 (cons 'Hom-Sap (get-human-sequence)))]))
  (printf "~a wins!~%" (game-on player-1 player-2)))
```


{{out}}
===Homo Sapiens Plays First (and wins!)===

```txt

> (play-game)
Human chooses first: choose your winning sequence of 3 H or T > hth
Hom-Sap chooses: H.T.H. Computer chooses: H.H.T
new-flip: H -> H
new-flip: T -> H.T
new-flip: H -> H.T.H
Hom-Sap wins!

```


===The Computer Plays First (and loses!)===

```txt

> (play-game)
Computer chooses first. Computer chooses: T.T.T
choose your winning sequence of 3 H or T > htt
Computer chooses: T.T.T. Hom-Sap chooses: H.T.T
new-flip: H -> H
new-flip: H -> H.H
new-flip: H -> H.H.H
new-flip: T -> H.H.H.T
new-flip: H -> H.H.H.T.H
new-flip: T -> H.H.H.T.H.T
new-flip: H -> H.H.H.T.H.T.H
new-flip: T -> H.H.H.T.H.T.H.T
new-flip: H -> H.H.H.T.H.T.H.T.H
new-flip: T -> H.H.H.T.H.T.H.T.H.T
new-flip: T -> H.H.H.T.H.T.H.T.H.T.T
Hom-Sap wins!

```

(Nail-biting stuff!)


## REXX

The REXX program keeps a running score   (number of wins out of so many games played)   as well as 

allowing the human to pick the number (length) of the coin toss sequence.

A fair amount of code was added to ensure a valid response from the human player as well as give an informative error message.

The human player is allowed to spell out the   '''H'''   or   '''T'''   (as '''heads''' or '''tails''').

A feature also added was to allow a seed for the   '''random'''   BIF to allow repeatability for a game.  

```rexx
/*REXX program plays/simulates  Penney's Game,  a  two-player  coin toss sequence game. */
__=copies('─',9)                                 /*literal for eyecatching fence.       */
signal on halt                                   /*a clean way out if  CLBF  quits.     */
parse arg # seed .                               /*obtain optional arguments from the CL*/
if #==''  | #==","     then #=3                  /*Not specified?  Then use the default.*/
if datatype(seed,'W')  then call random ,,seed   /*use seed for RANDOM #s repeatability.*/
wins=0;    do games=1                            /*simulate a number of Penney's games. */
           call game                             /*simulate a single inning of a game.  */
           end   /*games*/                       /*keep at it until  QUIT  or  halt.    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
halt:  say;   say __  "Penney's Game was halted.";   say;   exit 13
r:     arg ,$;       do  arg(1);    $=$ || random(0,1);   end;          return $
s:     if arg(1)==1  then return arg(3);   return word(arg(2) 's',1)       /*pluralizer.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
game:  @.=;  tosses=@.                                 /*the coin toss sequence so far. */
       toss1=r(1)                                      /*result:   0=computer,   1=CBLF.*/
       if \toss1  then call randComp                   /*maybe let the computer go first*/
       if  toss1  then say __ "You win the first toss, so you pick your sequence first."
                  else say __ "The computer won first toss, the pick was: "    @.comp
                       call prompter                   /*get the human's guess from C.L.*/
                       call randComp                   /*get computer's guess if needed.*/
                                                       /*CBLF:  carbon-based life form. */
       say __  "      your pick:"  @.CBLF              /*echo human's pick to terminal. */
       say __  "computer's pick:"  @.comp              /*  "  comp.'s   "   "     "     */
       say                                             /* [↓]  flip the coin 'til a win.*/
             do  flips=1  until pos(@.CBLF,tosses)\==0  |  pos(@.comp,tosses)\==0
             tosses=tosses || translate(r(1),'HT',10)
             end   /*flips*/                           /* [↑]   this is a flipping coin,*/
                                                       /* [↓] series of tosses*/
       say __ "The tossed coin series was: "   tosses
       say
       @@@="won this toss with "   flips   ' coin tosses.'
       if pos(@.CBLF,tosses)\==0  then do;  say __  "You"  @@@;  wins=wins+1;  end
                                  else      say __  "The computer"  @@@
       _=wins;  if _==0  then _='no'
       say __ "You've won"  _  "game"s(wins)  'out of ' games"."
       say;  say copies('╩╦',79%2)'╩';  say
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
prompter: oops=__ 'Oops!  ';            a=             /*define some handy REXX literals*/
          @a_z='ABCDEFG-IJKLMNOPQRS+UVWXYZ'            /*the extraneous alphabetic chars*/
          p=__ 'Pick a sequence of'  #  "coin tosses of  H or T (Heads or Tails) or Quit:"
             do  until  ok;   say;  say p;  pull a     /*uppercase the answer.          */
             if abbrev('QUIT',a,1)  then exit 1        /*the human wants to  quit.      */
             a=space(translate(a,,@a_z',./\;:_'),0)    /*elide extraneous characters.   */
             b=translate(a,10,'HT');    L=length(a)    /*translate ───► bin; get length.*/
             ok=0                                      /*the response is  OK  (so far). */
                 select                                /*verify the user response.      */
                 when \datatype(b,'B') then say oops "Illegal response."
                 when \datatype(a,'M') then say oops "Illegal characters in response."
                 when L==0             then say oops "No choice was given."
                 when L<#              then say oops "Not enough coin choices."
                 when L>#              then say oops "Too many coin choices."
                 when a==@.comp        then say oops "You can't choose the computer's choice: " @.comp
                 otherwise         ok=1
                 end   /*select*/
             end       /*until ok*/
          @.CBLF=a;           @.CBLF!=b                /*we have the human's guess now. */
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
randComp: if @.comp\==''  then return                  /*the computer already has a pick*/
          _=@.CBLF!                                    /* [↓] use best-choice algorithm.*/
          if _\==''  then g=left((\substr(_, min(2, #), 1))left(_, 1)substr(_, 3), #)
            do  until g\==@.CBLF!;   g=r(#);   end     /*otherwise, generate a choice.  */
          @.comp=translate(g, 'HT', 10)
          return
```

'''output'''   of a six-game session   (ended by user entering a '''quit'''): 

```txt

───────── The computer won first toss, the pick was:  HHH

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
tail tail tails          ◄■■■■■■■■■■■ human's input

─────────       your pick: TTT
───────── computer's pick: HHH

───────── The computer won this toss with  12  coin tosses.

───────── The tossed coin series was:  HHTTHHTHTHHH

───────── You've won no games out of  1.

╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩

───────── You win the first toss, so you pick your sequence first.

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
h, h, t                  ◄■■■■■■■■■■■ human's input

─────────       your pick: HHT
───────── computer's pick: THT

───────── The computer won this toss with  5  coin tosses.

───────── The tossed coin series was:  TTTHT

───────── You've won no games out of  2.

╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩

───────── The computer won first toss, the pick was:  HHT

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
heads heads heads        ◄■■■■■■■■■■■ human's input

─────────       your pick: HHH
───────── computer's pick: HHT

───────── The computer won this toss with  3  coin tosses.

───────── The tossed coin series was:  HHT

───────── You've won no games out of  3.

╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩

───────── The computer won first toss, the pick was:  HTH

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
t t h                    ◄■■■■■■■■■■■ human's input 

─────────       your pick: TTH
───────── computer's pick: HTH

───────── You won this toss with  3  coin tosses.

───────── The tossed coin series was:  TTH

───────── You've won 1 game out of  4.

╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩

───────── You win the first toss, so you pick your sequence first.

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
t,h,h                    ◄■■■■■■■■■■■ human's input

─────────       your pick: THH
───────── computer's pick: HTH

───────── You won this toss with  4  coin tosses.

───────── The tossed coin series was:  TTHH

───────── You've won 2 games out of  5.

╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩

───────── The computer won first toss, the pick was:  TTH

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
tth                      ◄■■■■■■■■■■■ human's input
───────── Oops!   You can't choose the computer's choice:  TTH

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
tht                      ◄■■■■■■■■■■■ human's input

─────────       your pick: THT
───────── computer's pick: TTH

───────── The computer won this toss with  3  coin tosses.

───────── The tossed coin series was:  TTH

───────── You've won 2 games out of  6.

╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩╦╩

───────── You win the first toss, so you pick your sequence first.

───────── Pick a sequence of 3 coin tosses of  H or T (Heads or Tails) or Quit:
quit                     ◄■■■■■■■■■■■ human's input

```


=={{Header|Ruby}}==

```ruby
Toss = [:Heads, :Tails]

def yourChoice
  puts "Enter your choice (H/T)"
  choice = []
  3.times do
    until (c = $stdin.getc.upcase) == "H" or c == "T"
    end
    choice << (c=="H" ? Toss[0] : Toss[1])
  end
  puts "You chose #{choice.join(' ')}"
  choice
end

loop do
  puts "\n%s I start, %s you start ..... %s" % [*Toss, coin = Toss.sample]
  if coin == Toss[0]
    myC = Toss.shuffle << Toss.sample
    puts "I chose #{myC.join(' ')}"
    yC = yourChoice
  else
    yC = yourChoice
    myC = Toss - [yC[1]] + yC.first(2)
    puts "I chose #{myC.join(' ')}"
  end
  
  seq = Array.new(3){Toss.sample}
  print seq.join(' ')
  loop do
    puts "\n I win!" or break   if seq == myC
    puts "\n You win!" or break if seq == yC
    seq.push(Toss.sample).shift
    print " #{seq[-1]}"
  end
end
```


{{out}}

```txt

Heads I start, Tails you start ..... Tails
Enter your choice (H/T)
THT
You chose Tails Heads Tails
I chose Tails Tails Heads
Heads Tails Heads Heads Tails Tails Tails Heads
 I win!

Heads I start, Tails you start ..... Heads
I chose Heads Heads Heads
Enter your choice (H/T)
THH
You chose Tails Heads Heads
Heads Heads Tails Heads Tails Heads Tails Tails Tails Tails Tails Tails Tails Heads Heads
 You win!

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

oo::class create Player {
    variable who seq seen idx

    constructor {name sequence} {
	set who $name
	set seq $sequence
	set seen {}
	set idx end-[expr {[string length $seq] - 1}]
    }

    method pick {} {
	return $seq
    }

    method name {} {
	return $who
    }

    method match {digit} {
	append seen $digit
	return [expr {[string range $seen $idx end] eq $seq}]
    }
}

oo::class create HumanPlayer {
    superclass Player
    constructor {length {otherPlayersSelection ""}} {
	fconfigure stdout -buffering none
	while true {
	    puts -nonewline "What do you pick? (length $length): "
	    if {[gets stdin pick] < 0} exit
	    set pick [regsub -all {[^HT]} [string map {0 H 1 T h H t T} $pick] ""]
	    if {[string length $pick] eq $length} break
	    puts "That's not a legal pick!"
	}
	set name "Human"
	if {[incr ::humans] > 1} {append name " #$::humans"}
	next $name $pick
    }
}

oo::class create RobotPlayer {
    superclass Player
    constructor {length {otherPlayersSelection ""}} {
	if {$otherPlayersSelection eq ""} {
	    set pick ""
	    for {set i 0} {$i < $length} {incr i} {
		append pick [lindex {H T} [expr {int(rand()*2)}]]
	    }
	} else {
	    if {$length != 3} {
		error "lengths other than 3 not implemented"
	    }
	    lassign [split $otherPlayersSelection ""] a b c
	    set pick [string cat [string map {H T T H} $b] $a $b]
	}
	set name "Robot"
	if {[incr ::robots] > 1} {append name " #$::robots"}
	puts "$name picks $pick"
	next $name $pick
    }
}

proc game {length args} {
    puts "Let's play Penney's Game!"

    # instantiate the players
    set picks {}
    set players {}
    while {[llength $args]} {
	set idx [expr {int(rand()*[llength $args])}]
	set p [[lindex $args $idx] new $length {*}$picks]
	set args [lreplace $args $idx $idx]
	lappend players $p
	lappend picks [$p pick]
    }

    # sanity check
    if {[llength $picks] != [llength [lsort -unique $picks]]} {
	puts "Two players picked the same thing; that's illegal"
    }

    # do the game loop
    while 1 {
	set coin [lindex {H T} [expr {int(rand()*2)}]]
	puts "Coin flip [incr counter] is $coin"
	foreach p $players {
	    if {[$p match $coin]} {
		puts "[$p name] has won!"
		return
	    }
	}
    }
}

game 3 HumanPlayer RobotPlayer
```

{{out|Sample Game}}
<!-- Huh! This was a long one… -->

```txt

Let's play Penney's Game!
What do you pick? (length 3): HTH
Robot picks HHT
Coin flip 1 is T
Coin flip 2 is H
Coin flip 3 is T
Coin flip 4 is T
Coin flip 5 is T
Coin flip 6 is H
Coin flip 7 is T
Coin flip 8 is T
Coin flip 9 is H
Coin flip 10 is T
Coin flip 11 is T
Coin flip 12 is T
Coin flip 13 is H
Coin flip 14 is T
Coin flip 15 is T
Coin flip 16 is H
Coin flip 17 is T
Coin flip 18 is T
Coin flip 19 is H
Coin flip 20 is T
Coin flip 21 is H
Human has won!

```



## UNIX Shell

{{works with|Bash}}

```sh
#!/bin/bash
main() {
  echo "Penney's Game"
  echo -n "Flipping to see who goes first ... "

  if [[ $(flip) == H ]]; then
    echo "I do."
    p2=$(choose_sequence)
    echo "I choose: $p2"
  else
    echo "You do."
  fi

  while true; do
    echo "Enter your three-flip sequence:"
    read p1
    case "$p1" in
     "$p2") echo "Sequence must be different from mine";;
     [hHTt][hHtT][hHtT]) break;;
     *) echo "Sequence must be three H's or T's";;
    esac
  done
  p1=$(tr a-z A-Z <<<"$p1")

  if [ -z "$p2" ]; then 
    p2=$(choose_sequence "$p1")
    echo "I choose: $p2"
  fi

  echo
  echo "Here we go.  $p1, you win; $p2, I win."
  flips=

  while true; do
    flip=$(flip)
    echo -n $flip
    flips+=$flip
    while (( ${#flips} > 3 )); do
      flips="${flips#?}"
    done
    case "$flips" in
      *$p1) echo $'\nYou win!'; exit 0;;
      *$p2) echo $'\nI win!'; exit 1;;
    esac
  done
}

choose_sequence() {
  local result
  if (( $# )); then
    case "$1" in
      ?Hh?) result=T;;
      *) result=H;;
    esac
    result+="${1%?}"
  else
    result=$(flip)$(flip)$(flip)
  fi
  echo "$result"
}

flip() {
  if (( RANDOM % 2 )); then 
    echo H
  else
    echo T
  fi
}

main "$@"
```


{{Output}}
PC first:

```txt
Penney's Game
Flipping to see who goes first ... I do.
I choose: HHT
Enter your three-flip sequence:
THH

Here we go.  THH, you win; HHT, I win.
THH
You win!
```


Human first:

```txt
penney
Penney's Game
Flipping to see who goes first ... You do.
Enter your three-flip sequence:
HTH
I choose: HHT

Here we go.  HTH, you win; HHT, I win.
HHHT
I win!
```


## VBA


```vb

Option Explicit

Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Private Const HT As String = "H T"

Public Sub PenneysGame()
Dim S$, YourSeq$, ComputeSeq$, i&, Seq$, WhoWin$, flag As Boolean

    Do
        S = WhoWillBeFirst(Choice("Who will be first"))
        If S = "ABORT" Then Exit Do
        Debug.Print S; " start"
        YourSeq = Choice("Your sequence", 3)
        If YourSeq Like "*ABORT*" Then Exit Do
        Debug.Print "Your sequence : " & YourSeq
        ComputeSeq = vbNullString
        For i = 1 To 3
            ComputeSeq = ComputeSeq & Random
        Next i
        Debug.Print "Computer sequence : " & ComputeSeq
        Seq = vbNullString
        Do
            Seq = Seq & Random
            Debug.Print Seq
            Sleep 1000
        Loop While Not Winner(ComputeSeq, YourSeq, Seq, WhoWin)
        Debug.Print WhoWin; " win"
        If MsgBox(WhoWin & " win" & vbCrLf & "Play again?", vbYesNo) = vbNo Then flag = True
        Debug.Print ""
    Loop While Not flag
    Debug.Print "Game over"
End Sub

Private Function WhoWillBeFirst(YourChoice As String) As String
Dim S$
    S = Random
    Select Case YourChoice
        Case "ABORT": WhoWillBeFirst = YourChoice
        Case Else:
            WhoWillBeFirst = IIf(S = YourChoice, "You", "Computer")
    End Select
End Function

Private Function Choice(Title As String, Optional Seq As Integer) As String
Dim S$, i&, t$
    If Seq = 0 Then Seq = 1
    t = Title
    For i = 1 To Seq
        S = vbNullString
        Do
            S = InputBox("Choose between H or T : ", t)
            If StrPtr(S) = 0 Then S = "Abort"
            S = UCase(S)
        Loop While S <> "H" And S <> "T" And S <> "ABORT"
        Choice = Choice & S
        t = Title & " " & Choice
        If Choice Like "*ABORT*" Then Exit For
    Next i
End Function

Private Function Random() As String
Randomize Timer
    Random = Split(HT, " ")(CInt(Rnd))
End Function

Private Function Winner(Cs$, Ys$, S$, W$) As Boolean
    If Len(S) < 3 Then
        Winner = False
    Else
        If Right(S, 3) = Cs And Right(S, 3) = Ys Then
            Winner = True
            W = "Computer & you"
        ElseIf Right(S, 3) = Cs And Right(S, 3) <> Ys Then
            Winner = True
            W = "Computer"
        ElseIf Right(S, 3) = Ys And Right(S, 3) <> Cs Then
            Winner = True
            W = "You"
        End If
    End If
End Function
```

{{out}}

```txt
You start
Your sequence : HHT
Computer sequence : TTH
H
HT
HTT
HTTH
Computer win

You start
Your sequence : TTH
Computer sequence : HHH
T
TT
TTT
TTTH
You win

Game over
```



## zkl


```zkl
fcn coinToss{ (0).random(2) and "H" or "T" } // (0).random(2) --> 0<=r<2
reg myBet, yourBet;
if(coinToss()=="H"){ // The toss says I go first
   myBet=(3).pump(String,coinToss);
   println("I bet ",myBet);
   yourBet=ask("Your bet of three (H/T): ")[0,3].toUpper();
}else{
   yourBet=ask("Your bet of three (H/T): ")[0,3].toUpper();
   myBet=((yourBet[1]=="H") and "T" or "H") + yourBet[0,2];
   println("I bet ",myBet);
}
print("Flipping: "); coins:="";
while(1){
   print(toss:=coinToss()); coins=coins + toss;
   if(Void!=coins.find(yourBet)){ println(" You win!"); break; }
   if(Void!=coins.find(myBet))  { println(" I win!"); break; }
   // ignore we both won
}
```

{{out}}

```txt

$ zkl bbb
Your bet of three (H/T): tth
I bet HTT
Flipping: HHHHHHTHHTHHHTHTHHHTHTHTT I win!

$ zkl bbb
I bet THT
Your bet of three (H/T): hhh
Flipping: THHH You win!

```


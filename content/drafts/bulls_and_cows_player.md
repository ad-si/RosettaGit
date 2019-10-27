+++
title = "Bulls and cows/Player"
description = ""
date = 2019-03-09T20:11:42Z
aliases = []
[extra]
id = 7508
[taxonomies]
categories = []
tags = []
+++

{{task|Games}}

;Task:
Write a ''player'' of the [[Bulls and cows|Bulls and Cows game]], rather than a scorer. The player should give intermediate answers that respect the scores to previous attempts.

One method is to generate a list of all possible numbers that could be the answer, then to prune the list by keeping only those numbers that would give an equivalent score to how your last guess was scored. Your next guess can be any number from the pruned list.

Either you guess correctly or run out of numbers to guess, which indicates a problem with the scoring.


;Related tasks:
*   [[Bulls and cows]]
*   [[Guess the number]]
*   [[Guess the number/With Feedback (Player)]]





## Ada

{{works with|Ada 2005}}

bulls_player.adb:

```Ada
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Discrete_Random;

procedure Bulls_Player is

   -- package for In-/Output of natural numbers
   package Nat_IO is new Ada.Text_IO.Integer_IO (Natural);

   -- for comparing length of the vectors
   use type Ada.Containers.Count_Type;

   -- number of digits
   Guessing_Length : constant := 4;

   -- digit has to be from 1 to 9
   type Digit is range 1 .. 9;
   -- a sequence has specified length of digits
   type Sequence is array (1 .. Guessing_Length) of Digit;

   -- data structure to store the possible answers
   package Sequence_Vectors is new Ada.Containers.Vectors
     (Element_Type => Sequence,
      Index_Type   => Positive);

   -- check if sequence contains each digit only once
   function Is_Valid (S : Sequence) return Boolean is
      Appeared : array (Digit) of Boolean := (others => False);
   begin
      for I in S'Range loop
         if Appeared (S (I)) then
            return False;
         end if;
         Appeared (S (I))  := True;
      end loop;
      return True;
   end Is_Valid;

   -- calculate all possible sequences and store them in the vector
   procedure Fill_Pool (Pool : in out Sequence_Vectors.Vector) is
      Finished : exception;
      -- count the sequence up by one
      function Next (S : Sequence) return Sequence is
         Result : Sequence := S;
         Index  : Positive := S'Last;
      begin
         loop
         -- overflow at a position causes next position to increase
            if Result (Index) = Digit'Last then
               Result (Index) := Digit'First;
               -- overflow at maximum position
               -- we have processed all possible values
               if Index = Result'First then
                  raise Finished;
               end if;
               Index := Index - 1;
            else
               Result (Index) := Result (Index) + 1;
               return Result;
            end if;
         end loop;
      end Next;
      X        : Sequence := (others => 1);
   begin
      loop
      -- append all valid values
         if Is_Valid (X) then
            Pool.Append (X);
         end if;
         X := Next (X);
      end loop;
   exception
      when Finished =>
         -- the exception tells us that we have added all possible values
         -- simply return and do nothing.
         null;
   end Fill_Pool;

   -- generate a random index from the pool
   function Random_Index (Pool : Sequence_Vectors.Vector) return Positive is
      subtype Possible_Indexes is Positive range
        Pool.First_Index .. Pool.Last_Index;
      package Index_Random is new Ada.Numerics.Discrete_Random
        (Possible_Indexes);
      Index_Gen : Index_Random.Generator;
   begin
      Index_Random.Reset (Index_Gen);
      return Index_Random.Random (Index_Gen);
   end Random_Index;

   -- get the answer from the player, simple validity tests
   procedure Get_Answer (S : Sequence; Bulls, Cows : out Natural) is
      Valid : Boolean := False;
   begin
      Bulls := 0;
      Cows  := 0;
      while not Valid loop
         -- output the sequence
         Ada.Text_IO.Put ("How is the score for:");
         for I in S'Range loop
            Ada.Text_IO.Put (Digit'Image (S (I)));
         end loop;
         Ada.Text_IO.New_Line;
         begin
            Ada.Text_IO.Put ("Bulls:");
            Nat_IO.Get (Bulls);
            Ada.Text_IO.Put ("Cows:");
            Nat_IO.Get (Cows);
            if Bulls + Cows <= Guessing_Length then
               Valid := True;
            else
               Ada.Text_IO.Put_Line ("Invalid answer, try again.");
            end if;
         exception
            when others =>
               null;
         end;
      end loop;
   end Get_Answer;

   -- remove all sequences that wouldn't give an equivalent score
   procedure Strip
     (V           : in out Sequence_Vectors.Vector;
      S           : Sequence;
      Bulls, Cows : Natural)
   is
      function Has_To_Be_Removed (Position : Positive) return Boolean is
         Testant    : constant Sequence := V.Element (Position);
         Bull_Score : Natural           := 0;
         Cows_Score : Natural := 0;
      begin
         for I in Testant'Range loop
            for J in S'Range loop
               if Testant (I) = S (J) then
                  -- same digit at same position: Bull!
                  if I = J then
                     Bull_Score := Bull_Score + 1;
                  else
                     Cow_Score := Cow_Score + 1;
                  end if;
               end if;
            end loop;
         end loop;
         return Cow_Score /= Cows or else Bull_Score /= Bulls;
      end Has_To_Be_Removed;
   begin
      for Index in reverse V.First_Index .. V.Last_Index loop
         if Has_To_Be_Removed (Index) then
            V.Delete (Index);
         end if;
      end loop;
   end Strip;

   -- main routine
   procedure Solve is
      All_Sequences : Sequence_Vectors.Vector;
      Test_Index    : Positive;
      Test_Sequence : Sequence;
      Bulls, Cows   : Natural;
   begin
      -- generate all possible sequences
      Fill_Pool (All_Sequences);
      loop
      -- pick at random
         Test_Index    := Random_Index (All_Sequences);
         Test_Sequence := All_Sequences.Element (Test_Index);
         -- ask player
         Get_Answer (Test_Sequence, Bulls, Cows);
         -- hooray, we have it!
         exit when Bulls = 4;
         All_Sequences.Delete (Test_Index);
         Strip (All_Sequences, Test_Sequence, Bulls, Cows);
         exit when All_Sequences.Length <= 1;
      end loop;
      if All_Sequences.Length = 0 then
         -- oops, shouldn't happen
         Ada.Text_IO.Put_Line
           ("I give up, there has to be a bug in" &
            "your scoring or in my algorithm.");
      else
         if All_Sequences.Length = 1 then
            Ada.Text_IO.Put ("The sequence you thought has to be:");
            Test_Sequence := All_Sequences.First_Element;
         else
            Ada.Text_IO.Put ("The sequence you thought of was:");
         end if;
         for I in Test_Sequence'Range loop
            Ada.Text_IO.Put (Digit'Image (Test_Sequence (I)));
         end loop;
      end if;
   end Solve;

begin
   -- output blah blah
   Ada.Text_IO.Put_Line ("Bulls and Cows, Your turn!");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line
     ("Think of a sequence of" &
      Integer'Image (Guessing_Length) &
      " different digits.");
   Ada.Text_IO.Put_Line ("I will try to guess it. For each correctly placed");
   Ada.Text_IO.Put_Line ("digit I score 1 Bull. For each digit that is on");
   Ada.Text_IO.Put_Line ("the wrong place I score 1 Cow. After each guess");
   Ada.Text_IO.Put_Line ("you tell me my score.");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Let's start.");
   Ada.Text_IO.New_Line;
   -- solve the puzzle
   Solve;
end Bulls_Player;
```


output:

```txt
Bulls and Cows, Your turn!

Think of a sequence of 4 different digits.
I will try to guess it. For each correctly placed
digit I score 1 Bull. For each digit that is on
the wrong place I score 1 Cow. After each guess
you tell me my score.

Let's start.

How is the score for: 8 1 7 5
Bulls:2
Cows:0
How is the score for: 4 1 6 5
Bulls:1
Cows:0
How is the score for: 3 9 7 5
Bulls:1
Cows:2
How is the score for: 9 1 7 3
Bulls:0
Cows:2
The sequence you thought has to be: 8 3 9 5
```



## AutoHotkey


```ahk
length:=4, i:=0, S:=P(9,length)

Gui, Add, Text, w83 vInfo, Think of a %length%-digit number with no duplicate digits.
Gui, Add, Edit, w40 vBulls
Gui, Add, Text, x+3, Bulls
Gui, Add, Edit, xm w40 vCows
Gui, Add, Text, x+3, Cows
Gui, Add, Button, xm w83 Default vDefault, Start
Gui, Add, Edit, ym w130 r8 vHistory ReadOnly
Gui, Show
Return

ButtonStart:
	If Default = Restart
		Reload
	Gui, Submit, NoHide
	GuiControl, Focus, Bulls
	If (Bulls = length)
	{
		GuiControl, , Info, Guessed in %i% tries!
		GuiControl, , Default, Restart
		Default = Restart
	}
	Else
	{
		If i = 0
		{
			GuiControl, , Default, Submit
			GuiControl, , History
		}
		Else
		{
			If (StrLen(Bulls) != 1 || StrLen(Cows) != 1)
				Return
			If Bulls is not digit
				Return
			If Cows is not digit
				Return
			GuiControl, , History, % History .= ": " Bulls " Bulls " Cows " Cows`n"
			GuiControl, , Bulls
			GuiControl, , Cows
			
			S:=Remove(S, Guess, Bulls, Cows)
		}
		If !S
		{
			GuiControl, , Info, Invalid response.
			GuiControl, , Default, Restart
			Default = Restart
		}
		Else
		{
			Guess := SubStr(S,1,length)
			GuiControl, , History, % History . Guess
			GuiControl, , Info, Enter a single digit number of bulls and cows.
			i++
		}
	}
Return

GuiEscape:
GuiClose:
	ExitApp

Remove(S, Guess, Bulls, Cows) {
	Loop, Parse, S, `n
		If (Bulls "," Cows = Response(Guess, A_LoopField))
			S2 .= A_LoopField . "`n"
	Return SubStr(S2,1,-1)
}

; from http://rosettacode.org/wiki/Bulls and Cows#AutoHotkey
Response(Guess,Code) {
	Bulls := 0, Cows := 0
	Loop, % StrLen(Code)
		If (SubStr(Guess, A_Index, 1) = SubStr(Code, A_Index, 1))
			Bulls++
		Else If (InStr(Code, SubStr(Guess, A_Index, 1)))
			Cows++
	Return Bulls "," Cows
}

; from http://rosettacode.org/wiki/Permutations#Alternate_Version
P(n,k="",opt=0,delim="",str="") {
	i:=0
	If !InStr(n,"`n")
		If n in 2,3,4,5,6,7,8,9
			Loop, %n%
				n := A_Index = 1 ? A_Index : n "`n" A_Index
		Else
			Loop, Parse, n, %delim%
				n := A_Index = 1 ? A_LoopField : n "`n" A_LoopField
	If (k = "")
		RegExReplace(n,"`n","",k), k++
	If k is not Digit
		Return "k must be a digit."
	If opt not in 0,1,2,3
		Return "opt invalid."
	If k = 0
		Return str
	Else
		Loop, Parse, n, `n
			If (!InStr(str,A_LoopField) || opt & 1)
				s .= (!i++ ? (opt & 2 ? str "`n" : "") : "`n" )
					. P(n,k-1,opt,delim,str . A_LoopField . delim)
		Return s
}
```



## BBC BASIC


```bbcbasic
      secret$ = ""
      REPEAT
        c$ = CHR$(&30 + RND(9))
        IF INSTR(secret$, c$) = 0 secret$ += c$
      UNTIL LEN(secret$) = 4
      
      FOR i% = 1234 TO 9876
        possible$ += STR$(i%)
      NEXT
      
      PRINT "Guess a four-digit number with no digit used twice."
      guesses% = 0
      REPEAT
        
        IF LEN(possible$) = 4 THEN
          guess$ = possible$
        ELSE
          guess$ = MID$(possible$, 4*RND(LEN(possible$) / 4) - 3, 4)
        ENDIF
        
        PRINT '"Computer guesses " guess$
        guesses% += 1
        
        IF guess$ = secret$ PRINT "Correctly guessed after "; guesses% " guesses!" : END
        
        PROCcount(secret$, guess$, bulls%, cows%)
        PRINT "giving " ;bulls% " bull(s) and " ;cows% " cow(s)."
        
        i% = 1
        REPEAT
          temp$ = MID$(possible$, i%, 4)
          PROCcount(temp$, guess$, testbulls%, testcows%)
          IF bulls%=testbulls% IF cows%=testcows% THEN
            i% += 4
          ELSE
            possible$ = LEFT$(possible$, i%-1) + MID$(possible$, i%+4)
          ENDIF
        UNTIL i% > LEN(possible$)
        
        IF INSTR(possible$, secret$) = 0 STOP
        
      UNTIL FALSE
      END
      
      DEF PROCcount(secret$, guess$, RETURN bulls%, RETURN cows%)
      LOCAL i%, c$
      bulls% = 0
      cows% = 0
      FOR i% = 1 TO 4
        c$ = MID$(secret$, i%, 1)
        IF MID$(guess$, i%, 1) = c$ THEN
          bulls% += 1
        ELSE IF INSTR(guess$, c$) THEN
            cows% += 1
          ENDIF
        ENDIF
      NEXT i%
      ENDPROC

```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <time.h>

char *list;
const char *line = "--------+--------------------\n";
int len = 0;

int irand(int n)
{
	int r, rand_max = RAND_MAX - (RAND_MAX % n);
	do { r = rand(); } while(r >= rand_max);
	return r / (rand_max / n);
}

char* get_digits(int n, char *ret)
{
	int i, j;
	char d[] = "123456789";

	for (i = 0; i < n; i++) { 
		j = irand(9 - i);
		ret[i] = d[i + j];
		if (j) d[i + j] = d[i], d[i] = ret[i];
	}
	return ret;
}

#define MASK(x) (1 << (x - '1'))
int score(const char *digits, const char *guess, int *cow)
{
	int i, bits = 0, bull = *cow = 0;

	for (i = 0; guess[i] != '\0'; i++)
		if (guess[i] != digits[i])
			bits |= MASK(digits[i]);
		else ++bull;

	while (i--) *cow += ((bits & MASK(guess[i])) != 0);

	return bull;
}

void pick(int n, int got, int marker, char *buf)
{
	int i, bits = 1;
	if (got >= n)
		strcpy(list + (n + 1) * len++, buf);
	else
		for (i = 0; i < 9; i++, bits *= 2) {
			if ((marker & bits)) continue;
			buf[got] = i + '1';
			pick(n, got + 1, marker | bits, buf);
		}
}

void filter(const char *buf, int n, int bull, int cow)
{
	int i = 0, c;
	char *ptr = list;

	while (i < len) {
		if (score(ptr, buf, &c) != bull || c != cow)
			strcpy(ptr, list + --len * (n + 1));
		else
			ptr += n + 1, i++;
	}
}

void game(const char *tgt, char *buf)
{
	int i, p, bull, cow, n = strlen(tgt);

	for (i = 0, p = 1; i < n && (p *= 9 - i); i++);
	list = malloc(p * (n + 1));

	pick(n, 0, 0, buf);
	for (p = 1, bull = 0; n - bull; p++) {
		strcpy(buf, list + (n + 1) * irand(len));
		bull = score(tgt, buf, &cow);

		printf("Guess %2d| %s    (from: %d)\n"
			"Score   | %d bull, %d cow\n%s",
			p, buf, len, bull, cow, line);

		filter(buf, n, bull, cow);
	}
}

int main(int c, char **v)
{
	int n = c > 1 ? atoi(v[1]) : 4;

	char secret[10] = "", answer[10] = "";
	srand(time(0));

	printf("%sSecret  | %s\n%s", line, get_digits(n, secret), line);
	game(secret, answer);

	return 0;
}
```
sample output for 4 digits:<lang>--------+--------------------
Secret  | 5437
--------+--------------------
Guess  1| 7198    (from: 3024)
Score   | 0 bull, 1 cow
--------+--------------------
Guess  2| 2386    (from: 720)
Score   | 0 bull, 1 cow
--------+--------------------
Guess  3| 5743    (from: 122)
Score   | 1 bull, 3 cow
--------+--------------------
Guess  4| 5437    (from: 5)
Score   | 4 bull, 0 cow
--------+--------------------

```
sample output for 9 digits<lang>--------+--------------------
Secret  | 758214936
--------+--------------------
Guess  1| 245863197    (from: 362880)
Score   | 0 bull, 9 cow
--------+--------------------
Guess  2| 964715382    (from: 133496)
Score   | 1 bull, 8 cow
--------+--------------------
Guess  3| 614927853    (from: 48722)
Score   | 0 bull, 9 cow
--------+--------------------
Guess  4| 567391248    (from: 15926)
Score   | 0 bull, 9 cow
--------+--------------------
Guess  5| 839174562    (from: 4473)
Score   | 1 bull, 8 cow
--------+--------------------
Guess  6| 489635721    (from: 1503)
Score   | 0 bull, 9 cow
--------+--------------------
Guess  7| 723156984    (from: 317)
Score   | 2 bull, 7 cow
--------+--------------------
Guess  8| 953278614    (from: 89)
Score   | 2 bull, 7 cow
--------+--------------------
Guess  9| 321479685    (from: 24)
Score   | 0 bull, 9 cow
--------+--------------------
Guess 10| 893752416    (from: 4)
Score   | 1 bull, 8 cow
--------+--------------------
Guess 11| 758214936    (from: 1)
Score   | 9 bull, 0 cow
--------+--------------------
```



## C++


```cpp

#include <iostream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <vector>
#include <time.h>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const unsigned int LEN = 4;

//--------------------------------------------------------------------------------------------------
class CowsAndBulls_Player
{
public:
    CowsAndBulls_Player() { fillPool(); }
    void play() { secret = createSecret(); guess(); }

private:
    void guess()
    {
	pair<int, int> res; int cc = 1;
	cout << endl << " SECRET: " << secret << endl << "
### ========
" << endl;
	cout << "+-----------+---------+--------+\n|   GUESS   |  BULLS  |  COWS  |\n+-----------+---------+--------+\n";
	while( true )
	{
	    string gs = gimmeANumber();
	    if( gs.empty() ) { cout << endl << "Something went wrong with the scoring..." << endl << "Cannot find an answer!" << endl; return; }
	    if( scoreIt( gs, res ) ) { cout << endl << "I found the secret number!" << endl << "It is: " << gs << endl; return; }
	    cout << "|    " << gs << "   |  " << setw( 3 ) << res.first << "    |  " << setw( 3 ) << res.second << "   |\n+-----------+---------+--------+\n";
	    clearPool( gs, res );
        }
    }

    void clearPool( string gs, pair<int, int>& r )
    {
	vector<string>::iterator pi = pool.begin();
	while( pi != pool.end() )
	{
	    if( removeIt( gs, ( *pi ), r ) ) pi = pool.erase( pi );
	    else  pi++;
	}
    }

    string gimmeANumber()
    {
	if( pool.empty() ) return "";
	return pool[rand() % pool.size()];
    }

    void fillPool()
    {
	for( int x = 1234; x < 9877; x++ )
	{
	    ostringstream oss; oss << x;
	    if( check( oss.str() ) ) pool.push_back( oss.str() );
	}
    }

    bool check( string s )
    {
	for( string::iterator si = s.begin(); si != s.end(); si++ )
	{
	    if( ( *si ) == '0' ) return false;
	    if( count( s.begin(), s.end(), ( *si ) ) > 1 ) return false;
	}
	return true;
    }

    bool removeIt( string gs, string ts, pair<int, int>& res )
    {
	pair<int, int> tp; getScore( gs, ts, tp );
	return tp != res;
    }

    bool scoreIt( string gs, pair<int, int>& res )
    {
	getScore( gs, secret, res );
	return res.first == LEN;
    }

    void getScore( string gs, string st, pair<int, int>& pr )
    {
	pr.first = pr.second = 0;
	for( unsigned int ui = 0; ui < LEN; ui++ )
	{
	    if( gs[ui] == st[ui] ) pr.first++;
	    else
	    {
		for( unsigned int vi = 0; vi < LEN; vi++ )
		    if( gs[ui] == st[vi] ) pr.second++;
	    }
	}
    }

    string createSecret()
    {
	string n = "123456789", rs = "";
	while( rs.length() < LEN )
	{
	    int r = rand() % n.length();
	    rs += n[r]; n.erase( r, 1 );
	}
	return rs;
    }

    string secret;
    vector<string> pool;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned int>( time( NULL ) ) ); CowsAndBulls_Player cb;
    cb.play(); cout << endl << endl;
    return system( "pause" );
}
//--------------------------------------------------------------------------------------------------

```

Output:

```txt

 SECRET: 6281

### ========

+-----------+---------+--------+
|   GUESS   |  BULLS  |  COWS  |
+-----------+---------+--------+
|    5132   |    0    |    2   |
+-----------+---------+--------+
|    1568   |    0    |    3   |
+-----------+---------+--------+
|    2685   |    1    |    2   |
+-----------+---------+--------+
|    2816   |    0    |    4   |
+-----------+---------+--------+

I found the secret number!
It is: 6281

```


=={{header|C sharp|C#}}==
{{works with|C#|3.0}}


```csharp

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace BullsAndCows
{
    class Program
    {
        const int ANSWER_SIZE = 4;

        static IEnumerable<string> Permutations(int size)
        {
            if (size > 0)
            {
                foreach (string s in Permutations(size - 1))
                    foreach (char n in "123456789")
                        if (!s.Contains(n))
                            yield return s + n;
            }
            else
                yield return "";
        }

        static IEnumerable<T> Shuffle<T>(IEnumerable<T> source)
        {
            Random random = new Random();
            List<T> list = source.ToList();
            while (list.Count > 0)
            {
                int ix = random.Next(list.Count);
                yield return list[ix];
                list.RemoveAt(ix);
            }
        }

        static bool ReadBullsCows(out int bulls, out int cows)
        {
            string[] input = Console.ReadLine().Split(',').ToArray();
            bulls = cows = 0;
            if (input.Length < 2)
                return false;
            else
                return int.TryParse(input[0], out bulls)
                    && int.TryParse(input[1], out cows);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("Bulls and Cows");
            Console.WriteLine("
### ========
");
            Console.WriteLine();
            List<string> answers = Shuffle(Permutations(ANSWER_SIZE)).ToList();
            while (answers.Count > 1)
            {
                string guess = answers[0];
                Console.Write("My guess is {0}. How many bulls, cows? ", guess);
                int bulls, cows;
                if (!ReadBullsCows(out bulls, out cows))
                    Console.WriteLine("Sorry, I didn't understand that. Please try again.");
                else
                    for (int ans = answers.Count - 1; ans >= 0; ans--)
                    {
                        int tb = 0, tc = 0;
                        for (int ix = 0; ix < ANSWER_SIZE; ix++)
                            if (answers[ans][ix] == guess[ix])
                                tb++;
                            else if (answers[ans].Contains(guess[ix]))
                                tc++;
                        if ((tb != bulls) || (tc != cows))
                            answers.RemoveAt(ans);
                    }
            }
            if (answers.Count == 1)
                Console.WriteLine("Hooray! The answer is {0}!", answers[0]);
            else
                Console.WriteLine("No possible answer fits the scores you gave.");
        }
    }
}

```

Example output:-

```txt

Bulls and Cows

### ========


My guess is 7854. How many bulls, cows? 0,1
My guess is 1539. How many bulls, cows? 1,2
My guess is 2935. How many bulls, cows? 2,1
My guess is 9635. How many bulls, cows? 1,3
Hooray! The answer is 5936!

```



## Common Lisp


```lisp

(defun random-number ()
  (do* ((lst '(1 2 3 4 5 6 7 8 9) (remove d lst))
        (l 9 (length lst))
        (d (nth (random l) lst) (nth (random l) lst))
        (number nil (cons d number)))
       ((= l 5) number)))

(defun validp (number)
  (loop for el in number with rst = (rest number)
        do (cond ((= el 0) (return-from validp nil))
                 ((member el rst) (return-from validp nil))
                 (t (setq rst (rest rst))))
        finally (return number)))

(defun bulls (number guess)
  (loop for x in number
        for y in guess
        counting (= x y) into b
        finally (return b)))

(defun bulls+cows (number guess)
  (loop for x in guess
        counting (member x number) into c
        finally (return c)))

(defun solve ()
  (let ((number (random-number))
        (possible-guesses (loop for i from 1234 to 9876
                                when (validp (map 'list #'digit-char-p (prin1-to-string i))) collect it)))
       (format t "Secret: ~a~%" number)
       (loop with guess = (nth (random (length possible-guesses)) possible-guesses)    
             with b = (bulls number guess)
             with c = (- (bulls+cows number guess) b)
             do  (format t "Guess:  ~a B: ~a C: ~a~%" guess b c)
                 (if (= b 4)
                   (return-from solve (format t "Solution found!")))
                 (setq possible-guesses (mapcan #'(lambda (x) (if (and (= b (bulls x guess))
								       (= c (- (bulls+cows x guess) b)))
							        (list x))) 
						(remove guess possible-guesses)))
                 (setq guess (nth (random (length possible-guesses)) possible-guesses))
                 (setq b (bulls number guess))
                 (setq c (- (bulls+cows number guess) b)))))

```

Output:

```txt

CL-USER> (solve)
Secret: (4 8 6 1)
Guess:  (3 5 1 6) B: 0 C: 2
Guess:  (7 1 9 5) B: 0 C: 1
Guess:  (6 8 3 7) B: 1 C: 1
Guess:  (5 8 4 3) B: 1 C: 1
Guess:  (1 8 6 4) B: 2 C: 2
Guess:  (4 8 6 1) B: 4 C: 0
Solution found!
NIL

```



## Crystal

{{trans|Ruby}}

```Ruby
size = 4
scores = [] of Tuple(Int32, Int32)
guesses = [] of Array(Char)
puts "Playing Bulls & Cows with #{size} unique digits."
possible_guesses = ('1'..'9').to_a.permutations(size).shuffle

loop do
  guesses << (current_guess = possible_guesses.pop)
  print "Guess #{guesses.size} (#{possible_guesses.size}) is #{current_guess.join}. Answer (bulls,cows)? "
  bulls, cows = gets.not_nil!.split(',').map(&.to_i)
  scores << (score = {bulls, cows})

  # handle win
  break (puts "Yeah!") if score == {size, 0}

  # filter possible guesses
  possible_guesses.select! do |guess|
    bulls = guess.zip(current_guess).count { |g, cg| g == cg }
    cows = size - (guess - current_guess).size - bulls
    {bulls, cows} == score
  end

  # handle 'no possible guesses left'
  if possible_guesses.empty?
    puts "Error in scoring?"
    guesses.zip(scores).each { |g, (b, c)| puts "#{g.join} => bulls #{b} cows #{c}" }
    break
  end
end
```



## D


```d
void main() {
    import std.stdio, std.random, std.algorithm, std.range, std.ascii;

    immutable d9 = "123456789";
    auto choices = cartesianProduct(d9, d9, d9, d9).map!(t => [t[]])
                   .filter!(a => a.sort().uniq.count == 4).array;

    do {
        const ans = choices[uniform(0, $)];
        writef("My guess is %s. How many bulls and cows? ", ans);
        immutable score = readln.filter!isDigit.map!q{ a - '0' }.array;
        choices = choices.remove!(c => score !=
            [c.zip(ans).count!(p => p[0] == p[1]),
             c.zip(ans).count!(p => p[0] != p[1] && ans.canFind(p[0]))]);
    } while (choices.length > 1);

    if (choices.empty)
        return "Nothing fits the scores you gave.".writeln;
    writeln("Solution found: ", choices[0]);
}
```

{{out|Example output}}

```txt
My guess is 9345. How many bulls and cows? 02
My guess is 8496. How many bulls and cows? 01
My guess is 2739. How many bulls and cows? 12
My guess is 3129. How many bulls and cows? 03
My guess is 7931. How many bulls and cows? 40
Solution found: 7931
```



## Elixir

{{works with|Elixir|1.2}}

```elixir
defmodule Bulls_and_cows do
  def player(size \\ 4) do
    possibility = permute(size) |> Enum.shuffle
    player(size, possibility, 1)
  end
  
  def player(size, possibility, i) do
    guess = hd(possibility)
    IO.puts "Guess #{i} is #{Enum.join(guess)}  (from #{length(possibility)} possibilities)"
    case get_score(size) do
      {^size, 0} -> IO.puts "Solved!"
      score ->
        case select(size, possibility, guess, score) do
          [] -> IO.puts "Sorry! I can't find a solution. Possible mistake in the scoring."
          selected -> player(size, selected, i+1)
        end
    end
  end
  
  defp get_score(size) do
    IO.gets("Answer (Bulls, cows)? ")
    |> String.split(~r/\D/, trim: true)
    |> Enum.map(&String.to_integer/1)
    |> case do
         [bulls, cows] when bulls+cows in 0..size -> {bulls, cows}
         _ -> get_score(size)
       end
  end
  
  defp select(size, possibility, guess, score) do
    Enum.filter(possibility, fn x ->
      bulls = Enum.zip(x, guess) |> Enum.count(fn {n,g} -> n==g end)
      cows = size - length(x -- guess) - bulls
      {bulls, cows} == score
    end)
  end
  
  defp permute(size), do: permute(size, Enum.to_list(1..9))
  defp permute(0, _), do: [[]]
  defp permute(size, list) do
    for x <- list, y <- permute(size-1, list--[x]), do: [x|y]
  end
end

Bulls_and_cows.player
```


{{out}}

```txt

Guess 1 is 6472  (from 3024 possibilities)
Answer (Bulls, cows)? 0 2
Guess 2 is 7321  (from 840 possibilities)
Answer (Bulls, cows)? 0 3
Guess 3 is 1263  (from 54 possibilities)
Answer (Bulls, cows)? 2 1
Guess 4 is 1234  (from 2 possibilities)
Answer (Bulls, cows)? 4 0
Solved!

```



## Euphoria

{{trans|C}}
{{works with|Euphoria|4.*}}

```euphoria
include std/sequence.e

constant line = "--------+--------------------\n"
constant digits = "123456789"
sequence list = {}

function get_digits(integer n)
    integer j
    sequence d = digits, ret = ""
    for i=1 to n do
        j = rand(length(digits)-i)
        ret &= d[i+j]
        if j then
            d[i+j] = d[i]
            d[i] = ret[i]
        end if
    end for
    return ret
end function

function MASK(integer x)
    return power(2,x-digits[1])
end function

function score(sequence pattern, sequence guess)
    integer bits = 0, bull = 0, cow = 0
    for i = 1 to length(guess) do
        if guess[i] != pattern[i] then
            bits += MASK(pattern[i])
        else
            bull += 1
        end if
    end for
    
    for i = 1 to length(guess) do
        cow += and_bits(bits,MASK(guess[i])) != 0
    end for
    
    return {bull, cow}
end function

procedure pick(integer n, integer got, integer marker, sequence buf)
    integer bits = 1
    if got >= n then
        list = append(list,buf)
    else
        for i = 0 to length(digits)-1 do
            if not and_bits(marker,bits) then
                buf[got+1] = i+digits[1]
                pick(n, got+1, or_bits(marker,bits), buf)
            end if
            bits *= 2
        end for
    end if
end procedure

function tester(sequence item, sequence data)
    return equal(score(item,data[1]),data[2])
end function

constant tester_id = routine_id("tester")

procedure game(sequence tgt)
    integer p, n = length(tgt)
    sequence buf = repeat(0,n), bc
    list = {}
    pick(n,0,0,buf)
    p = 1
    bc = {0,0}
    while bc[1]<n do
        buf = list[rand($)]
        bc = score(tgt,buf)
        printf(1,"Guess %2d| %s    (from: %d)\nScore   | %d bull, %d cow\n%s",
                    {p, buf, length(list)} & bc & {line})
        
        list = filter(list, tester_id, {buf, bc})
        p+=1
    end while
end procedure

constant n = 4
sequence secret = get_digits(n)
printf(1,"%sSecret  | %s\n%s", {line, secret, line})
game(secret)
```


Output:

```txt
--------+--------------------
Secret  | 6478
--------+--------------------
Guess  1| 5813    (from: 3024)
Score   | 0 bull, 1 cow
--------+--------------------
Guess  2| 6782    (from: 720)
Score   | 1 bull, 2 cow
--------+--------------------
Guess  3| 2689    (from: 62)
Score   | 0 bull, 2 cow
--------+--------------------
Guess  4| 7562    (from: 19)
Score   | 0 bull, 2 cow
--------+--------------------
Guess  5| 6478    (from: 4)
Score   | 4 bull, 0 cow
--------+--------------------
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
module Player
  implicit none

contains

subroutine Init(candidates)
  integer, intent(in out) :: candidates(:)
  integer :: a, b, c, d, n
    
           n = 0
thousands: do a = 1, 9
hundreds:    do b = 1, 9
tens:          do c = 1, 9
units:           do d = 1, 9
                   if (b == a) cycle hundreds
                   if (c == b .or. c == a) cycle tens
                   if (d == c .or. d == b .or. d == a) cycle units
                   n = n + 1
                   candidates(n) = a*1000 + b*100 + c*10 + d
                 end do units
               end do tens
             end do hundreds
           end do thousands

end subroutine init

subroutine Evaluate(bulls, cows, guess, candidates) 
  integer, intent(in) :: bulls, cows, guess
  integer, intent(in out) :: candidates(:)
  integer :: b, c, s, i, j
  character(4) :: n1, n2
   
  write(n1, "(i4)") guess
  do i = 1, size(candidates)
    if (candidates(i) == 0) cycle
    b = 0
    c = 0
    write(n2, "(i4)") candidates(i)
    do j = 1, 4
      s = index(n1, n2(j:j)) 
      if(s /= 0) then
        if(s == j) then
          b = b + 1
        else
          c = c + 1
        end if
      end if
    end do
    if(.not.(b == bulls .and. c == cows)) candidates(i) = 0
  end do
end subroutine Evaluate

function Nextguess(candidates)
  integer :: Nextguess
  integer, intent(in out) :: candidates(:)
  integer :: i

  nextguess = 0
  do i = 1, size(candidates)
    if(candidates(i) /= 0) then
      nextguess = candidates(i)
      candidates(i) = 0
      return
     end if
  end do
end function
end module Player

program Bulls_Cows
  use Player
  implicit none

  integer :: bulls, cows, initial, guess
  integer :: candidates(3024) = 0
  real :: rnum

! Fill candidates array with all possible number combinations
  call Init(candidates)

! Random initial guess
  call random_seed
  call random_number(rnum)
  initial = 3024 * rnum + 1
  guess = candidates(initial)
  candidates(initial) = 0
  
  do 
    write(*, "(a, i4)") "My guess is ", guess
    write(*, "(a)", advance = "no") "Please score number of Bulls and Cows: "
    read*, bulls, cows
    write(*,*)
    if (bulls == 4) then
      write(*, "(a)") "Solved!"
      exit
    end if

! We haven't found the solution yet so evaluate the remaining candidates
! and eliminate those that do not match the previous score given    
    call Evaluate(bulls, cows, guess, candidates)

! Get the next guess from the candidates that are left
    guess = Nextguess(candidates)
    if(guess == 0) then
! If we get here then no solution is achievable from the scores given or the program is bugged     
      write(*, "(a)") "Sorry! I can't find a solution. Possible mistake in the scoring"
      exit
    end if
  end do
end program
```

Output

```txt

My guess is 1528
Please score number of Bulls and Cows: 0 1

My guess is 2346
Please score number of Bulls and Cows: 0 1

My guess is 3179
Please score number of Bulls and Cows: 1 2

My guess is 3795
Please score number of Bulls and Cows: 0 2

My guess is 4971
Please score number of Bulls and Cows: 2 2

My guess is 9471
Please score number of Bulls and Cows: 4 0

Solved!
```



## Go

Notes:  Strategy per the suggestion in the problem description.  Check algorithm lifted from Bulls and cows program.  Code here uses Go's built in map type as the container for the list of still-possible numbers; only the map key is used, the value is assigned a dummy of 0.

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    fmt.Println(`Cows and bulls/player
You think of four digit number of unique digits in the range 1 to 9.
I guess.  You score my guess:
    A correct digit but not in the correct place is a cow.
    A correct digit in the correct place is a bull.
You give my score as two numbers separated with a space.`)

    // generate possible patterns, store in map
    m := make(map[string]int)
    var g func([]byte, int)
    g = func(digits []byte, fixed int) {
        if fixed == 4 {
            m[string(digits[:4])] = 0
            return
        }
        for i := fixed; i < len(digits); i++ {
            digits[fixed], digits[i] = digits[i], digits[fixed]
            g(digits, fixed+1)
            digits[fixed], digits[i] = digits[i], digits[fixed]
        }
    }
    g([]byte("123456789"), 0)

    // guess/score/eliminate loop
    for in := bufio.NewReader(os.Stdin);; {
        // pick a value, ie, guess
        var guess string
        for guess = range m {
            delete(m, guess)
            break
        }

        // get and parse score
        var c, b uint
        for ;; fmt.Println("Score guess as two numbers: cows bulls") {
            fmt.Printf("My guess: %s.  Score? (c b) ", guess)
            score, err := in.ReadString('\n')
            if err != nil {
                fmt.Println("\nSo, bye.")
                return
            }
            s2 := strings.Fields(score)
            if len(s2) == 2 {
                c2, err := strconv.ParseUint(s2[0], 10, 0)
                if err == nil && c2 <= 4 {
                    b2, err := strconv.ParseUint(s2[1], 10, 0)
                    if err == nil && c2+b2 <= 4 {
                        c = uint(c2)
                        b = uint(b2)
                        break
                    }
                }
            }
        }

        // check for win
        if b == 4 {
            fmt.Println("I did it. :)")
            return
        }

        // eliminate patterns with non-matching scores
        for pat := range m {
            var cows, bulls uint
            for ig, cg := range guess {
                switch strings.IndexRune(pat, cg) {
                case -1:
                default: // I just think cows should go first
                    cows++
                case ig:
                    bulls++
                }
            }
            if cows != c || bulls != b {
                delete(m, pat)
            }
        }

        // check for inconsistency
        if len(m) == 0 {
            fmt.Println("Oops, check scoring.")
            return
        }
    }
}
```



## Haskell


```haskell
import Data.List
import Control.Monad
import System.Random (randomRIO)
import Data.Char(digitToInt)

combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

player = do
  let ps = concatMap permutations $ combinationsOf 4 ['1'..'9']
  play ps   where
  
  play ps =
    if null ps then 
	putStrLn "Unable to find a solution"
    else do i <- randomRIO(0,length ps - 1)
            let p = ps!!i :: String
	    putStrLn ("My guess is " ++ p) >>  putStrLn "How many bulls and cows?" 
	    input <- takeInput
	    let bc = input ::[Int]
		ps' = filter((==sum bc).length. filter id. map (flip elem p))
		    $ filter((==head bc).length. filter id. zipWith (==) p) ps
	    if length ps' == 1 then putStrLn $ "The answer is " ++ head ps'
		else play ps'

  takeInput = do
    inp <- getLine
    let ui = map digitToInt $ take 2 $ filter(`elem` ['0'..'4']) inp
    if sum ui > 4 || length ui /= 2 then
      do putStrLn "Wrong input. Try again"
	 takeInput
	else return ui
```

Example:

```haskell
*Main> player
My guess is 4923
How many bulls and cows?
2 2
My guess is 3924
How many bulls and cows?
1 3
My guess is 4329
How many bulls and cows?
1 3
My guess is 4932
How many bulls and cows?
4 0
The answer is 4932
```



## J



```j
require'misc'

poss=:1+~.4{."1 (i.!9)A.i.9 
fmt=: ' ' -.~ ":

play=:3 :0
  while.1<#poss=.poss do.
    smoutput'guessing ',fmt guess=.({~ ?@#)poss
    bc=.+/\_".prompt 'how many bull and cows? '
    poss=.poss #~({.bc)=guess+/@:="1 poss
    poss=.poss #~({:bc)=guess+/@e."1 poss
  end.
  if.#poss do.
    'the answer is ',fmt,poss
  else.
    'no valid possibilities'
  end.
)
```


For example:

```j
   play''
guessing 7461
how many bull and cows? 0 1
guessing 3215
how many bull and cows? 0 3
guessing 2357
how many bull and cows? 2 0
guessing 1359
how many bull and cows? 3 0
the answer is 1358
```



## Julia

{{works with|Julia version 0.6.0}}

```Julia

countbulls(a, b) = sum([a[i] == b[i] for i in 1:length(a)])
countcows(a, b) = sum([a[i] == b[j] for i in 1:length(a), j in 1:length(b) if i != j])
validate(a, b) = typeof(a) == Int && typeof(b) == Int && a >= 0 && b >= 0 && a + b < 5

function doguess()
    poss = [a for a in collect(Base.product(1:9,1:9,1:9,1:9)) if length(unique(a))[1]==4]
    while(length(poss) > 0)
        ans = rand(poss, 1)[1]
        while true
            println("My guess: $(ans[1])$(ans[2])$(ans[3])$(ans[4]). How many bulls and cows?")
            regres = match(r"\D*(\d+)\D*(\d+)", readline())
            bul, cow = parse(Int,regres[1]), parse(Int,regres[2])
            if(validate(bul, cow))
                break
            else
                println("Please enter an integer each for bulls and cows.")
            end
        end
        if(bul == 4)
            return ans
        end
        filter!(i -> (countbulls(ans,i), countcows(ans, i)) == (bul, cow), poss)
    end
    Base.throw("ERROR: No solutions found. Inconsistent scoring by other player?")
end

answ = doguess()
println("The winning pick: $(answ[1])$(answ[2])$(answ[3])$(answ[4])")

```

Example game:

```Julia

My guess: 9517. How many bulls and cows?
0 1
My guess: 3285. How many bulls and cows?
1 1
My guess: 4298. How many bulls and cows?
0 2
My guess: 2465. How many bulls and cows?
0 2
My guess: 3926. How many bulls and cows?
2 0
My guess: 3724. How many bulls and cows?
3 0
My guess: 3124. How many bulls and cows?
4 0
The winning pick: 3124

```



## Kotlin


```scala
// version 1.1.2

import java.util.Random

fun countBullsAndCows(guess: IntArray, answer: IntArray): Pair<Int,Int> {
    var bulls = 0
    var cows = 0
    for ((i, d) in guess.withIndex()) {
        if (answer[i] == d) bulls++
        else if (d in answer) cows++
    }
    return bulls to cows
} 

fun main(args: Array<String>) {
    val r = Random()
    val choices = mutableListOf<IntArray>()
    // generate all possible distinct 4 digit (1 to 9) integer arrays  
    for (i in 1..9) {
        for (j in 1..9) {
            if (j == i) continue
            for (k in 1..9) {
                if (k == i || k == j) continue
                for (l in 1..9) {
                    if (l == i || l == j || l == k) continue
                    choices.add(intArrayOf(i, j, k, l))
                }
            }
        }
    } 

    // pick one at random as the answer
    val answer = choices[r.nextInt(choices.size)]

    // keep guessing, pruning the list as we go based on the score, until answer found
    while (true) {
        val guess = choices[r.nextInt(choices.size)]
        val (bulls, cows) = countBullsAndCows(guess, answer)
        println("Guess = ${guess.joinToString("")}  Bulls = $bulls  Cows = $cows")
        if (bulls == 4) {
            println("You've just found the answer!")
            return
        }
        for (i in choices.size - 1 downTo 0) {
            val (bulls2, cows2) = countBullsAndCows(choices[i], answer)
            // if score is no better remove it from the list of choices 
            if (bulls2 <= bulls && cows2 <= cows) choices.removeAt(i)
        }
        if (choices.size == 0) 
            println("Something went wrong as no choices left! Aborting program")
    }
}
```

Sample game:
{{out}}

```txt

Guess = 7185  Bulls = 0  Cows = 2
Guess = 7862  Bulls = 1  Cows = 2
Guess = 6982  Bulls = 2  Cows = 0
Guess = 1762  Bulls = 2  Cows = 2
Guess = 1264  Bulls = 0  Cows = 3
Guess = 2761  Bulls = 1  Cows = 3
Guess = 6742  Bulls = 3  Cows = 0
Guess = 1627  Bulls = 0  Cows = 4
Guess = 6712  Bulls = 4  Cows = 0
You've just found the answer!

```



## Liberty BASIC

As supplied rhe code shows the remaining pool of numbers after each guess is scored.

```lb

guesses    =0

do while len( secret$) <4    '    zero not allowed    <<<<<<<<<
    n$ =chr$( int( rnd( 1) *9) +49)
    if not( instr( secret$, n$)) then secret$ =secret$ +n$
loop

print " Secretly, my opponent just chose a number. But she didn't tell anyone!                                     "; secret$; "."
print "     I can however be given a score for my guesses."

for i =1234 to 9876    '                    <<<<<<<<<
    if check( str$( i)) =0 then available$ =available$ +" " +str$( i): k =k +1
next i

available$ =trim$( available$)  '   remove the surplus, leading space

print
print "Currently holding "; k; " possible numbers. "

while 1
    guess$ =word$( available$, 1 +int( k *rnd( 1)), " ")
    print
    print "Computer guessed "; guess$; " & got ";

    bulls    =0
    cows     =0
    guesses  =guesses +1

    r$    =score$( guess$, secret$)

    bulls =val( word$( r$, 1, ","))
    cows  =val( word$( r$, 2, ","))

    print bulls; " bull(s), and "; cows; " cow(s), .... ";

    if guess$ =secret$ then
        print "Computer won after "; guesses; " guesses!";
        secs =( time$( "seconds") -now +86400) mod 86400
        print " That took "; secs; " seconds. ENDED!"
        print
        print " Now scroll right to see original choice and check!"
        exit while
    end if

    print " so possible numbers are now only..."
    kk      =0
    new$    =""

    for j =1 to k
        bullsT    =0
        cowsT     =0

        possible$ =word$( available$, j, " ")

        r$ =score$( guess$, possible$)

        bullsT =val( word$( r$, 1, ","))
        cowsT  =val( word$( r$, 2, ","))

        if ( bullsT =bulls) and ( cowsT =cows)  then
            new$ =new$ +" " +possible$    '    keep those with same score
            kk =kk +1
            print possible$; " ";
            if ( kk mod 20) =0 then print
        end if

        scan
    next j

    available$ =trim$( new$)
    k =kk

    scan
wend

end

function score$( a$, b$)    '   return as a csv string the number of bulls & cows.
    bulls    = 0:  cows     = 0
    for i = 1 to 4
        c$ = mid$( a$, i, 1)
        if mid$( b$, i, 1) = c$ then
            bulls = bulls + 1
        else
            if instr( b$, c$) <>0 and instr( b$, c$) <>i then cows = cows + 1
        end if
    next i
    score$ =str$( bulls); ","; str$( cows)
end function

function check( i$)
    check =0    '    zero flags available: 1 means not available
    for i =1 to 3
        for j =i +1 to 4
            if mid$( i$, i, 1) =mid$( i$, j, 1) then check =1
        next j
    next i
    if instr( i$, "0") then check =1
end function

```

 

## Mathematica


```Mathematica

bullCow={Count[#1-#2,0],Length[#1\[Intersection]#2]-Count[#1-#2,0]}&;
Module[{r,input,candidates=Permutations[Range[9],{4}]},
While[True,
	r=InputString[];
	If[r===$Canceled,Break[],
		input=ToExpression/@StringSplit@r;
		If[Length@input!=3,Print["Input the guess, number of bulls, number of cows, delimited by space."],
			candidates=Select[candidates,bullCow[ToCharacterCode@StringJoin[ToString/@#],ToCharacterCode@ToString@input[[1]]]==input[[2;;3]]&];
			candidates=SortBy[candidates,{-3,-1}.bullCow[ToCharacterCode@StringJoin[ToString/@#],ToCharacterCode@ToString@input[[1]]]&];
			If[candidates==={},Print["No more candidates."];Break[]];
			If[Length@candidates==1,Print["Must be: "<>StringJoin[ToString/@candidates[[1]]]];Break[]];
			Print[ToString@Length@candidates<>" candidates remaining."];
			Print["Can try "<>StringJoin[ToString/@First@candidates]<>"."];
	]]]]

```

Output:

```txt

120 candidates remaining.
Can try 1256.
36 candidates remaining.
Can try 1537.
5 candidates remaining.
Can try 1584.
Must be: 1639

```



## MATLAB


```MATLAB
function BullsAndCowsPlayer
% Plays the game Bulls and Cows as the player
    
    % Generate list of all possible numbers
    nDigits = 4;
    lowVal = 1;
    highVal = 9;
    combs = nchoosek(lowVal:highVal, nDigits);
    nCombs = size(combs, 1);
    nPermsPerComb = factorial(nDigits);
    gList = zeros(nCombs.*nPermsPerComb, nDigits);
    for k = 1:nCombs
        gList(nPermsPerComb*(k-1)+1:nPermsPerComb*k, :) = perms(combs(k, :));
    end
    
    % Prompt user
    fprintf('Think of a number with:\n')
    fprintf('  %d digits\n', nDigits)
    fprintf('  Each digit between %d and %d inclusive\n', lowVal, highVal)
    fprintf('  No repeated digits\n')
    fprintf('I''ll try to guess that number and you score me:\n')
    fprintf('  1 Bull per correct digit in the correct place\n')
    fprintf('  1 Cow per correct digit in the wrong place\n')
    fprintf('Think of your number and press Enter when ready\n')
    pause
    
    % Play game until all digits are correct
    nBulls = 0;
    nGuesses = 0;
    while nBulls < 4 && ~isempty(gList)
        nList = size(gList, 1);
        g = gList(randi(nList), :);     % Random guess from list
        fprintf('My guess: %s?\n', sprintf('%d', g))
        nBulls = input('How many bulls? ');
        if nBulls < 4
            nCows = input('How many cows? ');
            del = false(nList, 1);
            for k = 1:nList
                del(k) = any([nBulls nCows] ~= CountBullsCows(g, gList(k, :)));
            end
            gList(del, :) = [];
        end
        nGuesses = nGuesses+1;
    end
    if isempty(gList)
        fprintf('That''s bull! You messed up your scoring.\n')
    else
        fprintf('Yay, I won! Only took %d guesses.\n', nGuesses)
    end
end

function score = CountBullsCows(guess, correct)
% Checks the guessed array of digits against the correct array to find the score
% Assumes arrays of same length and valid numbers
    bulls = guess == correct;
    cows = ismember(guess(~bulls), correct);
    score = [sum(bulls) sum(cows)];
end
```

{{out}}
Secret number: 5612

```txt
Think of a number with:
  4 digits
  Each digit between 1 and 9 inclusive
  No repeated digits
I'll try to guess that number and you score me:
  1 Bull per correct digit in the correct place
  1 Cow per correct digit in the wrong place
Think of your number and press Enter when ready
My guess: 3489?
How many bulls? 0
How many cows? 0
My guess: 2156?
How many bulls? 0
How many cows? 4
My guess: 5621?
How many bulls? 2
How many cows? 2
My guess: 1625?
How many bulls? 1
How many cows? 3
My guess: 5612?
How many bulls? 4
Yay, I won! Only took 5 guesses.
```

Secret number: 9934

```txt
Think of a number with:
  4 digits
  Each digit between 1 and 9 inclusive
  No repeated digits
I'll try to guess that number and you score me:
  1 Bull per correct digit in the correct place
  1 Cow per correct digit in the wrong place
Think of your number and press Enter when ready
My guess: 4798?
How many bulls? 0
How many cows? 2
My guess: 7862?
How many bulls? 0
How many cows? 0
My guess: 9413?
How many bulls? 1
How many cows? 2
My guess: 5439?
How many bulls? 0
How many cows? 3
My guess: 9154?
How many bulls? 2
How many cows? 0
That's bull! You messed up your scoring.
```



## Perl

{{works with|Perl 5.10.0 and above}}


```perl
#!/usr/bin/perl
use warnings;
use strict;
use v5.10;

# Build a list of all possible solutions.  The regular expression weeds
# out numbers containing zeroes or repeated digits.  See how Perl
# automatically converts numbers to strings for us, just because we
# use them as if they were strings:
my @candidates = grep {not /0 | (\d) .* \1 /x} 1234 .. 9876;

# Repeatedly prompt for input until the user supplies a reasonable score.
# The regex validates the user's input and then returns two numbers,
# $+{BULLS} and $+{COWS}.

sub read_score($) {
    (my $guess) = @_;

    for (;;) {
        say "My guess: $guess   (from ", 0+@candidates, " possibilities)"; 
        if (<> =~ / ^ \h* (?<BULLS> \d) \h* (?<COWS> \d) \h* $ /x and
            $+{BULLS} + $+{COWS} <= 4) {
                return ($+{BULLS}, $+{COWS});
        }

        say "Please specify the number of bulls and the number of cows";
    }
}

sub score_correct($$$$) {
    my ($a, $b, $bulls, $cows) = @_;

    # Count the positions at which the digits match: 
    my $exact = () = grep {substr($a, $_, 1) eq substr($b, $_, 1)} 0 .. 3;

    # Cross-match all digits in $a against all digits in $b, using a regex
    # (specifically, a character class) instead of an explicit loop:
    my $loose = () = $a =~ /[$b]/g;

    return $bulls == $exact && $cows == $loose - $exact;
}

do {
    # Pick a number, display it, get the score, and discard candidates
    # that don't match the score:
    my $guess = @candidates[rand @candidates];
    my ($bulls, $cows) = read_score $guess;
    @candidates = grep {score_correct $_, $guess, $bulls, $cows} @candidates;
} while (@candidates > 1);

say(@candidates?
    "Your secret number is @candidates":
    "I think you made a mistake with your scoring");

```


Sample game:

```perl
msl@64Lucid:~/perl$ ./bulls-and-cows 
My guess: 1869   (from 3024 possibilities)
1 0
My guess: 3265   (from 240 possibilities)
0 2
My guess: 7853   (from 66 possibilities)
1 2
My guess: 7539   (from 7 possibilities)
0 3
Your secret number is 1357
msl@64Lucid:~/perl$
```



## Perl 6

{{works with|Rakudo|2018.12}}
{{trans|Perl}}


```perl6
# we use the [] reduction meta operator along with the Cartesian Product
# operator X to create the Cartesian Product of four times [1..9] and then get
# all the elements where the number of unique digits is four.
my @candidates = ([X] [1..9] xx 4).grep: *.unique == 4;

repeat {
	my $guess = @candidates.pick;
	my ($bulls, $cows) = read-score;
	@candidates .= grep: &score-correct;

	# note how we declare our two subroutines within the repeat block. This
	# limits the scope in which the routines are known to the scope in which
	# they are needed and saves us a lot of arguments to our two routines.
	sub score-correct($a) {
		my $exact = [+] $a >>==<< $guess;

		# number of elements of $a that match any element of $b
		my $loose = +$a.grep: any @$guess;

		return $bulls == $exact && $cows == $loose - $exact;
	}

	sub read-score() {
		loop {
			my $score = prompt "My guess: {$guess.join}.\n";

			if $score ~~ m:s/^ $<bulls>=(\d) $<cows>=(\d) $/
				and $<bulls> + $<cows> <= 4 {
				return +$<bulls>, +$<cows>;
			}

			say "Please specify the number of bulls and cows";
		}
	}
} while @candidates > 1;

say @candidates
	?? "Your secret number is {@candidates[0].join}!"
	!! "I think you made a mistake with your scoring.";
```

{{out}}

```txt
My guess: 4235.
2 1
My guess: 1435.
2 1
My guess: 1245.
2 1
Your secret number is 1234!
```



## Phix

{{Trans|Euphoria}}

```Phix
constant line = " +---------+-----------------------------+-------+------+\n"
constant digits = "123456789"
 
function mask(integer ch)
    return power(2,ch-'1')
end function

function score(sequence guess, sequence goal)
integer bits = 0, bulls = 0, cows = 0
    for i=1 to length(guess) do
        if guess[i]=goal[i] then
            bulls += 1
        else
            bits += mask(goal[i])
        end if
    end for
    for i=1 to length(guess) do
        cows += (and_bits(bits,mask(guess[i]))!=0)
    end for
    return {bulls, cows}
end function

sequence list = {}
 
procedure pick(integer n, integer got, integer marker, sequence buf)
integer bits = 1
    if got>=n then
        list = append(list,buf)
    else
        for i=0 to length(digits)-1 do
            if not and_bits(marker,bits) then
                buf[got+1] = i+'1'
                pick(n, got+1, or_bits(marker,bits), buf)
            end if
            bits *= 2
        end for
    end if
end procedure
 
procedure filter_list(sequence guess, integer bulls, integer cows)
integer l = length(list), idx = 0
sequence bc = {bulls,cows}
    for i=1 to l do
        if score(guess,list[i])=bc then
            idx += 1
            list[idx] = list[i]
        end if
    end for
    list = list[1..idx]
end procedure
 
procedure game(sequence tgt)
integer n = length(tgt), attempt = 1, bulls = 0, cows 
sequence guess
    pick(n,0,0,repeat(0,n))
    while bulls<n do
        guess = list[rand(length(list))]
        {bulls, cows} = score(guess,tgt)
        printf(1," | Guess %-2d| %9s    %-14s |   %d   |   %d  |\n",
                    {attempt, guess, sprintf("(from %d)",length(list)), bulls, cows})
        filter_list(guess, bulls, cows)
        attempt += 1
    end while
    puts(1,line)
end procedure
 
constant N = 4
sequence secret = shuffle(digits)[1..N]
printf(1,"%s | Secret  | %9s                   | BULLS | COWS |\n%s", {line, secret, line})
game(secret)
```

{{out}}
N=4:

```txt

 +---------+-----------------------------+-------+------+
 | Secret  |      2685                   | BULLS | COWS |
 +---------+-----------------------------+-------+------+
 | Guess 1 |      2951    (from 3024)    |   1   |   1  |
 | Guess 2 |      6541    (from 480)     |   0   |   2  |
 | Guess 3 |      1976    (from 119)     |   0   |   1  |
 | Guess 4 |      3652    (from 26)      |   1   |   2  |
 | Guess 5 |      2685    (from 1)       |   4   |   0  |
 +---------+-----------------------------+-------+------+

```

N=9:

```txt

 +---------+-----------------------------+-------+------+
 | Secret  | 451382796                   | BULLS | COWS |
 +---------+-----------------------------+-------+------+
 | Guess 1 | 392815476    (from 362880)  |   1   |   8  |
 | Guess 2 | 579143826    (from 133497)  |   1   |   8  |
 | Guess 3 | 785614923    (from 48828)   |   0   |   9  |
 | Guess 4 | 627583491    (from 18216)   |   2   |   7  |
 | Guess 5 | 357182649    (from 3719)    |   3   |   6  |
 | Guess 6 | 327461859    (from 321)     |   0   |   9  |
 | Guess 7 | 913582746    (from 53)      |   4   |   5  |
 | Guess 8 | 973285641    (from 6)       |   1   |   8  |
 | Guess 9 | 451382796    (from 2)       |   9   |   0  |
 +---------+-----------------------------+-------+------+

```

N=2:

```txt

 +---------+-----------------------------+-------+------+
 | Secret  |        42                   | BULLS | COWS |
 +---------+-----------------------------+-------+------+
 | Guess 1 |        98    (from 72)      |   0   |   0  |
 | Guess 2 |        62    (from 42)      |   1   |   0  |
 | Guess 3 |        52    (from 10)      |   1   |   0  |
 | Guess 4 |        32    (from 4)       |   1   |   0  |
 | Guess 5 |        12    (from 3)       |   1   |   0  |
 | Guess 6 |        72    (from 2)       |   1   |   0  |
 | Guess 7 |        42    (from 1)       |   2   |   0  |
 +---------+-----------------------------+-------+------+

```

N=1:

```txt

 +---------+-----------------------------+-------+------+
 | Secret  |         7                   | BULLS | COWS |
 +---------+-----------------------------+-------+------+
 | Guess 1 |         3    (from 9)       |   0   |   0  |
 | Guess 2 |         5    (from 8)       |   0   |   0  |
 | Guess 3 |         4    (from 7)       |   0   |   0  |
 | Guess 4 |         8    (from 6)       |   0   |   0  |
 | Guess 5 |         6    (from 5)       |   0   |   0  |
 | Guess 6 |         7    (from 4)       |   1   |   0  |
 +---------+-----------------------------+-------+------+

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(de bullsAndCows ()
   (let Choices (shuffle (mapcan permute (subsets 4 (range 1 9))))
      (use (Guess Bulls Cows)
         (loop
            (prinl "Guessing " (setq Guess (pop 'Choices)))
            (prin "How many bulls and cows? ")
            (setq Bulls (read)  Cows (read))
            (setq Choices
               (filter
                  '((C)
                     (let B (cnt = Guess C)
                        (and
                           (= Bulls B)
                           (= Cows (- (length (sect Guess C)) B)) ) ) )
                  Choices ) )
            (NIL Choices "No matching solution")
            (NIL (cdr Choices) (pack "The answer is " (car Choices))) ) ) ) )
```

Output:

```txt
: (bullsAndCows)
Guessing 4217
How many bulls and cows? 0 2
Guessing 5762
How many bulls and cows? 1 1
Guessing 9372
How many bulls and cows? 0 1
Guessing 7864
How many bulls and cows? 1 2
Guessing 8754
How many bulls and cows? 0 2
-> "The answer is 2468"
```



## Prolog

Works with SWI-Prolog. Use of library clfd written by '''Markus Triska'''.

There is no algorithm. We explain to Prolog the constraints on the numbers according to the previous guesses and we let Prolog decides of the next guess.

The IA :

```Prolog
:- module('ia.pl', [tirage/1]).
:- use_module(library(clpfd)).

% to store the previous guesses and the answers
:- dynamic guess/2.

% parameters of the engine

% length of the guess
proposition(4).

% Numbers of digits
% 0 -> 8
digits(8).


% tirage(-)
tirage(Ms) :-
	% are there previous guesses ?
	(  bagof([P, R], guess(P,R), Propositions)
	->  tirage(Propositions, Ms)
	;   % First try
	    tirage_1(Ms)),
	!.

% tirage_1(-)
% We choose the first Len numbers
tirage_1(L):-
	proposition(Len),
	Max is Len-1,
	numlist(0, Max, L).


% tirage(+,-)
tirage(L, Ms) :-
	proposition(Len),
        length(Ms, Len),

	digits(Digits),

	% The guess contains only this numbers
        Ms ins 0..Digits,
	all_different(Ms),

	% post the constraints
        maplist(placees(Ms), L),

	% compute a possible solution
	label(Ms).

% placees(+, +])
placees(Sol, [Prop, [BP, MP]]) :-
	V #= 0,

	% compute the numbers of digits in good places
	compte_bien_placees(Sol, Prop, V, BP1),
	BP1 #= BP,

	% compute the numbers of digits inbad places
	compte_mal_placees(Sol, Prop, 0, V, MP1),
	MP1 #= MP.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compte_mal_placees(+, +, +, +, -).
% @arg1 : guess to create
% @arg2 : guess already used
% @arg3 : range of the first digit of the previuos arg
% @arg4 : current counter of the digit in bad places
% @arg5 : final counter of the digit in bad places
%
%
compte_mal_placees(_, [], _, MP, MP).

compte_mal_placees(Sol, [H | T], N, MPC, MPF) :-
	compte_une_mal_placee(H, N, Sol, 0,  0, VF),
	MPC1 #= MPC + VF,
	N1 is N+1,
	compte_mal_placees(Sol, T, N1, MPC1, MPF).


% Here we check one digit of an already done guess
% compte_une_mal_placee(+, +, +, +, -).
% @arg1 : the digit
% @arg2 : range of this digit
% @arg3 : guess to create
%         we check each digit of this guess
% @arg4 : range of the digit of this guess
% @arg5 : current counter of the digit in bad places
% @arg6 : final counter of the digit in bad places
%
compte_une_mal_placee(_H, _N, [], _, TT, TT).

% digit in the same range, continue
compte_une_mal_placee(H, NH, [_H1 | T], NH, TTC, TTF) :-
	NH1 is NH + 1, !,
	compte_une_mal_placee(H, NH, T, NH1, TTC, TTF).

% same digit in different places
% increment the counter and continue continue
compte_une_mal_placee(H, NH, [H1 | T], NH1, TTC, TTF) :-
	H #= H1,
	NH \= NH1,
	NH2 is NH1 + 1,
	TTC1 #= TTC + 1,
	compte_une_mal_placee(H, NH, T, NH2, TTC1, TTF).

compte_une_mal_placee(H, NH, [H1 | T], NH1, TTC, TTF) :-
	H #\= H1,
	NH2 is NH1 + 1,
	compte_une_mal_placee(H, NH, T, NH2, TTC, TTF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compte_bien_placees(+, +, +, -)
% @arg1 : guess to create
% @arg2 : previous guess
% @arg3 : current counter of the digit in good places
% @arg4 : final counter of the digit in good places
%
%
compte_bien_placees([], [], MP, MP).

compte_bien_placees([H | T], [H1 | T1], MPC, MPF) :-
	H #= H1,
	MPC1 #= MPC + 1,
	compte_bien_placees(T, T1, MPC1, MPF).

compte_bien_placees([H | T], [H1 | T1], MPC, MPF) :-
	H #\= H1,
	compte_bien_placees(T, T1, MPC, MPF).

```

The code to play :

```Prolog
bulls_and_cows :-
	retractall('ia.pl':guess(_,_)),
	retractall(coups(_)),
	assert(coups(1)),

	repeat,
	(   tirage(Ms)
	->  maplist(add_1, Ms, Ms1),
	    atomic_list_concat(Ms1, Guess),
	    retract(coups(Coup)),
	    Coup_1 is Coup + 1,
	    assert(coups(Coup_1)),
	    format('~w My guess ~w~n', [Coup, Guess]),
	    write('Bulls : '), read(Bulls),
	    write('Cows  : '), read(Cows), nl,
	    assert('ia.pl':guess(Ms, [Bulls, Cows])),
	    Bulls = 4
	;   writeln('Sorry, I can''t find a solution !'), true).

add_1(X, Y) :-
	Y is X + 1.

```

A game :

```txt
 ?- bulls_and_cows.
1 My guess 1234
Bulls : 0.
Cows  : 0.

2 My guess 5678
Bulls : 1.
Cows  : 2.

3 My guess 5769
Bulls : 0.
Cows  : 3.

4 My guess 8695
Bulls : 0.
Cows  : 3.

5 My guess 9876
Bulls : 4.
Cows  : 0.

true .

```



## PureBasic


```PureBasic
#answerSize = 4
Structure history
  answer.s
  bulls.i
  cows.i
EndStructure

Procedure evaluateGuesses(*answer.history, List remainingGuesses.s())
  Protected i, cows, bulls

  ForEach remainingGuesses()
    bulls = 0: cows = 0 
    For i = 1 To #answerSize
      If Mid(remainingGuesses(), i, 1) = Mid(*answer\answer, i, 1)
        bulls + 1
      ElseIf FindString(remainingGuesses(), Mid(*answer\answer, i, 1), 1)
        cows + 1
      EndIf 
    Next
    If bulls <> *answer\bulls Or cows <> *answer\cows
      DeleteElement(remainingGuesses())
    EndIf
  Next
EndProcedure

Procedure findPermutations(List permutations.s(), elementChar.s, permSize)
  Protected i, j, stackDepth, elementCount = Len(elementChar) - 1, working.s = Space(permSize), *working = @working
  permSize - 1
  Dim stack(permSize) ;holds index states
  
  Dim elements(elementCount)
  Dim elementChar.c(elementCount)
  For i = 0 To elementCount
    elementChar(i) = PeekC(@elementChar + i * SizeOf(Character))
  Next
  
  i = 0
  Repeat 
    While i <= elementCount
      If elements(i) = 0
        stack(stackDepth) = i
        If stackDepth = permSize
          For j = 0 To permSize
            PokeC(*working + j * SizeOf(Character), elementChar(stack(j)))
          Next
          AddElement(permutations())
          permutations() = working
        Else
          elements(i) = 1
          stackDepth + 1
          i = 0
          Continue ;skip update
        EndIf 
      EndIf 
      i + 1
    Wend
    stackDepth - 1
    If stackDepth < 0
      Break
    EndIf 
    i = stack(stackDepth) + 1
    elements(i - 1) = 0
  ForEver
EndProcedure


If OpenConsole()
  Define guess.s, guessNum, score.s, delimeter.s
  NewList remainingGuesses.s()
  NewList answer.history()
  findPermutations(remainingGuesses(), "123456789", 4)
  
  PrintN("Playing Bulls & Cows with " + Str(#answerSize) + " unique digits." + #CRLF$)
  Repeat
    If ListSize(remainingGuesses()) = 0
      If answer()\bulls = #answerSize And answer()\cows = 0
        PrintN(#CRLF$ + "Solved!")
        Break ;exit Repeat/Forever
      EndIf
      
      PrintN(#CRLF$ + "BadScoring!  Nothing fits the scores you gave.")
      ForEach answer()
        PrintN(answer()\answer + " -> [" + Str(answer()\bulls) + ", " + Str(answer()\cows) + "]")
      Next
      Break ;exit Repeat/Forever
    Else
      guessNum + 1
      SelectElement(remainingGuesses(), Random(ListSize(remainingGuesses()) - 1))
      guess = remainingGuesses()
      DeleteElement(remainingGuesses())
      
      Print("Guess #" + Str(guessNum) + " is " + guess + ".  What does it score (bulls, cows)?")
      score = Input()
      If CountString(score, ",") > 0: delimeter = ",": Else: delimeter = " ": EndIf
      
      AddElement(answer())
      answer()\answer = guess
      answer()\bulls = Val(StringField(score, 1, delimeter))
      answer()\cows = Val(StringField(score, 2, delimeter))
      evaluateGuesses(@answer(), remainingGuesses())
    EndIf
  ForEver
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input() 
  CloseConsole()
EndIf
```

Sample output:

```txt
Playing Bulls & Cows with 4 unique digits.

Guess #1 is 6273.  What does it score (bulls, cows)?0,2
Guess #2 is 7694.  What does it score (bulls, cows)?0,2
Guess #3 is 9826.  What does it score (bulls, cows)?0,3
Guess #4 is 2569.  What does it score (bulls, cows)?2,0
Guess #5 is 2468.  What does it score (bulls, cows)?4,0

Solved!


Press ENTER to exit
```



## Python


```python
from itertools import permutations
from random import shuffle

try:
    raw_input
except:
    raw_input = input
try:
    from itertools import izip
except:
    izip = zip
    
digits = '123456789'
size = 4

def parse_score(score):
    score = score.strip().split(',')
    return tuple(int(s.strip()) for s in score)

def scorecalc(guess, chosen):
    bulls = cows = 0
    for g,c in izip(guess, chosen):
        if g == c:
            bulls += 1
        elif g in chosen:
            cows += 1
    return bulls, cows

choices = list(permutations(digits, size))
shuffle(choices)
answers = []
scores  = []

print ("Playing Bulls & Cows with %i unique digits\n" % size)
       
while True:
    ans = choices[0]
    answers.append(ans)
    #print ("(Narrowed to %i possibilities)" % len(choices))
    score = raw_input("Guess %2i is %*s. Answer (Bulls, cows)? "
                      % (len(answers), size, ''.join(ans)))
    score = parse_score(score)
    scores.append(score)
    #print("Bulls: %i, Cows: %i" % score)
    found =  score == (size, 0)
    if found:
        print ("Ye-haw!")
        break
    choices = [c for c in choices if scorecalc(c, ans) == score]
    if not choices:
        print ("Bad scoring? nothing fits those scores you gave:")
        print ('  ' +
               '\n  '.join("%s -> %s" % (''.join(an),sc)
                           for an,sc in izip(answers, scores)))
        break
```


'''Sample output'''

```txt
Playing Bulls & Cows with 4 unique digits

Guess  1 is 1935. Answer (Bulls, cows)? 0,2
Guess  2 is 4169. Answer (Bulls, cows)? 0,3
Guess  3 is 6413. Answer (Bulls, cows)? 1,1
Guess  4 is 9612. Answer (Bulls, cows)? 1,1
Guess  5 is 9481. Answer (Bulls, cows)? 3,0
Guess  6 is 9471. Answer (Bulls, cows)? 4,0
Ye-haw!
```


'''Sample bad output'''

If the scores are inconsistent you get output like:

```txt
Playing Bulls & Cows with 4 unique digits

Guess  1 is 1549. Answer (Bulls, cows)? 0,0
Guess  2 is 3627. Answer (Bulls, cows)? 1,0
Bad scoring? nothing fits those scores you gave:
  1549 -> (0, 0)
  3627 -> (1, 0)
```



## Racket

Generate the list of possible choices. Each choice is represented as list of 4 numbers.

```Racket
#lang racket/base
(require racket/string
         racket/list)

(define (permutations-getall items size)
  (if (zero? size)
      '(())
      (for/list ([tail (in-list (permutations-getall items (- size 1)))]
                  #:when #t
                  [i (in-list items)]
                  #:unless (member i tail))
        (cons i tail))))

(define digits  (list 1 2 3 4 5 6 7 8 9))

(define size 4)

(define all-choices (shuffle (permutations-getall digits size)))

(define (listnum->string list)
  (apply string-append (map number->string list)))
```


Now define some auxiliary functions to parse the user input (with a minimum error checking) and calculate the score of another possible guess.

```Racket
 (define (parse-score score)
  (if (string? score)
      (let ([splited-score (string-split score ",")])
        (if (= (length (string-split score ",")) 2)
            (apply values (map (lambda (s) (string->number (string-trim s))) splited-score))
            (values #f #f)))
      (values #f #f)))
 
(define (calculate-score guess chosen)
  (define (in-chosen x) (member x chosen))
  (let ([bulls (count = guess chosen)]
        [cows+bulls (count in-chosen guess)])
    (values bulls (- cows+bulls bulls))))
```


Main part of the game.

```Racket
(printf "Playing Bulls & Cows with ~a unique digits\n" size)
 
(let loop ([choices all-choices] [num 1])
  (if (null? choices)
      (printf "Bad scoring! nothing fits those scores you gave.")
      (let ([guess (car choices)])
        #;(printf "(Narrowed to ~a possibilities)\n" (length choices))
        (printf "Guess #~a is ~a. Answer: Bulls, Cows? " num (listnum->string guess))
        (let-values ([(bulls cows) (parse-score (read-line))])
          ;parse-score returns (#f #f) on errors
          (if (and bulls cows)
              (begin 
                (printf "Bulls: ~a, Cows: ~a\n" bulls cows)
                (if (and (= bulls size) (= cows 0))
                    (printf "Ye-haw!")
                    (let () 
                      (define (equal-score? chosen) 
                        (let-values ([(c-bulls c-cows) (calculate-score guess chosen)])
                          (and (= c-bulls bulls) (= c-cows cows))))
                      (loop (filter equal-score? choices) (+ num 1)))))
              (begin 
                (printf "Sorry, I didn't understand that. Please try again.\n")
                (loop choices num)))))))
```


'''Sample Output:'''

```txt
Playing Bulls & Cows with 4 unique digits
Guess #1 is 3958. Answer: Bulls, Cows? 0,1
Bulls: 0, Cows: 1
Guess #2 is 1364. Answer: Bulls, Cows? bad
Sorry, I didn't understand that. Please try again.
Guess #2 is 1364. Answer: Bulls, Cows? 2,1
Bulls: 2, Cows: 1
Guess #3 is 4362. Answer: Bulls, Cows? 0,3
Bulls: 0, Cows: 3
Guess #4 is 1234. Answer: Bulls, Cows? 4,0
Bulls: 4, Cows: 0
Ye-haw!
```


'''Sample Output: Wrong scoring'''

```txt
Playing Bulls & Cows with 4 unique digits
Guess #1 is 7189. Answer: Bulls, Cows? 0,0
Bulls: 0, Cows: 0
Guess #2 is 2453. Answer: Bulls, Cows? 1,0
Bulls: 1, Cows: 0
Bad scoring! nothing fits those scores you gave.
```



## REXX

About a third of the REXX program deals with presentation and/or validation of answers.

```rexx
/*REXX program plays the    Bulls & Cows   game with  CBLFs (Carbon Based Life Forms).  */
parse arg ? .;   if datatype(?,'W')  then call random ,,? /*Random seed? Make repeatable*/
L=1234;  H=9876;    call gen@                             /*generate all possibilities. */
       do forever;  g=random(L,H); if @.g\==.  then leave /*obtain a random 1st guess.  */
       end   /*forever*/                                  /* [↑]  obtain rand 1st guess.*/
$$1= '───── How many bulls and cows were guessed with '; $$2=" ?            [─── or QUIT]"
       do until #()<2 | bull==4;   say;   call ask        /*examine @ list;  get answer.*/
           do ?=L  to H;     if @.?==.  then iterate      /*is this already eliminated ?*/
           call bull#  ?,g                                /*obtain bulls and cows count.*/
           if bull\==bulls | cow\==cows  then @.?=.       /*eliminate this possibility. */
           end   /*?*/
       end   /*until*/

if #==0  then do;  call serr  "At least one of your responses was invalid.";  exit;    end
say;   say "           ╔═════════════════════════════════════════════════╗"
       say "           ║                                                 ║"
       say "           ║   Your secret Bulls and Cows number is: " g  "  ║"
       say "           ║                                                 ║"
       say "           ╚═════════════════════════════════════════════════╝";           say
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
#:     #=0;    do k=L  to H;  if @.k==.  then iterate;  #=#+1;   g=k;  end;       return #
gen@:  @.=.;   do j=L  to H;  if \rep() & pos(0, j)==0  then @.j=j;    end;       return
rep:   do k=1  for 3;  if pos(substr(j, k, 1), j, k+1)\==0  then return 1;   end; return 0
serr:  say;    say  '───── ***error***   '      !     arg(1);                     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
bull#: parse arg n,q;     w=length(n);    bulls=0      /*W: # digits in N;  bull cntr=0 */
              do j=1  for w;    if substr(n, j, 1) \== substr(q, j, 1)  then iterate
              bulls=bulls+1;    q=overlay(., q, j)     /*bump counter;  disallow for cow*/
              end   /*j*/                              /* [↑]  bull count═══════════════*/
       cows=0                                          /*set the number of cows to zero.*/
              do k=1  for w;    _=substr(n, k, 1);   if pos(_, q)==0  then iterate
              cows=cows + 1;    q=translate(q, , _)    /*bump counter;  allow multiple #*/
              end   /*k*/                              /* [↑]  cow  count═══════════════*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
ask:   do forever; say $$1 g $$2;  pull x 1 bull cow . /*display prompt;  obtain answer.*/
          select                                       /* [↑]  PULL capitalizes the args*/
          when abbrev('QUIT', x, 1)  then exit         /*the user wants to quit playing.*/
          when bull == ''            then != "no numbers were entered."
          when cow  == ''            then != "not enough numbers were entered."
          when words(x) > 2          then != "too many numbers entered: "              x
          when \datatype(bull, 'W')  then != "1st number (bulls) not an integer: "    bull
          when \datatype(cow , 'W')  then != "2nd number (cows) not an integer: "     cow
          when bull <0 | bull >4     then != "1st number (bulls) not 0 ──► 4: "       bull
          when cow  <0 | cow  >4     then != "2nd number (cows) not 0 ──► 4: "        cow
          when bull + cow > 4        then != "sum of bulls and cows can't be > 4: "    x
          otherwise                       !=
          end   /*select*/
       if !\==''  then do;  call serr;  iterate;  end  /*prompt the user and try again. */
       bull=bull/1;         cow=cow/1;  return         /*normalize bulls & cows numbers.*/
       end     /*forever*/
```





## Ring


```ring

# Project : Bulls and cows/Player

secret = ""
while len(secret) != 4
        c = char(48 + random(9))
        if substr(secret, c) = 0 
           secret = secret + c
        ok
end 
see "secret = " + secret + nl
 
possible = ""
for i = 1234 to 9876
     possible = possible + string(i)
next
 
see "guess a four-digit number with no digit used twice." + nl
guesses = 0
while true 
        bulls = 0
        cows = 0
        if len(possible) = 4 
           guess = possible
        else
           guess = substr(possible, 4*random(len(possible) / 4) - 3, 4)
        ok 
        see "computer guesses " + guess + nl
        guesses = guesses + 1 
        if guess = secret
           see "correctly guessed after " + guesses + " guesses!" + nl
           exit
        ok 
        if len(guess) = 4
           count(secret, guess, bulls, cows)
        ok
        i = 1
        testbulls = 0
        testcows = 0
        while  i <= len(possible)
                 temp = substr(possible, i, 4)
                 if len(guess) = 4
                    count(temp, guess, testbulls, testcows)
                 ok
                 if bulls=testbulls 
                    if cows=testcows 
                       i = i + 4
                    ok
                 else
                    possible = left(possible, i-1) + substr(possible, i+4)
                 ok
        end 
        if substr(possible, secret) = 0 
           exit
        ok 
end
 
func count(secret, guess, bulls,  cows)
       bulls = 0
       cows = 0
       for nr = 1 to 4
            c = secret[nr]
            if guess != 0
               if guess[nr] = c 
                  bulls = bulls + 1
                  if substr(guess, c) > 0
                     cows = cows + 1
                  ok
               ok
            ok
       next 
       see "giving " + bulls + " bull(s) and " + cows  + " cow(s)." + nl
       return [bulls, cows]

```



## Ruby

Ruby Version 1.9+

```ruby
size = 4
scores = []
guesses = []
puts "Playing Bulls & Cows with #{size} unique digits."
possible_guesses = [*'1'..'9'].permutation(size).to_a.shuffle

loop do
  guesses << current_guess = possible_guesses.pop
  print "Guess #{guesses.size} is #{current_guess.join}. Answer (bulls,cows)? "
  scores << score = gets.split(',').map(&:to_i)
  
  # handle win
  break (puts "Yeah!") if score == [size,0]
  
  # filter possible guesses
  possible_guesses.select! do |guess|
    bulls = guess.zip(current_guess).count{|g,cg| g == cg}
    cows = size - (guess - current_guess).size - bulls
    [bulls, cows] == score
  end
  
  # handle 'no possible guesses left'
  if possible_guesses.empty?
    puts "Error in scoring?"
    guesses.zip(scores).each{|g, (b, c)| puts "#{g.join} => bulls #{b} cows #{c}"}
    break
  end
end
```

'''Regular output'''

```txt

Playing Bulls & Cows with 4 unique digits.
Guess 1 is 7158. Answer (bulls,cows)? 0,1
Guess 2 is 6843. Answer (bulls,cows)? 0,2
Guess 3 is 1439. Answer (bulls,cows)? 2,1
Guess 4 is 3479. Answer (bulls,cows)? 0,2
Guess 5 is 1234. Answer (bulls,cows)? 4,0
Yeah!

```

'''Wrong scoring'''

```txt

Playing Bulls & Cows with 4 unique digits.
Guess 1 is 2857. Answer (bulls,cows)? 0,0
Guess 2 is 6419. Answer (bulls,cows)? 1,0
Error in scoring?
2857 => bulls 0 cows 0
6419 => bulls 1 cows 0

```



## Scala


```scala

    def allCombinations: Seq[List[Byte]] = {
      (0 to 9).map(_.byteValue).toList.combinations(4).toList.flatMap(_.permutations)
    }
    
    def nextGuess(possible: Seq[List[Byte]]): List[Byte] = possible match {
      case Nil => throw new IllegalStateException 
      case List(only) => only
      case _ => possible(Random.nextInt(possible.size))
    }
    
    def doGuess(guess: List[Byte]): Pair[Int, Int] = {
      println("My guess is " + guess);
      val arr = readLine().split(' ').map(Integer.valueOf(_))
      (arr(0), arr(1))
    }
    
    def testGuess(alt: List[Byte], guess: List[Byte]): Pair[Int, Int] = {
      val bulls =alt.zip(guess).filter(p => p._1 == p._2).size
      val cows = guess.filter(alt.contains(_)).size - bulls
      (bulls, cows)
    }
    
    def play(possible: Seq[List[Byte]]): List[Byte] = {
      val curGuess = nextGuess(possible)
      val bc = doGuess(curGuess)
      if (bc._1 == 4) { println("Ye-haw!"); curGuess } 
      else 
        play(possible.filter(p => testGuess(p, curGuess) == bc))
      
    }
  
    def main(args: Array[String]) {
      play(allCombinations)
    }

```

{{out}}

```txt

My guess is List(5, 8, 2, 7)
0 1
My guess is List(1, 0, 8, 9)
1 0
My guess is List(7, 0, 4, 3)
0 2
My guess is List(4, 5, 3, 9)
1 1
My guess is List(6, 3, 7, 9)
0 1
My guess is List(1, 2, 3, 4)
4 0
Ye-haw!

```




## Sidef


```ruby
# Build a list of all possible solutions.  The regular expression weeds
# out numbers containing zeroes or repeated digits.
var candidates = (1234..9876 -> grep {|n| !("#{n}" =~ /0 | (\d) .*? \1 /x) }.map{.digits});

# Repeatedly prompt for input until the user supplies a reasonable score.
# The regex validates the user's input and then returns two numbers.
func read_score(guess) {
    loop {
        "My guess: %s   (from %d possibilities)\n" \
            -> printf(guess.join, candidates.len);

        if (var m = (Sys.scanln("bulls cows: ") =~ /^\h*(\d)\h*(\d)\h*$/)) {
            var (bulls, cows) = m.cap.map{.to_i}...;
            bulls+cows <= 4 && return(bulls, cows);
        }

        say "Please specify the number of bulls and the number of cows";
    }
}

func score_correct(a, b, bulls, cows) {
    var (exact, loose) = (0, 0);

    for i in ^4 {
        a[i] == b[i] ? ++exact
                     : (a[i]~~b && ++loose)
    }

    (bulls == exact) && (cows == loose)
}

# Pick a number, display it, get the score, and discard candidates
# that don't match the score:
loop {
    var guess = candidates.pick;
    var (bulls, cows) = read_score(guess);
    candidates.grep!{|n| score_correct(n, guess, bulls, cows) }
    candidates.len > 1 || break
}

# Print the secret number or the error message
(
    candidates.len == 1 ? ("Your secret number is: %d" % candidates[0].join)
                        : ("I think you made a mistake with your scoring")
)->say
```


'''Output:'''

```txt

My guess: 7432   (from 3024 possibilities)
bulls cows: 0 1
My guess: 9216   (from 720 possibilities)
bulls cows: 1 1
My guess: 6813   (from 128 possibilities)
bulls cows: 0 1
My guess: 9157   (from 24 possibilities)
bulls cows: 1 3
Your secret number is: 9571

```



## Tcl

{{trans|Python}}
{{tcllib|struct::list}}
{{tcllib|struct::set}}

```tcl
package require struct::list
package require struct::set

proc scorecalc {guess chosen} {
    set bulls 0
    set cows 0
    foreach g $guess c $chosen {
	if {$g eq $c} {
	    incr bulls
	} elseif {$g in $chosen} {
	    incr cows
	}
    }
    return [list $bulls $cows]
}

# Allow override on command line
set size [expr {$argc ? int($argv) : 4}]

set choices {}
struct::list foreachperm p [split 123456789 ""] {
    struct::set include choices [lrange $p 1 $size]
}
set answers {}
set scores {}

puts "Playing Bulls & Cows with $size unique digits\n"
fconfigure stdout -buffering none
while 1 {
    set ans [lindex $choices [expr {int(rand()*[llength $choices])}]]
    lappend answers $ans
    puts -nonewline \
	"Guess [llength $answers] is [join $ans {}]. Answer (Bulls, cows)? "
    set score [scan [gets stdin] %d,%d]
    lappend scores $score
    if {$score eq {$size 0}} {
	puts "Ye-haw!"
	break
    }
    foreach c $choices[set choices {}] {
	if {[scorecalc $c $ans] eq $score} {
	    lappend choices $c
	}
    }
    if {![llength $choices]} {
	puts "Bad scoring? nothing fits those scores you gave:"
	foreach a $answers s $scores {
	    puts "  [join $a {}] -> ([lindex $s 0], [lindex $s 1])"
	}
	break
    }
}
```

'''Sample Output'''

```txt

Playing Bulls & Cows with 4 unique digits

Guess 1 is 8527. Answer (Bulls, cows)? 0,1
Guess 2 is 5143. Answer (Bulls, cows)? 0,2
Guess 3 is 9456. Answer (Bulls, cows)? 2,0
Guess 4 is 9412. Answer (Bulls, cows)? 2,1
Guess 5 is 9481. Answer (Bulls, cows)? 3,0
Guess 6 is 9471. Answer (Bulls, cows)? 4,0
Ye-haw!

```

'''Sample Bad Output'''

```txt

Playing Bulls & Cows with 4 unique digits

Guess 1 is 6578. Answer (Bulls, cows)? 0,0
Guess 2 is 3241. Answer (Bulls, cows)? 1,0
Bad scoring? nothing fits those scores you gave:
  6578 -> (0, 0)
  3241 -> (1, 0)

```



## VBA



```vb

Option Explicit
 
Sub Main_Bulls_And_Cows_Player()
Dim collSoluces As New Collection, Elem As Variant, Soluce As String
Dim strNumber As String, cpt As Byte, p As Byte
Dim i As Byte, Bulls() As Boolean, NbBulls As Byte, Cows As Byte, Poss As Long
Const NUMBER_OF_DIGITS As Byte = 4
        
        strNumber = CreateNb(NUMBER_OF_DIGITS)
        Debug.Print "TOSS : " & StrConv(strNumber, vbUnicode)
        Debug.Print "---------- START ------------"
        Set collSoluces = CollOfPossibleNumbers
        Poss = collSoluces.Count
        For Each Elem In collSoluces
            'Debug.Print "Number of possibilities : " & Poss
            Debug.Print "Attempt : " & StrConv(Elem, vbUnicode)
            NbBulls = 0: Soluce = Elem
            ReDim Bulls(NUMBER_OF_DIGITS - 1)
            For i = 1 To NUMBER_OF_DIGITS
                If IsBull(strNumber, Mid(Elem, i, 1), i) Then
                    Bulls(i - 1) = True: NbBulls = NbBulls + 1
                    RemoveIfNotBull collSoluces, Mid(Elem, i, 1), i
                End If
            Next i
            Cows = 0
            For i = 1 To NUMBER_OF_DIGITS
                If Not Bulls(i - 1) Then
                    If IsCow(collSoluces, strNumber, Mid(Elem, i, 1), p) Then
                        If Not Bulls(p - 1) Then Cows = Cows + 1
                    End If
                End If
            Next i
            Poss = collSoluces.Count
            Debug.Print "Bulls : " & NbBulls & ", Cows : " & Cows
            If Poss = 1 Then Exit For
        Next
                Debug.Print "---------- THE END ------------"
        Debug.Print "TOSS WAS : " & StrConv(strNumber, vbUnicode) & " We found : " & StrConv(Soluce, vbUnicode)
End Sub
 
Function CreateNb(NbDigits As Byte) As String
Dim myColl As New Collection
Dim strTemp As String
Dim bytAlea As Byte
 
    Randomize
    Do
        bytAlea = Int((Rnd * 9) + 1)
        On Error Resume Next
        myColl.Add CStr(bytAlea), CStr(bytAlea)
        If Err <> 0 Then
            On Error GoTo 0
        Else
            strTemp = strTemp & CStr(bytAlea)
        End If
    Loop While Len(strTemp) < NbDigits
    CreateNb = strTemp
End Function
 
Function CollOfPossibleNumbers() As Collection
Dim TempColl As New Collection
Dim x As String
Dim i As Long
Dim Flag As Boolean
Dim b As Byte
 
    For i = 1234 To 9876
        Flag = False
        For b = 1 To 4
            x = CStr(i)
            If Len(Replace(x, Mid(x, b, 1), "")) < 3 Then
                Flag = True: Exit For
            End If
        Next
        If Not Flag Then TempColl.Add x, x
    Next i
    Set CollOfPossibleNumbers = TempColl
End Function
 
Function IsBull(strgNb As String, Digit As String, place As Byte) As Boolean
    IsBull = (Mid(strgNb, place, 1) = Digit)
End Function

Function IsCow(C As Collection, strgNb As String, Digit As String, place As Byte) As Boolean
    If (InStr(strgNb, Digit) > 0) Then
        IsCow = True: place = InStr(strgNb, Digit)
        RemoveIfNotCow C, Digit
    End If
End Function
 
Sub RemoveIfNotBull(C As Collection, Digit As String, place As Byte)
Dim E As Variant
 
    For Each E In C
        If Mid(E, place, 1) <> Digit Then C.Remove E
    Next
End Sub
 
Sub RemoveIfNotCow(C As Collection, Digit As String)
Dim E As Variant
 
    For Each E In C
        If (InStr(E, Digit) = 0) Then C.Remove E
    Next
End Sub

```

{{out}}

```txt
TOSS : 9 2 4 7 
---------- START ------------
Attempt : 1 2 3 4 
Bulls : 1, Cows : 1
Attempt : 1 2 4 0 
Bulls : 2, Cows : 0
Attempt : 1 2 4 3 
Bulls : 2, Cows : 0
Attempt : 1 2 4 5 
Bulls : 2, Cows : 0
Attempt : 1 2 4 6 
Bulls : 2, Cows : 0
Attempt : 1 2 4 7 
Bulls : 3, Cows : 0
Attempt : 3 2 4 7 
Bulls : 3, Cows : 0
Attempt : 5 2 4 7 
Bulls : 3, Cows : 0
Attempt : 6 2 4 7 
Bulls : 3, Cows : 0
Attempt : 8 2 4 7 
Bulls : 3, Cows : 0
Attempt : 9 2 4 7 
Bulls : 4, Cows : 0
---------- THE END ------------
TOSS WAS : 9 2 4 7  We found : 9 2 4 7 
```



## Yabasic

{{trans|Liberty BASIC}}

```Yabasic

clear screen

guesses = 0

void = ran()

while(len(secret$) < 4)    //    zero not allowed
    n$ = chr$(int(ran(1) * 9) + 49)
    if not(instr(secret$, n$)) secret$ = secret$ + n$
wend

print " Secretly, my opponent just chose a number. But she didn't tell anyone!\n\t\t\t\t", secret$, "."
print "     I can however be given a score for my guesses."

for i = 1234 to 9876
    if check(str$(i)) = 0 then
    	available$ = available$ + " " + str$(i)
    	k = k +1
    end if
next i

available$ = trim$(available$)  //   remove the surplus, leading space

while(true)
    print
    print "Currently holding ", k, " possible numbers. "

    guess$ =word$(available$, 1 + int(k * ran(1)), " ")
    print "Computer guessed ", guess$, " & got ";

    bulls = 0
    cows = 0
    guesses = guesses + 1

    r$ = score$(guess$, secret$)

    bulls = val(word$(r$, 1, ","))
    cows  = val(word$(r$, 2, ","))

    print bulls, " bull(s), and ", cows, " cow(s)."

    if guess$ = secret$ then
        print "\nComputer won after ", guesses, " guesses!";
        print " That took ", right$(time$, 1), " seconds. ENDED!"
        break
    end if
    kk = 0
    new$ = ""

    for j = 1 to k
        bullsT = 0
        cowsT = 0

        possible$ = word$(available$, j, " ")

        r$ = score$(guess$, possible$)

        bullsT = val(word$(r$, 1, ","))
        cowsT = val(word$(r$, 2, ","))

        if (bullsT = bulls) and ( cowsT = cows)  then
            new$ = new$ + " " + possible$    //    keep those with same score
            kk = kk + 1
        end if
    next j

    available$ = trim$(new$)
    k = kk
    
wend

sub score$(a$, b$)    //   return as a csv string the number of bulls & cows.
    local i, c$, bulls, cows
	
    bulls = 0 : cows = 0
    for i = 1 to 4
        c$ = mid$(a$, i, 1)
        if mid$(b$, i, 1) = c$ then
            bulls = bulls + 1
        else
            if (instr(b$, c$) <> 0) and (instr(b$, c$) <> i) cows = cows + 1
        end if
    next i
    return str$(bulls) + "," + str$(cows)
end sub

sub check(i$)
    local t, i, j
	
    t = 0    //    zero flags available: 1 means not available
    for i = 1 to 3
        for j = i + 1 to 4
            if mid$(i$, i, 1) = mid$(i$, j, 1) t = 1
        next j
    next i
    if instr(i$, "0") t = 1
    return t
end sub

sub word$(l$, i, d$)
    local c$(1), n
	
    n = token(l$, c$(), d$)
    return c$(i)
end sub
```



## zkl

{{trans|D}}

```zkl
d9:="123456789";
choices:=Walker.cproduct(d9,d9,d9,d9).pump(List,// lazy,-->3024, order is important
   fcn(list){ s:=list.concat(); (s.unique().len()==4) and s or Void.Skip });
do{
   guess:=choices[(0).random(choices.len())];
   score:=ask("My guess is %s. How many bulls and cows? ".fmt(guess)).strip();
   bulls,cows:=score.split("").apply("toInt");  // "12"-->(1,2)
   choices=choices.filter('wrap(c){
      bulls==c.zipWith('==,guess).sum(0) and  // 0 + True == 1
      cows ==c.zipWith('wrap(a,b){ a!=b and guess.holds(a) },guess).sum(0)
   });
}while(choices.len()>1);

if(not choices) "Nothing fits the scores you gave.".println();
else            "Solution found: ".println(choices[0]);
```

{{out}}

```txt

My guess is 1842. How many bulls and cows? 12
My guess is 7824. How many bulls and cows? 11
My guess is 1425. How many bulls and cows? 12
My guess is 1294. How many bulls and cows? 30
My guess is 1264. How many bulls and cows? 30
Solution found: 1234

```


{{omit from|GUISS}}

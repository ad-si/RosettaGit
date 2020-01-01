+++
title = "Number reversal game"
description = ""
date = 2019-07-25T22:09:55Z
aliases = []
[extra]
id = 6745
[taxonomies]
categories = []
tags = []
+++

{{task|Games}}
[[Category:Puzzles]]

;Task:
Given a jumbled list of the numbers '''1''' to '''9''' that are definitely ''not'' in
ascending order.

Show the list,   and then ask the player how many digits from the
left to reverse.

Reverse those digits,   then ask again,   until all the digits end up in ascending order.


The score is the count of the reversals needed to attain the ascending order.


Note: Assume the player's input does not need extra validation.


;Related tasks:
*   [[Sorting algorithms/Pancake sort]]
*   [[wp:Pancake sorting|Pancake sorting]].
*   [[Topswops]]





## Ada


```ada

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Numerics.Discrete_Random;

procedure NumberReverse is

   subtype RandRange is Integer range 1..9;
   type NumArrayType is array (Integer range 1..9) of Integer;

   package RandNumbers is new Ada.Numerics.Discrete_Random(RandRange);
   use RandNumbers;

   G : Generator;

   procedure FillArray (A : in out NumArrayType) is
      Temp : RandRange;
   begin
      A := (others => 0);
      for I in 1..9 loop
         Temp := Random(G);
         while A(Temp) /= 0 loop
            Temp := Random(G);
         end loop;
         A(Temp) := I;
      end loop;
   end FillArray;

   procedure Put(A : in NumArrayType) is
   begin
      for I in 1..9 loop
         Put(A(I), 0);
         Put(" ");
      end loop;
   end Put;

   procedure Prompt (Index : out Integer) is
   begin
      New_Line;
      Put("How many numbers would you like to reverse: ");
      Get(Index);
   end Prompt;

   procedure ReverseArray(Arr : in out NumArrayType;
                          Index : in Integer) is
      Temp : RandRange;
   begin
      for I in 1..Index/2 loop
         Temp := Arr(I);
         Arr(I) := Arr(Index + 1 - I);
         Arr(Index + 1 - I) := Temp;
      end loop;
   end ReverseArray;

   Sorted : constant NumArrayType := (1,2,3,4,5,6,7,8,9);
   Arr    : NumArrayType;
   Index  : Integer;
   Count  : Integer := 0;
begin
   Reset(G);
   loop
      FillArray(Arr);
      exit when Sorted /= Arr;
   end loop;
   loop
      Put(Arr);
      Prompt(Index);
      Count := Count + 1;
      ReverseArray(Arr, Index);
      exit when Sorted = Arr;
   end loop;
   Put(Arr);
   New_Line;
   Put("Congratulations! You win. It took " &
         Integer'Image(Count) & " tries.");
end NumberReverse;

```



## Astro


```python
print '# Number reversal game'

var data, trials = list(1..9), 0

while data == sort data:
    random.shuffle data

while data != sort data:
    trials += 1
    flip = int input '#${trials}: LIST: ${join data} Flip how many?: '
    data[:flip] = reverse data[:flip]

print '\nYou took ${trials} attempts to put digits in order!'
```



## AutoHotkey


```Autohotkey
; Submitted by MasterFocus --- http://tiny.cc/iTunis

ScrambledList := CorrectList := "1 2 3 4 5 6 7 8 9" ; Declare two identical correct sequences
While ( ScrambledList = CorrectList )
  Sort, ScrambledList, Random D%A_Space% ; Shuffle one of them inside a While-loop to ensure it's shuffled

Attempts := 0
While ( ScrambledList <> CorrectList ) ; Repeat until the sequence is correct
{
  InputBox, EnteredNumber, Number Reversal Game, Attempts so far: %Attempts%`n`nCurrent sequence: %ScrambledList%`n`nHow many numbers (from the left) should be flipped?, , 400, 200
  If ErrorLevel
    ExitApp ; Exit if user presses ESC or Cancel
  If EnteredNumber is not integer
    Continue ; Discard attempt if entered number is not an integer
  If ( EnteredNumber <= 1 )
    Continue ; Discard attempt if entered number is <= 1
  Attempts++ ; Increase the number of attempts
  ; Reverse the first part of the string and add the second part
  ; The entered number is multiplied by 2 to deal with the spaces
  ScrambledList := Reverse(SubStr(ScrambledList,1,(EnteredNumber*2)-1)) SubStr(ScrambledList,EnteredNumber*2)
}

MsgBox, You took %Attempts% attempts to get the correct sequence. ; Final message

Return

;-------------------

Reverse(Str) ; Auxiliar function (flips a string)
{
  Loop, Parse, Str
    Out := A_LoopField Out
  Return Out
}
```



## AWK


```awk
BEGIN {
	print "\nWelcome to the number reversal game!\n"

	print "You must put the numbers in order from 1 to 9."
	print "Your only moves are to reverse groups of numbers"
	print "on the left side of the list."

	newgame()
	prompt()
}

# start a new game
function newgame(  i, j, t) {
	# score = number of moves
	score = 0

	# list = list of numbers
	split("123456789", list, "")
	do {
		# Knuth shuffle
		for (i = 9; i > 1; i--) {
			j = int(i * rand()) + 1
			t = list[i]
			list[i] = list[j]
			list[j] = t
		}
	} while (win())
}

# numbers in order?
function win(  i) {
	# check if list[1] == 1, list[2] == 2, ...
	for (i = 1; i <= 9; i++) if (list[i] != i) return 0
	return 1
}

# user prompt
function prompt(  i) {
	printf "\nYour list: "
	for (i = 1; i < 9; i++) printf "%d, ", list[i]
	printf "%d\n", list[9]

	if (win()) {
		print "YOU WIN!"
		printf "Your score is %d moves.\n", score
		printf "Would you like to play again (y/n)? "
		again = 1
	} else {
		printf "How many numbers to reverse? "
	}
}

# user input in $1
{
	if (again) {
		again = 0
		if (tolower(substr($1, 1, 1)) == "y")
			newgame()
		else
			exit
	} else {
		score += 1
		reverse($1)
	}
	prompt()
}

function reverse(right,  left, t) {
	left = 1
	while (right > left) {
		t = list[left]
		list[left] = list[right]
		list[right] = t
		left++
		right--
	}
}

END { print "\n\nBye!" }
```



## BASIC

{{works with|QBasic}}

{{works with|FreeBASIC}}


```qbasic
PRINT "Given a jumbled list of the numbers 1 to 9,"
PRINT "you must select how many digits from the left to reverse."
PRINT "Your goal is to get the digits in order with 1 on the left and 9 on the right."

RANDOMIZE TIMER

DIM nums(1 TO 9) AS INTEGER
DIM L0 AS INTEGER, n AS INTEGER, flp AS INTEGER, tries AS INTEGER, again AS INTEGER

'initial values
FOR L0 = 1 TO 9
    nums(L0) = L0
NEXT

DO  'shuffle
    FOR L0 = 9 TO 2 STEP -1
        n = INT(RND * (L0)) + 1
        IF n <> L0 THEN SWAP nums(n), nums(L0)
    NEXT
    FOR L0 = 1 TO 8 'make sure it's not in order
        IF nums(L0) > nums(L0 + 1) THEN EXIT DO
    NEXT
LOOP

again = -1
DO
    IF tries < 10 THEN PRINT " ";
    PRINT tries; ":";
    FOR L0 = 1 TO 9
        PRINT nums(L0);
    NEXT

    IF NOT again THEN EXIT DO

    INPUT " -- How many numbers should be flipped"; flp
    IF flp < 0 THEN flp = 0
    IF flp > 9 THEN flp = 0

    FOR L0 = 1 TO (flp \ 2)
        SWAP nums(L0), nums(flp - L0 + 1)
    NEXT

    again = 0
    'check for order
    FOR L0 = 1 TO 8
        IF nums(L0) > nums(L0 + 1) THEN
            again = -1
            EXIT FOR
        END IF
    NEXT

    IF flp > 0 THEN tries = tries + 1
LOOP

PRINT : PRINT "You took "; LTRIM$(RTRIM$(STR$(tries))); " tries to put the digits in order."
```


Sample output:

```txt

 Given a jumbled list of the numbers 1 to 9,
 you must select how many digits from the left to reverse.
 Your goal is to get the digits in order with 1 on the left and 9 on the right.
   0 : 1  4  5  3  2  7  6  8  9  -- How many numbers should be flipped? 7
   1 : 6  7  2  3  5  4  1  8  9  -- How many numbers should be flipped? 2
   2 : 7  6  2  3  5  4  1  8  9  -- How many numbers should be flipped? 7
   3 : 1  4  5  3  2  6  7  8  9  -- How many numbers should be flipped? 3
   4 : 5  4  1  3  2  6  7  8  9  -- How many numbers should be flipped? 5
   5 : 2  3  1  4  5  6  7  8  9  -- How many numbers should be flipped? 2
   6 : 3  2  1  4  5  6  7  8  9  -- How many numbers should be flipped? 3
   7 : 1  2  3  4  5  6  7  8  9
 You took 7  tries to put the digits in order.

```



## BASIC256


```BASIC256

print "Dada una lista aleatoria de numeros del 1 al 9,"
print "indica cuantos digitos de la izquierda voltear."
print " El objetivo es obtener los digitos en orden "
print " con el 1 a la izquierda y el 9 a la derecha." + chr(10)

dim nums(10)
dim a(10)
intentos = 0: denuevo = true: colum = 6

#valores iniciales
for i = 1 to 9
	nums[i] = i
next i

do  #barajamos
	for i = 9 to 2 step -1
		n = int(rand * i) + 1
		if n <> i then
			a[i] = nums[i]
			nums[i] = nums[n]
			nums[n] = a[i]
		end if
	next i
	for i = 1 to 8 #nos aseguramos que no estén en orden
		if nums[i] > nums[i + 1] then exit do
	next i
until false

do
	if intentos < 10 then print " ";
	print intentos; ": ";
	for i = 1 to 9
		print nums[i];" ";
	next i

	if not denuevo then exit do

	input "  -- Cuantos volteamos ", volteo
	if volteo < 0 or volteo > 9 then volteo = 0

	for i = 1 to (volteo \ 2)
		a[i] = nums[volteo - i + 1]
		nums[volteo - i + 1] = nums[i]
		nums[i] = a[i]
	next i

	denuevo = false
	#comprobamos el orden
	for i = 1 to 8
		if nums[i] > nums[i + 1] then
			denuevo = true
			exit for
		end if
	next i

	if volteo > 0 then intentos += 1
until false
print chr(10) + chr(10) + "   Necesitaste "; intentos; " intentos."
end

```

{{out}}

```txt

Dada una lista aleatoria de numeros del 1 al 9,
indica cuantos digitos de la izquierda voltear.
 El objetivo es obtener los digitos en orden
 con el 1 a la izquierda y el 9 a la derecha.

 0: 2 1 4 3 5 9 8 6 7   -- Cuantos volteamos 6
 1: 9 5 3 4 1 2 8 6 7   -- Cuantos volteamos 9
 2: 7 6 8 2 1 4 3 5 9   -- Cuantos volteamos 3
 3: 8 6 7 2 1 4 3 5 9   -- Cuantos volteamos 8
 4: 5 3 4 1 2 7 6 8 9   -- Cuantos volteamos 6
 5: 7 2 1 4 3 5 6 8 9   -- Cuantos volteamos 7
 6: 6 5 3 4 1 2 7 8 9   -- Cuantos volteamos 6
 7: 2 1 4 3 5 6 7 8 9   -- Cuantos volteamos 3
 8: 4 1 2 3 5 6 7 8 9   -- Cuantos volteamos 4
 9: 3 2 1 4 5 6 7 8 9   -- Cuantos volteamos 3
10: 1 2 3 4 5 6 7 8 9

   Necesitaste 10 intentos.

```



## Batch File

Note that I did not use the FOR command for looping. I used Batch File labels instead.

```dos

::
::Number Reversal Game Task from Rosetta Code Wiki
::Batch File Implementation
::
::Please do not open this from command prompt.
::Directly Open the Batch File to play...
::

@echo off
setlocal enabledelayedexpansion
title Number Reversal Game

:begin
set score=0

::The ascending list of 9 digits
set list=123456789


::Generating a random set of 9 digits...
set cyc=9
:gen
set /a tmp1=%random%%%%cyc%
set n%cyc%=!list:~%tmp1%,1!
set tmp2=!n%cyc%!
set list=!list:%tmp2%=!
if not %cyc%==2 (
	set /a cyc-=1
	goto :gen
)
set /a n1=%list%

::Display the Game
cls
echo.
echo ***Number Reversal Game***
:loopgame
echo.
echo Current arrangement: %n1%%n2%%n3%%n4%%n5%%n6%%n7%%n8%%n9%
set /p move=How many digits from the left should I reverse?

::Reverse digits according to the player's input
::NOTE: The next command uses the fact that in Batch File,
::The output for the division operation is only the integer part of the quotient.
set /a lim=(%move%+1)/2

set cyc2=1
:reverse
set /a tmp4=%move%-%cyc2%+1
set tmp5=!n%cyc2%!
set n%cyc2%=!n%tmp4%!
set n%tmp4%=%tmp5%
if not %cyc2%==%lim% (
	set /a cyc2+=1
	goto :reverse
)

::Increment the number of moves took by the player
set /a score+=1

::IF already won...
if %n1%%n2%%n3%%n4%%n5%%n6%%n7%%n8%%n9%==123456789 (
	echo.
	echo Set: %n1%%n2%%n3%%n4%%n5%%n6%%n7%%n8%%n9% DONE^^!
	echo You took %score% moves to arrange the numbers in ascending order.
	pause>nul
	exit
) else (
goto :loopgame
)

```

Sample Output:

```txt

***Number Reversal Game***

Current arrangement: 349718652
How many digits from the left should I reverse?3

Current arrangement: 943718652
How many digits from the left should I reverse?9

Current arrangement: 256817349
How many digits from the left should I reverse?4

Current arrangement: 865217349
How many digits from the left should I reverse?8

Current arrangement: 437125689
How many digits from the left should I reverse?3

Current arrangement: 734125689
How many digits from the left should I reverse?7

Current arrangement: 652143789
How many digits from the left should I reverse?6

Current arrangement: 341256789
How many digits from the left should I reverse?2

Current arrangement: 431256789
How many digits from the left should I reverse?4

Current arrangement: 213456789
How many digits from the left should I reverse?2

Set: 123456789 DONE!
You took 10 moves to arrange the numbers in ascending order.

```



## BBC BASIC

Note the use of the MOD(array()) function to test the equality of two arrays.

```bbcbasic
      DIM list%(8), done%(8), test%(8)
      list%() = 1, 2, 3, 4, 5, 6, 7, 8, 9
      done%() = list%()

      REM Shuffle:
      REPEAT
        FOR i% = 9 TO 2 STEP -1
          SWAP list%(i%-1), list%(RND(i%)-1)
        NEXT
        test%() = list%() - done%()
      UNTIL MOD(test%()) <> 0

      REM Run the game:
      tries% = 0
      REPEAT
        tries% += 1
        FOR i% = 0 TO 8 : PRINT ; list%(i%) " " ; : NEXT
        INPUT ": Reverse how many", n%
        PROCreverse(list%(), n%)
        test%() = list%() - done%()
      UNTIL MOD(test%()) = 0
      PRINT "You took " ; tries% " attempts."
      END

      DEF PROCreverse(a%(), n%)
      IF n% < 2 ENDPROC
      LOCAL i%
      n% -= 1
      FOR i% = 0 TO n% DIV 2
        SWAP a%(i%), a%(n%-i%)
      NEXT
      ENDPROC
```

'''Output:'''

```txt

7 2 3 1 5 8 6 4 9 : Reverse how many? 6
8 5 1 3 2 7 6 4 9 : Reverse how many? 8
4 6 7 2 3 1 5 8 9 : Reverse how many? 3
7 6 4 2 3 1 5 8 9 : Reverse how many? 7
5 1 3 2 4 6 7 8 9 : Reverse how many? 5
4 2 3 1 5 6 7 8 9 : Reverse how many? 4
1 3 2 4 5 6 7 8 9 : Reverse how many? 3
2 3 1 4 5 6 7 8 9 : Reverse how many? 2
3 2 1 4 5 6 7 8 9 : Reverse how many? 3
You took 9 attempts.

```



## Brat


```brat
sorted = 1.to 9
length = sorted.length
numbers = sorted.shuffle

#Make certain numbers are shuffled
true? numbers == sorted
  { while { sorted == numbers.shuffle! } }

turns = 0

while { numbers != sorted } {
  print "#{numbers} - How many to reverse? "
  num  = g.to_i
  numbers = numbers[0, num - 1].reverse + numbers[num, length]
  turns = turns + 1
}

p numbers
p "It took #{turns} turns to sort numbers."
```



## C

An example of a number reversal game could be:

```c
void number_reversal_game()
{
    printf("Number Reversal Game. Type a number to flip the first n numbers.");
    printf("Win by sorting the numbers in ascending order.\n");
    printf("Anything besides numbers are ignored.\n");
    printf("\t  |1__2__3__4__5__6__7__8__9|\n");
    int list[9] = {1,2,3,4,5,6,7,8,9};
    shuffle_list(list,9);

    int tries=0;
    unsigned int i;
    int input;

    while(!check_array(list, 9))
    {
        ((tries<10) ? printf("Round %d :  ", tries) : printf("Round %d : ", tries));
        for(i=0;i<9;i++)printf("%d  ",list[i]);
        printf("  Gimme that number:");
        while(1)
        {
            //Just keep asking for proper input
            scanf("%d", &input);
            if(input>1&&input<10)
                break;

            printf("\n%d - Please enter a number between 2 and 9:", (int)input);
        }
        tries++;
        do_flip(list, 9, input);
    }
    printf("Hurray! You solved it in %d moves!\n", tries);
}
```


Which uses the following helper functions:

```c
void shuffle_list(int *list, int len)
{
    //We'll just be swapping 100 times. Could be more/less. Doesn't matter much.
    int n=100;
    int a=0;
    int b=0;
    int buf = 0;
    //Random enough for common day use..
    srand(time(NULL));
    while(n--)
    {
        //Get random locations and swap
        a = rand()%len;
        b = rand()%len;
        buf = list[a];
        list[a] = list[b];
        list[b] = buf;
    }
    // "shuffled to ordered state" fix:
    if (check_array(list, len)) shuffle_list (list, len);
}

void do_flip(int *list, int length, int num)
{
    //Flip a part on an array
    int swap;
    int i=0;
    for(i;i<--num;i++)
    {
        swap=list[i];
        list[i]=list[num];
        list[num]=swap;
    }
}

int check_array(int *arr, int len)
{
    while(--len)
    {
        if(arr[len]!=(arr[len-1]+1))
            return 0;
    }
    return 1;
}
```


## C#
C# 3.0

```c#
using System;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        var r = new Random();

        var tries = 1;
        var sorted = Enumerable.Range(1, 9).ToList();
        var values = sorted.OrderBy(x => r.Next(-1, 1)).ToList();

        while (Enumerable.SequenceEqual(sorted, values)) {
            values = sorted.OrderBy(x => r.Next(-1, 1)).ToList();
        }

        //values = "1 3 9 2 7 5 4 8 6".Split().Select(x => int.Parse(x)).ToList();

        while (!Enumerable.SequenceEqual(sorted, values))
        {
            Console.Write("# {0}: LIST: {1} - Flip how many? ", tries, String.Join(" ", values));

            values.Reverse(0, int.Parse(Console.ReadLine()));
            tries += 1;
        }

        Console.WriteLine("\nYou took {0} attempts to put the digits in order!", tries - 1);
        Console.ReadLine();
    }
}
```


C# 1.0

```c#
class Program
{
	static void Main(string[] args)
	{
		int[] values = new int[9];
		Random tRandom = new Random();
		int tries = 0;

		for (int x = 0; x < values.Length; x++)
		{
			values[x] = x + 1;
		}

		values = RandomPermutation<int>(values);

		do
		{
			Console.Write("Numbers: ");
			for (int x = 0; x < values.Length; x++)
			{
				Console.Write(" ");
				Console.Write(values[x]);
			}
			Console.WriteLine(". Enter number of numbers from the left to reverse: ");

			string tIn = "";
			do
			{
				tIn = Console.ReadLine();
			} while (tIn.Length != 1);

			int nums = Convert.ToInt32(tIn.ToString());

			int[] newValues = new int[9];
			for (int x = nums; x < newValues.Length; x++)
			{
				// Move those not reversing
				newValues[x] = values[x];
			}
			for (int x = 0; x < nums; x++)
			{
				// Reverse the rest
				newValues[x] = values[nums - 1 - x];
			}
			values = newValues;
			tries++;
		} while (!check(values));

		Console.WriteLine("Success!");
		Console.WriteLine("Attempts: " + tries);

		Console.Read();
	}

	public static bool check(int[] p)
	{
		// Check all items
		for (int x = 0; x < p.Length - 1; x++)
		{
			if (p[x + 1] <= p[x])
				return false;
		}

		return true;
	}

	public static T[] RandomPermutation<T>(T[] array)
	{
		T[] retArray = new T[array.Length];
		array.CopyTo(retArray, 0);

		Random random = new Random();
		for (int i = 0; i < array.Length; i += 1)
		{
			int swapIndex = random.Next(i, array.Length);
			if (swapIndex != i)
			{
				T temp = retArray[i];
				retArray[i] = retArray[swapIndex];
				retArray[swapIndex] = temp;
			}
		}

		return retArray;
	}
}
```



## C++

The C code can be used with C++, although the following uses proper C++ iostreams:

```CPP
void number_reversal_game()
{
	cout << "Number Reversal Game. Type a number to flip the first n numbers.";
	cout << "You win by sorting the numbers in ascending order.";
	cout << "Anything besides numbers are ignored.\n";
	cout << "\t  |1__2__3__4__5__6__7__8__9|\n";
	int list[9] = {1,2,3,4,5,6,7,8,9};
        do
        {
	  shuffle_list(list,9);
        } while(check_array(list, 9));

	int tries=0;
	unsigned int i;
	int input;

	while(!check_array(list, 9))
	{
		cout << "Round " << tries << ((tries<10) ? " :  " : " : ");
		for(i=0;i<9;i++)cout << list[i] << "  ";
		cout << "  Gimme that number:";
		while(1)
		{
			cin >> input;
			if(input>1&&input<10)
				break;

			cout << "\nPlease enter a number between 2 and 9:";
		}
		tries++;
		do_flip(list, 9, input);
	}
	cout << "Hurray! You solved it in %d moves!\n";
}
```


This uses the same helper functions as the C version.


###  Alternate version using the C++ standard library

This version uses the C++ standard library (note that none of the C helper functions are needed).

```cpp

#include <iostream>
#include <algorithm>
#include <functional>
#include <iterator>
#include <cstdlib>
#include <ctime>

template<typename T, int size>
 bool is_sorted(T (&array)[size])
{
  return std::adjacent_find(array, array+size, std::greater<T>())
    == array+size;
}

int main()
{
  std::srand(std::time(0));

  int list[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  do
  {
    std::random_shuffle(list, list+9);
  } while (is_sorted(list));

  int score = 0;

  do
  {
    std::cout << "Current list: ";
    std::copy(list, list+9, std::ostream_iterator<int>(std::cout, " "));

    int rev;
    while (true)
    {
      std::cout << "\nDigits to reverse? ";
      std::cin >> rev;
      if (rev > 1 && rev < 10)
        break;
      std::cout << "Please enter a value between 2 and 9.";
    }

    ++score;
    std::reverse(list, list + rev);
  } while (!is_sorted(list));

  std::cout << "Congratulations, you sorted the list.\n"
            << "You needed " << score << " reversals." << std::endl;
  return 0;
}

```



## Clojure


```clojure
(defn flip-at [n coll]
  (let [[x y] (split-at n coll)]
    (concat (reverse x) y )))

(def sorted '(1 2 3 4 5 6 7 8 9))  ; i.e. (range 1 10)
(def unsorted? #(not= % sorted))

(loop [unsorted (first (filter unsorted? (iterate shuffle sorted))), steps 0]
  (if (= unsorted sorted)
    (printf "Done! That took you %d steps%n" steps)
    (do
      (println unsorted)
      (printf "Reverse how many? ")
      (flush)
      (let [flipcount (read)]
        (recur (flip-at flipcount unsorted), (inc steps))))))
```



## Common Lisp


```lisp
(defun shuffle! (vector)
  (loop for i from (1- (length vector)) downto 1
       do (rotatef (aref vector i)
                   (aref vector (random i)))))

(defun slice (vector start &optional end)
  (let ((end (or end (length vector))))
    (make-array (- end start)
                :element-type (array-element-type vector)
                :displaced-to vector
                :displaced-index-offset start)))

(defun orderedp (seq)
  (apply #'<= (coerce seq 'list)))

(defun prompt-integer (prompt)
  (format t "~A: " prompt)
  (finish-output)
  (clear-input)
  (parse-integer (read-line)))

(defun game ()
  (let ((numbers (vector 1 2 3 4 5 6 7 8 9)))
    (shuffle! numbers)
    (let ((score
           (do ((score 0 (1+ score)))
               ((orderedp numbers) score)
             (format t "~A~%" numbers)
             (let* ((n (prompt-integer "How many numbers to reverse"))
                    (slice (slice numbers 0 n)))
               (replace slice (nreverse slice))))))
      (format t "~A~%Congratulations, you did it in ~D reversals!~%" numbers score))))

```



## Crystal


```Ruby

SIZE = 9
ordered = (1..SIZE).to_a
shuffled = (1..SIZE).to_a

while shuffled == ordered
  shuffled.shuffle!
end

score = 0
until shuffled == ordered
  print "#{shuffled} Enter items to reverse: "

  next unless guess = gets
  next unless num = guess.to_i?
  next if num < 2 || num > SIZE

  shuffled[0, num] = shuffled[0, num].reverse
  score += 1
end

puts "#{shuffled} Your score: #{score}"

```



## D


```d
import std.stdio, std.random, std.string, std.conv, std.algorithm,
       std.range;

void main() {
    auto data = iota(1, 10).array;
    do data.randomShuffle;
    while (data.isSorted);

    int trial;
    while (!data.isSorted) {
        writef("%d: %s How many numbers to flip? ", ++trial, data);
        data[0 .. readln.strip.to!uint].reverse;
    }
    writefln("\nYou took %d attempts.", trial);
}
```

{{out}}

```txt
1: [7, 2, 1, 6, 3, 8, 9, 5, 4] How many numbers to flip? 7
2: [9, 8, 3, 6, 1, 2, 7, 5, 4] How many numbers to flip? 9
3: [4, 5, 7, 2, 1, 6, 3, 8, 9] How many numbers to flip? 3
4: [7, 5, 4, 2, 1, 6, 3, 8, 9] How many numbers to flip? 7
5: [3, 6, 1, 2, 4, 5, 7, 8, 9] How many numbers to flip? 2
6: [6, 3, 1, 2, 4, 5, 7, 8, 9] How many numbers to flip? 6
7: [5, 4, 2, 1, 3, 6, 7, 8, 9] How many numbers to flip? 5
8: [3, 1, 2, 4, 5, 6, 7, 8, 9] How many numbers to flip? 3
9: [2, 1, 3, 4, 5, 6, 7, 8, 9] How many numbers to flip? 2

You took 9 attempts.
```



## Egel


```Egel

import "prelude.eg"
import "io.ego"
import "random.ego"

using System
using IO
using List
using Math

def swap =
    [ (I J) XX -> insert I (nth J XX) (insert J (nth I XX) XX) ]

def shuffle =
    [ XX ->
        let INDICES = reverse (fromto 0 ((length XX) - 1)) in
        let SWAPS = map [ I -> I (between 0 I) ] INDICES in
            foldr [I J -> swap I J] XX SWAPS ]

def prompt =
    [ XX TURN ->
        let _ = print TURN ". " in
        let _ = map [ X -> print X " " ] XX in
        let _ = print " : " in
            toint getline ]

def game =
    [ GOAL SHUFFLE TURN ->
        if SHUFFLE == GOAL then
            let _ = print "the goal was " in
            let _ = map [ X -> print X " " ] GOAL in
                print "\nit took you " TURN " turns\n"
        else
            let N = prompt SHUFFLE TURN in
            let YY = (reverse (take N SHUFFLE)) ++ (drop N SHUFFLE) in
                game GOAL YY (TURN + 1) ]

def main =
    let XX = fromto 1 9 in game XX (shuffle XX) 0
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE}

	make
			-- Plays Number Reversal Game.
		local
			count: INTEGER
		do
			initialize_game
			io.put_string ("Let's play the number reversal game.%N")
			across
				numbers as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
			from
			until
				is_sorted (numbers, 1, numbers.count)
			loop
				io.put_string ("%NHow many numbers should be reversed?%N")
				io.read_integer
				reverse_array (io.last_integer)
				across
					numbers as ar
				loop
					io.put_string (ar.item.out + "%T")
				end
				count := count + 1
			end
			io.put_string ("%NYou needed " + count.out + " reversals.")
		end

feature {NONE}

	initialize_game
			-- Array with numbers from 1 to 9 in a random unsorted order.
		local
			random: V_RANDOM
			item, i: INTEGER
		do
			create random
			create numbers.make_empty
			from
				i := 1
			until
				numbers.count = 9 and not is_sorted (numbers, 1, numbers.count)
			loop
				item := random.bounded_item (1, 9)
				if not numbers.has (item) then
					numbers.force (item, i)
					i := i + 1
				end
				random.forth
			end
		end

	numbers: ARRAY [INTEGER]

	reverse_array (upper: INTEGER)
			-- Array numbers with first element up to nth element reversed.
		require
			upper_positive: upper > 0
			ar_not_void: numbers /= Void
		local
			i, j: INTEGER
			new_array: ARRAY [INTEGER]
		do
			create new_array.make_empty
			new_array.deep_copy (numbers)
			from
				i := 1
				j := upper
			until
				i > j
			loop
				new_array [i] := numbers [j]
				new_array [j] := numbers [i]
				i := i + 1
				j := j - 1
			end
			numbers := new_array
		end

	is_sorted (ar: ARRAY [INTEGER]; l, r: INTEGER): BOOLEAN
			-- Is Array 'ar' sorted in ascending order?
		require
			ar_not_empty: not ar.is_empty
		do
			Result := True
			across
				1 |..| (r - 1) as c
			loop
				if ar [c.item] > ar [c.item + 1] then
					Result := False
				end
			end
		end

end

```


{{out}}

```txt

Let's play the number reversal game.
7 8 5 6 9 6 1 4 2
How many numbers should be reversed?
5
9 3 5 8 7 6 1 4 2
How many numbers should be reversed?
9
2 4 1 6 7 8 5 3 9
How many numbers should be reversed?
6
8 7 6 1 4 2 5 3 9
How many numbers should be reversed?
8
3 5 2 4 1 6 7 8 9
How many numbers should be reversed?
2
5 3 2 4 1 6 7 8 9
How many numbers should be reversed?
5
1 4 2 3 5 6 7 8 9
How many numbers should be reversed?
2
4 1 2 3 5 6 7 8 9
How many numbers should be reversed?
4
3 2 1 4 5 6 7 8 9
How many numbers should be reversed?
3
1 2 3 4 5 6 7 8 9
You needed 9 reversals.

```



## Elena

ELENA 4.x:

```elena
import system'routines;
import extensions;

public program()
{
    var sorted := Array.allocate(9).populate:(n => n + 1 );
    var values := sorted.clone().randomize:9;

    while (sorted.sequenceEqual:values)
    {
        values := sorted.randomize:9
    };

    var tries := new Integer();
    until (sorted.sequenceEqual:values)
    {
        tries.append:1;

        console.print("# ",tries," : LIST : ",values," - Flip how many?");

        values.sequenceReverse(0, console.readLine().toInt())
    };

    console.printLine("You took ",tries," attempts to put the digits in order!").readChar()
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Number_reversal_game do
  def start( n ) when n > 1 do
    IO.puts "Usage: #{usage(n)}"
    targets = Enum.to_list( 1..n )
    jumbleds = Enum.shuffle(targets)
    attempt = loop( targets, jumbleds, 0 )
    IO.puts "Numbers sorted in #{attempt} atttempts"
  end

  defp loop( targets, targets, attempt ), do: attempt
  defp loop( targets, jumbleds, attempt ) do
    IO.inspect jumbleds
    {n,_} = IO.gets("How many digits from the left to reverse? ") |> Integer.parse
    loop( targets, Enum.reverse_slice(jumbleds, 0, n), attempt+1 )
  end

  defp usage(n), do: "Given a jumbled list of the numbers 1 to #{n} that are definitely not in ascending order, show the list then ask the player how many digits from the left to reverse. Reverse those digits, then ask again, until all the digits end up in ascending order."
end

Number_reversal_game.start( 9 )
```


{{out}}

```txt

Usage: Given a jumbled list of the numbers 1 to 9 that are definitely not in ascending order, show the list then ask the player how many digits from the left to reverse. Reverse those digits, then ask again, until all he digits end up in ascending order.
[3, 9, 4, 7, 6, 5, 1, 2, 8]
How many digits from the left to reverse? 2
[9, 3, 4, 7, 6, 5, 1, 2, 8]
How many digits from the left to reverse? 9
[8, 2, 1, 5, 6, 7, 4, 3, 9]
How many digits from the left to reverse? 8
[3, 4, 7, 6, 5, 1, 2, 8, 9]
How many digits from the left to reverse? 3
[7, 4, 3, 6, 5, 1, 2, 8, 9]
How many digits from the left to reverse? 7
[2, 1, 5, 6, 3, 4, 7, 8, 9]
How many digits from the left to reverse? 4
[6, 5, 1, 2, 3, 4, 7, 8, 9]
How many digits from the left to reverse? 6
[4, 3, 2, 1, 5, 6, 7, 8, 9]
How many digits from the left to reverse? 4
Numbers sorted in 8 atttempts

```



## Erlang


```Erlang

-module( number_reversal_game ).

-export( [task/0, start/1] ).

start( N ) when N > 1 ->
	io:fwrite( "Usage: ~s~n", [usage(N)] ),
	Targets = lists:seq( 1, N ),
	Jumbleds = [X||{_,X} <- lists:sort([ {random:uniform(), Y} || Y <- Targets])],
	Attempt = loop( Targets, Jumbleds, 0 ),
	io:fwrite( "Numbers sorted in ~p atttempts~n", [Attempt] ).

task() -> start( 9 ).



loop( Targets, Targets, Attempt ) -> Attempt;
loop( Targets, Jumbleds, Attempt ) ->
	io:fwrite( "~p~n", [Jumbleds] ),
	{ok,[N]} = io:fread( "How many digits from the left to reverse? ", "~d" ),
	{Lefts, Rights} = lists:split( N, Jumbleds ),
	loop( Targets, lists:reverse(Lefts) ++ Rights, Attempt + 1 ).

usage(N) -> io_lib:format( "Given a jumbled list of the numbers 1 to ~p that are definitely not in ascending order, show the list then ask the player how many digits from the left to reverse. Reverse those digits, then ask again, until all the digits end up in ascending order.", [N] ).

```

{{out}}
Not being a very good player I show a test run with only 3 numbers.

```txt

35> number_reversal_game:start(3).
Usage: Given a jumbled list of the numbers 1 to 3 that are definitely not in ascending order, show
the list then ask the player how many digits from the left to reverse. Reverse those digits, then
ask again, until all the digits end up in ascending order.
[2,3,1]
How many digits from the left to reverse? 3
[1,3,2]
How many digits from the left to reverse? 3
[2,3,1]
How many digits from the left to reverse? 2
[3,2,1]
How many digits from the left to reverse? 3
Numbers sorted in 4 atttempts

```



## Euphoria


```euphoria
include get.e

function accending(sequence s)
    for i = 1 to length(s)-1 do
        if s[i]>s[i+1] then
            return 0
        end if
    end for
    return 1
end function

puts(1,"Given a jumbled list of the numbers 1 to 9,\n")
puts(1,"you must select how many digits from the left to reverse.\n")
puts(1,"Your goal is to get the digits in order with 1 on the left and 9 on the right.\n")

sequence nums
nums = repeat(0,9)
integer n,flp,tries,temp

-- initial values
for i = 1 to 9 do
    nums[i] = i
end for

while accending(nums) do -- shuffle
    for i = 1 to 9 do
        n = rand(9)
        temp = nums[n]
        nums[n] = nums[i]
        nums[i] = temp
    end for
end while

tries = 0
while 1 do
    printf(1,"%2d : ",tries)
    for i = 1 to 9 do
        printf(1,"%d ",nums[i])
    end for

    if accending(nums) then
        exit
    end if

    flp = prompt_number(" -- How many numbers should be flipped? ",{1,9})
    for i = 1 to flp/2 do
        temp = nums[i]
        nums[i] = nums[flp-i+1]
        nums[flp-i+1] = temp
    end for

    tries += 1
end while

printf(1,"\nYou took %d tries to put the digits in order.", tries)
```


Output:

```txt
Given a jumbled list of the numbers 1 to 9,
you must select how many digits from the left to reverse.
Your goal is to get the digits in order with 1 on the left and 9 on the right.
 0 : 4 8 7 5 1 6 3 9 2  -- How many numbers should be flipped? 8
 1 : 9 3 6 1 5 7 8 4 2  -- How many numbers should be flipped? 9
 2 : 2 4 8 7 5 1 6 3 9  -- How many numbers should be flipped? 3
 3 : 8 4 2 7 5 1 6 3 9  -- How many numbers should be flipped? 8
 4 : 3 6 1 5 7 2 4 8 9  -- How many numbers should be flipped? 5
 5 : 7 5 1 6 3 2 4 8 9  -- How many numbers should be flipped? 7
 6 : 4 2 3 6 1 5 7 8 9  -- How many numbers should be flipped? 4
 7 : 6 3 2 4 1 5 7 8 9  -- How many numbers should be flipped? 6
 8 : 5 1 4 2 3 6 7 8 9  -- How many numbers should be flipped? 5
 9 : 3 2 4 1 5 6 7 8 9  -- How many numbers should be flipped? 3
10 : 4 2 3 1 5 6 7 8 9  -- How many numbers should be flipped? 4
11 : 1 3 2 4 5 6 7 8 9  -- How many numbers should be flipped? 2
12 : 3 1 2 4 5 6 7 8 9  -- How many numbers should be flipped? 3
13 : 2 1 3 4 5 6 7 8 9  -- How many numbers should be flipped? 2
14 : 1 2 3 4 5 6 7 8 9
You took 14 tries to put the digits in order.
```


=={{header|F Sharp|F#}}==

```fsharp
let rand = System.Random()

while true do
  let rec randomNums() =
    let xs = [|for i in 1..9 -> rand.Next(), i|] |> Array.sort |> Array.map snd
    if xs = Array.sort xs then randomNums() else xs

  let xs = randomNums()

  let suffix = function | 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"

  let rec move i =
    printf "\n%A\n\nReverse how many digits from the left in your %i%s move? : " xs i (suffix i)
    let n = stdin.ReadLine() |> int
    Array.blit (Array.rev xs.[0..n-1]) 0 xs 0 n
    if xs <> Array.sort xs then
        move (i+1)
    else
        printfn "\nYou took %i moves to put the digits in order!\n" i

  move 1
```



## Factor


```factor
USING: formatting io kernel math math.parser math.ranges
namespaces random sequences strings ;
IN: rosetta.number-reversal

: make-jumbled-array ( -- sorted jumbled )
    CHAR: 1 CHAR: 9 [a,b] [ 1string ] map dup clone randomize
    [ 2dup = ] [ randomize ] while ;

SYMBOL: trials

: prompt ( jumbled -- n )
    trials get "#%2d: " printf
    ", " join write
    "   Flip how many? " write flush
    readln string>number ;

: game-loop ( sorted jumbled -- )
    2dup = [
        2drop trials get
        "\nYou took %d attempts to put the digits in order!\n" printf
        flush
    ] [
        trials [ 1 + ] change
        dup dup prompt head-slice reverse! drop
        game-loop
    ] if ;

: play ( -- )
    0 trials set
    make-jumbled-array game-loop ;
```



## Forth


```forth
include random.fs

variable flips
create nums 9 cells allot

: .array ( addr len -- ) 0 ?do dup @ . cell+ loop drop ;

: shuffle ( addr len -- )
  2 swap do
    dup i random cells +
    over @ over @  swap
    rot  ! over !
    cell+
  -1 +loop drop ;

: sorted? ( addr len -- )
  1- cells bounds ?do
    i 2@ < if unloop false exit then
  cell +loop true ;

: init
  0 flips !
  nums 10 1 do
    i over !  cell+
  loop drop
  begin nums 9 shuffle nums 9 sorted? 0= until
  cr nums 9 .array ;

: reverse ( addr len -- )
  1- cells bounds
  begin 2dup >
  while over @ over @ >r over ! over r> swap !
        cell+ swap 1 cells - swap
  repeat 2drop ;

: flip ( n -- )
  dup 2 10 within 0= if . ." must be within 2 to 9" exit then
  nums swap reverse
  1 flips +!
  nums 9 sorted? if
    cr ." Got it in " flips @ . ." tries!"
  else
    cr nums 9 .array
  then ;

init
1 2 8 5 7 6 9 4 3  ok
7 flip
9 6 7 5 8 2 1 4 3  ok

```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program Reversal_game
  implicit none

  integer :: list(9) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)
  integer :: pos, attempts = 0

  call random_seed
  do while (sorted(list))
    call Shuffle(list)
  end do
  write(*, "(9i5)") list
  write(*,*)
  do while (.not. Sorted(list))
    write(*, "(a)", advance="no") "How many numbers from the left do you want to reverse? : "
    read*, pos
    attempts = attempts + 1
    list(1:pos) = list(pos:1:-1)
    write(*, "(9i5)") list
    write(*,*)
  end do
  write(*,*)
  write(*, "(a,i0,a)") "Congratulations! Solved in ", attempts, " attempts"

contains

subroutine Shuffle(a)
  integer, intent(inout) :: a(:)
  integer :: i, randpos, temp
  real :: r

  do i = size(a), 2, -1
    call random_number(r)
    randpos = int(r * i) + 1
    temp = a(randpos)
    a(randpos) = a(i)
    a(i) = temp
  end do
end subroutine

function Sorted(a)
  logical :: Sorted
  integer, intent(in) :: a(:)
  integer :: i

  do i = 1, size(a)-1
    if(list(i+1) /= list(i)+1) then
      Sorted = .false.
      return
    end if
  end do
  Sorted = .true.
end function

end program
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "sort"
    "time"
)

func main() {
    rand.Seed(time.Now().UnixNano())
    var k []int
    for {
        k = rand.Perm(9)
        for i, r := range k {
            if r == 0 {
                k[i] = 9
            }
        }
        if !sort.IntsAreSorted(k) {
            break
        }
    }
    fmt.Println("Sort digits by reversing a number of digits on the left.")
    var n, score int
    for {
        fmt.Print("Digits: ", k, ". How many to reverse? ")
        i, _ := fmt.Scanln(&n)
        score++
        if i == 0 || n < 2 || n > 9 {
            fmt.Println("\n(Enter a number from 2 to 9)")
            continue
        }
        for l, r := 0, n-1; l < r; l, r = l+1, r-1 {
            k[l], k[r] = k[r], k[l]
        }
        if sort.IntsAreSorted(k) {
            fmt.Print("Digits: ", k, ".\n")
            fmt.Print("Your score: ", score, ".  Good job.\n")
            return
        }
    }
}
```



## Groovy


```groovy
sorted = [*(1..9)]
arr    = sorted.clone()

void flipstart(n) { arr[0..<n] = arr[0..<n].reverse() }

int steps = 0
while (arr==sorted) Collections.shuffle(arr)
while (arr!=sorted) {
    println arr.join(' ')
    print 'Reverse how many? '
    def flipcount = System.in.readLine()
    flipstart( flipcount.toInteger() )
    steps += 1
}
println "Done! That took you ${steps} steps"
```



## Haskell

Using Rosetta [[Knuth shuffle#Haskell|Knuth Shuffle]]

```haskell
import Data.List
import Control.Arrow
import Rosetta.Knuthshuffle

numberRevGame = do
  let goal = [1..9]

      shuffle xs =
	if xs /= goal then return xs
		    else shuffle =<< knuthShuffle xs

      prefixFlipAt k = uncurry (++). first reverse. splitAt k

      prompt r ry = do
	 putStr $ show r ++ ". " ++ concatMap (flip (++) " ". show) ry
		   ++ " How many to flip? "
	 answ <- getLine
	 let n = read answ
	 if n<10 && 0<n then return n
	    else do
	      putStrLn "Error. The number should be between 0 and 10. Try again"
	      prompt r ry

      playNRG r nrs =
	if nrs == goal then do
	  putStrLn $ "The answer is: " ++ concatMap (flip (++) " ". show) nrs
	  putStrLn $ "It took you " ++ show r ++ " attempts to sort the numbers."
	  putStrLn ""
		else do
		  answ <- prompt r nrs
		  playNRG (succ r) (prefixFlipAt answ nrs)

  start <- shuffle goal

  playNRG 1 start
```

Play:

```txt
*Main> numberRevGame
1. 3 5 1 4 7 6 8 2 9  How many to flip? 7
2. 8 6 7 4 1 5 3 2 9  How many to flip? 8
3. 2 3 5 1 4 7 6 8 9  How many to flip? 3
4. 5 3 2 1 4 7 6 8 9  How many to flip? 6
5. 7 4 1 2 3 5 6 8 9  How many to flip? 7
6. 6 5 3 2 1 4 7 8 9  How many to flip? 6
7. 4 1 2 3 5 6 7 8 9  How many to flip? 4
8. 3 2 1 4 5 6 7 8 9  How many to flip? 3
The answer is: 1 2 3 4 5 6 7 8 9
It took you 9 attempts to sort the numbers.
```



## HicEst


```HicEst
   WRITE(Messagebox) "You took ", Reversals(), " attempts"

FUNCTION Reversals()
  DIMENSION digits(9), temp(9)

  digits = 0
  DO i = 1, 9 ! create the shuffled digits
 1   x = CEILING( RAN(9) )
     IF( INDEX(digits, x) ) GOTO 1 ! HicEst has no WHILE
     digits(i) = x
  ENDDO

  DO Reversals = 1, 1E6 ! HicEst needs an upper bound
     DLG(NameEdit=Flips, DNum, MIn=0, MAx=9, Text=digits)
     DO j = 1, Flips/2
        swap = digits(j)
        digits(j) = digits(Flips+1-j)
        digits(Flips+1-j) = swap
     ENDDO
     temp = digits($+1) > digits($) ! $ = left side index
     IF( SUM(temp) == 8 ) RETURN
  ENDDO
END
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(cq)                                             # Number Reversal Game
local x,nums,R,flips

$define PROTECT ["WIN","ASK"]
$define MAGIC   ["xyzzy","abracadabra","hocus","pocus","presto","changeo","open","sesame","irs"]

put(cq,"game")                                                 # start command queue - use command line
rule := string(&digits--'0')                                   # ruler and move commands
every put(protected := [], map(!PROTECT))                      # protected commands
every put(magic := [], !MAGIC)

while x := get(cq) | "MOVE" do {                               # command from queue or ask for move
   case x of {
      "help" | "h" | "?" :                                     # --- start of user facing commands ---
         write("Input a position.  The list will be flipped left to right at that point.\n",
	       "You win when the list is sorted.\n",
	       "Commands:\n",
               "   help, h, ? - shows this\n",
               "   new, g     - new game\n",
               "   ruler, r   - shows a ruler\n",
               "   show, s    - shows the list\n",
               "   <n>        - flips the list at ruler position n\n",
               "   quit, q    - quit\n",
               "and various magic words.\n"
               ) & put(cq,"rule")
      "game" | "g" | "new"  : {
         put(cq,"help")
         flips := 0
         nums := rule
         until nums ~== rule do
         every !nums :=: ?nums                                 # shuffle
         }
      "rule" | "ruler" | "r" :
         put(cq,"show") & every writes(" " || " " | !(if /mirror then rule else reverse(rule)) | "\n")
      "show" | "s" :
         every writes(" " || "=" | !nums | " =\n")
      !rule : {                                                #  0 - 9 for flipping
         if /mirror then nums[1+:x] := reverse(nums[1+:x])
         else nums[0-:x] := reverse(nums[0-:x])
         flips +:= 1
         put(cq,if nums == rule then "WIN" else "show")
         }
      "quit" | "q" :
         break write("Goodbye.")
      !magic:                                                  # --- start of magic
         write("That has no power here.  Try again!")
      "magic" | "mirror" | "m" : {
         mirror := if /mirror then 1 else &null
         write("Wait! What is this? The writing has reversed.")
         }
      !protected:                                              # --- Start of internal (upper case) and protected commands
         put(cq,?rule) & write("Tisk, Tisk, don't try and cheat.  Take a random penalty flip!")
      "MOVE" :
         put(cq,ask("Command? : ") )
      "WIN" :
         put(cq,"ASK") & write("Congratulations you won in ",flips," flips!")
      "ASK" :
         put(cq,case ask("Play another game? : ") of { "y"|"yes" : "game"; "n"|"no" : "quit"; default : "ASK" } )
      default:                                                 # --- say what?
         write("Sorry I don't know that command, try help?")
      }
   }
end

procedure ask(s)                       #: ask for input with prompt s and return the 1st word in lower case
writes(\s)
map(trim(read())) ? return tab(upto(' ')|0)
end

```


Sample output:
```txt
Input a position.  The list will be flipped left to right at that point.
You win when the list is sorted.
Commands:
   help, h, ? - shows this
   new, g     - new game
   ruler, r   - shows a ruler
   show, s    - shows the list
   <n>        - flips the list at ruler position n
   quit, q    - quit
and various magic words.

  123456789
 =745361289 =
Command? : 7
 =216354789 =
Command? : 3
 =612354789 =
Command? : 6
 =453216789 =
Command? : 2
 =543216789 =
Command? : 5
Congratulations you won in 5 flips!
Play another game? :
```



## Inform 7


```inform7
Number Reversal Game is a room.

The current list is a list of numbers that varies.

When play begins:
	now the current list is a shuffled list from 1 to 9;
	now the command prompt is "Current list: [current list in brace notation].[line break]How many items to flip? ".

Definition: a list of numbers (called L) is sorted:
	repeat with N running from 1 to the number of entries in L:
		if entry N in L is not N, no;
	yes.

To decide which list of numbers is a shuffled list from (min - number) to (max - number):
	let result be a list of numbers;
	repeat with N running from min to max:
		add N to result;
	while true is true:
		sort result in random order;
		if result is not sorted, decide on result.

Flipping is an action applying to one number. Understand "[number]" as flipping.

Carry out flipping:
	let N be the number understood;
	let L be the current list;
	truncate the current list to the first N entries;
	reverse the current list;
	truncate L to the last (number of entries in L minus N) entries;
	add L to the current list.

Report flipping: say "".

Every turn:
	if the current list is sorted, end the story saying "You have won".

This is the new print final score rule:
	say "It took you [turn count] flip[s] to sort the list."

The new print final score rule is listed instead of the print final score rule in the for printing the player's obituary rules.
```



## Io


```io
withRange := method( a, z,
    Range clone setRange(a,z)
)
sorted := withRange(1,9) asList
numbers := sorted clone shuffle
while( numbers==sorted, numbers = numbers shuffle)

steps :=0
stdin := File standardInput
while( numbers != sorted,
    writeln(numbers join(" "))
    write("Reverse how many? ")
    flipcount := stdin readLine asNumber
    withRange(0, ((flipcount-1)/2) floor) foreach( i,
        numbers swapIndices(i,flipcount-1-i)
    )
    steps = steps+1
)
writeln("Done! That took you ", steps, " steps")
```



## J

'''Solution:'''

```j
require 'misc'                      NB. for the verb prompt

INTRO=: noun define
Number Reversal Game
Flip groups of numbers from the left of the list until
the numbers are sorted in ascending order.
)

reversegame=: verb define
  whilst. (-: /:~)nums do.
    nums=. 1+9?9                    NB. 1-9 in random order
  end.
  score=. 0
  smoutput INTRO                    NB. Display instructions
  while. -.(-: /:~)nums do.
    score=. 1+ score                NB. increment score
    nnum=. 0".prompt (":score),': ',(":nums),' How many numbers to flip?: '
    if. 0 = #nnum do. return. end.  NB. exit on ENTER without number
    nums=. (C.i.-nnum) C. nums      NB. reverse first nnum numbers
  end.
  'You took ',(": score), ' attempts to put the numbers in order.'
)
```

'''Example Usage:'''

```j
   reversegame''
Number Reversal Game
Sort the numbers in ascending order by repeatedly
flipping sets of numbers from the left.

1: 4 7 5 6 9 1 2 8 3 How many numbers to flip?: 5
2: 9 6 5 7 4 1 2 8 3 How many numbers to flip?: 9
3: 3 8 2 1 4 7 5 6 9 How many numbers to flip?: 2
4: 8 3 2 1 4 7 5 6 9 How many numbers to flip?: 8
5: 6 5 7 4 1 2 3 8 9 How many numbers to flip?: 3
6: 7 5 6 4 1 2 3 8 9 How many numbers to flip?: 7
7: 3 2 1 4 6 5 7 8 9 How many numbers to flip?: 5
8: 6 4 1 2 3 5 7 8 9 How many numbers to flip?: 6
9: 5 3 2 1 4 6 7 8 9 How many numbers to flip?: 5
10: 4 1 2 3 5 6 7 8 9 How many numbers to flip?: 4
11: 3 2 1 4 5 6 7 8 9 How many numbers to flip?: 3
You took 11 attempts to put the numbers in order.
```



## Java


```java5
import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.Collections;


public class ReversalGame {
    private List<Integer> gameList;

    public ReversalGame() {
        initialize();
    }

    public void play() throws Exception {
        int i = 0;
        int moveCount = 0;
        Scanner scanner = new Scanner(System.in);
        while (true) {
            System.out.println(gameList);
            System.out.println("Please enter a index to reverse from 2 to 9. Enter 99 to quit");
            i = scanner.nextInt();
            if (i == 99) {
                break;
            }
            if (i < 2 || i > 9) {
                System.out.println("Invalid input");
            } else {
                moveCount++;
                reverse(i);
                if (isSorted()) {
                    System.out.println("Congratulations you solved this in " + moveCount + " moves!");
                    break;
                }
            }

        }
        scanner.close();
    }

    private void reverse(int position) {
        Collections.reverse(gameList.subList(0, position));
    }

    private boolean isSorted() {
        for (int i=0; i < gameList.size() - 1; ++i) {
            if (gameList.get(i).compareTo(gameList.get(i + 1)) > 0) {
                return false;
            }
        }
        return true;
    }

    private void initialize() {
        this.gameList = new ArrayList<Integer>(9);
        for (int i=1; i < 10; ++i) {
            gameList.add(i);
        }
        while (isSorted()) {
            Collections.shuffle(gameList);
        }
    }


    public static void main(String[] args) {
        try {
            ReversalGame game = new ReversalGame();
            game.play();
        } catch (Exception e) {
            System.out.println(e.getMessage());
            e.printStackTrace();
        }
    }
}
```



## JavaScript

Use <code>knuth_shuffle()</code> function from [[Knuth shuffle#JavaScript|here]].


```html4strict><html

<head>
    <title>Number Reversal Game</title>
</head>

<body>
<div id="start"></div>
<div id="progress"></div>
<div id="score"></div>
<script type="text/javascript">
```


```javascript
function endGame(progress) {
    var scoreId = progress.scoreId,
        result = 'You took ' + progress.count + ' attempts to put the digits in order!';
    if (progress.abort === true) {
        result = 'Game aborted.';
    }
    document.getElementById(scoreId).innerHTML = result;
}

function reverseFirstN(arr, n) {
    var reversed = arr.slice(0, n).reverse();
    return reversed.concat(arr.slice(n));
}

function isSorted(arr) {
    return arr.slice(0).sort().toString() === arr.toString();
}

function gameLoop(progress) {
    if (isSorted(progress.arr)) {
        endGame(progress);
    } else {
        var n = parseInt(window.prompt('How many elements to reverse?', ''), 10);
        if (isNaN(n)) {
            progress.abort = true;
        } else {
            progress.arr = reverseFirstN(progress.arr, n);
            progress.innerHTML += '<p>' + progress.arr + '</p>';
            progress.count += 1;
        }
        if (progress.abort !== true) {
            // allow window to repaint before next guess
            setTimeout(function () {
                gameLoop(progress);
            }, 1);
        }
    }
}

function knuth_shuffle(a) {
    var n = a.length,
        r,
        temp;
    while (n > 1) {
        r = Math.floor(n * Math.random());
        n -= 1;
        temp = a[n];
        a[n] = a[r];
        a[r] = temp;
    }
    return a;
}

function playGame(startId, progressId, scoreId) {
    var progress = document.getElementById(progressId);
    progress.arr = knuth_shuffle([1, 2, 3, 4, 5, 6, 7, 8, 9]);
    document.getElementById(startId).innerHTML = '<p>' + progress.arr.toString() + '</p>';

    progress.count = 0;
    progress.scoreId = scoreId;

    // allow window to repaint before prompting for a guess
    setTimeout(function () {
        gameLoop(progress);
    }, 1);
}

playGame('start', 'progress', 'score');
```


```html4strict></script

</body>
</html>
```



## jq


```jq
# Input: the initial array
def play:
  def sorted: . == sort;

  def reverse(n):  (.[0:n] | reverse)  + .[n:];

  def prompt: "List: \(.list)\nEnter a pivot number: ";

  def report: "Great! Your score is \(.score)";

  {list: ., score: 0}
  | (if .list | sorted then "List was sorted to begin with."
     else
     prompt,
     ( label $done
     | foreach inputs as $n (.;
         .list |= reverse($n) | .score +=1;
         if .list | sorted then report, break $done else prompt end ))
     end);
```


'''Example'''

```jq
[1,2,3,9,8,7,6,5,4] | play
```


'''Transcript'''

```txt
$ jq -r -n -f play.jq
List: [1,2,3,9,8,7,6,5,4]
Enter a pivot number:
9
List: [4,5,6,7,8,9,3,2,1]
Enter a pivot number:
6
List: [9,8,7,6,5,4,3,2,1]
Enter a pivot number:
9
Great! Your score is 3
```



## Julia


```julia
# v0.6

function numrevgame()
    l = collect(1:9)
    while issorted(l) shuffle!(l) end
    score = 0
    println("# Number reversal game")
    while !issorted(l)
        print("$l\nInsert the index up to which to revert: ")
        n = parse(Int, readline())
        reverse!(l, 1, n)
        score += 1
    end
    println("$l... You won!\nScore: $score")
    return score
end

numrevgame()
```



## Kotlin


```scala
// version 1.1.2

fun isAscending(a: IntArray): Boolean {
    for (i in 0..8) if (a[i] != i + 1) return false
    return true
}

fun main(args: Array<String>) {
    val r = java.util.Random()
    var count = 0
    val numbers = IntArray(9)
    numbers[0] = 2 + r.nextInt(8) // this will ensure list isn't ascending
    for (i in 1..8) {
        var rn: Int
        do {
            rn = 1 + r.nextInt(9)
        } while (rn in numbers)
        numbers[i] = rn
    }
    println("Here's your first list : ${numbers.joinToString()}")
    while (true) {
        var rev: Int
        do {
            print("How many numbers from the left are to be reversed : ")
            rev = readLine()!!.toInt()
        } while (rev !in 2..9)
        count++
        var i = 0
        var j = rev - 1
        while (i < j) {
            val temp = numbers[i]
            numbers[i++] = numbers[j]
            numbers[j--] = temp
        }
        if (isAscending(numbers)) {
            println("Here's your final list : ${numbers.joinToString()}")
            break
        }
        println("Here's your list now   : ${numbers.joinToString()}")
    }
    println("So you've completed the game with a score of $count")
}
```

Sample game:
{{out}}

```txt

Here's your first list : 4, 3, 2, 7, 1, 6, 5, 8, 9
How many numbers from the left are to be reversed : 4
Here's your list now   : 7, 2, 3, 4, 1, 6, 5, 8, 9
How many numbers from the left are to be reversed : 7
Here's your list now   : 5, 6, 1, 4, 3, 2, 7, 8, 9
How many numbers from the left are to be reversed : 2
Here's your list now   : 6, 5, 1, 4, 3, 2, 7, 8, 9
How many numbers from the left are to be reversed : 6
Here's your list now   : 2, 3, 4, 1, 5, 6, 7, 8, 9
How many numbers from the left are to be reversed : 3
Here's your list now   : 4, 3, 2, 1, 5, 6, 7, 8, 9
How many numbers from the left are to be reversed : 4
Here's your final list : 1, 2, 3, 4, 5, 6, 7, 8, 9
So you've completed the game with a score of 6

```



## Lua


```Lua
-- Initialisation
math.randomseed(os.time())
numList = {values = {}}

-- Check whether list contains n
function numList:contains (n)
  for k, v in pairs(self.values) do
    if v == n then return true end
  end
  return false
end

-- Check whether list is in order
function numList:inOrder ()
  for k, v in pairs(self.values) do
    if k ~=v then return false end
  end
  return true
end

-- Create necessarily out-of-order list
function numList:build ()
  local newNum
  repeat
    for i = 1, 9 do
      repeat
        newNum = math.random(1, 9)
      until not numList:contains(newNum)
      table.insert(self.values, newNum)
    end
  until not numList:inOrder()
end

-- Display list of numbers on one line
function numList:show ()
  for k, v in pairs(self.values) do
    io.write(v .. " ")
  end
  io.write(":\t")
end

-- Reverse n values from left
function numList:reverse (n)
  local swapList = {}
  for k, v in pairs(self.values) do
    table.insert(swapList, v)
  end
  for i = 1, n do
    swapList[i] = self.values[n + 1 - i]
  end
  self.values = swapList
end

-- Main procedure
local score = 0
print("\nRosetta Code Number Reversal Game in Lua")
print("
### ==================================
\n")
numList:build()
repeat
  numList:show()
  numList:reverse(tonumber(io.read()))
  score = score + 1
until numList:inOrder()
numList:show()
print("\n\nW00t!  You scored:", score)
```



## M2000 Interpreter

Using some ready made Print's from BASIC example

Using stack of values which we can move easy values. When we call subs and pass arguments that arguments as values stored to same stack, but because we pop them (read to variables) we leave stack with 9 values for the game.

To check if we have all values in order, we just check the difference of two items one after the other, if is one, and if not one then we leave checking returning false.

Sub CheckStack get a value by reference. Because stack of values always get values, a reference is a copy of a weak reference, so in CheckStack a new variable defined as local ok, and get the reference from weak reference in stack. When a sub exit any new,including local variables removed (deleted). Subs are always local to Module, and have same scope as module scope.

To display the values of stack we use Stack statement with no arguments.


```M2000 Interpreter

Module Number_Reversal_Game {
      PRINT "Given a jumbled list of the numbers 1 to 9,"
      PRINT "you must select how many digits from the left to reverse."
      PRINT "Your goal is to get the digits in order with 1 on the left and 9 on the right."
      \\ work on a new stack - old stack parked, and attached at the exit of this block
      Stack New {
            Data 1,2,3,4,5,6,7,8,9
            \\ Create jumbled list
            For i=1 to 30: Reverse(Random(2,9)):Next i
            Tries=0
            fin=false
           Repeat {
            \\ Show Stack
                  Stack
                  Try ok {
                        INPUT " -- How many numbers should be flipped:", flp%
                  }
                  if not Ok then print: Restart
                  if flp%<2 or flp%>9 then Restart
                  Reverse(flp%)
                  Tries++
                  CheckStack(&fin)
            } until Fin
            \\ show stack again
            Stack
            PRINT "You took "; tries; " tries to put the digits in order."
      }
      Sub Reverse(n)
            Shift 1, -n       ' shift one item nth times in reverse
      End Sub
      Sub CheckStack(&ok)
            ok=true
            if stack.size<2 then exit sub
            Local i
            For i=2 to stack.size {
                        ok=stackitem(i)-stackitem(i-1)=1
                        if ok else exit
            }
      End Sub
}
Number_Reversal_Game

```



## Mathematica

<lang>Module[{array = Range@9, score = 0},
 While[array == Range@9, array = RandomSample@Range@9];
 While[array != Range@9,
  Print@array; (array[[;; #]] = Reverse@array[[;; #]]) &@
   Input["How many digits would you like to reverse?"]; score++];
 Print@array; Print["Your score:", score]]
```



## MATLAB


```MATLAB
function NumberReversalGame
    list = randperm(9);
    while issorted(list)
        list = randperm(9);
    end
    fprintf('Given a list of numbers, try to put them into ascending order\n')
    fprintf('by sequentially reversing everything left of a point you choose\n')
    fprintf('in the array. Try to do it in as few reversals as possible.\n')
    fprintf('No input will quit the game.\n')
    fprintf('Position Num:%s\n', sprintf(' %d', 1:length(list)))
    fprintf('Current List:%s', sprintf(' %d', list))
    pivot = 1;
    nTries = 0;
    while ~isempty(pivot) && ~issorted(list)
        pivot = input('    Enter position of reversal limit: ');
        if pivot
            list(1:pivot) = list(pivot:-1:1);
            fprintf('Current List:%s', sprintf(' %d', list))
            nTries = nTries+1;
        end
    end
    if issorted(list)
        fprintf('\nCongratulations! You win! Only %d reversals.\n', nTries)
    else
        fprintf('\nPlay again soon!\n')
    end
end
```

{{out}}

```txt
Given a list of numbers, try to put them into ascending order
by sequentially reversing everything left of a point you choose
in the array. Try to do it in as few reversals as possible.
No input will quit the game.
Position Num: 1 2 3 4 5 6 7 8 9
Current List: 1 6 3 2 8 7 9 4 5    Enter position of reversal limit: 7
Current List: 9 7 8 2 3 6 1 4 5    Enter position of reversal limit: 9
Current List: 5 4 1 6 3 2 8 7 9    Enter position of reversal limit: 7
Current List: 8 2 3 6 1 4 5 7 9    Enter position of reversal limit: 8
Current List: 7 5 4 1 6 3 2 8 9    Enter position of reversal limit: 7
Current List: 2 3 6 1 4 5 7 8 9    Enter position of reversal limit: 3
Current List: 6 3 2 1 4 5 7 8 9    Enter position of reversal limit: 6
Current List: 5 4 1 2 3 6 7 8 9    Enter position of reversal limit: 5
Current List: 3 2 1 4 5 6 7 8 9    Enter position of reversal limit: 3
Current List: 1 2 3 4 5 6 7 8 9
Congratulations! You win! Only 9 reversals.
```



## Nim


```nim
import math, rdstdin, strutils, algorithm
randomize()

proc shuffle[T](x: var seq[T]) =
  for i in countdown(x.high, 0):
    let j = random(i + 1)
    swap(x[i], x[j])

proc isSorted[T](s: openarray[T]): bool =
  var last = low(T)
  for c in s:
    if c < last:
      return false
    last = c
  return true

proc toString[T](s: openarray[T]): string =
  result = ""
  for i, x in s:
    if i > 0:
      result.add " "
    result.add($x)

echo """number reversal game
    Given a jumbled list of the numbers 1 to 9
    Show the list.
    Ask the player how many digits from the left to reverse.
    Reverse those digits then ask again.
    until all the digits end up in ascending order."""

var data = @[1,2,3,4,5,6,7,8,9]
var trials = 0
while isSorted data:
  shuffle data
while not isSorted data:
  inc trials
  var flip = parseInt readLineFromStdin(
    "#" & $trials & ": List: '" & toString(data) & "' Flip how many?: ")
  reverse(data, 0, flip - 1)

echo "You took ", trials, " attempts to put the digits in order!"
```

Example:

```txt
#1: List: '6 5 8 7 2 1 9 3 4' Flip how many?: 5
#2: List: '2 7 8 5 6 1 9 3 4' Flip how many?: 6
#3: List: '1 6 5 8 7 2 9 3 4' Flip how many?: 9
#4: List: '4 3 9 2 7 8 5 6 1' Flip how many?: 4
#5: List: '2 9 3 4 7 8 5 6 1' Flip how many?: 8
#6: List: '6 5 8 7 4 3 9 2 1' Flip how many?: 6
#7: List: '3 4 7 8 5 6 9 2 1' Flip how many?: 7
#8: List: '9 6 5 8 7 4 3 2 1' Flip how many?: 3
#9: List: '5 6 9 8 7 4 3 2 1' Flip how many?: 5
#10: List: '7 8 9 6 5 4 3 2 1' Flip how many?: 3
#11: List: '9 8 7 6 5 4 3 2 1' Flip how many?: 9
You took 11 attempts to put the digits in order!
```



## OCaml



###  Imperative



```ocaml
let swap ar i j =
  let tmp = ar.(i) in
  ar.(i) <- ar.(j);
  ar.(j) <- tmp

let shuffle ar =
  for i = pred(Array.length ar) downto 1 do
    let j = Random.int (i + 1) in
    swap ar i j
  done

let reversal ar n =
  for i = 0 to pred(n/2) do
    let j = (pred n) - i in
    swap ar i j
  done

let sorted ar =
  try
    let prev = ref ar.(0) in
    for i = 1 to pred(Array.length ar) do
      if ar.(i) < !prev then raise Exit;
      prev := ar.(i)
    done;
    (true)
  with Exit ->
    (false)

let () =
  print_endline "\
  Number Reversal Game
  Sort the numbers in ascending order by repeatedly
  flipping sets of numbers from the left.";
  Random.self_init();
  let nums = Array.init 9 (fun i -> succ i) in
  while sorted nums do shuffle nums done;
  let n = ref 1 in
  while not(sorted nums) do
    Printf.printf "#%2d: " !n;
    Array.iter (Printf.printf " %d") nums;
    print_newline();
    let r = read_int() in
    reversal nums r;
    incr n;
  done;
  print_endline "Congratulations!";
  Printf.printf "You took %d attempts to put the digits in order.\n" !n;
;;
```



###  Functional



```ocaml
let revert li n =
  let rec aux acc i = function
  | [] -> acc
  | xs when i <= 0 -> acc @ xs
  | x::xs -> aux (x::acc) (pred i) xs
  in
  aux [] n li

let take li n =
  let rec aux acc i = function
  | x::xs when i = n -> (x, List.rev_append acc xs)
  | x::xs -> aux (x::acc) (succ i) xs
  | _ -> invalid_arg "take"
  in
  aux [] 0 li

let shuffle li =
  let rec aux acc len = function
  | [] -> acc
  | li ->
      let x, xs = take li (Random.int len) in
      aux (x::acc) (pred len) xs
  in
  aux [] (List.length li) li

let rec sorted = function
  | [] -> (true)
  | x::y::_ when x > y -> (false)
  | x::xs -> sorted xs

let () =
  print_endline "\
  Number Reversal Game
  Sort the numbers in ascending order by repeatedly
  flipping sets of numbers from the left.";
  Random.self_init();
  let li = shuffle [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let rec loop n li =
    Printf.printf "#%2d: " n;
    List.iter (Printf.printf " %d") li;
    Printf.printf "  ? %!";
    let r = read_int() in
    let li = revert li r in
    if not(sorted li)
    then loop (succ n) li
    else Printf.printf "You took %d attempts to put the digits in order.\n" n;
  in
  loop 1 li
;;
```



## Oforth



```Oforth
import: console

: reversalGame
| l n |
   doWhile: [
      ListBuffer new ->l
      while(l size 9 <>) [ 9 rand dup l include ifFalse: [ l add ] else: [ drop ] ]
      l sort l ==
      ]

   0 while(l sort l <>) [
      System.Out "List is " << l << " ==> how many digits from left to reverse : " <-
      System.Console askln asInteger dup ifNull: [ drop continue ] ->n
      1+ l left(n) reverse l right(l size n -) + ->l
      ]
   "You won ! Your score is :" . println ;
```


{{out}}

```txt

>reversalGame
List is [9, 1, 3, 6, 5, 7, 8, 4, 2] ==> how many digits from left to reverse : 9
List is [2, 4, 8, 7, 5, 6, 3, 1, 9] ==> how many digits from left to reverse : 3
List is [8, 4, 2, 7, 5, 6, 3, 1, 9] ==> how many digits from left to reverse : 8
List is [1, 3, 6, 5, 7, 2, 4, 8, 9] ==> how many digits from left to reverse : 5
List is [7, 5, 6, 3, 1, 2, 4, 8, 9] ==> how many digits from left to reverse : 7
List is [4, 2, 1, 3, 6, 5, 7, 8, 9] ==> how many digits from left to reverse : 5
List is [6, 3, 1, 2, 4, 5, 7, 8, 9] ==> how many digits from left to reverse : 6
List is [5, 4, 2, 1, 3, 6, 7, 8, 9] ==> how many digits from left to reverse : 5
List is [3, 1, 2, 4, 5, 6, 7, 8, 9] ==> how many digits from left to reverse : 3
List is [2, 1, 3, 4, 5, 6, 7, 8, 9] ==> how many digits from left to reverse : 2
You won ! Your score is : 10

```



## Oz


```oz
declare
  proc {Main}
     proc {Loop N Xs}
        if {Not {IsSorted Xs}} then
           Num NewXs
        in
           {System.printInfo N#": "}
           {System.print Xs}
           {System.printInfo " -- Reverse how many? "}
           Num = {String.toInt {ReadLine}}
           NewXs = {Append
                    {Reverse {List.take Xs Num}}
                    {List.drop Xs Num}}
           {Loop N+1 NewXs}
        else
           {System.showInfo "You took "#N#" tries to put the digits in order."}
        end
     end
     fun {EnsureShuffled Xs}
        Ys = {Shuffle Xs}
     in
        if {Not {IsSorted Ys}} then Ys
        else {EnsureShuffled Xs}
        end
     end
  in
     {Loop 0 {EnsureShuffled {List.number 1 9 1}}}
  end

  fun {IsSorted Xs}
     {Sort Xs Value.'<'} == Xs
  end

  local
     class TextFile from Open.file Open.text end
     StdIn = {New TextFile init(name:stdin)}
  in
     fun {ReadLine}
        {StdIn getS($)}
     end
  end

  fun {Shuffle Xs}
     {FoldL Xs
      fun {$ Z _}
         {Pick {Diff Xs Z}}|Z
      end
      nil}
  end

  fun {Pick Xs}
     {Nth Xs {OS.rand} mod {Length Xs} + 1}
  end

  fun {Diff Xs Ys}
     {FoldL Ys List.subtract Xs}
  end
in
  {Main}
```



## PARI/GP


```parigp
game()={
  my(v=numtoperm(9,random(9!-1)),score,in,t); \\ Create vector with 1..9, excluding the one sorted in ascending order
  while(v!=vecsort(v),
    print(concat(concat([""],v)));
    in=input();
    for(i=0,in\2-1,
      t=v[9-i];
	  v[9-i]=v[10-in+i];
	  v[10-in+i]=t
    );
    score++
  );
  score
};
```



## Perl


```Perl
use List::Util qw(shuffle);

my $turn = 0;
my @jumble = shuffle 1..9;

while ( join('', @jumble) eq '123456789' ) {
    @jumble = shuffle 1..9;
}

until ( join('', @jumble) eq '123456789' ) {
    $turn++;
    printf "%2d: @jumble - Flip how many digits ? ", $turn;

    my $d = <>;

    @jumble[0..$d-1] = reverse @jumble[0..$d-1];
}

print "    @jumble\n";
print "You won in $turn turns.\n";
```


Output:

```txt
 1: 9 8 2 5 1 7 6 3 4 - Flip how many digits ? 9
 2: 4 3 6 7 1 5 2 8 9 - Flip how many digits ? 4
 3: 7 6 3 4 1 5 2 8 9 - Flip how many digits ? 7
 4: 2 5 1 4 3 6 7 8 9 - Flip how many digits ? 2
 5: 5 2 1 4 3 6 7 8 9 - Flip how many digits ? 5
 6: 3 4 1 2 5 6 7 8 9 - Flip how many digits ? 2
 7: 4 3 1 2 5 6 7 8 9 - Flip how many digits ? 4
 8: 2 1 3 4 5 6 7 8 9 - Flip how many digits ? 2
    1 2 3 4 5 6 7 8 9
You won in 8 turns.
```



## Perl 6


Do-at-least-once loops are fairly rare, but this program wants to have two of them.  We use the built-in <tt>.pick(*)</tt> method to shuffle the numbers.  We use <tt>.=</tt> to dispatch a mutating method in two spots; the first is just a different way to write <tt>++</tt>, while the second of these reverses an array slice in place.  The <tt>[<]</tt> is a reduction operator on less than, so it returns true if the elements of the list are strictly ordered.  We also see in the first repeat loop that, although the while condition is not tested till after the loop, the while condition can in fact declare the variable that will be initialized the first time through the loop, which is a neat trick, and not half unreadable once you get used to it.


```perl6
repeat while [<] my @jumbled-list {
    @jumbled-list = (1..9).pick(*)
}

my $turn = 0;
repeat until [<] @jumbled-list {
    my $d = prompt $turn.=succ.fmt('%2d: ') ~
                   @jumbled-list ~
                   " - Flip how many digits? "
        or exit;
    @jumbled-list[^$d] .= reverse;
}

say "    @jumbled-list[]";
say "You won in $turn turns.";
```


Output:

```txt
 1: 3 5 8 2 7 9 6 1 4 - Flip how many digits ? 6
 2: 9 7 2 8 5 3 6 1 4 - Flip how many digits ? 9
 3: 4 1 6 3 5 8 2 7 9 - Flip how many digits ? 6
 4: 8 5 3 6 1 4 2 7 9 - Flip how many digits ? 8
 5: 7 2 4 1 6 3 5 8 9 - Flip how many digits ? 7
 6: 5 3 6 1 4 2 7 8 9 - Flip how many digits ? 3
 7: 6 3 5 1 4 2 7 8 9 - Flip how many digits ? 6
 8: 2 4 1 5 3 6 7 8 9 - Flip how many digits ? 4
 9: 5 1 4 2 3 6 7 8 9 - Flip how many digits ? 5
10: 3 2 4 1 5 6 7 8 9 - Flip how many digits ? 3
11: 4 2 3 1 5 6 7 8 9 - Flip how many digits ? 4
12: 1 3 2 4 5 6 7 8 9 - Flip how many digits ? 2
13: 3 1 2 4 5 6 7 8 9 - Flip how many digits ? 3
14: 2 1 3 4 5 6 7 8 9 - Flip how many digits ? 2
    1 2 3 4 5 6 7 8 9
You won in 14 turns.
```



## Phix

Simplified copy of [[Number_reversal_game#Euphoria|Euphoria]]

```Phix
puts(1,"Given a jumbled list of the numbers 1 to 9,\n")
puts(1,"you must select how many digits from the left to reverse.\n")
puts(1,"Your goal is to get the digits in order with 1 on the left and 9 on the right.\n")

constant inums = tagset(9)
sequence nums
integer turns = 0, flip

while 1 do
    nums = shuffle(inums)
    if nums!=inums then exit end if
end while

while 1 do
    printf(1,"%2d : %d %d %d %d %d %d %d %d %d ",turns&nums)
    if nums=inums then exit end if
    flip = prompt_number(" -- How many numbers should be flipped? ",{1,9})
    nums[1..flip] = reverse(nums[1..flip])
    turns += 1
end while

printf(1,"\nYou took %d turns to put the digits in order.", turns)
```

{{out}}
<pre style="font-size: 8px">
Given a jumbled list of the numbers 1 to 9,
you must select how many digits from the left to reverse.
Your goal is to get the digits in order with 1 on the left and 9 on the right.
 0 : 3 7 8 1 6 2 5 4 9  -- How many numbers should be flipped? 3
 1 : 8 7 3 1 6 2 5 4 9  -- How many numbers should be flipped? 8
 2 : 4 5 2 6 1 3 7 8 9  -- How many numbers should be flipped? 4
 3 : 6 2 5 4 1 3 7 8 9  -- How many numbers should be flipped? 6
 4 : 3 1 4 5 2 6 7 8 9  -- How many numbers should be flipped? 4
 5 : 5 4 1 3 2 6 7 8 9  -- How many numbers should be flipped? 5
 6 : 2 3 1 4 5 6 7 8 9  -- How many numbers should be flipped? 2
 7 : 3 2 1 4 5 6 7 8 9  -- How many numbers should be flipped? 3
 8 : 1 2 3 4 5 6 7 8 9
You took 8 turns to put the digits in order.

```



## PHP



```PHP
class ReversalGame {
    private $numbers;

    public function __construct() {
        $this->initialize();
    }

    public function play() {
        $i = 0;
        $moveCount = 0;
        while (true) {
            echo json_encode($this->numbers) . "\n";
            echo "Please enter an index to reverse from 2 to 9. Enter 99 to quit\n";
            $i = intval(rtrim(fgets(STDIN), "\n"));
            if ($i == 99) {
                break;
            }
            if ($i < 2 || $i > 9) {
                echo "Invalid input\n";
            } else {
                $moveCount++;
                $this->reverse($i);
                if ($this->isSorted()) {
                    echo "Congratulations you solved this in $moveCount moves!\n";
                    break;
                }
            }

        }
    }

    private function reverse($position) {
        array_splice($this->numbers, 0, $position, array_reverse(array_slice($this->numbers, 0, $position)));
    }

    private function isSorted() {
        for ($i = 0; $i < count($this->numbers) - 1; ++$i) {
            if ($this->numbers[$i] > $this->numbers[$i + 1]) {
                return false;
            }
        }
        return true;
    }

    private function initialize() {
        $this->numbers = range(1, 9);
        while ($this->isSorted()) {
            shuffle($this->numbers);
        }
    }

}

$game = new ReversalGame();
$game->play();

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(de reversalGame ()
   (let (Lst (shuffle (range 1 9))  Cnt 0)
      (while (apply < Lst)
         (setq Lst (shuffle Lst)) )
      (loop
         (printsp Lst)
         (T (apply < Lst) Cnt)
         (NIL (num? (read)))
         (setq Lst (flip Lst @))
         (inc 'Cnt) ) ) )
```

Output:

```txt
: (reversalGame)
(1 7 6 8 4 2 3 5 9) 4
(8 6 7 1 4 2 3 5 9) 8
(5 3 2 4 1 7 6 8 9) 6
(7 1 4 2 3 5 6 8 9) 7
(6 5 3 2 4 1 7 8 9) 6
(1 4 2 3 5 6 7 8 9) 2
(4 1 2 3 5 6 7 8 9) 4
(3 2 1 4 5 6 7 8 9) 3
(1 2 3 4 5 6 7 8 9) -> 8
```



## PL/I


```PL/I

digits: procedure options (main); /* 23 April 2010 */
   declare s character (9) varying;
   declare i fixed binary;
   declare digit character (1);

restart:
   put skip list ('In this game, you are given a group of digits.');
   put skip list ('You will specify one of those digits.');
   put skip list ('The computer will then reverse the digits up to the one you nominate.');
   put skip list ('Your task is to repeat this process a number of times until all the digits');
   put skip list ('are in order, left to right, 1 to 9.');
   put skip list ('Here are your digits');

redo:
   s = '';
   do until (length(s) = 9);
      digit = trim ( fixed(trunc (1+random()*9) ) );
      if index(s, digit) = 0 then s = s || digit;
   end;
   if s = '123456789' then go to redo;

loop:
   do forever;
      put skip list (s);
      if s = '123456789' then leave;
      get edit (digit) (a(1));
      i = index(s, digit);
      if i = 0 then do; put skip list ('invalid request'); iterate loop; end;
      s = reverse( substr(s, 1, i) ) || substr(s, i+1, length(s)-i);
   end;
   put skip list ('Congratulations');
   go to restart;
end digits;

```



## PowerShell


```PowerShell

#adding the below function to the previous users submission to prevent the small
#chance of getting an array that is in ascending order.

#Full disclosure: I am an infrastructure engineer, not a dev. My code is likely
#bad.

function generateArray{
    $fArray = 1..9 | Get-Random -Count 9
    if (-join $fArray -eq -join @(1..9)){
        generateArray
    }
    return $fArray
}
$array = generateArray

#everything below is untouched from original submission
$nTries = 0
While(-join $Array -ne -join @(1..9)){
    $nTries++
    $nReverse = Read-Host -Prompt "[$Array] -- How many digits to reverse? "
    [Array]::Reverse($Array,0,$nReverse)
}
"$Array"
"Your score: $nTries"

```



## PureBasic


```PureBasic
Dim MyList(9)

Declare is_list_sorted()

If OpenConsole()
  Define score, indata, i, txt$

  For i=1 To 9         ;- Initiate the list
    MyList(i)=i
  Next
  While is_list_sorted()
    For i=1 To 9      ;- Do a Fisher–Yates shuffle
      Swap MyList(i), MyList(Random(i)+1)
    Next
  Wend

  ;- Start the Game
  Repeat
    score+1
    txt$=RSet(str(score), 3)+": "      ;- Show current list
    For i=1 To 9
      txt$+str(MyList(i))+" "
    Next
    Repeat                             ;- Get input & swap
      Print(txt$+"| How many numbers should be flipped? "): indata=Val(Input())
    Until indata>=1 And indata<=9      ;- Verify the input
    For i=1 To (indata/2)
      Swap MyList(i),MyList(indata-i+1)
    Next
  Until is_list_sorted()

  ;- Present result & wait for users input before closing down
  PrintN(#CRLF$+"You did it in "+str(score)+" moves")
  Print("Press ENTER to exit"): Input()
  CloseConsole()
EndIf

Procedure is_list_sorted()
  Protected i
  Shared MyList()
  For i=1 To 9
    If MyList(i)<>i
      ProcedureReturn #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure
```



## Python


```python
'''
number reversal game
    Given a jumbled list of the numbers 1 to 9
    Show the list.
    Ask the player how many digits from the left to reverse.
    Reverse those digits then ask again.
    until all the digits end up in ascending order.

'''

import random

print(__doc__)
data, trials = list('123456789'), 0
while data == sorted(data):
    random.shuffle(data)
while data != sorted(data):
    trials += 1
    flip = int(input('#%2i: LIST: %r Flip how many?: ' % (trials, ' '.join(data))))
    data[:flip] = reversed(data[:flip])

print('\nYou took %2i attempts to put the digits in order!' % trials)
```


'''Sample output:'''

```txt

number reversal game
    Given a jumbled list of the numbers 1 to 9
    Show the list.
    Ask the player how many digits from the left to reverse.
    Reverse those digits then ask again.
    until all the digits end up in ascending order.


# 1: LIST: '1 3 9 2 7 5 4 8 6' Flip how many?: 9
# 2: LIST: '6 8 4 5 7 2 9 3 1' Flip how many?: 6
# 3: LIST: '2 7 5 4 8 6 9 3 1' Flip how many?: 8
# 4: LIST: '3 9 6 8 4 5 7 2 1' Flip how many?: 7
# 5: LIST: '7 5 4 8 6 9 3 2 1' Flip how many?: 3
# 6: LIST: '4 5 7 8 6 9 3 2 1' Flip how many?: 6
# 7: LIST: '9 6 8 7 5 4 3 2 1' Flip how many?: 2
# 8: LIST: '6 9 8 7 5 4 3 2 1' Flip how many?: 4
# 9: LIST: '7 8 9 6 5 4 3 2 1' Flip how many?: 3
#10: LIST: '9 8 7 6 5 4 3 2 1' Flip how many?: 9

You took 10 attempts to put the digits in order!
```



## R

<lang>reversalGame <- function(){
  cat("Welcome to the Number Reversal Game! \n")
  cat("Sort the numbers into ascending order by repeatedly \n",
      "reversing the first n digits, where you specify n. \n \n", sep="")

  # Generate a list that's definitely not in ascending order, per instuctions
  data <- sample(1:9, 9)
  while (all(data == 1:9)){
    cat("What were the chances...? \n")
    data <- sample(1:9, 9)
  }
  trials <- 0

  # Play the game
  while (any(data != 1:9)){
    trials <- trials + 1
    cat("Trial", sprintf("%02d", trials), " # ", data, " #  ")
    answer <- readline(paste("Flip how many? "))
    data[1:answer] <- data[answer:1]
  }

  # Victory!
  cat("Well done.  You needed", trials, "flips. \n")
}
```


Sample output:
<lang>>reversalGame()
Welcome to the Number Reversal Game!
Sort the numbers into ascending order by repeatedly
reversing the first n digits, where you specify n.

Trial 01  #  4 3 2 1 7 9 8 5 6  #  Flip how many? 7
Trial 02  #  8 9 7 1 2 3 4 5 6  #  Flip how many? 2
Trial 03  #  9 8 7 1 2 3 4 5 6  #  Flip how many? 9
Trial 04  #  6 5 4 3 2 1 7 8 9  #  Flip how many? 6
Well done.  You needed 4 flips.
```



## Racket


```Racket
#lang racket
(let loop ([nums (range 1 10)] [n 0])
  (cond [(apply < nums) (if (zero? n)
                            (loop (shuffle nums) 0)
                            (printf "Done in ~s steps.\n" n))]
        [else (printf "Step #~s: ~s\nFlip how many? " n nums)
              (define-values (l r) (split-at nums (read)))
              (loop (append (reverse l) r) (add1 n))]))
```



## Rascal


```Rascal
import Prelude;
import vis::Figure;
import vis::Render;

public void NumberReversalGame(){

	//generate randomlist
	L = [1..9];
	score = 0;
	randomlist = [];
	while (L != []){
		temp = takeOneFrom(L);
		randomlist += temp[0];
		L = temp[1];
	}

	// user interaction
	score = 0;
	text1 = "";
	figure =
	box(vcat([
		box(text(str(){
			return "<randomlist>";})),
		box(textfield("Insert number of digits you would like to reverse here.",
			void(str s){
				score += 1;
				n = toInt(s);
				spliced = slice(randomlist, 0, n);
				randomlist = reverse(spliced) + (randomlist - spliced);
				},
			fillColor("lightblue"))),
		box(text(str(){
			return ((randomlist == [1 .. 9]) ? "Well done! Your score: <score>." : "Keep going!");}))
				]));

	render(figure);
}
```


Output:

[[File:NRG3.jpg]]


## REBOL



```REBOL
REBOL []

print "NUMBER REVERSAL GAME"

tries: 0
goal: [1 2 3 4 5 6 7 8 9]
random/seed now

until [
    jumble: random goal
    jumble != goal ; repeat in the unlikely case that jumble isn't jumbled
]

while [jumble != goal] [
    prin jumble
    prin " How many to flip? "
    flip-index: to-integer input ; no validation!
    reverse/part jumble flip-index
    tries: tries + 1
]

print rejoin ["You took " tries " attempts."]
```



## REXX

This REXX version:
:::*   displays the game's objective and rules
:::*   validates the input   (must be a single non-zero decimal digit)
:::*   allows the user to enter   '''quit'''
:::*   allows the user to halt the game via   '''Cntl-Break'''   (or equivalent)

```rexx
/*REXX program (a game):  reverse a jumbled set of decimal digits 'til they're in order.*/
signal on halt                                   /*allows the CBLF to  HALT the program.*/
___= copies('─', 9);      pad=left('', 9)        /*a fence used for computer's messages.*/
say ___  "This game will show you nine random unique digits  (1 ──► 9),  and you'll enter"
say ___  "one of those digits  which will reverse all the digits from the left-most digit"
say ___  "up to  (and including)  that decimal digit.  The game's objective is to get all"
say ___  "of the digits in ascending order with the fewest tries.    Here're your digits:"
ok= 123456789                                    /*the result that the string should be.*/
$=                                               /* ◄─── decimal target to be generated.*/
      do  until length($)==9;     _=random(1, 9) /*build a random unique numeric string.*/
      if pos(_, $) \== 0  then iterate           /*¬ unique? Only use a decimal dig once*/
      $=$ || _                                   /*construct a string of unique digits. */
      if $==ok  then $=                          /*string can't be in order, start over.*/
      end   /*until*/

  do  score=1  until  $==ok;           say       /* [↓]  display the digits & the prompt*/
  say ___  $   right('please enter a digit  (or  Quit):', 50)
  parse pull ox  .  1  ux . 1  x  .;   upper ux  /*get a decimal digit (maybe) from CBLF*/
  if abbrev('QUIT', ux, 1)  then signal halt     /*does the CBLF want to quit this game?*/
  if length(x)>1  then do;  say ___ pad '***error***  invalid input:  ' ox;  iterate;  end
  if x=''  then  iterate                         /*try again, CBLF didn't enter anything*/
  g=pos(x, $)                                    /*validate if the input digit is legal.*/
  if g==0  then say ___ pad '***error***  invalid digit:  '     ox
           else $=strip(reverse(left($,g))substr($,g+1))  /*reverse some (or all) digits*/
  end   /*score*/

say;    say ___  $;     say;    say center(' Congratulations! ', 70, "═");       say
say ___ pad  'Your score was' score;  exit       /*stick a fork in it,  we're all done. */
halt:  say  ___  pad  'quitting.';     exit      /*  "   "   "   "  "     "    "    "   */
```

{{out|output|text=  from playing one game of the   ''number reversal game'':

```txt

───────── This game will show you nine random unique digits  (1 ──► 9),  and you'll enter
───────── one of those digits  which will reverse all the digits from the left-most digit
───────── up to  (and including)  that decimal digit.  The game's objective is to get all
───────── of the digits in ascending order with the fewest tries.    Here're your digits:

───────── 613279548                  please enter a digit  (or  Quit):
5                    ◄■■■■■■■■■ user input

───────── 597231648                  please enter a digit  (or  Quit):
9                    ◄■■■■■■■■■ user input

───────── 957231648                  please enter a digit  (or  Quit):
8                    ◄■■■■■■■■■ user input

───────── 846132759                  please enter a digit  (or  Quit):
5                    ◄■■■■■■■■■ user input

───────── 572316489                  please enter a digit  (or  Quit):
7                    ◄■■■■■■■■■ user input

───────── 752316489                  please enter a digit  (or  Quit):
4                    ◄■■■■■■■■■ user input

───────── 461325789                  please enter a digit  (or  Quit):
6                    ◄■■■■■■■■■ user input

───────── 641325789                  please enter a digit  (or  Quit):
5                    ◄■■■■■■■■■ user input

───────── 523146789                  please enter a digit  (or  Quit):
4                    ◄■■■■■■■■■ user input

───────── 413256789                  please enter a digit  (or  Quit):
2                    ◄■■■■■■■■■ user input

───────── 231456789                  please enter a digit  (or  Quit):
3                    ◄■■■■■■■■■ user input

───────── 321456789                  please enter a digit  (or  Quit):
1                    ◄■■■■■■■■■ user input

───────── 123456789

══════════════════════════ Congratulations! ══════════════════════════

─────────           Your score was 12

```



## Ring


```ring

# Project : Number reversal game

rever = 1:9
leftrever = []
for n = 1 to len(rever)
     rnd = random(8) + 1
     temp = rever[n]
     rever[n] = rever[rnd]
     rever[rnd] = temp
next
see rever
see nl
while true
         num = 0
         leftrever = []
         showarray(rever)
         see " : Reverse how many = "
         give r
         r = number(r)
         for n = 1 to r
               add(leftrever, rever[n])
         next
         leftrever = reverse(leftrever)
         for pos = 1 to r
               rever[pos] = leftrever[pos]
         next
         for m = 1 to len(rever)
              if rever[m] = m
                 num = num + 1
              ok
         next
         if num = len(rever)
            exit
         ok
end
see "You took " + num + " attempts." + nl

func swap(a, b)
       temp = a
       a = b
       b = temp
       return [a, b]

func showarray(vect)
       svect = ""
       for n = 1 to len(vect)
             svect = svect + vect[n] + " "
       next
       svect = left(svect, len(svect) - 1)
       see svect

```

Output:

```txt

7 2 3 1 5 8 6 4 9 : Reverse how many? 6
8 5 1 3 2 7 6 4 9 : Reverse how many? 8
4 6 7 2 3 1 5 8 9 : Reverse how many? 3
7 6 4 2 3 1 5 8 9 : Reverse how many? 7
5 1 3 2 4 6 7 8 9 : Reverse how many? 5
4 2 3 1 5 6 7 8 9 : Reverse how many? 4
1 3 2 4 5 6 7 8 9 : Reverse how many? 3
2 3 1 4 5 6 7 8 9 : Reverse how many? 2
3 2 1 4 5 6 7 8 9 : Reverse how many? 3
You took 9 attempts.

```



## Ruby


```ruby
ary = (1..9).to_a
ary.shuffle! while ary == ary.sort
score = 0
until ary == ary.sort
  print "#{ary.inspect} -- How many digits to reverse? "
  num = gets.to_i  # should validate input
  ary[0, num] = ary[0, num].reverse
  score += 1
end
p ary
puts "Your score: #{score}"
```


sample output:

```txt
$ ruby number_reversal_game.rb
[4, 2, 1, 8, 7, 6, 3, 5, 9] -- How many digits to reverse? 4
[8, 1, 2, 4, 7, 6, 3, 5, 9] -- How many digits to reverse? 8
[5, 3, 6, 7, 4, 2, 1, 8, 9] -- How many digits to reverse? 4
[7, 6, 3, 5, 4, 2, 1, 8, 9] -- How many digits to reverse? 7
[1, 2, 4, 5, 3, 6, 7, 8, 9] -- How many digits to reverse? 4
[5, 4, 2, 1, 3, 6, 7, 8, 9] -- How many digits to reverse? 5
[3, 1, 2, 4, 5, 6, 7, 8, 9] -- How many digits to reverse? 3
[2, 1, 3, 4, 5, 6, 7, 8, 9] -- How many digits to reverse? 2
[1, 2, 3, 4, 5, 6, 7, 8, 9]
Your score: 8
```



## Run BASIC


```runbasic
for i = 1 to 9  ' get numbers 1 to 9
 n(i) = i
next i
numShuffles = 3
' ----------------------------------
' shuffle numbers
' ----------------------------------
for 	i 	= 1 to 9 * numShuffles
	i1	= int(rnd(1)*9) + 1
	i2	= int(rnd(1)*9) + 1
	h2	= n(i1)
	n(i1)	= n(i2)
	n(i2)	= h2
next i

for i = 1 to 9
 a$ = a$ + str$(n(i)) + " "
next i
count = 0

[loop]
  count = count + 1
  print count;" : ";a$
    for i = 1 to 9                ' check if in sequence
      j = i * 2
      if mid$(a$,j-1,1) > mid$(a$,j,1) then goto [notOrdered]
    next i
  print "It took ";count;" tries"
end

[notOrdered]
input "How many numbers to flip:";i
i  = ((i-1) * 2) + 1
b$ = ""
for j = i to 1 step -2
   b$ = b$ + mid$(a$,j,2)
next j
a$  = b$ + mid$(a$,i + 2)
goto [loop]
end
```



## Scala


```Scala
object NumberReversalGame extends App {
  def play(n: Int, cur: List[Int], goal: List[Int]) {
    readLine(s"""$n. ${cur mkString " "}  How many to flip? """) match {
      case null => println
      case s => scala.util.Try(s.toInt) match {
        case scala.util.Success(i) if i > 0 && i <= cur.length =>
          (cur.take(i).reverse ++ cur.drop(i)) match {
            case done if done == goal =>
              println(s"Congratulations! You solved "+goal.mkString(" "))
              println(s"Your score is $n (lower is better)")
            case next => play(n + 1, next, goal)
          }
        case _ => println(s"Choose a number between 1 and ${cur.length}")
          play(n + 1, cur, goal)
      }
    }
  }

  def play(size: Int) {
    val goal = List.range(1, size + 1)
    def init: List[Int] = scala.util.Random.shuffle(goal) match {
      case repeat if repeat == goal => init
      case done => done
    }
    play(1, init, goal)
  }

  play(9)
}
```

{{out}}

```txt
1. 8 4 2 9 6 3 7 5 1  How many to flip? 3
2. 2 4 8 9 6 3 7 5 1  How many to flip? 8
3. 5 7 3 6 9 8 4 2 1  How many to flip? 3
4. 3 7 5 6 9 8 4 2 1  How many to flip? 7
5. 4 8 9 6 5 7 3 2 1  How many to flip? 6
6. 7 5 6 9 8 4 3 2 1  How many to flip? 2
7. 5 7 6 9 8 4 3 2 1  How many to flip? 5
8. 8 9 6 7 5 4 3 2 1  How many to flip? 3
9. 6 9 8 7 5 4 3 2 1  How many to flip? 4
10. 7 8 9 6 5 4 3 2 1  How many to flip? 3
11. 9 8 7 6 5 4 3 2 1  How many to flip? 9
Congratulations! You solved 1 2 3 4 5 6 7 8 9
Your score is 11 (lower is better)
```



## Scheme


{{libheader|Scheme/SRFIs}}


```scheme

(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 1)   ; list functions
        (srfi 27)) ; random numbers

(random-source-randomize! default-random-source)

(define (make-randomised-list)
  (let ((vec (apply vector (iota 9 1))))
    (do ((c 0 (+ 1 c)))
      ((and (>= c 20)                            ; at least 20 tries
            (not (apply < (vector->list vec)))) ; ensures list not in order
       (vector->list vec))
      (let* ((i (random-integer 9)) ; swap two randomly chosen elements
             (j (random-integer 9))
             (tmp (vector-ref vec i)))
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j tmp)))))

(define (play-game lst plays)
  (define (reverse-first n lst)
    (let-values (((start tail) (split-at lst n)))
                (append (reverse start) tail)))
  ;
  (display "List: ") (display lst) (newline)
  (display "How many numbers should be flipped? ")
  (let* ((flip (string->number (read-line)))
         (new-lst (reverse-first flip lst)))
    (if (apply < new-lst)
      (display (string-append "Finished in "
                              (number->string plays)
                              " attempts\n"))
      (play-game new-lst (+ 1 plays)))))

(play-game (make-randomised-list) 1)

```


{{out}}

```txt

List: (1 8 3 4 9 2 7 5 6)
How many numbers should be flipped? 5
List: (9 4 3 8 1 2 7 5 6)
How many numbers should be flipped? 9
List: (6 5 7 2 1 8 3 4 9)
How many numbers should be flipped? 6
List: (8 1 2 7 5 6 3 4 9)
How many numbers should be flipped? 8
List: (4 3 6 5 7 2 1 8 9)
How many numbers should be flipped? 5
List: (7 5 6 3 4 2 1 8 9)
How many numbers should be flipped? 7
List: (1 2 4 3 6 5 7 8 9)
How many numbers should be flipped? 5
List: (6 3 4 2 1 5 7 8 9)
How many numbers should be flipped? 6
List: (5 1 2 4 3 6 7 8 9)
How many numbers should be flipped? 5
List: (3 4 2 1 5 6 7 8 9)
How many numbers should be flipped? 2
List: (4 3 2 1 5 6 7 8 9)
How many numbers should be flipped? 4
Finished in 11 attempts

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const array integer: sortedList is [] ( 1, 2, 3, 4, 5, 6, 7, 8, 9);
    var array integer: list is 0 times 0;
    var array integer: reversedList is 0 times 0;
    var integer: score is 0;
    var integer: index is 0;
    var integer: number is 0;
    var integer: reverse is 0;
  begin
    for number range sortedList do
      index := rand(1, succ(length(list)));
      list := list[.. pred(index)] & [] (number) & list[index ..];
    end for;
    repeat
      write("Current list: ");
      for number range list do
        write(number <& " ");
      end for;
      repeat
        write(" Digits to reverse? ");
        readln(reverse);
        if reverse < 2 or reverse > 9 then
          write("Please enter a value between 2 and 9.");
        end if;
      until reverse >= 2 and reverse <= 9;
      incr(score);
      reversedList := 0 times 0;
      for index range reverse downto 1 do
        reversedList &:= list[index];
      end for;
      list := reversedList & list[succ(reverse) ..];
    until list = sortedList;
    writeln("Congratulations, you sorted the list in " <& score <& " reversals.");
  end func;
```


{{out}}

```txt

Current list: 7 6 2 9 1 3 4 5 8  Digits to reverse? 4
Current list: 9 2 6 7 1 3 4 5 8  Digits to reverse? 9
Current list: 8 5 4 3 1 7 6 2 9  Digits to reverse? 8
Current list: 2 6 7 1 3 4 5 8 9  Digits to reverse? 3
Current list: 7 6 2 1 3 4 5 8 9  Digits to reverse? 7
Current list: 5 4 3 1 2 6 7 8 9  Digits to reverse? 5
Current list: 2 1 3 4 5 6 7 8 9  Digits to reverse? 2
Congratulations, you sorted the list in 7 reversals.

```



## Sidef

{{trans|Perl}}

```ruby
var turn = 0;
var jumble = @(1..9).bshuffle;        # best-shuffle

for (turn; jumble != 1..9; ++turn) {
    printf("%2d: %s - Flip how many digits ? ", turn, jumble.join(' '));
    var d = read(Number) \\ break;
    jumble[0 .. d-1] = [jumble[0 .. d-1]].reverse...;
}

print "    #{jumble.join(' ')}\n";
print "You won in #{turn} turns.\n";
```



## Tcl


```tcl
package require Tcl 8.5
# Simple shuffler, not very efficient but good enough for here
proc shuffle list {
    set result {}
    while {[llength $list]} {
	set i [expr {int([llength $list] * rand())}]
	lappend result [lindex $list $i]
	set list [lreplace $list $i $i]
    }
    return $result
}
# Returns the list with the prefix of it reversed
proc flipfirst {list n} {
    concat [lreverse [lrange $list 0 $n-1]] [lrange $list $n end]
}

# Core game engine; list to play with is optional argument
proc nrgame {{target "1 2 3 4 5 6 7 8 9"}} {
    set nums $target
    while {$nums eq $target} {set nums [shuffle $nums]}
    set goes 0
    while {$nums ne $target} {
	incr goes
	puts -nonewline "#${goes}: List is '[join $nums {, }]', how many to reverse? "
	flush stdout
	gets stdin n
	if {$n eq "q"} {return quit}
	# Input validation would go here
	set nums [flipfirst $nums $n]
    }
    return $goes
}

# Print some instructions and wait for the user to win
puts "Welcome to the Number Reversal Game!"
puts "------------------------------------"
puts "I'll show you a list of numbers, you need to reverse prefixes of them"
puts "to get the whole list in ascending order. A 'q' will quit early.\n"
puts ""
set outcome [nrgame]
if {$outcome ne "quit"} {
    puts "\nYou took $outcome attempts to put the digits in order."
}
```

Sample output:

```txt

Welcome to the Number Reversal Game!
------------------------------------
I'll show you a list of numbers, you need to reverse prefixes of them
to get the whole list in ascending order. A 'q' will quit early.


#1: List is '8, 6, 2, 5, 7, 9, 3, 1, 4', how many to reverse? 6
#2: List is '9, 7, 5, 2, 6, 8, 3, 1, 4', how many to reverse? 9
#3: List is '4, 1, 3, 8, 6, 2, 5, 7, 9', how many to reverse? 4
#4: List is '8, 3, 1, 4, 6, 2, 5, 7, 9', how many to reverse? 8
#5: List is '7, 5, 2, 6, 4, 1, 3, 8, 9', how many to reverse? 7
#6: List is '3, 1, 4, 6, 2, 5, 7, 8, 9', how many to reverse? 4
#7: List is '6, 4, 1, 3, 2, 5, 7, 8, 9', how many to reverse? 6
#8: List is '5, 2, 3, 1, 4, 6, 7, 8, 9', how many to reverse? 5
#9: List is '4, 1, 3, 2, 5, 6, 7, 8, 9', how many to reverse? 4
#10: List is '2, 3, 1, 4, 5, 6, 7, 8, 9', how many to reverse? 2
#11: List is '3, 2, 1, 4, 5, 6, 7, 8, 9', how many to reverse? 3

You took 11 attempts to put the digits in order.

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
numbers=RANDOM_NUMBERS (1,9,9),nr=0

SECTION check
LOOP o,n=numbers
 IF (n!=o) THEN
  DO PRINT
  EXIT
 ELSEIF (n==9&&o==9) THEN
  DO PRINT
  PRINT " You made it ... in round ",r
  STOP
 ELSE
  CYCLE
 ENDIF
ENDLOOP
ENDSECTION

SECTION print
  PRINT numbers
ENDSECTION


DO PRINT
LOOP r=1,14
IF (nr>=0&&nr<10) THEN
  ASK "Reverse - how many?": nr=""
  i=""
  LOOP n=1,nr
   i=APPEND(i,n)
  ENDLOOP
  numbers   =SPLIT (numbers)
  reverse_nr=SELECT (numbers,#i,keep_nr), reverse_nr=REVERSE(reverse_nr)
  numbers   =APPEND (reverse_nr,keep_nr), numbers   =JOIN   (numbers)
  DO check
ENDIF
ENDLOOP

```

Output:
<pre style='height:30ex;overflow:scroll'>
2'1'7'8'5'6'4'9'3
Reverse - how many? >8
9'4'6'5'8'7'1'2'3
Reverse - how many? >9
3'2'1'7'8'5'6'4'9
Reverse - how many? >5
8'7'1'2'3'5'6'4'9
Reverse - how many? >8
4'6'5'3'2'1'7'8'9
Reverse - how many? >3
5'6'4'3'2'1'7'8'9
Reverse - how many? >2
6'5'4'3'2'1'7'8'9
Reverse - how many? >6
1'2'3'4'5'6'7'8'9
 You made it ... in round 7

```



## UNIX Shell

This task becomes easier if your shell has arrays and a random number generator. POSIX shells have not those. If you would do this task with a POSIX shell, then you would pretend arrays with <tt>eval</tt>, as in <tt>eval "\$array_$index=value"</tt>, and you would implement your own random number generator.

This solution uses the arrays and the random number generator from the Korn shell. It also shows a few other Korn features: the <tt>(( ))</tt> and <tt>[[ ]]</tt> commands, the <tt>print</tt> statement, and some local <tt>integer</tt> variables in some <tt>function</tt>.

{{trans|AWK}}
{{works with|pdksh|5.2.14}}

```bash
print "\nWelcome to the number reversal game!\n"

print "You must put the numbers in order from 1 to 9."
print "Your only moves are to reverse groups of numbers"
print "on the left side of the list."

integer list score

# start a new game
function newgame {
	integer i j t

	# score = number of moves
	((score = 0))

	# list = array of numbers
	set -A list 1 2 3 4 5 6 7 8 9
	while true; do
		# Knuth shuffle
		((i = 9))
		while ((i > 1)); do
			# get random j from 0 to i - 1
			((j = RANDOM))
			((j < 32768 % i)) && continue
			((j %= i))

			# decrement i, swap list[i] with list[j]
			((i -= 1))
			((t = list[i]))
			((list[i] = list[j]))
			((list[j] = t))
		done
		win || break
	done
}

# numbers in order?
function win {
	integer i

	# check if list[0] == 1, list[1] == 2, ...
	((i = 0))
	while ((i < 9)); do
		((list[i] != i + 1)) && return 1
		((i += 1))
	done
	return 0
}

# reverse first $1 elements of list
function reverse {
	integer left right t

	((left = 0))
	((right = $1 - 1))
	while ((right > left)); do
		((t = list[left]))
		((list[left] = list[right]))
		((list[right] = t))
		((left += 1))
		((right -= 1))
	done
}


integer i

newgame
while true; do
	print -n "\nYour list: "
	((i = 0))
	while ((i < 8)); do
		printf "%d, " ${list[i]}
		((i += 1))
	done
	printf "%d\n" ${list[8]}

	if win; then
		print "YOU WIN!"
		printf "Your score is %d moves.\n" $score
		print -n "Would you like to play again (y/n)? "

		if read again && [[ $again = @(y|Y)* ]]; then
			newgame
		else
			print "\n\nBye!"
			exit
		fi
	else
		print -n "How many numbers to reverse? "

		if read i; then
			((score += 1))
			reverse $i
		else
			print "\n\nBye!"
			exit
		fi
	fi
done
```



## VBA

{{incomplete|VBA|No check for an initial scramble to the sorted state (refer to talk page for relevant discussion on issue).}}
{{trans|Phix}}
```vb
Private Function shuffle(ByVal a As Variant) As Variant
    Dim t As Variant, i As Integer
    For i = UBound(a) To LBound(a) + 1 Step -1
        j = Int((UBound(a) - LBound(a) + 1) * Rnd + LBound(a))
        t = a(i)
        a(i) = a(j)
        a(j) = t
    Next i
    shuffle = a
End Function
Private Sub reverse(ByRef a As Variant, n As Integer)
    Dim b As Variant
    b = a
    For i = 1 To n
        a(i) = b(n + 1 - i)
    Next i
End Sub
Public Sub game()
    Debug.Print "Given a jumbled list of the numbers 1 to 9"
    Debug.Print "you must select how many digits from the left to reverse."
    Debug.Print "Your goal is to get the digits in order with 1 on the left and 9 on the right."

    inums = [{1,2,3,4,5,6,7,8,9}]
    Dim nums As Variant
    Dim turns As Integer, flip As Integer

    nums = shuffle(inums)

    Do While True
        Debug.Print turns; ":";
        For Each x In nums: Debug.Print x;: Next x
        Debug.Print
        flag = False
        For i = LBound(nums) To UBound(nums)
            If nums(i) <> inums(i) Then
                flag = True
                Exit For
            End If
        Next i
        If flag Then
            flip = InputBox(" -- How many numbers should be flipped? ")
            reverse nums, flip
            turns = turns + 1
        Else
            Exit Do
        End If
    Loop

    Debug.Print "You took"; turns; "turns to put the digits in order."
End Sub
```
{{out}}

```txt
Given a jumbled list of the numbers 1 to 9
you must select how many digits from the left to reverse.
Your goal is to get the digits in order with 1 on the left and 9 on the right.
 0 : 1  4  3  9  8  7  6  2  5
 1 : 9  3  4  1  8  7  6  2  5
 2 : 5  2  6  7  8  1  4  3  9
 3 : 8  7  6  2  5  1  4  3  9
 4 : 3  4  1  5  2  6  7  8  9
 5 : 5  1  4  3  2  6  7  8  9
 6 : 2  3  4  1  5  6  7  8  9
 7 : 4  3  2  1  5  6  7  8  9
 8 : 1  2  3  4  5  6  7  8  9
You took 8 turns to put the digits in order.
```



## XPL0


```XPL0
int  Taken, I, Digit, Num, Score, Rev, Temp;
char List(9);
include c:\cxpl\codes;
                        \make jumbled list of digits 1 to 9
[loop   [Taken:= 0;     \bit array indicates which digits are taken
        for I:= 0 to 9-1 do
                [repeat Digit:= Ran(9)+1 until (Taken & 1<<Digit) = 0;
                Taken:= Taken + 1<<Digit;       \mark digit as taken
                List(I):= Digit;                \add digit to the list
                ];
for I:= 0 to 9-2 do if List(I) > List(I+1) then quit;
];                      \quit loop when digits are not in ascending order
Score:= 0;
loop    [for I:= 0 to 9-1 do [IntOut(0, List(I));  ChOut(0, ^ )];
        Num:= 0;  for I:= 0 to 9-1 do Num:= Num*10 + List(I);
        if Num = 123456789 then quit;
        Text(0, "^M^JReverse how many digits? ");  Rev:= IntIn(0);
        for I:= 0 to Rev/2-1 do
            [Temp:= List(I);  List(I):= List(Rev-1-I);  List(Rev-1-I):= Temp];
        Score:= Score+1;
        ];
Text(0, "^M^JCongrats! You did it in "); IntOut(0, Score);
Text(0, " moves!!^M^J");
]
```


Example output:

```txt

1 3 5 4 8 9 7 2 6
Reverse how many digits? 3
5 3 1 4 8 9 7 2 6
Reverse how many digits? 4
4 1 3 5 8 9 7 2 6
Reverse how many digits? 5
8 5 3 1 4 9 7 2 6
Reverse how many digits? 2
5 8 3 1 4 9 7 2 6

```



## zkl

{{trans|AutoHotkey}}

```zkl
correctList,scrambledList,N:=[1..9].walk(), correctList.shuffle(),correctList.len();
correctList,scrambledList=correctList.concat(""), scrambledList.concat(""); // list to string
attempts:=0;
while(scrambledList!=correctList){ // Repeat until the sequence is correct
   n:=ask(("[%d] %s How many numbers (from the left) should be flipped? ")
	      .fmt(attempts,scrambledList));
   try{ n=n.toInt() }catch{ println("Not a number"); continue; }
   if(not (0<=n<N)){ println("Out of range"); continue; }
   attempts+=1;
   // Reverse the first part of the string and add the second part
   scrambledList=scrambledList[0,n].reverse() + scrambledList[n,*];
}
println("You took %d attempts to get the correct sequence.".fmt(attempts));
```

{{out}}

```txt

$ zkl bbb
[0] 145327689 How many numbers (from the left) should be flipped? 7
[1] 672354189 How many numbers (from the left) should be flipped? 2
[2] 762354189 How many numbers (from the left) should be flipped? 7
[3] 145326789 How many numbers (from the left) should be flipped? 3
[4] 541326789 How many numbers (from the left) should be flipped? 5
[5] 231456789 How many numbers (from the left) should be flipped? 2
[6] 321456789 How many numbers (from the left) should be flipped? 3
You took 7 attempts to get the correct sequence.

```


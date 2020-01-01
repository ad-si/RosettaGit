+++
title = "Dutch national flag problem"
description = ""
date = 2019-07-20T10:39:40Z
aliases = []
[extra]
id = 11939
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[File:Dutch_flag_3.jpg|350px||right]]

The Dutch national flag is composed of three coloured bands in the order:
::*   red     (top)
::*   then white,   and
::*   lastly blue   (at the bottom).


The problem posed by [[wp:Edsger Dijkstra|Edsger Dijkstra]] is:
:Given a number of red, blue and white balls in random order, arrange them in the order of the colours in the Dutch national flag.
When the problem was first posed, Dijkstra then went on to successively refine a solution, minimising the number of swaps and the number of times the colour of a ball needed to determined and restricting the balls to end in an array, ...


;Task
# Generate a randomized order of balls ''ensuring that they are not in the order of the Dutch national flag''.
# Sort the balls in a way idiomatic to your language.
# Check the sorted balls ''are'' in the order of the Dutch national flag.


;C.f.:
* [[wp:Dutch national flag problem|Dutch national flag problem]]
* [https://www.google.co.uk/search?rlz=1C1DSGK_enGB472GB472&sugexp=chrome,mod=8&sourceid=chrome&ie=UTF-8&q=Dutch+national+flag+problem#hl=en&rlz=1C1DSGK_enGB472GB472&sclient=psy-ab&q=Probabilistic+analysis+of+algorithms+for+the+Dutch+national+flag+problem&oq=Probabilistic+analysis+of+algorithms+for+the+Dutch+national+flag+problem&gs_l=serp.3...60754.61818.1.62736.1.1.0.0.0.0.72.72.1.1.0...0.0.Pw3RGungndU&psj=1&bav=on.2,or.r_gc.r_pw.r_cp.r_qf.,cf.osb&fp=c33d18147f5082cc&biw=1395&bih=951 Probabilistic analysis of algorithms for the Dutch national flag problem] by Wei-Mei Chen. (pdf)





## ABAP

This works for ABAP Version 7.40 and above, the color blue is excluded as an option for the last entry to insure an unsorted sequence.


```ABAP

report z_dutch_national_flag_problem.

interface sorting_problem.
  methods:
    generate_unsorted_sequence
      importing
        lenght_of_sequence       type int4
      returning
        value(unsorted_sequence) type string,

    sort_sequence
      changing
        sequence_to_be_sorted type string,

    is_sorted
      importing
        sequence_to_check type string
      returning
        value(sorted)     type abap_bool.
endinterface.


class dutch_national_flag_problem definition.
  public section.
    interfaces:
      sorting_problem.


    constants:
      begin of dutch_flag_colors,
        red   type char1 value 'R',
        white type char1 value 'W',
        blue  type char1 value 'B',
      end of dutch_flag_colors.
endclass.


class dutch_national_flag_problem implementation.
  method sorting_problem~generate_unsorted_sequence.
    data(random_int_generator) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min = 0
      max = 2 ).

    do lenght_of_sequence - 1 times.
      data(random_int) = random_int_generator->get_next( ).

      data(next_color) = cond char1(
        when random_int eq 0 then dutch_flag_colors-red
        when random_int eq 1 then dutch_flag_colors-white
        when random_int eq 2 then dutch_flag_colors-blue ).

      unsorted_sequence = |{ unsorted_sequence }{ next_color }|.
    enddo.

    if strlen( unsorted_sequence ) > 0.
      random_int = random_int_generator->get_next( ).

      next_color = cond char1(
        when random_int eq 0 or random_int eq 2 then dutch_flag_colors-red
        when random_int eq 1 then dutch_flag_colors-white ).

      unsorted_sequence = |{ unsorted_sequence }{ next_color }|.
    endif.
  endmethod.


  method sorting_problem~sort_sequence.
    data(low_index) = 0.
    data(middle_index) = 0.
    data(high_index) = strlen( sequence_to_be_sorted ) - 1.

    while middle_index <= high_index.
      data(current_color) = sequence_to_be_sorted+middle_index(1).

      if current_color eq dutch_flag_colors-red.
        data(buffer) = sequence_to_be_sorted+low_index(1).

        sequence_to_be_sorted = replace(
          val = sequence_to_be_sorted
          off = middle_index
          len = 1
          with = buffer ).

        sequence_to_be_sorted = replace(
          val = sequence_to_be_sorted
          off = low_index
          len = 1
          with = current_color ).

        low_index = low_index + 1.

        middle_index = middle_index + 1.
      elseif current_color eq dutch_flag_colors-blue.
        buffer = sequence_to_be_sorted+high_index(1).

        sequence_to_be_sorted = replace(
          val = sequence_to_be_sorted
          off = middle_index
          len = 1
          with = buffer ).

        sequence_to_be_sorted = replace(
          val = sequence_to_be_sorted
          off = high_index
          len = 1
          with = current_color ).

        high_index = high_index - 1.
      else.
        middle_index = middle_index + 1.
      endif.
    endwhile.
  endmethod.


  method sorting_problem~is_sorted.
    sorted = abap_true.

    do strlen( sequence_to_check ) - 1 times.
      data(current_character_index) = sy-index - 1.
      data(current_color) = sequence_to_check+current_character_index(1).
      data(next_color) = sequence_to_check+sy-index(1).

      sorted = cond abap_bool(
        when ( current_color eq dutch_flag_colors-red and
               ( next_color eq current_color or
                 next_color eq dutch_flag_colors-white or
                 next_color eq dutch_flag_colors-blue ) )
             or
             ( current_color eq dutch_flag_colors-white and
               ( next_color eq current_color or
                 next_color eq dutch_flag_colors-blue ) )
             or
             ( current_color eq dutch_flag_colors-blue and
               current_color eq next_color )
        then sorted
        else abap_false ).

      check sorted eq abap_false.
      return.
    enddo.
  endmethod.
endclass.


start-of-selection.
  data dutch_national_flag_problem type ref to sorting_problem.

  dutch_national_flag_problem = new dutch_national_flag_problem( ).

  data(sequence) = dutch_national_flag_problem->generate_unsorted_sequence( 20 ).

  write:|{ sequence }, is sorted? -> { dutch_national_flag_problem->is_sorted( sequence ) }|, /.

  dutch_national_flag_problem->sort_sequence( changing sequence_to_be_sorted = sequence ).

  write:|{ sequence }, is sorted? -> { dutch_national_flag_problem->is_sorted( sequence ) }|, /.

```


{{output}}

```txt

RBWRWWRBWWRWBBRBRRWR, is sorted? ->

RRRRRRRRWWWWWWWBBBBB, is sorted? -> X

```



## Ada



```Ada
with Ada.Text_IO, Ada.Numerics.Discrete_Random, Ada.Command_Line;

procedure Dutch_National_Flag is

   type Colour_Type is (Red, White, Blue);

   Number: Positive range 2 .. Positive'Last :=
     Positive'Value(Ada.Command_Line.Argument(1));
   -- no sorting if the Number of balls is less than 2

   type Balls is array(1 .. Number) of Colour_Type;

   function Is_Sorted(B: Balls) return Boolean is
      -- checks if balls are in order
   begin
      for I in Balls'First .. Balls'Last-1 loop
         if B(I) > B(I+1) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Sorted;

   function Random_Balls return Balls is
      -- generates an array of random balls, ensuring they are not in order
      package Random_Colour is new Ada.Numerics.Discrete_Random(Colour_Type);
      Gen: Random_Colour.Generator;
      B: Balls;
   begin
      Random_Colour.Reset(Gen);
      loop
         for I in Balls'Range loop
            B(I) := Random_Colour.Random(Gen);
         end loop;
         exit when (not Is_Sorted(B));
         -- ... ensuring they are not in order
      end loop;
      return B;
   end Random_Balls;

   procedure Print(Message: String; B: Balls) is
   begin
      Ada.Text_IO.Put(Message);
      for I in B'Range loop
         Ada.Text_IO.Put(Colour_Type'Image(B(I)));
         if I < B'Last then
            Ada.Text_IO.Put(", ");
         else
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end Print;

   procedure Sort(Bls: in out Balls) is
      -- sort Bls in O(1) time

      Cnt: array(Colour_Type) of Natural := (Red => 0, White => 0, Blue => 0);
      Col: Colour_Type;

      procedure Move_Colour_To_Top(Bls: in out Balls;
                                   Colour: Colour_Type;
                                   Start: Positive;
                                   Count: Natural) is
         This: Positive := Start;
         Tmp: Colour_Type;
      begin
         for N in Start .. Start+Count-1 loop
            while Bls(This) /= Colour loop
               This := This + 1;
            end loop; -- This is the first index >= N with B(This) = Colour
            Tmp := Bls(N); Bls(N) := Bls(This); Bls(This) := Tmp; -- swap
            This := This + 1;
         end loop;
      end  Move_Colour_To_Top;

   begin
      for Ball in Balls'Range loop
         -- count how often each colour is found
         Col := Bls(Ball);
         Cnt(Col) := Cnt(Col) + 1;
      end loop;
      Move_Colour_To_Top(Bls, Red,   Start => 1,          Count => Cnt(Red));
      Move_Colour_To_Top(Bls, White, Start => 1+Cnt(Red), Count => Cnt(White));
      -- all the remaining balls are blue
   end Sort;

   A: Balls := Random_Balls;

begin
   Print("Original Order: ", A);

   pragma Assert(not Is_Sorted(A));   -- Check if A is unsorted

   Sort(A); -- A = ((Red**Cnt(Red)= & (White**Cnt(White)) & (Blue**Cnt(Blue)))

   pragma Assert(Is_Sorted(A));   -- Check if A is actually sorted

   Print("After Sorting:  ", A);
end Dutch_National_Flag;
```


{{out}}


```txt
>./dutch_national_flag 5
Original Order: RED, RED, BLUE, RED, BLUE
After Sorting:  RED, RED, RED, BLUE, BLUE
>./dutch_national_flag 5
Original Order: BLUE, RED, RED, WHITE, RED
After Sorting:  RED, RED, RED, WHITE, BLUE
>./dutch_national_flag 7
Original Order: WHITE, WHITE, BLUE, WHITE, BLUE, BLUE, WHITE
After Sorting:  WHITE, WHITE, WHITE, WHITE, BLUE, BLUE, BLUE
```



## AutoHotkey


```AutoHotKey
RandGen(MaxBalls){
Random,k,3,MaxBalls
Loop,% k{
	Random,k,1,3
	o.=k
}return o
}
While((!InStr(o,1)||!InStr(o,2)||!InStr(o,3))||!RegExReplace(o,"\b1+2+3+\b"))
	o:=RandGen(3)
Loop,% StrLen(o)
	F.=SubStr(o,A_Index,1) ","
F:=RTrim(F,",")
Sort,F,N D`,
MsgBox,% F:=RegExReplace(RegExReplace(RegExReplace(F,"(1)","Red"),"(2)","White"),"(3)","Blue")
```



## AutoIt

Given each color a value in descending order ( Red = 1, White = 2 And Blue = 3)

```Autoit

#include <Array.au3>
Dutch_Flag(50)
Func Dutch_Flag($arrayitems)
	Local $avArray[$arrayitems]
	For $i = 0 To UBound($avArray) - 1
		$avArray[$i] = Random(1, 3, 1)
	Next
	Local $low = 2, $high = 3, $i = 0
	Local $arraypos = -1
	Local $p = UBound($avArray) - 1
	While $i < $p
			if $avArray[$i] < $low Then
				$arraypos += 1
				_ArraySwap($avArray[$i], $avArray[$arraypos])
				$i += 1
			ElseIf $avArray[$i] >= $high Then
				_ArraySwap($avArray[$i], $avArray[$p])
				$p -= 1
			Else
				$i += 1
			EndIf
		WEnd
		_ArrayDisplay($avArray)
EndFunc   ;==>Dutch_Flag

```



## AWK

{{works with|gawk}}

```awk

BEGIN {
    weight[1] = "red"; weight[2] = "white"; weight[3] = "blue";
    # ballnr must be >= 3. Using very high numbers here may make your computer
    # run out of RAM. (10 millions balls ~= 2.5GiB RAM on x86_64)
    ballnr = 10

    srand()
    # Generating a random pool of balls. This python-like loop is actually
    # a prettyfied one-liner
    do
        for (i = 1; i <= ballnr; i++)
            do
                balls[i] = int(3 * rand() + 1)
            # These conditions ensure the 3 first balls contains
            # a white, blue and red ball. Removing 'i < 4' would
            # hit performance a lot.
            while ( (i < 4 && i > 1 && balls[i] == balls[i - 1]) ||
                    (i < 4 && i > 2 && balls[i] == balls[i - 2]) )
    while (is_dnf(balls, ballnr))

    printf("BEFORE: ")
    print_balls(balls, ballnr, weight)

    # Using gawk default quicksort. Using variants of PROCINFO["sorted_in"]
    # wasn't faster than a simple call to asort().
    asort(balls)

    printf("\n\nAFTER : ")
    print_balls(balls, ballnr, weight)

    sorting = is_dnf(balls, ballnr) ? "valid" : "invalid"
    print("\n\nSorting is " sorting ".")
}

function print_balls(balls, ballnr, weight      ,i) {
    for (i = 1; i <= ballnr; i++)
        printf("%-7s", weight[balls[i]])
}

function is_dnf(balls, ballnr) {
    # Checking if the balls are sorted in the Dutch national flag order,
    # using a simple scan with weight comparison
    for (i = 2; i <= ballnr; i++)
        if (balls[i - 1] > balls[i])
            return 0
    return 1
}

```


Output:

<lang>
BEFORE: blue   red    white  red    white  blue   red    white  blue   white

AFTER : red    red    red    white  white  white  white  blue   blue   blue

Sorting is valid.

```



## BaCon


```qbasic
DECLARE color$[] = { "red", "white", "blue" }

DOTIMES 16
    ball$ = APPEND$(ball$, 0, color$[RANDOM(3)] )
DONE

PRINT "Unsorted: ", ball$

PRINT "  Sorted: ", REPLACE$(SORT$(REPLACE$(ball$, "blue", "z")), "z", "blue")
```

{{out}}

```txt

Unsorted: red white blue blue red white white white blue white blue red blue red white red
  Sorted: red red red red red white white white white white white blue blue blue blue blue

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0,0)

      nBalls% = 12
      DIM Balls$(nBalls%-1), Weight%(nBalls%-1), DutchFlag$(2)
      DutchFlag$() = "Red ", "White ", "Blue "

      REM. Generate random list of balls, ensuring not sorted:
      REPEAT
        prev% = 0 : sorted% = TRUE
        FOR ball% = 0 TO nBalls%-1
          index% = RND(3) - 1
          Balls$(ball%) = DutchFlag$(index%)
          IF index% < prev% THEN sorted% = FALSE
          prev% = index%
        NEXT
      UNTIL NOT sorted%
      PRINT "Random list: " SUM(Balls$())

      REM. Assign Dutch Flag weightings to ball colours:
      DutchFlag$ = SUM(DutchFlag$())
      FOR ball% = 0 TO nBalls%-1
        Weight%(ball%) = INSTR(DutchFlag$, Balls$(ball%))
      NEXT

      REM. Sort into Dutch Flag colour sequence:
      C% = nBalls%
      CALL Sort%, Weight%(0), Balls$(0)
      PRINT "Sorted list: " SUM(Balls$())

      REM Final check:
      prev% = 0 : sorted% = TRUE
      FOR ball% = 0 TO nBalls%-1
        weight% = INSTR(DutchFlag$, Balls$(ball%))
        IF weight% < prev% THEN sorted% = FALSE
        prev% = weight%
      NEXT
      IF NOT sorted% PRINT "Error: Balls are not in correct order!"
```

{{out}}

```txt

Random list: Red White Red Blue White Red White Blue Red Red Blue Red
Sorted list: Red Red Red Red Red Red White White White Blue Blue Blue

```



## C


```c
#include <stdio.h> //printf()
#include <stdlib.h> //srand(), rand(), RAND_MAX, qsort()
#include <stdbool.h> //true, false
#include <time.h> //time()

#define NUMBALLS 5 //NUMBALLS>1

int compar(const void *a, const void *b){
	char c1=*(const char*)a, c2=*(const char*)b; //first cast void* to char*, then dereference
	return c1-c2;
}

_Bool issorted(char *balls){
	int i,state;
	state=0;
	for(i=0;i<NUMBALLS;i++){
		if(balls[i]<state)return false;
		if(balls[i]>state)state=balls[i];
	}
	return true;
}

void printout(char *balls){
	int i;
	char str[NUMBALLS+1];
	for(i=0;i<NUMBALLS;i++)str[i]=balls[i]==0?'r':balls[i]==1?'w':'b';
	printf("%s\n",str);
}

int main(void) {
	char balls[NUMBALLS]; //0=r, 1=w, 2=b
	int i;
	srand(time(NULL)); //not a good seed but good enough for the example
	rand(); //rand() always starts with the same values for certain seeds, making
	        //  testing pretty irritating
	// Generate balls
	for(i=0;i<NUMBALLS;i++)balls[i]=(double)rand()/RAND_MAX*3;
	while(issorted(balls)){ //enforce that we start with non-sorted balls
		printf("Accidentally still sorted: ");
		printout(balls);
		for(i=0;i<NUMBALLS;i++)balls[i]=(double)rand()/RAND_MAX*3;
	}
	printf("Non-sorted: ");
	printout(balls);
	qsort(balls,NUMBALLS,sizeof(char),compar); //sort them using quicksort (stdlib)
	if(issorted(balls)){ //unnecessary check but task enforces it
		printf("Sorted: ");
		printout(balls);
	} else {
		printf("Sort failed: ");
		printout(balls);
	}
	return 0;
}
```

{{out}}

```txt
Accidentally still sorted:rrrww
Non-sorted: rbwww
Sorted: rwwwb
```

=={{header|C_sharp|C#}}==

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RosettaCode
{
    class Program
    {
        static void QuickSort(IComparable[] elements, int left, int right)
        {
            int i = left, j = right;
            IComparable pivot = elements[left + (right - left) / 2];

            while (i <= j)
            {
                while (elements[i].CompareTo(pivot) < 0) i++;
                while (elements[j].CompareTo(pivot) > 0) j--;

                if (i <= j)
                {
                    // Swap
                    IComparable tmp = elements[i];
                    elements[i] = elements[j];
                    elements[j] = tmp;
                    i++;
                    j--;
                }
            }

            // Recursive calls
            if (left < j) QuickSort(elements, left, j);
            if (i < right) QuickSort(elements, i, right);
        }
        const int NUMBALLS = 5;
        static void Main(string[] args)
        {
            Func<string[], bool> IsSorted = (ballList) =>
                {
                    int state = 0;
                    for (int i = 0; i < NUMBALLS; i++)
                    {
                        if (int.Parse(ballList[i]) < state)
                            return false;
                        if (int.Parse(ballList[i]) > state)
                            state = int.Parse(ballList[i]);
                    }
                    return true;
                };
            Func<string[], string> PrintOut = (ballList2) =>
                {
                    StringBuilder str = new StringBuilder();
                    for (int i = 0; i < NUMBALLS; i++)
                        str.Append(int.Parse(ballList2[i]) == 0 ? "r" : int.Parse(ballList2[i]) == 1 ? "w" : "b");
                    return str.ToString();
                };
            bool continueLoop = true;
            string[] balls = new string[NUMBALLS]; // 0 = r, 1 = w, 2 = b
            Random numberGenerator = new Random();
            do // Enforce that we start with non-sorted balls
            {
                // Generate balls
                for (int i = 0; i < NUMBALLS; i++)
                    balls[i] = numberGenerator.Next(3).ToString();

                continueLoop = IsSorted(balls);
                if (continueLoop)
                    Console.WriteLine("Accidentally still sorted: {0}", PrintOut(balls));
            } while (continueLoop);
            Console.WriteLine("Non-sorted: {0}", PrintOut(balls));
            QuickSort(balls, 0, NUMBALLS - 1); // Sort them using quicksort
            Console.WriteLine("{0}: {1}", IsSorted(balls) ? "Sorted" : "Sort failed", PrintOut(balls));
        }
    }
}

```



## C++


```cpp
#include <algorithm>
#include <iostream>

// Dutch national flag problem
template <typename BidIt, typename T>
void dnf_partition(BidIt first, BidIt last, const T& low, const T& high)
{
    for (BidIt next = first; next != last; ) {
        if (*next < low) {
            std::iter_swap(first++, next++);
        } else if (!(*next < high)) {
            std::iter_swap(next, --last);
        } else {
            ++next;
        }
    }
}

enum Colors { RED, WHITE, BLUE };

void print(const Colors *balls, size_t size)
{
    static const char *label[] = { "red", "white", "blue" };

    std::cout << "Balls:";
    for (size_t i = 0; i < size; ++i) {
        std::cout << ' ' << label[balls[i]];
    }
    std::cout << "\nSorted: " << std::boolalpha << std::is_sorted(balls, balls + size) << '\n';
}

int main()
{
    Colors balls[] = { RED, WHITE, BLUE, RED, WHITE, BLUE, RED, WHITE, BLUE };

    std::random_shuffle(balls, balls + 9);
    print(balls, 9);

    dnf_partition(balls, balls + 9, WHITE, BLUE);
    print(balls, 9);
}
```

{{out}}

```txt

Balls: blue white red blue red blue white red white
Sorted: false
Balls: red red red white white white blue blue blue
Sorted: true

```



## Ceylon

Be sure to add ceylon.random in your module.ceylon file.

```ceylon
import ceylon.random {

	DefaultRandom
}

abstract class Colour(name, ordinal) of red | white | blue  satisfies Comparable<Colour> {
	shared String name;
	shared Integer ordinal;
	string => name;
	compare(Colour other) => this.ordinal <=> other.ordinal;
}

object red extends Colour("red", 0) {}
object white extends Colour("white", 1) {}
object blue extends Colour("blue", 2) {}

Colour[] allColours = `Colour`.caseValues;

shared void run() {

	function ordered({Colour*} colours) =>
			colours.paired.every(([c1, c2]) => c1 <= c2);

	value random = DefaultRandom();

	function randomBalls(Integer length = 15) {
		while (true) {
			value balls = random.elements(allColours).take(length);
			if (!ordered(balls)) {
				return balls.sequence();
			}
		}
	}

	function dutchSort({Colour*} balls, Colour mid = white) {
		value array = Array { *balls };
		if (array.empty) {
			return [];
		}
		variable value i = 0;
		variable value j = 0;
		variable value n = array.size - 1;
		while (j <= n) {
			assert (exists ball = array[j]);
			if (ball < mid) {
				array.swap(i, j);
				i ++;
				j ++;
			}
			else if (ball > mid) {
				array.swap(n, j);
				n --;
			}
			else {
				j ++;
			}
		}
		return array;
	}

	function idiomaticSort({Colour*} balls) =>
			balls.sort(increasing);

    value initialBalls = randomBalls();

    "the initial balls are not randomized"
    assert (!ordered(initialBalls));

    print(initialBalls);

    value sortedBalls1 = idiomaticSort(initialBalls);
    value sortedBalls2 = dutchSort(initialBalls);

    "the idiomatic sort didn't work"
    assert (ordered(sortedBalls1));

    "the dutch sort didn't work"
    assert (ordered(sortedBalls2));

    print(sortedBalls1);
    print(sortedBalls2);
}
```



## D


```d
import std.stdio, std.random, std.algorithm, std.traits, std.array;

enum DutchColors { red, white, blue }

void dutchNationalFlagSort(DutchColors[] items) pure nothrow @nogc {
    int lo, mid, hi = items.length - 1;

    while (mid <= hi)
        final switch (items[mid]) {
            case DutchColors.red:
                swap(items[lo++], items[mid++]);
                break;
            case DutchColors.white:
                mid++;
                break;
            case DutchColors.blue:
                swap(items[mid], items[hi--]);
                break;
        }
}

void main() {
    DutchColors[12] balls;
    foreach (ref ball; balls)
        ball = uniform!DutchColors;

    writeln("Original Ball order:\n", balls);
    balls.dutchNationalFlagSort;
    writeln("\nSorted Ball Order:\n", balls);
    assert(balls[].isSorted, "Balls not sorted.");
}
```

{{out}}

```txt
Original Ball order:
[red, white, white, blue, white, red, red, red, red, red, blue, red]

Sorted Ball Order:
[red, red, red, red, red, red, red, white, white, white, blue, blue]
```



### Bidirectional Range Version


```d
import std.stdio, std.random, std.algorithm, std.range,
       std.array, std.traits;

/*
This implementation has less requirements, it works with just
a Bidirectional Range instead of a Random Access Range.

(Comments modified from "Notes on Programming" by Alexander
 Stepanov.)

  Let us assume that somehow we managed to solve the problem up
  to some middle point s:

  0000001111?????22222222
        ^   ^   ^
        f   s   l         (first, second, last)

  If s points to an item with value 0 (red) we swap it with an
  element pointed at by f and advance both f and s.
  If s refers to an item 1 (white) we just advance s.
  If s refers to an item 2 (blue) we swap elements
  pointed by l and s and we decrement l.

In D/Phobos we use Ranges, that are like pairs of iterators.
So 'secondLast' represents the s and l iterators, and the 'first'
range contains f plus an unused end.

secondLast represents the inclusive range of items not yet seen.
When it's empty, the algorithm has finished.

Loop variant: in each iteration of the for loop the length of
secondLast decreases by 1. So the algorithm terminates.
*/
void dutchNationalFlagSort(Range, T)(Range secondLast,
                                     in T lowVal, in T highVal)
pure nothrow if (isBidirectionalRange!Range &&
                 hasSwappableElements!Range &&
                 is(ElementType!Range == T)) {
    for (auto first = secondLast; !secondLast.empty; )
        if (secondLast.front == lowVal) {
            swap(first.front, secondLast.front);
            first.popFront();
            secondLast.popFront();
        } else if (secondLast.front == highVal) {
            swap(secondLast.front, secondLast.back);
            secondLast.popBack();
        } else
            secondLast.popFront();
}

void main() {
    enum DutchColors { red, white, blue }
    DutchColors[12] balls;
    foreach (ref ball; balls)
        ball = [EnumMembers!DutchColors][uniform(0, $)];

    writeln("Original Ball order:\n", balls);
    balls[].dutchNationalFlagSort(DutchColors.red,
                                  DutchColors.blue);
    writeln("\nSorted Ball Order:\n", balls);
    assert(balls[].isSorted(), "Balls not sorted");

    // More tests:
    foreach (i; 0 .. 100_000) {
        int n = uniform(0, balls.length);
        foreach (ref ball; balls[0 .. n])
            ball = [EnumMembers!DutchColors][uniform(0, $)];
        balls[0 .. n].dutchNationalFlagSort(DutchColors.red,
                                            DutchColors.blue);
        assert(balls[0 .. n].isSorted());
    }
}
```

The output is the same.


### More Verified Version

This version uses more contract programming and asserts to verify the code correctness.
With hints from: toccata.lri.fr/gallery/flag.en.html

```d
import std.stdio, std.random, std.algorithm, std.traits, std.range;

enum Color : ubyte { blue, white, red }

immutable isMonochrome = (in Color[] a, in size_t i, in size_t j, in Color c)
    pure nothrow @safe @nogc => iota(i, j).all!(k => a[k] == c);

bool isPermutation(in Color[] a1, in Color[] a2) pure nothrow @safe @nogc {
    size_t[EnumMembers!Color.length] counts1, counts2;
    foreach (immutable x; a1)
        counts1[x]++;
    foreach (immutable x; a2)
        counts2[x]++;
    return counts1 == counts2;
}


void dutchNationalFlagSort(Color[] a) pure nothrow @safe @nogc
    // This function is not @nogc in -debug builds.
    /*
    Scan of the array 'a' from left to right using 'i' and we
    maintain this invariant, using indices 'b' and 'r':

    0         b          i           r
    +---------+----------+-----------+-------+
    |  blue   |  white   |     ?     |  red  |
    +---------+----------+-----------+-------+
    */
out {
    // Find b and r.
    immutable bRaw = a.countUntil!q{a != b}(Color.blue);
    immutable size_t b = (bRaw == -1) ? a.length : bRaw;
    immutable rRaw = a.retro.countUntil!q{a != b}(Color.red);
    immutable size_t r = (rRaw == -1) ? 0 : (a.length - rRaw);

    assert(isMonochrome(a, 0, b, Color.blue));
    assert(isMonochrome(a, b, r, Color.white));
    assert(isMonochrome(a, r, a.length, Color.red));
    // debug assert(isPermutation(a, a.old));
} body {
    size_t b = 0, i = 0, r = a.length;
    debug {
        /*ghost*/ immutable aInit = a.idup; // For loop invariant.
        /*ghost*/ size_t riPred = r - i;    // For loop variant.
    }

    while (i < r) {
        /*invariant*/ assert(0 <= b && b <= i && i <= r && r <= a.length);
        /*invariant*/ assert(isMonochrome(a, 0, b, Color.blue));
        /*invariant*/ assert(isMonochrome(a, b, i, Color.white));
        /*invariant*/ assert(isMonochrome(a, r, a.length, Color.red));
        /*invariant*/ debug assert(isPermutation(a, aInit));

        final switch (a[i]) with (Color) {
            case blue:
                a[b].swap(a[i]);
                b++;
                i++;
                break;
            case white:
                i++;
                break;
            case red:
                r--;
                a[r].swap(a[i]);
                break;
        }

        debug {
            /*variant*/ assert((r - i) < riPred);
            riPred = r - i;
        }
    }
}

void main() {
    Color[12] balls;

    // Test special cases.
    foreach (immutable color; [EnumMembers!Color]) {
        balls[] = color;
        balls.dutchNationalFlagSort;
        assert(balls[].isSorted, "Balls not sorted.");
    }

    foreach (ref b; balls)
        b = uniform!Color;

    writeln("Original Ball order:\n", balls);
    balls.dutchNationalFlagSort;
    writeln("\nSorted Ball Order:\n", balls);
    assert(balls[].isSorted, "Balls not sorted.");
}
```

The output is the same.


## Elixir

{{trans|Erlang}}

```elixir
defmodule Dutch_national_flag do
  defp ball(:red),   do: 1
  defp ball(:white), do: 2
  defp ball(:blue),  do: 3

  defp random_ball, do: Enum.random([:red, :white, :blue])

  defp random_ball(n), do: (for _ <- 1..n, do: random_ball())

  defp is_dutch([]), do: true
  defp is_dutch([_]), do: true
  defp is_dutch([b,h|l]), do: ball(b) < ball(h) and is_dutch([h|l])
  defp is_dutch(_), do: false

  def  dutch(list), do: dutch([], [], [], list)

  defp dutch(r, w, b, []),              do: r ++ w ++ b
  defp dutch(r, w, b, [:red   | list]), do: dutch([:red | r],  w,  b, list)
  defp dutch(r, w, b, [:white | list]), do: dutch(r, [:white | w], b, list)
  defp dutch(r, w, b, [:blue  | list]), do: dutch(r, w,  [:blue | b], list)

  def problem(n \\ 10) do
    list = random_ball(n)
    if is_dutch(list) do
      IO.puts "The random sequence #{inspect list} is already in the order of the Dutch flag!"
    else
      IO.puts "The starting random sequence is #{inspect list};"
      IO.puts "The ordered sequence is #{inspect dutch(list)}."
    end
  end
end

Dutch_national_flag.problem
```


{{out}}

```txt

The starting random sequence is [:blue, :white, :blue, :red, :red, :white, :blue
, :white, :white, :blue];
The ordered sequence is [:red, :red, :white, :white, :white, :white, :blue, :blu
e, :blue, :blue].

```



## Erlang


```erlang
-module(dutch).
-export([random_balls/1, is_dutch/1, dutch/1]).

ball(red)   -> 1;
ball(white) -> 2;
ball(blue)  -> 3.

random_ball() -> lists:nth(random:uniform(3), [red, white, blue]).

random_balls(N)   -> random_balls(N,[]).
random_balls(0,L) -> L;
random_balls(N,L) when N > 0 ->
  B = random_ball(),
  random_balls(N-1, [B|L]).

is_dutch([])        -> true;
is_dutch([_])       -> true;
is_dutch([B|[H|L]]) -> (ball(B) < ball(H)) and is_dutch([H|L]);
is_dutch(_)         -> false.

dutch(L) -> dutch([],[],[],L).

dutch(R, W, B, [])          -> R ++ W ++ B;
dutch(R, W, B, [red   | L]) -> dutch([red|R],  W,  B,  L);
dutch(R, W, B, [white | L]) -> dutch(R, [white|W], B,  L);
dutch(R, W, B, [blue  | L]) -> dutch(R, W,   [blue|B], L).
```


Sample usage:

```erlang
main(_) ->
   L = random_balls(10),
   case is_dutch(L) of
     true  -> io:format("The random sequence ~p is already in the order of the Dutch flag!~n", [L]);
     false -> io:format("The starting random sequence is ~p;~nThe ordered sequence is ~p.~n", [L, dutch(L)])
   end.
```


{{out}}

```txt
The starting random sequence is [white,white,blue,blue,white,red,white,blue,
                                 blue,white];
The ordered sequence is [red,white,white,white,white,white,blue,blue,blue,
                         blue].
```


=={{header|F_Sharp|F#}}==

```fsharp
(* Since the task description here does not impose Dijsktra's original restrictions
    * Changing the order is only allowed by swapping 2 elements
    * Every element must only be inspected once
   we have several options ...
   One way -- especially when we work with immutable data structures --
   is to scan the unordered list, collect the different
   colours on our way and append the 3 sub-lists in the correct order.
*)
let rnd = System.Random()

type color = | Red | White | Blue

let isDutch s =
    Seq.forall2 (fun last this ->
        match (last, this) with
        | (Red, Red) | (Red, White) | (White, White) | (White, Blue) | (Blue, Blue) -> true | _ -> false
    ) s (Seq.skip 1 s)

[<EntryPoint>]
let main argv =
    let n = 10
    let rec getBallsToSort n s =
        let sn = Seq.take n s
        if (isDutch sn) then (getBallsToSort n (Seq.skip 1 s)) else sn
    let balls = getBallsToSort n (Seq.initInfinite (fun _ -> match (rnd.Next(3)) with | 0 -> Red | 1 -> White | _ -> Blue))
    printfn "Sort the sequence of %i balls: %A" n (Seq.toList balls)
    let (rs,ws,bs) =
        balls
        |> Seq.fold (fun (rs,ws,bs) b ->
            match b with | Red -> (b::rs,ws,bs) | White -> (rs,b::ws,bs) | Blue -> (rs,ws,b::bs))
            ([],[],[])
    let sorted = rs @ ws @ bs
    printfn "The sequence %A is sorted: %b" sorted (isDutch sorted)
    0
```

{{out}}

```txt
Sort the sequence of 10 balls: [Red; White; Red; Blue; White; White; Blue; Blue; White; White]
The sequence [Red; Red; White; White; White; White; White; Blue; Blue; Blue] is sorted: true
```



## Forth

This demo is by no means exemplary however there was no other Forth entry.  This code runs on the infamous TI-99, one of the slowest computers ever.  This demo uses Dijkstra's three colour algorithm to sort four different inputs. The flag is sorted on the screen so you can see it happen. The input data patterns are: random, checker-board, Russian flag and French (imperfect) flag.
Using three variables for the screen position pointers (vs stack juggling) makes the Dijkstra algorithm translate nicely into Forth.

A video of the results can be seen here:

https://github.com/bfox9900/CAMEL99-V2/blob/master/Video/DIJKSTRAFLAG%20.mp4

<lang>\ Dutch flag DEMO for CAMEL99 Forth
\ *SORTS IN PLACE FROM Video MEMORY*

 INCLUDE DSK1.GRAFIX.F
 INCLUDE DSK1.RANDOM.F
 INCLUDE DSK1.CASE.F

\ TMS9918 Video chip Specific code
HEX
FFFF FFFF FFFF FFFF PATTERN: SQUARE

\ define colors and characters
DECIMAL
24 32 *  CONSTANT SIZE     \ flag will fill GRAPHICS screen
SIZE 3 / CONSTANT #256     \ 256 chars per segment of flag
1        CONSTANT REDSQR   \ red character
9        CONSTANT WHTSQR   \ white character
19       CONSTANT BLUSQR   \ blue character

\ color constants
1        CONSTANT TRANS
7        CONSTANT RED
5        CONSTANT BLU
16       CONSTANT WHT

SQUARE REDSQR CHARDEF
SQUARE BLUSQR CHARDEF
SQUARE WHTSQR CHARDEF

\ charset  FG    BG
  0        RED TRANS COLOR
  1        WHT TRANS COLOR
  2        BLU TRANS COLOR

\ screen fillers
: RNDI    ( -- n ) SIZE 1+ RND ; \ return a random VDP screen address

: NOTRED    (  -- n ) \ return rnd index that is not RED
           BEGIN
              RNDI DUP VC@ REDSQR =
           WHILE DROP
           REPEAT ;

: NOTREDWHT    ( -- n ) \ return rnd index that is not RED or WHITE
           BEGIN  RNDI DUP
              VC@  DUP REDSQR =
              SWAP WHTSQR = OR
           WHILE
              DROP
           REPEAT ;

: RNDRED  (  -- ) \ Random RED on VDP screen
          #256 0 DO   REDSQR NOTRED VC!   LOOP ;

: RNDWHT (  -- ) \ place white where there is no red or white
          #256 0 DO   WHTSQR NOTREDWHT VC!   LOOP ;

: BLUSCREEN ( -- )
           0 768 BLUSQR VFILL ;

\ load the screen with random red,white&blue squares
: RNDSCREEN ( -- )
            BLUSCREEN  RNDRED  RNDWHT ;

: CHECKERED  ( -- ) \ red,wht,blue checker board
         SIZE 0
         DO
            BLUSQR I VC!
            WHTSQR I 1+ VC!
            REDSQR I 2+ VC!
         3 +LOOP ;

: RUSSIAN  \ Russian flag
            0  0 WHTSQR 256 HCHAR
            0  8 BLUSQR 256 HCHAR
            0 16 REDSQR 256 HCHAR ;

: FRENCH  \ kind of a French flag
           0  0 BLUSQR 256 VCHAR
          10 16 WHTSQR 256 VCHAR
          21  8 REDSQR 256 VCHAR ;

\
### =================================================

\ Algorithm Dijkstra(A)  \ A is an array of three colors
\ begin
\   r <- 1;
\   b <- n;
\   w <- n;
\ while (w>=r)
\       check the color of A[w]
\       case 1: red
\               swap(A[r],A [w]);
\                r<-r+1;
\       case 2: white
\               w<-w-1
\       case 3: blue
\               swap(A[w],A[b]);
\               w<-w-1;
\               b<-b-1;
\ end

\
### ================================================

\ Dijkstra three color Algorithm in Forth

\ screen address pointers
VARIABLE R
VARIABLE B
VARIABLE W

: XCHG  ( vadr1 vadr2 -- ) \ Exchange chars in Video RAM
       OVER VC@ OVER VC@       ( -- addr1 addr2 char1 char2)
       SWAP ROT VC! SWAP VC! ; \ exchange chars in Video RAM

: DIJKSTRA ( -- )
           0 R !
           SIZE 1- DUP  B !  W !
           BEGIN
               W @  R @  1- >
           WHILE
               W @ VC@  ( fetch Video char at pointer W)
               CASE
                 REDSQR OF  R @ W @  XCHG
                            1 R +!           ENDOF

                 WHTSQR OF -1 W +!           ENDOF

                 BLUSQR OF  W @ B @  XCHG
                           -1 W +!
                           -1 B +!           ENDOF
               ENDCASE
           REPEAT ;

: WAIT ( -- )  11 11 AT-XY ." Finished!" 1500 MS ;

: RUN  ( -- )
         PAGE
         CR ." Dijkstra Dutch flag Demo"  CR
         CR ." Sorted in-place in Video RAM" CR
         CR
         CR ." Using the 3 colour algorithm" CR
         CR ." Press any key to begin" KEY DROP
         RNDSCREEN  DIJKSTRA WAIT
         CHECKERED  DIJKSTRA WAIT
         RUSSIAN    DIJKSTRA WAIT
         FRENCH     DIJKSTRA WAIT
         0 23 AT-XY
         CR ." Completed"
;

```



## Fortran

Please find the example run along with compilation instructions on a GNU/linux platform in the comments at the beginning of the FORTRAN 2008 program source.  The Netherlands program, using equal numbers of colors, solved the problem at three sample sizes.  Swaps number 2/3 the total of samples, convincingly demonstrating the O(n) time behavior that's directly provable by inspection.  The color strings are chosen for ASCII sort.  Feature not used.

Abhor code duplication.  I've repeated code anyway to demonstrate FORTRAN pointers, which behave like an alias.  A subroutine with traditional arguments including the number of valid elements of the array is appropriate.  I'd use one long array instead of 3 arrays and the size intrinsic.
<lang>
!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Mon Jun  3 11:18:24
!
!a=./f && make FFLAGS='-O0 -g' $a && OMP_NUM_THREADS=2 $a < unixdict.txt
!gfortran -std=f2008 -O0 -g -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
! Original and flag sequences
! WHITE RED   blue  blue  RED   WHITE WHITE WHITE blue  RED   RED   blue
! RED   RED   RED   RED   WHITE WHITE WHITE WHITE blue  blue  blue  blue
!          12 items,           8  swaps.
!         999 items,         666  swaps.
!        9999 items,        6666  swaps.
!
!Compilation finished at Mon Jun  3 11:18:24

program Netherlands

  character(len=6), parameter, dimension(3) :: colors = (/'RED   ', 'WHITE ', 'blue  '/)
  integer, dimension(12) :: sort_me
  integer, dimension(999), target :: a999
  integer, dimension(9999), target :: a9999
  integer, dimension(:), pointer  :: pi
  integer :: i, swaps
  data sort_me/4*1,4*2,4*3/
  call shuffle(sort_me, 5)
  write(6,*)'Original and flag sequences'
  write(6,*) (colors(sort_me(i)), i = 1, size(sort_me))
  call partition3way(sort_me, 2, swaps)
  write(6,*) (colors(sort_me(i)), i = 1, size(sort_me))
  write(6,*) 12,'items,',swaps,' swaps.'
  pi => a999
  do i=1, size(pi)
    pi(i) = 1 + L(size(pi)/3 .lt. i) + L(2*size(pi)/3 .lt. i)
  end do
  call shuffle(pi, size(pi)/3+1)
  call partition3way(pi, 2, swaps)
  write(6,*) size(pi),'items,',swaps,' swaps.'
  pi => a9999
  do i=1, size(pi)
    pi(i) = 1 + L(size(pi)/3 .lt. i) + L(2*size(pi)/3 .lt. i)
  end do
  call shuffle(pi, size(pi)/3+1)
  call partition3way(pi, 2, swaps)
  write(6,*) size(pi),'items,',swaps,' swaps.'

contains

  integer function L(q)
    ! In Ken Iverson's spirit, APL logicals are more useful as integers.
    logical, intent(in) :: q
    if (q) then
      L = 1
    else
      L = 0
    end if
  end function L

  subroutine swap(a,i,j)
    integer, dimension(:), intent(inout) :: a
    integer, intent(in) :: i, j
    integer :: t
    t = a(i)
    a(i) = a(j)
    a(j) = t
  end subroutine swap

  subroutine partition3way(a, pivot, swaps)
    integer, dimension(:), intent(inout) :: a
    integer, intent(in) :: pivot
    integer, intent(out) :: swaps
    integer :: i, j, k
    swaps = 0
    i = 0
    j = 1
    k = size(a) + 1
    do while (j .lt. k)
      if (pivot .eq. a(j)) then
        j = j+1
        swaps = swaps-1
      else if (pivot .lt. a(j)) then
        k = k-1
        call swap(a, k, j)
      else
        i = i+1
        call swap(a, i, j)
        j = j+1
      end if
      swaps = swaps+1
    end do
  end subroutine partition3way

  subroutine shuffle(a, n) ! a rather specialized shuffle not for general use
    integer, intent(inout), dimension(:) :: a
    integer, intent(in) :: n
    integer :: i, j, k
    real :: harvest
    do i=1, size(a)-1
      call random_number(harvest)
      harvest = harvest - epsilon(harvest)*L(harvest.eq.1)
      k = L(i.eq.1)*(n-1) + i
      j = i + int((size(a) - k) * harvest)
      call swap(a, i, j)
    end do
  end subroutine shuffle

end program Netherlands

```



## FreeBASIC


```freebasic

' El problema planteado por Edsger Dijkstra es:
' "Dado un número de bolas rojas, azules y blancas en orden aleatorio,
' ordénelas en el orden de los colores de la bandera nacional holandesa."

Dim As String c = "RBW", n = "121509"
Dim As Integer bolanum = 9
Dim As Integer d(bolanum), k, i, j
Randomize Timer

Color 15: Print "Aleatorio: ";
For k = 1 To bolanum
    d(k) = Int(Rnd * 3) + 1
    Color Val(Mid(n, d(k), 2))
    Print Mid(c, d(k), 1) & Chr(219);
Next k

Color 15: Print : Print "Ordenado:  ";
For i = 1 To 3
    For j = 1 To bolanum
        If d(j) = i Then Color Val(Mid(n, i, 2)): Print Mid(c, i, 1) & Chr(219);
    Next j
Next i
End

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=e57a862aff12647fa80c84a595161cb9 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim Red As String = "0"
Dim White As String = "1"
Dim Blue As String = "2"
Dim siCount As Short
Dim sColours As New String[]
Dim sTemp As String

For siCount = 1 To 20
  sColours.Add(Rand(Red, Blue))
Next

Print "Random: - ";

For siCount = 1 To 2
  For Each sTemp In sColours
    If sTemp = Red Then Print "Red ";
    If sTemp = White Then Print "White ";
    If sTemp = Blue Then Print "Blue ";
  Next
  sColours.Sort
  Print
  If siCount = 1 Then Print "Sorted: - ";
Next

End
```

Output:

```txt

Random: - Blue Red Red White White White White Red Blue White Red Red White Blue White White Blue Red White Blue
Sorted: - Red Red Red Red Red Red White White White White White White White White White Blue Blue Blue Blue Blue

```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// constants define order of colors in Dutch national flag
const (
    red = iota
    white
    blue
    nColors
)

// zero object of type is valid red ball.
type ball struct {
    color int
}

// order of balls based on DNF
func (b1 ball) lt(b2 ball) bool {
    return b1.color < b2.color
}

// type for arbitrary ordering of balls
type ordering []ball

// predicate tells if balls are ordered by DNF
func (o ordering) ordered() bool {
    var b0 ball
    for _, b := range o {
        if b.lt(b0) {
            return false
        }
        b0 = b
    }
    return true
}

func init() {
    rand.Seed(time.Now().Unix())
}

// constructor returns new ordering of balls which is randomized but
// guaranteed to be not in DNF order.  function panics for n < 2.
func outOfOrder(n int) ordering {
    if n < 2 {
        panic(fmt.Sprintf("%d invalid", n))
    }
    r := make(ordering, n)
    for {
        for i, _ := range r {
            r[i].color = rand.Intn(nColors)
        }
        if !r.ordered() {
            break
        }
    }
    return r
}

// O(n) algorithm
// http://www.csse.monash.edu.au/~lloyd/tildeAlgDS/Sort/Flag/
func (a ordering) sort3() {
    lo, mid, hi := 0, 0, len(a)-1
    for mid <= hi {
        switch a[mid].color {
        case red:
            a[lo], a[mid] = a[mid], a[lo]
            lo++
            mid++
        case white:
            mid++
        default:
            a[mid], a[hi] = a[hi], a[mid]
            hi--
        }
    }
}

func main() {
    f := outOfOrder(12)
    fmt.Println(f)
    f.sort3()
    fmt.Println(f)
}
```

{{out}}

```txt

[{1} {0} {0} {2} {1} {1} {1} {2} {2} {0} {1} {2}]
[{0} {0} {0} {1} {1} {1} {1} {1} {2} {2} {2} {2}]

```



## Haskell

With the Color data type we take care that no other values than Red, White and Blue can be used.
The "deriving" clause is a key aspect: We want Haskell to make Color automatically an instance of the classes Show, Eq, Ord and Enum.
- Show means that Haskell can convert the data constructors Red, White and Blue to text.
- Eq means that two values of type Color can be compared for equality, as if they were numbers or characters.
- Ord means that one can sort a list of values of type Color according to the order in which the constructors Red, White and Blue were declared. We don't need to check if the order of the colors is right - it just is.
- Enum menas that Red, White and Blue are automatically enumerated: every constructor is assigned to an integer.

The function "sort" works with anything that belongs to the Eq and Ord classes.
The function "randomRIO" takes a range of two integers to give a random value within the range. We make Color an instance of Enum so that we can give Red, White and Blue as integers to randomRIO and convert the random number back to Red, White or Blue.

```Haskell
import Data.List (sort)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

data Color = Red | White | Blue deriving (Show, Eq, Ord, Enum)

dutch :: [Color] -> [Color]
dutch = sort

isDutch :: [Color] -> Bool
isDutch x = x == dutch x

randomBalls :: Int -> [Color]
randomBalls 0 = []
randomBalls n = toEnum (unsafePerformIO (randomRIO (fromEnum Red,
    fromEnum Blue))) : randomBalls (n - 1)

main :: IO ()
main = do
    let a = randomBalls 20
    case isDutch a of
        True -> putStrLn $ "The random sequence " ++ show a ++
            " is already in the order of the Dutch national flag!"
        False -> do
            putStrLn $ "The starting random sequence is " ++ show a ++ "\n"
            putStrLn $ "The ordered sequence is " ++ show (dutch a)
```

{{out}}

```txt

The starting random sequence is [White,Blue,Blue,Blue,Blue,Blue,Blue,Red,Red,
White,White,Blue,White,White,Red,White,Blue,White,Red,Red]

The ordered sequence is [Red,Red,Red,Red,Red,White,White,White,White,White,
White,White,Blue,Blue,Blue,Blue,Blue,Blue,Blue,Blue]

```


To understand ''why'' Dijsktra was interested in the problem, here's an example showing difficiency of using generic sort:

```haskell
inorder n = and $ zipWith (<=) n (tail n) -- or use Data.List.Ordered

mk012 :: Int -> Int -> [Int]	-- definitely unordered
mk012 n = (++[0]).(2:).map (`mod` 3).take n.frr where
	-- frr = Fast Rubbish Randoms
	frr = tail . iterate (\n -> n * 7 + 13)

dutch1 n = (filter (==0) n)++(filter (==1) n)++(filter (==2) n)

dutch2 n = a++b++c where
	(a,b,c) = foldl f ([],[],[]) n -- scan list once; it *may* help
	f (a,b,c) x = case x of
		0 -> (0:a, b, c)
		1 -> (a, x:b, c)
		2 -> (a, b, x:c)

main = do -- 3 methods, comment/uncomment each for speed comparisons
--	print $ inorder $ sort s	-- O(n log n)
--	print $ inorder $ dutch1 s	-- O(n)
	print $ inorder $ dutch2 s	-- O(n)
	where s = mk012 10000000 42
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both languages.

The problem statement isn't clear on whether the randomized list of balls
has to contain at least one of each color.  The approach below assumes that
you can have no balls of a given color (including no balls at all - though
that makes ensuring they're not properly sorted at the start hard...).  To
force at least one of each color ball, change "?n-1" to "?n" in the 3rd line.


```unicon
procedure main(a)
    n := integer(!a) | 20
    every (nr|nw|nb) := ?n-1
    sIn := repl("r",nw)||repl("w",nb)||repl("b",nr)
    write(sRand := bestShuffle(sIn))
    write(sOut := map(csort(map(sRand,"rwb","123")),"123","rwb"))
    if sIn ~== sOut then write("Eh? Not in correct order!")
end

procedure bestShuffle(s)    # (Taken from the Best Shuffle task)
    t := s
    every !t :=: ?t    # Uncommented to get a random best shuffling
    every i := 1 to *t do
        every j := (1 to i-1) | (i+1 to *t) do
           if (t[i] ~== s[j]) & (s[i] ~== t[j]) then break t[i] :=: t[j]
    return t
end

procedure csort(w)
    every (s := "") ||:= (find(c := !cset(w),w),c)
    return s
end
```


A few sample runs:


```txt

->dutch
bwwwwwwwwwrrrrrrbbbrrbrwwwrw
rrrrrrrrrrwwwwwwwwwwwwwbbbbb
->dutch
bbbbbbrbbbbbbrwwrwwrwwwwrw
rrrrrwwwwwwwwwbbbbbbbbbbbb
->dutch
bbbbbbbbbwbbwrrrrrrrrrwrrwwrr
rrrrrrrrrrrrrwwwwwbbbbbbbbbbb
->dutch
wbrbrrwwrbrbwrrrrrrwrrrrrrrrr
rrrrrrrrrrrrrrrrrrrrwwwwwbbbb
->

```



## J

We shall define a routine to convert the values 0 1 2 to ball names:

```J
i2b=: {&(;:'red white blue')
```

and its inverse

```J>b2i=: i2b inv</lang

Next, we need a random assortment of balls:

```J
   BALLS=: i2b ?20#3
   BALLS
┌────┬───┬────┬───┬───┬─────┬─────┬─────┬────┬────┬─────┬────┬────┬───┬────┬───┬─────┬───┬────┬───┐
│blue│red│blue│red│red│white│white│white│blue│blue│white│blue│blue│red│blue│red│white│red│blue│red│
└────┴───┴────┴───┴───┴─────┴─────┴─────┴────┴────┴─────┴────┴────┴───┴────┴───┴─────┴───┴────┴───┘
```

And we want to sort them in their canonical order:

```J
      /:~&.b2i BALLS
┌───┬───┬───┬───┬───┬───┬───┬─────┬─────┬─────┬─────┬─────┬────┬────┬────┬────┬────┬────┬────┬────┐
│red│red│red│red│red│red│red│white│white│white│white│white│blue│blue│blue│blue│blue│blue│blue│blue│
└───┴───┴───┴───┴───┴───┴───┴─────┴─────┴─────┴─────┴─────┴────┴────┴────┴────┴────┴────┴────┴────┘
```

Note that if we were not using J's built in sort, we would probably want to use [[Counting_sort|bin sort]] here.

Anyways, we can test that they are indeed sorted properly:

```J
   assert@(-: /:~)&b2i /:~&.b2i BALLS
```



## Java

The elements of an <code>enum</code> implement <code>Comparable</code> so the build-in sort works. You can also use this comparability to check the sort has worked.

```java
import java.util.Arrays;
import java.util.Random;

public class DutchNationalFlag {
    enum DutchColors {
        RED, WHITE, BLUE
    }

    public static void main(String[] args){
        DutchColors[] balls = new DutchColors[12];
        DutchColors[] values = DutchColors.values();
        Random rand = new Random();

        for (int i = 0; i < balls.length; i++)
            balls[i]=values[rand.nextInt(values.length)];
        System.out.println("Before: " + Arrays.toString(balls));

        Arrays.sort(balls);
        System.out.println("After:  " + Arrays.toString(balls));

        boolean sorted = true;
        for (int i = 1; i < balls.length; i++ ){
            if (balls[i-1].compareTo(balls[i]) > 0){
                sorted=false;
                break;
            }
        }
        System.out.println("Correctly sorted: " + sorted);
    }
}
```


{{out}}

```txt
Before: [WHITE, RED, BLUE, RED, WHITE, WHITE, WHITE, RED, WHITE, RED, WHITE, WHITE]
After:  [RED, RED, RED, RED, WHITE, WHITE, WHITE, WHITE, WHITE, WHITE, WHITE, BLUE]
Correctly sorted: true
```



## Javascript


### ES6


```javascript
const dutchNationalFlag = () => {

  /**
   * Return the name of the given number in this way:
   * 0 = Red
   * 1 = White
   * 2 = Blue
   * @param {!number} e
   */
  const name = e => e > 1 ? 'Blue' : e > 0 ? 'White' : 'Red';

  /**
   * Given an array of numbers return true if each number is bigger than
   * or the same as the previous
   * @param {!Array<!number>} arr
   */
  const isSorted = arr => arr.every((e,i) => e >= arr[Math.max(i-1, 0)]);

  /**
   * Generator that keeps yielding a random int between 0(inclusive) and
   * max(exclusive), up till n times, and then is done.
   * @param max
   * @param n
   */
  function* randomGen (max, n) {
    let i = 0;
    while (i < n) {
      i += 1;
      yield Math.floor(Math.random() * max);
    }
  }

  /**
   * An array of random integers between 0 and 3
   * @type {[!number]}
   */
  const mixedBalls = [...(randomGen(3, 22))];

  /**
   * Sort the given array into 3 sub-arrays and then concatenate those.
   */
  const sortedBalls = mixedBalls
    .reduce((p,c) => p[c].push(c) && p, [[],[],[]])
    .reduce((p,c) => p.concat(c), []);

  /**
   * A verbatim implementation of the Wikipedia pseudo-code
   * @param {!Array<!number>} A
   * @param {!number} mid The value of the 'mid' number. In our case 1 as
   * low is 0 and high is 2
   */
  const dutchSort = (A, mid) => {
    let i = 0;
    let j = 0;
    let n = A.length - 1;
    while(j <= n) {
      if (A[j] < mid) {
        [A[i], A[j]] = [A[j], A[i]];
        i += 1;
        j += 1;
      } else if (A[j] > mid) {
        [A[j], A[n]] = [A[n], A[j]];
        n -= 1
      } else {
        j += 1;
      }
    }
  };

  console.log(`Mixed balls       : ${mixedBalls.map(name).join()}`);
  console.log(`Is sorted: ${isSorted(mixedBalls)}`);

  console.log(`Sorted balls      : ${sortedBalls.map(name).join()}`);
  console.log(`Is sorted: ${isSorted(sortedBalls)}`);

  // Only do the dutch sort now as it mutates the mixedBalls array in place.
  dutchSort(mixedBalls, 1);
  console.log(`Dutch Sorted balls: ${mixedBalls.map(name).join()}`);
  console.log(`Is sorted: ${isSorted(mixedBalls)}`);
};
dutchNationalFlag();

```

{{out}}

```txt

Mixed balls       : Red,Red,Blue,Red,White,Red,White,Blue,Blue,White,White,Blue,Red,Blue,Blue,Red,White,Red,Red,Red,White,White
Is sorted: false
Sorted balls      : Red,Red,Red,Red,Red,Red,Red,Red,Red,White,White,White,White,White,White,White,Blue,Blue,Blue,Blue,Blue,Blue
Is sorted: true
Dutch Sorted balls: Red,Red,Red,Red,Red,Red,Red,Red,Red,White,White,White,White,White,White,White,Blue,Blue,Blue,Blue,Blue,Blue
Is sorted: true

```





## Julia

Here the task is solved two ways, with a specialized routine and using the <tt>sort</tt> built-in.  <tt>dutchsort</tt> is a specialized sort based upon the <tt>three-way-partition</tt> pseudocode provided in the Wikipedia article referenced in the task description.  Timing each shows that <tt>dutchsort</tt> is about two orders of magnitude faster than <tt>sort</tt>.  (This relative performance result holds for a variety of color array sizes.)

'''Function'''

```Julia

const COLORS = ["red", "white", "blue"]

function dutchsort!(a::Array{ASCIIString,1}, lo=COLORS[1], hi=COLORS[end])
    i = 1
    j = 1
    n = length(a)
    while j <= n
        if a[j] == lo
            a[i], a[j] = a[j], a[i]
            i += 1
            j += 1
        elseif a[j] == hi
            a[j], a[n] = a[n], a[j]
            n -= 1
        else
            j += 1
        end
    end
    return a
end

function dutchsort(a::Array{ASCIIString,1}, lo=COLORS[1], hi=COLORS[end])
    dutchsort!(copy(a), lo, hi)
end

```


'''Main'''

```Julia

function formatdf(a::Array{ASCIIString,1})
    i = 0
    s = "    "
    for c in a
        s *= @sprintf "%6s" c
        i += 1
        i %= 8
        if i == 0
            s *= "\n    "
        end
    end
    return s
end

cnum = 20
d = [COLORS[rand(1:3)] for i in 1:cnum]
while d == dutchsort(d)
    d = [COLORS[rand(1:3)] for i in 1:cnum]
end

println("The original list is:")
println(formatdf(d))

print("Sorting with dutchsort, ")
@time e = dutchsort(d)
println(formatdf(e))

print("Sorting conventionally, ")
@time e = sort(d, by=x->findfirst(COLORS, x))
println(formatdf(e))

```


{{out}}

```txt

The original list is:
       red  blue   red  blue white  blue white white
      blue white white  blue white white  blue white
     white  blue  blue  blue
Sorting with dutchsort, elapsed time: 0.000520454 seconds (14104 bytes allocated)
       red   red white white white white white white
     white white white  blue  blue  blue  blue  blue
      blue  blue  blue  blue
Sorting conventionally, elapsed time: 0.062974782 seconds (1688896 bytes allocated)
       red   red white white white white white white
     white white white  blue  blue  blue  blue  blue
      blue  blue  blue  blue

```



## Kotlin

{{trans|D}}

```scala
// version 1.1.4

import java.util.Random

enum class DutchColors { RED, WHITE, BLUE }

fun Array<DutchColors>.swap(i: Int, j: Int) {
    val temp = this[i]
    this[i] = this[j]
    this[j] = temp
}

fun Array<DutchColors>.sort() {
    var lo = 0
    var mid = 0
    var hi = this.lastIndex

    while (mid <= hi) {
        when (this[mid]) {
            DutchColors.RED   -> this.swap(lo++, mid++)
            DutchColors.WHITE -> mid++
            DutchColors.BLUE  -> this.swap(mid, hi--)
        }
    }
}

fun Array<DutchColors>.isSorted(): Boolean {
    return (1 until this.size)
        .none { this[it].ordinal < this[it - 1].ordinal }
}

const val NUM_BALLS = 9

fun main(args: Array<String>) {
    val r = Random()
    val balls  = Array(NUM_BALLS) { DutchColors.RED }
    val colors = DutchColors.values()

    // give balls random colors whilst ensuring they're not already sorted
    do {
        for (i in 0 until NUM_BALLS) balls[i] = colors[r.nextInt(3)]
    }
    while (balls.isSorted())

    // print the colors of the balls before sorting
    println("Before sorting : ${balls.contentToString()}")

    // sort the balls in DutchColors order
    balls.sort()

    // print the colors of the balls after sorting
    println("After sorting  : ${balls.contentToString()}")
}
```


Sample output:

```txt

Before sorting : [WHITE, RED, RED, WHITE, BLUE, WHITE, BLUE, RED, RED]
After sorting  : [RED, RED, RED, RED, WHITE, WHITE, WHITE, BLUE, BLUE]

```



## Lasso


```Lasso
define orderdutchflag(a) => {
	local(r = array, w = array, b = array)
	with i in #a do => {
		match(#i) => {
			case('Red')
				#r->insert(#i)
			case('White')
				#w->insert(#i)
			case('Blue')
				#b->insert(#i)
		}
	}
	return #r + #w + #b
}

orderdutchflag(array('Red', 'Red', 'Blue', 'Blue', 'Blue', 'Red', 'Red', 'Red', 'White', 'Blue'))
```

{{out}}

```txt
array(Red, Red, Red, Red, Red, White, Blue, Blue, Blue, Blue)
```



## Logo


```logo
; We'll just use words for the balls
make "colors {red white blue}

; to get a mapping from colors back to a numeric value,
; we make variables out of the color names (e.g. the variable
; "red" has value "1").
foreach arraytolist :colors [
  make ? #
]

; Make a random list of a given size
to random_balls :n
  local "balls
  make "balls array n
  repeat n [
    setitem # :balls pick :colors
  ]
  output :balls
end

; Test for Dutchness
to dutch? :array
   output dutchlist? arraytolist :array
end

; List is easier than array to test
to dutchlist? :list
  output cond [
    [(less? count :list 2) "true]
    [(greater? thing first :list thing item 2 :list) "false ]
    [else dutchlist? butfirst :list]
  ]
end

; But array is better for sorting algorithm
to dutch :array
  local "lo
  make "lo 0
  local "hi
  make "hi sum 1 count :array
  local "i
  make "i 1
  while [:i < :hi] [
    case (item :i :array) [
      [[red]
         make "lo sum :lo 1
         swap :array :lo :i
         make "i sum :i 1
      ]
      [[white]
         make "i sum :i 1
      ]
      [[blue]
         make "hi difference :hi 1
         swap :array :hi :i
      ]
    ]
  ]
  output :array
end

; utility routine to swap array elements
to swap :array :a :b
  local "temp
  make "temp item :a :array
  setitem :a :array item :b :array
  setitem :b :array :temp
end
```


Test code:
<lang>do.while [
  make "list random_balls 10
] [dutch? :list]

print (sentence [Start list:] arraytolist :list)
print (sentence [Sorted:] arraytolist dutch :list)
bye
```


{{out}}

```txt
Start list: white blue red red red white blue red red white
Sorted: red red red red red white white white blue blue
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
flagSort[data_List] := Sort[data, (#1 === RED || #2 === BLUE) &]
```

{{out}}

```txt
flagSort[{WHITE, RED, RED, WHITE, WHITE, BLUE, WHITE, BLUE, BLUE, WHITE, WHITE, BLUE}]

{RED, RED, WHITE, WHITE, WHITE, WHITE, WHITE, WHITE, BLUE, BLUE, BLUE, BLUE}
```



## PARI/GP

A [[counting sort]] might be more appropriate here, but that would conceal the details of the sort.

```parigp
compare(a,b)={
  if (a==b,
    0
  ,
   if(a=="red" || b=="blue", -1, 1)
  )
};
r(n)=vector(n,i,if(random(3),if(random(2),"red","white"),"blue"));
inorder(v)=for(i=2,#v,if(compare(v[i-1],v[i])>0,return(0)));1;

v=r(10);
while(inorder(v), v=r(10));
v=vecsort(v,compare);
inorder(v)
```


{{out}}

```txt
1
```



## Perl

The task is probably not to just sort an array. The wikipedia links has a slightly better explanation that leads to the following code:

```perl
use warnings;
use strict;
use 5.010; # //

use List::Util qw( shuffle );

my @colours = qw( blue white red );

sub are_ordered {
    my $balls = shift;
    my $last = 0;
    for my $ball (@$balls) {
        return if $ball < $last;
        $last = $ball;
    }
    return 1;
}


sub show {
    my $balls = shift;
    print join(' ', map $colours[$_], @$balls), "\n";
}


sub debug {
    return unless $ENV{DEBUG};

    my ($pos, $top, $bottom, $balls) = @_;
    for my $i (0 .. $#$balls) {
        my ($prefix, $suffix) = (q()) x 2;

        ($prefix, $suffix) = qw/( )/ if $i == $pos;
        $prefix           .= '>'     if $i == $top;
        $suffix           .= '<'     if $i == $bottom;

        print STDERR " $prefix$colours[$balls->[$i]]$suffix";
    }
    print STDERR "\n";
}


my $count = shift // 10;
die "$count: Not enough balls\n" if $count < 3;

my $balls = [qw( 2 1 0 )];
push @$balls, int rand 3 until @$balls == $count;
do { @$balls = shuffle @$balls } while are_ordered($balls);

show($balls);

my $top    = 0;
my $bottom = $#$balls;

my $i = 0;
while ($i <= $bottom) {
    debug($i, $top, $bottom, $balls);
    my $col = $colours[ $balls->[$i] ];
    if ('red' eq $col and $i < $bottom) {
        @{$balls}[$bottom, $i] = @{$balls}[$i, $bottom];
        $bottom--;
    } elsif ('blue' eq $col and $i > $top) {
        @{$balls}[$top, $i] = @{$balls}[$i, $top];
        $top++;
    } else {
        $i++;
    }
}
debug($i, $top, $bottom, $balls);

show($balls);
are_ordered($balls) or die "Incorrect\n";
```

You can run it with no parameters, it sorts 10 balls in such a case. If you provide one parameter, it is used as the number of balls. The second parameter turns on debugging that shows how the balls are being swapped.


## Perl 6

Here are five ways to do it, all one liners (apart from the test apparatus).

```perl6>enum NL <red white blue
;
my @colors;

sub how'bout (&this-way) {
    sub show {
        say @colors;
        say "Ordered: ", [<=] @colors;
    }

    @colors = NL.roll(20);
    show;
    this-way;
    show;
    say '';
}

say "Using functional sort";
how'bout { @colors = sort *.value, @colors }

say "Using in-place sort";
how'bout { @colors .= sort: *.value }

say "Using a Bag";
how'bout { @colors = flat red, white, blue Zxx bag(@colors».key)<red white blue> }

say "Using the classify method";
how'bout { @colors = flat (.list for %(@colors.classify: *.value){0,1,2}) }

say "Using multiple greps";
how'bout { @colors = flat (.grep(red), .grep(white), .grep(blue) given @colors) }
```

{{out}}

```txt
Using functional sort
red red white white red red red red red red red white red white red red red white white white
Ordered: False
red red red red red red red red red red red red red white white white white white white white
Ordered: True

Using in-place sort
red blue white red white blue white blue red white blue blue blue red white white red blue red blue
Ordered: False
red red red red red red white white white white white white blue blue blue blue blue blue blue blue
Ordered: True

Using a Bag
red blue blue blue white red white red white blue blue red red red red blue blue red white blue
Ordered: False
red red red red red red red red white white white white blue blue blue blue blue blue blue blue
Ordered: True

Using the classify method
blue red white blue blue white white red blue red red white red blue white white red blue red white
Ordered: False
red red red red red red red white white white white white white white blue blue blue blue blue blue
Ordered: True

Using multiple greps
red white blue white white red blue white red white red white white white white white red red blue red
Ordered: False
red red red red red red red white white white white white white white white white white blue blue blue
Ordered: True
```



## Phix

Minimizes the number of read and swap operations, straight translation of the wikipedia pseudocode:

```Phix
function three_way_partition(sequence s, integer mid)
integer i=1, j=1, n = length(s)

    while j < n do
        if s[j] < mid then
            {s[i],s[j]} = {s[j],s[i]}
            i += 1
            j += 1
        elsif s[j] > mid then
            {s[j],s[n]} = {s[n],s[j]}
            n -= 1
        else
            j += 1
        end if
    end while
    return s
end function

constant colours = {"red","white","blue"}
enum /*red,*/ white = 2, blue, maxc = blue

procedure show(string msg, sequence s)
    for i=1 to length(s) do
        s[i] = colours[s[i]]
    end for
    printf(1,"%s: %s\n",{msg,join(s)})
end procedure

sequence unsorted, sorted
    while 1 do
        unsorted = sq_rand(repeat(maxc,12))
--      sorted = sort(unsorted) -- (works just as well)
        sorted = three_way_partition(unsorted, white)
        if unsorted!=sorted then exit end if
        ?"oops"
    end while
    show("Unsorted",unsorted)
    show("Sorted",sorted)
```

<small>I thought of unsorted=shuffle(unsorted) in the "oops" loop, but of course that'd repeat forever should they all be the same colour.</small>
{{out}}

```txt

Unsorted: blue blue blue blue red white white red white red white blue
Sorted: red red red white white white white blue blue blue blue blue

```



## PicoLisp


```PicoLisp
(def 'Colors
   (list
      (def 'RED 1)
      (def 'WHITE 2)
      (def 'BLUE 3) ) )

(let (L (make (do 9 (link (get Colors (rand 1 3)))))  S (by val sort L))
   (prin "Original balls ")
   (print L)
   (prinl (unless (= L S) " not sorted"))
   (prin "Sorted balls   ")
   (print S)
   (prinl " are sorted") )
```

{{out}}

```txt
Original balls (RED BLUE WHITE BLUE BLUE RED WHITE WHITE WHITE) not sorted
Sorted balls   (RED RED WHITE WHITE WHITE WHITE BLUE BLUE BLUE) are sorted
```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

$Colors = 'red', 'white','blue'

#  Select 10 random colors
$RandomBalls = 1..10 | ForEach { $Colors | Get-Random }

#  Ensure we aren't finished before we start. For some reason. It's in the task requirements.
While ( $RandomBalls -eq $RandomBalls | Sort { $Colors.IndexOf( $_ ) } )
    { $RandomBalls = 1..10 | ForEach { $Colors | Get-Random } }

#  Sort the colors
$SortedBalls = $RandomBalls | Sort { $Colors.IndexOf( $_ ) }

#  Display the results
$RandomBalls
''
$SortedBalls

```

{{out}}

```txt

white
blue
blue
blue
white
red
white
blue
red
red

red
red
red
white
white
white
blue
blue
blue
blue

```



## Prolog

Works with SWI-Prolog 6.1.11

### Prolog spirit


```Prolog
dutch_flag(N) :-
	length(L, N),
	repeat,
	  maplist(init,L),
	\+is_dutch_flag(L) ,
	writeln(L),
	test_sorted(L),
	sort_dutch_flag(L, TmpFlag),
	append(TmpFlag, Flag),
	writeln(Flag),
	test_sorted(Flag).


sort_dutch_flag([], [[], [], []]).

sort_dutch_flag([blue | T], [R, W, [blue|B]]) :-
	sort_dutch_flag(T, [R, W, B]).

sort_dutch_flag([red | T], [[red|R], W, B]) :-
	sort_dutch_flag(T, [R, W, B]).


sort_dutch_flag([white | T], [R, [white | W], B]) :-
	sort_dutch_flag(T, [R, W, B]).


init(C) :-
	R is random(3),
	nth0(R, [blue, red, white], C).


test_sorted(Flag) :-
	(   is_dutch_flag(Flag)
	->  write('it is a dutch flag')
	;   write('it is not a dutch flag')),
	nl,nl.

% First color must be red
is_dutch_flag([red | T]) :-
	is_dutch_flag_red(T).


is_dutch_flag_red([red|T]) :-
	is_dutch_flag_red(T);
	% second color must be white
	T = [white | T1],
	is_dutch_flag_white(T1).


is_dutch_flag_white([white | T]) :-
	is_dutch_flag_white(T);
	% last one must be blue
	T = [blue | T1],
	is_dutch_flag_blue(T1).

is_dutch_flag_blue([blue | T]) :-
	is_dutch_flag_blue(T).

is_dutch_flag_blue([]).

```

{{out}}

```txt
 ?- dutch_flag(20).
[blue,white,white,blue,blue,blue,red,blue,red,blue,blue,blue,white,red,red,blue,blue,red,blue,red]
it is not a dutch flag

[red,red,red,red,red,red,white,white,white,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue,blue]
it is a dutch flag
true .

```



### Functional spirit

Use of filters.

```Prolog
dutch_flag(N) :-
	length(L, N),

	% create the list to sort
	repeat,
	  maplist(init,L),
	\+is_dutch_flag(L) ,
	writeln(L),
	test_sorted(L),

	foldl(\X^Y^Z^(Y = [Red, White, Blue],
		      (	  X = blue
		      ->  append_dl(Blue, [X|U]-U, Blue1),
			  Z = [Red, White, Blue1]
		      ;	  X = red
		      ->  append_dl(Red, [X|U]-U, Red1),
			  Z = [Red1, White, Blue]
		      ;	  append_dl(White, [X|U]-U, White1),
			  Z = [Red, White1, Blue])),
	      L, [R-R, W-W, B-B], [R1, W1, B1]),
	append_dl(R1, W1, B1, Flag-[]),
	write(Flag), nl,
	test_sorted(Flag).

% append lists in O(1)
append_dl(A-B, B-C, A-C).
append_dl(A-B, B-C, C-D, A-D).


init(C) :-
	R is random(3),
	nth0(R, [blue, red, white], C).


test_sorted(Flag) :-
	(   is_dutch_flag(Flag)
	->  write('it is a dutch flag')
	;   write('it is not a dutch flag')),
	nl,nl.

% First color must be red
is_dutch_flag([red | T]) :-
	is_dutch_flag_red(T).


is_dutch_flag_red([red|T]) :-
	is_dutch_flag_red(T);
	% second color must be white
	T = [white | T1],
	is_dutch_flag_white(T1).


is_dutch_flag_white([white | T]) :-
	is_dutch_flag_white(T);
	% last one must be blue
	T = [blue | T1],
	is_dutch_flag_blue(T1).

is_dutch_flag_blue([blue | T]) :-
	is_dutch_flag_blue(T).

is_dutch_flag_blue([]).

```



## Python


### Python: Sorted

The heart of the idiomatic Dutch sort in python is the call to function <code>sorted</code> in function <code>dutch_flag_sort</code>.

```python
import random

colours_in_order = 'Red White Blue'.split()

def dutch_flag_sort(items, order=colours_in_order):
    'return sort of items using the given order'
    reverse_index = dict((x,i) for i,x in enumerate(order))
    return sorted(items, key=lambda x: reverse_index[x])

def dutch_flag_check(items, order=colours_in_order):
    'Return True if each item of items is in the given order'
    reverse_index = dict((x,i) for i,x in enumerate(order))
    order_of_items = [reverse_index[item] for item in items]
    return all(x <= y for x, y in zip(order_of_items, order_of_items[1:]))

def random_balls(mx=5):
    'Select from 1 to mx balls of each colour, randomly'
    balls = sum([[colour] * random.randint(1, mx)
                 for colour in colours_in_order], [])
    random.shuffle(balls)
    return balls

def main():
    # Ensure we start unsorted
    while True:
        balls = random_balls()
        if not dutch_flag_check(balls):
            break
    print("Original Ball order:", balls)
    sorted_balls = dutch_flag_sort(balls)
    print("Sorted Ball Order:", sorted_balls)
    assert dutch_flag_check(sorted_balls), 'Whoops. Not sorted!'

if __name__ == '__main__':
    main()
```

{{out|Sample output}}

```txt
Original Ball order: ['Red', 'Red', 'Blue', 'Blue', 'Blue', 'Red', 'Red', 'Red', 'White', 'Blue']
Sorted Ball Order: ['Red', 'Red', 'Red', 'Red', 'Red', 'White', 'Blue', 'Blue', 'Blue', 'Blue']
```



### Python: sum of filters

This follows the [[wp:Dutch_national_flag_problem#Critic|critics section]]
of the wikipedia article by using a sum of filters.

Replace the function/function call dutch_flag_sort above, with dutch_flag_sort2 defined as:

```python
from itertools import chain
def dutch_flag_sort2(items, order=colours_in_order):
    'return summed filter of items using the given order'
    return list(chain.from_iterable(filter(lambda c: c==colour, items)
                                    for colour in order))
```


Or equivalently using a list comprehension (though perhaps less clear):

```python
def dutch_flag_sort2(items, order=colours_in_order):
    'return summed filter of items using the given order'
    return [c for colour in order for c in items if c==colour]
```

Output follows that of the sorting solution above.


### Python: Construct from ball counts

This reconstructs the correct output by counting how many of each colour there are.

Replace the function/function call dutch_flag_sort above, with dutch_flag_sort3 defined as:

```python
def dutch_flag_sort3(items, order=colours_in_order):
    'counts each colour to construct flag'
    return sum([[colour] * items.count(colour) for colour in order], [])
```

Output follows that of the sorting solution above.

===Python: Explicit in-place sort===

```python
import random

colours_in_order = 'Red White Blue'.split()

def dutch_flag_sort(items):
    '''\
    In-place sort of list items using the given order.
    Python idiom is to return None when argument is modified in-place

    O(n)? Algorithm from Go language implementation of
    http://www.csse.monash.edu.au/~lloyd/tildeAlgDS/Sort/Flag/'''

    lo, mid, hi = 0, 0, len(items)-1
    while mid <= hi:
        colour = items[mid]
        if colour == 'Red':
            items[lo], items[mid] = items[mid], items[lo]
            lo += 1
            mid += 1
        elif colour == 'White':
            mid += 1
        else:
            items[mid], items[hi] = items[hi], items[mid]
            hi -= 1

def dutch_flag_check(items, order=colours_in_order):
    'Return True if each item of items is in the given order'
    order_of_items = [order.index(item) for item in items]
    return all(x <= y for x, y in zip(order_of_items, order_of_items[1:]))

def random_balls(mx=5):
    'Select from 1 to mx balls of each colour, randomly'
    balls = sum(([[colour] * random.randint(1, mx)
                 for colour in colours_in_order]), [])
    random.shuffle(balls)
    return balls

def main():
    # Ensure we start unsorted
    while 1:
        balls = random_balls()
        if not dutch_flag_check(balls):
            break
    print("Original Ball order:", balls)
    dutch_flag_sort(balls)
    print("Sorted Ball Order:", balls)
    assert dutch_flag_check(balls), 'Whoops. Not sorted!'

if __name__ == '__main__':
    main()
```

Output follows that of the sorting solution above.


## Racket



```Racket

#lang racket

(define dutch-colors '(red white blue))

(define (dutch-order? balls)
  ;; drop each color from the front, should end up empty
  (null? (for/fold ([r balls]) ([color dutch-colors])
           (dropf r (curry eq? color)))))

(define (random-balls)
  (define balls
    (for/list ([i (random 20)])
      (list-ref dutch-colors (random (length dutch-colors)))))
  (if (dutch-order? balls) (random-balls) balls))

;; first method: use a key to map colors to integers
(define (order->key order)
  (let ([alist (for/list ([x order] [i (in-naturals)]) (cons x i))])
    (λ(b) (cdr (assq b alist)))))
(define (sort-balls/key balls)
  (sort balls < #:key (order->key dutch-colors)))

;; second method: use a comparator built from the ordered list
(define ((order<? ord) x y)
  (memq y (cdr (memq x ord))))
(define (sort-balls/compare balls)
  (sort balls (order<? dutch-colors)))

(define (test sort)
  (define balls (random-balls))
  (define sorted (sort balls))
  (printf "Testing ~a:\n  Random: ~s\n  Sorted: ~s\n      ==> ~s\n"
          (object-name sort)
          balls sorted (if (dutch-order? sorted) 'OK 'BAD)))
(for-each test (list sort-balls/key sort-balls/compare))

```


{{out}}

```txt

Testing sort-balls/order:
  Random: (red blue blue white red blue red red blue blue red red white blue)
  Sorted: (red red red red red red white white blue blue blue blue blue blue)
      ==> OK
Testing sort-balls/compare:
  Random: (red blue white blue white white white blue red blue blue blue white)
  Sorted: (red red white white white white white blue blue blue blue blue blue)
      ==> OK

```



## REXX

===colors (as words)===
This version uses a version of a bin sort with counts, and has been generalized to allow any number of colors.

The REXX solution could've been simplified somewhat by the use of the   '''countstr'''   BIF   (but some older REXX interpreters don't have).


```rexx
/*REXX program reorders a set of random colored balls into a correct order, which is the*/
/*────────────────────────────────── order of colors on the Dutch flag:  red white blue.*/
parse arg N colors                               /*obtain optional arguments from the CL*/
if N='' |  N=","  then N=15                      /*Not specified?  Then use the default.*/
if colors=''  then colors= 'red white blue'      /* "      "         "   "   "      "   */
#=words(colors)                                  /*count the number of colors specified.*/
@=word(colors, #)    word(colors, 1)             /*ensure balls aren't already in order.*/

    do g=3  to N                                 /*generate a random # of colored balls.*/
    @=@  word( colors, random(1, #) )            /*append a random color to the  @ list.*/
    end   /*g*/

say 'number of colored balls generated = '   N       ;  say
say center(' original ball order ', length(@), "─")
say @                                                ;  say
$=;                          do j=1  for #;
                             _=word(colors, j);      $=$  copies(_' ',   countWords(_, @))
                             end   /*j*/
say
say center(' sorted  ball order ', length(@), "─")
say space($)
say
    do k=2  to  N                                /*verify the balls are in correct order*/
    if wordpos(word($,k), colors) >= wordpos(word($,k-1), colors)  then iterate
    say "The list of sorted balls isn't in proper order!";         exit 13
    end   /*k*/
say
say 'The sorted colored ball list has been confirmed as being sorted correctly.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
countWords:  procedure;   parse arg ?,hay;  s=1
                  do r=0  until _==0;  _=wordpos(?, hay, s);  s=_+1;  end /*r*/;  return r
```

'''output'''   when using the default input:

```txt

number of colored balls generated =  15

────────────────────────── original ball order ───────────────────────────
blue red white blue white white red blue blue blue red blue red blue white


─────────────────────────── sorted  ball order ───────────────────────────
red red red red white white white white blue blue blue blue blue blue blue


The sorted colored ball list has been confirmed as being sorted correctly.

```


===colors (as letters)===

```rexx
/*REXX program reorders a set of random colored balls into a correct order, which is the*/
/*────────────────────────────────── order of colors on the Dutch flag:  red white blue.*/
parse arg N colors                               /*obtain optional arguments from the CL*/
if N='' |  N=","  then N=15                      /*Not specified?  Then use the default.*/
if colors=''      then colors= "RWB"             /*use default:  R=red, W=white, B=blue */
#=length(colors)                                 /*count the number of colors specified.*/
@=right(colors, 1)left(colors, 1)                /*ensure balls aren't already in order.*/

    do g=3  to N                                 /*generate a random # of colored balls.*/
    @=@ ||substr( colors, random(1, #), 1)       /*append a color (1char) to the @ list.*/
    end   /*g*/

say 'number of colored balls generated = '    N      ;    say
say center(' original ball order ', max(30,2*#), "─")
say @                                                ;    say
$=;                          do j=1  for #;             _=substr(colors, j, 1)
                             #=length(@) - length( space( translate(@, , _),   0) )
                             $=$ || copies(_, #)
                             end   /*j*/
say center(' sorted  ball order ', max(30, 2*#), "─")
say $
say
    do k=2  to N                                 /*verify the balls are in correct order*/
    if pos(substr($,k,1), colors) >= pos(substr($,k-1,1), colors)  then iterate
    say "The list of sorted balls isn't in proper order!";         exit 13
    end   /*k*/
say
say 'The sorted colored ball list has been confirmed as being sorted correctly.'
exit                                             /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input:

```txt

number of colored balls generated =  15

──── original ball order ─────
BRRRRBWWRBWRRBR

───── sorted  ball order ─────
RRRRRRRRWWWBBBB


The sorted colored ball list has been confirmed as being sorted correctly.


```


## Ring


```ring

# Project : Dutch national flag problem

flag = ["Red","White","Blue"]
balls = list(10)

see "Random: |"
for i = 1 to 10
     color = random(2) + 1
     balls[i] = flag[color]
     see  balls[i] + " |"
next
see nl

see "Sorted: |"
for i = 1 to 3
     color = flag[i]
     for j = 1 to 10
          if balls[j] = color
             see balls[j] + " |"
          ok
     next
next

```

Output:

```txt

Random: |Red |Blue |Red |White |Red |Blue |White |Blue |Red |White |
Sorted: |Red |Red |Red |Red |White |White |White |Blue |Blue |Blue |

```



## Ruby


```ruby
class Ball
  FLAG = {red: 1, white: 2, blue: 3}

  def initialize
    @color = FLAG.keys.sample
  end

  def color
    @color
  end

  def <=>(other)  # needed for sort, results in -1 for <, 0 for == and 1 for >.
    FLAG[self.color] <=> FLAG[other.color]
  end

  def inspect
    @color
  end
end

balls = []
balls = Array.new(8){Ball.new} while balls == balls.sort

puts "Random: #{balls}"
puts "Sorted: #{balls.sort}"

```

{{out}}

```txt
Random: [blue, red, red, red, blue, blue, white, red]
Sorted: [red, red, red, red, white, blue, blue, blue]

```



## Run BASIC


```runbasic
flag$ = "Red,White,Blue"

print "Random: |";
for i = 1 to 10
color = rnd(0) * 3 + 1
balls$(i) = word$(flag$,color,",")
print  balls$(i);" |";
next i

print :print "Sorted: |";
for i = 1 to 3
 color$ = word$(flag$,i,",")
 for j = 1 to 10
  if balls$(j) = color$ then
    print balls$(j);" |";
  end if
next j
next i
```


```txt
Random: |White |Blue |White |Red |Red |White |Red |Blue |Red |White |
Sorted: |Red |Red |Red |Red |White |White |White |White |Blue |Blue |
```



## Rust

{{libheader|rand}}

```rust
extern crate rand;

use rand::Rng;

// Color enums will be sorted by their top-to-bottom declaration order
#[derive(Eq,Ord,PartialOrd,PartialEq,Debug)]
enum Color {
    Red,
    White,
    Blue
}

fn is_sorted(list: &Vec<Color>) -> bool {
    let mut state = &Color::Red;
    for current in list.iter() {
        if current < state { return false; }
        if current > state { state = current; }
    }
    true
}


fn main() {
    let mut rng = rand::thread_rng();
    let mut colors: Vec<Color> = Vec::new();

    for _ in 1..10 {
        let r = rng.gen_range(0, 3);
        if      r == 0 { colors.push(Color::Red); }
        else if r == 1 { colors.push(Color::White); }
        else if r == 2 { colors.push(Color::Blue); }
    }

    while is_sorted(&colors) {
        rng.shuffle(&mut colors);
    }

    println!("Before: {:?}", colors);
    colors.sort();
    println!("After:  {:?}", colors);
    if !is_sorted(&colors) {
        println!("Oops, did not sort colors correctly!");
    }
}
```



## Scala


```scala
object FlagColor extends Enumeration {
    type FlagColor = Value
    val Red, White, Blue = Value
}

val genBalls = (1 to 10).map(i => FlagColor(scala.util.Random.nextInt(FlagColor.maxId)))
val sortedBalls = genBalls.sorted
val sorted = if (genBalls == sortedBalls) "sorted" else "not sorted"

println(s"Generated balls (${genBalls mkString " "}) are $sorted.")
println(s"Sorted balls (${sortedBalls mkString " "}) are sorted.")
```


{{out}}

```txt
Generated balls (Blue Blue Blue White Blue Blue Red Red Blue White) are not sorted.
Sorted balls (Red Red White White Blue Blue Blue Blue Blue Blue) are sorted.
```



## SQL


```SQL
-- Create and populate tables
create table colours (id integer primary key, name varchar(5));
insert into colours (id, name) values ( 1, 'red'  );
insert into colours (id, name) values ( 2, 'white');
insert into colours (id, name) values ( 3, 'blue' );

create table balls ( colour integer references colours );
insert into balls ( colour ) values ( 2 );
insert into balls ( colour ) values ( 2 );
insert into balls ( colour ) values ( 3 );
insert into balls ( colour ) values ( 2 );
insert into balls ( colour ) values ( 1 );
insert into balls ( colour ) values ( 3 );
insert into balls ( colour ) values ( 3 );
insert into balls ( colour ) values ( 2 );

-- Show the balls are unsorted
select
	colours.name
from
	balls
	join colours on balls.colour = colours.id;

-- Show the balls in dutch flag order
select
	colours.name
from
	balls
	join colours on balls.colour = colours.id
order by
	colours.id;

-- Tidy up
drop table balls;
drop table colours;
```

{{out}}

```txt
COLOUR
------
white
white
blue
white
red
blue
blue
white


COLOUR
------
red
white
white
white
white
blue
blue
blue
```

# ''Generating a randomized order of balls ensuring that they are not in the order of the Dutch national flag.'' Hmm - just loaded some data - could do better here...
# ''Sort the balls in a way idiomatic to your language.'' Yup!
# ''Check the sorted balls are in the order of the Dutch national flag.'' Not checked beyond eyeballing - is there a db implementation that gets <tt>order by</tt> wrong??


## Tcl

This isn't very efficient in terms of the sorting itself (and it happens to use <code>lsearch</code> twice in the comparator!) but it is very simple to write like this.

```tcl
# The comparison function
proc dutchflagcompare {a b} {
    set colors {red white blue}
    return [expr {[lsearch $colors $a] - [lsearch $colors $b]}]
}

# The test function (evil shimmer of list to string!)
proc isFlagSorted lst {
    expr {![regexp {blue.*(white|red)} $lst] && ![regexp {white.*red} $lst]}
}

# A ball generator
proc generateBalls n {
    for {set i 0} {$i<$n} {incr i} {
	lappend result [lindex {red white blue} [expr {int(rand()*3)}]]
    }
    return $result
}

# Do the challenge with 20 balls
set balls [generateBalls 20]
if {[isFlagSorted $balls]} {
    error "already a sorted flag"
}
set sorted [lsort -command dutchflagcompare $balls]
if {[isFlagSorted $sorted]} {
    puts "Sorted the flag\n$sorted"
} else {
    puts "sort failed\n$sorted"
}
```

{{out}}

```txt

Sorted the flag
red red red red red red red white white white white white white white white white blue blue blue blue

```



## UNIX Shell

{{works with|Bash}}

```bash
COLORS=(red white blue)

# to go from name to number, we make variables out of the color names
# (e.g. the variable "$red" has value "1").
for (( i=0; i<${#COLORS[@]}; ++i )); do
  eval ${COLORS[i]}=$i
done

# Make a random list
function random_balls {
  local -i n="$1"
  local -i i
  local balls=()
  for (( i=0; i < n; ++i )); do
    balls+=("${COLORS[RANDOM%${#COLORS[@]}]}")
  done
  echo "${balls[@]}"
}

# Test for Dutchness
function dutch? {
  if (( $# < 2 )); then
     return 0
  else
    local first="$1"
    shift
    if eval "(( $first > $1 ))"; then
      return 1
    else
      dutch? "$@"
    fi
  fi
}

# Sort into order
function dutch {
  local -i lo=-1 hi=$# i=0
  local a=("$@")
  while (( i < hi )); do
    case "${a[i]}" in
      red)
        let lo+=1
        local t="${a[lo]}"
        a[lo]="${a[i]}"
        a[i]="$t"
        let i+=1
        ;;
      white) let i+=1;;
      blue)
        let hi-=1
        local t="${a[hi]}"
        a[hi]="${a[i]}"
        a[i]="$t"
        ;;
    esac
  done
  echo "${a[@]}"
}
```


Test code:

```bash
declare -i len=${1:-10}
balls=()
while (( ${#balls[@]} < len )) || dutch? "${balls[@]}"; do
  balls=($(random_balls "$len"))
done
echo "Initial list: ${balls[@]}"
balls=($(dutch "${balls[@]}"))
echo "Sorted: ${balls[@]}"
```


{{out}}

```txt
Initial list: blue blue red blue red blue blue white blue red
Sorted: red red red white blue blue blue blue blue blue
```



## VBScript


```vb

'Solution derived from http://www.geeksforgeeks.org/sort-an-array-of-0s-1s-and-2s/.

'build an unsorted array with n elements
Function build_unsort(n)
	flag = Array("red","white","blue")
	Set random = CreateObject("System.Random")
	Dim arr()
	ReDim arr(n)
	For i = 0 To n
		arr(i) = flag(random.Next_2(0,3))
	Next
	build_unsort = arr
End Function

'sort routine
Function sort(arr)
	lo = 0
	mi = 0
	hi = UBound(arr)
	Do While mi <= hi
		Select Case arr(mi)
			Case "red"
				tmp = arr(lo)
				arr(lo) = arr(mi)
				arr(mi) = tmp
				lo = lo + 1
				mi = mi + 1
			Case "white"
				mi = mi + 1
			Case "blue"
				tmp = arr(mi)
				arr(mi) = arr(hi)
				arr(hi) = tmp
				hi = hi - 1
		End Select
	Loop
	sort = Join(arr,",")
End Function

unsort = build_unsort(11)
WScript.StdOut.Write "Unsorted: " & Join(unsort,",")
WScript.StdOut.WriteLine
WScript.StdOut.Write "Sorted: " & sort(unsort)
WScript.StdOut.WriteLine

```


{{Out}}

```txt

Unsorted: blue,white,white,blue,red,red,blue,red,red,red,white,white
Sorted: red,red,red,red,red,white,white,white,white,blue,blue,blue

```



## Visual FoxPro


### SQL Version


```vfp

CLOSE DATABASES ALL
LOCAL lcCollate As String, i As Integer, n As Integer
lcCollate = SET("Collate")
SET COLLATE TO "Machine"
*!* Colours table
CREATE CURSOR colours (id I UNIQUE, colour V(5))
INSERT INTO colours VALUES (1, "Red")
INSERT INTO colours VALUES (2, "White")
INSERT INTO colours VALUES (3, "Blue")
*!* Balls table
CREATE CURSOR balls (colour I, rowid I AUTOINC)
INDEX ON colour TAG colour
SET ORDER TO 0
*!* Make sure there is at least 1 of each colour
INSERT INTO balls (colour) VALUES(3)
INSERT INTO balls (colour) VALUES(1)
INSERT INTO balls (colour) VALUES(2)
RAND(-1)	&& Initialise random number generator
n = 24
FOR i = 4 TO n
	INSERT INTO balls (colour) VALUES (RanInt())
ENDFOR
*!* Show unsorted
SELECT bb.rowid, cc.colour FROM colours cc JOIN balls bb ON cc.id = bb.colour
*!* Select by correct order
SELECT bb.rowid, cc.colour FROM colours cc JOIN balls bb ON cc.id = bb.colour ;
ORDER BY cc.id INTO CURSOR dutchflag
*!* Show sorted records
BROWSE NOMODIFY IN SCREEN
SET COLLATE TO lcCollate

FUNCTION RanInt() As Integer
RETURN INT(3*RAND()) + 1
ENDFUNC

```


### Array Version


```vfp

LOCAL i As Integer, n As Integer, colours As String, k As Integer
colours = "Red,White,Blue"
n = 15
LOCAL ARRAY balls[n,2]
*!* Make sure there is at least 1 of each colour
balls[1,1] = "Blue"
balls[1,2] = 3
balls[2,1] = "Red"
balls[2,2] = 1
balls[3,1] = "White"
balls[3,2] = 2
RAND(-1)	&& Initialise random number generator
FOR i = 4 TO n
	k = RanInt()
	balls[i,1] = GETWORDNUM(colours, k, ",")
	balls[i,2] = k
ENDFOR
*!* Show the unsorted array
CLEAR
? "Unsorted..."
FOR i = 1 TO n
	? balls[i,1], balls[i,2]
ENDFOR
*!* Sort the array on column 2
ASORT(balls, 2)
*!* And show it...
?
? "Sorted..."
FOR i = 1 TO n
	? balls[i,1], balls[i,2]
ENDFOR

FUNCTION RanInt() As Integer
RETURN INT(3*RAND()) + 1
ENDFUNC

```



## zkl


```zkl
const RED=0, WHITE=1, BLUE=2; var BALLS=T(RED,WHITE,BLUE);
fcn colorBalls(balls){ balls.apply(T("red","white","blue").get).concat(", "); }

reg balls, sortedBalls;
do{
   balls=(0).pump(12,List,fcn{ BALLS[(0).random(3)] }); // create list of 12 random balls
   sortedBalls=balls.sort(); // balls is read only, sort creates new list
}while(balls==sortedBalls); // make sure sort does something
println("Original ball order:\n", colorBalls(balls));
println("\nSorted ball order:\n", colorBalls(sortedBalls));
```

{{out}}

```txt

Original ball order:
white, white, red, blue, red, red, red, red, blue, red, white, blue

Sorted ball order:
red, red, red, red, red, red, white, white, white, blue, blue, blue

```



## ZX Spectrum Basic

{{trans|Run_BASIC}}

```zxbasic
10 LET r$="Red": LET w$="White": LET b$="Blue"
20 LET c$="RWB"
30 DIM b(10)
40 PRINT "Random:"
50 FOR n=1 TO 10
60 LET b(n)=INT (RND*3)+1
70 PRINT VAL$ (c$(b(n))+"$");" ";
80 NEXT n
90 PRINT ''"Sorted:"
100 FOR i=1 TO 3
110 FOR j=1 TO 10
120 IF b(j)=i THEN PRINT VAL$ (c$(i)+"$");" ";
130 NEXT j
140 NEXT i
```


+++
title = "Elementary cellular automaton"
description = ""
date = 2019-10-14T10:07:18Z
aliases = []
[extra]
id = 17291
[taxonomies]
categories = []
tags = []
+++

{{task}}

An '''[[wp:elementary cellular automaton|elementary cellular automaton]]''' is a one-dimensional [[wp:cellular automaton|cellular automaton]] where there are two possible states (labeled 0 and 1) and the rule to determine the state of a cell in the next generation depends only on the current state of the cell and its two immediate neighbors.  Those three values can be encoded with three bits.

The rules of evolution are then encoded with eight bits indicating the outcome of each of the eight possibilities 111, 110, 101, 100, 011, 010, 001 and 000 in this order.  Thus for instance the rule 13 means that a state is updated to 1 only in the cases 011, 010 and 000, since 13 in binary is 0b00001101.


;Task:
Create a subroutine, program or function that allows to create and visualize the evolution of any of the 256 possible elementary cellular automaton of arbitrary space length and for any given initial state.  You can demonstrate your solution with any automaton of your choice.

The space state should ''wrap'':  this means that the left-most cell should be considered as the right neighbor of the right-most cell, and reciprocally.

This task is basically a generalization of [[one-dimensional cellular automata]].


;See also
* [http://natureofcode.com/book/chapter-7-cellular-automata Cellular automata (natureofcode.com)]





## Ada

{{works with|Ada 2012}}

```Ada
with Ada.Text_IO;
procedure Elementary_Cellular_Automaton is
   
   type t_Rule  is new Integer range 0..2**8-1;
   type t_State is array (Integer range <>) of Boolean;

   Cell_Image : constant array (Boolean) of Character := ('.', '#');

   function Image (State : in t_State) return String is
     (Cell_Image(State(State'First)) &
      (if State'Length <= 1 then ""
       else Image(State(State'First+1..State'Last))));

   -- More convenient representation of the rule
   type t_RuleA is array (Boolean, Boolean, Boolean) of Boolean;

   function Translate (Rule : in t_Rule) return t_RuleA is
      -- Better not use Pack attribute and Unchecked_Conversion
      -- because it would not be endianness independent...
      Remain : t_Rule := Rule;
   begin
      return Answer : t_RuleA do
         for K in Boolean loop
            for J in Boolean loop
               for I in Boolean loop
                  Answer(I,J,K) := (Remain mod 2 = 1);
                  Remain := Remain / 2;
               end loop;
            end loop;
         end loop;
      end return;
   end Translate;

   procedure Show_Automaton (Rule        : in t_Rule;
                             Initial     : in t_State;
                             Generations : in Positive) is
      RuleA : constant t_RuleA  := Translate(Rule);
      Width : constant Positive := Initial'Length;
      -- More convenient indices for neighbor wraparound with "mod"
      subtype t_State0 is t_State (0..Width-1);
      State     : t_State0 := Initial;
      New_State : t_State0;
   begin
      Ada.Text_IO.Put_Line ("Rule" & t_Rule'Image(Rule) & " :");
      for Generation in 1..Generations loop
         Ada.Text_IO.Put_Line (Image(State));
         for Cell in State'Range loop
            New_State(Cell) := RuleA(State((Cell-1) mod Width),
                                     State(Cell),
                                     State((Cell+1) mod Width));
         end loop;
         State := New_State;
      end loop;
   end Show_Automaton;

begin
   Show_Automaton (Rule        => 90,
                   Initial     => (-10..-1 => False, 0 => True, 1..10 => False),
                   Generations => 15);
   Show_Automaton (Rule        => 30,
                   Initial     => (-15..-1 => False, 0 => True, 1..15 => False),
                   Generations => 20);
   Show_Automaton (Rule        => 122,
                   Initial     => (-12..-1 => False, 0 => True, 1..12 => False),
                   Generations => 25);
end Elementary_Cellular_Automaton;

```

{{Output}}

```txt
Rule 90 :
..........#..........
.........#.#.........
........#...#........
.......#.#.#.#.......
......#.......#......
.....#.#.....#.#.....
....#...#...#...#....
...#.#.#.#.#.#.#.#...
..#...............#..
.#.#.............#.#.
#...#...........#...#
##.#.#.........#.#.##
.#....#.......#....#.
#.#..#.#.....#.#..#.#
#..##...#...#...##..#
Rule 30 :
...............#...............
..............###..............
.............#..##.............
............####.##............
...........#...#..##...........
..........###.####.##..........
.........#..#....#..##.........
........######..####.##........
.......#.....###...#..##.......
......###...#..##.####.##......
.....#..##.####.#....#..##.....
....####.#....#.##..####.##....
...#...#.##..##..###...#..##...
..###.##..###.###..##.####.##..
.#..#..###..#...###.#....#..##.
#######..#####.#..#.##..####.##
......###....#.####..###...#...
.....#..##..##....###..##.###..
....####.###.##..#..###.#...##.
...#...#...#..######..#.##.#.##
Rule 122 :
............#............
...........#.#...........
..........#.#.#..........
.........#.#.#.#.........
........#.#.#.#.#........
.......#.#.#.#.#.#.......
......#.#.#.#.#.#.#......
.....#.#.#.#.#.#.#.#.....
....#.#.#.#.#.#.#.#.#....
...#.#.#.#.#.#.#.#.#.#...
..#.#.#.#.#.#.#.#.#.#.#..
.#.#.#.#.#.#.#.#.#.#.#.#.
#.#.#.#.#.#.#.#.#.#.#.#.#
##.#.#.#.#.#.#.#.#.#.#.##
.##.#.#.#.#.#.#.#.#.#.##.
####.#.#.#.#.#.#.#.#.####
...##.#.#.#.#.#.#.#.##...
..####.#.#.#.#.#.#.####..
.##..##.#.#.#.#.#.##..##.
########.#.#.#.#.########
.......##.#.#.#.##.......
......####.#.#.####......
.....##..##.#.##..##.....
....########.########....
...##......###......##...
```



## AutoHotkey

{{works with|AutoHotkey 1.1}}

```AutoHotkey
state := StrSplit("0000000001000000000")
rule := 90
output := "Rule: " rule
Loop, 10 {
	output .= "`n" A_Index "`t" PrintState(state)
	state := NextState(state, rule)
}
Gui, Font,, Courier New
Gui, Add, Text,, % output
Gui, Show
return

GuiClose:
ExitApp

; Returns the next state based on the current state and rule.
NextState(state, rule) {
	r := ByteDigits(rule)
	result := {}
	for i, val in state {
		if (i = 1)			; The leftmost cell
			result.Insert(r[state[state.MaxIndex()] state.1 state.2])
		else if (i = state.MaxIndex())	; The rightmost cell
			result.Insert(r[state[i-1] val state.1])
		else				; All cells between leftmost and rightmost
			result.Insert(r[state[i - 1] val state[i + 1]])
	}
	return result
}

; Returns an array with each three digit sequence as a key corresponding to a value 
; of true or false depending on the rule.
ByteDigits(rule) { 
	res := {}
	for i, val in ["000", "001", "010", "011", "100", "101", "110", "111"] {
		res[val] := Mod(rule, 2)
		rule >>= 1
	}
	return res
}

; Converts 0 and 1 to . and # respectively and returns a string representing the state
PrintState(state) {
	for i, val in state
		result .= val = 1 ? "#" : "."
	return result
}
```

{{Output}}

```txt
Rule: 90
1	.........#.........
2	........#.#........
3	.......#...#.......
4	......#.#.#.#......
5	.....#.......#.....
6	....#.#.....#.#....
7	...#...#...#...#...
8	..#.#.#.#.#.#.#.#..
9	.#...............#.
10	#.#.............#.#
```



## C

64 cells, edges are cyclic.

```c>#include <stdio.h

#include <limits.h>

typedef unsigned long long ull;
#define N  (sizeof(ull) * CHAR_BIT)
#define B(x) (1ULL << (x))

void evolve(ull state, int rule)
{
	int i;
	ull st;

	printf("Rule %d:\n", rule);
	do {
		st = state;
		for (i = N; i--; ) putchar(st & B(i) ? '#' : '.');
		putchar('\n');

		for (state = i = 0; i < N; i++)
			if (rule & B(7 & (st>>(i-1) | st<<(N+1-i))))
				state |= B(i);
	} while (st != state);
}

int main(int argc, char **argv)
{
	evolve(B(N/2), 90);
	evolve(B(N/4)|B(N - N/4), 30); // well, enjoy the fireworks

	return 0;
}
```

{{out}}

```txt

Rule 90:
................................#...............................
...............................#.#..............................
..............................#...#.............................
.............................#.#.#.#............................
............................#.......#...........................
...........................#.#.....#.#..........................
..........................#...#...#...#.........................
.........................#.#.#.#.#.#.#.#........................
........................#...............#.......................
                     ---(output snipped)---

```



## C++


```cpp>#include <bitset

#include <stdio.h>

#define SIZE	           80
#define RULE               30
#define RULE_TEST(x)       (RULE & 1 << (7 & (x)))

void evolve(std::bitset<SIZE> &s) {
    int i;
    std::bitset<SIZE> t(0);
    t[SIZE-1] = RULE_TEST( s[0] << 2 | s[SIZE-1] << 1 | s[SIZE-2] );
    t[     0] = RULE_TEST( s[1] << 2 | s[     0] << 1 | s[SIZE-1] );
    for (i = 1; i < SIZE-1; i++)
	t[i] = RULE_TEST( s[i+1] << 2 | s[i] << 1 | s[i-1] );
    for (i = 0; i < SIZE; i++) s[i] = t[i];
}
void show(std::bitset<SIZE> s) {
    int i;
    for (i = SIZE; --i; ) printf("%c", s[i] ? '#' : ' ');
    printf("\n");
}
int main() {
    int i;
    std::bitset<SIZE> state(1);
    state <<= SIZE / 2;
    for (i=0; i<10; i++) {
	show(state);
	evolve(state);
    }
    return 0;
}
```

{{out}}

```txt
                                       #                                       |
                                      ###                                      |
                                     ##  #                                     |
                                    ## ####                                    |
                                   ##  #   #                                   |
                                  ## #### ###                                  |
                                 ##  #    #  #                                 |
                                ## ####  ######                                |
                               ##  #   ###     #                               |
                              ## #### ##  #   ###                              |
```



=={{header|C sharp|C#}}==

```csharp

using System;
using System.Collections;
namespace ElementaryCellularAutomaton
{
    class Automata
    {
        BitArray cells, ncells;
        const int MAX_CELLS = 19;

        public void run()
        {
            cells = new BitArray(MAX_CELLS);
            ncells = new BitArray(MAX_CELLS);
            while (true)
            {
                Console.Clear();
                Console.WriteLine("What Rule do you want to visualize");
                doRule(int.Parse(Console.ReadLine()));
                Console.WriteLine("Press any key to continue...");
                Console.ReadKey();
            }
        }

        private byte getCells(int index)
        {
            byte b;
            int i1 = index - 1,
                i2 = index,
                i3 = index + 1;

            if (i1 < 0) i1 = MAX_CELLS - 1;
            if (i3 >= MAX_CELLS) i3 -= MAX_CELLS;

            b = Convert.ToByte(
                4 * Convert.ToByte(cells.Get(i1)) +
                2 * Convert.ToByte(cells.Get(i2)) +
                Convert.ToByte(cells.Get(i3)));
            return b;
        }

        private string getBase2(int i)
        {
            string s = Convert.ToString(i, 2);
            while (s.Length < 8)
            { s = "0" + s; }
            return s;
        }

        private void doRule(int rule)
        {
            Console.Clear();
            string rl = getBase2(rule);
            cells.SetAll(false);
            ncells.SetAll(false);
            cells.Set(MAX_CELLS / 2, true);

            Console.WriteLine("Rule: " + rule + "\n----------\n");

            for (int gen = 0; gen < 51; gen++)
            {
                Console.Write("{0, 4}", gen + ": ");

                foreach (bool b in cells)
                    Console.Write(b ? "#" : ".");

                Console.WriteLine("");

                int i = 0;
                while (true)
                {
                    byte b = getCells(i);
                    ncells[i] = '1' == rl[7 - b] ? true : false;
                    if (++i == MAX_CELLS) break;
                }

                i = 0;
                foreach (bool b in ncells)
                    cells[i++] = b;
            }
            Console.WriteLine("");
        }

    };
    class Program
    {
        static void Main(string[] args)
        {
            Automata t = new Automata();
            t.run();
        }
    }
}

```

{{out}}

```txt

 Rule: 90
----------

 0: .........#.........
 1: ........#.#........
 2: .......#...#.......
 3: ......#.#.#.#......
 4: .....#.......#.....
 5: ....#.#.....#.#....
 6: ...#...#...#...#...
 7: ..#.#.#.#.#.#.#.#..
 8: .#...............#.
 9: #.#.............#.#
10: #..#...........#..#
11: ###.#.........#.###
12: ..#..#.......#..#..
13: .#.##.#.....#.##.#.
14: #..##..#...#..##..#
15: #######.#.#.#######
16: ......#.....#......
17: .....#.#...#.#.....
18: ....#...#.#...#....
19: ...#.#.#...#.#.#...
20: ..#.....#.#.....#..
21: .#.#...#...#...#.#.
22: #...#.#.#.#.#.#...#
23: ##.#...........#.##
24: .#..#.........#..#.
25: #.##.#.......#.##.#
26: #.##..#.....#..##.#
27: #.####.#...#.####.#
28: #.#..#..#.#..#..#.#
29: #..##.##...##.##..#
30: #####.###.###.#####
31: ....#.#.#.#.#.#....
32: ...#...........#...
33: ..#.#.........#.#..
34: .#...#.......#...#.
35: #.#.#.#.....#.#.#.#
36: #......#...#......#
37: ##....#.#.#.#....##
38: .##..#.......#..##.
39: #####.#.....#.#####
40: ....#..#...#..#....
41: ...#.##.#.#.##.#...
42: ..#..##.....##..#..
43: .#.#####...#####.#.
44: #..#...##.##...#..#
45: ###.#.###.###.#.###
46: ..#...#.#.#.#...#..
47: .#.#.#.......#.#.#.
48: #.....#.....#.....#
49: ##...#.#...#.#...##
50: .##.#...#.#...#.##.

```



## Ceylon


```ceylon
class Rule(number) satisfies Correspondence<Boolean[3], Boolean> {
	
	shared Byte number;
	
	"all 3 bit patterns will return a value so this is always true"
	shared actual Boolean defines(Boolean[3] key) => true;
	
	shared actual Boolean? get(Boolean[3] key) => 
			number.get((key[0] then 4 else 0) + (key[1] then 2 else 0) + (key[2] then 1 else 0));
	
	function binaryString(Integer integer, Integer maxPadding) =>
			Integer.format(integer, 2).padLeading(maxPadding, '0');
	
	string => 
			let (digits = binaryString(number.unsigned, 8))
			"Rule #``number``
			 ``" | ".join { for (pattern in $111..0) binaryString(pattern, 3) }`` 
			 ``" | ".join(digits.map((Character element) => element.string.pad(3)))``";
}

class ElementaryAutomaton {
	
	shared static ElementaryAutomaton|ParseException parse(Rule rule, String cells, Character aliveChar, Character deadChar) {
		if (!cells.every((Character element) => element == aliveChar || element == deadChar)) {
			return ParseException("the string was not a valid automaton");
		}
		return ElementaryAutomaton(rule, cells.map((Character element) => element == aliveChar));
	}

	shared Rule rule;
	
	Array<Boolean> cells;
	
	shared new(Rule rule, {Boolean*} initialCells) {
		this.rule = rule;
		this.cells = Array { *initialCells }; 
	}
	
	shared Boolean evolve() {
		
		if (cells.empty) {
			return false;
		}
		
		function left(Integer index) {
			assert (exists cell = cells[index - 1] else cells.last);
			return cell;
		}

		function right(Integer index) {
			assert (exists cell = cells[index + 1] else cells.first);
			return cell;
		}
		
		value newCells = Array.ofSize(cells.size, false);
		for (index->cell in cells.indexed) {
			value neighbourhood = [left(index), cell, right(index)];
			assert (exists newCell = rule[neighbourhood]);
			newCells[index] = newCell;
		}

		if (newCells == cells) {
			return false;
		}
		
		newCells.copyTo(cells);
		return true;	
	}
	
	shared void display(Character aliveChar = '#', Character deadChar = ' ') {
		print("".join(cells.map((Boolean element) => element then aliveChar else deadChar)));
	}
}

shared void run() {
	value rule = Rule(90.byte);
	print(rule);
	
	value automaton = ElementaryAutomaton.parse(rule, "          #          ", '#', ' ');
	assert (is ElementaryAutomaton automaton);
	
	for (generation in 0..10) {
		automaton.display();
		automaton.evolve();
	}
}
```

{{out}}

```txt
Rule #90
111 | 110 | 101 | 100 | 011 | 010 | 001 | 000 
 0  |  1  |  0  |  1  |  1  |  0  |  1  |  0 
          #          
         # #         
        #   #        
       # # # #       
      #       #      
     # #     # #     
    #   #   #   #    
   # # # # # # # #   
  #               #  
 # #             # # 
#   #           #   #
```



## Common Lisp


```lisp
(defun automaton (init rule &optional (stop 10))
  (labels ((next-gen (cells)
             (mapcar #'new-cell 
                     (cons (car (last cells)) cells)
                     cells
                     (append (cdr cells) (list (car cells)))))

           (new-cell (left current right)
             (let ((shift (+ (* left 4) (* current 2) right)))
               (if (logtest rule (ash 1 shift)) 1 0)))

           (pretty-print (cells)
             (format T "~{~a~}~%" 
                     (mapcar (lambda (x) (if (zerop x) #\. #\#))
                             cells))))

    (loop for cells = init then (next-gen cells)
          for i below stop
          do (pretty-print cells))))

(automaton '(0 0 0 0 0 0 1 0 0 0 0 0 0) 90)
```


{{Out}}

```txt
......#......
.....#.#.....
....#...#....
...#.#.#.#...
..#.......#..
.#.#.....#.#.
#...#...#...#
##.#.#.#.#.##
.#.........#.
#.#.......#.#
```



## D

{{trans|Python}}

```d
import std.stdio, std.string, std.conv, std.range, std.algorithm, std.typecons;

enum mod = (in int n, in int m) pure nothrow @safe @nogc => ((n % m) + m) % m;

struct ECAwrap {
    public string front;
    public enum bool empty = false;
    private immutable const(char)[string] next;

    this(in string cells_, in uint rule) pure @safe {
        this.front = cells_;
        immutable ruleBits = "%08b".format(rule).retro.text;
        next = 8.iota.map!(n => tuple("%03b".format(n), char(ruleBits[n]))).assocArray;
    }

    void popFront() pure @safe {
        alias c = front;
        c = iota(c.length)
            .map!(i => next[[c[(i - 1).mod($)], c[i], c[(i + 1) % $]]])
            .text;
    }
}

void main() @safe {
    enum nLines = 50;
    immutable string start = "0000000001000000000";
    immutable uint[] rules = [90, 30, 122];
    writeln("Rules: ", rules);
    auto ecas = rules.map!(rule => ECAwrap(start, rule)).array;

    foreach (immutable i; 0 .. nLines) {
        writefln("%2d: %-(%s    %)", i, ecas.map!(eca => eca.front.tr("01", ".#")));
        foreach (ref eca; ecas)
            eca.popFront;
    }
}
```

{{out}}

```txt
Rules: [90, 30, 122]
 0: .........#.........    .........#.........    .........#.........
 1: ........#.#........    ........###........    ........#.#........
 2: .......#...#.......    .......##..#.......    .......#.#.#.......
 3: ......#.#.#.#......    ......##.####......    ......#.#.#.#......
 4: .....#.......#.....    .....##..#...#.....    .....#.#.#.#.#.....
 5: ....#.#.....#.#....    ....##.####.###....    ....#.#.#.#.#.#....
 6: ...#...#...#...#...    ...##..#....#..#...    ...#.#.#.#.#.#.#...
 7: ..#.#.#.#.#.#.#.#..    ..##.####..######..    ..#.#.#.#.#.#.#.#..
 8: .#...............#.    .##..#...###.....#.    .#.#.#.#.#.#.#.#.#.
 9: #.#.............#.#    ##.####.##..#...###    #.#.#.#.#.#.#.#.#.#
10: #..#...........#..#    ...#....#.####.##..    ##.#.#.#.#.#.#.#.##
11: ###.#.........#.###    ..###..##.#....#.#.    .##.#.#.#.#.#.#.##.
12: ..#..#.......#..#..    .##..###..##..##.##    ####.#.#.#.#.#.####
13: .#.##.#.....#.##.#.    .#.###..###.###..#.    ...##.#.#.#.#.##...
14: #..##..#...#..##..#    ##.#..###...#..####    ..####.#.#.#.####..
15: #######.#.#.#######    ...####..#.#####...    .##..##.#.#.##..##.
16: ......#.....#......    ..##...###.#....#..    ########.#.########
17: .....#.#...#.#.....    .##.#.##...##..###.    .......##.##.......
18: ....#...#.#...#....    ##..#.#.#.##.###..#    ......#######......
19: ...#.#.#...#.#.#...    ..###.#.#.#..#..###    .....##.....##.....
20: ..#.....#.#.....#..    ###...#.#.#######..    ....####...####....
21: .#.#...#...#...#.#.    #..#.##.#.#......##    ...##..##.##..##...
22: #...#.#.#.#.#.#...#    .###.#..#.##....##.    ..###############..
23: ##.#...........#.##    ##...####.#.#..##.#    .##.............##.
24: .#..#.........#..#.    ..#.##....#.####..#    ####...........####
25: #.##.#.......#.##.#    ###.#.#..##.#...###    ...##.........##...
26: #.##..#.....#..##.#    ....#.####..##.##..    ..####.......####..
27: #.####.#...#.####.#    ...##.#...###..#.#.    .##..##.....##..##.
28: #.#..#..#.#..#..#.#    ..##..##.##..###.##    ########...########
29: #..##.##...##.##..#    ###.###..#.###...#.    .......##.##.......
30: #####.###.###.#####    #...#..###.#..#.##.    ......#######......
31: ....#.#.#.#.#.#....    ##.#####...####.#..    .....##.....##.....
32: ...#...........#...    #..#....#.##....###    ....####...####....
33: ..#.#.........#.#..    .####..##.#.#..##..    ...##..##.##..##...
34: .#...#.......#...#.    ##...###..#.####.#.    ..###############..
35: #.#.#.#.....#.#.#.#    #.#.##..###.#....#.    .##.............##.
36: #......#...#......#    #.#.#.###...##..##.    ####...........####
37: ##....#.#.#.#....##    #.#.#.#..#.##.###..    ...##.........##...
38: .##..#.......#..##.    #.#.#.####.#..#..##    ..####.......####..
39: #####.#.....#.#####    ..#.#.#....#######.    .##..##.....##..##.
40: ....#..#...#..#....    .##.#.##..##......#    ########...########
41: ...#.##.#.#.##.#...    .#..#.#.###.#....##    .......##.##.......
42: ..#..##.....##..#..    .####.#.#...##..##.    ......#######......
43: .#.#####...#####.#.    ##....#.##.##.###.#    .....##.....##.....
44: #..#...##.##...#..#    ..#..##.#..#..#...#    ....####...####....
45: ###.#.###.###.#.###    ######..########.##    ...##..##.##..##...
46: ..#...#.#.#.#...#..    ......###........#.    ..###############..
47: .#.#.#.......#.#.#.    .....##..#......###    .##.............##.
48: #.....#.....#.....#    #...##.####....##..    ####...........####
49: ##...#.#...#.#...##    ##.##..#...#..##.##    ...##.........##...
```



## EchoLisp

Pictures of the (nice) generated colored bit-maps : The Escher like [http://www.echolalie.org/echolisp/images/automaton-1.png (task 90 5)] and the fractal like  [http://www.echolalie.org/echolisp/images/automaton-2.png (task 22 1)]


```scheme

(lib 'types) ;; int32 vectors
(lib 'plot)

(define-constant BIT0 0)
(define-constant BIT1 (rgb 0.8 0.9 0.7)) ;; colored bit 1

;; integer to pattern
(define ( n->pat n)
		(for/vector ((i 8))
		#:when (bitwise-bit-set? n i)
		(for/vector  ((j (in-range 2 -1 -1)))
		(if (bitwise-bit-set? i j) BIT1  BIT0 ))))
		
;; test if three pixels match a pattern
(define (pmatch a b c pat)
		(for/or ((v pat))
		(and (= a (vector-ref v 0))  (= b (vector-ref v 1))   (= c (vector-ref v 2)) )))
		
;; next generation = next row
(define (generate x0 width PAT PIX (x)) 
		(for ((dx (in-range 0 width)))
		(set! x (+ x0 dx))
		(vector-set! PIX (+ x width) ;; next row
			(if 
			(pmatch 
				(vector-ref PIX (if (zero? dx) (+ x0 width) (1- x))) ;; let's wrap
				(vector-ref PIX x)  
				(vector-ref PIX (if (= dx (1- width)) x0 (1+ x)))
				PAT) 
			BIT1 BIT0))))
		
;; n is the pattern, starters in the number of set pixels at generation 0
(define (task n (starters 1))
		(define width (first (plot-size)))
		(define height (rest (plot-size)))
		(define PAT (n->pat n))
		(plot-clear)
		
		(define PIX (pixels->int32-vector))
		(init-pix  starters  width height PIX)
		
		(for ((y (1- height)))
			(generate (* y width) width PAT into: PIX))
		(vector->pixels PIX))
		
;; put n starters on first row
(define (init-pix starters width height PIX)
	(define dw (floor (/ width (1+ starters))))
	(for ((x (in-range dw width (1+ dw))))  
				(vector-set! PIX x BIT1)))

;; usage
(task 99 3) → 672400 ;; ESC to see it
(task 22)   → 672400

;; check pattern generator
(n->pat 13)
    → #( #( 0 0 0) #( 0 -5052980 0) #( 0 -5052980 -5052980))


```




## Elixir

{{works with|Elixir|1.3}}
{{trans|Ruby}}

```elixir
defmodule Elementary_cellular_automaton do
  def run(start_str, rule, times) do
    IO.puts "rule : #{rule}"
    each(start_str, rule_pattern(rule), times)
  end
  
  defp rule_pattern(rule) do
    list = Integer.to_string(rule, 2) |> String.pad_leading(8, "0")
           |> String.codepoints |> Enum.reverse
    Enum.map(0..7, fn i -> Integer.to_string(i, 2) |> String.pad_leading(3, "0") end)
    |> Enum.zip(list) |> Map.new
  end
  
  defp each(_, _, 0), do: :ok
  defp each(str, patterns, times) do
    IO.puts String.replace(str, "0", ".") |> String.replace("1", "#")
    str2 = String.last(str) <> str <> String.first(str)
    next_str = Enum.map_join(0..String.length(str)-1, fn i ->
      Map.get(patterns, String.slice(str2, i, 3))
    end)
    each(next_str, patterns, times-1)
  end
end

pad = String.duplicate("0", 14)
str = pad <> "1" <> pad
Elementary_cellular_automaton.run(str, 18, 25)
```


{{out}}

```txt

rule : 18
..............#..............
.............#.#.............
............#...#............
...........#.#.#.#...........
..........#.......#..........
.........#.#.....#.#.........
........#...#...#...#........
.......#.#.#.#.#.#.#.#.......
......#...............#......
.....#.#.............#.#.....
....#...#...........#...#....
...#.#.#.#.........#.#.#.#...
..#.......#.......#.......#..
.#.#.....#.#.....#.#.....#.#.
#...#...#...#...#...#...#...#
.#.#.#.#.#.#.#.#.#.#.#.#.#.#.
#...........................#
.#.........................#.
#.#.......................#.#
...#.....................#...
..#.#...................#.#..
.#...#.................#...#.
#.#.#.#...............#.#.#.#
.......#.............#.......
......#.#...........#.#......

```


=={{header|F_Sharp|F#}}==

### The Function


```fsharp

// Elementary Cellular Automaton . Nigel Galloway: July 31st., 2019
let eca N=
  let N=Array.init 8 (fun n->(N>>>n)%2)
  Seq.unfold(fun G->Some(G,[|yield Array.last G; yield! G; yield Array.head G|]|>Array.windowed 3|>Array.map(fun n->N.[n.[2]+2*n.[1]+4*n.[0]])))

```


### The Task


```fsharp

eca 90 [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] |> Seq.take 80 |> Seq.iter(fun n->Array.iter(fun n->printf "%s" (if n=0 then " " else "@"))n; printfn "")

```

{{out}}

```txt

                                @                         
                               @ @                               
                              @   @                              
                             @ @ @ @                             
                            @       @                            
                           @ @     @ @                           
                          @   @   @   @                          
                         @ @ @ @ @ @ @ @                         
                        @               @                        
                       @ @             @ @                       
                      @   @           @   @                      
                     @ @ @ @         @ @ @ @                     
                    @       @       @       @                    
                   @ @     @ @     @ @     @ @                   
                  @   @   @   @   @   @   @   @                  
                 @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @                 
                @                               @                
               @ @                             @ @               
              @   @                           @   @              
             @ @ @ @                         @ @ @ @             
            @       @                       @       @            
           @ @     @ @                     @ @     @ @           
          @   @   @   @                   @   @   @   @          
         @ @ @ @ @ @ @ @                 @ @ @ @ @ @ @ @         
        @               @               @               @        
       @ @             @ @             @ @             @ @       
      @   @           @   @           @   @           @   @      
     @ @ @ @         @ @ @ @         @ @ @ @         @ @ @ @     
    @       @       @       @       @       @       @       @    
   @ @     @ @     @ @     @ @     @ @     @ @     @ @     @ @   
  @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @  
 @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ 
@                                                               @
@@                                                             @@
 @@                                                           @@ 
@@@@                                                         @@@@
   @@                                                       @@   
  @@@@                                                     @@@@  
 @@  @@                                                   @@  @@ 
@@@@@@@@                                                 @@@@@@@@
       @@                                               @@       
      @@@@                                             @@@@      
     @@  @@                                           @@  @@     
    @@@@@@@@                                         @@@@@@@@    
   @@      @@                                       @@      @@   
  @@@@    @@@@                                     @@@@    @@@@  
 @@  @@  @@  @@                                   @@  @@  @@  @@ 
@@@@@@@@@@@@@@@@                                 @@@@@@@@@@@@@@@@
               @@                               @@               
              @@@@                             @@@@              
             @@  @@                           @@  @@             
            @@@@@@@@                         @@@@@@@@            
           @@      @@                       @@      @@           
          @@@@    @@@@                     @@@@    @@@@          
         @@  @@  @@  @@                   @@  @@  @@  @@         
        @@@@@@@@@@@@@@@@                 @@@@@@@@@@@@@@@@        
       @@              @@               @@              @@       
      @@@@            @@@@             @@@@            @@@@      
     @@  @@          @@  @@           @@  @@          @@  @@     
    @@@@@@@@        @@@@@@@@         @@@@@@@@        @@@@@@@@    
   @@      @@      @@      @@       @@      @@      @@      @@   
  @@@@    @@@@    @@@@    @@@@     @@@@    @@@@    @@@@    @@@@  
 @@  @@  @@  @@  @@  @@  @@  @@   @@  @@  @@  @@  @@  @@  @@  @@ 
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                               @ @                               
                              @   @                              
                             @ @ @ @                             
                            @       @                            
                           @ @     @ @                           
                          @   @   @   @                          
                         @ @ @ @ @ @ @ @                         
                        @               @                        
                       @ @             @ @                       
                      @   @           @   @                      
                     @ @ @ @         @ @ @ @                     
                    @       @       @       @                    
                   @ @     @ @     @ @     @ @                   
                  @   @   @   @   @   @   @   @                  
                 @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @                 
                @                               @                

```


```fsharp

eca 110 [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1|] |> Seq.take 80 |> Seq.iter(fun n->Array.iter(fun n->printf "%s" (if n=0 then " " else "@"))n; printfn "")

```

{{out}}

```txt

                                                                @
                                                               @@
                                                              @@@
                                                             @@ @
                                                            @@@@@
                                                           @@   @
                                                          @@@  @@
                                                         @@ @ @@@
                                                        @@@@@@@ @
                                                       @@     @@@
                                                      @@@    @@ @
                                                     @@ @   @@@@@
                                                    @@@@@  @@   @
                                                   @@   @ @@@  @@
                                                  @@@  @@@@ @ @@@
                                                 @@ @ @@  @@@@@ @
                                                @@@@@@@@ @@   @@@
                                               @@      @@@@  @@ @
                                              @@@     @@  @ @@@@@
                                             @@ @    @@@ @@@@   @
                                            @@@@@   @@ @@@  @  @@
                                           @@   @  @@@@@ @ @@ @@@
                                          @@@  @@ @@   @@@@@@@@ @
                                         @@ @ @@@@@@  @@      @@@
                                        @@@@@@@    @ @@@     @@ @
                                       @@     @   @@@@ @    @@@@@
                                      @@@    @@  @@  @@@   @@   @
                                     @@ @   @@@ @@@ @@ @  @@@  @@
                                    @@@@@  @@ @@@ @@@@@@ @@ @ @@@
                                   @@   @ @@@@@ @@@    @@@@@@@@ @
                                  @@@  @@@@   @@@ @   @@      @@@
                                 @@ @ @@  @  @@ @@@  @@@     @@ @
                                @@@@@@@@ @@ @@@@@ @ @@ @    @@@@@
                               @@      @@@@@@   @@@@@@@@   @@   @
                              @@@     @@    @  @@      @  @@@  @@
                             @@ @    @@@   @@ @@@     @@ @@ @ @@@
                            @@@@@   @@ @  @@@@@ @    @@@@@@@@@@ @
                           @@   @  @@@@@ @@   @@@   @@        @@@
                          @@@  @@ @@   @@@@  @@ @  @@@       @@ @
                         @@ @ @@@@@@  @@  @ @@@@@ @@ @      @@@@@
                        @@@@@@@    @ @@@ @@@@   @@@@@@     @@   @
                       @@     @   @@@@ @@@  @  @@    @    @@@  @@
                      @@@    @@  @@  @@@ @ @@ @@@   @@   @@ @ @@@
                     @@ @   @@@ @@@ @@ @@@@@@@@ @  @@@  @@@@@@@ @
                    @@@@@  @@ @@@ @@@@@@      @@@ @@ @ @@     @@@
                   @@   @ @@@@@ @@@    @     @@ @@@@@@@@@    @@ @
                  @@@  @@@@   @@@ @   @@    @@@@@       @   @@@@@
                 @@ @ @@  @  @@ @@@  @@@   @@   @      @@  @@   @
                @@@@@@@@ @@ @@@@@ @ @@ @  @@@  @@     @@@ @@@  @@
               @@      @@@@@@   @@@@@@@@ @@ @ @@@    @@ @@@ @ @@@
              @@@     @@    @  @@      @@@@@@@@ @   @@@@@ @@@@@ @
             @@ @    @@@   @@ @@@     @@      @@@  @@   @@@   @@@
            @@@@@   @@ @  @@@@@ @    @@@     @@ @ @@@  @@ @  @@ @
           @@   @  @@@@@ @@   @@@   @@ @    @@@@@@@ @ @@@@@ @@@@@
          @@@  @@ @@   @@@@  @@ @  @@@@@   @@     @@@@@   @@@   @
         @@ @ @@@@@@  @@  @ @@@@@ @@   @  @@@    @@   @  @@ @  @@
        @@@@@@@    @ @@@ @@@@   @@@@  @@ @@ @   @@@  @@ @@@@@ @@@
       @@     @   @@@@ @@@  @  @@  @ @@@@@@@@  @@ @ @@@@@   @@@ @
      @@@    @@  @@  @@@ @ @@ @@@ @@@@      @ @@@@@@@   @  @@ @@@
     @@ @   @@@ @@@ @@ @@@@@@@@ @@@  @     @@@@     @  @@ @@@@@ @
    @@@@@  @@ @@@ @@@@@@      @@@ @ @@    @@  @    @@ @@@@@   @@@
   @@   @ @@@@@ @@@    @     @@ @@@@@@   @@@ @@   @@@@@   @  @@ @
  @@@  @@@@   @@@ @   @@    @@@@@    @  @@ @@@@  @@   @  @@ @@@@@
 @@ @ @@  @  @@ @@@  @@@   @@   @   @@ @@@@@  @ @@@  @@ @@@@@   @
@@@@@@@@ @@ @@@@@ @ @@ @  @@@  @@  @@@@@   @ @@@@ @ @@@@@   @  @@
       @@@@@@   @@@@@@@@ @@ @ @@@ @@   @  @@@@  @@@@@   @  @@ @@ 
      @@    @  @@      @@@@@@@@ @@@@  @@ @@  @ @@   @  @@ @@@@@@ 
     @@@   @@ @@@     @@      @@@  @ @@@@@@ @@@@@  @@ @@@@@    @ 
    @@ @  @@@@@ @    @@@     @@ @ @@@@    @@@   @ @@@@@   @   @@ 
   @@@@@ @@   @@@   @@ @    @@@@@@@  @   @@ @  @@@@   @  @@  @@@ 
  @@   @@@@  @@ @  @@@@@   @@     @ @@  @@@@@ @@  @  @@ @@@ @@ @ 
 @@@  @@  @ @@@@@ @@   @  @@@    @@@@@ @@   @@@@ @@ @@@@@ @@@@@@ 
@@ @ @@@ @@@@   @@@@  @@ @@ @   @@   @@@@  @@  @@@@@@   @@@    @ 
@@@@@@ @@@  @  @@  @ @@@@@@@@  @@@  @@  @ @@@ @@    @  @@ @   @@@
     @@@ @ @@ @@@ @@@@      @ @@ @ @@@ @@@@ @@@@   @@ @@@@@  @@  
    @@ @@@@@@@@ @@@  @     @@@@@@@@@ @@@  @@@  @  @@@@@   @ @@@  
   @@@@@      @@@ @ @@    @@       @@@ @ @@ @ @@ @@   @  @@@@ @  
  @@   @     @@ @@@@@@   @@@      @@ @@@@@@@@@@@@@@  @@ @@  @@@  
 @@@  @@    @@@@@    @  @@ @     @@@@@            @ @@@@@@ @@ @  
@@ @ @@@   @@   @   @@ @@@@@    @@   @           @@@@    @@@@@@  

```



## Factor


```factor
USING: assocs formatting grouping io kernel math math.bits
math.combinatorics sequences sequences.extras ;

: make-rules ( n -- assoc )
    { f t } 3 selections swap make-bits 8 f pad-tail zip ;

: next-state ( assoc seq -- assoc seq' )
    dupd 3 circular-clump -1 rotate [ of ] with map ;

: first-state ( -- seq ) 15 f <repetition> dup { t } glue ;

: show-state ( seq -- ) [ "#" "." ? write ] each nl ;

: show-automaton ( rule -- )
    dup "Rule %d:\n" printf make-rules first-state 16
    [ dup show-state next-state ] times 2drop ;

90 show-automaton
```

{{out}}

```txt

Rule 90:
...............#...............
..............#.#..............
.............#...#.............
............#.#.#.#............
...........#.......#...........
..........#.#.....#.#..........
.........#...#...#...#.........
........#.#.#.#.#.#.#.#........
.......#...............#.......
......#.#.............#.#......
.....#...#...........#...#.....
....#.#.#.#.........#.#.#.#....
...#.......#.......#.......#...
..#.#.....#.#.....#.#.....#.#..
.#...#...#...#...#...#...#...#.
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Elementary_cellular_automaton this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## GFA Basic


<lang>
'
' Elementary One-Dimensional Cellular Automaton
'
' World is cyclic, and rules are defined by a parameter
'
' start$="01110110101010100100" ! start state for world
' rules%=104 ! number defining rule-set to use
start$="00000000000000000000100000000000000000000"
rules%=18
max_cycles%=20 ! give a maximum depth to world
'
' Global variables hold the world, with two rows
' world! is treated as cyclical
' cur% gives the row for current world,
' new% gives the row for the next world.
'
size%=LEN(start$)
DIM world!(size%,2)
cur%=0
new%=1
clock%=0
'
@setup_world(start$)
OPENW 1
CLEARW 1
DO
  @display_world
  @update_world
  EXIT IF @same_state
  clock%=clock%+1
  EXIT IF clock%>max_cycles% ! safety net
LOOP
~INP(2)
CLOSEW 1
'
' parse given string to set up initial states in world
' -- assumes world! is of correct size
'
PROCEDURE setup_world(defn$)
  LOCAL i%
  ' clear out the array
  ARRAYFILL world!(),FALSE
  ' for each 1 in string, set cell to true
  FOR i%=1 TO LEN(defn$)
    IF MID$(defn$,i%,1)="1"
      world!(i%-1,0)=TRUE
    ENDIF
  NEXT i%
  ' set references to cur and new
  cur%=0
  new%=1
RETURN
'
' Display the world
'
PROCEDURE display_world
  LOCAL i%
  FOR i%=1 TO size%
    IF world!(i%-1,cur%)
      PRINT "#";
    ELSE
      PRINT ".";
    ENDIF
  NEXT i%
  PRINT ""
RETURN
'
' Create new version of world
'
PROCEDURE update_world
  LOCAL i%
  FOR i%=1 TO size%
    world!(i%-1,new%)=@new_state(@get_value(i%-1))
  NEXT i%
  ' reverse cur/new
  cur%=1-cur%
  new%=1-new%
RETURN
'
' Test if cur/new states are the same
'
FUNCTION same_state
  LOCAL i%
  FOR i%=1 TO size%
    IF world!(i%-1,cur%)<>world!(i%-1,new%)
      RETURN FALSE
    ENDIF
  NEXT i%
  RETURN TRUE
ENDFUNC
'
' Return new state of cell given value
'
FUNCTION new_state(value%)
  RETURN BTST(rules%,value%)
ENDFUNC
'
' Compute value for cell + neighbours
'
FUNCTION get_value(cell%)
  LOCAL result%
  result%=0
  IF cell%-1<0 ! check for wrapping at left
    IF world!(size%-1,cur%)
      result%=result%+4
    ENDIF
  ELSE ! no wrapping
    IF world!(cell%-1,cur%)
      result%=result%+4
    ENDIF
  ENDIF
  IF world!(cell%,cur%)
    result%=result%+2
  ENDIF
  IF cell%+1>size% ! check for wrapping at right
    IF world!(0,cur%)
      result%=result%+1
    ENDIF
  ELSE ! no wrapping
    IF world!(cell%+1,cur%)
      result%=result%+1
    ENDIF
  ENDIF
  RETURN result%
ENDFUNC

```



## Go


```go
package main

import (
    "fmt"
    "math/big"
    "math/rand"
    "strings"
)

func main() {
    const cells = 20
    const generations = 9
    fmt.Println("Single 1, rule 90:")
    a := big.NewInt(1)
    a.Lsh(a, cells/2)
    elem(90, cells, generations, a)
    fmt.Println("Random intial state, rule 30:")
    a = big.NewInt(1)
    a.Rand(rand.New(rand.NewSource(3)), a.Lsh(a, cells))
    elem(30, cells, generations, a)
}

func elem(rule uint, cells, generations int, a *big.Int) {
    output := func() {
        fmt.Println(strings.Replace(strings.Replace(
            fmt.Sprintf("%0*b", cells, a), "0", " ", -1), "1", "#", -1))
    }
    output()
    a1 := new(big.Int)
    set := func(cell int, k uint) {
        a1.SetBit(a1, cell, rule>>k&1)
    }
    last := cells - 1
    for r := 0; r < generations; r++ {
        k := a.Bit(last) | a.Bit(0)<<1 | a.Bit(1)<<2
        set(0, k)
        for c := 1; c < last; c++ {
            k = k>>1 | a.Bit(c+1)<<2
            set(c, k)
        }
        set(last, k>>1|a.Bit(0)<<2)
        a, a1 = a1, a
        output()
    }
}
```

{{out}}

```txt

Single 1, rule 90:
         #          
        # #         
       #   #        
      # # # #       
     #       #      
    # #     # #     
   #   #   #   #    
  # # # # # # # #   
 #               #  
# #             # # 
Random intial state, rule 30:
 #   # #  ####     #
 ## ## ####   #   ##
 #  #  #   # ### ## 
######### ## #   # #
          #  ## ## #
#        #####  #  #
 #      ##    ######
 ##    ## #  ##     
## #  ##  #### #    
#  #### ###    ##  #

```



## Haskell


===Array-based solution ===
Straight-forward implementation of CA on a cyclic domain, using imutable arrays:


```Haskell
import Data.Array (listArray, (!), bounds, elems)

step rule a = listArray (l,r) res
  where (l,r) = bounds a
        res = [rule (a!r)     (a!l) (a!(l+1)) ] ++
              [rule (a!(i-1)) (a!i) (a!(i+1)) | i <- [l+1..r-1] ] ++
              [rule (a!(r-1)) (a!r) (a!l)     ]

runCA rule = iterate (step rule)
```


The following gives decoding of the CA rule and prepares the initial CA state:

```Haskell
rule n l x r = n `div` (2^(4*l + 2*x + r)) `mod` 2

initial n = listArray (0,n-1) . center . padRight n
  where
    padRight n lst = take n $ lst ++ repeat 0
    center = take n . drop (n `div` 2+1) . cycle
```


Finally the IO stuff:

```Haskell
displayCA n rule init = mapM_ putStrLn $ take n result
  where result = fmap display . elems <$> runCA rule init
        display 0 = ' '
        display 1 = '*'
```


{{Out}}

```txt
λ> displayCA 40 (rule 90) (initial 40 [1])
                   *                    
                  * *                   
                 *   *                  
                * * * *                 
               *       *                
              * *     * *               
             *   *   *   *              
            * * * * * * * *             
           *               *            
          * *             * *           
         *   *           *   *          
        * * * *         * * * *         
       *       *       *       *        
      * *     * *     * *     * *       
     *   *   *   *   *   *   *   *      
    * * * * * * * * * * * * * * * *     
   *                               *    
  * *                             * *   
 *   *                           *   *  
* * * *                         * * * * 
       *                       *        
      * *                     * *       
     *   *                   *   *      
    * * * *                 * * * *     
   *       *               *       *    
  * *     * *             * *     * *   
 *   *   *   *           *   *   *   *  
* * * * * * * *         * * * * * * * * 
               *       *                
              * *     * *               
             *   *   *   *              
            * * * * * * * *             
           *               *            
          * *             * *           
         *   *           *   *          
        * * * *         * * * *         
       *       *       *       *        
      * *     * *     * *     * *       
     *   *   *   *   *   *   *   *      
    * * * * * * * * * * * * * * * *  
```



###  Comonadic solution 

This solution is more involved, but it is slightly more efficient than Array-based one. What is more important, this solution is guaranteed to be total and correct by type checker.

The cyclic CA domain is represented by an infinite ''zipper list''. First we provide the datatype, the viewer and constructor:


```Haskell
{-# LANGUAGE DeriveFunctor #-}

import Control.Comonad
import Data.InfList (InfList (..))
import qualified Data.InfList as Inf

data Cycle a = Cycle Int a a (InfList a) deriving Functor

view (Cycle n _ x r) = Inf.take n (x ::: r)

fromList []  = let a = a in Cycle 0 a a (Inf.repeat a)
-- zero cycle length ensures that elements of the empty cycle will never be accessed
fromList lst = let x:::r = Inf.cycle lst
               in Cycle (length lst) (last lst) x r
```


In order to run the CA on the domain we make it an instance of <code>Comonad</code> class. Running the CA turns to be just an iterative comonadic ''extension'' of the rule:


```Haskell
instance Comonad Cycle where
  extract (Cycle _ _ x _) = x
  duplicate x@(Cycle n _ _ _) = fromList $ take n $ iterate shift x
    where shift (Cycle n _ x (r:::rs)) = Cycle n x r rs

step rule  (Cycle _ l x (r:::_)) = rule l x r

runCA rule = iterate (=>> step rule)
```



Rule definition and I/O routine is the same as in Array-based solution:


```Haskell
rule n l x r = n `div` (2^(4*l + 2*x + r)) `mod` 2

initial n lst = fromList $ center $ padRight n lst
  where
    padRight n lst = take n $ lst ++ repeat 0
    center = take n . drop (n `div` 2+1) . cycle

displayCA n rule init = mapM_ putStrLn $ take n result
  where result = fmap display . view <$> runCA rule init
        display 0 = ' '
        display 1 = '*'
```


See also [[Elementary cellular automaton/Infinite length#Haskell]]


## J

[[File:J-Elementary_cellular_automaton-90.png|200px|thumb]]
We'll define a state transition mechanism, and then rely on the language for iteration and display:


```J
   next=: ((8$2) #: [) {~ 2 #. 1 - [: |: |.~"1 0&_1 0 1@]
   ' *'{~90 next^:(i.9) 0 0 0 0 0 0 1 0 0 0 0 0
      *     
     * *    
    *   *   
   * * * *  
  *       * 
 * *     * *
    *   *   
   * * * *  
  *       * 
```


Or, we can view this on a larger scale, graphically:


```J
   require'viewmat'
   viewmat 90 next^:(i.200) 0=i:200
```



## Java

{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.Timer;

public class WolframCA extends JPanel {
    final int[] ruleSet = {30, 45, 50, 57, 62, 70, 73, 75, 86, 89, 90, 99,
        101, 105, 109, 110, 124, 129, 133, 135, 137, 139, 141, 164,170, 232};
    byte[][] cells;
    int rule = 0;

    public WolframCA() {
        Dimension dim = new Dimension(900, 450);
        setPreferredSize(dim);
        setBackground(Color.white);
        setFont(new Font("SansSerif", Font.BOLD, 28));

        cells = new byte[dim.height][dim.width];
        cells[0][dim.width / 2] = 1;

        new Timer(5000, (ActionEvent e) -> {
            rule++;
            if (rule == ruleSet.length)
                rule = 0;
            repaint();
        }).start();
    }

    private byte rules(int lhs, int mid, int rhs) {
        int idx = (lhs << 2 | mid << 1 | rhs);
        return (byte) (ruleSet[rule] >> idx & 1);
    }

    void drawCa(Graphics2D g) {
        g.setColor(Color.black);
        for (int r = 0; r < cells.length - 1; r++) {
            for (int c = 1; c < cells[r].length - 1; c++) {
                byte lhs = cells[r][c - 1];
                byte mid = cells[r][c];
                byte rhs = cells[r][c + 1];
                cells[r + 1][c] = rules(lhs, mid, rhs); // next generation
                if (cells[r][c] == 1) {
                    g.fillRect(c, r, 1, 1);
                }
            }
        }
    }

    void drawLegend(Graphics2D g) {
        String s = String.valueOf(ruleSet[rule]);
        int sw = g.getFontMetrics().stringWidth(s);

        g.setColor(Color.white);
        g.fillRect(16, 5, 55, 30);

        g.setColor(Color.darkGray);
        g.drawString(s, 16 + (55 - sw) / 2, 30);
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawCa(g);
        drawLegend(g);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Wolfram CA");
            f.setResizable(false);
            f.add(new WolframCA(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```

[[File:ca java.png|900px]]


## jq

{{works with|jq|1.5}}

For simplicity we will use strings of 0s and 1s to represent the
automaton, its states, and the rules, except that the "automaton"
function will accept decimal rule specifications.

'''Helper functions'''

```jq
# The ordinal value of the relevant states:
def states:
  {"111": 1, "110": 2, "101": 3, "100": 4, "011": 5, "010": 6, "001": 7, "000": 8};

# Compute the next "state"
# input: a state ("111" or "110" ...)
# rule: the rule represented as a string of 0s and 1s 
# output: the next state "0" or "1" depending on the rule
def next(rule):
  states[.] as $n | rule[($n-1):$n] ;

# The state of cell $n, using 0-based indexing
def triple($n):
  if $n == 0 then .[-1:] + .[0:2]
  elif $n == (length-1) then .[-2:] + .[0:1]
  else .[$n-1:$n+2]
  end;

# input: non-negative decimal integer
# output: 0-1 binary string
def binary_digits:
  if . == 0 then "0"
  else [recurse( if . == 0 then empty else ./2 | floor end ) % 2 | tostring]
    | reverse
    | .[1:] # remove the leading 0
    | join("")
  end ;
```


'''Main function'''

```jq
# "rule" can be given as a decimal or string of 0s and 1s:
def automaton(rule; steps):

  # Compute the rule as a string of length 8
  def tos:
    if type == "number" then "0000000" + binary_digits else . end
    | .[-8:];

  # input: the current state of the automaton
  # output: its next state
  def update(rule):
    . as $in
    | reduce range(0; length) as $n ("";
      . + ($in|triple($n)|next(rule)));

  (rule | tos) as $rule
  | limit(steps; while(true; update($rule) )) ;

# Example

"0000001000000"             # initial state
| automaton($rule; $steps)  # $rule and $steps are taken from the command line
| gsub("0"; ".")            # pretty print
| gsub("1"; "#")

```



'''Command-line Invocation'''
    $ jq -r -n -f program.jq --argjson steps 10 --argjson rule 90

{{out}}

```txt
"......#......"
".....#.#....."
"....#...#...."
"...#.#.#.#..."
"..#.......#.."
".#.#.....#.#."
"#...#...#...#"
"##.#.#.#.#.##"
".#.........#."
"#.#.......#.#"
```



## Julia


```julia

const lines = 10
const start = ".........#........."
const rules = [90, 30, 14]
 
rule2poss(rule) = [rule & (1 << (i - 1)) != 0 for i in 1:8]
 
cells2bools(cells) = [cells[i] == '#' for i in 1:length(cells)]
 
bools2cells(bset) = prod([bset[i] ? "#" : "." for i in 1:length(bset)])
 
function transform(bset, ruleposs)
    newbset = map(x->ruleposs[x],
        [bset[i - 1] * 4 + bset[i] * 2 + bset[i + 1] + 1
        for i in 2:length(bset)-1])
    vcat(newbset[end], newbset, newbset[1])
end
 
const startset = cells2bools(start)
 
for rul in rules
    println("\nUsing Rule $rul:")
    bset = vcat(startset[end], startset, startset[1]) # wrap ends
    rp = rule2poss(rul)
    for _ in 1:lines
        println(bools2cells(bset[2:end-1]))  # unwrap ends
        bset = transform(bset, rp)
    end
end

```
 {{output}} 
```txt

Using Rule 90:
 .........#.........
 ........#.#........
 .......#...#.......
 ......#.#.#.#......
 .....#.......#.....
 ....#.#.....#.#....
 ...#...#...#...#...
 ..#.#.#.#.#.#.#.#..
 .#...............#.
 #.#.............#.#

Using Rule 30:
 .........#.........
 ........###........
 .......##..#.......
 ......##.####......
 .....##..#...#.....
 ....##.####.###....
 ...##..#....#..#...
 ..##.####..######..
 .##..#...###.....#.
 ##.####.##..#...###

Using Rule 14:
 .........#.........
 ........##.........
 .......##..........
 ......##...........
 .....##............
 ....##.............
 ...##..............
 ..##...............
 .##................
 ##.................

```



## Kotlin

{{trans|C++}}

```scala
// version 1.1.51

import java.util.BitSet

const val SIZE  = 32
const val LINES = SIZE / 2
const val RULE  = 90

fun ruleTest(x: Int) = (RULE and (1 shl (7 and x))) != 0

infix fun Boolean.shl(bitCount: Int) = (if (this) 1 else 0) shl bitCount

fun Boolean.toInt() = if (this) 1 else 0

fun evolve(s: BitSet) {
    val t = BitSet(SIZE)  // all false by default
    t[SIZE - 1] = ruleTest((s[0] shl 2) or (s[SIZE - 1] shl 1) or s[SIZE - 2].toInt())
    t[0] = ruleTest((s[1] shl 2) or (s[0] shl 1) or s[SIZE - 1].toInt())
    for (i in 1 until SIZE - 1) {
        t[i] = ruleTest((s[i + 1] shl 2) or (s[i] shl 1) or s[i - 1].toInt())
    }
    for (i in 0 until SIZE) s[i] = t[i]
}

fun show(s: BitSet) {
    for (i in SIZE - 1 downTo 0) print(if (s[i]) "*" else " ")
    println()
}

fun main(args: Array<String>) {
    var state = BitSet(SIZE)
    state.set(LINES)
    println("Rule $RULE:")
    repeat(LINES) {
        show(state)
        evolve(state)
    }
}
```


{{out}}

```txt

Rule 90:
               *                
              * *               
             *   *              
            * * * *             
           *       *            
          * *     * *           
         *   *   *   *          
        * * * * * * * *         
       *               *        
      * *             * *       
     *   *           *   *      
    * * * *         * * * *     
   *       *       *       *    
  * *     * *     * *     * *   
 *   *   *   *   *   *   *   *  
* * * * * * * * * * * * * * * * 

```



## Mathematica

Mathematica provides built-in functions for cellular automata. For example visualizing the first 100 rows of rule 30 on an 8-bit grid with a single initial cell:


```Mathematica

ArrayPlot[CellularAutomaton[30, {0, 0, 0, 0, 1, 0, 0, 0}, 100]]

```



## MATLAB


```MATLAB
function init = cellularAutomaton(rule, init, n)
  init(n + 1, :) = 0;
  for k = 1 : n
    init(k + 1, :) = bitget(rule, 1 + filter2([4 2 1], init(k, :)));
  end
```

{{out}}

```MATLAB>>
  char(cellularAutomaton(90, ~(-15:15), 15) * 10 + 32)
ans =
               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *
```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;

package Automaton {
    sub new {
	my $class = shift;
	my $rule = [ reverse split //, sprintf "%08b", shift ];
	return bless { rule => $rule, cells => [ @_ ] }, $class;
    }
    sub next {
	my $this = shift;
	my @previous = @{$this->{cells}};
	$this->{cells} = [
	    @{$this->{rule}}[
	    map {
	      4*$previous[($_ - 1) % @previous]
	    + 2*$previous[$_]
	    +   $previous[($_ + 1) % @previous]
	    } 0 .. @previous - 1
	    ]
	];
	return $this;
    }
    use overload
    q{""} => sub {
	my $this = shift;
	join '', map { $_ ? '#' : ' ' } @{$this->{cells}}
    };
}

my @a = map 0, 1 .. 91; $a[45] = 1;
my $a = Automaton->new(90, @a);

for (1..40) {
    print "|$a|\n"; $a->next;
}
```
 
{{out}}

```txt
|                                             #                                             |
|                                            # #                                            |
|                                           #   #                                           |
|                                          # # # #                                          |
|                                         #       #                                         |
|                                        # #     # #                                        |
|                                       #   #   #   #                                       |
|                                      # # # # # # # #                                      |
|                                     #               #                                     |
|                                    # #             # #                                    |
|                                   #   #           #   #                                   |
|                                  # # # #         # # # #                                  |
|                                 #       #       #       #                                 |
|                                # #     # #     # #     # #                                |
|                               #   #   #   #   #   #   #   #                               |
|                              # # # # # # # # # # # # # # # #                              |
|                             #                               #                             |
|                            # #                             # #                            |
|                           #   #                           #   #                           |
|                          # # # #                         # # # #                          |
|                         #       #                       #       #                         |
|                        # #     # #                     # #     # #                        |
|                       #   #   #   #                   #   #   #   #                       |
|                      # # # # # # # #                 # # # # # # # #                      |
|                     #               #               #               #                     |
|                    # #             # #             # #             # #                    |
|                   #   #           #   #           #   #           #   #                   |
|                  # # # #         # # # #         # # # #         # # # #                  |
|                 #       #       #       #       #       #       #       #                 |
|                # #     # #     # #     # #     # #     # #     # #     # #                |
|               #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #               |
|              # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #              |
|             #                                                               #             |
|            # #                                                             # #            |
|           #   #                                                           #   #           |
|          # # # #                                                         # # # #          |
|         #       #                                                       #       #         |
|        # #     # #                                                     # #     # #        |
|       #   #   #   #                                                   #   #   #   #       |
|      # # # # # # # #                                                 # # # # # # # #      |
```



## Perl 6


Using the <tt>Automaton</tt> class defined at [[One-dimensional_cellular_automata#Perl_6]]:


```perl6
class Automaton {
    has $.rule;
    has @.cells;
    has @.code = $!rule.fmt('%08b').flip.comb».Int;
 
    method gist { "|{ @!cells.map({+$_ ?? '#' !! ' '}).join }|" }
 
    method succ {
        self.new: :$!rule, :@!code, :cells( 
            @!code[
                    4 «*« @!cells.rotate(-1)
                »+« 2 «*« @!cells
                »+«       @!cells.rotate(1)
            ]
        )
    }
}

my @padding = 0 xx 10;

my Automaton $a .= new:
    :rule(30),
    :cells(flat @padding, 1, @padding);

say $a++ for ^10;
```


{{out}}

```txt

|          #          |
|         ###         |
|        ##  #        |
|       ## ####       |
|      ##  #   #      |
|     ## #### ###     |
|    ##  #    #  #    |
|   ## ####  ######   |
|  ##  #   ###     #  |
| ## #### ##  #   ### |

```



## Phix

String-based solution

```Phix
string s = ".........#.........", t=s, r = "........"
integer rule = 90, k, l = length(s)
for i=1 to 8 do
    r[i] = iff(mod(rule,2)?'#':'.')
    rule = floor(rule/2)
end for
for i=0 to 50 do
    ?s
    for j=1 to l do
        k = (s[iff(j=1?l:j-1)]='#')*4
          + (s[          j   ]='#')*2
          + (s[iff(j=l?1:j+1)]='#')+1
        t[j] = r[k]
    end for
    s = t
end for
```

Output matches that of D and Python:wrap for rule = 90, 30, 122 (if you edit/run 3 times)


## PicoLisp


```PicoLisp
(de dictionary (N)
   (extract
      '((A B)
         (and
            (= "1" B)
            (mapcar
               '((L) (if (= "1" L) "#" "."))
               A ) ) )
      (mapcar
         '((N) (chop (pad 3 (bin N))))
         (range 7 0) )
      (chop (pad 8 (bin N))) ) )
(de cellular (Lst N)
   (let (Lst (chop Lst)  D (dictionary N))
      (do 10
         (prinl Lst)
         (setq Lst
            (make
               (map
                  '((L)
                     (let Y (head 3 L)
                        (and
                           (cddr Y)
                           (link (if (member Y D) "#" ".")) ) ) )
                  (conc (cons (last Lst)) Lst (cons (car Lst))) ) ) ) ) ) )
(cellular
   ".........#........."
   90 )
```

{{out}}

```txt

.........#.........
........#.#........
.......#...#.......
......#.#.#.#......
.....#.......#.....
....#.#.....#.#....
...#...#...#...#...
..#.#.#.#.#.#.#.#..
.#...............#.
#.#.............#.#

```



## Prolog


```prolog
play :-	initial(I), do_auto(50, I).

initial([0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0]).

do_auto(0, _) :- !.
do_auto(N, I) :- 
	maplist(writ, I), nl,
	apply_rules(I, Next),
	succ(N1, N),
	do_auto(N1, Next).

r(0,0,0,0). 
r(0,0,1,1). 
r(0,1,0,0). 
r(0,1,1,1). 
r(1,0,0,1). 
r(1,0,1,0). 
r(1,1,0,1). 
r(1,1,1,0).

apply_rules(In, Out) :-
	apply1st(In, First),
	Out = [First|_],
	apply(In, First, First, Out).

apply1st([A,B|T], A1) :-                            last([A,B|T], Last), r(Last,A,B,A1).
apply([A,B], Prev, First, [Prev, This]) :-          r(A,B,First,This).
apply([A,B,C|T], Prev, First, [Prev,This|Rest]) :-  r(A,B,C,This), apply([B,C|T], This, First, [This|Rest]).

writ(0) :- write('.').
writ(1) :- write(1).
```



## Python


### Python: Zero padded

Note: This only fitted the original task description that read:
:''You can deal with the limit conditions (what happens on the borders of the space) in any way you please.''


```python
def eca(cells, rule):
    lencells = len(cells)
    c = "0" + cells + "0"    # Zero pad the ends
    rulebits = '{0:08b}'.format(rule)
    neighbours2next = {'{0:03b}'.format(n):rulebits[::-1][n] for n in range(8)}
    yield c[1:-1]
    while True:
        c = ''.join(['0',
                     ''.join(neighbours2next[c[i-1:i+2]]
                             for i in range(1,lencells+1)),
                     '0'])
        yield c[1:-1]

if __name__ == '__main__':
    lines, start, rules = 50, '0000000001000000000', (90, 30, 122)
    zipped = [range(lines)] + [eca(start, rule) for rule in rules]
    print('\n   Rules: %r' % (rules,))
    for data in zip(*zipped):
        i = data[0]
        cells = data[1:]
        print('%2i: %s' % (i, '    '.join(cells).replace('0', '.').replace('1', '#')))
```


{{out}}
(Note how Rule 30 does '''not''' look random).

```txt
   Rules: (90, 30, 122)
 0: .........#.........    .........#.........    .........#.........
 1: ........#.#........    ........###........    ........#.#........
 2: .......#...#.......    .......##..#.......    .......#.#.#.......
 3: ......#.#.#.#......    ......##.####......    ......#.#.#.#......
 4: .....#.......#.....    .....##..#...#.....    .....#.#.#.#.#.....
 5: ....#.#.....#.#....    ....##.####.###....    ....#.#.#.#.#.#....
 6: ...#...#...#...#...    ...##..#....#..#...    ...#.#.#.#.#.#.#...
 7: ..#.#.#.#.#.#.#.#..    ..##.####..######..    ..#.#.#.#.#.#.#.#..
 8: .#...............#.    .##..#...###.....#.    .#.#.#.#.#.#.#.#.#.
 9: #.#.............#.#    ##.####.##..#...###    #.#.#.#.#.#.#.#.#.#
10: ...#...........#...    #..#....#.####.##..    .#.#.#.#.#.#.#.#.#.
11: ..#.#.........#.#..    #####..##.#....#.#.    #.#.#.#.#.#.#.#.#.#
12: .#...#.......#...#.    #....###..##..##.##    .#.#.#.#.#.#.#.#.#.
13: #.#.#.#.....#.#.#.#    ##..##..###.###..#.    #.#.#.#.#.#.#.#.#.#
14: .......#...#.......    #.###.###...#..####    .#.#.#.#.#.#.#.#.#.
15: ......#.#.#.#......    #.#...#..#.#####...    #.#.#.#.#.#.#.#.#.#
16: .....#.......#.....    #.##.#####.#....#..    .#.#.#.#.#.#.#.#.#.
17: ....#.#.....#.#....    #.#..#.....##..###.    #.#.#.#.#.#.#.#.#.#
18: ...#...#...#...#...    #.#####...##.###..#    .#.#.#.#.#.#.#.#.#.
19: ..#.#.#.#.#.#.#.#..    #.#....#.##..#..###    #.#.#.#.#.#.#.#.#.#
20: .#...............#.    #.##..##.#.######..    .#.#.#.#.#.#.#.#.#.
21: #.#.............#.#    #.#.###..#.#.....#.    #.#.#.#.#.#.#.#.#.#
22: ...#...........#...    #.#.#..###.##...###    .#.#.#.#.#.#.#.#.#.
23: ..#.#.........#.#..    #.#.####...#.#.##..    #.#.#.#.#.#.#.#.#.#
24: .#...#.......#...#.    #.#.#...#.##.#.#.#.    .#.#.#.#.#.#.#.#.#.
25: #.#.#.#.....#.#.#.#    #.#.##.##.#..#.#.##    #.#.#.#.#.#.#.#.#.#
26: .......#...#.......    #.#.#..#..####.#.#.    .#.#.#.#.#.#.#.#.#.
27: ......#.#.#.#......    #.#.#######....#.##    #.#.#.#.#.#.#.#.#.#
28: .....#.......#.....    #.#.#......#..##.#.    .#.#.#.#.#.#.#.#.#.
29: ....#.#.....#.#....    #.#.##....#####..##    #.#.#.#.#.#.#.#.#.#
30: ...#...#...#...#...    #.#.#.#..##....###.    .#.#.#.#.#.#.#.#.#.
31: ..#.#.#.#.#.#.#.#..    #.#.#.####.#..##..#    #.#.#.#.#.#.#.#.#.#
32: .#...............#.    #.#.#.#....####.###    .#.#.#.#.#.#.#.#.#.
33: #.#.............#.#    #.#.#.##..##....#..    #.#.#.#.#.#.#.#.#.#
34: ...#...........#...    #.#.#.#.###.#..###.    .#.#.#.#.#.#.#.#.#.
35: ..#.#.........#.#..    #.#.#.#.#...####..#    #.#.#.#.#.#.#.#.#.#
36: .#...#.......#...#.    #.#.#.#.##.##...###    .#.#.#.#.#.#.#.#.#.
37: #.#.#.#.....#.#.#.#    #.#.#.#.#..#.#.##..    #.#.#.#.#.#.#.#.#.#
38: .......#...#.......    #.#.#.#.####.#.#.#.    .#.#.#.#.#.#.#.#.#.
39: ......#.#.#.#......    #.#.#.#.#....#.#.##    #.#.#.#.#.#.#.#.#.#
40: .....#.......#.....    #.#.#.#.##..##.#.#.    .#.#.#.#.#.#.#.#.#.
41: ....#.#.....#.#....    #.#.#.#.#.###..#.##    #.#.#.#.#.#.#.#.#.#
42: ...#...#...#...#...    #.#.#.#.#.#..###.#.    .#.#.#.#.#.#.#.#.#.
43: ..#.#.#.#.#.#.#.#..    #.#.#.#.#.####...##    #.#.#.#.#.#.#.#.#.#
44: .#...............#.    #.#.#.#.#.#...#.##.    .#.#.#.#.#.#.#.#.#.
45: #.#.............#.#    #.#.#.#.#.##.##.#.#    #.#.#.#.#.#.#.#.#.#
46: ...#...........#...    #.#.#.#.#.#..#..#.#    .#.#.#.#.#.#.#.#.#.
47: ..#.#.........#.#..    #.#.#.#.#.#######.#    #.#.#.#.#.#.#.#.#.#
48: .#...#.......#...#.    #.#.#.#.#.#.......#    .#.#.#.#.#.#.#.#.#.
49: #.#.#.#.....#.#.#.#    #.#.#.#.#.##.....##    #.#.#.#.#.#.#.#.#.#
```



### Python: wrap

The ends of the cells wrap-around.

```python
def eca_wrap(cells, rule):
    lencells = len(cells)
    rulebits = '{0:08b}'.format(rule)
    neighbours2next = {tuple('{0:03b}'.format(n)):rulebits[::-1][n] for n in range(8)}
    c = cells
    while True:
        yield c
        c = ''.join(neighbours2next[(c[i-1], c[i], c[(i+1) % lencells])] for i in range(lencells))

if __name__ == '__main__':
    lines, start, rules = 50, '0000000001000000000', (90, 30, 122)
    zipped = [range(lines)] + [eca_wrap(start, rule) for rule in rules]
    print('\n   Rules: %r' % (rules,))
    for data in zip(*zipped):
        i = data[0]
        cells = data[1:]
        print('%2i: %s' % (i, '    '.join(cells).replace('0', '.').replace('1', '#')))

```


{{out}}

```txt
   Rules: (90, 30, 122)
 0: .........#.........    .........#.........    .........#.........
 1: ........#.#........    ........###........    ........#.#........
 2: .......#...#.......    .......##..#.......    .......#.#.#.......
 3: ......#.#.#.#......    ......##.####......    ......#.#.#.#......
 4: .....#.......#.....    .....##..#...#.....    .....#.#.#.#.#.....
 5: ....#.#.....#.#....    ....##.####.###....    ....#.#.#.#.#.#....
 6: ...#...#...#...#...    ...##..#....#..#...    ...#.#.#.#.#.#.#...
 7: ..#.#.#.#.#.#.#.#..    ..##.####..######..    ..#.#.#.#.#.#.#.#..
 8: .#...............#.    .##..#...###.....#.    .#.#.#.#.#.#.#.#.#.
 9: #.#.............#.#    ##.####.##..#...###    #.#.#.#.#.#.#.#.#.#
10: #..#...........#..#    ...#....#.####.##..    ##.#.#.#.#.#.#.#.##
11: ###.#.........#.###    ..###..##.#....#.#.    .##.#.#.#.#.#.#.##.
12: ..#..#.......#..#..    .##..###..##..##.##    ####.#.#.#.#.#.####
13: .#.##.#.....#.##.#.    .#.###..###.###..#.    ...##.#.#.#.#.##...
14: #..##..#...#..##..#    ##.#..###...#..####    ..####.#.#.#.####..
15: #######.#.#.#######    ...####..#.#####...    .##..##.#.#.##..##.
16: ......#.....#......    ..##...###.#....#..    ########.#.########
17: .....#.#...#.#.....    .##.#.##...##..###.    .......##.##.......
18: ....#...#.#...#....    ##..#.#.#.##.###..#    ......#######......
19: ...#.#.#...#.#.#...    ..###.#.#.#..#..###    .....##.....##.....
20: ..#.....#.#.....#..    ###...#.#.#######..    ....####...####....
21: .#.#...#...#...#.#.    #..#.##.#.#......##    ...##..##.##..##...
22: #...#.#.#.#.#.#...#    .###.#..#.##....##.    ..###############..
23: ##.#...........#.##    ##...####.#.#..##.#    .##.............##.
24: .#..#.........#..#.    ..#.##....#.####..#    ####...........####
25: #.##.#.......#.##.#    ###.#.#..##.#...###    ...##.........##...
26: #.##..#.....#..##.#    ....#.####..##.##..    ..####.......####..
27: #.####.#...#.####.#    ...##.#...###..#.#.    .##..##.....##..##.
28: #.#..#..#.#..#..#.#    ..##..##.##..###.##    ########...########
29: #..##.##...##.##..#    ###.###..#.###...#.    .......##.##.......
30: #####.###.###.#####    #...#..###.#..#.##.    ......#######......
31: ....#.#.#.#.#.#....    ##.#####...####.#..    .....##.....##.....
32: ...#...........#...    #..#....#.##....###    ....####...####....
33: ..#.#.........#.#..    .####..##.#.#..##..    ...##..##.##..##...
34: .#...#.......#...#.    ##...###..#.####.#.    ..###############..
35: #.#.#.#.....#.#.#.#    #.#.##..###.#....#.    .##.............##.
36: #......#...#......#    #.#.#.###...##..##.    ####...........####
37: ##....#.#.#.#....##    #.#.#.#..#.##.###..    ...##.........##...
38: .##..#.......#..##.    #.#.#.####.#..#..##    ..####.......####..
39: #####.#.....#.#####    ..#.#.#....#######.    .##..##.....##..##.
40: ....#..#...#..#....    .##.#.##..##......#    ########...########
41: ...#.##.#.#.##.#...    .#..#.#.###.#....##    .......##.##.......
42: ..#..##.....##..#..    .####.#.#...##..##.    ......#######......
43: .#.#####...#####.#.    ##....#.##.##.###.#    .....##.....##.....
44: #..#...##.##...#..#    ..#..##.#..#..#...#    ....####...####....
45: ###.#.###.###.#.###    ######..########.##    ...##..##.##..##...
46: ..#...#.#.#.#...#..    ......###........#.    ..###############..
47: .#.#.#.......#.#.#.    .....##..#......###    .##.............##.
48: #.....#.....#.....#    #...##.####....##..    ####...........####
49: ##...#.#...#.#...##    ##.##..#...#..##.##    ...##.........##...
```



### Python: Infinite

Note: This only fitted the original task description that read:
:''You can deal with the limit conditions (what happens on the borders of the space) in any way you please.''

Pad and extend with inverse of end cells on each iteration. 

```python
def _notcell(c):
    return '0' if c == '1' else '1'

def eca_infinite(cells, rule):
    lencells = len(cells)
    rulebits = '{0:08b}'.format(rule)
    neighbours2next = {'{0:03b}'.format(n):rulebits[::-1][n] for n in range(8)}
    c = cells
    while True:
        yield c
        c = _notcell(c[0])*2 + c + _notcell(c[-1])*2    # Extend and pad the ends

        c = ''.join(neighbours2next[c[i-1:i+2]] for i in range(1,len(c) - 1))
        #yield c[1:-1]

if __name__ == '__main__':
    lines, start, rules = 20, '1', (90, 30, 122)
    zipped = [range(lines)] + [eca_infinite(start, rule) for rule in rules]
    print('\n   Rules: %r' % (rules,))
    for data in zip(*zipped):
        i = data[0]
        cells = ['%s%s%s' % (' '*(lines - i), c, ' '*(lines - i)) for c in data[1:]]
        print('%2i: %s' % (i, '    '.join(cells).replace('0', '.').replace('1', '#')))
```


{{out}}

```txt
   Rules: (90, 30, 122)
 0:                          #                                                      #                                                      #                         
 1:                         #.#                                                    ###                                                    #.#                        
 2:                        #...#                                                  ##..#                                                  #.#.#                       
 3:                       #.#.#.#                                                ##.####                                                #.#.#.#                      
 4:                      #.......#                                              ##..#...#                                              #.#.#.#.#                     
 5:                     #.#.....#.#                                            ##.####.###                                            #.#.#.#.#.#                    
 6:                    #...#...#...#                                          ##..#....#..#                                          #.#.#.#.#.#.#                   
 7:                   #.#.#.#.#.#.#.#                                        ##.####..######                                        #.#.#.#.#.#.#.#                  
 8:                  #...............#                                      ##..#...###.....#                                      #.#.#.#.#.#.#.#.#                 
 9:                 #.#.............#.#                                    ##.####.##..#...###                                    #.#.#.#.#.#.#.#.#.#                
10:                #...#...........#...#                                  ##..#....#.####.##..#                                  #.#.#.#.#.#.#.#.#.#.#               
11:               #.#.#.#.........#.#.#.#                                ##.####..##.#....#.####                                #.#.#.#.#.#.#.#.#.#.#.#              
12:              #.......#.......#.......#                              ##..#...###..##..##.#...#                              #.#.#.#.#.#.#.#.#.#.#.#.#             
13:             #.#.....#.#.....#.#.....#.#                            ##.####.##..###.###..##.###                            #.#.#.#.#.#.#.#.#.#.#.#.#.#            
14:            #...#...#...#...#...#...#...#                          ##..#....#.###...#..###..#..#                          #.#.#.#.#.#.#.#.#.#.#.#.#.#.#           
15:           #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#                        ##.####..##.#..#.#####..#######                        #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#          
16:          #...............................#                      ##..#...###..####.#....###......#                      #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#         
17:         #.#.............................#.#                    ##.####.##..###....##..##..#....###                    #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#        
18:        #...#...........................#...#                  ##..#....#.###..#..##.###.####..##..#                  #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#       
19:       #.#.#.#.........................#.#.#.#                ##.####..##.#..######..#...#...###.####                #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#      
20:      #.......#.......................#.......#              ##..#...###..####.....####.###.##...#...#              #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#     
21:     #.#.....#.#.....................#.#.....#.#            ##.####.##..###...#...##....#...#.#.###.###            #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#    
22:    #...#...#...#...................#...#...#...#          ##..#....#.###..#.###.##.#..###.##.#.#...#..#          #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#   
23:   #.#.#.#.#.#.#.#.................#.#.#.#.#.#.#.#        ##.####..##.#..###.#...#..####...#..#.##.######        #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#  
24:  #...............#...............#...............#      ##..#...###..####...##.#####...#.#####.#..#.....#      #.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.# 
```



## Racket


This is the base code for the three elementary CA tasks. The "wrap" code
is a little over-complicated for the simple cases of wrapping on word
boundaries and for CA's with a narrower word. However, it will be used
unmodified for [[Elementary cellular automaton/Infinite length]].


```racket
#lang racket
(require racket/fixnum)
(provide usable-bits/fixnum usable-bits/fixnum-1 CA-next-generation
         wrap-rule-truncate-left-word show-automaton)

(define usable-bits/fixnum 30)
(define usable-bits/fixnum-1 (sub1 usable-bits/fixnum))
(define usable-bits/mask (fx- (fxlshift 1 usable-bits/fixnum) 1))
(define 2^u-b-1 (fxlshift 1 usable-bits/fixnum-1))
(define (fxior3 a b c) (fxior (fxior a b) c))
(define (if-bit-set n i [result 1]) (if (bitwise-bit-set? n i) result 0))

(define (shift-right-1-bit-with-lsb-L L n)
  (fxior (if-bit-set L 0 2^u-b-1) (fxrshift n 1)))

(define (shift-left-1-bit-with-msb-R n R)
  (fxior (fxand usable-bits/mask (fxlshift n 1))
         (if-bit-set R usable-bits/fixnum-1)))

(define ((CA-next-bit-state rule) L n R)
  (for/fold ([n+ 0])
            ([b (in-range usable-bits/fixnum-1 -1 -1)])
    (define rule-bit (fxior3 (if-bit-set (shift-right-1-bit-with-lsb-L L n) b 4)
                             (if-bit-set n b 2)
                             (if-bit-set (shift-left-1-bit-with-msb-R n R) b)))
    (fxior (fxlshift n+ 1) (if-bit-set rule rule-bit))))

;; CA-next-generation generates a function which takes:
;;  v-in   : an fxvector representing the CA's current state as a bit field. This may be mutated
;;  offset : the offset of the leftmost element of v-in; this is used in infinite CA to allow the CA
;;           to occupy negative indices
;;  wrap-rule : provided for automata that are not an integer number of usable-bits/fixnum bits wide
;;  wrap-rule = #f - v-in and offset are unchanged
;;  wrap-rule : (v-in vl-1 offset) -> (values v-out vl-1+ offset-)
;;             v-in as passed into CA-next-generation
;;             vl-1=(sub1 (length v-in)), since its precomputed vaule is needed
;;             offset as passed into CA-next-generation
;;             v-out: either a new copy of v-in, or v-in itself (which might be mutated)
;;             vl-1+: (sub1 (length v-out))
;;             offset- : a new value for offset (it will have decreased since the CA grows to the left
;;             with offset, and to the right with (length v-out)
(define (CA-next-generation rule #:wrap-rule (wrap-rule values))
  (define next-state (CA-next-bit-state rule))
  (lambda (v-in offset)
    (define vl-1 (fx- (fxvector-length v-in) 1))
    (define-values [v+ v+l-1 offset-] (wrap-rule v-in vl-1 offset))
    (define rv
      (for/fxvector ([l (in-sequences (in-value (fxvector-ref v+ v+l-1)) (in-fxvector v+))]
                     [n (in-fxvector v+)]
                     [r (in-sequences (in-fxvector v+ 1) (in-value (fxvector-ref v+ 0)))])
        (next-state l n r)))
    (values rv offset-)))

;; CA-next-generation with the default (non) wrap rule wraps the MSB of the left-hand word (L) and the
;; LSB of the right-hand word (R) in the CA. If the CA is not a multiple of usable-bits/fixnum wide,
;; then we use this function to put these bits where they can be used... i.e. the actual MSB is copied
;; to the word's MSB and the LSB is copied to the bit that is to the left of the actual MSB.
(define (wrap-rule-truncate-left-word sig-bits)
  (define wlb-mask (fx- (fxlshift 1 sig-bits) 1))
  (unless (fx< sig-bits (fx- usable-bits/fixnum 1))
    (error "we need at least 2 bits in the top of the word to do this safely"))
  (lambda (v-in vl-1 offset)
    (define v0 (fxvector-ref v-in 0))
    ;; this must wrap to wlb of the first word
    (define last-bit (fxlshift (fxand 1 (fxvector-ref v-in vl-1)) sig-bits))
    ;; this must wrap to the extreme left of the first word
    (define first-bit (if-bit-set v0 (fx- sig-bits 1) 2^u-b-1))
    (fxvector-set! v-in 0 (fxior3 last-bit first-bit (fxand v0 wlb-mask)))
    (values v-in vl-1 offset)))

;; This displays a state of the CA
(define (show-automaton v #:step (step #f) #:sig-bits (sig-bits #f) #:push-right (push-right #f))
  (when step (printf "[~a] " (~a #:align 'right #:width 10 step)))
  (when push-right (display (make-string (* usable-bits/fixnum push-right) #\.)))
  (when (number? sig-bits)
    (display (~a #:width sig-bits #:align 'right #:pad-string "0"
                 (number->string (fxvector-ref v 0) 2))))
  (for ([n (in-fxvector v (if sig-bits 1 0))])
    (display (~a #:width usable-bits/fixnum #:align 'right #:pad-string "0" (number->string n 2)))))

(module+ main
  (define ng/122/19-bits (CA-next-generation 122 #:wrap-rule (wrap-rule-truncate-left-word 19)))
  (for/fold ([v (fxvector #b1000000000)] [o 0]) ([step (in-range 40)])
    (show-automaton v #:step step #:sig-bits 19)
    (newline)
    (ng/122/19-bits v o)))
```


{{out}}


```txt
[         0] 0000000001000000000
[         1] 0000000010100000000
[         2] 0000000101010000000
[         3] 0000001010101000000
[         4] 0000010101010100000
[         5] 0000101010101010000
[         6] 0001010101010101000
[         7] 0010101010101010100
[         8] 0101010101010101010
[         9] 1010101010101010101
[        10] 1100000001111010101
[        11] 1100000001101101010
[        12] 1111010101010101111
[        13] 1100000001100011010
[        14] 0011110101010111100
[        15] 0110011010101100110
[        16] 1111111101011111111
[        17] 1100000001100000001
[        18] 0000001111111000000
[        19] 0000011000001100000
[        20] 0000111100011110000
[        21] 0001100110110011000
[        22] 0011111111111111100
[        23] 0110000000000000110
[        24] 1111000000000001111
[        25] 1100000001100011000
[        26] 0011110000000111100
[        27] 0110011000001100110
[        28] 1111111100011111111
[        29] 1100000001100000001
[        30] 0000001111111000000
[        31] 0000011000001100000
[        32] 0000111100011110000
[        33] 0001100110110011000
[        34] 0011111111111111100
[        35] 0110000000000000110
[        36] 1111000000000001111
[        37] 1100000001100011000
[        38] 0011110000000111100
[        39] 0110011000001100110
#fx(522495)
0
```



## Ruby


```ruby
class ElemCellAutomat
  include Enumerable
  
  def initialize (start_str, rule, disp=false)
    @cur = start_str
    @patterns = Hash[8.times.map{|i|["%03b"%i, "01"[rule[i]]]}]
    puts "Rule (#{rule}) : #@patterns" if disp
  end
  
  def each
    return to_enum unless block_given?
    loop do
      yield @cur
      str = @cur[-1] + @cur + @cur[0]
      @cur = @cur.size.times.map {|i| @patterns[str[i,3]]}.join
    end
  end
  
end

eca = ElemCellAutomat.new('1'.center(39, "0"), 18, true)
eca.take(30).each{|line| puts line.tr("01", ".#")}
```

{{out}}

```txt

Rule (18) : {"000"=>"0", "001"=>"1", "010"=>"0", "011"=>"0", "100"=>"1", "101"=>"0", "110"=>"0", "111"=>"0"}
...................#...................
..................#.#..................
.................#...#.................
................#.#.#.#................
...............#.......#...............
..............#.#.....#.#..............
.............#...#...#...#.............
............#.#.#.#.#.#.#.#............
...........#...............#...........
..........#.#.............#.#..........
.........#...#...........#...#.........
........#.#.#.#.........#.#.#.#........
.......#.......#.......#.......#.......
......#.#.....#.#.....#.#.....#.#......
.....#...#...#...#...#...#...#...#.....
....#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#....
...#...............................#...
..#.#.............................#.#..
.#...#...........................#...#.
#.#.#.#.........................#.#.#.#
.......#.......................#.......
......#.#.....................#.#......
.....#...#...................#...#.....
....#.#.#.#.................#.#.#.#....
...#.......#...............#.......#...
..#.#.....#.#.............#.#.....#.#..
.#...#...#...#...........#...#...#...#.
#.#.#.#.#.#.#.#.........#.#.#.#.#.#.#.#
...............#.......#...............
..............#.#.....#.#..............

```


## Rust


```Rust

fn main() {
    struct ElementaryCA {
        rule: u8,
        state: u64,
    }
    impl ElementaryCA {
        fn new(rule: u8) -> (u64, ElementaryCA) {
            let out = ElementaryCA {
                rule,
                state: 1,
            };
            (out.state, out)
        }
        fn next(&mut self) -> u64 {
            let mut next_state = 0u64;
            let state = self.state;
            for i in 0..64 {
                next_state |= (((self.rule as u64)>>(7 & (state.rotate_left(1).rotate_right(i as u32)))) & 1)<<i;
            }
            self.state = next_state;
            self.state
        }
    }
    fn rep_u64(val: u64) -> String {
        let mut out = String::new();
        for i in (0..64).rev() {
            if 1<<i & val != 0 {
                out = out + "\u{2588}";
            } else {
                out = out + "-";
            }
        }
        out
    }

    let (i, mut thirty) = ElementaryCA::new(154);
    println!("{}",rep_u64(i));
    for _ in 0..32 {
        let s = thirty.next();
        println!("{}", rep_u64(s));
    }
}

```

{{out}}

```txt

---------------------------------------------------------------█
█-------------------------------------------------------------█-
-█-----------------------------------------------------------█--
█-█---------------------------------------------------------█-█-
---█-------------------------------------------------------█----
--█-█-----------------------------------------------------█-█---
-█---█---------------------------------------------------█---█--
█-█-█-█-------------------------------------------------█-█-█-█-
-------█-----------------------------------------------█--------
------█-█---------------------------------------------█-█-------
-----█---█-------------------------------------------█---█------
----█-█-█-█-----------------------------------------█-█-█-█-----
---█-------█---------------------------------------█-------█----
--█-█-----█-█-------------------------------------█-█-----█-█---
-█---█---█---█-----------------------------------█---█---█---█--
█-█-█-█-█-█-█-█---------------------------------█-█-█-█-█-█-█-█-
---------------█-------------------------------█----------------
--------------█-█-----------------------------█-█---------------
-------------█---█---------------------------█---█--------------
------------█-█-█-█-------------------------█-█-█-█-------------
-----------█-------█-----------------------█-------█------------
----------█-█-----█-█---------------------█-█-----█-█-----------
---------█---█---█---█-------------------█---█---█---█----------
--------█-█-█-█-█-█-█-█-----------------█-█-█-█-█-█-█-█---------
-------█---------------█---------------█---------------█--------
------█-█-------------█-█-------------█-█-------------█-█-------
-----█---█-----------█---█-----------█---█-----------█---█------
----█-█-█-█---------█-█-█-█---------█-█-█-█---------█-█-█-█-----
---█-------█-------█-------█-------█-------█-------█-------█----
--█-█-----█-█-----█-█-----█-█-----█-█-----█-█-----█-█-----█-█---
-█---█---█---█---█---█---█---█---█---█---█---█---█---█---█---█--
█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-█-
----------------------------------------------------------------

```



## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.event.ActionEvent

import javax.swing._

object ElementaryCellularAutomaton extends App {

  SwingUtilities.invokeLater(() =>
    new JFrame("Elementary Cellular Automaton") {

      class ElementaryCellularAutomaton extends JPanel {
        private val dim = new Dimension(900, 450)
        private val cells = Array.ofDim[Byte](dim.height, dim.width)
        private var rule = 0

        private def ruleSet =
          Seq(30, 45, 50, 57, 62, 70, 73, 75, 86, 89, 90, 99, 101, 105, 109, 110, 124, 129, 133, 135, 137, 139, 141, 164, 170, 232)

        override def paintComponent(gg: Graphics): Unit = {
          def drawCa(g: Graphics2D): Unit = {

            def rules(lhs: Int, mid: Int, rhs: Int) = {
              val idx = lhs << 2 | mid << 1 | rhs
              (ruleSet(rule) >> idx & 1).toByte
            }

            g.setColor(Color.black)
            for (r <- 0 until cells.length - 1;
                 c <- 1 until cells(r).length - 1;
                 lhs = cells(r)(c - 1);
                 mid = cells(r)(c);
                 rhs = cells(r)(c + 1)) {
              cells(r + 1)(c) = rules(lhs, mid, rhs) // next generation
              if (cells(r)(c) == 1) g.fillRect(c, r, 1, 1)
            }
          }

          def drawLegend(g: Graphics2D): Unit = {
            val s = ruleSet(rule).toString
            val sw = g.getFontMetrics.stringWidth(ruleSet(rule).toString)
            g.setColor(Color.white)
            g.fillRect(16, 5, 55, 30)
            g.setColor(Color.darkGray)
            g.drawString(s, 16 + (55 - sw) / 2, 30)
          }

          super.paintComponent(gg)
          val g = gg.asInstanceOf[Graphics2D]
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          drawCa(g)
          drawLegend(g)
        }

        new Timer(5000, (_: ActionEvent) => {
          rule += 1
          if (rule == ruleSet.length) rule = 0
          repaint()
        }).start()
        cells(0)(dim.width / 2) = 1
        setBackground(Color.white)
        setFont(new Font("SansSerif", Font.BOLD, 28))
        setPreferredSize(dim)
      }

      add(new ElementaryCellularAutomaton, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    })

}
```



## Scheme


```scheme
; uses SRFI-1 library http://srfi.schemers.org/srfi-1/srfi-1.html

(define (evolve ls r)
  (unfold
    (lambda (x) (null? (cddr x)))
    (lambda (x)
      (vector-ref r (+ (* 4 (first x)) (* 2 (second x)) (third x))))
    cdr
    (cons (last ls) (append ls (list (car ls))))))

(define (automaton s r n)
  (define (*automaton s0 rv n)
    (for-each (lambda (x) (display (if (zero? x) #\. #\#))) s0)
    (newline)
    (if (not (zero? n))
      (let ((s1 (evolve s0 rv)))
	(*automaton s1 rv (- n 1)))))
  (display "Rule ")
  (display r)
  (newline)
  (*automaton
    s
    (list->vector
      (append
	(int->bin r)
	(make-list (- 7 (floor (/ (log r) (log 2)))) 0)))
    n))

(automaton '(0 1 0 0 0 1 0 1 0 0 1 1 1 1 0 0 0 0 0 1) 30 20)
```

{{out}}


```txt
Rule 30
.#...#.#..####.....#
.##.##.####...#...##
.#..#..#...#.###.##.
#########.##.#...#.#
..........#..##.##.#
#........#####..#..#
.#......##....######
.##....##.#..##.....
##.#..##..####.#....
#..####.###....##..#
.###....#..#..##.###
.#..#..########..#..
########.......####.
#.......#.....##....
##.....###...##.#..#
..#...##..#.##..####
####.##.###.#.###...
#....#..#...#.#..#.#
.#..######.##.####.#
.####......#..#....#
.#...#....######..##
```



## Sidef

{{trans|Perl}}

```ruby
class Automaton(rule, cells) {

    method init {
        rule = sprintf("%08b", rule).chars.map{.to_i}.reverse
    }

    method next {
        var previous = cells.map{_}
        var len = previous.len
        cells[] = rule[
                    previous.range.map { |i|
                        4*previous[i-1 % len] +
                        2*previous[i]         +
                        previous[i+1 % len]
                    }
                  ]
    }

    method to_s {
        cells.map { _ ? '#' : ' ' }.join
    }
}

var size = 20
var arr = size.of(0)
arr[size/2] = 1

var auto = Automaton(90, arr)

(size/2).times {
    print "|#{auto}|\n"
    auto.next
}
```

{{out}}

```txt

|          #         |
|         # #        |
|        #   #       |
|       # # # #      |
|      #       #     |
|     # #     # #    |
|    #   #   #   #   |
|   # # # # # # # #  |
|  #               # |
| # #             # #|

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

oo::class create ElementaryAutomaton {
    variable rules
    # Decode the rule number to get a collection of state mapping rules.
    # In effect, "compiles" the rule number
    constructor {ruleNumber} {
	set ins {111 110 101 100 011 010 001 000}
	set bits [split [string range [format %08b $ruleNumber] end-7 end] ""]
	foreach input {111 110 101 100 011 010 001 000} state $bits {
	    dict set rules $input $state
	}
    }

    # Apply the rule to an automaton state to get a new automaton state.
    # We wrap the edges; the state space is circular.
    method evolve {state} {
	set len [llength $state]
	for {set i 0} {$i < $len} {incr i} {
	    lappend result [dict get $rules [
		    lindex $state [expr {($i-1)%$len}]][
		    lindex $state $i][
		    lindex $state [expr {($i+1)%$len}]]]
	}
	return $result
    }

    # Simple driver method; omit the initial state to get a centred dot
    method run {steps {initialState ""}} {
	if {[llength [info level 0]] < 4} {
	    set initialState "[string repeat . $steps]1[string repeat . $steps]"
	}
	set s [split [string map ". 0 # 1" $initialState] ""]
	for {set i 0} {$i < $steps} {incr i} {
	    puts [string map "0 . 1 #" [join $s ""]]
	    set s [my evolve $s]
	}
	puts [string map "0 . 1 #" [join $s ""]]
    }
}
```

Demonstrating:

```tcl
puts "Rule 90 (with default state):"
ElementaryAutomaton create rule90 90
rule90 run 20
puts "\nRule 122:"
[ElementaryAutomaton new 122] run 25 "..........#......…."
```

{{out}}

```txt

Rule 90 (with default state):
....................#....................
...................#.#...................
..................#...#..................
.................#.#.#.#.................
................#.......#................
...............#.#.....#.#...............
..............#...#...#...#..............
.............#.#.#.#.#.#.#.#.............
............#...............#............
...........#.#.............#.#...........
..........#...#...........#...#..........
.........#.#.#.#.........#.#.#.#.........
........#.......#.......#.......#........
.......#.#.....#.#.....#.#.....#.#.......
......#...#...#...#...#...#...#...#......
.....#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.....
....#...............................#....
...#.#.............................#.#...
..#...#...........................#...#..
.#.#.#.#.........................#.#.#.#.
#.......#.......................#.......#

Rule 122:
..........#..........
.........#.#.........
........#.#.#........
.......#.#.#.#.......
......#.#.#.#.#......
.....#.#.#.#.#.#.....
....#.#.#.#.#.#.#....
...#.#.#.#.#.#.#.#...
..#.#.#.#.#.#.#.#.#..
.#.#.#.#.#.#.#.#.#.#.
#.#.#.#.#.#.#.#.#.#.#
##.#.#.#.#.#.#.#.#.##
.##.#.#.#.#.#.#.#.##.
####.#.#.#.#.#.#.####
...##.#.#.#.#.#.##...
..####.#.#.#.#.####..
.##..##.#.#.#.##..##.
########.#.#.########
.......##.#.##.......
......####.####......
.....##..###..##.....
....######.######....
...##....###....##...
..####..##.##..####..
.##..###########..##.
######.........######

```



## zkl


```zkl
fcn rule(n){ n=n.toString(2); "00000000"[n.len() - 8,*] + n }
fcn applyRule(rule,cells){
   cells=String(cells[-1],cells,cells[0]); // wrap cell ends
   (cells.len() - 2).pump(String,'wrap(n){ rule[7 - cells[n,3].toInt(2)] })
}
```


```zkl
cells:="0000000000000001000000000000000"; r90:=rule(90); map:=" *";
r90.println(" rule 90");
do(20){ cells.apply(map.get).println(); cells=applyRule(r90,cells); }
```

{{out}}

```txt

01011010 rule 90
               *               
              * *              
             *   *             
            * * * *            
           *       *           
          * *     * *          
         *   *   *   *         
        * * * * * * * *        
       *               *       
      * *             * *      
     *   *           *   *     
    * * * *         * * * *    
   *       *       *       *   
  * *     * *     * *     * *  
 *   *   *   *   *   *   *   * 
* * * * * * * * * * * * * * * *
*                             *
**                           **
 **                         ** 
****                       ****

```


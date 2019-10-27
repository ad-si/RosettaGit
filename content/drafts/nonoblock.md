+++
title = "Nonoblock"
description = ""
date = 2019-05-16T18:25:33Z
aliases = []
[extra]
id = 17480
[taxonomies]
categories = []
tags = []
+++

{{task}} [[Category:Puzzles]]
Nonoblock is a chip off the old [[Nonogram solver|Nonogram]] puzzle.


;Given:
* The number of cells in a row.
* The size of each, (space separated), connected block of cells to fit in the row, in left-to right order.


;Task: 
* show all possible positions. 
* show the number of positions of the blocks for the following cases within the row. 
* show all output on this page. 
* use a "neat" diagram of the block positions.


;Enumerate the following configurations:
#   '''5'''   cells   and   '''[2, 1]'''   blocks
#   '''5'''   cells   and   '''[]'''   blocks   (no blocks)
#   '''10'''   cells   and   '''[8]'''   blocks
#   '''15'''   cells   and   '''[2, 3, 2, 3]'''   blocks
#   '''5'''   cells   and   '''[2, 3]'''   blocks   (should give some indication of this not being possible)


;Example:
Given a row of five cells and a block of two cells followed by a block of one cell - in that order, the example could be shown as:

    |_|_|_|_|_| # 5 cells and [2, 1] blocks

And would expand to the following 3 possible rows of block positions:

    |A|A|_|B|_|
    |A|A|_|_|B|
    |_|A|A|_|B|


Note how the sets of blocks are always separated by a space.

Note also that it is not necessary for each block to have a separate letter. 
Output approximating

This:
                        |#|#|_|#|_|
                        |#|#|_|_|#|
                        |_|#|#|_|#|

This would also work:
                        ##.#.
                        ##..#
                       .##.#


;An algorithm:
* Find the minimum space to the right that is needed to legally hold all but the leftmost block of cells (with a space between blocks remember).
* The leftmost cell can legitimately be placed in all positions from the LHS up to a RH position that allows enough room for the rest of the blocks.
* for each position of the LH block recursively compute the position of the rest of the blocks in the ''remaining'' space to the right of the current placement of the LH block.

(This is the algorithm used in the [[Nonoblock#Python]] solution).    


;Reference:
* The blog post [http://paddy3118.blogspot.co.uk/2014/03/nonogram-puzzle-solver-part-1.html Nonogram puzzle solver (part 1)] Inspired this task and donated its [[Nonoblock#Python]] solution.





## C++


```cpp

#include <iomanip>
#include <iostream>
#include <algorithm>
#include <numeric>
#include <string>
#include <vector>

typedef std::pair<int, std::vector<int> > puzzle;

class nonoblock {
public:
    void solve( std::vector<puzzle>& p ) {
        for( std::vector<puzzle>::iterator i = p.begin(); i != p.end(); i++ ) {
            counter = 0;
            std::cout << " Puzzle: " << ( *i ).first << " cells and blocks [ ";
            for( std::vector<int>::iterator it = ( *i ).second.begin(); it != ( *i ).second.end(); it++ )
                std::cout << *it << " ";
            std::cout << "] ";
            int s = std::accumulate( ( *i ).second.begin(), ( *i ).second.end(), 0 ) + ( ( *i ).second.size() > 0 ? ( *i ).second.size() - 1 : 0 );
            if( ( *i ).first - s < 0 ) {
                std::cout << "has no solution!\n\n\n";
                continue;
            }
            std::cout << "\n Possible configurations:\n\n";
            std::string b( ( *i ).first, '-' );
            solve( *i, b, 0 );
            std::cout << "\n\n";
        } 
    }

private:
    void solve( puzzle p, std::string n, int start ) {
        if( p.second.size() < 1 ) {
            output( n );
            return;
        }
        std::string temp_string;
        int offset,
            this_block_size = p.second[0];

        int space_need_for_others = std::accumulate( p.second.begin() + 1, p.second.end(), 0 );
        space_need_for_others += p.second.size() - 1;

        int space_for_curr_block = p.first - space_need_for_others - std::accumulate( p.second.begin(), p.second.begin(), 0 );

        std::vector<int> v1( p.second.size() - 1 );
        std::copy( p.second.begin() + 1, p.second.end(), v1.begin() );
        puzzle p1 = std::make_pair( space_need_for_others, v1 );

        for( int a = 0; a < space_for_curr_block; a++ ) {
            temp_string = n;

            if( start + this_block_size > n.length() ) return;

            for( offset = start; offset < start + this_block_size; offset++ )
                temp_string.at( offset ) = 'o';

            if( p1.first ) solve( p1, temp_string, offset + 1 );
            else output( temp_string );

            start++;
        }
    }
    void output( std::string s ) {
        char b = 65 - ( s.at( 0 ) == '-' ? 1 : 0 );
        bool f = false;
        std::cout << std::setw( 3 ) << ++counter << "\t|";
        for( std::string::iterator i = s.begin(); i != s.end(); i++ ) {
            b += ( *i ) == 'o' && f ? 1 : 0;
            std::cout << ( ( *i ) == 'o' ? b : '_' ) << "|";
            f = ( *i ) == '-' ? true : false;
        }
        std::cout << "\n";
    }

    unsigned counter;
};

int main( int argc, char* argv[] )
{
    std::vector<puzzle> problems;
    std::vector<int> blocks; 
    blocks.push_back( 2 ); blocks.push_back( 1 );
    problems.push_back( std::make_pair( 5, blocks ) );
    blocks.clear();
    problems.push_back( std::make_pair( 5, blocks ) );
    blocks.push_back( 8 );
    problems.push_back( std::make_pair( 10, blocks ) );
    blocks.clear();
    blocks.push_back( 2 ); blocks.push_back( 3 );
    problems.push_back( std::make_pair( 5, blocks ) );
    blocks.push_back( 2 ); blocks.push_back( 3 );
    problems.push_back( std::make_pair( 15, blocks ) );

    nonoblock nn;
    nn.solve( problems );

    return 0;
}

```

{{out}}

```txt

 Puzzle: 5 cells and blocks [ 2 1 ]
 Possible configurations:

  1     |A|A|_|B|_|
  2     |A|A|_|_|B|
  3     |_|A|A|_|B|


 Puzzle: 5 cells and blocks [ ]
 Possible configurations:

  1     |_|_|_|_|_|


 Puzzle: 10 cells and blocks [ 8 ]
 Possible configurations:

  1     |A|A|A|A|A|A|A|A|_|_|
  2     |_|A|A|A|A|A|A|A|A|_|
  3     |_|_|A|A|A|A|A|A|A|A|


 Puzzle: 5 cells and blocks [ 2 3 ] has no solution!


 Puzzle: 15 cells and blocks [ 2 3 2 3 ]
 Possible configurations:

  1     |A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
  2     |A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
  3     |A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
  4     |A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
  5     |A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
  6     |A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
  7     |A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
  8     |A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
  9     |A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
 10     |A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
 11     |_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
 12     |_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
 13     |_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
 14     |_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
 15     |_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|


```



## C sharp

This solution uses a StringBuilder. Spaces are moved from right to left and the problem is then solved recursively.

```csharp
using System;
using System.Linq;
using System.Text;

public static class Nonoblock
{
    public static void Main() {
        Positions(5, 2,1);
        Positions(5);
        Positions(10, 8);
        Positions(15, 2,3,2,3);
        Positions(5, 2,3);
    }

    public static void Positions(int cells, params int[] blocks) {
        if (cells < 0 || blocks == null || blocks.Any(b => b < 1)) throw new ArgumentOutOfRangeException();
        Console.WriteLine($"{cells} cells with [{string.Join(", ", blocks)}]");
        if (blocks.Sum() + blocks.Length - 1 > cells) {
            Console.WriteLine("No solution");
            return;
        }
        var spaces = new int[blocks.Length + 1];
        int total = -1;
        for (int i = 0; i < blocks.Length; i++) {
            total += blocks[i] + 1;
            spaces[i+1] = total;
        }
        spaces[spaces.Length - 1] = cells - 1;
        var sb = new StringBuilder(string.Join(".", blocks.Select(b => new string('#', b))).PadRight(cells, '.'));
        Iterate(sb, spaces, spaces.Length - 1, 0);
        Console.WriteLine();
    }

    private static void Iterate(StringBuilder output, int[] spaces, int index, int offset) {
        Console.WriteLine(output.ToString());
        if (index <= 0) return;
        int count = 0;
        while (output[spaces[index] - offset] != '#') {
            count++;
            output.Remove(spaces[index], 1);
            output.Insert(spaces[index-1], '.');
            spaces[index-1]++;
            Iterate(output, spaces, index - 1, 1);
        }
        if (offset == 0) return;
        spaces[index-1] -= count;
        output.Remove(spaces[index-1], count);
        output.Insert(spaces[index] - count, ".", count);
    }

}
```

{{out}}
<pre style="height:50ex;overflow:scroll">
5 cells with [2, 1]
##.#.
##..#
.##.#

5 cells with []
.....

10 cells with [8]
########..
.########.
..########

15 cells with [2, 3, 2, 3]
##.###.##.###..
##.###.##..###.
##.###..##.###.
##..###.##.###.
.##.###.##.###.
##.###.##...###
##.###..##..###
##..###.##..###
.##.###.##..###
##.###...##.###
##..###..##.###
.##.###..##.###
##...###.##.###
.##..###.##.###
..##.###.##.###

5 cells with [2, 3]
No solution
```



## D

{{trans|python}}

```d
import std.stdio, std.array, std.algorithm, std.exception, std.conv,
       std.concurrency, std.range;

struct Solution { uint pos, len; }

Generator!(Solution[]) nonoBlocks(in uint[] blocks, in uint cells) {
    return new typeof(return)({
        if (blocks.empty || blocks[0] == 0) {
            yield([Solution(0, 0)]);
        } else {
            enforce(blocks.sum + blocks.length - 1 <= cells,
                    "Those blocks cannot fit in those cells.");
            immutable firstBl = blocks[0];
            const restBl = blocks.dropOne;

            // The other blocks need space.
            immutable minS = restBl.map!(b => b + 1).sum;

            // Slide the start position from left to max RH
            // index allowing for other blocks.
            foreach (immutable bPos; 0 .. cells - minS - firstBl + 1) {
                if (restBl.empty) {
                    // No other blocks to the right so just yield
                    // this one.
                    yield([Solution(bPos, firstBl)]);
                } else {
                    // More blocks to the right so create a sub-problem
                    // of placing the restBl blocks in the cells one
                    // space to the right of the RHS of this block.
                    immutable offset = bPos + firstBl + 1;
                    immutable newCells = cells - offset;

                    // Recursive call to nonoBlocks yields multiple
                    // sub-positions.
                    foreach (const subPos; nonoBlocks(restBl, newCells)) {
                        // Remove the offset from sub block positions.
                        auto rest = subPos.map!(sol => Solution(offset + sol.pos, sol.len));

                        // Yield this block plus sub blocks positions.
                        yield(Solution(bPos, firstBl) ~ rest.array);
                    }
                }
            }
        }
    });
}


/// Pretty prints each run of blocks with a
/// different letter for each block of filled cells.
string show(in Solution[] vec, in uint nCells) pure {
    auto result = ['_'].replicate(nCells);
    foreach (immutable i, immutable sol; vec)
        foreach (immutable j; sol.pos .. sol.pos + sol.len)
            result[j] = (result[j] == '_') ? to!char('A' + i) : '?';
    return '[' ~ result ~ ']';
}

void main() {
    static struct Problem { uint[] blocks; uint nCells; }

    immutable Problem[] problems = [{[2, 1], 5},
                                    {[], 5},
                                    {[8], 10},
                                    {[2, 3, 2, 3], 15},
                                    {[4, 3], 10},
                                    {[2, 1], 5},
                                    {[3, 1], 10},
                                    {[2, 3], 5}];

    foreach (immutable prob; problems) {
        writefln("Configuration (%d cells and %s blocks):",
                 prob.nCells, prob.blocks);
        show([], prob.nCells).writeln;
        "Possibilities:".writeln;
        auto nConfigs = 0;
        foreach (const sol; nonoBlocks(prob.tupleof)) {
            show(sol, prob.nCells).writeln;
            nConfigs++;
        }
        writefln("A total of %d possible configurations.", nConfigs);
        writeln;
    }
}
```

{{out}}

```txt
Configuration (5 cells and [2, 1] blocks):
[_____]
Possibilities:
[AA_B_]
[AA__B]
[_AA_B]
A total of 3 possible configurations.

Configuration (5 cells and [] blocks):
[_____]
Possibilities:
[_____]
A total of 1 possible configurations.

Configuration (10 cells and [8] blocks):
[__________]
Possibilities:
[AAAAAAAA__]
[_AAAAAAAA_]
[__AAAAAAAA]
A total of 3 possible configurations.

Configuration (15 cells and [2, 3, 2, 3] blocks):
[_______________]
Possibilities:
[AA_BBB_CC_DDD__]
[AA_BBB_CC__DDD_]
[AA_BBB_CC___DDD]
[AA_BBB__CC_DDD_]
[AA_BBB__CC__DDD]
[AA_BBB___CC_DDD]
[AA__BBB_CC_DDD_]
[AA__BBB_CC__DDD]
[AA__BBB__CC_DDD]
[AA___BBB_CC_DDD]
[_AA_BBB_CC_DDD_]
[_AA_BBB_CC__DDD]
[_AA_BBB__CC_DDD]
[_AA__BBB_CC_DDD]
[__AA_BBB_CC_DDD]
A total of 15 possible configurations.

Configuration (10 cells and [4, 3] blocks):
[__________]
Possibilities:
[AAAA_BBB__]
[AAAA__BBB_]
[AAAA___BBB]
[_AAAA_BBB_]
[_AAAA__BBB]
[__AAAA_BBB]
A total of 6 possible configurations.

Configuration (5 cells and [2, 1] blocks):
[_____]
Possibilities:
[AA_B_]
[AA__B]
[_AA_B]
A total of 3 possible configurations.

Configuration (10 cells and [3, 1] blocks):
[__________]
Possibilities:
[AAA_B_____]
[AAA__B____]
[AAA___B___]
[AAA____B__]
[AAA_____B_]
[AAA______B]
[_AAA_B____]
[_AAA__B___]
[_AAA___B__]
[_AAA____B_]
[_AAA_____B]
[__AAA_B___]
[__AAA__B__]
[__AAA___B_]
[__AAA____B]
[___AAA_B__]
[___AAA__B_]
[___AAA___B]
[____AAA_B_]
[____AAA__B]
[_____AAA_B]
A total of 21 possible configurations.

Configuration (5 cells and [2, 3] blocks):
[_____]
Possibilities:
object.Exception @nonoblock.d(17): Those blocks cannot fit in those cells.
----------------
0x0040AC17 in pure @safe void std.exception.bailOut(immutable(char)[], uint, const(char[]))
...
```



## EchoLisp


```scheme

;; size is the remaining # of cells
;; blocks is the list of remaining blocks size
;; cells is a stack where we push 0 = space or block size.
(define (nonoblock size blocks into: cells)
(cond
	((and (empty? blocks) (= 0 size)) (print-cells (stack->list cells)))
	
	((<= size 0) #f) ;; no hope - cut search
	((> (apply + blocks) size)  #f) ;; no hope - cut search
	
	(else
		(push cells 0) ;; space
		(nonoblock (1- size) blocks  cells)
		(pop cells)

	(when (!empty? blocks) 
		(when (stack-empty? cells) ;; first one (no space is allowed)
		(push cells (first blocks))
		(nonoblock  (- size (first blocks)) (rest blocks) cells)
		(pop cells))

		(push cells 0) ;; add space before
		(push cells (first blocks))
		(nonoblock  (- size (first blocks) 1) (rest blocks) cells)
		(pop cells)
		(pop cells)))))
	
(string-delimiter "")
(define block-symbs #( ?  📦 💣 💊  🍒 🌽 📘 📙 💰 🍯 ))

(define (print-cells cells)
	(writeln (string-append "|"
		(for/string ((cell cells)) 
			(if (zero? cell) "_"  
				(for/string ((i cell)) [block-symbs cell]))) "|")))
	
(define (task  nonotest)
	(for ((test nonotest)) 
		(define size (first test))
		(define blocks (second test))
		(printf "\n size:%d blocks:%d" size blocks)
		(if
			(> (+ (apply + blocks)(1- (length blocks))) size) 
				(writeln "❌ no solution for" size blocks)
			    (nonoblock size blocks (stack 'cells)))))

```

			    
{{out}}

```txt

(define nonotest '((5 (2 1)) (5 ()) (10 (8)) (15 (2 3 2 3)) (5 (2 3))))
(task nonotest)

    size:5 blocks:(2 1)
    |💣💣__📦|    
    |💣💣_📦_|    
    |_💣💣_📦|    

    size:5 blocks:()
    |_____|    

    size:10 blocks:(8)
    |__💰💰💰💰💰💰💰💰|    
    |💰💰💰💰💰💰💰💰__|    
    |_💰💰💰💰💰💰💰💰_|    

    size:15 blocks:(2 3 2 3)
    |__💣💣_💊💊💊_💣💣_💊💊💊|    
    |💣💣___💊💊💊_💣💣_💊💊💊|    
    |💣💣__💊💊💊__💣💣_💊💊💊|    
    |💣💣__💊💊💊_💣💣__💊💊💊|    
    |💣💣__💊💊💊_💣💣_💊💊💊_|    
    |💣💣_💊💊💊___💣💣_💊💊💊|    
    |💣💣_💊💊💊__💣💣__💊💊💊|    
    |💣💣_💊💊💊__💣💣_💊💊💊_|    
    |💣💣_💊💊💊_💣💣___💊💊💊|    
    |💣💣_💊💊💊_💣💣__💊💊💊_|    
    |💣💣_💊💊💊_💣💣_💊💊💊__|    
    |_💣💣__💊💊💊_💣💣_💊💊💊|    
    |_💣💣_💊💊💊__💣💣_💊💊💊|    
    |_💣💣_💊💊💊_💣💣__💊💊💊|    
    |_💣💣_💊💊💊_💣💣_💊💊💊_|    

    size:5 blocks:(2 3)
    ❌ no solution for     5     (2 3)  

```
  


## Elixir

{{trans|Ruby}}

```elixir
defmodule Nonoblock do
  def solve(cell, blocks) do
    width = Enum.sum(blocks) + length(blocks) - 1
    if cell < width do
      raise "Those blocks will not fit in those cells"
    else
      nblocks(cell, blocks, "")
    end
  end
  
  defp nblocks(cell, _, position) when cell<=0, do:
    display(String.slice(position, 0..cell-1))
  defp nblocks(cell, blocks, position) when length(blocks)==0 or hd(blocks)==0, do:
    display(position <> String.duplicate(".", cell))
  defp nblocks(cell, blocks, position) do
    rest = cell - Enum.sum(blocks) - length(blocks) + 2
    [bl | brest] = blocks
    Enum.reduce(0..rest-1, 0, fn i,acc ->
      acc + nblocks(cell-i-bl-1, brest, position <> String.duplicate(".", i) <> String.duplicate("#",bl) <> ".")
    end)
  end
  
  defp display(str) do
    IO.puts nonocell(str)
    1                           # number of positions
  end
  
  def nonocell(str) do                  # "##.###..##" -> "|A|A|_|B|B|B|_|_|C|C|"
    slist = String.to_char_list(str) |> Enum.chunk_by(&(&1==?.)) |> Enum.map(&List.to_string(&1))
    chrs = Enum.map(?A..?Z, &List.to_string([&1]))
    result = nonocell_replace(slist, chrs, "")
             |> String.replace(".", "_")
             |> String.split("") |> Enum.join("|")
    "|" <> result
  end
  
  defp nonocell_replace([], _, result), do: result
  defp nonocell_replace([h|t], chrs, result) do
    if String.first(h) == "#" do
      [c | rest] = chrs
      nonocell_replace(t, rest, result <> String.replace(h, "#", c))
    else
      nonocell_replace(t, chrs, result <> h)
    end
  end
end

conf = [{ 5, [2, 1]},
        { 5, []},
        {10, [8]},
        {15, [2, 3, 2, 3]},
        { 5, [2, 3]}       ]
Enum.each(conf, fn {cell, blocks} ->
  try do
    IO.puts "Configuration:"
    IO.puts "#{Nonoblock.nonocell(String.duplicate(".",cell))} # #{cell} cells and #{inspect blocks} blocks"
    IO.puts "Possibilities:"
    count = Nonoblock.solve(cell, blocks)
    IO.puts "A total of #{count} Possible configurations.\n"
  rescue
    e in RuntimeError -> IO.inspect e
  end
end)
```


{{out}}

```txt

Configuration:
|_|_|_|_|_| # 5 cells and [2, 1] blocks
Possibilities:
|A|A|_|B|_|
|A|A|_|_|B|
|_|A|A|_|B|
A total of 3 Possible configurations.

Configuration:
|_|_|_|_|_| # 5 cells and [] blocks
Possibilities:
|_|_|_|_|_|
A total of 1 Possible configurations.

Configuration:
|_|_|_|_|_|_|_|_|_|_| # 10 cells and '\b' blocks
Possibilities:
|A|A|A|A|A|A|A|A|_|_|
|_|A|A|A|A|A|A|A|A|_|
|_|_|A|A|A|A|A|A|A|A|
A total of 3 Possible configurations.

Configuration:
|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_| # 15 cells and [2, 3, 2, 3] blocks
Possibilities:
|A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
|A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
|A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
|A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
|A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
|A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
|A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
|A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
|A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
|A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
|_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
|_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
|_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
|_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
|_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|
A total of 15 Possible configurations.

Configuration:
|_|_|_|_|_| # 5 cells and [2, 3] blocks
Possibilities:
%RuntimeError{message: "Those blocks will not fit in those cells"}

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "strings"
)

func printBlock(data string, le int) {
    a := []byte(data)
    sumBytes := 0
    for _, b := range a {
        sumBytes += int(b - 48)
    }
    fmt.Printf("\nblocks %c, cells %d\n", a, le)
    if le-sumBytes <= 0 {
        fmt.Println("No solution")
        return
    }
    prep := make([]string, len(a))
    for i, b := range a {
        prep[i] = strings.Repeat("1", int(b-48))
    }
    for _, r := range genSequence(prep, le-sumBytes+1) {
        fmt.Println(r[1:])
    }
}

func genSequence(ones []string, numZeros int) []string {
    if len(ones) == 0 {
        return []string{strings.Repeat("0", numZeros)}
    }
    var result []string
    for x := 1; x < numZeros-len(ones)+2; x++ {
        skipOne := ones[1:]
        for _, tail := range genSequence(skipOne, numZeros-x) {
            result = append(result, strings.Repeat("0", x)+ones[0]+tail)
        }
    }
    return result
}

func main() {
    printBlock("21", 5)
    printBlock("", 5)
    printBlock("8", 10)
    printBlock("2323", 15)
    printBlock("23", 5)
}
```


{{out}}

```txt

blocks [2 1], cells 5
11010
11001
01101

blocks [], cells 5
00000

blocks [8], cells 10
1111111100
0111111110
0011111111

blocks [2 3 2 3], cells 15
110111011011100
110111011001110
110111011000111
110111001101110
110111001100111
110111000110111
110011101101110
110011101100111
110011100110111
110001110110111
011011101101110
011011101100111
011011100110111
011001110110111
001101110110111

blocks [2 3], cells 5
No solution

```



## J


Implementation:


```J
nonoblock=:4 :0
  s=. 1+(1+x)-+/1+y
  pad=.1+(#~ s >+/"1)((1+#y)#s) #: i.s^1+#y
  ~.pad (_1}.1 }. ,. #&, 0 ,. 1 + i.@#@])"1]y,0
)

neat=: [: (#~ # $ 0 1"_)@": {&(' ',65}.a.)&.>
```


Task example:


```J
   neat 5 nonoblock 2 1
│A│A│ │B│ │
│A│A│ │ │B│
│ │A│A│ │B│
   neat 5 nonoblock ''
│ │ │ │ │ │
   neat 10 nonoblock 8
│A│A│A│A│A│A│A│A│ │ │
│ │A│A│A│A│A│A│A│A│ │
│ │ │A│A│A│A│A│A│A│A│
   neat 15 nonoblock 2 3 2 3
│A│A│ │B│B│B│ │C│C│ │D│D│D│ │ │
│A│A│ │B│B│B│ │C│C│ │ │D│D│D│ │
│A│A│ │B│B│B│ │C│C│ │ │ │D│D│D│
│A│A│ │B│B│B│ │ │C│C│ │D│D│D│ │
│A│A│ │B│B│B│ │ │C│C│ │ │D│D│D│
│A│A│ │B│B│B│ │ │ │C│C│ │D│D│D│
│A│A│ │ │B│B│B│ │C│C│ │D│D│D│ │
│A│A│ │ │B│B│B│ │C│C│ │ │D│D│D│
│A│A│ │ │B│B│B│ │ │C│C│ │D│D│D│
│A│A│ │ │ │B│B│B│ │C│C│ │D│D│D│
│ │A│A│ │B│B│B│ │C│C│ │D│D│D│ │
│ │A│A│ │B│B│B│ │C│C│ │ │D│D│D│
│ │A│A│ │B│B│B│ │ │C│C│ │D│D│D│
│ │A│A│ │ │B│B│B│ │C│C│ │D│D│D│
│ │ │A│A│ │B│B│B│ │C│C│ │D│D│D│
   neat 5 nonoblock 2 3

```



## Java

{{works with|Java|8}}

```java
import java.util.*;
import static java.util.Arrays.stream;
import static java.util.stream.Collectors.toList;

public class Nonoblock {

    public static void main(String[] args) {
        printBlock("21", 5);
        printBlock("", 5);
        printBlock("8", 10);
        printBlock("2323", 15);
        printBlock("23", 5);
    }

    static void printBlock(String data, int len) {
        int sumChars = data.chars().map(c -> Character.digit(c, 10)).sum();
        String[] a = data.split("");

        System.out.printf("%nblocks %s, cells %s%n", Arrays.toString(a), len);
        if (len - sumChars <= 0) {
            System.out.println("No solution");
            return;
        }

        List<String> prep = stream(a).filter(x -> !"".equals(x))
                .map(x -> repeat(Character.digit(x.charAt(0), 10), "1"))
                .collect(toList());

        for (String r : genSequence(prep, len - sumChars + 1))
            System.out.println(r.substring(1));
    }

    // permutation generator, translated from Python via D
    static List<String> genSequence(List<String> ones, int numZeros) {
        if (ones.isEmpty())
            return Arrays.asList(repeat(numZeros, "0"));

        List<String> result = new ArrayList<>();
        for (int x = 1; x < numZeros - ones.size() + 2; x++) {
            List<String> skipOne = ones.stream().skip(1).collect(toList());
            for (String tail : genSequence(skipOne, numZeros - x))
                result.add(repeat(x, "0") + ones.get(0) + tail);
        }
        return result;
    }

    static String repeat(int n, String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++)
            sb.append(s);
        return sb.toString();
    }
}
```


```txt
blocks [2, 1], cells 5
11010
11001
01101

blocks [], cells 5
00000

blocks [8], cells 10
1111111100
0111111110
0011111111

blocks [2, 3, 2, 3], cells 15
110111011011100
110111011001110
110111011000111
110111001101110
110111001100111
110111000110111
110011101101110
110011101100111
110011100110111
110001110110111
011011101101110
011011101100111
011011100110111
011001110110111
001101110110111

blocks [2, 3], cells 5
No solution
```



## Julia


```julia
minsized(arr) = join(map(x->"#"^x, arr), ".")
minlen(arr) = sum(arr) + length(arr) - 1
 
function sequences(blockseq, numblanks)
    if isempty(blockseq)
        return ["." ^ numblanks]
    elseif minlen(blockseq) == numblanks
        return minsized(blockseq)
    else    
        result = Vector{String}()
        allbuthead = blockseq[2:end]
        for leftspace in 0:(numblanks - minlen(blockseq))
            header = "." ^ leftspace * "#" ^ blockseq[1] * "."
            rightspace = numblanks - length(header)
            if isempty(allbuthead)
                push!(result, rightspace <= 0 ? header[1:numblanks] : header * "." ^ rightspace)            
            elseif minlen(allbuthead) == rightspace
                push!(result, header * minsized(allbuthead))
            else
                map(x -> push!(result, header * x), sequences(allbuthead, rightspace))
            end
        end
    end
    result
end

function nonoblocks(bvec, len)
    println("With blocks $bvec and $len cells:")
    len < minlen(bvec) ? println("No solution") : for seq in sequences(bvec, len) println(seq) end
end

nonoblocks([2, 1], 5)
nonoblocks(Vector{Int}([]), 5)
nonoblocks([8], 10)
nonoblocks([2, 3, 2, 3], 15)
nonoblocks([2, 3], 5)

```
 {{output}} 
```txt

 With blocks [2, 1] and 5 cells:
 ##.#.
 ##..#
 .##.#
 With blocks Int64[] and 5 cells:
 .....
 With blocks [8] and 10 cells:
 ########..
 .########.
 ..########
 With blocks [2, 3, 2, 3] and 15 cells:
 ##.###.##.###..
 ##.###.##..###.
 ##.###.##...###
 ##.###..##.###.
 ##.###..##..###
 ##.###...##.###
 ##..###.##.###.
 ##..###.##..###
 ##..###..##.###
 ##...###.##.###
 .##.###.##.###.
 .##.###.##..###
 .##.###..##.###
 .##..###.##.###
 ..##.###.##.###
 With blocks [2, 3] and 5 cells:
 No solution

```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

fun printBlock(data: String, len: Int) {
    val a = data.toCharArray()
    val sumChars = a.map { it.toInt() - 48 }.sum()
    println("\nblocks ${a.asList()}, cells $len")
    if (len - sumChars <= 0) {
        println("No solution")
        return
    }
    val prep = a.map { "1".repeat(it.toInt() - 48) }
    for (r in genSequence(prep, len - sumChars + 1)) println(r.substring(1))
}

fun genSequence(ones: List<String>, numZeros: Int): List<String> {
    if (ones.isEmpty()) return listOf("0".repeat(numZeros))
    val result = mutableListOf<String>()
    for (x in 1 until numZeros - ones.size + 2) {
        val skipOne = ones.drop(1)
        for (tail in genSequence(skipOne, numZeros - x)) {
            result.add("0".repeat(x) + ones[0] + tail)
        }
    }
    return result
}

fun main(args: Array<String>) {
    printBlock("21", 5)
    printBlock("", 5)
    printBlock("8", 10)
    printBlock("2323", 15)
    printBlock("23", 5)
}
```


{{out}}

```txt

blocks [2, 1], cells 5
11010
11001
01101

blocks [], cells 5
00000

blocks [8], cells 10
1111111100
0111111110
0011111111

blocks [2, 3, 2, 3], cells 15
110111011011100
110111011001110
110111011000111
110111001101110
110111001100111
110111000110111
110011101101110
110011101100111
110011100110111
110001110110111
011011101101110
011011101100111
011011100110111
011001110110111
001101110110111

blocks [2, 3], cells 5
No solution

```



## M2000 Interpreter


### Recursive


```M2000 Interpreter

Module NonoBlock {
      Form 80,40
      Flush
      Print "Nonoblock" 
      Data 5, (2, 1)
      Data 5, (,)
      Data 10, (8,) 
      Data 15, (2,3,2,3)
      Data 5, (2,3)
      Def BLen(a$)=(Len(a$)-1)/2
      Function UseLetter(arr) {
            Dim Base 0, Res$(Len(arr))
            Link Res$() to Res()
            Def Ord$(a$)=ChrCode$(Chrcode(a$)+1)
            L$="A"
            i=each(arr)
            While i {
                  Res$(i^)=String$("|"+L$, Array(i))+"|"
                  L$=Ord$(L$)
            }
            =Res()
      }
      Count=0
      For i=1 to 5
      Read Cells, Blocks
      Blocks=UseLetter(Blocks)
      Print str$(i,"")+".", "Cells=";Cells, "", iF(len(Blocks)=0->("Empty",), Blocks)
      PrintRow( "|", Cells, Blocks, &Count)
      CheckCount()
      Next I
      Sub CheckCount()
            If count=0 Then Print " Impossible"
            count=0
      End Sub
      Sub PrintRow(Lpart$, Cells, Blocks, &Comp)
            If len(Blocks)=0 Then Comp++ :Print Format$("{0::-3} {1}", Comp, lpart$+String$("_|", Cells)):  Exit Sub
            If Cells<=0 Then Exit Sub
            Local TotalBlocksLength=0, Sep_Spaces=-1
            Local Block=Each(Blocks), block$
            While Block {
                  Block$=Array$(Block)    
                  TotalBlocksLength+=Blen(Block$)
                  Sep_Spaces++
            }
            Local MaxLengthNeed=TotalBlocksLength+Sep_Spaces
            If MaxLengthNeed>Cells Then Exit Sub
            block$=Array$(Car(Blocks))
            local temp=Blen(block$)
            block$=Mid$(Block$, 2)
            If Len(Blocks)>1 Then block$+="_|" :temp++
            PrintRow(Lpart$+block$, Cells-temp, Cdr(Blocks), &Comp)
            PrintRow(lpart$+String$("_|", 1), Cells-1,Blocks, &Comp)
      End Sub
}
NonoBlock

```


{{out}}
<pre style="height:30ex;overflow:scroll">
Nonoblock
1.    Cells=5 |A|A| |B| 
  1 |A|A|_|B|_|
  2 |A|A|_|_|B|
  3 |_|A|A|_|B|
2.    Cells=5 Empty
  1 |_|_|_|_|_|
3.    Cells=10 |A|A|A|A|A|A|A|A| 
  1 |A|A|A|A|A|A|A|A|_|_|
  2 |_|A|A|A|A|A|A|A|A|_|
  3 |_|_|A|A|A|A|A|A|A|A|
4.    Cells=15 |A|A| |B|B|B| |C|C| |D|D|D| 
  1 |A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
  2 |A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
  3 |A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
  4 |A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
  5 |A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
  6 |A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
  7 |A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
  8 |A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
  9 |A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
 10 |A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
 11 |_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
 12 |_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
 13 |_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
 14 |_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
 15 |_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|
5.    Cells=5 |A|A| |B|B|B| 
 Impossible

</pre >

### Non Recursive


```M2000 Interpreter

Module Nonoblock (n, m) {
      Print "Cells:",n," Blocks:",m
      Dim n(1 to n), m(1 to m), sp(1 to m*2), sk(1 to m*2), part(1 to m)
      queue=0      
      If m>0 Then {
            Print "Block Size:",
            For i=1 to m {
                  Read m(i)
                  Print m(i),
            }
            Print
            part(m)=m(m)      
            If m>1 Then {
                  For i=m-1 to 1 {
                        part(i)=m(i)+part(i+1)+1
                  }
            }
      }
      If part(1)>n Then {
          Print "Impossible"  
      } Else {
            p1=0
            l=0
            Counter=0
            While p1<=n-part(1) {
                  k=0
                  p=p1+1
                  For i=1 to n {
                        n(i)=0
                  }
                  flag=True
                  Repeat {
                        While k<m {
                              k++
                              l=0
                              While l<m(k) and p<=n {
                                    l++
                                    n(p)=1
                                    p++
                              }
                              If p<n Then {
                                    n(p)=0
                                    p++
                                   If k<m Then {
                                          If p+part(k+1)<n+1 Then {
                                                queue++
                                                sp(queue)=p
                                                sk(queue)=k
                                          }
                                    }
                              }
                        }
                        flag=True
                        If l=m(k)  Then {
                              counter++
                              Print Str$(counter,"0000  ");
                              For i=1 to n {
                                    Print n(i);" ";
                              }
                              Print
                              If queue>0 Then  {
                                    p=sp(queue)
                                    k=sk(queue)
                                    queue--
                                    For i=p to n {
                                          n(i)=0
                                    }
                                    p++
                                    If k<m Then {
                                          If p+part(k+1)<n+1 Then {
                                                queue++
                                                sp(queue)=p
                                               ' sk(queue)=k
                                       }
                                    }
                                    flag=False
                              } 
                        }
                  } Until flag
                  p1++
                  If k=0 Then Exit
            }
      }
}

Nonoblock 5,2,2,1
Nonoblock 5,0
Nonoblock 10,1,8
Nonoblock 15,4,2,3,2,3
Nonoblock 5,2,3,2

```



## Perl


```perl
use strict;
use warnings;

while( <DATA> )
  {
  print "\n$_", tr/\n/=/cr;
  my ($cells, @blocks) = split;
  my $letter = 'A';
  $_ = join '.', map { $letter++ x $_ } @blocks;
  $cells < length and print("no solution\n"), next;
  $_ .= '.' x ($cells - length) . "\n";
  1 while print, s/^(\.*)\b(.*?)\b(\w+)\.\B/$2$1.$3/;
  }

__DATA__
5 2 1
5
10 8
15 2 3 2 3
5 2 3
```

{{out}}

```txt

5 2 1
=====
AA.B.
AA..B
.AA.B

5
=
.....

10 8
====
AAAAAAAA..
.AAAAAAAA.
..AAAAAAAA

15 2 3 2 3

### ====

AA.BBB.CC.DDD..
AA.BBB.CC..DDD.
AA.BBB..CC.DDD.
AA..BBB.CC.DDD.
.AA.BBB.CC.DDD.
AA.BBB.CC...DDD
AA.BBB..CC..DDD
AA..BBB.CC..DDD
.AA.BBB.CC..DDD
AA.BBB...CC.DDD
AA..BBB..CC.DDD
.AA.BBB..CC.DDD
AA...BBB.CC.DDD
.AA..BBB.CC.DDD
..AA.BBB.CC.DDD

5 2 3
=====
no solution


```



## Perl 6

{{trans|Perl}}

```perl6
for (5, [2,1]), (5, []), (10, [8]), (5, [2,3]), (15, [2,3,2,3]) -> ($cells, @blocks) {
    say $cells, ' cells with blocks: ', @blocks ?? join ', ', @blocks !! '∅';
    my $letter = 'A';
    my $row = join '.', map { $letter++ x $_ }, @blocks;
    say "no solution\n" and next if $cells < $row.chars;
    say $row ~= '.' x $cells - $row.chars;
    say $row while $row ~~ s/^^ (\.*) <|w> (.*?) <|w> (\w+) \.<!|w> /$1$0.$2/;
    say '';
}
```

{{out}}

```txt
5 cells with blocks: 2, 1
AA.B.
AA..B
.AA.B

5 cells with blocks: ∅
.....

10 cells with blocks: 8
AAAAAAAA..
.AAAAAAAA.
..AAAAAAAA

5 cells with blocks: 2, 3
no solution

15 cells with blocks: 2, 3, 2, 3
AA.BBB.CC.DDD..
AA.BBB.CC..DDD.
AA.BBB..CC.DDD.
AA..BBB.CC.DDD.
.AA.BBB.CC.DDD.
AA.BBB.CC...DDD
AA.BBB..CC..DDD
AA..BBB.CC..DDD
.AA.BBB.CC..DDD
AA.BBB...CC.DDD
AA..BBB..CC.DDD
.AA.BBB..CC.DDD
AA...BBB.CC.DDD
.AA..BBB.CC.DDD
..AA.BBB.CC.DDD
```



## Phix


```Phix
function nobr(sequence res, string neat, integer ni, integer ch, sequence blocks)
    if length(blocks)=0 then
        res = append(res,neat)
    else
        integer b = blocks[1]
        blocks = blocks[2..$]
        integer l = (sum(blocks)+length(blocks)-1)*2,
                e = length(neat)-l-b*2
        for i=ni to e by 2 do
            for j=i to i+b*2-2 by 2 do
                neat[j] = ch
            end for
            res = nobr(res,neat,i+b*2+2,ch+1,blocks)
            neat[i] = ' '
        end for
    end if
    return res
end function

function nonoblock(integer len, sequence blocks)
    string neat = "|"&join(repeat(' ',len),'|')&"|"
    return nobr({},neat,2,'A',blocks)
end function

sequence tests = {{5,{2,1}},
                  {5,{}},
                  {10,{8}},
                  {15,{2, 3, 2, 3}},
                  {10,{4, 3}},
                  {5,{2,1}},
                  {10,{3, 1}},
                  {5,{2, 3}}}
integer len
sequence blocks, res
for i=1 to length(tests) do
    {len,blocks} = tests[i]
    string ti = sprintf("%d cells with blocks %s",{len,sprint(blocks)})
    printf(1,"%s\n%s\n",{ti,repeat('=',length(ti))})
    res = nonoblock(len,blocks)
    if length(res)=0 then
        printf(1,"No solutions.\n")
    else
        for ri=1 to length(res) do
            printf(1,"%3d:  %s\n",{ri,res[ri]})
        end for
    end if
    printf(1,"\n")
end for
```

{{out}}

```txt

5 cells with blocks {2,1}

### ===================

  1:  |A|A| |B| |
  2:  |A|A| | |B|
  3:  | |A|A| |B|

5 cells with blocks {}

### ================

  1:  | | | | | |

10 cells with blocks {8}

### ==================

  1:  |A|A|A|A|A|A|A|A| | |
  2:  | |A|A|A|A|A|A|A|A| |
  3:  | | |A|A|A|A|A|A|A|A|

15 cells with blocks {2,3,2,3}

### ========================

  1:  |A|A| |B|B|B| |C|C| |D|D|D| | |
  2:  |A|A| |B|B|B| |C|C| | |D|D|D| |
  3:  |A|A| |B|B|B| |C|C| | | |D|D|D|
  4:  |A|A| |B|B|B| | |C|C| |D|D|D| |
  5:  |A|A| |B|B|B| | |C|C| | |D|D|D|
  6:  |A|A| |B|B|B| | | |C|C| |D|D|D|
  7:  |A|A| | |B|B|B| |C|C| |D|D|D| |
  8:  |A|A| | |B|B|B| |C|C| | |D|D|D|
  9:  |A|A| | |B|B|B| | |C|C| |D|D|D|
 10:  |A|A| | | |B|B|B| |C|C| |D|D|D|
 11:  | |A|A| |B|B|B| |C|C| |D|D|D| |
 12:  | |A|A| |B|B|B| |C|C| | |D|D|D|
 13:  | |A|A| |B|B|B| | |C|C| |D|D|D|
 14:  | |A|A| | |B|B|B| |C|C| |D|D|D|
 15:  | | |A|A| |B|B|B| |C|C| |D|D|D|

10 cells with blocks {4,3}

### ====================

  1:  |A|A|A|A| |B|B|B| | |
  2:  |A|A|A|A| | |B|B|B| |
  3:  |A|A|A|A| | | |B|B|B|
  4:  | |A|A|A|A| |B|B|B| |
  5:  | |A|A|A|A| | |B|B|B|
  6:  | | |A|A|A|A| |B|B|B|

5 cells with blocks {2,1}

### ===================

  1:  |A|A| |B| |
  2:  |A|A| | |B|
  3:  | |A|A| |B|

10 cells with blocks {3,1}

### ====================

  1:  |A|A|A| |B| | | | | |
  2:  |A|A|A| | |B| | | | |
  3:  |A|A|A| | | |B| | | |
  4:  |A|A|A| | | | |B| | |
  5:  |A|A|A| | | | | |B| |
  6:  |A|A|A| | | | | | |B|
  7:  | |A|A|A| |B| | | | |
  8:  | |A|A|A| | |B| | | |
  9:  | |A|A|A| | | |B| | |
 10:  | |A|A|A| | | | |B| |
 11:  | |A|A|A| | | | | |B|
 12:  | | |A|A|A| |B| | | |
 13:  | | |A|A|A| | |B| | |
 14:  | | |A|A|A| | | |B| |
 15:  | | |A|A|A| | | | |B|
 16:  | | | |A|A|A| |B| | |
 17:  | | | |A|A|A| | |B| |
 18:  | | | |A|A|A| | | |B|
 19:  | | | | |A|A|A| |B| |
 20:  | | | | |A|A|A| | |B|
 21:  | | | | | |A|A|A| |B|

5 cells with blocks {2,3}

### ===================

No solutions.

```



## Python


```python
def nonoblocks(blocks, cells):
    if not blocks or blocks[0] == 0:
        yield [(0, 0)]
    else:
        assert sum(blocks) + len(blocks)-1 <= cells, \
            'Those blocks will not fit in those cells'
        blength, brest = blocks[0], blocks[1:]      # Deal with the first block of length
        minspace4rest = sum(1+b for b in brest)     # The other blocks need space
        # Slide the start position from left to max RH index allowing for other blocks.
        for bpos in range(0, cells - minspace4rest - blength + 1):
            if not brest:
                # No other blocks to the right so just yield this one.
                yield [(bpos, blength)]
            else:
                # More blocks to the right so create a *sub-problem* of placing
                # the brest blocks in the cells one space to the right of the RHS of 
                # this block.
                offset = bpos + blength +1
                nonoargs = (brest, cells - offset)  # Pre-compute arguments to nonoargs
                # recursive call to nonoblocks yields multiple sub-positions
                for subpos in nonoblocks(*nonoargs):
                    # Remove the offset from sub block positions
                    rest = [(offset + bp, bl) for bp, bl in subpos]
                    # Yield this block plus sub blocks positions
                    vec = [(bpos, blength)] + rest
                    yield vec

def pblock(vec, cells):
    'Prettyprints each run of blocks with a different letter A.. for each block of filled cells'
    vector = ['_'] * cells
    for ch, (bp, bl) in enumerate(vec, ord('A')):
        for i in range(bp, bp + bl):
            vector[i] = chr(ch) if vector[i] == '_' else'?'
    return '|' + '|'.join(vector) + '|'


if __name__ == '__main__':
    for blocks, cells in (
            ([2, 1], 5),
            ([], 5),
            ([8], 10),
            ([2, 3, 2, 3], 15),
           # ([4, 3], 10),
           # ([2, 1], 5),
           # ([3, 1], 10),
            ([2, 3], 5),
            ):
        print('\nConfiguration:\n    %s # %i cells and %r blocks' % (pblock([], cells), cells, blocks))        
        print('  Possibilities:')
        for i, vector in enumerate(nonoblocks(blocks, cells)):
            print('   ', pblock(vector, cells))
        print('  A total of %i Possible configurations.' % (i+1))
```


{{out}}

```txt
Configuration:
    |_|_|_|_|_| # 5 cells and [2, 1] blocks
  Possibilities:
    |A|A|_|B|_|
    |A|A|_|_|B|
    |_|A|A|_|B|
  A total of 3 Possible configurations.

Configuration:
    |_|_|_|_|_| # 5 cells and [] blocks
  Possibilities:
    |_|_|_|_|_|
  A total of 1 Possible configurations.

Configuration:
    |_|_|_|_|_|_|_|_|_|_| # 10 cells and [8] blocks
  Possibilities:
    |A|A|A|A|A|A|A|A|_|_|
    |_|A|A|A|A|A|A|A|A|_|
    |_|_|A|A|A|A|A|A|A|A|
  A total of 3 Possible configurations.

Configuration:
    |_|_|_|_|_|_|_|_|_|_|_|_|_|_|_| # 15 cells and [2, 3, 2, 3] blocks
  Possibilities:
    |A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
    |A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
    |A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
    |A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
    |A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
    |A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
    |A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
    |A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
    |A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
    |A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
    |_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
    |_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
    |_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
    |_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
    |_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|
  A total of 15 Possible configurations.

Configuration:
    |_|_|_|_|_| # 5 cells and [2, 3] blocks
  Possibilities:
Traceback (most recent call last):
  File "C:/Users/Paddy/Google Drive/Code/nonoblocks.py", line 104, in <module>
    for i, vector in enumerate(nonoblocks(blocks, cells)):
  File "C:/Users/Paddy/Google Drive/Code/nonoblocks.py", line 60, in nonoblocks
    'Those blocks will not fit in those cells'
AssertionError: Those blocks will not fit in those cells
```



## Racket


This implementation does not "error" on the impossible case.

Knowing that there are no solutions (empty result list) is good enough.

Also, the blocks are not identified. I suppose they could be easily enough, but in the nonogram task, these patterns are converted to bit-fields shortly after the nonoblock generation, and bits have no names (sad, but true).


```racket
#lang racket
(require racket/trace)

(define add1-to-car (match-lambda [(cons (app add1 p1) t) (cons p1 t)]))

;; inputs:
;;   cells  -- available cells
;;   blocks -- list of block widths
;; output:
;;   gap-block+gaps
;;   where gap-block+gaps is:
;;   (list gap)                            -- a single gap
;;   (list gap block-width gap-block+gaps) -- padding to left, a block, right hand side
(define (nonoblock cells blocks)
  (match* ((- cells (apply + (length blocks) -1 blocks)) #| padding available on both sides |# blocks)
    [(_ (list)) (list (list cells))] ; generates an empty list of padding
    
    [((? negative?) _) null] ; impossible to satisfy
    
    [((and avp
           ;; use add1 with in-range because we actually want from 0 to available-padding
           ;; without add1, in-range iterates from 0 to (available-padding - 1)
           (app add1 avp+1))
      (list block))
     (for/list ((l-pad (in-range 0 avp+1)))
       (define r-pad (- avp l-pad)) ; what remains goes to right
       (list l-pad block r-pad))]
    
    [((app add1 avp+1) (list block more-blocks ...))
     (for*/list ((l-pad (in-range 0 avp+1))
                 (cells-- (in-value (- cells block l-pad 1)))
                 (r-blocks (in-value (nonoblock cells-- more-blocks)))
                 (r-block (in-list r-blocks)))
       (list* l-pad block (add1-to-car r-block)))])) ; put a single space pad on left of r-block

(define (neat rslt)
  (define dots (curryr make-string #\.))
  (define Xes (curryr make-string #\X))
  (define inr
    (match-lambda
      [(list 0 (app Xes b) t ...)
       (string-append b (inr t))]
      [(list (app dots p) (app Xes b) t ...)
       (string-append p b (inr t))]
      [(list (app dots p)) p]))
  (define (neat-row r)
    (string-append "|" (inr r) "|"))
  (string-join (map neat-row rslt) "\n"))

(define (tst c b)
  (define rslt (nonoblock c b))
  (define rslt-l (length rslt))
  (printf "~a cells, ~a blocks => ~a~%~a~%" c b
          (match rslt-l
            [0 "impossible"]
            [1 "1 solution"]
            [(app (curry format "~a solutions") r) r])
          (neat rslt)))

(module+ test
  (tst  5 '[2 1])
  (tst  5 '[])
  (tst 10 '[8])
  (tst 15 '[2 3 2 3])
  (tst  5 '[2 3]))
```


{{out}}

```txt
5 cells, (2 1) blocks => 3 solutions
|XX.X.|
|XX..X|
|.XX.X|
5 cells, () blocks => 1 solution
|.....|
10 cells, (8) blocks => 3 solutions
|XXXXXXXX..|
|.XXXXXXXX.|
|..XXXXXXXX|
15 cells, (2 3 2 3) blocks => 15 solutions
|XX.XXX.XX.XXX..|
|XX.XXX.XX..XXX.|
|XX.XXX.XX...XXX|
|XX.XXX..XX.XXX.|
|XX.XXX..XX..XXX|
|XX.XXX...XX.XXX|
|XX..XXX.XX.XXX.|
|XX..XXX.XX..XXX|
|XX..XXX..XX.XXX|
|XX...XXX.XX.XXX|
|.XX.XXX.XX.XXX.|
|.XX.XXX.XX..XXX|
|.XX.XXX..XX.XXX|
|.XX..XXX.XX.XXX|
|..XX.XXX.XX.XXX|
5 cells, (2 3) blocks => impossible

```



## REXX


```rexx
/*REXX program enumerates all possible configurations (or an error) for nonogram puzzles*/
             $.=;    $.1=  5   2 1
                     $.2=  5
                     $.3= 10   8
                     $.4= 15   2 3 2 3
                     $.5=  5   2 3
      do  i=1  while $.i\==''
      parse var  $.i   N  blocks                 /*obtain  N  and  blocks   from array. */
      N= strip(N);     blocks= space(blocks)     /*assign stripped   N   and   blocks.  */
      call nono                                  /*incoke NONO subroutine for heavy work*/
      end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
nono: say copies('=', 70)                                 /*display seperator for title.*/
      say 'For '   N   " cells  and blocks of: "   blocks /*display the title for output*/
      z=                                                  /*assign starter value for Z. */
          do w=1  for words(blocks)                       /*process each of the blocks. */
          z= z copies('#', word(blocks,w) )               /*build a string for 1st value*/
          end   /*w*/                                     /*Z  now has a leading blank. */
      #= 1                                                /*number of positions (so far)*/
      z= translate( strip(z), ., ' ');   L= length(z)     /*change blanks to periods.   */
      if L>N  then do;   say '***error***  invalid blocks for number of cells.';   return
                   end
      @.0=;           @.1= z;         !.=0       /*assign default and the first position*/
      z= pad(z)                                  /*fill─out (pad) the value with periods*/

         do prepend=1  while words(blocks)\==0   /*process all the positions (leading .)*/
         new= . || @.prepend                     /*create positions with leading dots.  */
         if length(new)>N  then leave            /*Length is too long?  Then stop adding*/
         call add                                /*add position that has a leading dot. */
         end   /*prepend*/                       /* [↑]  prepend positions with dots.   */

         do   k=1  for N                         /*process each of the positions so far.*/
           do c=1  for N                         /*   "      "   "  "  position blocks. */
           if @.c==''  then iterate              /*if string is null,  skip the string. */
           p= loc(@.c, k)                        /*find location of block in position.  */
           if p==0 | p>=N  then iterate          /*Location zero or out─of─range?  Skip.*/
           new= strip( insert(., @.c, p),'T',.)  /*insert a dot and strip trailing dots.*/
           if strip(new,'T',.)=@.c  then iterate /*Is it the same value?  Then skip it. */
           if length(new)<=N  then call add      /*Is length OK?   Then add position.   */
           end   /*k*/
         end     /*c*/
      say
      say '─position─'  center("value", max(7, length(z) ), '─')  /*show hdr for output.*/

               do m=1  for #
               say center(m, 10)   pad(@.m)      /*display the index count and position.*/
               end   /*m*/
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
loc:  _=0; do arg(2); _=pos('#.',pad(arg(1)),_+1); if _==0  then return 0; end; return _+1
add:  if !.new==1  then return;  #= # + 1;     @.#= new;    !.new=1;    return
pad:  return  left( arg(1), N, .)
```

{{out|output|text=  when using the default inputs:}}

```txt


### ================================================================

For  5  cells  and blocks of:  2 1

─position─ ─value─
    1      ##.#.
    2      .##.#
    3      ##..#

### ================================================================

For  5  cells  and blocks of:

─position─ ─value─
    1      .....

### ================================================================

For  10  cells  and blocks of:  8

─position─ ──value───
    1      ########..
    2      .########.
    3      ..########

### ================================================================

For  15  cells  and blocks of:  2 3 2 3

─position─ ─────value─────
    1      ##.###.##.###..
    2      .##.###.##.###.
    3      ..##.###.##.###
    4      ##..###.##.###.
    5      .##..###.##.###
    6      ##...###.##.###
    7      ##.###..##.###.
    8      .##.###..##.###
    9      ##..###..##.###
    10     ##.###...##.###
    11     ##.###.##..###.
    12     .##.###.##..###
    13     ##..###.##..###
    14     ##.###..##..###
    15     ##.###.##...###

### ================================================================

For  5  cells  and blocks of:  2 3
***error***  invalid blocks for number of cells.

```



## Ruby

'''Simple version:'''

```ruby
def nonoblocks(cell, blocks)
  raise 'Those blocks will not fit in those cells' if cell < blocks.inject(0,:+) + blocks.size - 1
  nblock(cell, blocks, '', [])
end

def nblock(cell, blocks, position, result)
  if cell <= 0
    result << position[0..cell-1]
  elsif blocks.empty? or blocks[0].zero?
    result << position + '.' * cell
  else
    rest = cell - blocks.inject(:+) - blocks.size + 2
    bl, *brest = blocks
    rest.times.inject(result) do |res, i|
      nblock(cell-i-bl-1, brest, position + '.'*i + '#'*bl + '.', res)
    end
  end
end

conf = [[ 5, [2, 1]],
        [ 5, []],
        [10, [8]],
        [15, [2, 3, 2, 3]],
        [ 5, [2, 3]],      ]
conf.each do |cell, blocks|
  begin
    puts "#{cell} cells and #{blocks} blocks"
    result = nonoblocks(cell, blocks)
    puts result, result.size, ""
  rescue => e
    p e
  end
end
```


{{out}}

```txt

5 cells and [2, 1] blocks
##.#.
##..#
.##.#
3

5 cells and [] blocks
.....
1

10 cells and [8] blocks
########..
.########.
..########
3

15 cells and [2, 3, 2, 3] blocks
##.###.##.###..
##.###.##..###.
##.###.##...###
##.###..##.###.
##.###..##..###
##.###...##.###
##..###.##.###.
##..###.##..###
##..###..##.###
##...###.##.###
.##.###.##.###.
.##.###.##..###
.##.###..##.###
.##..###.##.###
..##.###.##.###
15

5 cells and [2, 3] blocks
#<RuntimeError: Those blocks will not fit in those cells>

```



### Class version

The output form consulted the one of the python.

```ruby
class NonoBlock
  def initialize(cell, blocks)
    raise 'Those blocks will not fit in those cells' if cell < blocks.inject(0,:+) + blocks.size - 1
    @result = []
    nonoblocks(cell, blocks, '')
  end
  
  def result(correct=true)
    correct ? @result.map(&:nonocell) : @result
  end
  
  private
  def nonoblocks(cell, blocks, position)
    if cell <= 0
      @result << position[0..cell-1]
    elsif blocks.empty? or blocks[0].zero?
      @result << position + '.' * cell
    else
      rest = cell - blocks.inject(0,:+) - blocks.size + 2
      bl, *brest = blocks
      rest.times do |i|
        nonoblocks(cell-i-bl-1, brest, position + '.'*i + '#'*bl + '.')
      end
    end
  end
end

class String
  def nonocell                  # "##.###..##" -> "|A|A|_|B|B|B|_|_|C|C|"
    chr = ('A'..'Z').each
    s = tr('.','_').gsub(/#+/){|sharp| chr.next * sharp.size}
    "|#{s.chars.join('|')}|"
  end
end

if __FILE__ == $0
  conf = [[ 5, [2, 1]],
          [ 5, []],
          [10, [8]],
          [15, [2, 3, 2, 3]],
          [ 5, [2, 3]]       ]
  conf.each do |cell, blocks|
    begin
      puts "Configuration:",
           "#{('.'*cell).nonocell} # #{cell} cells and #{blocks} blocks",
           "Possibilities:"
      result = NonoBlock.new(cell, blocks).result
      puts result,
           "A total of #{result.size} Possible configurations.", ""
    rescue => e
      p e
    end
  end
end
```


{{out}}
<pre style="height: 64ex; overflow: scroll">
Configuration:
|_|_|_|_|_| # 5 cells and [2, 1] blocks
Possibilities:
|A|A|_|B|_|
|A|A|_|_|B|
|_|A|A|_|B|
A total of 3 Possible configurations.

Configuration:
|_|_|_|_|_| # 5 cells and [] blocks
Possibilities:
|_|_|_|_|_|
A total of 1 Possible configurations.

Configuration:
|_|_|_|_|_|_|_|_|_|_| # 10 cells and [8] blocks
Possibilities:
|A|A|A|A|A|A|A|A|_|_|
|_|A|A|A|A|A|A|A|A|_|
|_|_|A|A|A|A|A|A|A|A|
A total of 3 Possible configurations.

Configuration:
|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_| # 15 cells and [2, 3, 2, 3] blocks
Possibilities:
|A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
|A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
|A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
|A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
|A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
|A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
|A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
|A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
|A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
|A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
|_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
|_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
|_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
|_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
|_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|
A total of 15 Possible configurations.

Configuration:
|_|_|_|_|_| # 5 cells and [2, 3] blocks
Possibilities:
#<RuntimeError: Those blocks will not fit in those cells>

```



## Rust

{{works with|Rust|1.29.2}}

```rust
struct Nonoblock {
  width: usize,
  config: Vec<usize>,
  spaces: Vec<usize>,
}

impl Nonoblock {
  pub fn new(width: usize, config: Vec<usize>) -> Nonoblock {
    Nonoblock {
      width: width,
      config: config,
      spaces: Vec::new(),
    }
  }

  pub fn solve(&mut self) -> Vec<Vec<i32>> {
    let mut output: Vec<Vec<i32>> = Vec::new();
    self.spaces = (0..self.config.len()).fold(Vec::new(), |mut s, i| {
      s.push(match i {
        0 => 0,
        _ => 1,
      });
      s
    });
    if self.spaces.iter().sum::<usize>() + self.config.iter().sum::<usize>() <= self.width {
      'finished: loop {
        match self.spaces.iter().enumerate().fold((0, vec![0; self.width]), |mut a, (i, s)| {
            (0..self.config[i]).for_each(|j| a.1[a.0 + j + *s] = 1 + i as i32);
            return (a.0 + self.config[i] + *s, a.1);
          }) {
          (_, out) => output.push(out),
        }
        let mut i: usize = 1;
        'calc: loop {
          let len = self.spaces.len();
          if i > len {
            break 'finished;
          } else {
            self.spaces[len - i] += 1
          }
          if self.spaces.iter().sum::<usize>() + self.config.iter().sum::<usize>() > self.width {
            self.spaces[len - i] = 1;
            i += 1;
          } else {
            break 'calc;
          }
        }
      }
    }
    output
  }
}

fn main() {
  let mut blocks = [
    Nonoblock::new(5, vec![2, 1]),
    Nonoblock::new(5, vec![]),
    Nonoblock::new(10, vec![8]),
    Nonoblock::new(15, vec![2, 3, 2, 3]),
    Nonoblock::new(5, vec![2, 3]),
  ];

  for block in blocks.iter_mut() {
    println!("{} cells and {:?} blocks", block.width, block.config);
    println!("{}",(0..block.width).fold(String::from("="), |a, _| a + "=="));
    let solutions = block.solve();
    if solutions.len() > 0 {
      for solution in solutions.iter() {
        println!("{}", solution.iter().fold(String::from("|"), |s, f| s + &match f {
          i if *i > 0 => (('A' as u8 + ((*i - 1) as u8) % 26) as char).to_string(),
          _ => String::from("_"),
        }+ "|"));
      }
    } else {
      println!("No solutions. ");
    }
    println!();
  }
}
```

{{out}}

```txt

5 cells and [2, 1] blocks

### =====

|A|A|_|B|_|
|A|A|_|_|B|
|_|A|A|_|B|

5 cells and [] blocks

### =====

|_|_|_|_|_|

10 cells and [8] blocks

### ===============

|A|A|A|A|A|A|A|A|_|_|
|_|A|A|A|A|A|A|A|A|_|
|_|_|A|A|A|A|A|A|A|A|

15 cells and [2, 3, 2, 3] blocks

### =========================

|A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
|A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
|A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
|A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
|A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
|A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
|A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
|A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
|A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
|A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
|_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
|_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
|_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
|_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
|_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|

5 cells and [2, 3] blocks

### =====

No solutions.

```



## Tcl

{{works with|Tcl|8.6}}
{{tcllib|generator}}
{{trans|Python}}

```tcl
package require Tcl 8.6
package require generator

generator define nonoblocks {blocks cells} {
    set sum [tcl::mathop::+ {*}$blocks]
    if {$sum == 0 || [lindex $blocks 0] == 0} {
	generator yield {{0 0}}
	return
    } elseif {$sum + [llength $blocks] - 1 > $cells} {
	error "those blocks will not fit in those cells"
    }

    set brest [lassign $blocks blen]
    for {set bpos 0} {$bpos <= $cells - $sum - [llength $brest]} {incr bpos} {
	if {![llength $brest]} {
	    generator yield [list [list $bpos $blen]]
	    return
	}
	set offset [expr {$bpos + $blen + 1}]
	generator foreach subpos [nonoblocks $brest [expr {$cells - $offset}]] {
	    generator yield [linsert [lmap b $subpos {
		lset b 0 [expr {[lindex $b 0] + $offset}]
	    }] 0 [list $bpos $blen]]
	}
    }
}

if {[info script] eq $::argv0} {
    proc pblock {cells {vec {}}} {
	set vector [lrepeat $cells "_"]
	set ch 64
	foreach b $vec {
	    incr ch
	    lassign $b bp bl
	    for {set i $bp} {$i < $bp + $bl} {incr i} {
		lset vector $i [format %c $ch]
	    }
	}
	return |[join $vector "|"]|
    }
    proc flist {items} {
	return [format "\[%s\]" [join $items ", "]]
    }
    foreach {blocks cells} {
	{2 1} 5
	{} 5
	{8} 10
	{2 3 2 3} 15
	{2 3} 5
    } {
	puts "\nConfiguration:"
	puts [format "%s # %d cells and %s blocks" \
		  [pblock $cells] $cells [flist $blocks]]
	puts "  Possibilities:"
	set i 0
	try {
	    generator foreach vector [nonoblocks $blocks $cells] {
		puts "    [pblock $cells $vector]"
		incr i
	    }
	    puts "  A total of $i possible configurations"
	} on error msg {
	    puts "    --> ERROR: $msg"
	}
    }
}

package provide nonoblock 1
```

{{out}}

```txt


Configuration:
|_|_|_|_|_| # 5 cells and [2, 1] blocks
  Possibilities:
    |A|A|_|B|_|
    |A|A|_|_|B|
    |_|A|A|_|B|
  A total of 3 possible configurations

Configuration:
|_|_|_|_|_| # 5 cells and [] blocks
  Possibilities:
    |_|_|_|_|_|
  A total of 1 possible configurations

Configuration:
|_|_|_|_|_|_|_|_|_|_| # 10 cells and [8] blocks
  Possibilities:
    |A|A|A|A|A|A|A|A|_|_|
    |_|A|A|A|A|A|A|A|A|_|
    |_|_|A|A|A|A|A|A|A|A|
  A total of 3 possible configurations

Configuration:
|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_| # 15 cells and [2, 3, 2, 3] blocks
  Possibilities:
    |A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
    |A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
    |A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
    |A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
    |A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
    |A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
    |A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
    |A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
    |A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
    |A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
    |_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
    |_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
    |_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
    |_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
    |_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|
  A total of 15 possible configurations

Configuration:
|_|_|_|_|_| # 5 cells and [2, 3] blocks
  Possibilities:
    --> ERROR: those blocks will not fit in those cells

```



## zkl

{{trans|Python}}

```zkl
fcn nonoblocks(blocks,cells){
   if(not blocks or blocks[0]==0) vm.yield( T(T(0,0)) );
   else{
      if(not ( blocks.sum(0) + blocks.len() -1<=cells ))
	 throw(Exception.AssertionError("Those blocks will not fit in those cells"));
      blength,brest:=blocks[0], blocks[1,*];      # Deal with the first block of length
      minspace4rest:=brest.reduce('+(1),0);       # The other blocks need space
      # Slide the start position from left to max RH index allowing for other blocks.
      foreach bpos in (cells - minspace4rest - blength +1){
         if(not brest) # No other blocks to the right so just yield this one.
	    vm.yield(T(T(bpos,blength)));
	 else{
	    # More blocks to the right so create a *sub-problem* of placing
	    # the brest blocks in the cells one space to the right of the RHS of 
	    # this block.
	    offset:=bpos + blength +1;
	    # recursive call to nonoblocks yields multiple sub-positions
	    foreach subpos in (Utils.Generator(nonoblocks,brest,cells - offset)){
	       # Remove the offset from sub block positions
	       rest:=subpos.pump(List,'wrap([(bp,bl)]){ T(offset + bp, bl) });
	       # Yield this block plus sub blocks positions
	       vm.yield(T( T(bpos,blength) ).extend(rest) );
	    }
	 }
      }
   }
}
# Pretty print each run of blocks with a different letter for each block of filled cells
fcn pblock(vec,cells){
   vector,ch:=cells.pump(List(),"_".copy), ["A".."Z"];
   vec.apply2('wrap([(a,b)]){ a.walker(b).pump(Void,vector.set.fp1(ch.next())) });
   String("|",vector.concat("|"),"|");
}
```


```zkl
foreach blocks,cells in (T( T(T(2,1),5), T(T,5), T(T(8),10), T(T(2,3,2,3),15),
			    T(T(2,3),5) )){
   println("\nConfiguration:\n    %s # %d cells and %s blocks"
         .fmt(pblock(T,cells),cells,blocks));
   println("  Possibilities:");
   Utils.Generator(nonoblocks,blocks,cells).reduce('wrap(n,vector){
      println("    ",pblock(vector,cells));
      n+1
   },0)
   : println("  A total of %d possible configurations.".fmt(_));
}
```

{{out}}

```txt

Configuration:
    |_|_|_|_|_| # 5 cells and L(2,1) blocks
  Possibilities:
    |A|A|_|B|_|
    |A|A|_|_|B|
    |_|A|A|_|B|
  A total of 3 possible configurations.

Configuration:
    |_|_|_|_|_| # 5 cells and L() blocks
  Possibilities:
    |_|_|_|_|_|
  A total of 1 possible configurations.

Configuration:
    |_|_|_|_|_|_|_|_|_|_| # 10 cells and L(8) blocks
  Possibilities:
    |A|A|A|A|A|A|A|A|_|_|
    |_|A|A|A|A|A|A|A|A|_|
    |_|_|A|A|A|A|A|A|A|A|
  A total of 3 possible configurations.

Configuration:
    |_|_|_|_|_|_|_|_|_|_|_|_|_|_|_| # 15 cells and L(2,3,2,3) blocks
  Possibilities:
    |A|A|_|B|B|B|_|C|C|_|D|D|D|_|_|
    |A|A|_|B|B|B|_|C|C|_|_|D|D|D|_|
    |A|A|_|B|B|B|_|C|C|_|_|_|D|D|D|
    |A|A|_|B|B|B|_|_|C|C|_|D|D|D|_|
    |A|A|_|B|B|B|_|_|C|C|_|_|D|D|D|
    |A|A|_|B|B|B|_|_|_|C|C|_|D|D|D|
    |A|A|_|_|B|B|B|_|C|C|_|D|D|D|_|
    |A|A|_|_|B|B|B|_|C|C|_|_|D|D|D|
    |A|A|_|_|B|B|B|_|_|C|C|_|D|D|D|
    |A|A|_|_|_|B|B|B|_|C|C|_|D|D|D|
    |_|A|A|_|B|B|B|_|C|C|_|D|D|D|_|
    |_|A|A|_|B|B|B|_|C|C|_|_|D|D|D|
    |_|A|A|_|B|B|B|_|_|C|C|_|D|D|D|
    |_|A|A|_|_|B|B|B|_|C|C|_|D|D|D|
    |_|_|A|A|_|B|B|B|_|C|C|_|D|D|D|
  A total of 15 possible configurations.

Configuration:
    |_|_|_|_|_| # 5 cells and L(2,3) blocks
  Possibilities:
VM#2 caught this unhandled exception:
   AssertionError : Those blocks will not fit in those cells
   <stack traces deleted>

```

